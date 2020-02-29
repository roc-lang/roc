use std::convert::TryFrom;

use bumpalo::collections::Vec;
use bumpalo::Bump;
use cranelift::frontend::Switch;
use cranelift::prelude::{
    AbiParam, ExternalName, FloatCC, FunctionBuilder, FunctionBuilderContext, IntCC, MemFlags,
};
use cranelift_codegen::ir::entities::{StackSlot, Value};
use cranelift_codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift_codegen::ir::{immediates::Offset32, types, InstBuilder, Signature, Type};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_codegen::Context;
use cranelift_module::{Backend, FuncId, Linkage, Module};

use crate::collections::ImMap;
use crate::crane::convert::{sig_from_layout, type_from_layout};
use crate::module::symbol::{Interns, Symbol};
use crate::mono::expr::{Expr, Proc, Procs};
use crate::mono::layout::{Builtin, Layout};
use crate::subs::{Subs, Variable};

type Scope = ImMap<Symbol, ScopeEntry>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScopeEntry {
    Stack { expr_type: Type, slot: StackSlot },
    Heap { expr_type: Type, ptr: Value },
    Arg { expr_type: Type, param: Value },
    Func { sig: Signature, func_id: FuncId },
}

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub cfg: TargetFrontendConfig,
    pub subs: Subs,
    pub interns: Interns,
    pub malloc: FuncId,
}

pub fn build_expr<'a, B: Backend>(
    env: &Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    expr: &Expr<'a>,
    procs: &Procs<'a>,
) -> Value {
    use crate::mono::expr::Expr::*;

    match expr {
        Int(num) => builder.ins().iconst(types::I64, *num),
        Float(num) => builder.ins().f64const(*num),
        Bool(val) => builder.ins().bconst(types::B1, *val),
        Byte(val) => builder.ins().iconst(types::I8, *val as i64),
        Cond {
            cond_lhs,
            cond_rhs,
            pass,
            fail,
            cond_layout,
            ret_var,
        } => {
            let branch = Branch2 {
                cond_lhs,
                cond_rhs,
                pass,
                fail,
                cond_layout,
                ret_var: *ret_var,
            };

            build_branch2(env, scope, module, builder, branch, procs)
        }
        Switch {
            cond,
            branches,
            default_branch,
            ret_var,
            cond_var,
        } => {
            let subs = &env.subs;
            let ret_content = subs.get_without_compacting(*ret_var).content;
            let ret_layout = Layout::from_content(env.arena, ret_content, subs)
                .unwrap_or_else(|_| panic!("TODO generate a runtime error in build_expr here!"));
            let ret_type = type_from_layout(env.cfg, &ret_layout);
            let switch_args = SwitchArgs {
                cond_var: *cond_var,
                cond_expr: cond,
                branches,
                default_branch,
                ret_type,
            };

            build_switch(env, scope, module, builder, switch_args, procs)
        }
        Store(ref stores, ref ret) => {
            let mut scope = im_rc::HashMap::clone(scope);
            let arena = &env.arena;
            let subs = &env.subs;
            let cfg = env.cfg;

            for (name, var, expr) in stores.iter() {
                let val = build_expr(env, &scope, module, builder, &expr, procs);
                let content = subs.get_without_compacting(*var).content;
                let layout = Layout::from_content(arena, content, subs)
                    .unwrap_or_else(|()| panic!("TODO generate a runtime error for this Store!"));
                let expr_type = type_from_layout(cfg, &layout);

                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    layout.stack_size(cfg),
                ));

                builder.ins().stack_store(val, slot, Offset32::new(0));

                // Make a new scope which includes the binding we just encountered.
                // This should be done *after* compiling the bound expr, since any
                // recursive (in the LetRec sense) bindings should already have
                // been extracted as procedures. Nothing in here should need to
                // access itself!
                scope = im_rc::HashMap::clone(&scope);

                scope.insert(*name, ScopeEntry::Stack { expr_type, slot });
            }

            build_expr(env, &scope, module, builder, ret, procs)
        }
        CallByName(ref symbol, ref args) => {
            let mut arg_vals = Vec::with_capacity_in(args.len(), env.arena);

            for arg in args.iter() {
                arg_vals.push(build_expr(env, scope, module, builder, arg, procs));
            }

            call_with_args(*symbol, arg_vals.into_bump_slice(), scope, module, builder)
        }
        FunctionPointer(ref name) => {
            let fn_id = match scope.get(name) {
                Some(ScopeEntry::Func{ func_id, .. }) => *func_id,
                other => panic!(
                    "FunctionPointer could not find function named {:?} in scope; instead, found {:?} in scope {:?}",
                    name, other, scope
                ),
            };

            let func_ref = module.declare_func_in_func(fn_id, &mut builder.func);

            builder.ins().func_addr(env.cfg.pointer_type(), func_ref)
        }
        CallByPointer(ref sub_expr, ref args, ref fn_var) => {
            let subs = &env.subs;
            let mut arg_vals = Vec::with_capacity_in(args.len(), env.arena);

            for arg in args.iter() {
                arg_vals.push(build_expr(env, scope, module, builder, arg, procs));
            }

            let content = subs.get_without_compacting(*fn_var).content;
            let layout = Layout::from_content(env.arena, content, &subs)
                .unwrap_or_else(|()| panic!("TODO generate a runtime error here!"));
            let sig = sig_from_layout(env.cfg, module, layout);
            let callee = build_expr(env, scope, module, builder, sub_expr, procs);
            let sig_ref = builder.import_signature(sig);
            let call = builder.ins().call_indirect(sig_ref, callee, &arg_vals);
            let results = builder.inst_results(call);

            debug_assert!(results.len() == 1);

            results[0]
        }
        Load(name) => match scope.get(name) {
            Some(ScopeEntry::Stack { expr_type, slot }) => {
                builder
                    .ins()
                    .stack_load(*expr_type, *slot, Offset32::new(0))
            }
            Some(ScopeEntry::Arg { param, .. }) => *param,
            Some(ScopeEntry::Heap { expr_type, ptr }) => {
                builder
                    .ins()
                    .load(*expr_type, MemFlags::new(), *ptr, Offset32::new(0))
            }
            Some(ScopeEntry::Func { .. }) => {
                panic!("TODO I don't yet know how to return fn pointers")
            }
            None => panic!("Could not find a var for {:?} in scope {:?}", name, scope),
        },
        Struct { layout, fields } => {
            let cfg = env.cfg;

            // Sort the fields
            let mut sorted_fields = Vec::with_capacity_in(fields.len(), env.arena);
            for field in fields.iter() {
                sorted_fields.push(field);
            }
            sorted_fields.sort_by_key(|k| &k.0);

            // Create a slot
            let slot = builder.create_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                layout.stack_size(cfg),
            ));

            // Create instructions for storing each field's expression
            for (index, (_, ref inner_expr)) in sorted_fields.iter().enumerate() {
                let val = build_expr(env, &scope, module, builder, inner_expr, procs);

                // Is there an existing function for this?
                let field_size = match inner_expr {
                    Int(_) => std::mem::size_of::<i64>(),
                    _ => panic!("I don't yet know how to calculate the offset for {:?} when building a cranelift struct", val),
                };
                let offset = i32::try_from(index * field_size)
                    .expect("TODO handle field size conversion to i32");

                builder.ins().stack_store(val, slot, Offset32::new(offset));
            }

            let ir_type = type_from_layout(cfg, layout);
            builder.ins().stack_addr(ir_type, slot, Offset32::new(0))
        }
        // Access {
        //     label,
        //     field_layout,
        //     struct_layout,
        // } => {
        //     panic!("I don't yet know how to crane build {:?}", expr);
        // }
        Str(str_literal) => {
            if str_literal.is_empty() {
                panic!("TODO build an empty string in Crane");
            } else {
                let bytes_len = str_literal.len() + 1/* TODO drop the +1 when we have structs and this is no longer a NUL-terminated CString.*/;
                let ptr = call_malloc(env, module, builder, bytes_len);
                let mem_flags = MemFlags::new();

                // Copy the bytes from the string literal into the array
                for (index, byte) in str_literal.bytes().enumerate() {
                    let val = builder.ins().iconst(types::I8, byte as i64);
                    let offset = Offset32::new(index as i32);

                    builder.ins().store(mem_flags, val, ptr, offset);
                }

                // Add a NUL terminator at the end.
                // TODO: Instead of NUL-terminating, return a struct
                // with the pointer and also the length and capacity.
                let nul_terminator = builder.ins().iconst(types::I8, 0);
                let index = bytes_len as u64 - 1;
                let offset = Offset32::new(index as i32);

                builder.ins().store(mem_flags, nul_terminator, ptr, offset);

                ptr
            }
        }
        _ => {
            panic!("I don't yet know how to crane build {:?}", expr);
        }
    }
}

struct Branch2<'a> {
    cond_lhs: &'a Expr<'a>,
    cond_rhs: &'a Expr<'a>,
    cond_layout: &'a Layout<'a>,
    pass: &'a Expr<'a>,
    fail: &'a Expr<'a>,
    ret_var: Variable,
}

fn build_branch2<'a, B: Backend>(
    env: &Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    branch: Branch2<'a>,
    procs: &Procs<'a>,
) -> Value {
    let subs = &env.subs;
    let ret_content = subs.get_without_compacting(branch.ret_var).content;
    let ret_layout = Layout::from_content(env.arena, ret_content, subs)
        .unwrap_or_else(|_| panic!("TODO generate a runtime error in build_branch2 here!"));
    let ret_type = type_from_layout(env.cfg, &ret_layout);
    // Declare a variable which each branch will mutate to be the value of that branch.
    // At the end of the expression, we will evaluate to this.
    let ret = cranelift::frontend::Variable::with_u32(0);

    // The block we'll jump to once the switch has completed.
    let ret_block = builder.create_block();

    builder.declare_var(ret, ret_type);

    let lhs = build_expr(env, scope, module, builder, branch.cond_lhs, procs);
    let rhs = build_expr(env, scope, module, builder, branch.cond_rhs, procs);
    let pass_block = builder.create_block();
    let fail_block = builder.create_block();

    match branch.cond_layout {
        Layout::Builtin(Builtin::Float64) => {
            // For floats, first do a `fcmp` comparison to get a bool answer about equality,
            // then use `brnz` to branch if that bool equality answer was nonzero (aka true).
            let is_eq = builder.ins().fcmp(FloatCC::Equal, lhs, rhs);

            builder.ins().brnz(is_eq, pass_block, &[]);
        }
        Layout::Builtin(Builtin::Int64) => {
            // For ints, we can compare and branch in the same instruction: `icmp`
            builder
                .ins()
                .br_icmp(IntCC::Equal, lhs, rhs, pass_block, &[]);
        }
        other => panic!("I don't know how to build a conditional for {:?}", other),
    }

    // Unconditionally jump to fail_block (if we didn't just jump to pass_block).
    builder.ins().jump(fail_block, &[]);

    let mut build_branch = |expr, block| {
        builder.switch_to_block(block);

        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);

        // Mutate the ret variable to be the outcome of this branch.
        let value = build_expr(env, scope, module, builder, expr, procs);

        builder.def_var(ret, value);

        // Unconditionally jump to ret_block, making the whole expression evaluate to ret.
        builder.ins().jump(ret_block, &[]);
    };

    build_branch(branch.pass, pass_block);
    build_branch(branch.fail, fail_block);

    // Finally, build ret_block - which contains our terminator instruction.
    {
        builder.switch_to_block(ret_block);
        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);

        // Now that ret has been mutated by the switch statement, evaluate to it.
        builder.use_var(ret)
    }
}
struct SwitchArgs<'a> {
    pub cond_expr: &'a Expr<'a>,
    pub cond_var: Variable,
    pub branches: &'a [(u64, Expr<'a>)],
    pub default_branch: &'a Expr<'a>,
    pub ret_type: Type,
}

fn build_switch<'a, B: Backend>(
    env: &Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    switch_args: SwitchArgs<'a>,
    procs: &Procs<'a>,
) -> Value {
    let mut switch = Switch::new();
    let SwitchArgs {
        branches,
        cond_expr,
        default_branch,
        ret_type,
        ..
    } = switch_args;
    let mut blocks = Vec::with_capacity_in(branches.len(), env.arena);

    // Declare a variable which each branch will mutate to be the value of that branch.
    // At the end of the expression, we will evaluate to this.
    let ret = cranelift::frontend::Variable::with_u32(0);

    builder.declare_var(ret, ret_type);

    // The block for the conditional's default branch.
    let default_block = builder.create_block();

    // The block we'll jump to once the switch has completed.
    let ret_block = builder.create_block();

    // Build the blocks for each branch, and register them in the switch.
    // Do this before emitting the switch, because it needs to be emitted at the front.
    for (int, _) in branches {
        let block = builder.create_block();

        blocks.push(block);

        switch.set_entry(*int, block);
    }

    // Run the switch. Each branch will mutate ret and then jump to ret_block.
    let cond = build_expr(env, scope, module, builder, cond_expr, procs);

    switch.emit(builder, cond, default_block);

    let mut build_branch = |block, expr| {
        builder.switch_to_block(block);
        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);

        // Mutate the ret variable to be the outcome of this branch.
        let value = build_expr(env, scope, module, builder, expr, procs);

        builder.def_var(ret, value);

        // Unconditionally jump to ret_block, making the whole expression evaluate to ret.
        builder.ins().jump(ret_block, &[]);
    };

    // Build the blocks for each branch
    for ((_, expr), block) in branches.iter().zip(blocks) {
        build_branch(block, expr);
    }

    // Build the block for the default branch
    build_branch(default_block, default_branch);

    // Finally, build ret_block - which contains our terminator instruction.
    {
        builder.switch_to_block(ret_block);
        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);

        // Now that ret has been mutated by the switch statement, evaluate to it.
        builder.use_var(ret)
    }
}

pub fn declare_proc<'a, B: Backend>(
    env: &Env<'a>,
    module: &mut Module<B>,
    symbol: Symbol,
    proc: &Proc<'a>,
) -> (FuncId, Signature) {
    let args = proc.args;
    let subs = &env.subs;
    let cfg = env.cfg;
    // TODO this Layout::from_content is duplicated when building this Proc
    let ret_content = subs.get_without_compacting(proc.ret_var).content;
    let ret_layout = Layout::from_content(env.arena, ret_content, subs)
        .unwrap_or_else(|_| panic!("TODO generate a runtime error in declare_proc here!"));
    let ret_type = type_from_layout(cfg, &ret_layout);

    // Create a signature for the function
    let mut sig = module.make_signature();

    // Add return type to the signature
    sig.returns.push(AbiParam::new(ret_type));

    // Add params to the signature
    for (layout, _name, _var) in args.iter() {
        let arg_type = type_from_layout(cfg, &layout);

        sig.params.push(AbiParam::new(arg_type));
    }

    // Declare the function in the module
    let fn_id = module
        .declare_function(symbol.ident_string(&env.interns), Linkage::Local, &sig)
        .unwrap_or_else(|err| panic!("Error when building function {:?} - {:?}", symbol, err));

    (fn_id, sig)
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
pub fn define_proc_body<'a, B: Backend>(
    env: &Env<'a>,
    ctx: &mut Context,
    module: &mut Module<B>,
    fn_id: FuncId,
    scope: &Scope,
    sig: Signature,
    proc: Proc<'a>,
    procs: &Procs<'a>,
) {
    let args = proc.args;
    let subs = &env.subs;
    let cfg = env.cfg;

    // Build the body of the function
    {
        let mut scope = scope.clone();
        let arena = env.arena;

        ctx.func.signature = sig;
        ctx.func.name = ExternalName::user(0, fn_id.as_u32());

        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let block = builder.create_block();

        builder.switch_to_block(block);
        builder.append_block_params_for_function_params(block);

        // Add args to scope
        for (&param, (_, arg_symbol, var)) in builder.block_params(block).iter().zip(args) {
            let content = subs.get_without_compacting(*var).content;
            // TODO this Layout::from_content is duplicated when building this Proc
            //
            let layout = Layout::from_content(arena, content, subs).unwrap_or_else(|()| {
                panic!("TODO generate a runtime error in define_proc_body here!")
            });
            let expr_type = type_from_layout(cfg, &layout);

            scope.insert(*arg_symbol, ScopeEntry::Arg { expr_type, param });
        }

        let body = build_expr(env, &scope, module, &mut builder, &proc.body, procs);

        builder.ins().return_(&[body]);
        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);
        builder.seal_all_blocks();

        builder.finalize();
    }

    module
        .define_function(fn_id, ctx)
        .expect("Defining Cranelift function failed");

    module.clear_context(ctx);
}

#[inline(always)]
fn call_with_args<'a, B: Backend>(
    symbol: Symbol,
    args: &'a [Value],
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
) -> Value {
    match symbol {
        Symbol::NUM_ADD => {
            debug_assert!(args.len() == 2);
            builder.ins().iadd(args[0], args[1])
        }
        Symbol::NUM_SUB => {
            debug_assert!(args.len() == 2);
            builder.ins().isub(args[0], args[1])
        }
        Symbol::NUM_MUL => {
            debug_assert!(args.len() == 2);
            builder.ins().imul(args[0], args[1])
        }
        _ => {
            let fn_id = match scope.get(&symbol) {
                    Some(ScopeEntry::Func{ func_id, .. }) => *func_id,
                    other => panic!(
                        "CallByName could not find function named {:?} in scope; instead, found {:?} in scope {:?}",
                        symbol, other, scope
                    ),
                };
            let local_func = module.declare_func_in_func(fn_id, &mut builder.func);
            let call = builder.ins().call(local_func, args);
            let results = builder.inst_results(call);

            debug_assert!(results.len() == 1);

            results[0]
        }
    }
}

fn call_malloc<B: Backend>(
    env: &Env<'_>,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    size: usize,
) -> Value {
    // Declare malloc inside this function
    let local_func = module.declare_func_in_func(env.malloc, &mut builder.func);

    // Convert the size argument to a Value
    let ptr_size_type = module.target_config().pointer_type();
    let size_arg = builder.ins().iconst(ptr_size_type, size as i64);

    // Call malloc and return the resulting pointer
    let call = builder.ins().call(local_func, &[size_arg]);
    let results = builder.inst_results(call);

    debug_assert!(results.len() == 1);

    results[0]
}
