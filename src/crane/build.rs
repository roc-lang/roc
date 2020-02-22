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
use inlinable_string::InlinableString;

use crate::collections::ImMap;
use crate::crane::convert::{sig_from_layout, type_from_layout, type_from_var};
use crate::mono::expr::{Expr, Proc, Procs};
use crate::mono::layout::{Builtin, Layout};
use crate::subs::{Subs, Variable};

type Scope = ImMap<InlinableString, ScopeEntry>;

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
            let ret_type = type_from_var(*ret_var, &env.subs, env.cfg);
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
                let expr_type = type_from_layout(cfg, &layout, subs);

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

                scope.insert(name.clone(), ScopeEntry::Stack { expr_type, slot });
            }

            build_expr(env, &scope, module, builder, ret, procs)
        }
        CallByName(ref name, ref args) => {
            // TODO try one of these alternative strategies (preferably the latter):
            //
            // 1. use SIMD string comparison to compare these strings faster
            // 2. pre-register Bool.or using module.add_function, and see if LLVM inlines it
            // 3. intern all these strings
            if name == "Bool.or" {
                panic!("TODO create a branch for ||");
            } else if name == "Bool.and" {
                panic!("TODO create a branch for &&");
            } else {
                let mut arg_vals = Vec::with_capacity_in(args.len(), env.arena);

                for arg in args.iter() {
                    arg_vals.push(build_expr(env, scope, module, builder, arg, procs));
                }

                let fn_id = match scope.get(name) {
                    Some(ScopeEntry::Func{ func_id, .. }) => *func_id,
                    other => panic!(
                        "CallByName could not find function named {:?} in scope; instead, found {:?} in scope {:?}",
                        name, other, scope
                    ),
                };
                let local_func = module.declare_func_in_func(fn_id, &mut builder.func);
                let call = builder.ins().call(local_func, &arg_vals);
                let results = builder.inst_results(call);

                debug_assert!(results.len() == 1);

                results[0]
            }
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
            let sig = sig_from_layout(env.cfg, module, layout, &subs);
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
            let subs = &env.subs;
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

            let ir_type = type_from_layout(cfg, layout, subs);
            builder.ins().stack_load(ir_type, slot, Offset32::new(0))
        }
        Access {
            label,
            field_layout,
            struct_layout: Layout::Struct(fields),
        } => {
            let cfg = env.cfg;

            // Reconstruct and sort the struct. Why does the struct_layout not contain all the fields?
            let mut fields = Vec::with_capacity_in(fields.len() + 1, env.arena);
            fields.push((label, field_layout));

            // Sort them, TODO: refactor duplicated code here and in the Record pattern above
            let mut sorted_fields = Vec::with_capacity_in(fields.len(), env.arena);
            for field in fields.iter() {
                sorted_fields.push(field);
            }
            sorted_fields.sort_by_key(|k| &k.0);

            // Get index of the field to access
            let target_index = sorted_fields
                .iter()
                .position(|(k, v)| k == &label)
                .expect("TODO: gracefully handle field label index not found");

            let offset = sorted_fields
                .iter()
                .take(target_index + 1)
                .map(|(_, layout)| match layout {
                    Layout::Builtin(Builtin::Int64) => std::mem::size_of::<i64>(),
                    _ => panic!(
                        "Missing struct field size in offset calculation for struct access for {:?}",
                        layout
                    ),
                })
                .sum();

            let offset = i32::try_from(offset)
                .expect("TODO gracefully handle usize -> i32 conversion in struct access");

            // Todo -> where do I get the slot from?
            builder
                .ins()
                .stack_load(cfg.pointer_type(), slot, Offset32::new(offset))
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
    let cfg = env.cfg;

    // Declare a variable which each branch will mutate to be the value of that branch.
    // At the end of the expression, we will evaluate to this.
    let ret_type = type_from_var(branch.ret_var, subs, cfg);
    let ret = cranelift::frontend::Variable::with_u32(0);

    // The block we'll jump to once the switch has completed.
    let ret_block = builder.create_ebb();

    builder.declare_var(ret, ret_type);

    let lhs = build_expr(env, scope, module, builder, branch.cond_lhs, procs);
    let rhs = build_expr(env, scope, module, builder, branch.cond_rhs, procs);
    let pass_block = builder.create_ebb();
    let fail_block = builder.create_ebb();

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

        // TODO re-enable this once Switch stops making unsealed
        // EBBs, e.g. https://docs.rs/cranelift-frontend/0.52.0/src/cranelift_frontend/switch.rs.html#143
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
        // TODO re-enable this once Switch stops making unsealed
        // EBBs, e.g. https://docs.rs/cranelift-frontend/0.52.0/src/cranelift_frontend/switch.rs.html#143
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
    let default_block = builder.create_ebb();

    // The block we'll jump to once the switch has completed.
    let ret_block = builder.create_ebb();

    // Build the blocks for each branch, and register them in the switch.
    // Do this before emitting the switch, because it needs to be emitted at the front.
    for (int, _) in branches {
        let block = builder.create_ebb();

        blocks.push(block);

        switch.set_entry(*int, block);
    }

    // Run the switch. Each branch will mutate ret and then jump to ret_ebb.
    let cond = build_expr(env, scope, module, builder, cond_expr, procs);

    switch.emit(builder, cond, default_block);

    let mut build_branch = |block, expr| {
        builder.switch_to_block(block);
        // TODO re-enable this once Switch stops making unsealed
        // EBBs, e.g. https://docs.rs/cranelift-frontend/0.52.0/src/cranelift_frontend/switch.rs.html#143
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
        // TODO re-enable this once Switch stops making unsealed
        // EBBs, e.g. https://docs.rs/cranelift-frontend/0.52.0/src/cranelift_frontend/switch.rs.html#143
        // builder.seal_block(block);

        // Now that ret has been mutated by the switch statement, evaluate to it.
        builder.use_var(ret)
    }
}

pub fn declare_proc<'a, B: Backend>(
    env: &Env<'a>,
    module: &mut Module<B>,
    name: InlinableString,
    proc: &Proc<'a>,
) -> (FuncId, Signature) {
    let args = proc.args;
    let subs = &env.subs;
    let cfg = env.cfg;
    // TODO this rtype_from_var is duplicated when building this Proc
    let ret_type = type_from_var(proc.ret_var, subs, env.cfg);

    // Create a signature for the function
    let mut sig = module.make_signature();

    // Add return type to the signature
    sig.returns.push(AbiParam::new(ret_type));

    // Add params to the signature
    for (layout, _name, _var) in args.iter() {
        let arg_type = type_from_layout(cfg, &layout, subs);

        sig.params.push(AbiParam::new(arg_type));
    }

    // Declare the function in the module
    let fn_id = module
        .declare_function(&name, Linkage::Local, &sig)
        .unwrap_or_else(|err| panic!("Error when building function {:?} - {:?}", name, err));

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

        let block = builder.create_ebb();

        builder.switch_to_block(block);
        builder.append_ebb_params_for_function_params(block);

        // Add args to scope
        for (&param, (_, arg_name, var)) in builder.ebb_params(block).iter().zip(args) {
            let content = subs.get_without_compacting(*var).content;
            // TODO this type_from_content is duplicated when building this Proc
            //
            let layout = Layout::from_content(arena, content, subs)
                .unwrap_or_else(|()| panic!("TODO generate a runtime error here!"));
            let expr_type = type_from_layout(cfg, &layout, subs);

            scope.insert(arg_name.clone(), ScopeEntry::Arg { expr_type, param });
        }

        let body = build_expr(env, &scope, module, &mut builder, &proc.body, procs);

        builder.ins().return_(&[body]);
        // TODO re-enable this once Switch stops making unsealed
        // EBBs, e.g. https://docs.rs/cranelift-frontend/0.52.0/src/cranelift_frontend/switch.rs.html#143
        // builder.seal_block(block);
        builder.seal_all_blocks();

        builder.finalize();
    }

    module
        .define_function(fn_id, ctx)
        .expect("Defining Cranelift function failed");

    module.clear_context(ctx);
}
