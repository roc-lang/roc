use std::convert::TryFrom;

use bumpalo::collections::Vec;
use bumpalo::Bump;
use cranelift::frontend::Switch;
use cranelift::prelude::{
    AbiParam, ExternalName, FloatCC, FunctionBuilder, FunctionBuilderContext, IntCC, MemFlags,
    Variable,
};
use cranelift_codegen::ir::entities::{StackSlot, Value};
use cranelift_codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift_codegen::ir::{immediates::Offset32, types, InstBuilder, Signature, Type};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_codegen::Context;
use cranelift_module::{Backend, FuncId, Linkage, Module};

use crate::crane::convert::{sig_from_layout, type_from_layout};
use roc_collections::all::ImMap;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::expr::{Expr, Proc, Procs};
use roc_mono::layout::{Builtin, Layout};

type Scope = ImMap<Symbol, ScopeEntry>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScopeEntry {
    Stack { expr_type: Type, slot: StackSlot },
    Heap { expr_type: Type, ptr: Value },
    Arg { expr_type: Type, param: Value },
    Func { sig: Signature, func_id: FuncId },

    // For empty records and unit types, storing is a no-op and values can be created "out of thin
    // air". No need to use stack space for them.
    ZeroSized,
}

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub cfg: TargetFrontendConfig,
    pub interns: Interns,
    pub malloc: FuncId,
    pub variable_counter: &'a mut u32,
}

impl<'a> Env<'a> {
    /// This is necessary when you want usize or isize,
    /// because cfg.pointer_type() returns a pointer type,
    /// not an integer type, which casues verification to fail.
    pub fn ptr_sized_int(&self) -> Type {
        Type::int(self.cfg.pointer_bits() as u16).unwrap()
    }

    /// Cranelift creates variables by index.
    /// For nested conditionals, we need unique variables
    pub fn fresh_variable(&mut self) -> Variable {
        let result = cranelift::frontend::Variable::with_u32(*self.variable_counter);

        *self.variable_counter += 1;

        result
    }
}

pub fn build_expr<'a, B: Backend>(
    env: &mut Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    expr: &'a Expr<'a>,
    procs: &'a Procs<'a>,
) -> Value {
    use roc_mono::expr::Expr::*;

    let ptr_bytes = env.cfg.pointer_bytes() as u32;

    match expr {
        Int(num) => builder.ins().iconst(types::I64, *num),
        Float(num) => builder.ins().f64const(*num),
        Bool(val) => builder.ins().iconst(types::I8, *val as i64),
        Byte(val) => builder.ins().iconst(types::I8, *val as i64),
        Cond {
            branch_symbol,
            pass: (pass_stores, pass_expr),
            fail: (fail_stores, fail_expr),
            cond_layout,
            ret_layout,
            ..
        } => {
            let cond_value = load_symbol(env, scope, builder, *branch_symbol);

            let pass = env.arena.alloc(Expr::Store(pass_stores, pass_expr));
            let fail = env.arena.alloc(Expr::Store(fail_stores, fail_expr));

            let branch = Branch2 {
                cond: cond_value,
                pass,
                fail,
                cond_layout,
                ret_layout,
            };

            build_branch2(env, scope, module, builder, branch, procs)
        }
        Switch {
            cond,
            branches,
            default_branch: (default_stores, default_expr),
            ret_layout,
            cond_layout,
        } => {
            let ret_type = type_from_layout(env.cfg, &ret_layout);

            let default_branch = env.arena.alloc(Expr::Store(default_stores, default_expr));

            let mut combined = Vec::with_capacity_in(branches.len(), env.arena);

            for (int, stores, expr) in branches.iter() {
                combined.push((*int, Expr::Store(stores, expr)));
            }

            let switch_args = SwitchArgs {
                cond_layout,
                cond_expr: cond,
                branches: combined.into_bump_slice(),
                default_branch,
                ret_type,
            };

            build_switch(env, scope, module, builder, switch_args, procs)
        }
        Store(stores, ret) => {
            let mut scope = im_rc::HashMap::clone(scope);
            let cfg = env.cfg;

            let ptr_size = cfg.pointer_bytes() as u32;
            for (name, layout, expr) in stores.iter() {
                let stack_size = layout.stack_size(ptr_size);

                if stack_size == 0 {
                    scope.insert(*name, ScopeEntry::ZeroSized);
                    continue;
                }

                let val = build_expr(env, &scope, module, builder, &expr, procs);
                let expr_type = type_from_layout(cfg, &layout);

                let slot = builder
                    .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, stack_size));

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
        CallByName(symbol, args) => call_by_name(env, *symbol, args, scope, module, builder, procs),
        FunctionPointer(name) => {
            let fn_id = match scope.get(name) {
                Some(ScopeEntry::Func{ func_id, .. }) => *func_id,
                other => panic!(
                    "FunctionPointer could not find function named {:?} declared in scope (and it was not special-cased in crane::build as a builtin); instead, found {:?} in scope {:?}", name, other, scope),
            };

            let func_ref = module.declare_func_in_func(fn_id, &mut builder.func);

            builder.ins().func_addr(env.cfg.pointer_type(), func_ref)
        }
        CallByPointer(sub_expr, args, layout) => {
            let mut arg_vals = Vec::with_capacity_in(args.len(), env.arena);

            for arg in args.iter() {
                arg_vals.push(build_expr(env, scope, module, builder, arg, procs));
            }

            let sig = sig_from_layout(env.cfg, module, layout);
            let callee = build_expr(env, scope, module, builder, sub_expr, procs);
            let sig_ref = builder.import_signature(sig);
            let call = builder.ins().call_indirect(sig_ref, callee, &arg_vals);
            let results = builder.inst_results(call);

            debug_assert!(results.len() == 1);

            results[0]
        }
        Load(name) => load_symbol(env, scope, builder, *name),
        Struct(sorted_fields) => {
            let cfg = env.cfg;

            // The slot size will be the sum of all the fields' sizes
            let mut slot_size = 0;

            for (_, field_layout) in sorted_fields.iter() {
                slot_size += field_layout.stack_size(ptr_bytes);
            }

            // Create a slot
            let slot = builder
                .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size));

            // Create instructions for storing each field's expression
            // NOTE assumes that all fields have the same width!
            for (index, (field_expr, field_layout)) in sorted_fields.iter().enumerate() {
                let val = build_expr(env, &scope, module, builder, field_expr, procs);

                let field_size = field_layout.stack_size(ptr_bytes);
                let offset = i32::try_from(index * field_size as usize)
                    .expect("TODO handle field size conversion to i32");

                builder.ins().stack_store(val, slot, Offset32::new(offset));
            }

            builder
                .ins()
                .stack_addr(cfg.pointer_type(), slot, Offset32::new(0))
        }
        Tag {
            tag_layout,
            arguments,
            ..
        } => {
            let cfg = env.cfg;
            let ptr_bytes = cfg.pointer_bytes() as u32;

            // NOTE: all variants of a tag union must have the same size, so (among other things)
            // it's easy to quickly index them in arrays. Therefore the size of this tag doens't
            // depend on the tag arguments, but solely on the layout of the whole tag union
            let slot_size = tag_layout.stack_size(ptr_bytes);

            // Create a slot
            let slot = builder
                .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot_size));

            // Create instructions for storing each field's expression
            let mut offset = 0;

            for (field_expr, field_layout) in arguments.iter() {
                let val = build_expr(env, &scope, module, builder, field_expr, procs);

                let field_size = field_layout.stack_size(ptr_bytes);
                let field_offset =
                    i32::try_from(offset).expect("TODO handle field size conversion to i32");

                builder
                    .ins()
                    .stack_store(val, slot, Offset32::new(field_offset));

                offset += field_size;
            }

            builder
                .ins()
                .stack_addr(cfg.pointer_type(), slot, Offset32::new(0))
        }
        AccessAtIndex {
            index,
            field_layouts,
            expr,
            ..
        } => {
            let cfg = env.cfg;
            let mut offset = 0;

            for (field_index, field_layout) in field_layouts.iter().enumerate() {
                if *index == field_index as u64 {
                    let offset = i32::try_from(offset)
                        .expect("TODO gracefully handle usize -> i32 conversion in struct access");

                    let mem_flags = MemFlags::new();
                    let expr = build_expr(env, scope, module, builder, expr, procs);

                    let ret_type = type_from_layout(cfg, field_layout);

                    return builder
                        .ins()
                        .load(ret_type, mem_flags, expr, Offset32::new(offset));
                }

                offset += field_layout.stack_size(ptr_bytes);
            }

            panic!(
                "field access out of bounds: index {:?} in layouts {:?}",
                index, field_layouts
            )
        }

        Str(str_literal) => {
            if str_literal.is_empty() {
                panic!("TODO build an empty string in Crane");
            } else {
                let bytes_len = str_literal.len() + 1/* TODO drop the +1 when we have structs and this is no longer a NUL-terminated CString.*/;
                let size = builder.ins().iconst(types::I64, bytes_len as i64);
                let ptr = call_malloc(env, module, builder, size);
                // TODO check if malloc returned null; if so, runtime error for OOM!
                let mem_flags = MemFlags::new();

                // Store the bytes from the string literal in the array
                for (index, byte) in str_literal.bytes().enumerate() {
                    let val = builder.ins().iconst(types::I8, byte as i64);
                    let offset = Offset32::new(index as i32);

                    builder.ins().store(mem_flags, val, ptr, offset);
                }

                // Add a NUL terminator at the end.
                // TODO: Instead of NUL-terminating, return a struct
                // with the pointer and also the length and capacity.
                let nul_terminator = builder.ins().iconst(types::I8, 0);
                let index = bytes_len as i32 - 1;
                let offset = Offset32::new(index);

                builder.ins().store(mem_flags, nul_terminator, ptr, offset);

                ptr
            }
        }
        Array { elem_layout, elems } => {
            let cfg = env.cfg;
            let ptr_bytes = cfg.pointer_bytes() as u32;
            let slot = builder.create_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                ptr_bytes * Builtin::LIST_WORDS,
            ));

            let elems_ptr = if elems.is_empty() {
                // Empty lists get a null pointer so they don't allocate on the heap
                builder.ins().iconst(cfg.pointer_type(), 0)
            } else {
                let elem_bytes = elem_layout.stack_size(ptr_bytes as u32);
                let bytes_len = elem_bytes as usize * elems.len();
                let size = builder.ins().iconst(types::I64, bytes_len as i64);
                let elems_ptr = call_malloc(env, module, builder, size);

                // TODO check if malloc returned null; if so, runtime error for OOM!
                let mem_flags = MemFlags::new();

                // Copy the elements from the literal into the array
                for (index, elem) in elems.iter().enumerate() {
                    let offset = Offset32::new(elem_bytes as i32 * index as i32);
                    let val = build_expr(env, scope, module, builder, elem, procs);

                    builder.ins().store(mem_flags, val, elems_ptr, offset);
                }

                elems_ptr
            };

            // Store the pointer
            {
                let offset = Offset32::new((Builtin::WRAPPER_PTR * ptr_bytes) as i32);

                builder.ins().stack_store(elems_ptr, slot, offset);
            }

            // Store the length
            {
                let length = builder
                    .ins()
                    .iconst(env.ptr_sized_int(), elems.len() as i64);
                let offset = Offset32::new((Builtin::WRAPPER_LEN * ptr_bytes) as i32);

                builder.ins().stack_store(length, slot, offset);
            }

            // Return the pointer to the wrapper
            builder
                .ins()
                .stack_addr(cfg.pointer_type(), slot, Offset32::new(0))
        }
        _ => {
            panic!("I don't yet know how to crane build {:?}", expr);
        }
    }
}

fn load_symbol<'a>(
    env: &mut Env<'a>,
    scope: &Scope,
    builder: &mut FunctionBuilder,
    name: Symbol,
) -> Value {
    match scope.get(&name) {
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
            Some(ScopeEntry::ZeroSized) => {
                // Create a slot
                let slot = builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    0
                ));

                builder
                    .ins()
                    .stack_addr(env.cfg.pointer_type(), slot, Offset32::new(0))
            }
            None => panic!(
                "Could not resolve lookup for {:?} because no ScopeEntry was found for {:?} in scope {:?}",
                name, name, scope
            ),
        }
}

struct Branch2<'a> {
    cond: Value,
    cond_layout: &'a Layout<'a>,
    pass: &'a Expr<'a>,
    fail: &'a Expr<'a>,
    ret_layout: &'a Layout<'a>,
}

fn build_branch2<'a, B: Backend>(
    env: &mut Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    branch: Branch2<'a>,
    procs: &'a Procs<'a>,
) -> Value {
    let ret_layout = branch.ret_layout;
    let ret_type = type_from_layout(env.cfg, &ret_layout);
    // Declare a variable which each branch will mutate to be the value of that branch.
    // At the end of the expression, we will evaluate to this.
    let ret = env.fresh_variable();

    // The block we'll jump to once the switch has completed.
    let ret_block = builder.create_block();

    builder.declare_var(ret, ret_type);

    let cond = branch.cond;
    let pass_block = builder.create_block();
    let fail_block = builder.create_block();

    match branch.cond_layout {
        Layout::Builtin(Builtin::Bool) => {
            builder.ins().brz(cond, fail_block, &[]);
        }
        other => panic!("I don't know how to build a conditional for {:?}", other),
    }

    // Unconditionally jump to pass_block (if we didn't just jump to pass_block).
    builder.ins().jump(pass_block, &[]);

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
    pub cond_layout: &'a Layout<'a>,
    pub branches: &'a [(u64, Expr<'a>)],
    pub default_branch: &'a Expr<'a>,
    pub ret_type: Type,
}

fn build_switch<'a, B: Backend>(
    env: &mut Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    switch_args: SwitchArgs<'a>,
    procs: &'a Procs<'a>,
) -> Value {
    let mut switch = Switch::new();
    let SwitchArgs {
        branches,
        cond_expr,
        cond_layout,
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
    let cond = match cond_layout {
        Layout::Union(_) => {
            let new_cond_layout = &Layout::Builtin(Builtin::Int64);
            // load the tag discriminator
            let full_cond = build_expr(env, scope, module, builder, cond_expr, procs);

            let mem_flags = MemFlags::new();

            let ret_type = type_from_layout(env.cfg, &new_cond_layout);

            builder
                .ins()
                .load(ret_type, mem_flags, full_cond, Offset32::new(0))
        }
        Layout::Builtin(Builtin::Float64) => {
            // for floats, we match on the bit pattern
            let full_cond = build_expr(env, scope, module, builder, cond_expr, procs);

            builder.ins().bitcast(types::I64, full_cond)
        }
        Layout::Builtin(_) => build_expr(env, scope, module, builder, cond_expr, procs),
        other => todo!("extract the switch value for layout {:?}", other),
    };

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
    env: &mut Env<'a>,
    module: &mut Module<B>,
    symbol: Symbol,
    proc: &Proc<'a>,
) -> (FuncId, Signature) {
    let args = proc.args;
    let cfg = env.cfg;
    // TODO this Layout::from_content is duplicated when building this Proc
    let ret_type = type_from_layout(cfg, &proc.ret_layout);

    // Create a signature for the function
    let mut sig = module.make_signature();

    // Add return type to the signature
    sig.returns.push(AbiParam::new(ret_type));

    // Add params to the signature
    for (layout, _name) in args.iter() {
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
    env: &mut Env<'a>,
    ctx: &mut Context,
    module: &mut Module<B>,
    fn_id: FuncId,
    scope: &Scope,
    sig: Signature,
    proc: &'a Proc<'a>,
    procs: &'a Procs<'a>,
) {
    let args = proc.args;
    let cfg = env.cfg;

    // Build the body of the function
    {
        let mut scope = scope.clone();

        ctx.func.signature = sig;
        ctx.func.name = ExternalName::user(0, fn_id.as_u32());

        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let block = builder.create_block();

        builder.switch_to_block(block);
        builder.append_block_params_for_function_params(block);

        // Add args to scope
        for (&param, (layout, arg_symbol)) in builder.block_params(block).iter().zip(args) {
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

fn build_arg<'a, B: Backend>(
    (arg, _): &'a (Expr<'a>, Layout<'a>),
    env: &mut Env<'a>,
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    procs: &'a Procs<'a>,
) -> Value {
    build_expr(env, scope, module, builder, arg, procs)
}

#[inline(always)]
#[allow(clippy::cognitive_complexity)]
fn call_by_name<'a, B: Backend>(
    env: &mut Env<'a>,
    symbol: Symbol,
    args: &'a [(Expr<'a>, Layout<'a>)],
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    procs: &'a Procs<'a>,
) -> Value {
    match symbol {
        Symbol::INT_ADD | Symbol::NUM_ADD => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            builder.ins().iadd(a, b)
        }
        Symbol::FLOAT_ADD => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            builder.ins().fadd(a, b)
        }
        Symbol::INT_SUB | Symbol::NUM_SUB => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            builder.ins().isub(a, b)
        }
        Symbol::FLOAT_SUB => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            builder.ins().fsub(a, b)
        }
        Symbol::NUM_MUL => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            builder.ins().imul(a, b)
        }
        Symbol::NUM_NEG => {
            debug_assert!(args.len() == 1);
            let num = build_arg(&args[0], env, scope, module, builder, procs);

            builder.ins().ineg(num)
        }
        Symbol::LIST_LEN => {
            debug_assert!(args.len() == 1);

            let list_ptr = build_arg(&args[0], env, scope, module, builder, procs);
            let ptr_bytes = env.cfg.pointer_bytes() as u32;
            let offset = Offset32::new((Builtin::WRAPPER_LEN * ptr_bytes) as i32);

            // Get the usize list length
            builder
                .ins()
                .load(env.ptr_sized_int(), MemFlags::new(), list_ptr, offset)
        }
        Symbol::INT_EQ_I64 | Symbol::INT_EQ_I8 => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            let result = builder.ins().icmp(IntCC::Equal, a, b);

            // convert to how we store bools (as I8)
            builder.ins().bint(types::I8, result)
        }
        Symbol::INT_EQ_I1 => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            let result = builder.ins().icmp(IntCC::Equal, a, b);

            // convert to how we store bools (as I8)
            builder.ins().bint(types::I8, result)
        }
        Symbol::FLOAT_EQ => {
            debug_assert!(args.len() == 2);
            let a = build_arg(&args[0], env, scope, module, builder, procs);
            let b = build_arg(&args[1], env, scope, module, builder, procs);

            let result = builder.ins().fcmp(FloatCC::Equal, a, b);

            // convert to how we store bools (as I8)
            builder.ins().bint(types::I8, result)
        }
        Symbol::BOOL_OR => {
            debug_assert!(args.len() == 2);

            let cond = build_arg(&args[0], env, scope, module, builder, procs);

            let branch2 = Branch2 {
                cond,
                cond_layout: &Layout::Builtin(Builtin::Bool),
                pass: &Expr::Bool(true),
                fail: &args[1].0,
                ret_layout: &Layout::Builtin(Builtin::Bool),
            };
            build_branch2(env, scope, module, builder, branch2, procs)
        }
        Symbol::BOOL_AND => {
            debug_assert!(args.len() == 2);

            let cond = build_arg(&args[0], env, scope, module, builder, procs);

            let branch2 = Branch2 {
                cond,
                cond_layout: &Layout::Builtin(Builtin::Bool),
                pass: &args[1].0,
                fail: &Expr::Bool(false),
                ret_layout: &Layout::Builtin(Builtin::Bool),
            };
            build_branch2(env, scope, module, builder, branch2, procs)
        }
        Symbol::LIST_GET_UNSAFE => {
            debug_assert!(args.len() == 2);

            let wrapper_ptr = build_arg(&args[0], env, scope, module, builder, procs);
            let elem_index = build_arg(&args[1], env, scope, module, builder, procs);

            let (_, list_layout) = &args[0];

            // Get the usize list length
            let ptr_bytes = env.cfg.pointer_bytes() as u32;
            let offset = Offset32::new((Builtin::WRAPPER_LEN * ptr_bytes) as i32);
            let _list_len =
                builder
                    .ins()
                    .load(env.ptr_sized_int(), MemFlags::new(), wrapper_ptr, offset);

            // TODO compare elem_index to _list_len to do array bounds checking.

            match list_layout {
                Layout::Builtin(Builtin::List(elem_layout)) => {
                    let cfg = env.cfg;
                    let elem_type = type_from_layout(cfg, elem_layout);
                    let elem_bytes = elem_layout.stack_size(cfg.pointer_bytes() as u32);
                    let elem_size = builder.ins().iconst(types::I64, elem_bytes as i64);

                    // Load the pointer we got to the wrapper struct
                    let elems_ptr = builder.ins().load(
                        env.cfg.pointer_type(),
                        MemFlags::new(),
                        wrapper_ptr,
                        Offset32::new(0),
                    );

                    // Multiply the requested index by the size of each element.
                    let offset = builder.ins().imul(elem_index, elem_size);

                    // Follow the pointer in the wrapper struct to the actual elements
                    builder.ins().load_complex(
                        elem_type,
                        MemFlags::new(),
                        &[elems_ptr, offset],
                        Offset32::new(0),
                    )
                }
                _ => {
                    unreachable!("Invalid List layout for List.get: {:?}", list_layout);
                }
            }
        }
        Symbol::LIST_SET => list_set(env, args, scope, module, builder, procs, InPlace::Clone),
        Symbol::LIST_SET_IN_PLACE => {
            list_set(env, args, scope, module, builder, procs, InPlace::InPlace)
        }
        _ => {
            let fn_id = match scope.get(&symbol) {
                    Some(ScopeEntry::Func{ func_id, .. }) => *func_id,
                    other => panic!(
                        "CallByName could not find function named {:?} declared in scope (and it was not special-cased in crane::build as a builtin); instead, found {:?} in scope {:?}",
                        symbol, other, scope
                    ),
                };
            let local_func = module.declare_func_in_func(fn_id, &mut builder.func);
            let mut arg_vals = Vec::with_capacity_in(args.len(), env.arena);

            for (arg, _layout) in args {
                arg_vals.push(build_expr(env, scope, module, builder, arg, procs));
            }

            let call = builder.ins().call(local_func, arg_vals.into_bump_slice());
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
    size: Value,
) -> Value {
    // Declare malloc inside this function
    let local_func = module.declare_func_in_func(env.malloc, &mut builder.func);

    // Call malloc and return the resulting pointer
    let call = builder.ins().call(local_func, &[size]);
    let results = builder.inst_results(call);

    debug_assert!(results.len() == 1);

    results[0]
}

fn list_set_in_place<'a>(
    env: &mut Env<'a>,
    wrapper_ptr: Value,
    elem_index: Value,
    elem: Value,
    elem_layout: &Layout<'a>,
    builder: &mut FunctionBuilder,
) -> Value {
    let elems_ptr = builder.ins().load(
        env.cfg.pointer_type(),
        MemFlags::new(),
        wrapper_ptr,
        Offset32::new(Builtin::WRAPPER_PTR as i32),
    );

    let elem_bytes = elem_layout.stack_size(env.cfg.pointer_bytes() as u32);
    let elem_size = builder.ins().iconst(types::I64, elem_bytes as i64);

    // Multiply the requested index by the size of each element.
    let offset = builder.ins().imul(elem_index, elem_size);

    builder.ins().store_complex(
        MemFlags::new(),
        elem,
        &[elems_ptr, offset],
        Offset32::new(0),
    );

    wrapper_ptr
}

fn load_list_len(env: &Env<'_>, builder: &mut FunctionBuilder, src_wrapper_ptr: Value) -> Value {
    let ptr_bytes = env.cfg.pointer_bytes() as u32;
    let offset = Offset32::new((Builtin::WRAPPER_LEN * ptr_bytes) as i32);

    builder.ins().load(
        env.ptr_sized_int(),
        MemFlags::new(),
        src_wrapper_ptr,
        offset,
    )
}

fn load_list_ptr(env: &Env<'_>, builder: &mut FunctionBuilder, src_wrapper_ptr: Value) -> Value {
    let ptr_bytes = env.cfg.pointer_bytes() as u32;
    let offset = Offset32::new((Builtin::WRAPPER_PTR * ptr_bytes) as i32);

    builder.ins().load(
        env.ptr_sized_int(),
        MemFlags::new(),
        src_wrapper_ptr,
        offset,
    )
}

fn clone_list<B: Backend>(
    env: &Env<'_>,
    builder: &mut FunctionBuilder,
    module: &mut Module<B>,
    src_wrapper_ptr: Value,
    elem_layout: &Layout<'_>,
) -> Value {
    let cfg = env.cfg;

    // Load the pointer we got to the wrapper struct
    let elems_ptr = load_list_ptr(env, builder, src_wrapper_ptr);

    // Get the usize list length
    let list_len = load_list_len(env, builder, src_wrapper_ptr);

    // Calculate the number of bytes we'll need to allocate.
    let elem_bytes = builder.ins().iconst(
        env.ptr_sized_int(),
        elem_layout.stack_size(cfg.pointer_bytes() as u32) as i64,
    );
    let size = builder.ins().imul(elem_bytes, list_len);

    // Allocate space for the new array that we'll copy into.
    let new_elems_ptr = call_malloc(env, module, builder, size);

    // TODO check if malloc returned null; if so, runtime error for OOM!

    // Either memcpy or deep clone the array elements
    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder.call_memcpy(env.cfg, new_elems_ptr, elems_ptr, size);
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    // Create a fresh wrapper struct for the newly populated array
    let ptr_bytes = cfg.pointer_bytes() as u32;
    let slot = builder.create_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        ptr_bytes * Builtin::LIST_WORDS,
    ));

    // Store the new pointer in the wrapper
    builder.ins().stack_store(
        new_elems_ptr,
        slot,
        Offset32::new((Builtin::WRAPPER_PTR * ptr_bytes) as i32),
    );

    // Store the length in slot the wrapper
    builder.ins().stack_store(
        list_len,
        slot,
        Offset32::new((Builtin::WRAPPER_LEN * ptr_bytes) as i32),
    );

    // Return the wrapper struct
    builder
        .ins()
        .stack_addr(cfg.pointer_type(), slot, Offset32::new(0))
}

enum InPlace {
    InPlace,
    Clone,
}

fn list_set<'a, B: Backend>(
    env: &mut Env<'a>,
    args: &'a [(Expr<'a>, Layout<'a>)],
    scope: &Scope,
    module: &mut Module<B>,
    builder: &mut FunctionBuilder,
    procs: &'a Procs<'a>,
    in_place: InPlace,
) -> Value {
    // set : List elem, Int, elem -> List elem
    let original_wrapper = build_arg(&args[0], env, scope, module, builder, procs);
    let (_, list_layout) = &args[0];

    // Get the usize list length
    let list_len = load_list_len(env, builder, original_wrapper);

    // Get the element to set
    let elem_index = build_arg(&args[1], env, scope, module, builder, procs);

    let pass_block = builder.create_block();
    let fail_block = builder.create_block();

    let ret_type = type_from_layout(env.cfg, &list_layout);
    // Declare a variable which each branch will mutate to be the value of that branch.
    // At the end of the expression, we will evaluate to this.
    let ret = env.fresh_variable();

    // The block we'll jump to once the switch has completed.
    let ret_block = builder.create_block();

    builder.declare_var(ret, ret_type);

    // Bounds check: only proceed if index < length.
    // Otherwise, return the list unaltered.
    let comparison =
        // Note: Check for index < length as the "true" condition,
        // to avoid misprediction. (In practice this should usually pass,
        // and CPUs generally default to predicting that a forward jump
        // shouldn't be taken; that is, they predict "else" won't be taken.)
        builder.ins().icmp(IntCC::UnsignedLessThan, list_len, elem_index);

    builder.ins().brz(comparison, fail_block, &[]);

    // Unconditionally jump to pass_block (if we didn't just jump to pass_block).
    builder.ins().jump(pass_block, &[]);

    // If the index is in bounds, clone and mutate in place.
    {
        builder.switch_to_block(pass_block);

        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);

        let then_val = match list_layout {
            Layout::Builtin(Builtin::List(elem_layout)) => {
                let wrapper_ptr = match in_place {
                    InPlace::InPlace => original_wrapper,
                    InPlace::Clone => {
                        clone_list(env, builder, module, original_wrapper, elem_layout)
                    }
                };
                let elem = build_arg(&args[2], env, scope, module, builder, procs);

                list_set_in_place(env, wrapper_ptr, elem_index, elem, elem_layout, builder)
            }
            _ => {
                unreachable!("Invalid List layout for List.set: {:?}", list_layout);
            }
        };

        // Mutate the ret variable to be the outcome of this branch.
        builder.def_var(ret, then_val);

        // Unconditionally jump to ret_block, making the whole expression evaluate to ret.
        builder.ins().jump(ret_block, &[]);
    }

    // If the index was out of bounds, return the original list unaltered.
    {
        builder.switch_to_block(fail_block);

        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);

        // Mutate the ret variable to be the outcome of this branch.
        builder.def_var(ret, original_wrapper);

        // Unconditionally jump to ret_block, making the whole expression evaluate to ret.
        builder.ins().jump(ret_block, &[]);
    }

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
