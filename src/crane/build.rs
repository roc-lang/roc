use bumpalo::collections::Vec;
use bumpalo::Bump;
use cranelift::frontend::Switch;
use cranelift::prelude::{
    AbiParam, ExternalName, FunctionBuilder, FunctionBuilderContext, MemFlags,
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
use crate::mono::layout::Layout;
use crate::subs::{Content, FlatType, Subs, Variable};

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
        // Cond {
        //     cond_lhs,
        //     cond_rhs,
        //     pass,
        //     fail,
        //     ret_var,
        // } => {
        //     let cond = Cond2 {
        //         cond_lhs,
        //         cond_rhs,
        //         pass,
        //         fail,
        //         ret_var: *ret_var,
        //     };

        //     build_cond(env, scope, cond, procs)
        // }
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
                branches: branches,
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
        _ => {
            panic!("I don't yet know how to crane build {:?}", expr);
        }
    }
}

// struct Cond2<'a> {
//     cond_lhs: &'a Expr<'a>,
//     cond_rhs: &'a Expr<'a>,
//     pass: &'a Expr<'a>,
//     fail: &'a Expr<'a>,
//     ret_var: Variable,
// }

// fn build_cond<'a, 'ctx, 'env>(
//     env: &Env<'ctx, 'env>,
//     scope: &Scope<'ctx>,
//     parent: FunctionValue<'ctx>,
//     cond: Cond2<'a>,
//     procs: &Procs<'a>,
// ) -> BasicValueEnum<'ctx> {
//     let builder = env.builder;
//     let context = env.context;
//     let subs = &env.subs;

//     let content = subs.get_without_compacting(cond.ret_var).content;
//     let ret_type = type_from_content(&content, subs, context).unwrap_or_else(|err| {
//         panic!(
//             "Error converting cond branch ret_type content {:?} to basic type: {:?}",
//             cond.pass, err
//         )
//     });

//     let lhs = build_expr(env, scope, cond.cond_lhs, procs);
//     let rhs = build_expr(env, scope, cond.cond_rhs, procs);

//     match (lhs_layout, rhs_layout) {
//         // TODO do this based on lhs_type and rhs_type
//         (Layout::Float64, Layout::Float64) => {
//             let then_ebb = builder.create_ebb();
//             builder.switch_to_block(then_ebb);

//             let else_ebb = builder.create_ebb();
//             builder.switch_to_block(else_ebb);

//     // build branch
//     let then_bb = context.append_basic_block("then");
//     let else_bb = context.append_basic_block("else");
//     let cont_bb = context.append_basic_block("branchcont");

//     builder.build_conditional_branch(comparison, &then_bb, &else_bb);

//     // build then block
//     builder.position_at_end(&then_bb);
//     let then_val = build_expr(env, scope, pass, procs);
//     builder.build_unconditional_branch(&cont_bb);

//     let then_bb = builder.get_insert_block().unwrap();

//     // build else block
//     builder.position_at_end(&else_bb);
//     let else_val = build_expr(env, scope, fail, procs);
//     builder.build_unconditional_branch(&cont_bb);

//     let else_bb = builder.get_insert_block().unwrap();

//     // emit merge block
//     builder.position_at_end(&cont_bb);

//     let phi = builder.build_phi(ret_type, "branch");

//     phi.add_incoming(&[
//         (&Into::<BasicValueEnum>::into(then_val), &then_bb),
//         (&Into::<BasicValueEnum>::into(else_val), &else_bb),
//     ]);

//     phi.as_basic_value()

//             builder.ins().fcmp(FloatCC::Equal, lhs, rhs, ebb, &[]);

//             build_phi2(
//                 env, scope, comparison, cond.pass, cond.fail, ret_type, procs,
//             )
//         }

//         (Layout::Int64, Layout::Int64) => {
//             let ebb = builder.create_ebb();

//             builder.ins().icmp(IntCC::Equal, lhs, rhs, ebb, &[]);

//             build_phi2(
//                 env, scope, comparison, cond.pass, cond.fail, ret_type, procs,
//             )
//         }
//         _ => panic!(
//             "Tried to make a branch out of incompatible conditions: lhs = {:?} and rhs = {:?}",
//             cond.cond_lhs, cond.cond_rhs
//         ),
//     }
// }
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
    let subs = &env.subs;
    let mut switch = Switch::new();
    let SwitchArgs {
        branches,
        cond_expr,
        cond_var,
        default_branch,
        ret_type,
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

    // If necessary, convert cond from Float to Int using a bitcast.
    let cond = match subs.get_without_compacting(cond_var).content {
        Content::Structure(FlatType::Apply {
            module_name,
            name,
            args,
        }) if module_name.as_str() == crate::types::MOD_NUM
            && name.as_str() == crate::types::TYPE_NUM =>
        {
            let arg = *args.iter().next().unwrap();

            match subs.get_without_compacting(arg).content {
                Content::Structure(FlatType::Apply {
                    module_name, name, ..
                }) if module_name.as_str() == crate::types::MOD_FLOAT => {
                    debug_assert!(name.as_str() == crate::types::TYPE_FLOATINGPOINT);

                    // This is an f64, but switch only works on u64.
                    //
                    // Since it's the same size, we could theoretically use raw_bitcast
                    // which doesn't actually change the bits, just allows
                    // them to be used as a different type from its register.
                    //
                    // However, in practice, this fails Cranelift verification.
                    builder.ins().bitcast(types::I64, cond)
                }
                _ => cond,
            }
        }
        other => panic!("Cannot Switch on type {:?}", other),
    };

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
