use bumpalo::Bump;
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
use crate::crane::convert::{content_to_crane_type, sig_from_layout, type_from_layout};
use crate::mono::expr::{Expr, Proc, Procs};
use crate::mono::layout::Layout;
use crate::subs::Subs;

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
        Branches { .. } => {
            panic!("TODO build_branches(env, scope, cond_lhs, branches, procs)");
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
                    .unwrap_or_else(|()| panic!("TODO generate a runtime error here!"));
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
                let mut arg_vals = Vec::with_capacity(args.len());

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
            let mut arg_vals = Vec::with_capacity(args.len());

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
// let builder = env.builder;
// let context = env.context;
// let subs = &env.subs;

// let content = subs.get_without_compacting(cond.ret_var).content;
// let ret_type = content_to_crane_type(&content, subs, context).unwrap_or_else(|err| {
//     panic!(
//         "Error converting cond branch ret_type content {:?} to basic type: {:?}",
//         cond.pass, err
//     )
// });

// let lhs = build_expr(env, scope, cond.cond_lhs, procs);
// let rhs = build_expr(env, scope, cond.cond_rhs, procs);

// match (lhs, rhs) {
//     (FloatValue(lhs_float), FloatValue(rhs_float)) => {
//         let comparison =
//             builder.build_float_compare(FloatPredicate::OEQ, lhs_float, rhs_float, "cond");

//         build_phi2(
//             env, scope, comparison, cond.pass, cond.fail, ret_type, procs,
//         )
//     }

//     (IntValue(lhs_int), IntValue(rhs_int)) => {
//         let comparison = builder.build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "cond");

//         build_phi2(
//             env, scope, comparison, cond.pass, cond.fail, ret_type, procs,
//         )
//     }
//     _ => panic!(
//         "Tried to make a branch out of incompatible conditions: lhs = {:?} and rhs = {:?}",
//         cond.cond_lhs, cond.cond_rhs
//     ),
// }
// }

// fn build_branches<'a, 'ctx, 'env>(
//     env: &Env<'ctx, 'env>,
//     scope: &Scope<'ctx>,
//     parent: FunctionValue<'ctx>,
//     cond_lhs: &'a Expr<'a>,
//     branches: &'a [(Expr<'a>, Expr<'a>, Expr<'a>)],
//     ret_type: BasicValueEnum<'ctx>,
//     procs: &Procs<'a, 'ctx>,
// ) -> BasicValueEnum<'ctx> {
//     let builder = env.builder;
//     let context = env.context;
//     let lhs = build_expr(env, scope, cond_lhs, procs);
//     let mut branch_iter = branches.into_iter();
//     let content = subs.get_without_compacting(cond.ret_var).content;
//     let ret_type = content_to_crane_type(&content, subs, context).unwrap_or_else(|err| {
//         panic!(
//             "Error converting cond branch ret_type content {:?} to basic type: {:?}",
//             cond.pass, err
//         )
//     });

//     for (cond_rhs, cond_pass, cond_else) in branches {
//         let rhs = build_expr(env, scope, cond_rhs, procs);
//         let pass = build_expr(env, scope, cond_pass, procs);
//         let fail = build_expr(env, scope, cond_else, procs);

//         let cond = Cond {
//             lhs,
//             rhs,
//             pass,
//             fail,
//             ret_type,
//         };

//         build_cond(env, scope, cond, procs)
//     }
// }

// TODO trim down these arguments
// #[allow(clippy::too_many_arguments)]
// fn build_phi2<'a, 'ctx, 'env>(
//     env: &Env<'ctx, 'env>,
//     scope: &Scope<'ctx>,
//     parent: FunctionValue<'ctx>,
//     comparison: IntValue<'ctx>,
//     pass: &'a Expr<'a>,
//     fail: &'a Expr<'a>,
//     ret_type: BasicTypeEnum<'ctx>,
//     procs: &Procs<'a>,
// ) -> BasicValueEnum<'ctx> {
//     let builder = env.builder;
//     let context = env.context;

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
// }

pub fn declare_proc<'a, B: Backend>(
    env: &Env<'a>,
    module: &mut Module<B>,
    name: InlinableString,
    proc: &Proc<'a>,
) -> (FuncId, Signature) {
    let args = proc.args;
    let subs = &env.subs;
    let cfg = env.cfg;
    let ret_content = subs.get_without_compacting(proc.ret_var).content;
    // TODO this content_to_crane_type is duplicated when building this Proc
    let ret_type = content_to_crane_type(&ret_content, subs, env.cfg).unwrap_or_else(|err| {
        panic!(
            "Error converting function return value content to basic type: {:?}",
            err
        )
    });

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

        let ebb = builder.create_ebb();

        builder.switch_to_block(ebb);
        builder.append_ebb_params_for_function_params(ebb);

        // Add args to scope
        for (&param, (_, arg_name, var)) in builder.ebb_params(ebb).iter().zip(args) {
            let content = subs.get_without_compacting(*var).content;
            // TODO this content_to_crane_type is duplicated when building this Proc
            //
            let layout = Layout::from_content(arena, content, subs)
                .unwrap_or_else(|()| panic!("TODO generate a runtime error here!"));
            let expr_type = type_from_layout(cfg, &layout, subs);

            scope.insert(arg_name.clone(), ScopeEntry::Arg { expr_type, param });
        }

        let body = build_expr(env, &scope, module, &mut builder, &proc.body, procs);

        builder.ins().return_(&[body]);
        builder.seal_all_blocks();
        builder.finalize();
    }

    module
        .define_function(fn_id, ctx)
        .expect("Defining Cranelift function failed");

    module.clear_context(ctx);
}
