use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};
use inlinable_string::InlinableString;

use crate::collections::ImMap;
use crate::gen::convert::{content_to_basic_type, layout_to_basic_type};
use crate::gen::env::Env;
use crate::ll::expr::{Expr, Proc, Procs};
use crate::subs::Variable;

/// This is for Inkwell's FunctionValue::verify - we want to know the verification
/// output in debug builds, but we don't want it to print to stdout in release builds!
#[cfg(debug_assertions)]
const PRINT_FN_VERIFICATION_OUTPUT: bool = true;

#[cfg(not(debug_assertions))]
const PRINT_FN_VERIFICATION_OUTPUT: bool = false;

type Scope<'ctx> = ImMap<InlinableString, (Variable, PointerValue<'ctx>)>;

pub fn build_expr<'a, 'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    parent: FunctionValue<'ctx>,
    expr: &Expr<'a>,
    procs: &Procs<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    use crate::ll::expr::Expr::*;

    match expr {
        Int(num) => env.context.i64_type().const_int(*num as u64, false).into(),
        Float(num) => env.context.f64_type().const_float(*num).into(),
        Cond {
            cond_lhs,
            cond_rhs,
            pass,
            fail,
            ret_var,
        } => {
            let cond = Cond2 {
                cond_lhs,
                cond_rhs,
                pass,
                fail,
                ret_var: *ret_var,
            };

            build_cond(env, scope, parent, cond, procs)
        }
        Branches { .. } => {
            panic!("TODO build_branches(env, scope, parent, cond_lhs, branches, procs)");
        }
        Store(ref stores, ref ret) => {
            let mut scope = im_rc::HashMap::clone(scope);
            let subs = &env.subs;
            let context = &env.context;

            for (name, var, expr) in stores.iter() {
                let content = subs.get_without_compacting(*var).content;
                let val = build_expr(env, &scope, parent, &expr, procs);
                let expr_bt =
                    content_to_basic_type(&content, subs, context).unwrap_or_else(|err| {
                        panic!(
                            "Error converting symbol {:?} to basic type: {:?} - scope was: {:?}",
                            name, err, scope
                        )
                    });
                let alloca = create_entry_block_alloca(env, parent, expr_bt, &name);

                env.builder.build_store(alloca, val);

                // Make a new scope which includes the binding we just encountered.
                // This should be done *after* compiling the bound expr, since any
                // recursive (in the LetRec sense) bindings should already have
                // been extracted as procedures. Nothing in here should need to
                // access itself!
                scope = im_rc::HashMap::clone(&scope);

                scope.insert(name.clone(), (*var, alloca));
            }

            build_expr(env, &scope, parent, ret, procs)
        }
        CallByName(ref name, ref args) => {
            // TODO try one of these alternative strategies (preferably the latter):
            //
            // 1. use SIMD string comparison to compare these strings faster
            // 2. pre-register Bool.or using module.add_function, and see if LLVM inlines it
            if name == "Bool.or" {
                panic!("TODO create a phi node for ||");
            } else if name == "Bool.and" {
                panic!("TODO create a phi node for &&");
            } else {
                let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    arg_vals.push(build_expr(env, scope, parent, arg, procs));
                }

                let fn_val = env
                    .module
                    .get_function(name)
                    .unwrap_or_else(|| panic!("Unrecognized function: {:?}", name));

                let call = env.builder.build_call(fn_val, arg_vals.as_slice(), "tmp");

                call.try_as_basic_value()
                    .left()
                    .unwrap_or_else(|| panic!("LLVM error: Invalid call by name."))
            }
        }
        FunctionPointer(ref fn_name) => {
            let ptr = env
                .module
                .get_function(fn_name)
                .unwrap_or_else(|| {
                    panic!("Could not get pointer to unknown function {:?}", fn_name)
                })
                .as_global_value()
                .as_pointer_value();

            BasicValueEnum::PointerValue(ptr)
        }
        CallByPointer(ref _ptr, ref args) => {
            let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity(args.len());

            for arg in args.iter() {
                arg_vals.push(build_expr(env, scope, parent, arg, procs));
            }

            panic!("TODO do a load(ptr) to get back the pointer, then pass *that* in here!");

            //             let call = match build_expr(env, scope, parent, expr, procs) {
            //                 BasicValueEnum::PointerValue(ptr) => {
            //                     env.builder.build_call(ptr, arg_vals.as_slice(), "tmp")
            //                 }
            //                 non_ptr => {
            //                     panic!(
            //                         "Tried to call by pointer, but encountered a non-pointer: {:?}",
            //                         non_ptr
            //                     );
            //                 }
            //             };

            //             call.try_as_basic_value()
            //                 .left()
            //                 .unwrap_or_else(|| panic!("LLVM error: Invalid call by pointer."))
        }

        Load(name) => match scope.get(name) {
            Some((_, ptr)) => env.builder.build_load(*ptr, name),
            None => panic!("Could not find a var for {:?} in scope {:?}", name, scope),
        },
        _ => {
            panic!("I don't yet know how to build {:?}", expr);
        }
    }
}

struct Cond2<'a> {
    cond_lhs: &'a Expr<'a>,
    cond_rhs: &'a Expr<'a>,
    pass: &'a Expr<'a>,
    fail: &'a Expr<'a>,
    ret_var: Variable,
}

fn build_cond<'a, 'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    parent: FunctionValue<'ctx>,
    cond: Cond2<'a>,
    procs: &Procs<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let context = env.context;
    let subs = &env.subs;

    let content = subs.get_without_compacting(cond.ret_var).content;
    let ret_type = content_to_basic_type(&content, subs, context).unwrap_or_else(|err| {
        panic!(
            "Error converting cond branch ret_type content {:?} to basic type: {:?}",
            cond.pass, err
        )
    });

    let lhs = build_expr(env, scope, parent, cond.cond_lhs, procs);
    let rhs = build_expr(env, scope, parent, cond.cond_rhs, procs);

    match (lhs, rhs) {
        (FloatValue(lhs_float), FloatValue(rhs_float)) => {
            let comparison =
                builder.build_float_compare(FloatPredicate::OEQ, lhs_float, rhs_float, "cond");

            build_phi2(
                env, scope, parent, comparison, cond.pass, cond.fail, ret_type, procs,
            )
        }

        (IntValue(lhs_int), IntValue(rhs_int)) => {
            let comparison = builder.build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "cond");

            build_phi2(
                env, scope, parent, comparison, cond.pass, cond.fail, ret_type, procs,
            )
        }
        _ => panic!(
            "Tried to make a branch out of incompatible conditions: lhs = {:?} and rhs = {:?}",
            cond.cond_lhs, cond.cond_rhs
        ),
    }
}

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
//     let lhs = build_expr(env, scope, parent, cond_lhs, procs);
//     let mut branch_iter = branches.into_iter();
//     let content = subs.get_without_compacting(cond.ret_var).content;
//     let ret_type = content_to_basic_type(&content, subs, context).unwrap_or_else(|err| {
//         panic!(
//             "Error converting cond branch ret_type content {:?} to basic type: {:?}",
//             cond.pass, err
//         )
//     });

//     for (cond_rhs, cond_pass, cond_else) in branches {
//         let rhs = build_expr(env, scope, parent, cond_rhs, procs);
//         let pass = build_expr(env, scope, parent, cond_pass, procs);
//         let fail = build_expr(env, scope, parent, cond_else, procs);

//         let cond = Cond {
//             lhs,
//             rhs,
//             pass,
//             fail,
//             ret_type,
//         };

//         build_cond(env, scope, parent, cond, procs)
//     }
// }

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
fn build_phi2<'a, 'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    parent: FunctionValue<'ctx>,
    comparison: IntValue<'ctx>,
    pass: &'a Expr<'a>,
    fail: &'a Expr<'a>,
    ret_type: BasicTypeEnum<'ctx>,
    procs: &Procs<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let context = env.context;

    // build branch
    let then_bb = context.append_basic_block(parent, "then");
    let else_bb = context.append_basic_block(parent, "else");
    let cont_bb = context.append_basic_block(parent, "branchcont");

    builder.build_conditional_branch(comparison, &then_bb, &else_bb);

    // build then block
    builder.position_at_end(&then_bb);
    let then_val = build_expr(env, scope, parent, pass, procs);
    builder.build_unconditional_branch(&cont_bb);

    let then_bb = builder.get_insert_block().unwrap();

    // build else block
    builder.position_at_end(&else_bb);
    let else_val = build_expr(env, scope, parent, fail, procs);
    builder.build_unconditional_branch(&cont_bb);

    let else_bb = builder.get_insert_block().unwrap();

    // emit merge block
    builder.position_at_end(&cont_bb);

    let phi = builder.build_phi(ret_type, "branch");

    phi.add_incoming(&[
        (&Into::<BasicValueEnum>::into(then_val), &then_bb),
        (&Into::<BasicValueEnum>::into(else_val), &else_bb),
    ]);

    phi.as_basic_value()
}

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
fn set_name(bv_enum: BasicValueEnum<'_>, name: &str) {
    match bv_enum {
        ArrayValue(val) => val.set_name(name),
        IntValue(val) => val.set_name(name),
        FloatValue(val) => val.set_name(name),
        PointerValue(val) => val.set_name(name),
        StructValue(val) => val.set_name(name),
        VectorValue(val) => val.set_name(name),
    }
}

/// Creates a new stack allocation instruction in the entry block of the function.
pub fn create_entry_block_alloca<'ctx>(
    env: &Env<'ctx, '_>,
    parent: FunctionValue<'_>,
    basic_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let builder = env.context.create_builder();
    let entry = parent.get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(&entry),
    }

    builder.build_alloca(basic_type, name)
}

pub fn build_proc<'a, 'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    name: InlinableString,
    proc: Proc<'a>,
    procs: &Procs<'a, 'ctx>,
) -> FunctionValue<'ctx> {
    let args = proc.args;
    let mut arg_names = Vec::new();
    let mut arg_basic_types = Vec::with_capacity(args.len());

    for (layout, name, _var) in args.iter() {
        let arg_type = layout_to_basic_type(&layout, &env.subs, env.context);

        arg_basic_types.push(arg_type);
        arg_names.push(name);
    }

    // Retrieve the function value from the module
    let fn_val = env.module.get_function(&name).unwrap_or_else(|| {
        panic!(
            "Function {:?} should have been registered in the LLVM module, but it was not!",
            name
        )
    });

    // Add a basic block for the entry point
    let entry = env.context.append_basic_block(fn_val, "entry");
    let builder = env.builder;

    builder.position_at_end(&entry);

    let mut scope = scope.clone();

    // Add args to scope
    for ((arg_val, arg_type), (_, arg_name, var)) in
        fn_val.get_param_iter().zip(arg_basic_types).zip(args)
    {
        set_name(arg_val, arg_name);

        let alloca = create_entry_block_alloca(env, fn_val, arg_type, arg_name);

        builder.build_store(alloca, arg_val);

        scope.insert(arg_name.clone(), (*var, alloca));
    }

    let body = build_expr(env, &scope, fn_val, &proc.body, procs);

    builder.build_return(Some(&body));

    fn_val
}

pub fn verify_fn(fn_val: FunctionValue<'_>) {
    if fn_val.verify(PRINT_FN_VERIFICATION_OUTPUT) {
        // TODO call pass_manager.run_on(&fn_val) to optimize it!
    } else {
        unsafe {
            fn_val.delete();
        }

        panic!("Invalid generated fn_val.")
    }
}
