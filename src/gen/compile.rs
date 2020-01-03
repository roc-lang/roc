use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{BasicValue, FunctionValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::can::expr::Expr;
use crate::can::ident::Lowercase;
use crate::can::pattern::Pattern::{self, *};
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::collections::MutMap;
use crate::gen::convert::content_to_basic_type;
use crate::gen::env::Env;
use crate::subs::{Content, FlatType, Subs};

/// This is for Inkwell's FunctionValue::verify - we want to know the verification
/// output in debug builds, but we don't want it to print to stdout in release builds!
#[cfg(debug_assertions)]
const PRINT_FN_VERIFICATION_OUTPUT: bool = true;

#[cfg(not(debug_assertions))]
const PRINT_FN_VERIFICATION_OUTPUT: bool = false;

type Procs<'ctx> = MutMap<Symbol, (Content, FunctionValue<'ctx>)>;

type Scope<'ctx> = ImMap<Symbol, (Content, PointerValue<'ctx>)>;

pub fn compile_standalone_expr<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    expr: &Expr,
) -> BasicValueEnum<'ctx> {
    compile_expr(env, &ImMap::default(), parent, expr, &mut MutMap::default())
}

fn compile_expr<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    parent: FunctionValue<'ctx>,
    expr: &Expr,
    procs: &mut Procs,
) -> BasicValueEnum<'ctx> {
    use crate::can::expr::Expr::*;

    match *expr {
        Int(_, num) => env.context.i64_type().const_int(num as u64, false).into(),
        Float(_, num) => env.context.f64_type().const_float(num).into(),
        When {
            ref loc_cond,
            ref branches,
            ..
        } => {
            if branches.len() < 2 {
                panic!("TODO support when-expressions of fewer than 2 branches.");
            }
            if branches.len() == 2 {
                let mut iter = branches.iter();

                let (pattern, branch_expr) = iter.next().unwrap();
                let (_, else_expr) = iter.next().unwrap();

                compile_when_branch(
                    env,
                    scope,
                    parent,
                    &loc_cond.value,
                    pattern.value.clone(),
                    &branch_expr.value,
                    &else_expr.value,
                    procs,
                )
            } else {
                panic!("TODO support when-expressions of more than 2 branches.");
            }
        }
        LetNonRec(ref def, ref loc_ret, _) => {
            match &def.loc_pattern.value {
                Pattern::Identifier(symbol) => {
                    let expr = &def.loc_expr.value;
                    let subs = &env.subs;
                    let context = &env.context;
                    let content = content_from_expr(scope, subs, expr);
                    let val = compile_expr(env, &scope, parent, &expr, procs);
                    let expr_bt = content_to_basic_type(&content, subs, context).unwrap_or_else(|err| panic!("Error converting symbol {:?} to basic type: {:?} - scope was: {:?}", symbol, err, scope));
                    let alloca = create_entry_block_alloca(env, parent, expr_bt, symbol.as_str());

                    env.builder.build_store(alloca, val);

                    // Make a new scope which includes the binding we just encountered.
                    // This should be done *after* compiling the bound expr, since this is a
                    // LetNonRec rather than a LetRec. It shouldn't need to access itself!
                    let mut scope = scope.clone();

                    scope.insert(symbol.clone(), (content.clone(), alloca));

                    compile_expr(env, &scope, parent, &loc_ret.value, procs)
                }
                pat => {
                    panic!("TODO code gen Def pattern {:?}", pat);
                }
            }
        }
        Closure(_, ref symbol, ref _recursive, ref args, ref body) => {
            let (ref loc_body_expr, ret_var) = **body;
            let subs = &env.subs;
            let mut arg_types = Vec::new();
            let mut arg_names = Vec::new();
            let ret_content = subs.get_without_compacting(ret_var).content;
            let ret_type = content_to_basic_type(&ret_content, &env.subs, env.context)
                .unwrap_or_else(|err| {
                    panic!(
                        "Error converting symbol {:?} to basic type: {:?} - scope was: {:?}",
                        symbol, err, scope
                    )
                });

            for (var, loc_pat) in args {
                let content = subs.get_without_compacting(*var).content;
                let name = match &loc_pat.value {
                    Pattern::Identifier(ident) => ident.as_str().into(),
                    pat => {
                        panic!("TODO code gen function arg for pattern {:?}", pat);
                    }
                };

                arg_types.push(content);
                arg_names.push(name);
            }

            let fn_val = compile_closure(
                env,
                symbol.as_str().into(),
                arg_types,
                arg_names.as_slice(),
                ret_type,
                &loc_body_expr.value,
                scope,
                procs,
                None,
            );

            if fn_val.verify(PRINT_FN_VERIFICATION_OUTPUT) {
                // TODO call pass_manager.run_on(&fn_val) to optimize it!

                // The closure evaluates to a pointer to the function.
                fn_val
                    .as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum()
            } else {
                unsafe {
                    fn_val.delete();
                }

                panic!("Invalid generated fn_val.")
            }
        }
        Call(ref boxed, ref loc_args, _) => {
            let (_, ref loc_expr, _) = **boxed;
            let mut arg_vars: Vec<BasicValueEnum> = Vec::with_capacity(loc_args.len());

            for (_var, loc_arg) in loc_args.iter() {
                let arg = compile_expr(env, scope, parent, &loc_arg.value, procs);

                arg_vars.push(arg.into());
            }

            let call = match &loc_expr.value {
                // TODO fix and then re-enable this. The problem is that it fails on
                // symbol lookup, because the closure's stored Symbol is always the
                // auto-generated one like "Test.blah$1" whereas the lookup is always
                // something like "Test.blah$identity" - so if we want to call by
                // name directly, we need to detect the closure's name *during code gen*
                // (while processing defs) and insert it into scope accordingly.
                // Afterwards, we should remove that Symbol from Expr.
                //
                // Var {
                //     resolved_symbol, ..
                // } => {
                //     // Call by name
                //     let fn_val = env
                //         .module
                //         .get_function(resolved_symbol.as_str())
                //         .unwrap_or_else(|| panic!("Unrecognized function: {:?}", resolved_symbol));

                //     env.builder.build_call(fn_val, arg_vars.as_slice(), "tmp")
                // }
                expr => {
                    // Call by pointer - the closure was anonymous, e.g. ((\a -> a) 5)
                    match compile_expr(env, scope, parent, expr, procs) {
                        BasicValueEnum::PointerValue(ptr) => {
                            env.builder.build_call(ptr, arg_vars.as_slice(), "tmp")
                        }
                        non_ptr => {
                            panic!(
                                "Tried to call by pointer, but encountered a non-pointer: {:?}",
                                non_ptr
                            );
                        }
                    }
                }
            };

            call.try_as_basic_value()
                .left()
                .unwrap_or_else(|| panic!("Invalid call produced."))
        }

        Var {
            ref resolved_symbol,
            ..
        } => match scope.get(resolved_symbol) {
            Some((_, ptr)) => env.builder.build_load(*ptr, resolved_symbol.as_str()),
            None => panic!(
                "Could not find a var for {:?} in scope {:?}",
                resolved_symbol, scope
            ),
        },
        _ => {
            panic!("I don't yet know how to compile {:?}", expr);
        }
    }
}

fn content_from_expr(scope: &Scope<'_>, subs: &Subs, expr: &Expr) -> Content {
    use crate::can::expr::Expr::*;

    match expr {
        Int(var, _)
        | Float(var, _)
        | When { expr_var: var, .. }
        | LetNonRec(_, _, var)
        | LetRec(_, _, var)
        | Closure(var, _, _, _, _) => subs.get_without_compacting(*var).content,
        Str(_) | BlockStr(_) => Content::Structure(FlatType::Apply {
            module_name: "Str".into(),
            name: "Str".into(),
            args: Vec::new(),
        }),
        Var {
            ref resolved_symbol,
            ..
        } => {
            let (content, _) = scope.get(resolved_symbol).unwrap_or_else(|| {
                panic!(
                    "Code gen problem: Couldn't find {:?} in scope {:?}",
                    resolved_symbol, scope
                )
            });

            content.clone()
        }
        other => panic!("TODO handle content_from_expr for {:?}", other),
    }
}

fn compile_when_branch<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    parent: FunctionValue<'ctx>,
    cond_expr: &Expr,
    pattern: Pattern,
    branch_expr: &Expr,
    else_expr: &Expr,
    procs: &mut Procs,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let context = env.context;

    match compile_expr(env, scope, parent, cond_expr, procs) {
        FloatValue(float_val) => match pattern {
            FloatLiteral(target_val) => {
                let comparison = builder.build_float_compare(
                    FloatPredicate::OEQ,
                    float_val,
                    context.f64_type().const_float(target_val),
                    "whencond",
                );

                let (then_bb, else_bb, then_val, else_val) = two_way_branch(
                    env,
                    scope,
                    parent,
                    comparison,
                    branch_expr,
                    else_expr,
                    procs,
                );
                let phi = builder.build_phi(context.f64_type(), "whenbranch");

                phi.add_incoming(&[
                    (&Into::<BasicValueEnum>::into(then_val), &then_bb),
                    (&Into::<BasicValueEnum>::into(else_val), &else_bb),
                ]);

                phi.as_basic_value().into_float_value().into()
            }

            _ => panic!("TODO support pattern matching on floats other than literals."),
        },

        IntValue(int_val) => match pattern {
            IntLiteral(target_val) => {
                let comparison = builder.build_int_compare(
                    IntPredicate::EQ,
                    int_val,
                    context.i64_type().const_int(target_val as u64, false),
                    "whencond",
                );

                let (then_bb, else_bb, then_val, else_val) = two_way_branch(
                    env,
                    scope,
                    parent,
                    comparison,
                    branch_expr,
                    else_expr,
                    procs,
                );
                let phi = builder.build_phi(context.i64_type(), "whenbranch");

                phi.add_incoming(&[
                    (&Into::<BasicValueEnum>::into(then_val), &then_bb),
                    (&Into::<BasicValueEnum>::into(else_val), &else_bb),
                ]);

                phi.as_basic_value().into_int_value().into()
            }
            _ => panic!("TODO support pattern matching on ints other than literals."),
        },
        _ => panic!(
            "TODO handle pattern matching on conditionals other than int and float literals."
        ),
    }
}

fn two_way_branch<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    scope: &Scope<'ctx>,
    parent: FunctionValue<'ctx>,
    comparison: IntValue<'ctx>,
    branch_expr: &Expr,
    else_expr: &Expr,
    procs: &mut Procs,
) -> (
    BasicBlock,
    BasicBlock,
    BasicValueEnum<'ctx>,
    BasicValueEnum<'ctx>,
) {
    let builder = env.builder;
    let context = env.context;

    // build branch
    let then_bb = context.append_basic_block(parent, "then");
    let else_bb = context.append_basic_block(parent, "else");
    let cont_bb = context.append_basic_block(parent, "casecont");

    builder.build_conditional_branch(comparison, &then_bb, &else_bb);

    // build then block
    builder.position_at_end(&then_bb);
    let then_val = compile_expr(env, scope, parent, branch_expr, procs);
    builder.build_unconditional_branch(&cont_bb);

    let then_bb = builder.get_insert_block().unwrap();

    // build else block
    builder.position_at_end(&else_bb);
    let else_val = compile_expr(env, scope, parent, else_expr, procs);
    builder.build_unconditional_branch(&cont_bb);

    let else_bb = builder.get_insert_block().unwrap();

    // emit merge block
    builder.position_at_end(&cont_bb);

    (then_bb, else_bb, then_val, else_val)
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

pub fn compile_closure<'ctx, BT>(
    env: &Env<'ctx, '_>,
    name: Lowercase,
    arg_types: Vec<Content>,
    arg_names: &[Lowercase],
    ret_type: BT,
    body_expr: &Expr,
    scope: &Scope<'ctx>,
    procs: &mut Procs,
    linkage: Option<Linkage>,
) -> FunctionValue<'ctx>
where
    BT: BasicType<'ctx>,
{
    // We need these to be separate, but they must have the same length!
    debug_assert!(arg_types.len() == arg_names.len());

    // Register the function value in the module
    let mut arg_basic_types = Vec::with_capacity(arg_types.len());

    for content in arg_types.iter() {
        arg_basic_types.push(
            content_to_basic_type(content, &env.subs, env.context).unwrap_or_else(|err| {
                panic!(
                    "Error converting function arg content to basic type: {:?}",
                    err
                )
            }),
        );
    }

    let fn_type = ret_type.fn_type(arg_basic_types.as_slice(), false);
    let fn_val = env.module.add_function(name.as_str(), fn_type, linkage);

    let entry = env.context.append_basic_block(fn_val, "entry");
    let builder = env.builder;

    builder.position_at_end(&entry);

    let mut scope = scope.clone();

    // Add args to scope
    for (((arg_val, arg_name), arg_type), content) in fn_val
        .get_param_iter()
        .zip(arg_names)
        .zip(arg_basic_types)
        .zip(arg_types.into_iter())
    {
        let arg_name = arg_name.as_str();

        set_name(arg_val, arg_name);

        let alloca = create_entry_block_alloca(env, fn_val, arg_type, arg_name);

        builder.build_store(alloca, arg_val);

        scope.insert(arg_name.into(), (content, alloca));
    }

    let body = compile_expr(env, &scope, fn_val, body_expr, procs);

    builder.build_return(Some(&body));

    fn_val
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
