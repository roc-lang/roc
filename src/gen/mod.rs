extern crate inkwell;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

use can::expr::Expr;
use can::pattern::Pattern::{self, *};
use can::procedure::Procedure;
use can::symbol::Symbol;
use collections::ImMap;
use collections::MutMap;
use subs::FlatType::*;
use subs::{Content, Subs, Variable};
use types;

enum TypedVal<'ctx> {
    FloatConst(FloatValue<'ctx>),
    IntConst(IntValue<'ctx>),
    #[allow(unused)]
    Typed(Variable, BasicValueEnum<'ctx>),
}

impl<'ctx> Into<BasicValueEnum<'ctx>> for TypedVal<'ctx> {
    fn into(self) -> BasicValueEnum<'ctx> {
        use self::TypedVal::*;

        match self {
            FloatConst(val) => val.into(),
            IntConst(val) => val.into(),
            Typed(_, val) => val,
        }
    }
}
pub fn content_to_basic_type<'ctx>(
    content: Content,
    subs: &mut Subs,
    context: &'ctx Context,
) -> Result<BasicTypeEnum<'ctx>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                if &*module_name == types::MOD_NUM && &*name == types::TYPE_NUM {
                    num_to_basic_type(subs.get(*args.iter().next().unwrap()).content, context)
                } else {
                    panic!(
                        "TODO handle content_to_basic_type for flat_type {}.{} with args {:?}",
                        module_name, name, args
                    );
                }
            }
            other => panic!("TODO handle content_to_basic_type for {:?}", other),
        },
        other => Err(format!("Cannot convert {:?} to BasicTypeEnum", other)),
    }
}

pub fn num_to_basic_type<'ctx>(
    content: Content,
    context: &'ctx Context,
) -> Result<BasicTypeEnum<'ctx>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                if &*module_name == types::MOD_FLOAT && &*name == types::TYPE_FLOATINGPOINT && args.is_empty() {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::FloatType(context.f64_type()))
                } else if &*module_name == types::MOD_INT && &*name == types::TYPE_INTEGER && args.is_empty() {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::IntType(context.i64_type()))
                } else {
                    Err(format!("Unrecognized numeric type: {}.{} with args {:?}", module_name, name, args))
                }
            }
            other => panic!(
                "TODO handle content_to_basic_type (branch 0) for {:?} which is NESTED inside Num.Num",
                other
            ),
        },

        other => panic!(
            "TODO handle content_to_basic_type (branch 1) for {:?} which is NESTED inside Num.Num",
            other
        ),
    }
}

pub fn compile_standalone_expr<'ctx, 'm>(
    env: &Env,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    module: &Module<'m>,
    parent: &FunctionValue<'ctx>,
    expr: &Expr,
) -> BasicValueEnum<'ctx> {
    compile_expr(
        env,
        context,
        builder,
        module,
        parent,
        expr,
        &mut ImMap::default(),
    )
    .into()
}

fn compile_expr<'ctx, 'm>(
    env: &Env,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    module: &Module<'m>,
    parent: &FunctionValue<'ctx>,
    expr: &Expr,
    vars: &mut ImMap<Symbol, PointerValue<'ctx>>,
) -> TypedVal<'ctx> {
    use self::TypedVal::*;
    use can::expr::Expr::*;

    match *expr {
        Int(num) => IntConst(context.i64_type().const_int(num as u64, false)),
        Float(num) => FloatConst(context.f64_type().const_float(num)),
        Case(_, ref loc_cond_expr, ref branches) => {
            if branches.len() < 2 {
                panic!("TODO support case-expressions of fewer than 2 branches.");
            }
            if branches.len() == 2 {
                let mut iter = branches.iter();

                let (pattern, branch_expr) = iter.next().unwrap();

                compile_case_branch(
                    env,
                    context,
                    builder,
                    module,
                    parent,
                    &loc_cond_expr.value,
                    pattern.value.clone(),
                    &branch_expr.value,
                    &iter.next().unwrap().1.value,
                    vars,
                )
            } else {
                panic!("TODO support case-expressions of more than 2 branches.");
            }
        }
        _ => {
            panic!("I don't yet know how to compile {:?}", expr);
        }
    }
}

pub struct Env {
    pub procedures: MutMap<Symbol, Procedure>,
    pub subs: Subs,
}

fn compile_case_branch<'ctx, 'm>(
    env: &Env,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    module: &Module<'m>,
    parent: &FunctionValue<'ctx>,
    cond_expr: &Expr,
    pattern: Pattern,
    branch_expr: &Expr,
    else_expr: &Expr,
    vars: &mut ImMap<Symbol, PointerValue<'ctx>>,
) -> TypedVal<'ctx> {
    use self::TypedVal::*;

    match compile_expr(env, context, builder, module, parent, cond_expr, vars) {
        FloatConst(float_val) => match pattern {
            FloatLiteral(target_val) => {
                let comparison = builder.build_float_compare(
                    FloatPredicate::OEQ,
                    float_val,
                    context.f64_type().const_float(target_val),
                    "casecond",
                );

                let (then_bb, else_bb, then_val, else_val) = two_way_branch(
                    env,
                    context,
                    builder,
                    module,
                    parent,
                    comparison,
                    branch_expr,
                    else_expr,
                    vars,
                );
                let phi = builder.build_phi(context.f64_type(), "casetmp");

                phi.add_incoming(&[
                    (&Into::<BasicValueEnum>::into(then_val), &then_bb),
                    (&Into::<BasicValueEnum>::into(else_val), &else_bb),
                ]);

                FloatConst(phi.as_basic_value().into_float_value())
            }

            _ => panic!("TODO support pattern matching on floats other than literals."),
        },

        IntConst(int_val) => match pattern {
            IntLiteral(target_val) => {
                let comparison = builder.build_int_compare(
                    IntPredicate::EQ,
                    int_val,
                    context.i64_type().const_int(target_val as u64, false),
                    "casecond",
                );

                let (then_bb, else_bb, then_val, else_val) = two_way_branch(
                    env,
                    context,
                    builder,
                    module,
                    parent,
                    comparison,
                    branch_expr,
                    else_expr,
                    vars,
                );
                let phi = builder.build_phi(context.i64_type(), "casetmp");

                phi.add_incoming(&[
                    (&Into::<BasicValueEnum>::into(then_val), &then_bb),
                    (&Into::<BasicValueEnum>::into(else_val), &else_bb),
                ]);

                IntConst(phi.as_basic_value().into_int_value())
            }
            _ => panic!("TODO support pattern matching on ints other than literals."),
        },
        Typed(_var, _basic_value_enum) => panic!(
            "TODO handle pattern matching on conditionals other than int and float literals."
        ),
    }
}

fn two_way_branch<'ctx, 'm>(
    env: &Env,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    module: &Module<'m>,
    parent: &FunctionValue<'ctx>,
    comparison: IntValue<'ctx>,
    branch_expr: &Expr,
    else_expr: &Expr,
    vars: &mut ImMap<Symbol, PointerValue<'ctx>>,
) -> (BasicBlock, BasicBlock, TypedVal<'ctx>, TypedVal<'ctx>) {
    // build branch
    let then_bb = context.append_basic_block(*parent, "then");
    let else_bb = context.append_basic_block(*parent, "else");
    let cont_bb = context.append_basic_block(*parent, "casecont");

    builder.build_conditional_branch(comparison, &then_bb, &else_bb);

    // build then block
    builder.position_at_end(&then_bb);
    let then_val = compile_expr(env, context, builder, module, parent, branch_expr, vars);
    builder.build_unconditional_branch(&cont_bb);

    let then_bb = builder.get_insert_block().unwrap();

    // build else block
    builder.position_at_end(&else_bb);
    let else_val = compile_expr(env, context, builder, module, parent, else_expr, vars);
    builder.build_unconditional_branch(&cont_bb);

    let else_bb = builder.get_insert_block().unwrap();

    // emit merge block
    builder.position_at_end(&cont_bb);

    (then_bb, else_bb, then_val, else_val)
}
