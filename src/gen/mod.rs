use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::can::expr::Expr;
use crate::can::pattern::Pattern::{self, *};
use crate::can::procedure::Procedure;
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::collections::MutMap;
use crate::subs::FlatType::*;
use crate::subs::{Content, Subs, Variable};
use crate::types;

pub struct Env<'ctx, 'env> {
    pub procedures: MutMap<Symbol, Procedure>,
    pub subs: Subs,
    pub resolved_vars: ImMap<Symbol, Variable>,

    pub context: &'ctx Context,
    pub builder: &'env Builder<'ctx>,
    pub module: &'env Module<'ctx>,
}

pub enum TypedVal<'ctx> {
    FloatConst(FloatValue<'ctx>),
    IntConst(IntValue<'ctx>),
    #[allow(unused)]
    Typed(Variable, BasicValueEnum<'ctx>),
}

impl<'ctx> TypedVal<'ctx> {
    pub fn new(
        content: Content,
        bv_enum: BasicValueEnum<'ctx>,
        subs: &Subs,
    ) -> Result<Self, String> {
        match content {
            Content::Structure(flat_type) => match flat_type {
                Apply {
                    module_name,
                    name,
                    args,
                } => {
                    let module_name = module_name.as_str();
                    let name = name.as_str();

                    if module_name == types::MOD_NUM && name == types::TYPE_NUM {
                        let arg = *args.iter().next().unwrap();
                        let arg_content = subs.get_without_compacting(arg).content;

                        num_to_typed_val(arg_content, bv_enum)
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

    fn into_basic_value(self) -> BasicValueEnum<'ctx> {
        use self::TypedVal::*;

        match self {
            FloatConst(val) => val.into(),
            IntConst(val) => val.into(),
            Typed(_, val) => val,
        }
    }
}

impl<'ctx> Into<BasicValueEnum<'ctx>> for TypedVal<'ctx> {
    fn into(self) -> BasicValueEnum<'ctx> {
        self.into_basic_value()
    }
}
pub fn content_to_basic_type<'ctx>(
    content: Content,
    subs: &Subs,
    context: &'ctx Context,
) -> Result<BasicTypeEnum<'ctx>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == types::MOD_NUM && name == types::TYPE_NUM {
                    let arg = *args.iter().next().unwrap();
                    let arg_content = subs.get_without_compacting(arg).content;

                    num_to_basic_type(arg_content, context)
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

pub fn num_to_basic_type(content: Content, context: &Context) -> Result<BasicTypeEnum<'_>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == types::MOD_FLOAT && name == types::TYPE_FLOATINGPOINT && args.is_empty() {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::FloatType(context.f64_type()))
                } else if module_name == types::MOD_INT && name == types::TYPE_INTEGER && args.is_empty() {
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

pub fn num_to_typed_val<'ctx>(
    content: Content,
    bv_enum: BasicValueEnum<'ctx>,
) -> Result<TypedVal<'ctx>, String> {
    use TypedVal::*;

    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == types::MOD_FLOAT && name == types::TYPE_FLOATINGPOINT && args.is_empty() {
                    debug_assert!(args.is_empty());
                    Ok(FloatConst(bv_enum.into_float_value()))
                } else if module_name == types::MOD_INT && name == types::TYPE_INTEGER && args.is_empty() {
                    debug_assert!(args.is_empty());

                    Ok(IntConst(bv_enum.into_int_value()))
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

pub fn compile_standalone_expr<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    parent: &FunctionValue<'ctx>,
    expr: &Expr,
) -> BasicValueEnum<'ctx> {
    compile_expr(env, parent, expr, &mut ImMap::default()).into()
}

fn compile_expr<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    parent: &FunctionValue<'ctx>,
    expr: &Expr,
    vars: &mut ImMap<Symbol, PointerValue<'ctx>>,
) -> TypedVal<'ctx> {
    use self::TypedVal::*;
    use crate::can::expr::Expr::*;

    match *expr {
        Int(_, num) => IntConst(env.context.i64_type().const_int(num as u64, false)),
        Float(_, num) => FloatConst(env.context.f64_type().const_float(num)),
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
                    parent,
                    &loc_cond.value,
                    pattern.value.clone(),
                    &branch_expr.value,
                    &else_expr.value,
                    vars,
                )
            } else {
                panic!("TODO support when-expressions of more than 2 branches.");
            }
        }
        LetNonRec(ref def, ref loc_ret) => match &def.loc_pattern.value {
            Pattern::Identifier(symbol) => {
                let var_name = symbol.as_str();
                let val = compile_expr(env, parent, &def.loc_expr.value, vars).into_basic_value();
                let var = env.resolved_vars.get(symbol).unwrap_or_else(|| {
                    panic!(
                        "Could not find var for symbol {:?} in resolved_vars {:?}",
                        symbol, env.resolved_vars
                    )
                });
                let content = env.subs.get_without_compacting(*var).content;
                let basic_type = content_to_basic_type(content, &env.subs, env.context)
                    .unwrap_or_else(|err| {
                        panic!(
                            "Error converting symbol {:?} to basic type: {:?}",
                            symbol, err
                        )
                    });
                let alloca = create_entry_block_alloca(env, parent, basic_type, var_name);

                env.builder.build_store(alloca, val);

                vars.insert(symbol.clone(), alloca);

                compile_expr(env, parent, &loc_ret.value, vars)
            }
            pat => {
                panic!("TODO code gen Def pattern {:?}", pat);
            }
        },
        Var {
            ref resolved_symbol,
            ..
        } => match vars.get(resolved_symbol) {
            Some(ptr) => {
                let bv_enum = env.builder.build_load(*ptr, resolved_symbol.as_str());
                let var = env
                    .resolved_vars
                    .get(resolved_symbol)
                    .unwrap_or_else(|| panic!("Somehow a Lookup did not get its copy_var set."));
                let content = env.subs.get_without_compacting(*var).content;

                TypedVal::new(content, bv_enum, &env.subs).unwrap_or_else(|err| {
                    panic!(
                        "Error creating TypedVal for lookup on {:?}: {:?} - vars were: {:?}",
                        resolved_symbol, err, vars
                    )
                })
            }
            None => panic!("Could not find a var for {:?}", resolved_symbol),
        },
        _ => {
            panic!("I don't yet know how to compile {:?}", expr);
        }
    }
}

/// Creates a new stack allocation instruction in the entry block of the function.
fn create_entry_block_alloca<'ctx, BT>(
    env: &Env<'ctx, '_>,
    parent: &FunctionValue<'_>,
    basic_type: BT,
    name: &str,
) -> PointerValue<'ctx>
where
    BT: BasicType<'ctx>,
{
    let builder = env.context.create_builder();
    let entry = parent.get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(&entry),
    }

    builder.build_alloca(basic_type, name)
}

fn compile_when_branch<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    parent: &FunctionValue<'ctx>,
    cond_expr: &Expr,
    pattern: Pattern,
    branch_expr: &Expr,
    else_expr: &Expr,
    vars: &mut ImMap<Symbol, PointerValue<'ctx>>,
) -> TypedVal<'ctx> {
    use self::TypedVal::*;

    let builder = env.builder;
    let context = env.context;

    match compile_expr(env, parent, cond_expr, vars) {
        FloatConst(float_val) => match pattern {
            FloatLiteral(target_val) => {
                let comparison = builder.build_float_compare(
                    FloatPredicate::OEQ,
                    float_val,
                    context.f64_type().const_float(target_val),
                    "casecond",
                );

                let (then_bb, else_bb, then_val, else_val) =
                    two_way_branch(env, *parent, comparison, branch_expr, else_expr, vars);
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

                let (then_bb, else_bb, then_val, else_val) =
                    two_way_branch(env, *parent, comparison, branch_expr, else_expr, vars);
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

fn two_way_branch<'ctx, 'env>(
    env: &Env<'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    comparison: IntValue<'ctx>,
    branch_expr: &Expr,
    else_expr: &Expr,
    vars: &mut ImMap<Symbol, PointerValue<'ctx>>,
) -> (BasicBlock, BasicBlock, TypedVal<'ctx>, TypedVal<'ctx>) {
    let builder = env.builder;
    let context = env.context;

    // build branch
    let then_bb = context.append_basic_block(parent, "then");
    let else_bb = context.append_basic_block(parent, "else");
    let cont_bb = context.append_basic_block(parent, "casecont");

    builder.build_conditional_branch(comparison, &then_bb, &else_bb);

    // build then block
    builder.position_at_end(&then_bb);
    let then_val = compile_expr(env, &parent, branch_expr, vars);
    builder.build_unconditional_branch(&cont_bb);

    let then_bb = builder.get_insert_block().unwrap();

    // build else block
    builder.position_at_end(&else_bb);
    let else_val = compile_expr(env, &parent, else_expr, vars);
    builder.build_unconditional_branch(&cont_bb);

    let else_bb = builder.get_insert_block().unwrap();

    // emit merge block
    builder.position_at_end(&cont_bb);

    (then_bb, else_bb, then_val, else_val)
}
