use bumpalo::{collections::Vec as BumpVec, Bump};

use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{BumpMap, BumpMapDefault, HumanIndex, SendMap};
use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_region::all::Region;
use roc_types::{
    subs::Variable,
    types::{self, AnnotationSource, IndexOrField, PReason, PatternCategory},
    types::{Category, Reason},
};

use crate::{
    lang::{
        core::{
            expr::{
                expr2::{ClosureExtra, Expr2, ExprId, WhenBranch},
                record_field::RecordField,
            },
            fun_def::FunctionDef,
            pattern::{DestructType, Pattern2, PatternId, PatternState2, RecordDestruct},
            types::{Type2, TypeId},
            val_def::ValueDef,
        },
        env::Env,
    },
    mem_pool::{pool::Pool, pool_vec::PoolVec, shallow_clone::ShallowClone},
};

/// A presence constraint is an additive constraint that defines the lower bound
/// of a type. For example, `Present(t1, IncludesTag(A, []))` means that the
/// type `t1` must contain at least the tag `A`. The additive nature of these
/// constraints makes them behaviorally different from unification-based constraints.
#[derive(Debug)]
pub enum PresenceConstraint<'a> {
    IncludesTag(TagName, BumpVec<'a, Type2>),
    IsOpen,
    Pattern(Region, PatternCategory, PExpected<Type2>),
}

#[derive(Debug)]
pub enum Constraint<'a> {
    Eq(Type2, Expected<Type2>, Category, Region),
    // Store(Type, Variable, &'static str, u32),
    Lookup(Symbol, Expected<Type2>, Region),
    Pattern(Region, PatternCategory, Type2, PExpected<Type2>),
    And(BumpVec<'a, Constraint<'a>>),
    Let(&'a LetConstraint<'a>),
    // SaveTheEnvironment,
    True, // Used for things that always unify, e.g. blanks and runtime errors
    Present(Type2, PresenceConstraint<'a>),
}

#[derive(Debug)]
pub struct LetConstraint<'a> {
    pub rigid_vars: BumpVec<'a, Variable>,
    pub flex_vars: BumpVec<'a, Variable>,
    pub def_types: BumpMap<Symbol, Type2>,
    pub defs_constraint: Constraint<'a>,
    pub ret_constraint: Constraint<'a>,
}

pub fn constrain_expr<'a>(
    arena: &'a Bump,
    env: &mut Env,
    expr: &Expr2,
    expected: Expected<Type2>,
    region: Region,
) -> Constraint<'a> {
    use Constraint::*;

    match expr {
        Expr2::Blank | Expr2::RuntimeError() | Expr2::InvalidLookup(_) => True,
        Expr2::Str(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::SmallStr(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::Var(symbol) => Lookup(*symbol, expected, region),
        Expr2::EmptyRecord => constrain_empty_record(expected, region),
        Expr2::SmallInt { var, .. } | Expr2::I128 { var, .. } | Expr2::U128 { var, .. } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);

            flex_vars.push(*var);

            let precision_var = env.var_store.fresh();

            let range_type = Type2::Variable(precision_var);

            let range_type_id = env.pool.add(range_type);

            exists(
                arena,
                flex_vars,
                Eq(
                    num_num(env.pool, range_type_id),
                    expected,
                    Category::Num,
                    region,
                ),
            )
        }
        Expr2::Float { var, .. } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);

            let mut and_constraints = BumpVec::with_capacity_in(2, arena);

            let num_type = Type2::Variable(*var);

            flex_vars.push(*var);

            let precision_var = env.var_store.fresh();

            let range_type = Type2::Variable(precision_var);

            let range_type_id = env.pool.add(range_type);

            and_constraints.push(Eq(
                num_type.shallow_clone(),
                Expected::ForReason(
                    Reason::FloatLiteral,
                    num_float(env.pool, range_type_id),
                    region,
                ),
                Category::Int,
                region,
            ));

            and_constraints.push(Eq(num_type, expected, Category::Frac, region));

            let defs_constraint = And(and_constraints);

            exists(arena, flex_vars, defs_constraint)
        }
        Expr2::List {
            elem_var, elems, ..
        } => {
            let mut flex_vars = BumpVec::with_capacity_in(1, arena);

            flex_vars.push(*elem_var);

            if elems.is_empty() {
                exists(
                    arena,
                    flex_vars,
                    Eq(
                        empty_list_type(env.pool, *elem_var),
                        expected,
                        Category::List,
                        region,
                    ),
                )
            } else {
                let mut constraints = BumpVec::with_capacity_in(1 + elems.len(), arena);

                let list_elem_type = Type2::Variable(*elem_var);

                let indexed_node_ids: Vec<(usize, ExprId)> =
                    elems.iter(env.pool).copied().enumerate().collect();

                for (index, elem_node_id) in indexed_node_ids {
                    let elem_expr = env.pool.get(elem_node_id);

                    let elem_expected = Expected::ForReason(
                        Reason::ElemInList {
                            index: HumanIndex::zero_based(index),
                        },
                        list_elem_type.shallow_clone(),
                        region,
                    );

                    let constraint = constrain_expr(arena, env, elem_expr, elem_expected, region);

                    constraints.push(constraint);
                }

                constraints.push(Eq(
                    list_type(env.pool, list_elem_type),
                    expected,
                    Category::List,
                    region,
                ));

                exists(arena, flex_vars, And(constraints))
            }
        }
        Expr2::Record { fields, record_var } => {
            if fields.is_empty() {
                constrain_empty_record(expected, region)
            } else {
                let field_types = PoolVec::with_capacity(fields.len() as u32, env.pool);

                let mut field_vars = BumpVec::with_capacity_in(fields.len(), arena);

                // Constraints need capacity for each field
                // + 1 for the record itself + 1 for record var
                let mut constraints = BumpVec::with_capacity_in(2 + fields.len(), arena);

                for (record_field_node_id, field_type_node_id) in
                    fields.iter_node_ids().zip(field_types.iter_node_ids())
                {
                    let record_field = env.pool.get(record_field_node_id);

                    match record_field {
                        RecordField::LabeledValue(pool_str, var, node_id) => {
                            let expr = env.pool.get(*node_id);

                            let (field_type, field_con) = constrain_field(arena, env, *var, expr);

                            field_vars.push(*var);

                            let field_type_id = env.pool.add(field_type);

                            env.pool[field_type_node_id] =
                                (*pool_str, types::RecordField::Required(field_type_id));

                            constraints.push(field_con);
                        }
                        e => todo!("{:?}", e),
                    }
                }

                let record_type = Type2::Record(field_types, env.pool.add(Type2::EmptyRec));

                let record_con = Eq(
                    record_type,
                    expected.shallow_clone(),
                    Category::Record,
                    region,
                );

                constraints.push(record_con);

                // variable to store in the AST
                let stored_con = Eq(
                    Type2::Variable(*record_var),
                    expected,
                    Category::Storage(std::file!(), std::line!()),
                    region,
                );

                field_vars.push(*record_var);
                constraints.push(stored_con);

                exists(arena, field_vars, And(constraints))
            }
        }
        Expr2::Tag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            let tag_name = TagName(name.as_str(env.pool).into());

            constrain_tag(
                arena,
                env,
                expected,
                region,
                tag_name,
                arguments,
                *ext_var,
                *variant_var,
            )
        }
        Expr2::Call {
            args,
            expr_var,
            expr_id: expr_node_id,
            closure_var,
            fn_var,
            called_via,
        } => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let call_expr = env.pool.get(*expr_node_id);

            let opt_symbol = if let Expr2::Var(symbol) = call_expr {
                Some(*symbol)
            } else {
                None
            };

            let fn_type = Type2::Variable(*fn_var);
            let fn_region = region;
            let fn_expected = Expected::NoExpectation(fn_type.shallow_clone());

            let fn_reason = Reason::FnCall {
                name: opt_symbol,
                arity: args.len() as u8,
                called_via: *called_via,
            };

            let fn_con = constrain_expr(arena, env, call_expr, fn_expected, region);

            // The function's return type
            // TODO: don't use expr_var?
            let ret_type = Type2::Variable(*expr_var);

            // type of values captured in the closure
            let closure_type = Type2::Variable(*closure_var);

            // This will be used in the occurs check
            let mut vars = BumpVec::with_capacity_in(2 + args.len(), arena);

            vars.push(*fn_var);
            // TODO: don't use expr_var?
            vars.push(*expr_var);
            vars.push(*closure_var);

            let mut arg_types = BumpVec::with_capacity_in(args.len(), arena);
            let mut arg_cons = BumpVec::with_capacity_in(args.len(), arena);

            for (index, arg_node_id) in args.iter_node_ids().enumerate() {
                let (arg_var, arg) = env.pool.get(arg_node_id);
                let arg_expr = env.pool.get(*arg);

                let region = region;
                let arg_type = Type2::Variable(*arg_var);

                let reason = Reason::FnArg {
                    name: opt_symbol,
                    arg_index: HumanIndex::zero_based(index),
                };

                let expected_arg = Expected::ForReason(reason, arg_type.shallow_clone(), region);

                let arg_con = constrain_expr(arena, env, arg_expr, expected_arg, region);

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = Expected::ForReason(
                fn_reason,
                Type2::Function(
                    PoolVec::new(arg_types.into_iter(), env.pool),
                    env.pool.add(closure_type),
                    env.pool.add(ret_type.shallow_clone()),
                ),
                region,
            );

            let category = Category::CallResult(opt_symbol, *called_via);

            let mut and_constraints = BumpVec::with_capacity_in(4, arena);

            and_constraints.push(fn_con);
            and_constraints.push(Eq(fn_type, expected_fn_type, category.clone(), fn_region));
            and_constraints.push(And(arg_cons));
            and_constraints.push(Eq(ret_type, expected, category, region));

            exists(arena, vars, And(and_constraints))
        }
        Expr2::Accessor {
            function_var,
            closure_var,
            field,
            record_var,
            ext_var,
            field_var,
        } => {
            let ext_var = *ext_var;
            let ext_type = Type2::Variable(ext_var);

            let field_var = *field_var;
            let field_type = Type2::Variable(field_var);

            let record_field =
                types::RecordField::Demanded(env.pool.add(field_type.shallow_clone()));

            let record_type = Type2::Record(
                PoolVec::new(vec![(*field, record_field)].into_iter(), env.pool),
                env.pool.add(ext_type),
            );

            let category = Category::Accessor(IndexOrField::Field(field.as_str(env.pool).into()));

            let record_expected = Expected::NoExpectation(record_type.shallow_clone());
            let record_con = Eq(
                Type2::Variable(*record_var),
                record_expected,
                category.clone(),
                region,
            );

            let function_type = Type2::Function(
                PoolVec::new(vec![record_type].into_iter(), env.pool),
                env.pool.add(Type2::Variable(*closure_var)),
                env.pool.add(field_type),
            );

            let mut flex_vars = BumpVec::with_capacity_in(5, arena);

            flex_vars.push(*record_var);
            flex_vars.push(*function_var);
            flex_vars.push(*closure_var);
            flex_vars.push(field_var);
            flex_vars.push(ext_var);

            let mut and_constraints = BumpVec::with_capacity_in(3, arena);

            and_constraints.push(Eq(
                function_type.shallow_clone(),
                expected,
                category.clone(),
                region,
            ));

            and_constraints.push(Eq(
                function_type,
                Expected::NoExpectation(Type2::Variable(*function_var)),
                category,
                region,
            ));

            and_constraints.push(record_con);

            exists(arena, flex_vars, And(and_constraints))
        }
        Expr2::Access {
            expr: expr_id,
            field,
            field_var,
            record_var,
            ext_var,
        } => {
            let ext_type = Type2::Variable(*ext_var);

            let field_type = Type2::Variable(*field_var);

            let record_field =
                types::RecordField::Demanded(env.pool.add(field_type.shallow_clone()));

            let record_type = Type2::Record(
                PoolVec::new(vec![(*field, record_field)].into_iter(), env.pool),
                env.pool.add(ext_type),
            );

            let record_expected = Expected::NoExpectation(record_type);

            let category = Category::RecordAccess(field.as_str(env.pool).into());

            let record_con = Eq(
                Type2::Variable(*record_var),
                record_expected.shallow_clone(),
                category.clone(),
                region,
            );

            let access_expr = env.pool.get(*expr_id);

            let constraint = constrain_expr(arena, env, access_expr, record_expected, region);

            let mut flex_vars = BumpVec::with_capacity_in(3, arena);

            flex_vars.push(*record_var);
            flex_vars.push(*field_var);
            flex_vars.push(*ext_var);

            let mut and_constraints = BumpVec::with_capacity_in(3, arena);

            and_constraints.push(constraint);
            and_constraints.push(Eq(field_type, expected, category, region));
            and_constraints.push(record_con);

            exists(arena, flex_vars, And(and_constraints))
        }
        Expr2::If {
            cond_var,
            expr_var,
            branches,
            final_else,
        } => {
            let expect_bool = |region| {
                let bool_type = Type2::Variable(Variable::BOOL);
                Expected::ForReason(Reason::IfCondition, bool_type, region)
            };

            let mut branch_cons = BumpVec::with_capacity_in(2 * branches.len() + 3, arena);

            // TODO why does this cond var exist? is it for error messages?
            // let first_cond_region = branches[0].0.region;
            let cond_var_is_bool_con = Eq(
                Type2::Variable(*cond_var),
                expect_bool(region),
                Category::If,
                region,
            );

            branch_cons.push(cond_var_is_bool_con);

            let final_else_expr = env.pool.get(*final_else);

            let mut flex_vars = BumpVec::with_capacity_in(2, arena);

            flex_vars.push(*cond_var);
            flex_vars.push(*expr_var);

            match expected {
                Expected::FromAnnotation(name, arity, ann_source, tipe) => {
                    let num_branches = branches.len() + 1;

                    for (index, branch_id) in branches.iter_node_ids().enumerate() {
                        let (cond_id, body_id) = env.pool.get(branch_id);

                        let cond = env.pool.get(*cond_id);
                        let body = env.pool.get(*body_id);

                        let cond_con =
                            constrain_expr(arena, env, cond, expect_bool(region), region);

                        let then_con = constrain_expr(
                            arena,
                            env,
                            body,
                            Expected::FromAnnotation(
                                name.clone(),
                                arity,
                                AnnotationSource::TypedIfBranch {
                                    index: HumanIndex::zero_based(index),
                                    num_branches,
                                    region: ann_source.region(),
                                },
                                tipe.shallow_clone(),
                            ),
                            region,
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }

                    let else_con = constrain_expr(
                        arena,
                        env,
                        final_else_expr,
                        Expected::FromAnnotation(
                            name,
                            arity,
                            AnnotationSource::TypedIfBranch {
                                index: HumanIndex::zero_based(branches.len()),
                                num_branches,
                                region: ann_source.region(),
                            },
                            tipe.shallow_clone(),
                        ),
                        region,
                    );

                    let ast_con = Eq(
                        Type2::Variable(*expr_var),
                        Expected::NoExpectation(tipe),
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    );

                    branch_cons.push(ast_con);
                    branch_cons.push(else_con);

                    exists(arena, flex_vars, And(branch_cons))
                }
                _ => {
                    for (index, branch_id) in branches.iter_node_ids().enumerate() {
                        let (cond_id, body_id) = env.pool.get(branch_id);

                        let cond = env.pool.get(*cond_id);
                        let body = env.pool.get(*body_id);

                        let cond_con =
                            constrain_expr(arena, env, cond, expect_bool(region), region);

                        let then_con = constrain_expr(
                            arena,
                            env,
                            body,
                            Expected::ForReason(
                                Reason::IfBranch {
                                    index: HumanIndex::zero_based(index),
                                    total_branches: branches.len(),
                                },
                                Type2::Variable(*expr_var),
                                // should be from body
                                region,
                            ),
                            region,
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }

                    let else_con = constrain_expr(
                        arena,
                        env,
                        final_else_expr,
                        Expected::ForReason(
                            Reason::IfBranch {
                                index: HumanIndex::zero_based(branches.len()),
                                total_branches: branches.len() + 1,
                            },
                            Type2::Variable(*expr_var),
                            // should come from final_else
                            region,
                        ),
                        region,
                    );

                    branch_cons.push(Eq(
                        Type2::Variable(*expr_var),
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    ));

                    branch_cons.push(else_con);

                    exists(arena, flex_vars, And(branch_cons))
                }
            }
        }
        Expr2::When {
            cond_var,
            expr_var,
            cond: cond_id,
            branches,
        } => {
            // Infer the condition expression's type.
            let cond_type = Type2::Variable(*cond_var);

            let cond = env.pool.get(*cond_id);

            let expr_con = constrain_expr(
                arena,
                env,
                cond,
                Expected::NoExpectation(cond_type.shallow_clone()),
                region,
            );

            let mut constraints = BumpVec::with_capacity_in(branches.len() + 1, arena);

            constraints.push(expr_con);

            let mut flex_vars = BumpVec::with_capacity_in(2, arena);

            flex_vars.push(*cond_var);
            flex_vars.push(*expr_var);

            match &expected {
                Expected::FromAnnotation(name, arity, ann_source, _typ) => {
                    // NOTE deviation from elm.
                    //
                    // in elm, `_typ` is used, but because we have this `expr_var` too
                    // and need to constrain it, this is what works and gives better error messages
                    let typ = Type2::Variable(*expr_var);

                    for (index, when_branch_id) in branches.iter_node_ids().enumerate() {
                        let when_branch = env.pool.get(when_branch_id);

                        // let pattern_region = Region::across_all(
                        //     when_branch.patterns.iter(env.pool).map(|v| &v.region),
                        // );

                        let pattern_expected = |sub_pattern, sub_region| {
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: HumanIndex::zero_based(index),
                                    sub_pattern,
                                },
                                cond_type.shallow_clone(),
                                sub_region,
                            )
                        };

                        let branch_con = constrain_when_branch(
                            arena,
                            env,
                            // TODO: when_branch.value.region,
                            region,
                            when_branch,
                            pattern_expected,
                            Expected::FromAnnotation(
                                name.clone(),
                                *arity,
                                AnnotationSource::TypedWhenBranch {
                                    index: HumanIndex::zero_based(index),
                                    region: ann_source.region(),
                                },
                                typ.shallow_clone(),
                            ),
                        );

                        constraints.push(branch_con);
                    }

                    constraints.push(Eq(typ, expected, Category::When, region));

                    return exists(arena, flex_vars, And(constraints));
                }

                _ => {
                    let branch_type = Type2::Variable(*expr_var);
                    let mut branch_cons = BumpVec::with_capacity_in(branches.len(), arena);

                    for (index, when_branch_id) in branches.iter_node_ids().enumerate() {
                        let when_branch = env.pool.get(when_branch_id);

                        // let pattern_region =
                        //     Region::across_all(when_branch.patterns.iter().map(|v| &v.region));

                        let pattern_expected = |sub_pattern, sub_region| {
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: HumanIndex::zero_based(index),
                                    sub_pattern,
                                },
                                cond_type.shallow_clone(),
                                sub_region,
                            )
                        };

                        let branch_con = constrain_when_branch(
                            arena,
                            env,
                            region,
                            when_branch,
                            pattern_expected,
                            Expected::ForReason(
                                Reason::WhenBranch {
                                    index: HumanIndex::zero_based(index),
                                },
                                branch_type.shallow_clone(),
                                // TODO: when_branch.value.region,
                                region,
                            ),
                        );

                        branch_cons.push(branch_con);
                    }

                    let mut and_constraints = BumpVec::with_capacity_in(2, arena);

                    and_constraints.push(And(branch_cons));
                    and_constraints.push(Eq(branch_type, expected, Category::When, region));

                    constraints.push(And(and_constraints));
                }
            }

            // exhautiveness checking happens when converting to mono::Expr
            exists(arena, flex_vars, And(constraints))
        }
        Expr2::LetValue {
            def_id,
            body_id,
            body_var,
        } => {
            let value_def = env.pool.get(*def_id);
            let body = env.pool.get(*body_id);

            let body_con = constrain_expr(arena, env, body, expected.shallow_clone(), region);

            match value_def {
                ValueDef::WithAnnotation { .. } => todo!("implement {:?}", value_def),
                ValueDef::NoAnnotation {
                    pattern_id,
                    expr_id,
                    expr_var,
                } => {
                    let pattern = env.pool.get(*pattern_id);

                    let mut flex_vars = BumpVec::with_capacity_in(1, arena);
                    flex_vars.push(*body_var);

                    let expr_type = Type2::Variable(*expr_var);

                    let pattern_expected = PExpected::NoExpectation(expr_type.shallow_clone());
                    let mut state = PatternState2 {
                        headers: BumpMap::new_in(arena),
                        vars: BumpVec::with_capacity_in(1, arena),
                        constraints: BumpVec::with_capacity_in(1, arena),
                    };

                    constrain_pattern(
                        arena,
                        env,
                        pattern,
                        region,
                        pattern_expected,
                        &mut state,
                        false,
                    );
                    state.vars.push(*expr_var);

                    let def_expr = env.pool.get(*expr_id);

                    let constrained_def = Let(arena.alloc(LetConstraint {
                        rigid_vars: BumpVec::new_in(arena),
                        flex_vars: state.vars,
                        def_types: state.headers,
                        defs_constraint: Let(arena.alloc(LetConstraint {
                            rigid_vars: BumpVec::new_in(arena), // always empty
                            flex_vars: BumpVec::new_in(arena), // empty, because our functions have no arguments
                            def_types: BumpMap::new_in(arena), // empty, because our functions have no arguments!
                            defs_constraint: And(state.constraints),
                            ret_constraint: constrain_expr(
                                arena,
                                env,
                                def_expr,
                                Expected::NoExpectation(expr_type),
                                region,
                            ),
                        })),
                        ret_constraint: body_con,
                    }));

                    let mut and_constraints = BumpVec::with_capacity_in(2, arena);

                    and_constraints.push(constrained_def);
                    and_constraints.push(Eq(
                        Type2::Variable(*body_var),
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        // TODO: needs to be ret region
                        region,
                    ));

                    exists(arena, flex_vars, And(and_constraints))
                }
            }
        }
        // In an expression like
        //   id = \x -> x
        //
        //   id 1
        // The `def_id` refers to the definition `id = \x -> x`,
        // and the body refers to `id 1`.
        Expr2::LetFunction {
            def_id,
            body_id,
            body_var: _,
        } => {
            let body = env.pool.get(*body_id);
            let body_con = constrain_expr(arena, env, body, expected.shallow_clone(), region);

            let function_def = env.pool.get(*def_id);

            let (name, arguments, body_id, rigid_vars, args_constrs) = match function_def {
                FunctionDef::WithAnnotation {
                    name,
                    arguments,
                    body_id,
                    rigids,
                    return_type: _,
                } => {
                    // The annotation gives us arguments with proper Type2s, but the constraints we
                    // generate below args bound to type variables. Create fresh ones and bind them
                    // to the types we already know.
                    let mut args_constrs = BumpVec::with_capacity_in(arguments.len(), arena);
                    let args_vars = PoolVec::with_capacity(arguments.len() as u32, env.pool);
                    for (arg_ty_node_id, arg_var_node_id) in
                        arguments.iter_node_ids().zip(args_vars.iter_node_ids())
                    {
                        let (ty, pattern) = env.pool.get(arg_ty_node_id);
                        let arg_var = env.var_store.fresh();
                        let ty = env.pool.get(*ty);
                        args_constrs.push(Eq(
                            Type2::Variable(arg_var),
                            Expected::NoExpectation(ty.shallow_clone()),
                            Category::Storage(std::file!(), std::line!()),
                            // TODO: should be the actual region of the argument
                            region,
                        ));
                        env.pool[arg_var_node_id] = (arg_var, *pattern);
                    }

                    let rigids = env.pool.get(*rigids);
                    let rigid_vars: BumpVec<Variable> =
                        BumpVec::from_iter_in(rigids.names.iter(env.pool).map(|&(_, v)| v), arena);

                    (name, args_vars, body_id, rigid_vars, args_constrs)
                }
                FunctionDef::NoAnnotation {
                    name,
                    arguments,
                    body_id,
                    return_var: _,
                } => {
                    (
                        name,
                        arguments.shallow_clone(),
                        body_id,
                        BumpVec::new_in(arena), // The function is unannotated, so there are no rigid type vars
                        BumpVec::new_in(arena), // No extra constraints to generate for arguments
                    )
                }
            };

            // A function definition is equivalent to a named value definition, where the
            // value is a closure. So, we create a closure definition in correspondence
            // with the function definition, generate type constraints for it, and demand
            // that type of the function is just the type of the resolved closure.
            let fn_var = env.var_store.fresh();
            let fn_ty = Type2::Variable(fn_var);

            let extra = ClosureExtra {
                return_type: env.var_store.fresh(),
                captured_symbols: PoolVec::empty(env.pool),
                closure_type: env.var_store.fresh(),
                closure_ext_var: env.var_store.fresh(),
            };
            let clos = Expr2::Closure {
                args: arguments.shallow_clone(),
                uniq_symbol: *name,
                body_id: *body_id,
                function_type: env.var_store.fresh(),
                extra: env.pool.add(extra),
                recursive: roc_can::expr::Recursive::Recursive,
            };
            let clos_con = constrain_expr(
                arena,
                env,
                &clos,
                Expected::NoExpectation(fn_ty.shallow_clone()),
                region,
            );

            // This is the `foo` part in `foo = \...`. We want to bind the name of the
            // function with its type, whose constraints we generated above.
            let mut def_pattern_state = PatternState2 {
                headers: BumpMap::new_in(arena),
                vars: BumpVec::new_in(arena),
                constraints: args_constrs,
            };
            def_pattern_state.headers.insert(*name, fn_ty);
            def_pattern_state.vars.push(fn_var);

            Let(arena.alloc(LetConstraint {
                rigid_vars,
                flex_vars: def_pattern_state.vars,
                def_types: def_pattern_state.headers, // Binding function name -> its type
                defs_constraint: Let(arena.alloc(LetConstraint {
                    rigid_vars: BumpVec::new_in(arena), // always empty
                    flex_vars: BumpVec::new_in(arena), // empty, because our functions have no arguments
                    def_types: BumpMap::new_in(arena), // empty, because our functions have no arguments
                    defs_constraint: And(def_pattern_state.constraints),
                    ret_constraint: clos_con,
                })),
                ret_constraint: body_con,
            }))
        }
        Expr2::Update {
            symbol,
            updates,
            ext_var,
            record_var,
        } => {
            let field_types = PoolVec::with_capacity(updates.len() as u32, env.pool);
            let mut flex_vars = BumpVec::with_capacity_in(updates.len() + 2, arena);
            let mut cons = BumpVec::with_capacity_in(updates.len() + 1, arena);
            let mut record_key_updates = SendMap::default();

            for (record_field_id, field_type_node_id) in
                updates.iter_node_ids().zip(field_types.iter_node_ids())
            {
                let record_field = env.pool.get(record_field_id);

                match record_field {
                    RecordField::LabeledValue(pool_str, var, node_id) => {
                        let expr = env.pool.get(*node_id);

                        let (field_type, field_con) = constrain_field_update(
                            arena,
                            env,
                            *var,
                            pool_str.as_str(env.pool).into(),
                            expr,
                        );

                        let field_type_id = env.pool.add(field_type);

                        env.pool[field_type_node_id] =
                            (*pool_str, types::RecordField::Required(field_type_id));

                        record_key_updates.insert(pool_str.as_str(env.pool).into(), Region::zero());

                        flex_vars.push(*var);
                        cons.push(field_con);
                    }
                    e => todo!("{:?}", e),
                }
            }

            let fields_type = Type2::Record(field_types, env.pool.add(Type2::Variable(*ext_var)));
            let record_type = Type2::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = Eq(
                record_type.shallow_clone(),
                Expected::NoExpectation(fields_type),
                Category::Record,
                region,
            );
            let record_con = Eq(
                record_type.shallow_clone(),
                expected,
                Category::Record,
                region,
            );

            flex_vars.push(*record_var);
            flex_vars.push(*ext_var);

            let con = Lookup(
                *symbol,
                Expected::ForReason(
                    Reason::RecordUpdateKeys(*symbol, record_key_updates),
                    record_type,
                    region,
                ),
                region,
            );

            // ensure constraints are solved in this order, gives better errors
            cons.insert(0, fields_con);
            cons.insert(1, con);
            cons.insert(2, record_con);

            exists(arena, flex_vars, And(cons))
        }

        Expr2::RunLowLevel { op, args, ret_var } => {
            // This is a modified version of what we do for function calls.

            // The operation's return type
            let ret_type = Type2::Variable(*ret_var);

            // This will be used in the occurs check
            let mut vars = BumpVec::with_capacity_in(1 + args.len(), arena);

            vars.push(*ret_var);

            let mut arg_types = BumpVec::with_capacity_in(args.len(), arena);
            let mut arg_cons = BumpVec::with_capacity_in(args.len(), arena);

            for (index, node_id) in args.iter_node_ids().enumerate() {
                let (arg_var, arg_id) = env.pool.get(node_id);

                vars.push(*arg_var);

                let arg_type = Type2::Variable(*arg_var);

                let reason = Reason::LowLevelOpArg {
                    op: *op,
                    arg_index: HumanIndex::zero_based(index),
                };
                let expected_arg =
                    Expected::ForReason(reason, arg_type.shallow_clone(), Region::zero());
                let arg = env.pool.get(*arg_id);

                let arg_con = constrain_expr(arena, env, arg, expected_arg, Region::zero());

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let category = Category::LowLevelOpResult(*op);

            let mut and_constraints = BumpVec::with_capacity_in(2, arena);

            and_constraints.push(And(arg_cons));
            and_constraints.push(Eq(ret_type, expected, category, region));

            exists(arena, vars, And(and_constraints))
        }
        Expr2::Closure {
            args,
            uniq_symbol,
            body_id,
            function_type: fn_var,
            extra,
            ..
        } => {
            // NOTE defs are treated somewhere else!
            let body = env.pool.get(*body_id);

            let ClosureExtra {
                captured_symbols,
                return_type: ret_var,
                closure_type: closure_var,
                closure_ext_var,
            } = env.pool.get(*extra);

            let closure_type = Type2::Variable(*closure_var);
            let return_type = Type2::Variable(*ret_var);

            let (mut vars, pattern_state, function_type) =
                constrain_untyped_args(arena, env, args, closure_type, return_type.shallow_clone());

            vars.push(*ret_var);
            vars.push(*closure_var);
            vars.push(*closure_ext_var);
            vars.push(*fn_var);

            let expected_body_type = Expected::NoExpectation(return_type);
            // Region here should come from body expr
            let ret_constraint = constrain_expr(arena, env, body, expected_body_type, region);

            let captured_symbols_as_vec = captured_symbols
                .iter(env.pool)
                .copied()
                .collect::<Vec<(Symbol, Variable)>>();

            // make sure the captured symbols are sorted!
            debug_assert_eq!(captured_symbols_as_vec, {
                let mut copy: Vec<(Symbol, Variable)> = captured_symbols_as_vec.clone();

                copy.sort();

                copy
            });

            let closure_constraint = constrain_closure_size(
                arena,
                env,
                *uniq_symbol,
                region,
                captured_symbols,
                *closure_var,
                *closure_ext_var,
                &mut vars,
            );

            let mut and_constraints = BumpVec::with_capacity_in(4, arena);

            and_constraints.push(Let(arena.alloc(LetConstraint {
                rigid_vars: BumpVec::new_in(arena),
                flex_vars: pattern_state.vars,
                def_types: pattern_state.headers,
                defs_constraint: And(pattern_state.constraints),
                ret_constraint,
            })));

            // "the closure's type is equal to expected type"
            and_constraints.push(Eq(
                function_type.shallow_clone(),
                expected,
                Category::Lambda,
                region,
            ));

            // "fn_var is equal to the closure's type" - fn_var is used in code gen
            and_constraints.push(Eq(
                Type2::Variable(*fn_var),
                Expected::NoExpectation(function_type),
                Category::Storage(std::file!(), std::line!()),
                region,
            ));

            and_constraints.push(closure_constraint);

            exists(arena, vars, And(and_constraints))
        }
        Expr2::LetRec { .. } => todo!(),
    }
}

fn exists<'a>(
    arena: &'a Bump,
    flex_vars: BumpVec<'a, Variable>,
    defs_constraint: Constraint<'a>,
) -> Constraint<'a> {
    Constraint::Let(arena.alloc(LetConstraint {
        rigid_vars: BumpVec::new_in(arena),
        flex_vars,
        def_types: BumpMap::new_in(arena),
        defs_constraint,
        ret_constraint: Constraint::True,
    }))
}

#[allow(clippy::too_many_arguments)]
fn constrain_tag<'a>(
    arena: &'a Bump,
    env: &mut Env,
    expected: Expected<Type2>,
    region: Region,
    tag_name: TagName,
    arguments: &PoolVec<(Variable, ExprId)>,
    ext_var: Variable,
    variant_var: Variable,
) -> Constraint<'a> {
    use Constraint::*;

    let mut flex_vars = BumpVec::with_capacity_in(arguments.len(), arena);
    let types = PoolVec::with_capacity(arguments.len() as u32, env.pool);
    let mut arg_cons = BumpVec::with_capacity_in(arguments.len(), arena);

    for (argument_node_id, type_node_id) in arguments.iter_node_ids().zip(types.iter_node_ids()) {
        let (var, expr_node_id) = env.pool.get(argument_node_id);

        let argument_expr = env.pool.get(*expr_node_id);

        let arg_con = constrain_expr(
            arena,
            env,
            argument_expr,
            Expected::NoExpectation(Type2::Variable(*var)),
            region,
        );

        arg_cons.push(arg_con);
        flex_vars.push(*var);

        env.pool[type_node_id] = Type2::Variable(*var);
    }

    let union_con = Eq(
        Type2::TagUnion(
            PoolVec::new(std::iter::once((tag_name.clone(), types)), env.pool),
            env.pool.add(Type2::Variable(ext_var)),
        ),
        expected.shallow_clone(),
        Category::TagApply {
            tag_name,
            args_count: arguments.len(),
        },
        region,
    );

    let ast_con = Eq(
        Type2::Variable(variant_var),
        expected,
        Category::Storage(std::file!(), std::line!()),
        region,
    );

    flex_vars.push(variant_var);
    flex_vars.push(ext_var);

    arg_cons.push(union_con);
    arg_cons.push(ast_con);

    exists(arena, flex_vars, And(arg_cons))
}

fn constrain_field<'a>(
    arena: &'a Bump,
    env: &mut Env,
    field_var: Variable,
    expr: &Expr2,
) -> (Type2, Constraint<'a>) {
    let field_type = Type2::Variable(field_var);
    let field_expected = Expected::NoExpectation(field_type.shallow_clone());
    let constraint = constrain_expr(arena, env, expr, field_expected, Region::zero());

    (field_type, constraint)
}

#[inline(always)]
fn constrain_field_update<'a>(
    arena: &'a Bump,
    env: &mut Env,
    field_var: Variable,
    field: Lowercase,
    expr: &Expr2,
) -> (Type2, Constraint<'a>) {
    let field_type = Type2::Variable(field_var);
    let reason = Reason::RecordUpdateValue(field);
    let field_expected = Expected::ForReason(reason, field_type.shallow_clone(), Region::zero());
    let con = constrain_expr(arena, env, expr, field_expected, Region::zero());

    (field_type, con)
}

fn constrain_empty_record<'a>(expected: Expected<Type2>, region: Region) -> Constraint<'a> {
    Constraint::Eq(Type2::EmptyRec, expected, Category::Record, region)
}

#[inline(always)]
fn constrain_when_branch<'a>(
    arena: &'a Bump,
    env: &mut Env,
    region: Region,
    when_branch: &WhenBranch,
    pattern_expected: impl Fn(HumanIndex, Region) -> PExpected<Type2>,
    expr_expected: Expected<Type2>,
) -> Constraint<'a> {
    let when_expr = env.pool.get(when_branch.body);

    let ret_constraint = constrain_expr(arena, env, when_expr, expr_expected, region);

    let mut state = PatternState2 {
        headers: BumpMap::new_in(arena),
        vars: BumpVec::with_capacity_in(1, arena),
        constraints: BumpVec::with_capacity_in(1, arena),
    };

    // TODO investigate for error messages, is it better to unify all branches with a variable,
    // then unify that variable with the expectation?
    for (sub_pattern, pattern_id) in when_branch.patterns.iter_node_ids().enumerate() {
        let pattern = env.pool.get(pattern_id);

        let pattern_expected = pattern_expected(
            HumanIndex::zero_based(sub_pattern),
            // TODO: use the proper subpattern region. Not available to us right now.
            region,
        );

        constrain_pattern(
            arena,
            env,
            pattern,
            // loc_pattern.region,
            region,
            pattern_expected,
            &mut state,
            true,
        );
    }

    if let Some(guard_id) = &when_branch.guard {
        let guard = env.pool.get(*guard_id);

        let guard_constraint = constrain_expr(
            arena,
            env,
            guard,
            Expected::ForReason(
                Reason::WhenGuard,
                Type2::Variable(Variable::BOOL),
                // TODO: loc_guard.region,
                region,
            ),
            region,
        );

        // must introduce the headers from the pattern before constraining the guard
        Constraint::Let(arena.alloc(LetConstraint {
            rigid_vars: BumpVec::new_in(arena),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: Constraint::And(state.constraints),
            ret_constraint: Constraint::Let(arena.alloc(LetConstraint {
                rigid_vars: BumpVec::new_in(arena),
                flex_vars: BumpVec::new_in(arena),
                def_types: BumpMap::new_in(arena),
                defs_constraint: guard_constraint,
                ret_constraint,
            })),
        }))
    } else {
        Constraint::Let(arena.alloc(LetConstraint {
            rigid_vars: BumpVec::new_in(arena),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: Constraint::And(state.constraints),
            ret_constraint,
        }))
    }
}

fn make_pattern_constraint(
    region: Region,
    category: PatternCategory,
    actual: Type2,
    expected: PExpected<Type2>,
    presence_con: bool,
) -> Constraint<'static> {
    if presence_con {
        Constraint::Present(
            actual,
            PresenceConstraint::Pattern(region, category, expected),
        )
    } else {
        Constraint::Pattern(region, category, actual, expected)
    }
}

/// This accepts PatternState (rather than returning it) so that the caller can
/// initialize the Vecs in PatternState using with_capacity
/// based on its knowledge of their lengths.
pub fn constrain_pattern<'a>(
    arena: &'a Bump,
    env: &mut Env,
    pattern: &Pattern2,
    region: Region,
    expected: PExpected<Type2>,
    state: &mut PatternState2<'a>,
    destruct_position: bool,
) {
    use Pattern2::*;

    match pattern {
        Underscore if destruct_position => {
            // This is an underscore in a position where we destruct a variable,
            // like a when expression:
            //   when x is
            //     A -> ""
            //     _ -> ""
            // so, we know that "x" (in this case, a tag union) must be open.
            state.constraints.push(Constraint::Present(
                expected.get_type(),
                PresenceConstraint::IsOpen,
            ));
        }

        Underscore | UnsupportedPattern(_) | MalformedPattern(_, _) | Shadowed { .. } => {
            // Neither the _ pattern nor erroneous ones add any constraints.
        }

        Identifier(symbol) => {
            if destruct_position {
                state.constraints.push(Constraint::Present(
                    expected.get_type_ref().shallow_clone(),
                    PresenceConstraint::IsOpen,
                ));
            }
            state.headers.insert(*symbol, expected.get_type());
        }

        NumLiteral(var, _) => {
            state.vars.push(*var);

            let type_id = env.pool.add(Type2::Variable(*var));

            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Num,
                num_num(env.pool, type_id),
                expected,
            ));
        }

        IntLiteral(_int_val) => {
            let precision_var = env.var_store.fresh();

            let range = env.add(Type2::Variable(precision_var), region);

            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Int,
                num_int(env.pool, range),
                expected,
            ));
        }

        FloatLiteral(_float_val) => {
            let precision_var = env.var_store.fresh();

            let range = env.add(Type2::Variable(precision_var), region);

            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Float,
                num_float(env.pool, range),
                expected,
            ));
        }

        StrLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Str,
                str_type(env.pool),
                expected,
            ));
        }

        CharacterLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Character,
                num_unsigned32(env.pool),
                expected,
            ));
        }

        RecordDestructure {
            whole_var,
            ext_var,
            destructs,
        } => {
            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            let ext_type = Type2::Variable(*ext_var);

            let mut field_types = Vec::new();

            for destruct_id in destructs.iter_node_ids() {
                let RecordDestruct {
                    var,
                    label,
                    symbol,
                    typ,
                } = env.pool.get(destruct_id);

                let pat_type = Type2::Variable(*var);
                let expected = PExpected::NoExpectation(pat_type.shallow_clone());

                if !state.headers.contains_key(symbol) {
                    state.headers.insert(*symbol, pat_type.shallow_clone());
                }

                let destruct_type = env.pool.get(*typ);

                let field_type = match destruct_type {
                    DestructType::Guard(guard_var, guard_id) => {
                        state.constraints.push(make_pattern_constraint(
                            region,
                            PatternCategory::PatternGuard,
                            Type2::Variable(*guard_var),
                            PExpected::ForReason(
                                PReason::PatternGuard,
                                pat_type.shallow_clone(),
                                // TODO: region should be from guard_id
                                region,
                            ),
                            destruct_position,
                        ));

                        state.vars.push(*guard_var);

                        let guard = env.pool.get(*guard_id);

                        // TODO: region should be from guard_id
                        constrain_pattern(
                            arena,
                            env,
                            guard,
                            region,
                            expected,
                            state,
                            destruct_position,
                        );

                        types::RecordField::Demanded(env.pool.add(pat_type))
                    }
                    DestructType::Optional(expr_var, expr_id) => {
                        state.constraints.push(make_pattern_constraint(
                            region,
                            PatternCategory::PatternDefault,
                            Type2::Variable(*expr_var),
                            PExpected::ForReason(
                                PReason::OptionalField,
                                pat_type.shallow_clone(),
                                // TODO: region should be from expr_id
                                region,
                            ),
                            destruct_position,
                        ));

                        state.vars.push(*expr_var);

                        let expr_expected = Expected::ForReason(
                            Reason::RecordDefaultField(label.as_str(env.pool).into()),
                            pat_type.shallow_clone(),
                            // TODO: region should be from expr_id
                            region,
                        );

                        let expr = env.pool.get(*expr_id);

                        // TODO: region should be from expr_id
                        let expr_con = constrain_expr(arena, env, expr, expr_expected, region);

                        state.constraints.push(expr_con);

                        types::RecordField::Optional(env.pool.add(pat_type))
                    }
                    DestructType::Required => {
                        // No extra constraints necessary.
                        types::RecordField::Demanded(env.pool.add(pat_type))
                    }
                };

                field_types.push((*label, field_type));

                state.vars.push(*var);
            }

            let record_type = Type2::Record(
                PoolVec::new(field_types.into_iter(), env.pool),
                env.pool.add(ext_type),
            );

            let whole_con = Constraint::Eq(
                Type2::Variable(*whole_var),
                Expected::NoExpectation(record_type),
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let record_con = make_pattern_constraint(
                region,
                PatternCategory::Record,
                Type2::Variable(*whole_var),
                expected,
                destruct_position,
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }
        Tag {
            whole_var,
            ext_var,
            tag_name: name,
            arguments,
        } => {
            let tag_name = TagName(name.as_str(env.pool).into());

            constrain_tag_pattern(
                arena,
                env,
                region,
                expected,
                state,
                *whole_var,
                *ext_var,
                arguments,
                tag_name,
                destruct_position,
            );
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn constrain_tag_pattern<'a>(
    arena: &'a Bump,
    env: &mut Env,
    region: Region,
    expected: PExpected<Type2>,
    state: &mut PatternState2<'a>,
    whole_var: Variable,
    ext_var: Variable,
    arguments: &PoolVec<(Variable, PatternId)>,
    tag_name: TagName,
    destruct_position: bool,
) {
    let mut argument_types = Vec::with_capacity(arguments.len());

    for (index, arg_id) in arguments.iter_node_ids().enumerate() {
        let (pattern_var, pattern_id) = env.pool.get(arg_id);
        let pattern = env.pool.get(*pattern_id);

        state.vars.push(*pattern_var);

        let pattern_type = Type2::Variable(*pattern_var);
        argument_types.push(pattern_type.shallow_clone());

        let expected = PExpected::ForReason(
            PReason::TagArg {
                tag_name: tag_name.clone(),
                index: HumanIndex::zero_based(index),
            },
            pattern_type,
            region,
        );

        // TODO region should come from pattern
        constrain_pattern(arena, env, pattern, region, expected, state, false);
    }

    let whole_con = if destruct_position {
        Constraint::Present(
            expected.get_type_ref().shallow_clone(),
            PresenceConstraint::IncludesTag(
                tag_name.clone(),
                BumpVec::from_iter_in(argument_types.into_iter(), arena),
            ),
        )
    } else {
        Constraint::Eq(
            Type2::Variable(whole_var),
            Expected::NoExpectation(Type2::TagUnion(
                PoolVec::new(
                    vec![(
                        tag_name.clone(),
                        PoolVec::new(argument_types.into_iter(), env.pool),
                    )]
                    .into_iter(),
                    env.pool,
                ),
                env.pool.add(Type2::Variable(ext_var)),
            )),
            Category::Storage(std::file!(), std::line!()),
            region,
        )
    };

    let tag_con = make_pattern_constraint(
        region,
        PatternCategory::Ctor(tag_name),
        Type2::Variable(whole_var),
        expected,
        destruct_position,
    );

    state.vars.push(whole_var);
    state.vars.push(ext_var);
    state.constraints.push(whole_con);
    state.constraints.push(tag_con);
}

fn constrain_untyped_args<'a>(
    arena: &'a Bump,
    env: &mut Env,
    arguments: &PoolVec<(Variable, PatternId)>,
    closure_type: Type2,
    return_type: Type2,
) -> (BumpVec<'a, Variable>, PatternState2<'a>, Type2) {
    let mut vars = BumpVec::with_capacity_in(arguments.len(), arena);

    let pattern_types = PoolVec::with_capacity(arguments.len() as u32, env.pool);

    let mut pattern_state = PatternState2 {
        headers: BumpMap::new_in(arena),
        vars: BumpVec::with_capacity_in(1, arena),
        constraints: BumpVec::with_capacity_in(1, arena),
    };

    for (arg_node_id, pattern_type_id) in
        arguments.iter_node_ids().zip(pattern_types.iter_node_ids())
    {
        let (pattern_var, pattern_id) = env.pool.get(arg_node_id);
        let pattern = env.pool.get(*pattern_id);

        let pattern_type = Type2::Variable(*pattern_var);
        let pattern_expected = PExpected::NoExpectation(pattern_type.shallow_clone());

        env.pool[pattern_type_id] = pattern_type;

        constrain_pattern(
            arena,
            env,
            pattern,
            // TODO needs to come from pattern
            Region::zero(),
            pattern_expected,
            &mut pattern_state,
            false,
        );

        vars.push(*pattern_var);
    }

    let function_type = Type2::Function(
        pattern_types,
        env.pool.add(closure_type),
        env.pool.add(return_type),
    );

    (vars, pattern_state, function_type)
}

#[allow(clippy::too_many_arguments)]
fn constrain_closure_size<'a>(
    arena: &'a Bump,
    env: &mut Env,
    _name: Symbol,
    region: Region,
    captured_symbols: &PoolVec<(Symbol, Variable)>,
    closure_var: Variable,
    closure_ext_var: Variable,
    variables: &mut BumpVec<'a, Variable>,
) -> Constraint<'a> {
    use Constraint::*;

    debug_assert!(variables.iter().any(|s| *s == closure_var));
    debug_assert!(variables.iter().any(|s| *s == closure_ext_var));

    let tag_arguments = PoolVec::with_capacity(captured_symbols.len() as u32, env.pool);
    let mut captured_symbols_constraints = BumpVec::with_capacity_in(captured_symbols.len(), arena);

    for (captured_symbol_id, tag_arg_id) in captured_symbols
        .iter_node_ids()
        .zip(tag_arguments.iter_node_ids())
    {
        let (symbol, var) = env.pool.get(captured_symbol_id);

        // make sure the variable is registered
        variables.push(*var);

        let tag_arg_type = Type2::Variable(*var);

        // this symbol is captured, so it must be part of the closure type
        env.pool[tag_arg_id] = tag_arg_type.shallow_clone();

        // make the variable equal to the looked-up type of symbol
        captured_symbols_constraints.push(Lookup(
            *symbol,
            Expected::NoExpectation(tag_arg_type),
            Region::zero(),
        ));
    }

    // This is incorrect, but the editor will be using the Can AST soon, so disregarding for now.
    let tag_name = TagName("FAKE CLOSURE".into());
    let closure_type = Type2::TagUnion(
        PoolVec::new(vec![(tag_name, tag_arguments)].into_iter(), env.pool),
        env.pool.add(Type2::Variable(closure_ext_var)),
    );

    let finalizer = Eq(
        Type2::Variable(closure_var),
        Expected::NoExpectation(closure_type),
        Category::ClosureSize,
        region,
    );

    captured_symbols_constraints.push(finalizer);

    And(captured_symbols_constraints)
}

#[inline(always)]
fn builtin_type(symbol: Symbol, args: PoolVec<Type2>) -> Type2 {
    Type2::Apply(symbol, args)
}

#[inline(always)]
fn str_type(pool: &mut Pool) -> Type2 {
    builtin_type(Symbol::STR_STR, PoolVec::empty(pool))
}

#[inline(always)]
fn empty_list_type(pool: &mut Pool, var: Variable) -> Type2 {
    list_type(pool, Type2::Variable(var))
}

#[inline(always)]
fn list_type(pool: &mut Pool, typ: Type2) -> Type2 {
    builtin_type(Symbol::LIST_LIST, PoolVec::new(vec![typ].into_iter(), pool))
}

#[inline(always)]
fn num_float(pool: &mut Pool, range: TypeId) -> Type2 {
    let num_floatingpoint_type = num_floatingpoint(pool, range);
    let num_floatingpoint_id = pool.add(num_floatingpoint_type);

    let num_num_type = num_num(pool, num_floatingpoint_id);
    let num_num_id = pool.add(num_num_type);

    Type2::Alias(
        Symbol::NUM_FRAC,
        PoolVec::new(vec![range].into_iter(), pool),
        num_num_id,
    )
}

#[inline(always)]
fn num_floatingpoint(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = range_type.shallow_clone();

    Type2::Opaque(
        Symbol::NUM_FLOATINGPOINT,
        PoolVec::new(vec![range].into_iter(), pool),
        pool.add(alias_content),
    )
}

#[inline(always)]
fn num_int(pool: &mut Pool, range: TypeId) -> Type2 {
    let num_integer_type = _num_integer(pool, range);
    let num_integer_id = pool.add(num_integer_type);

    let num_num_type = num_num(pool, num_integer_id);
    let num_num_id = pool.add(num_num_type);

    Type2::Alias(
        Symbol::NUM_INT,
        PoolVec::new(vec![range].into_iter(), pool),
        num_num_id,
    )
}

#[inline(always)]
fn _num_signed64(pool: &mut Pool) -> Type2 {
    Type2::Alias(
        Symbol::NUM_SIGNED64,
        PoolVec::empty(pool),
        pool.add(Type2::EmptyTagUnion),
    )
}

#[inline(always)]
fn num_unsigned32(pool: &mut Pool) -> Type2 {
    let alias_content = Type2::EmptyTagUnion;

    Type2::Alias(
        Symbol::NUM_UNSIGNED32,
        PoolVec::empty(pool),
        pool.add(alias_content),
    )
}

#[inline(always)]
fn _num_integer(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = range_type.shallow_clone();

    Type2::Opaque(
        Symbol::NUM_INTEGER,
        PoolVec::new(vec![range].into_iter(), pool),
        pool.add(alias_content),
    )
}

#[inline(always)]
fn num_num(pool: &mut Pool, type_id: TypeId) -> Type2 {
    let range_type = pool.get(type_id);

    let alias_content = range_type.shallow_clone();

    Type2::Opaque(
        Symbol::NUM_NUM,
        PoolVec::new(vec![type_id].into_iter(), pool),
        pool.add(alias_content),
    )
}

#[cfg(test)]
pub mod test_constrain {
    use bumpalo::Bump;
    use roc_can::expected::Expected;
    use roc_collections::all::MutMap;
    use roc_module::{
        ident::Lowercase,
        symbol::{IdentIds, Interns, ModuleIds, Symbol},
    };
    use roc_parse::parser::{SourceError, SyntaxError};
    use roc_region::all::Region;
    use roc_solve::module::Solved;
    use roc_types::{
        pretty_print::{name_and_print_var, DebugPrint},
        subs::{Subs, VarStore, Variable},
    };

    use super::Constraint;
    use crate::{
        constrain::constrain_expr,
        lang::{
            core::{
                expr::{expr2::Expr2, expr_to_expr2::loc_expr_to_expr2, output::Output},
                types::Type2,
            },
            env::Env,
            scope::Scope,
        },
        mem_pool::pool::Pool,
        solve_type,
    };
    use indoc::indoc;

    fn run_solve(
        arena: &Bump,
        mempool: &mut Pool,
        aliases: MutMap<Symbol, roc_types::types::Alias>,
        rigid_variables: MutMap<Variable, Lowercase>,
        constraint: Constraint,
        var_store: VarStore,
    ) -> (Solved<Subs>, solve_type::Env, Vec<solve_type::TypeError>) {
        let env = solve_type::Env {
            vars_by_symbol: MutMap::default(),
            aliases,
        };

        let mut subs = Subs::new_from_varstore(var_store);

        for (var, name) in rigid_variables {
            subs.rigid_var(var, name);
        }

        // Now that the module is parsed, canonicalized, and constrained,
        // we need to type check it.
        let mut problems = Vec::new();

        // Run the solver to populate Subs.
        let (solved_subs, solved_env) =
            solve_type::run(arena, mempool, &env, &mut problems, subs, &constraint);

        (solved_subs, solved_env, problems)
    }

    fn infer_eq(actual: &str, expected_str: &str) {
        let mut env_pool = Pool::with_capacity(1024);
        let env_arena = Bump::new();
        let code_arena = Bump::new();

        let mut var_store = VarStore::default();
        let var = var_store.fresh();
        let dep_idents = IdentIds::exposed_builtins(8);
        let exposed_ident_ids = IdentIds::default();
        let mut module_ids = ModuleIds::default();
        let mod_id = module_ids.get_or_insert(&"ModId123".into());

        let mut env = Env::new(
            mod_id,
            &env_arena,
            &mut env_pool,
            &mut var_store,
            dep_idents,
            &module_ids,
            exposed_ident_ids,
        );

        let mut scope = Scope::new(env.home, env.pool, env.var_store);

        let region = Region::zero();

        let expr2_result = str_to_expr2(&code_arena, actual, &mut env, &mut scope, region);

        match expr2_result {
            Ok((expr, output)) => {
                let constraint = constrain_expr(
                    &code_arena,
                    &mut env,
                    &expr,
                    Expected::NoExpectation(Type2::Variable(var)),
                    Region::zero(),
                );

                let Env {
                    pool,
                    var_store: ref_var_store,
                    mut dep_idents,
                    ..
                } = env;

                // extract the var_store out of the env again
                let mut var_store = VarStore::default();
                std::mem::swap(ref_var_store, &mut var_store);

                let rigids = output.introduced_variables.name_by_var;

                let (mut solved, _, _) = run_solve(
                    &code_arena,
                    pool,
                    Default::default(),
                    rigids,
                    constraint,
                    var_store,
                );

                let subs = solved.inner_mut();

                // Connect the ModuleId to it's IdentIds
                dep_idents.insert(mod_id, env.ident_ids);

                let interns = Interns {
                    module_ids: env.module_ids.clone(),
                    all_ident_ids: dep_idents,
                };

                let actual_str =
                    name_and_print_var(var, subs, mod_id, &interns, DebugPrint::NOTHING);

                assert_eq!(actual_str, expected_str);
            }
            Err(e) => panic!("syntax error {e:?}"),
        }
    }

    pub fn str_to_expr2<'a>(
        arena: &'a Bump,
        input: &'a str,
        env: &mut Env<'a>,
        scope: &mut Scope,
        region: Region,
    ) -> Result<(Expr2, Output), SourceError<'a, SyntaxError<'a>>> {
        match roc_parse::test_helpers::parse_loc_with(arena, input.trim()) {
            Ok(loc_expr) => Ok(loc_expr_to_expr2(arena, loc_expr, env, scope, region)),
            Err(fail) => Err(fail),
        }
    }

    #[test]
    fn constrain_str() {
        infer_eq(
            indoc!(
                r#"
                "type inference!"
                "#
            ),
            "Str",
        )
    }

    // This will be more useful once we actually map
    // strings less than 15 chars to SmallStr
    #[test]
    fn constrain_small_str() {
        infer_eq(
            indoc!(
                r#"
                "a"
                "#
            ),
            "Str",
        )
    }

    #[test]
    fn constrain_empty_record() {
        infer_eq(
            indoc!(
                r#"
                {}
                "#
            ),
            "{}",
        )
    }

    #[test]
    fn constrain_small_int() {
        infer_eq(
            indoc!(
                r#"
                12
                "#
            ),
            "Num *",
        )
    }

    #[test]
    fn constrain_float() {
        infer_eq(
            indoc!(
                r#"
                3.14
                "#
            ),
            "Float *",
        )
    }

    #[test]
    fn constrain_record() {
        infer_eq(
            indoc!(
                r#"
                { x : 1, y : "hi" }
                "#
            ),
            "{ x : Num *, y : Str }",
        )
    }

    #[test]
    fn constrain_empty_list() {
        infer_eq(
            indoc!(
                r#"
                []
                "#
            ),
            "List *",
        )
    }

    #[test]
    fn constrain_list() {
        infer_eq(
            indoc!(
                r#"
                [1, 2]
                "#
            ),
            "List (Num *)",
        )
    }

    #[test]
    fn constrain_list_of_records() {
        infer_eq(
            indoc!(
                r#"
                [{ x: 1 }, { x: 3 }]
                "#
            ),
            "List { x : Num * }",
        )
    }

    #[test]
    fn constrain_tag() {
        infer_eq(
            indoc!(
                r#"
                Foo
                "#
            ),
            "[Foo]",
        )
    }

    #[test]
    fn constrain_call_and_accessor() {
        infer_eq(
            indoc!(
                r#"
                .foo { foo: "bar" }
                "#
            ),
            "Str",
        )
    }

    #[test]
    fn constrain_access() {
        infer_eq(
            indoc!(
                r#"
                { foo: "bar" }.foo
                "#
            ),
            "Str",
        )
    }

    #[test]
    fn constrain_if() {
        infer_eq(
            indoc!(
                r#"
                if True then Green else Red
                "#
            ),
            "[Green, Red]",
        )
    }

    #[test]
    fn constrain_when() {
        infer_eq(
            indoc!(
                r#"
                when if True then Green else Red is
                    Green -> Blue
                    Red -> Purple
                "#
            ),
            "[Blue, Purple]",
        )
    }

    #[test]
    fn constrain_let_value() {
        infer_eq(
            indoc!(
                r#"
                person = { name: "roc" }

                person
                "#
            ),
            "{ name : Str }",
        )
    }

    #[test]
    fn constrain_update() {
        infer_eq(
            indoc!(
                r#"
                person = { name: "roc" }

                { person & name: "bird" }
                "#
            ),
            "{ name : Str }",
        )
    }

    #[ignore = "TODO: implement builtins in the editor"]
    #[test]
    fn constrain_run_low_level() {
        infer_eq(
            indoc!(
                r#"
                List.map [{ name: "roc" }, { name: "bird" }] .name
                "#
            ),
            "List Str",
        )
    }

    #[test]
    fn dual_arity_lambda() {
        infer_eq(
            indoc!(
                r#"
                    \a, b -> Pair a b
                "#
            ),
            "a, b -> [Pair a b]",
        );
    }

    #[test]
    fn anonymous_identity() {
        infer_eq(
            indoc!(
                r#"
                    (\a -> a) 3.14
                "#
            ),
            "Float *",
        );
    }

    #[test]
    fn identity_of_identity() {
        infer_eq(
            indoc!(
                r#"
                    (\val -> val) (\val -> val)
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn identity_function() {
        infer_eq(
            indoc!(
                r#"
                    \val -> val
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn apply_function() {
        infer_eq(
            indoc!(
                r#"
                    \f, x -> f x
                "#
            ),
            "(a -> b), a -> b",
        );
    }

    #[test]
    fn flip_function() {
        infer_eq(
            indoc!(
                r#"
                    \f -> (\a, b -> f b a)
                "#
            ),
            "(a, b -> d) -> (b, a -> d)",
        );
    }

    #[test]
    fn always_function() {
        infer_eq(
            indoc!(
                r#"
                    \val -> \_ -> val
                "#
            ),
            "a -> (* -> a)",
        );
    }

    #[test]
    fn pass_a_function() {
        infer_eq(
            indoc!(
                r#"
                    \f -> f {}
                "#
            ),
            "({} -> a) -> a",
        );
    }

    #[test]
    fn constrain_closure() {
        infer_eq(
            indoc!(
                r#"
                x = 1

                \{} -> x
                "#
            ),
            "{}* -> Num a",
        )
    }

    #[test]
    fn recursive_identity() {
        infer_eq(
            indoc!(
                r#"
                    identity = \val -> val

                    identity
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn use_apply() {
        infer_eq(
            indoc!(
                r#"
                identity = \a -> a
                apply = \f, x -> f x

                apply identity 5
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn nested_let_function() {
        infer_eq(
            indoc!(
                r#"
                    curryPair = \a ->
                        getB = \b -> Pair a b
                        getB

                    curryPair
                "#
            ),
            "a -> (b -> [Pair a b])",
        );
    }

    #[test]
    fn record_with_bound_var() {
        infer_eq(
            indoc!(
                r#"
                    fn = \rec ->
                        x = rec.x

                        rec

                    fn
                "#
            ),
            "{ x : a }b -> { x : a }b",
        );
    }

    #[test]
    fn using_type_signature() {
        infer_eq(
            indoc!(
                r#"
                    bar : custom -> custom
                    bar = \x -> x

                    bar
                "#
            ),
            "custom -> custom",
        );
    }

    #[ignore = "Currently panics at 'Invalid Cycle', ast/src/lang/core/def/def.rs:1212:21"]
    #[test]
    fn using_type_signature2() {
        infer_eq(
            indoc!(
                r#"
                    id1 : tya -> tya
                    id1 = \x -> x

                    id2 : tyb -> tyb
                    id2 = id1

                    id2
                "#
            ),
            "tyb -> tyb",
        );
    }

    #[ignore = "Implement annotation-only decls"]
    #[test]
    fn type_signature_without_body() {
        infer_eq(
            indoc!(
                r#"
                    foo: Str -> {}

                    foo "hi"
                "#
            ),
            "{}",
        );
    }

    #[ignore = "Implement annotation-only decls"]
    #[test]
    fn type_signature_without_body_rigid() {
        infer_eq(
            indoc!(
                r#"
                    foo : Num * -> custom

                    foo 2
                "#
            ),
            "custom",
        );
    }

    #[test]
    fn inference_var_inside_arrow() {
        infer_eq(
            indoc!(
                r#"
                id : _ -> _
                id = \x -> x
                id
                "#
            ),
            "a -> a",
        )
    }

    #[test]
    #[ignore = "TODO: Type2::substitute"]
    fn inference_var_inside_ctor() {
        infer_eq(
            indoc!(
                r#"
                canIGo : _ -> Result _ _
                canIGo = \color ->
                    when color is
                        "green" -> Ok "go!"
                        "yellow" -> Err (SlowIt "whoa, let's slow down!")
                        "red" -> Err (StopIt "absolutely not")
                        _ -> Err (UnknownColor "this is a weird stoplight")
                canIGo
                "#
            ),
            "Str -> Result Str [SlowIt Str, StopIt Str, UnknownColor Str]*",
        )
    }

    #[test]
    #[ignore = "TODO: Gives { x : *, y : * } -> { x : *, y : * }. This is a bug in typechecking defs with annotations."]
    fn inference_var_inside_ctor_linked() {
        infer_eq(
            indoc!(
                r#"
                swapRcd: {x: _, y: _} -> {x: _, y: _}
                swapRcd = \{x, y} -> {x: y, y: x}
                swapRcd
                "#
            ),
            "{ x : a, y : b } -> { x : b, y : a }",
        )
    }

    #[test]
    fn inference_var_link_with_rigid() {
        infer_eq(
            indoc!(
                r#"
                swapRcd: {x: tx, y: ty} -> {x: _, y: _}
                swapRcd = \{x, y} -> {x: y, y: x}
                swapRcd
                "#
            ),
            "{ x : tx, y : ty } -> { x : ty, y : tx }",
        )
    }

    #[test]
    #[ignore = "TODO: Type2::substitute"]
    fn inference_var_inside_tag_ctor() {
        infer_eq(
            indoc!(
                r#"
                badComics: Bool -> [CowTools _, Thagomizer _]
                badComics = \c ->
                    when c is
                        True -> CowTools "The Far Side"
                        False ->  Thagomizer "The Far Side"
                badComics
                "#
            ),
            "Bool -> [CowTools Str, Thagomizer Str]",
        )
    }

    #[test]
    fn inference_var_tag_union_ext() {
        // TODO: we should really be inferring [Blue, Orange]a -> [Lavender, Peach]a here.
        // See https://github.com/roc-lang/roc/issues/2053
        infer_eq(
            indoc!(
                r#"
                pastelize: _ -> [Lavender, Peach]_
                pastelize = \color ->
                    when color is
                        Blue -> Lavender
                        Orange -> Peach
                        col -> col
                pastelize
                "#
            ),
            "[Blue, Lavender, Orange, Peach]a -> [Blue, Lavender, Orange, Peach]a",
        )
    }

    #[test]
    #[ignore = "TODO: gives { email : a, name : b }c -> { email : a, name : b }c. This is a bug in typechecking defs with annotations."]
    fn inference_var_rcd_union_ext() {
        infer_eq(
            indoc!(
                r#"
                setRocEmail : _ -> { name: Str, email: Str }_
                setRocEmail = \person ->
                    { person & email: "\(person.name)@roclang.com" }
                setRocEmail
                "#
            ),
            "{ email : Str, name : Str }a -> { email : Str, name : Str }a",
        )
    }

    #[test]
    fn infer_union_input_position1() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A -> X
                       B -> Y
                 "#
            ),
            "[A, B] -> [X, Y]",
        )
    }

    #[test]
    fn infer_union_input_position2() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A -> X
                       B -> Y
                       _ -> Z
                 "#
            ),
            "[A, B]* -> [X, Y, Z]",
        )
    }

    #[test]
    fn infer_union_input_position3() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A M -> X
                       A N -> Y
                 "#
            ),
            "[A [M, N]] -> [X, Y]",
        )
    }

    #[test]
    fn infer_union_input_position4() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A M -> X
                       A N -> Y
                       A _ -> Z
                 "#
            ),
            "[A [M, N]] -> [X, Y, Z]",
        )
    }

    #[test]
    #[ignore = "TODO: currently [A [M [J]*, N [K]*]] -> [X]*"]
    fn infer_union_input_position5() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A (M J) -> X
                       A (N K) -> X
                 "#
            ),
            "[A [M [J], N [K]]] -> [X]*",
        )
    }

    #[test]
    fn infer_union_input_position6() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A M -> X
                       B   -> X
                       A N -> X
                 "#
            ),
            "[A [M, N], B] -> [X]",
        )
    }

    #[test]
    #[ignore = "TODO: currently [A]* -> [A, X]*"]
    fn infer_union_input_position7() {
        infer_eq(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                         A -> X
                         t -> t
                 "#
            ),
            // TODO: we could be a bit smarter by subtracting "A" as a possible
            // tag in the union known by t, which would yield the principal type
            // [A,]a -> [X]a
            "[A, X]a -> [A, X]a",
        )
    }

    #[test]
    fn infer_union_input_position8() {
        infer_eq(
            indoc!(
                r#"
                 \opt ->
                     when opt is
                         Some ({tag: A}) -> 1
                         Some ({tag: B}) -> 1
                         None -> 0
                 "#
            ),
            "[None, Some { tag : [A, B] }*] -> Num *",
        )
    }

    #[test]
    #[ignore = "TODO: panicked at 'Invalid Cycle', ast/src/lang/core/def/def.rs:1208:21"]
    fn infer_union_input_position9() {
        infer_eq(
            indoc!(
                r#"
                 opt : [Some Str, None]
                 opt = Some ""
                 rcd = { opt }

                 when rcd is
                     { opt: Some s } -> s
                     { opt: None } -> "?"
                 "#
            ),
            "Str",
        )
    }

    #[test]
    #[ignore = "TODO: currently <type mismatch> -> Num a"]
    fn infer_union_input_position10() {
        infer_eq(
            indoc!(
                r#"
                 \r ->
                     when r is
                         { x: Blue, y ? 3 } -> y
                         { x: Red, y ? 5 } -> y
                 "#
            ),
            "{ x : [Blue, Red], y ? Num a }* -> Num a",
        )
    }
}
