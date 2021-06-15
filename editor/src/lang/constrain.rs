use bumpalo::{collections::Vec as BumpVec, Bump};

use crate::lang::{
    ast::{Expr2, RecordField, WhenBranch},
    expr::Env,
    pattern::{DestructType, Pattern2, PatternState2, RecordDestruct},
    pool::{Pool, PoolStr, PoolVec, ShallowClone},
    types::{Type2, TypeId},
};

use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{BumpMap, BumpMapDefault, Index};
use roc_module::{ident::TagName, symbol::Symbol};
use roc_region::all::Region;
use roc_types::{
    subs::Variable,
    types::{self, AnnotationSource, PReason, PatternCategory},
    types::{Category, Reason},
};

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
        Expr2::Str(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::SmallStr(_) => Eq(str_type(env.pool), expected, Category::Str, region),
        Expr2::Blank => True,
        Expr2::EmptyRecord => constrain_empty_record(expected, region),
        Expr2::Var(symbol) => Lookup(*symbol, expected, region),
        Expr2::SmallInt { var, .. } => {
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

            and_constraints.push(Eq(num_type, expected, Category::Float, region));

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

                for (index, elem_node_id) in elems.iter_node_ids().enumerate() {
                    let elem_expr = env.pool.get(elem_node_id);

                    let elem_expected = Expected::ForReason(
                        Reason::ElemInList {
                            index: Index::zero_based(index),
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
        Expr2::GlobalTag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            let mut flex_vars = BumpVec::with_capacity_in(arguments.len(), arena);
            let types = PoolVec::with_capacity(arguments.len() as u32, env.pool);
            let mut arg_cons = BumpVec::with_capacity_in(arguments.len(), arena);

            for (argument_node_id, type_node_id) in
                arguments.iter_node_ids().zip(types.iter_node_ids())
            {
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
                    PoolVec::new(std::iter::once((*name, types)), env.pool),
                    env.pool.add(Type2::Variable(*ext_var)),
                ),
                expected.shallow_clone(),
                Category::TagApply {
                    tag_name: TagName::Global(name.as_str(env.pool).into()),
                    args_count: arguments.len(),
                },
                region,
            );

            let ast_con = Eq(
                Type2::Variable(*variant_var),
                expected,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            flex_vars.push(*variant_var);
            flex_vars.push(*ext_var);

            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(arena, flex_vars, And(arg_cons))
        }
        Expr2::Call {
            args,
            expr_var,
            expr: expr_node_id,
            closure_var,
            fn_var,
            ..
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
                    arg_index: Index::zero_based(index),
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

            let category = Category::CallResult(opt_symbol);

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

            let category = Category::Accessor(field.as_str(env.pool).into());

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

            let category = Category::Access(field.as_str(env.pool).into());

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
                Expected::FromAnnotation(name, arity, _, tipe) => {
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
                                    index: Index::zero_based(index),
                                    num_branches,
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
                                index: Index::zero_based(branches.len()),
                                num_branches,
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
                                    index: Index::zero_based(index),
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
                                index: Index::zero_based(branches.len()),
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
                Expected::FromAnnotation(name, arity, _, _typ) => {
                    // NOTE deviation from elm.
                    //
                    // in elm, `_typ` is used, but because we have this `expr_var` too
                    // and need to constrain it, this is what works and gives better error messages
                    let typ = Type2::Variable(*expr_var);

                    for (index, when_branch_id) in branches.iter_node_ids().enumerate() {
                        let when_branch = env.pool.get(when_branch_id);

                        let pattern_region = region;
                        // let pattern_region = Region::across_all(
                        //     when_branch.patterns.iter(env.pool).map(|v| &v.region),
                        // );

                        let branch_con = constrain_when_branch(
                            arena,
                            env,
                            // TODO: when_branch.value.region,
                            region,
                            when_branch,
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: Index::zero_based(index),
                                },
                                cond_type.shallow_clone(),
                                pattern_region,
                            ),
                            Expected::FromAnnotation(
                                name.clone(),
                                *arity,
                                AnnotationSource::TypedWhenBranch {
                                    index: Index::zero_based(index),
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

                        let pattern_region = region;
                        // let pattern_region =
                        //     Region::across_all(when_branch.patterns.iter().map(|v| &v.region));

                        let branch_con = constrain_when_branch(
                            arena,
                            env,
                            region,
                            when_branch,
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: Index::zero_based(index),
                                },
                                cond_type.shallow_clone(),
                                pattern_region,
                            ),
                            Expected::ForReason(
                                Reason::WhenBranch {
                                    index: Index::zero_based(index),
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
        _ => todo!("implement constraints for {:?}", expr),
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

fn constrain_empty_record<'a>(expected: Expected<Type2>, region: Region) -> Constraint<'a> {
    Constraint::Eq(Type2::EmptyRec, expected, Category::Record, region)
}

#[inline(always)]
fn constrain_when_branch<'a>(
    arena: &'a Bump,
    env: &mut Env,
    region: Region,
    when_branch: &WhenBranch,
    pattern_expected: PExpected<Type2>,
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
    for pattern_id in when_branch.patterns.iter_node_ids() {
        let pattern = env.pool.get(pattern_id);

        constrain_pattern(
            arena,
            env,
            pattern,
            // loc_pattern.region,
            region,
            pattern_expected.shallow_clone(),
            &mut state,
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

/// This accepts PatternState (rather than returning it) so that the caller can
/// intiialize the Vecs in PatternState using with_capacity
/// based on its knowledge of their lengths.
pub fn constrain_pattern<'a>(
    arena: &'a Bump,
    env: &mut Env,
    pattern: &Pattern2,
    region: Region,
    expected: PExpected<Type2>,
    state: &mut PatternState2<'a>,
) {
    use Pattern2::*;

    match pattern {
        Underscore | UnsupportedPattern(_) | MalformedPattern(_, _) | Shadowed { .. } => {
            // Neither the _ pattern nor erroneous ones add any constraints.
        }

        Identifier(symbol) => {
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

                if !state.headers.contains_key(&symbol) {
                    state.headers.insert(*symbol, pat_type.shallow_clone());
                }

                let destruct_type = env.pool.get(*typ);

                let field_type = match destruct_type {
                    DestructType::Guard(guard_var, guard_id) => {
                        state.constraints.push(Constraint::Pattern(
                            region,
                            PatternCategory::PatternGuard,
                            Type2::Variable(*guard_var),
                            PExpected::ForReason(
                                PReason::PatternGuard,
                                pat_type.shallow_clone(),
                                // TODO: region should be from guard_id
                                region,
                            ),
                        ));

                        state.vars.push(*guard_var);

                        let guard = env.pool.get(*guard_id);

                        // TODO: region should be from guard_id
                        constrain_pattern(arena, env, guard, region, expected, state);

                        types::RecordField::Demanded(env.pool.add(pat_type))
                    }
                    DestructType::Optional(expr_var, expr_id) => {
                        state.constraints.push(Constraint::Pattern(
                            region,
                            PatternCategory::PatternDefault,
                            Type2::Variable(*expr_var),
                            PExpected::ForReason(
                                PReason::OptionalField,
                                pat_type.shallow_clone(),
                                // TODO: region should be from expr_id
                                region,
                            ),
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

            let record_con = Constraint::Pattern(
                region,
                PatternCategory::Record,
                Type2::Variable(*whole_var),
                expected,
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }
        GlobalTag {
            whole_var,
            ext_var,
            tag_name,
            arguments,
        } => {
            let mut argument_types = Vec::with_capacity(arguments.len());

            for (index, arg_id) in arguments.iter_node_ids().enumerate() {
                let (pattern_var, pattern) = env.pool.get(arg_id);

                state.vars.push(*pattern_var);

                let pattern_type = Type2::Variable(*pattern_var);
                argument_types.push(pattern_type.shallow_clone());

                let expected = PExpected::ForReason(
                    PReason::TagArg {
                        tag_name: TagName::Global(tag_name.as_str(env.pool).into()),
                        index: Index::zero_based(index),
                    },
                    pattern_type,
                    region,
                );

                // TODO region should come from pattern
                constrain_pattern(arena, env, pattern, region, expected, state);
            }

            let whole_con = Constraint::Eq(
                Type2::Variable(*whole_var),
                Expected::NoExpectation(Type2::TagUnion(
                    PoolVec::new(
                        vec![(
                            *tag_name,
                            PoolVec::new(argument_types.into_iter(), env.pool),
                        )]
                        .into_iter(),
                        env.pool,
                    ),
                    env.pool.add(Type2::Variable(*ext_var)),
                )),
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let tag_con = Constraint::Pattern(
                region,
                PatternCategory::Ctor(TagName::Global(tag_name.as_str(env.pool).into())),
                Type2::Variable(*whole_var),
                expected,
            );

            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            state.constraints.push(whole_con);
            state.constraints.push(tag_con);
        }
        PrivateTag { .. } => todo!(),
    }
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
        Symbol::NUM_FLOAT,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        num_num_id,
    )
}

#[inline(always)]
fn num_floatingpoint(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_FLOATINGPOINT)
                PoolStr::new("Num.@FloatingPoint", pool),
                PoolVec::new(vec![range_type.shallow_clone()].into_iter(), pool),
            )]
            .into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_FLOATINGPOINT,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
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
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        num_num_id,
    )
}

#[inline(always)]
fn _num_signed64(pool: &mut Pool) -> Type2 {
    let alias_content = Type2::TagUnion(
        PoolVec::new(
            // TagName::Private(Symbol::NUM_AT_SIGNED64)
            vec![(PoolStr::new("Num.@Signed64", pool), PoolVec::empty(pool))].into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_SIGNED64,
        PoolVec::empty(pool),
        pool.add(alias_content),
    )
}

#[inline(always)]
fn _num_integer(pool: &mut Pool, range: TypeId) -> Type2 {
    let range_type = pool.get(range);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_INTEGER)
                PoolStr::new("Num.@Integer", pool),
                PoolVec::new(vec![range_type.shallow_clone()].into_iter(), pool),
            )]
            .into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_INTEGER,
        PoolVec::new(vec![(PoolStr::new("range", pool), range)].into_iter(), pool),
        pool.add(alias_content),
    )
}

#[inline(always)]
fn num_num(pool: &mut Pool, type_id: TypeId) -> Type2 {
    let range_type = pool.get(type_id);

    let alias_content = Type2::TagUnion(
        PoolVec::new(
            vec![(
                // TagName::Private(Symbol::NUM_AT_NUM)
                PoolStr::new("Num.@Num", pool),
                PoolVec::new(vec![range_type.shallow_clone()].into_iter(), pool),
            )]
            .into_iter(),
            pool,
        ),
        pool.add(Type2::EmptyTagUnion),
    );

    Type2::Alias(
        Symbol::NUM_NUM,
        PoolVec::new(
            vec![(PoolStr::new("range", pool), type_id)].into_iter(),
            pool,
        ),
        pool.add(alias_content),
    )
}
