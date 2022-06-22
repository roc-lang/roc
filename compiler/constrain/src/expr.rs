use crate::builtins::{
    empty_list_type, float_literal, int_literal, list_type, num_literal, num_u32, str_type,
};
use crate::pattern::{constrain_pattern, PatternState};
use roc_can::annotation::IntroducedVariables;
use roc_can::constraint::{Constraint, Constraints, OpportunisticResolve};
use roc_can::def::{Declaration, Def, Expects};
use roc_can::exhaustive::{sketch_pattern_to_rows, sketch_when_branches, ExhaustiveContext};
use roc_can::expected::Expected::{self, *};
use roc_can::expected::PExpected;
use roc_can::expr::Expr::{self, *};
use roc_can::expr::{AccessorData, AnnotatedMark, ClosureData, Field, WhenBranch};
use roc_can::pattern::Pattern;
use roc_can::traverse::symbols_introduced_from_pattern;
use roc_collections::all::{HumanIndex, MutMap, SendMap};
use roc_module::ident::Lowercase;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::{IllegalCycleMark, Variable};
use roc_types::types::Type::{self, *};
use roc_types::types::{
    AliasKind, AnnotationSource, Category, OptAbleType, PReason, Reason, RecordField, TypeExtension,
};

/// This is for constraining Defs
#[derive(Default, Debug)]
pub struct Info {
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub def_types: SendMap<Symbol, Loc<Type>>,
}

impl Info {
    pub fn with_capacity(capacity: usize) -> Self {
        Info {
            vars: Vec::with_capacity(capacity),
            constraints: Vec::with_capacity(capacity),
            def_types: SendMap::default(),
        }
    }
}

pub struct Env {
    /// for example `a` in the annotation `identity : a -> a`, we add it to this
    /// map so that expressions within that annotation can share these vars.
    pub rigids: MutMap<Lowercase, Variable>,
    pub resolutions_to_make: Vec<OpportunisticResolve>,
    pub home: ModuleId,
}

fn constrain_untyped_args(
    constraints: &mut Constraints,
    env: &mut Env,
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    closure_type: Type,
    return_type: Type,
) -> (Vec<Variable>, PatternState, Type) {
    let mut vars = Vec::with_capacity(arguments.len());
    let mut pattern_types = Vec::with_capacity(arguments.len());

    let mut pattern_state = PatternState::default();

    for (pattern_var, annotated_mark, loc_pattern) in arguments {
        // Untyped args don't need exhaustiveness checking because they are the source of truth!
        let _ = annotated_mark;

        let pattern_type = Type::Variable(*pattern_var);
        let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

        pattern_types.push(pattern_type);

        constrain_pattern(
            constraints,
            env,
            &loc_pattern.value,
            loc_pattern.region,
            pattern_expected,
            &mut pattern_state,
        );

        vars.push(*pattern_var);
    }

    let function_type =
        Type::Function(pattern_types, Box::new(closure_type), Box::new(return_type));

    (vars, pattern_state, function_type)
}

pub fn constrain_expr(
    constraints: &mut Constraints,
    env: &mut Env,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Constraint {
    match expr {
        &Int(var, precision, _, _, bound) => {
            int_literal(constraints, var, precision, expected, region, bound)
        }
        &Num(var, _, _, bound) => num_literal(constraints, var, expected, region, bound),
        &Float(var, precision, _, _, bound) => {
            float_literal(constraints, var, precision, expected, region, bound)
        }
        EmptyRecord => constrain_empty_record(constraints, region, expected),
        Expr::Record { record_var, fields } => {
            if fields.is_empty() {
                constrain_empty_record(constraints, region, expected)
            } else {
                let mut field_types = SendMap::default();
                let mut field_vars = Vec::with_capacity(fields.len());

                // Constraints need capacity for each field
                // + 1 for the record itself + 1 for record var
                let mut rec_constraints = Vec::with_capacity(2 + fields.len());

                for (label, field) in fields {
                    let field_var = field.var;
                    let loc_field_expr = &field.loc_expr;
                    let (field_type, field_con) =
                        constrain_field(constraints, env, field_var, &*loc_field_expr);

                    field_vars.push(field_var);
                    field_types.insert(label.clone(), RecordField::Required(field_type));

                    rec_constraints.push(field_con);
                }

                let record_type = Type::Record(field_types, TypeExtension::Closed);

                let record_con = constraints.equal_types_with_storage(
                    record_type,
                    expected,
                    Category::Record,
                    region,
                    *record_var,
                );

                rec_constraints.push(record_con);
                field_vars.push(*record_var);

                let and_constraint = constraints.and_constraint(rec_constraints);
                constraints.exists(field_vars, and_constraint)
            }
        }
        Update {
            record_var,
            ext_var,
            symbol,
            updates,
        } => {
            let mut fields: SendMap<Lowercase, RecordField<Type>> = SendMap::default();
            let mut vars = Vec::with_capacity(updates.len() + 2);
            let mut cons = Vec::with_capacity(updates.len() + 1);
            for (field_name, Field { var, loc_expr, .. }) in updates.clone() {
                let (var, tipe, con) = constrain_field_update(
                    constraints,
                    env,
                    var,
                    loc_expr.region,
                    field_name.clone(),
                    &loc_expr,
                );
                fields.insert(field_name, RecordField::Required(tipe));
                vars.push(var);
                cons.push(con);
            }

            let fields_type =
                Type::Record(fields, TypeExtension::from_type(Type::Variable(*ext_var)));
            let record_type = Type::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = constraints.equal_types_var(
                *record_var,
                NoExpectation(fields_type),
                Category::Record,
                region,
            );
            let record_con =
                constraints.equal_types_var(*record_var, expected, Category::Record, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let con = constraints.lookup(
                *symbol,
                ForReason(
                    Reason::RecordUpdateKeys(
                        *symbol,
                        updates
                            .iter()
                            .map(|(key, field)| (key.clone(), field.region))
                            .collect(),
                    ),
                    record_type,
                    region,
                ),
                region,
            );

            // ensure constraints are solved in this order, gives better errors
            cons.insert(0, fields_con);
            cons.insert(1, con);
            cons.insert(2, record_con);

            let and_constraint = constraints.and_constraint(cons);
            constraints.exists(vars, and_constraint)
        }
        Str(_) => constraints.equal_types(str_type(), expected, Category::Str, region),
        SingleQuote(_) => constraints.equal_types(num_u32(), expected, Category::Character, region),
        List {
            elem_var,
            loc_elems,
        } => {
            if loc_elems.is_empty() {
                let eq = constraints.equal_types(
                    empty_list_type(*elem_var),
                    expected,
                    Category::List,
                    region,
                );
                constraints.exists(vec![*elem_var], eq)
            } else {
                let list_elem_type = Type::Variable(*elem_var);
                let mut list_constraints = Vec::with_capacity(1 + loc_elems.len());

                for (index, loc_elem) in loc_elems.iter().enumerate() {
                    let elem_expected = ForReason(
                        Reason::ElemInList {
                            index: HumanIndex::zero_based(index),
                        },
                        list_elem_type.clone(),
                        loc_elem.region,
                    );
                    let constraint = constrain_expr(
                        constraints,
                        env,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    list_constraints.push(constraint);
                }

                list_constraints.push(constraints.equal_types(
                    list_type(list_elem_type),
                    expected,
                    Category::List,
                    region,
                ));

                let and_constraint = constraints.and_constraint(list_constraints);
                constraints.exists([*elem_var], and_constraint)
            }
        }
        Call(boxed, loc_args, called_via) => {
            let (fn_var, loc_fn, closure_var, ret_var) = &**boxed;
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let opt_symbol = if let Var(symbol) | AbilityMember(symbol, _, _) = loc_fn.value {
                Some(symbol)
            } else {
                None
            };

            let fn_type = Variable(*fn_var);
            let fn_region = loc_fn.region;
            let fn_expected = NoExpectation(fn_type);

            let fn_reason = Reason::FnCall {
                name: opt_symbol,
                arity: loc_args.len() as u8,
            };

            let fn_con =
                constrain_expr(constraints, env, loc_fn.region, &loc_fn.value, fn_expected);

            // The function's return type
            let ret_type = Variable(*ret_var);

            // type of values captured in the closure
            let closure_type = Variable(*closure_var);

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(*fn_var);
            vars.push(*ret_var);
            vars.push(*closure_var);

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_type = Variable(*arg_var);

                let reason = Reason::FnArg {
                    name: opt_symbol,
                    arg_index: HumanIndex::zero_based(index),
                };
                let expected_arg = ForReason(reason, arg_type.clone(), region);
                let arg_con = constrain_expr(
                    constraints,
                    env,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = ForReason(
                fn_reason,
                Function(arg_types, Box::new(closure_type), Box::new(ret_type)),
                region,
            );

            let category = Category::CallResult(opt_symbol, *called_via);

            let and_cons = [
                fn_con,
                constraints.equal_types_var(*fn_var, expected_fn_type, category.clone(), fn_region),
                constraints.and_constraint(arg_cons),
                constraints.equal_types_var(*ret_var, expected, category, region),
            ];

            let and_constraint = constraints.and_constraint(and_cons);
            constraints.exists(vars, and_constraint)
        }
        Var(symbol) => {
            // make lookup constraint to lookup this symbol's type in the environment
            constraints.lookup(*symbol, expected, region)
        }
        &AbilityMember(symbol, specialization_id, specialization_var) => {
            // make lookup constraint to lookup this symbol's type in the environment
            let store_expected = constraints.equal_types_var(
                specialization_var,
                expected,
                Category::Storage(file!(), line!()),
                region,
            );
            let lookup_constr = constraints.lookup(
                symbol,
                Expected::NoExpectation(Type::Variable(specialization_var)),
                region,
            );

            // Make sure we attempt to resolve the specialization.
            env.resolutions_to_make.push(OpportunisticResolve {
                specialization_variable: specialization_var,
                specialization_expectation: constraints.push_expected_type(
                    Expected::NoExpectation(Type::Variable(specialization_var)),
                ),
                member: symbol,
                specialization_id,
            });

            constraints.and_constraint([store_expected, lookup_constr])
        }
        Closure(ClosureData {
            function_type: fn_var,
            closure_type: closure_var,
            return_type: ret_var,
            arguments,
            loc_body: boxed,
            captured_symbols,
            name,
            ..
        }) => {
            // NOTE defs are treated somewhere else!
            let loc_body_expr = &**boxed;

            let ret_var = *ret_var;
            let closure_var = *closure_var;

            let closure_type = Type::Variable(closure_var);
            let return_type = Type::Variable(ret_var);
            let (mut vars, pattern_state, function_type) = constrain_untyped_args(
                constraints,
                env,
                arguments,
                closure_type,
                return_type.clone(),
            );

            vars.push(ret_var);
            vars.push(closure_var);
            vars.push(*fn_var);

            let body_type = NoExpectation(return_type);
            let ret_constraint = constrain_expr(
                constraints,
                env,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

            // make sure the captured symbols are sorted!
            debug_assert_eq!(captured_symbols.clone(), {
                let mut copy = captured_symbols.clone();
                copy.sort();
                copy
            });

            let closure_constraint = constrain_closure_size(
                constraints,
                *name,
                region,
                captured_symbols,
                closure_var,
                &mut vars,
            );

            let pattern_state_constraints = constraints.and_constraint(pattern_state.constraints);
            let cons = [
                constraints.let_constraint(
                    [],
                    pattern_state.vars,
                    pattern_state.headers,
                    pattern_state_constraints,
                    ret_constraint,
                ),
                constraints.equal_types_with_storage(
                    function_type,
                    expected,
                    Category::Lambda,
                    region,
                    *fn_var,
                ),
                closure_constraint,
            ];

            constraints.exists_many(vars, cons)
        }

        Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => {
            let expect_bool = |region| {
                let bool_type = Type::Variable(Variable::BOOL);
                Expected::ForReason(Reason::ExpectCondition, bool_type, region)
            };

            let cond_con = constrain_expr(
                constraints,
                env,
                loc_condition.region,
                &loc_condition.value,
                expect_bool(loc_condition.region),
            );

            let continuation_con = constrain_expr(
                constraints,
                env,
                loc_continuation.region,
                &loc_continuation.value,
                expected,
            );

            // + 2 for cond_con and continuation_con
            let mut all_constraints = Vec::with_capacity(lookups_in_cond.len() + 2);

            all_constraints.push(cond_con);
            all_constraints.push(continuation_con);

            let mut vars = Vec::with_capacity(lookups_in_cond.len());

            for (symbol, var) in lookups_in_cond.iter() {
                vars.push(*var);

                all_constraints.push(constraints.lookup(
                    *symbol,
                    NoExpectation(Type::Variable(*var)),
                    Region::zero(),
                ));
            }

            constraints.exists_many(vars, all_constraints)
        }

        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let expect_bool = |region| {
                let bool_type = Type::Variable(Variable::BOOL);
                Expected::ForReason(Reason::IfCondition, bool_type, region)
            };
            let mut branch_cons = Vec::with_capacity(2 * branches.len() + 3);

            // TODO why does this cond var exist? is it for error messages?
            let first_cond_region = branches[0].0.region;
            let cond_var_is_bool_con = constraints.equal_types_var(
                *cond_var,
                expect_bool(first_cond_region),
                Category::If,
                first_cond_region,
            );

            branch_cons.push(cond_var_is_bool_con);

            match expected {
                FromAnnotation(name, arity, ann_source, tipe) => {
                    let num_branches = branches.len() + 1;
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_con = constrain_expr(
                            constraints,
                            env,
                            loc_cond.region,
                            &loc_cond.value,
                            expect_bool(loc_cond.region),
                        );

                        let then_con = constrain_expr(
                            constraints,
                            env,
                            loc_body.region,
                            &loc_body.value,
                            FromAnnotation(
                                name.clone(),
                                arity,
                                AnnotationSource::TypedIfBranch {
                                    index: HumanIndex::zero_based(index),
                                    num_branches,
                                    region: ann_source.region(),
                                },
                                tipe.clone(),
                            ),
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }

                    let else_con = constrain_expr(
                        constraints,
                        env,
                        final_else.region,
                        &final_else.value,
                        FromAnnotation(
                            name,
                            arity,
                            AnnotationSource::TypedIfBranch {
                                index: HumanIndex::zero_based(branches.len()),
                                num_branches,
                                region: ann_source.region(),
                            },
                            tipe.clone(),
                        ),
                    );

                    let ast_con = constraints.equal_types_var(
                        *branch_var,
                        NoExpectation(tipe),
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    );

                    branch_cons.push(ast_con);
                    branch_cons.push(else_con);

                    constraints.exists_many([*cond_var, *branch_var], branch_cons)
                }
                _ => {
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_con = constrain_expr(
                            constraints,
                            env,
                            loc_cond.region,
                            &loc_cond.value,
                            expect_bool(loc_cond.region),
                        );

                        let then_con = constrain_expr(
                            constraints,
                            env,
                            loc_body.region,
                            &loc_body.value,
                            ForReason(
                                Reason::IfBranch {
                                    index: HumanIndex::zero_based(index),
                                    total_branches: branches.len(),
                                },
                                Type::Variable(*branch_var),
                                loc_body.region,
                            ),
                        );

                        branch_cons.push(cond_con);
                        branch_cons.push(then_con);
                    }
                    let else_con = constrain_expr(
                        constraints,
                        env,
                        final_else.region,
                        &final_else.value,
                        ForReason(
                            Reason::IfBranch {
                                index: HumanIndex::zero_based(branches.len()),
                                total_branches: branches.len() + 1,
                            },
                            Type::Variable(*branch_var),
                            final_else.region,
                        ),
                    );

                    branch_cons.push(constraints.equal_types_var(
                        *branch_var,
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    ));
                    branch_cons.push(else_con);

                    constraints.exists_many([*cond_var, *branch_var], branch_cons)
                }
            }
        }
        When {
            cond_var: real_cond_var,
            expr_var,
            loc_cond,
            branches,
            branches_cond_var,
            exhaustive,
            ..
        } => {
            let branches_cond_var = *branches_cond_var;
            let branches_cond_type = Variable(branches_cond_var);

            let body_var = *expr_var;
            let body_type = Variable(body_var);

            let branches_region = {
                debug_assert!(!branches.is_empty());
                Region::span_across(
                    &loc_cond.region,
                    // &branches.first().unwrap().region(),
                    &branches.last().unwrap().pattern_region(),
                )
            };

            let branch_expr_reason =
                |expected: &Expected<Type>, index, branch_region| match expected {
                    FromAnnotation(name, arity, ann_source, _typ) => {
                        // NOTE deviation from elm.
                        //
                        // in elm, `_typ` is used, but because we have this `expr_var` too
                        // and need to constrain it, this is what works and gives better error messages
                        FromAnnotation(
                            name.clone(),
                            *arity,
                            AnnotationSource::TypedWhenBranch {
                                index,
                                region: ann_source.region(),
                            },
                            body_type.clone(),
                        )
                    }

                    _ => ForReason(
                        Reason::WhenBranch { index },
                        body_type.clone(),
                        branch_region,
                    ),
                };

            // Our goal is to constrain and introduce variables in all pattern when branch patterns before
            // looking at their bodies.
            //
            //   pat1 -> body1
            //   *^^^    +~~~~
            //   pat2 -> body2
            //   *^^^    +~~~~
            //
            //   * solve first
            //   + solve second
            //
            // For a single pattern/body pair, we must introduce variables and symbols defined in the
            // pattern before solving the body, since those definitions are effectively let-bound.
            //
            // But also, we'd like to solve all branch pattern constraints in one swoop before looking at
            // the bodies, because the patterns may have presence constraints that expect to be built up
            // together.
            //
            // For this reason, we distinguish the two - and introduce variables in the branch patterns
            // as part of the pattern constraint, solving all of those at once, and then solving the body
            // constraints.
            let mut pattern_vars = Vec::with_capacity(branches.len());
            let mut pattern_headers = SendMap::default();
            let mut pattern_cons = Vec::with_capacity(branches.len() + 2);
            let mut branch_cons = Vec::with_capacity(branches.len());

            for (index, when_branch) in branches.iter().enumerate() {
                let expected_pattern = |sub_pattern, sub_region| {
                    PExpected::ForReason(
                        PReason::WhenMatch {
                            index: HumanIndex::zero_based(index),
                            sub_pattern,
                        },
                        branches_cond_type.clone(),
                        sub_region,
                    )
                };

                let (new_pattern_vars, new_pattern_headers, pattern_con, branch_con) =
                    constrain_when_branch_help(
                        constraints,
                        env,
                        region,
                        when_branch,
                        expected_pattern,
                        branch_expr_reason(
                            &expected,
                            HumanIndex::zero_based(index),
                            when_branch.value.region,
                        ),
                    );

                pattern_vars.extend(new_pattern_vars);
                debug_assert!(
                    pattern_headers
                        .clone()
                        .intersection(new_pattern_headers.clone())
                        .is_empty(),
                    "Two patterns introduce the same symbols - that's a bug!\n{:?}",
                    pattern_headers.clone().intersection(new_pattern_headers)
                );
                pattern_headers.extend(new_pattern_headers);
                pattern_cons.push(pattern_con);

                branch_cons.push(branch_con);
            }

            // Deviation: elm adds another layer of And nesting
            //
            // Record the original conditional expression's constraint.
            // Each branch's pattern must have the same type
            // as the condition expression did.
            //
            // The return type of each branch must equal the return type of
            // the entire when-expression.

            // After solving the condition variable with what's expected from the branch patterns,
            // check it against the condition expression.
            //
            // First, solve the condition type.
            let real_cond_var = *real_cond_var;
            let real_cond_type = Type::Variable(real_cond_var);
            let cond_constraint = constrain_expr(
                constraints,
                env,
                loc_cond.region,
                &loc_cond.value,
                Expected::NoExpectation(real_cond_type),
            );
            pattern_cons.push(cond_constraint);

            // Now check the condition against the type expected by the branches.
            let sketched_rows = sketch_when_branches(real_cond_var, branches_region, branches);
            let cond_matches_branches_constraint = constraints.exhaustive(
                real_cond_var,
                loc_cond.region,
                Ok((
                    loc_cond.value.category(),
                    Expected::ForReason(Reason::WhenBranches, branches_cond_type, branches_region),
                )),
                sketched_rows,
                ExhaustiveContext::BadCase,
                *exhaustive,
            );
            pattern_cons.push(cond_matches_branches_constraint);

            // Solve all the pattern constraints together, introducing variables in the pattern as
            // need be before solving the bodies.
            let pattern_constraints = constraints.and_constraint(pattern_cons);
            let body_constraints = constraints.and_constraint(branch_cons);
            let when_body_con = constraints.let_constraint(
                [],
                pattern_vars,
                pattern_headers,
                pattern_constraints,
                body_constraints,
            );

            let result_con =
                constraints.equal_types_var(body_var, expected, Category::When, region);

            let total_cons = [when_body_con, result_con];
            let branch_constraints = constraints.and_constraint(total_cons);

            constraints.exists(
                [branches_cond_var, real_cond_var, *expr_var],
                branch_constraints,
            )
        }
        Access {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let ext_var = *ext_var;
            let ext_type = Type::Variable(ext_var);
            let field_var = *field_var;
            let field_type = Type::Variable(field_var);

            let mut rec_field_types = SendMap::default();

            let label = field.clone();
            rec_field_types.insert(label, RecordField::Demanded(field_type));

            let record_type = Type::Record(rec_field_types, TypeExtension::from_type(ext_type));
            let record_expected = Expected::NoExpectation(record_type);

            let category = Category::Access(field.clone());

            let record_con = constraints.equal_types_var(
                *record_var,
                record_expected.clone(),
                category.clone(),
                region,
            );

            let constraint =
                constrain_expr(constraints, env, region, &loc_expr.value, record_expected);

            let eq = constraints.equal_types_var(field_var, expected, category, region);
            constraints.exists_many(
                [*record_var, field_var, ext_var],
                [constraint, eq, record_con],
            )
        }
        Accessor(AccessorData {
            name: closure_name,
            function_var,
            field,
            record_var,
            closure_var,
            ext_var,
            field_var,
        }) => {
            let ext_var = *ext_var;
            let ext_type = Variable(ext_var);
            let field_var = *field_var;
            let field_type = Variable(field_var);

            let mut field_types = SendMap::default();
            let label = field.clone();
            field_types.insert(label, RecordField::Demanded(field_type.clone()));
            let record_type = Type::Record(field_types, TypeExtension::from_type(ext_type));

            let category = Category::Accessor(field.clone());

            let record_expected = Expected::NoExpectation(record_type.clone());
            let record_con =
                constraints.equal_types_var(*record_var, record_expected, category.clone(), region);

            let lambda_set = Type::ClosureTag {
                name: *closure_name,
                captures: vec![],
            };

            let closure_type = Type::Variable(*closure_var);

            let function_type = Type::Function(
                vec![record_type],
                Box::new(closure_type),
                Box::new(field_type),
            );

            let cons = [
                constraints.equal_types_var(
                    *closure_var,
                    NoExpectation(lambda_set),
                    category.clone(),
                    region,
                ),
                constraints.equal_types(function_type.clone(), expected, category.clone(), region),
                constraints.equal_types(
                    function_type,
                    NoExpectation(Variable(*function_var)),
                    category,
                    region,
                ),
                record_con,
            ];

            constraints.exists_many(
                [*record_var, *function_var, *closure_var, field_var, ext_var],
                cons,
            )
        }
        LetRec(defs, loc_ret, cycle_mark) => {
            let body_con = constrain_expr(
                constraints,
                env,
                loc_ret.region,
                &loc_ret.value,
                expected.clone(),
            );

            constrain_recursive_defs(constraints, env, defs, body_con, *cycle_mark)
        }
        LetNonRec(def, loc_ret) => {
            let mut stack = Vec::with_capacity(1);

            let mut loc_ret = loc_ret;

            stack.push(def);

            while let LetNonRec(def, new_loc_ret) = &loc_ret.value {
                stack.push(def);
                loc_ret = new_loc_ret;
            }

            let mut body_con = constrain_expr(
                constraints,
                env,
                loc_ret.region,
                &loc_ret.value,
                expected.clone(),
            );

            while let Some(def) = stack.pop() {
                body_con = constrain_def(constraints, env, def, body_con)
            }

            body_con
        }
        Tag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            // +2 because we push all the arguments, plus variant_var and ext_var
            let num_vars = arguments.len() + 2;
            let mut vars = Vec::with_capacity(num_vars);
            let mut types = Vec::with_capacity(arguments.len());
            let mut arg_cons = Vec::with_capacity(arguments.len());

            for (var, loc_expr) in arguments {
                let arg_con = constrain_expr(
                    constraints,
                    env,
                    loc_expr.region,
                    &loc_expr.value,
                    Expected::NoExpectation(Type::Variable(*var)),
                );

                arg_cons.push(arg_con);
                vars.push(*var);
                types.push(Type::Variable(*var));
            }

            let union_con = constraints.equal_types_with_storage(
                Type::TagUnion(
                    vec![(name.clone(), types)],
                    TypeExtension::from_type(Type::Variable(*ext_var)),
                ),
                expected.clone(),
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: arguments.len(),
                },
                region,
                *variant_var,
            );

            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);

            constraints.exists_many(vars, arg_cons)
        }
        ZeroArgumentTag {
            variant_var,
            ext_var,
            name,
            closure_name,
        } => {
            let union_con = constraints.equal_types_with_storage(
                Type::FunctionOrTagUnion(
                    name.clone(),
                    *closure_name,
                    TypeExtension::from_type(Type::Variable(*ext_var)),
                ),
                expected.clone(),
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: 0,
                },
                region,
                *variant_var,
            );

            constraints.exists_many([*variant_var, *ext_var], [union_con])
        }
        OpaqueRef {
            opaque_var,
            name,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => {
            let (arg_var, arg_loc_expr) = &**argument;
            let arg_type = Type::Variable(*arg_var);

            let opaque_type = Type::Alias {
                symbol: *name,
                type_arguments: type_arguments
                    .iter()
                    .map(|v| OptAbleType {
                        typ: Type::Variable(v.var),
                        opt_ability: v.opt_ability,
                    })
                    .collect(),
                lambda_set_variables: lambda_set_variables.clone(),
                actual: Box::new(arg_type.clone()),
                kind: AliasKind::Opaque,
            };

            // Constrain the argument
            let arg_con = constrain_expr(
                constraints,
                env,
                arg_loc_expr.region,
                &arg_loc_expr.value,
                Expected::NoExpectation(arg_type.clone()),
            );

            // Link the entire wrapped opaque type (with the now-constrained argument) to the
            // expected type
            let opaque_con = constraints.equal_types_with_storage(
                opaque_type,
                expected,
                Category::OpaqueWrap(*name),
                region,
                *opaque_var,
            );

            // Link the entire wrapped opaque type (with the now-constrained argument) to the type
            // variables of the opaque type
            // TODO: better expectation here
            let link_type_variables_con = constraints.equal_types(
                arg_type,
                Expected::NoExpectation((**specialized_def_type).clone()),
                Category::OpaqueArg,
                arg_loc_expr.region,
            );

            let mut vars = vec![*arg_var, *opaque_var];
            // Also add the fresh variables we created for the type argument and lambda sets
            vars.extend(type_arguments.iter().map(|v| v.var));
            vars.extend(lambda_set_variables.iter().map(|v| {
                v.0.expect_variable("all lambda sets should be fresh variables here")
            }));

            constraints.exists_many(vars, [arg_con, opaque_con, link_type_variables_con])
        }

        RunLowLevel { args, ret_var, op } => {
            // This is a modified version of what we do for function calls.

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            let mut add_arg = |index, arg_type: Type, arg| {
                let reason = Reason::LowLevelOpArg {
                    op: *op,
                    arg_index: HumanIndex::zero_based(index),
                };
                let expected_arg = ForReason(reason, arg_type.clone(), Region::zero());
                let arg_con = constrain_expr(constraints, env, Region::zero(), arg, expected_arg);

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            };

            for (index, (arg_var, arg)) in args.iter().enumerate() {
                vars.push(*arg_var);

                add_arg(index, Variable(*arg_var), arg);
            }

            let category = Category::LowLevelOpResult(*op);

            // Deviation: elm uses an additional And here
            let eq = constraints.equal_types_var(*ret_var, expected, category, region);
            arg_cons.push(eq);
            constraints.exists_many(vars, arg_cons)
        }
        ForeignCall {
            args,
            ret_var,
            foreign_symbol,
        } => {
            // This is a modified version of what we do for function calls.

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            let mut add_arg = |index, arg_type: Type, arg| {
                let reason = Reason::ForeignCallArg {
                    foreign_symbol: foreign_symbol.clone(),
                    arg_index: HumanIndex::zero_based(index),
                };
                let expected_arg = ForReason(reason, arg_type.clone(), Region::zero());
                let arg_con = constrain_expr(constraints, env, Region::zero(), arg, expected_arg);

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            };

            for (index, (arg_var, arg)) in args.iter().enumerate() {
                vars.push(*arg_var);

                add_arg(index, Variable(*arg_var), arg);
            }

            let category = Category::ForeignCall;

            // Deviation: elm uses an additional And here
            let eq = constraints.equal_types_var(*ret_var, expected, category, region);
            arg_cons.push(eq);
            constraints.exists_many(vars, arg_cons)
        }
        TypedHole(var) => {
            // store the expected type for this position
            constraints.equal_types_var(
                *var,
                expected,
                Category::Storage(std::file!(), std::line!()),
                region,
            )
        }
        RuntimeError(_) => {
            // Runtime Errors have no constraints because they're going to crash.
            Constraint::True
        }
    }
}

/// Constrain a when branch, returning (variables in pattern, symbols introduced in pattern, pattern constraint, body constraint).
/// We want to constraint all pattern constraints in a "when" before body constraints.
#[inline(always)]
fn constrain_when_branch_help(
    constraints: &mut Constraints,
    env: &mut Env,
    region: Region,
    when_branch: &WhenBranch,
    pattern_expected: impl Fn(HumanIndex, Region) -> PExpected<Type>,
    expr_expected: Expected<Type>,
) -> (
    Vec<Variable>,
    SendMap<Symbol, Loc<Type>>,
    Constraint,
    Constraint,
) {
    let ret_constraint = constrain_expr(
        constraints,
        env,
        region,
        &when_branch.value.value,
        expr_expected,
    );

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(2),
        constraints: Vec::with_capacity(2),
        delayed_is_open_constraints: Vec::new(),
    };

    // TODO investigate for error messages, is it better to unify all branches with a variable,
    // then unify that variable with the expectation?
    for (i, loc_pattern) in when_branch.patterns.iter().enumerate() {
        let pattern_expected = pattern_expected(HumanIndex::zero_based(i), loc_pattern.region);

        constrain_pattern(
            constraints,
            env,
            &loc_pattern.value,
            loc_pattern.region,
            pattern_expected,
            &mut state,
        );
    }

    let (pattern_constraints, body_constraints) = if let Some(loc_guard) = &when_branch.guard {
        let guard_constraint = constrain_expr(
            constraints,
            env,
            region,
            &loc_guard.value,
            Expected::ForReason(
                Reason::WhenGuard,
                Type::Variable(Variable::BOOL),
                loc_guard.region,
            ),
        );

        // must introduce the headers from the pattern before constraining the guard
        state
            .constraints
            .append(&mut state.delayed_is_open_constraints);
        let state_constraints = constraints.and_constraint(state.constraints);
        let inner = constraints.let_constraint([], [], [], guard_constraint, ret_constraint);

        (state_constraints, inner)
    } else {
        state
            .constraints
            .append(&mut state.delayed_is_open_constraints);
        let state_constraints = constraints.and_constraint(state.constraints);
        (state_constraints, ret_constraint)
    };

    (
        state.vars,
        state.headers,
        pattern_constraints,
        body_constraints,
    )
}

fn constrain_field(
    constraints: &mut Constraints,
    env: &mut Env,
    field_var: Variable,
    loc_expr: &Loc<Expr>,
) -> (Type, Constraint) {
    let field_type = Variable(field_var);
    let field_expected = NoExpectation(field_type.clone());
    let constraint = constrain_expr(
        constraints,
        env,
        loc_expr.region,
        &loc_expr.value,
        field_expected,
    );

    (field_type, constraint)
}

#[inline(always)]
fn constrain_empty_record(
    constraints: &mut Constraints,
    region: Region,
    expected: Expected<Type>,
) -> Constraint {
    constraints.equal_types(Type::EmptyRec, expected, Category::Record, region)
}

/// Constrain top-level module declarations
#[inline(always)]
pub fn constrain_decls(
    constraints: &mut Constraints,
    home: ModuleId,
    decls: &[Declaration],
) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;

    let mut env = Env {
        home,
        rigids: MutMap::default(),
        resolutions_to_make: vec![],
    };

    for decl in decls.iter().rev() {
        // Clear the rigids from the previous iteration.
        // rigids are not shared between top-level definitions
        env.rigids.clear();

        match decl {
            Declaration::Declare(def) | Declaration::Builtin(def) => {
                constraint = constrain_def(constraints, &mut env, def, constraint);
            }
            Declaration::DeclareRec(defs, cycle_mark) => {
                constraint =
                    constrain_recursive_defs(constraints, &mut env, defs, constraint, *cycle_mark);
            }
            Declaration::Expects(expects) => {
                constraint = constrain_expects(constraints, &mut env, expects, constraint);
            }
            Declaration::InvalidCycle(_) => {
                // invalid cycles give a canonicalization error. we skip them here.
                continue;
            }
        }

        debug_assert!(
            env.resolutions_to_make.is_empty(),
            "Resolutions not attached after def!"
        );
    }

    // this assert make the "root" of the constraint wasn't dropped
    debug_assert!(constraints.contains_save_the_environment(&constraint));

    constraint
}

pub fn constrain_def_pattern(
    constraints: &mut Constraints,
    env: &mut Env,
    loc_pattern: &Loc<Pattern>,
    expr_type: Type,
) -> PatternState {
    let pattern_expected = PExpected::NoExpectation(expr_type);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
        delayed_is_open_constraints: vec![],
    };

    constrain_pattern(
        constraints,
        env,
        &loc_pattern.value,
        loc_pattern.region,
        pattern_expected,
        &mut state,
    );

    state
}

/// Generate constraints for a definition with a type signature
fn constrain_typed_def(
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    body_con: Constraint,
    annotation: &roc_can::def::Annotation,
) -> Constraint {
    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut def_pattern_state =
        constrain_def_pattern(constraints, env, &def.loc_pattern, expr_type.clone());

    def_pattern_state.vars.push(expr_var);

    let arity = annotation.signature.arity();
    let rigids = &env.rigids;
    let mut ftv = rigids.clone();

    let InstantiateRigids {
        signature,
        new_rigid_variables,
        new_infer_variables,
    } = instantiate_rigids(
        &annotation.signature,
        &annotation.introduced_variables,
        &def.loc_pattern,
        &mut ftv,
        &mut def_pattern_state.headers,
    );

    let env = &mut Env {
        home: env.home,
        resolutions_to_make: vec![],
        rigids: ftv,
    };

    let annotation_expected = FromAnnotation(
        def.loc_pattern.clone(),
        arity,
        AnnotationSource::TypedBody {
            region: annotation.region,
        },
        signature.clone(),
    );

    def_pattern_state.constraints.push(constraints.equal_types(
        expr_type,
        annotation_expected,
        Category::Storage(std::file!(), std::line!()),
        Region::span_across(&annotation.region, &def.loc_expr.region),
    ));

    // when a def is annotated, and it's body is a closure, treat this
    // as a named function (in elm terms) for error messages.
    //
    // This means we get errors like "the first argument of `f` is weird"
    // instead of the more generic "something is wrong with the body of `f`"
    match (&def.loc_expr.value, &signature) {
        (
            Closure(ClosureData {
                function_type: fn_var,
                closure_type: closure_var,
                return_type: ret_var,
                captured_symbols,
                arguments,
                loc_body,
                name,
                ..
            }),
            Type::Function(arg_types, signature_closure_type, ret_type),
        ) => {
            // NOTE if we ever have problems with the closure, the ignored `_closure_type`
            // is probably a good place to start the investigation!

            let region = def.loc_expr.region;

            let loc_body_expr = &**loc_body;
            let mut argument_pattern_state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(arguments.len()),
                constraints: Vec::with_capacity(1),
                delayed_is_open_constraints: vec![],
            };
            let mut vars = Vec::with_capacity(argument_pattern_state.vars.capacity() + 1);
            let ret_var = *ret_var;
            let closure_var = *closure_var;
            let ret_type = *ret_type.clone();

            vars.push(ret_var);
            vars.push(closure_var);

            constrain_typed_function_arguments(
                constraints,
                env,
                def,
                &mut def_pattern_state,
                &mut argument_pattern_state,
                arguments,
                arg_types,
            );

            let closure_constraint = constrain_closure_size(
                constraints,
                *name,
                region,
                captured_symbols,
                closure_var,
                &mut vars,
            );

            let body_type = FromAnnotation(
                def.loc_pattern.clone(),
                arguments.len(),
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                ret_type.clone(),
            );

            let ret_constraint = constrain_expr(
                constraints,
                env,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );
            let ret_constraint = attach_resolution_constraints(constraints, env, ret_constraint);

            vars.push(*fn_var);
            let defs_constraint = constraints.and_constraint(argument_pattern_state.constraints);

            let signature_closure_type = *signature_closure_type.clone();
            let signature_index = constraints.push_type(signature);
            let cons = [
                constraints.let_constraint(
                    [],
                    argument_pattern_state.vars,
                    argument_pattern_state.headers,
                    defs_constraint,
                    ret_constraint,
                ),
                constraints.equal_types_var(
                    closure_var,
                    Expected::FromAnnotation(
                        def.loc_pattern.clone(),
                        arity,
                        AnnotationSource::TypedBody {
                            region: annotation.region,
                        },
                        signature_closure_type,
                    ),
                    Category::ClosureSize,
                    region,
                ),
                constraints.store_index(signature_index, *fn_var, std::file!(), std::line!()),
                constraints.store_index(signature_index, expr_var, std::file!(), std::line!()),
                constraints.store(ret_type, ret_var, std::file!(), std::line!()),
                closure_constraint,
            ];

            let expr_con = constraints.exists_many(vars, cons);

            constrain_def_make_constraint(
                constraints,
                new_rigid_variables.into_iter(),
                new_infer_variables.into_iter(),
                expr_con,
                body_con,
                def_pattern_state,
            )
        }

        _ => {
            let annotation_expected = FromAnnotation(
                def.loc_pattern.clone(),
                arity,
                AnnotationSource::TypedBody {
                    region: annotation.region,
                },
                signature.clone(),
            );

            let ret_constraint = constrain_expr(
                constraints,
                env,
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            );
            let ret_constraint = attach_resolution_constraints(constraints, env, ret_constraint);

            let cons = [
                ret_constraint,
                // Store type into AST vars. We use Store so errors aren't reported twice
                constraints.store(signature, expr_var, std::file!(), std::line!()),
            ];
            let expr_con = constraints.and_constraint(cons);

            constrain_def_make_constraint(
                constraints,
                new_rigid_variables.into_iter(),
                new_infer_variables.into_iter(),
                expr_con,
                body_con,
                def_pattern_state,
            )
        }
    }
}

fn constrain_typed_function_arguments(
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    def_pattern_state: &mut PatternState,
    argument_pattern_state: &mut PatternState,
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    arg_types: &[Type],
) {
    // ensure type matches the one in the annotation
    let opt_label = if let Pattern::Identifier(label) = def.loc_pattern.value {
        Some(label)
    } else {
        None
    };

    let it = arguments.iter().zip(arg_types.iter()).enumerate();
    for (index, ((pattern_var, annotated_mark, loc_pattern), ann)) in it {
        if loc_pattern.value.surely_exhaustive() {
            // OPT: we don't need to perform any type-level exhaustiveness checking.
            // Check instead only that the pattern unifies with the annotation type.
            let pattern_expected = PExpected::ForReason(
                PReason::TypedArg {
                    index: HumanIndex::zero_based(index),
                    opt_name: opt_label,
                },
                ann.clone(),
                loc_pattern.region,
            );

            constrain_pattern(
                constraints,
                env,
                &loc_pattern.value,
                loc_pattern.region,
                pattern_expected,
                argument_pattern_state,
            );

            {
                // NOTE: because we perform an equality with part of the signature
                // this constraint must be to the def_pattern_state's constraints
                def_pattern_state.vars.push(*pattern_var);

                let pattern_con = constraints.equal_types_var(
                    *pattern_var,
                    Expected::NoExpectation(ann.clone()),
                    Category::Storage(std::file!(), std::line!()),
                    loc_pattern.region,
                );

                def_pattern_state.constraints.push(pattern_con);
            }
        } else {
            // We need to check the types, and run exhaustiveness checking.
            let &AnnotatedMark {
                annotation_var,
                exhaustive,
            } = annotated_mark;

            def_pattern_state.vars.push(*pattern_var);
            def_pattern_state.vars.push(annotation_var);

            {
                // First, solve the type that the pattern is expecting to match in this
                // position.
                let pattern_expected = PExpected::NoExpectation(Type::Variable(*pattern_var));
                constrain_pattern(
                    constraints,
                    env,
                    &loc_pattern.value,
                    loc_pattern.region,
                    pattern_expected,
                    argument_pattern_state,
                );
            }

            {
                // Store the actual type in a variable.
                argument_pattern_state
                    .constraints
                    .push(constraints.equal_types_var(
                        annotation_var,
                        Expected::NoExpectation(ann.clone()),
                        Category::Storage(file!(), line!()),
                        Region::zero(),
                    ));
            }

            {
                // let pattern_expected = PExpected::ForReason(
                //     PReason::TypedArg {
                //         index: HumanIndex::zero_based(index),
                //         opt_name: opt_label,
                //     },
                //     ann.clone(),
                //     loc_pattern.region,
                // );

                // Exhaustiveness-check the type in the pattern against what the
                // annotation wants.
                let sketched_rows =
                    sketch_pattern_to_rows(annotation_var, loc_pattern.region, &loc_pattern.value);
                let category = loc_pattern.value.category();
                let expected = PExpected::ForReason(
                    PReason::TypedArg {
                        index: HumanIndex::zero_based(index),
                        opt_name: opt_label,
                    },
                    Type::Variable(*pattern_var),
                    loc_pattern.region,
                );
                let exhaustive_constraint = constraints.exhaustive(
                    annotation_var,
                    loc_pattern.region,
                    Err((category, expected)),
                    sketched_rows,
                    ExhaustiveContext::BadArg,
                    exhaustive,
                );
                argument_pattern_state
                    .constraints
                    .push(exhaustive_constraint)
            }
        }
    }
}

#[inline(always)]
fn attach_resolution_constraints(
    constraints: &mut Constraints,
    env: &mut Env,
    constraint: Constraint,
) -> Constraint {
    let resolution_constrs =
        constraints.and_constraint(env.resolutions_to_make.drain(..).map(Constraint::Resolve));
    constraints.and_constraint([constraint, resolution_constrs])
}

fn constrain_expects(
    constraints: &mut Constraints,
    env: &mut Env,
    expects: &Expects,
    body_con: Constraint,
) -> Constraint {
    let expect_bool = |region| {
        let bool_type = Type::Variable(Variable::BOOL);
        Expected::ForReason(Reason::ExpectCondition, bool_type, region)
    };

    let mut expect_constraints = Vec::with_capacity(expects.conditions.len());

    let it = expects.regions.iter().zip(expects.conditions.iter());
    for (region, condition) in it {
        let expected = expect_bool(*region);
        expect_constraints.push(constrain_expr(
            constraints,
            env,
            *region,
            condition,
            expected,
        ));
    }

    let defs_constraint = constraints.and_constraint(expect_constraints);
    constraints.let_constraint([], [], [], defs_constraint, body_con)
}

fn constrain_def(
    constraints: &mut Constraints,
    env: &mut Env,
    def: &Def,
    body_con: Constraint,
) -> Constraint {
    match &def.annotation {
        Some(annotation) => constrain_typed_def(constraints, env, def, body_con, annotation),
        None => {
            let expr_var = def.expr_var;
            let expr_type = Type::Variable(expr_var);

            let mut def_pattern_state =
                constrain_def_pattern(constraints, env, &def.loc_pattern, expr_type.clone());

            def_pattern_state.vars.push(expr_var);
            // no annotation, so no extra work with rigids

            let expr_con = constrain_expr(
                constraints,
                env,
                def.loc_expr.region,
                &def.loc_expr.value,
                NoExpectation(expr_type),
            );
            let expr_con = attach_resolution_constraints(constraints, env, expr_con);

            constrain_def_make_constraint(
                constraints,
                std::iter::empty(),
                std::iter::empty(),
                expr_con,
                body_con,
                def_pattern_state,
            )
        }
    }
}

/// Create a let-constraint for a non-recursive def.
/// Recursive defs should always use `constrain_recursive_defs`.
pub fn constrain_def_make_constraint(
    constraints: &mut Constraints,
    annotation_rigid_variables: impl Iterator<Item = Variable>,
    annotation_infer_variables: impl Iterator<Item = Variable>,
    def_expr_con: Constraint,
    after_def_con: Constraint,
    def_pattern_state: PatternState,
) -> Constraint {
    let all_flex_variables = (def_pattern_state.vars.into_iter()).chain(annotation_infer_variables);

    let pattern_constraints = constraints.and_constraint(def_pattern_state.constraints);
    let def_pattern_and_body_con = constraints.and_constraint([pattern_constraints, def_expr_con]);

    constraints.let_constraint(
        annotation_rigid_variables,
        all_flex_variables,
        def_pattern_state.headers,
        def_pattern_and_body_con,
        after_def_con,
    )
}

fn constrain_closure_size(
    constraints: &mut Constraints,
    name: Symbol,
    region: Region,
    captured_symbols: &[(Symbol, Variable)],
    closure_var: Variable,
    variables: &mut Vec<Variable>,
) -> Constraint {
    debug_assert!(variables.iter().any(|s| *s == closure_var));

    let mut captured_types = Vec::with_capacity(captured_symbols.len());
    let mut captured_symbols_constraints = Vec::with_capacity(captured_symbols.len());

    for (symbol, var) in captured_symbols {
        // make sure the variable is registered
        variables.push(*var);

        // this symbol is captured, so it must be part of the closure type
        captured_types.push(Type::Variable(*var));

        // make the variable equal to the looked-up type of symbol
        captured_symbols_constraints.push(constraints.lookup(
            *symbol,
            Expected::NoExpectation(Type::Variable(*var)),
            Region::zero(),
        ));
    }

    // pick a more efficient representation if we don't actually capture anything
    let closure_type = Type::ClosureTag {
        name,
        captures: captured_types,
    };

    let finalizer = constraints.equal_types_var(
        closure_var,
        NoExpectation(closure_type),
        Category::ClosureSize,
        region,
    );

    captured_symbols_constraints.push(finalizer);

    constraints.and_constraint(captured_symbols_constraints)
}

pub struct InstantiateRigids {
    pub signature: Type,
    pub new_rigid_variables: Vec<Variable>,
    pub new_infer_variables: Vec<Variable>,
}

fn instantiate_rigids(
    annotation: &Type,
    introduced_vars: &IntroducedVariables,
    loc_pattern: &Loc<Pattern>,
    ftv: &mut MutMap<Lowercase, Variable>, // rigids defined before the current annotation
    headers: &mut SendMap<Symbol, Loc<Type>>,
) -> InstantiateRigids {
    let mut annotation = annotation.clone();
    let mut new_rigid_variables: Vec<Variable> = Vec::new();

    let mut rigid_substitution: MutMap<Variable, Variable> = MutMap::default();
    for named in introduced_vars.iter_named() {
        use std::collections::hash_map::Entry::*;

        match ftv.entry(named.name().clone()) {
            Occupied(occupied) => {
                let existing_rigid = occupied.get();
                rigid_substitution.insert(named.variable(), *existing_rigid);
            }
            Vacant(vacant) => {
                // It's possible to use this rigid in nested defs
                vacant.insert(named.variable());
                new_rigid_variables.push(named.variable());
            }
        }
    }

    // wildcards are always freshly introduced in this annotation
    new_rigid_variables.extend(introduced_vars.wildcards.iter().map(|v| v.value));

    // lambda set vars are always freshly introduced in this annotation
    new_rigid_variables.extend(introduced_vars.lambda_sets.iter().copied());

    let new_infer_variables: Vec<Variable> =
        introduced_vars.inferred.iter().map(|v| v.value).collect();

    // Instantiate rigid variables
    if !rigid_substitution.is_empty() {
        annotation.substitute_variables(&rigid_substitution);
    }

    // TODO investigate when we can skip this. It seems to only be required for correctness
    // for recursive functions. For non-recursive functions the final type is correct, but
    // alias information is sometimes lost
    //
    // Skipping all of this cloning here would be neat!
    let loc_annotation_ref = Loc::at(loc_pattern.region, &annotation);
    if let Pattern::Identifier(symbol) = loc_pattern.value {
        headers.insert(symbol, Loc::at(loc_pattern.region, annotation.clone()));
    } else if let Some(new_headers) =
        crate::pattern::headers_from_annotation(&loc_pattern.value, &loc_annotation_ref)
    {
        headers.extend(new_headers)
    }

    InstantiateRigids {
        signature: annotation,
        new_rigid_variables,
        new_infer_variables,
    }
}

fn constrain_recursive_defs(
    constraints: &mut Constraints,
    env: &mut Env,
    defs: &[Def],
    body_con: Constraint,
    cycle_mark: IllegalCycleMark,
) -> Constraint {
    rec_defs_help(constraints, env, defs, body_con, cycle_mark)
}

pub fn rec_defs_help(
    constraints: &mut Constraints,
    env: &mut Env,
    defs: &[Def],
    body_con: Constraint,
    cycle_mark: IllegalCycleMark,
) -> Constraint {
    // We partition recursive defs into three buckets:
    //   rigid: those with fully-elaborated type annotations (no inference vars), e.g. a -> b
    //   hybrid: those with type annotations containing an inference variable, e.g. _ -> b
    //   flex: those without a type annotation
    let mut rigid_info = Info::with_capacity(defs.len());
    let mut hybrid_and_flex_info = Info::with_capacity(defs.len());

    for def in defs {
        let expr_var = def.expr_var;
        let expr_type = Type::Variable(expr_var);

        let mut def_pattern_state =
            constrain_def_pattern(constraints, env, &def.loc_pattern, expr_type.clone());

        def_pattern_state.vars.push(expr_var);

        match &def.annotation {
            None => {
                let expr_con = constrain_expr(
                    constraints,
                    env,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    NoExpectation(expr_type),
                );
                let expr_con = attach_resolution_constraints(constraints, env, expr_con);

                let def_con = expr_con;

                hybrid_and_flex_info.vars.extend(def_pattern_state.vars);
                hybrid_and_flex_info.constraints.push(def_con);
                hybrid_and_flex_info
                    .def_types
                    .extend(def_pattern_state.headers);
            }

            Some(annotation) => {
                let arity = annotation.signature.arity();
                let mut ftv = env.rigids.clone();

                let InstantiateRigids {
                    signature,
                    new_rigid_variables,
                    new_infer_variables,
                } = instantiate_rigids(
                    &annotation.signature,
                    &annotation.introduced_variables,
                    &def.loc_pattern,
                    &mut ftv,
                    &mut def_pattern_state.headers,
                );

                let is_hybrid = !new_infer_variables.is_empty();

                hybrid_and_flex_info.vars.extend(new_infer_variables);

                let annotation_expected = FromAnnotation(
                    def.loc_pattern.clone(),
                    arity,
                    AnnotationSource::TypedBody {
                        region: annotation.region,
                    },
                    signature.clone(),
                );

                // when a def is annotated, and it's body is a closure, treat this
                // as a named function (in elm terms) for error messages.
                //
                // This means we get errors like "the first argument of `f` is weird"
                // instead of the more generic "something is wrong with the body of `f`"
                match (&def.loc_expr.value, &signature) {
                    (
                        Closure(ClosureData {
                            function_type: fn_var,
                            closure_type: closure_var,
                            return_type: ret_var,
                            captured_symbols,
                            arguments,
                            loc_body,
                            name,
                            ..
                        }),
                        Type::Function(arg_types, _closure_type, ret_type),
                    ) => {
                        // NOTE if we ever have trouble with closure type unification, the ignored
                        // `_closure_type` here is a good place to start investigating

                        let expected = annotation_expected;
                        let region = def.loc_expr.region;

                        let loc_body_expr = &**loc_body;
                        let mut state = PatternState {
                            headers: SendMap::default(),
                            vars: Vec::with_capacity(arguments.len()),
                            constraints: Vec::with_capacity(1),
                            delayed_is_open_constraints: vec![],
                        };
                        let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
                        let ret_var = *ret_var;
                        let closure_var = *closure_var;
                        let ret_type = *ret_type.clone();

                        vars.push(ret_var);
                        vars.push(closure_var);

                        constrain_typed_function_arguments(
                            constraints,
                            env,
                            def,
                            &mut def_pattern_state,
                            &mut state,
                            arguments,
                            arg_types,
                        );
                        let pattern_types = arguments.iter().map(|a| Type::Variable(a.0)).collect();

                        let closure_constraint = constrain_closure_size(
                            constraints,
                            *name,
                            region,
                            captured_symbols,
                            closure_var,
                            &mut vars,
                        );

                        let fn_type = Type::Function(
                            pattern_types,
                            Box::new(Type::Variable(closure_var)),
                            Box::new(ret_type.clone()),
                        );
                        let body_type = NoExpectation(ret_type.clone());
                        let expr_con = constrain_expr(
                            constraints,
                            env,
                            loc_body_expr.region,
                            &loc_body_expr.value,
                            body_type,
                        );
                        let expr_con = attach_resolution_constraints(constraints, env, expr_con);

                        vars.push(*fn_var);

                        let signature_index = constraints.push_type(signature);
                        let state_constraints = constraints.and_constraint(state.constraints);
                        let cons = [
                            constraints.let_constraint(
                                [],
                                state.vars,
                                state.headers,
                                state_constraints,
                                expr_con,
                            ),
                            constraints.equal_types(
                                fn_type.clone(),
                                expected.clone(),
                                Category::Lambda,
                                region,
                            ),
                            // "fn_var is equal to the closure's type" - fn_var is used in code gen
                            // Store type into AST vars. We use Store so errors aren't reported twice
                            constraints.store_index(
                                signature_index,
                                *fn_var,
                                std::file!(),
                                std::line!(),
                            ),
                            constraints.store_index(
                                signature_index,
                                expr_var,
                                std::file!(),
                                std::line!(),
                            ),
                            constraints.store(ret_type, ret_var, std::file!(), std::line!()),
                            closure_constraint,
                        ];

                        let and_constraint = constraints.and_constraint(cons);
                        let def_con = constraints.exists(vars, and_constraint);

                        if is_hybrid {
                            hybrid_and_flex_info.vars.extend(&new_rigid_variables);
                            hybrid_and_flex_info.constraints.push(def_con);
                            hybrid_and_flex_info
                                .def_types
                                .extend(def_pattern_state.headers);
                        } else {
                            rigid_info.vars.extend(&new_rigid_variables);

                            rigid_info.constraints.push(constraints.let_constraint(
                                new_rigid_variables,
                                def_pattern_state.vars,
                                [], // no headers introduced (at this level)
                                def_con,
                                Constraint::True,
                            ));
                            rigid_info.def_types.extend(def_pattern_state.headers);
                        }
                    }
                    _ => {
                        let expected = annotation_expected;

                        let ret_constraint = constrain_expr(
                            constraints,
                            env,
                            def.loc_expr.region,
                            &def.loc_expr.value,
                            expected,
                        );
                        let ret_constraint =
                            attach_resolution_constraints(constraints, env, ret_constraint);

                        let cons = [
                            ret_constraint,
                            // Store type into AST vars. We use Store so errors aren't reported twice
                            constraints.store(signature, expr_var, std::file!(), std::line!()),
                        ];
                        let def_con = constraints.and_constraint(cons);

                        if is_hybrid {
                            hybrid_and_flex_info.vars.extend(&new_rigid_variables);
                            hybrid_and_flex_info.constraints.push(def_con);
                            hybrid_and_flex_info
                                .def_types
                                .extend(def_pattern_state.headers);
                        } else {
                            rigid_info.vars.extend(&new_rigid_variables);

                            rigid_info.constraints.push(constraints.let_constraint(
                                new_rigid_variables,
                                def_pattern_state.vars,
                                [], // no headers introduced (at this level)
                                def_con,
                                Constraint::True,
                            ));
                            rigid_info.def_types.extend(def_pattern_state.headers);
                        }
                    }
                }
            }
        }
    }

    // Strategy for recursive defs:
    //
    // 1. Let-generalize all rigid annotations. These are the source of truth we'll solve
    //    everything else with. If there are circular type errors here, they will be caught
    //    during the let-generalization.
    //
    // 2. Introduce all symbols of the flex + hybrid defs, but don't generalize them yet.
    //    Now, solve those defs' bodies. This way, when checking something like
    //      f = \x -> f [x]
    //    we introduce `f: b -> c`, then constrain the call `f [x]`,
    //    forcing `b -> c ~ List b -> c` and correctly picking up a recursion error.
    //    Had we generalized `b -> c`, the call `f [x]` would have been generalized, and this
    //    error would not be found.
    //
    //    - This works just as well for mutually recursive defs.
    //    - For hybrid defs, we also ensure solved types agree with what the
    //      elaborated parts of their type annotations demand.
    //
    // 3. Now properly let-generalize the flex + hybrid defs, since we now know their types and
    //    that they don't have circular type errors.
    //
    // 4. Solve the bodies of the typed body defs, and check that they agree the types of the type
    //    annotation.
    //
    // 5. Solve the rest of the program that happens after this recursive def block.

    // 2. Solve untyped defs without generalization of their symbols.
    let untyped_body_constraints = constraints.and_constraint(hybrid_and_flex_info.constraints);
    let untyped_def_symbols_constr = constraints.let_constraint(
        [],
        [],
        hybrid_and_flex_info.def_types.clone(),
        Constraint::True,
        untyped_body_constraints,
    );

    // an extra constraint that propagates information to the solver to check for invalid recursion
    // and generate a good error message there.
    let (loc_symbols, expr_regions): (Vec<_>, Vec<_>) = defs
        .iter()
        .flat_map(|def| {
            symbols_introduced_from_pattern(&def.loc_pattern)
                .map(move |loc_symbol| ((loc_symbol.value, loc_symbol.region), def.loc_expr.region))
        })
        .unzip();

    let cycle_constraint = constraints.check_cycle(loc_symbols, expr_regions, cycle_mark);

    let typed_body_constraints = constraints.and_constraint(rigid_info.constraints);
    let typed_body_and_final_constr =
        constraints.and_constraint([typed_body_constraints, cycle_constraint, body_con]);

    // 3. Properly generalize untyped defs after solving them.
    let inner = constraints.let_constraint(
        [],
        hybrid_and_flex_info.vars,
        hybrid_and_flex_info.def_types,
        untyped_def_symbols_constr,
        // 4 + 5. Solve the typed body defs, and the rest of the program.
        typed_body_and_final_constr,
    );

    // 1. Let-generalize annotations we know.
    constraints.let_constraint(
        rigid_info.vars,
        [],
        rigid_info.def_types,
        Constraint::True,
        inner,
    )
}

#[inline(always)]
fn constrain_field_update(
    constraints: &mut Constraints,
    env: &mut Env,
    var: Variable,
    region: Region,
    field: Lowercase,
    loc_expr: &Loc<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = Type::Variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = ForReason(reason, field_type.clone(), region);
    let con = constrain_expr(constraints, env, loc_expr.region, &loc_expr.value, expected);

    (var, field_type, con)
}
