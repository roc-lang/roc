use crate::builtins::{empty_list_type, float_literal, int_literal, list_type, str_type};
use crate::pattern::{constrain_pattern, PatternState};
use roc_can::annotation::IntroducedVariables;
use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::LetConstraint;
use roc_can::def::{Declaration, Def};
use roc_can::expected::Expected::{self, *};
use roc_can::expected::PExpected;
use roc_can::expr::Expr::{self, *};
use roc_can::expr::{ClosureData, Field, WhenBranch};
use roc_can::pattern::Pattern;
use roc_collections::all::{ImMap, Index, MutSet, SendMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;
use roc_types::types::Type::{self, *};
use roc_types::types::{AnnotationSource, Category, PReason, Reason, RecordField};

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

#[inline(always)]
pub fn exists(flex_vars: Vec<Variable>, constraint: Constraint) -> Constraint {
    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars,
        def_types: SendMap::default(),
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

pub struct Env {
    /// Whenever we encounter a user-defined type variable (a "rigid" var for short),
    /// for example `a` in the annotation `identity : a -> a`, we add it to this
    /// map so that expressions within that annotation can share these vars.
    pub rigids: ImMap<Lowercase, Variable>,
    pub home: ModuleId,
}

fn constrain_untyped_args(
    env: &Env,
    arguments: &[(Variable, Loc<Pattern>)],
    closure_type: Type,
    return_type: Type,
) -> (Vec<Variable>, PatternState, Type) {
    let mut vars = Vec::with_capacity(arguments.len());
    let mut pattern_types = Vec::with_capacity(arguments.len());

    let mut pattern_state = PatternState::default();

    for (pattern_var, loc_pattern) in arguments {
        let pattern_type = Type::Variable(*pattern_var);
        let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

        pattern_types.push(pattern_type);

        constrain_pattern(
            env,
            &loc_pattern.value,
            loc_pattern.region,
            pattern_expected,
            &mut pattern_state,
            true,
        );

        vars.push(*pattern_var);
    }

    let function_type =
        Type::Function(pattern_types, Box::new(closure_type), Box::new(return_type));

    (vars, pattern_state, function_type)
}

pub fn constrain_expr(
    env: &Env,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Constraint {
    match expr {
        Int(var, precision, _, _) => int_literal(*var, *precision, expected, region),
        Num(var, _, _) => exists(
            vec![*var],
            Eq(
                crate::builtins::num_num(Type::Variable(*var)),
                expected,
                Category::Num,
                region,
            ),
        ),
        Float(var, precision, _, _) => float_literal(*var, *precision, expected, region),
        EmptyRecord => constrain_empty_record(region, expected),
        Expr::Record { record_var, fields } => {
            if fields.is_empty() {
                constrain_empty_record(region, expected)
            } else {
                let mut field_exprs = SendMap::default();
                let mut field_types = SendMap::default();
                let mut field_vars = Vec::with_capacity(fields.len());

                // Constraints need capacity for each field
                // + 1 for the record itself + 1 for record var
                let mut constraints = Vec::with_capacity(2 + fields.len());

                for (label, field) in fields {
                    let field_var = field.var;
                    let loc_field_expr = &field.loc_expr;
                    let (field_type, field_con) = constrain_field(env, field_var, &*loc_field_expr);

                    field_vars.push(field_var);
                    field_exprs.insert(label.clone(), loc_field_expr);
                    field_types.insert(label.clone(), RecordField::Required(field_type));

                    constraints.push(field_con);
                }

                let record_type = Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // We can put `static EMPTY_REC: Type = Type::EmptyRec`, but that requires a
                    // lifetime parameter on `Type`
                    Box::new(Type::EmptyRec),
                );
                let record_con = Eq(record_type, expected.clone(), Category::Record, region);
                constraints.push(record_con);

                // variable to store in the AST
                let stored_con = Eq(
                    Type::Variable(*record_var),
                    expected,
                    Category::Storage(std::file!(), std::line!()),
                    region,
                );

                field_vars.push(*record_var);
                constraints.push(stored_con);

                exists(field_vars, And(constraints))
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

            let fields_type = Type::Record(fields, Box::new(Type::Variable(*ext_var)));
            let record_type = Type::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = Eq(
                record_type.clone(),
                NoExpectation(fields_type),
                Category::Record,
                region,
            );
            let record_con = Eq(record_type.clone(), expected, Category::Record, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let con = Lookup(
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

            exists(vars, And(cons))
        }
        Str(_) => Eq(str_type(), expected, Category::Str, region),
        List {
            elem_var,
            loc_elems,
        } => {
            if loc_elems.is_empty() {
                exists(
                    vec![*elem_var],
                    Eq(empty_list_type(*elem_var), expected, Category::List, region),
                )
            } else {
                let list_elem_type = Type::Variable(*elem_var);
                let mut constraints = Vec::with_capacity(1 + loc_elems.len());

                for (index, loc_elem) in loc_elems.iter().enumerate() {
                    let elem_expected = ForReason(
                        Reason::ElemInList {
                            index: Index::zero_based(index),
                        },
                        list_elem_type.clone(),
                        loc_elem.region,
                    );
                    let constraint =
                        constrain_expr(env, loc_elem.region, &loc_elem.value, elem_expected);

                    constraints.push(constraint);
                }

                constraints.push(Eq(
                    list_type(list_elem_type),
                    expected,
                    Category::List,
                    region,
                ));

                exists(vec![*elem_var], And(constraints))
            }
        }
        Call(boxed, loc_args, called_via) => {
            let (fn_var, loc_fn, closure_var, ret_var) = &**boxed;
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let opt_symbol = if let Var(symbol) = loc_fn.value {
                Some(symbol)
            } else {
                None
            };

            let fn_type = Variable(*fn_var);
            let fn_region = loc_fn.region;
            let fn_expected = NoExpectation(fn_type.clone());

            let fn_reason = Reason::FnCall {
                name: opt_symbol,
                arity: loc_args.len() as u8,
            };

            let fn_con = constrain_expr(env, loc_fn.region, &loc_fn.value, fn_expected);

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
                    arg_index: Index::zero_based(index),
                };
                let expected_arg = ForReason(reason, arg_type.clone(), region);
                let arg_con = constrain_expr(env, loc_arg.region, &loc_arg.value, expected_arg);

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = ForReason(
                fn_reason,
                Function(
                    arg_types,
                    Box::new(closure_type),
                    Box::new(ret_type.clone()),
                ),
                region,
            );

            let category = Category::CallResult(opt_symbol, *called_via);

            exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(fn_type, expected_fn_type, category.clone(), fn_region),
                    And(arg_cons),
                    Eq(ret_type, expected, category, region),
                ]),
            )
        }
        Var(symbol) => {
            // make lookup constraint to lookup this symbol's type in the environment
            Lookup(*symbol, expected, region)
        }
        Closure(ClosureData {
            function_type: fn_var,
            closure_type: closure_var,
            closure_ext_var,
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
            let closure_ext_var = *closure_ext_var;

            let closure_type = Type::Variable(closure_var);
            let return_type = Type::Variable(ret_var);
            let (mut vars, pattern_state, function_type) =
                constrain_untyped_args(env, arguments, closure_type, return_type.clone());

            vars.push(ret_var);
            vars.push(closure_var);
            vars.push(closure_ext_var);
            vars.push(*fn_var);

            let body_type = NoExpectation(return_type);
            let ret_constraint =
                constrain_expr(env, loc_body_expr.region, &loc_body_expr.value, body_type);

            // make sure the captured symbols are sorted!
            debug_assert_eq!(captured_symbols.clone(), {
                let mut copy = captured_symbols.clone();
                copy.sort();
                copy
            });

            let closure_constraint = constrain_closure_size(
                *name,
                region,
                captured_symbols,
                closure_var,
                closure_ext_var,
                &mut vars,
            );

            exists(
                vars,
                And(vec![
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: pattern_state.vars,
                        def_types: pattern_state.headers,
                        defs_constraint: And(pattern_state.constraints),
                        ret_constraint,
                    })),
                    // "the closure's type is equal to expected type"
                    Eq(function_type.clone(), expected, Category::Lambda, region),
                    // "fn_var is equal to the closure's type" - fn_var is used in code gen
                    Eq(
                        Type::Variable(*fn_var),
                        NoExpectation(function_type),
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    ),
                    closure_constraint,
                ]),
            )
        }

        Expect(loc_cond, continuation) => {
            let expect_bool = |region| {
                let bool_type = Type::Variable(Variable::BOOL);
                Expected::ForReason(Reason::ExpectCondition, bool_type, region)
            };

            let cond_con = constrain_expr(
                env,
                loc_cond.region,
                &loc_cond.value,
                expect_bool(loc_cond.region),
            );

            let continuation_con =
                constrain_expr(env, continuation.region, &continuation.value, expected);

            exists(vec![], And(vec![cond_con, continuation_con]))
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
            let cond_var_is_bool_con = Eq(
                Type::Variable(*cond_var),
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
                            env,
                            loc_cond.region,
                            &loc_cond.value,
                            expect_bool(loc_cond.region),
                        );

                        let then_con = constrain_expr(
                            env,
                            loc_body.region,
                            &loc_body.value,
                            FromAnnotation(
                                name.clone(),
                                arity,
                                AnnotationSource::TypedIfBranch {
                                    index: Index::zero_based(index),
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
                        env,
                        final_else.region,
                        &final_else.value,
                        FromAnnotation(
                            name,
                            arity,
                            AnnotationSource::TypedIfBranch {
                                index: Index::zero_based(branches.len()),
                                num_branches,
                                region: ann_source.region(),
                            },
                            tipe.clone(),
                        ),
                    );

                    let ast_con = Eq(
                        Type::Variable(*branch_var),
                        NoExpectation(tipe),
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    );

                    branch_cons.push(ast_con);
                    branch_cons.push(else_con);

                    exists(vec![*cond_var, *branch_var], And(branch_cons))
                }
                _ => {
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_con = constrain_expr(
                            env,
                            loc_cond.region,
                            &loc_cond.value,
                            expect_bool(loc_cond.region),
                        );

                        let then_con = constrain_expr(
                            env,
                            loc_body.region,
                            &loc_body.value,
                            ForReason(
                                Reason::IfBranch {
                                    index: Index::zero_based(index),
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
                        env,
                        final_else.region,
                        &final_else.value,
                        ForReason(
                            Reason::IfBranch {
                                index: Index::zero_based(branches.len()),
                                total_branches: branches.len() + 1,
                            },
                            Type::Variable(*branch_var),
                            final_else.region,
                        ),
                    );

                    branch_cons.push(Eq(
                        Type::Variable(*branch_var),
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        region,
                    ));
                    branch_cons.push(else_con);

                    exists(vec![*cond_var, *branch_var], And(branch_cons))
                }
            }
        }
        When {
            cond_var,
            expr_var,
            loc_cond,
            branches,
            ..
        } => {
            // Infer the condition expression's type.
            let cond_var = *cond_var;
            let cond_type = Variable(cond_var);
            let expr_con = constrain_expr(
                env,
                region,
                &loc_cond.value,
                NoExpectation(cond_type.clone()),
            );

            let mut constraints = Vec::with_capacity(branches.len() + 1);
            constraints.push(expr_con);

            match &expected {
                FromAnnotation(name, arity, ann_source, _typ) => {
                    // NOTE deviation from elm.
                    //
                    // in elm, `_typ` is used, but because we have this `expr_var` too
                    // and need to constrain it, this is what works and gives better error messages
                    let typ = Type::Variable(*expr_var);

                    for (index, when_branch) in branches.iter().enumerate() {
                        let pattern_region =
                            Region::across_all(when_branch.patterns.iter().map(|v| &v.region));

                        let branch_con = constrain_when_branch(
                            env,
                            when_branch.value.region,
                            when_branch,
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: Index::zero_based(index),
                                },
                                cond_type.clone(),
                                pattern_region,
                            ),
                            FromAnnotation(
                                name.clone(),
                                *arity,
                                AnnotationSource::TypedWhenBranch {
                                    index: Index::zero_based(index),
                                    region: ann_source.region(),
                                },
                                typ.clone(),
                            ),
                        );

                        constraints.push(branch_con);
                    }

                    constraints.push(Eq(typ, expected, Category::When, region));

                    return exists(vec![cond_var, *expr_var], And(constraints));
                }

                _ => {
                    let branch_type = Variable(*expr_var);
                    let mut branch_cons = Vec::with_capacity(branches.len());

                    for (index, when_branch) in branches.iter().enumerate() {
                        let pattern_region =
                            Region::across_all(when_branch.patterns.iter().map(|v| &v.region));
                        let branch_con = constrain_when_branch(
                            env,
                            region,
                            when_branch,
                            PExpected::ForReason(
                                PReason::WhenMatch {
                                    index: Index::zero_based(index),
                                },
                                cond_type.clone(),
                                pattern_region,
                            ),
                            ForReason(
                                Reason::WhenBranch {
                                    index: Index::zero_based(index),
                                },
                                branch_type.clone(),
                                when_branch.value.region,
                            ),
                        );

                        branch_cons.push(branch_con);
                    }

                    constraints.push(And(vec![
                        // Record the original conditional expression's constraint.
                        // Each branch's pattern must have the same type
                        // as the condition expression did.
                        And(branch_cons),
                        // The return type of each branch must equal
                        // the return type of the entire when-expression.
                        Eq(branch_type, expected, Category::When, region),
                    ]));
                }
            }

            // exhautiveness checking happens when converting to mono::Expr
            exists(vec![cond_var, *expr_var], And(constraints))
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
            rec_field_types.insert(label, RecordField::Demanded(field_type.clone()));

            let record_type = Type::Record(rec_field_types, Box::new(ext_type));
            let record_expected = Expected::NoExpectation(record_type);

            let category = Category::Access(field.clone());

            let record_con = Eq(
                Type::Variable(*record_var),
                record_expected.clone(),
                category.clone(),
                region,
            );

            let constraint = constrain_expr(
                &Env {
                    home: env.home,
                    rigids: ImMap::default(),
                },
                region,
                &loc_expr.value,
                record_expected,
            );

            exists(
                vec![*record_var, field_var, ext_var],
                And(vec![
                    constraint,
                    Eq(field_type, expected, category, region),
                    record_con,
                ]),
            )
        }
        Accessor {
            name: closure_name,
            function_var,
            field,
            record_var,
            closure_ext_var: closure_var,
            ext_var,
            field_var,
        } => {
            let ext_var = *ext_var;
            let ext_type = Variable(ext_var);
            let field_var = *field_var;
            let field_type = Variable(field_var);

            let mut field_types = SendMap::default();
            let label = field.clone();
            field_types.insert(label, RecordField::Demanded(field_type.clone()));
            let record_type = Type::Record(field_types, Box::new(ext_type));

            let category = Category::Accessor(field.clone());

            let record_expected = Expected::NoExpectation(record_type.clone());
            let record_con = Eq(
                Type::Variable(*record_var),
                record_expected,
                category.clone(),
                region,
            );

            let lambda_set = Type::ClosureTag {
                name: *closure_name,
                ext: *closure_var,
            };

            let function_type = Type::Function(
                vec![record_type],
                Box::new(lambda_set),
                Box::new(field_type),
            );

            exists(
                vec![*record_var, *function_var, *closure_var, field_var, ext_var],
                And(vec![
                    Eq(function_type.clone(), expected, category.clone(), region),
                    Eq(
                        function_type,
                        NoExpectation(Variable(*function_var)),
                        category,
                        region,
                    ),
                    record_con,
                ]),
            )
        }
        LetRec(defs, loc_ret, var) => {
            let body_con = constrain_expr(env, loc_ret.region, &loc_ret.value, expected.clone());

            exists(
                vec![*var],
                And(vec![
                    constrain_recursive_defs(env, defs, body_con),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(
                        Type::Variable(*var),
                        expected,
                        Category::Storage(std::file!(), std::line!()),
                        loc_ret.region,
                    ),
                ]),
            )
        }
        LetNonRec(def, loc_ret, var) => {
            let mut stack = Vec::with_capacity(1);

            let mut loc_ret = loc_ret;

            stack.push((def, var, loc_ret.region));

            while let LetNonRec(def, new_loc_ret, var) = &loc_ret.value {
                stack.push((def, var, new_loc_ret.region));
                loc_ret = new_loc_ret;
            }

            let mut body_con =
                constrain_expr(env, loc_ret.region, &loc_ret.value, expected.clone());

            while let Some((def, var, ret_region)) = stack.pop() {
                body_con = exists(
                    vec![*var],
                    And(vec![
                        constrain_def(env, def, body_con),
                        // Record the type of the entire def-expression in the variable.
                        // Code gen will need that later!
                        Eq(
                            Type::Variable(*var),
                            expected.clone(),
                            Category::Storage(std::file!(), std::line!()),
                            ret_region,
                        ),
                    ]),
                )
            }

            body_con
        }
        Tag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            let mut vars = Vec::with_capacity(arguments.len());
            let mut types = Vec::with_capacity(arguments.len());
            let mut arg_cons = Vec::with_capacity(arguments.len());

            for (var, loc_expr) in arguments {
                let arg_con = constrain_expr(
                    env,
                    loc_expr.region,
                    &loc_expr.value,
                    Expected::NoExpectation(Type::Variable(*var)),
                );

                arg_cons.push(arg_con);
                vars.push(*var);
                types.push(Type::Variable(*var));
            }

            let union_con = Eq(
                Type::TagUnion(
                    vec![(name.clone(), types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
                expected.clone(),
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: arguments.len(),
                },
                region,
            );
            let ast_con = Eq(
                Type::Variable(*variant_var),
                expected,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }
        ZeroArgumentTag {
            variant_var,
            ext_var,
            name,
            arguments,
            closure_name,
        } => {
            let mut vars = Vec::with_capacity(arguments.len());
            let mut types = Vec::with_capacity(arguments.len());
            let mut arg_cons = Vec::with_capacity(arguments.len());

            for (var, loc_expr) in arguments {
                let arg_con = constrain_expr(
                    env,
                    loc_expr.region,
                    &loc_expr.value,
                    Expected::NoExpectation(Type::Variable(*var)),
                );

                arg_cons.push(arg_con);
                vars.push(*var);
                types.push(Type::Variable(*var));
            }

            let union_con = Eq(
                Type::FunctionOrTagUnion(
                    name.clone(),
                    *closure_name,
                    Box::new(Type::Variable(*ext_var)),
                ),
                expected.clone(),
                Category::TagApply {
                    tag_name: name.clone(),
                    args_count: arguments.len(),
                },
                region,
            );
            let ast_con = Eq(
                Type::Variable(*variant_var),
                expected,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }

        RunLowLevel { args, ret_var, op } => {
            // This is a modified version of what we do for function calls.

            // The operation's return type
            let ret_type = Variable(*ret_var);

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            let mut add_arg = |index, arg_type: Type, arg| {
                let reason = Reason::LowLevelOpArg {
                    op: *op,
                    arg_index: Index::zero_based(index),
                };
                let expected_arg = ForReason(reason, arg_type.clone(), Region::zero());
                let arg_con = constrain_expr(env, Region::zero(), arg, expected_arg);

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            };

            for (index, (arg_var, arg)) in args.iter().enumerate() {
                vars.push(*arg_var);

                add_arg(index, Variable(*arg_var), arg);
            }

            let category = Category::LowLevelOpResult(*op);

            exists(
                vars,
                And(vec![
                    And(arg_cons),
                    Eq(ret_type, expected, category, region),
                ]),
            )
        }
        ForeignCall {
            args,
            ret_var,
            foreign_symbol,
        } => {
            // This is a modified version of what we do for function calls.

            // The operation's return type
            let ret_type = Variable(*ret_var);

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(1 + args.len());

            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(args.len());
            let mut arg_cons = Vec::with_capacity(args.len());

            let mut add_arg = |index, arg_type: Type, arg| {
                let reason = Reason::ForeignCallArg {
                    foreign_symbol: foreign_symbol.clone(),
                    arg_index: Index::zero_based(index),
                };
                let expected_arg = ForReason(reason, arg_type.clone(), Region::zero());
                let arg_con = constrain_expr(env, Region::zero(), arg, expected_arg);

                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            };

            for (index, (arg_var, arg)) in args.iter().enumerate() {
                vars.push(*arg_var);

                add_arg(index, Variable(*arg_var), arg);
            }

            let category = Category::ForeignCall;

            exists(
                vars,
                And(vec![
                    And(arg_cons),
                    Eq(ret_type, expected, category, region),
                ]),
            )
        }
        RuntimeError(_) => {
            // Runtime Errors have no constraints because they're going to crash.
            True
        }
    }
}

#[inline(always)]
fn constrain_when_branch(
    env: &Env,
    region: Region,
    when_branch: &WhenBranch,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
) -> Constraint {
    let ret_constraint = constrain_expr(env, region, &when_branch.value.value, expr_expected);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    // TODO investigate for error messages, is it better to unify all branches with a variable,
    // then unify that variable with the expectation?
    for loc_pattern in &when_branch.patterns {
        constrain_pattern(
            env,
            &loc_pattern.value,
            loc_pattern.region,
            pattern_expected.clone(),
            &mut state,
            true,
        );
    }

    if let Some(loc_guard) = &when_branch.guard {
        let guard_constraint = constrain_expr(
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
        Constraint::Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: Constraint::And(state.constraints),
            ret_constraint: Constraint::Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: Vec::new(),
                def_types: SendMap::default(),
                defs_constraint: guard_constraint,
                ret_constraint,
            })),
        }))
    } else {
        Constraint::Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: Constraint::And(state.constraints),
            ret_constraint,
        }))
    }
}

fn constrain_field(env: &Env, field_var: Variable, loc_expr: &Loc<Expr>) -> (Type, Constraint) {
    let field_type = Variable(field_var);
    let field_expected = NoExpectation(field_type.clone());
    let constraint = constrain_expr(env, loc_expr.region, &loc_expr.value, field_expected);

    (field_type, constraint)
}

#[inline(always)]
fn constrain_empty_record(region: Region, expected: Expected<Type>) -> Constraint {
    Eq(EmptyRec, expected, Category::Record, region)
}

/// Constrain top-level module declarations
#[inline(always)]
pub fn constrain_decls(home: ModuleId, decls: &[Declaration]) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;

    let mut env = Env {
        home,
        rigids: ImMap::default(),
    };

    for decl in decls.iter().rev() {
        // Clear the rigids from the previous iteration.
        // rigids are not shared between top-level definitions
        env.rigids.clear();

        match decl {
            Declaration::Declare(def) | Declaration::Builtin(def) => {
                constraint = constrain_def(&env, def, constraint);
            }
            Declaration::DeclareRec(defs) => {
                constraint = constrain_recursive_defs(&env, defs, constraint);
            }
            Declaration::InvalidCycle(_) => {
                // invalid cycles give a canonicalization error. we skip them here.
                continue;
            }
        }
    }

    // this assert make the "root" of the constraint wasn't dropped
    debug_assert!(constraint.contains_save_the_environment());

    constraint
}

fn constrain_def_pattern(env: &Env, loc_pattern: &Loc<Pattern>, expr_type: Type) -> PatternState {
    let pattern_expected = PExpected::NoExpectation(expr_type);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    constrain_pattern(
        env,
        &loc_pattern.value,
        loc_pattern.region,
        pattern_expected,
        &mut state,
        true,
    );

    state
}

fn constrain_def(env: &Env, def: &Def, body_con: Constraint) -> Constraint {
    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut def_pattern_state = constrain_def_pattern(env, &def.loc_pattern, expr_type.clone());

    def_pattern_state.vars.push(expr_var);

    let mut new_rigids = Vec::new();

    let expr_con = match &def.annotation {
        Some(annotation) => {
            let arity = annotation.signature.arity();
            let rigids = &env.rigids;
            let mut ftv = rigids.clone();

            let signature = instantiate_rigids(
                &annotation.signature,
                &annotation.introduced_variables,
                &mut new_rigids,
                &mut ftv,
                &def.loc_pattern,
                &mut def_pattern_state.headers,
            );

            let env = &Env {
                home: env.home,
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

            def_pattern_state.constraints.push(Eq(
                expr_type,
                annotation_expected.clone(),
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
                        closure_ext_var,
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
                    let mut state = PatternState {
                        headers: SendMap::default(),
                        vars: Vec::with_capacity(arguments.len()),
                        constraints: Vec::with_capacity(1),
                    };
                    let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
                    let mut pattern_types = Vec::with_capacity(state.vars.capacity());
                    let ret_var = *ret_var;
                    let closure_var = *closure_var;
                    let closure_ext_var = *closure_ext_var;
                    let ret_type = *ret_type.clone();

                    vars.push(ret_var);
                    vars.push(closure_var);
                    vars.push(closure_ext_var);

                    let it = arguments.iter().zip(arg_types.iter()).enumerate();
                    for (index, ((pattern_var, loc_pattern), loc_ann)) in it {
                        {
                            // ensure type matches the one in the annotation
                            let opt_label =
                                if let Pattern::Identifier(label) = def.loc_pattern.value {
                                    Some(label)
                                } else {
                                    None
                                };
                            let pattern_type: &Type = loc_ann;

                            let pattern_expected = PExpected::ForReason(
                                PReason::TypedArg {
                                    index: Index::zero_based(index),
                                    opt_name: opt_label,
                                },
                                pattern_type.clone(),
                                loc_pattern.region,
                            );

                            constrain_pattern(
                                env,
                                &loc_pattern.value,
                                loc_pattern.region,
                                pattern_expected,
                                &mut state,
                                false,
                            );
                        }

                        {
                            // NOTE: because we perform an equality with part of the signature
                            // this constraint must be to the def_pattern_state's constraints
                            def_pattern_state.vars.push(*pattern_var);
                            pattern_types.push(Type::Variable(*pattern_var));

                            let pattern_con = Eq(
                                Type::Variable(*pattern_var),
                                Expected::NoExpectation(loc_ann.clone()),
                                Category::Storage(std::file!(), std::line!()),
                                loc_pattern.region,
                            );

                            def_pattern_state.constraints.push(pattern_con);
                        }
                    }

                    let closure_constraint = constrain_closure_size(
                        *name,
                        region,
                        captured_symbols,
                        closure_var,
                        closure_ext_var,
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

                    let ret_constraint =
                        constrain_expr(env, loc_body_expr.region, &loc_body_expr.value, body_type);

                    vars.push(*fn_var);
                    let defs_constraint = And(state.constraints);

                    exists(
                        vars,
                        And(vec![
                            Let(Box::new(LetConstraint {
                                rigid_vars: Vec::new(),
                                flex_vars: state.vars,
                                def_types: state.headers,
                                defs_constraint,
                                ret_constraint,
                            })),
                            Eq(
                                Type::Variable(closure_var),
                                Expected::FromAnnotation(
                                    def.loc_pattern.clone(),
                                    arity,
                                    AnnotationSource::TypedBody {
                                        region: annotation.region,
                                    },
                                    *signature_closure_type.clone(),
                                ),
                                Category::ClosureSize,
                                region,
                            ),
                            Store(signature.clone(), *fn_var, std::file!(), std::line!()),
                            Store(signature, expr_var, std::file!(), std::line!()),
                            Store(ret_type, ret_var, std::file!(), std::line!()),
                            closure_constraint,
                        ]),
                    )
                }

                _ => {
                    let expected = annotation_expected;

                    let ret_constraint =
                        constrain_expr(env, def.loc_expr.region, &def.loc_expr.value, expected);

                    And(vec![
                        Let(Box::new(LetConstraint {
                            rigid_vars: Vec::new(),
                            flex_vars: vec![],
                            def_types: SendMap::default(),
                            defs_constraint: True,
                            ret_constraint,
                        })),
                        // Store type into AST vars. We use Store so errors aren't reported twice
                        Store(signature, expr_var, std::file!(), std::line!()),
                    ])
                }
            }
        }
        None => {
            // no annotation, so no extra work with rigids

            constrain_expr(
                env,
                def.loc_expr.region,
                &def.loc_expr.value,
                NoExpectation(expr_type),
            )
        }
    };

    Let(Box::new(LetConstraint {
        rigid_vars: new_rigids,
        flex_vars: def_pattern_state.vars,
        def_types: def_pattern_state.headers,
        defs_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),        // always empty
            flex_vars: Vec::new(),         // empty, because our functions have no arguments
            def_types: SendMap::default(), // empty, because our functions have no arguments!
            defs_constraint: And(def_pattern_state.constraints),
            ret_constraint: expr_con,
        })),
        ret_constraint: body_con,
    }))
}

fn constrain_closure_size(
    name: Symbol,
    region: Region,
    captured_symbols: &[(Symbol, Variable)],
    closure_var: Variable,
    closure_ext_var: Variable,
    variables: &mut Vec<Variable>,
) -> Constraint {
    debug_assert!(variables.iter().any(|s| *s == closure_var));
    debug_assert!(variables.iter().any(|s| *s == closure_ext_var));

    let mut tag_arguments = Vec::with_capacity(captured_symbols.len());
    let mut captured_symbols_constraints = Vec::with_capacity(captured_symbols.len());

    for (symbol, var) in captured_symbols {
        // make sure the variable is registered
        variables.push(*var);

        // this symbol is captured, so it must be part of the closure type
        tag_arguments.push(Type::Variable(*var));

        // make the variable equal to the looked-up type of symbol
        captured_symbols_constraints.push(Constraint::Lookup(
            *symbol,
            Expected::NoExpectation(Type::Variable(*var)),
            Region::zero(),
        ));
    }

    // pick a more efficient representation if we don't actually capture anything
    let closure_type = if tag_arguments.is_empty() {
        Type::ClosureTag {
            name,
            ext: closure_ext_var,
        }
    } else {
        let tag_name = TagName::Closure(name);
        Type::TagUnion(
            vec![(tag_name, tag_arguments)],
            Box::new(Type::Variable(closure_ext_var)),
        )
    };

    let finalizer = Eq(
        Type::Variable(closure_var),
        NoExpectation(closure_type),
        Category::ClosureSize,
        region,
    );

    captured_symbols_constraints.push(finalizer);

    Constraint::And(captured_symbols_constraints)
}

fn instantiate_rigids(
    annotation: &Type,
    introduced_vars: &IntroducedVariables,
    new_rigids: &mut Vec<Variable>,
    ftv: &mut ImMap<Lowercase, Variable>, // rigids defined before the current annotation
    loc_pattern: &Loc<Pattern>,
    headers: &mut SendMap<Symbol, Loc<Type>>,
) -> Type {
    let mut annotation = annotation.clone();
    let mut rigid_substitution: ImMap<Variable, Type> = ImMap::default();

    let outside_rigids: MutSet<Variable> = ftv.values().copied().collect();

    for (name, var) in introduced_vars.var_by_name.iter() {
        if let Some(existing_rigid) = ftv.get(name) {
            rigid_substitution.insert(*var, Type::Variable(*existing_rigid));
        } else {
            // It's possible to use this rigid in nested defs
            ftv.insert(name.clone(), *var);
        }
    }

    // Instantiate rigid variables
    if !rigid_substitution.is_empty() {
        annotation.substitute(&rigid_substitution);
    }

    if let Some(new_headers) = crate::pattern::headers_from_annotation(
        &loc_pattern.value,
        &Loc::at(loc_pattern.region, annotation.clone()),
    ) {
        for (symbol, loc_type) in new_headers {
            for var in loc_type.value.variables() {
                // a rigid is only new if this annotation is the first occurrence of this rigid
                if !outside_rigids.contains(&var) {
                    new_rigids.push(var);
                }
            }
            headers.insert(symbol, loc_type);
        }
    }

    for (i, wildcard) in introduced_vars.wildcards.iter().enumerate() {
        ftv.insert(format!("*{}", i).into(), *wildcard);
    }

    annotation
}

fn constrain_recursive_defs(env: &Env, defs: &[Def], body_con: Constraint) -> Constraint {
    rec_defs_help(
        env,
        defs,
        body_con,
        Info::with_capacity(defs.len()),
        Info::with_capacity(defs.len()),
    )
}

pub fn rec_defs_help(
    env: &Env,
    defs: &[Def],
    body_con: Constraint,
    mut rigid_info: Info,
    mut flex_info: Info,
) -> Constraint {
    for def in defs {
        let expr_var = def.expr_var;
        let expr_type = Type::Variable(expr_var);

        let mut def_pattern_state = constrain_def_pattern(env, &def.loc_pattern, expr_type.clone());

        def_pattern_state.vars.push(expr_var);

        let mut new_rigids = Vec::new();
        match &def.annotation {
            None => {
                let expr_con = constrain_expr(
                    env,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    NoExpectation(expr_type),
                );

                // TODO investigate if this let can be safely removed
                let def_con = Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: Vec::new(), // empty because Roc function defs have no args
                    def_types: SendMap::default(), // empty because Roc function defs have no args
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                flex_info.vars = def_pattern_state.vars;
                flex_info.constraints.push(def_con);
                flex_info.def_types.extend(def_pattern_state.headers);
            }

            Some(annotation) => {
                let arity = annotation.signature.arity();
                let mut ftv = env.rigids.clone();

                let signature = instantiate_rigids(
                    &annotation.signature,
                    &annotation.introduced_variables,
                    &mut new_rigids,
                    &mut ftv,
                    &def.loc_pattern,
                    &mut def_pattern_state.headers,
                );

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
                            closure_ext_var,
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
                        };
                        let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
                        let mut pattern_types = Vec::with_capacity(state.vars.capacity());
                        let ret_var = *ret_var;
                        let closure_var = *closure_var;
                        let closure_ext_var = *closure_ext_var;
                        let ret_type = *ret_type.clone();

                        vars.push(ret_var);
                        vars.push(closure_var);
                        vars.push(closure_ext_var);

                        let it = arguments.iter().zip(arg_types.iter()).enumerate();
                        for (index, ((pattern_var, loc_pattern), loc_ann)) in it {
                            {
                                // ensure type matches the one in the annotation
                                let opt_label =
                                    if let Pattern::Identifier(label) = def.loc_pattern.value {
                                        Some(label)
                                    } else {
                                        None
                                    };
                                let pattern_type: &Type = loc_ann;

                                let pattern_expected = PExpected::ForReason(
                                    PReason::TypedArg {
                                        index: Index::zero_based(index),
                                        opt_name: opt_label,
                                    },
                                    pattern_type.clone(),
                                    loc_pattern.region,
                                );

                                constrain_pattern(
                                    env,
                                    &loc_pattern.value,
                                    loc_pattern.region,
                                    pattern_expected,
                                    &mut state,
                                    false,
                                );
                            }

                            {
                                // NOTE: because we perform an equality with part of the signature
                                // this constraint must be to the def_pattern_state's constraints
                                def_pattern_state.vars.push(*pattern_var);
                                pattern_types.push(Type::Variable(*pattern_var));

                                let pattern_con = Eq(
                                    Type::Variable(*pattern_var),
                                    Expected::NoExpectation(loc_ann.clone()),
                                    Category::Storage(std::file!(), std::line!()),
                                    loc_pattern.region,
                                );

                                def_pattern_state.constraints.push(pattern_con);
                            }
                        }

                        let closure_constraint = constrain_closure_size(
                            *name,
                            region,
                            captured_symbols,
                            closure_var,
                            closure_ext_var,
                            &mut vars,
                        );

                        let fn_type = Type::Function(
                            pattern_types,
                            Box::new(Type::Variable(closure_var)),
                            Box::new(ret_type.clone()),
                        );
                        let body_type = NoExpectation(ret_type.clone());
                        let expr_con = constrain_expr(
                            env,
                            loc_body_expr.region,
                            &loc_body_expr.value,
                            body_type,
                        );

                        vars.push(*fn_var);

                        let def_con = exists(
                            vars,
                            And(vec![
                                Let(Box::new(LetConstraint {
                                    rigid_vars: Vec::new(),
                                    flex_vars: state.vars,
                                    def_types: state.headers,
                                    defs_constraint: And(state.constraints),
                                    ret_constraint: expr_con,
                                })),
                                Eq(fn_type.clone(), expected.clone(), Category::Lambda, region),
                                // "fn_var is equal to the closure's type" - fn_var is used in code gen
                                // Store type into AST vars. We use Store so errors aren't reported twice
                                Store(signature.clone(), *fn_var, std::file!(), std::line!()),
                                Store(signature, expr_var, std::file!(), std::line!()),
                                Store(ret_type, ret_var, std::file!(), std::line!()),
                                closure_constraint,
                            ]),
                        );

                        rigid_info.vars.extend(&new_rigids);

                        rigid_info.constraints.push(Let(Box::new(LetConstraint {
                            rigid_vars: new_rigids,
                            flex_vars: def_pattern_state.vars,
                            def_types: SendMap::default(), // no headers introduced (at this level)
                            defs_constraint: def_con,
                            ret_constraint: True,
                        })));
                        rigid_info.def_types.extend(def_pattern_state.headers);
                    }
                    _ => {
                        let expected = annotation_expected;

                        let ret_constraint =
                            constrain_expr(env, def.loc_expr.region, &def.loc_expr.value, expected);

                        let def_con = And(vec![
                            Let(Box::new(LetConstraint {
                                rigid_vars: Vec::new(),
                                flex_vars: vec![],
                                def_types: SendMap::default(),
                                defs_constraint: True,
                                ret_constraint,
                            })),
                            // Store type into AST vars. We use Store so errors aren't reported twice
                            Store(signature, expr_var, std::file!(), std::line!()),
                        ]);

                        rigid_info.vars.extend(&new_rigids);

                        rigid_info.constraints.push(Let(Box::new(LetConstraint {
                            rigid_vars: new_rigids,
                            flex_vars: def_pattern_state.vars,
                            def_types: SendMap::default(), // no headers introduced (at this level)
                            defs_constraint: def_con,
                            ret_constraint: True,
                        })));
                        rigid_info.def_types.extend(def_pattern_state.headers);
                    }
                }
            }
        }
    }

    Let(Box::new(LetConstraint {
        rigid_vars: rigid_info.vars,
        flex_vars: Vec::new(),
        def_types: rigid_info.def_types,
        defs_constraint: True,
        ret_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: flex_info.vars,
            def_types: flex_info.def_types.clone(),
            defs_constraint: Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: Vec::new(),
                def_types: flex_info.def_types,
                defs_constraint: True,
                ret_constraint: And(flex_info.constraints),
            })),
            ret_constraint: And(vec![And(rigid_info.constraints), body_con]),
        })),
    }))
}

#[inline(always)]
fn constrain_field_update(
    env: &Env,
    var: Variable,
    region: Region,
    field: Lowercase,
    loc_expr: &Loc<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = Type::Variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = ForReason(reason, field_type.clone(), region);
    let con = constrain_expr(env, loc_expr.region, &loc_expr.value, expected);

    (var, field_type, con)
}
