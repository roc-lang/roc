use crate::can::def::{Declaration, Def};
use crate::can::expr::Expr::{self, *};
use crate::can::expr::Field;
use crate::can::ident::{Lowercase, TagName};
use crate::can::pattern::Pattern;
use crate::collections::{ImMap, SendMap};
use crate::constrain::builtins::{
    empty_list_type, float_literal, int_literal, list_type, str_type,
};
use crate::constrain::pattern::{constrain_pattern, PatternState};
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::subs::Variable;
use crate::types::Alias;
use crate::types::AnnotationSource::{self, *};
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::PReason;
use crate::types::Type::{self, *};
use crate::types::{LetConstraint, PExpected, Reason};

/// This is for constraining Defs
#[derive(Default, Debug)]
pub struct Info {
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub def_types: SendMap<Symbol, Located<Type>>,
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
        def_aliases: SendMap::default(),
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

#[inline(always)]
pub fn exists_with_aliases(
    aliases: SendMap<Symbol, Alias>,
    flex_vars: Vec<Variable>,
    constraint: Constraint,
) -> Constraint {
    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars,
        def_types: SendMap::default(),
        def_aliases: aliases,
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

pub fn constrain_expr(
    env: &Env,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Constraint {
    match expr {
        Int(var, _) => int_literal(*var, expected, region),
        Float(var, _) => float_literal(*var, expected, region),
        EmptyRecord => constrain_empty_record(region, expected),
        Expr::Record(stored_var, fields) => {
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
                    field_types.insert(label.clone(), field_type);

                    constraints.push(field_con);
                }

                let record_type = Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(Type::EmptyRec),
                );
                let record_con = Eq(record_type, expected.clone(), region);
                constraints.push(record_con);

                // variable to store in the AST
                let stored_con = Eq(Type::Variable(*stored_var), expected, region);

                field_vars.push(*stored_var);
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
            let mut fields: SendMap<Lowercase, Type> = SendMap::default();
            let mut vars = Vec::with_capacity(updates.len() + 2);
            let mut cons = Vec::with_capacity(updates.len() + 1);
            for (field_name, Field { var, loc_expr, .. }) in updates.clone() {
                let (var, tipe, con) =
                    constrain_field_update(env, var, region, field_name.clone(), &loc_expr);
                fields.insert(field_name, tipe);
                vars.push(var);
                cons.push(con);
            }

            let fields_type = Type::Record(fields.clone(), Box::new(Type::Variable(*ext_var)));
            let record_type = Type::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = Eq(record_type.clone(), NoExpectation(fields_type), region);
            let record_con = Eq(record_type.clone(), expected, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let con = Lookup(
                *symbol,
                ForReason(
                    Reason::RecordUpdateKeys(*symbol, fields),
                    record_type,
                    region,
                ),
                region,
            );

            cons.push(con);
            cons.push(fields_con);
            cons.push(record_con);

            exists(vars, And(cons))
        }
        Str(_) | BlockStr(_) => Eq(str_type(), expected, region),
        List {
            entry_var,
            loc_elems,
            ..
        } => {
            if loc_elems.is_empty() {
                exists(
                    vec![*entry_var],
                    Eq(empty_list_type(*entry_var), expected, region),
                )
            } else {
                let list_elem_type = Type::Variable(*entry_var);
                let mut constraints = Vec::with_capacity(1 + loc_elems.len());

                for loc_elem in loc_elems {
                    let elem_expected =
                        ForReason(Reason::ElemInList, list_elem_type.clone(), region);
                    let constraint =
                        constrain_expr(env, loc_elem.region, &loc_elem.value, elem_expected);

                    constraints.push(constraint);
                }

                constraints.push(Eq(list_type(list_elem_type), expected, region));

                exists(vec![*entry_var], And(constraints))
            }
        }
        Call(boxed, loc_args, _application_style) => {
            let (fn_var, loc_fn, ret_var) = &**boxed;
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_type = Variable(*fn_var);
            let fn_region = loc_fn.region;
            let fn_expected = NoExpectation(fn_type.clone());
            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall {
                arity: loc_args.len() as u8,
            };

            let fn_con = constrain_expr(env, loc_fn.region, &loc_fn.value, fn_expected);

            // The function's return type
            let ret_type = Variable(*ret_var);

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(*fn_var);
            vars.push(*ret_var);

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_type = Variable(*arg_var);
                // TODO look up the name and use NamedFnArg if possible.
                let reason = Reason::AnonymousFnArg {
                    arg_index: index as u8,
                };
                let expected_arg = ForReason(reason, arg_type.clone(), region);
                let arg_con = constrain_expr(env, loc_arg.region, &loc_arg.value, expected_arg);

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = ForReason(
                fn_reason,
                Function(arg_types, Box::new(ret_type.clone())),
                region,
            );

            exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(fn_type, expected_fn_type, fn_region),
                    And(arg_cons),
                    Eq(ret_type, expected, region),
                ]),
            )
        }
        Var(symbol) => Lookup(*symbol, expected, region),
        Closure(fn_var, _symbol, _recursive, args, boxed) => {
            let (loc_body_expr, ret_var) = &**boxed;
            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(args.len()),
                constraints: Vec::with_capacity(1),
            };
            let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
            let mut pattern_types = Vec::with_capacity(state.vars.capacity());
            let ret_var = *ret_var;
            let ret_type = Type::Variable(ret_var);

            vars.push(ret_var);

            for (pattern_var, loc_pattern) in args {
                let pattern_type = Type::Variable(*pattern_var);
                let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

                pattern_types.push(pattern_type);

                constrain_pattern(
                    &loc_pattern.value,
                    loc_pattern.region,
                    pattern_expected,
                    &mut state,
                );

                vars.push(*pattern_var);
            }

            let fn_type = Type::Function(pattern_types, Box::new(ret_type.clone()));
            let body_type = NoExpectation(ret_type);
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
                        def_aliases: SendMap::default(),
                        defs_constraint,
                        ret_constraint,
                    })),
                    // "the closure's type is equal to expected type"
                    Eq(fn_type.clone(), expected, region),
                    // "fn_var is equal to the closure's type" - fn_var is used in code gen
                    Eq(Type::Variable(*fn_var), NoExpectation(fn_type), region),
                ]),
            )
        }

        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            // TODO use Bool alias here, so we don't allocate this type every time
            let bool_type = Type::TagUnion(
                vec![
                    (TagName::Global("True".into()), vec![]),
                    (TagName::Global("False".into()), vec![]),
                ],
                Box::new(Type::EmptyTagUnion),
            );
            let expect_bool = Expected::ForReason(Reason::IfCondition, bool_type, region);
            let mut branch_cons = Vec::with_capacity(2 * branches.len() + 2);

            match expected {
                FromAnnotation(name, arity, _, tipe) => {
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_con = Eq(
                            Type::Variable(*cond_var),
                            expect_bool.clone(),
                            loc_cond.region,
                        );
                        let then_con = constrain_expr(
                            env,
                            loc_body.region,
                            &loc_body.value,
                            FromAnnotation(
                                name.clone(),
                                arity,
                                AnnotationSource::TypedIfBranch(index + 1),
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
                            AnnotationSource::TypedIfBranch(branches.len() + 1),
                            tipe.clone(),
                        ),
                    );

                    let ast_con = Eq(Type::Variable(*branch_var), NoExpectation(tipe), region);

                    branch_cons.push(ast_con);
                    branch_cons.push(else_con);

                    exists(vec![*cond_var, *branch_var], And(branch_cons))
                }
                _ => {
                    for (index, (loc_cond, loc_body)) in branches.iter().enumerate() {
                        let cond_con = Eq(
                            Type::Variable(*cond_var),
                            expect_bool.clone(),
                            loc_cond.region,
                        );
                        let then_con = constrain_expr(
                            env,
                            loc_body.region,
                            &loc_body.value,
                            ForReason(
                                Reason::IfBranch { index: index + 1 },
                                Type::Variable(*branch_var),
                                region,
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
                                index: branches.len() + 1,
                            },
                            Type::Variable(*branch_var),
                            region,
                        ),
                    );

                    branch_cons.push(Eq(Type::Variable(*branch_var), expected, region));
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
                FromAnnotation(name, arity, _, typ) => {
                    // record the  type of the whole expression in the AST
                    let ast_con = Eq(Type::Variable(*expr_var), expected.clone(), region);
                    constraints.push(ast_con);

                    for (index, (loc_when_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = constrain_when_branch(
                            env,
                            region,
                            &loc_when_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            FromAnnotation(
                                name.clone(),
                                *arity,
                                TypedWhenBranch(index),
                                typ.clone(),
                            ),
                        );

                        constraints.push(branch_con);
                    }
                }

                _ => {
                    let branch_type = Variable(*expr_var);
                    let mut branch_cons = Vec::with_capacity(branches.len());

                    for (index, (loc_when_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = constrain_when_branch(
                            env,
                            region,
                            &loc_when_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            ForReason(Reason::WhenBranch { index }, branch_type.clone(), region),
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
                        Eq(branch_type, expected, region),
                    ]));
                }
            }

            // TODO check for exhaustiveness. If this `case` is non-exaustive, then:
            //
            // 1. Record a Problem.
            // 2. Add an extra _ branch at the end which throws a runtime error.

            exists(vec![cond_var, *expr_var], And(constraints))
        }
        Access {
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
            rec_field_types.insert(label, field_type.clone());

            let record_type = Type::Record(rec_field_types, Box::new(ext_type));
            let record_expected = Expected::NoExpectation(record_type);

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
                vec![field_var, ext_var],
                And(vec![constraint, Eq(field_type, expected, region)]),
            )
        }
        Accessor {
            field,
            ext_var,
            field_var,
        } => {
            let ext_var = *ext_var;
            let ext_type = Variable(ext_var);
            let field_var = *field_var;
            let field_type = Variable(field_var);

            let mut field_types = SendMap::default();
            let label = field.clone();
            field_types.insert(label, field_type.clone());
            let record_type = Type::Record(field_types, Box::new(ext_type));

            exists(
                vec![field_var, ext_var],
                Eq(
                    Type::Function(vec![record_type], Box::new(field_type)),
                    expected,
                    region,
                ),
            )
        }
        LetRec(defs, loc_ret, var, aliases) => {
            let body_con = constrain_expr(env, loc_ret.region, &loc_ret.value, expected.clone());

            exists_with_aliases(
                aliases.clone(),
                vec![*var],
                And(vec![
                    constrain_recursive_defs(env, defs, body_con),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(Type::Variable(*var), expected, loc_ret.region),
                ]),
            )
        }
        LetNonRec(def, loc_ret, var, aliases) => {
            let body_con = constrain_expr(env, loc_ret.region, &loc_ret.value, expected.clone());

            exists_with_aliases(
                aliases.clone(),
                vec![*var],
                And(vec![
                    constrain_def(env, def, body_con),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(Type::Variable(*var), expected, loc_ret.region),
                ]),
            )
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
                region,
            );
            let ast_con = Eq(Type::Variable(*variant_var), expected, region);

            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }
        RuntimeError(_) => True,
    }
}

#[inline(always)]
fn constrain_when_branch(
    env: &Env,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
) -> Constraint {
    let ret_constraint = constrain_expr(env, region, &loc_expr.value, expr_expected);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    constrain_pattern(
        &loc_pattern.value,
        loc_pattern.region,
        pattern_expected,
        &mut state,
    );

    Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: state.vars,
        def_types: state.headers,
        def_aliases: SendMap::default(),
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint,
    }))
}

fn constrain_field(env: &Env, field_var: Variable, loc_expr: &Located<Expr>) -> (Type, Constraint) {
    let field_type = Variable(field_var);
    let field_expected = NoExpectation(field_type.clone());
    let constraint = constrain_expr(env, loc_expr.region, &loc_expr.value, field_expected);

    (field_type, constraint)
}

#[inline(always)]
fn constrain_empty_record(region: Region, expected: Expected<Type>) -> Constraint {
    Eq(EmptyRec, expected, region)
}

#[inline(always)]
pub fn constrain_decls(home: ModuleId, decls: &[Declaration]) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;
    for decl in decls.iter().rev() {
        // NOTE: rigids are empty because they are not shared between top-level definitions
        match decl {
            Declaration::Declare(def) => {
                constraint = constrain_def(
                    &Env {
                        home,
                        rigids: ImMap::default(),
                    },
                    def,
                    constraint,
                );
            }
            Declaration::DeclareRec(defs) => {
                constraint = constrain_recursive_defs(
                    &Env {
                        home,
                        rigids: ImMap::default(),
                    },
                    defs,
                    constraint,
                );
            }
            Declaration::InvalidCycle(_, _) => panic!("TODO handle invalid cycle"),
        }
    }

    constraint
}

fn constrain_def_pattern(loc_pattern: &Located<Pattern>, expr_type: Type) -> PatternState {
    let pattern_expected = PExpected::NoExpectation(expr_type);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    constrain_pattern(
        &loc_pattern.value,
        loc_pattern.region,
        pattern_expected,
        &mut state,
    );

    state
}

fn constrain_def(env: &Env, def: &Def, body_con: Constraint) -> Constraint {
    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut pattern_state = constrain_def_pattern(&def.loc_pattern, expr_type.clone());

    pattern_state.vars.push(expr_var);

    let mut def_aliases = SendMap::default();
    let mut new_rigids = Vec::new();

    let expr_con = match &def.annotation {
        Some((annotation, free_vars, ann_def_aliases)) => {
            def_aliases = ann_def_aliases.clone();

            let arity = annotation.arity();
            let rigids = &env.rigids;
            let mut ftv = rigids.clone();

            let annotation = instantiate_rigids(
                annotation,
                &free_vars,
                &mut new_rigids,
                &mut ftv,
                &def.loc_pattern,
                &mut pattern_state.headers,
            );

            let annotation_expected = FromAnnotation(
                def.loc_pattern.clone(),
                arity,
                AnnotationSource::TypedBody,
                annotation,
            );

            pattern_state.constraints.push(Eq(
                expr_type,
                annotation_expected.clone(),
                // TODO proper region
                Region::zero(),
            ));

            constrain_expr(
                &Env {
                    home: env.home,
                    rigids: ftv,
                },
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            )
        }
        None => constrain_expr(
            env,
            def.loc_expr.region,
            &def.loc_expr.value,
            NoExpectation(expr_type),
        ),
    };

    Let(Box::new(LetConstraint {
        rigid_vars: new_rigids,
        flex_vars: pattern_state.vars,
        def_types: pattern_state.headers,
        def_aliases,
        defs_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),        // always empty
            flex_vars: Vec::new(),         // empty, because our functions have no arguments
            def_types: SendMap::default(), // empty, because our functions have no arguments!
            def_aliases: SendMap::default(),
            defs_constraint: And(pattern_state.constraints),
            ret_constraint: expr_con,
        })),
        ret_constraint: body_con,
    }))
}

fn instantiate_rigids(
    annotation: &Type,
    free_vars: &SendMap<Lowercase, Variable>,
    new_rigids: &mut Vec<Variable>,
    ftv: &mut ImMap<Lowercase, Variable>,
    loc_pattern: &Located<Pattern>,
    headers: &mut SendMap<Symbol, Located<Type>>,
) -> Type {
    let mut annotation = annotation.clone();
    let mut rigid_substitution: ImMap<Variable, Type> = ImMap::default();

    for (name, var) in free_vars {
        if let Some(existing_rigid) = ftv.get(name) {
            rigid_substitution.insert(*var, Type::Variable(*existing_rigid));
        } else {
            // It's possible to use this rigid in nested defs
            ftv.insert(name.clone(), *var);

            new_rigids.push(*var);
        }
    }

    // Instantiate rigid variables
    if !rigid_substitution.is_empty() {
        annotation.substitute(&rigid_substitution);
    }

    if let Some(new_headers) = crate::constrain::pattern::headers_from_annotation(
        &loc_pattern.value,
        &Located::at(loc_pattern.region, annotation.clone()),
    ) {
        for (symbol, loc_type) in new_headers {
            new_rigids.extend(loc_type.value.variables());
            headers.insert(symbol, loc_type);
        }
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
    let mut def_aliases = SendMap::default();
    for def in defs {
        let expr_var = def.expr_var;
        let expr_type = Type::Variable(expr_var);

        let pattern_expected = PExpected::NoExpectation(expr_type.clone());

        let mut pattern_state = PatternState {
            headers: SendMap::default(),
            vars: flex_info.vars.clone(),
            constraints: Vec::with_capacity(1),
        };

        constrain_pattern(
            &def.loc_pattern.value,
            def.loc_pattern.region,
            pattern_expected,
            &mut pattern_state,
        );

        pattern_state.vars.push(expr_var);

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
                    def_aliases: SendMap::default(),
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                flex_info.vars = pattern_state.vars;
                flex_info.constraints.push(def_con);
                flex_info.def_types.extend(pattern_state.headers);
            }

            Some((annotation, free_vars, ann_def_aliases)) => {
                for (symbol, alias) in ann_def_aliases.clone() {
                    def_aliases.insert(symbol, alias);
                }

                let arity = annotation.arity();
                let mut ftv = env.rigids.clone();

                let annotation = instantiate_rigids(
                    annotation,
                    &free_vars,
                    &mut new_rigids,
                    &mut ftv,
                    &def.loc_pattern,
                    &mut pattern_state.headers,
                );

                let annotation_expected = FromAnnotation(
                    def.loc_pattern.clone(),
                    arity,
                    AnnotationSource::TypedBody,
                    annotation.clone(),
                );
                let expr_con = constrain_expr(
                    &Env {
                        rigids: ftv,
                        home: env.home,
                    },
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    NoExpectation(expr_type.clone()),
                );

                // ensure expected type unifies with annotated type
                rigid_info.constraints.push(Eq(
                    expr_type,
                    annotation_expected.clone(),
                    def.loc_expr.region,
                ));

                // TODO investigate if this let can be safely removed
                let def_con = Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: Vec::new(), // empty because Roc function defs have no args
                    def_types: SendMap::default(), // empty because Roc function defs have no args
                    def_aliases: SendMap::default(),
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                rigid_info.vars.extend(&new_rigids);
                // because of how in Roc headers point to variables, we must include the pattern var here
                rigid_info.vars.extend(pattern_state.vars);
                rigid_info.constraints.push(Let(Box::new(LetConstraint {
                    rigid_vars: new_rigids,
                    flex_vars: Vec::new(),         // no flex vars introduced
                    def_types: SendMap::default(), // no headers introduced (at this level)
                    def_aliases: SendMap::default(),
                    defs_constraint: def_con,
                    ret_constraint: True,
                })));
                rigid_info.def_types.extend(pattern_state.headers);
            }
        }
    }

    Let(Box::new(LetConstraint {
        rigid_vars: rigid_info.vars,
        flex_vars: Vec::new(),
        def_types: rigid_info.def_types,
        def_aliases,
        defs_constraint: True,
        ret_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: flex_info.vars,
            def_types: flex_info.def_types.clone(),
            def_aliases: SendMap::default(),
            defs_constraint: Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: Vec::new(),
                def_types: flex_info.def_types,
                def_aliases: SendMap::default(),
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
    loc_expr: &Located<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = Type::Variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = ForReason(reason, field_type.clone(), region);
    let con = constrain_expr(env, loc_expr.region, &loc_expr.value, expected);

    (var, field_type, con)
}
