use crate::can::def::Declaration;
use crate::can::def::Def;
use crate::can::expr::Expr::{self, *};
use crate::can::expr::Field;
use crate::can::ident::Lowercase;
use crate::can::pattern::Pattern;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::constrain::builtins::{
    empty_list_type, float_literal, int_literal, list_type, str_type,
};
use crate::constrain::pattern::{constrain_pattern, PatternState};
use crate::region::{Located, Region};
use crate::subs::Variable;
use crate::types::AnnotationSource::*;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::PReason;
use crate::types::Type::{self, *};
use crate::types::{LetConstraint, PExpected, Reason};

/// Whenever we encounter a user-defined type variable (a "rigid" var for short),
/// for example `a` in the annotation `identity : a -> a`, we add it to this
/// map so that expressions within that annotation can share these vars.
pub type Rigids = ImMap<Lowercase, Type>;

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
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

pub fn constrain_expr(
    rigids: &Rigids,
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
                    let (field_type, field_con) =
                        constrain_field(rigids, field_var, &*loc_field_expr);

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
            ident,
            symbol,
            updates,
        } => {
            let mut fields: SendMap<Lowercase, Type> = SendMap::default();
            let mut vars = Vec::with_capacity(updates.len() + 2);
            let mut cons = Vec::with_capacity(updates.len() + 1);
            for (field_name, Field { var, loc_expr, .. }) in updates.clone() {
                let (var, tipe, con) =
                    constrain_field_update(rigids, var, region, field_name.clone(), &loc_expr);
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
                symbol.clone(),
                ForReason(
                    Reason::RecordUpdateKeys(ident.clone(), fields),
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
        List(list_var, loc_elems) => {
            if loc_elems.is_empty() {
                Eq(empty_list_type(*list_var), expected, region)
            } else {
                let list_elem_type = Type::Variable(*list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));

                for (elem_var, loc_elem) in loc_elems {
                    let elem_type = Variable(*elem_var);
                    let elem_expected = NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_elem_type.clone(),
                        ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let constraint =
                        constrain_expr(rigids, loc_elem.region, &loc_elem.value, elem_expected);

                    constraints.push(list_elem_constraint);
                    constraints.push(constraint);
                }

                constraints.push(Eq(list_type(list_elem_type), expected, region));

                And(constraints)
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

            let fn_con = constrain_expr(rigids, loc_fn.region, &loc_fn.value, fn_expected);

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
                let arg_con = constrain_expr(rigids, loc_arg.region, &loc_arg.value, expected_arg);

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
        Var {
            symbol_for_lookup, ..
        } => Lookup(symbol_for_lookup.clone(), expected, region),
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
            let ret_constraint = constrain_expr(
                rigids,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

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
                    // "the closure's type is equal to expected type"
                    Eq(fn_type.clone(), expected, region),
                    // "fn_var is equal to the closure's type" - fn_var is used in code gen
                    Eq(Type::Variable(*fn_var), NoExpectation(fn_type), region),
                ]),
            )
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
                rigids,
                region,
                &loc_cond.value,
                NoExpectation(cond_type.clone()),
            );

            let mut constraints = Vec::with_capacity(branches.len() + 1);

            match expected {
                FromAnnotation(name, arity, _, typ) => {
                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = constrain_when_branch(
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            FromAnnotation(
                                name.clone(),
                                arity,
                                TypedWhenBranch(index),
                                typ.clone(),
                            ),
                        );

                        // TODO investigate: why doesn't this use expr_var?
                        // Shouldn't it?
                        constraints.push(exists(
                            vec![cond_var],
                            // Each branch's pattern must have the same type
                            // as the condition expression did.
                            And(vec![expr_con.clone(), branch_con]),
                        ));
                    }
                }

                _ => {
                    let branch_type = Variable(*expr_var);
                    let mut branch_cons = Vec::with_capacity(branches.len());

                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = constrain_when_branch(
                            rigids,
                            region,
                            loc_pattern,
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

                    constraints.push(exists(
                        vec![cond_var],
                        And(vec![
                            // Record the original conditional expression's constraint.
                            expr_con,
                            // Each branch's pattern must have the same type
                            // as the condition expression did.
                            And(branch_cons),
                            // The return type of each branch must equal
                            // the return type of the entire when-expression.
                            Eq(branch_type, expected, region),
                        ]),
                    ));
                }
            }

            // TODO check for exhaustiveness. If this `case` is non-exaustive, then:
            //
            // 1. Record a Problem.
            // 2. Add an extra _ branch at the end which throws a runtime error.

            And(constraints)
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

            let constraint =
                constrain_expr(&ImMap::default(), region, &loc_expr.value, record_expected);

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
        LetRec(defs, loc_ret, var) => {
            let body_con = constrain_expr(rigids, loc_ret.region, &loc_ret.value, expected.clone());
            And(vec![
                constrain_recursive_defs(rigids, defs, body_con),
                // Record the type of tne entire def-expression in the variable.
                // Code gen will need that later!
                Eq(Type::Variable(*var), expected, loc_ret.region),
            ])
        }
        LetNonRec(def, loc_ret, var) => {
            let body_con = constrain_expr(rigids, loc_ret.region, &loc_ret.value, expected.clone());

            And(vec![
                constrain_def(rigids, def, body_con),
                // Record the type of tne entire def-expression in the variable.
                // Code gen will need that later!
                Eq(Type::Variable(*var), expected, loc_ret.region),
            ])
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
                    rigids,
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
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }
        RuntimeError(_) => True,
    }
}

#[inline(always)]
fn constrain_when_branch(
    rigids: &Rigids,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
) -> Constraint {
    let ret_constraint = constrain_expr(rigids, region, &loc_expr.value, expr_expected);

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
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint,
    }))
}

fn constrain_field(
    rigids: &Rigids,
    field_var: Variable,
    loc_expr: &Located<Expr>,
) -> (Type, Constraint) {
    let field_type = Variable(field_var);
    let field_expected = NoExpectation(field_type.clone());
    let constraint = constrain_expr(rigids, loc_expr.region, &loc_expr.value, field_expected);

    (field_type, constraint)
}

#[inline(always)]
fn constrain_empty_record(region: Region, expected: Expected<Type>) -> Constraint {
    Eq(EmptyRec, expected, region)
}

#[inline(always)]
pub fn constrain_decls(decls: &[Declaration]) -> Constraint {
    let mut constraint = Constraint::SaveTheEnvironment;
    for decl in decls.iter().rev() {
        // NOTE: rigids are empty because they are not shared between top-level definitions
        match decl {
            Declaration::Declare(def) => {
                constraint = constrain_def(&ImMap::default(), def, constraint);
            }
            Declaration::DeclareRec(defs) => {
                constraint = constrain_recursive_defs(&ImMap::default(), defs, constraint);
            }
            Declaration::InvalidCycle(_, _) => panic!("TODO handle invalid cycle"),
        }
    }

    constraint
}

fn constrain_def_pattern(loc_pattern: &Located<Pattern>, expr_type: Type) -> PatternState {
    // Exclude the current ident from shadowable_idents; you can't shadow yourself!
    // (However, still include it in scope, because you *can* recursively refer to yourself.)
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

pub fn constrain_def(rigids: &Rigids, def: &Def, body_con: Constraint) -> Constraint {
    use crate::types::AnnotationSource;

    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut pattern_state = constrain_def_pattern(&def.loc_pattern, expr_type.clone());

    pattern_state.vars.push(expr_var);

    let mut new_rigids = Vec::new();

    let expr_con = match &def.annotation {
        Some((annotation, free_vars)) => {
            let mut ftv: Rigids = rigids.clone();

            for (var, name) in free_vars {
                // if the rigid is known already, nothing needs to happen
                // otherwise register it.
                if !rigids.contains_key(name) {
                    // possible use this rigid in nested def's
                    ftv.insert(name.clone(), Type::Variable(*var));

                    new_rigids.push(*var);
                }
            }

            let annotation_expected = FromAnnotation(
                def.loc_pattern.clone(),
                annotation.arity(),
                AnnotationSource::TypedBody,
                annotation.clone(),
            );

            pattern_state.constraints.push(Eq(
                expr_type,
                annotation_expected.clone(),
                Region::zero(),
            ));

            constrain_expr(
                &ftv,
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            )
        }
        None => constrain_expr(
            rigids,
            def.loc_expr.region,
            &def.loc_expr.value,
            NoExpectation(expr_type),
        ),
    };

    Let(Box::new(LetConstraint {
        rigid_vars: new_rigids,
        flex_vars: pattern_state.vars,
        def_types: pattern_state.headers,
        defs_constraint: Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),        // always empty
            flex_vars: Vec::new(),         // empty, because our functions have no arguments
            def_types: SendMap::default(), // empty, because our functions have no arguments!
            defs_constraint: And(pattern_state.constraints),
            ret_constraint: expr_con,
        })),
        ret_constraint: body_con,
    }))
}

fn constrain_recursive_defs(rigids: &Rigids, defs: &[Def], body_con: Constraint) -> Constraint {
    rec_defs_help(
        rigids,
        defs,
        body_con,
        Info::with_capacity(defs.len()),
        Info::with_capacity(defs.len()),
    )
}

pub fn rec_defs_help(
    rigids: &Rigids,
    defs: &[Def],
    body_con: Constraint,
    mut rigid_info: Info,
    mut flex_info: Info,
) -> Constraint {
    use crate::types::AnnotationSource;
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
                    rigids,
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

                flex_info.vars = pattern_state.vars;
                flex_info.constraints.push(def_con);
                flex_info.def_types.extend(pattern_state.headers);
            }

            Some((annotation, seen_rigids)) => {
                let mut ftv: Rigids = rigids.clone();

                for (var, name) in seen_rigids {
                    // if the rigid is known already, nothing needs to happen
                    // otherwise register it.
                    if !rigids.contains_key(name) {
                        // possible use this rigid in nested def's
                        ftv.insert(name.clone(), Type::Variable(*var));

                        new_rigids.push(*var);
                    }
                }

                let annotation_expected = FromAnnotation(
                    def.loc_pattern.clone(),
                    annotation.arity(),
                    AnnotationSource::TypedBody,
                    annotation.clone(),
                );
                let expr_con = constrain_expr(
                    &ftv,
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
                    defs_constraint: True, // I think this is correct, once again because there are no args
                    ret_constraint: expr_con,
                }));

                rigid_info.vars.extend(&new_rigids);
                rigid_info.constraints.push(Let(Box::new(LetConstraint {
                    rigid_vars: new_rigids,
                    flex_vars: Vec::new(),         // no flex vars introduced
                    def_types: SendMap::default(), // no headers introduced (at this level)
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
    rigids: &Rigids,
    var: Variable,
    region: Region,
    field: Lowercase,
    loc_expr: &Located<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = Type::Variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = ForReason(reason, field_type.clone(), region);
    let con = constrain_expr(rigids, loc_expr.region, &loc_expr.value, expected);

    (var, field_type, con)
}
