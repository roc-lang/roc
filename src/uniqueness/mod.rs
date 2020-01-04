use crate::can::def::Def;
use crate::can::expr::Expr;
use crate::can::expr::Output;
use crate::can::pattern;
use crate::can::pattern::{Pattern, RecordDestruct};
use crate::can::procedure::{Procedure, References};
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::constrain::expr::{Info, Rigids};
use crate::ident::Ident;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::AnnotationSource::TypedWhenBranch;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self};
use crate::types::Fields;
use crate::types::LetConstraint;
use crate::types::PExpected::{self};
use crate::types::PReason::{self};
use crate::types::Reason;
use crate::types::Type::{self, *};
use crate::uniqueness::constrain::exists;
use crate::uniqueness::sharing::VarUsage;

pub use crate::can::expr::Expr::*;

pub mod boolean_algebra;
mod constrain;
pub mod sharing;

pub struct Env {
    pub bound_names: ImMap<Symbol, Variable>,
    pub procedures: ImMap<Symbol, Procedure>,
}

pub fn canonicalize_declaration(
    var_store: &VarStore,
    region: Region,
    loc_expr: Located<Expr>,
    _declared_idents: &ImMap<Ident, (Symbol, Region)>,
    expected: Expected<Type>,
) -> (Output, Constraint) {
    let rigids = ImMap::default();
    let mut var_usage = VarUsage::default();

    canonicalize_expr(
        &rigids,
        var_store,
        &mut var_usage,
        region,
        &loc_expr.value,
        expected,
    )
}

pub struct PatternState {
    pub headers: SendMap<Symbol, Located<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
}

fn canonicalize_pattern(
    var_store: &VarStore,
    state: &mut PatternState,
    pattern: &Located<Pattern>,
    expected: PExpected<Type>,
) {
    use crate::can::pattern::Pattern::*;
    use crate::types::PatternCategory;

    match &pattern.value {
        Identifier(symbol) => {
            state.headers.insert(
                symbol.clone(),
                Located {
                    region: pattern.region,
                    value: expected.get_type(),
                },
            );
        }

        IntLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Int,
                Type::int(),
                expected,
            ));
        }
        FloatLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Float,
                Type::float(),
                expected,
            ));
        }

        StrLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Str,
                Type::string(),
                expected,
            ));
        }

        RecordDestructure(ext_var, patterns) => {
            state.vars.push(*ext_var);
            let ext_type = Type::Variable(*ext_var);

            let mut field_types = Fields::default();
            for RecordDestruct {
                var,
                label,
                symbol,
                guard,
            } in patterns
            {
                let pat_type = Type::Variable(*var);
                let pattern_expected = PExpected::NoExpectation(pat_type.clone());

                match guard {
                    Some((_guard_var, loc_guard)) => {
                        state.headers.insert(
                            symbol.clone(),
                            Located {
                                region: pattern.region,
                                value: pat_type.clone(),
                            },
                        );

                        canonicalize_pattern(var_store, state, loc_guard, pattern_expected);
                    }
                    None => {
                        canonicalize_pattern(var_store, state, pattern, pattern_expected);
                    }
                }

                state.vars.push(*var);
                field_types.required.insert(label.clone(), pat_type);
            }

            let record_type =
                constrain::lift(var_store, Type::Record(field_types, Box::new(ext_type)));
            let record_con = Constraint::Pattern(
                pattern.region,
                PatternCategory::Record,
                record_type,
                expected,
            );

            state.constraints.push(record_con);
        }

        Tag(_) | AppliedTag(_, _) => {
            panic!("TODO add_constraints for {:?}", pattern);
        }

        Underscore | Shadowed(_) | UnsupportedPattern(_) => {
            // no constraints
        }
    }
}

pub fn canonicalize_expr(
    rigids: &Rigids,
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> (Output, Constraint) {
    pub use crate::can::expr::Expr::*;

    match expr {
        Int(_, _) => {
            let constraint = constrain::int_literal(var_store, expected, region);
            (Output::default(), constraint)
        }
        Float(_, _) => {
            let constraint = constrain::float_literal(var_store, expected, region);
            (Output::default(), constraint)
        }
        BlockStr(_) | Str(_) => {
            let inferred = constrain::lift(var_store, constrain::str_type());
            let constraint = Eq(inferred, expected, region);
            (Output::default(), constraint)
        }
        EmptyRecord => {
            let constraint = Eq(constrain::lift(var_store, EmptyRec), expected, region);

            (Output::default(), constraint)
        }
        Record(variable, fields) => {
            // NOTE: canonicalization guarantees at least one field
            // zero fields generates an EmptyRecord
            let mut field_types = Fields::default();
            let mut field_vars = Vec::with_capacity(fields.len());

            // Constraints need capacity for each field + 1 for the record itself.
            let mut constraints = Vec::with_capacity(1 + fields.len());
            let mut output = Output::default();

            for (label, (_, loc_expr)) in fields.iter() {
                let field_var = var_store.fresh();
                let field_type = Variable(field_var);
                let field_expected = Expected::NoExpectation(field_type.clone());
                let (field_out, field_con) = canonicalize_expr(
                    rigids,
                    var_store,
                    var_usage,
                    loc_expr.region,
                    &loc_expr.value,
                    field_expected,
                );

                field_vars.push(field_var);
                field_types.required.insert(label.clone(), field_type);

                constraints.push(field_con);
                output.references = output.references.union(field_out.references);
            }

            let record_type = constrain::lift(
                var_store,
                Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(Type::EmptyRec),
                ),
            );
            let record_con = Eq(record_type, expected.clone(), region);
            let ext_con = Eq(Type::Variable(*variable), expected, region);

            constraints.push(record_con);
            constraints.push(ext_con);

            let constraint = exists(field_vars, And(constraints));

            (output, constraint)
        }
        Tag(name, arguments) => {
            panic!("TODO implement tag {:?} {:?}", name, arguments);
        }
        List(variable, loc_elems) => {
            if loc_elems.is_empty() {
                let list_var = *variable;
                let inferred = constrain::lift(var_store, constrain::empty_list_type(list_var));
                let constraint = Eq(inferred, expected, region);
                (Output::default(), constraint)
            } else {
                // constrain `expected ~ List a` and that all elements `~ a`.
                let list_var = *variable; // `v` in the type (List v)
                let list_type = Type::Variable(list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));
                let mut references = References::new();

                for (elem_var, loc_elem) in loc_elems.iter() {
                    let elem_type = Variable(*elem_var);
                    let elem_expected = Expected::NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_type.clone(),
                        Expected::ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let (elem_out, constraint) = canonicalize_expr(
                        rigids,
                        var_store,
                        var_usage,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(list_elem_constraint);
                    constraints.push(constraint);

                    references = references.union(elem_out.references);
                }
                let inferred = constrain::lift(var_store, constrain::list_type(list_type));
                constraints.push(Eq(inferred, expected, region));

                let mut output = Output::default();

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                (output, And(constraints))
            }
        }
        Var {
            symbol_for_lookup, ..
        } => {
            var_usage.register(symbol_for_lookup);
            match var_usage.get_usage(symbol_for_lookup) {
                Some(sharing::ReferenceCount::Shared) => {
                    // the variable is used/consumed more than once, so it must be Shared
                    let val_var = var_store.fresh();
                    let uniq_var = var_store.fresh();

                    let val_type = Variable(val_var);
                    let uniq_type = Variable(uniq_var);

                    let attr_type = constrain::attr_type(uniq_type.clone(), val_type);

                    (
                        Output::default(),
                        And(vec![
                            Lookup(symbol_for_lookup.clone(), expected.clone(), region),
                            Eq(attr_type, expected, region),
                            Eq(
                                uniq_type,
                                Expected::NoExpectation(constrain::shared_type()),
                                region,
                            ),
                        ]),
                    )
                }
                Some(sharing::ReferenceCount::Unique) => {
                    // no additional constraints, keep uniqueness unbound
                    (
                        Output::default(),
                        Lookup(symbol_for_lookup.clone(), expected.clone(), region),
                    )
                }
                None => panic!("symbol not analyzed"),
            }
        }
        Closure(_symbol, _recursion, args, boxed_body) => {
            let (body, ret_var) = &**boxed_body;

            // first, generate constraints for the arguments
            let mut arg_types = Vec::new();
            let mut arg_vars = Vec::new();

            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(1),
                constraints: Vec::with_capacity(1),
            };

            let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
            let ret_type = Variable(*ret_var);

            vars.push(*ret_var);

            for (arg_var, pattern) in args {
                let arg_typ = Variable(*arg_var);
                canonicalize_pattern(
                    var_store,
                    &mut state,
                    &pattern,
                    PExpected::NoExpectation(arg_typ.clone()),
                );
                arg_types.push(arg_typ);
                arg_vars.push(arg_var);

                vars.push(*arg_var);
            }

            let fn_typ = constrain::lift(
                var_store,
                Type::Function(arg_types, Box::new(ret_type.clone())),
            );

            let (output, ret_constraint) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                region,
                &body.value,
                Expected::NoExpectation(ret_type),
            );

            // remove identifiers bound in the arguments from VarUsage
            for (_, pattern) in args {
                for identifier in pattern::symbols_from_pattern(&pattern.value) {
                    var_usage.unregister(&identifier);
                }
            }

            let defs_constraint = And(state.constraints);
            let constraint = exists(
                vars,
                And(vec![
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: state.vars,
                        def_types: state.headers,
                        defs_constraint: defs_constraint,
                        ret_constraint,
                    })),
                    // "the closure's type is equal to expected  type"
                    Eq(fn_typ, expected, region),
                ]),
            );

            (output, constraint)
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, fn_expr, ret_var) = &**boxed;
            let fn_type = Variable(*fn_var);
            let ret_type = Variable(*ret_var);
            let fn_expected = Expected::NoExpectation(fn_type.clone());
            let fn_region = fn_expr.region;

            let mut vars = Vec::with_capacity(2 + loc_args.len());

            // Canonicalize the function expression and its arguments
            let (_, fn_con) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                fn_region,
                &fn_expr.value,
                fn_expected,
            );

            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall {
                arity: loc_args.len() as u8,
            };

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_type = Variable(*arg_var);

                let reason = Reason::AnonymousFnArg {
                    arg_index: index as u8,
                };

                let expected_arg = Expected::ForReason(reason, arg_type.clone(), region);
                let (_, arg_con) = canonicalize_expr(
                    rigids,
                    var_store,
                    var_usage,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = Expected::ForReason(
                fn_reason,
                constrain::lift(var_store, Function(arg_types, Box::new(ret_type.clone()))),
                region,
            );

            (
                Output::default(),
                exists(
                    vars,
                    And(vec![
                        fn_con,
                        Eq(fn_type, expected_fn_type, fn_region),
                        And(arg_cons),
                        Eq(ret_type, expected, region),
                    ]),
                ),
            )
        }
        LetRec(defs, loc_ret, _) => {
            // NOTE doesn't currently unregister bound symbols
            // may be a problem when symbols are not globally unique
            let (_, body_con) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                loc_ret.region,
                &loc_ret.value,
                expected,
            );
            (
                Output::default(),
                constrain_recursive_defs(rigids, var_store, var_usage, defs, body_con),
            )
        }
        LetNonRec(def, loc_ret, _) => {
            // NOTE doesn't currently unregister bound symbols
            // may be a problem when symbols are not globally unique
            let (_, body_con) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                loc_ret.region,
                &loc_ret.value,
                expected,
            );

            (
                Output::default(),
                constrain_def(rigids, var_store, var_usage, def, body_con),
            )
        }
        When {
            cond_var,
            loc_cond,
            branches,
            ..
        } => {
            let cond_var = *cond_var;
            let cond_type = Variable(cond_var);
            let (mut output, expr_con) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                region,
                &loc_cond.value,
                Expected::NoExpectation(cond_type.clone()),
            );

            let mut constraints = Vec::with_capacity(branches.len() + 1);

            let old_var_usage = var_usage.clone();

            match expected {
                Expected::FromAnnotation(name, arity, _, typ) => {
                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let mut branch_var_usage = old_var_usage.clone();
                        let branch_con = canonicalize_when_branch(
                            var_store,
                            &mut branch_var_usage,
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::FromAnnotation(
                                name.clone(),
                                arity,
                                TypedWhenBranch(index),
                                typ.clone(),
                            ),
                            &mut output,
                        );

                        // required for a case like
                        //
                        // when b is
                        //      Foo x -> x + x
                        //      Bar x -> x
                        //
                        // In this case the `x` in the second branch is used uniquely
                        for symbol in pattern::symbols_from_pattern(&loc_pattern.value) {
                            branch_var_usage.unregister(&symbol);
                        }

                        var_usage.or(&branch_var_usage);

                        constraints.push(exists(
                            vec![cond_var],
                            // Each branch's pattern must have the same type
                            // as the condition expression did.
                            And(vec![expr_con.clone(), branch_con]),
                        ));
                    }
                }

                _ => {
                    let branch_var = var_store.fresh();
                    let branch_type = Variable(branch_var);
                    let mut branch_cons = Vec::with_capacity(branches.len());

                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let mut branch_var_usage = old_var_usage.clone();
                        let branch_con = canonicalize_when_branch(
                            var_store,
                            &mut branch_var_usage,
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::ForReason(
                                Reason::WhenBranch { index },
                                branch_type.clone(),
                                region,
                            ),
                            &mut output,
                        );

                        // required for a case like
                        //
                        // case b when
                        //      Foo x -> x + x
                        //      Bar x -> x
                        //
                        // In this case the `x` in the second branch is used uniquely
                        for symbol in pattern::symbols_from_pattern(&loc_pattern.value) {
                            branch_var_usage.unregister(&symbol);
                        }

                        var_usage.or(&branch_var_usage);

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
                            // the return type of the entire case-expression.
                            Eq(branch_type, expected, region),
                        ]),
                    ));
                }
            }

            (output, And(constraints))
        }

        Access {
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let ext_type = Type::Variable(*ext_var);
            let field_type = Type::Variable(*field_var);

            let mut rec_field_types = Fields::default();

            rec_field_types
                .required
                .insert(field.clone(), field_type.clone());

            let record_type =
                constrain::lift(var_store, Type::Record(rec_field_types, Box::new(ext_type)));
            let record_expected = Expected::NoExpectation(record_type);

            let (output, mut constraint) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                loc_expr.region,
                &loc_expr.value,
                record_expected,
            );

            constraint = exists(
                vec![*field_var, *ext_var],
                And(vec![constraint, Eq(field_type, expected, region)]),
            );

            (output, constraint)
        }

        Accessor {
            field,
            field_var,
            ext_var,
        } => {
            let ext_type = Variable(*ext_var);
            let field_type = Variable(*field_var);
            let mut field_types = Fields::default();

            field_types
                .required
                .insert(field.clone(), field_type.clone());

            let record_type =
                constrain::lift(var_store, Type::Record(field_types, Box::new(ext_type)));

            (
                Output::default(),
                exists(
                    vec![*field_var, *ext_var],
                    Eq(
                        Type::Function(vec![record_type], Box::new(field_type)),
                        expected,
                        region,
                    ),
                ),
            )
        }
        RuntimeError(_) => (Output::default(), True),
        // _ => panic!("{:?}", expr),
    }
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn canonicalize_when_branch(
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    rigids: &Rigids,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
    _output: &mut Output,
) -> Constraint {
    let (_, ret_constraint) = canonicalize_expr(
        rigids,
        var_store,
        var_usage,
        region,
        &loc_expr.value,
        expr_expected,
    );

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    // mutates the state, so return value is not used
    canonicalize_pattern(var_store, &mut state, &loc_pattern, pattern_expected);

    Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: state.vars,
        def_types: state.headers,
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint,
    }))
}

fn constrain_def_pattern(
    var_store: &VarStore,
    loc_pattern: &Located<Pattern>,
    expr_type: Type,
) -> PatternState {
    // Exclude the current ident from shadowable_idents; you can't shadow yourself!
    // (However, still include it in scope, because you *can* recursively refer to yourself.)
    let pattern_expected = PExpected::NoExpectation(expr_type);

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    canonicalize_pattern(var_store, &mut state, loc_pattern, pattern_expected);

    state
}

pub fn constrain_def(
    rigids: &Rigids,
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    def: &Def,
    body_con: Constraint,
) -> Constraint {
    use crate::types::AnnotationSource;

    let expr_var = def.expr_var;
    let expr_type = Type::Variable(expr_var);

    let mut pattern_state = constrain_def_pattern(var_store, &def.loc_pattern, expr_type.clone());

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

            let annotation_expected = Expected::FromAnnotation(
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

            canonicalize_expr(
                &ftv,
                var_store,
                var_usage,
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            )
            .1
        }
        None => {
            canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                def.loc_expr.region,
                &def.loc_expr.value,
                Expected::NoExpectation(expr_type),
            )
            .1
        }
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

fn constrain_recursive_defs(
    rigids: &Rigids,
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    defs: &[Def],
    body_con: Constraint,
) -> Constraint {
    rec_defs_help(
        rigids,
        var_store,
        var_usage,
        defs,
        body_con,
        Info::with_capacity(defs.len()),
        Info::with_capacity(defs.len()),
    )
}

pub fn rec_defs_help(
    rigids: &Rigids,
    var_store: &VarStore,
    var_usage: &mut VarUsage,
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

        canonicalize_pattern(
            var_store,
            &mut pattern_state,
            &def.loc_pattern,
            pattern_expected,
        );

        pattern_state.vars.push(expr_var);

        let mut new_rigids = Vec::new();
        match &def.annotation {
            None => {
                let (_, expr_con) = canonicalize_expr(
                    rigids,
                    var_store,
                    var_usage,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    Expected::NoExpectation(expr_type),
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

                let annotation_expected = Expected::FromAnnotation(
                    def.loc_pattern.clone(),
                    annotation.arity(),
                    AnnotationSource::TypedBody,
                    annotation.clone(),
                );
                let (_, expr_con) = canonicalize_expr(
                    &ftv,
                    var_store,
                    var_usage,
                    def.loc_expr.region,
                    &def.loc_expr.value,
                    Expected::NoExpectation(expr_type.clone()),
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
