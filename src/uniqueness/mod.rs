use crate::can::def::Def;
use crate::can::expr::Expr;
use crate::can::expr::Field;
use crate::can::ident::Lowercase;
use crate::can::pattern;
use crate::can::pattern::{Pattern, RecordDestruct};
use crate::can::procedure::Procedure;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::constrain::builtins;
use crate::constrain::expr::exists;
use crate::constrain::expr::{Info, Rigids};
use crate::ident::Ident;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::AnnotationSource::TypedWhenBranch;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self};
use crate::types::LetConstraint;
use crate::types::PExpected::{self};
use crate::types::PReason::{self};
use crate::types::Reason;
use crate::types::RecordFieldLabel;
use crate::types::Type::{self, *};
use crate::uniqueness::boolean_algebra::Bool;
use crate::uniqueness::sharing::VarUsage;

pub use crate::can::expr::Expr::*;

pub mod boolean_algebra;
mod constrain;
pub mod sharing;

pub struct Env {
    pub bound_names: ImMap<Symbol, Variable>,
    pub procedures: ImMap<Symbol, Procedure>,
}

pub fn constrain_declaration(
    var_store: &VarStore,
    region: Region,
    loc_expr: Located<Expr>,
    _declared_idents: &ImMap<Ident, (Symbol, Region)>,
    expected: Expected<Type>,
) -> Constraint {
    let rigids = ImMap::default();
    let mut var_usage = VarUsage::default();

    constrain_expr(
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

fn constrain_pattern(
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
            let uniq_var = var_store.fresh();
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Int,
                constrain::attr_type(Bool::Variable(uniq_var), Type::int()),
                expected,
            ));
        }
        FloatLiteral(_) => {
            let uniq_var = var_store.fresh();
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Float,
                constrain::attr_type(Bool::Variable(uniq_var), Type::float()),
                expected,
            ));
        }

        StrLiteral(_) => {
            let uniq_var = var_store.fresh();
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Str,
                constrain::attr_type(Bool::Variable(uniq_var), Type::string()),
                expected,
            ));
        }

        RecordDestructure(ext_var, patterns) => {
            let mut pattern_uniq_vars = Vec::with_capacity(patterns.len());

            state.vars.push(*ext_var);
            let ext_type = Type::Variable(*ext_var);

            let mut field_types: SendMap<RecordFieldLabel, Type> = SendMap::default();
            for RecordDestruct {
                var,
                label,
                symbol,
                guard,
            } in patterns
            {
                let pat_uniq_var = var_store.fresh();
                pattern_uniq_vars.push(pat_uniq_var);

                let pat_type =
                    constrain::attr_type(Bool::Variable(pat_uniq_var), Type::Variable(*var));
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(&symbol) {
                    state.headers.insert(
                        symbol.clone(),
                        Located::at(pattern.region, pat_type.clone()),
                    );
                }

                field_types.insert(label.clone(), pat_type.clone());

                if let Some((guard_var, loc_guard)) = guard {
                    state.constraints.push(Eq(
                        Type::Variable(*guard_var),
                        Expected::NoExpectation(pat_type.clone()),
                        pattern.region,
                    ));
                    state.vars.push(*guard_var);
                    constrain_pattern(var_store, state, loc_guard, expected);
                }

                state.vars.push(*var);
            }

            let record_uniq_type = if pattern_uniq_vars.is_empty() {
                // explicitly keep uniqueness of empty record (match) free
                let empty_var = var_store.fresh();
                state.vars.push(empty_var);
                Bool::Variable(empty_var)
            } else {
                boolean_algebra::any(pattern_uniq_vars.into_iter().map(Bool::Variable))
            };

            let record_type = constrain::attr_type(
                record_uniq_type,
                Type::Record(field_types, Box::new(ext_type)),
            );
            let record_con = Constraint::Pattern(
                pattern.region,
                PatternCategory::Record,
                record_type,
                expected,
            );

            state.constraints.push(record_con);
        }

        AppliedTag(ext_var, symbol, patterns) => {
            let mut argument_types = Vec::with_capacity(patterns.len());
            let mut pattern_uniq_vars = Vec::with_capacity(patterns.len());

            for (pattern_var, loc_pattern) in patterns {
                state.vars.push(*pattern_var);

                let pat_uniq_var = var_store.fresh();
                pattern_uniq_vars.push(pat_uniq_var);

                let pattern_type = constrain::attr_type(
                    Bool::Variable(pat_uniq_var),
                    Type::Variable(*pattern_var),
                );
                argument_types.push(pattern_type.clone());

                let expected = PExpected::NoExpectation(pattern_type);
                constrain_pattern(var_store, state, loc_pattern, expected);
            }

            let record_uniq_type = if pattern_uniq_vars.is_empty() {
                // explicitly keep uniqueness of empty tag union (match) free
                let empty_var = var_store.fresh();
                state.vars.push(empty_var);
                Bool::Variable(empty_var)
            } else {
                boolean_algebra::any(pattern_uniq_vars.into_iter().map(Bool::Variable))
            };

            let union_type = constrain::attr_type(
                record_uniq_type,
                Type::TagUnion(
                    vec![(symbol.clone(), argument_types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
            );

            let tag_con = Constraint::Pattern(
                pattern.region,
                PatternCategory::Ctor(symbol.clone()),
                union_type,
                expected,
            );

            state.vars.push(*ext_var);
            state.constraints.push(tag_con);
        }

        Underscore | Shadowed(_) | UnsupportedPattern(_) => {
            // no constraints
        }
    }
}

pub fn constrain_expr(
    rigids: &Rigids,
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Constraint {
    pub use crate::can::expr::Expr::*;

    match expr {
        Int(var, _) => {
            let uniq_var = var_store.fresh();
            exists(
                vec![*var, uniq_var],
                And(vec![
                    Eq(
                        Type::Variable(*var),
                        Expected::ForReason(
                            Reason::IntLiteral,
                            constrain::attr_type(Bool::Variable(uniq_var), Type::int()),
                            region,
                        ),
                        region,
                    ),
                    Eq(Type::Variable(*var), expected, region),
                ]),
            )
        }
        Float(var, _) => {
            let uniq_var = var_store.fresh();
            exists(
                vec![*var, uniq_var],
                And(vec![
                    Eq(
                        Type::Variable(*var),
                        Expected::ForReason(
                            Reason::FloatLiteral,
                            constrain::attr_type(Bool::Variable(uniq_var), Type::float()),
                            region,
                        ),
                        region,
                    ),
                    Eq(Type::Variable(*var), expected, region),
                ]),
            )
        }
        BlockStr(_) | Str(_) => {
            let uniq_type = var_store.fresh();
            let inferred = constrain::attr_type(Bool::Variable(uniq_type), Type::string());

            exists(vec![uniq_type], Eq(inferred, expected, region))
        }
        EmptyRecord => {
            let uniq_type = var_store.fresh();

            exists(
                vec![uniq_type],
                Eq(
                    constrain::attr_type(Bool::Variable(uniq_type), EmptyRec),
                    expected,
                    region,
                ),
            )
        }
        Record(variable, fields) => {
            // NOTE: canonicalization guarantees at least one field
            // zero fields generates an EmptyRecord
            let mut field_types = SendMap::default();
            let mut field_vars = Vec::with_capacity(fields.len());
            field_vars.push(*variable);

            // Constraints need capacity for each field + 1 for the record itself + 1 for ext
            let mut constraints = Vec::with_capacity(2 + fields.len());

            for (label, ref field) in fields.iter() {
                let field_var = var_store.fresh();
                let field_type = Variable(field_var);
                let field_expected = Expected::NoExpectation(field_type.clone());
                let loc_expr = &*field.loc_expr;
                let field_con = constrain_expr(
                    rigids,
                    var_store,
                    var_usage,
                    loc_expr.region,
                    &loc_expr.value,
                    field_expected,
                );

                field_vars.push(field_var);
                field_types.insert(label.clone(), field_type);

                constraints.push(field_con);
            }

            let record_uniq_var = var_store.fresh();
            field_vars.push(record_uniq_var);
            let record_type = constrain::attr_type(
                Bool::Variable(record_uniq_var),
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

            exists(field_vars, And(constraints))
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
                    var_store,
                    var_usage,
                    loc_expr.region,
                    &loc_expr.value,
                    Expected::NoExpectation(Type::Variable(*var)),
                );

                arg_cons.push(arg_con);
                vars.push(*var);
                types.push(Type::Variable(*var));
            }

            let uniq_var = var_store.fresh();
            vars.push(uniq_var);

            let union_type = constrain::attr_type(
                Bool::Variable(uniq_var),
                Type::TagUnion(
                    vec![(name.clone(), types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
            );

            let union_con = Eq(union_type, expected.clone(), region);
            let ast_con = Eq(Type::Variable(*variant_var), expected, region);

            vars.push(*variant_var);
            vars.push(*ext_var);
            arg_cons.push(union_con);
            arg_cons.push(ast_con);

            exists(vars, And(arg_cons))
        }
        List(variable, loc_elems) => {
            let uniq_var = var_store.fresh();
            if loc_elems.is_empty() {
                let list_var = *variable;
                let inferred = constrain::attr_type(
                    Bool::Variable(uniq_var),
                    builtins::empty_list_type(list_var),
                );
                exists(vec![*variable, uniq_var], Eq(inferred, expected, region))
            } else {
                // constrain `expected ~ List a` and that all elements `~ a`.
                let list_var = *variable; // `v` in the type (List v)
                let list_type = Type::Variable(list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));
                let mut elem_vars = Vec::with_capacity(2 + loc_elems.len());

                for (elem_var, loc_elem) in loc_elems.iter() {
                    let elem_type = Variable(*elem_var);
                    let elem_expected = Expected::NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_type.clone(),
                        Expected::ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let constraint = constrain_expr(
                        rigids,
                        var_store,
                        var_usage,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(list_elem_constraint);
                    constraints.push(constraint);
                    elem_vars.push(*elem_var);
                }

                elem_vars.push(list_var);
                elem_vars.push(uniq_var);

                let inferred =
                    constrain::attr_type(Bool::Variable(uniq_var), builtins::list_type(list_type));
                constraints.push(Eq(inferred, expected, region));

                exists(elem_vars, And(constraints))
            }
        }
        Var {
            symbol_for_lookup, ..
        } => {
            var_usage.register(symbol_for_lookup);
            let usage = var_usage.get_usage(symbol_for_lookup);

            match usage {
                Some(sharing::ReferenceCount::Shared) => {
                    // the variable is used/consumed more than once, so it must be Shared
                    let val_var = var_store.fresh();
                    let uniq_var = var_store.fresh();

                    let val_type = Variable(val_var);
                    let uniq_type = Bool::Variable(uniq_var);

                    let attr_type = constrain::attr_type(uniq_type.clone(), val_type);

                    exists(
                        vec![val_var, uniq_var],
                        And(vec![
                            Lookup(symbol_for_lookup.clone(), expected.clone(), region),
                            Eq(attr_type, expected, region),
                            Eq(
                                Type::Boolean(uniq_type),
                                Expected::NoExpectation(Type::Boolean(constrain::shared_type())),
                                region,
                            ),
                        ]),
                    )
                }
                Some(sharing::ReferenceCount::Unique) => {
                    // no additional constraints, keep uniqueness unbound
                    Lookup(symbol_for_lookup.clone(), expected.clone(), region)
                }
                None => panic!("symbol not analyzed"),
            }
        }
        Closure(fn_var, _symbol, _recursion, args, boxed) => {
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
            vars.push(*fn_var);

            for (pattern_var, loc_pattern) in args {
                let pattern_type = Type::Variable(*pattern_var);
                let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

                pattern_types.push(pattern_type);

                constrain_pattern(var_store, &mut state, loc_pattern, pattern_expected);

                vars.push(*pattern_var);
            }

            let fn_uniq_var = var_store.fresh();
            vars.push(fn_uniq_var);

            let fn_type = constrain::attr_type(
                Bool::Variable(fn_uniq_var),
                Type::Function(pattern_types, Box::new(ret_type.clone())),
            );
            let body_type = Expected::NoExpectation(ret_type);
            let ret_constraint = constrain_expr(
                rigids,
                var_store,
                var_usage,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

            let defs_constraint = And(state.constraints);

            // remove identifiers bound in the arguments from VarUsage
            // makes e.g. `(\x -> x) (\x -> x)` count as unique in both cases
            for (_, pattern) in args {
                for identifier in pattern::symbols_from_pattern(&pattern.value) {
                    var_usage.unregister(&identifier);
                }
            }

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
                    Eq(
                        Type::Variable(*fn_var),
                        Expected::NoExpectation(fn_type),
                        region,
                    ),
                ]),
            )
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, fn_expr, ret_var) = &**boxed;
            let fn_type = Variable(*fn_var);
            let ret_type = Variable(*ret_var);
            let fn_expected = Expected::NoExpectation(fn_type.clone());
            let fn_region = fn_expr.region;

            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(*fn_var);
            vars.push(*ret_var);

            // Canonicalize the function expression and its arguments
            let fn_con = constrain_expr(
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
                let arg_con = constrain_expr(
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

            let expected_uniq_type = var_store.fresh();
            vars.push(expected_uniq_type);
            let expected_fn_type = Expected::ForReason(
                fn_reason,
                constrain::attr_type(
                    Bool::Variable(expected_uniq_type),
                    Function(arg_types, Box::new(ret_type.clone())),
                ),
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
        LetRec(defs, loc_ret, var) => {
            // NOTE doesn't currently unregister bound symbols
            // may be a problem when symbols are not globally unique
            let body_con = constrain_expr(
                rigids,
                var_store,
                var_usage,
                loc_ret.region,
                &loc_ret.value,
                expected.clone(),
            );
            exists(
                vec![*var],
                And(vec![
                    constrain_recursive_defs(rigids, var_store, var_usage, defs, body_con),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(Type::Variable(*var), expected, loc_ret.region),
                ]),
            )
        }
        LetNonRec(def, loc_ret, var) => {
            // NOTE doesn't currently unregister bound symbols
            // may be a problem when symbols are not globally unique
            let body_con = constrain_expr(
                rigids,
                var_store,
                var_usage,
                loc_ret.region,
                &loc_ret.value,
                expected.clone(),
            );

            exists(
                vec![*var],
                And(vec![
                    constrain_def(rigids, var_store, var_usage, def, body_con),
                    // Record the type of tne entire def-expression in the variable.
                    // Code gen will need that later!
                    Eq(Type::Variable(*var), expected, loc_ret.region),
                ]),
            )
        }
        If { .. } => panic!("TODO constrain uniq if"),
        When {
            cond_var,
            loc_cond,
            branches,
            ..
        } => {
            let cond_var = *cond_var;
            let cond_type = Variable(cond_var);
            let expr_con = constrain_expr(
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
                        let branch_con = constrain_when_branch(
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
                        let branch_con = constrain_when_branch(
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

            And(constraints)
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
            let mut cons = Vec::with_capacity(updates.len() + 3);
            for (field_name, Field { var, loc_expr, .. }) in updates.clone() {
                let (var, tipe, con) = constrain_field_update(
                    rigids,
                    var_store,
                    var_usage,
                    var,
                    region,
                    field_name.clone(),
                    &loc_expr,
                );
                fields.insert(field_name, tipe);
                vars.push(var);
                cons.push(con);
            }

            let uniq_var = var_store.fresh();
            vars.push(uniq_var);

            let fields_type = constrain::attr_type(
                Bool::Variable(uniq_var),
                Type::Record(fields.clone(), Box::new(Type::Variable(*ext_var))),
            );
            let record_type = Type::Variable(*record_var);

            // NOTE from elm compiler: fields_type is separate so that Error propagates better
            let fields_con = Eq(
                record_type.clone(),
                Expected::NoExpectation(fields_type),
                region,
            );
            let record_con = Eq(record_type.clone(), expected, region);

            vars.push(*record_var);
            vars.push(*ext_var);

            let con = Lookup(
                symbol.clone(),
                Expected::ForReason(
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

        Access {
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let ext_type = Type::Variable(*ext_var);

            let field_uniq_var = var_store.fresh();
            let field_uniq_type = Bool::Variable(field_uniq_var);
            let field_type =
                constrain::attr_type(field_uniq_type.clone(), Type::Variable(*field_var));

            let mut rec_field_types = SendMap::default();

            rec_field_types.insert(field.clone(), field_type.clone());

            let record_uniq_var = var_store.fresh();
            let record_uniq_type = Bool::Variable(record_uniq_var);
            let record_type = constrain::attr_type(
                record_uniq_type.clone(),
                Type::Record(rec_field_types, Box::new(ext_type)),
            );
            let record_expected = Expected::NoExpectation(record_type);

            let mut constraint = constrain_expr(
                rigids,
                var_store,
                var_usage,
                loc_expr.region,
                &loc_expr.value,
                record_expected,
            );

            let uniq_con = Eq(
                Type::Boolean(field_uniq_type),
                Expected::NoExpectation(Type::Boolean(record_uniq_type)),
                region,
            );

            constraint = exists(
                vec![*field_var, *ext_var, field_uniq_var, record_uniq_var],
                And(vec![constraint, Eq(field_type, expected, region), uniq_con]),
            );

            constraint
        }

        Accessor {
            field,
            field_var,
            ext_var,
        } => {
            let mut field_types = SendMap::default();

            let field_uniq_var = var_store.fresh();
            let field_uniq_type = Bool::Variable(field_uniq_var);
            let field_type =
                constrain::attr_type(field_uniq_type.clone(), Type::Variable(*field_var));

            field_types.insert(field.clone(), field_type.clone());

            let record_uniq_var = var_store.fresh();
            let record_uniq_type = Bool::Variable(record_uniq_var);
            let record_type = constrain::attr_type(
                record_uniq_type.clone(),
                Type::Record(field_types, Box::new(Type::Variable(*ext_var))),
            );

            let fn_uniq_var = var_store.fresh();
            let fn_type = constrain::attr_type(
                Bool::Variable(fn_uniq_var),
                Type::Function(vec![record_type], Box::new(field_type)),
            );

            let uniq_con = Eq(
                Type::Boolean(field_uniq_type),
                Expected::NoExpectation(Type::Boolean(record_uniq_type)),
                region,
            );

            exists(
                vec![
                    *field_var,
                    *ext_var,
                    fn_uniq_var,
                    field_uniq_var,
                    record_uniq_var,
                ],
                And(vec![Eq(fn_type, expected, region), uniq_con]),
            )
        }
        RuntimeError(_) => True,
    }
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn constrain_when_branch(
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    rigids: &Rigids,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
) -> Constraint {
    let ret_constraint = constrain_expr(
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
    constrain_pattern(var_store, &mut state, &loc_pattern, pattern_expected);

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

    constrain_pattern(var_store, &mut state, loc_pattern, pattern_expected);

    state
}

/// Turn e.g. `Int` into `Attr.Attr * Int`
fn annotation_to_attr_type(var_store: &VarStore, ann: &Type) -> (Vec<Variable>, Type) {
    use crate::types::Type::*;

    match ann {
        Variable(_) | Boolean(_) | Erroneous(_) => (vec![], ann.clone()),
        EmptyRec | EmptyTagUnion => {
            let uniq_var = var_store.fresh();
            (
                vec![uniq_var],
                constrain::attr_type(Bool::Variable(uniq_var), ann.clone()),
            )
        }

        Function(arguments, result) => {
            let uniq_var = var_store.fresh();
            let (mut arg_vars, args_lifted) = annotation_to_attr_type_many(var_store, arguments);
            let (result_vars, result_lifted) = annotation_to_attr_type(var_store, result);

            arg_vars.extend(result_vars);
            arg_vars.push(uniq_var);

            (
                arg_vars,
                constrain::attr_type(
                    Bool::Variable(uniq_var),
                    Type::Function(args_lifted, Box::new(result_lifted)),
                ),
            )
        }

        Apply {
            module_name,
            name,
            args,
        } => {
            let uniq_var = var_store.fresh();
            let (mut arg_vars, args_lifted) = annotation_to_attr_type_many(var_store, args);

            arg_vars.push(uniq_var);

            (
                arg_vars,
                constrain::attr_type(
                    Bool::Variable(uniq_var),
                    Type::Apply {
                        module_name: module_name.clone(),
                        name: name.clone(),
                        args: args_lifted,
                    },
                ),
            )
        }

        Record(fields, ext_type) => {
            let uniq_var = var_store.fresh();
            let mut vars = Vec::with_capacity(fields.len());
            let mut lifted_fields = SendMap::default();

            for (label, tipe) in fields.clone() {
                let (new_vars, lifted_field) = annotation_to_attr_type(var_store, &tipe);
                vars.extend(new_vars);
                lifted_fields.insert(label, lifted_field);
            }

            vars.push(uniq_var);

            (
                vars,
                constrain::attr_type(
                    Bool::Variable(uniq_var),
                    Type::Record(lifted_fields, ext_type.clone()),
                ),
            )
        }

        TagUnion(tags, ext_type) => {
            let uniq_var = var_store.fresh();
            let mut vars = Vec::with_capacity(tags.len());
            let mut lifted_tags = Vec::with_capacity(tags.len());

            for (tag, fields) in tags {
                let (new_vars, lifted_fields) = annotation_to_attr_type_many(var_store, fields);
                vars.extend(new_vars);
                lifted_tags.push((tag.clone(), lifted_fields));
            }

            vars.push(uniq_var);

            (
                vars,
                constrain::attr_type(
                    Bool::Variable(uniq_var),
                    Type::TagUnion(lifted_tags, ext_type.clone()),
                ),
            )
        }

        Alias(module_name, uppercase, fields, actual) => {
            let uniq_var = var_store.fresh();

            let (mut actual_vars, lifted_actual) = annotation_to_attr_type(var_store, actual);

            actual_vars.push(uniq_var);

            (
                actual_vars,
                constrain::attr_type(
                    Bool::Variable(uniq_var),
                    Type::Alias(
                        module_name.clone(),
                        uppercase.clone(),
                        fields.clone(),
                        Box::new(lifted_actual),
                    ),
                ),
            )
        }
    }
}

fn annotation_to_attr_type_many(var_store: &VarStore, anns: &[Type]) -> (Vec<Variable>, Vec<Type>) {
    anns.iter()
        .fold((Vec::new(), Vec::new()), |(mut vars, mut types), value| {
            let (new_vars, tipe) = annotation_to_attr_type(var_store, value);
            vars.extend(new_vars);
            types.push(tipe);

            (vars, types)
        })
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
            let (uniq_vars, annotation) = annotation_to_attr_type(var_store, annotation);

            pattern_state.vars.extend(uniq_vars);

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
                annotation,
            );

            pattern_state.constraints.push(Eq(
                expr_type,
                annotation_expected.clone(),
                Region::zero(),
            ));

            constrain_expr(
                &ftv,
                var_store,
                var_usage,
                def.loc_expr.region,
                &def.loc_expr.value,
                annotation_expected,
            )
        }
        None => constrain_expr(
            rigids,
            var_store,
            var_usage,
            def.loc_expr.region,
            &def.loc_expr.value,
            Expected::NoExpectation(expr_type),
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

        constrain_pattern(
            var_store,
            &mut pattern_state,
            &def.loc_pattern,
            pattern_expected,
        );

        pattern_state.vars.push(expr_var);

        let mut new_rigids = Vec::new();
        match &def.annotation {
            None => {
                let expr_con = constrain_expr(
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
                let expr_con = constrain_expr(
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

#[inline(always)]
fn constrain_field_update(
    rigids: &Rigids,
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    var: Variable,
    region: Region,
    field: Lowercase,
    loc_expr: &Located<Expr>,
) -> (Variable, Type, Constraint) {
    let field_type = Type::Variable(var);
    let reason = Reason::RecordUpdateValue(field);
    let expected = Expected::ForReason(reason, field_type.clone(), region);
    let con = constrain_expr(
        rigids,
        var_store,
        var_usage,
        loc_expr.region,
        &loc_expr.value,
        expected,
    );

    (var, field_type, con)
}
