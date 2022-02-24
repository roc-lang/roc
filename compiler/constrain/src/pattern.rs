use crate::builtins;
use crate::expr::{constrain_expr, Env};
use roc_can::constraint::{Constraint, PresenceConstraint};
use roc_can::expected::{Expected, PExpected};
use roc_can::pattern::Pattern::{self, *};
use roc_can::pattern::{DestructType, RecordDestruct};
use roc_collections::all::{Index, SendMap};
use roc_error_macros::todo_opaques;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;
use roc_types::types::{Category, PReason, PatternCategory, Reason, RecordField, Type};

#[derive(Default)]
pub struct PatternState {
    pub headers: SendMap<Symbol, Loc<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
}

/// If there is a type annotation, the pattern state headers can be optimized by putting the
/// annotation in the headers. Normally
///
/// x = 4
///
/// Would add `x => <42>` to the headers (i.e., symbol points to a type variable). If the
/// definition has an annotation, we instead now add `x => Int`.
pub fn headers_from_annotation(
    pattern: &Pattern,
    annotation: &Loc<Type>,
) -> Option<SendMap<Symbol, Loc<Type>>> {
    let mut headers = SendMap::default();
    // Check that the annotation structurally agrees with the pattern, preventing e.g. `{ x, y } : Int`
    // in such incorrect cases we don't put the full annotation in headers, just a variable, and let
    // inference generate a proper error.
    let is_structurally_valid = headers_from_annotation_help(pattern, annotation, &mut headers);

    if is_structurally_valid {
        Some(headers)
    } else {
        None
    }
}

fn headers_from_annotation_help(
    pattern: &Pattern,
    annotation: &Loc<Type>,
    headers: &mut SendMap<Symbol, Loc<Type>>,
) -> bool {
    match pattern {
        Identifier(symbol) | Shadowed(_, _, symbol) => {
            headers.insert(*symbol, annotation.clone());
            true
        }
        Underscore
        | MalformedPattern(_, _)
        | UnsupportedPattern(_)
        | OpaqueNotInScope(..)
        | NumLiteral(..)
        | IntLiteral(..)
        | FloatLiteral(..)
        | StrLiteral(_) => true,

        RecordDestructure { destructs, .. } => match annotation.value.shallow_dealias() {
            Type::Record(fields, _) => {
                for loc_destruct in destructs {
                    let destruct = &loc_destruct.value;

                    // NOTE: We ignore both Guard and optionality when
                    // determining the type of the assigned def (which is what
                    // gets added to the header here).
                    //
                    // For example, no matter whether it's `{ x } = rec` or
                    // `{ x ? 0 } = rec` or `{ x: 5 } -> ...` in all cases
                    // the type of `x` within the binding itself is the same.
                    if let Some(field_type) = fields.get(&destruct.label) {
                        headers.insert(
                            destruct.symbol,
                            Loc::at(annotation.region, field_type.clone().into_inner()),
                        );
                    } else {
                        return false;
                    }
                }
                true
            }
            Type::EmptyRec => destructs.is_empty(),
            _ => false,
        },

        AppliedTag {
            tag_name,
            arguments,
            ..
        } => match annotation.value.shallow_dealias() {
            Type::TagUnion(tags, _) => {
                if let Some((_, arg_types)) = tags.iter().find(|(name, _)| name == tag_name) {
                    if !arguments.len() == arg_types.len() {
                        return false;
                    }

                    arguments
                        .iter()
                        .zip(arg_types.iter())
                        .all(|(arg_pattern, arg_type)| {
                            headers_from_annotation_help(
                                &arg_pattern.1.value,
                                &Loc::at(annotation.region, arg_type.clone()),
                                headers,
                            )
                        })
                } else {
                    false
                }
            }
            _ => false,
        },

        UnwrappedOpaque { .. } => todo_opaques!(),
    }
}

/// This accepts PatternState (rather than returning it) so that the caller can
/// initialize the Vecs in PatternState using with_capacity
/// based on its knowledge of their lengths.
pub fn constrain_pattern(
    env: &Env,
    pattern: &Pattern,
    region: Region,
    expected: PExpected<Type>,
    state: &mut PatternState,
) {
    match pattern {
        Underscore => {
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
        UnsupportedPattern(_) | MalformedPattern(_, _) | OpaqueNotInScope(..) => {
            // Erroneous patterns don't add any constraints.
        }

        Identifier(symbol) | Shadowed(_, _, symbol) => {
            state.constraints.push(Constraint::Present(
                expected.get_type_ref().clone(),
                PresenceConstraint::IsOpen,
            ));
            state.headers.insert(
                *symbol,
                Loc {
                    region,
                    value: expected.get_type(),
                },
            );
        }

        &NumLiteral(var, _, _, bound) => {
            state.vars.push(var);

            let num_type = builtins::num_num(Type::Variable(var));

            let num_type = builtins::add_numeric_bound_constr(
                &mut state.constraints,
                num_type,
                bound,
                region,
                Category::Num,
            );

            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Num,
                num_type,
                expected,
            ));
        }

        &IntLiteral(num_var, precision_var, _, _, bound) => {
            // First constraint on the free num var; this improves the resolved type quality in
            // case the bound is an alias.
            let num_type = builtins::add_numeric_bound_constr(
                &mut state.constraints,
                Type::Variable(num_var),
                bound,
                region,
                Category::Int,
            );

            // Link the free num var with the int var and our expectation.
            let int_type = builtins::num_int(Type::Variable(precision_var));

            state.constraints.push(Constraint::Eq(
                num_type, // TODO check me if something breaks!
                Expected::NoExpectation(int_type),
                Category::Int,
                region,
            ));

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Int,
                Type::Variable(num_var),
                expected,
            ));
        }

        &FloatLiteral(num_var, precision_var, _, _, bound) => {
            // First constraint on the free num var; this improves the resolved type quality in
            // case the bound is an alias.
            let num_type = builtins::add_numeric_bound_constr(
                &mut state.constraints,
                Type::Variable(num_var),
                bound,
                region,
                Category::Float,
            );

            // Link the free num var with the float var and our expectation.
            let float_type = builtins::num_float(Type::Variable(precision_var));

            state.constraints.push(Constraint::Eq(
                num_type.clone(), // TODO check me if something breaks!
                Expected::NoExpectation(float_type),
                Category::Float,
                region,
            ));

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Float,
                num_type, // TODO check me if something breaks!
                expected,
            ));
        }

        StrLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Str,
                builtins::str_type(),
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
            let ext_type = Type::Variable(*ext_var);

            let mut field_types: SendMap<Lowercase, RecordField<Type>> = SendMap::default();

            for Loc {
                value:
                    RecordDestruct {
                        var,
                        label,
                        symbol,
                        typ,
                    },
                ..
            } in destructs
            {
                let pat_type = Type::Variable(*var);
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(symbol) {
                    state
                        .headers
                        .insert(*symbol, Loc::at(region, pat_type.clone()));
                }

                let field_type = match typ {
                    DestructType::Guard(guard_var, loc_guard) => {
                        state.constraints.push(Constraint::Present(
                            Type::Variable(*guard_var),
                            PresenceConstraint::Pattern(
                                region,
                                PatternCategory::PatternGuard,
                                PExpected::ForReason(
                                    PReason::PatternGuard,
                                    pat_type.clone(),
                                    loc_guard.region,
                                ),
                            ),
                        ));
                        state.vars.push(*guard_var);

                        constrain_pattern(env, &loc_guard.value, loc_guard.region, expected, state);

                        RecordField::Demanded(pat_type)
                    }
                    DestructType::Optional(expr_var, loc_expr) => {
                        state.constraints.push(Constraint::Present(
                            Type::Variable(*expr_var),
                            PresenceConstraint::Pattern(
                                region,
                                PatternCategory::PatternDefault,
                                PExpected::ForReason(
                                    PReason::OptionalField,
                                    pat_type.clone(),
                                    loc_expr.region,
                                ),
                            ),
                        ));

                        state.vars.push(*expr_var);

                        let expr_expected = Expected::ForReason(
                            Reason::RecordDefaultField(label.clone()),
                            pat_type.clone(),
                            loc_expr.region,
                        );

                        let expr_con =
                            constrain_expr(env, loc_expr.region, &loc_expr.value, expr_expected);
                        state.constraints.push(expr_con);

                        RecordField::Optional(pat_type)
                    }
                    DestructType::Required => {
                        // No extra constraints necessary.
                        RecordField::Demanded(pat_type)
                    }
                };

                field_types.insert(label.clone(), field_type);

                state.vars.push(*var);
            }

            let record_type = Type::Record(field_types, Box::new(ext_type));

            let whole_con = Constraint::Eq(
                Type::Variable(*whole_var),
                Expected::NoExpectation(record_type),
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let record_con = Constraint::Present(
                Type::Variable(*whole_var),
                PresenceConstraint::Pattern(region, PatternCategory::Record, expected),
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }
        AppliedTag {
            whole_var,
            ext_var,
            tag_name,
            arguments,
        } => {
            let mut argument_types = Vec::with_capacity(arguments.len());
            for (index, (pattern_var, loc_pattern)) in arguments.iter().enumerate() {
                state.vars.push(*pattern_var);

                let pattern_type = Type::Variable(*pattern_var);
                argument_types.push(pattern_type.clone());

                let expected = PExpected::ForReason(
                    PReason::TagArg {
                        tag_name: tag_name.clone(),
                        index: Index::zero_based(index),
                    },
                    pattern_type,
                    region,
                );
                constrain_pattern(env, &loc_pattern.value, loc_pattern.region, expected, state);
            }

            let whole_con = Constraint::Present(
                expected.clone().get_type(),
                PresenceConstraint::IncludesTag(tag_name.clone(), argument_types.clone()),
            );

            let tag_con = Constraint::Present(
                Type::Variable(*whole_var),
                PresenceConstraint::Pattern(
                    region,
                    PatternCategory::Ctor(tag_name.clone()),
                    expected,
                ),
            );

            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            state.constraints.push(whole_con);
            state.constraints.push(tag_con);
        }

        UnwrappedOpaque { .. } => todo_opaques!(),
    }
}
