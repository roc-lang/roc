use crate::builtins;
use roc_can::constraint::Constraint;
use roc_can::expected::{Expected, PExpected};
use roc_can::pattern::Pattern::{self, *};
use roc_can::pattern::RecordDestruct;
use roc_collections::all::SendMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::Variable;
use roc_types::types::{PatternCategory, Type};

pub struct PatternState {
    pub headers: SendMap<Symbol, Located<Type>>,
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
    annotation: &Located<Type>,
) -> Option<SendMap<Symbol, Located<Type>>> {
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
    annotation: &Located<Type>,
    headers: &mut SendMap<Symbol, Located<Type>>,
) -> bool {
    match pattern {
        Identifier(symbol) => {
            headers.insert(symbol.clone(), annotation.clone());
            true
        }
        Underscore
        | Shadowed(_, _)
        | UnsupportedPattern(_)
        | NumLiteral(_, _)
        | IntLiteral(_)
        | FloatLiteral(_)
        | StrLiteral(_) => true,

        RecordDestructure { destructs, .. } => match annotation.value.shallow_dealias() {
            Type::Record(fields, _) => {
                for destruct in destructs {
                    // NOTE ignores the .guard field.
                    if let Some(field_type) = fields.get(&destruct.value.label) {
                        headers.insert(
                            destruct.value.symbol.clone(),
                            Located::at(annotation.region, field_type.clone()),
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
                                &Located::at(annotation.region, arg_type.clone()),
                                headers,
                            )
                        })
                } else {
                    false
                }
            }
            _ => false,
        },
    }
}

/// This accepts PatternState (rather than returning it) so that the caller can
/// intiialize the Vecs in PatternState using with_capacity
/// based on its knowledge of their lengths.
pub fn constrain_pattern(
    pattern: &Pattern,
    region: Region,
    expected: PExpected<Type>,
    state: &mut PatternState,
) {
    match pattern {
        Underscore | UnsupportedPattern(_) | Shadowed(_, _) => {
            // Neither the _ pattern nor erroneous ones add any constraints.
        }

        Identifier(symbol) => {
            state.headers.insert(
                symbol.clone(),
                Located {
                    region,
                    value: expected.get_type(),
                },
            );
        }

        NumLiteral(var, _) => {
            state.vars.push(*var);

            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Num,
                builtins::builtin_type(Symbol::NUM_NUM, vec![Type::Variable(*var)]),
                expected,
            ));
        }

        IntLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Float,
                builtins::builtin_type(Symbol::INT_INT, vec![]),
                expected,
            ));
        }

        FloatLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Float,
                builtins::builtin_type(Symbol::FLOAT_FLOAT, vec![]),
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

            let mut field_types: SendMap<Lowercase, Type> = SendMap::default();

            for Located {
                value:
                    RecordDestruct {
                        var,
                        label,
                        symbol,
                        guard,
                    },
                ..
            } in destructs
            {
                let pat_type = Type::Variable(*var);
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(&symbol) {
                    state
                        .headers
                        .insert(symbol.clone(), Located::at(region, pat_type.clone()));
                }

                field_types.insert(label.clone(), pat_type.clone());

                if let Some((guard_var, loc_guard)) = guard {
                    state.constraints.push(Constraint::Eq(
                        Type::Variable(*guard_var),
                        Expected::NoExpectation(pat_type.clone()),
                        region,
                    ));
                    state.vars.push(*guard_var);

                    constrain_pattern(&loc_guard.value, loc_guard.region, expected, state);
                }

                state.vars.push(*var);
            }

            let record_type = Type::Record(field_types, Box::new(ext_type));

            let whole_con = Constraint::Eq(
                Type::Variable(*whole_var),
                Expected::NoExpectation(record_type),
                region,
            );

            let record_con = Constraint::Pattern(
                region,
                PatternCategory::Record,
                Type::Variable(*whole_var),
                expected,
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
            for (pattern_var, loc_pattern) in arguments {
                state.vars.push(*pattern_var);

                let pattern_type = Type::Variable(*pattern_var);
                argument_types.push(pattern_type.clone());

                let expected = PExpected::NoExpectation(pattern_type);
                constrain_pattern(&loc_pattern.value, loc_pattern.region, expected, state);
            }

            let whole_con = Constraint::Eq(
                Type::Variable(*whole_var),
                Expected::NoExpectation(Type::TagUnion(
                    vec![(tag_name.clone(), argument_types)],
                    Box::new(Type::Variable(*ext_var)),
                )),
                region,
            );

            let tag_con = Constraint::Pattern(
                region,
                PatternCategory::Ctor(tag_name.clone()),
                Type::Variable(*whole_var),
                expected,
            );

            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            state.constraints.push(whole_con);
            state.constraints.push(tag_con);
        }
    }
}
