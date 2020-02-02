use crate::can::ident::Lowercase;
use crate::can::pattern::Pattern::{self, *};
use crate::can::pattern::RecordDestruct;
use crate::collections::SendMap;
use crate::module::symbol::Symbol;
use crate::region::{Located, Region};
use crate::subs::Variable;
use crate::types::{Constraint, Expected, PExpected, PatternCategory, Type};

pub struct PatternState {
    pub headers: SendMap<Symbol, Located<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
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
        Underscore | UnsupportedPattern(_) => {
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
        IntLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Int,
                Type::int(),
                expected,
            ));
        }

        FloatLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Float,
                Type::float(),
                expected,
            ));
        }

        StrLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                region,
                PatternCategory::Str,
                Type::string(),
                expected,
            ));
        }

        RecordDestructure(ext_var, patterns) => {
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
            } in patterns
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
            let record_con =
                Constraint::Pattern(region, PatternCategory::Record, record_type, expected);

            state.constraints.push(record_con);
        }
        AppliedTag(ext_var, tag_name, patterns) => {
            let mut argument_types = Vec::with_capacity(patterns.len());
            for (pattern_var, loc_pattern) in patterns {
                state.vars.push(*pattern_var);

                let pattern_type = Type::Variable(*pattern_var);
                argument_types.push(pattern_type.clone());

                let expected = PExpected::NoExpectation(pattern_type);
                constrain_pattern(&loc_pattern.value, loc_pattern.region, expected, state);
            }

            let tag_con = Constraint::Pattern(
                region,
                PatternCategory::Ctor(tag_name.clone()),
                Type::TagUnion(
                    vec![(tag_name.clone(), argument_types)],
                    Box::new(Type::Variable(*ext_var)),
                ),
                expected,
            );

            state.vars.push(*ext_var);
            state.constraints.push(tag_con);
        }
        Shadowed(_, _) => {
            panic!("TODO constrain Shadowed pattern");
        }
    }
}
