use crate::can::pattern::Pattern::{self, *};
use crate::can::pattern::RecordDestruct;
use crate::can::symbol::Symbol;
use crate::collections::SendMap;
use crate::region::{Located, Region};
use crate::subs::Variable;
use crate::types::{Constraint, PExpected, PatternCategory, RecordFieldLabel, Type};

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

            let mut field_types: SendMap<RecordFieldLabel, Type> = SendMap::default();

            for RecordDestruct {
                var,
                label,
                symbol,
                guard,
            } in patterns
            {
                let pat_type = Type::Variable(*var);
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(&symbol) {
                    state
                        .headers
                        .insert(symbol.clone(), Located::at(region, pat_type.clone()));
                }

                field_types.insert(RecordFieldLabel::Required(label.clone()), pat_type.clone());

                // TODO investigate: shouldn't guard_var be constrained somewhere?
                if let Some((_guard_var, loc_guard)) = guard {
                    constrain_pattern(&loc_guard.value, loc_guard.region, expected, state);
                }

                state.vars.push(*var);
            }

            let record_type = Type::Record(field_types, Box::new(ext_type));
            let record_con =
                Constraint::Pattern(region, PatternCategory::Record, record_type, expected);

            state.constraints.push(record_con);
        }

        Tag(_) => {
            panic!("TODO constrain Tag pattern");
        }
        AppliedTag(_, _) => {
            panic!("TODO constrain AppliedTag pattern");
        }
        Shadowed(_) => {
            panic!("TODO constrain Shadowed pattern");
        }
    }
}
