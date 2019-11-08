use can::env::Env;
use can::problem::Problem;
use can::scope::Scope;
use can::symbol::Symbol;
use collections::ImMap;
use ident::{Ident, VariantName};
use parse::ast;
use region::{Located, Region};
use subs::Subs;
use subs::Variable;
use types::{Constraint, PExpected, PatternCategory, Type};

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(Variable, Symbol),
    Variant(Variable, Symbol),
    AppliedVariant(Variable, Symbol, Vec<Located<Pattern>>),
    IntLiteral(i64),
    FloatLiteral(f64),
    ExactString(Box<str>),
    EmptyRecordLiteral(Variable),
    Underscore(Variable),

    // Runtime Exceptions
    Shadowed(Located<Ident>),
    UnrecognizedVariant(Located<VariantName>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
}

/// Different patterns are supported in different circumstances.
/// For example, case branches can pattern match on number literals, but
/// assignments and function args can't. Underscore is supported in function
/// arg patterns and in case branch patterns, but not in assignments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    Assignment,
    FunctionArg,
    CaseBranch,
}

pub fn canonicalize_pattern<'a>(
    env: &'a mut Env,
    subs: &mut Subs,
    scope: &mut Scope,
    pattern_type: &'a PatternType,
    loc_pattern: &'a Located<ast::Pattern<'a>>,
    shadowable_idents: &'a mut ImMap<Ident, (Symbol, Region)>,
    expected: PExpected<Type>,
) -> (Located<Pattern>, State) {
    use self::PatternType::*;
    use can::ast::Pattern::*;

    let region = loc_pattern.region;
    let pattern = match &loc_pattern.value {
        &Identifier(ref name) => {
            let unqualified_ident = Ident::Unqualified(name.to_string());

            // We use shadowable_idents for this, and not scope, because for assignments
            // they are different. When canonicalizing a particular assignment, that new
            // ident is in scope (for recursion) but not shadowable.
            //
            // For example, when canonicalizing (fibonacci = ...), `fibonacci` should be in scope
            // so that it can refer to itself without getting a naming problem, but it should not
            // be in the collection of shadowable idents because you can't shadow yourself!
            match shadowable_idents.get(&unqualified_ident) {
                Some((_, region)) => {
                    let loc_shadowed_ident = Located {
                        region: *region,
                        value: unqualified_ident,
                    };

                    // This is already in scope, meaning it's about to be shadowed.
                    // Shadowing is not allowed!
                    env.problem(Problem::Shadowing(loc_shadowed_ident.clone()));

                    // Change this Pattern to a Shadowed variant, so that
                    // codegen knows to generate a runtime exception here.
                    Pattern::Shadowed(loc_shadowed_ident)
                }
                None => {
                    // Make sure we aren't shadowing something in the home module's scope.
                    let qualified_ident =
                        Ident::Qualified(env.home.to_string(), unqualified_ident.name());

                    match scope.idents.get(&qualified_ident) {
                        Some((_, region)) => {
                            let loc_shadowed_ident = Located {
                                region: *region,
                                value: qualified_ident,
                            };

                            // This is already in scope, meaning it's about to be shadowed.
                            // Shadowing is not allowed!
                            env.problem(Problem::Shadowing(loc_shadowed_ident.clone()));

                            // Change this Pattern to a Shadowed variant, so that
                            // codegen knows to generate a runtime exception here.
                            Pattern::Shadowed(loc_shadowed_ident)
                        }
                        None => {
                            let new_ident = qualified_ident.clone();
                            let new_name = qualified_ident.name();
                            let symbol = scope.symbol(&new_name);

                            // This is a fresh identifier that wasn't already in scope.
                            // Add it to scope!
                            let symbol_and_region = (symbol.clone(), region);

                            // Add this to both scope.idents *and* shadowable_idents.
                            // The latter is relevant when recursively canonicalizing Variant patterns,
                            // which can bring multiple new idents into scope. For example, it's important
                            // that we catch (Blah foo foo) as being an example of shadowing.
                            scope
                                .idents
                                .insert(new_ident.clone(), symbol_and_region.clone());
                            shadowable_idents.insert(new_ident, symbol_and_region.clone());

                            Pattern::Identifier(subs.mk_flex_var(), symbol)
                        }
                    }
                }
            }
        }

        //         &AppliedVariant((ref loc_name, ref loc_args)) => {
        //             // Canonicalize the variant's arguments.
        //             let mut can_args: Vec<Located<Pattern>> = Vec::new();

        //             for loc_arg in loc_args {
        //                 let loc_can_arg =
        //                     canonicalize_pattern(env, subs, constraints, scope, pattern_type, &loc_arg, shadowable_idents);

        //                 can_args.push(loc_can_arg);
        //             }

        //             // Canonicalize the variant's name.
        //             let symbol = Symbol::from_variant(&loc_name.value, &env.home);

        //             if env.variants.contains_key(&symbol) {
        //                 // No problems; the qualified variant name was in scope!
        //                 Pattern::AppliedVariant(symbol, can_args)
        //             } else {
        //                 // We couldn't find the variant name in scope. NAMING PROBLEM!
        //                 env.problem(Problem::UnrecognizedVariant(loc_name.clone()));

        //                 Pattern::UnrecognizedVariant(loc_name.clone())
        //             }
        //         }
        &Variant(module_parts, name) => {
            // Canonicalize the variant's name.
            let variant = if module_parts.is_empty() {
                VariantName::Unqualified(name.to_string())
            } else {
                VariantName::Qualified(module_parts.to_vec().join("."), name.to_string())
            };

            let symbol = Symbol::from_variant(&variant, &env.home);

            if env.variants.contains_key(&symbol) {
                // No problems; the qualified variant name was in scope!
                Pattern::Variant(subs.mk_flex_var(), symbol)
            } else {
                let loc_name = Located {
                    region: region,
                    value: variant,
                };
                // We couldn't find the variant name in scope. NAMING PROBLEM!
                env.problem(Problem::UnrecognizedVariant(loc_name.clone()));

                Pattern::UnrecognizedVariant(loc_name.clone())
            }
        }

        // &IntLiteral(ref num) => match pattern_type {
        //     CaseBranch => Pattern::IntLiteral(*num),
        //     ptype @ Assignment | ptype @ FunctionArg => unsupported_pattern(env, *ptype, region),
        // },

        // &FloatLiteral(ref num) => match pattern_type {
        //     CaseBranch => Pattern::FloatLiteral(*num),
        //     ptype @ Assignment | ptype @ FunctionArg => unsupported_pattern(env, *ptype, region),
        // },

        // &ExactString(ref string) => match pattern_type {
        //     CaseBranch => Pattern::ExactString(string.clone()),
        //     ptype @ Assignment | ptype @ FunctionArg => unsupported_pattern(env, *ptype, region),
        // },
        &Underscore => match pattern_type {
            CaseBranch | FunctionArg => Pattern::Underscore(subs.mk_flex_var()),
            Assignment => unsupported_pattern(env, Assignment, region),
        },

        // &EmptyRecordLiteral => Pattern::EmptyRecordLiteral,
        _ => panic!("TODO finish restoring can_pattern branches"),
    };

    let mut state = State {
        headers: ImMap::default(),
        vars: Vec::new(),
        constraints: Vec::new(),
    };

    add_constraints(&loc_pattern.value, loc_pattern.region, expected, &mut state);

    (
        Located {
            region,
            value: pattern,
        },
        state,
    )
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern<'a>(env: &'a mut Env, pattern_type: PatternType, region: Region) -> Pattern {
    env.problem(Problem::UnsupportedPattern(pattern_type, region));

    Pattern::UnsupportedPattern(region)
}

// CONSTRAIN

pub struct State {
    pub headers: ImMap<Symbol, Located<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
}

fn add_constraints<'a>(
    pattern: &'a ast::Pattern<'a>,
    region: Region,
    expected: PExpected<Type>,
    state: &'a mut State,
) {
    use parse::ast::Pattern::*;

    match pattern {
        Underscore | Malformed(_) | QualifiedIdentifier(_) => {
            // Neither the _ pattern nor malformed ones add any constraints.
            ()
        }
        Identifier(name) => {
            state.headers.insert(
                Symbol::new("TODO pass home into add_constraints, or improve Symbol to not need it for idents in patterns", name),
                Located {
                    region,
                    value: expected.get_type(),
                },
            );
        }
        IntLiteral(_) | HexIntLiteral(_) | OctalIntLiteral(_) | BinaryIntLiteral(_) => {
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

        SpaceBefore(pattern, _) | SpaceAfter(pattern, _) => {
            add_constraints(pattern, region, expected, state)
        }

        Variant(_, _) | Apply(_, _) | RecordDestructure(_) | EmptyRecordLiteral => {
            panic!("TODO add_constraints for {:?}", pattern);
        }
    }
}
