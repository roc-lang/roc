use crate::can::env::Env;
use crate::can::ident::Lowercase;
use crate::can::num::{finish_parsing_base, finish_parsing_float, finish_parsing_int};
use crate::can::problem::Problem;
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::ident::Ident;
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::VarStore;
use crate::subs::Variable;
use im_rc::Vector;

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(Symbol),
    Tag(Symbol),
    /// TODO replace regular Tag with this
    AppliedTag(Symbol, Vec<Located<Pattern>>),
    IntLiteral(i64),
    FloatLiteral(f64),
    StrLiteral(Box<str>),
    RecordDestructure(Variable, Vec<RecordDestruct>),
    Underscore,

    // Runtime Exceptions
    Shadowed(Located<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDestruct {
    pub var: Variable,
    pub label: Lowercase,
    pub symbol: Symbol,
    pub guard: Option<(Variable, Located<Pattern>)>,
}

pub fn symbols_from_pattern(pattern: &Pattern) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    symbols_from_pattern_help(pattern, &mut symbols);

    symbols
}

pub fn symbols_from_pattern_help(pattern: &Pattern, symbols: &mut Vec<Symbol>) {
    if let Pattern::Identifier(symbol) = pattern {
        symbols.push(symbol.clone());
    }
}

/// Different patterns are supported in different circumstances.
/// For example, when branches can pattern match on number literals, but
/// assignments and function args can't. Underscore is supported in function
/// arg patterns and in when branch patterns, but not in assignments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    TopLevelDef,
    Assignment,
    FunctionArg,
    WhenBranch,
}

pub fn canonicalize_pattern<'a>(
    env: &'a mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    pattern_type: PatternType,
    pattern: &'a ast::Pattern<'a>,
    region: Region,
    shadowable_idents: &'a mut ImMap<Ident, (Symbol, Region)>,
) -> Located<Pattern> {
    use self::PatternType::*;
    use crate::parse::ast::Pattern::*;

    let can_pattern = match &pattern {
        &Identifier(ref name) => {
            match canonicalize_pattern_identifier(name, env, scope, region, shadowable_idents) {
                Ok(symbol) => Pattern::Identifier(symbol),
                Err(loc_shadowed_ident) => Pattern::Shadowed(loc_shadowed_ident),
            }
        }
        &GlobalTag(name) => {
            // Canonicalize the tag's name.
            Pattern::Tag(Symbol::from_global_tag(name))
        }
        &PrivateTag(name) => {
            // Canonicalize the tag's name.
            Pattern::Tag(Symbol::from_private_tag(&env.home, name))
        }
        &FloatLiteral(ref string) => match pattern_type {
            WhenBranch => {
                let float = finish_parsing_float(string)
                    .unwrap_or_else(|_| panic!("TODO handle malformed float pattern"));

                Pattern::FloatLiteral(float)
            }
            ptype @ Assignment | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        &Underscore => match pattern_type {
            WhenBranch | FunctionArg => Pattern::Underscore,
            ptype @ Assignment | ptype @ TopLevelDef => unsupported_pattern(env, ptype, region),
        },

        &IntLiteral(string) => match pattern_type {
            WhenBranch => {
                let int = finish_parsing_int(string)
                    .unwrap_or_else(|_| panic!("TODO handle malformed int pattern"));

                Pattern::IntLiteral(int)
            }
            ptype @ Assignment | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        &NonBase10Literal {
            string,
            base,
            is_negative,
        } => match pattern_type {
            WhenBranch => {
                let int = finish_parsing_base(string, *base)
                    .unwrap_or_else(|_| panic!("TODO handle malformed {:?} pattern", base));

                if *is_negative {
                    Pattern::IntLiteral(-int)
                } else {
                    Pattern::IntLiteral(int)
                }
            }
            ptype @ Assignment | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        &StrLiteral(_string) => match pattern_type {
            WhenBranch => {
                panic!("TODO check whether string pattern is malformed.");
                // Pattern::StrLiteral((*string).into())
            }
            ptype @ Assignment | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        &SpaceBefore(sub_pattern, _) | SpaceAfter(sub_pattern, _) | Nested(sub_pattern) => {
            return canonicalize_pattern(
                env,
                var_store,
                scope,
                pattern_type,
                sub_pattern,
                region,
                shadowable_idents,
            )
        }
        &RecordDestructure(patterns) => {
            let mut fields = Vec::with_capacity(patterns.len());

            for loc_pattern in patterns {
                match loc_pattern.value {
                    Identifier(label) => {
                        let symbol = match canonicalize_pattern_identifier(
                            &label,
                            env,
                            scope,
                            region,
                            shadowable_idents,
                        ) {
                            Ok(symbol) => symbol,
                            Err(loc_shadowed_ident) => {
                                // If any idents are shadowed, consider the entire
                                // destructure pattern shadowed!
                                let _loc_pattern = Located {
                                    region,
                                    value: Pattern::Shadowed(loc_shadowed_ident),
                                };
                                panic!("TODO gather all the shadowing errors, not just the first one, and report them in Problems.");
                            }
                        };

                        fields.push(RecordDestruct {
                            var: var_store.fresh(),
                            label: Lowercase::from(label),
                            symbol,
                            guard: None,
                        });
                    }
                    RecordField(label, loc_guard) => {
                        let symbol = match canonicalize_pattern_identifier(
                            &label,
                            env,
                            scope,
                            region,
                            shadowable_idents,
                        ) {
                            Ok(symbol) => symbol,
                            Err(loc_shadowed_ident) => {
                                // If any idents are shadowed, consider the entire
                                // destructure pattern shadowed!
                                let _loc_pattern = Located {
                                    region,
                                    value: Pattern::Shadowed(loc_shadowed_ident),
                                };
                                panic!("TODO gather all the shadowing errors, not just the first one, and report them in Problems.");
                            }
                        };

                        let can_guard = canonicalize_pattern(
                            env,
                            var_store,
                            scope,
                            pattern_type,
                            &loc_guard.value,
                            loc_guard.region,
                            shadowable_idents,
                        );

                        fields.push(RecordDestruct {
                            var: var_store.fresh(),
                            label: Lowercase::from(label),
                            symbol,
                            guard: Some((var_store.fresh(), can_guard)),
                        });
                    }
                    _ => panic!("invalid pattern in record"),
                }
            }

            Pattern::RecordDestructure(var_store.fresh(), fields)
        }
        &RecordField(_name, _loc_pattern) => {
            unreachable!("should be handled in RecordDestructure");
        }

        _ => panic!("TODO finish restoring can_pattern branch for {:?}", pattern),
    };

    Located {
        region,
        value: can_pattern,
    }
}

pub fn canonicalize_pattern_identifier<'a>(
    name: &'a &str,
    env: &'a mut Env,
    scope: &mut Scope,
    region: Region,
    shadowable_idents: &'a mut ImMap<Ident, (Symbol, Region)>,
) -> Result<Symbol, Located<Ident>> {
    let lowercase_ident = Ident::Unqualified((*name).into());

    // We use shadowable_idents for this, and not scope, because for assignments
    // they are different. When canonicalizing a particular assignment, that new
    // ident is in scope (for recursion) but not shadowable.
    //
    // For example, when canonicalizing (fibonacci = ...), `fibonacci` should be in scope
    // so that it can refer to itself without getting a naming problem, but it should not
    // be in the collection of shadowable idents because you can't shadow yourself!
    match shadowable_idents.get(&lowercase_ident) {
        Some((_, region)) => {
            let loc_shadowed_ident = Located {
                region: *region,
                value: lowercase_ident,
            };

            // This is already in scope, meaning it's about to be shadowed.
            // Shadowing is not allowed!
            env.problem(Problem::Shadowing(loc_shadowed_ident.clone()));

            // Change this Pattern to a Shadowed variant, so that
            // codegen knows to generate a runtime exception here.
            Err(loc_shadowed_ident)
        }
        None => {
            // Make sure we aren't shadowing something in the home module's scope.
            let qualified_ident = Ident::Qualified(env.home.clone(), lowercase_ident.name());

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
                    Err(loc_shadowed_ident)
                }
                None => {
                    let new_ident = qualified_ident.clone();
                    let new_name = qualified_ident.name();
                    let symbol = scope.symbol(&new_name);

                    // This is a fresh identifier that wasn't already in scope.
                    // Add it to scope!
                    let symbol_and_region = (symbol.clone(), region);

                    // Add this to both scope.idents *and* shadowable_idents.
                    // The latter is relevant when recursively canonicalizing
                    // tag application patterns, which can bring multiple
                    // new idents into scope. For example, it's important that
                    // we catch (Blah foo foo) -> … as being an example of shadowing.
                    shadowable_idents.insert(new_ident.clone(), symbol_and_region.clone());
                    scope.idents.insert(new_ident, symbol_and_region);

                    Ok(symbol)
                }
            }
        }
    }
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern(env: &mut Env, pattern_type: PatternType, region: Region) -> Pattern {
    env.problem(Problem::UnsupportedPattern(pattern_type, region));

    Pattern::UnsupportedPattern(region)
}

pub fn remove_idents(pattern: &ast::Pattern, idents: &mut ImMap<Ident, (Symbol, Region)>) {
    use crate::parse::ast::Pattern::*;

    match &pattern {
        Identifier(name) => {
            idents.remove(&(Ident::Unqualified((*name).into())));
        }
        QualifiedIdentifier(_name) => {
            panic!("TODO implement QualifiedIdentifier pattern in remove_idents.");
        }
        Apply(_, _) => {
            panic!("TODO implement Apply pattern in remove_idents.");
            // AppliedVariant(_, Some(loc_args)) => {
            //     for loc_arg in loc_args {
            //         remove_idents(loc_arg.value, idents);
            //     }
            // }
        }
        RecordDestructure(patterns) => {
            for loc_pattern in patterns {
                remove_idents(&loc_pattern.value, idents);
            }
        }
        RecordField(_, loc_pattern) => {
            remove_idents(&loc_pattern.value, idents);
        }
        SpaceBefore(pattern, _) | SpaceAfter(pattern, _) | Nested(pattern) => {
            // Ignore the newline/comment info; it doesn't matter in canonicalization.
            remove_idents(pattern, idents)
        }
        GlobalTag(_)
        | PrivateTag(_)
        | IntLiteral(_)
        | NonBase10Literal { .. }
        | FloatLiteral(_)
        | StrLiteral(_)
        | BlockStrLiteral(_)
        | Malformed(_)
        | Underscore => {}
    }
}

pub fn idents_from_patterns<'a, I>(
    loc_patterns: I,
    scope: &Scope,
) -> Vector<(Ident, (Symbol, Region))>
where
    I: Iterator<Item = &'a Located<ast::Pattern<'a>>>,
{
    let mut answer = Vector::new();

    for loc_pattern in loc_patterns {
        add_idents_from_pattern(&loc_pattern.region, &loc_pattern.value, scope, &mut answer);
    }

    answer
}

/// helper function for idents_from_patterns
fn add_idents_from_pattern<'a>(
    region: &'a Region,
    pattern: &'a ast::Pattern<'a>,
    scope: &'a Scope,
    answer: &'a mut Vector<(Ident, (Symbol, Region))>,
) {
    use crate::parse::ast::Pattern::*;

    match pattern {
        Identifier(name) => {
            let symbol = scope.symbol(&name);

            answer.push_back((Ident::Unqualified((*name).into()), (symbol, *region)));
        }
        QualifiedIdentifier(_name) => {
            panic!("TODO implement QualifiedIdentifier pattern.");
        }
        Apply(_, _) => {
            panic!("TODO implement Apply pattern.");
            // &AppliedVariant(_, ref opt_loc_args) => match opt_loc_args {
            // &None => (),
            // &Some(ref loc_args) => {
            //     for loc_arg in loc_args.iter() {
            //         add_idents_from_pattern(loc_arg, scope, answer);
            //     }
            // }
            // },
        }

        RecordDestructure(patterns) => {
            for loc_pattern in patterns {
                add_idents_from_pattern(&loc_pattern.region, &loc_pattern.value, scope, answer);
            }
        }
        RecordField(name, loc_pattern) => {
            let symbol = scope.symbol(&name);

            answer.push_back((Ident::Unqualified((*name).into()), (symbol, *region)));
            add_idents_from_pattern(&loc_pattern.region, &loc_pattern.value, scope, answer);
        }
        SpaceBefore(pattern, _) | SpaceAfter(pattern, _) | Nested(pattern) => {
            // Ignore the newline/comment info; it doesn't matter in canonicalization.
            add_idents_from_pattern(region, pattern, scope, answer)
        }
        GlobalTag(_)
        | PrivateTag(_)
        | IntLiteral(_)
        | NonBase10Literal { .. }
        | FloatLiteral(_)
        | StrLiteral(_)
        | BlockStrLiteral(_)
        | Malformed(_)
        | Underscore => (),
    }
}
