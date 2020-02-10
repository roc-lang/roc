use crate::can::env::Env;
use crate::can::ident::{Ident, Lowercase, TagName};
use crate::can::num::{finish_parsing_base, finish_parsing_float, finish_parsing_int};
use crate::can::problem::{Problem, RuntimeError};
use crate::can::scope::Scope;
use crate::module::symbol::Symbol;
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::VarStore;
use crate::subs::Variable;

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(Symbol),
    AppliedTag(Variable, TagName, Vec<(Variable, Located<Pattern>)>),
    IntLiteral(i64),
    FloatLiteral(f64),
    StrLiteral(Box<str>),
    RecordDestructure(Variable, Vec<Located<RecordDestruct>>),
    Underscore,

    // Runtime Exceptions
    Shadowed(Region, Located<Ident>),
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
    use Pattern::*;

    match pattern {
        Identifier(symbol) => {
            symbols.push(symbol.clone());
        }

        AppliedTag(_, _, arguments) => {
            for (_, nested) in arguments {
                symbols_from_pattern_help(&nested.value, symbols);
            }
        }
        RecordDestructure(_, destructs) => {
            for destruct in destructs {
                symbols.push(destruct.value.symbol.clone());
            }
        }

        IntLiteral(_) | FloatLiteral(_) | StrLiteral(_) | Underscore | UnsupportedPattern(_) => {}

        Shadowed(_, _) => {}
    }
}

/// Different patterns are supported in different circumstances.
/// For example, when branches can pattern match on number literals, but
/// assignments and function args can't. Underscore is supported in function
/// arg patterns and in when branch patterns, but not in assignments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    TopLevelDef,
    DefExpr,
    FunctionArg,
    WhenBranch,
}

pub fn canonicalize_pattern<'a>(
    env: &mut Env<'a>,
    var_store: &VarStore,
    scope: &mut Scope,
    pattern_type: PatternType,
    pattern: &ast::Pattern<'a>,
    region: Region,
) -> Located<Pattern> {
    use crate::parse::ast::Pattern::*;
    use PatternType::*;

    let can_pattern = match pattern {
        Identifier(name) => match scope.introduce(
            (*name).into(),
            &env.exposed_ident_ids,
            &mut env.ident_ids,
            region,
        ) {
            Ok(symbol) => Pattern::Identifier(symbol),
            Err((original_region, shadow)) => {
                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                    original_region,
                    shadow: shadow.clone(),
                }));

                Pattern::Shadowed(original_region, shadow)
            }
        },
        GlobalTag(name) => {
            // Canonicalize the tag's name.
            Pattern::AppliedTag(var_store.fresh(), TagName::Global((*name).into()), vec![])
        }
        PrivateTag(name) => {
            let ident_id = env.ident_ids.get_or_insert(&(*name).into());

            // Canonicalize the tag's name.
            Pattern::AppliedTag(
                var_store.fresh(),
                TagName::Private(Symbol::new(env.home, ident_id)),
                vec![],
            )
        }
        Apply(tag, patterns) => {
            let tag_name = match tag.value {
                GlobalTag(name) => TagName::Global(name.into()),
                PrivateTag(name) => {
                    let ident_id = env.ident_ids.get_or_insert(&name.into());

                    TagName::Private(Symbol::new(env.home, ident_id))
                }
                _ => unreachable!("Other patterns cannot be applied"),
            };

            let mut can_patterns = Vec::with_capacity(patterns.len());
            for loc_pattern in *patterns {
                can_patterns.push((
                    var_store.fresh(),
                    canonicalize_pattern(
                        env,
                        var_store,
                        scope,
                        pattern_type,
                        &loc_pattern.value,
                        loc_pattern.region,
                    ),
                ));
            }

            Pattern::AppliedTag(var_store.fresh(), tag_name, can_patterns)
        }

        FloatLiteral(ref string) => match pattern_type {
            WhenBranch => {
                let float = finish_parsing_float(string)
                    .unwrap_or_else(|_| panic!("TODO handle malformed float pattern"));

                Pattern::FloatLiteral(float)
            }
            ptype @ DefExpr | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        Underscore => match pattern_type {
            WhenBranch | FunctionArg => Pattern::Underscore,
            ptype @ DefExpr | ptype @ TopLevelDef => unsupported_pattern(env, ptype, region),
        },

        IntLiteral(string) => match pattern_type {
            WhenBranch => {
                let int = finish_parsing_int(string)
                    .unwrap_or_else(|_| panic!("TODO handle malformed int pattern"));

                Pattern::IntLiteral(int)
            }
            ptype @ DefExpr | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        NonBase10Literal {
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
            ptype @ DefExpr | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        StrLiteral(_string) => match pattern_type {
            WhenBranch => {
                panic!("TODO check whether string pattern is malformed.");
                // Pattern::StrLiteral((*string).into())
            }
            ptype @ DefExpr | ptype @ TopLevelDef | ptype @ FunctionArg => {
                unsupported_pattern(env, ptype, region)
            }
        },

        SpaceBefore(sub_pattern, _) | SpaceAfter(sub_pattern, _) | Nested(sub_pattern) => {
            return canonicalize_pattern(env, var_store, scope, pattern_type, sub_pattern, region)
        }
        RecordDestructure(patterns) => {
            let ext_var = var_store.fresh();
            let mut fields = Vec::with_capacity(patterns.len());
            let mut opt_erroneous = None;

            for loc_pattern in *patterns {
                match loc_pattern.value {
                    Identifier(label) => {
                        match scope.introduce(
                            label.into(),
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            region,
                        ) {
                            Ok(symbol) => {
                                fields.push(Located {
                                    region: loc_pattern.region,
                                    value: RecordDestruct {
                                        var: var_store.fresh(),
                                        label: Lowercase::from(label),
                                        symbol,
                                        guard: None,
                                    },
                                });
                            }
                            Err((original_region, shadow)) => {
                                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                                    original_region,
                                    shadow: shadow.clone(),
                                }));

                                // No matter what the other patterns
                                // are, we're definitely shadowed and will
                                // get a runtime exception as soon as we
                                // encounter the first bad pattern.
                                opt_erroneous = Some(Pattern::Shadowed(original_region, shadow));
                            }
                        };
                    }
                    RecordField(label, loc_guard) => {
                        match scope.introduce(
                            label.into(),
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            region,
                        ) {
                            Ok(symbol) => {
                                let can_guard = canonicalize_pattern(
                                    env,
                                    var_store,
                                    scope,
                                    pattern_type,
                                    &loc_guard.value,
                                    loc_guard.region,
                                );

                                fields.push(Located {
                                    region: loc_pattern.region,
                                    value: RecordDestruct {
                                        var: var_store.fresh(),
                                        label: Lowercase::from(label),
                                        symbol,
                                        guard: Some((var_store.fresh(), can_guard)),
                                    },
                                });
                            }
                            Err((original_region, shadow)) => {
                                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                                    original_region,
                                    shadow: shadow.clone(),
                                }));

                                // No matter what the other patterns
                                // are, we're definitely shadowed and will
                                // get a runtime exception as soon as we
                                // encounter the first bad pattern.
                                opt_erroneous = Some(Pattern::Shadowed(original_region, shadow));
                            }
                        };
                    }
                    _ => panic!("invalid pattern in record"),
                }
            }

            // If we encountered an erroneous pattern (e.g. one with shadowing),
            // use the resulting RuntimeError. Otherwise, return a successful record destructure.
            opt_erroneous.unwrap_or_else(|| Pattern::RecordDestructure(ext_var, fields))
        }
        RecordField(_name, _loc_pattern) => {
            unreachable!("should have been handled in RecordDestructure");
        }

        _ => panic!("TODO finish restoring can_pattern branch for {:?}", pattern),
    };

    Located {
        region,
        value: can_pattern,
    }
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern<'a>(
    env: &mut Env<'a>,
    pattern_type: PatternType,
    region: Region,
) -> Pattern {
    env.problem(Problem::UnsupportedPattern(pattern_type, region));

    Pattern::UnsupportedPattern(region)
}

pub fn bindings_from_patterns<'a, I>(loc_patterns: I, scope: &Scope) -> Vec<(Symbol, Region)>
where
    I: Iterator<Item = &'a Located<Pattern>>,
{
    let mut answer = Vec::new();

    for loc_pattern in loc_patterns {
        add_bindings_from_patterns(&loc_pattern.region, &loc_pattern.value, scope, &mut answer);
    }

    answer
}

/// helper function for idents_from_patterns
fn add_bindings_from_patterns(
    region: &Region,
    pattern: &Pattern,
    scope: &Scope,
    answer: &mut Vec<(Symbol, Region)>,
) {
    use Pattern::*;

    match pattern {
        Identifier(symbol) => {
            answer.push((*symbol, *region));
        }
        AppliedTag(_, _, loc_args) => {
            for (_, loc_arg) in loc_args {
                add_bindings_from_patterns(&loc_arg.region, &loc_arg.value, scope, answer);
            }
        }
        RecordDestructure(_, destructs) => {
            for Located {
                region,
                value: RecordDestruct { symbol, .. },
            } in destructs
            {
                answer.push((*symbol, *region));
            }
        }
        IntLiteral(_)
        | FloatLiteral(_)
        | StrLiteral(_)
        | Underscore
        | Shadowed(_, _)
        | UnsupportedPattern(_) => (),
    }
}
