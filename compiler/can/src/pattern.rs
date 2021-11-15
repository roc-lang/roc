use crate::env::Env;
use crate::expr::{canonicalize_expr, unescape_char, Expr, Output};
use crate::num::{finish_parsing_base, finish_parsing_float, finish_parsing_int};
use crate::scope::Scope;
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_parse::ast::{self, StrLiteral, StrSegment};
use roc_parse::pattern::PatternType;
use roc_problem::can::{MalformedPatternProblem, Problem, RuntimeError};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(Symbol),
    AppliedTag {
        whole_var: Variable,
        ext_var: Variable,
        tag_name: TagName,
        arguments: Vec<(Variable, Located<Pattern>)>,
    },
    RecordDestructure {
        whole_var: Variable,
        ext_var: Variable,
        destructs: Vec<Located<RecordDestruct>>,
    },
    IntLiteral(Variable, Box<str>, i64),
    NumLiteral(Variable, Box<str>, i64),
    FloatLiteral(Variable, Box<str>, f64),
    StrLiteral(Box<str>),
    Underscore,

    // Runtime Exceptions
    Shadowed(Region, Located<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // parse error patterns
    MalformedPattern(MalformedPatternProblem, Region),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDestruct {
    pub var: Variable,
    pub label: Lowercase,
    pub symbol: Symbol,
    pub typ: DestructType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DestructType {
    Required,
    Optional(Variable, Located<Expr>),
    Guard(Variable, Located<Pattern>),
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
            symbols.push(*symbol);
        }

        AppliedTag { arguments, .. } => {
            for (_, nested) in arguments {
                symbols_from_pattern_help(&nested.value, symbols);
            }
        }
        RecordDestructure { destructs, .. } => {
            for destruct in destructs {
                // when a record field has a pattern guard, only symbols in the guard are introduced
                if let DestructType::Guard(_, subpattern) = &destruct.value.typ {
                    symbols_from_pattern_help(&subpattern.value, symbols);
                } else {
                    symbols.push(destruct.value.symbol);
                }
            }
        }

        NumLiteral(_, _, _)
        | IntLiteral(_, _, _)
        | FloatLiteral(_, _, _)
        | StrLiteral(_)
        | Underscore
        | MalformedPattern(_, _)
        | UnsupportedPattern(_) => {}

        Shadowed(_, _) => {}
    }
}

pub fn canonicalize_pattern<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    pattern_type: PatternType,
    pattern: &ast::Pattern<'a>,
    region: Region,
) -> (Output, Located<Pattern>) {
    use roc_parse::ast::Pattern::*;
    use PatternType::*;

    let mut output = Output::default();
    let can_pattern = match pattern {
        Identifier(name) => match scope.introduce(
            (*name).into(),
            &env.exposed_ident_ids,
            &mut env.ident_ids,
            region,
        ) {
            Ok(symbol) => {
                output.references.bound_symbols.insert(symbol);

                Pattern::Identifier(symbol)
            }
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
            Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: TagName::Global((*name).into()),
                arguments: vec![],
            }
        }
        PrivateTag(name) => {
            let ident_id = env.ident_ids.get_or_insert(&(*name).into());

            // Canonicalize the tag's name.
            Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: TagName::Private(Symbol::new(env.home, ident_id)),
                arguments: vec![],
            }
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
                let (new_output, can_pattern) = canonicalize_pattern(
                    env,
                    var_store,
                    scope,
                    pattern_type,
                    &loc_pattern.value,
                    loc_pattern.region,
                );

                output.union(new_output);

                can_patterns.push((var_store.fresh(), can_pattern));
            }

            Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name,
                arguments: can_patterns,
            }
        }

        FloatLiteral(str) => match pattern_type {
            WhenBranch => match finish_parsing_float(str) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedFloat;
                    malformed_pattern(env, problem, region)
                }
                Ok(float) => Pattern::FloatLiteral(var_store.fresh(), (*str).into(), float),
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        Underscore(_) => match pattern_type {
            WhenBranch | FunctionArg => Pattern::Underscore,
            TopLevelDef | DefExpr => bad_underscore(env, region),
        },

        NumLiteral(str) => match pattern_type {
            WhenBranch => match finish_parsing_int(str) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedInt;
                    malformed_pattern(env, problem, region)
                }
                Ok(int) => Pattern::NumLiteral(var_store.fresh(), (*str).into(), int),
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        NonBase10Literal {
            string,
            base,
            is_negative,
        } => match pattern_type {
            WhenBranch => match finish_parsing_base(string, *base, *is_negative) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedBase(*base);
                    malformed_pattern(env, problem, region)
                }
                Ok(int) => {
                    let sign_str = if *is_negative { "-" } else { "" };
                    let int_str = format!("{}{}", sign_str, int.to_string()).into_boxed_str();
                    let i = if *is_negative { -int } else { int };
                    Pattern::IntLiteral(var_store.fresh(), int_str, i)
                }
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        StrLiteral(literal) => match pattern_type {
            WhenBranch => flatten_str_literal(literal),
            ptype => unsupported_pattern(env, ptype, region),
        },

        SpaceBefore(sub_pattern, _) | SpaceAfter(sub_pattern, _) => {
            return canonicalize_pattern(env, var_store, scope, pattern_type, sub_pattern, region)
        }
        RecordDestructure(patterns) => {
            let ext_var = var_store.fresh();
            let whole_var = var_store.fresh();
            let mut destructs = Vec::with_capacity(patterns.len());
            let mut opt_erroneous = None;

            for loc_pattern in patterns.iter() {
                match loc_pattern.value {
                    Identifier(label) => {
                        match scope.introduce(
                            label.into(),
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            region,
                        ) {
                            Ok(symbol) => {
                                output.references.bound_symbols.insert(symbol);

                                destructs.push(Located {
                                    region: loc_pattern.region,
                                    value: RecordDestruct {
                                        var: var_store.fresh(),
                                        label: Lowercase::from(label),
                                        symbol,
                                        typ: DestructType::Required,
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

                    RequiredField(label, loc_guard) => {
                        // a guard does not introduce the label into scope!
                        let symbol = scope.ignore(label.into(), &mut env.ident_ids);
                        let (new_output, can_guard) = canonicalize_pattern(
                            env,
                            var_store,
                            scope,
                            pattern_type,
                            &loc_guard.value,
                            loc_guard.region,
                        );

                        output.union(new_output);

                        destructs.push(Located {
                            region: loc_pattern.region,
                            value: RecordDestruct {
                                var: var_store.fresh(),
                                label: Lowercase::from(label),
                                symbol,
                                typ: DestructType::Guard(var_store.fresh(), can_guard),
                            },
                        });
                    }
                    OptionalField(label, loc_default) => {
                        // an optional DOES introduce the label into scope!
                        match scope.introduce(
                            label.into(),
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            region,
                        ) {
                            Ok(symbol) => {
                                let (can_default, expr_output) = canonicalize_expr(
                                    env,
                                    var_store,
                                    scope,
                                    loc_default.region,
                                    &loc_default.value,
                                );

                                // an optional field binds the symbol!
                                output.references.bound_symbols.insert(symbol);

                                output.union(expr_output);

                                destructs.push(Located {
                                    region: loc_pattern.region,
                                    value: RecordDestruct {
                                        var: var_store.fresh(),
                                        label: Lowercase::from(label),
                                        symbol,
                                        typ: DestructType::Optional(var_store.fresh(), can_default),
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
                    _ => unreachable!("Any other pattern should have given a parse error"),
                }
            }

            // If we encountered an erroneous pattern (e.g. one with shadowing),
            // use the resulting RuntimeError. Otherwise, return a successful record destructure.
            opt_erroneous.unwrap_or(Pattern::RecordDestructure {
                whole_var,
                ext_var,
                destructs,
            })
        }

        RequiredField(_name, _loc_pattern) => {
            unreachable!("should have been handled in RecordDestructure");
        }
        OptionalField(_name, _loc_pattern) => {
            unreachable!("should have been handled in RecordDestructure");
        }

        Malformed(_str) => {
            let problem = MalformedPatternProblem::Unknown;
            malformed_pattern(env, problem, region)
        }

        MalformedIdent(_str, problem) => {
            let problem = MalformedPatternProblem::BadIdent(*problem);
            malformed_pattern(env, problem, region)
        }

        QualifiedIdentifier { .. } => {
            let problem = MalformedPatternProblem::QualifiedIdentifier;
            malformed_pattern(env, problem, region)
        }
    };

    (
        output,
        Located {
            region,
            value: can_pattern,
        },
    )
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern(env: &mut Env, pattern_type: PatternType, region: Region) -> Pattern {
    use roc_problem::can::BadPattern;
    env.problem(Problem::UnsupportedPattern(
        BadPattern::Unsupported(pattern_type),
        region,
    ));

    Pattern::UnsupportedPattern(region)
}

fn bad_underscore(env: &mut Env, region: Region) -> Pattern {
    use roc_problem::can::BadPattern;
    env.problem(Problem::UnsupportedPattern(
        BadPattern::UnderscoreInDef,
        region,
    ));

    Pattern::UnsupportedPattern(region)
}

/// When we detect a malformed pattern like `3.X` or `0b5`,
/// report it to Env and return an UnsupportedPattern runtime error pattern.
fn malformed_pattern(env: &mut Env, problem: MalformedPatternProblem, region: Region) -> Pattern {
    env.problem(Problem::RuntimeError(RuntimeError::MalformedPattern(
        problem, region,
    )));

    Pattern::MalformedPattern(problem, region)
}

pub fn bindings_from_patterns<'a, I>(loc_patterns: I) -> Vec<(Symbol, Region)>
where
    I: Iterator<Item = &'a Located<Pattern>>,
{
    let mut answer = Vec::new();

    for loc_pattern in loc_patterns {
        add_bindings_from_patterns(&loc_pattern.region, &loc_pattern.value, &mut answer);
    }

    answer
}

/// helper function for idents_from_patterns
fn add_bindings_from_patterns(
    region: &Region,
    pattern: &Pattern,
    answer: &mut Vec<(Symbol, Region)>,
) {
    use Pattern::*;

    match pattern {
        Identifier(symbol) => {
            answer.push((*symbol, *region));
        }
        AppliedTag {
            arguments: loc_args,
            ..
        } => {
            for (_, loc_arg) in loc_args {
                add_bindings_from_patterns(&loc_arg.region, &loc_arg.value, answer);
            }
        }
        RecordDestructure { destructs, .. } => {
            for Located {
                region,
                value: RecordDestruct { symbol, .. },
            } in destructs
            {
                answer.push((*symbol, *region));
            }
        }
        NumLiteral(_, _, _)
        | IntLiteral(_, _, _)
        | FloatLiteral(_, _, _)
        | StrLiteral(_)
        | Underscore
        | Shadowed(_, _)
        | MalformedPattern(_, _)
        | UnsupportedPattern(_) => (),
    }
}

fn flatten_str_literal(literal: &StrLiteral<'_>) -> Pattern {
    use ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => Pattern::StrLiteral((*str_slice).into()),
        Line(segments) => flatten_str_lines(&[segments]),
        Block(lines) => flatten_str_lines(lines),
    }
}

fn flatten_str_lines(lines: &[&[StrSegment<'_>]]) -> Pattern {
    use StrSegment::*;

    let mut buf = String::new();

    for line in lines {
        for segment in line.iter() {
            match segment {
                Plaintext(string) => {
                    buf.push_str(string);
                }
                Unicode(loc_digits) => {
                    todo!("parse unicode digits {:?}", loc_digits);
                }
                Interpolated(loc_expr) => {
                    return Pattern::UnsupportedPattern(loc_expr.region);
                }
                EscapedChar(escaped) => buf.push(unescape_char(escaped)),
            }
        }
    }

    Pattern::StrLiteral(buf.into())
}
