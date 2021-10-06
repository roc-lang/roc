#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]

use bumpalo::collections::Vec as BumpVec;
use roc_can::expr::unescape_char;
use roc_can::num::{finish_parsing_base, finish_parsing_float, finish_parsing_int};
use roc_collections::all::BumpMap;
use roc_module::symbol::{Interns, Symbol};
use roc_parse::ast::{StrLiteral, StrSegment};
use roc_parse::pattern::PatternType;
use roc_problem::can::{MalformedPatternProblem, Problem, RuntimeError};
use roc_region::all::Region;
use roc_types::subs::Variable;

use crate::ast_error::{ASTResult, UnexpectedPattern2Variant};
use crate::constrain::Constraint;
use crate::lang::core::expr::expr_to_expr2::to_expr_id;
use crate::lang::env::Env;
use crate::lang::scope::Scope;
use crate::mem_pool::pool::{NodeId, Pool};
use crate::mem_pool::pool_str::PoolStr;
use crate::mem_pool::pool_vec::PoolVec;
use crate::mem_pool::shallow_clone::ShallowClone;

use super::expr::expr2::{ExprId, FloatVal, IntVal};
use super::expr::output::Output;
use super::types::Type2;

pub type PatternId = NodeId<Pattern2>;

#[derive(Debug)]
pub enum Pattern2 {
    Identifier(Symbol),        // 8B
    NumLiteral(Variable, i64), // 4B + 8B
    IntLiteral(IntVal),        // 16B
    FloatLiteral(FloatVal),    // 16B
    StrLiteral(PoolStr),       // 8B
    Underscore,                // 0B
    GlobalTag {
        whole_var: Variable,                       // 4B
        ext_var: Variable,                         // 4B
        tag_name: PoolStr,                         // 8B
        arguments: PoolVec<(Variable, PatternId)>, // 8B
    },
    PrivateTag {
        whole_var: Variable,                       // 4B
        ext_var: Variable,                         // 4B
        tag_name: Symbol,                          // 8B
        arguments: PoolVec<(Variable, PatternId)>, // 8B
    },
    RecordDestructure {
        whole_var: Variable,                // 4B
        ext_var: Variable,                  // 4B
        destructs: PoolVec<RecordDestruct>, // 8B
    },

    // Runtime Exceptions
    // TODO: figure out how to better handle regions
    // to keep this member under 32. With 2 Regions
    // it ends up at size 40
    Shadowed {
        shadowed_ident: PoolStr,
        // definition: Region,
        // shadowed_at: Region,
    },

    /// Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // parse error patterns
    MalformedPattern(MalformedPatternProblem, Region),
}

impl ShallowClone for Pattern2 {
    fn shallow_clone(&self) -> Self {
        todo!()
    }
}

#[derive(Debug)]
pub struct PatternState2<'a> {
    pub headers: BumpMap<Symbol, Type2>,
    pub vars: BumpVec<'a, Variable>,
    pub constraints: BumpVec<'a, Constraint<'a>>,
}

#[derive(Debug)]
pub struct RecordDestruct {
    pub var: Variable,             // 4B
    pub label: PoolStr,            // 8B
    pub symbol: Symbol,            // 8B
    pub typ: NodeId<DestructType>, // 4B
}

#[derive(Clone, Debug)]
pub enum DestructType {
    Required,
    Optional(Variable, ExprId), // 4B + 4B
    Guard(Variable, PatternId), // 4B + 4B
}

pub fn as_pattern_id<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    pattern_id: PatternId,
    pattern_type: PatternType,
    pattern: &roc_parse::ast::Pattern<'a>,
    region: Region,
) -> Output {
    let (output, can_pattern) = to_pattern2(env, scope, pattern_type, pattern, region);

    env.pool[pattern_id] = can_pattern;
    env.set_region(pattern_id, region);

    output
}

pub fn to_pattern_id<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
    pattern: &roc_parse::ast::Pattern<'a>,
    region: Region,
) -> (Output, PatternId) {
    let (output, can_pattern) = to_pattern2(env, scope, pattern_type, pattern, region);

    let pattern_id = env.pool.add(can_pattern);
    env.set_region(pattern_id, region);

    (output, pattern_id)
}

pub fn to_pattern2<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
    pattern: &roc_parse::ast::Pattern<'a>,
    region: Region,
) -> (Output, Pattern2) {
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

                Pattern2::Identifier(symbol)
            }
            Err((original_region, shadow)) => {
                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                    original_region,
                    shadow: shadow.clone(),
                }));

                let name: &str = shadow.value.as_ref();

                Pattern2::Shadowed {
                    shadowed_ident: PoolStr::new(name, env.pool),
                }
            }
        },

        QualifiedIdentifier { .. } => {
            let problem = MalformedPatternProblem::QualifiedIdentifier;
            malformed_pattern(env, problem, region)
        }

        Underscore(_) => match pattern_type {
            WhenBranch | FunctionArg => Pattern2::Underscore,
            TopLevelDef | DefExpr => underscore_in_def(env, region),
        },

        FloatLiteral(ref string) => match pattern_type {
            WhenBranch => match finish_parsing_float(string) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedFloat;
                    malformed_pattern(env, problem, region)
                }
                Ok(float) => Pattern2::FloatLiteral(FloatVal::F64(float)),
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        NumLiteral(string) => match pattern_type {
            WhenBranch => match finish_parsing_int(string) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedInt;
                    malformed_pattern(env, problem, region)
                }
                Ok(int) => Pattern2::NumLiteral(env.var_store.fresh(), int),
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
                    if *is_negative {
                        Pattern2::IntLiteral(IntVal::I64(-int))
                    } else {
                        Pattern2::IntLiteral(IntVal::I64(int))
                    }
                }
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        StrLiteral(literal) => match pattern_type {
            WhenBranch => flatten_str_literal(env.pool, literal),
            ptype => unsupported_pattern(env, ptype, region),
        },

        GlobalTag(name) => {
            // Canonicalize the tag's name.
            Pattern2::GlobalTag {
                whole_var: env.var_store.fresh(),
                ext_var: env.var_store.fresh(),
                tag_name: PoolStr::new(name, env.pool),
                arguments: PoolVec::empty(env.pool),
            }
        }
        PrivateTag(name) => {
            let ident_id = env.ident_ids.get_or_insert(&(*name).into());

            // Canonicalize the tag's name.
            Pattern2::PrivateTag {
                whole_var: env.var_store.fresh(),
                ext_var: env.var_store.fresh(),
                tag_name: Symbol::new(env.home, ident_id),
                arguments: PoolVec::empty(env.pool),
            }
        }

        Apply(tag, patterns) => {
            let can_patterns = PoolVec::with_capacity(patterns.len() as u32, env.pool);
            for (loc_pattern, node_id) in (*patterns).iter().zip(can_patterns.iter_node_ids()) {
                let (new_output, can_pattern) = to_pattern2(
                    env,
                    scope,
                    pattern_type,
                    &loc_pattern.value,
                    loc_pattern.region,
                );

                output.union(new_output);

                let can_pattern_id = env.pool.add(can_pattern);

                env.pool[node_id] = (env.var_store.fresh(), can_pattern_id);
            }

            match tag.value {
                GlobalTag(name) => Pattern2::GlobalTag {
                    whole_var: env.var_store.fresh(),
                    ext_var: env.var_store.fresh(),
                    tag_name: PoolStr::new(name, env.pool),
                    arguments: can_patterns,
                },
                PrivateTag(name) => {
                    let ident_id = env.ident_ids.get_or_insert(&name.into());

                    Pattern2::PrivateTag {
                        whole_var: env.var_store.fresh(),
                        ext_var: env.var_store.fresh(),
                        tag_name: Symbol::new(env.home, ident_id),
                        arguments: can_patterns,
                    }
                }
                _ => unreachable!("Other patterns cannot be applied"),
            }
        }

        RecordDestructure(patterns) => {
            let ext_var = env.var_store.fresh();
            let whole_var = env.var_store.fresh();
            let destructs = PoolVec::with_capacity(patterns.len() as u32, env.pool);
            let opt_erroneous = None;

            for (node_id, loc_pattern) in destructs.iter_node_ids().zip((*patterns).iter()) {
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

                                let destruct = RecordDestruct {
                                    var: env.var_store.fresh(),
                                    label: PoolStr::new(label, env.pool),
                                    symbol,
                                    typ: env.pool.add(DestructType::Required),
                                };

                                env.pool[node_id] = destruct;
                                env.set_region(node_id, loc_pattern.region);
                            }
                            Err((original_region, shadow)) => {
                                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                                    original_region,
                                    shadow: shadow.clone(),
                                }));

                                // let shadowed = Pattern2::Shadowed {
                                // definition: original_region,
                                // shadowed_at: loc_pattern.region,
                                // shadowed_ident: shadow.value,
                                // };

                                // No matter what the other patterns
                                // are, we're definitely shadowed and will
                                // get a runtime exception as soon as we
                                // encounter the first bad pattern.
                                // opt_erroneous = Some();
                                // env.pool[node_id] = sha;
                                // env.set_region(node_id, loc_pattern.region);
                                todo!("we must both report/store the problem, but also not lose any information")
                            }
                        };
                    }

                    RequiredField(label, loc_guard) => {
                        // a guard does not introduce the label into scope!
                        let symbol = scope.ignore(label.into(), &mut env.ident_ids);
                        let (new_output, can_guard) = to_pattern_id(
                            env,
                            scope,
                            pattern_type,
                            &loc_guard.value,
                            loc_guard.region,
                        );

                        let destruct = RecordDestruct {
                            var: env.var_store.fresh(),
                            label: PoolStr::new(label, env.pool),
                            symbol,
                            typ: env
                                .pool
                                .add(DestructType::Guard(env.var_store.fresh(), can_guard)),
                        };

                        output.union(new_output);

                        env.pool[node_id] = destruct;
                        env.set_region(node_id, loc_pattern.region);
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
                                let (can_default, expr_output) =
                                    to_expr_id(env, scope, &loc_default.value, loc_default.region);

                                // an optional field binds the symbol!
                                output.references.bound_symbols.insert(symbol);

                                output.union(expr_output);

                                let destruct = RecordDestruct {
                                    var: env.var_store.fresh(),
                                    label: PoolStr::new(label, env.pool),
                                    symbol,
                                    typ: env.pool.add(DestructType::Optional(
                                        env.var_store.fresh(),
                                        can_default,
                                    )),
                                };

                                env.pool[node_id] = destruct;
                                env.set_region(node_id, loc_pattern.region);
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
                                // opt_erroneous = Some(Pattern::Shadowed(original_region, shadow));
                                todo!("must report problem but also not loose any information")
                            }
                        };
                    }
                    _ => unreachable!("Any other pattern should have given a parse error"),
                }
            }

            // If we encountered an erroneous pattern (e.g. one with shadowing),
            // use the resulting RuntimeError. Otherwise, return a successful record destructure.
            opt_erroneous.unwrap_or(Pattern2::RecordDestructure {
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

        MalformedIdent(_str, bad_ident) => {
            let problem = MalformedPatternProblem::BadIdent(*bad_ident);
            malformed_pattern(env, problem, region)
        }

        SpaceBefore(sub_pattern, _) | SpaceAfter(sub_pattern, _) => {
            return to_pattern2(env, scope, pattern_type, sub_pattern, region)
        }
    };

    (output, can_pattern)
}

pub fn symbols_from_pattern(pool: &Pool, initial: &Pattern2) -> Vec<Symbol> {
    use Pattern2::*;
    let mut symbols = Vec::new();
    let mut stack = vec![initial];

    while let Some(pattern) = stack.pop() {
        match pattern {
            Identifier(symbol) => {
                symbols.push(*symbol);
            }

            GlobalTag { arguments, .. } | PrivateTag { arguments, .. } => {
                for (_, pat_id) in arguments.iter(pool) {
                    let pat = pool.get(*pat_id);
                    stack.push(pat);
                }
            }

            RecordDestructure { destructs, .. } => {
                for destruct in destructs.iter(pool) {
                    let destruct_type = pool.get(destruct.typ);

                    if let DestructType::Guard(_, subpattern_id) = &destruct_type {
                        let subpattern = pool.get(*subpattern_id);
                        stack.push(subpattern);
                    } else {
                        symbols.push(destruct.symbol);
                    }
                }
            }

            NumLiteral(_, _)
            | IntLiteral(_)
            | FloatLiteral(_)
            | StrLiteral(_)
            | Underscore
            | MalformedPattern(_, _)
            | Shadowed { .. }
            | UnsupportedPattern(_) => {}
        }
    }

    symbols
}

pub fn get_identifier_string(pattern: &Pattern2, interns: &Interns) -> ASTResult<String> {
    match pattern {
        Pattern2::Identifier(symbol) => Ok(symbol.ident_str(interns).to_string()),
        other => UnexpectedPattern2Variant {
            required_pattern2: "Identifier".to_string(),
            encountered_pattern2: format!("{:?}", other),
        }
        .fail()?,
    }
}

pub fn symbols_and_variables_from_pattern(
    pool: &Pool,
    initial: &Pattern2,
    initial_var: Variable,
) -> Vec<(Symbol, Variable)> {
    use Pattern2::*;
    let mut symbols = Vec::new();
    let mut stack = vec![(initial_var, initial)];

    while let Some((variable, pattern)) = stack.pop() {
        match pattern {
            Identifier(symbol) => {
                symbols.push((*symbol, variable));
            }

            GlobalTag { arguments, .. } | PrivateTag { arguments, .. } => {
                for (var, pat_id) in arguments.iter(pool) {
                    let pat = pool.get(*pat_id);
                    stack.push((*var, pat));
                }
            }

            RecordDestructure { destructs, .. } => {
                for destruct in destructs.iter(pool) {
                    let destruct_type = pool.get(destruct.typ);

                    if let DestructType::Guard(_, subpattern_id) = &destruct_type {
                        let subpattern = pool.get(*subpattern_id);
                        stack.push((destruct.var, subpattern));
                    } else {
                        symbols.push((destruct.symbol, destruct.var));
                    }
                }
            }

            NumLiteral(_, _)
            | IntLiteral(_)
            | FloatLiteral(_)
            | StrLiteral(_)
            | Underscore
            | MalformedPattern(_, _)
            | Shadowed { .. }
            | UnsupportedPattern(_) => {}
        }
    }

    symbols
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern<'a>(
    env: &mut Env<'a>,
    pattern_type: PatternType,
    region: Region,
) -> Pattern2 {
    use roc_problem::can::BadPattern;
    env.problem(Problem::UnsupportedPattern(
        BadPattern::Unsupported(pattern_type),
        region,
    ));

    Pattern2::UnsupportedPattern(region)
}

fn underscore_in_def<'a>(env: &mut Env<'a>, region: Region) -> Pattern2 {
    use roc_problem::can::BadPattern;
    env.problem(Problem::UnsupportedPattern(
        BadPattern::UnderscoreInDef,
        region,
    ));

    Pattern2::UnsupportedPattern(region)
}

pub(crate) fn flatten_str_literal(pool: &mut Pool, literal: &StrLiteral<'_>) -> Pattern2 {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => Pattern2::StrLiteral(PoolStr::new(str_slice, pool)),
        Line(segments) => flatten_str_lines(pool, &[segments]),
        Block(lines) => flatten_str_lines(pool, lines),
    }
}

pub(crate) fn flatten_str_lines(pool: &mut Pool, lines: &[&[StrSegment<'_>]]) -> Pattern2 {
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
                    return Pattern2::UnsupportedPattern(loc_expr.region);
                }
                EscapedChar(escaped) => buf.push(unescape_char(escaped)),
            }
        }
    }

    Pattern2::StrLiteral(PoolStr::new(&buf, pool))
}

/// When we detect a malformed pattern like `3.X` or `0b5`,
/// report it to Env and return an UnsupportedPattern runtime error pattern.
fn malformed_pattern<'a>(
    env: &mut Env<'a>,
    problem: MalformedPatternProblem,
    region: Region,
) -> Pattern2 {
    env.problem(Problem::RuntimeError(RuntimeError::MalformedPattern(
        problem, region,
    )));

    Pattern2::MalformedPattern(problem, region)
}
