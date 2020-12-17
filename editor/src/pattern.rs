#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]
use crate::ast::{ExprId, FloatVal, IntVal};
use crate::expr::Env;
use crate::pool::{NodeId, Pool, PoolStr, PoolVec};
use roc_can::expr::{unescape_char, Output};
use roc_can::scope::Scope;
use roc_module::symbol::Symbol;
use roc_parse::ast::{StrLiteral, StrSegment};
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::Region;
use roc_types::subs::Variable;

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
    Shadowed {
        shadowed_ident: PoolStr,
        definition: Region,
        shadowed_at: Region,
    },

    /// Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // parse error patterns
    MalformedPattern(MalformedPatternProblem, Region),
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MalformedPatternProblem {
    MalformedInt,
    MalformedFloat,
    MalformedBase(roc_parse::ast::Base),
    Unknown,
    QualifiedIdentifier,
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
                    shadowed_at: region,
                    definition: original_region,
                }
            }
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
                for (_, pattern_id) in arguments.iter(pool) {
                    stack.push(pool.get(*pattern_id));
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

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern<'a>(
    env: &mut Env<'a>,
    pattern_type: PatternType,
    region: Region,
) -> Pattern2 {
    env.problem(Problem::UnsupportedPattern(pattern_type, region));

    Pattern2::UnsupportedPattern(region)
}

fn flatten_str_literal(pool: &mut Pool, literal: &StrLiteral<'_>) -> Pattern2 {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => Pattern2::StrLiteral(PoolStr::new(str_slice, pool)),
        Line(segments) => flatten_str_lines(pool, &[segments]),
        Block(lines) => flatten_str_lines(pool, lines),
    }
}

fn flatten_str_lines(pool: &mut Pool, lines: &[&[StrSegment<'_>]]) -> Pattern2 {
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
