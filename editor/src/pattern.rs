use crate::pool::{NodeId, Pool, PoolStr, PoolVec};
use arraystring::{typenum::U30, ArrayString};
use roc_can::def::Annotation;
use roc_can::expr::Recursive;
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::Variable;
use roc_types::types::Type;

#[derive(Debug)]
pub enum Pattern2 {
    Identifier(Symbol),        // 8B
    NumLiteral(Variable, i64), // 4B + 8B
    IntLiteral(IntVal),        // 16B
    FloatLiteral(FloatVal),    // 16B
    StrLiteral(PoolStr),       // 8B
    Underscore,                // 0B
    AppliedTag {
        whole_var: Variable,                       // 4B
        ext_var: Variable,                         // 4B
        tag_name: PoolStr,                         // 8B
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

pub fn symbols_from_pattern(pool: &Pool, initial: &Pattern2) -> Vec<Symbol> {
    use Pattern2::*;
    let mut symbols = Vec::new();
    let mut stack = vec![initial];

    while let Some(pattern) = stack.pop() {
        match pattern {
            Identifier(symbol) => {
                symbols.push(*symbol);
            }

            AppliedTag { arguments, .. } => {
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
