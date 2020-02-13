use crate::collections::{default_hasher, MutMap};
use crate::module::symbol::Symbol;
use crate::region::Region;
use crate::solve::SolvedType;
use std::collections::HashMap;

/// Keep this up to date by hand!
///
const NUM_BUILTIN_IMPORTS: usize = 7;

pub fn types() -> MutMap<Symbol, (SolvedType, Region)> {
    let mut types = HashMap::with_capacity_and_hasher(NUM_BUILTIN_IMPORTS, default_hasher());

    // TODO instead of using Region::zero for all of these,
    // instead use the Region where they were defined in their
    // source .roc files! This can give nicer error messages.

    // Str module

    // Str : [ @Str ]
    types.insert(
        Symbol::STR_STR,
        (
            SolvedType::Alias(
                Symbol::STR_STR,
                Vec::new(),
                Box::new(SolvedType::Apply(Symbol::STR_AT_STR, Vec::new())),
            ),
            Region::zero(),
        ),
    );

    // Num module

    // Num range : [ @Num range ]
    types.insert(
        Symbol::NUM_NUM,
        (
            SolvedType::Alias(
                Symbol::NUM_NUM,
                vec!["range".into()],
                Box::new(SolvedType::Apply(
                    Symbol::NUM_AT_NUM,
                    vec![SolvedType::Rigid("range".into())],
                )),
            ),
            Region::zero(),
        ),
    );

    // Int module

    // Integer : [ @Integer ]
    types.insert(
        Symbol::INT_INTEGER,
        (
            SolvedType::Alias(
                Symbol::INT_INTEGER,
                Vec::new(),
                Box::new(SolvedType::Apply(Symbol::INT_AT_INTEGER, Vec::new())),
            ),
            Region::zero(),
        ),
    );

    // Int : Num Integer
    types.insert(
        Symbol::INT_INT,
        (
            SolvedType::Alias(
                Symbol::INT_INT,
                Vec::new(),
                Box::new(SolvedType::Apply(
                    Symbol::NUM_NUM,
                    vec![SolvedType::Apply(Symbol::INT_INTEGER, Vec::new())],
                )),
            ),
            Region::zero(),
        ),
    );

    // Float module

    // FloatingPoint : [ @FloatingPoint ]
    types.insert(
        Symbol::FLOAT_FLOATINGPOINT,
        (
            SolvedType::Alias(
                Symbol::FLOAT_FLOATINGPOINT,
                Vec::new(),
                Box::new(SolvedType::Apply(
                    Symbol::FLOAT_AT_FLOATINTPOINT,
                    Vec::new(),
                )),
            ),
            Region::zero(),
        ),
    );

    // Float : Num FloatingPoint
    types.insert(
        Symbol::FLOAT_FLOAT,
        (
            SolvedType::Alias(
                Symbol::FLOAT_FLOAT,
                Vec::new(),
                Box::new(SolvedType::Apply(
                    Symbol::NUM_NUM,
                    vec![SolvedType::Apply(Symbol::FLOAT_FLOATINGPOINT, Vec::new())],
                )),
            ),
            Region::zero(),
        ),
    );

    // highest : Float
    types.insert(
        Symbol::FLOAT_HIGHEST,
        (
            SolvedType::Apply(Symbol::FLOAT_FLOAT, Vec::new()),
            Region::zero(),
        ),
    );

    // lowest : Float
    types.insert(
        Symbol::FLOAT_LOWEST,
        (
            SolvedType::Apply(Symbol::FLOAT_FLOAT, Vec::new()),
            Region::zero(),
        ),
    );

    // List module

    // List elem : [ @List elem ]
    types.insert(
        Symbol::LIST_LIST,
        (
            SolvedType::Alias(
                Symbol::LIST_LIST,
                vec!["elem".into()],
                Box::new(SolvedType::Apply(
                    Symbol::LIST_AT_LIST,
                    vec![SolvedType::Rigid("elem".into())],
                )),
            ),
            Region::zero(),
        ),
    );

    types
}
