use crate::can::ident::TagName;
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

    let mut add_type = |symbol, typ| {
        debug_assert!(
            !types.contains_key(&symbol),
            "Duplicate type definition for {:?}",
            symbol
        );

        // TODO instead of using Region::zero for all of these,
        // instead use the Region where they were defined in their
        // source .roc files! This can give nicer error messages.
        types.insert(symbol, (typ, Region::zero()));
    };

    // Num module

    // Num range : [ @Num range ]
    add_type(
        Symbol::NUM_NUM,
        SolvedType::Alias(
            Symbol::NUM_NUM,
            vec!["range".into()],
            Box::new(SolvedType::Apply(
                Symbol::NUM_AT_NUM,
                vec![SolvedType::Rigid("range".into())],
            )),
        ),
    );

    // Int module

    // Integer : [ @Integer ]
    add_type(
        Symbol::INT_INTEGER,
        SolvedType::Alias(
            Symbol::INT_INTEGER,
            Vec::new(),
            Box::new(SolvedType::Apply(Symbol::INT_AT_INTEGER, Vec::new())),
        ),
    );

    // Int : Num Integer
    add_type(
        Symbol::INT_INT,
        SolvedType::Alias(
            Symbol::INT_INT,
            Vec::new(),
            Box::new(SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![SolvedType::Apply(Symbol::INT_INTEGER, Vec::new())],
            )),
        ),
    );

    // highest : Float
    add_type(Symbol::INT_HIGHEST, int_type());

    // lowest : Float
    add_type(Symbol::INT_LOWEST, int_type());

    // Float module

    // FloatingPoint : [ @FloatingPoint ]
    add_type(
        Symbol::FLOAT_FLOATINGPOINT,
        SolvedType::Alias(
            Symbol::FLOAT_FLOATINGPOINT,
            Vec::new(),
            Box::new(SolvedType::Apply(
                Symbol::FLOAT_AT_FLOATINGPOINT,
                Vec::new(),
            )),
        ),
    );

    // Float : Num FloatingPoint
    add_type(
        Symbol::FLOAT_FLOAT,
        SolvedType::Alias(
            Symbol::FLOAT_FLOAT,
            Vec::new(),
            Box::new(SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![SolvedType::Apply(Symbol::FLOAT_FLOATINGPOINT, Vec::new())],
            )),
        ),
    );

    // div : Float -> Float
    add_type(
        Symbol::FLOAT_DIV,
        SolvedType::Func(vec![float_type(), float_type()], Box::new(float_type())),
    );

    // highest : Float
    add_type(Symbol::FLOAT_HIGHEST, float_type());

    // lowest : Float
    add_type(Symbol::FLOAT_LOWEST, float_type());

    // Bool module

    // Bool : [ True, False ]
    add_type(
        Symbol::BOOL_BOOL,
        SolvedType::Alias(
            Symbol::BOOL_BOOL,
            Vec::new(),
            Box::new(SolvedType::TagUnion(
                vec![
                    (TagName::Global("True".into()), Vec::new()),
                    (TagName::Global("False".into()), Vec::new()),
                ],
                Box::new(SolvedType::EmptyTagUnion),
            )),
        ),
    );

    // and : Bool, Bool -> Bool
    add_type(
        Symbol::BOOL_AND,
        SolvedType::Func(vec![bool_type(), bool_type()], Box::new(bool_type())),
    );

    // or : Bool, Bool -> Bool
    add_type(
        Symbol::BOOL_OR,
        SolvedType::Func(vec![bool_type(), bool_type()], Box::new(bool_type())),
    );

    // Str module

    // Str : [ @Str ]
    add_type(
        Symbol::STR_STR,
        SolvedType::Alias(
            Symbol::STR_STR,
            Vec::new(),
            Box::new(SolvedType::Apply(Symbol::STR_AT_STR, Vec::new())),
        ),
    );

    // isEmpty : Str -> Bool
    add_type(
        Symbol::STR_ISEMPTY,
        SolvedType::Func(vec![str_type()], Box::new(bool_type())),
    );

    // List module

    // List elem : [ @List elem ]
    add_type(
        Symbol::LIST_LIST,
        SolvedType::Alias(
            Symbol::LIST_LIST,
            vec!["elem".into()],
            Box::new(SolvedType::Apply(
                Symbol::LIST_AT_LIST,
                vec![SolvedType::Rigid("elem".into())],
            )),
        ),
    );

    // isEmpty : List * -> Bool
    add_type(
        Symbol::LIST_ISEMPTY,
        SolvedType::Func(
            vec![SolvedType::Apply(
                Symbol::LIST_LIST,
                vec![SolvedType::Wildcard],
            )],
            Box::new(bool_type()),
        ),
    );

    types
}

#[inline(always)]
fn float_type() -> SolvedType {
    SolvedType::Apply(Symbol::FLOAT_FLOAT, Vec::new())
}

#[inline(always)]
fn int_type() -> SolvedType {
    SolvedType::Apply(Symbol::INT_INT, Vec::new())
}

#[inline(always)]
fn bool_type() -> SolvedType {
    SolvedType::Apply(Symbol::BOOL_BOOL, Vec::new())
}

#[inline(always)]
fn str_type() -> SolvedType {
    SolvedType::Apply(Symbol::STR_STR, Vec::new())
}
