use crate::can::ident::TagName;
use crate::collections::{default_hasher, MutMap};
use crate::module::symbol::Symbol;
use crate::region::{Located, Region};
use crate::solve::{BuiltinAlias, SolvedType};
use crate::subs::VarId;
use std::collections::HashMap;

/// Keep this up to date by hand!
///
const NUM_BUILTIN_IMPORTS: usize = 7;

pub fn aliases() -> MutMap<Symbol, BuiltinAlias> {
    use SolvedType::Flex;
    let mut aliases = HashMap::with_capacity_and_hasher(NUM_BUILTIN_IMPORTS, default_hasher());

    let mut add_alias = |symbol, alias| {
        debug_assert!(
            !aliases.contains_key(&symbol),
            "Duplicate alias definition for {:?}",
            symbol
        );

        // TODO instead of using Region::zero for all of these,
        // instead use the Region where they were defined in their
        // source .roc files! This can give nicer error messages.
        aliases.insert(symbol, alias);
    };

    // These can be shared between definitions, they will get instantiated when converted to Type
    let tvar1 = VarId::from_u32(1);
    let tvar2 = VarId::from_u32(2);
    let tvar3 = VarId::from_u32(3);
    let tvar4 = VarId::from_u32(4);

    let single_private_tag = |symbol, targs| {
        SolvedType::TagUnion(
            vec![(TagName::Private(symbol), targs)],
            Box::new(SolvedType::EmptyTagUnion),
        )
    };

    // Num range : [ @Num range ]
    add_alias(
        Symbol::NUM_NUM,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), ("range".into(), tvar1))],
            typ: single_private_tag(Symbol::NUM_AT_NUM, vec![Flex(tvar1)]),
        },
    );

    // Integer : [ @Integer ]
    add_alias(
        Symbol::INT_INTEGER,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::INT_AT_INTEGER, Vec::new()),
        },
    );

    // Int : Num Integer
    add_alias(
        Symbol::INT_INT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![SolvedType::Apply(Symbol::INT_INTEGER, Vec::new())],
            ),
        },
    );

    // FloatingPoint : [ @FloatingPoint ]
    add_alias(
        Symbol::FLOAT_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::FLOAT_AT_FLOATINGPOINT, Vec::new()),
        },
    );

    // Float : Num FloatingPoint
    add_alias(
        Symbol::FLOAT_FLOAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![SolvedType::Apply(Symbol::FLOAT_FLOATINGPOINT, Vec::new())],
            ),
        },
    );

    // Bool : [ True, False ]
    add_alias(
        Symbol::BOOL_BOOL,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::TagUnion(
                vec![
                    (TagName::Global("True".into()), Vec::new()),
                    (TagName::Global("False".into()), Vec::new()),
                ],
                Box::new(SolvedType::EmptyTagUnion),
            ),
        },
    );

    // Result a e : [ Ok a, Err e ]
    add_alias(
        Symbol::RESULT_RESULT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![
                Located::at(Region::zero(), ("a".into(), tvar1)),
                Located::at(Region::zero(), ("e".into(), tvar2)),
            ],
            typ: SolvedType::TagUnion(
                vec![
                    (TagName::Global("Ok".into()), vec![Flex(tvar1)]),
                    (TagName::Global("Err".into()), vec![Flex(tvar2)]),
                ],
                Box::new(SolvedType::EmptyTagUnion),
            ),
        },
    );

    // List elem : [ @List elem ]
    add_alias(
        Symbol::LIST_LIST,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), ("elem".into(), tvar1))],
            typ: single_private_tag(Symbol::LIST_AT_LIST, vec![Flex(tvar1)]),
        },
    );

    aliases
}

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

    // Int module

    // highest : Int
    add_type(Symbol::INT_HIGHEST, int_type());

    // lowest : Int
    add_type(Symbol::INT_LOWEST, int_type());

    // Float module

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
