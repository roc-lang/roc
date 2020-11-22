use crate::solved_types::{BuiltinAlias, SolvedType};
use crate::subs::VarId;
use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use std::collections::HashMap;

const NUM_BUILTIN_IMPORTS: usize = 8;

/// These can be shared between definitions, they will get instantiated when converted to Type
const TVAR1: VarId = VarId::from_u32(1);
const TVAR2: VarId = VarId::from_u32(2);

pub fn aliases() -> MutMap<Symbol, BuiltinAlias> {
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

    // Num range : [ @Num range ]
    add_alias(
        Symbol::NUM_NUM,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: num_alias_content(flex(TVAR1)),
        },
    );

    // Integer : [ @Integer ]
    add_alias(
        Symbol::NUM_INTEGER,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: integer_alias_content(),
        },
    );

    // Int : Num Integer
    add_alias(
        Symbol::NUM_INT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(),
        },
    );

    // FloatingPoint : [ @FloatingPoint ]
    add_alias(
        Symbol::NUM_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: floatingpoint_alias_content(),
        },
    );

    // Float : Num FloatingPoint
    add_alias(
        Symbol::NUM_FLOAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: float_alias_content(),
        },
    );

    // Bool : [ True, False ]
    add_alias(
        Symbol::BOOL_BOOL,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: bool_alias_content(),
        },
    );

    // Result a e : [ Ok a, Err e ]
    add_alias(
        Symbol::RESULT_RESULT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![
                Located::at(Region::zero(), "ok".into()),
                Located::at(Region::zero(), "err".into()),
            ],
            typ: result_alias_content(flex(TVAR1), flex(TVAR2)),
        },
    );

    aliases
}

#[inline(always)]
pub fn flex(tvar: VarId) -> SolvedType {
    SolvedType::Flex(tvar)
}

#[inline(always)]
pub fn num_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_NUM,
        vec![("range".into(), range.clone())],
        Box::new(num_alias_content(range)),
    )
}

#[inline(always)]
fn num_alias_content(range: SolvedType) -> SolvedType {
    single_private_tag(Symbol::NUM_AT_NUM, vec![range])
}

// FLOATING POINT

#[inline(always)]
pub fn floatingpoint_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_FLOATINGPOINT,
        Vec::new(),
        Box::new(floatingpoint_alias_content()),
    )
}

#[inline(always)]
fn floatingpoint_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_FLOATINGPOINT, Vec::new())
}

// FLOAT

#[inline(always)]
pub fn float_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_FLOAT,
        Vec::new(),
        Box::new(float_alias_content()),
    )
}

#[inline(always)]
fn float_alias_content() -> SolvedType {
    num_type(floatingpoint_type())
}

// INT

#[inline(always)]
pub fn int_type() -> SolvedType {
    SolvedType::Alias(Symbol::NUM_INT, Vec::new(), Box::new(int_alias_content()))
}

#[inline(always)]
fn int_alias_content() -> SolvedType {
    num_type(integer_type())
}

// INTEGER

#[inline(always)]
pub fn integer_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_INTEGER,
        Vec::new(),
        Box::new(integer_alias_content()),
    )
}

#[inline(always)]
fn integer_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_INTEGER, Vec::new())
}

#[inline(always)]
pub fn bool_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::BOOL_BOOL,
        Vec::new(),
        Box::new(bool_alias_content()),
    )
}

fn bool_alias_content() -> SolvedType {
    SolvedType::TagUnion(
        vec![
            (TagName::Global("False".into()), vec![]),
            (TagName::Global("True".into()), vec![]),
        ],
        Box::new(SolvedType::EmptyTagUnion),
    )
}

#[inline(always)]
pub fn ordering_type() -> SolvedType {
    // [ LT, EQ, GT ]
    SolvedType::TagUnion(
        vec![
            (TagName::Global("GT".into()), vec![]),
            (TagName::Global("EQ".into()), vec![]),
            (TagName::Global("LT".into()), vec![]),
        ],
        Box::new(SolvedType::EmptyTagUnion),
    )
}

#[inline(always)]
pub fn result_type(a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::RESULT_RESULT,
        vec![("ok".into(), a.clone()), ("err".into(), e.clone())],
        Box::new(result_alias_content(a, e)),
    )
}

#[inline(always)]
fn result_alias_content(a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::TagUnion(
        vec![
            (TagName::Global("Ok".into()), vec![a]),
            (TagName::Global("Err".into()), vec![e]),
        ],
        Box::new(SolvedType::EmptyTagUnion),
    )
}

#[inline(always)]
pub fn list_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::LIST_LIST, vec![a])
}

#[inline(always)]
pub fn str_type() -> SolvedType {
    SolvedType::Apply(Symbol::STR_STR, Vec::new())
}

#[inline(always)]
pub fn set_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::SET_SET, vec![a])
}

#[inline(always)]
pub fn map_type(key: SolvedType, value: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::MAP_MAP, vec![key, value])
}

fn single_private_tag(symbol: Symbol, type_arguments: Vec<SolvedType>) -> SolvedType {
    SolvedType::TagUnion(
        vec![(TagName::Private(symbol), type_arguments)],
        Box::new(SolvedType::EmptyTagUnion),
    )
}
