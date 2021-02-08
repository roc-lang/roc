use crate::solved_types::{BuiltinAlias, SolvedType};
use crate::subs::VarId;
use crate::types::RecordField;
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

    // Int range : [ @Int range ]
    add_alias(
        Symbol::NUM_INT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: int_alias_content(flex(TVAR1)),
        },
    );

    // Float range : [ @Float range ]
    add_alias(
        Symbol::NUM_FLOAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: float_alias_content(flex(TVAR1)),
        },
    );

    // Num range : [ @Num range ]
    add_alias(
        Symbol::NUM_NUM,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: num_alias_content(flex(TVAR1)),
        },
    );

    // Integer range : [ @Integer range ]
    add_alias(
        Symbol::NUM_INTEGER,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: integer_alias_content(flex(TVAR1)),
        },
    );

    // Natural : [ @Natural ]
    add_alias(
        Symbol::NUM_NATURAL,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: natural_alias_content(),
        },
    );

    // Nat : Int Natural
    add_alias(
        Symbol::NUM_NAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(natural_type()),
        },
    );

    // Signed128 : [ @Signed128 ]
    add_alias(
        Symbol::NUM_SIGNED128,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed128_alias_content(),
        },
    );

    // I128 : Int Signed128
    add_alias(
        Symbol::NUM_I128,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(signed128_type()),
        },
    );

    // U128 : Int Unsigned128
    add_alias(
        Symbol::NUM_U128,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(unsigned128_type()),
        },
    );

    // Signed64 : [ @Signed64 ]
    add_alias(
        Symbol::NUM_SIGNED64,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed64_alias_content(),
        },
    );

    // I64 : Int Signed64
    add_alias(
        Symbol::NUM_I64,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(signed64_type()),
        },
    );

    // U64 : Int Unsigned64
    add_alias(
        Symbol::NUM_U64,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(unsigned64_type()),
        },
    );

    // Signed32 : [ @Signed32 ]
    add_alias(
        Symbol::NUM_SIGNED32,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed32_alias_content(),
        },
    );

    // I32 : Int Signed32
    add_alias(
        Symbol::NUM_I32,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(signed32_type()),
        },
    );

    // U32 : Int Unsigned32
    add_alias(
        Symbol::NUM_U32,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(unsigned32_type()),
        },
    );

    // Signed16 : [ @Signed16 ]
    add_alias(
        Symbol::NUM_SIGNED16,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed16_alias_content(),
        },
    );

    // I16 : Int Signed16
    add_alias(
        Symbol::NUM_I16,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(signed16_type()),
        },
    );

    // U16 : Int Unsigned16
    add_alias(
        Symbol::NUM_U16,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(unsigned16_type()),
        },
    );

    // Signed8 : [ @Signed8 ]
    add_alias(
        Symbol::NUM_SIGNED8,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed8_alias_content(),
        },
    );

    // I8 : Int Signed8
    add_alias(
        Symbol::NUM_I8,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(signed8_type()),
        },
    );

    // U8 : Int Unsigned8
    add_alias(
        Symbol::NUM_U8,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: int_alias_content(unsigned8_type()),
        },
    );

    // Binary64 : [ @Binary64 ]
    add_alias(
        Symbol::NUM_BINARY64,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: binary64_alias_content(),
        },
    );

    // Binary32 : [ @Binary32 ]
    add_alias(
        Symbol::NUM_BINARY32,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: binary32_alias_content(),
        },
    );

    // FloatingPoint range : [ @FloatingPoint range ]
    add_alias(
        Symbol::NUM_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: floatingpoint_alias_content(flex(TVAR1)),
        },
    );

    // F64 : Float Binary64
    add_alias(
        Symbol::NUM_F64,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: float_alias_content(binary64_type()),
        },
    );

    // F32 : Float Binary32
    add_alias(
        Symbol::NUM_F32,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: float_alias_content(binary32_type()),
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

    // Result ok err : [ Ok ok, Err err ]
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

    // Utf8ByteProblem : [ InvalidStartByte, UnexpectedEndOfSequence, ExpectedContinuation, OverlongEncoding, CodepointTooLarge, EncodesSurrogateHalf ]
    add_alias(
        Symbol::STR_UT8_BYTE_PROBLEM,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: str_utf8_byte_problem_alias_content(),
        },
    );

    // Utf8Problem : { byteIndex : Nat, problem : Utf8ByteProblem }
    add_alias(
        Symbol::STR_UT8_PROBLEM,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: str_utf8_byte_problem_alias_content(),
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
pub fn floatingpoint_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_FLOATINGPOINT,
        vec![("range".into(), range.clone())],
        Box::new(floatingpoint_alias_content(range)),
    )
}

#[inline(always)]
fn floatingpoint_alias_content(range: SolvedType) -> SolvedType {
    single_private_tag(Symbol::NUM_AT_FLOATINGPOINT, vec![range])
}

// FLOAT

#[inline(always)]
pub fn float_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_FLOAT,
        vec![("range".into(), range.clone())],
        Box::new(float_alias_content(range)),
    )
}

#[inline(always)]
fn float_alias_content(range: SolvedType) -> SolvedType {
    num_type(floatingpoint_type(range))
}

// Nat

#[inline(always)]
pub fn nat_type() -> SolvedType {
    SolvedType::Alias(Symbol::NUM_NAT, vec![], Box::new(nat_alias_content()))
}

#[inline(always)]
fn nat_alias_content() -> SolvedType {
    int_alias_content(natural_type())
}

// INT

#[inline(always)]
pub fn int_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_INT,
        vec![("range".into(), range.clone())],
        Box::new(int_alias_content(range)),
    )
}

#[inline(always)]
fn int_alias_content(range: SolvedType) -> SolvedType {
    num_type(integer_type(range))
}

// INTEGER

#[inline(always)]
pub fn integer_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_INTEGER,
        vec![("range".into(), range.clone())],
        Box::new(integer_alias_content(range)),
    )
}

#[inline(always)]
fn integer_alias_content(range: SolvedType) -> SolvedType {
    single_private_tag(Symbol::NUM_AT_INTEGER, vec![range])
}

#[inline(always)]
pub fn u8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_U8,
        vec![],
        Box::new(int_alias_content(unsigned8_type())),
    )
}

#[inline(always)]
pub fn binary64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_BINARY64,
        vec![],
        Box::new(binary64_alias_content()),
    )
}

#[inline(always)]
pub fn binary64_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_BINARY64, vec![])
}

#[inline(always)]
pub fn binary32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_BINARY32,
        vec![],
        Box::new(binary32_alias_content()),
    )
}

#[inline(always)]
fn binary32_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_BINARY32, vec![])
}

#[inline(always)]
pub fn natural_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_NATURAL,
        vec![],
        Box::new(natural_alias_content()),
    )
}

#[inline(always)]
fn natural_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_NATURAL, vec![])
}

#[inline(always)]
pub fn signed128_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED128,
        vec![],
        Box::new(signed128_alias_content()),
    )
}

#[inline(always)]
fn signed128_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_SIGNED128, vec![])
}

#[inline(always)]
pub fn signed64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED64,
        vec![],
        Box::new(signed64_alias_content()),
    )
}

#[inline(always)]
fn signed64_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_SIGNED64, vec![])
}

#[inline(always)]
pub fn signed32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED32,
        vec![],
        Box::new(signed32_alias_content()),
    )
}

#[inline(always)]
fn signed32_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_SIGNED32, vec![])
}

#[inline(always)]
pub fn signed16_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED16,
        vec![],
        Box::new(signed16_alias_content()),
    )
}

#[inline(always)]
fn signed16_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_SIGNED16, vec![])
}

#[inline(always)]
pub fn signed8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED8,
        vec![],
        Box::new(signed8_alias_content()),
    )
}

#[inline(always)]
fn signed8_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_SIGNED8, vec![])
}

#[inline(always)]
pub fn unsigned128_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED128,
        vec![],
        Box::new(unsigned128_alias_content()),
    )
}

#[inline(always)]
fn unsigned128_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_UNSIGNED128, vec![])
}

#[inline(always)]
pub fn unsigned64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED64,
        vec![],
        Box::new(unsigned64_alias_content()),
    )
}

#[inline(always)]
fn unsigned64_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_UNSIGNED64, vec![])
}

#[inline(always)]
pub fn unsigned32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED32,
        vec![],
        Box::new(unsigned32_alias_content()),
    )
}

#[inline(always)]
fn unsigned32_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_UNSIGNED32, vec![])
}

#[inline(always)]
pub fn unsigned16_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED16,
        vec![],
        Box::new(unsigned16_alias_content()),
    )
}

#[inline(always)]
fn unsigned16_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_UNSIGNED16, vec![])
}

#[inline(always)]
pub fn unsigned8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED8,
        vec![],
        Box::new(unsigned8_alias_content()),
    )
}

#[inline(always)]
fn unsigned8_alias_content() -> SolvedType {
    single_private_tag(Symbol::NUM_AT_UNSIGNED8, vec![])
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
pub fn str_utf8_problem_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::STR_UT8_PROBLEM,
        Vec::new(),
        Box::new(str_utf8_problem_alias_content()),
    )
}

#[inline(always)]
pub fn str_utf8_problem_alias_content() -> SolvedType {
    SolvedType::Record {
        fields: vec![
            ("byteIndex".into(), RecordField::Required(nat_type())),
            (
                "problem".into(),
                RecordField::Required(str_utf8_byte_problem_type()),
            ),
        ],
        ext: Box::new(SolvedType::EmptyRecord),
    }
}

#[inline(always)]
pub fn str_utf8_byte_problem_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::STR_UT8_BYTE_PROBLEM,
        Vec::new(),
        Box::new(str_utf8_byte_problem_alias_content()),
    )
}

#[inline(always)]
pub fn str_utf8_byte_problem_alias_content() -> SolvedType {
    // 1. This must have the same values as the Zig struct Utf8ByteProblem in src/str.zig
    // 2. This must be in alphabetical order
    //
    // [ CodepointTooLarge, EncodesSurrogateHalf, OverlongEncoding, InvalidStartByte, UnexpectedEndOfSequence, ExpectedContinuation ]
    SolvedType::TagUnion(
        vec![
            (TagName::Global("CodepointTooLarge".into()), vec![]),
            (TagName::Global("EncodesSurrogateHalf".into()), vec![]),
            (TagName::Global("ExpectedContinuation".into()), vec![]),
            (TagName::Global("InvalidStartByte".into()), vec![]),
            (TagName::Global("OverlongEncoding".into()), vec![]),
            (TagName::Global("UnexpectedEndOfSequence".into()), vec![]),
        ],
        Box::new(SolvedType::EmptyTagUnion),
    )
}

#[inline(always)]
pub fn set_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::SET_SET, vec![a])
}

#[inline(always)]
pub fn dict_type(key: SolvedType, value: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::DICT_DICT, vec![key, value])
}

pub fn single_private_tag(symbol: Symbol, type_arguments: Vec<SolvedType>) -> SolvedType {
    SolvedType::TagUnion(
        vec![(TagName::Private(symbol), type_arguments)],
        Box::new(SolvedType::EmptyTagUnion),
    )
}
