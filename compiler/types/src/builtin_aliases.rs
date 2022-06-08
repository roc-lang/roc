use crate::solved_types::{BuiltinAlias, SolvedType};
use crate::subs::VarId;
use crate::types::{AliasKind, RecordField};
use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use std::collections::HashMap;

const NUM_BUILTIN_IMPORTS: usize = 8;

/// These can be shared between definitions, they will get instantiated when converted to Type
const TVAR1: VarId = VarId::from_u32(1);

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

    // Int range : Num (Integer range)
    add_alias(
        Symbol::NUM_INT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Loc::at(Region::zero(), "range".into())],
            typ: int_alias_content(flex(TVAR1)),
            kind: AliasKind::Structural,
        },
    );

    // Frac range : Num (FloatingPoint range)
    add_alias(
        Symbol::NUM_FRAC,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Loc::at(Region::zero(), "range".into())],
            typ: frac_alias_content(flex(TVAR1)),
            kind: AliasKind::Structural,
        },
    );

    // Num range := range
    add_alias(
        Symbol::NUM_NUM,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Loc::at(Region::zero(), "range".into())],
            typ: num_alias_content(flex(TVAR1)),
            kind: AliasKind::Opaque,
        },
    );

    // Integer range := range
    add_alias(
        Symbol::NUM_INTEGER,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Loc::at(Region::zero(), "range".into())],
            typ: integer_alias_content(flex(TVAR1)),
            kind: AliasKind::Opaque,
        },
    );

    // Natural := []
    add_alias(
        Symbol::NUM_NATURAL,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: natural_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // Nat : Int Natural
    add_alias(
        Symbol::NUM_NAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: nat_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Signed128 := []
    add_alias(
        Symbol::NUM_SIGNED128,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed128_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // I128 : Int Signed128
    add_alias(
        Symbol::NUM_I128,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: i128_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // U128 : Int Unsigned128
    add_alias(
        Symbol::NUM_U128,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: u128_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Signed64 := []
    add_alias(
        Symbol::NUM_SIGNED64,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed64_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // I64 : Int Signed64
    add_alias(
        Symbol::NUM_I64,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: i64_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // U64 : Int Unsigned64
    add_alias(
        Symbol::NUM_U64,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: u64_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Signed32 := []
    add_alias(
        Symbol::NUM_SIGNED32,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed32_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // I32 : Int Signed32
    add_alias(
        Symbol::NUM_I32,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: i32_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // U32 : Int Unsigned32
    add_alias(
        Symbol::NUM_U32,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: u32_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Signed16 := []
    add_alias(
        Symbol::NUM_SIGNED16,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed16_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // I16 : Int Signed16
    add_alias(
        Symbol::NUM_I16,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: i16_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // U16 : Int Unsigned16
    add_alias(
        Symbol::NUM_U16,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: u16_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Signed8 := []
    add_alias(
        Symbol::NUM_SIGNED8,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: signed8_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // I8 : Int Signed8
    add_alias(
        Symbol::NUM_I8,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: i8_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // U8 : Int Unsigned8
    add_alias(
        Symbol::NUM_U8,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: u8_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Decimal := []
    add_alias(
        Symbol::NUM_DECIMAL,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: decimal_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // Binary64 := []
    add_alias(
        Symbol::NUM_BINARY64,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: binary64_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // Binary32 := []
    add_alias(
        Symbol::NUM_BINARY32,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: binary32_alias_content(),
            kind: AliasKind::Opaque,
        },
    );

    // FloatingPoint range := range
    add_alias(
        Symbol::NUM_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Loc::at(Region::zero(), "range".into())],
            typ: floatingpoint_alias_content(flex(TVAR1)),
            kind: AliasKind::Opaque,
        },
    );

    // Dec : Frac Decimal
    add_alias(
        Symbol::NUM_DEC,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: dec_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // F64 : Frac Binary64
    add_alias(
        Symbol::NUM_F64,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: f64_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // F32 : Frac Binary32
    add_alias(
        Symbol::NUM_F32,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: f32_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Bool : [True, False]
    add_alias(
        Symbol::BOOL_BOOL,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: bool_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Utf8ByteProblem : [InvalidStartByte, UnexpectedEndOfSequence, ExpectedContinuation, OverlongEncoding, CodepointTooLarge, EncodesSurrogateHalf]
    add_alias(
        Symbol::STR_UT8_BYTE_PROBLEM,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: str_utf8_byte_problem_alias_content(),
            kind: AliasKind::Structural,
        },
    );

    // Utf8Problem : { byteIndex : Nat, problem : Utf8ByteProblem }
    add_alias(
        Symbol::STR_UT8_PROBLEM,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: str_utf8_byte_problem_alias_content(),
            kind: AliasKind::Structural,
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
        vec![(range.clone())],
        vec![],
        Box::new(num_alias_content(range)),
        AliasKind::Opaque,
    )
}

#[inline(always)]
fn num_alias_content(range: SolvedType) -> SolvedType {
    range
}

// FLOATING POINT

#[inline(always)]
pub fn floatingpoint_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_FLOATINGPOINT,
        vec![(range.clone())],
        vec![],
        Box::new(floatingpoint_alias_content(range)),
        AliasKind::Opaque,
    )
}

#[inline(always)]
fn floatingpoint_alias_content(range: SolvedType) -> SolvedType {
    range
}

// FRAC

#[inline(always)]
pub fn frac_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_FRAC,
        vec![(range.clone())],
        vec![],
        Box::new(frac_alias_content(range)),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn frac_alias_content(range: SolvedType) -> SolvedType {
    num_type(floatingpoint_type(range))
}

// F64

#[inline(always)]
pub fn f64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_F64,
        vec![],
        vec![],
        Box::new(f64_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn f64_alias_content() -> SolvedType {
    frac_alias_content(binary64_type())
}

// F32

#[inline(always)]
pub fn f32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_F32,
        vec![],
        vec![],
        Box::new(f32_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn f32_alias_content() -> SolvedType {
    frac_alias_content(binary32_type())
}

// Nat

#[inline(always)]
pub fn nat_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_NAT,
        vec![],
        vec![],
        Box::new(nat_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn nat_alias_content() -> SolvedType {
    int_alias_content(natural_type())
}

// I128

#[inline(always)]
pub fn i128_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_I128,
        vec![],
        vec![],
        Box::new(i128_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn i128_alias_content() -> SolvedType {
    int_alias_content(signed128_type())
}

// I128

#[inline(always)]
pub fn u128_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_U128,
        vec![],
        vec![],
        Box::new(u128_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn u128_alias_content() -> SolvedType {
    int_alias_content(unsigned128_type())
}

// U64

#[inline(always)]
pub fn u64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_U64,
        vec![],
        vec![],
        Box::new(u64_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn u64_alias_content() -> SolvedType {
    int_alias_content(unsigned64_type())
}

// I64

#[inline(always)]
pub fn i64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_I64,
        vec![],
        vec![],
        Box::new(i64_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn i64_alias_content() -> SolvedType {
    int_alias_content(signed64_type())
}

// U32

#[inline(always)]
pub fn u32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_U32,
        vec![],
        vec![],
        Box::new(u32_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn u32_alias_content() -> SolvedType {
    int_alias_content(unsigned32_type())
}

// I32

#[inline(always)]
pub fn i32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_I32,
        vec![],
        vec![],
        Box::new(i32_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn i32_alias_content() -> SolvedType {
    int_alias_content(signed32_type())
}

// U16

#[inline(always)]
pub fn u16_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_U16,
        vec![],
        vec![],
        Box::new(u16_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn u16_alias_content() -> SolvedType {
    int_alias_content(unsigned16_type())
}

// I16

#[inline(always)]
pub fn i16_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_I16,
        vec![],
        vec![],
        Box::new(i16_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn i16_alias_content() -> SolvedType {
    int_alias_content(signed16_type())
}

// U8

#[inline(always)]
pub fn u8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_U8,
        vec![],
        vec![],
        Box::new(u8_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn u8_alias_content() -> SolvedType {
    int_alias_content(unsigned8_type())
}

// I8

#[inline(always)]
pub fn i8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_I8,
        vec![],
        vec![],
        Box::new(i8_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn i8_alias_content() -> SolvedType {
    int_alias_content(signed8_type())
}

// INT

#[inline(always)]
pub fn int_type(range: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_INT,
        vec![(range.clone())],
        vec![],
        Box::new(int_alias_content(range)),
        AliasKind::Structural,
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
        vec![(range.clone())],
        vec![],
        Box::new(integer_alias_content(range)),
        AliasKind::Opaque,
    )
}

#[inline(always)]
fn integer_alias_content(range: SolvedType) -> SolvedType {
    range
}

#[inline(always)]
pub fn binary64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_BINARY64,
        vec![],
        vec![],
        Box::new(binary64_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
pub fn binary64_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn binary32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_BINARY32,
        vec![],
        vec![],
        Box::new(binary32_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn binary32_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn natural_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_NATURAL,
        vec![],
        vec![],
        Box::new(natural_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn natural_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn signed128_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED128,
        vec![],
        vec![],
        Box::new(signed128_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn signed128_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn signed64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED64,
        vec![],
        vec![],
        Box::new(signed64_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn signed64_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn signed32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED32,
        vec![],
        vec![],
        Box::new(signed32_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn signed32_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn signed16_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED16,
        vec![],
        vec![],
        Box::new(signed16_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn signed16_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn signed8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_SIGNED8,
        vec![],
        vec![],
        Box::new(signed8_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn signed8_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn unsigned128_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED128,
        vec![],
        vec![],
        Box::new(unsigned128_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn unsigned128_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn unsigned64_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED64,
        vec![],
        vec![],
        Box::new(unsigned64_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn unsigned64_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn unsigned32_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED32,
        vec![],
        vec![],
        Box::new(unsigned32_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn unsigned32_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn unsigned16_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED16,
        vec![],
        vec![],
        Box::new(unsigned16_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn unsigned16_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
pub fn unsigned8_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_UNSIGNED8,
        vec![],
        vec![],
        Box::new(unsigned8_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn unsigned8_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

#[inline(always)]
fn decimal_alias_content() -> SolvedType {
    SolvedType::EmptyTagUnion
}

// Dec

#[inline(always)]
pub fn dec_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_DEC,
        vec![],
        vec![],
        Box::new(dec_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
fn dec_alias_content() -> SolvedType {
    frac_alias_content(decimal_type())
}

#[inline(always)]
pub fn decimal_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::NUM_DECIMAL,
        vec![],
        vec![],
        Box::new(decimal_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
pub fn bool_type() -> SolvedType {
    SolvedType::Alias(
        Symbol::BOOL_BOOL,
        vec![],
        vec![],
        Box::new(bool_alias_content()),
        AliasKind::Structural,
    )
}

fn bool_alias_content() -> SolvedType {
    SolvedType::TagUnion(
        vec![
            (TagName("False".into()), vec![]),
            (TagName("True".into()), vec![]),
        ],
        Box::new(SolvedType::EmptyTagUnion),
    )
}

#[inline(always)]
pub fn ordering_type() -> SolvedType {
    // [LT, EQ, GT]
    SolvedType::TagUnion(
        vec![
            (TagName("EQ".into()), vec![]),
            (TagName("GT".into()), vec![]),
            (TagName("LT".into()), vec![]),
        ],
        Box::new(SolvedType::EmptyTagUnion),
    )
}

#[inline(always)]
pub fn result_type(a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::Alias(
        Symbol::RESULT_RESULT,
        vec![a.clone(), e.clone()],
        vec![],
        Box::new(result_alias_content(a, e)),
        AliasKind::Structural,
    )
}

#[inline(always)]
pub fn box_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::BOX_BOX_TYPE, vec![a])
}

#[inline(always)]
fn result_alias_content(a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::TagUnion(
        vec![
            (TagName("Err".into()), vec![e]),
            (TagName("Ok".into()), vec![a]),
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
        vec![],
        vec![],
        Box::new(str_utf8_problem_alias_content()),
        AliasKind::Structural,
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
        vec![],
        vec![],
        Box::new(str_utf8_byte_problem_alias_content()),
        AliasKind::Structural,
    )
}

#[inline(always)]
pub fn str_utf8_byte_problem_alias_content() -> SolvedType {
    // 1. This must have the same values as the Zig struct Utf8ByteProblem in src/str.zig
    // 2. This must be in alphabetical order
    //
    // [CodepointTooLarge, EncodesSurrogateHalf, OverlongEncoding, InvalidStartByte, UnexpectedEndOfSequence, ExpectedContinuation]
    SolvedType::TagUnion(
        vec![
            (TagName("CodepointTooLarge".into()), vec![]),
            (TagName("EncodesSurrogateHalf".into()), vec![]),
            (TagName("ExpectedContinuation".into()), vec![]),
            (TagName("InvalidStartByte".into()), vec![]),
            (TagName("OverlongEncoding".into()), vec![]),
            (TagName("UnexpectedEndOfSequence".into()), vec![]),
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
