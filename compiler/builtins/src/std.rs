use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::solved_types::{BuiltinAlias, SolvedType};
use roc_types::subs::VarId;
use std::collections::HashMap;

#[derive(Clone, Copy)]
pub enum Mode {
    Standard,
    Uniqueness,
}

pub struct StdLib {
    pub mode: Mode,
    pub types: MutMap<Symbol, (SolvedType, Region)>,
    pub aliases: MutMap<Symbol, BuiltinAlias>,
}

pub fn standard_stdlib() -> StdLib {
    StdLib {
        mode: Mode::Standard,
        types: types(),
        aliases: aliases(),
    }
}

/// Keep this up to date by hand!
///
const NUM_BUILTIN_IMPORTS: usize = 7;

/// These can be shared between definitions, they will get instantiated when converted to Type
const TVAR1: VarId = VarId::from_u32(1);
const TVAR2: VarId = VarId::from_u32(2);
const TVAR3: VarId = VarId::from_u32(3);

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
            vars: vec![Located::at(Region::zero(), "range".into())],
            typ: single_private_tag(Symbol::NUM_AT_NUM, vec![flex(TVAR1)]),
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
                Located::at(Region::zero(), "a".into()),
                Located::at(Region::zero(), "e".into()),
            ],
            typ: SolvedType::TagUnion(
                vec![
                    (TagName::Global("Ok".into()), vec![flex(TVAR1)]),
                    (TagName::Global("Err".into()), vec![flex(TVAR2)]),
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
            vars: vec![Located::at(Region::zero(), "elem".into())],
            typ: single_private_tag(Symbol::LIST_AT_LIST, vec![flex(TVAR1)]),
        },
    );

    // Str : [ @Str ]
    add_alias(
        Symbol::STR_STR,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![],
            typ: single_private_tag(Symbol::STR_AT_STR, vec![]),
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

    // add or (+) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_ADD,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(num_type(flex(TVAR1))),
        ),
    );

    // sub or (-) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_SUB,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(num_type(flex(TVAR1))),
        ),
    );

    // mul or (*) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_MUL,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(num_type(flex(TVAR1))),
        ),
    );

    // abs : Num a -> Num a
    add_type(
        Symbol::NUM_ABS,
        SolvedType::Func(vec![num_type(flex(TVAR1))], Box::new(num_type(flex(TVAR1)))),
    );

    // neg : Num a -> Num a
    add_type(
        Symbol::NUM_NEG,
        SolvedType::Func(vec![num_type(flex(TVAR1))], Box::new(num_type(flex(TVAR1)))),
    );

    // isLt or (<) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LT,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // isLte or (<=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LE,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // isGt or (>) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_GT,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // isGte or (>=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_GE,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // Int module

    // highest : Int
    add_type(Symbol::INT_HIGHEST, int_type());

    // lowest : Int
    add_type(Symbol::INT_LOWEST, int_type());

    let div_by_zero = SolvedType::TagUnion(
        vec![(TagName::Global("DivByZero".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    // div : Int, Int -> Result Int [ DivByZero ]*
    add_type(
        Symbol::INT_DIV,
        SolvedType::Func(
            vec![int_type(), int_type()],
            Box::new(result_type(flex(TVAR1), div_by_zero.clone())),
        ),
    );

    // mod : Int, Int -> Result Int [ DivByZero ]*
    add_type(
        Symbol::INT_MOD,
        SolvedType::Func(
            vec![int_type(), int_type()],
            Box::new(result_type(flex(TVAR1), div_by_zero)),
        ),
    );

    // Float module

    // div : Float, Float -> Float
    add_type(
        Symbol::FLOAT_DIV,
        SolvedType::Func(vec![float_type(), float_type()], Box::new(float_type())),
    );

    // mod : Float, Float -> Float
    add_type(
        Symbol::FLOAT_MOD,
        SolvedType::Func(vec![float_type(), float_type()], Box::new(float_type())),
    );

    // sqrt : Float -> Float
    add_type(
        Symbol::FLOAT_SQRT,
        SolvedType::Func(vec![float_type()], Box::new(float_type())),
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

    // xor : Bool, Bool -> Bool
    add_type(
        Symbol::BOOL_XOR,
        SolvedType::Func(vec![bool_type(), bool_type()], Box::new(bool_type())),
    );

    // not : Bool -> Bool
    add_type(
        Symbol::BOOL_NOT,
        SolvedType::Func(vec![bool_type()], Box::new(bool_type())),
    );

    // Str module

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

    // get : List elem, Int -> Result elem [ IndexOutOfBounds ]*
    let index_out_of_bounds = SolvedType::TagUnion(
        vec![(TagName::Global("IndexOutOfBounds".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::LIST_GET,
        SolvedType::Func(
            vec![list_type(flex(TVAR1)), int_type()],
            Box::new(result_type(flex(TVAR1), index_out_of_bounds)),
        ),
    );

    add_type(
        Symbol::LIST_GET_UNSAFE, // TODO remove this once we can code gen Result
        SolvedType::Func(
            vec![list_type(flex(TVAR1)), int_type()],
            Box::new(flex(TVAR1)),
        ),
    );

    // set : List elem, Int, elem -> List elem
    add_type(
        Symbol::LIST_SET,
        SolvedType::Func(
            vec![list_type(flex(TVAR1)), int_type(), flex(TVAR1)],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // map : List before, (before -> after) -> List after
    add_type(
        Symbol::LIST_MAP,
        SolvedType::Func(
            vec![
                list_type(flex(TVAR1)),
                SolvedType::Func(vec![flex(TVAR1)], Box::new(flex(TVAR2))),
            ],
            Box::new(list_type(flex(TVAR2))),
        ),
    );

    // foldr : List a, (a -> b -> b), b -> b
    add_type(
        Symbol::LIST_FOLDR,
        SolvedType::Func(
            vec![
                list_type(flex(TVAR1)),
                SolvedType::Func(vec![flex(TVAR1), flex(TVAR2)], Box::new(flex(TVAR2))),
                flex(TVAR2),
            ],
            Box::new(flex(TVAR2)),
        ),
    );

    // push : List a -> a -> List a
    add_type(
        Symbol::LIST_PUSH,
        SolvedType::Func(
            vec![list_type(flex(TVAR1))],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // length : List a -> Int
    add_type(
        Symbol::LIST_LENGTH,
        SolvedType::Func(vec![list_type(flex(TVAR1))], Box::new(int_type())),
    );

    // Result module

    // map : Result a err, (a -> b) -> Result b err
    add_type(
        Symbol::RESULT_MAP,
        SolvedType::Func(
            vec![
                result_type(flex(TVAR1), flex(TVAR3)),
                SolvedType::Func(vec![flex(TVAR1)], Box::new(flex(TVAR2))),
            ],
            Box::new(result_type(flex(TVAR2), flex(TVAR3))),
        ),
    );

    types
}

#[inline(always)]
fn flex(tvar: VarId) -> SolvedType {
    SolvedType::Flex(tvar)
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

#[inline(always)]
fn num_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::NUM_NUM, vec![a])
}

#[inline(always)]
fn result_type(a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::RESULT_RESULT, vec![a, e])
}

#[inline(always)]
fn list_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::LIST_LIST, vec![a])
}
