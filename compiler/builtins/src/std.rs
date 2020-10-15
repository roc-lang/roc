use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::solved_types::{BuiltinAlias, SolvedType};
use roc_types::subs::VarId;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Standard,
    Uniqueness,
}

#[derive(Debug, Clone)]
pub struct StdLib {
    pub mode: Mode,
    pub types: MutMap<Symbol, (SolvedType, Region)>,
    pub aliases: MutMap<Symbol, BuiltinAlias>,
    pub applies: MutSet<Symbol>,
}

pub fn standard_stdlib() -> StdLib {
    StdLib {
        mode: Mode::Standard,
        types: types(),
        aliases: aliases(),
        applies: vec![
            Symbol::LIST_LIST,
            Symbol::SET_SET,
            Symbol::MAP_MAP,
            Symbol::STR_STR,
        ]
        .into_iter()
        .collect(),
    }
}

/// Keep this up to date by hand! It's the number of builtin aliases that are imported by default.
const NUM_BUILTIN_IMPORTS: usize = 7;

/// These can be shared between definitions, they will get instantiated when converted to Type
const TVAR1: VarId = VarId::from_u32(1);
const TVAR2: VarId = VarId::from_u32(2);
const TVAR3: VarId = VarId::from_u32(3);
const TVAR4: VarId = VarId::from_u32(4);
const TOP_LEVEL_CLOSURE_VAR: VarId = VarId::from_u32(5);

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
        Symbol::NUM_INTEGER,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::NUM_AT_INTEGER, Vec::new()),
        },
    );

    // Int : Num Integer
    add_alias(
        Symbol::NUM_INT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![SolvedType::Apply(Symbol::NUM_INTEGER, Vec::new())],
            ),
        },
    );

    // FloatingPoint : [ @FloatingPoint ]
    add_alias(
        Symbol::NUM_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::NUM_AT_FLOATINGPOINT, Vec::new()),
        },
    );

    // Float : Num FloatingPoint
    add_alias(
        Symbol::NUM_FLOAT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: SolvedType::Apply(
                Symbol::NUM_NUM,
                vec![SolvedType::Apply(Symbol::NUM_FLOATINGPOINT, Vec::new())],
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
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(num_type(flex(TVAR1))),
        ),
    );

    // addChecked : Num a, Num a -> Result (Num a) [ IntOverflow ]*
    let overflow = SolvedType::TagUnion(
        vec![(TagName::Global("Overflow".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::NUM_ADD_CHECKED,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(result_type(num_type(flex(TVAR1)), overflow)),
        ),
    );

    // addWrap : Int, Int -> Int
    add_type(
        Symbol::NUM_ADD_WRAP,
        top_level_function(vec![int_type(), int_type()], Box::new(int_type())),
    );

    // sub or (-) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_SUB,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(num_type(flex(TVAR1))),
        ),
    );

    // mul or (*) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_MUL,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(num_type(flex(TVAR1))),
        ),
    );

    // abs : Num a -> Num a
    add_type(
        Symbol::NUM_ABS,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(num_type(flex(TVAR1)))),
    );

    // neg : Num a -> Num a
    add_type(
        Symbol::NUM_NEG,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(num_type(flex(TVAR1)))),
    );

    // isEq or (==) : a, a -> Bool
    add_type(
        Symbol::BOOL_EQ,
        top_level_function(vec![flex(TVAR1), flex(TVAR1)], Box::new(bool_type())),
    );

    // isNeq or (!=) : a, a -> Bool
    add_type(
        Symbol::BOOL_NEQ,
        top_level_function(vec![flex(TVAR1), flex(TVAR1)], Box::new(bool_type())),
    );

    // isLt or (<) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LT,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // isLte or (<=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LTE,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // isGt or (>) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_GT,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // isGte or (>=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_GTE,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // compare : Num a, Num a -> [ LT, EQ, GT ]
    add_type(
        Symbol::NUM_COMPARE,
        top_level_function(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(ordering_type()),
        ),
    );

    // toFloat : Num a -> Float
    add_type(
        Symbol::NUM_TO_FLOAT,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(float_type())),
    );

    // isNegative : Num a -> Bool
    add_type(
        Symbol::NUM_IS_NEGATIVE,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // isPositive : Num a -> Bool
    add_type(
        Symbol::NUM_IS_POSITIVE,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // isZero : Num a -> Bool
    add_type(
        Symbol::NUM_IS_ZERO,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // isEven : Num a -> Bool
    add_type(
        Symbol::NUM_IS_EVEN,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // isOdd : Num a -> Bool
    add_type(
        Symbol::NUM_IS_ODD,
        top_level_function(vec![num_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // maxInt : Int
    add_type(Symbol::NUM_MAX_INT, int_type());

    // minInt : Int
    add_type(Symbol::NUM_MIN_INT, int_type());

    // div : Int, Int -> Result Int [ DivByZero ]*
    let div_by_zero = SolvedType::TagUnion(
        vec![(TagName::Global("DivByZero".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::NUM_DIV_INT,
        top_level_function(
            vec![int_type(), int_type()],
            Box::new(result_type(int_type(), div_by_zero.clone())),
        ),
    );

    // rem : Int, Int -> Result Int [ DivByZero ]*
    add_type(
        Symbol::NUM_REM,
        top_level_function(
            vec![int_type(), int_type()],
            Box::new(result_type(int_type(), div_by_zero.clone())),
        ),
    );

    // mod : Int, Int -> Result Int [ DivByZero ]*
    add_type(
        Symbol::NUM_MOD_INT,
        top_level_function(
            vec![int_type(), int_type()],
            Box::new(result_type(int_type(), div_by_zero.clone())),
        ),
    );

    // Float module

    // div : Float, Float -> Float
    add_type(
        Symbol::NUM_DIV_FLOAT,
        top_level_function(
            vec![float_type(), float_type()],
            Box::new(result_type(float_type(), div_by_zero.clone())),
        ),
    );

    // mod : Float, Float -> Result Int [ DivByZero ]*
    add_type(
        Symbol::NUM_MOD_FLOAT,
        top_level_function(
            vec![float_type(), float_type()],
            Box::new(result_type(float_type(), div_by_zero)),
        ),
    );

    // sqrt : Float -> Float
    let sqrt_of_negative = SolvedType::TagUnion(
        vec![(TagName::Global("SqrtOfNegative".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::NUM_SQRT,
        top_level_function(
            vec![float_type()],
            Box::new(result_type(float_type(), sqrt_of_negative)),
        ),
    );

    // round : Float -> Int
    add_type(
        Symbol::NUM_ROUND,
        top_level_function(vec![float_type()], Box::new(int_type())),
    );

    // sin : Float -> Float
    add_type(
        Symbol::NUM_SIN,
        top_level_function(vec![float_type()], Box::new(float_type())),
    );

    // cos : Float -> Float
    add_type(
        Symbol::NUM_COS,
        top_level_function(vec![float_type()], Box::new(float_type())),
    );

    // tan : Float -> Float
    add_type(
        Symbol::NUM_TAN,
        top_level_function(vec![float_type()], Box::new(float_type())),
    );

    // maxFloat : Float
    add_type(Symbol::NUM_MAX_FLOAT, float_type());

    // minFloat : Float
    add_type(Symbol::NUM_MIN_FLOAT, float_type());

    // pow : Float, Float -> Float
    add_type(
        Symbol::NUM_POW,
        top_level_function(vec![float_type(), float_type()], Box::new(float_type())),
    );

    // ceiling : Float -> Int
    add_type(
        Symbol::NUM_CEILING,
        top_level_function(vec![float_type()], Box::new(int_type())),
    );

    // powInt : Int, Int -> Int
    add_type(
        Symbol::NUM_POW_INT,
        top_level_function(vec![int_type(), int_type()], Box::new(int_type())),
    );

    // floor : Float -> Int
    add_type(
        Symbol::NUM_FLOOR,
        top_level_function(vec![float_type()], Box::new(int_type())),
    );

    // atan : Float -> Float
    add_type(
        Symbol::NUM_ATAN,
        top_level_function(vec![float_type()], Box::new(float_type())),
    );

    // Bool module

    // and : Bool, Bool -> Bool
    add_type(
        Symbol::BOOL_AND,
        top_level_function(vec![bool_type(), bool_type()], Box::new(bool_type())),
    );

    // or : Bool, Bool -> Bool
    add_type(
        Symbol::BOOL_OR,
        top_level_function(vec![bool_type(), bool_type()], Box::new(bool_type())),
    );

    // xor : Bool, Bool -> Bool
    add_type(
        Symbol::BOOL_XOR,
        top_level_function(vec![bool_type(), bool_type()], Box::new(bool_type())),
    );

    // not : Bool -> Bool
    add_type(
        Symbol::BOOL_NOT,
        top_level_function(vec![bool_type()], Box::new(bool_type())),
    );

    // Str module

    // Str.split : Str, Str -> List Str
    add_type(
        Symbol::STR_SPLIT,
        SolvedType::Func(
            vec![str_type(), str_type()],
            Box::new(list_type(str_type())),
        ),
    );

    // Str.concat : Str, Str -> Str
    add_type(
        Symbol::STR_CONCAT,
        top_level_function(vec![str_type(), str_type()], Box::new(str_type())),
    );

    // isEmpty : Str -> Bool
    add_type(
        Symbol::STR_IS_EMPTY,
        top_level_function(vec![str_type()], Box::new(bool_type())),
    );

    // List module

    // get : List elem, Int -> Result elem [ OutOfBounds ]*
    let index_out_of_bounds = SolvedType::TagUnion(
        vec![(TagName::Global("OutOfBounds".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::LIST_GET,
        top_level_function(
            vec![list_type(flex(TVAR1)), int_type()],
            Box::new(result_type(flex(TVAR1), index_out_of_bounds)),
        ),
    );

    // first : List elem -> Result elem [ ListWasEmpty ]*
    let list_was_empty = SolvedType::TagUnion(
        vec![(TagName::Global("ListWasEmpty".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::LIST_FIRST,
        top_level_function(
            vec![list_type(flex(TVAR1))],
            Box::new(result_type(flex(TVAR1), list_was_empty)),
        ),
    );

    // set : List elem, Int, elem -> List elem
    add_type(
        Symbol::LIST_SET,
        top_level_function(
            vec![list_type(flex(TVAR1)), int_type(), flex(TVAR1)],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // concat : List elem, List elem -> List elem
    add_type(
        Symbol::LIST_CONCAT,
        top_level_function(
            vec![list_type(flex(TVAR1)), list_type(flex(TVAR1))],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // walkRight : List elem, (elem -> accum -> accum), accum -> accum
    add_type(
        Symbol::LIST_WALK_RIGHT,
        top_level_function(
            vec![
                list_type(flex(TVAR1)),
                closure(vec![flex(TVAR1), flex(TVAR2)], TVAR3, Box::new(flex(TVAR2))),
                flex(TVAR2),
            ],
            Box::new(flex(TVAR2)),
        ),
    );

    // keepIf : List elem, (elem -> Bool) -> List elem
    add_type(
        Symbol::LIST_KEEP_IF,
        top_level_function(
            vec![
                list_type(flex(TVAR1)),
                closure(vec![flex(TVAR1)], TVAR2, Box::new(bool_type())),
            ],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // map : List before, (before -> after) -> List after
    add_type(
        Symbol::LIST_MAP,
        top_level_function(
            vec![
                list_type(flex(TVAR1)),
                closure(vec![flex(TVAR1)], TVAR3, Box::new(flex(TVAR2))),
            ],
            Box::new(list_type(flex(TVAR2))),
        ),
    );

    // append : List elem, elem -> List elem
    add_type(
        Symbol::LIST_APPEND,
        top_level_function(
            vec![list_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // prepend : List elem, elem -> List elem
    add_type(
        Symbol::LIST_PREPEND,
        top_level_function(
            vec![list_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // join : List (List elem) -> List elem
    add_type(
        Symbol::LIST_JOIN,
        top_level_function(
            vec![list_type(list_type(flex(TVAR1)))],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // single : a -> List a
    add_type(
        Symbol::LIST_SINGLE,
        top_level_function(vec![flex(TVAR1)], Box::new(list_type(flex(TVAR1)))),
    );

    // repeat : Int, elem -> List elem
    add_type(
        Symbol::LIST_REPEAT,
        top_level_function(
            vec![int_type(), flex(TVAR1)],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // reverse : List elem -> List elem
    add_type(
        Symbol::LIST_REVERSE,
        top_level_function(
            vec![list_type(flex(TVAR1))],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // len : List * -> Int
    add_type(
        Symbol::LIST_LEN,
        top_level_function(vec![list_type(flex(TVAR1))], Box::new(int_type())),
    );

    // isEmpty : List * -> Bool
    add_type(
        Symbol::LIST_IS_EMPTY,
        top_level_function(vec![list_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // Map module

    // empty : Map k v
    add_type(Symbol::MAP_EMPTY, map_type(flex(TVAR1), flex(TVAR2)));

    // singleton : k, v -> Map k v
    add_type(
        Symbol::MAP_SINGLETON,
        top_level_function(
            vec![flex(TVAR1), flex(TVAR2)],
            Box::new(map_type(flex(TVAR1), flex(TVAR2))),
        ),
    );

    // get : Map k v, k -> Result v [ KeyNotFound ]*
    let key_not_found = SolvedType::TagUnion(
        vec![(TagName::Global("KeyNotFound".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::MAP_GET,
        top_level_function(
            vec![map_type(flex(TVAR1), flex(TVAR2)), flex(TVAR1)],
            Box::new(result_type(flex(TVAR2), key_not_found)),
        ),
    );

    add_type(
        Symbol::MAP_INSERT,
        top_level_function(
            vec![map_type(flex(TVAR1), flex(TVAR2)), flex(TVAR1), flex(TVAR2)],
            Box::new(map_type(flex(TVAR1), flex(TVAR2))),
        ),
    );

    // Set module

    // empty : Set a
    add_type(Symbol::SET_EMPTY, set_type(flex(TVAR1)));

    // singleton : a -> Set a
    add_type(
        Symbol::SET_SINGLETON,
        top_level_function(vec![flex(TVAR1)], Box::new(set_type(flex(TVAR1)))),
    );

    // union : Set a, Set a -> Set a
    add_type(
        Symbol::SET_UNION,
        top_level_function(
            vec![set_type(flex(TVAR1)), set_type(flex(TVAR1))],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    // diff : Set a, Set a -> Set a
    add_type(
        Symbol::SET_DIFF,
        top_level_function(
            vec![set_type(flex(TVAR1)), set_type(flex(TVAR1))],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    // foldl : Set a, (a -> b -> b), b -> b
    add_type(
        Symbol::SET_FOLDL,
        top_level_function(
            vec![
                set_type(flex(TVAR1)),
                closure(vec![flex(TVAR1), flex(TVAR2)], TVAR3, Box::new(flex(TVAR2))),
                flex(TVAR2),
            ],
            Box::new(flex(TVAR2)),
        ),
    );

    add_type(
        Symbol::SET_INSERT,
        top_level_function(
            vec![set_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    add_type(
        Symbol::SET_REMOVE,
        top_level_function(
            vec![set_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    // Result module

    // map : Result a err, (a -> b) -> Result b err
    add_type(
        Symbol::RESULT_MAP,
        top_level_function(
            vec![
                result_type(flex(TVAR1), flex(TVAR3)),
                closure(vec![flex(TVAR1)], TVAR4, Box::new(flex(TVAR2))),
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
fn top_level_function(arguments: Vec<SolvedType>, ret: Box<SolvedType>) -> SolvedType {
    SolvedType::Func(
        arguments,
        Box::new(SolvedType::Flex(TOP_LEVEL_CLOSURE_VAR)),
        ret,
    )
}

#[inline(always)]
fn closure(arguments: Vec<SolvedType>, closure_var: VarId, ret: Box<SolvedType>) -> SolvedType {
    SolvedType::Func(arguments, Box::new(SolvedType::Flex(closure_var)), ret)
}

#[inline(always)]
fn float_type() -> SolvedType {
    SolvedType::Apply(Symbol::NUM_FLOAT, Vec::new())
}

#[inline(always)]
fn int_type() -> SolvedType {
    SolvedType::Apply(Symbol::NUM_INT, Vec::new())
}

#[inline(always)]
fn bool_type() -> SolvedType {
    SolvedType::Apply(Symbol::BOOL_BOOL, Vec::new())
}

#[inline(always)]
fn ordering_type() -> SolvedType {
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

#[inline(always)]
fn set_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::SET_SET, vec![a])
}

#[inline(always)]
fn map_type(key: SolvedType, value: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::MAP_MAP, vec![key, value])
}
