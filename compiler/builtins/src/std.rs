use roc_can::def::Def;
use roc_can::expr::Expr;
use roc_can::expr::Recursive;
use roc_collections::all::SendMap;
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::ident::TagName;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::solved_types::{BuiltinAlias, SolvedType};
use roc_types::subs::VarId;
use roc_types::subs::Variable;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Standard,
    Uniqueness,
}

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
const TVAR_NONE: VarId = VarId::from_u32(0);
const TVAR1: VarId = VarId::from_u32(1);
const TVAR2: VarId = VarId::from_u32(2);
const TVAR3: VarId = VarId::from_u32(3);

/// Some builtins cannot be constructed in code gen alone, and need to be defined
/// as separate Roc defs. For example, List.get has this type:
///
/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
///
/// Because this returns an open tag union for its Err type, it's not possible
/// for code gen to return a hardcoded value for OutOfBounds. For example,
/// if this Result unifies to [ Foo, OutOfBounds ] then OutOfBOunds will
/// get assigned the number 1 (because Foo got 0 alphabetically), whereas
/// if it unifies to [ OutOfBounds, Qux ] then OutOfBounds will get the number 0.
///
/// Getting these numbers right requires having List.get participate in the
/// normal type-checking and monomorphization processes. As such, this function
/// returns a normal def for List.get, which performs a bounds check and then
/// delegates to the compiler-internal List.getUnsafe function to do the actual
/// lookup (if the bounds check passed). That internal function is hardcoded in code gen,
/// which works fine because it doesn't involve any open tag unions.
pub fn builtin_defs() -> Vec<Def> {
    vec![list_get(), list_first(), int_div()]
}

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

    // isEq or (==) : a, a -> Bool
    add_type(
        Symbol::BOOL_EQ,
        SolvedType::Func(vec![flex(TVAR1), flex(TVAR1)], Box::new(bool_type())),
    );

    // isNeq or (!=) : a, a -> Bool
    add_type(
        Symbol::BOOL_NEQ,
        SolvedType::Func(vec![flex(TVAR1), flex(TVAR1)], Box::new(bool_type())),
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
        Symbol::NUM_LTE,
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
        Symbol::NUM_GTE,
        SolvedType::Func(
            vec![num_type(flex(TVAR1)), num_type(flex(TVAR1))],
            Box::new(bool_type()),
        ),
    );

    // toFloat : Num a -> Float
    add_type(
        Symbol::NUM_TO_FLOAT,
        SolvedType::Func(vec![num_type(flex(TVAR1))], Box::new(float_type())),
    );

    // Int module

    // equals : Int, Int -> Bool
    add_type(
        Symbol::INT_EQ_I64,
        SolvedType::Func(vec![int_type(), int_type()], Box::new(bool_type())),
    );

    // notEquals : Int, Int -> Bool
    add_type(
        Symbol::INT_NEQ_I64,
        SolvedType::Func(vec![int_type(), int_type()], Box::new(bool_type())),
    );

    // highest : Int
    add_type(Symbol::INT_HIGHEST, int_type());

    // lowest : Int
    add_type(Symbol::INT_LOWEST, int_type());

    // div : Int, Int -> Int
    add_type(
        Symbol::INT_DIV_UNSAFE,
        SolvedType::Func(vec![int_type(), int_type()], Box::new(int_type())),
    );

    let div_by_zero = SolvedType::TagUnion(
        vec![(TagName::Global("DivByZero".into()), vec![])],
        Box::new(SolvedType::Wildcard),
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

    // round : Float -> Int
    add_type(
        Symbol::FLOAT_ROUND,
        SolvedType::Func(vec![float_type()], Box::new(int_type())),
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

    // get : List elem, Int -> Result elem [ OutOfBounds ]*
    let index_out_of_bounds = SolvedType::TagUnion(
        vec![(TagName::Global("OutOfBounds".into()), vec![])],
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
        Symbol::LIST_GET_UNSAFE,
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

    // concat : List elem, List elem -> List elem
    add_type(
        Symbol::LIST_CONCAT,
        SolvedType::Func(
            vec![list_type(flex(TVAR1)), list_type(flex(TVAR1))],
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
            vec![list_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(list_type(flex(TVAR1))),
        ),
    );

    // len : List * -> Int
    add_type(
        Symbol::LIST_LEN,
        SolvedType::Func(vec![list_type(flex(TVAR1))], Box::new(int_type())),
    );

    // isEmpty : List * -> Bool
    add_type(
        Symbol::LIST_IS_EMPTY,
        SolvedType::Func(vec![list_type(flex(TVAR1))], Box::new(bool_type())),
    );

    // Map module

    // empty : Map k v
    add_type(Symbol::MAP_EMPTY, map_type(flex(TVAR1), flex(TVAR2)));

    // singleton : k, v -> Map k v
    add_type(
        Symbol::MAP_SINGLETON,
        SolvedType::Func(
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
        SolvedType::Func(
            vec![map_type(flex(TVAR1), flex(TVAR2)), flex(TVAR1)],
            Box::new(result_type(flex(TVAR2), key_not_found)),
        ),
    );

    add_type(
        Symbol::MAP_INSERT,
        SolvedType::Func(
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
        SolvedType::Func(vec![flex(TVAR1)], Box::new(set_type(flex(TVAR1)))),
    );

    // union : Set a, Set a -> Set a
    add_type(
        Symbol::SET_UNION,
        SolvedType::Func(
            vec![set_type(flex(TVAR1)), set_type(flex(TVAR1))],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    // diff : Set a, Set a -> Set a
    add_type(
        Symbol::SET_DIFF,
        SolvedType::Func(
            vec![set_type(flex(TVAR1)), set_type(flex(TVAR1))],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    // foldl : Set a, (a -> b -> b), b -> b
    add_type(
        Symbol::SET_FOLDL,
        SolvedType::Func(
            vec![
                set_type(flex(TVAR1)),
                SolvedType::Func(vec![flex(TVAR1), flex(TVAR2)], Box::new(flex(TVAR2))),
                flex(TVAR2),
            ],
            Box::new(flex(TVAR2)),
        ),
    );

    add_type(
        Symbol::SET_INSERT,
        SolvedType::Func(
            vec![set_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(set_type(flex(TVAR1))),
        ),
    );

    add_type(
        Symbol::SET_REMOVE,
        SolvedType::Func(
            vec![set_type(flex(TVAR1)), flex(TVAR1)],
            Box::new(set_type(flex(TVAR1))),
        ),
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

#[inline(always)]
fn set_type(a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::SET_SET, vec![a])
}

#[inline(always)]
fn map_type(key: SolvedType, value: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::MAP_MAP, vec![key, value])
}

#[inline(always)]
fn no_region<T>(value: T) -> Located<T> {
    Located {
        region: Region::zero(),
        value,
    }
}

#[inline(always)]
fn tag(name: &'static str, args: Vec<Expr>, var_store: &VarStore) -> Expr {
    Expr::Tag {
        variant_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        name: TagName::Global(name.into()),
        arguments: args
            .into_iter()
            .map(|expr| (var_store.fresh(), no_region(expr)))
            .collect::<Vec<(Variable, Located<Expr>)>>(),
    }
}

#[inline(always)]
fn call(symbol: Symbol, args: Vec<Expr>, var_store: &VarStore) -> Expr {
    Expr::Call(
        Box::new((
            var_store.fresh(),
            no_region(Expr::Var(symbol)),
            var_store.fresh(),
        )),
        args.into_iter()
            .map(|expr| (var_store.fresh(), no_region(expr)))
            .collect::<Vec<(Variable, Located<Expr>)>>(),
        CalledVia::Space,
    )
}

#[inline(always)]
fn defn(fn_name: Symbol, args: Vec<Symbol>, var_store: &VarStore, body: Expr) -> Def {
    use roc_can::expr::Expr::*;
    use roc_can::pattern::Pattern::*;

    let closure_args = args
        .into_iter()
        .map(|symbol| (var_store.fresh(), no_region(Identifier(symbol))))
        .collect();

    let expr = Closure(
        var_store.fresh(),
        fn_name,
        Recursive::NotRecursive,
        closure_args,
        Box::new((no_region(body), var_store.fresh())),
    );

    Def {
        loc_pattern: no_region(Identifier(fn_name)),
        loc_expr: no_region(expr),
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
    }
}

/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
fn list_get(var_store: &VarStore) -> Def {
    use roc_can::expr::Expr::*;

    defn(
        Symbol::LIST_GET,
        vec![Symbol::LIST_GET_ARG_LIST, Symbol::LIST_GET_ARG_INDEX],
        var_store,
        // Perform a bounds check. If it passes, delegate to List.#getUnsafe
        If {
            cond_var: var_store.fresh(),
            branch_var: var_store.fresh(),
            branches: vec![(
                // if-condition
                no_region(
                    // index < List.len list
                    call(
                        Symbol::NUM_LT,
                        vec![
                            Var(Symbol::LIST_GET_ARG_INDEX),
                            call(
                                Symbol::LIST_LEN,
                                vec![Var(Symbol::LIST_GET_ARG_LIST)],
                                var_store,
                            ),
                        ],
                        var_store,
                    ),
                ),
                // then-branch
                no_region(
                    // Ok
                    tag(
                        "Ok",
                        vec![
                            // List.getUnsafe list index
                            Call(
                                Box::new((
                                    var_store.fresh(),
                                    no_region(Var(Symbol::LIST_GET_UNSAFE)),
                                    var_store.fresh(),
                                )),
                                vec![
                                    (var_store.fresh(), no_region(Var(Symbol::LIST_GET_ARG_LIST))),
                                    (
                                        var_store.fresh(),
                                        no_region(Var(Symbol::LIST_GET_ARG_INDEX)),
                                    ),
                                ],
                                CalledVia::Space,
                            ),
                        ],
                        var_store,
                    ),
                ),
            )],
            final_else: Box::new(
                // else-branch
                no_region(
                    // Err
                    tag(
                        "Err",
                        vec![tag("OutOfBounds", Vec::new(), var_store)],
                        var_store,
                    ),
                ),
            ),
        },
    )
}

/// Int.div : Int, Int -> Result Int [ DivByZero ]*
fn int_div(var_store: &VarStore) -> Def {
    use roc_can::expr::Expr::*;
    use roc_can::pattern::Pattern::*;

    let args = vec![
        (
            var_store.fresh(),
            no_region(Identifier(Symbol::INT_DIV_ARG_NUMERATOR)),
        ),
        (
            var_store.fresh(),
            no_region(Identifier(Symbol::INT_DIV_ARG_DENOMINATOR)),
        ),
    ];

    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // Int.eq denominator 0
                call(
                    Symbol::INT_NEQ_I64,
                    vec![
                        Var(Symbol::INT_DIV_ARG_DENOMINATOR),
                        (Int(var_store.fresh(), 0)),
                    ],
                    var_store,
                ),
            ),
            // denominator was not zero
            no_region(
                // Ok (Int.#divUnsafe numerator denominator)
                tag(
                    "Ok",
                    vec![
                        // Int.#divUnsafe numerator denominator
                        call(
                            Symbol::INT_DIV_UNSAFE,
                            vec![
                                (Var(Symbol::INT_DIV_ARG_NUMERATOR)),
                                (Var(Symbol::INT_DIV_ARG_DENOMINATOR)),
                            ],
                            var_store,
                        ),
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // denominator was zero
            no_region(tag(
                "Err",
                vec![tag("DivByZero", Vec::new(), var_store)],
                var_store,
            )),
        ),
    };

    let expr = Closure(
        var_store.fresh(),
        Symbol::INT_DIV,
        Recursive::NotRecursive,
        args,
        Box::new((no_region(body), var_store.fresh())),
    );

    Def {
        loc_pattern: no_region(Identifier(Symbol::INT_DIV)),
        loc_expr: no_region(expr),
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
    }
}

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
fn list_first(var_store: &VarStore) -> Def {
    use roc_can::expr::Expr::*;

    defn(
        Symbol::LIST_FIRST,
        vec![Symbol::LIST_FIRST_ARG],
        var_store,
        // Perform a bounds check. If it passes, delegate to List.getUnsafe.
        If {
            // TODO Use "when" instead of "if" so that we can have False be the first branch.
            // We want that for branch prediction; usually we expect the list to be nonempty.
            cond_var: var_store.fresh(),
            branch_var: var_store.fresh(),
            branches: vec![(
                // if-condition
                no_region(
                    // List.isEmpty list
                    call(
                        Symbol::LIST_IS_EMPTY,
                        vec![Var(Symbol::LIST_FIRST_ARG)],
                        var_store,
                    ),
                ),
                // list was empty
                no_region(
                    // Err ListWasEmpty
                    tag(
                        "Err",
                        vec![tag("ListWasEmpty", Vec::new(), var_store)],
                        var_store,
                    ),
                ),
            )],
            final_else: Box::new(
                // list was not empty
                no_region(
                    // Ok (List.#getUnsafe list 0)
                    tag(
                        "Ok",
                        vec![
                            // List.#getUnsafe list 0
                            call(
                                Symbol::LIST_GET_UNSAFE,
                                vec![(Var(Symbol::LIST_FIRST_ARG)), (Int(var_store.fresh(), 0))],
                                var_store,
                            ),
                        ],
                        var_store,
                    ),
                ),
            ),
        },
    )
}
