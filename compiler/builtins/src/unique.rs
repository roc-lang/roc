use crate::std::StdLib;
use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::solved_types::{BuiltinAlias, SolvedAtom, SolvedType};
use roc_types::subs::VarId;
use std::collections::HashMap;

/// Keep this up to date by hand!
///
const NUM_BUILTIN_IMPORTS: usize = 7;

/// These can be shared between definitions, they will get instantiated when converted to Type
const TVAR1: VarId = VarId::from_u32(1);
const TVAR2: VarId = VarId::from_u32(2);
const TVAR3: VarId = VarId::from_u32(3);

/// These can be shared between definitions, they will get instantiated when converted to Type
const FUVAR: VarId = VarId::from_u32(1000);
const UVAR1: VarId = VarId::from_u32(1001);
const UVAR2: VarId = VarId::from_u32(1002);
const UVAR3: VarId = VarId::from_u32(1003);
const UVAR4: VarId = VarId::from_u32(1004);
const UVAR5: VarId = VarId::from_u32(1005);
const UVAR6: VarId = VarId::from_u32(1006);

pub struct IDStore(u32);

impl IDStore {
    fn new() -> Self {
        IDStore(2000)
    }

    fn fresh(&mut self) -> VarId {
        let result = VarId::from_u32(self.0);

        self.0 += 1;

        result
    }
}

fn shared() -> SolvedType {
    SolvedType::Boolean(SolvedAtom::Zero, vec![])
}

fn boolean(b: VarId) -> SolvedType {
    SolvedType::Boolean(SolvedAtom::Variable(b), vec![])
}

fn disjunction(free: VarId, rest: Vec<VarId>) -> SolvedType {
    let solved_rest = rest.into_iter().map(SolvedAtom::Variable).collect();

    SolvedType::Boolean(SolvedAtom::Variable(free), solved_rest)
}

pub fn uniq_stdlib() -> StdLib {
    use crate::std::Mode;

    let types = types();
    let aliases = aliases();

    /*
    debug_assert!({
        let normal_types: MutSet<Symbol> = builtins::types().keys().copied().collect();
        let normal_aliases: MutSet<Symbol> = builtins::aliases().keys().copied().collect();

        let unique_types = types.keys().copied().collect();
        let unique_aliases = aliases.keys().copied().collect();

        let missing_unique_types: MutSet<Symbol> =
            normal_types.difference(&unique_types).copied().collect();
        let missing_normal_types: MutSet<Symbol> =
            unique_types.difference(&normal_types).copied().collect();

        let missing_unique_aliases: MutSet<Symbol> = normal_aliases
            .difference(&unique_aliases)
            .copied()
            .collect();
        let missing_normal_aliases: MutSet<Symbol> = unique_aliases
            .difference(&normal_aliases)
            .copied()
            .filter(|v| *v != Symbol::ATTR_ATTR)
            .collect();

        let cond = missing_normal_types.is_empty()
            && missing_unique_types.is_empty()
            && missing_normal_aliases.is_empty()
            && missing_unique_aliases.is_empty();

        if !cond {
            println!("Missing hardcoded types for:");
            println!("normal types: {:?}", missing_normal_types);
            println!("unique types: {:?}", missing_unique_types);
            println!("normal aliases: {:?}", missing_normal_aliases);
            println!("unique aliases: {:?}", missing_unique_aliases);
        }

        cond
    });
    */

    StdLib {
        mode: Mode::Uniqueness,
        types,
        aliases,
        applies: vec![
            Symbol::ATTR_ATTR,
            Symbol::LIST_LIST,
            Symbol::SET_SET,
            Symbol::MAP_MAP,
            Symbol::STR_STR,
        ]
        .into_iter()
        .collect(),
    }
}

pub fn aliases() -> MutMap<Symbol, BuiltinAlias> {
    // let mut aliases = builtins::aliases();
    let mut aliases = MutMap::default();

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

    // Num : Num Integer
    add_alias(
        Symbol::NUM_NUM,
        BuiltinAlias {
            region: Region::zero(),
            vars: vec![Located::at(Region::zero(), "a".into())],
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

    // FloatingPoint : [ @FloatingPoint ]
    add_alias(
        Symbol::FLOAT_FLOATINGPOINT,
        BuiltinAlias {
            region: Region::zero(),
            vars: Vec::new(),
            typ: single_private_tag(Symbol::FLOAT_AT_FLOATINGPOINT, Vec::new()),
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
                vec![lift(
                    UVAR1,
                    SolvedType::Apply(Symbol::INT_INTEGER, Vec::new()),
                )],
            ),
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
                vec![lift(
                    UVAR1,
                    SolvedType::Apply(Symbol::FLOAT_FLOATINGPOINT, Vec::new()),
                )],
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

    // add or (+) : Attr u1 (Num a), Attr u2 (Num a) -> Attr u3 (Num a)
    add_type(
        Symbol::NUM_ADD,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            num_type(UVAR3, TVAR1),
        ),
    );

    // sub or (-) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_SUB,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            num_type(UVAR3, TVAR1),
        ),
    );

    // mul or (*) : Num a, Num a -> Num a
    add_type(
        Symbol::NUM_MUL,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            num_type(UVAR3, TVAR1),
        ),
    );

    // abs : Num a -> Num a
    add_type(
        Symbol::NUM_ABS,
        unique_function(vec![num_type(UVAR1, TVAR1)], num_type(UVAR2, TVAR1)),
    );

    // neg : Num a -> Num a
    add_type(
        Symbol::NUM_NEG,
        unique_function(vec![num_type(UVAR1, TVAR1)], num_type(UVAR2, TVAR1)),
    );

    // isLt or (<) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LT,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            bool_type(UVAR3),
        ),
    );

    // isLte or (<=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_LE,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            bool_type(UVAR3),
        ),
    );

    // isGt or (>) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_GT,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            bool_type(UVAR3),
        ),
    );

    // isGte or (>=) : Num a, Num a -> Bool
    add_type(
        Symbol::NUM_GE,
        unique_function(
            vec![num_type(UVAR1, TVAR1), num_type(UVAR2, TVAR1)],
            bool_type(UVAR3),
        ),
    );

    // toFloat : Num a -> Float
    add_type(
        Symbol::NUM_TO_FLOAT,
        unique_function(vec![num_type(UVAR1, TVAR1)], float_type(UVAR2)),
    );

    // Int module

    // highest : Int
    add_type(Symbol::INT_HIGHEST, int_type(UVAR1));

    // lowest : Int
    add_type(Symbol::INT_LOWEST, int_type(UVAR1));

    // div or (//) : Int, Int -> Int
    add_type(
        Symbol::INT_DIV,
        unique_function(vec![int_type(UVAR1), int_type(UVAR2)], int_type(UVAR3)),
    );

    // mod : Int, Int -> Int
    add_type(
        Symbol::INT_MOD,
        unique_function(vec![int_type(UVAR1), int_type(UVAR2)], int_type(UVAR3)),
    );

    // Float module

    // div : Float, Float -> Float
    add_type(
        Symbol::FLOAT_DIV,
        unique_function(
            vec![float_type(UVAR1), float_type(UVAR2)],
            float_type(UVAR3),
        ),
    );

    // mod : Float, Float -> Float
    add_type(
        Symbol::FLOAT_MOD,
        unique_function(
            vec![float_type(UVAR1), float_type(UVAR2)],
            float_type(UVAR3),
        ),
    );

    // sqrt : Float -> Float
    add_type(
        Symbol::FLOAT_SQRT,
        unique_function(vec![float_type(UVAR1)], float_type(UVAR2)),
    );

    // round : Float -> Int
    add_type(
        Symbol::FLOAT_ROUND,
        unique_function(vec![float_type(UVAR1)], int_type(UVAR2)),
    );

    // highest : Float
    add_type(Symbol::FLOAT_HIGHEST, float_type(UVAR1));

    // lowest : Float
    add_type(Symbol::FLOAT_LOWEST, float_type(UVAR1));

    // Bool module

    // isEq or (==) : a, a -> Attr u Bool
    add_type(
        Symbol::BOOL_EQ,
        unique_function(vec![flex(TVAR1), flex(TVAR1)], bool_type(UVAR3)),
    );

    // isNeq or (!=) : a, a -> Attr u Bool
    add_type(
        Symbol::BOOL_NEQ,
        unique_function(vec![flex(TVAR1), flex(TVAR1)], bool_type(UVAR3)),
    );

    // and or (&&) : Attr u1 Bool, Attr u2 Bool -> Attr u3 Bool
    add_type(
        Symbol::BOOL_AND,
        unique_function(vec![bool_type(UVAR1), bool_type(UVAR2)], bool_type(UVAR3)),
    );

    // or or (||)  : Attr u1 Bool, Attr u2 Bool -> Attr u3 Bool
    add_type(
        Symbol::BOOL_OR,
        unique_function(vec![bool_type(UVAR1), bool_type(UVAR2)], bool_type(UVAR3)),
    );

    // xor : Attr u1 Bool, Attr u2 Bool -> Attr u3 Bool
    add_type(
        Symbol::BOOL_XOR,
        unique_function(vec![bool_type(UVAR1), bool_type(UVAR2)], bool_type(UVAR3)),
    );

    // not : Attr u1 Bool -> Attr u2 Bool
    add_type(
        Symbol::BOOL_NOT,
        unique_function(vec![bool_type(UVAR1)], bool_type(UVAR2)),
    );

    // List module

    // isEmpty : Attr u (List *) -> Attr v Bool
    add_type(
        Symbol::LIST_IS_EMPTY,
        unique_function(vec![list_type(UVAR1, TVAR1)], bool_type(UVAR2)),
    );

    // len : Attr u (List *) -> Attr v Int
    add_type(
        Symbol::LIST_LEN,
        unique_function(vec![list_type(UVAR1, TVAR1)], int_type(UVAR2)),
    );

    // get : List a, Int -> Result a [ OutOfBounds ]*
    let index_out_of_bounds = SolvedType::TagUnion(
        vec![(TagName::Global("OutOfBounds".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(
        Symbol::LIST_GET,
        unique_function(
            vec![list_type(UVAR1, TVAR1), int_type(UVAR2)],
            result_type(UVAR3, flex(TVAR1), lift(UVAR4, index_out_of_bounds)),
        ),
    );

    add_type(
        Symbol::LIST_GET_UNSAFE,
        unique_function(vec![list_type(UVAR1, TVAR1), int_type(UVAR2)], flex(TVAR1)),
    );

    // set : Attr (w | u | v) (List (Attr u a))
    //     , Attr * Int
    //     , Attr (u | v) a
    //    -> List a
    add_type(Symbol::LIST_SET, {
        let u = UVAR1;
        let v = UVAR2;
        let w = UVAR3;
        let star1 = UVAR4;
        let star2 = UVAR5;

        let a = TVAR1;

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(w, vec![u, v]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                int_type(star1),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![disjunction(u, vec![v]), flex(a)]),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star2),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                ],
            ),
        )
    });

    // push : Attr (w | u | v) (List (Attr u a))
    //      , Attr (u | v) a
    //     -> Attr * (List (Attr u a))
    add_type(Symbol::LIST_PUSH, {
        let u = UVAR1;
        let v = UVAR2;
        let w = UVAR3;
        let star = UVAR4;

        let a = TVAR1;

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(w, vec![u, v]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![disjunction(u, vec![v]), flex(a)]),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                ],
            ),
        )
    });

    // map : List a, (a -> b) -> List b
    add_type(
        Symbol::LIST_MAP,
        unique_function(
            vec![
                list_type(UVAR1, TVAR1),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        shared(),
                        SolvedType::Func(vec![flex(TVAR1)], Box::new(flex(TVAR2))),
                    ],
                ),
            ],
            list_type(UVAR2, TVAR2),
        ),
    );

    // foldr : List a, (a -> b -> b), b -> b
    add_type(
        Symbol::LIST_FOLDR,
        unique_function(
            vec![
                list_type(UVAR1, TVAR1),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        shared(),
                        SolvedType::Func(vec![flex(TVAR1), flex(TVAR2)], Box::new(flex(TVAR2))),
                    ],
                ),
                flex(TVAR2),
            ],
            flex(TVAR2),
        ),
    );

    // Map module

    // empty : Map k v
    add_type(Symbol::MAP_EMPTY, map_type(UVAR1, TVAR1, TVAR2));

    // singleton : k, v -> Map k v
    add_type(
        Symbol::MAP_SINGLETON,
        unique_function(
            vec![flex(TVAR1), flex(TVAR2)],
            map_type(UVAR1, TVAR1, TVAR2),
        ),
    );

    // get : Attr (u | v | *) (Map (Attr u key) (Attr v val), (Attr * key) -> Attr * (Result (Attr v val) [ KeyNotFound ]*)
    let key_not_found = SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![
            SolvedType::Wildcard,
            SolvedType::TagUnion(
                vec![(TagName::Global("KeyNotFound".into()), vec![])],
                Box::new(SolvedType::Wildcard),
            ),
        ],
    );

    add_type(Symbol::MAP_GET, {
        let mut store = IDStore::new();

        let u = store.fresh();
        let v = store.fresh();
        let key = store.fresh();
        let val = store.fresh();
        let star1 = store.fresh();
        let star2 = store.fresh();
        let star3 = store.fresh();

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u, v]),
                        SolvedType::Apply(
                            Symbol::MAP_MAP,
                            vec![attr_type(u, key), attr_type(v, val)],
                        ),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![disjunction(star2, vec![u]), flex(key)],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star3),
                    SolvedType::Apply(
                        Symbol::RESULT_RESULT,
                        vec![attr_type(v, val), key_not_found],
                    ),
                ],
            ),
        )
    });

    // insert : Attr (u | v | *) (Map (Attr u key) (Attr v val)), Attr (u | *) key, Attr (v | *) val -> Attr * (Map (Attr u key) (Attr v val))
    add_type(Symbol::MAP_INSERT, {
        let mut store = IDStore::new();

        let u = store.fresh();
        let v = store.fresh();
        let key = store.fresh();
        let val = store.fresh();
        let star1 = store.fresh();
        let star2 = store.fresh();
        let star3 = store.fresh();

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u, v]),
                        SolvedType::Apply(
                            Symbol::MAP_MAP,
                            vec![attr_type(u, key), attr_type(v, val)],
                        ),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![disjunction(star2, vec![u]), flex(key)],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![disjunction(star2, vec![v]), flex(val)],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star3),
                    SolvedType::Apply(Symbol::MAP_MAP, vec![attr_type(u, key), attr_type(v, val)]),
                ],
            ),
        )
    });

    // Set module

    // empty : Set a
    add_type(Symbol::SET_EMPTY, set_type(UVAR1, TVAR1));

    // singleton : a -> Set a
    add_type(
        Symbol::SET_SINGLETON,
        unique_function(vec![flex(TVAR1)], set_type(UVAR1, TVAR1)),
    );

    // op : Attr (u | *) (Set (Attr u a)), Attr (u | *) (Set (Attr u a)) -> Attr * Set (Attr u a)
    let set_combine = {
        let mut store = IDStore::new();

        let u = store.fresh();
        let a = store.fresh();
        let star1 = store.fresh();
        let star2 = store.fresh();
        let star3 = store.fresh();

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u]),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star2, vec![u]),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                    ],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star3),
                    SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                ],
            ),
        )
    };

    // union : Set a, Set a -> Set a
    add_type(Symbol::SET_UNION, set_combine.clone());

    // diff : Set a, Set a -> Set a
    add_type(Symbol::SET_DIFF, set_combine);

    // foldl : Attr (u | *) (Set (Attr u a)), Attr Shared (Attr u a -> b -> b), b -> b
    add_type(Symbol::SET_FOLDL, {
        let mut store = IDStore::new();

        let u = store.fresh();
        let a = store.fresh();
        let b = store.fresh();
        let star1 = store.fresh();

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u]),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        shared(),
                        SolvedType::Func(vec![attr_type(u, a), flex(b)], Box::new(flex(b))),
                    ],
                ),
                flex(b),
            ],
            flex(b),
        )
    });

    // insert : Attr (u | *) (Set (Attr u a)), Attr (u | *) a -> Attr * (Set (Attr u a))
    add_type(Symbol::SET_INSERT, {
        let mut store = IDStore::new();

        let u = store.fresh();
        let a = store.fresh();
        let star1 = store.fresh();
        let star2 = store.fresh();
        let star3 = store.fresh();

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u]),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![disjunction(star2, vec![u]), flex(a)],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star3),
                    SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                ],
            ),
        )
    });

    // we can remove a key that is shared from a set of unique keys
    // remove : Attr (u | *) (Set (Attr u a)), Attr * a -> Attr * (Set (Attr u a))
    add_type(Symbol::SET_REMOVE, {
        let mut store = IDStore::new();

        let u = store.fresh();
        let a = store.fresh();
        let star1 = store.fresh();
        let star2 = store.fresh();
        let star3 = store.fresh();

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u]),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                    ],
                ),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![flex(star2), flex(a)]),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star3),
                    SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                ],
            ),
        )
    });

    // Str module

    // isEmpty : Attr u Str -> Attr v Bool
    add_type(Symbol::STR_ISEMPTY, {
        unique_function(vec![str_type(UVAR1)], bool_type(UVAR2))
    });

    // Result module

    // map : Attr (* | u | v) (Result (Attr u a) e), Attr * (Attr u a -> b) -> Attr * (Result b e)
    add_type(Symbol::RESULT_MAP, {
        let u = UVAR1;
        let star1 = UVAR4;
        let star2 = UVAR5;
        let star3 = UVAR6;

        let a = TVAR1;
        let b = TVAR2;
        let e = TVAR3;
        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        disjunction(star1, vec![u]),
                        SolvedType::Apply(Symbol::RESULT_RESULT, vec![attr_type(u, a), flex(e)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star2),
                        SolvedType::Func(vec![attr_type(u, a)], Box::new(flex(b))),
                    ],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star3),
                    SolvedType::Apply(Symbol::RESULT_RESULT, vec![flex(b), flex(e)]),
                ],
            ),
        )
    });

    types
}

#[inline(always)]
fn flex(tvar: VarId) -> SolvedType {
    SolvedType::Flex(tvar)
}

#[inline(always)]
fn unique_function(args: Vec<SolvedType>, ret: SolvedType) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(FUVAR), SolvedType::Func(args, Box::new(ret))],
    )
}

#[allow(dead_code)]
#[inline(always)]
fn attr_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(Symbol::ATTR_ATTR, vec![flex(u), flex(a)])
}

#[inline(always)]
fn lift(u: VarId, a: SolvedType) -> SolvedType {
    SolvedType::Apply(Symbol::ATTR_ATTR, vec![flex(u), a])
}

#[inline(always)]
fn float_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::FLOAT_FLOAT, Vec::new())],
    )
}

#[inline(always)]
fn int_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::INT_INT, Vec::new())],
    )
}

#[inline(always)]
fn bool_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::BOOL_BOOL, Vec::new())],
    )
}

#[inline(always)]
fn str_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::STR_STR, Vec::new())],
    )
}

#[inline(always)]
fn num_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::NUM_NUM, vec![flex(a)])],
    )
}

#[inline(always)]
fn result_type(u: VarId, a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![
            flex(u),
            SolvedType::Apply(Symbol::RESULT_RESULT, vec![a, e]),
        ],
    )
}

#[inline(always)]
fn list_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)])],
    )
}

#[inline(always)]
fn set_type(u: VarId, a: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), SolvedType::Apply(Symbol::SET_SET, vec![flex(a)])],
    )
}

#[inline(always)]
fn map_type(u: VarId, key: VarId, value: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![
            flex(u),
            SolvedType::Apply(Symbol::MAP_MAP, vec![flex(key), flex(value)]),
        ],
    )
}
