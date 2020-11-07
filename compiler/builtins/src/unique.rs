use crate::std::StdLib;
use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::builtin_aliases;
use roc_types::solved_types::{SolvedBool, SolvedType};
use roc_types::subs::VarId;
use std::collections::HashMap;

/// Example:
///
///     let_tvars! { a, b, c }
///
/// This is equivalent to:
///
///     let a = VarId::from_u32(1);
///     let b = VarId::from_u32(2);
///     let c = VarId::from_u32(3);
///
/// The idea is that this is less error-prone than assigning hardcoded IDs by hand.
macro_rules! let_tvars {
    ($($name:ident,)+) => { let_tvars!($($name),+) };
    ($($name:ident),*) => {
        let mut _current_tvar = 0;

        $(
            _current_tvar += 1;

            let $name = VarId::from_u32(_current_tvar);
        )*
    };
}

/// Keep this up to date by hand!
///
const NUM_BUILTIN_IMPORTS: usize = 7;

/// These can be shared between definitions, they will get instantiated when converted to Type
const FUVAR: VarId = VarId::from_u32(1000);
const TOP_LEVEL_CLOSURE_VAR: VarId = VarId::from_u32(1001);

fn shared(base: SolvedType) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![SolvedType::Boolean(SolvedBool::SolvedShared), base],
    )
}

fn boolean(b: VarId) -> SolvedType {
    SolvedType::Boolean(SolvedBool::SolvedContainer(b, vec![]))
}

fn container(cvar: VarId, mvars: Vec<VarId>) -> SolvedType {
    SolvedType::Boolean(SolvedBool::SolvedContainer(cvar, mvars))
}

pub fn uniq_stdlib() -> StdLib {
    use crate::std::Mode;

    let types = types();

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

    fn div_by_zero() -> SolvedType {
        SolvedType::TagUnion(
            vec![(TagName::Global("DivByZero".into()), vec![])],
            Box::new(SolvedType::Wildcard),
        )
    }

    // Num module

    // add or (+) : Attr u (Num (Attr u num))
    //            , Attr v (Num (Attr v num))
    //           -> Attr w (Num (Attr w num))
    add_type(Symbol::NUM_ADD, {
        let_tvars! { u, v, w, num };
        unique_function(vec![num_type(u, num), num_type(v, num)], num_type(w, num))
    });

    // addChecked : Num a, Num a -> Result (Num a) [ IntOverflow ]*
    let overflow = SolvedType::TagUnion(
        vec![(TagName::Global("Overflow".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(Symbol::NUM_ADD_CHECKED, {
        let_tvars! { u, v, w, num, result, star };
        unique_function(
            vec![num_type(u, num), num_type(v, num)],
            result_type(result, num_type(w, num), lift(star, overflow)),
        )
    });

    // addWrap : Int, Int -> Int
    add_type(Symbol::NUM_ADD_WRAP, {
        let_tvars! { u, v, w };
        unique_function(vec![int_type(u), int_type(v)], int_type(w))
    });

    // sub or (-) : Num a, Num a -> Num a
    add_type(Symbol::NUM_SUB, {
        let_tvars! { u, v, w, num };
        unique_function(vec![num_type(u, num), num_type(v, num)], num_type(w, num))
    });

    // mul or (*) : Num a, Num a -> Num a
    add_type(Symbol::NUM_MUL, {
        let_tvars! { u, v, w, num };
        unique_function(vec![num_type(u, num), num_type(v, num)], num_type(w, num))
    });

    // abs : Num a -> Num a
    add_type(Symbol::NUM_ABS, {
        let_tvars! { u, v, num };
        unique_function(vec![num_type(u, num)], num_type(v, num))
    });

    // neg : Num a -> Num a
    add_type(Symbol::NUM_NEG, {
        let_tvars! { u, v, num };
        unique_function(vec![num_type(u, num)], num_type(v, num))
    });

    let mut add_num_comparison = |symbol| {
        add_type(symbol, {
            let_tvars! { u, v, w, num };
            unique_function(vec![num_type(u, num), num_type(v, num)], bool_type(w))
        });
    };

    // isLt or (<) : Num a, Num a -> Bool
    add_num_comparison(Symbol::NUM_LT);

    // isLte or (<=) : Num a, Num a -> Bool
    add_num_comparison(Symbol::NUM_LTE);

    // isGt or (>) : Num a, Num a -> Bool
    add_num_comparison(Symbol::NUM_GT);

    // isGte or (>=) : Num a, Num a -> Bool
    add_num_comparison(Symbol::NUM_GTE);

    // compare : Num a, Num a -> [ LT, EQ, GT ]
    add_type(Symbol::NUM_COMPARE, {
        let_tvars! { u, v, w, num };
        unique_function(vec![num_type(u, num), num_type(v, num)], ordering_type(w))
    });

    // toFloat : Num a -> Float
    add_type(Symbol::NUM_TO_FLOAT, {
        let_tvars! { star1, star2, a };
        unique_function(vec![num_type(star1, a)], float_type(star2))
    });

    // rem : Attr * Int, Attr * Int -> Attr * (Result (Attr * Int) (Attr * [ DivByZero ]*))
    add_type(Symbol::NUM_REM, {
        let_tvars! { star1, star2, star3, star4, star5 };
        unique_function(
            vec![int_type(star1), int_type(star2)],
            result_type(star3, int_type(star4), lift(star5, div_by_zero())),
        )
    });

    // maxInt : Int
    add_type(Symbol::NUM_MAX_INT, {
        let_tvars! { star };
        int_type(star)
    });

    // minInt : Int
    add_type(Symbol::NUM_MIN_INT, {
        let_tvars! { star };
        int_type(star)
    });

    // divFloor or (//) : Int, Int -> Result Int [ DivByZero ]*
    add_type(Symbol::NUM_DIV_INT, {
        let_tvars! { star1, star2, star3, star4, star5 };
        unique_function(
            vec![int_type(star1), int_type(star2)],
            result_type(star3, int_type(star4), lift(star5, div_by_zero())),
        )
    });

    // divFloat : Float, Float -> Float
    add_type(Symbol::NUM_DIV_FLOAT, {
        let_tvars! { star1, star2, star3, star4, star5};
        unique_function(
            vec![float_type(star1), float_type(star2)],
            result_type(star3, float_type(star4), lift(star5, div_by_zero())),
        )
    });

    // round : Float -> Int
    add_type(Symbol::NUM_ROUND, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], int_type(star2))
    });

    // sqrt : Float -> Float
    let sqrt_of_negative = SolvedType::TagUnion(
        vec![(TagName::Global("SqrtOfNegative".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(Symbol::NUM_SQRT, {
        let_tvars! { star1, star2, star3, star4 };
        unique_function(
            vec![float_type(star1)],
            result_type(star2, float_type(star3), lift(star4, sqrt_of_negative)),
        )
    });

    // sin : Float -> Float
    add_type(Symbol::NUM_SIN, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], float_type(star2))
    });

    // cos : Float -> Float
    add_type(Symbol::NUM_COS, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], float_type(star2))
    });

    // tan : Float -> Float
    add_type(Symbol::NUM_TAN, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], float_type(star2))
    });

    // maxFloat : Float
    add_type(Symbol::NUM_MAX_FLOAT, {
        let_tvars! { star };
        float_type(star)
    });

    // minFloat : Float
    add_type(Symbol::NUM_MIN_FLOAT, {
        let_tvars! { star };
        float_type(star)
    });

    // isNegative : Num a -> Bool
    add_type(Symbol::NUM_IS_NEGATIVE, {
        let_tvars! { star1, star2, a };
        unique_function(vec![num_type(star1, a)], bool_type(star2))
    });

    // isPositive : Num a -> Bool
    add_type(Symbol::NUM_IS_POSITIVE, {
        let_tvars! { star1, star2, a };
        unique_function(vec![num_type(star1, a)], bool_type(star2))
    });

    // isZero : Num a -> Bool
    add_type(Symbol::NUM_IS_ZERO, {
        let_tvars! { star1, star2, a };
        unique_function(vec![num_type(star1, a)], bool_type(star2))
    });

    // isEven : Num a -> Bool
    add_type(Symbol::NUM_IS_EVEN, {
        let_tvars! { star1, star2, a };
        unique_function(vec![num_type(star1, a)], bool_type(star2))
    });

    // isOdd : Num a -> Bool
    add_type(Symbol::NUM_IS_ODD, {
        let_tvars! { star1, star2, a };
        unique_function(vec![num_type(star1, a)], bool_type(star2))
    });

    // pow : Float, Float -> Float
    add_type(Symbol::NUM_POW, {
        let_tvars! { star1, star2, star3 };
        unique_function(
            vec![float_type(star1), float_type(star2)],
            float_type(star3),
        )
    });

    // ceiling : Float -> Int
    add_type(Symbol::NUM_CEILING, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], int_type(star2))
    });

    // powInt : Int, Int -> Int
    add_type(Symbol::NUM_POW_INT, {
        let_tvars! { star1, star2, star3 };
        unique_function(vec![int_type(star1), int_type(star2)], int_type(star3))
    });

    // floor : Float -> Int
    add_type(Symbol::NUM_FLOOR, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], int_type(star2))
    });

    // atan : Float -> Float
    add_type(Symbol::NUM_ATAN, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], float_type(star2))
    });

    // acos : Float -> Float
    add_type(Symbol::NUM_ACOS, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], float_type(star2))
    });

    // asin : Float -> Float
    add_type(Symbol::NUM_ASIN, {
        let_tvars! { star1, star2 };
        unique_function(vec![float_type(star1)], float_type(star2))
    });

    // Bool module

    // isEq or (==) : Attr * a, Attr * a -> Attr * Bool
    add_type(Symbol::BOOL_EQ, {
        let_tvars! { star1, star2, star3, a };
        unique_function(
            vec![attr_type(star1, a), attr_type(star2, a)],
            bool_type(star3),
        )
    });

    // isNeq or (!=) : Attr * a, Attr * a -> Attr * Bool
    add_type(Symbol::BOOL_NEQ, {
        let_tvars! { star1, star2, star3, a };
        unique_function(
            vec![attr_type(star1, a), attr_type(star2, a)],
            bool_type(star3),
        )
    });

    // and or (&&) : Attr u1 Bool, Attr u2 Bool -> Attr u3 Bool
    add_type(Symbol::BOOL_AND, {
        let_tvars! { star1, star2, star3};
        unique_function(vec![bool_type(star1), bool_type(star2)], bool_type(star3))
    });

    // or or (||)  : Attr u1 Bool, Attr u2 Bool -> Attr u3 Bool
    add_type(Symbol::BOOL_OR, {
        let_tvars! { star1, star2, star3};
        unique_function(vec![bool_type(star1), bool_type(star2)], bool_type(star3))
    });

    // xor : Attr u1 Bool, Attr u2 Bool -> Attr u3 Bool
    add_type(Symbol::BOOL_XOR, {
        let_tvars! { star1, star2, star3};
        unique_function(vec![bool_type(star1), bool_type(star2)], bool_type(star3))
    });

    // not : Attr u1 Bool -> Attr u2 Bool
    add_type(Symbol::BOOL_NOT, {
        let_tvars! { star1, star2 };
        unique_function(vec![bool_type(star1)], bool_type(star2))
    });

    // List module

    // isEmpty : Attr * (List *) -> Attr * Bool
    add_type(Symbol::LIST_IS_EMPTY, {
        let_tvars! { star1, a, star2 };
        unique_function(vec![list_type(star1, a)], bool_type(star2))
    });

    // len : Attr * (List *) -> Attr * Int
    add_type(Symbol::LIST_LEN, {
        let_tvars! { star1, a, star2 };
        unique_function(vec![list_type(star1, a)], int_type(star2))
    });

    // List.first :
    //     Attr (* | u) (List (Attr u a)),
    //     -> Attr * (Result (Attr u a) (Attr * [ OutOfBounds ]*))
    let list_was_empty = SolvedType::TagUnion(
        vec![(TagName::Global("ListWasEmpty".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(Symbol::LIST_FIRST, {
        let_tvars! { a, u, star1, star2, star3 };

        unique_function(
            vec![SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    container(star1, vec![u]),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                ],
            )],
            result_type(star2, attr_type(u, a), lift(star3, list_was_empty)),
        )
    });

    // List.get :
    //     Attr (* | u) (List (Attr u a)),
    //     Attr * Int
    //     -> Attr * (Result (Attr u a) (Attr * [ OutOfBounds ]*))
    let index_out_of_bounds = SolvedType::TagUnion(
        vec![(TagName::Global("OutOfBounds".into()), vec![])],
        Box::new(SolvedType::Wildcard),
    );

    add_type(Symbol::LIST_GET, {
        let_tvars! { a, u, star1, star2, star3, star4 };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        container(star1, vec![u]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                int_type(star2),
            ],
            result_type(star3, attr_type(u, a), lift(star4, index_out_of_bounds)),
        )
    });

    // List.set :
    //     Attr (w | u | v) (List (Attr u a)),
    //     Attr * Int,
    //     Attr (u | v) a
    //     -> Attr * (List (Attr u  a))
    add_type(Symbol::LIST_SET, {
        let_tvars! { u, v, w, star1, star2, a };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        container(w, vec![u, v]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                int_type(star1),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![container(u, vec![v]), flex(a)]),
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

    // single : a -> Attr * (List a)
    add_type(Symbol::LIST_SINGLE, {
        let_tvars! { a, star };

        unique_function(
            vec![flex(a)],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            ),
        )
    });

    // reverse : Attr * (List (Attr * a)) -> Attr * (List (Attr * a))
    add_type(Symbol::LIST_REVERSE, {
        let_tvars! { a, star1, star2 };

        unique_function(
            vec![SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star1),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            )],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star2),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            ),
        )
    });

    // To repeat an item, it must be shared!
    //
    // repeat : Attr * Int
    //        , Attr Shared a
    //       -> Attr * (List (Attr Shared a))
    add_type(Symbol::LIST_REPEAT, {
        let_tvars! { a, star1, star2 };

        unique_function(
            vec![int_type(star1), shared(flex(a))],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star2),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![shared(flex(a))]),
                ],
            ),
        )
    });

    // concat : Attr * (List (Attr * a)), Attr * (List (Attr * a)) -> Attr * (List (Attr * a))
    add_type(Symbol::LIST_CONCAT, {
        let_tvars! { a, star1, star2, star3 };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star2),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                    ],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star3),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            ),
        )
    });

    // append : Attr * (List a)
    //      , a
    //     -> Attr * (List a)
    //
    // NOTE: we demand the new item to have the same uniqueness as the other list items.
    // It could be allowed to add unique items to shared lists, but that requires special code gen
    add_type(Symbol::LIST_APPEND, {
        let_tvars! { a, star1, star2 };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                    ],
                ),
                flex(a),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star2),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            ),
        )
    });

    // prepend : Attr * (List a)
    //      , a
    //     -> Attr * (List a)
    //
    // NOTE: we demand the new item to have the same uniqueness as the other list items.
    // It could be allowed to add unique items to shared lists, but that requires special code gen
    add_type(Symbol::LIST_PREPEND, {
        let_tvars! { a, star1, star2 };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                    ],
                ),
                flex(a),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star2),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            ),
        )
    });

    // contains : Attr * (List a)
    //          , a
    //          -> Attr * Bool
    add_type(Symbol::LIST_CONTAINS, {
        let_tvars! { a, star1, star2 };

        unique_function(vec![list_type(star1, a), flex(a)], bool_type(star2))
    });

    // join : Attr * (List (Attr * (List a)))
    //     -> Attr * (List a)
    add_type(Symbol::LIST_JOIN, {
        let_tvars! { a, star1, star2, star3 };

        unique_function(
            vec![SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star1),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![list_type(star2, a)]),
                ],
            )],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    boolean(star3),
                    SolvedType::Apply(Symbol::LIST_LIST, vec![flex(a)]),
                ],
            ),
        )
    });

    // List.map does not need to check the container rule on the input list.
    // There is no way in which this signature can cause unique values to be duplicated
    //
    //      foo : Attr Shared (List (Attr u a))
    //
    //      List.map : Attr * (List (Attr u a)) -> (Attr u a -> b) -> Attr * (List b)
    //      List.unsafeGet : Attr (* | u) (List (Attr u a)) -> Attr u a
    //
    //      -- the elements still have uniqueness `u`, and will be made shared whenever accessing an element in `foo`
    //      bar1 : Attr * (List (Attr u a))
    //      bar1 = List.map foo (\x -> x)
    //
    //      -- no reference to `foo`'s elements can escape
    //      bar2 : Attr * (List (Attr * Int))
    //      bar2 = List.map foo (\_ -> 32)

    // map : Attr * (List a)
    //     , Attr Shared (a -> b)
    //    -> Attr * (List b)
    add_type(Symbol::LIST_MAP, {
        let_tvars! { a, b, star1, star2, closure };

        unique_function(
            vec![
                list_type(star1, a),
                shared(SolvedType::Func(
                    vec![flex(a)],
                    Box::new(flex(closure)),
                    Box::new(flex(b)),
                )),
            ],
            list_type(star2, b),
        )
    });

    // keepIf : Attr * (List a)
    //        , Attr Shared (a -> Attr * Bool)
    //       -> Attr * (List a)
    add_type(Symbol::LIST_KEEP_IF, {
        let_tvars! { a, star1, star2, star3, closure };

        unique_function(
            vec![
                list_type(star1, a),
                shared(SolvedType::Func(
                    vec![flex(a)],
                    Box::new(flex(closure)),
                    Box::new(bool_type(star2)),
                )),
            ],
            list_type(star3, a),
        )
    });

    // walkRight : Attr (* | u) (List (Attr u a))
    //           , Attr Shared (Attr u a -> b -> b)
    //           , b
    //          -> b
    add_type(Symbol::LIST_WALK_RIGHT, {
        let_tvars! { u, a, b, star1, closure };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        container(star1, vec![u]),
                        SolvedType::Apply(Symbol::LIST_LIST, vec![attr_type(u, a)]),
                    ],
                ),
                shared(SolvedType::Func(
                    vec![attr_type(u, a), flex(b)],
                    Box::new(flex(closure)),
                    Box::new(flex(b)),
                )),
                flex(b),
            ],
            flex(b),
        )
    });

    // Map module

    // empty : Attr * (Map k v)
    add_type(Symbol::MAP_EMPTY, {
        let_tvars! { star, k , v };
        map_type(star, k, v)
    });

    // singleton : k, v -> Attr * (Map k v)
    add_type(Symbol::MAP_SINGLETON, {
        let_tvars! { star, k , v };
        unique_function(vec![flex(k), flex(v)], map_type(star, k, v))
    });

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

    // get : Attr (* | u) (Map (Attr * key) (Attr u val))
    //     , Attr * key
    //    -> Attr * (Result (Attr u val) [ KeyNotFound ]*)
    add_type(Symbol::MAP_GET, {
        let_tvars! { u, key, val, star1, star2, star3, star4 };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        container(star1, vec![u]),
                        SolvedType::Apply(
                            Symbol::MAP_MAP,
                            vec![attr_type(star2, key), attr_type(u, val)],
                        ),
                    ],
                ),
                SolvedType::Apply(Symbol::ATTR_ATTR, vec![flex(star3), flex(key)]),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star4),
                    SolvedType::Apply(
                        Symbol::RESULT_RESULT,
                        vec![attr_type(u, val), key_not_found],
                    ),
                ],
            ),
        )
    });

    // insert : Attr * (Map key value)
    //        , key
    //        , value
    //        , Attr * (Map key value)
    add_type(Symbol::MAP_INSERT, {
        let_tvars! { star1, star2, key, value };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::MAP_MAP, vec![flex(key), flex(value)]),
                    ],
                ),
                flex(key),
                flex(value),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star2),
                    SolvedType::Apply(Symbol::MAP_MAP, vec![flex(key), flex(value)]),
                ],
            ),
        )
    });

    // Set module

    // empty : Set a
    add_type(Symbol::SET_EMPTY, {
        let_tvars! { star, a };
        set_type(star, a)
    });

    // singleton : a -> Set a
    add_type(Symbol::SET_SINGLETON, {
        let_tvars! { star, a };
        unique_function(vec![flex(a)], set_type(star, a))
    });

    // union : Attr * (Set * a)
    //       , Attr * (Set * a)
    //      -> Attr * (Set * a)
    let set_combine = {
        let_tvars! { star1, star2, star3, star4, star5, star6, a };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(star2, a)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star3),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(star4, a)]),
                    ],
                ),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star5),
                    SolvedType::Apply(Symbol::SET_SET, vec![attr_type(star6, a)]),
                ],
            ),
        )
    };

    // union : Attr * (Set * a)
    //       , Attr * (Set * a)
    //      -> Attr * (Set * a)
    add_type(Symbol::SET_UNION, set_combine.clone());

    // diff : Attr * (Set * a)
    //      , Attr * (Set * a)
    //     -> Attr * (Set * a)
    add_type(Symbol::SET_DIFF, set_combine);

    // foldl : Attr (* | u) (Set (Attr u a))
    //       , Attr Shared (Attr u a -> b -> b)
    //       , b
    //      -> b
    add_type(Symbol::SET_FOLDL, {
        let_tvars! { star, u, a, b, closure };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        container(star, vec![u]),
                        SolvedType::Apply(Symbol::SET_SET, vec![attr_type(u, a)]),
                    ],
                ),
                shared(SolvedType::Func(
                    vec![attr_type(u, a), flex(b)],
                    Box::new(flex(closure)),
                    Box::new(flex(b)),
                )),
                flex(b),
            ],
            flex(b),
        )
    });

    // insert : Attr * (Set a)
    //        , a
    //        , Attr * (Set a)
    add_type(Symbol::SET_INSERT, {
        let_tvars! { star1, star2, a };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::SET_SET, vec![flex(a)]),
                    ],
                ),
                flex(a),
            ],
            SolvedType::Apply(
                Symbol::ATTR_ATTR,
                vec![
                    flex(star2),
                    SolvedType::Apply(Symbol::SET_SET, vec![flex(a)]),
                ],
            ),
        )
    });

    // we can remove a key that is shared from a set of unique keys
    //
    // remove : Attr * (Set (Attr u a))
    //        , Attr * a
    //        , Attr * (Set (Attr u a))
    add_type(Symbol::SET_REMOVE, {
        let_tvars! { u, a, star1, star2, star3 };

        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
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

    // isEmpty : Attr * Str -> Attr * Bool
    add_type(Symbol::STR_IS_EMPTY, {
        let_tvars! { star1, star2 };
        unique_function(vec![str_type(star1)], bool_type(star2))
    });

    // Str.concat : Attr * Str, Attr * Str -> Attr * Str
    add_type(Symbol::STR_CONCAT, {
        let_tvars! { star1, star2, star3 };
        unique_function(vec![str_type(star1), str_type(star2)], str_type(star3))
    });

    // Result module

    // map : Attr * (Result (Attr a e))
    //     , Attr * (a -> b)
    //    -> Attr * (Result b e)
    add_type(Symbol::RESULT_MAP, {
        let_tvars! { star1, star2, star3, a, b, e, closure };
        unique_function(
            vec![
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star1),
                        SolvedType::Apply(Symbol::RESULT_RESULT, vec![flex(a), flex(e)]),
                    ],
                ),
                SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![
                        flex(star2),
                        SolvedType::Func(vec![flex(a)], Box::new(flex(closure)), Box::new(flex(b))),
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
        vec![
            flex(FUVAR),
            SolvedType::Func(
                args,
                Box::new(SolvedType::Flex(TOP_LEVEL_CLOSURE_VAR)),
                Box::new(ret),
            ),
        ],
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
        vec![
            flex(u),
            SolvedType::Alias(
                Symbol::NUM_FLOAT,
                Vec::new(),
                Box::new(builtin_aliases::num_type(SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![flex(u), builtin_aliases::floatingpoint_type()],
                ))),
            ),
        ],
    )
}

#[inline(always)]
fn int_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![
            flex(u),
            SolvedType::Alias(
                Symbol::NUM_INT,
                Vec::new(),
                Box::new(builtin_aliases::num_type(SolvedType::Apply(
                    Symbol::ATTR_ATTR,
                    vec![flex(u), builtin_aliases::integer_type()],
                ))),
            ),
        ],
    )
}

#[inline(always)]
fn bool_type(u: VarId) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), builtin_aliases::bool_type()],
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
        vec![flex(u), builtin_aliases::num_type(attr_type(u, a))],
    )
}

#[inline(always)]
fn result_type(u: VarId, a: SolvedType, e: SolvedType) -> SolvedType {
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![flex(u), builtin_aliases::result_type(a, e)],
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

#[inline(always)]
fn ordering_type(u: VarId) -> SolvedType {
    // [ LT, EQ, GT ]
    SolvedType::Apply(
        Symbol::ATTR_ATTR,
        vec![
            flex(u),
            SolvedType::TagUnion(
                vec![
                    (TagName::Global("GT".into()), vec![]),
                    (TagName::Global("EQ".into()), vec![]),
                    (TagName::Global("LT".into()), vec![]),
                ],
                Box::new(SolvedType::EmptyTagUnion),
            ),
        ],
    )
}
