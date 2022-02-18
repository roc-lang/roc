#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

// #[cfg(feature = "gen-dev")]
// use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

// use crate::assert_wasm_evals_to as assert_evals_to;
#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocList, RocStr};

#[test]
fn width_and_alignment_u8_u8() {
    use roc_mono::layout::Layout;
    use roc_mono::layout::UnionLayout;

    let t = &[Layout::u8()] as &[_];
    let tt = [t, t];

    let layout = Layout::Union(UnionLayout::NonRecursive(&tt));

    let target_info = roc_target::TargetInfo::default_x86_64();
    assert_eq!(layout.alignment_bytes(target_info), 1);
    assert_eq!(layout.stack_size(target_info), 2);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn applied_tag_nothing_ir() {
    assert_evals_to!(
        indoc!(
            r#"
                Maybe a : [ Just a, Nothing ]

                x : Maybe I64
                x = Nothing

                x
                "#
        ),
        1,
        (i64, u8),
        |(_, tag)| tag
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn applied_tag_nothing() {
    assert_evals_to!(
        indoc!(
            r#"
                Maybe a : [ Just a, Nothing ]

                x : Maybe I64
                x = Nothing

                x
                "#
        ),
        1,
        (i64, u8),
        |(_, tag)| tag
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn applied_tag_just() {
    assert_evals_to!(
        indoc!(
            r#"
                Maybe a : [ Just a, Nothing ]

                y : Maybe I64
                y = Just 0x4

                y
                "#
        ),
        (0x4, 0),
        (i64, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn applied_tag_just_ir() {
    assert_evals_to!(
        indoc!(
            r#"
                Maybe a : [ Just a, Nothing ]

                y : Maybe I64
                y = Just 0x4

                y
                "#
        ),
        (0x4, 0),
        (i64, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn applied_tag_just_enum() {
    assert_evals_to!(
        indoc!(
            r#"
                Fruit : [ Orange, Apple, Banana ]
                Maybe a : [ Just a, Nothing ]

                orange : Fruit
                orange = Orange

                y : Maybe Fruit
                y = Just orange

                y
                "#
        ),
        (2, 0),
        (u8, u8)
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm"))]
// fn raw_result() {
//     assert_evals_to!(
//         indoc!(
//             r#"
//             x : Result I64 I64
//             x = Err 41

//             x
//             "#
//         ),
//         0,
//         i8
//     );
// }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn true_is_true() {
    assert_evals_to!(
        indoc!(
            r#"
                   bool : [True, False]
                   bool = True

                   bool
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn false_is_false() {
    assert_evals_to!(
        indoc!(
            r#"
                   bool : [True, False]
                   bool = False

                   bool
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn basic_enum() {
    assert_evals_to!(
        indoc!(
            r#"
                Fruit : [ Apple, Orange, Banana ]

                apple : Fruit
                apple = Apple

                orange : Fruit
                orange = Orange

                apple == orange
                "#
        ),
        false,
        bool
    );
}

//    #[test]
// #[cfg(any(feature = "gen-llvm"))]
//    fn linked_list_empty() {
//        assert_evals_to!(
//            indoc!(
//                r#"
//                LinkedList a : [ Cons a (LinkedList a), Nil ]
//
//                empty : LinkedList I64
//                empty = Nil
//
//                1
//                "#
//            ),
//            1,
//            i64
//        );
//    }
//
//    #[test]
// #[cfg(any(feature = "gen-llvm"))]
//    fn linked_list_singleton() {
//        assert_evals_to!(
//            indoc!(
//                r#"
//                LinkedList a : [ Cons a (LinkedList a), Nil ]
//
//                singleton : LinkedList I64
//                singleton = Cons 0x1 Nil
//
//                1
//                "#
//            ),
//            1,
//            i64
//        );
//    }
//
//    #[test]
// #[cfg(any(feature = "gen-llvm"))]
//    fn linked_list_is_empty() {
//        assert_evals_to!(
//            indoc!(
//                r#"
//                LinkedList a : [ Cons a (LinkedList a), Nil ]
//
//                isEmpty : LinkedList a -> Bool
//                isEmpty = \list ->
//                    when list is
//                        Nil -> True
//                        Cons _ _ -> False
//
//                isEmpty (Cons 4 Nil)
//                "#
//            ),
//            false,
//            bool
//        );
//    }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn even_odd() {
    assert_evals_to!(
        indoc!(
            r#"
                even = \n ->
                    when n is
                        0 -> True
                        1 -> False
                        _ -> odd (n - 1)

                odd = \n ->
                    when n is
                        0 -> False
                        1 -> True
                        _ -> even (n - 1)

                odd 5 && even 42
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_literal_true() {
    assert_evals_to!(
        indoc!(
            r#"
                if True then -1 else 1
                "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_if_float() {
    assert_evals_to!(
        indoc!(
            r#"
                if True then -1.0 else 1.0
                "#
        ),
        -1.0,
        f64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_nothing() {
    assert_evals_to!(
        indoc!(
            r#"
                x : [ Nothing, Just I64 ]
                x = Nothing

                when x is
                    Nothing -> 0x2
                    Just _ -> 0x1
                "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_just() {
    assert_evals_to!(
        indoc!(
            r#"
                x : [ Nothing, Just I64 ]
                x = Just 41

                when x is
                    Just v -> v + 0x1
                    Nothing -> 0x1
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_result() {
    assert_evals_to!(
        indoc!(
            r#"
                x : Result I64 I64
                x = Err 41

                when x is
                    Err v ->  v + 1
                    Ok _ -> 1
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_these() {
    assert_evals_to!(
        indoc!(
            r#"
                These a b : [ This a, That b, These a b ]

                x : These I64 I64
                x = These 0x3 0x2

                when x is
                    These a b -> a + b
                    That v -> v
                    This v -> v
                "#
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn match_on_two_values() {
    // this will produce a Chain internally
    assert_evals_to!(
        indoc!(
            r#"
                when Pair 2 3 is
                    Pair 4 3 -> 9
                    Pair a b -> a + b
                "#
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn pair_with_underscore() {
    assert_evals_to!(
        indoc!(
            r#"
                when Pair 2 3 is
                    Pair 4 _ -> 1
                    Pair 3 _ -> 2
                    Pair a b -> a + b
                "#
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn result_with_underscore() {
    // This test revealed an issue with hashing Test values
    assert_evals_to!(
        indoc!(
            r#"
            x : Result I64 I64
            x = Ok 2

            when x is
                Ok 3 -> 1
                Ok _ -> 2
                Err _ -> 3
            "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn maybe_is_just_not_nested() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

                Maybe a : [ Just a, Nothing ]

                isJust : Maybe a -> Bool
                isJust = \list ->
                    when list is
                        Nothing -> False
                        Just _ -> True

                main =
                    isJust (Just 42)
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn maybe_is_just_nested() {
    assert_evals_to!(
        indoc!(
            r#"
                Maybe a : [ Just a, Nothing ]

                isJust : Maybe a -> Bool
                isJust = \list ->
                    when list is
                        Nothing -> False
                        Just _ -> True

                isJust (Just 42)
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn nested_pattern_match() {
    assert_evals_to!(
        indoc!(
            r#"
                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe I64)
                x = Just (Just 41)

                when x is
                    Just (Just v) -> v + 0x1
                    _ -> 0x1
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn if_guard_vanilla() {
    assert_evals_to!(
        indoc!(
            r#"
                when "fooz" is
                    s if s == "foo" -> 0
                    s -> List.len (Str.toUtf8 s)
                "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn when_on_single_value_tag() {
    // this fails because the switched-on symbol is not defined
    assert_evals_to!(
        indoc!(
            r#"
            when Identity 0 is
                Identity 0 -> 0
                Identity s -> s
            "#
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_multiple() {
    assert_evals_to!(
        indoc!(
            r#"
            f = \n ->
                when Identity n 0 is
                        Identity x _ if x == 0 -> x + 0
                        Identity x _ if x == 1 -> x + 0
                        Identity x _ if x == 2 -> x + 0
                        Identity x _ -> x - x

            { a: f 0, b: f 1, c: f 2, d: f 4 }
                "#
        ),
        [0, 1, 2, 0],
        [i64; 4]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_constructor_switch() {
    assert_evals_to!(
        indoc!(
            r#"
            when Identity 32 0 is
                    Identity 41 _ -> 0
                    Identity s 0 if s == 32 -> 3
                    # Identity s 0 -> s
                    Identity z _ -> z
                "#
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when Identity 42 "" is
                    Identity 41 _ -> 0
                    Identity 42 _ if 3 == 3 -> 1
                    Identity z _ -> z
                "#
        ),
        1,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when Identity 42 "" is
                    Identity 41 _ -> 0
                    Identity 42 _ if 3 != 3 -> 1
                    Identity z _ -> z
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_constructor_chain() {
    assert_evals_to!(
        indoc!(
            r#"
            when Identity 43 0 is
                    Identity 42 _ if 3 == 3 -> 43
                    # Identity 42 _ -> 1
                    Identity z _ -> z
                "#
        ),
        43,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_pattern_false() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    when 2 is
                        2 if False -> 0
                        _ -> 42

                wrapper {}
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_switch() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    when 2 is
                        2 | 3 if False -> 0
                        _ -> 42

                wrapper {}
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_pattern_true() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    when 2 is
                        2 if True -> 42
                        _ -> 0

                wrapper {}
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn if_guard_exhaustiveness() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    when 2 is
                        _ if False -> 0
                        _ -> 42

                wrapper {}
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_enum() {
    assert_evals_to!(
        indoc!(
            r#"
                Fruit : [ Apple, Orange, Banana ]

                apple : Fruit
                apple = Apple

                when apple is
                    Apple -> 1
                    Banana -> 2
                    Orange -> 3
                "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn pattern_matching_unit() {
    assert_evals_to!(
        indoc!(
            r#"
                Unit : [ Unit ]

                f : Unit -> I64
                f = \Unit -> 42

                f Unit
                "#
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                Unit : [ Unit ]

                x : Unit
                x = Unit

                when x is
                    Unit -> 42
                "#
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                f : {} -> I64
                f = \{} -> 42

                f {}
                "#
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                when {} is
                    {} -> 42
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn one_element_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                x : [ Pair I64 ]
                x = Pair 2

                x
                "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn nested_tag_union() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe I64)
                x = Just (Just 41)

                main =
                    x
                "#
        ),
        ((41, 0), 0),
        ((i64, u8), u8)
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn unit_type() {
    assert_evals_to!(
        indoc!(
            r#"
                Unit : [ Unit ]

                v : Unit
                v = Unit

                v
                "#
        ),
        (),
        ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn join_point_if() {
    assert_evals_to!(
        indoc!(
            r#"
                x =
                    if True then 1 else 2

                x
                "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn join_point_when() {
    assert_evals_to!(
        indoc!(
            r#"
            wrapper = \{} ->
                x : [ Red, White, Blue ]
                x = Blue

                y =
                    when x is
                        Red -> 1
                        White -> 2
                        Blue -> 3.1

                y

            wrapper {}
            "#
        ),
        3.1,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn join_point_with_cond_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    y =
                        when 1 + 2 is
                            3 -> 3
                            1 -> 1
                            _ -> 0

                    y

                wrapper {}
            "#
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            y =
                if 1 + 2 > 0 then
                    3
                else
                    0

            y
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_single_tag_construction() {
    assert_evals_to!(indoc!("Three (1 == 1) 32"), (32i64, true), (i64, bool));

    assert_evals_to!(
        indoc!("Three (1 == 1) (if True then Red else if True then Green else Blue) 32"),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_single_tag_pattern_match() {
    assert_evals_to!(
        indoc!(
            r"#
                x = Three (1 == 1) 32

                when x is
                    Three bool int ->
                        { bool, int }
                #"
        ),
        (32i64, true),
        (i64, bool)
    );

    assert_evals_to!(
        indoc!(
            r"#
                x = Three (1 == 1) (if True then Red else if True then Green else Blue) 32

                when x is
                    Three bool color int ->
                        { bool, color, int }
                #"
        ),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_multi_tag_construction_two() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [ Three Bool I64, Empty ]
                x = Three (1 == 1) 32

                x

                #"
        ),
        ((32i64, true), 1),
        ((i64, bool), u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_multi_tag_construction_three() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [ Three Bool [ Red, Green, Blue ] I64, Empty ]
                x = Three (1 == 1) (if True then Red else if True then Green else Blue) 32

                x
                #"
        ),
        ((32i64, true, 2u8), 1),
        ((i64, bool, u8), u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_multi_tag_pattern_match() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [ Three Bool I64, Empty ]
                x = Three (1 == 1) 32

                when x is
                    Three bool int ->
                        { bool, int }

                    Empty ->
                        { bool: False, int: 0 }
                #"
        ),
        (32i64, true),
        (i64, bool)
    );

    assert_evals_to!(
        indoc!(
            r"#
                x : [ Three Bool [ Red, Green, Blue ] I64, Empty ]
                x = Three (1 == 1) (if True then Red else if True then Green else Blue) 32

                when x is
                    Three bool color int ->
                        { bool, color, int }
                    Empty ->
                        { bool: False, color: Red, int: 0 }
                #"
        ),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn phantom_polymorphic() {
    // see https://github.com/rtfeldman/roc/issues/786 and below
    assert_evals_to!(
        indoc!(
            r"#
                Point coordinate : [ Point coordinate I64 I64 ]

                World : [ @World ]

                zero : Point World
                zero = Point @World 0 0

                add : Point a -> Point a
                add = \(Point c x y) -> (Point c x y)

                add zero
                #"
        ),
        (0, 0),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn phantom_polymorphic_record() {
    // see https://github.com/rtfeldman/roc/issues/786
    // also seemed to hit an issue where we check whether `add`
    // has a Closure layout while the type is not fully specialized yet
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

                Point coordinate : { coordinate : coordinate, x : I64, y : I64 }

                zero : Point I64
                zero = { coordinate : 0, x : 0, y : 0 }

                add : Point a -> Point a
                add = \{ coordinate } -> { coordinate, x: 0 + 0, y: 0 }

                main = add zero
                "#
        ),
        (0, 0),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn result_never() {
    assert_evals_to!(
        indoc!(
            r"#
                res : Result I64 []
                res = Ok 4

                # we should provide this in the stdlib
                never : [] -> a

                when res is
                    Ok v -> v
                    Err empty -> never empty
                #"
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn nested_recursive_literal() {
    assert_evals_to!(
        indoc!(
            r"#
                Expr : [ Add Expr Expr, Val I64, Var I64 ]

                e : Expr
                e = Add (Add (Val 3) (Val 1)) (Add (Val 1) (Var 1))

                e
                #"
        ),
        0,
        usize,
        |_| 0
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn newtype_wrapper() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

                ConsList a : [ Nil, Cons a (ConsList a) ]

                foo : ConsList I64 -> ConsList I64
                foo = \t ->
                    when Delmin (Del t 0.0) is
                        Delmin (Del ry _) -> Cons 42 ry

                main = foo Nil
                "#
        ),
        42,
        &i64,
        |x: &i64| *x
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn applied_tag_function() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List [ Foo Str ]
            x = List.map [ "a", "b" ] Foo

            x
            "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice("a".as_bytes()),
            RocStr::from_slice("b".as_bytes())
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn applied_tag_function_result() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List (Result Str *)
            x = List.map [ "a", "b" ] Ok

            List.keepOks x (\y -> y)
            "#
        ),
        RocList::from_slice(&[
            (RocStr::from_slice("a".as_bytes())),
            (RocStr::from_slice("b".as_bytes()))
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn applied_tag_function_linked_list() {
    assert_evals_to!(
        indoc!(
            r#"
            ConsList a : [ Nil, Cons a (ConsList a) ]

            x : List (ConsList Str)
            x = List.map2 [ "a", "b" ] [ Nil, Cons "c" Nil ] Cons

            when List.first x is
                Ok (Cons "a" Nil) -> 1
                _ -> 0
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = "")]
fn tag_must_be_its_own_type() {
    assert_evals_to!(
        indoc!(
            r#"
            z : [ A, B, C ]
            z = Z

            z
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn recursive_tag_union_into_flat_tag_union() {
    // Comprehensive test for correctness in cli/tests/repl_eval
    assert_evals_to!(
        indoc!(
            r#"
            Item : [ Shallow [ L Str, R Str ], Deep Item ]
            i : Item
            i = Deep (Shallow (R "woo"))
            i
            "#
        ),
        0,
        usize,
        |_| 0
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn monomorphized_tag() {
    assert_evals_to!(
        indoc!(
            r#"
            b = False
            f : Bool, [True, False, Idk] -> U8
            f = \_, _ -> 18
            f b b
            "#
        ),
        18,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn monomorphized_applied_tag() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            main =
                a = A "abc"
                f = \x ->
                    when x is
                        A y -> y
                        B y -> y
                f a
            "#
        ),
        RocStr::from_slice(b"abc"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn monomorphized_tag_with_polymorphic_arg() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                a = A
                wrap = Wrapped a

                useWrap1 : [Wrapped [A], Other] -> U8
                useWrap1 =
                    \w -> when w is
                        Wrapped A -> 2
                        Other -> 3

                useWrap2 : [Wrapped [A, B]] -> U8
                useWrap2 =
                    \w -> when w is
                        Wrapped A -> 5
                        Wrapped B -> 7

                useWrap1 wrap * useWrap2 wrap
            "#
        ),
        10,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn monomorphized_tag_with_polymorphic_arg_and_monomorphic_arg() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                mono : U8
                mono = 15
                poly = A
                wrap = Wrapped poly mono

                useWrap1 : [Wrapped [A] U8, Other] -> U8
                useWrap1 =
                    \w -> when w is
                        Wrapped A n -> n
                        Other -> 0

                useWrap2 : [Wrapped [A, B] U8] -> U8
                useWrap2 =
                    \w -> when w is
                        Wrapped A n -> n
                        Wrapped B _ -> 0

                useWrap1 wrap * useWrap2 wrap
            "#
        ),
        225,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn issue_2365_monomorphize_tag_with_non_empty_ext_var() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Single a : [A, B, C]a
            Compound a : Single [D, E, F]a

            single : {} -> Single *
            single = \{} -> C

            compound : {} -> Compound *
            compound = \{} -> single {}

            main = compound {}
            "#
        ),
        2, // C
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn issue_2365_monomorphize_tag_with_non_empty_ext_var_wrapped() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Single a : [A, B, C]a
            Compound a : Single [D, E, F]a

            single : {} -> Result Str (Single *)
            single = \{} -> Err C

            compound : {} -> Result Str (Compound *)
            compound = \{} ->
                when single {} is
                    Ok s -> Ok s
                    Err e -> Err e

            main = compound {}
            "#
        ),
        2, // C
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn issue_2365_monomorphize_tag_with_non_empty_ext_var_wrapped_nested() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Single a : [A, B, C]a
            Compound a : Single [D, E, F]a

            main =
                single : {} -> Result Str (Single *)
                single = \{} -> Err C

                compound : {} -> Result Str (Compound *)
                compound = \{} ->
                    when single {} is
                        Ok s -> Ok s
                        Err e -> Err e

                compound {}
            "#
        ),
        2, // C
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn issue_2445() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            none : [ None, Update a ]
            none = None

            press : [ None, Update U8 ]
            press = none

            main =
                when press is
                    None -> 15
                    Update _ -> 25
            "#
        ),
        15,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn issue_2458() {
    assert_evals_to!(
        indoc!(
            r#"
            Foo a : [ Blah (Bar a), Nothing {} ]
            Bar a : Foo a

            v : Bar {}
            v = Blah (Blah (Nothing {}))

            when v is
                Blah (Blah (Nothing {})) -> 15
                _ -> 25
            "#
        ),
        15,
        u8
    )
}

#[test]
#[ignore = "See https://github.com/rtfeldman/roc/issues/2466"]
#[cfg(any(feature = "gen-llvm"))]
fn issue_2458_deep_recursion_var() {
    assert_evals_to!(
        indoc!(
            r#"
            Foo a : [ Blah (Result (Bar a) {}) ]
            Bar a : Foo a

            v : Bar {}

            when v is
                Blah (Ok (Blah (Err {}))) -> "1"
                _ -> "2"
            "#
        ),
        15,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn issue_1162() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            RBTree k : [ Node k (RBTree k) (RBTree k), Empty ]

            balance : a, RBTree a -> RBTree a
            balance = \key, left ->
                  when left is
                    Node _ _ lRight ->
                        Node key lRight Empty

                    _ ->
                        Empty


            tree : RBTree {}
            tree =
                balance {} Empty

            main : U8
            main =
                when tree is
                    Empty -> 15
                    _ -> 25
            "#
        ),
        15,
        u8
    )
}
