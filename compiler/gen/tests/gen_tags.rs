#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate libc;
extern crate roc_gen;

#[macro_use]
mod helpers;

#[cfg(test)]
mod gen_tags {
    #[test]
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
            (i64, i64),
            |(tag, _)| tag
        );
    }

    #[test]
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
            (i64, i64),
            |(tag, _)| tag
        );
    }

    #[test]
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
            (0, 0x4),
            (i64, i64)
        );
    }

    #[test]
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
            (0, 0x4),
            (i64, i64)
        );
    }

    #[test]
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
            (0, 2),
            (i64, u8)
        );
    }

    // #[test]
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
    fn pair_with_guard_pattern() {
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
    fn result_with_guard_pattern() {
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
    fn maybe_is_just() {
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
            (0, (0, 41)),
            (i64, (i64, i64))
        );
    }
    #[test]
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
    fn nested_record_load() {
        assert_evals_to!(
            indoc!(
                r#"
                x = { a : { b : 0x5 } }

                y = x.a

                y.b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
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
    fn alignment_in_single_tag_construction() {
        assert_evals_to!(indoc!("Three (1 == 1) 32"), (32i64, true), (i64, bool));

        assert_evals_to!(
            indoc!("Three (1 == 1) (if True then Red else if True then Green else Blue) 32"),
            (32i64, true, 2u8),
            (i64, bool, u8)
        );
    }

    #[test]
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
    fn alignment_in_multi_tag_construction() {
        assert_evals_to!(
            indoc!(
                r"#
                x : [ Three Bool I64, Empty ]
                x = Three (1 == 1) 32

                x

                #"
            ),
            (1, 32i64, true),
            (i64, i64, bool)
        );

        assert_evals_to!(
            indoc!(
                r"#
                x : [ Three Bool [ Red, Green, Blue ] I64, Empty ]
                x = Three (1 == 1) (if True then Red else if True then Green else Blue) 32

                x
                #"
            ),
            (1, 32i64, true, 2u8),
            (i64, i64, bool, u8)
        );
    }

    #[test]
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
    fn phantom_polymorphic() {
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
    fn phantom_polymorphic_record() {
        assert_evals_to!(
            indoc!(
                r"#
                Point coordinate : { coordinate : coordinate, x : I64, y : I64 }

                zero : Point {}
                zero = { coordinate : {}, x : 0, y : 0 }

                add : Point a -> Point a
                add = \{ coordinate } -> { coordinate, x: 0, y: 0 }

                add zero
                #"
            ),
            (0, 0),
            (i64, i64)
        );
    }
}
