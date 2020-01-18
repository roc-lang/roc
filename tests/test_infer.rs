#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer {
    use crate::helpers::{assert_correct_variable_usage, can_expr, with_larger_debug_stack};
    use roc::infer::infer_expr;
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};
    use roc::subs::Subs;

    // HELPERS

    fn infer_eq_help(src: &str) -> (Vec<roc::types::Problem>, String) {
        let (_expr, output, _, var_store, variable, constraint) = can_expr(src);
        let mut subs = Subs::new(var_store.into());

        assert_correct_variable_usage(&constraint);

        for (var, name) in output.rigids {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (content, solved) = infer_expr(subs, &mut unify_problems, &constraint, variable);
        let mut subs = solved.into_inner();

        name_all_type_vars(variable, &mut subs);

        let actual_str = content_to_string(content, &mut subs);

        (unify_problems, actual_str)
    }
    fn infer_eq(src: &str, expected: &str) {
        let (_, actual) = infer_eq_help(src);

        assert_eq!(actual, expected.to_string());
    }

    fn infer_eq_without_problem(src: &str, expected: &str) {
        let (problems, actual) = infer_eq_help(src);

        if !problems.is_empty() {
            // fail with an assert, but print the problems normally so rust doesn't try to diff
            // an empty vec with the problems.
            println!("expected:\n{:?}\ninfered:\n{:?}", expected, actual);
            assert_eq!(0, 1);
        }
        assert_eq!(actual, expected.to_string());
    }

    #[test]
    fn int_literal() {
        infer_eq("5", "Int");
    }

    #[test]
    fn float_literal() {
        infer_eq("0.5", "Float");
    }

    #[test]
    fn string_literal() {
        infer_eq(
            indoc!(
                r#"
                "type inference!"
            "#
            ),
            "Str",
        );
    }

    #[test]
    fn empty_string() {
        infer_eq(
            indoc!(
                r#"
                ""
            "#
            ),
            "Str",
        );
    }

    // #[test]
    // fn block_string_literal() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             """type
    //             inference!"""
    //         "#
    //         ),
    //         "Str",
    //     );
    // }

    // LIST

    #[test]
    fn empty_list() {
        infer_eq(
            indoc!(
                r#"
                []
            "#
            ),
            "List *",
        );
    }

    #[test]
    fn list_of_lists() {
        infer_eq(
            indoc!(
                r#"
                [[]]
            "#
            ),
            "List (List *)",
        );
    }

    #[test]
    fn triple_nested_list() {
        infer_eq(
            indoc!(
                r#"
                [[[]]]
            "#
            ),
            "List (List (List *))",
        );
    }

    #[test]
    fn nested_empty_list() {
        infer_eq(
            indoc!(
                r#"
                [ [], [ [] ] ]
            "#
            ),
            "List (List (List *))",
        );
    }

    // #[test]
    // fn concat_different_types() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             empty = []
    //             one = List.concat [ 1 ] empty
    //             str = List.concat [ "blah" ] empty

    //             empty
    //         "#
    //         ),
    //         "List *",
    //     );
    // }

    #[test]
    fn list_of_one_int() {
        infer_eq(
            indoc!(
                r#"
                [42]
            "#
            ),
            "List Int",
        );
    }

    #[test]
    fn triple_nested_int_list() {
        infer_eq(
            indoc!(
                r#"
                [[[ 5 ]]]
            "#
            ),
            "List (List (List Int))",
        );
    }

    #[test]
    fn list_of_ints() {
        infer_eq(
            indoc!(
                r#"
                [ 1, 2, 3 ]
            "#
            ),
            "List Int",
        );
    }

    #[test]
    fn nested_list_of_ints() {
        infer_eq(
            indoc!(
                r#"
                [ [ 1 ], [ 2, 3 ] ]
            "#
            ),
            "List (List Int)",
        );
    }

    #[test]
    fn list_of_one_string() {
        infer_eq(
            indoc!(
                r#"
                [ "cowabunga" ]
            "#
            ),
            "List Str",
        );
    }

    #[test]
    fn triple_nested_string_list() {
        infer_eq(
            indoc!(
                r#"
                [[[ "foo" ]]]
            "#
            ),
            "List (List (List Str))",
        );
    }

    #[test]
    fn list_of_strings() {
        infer_eq(
            indoc!(
                r#"
                [ "foo", "bar" ]
            "#
            ),
            "List Str",
        );
    }

    // // INTERPOLATED STRING

    // #[test]
    // fn infer_interpolated_string() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             whatItIs = "great"

    //             "type inference is \(whatItIs)!"
    //         "#
    //         ),
    //         "Str",
    //     );
    // }

    // LIST MISMATCH

    #[test]
    fn mismatch_heterogeneous_list() {
        infer_eq(
            indoc!(
                r#"
                [ "foo", 5 ]
            "#
            ),
            "List <type mismatch>",
        );
    }

    #[test]
    fn mismatch_heterogeneous_nested_list() {
        infer_eq(
            indoc!(
                r#"
                [ [ "foo", 5 ] ]
            "#
            ),
            "List (List <type mismatch>)",
        );
    }

    #[test]
    fn mismatch_heterogeneous_nested_empty_list() {
        infer_eq(
            indoc!(
                r#"
                [ [ 1 ], [ [] ] ]
            "#
            ),
            "List <type mismatch>",
        );
    }

    // CLOSURE

    #[test]
    fn always_return_empty_record() {
        infer_eq(
            indoc!(
                r#"
                \_ -> {}
            "#
            ),
            "* -> {}",
        );
    }

    #[test]
    fn two_arg_return_int() {
        infer_eq(
            indoc!(
                r#"
                \_, _ -> 42
            "#
            ),
            "*, * -> Int",
        );
    }

    #[test]
    fn three_arg_return_string() {
        infer_eq(
            indoc!(
                r#"
                \_, _, _ -> "test!"
            "#
            ),
            "*, *, * -> Str",
        );
    }

    // DEF

    #[test]
    fn def_empty_record() {
        infer_eq(
            indoc!(
                r#"
                foo = {}

                foo
            "#
            ),
            "{}",
        );
    }

    #[test]
    fn def_string() {
        infer_eq(
            indoc!(
                r#"
                str = "thing"

                str
            "#
            ),
            "Str",
        );
    }

    #[test]
    fn def_1_arg_closure() {
        infer_eq(
            indoc!(
                r#"
                fn = \_ -> {}

                fn
            "#
            ),
            "* -> {}",
        );
    }

    #[test]
    fn def_2_arg_closure() {
        infer_eq(
            indoc!(
                r#"
                func = \_, _ -> 42

                func
            "#
            ),
            "*, * -> Int",
        );
    }

    #[test]
    fn def_3_arg_closure() {
        infer_eq(
            indoc!(
                r#"
                f = \_, _, _ -> "test!"

                f
            "#
            ),
            "*, *, * -> Str",
        );
    }

    #[test]
    fn def_multiple_functions() {
        infer_eq(
            indoc!(
                r#"
                a = \_, _, _ -> "test!"

                b = a

                b
            "#
            ),
            "*, *, * -> Str",
        );
    }

    #[test]
    fn def_multiple_strings() {
        infer_eq(
            indoc!(
                r#"
                a = "test!"

                b = a

                b
            "#
            ),
            "Str",
        );
    }

    #[test]
    fn def_multiple_ints() {
        infer_eq(
            indoc!(
                r#"
                c = b

                b = a

                a = 42

                c
            "#
            ),
            "Int",
        );
    }

    #[test]
    fn def_returning_closure() {
        infer_eq(
            indoc!(
                r#"
                f = \z -> z
                g = \z -> z

                (\x ->
                    a = f x
                    b = g x
                    x
                )
            "#
            ),
            "a -> a",
        );
    }

    // CALLING FUNCTIONS

    #[test]
    fn call_returns_int() {
        infer_eq(
            indoc!(
                r#"
                alwaysFive = \_ -> 5

                alwaysFive "stuff"
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn identity_returns_given_type() {
        infer_eq(
            indoc!(
                r#"
                identity = \a -> a

                identity "hi"
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn identity_infers_principal_type() {
        infer_eq(
            indoc!(
                r#"
                identity = \x -> x

                y = identity 5

                identity
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn identity_works_on_incompatible_types() {
        infer_eq(
            indoc!(
                r#"
                identity = \a -> a

                x = identity 5
                y = identity "hi"

                x
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn call_returns_list() {
        infer_eq(
            indoc!(
                r#"
                enlist = \val -> [ val ]

                enlist 5
                "#
            ),
            "List Int",
        );
    }

    #[test]
    fn indirect_always() {
        infer_eq(
            indoc!(
                r#"
                    always = \val -> (\_ -> val)
                    alwaysFoo = always "foo"

                    alwaysFoo 42
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn pizza_desugar() {
        infer_eq(
            indoc!(
                r#"
                1 |> (\a -> a)
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn pizza_desugar_two_arguments() {
        infer_eq(
            indoc!(
                r#"
                always = \a, b -> a

                1 |> always "foo"
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn anonymous_identity() {
        infer_eq(
            indoc!(
                r#"
                    (\a -> a) 3.14
                "#
            ),
            "Float",
        );
    }

    #[test]
    fn identity_of_identity() {
        infer_eq(
            indoc!(
                r#"
                    (\val -> val) (\val -> val)
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn recursive_identity() {
        infer_eq(
            indoc!(
                r#"
                    identity = \val -> val

                    identity identity
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn identity_function() {
        infer_eq(
            indoc!(
                r#"
                    \val -> val
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn use_apply() {
        infer_eq(
            indoc!(
                r#"
                    apply = \f, x -> f x
                    identity = \a -> a

                    apply identity 5
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn apply_function() {
        infer_eq(
            indoc!(
                r#"
                    \f, x -> f x
                "#
            ),
            "(a -> b), a -> b",
        );
    }

    // #[test]
    // TODO FIXME this should pass, but instead fails to canonicalize
    // fn use_flip() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //                 flip = \f -> (\a b -> f b a)
    //                 neverendingInt = \f int -> f int
    //                 x = neverendingInt (\a -> a) 5

    //                 flip neverendingInt
    //             "#
    //         ),
    //         "(Int, (a -> a)) -> Int",
    //     );
    // }

    #[test]
    fn flip_function() {
        infer_eq(
            indoc!(
                r#"
                    \f -> (\a, b -> f b a),
                "#
            ),
            "(a, b -> c) -> (b, a -> c)",
        );
    }

    #[test]
    fn always_function() {
        infer_eq(
            indoc!(
                r#"
                    \val -> \_ -> val
                "#
            ),
            "a -> (* -> a)",
        );
    }

    #[test]
    fn pass_a_function() {
        infer_eq(
            indoc!(
                r#"
                    \f -> f {}
                "#
            ),
            "({} -> a) -> a",
        );
    }

    // OPERATORS

    // #[test]
    // fn div_operator() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             \l r -> l / r
    //         "#
    //         ),
    //         "Float, Float -> Float",
    //     );
    // }

    //     #[test]
    //     fn basic_float_division() {
    //         infer_eq(
    //             indoc!(
    //                 r#"
    //                 1 / 2
    //             "#
    //             ),
    //             "Float",
    //         );
    //     }

    //     #[test]
    //     fn basic_int_division() {
    //         infer_eq(
    //             indoc!(
    //                 r#"
    //                 1 // 2
    //             "#
    //             ),
    //             "Int",
    //         );
    //     }

    //     #[test]
    //     fn basic_addition() {
    //         infer_eq(
    //             indoc!(
    //                 r#"
    //                 1 + 2
    //             "#
    //             ),
    //             "Int",
    //         );
    //     }

    // #[test]
    // fn basic_circular_type() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             \x -> x x
    //         "#
    //         ),
    //         "<Type Mismatch: Circular Type>",
    //     );
    // }

    // #[test]
    // fn y_combinator_has_circular_type() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \f -> (\x -> f x x) (\x -> f x x)
    //         "#)),
    //         Erroneous(Problem::CircularType)
    //     );
    // }

    // #[test]
    // fn no_higher_ranked_types() {
    //     // This should error because it can't type of alwaysFive
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             \always -> [ always [], always "" ]
    //        "#
    //         ),
    //         "<type mismatch>",
    //     );
    // }

    #[test]
    fn always_with_list() {
        infer_eq(
            indoc!(
                r#"
               alwaysFive = \_ -> 5

               [ alwaysFive "foo", alwaysFive [] ]
           "#
            ),
            "List Int",
        );
    }

    #[test]
    fn if_with_int_literals() {
        infer_eq(
            indoc!(
                r#"
                if True then
                    42
                else
                    24
            "#
            ),
            "Int",
        );
    }

    #[test]
    fn when_with_int_literals() {
        infer_eq(
            indoc!(
                r#"
                when 1 is
                 1 -> 2
                 3 -> 4
            "#
            ),
            "Int",
        );
    }

    // RECORDS

    #[test]
    fn empty_record() {
        infer_eq("{}", "{}");
    }

    #[test]
    fn one_field_record() {
        infer_eq("{ x: 5 }", "{ x : Int }");
    }

    #[test]
    fn two_field_record() {
        infer_eq("{ x: 5, y : 3.14 }", "{ x : Int, y : Float }");
    }

    #[test]
    fn record_literal_accessor() {
        infer_eq("{ x: 5, y : 3.14 }.x", "Int");
    }

    #[test]
    fn record_arg() {
        infer_eq("\\rec -> rec.x", "{ x : a }* -> a");
    }

    #[test]
    fn record_with_bound_var() {
        with_larger_debug_stack(|| {
            infer_eq(
                indoc!(
                    r#"
                fn = \rec ->
                    x = rec.x

                    rec

                fn
            "#
                ),
                "{ x : a }b -> { x : a }b",
            );
        });
    }

    #[test]
    fn using_type_signature() {
        infer_eq(
            indoc!(
                r#"
            bar : custom -> custom
            bar = \x -> x

            bar
            "#
            ),
            "custom -> custom",
        );
    }

    #[test]
    fn type_signature_without_body() {
        infer_eq(
            indoc!(
                r#"
            foo: Int -> Bool

            foo 2
            "#
            ),
            "Bool",
        );
    }

    #[test]
    fn type_signature_without_body_rigid() {
        infer_eq(
            indoc!(
                r#"
            foo : Int -> custom

            foo 2
            "#
            ),
            "custom",
        );
    }

    #[test]
    fn accessor_function() {
        infer_eq(".foo", "{ foo : a }* -> a");
    }

    #[test]
    fn type_signature_without_body_record() {
        infer_eq(
            indoc!(
                r#"
            { x, y } : { x : (Int -> custom) , y : Int }

            x
            "#
            ),
            "Int -> custom",
        );
    }

    #[test]
    fn record_pattern_match_infer() {
        infer_eq(
            indoc!(
                r#"
                when foo is
                    { x: 4 }-> x
            "#
            ),
            "Int",
        );
    }

    #[test]
    fn empty_record_pattern() {
        infer_eq(
            indoc!(
                r#"
                # technically, an empty record can be destructured
                {} = {}
                bar = \{} -> 42

                when foo is
                    { x: {} } -> x
            "#
            ),
            "{}*",
        );
    }

    #[test]
    fn record_type_annotation() {
        // check that a closed record remains closed
        infer_eq(
            indoc!(
                r#"
                foo : { x : custom } -> custom
                foo = \{ x } -> x

                foo
            "#
            ),
            "{ x : custom } -> custom",
        );
    }

    #[test]
    fn record_update() {
        infer_eq(
            indoc!(
                r#"
                user = { year: "foo", name: "Sam" }

                { user & year: "foo" }
                "#
            ),
            "{ name : Str, year : Str }",
        );
    }

    #[test]
    fn bare_tag() {
        infer_eq(
            indoc!(
                r#"Foo
                "#
            ),
            "[ Foo ]*",
        );
    }

    #[test]
    fn single_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\Foo -> 42
                "#
            ),
            "[ Foo ]* -> Int",
        );
    }

    #[test]
    fn single_private_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\@Foo -> 42
                "#
            ),
            "[ Test.@Foo ]* -> Int",
        );
    }

    #[test]
    fn two_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\x ->
                    when x is
                        True -> 1
                        False -> 0
                "#
            ),
            "[ False, True ]* -> Int",
        );
    }

    #[test]
    fn tag_application() {
        infer_eq(
            indoc!(
                r#"Foo "happy" 2020
                "#
            ),
            "[ Foo Str Int ]*",
        );
    }

    #[test]
    fn private_tag_application() {
        infer_eq(
            indoc!(
                r#"@Foo "happy" 2020
                "#
            ),
            "[ Test.@Foo Str Int ]*",
        );
    }

    #[test]
    fn record_extraction() {
        with_larger_debug_stack(|| {
            infer_eq(
                indoc!(
                    r#"
                f = \x ->
                    when x is
                        { a, b } -> a

                f
                "#
                ),
                "{ a : a, b : * }* -> a",
            );
        });
    }

    #[test]
    fn record_field_pattern_match_with_guard() {
        infer_eq(
            indoc!(
                r#"
                    when foo is
                        { x: 4 } -> x
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn tag_union_pattern_match() {
        infer_eq(
            indoc!(
                r#"
                \Foo x -> Foo x
                "#
            ),
            "[ Foo a ]* -> [ Foo a ]*",
        );
    }

    #[test]
    fn tag_union_pattern_match_ignored_field() {
        infer_eq(
            indoc!(
                r#"
                \Foo x _ -> Foo x "y"
                "#
            ),
            "[ Foo a * ]* -> [ Foo a Str ]*",
        );
    }

    #[test]
    fn global_tag_with_field() {
        infer_eq(
            indoc!(
                r#"
                    when Foo 4 is
                        Foo x -> x
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn private_tag_with_field() {
        infer_eq(
            indoc!(
                r#"
                    when @Foo 4 is
                        @Foo x -> x
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn annotation_using_num() {
        infer_eq_without_problem(
            indoc!(
                r#"
                   int : Num.Num Int.Integer

                   int
                   "#
            ),
            "Int",
        );
    }

    #[test]
    fn annotation_using_num_used() {
        // There was a problem where `int`, because it is only an annotation
        // wasn't added to the vars_by_symbol.
        infer_eq_without_problem(
            indoc!(
                r#"
                   int : Num.Num Int.Integer

                   p = (\x -> x) int

                   p
                   "#
            ),
            "Int",
        );
    }

    #[test]
    fn num_identity() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    numIdentity : Num.Num a -> Num.Num a
                    numIdentity = \x -> x

                    y = numIdentity 3.14

                    { numIdentity, x : numIdentity 42, y }
                    "#
            ),
            "{ numIdentity : Num a -> Num a, x : Int, y : Float }",
        );
    }

    #[test]
    fn when_with_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    x : Num.Num Int.Integer
                    x =
                        when 2 is
                            3 -> 4
                            _ -> 5

                    x
                   "#
            ),
            "Int",
        );
    }

    // TODO add more realistic function when able
    #[test]
    fn integer_sum() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f = \n ->
                    when n is
                        0 -> 0
                        _ -> f n

                f
                   "#
            ),
            "Int -> Int",
        );
    }

    #[test]
    fn identity_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                map : (a -> b), [ Identity a ] -> [ Identity b ]
                map = \f, identity ->
                    when identity is
                        Identity v -> Identity (f v)
                map
                   "#
            ),
            "(a -> b), [ Identity a ] -> [ Identity b ]",
        );
    }

    #[test]
    fn to_bit() {
        infer_eq_without_problem(
            indoc!(
                r#"
                   # toBit : [ False, True ] -> Num.Num Int.Integer
                   toBit = \bool ->
                       when bool is
                           True -> 1
                           False -> 0

                   toBit
                      "#
            ),
            "[ False, True ]* -> Int",
        );
    }

    // this test is related to a bug where ext_var would have an incorrect rank.
    // This match has duplicate cases, but that's not important because exhaustiveness happens
    // after inference.
    #[test]
    fn to_bit_record() {
        infer_eq_without_problem(
            indoc!(
                r#"
                foo = \rec ->
                        when rec is
                            { x } -> "1"
                            { y } -> "2"

                foo
                      "#
            ),
            "{ x : *, y : * }* -> Str",
        );
    }

    #[test]
    fn from_bit() {
        infer_eq_without_problem(
            indoc!(
                r#"
                   fromBit = \int ->
                       when int is
                           0 -> False
                           _ -> True

                   fromBit
                      "#
            ),
            "Int -> [ False, True ]*",
        );
    }

    #[test]
    fn result_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map : (a -> b), [ Err e, Ok a ] -> [ Err e, Ok b ]
                    map = \f, result ->
                        when result is
                            Ok v -> Ok (f v)
                            Err e -> Err e

                    map
                       "#
            ),
            "(a -> b), [ Err e, Ok a ] -> [ Err e, Ok b ]",
        );
    }

    #[test]
    fn record_from_load() {
        infer_eq_without_problem(
            indoc!(
                r#"
                foo = \{ x } -> x

                foo { x: 5 }
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn defs_from_load() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    alwaysThreePointZero = \_ -> 3.0

                    answer = 42

                    identity = \a -> a

                    threePointZero = identity (alwaysThreePointZero {})

                    threePointZero
                "#
            ),
            "Float",
        );
    }

    // // currently fails, the rank of Cons's ext_var is 3, where 2 is the highest pool
    // #[test]
    // fn linked_list_map() {
    //     with_larger_debug_stack(|| {
    //         infer_eq_without_problem(
    //             indoc!(
    //                 r#"
    //                 map = \f, list ->
    //                     when list is
    //                         Nil -> Nil
    //                         Cons x xs ->
    //                             a = f x
    //                             b = map f xs
    //
    //                             Cons a b
    //
    //                 map
    //                    "#
    //             ),
    //             "Attr.Attr * Int",
    //         );
    //     });
    // }
}
