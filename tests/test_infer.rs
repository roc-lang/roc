#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer {
    use helpers::can_expr;
    use roc::infer::infer_expr;
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};

    // HELPERS

    fn infer_eq(src: &str, expected: &str) {
        let (_, output, _, procedures, mut subs, variable) = can_expr(src);

        let content = infer_expr(&mut subs, procedures, &output.constraint, variable);

        name_all_type_vars(variable, &mut subs);

        let actual_str = content_to_string(content, &mut subs);

        assert_eq!(actual_str, expected.to_string());
    }

    #[test]
    fn empty_record() {
        infer_eq("{}", "{}");
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
            "List (List <type mismatch>)",
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
                \_ _ -> 42
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
                \_ _ _ -> "test!"
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
                func = \_ _ -> 42

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
                f = \_ _ _ -> "test!"

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
                a = \_ _ _ -> "test!"

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

    // TODO type annotations
    // TODO BoundTypeVariables
    // TODO conditionals

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
                always = \a b -> a

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

    // #[test]
    // TODO FIXME this should work, but instead causes a stack overflow!
    // fn recursive_identity() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //                 identity = \val -> val

    //                 identity identity
    //             "#
    //         ),
    //         "a -> a",
    //     );
    // }

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
                    apply = \f x -> f x
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
                    \f x -> f x
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
                    \f -> (\a b -> f b a),
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

    // #[test]
    // TODO this should pass, but instead infers the wrong type
    // fn pass_a_function() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //                 \to_a -> to_a {}
    //             "#
    //         ),
    //         "({} -> a) -> a",
    //     );
    // }

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
    fn case_with_int_literals() {
        infer_eq(
            indoc!(
                r#"
                case 1 when
                 1 -> 2
                 3 -> 4
            "#
            ),
            "Int",
        );
    }
}
