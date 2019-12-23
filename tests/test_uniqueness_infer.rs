#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer_uniq {
    use crate::helpers::uniq_expr;
    use roc::infer::infer_expr;
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};

    // HELPERS

    fn infer_eq(src: &str, expected: &str) {
        let (
            _output2,
            _output1,
            _,
            mut subs1,
            variable1,
            mut subs2,
            variable2,
            constraint1,
            constraint2,
        ) = uniq_expr(src);

        let mut unify_problems = Vec::new();
        let content1 = infer_expr(&mut subs1, &mut unify_problems, &constraint1, variable1);
        let content2 = infer_expr(&mut subs2, &mut unify_problems, &constraint2, variable2);

        name_all_type_vars(variable1, &mut subs1);
        name_all_type_vars(variable2, &mut subs2);

        let _actual_str = content_to_string(content1, &mut subs1);
        let uniq_actual_str = content_to_string(content2, &mut subs2);

        // assert_eq!(actual_str, expected.to_string());
        assert_eq!(expected.to_string(), uniq_actual_str);
    }

    #[test]
    fn empty_record() {
        infer_eq("{}", "Attr.Attr * {}");
    }

    #[test]
    fn int_literal() {
        infer_eq("5", "Attr.Attr * Int");
    }

    #[test]
    fn float_literal() {
        infer_eq("0.5", "Attr.Attr * Float");
    }

    #[test]
    fn string_literal() {
        infer_eq(
            indoc!(
                r#"
                "type inference!"
            "#
            ),
            "Attr.Attr * Str",
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
            "Attr.Attr * Str",
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
            "Attr.Attr * (List *)",
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
            "Attr.Attr * (List (Attr.Attr * (List *)))",
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
            "Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * (List *)))))",
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
            "Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * (List *)))))",
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
            "Attr.Attr * (List (Attr.Attr * Int))",
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
            "Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * Int))))))",
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
            "Attr.Attr * (List (Attr.Attr * Int))",
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
            "Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * Int))))",
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
            "Attr.Attr * (List (Attr.Attr * Str))",
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
            "Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * (List (Attr.Attr * Str))))))",
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
            "Attr.Attr * (List (Attr.Attr * Str))",
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
            "Attr.Attr * (List <type mismatch>)",
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
            "Attr.Attr * (List (Attr.Attr * (List <type mismatch>)))",
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
            "Attr.Attr * (List <type mismatch>)",
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
            "Attr.Attr * (* -> Attr.Attr * {})",
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
            "Attr.Attr * (*, * -> Attr.Attr * Int)",
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
            "Attr.Attr * (*, *, * -> Attr.Attr * Str)",
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
            "Attr.Attr * {}",
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
            "Attr.Attr * Str",
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
            "Attr.Attr * (* -> Attr.Attr * {})",
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
            "Attr.Attr * (*, * -> Attr.Attr * Int)",
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
            "Attr.Attr * (*, *, * -> Attr.Attr * Str)",
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
            "Attr.Attr * (*, *, * -> Attr.Attr * Str)",
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
            "Attr.Attr * Str",
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
            "Attr.Attr * Int",
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
            // x is used 3 times, so must be shared
            "Attr.Attr * (Attr.Attr Attr.Shared a -> Attr.Attr Attr.Shared a)",
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
            "Attr.Attr * Int",
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
            "Attr.Attr * Str",
        );
    }

    #[test]
    fn identity_infers_principal_type() {
        infer_eq(
            indoc!(
                r#"
                identity = \a -> a

                x = identity 5

                identity
                "#
            ),
            "Attr.Attr * (a -> a)",
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
            "Attr.Attr Int",
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
            "Attr.Attr * (List (Attr.Attr * Int))",
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
            "Attr.Attr * Str",
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
            "Attr.Attr * Int",
        );
    }

    #[test]
    fn pizza_desugared() {
        infer_eq(
            indoc!(
                r#"
                (\a -> a) 1
                "#
            ),
            "Attr.Attr * Int",
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
            "Attr.Attr * Int",
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
            "Attr.Attr * Float",
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
            "Attr.Attr * (a -> a)",
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
            "Attr.Attr * (a -> a)",
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
            "Attr.Attr * Int",
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
            "Attr.Attr * (Attr.Attr * (a -> b), a -> b)",
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
            "Attr.Attr * (Attr.Attr * (a, b -> c) -> Attr.Attr * (b, a -> c))",
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
            "Attr.Attr * (a -> Attr.Attr * (* -> a))",
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
            "Attr.Attr * (Attr.Attr * (Attr.Attr * {} -> a) -> a)",
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
            "Attr.Attr * (List (Attr.Attr * Int))",
        );
    }

    // #[test]
    // fn if_with_int_literals() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             if 1 == 1 then
    //                 42
    //             else
    //                 24
    //         "#
    //         ),
    //         "Int",
    //     );
    // }

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
            "Attr.Attr * Int",
        );
    }

    #[test]
    fn accessor_function() {
        infer_eq(".foo", "Attr.Attr * { foo : a }* -> a");
    }

    #[test]
    fn record() {
        infer_eq("{ foo: 42 }", "Attr.Attr * { foo : (Attr.Attr * Int) }");
    }

    #[test]
    fn record_access() {
        infer_eq("{ foo: 42 }.foo", "Attr.Attr * Int");
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
            "Attr.Attr * {}*",
        );
    }
}
