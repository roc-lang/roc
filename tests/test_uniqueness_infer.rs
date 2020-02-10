#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer_uniq {
    use crate::helpers::{assert_correct_variable_usage, uniq_expr};
    use roc::infer::infer_expr;
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};

    // HELPERS

    fn infer_eq_help(src: &str) -> (Vec<roc::types::Problem>, String) {
        let (_output, _problems, subs, variable, constraint, home, interns) = uniq_expr(src);

        assert_correct_variable_usage(&constraint);

        let mut unify_problems = Vec::new();
        let (content, solved) = infer_expr(subs, &mut unify_problems, &constraint, variable);
        let mut subs = solved.into_inner();

        name_all_type_vars(variable, &mut subs);

        let actual_str = content_to_string(content, &mut subs, home, &interns);

        (unify_problems, actual_str)
    }
    fn infer_eq_ignore_problems(src: &str, expected: &str) {
        let (_, actual) = infer_eq_help(src);

        assert_eq!(actual, expected.to_string());
    }

    fn infer_eq(src: &str, expected: &str) {
        let (problems, actual) = infer_eq_help(src);

        if !problems.is_empty() {
            panic!("expected:\n{:?}\ninferred:\n{:?}", expected, actual);
        }
        assert_eq!(actual, expected.to_string());
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
    fn empty_list_literal() {
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
        infer_eq_ignore_problems(
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
        infer_eq_ignore_problems(
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
        infer_eq_ignore_problems(
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

    // #[test]
    // fn def_returning_closure() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             f = \z -> z
    //             g = \z -> z
    //
    //             (\x ->
    //                 a = f x
    //                 b = g x
    //                 x
    //             )
    //         "#
    //         ),
    //         // x is used 3 times, so must be shared
    //         "Attr.Attr * (Attr.Attr Attr.Shared a -> Attr.Attr Attr.Shared a)",
    //     );
    // }

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
            "Attr.Attr Attr.Shared (a -> a)",
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
            // TODO investigate why is this not shared?
            // maybe because y is not used it is dropped?
            "Attr.Attr * Int",
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

    // TODO when symbols are unique, this should work again
    //    #[test]
    //    fn identity_of_identity() {
    //        infer_eq(
    //            indoc!(
    //                r#"
    //                    (\val -> val) (\val -> val)
    //                "#
    //            ),
    //            "Attr.Attr * (a -> a)",
    //        );
    //    }

    #[test]
    fn recursive_identity() {
        infer_eq(
            indoc!(
                r#"
                    identity = \val -> val

                    identity identity
                "#
            ),
            "Attr.Attr Attr.Shared (a -> a)",
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
            "Attr.Attr * Int",
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
            "Attr.Attr * Int",
        );
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

    #[test]
    fn record_update() {
        infer_eq(
            indoc!(
                r#"
                user = { year: "foo", name: "Sam" }

                { user & year: "foo" }
                "#
            ),
            "Attr.Attr * { name : (Attr.Attr * Str), year : (Attr.Attr * Str) }",
        );
    }

    #[test]
    fn bare_tag() {
        infer_eq(
            indoc!(
                r#"Foo
                "#
            ),
            "Attr.Attr * [ Foo ]*",
        );
    }

    #[test]
    fn single_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\Foo -> 42
                "#
            ),
            "Attr.Attr * (Attr.Attr * [ Foo ]* -> Attr.Attr * Int)",
        );
    }

    #[test]
    fn single_private_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\@Foo -> 42
                "#
            ),
            "Attr.Attr * (Attr.Attr * [ @Foo ]* -> Attr.Attr * Int)",
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
            "Attr.Attr * (Attr.Attr * [ False, True ]* -> Attr.Attr * Int)",
        );
    }

    #[test]
    fn tag_application() {
        infer_eq(
            indoc!(
                r#"Foo "happy" 2020
                "#
            ),
            "Attr.Attr * [ Foo (Attr.Attr * Str) (Attr.Attr * Int) ]*",
        );
    }

    #[test]
    fn private_tag_application() {
        infer_eq(
            indoc!(
                r#"@Foo "happy" 2020
                "#
            ),
            "Attr.Attr * [ @Foo (Attr.Attr * Str) (Attr.Attr * Int) ]*",
        );
    }

    #[test]
    fn record_field_accessor_function() {
        infer_eq(
            indoc!(
                r#"
                .left
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a) { left : (Attr.Attr a b) }* -> Attr.Attr a b)",
        );
    }

    #[test]
    fn record_field_access_syntax() {
        infer_eq(
            indoc!(
                r#"
                \rec -> rec.left
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a) { left : (Attr.Attr a b) }* -> Attr.Attr a b)",
        );
    }

    #[test]
    fn record_field_pattern_match_two() {
        infer_eq(
            indoc!(
                r#"
                \{ left, right } -> { left, right }
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a | b) { left : (Attr.Attr a c), right : (Attr.Attr b d) }* -> Attr.Attr * { left : (Attr.Attr a c), right : (Attr.Attr b d) })",
        );
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
            "Attr.Attr * Int",
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
            // NOTE: Foo loses the relation to the uniqueness attribute `a`
            // That is fine. Whenever we try to extract from it, the relation will be enforced
            "Attr.Attr * (Attr.Attr (* | a) [ Foo (Attr.Attr a b) ]* -> Attr.Attr * [ Foo (Attr.Attr a b) ]*)",
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
            // TODO: is it safe to ignore uniqueness constraints from patterns that bind no identifiers?
            // i.e. the `b` could be ignored in this example, is that true in general?
            // seems like it because we don't really extract anything.
            "Attr.Attr * (Attr.Attr (* | a | b) [ Foo (Attr.Attr b c) (Attr.Attr a *) ]* -> Attr.Attr * [ Foo (Attr.Attr b c) (Attr.Attr * Str) ]*)",
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
            "Attr.Attr * Int",
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
            "Attr.Attr * Int",
        );
    }

    #[test]
    fn type_annotation() {
        infer_eq(
            indoc!(
                r#"
                x : Num.Num Int.Integer
                x = 4

                x
                "#
            ),
            "Attr.Attr * Int",
        );
    }

    #[test]
    fn record_field_pattern_match() {
        infer_eq(
            indoc!(
                r#"
                \{ left } -> left
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a) { left : (Attr.Attr a b) }* -> Attr.Attr a b)",
        );
    }

    #[test]
    fn sharing_analysis_record_one_field_pattern() {
        infer_eq(
            indoc!(
                r#"
                \{ left } -> left
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a) { left : (Attr.Attr a b) }* -> Attr.Attr a b)",
        );
    }

    #[test]
    fn num_identity_def() {
        infer_eq(
            indoc!(
                r#"
                   numIdentity : Num.Num a -> Num.Num a
                   numIdentity = \x -> x

                   numIdentity
                   "#
            ),
            "Attr.Attr * (Attr.Attr a (Num b) -> Attr.Attr a (Num b))",
        );
    }

    #[test]
    fn record_field_access_binding() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    x = r.left

                    x
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a) { left : (Attr.Attr a b) }* -> Attr.Attr a b)",
        );
    }

    #[test]
    fn sharing_analysis_record_one_field_access() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    x = r.left

                    x
                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a) { left : (Attr.Attr a b) }* -> Attr.Attr a b)",
        );
    }

    #[test]
    fn num_identity_applied() {
        infer_eq(
            indoc!(
                r#"
                   numIdentity : Num.Num b -> Num.Num b
                   numIdentity = \foo -> foo

                   p = numIdentity 42
                   q = numIdentity 3.14

                   { numIdentity, p, q }
                   "#
            ), "Attr.Attr * { numIdentity : (Attr.Attr Attr.Shared (Attr.Attr a (Num b) -> Attr.Attr a (Num b))), p : (Attr.Attr * Int), q : (Attr.Attr * Float) }"
        );
    }

    #[test]
    fn sharing_analysis_record_twice_access() {
        infer_eq(
                    indoc!(
                        r#"
                        \r ->
                            v = r.x
                            w = r.x

                            r

                        "#
                    ),
                "Attr.Attr * (Attr.Attr a { x : (Attr.Attr Attr.Shared b) }c -> Attr.Attr a { x : (Attr.Attr Attr.Shared b) }c)" ,
                );
    }

    #[test]
    fn sharing_analysis_record_access_two_fields() {
        infer_eq(
                    indoc!(
                        r#"
                        \r ->
                            v = r.x
                            w = r.y

                            r

                        "#
                    ),
                "Attr.Attr * (Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared c) }d -> Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared c) }d)",
                );
    }

    #[test]
    fn sharing_analysis_record_alias() {
        infer_eq(
                    indoc!(
                        r#"
                        \r ->
                            v = r.x
                            w = r.y

                            p = r

                            p
                        "#
                    ),
                "Attr.Attr * (Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared c) }d -> Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared c) }d)"
                );
    }

    #[test]
    fn sharing_analysis_record_access_field_twice() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    n = r.x
                    m = r.x

                    r
                        "#
            ),
            "Attr.Attr * (Attr.Attr a { x : (Attr.Attr Attr.Shared b) }c -> Attr.Attr a { x : (Attr.Attr Attr.Shared b) }c)",
        );
    }

    #[test]
    fn sharing_analysis_record_update_duplicate_field() {
        infer_eq(
            indoc!(
                r#"
                \r -> { r & x: r.x, y: r.x }
                "#
            ),
         "Attr.Attr * (Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared b) }c -> Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared b) }c)"
        );
    }

    #[test]
    fn record_access_nested_field() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    v = r.foo.bar
                    w = r.foo.baz

                    r
                "#
            ),
            "Attr.Attr * (Attr.Attr (a | b) { foo : (Attr.Attr a { bar : (Attr.Attr Attr.Shared d), baz : (Attr.Attr Attr.Shared c) }e) }f -> Attr.Attr (a | b) { foo : (Attr.Attr a { bar : (Attr.Attr Attr.Shared d), baz : (Attr.Attr Attr.Shared c) }e) }f)"
        );
    }

    #[test]
    fn record_access_nested_field_is_safe() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    v = r.foo.bar

                    x = v
                    y = v

                    r
                "#
            ),
            "Attr.Attr * (Attr.Attr (a | b) { foo : (Attr.Attr a { bar : (Attr.Attr Attr.Shared c) }d) }e -> Attr.Attr (a | b) { foo : (Attr.Attr a { bar : (Attr.Attr Attr.Shared c) }d) }e)"
        );
    }

    #[test]
    fn record_update_is_safe() {
        infer_eq(
            indoc!(
                r#"
                \r ->

                    s = { r & y: r.x }

                    p = s.x
                    q = s.y

                    s
                "#
            ),
            "Attr.Attr * (Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared b) }c -> Attr.Attr a { x : (Attr.Attr Attr.Shared b), y : (Attr.Attr Attr.Shared b) }c)",
        );
    }

    #[test]
    fn triple_nested_record() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    if True then
                        r.foo.bar.baz
                    else
                        r.tic.tac.toe

                "#
            ),
            "Attr.Attr * (Attr.Attr (* | a | b | c | d | e) { foo : (Attr.Attr (a | c | d) { bar : (Attr.Attr (c | d) { baz : (Attr.Attr d f) }*) }*), tic : (Attr.Attr (b | d | e) { tac : (Attr.Attr (b | d) { toe : (Attr.Attr d f) }*) }*) }* -> Attr.Attr d f)"
        );
    }

    #[test]
    fn when_with_annotation() {
        infer_eq(
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
            "Attr.Attr * Int",
        );
    }

    // TODO add more realistic recursive example when able
    #[test]
    fn factorial_is_shared() {
        infer_eq(
            indoc!(
                r#"
                    factorial = \n ->
                        when n is
                            0 -> 1
                            1 -> 1
                            m -> factorial m

                    factorial
                   "#
            ),
            "Attr.Attr Attr.Shared (Attr.Attr * Int -> Attr.Attr * Int)",
        );
    }

    #[test]
    #[ignore]
    fn quicksort_swap() {
        infer_eq(
            indoc!(
                r#"
                swap : Num.Num Int.Integer, Num.Num Int.Integer, List.List a -> List.List a
                swap \i, j, list ->
                    when Pair (List.get i list) (List.get j list) is
                        Pair (Ok atI) (Ok atJ) ->
                            list
                                |> List.set i atJ
                                |> List.set j atI
                        _ ->
                            list

                swap
                   "#
            ),
            "Attr.Attr * (Attr.Attr Attr.Shared Int, Attr.Attr Attr.Shared Int, Attr.Attr a (List b) -> Attr.Attr a (List b))",
        );
    }

    #[test]
    #[ignore]
    fn quicksort() {
        infer_eq(
            indoc!(
                r#"
                swap : Int, Int, List a -> List a
                swap \i, j, list ->
                    when Pair (List.get i list) (List.get j list) is
                        Pair (Ok atI) (Ok atJ) ->
                            list
                                |> List.set i atJ
                                |> List.set j atI
                        _ ->
                            list

                partition : Int, Int, List Int -> [ Pair Int (List Int) ]
                partition = \low, high, initialList ->
                    when List.get high initialList is
                        Ok pivot ->

                            go \i, j, list =
                                if j < high then
                                    when List.get j list is
                                        Ok value ->
                                            if value <= pivot then
                                                go (i + 1) (j + 1) (swap (i + 1) j list)
                                            else
                                                go i (j + 1) list

                                        _ ->
                                            Pair i list
                                else
                                    Pair i list

                            Pair newI newList = go (low - 1) low initialList

                            Pair (newI + 1) (swap (newI + 1) high newList)

                        Err _ ->
                            Pair (low - 1) initialList

                quicksort : List Int, Int, Int -> List Int
                quicksort = \list, low, high ->
                    Pair partitionIndex partitioned = partition low high list

                    arr
                        |> quicksort low (partitionIndex - 1)
                        |> quicksort (partitionIndex + 1) high

                quicksort
                   "#
            ),
            "Attr.Attr * (Attr.Attr Attr.Shared Int, Attr.Attr Attr.Shared Int, Attr.Attr a (List b) -> Attr.Attr a (List b))",
        );
    }

    #[test]
    fn shared_branch_unique_branch_access() {
        infer_eq(
            indoc!(
                r#"
                    r = { left: 20 }
                    s = { left: 20 }

                    if True then
                        r.left
                    else
                        v = s.left
                        s.left

                    "#
            ),
            "Attr.Attr Attr.Shared Int",
        );
    }

    #[test]
    fn shared_branch_unique_branch_nested_access() {
        infer_eq(
            indoc!(
                r#"
                    r = { left: 20 }
                    s = { left: 20 }

                    if True then
                        { y: r.left }
                    else
                        v = s.left
                        { y: s.left }

                    "#
            ),
            "Attr.Attr * { y : (Attr.Attr Attr.Shared Int) }",
        );
    }

    #[test]
    fn shared_branch_unique_branch_current() {
        infer_eq(
            indoc!(
                r#"
                       r = "foo"
                       s = { left : "foo" }

                       when 0 is
                           1 -> { x: s.left, y: s.left }
                           0 -> { x: s.left, y: r }
                           )
                   "#
            ),
            "Attr.Attr * { x : (Attr.Attr Attr.Shared Str), y : (Attr.Attr Attr.Shared Str) }",
        );
    }

    #[test]
    fn shared_branch_unique_branch_curr() {
        infer_eq(
            indoc!(
                r#"
                       r = "foo"
                       s = { left : "foo" }

                       v = s.left

                       when 0 is
                           1 -> { x: v, y: v }
                           0 -> { x: v, y: r }
                           )
                   "#
            ),
            "Attr.Attr * { x : (Attr.Attr Attr.Shared Str), y : (Attr.Attr Attr.Shared Str) }",
        );
    }

    #[test]
    fn duplicated_record() {
        infer_eq(
                   indoc!(
                       r#"
                       s = { left: 20, right: 20 }

                       { left: s, right: s }
                       "#
                   ),
                   // it's fine that the inner fields are not shared: only shared extraction is possible
                   "Attr.Attr * { left : (Attr.Attr Attr.Shared { left : (Attr.Attr * Int), right : (Attr.Attr * Int) }), right : (Attr.Attr Attr.Shared { left : (Attr.Attr * Int), right : (Attr.Attr * Int) }) }",
               );
    }

    #[test]
    fn result_succeed_alias() {
        infer_eq(
            indoc!(
                r#"
                       Result e a : [ Err e, Ok a ]

                       succeed : q -> Result p q
                       succeed = \x -> Ok x

                       succeed
                       "#
            ),
            "Attr.Attr * (a -> Attr.Attr * (Result * a))",
        );
    }

    #[test]
    fn result_succeed() {
        infer_eq(
            indoc!(
                r#"
                       succeed : a -> [ Err e, Ok a ]
                       succeed = \x -> Ok x

                       succeed
                       "#
            ),
            "Attr.Attr * (a -> Attr.Attr * [ Err *, Ok a ])",
        );
    }

    #[test]
    fn list_singleton_alias() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                singleton : a -> List a
                singleton = \x -> Cons x Nil

                singleton
                       "#
            ),
            "Attr.Attr * (a -> Attr.Attr * (List a))",
        );
    }

    #[test]
    fn list_singleton_as() {
        infer_eq(
            indoc!(
                r#"
                singleton : a -> [ Cons a (List a), Nil ] as List a
                singleton = \x -> Cons x Nil

                singleton
                       "#
            ),
            "Attr.Attr * (a -> Attr.Attr * (List a))",
        );
    }

    #[test]
    fn list_singleton_infer() {
        infer_eq(
            indoc!(
                r#"
                singleton = \x -> Cons x Nil

                singleton
                       "#
            ),
            "Attr.Attr * (a -> Attr.Attr * [ Cons a (Attr.Attr * [ Nil ]*) ]*)",
        );
    }

    #[test]
    fn list_map_alias() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                map : (a -> b), List a -> List b
                map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons x xs ->
                                a = f x
                                b = map f xs

                                Cons a b


                map
                       "#
            ),
            "Attr.Attr Attr.Shared (Attr.Attr Attr.Shared (Attr.Attr a b -> c), Attr.Attr * (List (Attr.Attr a b)) -> Attr.Attr * (List c))" ,
        );
    }

    #[test]
    fn list_map_infer() {
        infer_eq(
            indoc!(
                r#"
                map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons x xs ->
                                a = f x
                                b = map f xs

                                Cons a b


                map
                       "#
            ),
            "Attr.Attr Attr.Shared (Attr.Attr Attr.Shared (Attr.Attr a b -> c), Attr.Attr d [ Cons (Attr.Attr a b) (Attr.Attr d e), Nil ]* as e -> Attr.Attr f [ Cons c (Attr.Attr f g), Nil ]* as g)" ,
        );
    }

    #[test]
    fn peano_map_alias() {
        infer_eq(
            indoc!(
                r#"
                Peano : [ S Peano, Z ]

                map : Peano -> Peano
                map = \peano ->
                        when peano is
                            Z -> Z
                            S rest ->
                                map rest |> S


                map
                       "#
            ),
            "Attr.Attr Attr.Shared (Attr.Attr * Peano -> Attr.Attr * Peano)",
        );
    }

    #[test]
    fn peano_map_infer() {
        infer_eq(
            indoc!(
                r#"
                map = \peano ->
                        when peano is
                            Z -> Z
                            S rest ->
                                map rest |> S


                map
                       "#
            ),
            "Attr.Attr Attr.Shared (Attr.Attr a [ S (Attr.Attr a b), Z ]* as b -> Attr.Attr c [ S (Attr.Attr c d), Z ]* as d)",
        );
    }

    // fails the variable usage check, but I also
    // Assume this gives an error because the generated uniqueness rigid
    // of the top-level signature (say `Attr u1 (List _)`, doesn't unify with the
    // annotation of result (e.g. `Attr u2 (List _)`
    //    #[test]
    //    fn rigid_in_let() {
    //        infer_eq(
    //            indoc!(
    //                r#"
    //                        List q : [ Cons q (List q), Nil ]
    //
    //                        toEmpty : List a -> List a
    //                        toEmpty = \_ ->
    //                            result : List a
    //                            result = Nil
    //
    //                            result
    //
    //                        toEmpty
    //                           "#
    //            ),
    //            "(a -> b), List a -> List b",
    //        );
    //    }
}
