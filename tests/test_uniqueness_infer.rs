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
        let (output, _problems, mut subs, variable, constraint, home, interns) = uniq_expr(src);

        assert_correct_variable_usage(&constraint);

        for (name, var) in output.rigids {
            subs.rigid_var(var, name);
        }

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
        infer_eq("{}", "Attr * {}");
    }

    #[test]
    fn int_literal() {
        infer_eq("5", "Attr * Int");
    }

    #[test]
    fn float_literal() {
        infer_eq("0.5", "Attr * Float");
    }

    #[test]
    fn string_literal() {
        infer_eq(
            indoc!(
                r#"
                "type inference!"
            "#
            ),
            "Attr * Str",
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
            "Attr * Str",
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
            "Attr * (List *)",
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
            "Attr * (List (Attr * (List *)))",
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
            "Attr * (List (Attr * (List (Attr * (List *)))))",
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
            "Attr * (List (Attr * (List (Attr * (List *)))))",
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
            "Attr * (List (Attr * Int))",
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
            "Attr * (List (Attr * (List (Attr * (List (Attr * Int))))))",
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
            "Attr * (List (Attr * Int))",
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
            "Attr * (List (Attr * (List (Attr * Int))))",
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
            "Attr * (List (Attr * Str))",
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
            "Attr * (List (Attr * (List (Attr * (List (Attr * Str))))))",
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
            "Attr * (List (Attr * Str))",
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
            "Attr * (List <type mismatch>)",
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
            "Attr * (List (Attr * (List <type mismatch>)))",
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
            "Attr * (List <type mismatch>)",
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
            "Attr * (* -> Attr * {})",
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
            "Attr * (*, * -> Attr * Int)",
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
            "Attr * (*, *, * -> Attr * Str)",
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
            "Attr * {}",
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
            "Attr * Str",
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
            "Attr * (* -> Attr * {})",
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
            "Attr * (*, * -> Attr * Int)",
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
            "Attr * (*, *, * -> Attr * Str)",
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
            "Attr * (*, *, * -> Attr * Str)",
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
            "Attr * Str",
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
            "Attr * Int",
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
    //         "Attr * (Attr Shared a -> Attr Shared a)",
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
            "Attr * Int",
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
            "Attr * Str",
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
            "Attr Shared (a -> a)",
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
            "Attr * Int",
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
            "Attr * (List (Attr * Int))",
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
            "Attr * Str",
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
            "Attr * Int",
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
            "Attr * Int",
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
            "Attr * Int",
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
            "Attr * Float",
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
    //            "Attr * (a -> a)",
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
            "Attr Shared (a -> a)",
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
            "Attr * (a -> a)",
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
            "Attr * Int",
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
            "Attr * (Attr * (a -> b), a -> b)",
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
            "Attr * (Attr * (a, b -> c) -> Attr * (b, a -> c))",
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
            "Attr * (a -> Attr * (* -> a))",
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
            "Attr * (Attr * (Attr * {} -> a) -> a)",
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
            "Attr * (List (Attr * Int))",
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
            "Attr * Int",
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
            "Attr * Int",
        );
    }

    #[test]
    fn record() {
        infer_eq("{ foo: 42 }", "Attr * { foo : (Attr * Int) }");
    }

    #[test]
    fn record_access() {
        infer_eq("{ foo: 42 }.foo", "Attr * Int");
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
            "Attr * {}*",
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
            "Attr * { name : (Attr * Str), year : (Attr * Str) }",
        );
    }

    #[test]
    fn bare_tag() {
        infer_eq(
            indoc!(
                r#"Foo
                "#
            ),
            "Attr * [ Foo ]*",
        );
    }

    #[test]
    fn single_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\Foo -> 42
                "#
            ),
            "Attr * (Attr * [ Foo ]* -> Attr * Int)",
        );
    }

    #[test]
    fn single_private_tag_pattern() {
        infer_eq(
            indoc!(
                r#"\@Foo -> 42
                "#
            ),
            "Attr * (Attr * [ @Foo ]* -> Attr * Int)",
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
            "Attr * (Attr * [ False, True ]* -> Attr * Int)",
        );
    }

    #[test]
    fn tag_application() {
        infer_eq(
            indoc!(
                r#"Foo "happy" 2020
                "#
            ),
            "Attr * [ Foo (Attr * Str) (Attr * Int) ]*",
        );
    }

    #[test]
    fn private_tag_application() {
        infer_eq(
            indoc!(
                r#"@Foo "happy" 2020
                "#
            ),
            "Attr * [ @Foo (Attr * Str) (Attr * Int) ]*",
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
            "Attr * (Attr (* | a) { left : (Attr a b) }* -> Attr a b)",
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
            "Attr * (Attr (* | a) { left : (Attr a b) }* -> Attr a b)",
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
            "Attr * (Attr (* | a | b) { left : (Attr b c), right : (Attr a d) }* -> Attr * { left : (Attr b c), right : (Attr a d) })",
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
            "Attr * Int",
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
            "Attr * (Attr (* | a) [ Foo (Attr a b) ]* -> Attr * [ Foo (Attr a b) ]*)",
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
            "Attr * (Attr (* | a | b) [ Foo (Attr b c) (Attr a *) ]* -> Attr * [ Foo (Attr b c) (Attr * Str) ]*)",
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
            "Attr * Int",
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
            "Attr * Int",
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
            "Attr * Int",
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
            "Attr * (Attr (* | a) { left : (Attr a b) }* -> Attr a b)",
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
            "Attr * (Attr (* | a) { left : (Attr a b) }* -> Attr a b)",
        );
    }

    #[test]
    fn num_identity_def() {
        infer_eq(
            indoc!(
                r#"
                   numIdentity : Num.Num p -> Num.Num p
                   numIdentity = \x -> x

                   numIdentity
                   "#
            ),
            "Attr * (Attr a (Num (Attr b p)) -> Attr a (Num (Attr b p)))",
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
            "Attr * (Attr (* | a) { left : (Attr a b) }* -> Attr a b)",
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
            "Attr * (Attr (* | a) { left : (Attr a b) }* -> Attr a b)",
        );
    }

    #[test]
    fn num_identity_applied() {
        infer_eq(
            indoc!(
                r#"
                   numIdentity : Num.Num p -> Num.Num p
                   numIdentity = \foo -> foo

                   p = numIdentity 42
                   q = numIdentity 3.14

                   { numIdentity, p, q }
                   "#
            ),
        "Attr * { numIdentity : (Attr Shared (Attr a (Num (Attr b p)) -> Attr a (Num (Attr b p)))), p : (Attr * Int), q : (Attr * Float) }"
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
            "Attr * (Attr a { x : (Attr Shared b) }c -> Attr a { x : (Attr Shared b) }c)",
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
                "Attr * (Attr a { x : (Attr Shared b), y : (Attr Shared c) }d -> Attr a { x : (Attr Shared b), y : (Attr Shared c) }d)",
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
                "Attr * (Attr Shared { x : (Attr Shared a), y : (Attr Shared b) }c -> Attr Shared { x : (Attr Shared a), y : (Attr Shared b) }c)"
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
            "Attr * (Attr a { x : (Attr Shared b) }c -> Attr a { x : (Attr Shared b) }c)",
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
         "Attr * (Attr a { x : (Attr Shared b), y : (Attr Shared b) }c -> Attr a { x : (Attr Shared b), y : (Attr Shared b) }c)"
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
            "Attr * (Attr (a | b) { foo : (Attr a { bar : (Attr Shared d), baz : (Attr Shared c) }e) }f -> Attr (a | b) { foo : (Attr a { bar : (Attr Shared d), baz : (Attr Shared c) }e) }f)"
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
            "Attr * (Attr (a | b) { foo : (Attr a { bar : (Attr Shared c) }d) }e -> Attr (a | b) { foo : (Attr a { bar : (Attr Shared c) }d) }e)"
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
            "Attr * (Attr a { x : (Attr Shared b), y : (Attr Shared b) }c -> Attr a { x : (Attr Shared b), y : (Attr Shared b) }c)",
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
            "Attr * (Attr (* | a | b | c | d | e) { foo : (Attr (b | d | e) { bar : (Attr (b | e) { baz : (Attr e f) }*) }*), tic : (Attr (a | c | e) { tac : (Attr (a | e) { toe : (Attr e f) }*) }*) }* -> Attr e f)"
           // "Attr * (Attr (* | a | b | c | d | e) { foo : (Attr (a | c | d) { bar : (Attr (c | d) { baz : (Attr d f) }*) }*), tic : (Attr (b | d | e) { tac : (Attr (b | d) { toe : (Attr d f) }*) }*) }* -> Attr d f)"
           // "Attr * (Attr (* | a | b | c | d | e) { foo : (Attr (b | c | e) { bar : (Attr (b | e) { baz : (Attr b f) }*) }*), tic : (Attr (a | b | d) { tac : (Attr (b | d) { toe : (Attr b f) }*) }*) }* -> Attr b f)"
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
            "Attr * Int",
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
            "Attr Shared (Attr * Int -> Attr * Int)",
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
            "Attr * (Attr Shared Int, Attr Shared Int, Attr a (List b) -> Attr a (List b))",
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
            "Attr * (Attr Shared Int, Attr Shared Int, Attr a (List b) -> Attr a (List b))",
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
            "Attr Shared Int",
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
            "Attr * { y : (Attr Shared Int) }",
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
            "Attr * { x : (Attr Shared Str), y : (Attr Shared Str) }",
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
            "Attr * { x : (Attr Shared Str), y : (Attr Shared Str) }",
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
                   "Attr * { left : (Attr Shared { left : (Attr * Int), right : (Attr * Int) }), right : (Attr Shared { left : (Attr * Int), right : (Attr * Int) }) }",
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
            "Attr * (Attr a q -> Attr * (Result (Attr * p) (Attr a q)))",
        );
    }

    #[test]
    fn result_succeed() {
        infer_eq(
            indoc!(
                r#"
                       succeed : p -> [ Err e, Ok p ]
                       succeed = \x -> Ok x

                       succeed
                       "#
            ),
            "Attr * (Attr a p -> Attr * [ Err (Attr * e), Ok (Attr a p) ])",
        );
    }

    #[test]
    fn list_singleton_alias() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                singleton : p -> List p
                singleton = \x -> Cons x Nil

                singleton
                       "#
            ),
            "Attr * (Attr a p -> Attr * (List (Attr a p)))",
        );
    }

    #[test]
    fn list_singleton_as() {
        infer_eq(
            indoc!(
                r#"
                singleton : p -> [ Cons p (List p), Nil ] as List p
                singleton = \x -> Cons x Nil

                singleton
                       "#
            ),
            "Attr * (Attr a p -> Attr * (List (Attr a p)))",
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
            "Attr * (a -> Attr * [ Cons a (Attr * [ Nil ]*) ]*)",
        );
    }

    #[test]
    fn list_map_alias() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                map : (p -> q), List p -> List q
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
            "Attr Shared (Attr Shared (Attr a p -> Attr b q), Attr * (List (Attr a p)) -> Attr * (List (Attr b q)))" ,
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
            "Attr Shared (Attr Shared (Attr a b -> c), Attr d [ Cons (Attr a b) (Attr d e), Nil ]* as e -> Attr f [ Cons c (Attr f g), Nil ]* as g)" ,
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
            "Attr Shared (Attr * Peano -> Attr * Peano)",
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
            "Attr Shared (Attr a [ S (Attr a b), Z ]* as b -> Attr c [ S (Attr c d), Z ]* as d)",
        );
    }

    // This snippet exhibits the rank issue. Seems to only occur when using recursive types with
    // recursive functions.
    #[test]
    fn rigids_in_signature() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                map : (p -> q), p -> List q
                map = \f, x -> map f x

                map
                       "#
            ),
            "Attr Shared (Attr * (Attr a p -> Attr b q), Attr a p -> Attr * (List (Attr b q)))",
        );
    }

    #[test]
    fn rigid_in_letnonrec() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                toEmpty : List p -> List p
                toEmpty = \_ ->
                    result : List p
                    result = Nil

                    result

                toEmpty
                   "#
            ),
            "Attr * (Attr * (List (Attr a p)) -> Attr * (List (Attr a p)))",
        );
    }

    #[test]
    fn rigid_in_letrec() {
        infer_eq(
            indoc!(
                r#"
                List a : [ Cons a (List a), Nil ]

                toEmpty : List p -> List p
                toEmpty = \_ ->
                    result : List p
                    result = Nil

                    toEmpty result

                toEmpty
                   "#
            ),
            "Attr Shared (Attr * (List (Attr a p)) -> Attr * (List (Attr a p)))",
        );
    }

    #[test]
    fn let_record_pattern_with_annotation_inline() {
        infer_eq(
            indoc!(
                r#"
               { x, y } : { x : Str.Str, y : Num.Num Float.FloatingPoint }
               { x, y } = { x : "foo", y : 3.14 }

               x
               "#
            ),
            "Attr * Str",
        );
    }

    // boolean variables introduced by the alias are not bound (by the alias) and thus not instantiated
    //    #[test]
    //    fn let_record_pattern_with_annotation_alias() {
    //        infer_eq(
    //            indoc!(
    //                r#"
    //               Foo : { x : Str.Str, y : Num.Num Float.FloatingPoint }
    //
    //               { x, y } : Foo
    //               { x, y } = { x : "foo", y : 3.14 }
    //
    //               x
    //               "#
    //            ),
    //            "Attr * Str",
    //        );
    //    }

    // infinite loop in type_to_var
    //    #[test]
    //    fn typecheck_mutually_recursive_tag_union() {
    //        infer_eq(
    //            indoc!(
    //                r#"
    //                      ListA a b : [ Cons a (ListB b a), Nil ]
    //                      ListB a b : [ Cons a (ListA b a), Nil ]
    //
    //                      List q : [ Cons q (List q), Nil ]
    //
    //                      toAs : (q -> p), ListA p q -> List p
    //                      toAs = \f, lista ->
    //                           when lista is
    //                               Nil -> Nil
    //                               Cons a listb ->
    //                                   when listb is
    //                                       Nil -> Nil
    //                                       Cons b newLista ->
    //                                           Cons a (Cons (f b) (toAs f newLista))
    //
    //                      toAs
    //                     "#
    //            ),
    //            "Attr Shared (Attr Shared (Attr a q -> Attr b p), Attr * (ListA (Attr b p) (Attr a q)) -> Attr * (List (Attr b p)))"
    //        );
    //    }

    #[test]
    fn infer_mutually_recursive_tag_union() {
        infer_eq(
                indoc!(
                    r#"
                       toAs = \f, lista ->
                            when lista is
                                Nil -> Nil
                                Cons a listb ->
                                    when listb is
                                        Nil -> Nil
                                        Cons b newLista ->
                                            Cons a (Cons (f b) (toAs f newLista))
    
                       toAs
                      "#
                ),
                "Attr Shared (Attr Shared (Attr a b -> c), Attr d [ Cons (Attr e f) (Attr * [ Cons (Attr a b) (Attr d g), Nil ]*), Nil ]* as g -> Attr h [ Cons (Attr e f) (Attr * [ Cons c (Attr h i) ]*), Nil ]* as i)",
            );
    }

    #[test]
    fn addition() {
        infer_eq(
            indoc!(
                r#"
                4 + 4
               "#
            ),
            "Attr * Int",
        );
    }

    #[test]
    fn list_get() {
        infer_eq(
            indoc!(
                r#"
                [1,2,3,4]
                    |> List.get 2
               "#
            ),
            "Attr * (Result (Attr * Int) (Attr * [ IndexOutOfBounds ]*))",
        );
    }

    #[test]
    fn list_set() {
        infer_eq(
            indoc!(
                r#"
                [1, 2 ]
                    |> List.set 1 42
               "#
            ),
            "Attr * (List (Attr * Int))",
        );
    }
}
