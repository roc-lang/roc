#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;

mod helpers;

#[cfg(test)]
mod test_uniq_solve {
    use crate::helpers::{
        assert_correct_variable_usage, infer_expr, uniq_expr, with_larger_debug_stack,
    };
    use roc_types::pretty_print::{content_to_string, name_all_type_vars};

    // HELPERS

    fn infer_eq_help(src: &str) -> (Vec<roc_solve::solve::TypeError>, String) {
        let (_loc_expr, output, can_problems, mut subs, variable, constraint, home, interns) =
            uniq_expr(src);

        assert_eq!(can_problems, Vec::new(), "Canonicalization problems");

        assert_correct_variable_usage(&constraint);

        for (var, name) in output.introduced_variables.name_by_var {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, variable);

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
        infer_eq("5", "Attr * (Num (Attr * *))");
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
            "Attr * (List (Attr * (Num (Attr * *))))",
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
            "Attr * (List (Attr * (List (Attr * (List (Attr * (Num (Attr * *))))))))",
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
            "Attr * (List (Attr * (Num (Attr * *))))",
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
            "Attr * (List (Attr * (List (Attr * (Num (Attr * *))))))",
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
            "Attr * (*, * -> Attr * (Num (Attr * *)))",
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
            "Attr * (*, * -> Attr * (Num (Attr * *)))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (List (Attr * (Num (Attr * *))))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
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
    //                 neverendingNum = \f int -> f int
    //                 x = neverendingNum (\a -> a) 5

    //                 flip neverendingNum
    //             "#
    //         ),
    //         "((Num (Attr * *)), (a -> a)) -> (Num (Attr * *))",
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
    //             "(Num (Attr * *))",
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
    //             "(Num (Attr * *))",
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
            "Attr * (List (Attr * (Num (Attr * *))))",
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
        );
    }

    #[test]
    fn record() {
        infer_eq("{ foo: 42 }", "Attr * { foo : (Attr * (Num (Attr * *))) }");
    }

    #[test]
    fn record_access() {
        infer_eq("{ foo: 42 }.foo", "Attr * (Num (Attr * *))");
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
                r#"
                    Foo
                "#
            ),
            "Attr * [ Foo ]*",
        );
    }

    #[test]
    fn single_tag_pattern() {
        infer_eq(
            indoc!(
                r#"
                    \Foo -> 42
                "#
            ),
            "Attr * (Attr * [ Foo ]* -> Attr * (Num (Attr * *)))",
        );
    }

    #[test]
    fn single_private_tag_pattern() {
        infer_eq(
            indoc!(
                r#"
                    \@Foo -> 42
                "#
            ),
            "Attr * (Attr * [ @Foo ]* -> Attr * (Num (Attr * *)))",
        );
    }

    #[test]
    fn two_tag_pattern() {
        infer_eq(
            indoc!(
                r#"
                    \x ->
                        when x is
                            True -> 1
                            False -> 0
                "#
            ),
            "Attr * (Attr * [ False, True ]* -> Attr * (Num (Attr * *)))",
        );
    }

    #[test]
    fn tag_application() {
        infer_eq(
            indoc!(
                r#"
                    Foo "happy" 2020
                "#
            ),
            "Attr * [ Foo (Attr * Str) (Attr * (Num (Attr * *))) ]*",
        );
    }

    #[test]
    fn private_tag_application() {
        infer_eq(
            indoc!(
                r#"
                    @Foo "happy" 2020
                "#
            ),
            "Attr * [ @Foo (Attr * Str) (Attr * (Num (Attr * *))) ]*",
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
            "Attr * (Attr (* | a | b) { left : (Attr a c), right : (Attr b d) }* -> Attr * { left : (Attr a c), right : (Attr b d) })"
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Attr (* | a | b) [ Foo (Attr a c) (Attr b *) ]* -> Attr * [ Foo (Attr a c) (Attr * Str) ]*)"
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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Num (Attr * *))",
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
        "Attr * { numIdentity : (Attr Shared (Attr a (Num (Attr b p)) -> Attr a (Num (Attr b p)))), p : (Attr * (Num (Attr * p))), q : (Attr * Float) }"
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

                        # force alias after access (nested let-block)
                        (p = r

                        p)
                "#
            ),
        "Attr * (Attr a { x : (Attr Shared b), y : (Attr Shared c) }d -> Attr a { x : (Attr Shared b), y : (Attr Shared c) }d)"
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
            "Attr * (Attr (* | a | b | c | d | e) { foo : (Attr (a | b | e) { bar : (Attr (a | e) { baz : (Attr e f) }*) }*), tic : (Attr (c | d | e) { tac : (Attr (d | e) { toe : (Attr e f) }*) }*) }* -> Attr e f)"
            // "Attr * (Attr (* | a | b | c | d | e) { foo : (Attr (a | b | c) { bar : (Attr (a | c) { baz : (Attr c f) }*) }*), tic : (Attr (c | d | e) { tac : (Attr (c | d) { toe : (Attr c f) }*) }*) }* -> Attr c f)"
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
            "Attr Shared (Attr * (Num (Attr * *)) -> Attr * (Num (Attr * *)))",
        );
    }

    #[test]
    fn quicksort_swap() {
        infer_eq(
            indoc!(
                r#"
                    swap : Int, Int, List a -> List a
                    swap = \i, j, list ->
                        when Pair (List.get list i) (List.get list j) is
                            Pair (Ok atI) (Ok atJ) ->
                                list
                                    |> List.set i atJ
                                    |> List.set j atI
                            _ ->
                                []

                    swap
                "#
            ),
            "Attr * (Attr Shared Int, Attr Shared Int, Attr * (List (Attr Shared a)) -> Attr * (List (Attr Shared a)))"
        );
    }

    #[test]
    fn quicksort() {
        with_larger_debug_stack(|| {
            infer_eq(
            indoc!(
                r#"
                quicksort : List (Num a), Int, Int -> List (Num a)
                quicksort = \list, low, high ->
                    when partition low high list is
                        Pair partitionIndex partitioned ->
                            partitioned
                                |> quicksort low (partitionIndex - 1)
                                |> quicksort (partitionIndex + 1) high


                swap : Int, Int, List a -> List a
                swap = \i, j, list ->
                    when Pair (List.get list i) (List.get list j) is
                        Pair (Ok atI) (Ok atJ) ->
                            list
                                |> List.set i atJ
                                |> List.set j atI

                        _ ->
                            []


                partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
                partition = \low, high, initialList ->
                    when List.get initialList high is
                        Ok pivot ->
                            go = \i, j, list ->
                                if j < high then
                                    when List.get list j is
                                        Ok value ->
                                            if value <= pivot then
                                                go (i + 1) (j + 1) (swap (i + 1) j list)
                                            else
                                                go i (j + 1) list

                                        Err _ ->
                                            Pair i list
                                else
                                    Pair i list

                            when go (low - 1) low initialList is
                                Pair newI newList ->
                                    Pair (newI + 1) (swap (newI + 1) high newList)

                        Err _ ->
                            Pair (low - 1) initialList

                quicksort
                   "#
            ),
            "Attr Shared (Attr b (List (Attr Shared (Num (Attr c a)))), Attr Shared Int, Attr Shared Int -> Attr b (List (Attr Shared (Num (Attr c a)))))"
        );
        })
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
            "Attr Shared (Num (Attr * *))",
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
            "Attr * { y : (Attr Shared (Num (Attr * *))) }",
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
            "Attr * { left : (Attr Shared { left : (Attr * (Num (Attr * *))), right : (Attr * (Num (Attr * *))) }), right : (Attr Shared { left : (Attr * (Num (Attr * *))), right : (Attr * (Num (Attr * *))) }) }",
        );
    }

    #[test]
    fn result_succeed_alias() {
        infer_eq(
            indoc!(
                r#"
                    Res e a : [ Err e, Ok a ]

                    succeed : q -> Res p q
                    succeed = \x -> Ok x

                    succeed
                "#
            ),
            "Attr * (Attr a q -> Attr * (Res (Attr * p) (Attr a q)))",
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
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    singleton : p -> ConsList p
                    singleton = \x -> Cons x Nil

                    singleton
                "#
            ),
            "Attr * (Attr a p -> Attr * (ConsList (Attr a p)))",
        );
    }

    #[test]
    fn list_singleton_as() {
        infer_eq(
            indoc!(
                r#"
                    singleton : p -> [ Cons p (ConsList p), Nil ] as ConsList p
                    singleton = \x -> Cons x Nil

                    singleton
                "#
            ),
            "Attr * (Attr a p -> Attr * (ConsList (Attr a p)))",
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
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    map : (p -> q), ConsList p -> ConsList q
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
            "Attr Shared (Attr Shared (Attr a p -> Attr b q), Attr * (ConsList (Attr a p)) -> Attr * (ConsList (Attr b q)))" ,
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
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    map : (p -> q), p -> ConsList q
                    map = \f, x -> map f x

                    map
                "#
            ),
            "Attr Shared (Attr * (Attr a p -> Attr b q), Attr a p -> Attr * (ConsList (Attr b q)))",
        );
    }

    #[test]
    fn rigid_in_letnonrec() {
        infer_eq(
            indoc!(
                r#"
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    toEmpty : ConsList p -> ConsList p
                    toEmpty = \_ ->
                        result : ConsList p
                        result = Nil

                        result

                    toEmpty
                "#
            ),
            "Attr * (Attr * (ConsList (Attr a p)) -> Attr * (ConsList (Attr a p)))",
        );
    }

    #[test]
    fn rigid_in_letrec() {
        infer_eq(
            indoc!(
                r#"
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    toEmpty : ConsList p -> ConsList p
                    toEmpty = \_ ->
                        result : ConsList p
                        result = Nil

                        toEmpty result

                    toEmpty
                "#
            ),
            "Attr Shared (Attr * (ConsList (Attr a p)) -> Attr * (ConsList (Attr a p)))",
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

    #[test]
    fn let_record_pattern_with_annotation_alias() {
        infer_eq(
            indoc!(
                r#"
               Foo : { x : Str, y : Float }

               { x, y } : Foo
               { x, y } = { x : "foo", y : 3.14 }

               x
               "#
            ),
            "Attr * Str",
        );
    }

    #[test]
    fn typecheck_mutually_recursive_tag_union() {
        infer_eq(
                    indoc!(
                        r#"
                            ListA a b : [ Cons a (ListB b a), Nil ]
                            ListB a b : [ Cons a (ListA b a), Nil ]

                            ConsList q : [ Cons q (ConsList q), Nil ]

                            toAs : (q -> p), ListA p q -> ConsList p
                            toAs =
                                \f, lista ->
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
                    "Attr Shared (Attr Shared (Attr a q -> Attr b p), Attr * (ListA (Attr b p) (Attr a q)) -> Attr * (ConsList (Attr b p)))"
                );
    }

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
                                        Cons b newConsLista ->
                                            Cons a (Cons (f b) (toAs f newConsLista))

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
            "Attr * (Num (Attr * *))",
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
            "Attr * (Result (Attr * (Num (Attr * *))) (Attr * [ IndexOutOfBounds ]*))",
        );
    }

    #[test]
    fn float_div_builtins() {
        infer_eq(
            indoc!(
                r#"
                Float.highest / Float.highest
               "#
            ),
            "Attr * Float",
        );
    }

    #[test]
    fn float_div_literals() {
        infer_eq(
            indoc!(
                r#"
                3.0 / 4.0
               "#
            ),
            "Attr * Float",
        );
    }

    #[test]
    fn float_div_literal_builtin() {
        infer_eq(
            indoc!(
                r#"
                3.0 / Float.highest
               "#
            ),
            "Attr * Float",
        );
    }

    #[test]
    fn int_div_builtins() {
        infer_eq(
            indoc!(
                r#"
                Int.highest // Int.highest
               "#
            ),
            "Attr * Int",
        );
    }

    #[test]
    fn int_div_literals() {
        infer_eq(
            indoc!(
                r#"
                3 // 4
               "#
            ),
            "Attr * Int",
        );
    }

    #[test]
    fn int_div_literal_builtin() {
        infer_eq(
            indoc!(
                r#"
                3 // Int.highest
               "#
            ),
            "Attr * Int",
        );
    }

    #[test]
    fn list_repeated_get() {
        infer_eq(
            indoc!(
                r#"
                \list ->
                    p = List.get list 1
                    q = List.get list 1

                    { p, q }
               "#
            ),
            "Attr * (Attr * (List (Attr Shared a)) -> Attr * { p : (Attr * (Result (Attr Shared a) (Attr * [ IndexOutOfBounds ]*))), q : (Attr * (Result (Attr Shared a) (Attr * [ IndexOutOfBounds ]*))) })"
        );
    }

    #[test]
    fn list_get_then_set() {
        infer_eq(
            indoc!(
                r#"
                    \list ->
                        when List.get list 0 is
                            Ok v ->
                                List.set list 0 (v + 1)

                            Err _ ->
                                list
               "#
            ),
            "Attr * (Attr a (List (Attr Shared (Num (Attr b c)))) -> Attr a (List (Attr Shared (Num (Attr b c)))))",
        );
    }

    #[test]
    fn list_is_empty_then_set() {
        infer_eq(
            indoc!(
                r#"
                    \list ->
                        if List.isEmpty list then
                            list
                        else
                            List.set list 0 42
               "#
            ),
            "Attr * (Attr (a | b) (List (Attr b (Num (Attr c d)))) -> Attr (a | b) (List (Attr b (Num (Attr c d)))))",
        );
    }

    #[test]
    fn list_set() {
        infer_eq(indoc!(r#"List.set"#), "Attr * (Attr (* | a | b) (List (Attr a c)), Attr * Int, Attr (a | b) c -> Attr * (List (Attr a c)))");
    }

    #[test]
    fn list_map() {
        infer_eq(
            indoc!(r#"List.map"#),
            "Attr * (Attr * (List a), Attr Shared (a -> b) -> Attr * (List b))",
        );
    }

    #[test]
    fn list_map_identity() {
        infer_eq(
            indoc!(r#"\list -> List.map list (\x -> x)"#),
            "Attr * (Attr * (List a) -> Attr * (List a))",
        );
    }

    #[test]
    fn list_foldr() {
        infer_eq(
            indoc!(r#"List.foldr"#),
            "Attr * (Attr * (List a), Attr Shared (a, b -> b), b -> b)",
        );
    }

    #[test]
    fn list_foldr_sum() {
        infer_eq(
            indoc!(
                r#"
                    sum = \list -> List.foldr list Num.add 0

                    sum
                "#
            ),
            "Attr * (Attr * (List (Attr * (Num (Attr a b)))) -> Attr * (Num (Attr a b)))",
        );
    }

    #[test]
    fn list_push() {
        infer_eq(
            indoc!(r#"List.push"#),
            "Attr * (Attr (* | a | b) (List (Attr a c)), Attr (a | b) c -> Attr * (List (Attr a c)))"
        );
    }

    #[test]
    fn list_push_singleton() {
        infer_eq(
            indoc!(
                r#"
                    singleton = \x -> List.push [] x

                    singleton
                "#
            ),
            "Attr * (Attr (* | a) b -> Attr * (List (Attr a b)))",
        );
    }

    #[test]
    fn list_foldr_reverse() {
        infer_eq(
            indoc!(
                r#"
                    reverse = \list -> List.foldr list (\e, l -> List.push l e) []

                    reverse
                "#
            ),
            "Attr * (Attr * (List (Attr (a | b) c)) -> Attr (* | a | b) (List (Attr b c)))",
            //"Attr * (Attr * (List (Attr (a | b) c)) -> Attr (* | a | b) (List (Attr a c)))",
        );
    }

    #[test]
    fn set_then_get() {
        infer_eq(
            indoc!(
                r#"
                    List.getUnsafe (List.set [ 12, 9, 7, 3 ] 1 42) 1
                "#
            ),
            "Attr * (Num (Attr * *))",
        );
    }

    #[test]
    fn use_correct_ext_var() {
        infer_eq(
            indoc!(
                r#"
                f = \r ->
                    g = r.q
                    h = r.p

                    42

                f
                "#
            ),
            //"Attr * (Attr (* | a | b) { p : (Attr a *), q : (Attr b *) }* -> Attr * (Num (Attr * *)))",
            "Attr * (Attr (* | a | b) { p : (Attr b *), q : (Attr a *) }* -> Attr * (Num (Attr * *)))"
        );
    }

    #[test]
    fn reconstruct_path() {
        infer_eq(
            indoc!(
                r#"
                reconstructPath : Map position position, position -> List position
                reconstructPath = \cameFrom, goal ->
                    when Map.get cameFrom goal is
                        Err KeyNotFound ->
                            []

                        Ok next ->
                            List.push (reconstructPath cameFrom next) goal

                reconstructPath
                "#
            ),
            "Attr Shared (Attr Shared (Map (Attr Shared position) (Attr Shared position)), Attr Shared position -> Attr * (List (Attr Shared position)))"
        );
    }

    #[test]
    fn cheapest_open() {
        with_larger_debug_stack(|| {
            infer_eq(
            indoc!(
                r#"
                Model position : { evaluated : Set position
                    , openSet : Set  position
                    , costs : Map.Map position Float
                    , cameFrom : Map.Map position position
                    }

                cheapestOpen : (position -> Float), Model position -> Result position [ KeyNotFound ]*
                cheapestOpen = \costFunction, model ->

                    folder = \position, resSmallestSoFar ->
                            when Map.get model.costs position is
                                Err e ->
                                    Err e

                                Ok cost ->
                                    positionCost = costFunction position

                                    when resSmallestSoFar is
                                        Err _ -> Ok { position, cost: cost + positionCost }
                                        Ok smallestSoFar ->
                                            if positionCost + cost < smallestSoFar.cost then
                                                Ok { position, cost: cost + positionCost }

                                            else
                                                Ok smallestSoFar

                    Set.foldl model.openSet folder (Err KeyNotFound)
                        |> Result.map (\x -> x.position)

                cheapestOpen
                "#
            ),
            "Attr * (Attr * (Attr Shared position -> Attr Shared Float), Attr * (Model (Attr Shared position)) -> Attr * (Result (Attr Shared position) (Attr * [ KeyNotFound ]*)))"
        )
        });
    }

    #[test]
    fn update_cost() {
        infer_eq(
            indoc!(
                r#"
                Model position : { evaluated : Set position
                    , openSet : Set  position
                    , costs : Map.Map position Float
                    , cameFrom : Map.Map position position
                    }

                reconstructPath : Map position position, position -> List position
                reconstructPath = \cameFrom, goal ->
                    when Map.get cameFrom goal is
                        Err KeyNotFound ->
                            []

                        Ok next ->
                            List.push (reconstructPath cameFrom next) goal

                updateCost : position, position, Model position -> Model position
                updateCost = \current, neighbour, model ->
                    newCameFrom = Map.insert model.cameFrom neighbour current

                    newCosts = Map.insert model.costs neighbour distanceTo

                    distanceTo = reconstructPath newCameFrom neighbour
                            |> List.len
                            |> Num.toFloat

                    newModel = { model & costs : newCosts , cameFrom : newCameFrom }

                    when Map.get model.costs neighbour is
                        Err KeyNotFound ->
                            newModel

                        Ok previousDistance ->
                            if distanceTo < previousDistance then
                                newModel

                            else
                                model
                updateCost
                "#
            ),
            "Attr * (Attr Shared position, Attr Shared position, Attr Shared (Model (Attr Shared position)) -> Attr Shared (Model (Attr Shared position)))"
        );
    }

    #[test]
    fn astar_full_code() {
        with_larger_debug_stack(|| {
            infer_eq(
            indoc!(
                r#"
                    Model position : { evaluated : Set position
                        , openSet : Set  position
                        , costs : Map.Map position Float
                        , cameFrom : Map.Map position position
                        }


                    initialModel : position -> Model position
                    initialModel = \start ->
                        { evaluated : Set.empty
                        , openSet : Set.singleton start
                        , costs : Map.singleton start 0.0
                        , cameFrom : Map.empty
                        }


                    cheapestOpen : (position -> Float), Model position -> Result position [ KeyNotFound ]*
                    cheapestOpen = \costFunction, model ->

                        folder = \position, resSmallestSoFar ->
                            when Map.get model.costs position is
                                Err e ->
                                    Err e

                                Ok cost ->
                                    positionCost = costFunction position

                                    when resSmallestSoFar is
                                        Err _ -> Ok { position, cost: cost + positionCost }
                                        Ok smallestSoFar ->
                                            if positionCost + cost < smallestSoFar.cost then
                                                Ok { position, cost: cost + positionCost }

                                            else
                                                Ok smallestSoFar


                        Set.foldl model.openSet folder (Err KeyNotFound)
                            |> Result.map (\x -> x.position)


                    reconstructPath : Map position position, position -> List position
                    reconstructPath = \cameFrom, goal ->
                        when Map.get cameFrom goal is
                            Err KeyNotFound ->
                                []

                            Ok next ->
                                List.push (reconstructPath cameFrom next) goal


                    updateCost : position, position, Model position -> Model position
                    updateCost = \current, neighbour, model ->
                        newCameFrom = Map.insert model.cameFrom neighbour current

                        newCosts = Map.insert model.costs neighbour distanceTo

                        distanceTo =
                            reconstructPath newCameFrom neighbour
                                |> List.length
                                |> Num.toFloat

                        newModel = { model & costs : newCosts , cameFrom : newCameFrom }

                        when Map.get model.costs neighbour is
                            Err KeyNotFound ->
                                newModel

                            Ok previousDistance ->
                                if distanceTo < previousDistance then
                                    newModel

                                else
                                    model


                    findPath : { costFunction: (position, position -> Float), moveFunction: (position -> Set position), start : position, end : position } -> Result (List position) [ KeyNotFound ]*
                    findPath = \{ costFunction, moveFunction, start, end } ->
                        astar costFunction moveFunction end (initialModel start)


                    astar : (position, position -> Float), (position -> Set position), position, Model position -> [ Err [ KeyNotFound ]*, Ok (List position) ]*
                    astar = \costFn, moveFn, goal, model ->
                        when cheapestOpen (\position -> costFn goal position) model is
                            Err _ ->
                                Err KeyNotFound

                            Ok current ->
                                if current == goal then
                                    Ok (reconstructPath model.cameFrom goal)

                                else
                                    modelPopped = { model & openSet : Set.remove model.openSet current, evaluated : Set.insert model.evaluated current }

                                    neighbours = moveFn current

                                    newNeighbours = Set.diff neighbours modelPopped.evaluated

                                    modelWithNeighbours = { modelPopped & openSet : Set.union modelPopped.openSet newNeighbours }

                                    modelWithCosts = Set.foldl newNeighbours (\nb, md -> updateCost current nb md) modelWithNeighbours

                                    astar costFn moveFn goal modelWithCosts

                    findPath
                "#
            ),
            "Attr * (Attr * { costFunction : (Attr Shared (Attr Shared position, Attr Shared position -> Attr Shared Float)), end : (Attr Shared position), moveFunction : (Attr Shared (Attr Shared position -> Attr * (Set (Attr Shared position)))), start : (Attr Shared position) } -> Attr * (Result (Attr * (List (Attr Shared position))) (Attr * [ KeyNotFound ]*)))"
        )
        });
    }

    #[test]
    fn equals() {
        infer_eq(
            indoc!(
                r#"
                \a, b -> a == b
                "#
            ),
            "Attr * (a, a -> Attr * Bool)",
        );
    }

    #[test]
    fn instantiated_alias() {
        infer_eq(
            indoc!(
                r#"
                Model a : { foo : Set a }


                initialModel : position -> Model Int
                initialModel = \_ -> { foo : Set.empty }

                initialModel
                "#
            ),
            "Attr * (Attr * position -> Attr * (Model (Attr * Int)))",
        );
    }

    #[test]
    fn when_with_or_pattern_and_guard() {
        infer_eq(
            indoc!(
                r#"
                \x ->
                    when x is
                        2 | 3 -> 0
                        a if a < 20 ->  1
                        3 | 4 if False -> 2
                        _ -> 3
                "#
            ),
            "Attr * (Attr Shared (Num (Attr * *)) -> Attr * (Num (Attr * *)))",
        );
    }
}
