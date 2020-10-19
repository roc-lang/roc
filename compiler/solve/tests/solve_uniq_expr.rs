#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;

mod helpers;

#[cfg(test)]
mod solve_uniq_expr {
    use crate::helpers::{
        assert_correct_variable_usage, infer_expr, uniq_expr, with_larger_debug_stack,
    };
    use roc_types::pretty_print::{content_to_string, name_all_type_vars};

    // HELPERS

    fn infer_eq_help(src: &str) -> (Vec<roc_solve::solve::TypeError>, String) {
        let (_loc_expr, output, mut can_problems, mut subs, variable, constraint, home, interns) =
            uniq_expr(src);

        // Disregard UnusedDef problems, because those are unavoidable when
        // returning a function from the test expression.
        can_problems.retain(|prob| match prob {
            roc_problem::can::Problem::UnusedDef(_, _) => false,
            _ => true,
        });

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
            panic!(
                "expected:\n{:?}\ninferred:\n{:?}\nproblems:\n{:?}",
                expected, actual, problems
            );
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
        with_larger_debug_stack(|| {
            infer_eq(
                indoc!(
                    r#"
                    []
                "#
                ),
                "Attr * (List *)",
            );
        })
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
                    always2 = \a, _ -> a

                    1 |> always2 "foo"
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
        infer_eq("{ foo: 42 }", "Attr * { foo : Attr * (Num (Attr * *)) }");
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
                    thunk = \{} -> 42

                    xEmpty = if thunk {} == 42 then { x: {} } else { x: {} }

                    when xEmpty is
                        { x: {} } -> {}
            "#
            ),
            "Attr * {}",
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
            "Attr * { name : Attr * Str, year : Attr * Str }",
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
            "Attr * (Attr (* | b) { left : Attr b a }* -> Attr b a)",
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
            "Attr * (Attr (* | b) { left : Attr b a }* -> Attr b a)",
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
            "Attr * (Attr (* | b | d) { left : Attr b a, right : Attr d c }* -> Attr * { left : Attr b a, right : Attr d c })"
        );
    }

    #[test]
    fn record_field_pattern_match_with_guard() {
        infer_eq(
            indoc!(
                r#"
                    when { x: 5 } is
                        { x: 4 } -> 4
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
            "Attr * (Attr (* | b) [ Foo (Attr b a) ]* -> Attr * [ Foo (Attr b a) ]*)",
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
            "Attr * (Attr (* | b | c) [ Foo (Attr b a) (Attr c *) ]* -> Attr * [ Foo (Attr b a) (Attr * Str) ]*)"
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
                    x : Num.Num Num.Integer
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
            "Attr * (Attr (* | b) { left : Attr b a }* -> Attr b a)",
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
            "Attr * (Attr (* | b) { left : Attr b a }* -> Attr b a)",
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
            "Attr * (Attr b (Num (Attr a p)) -> Attr b (Num (Attr a p)))",
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
            "Attr * (Attr (* | b) { left : Attr b a }* -> Attr b a)",
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
            "Attr * (Attr (* | b) { left : Attr b a }* -> Attr b a)",
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
        "Attr * { numIdentity : Attr Shared (Attr b (Num (Attr a p)) -> Attr b (Num (Attr a p))), p : Attr * (Num (Attr * p)), q : Attr * Float }"
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
            "Attr * (Attr c { x : Attr Shared a }b -> Attr c { x : Attr Shared a }b)",
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
        "Attr * (Attr d { x : Attr Shared a, y : Attr Shared b }c -> Attr d { x : Attr Shared a, y : Attr Shared b }c)",
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
            "Attr * (Attr d { x : Attr Shared a, y : Attr Shared b }c -> Attr d { x : Attr Shared a, y : Attr Shared b }c)"
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
            "Attr * (Attr c { x : Attr Shared a }b -> Attr c { x : Attr Shared a }b)",
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
         "Attr * (Attr c { x : Attr Shared a, y : Attr Shared a }b -> Attr c { x : Attr Shared a, y : Attr Shared a }b)"
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
            "Attr * (Attr (f | d) { foo : Attr d { bar : Attr Shared a, baz : Attr Shared b }c }e -> Attr (f | d) { foo : Attr d { bar : Attr Shared a, baz : Attr Shared b }c }e)"
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
            "Attr * (Attr (e | c) { foo : Attr c { bar : Attr Shared a }b }d -> Attr (e | c) { foo : Attr c { bar : Attr Shared a }b }d)"
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
            "Attr * (Attr c { x : Attr Shared a, y : Attr Shared a }b -> Attr c { x : Attr Shared a, y : Attr Shared a }b)",
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
            "Attr * (Attr (* | b | c | d | e | f) { foo : Attr (d | b | c) { bar : Attr (c | b) { baz : Attr b a }* }*, tic : Attr (f | b | e) { tac : Attr (e | b) { toe : Attr b a }* }* }* -> Attr b a)"
        );
    }

    #[test]
    fn when_with_annotation() {
        infer_eq(
            indoc!(
                r#"
                    x : Num.Num Num.Integer
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
            "Attr * (Attr * Int, Attr * Int, Attr * (List (Attr Shared a)) -> Attr * (List (Attr Shared a)))"
        );
    }

    #[test]
    #[ignore]
    fn quicksort() {
        // theory: partition is handled before swap, so swap is not known, and therefore not taken
        // out of its closure
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
            "Attr Shared (Attr b (List (Attr Shared (Num (Attr Shared a)))), Attr Shared Int, Attr Shared Int -> Attr b (List (Attr Shared (Num (Attr Shared a)))))"
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
            "Attr * { y : Attr Shared (Num (Attr * *)) }",
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
            "Attr * { x : Attr Shared Str, y : Attr Shared Str }",
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
            "Attr * { x : Attr Shared Str, y : Attr Shared Str }",
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
            "Attr * { left : Attr Shared { left : Attr * (Num (Attr * *)), right : Attr * (Num (Attr * *)) }, right : Attr Shared { left : Attr * (Num (Attr * *)), right : Attr * (Num (Attr * *)) } }",
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
            "Attr Shared (Attr Shared (Attr a p -> Attr b q), Attr (* | a) (ConsList (Attr a p)) -> Attr * (ConsList (Attr b q)))" ,
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
            "Attr Shared (Attr Shared (Attr b a -> c), Attr (e | b) [ Cons (Attr b a) (Attr (e | b) d), Nil ]* as d -> Attr g [ Cons c (Attr g f), Nil ]* as f)" ,
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
            "Attr Shared (Attr b [ S (Attr b a), Z ]* as a -> Attr d [ S (Attr d c), Z ]* as c)",
        );
    }

    #[test]
    fn rigids_in_signature() {
        infer_eq(
            indoc!(
                r#"
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    map : (p -> q), ConsList p -> ConsList q
                    map = \f, list ->
                        when list is
                            Cons x xs ->
                                Cons (f x) (map f xs)

                            Nil ->
                                Nil

                    map
                "#
            ),
            "Attr Shared (Attr Shared (Attr a p -> Attr b q), Attr (* | a) (ConsList (Attr a p)) -> Attr * (ConsList (Attr b q)))",
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
            "Attr * (Attr * (ConsList (Attr * p)) -> Attr * (ConsList (Attr * p)))",
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
            "Attr Shared (Attr * (ConsList (Attr * p)) -> Attr * (ConsList (Attr * p)))",
        );
    }

    #[test]
    fn let_record_pattern_with_annotation_inline() {
        infer_eq(
            indoc!(
                r#"
                    { x, y } : { x : Str.Str, y : Num.Num Num.FloatingPoint }
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
    fn alias_of_alias() {
        infer_eq(
            indoc!(
                r#"
               Foo : { x : Str, y : Float }

               Bar : Foo

               f : Bar -> Str
               f = \{ x } -> x

               f
               "#
            ),
            "Attr * (Attr (* | a) Bar -> Attr a Str)",
        );
    }

    #[test]
    fn alias_of_alias_with_type_variable() {
        infer_eq(
            indoc!(
                r#"
                Identity a : [ Identity a ]

                ID a : Identity a

                f : ID a -> a
                f = \Identity x -> x

                f
                "#
            ),
            "Attr * (Attr (* | b) (ID (Attr b a)) -> Attr b a)",
        );
    }

    #[test]
    fn alias_assoc_list_head() {
        infer_eq(
           indoc!(
               r#"
                   ConsList a : [ Cons a (ConsList a), Nil ]
                   AssocList a b : ConsList { key: a, value : b }
                   Maybe a : [ Just a, Nothing ]

                   # AssocList2 a b : [ Cons { key: a, value : b } (AssocList2 a b), Nil ]

                   head : AssocList k v -> Maybe { key: k , value: v  }
                   head = \alist ->
                       when alist is
                           Cons first _ ->
                                Just first

                           Nil ->
                               Nothing

                   head
               "#
           ),
            "Attr * (Attr (* | c) (AssocList (Attr a k) (Attr b v)) -> Attr * (Maybe (Attr c { key : Attr a k, value : Attr b v })))"
       );
    }

    #[test]
    fn cons_list_as_assoc_list_head() {
        infer_eq(
           indoc!(
               r#"
                   ConsList a : [ Cons a (ConsList a), Nil ]
                   Maybe a : [ Just a, Nothing ]

                   head : ConsList { key: k, value: v } -> Maybe { key: k , value: v  }
                   head = \alist ->
                       when alist is
                           Cons first _ ->
                                Just first

                           Nil ->
                               Nothing

                   head
               "#
           ),
        "Attr * (Attr (* | c) (ConsList (Attr c { key : Attr a k, value : Attr b v })) -> Attr * (Maybe (Attr c { key : Attr a k, value : Attr b v })))"
       );
    }

    #[test]
    fn assoc_list_map() {
        infer_eq(
            indoc!(
                r#"
                ConsList a : [ Cons a (ConsList a), Nil ]

                map : ConsList a -> ConsList a
                map = \list ->
                    when list is
                        Cons r xs -> Cons r xs
                        Nil -> Nil

                map
                "#
            ),
            "Attr * (Attr (c | b) (ConsList (Attr b a)) -> Attr (c | b) (ConsList (Attr b a)))",
        );
    }

    #[test]
    fn same_uniqueness_builtin_list() {
        infer_eq(
                    indoc!(
                        r#"
                            toAs : (q -> p), p, q -> List p
                            toAs =
                                \f, x, y -> [ x, (f y) ]
                            toAs
                             "#
                    ),
                    "Attr * (Attr * (Attr a q -> Attr b p), Attr b p, Attr a q -> Attr * (List (Attr b p)))"
                );
    }

    #[test]
    fn same_uniqueness_cons_list() {
        infer_eq(
                    indoc!(
                        r#"
                            ConsList q : [ Cons q (ConsList q), Nil ]

                            toAs : (q -> p), p, q -> ConsList p
                            toAs =
                                \f, x, y ->
                                    # Cons (f y) (Cons x Nil)
                                    Cons x (Cons (f y) Nil)
                            toAs
                             "#
                    ),
                    "Attr * (Attr * (Attr a q -> Attr b p), Attr b p, Attr a q -> Attr * (ConsList (Attr b p)))"
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

                            foo = \_ ->
                                x = 4
                                { a : x, b : x }.a

                            toAs
                             "#
                    ),
                    "Attr Shared (Attr Shared (Attr a q -> Attr b p), Attr (c | a | b) (ListA (Attr b p) (Attr a q)) -> Attr * (ConsList (Attr b p)))"
                );
    }

    #[test]
    fn typecheck_triple_mutually_recursive_tag_union() {
        infer_eq(
                    indoc!(
                        r#"
                        ListA a b : [ Cons a (ListB b a), Nil ]
                        ListB a b : [ Cons a (ListC b a), Nil ]
                        ListC a b : [ Cons a (ListA b a), Nil ]

                        ConsList q : [ Cons q (ConsList q), Nil ]

                        toAs : (q -> p), ListA p q -> ConsList p
                        toAs =
                            \f, lista ->
                                when lista is
                                    Nil -> Nil
                                    Cons a listb ->
                                        when listb is
                                            Nil -> Nil
                                            Cons b listc ->
                                                when listc is
                                                    Nil ->
                                                        Nil

                                                    Cons c newListA ->
                                                        Cons a (Cons (f b) (Cons c (toAs f newListA)))

                        toAs
                        "#
                    ),
                    "Attr Shared (Attr Shared (Attr a q -> Attr b p), Attr (c | a | b) (ListA (Attr b p) (Attr a q)) -> Attr * (ConsList (Attr b p)))"
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
                "Attr Shared (Attr Shared (Attr b a -> c), Attr (g | b | e) [ Cons (Attr e d) (Attr (g | b | e) [ Cons (Attr b a) (Attr (g | b | e) f), Nil ]*), Nil ]* as f -> Attr i [ Cons (Attr e d) (Attr * [ Cons c (Attr i h) ]*), Nil ]* as h)"
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
            "Attr a (Num (Attr a *))",
        );
    }

    #[test]
    fn list_get_at() {
        infer_eq(
            indoc!(
                r#"
                    [1,2,3,4]
                        |> List.get 2
               "#
            ),
            "Attr * (Result (Attr * (Num (Attr * *))) (Attr * [ OutOfBounds ]*))",
        );
    }

    #[test]
    fn float_div_builtins() {
        infer_eq(
            indoc!(
                r#"
                    Num.maxFloat / Num.maxFloat
                "#
            ),
            "Attr * (Result (Attr * Float) (Attr * [ DivByZero ]*))",
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
            "Attr * (Result (Attr * Float) (Attr * [ DivByZero ]*))",
        );
    }

    #[test]
    fn float_div_literal_builtin() {
        infer_eq(
            indoc!(
                r#"
                    3.0 / Num.maxFloat
                "#
            ),
            "Attr * (Result (Attr * Float) (Attr * [ DivByZero ]*))",
        );
    }

    #[test]
    fn int_div_builtins() {
        infer_eq(
            indoc!(
                r#"
                    Num.maxInt // Num.maxInt
                "#
            ),
            "Attr * (Result (Attr * Int) (Attr * [ DivByZero ]*))",
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
            "Attr * (Result (Attr * Int) (Attr * [ DivByZero ]*))",
        );
    }

    #[test]
    fn int_div_literal_builtin() {
        infer_eq(
            indoc!(
                r#"
                    3 // Num.maxInt
                "#
            ),
            "Attr * (Result (Attr * Int) (Attr * [ DivByZero ]*))",
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
            "Attr * (Attr * (List (Attr Shared a)) -> Attr * { p : Attr * (Result (Attr Shared a) (Attr * [ OutOfBounds ]*)), q : Attr * (Result (Attr Shared a) (Attr * [ OutOfBounds ]*)) })"
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
            "Attr * (Attr b (List (Attr Shared (Num (Attr Shared a)))) -> Attr b (List (Attr Shared (Num (Attr Shared a)))))",
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
            "Attr * (Attr (d | c) (List (Attr c (Num (Attr b a)))) -> Attr (d | c) (List (Attr c (Num (Attr b a)))))",
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
    fn list_walk_right_sum() {
        infer_eq(
            indoc!(
                r#"
                    sum = \list -> List.walkRight list Num.add 0

                    sum
                "#
            ),
            "Attr * (Attr (* | b) (List (Attr b (Num (Attr b a)))) -> Attr c (Num (Attr c a)))",
        );
    }

    #[test]
    fn num_add() {
        infer_eq(
            "Num.add",
            "Attr * (Attr b (Num (Attr b a)), Attr c (Num (Attr c a)) -> Attr d (Num (Attr d a)))",
        );
    }

    #[test]
    fn list_isempty() {
        infer_eq("List.isEmpty", "Attr * (Attr * (List *) -> Attr * Bool)");
    }

    #[test]
    fn list_len() {
        infer_eq("List.len", "Attr * (Attr * (List *) -> Attr * Int)");
    }

    #[test]
    fn list_get() {
        infer_eq("List.get", "Attr * (Attr (* | b) (List (Attr b a)), Attr * Int -> Attr * (Result (Attr b a) (Attr * [ OutOfBounds ]*)))");
    }

    #[test]
    fn list_set() {
        infer_eq("List.set", "Attr * (Attr (* | b | c) (List (Attr b a)), Attr * Int, Attr (b | c) a -> Attr * (List (Attr b a)))");
    }

    #[test]
    fn list_single() {
        infer_eq("List.single", "Attr * (a -> Attr * (List a))");
    }

    #[test]
    fn list_repeat() {
        infer_eq(
            "List.repeat",
            "Attr * (Attr * Int, Attr Shared a -> Attr * (List (Attr Shared a)))",
        );
    }

    #[test]
    fn list_append() {
        infer_eq(
            "List.append",
            "Attr * (Attr * (List a), a -> Attr * (List a))",
        );
    }

    #[test]
    fn list_map() {
        infer_eq(
            "List.map",
            "Attr * (Attr * (List a), Attr Shared (a -> b) -> Attr * (List b))",
        );
    }

    #[test]
    fn list_push_singleton() {
        infer_eq(
            indoc!(
                r#"
                    singleton = \x -> List.append [] x

                    singleton
                "#
            ),
            "Attr * (a -> Attr * (List a))",
        );
    }

    #[test]
    fn list_walk_right_reverse() {
        infer_eq(
            indoc!(
                r#"
                    reverse = \list -> List.walkRight list (\e, l -> List.append l e) []

                    reverse
                "#
            ),
            "Attr * (Attr (* | b) (List (Attr b a)) -> Attr * (List (Attr b a)))",
        );
    }

    #[test]
    fn set_then_get() {
        infer_eq(
            indoc!(
                r#"
                    when List.get (List.set [ 12, 9, 7, 3 ] 1 42) 1 is
                        Ok num -> num
                        Err OutOfBounds -> 0
                "#
            ),
            "Attr * (Num (Attr * *))",
        );
    }

    #[test]
    fn set_empty() {
        infer_eq("Set.empty", "Attr * (Set *)");
    }

    #[test]
    fn set_singelton() {
        infer_eq("Set.singleton", "Attr * (a -> Attr * (Set a))");
    }

    #[test]
    fn set_union() {
        infer_eq(
            "Set.union",
            "Attr * (Attr * (Set (Attr * a)), Attr * (Set (Attr * a)) -> Attr * (Set (Attr * a)))",
        );
    }

    #[test]
    fn set_diff() {
        infer_eq(
            "Set.diff",
            "Attr * (Attr * (Set (Attr * a)), Attr * (Set (Attr * a)) -> Attr * (Set (Attr * a)))",
        );
    }

    #[test]
    fn set_foldl() {
        infer_eq(
            "Set.foldl",
            "Attr * (Attr (* | b) (Set (Attr b a)), Attr Shared (Attr b a, c -> c), c -> c)",
        );
    }

    #[test]
    fn set_insert() {
        infer_eq("Set.insert", "Attr * (Attr * (Set a), a -> Attr * (Set a))");
    }

    #[test]
    fn set_remove() {
        infer_eq(
            "Set.remove",
            "Attr * (Attr * (Set (Attr b a)), Attr * a -> Attr * (Set (Attr b a)))",
        );
    }

    #[test]
    fn map_empty() {
        infer_eq("Map.empty", "Attr * (Map * *)");
    }

    #[test]
    fn map_singelton() {
        infer_eq("Map.singleton", "Attr * (a, b -> Attr * (Map a b))");
    }

    #[test]
    fn map_get() {
        infer_eq("Map.get", "Attr * (Attr (* | c) (Map (Attr * a) (Attr c b)), Attr * a -> Attr * (Result (Attr c b) (Attr * [ KeyNotFound ]*)))");
    }

    #[test]
    fn map_insert() {
        infer_eq(
            "Map.insert",
            "Attr * (Attr * (Map a b), a, b -> Attr * (Map a b))",
        );
    }

    #[test]
    fn str_is_empty() {
        infer_eq("Str.isEmpty", "Attr * (Attr * Str -> Attr * Bool)");
    }

    #[test]
    fn str_concat() {
        infer_eq(
            "Str.concat",
            "Attr * (Attr * Str, Attr * Str -> Attr * Str)",
        );
    }

    #[test]
    fn result_map() {
        infer_eq(
            "Result.map",
            "Attr * (Attr * (Result a b), Attr * (a -> c) -> Attr * (Result c b))",
        );
    }

    #[test]
    fn list_roc_head() {
        infer_eq(
            indoc!(
                r#"
                ConsList a : [ Cons a (ConsList a), Nil ]
                Maybe a : [ Just a, Nothing ]

                head : ConsList a -> Maybe a
                head = \list ->
                    when list is
                        Cons x _ -> Just x
                        Nil -> Nothing

                head
                "#
            ),
            "Attr * (Attr (* | b) (ConsList (Attr b a)) -> Attr * (Maybe (Attr b a)))",
        );
    }

    #[test]
    fn list_roc_is_empty() {
        infer_eq(
            indoc!(
                r#"
                ConsList a : [ Cons a (ConsList a), Nil ]

                isEmpty : ConsList a -> Bool
                isEmpty = \list ->
                    when list is
                        Cons _ _ -> False
                        Nil -> True

                isEmpty
                "#
            ),
            "Attr * (Attr (* | b) (ConsList (Attr b a)) -> Attr * Bool)",
        );
    }

    #[test]
    fn hidden_uniqueness_one() {
        infer_eq(
            indoc!(
                r#"
                Model : { foo : Int }

                extract : Model -> Int
                extract = \{ foo } -> foo

                extract
                "#
            ),
            "Attr * (Attr (* | a) Model -> Attr a Int)",
        );
    }

    #[test]
    fn hidden_uniqueness_two() {
        infer_eq(
            indoc!(
                r#"
                Model : { foo : Int, bar : Int  }

                extract : Model -> Int
                extract = \{ foo } -> foo

                extract
                "#
            ),
            "Attr * (Attr (* | a) Model -> Attr a Int)",
        );
    }

    #[test]
    fn hidden_uniqueness_three() {
        infer_eq(
            indoc!(
                r#"
                Model : { foo : Int, bar : Int }

                # extract : { foo : Int, bar : Int  } -> Int
                extract : Model -> Int
                # extract = \r -> r.foo + r.bar
                extract = \{foo, bar} -> foo + bar

                extract
                "#
            ),
            "Attr * (Attr (* | * | *) Model -> Attr * Int)",
        );
    }

    #[test]
    fn peano_roc_is_empty() {
        infer_eq(
            indoc!(
                r#"
                Peano : [ Z, S Peano ]

                isEmpty : Peano -> Bool
                isEmpty = \list ->
                    when list is
                        S _ -> False
                        Z -> True

                isEmpty
                "#
            ),
            "Attr * (Attr * Peano -> Attr * Bool)",
        );
    }

    #[test]
    fn result_roc_map() {
        infer_eq(
            indoc!(
                r#"
                map : Result a e, (a -> b) -> Result b e
                map = \result, f ->
                    when result is
                        Ok v -> Ok (f v)
                        Err e -> Err e

                map
                "#
            ),
            "Attr * (Attr (* | c | d) (Result (Attr c a) (Attr d e)), Attr * (Attr c a -> Attr f b) -> Attr * (Result (Attr f b) (Attr d e)))"
        );
    }

    #[test]
    fn result_roc_with_default_with_signature() {
        infer_eq(
            indoc!(
                r#"
                withDefault : Result a e, a -> a
                withDefault = \result, default ->
                    when result is
                        Ok v -> v
                        Err _ -> default

                withDefault
                "#
            ),
            "Attr * (Attr (* | b | c) (Result (Attr b a) (Attr c e)), Attr b a -> Attr b a)",
        );
    }

    #[test]
    fn result_roc_with_default_no_signature() {
        infer_eq(
            indoc!(
                r#"
                \result, default ->
                    when result is
                        Ok x -> x
                        Err _ -> default
                "#
            ),
            "Attr * (Attr (* | a | c) [ Err (Attr a *), Ok (Attr c b) ]*, Attr c b -> Attr c b)",
        );
    }

    #[test]
    fn record_pattern_match_field() {
        infer_eq(
            indoc!(
                r#"
                f : { x : b } -> b
                f = \{ x } -> x

                f
                "#
            ),
            "Attr * (Attr (* | a) { x : Attr a b } -> Attr a b)",
        );
    }

    #[test]
    fn int_addition_with_annotation() {
        infer_eq(
            indoc!(
                r#"
                f : Int, Int -> Int
                f = \a, b -> a + b

                f
                "#
            ),
            "Attr * (Attr * Int, Attr * Int -> Attr * Int)",
        );
    }

    #[test]
    fn int_abs_with_annotation() {
        infer_eq(
            indoc!(
                r#"
                foobar : Int -> Int
                foobar = \x -> Num.abs x

                foobar
                "#
            ),
            "Attr * (Attr * Int -> Attr * Int)",
        );
    }

    #[test]
    fn int_addition_without_annotation() {
        infer_eq(
            indoc!(
                r#"
                f = \a, b -> a + b + 0x0

                f
                "#
            ),
            "Attr * (Attr a Int, Attr b Int -> Attr c Int)",
        );
    }

    #[test]
    fn num_addition_with_annotation() {
        infer_eq(
            indoc!(
                r#"
                f : Num a, Num a -> Num a
                f = \a, b -> a + b

                f
                "#
            ),
            "Attr * (Attr b (Num (Attr b a)), Attr c (Num (Attr c a)) -> Attr d (Num (Attr d a)))",
        );
    }

    #[test]
    fn num_addition_without_annotation() {
        infer_eq(
            indoc!(
                r#"
                f = \a, b -> a + b

                f
                "#
            ),
            "Attr * (Attr b (Num (Attr b a)), Attr c (Num (Attr c a)) -> Attr d (Num (Attr d a)))",
        );
    }

    #[test]
    fn num_abs_with_annotation() {
        infer_eq(
            indoc!(
                r#"
                f : Num a -> Num a
                f = \x -> Num.abs x

                f
                "#
            ),
            "Attr * (Attr b (Num (Attr b a)) -> Attr c (Num (Attr c a)))",
        );
    }

    #[test]
    fn num_abs_without_annotation() {
        infer_eq(
            indoc!(
                r#"
                \x -> Num.abs x
                "#
            ),
            "Attr * (Attr b (Num (Attr b a)) -> Attr c (Num (Attr c a)))",
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
            //"Attr * (Attr (* | a | b) { p : Attr a *, q : Attr b * }* -> Attr * (Num (Attr * *)))",
            "Attr * (Attr (* | a | b) { p : Attr a *, q : Attr b * }* -> Attr * (Num (Attr * *)))",
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
                            List.append (reconstructPath cameFrom next) goal

                reconstructPath
                "#
            ),
            "Attr Shared (Attr Shared (Map (Attr * position) (Attr Shared position)), Attr Shared position -> Attr * (List (Attr Shared position)))"
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
            "Attr * (Attr * (Attr Shared position -> Attr * Float), Attr (* | * | a | b) (Model (Attr Shared position)) -> Attr * (Result (Attr Shared position) (Attr * [ KeyNotFound ]*)))"
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
                            List.append (reconstructPath cameFrom next) goal

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
    #[ignore]
    fn astar_full_code() {
        // theory: things are canonicalized in an order that leaves too much captured
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
                                List.append (reconstructPath cameFrom next) goal


                    updateCost : position, position, Model position -> Model position
                    updateCost = \current, neighbour, model ->
                        newCameFrom = Map.insert model.cameFrom neighbour current

                        newCosts = Map.insert model.costs neighbour distanceTo

                        distanceTo =
                            reconstructPath newCameFrom neighbour
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
            "Attr * (Attr * { costFunction : Attr Shared (Attr Shared position, Attr Shared position -> Attr * Float), end : Attr Shared position, moveFunction : Attr Shared (Attr Shared position -> Attr * (Set (Attr * position))), start : Attr Shared position } -> Attr * (Result (Attr * (List (Attr Shared position))) (Attr * [ KeyNotFound ]*)))"
        )
        });
    }

    #[test]
    fn bool_eq() {
        infer_eq(
            "\\a, b -> a == b",
            "Attr * (Attr * a, Attr * a -> Attr * Bool)",
        );
    }

    #[test]
    fn bool_neq() {
        infer_eq(
            "\\a, b -> a != b",
            "Attr * (Attr * a, Attr * a -> Attr * Bool)",
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
            "Attr * (Attr Shared (Num (Attr Shared *)) -> Attr * (Num (Attr * *)))",
        );
    }

    // OPTIONAL RECORD FIELDS

    #[test]
    fn optional_field_unifies_with_missing() {
        infer_eq(
            indoc!(
                r#"
                    negatePoint : { x : Int, y : Int, z ? Num c } -> { x : Int, y : Int, z : Num c }

                    negatePoint { x: 1, y: 2 }
                "#
            ),
            "Attr * { x : Attr * Int, y : Attr * Int, z : Attr * (Num (Attr * c)) }",
        );
    }

    #[test]
    fn open_optional_field_unifies_with_missing() {
        infer_eq(
            indoc!(
                r#"
                    negatePoint : { x : Int, y : Int, z ? Num c }r -> { x : Int, y : Int, z : Num c }r

                    a = negatePoint { x: 1, y: 2 }
                    b = negatePoint { x: 1, y: 2, blah : "hi" }

                    { a, b }
                "#
            ),
            "Attr * { a : Attr * { x : Attr * Int, y : Attr * Int, z : Attr * (Num (Attr * c)) }, b : Attr * { blah : Attr * Str, x : Attr * Int, y : Attr * Int, z : Attr * (Num (Attr * c)) } }"
        );
    }

    #[test]
    fn optional_field_unifies_with_present() {
        infer_eq(
            indoc!(
                r#"
                    negatePoint : { x : Num a, y : Num b, z ? c } -> { x : Num a, y : Num b, z : c }

                    negatePoint { x: 1, y: 2.1, z: 0x3 }
                "#
            ),
            "Attr * { x : Attr * (Num (Attr * a)), y : Attr * Float, z : Attr * Int }",
        );
    }

    #[test]
    fn open_optional_field_unifies_with_present() {
        infer_eq(
            indoc!(
                r#"
                    negatePoint : { x : Num a, y : Num b, z ? c }r -> { x : Num a, y : Num b, z : c }r

                    a = negatePoint { x: 1, y: 2.1 }
                    b = negatePoint { x: 1, y: 2.1, blah : "hi" }

                    { a, b }
                "#
            ),
            "Attr * { a : Attr * { x : Attr * (Num (Attr * a)), y : Attr * Float, z : Attr * c }, b : Attr * { blah : Attr * Str, x : Attr * (Num (Attr * a)), y : Attr * Float, z : Attr * c } }"
        );
    }

    #[test]
    fn optional_field_function() {
        infer_eq(
            indoc!(
                r#"
                \{ x, y ? 0 } -> x + y
                "#
            ),
            "Attr * (Attr (* | b | c) { x : Attr b (Num (Attr b a)), y ? Attr c (Num (Attr c a)) }* -> Attr d (Num (Attr d a)))"
        );
    }

    #[test]
    fn optional_field_let() {
        infer_eq(
            indoc!(
                r#"
                { x, y ? 0 } = { x: 32 }

                x + y
                "#
            ),
            "Attr a (Num (Attr a *))",
        );
    }

    #[test]
    fn optional_field_when() {
        infer_eq(
            indoc!(
                r#"
                \r ->
                    when r is
                        { x, y ? 0 } -> x + y
                "#
            ),
            "Attr * (Attr (* | b | c) { x : Attr b (Num (Attr b a)), y ? Attr c (Num (Attr c a)) }* -> Attr d (Num (Attr d a)))"
        );
    }

    #[test]
    fn list_walk_right() {
        infer_eq(
            indoc!(
                r#"
                List.walkRight 
                "#
            ),
            "Attr * (Attr (* | b) (List (Attr b a)), Attr Shared (Attr b a, c -> c), c -> c)",
        );
    }

    #[test]
    fn list_walk_right_example() {
        infer_eq(
            indoc!(
                r#"
                empty : List Int
                empty = 
                    []

                List.walkRight empty (\a, b -> a + b) 0
                "#
            ),
            "Attr a Int",
        );
    }
}
