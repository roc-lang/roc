#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;

mod helpers;

#[cfg(test)]
mod test_solve {
    use crate::helpers::{assert_correct_variable_usage, can_expr, infer_expr, CanExprOut};
    use roc_types::pretty_print::{content_to_string, name_all_type_vars};
    use roc_types::subs::Subs;

    // HELPERS

    fn infer_eq_help(
        src: &str,
    ) -> (
        Vec<roc_types::types::Problem>,
        Vec<roc_problem::can::Problem>,
        String,
    ) {
        let CanExprOut {
            output,
            var_store,
            var,
            constraint,
            home,
            interns,
            problems: can_problems,
            ..
        } = can_expr(src);
        let mut subs = Subs::new(var_store.into());

        assert_correct_variable_usage(&constraint);

        for (var, name) in output.introduced_variables.name_by_var {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        name_all_type_vars(var, &mut subs);

        let actual_str = content_to_string(content, &mut subs, home, &interns);

        (unify_problems, can_problems, actual_str)
    }

    fn infer_eq(src: &str, expected: &str) {
        let (_, _can_problems, actual) = infer_eq_help(src);

        assert_eq!(actual, expected.to_string());
    }

    fn infer_eq_without_problem(src: &str, expected: &str) {
        let (type_problems, _, actual) = infer_eq_help(src);

        if !type_problems.is_empty() {
            // fail with an assert, but print the problems normally so rust doesn't try to diff
            // an empty vec with the problems.
            panic!("expected:\n{:?}\ninferred:\n{:?}", expected, actual);
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

    #[test]
    fn concat_different_types() {
        infer_eq(
            indoc!(
                r#"
                empty = []
                one = List.concat [ 1 ] empty
                str = List.concat [ "blah" ] empty

                empty
            "#
            ),
            "List *",
        );
    }

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
                    foo: Str -> {}

                    foo "hi"
                "#
            ),
            "{}",
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
                    { x, y } : { x : ({} -> custom), y : {} }

                    x
                "#
            ),
            "{} -> custom",
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
            "[ @Foo ]* -> Int",
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
            "[ @Foo Str Int ]*",
        );
    }

    #[test]
    fn record_extraction() {
        infer_eq(
            indoc!(
                r#"
                    f = \x ->
                        when x is
                            { a, b: _ } -> a

                    f
                "#
            ),
            "{ a : a, b : * }* -> a",
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
                    when Foo "blah" is
                        Foo x -> x
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn private_tag_with_field() {
        infer_eq(
            indoc!(
                r#"
                    when @Foo "blah" is
                        @Foo x -> x
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn annotation_using_int() {
        infer_eq(
            indoc!(
                r#"
                   int : Int

                   int
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn annotation_using_num_integer() {
        infer_eq(
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
    fn annotated_int() {
        infer_eq(
            indoc!(
                r#"
                   int : Int
                   int = 5

                   int
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn qualified_annotated_int() {
        infer_eq(
            indoc!(
                r#"
                   int : Int.Int
                   int = 5

                   int
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn annotated_num_integer() {
        infer_eq(
            indoc!(
                r#"
                   int : Num Integer
                   int = 5.5

                   int
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn qualified_annotated_num_integer() {
        infer_eq(
            indoc!(
                r#"
                   int : Num.Num Int.Integer
                   int = 5.5

                   int
                "#
            ),
            "Int",
        );
    }

    #[test]
    fn annotation_using_float() {
        infer_eq(
            indoc!(
                r#"
                   float : Float

                   float
                "#
            ),
            "Float",
        );
    }

    #[test]
    fn annotation_using_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num FloatingPoint

                   float
                "#
            ),
            "Float",
        );
    }

    #[test]
    fn qualified_annotated_float() {
        infer_eq(
            indoc!(
                r#"
                   float : Float.Float
                   float = 5.5

                   float
                "#
            ),
            "Float",
        );
    }

    #[test]
    fn annotated_float() {
        infer_eq(
            indoc!(
                r#"
                   float : Float
                   float = 5.5

                   float
                "#
            ),
            "Float",
        );
    }

    #[test]
    fn annotated_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num FloatingPoint
                   float = 5.5

                   float
                "#
            ),
            "Float",
        );
    }

    #[test]
    fn fake_result_ok() {
        infer_eq(
            indoc!(
                r#"
                    Res a e : [ Okay a, Error e ]

                    ok : Res Int *
                    ok = Okay 5

                    ok
                "#
            ),
            "Res Int *",
        );
    }

    #[test]
    fn fake_result_err() {
        infer_eq(
            indoc!(
                r#"
                    Res a e : [ Okay a, Error e ]

                    err : Res * Str
                    err = Error "blah"

                    err
                "#
            ),
            "Res * Str",
        );
    }

    #[test]
    fn basic_result_ok() {
        infer_eq(
            indoc!(
                r#"
                    ok : Result Int *
                    ok = Ok 5

                    ok
                "#
            ),
            "Result Int *",
        );
    }

    #[test]
    fn basic_result_err() {
        infer_eq(
            indoc!(
                r#"
                    err : Result * Str
                    err = Err "blah"

                    err
                "#
            ),
            "Result * Str",
        );
    }

    #[test]
    fn basic_result_conditional() {
        infer_eq(
            indoc!(
                r#"
                    ok : Result Int *
                    ok = Ok 5

                    err : Result * Str
                    err = Err "blah"

                    if 1 > 0 then
                        ok
                    else
                        err
                "#
            ),
            "Result Int Str",
        );
    }

    #[test]
    fn qualified_annotated_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.Num Float.FloatingPoint
                   float = 5.5

                   float
                "#
            ),
            "Float",
        );
    }

    // #[test]
    // fn annotation_using_num_used() {
    //     // There was a problem where `Int`, because it is only an annotation
    //     // wasn't added to the vars_by_symbol.
    //     infer_eq_without_problem(
    //         indoc!(
    //             r#"
    //                int : Int

    //                p = (\x -> x) int

    //                p
    //                "#
    //         ),
    //         "Int",
    //     );
    // }

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
                            { x: _ } -> "1"
                            { y: _ } -> "2"

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
    fn result_map_explicit() {
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
    fn result_map_alias() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Res e a : [ Ok a, Err e ]

                    map : (a -> b), Res e a -> Res e b
                    map = \f, result ->
                        when result is
                            Ok v -> Ok (f v)
                            Err e -> Err e

                    map
                       "#
            ),
            "(a -> b), Res e a -> Res e b",
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

    #[test]
    fn use_as_in_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    foo : Str.Str as Foo -> Foo
                    foo = \_ -> "foo"

                    foo
                "#
            ),
            "Foo -> Foo",
        );
    }

    #[test]
    fn use_alias_in_let() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Foo : Str.Str

                    foo : Foo -> Foo
                    foo = \_ -> "foo"

                    foo
                "#
            ),
            "Foo -> Foo",
        );
    }

    #[test]
    fn use_alias_with_argument_in_let() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Foo a : { foo : a }

                v : Foo (Num.Num Int.Integer)
                v = { foo: 42 }

                v
                "#
            ),
            "Foo Int",
        );
    }

    #[test]
    fn identity_alias() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Foo a : { foo : a }

                id : Foo a -> Foo a
                id = \x -> x

                id
                "#
            ),
            "Foo a -> Foo a",
        );
    }

    #[test]
    fn linked_list_empty() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    empty : [ Cons a (ConsList a), Nil ] as ConsList a
                    empty = Nil

                    empty
                       "#
            ),
            "ConsList a",
        );
    }

    #[test]
    fn linked_list_singleton() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    singleton : a -> [ Cons a (ConsList a), Nil ] as ConsList a
                    singleton = \x -> Cons x Nil

                    singleton
                       "#
            ),
            "a -> ConsList a",
        );
    }

    #[test]
    fn peano_length() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Peano : [ S Peano, Z ]

                    length : Peano -> Num.Num Int.Integer
                    length = \peano ->
                        when peano is
                            Z -> 0
                            S v -> length v

                    length
                       "#
            ),
            "Peano -> Int",
        );
    }

    #[test]
    fn peano_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map : [ S Peano, Z ] as Peano -> Peano
                    map = \peano ->
                        when peano is
                            Z -> Z
                            S v -> S (map v)

                    map
                       "#
            ),
            "Peano -> Peano",
        );
    }

    #[test]
    fn infer_linked_list_map() {
        infer_eq_without_problem(
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
            "(a -> b), [ Cons a c, Nil ]* as c -> [ Cons b d, Nil ]* as d",
        );
    }

    #[test]
    fn typecheck_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    map : (a -> b), ConsList a -> ConsList b
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons x xs ->
                                Cons (f x) (map f xs)

                    map
                       "#
            ),
            "(a -> b), ConsList a -> ConsList b",
        );
    }

    #[test]
    fn mismatch_in_alias_args_gets_reported() {
        infer_eq(
            indoc!(
                r#"
                Foo a : a

                r : Foo {}
                r = {}

                s : Foo Str.Str
                s = "bar"

                when {} is
                    _ -> s
                    _ -> r
                "#
            ),
            "<type mismatch>",
        );
    }

    #[test]
    fn mismatch_in_apply_gets_reported() {
        infer_eq(
            indoc!(
                r#"
                r : { x : (Num.Num Int.Integer) }
                r = { x : 1 }

                s : { left : { x : Num.Num Float.FloatingPoint } }
                s = { left: { x : 3.14 } }

                when 0 is
                    1 -> s.left
                    0 -> r
                   "#
            ),
            "<type mismatch>",
        );
    }

    #[test]
    fn mismatch_in_tag_gets_reported() {
        infer_eq(
            indoc!(
                r#"
                r : [ Ok Str.Str ]
                r = Ok 1

                s : { left: [ Ok {} ] }
                s = { left: Ok 3.14  }

                when 0 is
                    1 -> s.left
                    0 -> r
                   "#
            ),
            "<type mismatch>",
        );
    }

    // TODO As intended, this fails, but it fails with the wrong error!
    //
    // #[test]
    // fn nums() {
    //     infer_eq_without_problem(
    //         indoc!(
    //             r#"
    //                 s : Num *
    //                 s = 3.1

    //                 s
    //                 "#
    //         ),
    //         "<Type Mismatch: _____________>",
    //     );
    // }

    #[test]
    fn manual_attr() {
        infer_eq(
            indoc!(
                r#"
                    r = Attr unknown "bar"

                    s = Attr unknown2 { left : Attr Shared "foo" }

                    when True is
                        _ -> { x : ((\Attr _ val -> val) s).left, y : r }
                        _ -> { x : ((\Attr _ val -> val) s).left, y : ((\Attr _ val -> val) s).left }
                "#
            ),
            "{ x : [ Attr [ Shared ]* Str ]*, y : [ Attr [ Shared ]* Str ]* }",
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
            "Peano -> Peano",
        );
    }

    #[test]
    fn unit_alias() {
        infer_eq(
            indoc!(
                r#"
                    Unit : [ Unit ]

                    unit : Unit
                    unit = Unit

                    unit
                "#
            ),
            "Unit",
        );
    }

    #[test]
    fn rigid_in_letnonrec() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    toEmpty : ConsList a -> ConsList a
                    toEmpty = \_ ->
                        result : ConsList a
                        result = Nil

                        result

                    toEmpty
                "#
            ),
            "ConsList a -> ConsList a",
        );
    }

    #[test]
    fn rigid_in_letrec() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList a : [ Cons a (ConsList a), Nil ]

                    toEmpty : ConsList a -> ConsList a
                    toEmpty = \_ ->
                        result : ConsList a
                        result = Nil

                        toEmpty result

                    toEmpty
                "#
            ),
            "ConsList a -> ConsList a",
        );
    }

    #[test]
    fn let_record_pattern_with_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    { x, y } : { x : Str.Str, y : Num.Num Float.FloatingPoint }
                    { x, y } = { x : "foo", y : 3.14 }

                    x
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn let_record_pattern_with_annotation_alias() {
        infer_eq(
            indoc!(
                r#"
                    Foo : { x : Str.Str, y : Num.Num Float.FloatingPoint }

                    { x, y } : Foo
                    { x, y } = { x : "foo", y : 3.14 }

                    x
                "#
            ),
            "Str",
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
            "[ S a, Z ]* as a -> [ S b, Z ]* as b",
        );
    }

    #[test]
    fn let_record_pattern_with_alias_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Foo : { x : Str.Str, y : Num.Num Float.FloatingPoint }

                    { x, y } : Foo
                    { x, y } = { x : "foo", y : 3.14 }

                    x
               "#
            ),
            "Str",
        );
    }

    // #[test]
    // fn let_tag_pattern_with_annotation() {
    //     infer_eq_without_problem(
    //         indoc!(
    //             r#"
    //                 UserId x : [ UserId Int ]
    //                 UserId x = UserId 42

    //                 x
    //             "#
    //         ),
    //         "Int",
    //     );
    // }

    #[test]
    fn typecheck_record_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList q : [ Cons { x: q, xs: ConsList q }, Nil ]

                    map : (a -> b), ConsList a -> ConsList b
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons { x,  xs } ->
                                Cons { x: f x, xs : map f xs }

                    map
                "#
            ),
            "(a -> b), ConsList a -> ConsList b",
        );
    }

    #[test]
    fn infer_record_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons { x,  xs } ->
                                Cons { x: f x, xs : map f xs }

                    map
                "#
            ),
            "(a -> b), [ Cons { x : a, xs : c }*, Nil ]* as c -> [ Cons { x : b, xs : d }, Nil ]* as d",
        );
    }

    #[test]
    fn typecheck_mutually_recursive_tag_union() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ListA a b : [ Cons a (ListB b a), Nil ]
                    ListB a b : [ Cons a (ListA b a), Nil ]

                    ConsList q : [ Cons q (ConsList q), Nil ]

                    toAs : (b -> a), ListA a b -> ConsList a
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
            "(b -> a), ListA a b -> ConsList a",
        );
    }

    #[test]
    fn typecheck_mutually_recursive_tag_union_listabc() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ListA a : [ Cons a (ListB a) ]
                    ListB a : [ Cons a (ListC a) ]
                    ListC a : [ Cons a (ListA a), Nil ]

                    val : ListC Int.Int
                    val = Cons 1 (Cons 2 (Cons 3 Nil))

                    val
                "#
            ),
            "ListC Int",
        );
    }

    #[test]
    fn infer_mutually_recursive_tag_union() {
        infer_eq_without_problem(
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
            "(a -> b), [ Cons c [ Cons a d, Nil ]*, Nil ]* as d -> [ Cons c [ Cons b e ]*, Nil ]* as e"
        );
    }

    #[test]
    fn type_more_general_than_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                partition : Int, Int, List Int -> [ Pair Int (List Int) ]
                partition = \low, high, initialList ->
                    when List.get initialList high is
                        Ok _ ->
                            Pair 0 []

                        Err _ ->
                            Pair (low - 1) initialList

                partition
                            "#
            ),
            "Int, Int, List Int -> [ Pair Int (List Int) ]",
        );
    }

    #[test]
    fn quicksort_partition() {
        infer_eq_without_problem(
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
                            list

                partition : Int, Int, List Int -> [ Pair Int (List Int) ]
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

                partition
            "#
            ),
            "Int, Int, List Int -> [ Pair Int (List Int) ]",
        );
    }

    #[test]
    fn identity_list() {
        infer_eq_without_problem(
            indoc!(
                r#"
                idList : List a -> List a
                idList = \list -> list

                foo : List Int -> List Int
                foo = \initialList -> idList initialList


                foo
            "#
            ),
            "List Int -> List Int",
        );
    }

    #[test]
    fn list_get() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    List.get [ 10, 9, 8, 7 ] 1
                "#
            ),
            "Result Int [ IndexOutOfBounds ]*",
        );
    }

    #[test]
    fn use_rigid_twice() {
        infer_eq_without_problem(
            indoc!(
                r#"
                id1 : q -> q
                id1 = \x -> x

                id2 : q -> q
                id2 = \x -> x

                { id1, id2 }
                "#
            ),
            "{ id1 : q -> q, id2 : q -> q }",
        );
    }

    #[test]
    fn map_insert() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Map.insert
                "#
            ),
            "Map a b, a, b -> Map a b",
        );
    }

    #[test]
    fn num_to_float() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.toFloat
                "#
            ),
            "Num * -> Float",
        );
    }

    #[test]
    fn reconstruct_path() {
        infer_eq_without_problem(
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
            "Map position position, position -> List position",
        );
    }

    #[test]
    fn use_correct_ext_record() {
        // Related to a bug solved in 81fbab0b3fe4765bc6948727e603fc2d49590b1c
        infer_eq_without_problem(
            indoc!(
                r#"
                f = \r ->
                    g = r.q
                    h = r.p

                    42

                f
                "#
            ),
            "{ p : *, q : * }* -> Int",
        );
    }

    #[test]
    fn use_correct_ext_tag_union() {
        // related to a bug solved in 08c82bf151a85e62bce02beeed1e14444381069f
        infer_eq_without_problem(
            indoc!(
                r#"
                boom = \_ -> boom {}

                Model position : { openSet : Set position }

                cheapestOpen : Model position -> Result position [ KeyNotFound ]*
                cheapestOpen = \model ->

                    folder = \position, resSmallestSoFar ->
                                    when resSmallestSoFar is
                                        Err _ -> resSmallestSoFar
                                        Ok smallestSoFar ->
                                            if position == smallestSoFar.position then resSmallestSoFar

                                            else
                                                Ok { position, cost: 0.0 }

                    Set.foldl model.openSet folder (Ok { position: boom {}, cost: 0.0 })
                        |> Result.map (\x -> x.position)

                astar : Model position -> Result position [ KeyNotFound ]*
                astar = \model -> cheapestOpen model

                astar
                "#
            ),
            "Model position -> Result position [ KeyNotFound ]*",
        );
    }
}
