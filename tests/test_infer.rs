#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer {
    use helpers::can_expr;
    // use roc::can::symbol::Symbol;
    // use roc::ident::{Ident, VariantName};
    use roc::infer::infer_expr;
    use roc::pretty_print_types::content_to_string;
    use roc::region::Located;
    // use roc::subs::Content::{self, *};
    use roc::subs::Subs;
    // use roc::subs::{FlatType, Variable};
    // use roc::types::Type::*;
    // use roc::types::{Problem, Type};

    // HELPERS

    fn infer_eq(src: &str, expected: &str) {
        let (expr, _, _, procedures) = can_expr(src);
        let mut subs = Subs::new();

        let content = infer_expr(&mut subs, Located::new(0, 0, 0, 0, expr), procedures);
        let actual_str = content_to_string(content, &mut subs);

        assert_eq!(actual_str, expected.to_string());
    }

    // fn apply(module_name: &str, type_name: &str, args: Vec<Variable>) -> Content {
    //     Structure(FlatType::Apply(
    //         module_name.to_string(),
    //         type_name.to_string(),
    //         args,
    //     ))
    // }

    //     fn var(num: u32) -> Variable {
    //         Variable::new_for_testing_only(num)
    //     }

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
                r#"# reset indentation
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
                r#"# reset indentation
                str = "thing"

                str
            "#
            ),
            "Str",
        );
    }

    // #[test]
    // fn def_1_arg_closure() {
    //     infer_eq(
    //         indoc!(
    //             r#"# reset indentation
    //             fn = \_ -> {}

    //             fn
    //         "#
    //         ),
    //         "* -> {}",
    //     );
    // }

    // #[test]
    // fn def_2_arg_closure() {
    //     infer_eq(
    //         indoc!(
    //             r#"# reset indentation
    //             func = \_ _ -> 42

    //             func
    //         "#
    //         ),
    //         "*, * -> Int",
    //     );
    // }

    // #[test]
    // fn def_3_arg_closure() {
    //     infer_eq(
    //         indoc!(
    //             r#"# reset indentation
    //             f = \_ _ _ -> "test!"

    //             f
    //         "#
    //         ),
    //         "*, *, * -> Str",
    //     );
    // }

    // #[test]
    // fn def_multiple_functions() {
    //     infer_eq(
    //         indoc!(
    //             r#"# reset indentation
    //             a = \_ _ _ -> "test!"

    //             b = a

    //             b
    //         "#
    //         ),
    //         "*, *, * -> Str",
    //     );
    // }

    #[test]
    fn def_multiple_strings() {
        infer_eq(
            indoc!(
                r#"# reset indentation
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
                r#"# reset indentation
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

    // #[test]
    // fn call_returns_int() {
    //     infer_eq(
    //         indoc!(
    //             r#"# reset indentation
    //             alwaysFive = \_ -> 5

    //             alwaysFive "stuff"
    //         "#
    //         ),
    //         "Int",
    //     );
    // }

    // #[test]
    // fn call_returns_list() {
    //     infer_eq(
    //         indoc!(
    //             r#"# reset indentation
    //             enlist = \val -> [ val ]

    //             enlist 5
    //         "#
    //         ),
    //         "List Int",
    //     );
    // }

    // TODO type annotations
    // TODO fix identity inference
    // TODO BoundTypeVariables
    // TODO conditionals

    //     #[test]
    //     fn indirect_always() {
    //         infer_eq(
    //             indoc!(r#"
    //                 always = \val -> (\_ -> val)
    //                 alwaysFoo = always "foo"

    //                 alwaysFoo 42
    //             "#),
    //             "Str"
    //         );
    //     }

    //     #[test]
    //     fn identity() {
    //         infer_eq(
    //             indoc!(r#"
    //                 \val -> val
    //             "#),
    //             "a -> a"
    //         );
    //     }

    //     #[test]
    //     fn always_function() {
    //         infer_eq(
    //             indoc!(r#"
    //                 \val -> \_ -> val
    //             "#),
    //             "a -> (* -> a)"
    //         );
    //     }

    // OPERATORS

    #[test]
    fn div_operator() {
        infer_eq(
            indoc!(
                r#"
                \l r -> l / r
            "#
            ),
            "Float, Float -> Float",
        );
    }

    #[test]
    fn basic_float_division() {
        infer_eq(
            indoc!(
                r#"
                1 / 2
            "#
            ),
            "Float",
        );
    }

    #[test]
    fn basic_int_division() {
        infer_eq(
            indoc!(
                r#"
                1 // 2
            "#
            ),
            "Int",
        );
    }

    // #[test]
    // fn basic_addition() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             1 + 2
    //         "#
    //         ),
    //         "Num *",
    //     );
    // }

    // #[test]
    // fn basic_circular_type() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \x -> x x
    //         "#)),
    //         Erroneous(Problem::CircularType)
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
    //        infer_eq(
    //        indoc!(r#"
    //            alwaysFive = \_ -> 5

    //            [ alwaysFive "foo", alwaysFive [] ]
    //        "#),
    //        "<type mismatch>"
    //    );
    // }

    // #[test]
    // fn infer_basic_case() {
    //     assert_eq!(
    //         infer("case 42 when 1 then 2.5 when _ then 3.5"),
    //         Builtin(Frac)
    //     );
    // }
}
