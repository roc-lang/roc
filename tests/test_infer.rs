#[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate indoc;

extern crate roc;
extern crate combine;

mod helpers;

#[cfg(test)]
mod test_infer {
    use helpers::{loc, parse_without_loc};
    use roc::canonicalize::{self, Expr, Symbol, Procedure};
    use roc::expr::{Ident, VariantName};
    use roc::collections::{MutMap, ImMap};
    use roc::types::{Type, Problem};
    use roc::types::Type::*;
    use roc::subs::Content::{self, *};
    use roc::subs::{FlatType, Variable};
    use roc::subs::Subs;
    use roc::region::{Located, Region};
    use roc::infer::infer_expr;
    use roc::pretty_print_types::content_to_string;


    // HELPERS

    fn infer_eq(src: &str, expected: &str) {
        let (expr, procedures) = can_expr(src);
        let mut subs = Subs::new();

        let content = infer_expr(&mut subs, loc(expr), procedures);
        let actual_str = content_to_string(content, &mut subs);

        assert_eq!(actual_str, expected.to_string());
    }

    fn can_expr(expr_str: &str) -> (Expr, MutMap<Symbol, Procedure>) {
        can_expr_with("blah", expr_str, &ImMap::default(), &ImMap::default())
    }

    fn can_expr_with(
        name: &str,
        expr_str: &str,
        declared_idents: &ImMap<Ident, (Symbol, Region)>,
        declared_variants: &ImMap<Symbol, Located<VariantName>>,
    ) -> (Expr, MutMap<Symbol, Procedure>) {
        let (expr, unparsed) = parse_without_loc(expr_str).unwrap_or_else(|errors| {
                panic!("Parse error trying to parse \"{}\" - {}", expr_str.to_string(), errors.to_string())
        });

        assert_eq!(unparsed, "".to_string());

        let home = "Test".to_string();
        let (loc_expr, _, problems, procedures) =
            canonicalize::canonicalize_declaration(home, name, loc(expr), declared_idents, declared_variants);

        assert_eq!(problems, Vec::new());

        (loc_expr.value, procedures)
    }

    fn apply(module_name: &str, type_name: &str, args: Vec<Variable>) -> Content {
        Structure(FlatType::Apply(module_name.to_string(), type_name.to_string(), args))
    }

    fn var(num: u32) -> Variable {
        Variable::new_for_testing_only(num)
    }

    #[test]
    fn empty_record() {
        infer_eq("{}", "{}");
    }

    #[test]
    fn int_literal() {
        infer_eq("5", "Num.Num *");
    }

    #[test]
    fn fractional_literal() {
        infer_eq("0.5", "Num.Num (Num.Fractional *)");
    }

    #[test]
    fn string_literal() {
        infer_eq(
            indoc!(r#"
                "type inference!"
            "#),
            "String.String"
        );
    }

    #[test]
    fn empty_string() {
        infer_eq(
            indoc!(r#"
                ""
            "#),
            "String.String"
        );
    }


    // LIST 


    #[test]
    fn empty_list() {
        infer_eq(
            indoc!(r#"
                []
            "#),
            "List.List *"
        );
    }

    #[test]
    fn list_of_lists() {
        infer_eq(
            indoc!(r#"
                [[]]
            "#),
            "List.List (List.List *)"
        );
    }

    #[test]
    fn triple_nested_list() {
        infer_eq(
            indoc!(r#"
                [[[]]]
            "#),
            "List.List (List.List (List.List *))"
        );
    }

    #[test]
    fn nested_empty_list() {
        infer_eq(
            indoc!(r#"
                [ [], [ [] ] ]
            "#),
            "List.List (List.List (List.List *))"
        );
    }

    #[test]
    fn list_of_one_num() {
        infer_eq(
            indoc!(r#"
                [42]
            "#),
            "List.List (Num.Num *)"
        );
    }

    #[test]
    fn triple_nested_num_list() {
        infer_eq(
            indoc!(r#"
                [[[ 5 ]]]
            "#),
            "List.List (List.List (List.List (Num.Num *)))"
        );
    }

    #[test]
    fn list_of_nums() {
        infer_eq(
            indoc!(r#"
                [ 1, 2, 3 ]
            "#),
            "List.List (Num.Num *)"
        );
    }

    #[test]
    fn nested_list_of_nums() {
        infer_eq(
            indoc!(r#"
                [ [ 1 ], [ 2, 3 ] ]
            "#),
            "List.List (List.List (Num.Num *))"
        );
    }

    #[test]
    fn list_of_one_string() {
        infer_eq(
            indoc!(r#"
                [ "cowabunga" ]
            "#),
            "List.List String.String"
        );
    }

    #[test]
    fn triple_nested_string_list() {
        infer_eq(
            indoc!(r#"
                [[[ "foo" ]]]
            "#),
            "List.List (List.List (List.List String.String))"
        );
    }

    #[test]
    fn list_of_strings() {
        infer_eq(
            indoc!(r#"
                [ "foo", "bar" ]
            "#),
            "List.List String.String"
        );
    }

    // INTERPOLATED STRING

    #[test]
    fn infer_interpolated_string() {
        infer_eq(
            indoc!(r#"
                whatItIs = "great"

                "type inference is \(whatItIs)!"
            "#),
            "String.String"
        );
    }


    // LIST MISMATCH

    #[test]
    fn mismatch_heterogeneous_list() {
        infer_eq(
            indoc!(r#"
                [ "foo", 5 ]
            "#),
            "List.List <type mismatch>"
        );
    }

    #[test]
    fn mismatch_heterogeneous_nested_list() {
        infer_eq(
            indoc!(r#"
                [ [ "foo", 5 ] ]
            "#),
            "List.List (List.List <type mismatch>)"
        );
    }

    #[test]
    fn mismatch_heterogeneous_nested_empty_list() {
        infer_eq(
            indoc!(r#"
                [ [ 1 ], [ [] ] ]
            "#),
            "List.List (List.List <type mismatch>)"
        );
    }

    // CLOSURE

    #[test]
    fn always_return_empty_record() {
        infer_eq(
            indoc!(r#"
                \_ -> {}
            "#),
            "* -> {}"
        );
    }

    #[test]
    fn two_arg_return_num() {
        infer_eq(
            indoc!(r#"
                \_ _ -> 42
            "#),
            "*, * -> Num.Num *"
        );
    }

    #[test]
    fn three_arg_return_string() {
        infer_eq(
            indoc!(r#"
                \_ _ _ -> "test!"
            "#),
            "*, *, * -> String.String"
        );
    }

    // ASSIGN

    #[test]
    fn assign_empty_record() {
        infer_eq(
            indoc!(r#"
                foo = {}

                foo
            "#),
            "{}"
        );
    }

    #[test]
    fn assign_string() {
        infer_eq(
            indoc!(r#"
                str = "thing"

                str
            "#),
            "String.String"
        );
    }

    #[test]
    fn assign_1_arg_closure() {
        infer_eq(
            indoc!(r#"
                fn = \_ -> {}

                fn
            "#),
            "* -> {}"
        );
    }

    #[test]
    fn assign_2_arg_closure() {
        infer_eq(
            indoc!(r#"
                func = \_ _ -> 42

                func
            "#),
            "*, * -> Num.Num *"
        );
    }

    #[test]
    fn assign_3_arg_closure() {
        infer_eq(
            indoc!(r#"
                f = \_ _ _ -> "test!"

                f
            "#),
            "*, *, * -> String.String"
        );
    }

    #[test]
    fn assign_multiple_functions() {
        infer_eq(
            indoc!(r#"
                a = \_ _ _ -> "test!"

                b = a

                b
            "#),
            "*, *, * -> String.String"
        );
    }

    #[test]
    fn assign_multiple_strings() {
        infer_eq(
            indoc!(r#"
                a = "test!"

                b = a

                b
            "#),
            "String.String"
        );
    }

    #[test]
    fn assign_multiple_nums() {
        infer_eq(
            indoc!(r#"
                c = b

                b = a

                a = 42

                c
            "#),
            "Num.Num *"
        );
    }

    // CALLING FUNCTIONS

    #[test]
    fn call_returns_num() {
        infer_eq(
            indoc!(r#"
                alwaysFive = \_ -> 5

                alwaysFive "stuff"
            "#),
            "Num.Num *"
        );
    }

    #[test]
    fn call_returns_list() {
        infer_eq(
            indoc!(r#"
                enlist = \val -> [ val ]

                enlist 5
            "#),
            "List.List (Num.Num *)"
        );
    }

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
//             "String.String"
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
            indoc!(r#"
                \l r -> l / r
            "#),
            "Num.Num Float.FloatingPoint, Num.Num Float.FloatingPoint -> Num.Num Float.FloatingPoint"
        );
    }

    #[test]
    fn basic_division() {
        infer_eq(
            indoc!(r#"
                1 / 2
            "#),
            "Num.Num Float.FloatingPoint"
        );
    }

    // #[test]
    // fn basic_addition() {
    //     infer_eq(
    //         indoc!(r#"
    //             1 + 2
    //         "#),
    //         "Num.Num *"
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
