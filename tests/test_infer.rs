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

        assert_eq!(problems, vec![]);

        (loc_expr.value, procedures)
    }

    fn apply(module_name: &str, type_name: &str, args: Vec<Variable>) -> Content {
        Structure(FlatType::Apply(module_name.to_string(), type_name.to_string(), args))
    }

    fn var(num: u32) -> Variable {
        Variable::new_for_testing_only(num)
    }

    #[test]
    fn infer_empty_record() {
        infer_eq("{}", "{}");
    }

    #[test]
    fn infer_int() {
        infer_eq("5", "Num.Num *");
    }

    #[test]
    fn infer_fractional() {
        infer_eq("0.5", "Num.Num (Num.Fractional *)");
    }

    #[test]
    fn infer_string() {
        infer_eq(
            indoc!(r#"
                "type inference!"
            "#),
            "String.String"
        );
    }

    #[test]
    fn infer_empty_string() {
        infer_eq(
            indoc!(r#"
                ""
            "#),
            "String.String"
        );
    }


    // LIST 


    #[test]
    fn infer_empty_list() {
        infer_eq(
            indoc!(r#"
                []
            "#),
            "List.List *"
        );
    }

    #[test]
    fn infer_list_of_one_num() {
        infer_eq(
            indoc!(r#"
                [42]
            "#),
            "List.List (Num.Num *)"
        );
    }

    #[test]
    fn infer_list_of_nums() {
        infer_eq(
            indoc!(r#"
                [ 1, 2, 3 ]
            "#),
            "List.List (Num.Num *)"
        );
    }

    #[test]
    fn infer_list_of_one_string() {
        infer_eq(
            indoc!(r#"
                [ "cowabunga" ]
            "#),
            "List.List String.String"
        );
    }

    #[test]
    fn infer_list_of_strings() {
        infer_eq(
            indoc!(r#"
                [ "foo", "bar" ]
            "#),
            "List.List String.String"
        );
    }


    // #[test]
    // fn infer_interpolated_string() {
    //     infer_eq(
    //         indoc!(r#"
    //             whatItIs = "great"

    //             "type inference is \(whatItIs)!"
    //         "#),
    //         "String.String"
    //     );
    // }

    // #[test]
    // fn int_thunk() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \_ -> 5
    //         "#)),
    //         Function(vec![var(0)], Box::new(Builtin(Int)))
    //     );
    // }

    // #[test]
    // fn string_thunk() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \_ -> "thunk!"
    //         "#)),
    //         Function(vec![var(0)], Box::new(Builtin(Str)))
    //     );
    // }


    // #[test]
    // fn identity_function() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \val -> val
    //         "#)),
    //         Function(vec![var(0)], box_var(0))
    //     );
    // }

    // #[test]
    // fn always_function() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \val -> \_ -> val
    //         "#)),
    //         Function(
    //             vec![var(0)],
    //             Box::new(Function(vec![var(1)], box_var(0)))
    //         )
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
    // fn infer_multiply() {
    //     assert_eq!(
    //         infer("2 * 3"),
    //         Builtin(Int)
    //     );

    //     assert_eq!(
    //         infer("2.5 * 3.5"),
    //         Builtin(Frac)
    //     );
    // }

    // #[test]
    // fn infer_basic_case() {
    //     assert_eq!(
    //         infer("case 42 when 1 then 2.5 when _ then 3.5"),
    //         Builtin(Frac)
    //     );
    // }
}
