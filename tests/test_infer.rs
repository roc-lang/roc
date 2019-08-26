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
    use roc::types::Builtin::*;
    use roc::subs::Content::{self, *};
    use roc::subs::FlatType;
    use roc::region::{Located, Region};
    use roc::infer::infer_expr;


    // HELPERS

    fn infer(src: &str) -> Content {
        let (expr, procedures) = can_expr(src);

        infer_expr(loc(expr), procedures)
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

    // fn box_var(var_id: TypeVarId) -> Box<Type> {
    //     Box::new(TypeVar(var_id))
    // }

    // fn var(var_id: TypeVarId) -> Type {
    //     TypeVar(var_id)
    // }

    #[test]
    fn infer_empty_record() {
        assert_eq!(
            infer("{}"),
            Structure(FlatType::EmptyRecord)
        );
    }

    // #[test]
    // fn infer_int() {
    //     assert_eq!(
    //         infer("5"),
    //         Structure(FlatType::EmptyRecord)
    //     );
    // }

    // #[test]
    // fn infer_frac() {
    //     assert_eq!(
    //         infer("0.5"),
    //         Builtin(Frac)
    //     );
    // }

    // #[test]
    // fn infer_approx() {
    //     assert_eq!(
    //         infer("~0.5"),
    //         Builtin(Approx)
    //     );
    // }

    // #[test]
    // fn infer_string() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             "type inference!"
    //         "#)),
    //         Builtin(Str)
    //     );
    // }

    // #[test]
    // fn infer_empty_string() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             ""
    //         "#)),
    //         Builtin(Str)
    //     );
    // }

    // #[test]
    // fn infer_interpolated_string() {
    //     assert_eq!(
    //         infer_expr(&Expr::InterpolatedStr(vec![], "type inference!".to_string()), MutMap::default()),
    //         Builtin(Str)
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
