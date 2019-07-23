#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod test_canonicalize {

    // OPERATOR PRECEDENCE

    // fn parse_with_precedence(input: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>> {
    //     parse_without_loc(input)
    //         .map(|(expr, remaining)| (expr::apply_precedence_and_associativity(loc(expr)).unwrap().value, remaining))
    // }

    // #[test]
    // fn two_operator_precedence() {
    //     assert_eq!(
    //         parse_with_precedence("x + y * 5"),
    //         Ok((Operator(
    //                 loc_box(var("x")),
    //                 loc(Plus),
    //                 loc_box(
    //                     Operator(
    //                         loc_box(var("y")),
    //                         loc(Star),
    //                         loc_box(Int(5))
    //                     )
    //                 ),
    //             ),
    //         ""))
    //     );

    //     assert_eq!(
    //         parse_with_precedence("x * y + 5"),
    //         Ok((Operator(
    //                 loc_box(
    //                     Operator(
    //                         loc_box(var("x")),
    //                         loc(Star),
    //                         loc_box(var("y")),
    //                     )
    //                 ),
    //                 loc(Plus),
    //                 loc_box(Int(5))
    //             ),
    //         ""))
    //     );
    // }

    // #[test]
    // fn compare_and() {
    //     assert_eq!(
    //         parse_with_precedence("x > 1 || True"),
    //         Ok((Operator(
    //                 loc_box(
    //                     Operator(
    //                         loc_box(var("x")),
    //                         loc(GreaterThan),
    //                         loc_box(Int(1))
    //                     )
    //                 ),
    //                 loc(Or),
    //                 loc_box(ApplyVariant(vname("True"), None))
    //             ),
    //         ""))
    //     );
    // }
}