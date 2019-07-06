// #[macro_use] extern crate pretty_assertions;
extern crate combine;
extern crate fraction;

extern crate roc;

#[cfg(test)]
mod test_eval {
    // use roc::operator::Operator::*;
    // use roc::expr::Pattern::*;
    // use roc::expr::Expr::*;
    // use roc::eval::eval;
    // use roc::eval::Evaluated;
    // use fraction::Fraction;

    // #[test]
    // fn one_plus_one() {
    //     assert_eq!(
    //         eval(Operator(Box::new(Int(1)), Plus, Box::new(Int(1)))),
    //         Evaluated::Int(2)
    //     );
    // }

    // #[test]
    // fn point_one_plus_point_two() {
    //     // 0.1 + 0.2 == 0.3 THAT'S WHAT'S UP
    //     assert_eq!(
    //         eval(Operator(Box::new(Frac(1, 10)), Plus, Box::new(Frac(2, 10)))),
    //         Evaluated::Frac(Fraction::new(3u64, 10u64))
    //     );
    // }

    // #[test]
    // fn addition_reduces() {
    //     assert_eq!(
    //         eval(Operator(Box::new(Frac(1, 3)), Plus, Box::new(Frac(7, 14)))),
    //         Evaluated::Frac(Fraction::new(5u64, 6u64))
    //     );
    // }

    // #[test]
    // fn division_reduces() {
    //     assert_eq!(
    //         eval(Operator(Box::new(Frac(1, 3)), Slash, Box::new(Frac(7, 14)))),
    //         Evaluated::ApplyVariant(
    //             "Ok".to_string(),
    //             Some(vec![Evaluated::Frac(Fraction::new(2u64, 3u64))])
    //         )
    //     );
    // }

    // #[test]
    // fn division_by_zero() {
    //     assert_eq!(
    //         eval(Operator(Box::new(Frac(1, 10)), Slash, Box::new(Frac(0, 10)))),
    //         Evaluated::ApplyVariant(
    //             "Err".to_string(),
    //             Some(vec![Evaluated::ApplyVariant("DivisionByZero".to_string(), None)])
    //         )
    //     );
    // }

    // #[test]
    // fn string_interpolation() {
    //     assert_eq!(
    //         eval(
    //             Assign(Identifier("foo".to_string()), Box::new(Str("one".to_string())),
    //             Box::new(Assign(Identifier("bar".to_string()), Box::new(Str("two".to_string())),
    //             Box::new(Assign(Identifier("baz".to_string()), Box::new(Str("three".to_string())),
    //                 Box::new(InterpolatedStr(
    //                     // "hi_\(foo)_\(bar)_\(baz)_string!"
    //                     vec![
    //                         ("hi_".to_string(), "foo".to_string()),
    //                         ("_".to_string(), "bar".to_string()),
    //                         ("_".to_string(), "baz".to_string()),
    //                     ],
    //                     "_string!".to_string()
    //                 ))
    //             )))))
    //         ),
    //         Evaluated::Str("hi_one_two_three_string!".to_string())
    //     );
    // }

    // #[test]
    // fn if_else() {
    //     assert_eq!(
    //         eval(
    //             If(Box::new(ApplyVariant("True".to_string(), None)),
    //                 Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
    //                 Box::new(Operator(Box::new(Int(4)), Plus, Box::new(Int(5))))
    //             )
    //         ),
    //         Evaluated::Int(3)
    //     );

    //     assert_eq!(
    //         eval(
    //             If(Box::new(ApplyVariant("False".to_string(), None)),
    //                 Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
    //                 Box::new(Operator(Box::new(Int(4)), Plus, Box::new(Int(5))))
    //             )
    //         ),
    //         Evaluated::Int(9)
    //     );
    // }
}
