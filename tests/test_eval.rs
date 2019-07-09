// #[macro_use] extern crate pretty_assertions;
extern crate combine;
extern crate fraction;

extern crate roc;

#[cfg(test)]
mod test_eval {
    use roc::operator::Operator::*;
    use roc::expr::Pattern::*;
    use roc::expr::Expr::*;
    use roc::expr::Expr;
    use roc::eval;
    use roc::eval::Evaluated;
    use roc::region::{Located, Region};
    use fraction::Fraction;

    fn loc_box<T>(val: T) -> Box<Located<T>> {
        Box::new(loc(val))
    }

    fn eval(expr: Expr) -> Evaluated {
        eval::eval(loc(expr))
    }

    fn loc<T>(val: T) -> Located<T> {
        Located::new(val, Region {
            start_line: 0,
            start_col: 0,

            end_line: 0,
            end_col: 0,
        })
    }

    #[test]
    fn one_plus_one() {
        assert_eq!(
            eval(Operator(loc_box(Int(1)), loc(Plus), loc_box(Int(1)))),
            Evaluated::Int(2)
        );
    }

    #[test]
    fn point_one_plus_point_two() {
        // 0.1 + 0.2 == 0.3 THAT'S WHAT'S UP
        assert_eq!(
            eval(Operator(loc_box(Frac(1, 10)), loc(Plus), loc_box(Frac(2, 10)))),
            Evaluated::Frac(Fraction::new(3u64, 10u64))
        );
    }

    #[test]
    fn addition_reduces() {
        assert_eq!(
            eval(Operator(loc_box(Frac(1, 3)), loc(Plus), loc_box(Frac(7, 14)))),
            Evaluated::Frac(Fraction::new(5u64, 6u64))
        );
    }

    #[test]
    fn division_reduces() {
        assert_eq!(
            eval(Operator(loc_box(Frac(1, 3)), loc(Slash), loc_box(Frac(7, 14)))),
            Evaluated::ApplyVariant(
                "Ok".to_string(),
                Some(vec![Evaluated::Frac(Fraction::new(2u64, 3u64))])
            )
        );
    }

    #[test]
    fn division_by_zero() {
        assert_eq!(
            eval(Operator(loc_box(Frac(1, 10)), loc(Slash), loc_box(Frac(0, 10)))),
            Evaluated::ApplyVariant(
                "Err".to_string(),
                Some(vec![Evaluated::ApplyVariant("DivisionByZero".to_string(), None)])
            )
        );
    }

    #[test]
    fn string_interpolation() {
        assert_eq!(
            eval(
                Assign(loc(Identifier("foo".to_string())), loc_box(Str("one".to_string())),
                loc_box(Assign(loc(Identifier("bar".to_string())), loc_box(Str("two".to_string())),
                loc_box(Assign(loc(Identifier("baz".to_string())), loc_box(Str("three".to_string())),
                    loc_box(InterpolatedStr(
                        // "hi_\(foo)_\(bar)_\(baz)_string!"
                        vec![
                            ("hi_".to_string(), loc("foo".to_string())),
                            ("_".to_string(), loc("bar".to_string())),
                            ("_".to_string(), loc("baz".to_string())),
                        ],
                        "_string!".to_string()
                    ))
                )))))
            ),
            Evaluated::Str("hi_one_two_three_string!".to_string())
        );
    }

    #[test]
    fn if_else() {
        assert_eq!(
            eval(
                If(loc_box(ApplyVariant("True".to_string(), None)),
                    loc_box(Operator(loc_box(Int(1)), loc(Plus), loc_box(Int(2)))),
                    loc_box(Operator(loc_box(Int(4)), loc(Plus), loc_box(Int(5))))
                )
            ),
            Evaluated::Int(3)
        );

        assert_eq!(
            eval(
                If(loc_box(ApplyVariant("False".to_string(), None)),
                    loc_box(Operator(loc_box(Int(1)), loc(Plus), loc_box(Int(2)))),
                    loc_box(Operator(loc_box(Int(4)), loc(Plus), loc_box(Int(5))))
                )
            ),
            Evaluated::Int(9)
        );
    }
}
