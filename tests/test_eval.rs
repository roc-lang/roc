#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod test_eval {
    use roc::expr::Operator::*;
    use roc::expr::Pattern::*;
    use roc::expr::Expr::*;
    use roc::eval::eval;
    use roc::eval::Evaluated;

    #[test]
    fn one_plus_one() {
        assert_eq!(
            eval(Operator(Box::new(Int(1)), Plus, Box::new(Int(1)))),
            Evaluated::Int(2)
        );
    }

    #[test]
    fn string_interpolation() {
        assert_eq!(
            eval(
                Assign(Identifier("foo".to_string()), Box::new(Str("one".to_string())),
                Box::new(Assign(Identifier("bar".to_string()), Box::new(Str("two".to_string())),
                Box::new(Assign(Identifier("baz".to_string()), Box::new(Str("three".to_string())),
                    Box::new(InterpolatedStr(
                        // "hi_\(foo)_\(bar)_\(baz)_string!"
                        vec![
                            ("hi_".to_string(), "foo".to_string()),
                            ("_".to_string(), "bar".to_string()),
                            ("_".to_string(), "baz".to_string()),
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
                If(Box::new(ApplyVariant("True".to_string(), None)),
                    Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
                    Box::new(Operator(Box::new(Int(4)), Plus, Box::new(Int(5))))
                )
            ),
            Evaluated::Int(3)
        );

        assert_eq!(
            eval(
                If(Box::new(ApplyVariant("False".to_string(), None)),
                    Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
                    Box::new(Operator(Box::new(Int(4)), Plus, Box::new(Int(5))))
                )
            ),
            Evaluated::Int(9)
        );
    }
}
