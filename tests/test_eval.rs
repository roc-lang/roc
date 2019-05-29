#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod eval_tests {
    use roc::expr::{Expr};
    use roc::expr::Expr::*;
    use roc::expr::Operator::*;

    fn eval(expr: Expr) -> Expr {
        roc::eval::from_evaluated(
            roc::eval::eval(expr)
        )
    }

    #[test]
    fn one_plus_one() {
        assert_eq!(
            eval(Operator(Box::new(Int(1)), Plus, Box::new(Int(1)))),
            Int(2)
        );
    }

    #[test]
    fn if_else() {
        assert_eq!(
            eval(
                If(Box::new(Bool(true)),
                    Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
                    Box::new(Operator(Box::new(Int(4)), Plus, Box::new(Int(5))))
                )
            ),
            Int(3)
        );

        assert_eq!(
            eval(
                If(Box::new(Bool(false)),
                    Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
                    Box::new(Operator(Box::new(Int(4)), Plus, Box::new(Int(5))))
                )
            ),
            Int(9)
        );
    }
}
