#[macro_use] extern crate pretty_assertions;

extern crate roc;

#[cfg(test)]
mod tests {
    use roc::solve::Type;
    use roc::solve::Type::*;
    use roc::solve::Expr;
    use roc::solve::Expr::*;
    use roc::solve::Operator;
    use roc::solve::Operator::*;
    use roc::solve::Problem;
    use roc::solve::infer_type;

    #[test]
    fn test_int_literal() {
        expect_type(Type::Int, HexOctalBinary(0x12));
    }

    #[test]
    fn test_float_literal() {
        expect_type(Type::Float, FractionalNumber(3.1));
    }

    #[test]
    fn test_number_literal() {
        expect_type(Type::Number, WholeNumber(5));
    }

    #[test]
    fn add_ints_returns_int() {
        expect_type(Type::Int, CallOperator(Plus, int(), int()));
    }

    #[test]
    fn add_floats_returns_float() {
        expect_type(Type::Float, CallOperator(Plus, float(), float()));
    }

    #[test]
    fn add_nums_returns_num() {
        expect_type(Type::Number, CallOperator(Plus, num(), num()));
    }

    #[test]
    fn add_num_int_returns_int() {
        expect_type(Type::Int, CallOperator(Plus, num(), int()));
        expect_type(Type::Int, CallOperator(Plus, int(), num()));
    }

    #[test]
    fn add_num_float_returns_float() {
        expect_type(Type::Float, CallOperator(Plus, num(), float()));
        expect_type(Type::Float, CallOperator(Plus, float(), num()));
    }

    #[test]
    fn add_int_float_returns_mismatch() {
        expect_mismatch(CallOperator(Plus, int(), float()));
    }

    fn expect_type<'a>(expected_type: Type<'a>, expr: Expr<'a>) {
        assert_eq!(expected_type, infer_type(expr).unwrap());
    }

    fn expect_mismatch<'a>(expr: Expr<'a>) {
        assert_eq!(Err(Problem::Mismatch), infer_type(expr));
    }

    fn int<'a>() -> Box<&'a Expr<'a>> { Box::new(&HexOctalBinary(0x12)) }
    fn float<'a>() -> Box<&'a Expr<'a>> { Box::new(&FractionalNumber(3.1)) }
    fn num<'a>() -> Box<&'a Expr<'a>> { Box::new(&WholeNumber(5)) }
}
