#[macro_use] extern crate pretty_assertions;

extern crate roc;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use roc::solve::solve_constraint;
    use roc::typ::Type::*;
    use roc::constrain::Constraint::*;
    use roc::expr::Expr::*;

    #[test]
    fn test_solve_true() {
        let expected = HashMap::new();

        assert_eq!(Ok(expected), solve_constraint(True));
    }

    #[test]
    fn test_solve_unify_basic() {
        let expected = HashMap::new();

        // TODO unify a function call.
        // TODO to do this, will nee to introduce let-bindings to put stuff in the Name Map
        // TODO since function calls are looked up by name.
        let type_to_unify:Type = ...
        let expected_type_to_unify:ExpectedType = ...

        assert_eq!(Ok(expected), solve_constraint(Unify(type_to_unify, expected_type_to_unify));
    }

//     #[test]
//     fn test_negate_number() {
//         expect_type(Type::Number, CallBuiltin(Negate, WholeNumber(5)));
//     }

//     #[test]
//     fn test_negate_float() {
//         expect_type(Type::Float, CallBuiltin(Negate, FractinalNumber(3.1)));
//     }

//     #[test]
//     fn test_negate_int_twice() {
//         expect_type(Type::Int, negate_twice(HexOctalBinary(0x12)));
//     }

//     #[test]
//     fn test_negate_number_twice() {
//         expect_type(Type::Number, negate_twice(WholeNumber(5)));
//     }

//     #[test]
//     fn test_negate_float_twice() {
//         expect_type(Type::Float, negate_twice(FractinalNumber(3.1)));
//     }


//     #[test]
//     fn test_int_literal() {
//         expect_type(Type::Int, HexOctalBinary(0x12));
//     }

//     #[test]
//     fn test_float_literal() {
//         expect_type(Type::Float, FractionalNumber(3.1));
//     }

//     #[test]
//     fn test_number_literal() {
//         expect_type(Type::Number, WholeNumber(5));
//     }

//     #[test]
//     fn add_ints_returns_int() {
//         expect_type(Type::Int, CallOperator(Plus, int(), int()));
//     }

//     #[test]
//     fn add_floats_returns_float() {
//         expect_type(Type::Float, CallOperator(Plus, float(), float()));
//     }

//     #[test]
//     fn add_nums_returns_num() {
//         expect_type(Type::Number, CallOperator(Plus, num(), num()));
//     }

//     #[test]
//     fn add_num_int_returns_int() {
//         expect_type(Type::Int, CallOperator(Plus, num(), int()));
//         expect_type(Type::Int, CallOperator(Plus, int(), num()));
//     }

//     #[test]
//     fn add_num_float_returns_float() {
//         expect_type(Type::Float, CallOperator(Plus, num(), float()));
//         expect_type(Type::Float, CallOperator(Plus, float(), num()));
//     }

//     #[test]
//     fn add_int_float_returns_mismatch() {
//         expect_mismatch(CallOperator(Plus, int(), float()));
//     }

//     fn expect_type<'a>(expected_type: Type<'a>, expr: Expr<'a>) {
//         assert_eq!(expected_type, infer_type(expr).unwrap());
//     }

//     fn expect_mismatch<'a>(expr: Expr<'a>) {
//         assert_eq!(Err(Problem::Mismatch), infer_type(expr));
//     }

//     #[inline]
//     fn negate_twice(expr) {
//         CallBuiltin(Negate, CallBuiltin(Negate, expr))
//     }

//     fn int<'a>() -> Box<&'a Expr<'a>> { Box::new(&HexOctalBinary(0x12)) }
//     fn float<'a>() -> Box<&'a Expr<'a>> { Box::new(&FractionalNumber(3.1)) }
//     fn num<'a>() -> Box<&'a Expr<'a>> { Box::new(&WholeNumber(5)) }

    // TODO test unions that ought to be equivalent, but only after
    // a reduction of some sort, e.g.
    //
    // ((a|b)|c) vs (a|(b|c))
    //
    // ((a|z)|(b|z)) vs (a|b|z)
    //
    // ideally, we fix these when constructing unions
    // e.g. if a user puts this in as an annotation, reduce it immediately
    // and when we're inferring unions, always infer them flat.
    // This way we can avoid checking recursively.
}
