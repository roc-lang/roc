#![feature(box_syntax, box_patterns)]

#[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate combine;

extern crate roc;

#[cfg(test)]
mod tests {
    #![feature(box_syntax, box_patterns)]

    use roc::expr::Expr::*;
    use roc::expr::Operator::*;
    use roc::parse;
    use combine::{Parser};

    #[test]
    fn parse_positive_int() {
        assert_eq!(Ok((Int(1234), "")), parse::number_literal().parse("1234"));
    }

    #[test]
    fn parse_negative_int() {
        assert_eq!(Ok((Int(-1234), "")), parse::number_literal().parse("-1234"));
    }

    #[test]
    fn parse_positive_ratio() {
        assert_eq!(Ok((Ratio(12345, 100), "")), parse::number_literal().parse("123.45"));
        assert_eq!(Ok((Ratio(4200, 100), "")), parse::number_literal().parse("42.00"));
    }

    #[test]
    fn parse_negative_ratio() {
        assert_eq!(Ok((Ratio(-1234567, 1000), "")), parse::number_literal().parse("-1234.567"));
        assert_eq!(Ok((Ratio(-1920, 10), "")), parse::number_literal().parse("-192.0"));
    }

    #[test]
    fn parse_ints_with_spaces() {
        assert_eq!(Ok((Int(987654321), "")), parse::number_literal().parse("987 6 5 432 1"));
        assert_eq!(Ok((Int(-1234567890), "")), parse::number_literal().parse("-1 234 567 890"));
    }

    #[test]
    fn parse_ratios_with_spaces() {
        assert_eq!(Ok((Ratio(-1234567, 1000), "")), parse::number_literal().parse("-1 23 4.567"));
        assert_eq!(Ok((Ratio(-1920, 10), "")), parse::number_literal().parse("-19 2.0"));
        assert_eq!(Ok((Ratio(12345, 100), "")), parse::number_literal().parse("1 2 3.45"));
        assert_eq!(Ok((Ratio(4200, 100), "")), parse::number_literal().parse("4 2.00"));
    }

    #[test]
    fn parse_single_operator() {
        match parse::expr().parse("1234 + 567") {
            Ok((CallOperator(v1, op, v2), "")) => {
                assert_eq!(*v1, Int(1234));
                assert_eq!(op, Plus);
                assert_eq!(*v2, Int(567));
            },
            _ => panic!("Expression didn't parse"),
        }
    }

    #[test]
    fn parse_multiple_operators() {
        #![feature(box_syntax, box_patterns)]
        match parse::expr().parse("1 + 2 * 3") {
            Ok((CallOperator(box v1, op1, box CallOperator(box v2, op2, box v3)), "")) => {
                assert_eq!(v1, Int(1));
                assert_eq!(op1, Plus);
                assert_eq!(v2, Int(2));
                assert_eq!(op2, Star);
                assert_eq!(v3, Int(3));
            },
            _ => panic!("Expression didn't parse"),
        }
    }
}
