#![feature(box_syntax, box_patterns)]

#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod tests {
    #![feature(box_syntax, box_patterns)]

    use roc::expr::Expr::*;
    use roc::expr::Expr;
    use roc::expr::Operator::*;
    use roc::parse;
    use combine::{Parser};

    // STRING LITERALS

    fn expect_parsed_str<'a>(expected_str: &'a str, actual_str: &'a str) {
        let expected = expected_str.to_string();

        assert_eq!(Ok((String(expected), "")), parse::string_literal().parse(actual_str));
    }

    #[test]
    fn parse_empty_string() {
        expect_parsed_str("", "\"\"");
    }

    #[test]
    fn parse_string_without_escape() {
        expect_parsed_str("a", "\"a\"");
        expect_parsed_str("ab", "\"ab\"");
        expect_parsed_str("abc", "\"abc\"");
        expect_parsed_str("123", "\"123\"");
        expect_parsed_str("abc123", "\"abc123\"");
        expect_parsed_str("123abc", "\"123abc\"");
        expect_parsed_str("123 abc 456 def", "\"123 abc 456 def\"");
    }

    #[test]
    fn parse_string_with_special_escapes() {
        expect_parsed_str("x\\x", "\"x\\\\x\"");
        expect_parsed_str("x\"x", "\"x\\\"x\"");
        expect_parsed_str("x\tx", "\"x\\tx\"");
        expect_parsed_str("x\rx", "\"x\\rx\"");
        expect_parsed_str("x\nx", "\"x\\nx\"");
    }

    #[test]
    fn parse_string_with_unicode_escapes() {
        expect_parsed_str("x\u{00A0}x", "\"x\\u{00A0}x\"");
        expect_parsed_str("x\u{101010}x", "\"x\\u{101010}x\"");
    }

    // NUMBER LITERALS
    
    fn expect_parsed_int<'a>(expected: i64, actual: &str) {
        assert_eq!(Ok((Int(expected), "")), parse::number_literal().parse(actual));
    }

    fn expect_parsed_ratio<'a>(expected_numerator: i64, expected_denominator: u64, actual: &str) {
        assert_eq!(Ok((Ratio(expected_numerator, expected_denominator), "")), parse::number_literal().parse(actual));
    }

    #[test]
    fn parse_positive_int() {
        expect_parsed_int(1234, "1234");
    }

    #[test]
    fn parse_negative_int() {
        expect_parsed_int(-1234, "-1234");
    }

    #[test]
    fn parse_positive_ratio() {
        expect_parsed_ratio(12345, 100, "123.45");
        expect_parsed_ratio(4200, 100, "42.00");
    }

    #[test]
    fn parse_negative_ratio() {
        expect_parsed_ratio(-1234567, 1000, "-1234.567");
        expect_parsed_ratio(-1920, 10, "-192.0");
    }

    #[test]
    fn parse_ints_with_spaces() {
        expect_parsed_int(987654321, "987 6 5 432 1");
        expect_parsed_int(-1234567890, "-1 234 567 890");
    }

    #[test]
    fn parse_ratios_with_spaces() {
        expect_parsed_ratio(-1234567, 1000, "-1 23 4.567");
        expect_parsed_ratio(-1920, 10, "-19 2.0");
        expect_parsed_ratio(12345, 100, "1 2 3.45");
        expect_parsed_ratio(4200, 100, "4 2.00");
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
