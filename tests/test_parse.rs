#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc;

extern crate quickcheck;

#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod helpers;

#[cfg(test)]
mod test_parse {
    use bumpalo::Bump;
    use helpers::parse_with;
    use roc::operator::Operator::*;
    use roc::parse::ast::Attempting;
    use roc::parse::ast::Expr::{self, *};
    use roc::parse::parser::{Fail, FailReason};
    use roc::region::{Located, Region};
    use std::{f64, i64};

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let arena = Bump::new();
        let actual = parse_with(&arena, input);

        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_parsing_fails<'a>(input: &'a str, reason: FailReason, attempting: Attempting) {
        let arena = Bump::new();
        let actual = parse_with(&arena, input);
        let expected_fail = Fail { reason, attempting };

        assert_eq!(Err(expected_fail), actual);
    }

    // STRING LITERALS

    fn expect_parsed_str(input: &str, expected: &str) {
        assert_parses_to(expected, Str(input.into()));
    }

    #[test]
    fn empty_string() {
        assert_parses_to(
            indoc!(
                r#"
                ""
                "#
            ),
            EmptyStr,
        );
    }

    #[test]
    fn one_char_string() {
        assert_parses_to(
            indoc!(
                r#"
                "x"
                "#
            ),
            Str("x".into()),
        );
    }

    #[test]
    fn multi_char_string() {
        assert_parses_to(
            indoc!(
                r#"
                "foo"
                "#
            ),
            Str("foo".into()),
        );
    }

    #[test]
    fn string_without_escape() {
        expect_parsed_str("a", r#""a""#);
        expect_parsed_str("ab", r#""ab""#);
        expect_parsed_str("abc", r#""abc""#);
        expect_parsed_str("123", r#""123""#);
        expect_parsed_str("abc123", r#""abc123""#);
        expect_parsed_str("123abc", r#""123abc""#);
        expect_parsed_str("123 abc 456 def", r#""123 abc 456 def""#);
    }

    #[test]
    fn string_with_special_escapes() {
        expect_parsed_str(r#"x\\x"#, r#""x\\x""#);
        expect_parsed_str(r#"x\"x"#, r#""x\"x""#);
        expect_parsed_str(r#"x\tx"#, r#""x\tx""#);
        expect_parsed_str(r#"x\rx"#, r#""x\rx""#);
        expect_parsed_str(r#"x\nx"#, r#""x\nx""#);
    }

    #[test]
    fn string_with_single_quote() {
        // This shoud NOT be escaped in a string.
        expect_parsed_str("x'x", r#""x'x""#);
    }

    #[test]
    fn empty_source_file() {
        assert_parsing_fails("", FailReason::Eof(Region::zero()), Attempting::Expression);
    }

    #[test]
    fn first_line_too_long() {
        let max_line_length = std::u16::MAX as usize;

        // the string literal "ZZZZZZZZZ" but with way more Zs
        let too_long_str_body: String = (1..max_line_length)
            .into_iter()
            .map(|_| "Z".to_string())
            .collect();
        let too_long_str = format!("\"{}\"", too_long_str_body);

        // Make sure it's longer than our maximum line length
        assert_eq!(too_long_str.len(), max_line_length + 1);

        assert_parsing_fails(
            &too_long_str,
            FailReason::LineTooLong(0),
            Attempting::Expression,
        );
    }

    // INT LITERALS

    #[test]
    fn zero_int() {
        assert_parses_to("0", Int("0"));
    }

    #[test]
    fn positive_int() {
        assert_parses_to("1", Int("1"));
        assert_parses_to("42", Int("42"));
    }

    #[test]
    fn negative_int() {
        assert_parses_to("-1", Int("-1"));
        assert_parses_to("-42", Int("-42"));
    }

    #[test]
    fn highest_int() {
        assert_parses_to(
            i64::MAX.to_string().as_str(),
            Int(i64::MAX.to_string().as_str()),
        );
    }

    #[test]
    fn lowest_int() {
        assert_parses_to(
            i64::MIN.to_string().as_str(),
            Int(i64::MIN.to_string().as_str()),
        );
    }

    #[test]
    fn int_with_underscore() {
        assert_parses_to("1_2_34_567", Int("1_2_34_567"));
        assert_parses_to("-1_2_34_567", Int("-1_2_34_567"));
        // The following cases are silly. They aren't supported on purpose,
        // but there would be a performance cost to explicitly disallowing them,
        // which doesn't seem like it would benefit anyone.
        assert_parses_to("1_", Int("1_"));
        assert_parses_to("1__23", Int("1__23"));
    }

    #[quickcheck]
    fn all_i64_values_parse(num: i64) {
        assert_parses_to(num.to_string().as_str(), Int(num.to_string().as_str()));
    }

    // FLOAT LITERALS

    #[test]
    fn zero_float() {
        assert_parses_to("0.0", Float("0.0"));
    }

    #[test]
    fn positive_float() {
        assert_parses_to("1.0", Float("1.0"));
        assert_parses_to("1.1", Float("1.1"));
        assert_parses_to("42.0", Float("42.0"));
        assert_parses_to("42.9", Float("42.9"));
    }

    #[test]
    fn negative_float() {
        assert_parses_to("-1.0", Float("-1.0"));
        assert_parses_to("-1.1", Float("-1.1"));
        assert_parses_to("-42.0", Float("-42.0"));
        assert_parses_to("-42.9", Float("-42.9"));
    }

    #[test]
    fn float_with_underscores() {
        assert_parses_to("1_23_456.0_1_23_456", Float("1_23_456.0_1_23_456"));
        assert_parses_to("-1_23_456.0_1_23_456", Float("-1_23_456.0_1_23_456"));
    }

    #[test]
    fn highest_float() {
        let string = format!("{}.0", f64::MAX);

        assert_parses_to(&string, Float(&string));
    }

    #[test]
    fn lowest_float() {
        let string = format!("{}.0", f64::MIN);

        assert_parses_to(&string, Float(&string));
    }

    #[quickcheck]
    fn all_f64_values_parse(num: f64) {
        assert_parses_to(num.to_string().as_str(), Float(num.to_string().as_str()));
    }

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        assert_parses_to("{}", EmptyRecord);
    }

    // OPERATORS

    #[test]
    fn one_plus_two() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("1")),
            Located::new(0, 1, 0, 2, Plus),
            Located::new(0, 2, 0, 3, Int("2")),
        ));
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "1+2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn minus_twelve_minus_five() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 3, Int("-12")),
            Located::new(0, 3, 0, 4, Minus),
            Located::new(0, 4, 0, 5, Int("5")),
        ));
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "-12-5");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn ten_times_eleven() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 2, Int("10")),
            Located::new(0, 2, 0, 3, Star),
            Located::new(0, 3, 0, 5, Int("11")),
        ));
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "10*11");

        assert_eq!(Ok(expected), actual);
    }

    // TODO test hex/oct/binary parsing
    //
    // TODO test for \t \r and \n in string literals *outside* unicode escape sequence!
    //
    // TODO verify that when a string literal contains a newline before the
    // closing " it correctly updates both the line *and* column in the State.
}
