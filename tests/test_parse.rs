#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate combine; // OBSOLETE
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_parser {
    use bumpalo::Bump;
    use helpers::located;
    use roc::parse;
    use roc::parse::ast::Attempting;
    use roc::parse::ast::Expr::{self, *};
    use roc::parse::parser::{Parser, State};
    use roc::parse::problems::Problem;
    use roc::region::Located;

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let state = State::from_input(&input);
        let arena = Bump::new();
        let parser = parse::expr();
        let answer = parser.parse(&arena, &state, Attempting::Expression);
        let actual = answer.map(|(_, expr)| expr);

        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_malformed_str<'a>(input: &'a str, expected_probs: Vec<Located<Problem>>) {
        let state = State::from_input(&input);
        let arena = Bump::new();
        let parser = parse::expr();
        let answer = parser.parse(&arena, &state, Attempting::Expression);
        let actual = answer.map(|(_, expr)| expr);

        assert_eq!(
            Ok(Expr::MalformedStr(expected_probs.into_boxed_slice())),
            actual
        );
    }

    /* fn raw(string: &str) -> Ident { */
    /*     Ident::Unqualified(string.to_string()) */
    /* } */

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
    fn one_char_list() {
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
    fn multi_char_list() {
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
        expect_parsed_str(r#"x\x"#, r#""x\\x""#);
        expect_parsed_str(r#"x"x"#, r#""x\"x""#);
        expect_parsed_str("x\tx", r#""x\tx""#);
        expect_parsed_str("x\rx", r#""x\rx""#);
        expect_parsed_str("x\nx", r#""x\nx""#);
    }

    #[test]
    fn string_with_escaped_interpolation() {
        assert_parses_to(
            // This should NOT be string interpolation, because of the \\
            indoc!(
                r#"
                "abcd\\(efg)hij"
                "#
            ),
            Str(r#"abcd\(efg)hij"#.into()),
        );
    }

    #[test]
    fn string_with_single_quote() {
        // This shoud NOT be escaped in a string.
        expect_parsed_str("x'x", r#""x'x""#);
    }

    #[test]
    fn string_with_valid_unicode_escapes() {
        expect_parsed_str("x\u{00A0}x", r#""x\u{00A0}x""#);
        expect_parsed_str("x\u{101010}x", r#""x\u{101010}x""#);
    }

    #[test]
    fn string_with_too_large_unicode_escape() {
        // Should be too big - max size should be 10FFFF.
        // (Rust has this restriction. I assume it's a good idea.)
        assert_malformed_str(
            r#""abc\u{110000}def""#,
            vec![located(0, 7, 0, 12, Problem::UnicodeCodePointTooLarge)],
        );
    }

    #[test]
    fn string_with_no_unicode_digits() {
        // No digits specified
        assert_malformed_str(
            r#""blah\u{}foo""#,
            vec![located(0, 5, 0, 8, Problem::NoUnicodeDigits)],
        );
    }

    #[test]
    fn string_with_no_unicode_opening_brace() {
        // No opening curly brace. It can't be sure if the closing brace
        // was intended to be a closing brace for the unicode escape, so
        // report that there were no digits specified.
        assert_malformed_str(
            r#""abc\u00A0}def""#,
            vec![located(0, 4, 0, 5, Problem::NoUnicodeDigits)],
        );
    }

    #[test]
    fn string_with_no_unicode_closing_brace() {
        // No closing curly brace
        assert_malformed_str(
            r#""blah\u{stuff""#,
            vec![located(0, 5, 0, 12, Problem::MalformedEscapedUnicode)],
        );
    }

    #[test]
    fn string_with_no_unicode_braces() {
        // No curly braces
        assert_malformed_str(
            r#""zzzz\uzzzzz""#,
            vec![located(0, 5, 0, 6, Problem::NoUnicodeDigits)],
        );
    }

    /* #[test] */
    /* fn string_with_interpolation_at_start() { */
    /*     assert_fully_parses( */
    /*         indoc!( */
    /*             r#" */
    /*             "\(abc)defg" */
    /*             "# */
    /*         ), */
    /*         InterpolatedStr(vec![("".to_string(), loc(raw("abc")))], "defg".to_string()), */
    /*     ); */
    /* } */

    /* #[test] */
    /* fn string_with_interpolation_at_end() { */
    /*     assert_fully_parses( */
    /*         indoc!( */
    /*             r#" */
    /*             "abcd\(efg)" */
    /*         "# */
    /*         ), */
    /*         InterpolatedStr(vec![("abcd".to_string(), loc(raw("efg")))], "".to_string()), */
    /*     ); */
    /* } */

    /* #[test] */
    /* fn string_with_interpolation_in_middle() { */
    /*     assert_fully_parses( */
    /*         indoc!( */
    /*             r#" */
    /*             "abcd\(efg)hij" */
    /*         "# */
    /*         ), */
    /*         InterpolatedStr( */
    /*             vec![("abcd".to_string(), loc(raw("efg")))], */
    /*             "hij".to_string(), */
    /*         ), */
    /*     ); */
    /* } */

    /* #[test] */
    /* fn string_with_multiple_interpolation() { */
    /* panic!("TODO start, middle, middle again, *and*, end"); */
    /*     assert_fully_parses( */
    /*         indoc!( */
    /*             r#" */
    /*             "abcd\(efg)hij" */
    /*         "# */
    /*         ), */
    /*         InterpolatedStr( */
    /*             vec![("abcd".to_string(), loc(raw("efg")))], */
    /*             "hij".to_string(), */
    /*         ), */
    /*     ); */
    /* } */

    // TODO test for \t \r and \n in string literals *outside* unicode escape sequence!
    //
    // TODO verify that when a string literal contains a newline before the
    // closing " it correctly updates both the line *and* column in the State.
    //
    // TODO verify that exceeding maximum line length panics
    // TODO verify that exceeding maximum line count panics

}
