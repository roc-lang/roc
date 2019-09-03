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
    use roc::parser::Expr::{self, *};
    use roc::parser::{Attempting, Parser, Problem, State};
    use roc::region::Located;

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) -> Vec<Located<Problem>> {
        let mut problems = Vec::new();

        {
            let state = State::from_input(&input);
            let arena = Bump::new();
            let parser = roc::parser::expr();
            let answer = parser.parse(&arena, &state, &mut problems, Attempting::Expression);
            let actual = answer.map(|(_, expr)| expr);

            assert_eq!(Ok(expected_expr), actual);
        }

        problems
    }

    fn _assert_parse_problems<'a>(
        input: &'a str,
        expected_attempting: Attempting,
        expected_probs: Vec<Problem>,
    ) {
        let mut problems = Vec::new();

        {
            let state = State::from_input(&input);
            let arena = Bump::new();
            let parser = roc::parser::expr();
            let answer = parser.parse(&arena, &state, &mut problems, Attempting::Expression);
            let actual = answer.map_err(|(_, attempting)| attempting);

            assert_eq!(Err(expected_attempting), actual);
        }

        assert_eq!(
            expected_probs,
            problems
                .into_iter()
                .map(|loc_prob| loc_prob.value)
                .collect::<Vec<Problem>>()
        )
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
}
