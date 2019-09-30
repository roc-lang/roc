#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc;

#[cfg(test)]
mod test_format {
    use bumpalo::Bump;
    use roc::parse;
    use roc::parse::ast::{format, Attempting, Expr};
    use roc::parse::blankspace::space0_before;
    use roc::parse::parser::{loc, Fail, Parser, State};

    fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Expr<'a>, Fail> {
        let state = State::new(&input, Attempting::Module);
        let parser = space0_before(loc(parse::expr(0)), 0);
        let answer = parser.parse(&arena, state);

        answer
            .map(|(loc_expr, _)| loc_expr.value)
            .map_err(|(fail, _)| fail)
    }

    fn assert_formats_to(input: &str, expected: &str) {
        let arena = Bump::new();
        let input = input.trim_end();
        let expected = expected.trim_end();

        match parse_with(&arena, input) {
            Ok(actual) => assert_eq!(format(&arena, &actual, 0), expected),
            Err(error) => panic!("Unexpected parse failure when parsing this for formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", input, error)
        }
    }

    fn assert_formats_same(input: &str) {
        assert_formats_to(input, input);
    }

    // STRING LITERALS

    #[test]
    fn empty_string() {
        assert_formats_same(indoc!(
            r#"
            ""
            "#
        ));
    }

    #[test]
    fn zero() {
        assert_formats_same(indoc!(
            r#"
            0
            "#
        ));
    }

    #[test]
    fn zero_point_zero() {
        assert_formats_same(indoc!(
            r#"
            0.0
            "#
        ));
    }

    #[test]
    fn int_with_underscores() {
        assert_formats_same(indoc!(
            r#"
            1_23_456
            "#
        ));
    }

    #[test]
    fn float_with_underscores() {
        assert_formats_same(indoc!(
            r#"
            1_23_456.7_89_10
            "#
        ));
    }

    #[test]
    fn basic_string() {
        assert_formats_same(indoc!(
            r#"
            "blah"
            "#
        ));
    }

    #[test]
    fn escaped_unicode_string() {
        assert_formats_same(indoc!(
            r#"
            "unicode: \u{A00A}!"
            "#
        ));
    }

    #[test]
    fn single_def() {
        assert_formats_same(indoc!(
            r#"# comment to reset indentation
            x = 5

            42
            "#
        ));
    }

    #[test]
    fn two_defs() {
        assert_formats_same(indoc!(
            r#"# comment to reset indentation
            x = 5
            y = 10

            42
            "#
        ));
    }

    #[test]
    fn record_destructuring() {
        assert_formats_same(indoc!(
            r#"# comment to reset indentation
            { x, y } = 5

            42
            "#
        ));
    }

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        assert_formats_same("{}");
    }
}
