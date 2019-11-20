#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
#[macro_use]
extern crate roc;

#[cfg(test)]
mod test_format {
    use bumpalo::Bump;
    use roc::parse;
    use roc::parse::ast::{format, Attempting, Expr};
    use roc::parse::blankspace::space0_before;
    use roc::parse::parser::{Fail, Parser, State};

    fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Expr<'a>, Fail> {
        let state = State::new(&input, Attempting::Module);
        let parser = space0_before(loc!(parse::expr(0)), 0);
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
            Ok(actual) => assert_eq!(format(&arena, &actual, 0, false), expected),
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
    fn escaped_quote_string() {
        assert_formats_same(indoc!(
            r#"
            "\""
            "#
        ));
    }

    #[test]
    fn empty_block_string() {
        assert_formats_same(indoc!(
            r#"
            """"""
            "#
        ));
    }

    #[test]
    fn basic_block_string() {
        assert_formats_same(indoc!(
            r#"
            """blah"""
            "#
        ));
    }

    #[test]
    fn newlines_block_string() {
        assert_formats_same(indoc!(
            r#"
            """blah
                    spam
            foo"""
            "#
        ));
    }

    #[test]
    fn quotes_block_string() {
        assert_formats_same(indoc!(
            r#"
            """
 
            "" \""" ""\"

            """
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
    fn multi_arg_closure() {
        assert_formats_same(indoc!(
            r#"
            \a b c -> a b c
            "#
        ));
    }

    // DEFS

    #[test]
    fn single_def() {
        assert_formats_same(indoc!(
            r#"
            x = 5

            42
            "#
        ));
    }

    #[test]
    fn two_defs() {
        assert_formats_same(indoc!(
            r#"
            x = 5
            y = 10

            42
            "#
        ));
    }

    #[test]
    fn parenthetical_def() {
        assert_formats_same(indoc!(
            r#"
            (UserId userId) = 5
            y = 10

            42
            "#
        ));
    }

    #[test]
    fn record_destructuring() {
        assert_formats_same(indoc!(
            r#"
            { x, y } = 5

            42
            "#
        ));
    }

    #[test]
    fn def_closure() {
        assert_formats_same(indoc!(
            r#"
            identity = \a -> a

            identity 42
            "#
        ));
    }

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        assert_formats_same("{}");
    }

    #[test]
    fn one_field() {
        assert_formats_same("{ x: 4 }");
    }

    #[test]
    fn two_fields() {
        assert_formats_same("{ x: 4, y: 42 }");
    }

    #[test]
    fn two_fields_newline() {
        assert_formats_same(indoc!(
            r#"
            {
                x: 4,
                y: 42
            }
        "#
        ));
    }

    #[test]
    fn two_fields_center_newline() {
        assert_formats_to(
            indoc!(
                r#"
            { x: 4,
                y: 42
            }
        "#
            ),
            indoc!(
                r#"
            {
                x: 4,
                y: 42
            }
                "#
            ),
        );
    }

    #[test]
    fn one_unnamed_field() {
        assert_formats_same(indoc!(
            r#"
            foo = 4

            { foo }
        "#
        ));
    }

    // IF

    #[test]
    fn single_line_if() {
        assert_formats_same(indoc!(
            r#"
            if foo bar then a b c else d e f
        "#
        ));

        assert_formats_same(indoc!(
            r#"
            if foo (a b c) then a b c else d e f
        "#
        ));
    }

    // CASE

    #[test]
    fn integer_case() {
        assert_formats_same(indoc!(
            r#"
            case b when
                1 ->
                    1

                _ ->
                    2
        "#
        ));
    }

    #[test]
    fn case_with_comments() {
        assert_formats_same(indoc!(
            r#"
            case b when
                # look at cases
                1 ->
                    # case 1
                    1

                # important
                # fall through
                _ ->
                    # case 2
                    # more comment
                    2

        "#
        ));
    }

    #[test]
    fn case_with_moving_comments() {
        assert_formats_to(
            indoc!(
                r#"
            case b when
                1 ->
                    1 # case 1

                # fall through
                _ ->
                    2
                "#
            ),
            indoc!(
                r#"
            case b when
                1 ->
                    1

                # case 1
                # fall through
                _ ->
                    2
                "#
            ),
        );
    }

    // NEWLINES

    #[test]
    fn multiple_blank_lines_collapse_to_one() {
        assert_formats_to(
            indoc!(
                r#"
                x = 5



                y = 10



                42
                "#
            ),
            indoc!(
                r#"
                x = 5

                y = 10

                42
                "#
            ),
        );
    }
}
