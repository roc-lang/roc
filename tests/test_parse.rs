#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod tests {
    use roc::expr::Expr::*;
    use roc::expr::Pattern::*;
    use roc::expr::{Expr};
    use roc::expr::Operator::*;
    use roc::parse;
    use roc::parse_state::{IndentablePosition};
    use combine::{Parser, eof};
    use combine::error::{ParseError};
    use combine::stream::{Stream};
    use combine::easy;
    use combine::stream::state::{State};

    fn standalone_expr<I>() -> impl Parser<Input = I, Output = Expr>
    where I: Stream<Item = char, Position = IndentablePosition>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
    {
        parse::expr().skip(eof())
    }

    fn parse_standalone(actual_str: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>>{
        let parse_state = State::with_positioner(actual_str, IndentablePosition::default());

        standalone_expr().easy_parse(parse_state).map(|(expr, state)| (expr, state.input))
    }

    fn easy_parse_standalone(actual_str: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>> {
        let parse_state = State::with_positioner(actual_str, IndentablePosition::default());

        standalone_expr().easy_parse(parse_state).map(|(expr, state)| (expr, state.input))
    }

    // STRING LITERALS

    fn expect_parsed_str<'a>(expected_str: &'a str, actual_str: &'a str) {
        assert_eq!(
            Ok((String(expected_str.to_string()), "")),
            parse_standalone(actual_str)
        );
    }

    fn expect_parsed_str_error<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
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
    fn parse_string_with_single_qoute() {
        // This shoud NOT be escaped in a string.
        expect_parsed_str("x'x", "\"x'x\"");
    }

    #[test]
    fn parse_string_with_valid_unicode_escapes() {
        expect_parsed_str("x\u{00A0}x", "\"x\\u{00A0}x\"");
        expect_parsed_str("x\u{101010}x", "\"x\\u{101010}x\"");
    }

    #[test]
    fn parse_string_with_invalid_unicode_escapes() {
        // Should be too big - max size should be 10FFFF.
        // (Rust has this restriction. I assume it's a good idea.)
        expect_parsed_str_error("\"x\\u{110000}x\"");

        // No digits specified
        expect_parsed_str_error("\"x\\u{}x\"");

        // No opening curly brace
        expect_parsed_str_error("\"x\\u}x\"");

        // No closing curly brace
        expect_parsed_str_error("\"x\\u{x\"");

        // No curly braces
        expect_parsed_str_error("\"x\\ux\"");
    }

    // CHAR LITERALS

    fn expect_parsed_char<'a>(expected: char, actual_str: &'a str) {
        assert_eq!(Ok((Char(expected), "")), parse_standalone(actual_str));
    }

    fn expect_parsed_char_error<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn parse_empty_char() {
        match easy_parse_standalone("''") {
            Ok(_) => panic!("Expected parse error"),
            Err(err) => {
                let errors = err.errors;

                assert_eq!(errors,
                    vec![
                        easy::Error::Unexpected('\''.into()),
                        easy::Error::Expected(parse::ERR_EMPTY_CHAR.into()),
                    ]
                );
            }
        };
    }

    #[test]
    fn parse_char_without_escape() {
        expect_parsed_char('a', "'a'");
        expect_parsed_char('1', "'1'");
        expect_parsed_char(' ', "' '");
    }

    #[test]
    fn parse_char_with_special_escapes() {
        expect_parsed_char('\\', "'\\\\'");
        expect_parsed_char('\'', "'\\''");
        expect_parsed_char('\t', "'\\t'");
        expect_parsed_char('\r', "'\\r'");
        expect_parsed_char('\n', "'\\n'");
    }

    #[test]
    fn parse_char_with_double_qoute() {
        // This shoud NOT be escaped in a char.
        expect_parsed_char('"', "'\"'");
    }

    #[test]
    fn parse_char_with_unicode_escapes() {
        expect_parsed_char('\u{00A0}', "'\\u{00A0}'");
        expect_parsed_char('\u{101010}', "'\\u{101010}'");
    }

    #[test]
    fn parse_char_with_invalid_unicode_escapes() {
        // Should be too big - max size should be 10FFFF.
        // (Rust has this rechariction. I assume it's a good idea.)
        expect_parsed_char_error("\"x\\u{110000}x\"");

        // No digits specified
        expect_parsed_char_error("\"x\\u{}x\"");

        // No opening curly brace
        expect_parsed_char_error("\"x\\u}x\"");

        // No closing curly brace
        expect_parsed_char_error("\"x\\u{x\"");

        // No curly braces
        expect_parsed_char_error("\"x\\ux\"");
    }

    // NUMBER LITERALS

    fn expect_parsed_int<'a>(expected: i64, actual: &str) {
        assert_eq!(Ok((Int(expected), "")), parse_standalone(actual));
    }

    fn expect_parsed_ratio<'a>(expected_numerator: i64, expected_denominator: u64, actual: &str) {
        assert_eq!(Ok((Frac(expected_numerator, expected_denominator), "")), parse_standalone(actual));
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
    fn parse_single_operator_with_var() {
        assert_eq!(
            // It's important that this isn't mistaken for 
            // a declaration like (x = 1)
            parse_standalone("x == 1"),
            Ok((Operator(
                Box::new(Var("x".to_string())),
                Equals,
                Box::new(Int(1))
            ), ""))
        );
    }


    #[test]
    fn parse_single_operator() {
        match parse_standalone("1234 + 567") {
            Ok((Operator(v1, op, v2), "")) => {
                assert_eq!(*v1, Int(1234));
                assert_eq!(op, Plus);
                assert_eq!(*v2, Int(567));
            },

            _ => panic!("Expression didn't parse"),
        }
    }

    #[test]
    fn parse_multiple_operators() {
        assert_eq!(parse_standalone("1 + 2 * 3"),
            Ok((Operator(
                Box::new(Int(1)),
                Plus,
                Box::new(Operator(Box::new(Int(2)), Star, Box::new(Int(3))))
            ), ""))
        );
    }

    #[test]
    fn parse_operators_with_parens() {
        assert_eq!(parse_standalone("(1 + 2)"),
            Ok((Operator(
                Box::new(Int(1)),
                Plus,
                Box::new(Int(2))
            ), ""))
        );

        assert_eq!(parse_standalone("(1 - 2)"),
            Ok((Operator(
                Box::new(Int(1)),
                Minus,
                Box::new(Int(2))
            ), ""))
        );

        assert_eq!(parse_standalone("(1 + 2 * 3)"),
            Ok((Operator(
                Box::new(Int(1)),
                Plus,
                Box::new(Operator(Box::new(Int(2)), Star, Box::new(Int(3))))
            ), ""))
        );

        assert_eq!(parse_standalone("1 + (2 * 3)"),
            Ok((Operator(
                Box::new(Int(1)),
                Plus,
                Box::new(Operator(Box::new(Int(2)), Star, Box::new(Int(3))))
            ), ""))
        );

        assert_eq!(parse_standalone("(1 + 2) * 3"),
            Ok((Operator(
                Box::new(Operator(Box::new(Int(1)), Plus, Box::new(Int(2)))),
                Star,
                Box::new(Int(3)),
            ), ""))
        );
    }


    // VAR

    fn expect_parsed_var<'a>(expected_str: &'a str) {
        let expected = expected_str.to_string();

        assert_eq!(Ok((Var(expected), "")), parse_standalone(expected_str));
    }

    fn expect_parsed_var_error<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    fn expect_parsed_capitalizedvar_error<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
    }


    #[test]
    fn parse_var() {
        expect_parsed_var("x");
        expect_parsed_var("x2");
        expect_parsed_var("foo");
        expect_parsed_var("foo2furious");
    }

    #[test]
    fn parse_invalid_var() {
        expect_parsed_var_error("5x");
        expect_parsed_var_error("2foo2furious");
        expect_parsed_var_error("2Foo2Furious");

        // TODO someday, capitalized vars should parse successfully as variants.
        // At that point, turn these into variant tests!
        expect_parsed_capitalizedvar_error("X");
        expect_parsed_capitalizedvar_error("X2");
        expect_parsed_capitalizedvar_error("Foo");
        expect_parsed_capitalizedvar_error("Foo2Furious");
    }

    // APPLY

    fn expect_parsed_apply<'a>(parse_str: &'a str, expr1: Expr, expr2: Expr) {
        assert_eq!(
            Ok((Apply(Box::new((expr1, expr2))), "")),
            parse_standalone(parse_str)
        );
    }

    fn expect_parsed_apply_error<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn parse_apply() {
        expect_parsed_apply(
            "(x) y",
            Var("x".to_string()),
            Var("y".to_string())
        );

        expect_parsed_apply(
            "(x 5) y",
            Func("x".to_string(), Box::new(Int(5))),
            Var("y".to_string())
        );

        expect_parsed_apply(
            "(x 5) (y 6)",
            Func("x".to_string(), Box::new(Int(5))),
            Func("y".to_string(), Box::new(Int(6))),
        );

        expect_parsed_apply(
            "(5) (6)",
            Int(5),
            Int(6)
        );
    }

    #[test]
    fn parse_invalid_apply() {
        expect_parsed_apply_error("(x 5)y");
    }


    // TODO write a bunch of parenthetical expression tests - try to repeat
    // all of the above tests except with parens too!
    // Also, verify them all with variable paren counts; ((foo)) should work.

    // FUNC

    // TODO try it with operators, e.g. foo bar + baz qux

    fn expect_parsed_func<'a>(parse_str: &'a str, func_str: &'a str, expr: Expr) {
        assert_eq!(
            Ok((Func(func_str.to_string(), Box::new(expr)), "")),
            parse_standalone(parse_str)
        );
    }

    fn expect_parsed_func_syntax_problem<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    fn expect_parsed_func_error<'a>(actual_str: &'a str) {
        assert!(
            parse_standalone(actual_str).is_err(),
            "Expected parsing error"
        );
    }


    #[test]
    fn parse_func() {
        expect_parsed_func("f 1", "f", Int(1));
        expect_parsed_func("foo  bar", "foo", Var("bar".to_string()));
        expect_parsed_func("foo \"hi\"", "foo", String("hi".to_string()));
    }

    #[test]
    fn parse_invalid_func() {
        expect_parsed_func_syntax_problem("1 f");
        expect_parsed_func_syntax_problem("(1 f)");
    }

    // PARENS

    #[test]
    fn parse_parens() {
        expect_parsed_int(1, "(1)");
        expect_parsed_int(-2, "((-2))");
        expect_parsed_str("a", "(\"a\")");
        expect_parsed_str("abc", "((\"abc\"))");
        expect_parsed_func("(f 1)", "f", Int(1));
        expect_parsed_func("(foo  bar)", "foo", Var("bar".to_string()));
        expect_parsed_func("(  foo \"hi\"  )", "foo", String("hi".to_string()));
    }

    #[test]
    fn parse_invalid_parens_func() {
        expect_parsed_func_error("(1 f");
        expect_parsed_func_error("(f 1");
    }

    // IF

    #[test]
    fn parse_if_space_separated_number() {
        assert_eq!(
            parse_standalone("if 12 34 5 then 5 4 32 1 else 1 3 37"),
            Ok(
                (
                    If(
                        Box::new(Int(12345)),
                        Box::new(Int(54321)),
                        Box::new(Int(1337))
                    ),
                "")
            )
        );
    }

    #[test]
    fn parse_if() {
        assert_eq!(
            parse_standalone("if foo then 1 else 2"),
            Ok(
                (
                    If(
                        Box::new(Var("foo".to_string())),
                        Box::new(Int(1)),
                        Box::new(Int(2))
                    ),
                "")
            )
        );
    }

    // COMPLEX EXPRESSIONS

    #[test]
    fn parse_complex_expressions() {
        expect_parsed_apply(
            "(x 5) (y + (f 6))",
            Func("x".to_string(), Box::new(Int(5))),
            Operator(
                Box::new(Var("y".to_string())),
                Plus,
                Box::new(Func("f".to_string(), Box::new(Int(6)))),
            )
        );

        assert_eq!(
            parse_standalone("(x 5)"),
            Ok((
                Func("x".to_string(), Box::new(Int(5))),
                "")
            )
        );

        assert_eq!(
            parse_standalone("(5)"),
            Ok((
                Int(5),
                "")
            )
        );

        assert_eq!(
            parse_standalone("((1905))"),
            Ok((
                Int(1905),
                "")
            )
        );

        assert_eq!(
            parse_standalone("6 + (685)"),
            Ok((
                Operator(
                    Box::new(Int(6)),
                    Plus,
                    Box::new(Int(685))
                ),
                "")
            )
        );

        assert_eq!(
            parse_standalone("12 + 34"),
            Ok((
                Operator(
                    Box::new(Int(12)),
                    Plus,
                    Box::new(Int(34))
                ),
                "")
            )
        );

        assert_eq!(
            parse_standalone("(51) + 19"),
            Ok((
                Operator(
                    Box::new(Int(51)),
                    Plus,
                    Box::new(Int(19))
                ),
                "")
            )
        );

        assert_eq!(
            parse_standalone("(x 5) + 123"),
            Ok((
                Operator(
                    Box::new(Func("x".to_string(), Box::new(Int(5)))),
                    Plus,
                    Box::new(Int(123))
                ),
                "")
            )
        );

        assert_eq!(
            parse_standalone("(x 5) + (2 * y)"),
            Ok((
                Operator(
                    Box::new(Func("x".to_string(), Box::new(Int(5)))),
                    Plus,
                    Box::new(
                        Operator(
                            Box::new(Int(2)),
                            Star,
                            Box::new(Var("y".to_string()))
                        )
                    )
                ),
                "")
            )
        );
    }

    // LET

    #[test]
    fn parse_let_returning_number() {
        assert_eq!(
            // let x = 5 in -10
            parse_standalone("x = 5\n-10"),
            Ok((
                Let(Identifier("x".to_string()), Box::new(Int(5)), Box::new(Int(-10))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 in 10
            parse_standalone("x=5\n-10"),
            Ok((
                Let(Identifier("x".to_string()), Box::new(Int(5)), Box::new(Int(-10))),
                "")
            )
        );
    }

    #[test]
    fn parse_let_with_operator() {
        assert_eq!(
            // let x = 5 + 10 in -20
            parse_standalone("x =(5 + 10)\n-20"),
            Ok((
                Let(Identifier("x".to_string()),
                    Box::new(Operator(Box::new(Int(5)), Plus, Box::new(Int(10)))),
                    Box::new(Int(-20))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 + 10 in -20
            parse_standalone("x=  5  +  10\n-20"),
            Ok((
                Let(Identifier("x".to_string()),
                    Box::new(Operator(Box::new(Int(5)), Plus, Box::new(Int(10)))),
                    Box::new(Int(-20))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 + 10 in -20
            parse_standalone("x=5\n    + 10\n-20"),
            Ok((
                Let(Identifier("x".to_string()),
                    Box::new(Operator(Box::new(Int(5)), Plus, Box::new(Int(10)))),
                    Box::new(Int(-20))),
                "")
            )
        );
    }

    #[test]
    fn parse_invalid_let_returning_number() {
        assert!(
            parse_standalone("x=5\n    -10").is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn parse_nested_let() {
        assert_eq!(
            // let x = 5 in let y = 12 in 3
            parse_standalone("x = 5\ny = 12\n3"),
            Ok((
                Let(Identifier("x".to_string()),
                    Box::new(Int(5)),
                    Box::new(
                        Let(Identifier("y".to_string()), Box::new(Int(12)),
                            Box::new(Int(3))
                        ))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 in let y = 12 in 3
            parse_standalone("x = 5 - -3\ny = 12 + 7\n3 * -5"),
            Ok((
                Let(Identifier("x".to_string()),
                    Box::new(
                        Operator(
                            Box::new(Int(5)), Minus, Box::new(Int(-3))
                        )
                    ),
                    Box::new(
                        Let(Identifier("y".to_string()),
                            Box::new(Operator(
                                Box::new(Int(12)), Plus, Box::new(Int(7))
                            )),
                            Box::new(Operator(
                                Box::new(Int(3)), Star, Box::new(Int(-5))
                            )),
                        ))),
                "")
            )
        );
    }

    #[test]
    fn parse_let_returning_var() {
        assert_eq!(
            parse_standalone("x=5\nx"),
            Ok((
                Let(Identifier("x".to_string()), Box::new(Int(5)), Box::new(Var("x".to_string()))),
                "")
            )
        );
    }

    #[test]
    fn parse_bad_equals_indent_let() {
        assert!(
            parse_standalone("  x=\n5\n\n5").is_err(),
            "Expected parsing error"
        );
    }
}
