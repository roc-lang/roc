#[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate smallvec;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod test_parse {
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
            Ok((Str(expected_str.to_string()), "")),
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
    fn empty_string() {
        assert_eq!(
            Ok((EmptyStr, "")),
            parse_standalone("\"\"")
        );
    }

    #[test]
    fn string_without_escape() {
        expect_parsed_str("a", "\"a\"");
        expect_parsed_str("ab", "\"ab\"");
        expect_parsed_str("abc", "\"abc\"");
        expect_parsed_str("123", "\"123\"");
        expect_parsed_str("abc123", "\"abc123\"");
        expect_parsed_str("123abc", "\"123abc\"");
        expect_parsed_str("123 abc 456 def", "\"123 abc 456 def\"");
    }

    #[test]
    fn string_with_special_escapes() {
        expect_parsed_str("x\\x", "\"x\\\\x\"");
        expect_parsed_str("x\"x", "\"x\\\"x\"");
        expect_parsed_str("x\tx", "\"x\\tx\"");
        expect_parsed_str("x\rx", "\"x\\rx\"");
        expect_parsed_str("x\nx", "\"x\\nx\"");
    }

    #[test]
    fn string_with_escaped_interpolation() {
        assert_eq!(
            // This should NOT be string interpolation, because of the \\
            parse_standalone("\"abcd\\\\(efg)hij\""),
            Ok((
                Str("abcd\\(efg)hij".to_string()),
                ""
            ))
        );
    }

    #[test]
    fn string_with_interpolation_at_end() {
        assert_eq!(
            parse_standalone("\"abcd\\(efg)\""),
            Ok((
                InterpolatedStr(
                    vec![("abcd".to_string(), "efg".to_string())],
                    "".to_string()
                ),
                "")
            )
        );
    }

    #[test]
    fn string_with_interpolation() {
        assert_eq!(
            parse_standalone("\"abcd\\(efg)hij\""),
            Ok((
                InterpolatedStr(
                    vec![("abcd".to_string(), "efg".to_string())],
                    "hij".to_string()
                ),
                "")
            )
        );
    }

    #[test]
    fn string_with_single_qoute() {
        // This shoud NOT be escaped in a string.
        expect_parsed_str("x'x", "\"x'x\"");
    }

    #[test]
    fn string_with_valid_unicode_escapes() {
        expect_parsed_str("x\u{00A0}x", "\"x\\u{00A0}x\"");
        expect_parsed_str("x\u{101010}x", "\"x\\u{101010}x\"");
    }

    #[test]
    fn string_with_invalid_unicode_escapes() {
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
    fn empty_char() {
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
    fn char_without_escape() {
        expect_parsed_char('a', "'a'");
        expect_parsed_char('1', "'1'");
        expect_parsed_char(' ', "' '");
    }

    #[test]
    fn char_with_special_escapes() {
        expect_parsed_char('\\', "'\\\\'");
        expect_parsed_char('\'', "'\\''");
        expect_parsed_char('\t', "'\\t'");
        expect_parsed_char('\r', "'\\r'");
        expect_parsed_char('\n', "'\\n'");
    }

    #[test]
    fn char_with_double_qoute() {
        // This shoud NOT be escaped in a char.
        expect_parsed_char('"', "'\"'");
    }

    #[test]
    fn char_with_unicode_escapes() {
        expect_parsed_char('\u{00A0}', "'\\u{00A0}'");
        expect_parsed_char('\u{101010}', "'\\u{101010}'");
    }

    #[test]
    fn char_with_invalid_unicode_escapes() {
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
    fn positive_int() {
        expect_parsed_int(1234, "1234");
    }

    #[test]
    fn negative_int() {
        expect_parsed_int(-1234, "-1234");
    }

    #[test]
    fn positive_ratio() {
        expect_parsed_ratio(12345, 100, "123.45");
        expect_parsed_ratio(4200, 100, "42.00");
    }

    #[test]
    fn negative_ratio() {
        expect_parsed_ratio(-1234567, 1000, "-1234.567");
        expect_parsed_ratio(-1920, 10, "-192.0");
    }

    #[test]
    fn ints_with_spaces() {
        expect_parsed_int(987654321, "987 6 5 432 1");
        expect_parsed_int(-1234567890, "-1 234 567 890");
    }

    #[test]
    fn ratios_with_spaces() {
        expect_parsed_ratio(-1234567, 1000, "-1 23 4.567");
        expect_parsed_ratio(-1920, 10, "-19 2.0");
        expect_parsed_ratio(12345, 100, "1 2 3.45");
        expect_parsed_ratio(4200, 100, "4 2.00");
    }

    #[test]
    fn single_operator_with_var() {
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
    fn single_operator() {
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
    fn multiple_operators() {
        assert_eq!(parse_standalone("1 + 2 * 3"),
            Ok((Operator(
                Box::new(Int(1)),
                Plus,
                Box::new(Operator(Box::new(Int(2)), Star, Box::new(Int(3))))
            ), ""))
        );
    }

    #[test]
    fn operators_with_parens() {
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

        assert_eq!(parse_standalone("(1 + 2)  * 3"),
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

    #[test]
    fn basic_var() {
        expect_parsed_var("x");
        expect_parsed_var("x2");
        expect_parsed_var("foo");
        expect_parsed_var("foo2furious");
    }

    #[test]
    fn invalid_var() {
        expect_parsed_var_error("5x");
        expect_parsed_var_error("2foo2furious");
        expect_parsed_var_error("2Foo2Furious");
    }

    // APPLY

    fn expect_parsed_apply<'a>(parse_str: &'a str, expr1: Expr, expr2: Expr) {
        assert_eq!(
            Ok((Apply(Box::new(expr1), vec![expr2]), "")),
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
    fn apply() {
        expect_parsed_apply(
            "(x) y",
            Var("x".to_string()),
            Var("y".to_string())
        );

        expect_parsed_apply(
            "(x 5) y",
            CallByName("x".to_string(), vec![Int(5)]),
            Var("y".to_string())
        );

        expect_parsed_apply(
            "(x 5) (y 6)",
            CallByName("x".to_string(), vec![Int(5)]),
            CallByName("y".to_string(), vec![Int(6)]),
        );

        expect_parsed_apply(
            "(5) (6)",
            Int(5),
            Int(6)
        );
    }

    #[test]
    fn invalid_apply() {
        expect_parsed_apply_error("(x 5)y");
    }


    // TODO write a bunch of parenthetical expression tests - try to repeat
    // all of the above tests except with parens too!
    // Also, verify them all with variable paren counts; ((foo)) should work.

    // FUNC

    fn expect_parsed_func<'a>(parse_str: &'a str, func_str: &'a str, args: Vec<Expr>) {
        assert_eq!(
            Ok((CallByName(func_str.to_string(), args), "")),
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
    fn single_arg_func() {
        expect_parsed_func("f 1", "f", vec![Int(1)]);
        expect_parsed_func("foo  bar", "foo", vec![Var("bar".to_string())]);
        expect_parsed_func("foo \"hi\"", "foo", vec![Str("hi".to_string())]);
    }

    #[test]
    fn multi_arg_func() {
        expect_parsed_func("f 1,  23,  456", "f", vec![Int(1), Int(23), Int(456)]);
        expect_parsed_func("foo  bar, 'z'", "foo", vec![Var("bar".to_string()), Char('z')]);
        expect_parsed_func("foo \"hi\", 1, blah", "foo", vec![Str("hi".to_string()), Int(1), Var("blah".to_string())]);
    }

    #[test]
    fn multiline_func() {
        expect_parsed_func("f\n 1", "f", vec![Int(1)]);
        expect_parsed_func("foo  bar,\n 'z'", "foo", vec![Var("bar".to_string()), Char('z')]);
        expect_parsed_func("foo \"hi\",\n 1,\n blah", "foo", vec![Str("hi".to_string()), Int(1), Var("blah".to_string())]);
    }

    #[test]
    fn func_with_operator() {
        assert_eq!(
            parse_standalone("f 5 + 6"),
            Ok(
                (
                    Operator(
                        Box::new(
                            CallByName("f".to_string(),
                                vec![Int(5)],
                            )
                        ),
                        Plus,
                        Box::new(Int(6))
                    ),
                "")
            )
        );
    }

    #[test]
    fn func_with_operator_and_multiple_args() {
        assert_eq!(
            parse_standalone("f 1, 2, 3 + 6"),
            Ok(
                (
                    Operator(
                        Box::new(
                            CallByName("f".to_string(),
                                vec![Int(1), Int(2), Int(3)],
                            )
                        ),
                        Plus,
                        Box::new(Int(6))
                    ),
                "")
            )
        );
    }


    #[test]
    fn invalid_func() {
        expect_parsed_func_syntax_problem("1 f");
        expect_parsed_func_syntax_problem("(1 f)");
    }

    // PARENS

    #[test]
    fn parens() {
        expect_parsed_int(1, "(1)");
        expect_parsed_int(-2, "((-2))");
        expect_parsed_str("a", "(\"a\")");
        expect_parsed_str("abc", "((\"abc\"))");
        expect_parsed_func("(f 1)", "f", vec![Int(1)]);
        expect_parsed_func("(foo  bar)", "foo", vec![Var("bar".to_string())]);
    }

    #[test]
    fn parens_with_spaces() {
        expect_parsed_func("(a \"x\" )", "a", vec![Str("x".to_string())]);
        expect_parsed_func("(  b \"y\")", "b", vec![Str("y".to_string())]);
        expect_parsed_func("(  c \"z\"  )", "c", vec![Str("z".to_string())]);
    }

    #[test]
    fn invalid_parens_func() {
        expect_parsed_func_error("(1 f");
        expect_parsed_func_error("(f 1");
    }

    // CASE

    #[test]
    fn one_branch_case() {
        assert_eq!(
            parse_standalone("case 1 when x then 2"),
            Ok((
                Case(
                    Box::new(Int(1)),
                    smallvec![( Identifier("x".to_string()), Box::new(Int(2)) )]
                ),
                ""
            ))
        );
    }

    #[test]
    fn two_branch_case() {
        assert_eq!(
            parse_standalone("case 1 when x then 2 when y then 3"),
            Ok((
                Case(
                    Box::new(Int(1)),
                    smallvec![
                        ( Identifier("x".to_string()), Box::new(Int(2)) ),
                        ( Identifier("y".to_string()), Box::new(Int(3)) )
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn two_branch_case_with_two_newlines() {
        assert_eq!(
            parse_standalone("case a\n\n  when b then 1\n\n  when\n    c then 2"),
            Ok((
                Case(
                    Box::new(Var("a".to_string())),
                    smallvec![
                        ( Identifier("b".to_string()), Box::new(Int(1)) ),
                        ( Identifier("c".to_string()), Box::new(Int(2)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn multi_newline_case_regression() {
        assert_eq!(
            parse_standalone("a =\n  case x\n   when b then 1\n\n   when c then 2\na"),
            Ok((
                Assign(
                    Identifier("a".to_string()),
                    Box::new(Case(
                        Box::new(Var("x".to_string())),
                        smallvec![
                            ( Identifier("b".to_string()), Box::new(Int(1)) ),
                            ( Identifier("c".to_string()), Box::new(Int(2)) ),
                        ]
                    )),
                    Box::new(Var("a".to_string()))
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_two_newlines() {
        assert_eq!(
            parse_standalone("case a\n\n  when b then 1"),
            Ok((
                Case(
                    Box::new(Var("a".to_string())),
                    smallvec![
                        ( Identifier("b".to_string()), Box::new(Int(1)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_number_pattern() {
        assert_eq!(
            parse_standalone("case 1 when 2 then 3"),
            Ok((
                Case(
                    Box::new(Int(1)),
                    smallvec![
                        ( Integer(2), Box::new(Int(3)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_empty_variant() {
        assert_eq!(
            parse_standalone("case 1 when Foo then 3"),
            Ok((
                Case(
                    Box::new(Int(1)),
                    smallvec![
                        ( Variant("Foo".to_string(), None), Box::new(Int(3)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_nonempty_variant() {
        assert_eq!(
            parse_standalone("case 1 when Foo x then 3"),
            Ok((
                Case(
                    Box::new(Int(1)),
                    smallvec![
                        ( Variant("Foo".to_string(), Some(vec![Identifier("x".to_string())])), Box::new(Int(3)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_two_branches_and_function_call() {
        assert_eq!(
            parse_standalone("case 0 when 2 then foo 9 when 1 then bar 8"),
            Ok((
                Case(
                    Box::new(Int(0)),
                    smallvec![
                        ( Integer(2), Box::new(CallByName("foo".to_string(), vec![Int(9)])) ),
                        ( Integer(1), Box::new(CallByName("bar".to_string(), vec![Int(8)])) ),
                    ]
                ),
                ""
            ))
        );
    }


    // IF

    #[test]
    fn indented_if() {
        assert_eq!(
            parse_standalone("if 12 34 5 then\n  5 4 32 1\n  else 1 3 37"),
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
    fn if_space_separated_number() {
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
    fn single_line_if() {
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

    // VARIANT

    #[test]
    fn basic_variant() {
        assert_eq!(
            parse_standalone("Abc"),
            Ok((
                ApplyVariant("Abc".to_string(), None),
                ""
            ))
        );
    }

    #[test]
    fn variant_with_one_arg() {
        assert_eq!(
            parse_standalone("Bbc 1"),
            Ok((
                ApplyVariant("Bbc".to_string(), Some(vec![Int(1)])),
                ""
            ))
        );
    }

    #[test]
    fn variant_with_two_args() {
        assert_eq!(
            parse_standalone("Bbc 1, 2"),
            Ok((
                ApplyVariant("Bbc".to_string(), Some(vec![Int(1), Int(2)])),
                ""
            ))
        );
    }

    #[test]
    fn variant_regression() {
        // Somehow parsing the variant "Abc" worked but "Foo" failed (?!)
        assert_eq!(
            parse_standalone("F"),
            Ok((
                ApplyVariant("F".to_string(), None),
                ""
            ))
        );
    }


    // COMPLEX EXPRESSIONS

    #[test]
    fn nested_let_variant() {
        assert_eq!(
            parse_standalone("one = Abc\n\ntwo = Bar\n\none"),
            Ok((
                Assign(
                    Identifier(
                        "one".to_string()
                    ),
                    Box::new(ApplyVariant(
                        "Abc".to_string(),
                        None
                    )),
                    Box::new(Assign(
                        Identifier(
                            "two".to_string()
                        ),
                        Box::new(ApplyVariant(
                            "Bar".to_string(),
                            None
                        )),
                        Box::new(Var(
                            "one".to_string()
                        ))
                    ))
                ),
                ""
            ))
        );
    }

    #[test]
    fn complex_expressions() {
        expect_parsed_apply(
            "(x 5) (y + (f 6))",
            CallByName("x".to_string(), vec![Int(5)]),
            Operator(
                Box::new(Var("y".to_string())),
                Plus,
                Box::new(CallByName("f".to_string(), vec![Int(6)])),
            )
        );

        assert_eq!(
            parse_standalone("(x 5)"),
            Ok((
                CallByName("x".to_string(), vec![Int(5)]),
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
                    Box::new(CallByName("x".to_string(), vec![Int(5)])),
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
                    Box::new(CallByName("x".to_string(), vec![Int(5)])),
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

    // ASSIGN

    #[test]
    fn let_with_function_application() {
        assert_eq!(
            parse_standalone("abc =\n  y 1\n\nabc"),
            Ok((
                Assign(
                    Identifier(
                        "abc".to_string()
                    ),
                    Box::new(CallByName(
                        "y".to_string(),
                        vec![
                            Int(
                                1
                            )
                        ]
                    )),
                    Box::new(Var(
                        "abc".to_string()
                    ))
                ),
                ""
            ))
        )
    }

    #[test]
    fn let_returning_number() {
        assert_eq!(
            // let x = 5 in -10
            parse_standalone("x = 5\n-10"),
            Ok((
                Assign(Identifier("x".to_string()), Box::new(Int(5)), Box::new(Int(-10))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 in 10
            parse_standalone("x=5\n-10"),
            Ok((
                Assign(Identifier("x".to_string()), Box::new(Int(5)), Box::new(Int(-10))),
                "")
            )
        );
    }

    #[test]
    fn let_with_operator() {
        assert_eq!(
            // let x = 5 + 10 in -20
            parse_standalone("x =(5 + 10)\n-20"),
            Ok((
                Assign(Identifier("x".to_string()),
                    Box::new(Operator(Box::new(Int(5)), Plus, Box::new(Int(10)))),
                    Box::new(Int(-20))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 + 10 in -20
            parse_standalone("x=  5  +  10\n-20"),
            Ok((
                Assign(Identifier("x".to_string()),
                    Box::new(Operator(Box::new(Int(5)), Plus, Box::new(Int(10)))),
                    Box::new(Int(-20))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 + 10 in -20
            parse_standalone("x=5\n    + 10\n-20"),
            Ok((
                Assign(Identifier("x".to_string()),
                    Box::new(Operator(Box::new(Int(5)), Plus, Box::new(Int(10)))),
                    Box::new(Int(-20))),
                "")
            )
        );
    }

    #[test]
    fn invalid_let_returning_number() {
        assert!(
            parse_standalone("x=5\n    -10").is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn nested_let() {
        assert_eq!(
            // let x = 5 in let y = 12 in 3
            parse_standalone("x = 5\ny = 12\n3"),
            Ok((
                Assign(Identifier("x".to_string()),
                    Box::new(Int(5)),
                    Box::new(
                        Assign(Identifier("y".to_string()), Box::new(Int(12)),
                            Box::new(Int(3))
                        ))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 in let y = 12 in 3
            parse_standalone("x = 5 - -3\ny = 12 + 7\n3 * -5"),
            Ok((
                Assign(Identifier("x".to_string()),
                    Box::new(
                        Operator(
                            Box::new(Int(5)), Minus, Box::new(Int(-3))
                        )
                    ),
                    Box::new(
                        Assign(Identifier("y".to_string()),
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
    fn let_returning_var() {
        assert_eq!(
            parse_standalone("x=5\nx"),
            Ok((
                Assign(Identifier("x".to_string()), Box::new(Int(5)), Box::new(Var("x".to_string()))),
                "")
            )
        );
    }

    #[test]
    fn bad_equals_indent_let() {
        assert!(
            parse_standalone("  x=\n5\n\n5").is_err(),
            "Expected parsing error"
        );
    }


    #[test]
    fn regression_on_calling_function_named_c() {
        // This was broken because case-expressions were greedily consuming 'c' characters for "case"
        assert_eq!(
            parse_standalone("f = (x) -> c 1\n\nf"),
            Ok((
                Assign(
                    Identifier("f".to_string()),
                    Box::new(Closure(
                        smallvec![Identifier("x".to_string())],
                        Box::new(CallByName("c".to_string(), vec![Int(1)]))
                    )),
                    Box::new(Var("f".to_string()))
                ),
                ""
            ))
        );
    }

    #[test]
    fn regression_on_passing_arguments_named_i() {
        // This was broken because if-expressions were greedily consuming 'i' characters for "if"
        assert_eq!(
            parse_standalone("x i"),
            Ok((
                CallByName("x".to_string(), vec![Var("i".to_string())]),
                ""
            ))
        );
    }
}
