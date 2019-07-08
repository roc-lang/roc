#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod test_parse {
    use roc::expr::Expr::*;
    use roc::expr::Pattern::*;
    use roc::expr::{Expr};
    use roc::operator::Operator::*;
    use roc::region::{Located, Region};
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

    /// Zero out the parse locations on everything in this Expr, so we can compare expected/actual without
    /// having to account for that.
    fn zero_loc_expr(expr: Expr) -> Expr {
        match expr {
            Int(_) | Frac(_, _) | EmptyStr | Str(_) | Char(_) | Var(_) | EmptyRecord => expr,
            InterpolatedStr(pairs, string) => InterpolatedStr(pairs.into_iter().map(|( prefix, ident )| ( prefix, zero_loc(ident))).collect(), string),
            Assign(pattern, expr1, expr2) => Assign(loc(pattern.value), loc_box(zero_loc_expr((*expr1).value)), loc_box(zero_loc_expr((*expr2).value))),
            CallByName(ident, args) => CallByName(ident, args.into_iter().map(|arg| loc(zero_loc_expr(arg.value))).collect()),
            Apply(fn_expr, args) => Apply(loc_box(zero_loc_expr((*fn_expr).value)), args.into_iter().map(|arg| loc(zero_loc_expr(arg.value))).collect()),
            Operator(left, op, right) => Operator(loc_box(zero_loc_expr((*left).value)), zero_loc(op), loc_box(zero_loc_expr((*right).value))),
            Closure(patterns, body) => Closure(patterns.into_iter().map(zero_loc).collect(), loc_box(zero_loc_expr((*body).value))),
            ApplyVariant(_, None) => expr,
            ApplyVariant(name, Some(args)) => ApplyVariant(name, Some(args.into_iter().map(|arg| loc(zero_loc_expr(arg.value))).collect())),
            If(condition, if_true, if_false) => If(loc_box(zero_loc_expr((*condition).value)), loc_box(zero_loc_expr((*if_true).value)), loc_box(zero_loc_expr((*if_false).value))),
            Case(condition, branches) =>
                Case(
                    loc_box(zero_loc_expr((*condition).value)),
                    branches.into_iter().map(|( pattern, expr )| ( zero_loc(pattern), loc_box(zero_loc_expr((*expr).value)))).collect()
                ),
        }
    }

    fn zero_loc<T>(located_val: Located<T>) -> Located<T> {
        loc(located_val.value)
    }

    fn parse_without_loc(actual_str: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>>{
        parse_standalone(actual_str)
            .map(|(expr, leftover)| (zero_loc_expr(expr), leftover))
    }

    fn parse_standalone(actual_str: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>>{
        let parse_state = State::with_positioner(actual_str, IndentablePosition::default());

        standalone_expr().easy_parse(parse_state).map(|(expr, state)| (expr, state.input))
    }

    fn easy_parse_standalone(actual_str: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>> {
        let parse_state = State::with_positioner(actual_str, IndentablePosition::default());

        standalone_expr().easy_parse(parse_state).map(|(expr, state)| (expr, state.input))
    }

    fn loc_box<T>(val: T) -> Box<Located<T>> {
        Box::new(loc(val))
    }

    fn loc<T>(val: T) -> Located<T> {
        Located::new(val, Region {
            start_line: 0,
            start_col: 0,

            end_line: 0,
            end_col: 0,
        })
    }

    // STRING LITERALS

    fn expect_parsed_str<'a>(expected_str: &'a str, actual_str: &'a str) {
        assert_eq!(
            Ok((Str(expected_str.to_string()), "")),
            parse_without_loc(actual_str)
        );
    }

    fn expect_parsed_str_error<'a>(actual_str: &'a str) {
        assert!(
            parse_without_loc(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn empty_string() {
        assert_eq!(
            Ok((EmptyStr, "")),
            parse_without_loc("\"\"")
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
            parse_without_loc("\"abcd\\\\(efg)hij\""),
            Ok((
                Str("abcd\\(efg)hij".to_string()),
                ""
            ))
        );
    }

    #[test]
    fn string_with_interpolation_at_end() {
        assert_eq!(
            parse_without_loc("\"abcd\\(efg)\""),
            Ok((
                InterpolatedStr(
                    vec![("abcd".to_string(), loc("efg".to_string()))],
                    "".to_string()
                ),
                "")
            )
        );
    }

    #[test]
    fn string_with_interpolation() {
        assert_eq!(
            parse_without_loc("\"abcd\\(efg)hij\""),
            Ok((
                InterpolatedStr(
                    vec![("abcd".to_string(), loc("efg".to_string()))],
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
        assert_eq!(Ok((Char(expected), "")), parse_without_loc(actual_str));
    }

    fn expect_parsed_char_error<'a>(actual_str: &'a str) {
        assert!(
            parse_without_loc(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn empty_char() {
        if easy_parse_standalone("''").is_ok() {
            panic!("Expected parse error");
        }
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
        assert_eq!(Ok((Int(expected), "")), parse_without_loc(actual));
    }

    fn expect_parsed_ratio<'a>(expected_numerator: i64, expected_denominator: i64, actual: &str) {
        assert_eq!(Ok((Frac(expected_numerator, expected_denominator), "")), parse_without_loc(actual));
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
    fn ints_with_underscores() {
        expect_parsed_int(987654321, "987_6_5_432_1");
        expect_parsed_int(-1234567890, "-1_234_567_890");
    }

    #[test]
    fn fracs_with_spaces() {
        expect_parsed_ratio(-1234567, 1000, "-1_23_4.567");
        expect_parsed_ratio(-1920, 10, "-19_2.0");
        expect_parsed_ratio(12345, 100, "1_2_3.45");
        expect_parsed_ratio(4200, 100, "4_2.00");
    }

    #[test]
    fn single_operator_with_var() {
        assert_eq!(
            // It's important that this isn't mistaken for
            // a declaration like (x = 1)
            parse_without_loc("x == 1"),
            Ok((Operator(
                loc_box(Var("x".to_string())),
                loc(Equals),
                loc_box(Int(1))
            ), ""))
        );
    }

    #[test]
    fn comparison_operators() {
        assert_eq!(
            parse_without_loc("x >= 0"),
            Ok((Operator(
                loc_box(Var("x".to_string())),
                loc(GreaterThanOrEq),
                loc_box(Int(0))
            ), ""))
        );
        assert_eq!(
            parse_without_loc("x > 0"),
            Ok((Operator(
                loc_box(Var("x".to_string())),
                loc(GreaterThan),
                loc_box(Int(0))
            ), ""))
        );
        assert_eq!(
            parse_without_loc("x <= 0"),
            Ok((Operator(
                loc_box(Var("x".to_string())),
                loc(LessThanOrEq),
                loc_box(Int(0))
            ), ""))
        );
        assert_eq!(
            parse_without_loc("x < 0"),
            Ok((Operator(
                loc_box(Var("x".to_string())),
                loc(LessThan),
                loc_box(Int(0))
            ), ""))
        );
    }


    #[test]
    fn single_operator() {
        match parse_without_loc("1234 + 567") {
            Ok((Operator(v1, op, v2), "")) => {
                assert_eq!((*v1).value, Int(1234));
                assert_eq!(op.value, Plus);
                assert_eq!((*v2).value, Int(567));
            },

            _ => panic!("Expression didn't parse"),
        }
    }

    #[test]
    fn multiple_operators() {
        assert_eq!(parse_without_loc("1 + 2 * 3"),
            Ok((Operator(
                loc_box(Int(1)),
                loc(Plus),
                loc_box(Operator(loc_box(Int(2)), loc(Star), loc_box(Int(3))))
            ), ""))
        );
    }

    #[test]
    fn operators_with_parens() {
        assert_eq!(parse_without_loc("(1 + 2)"),
            Ok((Operator(
                loc_box(Int(1)),
                loc(Plus),
                loc_box(Int(2))
            ), ""))
        );

        assert_eq!(parse_without_loc("(1 - 2)"),
            Ok((Operator(
                loc_box(Int(1)),
                loc(Minus),
                loc_box(Int(2))
            ), ""))
        );

        assert_eq!(parse_without_loc("(1 + 2 * 3)"),
            Ok((Operator(
                loc_box(Int(1)),
                loc(Plus),
                loc_box(Operator(loc_box(Int(2)), loc(Star), loc_box(Int(3))))
            ), ""))
        );

        assert_eq!(parse_without_loc("1 + (2 * 3)"),
            Ok((Operator(
                loc_box(Int(1)),
                loc(Plus),
                loc_box(Operator(loc_box(Int(2)), loc(Star), loc_box(Int(3))))
            ), ""))
        );

        assert_eq!(parse_without_loc("(1 + 2)  * 3"),
            Ok((Operator(
                loc_box(Operator(loc_box(Int(1)), loc(Plus), loc_box(Int(2)))),
                loc(Star),
                loc_box(Int(3)),
            ), ""))
        );
    }


    // VAR

    fn expect_parsed_var<'a>(expected_str: &'a str) {
        let expected = expected_str.to_string();

        assert_eq!(Ok((Var(expected), "")), parse_without_loc(expected_str));
    }

    fn expect_parsed_var_error<'a>(actual_str: &'a str) {
        assert!(
            parse_without_loc(actual_str).is_err(),
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
            Ok((Apply(loc_box(expr1), vec![loc(expr2)]), "")),
            parse_without_loc(parse_str)
        );
    }

    fn expect_parsed_apply_error<'a>(actual_str: &'a str) {
        assert!(
            parse_without_loc(actual_str).is_err(),
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
            CallByName("x".to_string(), vec![loc(Int(5))]),
            Var("y".to_string())
        );

        expect_parsed_apply(
            "(x 5) (y 6)",
            CallByName("x".to_string(), vec![loc(Int(5))]),
            CallByName("y".to_string(), vec![loc(Int(6))]),
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

    // CLOSURE

    #[test]
    fn single_arg_closure() {
        assert_eq!(
            parse_without_loc("\\a => b"),
            Ok((
                Closure(
                    vec![loc(Identifier("a".to_string()))],
                    loc_box(Var("b".to_string()))
                ),
                ""
            ))
        );
    }

    #[test]
    fn multi_arg_closure() {
        assert_eq!(
            parse_without_loc("\\a b => c"),
            Ok((
                Closure(
                    vec![loc(Identifier("a".to_string())), loc(Identifier("b".to_string()))],
                    loc_box(Var("c".to_string()))
                ),
                ""
            ))
        );
    }


    // FUNC

    fn expect_parsed_func<'a>(parse_str: &'a str, func_str: &'a str, args: Vec<Located<Expr>>) {
        assert_eq!(
            Ok((CallByName(func_str.to_string(), args), "")),
            parse_without_loc(parse_str)
        );
    }

    fn expect_parsed_func_syntax_problem<'a>(actual_str: &'a str) {
        assert!(
            parse_without_loc(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    fn expect_parsed_func_error<'a>(actual_str: &'a str) {
        assert!(
            parse_without_loc(actual_str).is_err(),
            "Expected parsing error"
        );
    }


    #[test]
    fn single_arg_func() {
        expect_parsed_func("f 1", "f", vec![loc(Int(1))]);
        expect_parsed_func("foo  bar", "foo", vec![loc(Var("bar".to_string()))]);
        expect_parsed_func("foo \"hi\"", "foo", vec![loc(Str("hi".to_string()))]);
    }

    #[test]
    fn multi_arg_func() {
        expect_parsed_func("f 1  23  456", "f", vec![loc(Int(1)), loc(Int(23)), loc(Int(456))]);
        expect_parsed_func("foo  bar 'z'", "foo", vec![loc(Var("bar".to_string())), loc(Char('z'))]);
        expect_parsed_func("foo \"hi\" 1 blah", "foo", vec![loc(Str("hi".to_string())), loc(Int(1)), loc(Var("blah".to_string()))]);
    }

    #[test]
    fn multiline_func() {
        expect_parsed_func("f\n 1", "f", vec![loc(Int(1))]);
        expect_parsed_func("foo  bar\n 'z'", "foo", vec![loc(Var("bar".to_string())), loc(Char('z'))]);
        expect_parsed_func("foo \"hi\"\n 1\n blah", "foo", vec![loc(Str("hi".to_string())), loc(Int(1)), loc(Var("blah".to_string()))]);
    }

    #[test]
    fn func_with_operator() {
        assert_eq!(
            parse_without_loc("f 5 + 6"),
            Ok(
                (
                    Operator(
                        loc_box(
                            CallByName("f".to_string(),
                                vec![loc(Int(5))],
                            )
                        ),
                        loc(Plus),
                        loc_box(Int(6))
                    ),
                "")
            )
        );
    }

    #[test]
    fn func_with_operator_and_multiple_args() {
        assert_eq!(
            parse_without_loc("f 1 2 3 + 6"),
            Ok(
                (
                    Operator(
                        loc_box(
                            CallByName("f".to_string(),
                                vec![loc(Int(1)), loc(Int(2)), loc(Int(3))],
                            )
                        ),
                        loc(Plus),
                        loc_box(Int(6))
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
        expect_parsed_func("(f 1)", "f", vec![loc(Int(1))]);
        expect_parsed_func("(foo  bar)", "foo", vec![loc(Var("bar".to_string()))]);
    }

    #[test]
    fn parens_with_spaces() {
        expect_parsed_func("(a 1 )", "a", vec![loc(Int(1))]);
        expect_parsed_func("(  b \"y\")", "b", vec![loc(Str("y".to_string()))]);
        expect_parsed_func("(  c \"z\"  )", "c", vec![loc(Str("z".to_string()))]);
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
            parse_without_loc("case 1 when x then 2"),
            Ok((
                Case(
                    loc_box(Int(1)),
                    vec![( loc(Identifier("x".to_string())), loc_box(Int(2)) )]
                ),
                ""
            ))
        );
    }

    #[test]
    fn two_branch_case() {
        assert_eq!(
            parse_without_loc("case 1 when x then 2 when y then 3"),
            Ok((
                Case(
                    loc_box(Int(1)),
                    vec![
                        ( loc(Identifier("x".to_string())), loc_box(Int(2)) ),
                        ( loc(Identifier("y".to_string())), loc_box(Int(3)) )
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn two_branch_case_with_two_newlines() {
        assert_eq!(
            parse_without_loc("case a\n\n  when b then 1\n\n  when\n    c then 2"),
            Ok((
                Case(
                    loc_box(Var("a".to_string())),
                    vec![
                        ( loc(Identifier("b".to_string())), loc_box(Int(1)) ),
                        ( loc(Identifier("c".to_string())), loc_box(Int(2)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn multi_newline_case_regression() {
        assert_eq!(
            parse_without_loc("a =\n  case x\n   when b then 1\n\n   when c then 2\na"),
            Ok((
                Assign(
                    loc(Identifier("a".to_string())),
                    loc_box(Case(
                        loc_box(Var("x".to_string())),
                        vec![
                            ( loc(Identifier("b".to_string())), loc_box(Int(1)) ),
                            ( loc(Identifier("c".to_string())), loc_box(Int(2)) ),
                        ]
                    )),
                    loc_box(Var("a".to_string()))
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_two_newlines() {
        assert_eq!(
            parse_without_loc("case a\n\n  when b then 1"),
            Ok((
                Case(
                    loc_box(Var("a".to_string())),
                    vec![
                        ( loc(Identifier("b".to_string())), loc_box(Int(1)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_number_pattern() {
        assert_eq!(
            parse_without_loc("case 1 when 2 then 3"),
            Ok((
                Case(
                    loc_box(Int(1)),
                    vec![
                        ( loc(Integer(2)), loc_box(Int(3)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_empty_variant() {
        assert_eq!(
            parse_without_loc("case 1 when Foo then 3"),
            Ok((
                Case(
                    loc_box(Int(1)),
                    vec![
                        ( loc(Variant("Foo".to_string(), None)), loc_box(Int(3)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_nonempty_variant() {
        assert_eq!(
            parse_without_loc("case 1 when Foo x then 3"),
            Ok((
                Case(
                    loc_box(Int(1)),
                    vec![
                        ( loc(Variant("Foo".to_string(), Some(vec![Identifier("x".to_string())]))), loc_box(Int(3)) ),
                    ]
                ),
                ""
            ))
        );
    }

    #[test]
    fn case_with_two_branches_and_function_call() {
        assert_eq!(
            parse_without_loc("case 0 when 2 then foo 9 when 1 then bar 8"),
            Ok((
                Case(
                    loc_box(Int(0)),
                    vec![
                        ( loc(Integer(2)), loc_box(CallByName("foo".to_string(), vec![loc(Int(9))])) ),
                        ( loc(Integer(1)), loc_box(CallByName("bar".to_string(), vec![loc(Int(8))])) ),
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
            parse_without_loc("if 12345 then\n  54321\n  else 1337"),
            Ok(
                (
                    If(
                        loc_box(Int(12345)),
                        loc_box(Int(54321)),
                        loc_box(Int(1337))
                    ),
                "")
            )
        );
    }

    #[test]
    fn if_underscore_separated_number() {
        assert_eq!(
            parse_without_loc("if 12_34_5 then 5_4_32_1 else 1_3_37"),
            Ok(
                (
                    If(
                        loc_box(Int(12345)),
                        loc_box(Int(54321)),
                        loc_box(Int(1337))
                    ),
                "")
            )
        );
    }

    #[test]
    fn single_line_if() {
        assert_eq!(
            parse_without_loc("if foo then 1 else 2"),
            Ok(
                (
                    If(
                        loc_box(Var("foo".to_string())),
                        loc_box(Int(1)),
                        loc_box(Int(2))
                    ),
                "")
            )
        );
    }

    // INLINE COMMENT

    #[test]
    fn inline_comment() {
        assert_eq!(
            parse_without_loc("if 12345 then # blah blah\n  54321 #whee!\n  else 1337"),
            Ok(
                (
                    If(
                        loc_box(Int(12345)),
                        loc_box(Int(54321)),
                        loc_box(Int(1337))
                    ),
                "")
            )
        );
    }

    #[test]
    fn inline_comment_in_assignment() {
        assert_eq!(
            parse_without_loc("foo = 1\n# comment\nbar"),
            Ok(
                (
                    Assign(
                        loc(Identifier("foo".to_string())),
                        loc_box(Int(1)),
                        loc_box(Var("bar".to_string())),
                    ),
                "")
            )
        );
    }

    #[test]
    fn horizontal_line_comment() {
        assert_eq!(
            parse_without_loc("if 12345 then ##### Heading #####\n  54321 #whee!\n  else 1337"),
            Ok(
                (
                    If(
                        loc_box(Int(12345)),
                        loc_box(Int(54321)),
                        loc_box(Int(1337))
                    ),
                "")
            )
        );
    }

    // BLOCK COMMENT

    #[test]
    fn block_comment() {
        assert_eq!(
            parse_without_loc("if 12345### blah\n\nblah etc\nwhee #comment ###then\n  54321\n  else 1337"),
            Ok(
                (
                    If(
                        loc_box(Int(12345)),
                        loc_box(Int(54321)),
                        loc_box(Int(1337))
                    ),
                "")
            )
        );
    }

    // VARIANT

    #[test]
    fn basic_variant() {
        assert_eq!(
            parse_without_loc("Abc"),
            Ok((
                ApplyVariant("Abc".to_string(), None),
                ""
            ))
        );
    }

    #[test]
    fn variant_with_one_arg() {
        assert_eq!(
            parse_without_loc("Bbc 1"),
            Ok((
                ApplyVariant("Bbc".to_string(), Some(vec![loc(Int(1))])),
                ""
            ))
        );
    }

    #[test]
    fn variant_with_two_args() {
        assert_eq!(
            parse_without_loc("Bbc 1 2"),
            Ok((
                ApplyVariant("Bbc".to_string(), Some(vec![loc(Int(1)), loc(Int(2))])),
                ""
            ))
        );
    }

    #[test]
    fn variant_regression() {
        // Somehow parsing the variant "Abc" worked but "Foo" failed (?!)
        assert_eq!(
            parse_without_loc("F"),
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
            parse_without_loc("one = Abc\n\ntwo = Bar\n\none"),
            Ok((
                Assign(
                    loc(Identifier(
                        "one".to_string()
                    )),
                    loc_box(ApplyVariant(
                        "Abc".to_string(),
                        None
                    )),
                    loc_box(Assign(
                        loc(Identifier(
                            "two".to_string()
                        )),
                        loc_box(ApplyVariant(
                            "Bar".to_string(),
                            None
                        )),
                        loc_box(Var(
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
            CallByName("x".to_string(), vec![loc(Int(5))]),
            Operator(
                loc_box(Var("y".to_string())),
                loc(Plus),
                loc_box(CallByName("f".to_string(), vec![loc(Int(6))])),
            )
        );

        assert_eq!(
            parse_without_loc("(x 5)"),
            Ok((
                CallByName("x".to_string(), vec![loc(Int(5))]),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("(5)"),
            Ok((
                Int(5),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("((1905))"),
            Ok((
                Int(1905),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("6 + (685)"),
            Ok((
                Operator(
                    loc_box(Int(6)),
                    loc(Plus),
                    loc_box(Int(685))
                ),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("12 + 34"),
            Ok((
                Operator(
                    loc_box(Int(12)),
                    loc(Plus),
                    loc_box(Int(34))
                ),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("(51) + 19"),
            Ok((
                Operator(
                    loc_box(Int(51)),
                    loc(Plus),
                    loc_box(Int(19))
                ),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("(x 5) + 123"),
            Ok((
                Operator(
                    loc_box(CallByName("x".to_string(), vec![loc(Int(5))])),
                    loc(Plus),
                    loc_box(Int(123))
                ),
                "")
            )
        );

        assert_eq!(
            parse_without_loc("(x 5) + (2 * y)"),
            Ok((
                Operator(
                    loc_box(CallByName("x".to_string(), vec![loc(Int(5))])),
                    loc(Plus),
                    loc_box(
                        Operator(
                            loc_box(Int(2)),
                            loc(Star),
                            loc_box(Var("y".to_string()))
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
            parse_without_loc("abc =\n  y 1\n\nabc"),
            Ok((
                Assign(
                    loc(Identifier(
                        "abc".to_string()
                    )),
                    loc_box(CallByName(
                        "y".to_string(),
                        vec![
                            loc(Int(
                                1
                            ))
                        ]
                    )),
                    loc_box(Var(
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
            parse_without_loc("x = 5\n-10"),
            Ok((
                Assign(loc(Identifier("x".to_string())), loc_box(Int(5)), loc_box(Int(-10))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 in 10
            parse_without_loc("x=5\n-10"),
            Ok((
                Assign(loc(Identifier("x".to_string())), loc_box(Int(5)), loc_box(Int(-10))),
                "")
            )
        );
    }

    #[test]
    fn let_with_operator() {
        assert_eq!(
            // let x = 5 + 10 in -20
            parse_without_loc("x =(5 + 10)\n-20"),
            Ok((
                Assign(loc(Identifier("x".to_string())),
                    loc_box(Operator(loc_box(Int(5)), loc(Plus), loc_box(Int(10)))),
                    loc_box(Int(-20))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 + 10 in -20
            parse_without_loc("x=  5  +  10\n-20"),
            Ok((
                Assign(loc(Identifier("x".to_string())),
                    loc_box(Operator(loc_box(Int(5)), loc(Plus), loc_box(Int(10)))),
                    loc_box(Int(-20))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 + 10 in -20
            parse_without_loc("x=5\n    + 10\n-20"),
            Ok((
                Assign(loc(Identifier("x".to_string())),
                    loc_box(Operator(loc_box(Int(5)), loc(Plus), loc_box(Int(10)))),
                    loc_box(Int(-20))),
                "")
            )
        );
    }

    #[test]
    fn invalid_let_returning_number() {
        assert!(
            parse_without_loc("x=5\n    -10").is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn nested_let() {
        assert_eq!(
            // let x = 5 in let y = 12 in 3
            parse_without_loc("x = 5\ny = 12\n3"),
            Ok((
                Assign(loc(Identifier("x".to_string())),
                    loc_box(Int(5)),
                    loc_box(
                        Assign(loc(Identifier("y".to_string())), loc_box(Int(12)),
                            loc_box(Int(3))
                        ))),
                "")
            )
        );

        assert_eq!(
            // let x = 5 in let y = 12 in 3
            parse_without_loc("x = 5 - -3\ny = 12 + 7\n3 * -5"),
            Ok((
                Assign(loc(Identifier("x".to_string())),
                    loc_box(
                        Operator(
                            loc_box(Int(5)), loc(Minus), loc_box(Int(-3))
                        )
                    ),
                    loc_box(
                        Assign(loc(Identifier("y".to_string())),
                            loc_box(Operator(
                                loc_box(Int(12)), loc(Plus), loc_box(Int(7))
                            )),
                            loc_box(Operator(
                                loc_box(Int(3)), loc(Star), loc_box(Int(-5))
                            )),
                        ))),
                "")
            )
        );
    }

    #[test]
    fn let_returning_var() {
        assert_eq!(
            parse_without_loc("x=5\nx"),
            Ok((
                Assign(loc(Identifier("x".to_string())), loc_box(Int(5)), loc_box(Var("x".to_string()))),
                "")
            )
        );
    }

    #[test]
    fn bad_equals_indent_let() {
        assert!(
            parse_without_loc("  x=\n5\n\n5").is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn regression_on_calling_function_named_c() {
        // This was broken because case-expressions were greedily consuming 'c' characters for "case"
        assert_eq!(
            parse_without_loc("f = \\x => c 1\n\nf"),
            Ok((
                Assign(
                    loc(Identifier("f".to_string())),
                    loc_box(Closure(
                        vec![loc(Identifier("x".to_string()))],
                        loc_box(CallByName("c".to_string(), vec![loc(Int(1))]))
                    )),
                    loc_box(Var("f".to_string()))
                ),
                ""
            ))
        );
    }

    #[test]
    fn regression_on_passing_arguments_named_i() {
        // This was broken because if-expressions were greedily consuming 'i' characters for "if"
        assert_eq!(
            parse_without_loc("x i"),
            Ok((
                CallByName("x".to_string(), vec![loc(Var("i".to_string()))]),
                ""
            ))
        );
    }
}
