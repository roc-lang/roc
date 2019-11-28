#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
#[macro_use]
extern crate roc;

#[cfg(test)]
mod test_format {
    use bumpalo::collections::String;
    use bumpalo::Bump;
    use roc::fmt::def::fmt_def;
    use roc::fmt::expr::fmt_expr;
    use roc::fmt::module::fmt_module;
    use roc::parse;
    use roc::parse::ast::{Attempting, Expr};
    use roc::parse::blankspace::space0_before;
    use roc::parse::module::{module, module_defs};
    use roc::parse::parser::{Fail, Parser, State};

    fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Expr<'a>, Fail> {
        let state = State::new(&input, Attempting::Module);
        let parser = space0_before(loc!(parse::expr(0)), 0);
        let answer = parser.parse(&arena, state);

        answer
            .map(|(loc_expr, _)| loc_expr.value)
            .map_err(|(fail, _)| fail)
    }

    fn expr_formats_to(input: &str, expected: &str) {
        let arena = Bump::new();
        let input = input.trim_end();
        let expected = expected.trim_end();

        match parse_with(&arena, input) {
            Ok(actual) => {
                let mut buf = String::new_in(&arena);

                fmt_expr(&mut buf, &actual, 0, false);

                assert_eq!(buf, expected)
            },
            Err(error) => panic!("Unexpected parse failure when parsing this for formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", input, error)
        }
    }

    fn expr_formats_same(input: &str) {
        expr_formats_to(input, input);
    }

    fn module_formats_to(src: &str, expected: &str) {
        let arena = Bump::new();
        let src = src.trim_end();
        let expected = expected.trim_end();

        match module().parse(&arena, State::new(&src, Attempting::Module)) {
            Ok((actual, state)) => {
                let mut buf = String::new_in(&arena);

                fmt_module(&mut buf, &actual);

                match module_defs().parse(&arena, state) {
                    Ok((loc_defs, _)) => {
                        for loc_def in loc_defs {
                            fmt_def(&mut buf, arena.alloc(loc_def.value), 0);
                        }
                    }
                    Err(error) => panic!("Unexpected parse failure when parsing this for defs formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, error)
                }

                assert_eq!(buf, expected)
            },
            Err(error) => panic!("Unexpected parse failure when parsing this for module header formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, error)
        };
    }

    fn module_formats_same(input: &str) {
        module_formats_to(input, input);
    }

    // STRING LITERALS

    #[test]
    fn empty_string() {
        expr_formats_same(indoc!(
            r#"
            ""
            "#
        ));
    }

    #[test]
    fn basic_string() {
        expr_formats_same(indoc!(
            r#"
            "blah"
            "#
        ));
    }

    #[test]
    fn escaped_unicode_string() {
        expr_formats_same(indoc!(
            r#"
            "unicode: \u{A00A}!"
            "#
        ));
    }

    #[test]
    fn escaped_quote_string() {
        expr_formats_same(indoc!(
            r#"
            "\""
            "#
        ));
    }

    #[test]
    fn empty_block_string() {
        expr_formats_same(indoc!(
            r#"
            """"""
            "#
        ));
    }

    #[test]
    fn basic_block_string() {
        expr_formats_same(indoc!(
            r#"
            """blah"""
            "#
        ));
    }

    #[test]
    fn newlines_block_string() {
        expr_formats_same(indoc!(
            r#"
            """blah
                    spam
            foo"""
            "#
        ));
    }

    #[test]
    fn quotes_block_string() {
        expr_formats_same(indoc!(
            r#"
            """
 
            "" \""" ""\"

            """
            "#
        ));
    }

    #[test]
    fn zero() {
        expr_formats_same(indoc!(
            r#"
            0
            "#
        ));
    }

    #[test]
    fn zero_point_zero() {
        expr_formats_same(indoc!(
            r#"
            0.0
            "#
        ));
    }

    #[test]
    fn int_with_underscores() {
        expr_formats_same(indoc!(
            r#"
            1_23_456
            "#
        ));
    }

    #[test]
    fn float_with_underscores() {
        expr_formats_same(indoc!(
            r#"
            1_23_456.7_89_10
            "#
        ));
    }

    #[test]
    fn multi_arg_closure() {
        expr_formats_same(indoc!(
            r#"
            \a b c -> a b c
            "#
        ));
    }

    // DEFS

    #[test]
    fn single_def() {
        expr_formats_same(indoc!(
            r#"
            x = 5

            42
            "#
        ));
    }

    #[test]
    fn two_defs() {
        expr_formats_same(indoc!(
            r#"
            x = 5
            y = 10

            42
            "#
        ));
    }

    #[test]
    fn parenthetical_def() {
        expr_formats_same(indoc!(
            r#"
            (UserId userId) = 5
            y = 10

            42
            "#
        ));
    }

    #[test]
    fn record_destructuring() {
        expr_formats_same(indoc!(
            r#"
            { x, y } = 5
            { x: 5 } = { x: 5 }

            42
            "#
        ));
    }

    // #[test]
    // fn record_field_destructuring() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         case foo of
    //             { x: 5 } -> 42
    //         "#
    //     ));
    // }

    #[test]
    fn def_closure() {
        expr_formats_same(indoc!(
            r#"
            identity = \a -> a

            identity 42
            "#
        ));
    }

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        expr_formats_same("{}");
    }

    #[test]
    fn one_field() {
        expr_formats_same("{ x: 4 }");
    }

    #[test]
    fn two_fields() {
        expr_formats_same("{ x: 4, y: 42 }");
    }

    #[test]
    fn two_fields_newline() {
        expr_formats_same(indoc!(
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
        expr_formats_to(
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
        expr_formats_same(indoc!(
            r#"
            foo = 4

            { foo }
        "#
        ));
    }

    // IF

    #[test]
    fn single_line_if() {
        expr_formats_same(indoc!(
            r#"
            if foo bar then a b c else d e f
        "#
        ));

        expr_formats_same(indoc!(
            r#"
            if foo (a b c) then a b c else d e f
        "#
        ));
    }

    // CASE

    #[test]
    fn integer_case() {
        expr_formats_same(indoc!(
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
        expr_formats_same(indoc!(
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
    fn nested_case() {
        expr_formats_same(indoc!(
            r#"
            case b when
                _ ->
                    case c when
                        _ ->
                            1
        "#
        ));
    }

    #[test]
    fn case_with_moving_comments() {
        expr_formats_to(
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
        expr_formats_to(
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

    #[test]
    fn def_returning_closure() {
        expr_formats_same(indoc!(
            r#"
                    f = \x -> x
                    g = \x -> x

                    \x ->
                        a = f x
                        b = f x

                        x
                "#
        ));
    }

    // MODULES

    #[test]
    fn single_line_interface() {
        module_formats_same(indoc!(
            r#"
                interface Foo exposes [] imports []
            "#
        ));
    }

    #[test]
    fn multiline_interface() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes []
                    imports []
            "#
        ));
    }

    #[test]
    fn interface_exposing() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes [ Bar, Baz, a, b ]
                    imports []
            "#
        ));
    }

    #[test]
    fn interface_importing() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes [ Bar, Baz, a, b ]
                    imports [ Blah, Thing.{ foo, bar }, Stuff ]
            "#
        ));
    }
}
