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

                fmt_expr(&mut buf, &actual, 0, false, true);

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
    fn def_with_comment() {
        expr_formats_same(indoc!(
            r#"
            # This variable is for greeting
            a = "Hello"

            a
            "#
        ));
    }

    #[test]
    fn def_with_comment_and_extra_space() {
        expr_formats_to(
            indoc!(
                r#"
            # This variable is for greeting




            a = "Hello"

            a
            "#
            ),
            indoc!(
                r#"
            # This variable is for greeting

            a = "Hello"

            a
            "#
            ),
        );
    }

    #[test]
    fn func_def() {
        expr_formats_same(indoc!(
            r#"
                f = \x, y ->
                    x

                f 4
            "#
        ));
    }

    #[test]
    fn new_line_above_return() {
        expr_formats_to(
            indoc!(
                r#"
                f = \x, y ->
                    y = 4
                    z = 8
                    x
                "string"
            "#
            ),
            indoc!(
                r#"
                f = \x, y ->
                    y = 4
                    z = 8

                    x

                "string"
            "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
                f = \x, y ->
                    a = 3
                    b = 6

                    c

                "string"
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
            \a, b, c -> a b c
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

    //    #[test]
    //    fn defs_with_defs() {
    //        expr_formats_to(indoc!(
    //            r#"
    //            x =
    //                y = 4
    //                z = 8
    //                w
    //
    //            x
    //            "#
    //        ), indoc!(
    //            r#"
    //            x =
    //                y = 4
    //                z = 8
    //
    //                w
    //
    //            x
    //            "#
    //        ));
    //    }

    #[test]
    fn comment_between_two_defs() {
        expr_formats_same(indoc!(
            r#"
            x = 5
            # Hello
            y = 10

            42
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            x = 5
            # Hello
            # two comments
            y = 10

            42
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            x = 5
            # Hello
            # two comments
            y = 10

            # v-- This is the return value

            42
            "#
        ));
    }

    #[test]
    fn space_between_comments() {
        expr_formats_same(indoc!(
            r#"
            # 9

            # A
            # B

            # C
            9
            "#
        ));
    }

    #[test]
    fn reduce_space_between_comments() {
        expr_formats_to(
            indoc!(
                r#"
                # First




                # Second
                x
                "#
            ),
            indoc!(
                r#"
                # First

                # Second
                x
                "#
            ),
        );

        //        expr_formats_to(
        //            indoc!(
        //                r#"
        //                f = \x ->
        //                    # 1st
        //
        //
        //
        //
        //                    # 2nd
        //                    x
        //
        //                f 4
        //                "#
        //            ),
        //            indoc!(
        //                r#"
        //                f = \x ->
        //                    # 1st
        //
        //                    # 2nd
        //                    x
        //
        //                f 4
        //                "#
        //            ),
        //        );
    }
    #[test]
    fn doesnt_detect_comment_in_comment() {
        expr_formats_same(indoc!(
            r#"
            # One Comment # Still one Comment
            9
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

        expr_formats_same(indoc!(
            r#"
            # A
            (UserId userId) = 5
            # B
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

    #[test]
    fn record_updating() {
        expr_formats_same(indoc!(
            r#"
            { shoes & leftShoe: nothing }
            "#
        ));

        expr_formats_to(indoc!(
            r#"
            {   shoes  &  rightShoe : nothing }
            "#
        ),indoc!(
            r#"
            { shoes & rightShoe: nothing }
            "#
        ));

        expr_formats_to(indoc!(
            r#"
            {   shoes  &  rightShoe : nothing }
            "#
        ),indoc!(
            r#"
            { shoes & rightShoe: nothing }
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            { shoes &
                rightShoe: newRightShoe,
                leftShoe: newLeftShoe
            }
            "#
        ));

        expr_formats_to(indoc!(
            r#"
            { shoes
                & rightShoe: bareFoot
                , leftShoe: bareFoot }
            "#
        ), indoc!(
            r#"
            { shoes &
                rightShoe: bareFoot,
                leftShoe: bareFoot
            }
            "#
        ));
    }

    // #[test]
    // fn record_field_destructuring() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         when foo is
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

        expr_formats_same(indoc!(
            r#"
            identity = \a ->
                a

            identity 44
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            identity = \a -> a

            # Hello
            identity 40
            "#
        ));

        //        expr_formats_to(indoc!(
        //            r#"
        //            identity = \a
        //                -> a
        //
        //            identity 41
        //            "#
        //        ), indoc!(
        //            r#"
        //            identity = \a ->
        //                a
        //
        //            identity 41
        //            "#
        //        ));

        expr_formats_same(indoc!(
            r#"
            identity = \a,
                b
                -> a

            identity 43
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            identity = \a,
                b,
                # it's c!!
                c
                -> a

            identity 43
            "#
        ));
    }

    // LIST
    #[test]
    fn empty_list() {
        expr_formats_same("[]");
        expr_formats_to("[     ]", "[]");
    }

    #[test]
    fn one_item_list() {
        expr_formats_same(indoc!("[ 4 ] "));
    }

    #[test]
    fn two_item_list() {
        expr_formats_same(indoc!("[ 7, 8 ] "));
        expr_formats_to(indoc!("[   7  ,   8  ] "), indoc!("[ 7, 8 ] "));
    }

    #[test]
    fn multi_line_list() {
        expr_formats_same(indoc!(
            r#"
            [
                7,
                8,
                9
            ]
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                [ 17
                , 18
                , 19
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    17,
                    18,
                    19
                ]
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                [ 27

                , 28


                , 29
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    27,
                    28,
                    29
                ]
                "#
            ),
        );

        //        expr_formats_to(indoc!(
        //            r#"
        //            [
        //            # Thirty Seven
        //
        //            37
        //            # Thirty Eight
        //            , 38
        //
        //
        //            , 39
        //            ]
        //            "#
        //        ), indoc!(
        //            r#"
        //            [
        //                # Thirty Seven
        //                37,
        //                # Thirty Eight
        //                38,
        //                39
        //            ]
        //            "#
        //        ));
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

    #[test]
    fn multi_line_if_condition() {
        expr_formats_same(indoc!(
            r#"
            if
                waterWillBoil pressure temperature
            then
                turnOnAc

            else
                identity
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                if


                    willBoil home water


                then
                    \_ -> leave

                else
                    identity
                "#
            ),
            indoc!(
                r#"
                if
                    willBoil home water
                then
                    \_ -> leave

                else
                    identity
                "#
            ),
        );
    }

    //    #[test]
    //    fn if_removes_newlines() {
    //        expr_formats_to(
    //            indoc!(
    //                r#"
    //                if
    //
    //                    # You never know!
    //                    isPrime 8
    //
    //                    # Top Comment
    //
    //                    # Bottom Comment
    //
    //
    //                then
    //
    //                    # A
    //
    //                    # B
    //
    //                    nothing
    //
    //                    # B again
    //
    //                else
    //
    //                    # C
    //                    # D
    //
    //                    # E
    //                    # F
    //
    //                    just (div 1 8)
    //                "#
    //            ),
    //            indoc!(
    //                r#"
    //                if
    //                    # You never know!
    //                    isPrime 8

    //                    # Top Comment

    //                    # Bottom Comment
    //                then
    //                    # A

    //                    # B

    //                    nothing

    //                    # B again
    //
    //                else
    //                    # C
    //                    # D

    //                    # E
    //                    # F
    //                    just (div 1 8)
    //                "#
    //            ),
    //        );
    //    }
    #[test]
    fn multi_line_if() {
        expr_formats_to(
            indoc!(
                r#"
                if lessThan four five then
                    four
                else
                    five
                "#
            ),
            indoc!(
                r#"
                if lessThan four five then
                    four

                else
                    five
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                if lessThan three four then


                    three




                else


                    four
                "#
            ),
            indoc!(
                r#"
                if lessThan three four then
                    three

                else
                    four
                "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
            if foo bar then
                a b c

            else
                d e f
            "#
        ));
    }

    //    fn multi_line_application() {
    //        expr_formats_same(indoc!(
    //            r#"
    //            combine
    //                peanutButter
    //                chocolate
    //            "#
    //        ));
    //    }

    // WHEN

    #[test]
    fn integer_when() {
        expr_formats_same(indoc!(
            r#"
            when b is
                1 ->
                    1

                _ ->
                    2
        "#
        ));
    }

    #[test]
    fn integer_when_with_space() {
        expr_formats_to(
            indoc!(
                r#"
            when year is
                1999 ->


                    1



                _ ->

                    0
        "#
            ),
            indoc!(
                r#"
            when year is
                1999 ->
                    1

                _ ->
                    0
            "#
            ),
        );
    }

    #[test]
    fn when_with_comments() {
        expr_formats_same(indoc!(
            r#"
            when b is
                # look at cases
                1 ->
                    # when 1
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
    fn nested_when() {
        expr_formats_same(indoc!(
            r#"
            when b is
                _ ->
                    when c is
                        _ ->
                            1
        "#
        ));
    }

    #[test]
    fn when_with_moving_comments() {
        expr_formats_to(
            indoc!(
                r#"
            when b is
                1 ->
                    1 # when 1

                # fall through
                _ ->
                    2
                "#
            ),
            indoc!(
                r#"
            when b is
                1 ->
                    1

                # when 1
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
