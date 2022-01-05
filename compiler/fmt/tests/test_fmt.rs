#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc_fmt;

#[cfg(test)]
mod test_fmt {
    use bumpalo::Bump;
    use roc_fmt::annotation::{Formattable, Newlines, Parens};
    use roc_fmt::def::fmt_def;
    use roc_fmt::module::fmt_module;
    use roc_fmt::Buf;
    use roc_parse::module::{self, module_defs};
    use roc_parse::parser::Parser;
    use roc_parse::state::State;
    use roc_test_utils::assert_multiline_str_eq;

    // Not intended to be used directly in tests; please use expr_formats_to or expr_formats_same
    fn expect_format_expr_helper(input: &str, expected: &str) {
        let arena = Bump::new();
        match roc_parse::test_helpers::parse_expr_with(&arena, input.trim()) {
            Ok(actual) => {
                let mut buf = Buf::new_in(&arena);

                actual.format_with_options(&mut buf, Parens::NotNeeded, Newlines::Yes, 0);

                assert_multiline_str_eq!(expected, buf.as_str());
            }
            Err(error) => panic!("Unexpected parse failure when parsing this for formatting:\n\n{}\n\nParse error was:\n\n{:?}\n\n", input, error)
        };
    }

    fn expr_formats_to(input: &str, expected: &str) {
        let input = input.trim_end();
        let expected = expected.trim_end();

        // First check that input formats to the expected version
        expect_format_expr_helper(input, expected);

        // Parse the expected result format it, asserting that it doesn't change
        // It's important that formatting be stable / idempotent
        expect_format_expr_helper(expected, expected);
    }

    fn expr_formats_same(input: &str) {
        expr_formats_to(input, input);
    }

    // Not intended to be used directly in tests; please use module_formats_to or module_formats_same
    fn expect_format_module_helper(src: &str, expected: &str) {
        let arena = Bump::new();
        match module::parse_header(&arena, State::new(src.as_bytes())) {
            Ok((actual, state)) => {
                let mut buf = Buf::new_in(&arena);

                fmt_module(&mut buf, &actual);

                match module_defs().parse(&arena, state) {
                    Ok((_, loc_defs, _)) => {
                        for loc_def in loc_defs {
                            fmt_def(&mut buf, arena.alloc(loc_def.value), 0);
                        }
                    }
                    Err(error) => panic!("Unexpected parse failure when parsing this for defs formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, error)
                }
                assert_multiline_str_eq!(expected, buf.as_str())
            }
            Err(error) => panic!("Unexpected parse failure when parsing this for module header formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, error)
        };
    }

    fn module_formats_to(input: &str, expected: &str) {
        // First check that input formats to the expected version
        expect_format_module_helper(input, expected);

        // Parse the expected result format it, asserting that it doesn't change
        // It's important that formatting be stable / idempotent
        expect_format_module_helper(expected, expected);
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
    #[ignore]
    fn def_with_comment_on_same_line() {
        // TODO(joshuawarner32): make trailing comments format stabily
        // This test currently fails because the comment ends up as SpaceBefore for the following `a`
        // This works fine when formatted _once_ - but if you format again, the formatter wants to
        // insert a newline between `a = "Hello"` and the comment, further muddying the waters.
        // Clearly the formatter shouldn't be allowed to migrate a comment around like that.
        expr_formats_to(
            indoc!(
                r#"
                a = "Hello" # This variable is for greeting

                a
                "#
            ),
            indoc!(
                r#"
                a = "Hello"
                # This variable is for greeting
                a
                "#
            ),
        );
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
    fn force_space_at_beginning_of_comment() {
        expr_formats_to(
            indoc!(
                r#"
                #comment
                f
                "#
            ),
            indoc!(
                r#"
                # comment
                f
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
            "unicode: \u(A00A)!"
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

    // #[test]
    // fn empty_block_string() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         """"""
    //         "#
    //     ));
    // }

    // #[test]
    // fn basic_block_string() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         """blah"""
    //         "#
    //     ));
    // }

    // #[test]
    // fn newlines_block_string() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         """blah
    //                 spam
    //         foo"""
    //         "#
    //     ));
    // }

    // #[test]
    // fn quotes_block_string() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         """

    //         "" \""" ""\"

    //         """
    //         "#
    //     ));
    // }

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

    #[test]
    fn destructure_tag_closure() {
        expr_formats_same(indoc!(
            r#"
            \Foo a -> Foo a
            "#
        ));
    }

    #[test]
    fn destructure_nested_tag_closure() {
        expr_formats_same(indoc!(
            r#"
            \Foo (Bar a) -> Foo (Bar a)
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

    #[test]
    fn excess_parens() {
        expr_formats_to(
            indoc!(
                r#"
                x = (5)
    
    
                y = ((10))
    
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
    fn defs_with_defs() {
        expr_formats_to(
            indoc!(
                r#"
                x =
                    y = 4
                    z = 8
                    w

                x
                "#
            ),
            indoc!(
                r#"
                x =
                    y = 4
                    z = 8

                    w

                x
                "#
            ),
        );
    }

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
        expr_formats_to(
            indoc!(
                r#"
                # 9

                # A
                # B

                # C
                9
                "#
            ),
            indoc!(
                r#"
                # 9
                # A
                # B
                # C
                9
                "#
            ),
        );
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

        expr_formats_to(
            indoc!(
                r#"
                f = \x ->
                    # 1st




                    # 2nd
                    x

                f 4
                "#
            ),
            indoc!(
                r#"
                f = \x ->
                    # 1st
                    # 2nd
                    x

                f 4
                "#
            ),
        );
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
    fn record_field_destructuring() {
        expr_formats_same(indoc!(
            r#"
                when foo is
                    { x: 5 } ->
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

        expr_formats_to(
            indoc!(
                r#"
                {   shoes  &  rightShoe : nothing }
                "#
            ),
            indoc!(
                r#"
                { shoes & rightShoe: nothing }
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                {   shoes  &  rightShoe : nothing }
                "#
            ),
            indoc!(
                r#"
                { shoes & rightShoe: nothing }
                "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
            { shoes &
                rightShoe: newRightShoe,
                leftShoe: newLeftShoe,
            }
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                { shoes
                    & rightShoe: bareFoot
                    , leftShoe: bareFoot }
                "#
            ),
            indoc!(
                r#"
                { shoes &
                    rightShoe: bareFoot,
                    leftShoe: bareFoot,
                }
                "#
            ),
        );
    }

    #[test]
    fn final_comments_in_records() {
        expr_formats_same(indoc!(
            r#"
            {
                x: 42,
                # comment
            }"#
        ));

        expr_formats_same(indoc!(
            r#"
            {
                x: 42,
                # comment
                # other comment
            }"#
        ));
    }

    #[test]
    fn final_comments_without_comma_in_records() {
        expr_formats_to(
            indoc!(
                r#"
            {
                y: 41,
                # comment 1
                x: 42 # comment 2
            }"#
            ),
            indoc!(
                r#"
            {
                y: 41,
                # comment 1
                x: 42,
                # comment 2
            }"#
            ),
        );
    }

    #[test]
    fn multiple_final_comments_without_comma_in_records() {
        expr_formats_to(
            indoc!(
                r#"
            {
                y: 41,
                x: 42 # comment 1
                # comment 2
            }"#
            ),
            indoc!(
                r#"
            {
                y: 41,
                x: 42,
                # comment 1
                # comment 2
            }"#
            ),
        );
    }

    #[test]
    fn comments_with_newlines_in_records() {
        expr_formats_to(
            indoc!(
                r#"
            {
                z: 44 #comment 0
                ,
                y: 41, # comment 1

                # comment 2
                x: 42
                
                # comment 3
                
                # comment 4
            }"#
            ),
            indoc!(
                r#"
            {
                z: 44,
                # comment 0
                y: 41,
                # comment 1
                # comment 2
                x: 42,
                # comment 3
                # comment 4
            }"#
            ),
        );
    }

    #[test]
    fn multiple_final_comments_with_comma_in_records() {
        expr_formats_to(
            indoc!(
                r#"
            {
                y: 41,
                x: 42, # comment 1
                # comment 2
            }"#
            ),
            indoc!(
                r#"
            {
                y: 41,
                x: 42,
                # comment 1
                # comment 2
            }"#
            ),
        );
    }

    #[test]
    fn trailing_comma_in_record_annotation() {
        expr_formats_to(
            indoc!(
                r#"
                f: {                    y : Int *,
                                         x : Int * ,
                   }
                
                f"#
            ),
            indoc!(
                r#"
                f :
                    {
                        y : Int *,
                        x : Int *,
                    }

                f"#
            ),
        );
    }

    #[test]
    fn trailing_comma_in_record_annotation_same() {
        expr_formats_same(indoc!(
            r#"
                f :
                    {
                        y : Int *,
                        x : Int *,
                    }

                f"#
        ));
    }

    #[test]
    fn multiline_type_definition() {
        expr_formats_same(indoc!(
            r#"
                f :
                    Int *

                f"#
        ));
    }

    #[test]
    fn multiline_empty_record_type_definition() {
        expr_formats_same(indoc!(
            r#"
                f :
                    {}

                f"#
        ));
    }

    #[test]
    fn type_definition_comment_after_colon() {
        expr_formats_to(
            indoc!(
                r#"
                f : # comment
                    {}

                f"#
            ),
            indoc!(
                r#"
                f :
                    # comment
                    {}

                f"#
            ),
        );
    }

    #[test]
    #[ignore]
    fn final_comment_in_empty_record_type_definition() {
        expr_formats_to(
            indoc!(
                r#"
                f :
                    { # comment
                    }

                f"#
            ),
            indoc!(
                r#"
                f :
                    {
                        # comment
                    }
                
                f"#
            ),
        );
    }

    #[test]
    #[ignore]
    fn multiline_inside_empty_record_annotation() {
        expr_formats_same(indoc!(
            r#"
                f :
                    {
                    }

                f"#
        ));
    }

    #[test]
    fn final_comment_record_annotation() {
        expr_formats_to(
            indoc!(
                r#"
                f :
                    { 
                        x: Int * # comment 1
                        ,
                        # comment 2
                    }

                f"#
            ),
            indoc!(
                r#"
                f :
                    {
                        x : Int *,
                        # comment 1
                        # comment 2
                    }
                
                f"#
            ),
        );
    }

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

        expr_formats_to(
            indoc!(
                r#"
                    identity = \a
                        -> a

                    identity 41
                "#
            ),
            indoc!(
                r#"
                    identity = \a -> a

                    identity 41
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    identity = \a
                        -> 
                            a + b

                    identity 4010
                "#
            ),
            indoc!(
                r#"
                    identity = \a ->
                        a + b

                    identity 4010
                "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
            identity = \a, b -> a

            identity 43
            "#
        ));

        // expr_formats_same(indoc!(
        //     r#"
        //    identity =
        //        \{
        //            x,
        //            y
        //         }
        //        -> a
        //
        //    identity 43
        //    "#
        // ));

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

    #[test]
    fn closure_multiline_pattern() {
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
                9,
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
                    19,
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
                    29,
                ]
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                [
                    157, 158,
                    159
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    157,
                    158,
                    159,
                ]
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                [
                    557, 648,
                    759, 837
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    557,
                    648,
                    759,
                    837,
                ]
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                [
                    257, 358,
                    # Hey!
                    459
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    257,
                    358,
                    # Hey!
                    459,
                ]
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                [
                # Thirty Seven

                37
                # Thirty Eight
                , 38


                , 39
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    # Thirty Seven
                    37,
                    # Thirty Eight
                    38,
                    39,
                ]
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                [ # 47!
                # Top 47
                  47
                # Bottom 47
                # Top 48
                , 48
                # Bottom 48
                # Top 49
                , 49
                # Bottom 49
                # 49!
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    # 47!
                    # Top 47
                    47,
                    # Bottom 47
                    # Top 48
                    48,
                    # Bottom 48
                    # Top 49
                    49,
                    # Bottom 49
                    # 49!
                ]
                "#
            ),
        );
    }
    #[test]
    fn ending_comments_in_list() {
        expr_formats_to(
            indoc!(
                r#"
                [ # Top 49
                 49
                # Bottom 49
                ,
                # 49!
                ]
                "#
            ),
            indoc!(
                r#"
                [
                    # Top 49
                    49,
                    # Bottom 49
                    # 49!
                ]
                "#
            ),
        );
    }
    #[test]
    fn multi_line_list_def() {
        expr_formats_same(indoc!(
            r#"
                l =
                    [
                        1,
                        2,
                    ]

                l
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                results = [
                    Ok 4,
                    Ok 5
                    ]

                allOks results
                "#
            ),
            indoc!(
                r#"
                results =
                    [
                        Ok 4,
                        Ok 5,
                    ]

                allOks results
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    results =
                        # Let's count past 6
                        [
                        Ok 6,
                        Err CountError
                        ]

                    allOks results
                "#
            ),
            indoc!(
                r#"
                    results =
                        # Let's count past 6
                        [
                            Ok 6,
                            Err CountError,
                        ]

                    allOks results
                "#
            ),
        );
    }

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        expr_formats_same("{}");
    }

    #[test]
    #[ignore]
    fn empty_record_with_comment() {
        expr_formats_same(indoc!(
            r#"
            {
                # comment
            }"#
        ));
    }

    #[test]
    #[ignore]
    fn empty_record_with_newline() {
        expr_formats_to(
            indoc!(
                r#"
            {    
            }"#
            ),
            "{}",
        );
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
                y: 42,
            }
            "#
        ));
    }

    #[test]
    fn multi_line_record_def() {
        expr_formats_same(indoc!(
            r#"
                pos =
                    {
                        x: 4,
                        y: 11,
                        z: 16,
                    }

                pos
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                pos = {
                        x: 5,
                        y: 10,
                    }

                pos
                "#
            ),
            indoc!(
                r#"
                pos =
                    {
                        x: 5,
                        y: 10,
                    }

                pos
                "#
            ),
        );
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
                    y: 42,
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
    }

    #[test]
    fn multi_line_if_condition_with_spaces() {
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

    #[test]
    fn multi_line_if_condition_with_multi_line_expr_1() {
        expr_formats_same(indoc!(
            r#"
            if
                snowWillFall
                    pressure
                    temperature
            then
                bundleUp
            else
                identity
            "#
        ));
    }

    #[test]
    fn multi_line_if_condition_with_multi_line_expr_2() {
        expr_formats_same(indoc!(
            r#"
            if
                1
                    == 2
            then
                "yes"
            else
                "no"
            "#
        ));
    }

    #[test]
    fn if_removes_newlines_from_else() {
        expr_formats_to(
            indoc!(
                r#"
                if
                    isPrime 8
                then
                    nothing
                else
                    # C
                    # D

                    # E
                    # F

                    just (div 1 8)
                "#
            ),
            indoc!(
                r#"
                if
                    isPrime 8
                then
                    nothing
                else
                    # C
                    # D
                    # E
                    # F
                    just (div 1 8)
                "#
            ),
        );
    }

    #[test]
    fn if_removes_newlines_from_then() {
        expr_formats_to(
            indoc!(
                r#"
                if
                    isPrime 9
                then
                    # EE
                    # FF

                    nothing

                    # GG

                else
                    just (div 1 9)
                "#
            ),
            indoc!(
                r#"
                if
                    isPrime 9
                then
                    # EE
                    # FF
                    nothing
                    # GG
                else
                    just (div 1 9)
                "#
            ),
        );
    }

    #[test]
    fn if_removes_newlines_from_condition() {
        expr_formats_to(
            indoc!(
                r#"
                if

                    # Is

                    # It

                    isPrime 10

                    # Prime?

                then
                    nothing
                else
                    just (div 1 10)
                "#
            ),
            indoc!(
                r#"
                if
                    # Is
                    # It
                    isPrime 10
                    # Prime?
                then
                    nothing
                else
                    just (div 1 10)
                "#
            ),
        );
    }

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

    #[test]
    fn multi_line_application() {
        expr_formats_same(indoc!(
            r#"
            combine
                peanutButter
                chocolate
            "#
        ));
    }

    #[test]
    fn partial_multi_line_application() {
        expr_formats_to(
            indoc!(
                r#"
                mix vodka
                    tonic
                "#
            ),
            indoc!(
                r#"
                mix
                    vodka
                    tonic
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                f
                    a b c
                "#
            ),
            indoc!(
                r#"
                f
                    a
                    b
                    c
                "#
            ),
        );
    }

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
    fn when_with_alternatives_1() {
        expr_formats_same(indoc!(
            r#"
            when b is
                1 | 2 ->
                    when c is
                        6 | 7 ->
                            8

                3 | 4 ->
                    5
        "#
        ));
    }

    #[test]
    fn when_with_alternatives_2() {
        expr_formats_same(indoc!(
            r#"
            when b is
                # a comment here
                1 | 2 ->
                    # a comment there
                    1
        "#
        ));
    }

    #[test]
    fn when_with_alternatives_3() {
        expr_formats_to(
            indoc!(
                r#"
            when b is
            1   |   2 |3 ->

                    1
            "#
            ),
            indoc!(
                r#"
            when b is
                1 | 2 | 3 ->
                    1
                "#
            ),
        );
    }

    #[test]
    fn when_with_alternatives_4() {
        expr_formats_to(
            indoc!(
                r#"
            when b is
                1   |   2 |
                    3
                ->

                        4
                5   |   6 | 7 ->

                        8
                9
                 | 10 -> 11

                12 | 13 ->
                  when c is
                    14 | 15 -> 16
                    17
                     |  18 -> 19
                20 -> 21

            "#
            ),
            indoc!(
                r#"
            when b is
                1
                 | 2
                 | 3 ->
                    4

                5 | 6 | 7 ->
                    8

                9
                 | 10 ->
                    11

                12 | 13 ->
                    when c is
                        14 | 15 ->
                            16

                        17
                         | 18 ->
                            19

                20 ->
                    21
                "#
            ),
        );
    }

    #[test]
    fn with_multiline_pattern_indentation() {
        expr_formats_to(
            indoc!(
                r#"
            when b is   3->4
                        9
                         |8->9
            "#
            ),
            indoc!(
                r#"
            when b is
                3 ->
                    4

                9
                 | 8 ->
                    9
            "#
            ),
        );
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

    #[test]
    fn multi_line_when_condition_1() {
        expr_formats_same(indoc!(
            r#"
            when
                complexFunction a b c
            is
                1 ->
                    Nothing

                _ ->
                    Just True
            "#
        ));
    }

    #[test]
    fn multi_line_when_condition_2() {
        expr_formats_same(indoc!(
            r#"
            when
                # this is quite complicated
                complexFunction a b c
                # Watch out
            is
                Complex x y ->
                    simplify x y

                Simple z ->
                    z
            "#
        ));
    }

    #[test]
    fn multi_line_when_condition_3() {
        expr_formats_to(
            indoc!(
                r#"
            x = 2
            y = 3

            when 1
                + 1 is
                2 ->
                    x

                _ ->
                    y
            "#
            ),
            indoc!(
                r#"
            x = 2
            y = 3

            when
                1
                    + 1
            is
                2 ->
                    x

                _ ->
                    y
            "#
            ),
        );
    }

    #[test]
    fn multi_line_when_condition_4() {
        expr_formats_to(
            indoc!(
                r#"
            x = 2
            y = 3

            when 2
                + 2
            is
                4 ->
                    x

                _ ->
                    y
            "#
            ),
            indoc!(
                r#"
            x = 2
            y = 3

            when
                2
                    + 2
            is
                4 ->
                    x

                _ ->
                    y
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

    #[test]
    fn inner_def_with_triple_newline_before() {
        // The triple newline used to cause the code in add_spaces to not indent the next line,
        // which of course is not the same tree (and nor does it parse)
        expr_formats_to(
            indoc!(
                r#"
                \x ->
                    m = 2


                    m1 = insert m n powerOf10

                    42
                "#
            ),
            indoc!(
                r#"
                \x ->
                    m = 2

                    m1 = insert m n powerOf10

                    42
                "#
            ),
        );
    }

    #[test]
    fn when_guard() {
        expr_formats_same(indoc!(
            r#"
            when maybeScore is
                Just score if score > 21 ->
                    win

                _ ->
                    nextRound
            "#
        ));
    }

    #[test]
    fn when_guard_using_function() {
        expr_formats_same(indoc!(
            r#"
            when authenticationResponse is
                Ok user if hasPermission user ->
                    loadPage route user

                Ok user ->
                    PageNotFound

                Err _ ->
                    ErrorPage
            "#
        ));
    }

    // ACCESSOR

    #[test]
    fn accessor() {
        expr_formats_same(indoc!(
            r#"
            .id
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            user.name
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            (getUser userId users).name
            "#
        ));
    }

    // PRECEDENCE CONFLICT

    #[test]
    fn precedence_conflict() {
        expr_formats_same(indoc!(
            r#"
            if True == False == True then
                False
            else
                True
            "#
        ));
    }

    #[test]
    fn multi_line_precedence_conflict_1() {
        expr_formats_to(
            indoc!(
                r#"
            if True
                == False == True
            then
                False
            else
                True
            "#
            ),
            indoc!(
                r#"
            if
                True
                    == False
                    == True
            then
                False
            else
                True
            "#
            ),
        );
    }

    #[test]
    fn multi_line_precedence_conflict_2() {
        expr_formats_to(
            indoc!(
                r#"
            if False
                == False == False then
                "true"
            else
                "false"
            "#
            ),
            indoc!(
                r#"
            if
                False
                    == False
                    == False
            then
                "true"
            else
                "false"
            "#
            ),
        );
    }

    #[test]
    fn precedence_conflict_functions() {
        expr_formats_same(indoc!(
            r#"
            when f x == g y == h z is
                True ->
                    Ok 1

                False ->
                    Err 2
            "#
        ));
    }

    #[test]
    fn binop_parens() {
        expr_formats_same(indoc!(
            r#"
            if 4 == (6 ^ 6 ^ 7 ^ 8) then
                "Hard to believe"
            else
                "Naturally"
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            if 5 == 1 ^ 1 ^ 1 ^ 1 then
                "Not buying it"
            else
                "True"
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
            if (1 == 1)
                && (2 == 1) && (3 == 2) then
                "true"
            else
                "false"
            "#
            ),
            indoc!(
                r#"
            if
                (1 == 1)
                    && (2 == 1)
                    && (3 == 2)
            then
                "true"
            else
                "false"
            "#
            ),
        );
    }

    #[test]
    fn precedence_conflict_greater_than() {
        expr_formats_same(indoc!(
            r#"
            3 > 4 > 10
            "#
        ));
    }

    #[test]
    fn precedence_conflict_greater_than_and_less_than() {
        expr_formats_same(indoc!(
            r#"
            1 < 4 > 1
            "#
        ));
    }

    #[test]
    fn binop_if() {
        expr_formats_same(indoc!(
            r#"
            5 * (if x > 0 then 1 else 2)
            "#
        ));
    }

    // UNARY OP

    #[test]
    fn unary_op() {
        expr_formats_same(indoc!(
            r#"
                y = -4

                !x
            "#
        ));
    }

    #[test]
    fn unary_call_parens() {
        expr_formats_same(indoc!(
            r#"
                !(f 1)
            "#
        ));
    }

    #[test]
    fn unary_call_no_parens() {
        // TIL: Negating a function "does what you might expect"... which is cool!
        expr_formats_same(indoc!(
            r#"
                !f 1
            "#
        ));
    }

    // BINARY OP

    #[test]
    fn binary_op() {
        expr_formats_same(indoc!(
            r#"
            1 == 1
            "#
        ));
    }

    #[test]
    fn binary_op_with_spaces() {
        expr_formats_to(
            indoc!(
                r#"
                2   !=   3
                "#
            ),
            indoc!(
                r#"
                2 != 3
                "#
            ),
        );
    }

    #[test]
    fn multi_line_binary_op_1() {
        expr_formats_same(indoc!(
            r#"
            isLast
                && isEmpty
                && isLoaded
            "#
        ));
    }

    #[test]
    fn multi_line_binary_op_2() {
        expr_formats_same(indoc!(
            r#"
            x = 1
                < 2

            f x
            "#
        ));
    }

    #[test]
    fn multi_line_binary_op_with_comments() {
        expr_formats_to(
            indoc!(
                r#"
                1
                * 2
                / 3
                // 4
                "#
            ),
            indoc!(
                r#"
                1
                    * 2
                    / 3
                    // 4
                "#
            ),
        );
    }

    #[test]
    fn partial_multi_line_binary_op_1() {
        expr_formats_to(
            indoc!(
                r#"
                2 % 3
                    %% 5
                    + 7
                "#
            ),
            indoc!(
                r#"
                2
                    % 3
                    %% 5
                    + 7
                "#
            ),
        );
    }

    #[test]
    fn partial_multi_line_binary_op_2() {
        expr_formats_to(
            indoc!(
                r#"
                isGreenLight
                    && isRedLight && isYellowLight
                "#
            ),
            indoc!(
                r#"
                isGreenLight
                    && isRedLight
                    && isYellowLight
                "#
            ),
        );
    }

    #[test]
    fn pipline_op_with_apply() {
        expr_formats_same(indoc!(
            r#"
            output
                |> List.set (offset + 0) b
                |> List.set (offset + 1) a
            "#
        ));
    }

    // MODULES

    #[test]
    fn single_line_interface() {
        module_formats_same(indoc!(
            r#"
                interface Foo exposes [] imports []"#
        ));
    }

    #[test]
    fn defs_with_trailing_comment() {
        // TODO: make the formatter add a space between '42' and # below:
        module_formats_to(
            indoc!(
                r#"
            interface Foo exposes [] imports []
            a = 42 # Yay greetings"#
            ),
            indoc!(
                r#"
            interface Foo exposes [] imports []
            a = 42# Yay greetings
            "#
            ),
        );
    }

    #[test]
    fn multiline_interface() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes []
                    imports []"#
        ));
    }

    #[test]
    fn interface_exposing() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes [ Bar, Baz, a, b ]
                    imports []"#
        ));
    }

    #[test]
    fn interface_importing() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes [ Bar, Baz, a, b ]
                    imports [ Blah, Thing.{ foo, bar }, Stuff ]"#
        ));
    }

    #[test]
    fn single_line_app() {
        module_formats_same(indoc!(
            r#"
                app "Foo" packages { pf: "platform" } imports [] provides [ main ] to pf"#
        ));
    }

    #[test]
    fn single_line_platform() {
        module_formats_same(
            "platform \"folkertdev/foo\" \
            requires { model=>Model, msg=>Msg } { main : Effect {} } \
            exposes [] \
            packages {} \
            imports [ Task.{ Task } ] \
            provides [ mainForHost ] \
            effects fx.Effect \
            { \
                putLine : Str -> Effect {}, \
                putInt : I64 -> Effect {}, \
                getInt : Effect { value : I64, errorCode : [ A, B ], isError : Bool } \
            }",
        );
    }

    /// Annotations and aliases

    #[test]
    fn list_alias() {
        expr_formats_same(indoc!(
            r#"
            ConsList a : [ Cons a (ConsList a), Nil ]

            f : ConsList a -> ConsList a
            f = \_ -> Nil

            f
            "#
        ));
    }

    #[test]
    fn wildcard() {
        expr_formats_same(indoc!(
            r#"
            f : List *
            f = []

            a
            "#
        ));
    }

    #[test]
    fn identity() {
        expr_formats_same(indoc!(
            r#"
            f : a -> a
            f = []

            a
            "#
        ));
    }

    #[test]
    fn multiline_tag_union_annotation() {
        expr_formats_same(indoc!(
            r#"
            b :
                [
                    True,
                    False,
                ]

            b
            "#
        ));
    }

    #[test]
    fn multiline_tag_union_annotation_beginning_on_same_line() {
        expr_formats_same(indoc!(
            r#"
            Expr : [
                    Add Expr Expr,
                    Mul Expr Expr,
                    Val I64,
                    Var I64,
                ]

            Expr"#
        ));
    }

    #[test]
    fn multiline_tag_union_annotation_with_final_comment() {
        expr_formats_to(
            indoc!(
                r#"
            b :
                [
                    True,
                    # comment 1
                    False # comment 2
                    ,
                    # comment 3
                ]

            b
            "#
            ),
            indoc!(
                r#"
                b :
                    [
                        True,
                        # comment 1
                        False,
                        # comment 2
                        # comment 3
                    ]
    
                b
                "#
            ),
        );
    }

    #[test]
    fn tag_union() {
        expr_formats_same(indoc!(
            r#"
            f : [ True, False ] -> [ True, False ]
            f = \x -> x

            a
            "#
        ));
    }

    // TODO: the current formatting seems a bit odd for multiline function annotations
    // (beside weird indentation, note the trailing space after the "->")
    // #[test]
    // fn multiline_tag_union_function_annotation() {
    //     expr_formats_same(indoc!(
    //         r#"
    //         f :
    //             [
    //                 True,
    //                 False,
    //             ] ->
    //             [
    //                 True,
    //                 False,
    //             ]
    //         f = \x -> x

    //         a
    //         "#
    //     ));
    // }

    #[test]
    fn recursive_tag_union() {
        expr_formats_same(indoc!(
            r#"
            f : [ Cons a (ConsList a), Nil ] as ConsList a -> [ Just a, Nothing ]
            f = \list ->
                when list is
                    Nil ->
                        Nothing

                    Cons first _ ->
                        Just first

            f
            "#
        ));
    }

    #[test]
    fn function_application_package_type() {
        expr_formats_same(indoc!(
            r#"
            main : Task.Task {} []
            main = 42

            main
            "#
        ));
    }

    #[test]
    fn record_type() {
        expr_formats_same(indoc!(
            r#"
            f : { foo : Int * }
            f = { foo: 1000 }

            a
            "#
        ));
    }

    #[test]
    fn record_pattern_with_apply_guard() {
        expr_formats_same(indoc!(
            r#"
            when { x: 1 } is
                { x: Just 4 } ->
                    4
            "#
        ));
    }

    #[test]
    fn record_pattern_with_record_guard() {
        expr_formats_same(indoc!(
            r#"
            when { x: 1 } is
                { x: { x: True } } ->
                    4
            "#
        ));
    }

    #[test]
    fn body_starts_with_spaces_multiline() {
        expr_formats_same(indoc!(
            r#"
            y =
                Foo
                    1
                    2

            y
            "#
        ));
    }

    #[test]
    fn backpassing_simple() {
        expr_formats_same(indoc!(
            r#"
                getChar = \ctx ->
                    x <- Task.await (getCharScope scope)
                    42

                42
            "#
        ));
    }

    #[test]
    fn backpassing_apply_tag() {
        expr_formats_same(indoc!(
            r#"
                getChar = \ctx ->
                    (T val newScope) <- Task.await (getCharScope scope)
                    42

                42
            "#
        ));
    }

    #[test]
    fn backpassing_parens_body() {
        expr_formats_same(indoc!(
            r#"
            Task.fromResult
                (
                    a, b <- binaryOp ctx
                    if a == b then
                        -1
                    else
                        0
                )
            "#
        ));
    }

    #[test]
    fn backpassing_body_on_newline() {
        expr_formats_same(indoc!(
            r#"
                getChar = \ctx ->
                    x <-
                        Task.await (getCharScope scope)
                    42

                42
            "#
        ));
    }

    #[test]
    /// Test that everything under examples/ is formatted correctly
    /// If this test fails on your diff, it probably means you need to re-format the examples.
    /// Try this:
    /// `cargo run -- format $(find examples -name \*.roc)`
    fn test_fmt_examples() {
        let mut count = 0;
        let mut root = std::env::current_dir()
            .unwrap()
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_owned();
        root.push("examples");
        for entry in walkdir::WalkDir::new(&root) {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension() == Some(&std::ffi::OsStr::new("roc")) {
                count += 1;
                let src = std::fs::read_to_string(path).unwrap();
                println!("Now trying to format {}", path.display());
                module_formats_same(&src);
            }
        }
        assert!(
            count > 0,
            "Expecting to find at least 1 .roc file to format under {}",
            root.display()
        );
    }

    // this is a parse error atm
    //    #[test]
    //    fn multiline_apply() {
    //        expr_formats_same(indoc!(
    //            r#"
    //            f :
    //                Result a
    //                    { x : Int *
    //                    , y : Float
    //                    }
    //                    c
    //                -> Int *
    //            f =
    //                \_ -> 4
    //            "#
    //        ));
    //    }
}
