#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc_fmt;

#[cfg(test)]
mod test_fmt {
    use std::path::PathBuf;

    use bumpalo::Bump;
    use roc_fmt::annotation::{Formattable, Newlines, Parens};
    use roc_fmt::def::fmt_defs;
    use roc_fmt::module::fmt_module;
    use roc_fmt::Buf;
    use roc_parse::ast::Module;
    use roc_parse::module::{self, module_defs};
    use roc_parse::parser::Parser;
    use roc_parse::state::State;
    use roc_test_utils::{assert_multiline_str_eq, workspace_root};

    /// Check that the given input is formatted to the given output.
    /// If the expected output is `None`, then this only checks that the input
    /// parses without error, formats without error, and
    /// (optionally, based on the value of `check_stability`) re-parses to
    /// the same AST as the original.
    fn expr_formats(input: &str, check_formatting: impl Fn(&str), check_stability: bool) {
        let arena = Bump::new();
        let input = input.trim();

        match roc_parse::test_helpers::parse_expr_with(&arena, input) {
            Ok(actual) => {
                use roc_fmt::spaces::RemoveSpaces;

                let mut buf = Buf::new_in(&arena);

                actual.format_with_options(&mut buf, Parens::NotNeeded, Newlines::Yes, 0);

                let output = buf.as_str();

                check_formatting(output);

                let reparsed_ast = roc_parse::test_helpers::parse_expr_with(&arena, output).unwrap_or_else(|err| {
                    panic!(
                        "After formatting, the source code no longer parsed!\n\n\
                        Parse error was: {:?}\n\n\
                        The code that failed to parse:\n\n{}\n\n\
                        The original ast was:\n\n{:#?}\n\n",
                        err, output, actual
                    );
                });

                let ast_normalized = actual.remove_spaces(&arena);
                let reparsed_ast_normalized = reparsed_ast.remove_spaces(&arena);

                // HACK!
                // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
                // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
                // I don't have the patience to debug this right now, so let's leave it for another day...
                // TODO: fix PartialEq impl on ast types
                if format!("{:?}", ast_normalized) != format!("{:?}", reparsed_ast_normalized) {
                    panic!(
                        "Formatting bug; formatting didn't reparse to the same AST (after removing spaces)\n\n\
                        * * * Source code before formatting:\n{}\n\n\
                        * * * Source code after formatting:\n{}\n\n",
                        input,
                        output
                    );
                }

                // Now verify that the resultant formatting is _stable_ - i.e. that it doesn't change again if re-formatted
                if check_stability {
                    let mut reformatted_buf = Buf::new_in(&arena);
                    reparsed_ast.format_with_options(&mut reformatted_buf, Parens::NotNeeded, Newlines::Yes, 0);

                    if output != reformatted_buf.as_str() {
                        eprintln!("Formatting bug; formatting is not stable. Reformatting the formatted code changed it again, as follows:\n\n");

                        assert_multiline_str_eq!(output, reformatted_buf.as_str());
                    }
                }
            }
            Err(error) => panic!("Unexpected parse failure when parsing this for formatting:\n\n{}\n\nParse error was:\n\n{:?}\n\n", input, error)
        };
    }

    fn check_formatting(expected: &'_ str) -> impl Fn(&str) + '_ {
        let expected = expected.trim();
        move |output| {
            assert_multiline_str_eq!(expected, output);
        }
    }

    fn expr_formats_to(input: &str, expected: &str) {
        expr_formats(input, check_formatting(expected), true);
    }

    fn expr_formats_same(input: &str) {
        expr_formats(input, check_formatting(input), true);
    }

    fn fmt_module_and_defs<'a>(
        arena: &Bump,
        src: &str,
        module: &Module<'a>,
        state: State<'a>,
        buf: &mut Buf<'_>,
    ) {
        fmt_module(buf, module);

        match module_defs().parse(arena, state, 0) {
            Ok((_, loc_defs, _)) => {
                fmt_defs(buf, &loc_defs, 0);
            }
            Err(error) => panic!(
                r"Unexpected parse failure when parsing this for defs formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n",
                src, error
            ),
        }
    }

    // Not intended to be used directly in tests; please use module_formats_to or module_formats_same
    fn expect_format_module_helper(src: &str, expected: &str) {
        let arena = Bump::new();
        let src = src.trim();
        let expected = expected.trim();

        match module::parse_header(&arena, State::new(src.as_bytes())) {
            Ok((actual, state)) => {
                use roc_fmt::spaces::RemoveSpaces;

                let mut buf = Buf::new_in(&arena);

                fmt_module_and_defs(&arena, src, &actual, state, &mut buf);

                let output = buf.as_str().trim();

                let (reparsed_ast, state) = module::parse_header(&arena, State::new(output.as_bytes())).unwrap_or_else(|err| {
                    panic!(
                        "After formatting, the source code no longer parsed!\n\nParse error was: {:?}\n\nThe code that failed to parse:\n\n{}\n\n",
                        err, output
                    );
                });

                let ast_normalized = actual.remove_spaces(&arena);
                let reparsed_ast_normalized = reparsed_ast.remove_spaces(&arena);

                // HACK!
                // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
                // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
                // I don't have the patience to debug this right now, so let's leave it for another day...
                // TODO: fix PartialEq impl on ast types
                if format!("{:?}", ast_normalized) != format!("{:?}", reparsed_ast_normalized) {
                    panic!(
                        "Formatting bug; formatting didn't reparse to the same AST (after removing spaces)\n\n\
                        * * * Source code before formatting:\n{}\n\n\
                        * * * Source code after formatting:\n{}\n\n",
                        src,
                        output
                    );
                }

                // Now verify that the resultant formatting is _stable_ - i.e. that it doesn't change again if re-formatted
                let mut reformatted_buf = Buf::new_in(&arena);

                fmt_module_and_defs(&arena, output, &reparsed_ast, state, &mut reformatted_buf);

                let reformatted = reformatted_buf.as_str().trim();

                if output != reformatted {
                    eprintln!("Formatting bug; formatting is not stable. Reformatting the formatted code changed it again, as follows:\n\n");

                    assert_multiline_str_eq!(output, reformatted);
                }

                // If everything was idempotent re-parsing worked, finally assert
                // that the formatted code was what we expected it to be.
                //
                // Do this last because if there were any serious problems with the
                // formatter (e.g. it wasn't idempotent), we want to know about
                // those more than we want to know that the expectation failed!
                assert_multiline_str_eq!(expected, output);
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
    fn comment_with_trailing_space() {
        expr_formats_to(
            &format!(
                indoc!(
                    r#"
            # first comment{space}
            x = 0 # second comment{space}

            x
            "#
                ),
                space = " ",
            ),
            indoc!(
                r#"
            # first comment
            x = 0 # second comment

            x
            "#
            ),
        );
    }

    #[test]
    fn def_with_inline_comment() {
        expr_formats_same(indoc!(
            r#"
            x = 0 # comment

            x
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                x = 0# comment

                x
                "#
            ),
            indoc!(
                r#"
                x = 0 # comment

                x
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                x = 0# comment
                x
                "#
            ),
            indoc!(
                r#"
                x = 0 # comment

                x
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                x = 0  # comment

                x
                "#
            ),
            indoc!(
                r#"
                x = 0 # comment

                x
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
    fn type_annotation_allow_blank_line_before_and_after_comment() {
        expr_formats_same(indoc!(
            r#"
                person : {
                    firstName : Str,
                    # comment
                    lastName : Str,
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person : {
                    firstName : Str,

                    # comment
                    lastName : Str,
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person : {
                    firstName : Str,
                    # comment

                    lastName : Str,
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person : {
                    firstName : Str,

                    # comment

                    lastName : Str,
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person : {
                    firstName : Str,

                    # comment 1

                    lastName : Str,

                    # comment 2
                    # comment 3
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person : {
                    firstName : Str,

                    # comment 1

                    lastName : Str,
                    # comment 2
                    # comment 3
                }

                person
                "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                person : {

                    # comment

                    firstName : Str,
                    lastName : Str,
                }

                person
                "#
            ),
            indoc!(
                r#"
                person : {
                    # comment

                    firstName : Str,
                    lastName : Str,
                }

                person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                person : {
                    firstName : Str,
                    lastName : Str,

                    # comment

                }

                person
                "#
            ),
            indoc!(
                r#"
                person : {
                    firstName : Str,
                    lastName : Str,

                    # comment
                }

                person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                person : {
                    firstName : Str,


                    # comment
                    lastName : Str,
                }

                person
                "#
            ),
            indoc!(
                r#"
                person : {
                    firstName : Str,

                    # comment
                    lastName : Str,
                }

                person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                person : {
                    firstName : Str,
                    # comment


                    lastName : Str,
                }

                person
                "#
            ),
            indoc!(
                r#"
                person : {
                    firstName : Str,
                    # comment

                    lastName : Str,
                }

                person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                person : {
                    firstName : Str,


                    # comment


                    lastName : Str,
                }

                person
                "#
            ),
            indoc!(
                r#"
                person : {
                    firstName : Str,

                    # comment

                    lastName : Str,
                }

                person
                "#
            ),
        );
    }

    #[test]
    fn record_allow_blank_line_before_and_after_comment() {
        expr_formats_same(indoc!(
            r#"
                person = {
                    firstName: "first",
                    # comment 1
                    lastName: "last",
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person = {
                    firstName: "first",
                    # comment 1

                    lastName: "last",
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person = {
                    firstName: "first",

                    # comment 1
                    lastName: "last",
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person = {
                    firstName: "first",

                    # comment 1

                    lastName: "last",
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person = {
                    firstName: "first",

                    # comment 1

                    lastName: "last",

                    # comment 2
                    # comment 3
                }

                person
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                person = {
                    firstName: "first",

                    # comment 1

                    lastName: "last",
                    # comment 2
                    # comment 3
                }

                person
                "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    person = {

                        # comment

                        firstName: "first",
                        lastName: "last",
                    }

                    person
                "#
            ),
            indoc!(
                r#"
                    person = {
                        # comment

                        firstName: "first",
                        lastName: "last",
                    }

                    person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    person = {
                        firstName: "first",
                        lastName: "last",

                        # comment

                    }

                    person
                "#
            ),
            indoc!(
                r#"
                    person = {
                        firstName: "first",
                        lastName: "last",

                        # comment
                    }

                    person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    person = {
                        firstName: "first",


                        # comment 1
                        lastName: "last",
                    }

                    person
                "#
            ),
            indoc!(
                r#"
                    person = {
                        firstName: "first",

                        # comment 1
                        lastName: "last",
                    }

                    person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    person = {
                        firstName: "first",
                        # comment 1


                        lastName: "last",
                    }

                    person
                "#
            ),
            indoc!(
                r#"
                    person = {
                        firstName: "first",
                        # comment 1

                        lastName: "last",
                    }

                    person
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    person = {
                        firstName: "first",


                        # comment 1


                        lastName: "last",
                    }

                    person
                "#
            ),
            indoc!(
                r#"
                    person = {
                        firstName: "first",

                        # comment 1

                        lastName: "last",
                    }

                    person
                "#
            ),
        );
    }

    #[test]
    fn list_allow_blank_line_before_and_after_comment() {
        expr_formats_same(indoc!(
            r#"
                list = [
                    0,
                    # comment
                    1,
                ]

                list
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                list = [
                    0,

                    # comment
                    1,
                ]

                list
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                list = [
                    0,
                    # comment

                    1,
                ]

                list
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                list = [
                    0,

                    # comment

                    1,
                ]

                list
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                list = [
                    0,

                    # comment 1

                    1,

                    # comment 2
                    # comment 3
                ]

                list
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                list = [
                    0,

                    # comment 1

                    1,
                    # comment 2
                    # comment 3
                ]

                list
                "#
        ));
        expr_formats_to(
            indoc!(
                r#"
                list = [

                    # comment

                    0,
                    1,
                ]

                list
                "#
            ),
            indoc!(
                r#"
                list = [
                    # comment

                    0,
                    1,
                ]

                list
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                list = [
                    0,
                    1,

                    # comment

                ]

                list
                "#
            ),
            indoc!(
                r#"
                list = [
                    0,
                    1,

                    # comment
                ]

                list
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                list = [
                    0,


                    # comment
                    1,
                ]

                list
                "#
            ),
            indoc!(
                r#"
                list = [
                    0,

                    # comment
                    1,
                ]

                list
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                list = [
                    0,
                    # comment


                    1,
                ]

                list
                "#
            ),
            indoc!(
                r#"
                list = [
                    0,
                    # comment

                    1,
                ]

                list
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                list = [
                    0,


                    # comment


                    1,
                ]

                list
                "#
            ),
            indoc!(
                r#"
                list = [
                    0,

                    # comment

                    1,
                ]

                list
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

    #[test]
    fn empty_block_string() {
        expr_formats_same(indoc!(
            r#"
            """
            """
            "#
        ));
    }

    #[test]
    fn oneline_empty_block_string() {
        expr_formats_to(
            indoc!(
                r#"
                """"""
                "#
            ),
            indoc!(
                r#"
                """
                """
                "#
            ),
        );
    }

    #[test]
    fn basic_block_string() {
        expr_formats_to(
            indoc!(
                r#"
                """griffin"""
                "#
            ),
            indoc!(
                r#"
                "griffin"
                "#
            ),
        );
    }

    #[test]
    fn multiline_basic_block_string() {
        expr_formats_to(
            indoc!(
                r#"
                """griffin
                harpy"""
                "#
            ),
            indoc!(
                r#"
                """
                griffin
                harpy
                """
                "#
            ),
        );
    }

    #[test]
    fn newlines_block_string() {
        expr_formats_to(
            indoc!(
                r#"
                """griffin
                        harpy
                phoenix"""
                "#
            ),
            indoc!(
                r#"
                """
                griffin
                        harpy
                phoenix
                """
                "#
            ),
        );
    }

    #[test]
    fn quotes_block_string_single_segment() {
        expr_formats_same(indoc!(
            r#"
            """
            "griffin"
            """
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
    fn lambda_returns_record() {
        expr_formats_same(indoc!(
            r#"
                toRecord = \_ -> {
                    x: 1,
                    y: 2,
                    z: 3,
                }

                toRecord
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                    func = \_ ->
                        { x: 1, y: 2, z: 3 }

                    func
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                toRecord = \_ ->
                    val = 0

                    {
                        x: 1,
                        y: 2,
                        z: 3,
                    }

                toRecord
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    toRecord = \_ ->
                        {
                            x: 1,
                            y: 2,
                            z: 3,
                        }

                    toRecord
                "#
            ),
            indoc!(
                r#"
                    toRecord = \_ -> {
                        x: 1,
                        y: 2,
                        z: 3,
                    }

                    toRecord
                "#
            ),
        );
    }

    #[test]
    fn lambda_returns_list() {
        expr_formats_same(indoc!(
            r#"
                toList = \_ -> [
                    1,
                    2,
                    3,
                ]

                toList
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                    func = \_ ->
                        [1, 2, 3]

                    func
                "#
        ));

        expr_formats_same(indoc!(
            r#"
                toList = \_ ->
                    val = 0

                    [
                        1,
                        2,
                        3,
                    ]

                toList
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    toList = \_ ->
                        [
                            1,
                            2,
                            3,
                        ]

                    toList
                "#
            ),
            indoc!(
                r#"
                    toList = \_ -> [
                        1,
                        2,
                        3,
                    ]

                    toList
                "#
            ),
        );
    }

    #[test]
    fn multiline_list_func_arg() {
        expr_formats_same(indoc!(
            r#"
                    result = func arg [
                        1,
                        2,
                        3,
                    ]

                    result
                "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    result = func arg
                        [ 1, 2, 3 ]

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func
                        arg
                        [1, 2, 3]

                    result
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    result = func arg [
                            1,
                            2,
                            3,
                        ]

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func arg [
                        1,
                        2,
                        3,
                    ]

                    result
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    result = func [
                            1,
                            2,
                            3,
                        ]
                        arg

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func
                        [
                            1,
                            2,
                            3,
                        ]
                        arg

                    result
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    result = func arg
                        [
                            1,
                            2,
                            3,
                        ]

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func arg [
                        1,
                        2,
                        3,
                    ]

                    result
                "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
                    result = func
                        arg
                        [
                            1,
                            2,
                            3,
                        ]

                    result
                "#
        ));
    }

    #[test]
    fn multiline_record_func_arg() {
        expr_formats_same(indoc!(
            r#"
                result = func arg {
                    x: 1,
                    y: 2,
                    z: 3,
                }

                result
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    result = func arg
                        { x: 1, y: 2, z: 3 }

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func
                        arg
                        { x: 1, y: 2, z: 3 }

                    result
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    result = func arg {
                            x: 1,
                            y: 2,
                            z: 3,
                        }

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func arg {
                        x: 1,
                        y: 2,
                        z: 3,
                    }

                    result
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    result = func {
                            x: 1,
                            y: 2,
                            z: 3,
                        }
                        arg

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func
                        {
                            x: 1,
                            y: 2,
                            z: 3,
                        }
                        arg

                    result
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    result = func arg
                        {
                            x: 1,
                            y: 2,
                            z: 3,
                        }

                    result
                "#
            ),
            indoc!(
                r#"
                    result = func arg {
                        x: 1,
                        y: 2,
                        z: 3,
                    }

                    result
                "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
                    result = func
                        arg
                        {
                            x: 1,
                            y: 2,
                            z: 3,
                        }

                    result
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
                f : {
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
                f : {
                    y : Int *,
                    x : Int *,
                }

                f
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    f :
                        {
                            y : Int *,
                            x : Int *,
                        }

                    f
                "#
            ),
            indoc!(
                r#"
                    f : {
                        y : Int *,
                        x : Int *,
                    }

                    f
                "#
            ),
        );
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

                f
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                f :
                    {
                    }

                f
            "#
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
                f : {
                    # comment
                }

                f"#
            ),
        );
    }

    #[test]
    fn multiline_curly_brace_type() {
        expr_formats_same(indoc!(
            r#"
                x : {
                    a : Int,
                }

                x
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                x :
                    { a : Int }

                x
            "#
        ));
    }

    #[test]
    fn multiline_brace_type() {
        expr_formats_same(indoc!(
            r#"
                x : [
                    Int,
                ]

                x
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                x :
                    [Int]

                x
            "#
        ));
    }

    #[test]
    fn multiline_fn_signature() {
        expr_formats_same(indoc!(
            r#"
                foo :
                    Str,
                    Nat
                    -> Bool

                foo
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    foo :
                        Str, Int, Nat -> Bool

                    foo
                "#
            ),
            indoc!(
                r#"
                    foo :
                        Str,
                        Int,
                        Nat
                        -> Bool

                    foo
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    foo :
                        Str,
                        Nat -> Bool

                    foo
                "#
            ),
            indoc!(
                r#"
                    foo :
                        Str,
                        Nat
                        -> Bool

                    foo
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    foo :

                        Str,
                        Nat

                        -> Bool

                    foo
                "#
            ),
            indoc!(
                r#"
                    foo :
                        Str,
                        Nat
                        -> Bool

                    foo
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    foo :

                        Str, Nat -> Bool

                    foo
                "#
            ),
            indoc!(
                r#"
                    foo :
                        Str,
                        Nat
                        -> Bool

                    foo
                "#
            ),
        );
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
                f : {
                    x : Int *, # comment 1
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
        expr_formats_same(indoc!("[4]"));
        expr_formats_to(indoc!("[ 4 ]"), indoc!("[4]"));
    }

    #[test]
    fn two_item_list() {
        expr_formats_same(indoc!("[7, 8]"));
        expr_formats_to(indoc!("[   7  ,   8  ]"), indoc!("[7, 8]"));
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
                l = [
                    1,
                    2,
                ]

                l
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                l =
                    [1, 2]

                l
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                l =
                    [
                        1,
                        2,
                    ]

                l
                "#
            ),
            indoc!(
                r#"
                l = [
                    1,
                    2,
                ]

                l
                "#
            ),
        );

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
                results = [
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
        expr_formats_to("{ }", "{}");
    }

    #[test]
    fn empty_record_patterns() {
        expr_formats_to(
            indoc!(
                r#"
                    f = \{  } -> "Hello World"

                    f
                "#
            ),
            indoc!(
                r#"
                    f = \{} -> "Hello World"

                    f
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    f = \a, b -> {  }

                    f
                "#
            ),
            indoc!(
                r#"
                    f = \a, b -> {}

                    f
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    { } <- f a b

                    {}
                "#
            ),
            indoc!(
                r#"
                    {} <- f a b

                    {}
                "#
            ),
        );
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
                pos = {
                    x: 4,
                    y: 11,
                    z: 16,
                }

                pos
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                pos =
                    { x: 4, y: 11, z: 16 }

                pos
            "#
        ));

        expr_formats_same(indoc!(
            r#"
                myDef =
                    list = [
                        a,
                        b,
                    ]

                    {
                        c,
                        d,
                    }

                myDef
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    pos =
                        {
                            x: 4,
                            y: 11,
                            z: 16,
                        }

                    pos
                "#
            ),
            indoc!(
                r#"
                    pos = {
                        x: 4,
                        y: 11,
                        z: 16,
                    }

                    pos
                "#
            ),
        );

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
                pos = {
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
                1 -> # when 1
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
    fn when_with_integer_comments() {
        expr_formats_same(indoc!(
            r#"
                when 0 is
                    1 # comment
                    | 2 -> "a"

                    _ -> "b"
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
    fn def_when() {
        expr_formats_same(indoc!(
            r#"
            myLongFunctionName = \x ->
                when b is
                    1 | 2 ->
                        when c is
                            6 | 7 ->
                                8

                    3 | 4 ->
                        5

            123
        "#
        ));
    }

    #[test]
    #[ignore] // TODO: reformat when-in-function-body with extra newline
    fn def_when_with_python_indentation() {
        expr_formats_to(
            // vvv Currently this input formats to _itself_ :( vvv
            // Instead, if the body of the `when` is multiline (the overwhelmingly common case)
            // we want to make sure the `when` is at the beginning of the line, inserting
            // a newline if necessary.
            indoc!(
                r#"
                myLongFunctionName = \x -> when b is
                    1 | 2 ->
                        when c is
                            6 | 7 ->
                                8

                    3 | 4 ->
                        5

                123
            "#
            ),
            indoc!(
                r#"
                myLongFunctionName = \x ->
                    when b is
                        1 | 2 ->
                            when c is
                                6 | 7 ->
                                    8

                        3 | 4 ->
                            5

                123
            "#
            ),
        );
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
                | 10 -> 11

                12 | 13 ->
                    when c is
                        14 | 15 -> 16
                        17
                        | 18 -> 19

                20 -> 21
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
                3 -> 4
                9
                | 8 -> 9
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

    #[test]
    fn multi_line_when_branch() {
        expr_formats_to(
            indoc!(
                r#"
            when x is
                Foo -> bar
                    "arg1" "arg2"
                Bar -> 2
            "#
            ),
            indoc!(
                r#"
            when x is
                Foo ->
                    bar
                        "arg1"
                        "arg2"

                Bar -> 2
            "#
            ),
        );
    }

    #[test]
    fn single_line_when_patterns() {
        expr_formats_same(indoc!(
            r#"
            when x is
                Foo -> 1
                Bar -> 2
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            when x is
                Foo -> 1
                Bar ->
                    2
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                when x is
                    Foo -> 1

                    Bar ->
                        2
                "#
            ),
            indoc!(
                r#"
                when x is
                    Foo -> 1
                    Bar ->
                        2
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                when x is
                    Foo -> 1

                    Bar -> 2
                "#
            ),
            indoc!(
                r#"
                when x is
                    Foo -> 1
                    Bar -> 2
                "#
            ),
        );
    }

    #[test]
    fn when_with_single_quote_char() {
        expr_formats_same(indoc!(
            r#"
                when x is
                    '0' -> 0
                    '1' -> 1
                "#
        ));
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
    fn multiline_binop_with_comments() {
        expr_formats_to(
            indoc!(
                r#"
            x = 1
                + 1 # comment 1
                - 1 # comment 2
                * 1 # comment 3

            x
            "#
            ),
            indoc!(
                r#"
            x =
                1
                + 1 # comment 1
                - 1 # comment 2
                * 1 # comment 3

            x
            "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
            x = 1
                + 1 # comment 1
                * 1 # comment 2

            x
            "#
            ),
            indoc!(
                r#"
            x =
                1
                + 1 # comment 1
                * 1 # comment 2

            x
            "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
            x = 1
                + 1 # comment

            x
            "#
            ),
            indoc!(
                r#"
            x =
                1
                + 1 # comment

            x
            "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
            x = 1
                * 1
                + 1 # comment

            x
            "#
            ),
            indoc!(
                r#"
            x =
                1
                * 1
                + 1 # comment

            x
            "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
            x = 1
                - 1
                * 1
                + 1

            x
            "#
            ),
            indoc!(
                r#"
            x =
                1
                - 1
                * 1
                + 1

            x
            "#
            ),
        );
    }

    #[test]
    fn multiline_binop_if_with_comments() {
        expr_formats_same(indoc!(
            r#"
            if
                x
                + 1 # comment 1
                > 0 # comment 2
            then
                y
                * 2 # comment 3
                < 1 # comment 4
            else
                42
            "#
        ));
    }

    #[test]
    fn multiline_binop_when_with_comments() {
        expr_formats_to(
            indoc!(
                r#"
            when
                x
                + 1 # comment 1
                > 0 # comment 2
            is
                y ->
                    3
                    * 2 # comment 3
                    < 1 # comment 4

                z ->
                    4
                        / 5  # comment 5
                            < 1    # comment 6

                46 # first pattern comment
                | 95 # alternative comment 1
                | 126 # alternative comment 2
                | 150 -> # This comment came after the ->
                    # This comment is for the expr
                        foo bar
                            |> Result.withDefault "" # one last comment

                _ ->
                    42
            "#
            ),
            indoc!(
                r#"
            when
                x
                + 1 # comment 1
                > 0 # comment 2
            is
                y ->
                    3
                    * 2 # comment 3
                    < 1 # comment 4

                z ->
                    4
                    / 5 # comment 5
                    < 1 # comment 6

                46 # first pattern comment
                | 95 # alternative comment 1
                | 126 # alternative comment 2
                | 150 -> # This comment came after the ->
                    # This comment is for the expr
                    foo bar
                    |> Result.withDefault "" # one last comment

                _ ->
                    42
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
        expr_formats_to(
            indoc!(
                r#"
            x = 1
                < 2

            f x
            "#
            ),
            indoc!(
                r#"
            x =
                1
                < 2

            f x
            "#
            ),
        );
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
                    // 5
                    + 7
                "#
            ),
            indoc!(
                r#"
                2
                % 3
                // 5
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

    #[test]
    fn apply_lambda() {
        expr_formats_same(indoc!(
            r#"
            List.map
                xs
                (\i ->
                    i + length)
            "#
        ));
    }

    #[test]
    fn pipline_apply_lambda_1() {
        expr_formats_same(indoc!(
            r#"
            shout
            |> List.map
                xs
                (\i -> i)
            "#
        ));
    }

    #[test]
    fn pipline_apply_lambda_2() {
        expr_formats_same(indoc!(
            r#"
            shout
            |> List.map
                xs
                (\i -> i)
            |> List.join
            "#
        ));
    }

    #[test]
    fn comment_between_multiline_ann_args() {
        expr_formats_same(indoc!(
            r#"
                blah :
                    Str,
                    # comment
                    (Str -> Str)
                    -> Str

                42
            "#
        ))
    }

    #[test]
    fn pipeline_apply_lambda_multiline() {
        expr_formats_same(indoc!(
            r#"
                example = \model ->
                    model
                    |> withModel
                        (\result ->
                            when result is
                                Err _ ->
                                    Err {}

                                Ok val ->
                                    Ok {}
                        )

                example
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    example = \model ->
                        model
                            |> withModel
                                (\result ->
                                        when result is
                                            Err _ ->
                                                Err {}

                                            Ok val ->
                                                Ok {}
                                )

                    example
                "#
            ),
            indoc!(
                r#"
                    example = \model ->
                        model
                        |> withModel
                            (\result ->
                                when result is
                                    Err _ ->
                                        Err {}

                                    Ok val ->
                                        Ok {}
                            )

                    example
                "#
            ),
        );
    }

    #[test]
    fn func_call_trailing_multiline_lambda() {
        expr_formats_same(indoc!(
            r#"
                list = List.map [1, 2, 3] \x ->
                    x + 1

                list
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
            a = 42 # Yay greetings
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
                    exposes [Bar, Baz, a, b]
                    imports []"#
        ));
    }

    #[test]
    fn interface_importing() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes [Bar, Baz, a, b]
                    imports [Blah, Thing.{ foo, bar }, Stuff]"#
        ));
    }

    #[test]
    fn multi_line_interface() {
        module_formats_same(indoc!(
            r#"
                interface Foo
                    exposes [
                        Stuff,
                        Things,
                        somethingElse,
                    ]
                    imports [
                        Blah,
                        Baz.{ stuff, things },
                    ]"#
        ));
    }

    #[test]
    fn single_line_app() {
        module_formats_same(indoc!(
            r#"
                app "Foo" packages { pf: "platform/main.roc" } imports [] provides [main] to pf"#
        ));
    }

    #[test]
    fn single_line_platform() {
        module_formats_same(
            "platform \"folkertdev/foo\" \
            requires { Model, Msg } { main : Effect {} } \
            exposes [] \
            packages {} \
            imports [Task.{ Task }] \
            provides [mainForHost]",
        );
    }

    #[test]
    fn module_defs_with_comments() {
        module_formats_to(
            &format!(
                indoc!(
                    r#"
                    interface Foo
                        exposes []
                        imports []

                    # comment 1{space}
                    def = "" # comment 2{space}
                    # comment 3{space}
                "#
                ),
                space = " "
            ),
            indoc!(
                r#"
                    interface Foo
                        exposes []
                        imports []

                    # comment 1
                    def = "" # comment 2
                    # comment 3
                "#
            ),
        );
    }

    #[test]
    fn format_tui_package_config() {
        // At one point this failed to reformat.
        module_formats_to(
            indoc!(
                r#"
                    platform "tui"
                        requires { Model } { main : { init : ({} -> Model), update : (Model, Str -> Model), view : (Model -> Str) } }
                        exposes []
                        packages {}
                        imports []
                        provides [ mainForHost ]

                    mainForHost : { init : ({} -> Model) as Init, update : (Model, Str -> Model) as Update, view : (Model -> Str) as View }
                    mainForHost = main
                "#
            ),
            indoc!(
                r#"
                    platform "tui"
                        requires { Model } { main : { init : {} -> Model, update : Model, Str -> Model, view : Model -> Str } }
                        exposes []
                        packages {}
                        imports []
                        provides [mainForHost]

                    mainForHost : { init : ({} -> Model) as Init, update : (Model, Str -> Model) as Update, view : (Model -> Str) as View }
                    mainForHost = main
                "#
            ),
        );
    }

    #[test]
    fn single_line_hosted() {
        module_formats_same(indoc!(
            r#"
                hosted Foo exposes [] imports [] generates Bar with []"#
        ));
    }

    #[test]
    fn multi_line_hosted() {
        module_formats_same(indoc!(
            r#"
                hosted Foo
                    exposes [
                        Stuff,
                        Things,
                        somethingElse,
                    ]
                    imports [
                        Blah,
                        Baz.{ stuff, things },
                    ]
                    generates Bar with [
                        map,
                        after,
                        loop,
                    ]"#
        ));
    }

    /// Annotations and aliases

    #[test]
    fn list_alias() {
        expr_formats_same(indoc!(
            r#"
            ConsList a : [Cons a (ConsList a), Nil]

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
    fn multiline_tag_union_annotation_no_comments() {
        expr_formats_same(indoc!(
            r#"
            b : [
                True,
                False,
            ]

            b
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            b :
                [True, False]

            b
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                b :
                    [
                        True,
                        False,
                    ]

                b
                "#
            ),
            indoc!(
                r#"
                b : [
                    True,
                    False,
                ]

                b
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                b : [
                        True,
                        False,
                    ]

                b
                "#
            ),
            indoc!(
                r#"
                b : [
                    True,
                    False,
                ]

                b
                "#
            ),
        );
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
                b : [
                    True,
                    # comment 1
                    False, # comment 2
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
            f : [True, False] -> [True, False]
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
            f : [Cons a (ConsList a), Nil] as ConsList a -> [Just a, Nothing]
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
                    b <- binaryOp ctx
                    if a == b then
                        -1
                    else
                        0
                )
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                    Task.fromResult
                        (b <- binaryOp ctx
                            if a == b then
                                -1
                            else
                                0
                            )
                "#
            ),
            indoc!(
                r#"
                    Task.fromResult
                        (
                            b <- binaryOp ctx
                            if a == b then
                                -1
                            else
                                0
                        )
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                    Task.fromResult
                        (b <- binaryOp ctx
                            if a == b then
                                -1
                            else
                                0)
                "#
            ),
            indoc!(
                r#"
                    Task.fromResult
                        (
                            b <- binaryOp ctx
                            if a == b then
                                -1
                            else
                                0
                        )
                "#
            ),
        );
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
    fn multiline_higher_order_function() {
        expr_formats_same(indoc!(
            r#"
            foo :
                (Str -> Bool)
                -> Bool

            42
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            foo :
                (Str -> Bool),
                Str
                -> Bool
            foo = \bar, baz ->
                42

            42
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                foo :
                    (Str -> Bool) -> Bool

                42
                "#
            ),
            indoc!(
                r#"
                foo :
                    (Str -> Bool)
                    -> Bool

                42
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                foo :
                    (Str -> Bool), Str -> Bool
                foo = \bar, baz ->
                    42

                42
                "#
            ),
            indoc!(
                r#"
                foo :
                    (Str -> Bool),
                    Str
                    -> Bool
                foo = \bar, baz ->
                    42

                42
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                foo :
                    (Str -> Bool), Str -> Bool # comment
                foo = \bar, baz ->
                    42

                42
                "#
            ),
            indoc!(
                r#"
                foo :
                    (Str -> Bool),
                    Str
                    -> Bool # comment
                foo = \bar, baz ->
                    42

                42
                "#
            ),
        );
    }

    #[test]
    fn multiline_opaque_tag_union() {
        expr_formats_same(indoc!(
            r#"
            A := [
                B,
                C,
            ]

            0
            "#
        ));
    }

    #[test]
    fn opaque_has_clause() {
        expr_formats_same(indoc!(
            r#"
            A := U8 has [Eq, Hash]

            0
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                A :=
                    U8
                    has [Eq, Hash]

                0
                "#
            ),
            indoc!(
                r#"
                A := U8
                     has [Eq, Hash]

                0
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                A := a | a has Hash has [ Eq, Hash ]

                0
                "#
            ),
            indoc!(
                r#"
                A := a | a has Hash
                     has [Eq, Hash]

                0
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                A := U8 has []

                0
                "#
            ),
            indoc!(
                r#"
                A := U8 has []

                0
                "#
            ),
        );
    }

    #[test]
    fn opaque_has_with_impls() {
        expr_formats_same(indoc!(
            r#"
            A := U8 has [Eq { eq }, Hash { hash }]

            0
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            A := U8 has [Eq { eq, eq1 }]

            0
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                A := U8 has [Eq {   eq,     eq1   }]
                A := U8 has [Eq {
                                    eq,
                                    eq1
                                }]

                0
                "#
            ),
            indoc!(
                r#"
                A := U8 has [Eq { eq, eq1 }]
                A := U8 has [
                         Eq {
                             eq,
                             eq1,
                         },
                     ]

                0
                "#
            ),
        );

        expr_formats_same(indoc!(
            r#"
            A := a | a has Other
                 has [Eq { eq }, Hash { hash }]

            0
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            A := U8 has [Eq {}]

            0
            "#
        ));
    }

    #[test]
    fn comments_in_multiline_tag_union_annotation() {
        expr_formats_to(
            indoc!(
                r#"
                    UnionAnn : [
                        Foo, # comment 1
                        Bar, # comment 2
                        Baz, # comment 3
                             # comment 4 line 1
                             # comment 4 line 2
                    ]

                    0
                "#
            ),
            indoc!(
                r#"
                    UnionAnn : [
                        Foo, # comment 1
                        Bar, # comment 2
                        Baz, # comment 3
                        # comment 4 line 1
                        # comment 4 line 2
                    ]

                    0
                "#
            ),
        );
    }

    #[test]
    /// Test that everything under examples/ is formatted correctly
    /// If this test fails on your diff, it probably means you need to re-format the examples.
    /// Try this:
    /// `cargo run -- format $(find examples -name \*.roc)`
    fn test_fmt_examples() {
        let mut count = 0;
        let mut root = workspace_root();
        root.push("examples");
        for entry in walkdir::WalkDir::new(&root) {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension() == Some(std::ffi::OsStr::new("roc")) {
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

    #[test]
    /// Test that builtins are formatted correctly
    /// If this test fails on your diff, it probably means you need to re-format a builtin.
    /// Try this:
    /// `cargo run -- format $(find compiler/builtins/roc -name \*.roc)`
    fn test_fmt_builtins() {
        let mut count = 0;
        let builtins_path = workspace_root()
            .join("crates")
            .join("compiler")
            .join("builtins")
            .join("roc");

        for entry in walkdir::WalkDir::new(&builtins_path) {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension() == Some(std::ffi::OsStr::new("roc")) {
                count += 1;
                let src = std::fs::read_to_string(path).unwrap();
                println!("Now trying to format {}", path.display());
                module_formats_same(&src);
            }
        }
        assert!(
            count > 0,
            "Expecting to find at least 1 .roc file to format under {}",
            builtins_path.display()
        );
    }

    #[test]
    fn expect_single_line() {
        expr_formats_same(indoc!(
            r#"
            x = 5

            expect x == y

            expect y == z

            42
            "#
        ));

        module_formats_same(indoc!(
            r#"
                interface Foo exposes [] imports []

                expect x == y

                expect y == z

                foo = bar
            "#
        ));
    }

    #[test]
    fn expect_multiline() {
        expr_formats_same(indoc!(
            r#"
            x = 5

            expect
                foo bar
                |> baz

            42
            "#
        ));

        module_formats_same(indoc!(
            r#"
                interface Foo exposes [] imports []

                expect
                    foo bar
                    |> baz

                expect
                    blah
                        etc

                foo = bar
            "#
        ));
    }

    #[test]
    fn single_line_string_literal_in_pattern() {
        expr_formats_same(indoc!(
            r#"
            when foo is
                "abc" -> ""
            "#
        ));
    }

    #[test]
    fn multi_line_string_literal_in_pattern() {
        expr_formats_same(indoc!(
            r#"
            when foo is
                """
                abc
                def
                """ -> ""
            "#
        ));
    }

    #[test]
    fn multi_line_string_literal_that_can_be_single_line_in_pattern() {
        expr_formats_to(
            indoc!(
                r#"
                when foo is
                    """
                    abc
                    """ -> ""
                "#
            ),
            indoc!(
                r#"
                when foo is
                    "abc" -> ""
                "#
            ),
        );
    }

    #[test]
    fn format_chars() {
        expr_formats_same(indoc!(
            r#"
            ' '
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            '\n'
            "#
        ));
    }

    #[test]
    fn format_nested_pipeline() {
        expr_formats_same(indoc!(
            r#"
            (a |> b) |> c
            "#
        ));

        expr_formats_same(indoc!(
            r#"
            a |> b |> c
            "#
        ));
    }

    #[test]
    fn ability_member_doc_comments() {
        module_formats_same(indoc!(
            r#"
            interface Foo exposes [] imports []

            A has
                ## This is member ab
                ab : a -> a | a has A

                ## This is member de
                de : a -> a | a has A

            f = g
            "#
        ));
    }

    #[test]
    fn leading_comments_preserved() {
        module_formats_same(indoc!(
            r#"
            # hello world
            interface Foo
                exposes []
                imports []
            "#
        ));

        module_formats_same(indoc!(
            r#"
            # hello world
            app "test" packages {} imports [] provides [] to "./platform"
            "#
        ));

        module_formats_same(indoc!(
            r#"
            # hello world
            platform "hello-world"
                requires {} { main : Str }
                exposes []
                packages {}
                imports []
                provides [mainForHost]
            "#
        ));
    }

    #[test]
    fn clauses_with_multiple_abilities() {
        expr_formats_same(indoc!(
            r#"
            f : {} -> a | a has Eq & Hash & Decode

            f
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                f : {} -> a | a has Eq & Hash & Decode,
                              b has Eq & Hash

                f
                "#
            ),
            indoc!(
                // TODO: ideally, this would look a bit nicer - consider
                // f : {} -> a
                //   | a has Eq & Hash & Decode,
                //     b has Eq & Hash
                r#"
                f : {} -> a | a has Eq & Hash & Decode, b has Eq & Hash

                f
                "#
            ),
        );
    }

    #[test]
    fn format_list_patterns() {
        expr_formats_same(indoc!(
            r#"
            when [] is
                [] -> []
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                when [] is
                    [  ] -> []
                "#
            ),
            indoc!(
                r#"
                when [] is
                    [] -> []
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                when [] is
                    [ x,  ..  ,    A  5   6,  .. ] -> []
                "#
            ),
            indoc!(
                r#"
                when [] is
                    [x, .., A 5 6, ..] -> []
                "#
            ),
        );

        expr_formats_to(
            indoc!(
                r#"
                when [] is
                    [ x, 4, 5 ] -> []
                    [ ..,   5 ] -> []
                    [ x,   .. ] -> []
                "#
            ),
            indoc!(
                r#"
                when [] is
                    [x, 4, 5] -> []
                    [.., 5] -> []
                    [x, ..] -> []
                "#
            ),
        );
    }

    #[test]
    fn format_crash() {
        expr_formats_same(indoc!(
            r#"
            _ = crash
            _ = crash ""

            crash "" ""
            "#
        ));

        expr_formats_to(
            indoc!(
                r#"
                _ = crash
                _ = crash    ""
                _ = crash   ""   ""
                try
                    foo
                    (\_ ->   crash "")
                "#
            ),
            indoc!(
                r#"
                _ = crash
                _ = crash ""
                _ = crash "" ""

                try
                    foo
                    (\_ -> crash "")
                "#
            ),
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

    #[test]
    fn parse_test_snapshots_format_without_error() {
        fn list(dir: &std::path::Path) -> std::vec::Vec<String> {
            std::fs::read_dir(dir)
                .unwrap()
                .map(|f| f.unwrap().file_name().to_str().unwrap().to_string())
                .collect::<std::vec::Vec<_>>()
        }

        fn check_saved_formatting(original: &'_ str, result_path: PathBuf) -> impl Fn(&str) + '_ {
            move |actual_result: &str| {
                if std::env::var("ROC_SNAPSHOT_TEST_OVERWRITE").is_ok() {
                    if original == actual_result {
                        std::fs::remove_file(&result_path)
                            .or_else(|e| {
                                if e.kind() == std::io::ErrorKind::NotFound {
                                    Ok(())
                                } else {
                                    Err(e)
                                }
                            })
                            .unwrap();
                    } else {
                        std::fs::write(&result_path, actual_result).unwrap();
                    }
                } else if original == actual_result {
                    // We represent this expectation on the filesystem as the  _absence_ of the .formatted.expr.roc file.
                    // This makes the directory a bit cleaner.

                    assert!(!result_path.exists(),
                        "Expected no file at {}\n\
                        This how we represent the expectation that the formatting of the input file should not change.\n\
                        consider running the tests with:\n\
                        `env ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test ...` (which will delete the file for you),\n\
                        and commiting the delete.",
                        result_path.display());
                } else {
                    let expected_result =
                        std::fs::read_to_string(&result_path).unwrap_or_else(|e| {
                            panic!(
                                "Error opening test output file {}:\n\
                                {:?}
                                Supposing the file is missing, consider running the tests with:\n\
                                `env ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test ...`\n\
                                and committing the file that creates.",
                                result_path.display(),
                                e
                            );
                        });

                    assert_multiline_str_eq!(expected_result.as_str(), actual_result);
                }
            }
        }

        let base = std::path::PathBuf::from("../parse/tests/snapshots/pass");
        for file in list(&base) {
            if let Some(prefix) = file.strip_suffix(".expr.roc") {
                println!("formatting {}", file);
                let contents = std::fs::read_to_string(base.join(&file)).unwrap();
                let formatted_path = base.join(format!("{}.expr.formatted.roc", prefix));
                expr_formats(
                    &contents,
                    check_saved_formatting(&contents, formatted_path),
                    false,
                );
            } else if file.ends_with(".module.roc") {
                // TODO: re-format module defs and ensure they're correct.
                // Note that these tests don't have an actual module header,
                // so we'll have to pre-pend that for this test.
            }
        }
    }
}
