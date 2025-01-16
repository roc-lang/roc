/// Simple tests for parsing expressions, module headers, module defs, etc.
/// Note, much more extensive tests are in the `test_syntax` crate (in test_snapshots).

#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate quickcheck;

#[macro_use(quickcheck)]
extern crate quickcheck_macros;

extern crate roc_module;
extern crate roc_parse;

#[cfg(test)]
mod test_parse {
    use bumpalo::collections::vec::Vec;
    use bumpalo::{self, Bump};
    use roc_parse::ast::Expr::{self, *};
    use roc_parse::ast::StrSegment::*;
    use roc_parse::ast::{self, EscapedChar};
    use roc_parse::ast::{CommentOrNewline, StrLiteral::*};
    use roc_parse::header::parse_module_defs;
    use roc_parse::parser::SyntaxError;
    use roc_parse::state::State;
    use roc_parse::test_helpers::parse_expr_with;
    use roc_region::all::{Loc, Region};
    use std::{f64, i64};

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, input.trim());
        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_parsing_fails(input: &str, _reason: SyntaxError) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, input);

        assert!(actual.is_err());
    }

    fn assert_segments<E: Fn(&Bump) -> Vec<'_, ast::StrSegment<'_>>>(input: &str, to_expected: E) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, arena.alloc(input));
        let expected_slice = to_expected(&arena);
        let expected_expr = Expr::Str(Line(&expected_slice));

        assert_eq!(Ok(expected_expr), actual);
    }

    fn parses_with_escaped_char<
        I: Fn(&str) -> String,
        E: Fn(EscapedChar, &Bump) -> Vec<'_, ast::StrSegment<'_>>,
    >(
        to_input: I,
        to_expected: E,
    ) {
        let arena = Bump::new();

        // Try parsing with each of the escaped chars Roc supports
        for (string, escaped) in &[
            ("\\\\", EscapedChar::Backslash),
            ("\\n", EscapedChar::Newline),
            ("\\r", EscapedChar::CarriageReturn),
            ("\\t", EscapedChar::Tab),
            ("\\\"", EscapedChar::DoubleQuote),
        ] {
            let actual = parse_expr_with(&arena, arena.alloc(to_input(string)));
            let expected_slice = to_expected(*escaped, &arena);
            let expected_expr = Expr::Str(Line(&expected_slice));

            assert_eq!(Ok(expected_expr), actual);
        }
    }

    // BACKSLASH ESCAPES

    #[test]
    fn string_with_escaped_char_at_end() {
        parses_with_escaped_char(
            |esc| format!(r#""abcd{esc}""#),
            |esc, arena| bumpalo::vec![in arena;  Plaintext("abcd"), EscapedChar(esc)],
        );
    }

    #[test]
    fn string_with_escaped_char_in_front() {
        parses_with_escaped_char(
            |esc| format!(r#""{esc}abcd""#),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abcd")],
        );
    }

    #[test]
    fn string_with_escaped_char_in_middle() {
        parses_with_escaped_char(
            |esc| format!(r#""ab{esc}cd""#),
            |esc, arena| bumpalo::vec![in arena; Plaintext("ab"), EscapedChar(esc), Plaintext("cd")],
        );
    }

    #[test]
    fn string_with_multiple_escaped_chars() {
        parses_with_escaped_char(
            |esc| format!(r#""{esc}abc{esc}de{esc}fghi{esc}""#),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abc"), EscapedChar(esc), Plaintext("de"), EscapedChar(esc), Plaintext("fghi"), EscapedChar(esc)],
        );
    }

    // UNICODE ESCAPES

    #[test]
    fn unicode_escape_in_middle() {
        assert_segments(r#""Hi, \u(123)!""#, |arena| {
            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Unicode(Loc::new(8, 11, "123")),
                 Plaintext("!")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_front() {
        assert_segments(r#""\u(1234) is a unicode char""#, |arena| {
            bumpalo::vec![in arena;
                 Unicode(Loc::new(4, 8, "1234")),
                 Plaintext(" is a unicode char")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_back() {
        assert_segments(r#""this is unicode: \u(1)""#, |arena| {
            bumpalo::vec![in arena;
                 Plaintext("this is unicode: "),
                 Unicode(Loc::new(21, 22, "1"))
            ]
        });
    }

    #[test]
    fn unicode_escape_multiple() {
        assert_segments(r#""\u(a1) this is \u(2Bcd) unicode \u(ef97)""#, |arena| {
            bumpalo::vec![in arena;
                 Unicode(Loc::new(4, 6, "a1")),
                 Plaintext(" this is "),
                 Unicode(Loc::new(19, 23, "2Bcd")),
                 Plaintext(" unicode "),
                 Unicode(Loc::new(36, 40, "ef97"))
            ]
        });
    }

    // INTERPOLATION

    #[test]
    fn escaped_interpolation() {
        assert_segments(r#""Hi, \${name}!""#, |arena| {
            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 EscapedChar(EscapedChar::Dollar),
                 Plaintext("{name}!"),
            ]
        });
    }

    #[test]
    fn string_with_old_interpolation_still_works_for_now() {
        assert_segments(r#""Hi, $(name)!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Interpolated(Loc::new(7, 11, expr)),
                 Plaintext("!")
            ]
        });
    }

    #[test]
    fn string_with_mixed_new_and_old_interpolation_braces_fails() {
        assert_parsing_fails(r#""${foo)""#, SyntaxError::Unexpected(Region::zero()));
        assert_parsing_fails(r#""$(foo}""#, SyntaxError::Unexpected(Region::zero()));
    }

    #[test]
    fn string_with_interpolation_in_middle() {
        assert_segments(r#""Hi, ${name}!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Interpolated(Loc::new(7, 11, expr)),
                 Plaintext("!")
            ]
        });
    }

    #[test]
    fn string_with_interpolation_in_front() {
        assert_segments(r#""${name}, hi!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Interpolated(Loc::new(3, 7, expr)),
                 Plaintext(", hi!")
            ]
        });
    }

    #[test]
    fn string_of_just_dollar_sign() {
        let arena = Bump::new();

        assert_eq!(
            Ok(Expr::Str(PlainLine("$"))),
            parse_expr_with(&arena, arena.alloc(r#""$""#))
        );
    }

    #[test]
    fn string_beginning_with_dollar() {
        let arena = Bump::new();

        assert_eq!(
            Ok(Expr::Str(PlainLine("$foo"))),
            parse_expr_with(&arena, arena.alloc(r#""$foo""#))
        );
    }

    #[test]
    fn string_ending_with_dollar() {
        let arena = Bump::new();

        assert_eq!(
            Ok(Expr::Str(PlainLine("foo$"))),
            parse_expr_with(&arena, arena.alloc(r#""foo$""#))
        );
    }

    #[test]
    fn string_with_interpolation_in_back() {
        assert_segments(r#""Hello ${name}""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hello "),
                 Interpolated(Loc::new(9, 13, expr))
            ]
        });
    }

    #[test]
    fn string_with_multiple_interpolations() {
        assert_segments(r#""Hi, ${name}! How is ${project} going?""#, |arena| {
            let expr1 = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            let expr2 = arena.alloc(Var {
                module_name: "",
                ident: "project",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Interpolated(Loc::new(7, 11, expr1)),
                 Plaintext("! How is "),
                 Interpolated(Loc::new(23, 30, expr2)),
                 Plaintext(" going?")
            ]
        });
    }

    #[test]
    fn string_with_non_interpolation_dollar_signs() {
        assert_segments(
            r#""$a Hi, ${name}! $b How is ${project} going? $c""#,
            |arena| {
                let expr1 = arena.alloc(Var {
                    module_name: "",
                    ident: "name",
                });

                let expr2 = arena.alloc(Var {
                    module_name: "",
                    ident: "project",
                });

                bumpalo::vec![in arena;
                     Plaintext("$a Hi, "),
                     Interpolated(Loc::new(10, 14, expr1)),
                     Plaintext("! $b How is "),
                     Interpolated(Loc::new(29, 36, expr2)),
                     Plaintext(" going? $c")
                ]
            },
        );
    }

    #[test]
    fn empty_source_file() {
        assert_parsing_fails("", SyntaxError::Eof(Region::zero()));
    }

    #[quickcheck]
    fn all_i64_values_parse(num: i64) {
        assert_parses_to(num.to_string().as_str(), Num(num.to_string().as_str()));
    }

    #[quickcheck]
    fn all_f64_values_parse(mut num: f64) {
        // NaN, Infinity, -Infinity (these would all parse as tags in Roc)
        if !num.is_finite() {
            num = 0.0;
        }

        // These can potentially be whole numbers. `Display` omits the decimal point for those,
        // causing them to no longer be parsed as fractional numbers by Roc.
        // Using `Debug` instead of `Display` ensures they always have a decimal point.
        let float_string = format!("{num:?}");

        assert_parses_to(float_string.as_str(), Float(float_string.as_str()));
    }

    // SINGLE QUOTE LITERAL
    #[test]
    fn single_quote() {
        assert_parses_to("'b'", Expr::SingleQuote("b"));
    }

    #[test]
    fn repro_keyword_bug() {
        // Reproducing this bug requires a bizarre set of things to all be true:
        //
        // * Must be parsing a *module* def (nested expr defs don't repro this)
        // * That top-level module def contains a def inside it
        // * That inner def is defining a function
        // * The name of the inner def begins with a keyword (`if`, `then`, `else`, `when`, `is`)
        //
        // If all of these are true, then lookups on that def get skipped over by the parser.
        // If any one of the above is false, then everything works.

        let arena = Bump::new();
        let src = indoc!(
            r"
                foo = \list ->
                    isTest = \_ -> 5
                    List.map list isTest
            "
        );
        let actual = parse_module_defs(&arena, State::new(src.as_bytes()), ast::Defs::default());

        // It should occur twice in the debug output - once for the pattern,
        // and then again for the lookup.
        let occurrences = format!("{actual:?}").split("isTest").count() - 1;

        assert_eq!(occurrences, 2);
    }

    #[test]
    fn outdenting_newline_after_else() {
        let arena = &Bump::new();

        // highlights a problem with the else branch demanding a newline after its expression
        let src = indoc!(
            r"
            main =
                v = \y -> if x then y else z

                1
            "
        );

        let state = State::new(src.as_bytes());
        let parsed = parse_module_defs(arena, state, ast::Defs::default());
        match parsed {
            Ok(_) => {
                // eprintln!("{:?}", _state);
            }
            Err(_) => {
                // eprintln!("{:?}, {:?}", _fail, _state);
                panic!("Failed to parse!");
            }
        }
    }

    #[test]
    fn parse_expr_size() {
        assert_eq!(std::mem::size_of::<roc_parse::ast::Expr>(), 40);
    }

    #[test]
    fn parse_two_line_comment_with_crlf() {
        let src = "# foo\r\n# bar\r\n42";
        assert_parses_to(
            src,
            Expr::SpaceBefore(
                &Expr::Num("42"),
                &[
                    CommentOrNewline::LineComment(" foo"),
                    // We used to have a bug where there was an extra CommentOrNewline::Newline between these.
                    CommentOrNewline::LineComment(" bar"),
                ],
            ),
        );
    }

    // PARSE ERROR

    // TODO this should be parse error, but isn't!
    // #[test]
    // fn trailing_paren() {
    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //                 r = "foo"
    //                 s = { left : "foo" }

    //                 when 0 is
    //                     1 -> { x: s.left, y: s.left }
    //                     0 -> { x: s.left, y: r }
    //                     )
    //             "#
    //         ),
    //         Str(PlainLine("")),
    //     );
    // }

    // TODO test for non-ASCII variables
    //
    // TODO verify that when a string literal contains a newline before the
    // closing " it correctly updates both the line *and* column in the State.
}
