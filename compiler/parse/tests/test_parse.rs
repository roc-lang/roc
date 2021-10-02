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
    use roc_module::operator::BinOp::{self, *};
    use roc_module::operator::{CalledVia, UnaryOp};
    use roc_parse::ast::AssignedField::*;
    use roc_parse::ast::CommentOrNewline::*;
    use roc_parse::ast::Expr::{self, *};
    use roc_parse::ast::Pattern::{self, *};
    use roc_parse::ast::StrLiteral::{self, *};
    use roc_parse::ast::StrSegment::*;
    use roc_parse::ast::{self, Def, EscapedChar, Spaceable, TypeAnnotation, WhenBranch};
    use roc_parse::header::{
        AppHeader, Effects, ExposesEntry, ImportsEntry, InterfaceHeader, ModuleName, PackageEntry,
        PackageName, PackageOrPath, PlatformHeader, PlatformRequires, PlatformRigid, To,
        TypedIdent,
    };
    use roc_parse::module::module_defs;
    use roc_parse::parser::{Parser, State, SyntaxError};
    use roc_parse::test_helpers::parse_expr_with;
    use roc_region::all::{Located, Region};
    use std::{f64, i64};

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, input.trim());

        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_parsing_fails<'a>(input: &'a str, _reason: SyntaxError) {
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
            ("\\\"", EscapedChar::Quote),
        ] {
            let actual = parse_expr_with(&arena, arena.alloc(to_input(string)));
            let expected_slice = to_expected(*escaped, &arena);
            let expected_expr = Expr::Str(Line(&expected_slice));

            assert_eq!(Ok(expected_expr), actual);
        }
    }

    fn single_binop<'a>(
        arena: &'a Bump,
        args: (
            Located<Expr<'a>>,
            Located<roc_module::operator::BinOp>,
            Located<Expr<'a>>,
        ),
    ) -> Expr<'a> {
        let (left, op, right) = args;

        Expr::BinOps(arena.alloc([(left, op)]), arena.alloc(right))
    }

    // STRING LITERALS

    fn expect_parsed_str(input: &str, expected: &str) {
        assert_parses_to(expected, Expr::Str(PlainLine(input)));
    }

    #[test]
    fn empty_string() {
        assert_parses_to(
            indoc!(
                r#"
                    ""
                "#
            ),
            Str(PlainLine("")),
        );
    }

    #[test]
    fn one_char_string() {
        assert_parses_to(
            indoc!(
                r#"
                    "x"
                "#
            ),
            Expr::Str(PlainLine("x")),
        );
    }

    #[test]
    fn multi_char_string() {
        assert_parses_to(
            indoc!(
                r#"
                    "foo"
                "#
            ),
            Expr::Str(PlainLine("foo")),
        );
    }

    #[test]
    fn string_without_escape() {
        expect_parsed_str("a", r#""a""#);
        expect_parsed_str("ab", r#""ab""#);
        expect_parsed_str("abc", r#""abc""#);
        expect_parsed_str("123", r#""123""#);
        expect_parsed_str("abc123", r#""abc123""#);
        expect_parsed_str("123abc", r#""123abc""#);
        expect_parsed_str("123 abc 456 def", r#""123 abc 456 def""#);
    }

    // BACKSLASH ESCAPES

    #[test]
    fn string_with_escaped_char_at_end() {
        parses_with_escaped_char(
            |esc| format!(r#""abcd{}""#, esc),
            |esc, arena| bumpalo::vec![in arena;  Plaintext("abcd"), EscapedChar(esc)],
        );
    }

    #[test]
    fn string_with_escaped_char_in_front() {
        parses_with_escaped_char(
            |esc| format!(r#""{}abcd""#, esc),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abcd")],
        );
    }

    #[test]
    fn string_with_escaped_char_in_middle() {
        parses_with_escaped_char(
            |esc| format!(r#""ab{}cd""#, esc),
            |esc, arena| bumpalo::vec![in arena; Plaintext("ab"), EscapedChar(esc), Plaintext("cd")],
        );
    }

    #[test]
    fn string_with_multiple_escaped_chars() {
        parses_with_escaped_char(
            |esc| format!(r#""{}abc{}de{}fghi{}""#, esc, esc, esc, esc),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abc"), EscapedChar(esc), Plaintext("de"), EscapedChar(esc), Plaintext("fghi"), EscapedChar(esc)],
        );
    }

    // UNICODE ESCAPES

    #[test]
    fn unicode_escape_in_middle() {
        assert_segments(r#""Hi, \u(123)!""#, |arena| {
            bumpalo::vec![in arena;
                Plaintext("Hi, "),
                Unicode(Located::new(0, 0, 8, 11, "123")),
                Plaintext("!")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_front() {
        assert_segments(r#""\u(1234) is a unicode char""#, |arena| {
            bumpalo::vec![in arena;
                Unicode(Located::new(0, 0, 4, 8, "1234")),
                Plaintext(" is a unicode char")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_back() {
        assert_segments(r#""this is unicode: \u(1)""#, |arena| {
            bumpalo::vec![in arena;
                Plaintext("this is unicode: "),
                Unicode(Located::new(0, 0, 21, 22, "1"))
            ]
        });
    }

    #[test]
    fn unicode_escape_multiple() {
        assert_segments(r#""\u(a1) this is \u(2Bcd) unicode \u(ef97)""#, |arena| {
            bumpalo::vec![in arena;
                Unicode(Located::new(0, 0, 4, 6, "a1")),
                Plaintext(" this is "),
                Unicode(Located::new(0, 0, 19, 23, "2Bcd")),
                Plaintext(" unicode "),
                Unicode(Located::new(0, 0, 36, 40, "ef97"))
            ]
        });
    }

    // INTERPOLATION

    #[test]
    fn string_with_interpolation_in_middle() {
        assert_segments(r#""Hi, \(name)!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                Plaintext("Hi, "),
                Interpolated(Located::new(0, 0, 7, 11, expr)),
                Plaintext("!")
            ]
        });
    }

    #[test]
    fn string_with_interpolation_in_front() {
        assert_segments(r#""\(name), hi!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                Interpolated(Located::new(0, 0, 3, 7, expr)),
                Plaintext(", hi!")
            ]
        });
    }

    #[test]
    fn string_with_interpolation_in_back() {
        assert_segments(r#""Hello \(name)""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                Plaintext("Hello "),
                Interpolated(Located::new(0, 0, 9, 13, expr))
            ]
        });
    }

    #[test]
    fn string_with_multiple_interpolations() {
        assert_segments(r#""Hi, \(name)! How is \(project) going?""#, |arena| {
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
                Interpolated(Located::new(0, 0, 7, 11, expr1)),
                Plaintext("! How is "),
                Interpolated(Located::new(0, 0, 23, 30, expr2)),
                Plaintext(" going?")
            ]
        });
    }

    #[test]
    fn empty_source_file() {
        assert_parsing_fails("", SyntaxError::Eof(Region::zero()));
    }

    #[test]
    fn first_line_too_long() {
        let max_line_length = u16::MAX as usize;

        // the string literal "ZZZZZZZZZ" but with way more Zs
        let too_long_str_body: String = (1..max_line_length)
            .into_iter()
            .map(|_| "Z".to_string())
            .collect();
        let too_long_str = format!("\"{}\"", too_long_str_body);

        // Make sure it's longer than our maximum line length
        assert_eq!(too_long_str.len(), max_line_length + 1);

        assert_parsing_fails(&too_long_str, SyntaxError::LineTooLong(0));
    }

    // INT LITERALS

    #[test]
    fn zero_int() {
        assert_parses_to("0", Num("0"));
    }

    #[test]
    fn positive_int() {
        assert_parses_to("1", Num("1"));
        assert_parses_to("42", Num("42"));
    }

    #[test]
    fn negative_int() {
        assert_parses_to("-1", Num("-1"));
        assert_parses_to("-42", Num("-42"));
    }

    #[test]
    fn highest_int() {
        assert_parses_to(
            i64::MAX.to_string().as_str(),
            Num(i64::MAX.to_string().as_str()),
        );
    }

    #[test]
    fn lowest_int() {
        assert_parses_to(
            i64::MIN.to_string().as_str(),
            Num(i64::MIN.to_string().as_str()),
        );
    }

    #[test]
    fn int_with_underscore() {
        assert_parses_to("1_2_34_567", Num("1_2_34_567"));
        assert_parses_to("-1_2_34_567", Num("-1_2_34_567"));
        // The following cases are silly. They aren't supported on purpose,
        // but there would be a performance cost to explicitly disallowing them,
        // which doesn't seem like it would benefit anyone.
        assert_parses_to("1_", Num("1_"));
        assert_parses_to("1__23", Num("1__23"));
    }

    #[quickcheck]
    fn all_i64_values_parse(num: i64) {
        assert_parses_to(num.to_string().as_str(), Num(num.to_string().as_str()));
    }

    // FLOAT LITERALS

    #[test]
    fn zero_float() {
        assert_parses_to("0.0", Float("0.0"));
    }

    #[test]
    fn positive_float() {
        assert_parses_to("1.0", Float("1.0"));
        assert_parses_to("1.1", Float("1.1"));
        assert_parses_to("42.0", Float("42.0"));
        assert_parses_to("42.9", Float("42.9"));
    }

    #[test]
    fn negative_float() {
        assert_parses_to("-1.0", Float("-1.0"));
        assert_parses_to("-1.1", Float("-1.1"));
        assert_parses_to("-42.0", Float("-42.0"));
        assert_parses_to("-42.9", Float("-42.9"));
    }

    #[test]
    fn float_with_underscores() {
        assert_parses_to("1_23_456.0_1_23_456", Float("1_23_456.0_1_23_456"));
        assert_parses_to("-1_23_456.0_1_23_456", Float("-1_23_456.0_1_23_456"));
    }

    #[test]
    fn highest_float() {
        let string = format!("{}.0", f64::MAX);

        assert_parses_to(&string, Float(&string));
    }

    #[test]
    fn lowest_float() {
        let string = format!("{}.0", f64::MIN);

        assert_parses_to(&string, Float(&string));
    }

    #[quickcheck]
    fn all_f64_values_parse(num: f64) {
        assert_parses_to(num.to_string().as_str(), Float(num.to_string().as_str()));
    }

    // SINGLE QUOTE LITERAL
    #[test]
    fn single_quote() {
        assert_parses_to("'b'", Expr::SingleQuote("b"));
    }

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        let arena = Bump::new();
        let expected = Record {
            fields: &[],
            final_comments: &[],
        };
        let actual = parse_expr_with(&arena, "{}");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn record_update() {
        let arena = Bump::new();
        let label1 = RequiredValue(
            Located::new(0, 0, 16, 17, "x"),
            &[],
            arena.alloc(Located::new(0, 0, 19, 20, Num("5"))),
        );
        let label2 = RequiredValue(
            Located::new(0, 0, 22, 23, "y"),
            &[],
            arena.alloc(Located::new(0, 0, 25, 26, Num("0"))),
        );
        let fields: &[_] = &[
            Located::new(0, 0, 16, 20, label1),
            Located::new(0, 0, 22, 26, label2),
        ];
        let var = Var {
            module_name: "Foo.Bar",
            ident: "baz",
        };
        let update_target = Located::new(0, 0, 2, 13, var);
        let expected = RecordUpdate {
            update: &*arena.alloc(update_target),
            fields,
            final_comments: &(&[] as &[_]),
        };

        let actual = parse_expr_with(&arena, "{ Foo.Bar.baz & x: 5, y: 0 }");
        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn record_with_if() {
        let arena = Bump::new();
        let if_expr = Expr::If(
            arena.alloc([(
                Located::new(0, 0, 8, 12, Expr::GlobalTag("True")),
                Located::new(0, 0, 18, 19, Expr::Num("1")),
            )]),
            arena.alloc(Located::new(0, 0, 25, 26, Num("2"))),
        );

        let label1 = RequiredValue(
            Located::new(0, 0, 1, 2, "x"),
            &[],
            arena.alloc(Located::new(0, 0, 5, 26, if_expr)),
        );
        let label2 = RequiredValue(
            Located::new(0, 0, 28, 29, "y"),
            &[],
            arena.alloc(Located::new(0, 0, 31, 32, Num("3"))),
        );
        let fields = &[
            Located::new(0, 0, 1, 26, label1),
            Located::new(0, 0, 28, 32, label2),
        ];
        let expected = Record {
            fields,
            final_comments: &[],
        };

        let actual = parse_expr_with(&arena, "{x : if True then 1 else 2, y: 3 }");
        assert_eq!(Ok(expected), actual);
    }

    // EXPECT

    #[test]
    fn expect() {
        let arena = Bump::new();

        let equals = (
            Located::new(0, 0, 7, 8, Num("1")),
            Located::new(0, 0, 9, 11, BinOp::Equals),
        );

        let condition = BinOps(
            arena.alloc([equals]),
            arena.alloc(Located::new(0, 0, 12, 13, Num("1"))),
        );
        let loc_condition = Located::new(0, 0, 7, 13, condition);

        let continuation = Expr::SpaceBefore(arena.alloc(Num("4")), &[Newline, Newline]);
        let loc_continuation = Located::new(2, 2, 0, 1, continuation);

        let expected = Expect(arena.alloc(loc_condition), arena.alloc(loc_continuation));

        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
            expect 1 == 1

            4
            "#
            ),
        );
        assert_eq!(Ok(expected), actual);
    }

    // OPERATORS

    #[test]
    fn one_plus_two() {
        let arena = Bump::new();
        let tuple = (
            Located::new(0, 0, 0, 1, Num("1")),
            Located::new(0, 0, 1, 2, Plus),
            Located::new(0, 0, 2, 3, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "1+2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn one_minus_two() {
        let arena = Bump::new();
        let tuple = (
            Located::new(0, 0, 0, 1, Num("1")),
            Located::new(0, 0, 1, 2, Minus),
            Located::new(0, 0, 2, 3, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "1-2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn var_minus_two() {
        let arena = Bump::new();
        let tuple = (
            Located::new(
                0,
                0,
                0,
                1,
                Var {
                    module_name: "",
                    ident: "x",
                },
            ),
            Located::new(0, 0, 1, 2, Minus),
            Located::new(0, 0, 2, 3, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x-2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn add_with_spaces() {
        let arena = Bump::new();
        let tuple = (
            Located::new(0, 0, 0, 1, Num("1")),
            Located::new(0, 0, 3, 4, Plus),
            Located::new(0, 0, 7, 8, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "1  +   2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn sub_with_spaces() {
        let arena = Bump::new();
        let tuple = (
            Located::new(0, 0, 0, 1, Num("1")),
            Located::new(0, 0, 3, 4, Minus),
            Located::new(0, 0, 7, 8, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "1  -   2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn add_var_with_spaces() {
        // This is a regression test! It used to break with subtraction and work
        // with other arithmetic operatos.
        //
        // Subtraction is special when it comes to parsing, because of unary negation.

        let arena = Bump::new();
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let tuple = (
            Located::new(0, 0, 0, 1, var),
            Located::new(0, 0, 2, 3, Plus),
            Located::new(0, 0, 4, 5, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x + 2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn sub_var_with_spaces() {
        // This is a regression test! It used to break with subtraction and work
        // with other arithmetic operatos.
        //
        // Subtraction is special when it comes to parsing, because of unary negation.
        let arena = Bump::new();
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let tuple = (
            Located::new(0, 0, 0, 1, var),
            Located::new(0, 0, 2, 3, Minus),
            Located::new(0, 0, 4, 5, Num("2")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x - 2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_before_add() {
        let arena = Bump::new();
        let spaced_int = Expr::SpaceAfter(arena.alloc(Num("3")), &[Newline]);
        let tuple = (
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(1, 1, 2, 3, Num("4")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  \n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_before_sub() {
        let arena = Bump::new();
        let spaced_int = Expr::SpaceAfter(arena.alloc(Num("3")), &[Newline]);
        let tuple = (
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Minus),
            Located::new(1, 1, 2, 3, Num("4")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  \n- 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_after_mul() {
        let arena = Bump::new();
        let spaced_int = arena.alloc(Num("4")).before(&[Newline]);
        let tuple = (
            Located::new(0, 0, 0, 1, Num("3")),
            Located::new(0, 0, 3, 4, Star),
            Located::new(1, 1, 2, 3, spaced_int),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  *\n  4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_after_sub() {
        let arena = Bump::new();
        let spaced_int = arena.alloc(Num("4")).before(&[Newline]);
        let tuple = (
            Located::new(0, 0, 0, 1, Num("3")),
            Located::new(0, 0, 3, 4, Minus),
            Located::new(1, 1, 2, 3, spaced_int),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  -\n  4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_and_spaces_before_less_than() {
        let arena = Bump::new();
        let spaced_int = arena.alloc(Num("1")).after(&[Newline]);
        let tuple = (
            Located::new(0, 0, 4, 5, spaced_int),
            Located::new(1, 1, 4, 5, LessThan),
            Located::new(1, 1, 6, 7, Num("2")),
        );

        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(0, 0, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(0, 1, 4, 7, single_binop(&arena, tuple))),
        );
        let loc_def = &*arena.alloc(Located::new(0, 1, 0, 7, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        // let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x = 1\n    < 2\n\n42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn comment_with_non_ascii() {
        let arena = Bump::new();
        let spaced_int = arena.alloc(Num("3")).after(&[LineComment(" 2 × 2")]);
        let tuple = (
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(1, 1, 2, 3, Num("4")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  # 2 × 2\n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn comment_before_op() {
        let arena = Bump::new();
        let spaced_int = arena.alloc(Num("3")).after(&[LineComment(" test!")]);
        let tuple = (
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(1, 1, 2, 3, Num("4")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  # test!\n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn comment_after_op() {
        let arena = Bump::new();
        let spaced_int = arena.alloc(Num("92")).before(&[LineComment(" test!")]);
        let tuple = (
            Located::new(0, 0, 0, 2, Num("12")),
            Located::new(0, 0, 4, 5, Star),
            Located::new(1, 1, 1, 3, spaced_int),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "12  * # test!\n 92");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn ops_with_newlines() {
        let arena = Bump::new();
        let spaced_int1 = arena.alloc(Num("3")).after(&[Newline]);
        let spaced_int2 = arena.alloc(Num("4")).before(&[Newline, Newline]);
        let tuple = (
            Located::new(0, 0, 0, 1, spaced_int1),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(3, 3, 2, 3, spaced_int2),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "3  \n+ \n\n  4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn space_only_after_minus() {
        // This is an edge case with minus because of unary negation.
        // (x- y) should parse like subtraction (x - (y)), not function application (x (-y))
        let arena = Bump::new();
        let var1 = Var {
            module_name: "",
            ident: "x",
        };
        let var2 = Var {
            module_name: "",
            ident: "y",
        };
        let tuple = (
            Located::new(0, 0, 0, 1, var1),
            Located::new(0, 0, 1, 2, Minus),
            Located::new(0, 0, 3, 4, var2),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x- y");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn minus_twelve_minus_five() {
        let arena = Bump::new();
        let tuple = (
            Located::new(0, 0, 0, 3, Num("-12")),
            Located::new(0, 0, 3, 4, Minus),
            Located::new(0, 0, 4, 5, Num("5")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "-12-5");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn ten_times_eleven() {
        let arena = Bump::new();
        let tuple = (
            Located::new(0, 0, 0, 2, Num("10")),
            Located::new(0, 0, 2, 3, Star),
            Located::new(0, 0, 3, 5, Num("11")),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "10*11");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn multiple_operators() {
        let arena = Bump::new();

        let lefts = [
            (
                Located::new(0, 0, 0, 2, Num("31")),
                Located::new(0, 0, 2, 3, Star),
            ),
            (
                Located::new(0, 0, 3, 5, Num("42")),
                Located::new(0, 0, 5, 6, Plus),
            ),
        ];
        let right = Located::new(0, 0, 6, 9, Num("534"));

        let expected = BinOps(&lefts, &right);
        let actual = parse_expr_with(&arena, "31*42+534");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn equals() {
        let arena = Bump::new();
        let var1 = Var {
            module_name: "",
            ident: "x",
        };
        let var2 = Var {
            module_name: "",
            ident: "y",
        };
        let tuple = (
            Located::new(0, 0, 0, 1, var1),
            Located::new(0, 0, 1, 3, Equals),
            Located::new(0, 0, 3, 4, var2),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x==y");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn equals_with_spaces() {
        let arena = Bump::new();
        let var1 = Var {
            module_name: "",
            ident: "x",
        };
        let var2 = Var {
            module_name: "",
            ident: "y",
        };
        let tuple = (
            Located::new(0, 0, 0, 1, var1),
            Located::new(0, 0, 2, 4, Equals),
            Located::new(0, 0, 5, 6, var2),
        );
        let expected = single_binop(&arena, tuple);
        let actual = parse_expr_with(&arena, "x == y");

        assert_eq!(Ok(expected), actual);
    }

    // VAR

    #[test]
    fn basic_var() {
        let arena = Bump::new();
        let expected = Var {
            module_name: "",
            ident: "whee",
        };
        let actual = parse_expr_with(&arena, "whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn var_when() {
        // Regression test for identifiers beginning with keywords (if/then/else/when/is)
        let arena = Bump::new();
        let expected = Var {
            module_name: "",
            ident: "whenever",
        };
        let actual = parse_expr_with(&arena, "whenever");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn var_is() {
        // Regression test for identifiers beginning with keywords (if/then/else/when/is)
        let arena = Bump::new();
        let expected = Var {
            module_name: "",
            ident: "isnt",
        };
        let actual = parse_expr_with(&arena, "isnt");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn var_if() {
        // Regression test for identifiers beginning with keywords (if/then/else/when/is)
        let arena = Bump::new();
        let expected = Var {
            module_name: "",
            ident: "iffy",
        };
        let actual = parse_expr_with(&arena, "iffy");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn var_then() {
        // Regression test for identifiers beginning with keywords (if/then/else/when/is)
        let arena = Bump::new();
        let expected = Var {
            module_name: "",
            ident: "thenever",
        };
        let actual = parse_expr_with(&arena, "thenever");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn var_else() {
        // Regression test for identifiers beginning with keywords (if/then/else/when/is)
        let arena = Bump::new();
        let expected = Var {
            module_name: "",
            ident: "elsewhere",
        };
        let actual = parse_expr_with(&arena, "elsewhere");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_var() {
        let arena = Bump::new();
        let expected = ParensAround(arena.alloc(Var {
            module_name: "",
            ident: "whee",
        }));
        let actual = parse_expr_with(&arena, "(whee)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_var() {
        let arena = Bump::new();
        let expected = Var {
            module_name: "One.Two",
            ident: "whee",
        };
        let actual = parse_expr_with(&arena, "One.Two.whee");

        assert_eq!(Ok(expected), actual);
    }

    // TAG

    #[test]
    fn basic_global_tag() {
        let arena = Bump::new();
        let expected = Expr::GlobalTag("Whee");
        let actual = parse_expr_with(&arena, "Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn basic_private_tag() {
        let arena = Bump::new();
        let expected = Expr::PrivateTag("@Whee");
        let actual = parse_expr_with(&arena, "@Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_private_tag() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Num("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 9, 11, Num("34")));
        let args = &[&*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 5, Expr::PrivateTag("@Whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "@Whee 12 34");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_global_tag() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 5, 7, Num("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 8, 10, Num("34")));
        let args = &[&*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, Expr::GlobalTag("Whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "Whee 12 34");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_parenthetical_global_tag_args() {
        let arena = Bump::new();
        let int1 = ParensAround(arena.alloc(Num("12")));
        let int2 = ParensAround(arena.alloc(Num("34")));
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, int1));
        let arg2 = arena.alloc(Located::new(0, 0, 11, 13, int2));
        let args = &[&*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, Expr::GlobalTag("Whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "Whee (12) (34)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_global_tag() {
        use roc_parse::ident::BadIdent;

        let arena = Bump::new();
        let expected = Expr::MalformedIdent("One.Two.Whee", BadIdent::QualifiedTag(0, 12));
        let actual = parse_expr_with(&arena, "One.Two.Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn private_qualified_tag() {
        use roc_parse::ident::BadIdent;

        let arena = Bump::new();
        let expected = Expr::MalformedIdent("@One.Two.Whee", BadIdent::BadPrivateTag(0, 4));
        let actual = parse_expr_with(&arena, "@One.Two.Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn tag_pattern() {
        let arena = Bump::new();
        let pattern = Located::new(0, 0, 1, 6, Pattern::GlobalTag("Thing"));
        let patterns = &[pattern];
        let expected = Closure(patterns, arena.alloc(Located::new(0, 0, 10, 12, Num("42"))));
        let actual = parse_expr_with(&arena, "\\Thing -> 42");

        assert_eq!(Ok(expected), actual);
    }

    // LISTS

    #[test]
    fn empty_list() {
        let arena = Bump::new();
        let expected = List {
            items: &[],
            final_comments: &[],
        };
        let actual = parse_expr_with(&arena, "[]");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn spaces_inside_empty_list() {
        // This is a regression test!
        let arena = Bump::new();
        let expected = List {
            items: &[],
            final_comments: &[],
        };
        let actual = parse_expr_with(&arena, "[  ]");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn packed_singleton_list() {
        let arena = Bump::new();
        let items = &[&*arena.alloc(Located::new(0, 0, 1, 2, Num("1")))];
        let expected = List {
            items,
            final_comments: &[],
        };
        let actual = parse_expr_with(&arena, "[1]");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn spaced_singleton_list() {
        let arena = Bump::new();
        let items = &[&*arena.alloc(Located::new(0, 0, 2, 3, Num("1")))];
        let expected = List {
            items,
            final_comments: &[],
        };
        let actual = parse_expr_with(&arena, "[ 1 ]");

        assert_eq!(Ok(expected), actual);
    }

    // FIELD ACCESS

    #[test]
    fn basic_field() {
        let arena = Bump::new();
        let var = Var {
            module_name: "",
            ident: "rec",
        };
        let expected = Access(arena.alloc(var), "field");
        let actual = parse_expr_with(&arena, "rec.field");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_basic_field() {
        let arena = Bump::new();
        let paren_var = ParensAround(arena.alloc(Var {
            module_name: "",
            ident: "rec",
        }));
        let expected = Access(arena.alloc(paren_var), "field");
        let actual = parse_expr_with(&arena, "(rec).field");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_field_qualified_var() {
        let arena = Bump::new();
        let paren_var = ParensAround(arena.alloc(Var {
            module_name: "One.Two",
            ident: "rec",
        }));
        let expected = Access(arena.alloc(paren_var), "field");
        let actual = parse_expr_with(&arena, "(One.Two.rec).field");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn multiple_fields() {
        let arena = Bump::new();
        let var = Var {
            module_name: "",
            ident: "rec",
        };
        let expected = Access(
            arena.alloc(Access(arena.alloc(Access(arena.alloc(var), "abc")), "def")),
            "ghi",
        );
        let actual = parse_expr_with(&arena, "rec.abc.def.ghi");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_field() {
        let arena = Bump::new();
        let var = Var {
            module_name: "One.Two",
            ident: "rec",
        };
        let expected = Access(
            arena.alloc(Access(arena.alloc(Access(arena.alloc(var), "abc")), "def")),
            "ghi",
        );
        let actual = parse_expr_with(&arena, "One.Two.rec.abc.def.ghi");

        assert_eq!(Ok(expected), actual);
    }

    // APPLY

    #[test]
    fn basic_apply() {
        let arena = Bump::new();
        let arg = arena.alloc(Located::new(0, 0, 5, 6, Num("1")));
        let args = &[&*arg];
        let expr = Var {
            module_name: "",
            ident: "whee",
        };
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, expr)),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "whee 1");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_two_args() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Num("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 10, 12, Num("34")));
        let args = &[&*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(
                0,
                0,
                0,
                4,
                Var {
                    module_name: "",
                    ident: "whee",
                },
            )),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "whee  12  34");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_three_args() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(
            0,
            0,
            2,
            3,
            Var {
                module_name: "",
                ident: "b",
            },
        ));
        let arg2 = arena.alloc(Located::new(
            0,
            0,
            4,
            5,
            Var {
                module_name: "",
                ident: "c",
            },
        ));
        let arg3 = arena.alloc(Located::new(
            0,
            0,
            6,
            7,
            Var {
                module_name: "",
                ident: "d",
            },
        ));
        let args = &[&*arg1, &*arg2, &*arg3];
        let expected = Expr::Apply(
            arena.alloc(Located::new(
                0,
                0,
                0,
                1,
                Var {
                    module_name: "",
                    ident: "a",
                },
            )),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "a b c d");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_apply() {
        let arena = Bump::new();
        let arg = arena.alloc(Located::new(0, 0, 7, 8, Num("1")));
        let args = &[&*arg];
        let parens_var = Expr::ParensAround(arena.alloc(Var {
            module_name: "",
            ident: "whee",
        }));
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 1, 5, parens_var)),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "(whee) 1");

        assert_eq!(Ok(expected), actual);
    }

    // UNARY OPERATORS

    #[test]
    fn unary_negation() {
        let arena = Bump::new();
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Negate);
        let arg1_expr = Var {
            module_name: "",
            ident: "foo",
        };
        let loc_arg1_expr = Located::new(0, 0, 1, 4, arg1_expr);
        let expected = UnaryOp(arena.alloc(loc_arg1_expr), loc_op);
        let actual = parse_expr_with(&arena, "-foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_not() {
        let arena = Bump::new();
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Not);
        let arg1_expr = Var {
            module_name: "",
            ident: "blah",
        };
        let loc_arg1_expr = Located::new(0, 0, 1, 5, arg1_expr);
        let expected = UnaryOp(arena.alloc(loc_arg1_expr), loc_op);
        let actual = parse_expr_with(&arena, "!blah");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_unary_negation() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 7, 9, Num("12")));
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Negate);
        let arg2 = arena.alloc(Located::new(
            0,
            0,
            10,
            13,
            Var {
                module_name: "",
                ident: "foo",
            },
        ));
        let args = &[&*arg1, &*arg2];
        let function = Located::new(
            0,
            0,
            1,
            5,
            Var {
                module_name: "",
                ident: "whee",
            },
        );
        let unary = Located::new(0, 0, 0, 5, UnaryOp(arena.alloc(function), loc_op));
        let expected = Expr::Apply(arena.alloc(unary), args, CalledVia::Space);
        let actual = parse_expr_with(&arena, "-whee  12 foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_unary_not() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 7, 9, Num("12")));
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Not);
        let arg2 = arena.alloc(Located::new(
            0,
            0,
            10,
            13,
            Var {
                module_name: "",
                ident: "foo",
            },
        ));
        let args = &[&*arg1, &*arg2];

        let function = Located::new(
            0,
            0,
            1,
            5,
            Var {
                module_name: "",
                ident: "whee",
            },
        );
        let unary = Located::new(0, 0, 0, 5, UnaryOp(arena.alloc(function), loc_op));
        let expected = Expr::Apply(arena.alloc(unary), args, CalledVia::Space);
        let actual = parse_expr_with(&arena, "!whee  12 foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_negation_with_parens() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 8, 10, Num("12")));
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Negate);
        let arg2 = arena.alloc(Located::new(
            0,
            0,
            11,
            14,
            Var {
                module_name: "",
                ident: "foo",
            },
        ));
        let args = &[&*arg1, &*arg2];
        let apply_expr = Expr::ParensAround(arena.alloc(Expr::Apply(
            arena.alloc(Located::new(
                0,
                0,
                2,
                6,
                Var {
                    module_name: "",
                    ident: "whee",
                },
            )),
            args,
            CalledVia::Space,
        )));
        let expected = UnaryOp(arena.alloc(Located::new(0, 0, 2, 14, apply_expr)), loc_op);
        let actual = parse_expr_with(&arena, "-(whee  12 foo)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_not_with_parens() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 8, 10, Num("12")));
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Not);
        let arg2 = arena.alloc(Located::new(
            0,
            0,
            11,
            14,
            Var {
                module_name: "",
                ident: "foo",
            },
        ));
        let args = &[&*arg1, &*arg2];
        let apply_expr = Expr::ParensAround(arena.alloc(Expr::Apply(
            arena.alloc(Located::new(
                0,
                0,
                2,
                6,
                Var {
                    module_name: "",
                    ident: "whee",
                },
            )),
            args,
            CalledVia::Space,
        )));
        let expected = UnaryOp(arena.alloc(Located::new(0, 0, 2, 14, apply_expr)), loc_op);
        let actual = parse_expr_with(&arena, "!(whee  12 foo)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_negation_arg() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Num("12")));
        let loc_op = Located::new(0, 0, 9, 10, UnaryOp::Negate);
        let var1 = Var {
            module_name: "",
            ident: "foo",
        };
        let loc_arg1_expr = Located::new(0, 0, 10, 13, var1);
        let arg_op = UnaryOp(arena.alloc(loc_arg1_expr), loc_op);
        let arg2 = arena.alloc(Located::new(0, 0, 9, 13, arg_op));
        let args = &[&*arg1, &*arg2];
        let var2 = Var {
            module_name: "",
            ident: "whee",
        };
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, var2)),
            args,
            CalledVia::Space,
        );
        let actual = parse_expr_with(&arena, "whee  12 -foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_negation_access() {
        // Regression test for https://github.com/rtfeldman/roc/issues/509
        let arena = Bump::new();
        let var = Var {
            module_name: "",
            ident: "rec1",
        };
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Negate);
        let access = Access(arena.alloc(var), "field");
        let loc_access = Located::new(0, 0, 1, 11, access);
        let expected = UnaryOp(arena.alloc(loc_access), loc_op);
        let actual = parse_expr_with(&arena, "-rec1.field");

        assert_eq!(Ok(expected), actual);
    }

    // CLOSURE

    #[test]
    fn single_arg_closure() {
        let arena = Bump::new();
        let pattern = Located::new(0, 0, 1, 2, Identifier("a"));
        let patterns = &[pattern];
        let expected = Closure(patterns, arena.alloc(Located::new(0, 0, 6, 8, Num("42"))));
        let actual = parse_expr_with(&arena, "\\a -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn single_underscore_closure() {
        let arena = Bump::new();
        let pattern = Located::new(0, 0, 1, 2, Pattern::Underscore(""));
        let patterns = &[pattern];
        let expected = Closure(patterns, arena.alloc(Located::new(0, 0, 6, 8, Num("42"))));
        let actual = parse_expr_with(&arena, "\\_ -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn malformed_ident_due_to_underscore() {
        // This is a regression test against a bug where if you included an
        // underscore in an argument name, it would parse as three arguments
        // (and would ignore the underscore as if it had been blank space).
        let arena = Bump::new();

        let pattern = Located::new(
            0,
            0,
            1,
            11,
            Pattern::MalformedIdent("the_answer", roc_parse::ident::BadIdent::Underscore(0, 5)),
        );
        let patterns = &[pattern];
        let expr = Located::new(0, 0, 15, 17, Expr::Num("42"));

        let expected = Closure(patterns, &expr);
        let actual = parse_expr_with(&arena, "\\the_answer -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn two_arg_closure() {
        let arena = Bump::new();
        let arg1 = Located::new(0, 0, 1, 2, Identifier("a"));
        let arg2 = Located::new(0, 0, 4, 5, Identifier("b"));
        let patterns = &[arg1, arg2];
        let expected = Closure(patterns, arena.alloc(Located::new(0, 0, 9, 11, Num("42"))));
        let actual = parse_expr_with(&arena, "\\a, b -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn three_arg_closure() {
        let arena = Bump::new();
        let arg1 = Located::new(0, 0, 1, 2, Identifier("a"));
        let arg2 = Located::new(0, 0, 4, 5, Identifier("b"));
        let arg3 = Located::new(0, 0, 7, 8, Identifier("c"));
        let patterns = bumpalo::vec![in &arena; arg1, arg2, arg3];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 12, 14, Num("42"))),
        );
        let actual = parse_expr_with(&arena, "\\a, b, c -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn closure_with_underscores() {
        let arena = Bump::new();
        let underscore1 = Located::new(0, 0, 1, 2, Pattern::Underscore(""));
        let underscore2 = Located::new(0, 0, 4, 9, Pattern::Underscore("name"));
        let patterns = bumpalo::vec![in &arena; underscore1, underscore2];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 13, 15, Num("42"))),
        );
        let actual = parse_expr_with(&arena, "\\_, _name -> 42");

        assert_eq!(Ok(expected), actual);
    }

    // DEF

    #[test]
    fn one_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(1, 1, 2, 3, Num("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 3, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                x=5

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn if_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(0, 0, 0, 4, Identifier("iffy"))),
            arena.alloc(Located::new(0, 0, 5, 6, Num("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(0, 0, 0, 6, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(2, 2, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                    iffy=5

                    42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn one_spaced_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(1, 1, 4, 5, Num("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 5, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                x = 5

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn two_spaced_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let newline = bumpalo::vec![in &arena; Newline];
        let def1 = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(1, 1, 4, 5, Num("5"))),
        );
        let loc_def1 = &*arena.alloc(Located::new(1, 1, 0, 5, def1));
        let def2 = Def::SpaceBefore(
            &*arena.alloc(Def::Body(
                arena.alloc(Located::new(2, 2, 0, 1, Identifier("y"))),
                arena.alloc(Located::new(2, 2, 4, 5, Num("6"))),
            )),
            newline.into_bump_slice(),
        );
        let loc_def2 = &*arena.alloc(Located::new(2, 2, 0, 5, def2));
        let defs = &[loc_def1, loc_def2];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(4, 4, 0, 2, ret);
        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                x = 5
                y = 6

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn record_destructure_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let newline = bumpalo::vec![in &arena; Newline];
        let fields = bumpalo::vec![in &arena;
            Located::new(1, 1, 2, 3, Identifier("x")),
            Located::new(1, 1, 5, 7, Identifier("y"))
        ];
        let def1 = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 8, RecordDestructure(&fields))),
            arena.alloc(Located::new(1, 1, 11, 12, Num("5"))),
        );
        let loc_def1 = &*arena.alloc(Located::new(1, 1, 0, 12, def1));
        let def2 = Def::SpaceBefore(
            &*arena.alloc(Def::Body(
                arena.alloc(Located::new(2, 2, 0, 1, Identifier("y"))),
                arena.alloc(Located::new(2, 2, 4, 5, Num("6"))),
            )),
            &newline,
        );
        let loc_def2 = &*arena.alloc(Located::new(2, 2, 0, 5, def2));
        let defs = &[loc_def1, loc_def2];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), &newlines);
        let loc_ret = Located::new(4, 4, 0, 2, ret);
        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            &reset_indentation,
        );

        assert_parses_to(
            indoc!(
                r#"
                # leading comment
                { x, y } = 5
                y = 6

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn one_backpassing() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let identifier_x = Located::new(1, 1, 0, 1, Identifier("x"));
        let identifier_y = Located::new(1, 1, 7, 8, Identifier("y"));

        let var_x = Var {
            module_name: "",
            ident: "x",
        };

        let var_y = Var {
            module_name: "",
            ident: "y",
        };
        let loc_var_y = arena.alloc(Located::new(1, 1, 12, 13, var_y));

        let closure = ParensAround(arena.alloc(Closure(arena.alloc([identifier_y]), loc_var_y)));
        let loc_closure = Located::new(1, 1, 5, 14, closure);

        let ret = Expr::SpaceBefore(arena.alloc(var_x), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 1, ret);

        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Expr::Backpassing(
                arena.alloc([identifier_x]),
                arena.alloc(loc_closure),
                arena.alloc(loc_ret),
            )),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                x <- (\y -> y)

                x
                "#
            ),
            expected,
        );
    }

    #[test]
    fn underscore_backpassing() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let underscore = Located::new(1, 1, 0, 1, Pattern::Underscore(""));
        let identifier_y = Located::new(1, 1, 7, 8, Identifier("y"));

        let num_4 = Num("4");

        let var_y = Var {
            module_name: "",
            ident: "y",
        };
        let loc_var_y = arena.alloc(Located::new(1, 1, 12, 13, var_y));

        let closure = ParensAround(arena.alloc(Closure(arena.alloc([identifier_y]), loc_var_y)));
        let loc_closure = Located::new(1, 1, 5, 14, closure);

        let ret = Expr::SpaceBefore(arena.alloc(num_4), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 1, ret);

        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Expr::Backpassing(
                arena.alloc([underscore]),
                arena.alloc(loc_closure),
                arena.alloc(loc_ret),
            )),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                _ <- (\y -> y)

                4
                "#
            ),
            expected,
        );
    }

    #[test]
    fn two_backpassing() {
        let arena = Bump::new();

        let inner_backpassing = {
            let identifier_z = Located::new(2, 2, 0, 1, Identifier("z"));

            let empty_record = Record {
                fields: &[],
                final_comments: &[],
            };
            let loc_empty_record = Located::new(2, 2, 5, 7, empty_record);

            let var_x = Var {
                module_name: "",
                ident: "x",
            };

            let newlines = bumpalo::vec![in &arena; Newline, Newline];
            let ret = Expr::SpaceBefore(arena.alloc(var_x), newlines.into_bump_slice());
            let loc_ret = Located::new(4, 4, 0, 1, ret);

            Expr::SpaceBefore(
                arena.alloc(Expr::Backpassing(
                    arena.alloc([identifier_z]),
                    arena.alloc(loc_empty_record),
                    arena.alloc(loc_ret),
                )),
                arena.alloc([Newline]),
            )
        };

        let identifier_x = Located::new(1, 1, 0, 1, Identifier("x"));
        let identifier_y = Located::new(1, 1, 7, 8, Identifier("y"));

        let var_y = Var {
            module_name: "",
            ident: "y",
        };
        let loc_var_y = arena.alloc(Located::new(1, 1, 12, 13, var_y));

        let closure = ParensAround(arena.alloc(Closure(arena.alloc([identifier_y]), loc_var_y)));
        let loc_closure = Located::new(1, 1, 5, 14, closure);

        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Expr::Backpassing(
                arena.alloc([identifier_x]),
                arena.alloc(loc_closure),
                arena.alloc(Located::new(2, 4, 0, 1, inner_backpassing)),
            )),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                x <- (\y -> y)
                z <- {} 

                x
                "#
            ),
            expected,
        );
    }

    #[test]
    fn multi_backpassing() {
        let arena = Bump::new();

        let identifier_x = Located::new(0, 0, 0, 1, Identifier("x"));
        let identifier_y = Located::new(0, 0, 3, 4, Identifier("y"));

        let var_x = Var {
            module_name: "",
            ident: "x",
        };
        let loc_var_x = Located::new(2, 2, 0, 1, var_x);

        let var_y = Var {
            module_name: "",
            ident: "y",
        };
        let loc_var_y = Located::new(2, 2, 4, 5, var_y);

        let var_list_map2 = Var {
            module_name: "List",
            ident: "map2",
        };

        let apply = Expr::Apply(
            arena.alloc(Located::new(0, 0, 8, 17, var_list_map2)),
            bumpalo::vec![ in &arena;
                &*arena.alloc(Located::new(0, 0, 18, 20, Expr::List{ items: &[], final_comments: &[] })),
                &*arena.alloc(Located::new(0, 0, 21, 23, Expr::List{ items: &[], final_comments: &[] })),
            ]
            .into_bump_slice(),
            CalledVia::Space,
        );

        let loc_closure = Located::new(0, 0, 8, 23, apply);

        let binop = single_binop(
            &arena,
            (
                loc_var_x,
                Located::new(2, 2, 2, 3, roc_module::operator::BinOp::Plus),
                loc_var_y,
            ),
        );

        let spaced_binop = Expr::SpaceBefore(arena.alloc(binop), &[Newline, Newline]);

        let expected = Expr::Backpassing(
            arena.alloc([identifier_x, identifier_y]),
            arena.alloc(loc_closure),
            arena.alloc(Located::new(2, 2, 0, 5, spaced_binop)),
        );

        assert_parses_to(
            indoc!(
                r#"
                x, y <- List.map2 [] []

                x + y
                "#
            ),
            expected,
        );
    }

    // #[test]
    // fn type_signature_def() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let applied_ann = TypeAnnotation::Apply("", "Int", &[]);
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 9, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 7, Num("4"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), &newline);
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 7, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), &newlines);
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : Int
    //             foo = 4

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    #[test]
    fn parse_as_ann() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let loc_x = Located::new(0, 0, 18, 19, TypeAnnotation::BoundVariable("x"));
        let loc_y = Located::new(0, 0, 20, 21, TypeAnnotation::BoundVariable("y"));
        let loc_a = Located::new(0, 0, 30, 31, TypeAnnotation::BoundVariable("a"));
        let loc_b = Located::new(0, 0, 32, 33, TypeAnnotation::BoundVariable("b"));
        let applied_ann_args = bumpalo::vec![in &arena; loc_x, loc_y];
        let applied_ann =
            TypeAnnotation::Apply("Foo.Bar", "Baz", applied_ann_args.into_bump_slice());
        let loc_applied_ann = &*arena.alloc(Located::new(0, 0, 6, 21, applied_ann));
        let applied_as_args = bumpalo::vec![in &arena; loc_a, loc_b];
        let applied_as = TypeAnnotation::Apply("", "Blah", applied_as_args.into_bump_slice());
        let loc_applied_as = &*arena.alloc(Located::new(0, 0, 25, 33, applied_as));
        let as_ann = TypeAnnotation::As(loc_applied_ann, &[], loc_applied_as);
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 6, 33, as_ann),
        );

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 33, signature));
        let defs = &[loc_ann];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(2, 2, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : Foo.Bar.Baz x y as Blah a b

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn parse_alias() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let loc_x = Located::new(0, 0, 23, 24, TypeAnnotation::BoundVariable("x"));
        let loc_y = Located::new(0, 0, 25, 26, TypeAnnotation::BoundVariable("y"));
        let loc_a = Located::new(0, 0, 5, 6, Pattern::Identifier("a"));
        let loc_b = Located::new(0, 0, 7, 8, Pattern::Identifier("b"));
        let applied_ann_args = bumpalo::vec![in &arena; loc_a, loc_b];
        let applied_alias_args = bumpalo::vec![in &arena; loc_x, loc_y];
        let applied_alias =
            TypeAnnotation::Apply("Foo.Bar", "Baz", applied_alias_args.into_bump_slice());
        let signature = Def::Alias {
            name: Located::new(0, 0, 0, 4, "Blah"),
            vars: applied_ann_args.into_bump_slice(),
            ann: Located::new(0, 0, 11, 26, applied_alias),
        };

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 26, signature));
        let defs = &[loc_ann];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(2, 2, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                Blah a b : Foo.Bar.Baz x y

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn multiline_type_signature() {
        assert_parses_to(
            "f :\n    {}\n\n42",
            Defs(
                &[&Located::new(
                    0,
                    1,
                    0,
                    6,
                    Def::Annotation(
                        Located::new(0, 0, 0, 1, Pattern::Identifier("f")),
                        Located::new(
                            1,
                            1,
                            4,
                            6,
                            TypeAnnotation::SpaceBefore(
                                &TypeAnnotation::Record {
                                    fields: &[],
                                    ext: None,
                                    final_comments: &[],
                                },
                                &[Newline],
                            ),
                        ),
                    ),
                )],
                &Located::new(
                    3,
                    3,
                    0,
                    2,
                    Expr::SpaceBefore(&Expr::Num("42"), &[Newline, Newline]),
                ),
            ),
        );
    }

    #[test]
    fn multiline_type_signature_with_comment() {
        assert_parses_to(
            "f :# comment\n    {}\n\n42",
            Defs(
                &[&Located::new(
                    0,
                    1,
                    0,
                    6,
                    Def::Annotation(
                        Located::new(0, 0, 0, 1, Pattern::Identifier("f")),
                        Located::new(
                            1,
                            1,
                            4,
                            6,
                            TypeAnnotation::SpaceBefore(
                                &TypeAnnotation::Record {
                                    fields: &[],
                                    ext: None,
                                    final_comments: &[],
                                },
                                &[LineComment(" comment")],
                            ),
                        ),
                    ),
                )],
                &Located::new(
                    3,
                    3,
                    0,
                    2,
                    Expr::SpaceBefore(&Expr::Num("42"), &[Newline, Newline]),
                ),
            ),
        );
    }
    // #[test]
    // fn type_signature_function_def() {
    //     use TypeAnnotation;
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];

    //     let int_type = TypeAnnotation::Apply("", "Int", &[]);
    //     let float_type = TypeAnnotation::Apply("", "Float", &[]);
    //     let bool_type = TypeAnnotation::Apply("", "Bool", &[]);

    //     let arguments = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 6, 9, int_type),
    //         Located::new(0, 0, 11, 16, float_type)
    //     ];
    //     let return_type = Located::new(0, 0, 20, 24, bool_type);
    //     let fn_ann = TypeAnnotation::Function(&arguments, &return_type);
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 20, 24, fn_ann),
    //     );

    //     let args = bumpalo::vec![in &arena;
    //     Located::new(1,1,7,8, Identifier("x")),
    //     Located::new(1,1,10,11, Underscore)
    //     ];
    //     let body = Located::new(1, 1, 15, 17, Num("42"));

    //     let closure = Expr::Closure(&args, &body);

    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 17, closure)),
    //     );
    //     let spaced = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 17, spaced));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : Int, Float -> Bool
    //             foo = \x, _ -> 42

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_private_open_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Private {
    //         name: Located::new(0, 0, 8, 13, "@True"),
    //         args: &[],
    //     };
    //     let tag2arg1 = Located::new(0, 0, 24, 27, TypeAnnotation::Apply("", "Two", &[]));
    //     let tag2arg2 = Located::new(0, 0, 28, 34, TypeAnnotation::Apply("", "Things", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg1, tag2arg2];
    //     let tag2 = Tag::Private {
    //         name: Located::new(0, 0, 15, 23, "@Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 13, tag1),
    //         Located::new(0, 0, 15, 34, tag2)
    //     ];
    //     let loc_wildcard = Located::new(0, 0, 36, 37, TypeAnnotation::Wildcard);
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: Some(arena.alloc(loc_wildcard)),
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 37, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : [ @True, @Perhaps Two Things ]*
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_private_closed_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Private {
    //         name: Located::new(0, 0, 8, 13, "@True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 24, 29, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Private {
    //         name: Located::new(0, 0, 15, 23, "@Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 13, tag1),
    //         Located::new(0, 0, 15, 29, tag2)
    //     ];
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: None,
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 31, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : [ @True, @Perhaps Thing ]
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_global_open_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Global {
    //         name: Located::new(0, 0, 8, 12, "True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Global {
    //         name: Located::new(0, 0, 14, 21, "Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 12, tag1),
    //         Located::new(0, 0, 14, 27, tag2)
    //     ];
    //     let loc_wildcard = Located::new(0, 0, 29, 30, TypeAnnotation::Wildcard);
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: Some(arena.alloc(loc_wildcard)),
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 30, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : [ True, Perhaps Thing ]*
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_global_closed_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Global {
    //         name: Located::new(0, 0, 8, 12, "True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Global {
    //         name: Located::new(0, 0, 14, 21, "Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 12, tag1),
    //         Located::new(0, 0, 14, 27, tag2)
    //     ];
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: None,
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 29, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : [ True, Perhaps Thing ]
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // WHEN

    #[test]
    fn two_branch_when() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern1 = Pattern::SpaceBefore(
            arena.alloc(StrLiteral(PlainLine(""))),
            newlines.into_bump_slice(),
        );
        let loc_pattern1 = Located::new(1, 1, 1, 3, pattern1);
        let expr1 = Num("1");
        let loc_expr1 = Located::new(1, 1, 7, 8, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(StrLiteral(PlainLine("mise"))), newlines);
        let loc_pattern2 = Located::new(2, 2, 1, 7, pattern2);
        let expr2 = Num("2");
        let loc_expr2 = Located::new(2, 2, 11, 12, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                    when x is
                     "" -> 1
                     "mise" -> 2
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_with_numbers() {
        let arena = Bump::new();
        let newlines = &[Newline];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(NumLiteral("1")), newlines);
        let loc_pattern1 = Located::new(1, 1, 1, 2, pattern1);
        let expr1 = Num("2");
        let loc_expr1 = Located::new(1, 1, 6, 7, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(NumLiteral("3")), newlines);
        let loc_pattern2 = Located::new(2, 2, 1, 2, pattern2);
        let expr2 = Num("4");
        let loc_expr2 = Located::new(2, 2, 6, 7, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when x is
                 1 -> 2
                 3 -> 4
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_with_negative_numbers() {
        let arena = Bump::new();
        let newlines = &[Newline];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(NumLiteral("1")), newlines);
        let loc_pattern1 = Located::new(1, 1, 1, 2, pattern1);
        let expr1 = Num("2");
        let loc_expr1 = Located::new(1, 1, 6, 7, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(NumLiteral("-3")), newlines);
        let loc_pattern2 = Located::new(2, 2, 1, 3, pattern2);
        let expr2 = Num("4");
        let loc_expr2 = Located::new(2, 2, 7, 8, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when x is
                 1 -> 2
                 -3 -> 4
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_with_function_application() {
        let arena = Bump::new();
        let newlines = &[Newline];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(NumLiteral("1")), newlines);
        let loc_pattern1 = Located::new(1, 1, 4, 5, pattern1);
        let num_2 = Num("2");
        let num_neg = Located::new(
            1,
            1,
            9,
            16,
            Expr::Var {
                module_name: "Num",
                ident: "neg",
            },
        );
        let expr0 = Located::new(2, 2, 5, 6, Expr::SpaceBefore(&num_2, &[Newline]));
        let expr1 = Expr::Apply(
            &num_neg,
            &*arena.alloc([&*arena.alloc(expr0)]),
            CalledVia::Space,
        );
        let loc_expr1 = Located::new(1, 2, 9, 6, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(Pattern::Underscore("")), newlines);
        let loc_pattern2 = Located::new(3, 3, 4, 5, pattern2);
        let expr2 = Num("4");
        let loc_expr2 = Located::new(3, 3, 9, 10, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when x is
                    1 -> Num.neg
                     2 
                    _ -> 4
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_if_guard() {
        let arena = Bump::new();

        let branch1 = {
            let newlines = &[Newline];
            let pattern1 = Pattern::SpaceBefore(arena.alloc(Pattern::Underscore("")), newlines);
            let loc_pattern1 = Located::new(1, 1, 4, 5, pattern1);
            let num_1 = Num("1");
            let expr1 = Located::new(
                2,
                2,
                8,
                9,
                Expr::SpaceBefore(arena.alloc(num_1), &[Newline]),
            );
            let loc_expr1 = expr1; // Located::new(1, 2, 9, 6, expr1);
            &*arena.alloc(WhenBranch {
                patterns: arena.alloc([loc_pattern1]),
                value: loc_expr1,
                guard: None,
            })
        };

        let branch2 = {
            let pattern1 =
                Pattern::SpaceBefore(arena.alloc(Pattern::Underscore("")), &[Newline, Newline]);
            let loc_pattern1 = Located::new(4, 4, 4, 5, pattern1);
            let num_1 = Num("2");
            let expr1 = Located::new(
                5,
                5,
                8,
                9,
                Expr::SpaceBefore(arena.alloc(num_1), &[Newline]),
            );
            let loc_expr1 = expr1; // Located::new(1, 2, 9, 6, expr1);
            &*arena.alloc(WhenBranch {
                patterns: arena.alloc([loc_pattern1]),
                value: loc_expr1,
                guard: None,
            })
        };

        let branch3 = {
            let pattern1 =
                Pattern::SpaceBefore(arena.alloc(Pattern::GlobalTag("Ok")), &[Newline, Newline]);
            let loc_pattern1 = Located::new(7, 7, 4, 6, pattern1);
            let num_1 = Num("3");
            let expr1 = Located::new(
                8,
                8,
                8,
                9,
                Expr::SpaceBefore(arena.alloc(num_1), &[Newline]),
            );
            let loc_expr1 = expr1; // Located::new(1, 2, 9, 6, expr1);
            &*arena.alloc(WhenBranch {
                patterns: arena.alloc([loc_pattern1]),
                value: loc_expr1,
                guard: None,
            })
        };

        let branches = &[branch1, branch2, branch3];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when x is
                    _ -> 
                        1

                    _ -> 
                        2

                    Ok -> 
                        3
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_in_parens() {
        let arena = Bump::new();

        let branch1 = {
            let newlines = &[Newline];
            let pattern1 = Pattern::SpaceBefore(arena.alloc(Pattern::GlobalTag("Ok")), newlines);
            let loc_pattern1 = Located::new(1, 1, 4, 6, pattern1);
            let num_1 = Num("3");
            let expr1 = Located::new(
                2,
                2,
                8,
                9,
                Expr::SpaceBefore(arena.alloc(num_1), &[Newline]),
            );
            let loc_expr1 = expr1; // Located::new(1, 2, 9, 6, expr1);
            &*arena.alloc(WhenBranch {
                patterns: arena.alloc([loc_pattern1]),
                value: loc_expr1,
                guard: None,
            })
        };

        let branches = &[branch1];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 6, 7, var);
        let when = Expr::When(arena.alloc(loc_cond), branches);
        let expected = Expr::ParensAround(&when);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                (when x is
                    Ok ->
                        3)
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_with_records() {
        let arena = Bump::new();
        let newlines = &[Newline];
        let identifiers1 = &[Located::new(1, 1, 3, 4, Identifier("y"))];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(RecordDestructure(identifiers1)), newlines);
        let loc_pattern1 = Located::new(1, 1, 1, 6, pattern1);
        let expr1 = Num("2");
        let loc_expr1 = Located::new(1, 1, 10, 11, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let identifiers2 = &[
            Located::new(2, 2, 3, 4, Identifier("z")),
            Located::new(2, 2, 6, 7, Identifier("w")),
        ];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(RecordDestructure(identifiers2)), newlines);
        let loc_pattern2 = Located::new(2, 2, 1, 9, pattern2);
        let expr2 = Num("4");
        let loc_expr2 = Located::new(2, 2, 13, 14, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when x is
                 { y } -> 2
                 { z, w } -> 4
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_in_parens_indented() {
        let arena = Bump::new();

        let branch1 = {
            let newlines = &[Newline];
            let pattern1 = Pattern::SpaceBefore(arena.alloc(Pattern::GlobalTag("Ok")), newlines);
            let loc_pattern1 = Located::new(1, 1, 4, 6, pattern1);
            let num_1 = Num("3");
            let expr1 = Located::new(1, 1, 10, 11, num_1);
            let loc_expr1 = expr1; // Located::new(1, 2, 9, 6, expr1);
            &*arena.alloc(WhenBranch {
                patterns: arena.alloc([loc_pattern1]),
                value: loc_expr1,
                guard: None,
            })
        };

        let branches = &[branch1];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 6, 7, var);
        let when = Expr::When(arena.alloc(loc_cond), branches);
        let spaced = Expr::SpaceAfter(&when, &[Newline]);
        let expected = Expr::ParensAround(&spaced);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                (when x is
                    Ok -> 3 
                     )
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_with_alternative_patterns() {
        let arena = Bump::new();
        let newlines = &[Newline];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(StrLiteral(PlainLine("blah"))), newlines);
        let pattern1_alt = StrLiteral(PlainLine("blop"));
        let loc_pattern1 = Located::new(1, 1, 1, 7, pattern1);
        let loc_pattern1_alt = Located::new(1, 1, 10, 16, pattern1_alt);
        let expr1 = Num("1");
        let loc_expr1 = Located::new(1, 1, 20, 21, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1, loc_pattern1_alt]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(StrLiteral(PlainLine("foo"))), newlines);
        let newlines = &[Newline];
        let pattern2_alt =
            Pattern::SpaceBefore(arena.alloc(StrLiteral(PlainLine("bar"))), newlines);
        let loc_pattern2 = Located::new(2, 2, 1, 6, pattern2);
        let loc_pattern2_alt = Located::new(3, 3, 2, 7, pattern2_alt);
        let expr2 = Num("2");
        let loc_expr2 = Located::new(3, 3, 11, 12, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2, loc_pattern2_alt]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when x is
                 "blah" | "blop" -> 1
                 "foo" |
                  "bar" -> 2
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    // MODULE

    #[test]
    fn empty_app_header() {
        let arena = Bump::new();
        let packages = Vec::new_in(&arena);
        let imports = Vec::new_in(&arena);
        let provides = Vec::new_in(&arena);
        let module_name = StrLiteral::PlainLine("test-app");
        let header = AppHeader {
            name: Located::new(0, 0, 4, 14, module_name),
            packages,
            imports,
            provides,
            to: Located::new(0, 0, 53, 57, To::ExistingPackage("blah")),
            before_header: &[],
            after_app_keyword: &[],
            before_packages: &[],
            after_packages: &[],
            before_imports: &[],
            after_imports: &[],
            before_provides: &[],
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "test-app" packages {} imports [] provides [] to blah
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn minimal_app_header() {
        use PackageOrPath::Path;

        let arena = Bump::new();
        let packages = Vec::new_in(&arena);
        let imports = Vec::new_in(&arena);
        let provides = Vec::new_in(&arena);
        let module_name = StrLiteral::PlainLine("test-app");
        let header = AppHeader {
            before_header: &[],
            name: Located::new(0, 0, 4, 14, module_name),
            packages,
            imports,
            provides,
            to: Located::new(0, 0, 30, 38, To::NewPackage(Path(PlainLine("./blah")))),
            after_app_keyword: &[],
            before_packages: &[],
            after_packages: &[],
            before_imports: &[],
            after_imports: &[],
            before_provides: &[],
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "test-app" provides [] to "./blah"
            "#
        );

        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn full_app_header() {
        use ExposesEntry::Exposed;
        use PackageOrPath::Path;

        let newlines = &[Newline];
        let pkg_entry = PackageEntry::Entry {
            shorthand: "base",
            spaces_after_shorthand: &[],
            package_or_path: Located::new(1, 1, 21, 33, Path(PlainLine("./platform"))),
        };
        let loc_pkg_entry = Located::new(1, 1, 15, 33, pkg_entry);
        let arena = Bump::new();
        let packages = bumpalo::vec![in &arena; loc_pkg_entry];
        let import = ImportsEntry::Package("foo", ModuleName::new("Bar.Baz"), Vec::new_in(&arena));
        let loc_import = Located::new(2, 2, 14, 25, import);
        let imports = bumpalo::vec![in &arena; loc_import];
        let provide_entry = Located::new(3, 3, 15, 24, Exposed("quicksort"));
        let provides = bumpalo::vec![in &arena; provide_entry];
        let module_name = StrLiteral::PlainLine("quicksort");

        let header = AppHeader {
            before_header: &[],
            name: Located::new(0, 0, 4, 15, module_name),
            packages,
            imports,
            provides,
            to: Located::new(3, 3, 30, 34, To::ExistingPackage("base")),
            after_app_keyword: &[],
            before_packages: newlines,
            after_packages: &[],
            before_imports: newlines,
            after_imports: &[],
            before_provides: newlines,
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "quicksort"
                    packages { base: "./platform" }
                    imports [ foo.Bar.Baz ]
                    provides [ quicksort ] to base
            "#
        );

        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn empty_platform_header() {
        let pkg_name = PackageName {
            account: "rtfeldman",
            pkg: "blah",
        };
        let arena = Bump::new();
        let effects = Effects {
            effect_type_name: "Blah",
            effect_shortname: "fx",
            entries: &[],
            spaces_before_effects_keyword: &[],
            spaces_after_effects_keyword: &[],
            spaces_after_type_name: &[],
        };

        let requires = {
            let region1 = Region::new(0, 0, 38, 47);
            let region2 = Region::new(0, 0, 45, 47);

            PlatformRequires {
                rigids: Vec::new_in(&arena),
                signature: Located::at(
                    region1,
                    TypedIdent::Entry {
                        ident: Located::new(0, 0, 38, 42, "main"),
                        spaces_before_colon: &[],
                        ann: Located::at(
                            region2,
                            TypeAnnotation::Record {
                                fields: &[],
                                ext: None,
                                final_comments: &[],
                            },
                        ),
                    },
                ),
            }
        };

        let header = PlatformHeader {
            before_header: &[],
            name: Located::new(0, 0, 9, 23, pkg_name),
            requires,
            exposes: Vec::new_in(&arena),
            packages: Vec::new_in(&arena),
            imports: Vec::new_in(&arena),
            provides: Vec::new_in(&arena),
            effects,
            after_platform_keyword: &[],
            before_requires: &[],
            after_requires: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_packages: &[],
            after_packages: &[],
            before_imports: &[],
            after_imports: &[],
            before_provides: &[],
            after_provides: &[],
        };

        let expected = roc_parse::ast::Module::Platform { header };

        let src = "platform rtfeldman/blah requires {} { main : {} } exposes [] packages {} imports [] provides [] effects fx.Blah {}";
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn nonempty_platform_header() {
        use ExposesEntry::Exposed;
        use PackageOrPath::Path;

        let newlines = &[Newline];
        let pkg_name = PackageName {
            account: "foo",
            pkg: "barbaz",
        };
        let pkg_entry = PackageEntry::Entry {
            shorthand: "foo",
            spaces_after_shorthand: &[],
            package_or_path: Located::new(3, 3, 20, 27, Path(PlainLine("./foo"))),
        };
        let loc_pkg_entry = Located::new(3, 3, 15, 27, pkg_entry);
        let arena = Bump::new();
        let packages = bumpalo::vec![in &arena; loc_pkg_entry];
        let imports = Vec::new_in(&arena);
        let provide_entry = Located::new(5, 5, 15, 26, Exposed("mainForHost"));
        let provides = bumpalo::vec![in &arena; provide_entry];
        let effects = Effects {
            effect_type_name: "Effect",
            effect_shortname: "fx",
            entries: &[],
            spaces_before_effects_keyword: newlines,
            spaces_after_effects_keyword: &[],
            spaces_after_type_name: &[],
        };

        let requires = {
            let region1 = Region::new(1, 1, 30, 39);
            let region2 = Region::new(1, 1, 37, 39);
            let region3 = Region::new(1, 1, 14, 26);

            PlatformRequires {
                rigids: bumpalo::vec![ in &arena; Located::at(region3, PlatformRigid::Entry { alias: "Model", rigid: "model" }) ],
                signature: Located::at(
                    region1,
                    TypedIdent::Entry {
                        ident: Located::new(1, 1, 30, 34, "main"),
                        spaces_before_colon: &[],
                        ann: Located::at(
                            region2,
                            TypeAnnotation::Record {
                                fields: &[],
                                ext: None,
                                final_comments: &[],
                            },
                        ),
                    },
                ),
            }
        };

        let header = PlatformHeader {
            before_header: &[],
            name: Located::new(0, 0, 9, 19, pkg_name),
            requires,
            exposes: Vec::new_in(&arena),
            packages,
            imports,
            provides,
            effects,
            after_platform_keyword: &[],
            before_requires: newlines,
            after_requires: &[],
            before_exposes: newlines,
            after_exposes: &[],
            before_packages: newlines,
            after_packages: &[],
            before_imports: newlines,
            after_imports: &[],
            before_provides: newlines,
            after_provides: &[],
        };

        let expected = roc_parse::ast::Module::Platform { header };

        let src = indoc!(
            r#"
                platform foo/barbaz
                    requires {model=>Model} { main : {} }
                    exposes []
                    packages { foo: "./foo" }
                    imports []
                    provides [ mainForHost ]
                    effects fx.Effect {}
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn empty_interface_header() {
        let arena = Bump::new();
        let exposes = Vec::new_in(&arena);
        let imports = Vec::new_in(&arena);
        let module_name = ModuleName::new("Foo");
        let header = InterfaceHeader {
            before_header: &[],
            name: Located::new(0, 0, 10, 13, module_name),
            exposes,
            imports,

            after_interface_keyword: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_imports: &[],
            after_imports: &[],
        };

        let expected = roc_parse::ast::Module::Interface { header };

        let src = indoc!(
            r#"
                interface Foo exposes [] imports []
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn nested_module() {
        let arena = Bump::new();
        let exposes = Vec::new_in(&arena);
        let imports = Vec::new_in(&arena);
        let module_name = ModuleName::new("Foo.Bar.Baz");
        let header = InterfaceHeader {
            before_header: &[],
            name: Located::new(0, 0, 10, 21, module_name),
            exposes,
            imports,

            after_interface_keyword: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_imports: &[],
            after_imports: &[],
        };

        let expected = roc_parse::ast::Module::Interface { header };

        let src = indoc!(
            r#"
                interface Foo.Bar.Baz exposes [] imports []
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
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
            r#"
                foo = \list ->
                    isTest = \_ -> 5
                    List.map list isTest
            "#
        );
        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.1);

        // It should occur twice in the debug output - once for the pattern,
        // and then again for the lookup.
        let occurrences = format!("{:?}", actual).split("isTest").count() - 1;

        assert_eq!(occurrences, 2);
    }

    #[test]
    fn standalone_module_defs() {
        use Def::*;

        let arena = Bump::new();
        let pattern1 = Identifier("foo");
        let pattern2 = Identifier("bar");
        let pattern3 = Identifier("baz");
        let def1 = SpaceBefore(
            arena.alloc(Body(
                arena.alloc(Located::new(1, 1, 0, 3, pattern1)),
                arena.alloc(Located::new(1, 1, 6, 7, Num("1"))),
            )),
            &[LineComment(" comment 1")],
        );
        let def2 = SpaceBefore(
            arena.alloc(Body(
                arena.alloc(Located::new(4, 4, 0, 3, pattern2)),
                arena.alloc(Located::new(4, 4, 6, 10, Str(PlainLine("hi")))),
            )),
            &[Newline, Newline, LineComment(" comment 2")],
        );
        let def3 = SpaceAfter(
            arena.alloc(SpaceBefore(
                arena.alloc(Body(
                    arena.alloc(Located::new(5, 5, 0, 3, pattern3)),
                    arena.alloc(Located::new(5, 5, 6, 13, Str(PlainLine("stuff")))),
                )),
                &[Newline],
            )),
            &[Newline, LineComment(" comment n")],
        );

        let expected = bumpalo::vec![in &arena;
            Located::new(1,1, 0, 7, def1),
            Located::new(4,4, 0, 10, def2),
            Located::new(5,5, 0, 13, def3),
        ];

        let src = indoc!(
            r#"
                # comment 1
                foo = 1

                # comment 2
                bar = "hi"
                baz = "stuff"
                # comment n
            "#
        );

        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.1);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn module_def_newline() {
        let arena = Bump::new();

        let src = indoc!(
            r#"
                main =
                    i = 64

                    i
            "#
        );

        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert!(actual.is_ok());
    }

    #[test]
    fn nested_def_annotation() {
        let arena = Bump::new();

        let src = indoc!(
            r#"
            main =
                wrappedNotEq : a, a -> Bool
                wrappedNotEq = \num1, num2 ->
                    num1 != num2

                wrappedNotEq 2 3
            "#
        );

        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert!(actual.is_ok());
    }

    #[test]
    fn outdenting_newline_after_else() {
        let arena = &Bump::new();

        // highlights a problem with the else branch demanding a newline after its expression
        let src = indoc!(
            r#"
            main =
                v = \y -> if x then y else z

                1
            "#
        );

        let state = State::new(src.as_bytes());
        let parser = module_defs();
        let parsed = parser.parse(arena, state);
        match parsed {
            Ok((_, _, _state)) => {
                // dbg!(_state);
            }
            Err((_, _fail, _state)) => {
                // dbg!(_fail, _state);
                assert!(false);
            }
        }
    }

    #[test]
    fn newline_after_equals() {
        // Regression test for https://github.com/rtfeldman/roc/issues/51
        let arena = Bump::new();
        let newlines = &[Newline, Newline];
        let num = arena.alloc(Num("5"));
        let def = Def::Body(
            arena.alloc(Located::new(0, 0, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(1, 1, 4, 5, Expr::SpaceBefore(num, &[Newline]))),
        );
        let loc_def = &*arena.alloc(Located::new(0, 1, 0, 5, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines);
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                x =
                    5

                42
                "#
            ),
            expected,
        );
    }

    // DOCS

    #[test]
    fn basic_docs() {
        let arena = Bump::new();
        let newlines = &[Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(6, 6, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(6, 6, 4, 5, Num("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(6, 6, 0, 5, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines);
        let loc_ret = Located::new(8, 8, 0, 2, ret);
        let reset_indentation = &[
            DocComment("first line of docs"),
            DocComment("    second line"),
            DocComment(" third line"),
            DocComment("fourth line"),
            DocComment(""),
            DocComment("sixth line after doc new line"),
        ];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation,
        );

        assert_parses_to(
            indoc!(
                r#"
                    ## first line of docs
                    ##     second line
                    ##  third line
                    ## fourth line
                    ##
                    ## sixth line after doc new line
                    x = 5

                    42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn not_docs() {
        let arena = Bump::new();
        let newlines = &[Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(4, 4, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(4, 4, 4, 5, Num("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(4, 4, 0, 5, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines);
        let loc_ret = Located::new(6, 6, 0, 2, ret);
        let reset_indentation = &[
            LineComment("######"),
            LineComment("## not docs!"),
            LineComment("#still not docs"),
            LineComment("#####"),
        ];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation,
        );

        assert_parses_to(
            indoc!(
                r#"
                    #######
                    ### not docs!
                    ##still not docs
                    ######
                    x = 5

                    42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn mixed_docs() {
        let arena = Bump::new();
        let newlines = &[Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(4, 4, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(4, 4, 4, 5, Num("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(4, 4, 0, 5, def));
        let defs = &[loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines);
        let loc_ret = Located::new(6, 6, 0, 2, ret);
        let reset_indentation = &[
            LineComment("## not docs!"),
            DocComment("docs, but with a problem"),
            DocComment("(namely that this is a mix of docs and regular comments)"),
            LineComment(" not docs"),
        ];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation,
        );

        assert_parses_to(
            indoc!(
                r#"
                    ### not docs!
                    ## docs, but with a problem
                    ## (namely that this is a mix of docs and regular comments)
                    # not docs
                    x = 5

                    42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn malformed_pattern_field_access() {
        // See https://github.com/rtfeldman/roc/issues/399
        let arena = Bump::new();
        let newlines = &[Newline];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(Pattern::Malformed("bar.and")), newlines);
        let loc_pattern1 = Located::new(1, 1, 4, 11, pattern1);
        let expr1 = Num("1");
        let loc_expr1 = Located::new(1, 1, 15, 16, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(Pattern::Underscore("")), newlines);
        let loc_pattern2 = Located::new(2, 2, 4, 5, pattern2);
        let expr2 = Num("4");
        let loc_expr2 = Located::new(2, 2, 9, 10, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                    when x is
                        bar.and -> 1
                        _ -> 4
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn malformed_pattern_module_name() {
        // See https://github.com/rtfeldman/roc/issues/399
        let arena = Bump::new();
        let newlines = &[Newline];
        let pattern1 = Pattern::SpaceBefore(arena.alloc(Pattern::Malformed("Foo.and")), newlines);
        let loc_pattern1 = Located::new(1, 1, 4, 11, pattern1);
        let expr1 = Num("1");
        let loc_expr1 = Located::new(1, 1, 15, 16, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern1]),
            value: loc_expr1,
            guard: None,
        });
        let newlines = &[Newline];
        let pattern2 = Pattern::SpaceBefore(arena.alloc(Pattern::Underscore("")), newlines);
        let loc_pattern2 = Located::new(2, 2, 4, 5, pattern2);
        let expr2 = Num("4");
        let loc_expr2 = Located::new(2, 2, 9, 10, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: arena.alloc([loc_pattern2]),
            value: loc_expr2,
            guard: None,
        });
        let branches = &[branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                    when x is
                        Foo.and -> 1
                        _ -> 4
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn pattern_with_space_in_parens() {
        // https://github.com/rtfeldman/roc/issues/929
        let arena = Bump::new();
        let actual = parse_expr_with(
            &arena,
            indoc!(
                r#"
                when Delmin (Del rx) 0 is
                    Delmin (Del ry ) _ -> Node Black 0 False ry
                "#
            ),
        );

        assert!(actual.is_ok());
    }

    #[test]
    fn parse_expr_size() {
        assert_eq!(std::mem::size_of::<roc_parse::ast::Expr>(), 40);
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
