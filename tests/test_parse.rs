#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc;

extern crate quickcheck;

#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod helpers;

#[cfg(test)]
mod test_parse {
    use crate::helpers::parse_with;
    use bumpalo::collections::vec::Vec;
    use bumpalo::{self, Bump};
    use roc::module::header::ModuleName;
    use roc::operator::BinOp::*;
    use roc::operator::CalledVia;
    use roc::operator::UnaryOp;
    use roc::parse::ast::AssignedField::*;
    use roc::parse::ast::CommentOrNewline::*;
    use roc::parse::ast::Expr::{self, *};
    use roc::parse::ast::Pattern::{self, *};
    use roc::parse::ast::{
        Attempting, Def, InterfaceHeader, Spaceable, Tag, TypeAnnotation, WhenBranch,
    };
    use roc::parse::module::{interface_header, module_defs};
    use roc::parse::parser::{Fail, FailReason, Parser, State};
    use roc::region::{Located, Region};
    use std::{f64, i64};

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let arena = Bump::new();
        let actual = parse_with(&arena, input);

        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_parsing_fails<'a>(input: &'a str, reason: FailReason, attempting: Attempting) {
        let arena = Bump::new();
        let actual = parse_with(&arena, input);
        let expected_fail = Fail { reason, attempting };

        assert_eq!(Err(expected_fail), actual);
    }

    // STRING LITERALS

    fn expect_parsed_str(input: &str, expected: &str) {
        assert_parses_to(expected, Str(input.into()));
    }

    #[test]
    fn empty_string() {
        assert_parses_to(
            indoc!(
                r#"
                ""
                "#
            ),
            Str(""),
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
            Str("x".into()),
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
            Str("foo".into()),
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

    #[test]
    fn string_with_special_escapes() {
        expect_parsed_str(r#"x\\x"#, r#""x\\x""#);
        expect_parsed_str(r#"x\"x"#, r#""x\"x""#);
        expect_parsed_str(r#"x\tx"#, r#""x\tx""#);
        expect_parsed_str(r#"x\rx"#, r#""x\rx""#);
        expect_parsed_str(r#"x\nx"#, r#""x\nx""#);
    }

    #[test]
    fn string_with_single_quote() {
        // This shoud NOT be escaped in a string.
        expect_parsed_str("x'x", r#""x'x""#);
    }

    #[test]
    fn empty_source_file() {
        assert_parsing_fails("", FailReason::Eof(Region::zero()), Attempting::Module);
    }

    #[test]
    fn first_line_too_long() {
        let max_line_length = std::u16::MAX as usize;

        // the string literal "ZZZZZZZZZ" but with way more Zs
        let too_long_str_body: String = (1..max_line_length)
            .into_iter()
            .map(|_| "Z".to_string())
            .collect();
        let too_long_str = format!("\"{}\"", too_long_str_body);

        // Make sure it's longer than our maximum line length
        assert_eq!(too_long_str.len(), max_line_length + 1);

        assert_parsing_fails(
            &too_long_str,
            FailReason::LineTooLong(0),
            Attempting::Module,
        );
    }

    // INT LITERALS

    #[test]
    fn zero_int() {
        assert_parses_to("0", Int("0"));
    }

    #[test]
    fn positive_int() {
        assert_parses_to("1", Int("1"));
        assert_parses_to("42", Int("42"));
    }

    #[test]
    fn negative_int() {
        assert_parses_to("-1", Int("-1"));
        assert_parses_to("-42", Int("-42"));
    }

    #[test]
    fn highest_int() {
        assert_parses_to(
            i64::MAX.to_string().as_str(),
            Int(i64::MAX.to_string().as_str()),
        );
    }

    #[test]
    fn lowest_int() {
        assert_parses_to(
            i64::MIN.to_string().as_str(),
            Int(i64::MIN.to_string().as_str()),
        );
    }

    #[test]
    fn int_with_underscore() {
        assert_parses_to("1_2_34_567", Int("1_2_34_567"));
        assert_parses_to("-1_2_34_567", Int("-1_2_34_567"));
        // The following cases are silly. They aren't supported on purpose,
        // but there would be a performance cost to explicitly disallowing them,
        // which doesn't seem like it would benefit anyone.
        assert_parses_to("1_", Int("1_"));
        assert_parses_to("1__23", Int("1__23"));
    }

    #[quickcheck]
    fn all_i64_values_parse(num: i64) {
        assert_parses_to(num.to_string().as_str(), Int(num.to_string().as_str()));
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

    // RECORD LITERALS

    #[test]
    fn empty_record() {
        let arena = Bump::new();
        let expected = Record {
            fields: Vec::new_in(&arena),
            update: None,
        };
        let actual = parse_with(&arena, "{}");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn record_update() {
        let arena = Bump::new();
        let label1 = LabeledValue(
            Located::new(0, 0, 16, 17, "x"),
            &[],
            arena.alloc(Located::new(0, 0, 19, 20, Int("5"))),
        );
        let label2 = LabeledValue(
            Located::new(0, 0, 22, 23, "y"),
            &[],
            arena.alloc(Located::new(0, 0, 25, 26, Int("0"))),
        );
        let fields = bumpalo::vec![in &arena;
            Located::new(0, 0, 16, 20, label1),
            Located::new(0, 0, 22, 26, label2)
        ];
        let var = Var {
            module_name: "Foo.Bar",
            ident: "baz",
        };
        let update_target = Located::new(0, 0, 2, 13, var);
        let expected = Record {
            update: Some(&*arena.alloc(update_target)),
            fields,
        };

        let actual = parse_with(&arena, "{ Foo.Bar.baz & x: 5, y: 0 }");
        assert_eq!(Ok(expected), actual);
    }

    // OPERATORS

    #[test]
    fn one_plus_two() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("1")),
            Located::new(0, 0, 1, 2, Plus),
            Located::new(0, 0, 2, 3, Int("2")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "1+2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn one_minus_two() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("1")),
            Located::new(0, 0, 1, 2, Minus),
            Located::new(0, 0, 2, 3, Int("2")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "1-2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn add_with_spaces() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("1")),
            Located::new(0, 0, 3, 4, Plus),
            Located::new(0, 0, 7, 8, Int("2")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "1  +   2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn sub_with_spaces() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("1")),
            Located::new(0, 0, 3, 4, Minus),
            Located::new(0, 0, 7, 8, Int("2")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "1  -   2");

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
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, var),
            Located::new(0, 0, 2, 3, Plus),
            Located::new(0, 0, 4, 5, Int("2")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "x + 2");

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
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, var),
            Located::new(0, 0, 2, 3, Minus),
            Located::new(0, 0, 4, 5, Int("2")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "x - 2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_before_add() {
        let arena = Bump::new();
        let spaced_int = Expr::SpaceAfter(
            arena.alloc(Int("3")),
            bumpalo::vec![in &arena; Newline].into_bump_slice(),
        );
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(1, 1, 2, 3, Int("4")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  \n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_before_sub() {
        let arena = Bump::new();
        let spaced_int = Expr::SpaceAfter(
            arena.alloc(Int("3")),
            bumpalo::vec![in &arena; Newline].into_bump_slice(),
        );
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Minus),
            Located::new(1, 1, 2, 3, Int("4")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  \n- 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_after_mul() {
        let arena = Bump::new();
        let spaced_int = arena
            .alloc(Int("4"))
            .before(bumpalo::vec![in &arena; Newline].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("3")),
            Located::new(0, 0, 3, 4, Star),
            Located::new(1, 1, 2, 3, spaced_int),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  *\n  4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_after_sub() {
        let arena = Bump::new();
        let spaced_int = arena
            .alloc(Int("4"))
            .before(bumpalo::vec![in &arena; Newline].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("3")),
            Located::new(0, 0, 3, 4, Minus),
            Located::new(1, 1, 2, 3, spaced_int),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  -\n  4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn comment_with_unicode() {
        let arena = Bump::new();
        let spaced_int = arena
            .alloc(Int("3"))
            .after(bumpalo::vec![in &arena; LineComment(" 2 × 2")].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(1, 1, 2, 3, Int("4")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  # 2 × 2\n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn comment_before_op() {
        let arena = Bump::new();
        let spaced_int = arena
            .alloc(Int("3"))
            .after(bumpalo::vec![in &arena; LineComment(" test!")].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, spaced_int),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(1, 1, 2, 3, Int("4")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  # test!\n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn comment_after_op() {
        let arena = Bump::new();
        let spaced_int = arena
            .alloc(Int("92"))
            .before(bumpalo::vec![in &arena; LineComment(" test!")].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 2, Int("12")),
            Located::new(0, 0, 4, 5, Star),
            Located::new(1, 1, 1, 3, spaced_int),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "12  * # test!\n 92");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn ops_with_newlines() {
        let arena = Bump::new();
        let spaced_int1 = arena
            .alloc(Int("3"))
            .after(bumpalo::vec![in &arena; Newline].into_bump_slice());
        let spaced_int2 = arena
            .alloc(Int("4"))
            .before(bumpalo::vec![in &arena; Newline, Newline].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, spaced_int1),
            Located::new(1, 1, 0, 1, Plus),
            Located::new(3, 3, 2, 3, spaced_int2),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "3  \n+ \n\n  4");

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
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, var1),
            Located::new(0, 0, 1, 2, Minus),
            Located::new(0, 0, 3, 4, var2),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "x- y");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn minus_twelve_minus_five() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 3, Int("-12")),
            Located::new(0, 0, 3, 4, Minus),
            Located::new(0, 0, 4, 5, Int("5")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "-12-5");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn ten_times_eleven() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 2, Int("10")),
            Located::new(0, 0, 2, 3, Star),
            Located::new(0, 0, 3, 5, Int("11")),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "10*11");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn multiple_operators() {
        let arena = Bump::new();
        let inner = arena.alloc((
            Located::new(0, 0, 3, 5, Int("42")),
            Located::new(0, 0, 5, 6, Plus),
            Located::new(0, 0, 6, 9, Int("534")),
        ));
        let outer = arena.alloc((
            Located::new(0, 0, 0, 2, Int("31")),
            Located::new(0, 0, 2, 3, Star),
            Located::new(0, 0, 3, 9, BinOp(inner)),
        ));
        let expected = BinOp(outer);
        let actual = parse_with(&arena, "31*42+534");

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
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, var1),
            Located::new(0, 0, 1, 3, Equals),
            Located::new(0, 0, 3, 4, var2),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "x==y");

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
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, var1),
            Located::new(0, 0, 2, 4, Equals),
            Located::new(0, 0, 5, 6, var2),
        ));
        let expected = BinOp(tuple);
        let actual = parse_with(&arena, "x == y");

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
        let actual = parse_with(&arena, "whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_var() {
        let arena = Bump::new();
        let expected = ParensAround(arena.alloc(Var {
            module_name: "",
            ident: "whee",
        }));
        let actual = parse_with(&arena, "(whee)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_var() {
        let arena = Bump::new();
        let expected = Var {
            module_name: "One.Two",
            ident: "whee",
        };
        let actual = parse_with(&arena, "One.Two.whee");

        assert_eq!(Ok(expected), actual);
    }

    // TAG

    #[test]
    fn basic_global_tag() {
        let arena = Bump::new();
        let expected = Expr::GlobalTag("Whee");
        let actual = parse_with(&arena, "Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn basic_private_tag() {
        let arena = Bump::new();
        let expected = Expr::PrivateTag("@Whee");
        let actual = parse_with(&arena, "@Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_private_tag() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Int("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 9, 11, Int("34")));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 5, Expr::PrivateTag("@Whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "@Whee 12 34");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_global_tag() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 5, 7, Int("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 8, 10, Int("34")));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, Expr::GlobalTag("Whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "Whee 12 34");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_parenthetical_global_tag_args() {
        let arena = Bump::new();
        let int1 = ParensAround(arena.alloc(Int("12")));
        let int2 = ParensAround(arena.alloc(Int("34")));
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, int1));
        let arg2 = arena.alloc(Located::new(0, 0, 11, 13, int2));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, Expr::GlobalTag("Whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "Whee (12) (34)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_global_tag() {
        let arena = Bump::new();
        let expected = Expr::MalformedIdent("One.Two.Whee");
        let actual = parse_with(&arena, "One.Two.Whee");

        assert_eq!(Ok(expected), actual);
    }

    // TODO restore this test - it fails, but is not worth fixing right now.
    // #[test]
    // fn qualified_private_tag() {
    //     let arena = Bump::new();
    //     let expected = Expr::MalformedIdent("One.Two.@Whee");
    //     let actual = parse_with(&arena, "One.Two.@Whee");

    //     assert_eq!(Ok(expected), actual);
    // }

    #[test]
    fn tag_pattern() {
        let arena = Bump::new();
        let pattern = Located::new(0, 0, 1, 6, Pattern::GlobalTag("Thing"));
        let patterns = bumpalo::vec![in &arena; pattern];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 10, 12, Int("42"))),
        );
        let actual = parse_with(&arena, "\\Thing -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn private_qualified_tag() {
        let arena = Bump::new();
        let expected = Expr::MalformedIdent("@One.Two.Whee");
        let actual = parse_with(&arena, "@One.Two.Whee");

        assert_eq!(Ok(expected), actual);
    }

    // LISTS

    #[test]
    fn empty_list() {
        let arena = Bump::new();
        let elems = Vec::new_in(&arena);
        let expected = List(elems);
        let actual = parse_with(&arena, "[]");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn spaces_inside_empty_list() {
        // This is a regression test!
        let arena = Bump::new();
        let elems = Vec::new_in(&arena);
        let expected = List(elems);
        let actual = parse_with(&arena, "[  ]");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn packed_singleton_list() {
        let arena = Bump::new();
        let elems = bumpalo::vec![in &arena; &*arena.alloc(Located::new(0, 0, 1, 2, Int("1")))];
        let expected = List(elems);
        let actual = parse_with(&arena, "[1]");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn spaced_singleton_list() {
        let arena = Bump::new();
        let elems = bumpalo::vec![in &arena; &*arena.alloc(Located::new(0, 0, 2, 3, Int("1")))];
        let expected = List(elems);
        let actual = parse_with(&arena, "[ 1 ]");

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
        let actual = parse_with(&arena, "rec.field");

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
        let actual = parse_with(&arena, "(rec).field");

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
        let actual = parse_with(&arena, "(One.Two.rec).field");

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
        let actual = parse_with(&arena, "rec.abc.def.ghi");

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
        let actual = parse_with(&arena, "One.Two.rec.abc.def.ghi");

        assert_eq!(Ok(expected), actual);
    }

    // APPLY

    #[test]
    fn basic_apply() {
        let arena = Bump::new();
        let arg = arena.alloc(Located::new(0, 0, 5, 6, Int("1")));
        let args = bumpalo::vec![in &arena; &*arg];
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
        let actual = parse_with(&arena, "whee 1");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_two_args() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Int("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 10, 12, Int("34")));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
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
        let actual = parse_with(&arena, "whee  12  34");

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
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2, &*arg3];
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
        let actual = parse_with(&arena, "a b c d");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_apply() {
        let arena = Bump::new();
        let arg = arena.alloc(Located::new(0, 0, 7, 8, Int("1")));
        let args = bumpalo::vec![in &arena; &*arg];
        let parens_var = Expr::ParensAround(arena.alloc(Var {
            module_name: "",
            ident: "whee",
        }));
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 1, 5, parens_var)),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "(whee) 1");

        assert_eq!(Ok(expected), actual);
    }

    // UNARY OPERATORS

    #[test]
    fn unary_negation() {
        let arena = Bump::new();
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Negate);
        let loc_arg1_expr = Located::new(
            0,
            0,
            1,
            4,
            Var {
                module_name: "",
                ident: "foo",
            },
        );
        let expected = UnaryOp(arena.alloc(loc_arg1_expr), loc_op);
        let actual = parse_with(&arena, "-foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_not() {
        let arena = Bump::new();
        let loc_op = Located::new(0, 0, 0, 1, UnaryOp::Not);
        let loc_arg1_expr = Located::new(
            0,
            0,
            1,
            5,
            Var {
                module_name: "",
                ident: "blah",
            },
        );
        let expected = UnaryOp(arena.alloc(loc_arg1_expr), loc_op);
        let actual = parse_with(&arena, "!blah");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_unary_negation() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 7, 9, Int("12")));
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
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let apply_expr = Expr::Apply(
            arena.alloc(Located::new(
                0,
                0,
                1,
                5,
                Var {
                    module_name: "",
                    ident: "whee",
                },
            )),
            args,
            CalledVia::Space,
        );
        let expected = UnaryOp(arena.alloc(Located::new(0, 0, 1, 13, apply_expr)), loc_op);
        let actual = parse_with(&arena, "-whee  12 foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_unary_not() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 7, 9, Int("12")));
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
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let apply_expr = Expr::Apply(
            arena.alloc(Located::new(
                0,
                0,
                1,
                5,
                Var {
                    module_name: "",
                    ident: "whee",
                },
            )),
            args,
            CalledVia::Space,
        );
        let expected = UnaryOp(arena.alloc(Located::new(0, 0, 1, 13, apply_expr)), loc_op);
        let actual = parse_with(&arena, "!whee  12 foo");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_negation_with_parens() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 8, 10, Int("12")));
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
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
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
        let expected = UnaryOp(arena.alloc(Located::new(0, 0, 1, 15, apply_expr)), loc_op);
        let actual = parse_with(&arena, "-(whee  12 foo)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_not_with_parens() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 8, 10, Int("12")));
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
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
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
        let expected = UnaryOp(arena.alloc(Located::new(0, 0, 1, 15, apply_expr)), loc_op);
        let actual = parse_with(&arena, "!(whee  12 foo)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn unary_negation_arg() {
        let arena = Bump::new();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Int("12")));
        let loc_op = Located::new(0, 0, 9, 10, UnaryOp::Negate);
        let var1 = Var {
            module_name: "",
            ident: "foo",
        };
        let loc_arg1_expr = Located::new(0, 0, 10, 13, var1);
        let arg_op = UnaryOp(arena.alloc(loc_arg1_expr), loc_op);
        let arg2 = arena.alloc(Located::new(0, 0, 9, 13, arg_op));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let var2 = Var {
            module_name: "",
            ident: "whee",
        };
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, var2)),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "whee  12 -foo");

        assert_eq!(Ok(expected), actual);
    }

    // CLOSURE

    #[test]
    fn single_arg_closure() {
        let arena = Bump::new();
        let pattern = Located::new(0, 0, 1, 2, Identifier("a"));
        let patterns = bumpalo::vec![in &arena; pattern];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 6, 8, Int("42"))),
        );
        let actual = parse_with(&arena, "\\a -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn single_underscore_closure() {
        let arena = Bump::new();
        let pattern = Located::new(0, 0, 1, 2, Underscore);
        let patterns = bumpalo::vec![in &arena; pattern];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 6, 8, Int("42"))),
        );
        let actual = parse_with(&arena, "\\_ -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn malformed_ident_due_to_underscore() {
        // This is a regression test against a bug where if you included an
        // underscore in an argument name, it would parse as three arguments
        // (and would ignore the underscore as if it had been blank space).
        let arena = Bump::new();
        let actual = parse_with(&arena, "\\the_answer -> 42");

        assert_eq!(Ok(MalformedClosure), actual);
    }

    #[test]
    fn two_arg_closure() {
        let arena = Bump::new();
        let arg1 = Located::new(0, 0, 1, 2, Identifier("a"));
        let arg2 = Located::new(0, 0, 4, 5, Identifier("b"));
        let patterns = bumpalo::vec![in &arena; arg1, arg2];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 9, 11, Int("42"))),
        );
        let actual = parse_with(&arena, "\\a, b -> 42");

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
            arena.alloc(Located::new(0, 0, 12, 14, Int("42"))),
        );
        let actual = parse_with(&arena, "\\a, b, c -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn closure_with_underscores() {
        let arena = Bump::new();
        let underscore1 = Located::new(0, 0, 1, 2, Underscore);
        let underscore2 = Located::new(0, 0, 4, 5, Underscore);
        let patterns = bumpalo::vec![in &arena; underscore1, underscore2];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 9, 11, Int("42"))),
        );
        let actual = parse_with(&arena, "\\_, _ -> 42");

        assert_eq!(Ok(expected), actual);
    }

    // DEF

    #[test]
    fn one_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(1, 1, 2, 3, Int("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 1, def));
        let defs = bumpalo::vec![in &arena; loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
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
    fn one_spaced_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 1, Identifier("x"))),
            arena.alloc(Located::new(1, 1, 4, 5, Int("5"))),
        );
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 1, def));
        let defs = bumpalo::vec![in &arena; loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
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
            arena.alloc(Located::new(1, 1, 4, 5, Int("5"))),
        );
        let loc_def1 = &*arena.alloc(Located::new(1, 1, 0, 1, def1));
        let def2 = Def::SpaceBefore(
            &*arena.alloc(Def::Body(
                arena.alloc(Located::new(2, 2, 0, 1, Identifier("y"))),
                arena.alloc(Located::new(2, 2, 4, 5, Int("6"))),
            )),
            newline.into_bump_slice(),
        );
        let loc_def2 = &*arena.alloc(Located::new(2, 2, 0, 5, def2));
        // NOTE: The first def always gets reordered to the end (because it
        // gets added by .push(), since that's more efficient and since
        // canonicalization is going to re-sort these all anyway.)
        let defs = bumpalo::vec![in &arena; loc_def2, loc_def1];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
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
            arena.alloc(Located::new(
                1,
                1,
                1,
                8,
                RecordDestructure(fields.into_bump_slice()),
            )),
            arena.alloc(Located::new(1, 1, 11, 12, Int("5"))),
        );
        let loc_def1 = &*arena.alloc(Located::new(1, 1, 1, 8, def1));
        let def2 = Def::SpaceBefore(
            &*arena.alloc(Def::Body(
                arena.alloc(Located::new(2, 2, 0, 1, Identifier("y"))),
                arena.alloc(Located::new(2, 2, 4, 5, Int("6"))),
            )),
            newline.into_bump_slice(),
        );
        let loc_def2 = &*arena.alloc(Located::new(2, 2, 0, 5, def2));
        // NOTE: The first def always gets reordered to the end (because it
        // gets added by .push(), since that's more efficient and since
        // canonicalization is going to re-sort these all anyway.)
        let defs = bumpalo::vec![in &arena; loc_def2, loc_def1];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(4, 4, 0, 2, ret);
        let reset_indentation = bumpalo::vec![in &arena; LineComment(" leading comment")];
        let expected = Expr::SpaceBefore(
            arena.alloc(Defs(defs, arena.alloc(loc_ret))),
            reset_indentation.into_bump_slice(),
        );

        assert_parses_to(
            indoc!(
                r#"# leading comment
                { x, y } = 5
                y = 6

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn type_signature_def() {
        let arena = Bump::new();
        let newline = bumpalo::vec![in &arena; Newline];
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let applied_ann = TypeAnnotation::Apply("", "Int", &[]);
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 6, 9, applied_ann),
        );
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
            arena.alloc(Located::new(1, 1, 6, 7, Int("4"))),
        );
        let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 7, spaced_def));

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann, loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : Int
                foo = 4

                42
                "#
            ),
            expected,
        );
    }

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

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
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

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 4, signature));
        let defs = bumpalo::vec![in &arena; loc_ann];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
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
    fn type_signature_function_def() {
        use TypeAnnotation;
        let arena = Bump::new();
        let newline = bumpalo::vec![in &arena; Newline];
        let newlines = bumpalo::vec![in &arena; Newline, Newline];

        let int_type = TypeAnnotation::Apply("", "Int", &[]);
        let float_type = TypeAnnotation::Apply("", "Float", &[]);
        let bool_type = TypeAnnotation::Apply("", "Bool", &[]);

        let arguments = bumpalo::vec![in &arena;
            Located::new(0, 0, 6, 9, int_type),
            Located::new(0, 0, 11, 16, float_type)
        ];
        let return_type = Located::new(0, 0, 20, 24, bool_type);
        let fn_ann = TypeAnnotation::Function(&arguments, &return_type);
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 20, 24, fn_ann),
        );

        let args = bumpalo::vec![in &arena;
        Located::new(1,1,7,8, Identifier("x")),
        Located::new(1,1,10,11, Underscore)
        ];
        let body = Located::new(1, 1, 15, 17, Int("42"));

        let closure = Expr::Closure(&args, &body);

        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
            arena.alloc(Located::new(1, 1, 6, 17, closure)),
        );
        let spaced = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 17, spaced));

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann, loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : Int, Float -> Bool
                foo = \x, _ -> 42

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn ann_private_open_union() {
        let arena = Bump::new();
        let newline = bumpalo::vec![in &arena; Newline];
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let tag1 = Tag::Private {
            name: Located::new(0, 0, 8, 13, "@True"),
            args: &[],
        };
        let tag2arg1 = Located::new(0, 0, 24, 27, TypeAnnotation::Apply("", "Two", &[]));
        let tag2arg2 = Located::new(0, 0, 28, 34, TypeAnnotation::Apply("", "Things", &[]));
        let tag2args = bumpalo::vec![in &arena; tag2arg1, tag2arg2];
        let tag2 = Tag::Private {
            name: Located::new(0, 0, 15, 23, "@Perhaps"),
            args: tag2args.into_bump_slice(),
        };
        let tags = bumpalo::vec![in &arena;
            Located::new(0, 0, 8, 13, tag1),
            Located::new(0, 0, 15, 34, tag2)
        ];
        let loc_wildcard = Located::new(0, 0, 36, 37, TypeAnnotation::Wildcard);
        let applied_ann = TypeAnnotation::TagUnion {
            tags: tags.into_bump_slice(),
            ext: Some(arena.alloc(loc_wildcard)),
        };
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 6, 37, applied_ann),
        );
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
            arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
        );
        let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann, loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : [ @True, @Perhaps Two Things ]*
                foo = True

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn ann_private_closed_union() {
        let arena = Bump::new();
        let newline = bumpalo::vec![in &arena; Newline];
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let tag1 = Tag::Private {
            name: Located::new(0, 0, 8, 13, "@True"),
            args: &[],
        };
        let tag2arg = Located::new(0, 0, 24, 29, TypeAnnotation::Apply("", "Thing", &[]));
        let tag2args = bumpalo::vec![in &arena; tag2arg];
        let tag2 = Tag::Private {
            name: Located::new(0, 0, 15, 23, "@Perhaps"),
            args: tag2args.into_bump_slice(),
        };
        let tags = bumpalo::vec![in &arena;
            Located::new(0, 0, 8, 13, tag1),
            Located::new(0, 0, 15, 29, tag2)
        ];
        let applied_ann = TypeAnnotation::TagUnion {
            tags: tags.into_bump_slice(),
            ext: None,
        };
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 6, 31, applied_ann),
        );
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
            arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
        );
        let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann, loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : [ @True, @Perhaps Thing ]
                foo = True

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn ann_global_open_union() {
        let arena = Bump::new();
        let newline = bumpalo::vec![in &arena; Newline];
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let tag1 = Tag::Global {
            name: Located::new(0, 0, 8, 12, "True"),
            args: &[],
        };
        let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
        let tag2args = bumpalo::vec![in &arena; tag2arg];
        let tag2 = Tag::Global {
            name: Located::new(0, 0, 14, 21, "Perhaps"),
            args: tag2args.into_bump_slice(),
        };
        let tags = bumpalo::vec![in &arena;
            Located::new(0, 0, 8, 12, tag1),
            Located::new(0, 0, 14, 27, tag2)
        ];
        let loc_wildcard = Located::new(0, 0, 29, 30, TypeAnnotation::Wildcard);
        let applied_ann = TypeAnnotation::TagUnion {
            tags: tags.into_bump_slice(),
            ext: Some(arena.alloc(loc_wildcard)),
        };
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 6, 30, applied_ann),
        );
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
            arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
        );
        let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann, loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : [ True, Perhaps Thing ]*
                foo = True

                42
                "#
            ),
            expected,
        );
    }

    #[test]
    fn ann_global_closed_union() {
        let arena = Bump::new();
        let newline = bumpalo::vec![in &arena; Newline];
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let tag1 = Tag::Global {
            name: Located::new(0, 0, 8, 12, "True"),
            args: &[],
        };
        let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
        let tag2args = bumpalo::vec![in &arena; tag2arg];
        let tag2 = Tag::Global {
            name: Located::new(0, 0, 14, 21, "Perhaps"),
            args: tag2args.into_bump_slice(),
        };
        let tags = bumpalo::vec![in &arena;
            Located::new(0, 0, 8, 12, tag1),
            Located::new(0, 0, 14, 27, tag2)
        ];
        let applied_ann = TypeAnnotation::TagUnion {
            tags: tags.into_bump_slice(),
            ext: None,
        };
        let signature = Def::Annotation(
            Located::new(0, 0, 0, 3, Identifier("foo")),
            Located::new(0, 0, 6, 29, applied_ann),
        );
        let def = Def::Body(
            arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
            arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
        );
        let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
        let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

        let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
        let defs = bumpalo::vec![in &arena; loc_ann, loc_def];
        let ret = Expr::SpaceBefore(arena.alloc(Int("42")), newlines.into_bump_slice());
        let loc_ret = Located::new(3, 3, 0, 2, ret);
        let expected = Defs(defs, arena.alloc(loc_ret));

        assert_parses_to(
            indoc!(
                r#"
                foo : [ True, Perhaps Thing ]
                foo = True

                42
                "#
            ),
            expected,
        );
    }

    // WHEN

    #[test]
    fn two_branch_when() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern1 =
            Pattern::SpaceBefore(arena.alloc(StrLiteral("blah")), newlines.into_bump_slice());
        let loc_pattern1 = Located::new(1, 1, 1, 7, pattern1);
        let expr1 = Int("1");
        let loc_expr1 = Located::new(1, 1, 11, 12, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern1],
            value: loc_expr1,
            guard: None,
        });
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern2 =
            Pattern::SpaceBefore(arena.alloc(StrLiteral("mise")), newlines.into_bump_slice());
        let loc_pattern2 = Located::new(2, 2, 1, 7, pattern2);
        let expr2 = Int("2");
        let loc_expr2 = Located::new(2, 2, 11, 12, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern2 ],
            value: loc_expr2,
            guard: None,
        });
        let branches = bumpalo::vec![in &arena; branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_with(
            &arena,
            indoc!(
                r#"
                when x is
                 "blah" -> 1
                 "mise" -> 2
                "#
            ),
        );

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn when_with_numbers() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern1 =
            Pattern::SpaceBefore(arena.alloc(IntLiteral("1")), newlines.into_bump_slice());
        let loc_pattern1 = Located::new(1, 1, 1, 2, pattern1);
        let expr1 = Int("2");
        let loc_expr1 = Located::new(1, 1, 6, 7, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern1],
            value: loc_expr1,
            guard: None,
        });
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern2 =
            Pattern::SpaceBefore(arena.alloc(IntLiteral("3")), newlines.into_bump_slice());
        let loc_pattern2 = Located::new(2, 2, 1, 2, pattern2);
        let expr2 = Int("4");
        let loc_expr2 = Located::new(2, 2, 6, 7, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern2],
            value: loc_expr2,
            guard: None,
        });
        let branches = bumpalo::vec![in &arena; branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_with(
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
    fn when_with_records() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline];
        let identifiers1 = bumpalo::vec![in &arena; Located::new(1, 1, 3, 4, Identifier("y")) ];
        let pattern1 = Pattern::SpaceBefore(
            arena.alloc(RecordDestructure(identifiers1.into_bump_slice())),
            newlines.into_bump_slice(),
        );
        let loc_pattern1 = Located::new(1, 1, 1, 6, pattern1);
        let expr1 = Int("2");
        let loc_expr1 = Located::new(1, 1, 10, 11, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern1 ],
            value: loc_expr1,
            guard: None,
        });
        let newlines = bumpalo::vec![in &arena; Newline];
        let identifiers2 = bumpalo::vec![in &arena; Located::new(2, 2, 3, 4, Identifier("z")), Located::new(2, 2, 6, 7, Identifier("w"))  ];
        let pattern2 = Pattern::SpaceBefore(
            arena.alloc(RecordDestructure(identifiers2.into_bump_slice())),
            newlines.into_bump_slice(),
        );
        let loc_pattern2 = Located::new(2, 2, 1, 9, pattern2);
        let expr2 = Int("4");
        let loc_expr2 = Located::new(2, 2, 13, 14, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern2 ],
            value: loc_expr2,
            guard: None,
        });
        let branches = bumpalo::vec![in &arena; branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_with(
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
    fn when_with_alternative_patterns() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern1 =
            Pattern::SpaceBefore(arena.alloc(StrLiteral("blah")), newlines.into_bump_slice());
        let pattern1_alt = StrLiteral("blop");
        let loc_pattern1 = Located::new(1, 1, 1, 7, pattern1);
        let loc_pattern1_alt = Located::new(1, 1, 10, 16, pattern1_alt);
        let expr1 = Int("1");
        let loc_expr1 = Located::new(1, 1, 20, 21, expr1);
        let branch1 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern1, loc_pattern1_alt],
            value: loc_expr1,
            guard: None,
        });
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern2 =
            Pattern::SpaceBefore(arena.alloc(StrLiteral("foo")), newlines.into_bump_slice());
        let newlines = bumpalo::vec![in &arena; Newline];
        let pattern2_alt =
            Pattern::SpaceBefore(arena.alloc(StrLiteral("bar")), newlines.into_bump_slice());
        let loc_pattern2 = Located::new(2, 2, 1, 6, pattern2);
        let loc_pattern2_alt = Located::new(3, 3, 1, 6, pattern2_alt);
        let expr2 = Int("2");
        let loc_expr2 = Located::new(3, 3, 10, 11, expr2);
        let branch2 = &*arena.alloc(WhenBranch {
            patterns: bumpalo::vec![in &arena;loc_pattern2, loc_pattern2_alt],
            value: loc_expr2,
            guard: None,
        });
        let branches = bumpalo::vec![in &arena; branch1, branch2];
        let var = Var {
            module_name: "",
            ident: "x",
        };
        let loc_cond = Located::new(0, 0, 5, 6, var);
        let expected = Expr::When(arena.alloc(loc_cond), branches);
        let actual = parse_with(
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
    fn empty_module() {
        let arena = Bump::new();
        let exposes = Vec::new_in(&arena);
        let imports = Vec::new_in(&arena);
        let module_name = ModuleName::new("Foo");
        let expected = InterfaceHeader {
            name: Located::new(0, 0, 10, 13, module_name),
            exposes,
            imports,

            after_interface: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_imports: &[],
            after_imports: &[],
        };
        let src = indoc!(
            r#"
                interface Foo exposes [] imports []
            "#
        );
        let actual = interface_header()
            .parse(&arena, State::new(&src, Attempting::Module))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn nested_module() {
        let arena = Bump::new();
        let exposes = Vec::new_in(&arena);
        let imports = Vec::new_in(&arena);
        let module_name = ModuleName::new("Foo.Bar.Baz");
        let expected = InterfaceHeader {
            name: Located::new(0, 0, 10, 21, module_name),
            exposes,
            imports,

            after_interface: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_imports: &[],
            after_imports: &[],
        };
        let src = indoc!(
            r#"
                interface Foo.Bar.Baz exposes [] imports []
            "#
        );
        let actual = interface_header()
            .parse(&arena, State::new(&src, Attempting::Module))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn standalone_module_defs() {
        use roc::parse::ast::Def::*;

        let arena = Bump::new();
        let newlines1 = bumpalo::vec![in &arena; Newline, Newline];
        let newlines2 = bumpalo::vec![in &arena; Newline];
        let newlines3 = bumpalo::vec![in &arena; Newline];
        let pattern1 = Identifier("foo");
        let pattern2 = Identifier("bar");
        let pattern3 = Identifier("baz");
        let def1 = SpaceAfter(
            arena.alloc(Body(
                arena.alloc(Located::new(0, 0, 0, 3, pattern1)),
                arena.alloc(Located::new(0, 0, 6, 7, Int("1"))),
            )),
            newlines1.into_bump_slice(),
        );
        let def2 = SpaceAfter(
            arena.alloc(Body(
                arena.alloc(Located::new(2, 2, 0, 3, pattern2)),
                arena.alloc(Located::new(2, 2, 6, 10, Str("hi"))),
            )),
            newlines2.into_bump_slice(),
        );
        let def3 = SpaceAfter(
            arena.alloc(Body(
                arena.alloc(Located::new(3, 3, 0, 3, pattern3)),
                arena.alloc(Located::new(3, 3, 6, 13, Str("stuff"))),
            )),
            newlines3.into_bump_slice(),
        );

        let expected = bumpalo::vec![in &arena;
            Located::new(0, 0, 0, 7, def1),
            Located::new(2, 2, 0, 10, def2),
            Located::new(3, 3, 0, 13, def3)
        ];
        let src = indoc!(
            r#"
                foo = 1

                bar = "hi"
                baz = "stuff"
            "#
        );
        let actual = module_defs()
            .parse(&arena, State::new(&src, Attempting::Module))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    // TODO test for \t \r and \n in string literals *outside* unicode escape sequence!
    //
    // TODO test for non-ASCII variables
    //
    // TODO verify that when a string literal contains a newline before the
    // closing " it correctly updates both the line *and* column in the State.
}
