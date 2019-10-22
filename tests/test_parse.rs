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
    use bumpalo::collections::vec::Vec;
    use bumpalo::{self, Bump};
    use helpers::parse_with;
    use roc::operator::CalledVia;
    use roc::operator::Operator::*;
    use roc::parse::ast::CommentOrNewline::*;
    use roc::parse::ast::Expr::{self, *};
    use roc::parse::ast::Pattern::*;
    use roc::parse::ast::{Attempting, Def, Spaceable};
    use roc::parse::parser::{Fail, FailReason};
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
        let expected = Record(Vec::new_in(&arena));
        let actual = parse_with(&arena, "{}");

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
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "1+2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn ops_with_spaces() {
        let arena = Bump::new();
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("1")),
            Located::new(0, 0, 3, 4, Plus),
            Located::new(0, 0, 7, 8, Int("2")),
        ));
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "1  +   2");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_before_op() {
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
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "3  \n+ 4");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn newline_after_op() {
        let arena = Bump::new();
        let spaced_int = arena
            .alloc(Int("4"))
            .before(bumpalo::vec![in &arena; Newline].into_bump_slice());
        let tuple = arena.alloc((
            Located::new(0, 0, 0, 1, Int("3")),
            Located::new(0, 0, 3, 4, Star),
            Located::new(1, 1, 2, 3, spaced_int),
        ));
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "3  *\n  4");

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
        let expected = Operator(tuple);
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
        let expected = Operator(tuple);
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
        let expected = Operator(tuple);
        let actual = parse_with(&arena, "3  \n+ \n\n  4");

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
        let expected = Operator(tuple);
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
        let expected = Operator(tuple);
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
            Located::new(0, 0, 3, 9, Operator(inner)),
        ));
        let expected = Operator(outer);
        let actual = parse_with(&arena, "31*42+534");

        assert_eq!(Ok(expected), actual);
    }

    // VAR

    #[test]
    fn basic_var() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let expected = Var(module_parts, "whee");
        let actual = parse_with(&arena, "whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_var() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let expected = Var(module_parts, "whee");
        let actual = parse_with(&arena, "(whee)");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_var() {
        let arena = Bump::new();
        let module_parts = bumpalo::vec![in &arena; "One", "Two"].into_bump_slice();
        let expected = Var(module_parts, "whee");
        let actual = parse_with(&arena, "One.Two.whee");

        assert_eq!(Ok(expected), actual);
    }

    // VARIANT

    #[test]
    fn basic_variant() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let expected = Expr::Variant(module_parts, "Whee");
        let actual = parse_with(&arena, "Whee");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_variant() {
        let arena = Bump::new();
        let module_parts = bumpalo::vec![in &arena; "One", "Two"].into_bump_slice();
        let expected = Expr::Variant(module_parts, "Whee");
        let actual = parse_with(&arena, "One.Two.Whee");

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
    fn parenthetical_basic_field() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let fields = bumpalo::vec![in &arena; "field"];
        let expected = Field(
            arena.alloc(Located::new(0, 0, 1, 4, Var(module_parts, "rec"))),
            fields,
        );
        let actual = parse_with(&arena, "(rec).field");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_field_qualified_var() {
        let arena = Bump::new();
        let module_parts = bumpalo::vec![in &arena; "One", "Two"].into_bump_slice();
        let fields = bumpalo::vec![in &arena; "field"];
        let expected = Field(
            arena.alloc(Located::new(0, 0, 1, 12, Var(module_parts, "rec"))),
            fields,
        );
        let actual = parse_with(&arena, "(One.Two.rec).field");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn basic_field() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let fields = bumpalo::vec![in &arena; "rec", "field"].into_bump_slice();
        let expected = QualifiedField(module_parts, fields);
        let actual = parse_with(&arena, "rec.field");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn qualified_field() {
        let arena = Bump::new();
        let module_parts = bumpalo::vec![in &arena; "One", "Two"].into_bump_slice();
        let fields = bumpalo::vec![in &arena; "rec", "field"].into_bump_slice();
        let expected = QualifiedField(module_parts, fields);
        let actual = parse_with(&arena, "One.Two.rec.field");

        assert_eq!(Ok(expected), actual);
    }

    // APPLY

    #[test]
    fn basic_apply() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let arg = arena.alloc(Located::new(0, 0, 5, 6, Int("1")));
        let args = bumpalo::vec![in &arena; &*arg];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, Var(module_parts, "whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "whee 1");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_two_args() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let arg1 = arena.alloc(Located::new(0, 0, 6, 8, Int("12")));
        let arg2 = arena.alloc(Located::new(0, 0, 10, 12, Int("34")));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 4, Var(module_parts, "whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "whee  12  34");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn apply_three_args() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let arg1 = arena.alloc(Located::new(0, 0, 2, 3, Var(module_parts, "b")));
        let arg2 = arena.alloc(Located::new(0, 0, 4, 5, Var(module_parts, "c")));
        let arg3 = arena.alloc(Located::new(0, 0, 6, 7, Var(module_parts, "d")));
        let args = bumpalo::vec![in &arena; &*arg1, &*arg2, &*arg3];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 0, 1, Var(module_parts, "a"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "a b c d");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn parenthetical_apply() {
        let arena = Bump::new();
        let module_parts = Vec::new_in(&arena).into_bump_slice();
        let arg = arena.alloc(Located::new(0, 0, 7, 8, Int("1")));
        let args = bumpalo::vec![in &arena; &*arg];
        let expected = Expr::Apply(
            arena.alloc(Located::new(0, 0, 1, 5, Var(module_parts, "whee"))),
            args,
            CalledVia::Space,
        );
        let actual = parse_with(&arena, "(whee) 1");

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
    fn two_arg_closure() {
        let arena = Bump::new();
        let arg1 = Located::new(0, 0, 1, 2, Identifier("a"));
        let arg2 = Located::new(0, 0, 3, 4, Identifier("b"));
        let patterns = bumpalo::vec![in &arena; arg1, arg2];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 8, 10, Int("42"))),
        );
        let actual = parse_with(&arena, "\\a b -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn three_arg_closure() {
        let arena = Bump::new();
        let arg1 = Located::new(0, 0, 1, 2, Identifier("a"));
        let arg2 = Located::new(0, 0, 3, 4, Identifier("b"));
        let arg3 = Located::new(0, 0, 5, 6, Identifier("c"));
        let patterns = bumpalo::vec![in &arena; arg1, arg2, arg3];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 10, 12, Int("42"))),
        );
        let actual = parse_with(&arena, "\\a b c -> 42");

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn closure_with_underscores() {
        let arena = Bump::new();
        let underscore1 = Located::new(0, 0, 1, 2, Underscore);
        let underscore2 = Located::new(0, 0, 3, 4, Underscore);
        let patterns = bumpalo::vec![in &arena; underscore1, underscore2];
        let expected = Closure(
            arena.alloc(patterns),
            arena.alloc(Located::new(0, 0, 8, 10, Int("42"))),
        );
        let actual = parse_with(&arena, "\\_ _ -> 42");

        assert_eq!(Ok(expected), actual);
    }

    // DEF

    #[test]
    fn one_def() {
        let arena = Bump::new();
        let newlines = bumpalo::vec![in &arena; Newline, Newline];
        let def = Def::BodyOnly(
            Located::new(1, 1, 0, 1, Identifier("x")),
            arena.alloc(Located::new(1, 1, 2, 3, Int("5"))),
        );
        let defs = bumpalo::vec![in &arena; (Vec::new_in(&arena).into_bump_slice(), def)];
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
        let def = Def::BodyOnly(
            Located::new(1, 1, 0, 1, Identifier("x")),
            arena.alloc(Located::new(1, 1, 4, 5, Int("5"))),
        );
        let defs = bumpalo::vec![in &arena; (Vec::new_in(&arena).into_bump_slice(), def)];
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
        let def1 = Def::BodyOnly(
            Located::new(1, 1, 0, 1, Identifier("x")),
            arena.alloc(Located::new(1, 1, 4, 5, Int("5"))),
        );
        let def2 = Def::BodyOnly(
            Located::new(2, 2, 0, 1, Identifier("y")),
            arena.alloc(Located::new(2, 2, 4, 5, Int("6"))),
        );
        // NOTE: The first def always gets reordered to the end (because it
        // gets added by .push(), since that's more efficient and since
        // canonicalization is going to re-sort these all anyway.)
        let defs = bumpalo::vec![in &arena;
            (newline.into_bump_slice(), def2),
            (Vec::new_in(&arena).into_bump_slice(), def1)
        ];
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
            Located::new(1, 1, 5, 6, Identifier("y"))
        ];
        let def1 = Def::BodyOnly(
            Located::new(1, 1, 0, 8, RecordDestructure(fields)),
            arena.alloc(Located::new(1, 1, 11, 12, Int("5"))),
        );
        let def2 = Def::BodyOnly(
            Located::new(2, 2, 0, 1, Identifier("y")),
            arena.alloc(Located::new(2, 2, 4, 5, Int("6"))),
        );
        // NOTE: The first def always gets reordered to the end (because it
        // gets added by .push(), since that's more efficient and since
        // canonicalization is going to re-sort these all anyway.)
        let defs = bumpalo::vec![in &arena;
            (newline.into_bump_slice(), def2),
            (Vec::new_in(&arena).into_bump_slice(), def1)
        ];
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

    // CASE

    // #[test]
    // fn two_branch_case() {
    //     let arena = Bump::new();
    //     let module_parts = Vec::new_in(&arena).into_bump_slice();
    //     let arg1 = Located::new(0, 0, 2, 3, Var(module_parts, "b"));
    //     let arg2 = Located::new(0, 0, 4, 5, Var(module_parts, "c"));
    //     let arg3 = Located::new(0, 0, 6, 7, Var(module_parts, "d"));
    //     let args = bumpalo::vec![in &arena; arg1, arg2, arg3];
    //     let tuple = arena.alloc((Located::new(0, 0, 0, 1, Var(module_parts, "a")), args));
    //     let expected = Expr::Apply(tuple);
    //     let actual = parse_with(
    //         &arena,
    //         indoc!(
    //             r#"
    //             case foo bar baz when
    //              "blah" -> foo a b
    //              "mise" -> bar c d
    //             "#
    //         ),
    //     );

    //     assert_eq!(Ok(expected), actual);
    // }

    // TODO test hex/oct/binary parsing
    //
    // TODO test for \t \r and \n in string literals *outside* unicode escape sequence!
    //
    // TODO test for non-ASCII variables
    //
    // TODO verify that when a string literal contains a newline before the
    // closing " it correctly updates both the line *and* column in the State.
}
