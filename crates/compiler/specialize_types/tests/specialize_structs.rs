#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;

mod helpers;

#[cfg(test)]
mod specialize_structs {
    use roc_specialize_types::{MonoExpr, Number};

    use crate::helpers::expect_mono_expr_str;

    use super::helpers::{expect_mono_expr, expect_mono_expr_with_interns, expect_no_expr};

    #[test]
    fn empty_record() {
        expect_no_expr("{}");
    }

    #[test]
    fn one_field_with_empty_record() {
        expect_no_expr("{ discardedField: {} }");
    }

    #[test]
    fn one_field_record_string_literal() {
        let string = "foo";
        let expected = format!("{{ discardedField: \"{string}\" }}");
        expect_mono_expr_with_interns(expected, |arena, interns| {
            MonoExpr::Str(interns.try_get_id(arena, string).unwrap())
        });
    }

    #[test]
    fn one_field_after_dropping_zero_sized() {
        let string = "foo";
        let expected =
            format!("{{ discarded: {{}}, discardedToo: \"{string}\", alsoDiscarded: {{}} }}");
        expect_mono_expr_with_interns(expected, |arena, interns| {
            MonoExpr::Str(interns.try_get_id(arena, string).unwrap())
        });
    }

    #[test]
    fn two_fields() {
        let one = 42;
        let two = 50;
        let expected = format!("{{ one: {one}, two: {two} }}");
        expect_mono_expr_str(
            expected,
            format!("Struct([Number(I8({one:?})), Number(I8({two:?}))])"),
        );
    }
}
