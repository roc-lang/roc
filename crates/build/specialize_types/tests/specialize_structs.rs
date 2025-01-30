#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_structs {
    use roc_specialize_types::MonoExpr;

    use crate::helpers::expect_mono_expr_str;

    use super::helpers::{expect_mono_expr_with_interns, expect_unit};

    #[test]
    fn empty_record() {
        expect_unit("{}");
    }

    #[test]
    fn one_field_with_empty_record() {
        expect_unit("{ discardedField: {} }");
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
    fn two_fields() {
        let one = 42;
        let two = 50;

        expect_mono_expr_str(
            format!("{{ one: {one}, two: {two} }}"),
            format!("Struct([Number(I8({one})), Number(I8({two}))])"),
        );
    }

    #[test]
    fn two_fields_one_unit() {
        let one = 42;
        let two = "{}";

        expect_mono_expr_str(
            format!("{{ one: {one}, two: {two} }}"),
            format!("Struct([Number(I8({one})), {{}}])"),
        );
    }
}
