#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;

mod helpers;

#[cfg(test)]
mod specialize_structs {
    use roc_specialize_types::{MonoExpr, Number};

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
        expect_mono_expr_with_interns(
            |arena, interns| interns.try_get(arena, string).unwrap(),
            expected,
            |id| MonoExpr::Str(id),
        );
    }

    #[test]
    fn one_field_after_dropping_zero_sized() {
        let string = "foo";
        let expected =
            format!("{{ discarded: {{}}, discardedToo: \"{string}\", alsoDiscarded: {{}} }}");
        expect_mono_expr_with_interns(
            |arena, interns| interns.try_get(arena, string).unwrap(),
            expected,
            |id| MonoExpr::Str(id),
        );
    }
}
