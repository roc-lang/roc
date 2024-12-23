#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_when {
    use crate::helpers::expect_mono_expr_str;

    #[test]
    fn single_branch() {
        expect_mono_expr_str(
            r"
                when 123 is
                    num -> num
            ",
            "When(Number(I8(123)), Identifier(`#UserApp.IdentId(1)`) -> `#UserApp.IdentId(1)`)",
        );
    }

    #[test]
    fn number_pattern() {
        expect_mono_expr_str(
            r"
                when 123 is
                    123 -> 321
                    num -> num
            ",
            "When(Number(I16(123)), Number(I16(123)) -> Number(I16(321)), Identifier(`#UserApp.IdentId(1)`) -> `#UserApp.IdentId(1)`)",
        );
    }

    #[test]
    fn underscore_pattern() {
        expect_mono_expr_str(
            r"
                when 123 is
                    123 -> 321
                    _ -> 0
            ",
            "When(Number(I8(123)), Number(I8(123)) -> Number(I16(321)), Underscore -> Number(I16(0)))"
        );
    }

    #[test]
    fn multiple_patterns_per_branch() {
        expect_mono_expr_str(
            r"
                when 123 is
                    123 | 321 -> 321
                    _ -> 0
            ",
            "When(Number(I16(123)), Number(I16(123)) | Number(I16(321)) -> Number(I16(321)), Underscore -> Number(I16(0)))"
        );
    }

    #[test]
    fn guard() {
        expect_mono_expr_str(
            r"
                when 123 is
                    123 if Bool.true -> 321
                    _ -> 0
            ",
            "When(Number(I8(123)), Number(I8(123)) if `Bool.true` -> Number(I16(321)), Underscore -> Number(I16(0)))"
        );
    }
}
