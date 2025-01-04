#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_pattern {
    use crate::helpers::expect_mono_pattern_str;

    #[test]
    fn identifier() {
        expect_mono_pattern_str(r"num", "Identifier(`#UserApp.IdentId(1)`)");
    }

    #[test]
    fn num_literal_i8() {
        expect_mono_pattern_str(r"42", "Number(I8(42))");
    }

    #[test]
    fn num_literal_i16() {
        expect_mono_pattern_str(r"256", "Number(I16(256))");
    }

    #[test]
    fn dec_literal() {
        expect_mono_pattern_str(r"120.4", "Number(Dec(120.4))");
    }

    #[test]
    fn as_pattern() {
        expect_mono_pattern_str(r"42 as the_answer", "As(Number(I8(42)), #UserApp.1)");
    }

    #[test]
    fn str_literal() {
        expect_mono_pattern_str(r#""hello""#, "StrLiteral(\"hello\")");
    }

    #[test]
    fn char_literal_ascii() {
        expect_mono_pattern_str(r"'a'", "Number(U8(97))");
    }

    #[test]
    fn char_literal_unicode() {
        expect_mono_pattern_str(r"'🎸'", "Number(U32(127928))");
    }
}
