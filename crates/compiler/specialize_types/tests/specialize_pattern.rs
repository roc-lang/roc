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
}
