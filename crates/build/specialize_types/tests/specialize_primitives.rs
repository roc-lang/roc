#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_primitives {
    use roc_module::symbol::Symbol;
    use roc_specialize_types::{MonoExpr, MonoTypeId, Number};

    use super::helpers::{expect_mono_expr, expect_mono_expr_with_interns};

    #[test]
    fn string_literal() {
        let string = "foo";
        let expected = format!("\"{string}\"");
        expect_mono_expr_with_interns(expected, |arena, interns| {
            MonoExpr::Str(interns.try_get_id(arena, string).unwrap())
        });
    }

    #[test]
    fn unbound_zero() {
        let expected = 0;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I8(expected)),
        );
    }

    #[test]
    fn unbound_negative_i8() {
        let expected = -42;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I8(expected)),
        );
    }

    #[test]
    fn unbound_positive_i8() {
        let expected = 42;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I8(expected)),
        );
    }

    #[test]
    fn unbound_u8() {
        let expected = 128;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::U8(expected)),
        );
    }

    #[test]
    fn unbound_negative_i16() {
        let expected = -5_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I16(expected)),
        );
    }

    #[test]
    fn unbound_positive_i16() {
        let expected = 5_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I16(expected)),
        );
    }

    #[test]
    fn unbound_u16() {
        let expected = 65_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::U16(expected)),
        );
    }
    #[test]
    fn unbound_negative_i32() {
        let expected = -2_000_000_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I32(expected)),
        );
    }

    #[test]
    fn unbound_positive_i32() {
        let expected = 2_000_000_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I32(expected)),
        );
    }

    #[test]
    fn unbound_u32() {
        let expected = 4_000_000_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::U32(expected)),
        );
    }

    #[test]
    fn unbound_negative_i64() {
        let expected = -9_000_000_000_000_000_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I64(expected)),
        );
    }

    #[test]
    fn unbound_positive_i64() {
        let expected = 9_000_000_000_000_000_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I64(expected)),
        );
    }

    #[test]
    fn unbound_u64() {
        let expected = 18_000_000_000_000_000_000;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::U64(expected)),
        );
    }

    #[test]
    fn unbound_negative_i128() {
        let expected = -170_141_183_460_469_231_731_687_303_715_884_105_728;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I128(expected)),
        );
    }

    #[test]
    fn unbound_positive_i128() {
        let expected = 170_141_183_460_469_231_731_687_303_715_884_105_727;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I128(expected)),
        );
    }

    #[test]
    fn unbound_u128() {
        let expected = 340_282_366_920_938_463_463_374_607_431_768_211_455;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::U128(expected)),
        );
    }

    #[test]
    fn unbound_f64() {
        let expected = std::f64::consts::PI;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::Dec(expected)),
        );
    }

    #[test]
    fn bool_true() {
        let expected = "Bool.true";
        expect_mono_expr(
            expected,
            MonoExpr::Lookup(Symbol::BOOL_TRUE, MonoTypeId::BOOL),
        );
    }

    #[test]
    fn bool_false() {
        let expected = "Bool.false";
        expect_mono_expr(
            expected,
            MonoExpr::Lookup(Symbol::BOOL_FALSE, MonoTypeId::BOOL),
        );
    }
}
