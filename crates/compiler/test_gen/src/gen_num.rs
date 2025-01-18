#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

// use crate::assert_wasm_evals_to as assert_evals_to;
#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocDec, RocOrder, RocResult, I128, U128};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u64_alias() {
    assert_evals_to!(
        indoc!(
            r"
            i : Num.U64
            i = 1

            i
            "
        ),
        1,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn i128_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
            i : I128
            i = 128

            i
            "
        ),
        I128::from(128),
        I128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i64_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main =
                    i : I64
                    i = 64

                    i
                "#
        ),
        64,
        i64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i32_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
            i : I32
            i = 32

            i
            "
        ),
        32,
        i32
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i16_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : I16
                    i = 16

                    i
                "
        ),
        16,
        i16
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i8_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : I8
                    i = 8

                    i
                "
        ),
        8,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i128_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : I128
                    f = 0x123

                    f
                "
        ),
        I128::from(0x123),
        I128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i64_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : I64
                    f = 0x123

                    f
                "
        ),
        0x123,
        i64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i32_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : I32
                    f = 0x123

                    f
                "
        ),
        0x123,
        i32
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i16_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : I16
                    f = 0x123

                    f
                "
        ),
        0x123,
        i16
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i8_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : I8
                    f = 0xA

                    f
                "
        ),
        0xA,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u128_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : U128
                    i = 128

                    i
                "
        ),
        U128::from(128),
        U128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u64_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : U64
                    i = 64

                    i
                "
        ),
        64,
        u64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u32_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : U32
                    i = 32

                    i
                "
        ),
        32,
        u32
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u16_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : U16
                    i = 16

                    i
                "
        ),
        16,
        u16
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u8_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    i : U8
                    i = 8

                    i
                "
        ),
        8,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u128_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : U128
                    f = 0x123

                    f
                "
        ),
        I128::from(0x123),
        I128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u64_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : U64
                    f = 0x123

                    f
                "
        ),
        0x123,
        u64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u32_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : U32
                    f = 0x123

                    f
                "
        ),
        0x123,
        u32
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u16_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : U16
                    f = 0x123

                    f
                "
        ),
        0x123,
        u16
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u8_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : U8
                    f = 0xA

                    f
                "
        ),
        0xA,
        u8
    );
}

#[test]
fn character_literal() {
    assert_evals_to!(
        indoc!(
            r"
                    x = 'A'

                    x
                "
        ),
        65,
        i64
    );
}

#[test]
fn character_literal_back_slash() {
    assert_evals_to!(
        indoc!(
            r"
                    x = '\\'

                    x
                "
        ),
        92,
        i64
    );
}

#[test]
fn character_literal_single_quote() {
    assert_evals_to!(
        indoc!(
            r"
                    x = '\''

                    x
                "
        ),
        39,
        i64
    );
}

#[test]
fn character_literal_new_line() {
    assert_evals_to!(
        indoc!(
            r"
                    x = '\n'

                    x
                "
        ),
        10,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn dec_float_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 2.1

                    x
                "
        ),
        RocDec::from_str("2.1").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_float_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : F64
                    f = 3.6

                    f
                "
        ),
        3.6,
        f64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f32_float_alias() {
    assert_evals_to!(
        indoc!(
            r"
                    f : F32
                    f = 3.6

                    f
                "
        ),
        3.6,
        f32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_sqrt_100() {
    assert_evals_to!("Num.sqrt 100f64", 10.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_sqrt_checked_0() {
    assert_evals_to!("Num.sqrt 0f64", 0.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_sqrt_checked_positive() {
    assert_evals_to!("Num.sqrt_checked 100f64", RocResult::ok(10.0), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_sqrt_checked_negative() {
    assert_evals_to!("Num.sqrt_checked -1f64", RocResult::err(()), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_log() {
    assert_evals_to!("Num.log 7.38905609893f64", 1.999999999999912, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_log_checked_one() {
    assert_evals_to!("Num.log_checked 1f64", RocResult::ok(0.0), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_log_checked_zero() {
    assert_evals_to!("Num.log_checked 0f64", RocResult::err(()), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_log_negative() {
    assert_evals_to!("Num.log -1f64", true, f64, |f: f64| f.is_nan());
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_round() {
    assert_evals_to!("Num.round 3.6f64", 4, i64);
    assert_evals_to!("Num.round 3.4f64", 3, i64);
    assert_evals_to!("Num.round 2.5f64", 3, i64);
    assert_evals_to!("Num.round -2.3f64", -2, i64);
    assert_evals_to!("Num.round -2.5f64", -3, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_abs() {
    assert_evals_to!("Num.abs -4.7f64", 4.7, f64);
    assert_evals_to!("Num.abs 5.8f64", 5.8, f64);

    assert_evals_to!("Num.abs Num.max_f64", f64::MAX, f64);
    assert_evals_to!("Num.abs Num.min_f64", f64::MAX, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f32_abs() {
    assert_evals_to!("Num.abs -4.7f32", 4.7, f32);
    assert_evals_to!("Num.abs 5.8f32", 5.8, f32);

    assert_evals_to!("Num.abs Num.max_f32", f32::MAX, f32);
    assert_evals_to!("Num.abs Num.min_f32", f32::MAX, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn i64_abs() {
    assert_evals_to!("Num.abs -6", 6, i64);
    assert_evals_to!("Num.abs 7", 7, i64);
    assert_evals_to!("Num.abs 0", 0, i64);
    assert_evals_to!("Num.abs -0", 0, i64);
    assert_evals_to!("Num.abs -1", 1, i64);
    assert_evals_to!("Num.abs 1", 1, i64);
    assert_evals_to!("Num.abs 9_000_000_000_000", 9_000_000_000_000, i64);
    assert_evals_to!("Num.abs -9_000_000_000_000", 9_000_000_000_000, i64);
    assert_evals_to!("Num.abs Num.max_i64", i64::MAX, i64);
    assert_evals_to!("Num.abs (Num.min_i64 + 1)", -(i64::MIN + 1), i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn various_sized_abs() {
    assert_evals_to!("Num.abs -6i8", 6, i8);
    assert_evals_to!("Num.abs -6i16", 6, i16);
    assert_evals_to!("Num.abs -6i32", 6, i32);
    assert_evals_to!("Num.abs -6i64", 6, i64);
    if !cfg!(feature = "gen-wasm") {
        assert_evals_to!("Num.abs -6i128", I128::from(6), I128);
    }
    assert_evals_to!("Num.abs 6u8", 6, u8);
    assert_evals_to!("Num.abs 6u16", 6, u16);
    assert_evals_to!("Num.abs 6u32", 6, u32);
    assert_evals_to!("Num.abs 6u64", 6, u64);
    if !cfg!(feature = "gen-wasm") {
        assert_evals_to!("Num.abs 6u128", U128::from(6), U128);
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(
    expected = r#"Roc failed with message: "Integer absolute overflowed because its argument is the minimum value"#
)]
fn abs_min_int_overflow() {
    assert_evals_to!(
        indoc!(
            r"
                Num.abs Num.min_i64
                "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_if_fn() {
    assert_evals_to!(
        indoc!(
            r"
                    limited_negate = \num ->
                        x =
                            if num == 1 then
                                -1
                            else if num == -1 then
                                1
                            else
                                num
                        x

                    limited_negate 1
                "
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_float_eq() {
    assert_evals_to!(
        indoc!(
            r"
                    1.0 == 1.0
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_add_dec() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 2.1

                    y : Dec
                    y = 3.1

                    z : Dec
                    z = x + y

                    z
                "
        ),
        RocDec::from_str("5.2").unwrap(),
        RocDec
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_f32() {
    assert_evals_to!("1.1f32 + 2.4f32 + 3", 6.5, f32);
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_f64() {
    assert_evals_to!("1.1f64 + 2.4 + 3", 6.5, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_wrap_add_nums() {
    assert_evals_to!(
        indoc!(
            r"
                    add2 = \num1, num2 -> num1 + num2

                    add2 4 5
                "
        ),
        9,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_f64() {
    assert_evals_to!("48f64 / 2", 24.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_f32() {
    assert_evals_to!("48f32 / 2", 24.0, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_checked_f64() {
    assert_evals_to!(
        indoc!(
            r"
                    when Num.div_checked 48 2f64 is
                        Ok val -> val
                        Err _ -> -1
                "
        ),
        24.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_checked_by_zero_f64() {
    assert_evals_to!(
        indoc!(
            r"
                    when Num.div_checked 47 0f64 is
                        Ok val -> val
                        Err _ -> -1
                "
        ),
        -1.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_dec() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 10

                    y : Dec
                    y = 3

                    x / y
                "
        ),
        RocDec::from_str("3.333333333333333333").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_checked_dec() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 10

                    y : Dec
                    y = 3

                    when Num.div_checked x y is
                        Ok val -> val
                        Err _ -> -1
                "
        ),
        RocDec::from_str("3.333333333333333333").unwrap(),
        RocDec
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_checked_by_zero_dec() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 10

                    y : Dec
                    y = 0

                    when Num.div_checked x y is
                        Ok val -> val
                        Err _ -> -1
                "
        ),
        RocDec::from_str("-1").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Decimal division by 0!"#)]
fn gen_div_dec_zero_by_zero() {
    assert_evals_to!("0dec / 0", RocDec::from_str("-1").unwrap(), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Decimal division by 0!"#)]
fn gen_div_dec_by_zero() {
    assert_evals_to!("1dec / 0", RocDec::from_str("-1").unwrap(), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer division by 0!"#)]
fn gen_div_ceil_by_zero() {
    assert_evals_to!(
        r"
            Num.div_ceil 5 0 == 0
        ",
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_int_eq() {
    assert_evals_to!(
        indoc!(
            r"
                    4 == 4
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_int_neq() {
    assert_evals_to!(
        indoc!(
            r"
                    4 != 5
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_less_than() {
    assert_evals_to!("4 < 5", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_less_than() {
    assert_evals_to!("4.0 < 5.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_greater_than() {
    assert_evals_to!("5.0 > 4.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_dec_eq() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 4

                    y : Dec
                    y = 4

                    x == y
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_dec_neq() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 4

                    y : Dec
                    y = 5

                    x != y
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_wrap_int_neq() {
    assert_evals_to!(
        indoc!(
            r"
                    wrapped_not_eq : a, a -> Bool where a implements Eq
                    wrapped_not_eq = \num1, num2 ->
                        num1 != num2

                    wrapped_not_eq 2 3
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_i8() {
    assert_evals_to!(
        indoc!(
            r"
                    1i8 + 2i8 + 3i8
                "
        ),
        6,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_u8() {
    assert_evals_to!(
        indoc!(
            r"
                    1u8 + 2u8 + 3u8
                "
        ),
        6,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_i16() {
    assert_evals_to!(
        indoc!(
            r"
                    1i16 + 2i16 + 3i16
                "
        ),
        6,
        i16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_u16() {
    assert_evals_to!(
        indoc!(
            r"
                    1u16 + 2u16 + 3u16
                "
        ),
        6,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_i32() {
    assert_evals_to!(
        indoc!(
            r"
                    1i32 + 2i32 + 3i32
                "
        ),
        6,
        i32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_u32() {
    assert_evals_to!(
        indoc!(
            r"
                    1u32 + 2u32 + 3u32
                "
        ),
        6,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_add_i64() {
    assert_evals_to!(
        indoc!(
            r"
                    1 + 2 + 3
                "
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_dec() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 1.5

                    y : Dec
                    y = 2.4

                    z : Dec
                    z = 3

                    (x - y) - z
                "
        ),
        RocDec::from_str("-3.9").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_mul_dec() {
    assert_evals_to!(
        indoc!(
            r"
                    x : Dec
                    x = 2

                    y : Dec
                    y = 4

                    z : Dec
                    z = 6

                    x * y * z
                "
        ),
        RocDec::from_str("48.0").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_f64() {
    assert_evals_to!("1.5f64 - 2.4 - 3", -3.9, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_f32() {
    assert_evals_to!("1.5f32 - 2.4 - 3", -3.9, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_i8() {
    assert_evals_to!("1i8 - 2i8 - 3i8", -4, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_u8() {
    assert_evals_to!("8u8 - 2u8 - 3u8", 3, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_i16() {
    assert_evals_to!("1i16 - 2i16 - 3i16", -4, i16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_u16() {
    assert_evals_to!("8u16 - 2u16 - 3u16", 3, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_i32() {
    assert_evals_to!("1i32 - 2i32 - 3i32", -4, i32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_u32() {
    assert_evals_to!("8u32 - 2u32 - 3u32", 3, u32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_sub_i64() {
    assert_evals_to!("1 - 2 - 3", -4, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_signed_mul_quadword_and_lower() {
    assert_evals_to!("2i64 * 4 * 6", 48, i64);
    assert_evals_to!("2i32 * 4 * 6", 48, i32);
    assert_evals_to!("2i16 * 4 * 6", 48, i16);
    assert_evals_to!("2i8 * 4 * 6", 48, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_unsigned_mul_quadword_and_lower() {
    assert_evals_to!("2u64 * 4 * 6", 48, u64);
    assert_evals_to!("2u32 * 4 * 6", 48, u32);
    assert_evals_to!("2u16 * 4 * 6", 48, u16);
    assert_evals_to!("2u8 * 4 * 6", 48, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_mul_f64() {
    assert_evals_to!("2f64 * 4 * 6", 48.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_mul_f32() {
    assert_evals_to!("2f32 * 4 * 6", 48.0, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_i64() {
    assert_evals_to!("1000i64 // 10", 100, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_u64() {
    assert_evals_to!("1000u64 // 10", 100, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"User crash with message: "Integer division by 0!"#)]
fn gen_div_by_zero_i64() {
    assert_evals_to!("1i64 // 0", 100, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_checked_i64() {
    assert_evals_to!(
        "Num.div_trunc_checked 1000 10",
        RocResult::ok(100),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_div_checked_by_zero_i64() {
    assert_evals_to!(
        "Num.div_trunc_checked 1000 0",
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_rem_i64() {
    assert_evals_to!("Num.rem 8 3", 2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"User crash with message: "Integer division by 0!"#)]
fn gen_rem_div_by_zero_i64() {
    assert_evals_to!("Num.rem 42 0", 100, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_rem_checked_i64() {
    assert_evals_to!(
        "Num.rem_checked 42 40",
        RocResult::ok(2),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_rem_checked_div_by_zero_i64() {
    assert_evals_to!(
        "Num.rem_checked 8 0",
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_positive_i64() {
    assert_evals_to!("Num.is_positive 0", false, bool);
    assert_evals_to!("Num.is_positive 1", true, bool);
    assert_evals_to!("Num.is_positive -5", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_negative_i64() {
    assert_evals_to!("Num.is_negative 0", false, bool);
    assert_evals_to!("Num.is_negative 3", false, bool);
    assert_evals_to!("Num.is_negative -2", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_positive_f64() {
    assert_evals_to!("Num.is_positive 0.0", false, bool);
    assert_evals_to!("Num.is_positive 4.7", true, bool);
    assert_evals_to!("Num.is_positive -8.5", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_negative_f64() {
    assert_evals_to!("Num.is_negative 0.0", false, bool);
    assert_evals_to!("Num.is_negative 9.9", false, bool);
    assert_evals_to!("Num.is_negative -4.4", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_zero_i64() {
    assert_evals_to!("Num.is_zero 0", true, bool);
    assert_evals_to!("Num.is_zero 0_0", true, bool);
    assert_evals_to!("Num.is_zero 1", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_zero_f64() {
    assert_evals_to!("Num.is_zero 0.0f64", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_zero_dec() {
    assert_evals_to!("Num.is_zero 0.0dec", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_odd() {
    assert_evals_to!("Num.is_odd 4", false, bool);
    assert_evals_to!("Num.is_odd 5", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_even() {
    assert_evals_to!("Num.is_even 6", true, bool);
    assert_evals_to!("Num.is_even 7", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn sin() {
    assert_evals_to!("Num.sin 0f64", 0.0, f64);
    assert_evals_to!("Num.sin 1.41421356237f64", 0.9877659459922529, f64);
    assert_evals_to!("Num.sin 0dec", RocDec::from_str("0.0").unwrap(), RocDec);
    assert_evals_to!(
        "Num.sin 1.414213562373095049dec",
        RocDec::from_str("0.987765945992735616").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn cos() {
    assert_evals_to!("Num.cos 0f64", 1.0, f64);
    assert_evals_to!("Num.cos 3.14159265359f64", -1.0, f64);
    assert_evals_to!("Num.cos 0dec", RocDec::from_str("1.0").unwrap(), RocDec);
    assert_evals_to!(
        "Num.cos 3.141592653589793238dec",
        RocDec::from_str("-1.0").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn tan() {
    assert_evals_to!("Num.tan 0f64", 0.0f64, f64);
    assert_evals_to!("Num.tan 0dec", RocDec::from_str("0.0").unwrap(), RocDec);
    // TODO: deal with answers rounding differently on different cpus.
    // These leads to results being off by a bit or 2.
    // assert_evals_to!("Num.tan 1f64", 1.5574077246549023f64, f64);
    // assert_evals_to!(
    //     "Num.tan 1dec",
    //     RocDec::from_str("1.557407724654902272").unwrap(),
    //     RocDec
    // );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bitwise_and() {
    assert_evals_to!("Num.bitwise_and 20 20", 20, i64);
    assert_evals_to!("Num.bitwise_and 25 10", 8, i64);
    assert_evals_to!("Num.bitwise_and 200 0", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bitwise_xor() {
    assert_evals_to!("Num.bitwise_xor 20 20", 0, i64);
    assert_evals_to!("Num.bitwise_xor 15 14", 1, i64);
    assert_evals_to!("Num.bitwise_xor 7 15", 8, i64);
    assert_evals_to!("Num.bitwise_xor 200 0", 200, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bitwise_or() {
    assert_evals_to!("Num.bitwise_or 1 1", 1, i64);
    assert_evals_to!("Num.bitwise_or 1 2", 3, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lt_u8() {
    assert_evals_to!("1u8 < 2u8", true, bool);
    assert_evals_to!("1u8 < 1u8", false, bool);
    assert_evals_to!("2u8 < 1u8", false, bool);
    assert_evals_to!("0u8 < 0u8", false, bool);
    assert_evals_to!("128u8 < 0u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lte_u8() {
    assert_evals_to!("1u8 <= 1u8", true, bool);
    assert_evals_to!("2u8 <= 1u8", false, bool);
    assert_evals_to!("1u8 <= 2u8", true, bool);
    assert_evals_to!("0u8 <= 0u8", true, bool);
    assert_evals_to!("128u8 <= 0u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gt_u8() {
    assert_evals_to!("2u8 > 1u8", true, bool);
    assert_evals_to!("2u8 > 2u8", false, bool);
    assert_evals_to!("1u8 > 1u8", false, bool);
    assert_evals_to!("0u8 > 0u8", false, bool);
    assert_evals_to!("0u8 > 128u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gte_u8() {
    assert_evals_to!("1u8 >= 1u8", true, bool);
    assert_evals_to!("1u8 >= 2u8", false, bool);
    assert_evals_to!("2u8 >= 1u8", true, bool);
    assert_evals_to!("0u8 >= 0u8", true, bool);
    assert_evals_to!("0u8 >= 128u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lt_u64() {
    assert_evals_to!("1u64 < 2u64", true, bool);
    assert_evals_to!("1u64 < 1u64", false, bool);
    assert_evals_to!("2u64 < 1u64", false, bool);
    assert_evals_to!("0u64 < 0u64", false, bool);
    assert_evals_to!("9223372036854775808u64 < 0u64", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lte_u64() {
    assert_evals_to!("1u64 <= 1u64", true, bool);
    assert_evals_to!("2u64 <= 1u64", false, bool);
    assert_evals_to!("1u64 <= 2u64", true, bool);
    assert_evals_to!("0u64 <= 0u64", true, bool);
    assert_evals_to!("9223372036854775808u64 <= 0u64", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gt_u64() {
    assert_evals_to!("2u64 > 1u64", true, bool);
    assert_evals_to!("2u64 > 2u64", false, bool);
    assert_evals_to!("1u64 > 1u64", false, bool);
    assert_evals_to!("0u64 > 0u64", false, bool);
    assert_evals_to!("0u64 > 9223372036854775808u64", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gte_u64() {
    assert_evals_to!("1u64 >= 1u64", true, bool);
    assert_evals_to!("1u64 >= 2u64", false, bool);
    assert_evals_to!("2u64 >= 1u64", true, bool);
    assert_evals_to!("0u64 >= 0u64", true, bool);
    assert_evals_to!("0u64 >= 9223372036854775808u64", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lt_i64() {
    assert_evals_to!("1 < 2", true, bool);
    assert_evals_to!("1 < 1", false, bool);
    assert_evals_to!("2 < 1", false, bool);
    assert_evals_to!("0 < 0", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lte_i64() {
    assert_evals_to!("1 <= 1", true, bool);
    assert_evals_to!("2 <= 1", false, bool);
    assert_evals_to!("1 <= 2", true, bool);
    assert_evals_to!("0 <= 0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gt_i64() {
    assert_evals_to!("2 > 1", true, bool);
    assert_evals_to!("2 > 2", false, bool);
    assert_evals_to!("1 > 1", false, bool);
    assert_evals_to!("0 > 0", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gte_i64() {
    assert_evals_to!("1 >= 1", true, bool);
    assert_evals_to!("1 >= 2", false, bool);
    assert_evals_to!("2 >= 1", true, bool);
    assert_evals_to!("0 >= 0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lt_f64() {
    assert_evals_to!("1.1f64 < 1.2", true, bool);
    assert_evals_to!("1.1f64 < 1.1", false, bool);
    assert_evals_to!("1.2f64 < 1.1", false, bool);
    assert_evals_to!("0.0f64 < 0.0", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lte_f64() {
    assert_evals_to!("1.1f64 <= 1.1", true, bool);
    assert_evals_to!("1.2f64 <= 1.1", false, bool);
    assert_evals_to!("1.1f64 <= 1.2", true, bool);
    assert_evals_to!("0.0f64 <= 0.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gt_f64() {
    assert_evals_to!("2.2f64 > 1.1", true, bool);
    assert_evals_to!("2.2f64 > 2.2", false, bool);
    assert_evals_to!("1.1f64 > 2.2", false, bool);
    assert_evals_to!("0.0f64 > 0.0", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gte_f64() {
    assert_evals_to!("1.1f64 >= 1.1", true, bool);
    assert_evals_to!("1.1f64 >= 1.2", false, bool);
    assert_evals_to!("1.2f64 >= 1.1", true, bool);
    assert_evals_to!("0.0f64 >= 0.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_approx_eq() {
    assert_evals_to!("Num.is_approx_eq 1e10f64 1.00001e10f64 {}", true, bool);
    assert_evals_to!("Num.is_approx_eq 1e-7f64 1e-8f64 {}", false, bool);
    assert_evals_to!("Num.is_approx_eq 1e-8f32 1e-9f32 {}", true, bool);
    assert_evals_to!("Num.is_approx_eq 1e10f64 1.0001e10f64 {}", false, bool);
    assert_evals_to!("Num.is_approx_eq 1.0f32 1.0 {}", true, bool);
    assert_evals_to!("Num.is_approx_eq (1f64 / 0.0) (1f64 / 0.0) {}", true, bool);
    assert_evals_to!("Num.is_approx_eq (0f64 / 0.0) (0f64 / 0.0) {}", false, bool);
    assert_evals_to!("Num.is_approx_eq 1e-8f64 0f64 {}", true, bool);
    assert_evals_to!("Num.is_approx_eq 1e-7f64 0f64 {}", false, bool);
    assert_evals_to!(
        "Num.is_approx_eq 1e-100f64 0f64 { atol: 0f64 }",
        false,
        bool
    );
    assert_evals_to!("Num.is_approx_eq 1e-7f64 0f64 { atol: 0f64 }", false, bool);
    assert_evals_to!("Num.is_approx_eq 1e-10f64 1e-20f64 {}", true, bool);
    assert_evals_to!("Num.is_approx_eq 1e-10f64 0f64 {}", true, bool);
    assert_evals_to!(
        "Num.is_approx_eq 1e-10f64 0.999999e-10f64 { atol: 0f64 }",
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_order_of_arithmetic_ops() {
    assert_evals_to!(
        indoc!(
            r"
                    1 + 3 * 7 - 2
                "
        ),
        20,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_order_of_arithmetic_ops_complex_float() {
    assert_evals_to!(
        indoc!(
            r"
                    3 - 48 * 2.0f64
                "
        ),
        -93.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_bind_variable_false() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = ||
                    when 10 is
                        x if x == 5 -> 0
                        _ -> 42

                wrapper()
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_bind_variable_true() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = ||
                    when 10 is
                        x if x == 10 -> 42
                        _ -> 0

                wrapper()
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn tail_call_elimination() {
    assert_evals_to!(
        indoc!(
            r"
                    sum = \n, accum ->
                        when n is
                            0 -> accum
                            _ -> sum (n - 1) (n + accum)

                    sum 1_000_000 0
                "
        ),
        500000500000,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_negate() {
    assert_evals_to!("Num.neg 123i8", -123, i8);
    assert_evals_to!("Num.neg Num.max_i8", -i8::MAX, i8);
    assert_evals_to!("Num.neg (Num.min_i8 + 1)", i8::MAX, i8);

    assert_evals_to!("Num.neg 123i16", -123, i16);
    assert_evals_to!("Num.neg Num.max_i16", -i16::MAX, i16);
    assert_evals_to!("Num.neg (Num.min_i16 + 1)", i16::MAX, i16);

    assert_evals_to!("Num.neg 123i32", -123, i32);
    assert_evals_to!("Num.neg Num.max_i32", -i32::MAX, i32);
    assert_evals_to!("Num.neg (Num.min_i32 + 1)", i32::MAX, i32);

    assert_evals_to!("Num.neg 123", -123, i64);
    assert_evals_to!("Num.neg Num.max_i64", -i64::MAX, i64);
    assert_evals_to!("Num.neg (Num.min_i64 + 1)", i64::MAX, i64);

    assert_evals_to!("Num.neg 12.3f32", -12.3, f32);
    assert_evals_to!("Num.neg 0.0f32", -0.0, f32);
    assert_evals_to!("Num.neg Num.max_f32", -f32::MAX, f32);
    assert_evals_to!("Num.neg Num.min_f32", -f32::MIN, f32);
    assert_evals_to!("Num.neg Num.infinity_f32", -f32::INFINITY, f32);
    // can't test equality for nan
    assert_evals_to!("Num.is_nan (Num.neg Num.nan_f32)", true, bool);

    assert_evals_to!("Num.neg 12.3f64", -12.3, f64);
    assert_evals_to!("Num.neg 0.0f64", -0.0, f64);
    assert_evals_to!("Num.neg Num.max_f64", -f64::MAX, f64);
    assert_evals_to!("Num.neg Num.min_f64", -f64::MIN, f64);
    assert_evals_to!("Num.neg Num.infinity_f64", -f64::INFINITY, f64);
    // can't test equality for nan
    assert_evals_to!("Num.is_nan (Num.neg Num.nan_f64)", true, bool);

    assert_evals_to!("Num.neg 123dec", RocDec::from(-123), RocDec);
    // 0 is signless, unlike f32/f64
    assert_evals_to!("Num.neg 0dec", RocDec::from(0), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(
    expected = r#"Roc failed with message: "Integer negation overflowed because its argument is the minimum value"#
)]
fn neg_min_int_overflow() {
    assert_evals_to!(
        indoc!(
            r"
                Num.neg Num.min_i64
                "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_wrap_int_neg() {
    assert_evals_to!(
        indoc!(
            r"
                    wrapped_neg = \num -> -num

                    wrapped_neg 3
                "
        ),
        -3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_basic_fn() {
    assert_evals_to!(
        indoc!(
            r"
                    always42 : Num.Num (Num.Integer Num.Signed64) -> Num.Num (Num.Integer Num.Signed64)
                    always42 = \_ -> 42

                    always42 5
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_to_float() {
    assert_evals_to!("Num.to_frac 0x9", RocDec::from(9i32), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_frac() {
    assert_evals_to!("Num.to_frac 9", RocDec::from(9i32), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn num_to_frac_f64_to_f32() {
    assert_evals_to!(
        indoc!(
            r"
                    f64 : F64
                    f64 = 9.0

                    f32 : F32
                    f32 = Num.to_frac f64
                    f32
                "
        ),
        9.0,
        f32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn num_to_frac_f32_to_f32() {
    assert_evals_to!(
        indoc!(
            r"

                    arg : F32
                    arg = 9.0

                    ret : F32
                    ret = Num.to_frac arg
                    ret
                "
        ),
        9.0,
        f32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_frac_f64_to_f64() {
    assert_evals_to!(
        indoc!(
            r"

                    arg : F64
                    arg = 9.0

                    ret : F64
                    ret = Num.to_frac arg
                    ret
                "
        ),
        9.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_frac_f32_to_f64() {
    assert_evals_to!(
        indoc!(
            r"

                    f32 : F32
                    f32 = 9.0

                    f64 : F64
                    f64 = Num.to_frac f32
                    f64
                "
        ),
        9.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_to_float() {
    assert_evals_to!(
        indoc!(
            r"
            x : F64
            x = Num.to_frac 0.5f64

            x
            "
        ),
        0.5,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn frac_is_nan() {
    assert_evals_to!("Num.is_nan (0 / 0f64)", true, bool);
    assert_evals_to!("Num.is_nan (1 / 0f64)", false, bool);
    assert_evals_to!("Num.is_nan 42f64", false, bool);
    assert_evals_to!("Num.is_nan 42dec", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn frac_is_infinite() {
    assert_evals_to!("Num.is_infinite (1 / 0f64)", true, bool);
    assert_evals_to!("Num.is_infinite (-1 / 0f64)", true, bool);
    assert_evals_to!("Num.is_infinite (0 / 0f64)", false, bool);
    assert_evals_to!("Num.is_infinite 42f64", false, bool);
    assert_evals_to!("Num.is_infinite 42dec", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn frac_is_finite() {
    assert_evals_to!("Num.is_finite 42f64", true, bool);
    assert_evals_to!("Num.is_finite (1 / 0f64)", false, bool);
    assert_evals_to!("Num.is_finite (0 / 0f64)", false, bool);
    assert_evals_to!("Num.is_finite 42dec", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_compare() {
    assert_evals_to!("Num.compare 0 1", RocOrder::Lt, RocOrder);
    assert_evals_to!("Num.compare 1 1", RocOrder::Eq, RocOrder);
    assert_evals_to!("Num.compare 1 0", RocOrder::Gt, RocOrder);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_compare() {
    assert_evals_to!("Num.compare 0.01 3.14", RocOrder::Lt, RocOrder);
    assert_evals_to!("Num.compare 3.14 3.14", RocOrder::Eq, RocOrder);
    assert_evals_to!("Num.compare 3.14 0.01", RocOrder::Gt, RocOrder);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pow_f64() {
    assert_evals_to!("Num.pow 2.0f64 2.0f64", 4.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pow_dec() {
    assert_evals_to!("Num.pow 2.0dec 2.0dec", RocDec::from(4), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn round_f64() {
    assert_evals_to!("Num.round 1.9f64", 2, i64);
    assert_evals_to!("Num.round -1.9f64", -2, i64);
    assert_evals_to!("Num.round 0.5f64", 1, i64);
    assert_evals_to!("Num.round -0.5f64", -1, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn round_dec() {
    assert_evals_to!("Num.round 1.9dec", 2, i64);
    assert_evals_to!("Num.round -1.9dec", -2, i64);
    assert_evals_to!("Num.round 0.5dec", 1, i64);
    assert_evals_to!("Num.round -0.5dec", -1, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn ceiling_f64() {
    assert_evals_to!("Num.ceiling 1.9f64", 2, i64);
    assert_evals_to!("Num.ceiling -1.9f64", -1, i64);
    assert_evals_to!("Num.ceiling 0.5f64", 1, i64);
    assert_evals_to!("Num.ceiling -0.5f64", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn ceiling_dec() {
    assert_evals_to!("Num.ceiling 1.9dec", 2, i64);
    assert_evals_to!("Num.ceiling -1.9dec", -1, i64);
    assert_evals_to!("Num.ceiling 0.5dec", 1, i64);
    assert_evals_to!("Num.ceiling -0.5dec", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn floor_f64() {
    assert_evals_to!("Num.floor 1.9f64", 1, i64);
    assert_evals_to!("Num.floor -1.9f64", -2, i64);
    assert_evals_to!("Num.floor 0.5f64", 0, i64);
    assert_evals_to!("Num.floor -0.5f64", -1, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn floor_dec() {
    assert_evals_to!("Num.floor 1.9dec", 1, i64);
    assert_evals_to!("Num.floor -1.9dec", -2, i64);
    assert_evals_to!("Num.floor 0.5dec", 0, i64);
    assert_evals_to!("Num.floor -0.5dec", -1, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pow_int() {
    assert_evals_to!("Num.pow_int 2 3", 8, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer raised to power overflowed!"#)]
fn pow_int_overflow() {
    assert_evals_to!("Num.pow_int 2u8 8", 0, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn atan() {
    assert_evals_to!("Num.atan 10f64", 1.4711276743037347, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer addition overflowed!"#)]
fn int_add_overflow() {
    assert_evals_to!("9_223_372_036_854_775_807 + 1", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_add_checked_ok() {
    assert_evals_to!(
        "Num.add_checked 1 2",
        RocResult::ok(3),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_add_checked_err() {
    assert_evals_to!(
        "Num.add_checked 9_223_372_036_854_775_807 1",
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_add_checked_pass() {
    assert_evals_to!(
        "Num.add_checked 1.0 0.0f64",
        RocResult::ok(1.0),
        RocResult<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_add_checked_fail() {
    assert_evals_to!(
        "Num.add_checked 1.7976931348623157e308f64 1.7976931348623157e308",
        RocResult::err(()),
        RocResult<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_add_overflow() {
    assert_evals_to!(
        "1.7976931348623157e308f64 + 1.7976931348623157e308",
        f64::INFINITY,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn add_wrap() {
    assert_evals_to!("Num.add_wrap 255u8 10u8", 9u8, u8);
    assert_evals_to!("Num.add_wrap 127i8 10i8", -119i8, i8);
    assert_evals_to!("Num.add_wrap -127i8 -10i8", 119i8, i8);
    assert_evals_to!("Num.add_wrap 65535u16 10", 9u16, u16);
    assert_evals_to!("Num.add_wrap  32767i16 10", -32759i16, i16);
    assert_evals_to!("Num.add_wrap -32767i16 -10", 32759i16, i16);
    assert_evals_to!("Num.add_wrap 4294967295u32 10", 9u32, u32);
    assert_evals_to!("Num.add_wrap  2147483647i32 10", -2147483639i32, i32);
    assert_evals_to!("Num.add_wrap -2147483647i32 -10", 2147483639i32, i32);
    assert_evals_to!("Num.add_wrap 18446744073709551615u64 10", 9u64, u64);
    assert_evals_to!(
        "Num.add_wrap 9223372036854775807i64 10",
        -9223372036854775799i64,
        i64
    );
    assert_evals_to!(
        "Num.add_wrap -9223372036854775807i64 -10",
        9223372036854775799i64,
        i64
    );
    assert_evals_to!(
        "Num.add_wrap 340282366920938463463374607431768211455u128 10",
        U128::from(9u128),
        U128
    );
    assert_evals_to!(
        "Num.add_wrap 170141183460469231731687303715884105727i128 10",
        I128::from(-170141183460469231731687303715884105719i128),
        I128
    );
    assert_evals_to!(
        "Num.add_wrap -170141183460469231731687303715884105727i128 -10",
        I128::from(170141183460469231731687303715884105719i128),
        I128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn add_saturated() {
    assert_evals_to!("Num.add_saturated 200u8 200u8", 255u8, u8);
    assert_evals_to!("Num.add_saturated 100i8 100i8", 127i8, i8);
    assert_evals_to!("Num.add_saturated -100i8 -100i8", -128i8, i8);
    assert_evals_to!("Num.add_saturated 40000u16 40000u16", 65535u16, u16);
    assert_evals_to!("Num.add_saturated 20000i16 20000i16", 32767i16, i16);
    assert_evals_to!("Num.add_saturated -20000i16 -20000i16", -32768i16, i16);
    assert_evals_to!(
        "Num.add_saturated 3000000000u32 3000000000u32",
        4294967295u32,
        u32
    );
    assert_evals_to!(
        "Num.add_saturated 2000000000i32 2000000000i32",
        2147483647i32,
        i32
    );
    assert_evals_to!(
        "Num.add_saturated -2000000000i32 -2000000000i32",
        -2147483648i32,
        i32
    );
    assert_evals_to!(
        "Num.add_saturated 10000000000000000000u64 10000000000000000000u64",
        18446744073709551615u64,
        u64
    );
    assert_evals_to!(
        "Num.add_saturated 5000000000000000000i64 5000000000000000000i64 ",
        9223372036854775807i64,
        i64
    );
    assert_evals_to!(
        "Num.add_saturated -5000000000000000000i64 -5000000000000000000i64 ",
        -9223372036854775808i64,
        i64
    );
    assert_evals_to!(
        "Num.add_saturated 200000000000000000000000000000000000000u128 200000000000000000000000000000000000000u128",
        U128::from(340282366920938463463374607431768211455u128),
        U128
    );
    assert_evals_to!(
        "Num.add_saturated 100000000000000000000000000000000000000i128 100000000000000000000000000000000000000i128",
        I128::from(170141183460469231731687303715884105727i128),
        I128
    );
    assert_evals_to!(
        "Num.add_saturated -100000000000000000000000000000000000000i128 -100000000000000000000000000000000000000i128",
        I128::from(-170141183460469231731687303715884105728i128),
        I128
    );
    assert_evals_to!(
        "Num.add_saturated Num.max_f32 Num.max_f32",
        std::f32::INFINITY,
        f32
    );
    assert_evals_to!(
        "Num.add_saturated Num.min_f32 Num.min_f32",
        std::f32::NEG_INFINITY,
        f32
    );
    assert_evals_to!(
        "Num.add_saturated Num.max_f64 Num.max_f64",
        std::f64::INFINITY,
        f64
    );
    assert_evals_to!(
        "Num.add_saturated Num.min_f64 Num.min_f64",
        std::f64::NEG_INFINITY,
        f64
    );

    assert_evals_to!(
        "Num.add_saturated 170_141_183_460_469_231_731dec 1",
        RocDec::from_str("170141183460469231731.687303715884105727").unwrap(),
        RocDec
    );
    assert_evals_to!(
        "Num.add_saturated -170_141_183_460_469_231_731dec -1",
        RocDec::from_str("-170141183460469231731.687303715884105728").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_sub_overflow() {
    assert_evals_to!(
        "-1.7976931348623157e308f64 - 1.7976931348623157e308",
        -f64::INFINITY,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_sub_checked() {
    assert_evals_to!(
                "Num.sub_checked 5 2",
                RocResult::ok(3),
                RocResult<i64, ()>
    );

    assert_evals_to!(
                "Num.sub_checked Num.min_i64 1 ",
                RocResult::err(()),
                RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_sub_checked() {
    assert_evals_to!(
        "Num.sub_checked 1.0 0.0f64",
        RocResult::ok(1.0),
        RocResult<f64, ()>
    );

    assert_evals_to!(
        "Num.sub_checked -1.7976931348623157e308f64 1.7976931348623157e308",
        RocResult::err(()),
        RocResult<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer subtraction overflowed!"#)]
fn int_sub_overflow() {
    assert_evals_to!("-9_223_372_036_854_775_808 - 1", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn sub_wrap() {
    assert_evals_to!("Num.sub_wrap 1u8 10u8", 247u8, u8);
    assert_evals_to!("Num.sub_wrap 127i8 -10i8", -119i8, i8);
    assert_evals_to!("Num.sub_wrap -127i8 10i8", 119i8, i8);
    assert_evals_to!("Num.sub_wrap 1u16 10", 65527u16, u16);
    assert_evals_to!("Num.sub_wrap 32767i16 -10", -32759i16, i16);
    assert_evals_to!("Num.sub_wrap -32767i16 10", 32759i16, i16);
    assert_evals_to!("Num.sub_wrap 1u32 10", 4294967287u32, u32);
    assert_evals_to!("Num.sub_wrap  2147483647i32 -10", -2147483639i32, i32);
    assert_evals_to!("Num.sub_wrap -2147483647i32 10", 2147483639i32, i32);
    assert_evals_to!("Num.sub_wrap 1u64 10", 18446744073709551607u64, u64);
    assert_evals_to!(
        "Num.sub_wrap 9223372036854775807i64 -10",
        -9223372036854775799i64,
        i64
    );
    assert_evals_to!(
        "Num.sub_wrap -9223372036854775807i64 10",
        9223372036854775799i64,
        i64
    );
    assert_evals_to!(
        "Num.sub_wrap 1u128 10",
        U128::from(340282366920938463463374607431768211447u128),
        U128
    );
    assert_evals_to!(
        "Num.sub_wrap 170141183460469231731687303715884105727i128 -10",
        I128::from(-170141183460469231731687303715884105719i128),
        I128
    );
    assert_evals_to!(
        "Num.sub_wrap -170141183460469231731687303715884105727i128 10",
        I128::from(170141183460469231731687303715884105719i128),
        I128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn sub_saturated() {
    assert_evals_to!("Num.sub_saturated 1u8 10u8", 0u8, u8);
    assert_evals_to!("Num.sub_saturated 100i8 -100i8", 127i8, i8);
    assert_evals_to!("Num.sub_saturated -100i8 100i8", -128i8, i8);
    assert_evals_to!("Num.sub_saturated 1u16 10u16", 0u16, u16);
    assert_evals_to!("Num.sub_saturated 20000i16 -20000i16", 32767i16, i16);
    assert_evals_to!("Num.sub_saturated -20000i16 20000i16", -32768i16, i16);
    assert_evals_to!("Num.sub_saturated 1u32 10u32", 0u32, u32);
    assert_evals_to!(
        "Num.sub_saturated 2000000000i32 -2000000000i32",
        2147483647i32,
        i32
    );
    assert_evals_to!(
        "Num.sub_saturated -2000000000i32 2000000000i32",
        -2147483648i32,
        i32
    );
    assert_evals_to!("Num.sub_saturated 1u64 10u64", 0u64, u64);
    assert_evals_to!(
        "Num.sub_saturated 5000000000000000000i64 -5000000000000000000i64 ",
        9223372036854775807i64,
        i64
    );
    assert_evals_to!(
        "Num.sub_saturated -5000000000000000000i64 5000000000000000000i64 ",
        -9223372036854775808i64,
        i64
    );
    assert_evals_to!("Num.sub_saturated 1u128 10", U128::from(0u128), U128);
    assert_evals_to!(
        "Num.sub_saturated 100000000000000000000000000000000000000i128 -100000000000000000000000000000000000000i128",
        I128::from(170141183460469231731687303715884105727i128),
        I128
    );
    assert_evals_to!(
        "Num.sub_saturated -100000000000000000000000000000000000000i128 100000000000000000000000000000000000000i128",
        I128::from(-170141183460469231731687303715884105728i128),
        I128
    );
    assert_evals_to!(
        "Num.sub_saturated Num.max_f32 -Num.max_f32",
        std::f32::INFINITY,
        f32
    );
    assert_evals_to!(
        "Num.sub_saturated Num.min_f32 -Num.min_f32",
        std::f32::NEG_INFINITY,
        f32
    );
    assert_evals_to!(
        "Num.sub_saturated Num.max_f64 -Num.max_f64",
        std::f64::INFINITY,
        f64
    );
    assert_evals_to!(
        "Num.sub_saturated Num.min_f64 -Num.min_f64",
        std::f64::NEG_INFINITY,
        f64
    );

    assert_evals_to!(
        "Num.sub_saturated 170_141_183_460_469_231_731dec -1",
        RocDec::from_str("170141183460469231731.687303715884105727").unwrap(),
        RocDec
    );
    assert_evals_to!(
        "Num.sub_saturated -170_141_183_460_469_231_731dec 1",
        RocDec::from_str("-170141183460469231731.687303715884105728").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer multiplication overflowed!"#)]
fn int_positive_mul_overflow() {
    assert_evals_to!("9_223_372_036_854_775_807 * 2", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer multiplication overflowed!"#)]
fn int_negative_mul_overflow() {
    assert_evals_to!("(-9_223_372_036_854_775_808) * 2", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_positive_mul_overflow() {
    assert_evals_to!("1.7976931348623157e308f64 * 2", f64::INFINITY, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_negative_mul_overflow() {
    assert_evals_to!("-1.7976931348623157e308f64 * 2", -f64::INFINITY, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_mul_checked() {
    assert_evals_to!(
        "Num.mul_checked 20 2",
        RocResult::ok(40),
        RocResult::<i64, ()>
    );

    assert_evals_to!(
        "Num.mul_checked Num.max_i64 2",
        RocResult::err(()),
        RocResult::<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_mul_checked() {
    assert_evals_to!(
        "Num.mul_checked 20.0 2.0f64",
        RocResult::ok(40.0),
        RocResult::<f64, ()>
    );

    assert_evals_to!(
        "Num.mul_checked 1.7976931348623157e308f64 2",
        RocResult::err(()),
        RocResult::<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mul_wrap() {
    assert_evals_to!("Num.mul_wrap 255u8 2", 254u8, u8);
    assert_evals_to!("Num.mul_wrap 127i8 2", -2i8, i8);
    assert_evals_to!("Num.mul_wrap -127i8 2", 2i8, i8);
    assert_evals_to!("Num.mul_wrap 65535u16 2", 65534u16, u16);
    assert_evals_to!("Num.mul_wrap  32767i16 2", -2i16, i16);
    assert_evals_to!("Num.mul_wrap -32767i16 2", 2i16, i16);
    assert_evals_to!("Num.mul_wrap 4294967295u32 2", 4294967294u32, u32);
    assert_evals_to!("Num.mul_wrap  2147483647i32 2", -2i32, i32);
    assert_evals_to!("Num.mul_wrap -2147483647i32 2", 2i32, i32);
    assert_evals_to!(
        "Num.mul_wrap 18446744073709551615u64 2",
        18446744073709551614u64,
        u64
    );
    assert_evals_to!("Num.mul_wrap 9223372036854775807i64 2", -2i64, i64);
    assert_evals_to!("Num.mul_wrap -9223372036854775807i64 2", 2i64, i64);
    assert_evals_to!(
        "Num.mul_wrap 340282366920938463463374607431768211455u128 2",
        U128::from(340282366920938463463374607431768211454u128),
        U128
    );
    assert_evals_to!(
        "Num.mul_wrap 170141183460469231731687303715884105727i128 2",
        I128::from(-2i128),
        I128
    );
    assert_evals_to!(
        "Num.mul_wrap -170141183460469231731687303715884105727i128 2",
        I128::from(2i128),
        I128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mul_saturated() {
    assert_evals_to!("Num.mul_saturated 200u8 2", 255u8, u8);
    assert_evals_to!("Num.mul_saturated 100i8 2", 127i8, i8);
    assert_evals_to!("Num.mul_saturated -100i8 2", -128i8, i8);
    assert_evals_to!("Num.mul_saturated 40000u16 2", 65535u16, u16);
    assert_evals_to!("Num.mul_saturated 20000i16 2", 32767i16, i16);
    assert_evals_to!("Num.mul_saturated -20000i16 2", -32768i16, i16);
    assert_evals_to!("Num.mul_saturated 3000000000u32 2", 4294967295u32, u32);
    assert_evals_to!("Num.mul_saturated 2000000000i32 2", 2147483647i32, i32);
    assert_evals_to!("Num.mul_saturated -2000000000i32 2", -2147483648i32, i32);
    assert_evals_to!(
        "Num.mul_saturated 10000000000000000000u64 2",
        18446744073709551615u64,
        u64
    );
    assert_evals_to!(
        "Num.mul_saturated 5000000000000000000i64 2",
        9223372036854775807i64,
        i64
    );
    assert_evals_to!(
        "Num.mul_saturated -5000000000000000000i64 2",
        -9223372036854775808i64,
        i64
    );
    assert_evals_to!(
        "Num.mul_saturated 200000000000000000000000000000000000000u128 2",
        U128::from(340282366920938463463374607431768211455u128),
        U128
    );
    assert_evals_to!(
        "Num.mul_saturated 100000000000000000000000000000000000000i128 2",
        I128::from(170141183460469231731687303715884105727i128),
        I128
    );
    assert_evals_to!(
        "Num.mul_saturated -100000000000000000000000000000000000000i128 2",
        I128::from(-170141183460469231731687303715884105728i128),
        I128
    );
    assert_evals_to!("Num.mul_saturated Num.max_f32 2", std::f32::INFINITY, f32);
    assert_evals_to!(
        "Num.mul_saturated Num.min_f32 2",
        std::f32::NEG_INFINITY,
        f32
    );
    assert_evals_to!("Num.mul_saturated Num.max_f64 2", std::f64::INFINITY, f64);
    assert_evals_to!(
        "Num.mul_saturated Num.min_f64 2",
        std::f64::NEG_INFINITY,
        f64
    );

    // TODO: This doesn't work anywhere? It returns -1.374607431768211456 : Dec ?
    /*
    assert_evals_to!(
        "Num.mul_saturated 170_141_183_460_469_231_731dec 2",
        RocDec::from_str("170141183460469231731.687303715884105727").unwrap(),
        RocDec
    );
    assert_evals_to!(
        "Num.mul_saturated -170_141_183_460_469_231_731dec 2",
        RocDec::from_str("-170141183460469231731.687303715884105728").unwrap(),
        RocDec
    );
    */
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_left_by() {
    assert_evals_to!("Num.shift_left_by 0b0000_0001 0", 0b0000_0001, i64);
    assert_evals_to!("Num.shift_left_by 0b0000_0001 1", 0b0000_0010, i64);
    assert_evals_to!("Num.shift_left_by 0b0000_0011 2", 0b0000_1100, i64);
    assert_evals_to!("Num.shift_left_by 2u16 2", 8, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_right_by() {
    // Sign Extended Right Shift

    let is_llvm_release_mode = cfg!(feature = "gen-llvm") && !cfg!(debug_assertions);

    assert_evals_to!("Num.shift_right_by 0b0100_0000i8 2", 0b0001_0000i8, i8);
    assert_evals_to!("Num.shift_right_by 0b1110_0000u8 1", 0b1111_0000u8, u8);
    assert_evals_to!("Num.shift_right_by 0b1100_0000u8 2", 0b1111_0000u8, u8);
    assert_evals_to!("Num.shift_right_by 0b0100_0000u8 12", 0b0000_0000u8, u8);

    // LLVM in release mode returns 0 instead of -1 for some reason
    if !is_llvm_release_mode {
        assert_evals_to!("Num.shift_right_by 0b1000_0000u8 12", 0b1111_1111u8, u8);
    }
    assert_evals_to!("Num.shift_right_by 12 0", 12, i64);
    assert_evals_to!("Num.shift_right_by 12 1", 6, i64);
    assert_evals_to!("Num.shift_right_by -12 1", -6, i64);
    assert_evals_to!("Num.shift_right_by 12 8", 0, i64);
    assert_evals_to!("Num.shift_right_by -12 8", -1, i64);
    assert_evals_to!("Num.shift_right_by 0 0", 0, i64);
    assert_evals_to!("Num.shift_right_by 0 1", 0, i64);

    assert_evals_to!("Num.shift_right_by 12i32 0", 12, i32);
    assert_evals_to!("Num.shift_right_by 12i32 1", 6, i32);
    assert_evals_to!("Num.shift_right_by -12i32 1", -6, i32);
    assert_evals_to!("Num.shift_right_by 12i32 8", 0, i32);
    assert_evals_to!("Num.shift_right_by -12i32 8", -1, i32);

    assert_evals_to!("Num.shift_right_by 12i8 0", 12, i8);
    assert_evals_to!("Num.shift_right_by 12i8 1", 6, i8);
    assert_evals_to!("Num.shift_right_by -12i8 1", -6, i8);
    assert_evals_to!("Num.shift_right_by 12i8 8", 0, i8);

    if !is_llvm_release_mode {
        assert_evals_to!("Num.shift_right_by -12i8 8", -1, i8);
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_right_zf_by() {
    // Logical Right Shift
    assert_evals_to!("Num.shift_right_zf_by 0b1100_0000u8 2", 0b0011_0000u8, u8);
    assert_evals_to!("Num.shift_right_zf_by 0b0000_0010u8 1", 0b0000_0001u8, u8);
    assert_evals_to!("Num.shift_right_zf_by 0b0000_1100u8 2", 0b0000_0011u8, u8);
    assert_evals_to!("Num.shift_right_zf_by 0b1000_0000u8 12", 0b0000_0000u8, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_right_cast_i8() {
    // arithmetic
    assert_evals_to!(
        "Num.shift_right_by (Num.to_i8 0b1100_0000u8) 2",
        0b1111_0000u8 as i8,
        i8
    );

    // logical
    assert_evals_to!(
        "Num.shift_right_zf_by (Num.to_i8 0b1100_0000u8) 2",
        0b0011_0000i8,
        i8
    );
    assert_evals_to!("Num.shift_right_zf_by 0b1100_0000u8 2", 0b0011_0000u8, u8);
    assert_evals_to!("Num.shift_right_zf_by 0b0000_0010u8 1", 0b0000_0001u8, u8);
    assert_evals_to!("Num.shift_right_zf_by 0b0000_1100u8 2", 0b0000_0011u8, u8);
    assert_evals_to!("Num.shift_right_zf_by 0b1000_0000u8 12", 0b0000_0000u8, u8);
    assert_evals_to!(
        "Num.shift_right_zf_by 0xffff_0000_0000_0000_0000_0000_0000_ffffu128 4",
        0x0fff_f000_0000_0000_0000_0000_0000_0fffu128,
        u128
    );
    assert_evals_to!(
        "Num.shift_right_zf_by 0xaaaa_0000_0000_bbbb_ffff_ffff_ffff_ffffu128 68",
        0x0000_0000_0000_0000_0aaa_a000_0000_0bbbu128,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_i128() {
    assert_evals_to!("Num.min_i128", I128::from(i128::MIN), I128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_i128() {
    assert_evals_to!("Num.max_i128", I128::from(i128::MAX), I128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_i64() {
    assert_evals_to!("Num.min_i64", i64::MIN, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_i64() {
    assert_evals_to!("Num.max_i64", i64::MAX, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_u64() {
    assert_evals_to!("Num.min_u64", u64::MIN, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_u64() {
    assert_evals_to!("Num.max_u64", u64::MAX, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_i32() {
    assert_evals_to!("Num.min_i32", i32::MIN, i32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_i32() {
    assert_evals_to!("Num.max_i32", i32::MAX, i32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_u32() {
    assert_evals_to!("Num.min_u32", u32::MIN, u32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_u32() {
    assert_evals_to!("Num.max_u32", u32::MAX, u32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_i16() {
    assert_evals_to!("Num.min_i16", i16::MIN, i16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_i16() {
    assert_evals_to!("Num.max_i16", i16::MAX, i16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_u16() {
    assert_evals_to!("Num.min_u16", u16::MIN, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_u16() {
    assert_evals_to!("Num.max_u16", u16::MAX, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_i8() {
    assert_evals_to!("Num.min_i8", i8::MIN, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_i8() {
    assert_evals_to!("Num.max_i8", i8::MAX, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_u8() {
    assert_evals_to!("Num.min_u8", u8::MIN, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_u8() {
    assert_evals_to!("Num.max_u8", u8::MAX, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_f64() {
    assert_evals_to!("Num.max_f64", f64::MAX, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_f64() {
    assert_evals_to!("Num.min_f64", f64::MIN, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_f32() {
    assert_evals_to!("Num.max_f32", f32::MAX, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_f32() {
    assert_evals_to!("Num.min_f32", f32::MIN, f32);
}

macro_rules! num_conversion_tests {
    ($($fn:expr, $typ:ty, ($($test_name:ident, $input:expr, $output:expr $(, [$($support_gen:literal),*])? )*))*) => {$($(
        #[test]
        #[cfg(any(feature = "gen-llvm", $($(feature = $support_gen,)*)?))]
        fn $test_name() {
            let input = format!("{} {}", $fn, $input);
            assert_evals_to!(&input, $output, $typ)
        }
    )*)*}
}

num_conversion_tests! {
    "Num.to_i8", i8, (
        to_i8_same_width, "15u8", 15, ["gen-wasm", "gen-dev"]
        to_i8_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_i8_truncate_wraps, "500i32", -12, ["gen-wasm", "gen-dev"]
    )
    "Num.to_i16", i16, (
        to_i16_same_width, "15u16", 15, ["gen-wasm", "gen-dev"]
        to_i16_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_i16_sign_extend_i8, "-15i8", -15, ["gen-wasm", "gen-dev"]
        to_i16_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_i16_truncate_wraps, "60000i32", -5536, ["gen-wasm", "gen-dev"]
    )
    "Num.to_i32", i32, (
        to_i32_same_width, "15u32", 15, ["gen-wasm", "gen-dev"]
        to_i32_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_i32_sign_extend_i8, "-15i8", -15, ["gen-wasm", "gen-dev"]
        to_i32_sign_extend_i16, "-15i16", -15, ["gen-wasm", "gen-dev"]
        to_i32_truncate, "115i64", 115, ["gen-wasm", "gen-dev"]
        to_i32_truncate_wraps, "5000000000i64", 705032704, ["gen-wasm", "gen-dev"]
    )
    "Num.to_i64", i64, (
        to_i64_same_width, "15u64", 15, ["gen-wasm", "gen-dev"]
        to_i64_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_i64_sign_extend_i8, "-15i8", -15, ["gen-wasm", "gen-dev"]
        to_i64_sign_extend_i16, "-15i16", -15, ["gen-wasm", "gen-dev"]
        to_i64_sign_extend_i32, "-15i32", -15, ["gen-wasm", "gen-dev"]
        to_i64_truncate, "115i128", 115
        to_i64_truncate_wraps, "10_000_000_000_000_000_000i128", -8446744073709551616
    )
    "Num.to_i128", i128, (
        to_i128_same_width, "15u128", 15, ["gen-dev"]
        to_i128_extend, "15i8", 15
    )
    "Num.to_u8", u8, (
        to_u8_same_width, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u8_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_u8_truncate_wraps, "500i32", 244, ["gen-wasm", "gen-dev"]
    )
    "Num.to_u16", u16, (
        to_u16_same_width, "15i16", 15, ["gen-wasm", "gen-dev"]
        to_u16_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u16_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_u16_truncate_wraps, "600000000i32", 17920, ["gen-wasm", "gen-dev"]
    )
    "Num.to_u32", u32, (
        to_u32_same_width, "15i32", 15, ["gen-wasm", "gen-dev"]
        to_u32_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u32_truncate, "115i64", 115, ["gen-wasm", "gen-dev"]
        to_u32_truncate_wraps, "5000000000000000000i64", 1156841472, ["gen-wasm", "gen-dev"]
    )
    "Num.to_u64", u64, (
        to_u64_same_width, "15i64", 15, ["gen-wasm", "gen-dev"]
        to_u64_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u64_truncate, "115i128", 115
        to_u64_truncate_wraps, "10_000_000_000_000_000_000_000i128", 1864712049423024128
    )
    "Num.to_u128", u128, (
        to_u128_same_width, "15i128", 15, ["gen-dev"]
        to_u128_extend, "15i8", 15
        to_u128_big, "11562537357600483583u64", 11562537357600483583, ["gen-dev"]
    )
    "Num.to_f32", f32, (
        to_f32_from_i8, "15i8", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_i16, "15i16", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_i32, "15i32", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_i64, "15i64", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_i128, "15i128", 15.0, ["gen-dev"]
        to_f32_from_u8, "15u8", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_u16, "15u16", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_u32, "15u32", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_u64, "15u64", 15.0, ["gen-wasm", "gen-dev"]
        to_f32_from_u128, "15u128", 15.0, ["gen-dev"]
        to_f32_from_f32, "1.5f32", 1.5, ["gen-wasm", "gen-dev"]
        to_f32_from_f64, "1.5f64", 1.5, ["gen-wasm", "gen-dev"]
    )
    "Num.to_f64", f64, (
        to_f64_from_i8, "15i8", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_i16, "15i16", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_i32, "15i32", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_i64, "15i64", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_i128, "15i128", 15.0, ["gen-dev"]
        to_f64_from_u8, "15u8", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_u16, "15u16", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_u32, "15u32", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_u64, "15u64", 15.0, ["gen-wasm", "gen-dev"]
        to_f64_from_u128, "15u128", 15.0, ["gen-dev"]
        to_f64_from_f32, "1.5f32", 1.5, ["gen-dev"]
        to_f64_from_f64, "1.5f64", 1.5, ["gen-wasm", "gen-dev"]
    )
}

macro_rules! to_int_checked_tests {
    ($($fn:expr, $typ:ty, ($($test_name:ident, $input:expr, $output:expr)*))*) => {$($(
        #[test]
        #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
        fn $test_name() {
            let sentinel = 23;
            // Some n = Ok n, None = OutOfBounds
            let expected = match $output.into() {
                None => sentinel,
                Some(n) => {
                    assert_ne!(n, sentinel);
                    n
                }
            };
            let input = format!("Result.with_default ({} {}) {}", $fn, $input, sentinel);
            assert_evals_to!(&input, expected, $typ)
        }
    )*)*}
}

to_int_checked_tests! {
    "Num.to_i8_checked", i8, (
        to_i8_checked_same,                             "15i8",    15
        to_i8_checked_same_width_unsigned_fits,         "15u8",    15
        to_i8_checked_same_width_unsigned_oob,          "128u8",   None
        to_i8_checked_larger_width_signed_fits_pos,     "15i16",   15
        to_i8_checked_larger_width_signed_oob_pos,      "128i16",  None
        to_i8_checked_larger_width_signed_fits_neg,     "-15i16",  -15
        to_i8_checked_larger_width_signed_oob_neg,      "-129i16", None
        to_i8_checked_larger_width_unsigned_fits_pos,   "15u16",   15
        to_i8_checked_larger_width_unsigned_oob_pos,    "128u16",  None
    )
    "Num.to_i16_checked", i16, (
        to_i16_checked_smaller_width_pos,                "15i8",      15
        to_i16_checked_smaller_width_neg,                "-15i8",     -15
        to_i16_checked_same,                             "15i16",     15
        to_i16_checked_same_width_unsigned_fits,         "15u16",     15
        to_i16_checked_same_width_unsigned_oob,          "32768u16",  None
        to_i16_checked_larger_width_signed_fits_pos,     "15i32",     15
        to_i16_checked_larger_width_signed_oob_pos,      "32768i32",  None
        to_i16_checked_larger_width_signed_fits_neg,     "-15i32",    -15
        to_i16_checked_larger_width_signed_oob_neg,      "-32769i32", None
        to_i16_checked_larger_width_unsigned_fits_pos,   "15u32",     15
        to_i16_checked_larger_width_unsigned_oob_pos,    "32768u32",  None
    )
    "Num.to_i32_checked", i32, (
        to_i32_checked_smaller_width_pos,                "15i8",      15
        to_i32_checked_smaller_width_neg,                "-15i8",     -15
        to_i32_checked_same,                             "15i32",     15
        to_i32_checked_same_width_unsigned_fits,         "15u32",     15
        to_i32_checked_same_width_unsigned_oob,          "2147483648u32",  None
        to_i32_checked_larger_width_signed_fits_pos,     "15i64",     15
        to_i32_checked_larger_width_signed_oob_pos,      "2147483648i64",  None
        to_i32_checked_larger_width_signed_fits_neg,     "-15i64",    -15
        to_i32_checked_larger_width_signed_oob_neg,      "-2147483649i64", None
        to_i32_checked_larger_width_unsigned_fits_pos,   "15u64",     15
        to_i32_checked_larger_width_unsigned_oob_pos,    "2147483648u64",  None
    )
    "Num.to_i64_checked", i64, (
        to_i64_checked_smaller_width_pos,                "15i8",      15
        to_i64_checked_smaller_width_neg,                "-15i8",     -15
        to_i64_checked_same,                             "15i64",     15
        to_i64_checked_same_width_unsigned_fits,         "15u64",     15
        to_i64_checked_same_width_unsigned_oob,          "9223372036854775808u64",  None
        to_i64_checked_larger_width_signed_fits_pos,     "15i128",     15
        to_i64_checked_larger_width_signed_oob_pos,      "9223372036854775808i128",  None
        to_i64_checked_larger_width_signed_fits_neg,     "-15i128",    -15
        to_i64_checked_larger_width_signed_oob_neg,      "-9223372036854775809i128", None
        to_i64_checked_larger_width_unsigned_fits_pos,   "15u128",     15
        to_i64_checked_larger_width_unsigned_oob_pos,    "9223372036854775808u128",  None
    )
    "Num.to_u8_checked", u8, (
        to_u8_checked_same,                           "15u8",   15
        to_u8_checked_same_width_signed_fits,         "15i8",   15
        to_u8_checked_same_width_signed_oob,          "-1i8",   None
        to_u8_checked_larger_width_signed_fits_pos,   "15i16",  15
        to_u8_checked_larger_width_signed_oob_pos,    "256i16", None
        to_u8_checked_larger_width_signed_oob_neg,    "-1i16",  None
        to_u8_checked_larger_width_unsigned_fits_pos, "15u16",  15
        to_u8_checked_larger_width_unsigned_oob_pos,  "256u16", None
    )
    "Num.to_u16_checked", u16, (
        to_u16_checked_smaller_width_pos,              "15i8",     15
        to_u16_checked_smaller_width_neg_oob,          "-15i8",    None
        to_u16_checked_same,                           "15u16",    15
        to_u16_checked_same_width_signed_fits,         "15i16",    15
        to_u16_checked_same_width_signed_oob,          "-1i16",    None
        to_u16_checked_larger_width_signed_fits_pos,   "15i32",    15
        to_u16_checked_larger_width_signed_oob_pos,    "65536i32", None
        to_u16_checked_larger_width_signed_oob_neg,    "-1i32",    None
        to_u16_checked_larger_width_unsigned_fits_pos, "15u32",    15
        to_u16_checked_larger_width_unsigned_oob_pos,  "65536u32", None
    )
    "Num.to_u32_checked", u32, (
        to_u32_checked_smaller_width_pos,              "15i8",     15
        to_u32_checked_smaller_width_neg_oob,          "-15i8",    None
        to_u32_checked_same,                           "15u32",    15
        to_u32_checked_same_width_signed_fits,         "15i32",    15
        to_u32_checked_same_width_signed_oob,          "-1i32",    None
        to_u32_checked_larger_width_signed_fits_pos,   "15i64",    15
        to_u32_checked_larger_width_signed_oob_pos,    "4294967296i64", None
        to_u32_checked_larger_width_signed_oob_neg,    "-1i64",    None
        to_u32_checked_larger_width_unsigned_fits_pos, "15u64",    15
        to_u32_checked_larger_width_unsigned_oob_pos,  "4294967296u64", None
    )
    "Num.to_u64_checked", u64, (
        to_u64_checked_smaller_width_pos,              "15i8",     15
        to_u64_checked_smaller_width_neg_oob,          "-15i8",    None
        to_u64_checked_same,                           "15u64",    15
        to_u64_checked_same_width_signed_fits,         "15i64",    15
        to_u64_checked_same_width_signed_oob,          "-1i64",    None
        to_u64_checked_larger_width_signed_fits_pos,   "15i128",   15
        to_u64_checked_larger_width_signed_oob_pos,    "18446744073709551616i128", None
        to_u64_checked_larger_width_signed_oob_neg,    "-1i128",   None
        to_u64_checked_larger_width_unsigned_fits_pos, "15u128",   15
        to_u64_checked_larger_width_unsigned_oob_pos,  "18446744073709551616u128", None
    )
}

fn wrap_with_default(test_roc_code: &str) -> String {
    format!("Result.with_default ({}) 123454321", test_roc_code)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_i128_checked_smaller_width_pos() {
    let test_roc_code = wrap_with_default("Num.to_i128_checked 15i8");
    assert_evals_to!(&test_roc_code, I128::from(15), I128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_i128_checked_smaller_width_neg() {
    let test_roc_code = wrap_with_default("Num.to_i128_checked -15i8");
    assert_evals_to!(&test_roc_code, I128::from(-15), I128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_i128_checked_same() {
    let test_roc_code = wrap_with_default("Num.to_i128_checked 15i128");
    assert_evals_to!(&test_roc_code, I128::from(15), I128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_i128_checked_same_width_unsigned_fits() {
    let test_roc_code = wrap_with_default("Num.to_i128_checked 15u128");
    assert_evals_to!(&test_roc_code, I128::from(15), I128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_i128_checked_same_width_unsigned_oob() {
    let test_roc_code =
        "Result.is_err (Num.to_i128_checked 170141183460469231731687303715884105728u128)";
    assert_evals_to!(&test_roc_code, true, bool)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_u128_checked_smaller_width_pos() {
    let test_roc_code = wrap_with_default("Num.to_u128_checked 15i8");
    assert_evals_to!(&test_roc_code, U128::from(15), U128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_u128_checked_smaller_width_neg_oob() {
    let test_roc_code = "Result.is_err (Num.to_u128_checked -15i8)";
    assert_evals_to!(&test_roc_code, true, bool)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_u128_checked_same() {
    let test_roc_code = wrap_with_default("Num.to_u128_checked 15u128");
    assert_evals_to!(&test_roc_code, U128::from(15), U128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_u128_checked_same_width_signed_fits() {
    let test_roc_code = wrap_with_default("Num.to_u128_checked 15i128");
    assert_evals_to!(&test_roc_code, U128::from(15), U128)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_u128_checked_same_width_signed_oob() {
    let test_roc_code = "Result.is_err (Num.to_u128_checked -1i128)";
    assert_evals_to!(&test_roc_code, true, bool)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn is_multiple_of_signed() {
    // true
    assert_evals_to!("Num.is_multiple_of 5 1", true, bool);
    assert_evals_to!("Num.is_multiple_of 5 -1", true, bool);
    assert_evals_to!("Num.is_multiple_of 0 0", true, bool);
    assert_evals_to!("Num.is_multiple_of 0 1", true, bool);
    assert_evals_to!("Num.is_multiple_of 0 -1", true, bool);
    // false
    assert_evals_to!("Num.is_multiple_of 5 2", false, bool);
    assert_evals_to!("Num.is_multiple_of 5 0", false, bool);

    // overflow
    assert_evals_to!("Num.is_multiple_of -9223372036854775808 -1", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn is_multiple_of_unsigned() {
    // true
    assert_evals_to!("Num.is_multiple_of 5u8 1", true, bool);
    assert_evals_to!("Num.is_multiple_of 0u8 0", true, bool);
    assert_evals_to!("Num.is_multiple_of 0u8 1", true, bool);
    assert_evals_to!("Num.is_multiple_of 0u8 0xFF", true, bool);

    // false
    assert_evals_to!("Num.is_multiple_of 5u8 2", false, bool);
    assert_evals_to!("Num.is_multiple_of 5u8 0", false, bool);

    // unsigned result is different from signed
    assert_evals_to!("Num.is_multiple_of 5u8 0xFF", false, bool);
    assert_evals_to!("Num.is_multiple_of 0xFCu8 0xFE", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_i32() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                x : I32
                x = 0

                main : I32
                main =
                    when x is
                        0 -> 42
                        _ -> -1
            "#
        ),
        42,
        i32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_i16() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                x : I16
                x = 0

                main : I16
                main =
                    when x is
                        0 -> 42
                        _ -> -1
            "#
        ),
        42,
        i16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str 1234", RocStr::from("1234"), RocStr);
    assert_evals_to!(r"Num.to_str 0", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str -1", RocStr::from("-1"), RocStr);

    let max = format!("{}", i64::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_i64",
        RocStr::from(max.as_str()),
        RocStr
    );

    let min = format!("{}", i64::MIN);
    assert_evals_to!(
        r"Num.to_str Num.min_i64",
        RocStr::from(min.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_u8() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str 0u8", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1u8", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10u8", RocStr::from("10"), RocStr);

    let max = format!("{}", u8::MAX);
    assert_evals_to!(r"Num.to_str Num.max_u8", RocStr::from(max.as_str()), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_u16() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str 0u16", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1u16", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10u16", RocStr::from("10"), RocStr);

    let max = format!("{}", u16::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_u16",
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_u32() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str 0u32", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1u32", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10u32", RocStr::from("10"), RocStr);

    let max = format!("{}", u32::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_u32",
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_u64() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str 0u64", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1u64", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10u64", RocStr::from("10"), RocStr);

    let max = format!("{}", u64::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_u64",
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_i8() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10i8", RocStr::from("-10"), RocStr);
    assert_evals_to!(r"Num.to_str -1i8", RocStr::from("-1"), RocStr);
    assert_evals_to!(r"Num.to_str 0i8", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1i8", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10i8", RocStr::from("10"), RocStr);

    let max = format!("{}", i8::MAX);
    assert_evals_to!(r"Num.to_str Num.max_i8", RocStr::from(max.as_str()), RocStr);

    let max = format!("{}", i8::MIN);
    assert_evals_to!(r"Num.to_str Num.min_i8", RocStr::from(max.as_str()), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_i16() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10i16", RocStr::from("-10"), RocStr);
    assert_evals_to!(r"Num.to_str -1i16", RocStr::from("-1"), RocStr);
    assert_evals_to!(r"Num.to_str 0i16", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1i16", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10i16", RocStr::from("10"), RocStr);

    let max = format!("{}", i16::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_i16",
        RocStr::from(max.as_str()),
        RocStr
    );

    let max = format!("{}", i16::MIN);
    assert_evals_to!(
        r"Num.to_str Num.min_i16",
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_i32() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10i32", RocStr::from("-10"), RocStr);
    assert_evals_to!(r"Num.to_str -1i32", RocStr::from("-1"), RocStr);
    assert_evals_to!(r"Num.to_str 0i32", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1i32", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10i32", RocStr::from("10"), RocStr);

    let max = format!("{}", i32::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_i32",
        RocStr::from(max.as_str()),
        RocStr
    );

    let max = format!("{}", i32::MIN);
    assert_evals_to!(
        r"Num.to_str Num.min_i32",
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_i64() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10i64", RocStr::from("-10"), RocStr);
    assert_evals_to!(r"Num.to_str -1i64", RocStr::from("-1"), RocStr);
    assert_evals_to!(r"Num.to_str 0i64", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1i64", RocStr::from("1"), RocStr);
    assert_evals_to!(r"Num.to_str 10i64", RocStr::from("10"), RocStr);

    let max = format!("{}", i64::MAX);
    assert_evals_to!(
        r"Num.to_str Num.max_i64",
        RocStr::from(max.as_str()),
        RocStr
    );

    let max = format!("{}", i64::MIN);
    assert_evals_to!(
        r"Num.to_str Num.min_i64",
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_f32() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10.75f32", RocStr::from("-10.75"), RocStr);
    assert_evals_to!(r"Num.to_str -1.75f32", RocStr::from("-1.75"), RocStr);
    assert_evals_to!(r"Num.to_str 0f32", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1.75f32", RocStr::from("1.75"), RocStr);
    assert_evals_to!(r"Num.to_str 10.75f32", RocStr::from("10.75"), RocStr);

    assert_evals_to!(
        r"Num.to_str Num.max_f32",
        f32::MAX,
        RocStr,
        |roc_str: RocStr| { roc_str.as_str().parse::<f32>().unwrap() }
    );

    assert_evals_to!(
        r"Num.to_str Num.min_f32",
        f32::MIN,
        RocStr,
        |roc_str: RocStr| { roc_str.as_str().parse::<f32>().unwrap() }
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_f64() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10.75f64", RocStr::from("-10.75"), RocStr);
    assert_evals_to!(r"Num.to_str -1.75f64", RocStr::from("-1.75"), RocStr);
    assert_evals_to!(r"Num.to_str 0f64", RocStr::from("0"), RocStr);
    assert_evals_to!(r"Num.to_str 1.75f64", RocStr::from("1.75"), RocStr);
    assert_evals_to!(r"Num.to_str 10.75f64", RocStr::from("10.75"), RocStr);

    assert_evals_to!(
        r"Num.to_str Num.max_f64",
        RocStr::from(f64::MAX.to_string().as_str()),
        RocStr
    );

    assert_evals_to!(
        r"Num.to_str Num.min_f64",
        RocStr::from(f64::MIN.to_string().as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_str_dec() {
    use roc_std::RocStr;

    assert_evals_to!(r"Num.to_str -10.75dec", RocStr::from("-10.75"), RocStr);
    assert_evals_to!(r"Num.to_str -1.75dec", RocStr::from("-1.75"), RocStr);
    assert_evals_to!(r"Num.to_str 0dec", RocStr::from("0.0"), RocStr);
    assert_evals_to!(r"Num.to_str 1.75dec", RocStr::from("1.75"), RocStr);
    assert_evals_to!(r"Num.to_str 10.75dec", RocStr::from("10.75"), RocStr);

    assert_evals_to!(
        r"Num.to_str 170141183460469.105727dec",
        RocStr::from("170141183460469.105727"),
        RocStr
    );

    assert_evals_to!(
        r"Num.to_str -170141183460469.105727dec",
        RocStr::from("-170141183460469.105727"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn u8_addition_greater_than_i8() {
    assert_evals_to!(
        indoc!(
            r"
            x : U8
            x = 100
            y : U8
            y = 100
            x + y
            "
        ),
        200,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn u8_sub_greater_than_i8() {
    assert_evals_to!(
        indoc!(
            r"
            x : U8
            x = 255
            y : U8
            y = 55
            x - y
            "
        ),
        200,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn u8_mul_greater_than_i8() {
    assert_evals_to!(
        indoc!(
            r"
            x : U8
            x = 40
            y : U8
            y = 5
            x * y
            "
        ),
        200,
        u8
    )
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_ints() {
    assert_evals_to!(
        indoc!(
            r"
            x = 100

            f : U8, U32 -> U64
            f = \_, _ -> 18

            f x x
            "
        ),
        18,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_floats() {
    assert_evals_to!(
        indoc!(
            r"
            x = 100.0

            f : F32, F64 -> U64
            f = \_, _ -> 18

            f x x
            "
        ),
        18,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_ints_names_dont_conflict() {
    assert_evals_to!(
        indoc!(
            r"
            f : U8 -> U64
            f = \_ -> 9
            x =
                n = 100
                f n

            y =
                n = 100
                f n

            x + y
            "
        ),
        18,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_ints_aliased() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                y = || 100
                w1 = || y()
                w2 = || y()

                f1 : U8, U32 -> U8
                f1 = \_, _ -> 1

                f2 : U32, U8 -> U8
                f2 = \_, _ -> 2

                f1 (w1 {}) (w2 {}) + f2 (w1 {}) (w2 {})
            "#
        ),
        3,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn to_float_f32() {
    assert_evals_to!(
        indoc!(
            r"
            n : U8
            n = 100

            f : F32
            f = Num.to_frac n
            f
            "
        ),
        100.,
        f32
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn to_float_f64() {
    assert_evals_to!(
        indoc!(
            r"
            n : U8
            n = 100

            f : F64
            f = Num.to_frac n
            f
            "
        ),
        100.,
        f64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
// https://github.com/roc-lang/roc/issues/2696
fn upcast_of_int_is_zext() {
    assert_evals_to!(
        indoc!(
            r"
            Num.to_u16 0b1000_0000u8
            "
        ),
        128,
        u16
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// https://github.com/roc-lang/roc/issues/2696
fn upcast_of_int_checked_is_zext() {
    assert_evals_to!(
        indoc!(
            r"
            when Num.to_u16_checked 0b1000_0000u8 is
                Ok 128u16 -> 1u8
                _ -> 0u8
            "
        ),
        1,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn modulo_of_unsigned() {
    assert_evals_to!(
        indoc!(
            r"
            0b1111_1111u8 % 64
            "
        ),
        63,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn div_of_unsigned() {
    assert_evals_to!(
        indoc!(
            r"
            0b1111_1111u8 // 2
            "
        ),
        127,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn dec_float_suffix() {
    assert_evals_to!(
        indoc!(
            r"
            123.0dec
            "
        ),
        RocDec::from_str("123.0").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn dec_no_decimal() {
    assert_evals_to!(
        indoc!(
            r"
            3dec
            "
        ),
        RocDec::from_str("3.0").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn ceiling_to_u32() {
    assert_evals_to!(
        indoc!(
            r"
            n : U32
            n = Num.ceiling 124.5f64
            n
            "
        ),
        125,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn floor_to_u32() {
    assert_evals_to!(
        indoc!(
            r"
            n : U32
            n = Num.floor 124.5f64
            n
            "
        ),
        124,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn round_to_u32() {
    assert_evals_to!(
        indoc!(
            r"
            n : U32
            n = Num.round 124.49f64
            n
            "
        ),
        124,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn promote_u64_number_layout() {
    assert_evals_to!(
        indoc!(
            r"
            9999999999999999999 + 1
            "
        ),
        10000000000000000000,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn promote_i128_number_layout() {
    assert_evals_to!(
        indoc!(
            r"
            {
                a: 18446744073709551616 + 1,
                b: -9223372036854775809 + 1,
            }
            "
        ),
        (
            I128::from(18446744073709551617),
            I128::from(-9223372036854775808)
        ),
        (I128, I128)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn promote_u128_number_layout() {
    assert_evals_to!(
        indoc!(
            r"
            170141183460469231731687303715884105728 + 1
            "
        ),
        U128::from(170141183460469231731687303715884105729),
        U128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_decimals() {
    assert_evals_to!(
        indoc!(
            r"
            when 42.42dec is
                42.42 -> 42
                0.05 -> 1
                3.14 -> 2
                _ -> 4
            "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            when 42.42dec is
                0.05 -> 1
                3.14 -> 2
                _ -> 4
            "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_i128() {
    assert_evals_to!(
        indoc!(
            r"
            when 1701411834604692317316873037158841057i128 is
                1701411834604692317316873037158841057 -> 42
                32 -> 1
                64 -> 2
                _ -> 4
            "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_u128() {
    assert_evals_to!(
        indoc!(
            r"
            when 170141183460469231731687303715884105728u128 is
                170141183460469231731687303715884105728u128 -> 42
                32 -> 1
                64 -> 2
                _ -> 4
            "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn condition_polymorphic_num_becomes_float() {
    assert_evals_to!(
        indoc!(
            r"
            x = if Bool.true then 2 else 3
            x * 5f32
            "
        ),
        10.,
        f32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_count_leading_zero_bits() {
    assert_evals_to!(r"Num.count_leading_zero_bits 0b0010_1000u8", 2, u8);
    assert_evals_to!(r"Num.count_leading_zero_bits 0b0010_1000u16", 10, u8);
    assert_evals_to!(r"Num.count_leading_zero_bits 0b0010_1000u32", 26, u8);
    assert_evals_to!(r"Num.count_leading_zero_bits 0b0010_1000u64", 58, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_count_trailing_zero_bits() {
    assert_evals_to!(r"Num.count_trailing_zero_bits 0b0010_1000u8", 3, u8);
    assert_evals_to!(r"Num.count_trailing_zero_bits 0b0010_0000u16", 5, u8);
    assert_evals_to!(r"Num.count_trailing_zero_bits 0u32", 32, u8);
    assert_evals_to!(r"Num.count_trailing_zero_bits 0b0010_1111u64", 0, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_count_one_bits() {
    assert_evals_to!(r"Num.count_one_bits 0b0010_1000u8", 2, u8);
    assert_evals_to!(r"Num.count_one_bits 0b0010_0000u16", 1, u8);
    assert_evals_to!(r"Num.count_one_bits 0u32", 0, u8);
    assert_evals_to!(r"Num.count_one_bits 0b0010_1111u64", 5, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_abs_diff_int() {
    assert_evals_to!(r"Num.abs_diff 0u8 0u8", 0, u8);
    assert_evals_to!(r"Num.abs_diff 1u8 2u8", 1, u8);
    assert_evals_to!(r"Num.abs_diff 2u8 1u8", 1, u8);
    assert_evals_to!(r"Num.abs_diff -1 1", 2, i64);
    assert_evals_to!(r"Num.abs_diff 1 -1", 2, i64);
    assert_evals_to!(r"Num.abs_diff Num.min_i64 -1", i64::MAX, i64);
}

#[test]
#[cfg(feature = "gen-llvm")]
fn num_abs_diff_large_bits() {
    assert_evals_to!(r"Num.abs_diff 0u128 0u128", U128::from(0), U128);
    assert_evals_to!(r"Num.abs_diff 1u128 2u128", U128::from(1), U128);
    assert_evals_to!(r"Num.abs_diff -1i128 1i128", I128::from(2), I128);
    assert_evals_to!(
        r"Num.abs_diff Num.min_i128 -1i128",
        I128::from(i128::MAX),
        I128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_abs_diff_float() {
    assert_evals_to!(r"Num.abs_diff 0.0f64 0.0", 0.0, f64);
    assert_evals_to!(r"Num.abs_diff 1.0f64 2.0", 1.0, f64);
    assert_evals_to!(r"Num.abs_diff 2.0f64 1.0", 1.0, f64);
    assert_evals_to!(r"Num.abs_diff -1.0f64 1.0", 2.0, f64);
    assert_evals_to!(r"Num.abs_diff 1.0f64 -1.0", 2.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer subtraction overflowed!"#)]
fn num_abs_max_overflow() {
    assert_evals_to!(r"Num.abs_diff Num.max_i64 -1", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = r#"Roc failed with message: "Integer subtraction overflowed!"#)]
fn num_abs_int_min_overflow() {
    assert_evals_to!(r"Num.abs_diff Num.min_i64 0", 0, i64);
}

#[test]
#[cfg(feature = "gen-llvm")]
#[should_panic(expected = r#"Roc failed with message: "Integer subtraction overflowed!"#)]
fn num_abs_large_bits_min_overflow() {
    assert_evals_to!(r"Num.abs_diff Num.min_i128 0", I128::from(0), I128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_abs_float_overflow() {
    assert_evals_to!("Num.abs_diff Num.max_f64 Num.min_f64", f64::INFINITY, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn bool_in_switch() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            loop : [ Continue, Break ]
            loop = Continue

            all = ||
                when loop is
                    Continue -> Bool.true
                    Break -> Bool.false

            main = all()
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn add_checked_dec() {
    assert_evals_to!(
        indoc!(
            r"
            Num.add_checked 2.0dec 4.0dec
            "
        ),
        RocResult::ok(RocDec::from(6)),
        RocResult<RocDec, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn sub_checked_dec() {
    assert_evals_to!(
        indoc!(
            r"
            Num.sub_checked 5.0dec 2.0dec
            "
        ),
        RocResult::ok(RocDec::from(3)),
        RocResult<RocDec, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mul_checked_dec() {
    assert_evals_to!(
        indoc!(
            r"
            Num.mul_checked 5.0dec 2.0dec
            "
        ),
        RocResult::ok(RocDec::from_str("10.0").unwrap()),
        RocResult<RocDec, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mul_checked_u128() {
    assert_evals_to!(
        indoc!(
            r"
            x : Result U128 [ Overflow ]
            x = Num.mul_checked 5u128 2u128

            x
            "
        ),
        RocResult::ok(U128::from(5u128 * 2u128)),
        RocResult<U128, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn sub_checked_u128() {
    assert_evals_to!(
        indoc!(
            r"
            x : Result U128 [ Overflow ]
            x = Num.sub_checked 5u128 2u128

            x
            "
        ),
        RocResult::ok(U128::from(5u128 - 2u128)),
        RocResult<U128, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn add_checked_u128() {
    assert_evals_to!(
        indoc!(
            r"
            x : Result U128 [ Overflow ]
            x = Num.add_checked 5u128 2u128

            x
            "
        ),
        RocResult::ok(U128::from(5u128 + 2u128)),
        RocResult<U128, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn num_min() {
    assert_evals_to!(r"Num.min 0 0", 0, i64);
    assert_evals_to!(r"Num.min 1 2", 1, i64);
    assert_evals_to!(r"Num.min 2 1", 1, i64);
    assert_evals_to!(r"Num.min 2 -2", -2, i64);
    assert_evals_to!(r"Num.min -2 2", -2, i64);
    assert_evals_to!(r"Num.min Num.min_i64 Num.max_i64", i64::MIN, i64);
    assert_evals_to!(r"Num.min Num.max_i64 Num.min_i64", i64::MIN, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn num_max() {
    assert_evals_to!(r"Num.max 0 0", 0, i64);
    assert_evals_to!(r"Num.max 1 2", 2, i64);
    assert_evals_to!(r"Num.max 2 1", 2, i64);
    assert_evals_to!(r"Num.max 2 -2", 2, i64);
    assert_evals_to!(r"Num.max -2 2", 2, i64);
    assert_evals_to!(r"Num.max Num.min_i64 Num.max_i64", i64::MAX, i64);
    assert_evals_to!(r"Num.max Num.max_i64 Num.min_i64", i64::MAX, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn with_decimal_point() {
    assert_evals_to!(
        r"Num.with_decimal_point 0",
        RocDec::from_str("0").unwrap(),
        RocDec
    );
    assert_evals_to!(
        r"Num.with_decimal_point 123000000000000000000",
        RocDec::from_str("123.0").unwrap(),
        RocDec
    );
    assert_evals_to!(
        r"Num.with_decimal_point Num.max_i128",
        RocDec::from_str("170141183460469231731.687303715884105727").unwrap(),
        RocDec
    );
    assert_evals_to!(
        r"Num.with_decimal_point Num.min_i128",
        RocDec::from_str("-170141183460469231731.687303715884105728").unwrap(),
        RocDec
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn without_decimal_point() {
    assert_evals_to!(
        r"Num.without_decimal_point 0",
        RocDec::from_str("0").unwrap(),
        RocDec
    );
    assert_evals_to!(
        r"Num.without_decimal_point 123.000000000000000000",
        I128::from(123000000000000000000),
        I128
    );
    assert_evals_to!(
        r"Num.without_decimal_point 170141183460469231731.687303715884105727",
        I128::from(i128::MAX),
        I128
    );
    assert_evals_to!(
        r"Num.without_decimal_point -170141183460469231731.687303715884105728",
        I128::from(i128::MIN),
        I128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f32_to_parts() {
    assert_evals_to!(r"Num.f32_to_parts 0", (0, 0, false), (u32, u8, bool));
    assert_evals_to!(
        r"Num.f32_to_parts Num.max_f32",
        (0x7FFFFF, 0xFE, false),
        (u32, u8, bool)
    );
    assert_evals_to!(
        r"Num.f32_to_parts Num.min_f32",
        (0x7FFFFF, 0xFE, true),
        (u32, u8, bool)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_to_parts() {
    assert_evals_to!(r"Num.f64_to_parts 0", (0, 0, false), (u64, u16, bool));
    assert_evals_to!(
        r"Num.f64_to_parts Num.max_f64",
        (0xFFFFFFFFFFFFF, 0x7FE, false),
        (u64, u16, bool)
    );
    assert_evals_to!(
        r"Num.f64_to_parts Num.min_f64",
        (0xFFFFFFFFFFFFF, 0x7FE, true),
        (u64, u16, bool)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f32_from_parts() {
    assert_evals_to!(
        r"
    Num.f32_from_parts {
        sign: Bool.false,
        exponent: 0,
        fraction: 0
    }",
        0.0,
        f32
    );
    assert_evals_to!(
        r"
    Num.f32_from_parts {
        sign: Bool.false,
        exponent: 0xFE,
        fraction: 0x7FFFFF
    }",
        f32::MAX,
        f32
    );
    assert_evals_to!(
        r"
    Num.f32_from_parts {
        sign: Bool.true,
        exponent: 0xFE,
        fraction: 0x7FFFFF
    }",
        f32::MIN,
        f32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_from_parts() {
    assert_evals_to!(
        r"
    Num.f64_from_parts {
        sign: Bool.false,
        exponent: 0,
        fraction: 0
    }",
        0.0,
        f64
    );
    assert_evals_to!(
        r"
    Num.f64_from_parts {
        sign: Bool.false,
        exponent: 0x7FE,
        fraction: 0xFFFFFFFFFFFFF
    }",
        f64::MAX,
        f64
    );
    assert_evals_to!(
        r"
    Num.f64_from_parts {
        sign: Bool.true,
        exponent: 0x7FE,
        fraction: 0xFFFFFFFFFFFFF
    }",
        f64::MIN,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn from_bool_true() {
    assert_evals_to!(
        indoc!(
            r"
            bool : Bool
            bool = Bool.true

            Num.from_bool bool
            "
        ),
        1,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn from_bool_false() {
    assert_evals_to!(
        indoc!(
            r"
            bool : Bool
            bool = Bool.false

            Num.from_bool bool
            "
        ),
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn nan_f32() {
    assert_evals_to!(r"Num.nan_f32", true, f32, |f: f32| f.is_nan());
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn nan_f64() {
    assert_evals_to!(r"Num.nan_f64", true, f64, |f: f64| f.is_nan());
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn infinity_f32() {
    assert_evals_to!(r"Num.infinity_f32", f32::INFINITY, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn infinity_f64() {
    assert_evals_to!(r"Num.infinity_f64", f64::INFINITY, f64);
}
#[allow(clippy::non_minimal_cfg)]
#[test]
#[cfg(any(feature = "gen-llvm"))]
fn cast_signed_unsigned() {
    assert_evals_to!(r"Num.to_i16 255u8", 255, i16);
    assert_evals_to!(r"Num.to_u16 127i8", 127, u16);
    assert_evals_to!(r"Num.to_u8 127i8", 127, u8);
    assert_evals_to!(r"Num.to_i8 127u8", 127, i8);
}
