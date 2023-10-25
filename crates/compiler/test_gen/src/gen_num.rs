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
use roc_std::{RocDec, RocOrder, RocResult};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn nat_alias() {
    assert_evals_to!(
        indoc!(
            r#"
            i : Num.Nat
            i = 1

            i
            "#
        ),
        1,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn i128_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r#"
            i : I128
            i = 128

            i
            "#
        ),
        128,
        i128
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
            r#"
            i : I32
            i = 32

            i
            "#
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
            r#"
                    i : I16
                    i = 16

                    i
                "#
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
            r#"
                    i : I8
                    i = 8

                    i
                "#
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
            r#"
                    f : I128
                    f = 0x123

                    f
                "#
        ),
        0x123,
        i128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i64_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r#"
                    f : I64
                    f = 0x123

                    f
                "#
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
            r#"
                    f : I32
                    f = 0x123

                    f
                "#
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
            r#"
                    f : I16
                    f = 0x123

                    f
                "#
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
            r#"
                    f : I8
                    f = 0xA

                    f
                "#
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
            r#"
                    i : U128
                    i = 128

                    i
                "#
        ),
        128,
        u128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u64_signed_int_alias() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : U64
                    i = 64

                    i
                "#
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
            r#"
                    i : U32
                    i = 32

                    i
                "#
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
            r#"
                    i : U16
                    i = 16

                    i
                "#
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
            r#"
                    i : U8
                    i = 8

                    i
                "#
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
            r#"
                    f : U128
                    f = 0x123

                    f
                "#
        ),
        0x123,
        i128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn u64_hex_int_alias() {
    assert_evals_to!(
        indoc!(
            r#"
                    f : U64
                    f = 0x123

                    f
                "#
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
            r#"
                    f : U32
                    f = 0x123

                    f
                "#
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
            r#"
                    f : U16
                    f = 0x123

                    f
                "#
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
            r#"
                    f : U8
                    f = 0xA

                    f
                "#
        ),
        0xA,
        u8
    );
}

#[test]
fn character_literal() {
    assert_evals_to!(
        indoc!(
            r#"
                    x = 'A'

                    x
                "#
        ),
        65,
        i64
    );
}

#[test]
fn character_literal_back_slash() {
    assert_evals_to!(
        indoc!(
            r#"
                    x = '\\'

                    x
                "#
        ),
        92,
        i64
    );
}

#[test]
fn character_literal_single_quote() {
    assert_evals_to!(
        indoc!(
            r#"
                    x = '\''

                    x
                "#
        ),
        39,
        i64
    );
}

#[test]
fn character_literal_new_line() {
    assert_evals_to!(
        indoc!(
            r#"
                    x = '\n'

                    x
                "#
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
            r#"
                    x : Dec
                    x = 2.1

                    x
                "#
        ),
        RocDec::from_str_to_i128_unsafe("2.1"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_float_alias() {
    assert_evals_to!(
        indoc!(
            r#"
                    f : F64
                    f = 3.6

                    f
                "#
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
            r#"
                    f : F32
                    f = 3.6

                    f
                "#
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
    assert_evals_to!("Num.sqrtChecked 100f64", RocResult::ok(10.0), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_sqrt_checked_negative() {
    assert_evals_to!("Num.sqrtChecked -1f64", RocResult::err(()), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_log() {
    assert_evals_to!("Num.log 7.38905609893f64", 1.999999999999912, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_log_checked_one() {
    assert_evals_to!("Num.logChecked 1f64", RocResult::ok(0.0), RocResult<f64, ()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_log_checked_zero() {
    assert_evals_to!("Num.logChecked 0f64", RocResult::err(()), RocResult<f64, ()>);
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

    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    {
        assert_evals_to!("Num.abs Num.maxF64", f64::MAX, f64);
        assert_evals_to!("Num.abs Num.minF64", f64::MAX, f64);
    }
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
    assert_evals_to!("Num.abs Num.maxI64", i64::MAX, i64);
    assert_evals_to!("Num.abs (Num.minI64 + 1)", -(i64::MIN + 1), i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn various_sized_abs() {
    assert_evals_to!("Num.abs -6i8", 6, i8);
    assert_evals_to!("Num.abs -6i16", 6, i16);
    assert_evals_to!("Num.abs -6i32", 6, i32);
    assert_evals_to!("Num.abs -6i64", 6, i64);
    if !cfg!(feature = "gen-wasm") {
        assert_evals_to!("Num.abs -6i128", 6, i128);
    }
    assert_evals_to!("Num.abs 6u8", 6, u8);
    assert_evals_to!("Num.abs 6u16", 6, u16);
    assert_evals_to!("Num.abs 6u32", 6, u32);
    assert_evals_to!("Num.abs 6u64", 6, u64);
    if !cfg!(feature = "gen-wasm") {
        assert_evals_to!("Num.abs 6u128", 6, u128);
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(
    expected = r#"Roc failed with message: "integer absolute overflowed because its argument is the minimum value"#
)]
fn abs_min_int_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                Num.abs Num.minI64
                "#
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
            r#"
                    limitedNegate = \num ->
                        x =
                            if num == 1 then
                                -1
                            else if num == -1 then
                                1
                            else
                                num
                        x

                    limitedNegate 1
                "#
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
            r#"
                    1.0 == 1.0
                "#
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
            r#"
                    x : Dec
                    x = 2.1

                    y : Dec
                    y = 3.1

                    z : Dec
                    z = x + y

                    z
                "#
        ),
        RocDec::from_str_to_i128_unsafe("5.2"),
        i128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_f32() {
    assert_evals_to!(
        indoc!(
            r#"
                    1.1f32 + 2.4f32 + 3
                "#
        ),
        6.5,
        f32
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_f64() {
    assert_evals_to!(
        indoc!(
            r#"
                    1.1f64 + 2.4 + 3
                "#
        ),
        6.5,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_wrap_add_nums() {
    assert_evals_to!(
        indoc!(
            r#"
                    add2 = \num1, num2 -> num1 + num2

                    add2 4 5
                "#
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
            r#"
                    when Num.divChecked 48 2f64 is
                        Ok val -> val
                        Err _ -> -1
                "#
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
            r#"
                    when Num.divChecked 47 0f64 is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        -1.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_dec() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 10

                    y : Dec
                    y = 3

                    x / y
                "#
        ),
        RocDec::from_str_to_i128_unsafe("3.333333333333333333"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_checked_dec() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 10

                    y : Dec
                    y = 3

                    when Num.divChecked x y is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        RocDec::from_str_to_i128_unsafe("3.333333333333333333"),
        i128
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_checked_by_zero_dec() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 10

                    y : Dec
                    y = 0

                    when Num.divChecked x y is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        RocDec::from_str_to_i128_unsafe("-1"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_int_eq() {
    assert_evals_to!(
        indoc!(
            r#"
                    4 == 4
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_int_neq() {
    assert_evals_to!(
        indoc!(
            r#"
                    4 != 5
                "#
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_dec_eq() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 4

                    y : Dec
                    y = 4

                    x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_dec_neq() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 4

                    y : Dec
                    y = 5

                    x != y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_wrap_int_neq() {
    assert_evals_to!(
        indoc!(
            r#"
                    wrappedNotEq : a, a -> Bool where a implements Eq
                    wrappedNotEq = \num1, num2 ->
                        num1 != num2

                    wrappedNotEq 2 3
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_i8() {
    assert_evals_to!(
        indoc!(
            r#"
                    1i8 + 2i8 + 3i8
                "#
        ),
        6,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_u8() {
    assert_evals_to!(
        indoc!(
            r#"
                    1u8 + 2u8 + 3u8
                "#
        ),
        6,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_i16() {
    assert_evals_to!(
        indoc!(
            r#"
                    1i16 + 2i16 + 3i16
                "#
        ),
        6,
        i16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_u16() {
    assert_evals_to!(
        indoc!(
            r#"
                    1u16 + 2u16 + 3u16
                "#
        ),
        6,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_i32() {
    assert_evals_to!(
        indoc!(
            r#"
                    1i32 + 2i32 + 3i32
                "#
        ),
        6,
        i32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_u32() {
    assert_evals_to!(
        indoc!(
            r#"
                    1u32 + 2u32 + 3u32
                "#
        ),
        6,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_add_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                    1 + 2 + 3
                "#
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_sub_dec() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 1.5

                    y : Dec
                    y = 2.4

                    z : Dec
                    z = 3

                    (x - y) - z
                "#
        ),
        RocDec::from_str_to_i128_unsafe("-3.9"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_mul_dec() {
    assert_evals_to!(
        indoc!(
            r#"
                    x : Dec
                    x = 2

                    y : Dec
                    y = 4

                    z : Dec
                    z = 6

                    x * y * z
                "#
        ),
        RocDec::from_str_to_i128_unsafe("48.0"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_sub_f64() {
    assert_evals_to!("1.5f64 - 2.4 - 3", -3.9, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_sub_f32() {
    assert_evals_to!("1.5f32 - 2.4 - 3", -3.9, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_i8() {
    assert_evals_to!("1i8 - 2i8 - 3i8", -4, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_u8() {
    assert_evals_to!("8u8 - 2u8 - 3u8", 3, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_i16() {
    assert_evals_to!("1i16 - 2i16 - 3i16", -4, i16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_u16() {
    assert_evals_to!("8u16 - 2u16 - 3u16", 3, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_i32() {
    assert_evals_to!("1i32 - 2i32 - 3i32", -4, i32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_u32() {
    assert_evals_to!("8u32 - 2u32 - 3u32", 3, u32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_sub_i64() {
    assert_evals_to!("1 - 2 - 3", -4, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_signed_mul_quadword_and_lower() {
    assert_evals_to!("2i64 * 4 * 6", 48, i64);
    assert_evals_to!("2i32 * 4 * 6", 48, i32);
    assert_evals_to!("2i16 * 4 * 6", 48, i16);
    assert_evals_to!("2i8 * 4 * 6", 48, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_unsigned_mul_quadword_and_lower() {
    assert_evals_to!("2u64 * 4 * 6", 48, u64);
    assert_evals_to!("2u32 * 4 * 6", 48, u32);
    assert_evals_to!("2u16 * 4 * 6", 48, u16);
    assert_evals_to!("2u8 * 4 * 6", 48, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_mul_f64() {
    assert_evals_to!("2f64 * 4 * 6", 48.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_checked_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Num.divTruncChecked 1000 10 is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        100,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_div_checked_by_zero_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Num.divTruncChecked 1000 0 is
                        Err DivByZero -> 99
                        _ -> -24
                "#
        ),
        99,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_rem_i64() {
    assert_evals_to!("Num.rem 8 3", 2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_rem_checked_div_by_zero_i64() {
    assert_evals_to!(
        indoc!(
            r#"
            when Num.remChecked 8 0 is
                Err DivByZero -> 4
                Ok _ -> -23
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_positive_i64() {
    assert_evals_to!("Num.isPositive 0", false, bool);
    assert_evals_to!("Num.isPositive 1", true, bool);
    assert_evals_to!("Num.isPositive -5", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_negative_i64() {
    assert_evals_to!("Num.isNegative 0", false, bool);
    assert_evals_to!("Num.isNegative 3", false, bool);
    assert_evals_to!("Num.isNegative -2", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_positive_f64() {
    assert_evals_to!("Num.isPositive 0.0", false, bool);
    assert_evals_to!("Num.isPositive 4.7", true, bool);
    assert_evals_to!("Num.isPositive -8.5", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_negative_f64() {
    assert_evals_to!("Num.isNegative 0.0", false, bool);
    assert_evals_to!("Num.isNegative 9.9", false, bool);
    assert_evals_to!("Num.isNegative -4.4", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_is_zero_i64() {
    assert_evals_to!("Num.isZero 0", true, bool);
    assert_evals_to!("Num.isZero 0_0", true, bool);
    assert_evals_to!("Num.isZero 1", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_zero_f64() {
    assert_evals_to!("Num.isZero 0.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_odd() {
    assert_evals_to!("Num.isOdd 4", false, bool);
    assert_evals_to!("Num.isOdd 5", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_is_even() {
    assert_evals_to!("Num.isEven 6", true, bool);
    assert_evals_to!("Num.isEven 7", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
    assert_evals_to!("Num.bitwiseAnd 20 20", 20, i64);
    assert_evals_to!("Num.bitwiseAnd 25 10", 8, i64);
    assert_evals_to!("Num.bitwiseAnd 200 0", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bitwise_xor() {
    assert_evals_to!("Num.bitwiseXor 20 20", 0, i64);
    assert_evals_to!("Num.bitwiseXor 15 14", 1, i64);
    assert_evals_to!("Num.bitwiseXor 7 15", 8, i64);
    assert_evals_to!("Num.bitwiseXor 200 0", 200, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bitwise_or() {
    assert_evals_to!("Num.bitwiseOr 1 1", 1, i64);
    assert_evals_to!("Num.bitwiseOr 1 2", 3, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn lt_u8() {
    assert_evals_to!("1u8 < 2u8", true, bool);
    assert_evals_to!("1u8 < 1u8", false, bool);
    assert_evals_to!("2u8 < 1u8", false, bool);
    assert_evals_to!("0u8 < 0u8", false, bool);
    assert_evals_to!("128u8 < 0u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn lte_u8() {
    assert_evals_to!("1u8 <= 1u8", true, bool);
    assert_evals_to!("2u8 <= 1u8", false, bool);
    assert_evals_to!("1u8 <= 2u8", true, bool);
    assert_evals_to!("0u8 <= 0u8", true, bool);
    assert_evals_to!("128u8 <= 0u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gt_u8() {
    assert_evals_to!("2u8 > 1u8", true, bool);
    assert_evals_to!("2u8 > 2u8", false, bool);
    assert_evals_to!("1u8 > 1u8", false, bool);
    assert_evals_to!("0u8 > 0u8", false, bool);
    assert_evals_to!("0u8 > 128u8", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gte_i64() {
    assert_evals_to!("1 >= 1", true, bool);
    assert_evals_to!("1 >= 2", false, bool);
    assert_evals_to!("2 >= 1", true, bool);
    assert_evals_to!("0 >= 0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn lt_f64() {
    assert_evals_to!("1.1f64 < 1.2", true, bool);
    assert_evals_to!("1.1f64 < 1.1", false, bool);
    assert_evals_to!("1.2f64 < 1.1", false, bool);
    assert_evals_to!("0.0f64 < 0.0", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn lte_f64() {
    assert_evals_to!("1.1f64 <= 1.1", true, bool);
    assert_evals_to!("1.2f64 <= 1.1", false, bool);
    assert_evals_to!("1.1f64 <= 1.2", true, bool);
    assert_evals_to!("0.0f64 <= 0.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gt_f64() {
    assert_evals_to!("2.2f64 > 1.1", true, bool);
    assert_evals_to!("2.2f64 > 2.2", false, bool);
    assert_evals_to!("1.1f64 > 2.2", false, bool);
    assert_evals_to!("0.0f64 > 0.0", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gte_f64() {
    assert_evals_to!("1.1f64 >= 1.1", true, bool);
    assert_evals_to!("1.1f64 >= 1.2", false, bool);
    assert_evals_to!("1.2f64 >= 1.1", true, bool);
    assert_evals_to!("0.0f64 >= 0.0", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_order_of_arithmetic_ops() {
    assert_evals_to!(
        indoc!(
            r#"
                    1 + 3 * 7 - 2
                "#
        ),
        20,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_order_of_arithmetic_ops_complex_float() {
    assert_evals_to!(
        indoc!(
            r#"
                    3 - 48 * 2.0f64
                "#
        ),
        -93.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn if_guard_bind_variable_false() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    when 10 is
                        x if x == 5 -> 0
                        _ -> 42

                wrapper {}
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn if_guard_bind_variable_true() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapper = \{} ->
                    when 10 is
                        x if x == 10 -> 42
                        _ -> 0

                wrapper {}
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn tail_call_elimination() {
    assert_evals_to!(
        indoc!(
            r#"
                    sum = \n, accum ->
                        when n is
                            0 -> accum
                            _ -> sum (n - 1) (n + accum)

                    sum 1_000_000 0
                "#
        ),
        500000500000,
        i64
    );
}

#[test]
#[cfg(feature = "gen-dev")]
fn int_negate_dev() {
    // TODO
    // dev backend yet to have `Num.maxI64` or `Num.minI64`.
    // add the "gen-dev" feature to the test below after implementing them both.
    assert_evals_to!("Num.neg 123", -123, i64);
    assert_evals_to!("Num.neg -123", 123, i64);
    assert_evals_to!("Num.neg 0", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn int_negate() {
    assert_evals_to!("Num.neg 123", -123, i64);
    assert_evals_to!("Num.neg Num.maxI64", -i64::MAX, i64);
    assert_evals_to!("Num.neg (Num.minI64 + 1)", i64::MAX, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(
    expected = r#"Roc failed with message: "integer negation overflowed because its argument is the minimum value"#
)]
fn neg_min_int_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                Num.neg Num.minI64
                "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_wrap_int_neg() {
    assert_evals_to!(
        indoc!(
            r#"
                    wrappedNeg = \num -> -num

                    wrappedNeg 3
                "#
        ),
        -3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn gen_basic_fn() {
    assert_evals_to!(
        indoc!(
            r#"
                    always42 : Num.Num (Num.Integer Num.Signed64) -> Num.Num (Num.Integer Num.Signed64)
                    always42 = \_ -> 42

                    always42 5
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_to_float() {
    assert_evals_to!("Num.toFrac 0x9", RocDec::from(9i32), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn num_to_frac() {
    assert_evals_to!("Num.toFrac 9", RocDec::from(9i32), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn num_to_frac_f64_to_f32() {
    assert_evals_to!(
        indoc!(
            r#"
                    f64 : F64
                    f64 = 9.0

                    f32 : F32
                    f32 = Num.toFrac f64
                    f32
                "#
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
            r#"

                    arg : F32
                    arg = 9.0

                    ret : F32
                    ret = Num.toFrac arg
                    ret
                "#
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
            r#"

                    arg : F64
                    arg = 9.0

                    ret : F64
                    ret = Num.toFrac arg
                    ret
                "#
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
            r#"

                    f32 : F32
                    f32 = 9.0

                    f64 : F64
                    f64 = Num.toFrac f32
                    f64
                "#
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
            r#"
            x : F64
            x = Num.toFrac 0.5f64

            x
            "#
        ),
        0.5,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn frac_is_nan() {
    assert_evals_to!("Num.isNaN (0 / 0f64)", true, bool);
    assert_evals_to!("Num.isNaN (1 / 0f64)", false, bool);
    assert_evals_to!("Num.isNaN 42f64", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn frac_is_infinite() {
    assert_evals_to!("Num.isInfinite (1 / 0f64)", true, bool);
    assert_evals_to!("Num.isInfinite (-1 / 0f64)", true, bool);
    assert_evals_to!("Num.isInfinite (0 / 0f64)", false, bool);
    assert_evals_to!("Num.isInfinite 42f64", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn frac_is_finite() {
    assert_evals_to!("Num.isFinite 42f64", true, bool);
    assert_evals_to!("Num.isFinite (1 / 0f64)", false, bool);
    assert_evals_to!("Num.isFinite (0 / 0f64)", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn int_compare() {
    assert_evals_to!("Num.compare 0 1", RocOrder::Lt, RocOrder);
    assert_evals_to!("Num.compare 1 1", RocOrder::Eq, RocOrder);
    assert_evals_to!("Num.compare 1 0", RocOrder::Gt, RocOrder);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_compare() {
    assert_evals_to!("Num.compare 0.01 3.14", RocOrder::Lt, RocOrder);
    assert_evals_to!("Num.compare 3.14 3.14", RocOrder::Eq, RocOrder);
    assert_evals_to!("Num.compare 3.14 0.01", RocOrder::Gt, RocOrder);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn pow() {
    assert_evals_to!("Num.pow 2.0f64 2.0f64", 4.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ceiling() {
    assert_evals_to!("Num.ceiling 1.1f64", 2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn floor() {
    assert_evals_to!("Num.floor 1.9f64", 1, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn pow_int() {
    assert_evals_to!("Num.powInt 2 3", 8, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn atan() {
    assert_evals_to!("Num.atan 10f64", 1.4711276743037347, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer addition overflowed!"#)]
fn int_add_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                9_223_372_036_854_775_807 + 1
                "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_add_checked_ok() {
    assert_evals_to!(
        "Num.addChecked 1 2",
        RocResult::ok(3),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_add_checked_err() {
    assert_evals_to!(
        "Num.addChecked 9_223_372_036_854_775_807 1",
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_add_wrap() {
    assert_evals_to!(
        "Num.addWrap 9_223_372_036_854_775_807 1",
        std::i64::MIN,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_add_checked_pass() {
    assert_evals_to!(
        "Num.addChecked 1.0 0.0f64",
        RocResult::ok(1.0),
        RocResult<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_add_checked_fail() {
    assert_evals_to!(
        "Num.addChecked 1.7976931348623157e308f64 1.7976931348623157e308",
        RocResult::err(()),
        RocResult<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn float_add_overflow() {
    assert_evals_to!(
        "1.7976931348623157e308f64 + 1.7976931348623157e308",
        f64::INFINITY,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer subtraction overflowed!"#)]
fn int_sub_overflow() {
    assert_evals_to!("-9_223_372_036_854_775_808 - 1", 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn int_sub_wrap() {
    assert_evals_to!(
        "Num.subWrap -9_223_372_036_854_775_808 1",
        std::i64::MAX,
        i64
    );

    assert_evals_to!("Num.subWrap -128i8 1", std::i8::MAX, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_sub_overflow() {
    assert_evals_to!(
        "-1.7976931348623157e308f64 - 1.7976931348623157e308",
        -f64::INFINITY,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn int_sub_checked() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.subChecked 5 2 is
                    Ok v -> v
                    _ -> -1
                "#
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                when Num.subChecked Num.minI64 1 is
                    Err Overflow -> -1
                    Ok v -> v
                "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_sub_checked() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.subChecked 1.0 0.0f64 is
                    Ok v -> v
                    Err Overflow -> -1.0
                "#
        ),
        1.0,
        f64
    );

    assert_evals_to!(
        indoc!(
            r#"
                when Num.subChecked -1.7976931348623157e308f64 1.7976931348623157e308 is
                    Err Overflow -> -1
                    Ok v -> v
                "#
        ),
        -1.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer multiplication overflowed!"#)]
fn int_positive_mul_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                9_223_372_036_854_775_807 * 2
                "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer multiplication overflowed!"#)]
fn int_negative_mul_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                (-9_223_372_036_854_775_808) * 2
                "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_positive_mul_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                    1.7976931348623157e308f64 * 2
                "#
        ),
        f64::INFINITY,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_negative_mul_overflow() {
    assert_evals_to!(
        indoc!(
            r#"
                    -1.7976931348623157e308f64 * 2
                "#
        ),
        -f64::INFINITY,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_mul_wrap_i64() {
    assert_evals_to!("Num.mulWrap Num.maxI64 2", -2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_mul_wrap_i128() {
    assert_evals_to!("Num.mulWrap Num.maxI128 2", -2, i128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_mul_checked() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.mulChecked 20 2 is
                    Ok v -> v
                    _ -> -1
                "#
        ),
        40,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                when Num.mulChecked Num.maxI64 2 is
                    Err Overflow -> -1
                    Ok v -> v
                "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn float_mul_checked() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.mulChecked 20.0 2.0f64 is
                    Ok v -> v
                    Err Overflow -> -1.0
                "#
        ),
        40.0,
        f64
    );

    assert_evals_to!(
        indoc!(
            r#"
                when Num.mulChecked 1.7976931348623157e308f64 2 is
                    Err Overflow -> -1
                    Ok v -> v
                "#
        ),
        -1.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_left_by() {
    assert_evals_to!("Num.shiftLeftBy 0b0000_0001 0", 0b0000_0001, i64);
    assert_evals_to!("Num.shiftLeftBy 0b0000_0001 1", 0b0000_0010, i64);
    assert_evals_to!("Num.shiftLeftBy 0b0000_0011 2", 0b0000_1100, i64);
    assert_evals_to!("Num.shiftLeftBy 2u16 2", 8, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_right_by() {
    // Sign Extended Right Shift

    let is_llvm_release_mode = cfg!(feature = "gen-llvm") && !cfg!(debug_assertions);

    assert_evals_to!("Num.shiftRightBy 0b0100_0000i8 2", 0b0001_0000i8, i8);
    assert_evals_to!("Num.shiftRightBy 0b1110_0000u8 1", 0b1111_0000u8, u8);
    assert_evals_to!("Num.shiftRightBy 0b1100_0000u8 2", 0b1111_0000u8, u8);
    assert_evals_to!("Num.shiftRightBy 0b0100_0000u8 12", 0b0000_0000u8, u8);

    // LLVM in release mode returns 0 instead of -1 for some reason
    if !is_llvm_release_mode {
        assert_evals_to!("Num.shiftRightBy 0b1000_0000u8 12", 0b1111_1111u8, u8);
    }
    assert_evals_to!("Num.shiftRightBy 12 0", 12, i64);
    assert_evals_to!("Num.shiftRightBy 12 1", 6, i64);
    assert_evals_to!("Num.shiftRightBy -12 1", -6, i64);
    assert_evals_to!("Num.shiftRightBy 12 8", 0, i64);
    assert_evals_to!("Num.shiftRightBy -12 8", -1, i64);
    assert_evals_to!("Num.shiftRightBy 0 0", 0, i64);
    assert_evals_to!("Num.shiftRightBy 0 1", 0, i64);

    assert_evals_to!("Num.shiftRightBy 12i32 0", 12, i32);
    assert_evals_to!("Num.shiftRightBy 12i32 1", 6, i32);
    assert_evals_to!("Num.shiftRightBy -12i32 1", -6, i32);
    assert_evals_to!("Num.shiftRightBy 12i32 8", 0, i32);
    assert_evals_to!("Num.shiftRightBy -12i32 8", -1, i32);

    assert_evals_to!("Num.shiftRightBy 12i8 0", 12, i8);
    assert_evals_to!("Num.shiftRightBy 12i8 1", 6, i8);
    assert_evals_to!("Num.shiftRightBy -12i8 1", -6, i8);
    assert_evals_to!("Num.shiftRightBy 12i8 8", 0, i8);

    if !is_llvm_release_mode {
        assert_evals_to!("Num.shiftRightBy -12i8 8", -1, i8);
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_right_zf_by() {
    // Logical Right Shift
    assert_evals_to!("Num.shiftRightZfBy 0b1100_0000u8 2", 0b0011_0000u8, u8);
    assert_evals_to!("Num.shiftRightZfBy 0b0000_0010u8 1", 0b0000_0001u8, u8);
    assert_evals_to!("Num.shiftRightZfBy 0b0000_1100u8 2", 0b0000_0011u8, u8);
    assert_evals_to!("Num.shiftRightZfBy 0b1000_0000u8 12", 0b0000_0000u8, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shift_right_cast_i8() {
    // FIXME (Brian) Something funny happening with 8-bit binary literals in tests

    // arithmetic
    assert_evals_to!(
        "Num.shiftRightBy (Num.toI8 0b1100_0000u8) 2",
        0b1111_0000u8 as i8,
        i8
    );

    // logical
    assert_evals_to!(
        "Num.shiftRightZfBy (Num.toI8 0b1100_0000u8) 2",
        0b0011_0000i8,
        i8
    );
    assert_evals_to!("Num.shiftRightZfBy 0b1100_0000u8 2", 0b0011_0000u8, u8);
    assert_evals_to!("Num.shiftRightZfBy 0b0000_0010u8 1", 0b0000_0001u8, u8);
    assert_evals_to!("Num.shiftRightZfBy 0b0000_1100u8 2", 0b0000_0011u8, u8);
    assert_evals_to!("Num.shiftRightZfBy 0b1000_0000u8 12", 0b0000_0000u8, u8);
    assert_evals_to!(
        "Num.shiftRightZfBy 0xffff_0000_0000_0000_0000_0000_0000_ffffu128 4",
        0x0fff_f000_0000_0000_0000_0000_0000_0fffu128,
        u128
    );
    assert_evals_to!(
        "Num.shiftRightZfBy 0xaaaa_0000_0000_bbbb_ffff_ffff_ffff_ffffu128 68",
        0x0000_0000_0000_0000_0aaa_a000_0000_0bbbu128,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn min_i128() {
    assert_evals_to!("Num.minI128", i128::MIN, i128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn max_i128() {
    assert_evals_to!("Num.maxI128", i128::MAX, i128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_i64() {
    assert_evals_to!("Num.minI64", i64::MIN, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_i64() {
    assert_evals_to!("Num.maxI64", i64::MAX, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_u64() {
    assert_evals_to!("Num.minU64", u64::MIN, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_u64() {
    assert_evals_to!("Num.maxU64", u64::MAX, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_i32() {
    assert_evals_to!("Num.minI32", i32::MIN, i32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_i32() {
    assert_evals_to!("Num.maxI32", i32::MAX, i32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_u32() {
    assert_evals_to!("Num.minU32", u32::MIN, u32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_u32() {
    assert_evals_to!("Num.maxU32", u32::MAX, u32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_i16() {
    assert_evals_to!("Num.minI16", i16::MIN, i16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_i16() {
    assert_evals_to!("Num.maxI16", i16::MAX, i16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_u16() {
    assert_evals_to!("Num.minU16", u16::MIN, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_u16() {
    assert_evals_to!("Num.maxU16", u16::MAX, u16);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_i8() {
    assert_evals_to!("Num.minI8", i8::MIN, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_i8() {
    assert_evals_to!("Num.maxI8", i8::MAX, i8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_u8() {
    assert_evals_to!("Num.minU8", u8::MIN, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_u8() {
    assert_evals_to!("Num.maxU8", u8::MAX, u8);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_f64() {
    assert_evals_to!("Num.maxF64", f64::MAX, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_f64() {
    assert_evals_to!("Num.minF64", f64::MIN, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn max_f32() {
    assert_evals_to!("Num.maxF32", f32::MAX, f32);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn min_f32() {
    assert_evals_to!("Num.minF32", f32::MIN, f32);
}

#[test]
#[cfg(all(feature = "gen-llvm", not(feature = "gen-llvm-wasm")))]
fn to_nat_truncate_wraps() {
    let input = "Num.toNat 10_000_000_000_000_000_000_000i128";
    assert_evals_to!(input, 1864712049423024128, u64)
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
    "Num.toI8", i8, (
        to_i8_same_width, "15u8", 15, ["gen-wasm", "gen-dev"]
        to_i8_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_i8_truncate_wraps, "500i32", -12, ["gen-wasm", "gen-dev"]
    )
    "Num.toI16", i16, (
        to_i16_same_width, "15u16", 15, ["gen-wasm", "gen-dev"]
        to_i16_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_i16_sign_extend_i8, "-15i8", -15, ["gen-wasm", "gen-dev"]
        to_i16_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_i16_truncate_wraps, "60000i32", -5536, ["gen-wasm", "gen-dev"]
    )
    "Num.toI32", i32, (
        to_i32_same_width, "15u32", 15, ["gen-wasm", "gen-dev"]
        to_i32_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_i32_sign_extend_i8, "-15i8", -15, ["gen-wasm", "gen-dev"]
        to_i32_sign_extend_i16, "-15i16", -15, ["gen-wasm", "gen-dev"]
        to_i32_truncate, "115i64", 115, ["gen-wasm", "gen-dev"]
        to_i32_truncate_wraps, "5000000000i64", 705032704, ["gen-wasm", "gen-dev"]
    )
    "Num.toI64", i64, (
        to_i64_same_width, "15u64", 15, ["gen-wasm", "gen-dev"]
        to_i64_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_i64_sign_extend_i8, "-15i8", -15, ["gen-wasm", "gen-dev"]
        to_i64_sign_extend_i16, "-15i16", -15, ["gen-wasm", "gen-dev"]
        to_i64_sign_extend_i32, "-15i32", -15, ["gen-wasm", "gen-dev"]
        to_i64_truncate, "115i128", 115
        to_i64_truncate_wraps, "10_000_000_000_000_000_000i128", -8446744073709551616
    )
    "Num.toI128", i128, (
        to_i128_same_width, "15u128", 15
        to_i128_extend, "15i8", 15
    )
    "Num.toU8", u8, (
        to_u8_same_width, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u8_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_u8_truncate_wraps, "500i32", 244, ["gen-wasm", "gen-dev"]
    )
    "Num.toU16", u16, (
        to_u16_same_width, "15i16", 15, ["gen-wasm", "gen-dev"]
        to_u16_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u16_truncate, "115i32", 115, ["gen-wasm", "gen-dev"]
        to_u16_truncate_wraps, "600000000i32", 17920, ["gen-wasm", "gen-dev"]
    )
    "Num.toU32", u32, (
        to_u32_same_width, "15i32", 15, ["gen-wasm", "gen-dev"]
        to_u32_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u32_truncate, "115i64", 115, ["gen-wasm", "gen-dev"]
        to_u32_truncate_wraps, "5000000000000000000i64", 1156841472, ["gen-wasm", "gen-dev"]
    )
    "Num.toU64", u64, (
        to_u64_same_width, "15i64", 15, ["gen-wasm", "gen-dev"]
        to_u64_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_u64_truncate, "115i128", 115
        to_u64_truncate_wraps, "10_000_000_000_000_000_000_000i128", 1864712049423024128
    )
    "Num.toU128", u128, (
        to_u128_same_width, "15i128", 15
        to_u128_extend, "15i8", 15
    )
    "Num.toNat", usize, (
        to_nat_same_width, "15i64", 15, ["gen-wasm", "gen-dev"]
        to_nat_extend, "15i8", 15, ["gen-wasm", "gen-dev"]
        to_nat_truncate, "115i128", 115
    )
    "Num.toF32", f32, (
        to_f32_from_i8, "15i8", 15.0
        to_f32_from_i16, "15i16", 15.0
        to_f32_from_i32, "15i32", 15.0
        to_f32_from_i64, "15i64", 15.0
        to_f32_from_i128, "15i128", 15.0
        to_f32_from_u8, "15u8", 15.0
        to_f32_from_u16, "15u16", 15.0
        to_f32_from_u32, "15u32", 15.0
        to_f32_from_u64, "15u64", 15.0
        to_f32_from_u128, "15u128", 15.0
        to_f32_from_nat, "15nat", 15.0
        to_f32_from_f32, "1.5f32", 1.5
        to_f32_from_f64, "1.5f64", 1.5
    )
    "Num.toF64", f64, (
        to_f64_from_i8, "15i8", 15.0
        to_f64_from_i16, "15i16", 15.0
        to_f64_from_i32, "15i32", 15.0
        to_f64_from_i64, "15i64", 15.0
        to_f64_from_i128, "15i128", 15.0
        to_f64_from_u8, "15u8", 15.0
        to_f64_from_u16, "15u16", 15.0
        to_f64_from_u32, "15u32", 15.0
        to_f64_from_u64, "15u64", 15.0
        to_f64_from_u128, "15u128", 15.0
        to_f64_from_nat, "15nat", 15.0
        to_f64_from_f32, "1.5f32", 1.5
        to_f64_from_f64, "1.5f64", 1.5
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
            let input = format!("Result.withDefault ({} {}) {}", $fn, $input, sentinel);
            assert_evals_to!(&input, expected, $typ)
        }
    )*)*}
}

to_int_checked_tests! {
    "Num.toI8Checked", i8, (
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
    "Num.toI16Checked", i16, (
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
    "Num.toI32Checked", i32, (
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
    "Num.toI64Checked", i64, (
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
    "Num.toI128Checked", i128, (
        to_i128_checked_smaller_width_pos,                "15i8",      15
        to_i128_checked_smaller_width_neg,                "-15i8",     -15
        to_i128_checked_same,                             "15i128",     15
        to_i128_checked_same_width_unsigned_fits,         "15u128",     15
        to_i128_checked_same_width_unsigned_oob,          "170141183460469231731687303715884105728u128",  None
    )
    "Num.toU8Checked", u8, (
        to_u8_checked_same,                           "15u8",   15
        to_u8_checked_same_width_signed_fits,         "15i8",   15
        to_u8_checked_same_width_signed_oob,          "-1i8",   None
        to_u8_checked_larger_width_signed_fits_pos,   "15i16",  15
        to_u8_checked_larger_width_signed_oob_pos,    "256i16", None
        to_u8_checked_larger_width_signed_oob_neg,    "-1i16",  None
        to_u8_checked_larger_width_unsigned_fits_pos, "15u16",  15
        to_u8_checked_larger_width_unsigned_oob_pos,  "256u16", None
    )
    "Num.toU16Checked", u16, (
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
    "Num.toU32Checked", u32, (
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
    "Num.toU64Checked", u64, (
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
    "Num.toU128Checked", u128, (
        to_u128_checked_smaller_width_pos,             "15i8",     15
        to_u128_checked_smaller_width_neg_oob,         "-15i8",    None
        to_u128_checked_same,                          "15u128",   15
        to_u128_checked_same_width_signed_fits,        "15i128",   15
        to_u128_checked_same_width_signed_oob,         "-1i128",   None
    )
    "Num.toNatChecked", usize, (
        to_nat_checked_smaller_width_pos,              "15i8",     15
        to_nat_checked_smaller_width_neg_oob,          "-15i8",    None
        to_nat_checked_same,                           "15u64",    15
        to_nat_checked_same_width_signed_fits,         "15i64",    15
        to_nat_checked_same_width_signed_oob,          "-1i64",    None
        to_nat_checked_larger_width_signed_fits_pos,   "15i128",   15
        to_nat_checked_larger_width_signed_oob_pos,    "18446744073709551616i128", None
        to_nat_checked_larger_width_signed_oob_neg,    "-1i128",   None
        to_nat_checked_larger_width_unsigned_fits_pos, "15u128",   15
        to_nat_checked_larger_width_unsigned_oob_pos,  "18446744073709551616u128", None
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn is_multiple_of_signed() {
    // true
    assert_evals_to!("Num.isMultipleOf 5 1", true, bool);
    assert_evals_to!("Num.isMultipleOf 5 -1", true, bool);
    assert_evals_to!("Num.isMultipleOf 0 0", true, bool);
    assert_evals_to!("Num.isMultipleOf 0 1", true, bool);
    assert_evals_to!("Num.isMultipleOf 0 -1", true, bool);
    // false
    assert_evals_to!("Num.isMultipleOf 5 2", false, bool);
    assert_evals_to!("Num.isMultipleOf 5 0", false, bool);

    // overflow
    assert_evals_to!("Num.isMultipleOf -9223372036854775808 -1", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn is_multiple_of_unsigned() {
    // true
    assert_evals_to!("Num.isMultipleOf 5u8 1", true, bool);
    assert_evals_to!("Num.isMultipleOf 0u8 0", true, bool);
    assert_evals_to!("Num.isMultipleOf 0u8 1", true, bool);
    assert_evals_to!("Num.isMultipleOf 0u8 0xFF", true, bool);

    // false
    assert_evals_to!("Num.isMultipleOf 5u8 2", false, bool);
    assert_evals_to!("Num.isMultipleOf 5u8 0", false, bool);

    // unsigned result is different from signed
    assert_evals_to!("Num.isMultipleOf 5u8 0xFF", false, bool);
    assert_evals_to!("Num.isMultipleOf 0xFCu8 0xFE", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u16_clearly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello"
                when Num.bytesToU16 bytes 234 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u16_subtly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello"
                when Num.bytesToU16 bytes 4 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u32_clearly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello"
                when Num.bytesToU32 bytes 234 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u32_subtly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello"
                when Num.bytesToU32 bytes 2 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u64_clearly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello"
                when Num.bytesToU64 bytes 234 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u64_subtly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello world"
                when Num.bytesToU64 bytes 4 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u128_clearly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello"
                when Num.bytesToU128 bytes 234 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u128_subtly_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                bytes = Str.toUtf8 "hello world!!!!!!"
                when Num.bytesToU128 bytes 2 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        1,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u16_max_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU16 [255, 255] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        65535,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u16_min_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU16 [0, 0] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        0,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u16_random_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU16 [164, 215] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        55_204,
        u16
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u32_min_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU32 [0, 0, 0, 0] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        0,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u32_max_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU32 [255, 255, 255, 255] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        4_294_967_295,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u32_random_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU32 [252, 124, 128, 121] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        2_038_463_740,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u64_min_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU64 [0, 0, 0, 0, 0, 0, 0, 0] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u64_max_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU64 [255, 255, 255, 255, 255, 255, 255, 255] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        18_446_744_073_709_551_615,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u64_random_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU64 [252, 124, 128, 121, 1, 32, 177, 211] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        15_254_008_603_586_100_476,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u128_min_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU128 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        0,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u128_max_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU128 [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        340_282_366_920_938_463_463_374_607_431_768_211_455,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bytes_to_u128_random_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
                when Num.bytesToU128 [252, 124, 128, 121, 1, 32, 177, 211, 3, 57, 203, 122, 95, 164, 23, 145] 0 is
                    Ok v -> v
                    Err OutOfBounds -> 1
                "#
        ),
        192_860_816_096_412_392_720_639_456_393_488_792_828,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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

    assert_evals_to!(r#"Num.toStr 1234"#, RocStr::from("1234"), RocStr);
    assert_evals_to!(r#"Num.toStr 0"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr -1"#, RocStr::from("-1"), RocStr);

    let max = format!("{}", i64::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxI64"#,
        RocStr::from(max.as_str()),
        RocStr
    );

    let min = format!("{}", i64::MIN);
    assert_evals_to!(
        r#"Num.toStr Num.minI64"#,
        RocStr::from(min.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_u8() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr 0u8"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1u8"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10u8"#, RocStr::from("10"), RocStr);

    let max = format!("{}", u8::MAX);
    assert_evals_to!(r#"Num.toStr Num.maxU8"#, RocStr::from(max.as_str()), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_u16() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr 0u16"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1u16"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10u16"#, RocStr::from("10"), RocStr);

    let max = format!("{}", u16::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxU16"#,
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_u32() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr 0u32"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1u32"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10u32"#, RocStr::from("10"), RocStr);

    let max = format!("{}", u32::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxU32"#,
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_u64() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr 0u64"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1u64"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10u64"#, RocStr::from("10"), RocStr);

    let max = format!("{}", u64::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxU64"#,
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_i8() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10i8"#, RocStr::from("-10"), RocStr);
    assert_evals_to!(r#"Num.toStr -1i8"#, RocStr::from("-1"), RocStr);
    assert_evals_to!(r#"Num.toStr 0i8"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1i8"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10i8"#, RocStr::from("10"), RocStr);

    let max = format!("{}", i8::MAX);
    assert_evals_to!(r#"Num.toStr Num.maxI8"#, RocStr::from(max.as_str()), RocStr);

    let max = format!("{}", i8::MIN);
    assert_evals_to!(r#"Num.toStr Num.minI8"#, RocStr::from(max.as_str()), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_i16() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10i16"#, RocStr::from("-10"), RocStr);
    assert_evals_to!(r#"Num.toStr -1i16"#, RocStr::from("-1"), RocStr);
    assert_evals_to!(r#"Num.toStr 0i16"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1i16"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10i16"#, RocStr::from("10"), RocStr);

    let max = format!("{}", i16::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxI16"#,
        RocStr::from(max.as_str()),
        RocStr
    );

    let max = format!("{}", i16::MIN);
    assert_evals_to!(
        r#"Num.toStr Num.minI16"#,
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_i32() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10i32"#, RocStr::from("-10"), RocStr);
    assert_evals_to!(r#"Num.toStr -1i32"#, RocStr::from("-1"), RocStr);
    assert_evals_to!(r#"Num.toStr 0i32"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1i32"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10i32"#, RocStr::from("10"), RocStr);

    let max = format!("{}", i32::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxI32"#,
        RocStr::from(max.as_str()),
        RocStr
    );

    let max = format!("{}", i32::MIN);
    assert_evals_to!(
        r#"Num.toStr Num.minI32"#,
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_i64() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10i64"#, RocStr::from("-10"), RocStr);
    assert_evals_to!(r#"Num.toStr -1i64"#, RocStr::from("-1"), RocStr);
    assert_evals_to!(r#"Num.toStr 0i64"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1i64"#, RocStr::from("1"), RocStr);
    assert_evals_to!(r#"Num.toStr 10i64"#, RocStr::from("10"), RocStr);

    let max = format!("{}", i64::MAX);
    assert_evals_to!(
        r#"Num.toStr Num.maxI64"#,
        RocStr::from(max.as_str()),
        RocStr
    );

    let max = format!("{}", i64::MIN);
    assert_evals_to!(
        r#"Num.toStr Num.minI64"#,
        RocStr::from(max.as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_f32() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10.75f32"#, RocStr::from("-10.75"), RocStr);
    assert_evals_to!(r#"Num.toStr -1.75f32"#, RocStr::from("-1.75"), RocStr);
    assert_evals_to!(r#"Num.toStr 0f32"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1.75f32"#, RocStr::from("1.75"), RocStr);
    assert_evals_to!(r#"Num.toStr 10.75f32"#, RocStr::from("10.75"), RocStr);

    assert_evals_to!(
        r#"Num.toStr Num.maxF32"#,
        RocStr::from("340282346638528860000000000000000000000"),
        RocStr
    );

    assert_evals_to!(
        r#"Num.toStr Num.minF32"#,
        RocStr::from("-340282346638528860000000000000000000000"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_f64() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10.75f64"#, RocStr::from("-10.75"), RocStr);
    assert_evals_to!(r#"Num.toStr -1.75f64"#, RocStr::from("-1.75"), RocStr);
    assert_evals_to!(r#"Num.toStr 0f64"#, RocStr::from("0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1.75f64"#, RocStr::from("1.75"), RocStr);
    assert_evals_to!(r#"Num.toStr 10.75f64"#, RocStr::from("10.75"), RocStr);

    assert_evals_to!(
        r#"Num.toStr Num.maxF64"#,
        RocStr::from(f64::MAX.to_string().as_str()),
        RocStr
    );

    assert_evals_to!(
        r#"Num.toStr Num.minF64"#,
        RocStr::from(f64::MIN.to_string().as_str()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_to_str_dec() {
    use roc_std::RocStr;

    assert_evals_to!(r#"Num.toStr -10.75dec"#, RocStr::from("-10.75"), RocStr);
    assert_evals_to!(r#"Num.toStr -1.75dec"#, RocStr::from("-1.75"), RocStr);
    assert_evals_to!(r#"Num.toStr 0dec"#, RocStr::from("0.0"), RocStr);
    assert_evals_to!(r#"Num.toStr 1.75dec"#, RocStr::from("1.75"), RocStr);
    assert_evals_to!(r#"Num.toStr 10.75dec"#, RocStr::from("10.75"), RocStr);

    assert_evals_to!(
        r#"Num.toStr 170141183460469.105727dec"#,
        RocStr::from("170141183460469.105727"),
        RocStr
    );

    assert_evals_to!(
        r#"Num.toStr -170141183460469.105727dec"#,
        RocStr::from("-170141183460469.105727"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn u8_addition_greater_than_i8() {
    assert_evals_to!(
        indoc!(
            r#"
            x : U8
            x = 100
            y : U8
            y = 100
            x + y
            "#
        ),
        200,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn u8_sub_greater_than_i8() {
    assert_evals_to!(
        indoc!(
            r#"
            x : U8
            x = 255
            y : U8
            y = 55
            x - y
            "#
        ),
        200,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn u8_mul_greater_than_i8() {
    assert_evals_to!(
        indoc!(
            r#"
            x : U8
            x = 40
            y : U8
            y = 5
            x * y
            "#
        ),
        200,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn add_saturated() {
    assert_evals_to!(
        indoc!(
            r#"
            x : U8
            x = 200
            y : U8
            y = 200
            Num.addSaturated x y
            "#
        ),
        255,
        u8
    );

    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = 100
            y : I8
            y = 100
            Num.addSaturated x y
            "#
        ),
        127,
        i8
    );

    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = -100
            y : I8
            y = -100
            Num.addSaturated x y
            "#
        ),
        -128,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn sub_saturated() {
    assert_evals_to!(
        indoc!(
            r#"
            x : U8
            x = 10
            y : U8
            y = 20
            Num.subSaturated x y
            "#
        ),
        0,
        u8
    );
    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = -100
            y : I8
            y = 100
            Num.subSaturated x y
            "#
        ),
        -128,
        i8
    );
    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = 100
            y : I8
            y = -100
            Num.subSaturated x y
            "#
        ),
        127,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mul_saturated() {
    assert_evals_to!(
        indoc!(
            r#"
            x : U8
            x = 20
            y : U8
            y = 20
            Num.mulSaturated x y
            "#
        ),
        255,
        u8
    );
    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = -20
            y : I8
            y = -20
            Num.mulSaturated x y
            "#
        ),
        127,
        i8
    );
    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = 20
            y : I8
            y = -20
            Num.mulSaturated x y
            "#
        ),
        -128,
        i8
    );
    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = -20
            y : I8
            y = 20
            Num.mulSaturated x y
            "#
        ),
        -128,
        i8
    );
    assert_evals_to!(
        indoc!(
            r#"
            x : I8
            x = 20
            y : I8
            y = 20
            Num.mulSaturated x y
            "#
        ),
        127,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn monomorphized_ints() {
    assert_evals_to!(
        indoc!(
            r#"
            x = 100

            f : U8, U32 -> Nat
            f = \_, _ -> 18

            f x x
            "#
        ),
        18,
        usize
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn monomorphized_floats() {
    assert_evals_to!(
        indoc!(
            r#"
            x = 100.0

            f : F32, F64 -> Nat
            f = \_, _ -> 18

            f x x
            "#
        ),
        18,
        usize
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn monomorphized_ints_names_dont_conflict() {
    assert_evals_to!(
        indoc!(
            r#"
            f : U8 -> Nat
            f = \_ -> 9
            x =
                n = 100
                f n

            y =
                n = 100
                f n

            x + y
            "#
        ),
        18,
        usize
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn monomorphized_ints_aliased() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                y = \{} -> 100
                w1 = \{} -> y {}
                w2 = \{} -> y {}

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_float_f32() {
    assert_evals_to!(
        indoc!(
            r#"
            n : U8
            n = 100

            f : F32
            f = Num.toFrac n
            f
            "#
        ),
        100.,
        f32
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_float_f64() {
    assert_evals_to!(
        indoc!(
            r#"
            n : U8
            n = 100

            f : F64
            f = Num.toFrac n
            f
            "#
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
            r#"
            Num.toU16 0b1000_0000u8
            "#
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
            r#"
            when Num.toU16Checked 0b1000_0000u8 is
                Ok 128u16 -> 1u8
                _ -> 0u8
            "#
        ),
        1,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn modulo_of_unsigned() {
    assert_evals_to!(
        indoc!(
            r#"
            0b1111_1111u8 % 64
            "#
        ),
        63,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn div_of_unsigned() {
    assert_evals_to!(
        indoc!(
            r#"
            0b1111_1111u8 // 2
            "#
        ),
        127,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dec_float_suffix() {
    assert_evals_to!(
        indoc!(
            r#"
            123.0dec
            "#
        ),
        RocDec::from_str_to_i128_unsafe("123.0"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dec_no_decimal() {
    assert_evals_to!(
        indoc!(
            r#"
            3dec
            "#
        ),
        RocDec::from_str_to_i128_unsafe("3.0"),
        i128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ceiling_to_u32() {
    assert_evals_to!(
        indoc!(
            r#"
            n : U32
            n = Num.ceiling 124.5f64
            n
            "#
        ),
        125,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn floor_to_u32() {
    assert_evals_to!(
        indoc!(
            r#"
            n : U32
            n = Num.floor 124.5f64
            n
            "#
        ),
        124,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn round_to_u32() {
    assert_evals_to!(
        indoc!(
            r#"
            n : U32
            n = Num.round 124.49f64
            n
            "#
        ),
        124,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn promote_u64_number_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            9999999999999999999 + 1
            "#
        ),
        10000000000000000000,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn promote_i128_number_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            {
                a: 18446744073709551616 + 1,
                b: -9223372036854775809 + 1,
            }
            "#
        ),
        (18446744073709551617, -9223372036854775808),
        (i128, i128)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn promote_u128_number_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            170141183460469231731687303715884105728 + 1
            "#
        ),
        170141183460469231731687303715884105729,
        u128
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_decimals() {
    assert_evals_to!(
        indoc!(
            r#"
            when 42.42dec is
                42.42 -> 42
                0.05 -> 1
                3.14 -> 2
                _ -> 4
            "#
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when 42.42dec is
                0.05 -> 1
                3.14 -> 2
                _ -> 4
            "#
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
            r#"
            when 1701411834604692317316873037158841057i128 is
                1701411834604692317316873037158841057 -> 42
                32 -> 1
                64 -> 2
                _ -> 4
            "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn when_on_u128() {
    assert_evals_to!(
        indoc!(
            r#"
            when 170141183460469231731687303715884105728u128 is
                170141183460469231731687303715884105728u128 -> 42
                32 -> 1
                64 -> 2
                _ -> 4
            "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn condition_polymorphic_num_becomes_float() {
    assert_evals_to!(
        indoc!(
            r#"
            x = if Bool.true then 2 else 3
            x * 5f32
            "#
        ),
        10.,
        f32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_count_leading_zero_bits() {
    assert_evals_to!(r#"Num.countLeadingZeroBits 0b0010_1000u8"#, 2, usize);
    assert_evals_to!(r#"Num.countLeadingZeroBits 0b0010_1000u16"#, 10, usize);
    assert_evals_to!(r#"Num.countLeadingZeroBits 0b0010_1000u32"#, 26, usize);
    assert_evals_to!(r#"Num.countLeadingZeroBits 0b0010_1000u64"#, 58, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_count_trailing_zero_bits() {
    assert_evals_to!(r#"Num.countTrailingZeroBits 0b0010_1000u8"#, 3, usize);
    assert_evals_to!(r#"Num.countTrailingZeroBits 0b0010_0000u16"#, 5, usize);
    assert_evals_to!(r#"Num.countTrailingZeroBits 0u32"#, 32, usize);
    assert_evals_to!(r#"Num.countTrailingZeroBits 0b0010_1111u64"#, 0, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_count_one_bits() {
    assert_evals_to!(r#"Num.countOneBits 0b0010_1000u8"#, 2, usize);
    assert_evals_to!(r#"Num.countOneBits 0b0010_0000u16"#, 1, usize);
    assert_evals_to!(r#"Num.countOneBits 0u32"#, 0, usize);
    assert_evals_to!(r#"Num.countOneBits 0b0010_1111u64"#, 5, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_abs_diff_int() {
    assert_evals_to!(r#"Num.absDiff 0u8 0u8"#, 0, u8);
    assert_evals_to!(r#"Num.absDiff 1u8 2u8"#, 1, u8);
    assert_evals_to!(r#"Num.absDiff 2u8 1u8"#, 1, u8);
    assert_evals_to!(r#"Num.absDiff -1 1"#, 2, i64);
    assert_evals_to!(r#"Num.absDiff 1 -1"#, 2, i64);
    assert_evals_to!(r#"Num.absDiff Num.minI64 -1"#, i64::MAX, i64);
}

#[test]
#[cfg(feature = "gen-llvm")]
fn num_abs_diff_large_bits() {
    assert_evals_to!(r#"Num.absDiff 0u128 0u128"#, 0, u128);
    assert_evals_to!(r#"Num.absDiff 1u128 2u128"#, 1, u128);
    assert_evals_to!(r#"Num.absDiff -1i128 1i128"#, 2, i128);
    assert_evals_to!(r#"Num.absDiff Num.minI128 -1i128"#, i128::MAX, i128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_abs_diff_float() {
    assert_evals_to!(r#"Num.absDiff 0.0f64 0.0"#, 0.0, f64);
    assert_evals_to!(r#"Num.absDiff 1.0f64 2.0"#, 1.0, f64);
    assert_evals_to!(r#"Num.absDiff 2.0f64 1.0"#, 1.0, f64);
    assert_evals_to!(r#"Num.absDiff -1.0f64 1.0"#, 2.0, f64);
    assert_evals_to!(r#"Num.absDiff 1.0f64 -1.0"#, 2.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer subtraction overflowed!"#)]
fn num_abs_max_overflow() {
    assert_evals_to!(r#"Num.absDiff Num.maxI64 -1"#, 0, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer subtraction overflowed!"#)]
fn num_abs_int_min_overflow() {
    assert_evals_to!(r#"Num.absDiff Num.minI64 0"#, 0, i64);
}

#[test]
#[cfg(feature = "gen-llvm")]
#[should_panic(expected = r#"Roc failed with message: "integer subtraction overflowed!"#)]
fn num_abs_large_bits_min_overflow() {
    assert_evals_to!(r#"Num.absDiff Num.minI128 0"#, 0, i128);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn num_abs_float_overflow() {
    assert_evals_to!("Num.absDiff Num.maxF64 Num.minF64", f64::INFINITY, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn bool_in_switch() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            loop : [ Continue {}, Break {} ]
            loop = Continue {}

            all = \{} ->
                when loop is
                    Continue {} -> Bool.true
                    Break {} -> Bool.false

            main = all {}
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
            r#"
            Num.addChecked 2.0dec 4.0dec
            "#
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
            r#"
            Num.subChecked 5.0dec 2.0dec
            "#
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
            r#"
            Num.mulChecked 5.0dec 2.0dec
            "#
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
            r#"
            x : Result U128 [ Overflow ]
            x = Num.mulChecked 5u128 2u128

            x
            "#
        ),
        RocResult::ok(5u128 * 2u128),
        RocResult<u128, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn sub_checked_u128() {
    assert_evals_to!(
        indoc!(
            r#"
            x : Result U128 [ Overflow ]
            x = Num.subChecked 5u128 2u128

            x
            "#
        ),
        RocResult::ok(5u128 - 2u128),
        RocResult<u128, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn add_checked_u128() {
    assert_evals_to!(
        indoc!(
            r#"
            x : Result U128 [ Overflow ]
            x = Num.addChecked 5u128 2u128

            x
            "#
        ),
        RocResult::ok(5u128 + 2u128),
        RocResult<u128, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn num_min() {
    assert_evals_to!(r#"Num.min 0 0"#, 0, i64);
    assert_evals_to!(r#"Num.min 1 2"#, 1, i64);
    assert_evals_to!(r#"Num.min 2 1"#, 1, i64);
    assert_evals_to!(r#"Num.min 2 -2"#, -2, i64);
    assert_evals_to!(r#"Num.min -2 2"#, -2, i64);
    assert_evals_to!(r#"Num.min Num.minI64 Num.maxI64"#, i64::MIN, i64);
    assert_evals_to!(r#"Num.min Num.maxI64 Num.minI64"#, i64::MIN, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn num_max() {
    assert_evals_to!(r#"Num.max 0 0"#, 0, i64);
    assert_evals_to!(r#"Num.max 1 2"#, 2, i64);
    assert_evals_to!(r#"Num.max 2 1"#, 2, i64);
    assert_evals_to!(r#"Num.max 2 -2"#, 2, i64);
    assert_evals_to!(r#"Num.max -2 2"#, 2, i64);
    assert_evals_to!(r#"Num.max Num.minI64 Num.maxI64"#, i64::MAX, i64);
    assert_evals_to!(r#"Num.max Num.maxI64 Num.minI64"#, i64::MAX, i64);
}
