app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Every Dec-to-integer truncating conversion, driven by a runtime value so the
# conversions reach the backends instead of being folded at compile time.

to_u8 : Dec -> U8
to_u8 = |d| d.to_u8_wrap()

to_i8 : Dec -> I8
to_i8 = |d| d.to_i8_wrap()

to_u16 : Dec -> U16
to_u16 = |d| d.to_u16_wrap()

to_i16 : Dec -> I16
to_i16 = |d| d.to_i16_wrap()

to_u32 : Dec -> U32
to_u32 = |d| d.to_u32_wrap()

to_i32 : Dec -> I32
to_i32 = |d| d.to_i32_wrap()

to_u64 : Dec -> U64
to_u64 = |d| d.to_u64_wrap()

to_i64 : Dec -> I64
to_i64 = |d| d.to_i64_wrap()

to_u128 : Dec -> U128
to_u128 = |d| d.to_u128_wrap()

to_i128 : Dec -> I128
to_i128 = |d| d.to_i128()

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }
    d = n.to_dec()

    # Every destination width recovers the whole part.
    Stdout.line!("widths: ${to_u8(d).to_str()} ${to_i8(d).to_str()} ${to_u16(d).to_str()} ${to_i16(d).to_str()} ${to_u32(d).to_str()} ${to_i32(d).to_str()} ${to_u64(d).to_str()} ${to_i64(d).to_str()} ${to_u128(d).to_str()} ${to_i128(d).to_str()}")

    # The fractional part truncates toward zero in both directions.
    frac = d + 0.75
    neg = 0.0 - frac
    Stdout.line!("trunc: ${to_u8(frac).to_str()} ${to_i8(neg).to_str()} ${to_i64(neg).to_str()}")

    # A Dec's whole part reaches ~1.7e20, so destinations narrower than that
    # must wrap rather than trap, while I128/U128 keep the whole value.
    big = d * 10000000000000000000.0
    Stdout.line!("wide: ${to_u64(big).to_str()} ${to_i128(big).to_str()} ${to_u8(big).to_str()}")
}
