//! Bit-level IEEE helpers shared by the binary32 and binary64 transcendentals.
//! Every helper is instantiated per float type with comptime bit-layout
//! constants, so each instance uses only operations of that float width.

const std = @import("std");

/// Bit layout constants for the IEEE binary float type `F`.
fn Layout(comptime F: type) type {
    return struct {
        const Bits = @Int(.unsigned, @bitSizeOf(F));
        const SignedBits = @Int(.signed, @bitSizeOf(F));
        const mantissa_bits = std.math.floatMantissaBits(F);
        const exponent_mask = (1 << std.math.floatExponentBits(F)) - 1;
        const sign_bit: Bits = 1 << (@bitSizeOf(F) - 1);
        const abs_mask: Bits = sign_bit - 1;
        const infinity_bits: Bits = @as(Bits, exponent_mask) << mantissa_bits;
        const sign_and_mantissa_mask: Bits = sign_bit | ((1 << mantissa_bits) - 1);
        /// Bits of 2^(mantissa_bits + 1); every float at or above it is an even integer.
        const even_integer_floor_bits: Bits = @as(Bits, std.math.floatExponentMax(F) + mantissa_bits + 1) << mantissa_bits;
    };
}

/// Quiet NaN with an empty payload.
pub fn canonicalNan(comptime F: type) F {
    const L = Layout(F);
    return @bitCast(L.infinity_bits | (1 << (L.mantissa_bits - 1)));
}

/// Positive infinity.
pub fn positiveInfinity(comptime F: type) F {
    return @bitCast(Layout(F).infinity_bits);
}

/// Multiplies `value` by 2^`power` with correct overflow, underflow, and
/// subnormal rounding. Subnormal inputs are normalized by multiplying by
/// 2^`subnormal_scale_log2` first.
pub fn scalePowerOfTwo(comptime F: type, comptime subnormal_scale_log2: i32, value: F, power: i32) F {
    const L = Layout(F);
    const subnormal_scale: F = comptime std.math.ldexp(@as(F, 1.0), subnormal_scale_log2);
    const subnormal_unscale: F = comptime std.math.ldexp(@as(F, 1.0), -subnormal_scale_log2);
    var bits: L.Bits = @bitCast(value);
    const sign = bits & L.sign_bit;
    var exponent: i32 = @intCast((bits >> L.mantissa_bits) & L.exponent_mask);
    if (exponent == L.exponent_mask or bits & L.abs_mask == 0) return value;

    if (exponent == 0) {
        const scaled = value * subnormal_scale;
        bits = @bitCast(scaled);
        exponent = @as(i32, @intCast((bits >> L.mantissa_bits) & L.exponent_mask)) - subnormal_scale_log2;
    }

    const new_exponent = exponent + power;
    if (new_exponent >= L.exponent_mask) return @bitCast(sign | L.infinity_bits);
    if (new_exponent > 0) return @bitCast((bits & L.sign_and_mantissa_mask) | (@as(L.Bits, @intCast(new_exponent)) << L.mantissa_bits));
    if (new_exponent <= -subnormal_scale_log2) return @bitCast(sign);

    const adjusted_power = new_exponent + subnormal_scale_log2;
    const normal_bits = (bits & L.sign_and_mantissa_mask) | (@as(L.Bits, @intCast(adjusted_power)) << L.mantissa_bits);
    const normal: F = @bitCast(normal_bits);
    return normal * subnormal_unscale;
}

/// Whether `value` is an odd integer.
pub fn isOddInteger(comptime F: type, value: F) bool {
    const L = Layout(F);
    const abs_bits = @as(L.Bits, @bitCast(value)) & L.abs_mask;
    if (abs_bits >= L.even_integer_floor_bits) return false;
    if (@trunc(value) != value) return false;
    const integer: L.SignedBits = @intFromFloat(value);
    return integer & 1 != 0;
}

/// Returns the IEEE `pow` function for `F`: it resolves every special case
/// (zeros, infinities, NaNs, and the exponents 1, -1, 2, 0.5, and -0.5) and
/// delegates to `finitePowerMagnitude` for a positive, finite, nonzero base
/// and a finite, nonzero exponent.
pub fn powFn(
    comptime F: type,
    comptime finitePowerMagnitude: fn (F, F) F,
) fn (F, F) F {
    const L = Layout(F);
    return struct {
        fn pow(base: F, exponent: F) F {
            @setFloatMode(.strict);

            if (exponent == 0.0 or base == 1.0) return 1.0;
            const base_bits: L.Bits = @bitCast(base);
            const exponent_bits: L.Bits = @bitCast(exponent);
            const base_abs_bits = base_bits & L.abs_mask;
            const exponent_abs_bits = exponent_bits & L.abs_mask;
            if (base_abs_bits > L.infinity_bits or exponent_abs_bits > L.infinity_bits) return canonicalNan(F);
            if (exponent == 1.0) return base;
            if (exponent == -1.0) return 1.0 / base;
            if (exponent == 2.0) return base * base;

            if (base_abs_bits == 0) {
                if (exponent < 0.0) return if (isOddInteger(F, exponent)) @bitCast((base_bits & L.sign_bit) | L.infinity_bits) else positiveInfinity(F);
                return if (isOddInteger(F, exponent)) base else 0.0;
            }

            if (exponent_abs_bits == L.infinity_bits) {
                if (base == -1.0) return 1.0;
                const tends_to_zero = (@abs(base) < 1.0) == (exponent > 0.0);
                return if (tends_to_zero) 0.0 else positiveInfinity(F);
            }
            if (base_abs_bits == L.infinity_bits) {
                if (base_bits >> (@bitSizeOf(F) - 1) != 0) {
                    const reciprocal: F = @bitCast(base_bits & L.sign_bit);
                    return pow(reciprocal, -exponent);
                }
                return if (exponent < 0.0) 0.0 else positiveInfinity(F);
            }
            if (exponent == 0.5) return @sqrt(base);
            if (exponent == -0.5) return 1.0 / @sqrt(base);

            const exponent_is_integer = @trunc(exponent) == exponent;
            if (base < 0.0 and !exponent_is_integer) return canonicalNan(F);

            var result = finitePowerMagnitude(@abs(base), exponent);
            if (base < 0.0 and isOddInteger(F, exponent)) result = -result;
            return result;
        }
    }.pow;
}

test "layout constants match the binary32 and binary64 encodings" {
    try std.testing.expectEqual(@as(u32, 0x7fc0_0000), @as(u32, @bitCast(canonicalNan(f32))));
    try std.testing.expectEqual(@as(u64, 0x7ff8_0000_0000_0000), @as(u64, @bitCast(canonicalNan(f64))));
    try std.testing.expectEqual(@as(u32, 0x4b80_0000), Layout(f32).even_integer_floor_bits);
    try std.testing.expectEqual(@as(u64, 0x4340_0000_0000_0000), Layout(f64).even_integer_floor_bits);
    try std.testing.expectEqual(@as(u32, 0x807f_ffff), Layout(f32).sign_and_mantissa_mask);
    try std.testing.expectEqual(@as(u64, 0x800f_ffff_ffff_ffff), Layout(f64).sign_and_mantissa_mask);
}
