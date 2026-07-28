//! Shared numeric conversion bounds and truncation helpers.

const std = @import("std");
const dec = @import("dec.zig");
const i128h = @import("compiler_rt_128.zig");

/// Exact F64 representation of the greatest finite F32 value.
pub const f32_max_as_f64: f64 = std.math.floatMax(f32);

/// Return whether an F64 can be narrowed by Roc's fallible F32 conversion.
/// The source value itself must be finite and within the finite F32 range;
/// values just beyond the boundary are rejected even when round-to-nearest
/// would demote them back to `floatMax(f32)`.
pub fn f64FitsF32(value: f64) bool {
    return std.math.isFinite(value) and
        value <= f32_max_as_f64 and value >= -f32_max_as_f64;
}

fn powerOfTwo(comptime Float: type, exponent: u32) Float {
    var result: Float = 1.0;
    var i: u32 = 0;
    while (i < exponent) : (i += 1) {
        result *= 2.0;
    }
    return result;
}

/// Return whether a signed 128-bit integer fits in the target integer type.
pub fn i128FitsTarget(value: i128, target_bits: u32, target_signed: bool) bool {
    if (target_bits >= 128) {
        return target_signed or value >= 0;
    }

    if (target_signed) {
        const shift: u7 = @intCast(target_bits - 1);
        const magnitude = @as(i128, @bitCast(i128h.shl(1, shift)));
        return value >= -magnitude and value < magnitude;
    }

    if (value < 0) return false;
    return u128FitsTarget(@intCast(value), target_bits, false);
}

/// Return whether an unsigned 128-bit integer fits in the target integer type.
pub fn u128FitsTarget(value: u128, target_bits: u32, target_signed: bool) bool {
    if (target_bits >= 128) {
        return !target_signed or value <= @as(u128, @bitCast(@as(i128, std.math.maxInt(i128))));
    }

    const shift: u7 = @intCast(if (target_signed) target_bits - 1 else target_bits);
    return value < i128h.shl(1, shift);
}

/// Return whether an already-truncated finite float fits in the target type.
pub fn truncatedFloatFitsTarget(comptime Float: type, truncated: Float, target_bits: u32, target_signed: bool) bool {
    if (target_signed) {
        const magnitude = powerOfTwo(Float, target_bits - 1);
        return truncated >= -magnitude and truncated < magnitude;
    }

    return truncated >= 0 and truncated < powerOfTwo(Float, target_bits);
}

/// Convert a float to an integer after truncating toward zero, or return null.
pub fn floatToIntTry(comptime Float: type, comptime Int: type, value: Float) ?Int {
    if (!std.math.isFinite(value)) return null;

    const truncated = @trunc(value);
    const int_info = @typeInfo(Int).int;
    const target_signed = int_info.signedness == .signed;
    if (!truncatedFloatFitsTarget(Float, truncated, int_info.bits, target_signed)) {
        return null;
    }

    if (int_info.bits <= 64) {
        return @intFromFloat(truncated);
    }

    const bits = floatToIntWrapBits(Float, truncated, int_info.bits);
    const Unsigned = std.meta.Int(.unsigned, int_info.bits);
    return @bitCast(@as(Unsigned, @truncate(bits)));
}

/// Convert an f64 to raw target integer bits after truncating toward zero.
pub fn f64ToIntTryBits(value: f64, target_bits: u32, target_signed: bool) ?u128 {
    return floatToIntTryBits(f64, value, target_bits, target_signed);
}

/// Convert a float to raw target integer bits after truncating toward zero.
pub fn floatToIntTryBits(comptime Float: type, value: Float, target_bits: u32, target_signed: bool) ?u128 {
    if (!std.math.isFinite(value)) return null;

    const truncated = @trunc(value);
    if (!truncatedFloatFitsTarget(Float, truncated, target_bits, target_signed)) {
        return null;
    }

    if (target_bits > 64) {
        return floatToIntWrapBits(Float, truncated, target_bits);
    }

    if (target_signed) {
        const int_value: i128 = @as(i64, @intFromFloat(truncated));

        return @bitCast(int_value);
    }

    const int_value: u128 = @as(u64, @intFromFloat(truncated));

    return int_value;
}

/// Convert a float to raw target integer bits using Roc's wrapping
/// float-to-integer semantics: NaN and the infinities produce 0, the
/// fractional part truncates toward zero, and the integer wraps modulo
/// 2^target_bits. This works from the IEEE-754 representation directly so
/// wrapping does not itself incur float rounding near a modulus boundary.
pub fn floatToIntWrapBits(comptime Float: type, value: Float, target_bits: u32) u128 {
    std.debug.assert(target_bits > 0 and target_bits <= 128);

    const Bits = switch (Float) {
        f32 => u32,
        f64 => u64,
        else => @compileError("floatToIntWrapBits supports only f32 and f64"),
    };
    const fraction_bits: u32 = if (Float == f32) 23 else 52;
    const exponent_bits: u32 = if (Float == f32) 8 else 11;
    const exponent_bias: i32 = if (Float == f32) 127 else 1023;
    const raw: Bits = @bitCast(value);
    const fraction_mask: Bits = (@as(Bits, 1) << @intCast(fraction_bits)) - 1;
    const exponent_mask: Bits = (@as(Bits, 1) << @intCast(exponent_bits)) - 1;
    const exponent: u32 = @intCast((raw >> @intCast(fraction_bits)) & exponent_mask);
    if (exponent == exponent_mask) return 0;
    if (exponent == 0) return 0;

    const significand: u128 = @as(u128, (raw & fraction_mask) | (@as(Bits, 1) << @intCast(fraction_bits)));
    const shift: i32 = @as(i32, @intCast(exponent)) - exponent_bias - @as(i32, @intCast(fraction_bits));
    var magnitude: u128 = if (shift >= 0) blk: {
        if (shift >= target_bits) return 0;
        break :blk i128h.shl(significand, @intCast(shift));
    } else blk: {
        const right_shift: u32 = @intCast(-shift);
        if (right_shift >= 128) return 0;
        break :blk i128h.shr(significand, @intCast(right_shift));
    };

    const mask = if (target_bits == 128)
        std.math.maxInt(u128)
    else
        i128h.shl(1, @intCast(target_bits)) - 1;
    magnitude &= mask;

    const sign_shift: std.math.Log2Int(Bits) = @intCast(@bitSizeOf(Bits) - 1);
    if (((raw >> sign_shift) & 1) != 0) {
        magnitude = (0 -% magnitude) & mask;
    }
    return magnitude;
}

/// Convert a float to an integer using Roc's wrapping float-to-integer
/// semantics (see `floatToIntWrapBits`).
pub fn floatToIntWrap(comptime Float: type, comptime Int: type, value: Float) Int {
    const int_info = @typeInfo(Int).int;
    const bits = floatToIntWrapBits(Float, value, int_info.bits);
    const U = std.meta.Int(.unsigned, int_info.bits);
    return @bitCast(@as(U, @truncate(bits)));
}

/// Convert a Roc Dec payload to raw target integer bits after truncating.
pub fn decToIntTryBits(dec_value: i128, target_bits: u32, target_signed: bool) ?u128 {
    const whole_part = i128h.divTrunc_i128(dec_value, dec.RocDec.one_point_zero_i128);
    if (!i128FitsTarget(whole_part, target_bits, target_signed)) {
        return null;
    }

    return @bitCast(whole_part);
}

test "integer target fit predicates cover signed and unsigned boundaries" {
    try std.testing.expect(i128FitsTarget(-128, 8, true));
    try std.testing.expect(i128FitsTarget(127, 8, true));
    try std.testing.expect(!i128FitsTarget(-129, 8, true));
    try std.testing.expect(!i128FitsTarget(128, 8, true));

    try std.testing.expect(!i128FitsTarget(-1, 8, false));
    try std.testing.expect(i128FitsTarget(255, 8, false));
    try std.testing.expect(!i128FitsTarget(256, 8, false));

    try std.testing.expect(u128FitsTarget(127, 8, true));
    try std.testing.expect(!u128FitsTarget(128, 8, true));
    try std.testing.expect(u128FitsTarget(255, 8, false));
    try std.testing.expect(!u128FitsTarget(256, 8, false));

    try std.testing.expect(i128FitsTarget(std.math.maxInt(i128), 128, true));
    try std.testing.expect(!i128FitsTarget(-1, 128, false));
    try std.testing.expect(u128FitsTarget(std.math.maxInt(u128), 128, false));
    try std.testing.expect(!u128FitsTarget(@as(u128, 1) << 127, 128, true));
}

test "float to int conversions truncate and reject target boundary violations" {
    try std.testing.expect(truncatedFloatFitsTarget(f64, 127.0, 8, true));
    try std.testing.expect(truncatedFloatFitsTarget(f64, -128.0, 8, true));
    try std.testing.expect(!truncatedFloatFitsTarget(f64, 128.0, 8, true));
    try std.testing.expect(!truncatedFloatFitsTarget(f64, -129.0, 8, true));
    try std.testing.expect(truncatedFloatFitsTarget(f64, 255.0, 8, false));
    try std.testing.expect(!truncatedFloatFitsTarget(f64, 256.0, 8, false));
    try std.testing.expect(!truncatedFloatFitsTarget(f64, -1.0, 8, false));

    try std.testing.expectEqual(@as(?i8, 42), floatToIntTry(f32, i8, 42.9));
    try std.testing.expectEqual(@as(?i8, -42), floatToIntTry(f32, i8, -42.9));
    try std.testing.expectEqual(@as(?u8, 255), floatToIntTry(f64, u8, 255.999));
    try std.testing.expectEqual(@as(?i8, null), floatToIntTry(f64, i8, 128.0));
    try std.testing.expectEqual(@as(?u8, null), floatToIntTry(f64, u8, -1.0));
    try std.testing.expectEqual(@as(?i8, null), floatToIntTry(f64, i8, std.math.inf(f64)));
    try std.testing.expectEqual(@as(?i8, null), floatToIntTry(f64, i8, std.math.nan(f64)));
}

test "f64 fallible narrowing checks the source against the exact f32 range" {
    const max_bits: u64 = 0x47ef_ffff_e000_0000;
    const above_max_bits: u64 = max_bits + 1;
    const max: f64 = @bitCast(max_bits);
    const above_max: f64 = @bitCast(above_max_bits);

    try std.testing.expectEqual(f32_max_as_f64, max);
    try std.testing.expect(f64FitsF32(max));
    try std.testing.expect(f64FitsF32(-max));
    try std.testing.expect(!f64FitsF32(above_max));
    try std.testing.expect(!f64FitsF32(-above_max));
    try std.testing.expect(!f64FitsF32(std.math.nan(f64)));
    try std.testing.expect(!f64FitsF32(std.math.inf(f64)));
    try std.testing.expect(!f64FitsF32(-std.math.inf(f64)));
    try std.testing.expectEqual(@as(u32, 0x7f7f_ffff), @as(u32, @bitCast(@as(f32, @floatCast(above_max)))));
}

test "wrapping float to int conversions wrap modulo 2^bits and zero non-finite inputs" {
    try std.testing.expectEqual(@as(i8, 42), floatToIntWrap(f64, i8, 42.7));
    try std.testing.expectEqual(@as(i8, -42), floatToIntWrap(f64, i8, -42.7));
    try std.testing.expectEqual(@as(i8, -128), floatToIntWrap(f64, i8, 128.0));
    try std.testing.expectEqual(@as(i8, -56), floatToIntWrap(f64, i8, 200.0));
    try std.testing.expectEqual(@as(u8, 255), floatToIntWrap(f64, u8, -1.0));
    try std.testing.expectEqual(@as(i8, -128), floatToIntWrap(f32, i8, 128.0));
    try std.testing.expectEqual(@as(i8, 0), floatToIntWrap(f64, i8, std.math.nan(f64)));
    try std.testing.expectEqual(@as(i8, 0), floatToIntWrap(f64, i8, std.math.inf(f64)));
    try std.testing.expectEqual(@as(i8, 0), floatToIntWrap(f64, i8, -std.math.inf(f64)));
    try std.testing.expectEqual(@as(i128, 1) << 100, floatToIntWrap(f64, i128, 0x1p100));
    try std.testing.expectEqual(std.math.minInt(i128), floatToIntWrap(f64, i128, 0x1p127));
    try std.testing.expectEqual(@as(u128, 1) << 127, floatToIntWrap(f64, u128, 0x1p127));
    try std.testing.expectEqual(std.math.maxInt(u128), floatToIntWrap(f64, u128, -1.0));
    try std.testing.expectEqual(std.math.maxInt(u32), floatToIntWrap(f32, u32, -1.0));
    try std.testing.expectEqual(std.math.maxInt(u64), floatToIntWrap(f64, u64, -1.0));

    try std.testing.expectEqual(@as(u128, @as(u8, @bitCast(@as(i8, -128)))), floatToIntWrapBits(f64, 128.0, 8));
    try std.testing.expectEqual(@as(u128, 0), floatToIntWrapBits(f64, std.math.nan(f64), 8));
}

test "raw float and Dec conversion bits preserve signed integer representation" {
    try std.testing.expectEqual(@as(?u128, 42), f64ToIntTryBits(42.9, 8, true));
    try std.testing.expectEqual(@as(?u128, @bitCast(@as(i128, -42))), f64ToIntTryBits(-42.9, 8, true));
    try std.testing.expectEqual(@as(?u128, 255), f64ToIntTryBits(255.999, 8, false));
    try std.testing.expectEqual(@as(?u128, null), f64ToIntTryBits(128.0, 8, true));
    try std.testing.expectEqual(@as(?u128, null), f64ToIntTryBits(256.0, 8, false));
    try std.testing.expectEqual(@as(?u128, null), f64ToIntTryBits(std.math.inf(f64), 8, true));

    try std.testing.expectEqual(@as(?u128, 42), decToIntTryBits(42_900_000_000_000_000_000, 8, true));
    try std.testing.expectEqual(@as(?u128, @bitCast(@as(i128, -42))), decToIntTryBits(-42_900_000_000_000_000_000, 8, true));
    try std.testing.expectEqual(@as(?u128, 255), decToIntTryBits(255_999_000_000_000_000_000, 8, false));
    try std.testing.expectEqual(@as(?u128, null), decToIntTryBits(128_000_000_000_000_000_000, 8, true));
    try std.testing.expectEqual(@as(?u128, null), decToIntTryBits(-dec.RocDec.one_point_zero_i128, 8, false));
}
