//! Shared numeric conversion bounds and truncation helpers.

const std = @import("std");
const dec = @import("dec.zig");
const i128h = @import("compiler_rt_128.zig");

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

    const as_f64: f64 = @floatCast(truncated);
    return switch (int_info.signedness) {
        .signed => @intCast(i128h.f64_to_i128(as_f64)),
        .unsigned => @intCast(i128h.f64_to_u128(as_f64)),
    };
}

/// Convert an f64 to raw target integer bits after truncating toward zero.
pub fn f64ToIntTryBits(value: f64, target_bits: u32, target_signed: bool) ?u128 {
    if (!std.math.isFinite(value)) return null;

    const truncated = @trunc(value);
    if (!truncatedFloatFitsTarget(f64, truncated, target_bits, target_signed)) {
        return null;
    }

    if (target_signed) {
        const int_value: i128 = if (target_bits <= 64)
            @as(i64, @intFromFloat(truncated))
        else
            i128h.f64_to_i128(truncated);

        return @bitCast(int_value);
    }

    const int_value: u128 = if (target_bits <= 64)
        @as(u64, @intFromFloat(truncated))
    else
        i128h.f64_to_u128(truncated);

    return int_value;
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
    try std.testing.expectEqual(@as(?u128, null), decToIntTryBits(-1_000_000_000_000_000_000, 8, false));
}
