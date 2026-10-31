//! Private fixed-width integer/float conversion support.
//! The two cores are copied verbatim (apart from public visibility) from Zig
//! 0.17.0 lib/compiler_rt/float_from_int.zig (floatFromInt) and
//! lib/compiler_rt/int_from_float.zig (intFromFloat), MIT licensed.
//! Those upstream files register every wrapper family by taking its address,
//! which emits unrelated wide conversions in Debug even with a no-op registrar.
//! Delete this copy and restore toolchain imports when Zig separates the pure
//! implementations from export registration.
const std = @import("std");
const math = std.math;

/// Convert an integer to the nearest float, rounding exact ties to even.
pub inline fn floatFromInt(comptime T: type, x: anytype) T {
    if (x == 0) return 0;

    // Various constants whose values follow from the type parameters.
    // Any reasonable optimizer will fold and propagate all of these.
    const Z = @Int(.unsigned, @bitSizeOf(@TypeOf(x)));
    const uT = @Int(.unsigned, @bitSizeOf(T));
    const inf = math.inf(T);
    const float_bits = @bitSizeOf(T);
    const int_bits = @bitSizeOf(@TypeOf(x));
    const exp_bits = math.floatExponentBits(T);
    const fractional_bits = math.floatFractionalBits(T);
    const exp_bias = math.maxInt(@Int(.unsigned, exp_bits - 1));
    const implicit_bit = if (T != f80) @as(uT, 1) << fractional_bits else 0;
    const max_exp = exp_bias;

    // Sign
    const abs_val = @abs(x);
    const sign_bit = if (x < 0) @as(uT, 1) << (float_bits - 1) else 0;
    var result: uT = sign_bit;

    // Compute significand
    const exp = int_bits - @clz(abs_val) - 1;
    if (int_bits <= fractional_bits or exp <= fractional_bits) {
        const shift_amt = fractional_bits - @as(math.Log2Int(uT), @intCast(exp));

        // Shift up result to line up with the significand - no rounding required
        result = @as(uT, @intCast(abs_val)) << shift_amt;
        result ^= implicit_bit; // Remove implicit integer bit
    } else {
        const shift_amt: math.Log2Int(Z) = @intCast(exp - fractional_bits);
        const exact_tie: bool = @ctz(abs_val) == shift_amt - 1;

        // Shift down result and remove implicit integer bit
        result = @as(uT, @intCast((abs_val >> (shift_amt - 1)))) ^ (implicit_bit << 1);

        // Round result, including round-to-even for exact ties
        result = ((result + 1) >> 1) & ~@as(uT, @intFromBool(exact_tie));
    }

    // Compute exponent
    if ((int_bits > max_exp) and (exp > max_exp)) // If exponent too large, overflow to infinity
        return @bitCast(sign_bit | @as(uT, @bitCast(inf)));

    result += (@as(uT, exp) + exp_bias) << math.floatMantissaBits(T);

    // If the result included a carry, we need to restore the explicit integer bit
    if (T == f80) result |= 1 << fractional_bits;

    return @bitCast(sign_bit | result);
}

/// Truncate toward zero, saturating out-of-range values by sign as Zig's runtime does.
pub inline fn intFromFloat(comptime I: type, a: anytype) I {
    const F = @TypeOf(a);
    const float_bits = @typeInfo(F).float.bits;
    const int_bits = @typeInfo(I).int.bits;
    const rep_t = @Int(.unsigned, float_bits);
    const sig_bits = math.floatMantissaBits(F);
    const exp_bits = math.floatExponentBits(F);
    const fractional_bits = math.floatFractionalBits(F);

    const implicit_bit = if (F != f80) (@as(rep_t, 1) << sig_bits) else 0;
    const max_exp = (1 << (exp_bits - 1));
    const exp_bias = max_exp - 1;
    const sig_mask = (@as(rep_t, 1) << sig_bits) - 1;

    // Break a into sign, exponent, significand
    const a_rep: rep_t = @bitCast(a);
    const negative = (a_rep >> (float_bits - 1)) != 0;
    const exponent = @as(i32, @intCast((a_rep << 1) >> (sig_bits + 1))) - exp_bias;
    const significand: rep_t = (a_rep & sig_mask) | implicit_bit;

    // If the exponent is negative, the result rounds to zero.
    if (exponent < 0) return 0;

    // If the value is too large for the integer type, saturate.
    switch (@typeInfo(I).int.signedness) {
        .unsigned => {
            if (negative) return 0;
            if (@as(c_uint, @intCast(exponent)) >= @min(int_bits, max_exp)) return math.maxInt(I);
        },
        .signed => if (@as(c_uint, @intCast(exponent)) >= @min(int_bits - 1, max_exp)) {
            return if (negative) math.minInt(I) else math.maxInt(I);
        },
    }

    // If 0 <= exponent < sig_bits, right shift to get the result.
    // Otherwise, shift left.
    var result: I = undefined;
    if (exponent < fractional_bits) {
        result = @intCast(significand >> @intCast(fractional_bits - exponent));
    } else {
        result = @as(I, @intCast(significand)) << @intCast(exponent - fractional_bits);
    }

    if ((@typeInfo(I).int.signedness == .signed) and negative)
        return ~result +% 1;
    return result;
}

/// Fixed-width core for the ARM unsigned integer-to-double adapter.
pub fn f64_floatFromInt_u64(a: u64) f64 {
    return floatFromInt(f64, a);
}
/// Fixed-width core for the ARM unsigned integer-to-single adapter.
pub fn f32_floatFromInt_u64(a: u64) f32 {
    return floatFromInt(f32, a);
}
/// Fixed-width core for the ARM double-to-signed-integer adapter.
pub fn i64_intFromFloat_f64(a: f64) i64 {
    return intFromFloat(i64, a);
}
/// Fixed-width core for the ARM double-to-unsigned-integer adapter.
pub fn u64_intFromFloat_f64(a: f64) u64 {
    return intFromFloat(u64, a);
}
/// Fixed-width core for the ARM single-to-signed-integer adapter.
pub fn i64_intFromFloat_f32(a: f32) i64 {
    return intFromFloat(i64, a);
}
/// Fixed-width core for the ARM single-to-unsigned-integer adapter.
pub fn u64_intFromFloat_f32(a: f32) u64 {
    return intFromFloat(u64, a);
}

fn expectFloatFromInt(comptime F: type, x: anytype) error{TestExpectedEqual}!void {
    const U = @Int(.unsigned, @bitSizeOf(F));
    const expected: F = @floatFromInt(x);
    try std.testing.expectEqual(@as(U, @bitCast(expected)), @as(U, @bitCast(floatFromInt(F, x))));
}

test "integer to float powers, endpoints, ties and seeded values" {
    inline for (.{ f32, f64 }) |F| {
        try expectFloatFromInt(F, @as(u64, 0));
        try expectFloatFromInt(F, @as(u64, math.maxInt(u64)));
        try expectFloatFromInt(F, @as(i64, math.minInt(i64)));
        try expectFloatFromInt(F, @as(i64, math.maxInt(i64)));
        for (0..64) |k| {
            const power = @as(u64, 1) << @intCast(k);
            for ([_]u64{ power - 1, power, power +| 1 }) |x| {
                try expectFloatFromInt(F, x);
                if (x <= math.maxInt(i64)) {
                    try expectFloatFromInt(F, @as(i64, @intCast(x)));
                    try expectFloatFromInt(F, -@as(i64, @intCast(x)));
                }
            }
        }
        // Both even and odd retained significands, and each side of their ties.
        const fractional_bits = math.floatFractionalBits(F);
        for (fractional_bits + 2..64) |k| {
            const spacing = @as(u64, 1) << @intCast(k - fractional_bits);
            const power = @as(u64, 1) << @intCast(k);
            for (0..2) |odd| {
                const tie = power + spacing * odd + spacing / 2;
                for ([_]u64{ tie - 1, tie, tie + 1 }) |x| {
                    try expectFloatFromInt(F, x);
                    if (x <= math.maxInt(i64)) try expectFloatFromInt(F, -@as(i64, @intCast(x)));
                }
            }
        }
        var prng = std.Random.DefaultPrng.init(0x7ac3_1892_02fe_4631);
        const random = prng.random();
        for (0..10000) |_| {
            const x = random.int(u64);
            try expectFloatFromInt(F, x);
            try expectFloatFromInt(F, @as(i64, @bitCast(x)));
        }
    }
}

test "float to integer truncation and upstream saturation contract" {
    inline for (.{ f32, f64 }) |F| {
        for ([_]F{ 0.0, -0.0, 0.5, -0.5, 1.5, -1.5, 12345.75, -12345.75, 0x1p52 - 1, 0x1p52, 0x1p52 + 1 }) |x| {
            try std.testing.expectEqual(@as(i64, @intFromFloat(x)), intFromFloat(i64, x));
            if (x >= 0) try std.testing.expectEqual(@as(u64, @intFromFloat(x)), intFromFloat(u64, x));
        }
        // Largest representable value below each upper bound; never cast a
        // rounded-out-of-range float with the language builtin.
        const U = @Int(.unsigned, @bitSizeOf(F));
        const signed_limit: F = 0x1p63;
        const unsigned_limit: F = 0x1p64;
        const below_signed: F = @bitCast(@as(U, @bitCast(signed_limit)) - 1);
        const below_unsigned: F = @bitCast(@as(U, @bitCast(unsigned_limit)) - 1);
        try std.testing.expectEqual(@as(i64, @intFromFloat(below_signed)), intFromFloat(i64, below_signed));
        try std.testing.expectEqual(@as(u64, @intFromFloat(below_unsigned)), intFromFloat(u64, below_unsigned));
        try std.testing.expectEqual(math.minInt(i64), intFromFloat(i64, @as(F, -0x1p63)));
        for ([_]F{ signed_limit, unsigned_limit, math.inf(F), math.nan(F) }) |x| {
            try std.testing.expectEqual(math.maxInt(i64), intFromFloat(i64, x));
            try std.testing.expectEqual(math.minInt(i64), intFromFloat(i64, -x));
            try std.testing.expectEqual(@as(u64, 0), intFromFloat(u64, -x));
        }
        for ([_]F{ unsigned_limit, math.inf(F), math.nan(F) }) |x| {
            try std.testing.expectEqual(math.maxInt(u64), intFromFloat(u64, x));
        }
        try std.testing.expectEqual(@as(u64, 0), intFromFloat(u64, @as(F, -1.5)));
        var prng = std.Random.DefaultPrng.init(0x6e29_9703_27ab_1048);
        const random = prng.random();
        for (0..10000) |_| {
            const x: F = @bitCast(random.int(U));
            if (math.isFinite(x) and x >= -0x1p63 and x < 0x1p63)
                try std.testing.expectEqual(@as(i64, @intFromFloat(x)), intFromFloat(i64, x));
            if (math.isFinite(x) and x >= 0 and x < 0x1p64)
                try std.testing.expectEqual(@as(u64, @intFromFloat(x)), intFromFloat(u64, x));
        }
    }
}

test "fixed width entries call the conversion cores" {
    try std.testing.expectEqual(@as(f64, 42), f64_floatFromInt_u64(42));
    try std.testing.expectEqual(@as(f32, 42), f32_floatFromInt_u64(42));
    try std.testing.expectEqual(@as(i64, -42), i64_intFromFloat_f64(-42.5));
    try std.testing.expectEqual(@as(u64, 42), u64_intFromFloat_f64(42.5));
    try std.testing.expectEqual(@as(i64, -42), i64_intFromFloat_f32(-42.5));
    try std.testing.expectEqual(@as(u64, 42), u64_intFromFloat_f32(42.5));
}
