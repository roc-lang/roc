//! Internal implementation for Roc's F64 sine, cosine, and tangent builtins.
//!
//! The algorithm is ported from Zig compiler_rt, which is in turn ported from
//! musl. This module intentionally exports only Roc-facing helpers; it does not
//! provide libm/compiler-rt symbols such as `tan`.

const std = @import("std");
const math = std.math;

const kernel = @import("trig.zig");
const rem_pio2 = @import("rem_pio2.zig").rem_pio2;

/// Sine for an F64 input, returned as F64.
pub fn sin64(x: f64) f64 {
    @setFloatMode(.strict);

    var ix = @as(u64, @bitCast(x)) >> 32;
    ix &= 0x7fff_ffff;

    if (ix <= 0x3fe9_21fb) {
        if (ix < 0x3e50_0000) return x;
        return kernel.sin(x, 0.0, 0);
    }
    if (ix >= 0x7ff0_0000) return x - x;

    var y: [2]f64 = undefined;
    const n = rem_pio2(x, &y);
    return switch (n & 3) {
        0 => kernel.sin(y[0], y[1], 1),
        1 => kernel.cos(y[0], y[1]),
        2 => -kernel.sin(y[0], y[1], 1),
        else => -kernel.cos(y[0], y[1]),
    };
}

/// Cosine for an F64 input, returned as F64.
pub fn cos64(x: f64) f64 {
    @setFloatMode(.strict);

    var ix = @as(u64, @bitCast(x)) >> 32;
    ix &= 0x7fff_ffff;

    if (ix <= 0x3fe9_21fb) {
        if (ix < 0x3e46_a09e) return 1.0;
        return kernel.cos(x, 0.0);
    }
    if (ix >= 0x7ff0_0000) return x - x;

    var y: [2]f64 = undefined;
    const n = rem_pio2(x, &y);
    return switch (n & 3) {
        0 => kernel.cos(y[0], y[1]),
        1 => -kernel.sin(y[0], y[1], 1),
        2 => -kernel.cos(y[0], y[1]),
        else => kernel.sin(y[0], y[1], 1),
    };
}

/// Tangent for an F64 input, returned as F64.
pub fn tan64(x: f64) f64 {
    @setFloatMode(.strict);

    var ix = @as(u64, @bitCast(x)) >> 32;
    ix &= 0x7fffffff;

    // |x| ~< pi/4
    if (ix <= 0x3fe921fb) {
        if (ix < 0x3e400000) { // |x| < 2**-27
            return x;
        }
        return kernel.tan(x, 0.0, false);
    }

    // tan(Inf or NaN) is NaN.
    if (ix >= 0x7ff00000) {
        return x - x;
    }

    var y: [2]f64 = undefined;
    const n = rem_pio2(x, &y);
    return kernel.tan(y[0], y[1], n & 1 != 0);
}

const Tan64Case = struct {
    input_bits: u64,
    expected_bits: u64,
};

fn bits64(comptime value: f64) u64 {
    return @bitCast(value);
}

fn fromBits64(comptime bits: u64) f64 {
    return @bitCast(bits);
}

fn buildSigned64(comptime positive_bits: []const u64, comptime special_bits: []const u64) [positive_bits.len * 2 + special_bits.len]u64 {
    var bits: [positive_bits.len * 2 + special_bits.len]u64 = undefined;
    var i: usize = 0;
    inline for (positive_bits) |positive| {
        bits[i] = positive;
        i += 1;
        bits[i] = positive | 0x8000000000000000;
        i += 1;
    }
    inline for (special_bits) |special| {
        bits[i] = special;
        i += 1;
    }
    return bits;
}

fn tanExpected64(comptime input: f64) f64 {
    if (math.isNan(input) or math.isInf(input)) return math.nan(f64);
    return math.tan(input);
}

fn buildTan64Cases(comptime input_bits: []const u64) [input_bits.len]Tan64Case {
    @setEvalBranchQuota(100_000);
    var cases: [input_bits.len]Tan64Case = undefined;
    inline for (input_bits, 0..) |bits, i| {
        const input = fromBits64(bits);
        cases[i] = .{
            .input_bits = bits,
            .expected_bits = @bitCast(tanExpected64(input)),
        };
    }
    return cases;
}

fn ordered64(value: f64) u64 {
    const bits: u64 = @bitCast(value);
    return if (bits & 0x8000000000000000 != 0) ~bits else bits | 0x8000000000000000;
}

fn ulpDiff64(a: f64, b: f64) u64 {
    const ordered_a = ordered64(a);
    const ordered_b = ordered64(b);
    return if (ordered_a > ordered_b) ordered_a - ordered_b else ordered_b - ordered_a;
}

fn expectClose64(input_bits: u64, expected_bits: u64, actual: f64) error{TestUnexpectedResult}!void {
    const input = @as(f64, @bitCast(input_bits));
    const expected = @as(f64, @bitCast(expected_bits));
    const actual_bits: u64 = @bitCast(actual);

    if (math.isNan(expected)) {
        if (!math.isNan(actual)) {
            std.debug.print("tan64(0x{x}) expected NaN, actual 0x{x} ({d})\n", .{ input_bits, actual_bits, actual });
            return error.TestUnexpectedResult;
        }
        return;
    }

    if (expected == 0.0) {
        if (actual_bits != expected_bits) {
            std.debug.print("tan64(0x{x}) expected signed zero 0x{x}, actual 0x{x}\n", .{ input_bits, expected_bits, actual_bits });
            return error.TestUnexpectedResult;
        }
        return;
    }

    const ulps = ulpDiff64(expected, actual);
    const abs_diff = @abs(expected - actual);
    const tolerance = @max(@as(f64, 0x1p-52), @abs(expected) * @as(f64, 0x1p-44));
    if (ulps > 16 and abs_diff > tolerance) {
        std.debug.print(
            "tan64(0x{x} / {d}) expected 0x{x} ({d}), actual 0x{x} ({d}), ulps {d}, abs diff {d}\n",
            .{ input_bits, input, expected_bits, expected, actual_bits, actual, ulps, abs_diff },
        );
        return error.TestUnexpectedResult;
    }
}

const tan64_positive_input_bits = [_]u64{
    0x0000000000000001, // smallest subnormal, early return
    0x0010000000000000, // smallest normal, early return
    0x3e3fffffffffffff, // just below 2^-27 early-return cutoff
    0x3e40000000000000, // at 2^-27 cutoff
    0x3e40000000000001, // just above 2^-27 cutoff
    bits64(0.2),
    bits64(0.8923),
    bits64(1.5),
    0x3fe921fb54442d17, // just below pi/4
    0x3fe921fb54442d18, // nearest double to pi/4
    0x3fe921fb54442d19, // just above pi/4
    bits64(37.45),
    bits64(89.123),
    bits64(1.0e6),
    bits64(1.0e20), // forces large argument reduction
    bits64(1.0e100), // forces large argument reduction
    0x7fefffffffffffff, // max finite
};

const tan64_input_bits = buildSigned64(&tan64_positive_input_bits, &.{
    0x0000000000000000, // +0
    0x8000000000000000, // -0
    0x7ff0000000000000, // +inf
    0xfff0000000000000, // -inf
    0x7ff8000000000001, // quiet NaN with payload
});

const tan64_cases = buildTan64Cases(&tan64_input_bits);

test "tan64 matches Zig tangent across branch and reduction cases" {
    inline for (tan64_cases) |case| {
        try expectClose64(case.input_bits, case.expected_bits, tan64(@bitCast(case.input_bits)));
    }
}
