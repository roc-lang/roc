//! Internal implementation for Roc's float tangent builtin.
//!
//! The algorithm is ported from Zig compiler_rt, which is in turn ported from
//! musl. This module intentionally exports only Roc-facing helpers; it does not
//! provide libm/ compiler-rt symbols such as `tan` or `tanf`.

const std = @import("std");
const math = std.math;

const kernel = @import("trig.zig");
const rem_pio2 = @import("rem_pio2.zig").rem_pio2;
const rem_pio2f = @import("rem_pio2f.zig").rem_pio2f;

/// Tangent for an F32 input, returned as F32.
pub fn tan32(x: f32) f32 {
    // Small multiples of pi/2 rounded to double precision.
    const t1pio2: f64 = 1.0 * math.pi / 2.0; // 0x3FF921FB, 0x54442D18
    const t2pio2: f64 = 2.0 * math.pi / 2.0; // 0x400921FB, 0x54442D18
    const t3pio2: f64 = 3.0 * math.pi / 2.0; // 0x4012D97C, 0x7F3321D2
    const t4pio2: f64 = 4.0 * math.pi / 2.0; // 0x401921FB, 0x54442D18

    var ix: u32 = @bitCast(x);
    const sign = ix >> 31 != 0;
    ix &= 0x7fffffff;

    if (ix <= 0x3f490fda) { // |x| ~<= pi/4
        if (ix < 0x39800000) { // |x| < 2**-12
            return x;
        }
        return kernel.tandf(x, false);
    }
    if (ix <= 0x407b53d1) { // |x| ~<= 5*pi/4
        if (ix <= 0x4016cbe3) { // |x| ~<= 3pi/4
            return kernel.tandf((if (sign) x + t1pio2 else x - t1pio2), true);
        } else {
            return kernel.tandf((if (sign) x + t2pio2 else x - t2pio2), false);
        }
    }
    if (ix <= 0x40e231d5) { // |x| ~<= 9*pi/4
        if (ix <= 0x40afeddf) { // |x| ~<= 7*pi/4
            return kernel.tandf((if (sign) x + t3pio2 else x - t3pio2), true);
        } else {
            return kernel.tandf((if (sign) x + t4pio2 else x - t4pio2), false);
        }
    }

    // tan(Inf or NaN) is NaN.
    if (ix >= 0x7f800000) {
        return x - x;
    }

    var y: f64 = undefined;
    const n = rem_pio2f(x, &y);
    return kernel.tandf(y, n & 1 != 0);
}

/// Tangent for an F64 input, returned as F64.
pub fn tan64(x: f64) f64 {
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

const Tan32Case = struct {
    input_bits: u32,
    expected_bits: u32,
};

const Tan64Case = struct {
    input_bits: u64,
    expected_bits: u64,
};

fn bits32(comptime value: f32) u32 {
    return @bitCast(value);
}

fn bits64(comptime value: f64) u64 {
    return @bitCast(value);
}

fn fromBits32(comptime bits: u32) f32 {
    return @bitCast(bits);
}

fn fromBits64(comptime bits: u64) f64 {
    return @bitCast(bits);
}

fn buildSigned32(comptime positive_bits: []const u32, comptime special_bits: []const u32) [positive_bits.len * 2 + special_bits.len]u32 {
    var bits: [positive_bits.len * 2 + special_bits.len]u32 = undefined;
    var i: usize = 0;
    inline for (positive_bits) |positive| {
        bits[i] = positive;
        i += 1;
        bits[i] = positive | 0x80000000;
        i += 1;
    }
    inline for (special_bits) |special| {
        bits[i] = special;
        i += 1;
    }
    return bits;
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

fn tanExpected32(comptime input: f32) f32 {
    if (math.isNan(input) or math.isInf(input)) return math.nan(f32);
    return math.tan(input);
}

fn tanExpected64(comptime input: f64) f64 {
    if (math.isNan(input) or math.isInf(input)) return math.nan(f64);
    return math.tan(input);
}

fn buildTan32Cases(comptime input_bits: []const u32) [input_bits.len]Tan32Case {
    @setEvalBranchQuota(100_000);
    var cases: [input_bits.len]Tan32Case = undefined;
    inline for (input_bits, 0..) |bits, i| {
        const input = fromBits32(bits);
        cases[i] = .{
            .input_bits = bits,
            .expected_bits = @bitCast(tanExpected32(input)),
        };
    }
    return cases;
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

fn ordered32(value: f32) u32 {
    const bits: u32 = @bitCast(value);
    return if (bits & 0x80000000 != 0) ~bits else bits | 0x80000000;
}

fn ordered64(value: f64) u64 {
    const bits: u64 = @bitCast(value);
    return if (bits & 0x8000000000000000 != 0) ~bits else bits | 0x8000000000000000;
}

fn ulpDiff32(a: f32, b: f32) u32 {
    const ordered_a = ordered32(a);
    const ordered_b = ordered32(b);
    return if (ordered_a > ordered_b) ordered_a - ordered_b else ordered_b - ordered_a;
}

fn ulpDiff64(a: f64, b: f64) u64 {
    const ordered_a = ordered64(a);
    const ordered_b = ordered64(b);
    return if (ordered_a > ordered_b) ordered_a - ordered_b else ordered_b - ordered_a;
}

fn expectClose32(input_bits: u32, expected_bits: u32, actual: f32) error{TestUnexpectedResult}!void {
    const input = @as(f32, @bitCast(input_bits));
    const expected = @as(f32, @bitCast(expected_bits));
    const actual_bits: u32 = @bitCast(actual);

    if (math.isNan(expected)) {
        if (!math.isNan(actual)) {
            std.debug.print("tan32(0x{x}) expected NaN, actual 0x{x} ({d})\n", .{ input_bits, actual_bits, actual });
            return error.TestUnexpectedResult;
        }
        return;
    }

    if (expected == 0.0) {
        if (actual_bits != expected_bits) {
            std.debug.print("tan32(0x{x}) expected signed zero 0x{x}, actual 0x{x}\n", .{ input_bits, expected_bits, actual_bits });
            return error.TestUnexpectedResult;
        }
        return;
    }

    const ulps = ulpDiff32(expected, actual);
    const abs_diff = @abs(expected - actual);
    const tolerance = @max(@as(f32, 0x1p-24), @abs(expected) * @as(f32, 0x1p-18));
    if (ulps > 16 and abs_diff > tolerance) {
        std.debug.print(
            "tan32(0x{x} / {d}) expected 0x{x} ({d}), actual 0x{x} ({d}), ulps {d}, abs diff {d}\n",
            .{ input_bits, input, expected_bits, expected, actual_bits, actual, ulps, abs_diff },
        );
        return error.TestUnexpectedResult;
    }
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

const tan32_positive_input_bits = [_]u32{
    0x00000001, // smallest subnormal, early return
    0x00800000, // smallest normal, early return
    0x397fffff, // just below 2^-12 early-return cutoff
    0x39800000, // at 2^-12 cutoff
    0x39800001, // just above 2^-12 cutoff
    bits32(0.2),
    bits32(0.8923),
    bits32(1.5),
    0x3f490fda, // just below rounded pi/4
    0x3f490fdb, // rounded pi/4
    0x3f490fdc, // just above rounded pi/4
    0x4016cbe2, // just below 3*pi/4 branch cutoff
    0x4016cbe3, // at 3*pi/4 branch cutoff
    0x4016cbe4, // just above 3*pi/4 branch cutoff
    0x407b53d0, // just below 5*pi/4 branch cutoff
    0x407b53d1, // at 5*pi/4 branch cutoff
    0x407b53d2, // just above 5*pi/4 branch cutoff
    0x40afedde, // just below 7*pi/4 branch cutoff
    0x40afeddf, // at 7*pi/4 branch cutoff
    0x40afede0, // just above 7*pi/4 branch cutoff
    0x40e231d4, // just below 9*pi/4 branch cutoff
    0x40e231d5, // at 9*pi/4 branch cutoff
    0x40e231d6, // just above 9*pi/4 branch cutoff
    bits32(37.45),
    bits32(89.123),
    bits32(1.0e6),
    bits32(1.0e10), // forces large argument reduction
    bits32(1.0e20), // forces large argument reduction
    0x7f7fffff, // max finite
};

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

const tan32_input_bits = buildSigned32(&tan32_positive_input_bits, &.{
    0x00000000, // +0
    0x80000000, // -0
    0x7f800000, // +inf
    0xff800000, // -inf
    0x7fc00001, // quiet NaN with payload
});

const tan64_input_bits = buildSigned64(&tan64_positive_input_bits, &.{
    0x0000000000000000, // +0
    0x8000000000000000, // -0
    0x7ff0000000000000, // +inf
    0xfff0000000000000, // -inf
    0x7ff8000000000001, // quiet NaN with payload
});

const tan32_cases = buildTan32Cases(&tan32_input_bits);
const tan64_cases = buildTan64Cases(&tan64_input_bits);

test "tan32 matches Zig tangent across branch and reduction cases" {
    inline for (tan32_cases) |case| {
        try expectClose32(case.input_bits, case.expected_bits, tan32(@bitCast(case.input_bits)));
    }
}

test "tan64 matches Zig tangent across branch and reduction cases" {
    inline for (tan64_cases) |case| {
        try expectClose64(case.input_bits, case.expected_bits, tan64(@bitCast(case.input_bits)));
    }
}
