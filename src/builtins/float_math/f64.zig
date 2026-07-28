//! Deterministic, binary64-only implementations of Roc's F64 transcendentals.
//!
//! The finite power kernel is ported from FreeBSD's fdlibm-derived `e_pow.c`,
//! via Rust libm's `src/math/pow.rs`. It computes `log2(base)` and the
//! exponent product in split high/low pieces instead of composing
//! independently-rounded log and exp
//! functions. The non-FMA evaluation path is used unconditionally so targets
//! with and without fused multiply-add instructions produce identical bits.
//!
//! Every floating-point value and operation in this file is binary64. Keeping
//! the complete algorithm in Roc's builtin payload prevents a backend from
//! substituting a target libm or LLVM intrinsic with different finite bits.

// Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved.
//
// Permission to use, copy, modify, and distribute this
// software is freely granted, provided that this notice
// is preserved.

const std = @import("std");
const trig = @import("tan.zig");

const canonical_nan: f64 = @bitCast(@as(u64, 0x7ff8_0000_0000_0000));
const positive_infinity: f64 = @bitCast(@as(u64, 0x7ff0_0000_0000_0000));

const bp = [2]f64{ 1.0, 1.5 };
const dp_high = [2]f64{ 0.0, 5.84962487220764160156e-01 };
const dp_low = [2]f64{ 0.0, 1.35003920212974897128e-08 };
const two_to_53: f64 = 9007199254740992.0;

// Polynomial coefficients for (3/2) * (log(x) - 2s - 2/3*s^3).
const l1: f64 = 5.99999999999994648725e-01;
const l2: f64 = 4.28571428578550184252e-01;
const l3: f64 = 3.33333329818377432918e-01;
const l4: f64 = 2.72728123808534006489e-01;
const l5: f64 = 2.30660745775561754067e-01;
const l6: f64 = 2.06975017800338417784e-01;

const p1: f64 = 1.66666666666666019037e-01;
const p2: f64 = -2.77777777770155933842e-03;
const p3: f64 = 6.61375632143793436117e-05;
const p4: f64 = -1.65339022054652515390e-06;
const p5: f64 = 4.13813679705723846039e-08;

const ln2: f64 = 6.93147180559945286227e-01;
const ln2_high: f64 = 6.93147182464599609375e-01;
const ln2_low: f64 = -1.90465429995776804525e-09;
const overflow_tail: f64 = 8.0085662595372944372e-017;
const inv_ln2: f64 = 1.44269504088896338700e+00;
const inv_ln2_high: f64 = 1.44269502162933349609e+00;
const inv_ln2_low: f64 = 1.92596299112661746887e-08;
const cp: f64 = 9.61796693925975554329e-01;
const cp_high: f64 = 9.61796700954437255859e-01;
const cp_low: f64 = -7.02846165095275826516e-09;

fn highWord(value: f64) u32 {
    return @truncate(@as(u64, @bitCast(value)) >> 32);
}

fn withHighWord(value: f64, high: u32) f64 {
    const bits: u64 = @bitCast(value);
    return @bitCast((@as(u64, high) << 32) | (bits & 0xffff_ffff));
}

fn withLowWord(value: f64, low: u32) f64 {
    const bits: u64 = @bitCast(value);
    return @bitCast((bits & 0xffff_ffff_0000_0000) | low);
}

fn scalePowerOfTwo(value: f64, power: i32) f64 {
    var bits: u64 = @bitCast(value);
    const sign = bits & 0x8000_0000_0000_0000;
    var exponent: i32 = @intCast((bits >> 52) & 0x7ff);
    if (exponent == 0x7ff or bits & 0x7fff_ffff_ffff_ffff == 0) return value;

    if (exponent == 0) {
        const scaled = value * 0x1.0p54;
        bits = @bitCast(scaled);
        exponent = @as(i32, @intCast((bits >> 52) & 0x7ff)) - 54;
    }

    const new_exponent = exponent + power;
    if (new_exponent >= 0x7ff) return @bitCast(sign | 0x7ff0_0000_0000_0000);
    if (new_exponent > 0) return @bitCast((bits & 0x800f_ffff_ffff_ffff) | (@as(u64, @intCast(new_exponent)) << 52));
    if (new_exponent <= -54) return @bitCast(sign);

    const adjusted_power = new_exponent + 54;
    const normal_bits = (bits & 0x800f_ffff_ffff_ffff) | (@as(u64, @intCast(adjusted_power)) << 52);
    const normal: f64 = @bitCast(normal_bits);
    return normal * 0x1.0p-54;
}

fn isOddInteger(value: f64) bool {
    const abs_bits = @as(u64, @bitCast(value)) & 0x7fff_ffff_ffff_ffff;
    if (abs_bits >= 0x4340_0000_0000_0000) return false;
    if (@trunc(value) != value) return false;
    const integer: i64 = @intFromFloat(value);
    return integer & 1 != 0;
}

/// Computes the magnitude of `base^exponent` for a positive, finite, nonzero
/// base and a finite, nonzero exponent. This is the non-FMA fdlibm path.
fn finitePowerMagnitude(base: f64, exponent: f64) f64 {
    @setFloatMode(.strict);

    var abs_base = base;
    var base_high = highWord(abs_base);
    const exponent_high = highWord(exponent);
    const exponent_abs_high = exponent_high & 0x7fff_ffff;
    const exponent_is_negative = exponent_high >> 31 != 0;

    var log2_high_part: f64 = undefined;
    var log2_low_part: f64 = undefined;

    if (exponent_abs_high > 0x41e0_0000) {
        // For |exponent| > 2^31, values not extremely close to one must
        // overflow or underflow. Larger exponents can decide from the side of
        // one alone.
        if (exponent_abs_high > 0x43f0_0000) {
            if (base_high <= 0x3fef_ffff) return if (exponent_is_negative) positive_infinity else 0.0;
            if (base_high >= 0x3ff0_0000) return if (exponent_is_negative) 0.0 else positive_infinity;
        }
        if (base_high < 0x3fef_ffff) return if (exponent_is_negative) positive_infinity else 0.0;
        if (base_high > 0x3ff0_0000) return if (exponent_is_negative) 0.0 else positive_infinity;

        // Here |1-base| <= 2^-20. Compute log2(base) as two pieces from the
        // short log1p series so multiplying by the large exponent retains its
        // low-order information.
        const difference = abs_base - 1.0;
        const correction = difference * difference * (0.5 - difference * (0.3333333333333333333333 - difference * 0.25));
        const high_product = inv_ln2_high * difference;
        const low_product = difference * inv_ln2_low - correction * inv_ln2;
        log2_high_part = withLowWord(high_product + low_product, 0);
        log2_low_part = low_product - (log2_high_part - high_product);
    } else {
        var base_exponent: i32 = 0;
        if (base_high < 0x0010_0000) {
            abs_base *= two_to_53;
            base_exponent -= 53;
            base_high = highWord(abs_base);
        }

        base_exponent += @as(i32, @intCast(base_high >> 20)) - 0x3ff;
        const fraction_high = base_high & 0x000f_ffff;
        const interval: usize = if (fraction_high <= 0x3988e)
            0
        else if (fraction_high < 0xbb67a)
            1
        else blk: {
            base_exponent += 1;
            break :blk 0;
        };

        base_high = fraction_high | 0x3ff0_0000;
        if (fraction_high >= 0xbb67a) base_high -= 0x0010_0000;
        abs_base = withHighWord(abs_base, base_high);

        // s = (base-bp)/(base+bp), retained as high and low pieces.
        const numerator = abs_base - bp[interval];
        const reciprocal = 1.0 / (abs_base + bp[interval]);
        const s = numerator * reciprocal;
        const s_high = withLowWord(s, 0);
        const t_high = withHighWord(0.0, ((base_high >> 1) | 0x2000_0000) + 0x0008_0000 + (@as(u32, @intCast(interval)) << 18));
        const t_low = abs_base - (t_high - bp[interval]);
        const s_low = reciprocal * ((numerator - s_high * t_high) - s_high * t_low);

        const s_squared = s * s;
        var remainder = s_squared * s_squared * (l1 + s_squared * (l2 + s_squared * (l3 + s_squared * (l4 + s_squared * (l5 + s_squared * l6)))));
        remainder += s_low * (s_high + s);
        const s_high_squared = s_high * s_high;
        const series_high = withLowWord(3.0 + s_high_squared + remainder, 0);
        const series_low = remainder - ((series_high - 3.0) - s_high_squared);
        const product_high = s_high * series_high;
        const product_low = s_low * series_high + series_low * s;
        const p_high = withLowWord(product_high + product_low, 0);
        const p_low = product_low - (p_high - product_high);
        const z_high = cp_high * p_high;
        const z_low = cp_low * p_high + p_low * cp + dp_low[interval];
        const float_base_exponent: f64 = @floatFromInt(base_exponent);
        log2_high_part = withLowWord((z_high + z_low) + dp_high[interval] + float_base_exponent, 0);
        log2_low_part = z_low - (((log2_high_part - float_base_exponent) - dp_high[interval]) - z_high);
    }

    // Multiply the two-piece logarithm by a split exponent.
    const exponent_high_part = withLowWord(exponent, 0);
    const product_low = (exponent - exponent_high_part) * log2_high_part + exponent * log2_low_part;
    var product_high = exponent_high_part * log2_high_part;
    const product = product_high + product_low;
    const product_bits: u64 = @bitCast(product);
    const product_high_word: u32 = @truncate(product_bits >> 32);
    const product_low_word: u32 = @truncate(product_bits);

    if (product_high_word >> 31 == 0 and product_high_word >= 0x4090_0000) {
        if (product_high_word != 0x4090_0000 or product_low_word != 0) return positive_infinity;
        if (product_low + overflow_tail > product - product_high) return positive_infinity;
    } else if (product_high_word >> 31 != 0 and (product_high_word & 0x7fff_ffff) >= 0x4090_cc00) {
        if (product_high_word != 0xc090_cc00 or product_low_word != 0) return 0.0;
        if (product_low <= product - product_high) return 0.0;
    }

    // Reduce the power-of-two exponent to a residual in [-0.5, 0.5].
    const product_abs_high = product_high_word & 0x7fff_ffff;
    var reduced_exponent = @as(i32, @intCast(product_abs_high >> 20)) - 0x3ff;
    var scale_exponent: i32 = 0;
    if (product_abs_high > 0x3fe0_0000) {
        const signed_product_high: i32 = @bitCast(product_high_word);
        const rounded_high = signed_product_high + (@as(i32, 0x0010_0000) >> @intCast(reduced_exponent + 1));
        reduced_exponent = @as(i32, @intCast((@as(u32, @bitCast(rounded_high)) & 0x7fff_ffff) >> 20)) - 0x3ff;
        const truncated_high = @as(u32, @bitCast(rounded_high)) & ~(@as(u32, 0x000f_ffff) >> @intCast(reduced_exponent));
        const rounded_value = withHighWord(0.0, truncated_high);
        scale_exponent = @intCast((@as(u32, @bitCast(rounded_high)) & 0x000f_ffff | 0x0010_0000) >> @intCast(20 - reduced_exponent));
        if (signed_product_high < 0) scale_exponent = -scale_exponent;
        product_high -= rounded_value;
    }

    // Compute 2^(product_high+product_low) for the reduced residual.
    const residual = withLowWord(product_low + product_high, 0);
    const residual_high = residual * ln2_high;
    const residual_low = (product_low - (residual - product_high)) * ln2 + residual * ln2_low;
    var exp_argument = residual_high + residual_low;
    const exp_tail = residual_low - (exp_argument - residual_high);
    const square = exp_argument * exp_argument;
    const correction = exp_argument - square * (p1 + square * (p2 + square * (p3 + square * (p4 + square * p5))));
    const approximation = (exp_argument * correction) / (correction - 2.0) - (exp_tail + exp_argument * exp_tail);
    exp_argument = 1.0 - (approximation - exp_argument);
    return scalePowerOfTwo(exp_argument, scale_exponent);
}

/// Returns `base` raised to `exponent`, computed entirely with binary64 operations.
pub fn pow(base: f64, exponent: f64) f64 {
    @setFloatMode(.strict);

    if (exponent == 0.0 or base == 1.0) return 1.0;
    const base_bits: u64 = @bitCast(base);
    const exponent_bits: u64 = @bitCast(exponent);
    const base_abs_bits = base_bits & 0x7fff_ffff_ffff_ffff;
    const exponent_abs_bits = exponent_bits & 0x7fff_ffff_ffff_ffff;
    if (base_abs_bits > 0x7ff0_0000_0000_0000 or exponent_abs_bits > 0x7ff0_0000_0000_0000) return canonical_nan;
    if (exponent == 1.0) return base;
    if (exponent == -1.0) return 1.0 / base;
    if (exponent == 2.0) return base * base;

    if (base_abs_bits == 0) {
        if (exponent < 0.0) return if (isOddInteger(exponent)) @bitCast((base_bits & 0x8000_0000_0000_0000) | 0x7ff0_0000_0000_0000) else positive_infinity;
        return if (isOddInteger(exponent)) base else 0.0;
    }

    if (exponent_abs_bits == 0x7ff0_0000_0000_0000) {
        if (base == -1.0) return 1.0;
        const tends_to_zero = (@abs(base) < 1.0) == (exponent > 0.0);
        return if (tends_to_zero) 0.0 else positive_infinity;
    }
    if (base_abs_bits == 0x7ff0_0000_0000_0000) {
        if (base_bits >> 63 != 0) {
            const reciprocal: f64 = @bitCast(base_bits & 0x8000_0000_0000_0000);
            return pow(reciprocal, -exponent);
        }
        return if (exponent < 0.0) 0.0 else positive_infinity;
    }
    if (exponent == 0.5) return @sqrt(base);
    if (exponent == -0.5) return 1.0 / @sqrt(base);

    const exponent_is_integer = @trunc(exponent) == exponent;
    if (base < 0.0 and !exponent_is_integer) return canonical_nan;

    var result = finitePowerMagnitude(@abs(base), exponent);
    if (base < 0.0 and isOddInteger(exponent)) result = -result;
    return result;
}

/// Returns the sine of `value`, computed entirely with binary64 operations.
pub fn sin(value: f64) f64 {
    return trig.sin64(value);
}

/// Returns the cosine of `value`, computed entirely with binary64 operations.
pub fn cos(value: f64) f64 {
    return trig.cos64(value);
}

/// Returns the tangent of `value`, computed entirely with binary64 operations.
pub fn tan(value: f64) f64 {
    return trig.tan64(value);
}

/// Returns the arcsine of `value` using Zig's explicit binary64 algorithm.
pub fn asin(value: f64) f64 {
    @setFloatMode(.strict);
    return std.math.asin(value);
}

/// Returns the arccosine of `value` using Zig's explicit binary64 algorithm.
pub fn acos(value: f64) f64 {
    @setFloatMode(.strict);
    return std.math.acos(value);
}

/// Returns the arctangent of `value` using Zig's explicit binary64 algorithm.
pub fn atan(value: f64) f64 {
    @setFloatMode(.strict);
    return std.math.atan(value);
}

test "F64 power special cases" {
    const negative_infinity: f64 = @bitCast(@as(u64, 0xfff0_0000_0000_0000));
    const negative_zero: f64 = @bitCast(@as(u64, 0x8000_0000_0000_0000));

    try std.testing.expectEqual(@as(f64, 8.0), pow(2.0, 3.0));
    try std.testing.expectEqual(@as(f64, -8.0), pow(-2.0, 3.0));
    try std.testing.expectEqual(@as(f64, 16.0), pow(-2.0, 4.0));
    try std.testing.expectEqual(@as(f64, 0.25), pow(2.0, -2.0));
    try std.testing.expectEqual(@as(u64, 0x4de0_90a3_62a0_5c19), @as(u64, @bitCast(pow(7.165387657176249e-68, -1.0))));
    try std.testing.expectEqual(@as(u64, 0x5574_34fc_b314_4853), @as(u64, @bitCast(pow(6.72744224805919e51, 2.0))));
    try std.testing.expectEqual(@as(f64, 1.0), pow(canonical_nan, 0.0));
    try std.testing.expectEqual(@as(f64, 1.0), pow(1.0, canonical_nan));
    try std.testing.expect(std.math.isNan(pow(canonical_nan, 1.0)));
    try std.testing.expect(std.math.isNan(pow(2.0, canonical_nan)));
    try std.testing.expectEqual(@as(u64, 0x8000_0000_0000_0000), @as(u64, @bitCast(pow(negative_zero, 3.0))));
    try std.testing.expectEqual(@as(u64, 0xfff0_0000_0000_0000), @as(u64, @bitCast(pow(negative_zero, -3.0))));
    try std.testing.expectEqual(@as(u64, 0x0000_0000_0000_0000), @as(u64, @bitCast(pow(negative_zero, 2.0))));
    try std.testing.expectEqual(positive_infinity, pow(negative_zero, -2.0));
    try std.testing.expectEqual(@as(f64, 1.0), pow(-1.0, positive_infinity));
    try std.testing.expectEqual(@as(f64, 0.0), pow(0.5, positive_infinity));
    try std.testing.expectEqual(positive_infinity, pow(2.0, positive_infinity));
    try std.testing.expectEqual(positive_infinity, pow(0.5, negative_infinity));
    try std.testing.expectEqual(@as(f64, 0.0), pow(2.0, negative_infinity));
    try std.testing.expectEqual(positive_infinity, pow(positive_infinity, 2.0));
    try std.testing.expectEqual(@as(f64, 0.0), pow(positive_infinity, -2.0));
    try std.testing.expectEqual(negative_infinity, pow(negative_infinity, 3.0));
    try std.testing.expectEqual(@as(u64, 0x8000_0000_0000_0000), @as(u64, @bitCast(pow(negative_infinity, -3.0))));
    try std.testing.expect(std.math.isNan(pow(-1.0, 0.5)));
}

test "F64 power approximations" {
    try std.testing.expectApproxEqRel(@as(f64, 0.004936270901760079), pow(0.2, 3.3), 0x1p-48);
    try std.testing.expectApproxEqRel(@as(f64, 13530.513990233081), pow(17.54697502703452, 3.3204523365293763), 0x1p-48);
}

test "F64 power stays within one ULP of high-precision oracles" {
    const Case = struct {
        base: f64,
        exponent: f64,
        nearest_bits: u64,
    };
    // These nearest-binary64 oracle bits were stable when independently
    // evaluated at 100, 180, and 280 decimal digits of precision.
    const cases = [_]Case{
        .{ .base = 0.2, .exponent = 3.3, .nearest_bits = 0x3f74_380e_2165_6684 },
        .{ .base = 17.54697502703452, .exponent = 3.3204523365293763, .nearest_bits = 0x40ca_6d41_ca6e_94c6 },
        .{ .base = 1.8742325878262631, .exponent = 1111.0207098305914, .nearest_bits = 0x7ede_3bbc_0ae0_45cb },
        .{ .base = 0.5797239088410756, .exponent = 1159.5420969274194, .nearest_bits = 0x06ee_dea9_9173_6578 },
        .{ .base = 1.0000000000000002, .exponent = 2251799813685248.5, .nearest_bits = 0x3ffa_6129_8e1e_069c },
        .{ .base = 0.9999999999999999, .exponent = 2251799813685248.5, .nearest_bits = 0x3fe8_ebef_9eac_820a },
        .{ .base = 1.0000000000000002, .exponent = -2251799813685248.5, .nearest_bits = 0x3fe3_68b2_fc6f_960a },
        .{ .base = 0.9999999999999999, .exponent = -2251799813685248.5, .nearest_bits = 0x3ff4_8b5e_3c3e_8187 },
        .{ .base = 1e-200, .exponent = 1.5, .nearest_bits = 0x01a5_6e1f_c2f8_f359 },
        .{ .base = 1e200, .exponent = 1.5, .nearest_bits = 0x7e37_e43c_8800_759b },
        .{ .base = 1e-200, .exponent = -1.5, .nearest_bits = 0x7e37_e43c_8800_759c },
        .{ .base = 1e200, .exponent = -1.5, .nearest_bits = 0x01a5_6e1f_c2f8_f359 },
        .{ .base = 2.2250738585072014e-308, .exponent = 1.0000000000000002, .nearest_bits = 0x000f_ffff_ffff_fd3c },
        .{ .base = 1.7976931348623157e308, .exponent = 0.9999999999999999, .nearest_bits = 0x7fef_ffff_ffff_fd39 },
        .{ .base = 0.5, .exponent = 1073.5, .nearest_bits = 0x0000_0000_0000_0001 },
        .{ .base = 2.0, .exponent = -1073.5, .nearest_bits = 0x0000_0000_0000_0001 },
        .{ .base = 0.999, .exponent = 700000.25, .nearest_bits = 0x00c8_6229_cc1a_415d },
        .{ .base = 1.001, .exponent = 700000.25, .nearest_bits = 0x7f04_dabc_29e8_60b2 },
        .{ .base = 12345.6789, .exponent = -73.25, .nearest_bits = 0x01b5_358b_e320_e281 },
        .{ .base = 1.23456789, .exponent = 1234.56789, .nearest_bits = 0x5763_ebec_f4b8_50e3 },
    };

    for (cases) |case| {
        const actual_bits: u64 = @bitCast(pow(case.base, case.exponent));
        const distance = if (actual_bits >= case.nearest_bits) actual_bits - case.nearest_bits else case.nearest_bits - actual_bits;
        try std.testing.expect(distance <= 1);
    }
}

test "deterministic F64 power result bits" {
    try std.testing.expectEqual(@as(u64, 0x3f74_380e_2165_6684), @as(u64, @bitCast(pow(0.2, 3.3))));
    try std.testing.expectEqual(@as(u64, 0x40ca_6d41_ca6e_94c6), @as(u64, @bitCast(pow(17.54697502703452, 3.3204523365293763))));
    try std.testing.expectEqual(@as(u64, 0x0010_0000_0000_0000), @as(u64, @bitCast(pow(2.0, -1022.0))));
    try std.testing.expectEqual(@as(u64, 0x0004_0000_0000_0000), @as(u64, @bitCast(pow(2.0, -1024.0))));
    try std.testing.expectEqual(@as(u64, 0x0000_0000_0000_0001), @as(u64, @bitCast(pow(2.0, -1074.0))));
    try std.testing.expectEqual(@as(u64, 0x0000_0000_0000_0000), @as(u64, @bitCast(pow(2.0, -1075.0))));
    try std.testing.expectEqual(@as(u64, 0x8000_0000_0000_0002), @as(u64, @bitCast(pow(-2.0, -1073.0))));
    try std.testing.expectEqual(@as(u64, 0x0000_b815_7268_fdaf), @as(u64, @bitCast(pow(10.0, -309.0))));
    try std.testing.expectEqual(@as(u64, 0x7ede_3bbc_0ae0_45cb), @as(u64, @bitCast(pow(1.8742325878262631, 1111.0207098305914))));
    try std.testing.expectEqual(@as(u64, 0x06ee_dea9_9173_6578), @as(u64, @bitCast(pow(0.5797239088410756, 1159.5420969274194))));
}

test "deterministic F64 trigonometric result bits" {
    try std.testing.expectEqual(@as(u64, 0x3fea_ed54_8f09_0cee), @as(u64, @bitCast(sin(1.0))));
    try std.testing.expectEqual(@as(u64, 0x3fe1_4a28_0fb5_068c), @as(u64, @bitCast(cos(1.0))));
    try std.testing.expectEqual(@as(u64, 0x3ff8_eb24_5cbe_e3a6), @as(u64, @bitCast(tan(1.0))));
    try std.testing.expectEqual(@as(u64, 0x3fe0_c152_382d_7366), @as(u64, @bitCast(asin(0.5))));
    try std.testing.expectEqual(@as(u64, 0x3ff0_c152_382d_7366), @as(u64, @bitCast(acos(0.5))));
    try std.testing.expectEqual(@as(u64, 0x3fe9_21fb_5444_2d18), @as(u64, @bitCast(atan(1.0))));
}

test "deterministic F64 trig branch and reduction bits" {
    const Case = struct { input: u64, sin_bits: u64, cos_bits: u64, tan_bits: u64 };
    const cases = [_]Case{
        .{ .input = 0x3e3f_ffff_ffff_ffff, .sin_bits = 0x3e3f_ffff_ffff_ffff, .cos_bits = 0x3ff0_0000_0000_0000, .tan_bits = 0x3e3f_ffff_ffff_ffff },
        .{ .input = 0x3e40_0000_0000_0000, .sin_bits = 0x3e40_0000_0000_0000, .cos_bits = 0x3ff0_0000_0000_0000, .tan_bits = 0x3e40_0000_0000_0000 },
        .{ .input = 0x3fe9_21fb_5444_2d17, .sin_bits = 0x3fe6_a09e_667f_3bcc, .cos_bits = 0x3fe6_a09e_667f_3bce, .tan_bits = 0x3fef_ffff_ffff_fffd },
        .{ .input = 0x3fe9_21fb_5444_2d18, .sin_bits = 0x3fe6_a09e_667f_3bcc, .cos_bits = 0x3fe6_a09e_667f_3bcd, .tan_bits = 0x3fef_ffff_ffff_ffff },
        .{ .input = 0x3fe9_21fb_5444_2d19, .sin_bits = 0x3fe6_a09e_667f_3bcd, .cos_bits = 0x3fe6_a09e_667f_3bcc, .tan_bits = 0x3ff0_0000_0000_0001 },
        .{ .input = 0x3ff9_21fb_5444_2d17, .sin_bits = 0x3ff0_0000_0000_0000, .cos_bits = 0x3cb4_6989_8cc5_1702, .tan_bits = 0x4329_153d_9443_ed0b },
        .{ .input = 0x3ff9_21fb_5444_2d18, .sin_bits = 0x3ff0_0000_0000_0000, .cos_bits = 0x3c91_a626_3314_5c07, .tan_bits = 0x434d_0296_7c31_cdb5 },
        .{ .input = 0x3ff9_21fb_5444_2d19, .sin_bits = 0x3ff0_0000_0000_0000, .cos_bits = 0xbca7_2cec_e675_d1fd, .tan_bits = 0xc336_17a1_5494_767a },
        .{ .input = 0x4008_0000_0000_0000, .sin_bits = 0x3fc2_1038_6db6_d55b, .cos_bits = 0xbfef_ae04_be85_e5d2, .tan_bits = 0xbfc2_3ef7_1254_b86f },
        .{ .input = 0x4014_0000_0000_0000, .sin_bits = 0xbfee_af81_f5e0_9933, .cos_bits = 0x3fd2_2785_706b_4ad9, .tan_bits = 0xc00b_0b4b_739b_bb07 },
        .{ .input = 0x4042_b999_9999_999a, .sin_bits = 0xbfcf_8ebb_3a97_1b21, .cos_bits = 0x3fef_0320_a2db_4fbe, .tan_bits = 0xbfd0_4806_7579_abdc },
        .{ .input = 0x412e_8480_0000_0000, .sin_bits = 0xbfd6_664b_2568_d867, .cos_bits = 0x3fed_f9df_9906_d32c, .tan_bits = 0xbfd7_e976_8ab7_34c0 },
        .{ .input = 0x4415_af1d_78b5_8c40, .sin_bits = 0xbfe4_a5e6_05fd_6450, .cos_bits = 0x3fe8_7272_0fc6_0d3d, .tan_bits = 0xbfeb_06fb_be99_5394 },
        .{ .input = 0xc415_af1d_78b5_8c40, .sin_bits = 0x3fe4_a5e6_05fd_6450, .cos_bits = 0x3fe8_7272_0fc6_0d3d, .tan_bits = 0x3feb_06fb_be99_5394 },
        .{ .input = 0x54b2_49ad_2594_c37d, .sin_bits = 0xbfd8_5c5e_5b92_9359, .cos_bits = 0x3fed_9757_4968_41f5, .tan_bits = 0xbfda_5807_d6f7_6f7d },
        .{ .input = 0x7fef_ffff_ffff_ffff, .sin_bits = 0x3f74_52fc_98b3_4e97, .cos_bits = 0xbfef_ffe6_2ecf_ab75, .tan_bits = 0xbf74_530c_fe72_9484 },
    };

    for (cases) |case| {
        const input: f64 = @bitCast(case.input);
        try std.testing.expectEqual(case.sin_bits, @as(u64, @bitCast(sin(input))));
        try std.testing.expectEqual(case.cos_bits, @as(u64, @bitCast(cos(input))));
        try std.testing.expectEqual(case.tan_bits, @as(u64, @bitCast(tan(input))));
    }
}

test "deterministic F64 inverse trig branch bits" {
    const Case = struct { input: u64, asin_bits: u64, acos_bits: u64, atan_bits: u64 };
    const cases = [_]Case{
        .{ .input = 0x3c5f_ffff_ffff_ffff, .asin_bits = 0x3c5f_ffff_ffff_ffff, .acos_bits = 0x3ff9_21fb_5444_2d18, .atan_bits = 0x3c5f_ffff_ffff_ffff },
        .{ .input = 0x3c60_0000_0000_0000, .asin_bits = 0x3c60_0000_0000_0000, .acos_bits = 0x3ff9_21fb_5444_2d18, .atan_bits = 0x3c60_0000_0000_0000 },
        .{ .input = 0x3c60_0000_0000_0001, .asin_bits = 0x3c60_0000_0000_0001, .acos_bits = 0x3ff9_21fb_5444_2d18, .atan_bits = 0x3c60_0000_0000_0001 },
        .{ .input = 0x3e3f_ffff_ffff_ffff, .asin_bits = 0x3e3f_ffff_ffff_ffff, .acos_bits = 0x3ff9_21fb_5244_2d18, .atan_bits = 0x3e3f_ffff_ffff_ffff },
        .{ .input = 0x3e40_0000_0000_0000, .asin_bits = 0x3e40_0000_0000_0000, .acos_bits = 0x3ff9_21fb_5244_2d18, .atan_bits = 0x3e40_0000_0000_0000 },
        .{ .input = 0x3e4f_ffff_ffff_ffff, .asin_bits = 0x3e4f_ffff_ffff_ffff, .acos_bits = 0x3ff9_21fb_5044_2d18, .atan_bits = 0x3e4f_ffff_ffff_fffe },
        .{ .input = 0x3e50_0000_0000_0000, .asin_bits = 0x3e50_0000_0000_0000, .acos_bits = 0x3ff9_21fb_5044_2d18, .atan_bits = 0x3e4f_ffff_ffff_ffff },
        .{ .input = 0x3fdb_ffff_ffff_ffff, .asin_bits = 0x3fdc_faf2_7460_fe9e, .acos_bits = 0x3ff1_e33e_b72b_ed71, .atan_bits = 0x3fda_64ee_c3cc_23fc },
        .{ .input = 0x3fdc_0000_0000_0000, .asin_bits = 0x3fdc_faf2_7460_fe9f, .acos_bits = 0x3ff1_e33e_b72b_ed70, .atan_bits = 0x3fda_64ee_c3cc_23fd },
        .{ .input = 0x3fdf_ffff_ffff_ffff, .asin_bits = 0x3fe0_c152_382d_7365, .acos_bits = 0x3ff0_c152_382d_7366, .atan_bits = 0x3fdd_ac67_0561_bb4f },
        .{ .input = 0x3fe0_0000_0000_0000, .asin_bits = 0x3fe0_c152_382d_7366, .acos_bits = 0x3ff0_c152_382d_7366, .atan_bits = 0x3fdd_ac67_0561_bb4f },
        .{ .input = 0x3fe0_0000_0000_0001, .asin_bits = 0x3fe0_c152_382d_7367, .acos_bits = 0x3ff0_c152_382d_7365, .atan_bits = 0x3fdd_ac67_0561_bb51 },
        .{ .input = 0x3fe5_ffff_ffff_ffff, .asin_bits = 0x3fe8_41de_b511_4bb2, .acos_bits = 0x3fea_0217_f377_0e7e, .atan_bits = 0x3fe3_45f0_1cce_37ba },
        .{ .input = 0x3fe6_0000_0000_0000, .asin_bits = 0x3fe8_41de_b511_4bb4, .acos_bits = 0x3fea_0217_f377_0e7d, .atan_bits = 0x3fe3_45f0_1cce_37bb },
        .{ .input = 0x3fef_3332_ffff_ffff, .asin_bits = 0x3ff5_8c2a_e9ab_49e8, .acos_bits = 0x3fcc_ae83_54c7_1987, .atan_bits = 0x3fe8_ba4a_6a9f_194a },
        .{ .input = 0x3fef_3333_0000_0000, .asin_bits = 0x3ff5_8c2a_e9ab_49ea, .acos_bits = 0x3fcc_ae83_54c7_1975, .atan_bits = 0x3fe8_ba4a_6a9f_194a },
        .{ .input = 0xbfe0_0000_0000_0000, .asin_bits = 0xbfe0_c152_382d_7366, .acos_bits = 0x4000_c152_382d_7366, .atan_bits = 0xbfdd_ac67_0561_bb4f },
        .{ .input = 0xbfe8_0000_0000_0000, .asin_bits = 0xbfeb_2353_15c6_80dc, .acos_bits = 0x4003_59d2_6f93_b6c3, .atan_bits = 0xbfe4_978f_a326_9ee1 },
    };

    for (cases) |case| {
        const input: f64 = @bitCast(case.input);
        try std.testing.expectEqual(case.asin_bits, @as(u64, @bitCast(asin(input))));
        try std.testing.expectEqual(case.acos_bits, @as(u64, @bitCast(acos(input))));
        try std.testing.expectEqual(case.atan_bits, @as(u64, @bitCast(atan(input))));
    }

    const AtanCase = struct { input: u64, expected: u64 };
    const atan_cases = [_]AtanCase{
        .{ .input = 0x3ff2_ffff_ffff_ffff, .expected = 0x3feb_de70_ed43_9fe6 },
        .{ .input = 0x3ff3_0000_0000_0000, .expected = 0x3feb_de70_ed43_9fe7 },
        .{ .input = 0x4003_7fff_ffff_ffff, .expected = 0x3ff2_e757_2883_3a54 },
        .{ .input = 0x4003_8000_0000_0000, .expected = 0x3ff2_e757_2883_3a54 },
        .{ .input = 0x440f_ffff_ffff_ffff, .expected = 0x3ff9_21fb_5444_2d18 },
        .{ .input = 0x4410_0000_0000_0000, .expected = 0x3ff9_21fb_5444_2d18 },
        .{ .input = 0xc003_8000_0000_0000, .expected = 0xbff2_e757_2883_3a54 },
    };
    for (atan_cases) |case| {
        try std.testing.expectEqual(case.expected, @as(u64, @bitCast(atan(@bitCast(case.input)))));
    }
}
