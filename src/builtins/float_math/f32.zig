//! Deterministic, binary32-only implementations of Roc's F32 transcendentals.
//!
//! Every floating-point value and operation in this file is binary32. In
//! particular, argument reduction does not widen to a wider floating-point
//! format: it multiplies the binary32 significand by a fixed-point expansion
//! of 2/pi, then converts the reduced fixed-point fraction directly to f32.

const std = @import("std");

const canonical_nan: f32 = @bitCast(@as(u32, 0x7fc0_0000));
const positive_infinity: f32 = @bitCast(@as(u32, 0x7f80_0000));

const Fixed = [5]u64;

// floor((2/pi) * 2^256), least-significant limb first.
const two_over_pi = [4]u64{
    0xfe51_63ab_debb_c561,
    0xdb62_9599_3c43_9041,
    0xfc27_57d1_f534_ddc0,
    0xa2f9_836e_4e44_1529,
};

const Reduction = struct {
    quadrant: u2,
    remainder: f32,
};

fn multiplyTwoOverPi(significand: u32) Fixed {
    var product = [_]u64{0} ** 5;
    var carry: u128 = 0;

    for (two_over_pi, 0..) |limb, index| {
        const wide = @as(u128, limb) * @as(u128, significand) + carry;
        product[index] = @truncate(wide);
        carry = wide >> 64;
    }
    product[4] = @truncate(carry);
    return product;
}

fn fixedBit(value: Fixed, bit_index: u16) bool {
    if (bit_index >= 320) return false;
    const limb: usize = @intCast(bit_index / 64);
    const bit: u6 = @intCast(bit_index % 64);
    return value[limb] & (@as(u64, 1) << bit) != 0;
}

fn lowerBits(value: Fixed, bit_count: u16) Fixed {
    var result = [_]u64{0} ** 5;
    const whole_limbs: usize = @intCast(bit_count / 64);
    const partial_bits: u6 = @intCast(bit_count % 64);

    var index: usize = 0;
    while (index < @min(whole_limbs, result.len)) : (index += 1) {
        result[index] = value[index];
    }
    if (whole_limbs < result.len and partial_bits != 0) {
        result[whole_limbs] = value[whole_limbs] & ((@as(u64, 1) << partial_bits) - 1);
    }
    return result;
}

fn powerOfTwoMinus(value: Fixed, exponent: u16) Fixed {
    std.debug.assert(exponent < 320);

    var result = [_]u64{0} ** 5;
    const limb: usize = @intCast(exponent / 64);
    const bit: u6 = @intCast(exponent % 64);
    result[limb] = @as(u64, 1) << bit;

    var borrow: u1 = 0;
    for (&result, value) |*result_limb, value_limb| {
        const subtrahend = @as(u128, value_limb) + @as(u128, borrow);
        const minuend = @as(u128, result_limb.*);
        result_limb.* = @truncate(minuend -% subtrahend);
        borrow = @intFromBool(minuend < subtrahend);
    }
    std.debug.assert(borrow == 0);
    return result;
}

fn highestSetBit(value: Fixed) ?u16 {
    var index: usize = value.len;
    while (index != 0) {
        index -= 1;
        const limb = value[index];
        if (limb != 0) {
            return @intCast(index * 64 + (63 - @clz(limb)));
        }
    }
    return null;
}

fn shiftedLowU32(value: Fixed, shift: u16) u32 {
    if (shift >= 320) return 0;
    const limb: usize = @intCast(shift / 64);
    const bit: u6 = @intCast(shift % 64);
    var result: u64 = value[limb] >> bit;
    if (bit != 0 and limb + 1 < value.len) {
        result |= value[limb + 1] << @intCast(64 - @as(u7, bit));
    }
    return @truncate(result);
}

fn anyBitsBelow(value: Fixed, bit_count: u16) bool {
    const capped = @min(bit_count, 320);
    const whole_limbs: usize = @intCast(capped / 64);
    const partial_bits: u6 = @intCast(capped % 64);

    for (value[0..whole_limbs]) |limb| {
        if (limb != 0) return true;
    }
    if (whole_limbs < value.len and partial_bits != 0) {
        const mask = (@as(u64, 1) << partial_bits) - 1;
        return value[whole_limbs] & mask != 0;
    }
    return false;
}

fn roundedShift(value: Fixed, shift: i16) u32 {
    if (shift <= 0) {
        const left: u5 = @intCast(-shift);
        return shiftedLowU32(value, 0) << left;
    }

    const right: u16 = @intCast(shift);
    var result = shiftedLowU32(value, right);
    const halfway = fixedBit(value, right - 1);
    const below_halfway = anyBitsBelow(value, right - 1);
    if (halfway and (below_halfway or result & 1 != 0)) result += 1;
    return result;
}

fn fixedFractionToF32(magnitude: Fixed, denominator_exponent: u16) f32 {
    const highest = highestSetBit(magnitude) orelse return 0.0;
    var unbiased_exponent: i16 = @as(i16, @intCast(highest)) - @as(i16, @intCast(denominator_exponent));

    if (unbiased_exponent >= -126) {
        var significand = roundedShift(magnitude, @as(i16, @intCast(highest)) - 23);
        if (significand == 0x0100_0000) {
            significand >>= 1;
            unbiased_exponent += 1;
        }
        const biased_exponent: u32 = @intCast(unbiased_exponent + 127);
        return @bitCast((biased_exponent << 23) | (significand & 0x007f_ffff));
    }

    const subnormal = roundedShift(magnitude, @as(i16, @intCast(denominator_exponent)) - 149);
    return @bitCast(subnormal);
}

fn reduce(value: f32) Reduction {
    const bits: u32 = @bitCast(value);
    const abs_bits = bits & 0x7fff_ffff;
    const exponent_bits = (abs_bits >> 23) & 0xff;
    std.debug.assert(exponent_bits != 0 and exponent_bits != 0xff);

    const significand = (abs_bits & 0x007f_ffff) | 0x0080_0000;
    const binary_exponent = @as(i16, @intCast(exponent_bits)) - 150;
    const denominator_exponent: u16 = @intCast(256 - binary_exponent);
    const product = multiplyTwoOverPi(significand);

    const rounds_up = fixedBit(product, denominator_exponent - 1);
    var quadrant: u2 = @truncate(@as(u2, @intFromBool(fixedBit(product, denominator_exponent))) |
        (@as(u2, @intFromBool(fixedBit(product, denominator_exponent + 1))) << 1));
    if (rounds_up) quadrant +%= 1;

    const fraction = lowerBits(product, denominator_exponent);
    const magnitude = if (rounds_up)
        powerOfTwoMinus(fraction, denominator_exponent)
    else
        fraction;

    var reduced_fraction = fixedFractionToF32(magnitude, denominator_exponent);
    const input_is_negative = bits >> 31 != 0;
    if (rounds_up != input_is_negative) reduced_fraction = -reduced_fraction;
    if (input_is_negative) quadrant = 0 -% quadrant;

    const pio2_hi: f32 = 1.5707962513e+00;
    const pio2_lo: f32 = 7.5497894159e-08;
    return .{
        .quadrant = quadrant,
        .remainder = reduced_fraction * pio2_hi + reduced_fraction * pio2_lo,
    };
}

fn sinKernel(x: f32) f32 {
    const bits: u32 = @bitCast(x);
    if (bits & 0x7fff_ffff < 0x3980_0000) return x;

    const s1: f32 = -1.6666667163e-01;
    const s2: f32 = 8.3333291113e-03;
    const s3: f32 = -1.9839334413e-04;
    const s4: f32 = 2.7183114939e-06;
    const z = x * x;
    return x + x * z * (s1 + z * (s2 + z * (s3 + z * s4)));
}

fn cosKernel(x: f32) f32 {
    const c0: f32 = -4.9999997020e-01;
    const c1: f32 = 4.1666623205e-02;
    const c2: f32 = -1.3886763481e-03;
    const c3: f32 = 2.4390447366e-05;
    const z = x * x;
    const w = z * z;
    return (1.0 + z * c0) + w * c1 + w * z * (c2 + z * c3);
}

const SinCos = struct { sin: f32, cos: f32 };

fn sinCos(value: f32) SinCos {
    const bits: u32 = @bitCast(value);
    const abs_bits = bits & 0x7fff_ffff;
    if (abs_bits >= 0x7f80_0000) {
        const nan = value - value;
        return .{ .sin = nan, .cos = nan };
    }

    const reduction: Reduction = if (abs_bits <= 0x3f49_0fda)
        .{ .quadrant = 0, .remainder = value }
    else
        reduce(value);
    const reduced_sin = sinKernel(reduction.remainder);
    const reduced_cos = cosKernel(reduction.remainder);

    return switch (reduction.quadrant) {
        0 => .{ .sin = reduced_sin, .cos = reduced_cos },
        1 => .{ .sin = reduced_cos, .cos = -reduced_sin },
        2 => .{ .sin = -reduced_sin, .cos = -reduced_cos },
        3 => .{ .sin = -reduced_cos, .cos = reduced_sin },
    };
}

/// Returns the sine of `value`, computed entirely with binary32 operations.
pub fn sin(value: f32) f32 {
    return sinCos(value).sin;
}

/// Returns the cosine of `value`, computed entirely with binary32 operations.
pub fn cos(value: f32) f32 {
    return sinCos(value).cos;
}

/// Returns the tangent of `value`, computed entirely with binary32 operations.
pub fn tan(value: f32) f32 {
    const result = sinCos(value);
    return result.sin / result.cos;
}

fn inverseRational(z: f32) f32 {
    const p0: f32 = 1.6666586697e-01;
    const p1: f32 = -4.2743422091e-02;
    const p2: f32 = -8.6563630030e-03;
    const q1: f32 = -7.0662963390e-01;
    const numerator = z * (p0 + z * (p1 + z * p2));
    const denominator = 1.0 + z * q1;
    return numerator / denominator;
}

/// Returns the inverse sine of `value`, computed entirely with binary32 operations.
pub fn asin(value: f32) f32 {
    const pio2_hi: f32 = 1.5707962513e+00;
    const pio2_lo: f32 = 7.5497894159e-08;
    const bits: u32 = @bitCast(value);
    const abs_bits = bits & 0x7fff_ffff;

    if (abs_bits >= 0x3f80_0000) {
        if (abs_bits == 0x3f80_0000) return if (bits >> 31 == 0) pio2_hi + pio2_lo else -(pio2_hi + pio2_lo);
        return canonical_nan;
    }
    if (abs_bits < 0x3f00_0000) {
        if (abs_bits < 0x3980_0000) return value;
        return value + value * inverseRational(value * value);
    }

    const z = (1.0 - @abs(value)) * 0.5;
    const root = @sqrt(z);
    const ratio = inverseRational(z);
    const local = pio2_hi - (2.0 * (root + root * ratio) - pio2_lo);
    return if (bits >> 31 == 0) local else -local;
}

/// Returns the inverse cosine of `value`, computed entirely with binary32 operations.
pub fn acos(value: f32) f32 {
    const pio2_hi: f32 = 1.5707962513e+00;
    const pio2_lo: f32 = 7.5497894159e-08;
    const bits: u32 = @bitCast(value);
    const abs_bits = bits & 0x7fff_ffff;

    if (abs_bits >= 0x3f80_0000) {
        if (abs_bits == 0x3f80_0000) return if (bits >> 31 == 0) 0.0 else 2.0 * (pio2_hi + pio2_lo);
        return canonical_nan;
    }
    if (abs_bits < 0x3f00_0000) {
        if (abs_bits <= 0x3280_0000) return pio2_hi + pio2_lo;
        return pio2_hi - (value - (pio2_lo - value * inverseRational(value * value)));
    }
    if (bits >> 31 != 0) {
        const z = (1.0 + value) * 0.5;
        const root = @sqrt(z);
        const correction = inverseRational(z) * root - pio2_lo;
        return 2.0 * (pio2_hi - (root + correction));
    }

    const z = (1.0 - value) * 0.5;
    const root = @sqrt(z);
    const root_bits: u32 = @bitCast(root);
    const root_hi: f32 = @bitCast(root_bits & 0xffff_f000);
    const correction = (z - root_hi * root_hi) / (root + root_hi);
    const tail = inverseRational(z) * root + correction;
    return 2.0 * (root_hi + tail);
}

/// Returns the inverse tangent of `value`, computed entirely with binary32 operations.
pub fn atan(value: f32) f32 {
    const high = [_]f32{ 4.6364760399e-01, 7.8539812565e-01, 9.8279368877e-01, 1.5707962513e+00 };
    const low = [_]f32{ 5.0121582440e-09, 3.7748947079e-08, 3.4473217170e-08, 7.5497894159e-08 };
    const coefficients = [_]f32{ 3.3333328366e-01, -1.9999158382e-01, 1.4253635705e-01, -1.0648017377e-01, 6.1687607318e-02 };

    const bits: u32 = @bitCast(value);
    const abs_bits = bits & 0x7fff_ffff;
    const negative = bits >> 31 != 0;
    if (abs_bits >= 0x4c80_0000) {
        if (abs_bits > 0x7f80_0000) return value;
        const result = high[3] + low[3];
        return if (negative) -result else result;
    }

    var reduced: f32 = undefined;
    var identity: ?u2 = null;
    if (abs_bits < 0x3ee0_0000) {
        if (abs_bits < 0x3980_0000) return value;
        reduced = value;
    } else {
        const magnitude = @abs(value);
        if (abs_bits < 0x3f98_0000) {
            if (abs_bits < 0x3f30_0000) {
                reduced = (2.0 * magnitude - 1.0) / (2.0 + magnitude);
                identity = 0;
            } else {
                reduced = (magnitude - 1.0) / (magnitude + 1.0);
                identity = 1;
            }
        } else if (abs_bits < 0x401c_0000) {
            reduced = (magnitude - 1.5) / (1.0 + 1.5 * magnitude);
            identity = 2;
        } else {
            reduced = -1.0 / magnitude;
            identity = 3;
        }
    }

    const z = reduced * reduced;
    const w = z * z;
    const odd = z * (coefficients[0] + w * (coefficients[2] + w * coefficients[4]));
    const even = w * (coefficients[1] + w * coefficients[3]);
    if (identity) |id| {
        const index: usize = id;
        const result = high[index] - ((reduced * (odd + even) - low[index]) - reduced);
        return if (negative) -result else result;
    }
    return reduced - reduced * (odd + even);
}

/// Returns the natural logarithm of `value`, computed entirely with binary32 operations.
pub fn log(value: f32) f32 {
    const ln2_hi: f32 = 6.9313812256e-01;
    const ln2_lo: f32 = 9.0580006145e-06;
    const lg1: f32 = 0xaaaaaa.0p-24;
    const lg2: f32 = 0xccce13.0p-25;
    const lg3: f32 = 0x91e9ee.0p-25;
    const lg4: f32 = 0xf89e26.0p-26;

    var x = value;
    var bits: u32 = @bitCast(x);
    var exponent: i32 = 0;
    if (bits < 0x0080_0000 or bits >> 31 != 0) {
        if (bits << 1 == 0) return -positive_infinity;
        if (bits >> 31 != 0) return canonical_nan;
        exponent -= 25;
        x *= 0x1.0p25;
        bits = @bitCast(x);
    } else if (bits >= 0x7f80_0000) {
        return x;
    } else if (bits == 0x3f80_0000) {
        return 0.0;
    }

    bits += 0x3f80_0000 - 0x3f35_04f3;
    exponent += @as(i32, @intCast(bits >> 23)) - 0x7f;
    bits = (bits & 0x007f_ffff) + 0x3f35_04f3;
    x = @bitCast(bits);

    const f = x - 1.0;
    const s = f / (2.0 + f);
    const z = s * s;
    const w = z * z;
    const approximation = z * (lg1 + w * lg3) + w * (lg2 + w * lg4);
    const half_square = 0.5 * f * f;
    const float_exponent: f32 = @floatFromInt(exponent);
    return s * (half_square + approximation) + float_exponent * ln2_lo - half_square + f + float_exponent * ln2_hi;
}

fn scalePowerOfTwo(value: f32, power: i32) f32 {
    var bits: u32 = @bitCast(value);
    const sign = bits & 0x8000_0000;
    var exponent: i32 = @intCast((bits >> 23) & 0xff);
    if (exponent == 0xff or bits & 0x7fff_ffff == 0) return value;

    var adjusted_power = power;
    if (exponent == 0) {
        const scaled = value * 0x1.0p24;
        bits = @bitCast(scaled);
        exponent = @as(i32, @intCast((bits >> 23) & 0xff)) - 24;
    }

    const new_exponent = exponent + adjusted_power;
    if (new_exponent >= 0xff) return @bitCast(sign | 0x7f80_0000);
    if (new_exponent > 0) return @bitCast((bits & 0x807f_ffff) | (@as(u32, @intCast(new_exponent)) << 23));
    if (new_exponent <= -24) return @bitCast(sign);

    adjusted_power = new_exponent + 24;
    const normal_bits = (bits & 0x807f_ffff) | (@as(u32, @intCast(adjusted_power)) << 23);
    const normal: f32 = @bitCast(normal_bits);
    return normal * 0x1.0p-24;
}

fn exp(value: f32) f32 {
    const half = [_]f32{ 0.5, -0.5 };
    const ln2_hi: f32 = 6.9314575195e-1;
    const ln2_lo: f32 = 1.4286067653e-6;
    const inv_ln2: f32 = 1.4426950216e+0;
    const p1: f32 = 1.6666625440e-1;
    const p2: f32 = -2.7667332906e-3;

    var x = value;
    var abs_bits: u32 = @bitCast(x);
    const sign: usize = abs_bits >> 31;
    abs_bits &= 0x7fff_ffff;
    if (abs_bits > 0x7f80_0000) return x;
    if (abs_bits >= 0x42ae_ac50) {
        if (abs_bits >= 0x42b1_7218 and sign == 0) return positive_infinity;
        if (sign != 0 and abs_bits >= 0x42cf_f1b5) return 0.0;
    }

    var power: i32 = 0;
    var high: f32 = x;
    var low: f32 = 0.0;
    if (abs_bits > 0x3eb1_7218) {
        if (abs_bits > 0x3f85_1592) {
            power = @intFromFloat(inv_ln2 * x + half[sign]);
        } else {
            power = if (sign == 0) 1 else -1;
        }
        const float_power: f32 = @floatFromInt(power);
        high = x - float_power * ln2_hi;
        low = float_power * ln2_lo;
        x = high - low;
    } else if (abs_bits <= 0x3900_0000) {
        return 1.0 + x;
    }

    const square = x * x;
    const correction = x - square * (p1 + square * p2);
    const result = 1.0 + (x * correction / (2.0 - correction) - low + high);
    return if (power == 0) result else scalePowerOfTwo(result, power);
}

fn isOddInteger(value: f32) bool {
    const abs_bits = @as(u32, @bitCast(value)) & 0x7fff_ffff;
    if (abs_bits >= 0x4b80_0000) return false;
    if (@trunc(value) != value) return false;
    const integer: i32 = @intFromFloat(value);
    return integer & 1 != 0;
}

fn integerPower(base: f32, exponent: f32) f32 {
    var remaining: u32 = @intFromFloat(@abs(exponent));
    var factor = base;
    var result: f32 = 1.0;
    while (remaining != 0) : (remaining >>= 1) {
        if (remaining & 1 != 0) result *= factor;
        factor *= factor;
    }
    return if (exponent < 0.0) 1.0 / result else result;
}

/// Returns `base` raised to `exponent`, computed entirely with binary32 operations.
pub fn pow(base: f32, exponent: f32) f32 {
    if (exponent == 0.0 or base == 1.0) return 1.0;
    const base_bits: u32 = @bitCast(base);
    const exponent_bits: u32 = @bitCast(exponent);
    const base_abs_bits = base_bits & 0x7fff_ffff;
    const exponent_abs_bits = exponent_bits & 0x7fff_ffff;
    if (base_abs_bits > 0x7f80_0000 or exponent_abs_bits > 0x7f80_0000) return canonical_nan;
    if (exponent == 1.0) return base;

    if (base_abs_bits == 0) {
        if (exponent < 0.0) return if (isOddInteger(exponent)) @bitCast((base_bits & 0x8000_0000) | 0x7f80_0000) else positive_infinity;
        return if (isOddInteger(exponent)) base else 0.0;
    }

    if (exponent_abs_bits == 0x7f80_0000) {
        if (base == -1.0) return 1.0;
        const tends_to_zero = (@abs(base) < 1.0) == (exponent > 0.0);
        return if (tends_to_zero) 0.0 else positive_infinity;
    }
    if (base_abs_bits == 0x7f80_0000) {
        if (base_bits >> 31 != 0) {
            const reciprocal: f32 = @bitCast(base_bits & 0x8000_0000);
            return pow(reciprocal, -exponent);
        }
        return if (exponent < 0.0) 0.0 else positive_infinity;
    }
    if (exponent == 0.5) return @sqrt(base);
    if (exponent == -0.5) return 1.0 / @sqrt(base);

    const exponent_is_integer = @trunc(exponent) == exponent;
    if (base < 0.0 and !exponent_is_integer) return canonical_nan;
    if (exponent_is_integer and exponent_abs_bits < 0x4b80_0000) return integerPower(base, exponent);

    var result = exp(exponent * log(@abs(base)));
    if (base < 0.0 and isOddInteger(exponent)) result = -result;
    return result;
}

test "fixed-point argument reduction covers the binary32 exponent range" {
    const cases = [_]f32{ 1.0, -1.0, 1000.0, -1000.0, @bitCast(@as(u32, 0x7f7f_ffff)) };
    for (cases) |value| {
        const reduced = reduce(value);
        try std.testing.expect(@abs(reduced.remainder) <= 0.7853982);
    }
}

test "F32 transcendental special cases" {
    try std.testing.expectEqual(@as(u32, 0x8000_0000), @as(u32, @bitCast(sin(-0.0))));
    try std.testing.expectEqual(@as(f32, 1.0), cos(0.0));
    try std.testing.expectEqual(@as(u32, 0x8000_0000), @as(u32, @bitCast(tan(-0.0))));
    try std.testing.expectEqual(@as(f32, 8.0), pow(2.0, 3.0));
    try std.testing.expect(std.math.isNan(asin(2.0)));
    try std.testing.expect(std.math.isNan(acos(-2.0)));
}

test "F32 transcendental approximations" {
    const tolerance: f32 = 0.000003;
    try std.testing.expectApproxEqAbs(@as(f32, 0.84147096), sin(1.0), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 0.5403023), cos(1.0), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 1.5574077), tan(1.0), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 0.5235988), asin(0.5), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 1.0471976), acos(0.5), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 0.7853982), atan(1.0), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 0.6931472), log(2.0), tolerance);
    try std.testing.expectApproxEqAbs(@as(f32, 0.004936), pow(0.2, 3.3), tolerance);
}

test "deterministic F32 result bits" {
    try std.testing.expectEqual(@as(u32, 0x3f57_6aa5), @as(u32, @bitCast(sin(1.0))));
    try std.testing.expectEqual(@as(u32, 0x3f0a_5140), @as(u32, @bitCast(cos(1.0))));
    try std.testing.expectEqual(@as(u32, 0x3fc7_5924), @as(u32, @bitCast(tan(1.0))));
    try std.testing.expectEqual(@as(u32, 0x3f06_0a92), @as(u32, @bitCast(asin(0.5))));
    try std.testing.expectEqual(@as(u32, 0x3f86_0a92), @as(u32, @bitCast(acos(0.5))));
    try std.testing.expectEqual(@as(u32, 0x3f49_0fdb), @as(u32, @bitCast(atan(1.0))));
    try std.testing.expectEqual(@as(u32, 0x3f31_7218), @as(u32, @bitCast(log(2.0))));
    try std.testing.expectEqual(@as(u32, 0x3ba1_c06f), @as(u32, @bitCast(pow(0.2, 3.3))));
}
