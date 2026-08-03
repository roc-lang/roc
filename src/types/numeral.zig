//! The single authority for numeric-literal value questions.
//!
//! A numeric literal's exact value is `sign * (before + after/10^scale)`,
//! where `before` and `after` are big-endian base-256 digit magnitudes
//! recorded by the parser (see src/parse/NumericLiteral.zig). This module
//! owns the only implementations of:
//!
//!   - `fits`/`computeFitSet`: whether that exact value is representable in a
//!     builtin numeric type (digit-count prefilters + u128 checked arithmetic,
//!     escalating to big-integer arithmetic only for adversarial digit counts).
//!   - `intBits`/`decBits`/`floatBits`: the bit pattern the value denotes in a
//!     concrete builtin type. Float conversion is correctly rounded
//!     (round-to-nearest, ties-to-even) straight from the binary digit facts —
//!     no decimal text is ever reconstructed.
//!
//! Every stage that needs one of these answers (checking, monotype lowering,
//! the interpreter's runtime `from_numeral`, compile-time folding) calls in
//! here, so the answers cannot drift apart between stages or engines.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// The builtin numeric types a literal can convert to.
pub const Target = enum(u4) {
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

/// The set of builtin numeric types that can represent a literal's exact
/// value. Computed once per literal by `computeFitSet`; intersected when two
/// literal type variables unify.
pub const FitSet = std.EnumSet(Target);

/// Dec's fractional precision: values are scaled by 10^18. This must equal
/// `builtins.dec.RocDec.decimal_places`, which is the canonical definition; the
/// `types` module cannot import `builtins`, so the value is restated here.
pub const dec_decimal_places: u32 = 18;
/// 10^18, the Dec scaling factor. This must equal
/// `builtins.dec.RocDec.one_point_zero_i128`, the canonical definition; the
/// `types` module cannot import `builtins`, so the value is restated here.
pub const dec_one: u128 = 1_000_000_000_000_000_000;

/// Borrowed view of a literal's exact digit facts.
///
/// The magnitude is `before + after/10^scale`; `after` counts `scale` decimal
/// digits, so `after < 10^scale` always. `is_fractional` is the syntactic
/// fact "written with a decimal point or nonzero fractional digits" — `1e5`
/// is not fractional even though it parses from a `frac` token.
pub const Exact = struct {
    /// Digits before the decimal point (after folding in any exponent),
    /// big-endian base-256, no leading zero bytes.
    before: []const u8,
    /// Digits after the decimal point as an integer magnitude, big-endian
    /// base-256. The digit string has exactly `scale` decimal digits.
    after: []const u8,
    /// Count of decimal digits after the point.
    scale: u32,
    is_negative: bool,
    is_fractional: bool,
};

/// Read a big-endian base-256 magnitude as u128, or null if it overflows.
pub fn magnitudeU128(bytes_be: []const u8) ?u128 {
    // More than 16 bytes cannot fit u128 (leading zero bytes are never stored).
    if (bytes_be.len > 16) return null;
    var value: u128 = 0;
    for (bytes_be) |byte| {
        value = (value << 8) | byte;
    }
    return value;
}

fn pow10U128(exponent: u32) ?u128 {
    // 10^38 is the largest power of ten that fits in u128.
    if (exponent > 38) return null;
    var value: u128 = 1;
    var remaining = exponent;
    while (remaining > 0) : (remaining -= 1) {
        value *= 10;
    }
    return value;
}

fn checkedMulU128(lhs: u128, rhs: u128) ?u128 {
    const product = @mulWithOverflow(lhs, rhs);
    if (product[1] != 0) return null;
    return product[0];
}

fn checkedAddU128(lhs: u128, rhs: u128) ?u128 {
    const sum = @addWithOverflow(lhs, rhs);
    if (sum[1] != 0) return null;
    return sum[0];
}

// int //

/// The literal's integer magnitude, or null when the literal is fractional
/// or exceeds u128. A magnitude alone does not mean the literal fits any
/// particular integer type; pair with `intTargetAccepts`.
pub fn intMagnitude(exact: Exact) ?u128 {
    if (exact.is_fractional) return null;
    return magnitudeU128(exact.before);
}

/// Whether an integer target type can represent `sign * magnitude`.
/// A syntactic minus on a zero magnitude is accepted by unsigned types.
pub fn intTargetAccepts(target: Target, magnitude: u128, is_negative: bool) bool {
    const max_positive: u128, const max_negative: u128 = switch (target) {
        .u8 => .{ std.math.maxInt(u8), 0 },
        .u16 => .{ std.math.maxInt(u16), 0 },
        .u32 => .{ std.math.maxInt(u32), 0 },
        .u64 => .{ std.math.maxInt(u64), 0 },
        .u128 => .{ std.math.maxInt(u128), 0 },
        .i8 => .{ std.math.maxInt(i8), @as(u128, std.math.maxInt(i8)) + 1 },
        .i16 => .{ std.math.maxInt(i16), @as(u128, std.math.maxInt(i16)) + 1 },
        .i32 => .{ std.math.maxInt(i32), @as(u128, std.math.maxInt(i32)) + 1 },
        .i64 => .{ std.math.maxInt(i64), @as(u128, std.math.maxInt(i64)) + 1 },
        .i128 => .{ std.math.maxInt(i128), @as(u128, std.math.maxInt(i128)) + 1 },
        .f32, .f64, .dec => return false,
    };
    if (is_negative and magnitude != 0) return magnitude <= max_negative;
    return magnitude <= max_positive;
}

/// The literal's value in a signed or unsigned 128-bit slot, or null when the
/// target integer type cannot represent it exactly.
pub const IntBits = union(enum) {
    /// The value, sign included, for signed targets (and any value ≤ maxInt(i128)).
    i128: i128,
    /// Magnitudes above maxInt(i128); only produced for the u128 target.
    u128: u128,
};

/// The exact bit pattern for the literal at an integer target type, or null
/// when the value is not representable there.
pub fn intBits(exact: Exact, target: Target) ?IntBits {
    const magnitude = intMagnitude(exact) orelse return null;
    if (!intTargetAccepts(target, magnitude, exact.is_negative)) return null;
    if (exact.is_negative and magnitude != 0) {
        const max_negative = @as(u128, std.math.maxInt(i128)) + 1;
        std.debug.assert(magnitude <= max_negative);
        if (magnitude == max_negative) return .{ .i128 = std.math.minInt(i128) };
        return .{ .i128 = -@as(i128, @intCast(magnitude)) };
    }
    if (magnitude > std.math.maxInt(i128)) return .{ .u128 = magnitude };
    return .{ .i128 = @intCast(magnitude) };
}

// dec //

/// The literal's exact value scaled by 10^18 (the Dec bit pattern), or null
/// when Dec cannot represent it exactly. Fractional digits beyond 18 places
/// are accepted only when they are all zeros (the value, not the spelling,
/// decides). Escalates to big-integer arithmetic only when the fractional
/// digit magnitude exceeds u128.
pub fn decBits(allocator: Allocator, exact: Exact) Allocator.Error!?i128 {
    const before = magnitudeU128(exact.before) orelse return null;
    const before_scaled = checkedMulU128(before, dec_one) orelse return null;

    var after_normalized: u128 = undefined;
    var scale_normalized: u32 = undefined;
    if (exact.scale <= dec_decimal_places) {
        // `after` has at most 18 decimal digits, so it always fits u128.
        after_normalized = magnitudeU128(exact.after) orelse unreachable;
        scale_normalized = exact.scale;
    } else if (magnitudeU128(exact.after)) |after| {
        // Digits beyond 18 places must all be zeros for the value to be
        // representable; strip them and renormalize.
        const excess = exact.scale - dec_decimal_places;
        if (pow10U128(excess)) |divisor| {
            if (after % divisor != 0) return null;
            after_normalized = after / divisor;
        } else {
            // 10^excess exceeds u128 ≥ after, so only a zero magnitude divides.
            if (after != 0) return null;
            after_normalized = 0;
        }
        scale_normalized = dec_decimal_places;
    } else {
        // Fractional magnitude beyond u128: divide out the excess places with
        // big-integer arithmetic; a nonzero remainder is unrepresentable.
        const quotient = (try divOutExcessDecimalPlaces(allocator, exact.after, exact.scale - dec_decimal_places)) orelse return null;
        after_normalized = quotient;
        scale_normalized = dec_decimal_places;
    }

    const after_scale = pow10U128(dec_decimal_places - scale_normalized) orelse unreachable;
    const after_scaled = checkedMulU128(after_normalized, after_scale) orelse return null;
    const magnitude = checkedAddU128(before_scaled, after_scaled) orelse return null;

    const max_positive: u128 = @intCast(std.math.maxInt(i128));
    const max_negative = max_positive + 1;
    if (exact.is_negative) {
        if (magnitude > max_negative) return null;
        if (magnitude == max_negative) return std.math.minInt(i128);
        return -@as(i128, @intCast(magnitude));
    }
    if (magnitude > max_positive) return null;
    return @intCast(magnitude);
}

/// Divide the (huge) fractional magnitude by 10^excess; return the quotient
/// if it fits u128 and the division is exact, else null.
fn divOutExcessDecimalPlaces(allocator: Allocator, after_be: []const u8, excess: u32) Allocator.Error!?u128 {
    // 10^excess = 2^excess · 5^excess, so `after` must have at least `excess`
    // trailing zero BITS to divide exactly. This costs one byte scan and
    // rejects virtually every adversarial huge-scale literal before any
    // big-integer work — the cheap counterpart of the in-u128 branch's
    // `after % divisor` guard in `decBits`.
    if (trailingZeroBits(after_be) < excess) return null;
    const big = std.math.big.int.Managed;
    var after_big = try bigFromBytes(allocator, after_be);
    defer after_big.deinit();
    var divisor = try big.init(allocator);
    defer divisor.deinit();
    var ten = try big.initSet(allocator, 10);
    defer ten.deinit();
    try divisor.pow(&ten, excess);
    var quotient = try big.init(allocator);
    defer quotient.deinit();
    var remainder = try big.init(allocator);
    defer remainder.deinit();
    try quotient.divTrunc(&remainder, &after_big, &divisor);
    if (!remainder.eqlZero()) return null;
    return quotient.toConst().toInt(u128) catch null;
}

/// Trailing zero bits of a big-endian magnitude, saturating at u32 max for a
/// zero magnitude (every power of two divides zero).
fn trailingZeroBits(bytes_be: []const u8) u32 {
    var zero_bits: u32 = 0;
    var i = bytes_be.len;
    while (i > 0) {
        i -= 1;
        const byte = bytes_be[i];
        if (byte != 0) return zero_bits + @ctz(byte);
        zero_bits += 8;
    }
    return std.math.maxInt(u32);
}

// float //

fn FloatSpec(comptime F: type) type {
    return struct {
        /// Mantissa precision in bits, implicit leading bit included.
        const precision = std.math.floatMantissaBits(F) + 1;
        /// Minimum normal exponent (of the implicit leading bit).
        const exponent_min = std.math.floatExponentMin(F);
        /// Maximum exponent (of the implicit leading bit).
        const exponent_max = std.math.floatExponentMax(F);
        /// Fixed-point shift used for fractional conversion: bit i of
        /// `floor(value * 2^shift)` has exponent `i - shift`. Chosen so even
        /// the smallest subnormal's rounding bits are integer bits, with
        /// margin so the ULP cutoff below stays positive.
        const fixed_point_shift = -(exponent_min - (precision - 1)) + 8;
        const Mantissa = std.meta.Int(.unsigned, precision + 1);
    };
}

/// The correctly-rounded (round-to-nearest, ties-to-even) value of the
/// literal at a binary floating-point type. Total: values beyond the target's
/// range become ±inf, exactly as an IEEE decimal→binary conversion would.
pub fn floatBits(comptime F: type, allocator: Allocator, exact: Exact) Allocator.Error!F {
    comptime std.debug.assert(F == f32 or F == f64);
    const magnitude = try floatMagnitude(F, allocator, exact);
    return if (exact.is_negative) -magnitude else magnitude;
}

fn floatMagnitude(comptime F: type, allocator: Allocator, exact: Exact) Allocator.Error!F {
    // Fast path (Clinger): when the combined digits M = before*10^scale + after
    // and the power 10^scale are both exactly representable in F, the single
    // IEEE division M / 10^scale is correctly rounded.
    if (clingerFastPath(F, exact)) |value| return value;

    if (exact.scale == 0 or magnitudeIsZero(exact.after)) {
        // Integer-valued: round the `before` magnitude directly.
        return roundBytesToFloat(F, allocator, exact.before);
    }

    return floatSlowPath(F, allocator, exact);
}

fn magnitudeIsZero(bytes_be: []const u8) bool {
    for (bytes_be) |byte| {
        if (byte != 0) return false;
    }
    return true;
}

fn clingerFastPath(comptime F: type, exact: Exact) ?F {
    // Largest power of ten exactly representable: f64 10^22, f32 10^10.
    const max_pow10: u32 = if (F == f64) 22 else 10;
    const max_mantissa: u128 = 1 << (std.math.floatMantissaBits(F) + 1);
    if (exact.scale > max_pow10) return null;
    const before = magnitudeU128(exact.before) orelse return null;
    const after = magnitudeU128(exact.after) orelse return null;
    const shift = pow10U128(exact.scale) orelse return null;
    const before_scaled = checkedMulU128(before, shift) orelse return null;
    const combined = checkedAddU128(before_scaled, after) orelse return null;
    if (combined > max_mantissa) return null;
    const numerator: F = @floatFromInt(combined);
    const denominator: F = @floatFromInt(shift);
    return numerator / denominator;
}

/// Round a big-endian base-256 integer magnitude to F (nearest, ties even).
fn roundBytesToFloat(comptime F: type, allocator: Allocator, bytes_be: []const u8) Allocator.Error!F {
    const limbs = try limbsFromBytes(allocator, bytes_be);
    defer allocator.free(limbs);
    return roundLimbsToFloat(F, limbs, false, 0);
}

fn floatSlowPath(comptime F: type, allocator: Allocator, exact: Exact) Allocator.Error!F {
    const spec = FloatSpec(F);
    const big = std.math.big.int.Managed;

    // fraction = after / 10^scale in fixed point: Q = floor(after * 2^shift / 10^scale),
    // with a sticky bit tracking the discarded remainder.
    var after_big = try bigFromBytes(allocator, exact.after);
    defer after_big.deinit();
    var after_shifted = try big.init(allocator);
    defer after_shifted.deinit();
    try after_shifted.shiftLeft(&after_big, spec.fixed_point_shift);

    var divisor = try big.init(allocator);
    defer divisor.deinit();
    var ten = try big.initSet(allocator, 10);
    defer ten.deinit();
    try divisor.pow(&ten, exact.scale);

    var quotient = try big.init(allocator);
    defer quotient.deinit();
    var remainder = try big.init(allocator);
    defer remainder.deinit();
    try quotient.divTrunc(&remainder, &after_shifted, &divisor);
    const sticky = !remainder.eqlZero();

    // fixed = before * 2^shift + Q; Q < 2^shift so this is exact concatenation.
    var before_big = try bigFromBytes(allocator, exact.before);
    defer before_big.deinit();
    var fixed = try big.init(allocator);
    defer fixed.deinit();
    try fixed.shiftLeft(&before_big, spec.fixed_point_shift);
    try fixed.add(&fixed, &quotient);

    const limb_count = fixed.len();
    return roundLimbsToFloat(F, fixed.limbs[0..limb_count], sticky, spec.fixed_point_shift);
}

/// Round `(limbs + sticky_tail_fraction) * 2^-fixed_point_shift` to F using
/// round-to-nearest, ties-to-even. `limbs` is a little-endian magnitude;
/// `sticky` records whether any value below the integer's LSB was discarded.
fn roundLimbsToFloat(comptime F: type, limbs: []const std.math.big.Limb, sticky: bool, fixed_point_shift: i32) F {
    const spec = FloatSpec(F);
    const bit_length = limbsBitLength(limbs);
    if (bit_length == 0) return 0.0;

    // Index of the mantissa ULP within the fixed-point integer: keep
    // `precision` bits below the MSB, but never represent an exponent smaller
    // than the subnormal minimum.
    const normal_ulp_index: i64 = @as(i64, @intCast(bit_length)) - spec.precision;
    const subnormal_ulp_index: i64 = @as(i64, spec.exponent_min - (spec.precision - 1)) + fixed_point_shift;
    const ulp_index: i64 = @max(normal_ulp_index, subnormal_ulp_index);

    if (ulp_index <= 0) {
        // Every bit is representable; the value is exact apart from the sticky
        // tail, which sits below the halfway point and never rounds up.
        std.debug.assert(bit_length <= spec.precision);
        const mantissa: spec.Mantissa = @intCast(limbsExtractTop(limbs, bit_length, bit_length));
        return assembleFloat(F, mantissa, -@as(i64, fixed_point_shift));
    }

    const drop: usize = @intCast(ulp_index);
    const ulp_exponent = @as(i64, @intCast(drop)) - fixed_point_shift;
    if (drop >= bit_length) {
        // The whole value sits below the smallest ULP; it can still round up
        // to one ULP when it exceeds the halfway point.
        if (drop == bit_length) {
            // MSB is exactly the halfway bit: round up iff anything below it.
            const below = limbsAnyBitBelow(limbs, bit_length - 1) or sticky;
            return if (below) assembleFloat(F, 1, ulp_exponent) else 0.0;
        }
        return 0.0;
    }

    const kept_bits = bit_length - drop;
    std.debug.assert(kept_bits <= spec.precision);
    var mantissa: spec.Mantissa = @intCast(limbsExtractTop(limbs, bit_length, kept_bits));
    const round_bit = limbsTestBit(limbs, drop - 1);
    const tail_sticky = sticky or limbsAnyBitBelow(limbs, drop - 1);
    if (round_bit and (tail_sticky or (mantissa & 1) == 1)) mantissa += 1;
    return assembleFloat(F, mantissa, ulp_exponent);
}

/// Assemble `mantissa * 2^ulp_exponent` as F. The mantissa always fits F's
/// precision (a rounding carry to 2^precision is still exact — the low bits
/// are zero), so scaling is exact; out-of-range magnitudes become inf.
fn assembleFloat(comptime F: type, mantissa: FloatSpec(F).Mantissa, ulp_exponent: i64) F {
    const spec = FloatSpec(F);
    if (mantissa == 0) return 0.0;
    const mantissa_bits: i64 = @intCast(@bitSizeOf(spec.Mantissa) - @clz(mantissa));
    if (mantissa_bits - 1 + ulp_exponent > spec.exponent_max) {
        return std.math.inf(F);
    }
    const as_float: F = @floatFromInt(mantissa);
    return std.math.ldexp(as_float, @intCast(ulp_exponent));
}

// limb helpers //

fn limbsFromBytes(allocator: Allocator, bytes_be: []const u8) Allocator.Error![]std.math.big.Limb {
    const Limb = std.math.big.Limb;
    const bytes_per_limb = @sizeOf(Limb);
    const limb_count = (bytes_be.len + bytes_per_limb - 1) / bytes_per_limb;
    const limbs = try allocator.alloc(Limb, @max(limb_count, 1));
    @memset(limbs, 0);
    for (bytes_be, 0..) |byte, index| {
        const byte_offset = bytes_be.len - 1 - index;
        const limb_index = byte_offset / bytes_per_limb;
        const shift: std.math.Log2Int(Limb) = @intCast((byte_offset % bytes_per_limb) * 8);
        limbs[limb_index] |= @as(Limb, byte) << shift;
    }
    return limbs;
}

fn bigFromBytes(allocator: Allocator, bytes_be: []const u8) Allocator.Error!std.math.big.int.Managed {
    const limbs = try limbsFromBytes(allocator, bytes_be);
    defer allocator.free(limbs);
    var result = try std.math.big.int.Managed.init(allocator);
    errdefer result.deinit();
    var used = limbs.len;
    while (used > 1 and limbs[used - 1] == 0) used -= 1;
    try result.ensureCapacity(used);
    @memcpy(result.limbs[0..used], limbs[0..used]);
    result.setMetadata(true, used);
    return result;
}

fn limbsBitLength(limbs: []const std.math.big.Limb) usize {
    var index = limbs.len;
    while (index > 0) {
        index -= 1;
        if (limbs[index] != 0) {
            const limb_bits = @bitSizeOf(std.math.big.Limb);
            return index * limb_bits + (limb_bits - @clz(limbs[index]));
        }
    }
    return 0;
}

fn limbsTestBit(limbs: []const std.math.big.Limb, bit: usize) bool {
    const limb_bits = @bitSizeOf(std.math.big.Limb);
    const limb_index = bit / limb_bits;
    if (limb_index >= limbs.len) return false;
    const shift: std.math.Log2Int(std.math.big.Limb) = @intCast(bit % limb_bits);
    return (limbs[limb_index] >> shift) & 1 != 0;
}

/// Whether any bit strictly below `bit` is set.
fn limbsAnyBitBelow(limbs: []const std.math.big.Limb, bit: usize) bool {
    const limb_bits = @bitSizeOf(std.math.big.Limb);
    const full_limbs = bit / limb_bits;
    for (limbs[0..@min(full_limbs, limbs.len)]) |limb| {
        if (limb != 0) return true;
    }
    const partial_bits = bit % limb_bits;
    if (partial_bits == 0 or full_limbs >= limbs.len) return false;
    const mask = (@as(std.math.big.Limb, 1) << @intCast(partial_bits)) - 1;
    return (limbs[full_limbs] & mask) != 0;
}

/// The top `count` bits of a magnitude whose total bit length is `bit_length`.
fn limbsExtractTop(limbs: []const std.math.big.Limb, bit_length: usize, count: usize) u64 {
    std.debug.assert(count <= 64 and count <= bit_length);
    var result: u64 = 0;
    var taken: usize = 0;
    while (taken < count) {
        const bit = bit_length - 1 - taken;
        result = (result << 1) | @intFromBool(limbsTestBit(limbs, bit));
        taken += 1;
    }
    return result;
}

// fit set //

/// Compute the set of builtin numeric types that can represent the literal's
/// exact value. Floats are total (out-of-range values become ±inf), so their
/// bits are always set. Allocates only when the fractional digit magnitude
/// exceeds u128 (adversarial literals).
pub fn computeFitSet(allocator: Allocator, exact: Exact) Allocator.Error!FitSet {
    var set = FitSet.initEmpty();
    set.insert(.f32);
    set.insert(.f64);

    if (!exact.is_fractional) {
        if (magnitudeU128(exact.before)) |magnitude| {
            inline for ([_]Target{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 }) |target| {
                if (intTargetAccepts(target, magnitude, exact.is_negative)) set.insert(target);
            }
        }
    }

    if ((try decBits(allocator, exact)) != null) set.insert(.dec);
    return set;
}

/// Whether one builtin numeric type can represent the literal's exact value.
pub fn fits(allocator: Allocator, exact: Exact, target: Target) Allocator.Error!bool {
    switch (target) {
        .f32, .f64 => return true,
        .dec => return (try decBits(allocator, exact)) != null,
        else => {
            const magnitude = intMagnitude(exact) orelse return false;
            return intTargetAccepts(target, magnitude, exact.is_negative);
        },
    }
}

// tests //

const testing = std.testing;

fn testExact(before: []const u8, after: []const u8, scale: u32, is_negative: bool, is_fractional: bool) Exact {
    return .{ .before = before, .after = after, .scale = scale, .is_negative = is_negative, .is_fractional = is_fractional };
}

/// Test helper: big-endian base-256 digits of a decimal digit string.
fn bytesFromDecimalDigits(allocator: Allocator, digits: []const u8) Allocator.Error![]u8 {
    var bytes_le = std.ArrayList(u8).empty;
    defer bytes_le.deinit(allocator);
    for (digits) |digit_char| {
        std.debug.assert(digit_char >= '0' and digit_char <= '9');
        var carry: u16 = digit_char - '0';
        for (bytes_le.items) |*byte| {
            const next = @as(u16, byte.*) * 10 + carry;
            byte.* = @truncate(next);
            carry = next >> 8;
        }
        while (carry != 0) {
            try bytes_le.append(allocator, @truncate(carry));
            carry >>= 8;
        }
    }
    var length = bytes_le.items.len;
    while (length > 0 and bytes_le.items[length - 1] == 0) length -= 1;
    const out = try allocator.alloc(u8, length);
    for (out, 0..) |*byte, index| byte.* = bytes_le.items[length - 1 - index];
    return out;
}

/// Test helper: parse "123.456" / "-1" style text into an owned Exact.
const TestParsed = struct {
    before: []u8,
    after: []u8,
    exact: Exact,

    fn deinit(self: TestParsed, allocator: Allocator) void {
        allocator.free(self.before);
        allocator.free(self.after);
    }
};

fn testParse(allocator: Allocator, text: []const u8) Allocator.Error!TestParsed {
    var rest = text;
    const is_negative = rest.len > 0 and rest[0] == '-';
    if (is_negative) rest = rest[1..];
    const point = std.mem.findScalar(u8, rest, '.');
    const before_digits = if (point) |p| rest[0..p] else rest;
    const after_digits = if (point) |p| rest[p + 1 ..] else "";
    const before = try bytesFromDecimalDigits(allocator, before_digits);
    errdefer allocator.free(before);
    const after = try bytesFromDecimalDigits(allocator, after_digits);
    errdefer allocator.free(after);
    const scale: u32 = @intCast(after_digits.len);
    return .{
        .before = before,
        .after = after,
        .exact = .{
            .before = before,
            .after = after,
            .scale = scale,
            .is_negative = is_negative,
            .is_fractional = point != null,
        },
    };
}

test "intBits assembles exact integers with sign handling" {
    const gpa = testing.allocator;
    const cases = [_]struct { text: []const u8, target: Target, expected: ?IntBits }{
        .{ .text = "0", .target = .u8, .expected = .{ .i128 = 0 } },
        .{ .text = "-0", .target = .u8, .expected = .{ .i128 = 0 } },
        .{ .text = "255", .target = .u8, .expected = .{ .i128 = 255 } },
        .{ .text = "256", .target = .u8, .expected = null },
        .{ .text = "-1", .target = .u8, .expected = null },
        .{ .text = "-128", .target = .i8, .expected = .{ .i128 = -128 } },
        .{ .text = "-129", .target = .i8, .expected = null },
        .{ .text = "170141183460469231731687303715884105727", .target = .i128, .expected = .{ .i128 = std.math.maxInt(i128) } },
        .{ .text = "170141183460469231731687303715884105728", .target = .i128, .expected = null },
        .{ .text = "-170141183460469231731687303715884105728", .target = .i128, .expected = .{ .i128 = std.math.minInt(i128) } },
        .{ .text = "-170141183460469231731687303715884105729", .target = .i128, .expected = null },
        .{ .text = "340282366920938463463374607431768211455", .target = .u128, .expected = .{ .u128 = std.math.maxInt(u128) } },
        .{ .text = "340282366920938463463374607431768211456", .target = .u128, .expected = null },
    };
    for (cases) |case| {
        const parsed = try testParse(gpa, case.text);
        defer parsed.deinit(gpa);
        const actual = intBits(parsed.exact, case.target);
        try testing.expectEqualDeep(case.expected, actual);
    }
}

test "fractional literals never fit integer targets" {
    const gpa = testing.allocator;
    const parsed = try testParse(gpa, "3.0");
    defer parsed.deinit(gpa);
    try testing.expectEqual(@as(?IntBits, null), intBits(parsed.exact, .u64));
    try testing.expect(!(try fits(gpa, parsed.exact, .u64)));
}

test "decBits computes exact Dec scaling and boundaries" {
    const gpa = testing.allocator;
    const cases = [_]struct { text: []const u8, expected: ?i128 }{
        .{ .text = "0", .expected = 0 },
        .{ .text = "1", .expected = 1_000_000_000_000_000_000 },
        .{ .text = "3.14", .expected = 3_140_000_000_000_000_000 },
        .{ .text = "0.000000000000000001", .expected = 1 },
        .{ .text = "0.0000000000000000001", .expected = null }, // 1e-19: unrepresentable
        .{ .text = "0.00000000000000000010", .expected = null }, // still 1e-19 despite the trailing zero
        .{ .text = "0.000000000000000001000", .expected = 1 }, // trailing zeros normalize away
        .{ .text = "170141183460469231731.687303715884105727", .expected = std.math.maxInt(i128) },
        .{ .text = "170141183460469231731.687303715884105728", .expected = null },
        .{ .text = "-170141183460469231731.687303715884105728", .expected = std.math.minInt(i128) },
        .{ .text = "-170141183460469231731.687303715884105729", .expected = null },
        .{ .text = "170141183460469231732", .expected = null },
    };
    for (cases) |case| {
        const parsed = try testParse(gpa, case.text);
        defer parsed.deinit(gpa);
        const actual = try decBits(gpa, parsed.exact);
        try testing.expectEqual(case.expected, actual);
    }
}

test "decBits accepts out-of-precision digits only when they are zeros" {
    const gpa = testing.allocator;
    // 19 fractional digits ending in zero: value is exactly 0.1... with 18 places.
    const ok = try testParse(gpa, "0.1234567890123456780");
    defer ok.deinit(gpa);
    try testing.expectEqual(@as(?i128, 123_456_789_012_345_678), try decBits(gpa, ok.exact));

    const bad = try testParse(gpa, "0.1234567890123456789");
    defer bad.deinit(gpa);
    try testing.expectEqual(@as(?i128, null), try decBits(gpa, bad.exact));

    // A fractional magnitude beyond u128 (60 digits), all-zero tail beyond 18.
    const huge_ok = try testParse(gpa, "0.123456789012345678000000000000000000000000000000000000000000");
    defer huge_ok.deinit(gpa);
    try testing.expectEqual(@as(?i128, 123_456_789_012_345_678), try decBits(gpa, huge_ok.exact));

    const huge_bad = try testParse(gpa, "0.123456789012345678000000000000000000000000000000000000000001");
    defer huge_bad.deinit(gpa);
    try testing.expectEqual(@as(?i128, null), try decBits(gpa, huge_bad.exact));
}

test "trailingZeroBits counts across big-endian byte boundaries" {
    // 0x0100 = 256: 8 trailing zero bits.
    try testing.expectEqual(@as(u32, 8), trailingZeroBits(&.{ 0x01, 0x00 }));
    // 0x30 = 48 = 16*3: 4 trailing zero bits.
    try testing.expectEqual(@as(u32, 4), trailingZeroBits(&.{0x30}));
    // Odd magnitude: none — so no 10^excess (excess >= 1) can divide it,
    // which is the fast-reject path for adversarial huge-scale fractions.
    try testing.expectEqual(@as(u32, 0), trailingZeroBits(&.{ 0x12, 0x01 }));
    // Zero magnitude: every power of two divides it.
    try testing.expectEqual(@as(u32, std.math.maxInt(u32)), trailingZeroBits(&.{ 0x00, 0x00 }));
}

test "decBits is null beyond Dec range in either direction" {
    const gpa = testing.allocator;
    const positive = try testParse(gpa, "999999999999999999999999999");
    defer positive.deinit(gpa);
    try testing.expectEqual(@as(?i128, null), try decBits(gpa, positive.exact));
    const negative = try testParse(gpa, "-999999999999999999999999999");
    defer negative.deinit(gpa);
    try testing.expectEqual(@as(?i128, null), try decBits(gpa, negative.exact));
}

fn expectFloatBitsMatchParseFloat(comptime F: type, gpa: Allocator, text: []const u8) (Allocator.Error || std.fmt.ParseFloatError || error{TestExpectedEqual})!void {
    const parsed = try testParse(gpa, text);
    defer parsed.deinit(gpa);
    const actual = try floatBits(F, gpa, parsed.exact);
    const expected = try std.fmt.parseFloat(F, text);
    try testing.expectEqual(@as(std.meta.Int(.unsigned, @bitSizeOf(F)), @bitCast(expected)), @as(std.meta.Int(.unsigned, @bitSizeOf(F)), @bitCast(actual)));
}

test "floatBits matches correctly-rounded parseFloat on curated cases" {
    const gpa = testing.allocator;
    const cases = [_][]const u8{
        "0",
        "-0.0",
        "1",
        "0.5",
        "3.14",
        "0.1",
        "16777217", // f32 tie: rounds to 16777216
        "16777219", // f32: rounds to 16777220
        "9007199254740993", // f64 tie: rounds to 9007199254740992
        "9007199254740995", // f64 odd tie: rounds to 9007199254740996
        "340282366920938463463374607431768211455", // u128 max
        "340282356779733661637539395458142568447", // f32 max boundary region
        "340282356779733661637539395458142568448", // first value rounding to f32 inf
        "0.000000000000000000000000000000000000000000001", // f32 subnormal
        "0.00000000000000000000000000000000000000000000140129846432481707092992861", // near f32 min subnormal
        "179769313486231580793728971405303415261810836411386668340988175735522011480040587012044836526658916000387525464711483839628816311250067936502217901232750671071284884197166651007039023537463697921", // near f64 max
        "1.00000000000000011102230246251565404236316680908203125", // f64 exact halfway + representable
    };
    inline for (.{ f32, f64 }) |F| {
        for (cases) |text| {
            try expectFloatBitsMatchParseFloat(F, gpa, text);
        }
    }
}

test "floatBits handles deep subnormal magnitudes" {
    const gpa = testing.allocator;
    // 0.<323 zeros>49406564584124654 — near f64 min subnormal (~4.94e-324).
    var text = std.ArrayList(u8).empty;
    defer text.deinit(gpa);
    try text.appendSlice(gpa, "0.");
    try text.appendNTimes(gpa, '0', 323);
    try text.appendSlice(gpa, "49406564584124654");
    try expectFloatBitsMatchParseFloat(f64, gpa, text.items);
    try expectFloatBitsMatchParseFloat(f32, gpa, text.items);

    // Exactly half of the smallest f64 subnormal rounds to zero (ties-to-even),
    // and the next representable decimal above it rounds up to that subnormal.
    // 2^-1075 is exactly 0.<323 zeros>247032822920623272088284396434110686182529901307162382212792841250337753635104375932649918180817996189898282347722858865463328355177969898199387398005390939063151173812746553391064834978629158339524656e-1075... rendered fully below.
    text.clearRetainingCapacity();
    try text.appendSlice(gpa, "0.");
    try text.appendNTimes(gpa, '0', 323);
    // First digits of 2^-1075 after those zeros; a truncated prefix is strictly
    // below the halfway point, so it must round to zero...
    try text.appendSlice(gpa, "2470328229206232720");
    try expectFloatBitsMatchParseFloat(f64, gpa, text.items);
    // ...and bumping the last digit up crosses the halfway point, so it must
    // round to the smallest subnormal.
    text.items[text.items.len - 1] = '9';
    try expectFloatBitsMatchParseFloat(f64, gpa, text.items);
}

test "floatBits random property vs parseFloat oracle" {
    const gpa = testing.allocator;
    var prng = std.Random.DefaultPrng.init(0x9e3779b97f4a7c15);
    const random = prng.random();

    var text = std.ArrayList(u8).empty;
    defer text.deinit(gpa);
    var round: usize = 0;
    while (round < 2000) : (round += 1) {
        const before_value = random.int(u64) >> @intCast(random.uintLessThan(u6, 60));
        const scale = random.uintLessThan(u32, 40);
        text.clearRetainingCapacity();
        try text.print(gpa, "{d}", .{before_value});
        if (scale > 0) {
            try text.append(gpa, '.');
            var digit: u32 = 0;
            while (digit < scale) : (digit += 1) {
                try text.append(gpa, '0' + random.uintLessThan(u8, 10));
            }
        }
        try expectFloatBitsMatchParseFloat(f64, gpa, text.items);
        try expectFloatBitsMatchParseFloat(f32, gpa, text.items);
    }
}

test "fit set summarizes representability across targets" {
    const gpa = testing.allocator;
    const parsed = try testParse(gpa, "300");
    defer parsed.deinit(gpa);
    const set = try computeFitSet(gpa, parsed.exact);
    try testing.expect(!set.contains(.u8));
    try testing.expect(!set.contains(.i8));
    try testing.expect(set.contains(.u16));
    try testing.expect(set.contains(.i16));
    try testing.expect(set.contains(.dec));
    try testing.expect(set.contains(.f32));
    try testing.expect(set.contains(.f64));

    const negative = try testParse(gpa, "-300");
    defer negative.deinit(gpa);
    const negative_set = try computeFitSet(gpa, negative.exact);
    try testing.expect(!negative_set.contains(.u16));
    try testing.expect(negative_set.contains(.i16));
}

test "huge digit counts stay fast and unrepresentable in fixed-width targets" {
    const gpa = testing.allocator;
    // 1E80000: 80001 decimal digits, integer-valued.
    var digits = try gpa.alloc(u8, 80001);
    defer gpa.free(digits);
    @memset(digits, '0');
    digits[0] = '1';
    const before = try bytesFromDecimalDigits(gpa, digits);
    defer gpa.free(before);
    const exact = testExact(before, &.{}, 0, false, false);

    // Regression guard for the quadratic text-reconstruction bug (issue 9567):
    // fit + bits over an 80k-digit literal must be cheap. The int/Dec answers
    // come from the >16-byte prefilter without touching the digits, and the
    // float conversion reads the digits once. (Wall-clock bounds live in the
    // CLI perf tests; a hang here would trip the suite timeout regardless.)
    const set = try computeFitSet(gpa, exact);
    try testing.expect(!set.contains(.u128));
    try testing.expect(!set.contains(.i128));
    try testing.expect(!set.contains(.dec));
    try testing.expect(set.contains(.f64));
    const f64_bits = try floatBits(f64, gpa, exact);
    try testing.expect(std.math.isPositiveInf(f64_bits));
    const f32_bits = try floatBits(f32, gpa, exact);
    try testing.expect(std.math.isPositiveInf(f32_bits));
    try testing.expectEqual(@as(?i128, null), try decBits(gpa, exact));
}
