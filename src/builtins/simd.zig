//! Bit-exact, target-independent meaning of Roc's 128-bit integer SIMD ops.
//!
//! Optimizing backends lower these operations to native vector instructions.
//! The evaluators and dev-backend helpers use this implementation directly so
//! every execution path has the same edge-case behavior.

const std = @import("std");

/// Runtime-side SIMD vocabulary. `base.LowLevel.simdOpIndex` is the explicit
/// compiler-to-runtime mapping and pins this declaration order.
pub const Op = enum(u8) {
    load_16_unchecked,
    store_16_unchecked,
    append_16,
    splat,
    get_lane_unchecked,
    with_lane_unchecked,
    to_u128_bits,
    from_u128_bits,
    add_wrap,
    sub_wrap,
    add_sat,
    sub_sat,
    neg_wrap,
    abs_wrap,
    min,
    max,
    abs_diff,
    avg_rounded,
    mul_wrap,
    mul_high,
    mul_q15_sat,
    mul_wide_lo,
    mul_wide_hi,
    dot_pairs,
    dot_pairs_sat,
    sad,
    @"and",
    @"or",
    xor,
    not,
    bit_select,
    eq_lanes,
    gt_lanes,
    gte_lanes,
    bitmask,
    shl_wrap,
    shr_wrap,
    shr_zf_wrap,
    shr_rounded,
    interleave_lo,
    interleave_hi,
    even_lanes,
    odd_lanes,
    reverse_lanes,
    table_lookup,
    concat_shift_bytes,
    widen_lo,
    widen_hi,
    pairwise_add_widen,
    narrow_wrap,
    narrow_sat,
    sum_lanes,
    sum_lanes_wrap,
    clmul_lo,
    clmul_hi,
};

/// One of Roc's eight signed or unsigned 128-bit integer vector shapes.
pub const Kind = enum(u3) {
    u8x16,
    i8x16,
    u16x8,
    i16x8,
    u32x4,
    i32x4,
    u64x2,
    i64x2,

    pub fn laneBits(self: Kind) u7 {
        return switch (self) {
            .u8x16, .i8x16 => 8,
            .u16x8, .i16x8 => 16,
            .u32x4, .i32x4 => 32,
            .u64x2, .i64x2 => 64,
        };
    }

    pub fn laneCount(self: Kind) u5 {
        return @intCast(@as(u16, 128) / self.laneBits());
    }

    pub fn isSigned(self: Kind) bool {
        return switch (self) {
            .i8x16, .i16x8, .i32x4, .i64x2 => true,
            .u8x16, .u16x8, .u32x4, .u64x2 => false,
        };
    }
};

fn laneMask(width: u7) u128 {
    return if (width == 128) ~@as(u128, 0) else (@as(u128, 1) << width) - 1;
}

/// Extract one lane's unsigned bit pattern from a packed 128-bit vector.
pub fn getLane(bits: u128, kind: Kind, index: u8) u64 {
    const width = kind.laneBits();
    return @truncate((bits >> @as(u7, @intCast(@as(u16, index) * width))) & laneMask(width));
}

/// Replace one lane in a packed 128-bit vector, truncating to its lane width.
pub fn withLane(bits: u128, kind: Kind, index: u8, value: u64) u128 {
    const width = kind.laneBits();
    const bit_offset: u7 = @intCast(@as(u16, index) * width);
    const positioned_mask = laneMask(width) << bit_offset;
    return (bits & ~positioned_mask) | ((@as(u128, value) & laneMask(width)) << bit_offset);
}

fn signedLane(raw: u64, width: u7) i128 {
    const sign = @as(u64, 1) << @intCast(width - 1);
    const value: i128 = @intCast(raw);
    return if ((raw & sign) == 0) value else value - (@as(i128, 1) << width);
}

fn signedBits(value: i128, width: u7) u64 {
    return @truncate(@as(u128, @bitCast(value)) & laneMask(width));
}

fn signedMin(width: u7) i128 {
    return -(@as(i128, 1) << @intCast(width - 1));
}

fn signedMax(width: u7) i128 {
    return (@as(i128, 1) << @intCast(width - 1)) - 1;
}

fn clampSigned(value: i128, width: u7) u64 {
    return signedBits(std.math.clamp(value, signedMin(width), signedMax(width)), width);
}

fn clampUnsigned(value: u128, width: u7) u64 {
    return @truncate(@min(value, laneMask(width)));
}

fn mapUnary(kind: Kind, a: u128, comptime f: fn (Kind, u64) u64) u128 {
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| out = withLane(out, kind, @intCast(i), f(kind, getLane(a, kind, @intCast(i))));
    return out;
}

fn mapBinary(kind: Kind, a: u128, b: u128, comptime f: fn (Kind, u64, u64) u64) u128 {
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| out = withLane(out, kind, @intCast(i), f(kind, getLane(a, kind, @intCast(i)), getLane(b, kind, @intCast(i))));
    return out;
}

fn addWrap(kind: Kind, a: u64, b: u64) u64 {
    return @truncate((@as(u128, a) + b) & laneMask(kind.laneBits()));
}

fn subWrap(kind: Kind, a: u64, b: u64) u64 {
    return @truncate((@as(u128, a) -% b) & laneMask(kind.laneBits()));
}

fn mulWrap(kind: Kind, a: u64, b: u64) u64 {
    return @truncate((@as(u128, a) * b) & laneMask(kind.laneBits()));
}

fn addSat(kind: Kind, a: u64, b: u64) u64 {
    if (kind.isSigned()) return clampSigned(signedLane(a, kind.laneBits()) + signedLane(b, kind.laneBits()), kind.laneBits());
    return clampUnsigned(@as(u128, a) + b, kind.laneBits());
}

fn subSat(kind: Kind, a: u64, b: u64) u64 {
    if (kind.isSigned()) return clampSigned(signedLane(a, kind.laneBits()) - signedLane(b, kind.laneBits()), kind.laneBits());
    return if (a < b) 0 else a - b;
}

fn negWrap(kind: Kind, a: u64) u64 {
    return @truncate((@as(u128, 0) -% a) & laneMask(kind.laneBits()));
}

fn absWrap(kind: Kind, a: u64) u64 {
    const value = signedLane(a, kind.laneBits());
    return if (value < 0) negWrap(kind, a) else a;
}

fn minLane(kind: Kind, a: u64, b: u64) u64 {
    if (kind.isSigned()) return if (signedLane(a, kind.laneBits()) < signedLane(b, kind.laneBits())) a else b;
    return @min(a, b);
}

fn maxLane(kind: Kind, a: u64, b: u64) u64 {
    if (kind.isSigned()) return if (signedLane(a, kind.laneBits()) > signedLane(b, kind.laneBits())) a else b;
    return @max(a, b);
}

fn absDiff(_: Kind, a: u64, b: u64) u64 {
    return if (a >= b) a - b else b - a;
}

fn avgRounded(_: Kind, a: u64, b: u64) u64 {
    return @intCast((@as(u128, a) + b + 1) >> 1);
}

fn eqLane(kind: Kind, a: u64, b: u64) u64 {
    return if (a == b) @truncate(laneMask(kind.laneBits())) else 0;
}

fn gtLane(kind: Kind, a: u64, b: u64) u64 {
    const yes = if (kind.isSigned()) signedLane(a, kind.laneBits()) > signedLane(b, kind.laneBits()) else a > b;
    return if (yes) @truncate(laneMask(kind.laneBits())) else 0;
}

fn gteLane(kind: Kind, a: u64, b: u64) u64 {
    const yes = if (kind.isSigned()) signedLane(a, kind.laneBits()) >= signedLane(b, kind.laneBits()) else a >= b;
    return if (yes) @truncate(laneMask(kind.laneBits())) else 0;
}

fn splat(kind: Kind, value: u64) u128 {
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| out = withLane(out, kind, @intCast(i), value);
    return out;
}

fn shift(kind: Kind, bits: u128, count_raw: u64, direction: enum { left, right, right_zero }) u128 {
    const width = kind.laneBits();
    const count: u7 = @intCast(count_raw % width);
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| {
        const raw = getLane(bits, kind, @intCast(i));
        const shifted: u64 = switch (direction) {
            .left => @truncate((@as(u128, raw) << count) & laneMask(width)),
            .right_zero => raw >> @intCast(count),
            .right => if (kind.isSigned()) signedBits(signedLane(raw, width) >> @intCast(count), width) else raw >> @intCast(count),
        };
        out = withLane(out, kind, @intCast(i), shifted);
    }
    return out;
}

fn roundedShift(kind: Kind, bits: u128, count_raw: u64) u128 {
    const width = kind.laneBits();
    if (count_raw == 0) return bits;
    if (count_raw >= width) return 0;
    const count: u7 = @intCast(count_raw);
    const bias = @as(i128, 1) << @intCast(count - 1);
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| {
        const value = (signedLane(getLane(bits, kind, @intCast(i)), width) + bias) >> @intCast(count);
        out = withLane(out, kind, @intCast(i), signedBits(value, width));
    }
    return out;
}

fn interleave(kind: Kind, a: u128, b: u128, high: bool) u128 {
    const half = kind.laneCount() / 2;
    const start = if (high) half else 0;
    var out: u128 = 0;
    for (0..half) |i| {
        out = withLane(out, kind, @intCast(2 * i), getLane(a, kind, @intCast(start + i)));
        out = withLane(out, kind, @intCast(2 * i + 1), getLane(b, kind, @intCast(start + i)));
    }
    return out;
}

fn parityLanes(kind: Kind, a: u128, b: u128, odd: bool) u128 {
    var out: u128 = 0;
    const half = kind.laneCount() / 2;
    const parity: usize = @intFromBool(odd);
    for (0..half) |i| {
        out = withLane(out, kind, @intCast(i), getLane(a, kind, @intCast(2 * i + parity)));
        out = withLane(out, kind, @intCast(half + i), getLane(b, kind, @intCast(2 * i + parity)));
    }
    return out;
}

fn reverse(kind: Kind, bits: u128) u128 {
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| out = withLane(out, kind, @intCast(i), getLane(bits, kind, @intCast(kind.laneCount() - 1 - i)));
    return out;
}

fn widen(src_kind: Kind, dst_kind: Kind, bits: u128, high: bool) u128 {
    const count = dst_kind.laneCount();
    const start = if (high) count else 0;
    var out: u128 = 0;
    for (0..count) |i| {
        const raw = getLane(bits, src_kind, @intCast(start + i));
        const extended = if (src_kind.isSigned()) signedBits(signedLane(raw, src_kind.laneBits()), dst_kind.laneBits()) else raw;
        out = withLane(out, dst_kind, @intCast(i), extended);
    }
    return out;
}

fn mulWide(src_kind: Kind, dst_kind: Kind, a: u128, b: u128, high: bool) u128 {
    const count = dst_kind.laneCount();
    const start = if (high) count else 0;
    var out: u128 = 0;
    for (0..count) |i| {
        const av = getLane(a, src_kind, @intCast(start + i));
        const bv = getLane(b, src_kind, @intCast(start + i));
        const product = if (src_kind.isSigned())
            signedBits(signedLane(av, src_kind.laneBits()) * signedLane(bv, src_kind.laneBits()), dst_kind.laneBits())
        else
            @as(u64, @truncate(@as(u128, av) * bv));
        out = withLane(out, dst_kind, @intCast(i), product);
    }
    return out;
}

fn pairwiseAddWiden(src_kind: Kind, dst_kind: Kind, bits: u128) u128 {
    var out: u128 = 0;
    for (0..dst_kind.laneCount()) |i| {
        const a = getLane(bits, src_kind, @intCast(2 * i));
        const b = getLane(bits, src_kind, @intCast(2 * i + 1));
        const sum = if (src_kind.isSigned())
            signedBits(signedLane(a, src_kind.laneBits()) + signedLane(b, src_kind.laneBits()), dst_kind.laneBits())
        else
            a + b;
        out = withLane(out, dst_kind, @intCast(i), sum);
    }
    return out;
}

fn narrow(src_kind: Kind, dst_kind: Kind, a: u128, b: u128, saturated: bool) u128 {
    var out: u128 = 0;
    const half = src_kind.laneCount();
    for (0..dst_kind.laneCount()) |i| {
        const source = if (i < half) a else b;
        const raw = getLane(source, src_kind, @intCast(i % half));
        const narrowed = if (!saturated)
            raw
        else if (dst_kind.isSigned())
            clampSigned(signedLane(raw, src_kind.laneBits()), dst_kind.laneBits())
        else if (src_kind.isSigned())
            if (signedLane(raw, src_kind.laneBits()) <= 0) 0 else clampUnsigned(@intCast(signedLane(raw, src_kind.laneBits())), dst_kind.laneBits())
        else
            clampUnsigned(raw, dst_kind.laneBits());
        out = withLane(out, dst_kind, @intCast(i), narrowed);
    }
    return out;
}

fn bitmask(kind: Kind, bits: u128) u64 {
    var out: u64 = 0;
    for (0..kind.laneCount()) |i| {
        const lane = getLane(bits, kind, @intCast(i));
        out |= ((lane >> @intCast(kind.laneBits() - 1)) & 1) << @intCast(i);
    }
    return out;
}

fn mulHigh(kind: Kind, a: u128, b: u128) u128 {
    const width = kind.laneBits();
    var out: u128 = 0;
    for (0..kind.laneCount()) |i| {
        const av = getLane(a, kind, @intCast(i));
        const bv = getLane(b, kind, @intCast(i));
        const high = if (kind.isSigned())
            signedBits((signedLane(av, width) * signedLane(bv, width)) >> @intCast(width), width)
        else
            @as(u64, @truncate((@as(u128, av) * bv) >> width));
        out = withLane(out, kind, @intCast(i), high);
    }
    return out;
}

fn q15(a: u128, b: u128) u128 {
    const kind = Kind.i16x8;
    var out: u128 = 0;
    for (0..8) |i| {
        const av = signedLane(getLane(a, kind, @intCast(i)), 16);
        const bv = signedLane(getLane(b, kind, @intCast(i)), 16);
        out = withLane(out, kind, @intCast(i), clampSigned((2 * av * bv + 32768) >> 16, 16));
    }
    return out;
}

fn dotPairs(a: u128, b: u128) u128 {
    var out: u128 = 0;
    for (0..4) |i| {
        const a0 = signedLane(getLane(a, .i16x8, @intCast(2 * i)), 16);
        const a1 = signedLane(getLane(a, .i16x8, @intCast(2 * i + 1)), 16);
        const b0 = signedLane(getLane(b, .i16x8, @intCast(2 * i)), 16);
        const b1 = signedLane(getLane(b, .i16x8, @intCast(2 * i + 1)), 16);
        out = withLane(out, .i32x4, @intCast(i), signedBits(a0 * b0 + a1 * b1, 32));
    }
    return out;
}

fn dotPairsSat(a: u128, b: u128) u128 {
    var out: u128 = 0;
    for (0..8) |i| {
        const a0 = getLane(a, .u8x16, @intCast(2 * i));
        const a1 = getLane(a, .u8x16, @intCast(2 * i + 1));
        const b0 = signedLane(getLane(b, .i8x16, @intCast(2 * i)), 8);
        const b1 = signedLane(getLane(b, .i8x16, @intCast(2 * i + 1)), 8);
        out = withLane(out, .i16x8, @intCast(i), clampSigned(@as(i128, a0) * b0 + @as(i128, a1) * b1, 16));
    }
    return out;
}

fn sad(a: u128, b: u128) u128 {
    var out: u128 = 0;
    for (0..2) |half| {
        var sum: u64 = 0;
        for (0..8) |i| sum += absDiff(.u8x16, getLane(a, .u8x16, @intCast(half * 8 + i)), getLane(b, .u8x16, @intCast(half * 8 + i)));
        out = withLane(out, .u64x2, @intCast(half), sum);
    }
    return out;
}

fn sumLanes(kind: Kind, bits: u128) u128 {
    if (kind.isSigned()) {
        var sum: i128 = 0;
        for (0..kind.laneCount()) |i| sum += signedLane(getLane(bits, kind, @intCast(i)), kind.laneBits());
        return @bitCast(sum);
    }
    var sum: u128 = 0;
    for (0..kind.laneCount()) |i| sum += getLane(bits, kind, @intCast(i));
    return sum;
}

fn carryless(a: u64, b: u64) u128 {
    var out: u128 = 0;
    for (0..64) |i| if (((b >> @intCast(i)) & 1) != 0) {
        out ^= @as(u128, a) << @intCast(i);
    };
    return out;
}

/// Evaluate a non-memory SIMD low-level operation. Scalar arguments occupy the
/// low bits of `a`, `b`, or `c`, exactly like their Roc scalar representation.
pub fn eval(op: Op, arg_kind: Kind, ret_kind: Kind, a: u128, b: u128, c: u128) u128 {
    return switch (op) {
        .splat => splat(ret_kind, @truncate(a)),
        .get_lane_unchecked => getLane(a, arg_kind, @truncate(b)),
        .with_lane_unchecked => withLane(a, arg_kind, @truncate(b), @truncate(c)),
        .to_u128_bits, .from_u128_bits => a,
        .add_wrap => mapBinary(arg_kind, a, b, addWrap),
        .sub_wrap => mapBinary(arg_kind, a, b, subWrap),
        .add_sat => mapBinary(arg_kind, a, b, addSat),
        .sub_sat => mapBinary(arg_kind, a, b, subSat),
        .neg_wrap => mapUnary(arg_kind, a, negWrap),
        .abs_wrap => mapUnary(arg_kind, a, absWrap),
        .min => mapBinary(arg_kind, a, b, minLane),
        .max => mapBinary(arg_kind, a, b, maxLane),
        .abs_diff => mapBinary(arg_kind, a, b, absDiff),
        .avg_rounded => mapBinary(arg_kind, a, b, avgRounded),
        .mul_wrap => mapBinary(arg_kind, a, b, mulWrap),
        .mul_high => mulHigh(arg_kind, a, b),
        .mul_q15_sat => q15(a, b),
        .mul_wide_lo => mulWide(arg_kind, ret_kind, a, b, false),
        .mul_wide_hi => mulWide(arg_kind, ret_kind, a, b, true),
        .dot_pairs => dotPairs(a, b),
        .dot_pairs_sat => dotPairsSat(a, b),
        .sad => sad(a, b),
        .@"and" => a & b,
        .@"or" => a | b,
        .xor => a ^ b,
        .not => ~a,
        .bit_select => (a & b) | (~a & c),
        .eq_lanes => mapBinary(arg_kind, a, b, eqLane),
        .gt_lanes => mapBinary(arg_kind, a, b, gtLane),
        .gte_lanes => mapBinary(arg_kind, a, b, gteLane),
        .bitmask => bitmask(arg_kind, a),
        .shl_wrap => shift(arg_kind, a, @truncate(b), .left),
        .shr_wrap => shift(arg_kind, a, @truncate(b), .right),
        .shr_zf_wrap => shift(arg_kind, a, @truncate(b), .right_zero),
        .shr_rounded => roundedShift(arg_kind, a, @truncate(b)),
        .interleave_lo => interleave(arg_kind, a, b, false),
        .interleave_hi => interleave(arg_kind, a, b, true),
        .even_lanes => parityLanes(arg_kind, a, b, false),
        .odd_lanes => parityLanes(arg_kind, a, b, true),
        .reverse_lanes => reverse(arg_kind, a),
        .table_lookup => blk: {
            var out: u128 = 0;
            for (0..16) |i| {
                const index = getLane(b, .u8x16, @intCast(i));
                out = withLane(out, .u8x16, @intCast(i), if (index < 16) getLane(a, .u8x16, @intCast(index)) else 0);
            }
            break :blk out;
        },
        .concat_shift_bytes => blk: {
            const count: u8 = @truncate(c);
            if (count == 0) break :blk a;
            if (count == 16) break :blk b;
            const shift_amount: u7 = @intCast(count * 8);
            break :blk (a >> shift_amount) | (b << @intCast(128 - @as(u16, count) * 8));
        },
        .widen_lo => widen(arg_kind, ret_kind, a, false),
        .widen_hi => widen(arg_kind, ret_kind, a, true),
        .pairwise_add_widen => pairwiseAddWiden(arg_kind, ret_kind, a),
        .narrow_wrap => narrow(arg_kind, ret_kind, a, b, false),
        .narrow_sat => narrow(arg_kind, ret_kind, a, b, true),
        .sum_lanes, .sum_lanes_wrap => sumLanes(arg_kind, a),
        .clmul_lo => carryless(getLane(a, .u64x2, 0), getLane(b, .u64x2, 0)),
        .clmul_hi => carryless(getLane(a, .u64x2, 1), getLane(b, .u64x2, 1)),
        else => unreachable,
    };
}

test "SIMD pinned edge cases" {
    try std.testing.expectEqual(@as(u64, 32767), getLane(eval(.mul_q15_sat, .i16x8, .i16x8, splat(.i16x8, 0x8000), splat(.i16x8, 0x8000), 0), .i16x8, 0));
    try std.testing.expectEqual(@as(u128, 0), eval(.table_lookup, .u8x16, .u8x16, 0xffffffffffffffffffffffffffffffff, splat(.u8x16, 16), 0));
    try std.testing.expectEqual(@as(u64, 1), getLane(eval(.shl_wrap, .u8x16, .u8x16, splat(.u8x16, 1), 8, 0), .u8x16, 0));
    try std.testing.expectEqual(@as(u64, 24), getLane(eval(.sad, .u8x16, .u64x2, splat(.u8x16, 9), splat(.u8x16, 6), 0), .u64x2, 0));
    try std.testing.expectEqual(@as(u128, 15), eval(.clmul_lo, .u64x2, .u64x2, splat(.u64x2, 3), splat(.u64x2, 5), 0));
    const bytes = splat(.u8x16, 56);
    const words = eval(.widen_lo, .u8x16, .u16x8, bytes, 0, 0);
    const narrowed = eval(.narrow_sat, .u16x8, .u8x16, words, words, 0);
    try std.testing.expectEqual(@as(u128, 0xffffffffffffffffffffffffffffffff), eval(.eq_lanes, .u8x16, .u8x16, narrowed, bytes, 0));
    try std.testing.expectEqual(@as(u128, 65535), eval(.bitmask, .u8x16, .u8x16, eval(.eq_lanes, .u8x16, .u8x16, narrowed, bytes, 0), 0, 0));
}
