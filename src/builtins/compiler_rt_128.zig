//! 128-bit integer arithmetic helpers.
//!
//! Provides public Zig functions for 128-bit operations that would otherwise
//! trigger implicit compiler-rt calls. All operations decompose to native
//! 64-bit arithmetic only.
//
// Adapted from Zig's lib/compiler_rt/ sources, which are MIT licensed:
// Copyright (c) Zig contributors
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const Log2Int = math.Log2Int;
const native_endian = builtin.cpu.arch.endian();

// HalveInt: split a 128-bit int into two 64-bit halves

fn HalveInt(comptime T: type, comptime signed_half: bool) type {
    return extern union {
        pub const bits = @divExact(@typeInfo(T).int.bits, 2);
        pub const HalfTU = std.meta.Int(.unsigned, bits);
        pub const HalfTS = std.meta.Int(.signed, bits);
        pub const HalfT = if (signed_half) HalfTS else HalfTU;

        all: T,
        s: if (native_endian == .little)
            extern struct { low: HalfT, high: HalfT }
        else
            extern struct { high: HalfT, low: HalfT },
    };
}

// Division helpers

const lo = switch (native_endian) {
    .big => 1,
    .little => 0,
};
const hi = 1 - lo;

fn divwide_generic(comptime T: type, _u1: T, _u0: T, v_: T, r: *T) T {
    const HalfT = HalveInt(T, false).HalfT;
    var v = v_;

    const b = @as(T, 1) << (@bitSizeOf(T) / 2);
    var un64: T = undefined;
    var un10: T = undefined;

    const s: Log2Int(T) = @intCast(@clz(v));
    if (s > 0) {
        v <<= s;
        un64 = (_u1 << s) | (_u0 >> @intCast((@bitSizeOf(T) - @as(T, @intCast(s)))));
        un10 = _u0 << s;
    } else {
        un64 = _u1;
        un10 = _u0;
    }

    const vn1 = v >> (@bitSizeOf(T) / 2);
    const vn0 = v & std.math.maxInt(HalfT);

    const un1 = un10 >> (@bitSizeOf(T) / 2);
    const un0 = un10 & std.math.maxInt(HalfT);

    var q1 = un64 / vn1;
    var rhat = un64 -% q1 *% vn1;

    while (q1 >= b or q1 * vn0 > b * rhat + un1) {
        q1 -= 1;
        rhat += vn1;
        if (rhat >= b) break;
    }

    const un21 = un64 *% b +% un1 -% q1 *% v;

    var q0 = un21 / vn1;
    rhat = un21 -% q0 *% vn1;

    while (q0 >= b or q0 * vn0 > b * rhat + un0) {
        q0 -= 1;
        rhat += vn1;
        if (rhat >= b) break;
    }

    r.* = (un21 *% b +% un0 -% q0 *% v) >> s;
    return q1 *% b +% q0;
}

fn divwide(comptime T: type, _u1: T, _u0: T, v: T, r: *T) T {
    if (@inComptime()) {
        return divwide_generic(T, _u1, _u0, v, r);
    } else if (T == u64 and builtin.target.cpu.arch == .x86_64 and builtin.target.os.tag != .windows) {
        var rem: T = undefined;
        const quo = asm (
            \\divq %[v]
            : [_] "={rax}" (-> T),
              [_] "={rdx}" (rem),
            : [v] "r" (v),
              [_] "{rax}" (_u0),
              [_] "{rdx}" (_u1),
        );
        r.* = rem;
        return quo;
    } else {
        return divwide_generic(T, _u1, _u0, v, r);
    }
}

fn udivmod(comptime T: type, a_: T, b_: T, maybe_rem: ?*T) T {
    const HalfT = HalveInt(T, false).HalfT;
    const SignedT = std.meta.Int(.signed, @bitSizeOf(T));

    if (b_ > a_) {
        if (maybe_rem) |rem| {
            rem.* = a_;
        }
        return 0;
    }

    const a: [2]HalfT = @bitCast(a_);
    const b: [2]HalfT = @bitCast(b_);
    var q: [2]HalfT = undefined;
    var r: [2]HalfT = undefined;

    if (b[hi] == 0) {
        r[hi] = 0;
        if (a[hi] < b[lo]) {
            q[hi] = 0;
            q[lo] = divwide(HalfT, a[hi], a[lo], b[lo], &r[lo]);
        } else {
            q[hi] = a[hi] / b[lo];
            q[lo] = divwide(HalfT, a[hi] % b[lo], a[lo], b[lo], &r[lo]);
        }
        if (maybe_rem) |rem| {
            rem.* = @bitCast(r);
        }
        return @bitCast(q);
    }

    const shift: Log2Int(T) = @clz(b[hi]) - @clz(a[hi]);
    var af: T = @bitCast(a);
    var bf = @as(T, @bitCast(b)) << shift;
    q = @bitCast(@as(T, 0));

    for (0..shift + 1) |_| {
        q[lo] <<= 1;
        const s = @as(SignedT, @bitCast(bf -% af -% 1)) >> (@bitSizeOf(T) - 1);
        q[lo] |= @intCast(s & 1);
        af -= bf & @as(T, @bitCast(s));
        bf >>= 1;
    }
    if (maybe_rem) |rem| {
        rem.* = @bitCast(af);
    }
    return @bitCast(q);
}

// Multiplication helpers

fn DoubleInt(comptime T: type) type {
    return switch (T) {
        u32 => i64,
        u64 => i128,
        i32 => i64,
        i64 => i128,
        else => unreachable,
    };
}

fn muldXi(comptime T: type, a: T, b: T) DoubleInt(T) {
    const DT = DoubleInt(T);
    const word_t = HalveInt(DT, false);
    const bits_in_word_2 = @sizeOf(T) * 8 / 2;
    const lower_mask = (~@as(T, 0)) >> bits_in_word_2;

    var r: word_t = undefined;
    r.s.low = (a & lower_mask) *% (b & lower_mask);
    var t: T = r.s.low >> bits_in_word_2;
    r.s.low &= lower_mask;
    t += (a >> bits_in_word_2) *% (b & lower_mask);
    r.s.low +%= (t & lower_mask) << bits_in_word_2;
    r.s.high = t >> bits_in_word_2;
    t = r.s.low >> bits_in_word_2;
    r.s.low &= lower_mask;
    t +%= (b >> bits_in_word_2) *% (a & lower_mask);
    r.s.low +%= (t & lower_mask) << bits_in_word_2;
    r.s.high +%= t >> bits_in_word_2;
    r.s.high +%= (a >> bits_in_word_2) *% (b >> bits_in_word_2);
    return r.all;
}

fn mulX(comptime T: type, a: T, b: T) T {
    const word_t = HalveInt(T, false);
    const x = word_t{ .all = a };
    const y = word_t{ .all = b };
    var r = word_t{ .all = muldXi(word_t.HalfT, x.s.low, y.s.low) };
    r.s.high +%= x.s.high *% y.s.low +% x.s.low *% y.s.high;
    return r.all;
}

// Public API: 128-bit multiplication

/// Signed 128-bit multiplication, decomposed to 64-bit ops.
pub fn mul_i128(a: i128, b: i128) i128 {
    return mulX(i128, a, b);
}

/// Wrapping unsigned 128-bit multiplication (low 128 bits only).
pub fn mul_u128_lo(a: u128, b: u128) u128 {
    return @bitCast(mulX(i128, @bitCast(a), @bitCast(b)));
}

// Public API: 128-bit division

/// Signed 128-bit truncating division.
pub fn divTrunc_i128(a: i128, b: i128) i128 {
    const s_a = a >> (128 - 1);
    const s_b = b >> (128 - 1);
    const an = (a ^ s_a) -% s_a;
    const bn = (b ^ s_b) -% s_b;
    const r = udivmod(u128, @bitCast(an), @bitCast(bn), null);
    const s = s_a ^ s_b;
    return (@as(i128, @bitCast(r)) ^ s) -% s;
}

/// Unsigned 128-bit truncating division.
pub fn divTrunc_u128(a: u128, b: u128) u128 {
    return udivmod(u128, a, b, null);
}

/// Signed 128-bit floor division.
pub fn divFloor_i128(a: i128, b: i128) i128 {
    const q = divTrunc_i128(a, b);
    const r = a -% mul_i128(q, b);
    // If remainder is nonzero and signs of a and b differ, subtract 1
    if (r != 0 and ((r ^ b) < 0)) {
        return q - 1;
    }
    return q;
}

// Public API: 128-bit remainder / modulo

/// Signed 128-bit truncating remainder (same sign as dividend).
pub fn rem_i128(a: i128, b: i128) i128 {
    const s_a = a >> (128 - 1);
    const s_b = b >> (128 - 1);
    const an = (a ^ s_a) -% s_a;
    const bn = (b ^ s_b) -% s_b;
    var r: u128 = undefined;
    _ = udivmod(u128, @as(u128, @bitCast(an)), @as(u128, @bitCast(bn)), &r);
    return (@as(i128, @bitCast(r)) ^ s_a) -% s_a;
}

/// Unsigned 128-bit remainder.
pub fn rem_u128(a: u128, b: u128) u128 {
    var r: u128 = undefined;
    _ = udivmod(u128, a, b, &r);
    return r;
}

/// Signed 128-bit modulo (result has same sign as divisor).
pub fn mod_i128(a: i128, b: i128) i128 {
    const r = rem_i128(a, b);
    // If remainder is nonzero and signs differ, add divisor
    if (r != 0 and ((r ^ b) < 0)) {
        return r + b;
    }
    return r;
}

// Public API: 128-bit <-> float conversions

/// Convert i128 to f32 using only 64-bit operations.
pub fn i128_to_f32(x: i128) f32 {
    if (x == 0) return 0.0;
    const sign: u32 = if (x < 0) 1 else 0;
    const abs_val: u128 = @abs(x);
    const result = u128_to_f32_impl(abs_val);
    return @bitCast(@as(u32, @bitCast(result)) | (sign << 31));
}

/// Convert u128 to f32 using only 64-bit operations.
pub fn u128_to_f32(x: u128) f32 {
    return u128_to_f32_impl(x);
}

fn u128_to_f32_impl(x: u128) f32 {
    if (x == 0) return 0.0;

    const leading_zeros: u8 = @clz(x);
    const bit_pos: u8 = 127 - leading_zeros;

    // f32 has 23 bits of mantissa (+ 1 implicit = 24 significant bits)
    if (bit_pos <= 23) {
        // Value fits exactly in f32 mantissa - truncate to u32 which is native
        const lo_val: u32 = @truncate(x);
        return @floatFromInt(lo_val);
    }

    // Check for overflow: f32 max exponent is 254 (bias 127), so bit_pos 128+ overflows
    if (bit_pos >= 128) return std.math.inf(f32);

    // Shift right to get 24 significant bits
    const shift: u7 = @intCast(bit_pos - 23);
    const shifted: u128 = x >> shift;
    var mantissa: u32 = @truncate(shifted);

    // Round to nearest even
    const round_bit: u128 = (x >> (@as(u7, shift) - 1)) & 1;
    const sticky_mask: u128 = (@as(u128, 1) << (@as(u7, shift) - 1)) - 1;
    const sticky: u128 = if ((x & sticky_mask) != 0) @as(u128, 1) else 0;

    if (round_bit != 0 and (sticky != 0 or (mantissa & 1) != 0)) {
        mantissa += 1;
        if (mantissa == (@as(u32, 1) << 24)) {
            mantissa >>= 1;
            const exponent: u32 = @as(u32, bit_pos) + 1 + 127;
            if (exponent >= 255) return std.math.inf(f32);
            const bits: u32 = (exponent << 23) | (mantissa & ((@as(u32, 1) << 23) - 1));
            return @bitCast(bits);
        }
    }

    const exponent: u32 = @as(u32, bit_pos) + 127;
    if (exponent >= 255) return std.math.inf(f32);
    const bits: u32 = (exponent << 23) | (mantissa & ((@as(u32, 1) << 23) - 1));
    return @bitCast(bits);
}

/// Convert i128 to f64 using only 64-bit operations.
pub fn i128_to_f64(x: i128) f64 {
    if (x == 0) return 0.0;

    const sign: u64 = if (x < 0) 1 else 0;
    const abs_val: u128 = @abs(x);

    const result = u128_to_f64_impl(abs_val);
    // Set sign bit
    return @bitCast(@as(u64, @bitCast(result)) | (sign << 63));
}

/// Convert u128 to f64 using only 64-bit operations.
pub fn u128_to_f64(x: u128) f64 {
    return u128_to_f64_impl(x);
}

fn u128_to_f64_impl(x: u128) f64 {
    if (x == 0) return 0.0;

    // Find the position of the highest set bit
    const leading_zeros: u8 = @clz(x);
    const bit_pos: u8 = 127 - leading_zeros; // 0-indexed position of highest bit

    // f64 has 52 bits of mantissa (+ 1 implicit)
    // We need to extract the top 53 significant bits and round
    if (bit_pos <= 52) {
        // Value fits exactly in f64 mantissa
        // The high part is zero, so the low 64 bits contain everything
        const lo_val: u64 = @truncate(x);
        return @floatFromInt(lo_val);
    }

    // We need to shift right by (bit_pos - 52) to get 53 bits
    const shift: u7 = @intCast(bit_pos - 52);
    const shifted: u128 = x >> shift;
    var mantissa: u64 = @truncate(shifted);

    // Round to nearest even
    const round_bit: u128 = x >> (@as(u7, shift) - 1) & 1;
    const sticky_mask: u128 = (@as(u128, 1) << (@as(u7, shift) - 1)) - 1;
    const sticky: u128 = if ((x & sticky_mask) != 0) @as(u128, 1) else 0;

    if (round_bit != 0 and (sticky != 0 or (mantissa & 1) != 0)) {
        mantissa += 1;
        // If mantissa overflows 53 bits, adjust exponent
        if (mantissa == (@as(u64, 1) << 53)) {
            mantissa >>= 1;
            const exponent: u64 = @as(u64, bit_pos) + 1 + 1023;
            const bits: u64 = (exponent << 52) | (mantissa & ((@as(u64, 1) << 52) - 1));
            return @bitCast(bits);
        }
    }

    // Construct the f64: exponent = bit_pos + 1023 (IEEE 754 bias)
    const exponent: u64 = @as(u64, bit_pos) + 1023;
    // Remove the implicit leading 1 bit
    const bits: u64 = (exponent << 52) | (mantissa & ((@as(u64, 1) << 52) - 1));
    return @bitCast(bits);
}

/// Convert f64 to i128 (truncating toward zero) using only 64-bit operations.
pub fn f64_to_i128(x: f64) i128 {
    if (std.math.isNan(x)) return 0;

    const bits: u64 = @bitCast(x);
    const sign = bits >> 63;
    const raw_exp = (bits >> 52) & 0x7FF;
    const mantissa = bits & ((@as(u64, 1) << 52) - 1);

    // Exponent bias is 1023
    if (raw_exp == 0) return 0; // zero or subnormal
    const exp: i64 = @as(i64, @intCast(raw_exp)) - 1023;

    if (exp < 0) return 0; // |x| < 1
    if (exp >= 127) {
        // Overflow: saturate
        return if (sign != 0) std.math.minInt(i128) else std.math.maxInt(i128);
    }

    // Reconstruct the integer: (1.mantissa) << (exp - 52)
    const full_mantissa: u128 = (@as(u128, 1) << 52) | @as(u128, mantissa);
    var result: u128 = undefined;
    if (exp >= 52) {
        const shift: u7 = @intCast(exp - 52);
        result = full_mantissa << shift;
    } else {
        const shift: u7 = @intCast(52 - exp);
        result = full_mantissa >> shift;
    }

    if (sign != 0) {
        return -%@as(i128, @intCast(result));
    } else {
        return @intCast(result);
    }
}

/// Convert f64 to u128 (truncating toward zero) using only 64-bit operations.
pub fn f64_to_u128(x: f64) u128 {
    if (std.math.isNan(x)) return 0;

    const bits: u64 = @bitCast(x);
    const sign = bits >> 63;
    if (sign != 0) return 0; // negative -> 0 for unsigned

    const raw_exp = (bits >> 52) & 0x7FF;
    const mantissa = bits & ((@as(u64, 1) << 52) - 1);

    if (raw_exp == 0) return 0;
    const exp: i64 = @as(i64, @intCast(raw_exp)) - 1023;

    if (exp < 0) return 0;
    if (exp >= 128) return std.math.maxInt(u128);

    const full_mantissa: u128 = (@as(u128, 1) << 52) | @as(u128, mantissa);
    if (exp >= 52) {
        const shift: u7 = @intCast(exp - 52);
        return full_mantissa << shift;
    } else {
        const shift: u7 = @intCast(52 - exp);
        return full_mantissa >> shift;
    }
}

// ─── Integer formatting (avoids std.fmt which calls @rem on u128) ────

/// Format a u128 as a decimal string into the provided buffer.
/// Returns the slice of `buf` that contains the formatted number.
/// Buffer must be at least 39 bytes (max u128 is 39 digits).
pub fn u128_to_str(buf: []u8, val: u128) FormatResult {
    return u128_to_str_inner(buf, val);
}

/// Result of formatting a 128-bit integer into a buffer.
pub const FormatResult = struct {
    str: []const u8,
    start: usize,
};

fn u128_to_str_inner(buf: []u8, val: u128) FormatResult {
    if (val == 0) {
        buf[buf.len - 1] = '0';
        return .{ .str = buf[buf.len - 1 ..], .start = buf.len - 1 };
    }

    var v = val;
    var pos: usize = buf.len;
    while (v != 0) {
        pos -= 1;
        const digit: u8 = @truncate(rem_u128(v, 10));
        buf[pos] = '0' + digit;
        v = divTrunc_u128(v, 10);
    }
    return .{ .str = buf[pos..], .start = pos };
}

/// Format an i128 as a decimal string into the provided buffer.
/// Returns the slice of `buf` that contains the formatted number.
/// Buffer must be at least 40 bytes (max i128 is 39 digits + sign).
pub fn i128_to_str(buf: []u8, val: i128) FormatResult {
    if (val < 0) {
        const result = u128_to_str_inner(buf, @abs(val));
        const sign_pos = result.start - 1;
        buf[sign_pos] = '-';
        return .{ .str = buf[sign_pos..], .start = sign_pos };
    }
    return u128_to_str_inner(buf, @as(u128, @intCast(val)));
}

// f64/f32 to string formatting.
// Zig's std.fmt.float uses u128 arithmetic internally (isPowerOf10, printIntAny),
// which pulls in __udivti3/__umodti3 from compiler-rt. Since we build with
// bundle_compiler_rt=false, we provide our own float-to-string conversion that
// uses only f64 and u64 arithmetic.

/// Format an f64 as a decimal string into the provided buffer.
/// Returns the slice of `buf` that contains the formatted number.
/// Buffer must be at least 32 bytes.
pub fn f64_to_str(buf: []u8, val: f64) []const u8 {
    // Special values
    if (math.isNan(val)) {
        @memcpy(buf[0..3], "NaN");
        return buf[0..3];
    }
    if (math.isInf(val)) {
        if (val < 0) {
            @memcpy(buf[0..4], "-inf");
            return buf[0..4];
        }
        @memcpy(buf[0..3], "inf");
        return buf[0..3];
    }

    var pos: usize = 0;

    // Zero (including -0)
    if (val == 0.0) {
        if (@as(u64, @bitCast(val)) >> 63 != 0) {
            @memcpy(buf[0..4], "-0.0");
            return buf[0..4];
        }
        @memcpy(buf[0..3], "0.0");
        return buf[0..3];
    }

    // Sign
    var v = val;
    if (v < 0) {
        buf[pos] = '-';
        pos += 1;
        v = -v;
    }

    // Get decimal exponent: floor(log10(v))
    var exp10: i32 = @intFromFloat(@floor(math.log10(v)));

    // Scale to [1.0, 10.0)
    var scaled: f64 = if (exp10 == 0)
        v
    else if (exp10 > 0 and exp10 < 23)
        v / pow10_f64(@intCast(exp10))
    else if (exp10 < 0 and exp10 > -23)
        v * pow10_f64(@intCast(-exp10))
    else
        v / math.pow(f64, 10.0, @floatFromInt(exp10));

    // Correct potential off-by-one from log10 rounding
    if (scaled >= 10.0) {
        scaled /= 10.0;
        exp10 += 1;
    }
    if (scaled < 1.0 and scaled > 0.0) {
        scaled *= 10.0;
        exp10 -= 1;
    }

    // Extract significant digits (up to 17 for f64)
    var digits: [18]u8 = undefined;
    var ndigits: usize = 0;
    var s = scaled;
    while (ndigits < 17) : (ndigits += 1) {
        const d: u8 = @intFromFloat(@min(s, 9.0));
        digits[ndigits] = d;
        s = (s - @as(f64, @floatFromInt(d))) * 10.0;
    }

    // Round: check if next digit >= 5
    if (s >= 5.0) {
        // Propagate rounding up
        var i: usize = ndigits;
        while (i > 0) {
            i -= 1;
            digits[i] += 1;
            if (digits[i] < 10) break;
            digits[i] = 0;
            if (i == 0) {
                // Carried past first digit: shift digits right, set first to 1
                var j: usize = ndigits;
                while (j > 0) : (j -= 1) {
                    digits[j] = digits[j - 1];
                }
                digits[0] = 1;
                exp10 += 1;
                break;
            }
        }
    }

    // Trim trailing zeros (keep at least 1 digit after decimal point)
    while (ndigits > 1 and digits[ndigits - 1] == 0) ndigits -= 1;

    // Format based on exponent range
    if (exp10 >= 0 and exp10 < 16) {
        // Decimal notation: e.g. 314.159
        const int_digits: usize = @intCast(exp10 + 1);
        if (int_digits >= ndigits) {
            // All digits are integer part
            for (digits[0..ndigits]) |d| {
                buf[pos] = '0' + d;
                pos += 1;
            }
            // Pad with zeros
            var pad = int_digits - ndigits;
            while (pad > 0) : (pad -= 1) {
                buf[pos] = '0';
                pos += 1;
            }
            buf[pos] = '.';
            pos += 1;
            buf[pos] = '0';
            pos += 1;
        } else {
            // Some digits before decimal, some after
            for (digits[0..int_digits]) |d| {
                buf[pos] = '0' + d;
                pos += 1;
            }
            buf[pos] = '.';
            pos += 1;
            for (digits[int_digits..ndigits]) |d| {
                buf[pos] = '0' + d;
                pos += 1;
            }
        }
    } else if (exp10 < 0 and exp10 >= -4) {
        // Small decimal: e.g. 0.00123
        buf[pos] = '0';
        pos += 1;
        buf[pos] = '.';
        pos += 1;
        var zeros: usize = @intCast(-exp10 - 1);
        while (zeros > 0) : (zeros -= 1) {
            buf[pos] = '0';
            pos += 1;
        }
        for (digits[0..ndigits]) |d| {
            buf[pos] = '0' + d;
            pos += 1;
        }
    } else {
        // Scientific notation: e.g. 1.23e45
        buf[pos] = '0' + digits[0];
        pos += 1;
        if (ndigits > 1) {
            buf[pos] = '.';
            pos += 1;
            for (digits[1..ndigits]) |d| {
                buf[pos] = '0' + d;
                pos += 1;
            }
        }
        buf[pos] = 'e';
        pos += 1;
        if (exp10 < 0) {
            buf[pos] = '-';
            pos += 1;
            pos = writeU64(buf, pos, @intCast(-exp10));
        } else {
            pos = writeU64(buf, pos, @intCast(exp10));
        }
    }

    return buf[0..pos];
}

/// Format an f32 as a decimal string (converts to f64 internally).
pub fn f32_to_str(buf: []u8, val: f32) []const u8 {
    return f64_to_str(buf, @as(f64, val));
}

/// Fast power-of-10 lookup for small exponents (0..22).
fn pow10_f64(exp: u5) f64 {
    const table = [_]f64{
        1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,
        1e8,  1e9,  1e10, 1e11, 1e12, 1e13, 1e14, 1e15,
        1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
    };
    return table[exp];
}

/// Write a u64 in decimal to buf starting at pos, return new pos.
fn writeU64(buf: []u8, start: usize, val: u64) usize {
    if (val == 0) {
        buf[start] = '0';
        return start + 1;
    }
    var tmp: [20]u8 = undefined;
    var len: usize = 0;
    var v = val;
    while (v != 0) {
        tmp[len] = @truncate(v % 10);
        len += 1;
        v /= 10;
    }
    var pos = start;
    var i: usize = len;
    while (i > 0) {
        i -= 1;
        buf[pos] = '0' + tmp[i];
        pos += 1;
    }
    return pos;
}
