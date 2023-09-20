const std = @import("std");
const builtin = @import("builtin");
const math = std.math;

// Eventually, we need to statically ingest compiler-rt and get it working with the surgical linker, then these should not be needed anymore.
// Until then, we are manually ingesting used parts of compiler-rt here.
//
// Taken from
// https://github.com/ziglang/zig/tree/4976b58ab16069f8d3267b69ed030f29685c1abe/lib/compiler_rt/
// Thank you Zig Contributors!

// Libcalls that involve u128 on Windows x86-64 are expected by LLVM to use the
// calling convention of @Vector(2, u64), rather than what's standard.
pub const want_windows_v2u64_abi = builtin.os.tag == .windows and builtin.cpu.arch == .x86_64 and @import("builtin").object_format != .c;

const v2u64 = @Vector(2, u64);

// Export it as weak incase it is already linked in by something else.
comptime {
    if (!want_windows_v2u64_abi) {
        @export(__muloti4, .{ .name = "__muloti4", .linkage = .Weak });
        @export(__lshrti3, .{ .name = "__lshrti3", .linkage = .Weak });
        @export(__divti3, .{ .name = "__divti3", .linkage = .Weak });
        @export(__modti3, .{ .name = "__modti3", .linkage = .Weak });
        @export(__umodti3, .{ .name = "__umodti3", .linkage = .Weak });
        @export(__udivti3, .{ .name = "__udivti3", .linkage = .Weak });
        @export(__fixdfti, .{ .name = "__fixdfti", .linkage = .Weak });
        @export(__fixsfti, .{ .name = "__fixsfti", .linkage = .Weak });
        @export(__fixunsdfti, .{ .name = "__fixunsdfti", .linkage = .Weak });
        @export(__fixunssfti, .{ .name = "__fixunssfti", .linkage = .Weak });
    }
}

pub fn __muloti4(a: i128, b: i128, overflow: *c_int) callconv(.C) i128 {
    if (2 * @bitSizeOf(i128) <= @bitSizeOf(usize)) {
        return muloXi4_genericFast(i128, a, b, overflow);
    } else {
        return muloXi4_genericSmall(i128, a, b, overflow);
    }
}

pub fn __divti3(a: i128, b: i128) callconv(.C) i128 {
    return div(a, b);
}

fn __divti3_windows_x86_64(a: v2u64, b: v2u64) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(div(@as(i128, @bitCast(a)), @as(i128, @bitCast(b)))));
}

inline fn div(a: i128, b: i128) i128 {
    const s_a = a >> (128 - 1);
    const s_b = b >> (128 - 1);

    const an = (a ^ s_a) -% s_a;
    const bn = (b ^ s_b) -% s_b;

    const r = udivmod(u128, @as(u128, @bitCast(an)), @as(u128, @bitCast(bn)), null);
    const s = s_a ^ s_b;
    return (@as(i128, @bitCast(r)) ^ s) -% s;
}

pub fn __udivti3(a: u128, b: u128) callconv(.C) u128 {
    return udivmod(u128, a, b, null);
}

fn __udivti3_windows_x86_64(a: v2u64, b: v2u64) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(udivmod(u128, @as(u128, @bitCast(a)), @as(u128, @bitCast(b)), null)));
}

pub fn __umodti3(a: u128, b: u128) callconv(.C) u128 {
    var r: u128 = undefined;
    _ = udivmod(u128, a, b, &r);
    return r;
}

fn __umodti3_windows_x86_64(a: v2u64, b: v2u64) callconv(.C) v2u64 {
    var r: u128 = undefined;
    _ = udivmod(u128, @as(u128, @bitCast(a)), @as(u128, @bitCast(b)), &r);
    return @as(v2u64, @bitCast(r));
}

pub fn __modti3(a: i128, b: i128) callconv(.C) i128 {
    return mod(a, b);
}

fn __modti3_windows_x86_64(a: v2u64, b: v2u64) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(mod(@as(i128, @bitCast(a)), @as(i128, @bitCast(b)))));
}

inline fn mod(a: i128, b: i128) i128 {
    const s_a = a >> (128 - 1); // s = a < 0 ? -1 : 0
    const s_b = b >> (128 - 1); // s = b < 0 ? -1 : 0

    const an = (a ^ s_a) -% s_a; // negate if s == -1
    const bn = (b ^ s_b) -% s_b; // negate if s == -1

    var r: u128 = undefined;
    _ = udivmod(u128, @as(u128, @bitCast(an)), @as(u128, @bitCast(bn)), &r);
    return (@as(i128, @bitCast(r)) ^ s_a) -% s_a; // negate if s == -1
}

pub fn __fixdfti(a: f64) callconv(.C) i128 {
    return floatToInt(i128, a);
}

fn __fixdfti_windows_x86_64(a: f64) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(floatToInt(i128, a)));
}

pub fn __fixsfti(a: f32) callconv(.C) i128 {
    return floatToInt(i128, a);
}

fn __fixsfti_windows_x86_64(a: f32) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(floatToInt(i128, a)));
}

pub fn __fixunsdfti(a: f64) callconv(.C) u128 {
    return floatToInt(u128, a);
}

fn __fixunsdfti_windows_x86_64(a: f64) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(floatToInt(u128, a)));
}

pub fn __fixunssfti(a: f32) callconv(.C) u128 {
    return floatToInt(u128, a);
}

fn __fixunssfti_windows_x86_64(a: f32) callconv(.C) v2u64 {
    return @as(v2u64, @bitCast(floatToInt(u128, a)));
}
// mulo - multiplication overflow
// * return a*%b.
// * return if a*b overflows => 1 else => 0
// - muloXi4_genericSmall as default
// - muloXi4_genericFast for 2*bitsize <= usize

inline fn muloXi4_genericSmall(comptime ST: type, a: ST, b: ST, overflow: *c_int) ST {
    overflow.* = 0;
    const min = math.minInt(ST);
    var res: ST = a *% b;
    // Hacker's Delight section Overflow subsection Multiplication
    // case a=-2^{31}, b=-1 problem, because
    // on some machines a*b = -2^{31} with overflow
    // Then -2^{31}/-1 overflows and any result is possible.
    // => check with a<0 and b=-2^{31}
    if ((a < 0 and b == min) or (a != 0 and @divTrunc(res, a) != b))
        overflow.* = 1;
    return res;
}

inline fn muloXi4_genericFast(comptime ST: type, a: ST, b: ST, overflow: *c_int) ST {
    overflow.* = 0;
    const EST = switch (ST) {
        i32 => i64,
        i64 => i128,
        i128 => i256,
        else => unreachable,
    };
    const min = math.minInt(ST);
    const max = math.maxInt(ST);
    var res: EST = @as(EST, a) * @as(EST, b);
    //invariant: -2^{bitwidth(EST)} < res < 2^{bitwidth(EST)-1}
    if (res < min or max < res)
        overflow.* = 1;
    return @as(ST, @truncate(res));
}

const native_endian = builtin.cpu.arch.endian();
const low = switch (native_endian) {
    .Big => 1,
    .Little => 0,
};
const high = 1 - low;

pub fn udivmod(comptime DoubleInt: type, a: DoubleInt, b: DoubleInt, maybe_rem: ?*DoubleInt) DoubleInt {
    // @setRuntimeSafety(builtin.is_test);

    const double_int_bits = @typeInfo(DoubleInt).Int.bits;
    const single_int_bits = @divExact(double_int_bits, 2);
    const SingleInt = std.meta.Int(.unsigned, single_int_bits);
    const SignedDoubleInt = std.meta.Int(.signed, double_int_bits);
    const Log2SingleInt = std.math.Log2Int(SingleInt);

    const n = @as([2]SingleInt, @bitCast(a));
    const d = @as([2]SingleInt, @bitCast(b));
    var q: [2]SingleInt = undefined;
    var r: [2]SingleInt = undefined;
    var sr: c_uint = undefined;
    // special cases, X is unknown, K != 0
    if (n[high] == 0) {
        if (d[high] == 0) {
            // 0 X
            // ---
            // 0 X
            if (maybe_rem) |rem| {
                rem.* = n[low] % d[low];
            }
            return n[low] / d[low];
        }
        // 0 X
        // ---
        // K X
        if (maybe_rem) |rem| {
            rem.* = n[low];
        }
        return 0;
    }
    // n[high] != 0
    if (d[low] == 0) {
        if (d[high] == 0) {
            // K X
            // ---
            // 0 0
            if (maybe_rem) |rem| {
                rem.* = n[high] % d[low];
            }
            return n[high] / d[low];
        }
        // d[high] != 0
        if (n[low] == 0) {
            // K 0
            // ---
            // K 0
            if (maybe_rem) |rem| {
                r[high] = n[high] % d[high];
                r[low] = 0;
                rem.* = @as(DoubleInt, @bitCast(r));
            }
            return n[high] / d[high];
        }
        // K K
        // ---
        // K 0
        if ((d[high] & (d[high] - 1)) == 0) {
            // d is a power of 2
            if (maybe_rem) |rem| {
                r[low] = n[low];
                r[high] = n[high] & (d[high] - 1);
                rem.* = @as(DoubleInt, @bitCast(r));
            }
            return n[high] >> @as(Log2SingleInt, @intCast(@ctz(d[high])));
        }
        // K K
        // ---
        // K 0
        sr = @as(c_uint, @bitCast(@as(c_int, @clz(d[high])) - @as(c_int, @clz(n[high]))));
        // 0 <= sr <= single_int_bits - 2 or sr large
        if (sr > single_int_bits - 2) {
            if (maybe_rem) |rem| {
                rem.* = a;
            }
            return 0;
        }
        sr += 1;
        // 1 <= sr <= single_int_bits - 1
        // q.all = a << (double_int_bits - sr);
        q[low] = 0;
        q[high] = n[low] << @as(Log2SingleInt, @intCast(single_int_bits - sr));
        // r.all = a >> sr;
        r[high] = n[high] >> @as(Log2SingleInt, @intCast(sr));
        r[low] = (n[high] << @as(Log2SingleInt, @intCast(single_int_bits - sr))) | (n[low] >> @as(Log2SingleInt, @intCast(sr)));
    } else {
        // d[low] != 0
        if (d[high] == 0) {
            // K X
            // ---
            // 0 K
            if ((d[low] & (d[low] - 1)) == 0) {
                // d is a power of 2
                if (maybe_rem) |rem| {
                    rem.* = n[low] & (d[low] - 1);
                }
                if (d[low] == 1) {
                    return a;
                }
                sr = @ctz(d[low]);
                q[high] = n[high] >> @as(Log2SingleInt, @intCast(sr));
                q[low] = (n[high] << @as(Log2SingleInt, @intCast(single_int_bits - sr))) | (n[low] >> @as(Log2SingleInt, @intCast(sr)));
                return @as(DoubleInt, @bitCast(q));
            }
            // K X
            // ---
            // 0 K
            sr = 1 + single_int_bits + @as(c_uint, @clz(d[low])) - @as(c_uint, @clz(n[high]));
            // 2 <= sr <= double_int_bits - 1
            // q.all = a << (double_int_bits - sr);
            // r.all = a >> sr;
            if (sr == single_int_bits) {
                q[low] = 0;
                q[high] = n[low];
                r[high] = 0;
                r[low] = n[high];
            } else if (sr < single_int_bits) {
                // 2 <= sr <= single_int_bits - 1
                q[low] = 0;
                q[high] = n[low] << @as(Log2SingleInt, @intCast(single_int_bits - sr));
                r[high] = n[high] >> @as(Log2SingleInt, @intCast(sr));
                r[low] = (n[high] << @as(Log2SingleInt, @intCast(single_int_bits - sr))) | (n[low] >> @as(Log2SingleInt, @intCast(sr)));
            } else {
                // single_int_bits + 1 <= sr <= double_int_bits - 1
                q[low] = n[low] << @as(Log2SingleInt, @intCast(double_int_bits - sr));
                q[high] = (n[high] << @as(Log2SingleInt, @intCast(double_int_bits - sr))) | (n[low] >> @as(Log2SingleInt, @intCast(sr - single_int_bits)));
                r[high] = 0;
                r[low] = n[high] >> @as(Log2SingleInt, @intCast(sr - single_int_bits));
            }
        } else {
            // K X
            // ---
            // K K
            sr = @as(c_uint, @bitCast(@as(c_int, @clz(d[high])) - @as(c_int, @clz(n[high]))));
            // 0 <= sr <= single_int_bits - 1 or sr large
            if (sr > single_int_bits - 1) {
                if (maybe_rem) |rem| {
                    rem.* = a;
                }
                return 0;
            }
            sr += 1;
            // 1 <= sr <= single_int_bits
            // q.all = a << (double_int_bits - sr);
            // r.all = a >> sr;
            q[low] = 0;
            if (sr == single_int_bits) {
                q[high] = n[low];
                r[high] = 0;
                r[low] = n[high];
            } else {
                r[high] = n[high] >> @as(Log2SingleInt, @intCast(sr));
                r[low] = (n[high] << @as(Log2SingleInt, @intCast(single_int_bits - sr))) | (n[low] >> @as(Log2SingleInt, @intCast(sr)));
                q[high] = n[low] << @as(Log2SingleInt, @intCast(single_int_bits - sr));
            }
        }
    }
    // Not a special case
    // q and r are initialized with:
    // q.all = a << (double_int_bits - sr);
    // r.all = a >> sr;
    // 1 <= sr <= double_int_bits - 1
    var carry: u32 = 0;
    var r_all: DoubleInt = undefined;
    while (sr > 0) : (sr -= 1) {
        // r:q = ((r:q)  << 1) | carry
        r[high] = (r[high] << 1) | (r[low] >> (single_int_bits - 1));
        r[low] = (r[low] << 1) | (q[high] >> (single_int_bits - 1));
        q[high] = (q[high] << 1) | (q[low] >> (single_int_bits - 1));
        q[low] = (q[low] << 1) | carry;
        // carry = 0;
        // if (r.all >= b)
        // {
        //     r.all -= b;
        //      carry = 1;
        // }
        r_all = @as(DoubleInt, @bitCast(r));
        const s: SignedDoubleInt = @as(SignedDoubleInt, @bitCast(b -% r_all -% 1)) >> (double_int_bits - 1);
        carry = @as(u32, @intCast(s & 1));
        r_all -= b & @as(DoubleInt, @bitCast(s));
        r = @as([2]SingleInt, @bitCast(r_all));
    }
    const q_all = (@as(DoubleInt, @bitCast(q)) << 1) | carry;
    if (maybe_rem) |rem| {
        rem.* = r_all;
    }
    return q_all;
}

pub inline fn floatToInt(comptime I: type, a: anytype) I {
    const Log2Int = math.Log2Int;
    const Int = @import("std").meta.Int;
    const F = @TypeOf(a);
    const float_bits = @typeInfo(F).Float.bits;
    const int_bits = @typeInfo(I).Int.bits;
    const rep_t = Int(.unsigned, float_bits);
    const sig_bits = math.floatMantissaBits(F);
    const exp_bits = math.floatExponentBits(F);
    const fractional_bits = floatFractionalBits(F);

    // const implicit_bit = if (F != f80) (@as(rep_t, 1) << sig_bits) else 0;
    const implicit_bit = @as(rep_t, 1) << sig_bits;
    const max_exp = (1 << (exp_bits - 1));
    const exp_bias = max_exp - 1;
    const sig_mask = (@as(rep_t, 1) << sig_bits) - 1;

    // Break a into sign, exponent, significand
    const a_rep: rep_t = @as(rep_t, @bitCast(a));
    const negative = (a_rep >> (float_bits - 1)) != 0;
    const exponent = @as(i32, @intCast((a_rep << 1) >> (sig_bits + 1))) - exp_bias;
    const significand: rep_t = (a_rep & sig_mask) | implicit_bit;

    // If the exponent is negative, the result rounds to zero.
    if (exponent < 0) return 0;

    // If the value is too large for the integer type, saturate.
    switch (@typeInfo(I).Int.signedness) {
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
        result = @as(I, @intCast(significand >> @as(Log2Int(rep_t), @intCast(fractional_bits - exponent))));
    } else {
        result = @as(I, @intCast(significand)) << @as(Log2Int(I), @intCast(exponent - fractional_bits));
    }

    if ((@typeInfo(I).Int.signedness == .signed) and negative)
        return ~result +% 1;
    return result;
}

/// Returns the number of fractional bits in the mantissa of floating point type T.
pub inline fn floatFractionalBits(comptime T: type) comptime_int {
    comptime std.debug.assert(@typeInfo(T) == .Float);

    // standard IEEE floats have an implicit 0.m or 1.m integer part
    // f80 is special and has an explicitly stored bit in the MSB
    // this function corresponds to `MANT_DIG - 1' from C
    return switch (@typeInfo(T).Float.bits) {
        16 => 10,
        32 => 23,
        64 => 52,
        80 => 63,
        128 => 112,
        else => @compileError("unknown floating point type " ++ @typeName(T)),
    };
}

pub fn __lshrti3(a: i128, b: i32) callconv(.C) i128 {
    return lshrXi3(i128, a, b);
}

// Logical shift right: shift in 0 from left to right
// Precondition: 0 <= b < T.bit_count
inline fn lshrXi3(comptime T: type, a: T, b: i32) T {
    const word_t = HalveInt(T, false);
    const S = std.math.Log2Int(word_t.HalfT);

    const input = word_t{ .all = a };
    var output: word_t = undefined;

    if (b >= word_t.bits) {
        output.s.high = 0;
        output.s.low = input.s.high >> @as(S, @intCast(b - word_t.bits));
    } else if (b == 0) {
        return a;
    } else {
        output.s.high = input.s.high >> @as(S, @intCast(b));
        output.s.low = input.s.high << @as(S, @intCast(word_t.bits - b));
        output.s.low |= input.s.low >> @as(S, @intCast(b));
    }

    return output.all;
}

/// Allows to access underlying bits as two equally sized lower and higher
/// signed or unsigned integers.
fn HalveInt(comptime T: type, comptime signed_half: bool) type {
    return extern union {
        pub const bits = @divExact(@typeInfo(T).Int.bits, 2);
        pub const HalfTU = std.meta.Int(.unsigned, bits);
        pub const HalfTS = std.meta.Int(.signed, bits);
        pub const HalfT = if (signed_half) HalfTS else HalfTU;

        all: T,
        s: if (native_endian == .Little)
            extern struct { low: HalfT, high: HalfT }
        else
            extern struct { high: HalfT, low: HalfT },
    };
}
