const std = @import("std");
const builtin = @import("builtin");
const math = std.math;

// Eventaully, we need to statically ingest compiler-rt and get it working with the surgical linker, then these should not be needed anymore.
// Until then, we are manually ingesting used parts of compiler-rt here.
//
// Taken from
// https://github.com/ziglang/zig/tree/4976b58ab16069f8d3267b69ed030f29685c1abe/lib/compiler_rt//
// Thank you Zig Contributors!

// Libcalls that involve u128 on Windows x86-64 are expected by LLVM to use the
// calling convention of @Vector(2, u64), rather than what's standard.
pub const want_windows_v2u64_abi = builtin.os.tag == .windows and builtin.cpu.arch == .x86_64 and @import("builtin").object_format != .c;

const v2u64 = @Vector(2, u64);

// Export it as weak incase it is already linked in by something else.
comptime {
    @export(__muloti4, .{ .name = "__muloti4", .linkage = .Weak });
    if (want_windows_v2u64_abi) {
        @export(__divti3_windows_x86_64, .{ .name = "__divti3", .linkage = .Weak });
        @export(__modti3_windows_x86_64, .{ .name = "__modti3", .linkage = .Weak });
        @export(__udivti3_windows_x86_64, .{ .name = "__udivti3", .linkage = .Weak });
    } else {
        @export(__divti3, .{ .name = "__divti3", .linkage = .Weak });
        @export(__modti3, .{ .name = "__modti3", .linkage = .Weak });
        @export(__udivti3, .{ .name = "__udivti3", .linkage = .Weak });
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
    return @bitCast(v2u64, div(@bitCast(i128, a), @bitCast(i128, b)));
}

inline fn div(a: i128, b: i128) i128 {
    const s_a = a >> (128 - 1);
    const s_b = b >> (128 - 1);

    const an = (a ^ s_a) -% s_a;
    const bn = (b ^ s_b) -% s_b;

    const r = udivmod(u128, @bitCast(u128, an), @bitCast(u128, bn), null);
    const s = s_a ^ s_b;
    return (@bitCast(i128, r) ^ s) -% s;
}

pub fn __udivti3(a: u128, b: u128) callconv(.C) u128 {
    return udivmod(u128, a, b, null);
}

fn __udivti3_windows_x86_64(a: v2u64, b: v2u64) callconv(.C) v2u64 {
    return @bitCast(v2u64, udivmod(u128, @bitCast(u128, a), @bitCast(u128, b), null));
}

pub fn __modti3(a: i128, b: i128) callconv(.C) i128 {
    return mod(a, b);
}

fn __modti3_windows_x86_64(a: v2u64, b: v2u64) callconv(.C) v2u64 {
    return @bitCast(v2u64, mod(@bitCast(i128, a), @bitCast(i128, b)));
}

inline fn mod(a: i128, b: i128) i128 {
    const s_a = a >> (128 - 1); // s = a < 0 ? -1 : 0
    const s_b = b >> (128 - 1); // s = b < 0 ? -1 : 0

    const an = (a ^ s_a) -% s_a; // negate if s == -1
    const bn = (b ^ s_b) -% s_b; // negate if s == -1

    var r: u128 = undefined;
    _ = udivmod(u128, @bitCast(u128, an), @bitCast(u128, bn), &r);
    return (@bitCast(i128, r) ^ s_a) -% s_a; // negate if s == -1
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
    return @truncate(ST, res);
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

    const n = @bitCast([2]SingleInt, a);
    const d = @bitCast([2]SingleInt, b);
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
                rem.* = @bitCast(DoubleInt, r);
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
                rem.* = @bitCast(DoubleInt, r);
            }
            return n[high] >> @intCast(Log2SingleInt, @ctz(SingleInt, d[high]));
        }
        // K K
        // ---
        // K 0
        sr = @bitCast(c_uint, @as(c_int, @clz(SingleInt, d[high])) - @as(c_int, @clz(SingleInt, n[high])));
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
        q[high] = n[low] << @intCast(Log2SingleInt, single_int_bits - sr);
        // r.all = a >> sr;
        r[high] = n[high] >> @intCast(Log2SingleInt, sr);
        r[low] = (n[high] << @intCast(Log2SingleInt, single_int_bits - sr)) | (n[low] >> @intCast(Log2SingleInt, sr));
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
                sr = @ctz(SingleInt, d[low]);
                q[high] = n[high] >> @intCast(Log2SingleInt, sr);
                q[low] = (n[high] << @intCast(Log2SingleInt, single_int_bits - sr)) | (n[low] >> @intCast(Log2SingleInt, sr));
                return @bitCast(DoubleInt, q);
            }
            // K X
            // ---
            // 0 K
            sr = 1 + single_int_bits + @as(c_uint, @clz(SingleInt, d[low])) - @as(c_uint, @clz(SingleInt, n[high]));
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
                q[high] = n[low] << @intCast(Log2SingleInt, single_int_bits - sr);
                r[high] = n[high] >> @intCast(Log2SingleInt, sr);
                r[low] = (n[high] << @intCast(Log2SingleInt, single_int_bits - sr)) | (n[low] >> @intCast(Log2SingleInt, sr));
            } else {
                // single_int_bits + 1 <= sr <= double_int_bits - 1
                q[low] = n[low] << @intCast(Log2SingleInt, double_int_bits - sr);
                q[high] = (n[high] << @intCast(Log2SingleInt, double_int_bits - sr)) | (n[low] >> @intCast(Log2SingleInt, sr - single_int_bits));
                r[high] = 0;
                r[low] = n[high] >> @intCast(Log2SingleInt, sr - single_int_bits);
            }
        } else {
            // K X
            // ---
            // K K
            sr = @bitCast(c_uint, @as(c_int, @clz(SingleInt, d[high])) - @as(c_int, @clz(SingleInt, n[high])));
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
                r[high] = n[high] >> @intCast(Log2SingleInt, sr);
                r[low] = (n[high] << @intCast(Log2SingleInt, single_int_bits - sr)) | (n[low] >> @intCast(Log2SingleInt, sr));
                q[high] = n[low] << @intCast(Log2SingleInt, single_int_bits - sr);
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
        r_all = @bitCast(DoubleInt, r);
        const s: SignedDoubleInt = @bitCast(SignedDoubleInt, b -% r_all -% 1) >> (double_int_bits - 1);
        carry = @intCast(u32, s & 1);
        r_all -= b & @bitCast(DoubleInt, s);
        r = @bitCast([2]SingleInt, r_all);
    }
    const q_all = (@bitCast(DoubleInt, q) << 1) | carry;
    if (maybe_rem) |rem| {
        rem.* = r_all;
    }
    return q_all;
}
