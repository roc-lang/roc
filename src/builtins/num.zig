//! Builtin numeric operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's numeric types and
//! operations, including integer and floating-point arithmetic, parsing,
//! overflow detection, and conversions. It defines numeric parsing utilities
//! and functions that are called from compiled Roc code to handle numeric
//! operations efficiently and safely.
const std = @import("std");
const i128h = @import("compiler_rt_128.zig");
const parse_float = @import("vendor_parse_float");

const WithOverflow = @import("utils.zig").WithOverflow;
const Ordering = @import("utils.zig").Ordering;
const RocOps = @import("utils.zig").RocOps;
const TestEnv = @import("utils.zig").TestEnv;
const RocStr = @import("str.zig").RocStr;
const math = std.math;

/// Result type for numeric parsing, with value and error code.
pub fn NumParseResult(comptime T: type) type {
    // on the roc side we sort by alignment; putting the errorcode last
    // always works out (no number with smaller alignment than 1)
    return extern struct {
        value: T,
        errorcode: u8, // 0 indicates success
    };
}

/// Bitwise parts of a 32-bit float.
pub const F32Parts = extern struct {
    fraction: u32,
    exponent: u8,
    sign: bool,
};

/// Bitwise parts of a 64-bit float.
pub const F64Parts = extern struct {
    fraction: u64,
    exponent: u16,
    sign: bool,
};

/// 256-bit unsigned integer, as two 128-bit values.
pub const U256 = struct {
    hi: u128,
    lo: u128,
};

/// Multiplies two u128 values, returning a 256-bit result.
/// Uses i128h.mul_u64_wide for each partial product and i128h.shl/shr for
/// shifts, so this is compiler-rt-free on all targets including wasm32.
pub fn mul_u128(a: u128, b: u128) U256 {
    const wide = i128h.mul_u64_wide;
    const shift = i128h.shr;
    const lower_mask: u128 = math.maxInt(u64);

    // Split each u128 into two u64 halves via @bitCast (no shift needed).
    const a_halves: [2]u64 = @bitCast(a);
    const b_halves: [2]u64 = @bitCast(b);
    const a_lo = a_halves[0];
    const a_hi = a_halves[1];
    const b_lo = b_halves[0];
    const b_hi = b_halves[1];

    var lo: u128 = wide(a_lo, b_lo);

    var t = shift(lo, 64);
    lo &= lower_mask;

    t += wide(a_hi, b_lo);
    lo += i128h.shl(t & lower_mask, 64);
    var hi: u128 = shift(t, 64);

    t = shift(lo, 64);
    lo &= lower_mask;

    t += wide(b_hi, a_lo);
    lo += i128h.shl(t & lower_mask, 64);
    hi += shift(t, 64);

    hi += wide(a_hi, b_hi);

    return .{ .hi = hi, .lo = lo };
}

/// Parses an integer from a RocStr
pub fn parseIntFromStr(comptime T: type, buf: RocStr) NumParseResult(T) {
    if (parseIntNoFmt(T, buf.asSlice())) |success| {
        return .{ .errorcode = 0, .value = success };
    } else |_| {
        return .{ .errorcode = 1, .value = 0 };
    }
}

const ParseIntError = error{
    InvalidCharacter,
    Overflow,
};

fn parseIntNoFmt(comptime T: type, bytes: []const u8) ParseIntError!T {
    if (bytes.len == 0) return error.InvalidCharacter;

    const info = @typeInfo(T).int;
    const signed = info.signedness == .signed;

    var index: usize = 0;
    const negative = bytes[index] == '-';
    if (bytes[index] == '-' or bytes[index] == '+') {
        index += 1;
        if (index == bytes.len) return error.InvalidCharacter;
    }

    const radix = detectRadix(bytes, &index);

    if (signed) {
        const positive_limit: u128 = @intCast(std.math.maxInt(T));
        const negative_limit = positive_limit + 1;
        const magnitude = if (negative)
            try parseMagnitude(negative_limit, bytes, index, radix)
        else
            try parseMagnitude(positive_limit, bytes, index, radix);

        if (negative) {
            if (magnitude == negative_limit) return std.math.minInt(T);
            const positive: T = @intCast(magnitude);
            return -positive;
        }
        return @intCast(magnitude);
    } else {
        const magnitude = try parseMagnitude(@as(u128, @intCast(std.math.maxInt(T))), bytes, index, radix);
        if (negative and magnitude != 0) return error.Overflow;
        return @intCast(magnitude);
    }
}

/// Parse an unsigned decimal integer, returning null on invalid input or overflow.
pub fn parseUnsignedDecimal(comptime T: type, bytes: []const u8) ?T {
    const info = @typeInfo(T).int;
    const limit: u128 = switch (info.signedness) {
        .signed => @intCast(std.math.maxInt(T)),
        .unsigned => @intCast(std.math.maxInt(T)),
    };
    const magnitude = parseMagnitude(limit, bytes, 0, 10) catch return null;
    return @intCast(magnitude);
}

fn detectRadix(bytes: []const u8, index: *usize) u8 {
    if (bytes.len - index.* >= 2 and bytes[index.*] == '0') {
        switch (bytes[index.* + 1]) {
            'b', 'B' => {
                index.* += 2;
                return 2;
            },
            'o', 'O' => {
                index.* += 2;
                return 8;
            },
            'x', 'X' => {
                index.* += 2;
                return 16;
            },
            else => {},
        }
    }
    return 10;
}

fn parseMagnitude(comptime limit: u128, bytes: []const u8, start: usize, radix: u8) ParseIntError!u128 {
    return switch (radix) {
        2 => parseMagnitudeRadix(2, limit, bytes, start),
        8 => parseMagnitudeRadix(8, limit, bytes, start),
        10 => parseMagnitudeRadix(10, limit, bytes, start),
        16 => parseMagnitudeRadix(16, limit, bytes, start),
        else => unreachable,
    };
}

fn parseMagnitudeRadix(comptime radix: u8, comptime limit: u128, bytes: []const u8, start: usize) ParseIntError!u128 {
    const max_before_mul = limit / radix;
    const max_digit = limit % radix;

    var value: u128 = 0;
    var saw_digit = false;
    var previous_underscore = false;

    for (bytes[start..]) |byte| {
        if (byte == '_') {
            if (!saw_digit or previous_underscore) return error.InvalidCharacter;
            previous_underscore = true;
            continue;
        }

        const digit = digitValue(byte) orelse return error.InvalidCharacter;
        if (digit >= radix) return error.InvalidCharacter;
        if (value > max_before_mul or (value == max_before_mul and digit > max_digit)) {
            return error.Overflow;
        }

        value = value * radix + digit;
        saw_digit = true;
        previous_underscore = false;
    }

    if (!saw_digit or previous_underscore) return error.InvalidCharacter;
    return value;
}

fn digitValue(byte: u8) ?u8 {
    return switch (byte) {
        '0'...'9' => byte - '0',
        'a'...'z' => byte - 'a' + 10,
        'A'...'Z' => byte - 'A' + 10,
        else => null,
    };
}

/// Exports a function to parse integers from strings.
pub fn exportParseInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(buf: RocStr) callconv(.c) NumParseResult(T) {
            return @call(.always_inline, parseIntFromStr, .{ T, buf });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Parses a floating-point number from a RocStr.
pub fn parseFloatFromStr(comptime T: type, buf: RocStr) NumParseResult(T) {
    const bytes = buf.asSlice();
    if (parse_float.parseFloat(T, bytes)) |success| {
        if (std.math.isInf(success) and !isExplicitInfinity(bytes)) {
            return .{ .errorcode = 1, .value = 0 };
        }
        return .{ .errorcode = 0, .value = success };
    } else |_| {
        return .{ .errorcode = 1, .value = 0 };
    }
}

fn isExplicitInfinity(bytes: []const u8) bool {
    var text = std.mem.trim(u8, bytes, " \t\r\n");
    if (text.len > 0 and (text[0] == '+' or text[0] == '-')) {
        text = text[1..];
    }
    return std.ascii.eqlIgnoreCase(text, "inf") or
        std.ascii.eqlIgnoreCase(text, "infinity");
}

/// Exports a function to parse floating-point numbers from strings.
pub fn exportParseFloat(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(buf: RocStr) callconv(.c) NumParseResult(T) {
            return @call(.always_inline, parseFloatFromStr, .{ T, buf });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Cast an integer to a float.
pub fn exportNumToFloatCast(comptime T: type, comptime F: type, comptime name: []const u8) void {
    const f = struct {
        fn func(x: T) callconv(.c) F {
            if (T == i128) {
                const result = i128h.i128_to_f64(x);
                return if (F == f32) @floatCast(result) else result;
            } else if (T == u128) {
                const result = i128h.u128_to_f64(x);
                return if (F == f32) @floatCast(result) else result;
            } else {
                return @floatFromInt(x);
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Raise a number to a power, with overflow handling.
pub fn exportPow(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            base: T,
            exp: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            switch (@typeInfo(T)) {
                // std.math.pow can handle ints via powi, but it turns any errors to unreachable
                // we want to catch overflow and report a proper error to the user
                .int => {
                    if (T == i128 or T == u128) {
                        // Use custom powi that avoids compiler_rt calls
                        if (powi128(T, base, exp)) |value| {
                            return value;
                        } else |err| switch (err) {
                            error.Overflow => {
                                roc_ops.crash("Integer raised to power overflowed!");
                            },
                            error.Underflow => return 0,
                        }
                    } else {
                        if (std.math.powi(T, base, exp)) |value| {
                            return value;
                        } else |err| switch (err) {
                            error.Overflow => {
                                roc_ops.crash("Integer raised to power overflowed!");
                            },
                            error.Underflow => return 0,
                        }
                    }
                },
                else => {
                    return std.math.pow(T, base, exp);
                },
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Integer power for i128/u128 that avoids compiler_rt calls.
/// Uses our custom mul_i128/mul_u128_lo instead of native * and @mulWithOverflow.
fn powi128(comptime T: type, base: T, exp: T) error{ Overflow, Underflow }!T {
    const info = @typeInfo(T).int;
    if (info.signedness == .signed and exp < 0) {
        // Negative exponent: result is 1/base^|exp|, which is 0 for |base|>1
        if (base == 1) return 1;
        if (base == -1) return if (@as(u1, @truncate(@as(u128, @bitCast(exp)))) == 0) @as(T, 1) else @as(T, -1);
        return error.Underflow;
    }

    if (base == 0) {
        if (exp == 0) return 1;
        return 0;
    }

    if (info.signedness == .signed) {
        const is_negative_result = base < 0 and (@as(u1, @truncate(@as(u128, @bitCast(@as(i128, exp))))) != 0);
        const magnitude_limit = if (is_negative_result)
            @as(u128, 1) << 127
        else
            @as(u128, @intCast(std.math.maxInt(i128)));
        var b: u128 = @abs(base);
        var e: u128 = @intCast(exp);
        var result: u128 = 1;

        while (e > 0) {
            if (e & 1 != 0) {
                result = mul_u128_with_limit(result, b, magnitude_limit) orelse return error.Overflow;
            }
            e = i128h.shr(e, 1);
            if (e > 0) {
                b = mul_u128_with_limit(b, b, magnitude_limit) orelse return error.Overflow;
            }
        }

        if (is_negative_result) {
            if (result == (@as(u128, 1) << 127)) return std.math.minInt(i128);
            return -@as(T, @intCast(result));
        }
        return @intCast(result);
    }

    var b: u128 = base;
    var e: u128 = exp;
    var result: u128 = 1;

    while (e > 0) {
        if (e & 1 != 0) {
            result = mul_u128_checked(result, b) orelse return error.Overflow;
        }
        e = i128h.shr(e, 1);
        if (e > 0) {
            b = mul_u128_checked(b, b) orelse return error.Overflow;
        }
    }

    return result;
}

fn mul_u128_checked(lhs: u128, rhs: u128) ?u128 {
    const product = mul_u128(lhs, rhs);
    return if (product.hi == 0) product.lo else null;
}

fn mul_u128_with_limit(lhs: u128, rhs: u128, limit: u128) ?u128 {
    const product = mul_u128(lhs, rhs);
    if (product.hi != 0 or product.lo > limit) return null;
    return product.lo;
}

/// Check if a value is NaN.
pub fn exportIsNan(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) bool {
            return std.math.isNan(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Check if a value is infinite.
pub fn exportIsInfinite(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) bool {
            return std.math.isInf(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Check if a value is finite.
pub fn exportIsFinite(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) bool {
            return std.math.isFinite(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute arcsine using zig std.math.
pub fn exportAsin(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return std.math.asin(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute arccosine using zig std.math.
pub fn exportAcos(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return std.math.acos(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute arctangent using zig std.math.
pub fn exportAtan(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return std.math.atan(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute sine using zig std.math.
pub fn exportSin(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return math.sin(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute cosine using zig std.math.
pub fn exportCos(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return math.cos(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute tangent using zig std.math.
pub fn exportTan(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return math.tan(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute natural logarithm using zig @log builtin.
pub fn exportLog(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return @log(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute absolute value using zig @abs builtin.
pub fn exportFAbs(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return @abs(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute square root using zig std.math.
pub fn exportSqrt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.c) T {
            return math.sqrt(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float to the nearest integer using zig std.math.
pub fn exportRound(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.c) T {
            return @as(T, @round(input));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float down to the nearest integer using zig std.math.
pub fn exportFloor(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.c) T {
            return @as(T, @floor(input));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float up to the nearest integer using zig std.math.
pub fn exportCeiling(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.c) T {
            return @as(T, @ceil(input));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Integer division with ceiling using zig std.math.
pub fn exportDivCeil(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            a: T,
            b: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return math.divCeil(T, a, b) catch {
                roc_ops.crash("Integer division by 0!");
            };
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Integer division truncating towards zero.
/// For signed integers, this rounds towards zero (not negative infinity).
pub fn exportDivTrunc(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            a: T,
            b: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            if (b == 0) {
                roc_ops.crash("Integer division by 0!");
            }
            if (T == i128) return i128h.divTrunc_i128(a, b);
            if (T == u128) return i128h.divTrunc_u128(a, b);
            return @divTrunc(a, b);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Integer remainder after truncating division.
/// The result has the same sign as the dividend.
pub fn exportRemTrunc(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            a: T,
            b: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            if (b == 0) {
                roc_ops.crash("Integer remainder by 0!");
            }
            if (T == i128) return i128h.rem_i128(a, b);
            if (T == u128) return i128h.rem_u128(a, b);
            return @rem(a, b);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// i128 division truncating towards zero - callable from generated code.
pub fn divTruncI128(a: i128, b: i128, roc_ops: *RocOps) callconv(.c) i128 {
    if (b == 0) {
        roc_ops.crash("Integer division by 0!");
    }
    return i128h.divTrunc_i128(a, b);
}

/// u128 division truncating towards zero - callable from generated code.
pub fn divTruncU128(a: u128, b: u128, roc_ops: *RocOps) callconv(.c) u128 {
    if (b == 0) {
        roc_ops.crash("Integer division by 0!");
    }
    return i128h.divTrunc_u128(a, b);
}

/// i128 remainder after truncating division - callable from generated code.
pub fn remTruncI128(a: i128, b: i128, roc_ops: *RocOps) callconv(.c) i128 {
    if (b == 0) {
        roc_ops.crash("Integer remainder by 0!");
    }
    return i128h.rem_i128(a, b);
}

/// u128 remainder after truncating division - callable from generated code.
pub fn remTruncU128(a: u128, b: u128, roc_ops: *RocOps) callconv(.c) u128 {
    if (b == 0) {
        roc_ops.crash("Integer remainder by 0!");
    }
    return i128h.rem_u128(a, b);
}

/// Result type for checked integer conversions.
pub fn ToIntCheckedResult(comptime T: type) type {
    // On the Roc side we sort by alignment; putting the errorcode last
    // always works out (no number with smaller alignment than 1).
    return extern struct {
        value: T,
        out_of_bounds: bool,
    };
}

/// Exports a function to convert to integer, checking only max bound.
pub fn exportToIntCheckingMax(comptime From: type, comptime To: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: From) callconv(.c) ToIntCheckedResult(To) {
            if (input > std.math.maxInt(To)) {
                return .{ .out_of_bounds = true, .value = 0 };
            }
            return .{ .out_of_bounds = false, .value = @as(To, @intCast(input)) };
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(From), .linkage = .strong });
}

/// Exports a function to convert to integer, checking both bounds.
pub fn exportToIntCheckingMaxAndMin(comptime From: type, comptime To: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: From) callconv(.c) ToIntCheckedResult(To) {
            if (input > std.math.maxInt(To) or input < std.math.minInt(To)) {
                return .{ .out_of_bounds = true, .value = 0 };
            }
            return .{ .out_of_bounds = false, .value = @as(To, @intCast(input)) };
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(From), .linkage = .strong });
}

/// Returns true if lhs is a multiple of rhs.
pub fn isMultipleOf(comptime T: type, lhs: T, rhs: T) bool {
    if (rhs == 0 or rhs == -1) {
        // lhs is a multiple of rhs iff
        //
        // - rhs == -1
        // - rhs == 0 and lhs == 0
        //
        // Note: lhs % 0 is a runtime panic, so we can't use @mod.
        return (rhs == -1) or (lhs == 0);
    } else {
        const rem = if (T == i128) i128h.mod_i128(lhs, rhs) else @mod(lhs, rhs);
        return rem == 0;
    }
}

/// Exports a function to check if a value is a multiple of another.
pub fn exportIsMultipleOf(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(lhs: T, rhs: T) callconv(.c) bool {
            return @call(.always_inline, isMultipleOf, .{ T, lhs, rhs });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Adds two numbers, returning result and overflow flag.
pub fn addWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    switch (@typeInfo(T)) {
        .int => {
            const answer = @addWithOverflow(self, other);
            return .{ .value = answer[0], .has_overflowed = answer[1] == 1 };
        },
        else => {
            const answer = self + other;
            const overflowed = !std.math.isFinite(answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
    }
}

/// Exports a function to add two numbers, returning overflow info.
pub fn exportAddWithOverflow(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) WithOverflow(T) {
            return @call(.always_inline, addWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to add two integers, saturating on overflow.
pub fn exportAddSaturatedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) T {
            const result = addWithOverflow(T, self, other);
            if (result.has_overflowed) {
                // We can unambiguously tell which way it wrapped, because we have N+1 bits including the overflow bit
                if (result.value >= 0 and @typeInfo(T).int.signedness == .signed) {
                    return std.math.minInt(T);
                } else {
                    return std.math.maxInt(T);
                }
            } else {
                return result.value;
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to add two integers, wrapping on overflow.
pub fn exportAddWrappedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) T {
            return self +% other;
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to add two numbers, panicking on overflow.
pub fn exportAddOrPanic(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            self: T,
            other: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            const result = addWithOverflow(T, self, other);
            if (result.has_overflowed) {
                roc_ops.crash("Integer addition overflowed!");
            } else {
                return result.value;
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Subtracts two numbers, returning result and overflow flag.
pub fn subWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    switch (@typeInfo(T)) {
        .int => {
            const answer = @subWithOverflow(self, other);
            return .{ .value = answer[0], .has_overflowed = answer[1] == 1 };
        },
        else => {
            const answer = self - other;
            const overflowed = !std.math.isFinite(answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
    }
}

/// Exports a function to subtract two numbers, returning overflow info.
pub fn exportSubWithOverflow(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) WithOverflow(T) {
            return @call(.always_inline, subWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to subtract two integers, saturating on overflow.
pub fn exportSubSaturatedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) T {
            const result = subWithOverflow(T, self, other);
            if (result.has_overflowed) {
                if (@typeInfo(T).int.signedness == .unsigned) {
                    return 0;
                } else if (self < 0) {
                    return std.math.minInt(T);
                } else {
                    return std.math.maxInt(T);
                }
            } else {
                return result.value;
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to subtract two integers, wrapping on overflow.
pub fn exportSubWrappedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) T {
            return self -% other;
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to subtract two numbers, panicking on overflow.
pub fn exportSubOrPanic(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            self: T,
            other: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            const result = subWithOverflow(T, self, other);
            if (result.has_overflowed) {
                roc_ops.crash("Integer subtraction overflowed!");
            } else {
                return result.value;
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Multiplies two numbers, returning result and overflow flag.
pub fn mulWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    switch (@typeInfo(T)) {
        .int => {
            if (T == i128) {
                const is_answer_negative = (self < 0) != (other < 0);
                const max = std.math.maxInt(i128);
                const min = std.math.minInt(i128);

                const self_u128 = @abs(self);
                if (self_u128 > @as(u128, @intCast(std.math.maxInt(i128)))) {
                    if (other == 0) {
                        return .{ .value = 0, .has_overflowed = false };
                    } else if (other == 1) {
                        return .{ .value = self, .has_overflowed = false };
                    } else if (is_answer_negative) {
                        return .{ .value = min, .has_overflowed = true };
                    } else {
                        return .{ .value = max, .has_overflowed = true };
                    }
                }

                const other_u128 = @abs(other);
                if (other_u128 > @as(u128, @intCast(std.math.maxInt(i128)))) {
                    if (self == 0) {
                        return .{ .value = 0, .has_overflowed = false };
                    } else if (self == 1) {
                        return .{ .value = other, .has_overflowed = false };
                    } else if (is_answer_negative) {
                        return .{ .value = min, .has_overflowed = true };
                    } else {
                        return .{ .value = max, .has_overflowed = true };
                    }
                }

                const answer256: U256 = mul_u128(self_u128, other_u128);

                if (is_answer_negative) {
                    if (answer256.hi != 0 or answer256.lo > (1 << 127)) {
                        return .{ .value = min, .has_overflowed = true };
                    } else if (answer256.lo == (1 << 127)) {
                        return .{ .value = min, .has_overflowed = false };
                    } else {
                        return .{ .value = -@as(i128, @intCast(answer256.lo)), .has_overflowed = false };
                    }
                } else {
                    if (answer256.hi != 0 or answer256.lo > @as(u128, @intCast(max))) {
                        return .{ .value = max, .has_overflowed = true };
                    } else {
                        return .{ .value = @as(i128, @intCast(answer256.lo)), .has_overflowed = false };
                    }
                }
            } else if (T == u128) {
                const answer256: U256 = mul_u128(self, other);
                if (answer256.hi != 0) {
                    return .{ .value = std.math.maxInt(u128), .has_overflowed = true };
                } else {
                    return .{ .value = answer256.lo, .has_overflowed = false };
                }
            } else {
                const answer = @mulWithOverflow(self, other);
                return .{ .value = answer[0], .has_overflowed = answer[1] == 1 };
            }
        },
        else => {
            const answer = self * other;
            const overflowed = !std.math.isFinite(answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
    }
}

/// Exports a function to multiply two numbers, returning overflow info.
pub fn exportMulWithOverflow(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) WithOverflow(T) {
            return @call(.always_inline, mulWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to multiply two integers, saturating on overflow.
pub fn exportMulSaturatedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) T {
            const result = @call(.always_inline, mulWithOverflow, .{ T, self, other });
            return result.value;
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to multiply two integers, wrapping on overflow.
pub fn exportMulWrappedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.c) T {
            if (T == i128) return i128h.mul_i128(self, other);
            if (T == u128) return i128h.mul_u128_lo(self, other);
            return self *% other;
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Shifts an i128 right with zero fill.
pub fn shiftRightZeroFillI128(self: i128, other: u8) callconv(.c) i128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        // Zero-fill right shift on a signed value: cast to unsigned, shift, cast back.
        return @bitCast(i128h.shr(@as(u128, @bitCast(self)), @as(u7, @intCast(other))));
    }
}

/// Shifts a u128 right with zero fill.
pub fn shiftRightZeroFillU128(self: u128, other: u8) callconv(.c) u128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        return i128h.shr(self, @as(u7, @intCast(other)));
    }
}

/// Compares two i128 values, returning ordering.
pub fn compareI128(self: i128, other: i128) callconv(.c) Ordering {
    if (self == other) {
        return Ordering.EQ;
    } else if (self < other) {
        return Ordering.LT;
    } else {
        return Ordering.GT;
    }
}

/// Compares two u128 values, returning ordering.
pub fn compareU128(self: u128, other: u128) callconv(.c) Ordering {
    if (self == other) {
        return Ordering.EQ;
    } else if (self < other) {
        return Ordering.LT;
    } else {
        return Ordering.GT;
    }
}

/// Returns true if self < other for i128.
pub fn lessThanI128(self: i128, other: i128) callconv(.c) bool {
    return self < other;
}

/// Returns true if self <= other for i128.
pub fn lessThanOrEqualI128(self: i128, other: i128) callconv(.c) bool {
    return self <= other;
}

/// Returns true if self > other for i128.
pub fn greaterThanI128(self: i128, other: i128) callconv(.c) bool {
    return self > other;
}

/// Returns true if self >= other for i128.
pub fn greaterThanOrEqualI128(self: i128, other: i128) callconv(.c) bool {
    return self >= other;
}

/// Returns true if self < other for u128.
pub fn lessThanU128(self: u128, other: u128) callconv(.c) bool {
    return self < other;
}

/// Returns true if self <= other for u128.
pub fn lessThanOrEqualU128(self: u128, other: u128) callconv(.c) bool {
    return self <= other;
}

/// Returns true if self > other for u128.
pub fn greaterThanU128(self: u128, other: u128) callconv(.c) bool {
    return self > other;
}

/// Returns true if self >= other for u128.
pub fn greaterThanOrEqualU128(self: u128, other: u128) callconv(.c) bool {
    return self >= other;
}

/// Exports a function to multiply two numbers, panicking on overflow.
pub fn exportMulOrPanic(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            self: T,
            other: T,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            const result = @call(.always_inline, mulWithOverflow, .{ T, self, other });
            if (result.has_overflowed) {
                roc_ops.crash("Integer multiplication overflowed!");
            } else {
                return result.value;
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to count leading zero bits.
pub fn exportCountLeadingZeroBits(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T) callconv(.c) u8 {
            return @as(u8, @clz(self));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to count trailing zero bits.
pub fn exportCountTrailingZeroBits(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T) callconv(.c) u8 {
            return @as(u8, @ctz(self));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to count one bits (population count).
pub fn exportCountOneBits(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T) callconv(.c) u8 {
            return @as(u8, @popCount(self));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Returns the bitwise parts of an f32.
pub fn f32ToParts(self: f32) callconv(.c) F32Parts {
    const u32Value = @as(u32, @bitCast(self));
    return F32Parts{
        .fraction = u32Value & 0x7fffff,
        .exponent = @truncate(u32Value >> 23 & 0xff),
        .sign = u32Value >> 31 & 1 == 1,
    };
}

/// Returns the bitwise parts of an f64.
pub fn f64ToParts(self: f64) callconv(.c) F64Parts {
    const u64Value = @as(u64, @bitCast(self));
    return F64Parts{
        .fraction = u64Value & 0xfffffffffffff,
        .exponent = @truncate(u64Value >> 52 & 0x7ff),
        .sign = u64Value >> 63 & 1 == 1,
    };
}

/// Constructs an f32 from its bitwise parts.
pub fn f32FromParts(parts: F32Parts) callconv(.c) f32 {
    return @as(f32, @bitCast(parts.fraction & 0x7fffff | (@as(u32, parts.exponent) << 23) | (@as(u32, @intFromBool(parts.sign)) << 31)));
}

/// Constructs an f64 from its bitwise parts.
pub fn f64FromParts(parts: F64Parts) callconv(.c) f64 {
    return @as(f64, @bitCast(parts.fraction & 0xfffffffffffff | (@as(u64, parts.exponent & 0x7ff) << 52) | (@as(u64, @intFromBool(parts.sign)) << 63)));
}

/// Returns the bit pattern of an f32 as u32.
pub fn f32ToBits(self: f32) callconv(.c) u32 {
    return @as(u32, @bitCast(self));
}

/// Returns the bit pattern of an f64 as u64.
pub fn f64ToBits(self: f64) callconv(.c) u64 {
    return @as(u64, @bitCast(self));
}

/// Returns the bit pattern of an i128 as u128.
pub fn i128ToBits(self: i128) callconv(.c) u128 {
    return @as(u128, @bitCast(self));
}

/// Constructs an f32 from its bit pattern.
pub fn f32FromBits(bits: u32) callconv(.c) f32 {
    return @as(f32, @bitCast(bits));
}

/// Constructs an f64 from its bit pattern.
pub fn f64FromBits(bits: u64) callconv(.c) f64 {
    return @as(f64, @bitCast(bits));
}

/// Constructs an i128 from its bit pattern.
pub fn i128FromBits(bits: u128) callconv(.c) i128 {
    return @as(i128, @bitCast(bits));
}

fn signedMinText(comptime T: type) []const u8 {
    return switch (T) {
        i8 => "-128",
        i16 => "-32768",
        i32 => "-2147483648",
        i64 => "-9223372036854775808",
        i128 => "-170141183460469231731687303715884105728",
        else => @compileError("unsupported signed integer type"),
    };
}

fn signedMaxText(comptime T: type) []const u8 {
    return switch (T) {
        i8 => "127",
        i16 => "32767",
        i32 => "2147483647",
        i64 => "9223372036854775807",
        i128 => "170141183460469231731687303715884105727",
        else => @compileError("unsupported signed integer type"),
    };
}

fn signedMaxPlusOneText(comptime T: type) []const u8 {
    return switch (T) {
        i8 => "128",
        i16 => "32768",
        i32 => "2147483648",
        i64 => "9223372036854775808",
        i128 => "170141183460469231731687303715884105728",
        else => @compileError("unsupported signed integer type"),
    };
}

fn signedMinMinusOneText(comptime T: type) []const u8 {
    return switch (T) {
        i8 => "-129",
        i16 => "-32769",
        i32 => "-2147483649",
        i64 => "-9223372036854775809",
        i128 => "-170141183460469231731687303715884105729",
        else => @compileError("unsupported signed integer type"),
    };
}

fn unsignedMaxText(comptime T: type) []const u8 {
    return switch (T) {
        u8 => "255",
        u16 => "65535",
        u32 => "4294967295",
        u64 => "18446744073709551615",
        u128 => "340282366920938463463374607431768211455",
        else => @compileError("unsupported unsigned integer type"),
    };
}

fn unsignedMaxPlusOneText(comptime T: type) []const u8 {
    return switch (T) {
        u8 => "256",
        u16 => "65536",
        u32 => "4294967296",
        u64 => "18446744073709551616",
        u128 => "340282366920938463463374607431768211456",
        else => @compileError("unsupported unsigned integer type"),
    };
}

const NumTestHelperError = error{
    TestExpectedEqual,
};

fn expectParseIntText(comptime T: type, text: []const u8, expected: T, roc_ops: *RocOps) NumTestHelperError!void {
    const roc_str = @import("str.zig").RocStr.fromSlice(text, roc_ops);
    defer roc_str.decref(roc_ops);

    const result = parseIntFromStr(T, roc_str);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);
    try std.testing.expectEqual(expected, result.value);
}

fn expectParseIntReject(comptime T: type, text: []const u8, roc_ops: *RocOps) NumTestHelperError!void {
    const roc_str = @import("str.zig").RocStr.fromSlice(text, roc_ops);
    defer roc_str.decref(roc_ops);

    const result = parseIntFromStr(T, roc_str);
    try std.testing.expectEqual(@as(u8, 1), result.errorcode);
    try std.testing.expectEqual(@as(T, 0), result.value);
}

fn expectAddWithOverflowOracle(comptime T: type, lhs: T, rhs: T) NumTestHelperError!void {
    const expected = @addWithOverflow(lhs, rhs);
    const actual = addWithOverflow(T, lhs, rhs);
    try std.testing.expectEqual(expected[0], actual.value);
    try std.testing.expectEqual(expected[1] == 1, actual.has_overflowed);
}

fn expectSubWithOverflowOracle(comptime T: type, lhs: T, rhs: T) NumTestHelperError!void {
    const expected = @subWithOverflow(lhs, rhs);
    const actual = subWithOverflow(T, lhs, rhs);
    try std.testing.expectEqual(expected[0], actual.value);
    try std.testing.expectEqual(expected[1] == 1, actual.has_overflowed);
}

fn expectMulWithOverflowOracle(comptime T: type, lhs: T, rhs: T) NumTestHelperError!void {
    const expected = @mulWithOverflow(lhs, rhs);
    const actual = mulWithOverflow(T, lhs, rhs);
    try std.testing.expectEqual(expected[0], actual.value);
    try std.testing.expectEqual(expected[1] == 1, actual.has_overflowed);
}

fn expectParseFloatBits(comptime T: type, text: []const u8, expected_bits: std.meta.Int(.unsigned, @bitSizeOf(T)), roc_ops: *RocOps) NumTestHelperError!void {
    const roc_str = @import("str.zig").RocStr.fromSlice(text, roc_ops);
    defer roc_str.decref(roc_ops);

    const result = parseFloatFromStr(T, roc_str);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);

    const Bits = std.meta.Int(.unsigned, @bitSizeOf(T));
    try std.testing.expectEqual(@as(Bits, expected_bits), @as(Bits, @bitCast(result.value)));
}

test "parseIntFromStr accepts and rejects exact integer width boundaries" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (.{ i8, i16, i32, i64, i128 }) |T| {
        try expectParseIntText(T, signedMinText(T), std.math.minInt(T), test_env.getOps());
        try expectParseIntText(T, signedMaxText(T), std.math.maxInt(T), test_env.getOps());
        try expectParseIntReject(T, signedMinMinusOneText(T), test_env.getOps());
        try expectParseIntReject(T, signedMaxPlusOneText(T), test_env.getOps());
    }

    inline for (.{ u8, u16, u32, u64, u128 }) |T| {
        try expectParseIntText(T, "0", 0, test_env.getOps());
        try expectParseIntText(T, unsignedMaxText(T), std.math.maxInt(T), test_env.getOps());
        try expectParseIntText(T, "-0", 0, test_env.getOps());
        try expectParseIntReject(T, "-1", test_env.getOps());
        try expectParseIntReject(T, unsignedMaxPlusOneText(T), test_env.getOps());
    }
}

test "parseIntFromStr validates radix prefixes and underscores at boundaries" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    try expectParseIntText(u8, "0xff", 255, test_env.getOps());
    try expectParseIntReject(u8, "0x100", test_env.getOps());
    try expectParseIntText(i8, "-0b10000000", -128, test_env.getOps());
    try expectParseIntReject(i8, "0b10000000", test_env.getOps());
    try expectParseIntText(u16, "0o17_7777", 65535, test_env.getOps());
    try expectParseIntReject(u16, "0o20_0000", test_env.getOps());
    try expectParseIntText(i32, "2_147_483_647", std.math.maxInt(i32), test_env.getOps());
    try expectParseIntReject(i32, "_1", test_env.getOps());
    try expectParseIntReject(i32, "1__0", test_env.getOps());
    try expectParseIntReject(i32, "10_", test_env.getOps());
}

test "integer overflow helpers match Zig overflow intrinsics across widths" {
    inline for (.{ i8, i16, i32, i64, i128 }) |T| {
        try expectAddWithOverflowOracle(T, std.math.maxInt(T), 1);
        try expectAddWithOverflowOracle(T, std.math.minInt(T), -1);
        try expectAddWithOverflowOracle(T, std.math.maxInt(T) - 1, 1);
        try expectSubWithOverflowOracle(T, std.math.minInt(T), 1);
        try expectSubWithOverflowOracle(T, std.math.maxInt(T), -1);
        try expectSubWithOverflowOracle(T, std.math.minInt(T) + 1, 1);
    }

    inline for (.{ i8, i16, i32, i64 }) |T| {
        try expectMulWithOverflowOracle(T, std.math.maxInt(T), 2);
        try expectMulWithOverflowOracle(T, std.math.minInt(T), -1);
        try expectMulWithOverflowOracle(T, std.math.maxInt(T), 1);
    }

    inline for (.{ u8, u16, u32, u64, u128 }) |T| {
        try expectAddWithOverflowOracle(T, std.math.maxInt(T), 1);
        try expectAddWithOverflowOracle(T, std.math.maxInt(T) - 1, 1);
        try expectSubWithOverflowOracle(T, 0, 1);
        try expectSubWithOverflowOracle(T, 1, 1);
    }

    inline for (.{ u8, u16, u32, u64 }) |T| {
        try expectMulWithOverflowOracle(T, std.math.maxInt(T), 2);
        try expectMulWithOverflowOracle(T, std.math.maxInt(T), 1);
    }

    const i128_positive_overflow = mulWithOverflow(i128, std.math.maxInt(i128), 2);
    try std.testing.expectEqual(std.math.maxInt(i128), i128_positive_overflow.value);
    try std.testing.expect(i128_positive_overflow.has_overflowed);

    const i128_negative_overflow = mulWithOverflow(i128, std.math.minInt(i128), -1);
    try std.testing.expectEqual(std.math.maxInt(i128), i128_negative_overflow.value);
    try std.testing.expect(i128_negative_overflow.has_overflowed);

    const i128_no_overflow = mulWithOverflow(i128, std.math.maxInt(i128), 1);
    try std.testing.expectEqual(std.math.maxInt(i128), i128_no_overflow.value);
    try std.testing.expect(!i128_no_overflow.has_overflowed);

    const u128_overflow = mulWithOverflow(u128, std.math.maxInt(u128), 2);
    try std.testing.expectEqual(std.math.maxInt(u128), u128_overflow.value);
    try std.testing.expect(u128_overflow.has_overflowed);

    const u128_no_overflow = mulWithOverflow(u128, std.math.maxInt(u128), 1);
    try std.testing.expectEqual(std.math.maxInt(u128), u128_no_overflow.value);
    try std.testing.expect(!u128_no_overflow.has_overflowed);
}

test "powi128 handles signed magnitude and overflow boundaries" {
    try std.testing.expectEqual(@as(i128, -128), try powi128(i128, -2, 7));
    try std.testing.expectEqual(@as(i128, 256), try powi128(i128, -2, 8));
    try std.testing.expectEqual(@as(i128, -1), try powi128(i128, -1, 3));
    try std.testing.expectEqual(@as(i128, 1), try powi128(i128, -1, 4));
    try std.testing.expectEqual(@as(i128, std.math.minInt(i128)), try powi128(i128, std.math.minInt(i128), 1));
    try std.testing.expectError(error.Overflow, powi128(i128, std.math.minInt(i128), 2));
    try std.testing.expectError(error.Overflow, powi128(i128, std.math.maxInt(i128), 2));
    try std.testing.expectError(error.Underflow, powi128(i128, 2, -1));

    try std.testing.expectEqual(@as(u128, 1024), try powi128(u128, 2, 10));
    try std.testing.expectError(error.Overflow, powi128(u128, std.math.maxInt(u128), 2));
}

test "parseFloatFromStr matches IEEE bit fixtures for finite edge cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (&[_]struct { text: []const u8, bits: u32 }{
        .{ .text = "0", .bits = 0x00000000 },
        .{ .text = "-0", .bits = 0x80000000 },
        .{ .text = "0.1", .bits = 0x3dcccccd },
        .{ .text = "1.40129846e-45", .bits = 0x00000001 },
        .{ .text = "1.17549435e-38", .bits = 0x00800000 },
        .{ .text = "3.4028235e38", .bits = 0x7f7fffff },
        .{ .text = "0x1.8p+1", .bits = 0x40400000 },
    }) |text| {
        try expectParseFloatBits(f32, text.text, text.bits, test_env.getOps());
    }

    inline for (&[_]struct { text: []const u8, bits: u64 }{
        .{ .text = "0", .bits = 0x0000000000000000 },
        .{ .text = "-0", .bits = 0x8000000000000000 },
        .{ .text = "0.1", .bits = 0x3fb999999999999a },
        .{ .text = "5e-324", .bits = 0x0000000000000001 },
        .{ .text = "2.2250738585072014e-308", .bits = 0x0010000000000000 },
        .{ .text = "1.7976931348623157e308", .bits = 0x7fefffffffffffff },
        .{ .text = "0x1.921fb54442d18p+1", .bits = 0x400921fb54442d18 },
    }) |text| {
        try expectParseFloatBits(f64, text.text, text.bits, test_env.getOps());
    }
}

test "parseIntFromStr decimal parsing" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test successful decimal parsing
    const valid_str = @import("str.zig").RocStr.fromSlice("42", test_env.getOps());
    defer valid_str.decref(test_env.getOps());

    const result = parseIntFromStr(i32, valid_str);
    try std.testing.expectEqual(@as(i32, 42), result.value);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);

    // Test negative number
    const neg_str = @import("str.zig").RocStr.fromSlice("-123", test_env.getOps());
    defer neg_str.decref(test_env.getOps());

    const neg_result = parseIntFromStr(i32, neg_str);
    try std.testing.expectEqual(@as(i32, -123), neg_result.value);
    try std.testing.expectEqual(@as(u8, 0), neg_result.errorcode);

    // Test zero
    const zero_str = @import("str.zig").RocStr.fromSlice("0", test_env.getOps());
    defer zero_str.decref(test_env.getOps());

    const zero_result = parseIntFromStr(i32, zero_str);
    try std.testing.expectEqual(@as(i32, 0), zero_result.value);
    try std.testing.expectEqual(@as(u8, 0), zero_result.errorcode);
}

test "parseIntFromStr hex and binary parsing" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test hexadecimal parsing
    const hex_str = @import("str.zig").RocStr.fromSlice("0xFF", test_env.getOps());
    defer hex_str.decref(test_env.getOps());

    const hex_result = parseIntFromStr(i32, hex_str);
    try std.testing.expectEqual(@as(i32, 255), hex_result.value);
    try std.testing.expectEqual(@as(u8, 0), hex_result.errorcode);

    // Test binary parsing
    const bin_str = @import("str.zig").RocStr.fromSlice("0b1010", test_env.getOps());
    defer bin_str.decref(test_env.getOps());

    const bin_result = parseIntFromStr(i32, bin_str);
    try std.testing.expectEqual(@as(i32, 10), bin_result.value);
    try std.testing.expectEqual(@as(u8, 0), bin_result.errorcode);

    // Test octal parsing
    const oct_str = @import("str.zig").RocStr.fromSlice("0o755", test_env.getOps());
    defer oct_str.decref(test_env.getOps());

    const oct_result = parseIntFromStr(i32, oct_str);
    try std.testing.expectEqual(@as(i32, 493), oct_result.value); // 7*64 + 5*8 + 5 = 493
    try std.testing.expectEqual(@as(u8, 0), oct_result.errorcode);
}

test "parseIntFromStr error cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test invalid string
    const invalid_str = @import("str.zig").RocStr.fromSlice("not_a_number", test_env.getOps());
    defer invalid_str.decref(test_env.getOps());

    const invalid_result = parseIntFromStr(i32, invalid_str);
    try std.testing.expectEqual(@as(i32, 0), invalid_result.value);
    try std.testing.expectEqual(@as(u8, 1), invalid_result.errorcode);

    // Test empty string
    const empty_str = @import("str.zig").RocStr.fromSlice("", test_env.getOps());
    defer empty_str.decref(test_env.getOps());

    const empty_result = parseIntFromStr(i32, empty_str);
    try std.testing.expectEqual(@as(i32, 0), empty_result.value);
    try std.testing.expectEqual(@as(u8, 1), empty_result.errorcode);

    // Test overflow (for i8)
    const overflow_str = @import("str.zig").RocStr.fromSlice("1000", test_env.getOps());
    defer overflow_str.decref(test_env.getOps());

    const overflow_result = parseIntFromStr(i8, overflow_str);
    try std.testing.expectEqual(@as(i8, 0), overflow_result.value);
    try std.testing.expectEqual(@as(u8, 1), overflow_result.errorcode);
}

test "parseFloatFromStr basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test successful float parsing
    const valid_str = @import("str.zig").RocStr.fromSlice("3.14159", test_env.getOps());
    defer valid_str.decref(test_env.getOps());

    const result = parseFloatFromStr(f32, valid_str);
    try std.testing.expectApproxEqRel(@as(f32, 3.14159), result.value, 0.00001);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);

    // Test negative float
    const neg_str = @import("str.zig").RocStr.fromSlice("-42.5", test_env.getOps());
    defer neg_str.decref(test_env.getOps());

    const neg_result = parseFloatFromStr(f64, neg_str);
    try std.testing.expectEqual(@as(f64, -42.5), neg_result.value);
    try std.testing.expectEqual(@as(u8, 0), neg_result.errorcode);

    // Test scientific notation
    const sci_str = @import("str.zig").RocStr.fromSlice("1.5e2", test_env.getOps());
    defer sci_str.decref(test_env.getOps());

    const sci_result = parseFloatFromStr(f32, sci_str);
    try std.testing.expectEqual(@as(f32, 150.0), sci_result.value);
    try std.testing.expectEqual(@as(u8, 0), sci_result.errorcode);
}

test "parseFloatFromStr error cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test invalid string
    const invalid_str = @import("str.zig").RocStr.fromSlice("not_a_float", test_env.getOps());
    defer invalid_str.decref(test_env.getOps());

    const invalid_result = parseFloatFromStr(f32, invalid_str);
    try std.testing.expectEqual(@as(f32, 0.0), invalid_result.value);
    try std.testing.expectEqual(@as(u8, 1), invalid_result.errorcode);

    // Test empty string
    const empty_str = @import("str.zig").RocStr.fromSlice("", test_env.getOps());
    defer empty_str.decref(test_env.getOps());

    const empty_result = parseFloatFromStr(f32, empty_str);
    try std.testing.expectEqual(@as(f32, 0.0), empty_result.value);
    try std.testing.expectEqual(@as(u8, 1), empty_result.errorcode);

    // Test malformed decimal
    const malformed_str = @import("str.zig").RocStr.fromSlice("3.14.15", test_env.getOps());
    defer malformed_str.decref(test_env.getOps());

    const malformed_result = parseFloatFromStr(f32, malformed_str);
    try std.testing.expectEqual(@as(f32, 0.0), malformed_result.value);
    try std.testing.expectEqual(@as(u8, 1), malformed_result.errorcode);

    const overflow_str = @import("str.zig").RocStr.fromSlice("1e999", test_env.getOps());
    defer overflow_str.decref(test_env.getOps());

    const overflow_result = parseFloatFromStr(f64, overflow_str);
    try std.testing.expectEqual(@as(f64, 0.0), overflow_result.value);
    try std.testing.expectEqual(@as(u8, 1), overflow_result.errorcode);
}

test "parseFloatFromStr special values" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test infinity
    const inf_str = @import("str.zig").RocStr.fromSlice("inf", test_env.getOps());
    defer inf_str.decref(test_env.getOps());

    const inf_result = parseFloatFromStr(f32, inf_str);
    try std.testing.expect(std.math.isInf(inf_result.value));
    try std.testing.expectEqual(@as(u8, 0), inf_result.errorcode);

    // Test negative infinity
    const neg_inf_str = @import("str.zig").RocStr.fromSlice("-inf", test_env.getOps());
    defer neg_inf_str.decref(test_env.getOps());

    const neg_inf_result = parseFloatFromStr(f32, neg_inf_str);
    try std.testing.expect(std.math.isNegativeInf(neg_inf_result.value));
    try std.testing.expectEqual(@as(u8, 0), neg_inf_result.errorcode);

    // Test NaN
    const nan_str = @import("str.zig").RocStr.fromSlice("nan", test_env.getOps());
    defer nan_str.decref(test_env.getOps());

    const nan_result = parseFloatFromStr(f32, nan_str);
    try std.testing.expect(std.math.isNan(nan_result.value));
    try std.testing.expectEqual(@as(u8, 0), nan_result.errorcode);
}

test "addWithOverflow basic functionality" {
    // Test normal addition without overflow
    const result1 = addWithOverflow(i32, 10, 20);
    try std.testing.expectEqual(@as(i32, 30), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test addition that would overflow
    const result2 = addWithOverflow(i8, 127, 1);
    try std.testing.expectEqual(@as(i8, -128), result2.value); // wraps around
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "addWithOverflow with floating point" {
    // Test normal floating point addition
    const result1 = addWithOverflow(f32, 1.5, 2.5);
    try std.testing.expectEqual(@as(f32, 4.0), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test infinite result
    const result2 = addWithOverflow(f32, std.math.floatMax(f32), std.math.floatMax(f32));
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "subWithOverflow basic functionality" {
    // Test normal subtraction without overflow
    const result1 = subWithOverflow(i32, 30, 10);
    try std.testing.expectEqual(@as(i32, 20), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test subtraction that would underflow
    const result2 = subWithOverflow(i8, -128, 1);
    try std.testing.expectEqual(@as(i8, 127), result2.value); // wraps around
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "mulWithOverflow basic functionality" {
    // Test normal multiplication without overflow
    const result1 = mulWithOverflow(i32, 6, 7);
    try std.testing.expectEqual(@as(i32, 42), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test multiplication that would overflow
    const result2 = mulWithOverflow(i8, 100, 2);
    try std.testing.expectEqual(@as(i8, -56), result2.value); // wraps around
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "isMultipleOf functionality" {
    // Test basic multiples
    try std.testing.expect(isMultipleOf(i32, 10, 5));
    try std.testing.expect(isMultipleOf(i32, 15, 3));
    try std.testing.expect(!isMultipleOf(i32, 10, 3));

    // Test edge cases
    try std.testing.expect(isMultipleOf(i32, 0, 5)); // 0 is multiple of anything
    try std.testing.expect(isMultipleOf(i32, 5, -1)); // anything is multiple of -1
    try std.testing.expect(isMultipleOf(i32, 0, 0)); // 0 is multiple of 0
    try std.testing.expect(!isMultipleOf(i32, 5, 0)); // 5 is not multiple of 0
}

test "compareI128 functionality" {
    const a: i128 = 1000000000000000000;
    const b: i128 = 2000000000000000000;
    const c: i128 = 1000000000000000000;

    try std.testing.expectEqual(@import("utils.zig").Ordering.LT, compareI128(a, b));
    try std.testing.expectEqual(@import("utils.zig").Ordering.GT, compareI128(b, a));
    try std.testing.expectEqual(@import("utils.zig").Ordering.EQ, compareI128(a, c));
}

test "compareU128 functionality" {
    const a: u128 = 1000000000000000000;
    const b: u128 = 2000000000000000000;
    const c: u128 = 1000000000000000000;

    try std.testing.expectEqual(@import("utils.zig").Ordering.LT, compareU128(a, b));
    try std.testing.expectEqual(@import("utils.zig").Ordering.GT, compareU128(b, a));
    try std.testing.expectEqual(@import("utils.zig").Ordering.EQ, compareU128(a, c));
}

test "128-bit comparison functions" {
    const small: i128 = 100;
    const large: i128 = 200;

    try std.testing.expect(lessThanI128(small, large));
    try std.testing.expect(!lessThanI128(large, small));
    try std.testing.expect(lessThanOrEqualI128(small, large));
    try std.testing.expect(lessThanOrEqualI128(small, small));
    try std.testing.expect(greaterThanI128(large, small));
    try std.testing.expect(!greaterThanI128(small, large));
    try std.testing.expect(greaterThanOrEqualI128(large, small));
    try std.testing.expect(greaterThanOrEqualI128(small, small));
}

test "mul_u128 basic functionality" {
    const a: u128 = 1000000;
    const b: u128 = 2000000;
    const result = mul_u128(a, b);

    // 1000000 * 2000000 = 2000000000000, which fits in u128
    try std.testing.expectEqual(@as(u128, 0), result.hi);
    try std.testing.expectEqual(@as(u128, 2000000000000), result.lo);
}

test "f32ToParts and f32FromParts roundtrip" {
    const values = [_]f32{ 0.0, 1.0, -1.0, 3.14159, -42.5, std.math.inf(f32), -std.math.inf(f32) };

    for (values) |val| {
        if (!std.math.isNan(val)) { // Skip NaN since NaN != NaN
            const parts = f32ToParts(val);
            const reconstructed = f32FromParts(parts);
            try std.testing.expectEqual(val, reconstructed);
        }
    }
}

test "f64ToParts and f64FromParts roundtrip" {
    const values = [_]f64{ 0.0, 1.0, -1.0, 3.141592653589793, -42.5, std.math.inf(f64), -std.math.inf(f64) };

    for (values) |val| {
        if (!std.math.isNan(val)) { // Skip NaN since NaN != NaN
            const parts = f64ToParts(val);
            const reconstructed = f64FromParts(parts);
            try std.testing.expectEqual(val, reconstructed);
        }
    }
}

test "f32ToBits and f32FromBits roundtrip" {
    const values = [_]f32{ 0.0, 1.0, -1.0, 3.14159, -42.5 };

    for (values) |val| {
        const bits = f32ToBits(val);
        const reconstructed = f32FromBits(bits);
        try std.testing.expectEqual(val, reconstructed);
    }
}

test "f64ToBits and f64FromBits roundtrip" {
    const values = [_]f64{ 0.0, 1.0, -1.0, 3.141592653589793, -42.5 };

    for (values) |val| {
        const bits = f64ToBits(val);
        const reconstructed = f64FromBits(bits);
        try std.testing.expectEqual(val, reconstructed);
    }
}

test "f32ToParts specific values" {
    // Test zero
    const zero_parts = f32ToParts(0.0);
    try std.testing.expectEqual(@as(u32, 0), zero_parts.fraction);
    try std.testing.expectEqual(@as(u8, 0), zero_parts.exponent);
    try std.testing.expectEqual(false, zero_parts.sign);

    // Test negative zero
    const neg_zero_parts = f32ToParts(-0.0);
    try std.testing.expectEqual(@as(u32, 0), neg_zero_parts.fraction);
    try std.testing.expectEqual(@as(u8, 0), neg_zero_parts.exponent);
    try std.testing.expectEqual(true, neg_zero_parts.sign);

    // Test 1.0
    const one_parts = f32ToParts(1.0);
    try std.testing.expectEqual(@as(u32, 0), one_parts.fraction);
    try std.testing.expectEqual(@as(u8, 127), one_parts.exponent); // bias is 127
    try std.testing.expectEqual(false, one_parts.sign);
}

test "shiftRightZeroFillI128 basic functionality" {
    // Test normal shift
    const value: i128 = 0x1000;
    const result1 = shiftRightZeroFillI128(value, 4);
    try std.testing.expectEqual(@as(i128, 0x100), result1);

    // Test shift by 0
    const result2 = shiftRightZeroFillI128(value, 0);
    try std.testing.expectEqual(value, result2);

    // Test large shift (should return 0)
    const result3 = shiftRightZeroFillI128(value, 128);
    try std.testing.expectEqual(@as(i128, 0), result3);

    // Test negative value (zero-fill right shift clears the sign bit)
    const neg_value: i128 = -1;
    const result4 = shiftRightZeroFillI128(neg_value, 1);
    try std.testing.expectEqual(@as(i128, std.math.maxInt(i128)), result4);
}

test "shiftRightZeroFillU128 basic functionality" {
    // Test normal shift
    const value: u128 = 0x1000;
    const result1 = shiftRightZeroFillU128(value, 4);
    try std.testing.expectEqual(@as(u128, 0x100), result1);

    // Test shift by 0
    const result2 = shiftRightZeroFillU128(value, 0);
    try std.testing.expectEqual(value, result2);

    // Test large shift (should return 0)
    const result3 = shiftRightZeroFillU128(value, 128);
    try std.testing.expectEqual(@as(u128, 0), result3);

    // Test max value
    const max_value: u128 = std.math.maxInt(u128);
    const result4 = shiftRightZeroFillU128(max_value, 1);
    try std.testing.expectEqual(@as(u128, 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF), result4);
}

test "addWithOverflow edge cases" {
    // Test adding to max value
    const result1 = addWithOverflow(i8, std.math.maxInt(i8), 1);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result1.value);
    try std.testing.expectEqual(true, result1.has_overflowed);

    // Test adding zero (should never overflow)
    const result2 = addWithOverflow(i8, std.math.maxInt(i8), 0);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result2.value);
    try std.testing.expectEqual(false, result2.has_overflowed);

    // Test boundary case
    const result3 = addWithOverflow(i8, std.math.maxInt(i8) - 1, 1);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result3.value);
    try std.testing.expectEqual(false, result3.has_overflowed);
}

test "subWithOverflow edge cases" {
    // Test subtracting from min value
    const result1 = subWithOverflow(i8, std.math.minInt(i8), 1);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result1.value);
    try std.testing.expectEqual(true, result1.has_overflowed);

    // Test subtracting zero (should never overflow)
    const result2 = subWithOverflow(i8, std.math.minInt(i8), 0);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result2.value);
    try std.testing.expectEqual(false, result2.has_overflowed);

    // Test boundary case
    const result3 = subWithOverflow(i8, std.math.minInt(i8) + 1, 1);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result3.value);
    try std.testing.expectEqual(false, result3.has_overflowed);
}

test "mulWithOverflow edge cases" {
    // Test multiplying by zero (should never overflow)
    const result1 = mulWithOverflow(i8, std.math.maxInt(i8), 0);
    try std.testing.expectEqual(@as(i8, 0), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test multiplying by one (should never overflow)
    const result2 = mulWithOverflow(i8, std.math.maxInt(i8), 1);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result2.value);
    try std.testing.expectEqual(false, result2.has_overflowed);

    // Test multiplying by -1
    const result3 = mulWithOverflow(i8, std.math.maxInt(i8), -1);
    try std.testing.expectEqual(@as(i8, -std.math.maxInt(i8)), result3.value);
    try std.testing.expectEqual(false, result3.has_overflowed);

    // Test multiplying min value by -1 (should overflow)
    const result4 = mulWithOverflow(i8, std.math.minInt(i8), -1);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result4.value);
    try std.testing.expectEqual(true, result4.has_overflowed);
}

test "mul_u128 large values" {
    // Test multiplication that would overflow into high bits
    const large1: u128 = 0xFFFFFFFFFFFFFFFF; // max u64
    const large2: u128 = 0xFFFFFFFFFFFFFFFF;
    const result = mul_u128(large1, large2);

    // Verify the actual result values
    try std.testing.expectEqual(@as(u128, 0), result.hi);
    try std.testing.expectEqual(@as(u128, 0xfffffffffffffffe0000000000000001), result.lo);
}

test "mul_u128 overflow into high bits" {
    // Test multiplication that overflows into high bits
    const large1: u128 = 0x10000000000000000; // 2^64
    const large2: u128 = 0x10000000000000000; // 2^64
    const result = mul_u128(large1, large2);

    // 2^64 * 2^64 = 2^128, which should give hi = 1, lo = 0
    try std.testing.expectEqual(@as(u128, 1), result.hi);
    try std.testing.expectEqual(@as(u128, 0), result.lo);
}
