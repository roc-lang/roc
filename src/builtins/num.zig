//! Builtin numeric operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's numeric types and
//! operations, including integer and floating-point arithmetic, parsing,
//! overflow detection, and conversions. It defines numeric parsing utilities
//! and functions that are called from compiled Roc code to handle numeric
//! operations efficiently and safely.
const std = @import("std");

const WithOverflow = @import("utils.zig").WithOverflow;
const Ordering = @import("utils.zig").Ordering;
const RocList = @import("list.zig").RocList;
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
pub fn mul_u128(a: u128, b: u128) U256 {
    var hi: u128 = undefined;
    var lo: u128 = undefined;

    const bits_in_dword_2: u32 = 64;
    const lower_mask: u128 = math.maxInt(u128) >> bits_in_dword_2;

    lo = (a & lower_mask) * (b & lower_mask);

    var t = lo >> bits_in_dword_2;

    lo &= lower_mask;

    t += (a >> bits_in_dword_2) * (b & lower_mask);

    lo += (t & lower_mask) << bits_in_dword_2;

    hi = t >> bits_in_dword_2;

    t = lo >> bits_in_dword_2;

    lo &= lower_mask;

    t += (b >> bits_in_dword_2) * (a & lower_mask);

    lo += (t & lower_mask) << bits_in_dword_2;

    hi += t >> bits_in_dword_2;

    hi += (a >> bits_in_dword_2) * (b >> bits_in_dword_2);

    return .{ .hi = hi, .lo = lo };
}

/// Parses an integer from a RocStr
pub fn parseIntFromStr(comptime T: type, buf: RocStr) NumParseResult(T) {
    const radix = 0;
    if (std.fmt.parseInt(T, buf.asSlice(), radix)) |success| {
        return .{ .errorcode = 0, .value = success };
    } else |_| {
        return .{ .errorcode = 1, .value = 0 };
    }
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
    if (std.fmt.parseFloat(T, buf.asSlice())) |success| {
        return .{ .errorcode = 0, .value = success };
    } else |_| {
        return .{ .errorcode = 1, .value = 0 };
    }
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
            return @floatFromInt(x);
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
                    if (std.math.powi(T, base, exp)) |value| {
                        return value;
                    } else |err| switch (err) {
                        error.Overflow => {
                            roc_ops.crash("Integer raised to power overflowed!");
                        },
                        error.Underflow => return 0,
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
            return @as(T, @intFromFloat((math.round(input))));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float down to the nearest integer using zig std.math.
pub fn exportFloor(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.c) T {
            return @as(T, @intFromFloat((math.floor(input))));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float up to the nearest integer using zig std.math.
pub fn exportCeiling(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.c) T {
            return @as(T, @intFromFloat((math.ceil(input))));
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
        const rem = @mod(lhs, rhs);
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
        return self >> @as(u7, @intCast(other));
    }
}

/// Shifts a u128 right with zero fill.
pub fn shiftRightZeroFillU128(self: u128, other: u8) callconv(.c) u128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        return self >> @as(u7, @intCast(other));
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

    // Test negative value (arithmetic right shift preserves sign bit)
    const neg_value: i128 = -1;
    const result4 = shiftRightZeroFillI128(neg_value, 1);
    try std.testing.expectEqual(@as(i128, -1), result4);
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
