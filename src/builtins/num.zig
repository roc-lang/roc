//! Builtin numeric operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's numeric types and
//! operations, including integer and floating-point arithmetic, parsing,
//! overflow detection, and conversions. It defines numeric parsing utilities
//! and functions that are called from compiled Roc code to handle numeric
//! operations efficiently and safely.
const std = @import("std");
const builtins = @import("builtins");

const WithOverflow = builtins.utils.WithOverflow;
const RocCrashed = builtins.host_abi.RocCrashed;
const Ordering = builtins.utils.Ordering;
const RocList = builtins.list.RocList;
const RocOps = builtins.utils.RocOps;
const RocStr = builtins.str.RocStr;
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
        fn func(buf: RocStr) callconv(.C) NumParseResult(T) {
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
        fn func(buf: RocStr) callconv(.C) NumParseResult(T) {
            return @call(.always_inline, parseFloatFromStr, .{ T, buf });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Cast an integer to a float.
pub fn exportNumToFloatCast(comptime T: type, comptime F: type, comptime name: []const u8) void {
    const f = struct {
        fn func(x: T) callconv(.C) F {
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
        ) callconv(.C) T {
            switch (@typeInfo(T)) {
                // std.math.pow can handle ints via powi, but it turns any errors to unreachable
                // we want to catch overflow and report a proper error to the user
                .int => {
                    if (std.math.powi(T, base, exp)) |value| {
                        return value;
                    } else |err| switch (err) {
                        error.Overflow => {
                            const utf8_bytes = "Integer raised to power overflowed!";
                            const roc_crashed_args = RocCrashed{
                                .utf8_bytes = utf8_bytes,
                                .len = utf8_bytes.len,
                            };
                            roc_ops.roc_crashed(&roc_crashed_args, roc_ops.env);
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
        fn func(input: T) callconv(.C) bool {
            return std.math.isNan(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Check if a value is infinite.
pub fn exportIsInfinite(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) bool {
            return std.math.isInf(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Check if a value is finite.
pub fn exportIsFinite(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) bool {
            return std.math.isFinite(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute arcsine using zig std.math.
pub fn exportAsin(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.asin(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute arccosine using zig std.math.
pub fn exportAcos(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.acos(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute arctangent using zig std.math.
pub fn exportAtan(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.atan(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute sine using zig std.math.
pub fn exportSin(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return math.sin(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute cosine using zig std.math.
pub fn exportCos(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return math.cos(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute tangent using zig std.math.
pub fn exportTan(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return math.tan(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute natural logarithm using zig @log builtin.
pub fn exportLog(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return @log(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute absolute value using zig @abs builtin.
pub fn exportFAbs(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return @abs(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Compute square root using zig std.math.
pub fn exportSqrt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: T) callconv(.C) T {
            return math.sqrt(input);
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float to the nearest integer using zig std.math.
pub fn exportRound(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.C) T {
            return @as(T, @intFromFloat((math.round(input))));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float down to the nearest integer using zig std.math.
pub fn exportFloor(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.C) T {
            return @as(T, @intFromFloat((math.floor(input))));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Round a float up to the nearest integer using zig std.math.
pub fn exportCeiling(comptime F: type, comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(input: F) callconv(.C) T {
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
        ) callconv(.C) T {
            return math.divCeil(T, a, b) catch {
                const utf8_bytes = "Integer division by 0!";
                const roc_crashed_args = RocCrashed{
                    .utf8_bytes = utf8_bytes,
                    .len = utf8_bytes.len,
                };
                roc_ops.roc_crashed(&roc_crashed_args, roc_ops.env);
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
        fn func(input: From) callconv(.C) ToIntCheckedResult(To) {
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
        fn func(input: From) callconv(.C) ToIntCheckedResult(To) {
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
        fn func(lhs: T, rhs: T) callconv(.C) bool {
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
        fn func(self: T, other: T) callconv(.C) WithOverflow(T) {
            return @call(.always_inline, addWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to add two integers, saturating on overflow.
pub fn exportAddSaturatedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.C) T {
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
        fn func(self: T, other: T) callconv(.C) T {
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
        ) callconv(.C) T {
            const result = addWithOverflow(T, self, other);
            if (result.has_overflowed) {
                const utf8_bytes = "Integer addition overflowed!";
                const roc_crashed_args = RocCrashed{
                    .utf8_bytes = utf8_bytes,
                    .len = utf8_bytes.len,
                };
                roc_ops.roc_crashed(&roc_crashed_args, roc_ops.env);
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
        fn func(self: T, other: T) callconv(.C) WithOverflow(T) {
            return @call(.always_inline, subWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to subtract two integers, saturating on overflow.
pub fn exportSubSaturatedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.C) T {
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
        fn func(self: T, other: T) callconv(.C) T {
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
        ) callconv(.C) T {
            const result = subWithOverflow(T, self, other);
            if (result.has_overflowed) {
                const utf8_bytes = "Integer subtraction overflowed!";
                const roc_crashed_args = RocCrashed{
                    .utf8_bytes = utf8_bytes,
                    .len = utf8_bytes.len,
                };
                roc_ops.roc_crashed(&roc_crashed_args, roc_ops.env);
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
        fn func(self: T, other: T) callconv(.C) WithOverflow(T) {
            return @call(.always_inline, mulWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to multiply two integers, saturating on overflow.
pub fn exportMulSaturatedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = @call(.always_inline, mulWithOverflow, .{ T, self, other });
            return result.value;
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to multiply two integers, wrapping on overflow.
pub fn exportMulWrappedInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            return self *% other;
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Shifts an i128 right with zero fill.
pub fn shiftRightZeroFillI128(self: i128, other: u8) callconv(.C) i128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        return self >> @as(u7, @intCast(other));
    }
}

/// Shifts a u128 right with zero fill.
pub fn shiftRightZeroFillU128(self: u128, other: u8) callconv(.C) u128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        return self >> @as(u7, @intCast(other));
    }
}

/// Compares two i128 values, returning ordering.
pub fn compareI128(self: i128, other: i128) callconv(.C) Ordering {
    if (self == other) {
        return Ordering.EQ;
    } else if (self < other) {
        return Ordering.LT;
    } else {
        return Ordering.GT;
    }
}

/// Compares two u128 values, returning ordering.
pub fn compareU128(self: u128, other: u128) callconv(.C) Ordering {
    if (self == other) {
        return Ordering.EQ;
    } else if (self < other) {
        return Ordering.LT;
    } else {
        return Ordering.GT;
    }
}

/// Returns true if self < other for i128.
pub fn lessThanI128(self: i128, other: i128) callconv(.C) bool {
    return self < other;
}

/// Returns true if self <= other for i128.
pub fn lessThanOrEqualI128(self: i128, other: i128) callconv(.C) bool {
    return self <= other;
}

/// Returns true if self > other for i128.
pub fn greaterThanI128(self: i128, other: i128) callconv(.C) bool {
    return self > other;
}

/// Returns true if self >= other for i128.
pub fn greaterThanOrEqualI128(self: i128, other: i128) callconv(.C) bool {
    return self >= other;
}

/// Returns true if self < other for u128.
pub fn lessThanU128(self: u128, other: u128) callconv(.C) bool {
    return self < other;
}

/// Returns true if self <= other for u128.
pub fn lessThanOrEqualU128(self: u128, other: u128) callconv(.C) bool {
    return self <= other;
}

/// Returns true if self > other for u128.
pub fn greaterThanU128(self: u128, other: u128) callconv(.C) bool {
    return self > other;
}

/// Returns true if self >= other for u128.
pub fn greaterThanOrEqualU128(self: u128, other: u128) callconv(.C) bool {
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
        ) callconv(.C) T {
            const result = @call(.always_inline, mulWithOverflow, .{ T, self, other });
            if (result.has_overflowed) {
                const utf8_bytes = "Integer multiplication overflowed!";
                const roc_crashed_args = RocCrashed{
                    .utf8_bytes = utf8_bytes,
                    .len = utf8_bytes.len,
                };
                roc_ops.roc_crashed(&roc_crashed_args, roc_ops.env);
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
        fn func(self: T) callconv(.C) u8 {
            return @as(u8, @clz(self));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to count trailing zero bits.
pub fn exportCountTrailingZeroBits(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T) callconv(.C) u8 {
            return @as(u8, @ctz(self));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Exports a function to count one bits (population count).
pub fn exportCountOneBits(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(self: T) callconv(.C) u8 {
            return @as(u8, @popCount(self));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Returns the bitwise parts of an f32.
pub fn f32ToParts(self: f32) callconv(.C) F32Parts {
    const u32Value = @as(u32, @bitCast(self));
    return F32Parts{
        .fraction = u32Value & 0x7fffff,
        .exponent = @truncate(u32Value >> 23 & 0xff),
        .sign = u32Value >> 31 & 1 == 1,
    };
}

/// Returns the bitwise parts of an f64.
pub fn f64ToParts(self: f64) callconv(.C) F64Parts {
    const u64Value = @as(u64, @bitCast(self));
    return F64Parts{
        .fraction = u64Value & 0xfffffffffffff,
        .exponent = @truncate(u64Value >> 52 & 0x7ff),
        .sign = u64Value >> 63 & 1 == 1,
    };
}

/// Constructs an f32 from its bitwise parts.
pub fn f32FromParts(parts: F32Parts) callconv(.C) f32 {
    return @as(f32, @bitCast(parts.fraction & 0x7fffff | (@as(u32, parts.exponent) << 23) | (@as(u32, @intFromBool(parts.sign)) << 31)));
}

/// Constructs an f64 from its bitwise parts.
pub fn f64FromParts(parts: F64Parts) callconv(.C) f64 {
    return @as(f64, @bitCast(parts.fraction & 0xfffffffffffff | (@as(u64, parts.exponent & 0x7ff) << 52) | (@as(u64, @intFromBool(parts.sign)) << 63)));
}

/// Returns the bit pattern of an f32 as u32.
pub fn f32ToBits(self: f32) callconv(.C) u32 {
    return @as(u32, @bitCast(self));
}

/// Returns the bit pattern of an f64 as u64.
pub fn f64ToBits(self: f64) callconv(.C) u64 {
    return @as(u64, @bitCast(self));
}

/// Returns the bit pattern of an i128 as u128.
pub fn i128ToBits(self: i128) callconv(.C) u128 {
    return @as(u128, @bitCast(self));
}

/// Constructs an f32 from its bit pattern.
pub fn f32FromBits(bits: u32) callconv(.C) f32 {
    return @as(f32, @bitCast(bits));
}

/// Constructs an f64 from its bit pattern.
pub fn f64FromBits(bits: u64) callconv(.C) f64 {
    return @as(f64, @bitCast(bits));
}

/// Constructs an i128 from its bit pattern.
pub fn i128FromBits(bits: u128) callconv(.C) i128 {
    return @as(i128, @bitCast(bits));
}
