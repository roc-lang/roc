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
const decimal_parse = @import("decimal_parse.zig");
const float_bits = @import("float_bits.zig");

const WithOverflow = @import("utils.zig").WithOverflow;
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

/// Result of parsing a number from the longest numeric prefix of some bytes.
///
/// `consumed` is the byte length of the longest token matching the type's
/// numeric grammar. `errorcode` is 0 on success, `prefix_parse_not_a_number`
/// when no prefix matched (`consumed` is then 0), or `prefix_parse_out_of_range`
/// when a token matched but the whole-token `from_str` of it failed.
pub fn NumPrefixParseResult(comptime T: type) type {
    return extern struct {
        value: T,
        consumed: u64,
        errorcode: u8,
    };
}

/// `NumPrefixParseResult.errorcode` when no prefix of the input is a number token.
pub const prefix_parse_not_a_number: u8 = 1;
/// `NumPrefixParseResult.errorcode` when the matched token does not denote a value of the type.
pub const prefix_parse_out_of_range: u8 = 2;

fn prefixParseResult(comptime T: type, consumed: usize, value: ?T) NumPrefixParseResult(T) {
    if (consumed == 0) {
        return .{ .value = 0, .consumed = 0, .errorcode = prefix_parse_not_a_number };
    }
    if (value) |success| {
        return .{ .value = success, .consumed = consumed, .errorcode = 0 };
    }
    return .{ .value = 0, .consumed = consumed, .errorcode = prefix_parse_out_of_range };
}

/// Parses an integer from a RocStr
pub fn parseIntFromStr(comptime T: type, buf: RocStr) NumParseResult(T) {
    if (parseIntSlice(T, buf.asSlice())) |success| {
        return .{ .errorcode = 0, .value = success };
    } else {
        return .{ .errorcode = 1, .value = 0 };
    }
}

/// Whole-input integer parse (`from_str` semantics): the input must be exactly
/// one integer token, and the token must denote a value of `T`.
pub fn parseIntSlice(comptime T: type, bytes: []const u8) ?T {
    const consumed = intPrefixLen(bytes);
    if (consumed == 0 or consumed != bytes.len) return null;
    return parseIntToken(T, bytes);
}

/// Parse an integer from the longest integer token at the start of `bytes`.
pub fn parseIntPrefix(comptime T: type, bytes: []const u8) NumPrefixParseResult(T) {
    const consumed = intPrefixLen(bytes);
    if (consumed == 0) return prefixParseResult(T, 0, null);
    return prefixParseResult(T, consumed, parseIntToken(T, bytes[0..consumed]));
}

/// Length of the longest integer token at the start of `bytes`, or 0.
///
/// An integer token is either an explicit-radix token (`sign? 0x|0o|0b` then
/// digits of that radix, `_` only between digits) or a decimal token
/// (`sign? D (_? D)* (e sign? D (_? D)*)?`). A radix prefix without any digit
/// of its radix is not a radix token, so `"0x"` matches the decimal token `"0"`.
pub fn intPrefixLen(bytes: []const u8) usize {
    const radix_len = radixIntPrefixLen(bytes);
    if (radix_len != 0) return radix_len;
    return decimal_parse.prefixLen(bytes, .int);
}

fn parseIntToken(comptime T: type, token: []const u8) ?T {
    if (hasExplicitRadix(token)) return parseIntNoFmt(T, token) catch null;
    return decimal_parse.parseInt(T, token);
}

fn radixIntPrefixLen(bytes: []const u8) usize {
    if (!hasExplicitRadix(bytes)) return 0;
    const digits_start: usize = @as(usize, @intFromBool(bytes[0] == '-' or bytes[0] == '+')) + 2;
    const radix: u8 = switch (bytes[digits_start - 1]) {
        'b', 'B' => 2,
        'o', 'O' => 8,
        'x', 'X' => 16,
        else => unreachable,
    };

    var end = digits_start;
    var index = digits_start;
    while (index < bytes.len) : (index += 1) {
        const byte = bytes[index];
        if (byte == '_') {
            if (index == digits_start or index + 1 == bytes.len or !isRadixDigit(bytes[index + 1], radix)) break;
            continue;
        }
        if (!isRadixDigit(byte, radix)) break;
        end = index + 1;
    }
    return if (end == digits_start) 0 else end;
}

fn isRadixDigit(byte: u8, radix: u8) bool {
    const digit = digitValue(byte) orelse return false;
    return digit < radix;
}

const ParseIntError = error{
    InvalidCharacter,
    Overflow,
};

fn hasExplicitRadix(bytes: []const u8) bool {
    if (bytes.len == 0) return false;
    const start: usize = @intFromBool(bytes[0] == '-' or bytes[0] == '+');
    if (bytes.len - start < 2 or bytes[start] != '0') return false;
    return switch (bytes[start + 1]) {
        'b', 'B', 'o', 'O', 'x', 'X' => true,
        else => false,
    };
}

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

/// Parses a floating-point number from a RocStr.
pub fn parseFloatFromStr(comptime T: type, buf: RocStr) NumParseResult(T) {
    if (parseFloatSlice(T, buf.asSlice())) |success| {
        return .{ .errorcode = 0, .value = success };
    } else {
        return .{ .errorcode = 1, .value = 0 };
    }
}

/// Whole-input float parse (`from_str` semantics): the input must be exactly
/// one float token, and a finite token must not round to infinity.
pub fn parseFloatSlice(comptime T: type, bytes: []const u8) ?T {
    const consumed = floatPrefixLen(bytes);
    if (consumed == 0 or consumed != bytes.len) return null;
    return parseFloatToken(T, bytes);
}

/// Parse a float from the longest float token at the start of `bytes`.
pub fn parseFloatPrefix(comptime T: type, bytes: []const u8) NumPrefixParseResult(T) {
    const consumed = floatPrefixLen(bytes);
    if (consumed == 0) return prefixParseResult(T, 0, null);
    return prefixParseResult(T, consumed, parseFloatToken(T, bytes[0..consumed]));
}

/// Length of the longest float token at the start of `bytes`, or 0.
///
/// A float token is `sign?` followed by one of: a hex mantissa
/// `0x (H+ | H+ . H* | . H+)` with optional `p sign? D+` exponent; a decimal
/// mantissa `(D+ | D+ . D* | . D+)` with optional `e sign? D+` exponent; or
/// `infinity`, `inf`, `nan` in any case. `_` is accepted only between digits.
/// A hex prefix without any hex digit is not a hex token, so `"0x"` matches `"0"`.
pub fn floatPrefixLen(bytes: []const u8) usize {
    const start: usize = @intFromBool(bytes.len > 0 and (bytes[0] == '-' or bytes[0] == '+'));
    const body = bytes[start..];

    if (body.len >= 2 and body[0] == '0' and (body[1] == 'x' or body[1] == 'X')) {
        const hex_len = floatMantissaExponentPrefixLen(body[2..], 16, 'p');
        if (hex_len != 0) return start + 2 + hex_len;
    }

    const decimal_len = floatMantissaExponentPrefixLen(body, 10, 'e');
    if (decimal_len != 0) return start + decimal_len;

    if (std.ascii.startsWithIgnoreCase(body, "infinity")) return start + "infinity".len;
    if (std.ascii.startsWithIgnoreCase(body, "inf")) return start + "inf".len;
    if (std.ascii.startsWithIgnoreCase(body, "nan")) return start + "nan".len;
    return 0;
}

fn floatMantissaExponentPrefixLen(bytes: []const u8, comptime radix: u8, comptime exponent_char: u8) usize {
    var index: usize = 0;
    var digits: usize = 0;
    var had_point = false;
    while (index < bytes.len) : (index += 1) {
        const byte = bytes[index];
        if (isRadixDigit(byte, radix)) {
            digits += 1;
        } else if (byte == '_' and index > 0 and isRadixDigit(bytes[index - 1], radix) and
            index + 1 < bytes.len and isRadixDigit(bytes[index + 1], radix))
        {
            continue;
        } else if (byte == '.' and !had_point) {
            had_point = true;
        } else {
            break;
        }
    }
    if (digits == 0) return 0;

    if (index < bytes.len and (bytes[index] | 0x20) == exponent_char) {
        var cursor = index + 1;
        if (cursor < bytes.len and (bytes[cursor] == '-' or bytes[cursor] == '+')) cursor += 1;
        if (cursor < bytes.len and isRadixDigit(bytes[cursor], 10)) {
            while (cursor < bytes.len) : (cursor += 1) {
                const byte = bytes[cursor];
                if (isRadixDigit(byte, 10)) continue;
                if (byte == '_' and isRadixDigit(bytes[cursor - 1], 10) and
                    cursor + 1 < bytes.len and isRadixDigit(bytes[cursor + 1], 10)) continue;
                break;
            }
            index = cursor;
        }
    }
    return index;
}

fn parseFloatToken(comptime T: type, token: []const u8) ?T {
    const value = parse_float.parseFloat(T, token) catch return null;
    if (std.math.isInf(value) and !isExplicitInfinity(token)) return null;
    return value;
}

fn isExplicitInfinity(bytes: []const u8) bool {
    var text = std.mem.trim(u8, bytes, " \t\r\n");
    if (text.len > 0 and (text[0] == '+' or text[0] == '-')) {
        text = text[1..];
    }
    return std.ascii.eqlIgnoreCase(text, "inf") or
        std.ascii.eqlIgnoreCase(text, "infinity");
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

/// i128 modulo (result carries the sign of the divisor) - callable from generated code.
pub fn modI128(a: i128, b: i128, roc_ops: *RocOps) callconv(.c) i128 {
    if (b == 0) {
        roc_ops.crash("Integer modulo by 0!");
    }
    return i128h.mod_i128(a, b);
}

/// Adds two numbers, returning result and overflow flag.
pub fn addWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    if (@typeInfo(T) == .int) {
        const answer = @addWithOverflow(self, other);
        return .{ .value = answer[0], .has_overflowed = answer[1] == 1 };
    } else {
        const answer = self + other;
        const overflowed = !std.math.isFinite(answer);
        return .{ .value = answer, .has_overflowed = overflowed };
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

/// Subtracts two numbers, returning result and overflow flag.
pub fn subWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    if (@typeInfo(T) == .int) {
        const answer = @subWithOverflow(self, other);
        return .{ .value = answer[0], .has_overflowed = answer[1] == 1 };
    } else {
        const answer = self - other;
        const overflowed = !std.math.isFinite(answer);
        return .{ .value = answer, .has_overflowed = overflowed };
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

/// Multiplies two numbers, returning result and overflow flag.
pub fn mulWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    if (@typeInfo(T) == .int) {
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
    } else {
        const answer = self * other;
        const overflowed = !std.math.isFinite(answer);
        return .{ .value = answer, .has_overflowed = overflowed };
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

/// Returns the bit pattern of an f32 as u32.
pub fn f32ToBits(self: f32) callconv(.c) u32 {
    return float_bits.normalizeF32NanBits(@bitCast(self));
}

/// Returns the bit pattern of an f64 as u64.
pub fn f64ToBits(self: f64) callconv(.c) u64 {
    return float_bits.normalizeF64NanBits(@bitCast(self));
}

/// Constructs an f32 from its bit pattern.
pub fn f32FromBits(bits: u32) callconv(.c) f32 {
    return @as(f32, @bitCast(bits));
}

/// Constructs an f64 from its bit pattern.
pub fn f64FromBits(bits: u64) callconv(.c) f64 {
    return @as(f64, @bitCast(bits));
}

fn signedMinText(comptime T: type) []const u8 {
    if (T == i8) return "-128";
    if (T == i16) return "-32768";
    if (T == i32) return "-2147483648";
    if (T == i64) return "-9223372036854775808";
    if (T == i128) return "-170141183460469231731687303715884105728";
    @compileError("unsupported signed integer type");
}

fn signedMaxText(comptime T: type) []const u8 {
    if (T == i8) return "127";
    if (T == i16) return "32767";
    if (T == i32) return "2147483647";
    if (T == i64) return "9223372036854775807";
    if (T == i128) return "170141183460469231731687303715884105727";
    @compileError("unsupported signed integer type");
}

fn signedMaxPlusOneText(comptime T: type) []const u8 {
    if (T == i8) return "128";
    if (T == i16) return "32768";
    if (T == i32) return "2147483648";
    if (T == i64) return "9223372036854775808";
    if (T == i128) return "170141183460469231731687303715884105728";
    @compileError("unsupported signed integer type");
}

fn signedMinMinusOneText(comptime T: type) []const u8 {
    if (T == i8) return "-129";
    if (T == i16) return "-32769";
    if (T == i32) return "-2147483649";
    if (T == i64) return "-9223372036854775809";
    if (T == i128) return "-170141183460469231731687303715884105729";
    @compileError("unsupported signed integer type");
}

fn unsignedMaxText(comptime T: type) []const u8 {
    if (T == u8) return "255";
    if (T == u16) return "65535";
    if (T == u32) return "4294967295";
    if (T == u64) return "18446744073709551615";
    if (T == u128) return "340282366920938463463374607431768211455";
    @compileError("unsupported unsigned integer type");
}

fn unsignedMaxPlusOneText(comptime T: type) []const u8 {
    if (T == u8) return "256";
    if (T == u16) return "65536";
    if (T == u32) return "4294967296";
    if (T == u64) return "18446744073709551616";
    if (T == u128) return "340282366920938463463374607431768211456";
    @compileError("unsupported unsigned integer type");
}

const NumTestHelperError = error{
    TestExpectedEqual,
};

/// Errors raised by the numeric prefix-parse test helpers.
pub const PrefixTestError = error{
    TestExpectedEqual,
    TestUnexpectedResult,
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

fn expectParseFloatBits(comptime T: type, text: []const u8, expected_bits: @Int(.unsigned, @bitSizeOf(T)), roc_ops: *RocOps) NumTestHelperError!void {
    const roc_str = @import("str.zig").RocStr.fromSlice(text, roc_ops);
    defer roc_str.decref(roc_ops);

    const result = parseFloatFromStr(T, roc_str);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);

    const Bits = @Int(.unsigned, @bitSizeOf(T));
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

test "parseIntFromStr accepts decimal exponent notation issue 10550" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Repro for https://github.com/roc-lang/roc/issues/10550.
    try expectParseIntText(u32, "2e5", 200_000, test_env.getOps());

    inline for (.{ u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }) |T| {
        try expectParseIntText(T, "2e1", 20, test_env.getOps());
        try expectParseIntText(T, "+2E1", 20, test_env.getOps());
    }

    inline for (.{ i8, i16, i32, i64, i128 }) |T| {
        try expectParseIntText(T, "-2e1", -20, test_env.getOps());
    }

    try expectParseIntText(u64, "2e1_0", 20_000_000_000, test_env.getOps());
    try expectParseIntText(u32, "4294967295e0", std.math.maxInt(u32), test_env.getOps());
    try expectParseIntText(i128, "-170141183460469231731687303715884105728e0", std.math.minInt(i128), test_env.getOps());
    try expectParseIntText(u128, "340282366920938463463374607431768211455e0", std.math.maxInt(u128), test_env.getOps());
    try expectParseIntText(u8, "0e999999999999999999999999999999999999", 0, test_env.getOps());
}

test "parseIntFromStr rejects fractional malformed and overflowing exponent notation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (.{ "2e-1", "20e-1", "2.0e5", "2e", "2e+", "2_e5", "2e_5", "2e5_", "2ee5" }) |text| {
        try expectParseIntReject(i64, text, test_env.getOps());
    }

    try expectParseIntReject(u32, "4294967296e0", test_env.getOps());
    try expectParseIntReject(u32, "4294967295e1", test_env.getOps());
    try expectParseIntReject(i128, "-170141183460469231731687303715884105729e0", test_env.getOps());
    try expectParseIntReject(u128, "340282366920938463463374607431768211456e0", test_env.getOps());
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

fn expectSeparatorsIgnored(comptime T: type, with_separators: []const u8, roc_ops: *RocOps) NumTestHelperError!void {
    var buf: [512]u8 = undefined;
    var n: usize = 0;
    for (with_separators) |c| {
        if (c != '_') {
            buf[n] = c;
            n += 1;
        }
    }

    const RocStrT = @import("str.zig").RocStr;
    const with_str = RocStrT.fromSlice(with_separators, roc_ops);
    defer with_str.decref(roc_ops);
    const without_str = RocStrT.fromSlice(buf[0..n], roc_ops);
    defer without_str.decref(roc_ops);

    const with = parseFloatFromStr(T, with_str);
    const without = parseFloatFromStr(T, without_str);
    try std.testing.expectEqual(@as(u8, 0), with.errorcode);
    try std.testing.expectEqual(@as(u8, 0), without.errorcode);

    const Bits = @Int(.unsigned, @bitSizeOf(T));
    try std.testing.expectEqual(@as(Bits, @bitCast(without.value)), @as(Bits, @bitCast(with.value)));
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

test "parseFloatFromStr rounds hex floats with more significant digits than the mantissa holds" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (&[_]struct { text: []const u8, bits: u32 }{
        // 1 + 2^-24 is exactly halfway between 1 and 1 + 2^-23, so it rounds to even.
        .{ .text = "0x1.00000100000000000000p0", .bits = 0x3f800000 },
        .{ .text = "0x1.00000100000000000001p0", .bits = 0x3f800001 },
    }) |text| {
        try expectParseFloatBits(f32, text.text, text.bits, test_env.getOps());
    }

    inline for (&[_]struct { text: []const u8, bits: u64 }{
        // 1 + 2^-53 is exactly halfway between 1 and 1 + 2^-52.
        .{ .text = "0x1.000000000000080000p0", .bits = 0x3ff0000000000000 },
        .{ .text = "0x1.0000000000000800000000000000p0", .bits = 0x3ff0000000000000 },
        .{ .text = "0x1.0000000000000801p0", .bits = 0x3ff0000000000001 },
        .{ .text = "0x1.0000000000000800000001p0", .bits = 0x3ff0000000000001 },
        .{ .text = "0x1.0000000000000800000000000001p0", .bits = 0x3ff0000000000001 },
        // 1 + 3 * 2^-53 is halfway between 1 + 2^-52 and 1 + 2^-51, so it rounds to even.
        .{ .text = "0x1.00000000000018000p0", .bits = 0x3ff0000000000002 },
        .{ .text = "0x1.0000000000001800000000000000p0", .bits = 0x3ff0000000000002 },
        .{ .text = "0x123456789abcdef01", .bits = 0x43f23456789abcdf },
        // 2^72 + 2^19 is halfway between 2^72 and 2^72 + 2^20.
        .{ .text = "0x1000000000000080000p0", .bits = 0x4470000000000000 },
        .{ .text = "0x1_0000_0000_0000_0800_01p0", .bits = 0x4470000000000001 },
        .{ .text = "0x0.00000000000000000010000000000000080000p0", .bits = 0x3b30000000000000 },
        .{ .text = "1.00000000000000000000000000", .bits = 0x3ff0000000000000 },
        .{ .text = "12345678901234567890123.0000000000000", .bits = 0x4484ea15b273b38a },
    }) |text| {
        try expectParseFloatBits(f64, text.text, text.bits, test_env.getOps());
    }
}

test "parseFloatFromStr ignores digit separators wherever they appear issue 10660" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // `_` is a separator, so a literal must parse identically with and without it.
    // The last case is the fuzzer-found input from the issue: leading zero groups mixed
    // with separators moved the decimal point by 163 orders of magnitude, which sent the
    // slow path into minutes of shifting before returning a wrong value.
    inline for (&[_][]const u8{
        "1_000",
        "1_000.5",
        "0.5_5",
        "0_0.1",
        "1_0e1_0",
        "-1_2.3_4",
        "0_0000_0000.0000_1",
        "1_2_3_4_5.6_7_8_9",
        "0.000_000_000_000_000_1",
        "1_000e-1_0",
        "123_456.789_012e1_2",
        "0_0000_0000.00_000_0_000000_00000_00_00_00_0_000000_0000_0_00000_00000_00_00_00_000_0_0_000000_00000_0_000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013478162400000000000",
    }) |text| {
        try expectSeparatorsIgnored(f64, text, test_env.getOps());
    }
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

test "addWithOverflow with floating point" {
    // Test normal floating point addition
    const result1 = addWithOverflow(f32, 1.5, 2.5);
    try std.testing.expectEqual(@as(f32, 4.0), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test infinite result
    const result2 = addWithOverflow(f32, std.math.floatMax(f32), std.math.floatMax(f32));
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "mul_u128 basic functionality" {
    const a: u128 = 1000000;
    const b: u128 = 2000000;
    const result = mul_u128(a, b);

    // 1000000 * 2000000 = 2000000000000, which fits in u128
    try std.testing.expectEqual(@as(u128, 0), result.hi);
    try std.testing.expectEqual(@as(u128, 2000000000000), result.lo);
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

test "float to bits normalizes every NaN representation" {
    const f32_nan_bits = [_]u32{ 0x7f80_0001, 0x7fc1_2345, 0xff80_0001, 0xffc1_2345 };
    for (f32_nan_bits) |bits| {
        try std.testing.expectEqual(float_bits.normalized_f32_nan_bits, f32ToBits(f32FromBits(bits)));
    }

    const f64_nan_bits = [_]u64{
        0x7ff0_0000_0000_0001,
        0x7ff9_2345_6789_abcd,
        0xfff0_0000_0000_0001,
        0xfff9_2345_6789_abcd,
    };
    for (f64_nan_bits) |bits| {
        try std.testing.expectEqual(float_bits.normalized_f64_nan_bits, f64ToBits(f64FromBits(bits)));
    }
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

// ── Numeric prefix parsing ──

/// Generative input for numeric prefix-parse property tests: a random
/// composition of small numeric-token pieces (signs, digit runs, `_`, radix
/// prefixes, `.`, exponent markers, special-value words) and terminators.
pub const prefix_parse_testing = struct {
    const pieces = [_][]const u8{
        "-",  "+", "_",  ".",  "0x",  "0o",       "0b",  "0X",
        "e",  "E", "e+", "e-", "p",   "p-",       "P+",  "a",
        "f",  "F", "g",  "x",  "inf", "infinity", "nan", "NaN",
        "In", ",", " ",  "]",  "\n",  "9",        "1",   "0",
    };

    /// Fill `buf` with a random composition of pieces and return the used prefix.
    pub fn randomText(random: std.Random, buf: []u8) []const u8 {
        var len: usize = 0;
        const piece_count = random.uintAtMost(usize, 7);
        var i: usize = 0;
        while (i < piece_count) : (i += 1) {
            if (random.boolean()) {
                const digit_count = random.intRangeAtMost(usize, 1, 3);
                var d: usize = 0;
                while (d < digit_count and len < buf.len) : (d += 1) {
                    buf[len] = '0' + random.uintLessThan(u8, 10);
                    len += 1;
                }
            } else {
                const piece = pieces[random.uintLessThan(usize, pieces.len)];
                if (len + piece.len > buf.len) break;
                @memcpy(buf[len..][0..piece.len], piece);
                len += piece.len;
            }
        }
        return buf[0..len];
    }

    /// Check the prefix-parse properties of one generated input against the
    /// whole-string parser of the same type.
    pub fn expectProperties(comptime T: type, text: []const u8, comptime prefixLen: fn ([]const u8) usize, comptime parsePrefix: fn ([]const u8) NumPrefixParseResult(T), comptime parseWhole: fn ([]const u8) ?T) PrefixTestError!void {
        const Bits = @Int(.unsigned, @bitSizeOf(T));
        const result = parsePrefix(text);
        const consumed: usize = @intCast(result.consumed);
        try std.testing.expect(consumed <= text.len);
        try std.testing.expectEqual(prefixLen(text), consumed);

        switch (result.errorcode) {
            0 => {
                try std.testing.expect(consumed > 0);
                const whole = parseWhole(text[0..consumed]) orelse return error.TestUnexpectedResult;
                try std.testing.expectEqual(@as(Bits, @bitCast(whole)), @as(Bits, @bitCast(result.value)));
            },
            prefix_parse_out_of_range => {
                try std.testing.expect(consumed > 0);
                try std.testing.expectEqual(@as(?T, null), parseWhole(text[0..consumed]));
            },
            prefix_parse_not_a_number => try std.testing.expectEqual(@as(usize, 0), consumed),
            else => return error.TestUnexpectedResult,
        }

        // Longest match: no longer prefix of the input is itself a whole token.
        var longer = consumed + 1;
        while (longer <= text.len) : (longer += 1) {
            try std.testing.expect(prefixLen(text[0..longer]) != longer);
        }

        // Whole-string acceptance followed by a byte that continues no token.
        if (parseWhole(text)) |whole| {
            var buf: [64]u8 = undefined;
            for (", ]\n") |terminator| {
                @memcpy(buf[0..text.len], text);
                buf[text.len] = terminator;
                const terminated = parsePrefix(buf[0 .. text.len + 1]);
                try std.testing.expectEqual(@as(u8, 0), terminated.errorcode);
                try std.testing.expectEqual(@as(u64, text.len), terminated.consumed);
                try std.testing.expectEqual(@as(Bits, @bitCast(whole)), @as(Bits, @bitCast(terminated.value)));
            }
        }
    }
};

fn expectPrefixOk(comptime T: type, result: NumPrefixParseResult(T), expected: T, consumed: usize) PrefixTestError!void {
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);
    try std.testing.expectEqual(@as(u64, consumed), result.consumed);
    if (@typeInfo(T) == .float) {
        const Bits = @Int(.unsigned, @bitSizeOf(T));
        try std.testing.expectEqual(@as(Bits, @bitCast(expected)), @as(Bits, @bitCast(result.value)));
    } else {
        try std.testing.expectEqual(expected, result.value);
    }
}

fn expectPrefixErr(comptime T: type, result: NumPrefixParseResult(T), errorcode: u8, consumed: usize) PrefixTestError!void {
    try std.testing.expectEqual(errorcode, result.errorcode);
    try std.testing.expectEqual(@as(u64, consumed), result.consumed);
}

test "parseIntPrefix width boundaries and overflow by one digit" {
    inline for (.{ u8, u16, u32, u64, u128 }) |T| {
        const max_text = comptime unsignedMaxText(T);
        try expectPrefixOk(T, parseIntPrefix(T, max_text ++ ","), std.math.maxInt(T), max_text.len);
        try expectPrefixErr(T, parseIntPrefix(T, comptime unsignedMaxPlusOneText(T) ++ "]"), prefix_parse_out_of_range, comptime unsignedMaxPlusOneText(T).len);
        try expectPrefixErr(T, parseIntPrefix(T, max_text ++ "0 "), prefix_parse_out_of_range, max_text.len + 1);
    }
    inline for (.{ i8, i16, i32, i64, i128 }) |T| {
        try expectPrefixOk(T, parseIntPrefix(T, comptime signedMaxText(T) ++ ","), std.math.maxInt(T), comptime signedMaxText(T).len);
        try expectPrefixOk(T, parseIntPrefix(T, comptime signedMinText(T) ++ " "), std.math.minInt(T), comptime signedMinText(T).len);
        try expectPrefixErr(T, parseIntPrefix(T, comptime signedMaxPlusOneText(T) ++ "]"), prefix_parse_out_of_range, comptime signedMaxPlusOneText(T).len);
        try expectPrefixErr(T, parseIntPrefix(T, comptime signedMinMinusOneText(T) ++ "\n"), prefix_parse_out_of_range, comptime signedMinMinusOneText(T).len);
    }

    try expectPrefixErr(u8, parseIntPrefix(u8, "256,"), prefix_parse_out_of_range, 3);
    try expectPrefixErr(u8, parseIntPrefix(u8, "300,"), prefix_parse_out_of_range, 3);
    try expectPrefixErr(i8, parseIntPrefix(i8, "-129"), prefix_parse_out_of_range, 4);
    try expectPrefixErr(i8, parseIntPrefix(i8, "128"), prefix_parse_out_of_range, 3);
    try expectPrefixErr(u8, parseIntPrefix(u8, "1e99,"), prefix_parse_out_of_range, 4);
}

test "parseIntPrefix never ends a token on a dangling sign, underscore, or exponent" {
    inline for (.{ u8, i8, u64, i128 }) |T| {
        try expectPrefixErr(T, parseIntPrefix(T, ""), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseIntPrefix(T, "-"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseIntPrefix(T, "+"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseIntPrefix(T, "-abc"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseIntPrefix(T, "_1"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseIntPrefix(T, " 1"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseIntPrefix(T, ".5"), prefix_parse_not_a_number, 0);

        try expectPrefixOk(T, parseIntPrefix(T, "1_"), 1, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "1__2"), 1, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "1_2x"), 12, 3);
        try expectPrefixOk(T, parseIntPrefix(T, "2e"), 2, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "2e+"), 2, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "2E+1,"), 20, 4);
        try expectPrefixOk(T, parseIntPrefix(T, "2e_1"), 2, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "2e1_"), 20, 3);
        try expectPrefixOk(T, parseIntPrefix(T, "2e-"), 2, 1);
        try expectPrefixErr(T, parseIntPrefix(T, "2e-1"), prefix_parse_out_of_range, 4);
        // `.` is not integer grammar: version strings split at the first `.`.
        try expectPrefixOk(T, parseIntPrefix(T, "1.2.3"), 1, 1);
    }
    try expectPrefixOk(u32, parseIntPrefix(u32, "2e5ast"), 200_000, 3);
    try expectPrefixOk(u8, parseIntPrefix(u8, "0e99999999999999999999,"), 0, 22);
}

test "parseIntPrefix radix tokens take the longest run of valid radix digits" {
    inline for (.{ u8, i8, u64, i128 }) |T| {
        try expectPrefixOk(T, parseIntPrefix(T, "0x"), 0, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "0xg"), 0, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "0x_1"), 0, 1);
        try expectPrefixOk(T, parseIntPrefix(T, "0b12"), 1, 3);
        try expectPrefixOk(T, parseIntPrefix(T, "0o78"), 7, 3);
        try expectPrefixOk(T, parseIntPrefix(T, "0B1_0_"), 2, 5);
        try expectPrefixOk(T, parseIntPrefix(T, "+0x1f,"), 31, 5);
        // Hex digits include `e`, so there is no exponent after a radix prefix.
        try expectPrefixOk(T, parseIntPrefix(T, "0x1e"), 30, 4);
    }
    try expectPrefixOk(u8, parseIntPrefix(u8, "0xFFg"), 255, 4);
    try expectPrefixErr(u8, parseIntPrefix(u8, "0x100"), prefix_parse_out_of_range, 5);
    try expectPrefixOk(i8, parseIntPrefix(i8, "-0x80]"), -128, 5);
    try expectPrefixOk(i8, parseIntPrefix(i8, "-0x"), 0, 2);
}

test "parseIntPrefix unsigned negative zero is Ok and negative magnitudes are out of range" {
    inline for (.{ u8, u16, u32, u64, u128 }) |T| {
        try expectPrefixOk(T, parseIntPrefix(T, "-0,"), 0, 2);
        try expectPrefixErr(T, parseIntPrefix(T, "-5,"), prefix_parse_out_of_range, 2);
    }
}

test "parseIntPrefix integer exponents may be negative, and non-integral values are out of range" {
    // The integer token grammar is `D (e sign? D)?`, independent of value, so
    // whole-string `from_str` is unchanged: `1e-0` is an integer and `0e-5`,
    // `2e-1` are complete tokens that do not denote integers.
    try expectPrefixOk(i64, parseIntPrefix(i64, "1e-0,"), 1, 4);
    try std.testing.expectEqual(@as(?i64, 1), parseIntSlice(i64, "1e-0"));
    try std.testing.expectEqual(@as(?i64, 10), parseIntSlice(i64, "10e-00"));
    try expectPrefixErr(i64, parseIntPrefix(i64, "0e-5,"), prefix_parse_out_of_range, 4);
    try std.testing.expectEqual(@as(?i64, null), parseIntSlice(i64, "0e-5"));
    try expectPrefixErr(u64, parseIntPrefix(u64, "2e-1,"), prefix_parse_out_of_range, 4);
    try expectPrefixOk(u64, parseIntPrefix(u64, "2e-"), 2, 1);
}

test "parseFloatPrefix decimal mantissa forms" {
    inline for (.{ f32, f64 }) |T| {
        try expectPrefixOk(T, parseFloatPrefix(T, ".5"), 0.5, 2);
        try expectPrefixOk(T, parseFloatPrefix(T, "1."), 1.0, 2);
        try expectPrefixOk(T, parseFloatPrefix(T, "1.x"), 1.0, 2);
        try expectPrefixOk(T, parseFloatPrefix(T, "1.5e3]"), 1500.0, 5);
        try expectPrefixOk(T, parseFloatPrefix(T, "-.5e1,"), -5.0, 5);
        try expectPrefixOk(T, parseFloatPrefix(T, "1_0 "), 10.0, 3);
        try expectPrefixOk(T, parseFloatPrefix(T, "1_"), 1.0, 1);
        try expectPrefixOk(T, parseFloatPrefix(T, "1._5"), 1.0, 2);
        try expectPrefixOk(T, parseFloatPrefix(T, "2e"), 2.0, 1);
        try expectPrefixOk(T, parseFloatPrefix(T, "2e+"), 2.0, 1);
        try expectPrefixOk(T, parseFloatPrefix(T, "2e-1,"), 0.2, 4);
        try expectPrefixOk(T, parseFloatPrefix(T, "1.2.3"), 1.2, 3);
        try expectPrefixErr(T, parseFloatPrefix(T, ""), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseFloatPrefix(T, "-"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseFloatPrefix(T, "."), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseFloatPrefix(T, "e5"), prefix_parse_not_a_number, 0);
        try expectPrefixErr(T, parseFloatPrefix(T, " 1"), prefix_parse_not_a_number, 0);
    }
}

test "parseFloatPrefix hex floats" {
    inline for (.{ f32, f64 }) |T| {
        try expectPrefixOk(T, parseFloatPrefix(T, "0x1p3"), 8.0, 5);
        try expectPrefixOk(T, parseFloatPrefix(T, "0x1.8p1,"), 3.0, 7);
        try expectPrefixOk(T, parseFloatPrefix(T, "-0X1P-1]"), -0.5, 7);
        try expectPrefixOk(T, parseFloatPrefix(T, "0x1p"), 1.0, 3);
        try expectPrefixOk(T, parseFloatPrefix(T, "0x"), 0.0, 1);
        try expectPrefixOk(T, parseFloatPrefix(T, "0xg"), 0.0, 1);
        try expectPrefixOk(T, parseFloatPrefix(T, "0x.8"), 0.5, 4);
    }
}

test "parseFloatPrefix special values underflow and overflow" {
    inline for (.{ f32, f64 }) |T| {
        const inf = std.math.inf(T);
        try expectPrefixOk(T, parseFloatPrefix(T, "inf"), inf, 3);
        try expectPrefixOk(T, parseFloatPrefix(T, "Infinity,"), inf, 8);
        try expectPrefixOk(T, parseFloatPrefix(T, "-INF]"), -inf, 4);
        try expectPrefixOk(T, parseFloatPrefix(T, "+infinix"), inf, 4);

        const nan_result = parseFloatPrefix(T, "NaN,");
        try std.testing.expectEqual(@as(u8, 0), nan_result.errorcode);
        try std.testing.expectEqual(@as(u64, 3), nan_result.consumed);
        try std.testing.expect(std.math.isNan(nan_result.value));
        try std.testing.expectEqual(@as(u64, 4), parseFloatPrefix(T, "-nanx").consumed);

        try expectPrefixErr(T, parseFloatPrefix(T, "in"), prefix_parse_not_a_number, 0);
    }

    try expectPrefixOk(f64, parseFloatPrefix(f64, "1e-400,"), 0.0, 6);
    try expectPrefixOk(f32, parseFloatPrefix(f32, "1e-50,"), 0.0, 5);
    try expectPrefixErr(f64, parseFloatPrefix(f64, "1e400,"), prefix_parse_out_of_range, 5);
    try expectPrefixErr(f32, parseFloatPrefix(f32, "1e39,"), prefix_parse_out_of_range, 4);
}

fn IntPrefixFns(comptime T: type) type {
    return struct {
        fn prefix(bytes: []const u8) NumPrefixParseResult(T) {
            return parseIntPrefix(T, bytes);
        }
        fn whole(bytes: []const u8) ?T {
            return parseIntSlice(T, bytes);
        }
    };
}

fn FloatPrefixFns(comptime T: type) type {
    return struct {
        fn prefix(bytes: []const u8) NumPrefixParseResult(T) {
            return parseFloatPrefix(T, bytes);
        }
        fn whole(bytes: []const u8) ?T {
            return parseFloatSlice(T, bytes);
        }
    };
}

test "numeric prefix parse properties over generated token compositions" {
    var prng = std.Random.DefaultPrng.init(0x7010_0bad_cafe);
    const random = prng.random();
    var buf: [48]u8 = undefined;

    var iteration: usize = 0;
    while (iteration < 20_000) : (iteration += 1) {
        const text = prefix_parse_testing.randomText(random, &buf);
        inline for (.{ u8, i8, u16, i32, u64, i64, u128, i128 }) |T| {
            const fns = IntPrefixFns(T);
            try prefix_parse_testing.expectProperties(T, text, intPrefixLen, fns.prefix, fns.whole);
        }
        inline for (.{ f32, f64 }) |T| {
            const fns = FloatPrefixFns(T);
            try prefix_parse_testing.expectProperties(T, text, floatPrefixLen, fns.prefix, fns.whole);
        }
    }
}
