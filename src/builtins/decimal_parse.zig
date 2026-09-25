//! Allocation-free parsing and exact conversion of decimal text.

const std = @import("std");

const max_u128_before_mul_10 = std.math.maxInt(u128) / 10;
const max_u128_last_digit = std.math.maxInt(u128) % 10;

const ParsedDecimal = struct {
    negative: bool,
    had_decimal_point: bool,
    mantissa_end: usize,
    token_len: usize,
    coefficient_digits: usize,
    fractional_digits: usize,
    leading_zero_digits: usize,
    trailing_zero_digits: usize,
    coefficient: u128,
    coefficient_overflow: bool,
    exponent_negative: bool,
    exponent_magnitude: u64,
    exponent_overflow: bool,

    fn isZero(self: ParsedDecimal) bool {
        return self.leading_zero_digits == self.coefficient_digits;
    }
};

/// Which decimal token grammar a scan recognizes.
pub const Grammar = enum {
    /// `sign? D (_? D)* (e +? D (_? D)*)?`: no decimal point, no negative exponent.
    int,
    /// `sign? (D+ | D+ . D* | . D+) (e sign? D (_? D)*)?`, with `_` between digits.
    dec,
};

/// Scan the longest prefix of `bytes` that is a token of `grammar`, returning
/// its exact coefficient-and-scale facts, or null when no prefix matches.
/// Underscores are accepted only between decimal digits. The token never ends
/// on a dangling sign, `_`, `e`, or exponent sign: an exponent that is not
/// followed by a digit is not part of the token.
fn scanPrefix(bytes: []const u8, comptime grammar: Grammar) ?ParsedDecimal {
    var index: usize = 0;
    const negative = index < bytes.len and bytes[index] == '-';
    if (index < bytes.len and (bytes[index] == '-' or bytes[index] == '+')) index += 1;

    var had_decimal_point = false;
    var saw_digit = false;
    var coefficient_digits: usize = 0;
    var fractional_digits: usize = 0;
    var leading_zero_digits: usize = 0;
    var trailing_zero_digits: usize = 0;
    var saw_nonzero = false;
    var coefficient: u128 = 0;
    var coefficient_overflow = false;

    mantissa: while (index < bytes.len) : (index += 1) {
        const byte = bytes[index];
        switch (byte) {
            '0'...'9' => {
                const digit = byte - '0';
                saw_digit = true;
                coefficient_digits += 1;
                if (had_decimal_point) fractional_digits += 1;

                if (!saw_nonzero and digit == 0) {
                    leading_zero_digits += 1;
                } else {
                    saw_nonzero = true;
                }
                trailing_zero_digits = if (digit == 0) trailing_zero_digits + 1 else 0;

                if (!coefficient_overflow) {
                    if (coefficient > max_u128_before_mul_10 or
                        (coefficient == max_u128_before_mul_10 and digit > max_u128_last_digit))
                    {
                        coefficient_overflow = true;
                    } else {
                        coefficient = coefficient * 10 + digit;
                    }
                }
            },
            '_' => {
                if (index == 0 or !isDigit(bytes[index - 1]) or index + 1 == bytes.len or !isDigit(bytes[index + 1])) break :mantissa;
            },
            '.' => {
                if (grammar == .int or had_decimal_point) break :mantissa;
                had_decimal_point = true;
            },
            else => break :mantissa,
        }
    }

    if (!saw_digit) return null;
    const mantissa_end = index;

    var exponent_negative = false;
    var exponent_magnitude: u64 = 0;
    var exponent_overflow = false;
    if (index < bytes.len and (bytes[index] == 'e' or bytes[index] == 'E')) exponent: {
        var cursor = index + 1;
        var candidate_negative = false;
        if (cursor < bytes.len and (bytes[cursor] == '+' or (grammar == .dec and bytes[cursor] == '-'))) {
            candidate_negative = bytes[cursor] == '-';
            cursor += 1;
        }
        if (cursor == bytes.len or !isDigit(bytes[cursor])) break :exponent;

        var magnitude: u64 = 0;
        var overflow = false;
        while (cursor < bytes.len) : (cursor += 1) {
            const byte = bytes[cursor];
            if (isDigit(byte)) {
                const digit = byte - '0';
                if (!overflow) {
                    if (magnitude > (std.math.maxInt(u64) - @as(u64, digit)) / 10) {
                        overflow = true;
                    } else {
                        magnitude = magnitude * 10 + digit;
                    }
                }
            } else if (byte == '_' and isDigit(bytes[cursor - 1]) and cursor + 1 < bytes.len and isDigit(bytes[cursor + 1])) {
                continue;
            } else {
                break;
            }
        }

        exponent_negative = candidate_negative;
        exponent_magnitude = magnitude;
        exponent_overflow = overflow;
        index = cursor;
    }

    return .{
        .negative = negative,
        .had_decimal_point = had_decimal_point,
        .mantissa_end = mantissa_end,
        .token_len = index,
        .coefficient_digits = coefficient_digits,
        .fractional_digits = fractional_digits,
        .leading_zero_digits = leading_zero_digits,
        .trailing_zero_digits = trailing_zero_digits,
        .coefficient = coefficient,
        .coefficient_overflow = coefficient_overflow,
        .exponent_negative = exponent_negative,
        .exponent_magnitude = exponent_magnitude,
        .exponent_overflow = exponent_overflow,
    };
}

/// Parse decimal syntax into exact coefficient-and-scale facts. The whole of
/// `bytes` must be a single token of `grammar`.
fn scan(bytes: []const u8, comptime grammar: Grammar) ?ParsedDecimal {
    const parsed = scanPrefix(bytes, grammar) orelse return null;
    if (parsed.token_len != bytes.len) return null;
    return parsed;
}

/// Length of the longest prefix of `bytes` that is a token of `grammar`, or 0.
/// The match depends only on syntax, never on whether the value is in range.
pub fn prefixLen(bytes: []const u8, comptime grammar: Grammar) usize {
    const parsed = scanPrefix(bytes, grammar) orelse return 0;
    return parsed.token_len;
}

fn isDigit(byte: u8) bool {
    return byte >= '0' and byte <= '9';
}

fn appendDecimalZeros(comptime limit: u128, initial: u128, count: usize) ?u128 {
    const max_before_mul = limit / 10;
    var value = initial;
    if (value > limit) return null;

    var remaining = count;
    while (remaining > 0) : (remaining -= 1) {
        if (value > max_before_mul) return null;
        value *= 10;
    }
    return value;
}

fn parseCoefficientPrefix(comptime limit: u128, bytes: []const u8, parsed: ParsedDecimal, keep_digits: usize) ?u128 {
    var index: usize = @intFromBool(bytes[0] == '-' or bytes[0] == '+');
    var consumed: usize = 0;
    var value: u128 = 0;
    const max_before_mul = limit / 10;
    const max_digit = limit % 10;

    while (index < parsed.mantissa_end and consumed < keep_digits) : (index += 1) {
        const byte = bytes[index];
        if (!isDigit(byte)) continue;
        const digit = byte - '0';
        if (value > max_before_mul or (value == max_before_mul and digit > max_digit)) return null;
        value = value * 10 + digit;
        consumed += 1;
    }
    std.debug.assert(consumed == keep_digits);
    return value;
}

fn positiveExponent(parsed: ParsedDecimal) ?usize {
    std.debug.assert(!parsed.exponent_negative);
    if (parsed.exponent_overflow or parsed.exponent_magnitude > 38) return null;
    return @intCast(parsed.exponent_magnitude);
}

/// Parse an exact integer. The whole of `bytes` must be one `.int` token, so
/// decimal points and negative exponents are rejected.
pub fn parseInt(comptime T: type, bytes: []const u8) ?T {
    const info = @typeInfo(T).int;
    const parsed = scan(bytes, .int) orelse return null;

    const zeros = positiveExponent(parsed) orelse {
        if (parsed.isZero()) return 0;
        return null;
    };
    if (parsed.coefficient_overflow) return null;

    const positive_limit: u128 = @intCast(std.math.maxInt(T));
    const magnitude = if (info.signedness == .signed and parsed.negative)
        appendDecimalZeros(positive_limit + 1, parsed.coefficient, zeros) orelse return null
    else
        appendDecimalZeros(positive_limit, parsed.coefficient, zeros) orelse return null;

    if (info.signedness == .unsigned) {
        if (parsed.negative and magnitude != 0) return null;
        return @intCast(magnitude);
    }
    if (!parsed.negative) return @intCast(magnitude);

    const negative_limit = positive_limit + 1;
    if (magnitude == negative_limit) return std.math.minInt(T);
    return -@as(T, @intCast(magnitude));
}

fn scaledMagnitude(comptime limit: u128, bytes: []const u8, parsed: ParsedDecimal, decimal_places: u8) ?u128 {
    if (parsed.isZero()) return 0;
    if (parsed.exponent_overflow) return null;

    const exponent: i128 = if (parsed.exponent_negative)
        -@as(i128, @intCast(parsed.exponent_magnitude))
    else
        @intCast(parsed.exponent_magnitude);
    const scale = exponent - @as(i128, @intCast(parsed.fractional_digits)) + decimal_places;

    if (scale >= 0) {
        if (scale > 38 or parsed.coefficient_overflow) return null;
        return appendDecimalZeros(limit, parsed.coefficient, @intCast(scale));
    }

    const drop: i128 = -scale;
    if (drop > @as(i128, @intCast(parsed.trailing_zero_digits))) return null;
    const drop_digits: usize = @intCast(drop);
    const keep_digits = parsed.coefficient_digits - drop_digits;
    return parseCoefficientPrefix(limit, bytes, parsed, keep_digits);
}

/// Parse a decimal value into a signed i128 scaled by `10^decimal_places`.
/// Values are accepted only when the scaled result is exact and in range.
pub fn parseScaledI128(bytes: []const u8, comptime decimal_places: u8) ?i128 {
    const parsed = scan(bytes, .dec) orelse return null;
    const positive_limit: u128 = @intCast(std.math.maxInt(i128));
    const magnitude = if (parsed.negative)
        scaledMagnitude(positive_limit + 1, bytes, parsed, decimal_places) orelse return null
    else
        scaledMagnitude(positive_limit, bytes, parsed, decimal_places) orelse return null;

    if (!parsed.negative) return @intCast(magnitude);
    if (magnitude == positive_limit + 1) return std.math.minInt(i128);
    return -@as(i128, @intCast(magnitude));
}
