//! Allocation-free parsing and exact conversion of decimal text.

const std = @import("std");

const max_u128_before_mul_10 = std.math.maxInt(u128) / 10;
const max_u128_last_digit = std.math.maxInt(u128) % 10;

const ParsedDecimal = struct {
    negative: bool,
    had_decimal_point: bool,
    mantissa_end: usize,
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

/// Parse decimal syntax into exact coefficient-and-scale facts.
/// Underscores are accepted only between decimal digits.
fn scan(bytes: []const u8) ?ParsedDecimal {
    if (bytes.len == 0) return null;

    var index: usize = 0;
    const negative = bytes[index] == '-';
    if (bytes[index] == '-' or bytes[index] == '+') {
        index += 1;
        if (index == bytes.len) return null;
    }

    var had_decimal_point = false;
    var saw_digit = false;
    var previous_was_digit = false;
    var coefficient_digits: usize = 0;
    var fractional_digits: usize = 0;
    var leading_zero_digits: usize = 0;
    var trailing_zero_digits: usize = 0;
    var saw_nonzero = false;
    var coefficient: u128 = 0;
    var coefficient_overflow = false;

    while (index < bytes.len) : (index += 1) {
        const byte = bytes[index];
        switch (byte) {
            '0'...'9' => {
                const digit = byte - '0';
                saw_digit = true;
                previous_was_digit = true;
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
                if (!previous_was_digit or index + 1 == bytes.len or !isDigit(bytes[index + 1])) return null;
                previous_was_digit = false;
            },
            '.' => {
                if (had_decimal_point or (!previous_was_digit and saw_digit)) return null;
                had_decimal_point = true;
                previous_was_digit = false;
            },
            'e', 'E' => break,
            else => return null,
        }
    }

    if (!saw_digit or (index > 0 and bytes[index - 1] == '_')) return null;
    const mantissa_end = index;

    var exponent_negative = false;
    var exponent_magnitude: u64 = 0;
    var exponent_overflow = false;
    if (index < bytes.len) {
        index += 1;
        if (index == bytes.len) return null;
        if (bytes[index] == '-' or bytes[index] == '+') {
            exponent_negative = bytes[index] == '-';
            index += 1;
            if (index == bytes.len) return null;
        }

        var exponent_saw_digit = false;
        previous_was_digit = false;
        while (index < bytes.len) : (index += 1) {
            const byte = bytes[index];
            if (isDigit(byte)) {
                const digit = byte - '0';
                exponent_saw_digit = true;
                previous_was_digit = true;
                if (!exponent_overflow) {
                    if (exponent_magnitude > (std.math.maxInt(u64) - @as(u64, digit)) / 10) {
                        exponent_overflow = true;
                    } else {
                        exponent_magnitude = exponent_magnitude * 10 + digit;
                    }
                }
            } else if (byte == '_') {
                if (!previous_was_digit or index + 1 == bytes.len or !isDigit(bytes[index + 1])) return null;
                previous_was_digit = false;
            } else {
                return null;
            }
        }
        if (!exponent_saw_digit or !previous_was_digit) return null;
    }

    return .{
        .negative = negative,
        .had_decimal_point = had_decimal_point,
        .mantissa_end = mantissa_end,
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
    if (parsed.exponent_negative and (parsed.exponent_overflow or parsed.exponent_magnitude != 0)) return null;
    if (parsed.exponent_overflow or parsed.exponent_magnitude > 38) return null;
    return @intCast(parsed.exponent_magnitude);
}

/// Parse an exact integer. Decimal points and negative effective exponents are
/// rejected, matching Roc numeric-literal integer conversion semantics.
pub fn parseInt(comptime T: type, bytes: []const u8) ?T {
    const info = @typeInfo(T).int;
    const parsed = scan(bytes) orelse return null;
    if (parsed.had_decimal_point) return null;

    const zeros = positiveExponent(parsed) orelse {
        if (!parsed.exponent_negative and parsed.isZero()) return 0;
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
    const parsed = scan(bytes) orelse return null;
    const positive_limit: u128 = @intCast(std.math.maxInt(i128));
    const magnitude = if (parsed.negative)
        scaledMagnitude(positive_limit + 1, bytes, parsed, decimal_places) orelse return null
    else
        scaledMagnitude(positive_limit, bytes, parsed, decimal_places) orelse return null;

    if (!parsed.negative) return @intCast(magnitude);
    if (magnitude == positive_limit + 1) return std.math.minInt(i128);
    return -@as(i128, @intCast(magnitude));
}
