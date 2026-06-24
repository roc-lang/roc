//! Parser-owned numeric literal data.
//!
//! Numeric syntax is interpreted here, before canonicalization. Later stages
//! consume these facts; they must not parse number token text again.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Index of a parsed numeric literal stored in `NodeStore.numeric_literals`.
pub const Idx = enum(u32) { _ };

/// Source-level numeric token category.
pub const Kind = enum(u8) {
    int,
    frac,
};

/// Deprecated compact suffixes such as `u64` and `dec`.
pub const DeprecatedSuffix = enum(u8) {
    none,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,

    /// Return the modern type name for this deprecated suffix.
    pub fn newTypeName(self: DeprecatedSuffix) ?[]const u8 {
        return switch (self) {
            .none => null,
            .u8 => "U8",
            .i8 => "I8",
            .u16 => "U16",
            .i16 => "I16",
            .u32 => "U32",
            .i32 => "I32",
            .u64 => "U64",
            .i64 => "I64",
            .u128 => "U128",
            .i128 => "I128",
            .f32 => "F32",
            .f64 => "F64",
            .dec => "Dec",
        };
    }

    /// Return the deprecated suffix text as it appears in source.
    pub fn oldText(self: DeprecatedSuffix) ?[]const u8 {
        return switch (self) {
            .none => null,
            .u8 => "u8",
            .i8 => "i8",
            .u16 => "u16",
            .i16 => "i16",
            .u32 => "u32",
            .i32 => "i32",
            .u64 => "u64",
            .i64 => "i64",
            .u128 => "u128",
            .i128 => "i128",
            .f32 => "f32",
            .f64 => "f64",
            .dec => "dec",
        };
    }
};

/// Compact exact integer payload for literals that fit in 128 bits.
pub const IntValue = struct {
    bytes: [16]u8,
    kind: IntKind,

    /// Whether the compact integer payload should be interpreted as signed or unsigned.
    pub const IntKind = enum(u8) {
        i128,
        u128,
    };

    /// Interpret the raw bytes as an `i128`.
    pub fn toI128(self: IntValue) i128 {
        return @bitCast(self.bytes);
    }
};

/// Compact decimal payload for small fractional literals.
pub const SmallDecValue = struct {
    numerator: i16,
    denominator_power_of_ten: u8,
};

/// Memory-efficient parsed payload used when the literal fits a common case.
pub const Compact = union(enum) {
    int: IntValue,
    small_dec: SmallDecValue,
    dec: i128,
    exact,
    invalid,
};

/// Numeric literal facts stored in `NodeStore`.
pub const Stored = struct {
    kind: Kind,
    compact: Compact,
    digits_start: u32,
    before_len: u32,
    after_len: u32,
    after_decimal_digit_count: u32,
    flags: Flags,

    /// Boolean facts packed into a byte for storage.
    pub const Flags = packed struct(u8) {
        is_negative: bool = false,
        had_decimal_point: bool = false,
        _padding: u6 = 0,
    };

    /// Return whether the literal has a leading minus sign.
    pub fn isNegative(self: Stored) bool {
        return self.flags.is_negative;
    }
};

/// Parsed numeric literal with owned base-256 digit buffers.
pub const Owned = struct {
    kind: Kind,
    compact: Compact,
    before: []u8,
    after: []u8,
    after_decimal_digit_count: u32,
    is_negative: bool,
    had_decimal_point: bool,

    /// Free owned digit buffers.
    pub fn deinit(self: Owned, allocator: std.mem.Allocator) void {
        allocator.free(self.before);
        allocator.free(self.after);
    }
};

const Split = struct {
    number_text: []const u8,
    deprecated_suffix: DeprecatedSuffix,
};

/// Source split for a token that used a deprecated numeric suffix.
pub const DeprecatedSuffixSource = struct {
    number_text: []const u8,
    deprecated_suffix: DeprecatedSuffix,
    deprecated_suffix_text: []const u8,
};

const DecimalParts = struct {
    before: []u8,
    after: []u8,

    fn deinit(self: DecimalParts, allocator: std.mem.Allocator) void {
        allocator.free(self.before);
        allocator.free(self.after);
    }
};

const Exact = struct {
    before: []u8,
    after: []u8,
    after_decimal_digit_count: u32,
    is_negative: bool,
    had_decimal_point: bool,

    fn deinit(self: Exact, allocator: std.mem.Allocator) void {
        allocator.free(self.before);
        allocator.free(self.after);
    }
};

/// Parse a numeric token into exact base-256 digit facts and a compact payload when possible.
pub fn parse(allocator: std.mem.Allocator, raw_text: []const u8, kind: Kind) std.mem.Allocator.Error!Owned {
    const split = splitDeprecatedSuffix(raw_text, kind);
    const exact = parseExact(allocator, split.number_text, kind) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidNumeral => return .{
            .kind = kind,
            .compact = .invalid,
            .before = try allocator.alloc(u8, 0),
            .after = try allocator.alloc(u8, 0),
            .after_decimal_digit_count = 0,
            .is_negative = false,
            .had_decimal_point = kind == .frac,
        },
    };
    errdefer exact.deinit(allocator);

    const compact = switch (kind) {
        .int => compactInt(split.number_text) orelse .exact,
        .frac => try compactFrac(allocator, split.number_text),
    };

    return .{
        .kind = kind,
        .compact = compact,
        .before = exact.before,
        .after = exact.after,
        .after_decimal_digit_count = exact.after_decimal_digit_count,
        .is_negative = exact.is_negative,
        .had_decimal_point = exact.had_decimal_point,
    };
}

fn splitDeprecatedSuffix(text: []const u8, kind: Kind) Split {
    const end = numberTextEnd(text, kind);
    if (end >= text.len) return .{ .number_text = text, .deprecated_suffix = .none };
    return .{ .number_text = text[0..end], .deprecated_suffix = deprecatedSuffixFromText(text[end..]) };
}

/// Return the deprecated suffix represented by `text`, or `.none`.
pub fn deprecatedSuffixFromText(text: []const u8) DeprecatedSuffix {
    if (std.mem.eql(u8, text, "u8")) return .u8;
    if (std.mem.eql(u8, text, "i8")) return .i8;
    if (std.mem.eql(u8, text, "u16")) return .u16;
    if (std.mem.eql(u8, text, "i16")) return .i16;
    if (std.mem.eql(u8, text, "u32")) return .u32;
    if (std.mem.eql(u8, text, "i32")) return .i32;
    if (std.mem.eql(u8, text, "u64")) return .u64;
    if (std.mem.eql(u8, text, "i64")) return .i64;
    if (std.mem.eql(u8, text, "u128")) return .u128;
    if (std.mem.eql(u8, text, "i128")) return .i128;
    if (std.mem.eql(u8, text, "f32")) return .f32;
    if (std.mem.eql(u8, text, "f64")) return .f64;
    if (std.mem.eql(u8, text, "dec")) return .dec;
    return .none;
}

/// Split deprecated numeric suffix text from a raw source token.
pub fn deprecatedSuffixFromSource(text: []const u8) DeprecatedSuffixSource {
    const int_end = numberTextEnd(text, .int);
    if (int_end < text.len) {
        const suffix = deprecatedSuffixFromText(text[int_end..]);
        if (suffix != .none) {
            return .{
                .number_text = text[0..int_end],
                .deprecated_suffix = suffix,
                .deprecated_suffix_text = text[int_end..],
            };
        }
    }

    const frac_end = numberTextEnd(text, .frac);
    if (frac_end < text.len) {
        const suffix = deprecatedSuffixFromText(text[frac_end..]);
        if (suffix != .none) {
            return .{
                .number_text = text[0..frac_end],
                .deprecated_suffix = suffix,
                .deprecated_suffix_text = text[frac_end..],
            };
        }
    }

    return .{
        .number_text = text,
        .deprecated_suffix = .none,
        .deprecated_suffix_text = "",
    };
}

fn numberTextEnd(text: []const u8, kind: Kind) usize {
    var index: usize = if (text.len > 0 and text[0] == '-') 1 else 0;
    if (index >= text.len) return text.len;

    if (text[index] == '0' and index + 1 < text.len) {
        const maybe_base = text[index + 1];
        if (maybe_base == 'x' or maybe_base == 'X' or maybe_base == 'o' or maybe_base == 'O' or maybe_base == 'b' or maybe_base == 'B') {
            const radix: u8 = switch (maybe_base) {
                'x', 'X' => 16,
                'o', 'O' => 8,
                'b', 'B' => 2,
                else => unreachable,
            };
            index += 2;
            while (index < text.len) : (index += 1) {
                const digit = digitValue(text[index]) orelse {
                    if (text[index] == '_') continue;
                    break;
                };
                if (digit >= radix) break;
            }
            return index;
        }
    }

    while (index < text.len and isDecDigitOrUnderscore(text[index])) : (index += 1) {}

    if (kind == .frac and index < text.len and text[index] == '.') {
        index += 1;
        while (index < text.len and isDecDigitOrUnderscore(text[index])) : (index += 1) {}
    }

    if (kind == .frac and index < text.len and (text[index] == 'e' or text[index] == 'E')) {
        const exponent_start = index;
        index += 1;
        if (index < text.len and (text[index] == '+' or text[index] == '-')) index += 1;
        const digits_start = index;
        while (index < text.len and isDecDigitOrUnderscore(text[index])) : (index += 1) {}
        if (digits_start == index) return exponent_start;
    }

    return index;
}

fn isDecDigitOrUnderscore(byte: u8) bool {
    return (byte >= '0' and byte <= '9') or byte == '_';
}

fn digitValue(byte: u8) ?u8 {
    return switch (byte) {
        '0'...'9' => byte - '0',
        'a'...'f' => byte - 'a' + 10,
        'A'...'F' => byte - 'A' + 10,
        else => null,
    };
}

fn parseExact(allocator: std.mem.Allocator, text: []const u8, kind: Kind) (Allocator.Error || error{InvalidNumeral})!Exact {
    return switch (kind) {
        .int => parseExactInteger(allocator, text),
        .frac => parseExactDecimal(allocator, text),
    };
}

fn parseExactInteger(allocator: std.mem.Allocator, text: []const u8) (Allocator.Error || error{InvalidNumeral})!Exact {
    if (text.len == 0) return error.InvalidNumeral;
    const is_negative = text[0] == '-';
    var first_digit: usize = @intFromBool(is_negative);
    if (first_digit >= text.len) return error.InvalidNumeral;

    var radix: u8 = 10;
    if (text[first_digit] == '0' and first_digit + 1 < text.len) {
        switch (text[first_digit + 1]) {
            'x', 'X' => {
                radix = 16;
                first_digit += 2;
            },
            'o', 'O' => {
                radix = 8;
                first_digit += 2;
            },
            'b', 'B' => {
                radix = 2;
                first_digit += 2;
            },
            else => {},
        }
    }
    if (first_digit >= text.len) return error.InvalidNumeral;

    const before = try intDigitsToBase256(allocator, trimLeadingZeros(text[first_digit..]), radix);
    errdefer allocator.free(before);
    const after = try allocator.alloc(u8, 0);

    return .{
        .before = before,
        .after = after,
        .after_decimal_digit_count = 0,
        .is_negative = is_negative,
        .had_decimal_point = false,
    };
}

fn parseExactDecimal(allocator: std.mem.Allocator, text: []const u8) (Allocator.Error || error{InvalidNumeral})!Exact {
    if (text.len == 0) return error.InvalidNumeral;
    const is_negative = text[0] == '-';
    const first: usize = @intFromBool(is_negative);
    if (first >= text.len) return error.InvalidNumeral;
    const had_decimal_point = std.mem.findScalar(u8, text[first..], '.') != null;

    const parts = try decimalParts(allocator, text[first..]);
    defer parts.deinit(allocator);

    const after_decimal_digit_count = std.math.cast(u32, parts.after.len) orelse return error.InvalidNumeral;

    const before = try decimalDigitsToBase256(allocator, trimLeadingZeros(parts.before));
    errdefer allocator.free(before);
    const after = try decimalDigitsToBase256(allocator, parts.after);
    errdefer allocator.free(after);

    return .{
        .before = before,
        .after = after,
        .after_decimal_digit_count = after_decimal_digit_count,
        .is_negative = is_negative,
        .had_decimal_point = had_decimal_point,
    };
}

fn decimalParts(allocator: std.mem.Allocator, unsigned_text: []const u8) (Allocator.Error || error{InvalidNumeral})!DecimalParts {
    const exp_index = std.mem.findAny(u8, unsigned_text, "eE");
    const mantissa = unsigned_text[0..(exp_index orelse unsigned_text.len)];
    const exponent: i64 = if (exp_index) |index| blk: {
        const exp_text = unsigned_text[index + 1 ..];
        if (exp_text.len == 0) return error.InvalidNumeral;
        break :blk parseI64NoUnderscores(allocator, exp_text) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidCharacter, error.Overflow => return error.InvalidNumeral,
        };
    } else 0;

    var before_raw = std.ArrayList(u8).empty;
    defer before_raw.deinit(allocator);
    var after_raw = std.ArrayList(u8).empty;
    defer after_raw.deinit(allocator);

    var saw_point = false;
    var saw_digit = false;
    for (mantissa) |byte| {
        switch (byte) {
            '_' => {},
            '.' => {
                if (saw_point) return error.InvalidNumeral;
                saw_point = true;
            },
            '0'...'9' => {
                saw_digit = true;
                if (saw_point) {
                    try after_raw.append(allocator, byte);
                } else {
                    try before_raw.append(allocator, byte);
                }
            },
            else => return error.InvalidNumeral,
        }
    }
    if (!saw_digit) return error.InvalidNumeral;

    if (exponent >= 0) {
        const exponent_usize = std.math.cast(usize, exponent) orelse return error.InvalidNumeral;
        const move_count: usize = @min(exponent_usize, after_raw.items.len);
        const zeros_count: usize = exponent_usize - move_count;
        const before_prefix_len = checkedAddUsize(before_raw.items.len, move_count) orelse return error.InvalidNumeral;
        const before_len = checkedAddUsize(before_prefix_len, zeros_count) orelse return error.InvalidNumeral;

        var before = try allocator.alloc(u8, before_len);
        errdefer allocator.free(before);
        @memcpy(before[0..before_raw.items.len], before_raw.items);
        @memcpy(before[before_raw.items.len..][0..move_count], after_raw.items[0..move_count]);
        @memset(before[before_prefix_len..], '0');

        const after = try allocator.dupe(u8, after_raw.items[move_count..]);
        errdefer allocator.free(after);
        return .{ .before = before, .after = after };
    }

    const move_left = std.math.cast(usize, -@as(i128, exponent)) orelse return error.InvalidNumeral;
    if (move_left <= before_raw.items.len) {
        const split = before_raw.items.len - move_left;
        const before = try allocator.dupe(u8, before_raw.items[0..split]);
        errdefer allocator.free(before);
        const after_len = checkedAfterDecimalLength(move_left, after_raw.items.len) orelse return error.InvalidNumeral;
        var after = try allocator.alloc(u8, after_len);
        errdefer allocator.free(after);
        @memcpy(after[0..move_left], before_raw.items[split..]);
        @memcpy(after[move_left..], after_raw.items);
        return .{ .before = before, .after = after };
    }

    const zeros_count = move_left - before_raw.items.len;
    const before = try allocator.alloc(u8, 0);
    errdefer allocator.free(before);
    const after_prefix_len = checkedAddUsize(zeros_count, before_raw.items.len) orelse return error.InvalidNumeral;
    const after_len = checkedAfterDecimalLength(after_prefix_len, after_raw.items.len) orelse return error.InvalidNumeral;
    var after = try allocator.alloc(u8, after_len);
    errdefer allocator.free(after);
    @memset(after[0..zeros_count], '0');
    @memcpy(after[zeros_count..][0..before_raw.items.len], before_raw.items);
    @memcpy(after[after_prefix_len..], after_raw.items);
    return .{ .before = before, .after = after };
}

fn compactInt(text: []const u8) ?Compact {
    if (text.len == 0) return null;
    const is_negative = text[0] == '-';
    var first_digit: usize = @intFromBool(is_negative);
    if (first_digit >= text.len) return null;

    var radix: u8 = 10;
    if (text[first_digit] == '0' and first_digit + 1 < text.len) {
        switch (text[first_digit + 1]) {
            'x', 'X' => {
                radix = 16;
                first_digit += 2;
            },
            'o', 'O' => {
                radix = 8;
                first_digit += 2;
            },
            'b', 'B' => {
                radix = 2;
                first_digit += 2;
            },
            else => {},
        }
    }
    if (first_digit >= text.len) return null;

    const magnitude = parseUnsignedMagnitude(text[first_digit..], radix) orelse return null;
    if (is_negative) {
        const min_i128_magnitude = @as(u128, @intCast(std.math.maxInt(i128))) + 1;
        if (magnitude > min_i128_magnitude) return null;
        const value: i128 = if (magnitude == min_i128_magnitude)
            std.math.minInt(i128)
        else
            -@as(i128, @intCast(magnitude));
        return .{ .int = .{ .bytes = @bitCast(value), .kind = .i128 } };
    }

    if (magnitude <= @as(u128, @intCast(std.math.maxInt(i128)))) {
        return .{ .int = .{ .bytes = @bitCast(@as(i128, @intCast(magnitude))), .kind = .i128 } };
    }
    return .{ .int = .{ .bytes = @bitCast(magnitude), .kind = .u128 } };
}

fn parseUnsignedMagnitude(digits: []const u8, radix: u8) ?u128 {
    var value: u128 = 0;
    var saw_digit = false;
    for (digits) |byte| {
        if (byte == '_') continue;
        const digit = digitValue(byte) orelse return null;
        if (digit >= radix) return null;
        saw_digit = true;

        const multiplied = @mulWithOverflow(value, radix);
        if (multiplied[1] != 0) return null;
        const added = @addWithOverflow(multiplied[0], digit);
        if (added[1] != 0) return null;
        value = added[0];
    }
    if (!saw_digit) return null;
    return value;
}

fn compactFrac(
    allocator: std.mem.Allocator,
    text: []const u8,
) std.mem.Allocator.Error!Compact {
    const parts = decimalParts(allocator, text[@intFromBool(text.len > 0 and text[0] == '-')..]) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidNumeral => return .invalid,
    };
    defer parts.deinit(allocator);

    const is_negative = text.len > 0 and text[0] == '-';
    const trimmed_before = trimLeadingZeros(parts.before);
    const trimmed_after = trimTrailingZeros(parts.after);
    const trimmed_after_count = std.math.cast(u32, trimmed_after.len) orelse return .exact;

    if (smallDecFromParts(trimmed_before, trimmed_after, trimmed_after_count, is_negative)) |small| {
        return .{ .small_dec = small };
    }

    if (decFromParts(trimmed_before, trimmed_after, trimmed_after_count, is_negative)) |dec| {
        return .{ .dec = dec };
    }

    return .exact;
}

fn smallDecFromParts(before: []const u8, after: []const u8, after_count: u32, is_negative: bool) ?SmallDecValue {
    if (after_count > std.math.maxInt(u8)) return null;
    var magnitude: u32 = 0;
    var saw_digit = false;
    for (before) |byte| {
        if (byte < '0' or byte > '9') return null;
        saw_digit = true;
        magnitude = checkedMulAdd(u32, magnitude, 10, byte - '0') orelse return null;
        if (magnitude > @as(u32, @intCast(std.math.maxInt(i16))) + @intFromBool(is_negative)) return null;
    }
    for (after) |byte| {
        if (byte < '0' or byte > '9') return null;
        saw_digit = true;
        magnitude = checkedMulAdd(u32, magnitude, 10, byte - '0') orelse return null;
        if (magnitude > @as(u32, @intCast(std.math.maxInt(i16))) + @intFromBool(is_negative)) return null;
    }
    if (!saw_digit) magnitude = 0;

    const signed: i32 = if (is_negative) -@as(i32, @intCast(magnitude)) else @intCast(magnitude);
    if (signed < std.math.minInt(i16) or signed > std.math.maxInt(i16)) return null;
    return .{
        .numerator = @intCast(signed),
        .denominator_power_of_ten = @intCast(after_count),
    };
}

fn decFromParts(before: []const u8, after: []const u8, after_count: u32, is_negative: bool) ?i128 {
    if (after_count > 18) return null;
    var magnitude: u128 = 0;
    var saw_digit = false;
    for (before) |byte| {
        if (byte < '0' or byte > '9') return null;
        saw_digit = true;
        magnitude = checkedMulAdd(u128, magnitude, 10, byte - '0') orelse return null;
    }
    for (after) |byte| {
        if (byte < '0' or byte > '9') return null;
        saw_digit = true;
        magnitude = checkedMulAdd(u128, magnitude, 10, byte - '0') orelse return null;
    }
    if (!saw_digit) magnitude = 0;

    var scale_count: u32 = 18 - after_count;
    while (scale_count > 0) : (scale_count -= 1) {
        const multiplied = @mulWithOverflow(magnitude, 10);
        if (multiplied[1] != 0) return null;
        magnitude = multiplied[0];
    }

    const max_positive: u128 = @intCast(std.math.maxInt(i128));
    const max_negative = max_positive + 1;
    const limit = if (is_negative) max_negative else max_positive;
    if (magnitude > limit) return null;
    if (is_negative) {
        if (magnitude == max_negative) return std.math.minInt(i128);
        return -@as(i128, @intCast(magnitude));
    }
    return @intCast(magnitude);
}

fn checkedMulAdd(comptime T: type, value: T, multiplier: T, addend: u8) ?T {
    const multiplied = @mulWithOverflow(value, multiplier);
    if (multiplied[1] != 0) return null;
    const added = @addWithOverflow(multiplied[0], @as(T, @intCast(addend)));
    if (added[1] != 0) return null;
    return added[0];
}

fn checkedAddUsize(lhs: usize, rhs: usize) ?usize {
    const added = @addWithOverflow(lhs, rhs);
    if (added[1] != 0) return null;
    return added[0];
}

fn checkedAfterDecimalLength(lhs: usize, rhs: usize) ?usize {
    const len = checkedAddUsize(lhs, rhs) orelse return null;
    if (len > std.math.maxInt(u32)) return null;
    return len;
}

fn parseI64NoUnderscores(allocator: std.mem.Allocator, text: []const u8) (Allocator.Error || error{ InvalidCharacter, Overflow })!i64 {
    const cleaned = try withoutUnderscores(allocator, text);
    defer allocator.free(cleaned);
    return try std.fmt.parseInt(i64, cleaned, 10);
}

fn withoutUnderscores(allocator: std.mem.Allocator, text: []const u8) std.mem.Allocator.Error![]u8 {
    var count = text.len;
    for (text) |byte| {
        if (byte == '_') count -= 1;
    }
    const out = try allocator.alloc(u8, count);
    var index: usize = 0;
    for (text) |byte| {
        if (byte == '_') continue;
        out[index] = byte;
        index += 1;
    }
    return out;
}

fn intDigitsToBase256(allocator: std.mem.Allocator, digits: []const u8, radix: u8) (Allocator.Error || error{InvalidNumeral})![]u8 {
    var bytes_le = std.ArrayList(u8).empty;
    defer bytes_le.deinit(allocator);

    for (digits) |byte| {
        if (byte == '_') continue;
        const digit = digitValue(byte) orelse return error.InvalidNumeral;
        if (digit >= radix) return error.InvalidNumeral;
        try appendRadixDigit(&bytes_le, allocator, radix, digit);
    }

    return bytesLeToBe(allocator, bytes_le.items);
}

fn decimalDigitsToBase256(allocator: std.mem.Allocator, digits: []const u8) (Allocator.Error || error{InvalidNumeral})![]u8 {
    if (digits.len == 0) return allocator.alloc(u8, 0);

    var limbs_le = std.ArrayList(u32).empty;
    defer limbs_le.deinit(allocator);

    var index: usize = 0;
    var chunk_len: usize = ((digits.len - 1) % decimal_chunk_digit_count) + 1;
    while (index < digits.len) {
        const chunk_digits = digits[index..][0..chunk_len];
        const chunk_value = try decimalChunkValue(chunk_digits);
        try appendDecimalChunk(&limbs_le, allocator, decimal_chunk_bases[chunk_len], chunk_value);
        index += chunk_len;
        chunk_len = decimal_chunk_digit_count;
    }

    return limbsLeToBytesBe(allocator, limbs_le.items);
}

const decimal_chunk_digit_count = 9;
const decimal_chunk_bases = [_]u32{
    1,
    10,
    100,
    1_000,
    10_000,
    100_000,
    1_000_000,
    10_000_000,
    100_000_000,
    1_000_000_000,
};

fn decimalChunkValue(digits: []const u8) error{InvalidNumeral}!u32 {
    std.debug.assert(digits.len > 0 and digits.len <= decimal_chunk_digit_count);

    var value: u32 = 0;
    for (digits) |byte| {
        if (byte < '0' or byte > '9') return error.InvalidNumeral;
        value = value * 10 + (byte - '0');
    }
    return value;
}

fn appendDecimalChunk(limbs_le: *std.ArrayList(u32), allocator: std.mem.Allocator, base: u32, chunk: u32) std.mem.Allocator.Error!void {
    var carry: u64 = chunk;
    for (limbs_le.items) |*limb| {
        const product = @as(u64, limb.*) * @as(u64, base) + carry;
        limb.* = @truncate(product);
        carry = product >> 32;
    }
    while (carry != 0) {
        try limbs_le.append(allocator, @truncate(carry));
        carry >>= 32;
    }
}

fn limbsLeToBytesBe(allocator: std.mem.Allocator, limbs_le: []const u32) std.mem.Allocator.Error![]u8 {
    const bytes_len = limbs_le.len * @sizeOf(u32);
    const bytes_le = try allocator.alloc(u8, bytes_len);
    defer allocator.free(bytes_le);

    for (limbs_le, 0..) |limb, index| {
        const offset = index * @sizeOf(u32);
        bytes_le[offset] = @truncate(limb);
        bytes_le[offset + 1] = @truncate(limb >> 8);
        bytes_le[offset + 2] = @truncate(limb >> 16);
        bytes_le[offset + 3] = @truncate(limb >> 24);
    }

    return bytesLeToBe(allocator, bytes_le);
}

fn appendRadixDigit(bytes_le: *std.ArrayList(u8), allocator: std.mem.Allocator, radix: u8, digit: u8) std.mem.Allocator.Error!void {
    var carry: u16 = digit;
    for (bytes_le.items) |*byte| {
        const next = @as(u16, byte.*) * radix + carry;
        byte.* = @truncate(next);
        carry = next >> 8;
    }
    while (carry != 0) {
        try bytes_le.append(allocator, @truncate(carry));
        carry >>= 8;
    }
}

fn bytesLeToBe(allocator: std.mem.Allocator, bytes_le: []const u8) std.mem.Allocator.Error![]u8 {
    var len = bytes_le.len;
    while (len > 0 and bytes_le[len - 1] == 0) len -= 1;
    const out = try allocator.alloc(u8, len);
    for (out, 0..) |*byte, i| {
        byte.* = bytes_le[len - 1 - i];
    }
    return out;
}

fn trimLeadingZeros(digits: []const u8) []const u8 {
    var start: usize = 0;
    while (start < digits.len and digits[start] == '0') : (start += 1) {}
    return digits[start..];
}

fn trimTrailingZeros(digits: []const u8) []const u8 {
    var end = digits.len;
    while (end > 0 and digits[end - 1] == '0') : (end -= 1) {}
    return digits[0..end];
}

test "deprecated suffix source splits without mistaking hex digits for suffixes" {
    const cases = [_]struct {
        text: []const u8,
        number_text: []const u8,
        suffix: DeprecatedSuffix,
        suffix_text: []const u8,
    }{
        .{ .text = "0xE", .number_text = "0xE", .suffix = .none, .suffix_text = "" },
        .{ .text = "0xf", .number_text = "0xf", .suffix = .none, .suffix_text = "" },
        .{ .text = "0xFFu32", .number_text = "0xFF", .suffix = .u32, .suffix_text = "u32" },
        .{ .text = "0b1010i16", .number_text = "0b1010", .suffix = .i16, .suffix_text = "i16" },
        .{ .text = "3.14f64", .number_text = "3.14", .suffix = .f64, .suffix_text = "f64" },
        .{ .text = "1.25e10dec", .number_text = "1.25e10", .suffix = .dec, .suffix_text = "dec" },
    };

    for (cases) |case| {
        const split = deprecatedSuffixFromSource(case.text);
        try std.testing.expectEqualStrings(case.number_text, split.number_text);
        try std.testing.expectEqual(case.suffix, split.deprecated_suffix);
        try std.testing.expectEqualStrings(case.suffix_text, split.deprecated_suffix_text);
    }
}

test "deprecated suffix text maps only old numeric spellings" {
    const cases = [_]struct {
        text: []const u8,
        suffix: DeprecatedSuffix,
        type_name: ?[]const u8,
    }{
        .{ .text = "u8", .suffix = .u8, .type_name = "U8" },
        .{ .text = "i8", .suffix = .i8, .type_name = "I8" },
        .{ .text = "u16", .suffix = .u16, .type_name = "U16" },
        .{ .text = "i16", .suffix = .i16, .type_name = "I16" },
        .{ .text = "u32", .suffix = .u32, .type_name = "U32" },
        .{ .text = "i32", .suffix = .i32, .type_name = "I32" },
        .{ .text = "u64", .suffix = .u64, .type_name = "U64" },
        .{ .text = "i64", .suffix = .i64, .type_name = "I64" },
        .{ .text = "u128", .suffix = .u128, .type_name = "U128" },
        .{ .text = "i128", .suffix = .i128, .type_name = "I128" },
        .{ .text = "f32", .suffix = .f32, .type_name = "F32" },
        .{ .text = "f64", .suffix = .f64, .type_name = "F64" },
        .{ .text = "dec", .suffix = .dec, .type_name = "Dec" },
        .{ .text = "U64", .suffix = .none, .type_name = null },
        .{ .text = "int", .suffix = .none, .type_name = null },
    };

    for (cases) |case| {
        const suffix = deprecatedSuffixFromText(case.text);
        try std.testing.expectEqual(case.suffix, suffix);
        if (case.type_name) |type_name| {
            try std.testing.expectEqualStrings(type_name, suffix.newTypeName().?);
            try std.testing.expectEqualStrings(case.text, suffix.oldText().?);
        } else {
            try std.testing.expectEqual(@as(?[]const u8, null), suffix.newTypeName());
            try std.testing.expectEqual(@as(?[]const u8, null), suffix.oldText());
        }
    }
}

test "parse normalizes deprecated suffixes to suffix-free numeric data" {
    var literal = try parse(std.testing.allocator, "123u64", .int);
    defer literal.deinit(std.testing.allocator);

    try std.testing.expect(literal.compact == .int);
    try std.testing.expectEqual(@as(i128, 123), literal.compact.int.toI128());
    try std.testing.expectEqualSlices(u8, &.{123}, literal.before);
    try std.testing.expectEqual(@as(usize, 0), literal.after.len);
}

test "parse exact integers keeps underscore-separated base digits" {
    var hex = try parse(std.testing.allocator, "0x12_34_56_78", .int);
    defer hex.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(u8, &.{ 0x12, 0x34, 0x56, 0x78 }, hex.before);

    var binary = try parse(std.testing.allocator, "0b1010_0101", .int);
    defer binary.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(u8, &.{0xa5}, binary.before);

    var octal = try parse(std.testing.allocator, "0o1_777", .int);
    defer octal.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(u8, &.{ 0x03, 0xff }, octal.before);
}

test "parse exact integers stores base-256 chunks" {
    var literal = try parse(std.testing.allocator, "0x0100_ffff", .int);
    defer literal.deinit(std.testing.allocator);

    try std.testing.expect(literal.compact == .int);
    try std.testing.expectEqualSlices(u8, &.{ 0x01, 0x00, 0xff, 0xff }, literal.before);
    try std.testing.expectEqual(@as(usize, 0), literal.after.len);
}

test "parse exact fractional chunks retain digits after decimal count" {
    var literal = try parse(std.testing.allocator, "0.0010dec", .frac);
    defer literal.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(u8, &.{}, literal.before);
    try std.testing.expectEqualSlices(u8, &.{10}, literal.after);
    try std.testing.expectEqual(@as(u32, 4), literal.after_decimal_digit_count);
}

test "compact fractional literals trim trailing zero scale consistently" {
    var literal = try parse(std.testing.allocator, "3.0", .frac);
    defer literal.deinit(std.testing.allocator);

    try std.testing.expect(literal.compact == .small_dec);
    try std.testing.expectEqual(@as(i16, 3), literal.compact.small_dec.numerator);
    try std.testing.expectEqual(@as(u8, 0), literal.compact.small_dec.denominator_power_of_ten);
    try std.testing.expectEqualSlices(u8, &.{3}, literal.before);
    try std.testing.expectEqualSlices(u8, &.{}, literal.after);
    try std.testing.expectEqual(@as(u32, 1), literal.after_decimal_digit_count);
}

test "parse fractional literal with minimum i64 exponent is invalid instead of panicking" {
    var literal = try parse(std.testing.allocator, "4e-9223372036854775808", .frac);
    defer literal.deinit(std.testing.allocator);

    try std.testing.expect(literal.compact == .invalid);
    try std.testing.expectEqualSlices(u8, &.{}, literal.before);
    try std.testing.expectEqualSlices(u8, &.{}, literal.after);
    try std.testing.expectEqual(@as(u32, 0), literal.after_decimal_digit_count);
}

test "parse large positive scientific exponent exactly" {
    var literal = try parse(std.testing.allocator, "1E80000", .frac);
    defer literal.deinit(std.testing.allocator);

    try std.testing.expect(literal.compact == .exact);
    try std.testing.expectEqual(@as(u32, 0), literal.after_decimal_digit_count);
    try std.testing.expectEqualSlices(u8, &.{}, literal.after);
    try std.testing.expectEqual(@as(usize, 33220), literal.before.len);
    try std.testing.expect(literal.before[0] != 0);
    for (literal.before[literal.before.len - 10000 ..]) |byte| {
        try std.testing.expectEqual(@as(u8, 0), byte);
    }
}
