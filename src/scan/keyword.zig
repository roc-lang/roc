const std = @import("std");

pub const TokenTag = enum(u8) {
    ident = 0, // Must be zero so that "no match" on kw check maps to this

    // 2-byte keywords (1-4)
    kw_as = 1,
    kw_or = 2,
    kw_if = 3,
    kw_in = 4,

    // 3-byte keywords (5-8)
    kw_and = 5,
    kw_use = 6,
    kw_for = 7,
    kw_mod = 8,

    // 4-byte keywords (9-10)
    kw_else = 9,
    kw_when = 10,

    // 5-byte keywords (11-12)
    kw_crash = 11,
    kw_while = 12,

    // 6-byte keywords (13-14)
    kw_return = 13,
    kw_expect = 14,

    // 2-byte symbols (20-35)
    symbol_assign = 20, // :=
    symbol_eq = 21, // ==
    symbol_gte = 22, // >=
    symbol_lte = 23, // <=
    symbol_fat_arrow = 24, // =>
    symbol_arrow = 25, // ->
    symbol_back_arrow = 26, // <-
    symbol_dot_dot = 27, // ..
    symbol_dot_paren = 28, // .(
    symbol_dot_brace = 29, // .{
    symbol_slash_slash = 30, // //
    symbol_lshift = 31, // <<
    symbol_rshift = 32, // >>
    symbol_ternary = 33, // ?:
};

pub fn tokenFromKeyword4B(bytes: u32, len: usize) TokenTag {
    const kw_vec: @Vector(4, u32) = @bitCast(kw_4b[len - 2]);
    const input_vec: @Vector(4, u32) = @splat(bytes);
    const match_vec = kw_vec == input_vec;

    const match_mask: u4 = @bitCast(match_vec);

    // If no match (mask == 0), @ctz returns 4 for a u4
    const match_index = @ctz(match_mask);

    // Lookup table for base offsets
    const offset_lut = [_]u8{ 1, 5, 9 }; // For len 2, 3, 4 respectively
    const base_offset = offset_lut[len - 2];

    // If match_index == 4, there was no match, return 0 (ident)
    // Otherwise return base_offset + match_index
    const has_match = @as(u8, @intFromBool(match_index != 4));
    const result = (base_offset + match_index) * has_match;
    return @enumFromInt(result);
}

pub fn tokenFromSymbol2B(bytes: u16) TokenTag {
    const input_vec: @Vector(16, u16) = @splat(bytes);
    const symbol_vec: @Vector(16, u16) = symbol_2b;
    const match_vec = symbol_vec == input_vec;

    const match_mask: u16 = @bitCast(match_vec);

    // If no match (mask == 0), @ctz returns 16 for a u16
    const match_index = @ctz(match_mask);

    // If match_index == 16, there was no match, return 0 (ident)
    // Otherwise return 20 + match_index
    const has_match = @as(u8, @intFromBool(match_index != 16));
    const result = (20 + @as(u8, @intCast(match_index))) * has_match;
    return @enumFromInt(result);
}

pub fn tokenFromKeyword5B(first_4b: u32, fifth_byte: u8) TokenTag {
    const is_crash = @intFromBool(first_4b == str_to_u32("cras") and fifth_byte == 'h');
    const is_while = @intFromBool(first_4b == str_to_u32("whil") and fifth_byte == 'e');

    // crash = 11, while = 12, ident = 0
    const result = (@as(u8, is_crash) * 11) | (@as(u8, is_while) * 12);
    return @enumFromInt(result);
}

pub fn tokenFromKeyword6B(first_4b: u32, last_bytes: u16) TokenTag {
    const return_bytes = str_to_u32("retu");
    const expect_bytes = str_to_u32("expe");
    const rn_bytes = (@as(u16, 'r') << 8) | @as(u16, 'n');
    const ct_bytes = (@as(u16, 'c') << 8) | @as(u16, 't');

    const is_return = @intFromBool(first_4b == return_bytes and last_bytes == rn_bytes);
    const is_expect = @intFromBool(first_4b == expect_bytes and last_bytes == ct_bytes);

    // return = 13, expect = 14, ident = 0
    const result = (@as(u8, is_return) * 13) | (@as(u8, is_expect) * 14);
    return @enumFromInt(result);
}

// This lets us index branchlessly into this by length of the input ident,
// and then do one 128-bit SIMD comparison to see which of the 2-4B keywords matched.
const kw_4b: [3]u128 = .{
    pack4xU32(kw_2b_str),
    pack4xU32(kw_3b_str),
    pack4xU32(kw_4b_str),
};

const kw_2b_str = .{
    "as\u{0}\u{0}",
    "or\u{0}\u{0}",
    "if\u{0}\u{0}",
    "in\u{0}\u{0}",
};

const kw_3b_str = .{
    "and\u{0}",
    "use\u{0}",
    "for\u{0}",
    "mod\u{0}",
};

const kw_4b_str = .{
    "else",
    "when",
    "    ", // currently unused - will never match
    "    ", // currently unused - will never match
};

const symbol_2b: [16]u16 = packU16(symbol_2b_str);

const symbol_2b_str = .{
    ":=",
    "==",
    ">=",
    "<=",
    "=>",
    "->",
    "<-",
    "..",
    ".(",
    ".{",
    "//",
    "<<",
    ">>",
    "?:",
    "  ", // currently unused - will never match
    "  ", // currently unused - will never match
};

const kw_5b = .{
    "crash", // We compare the first 4B using u32, and then see if the 5th byte was 'h'
    "while", // We compare the first 4B using u32, and then see if the 5th byte was 'e'
};

const kw_6b = .{
    "return\u{0}\u{0}",
    "expect\u{0}\u{0}",
};

fn str_to_u32(comptime s: *const [4]u8) u32 {
    return (@as(u32, s[0]) << 0) |
        (@as(u32, s[1]) << 8) |
        (@as(u32, s[2]) << 16) |
        (@as(u32, s[3]) << 24);
}

fn pack4xU32(comptime strings: anytype) u128 {
    const packed_0 = str_to_u32(strings[0]);
    const packed_1 = str_to_u32(strings[1]);
    const packed_2 = str_to_u32(strings[2]);
    const packed_3 = str_to_u32(strings[3]);

    return (@as(u128, packed_0) << 0) |
        (@as(u128, packed_1) << 32) |
        (@as(u128, packed_2) << 64) |
        (@as(u128, packed_3) << 96);
}

fn packU16(comptime strings: anytype) [16]u16 {
    var answer: [16]u16 = undefined;

    inline for (strings, 0..) |s, i| {
        if (s.len != 2) {
            @compileError("All strings in the array given to packU16 must be 2 bytes long.");
        }

        answer[i] = (@as(u16, @intCast(s[0])) << 8) | @as(u16, @intCast(s[1]));
    }

    return answer;
}

test "packU16" {
    const expected = [_]u16{
        0x3a3d, // :=
        0x3d3d, // ==
        0x3e3d, // >=
        0x3c3d, // <=
        0x3d3e, // =>
        0x2d3e, // ->
        0x3c2d, // <-
        0x2e2e, // ..
        0x2e28, // .(
        0x2e7b, // .{
        0x2f2f, // //
        0x3c3c, // <<
        0x3e3e, // >>
        0x3f3a, // ?:
        0x2020, // (spaces)
        0x2020, // (spaces)
    };

    const actual = packU16(symbol_2b_str);

    try std.testing.expectEqual(expected, actual);
}

test "tokenFromKeyword4B" {
    // Test 2-byte keywords
    try std.testing.expectEqual(TokenTag.kw_as, tokenFromKeyword4B(str_to_u32("as\x00\x00"), 2));
    try std.testing.expectEqual(TokenTag.kw_or, tokenFromKeyword4B(str_to_u32("or\x00\x00"), 2));
    try std.testing.expectEqual(TokenTag.kw_if, tokenFromKeyword4B(str_to_u32("if\x00\x00"), 2));
    try std.testing.expectEqual(TokenTag.kw_in, tokenFromKeyword4B(str_to_u32("in\x00\x00"), 2));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword4B(str_to_u32("xx\x00\x00"), 2));

    // Test 3-byte keywords
    try std.testing.expectEqual(TokenTag.kw_and, tokenFromKeyword4B(str_to_u32("and\x00"), 3));
    try std.testing.expectEqual(TokenTag.kw_use, tokenFromKeyword4B(str_to_u32("use\x00"), 3));
    try std.testing.expectEqual(TokenTag.kw_for, tokenFromKeyword4B(str_to_u32("for\x00"), 3));
    try std.testing.expectEqual(TokenTag.kw_mod, tokenFromKeyword4B(str_to_u32("mod\x00"), 3));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword4B(str_to_u32("xxx\x00"), 3));

    // Test 4-byte keywords
    try std.testing.expectEqual(TokenTag.kw_else, tokenFromKeyword4B(str_to_u32("else"), 4));
    try std.testing.expectEqual(TokenTag.kw_when, tokenFromKeyword4B(str_to_u32("when"), 4));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword4B(str_to_u32("xxxx"), 4));
}

test "tokenFromSymbol2B" {
    try std.testing.expectEqual(TokenTag.symbol_assign, tokenFromSymbol2B(0x3a3d)); // :=
    try std.testing.expectEqual(TokenTag.symbol_eq, tokenFromSymbol2B(0x3d3d)); // ==
    try std.testing.expectEqual(TokenTag.symbol_gte, tokenFromSymbol2B(0x3e3d)); // >=
    try std.testing.expectEqual(TokenTag.symbol_lte, tokenFromSymbol2B(0x3c3d)); // <=
    try std.testing.expectEqual(TokenTag.symbol_fat_arrow, tokenFromSymbol2B(0x3d3e)); // =>
    try std.testing.expectEqual(TokenTag.symbol_arrow, tokenFromSymbol2B(0x2d3e)); // ->
    try std.testing.expectEqual(TokenTag.symbol_back_arrow, tokenFromSymbol2B(0x3c2d)); // <-
    try std.testing.expectEqual(TokenTag.symbol_dot_dot, tokenFromSymbol2B(0x2e2e)); // ..
    try std.testing.expectEqual(TokenTag.symbol_dot_paren, tokenFromSymbol2B(0x2e28)); // .(
    try std.testing.expectEqual(TokenTag.symbol_dot_brace, tokenFromSymbol2B(0x2e7b)); // .{
    try std.testing.expectEqual(TokenTag.symbol_slash_slash, tokenFromSymbol2B(0x2f2f)); // //
    try std.testing.expectEqual(TokenTag.symbol_lshift, tokenFromSymbol2B(0x3c3c)); // <<
    try std.testing.expectEqual(TokenTag.symbol_rshift, tokenFromSymbol2B(0x3e3e)); // >>
    try std.testing.expectEqual(TokenTag.symbol_ternary, tokenFromSymbol2B(0x3f3a)); // ?:
    try std.testing.expectEqual(TokenTag.ident, tokenFromSymbol2B(0x9999)); // no match
}

test "tokenFromKeyword5B" {
    try std.testing.expectEqual(TokenTag.kw_crash, tokenFromKeyword5B(str_to_u32("cras"), 'h'));
    try std.testing.expectEqual(TokenTag.kw_while, tokenFromKeyword5B(str_to_u32("whil"), 'e'));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword5B(str_to_u32("cras"), 'x'));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword5B(str_to_u32("whil"), 'x'));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword5B(str_to_u32("xxxx"), 'x'));
}

test "tokenFromKeyword6B" {
    try std.testing.expectEqual(TokenTag.kw_return, tokenFromKeyword6B(str_to_u32("retu"), (@as(u16, 'r') << 8) | 'n'));
    try std.testing.expectEqual(TokenTag.kw_expect, tokenFromKeyword6B(str_to_u32("expe"), (@as(u16, 'c') << 8) | 't'));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword6B(str_to_u32("retu"), (@as(u16, 'x') << 8) | 'x'));
    try std.testing.expectEqual(TokenTag.ident, tokenFromKeyword6B(str_to_u32("xxxx"), (@as(u16, 'x') << 8) | 'x'));
}
