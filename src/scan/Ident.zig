//! A 4-byte representation of an identifier - either an index into a Literals.zig structure
//! (which holds the length), or else stored inline if it can fit. Also stores metadata about
//! whether it representes an unused (starts with `_`), reused (starts with `$`), or effectful
//! (ends in `!`) identifier, so you never have to dereference a pointer to find that out.
const std = @import("std");

const Ident = @This();

data: u32,

// For inline idents: metadata bits are in the sign bits of each byte
const INLINE_BIT_POS = 31;
const INLINE_REUSED_BIT_POS = 23;
const INLINE_UNUSED_BIT_POS = 15;
const INLINE_EFFECTFUL_BIT_POS = 7;

const INLINE_BIT_MASK = @as(u32, 1) << INLINE_BIT_POS;
const INLINE_REUSED_BIT_MASK = @as(u32, 1) << INLINE_REUSED_BIT_POS;
const INLINE_UNUSED_BIT_MASK = @as(u32, 1) << INLINE_UNUSED_BIT_POS;
const INLINE_EFFECTFUL_BIT_MASK = @as(u32, 1) << INLINE_EFFECTFUL_BIT_POS;

// For indexed idents: metadata bits are in bits 28-30
const INDEXED_REUSED_BIT_POS = 30;
const INDEXED_UNUSED_BIT_POS = 29;
const INDEXED_EFFECTFUL_BIT_POS = 28;

const INDEXED_REUSED_BIT_MASK = @as(u32, 1) << INDEXED_REUSED_BIT_POS;
const INDEXED_UNUSED_BIT_MASK = @as(u32, 1) << INDEXED_UNUSED_BIT_POS;
const INDEXED_EFFECTFUL_BIT_MASK = @as(u32, 1) << INDEXED_EFFECTFUL_BIT_POS;

const SIGN_BIT_MASK: u32 = 0x80808080;
const INDEX_MASK: u32 = 0x0FFFFFFF; // Lower 28 bits

/// Init an identifier consisting of 1-4 ASCII bytes - which must be verified ahead of time!
pub fn initSmallAscii(bytes: []const u8, reused: bool, unused: bool, effectful: bool) Ident {
    std.debug.assert(bytes.len >= 1 and bytes.len <= 4);

    var data: u32 = 0;

    // Branchlessly load bytes, using 0 for positions beyond bytes.len
    inline for (0..4) |i| {
        const in_bounds = @intFromBool(i < bytes.len);
        const offset = @min(i, bytes.len - 1);
        const byte = bytes[offset] * in_bounds;
        std.debug.assert(byte < 128);
        data |= @as(u32, byte) << @intCast(i * 8);
    }

    if (reused) data |= INLINE_REUSED_BIT_MASK;
    if (unused) data |= INLINE_UNUSED_BIT_MASK;
    if (effectful) data |= INLINE_EFFECTFUL_BIT_MASK;

    return Ident{ .data = data };
}

pub fn initFromIdx(idx: u28, reused: bool, unused: bool, effectful: bool) Ident {
    std.debug.assert(idx <= INDEX_MASK);

    var data: u32 = idx;

    data |= INLINE_BIT_MASK;
    if (reused) data |= INDEXED_REUSED_BIT_MASK;
    if (unused) data |= INDEXED_UNUSED_BIT_MASK;
    if (effectful) data |= INDEXED_EFFECTFUL_BIT_MASK;

    return Ident{ .data = data };
}

pub fn isInline(self: Ident) bool {
    return (self.data & INLINE_BIT_MASK) == 0;
}

pub fn isReused(self: Ident) bool {
    // Branchlessly select the correct bit based on inline status
    const is_inline = @intFromBool(self.isInline());
    const inline_bit = @intFromBool((self.data & INLINE_REUSED_BIT_MASK) != 0);
    const indexed_bit = @intFromBool((self.data & INDEXED_REUSED_BIT_MASK) != 0);

    // If inline, use inline_bit; otherwise use indexed_bit
    return (inline_bit & is_inline) | (indexed_bit & (1 - is_inline)) != 0;
}

pub fn isUnused(self: Ident) bool {
    // Branchlessly select the correct bit based on inline status
    const is_inline = @intFromBool(self.isInline());
    const inline_bit = @intFromBool((self.data & INLINE_UNUSED_BIT_MASK) != 0);
    const indexed_bit = @intFromBool((self.data & INDEXED_UNUSED_BIT_MASK) != 0);

    // If inline, use inline_bit; otherwise use indexed_bit
    return (inline_bit & is_inline) | (indexed_bit & (1 - is_inline)) != 0;
}

pub fn isEffectful(self: Ident) bool {
    // Branchlessly select the correct bit based on inline status
    const is_inline = @intFromBool(self.isInline());
    const inline_bit = @intFromBool((self.data & INLINE_EFFECTFUL_BIT_MASK) != 0);
    const indexed_bit = @intFromBool((self.data & INDEXED_EFFECTFUL_BIT_MASK) != 0);

    // If inline, use inline_bit; otherwise use indexed_bit
    return (inline_bit & is_inline) | (indexed_bit & (1 - is_inline)) != 0;
}

pub fn getIndex(self: Ident) ?u28 {
    if (self.isInline()) {
        return null;
    }
    return @intCast(self.data & INDEX_MASK);
}

pub fn getBytes(self: Ident, buffer: *[4]u8) []const u8 {
    if (!self.isInline()) {
        return &[_]u8{};
    }

    const masked = self.data & ~SIGN_BIT_MASK;

    var len: usize = 0;
    for (0..4) |i| {
        const byte = @as(u8, @intCast((masked >> @intCast(i * 8)) & 0xFF));
        if (byte == 0) break;
        buffer[i] = byte;
        len += 1;
    }

    return buffer[0..len];
}

test "init with 1 byte" {
    const ident = initSmallAscii("x", false, false, false);
    try std.testing.expect(ident.isInline());
    try std.testing.expect(!ident.isReused());
    try std.testing.expect(!ident.isUnused());
    try std.testing.expect(!ident.isEffectful());

    var buffer: [4]u8 = undefined;
    const bytes = ident.getBytes(&buffer);
    try std.testing.expectEqualStrings("x", bytes);
}

test "init with 2 bytes" {
    const ident = initSmallAscii("ab", true, false, true);
    try std.testing.expect(ident.isInline());
    try std.testing.expect(ident.isReused());
    try std.testing.expect(!ident.isUnused());
    try std.testing.expect(ident.isEffectful());

    var buffer: [4]u8 = undefined;
    const bytes = ident.getBytes(&buffer);
    try std.testing.expectEqualStrings("ab", bytes);
}

test "init with 3 bytes" {
    const ident = initSmallAscii("foo", false, true, false);
    try std.testing.expect(ident.isInline());
    try std.testing.expect(!ident.isReused());
    try std.testing.expect(ident.isUnused());
    try std.testing.expect(!ident.isEffectful());

    var buffer: [4]u8 = undefined;
    const bytes = ident.getBytes(&buffer);
    try std.testing.expectEqualStrings("foo", bytes);
}

test "init with 4 bytes" {
    const ident = initSmallAscii("test", true, true, true);
    try std.testing.expect(ident.isInline());
    try std.testing.expect(ident.isReused());
    try std.testing.expect(ident.isUnused());
    try std.testing.expect(ident.isEffectful());

    var buffer: [4]u8 = undefined;
    const bytes = ident.getBytes(&buffer);
    try std.testing.expectEqualStrings("test", bytes);
}

test "initFromIndex" {
    const index: u28 = 12345;
    const ident = initFromIdx(index, true, false, true);

    try std.testing.expect(!ident.isInline());
    try std.testing.expect(ident.isReused());
    try std.testing.expect(!ident.isUnused());
    try std.testing.expect(ident.isEffectful());

    const retrieved_index = ident.getIndex();
    try std.testing.expect(retrieved_index != null);
    try std.testing.expectEqual(index, retrieved_index.?);

    var buffer: [4]u8 = undefined;
    const bytes = ident.getBytes(&buffer);
    try std.testing.expectEqual(@as(usize, 0), bytes.len);
}

test "mixed metadata bits" {
    const ident1 = initSmallAscii("hi", true, true, false);
    try std.testing.expect(ident1.isReused());
    try std.testing.expect(ident1.isUnused());
    try std.testing.expect(!ident1.isEffectful());

    const ident2 = initSmallAscii("bye", false, false, true);
    try std.testing.expect(!ident2.isReused());
    try std.testing.expect(!ident2.isUnused());
    try std.testing.expect(ident2.isEffectful());

    var buffer: [4]u8 = undefined;
    try std.testing.expectEqualStrings("hi", ident1.getBytes(&buffer));
    try std.testing.expectEqualStrings("bye", ident2.getBytes(&buffer));
}

test "getIndex returns null for inline idents" {
    const ident = initSmallAscii("abc", false, false, false);
    try std.testing.expectEqual(@as(?u28, null), ident.getIndex());
}

test "maximum index value" {
    const max_index: u28 = INDEX_MASK;
    const ident = initFromIdx(max_index, false, false, false);

    const retrieved = ident.getIndex();
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqual(max_index, retrieved.?);
}

test "index with metadata bits" {
    const test_cases = [_]struct {
        index: u28,
        reused: bool,
        unused: bool,
        effectful: bool,
    }{
        .{ .index = 0, .reused = false, .unused = false, .effectful = false },
        .{ .index = 12345, .reused = true, .unused = false, .effectful = false },
        .{ .index = 67890, .reused = false, .unused = true, .effectful = false },
        .{ .index = 111111, .reused = false, .unused = false, .effectful = true },
        .{ .index = 999999, .reused = true, .unused = true, .effectful = false },
        .{ .index = 5555555, .reused = false, .unused = true, .effectful = true },
        .{ .index = 10000000, .reused = true, .unused = false, .effectful = true },
        .{ .index = INDEX_MASK, .reused = true, .unused = true, .effectful = true },
    };

    for (test_cases) |tc| {
        const ident = initFromIdx(tc.index, tc.reused, tc.unused, tc.effectful);

        try std.testing.expect(!ident.isInline());
        try std.testing.expectEqual(tc.reused, ident.isReused());
        try std.testing.expectEqual(tc.unused, ident.isUnused());
        try std.testing.expectEqual(tc.effectful, ident.isEffectful());

        const retrieved_index = ident.getIndex();
        try std.testing.expect(retrieved_index != null);
        try std.testing.expectEqual(tc.index, retrieved_index.?);
    }
}

test "inline with metadata bits" {
    const test_cases = [_]struct {
        text: []const u8,
        reused: bool,
        unused: bool,
        effectful: bool,
    }{
        .{ .text = "a", .reused = false, .unused = false, .effectful = false },
        .{ .text = "ab", .reused = true, .unused = false, .effectful = false },
        .{ .text = "abc", .reused = false, .unused = true, .effectful = false },
        .{ .text = "abcd", .reused = false, .unused = false, .effectful = true },
        .{ .text = "xy", .reused = true, .unused = true, .effectful = false },
        .{ .text = "foo", .reused = false, .unused = true, .effectful = true },
        .{ .text = "bar", .reused = true, .unused = false, .effectful = true },
        .{ .text = "test", .reused = true, .unused = true, .effectful = true },
    };

    for (test_cases) |tc| {
        const ident = initSmallAscii(tc.text, tc.reused, tc.unused, tc.effectful);

        try std.testing.expect(ident.isInline());
        try std.testing.expectEqual(tc.reused, ident.isReused());
        try std.testing.expectEqual(tc.unused, ident.isUnused());
        try std.testing.expectEqual(tc.effectful, ident.isEffectful());

        var buffer: [4]u8 = undefined;
        const bytes = ident.getBytes(&buffer);
        try std.testing.expectEqualStrings(tc.text, bytes);
    }
}
