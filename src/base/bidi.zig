//! Source-level bidirectional control policy and safe display spelling.
//! The fixed set is Unicode's Bidi_Control property (UAX #9).
const std = @import("std");

/// A forbidden literal source character; escaped literal values remain legal.
pub const Control = struct {
    codepoint: u21,
    abbreviation: []const u8,
    name: []const u8,
    utf8: []const u8,
    visible: []const u8,
};

/// Complete Bidi_Control set, deliberately independent of locale and context.
pub const controls = [_]Control{
    .{ .codepoint = 0x061C, .abbreviation = "ALM", .name = "ARABIC LETTER MARK", .utf8 = "\u{061c}", .visible = "<U+061C ALM>" },
    .{ .codepoint = 0x200E, .abbreviation = "LRM", .name = "LEFT-TO-RIGHT MARK", .utf8 = "\u{200e}", .visible = "<U+200E LRM>" },
    .{ .codepoint = 0x200F, .abbreviation = "RLM", .name = "RIGHT-TO-LEFT MARK", .utf8 = "\u{200f}", .visible = "<U+200F RLM>" },
    .{ .codepoint = 0x202A, .abbreviation = "LRE", .name = "LEFT-TO-RIGHT EMBEDDING", .utf8 = "\u{202a}", .visible = "<U+202A LRE>" },
    .{ .codepoint = 0x202B, .abbreviation = "RLE", .name = "RIGHT-TO-LEFT EMBEDDING", .utf8 = "\u{202b}", .visible = "<U+202B RLE>" },
    .{ .codepoint = 0x202C, .abbreviation = "PDF", .name = "POP DIRECTIONAL FORMATTING", .utf8 = "\u{202c}", .visible = "<U+202C PDF>" },
    .{ .codepoint = 0x202D, .abbreviation = "LRO", .name = "LEFT-TO-RIGHT OVERRIDE", .utf8 = "\u{202d}", .visible = "<U+202D LRO>" },
    .{ .codepoint = 0x202E, .abbreviation = "RLO", .name = "RIGHT-TO-LEFT OVERRIDE", .utf8 = "\u{202e}", .visible = "<U+202E RLO>" },
    .{ .codepoint = 0x2066, .abbreviation = "LRI", .name = "LEFT-TO-RIGHT ISOLATE", .utf8 = "\u{2066}", .visible = "<U+2066 LRI>" },
    .{ .codepoint = 0x2067, .abbreviation = "RLI", .name = "RIGHT-TO-LEFT ISOLATE", .utf8 = "\u{2067}", .visible = "<U+2067 RLI>" },
    .{ .codepoint = 0x2068, .abbreviation = "FSI", .name = "FIRST STRONG ISOLATE", .utf8 = "\u{2068}", .visible = "<U+2068 FSI>" },
    .{ .codepoint = 0x2069, .abbreviation = "PDI", .name = "POP DIRECTIONAL ISOLATE", .utf8 = "\u{2069}", .visible = "<U+2069 PDI>" },
};

/// Match one exact UTF-8 encoding at the start, even in otherwise invalid input.
pub fn at(bytes: []const u8) ?Control {
    if (bytes.len == 0 or (bytes[0] != 0xD8 and bytes[0] != 0xE2)) return null;
    for (controls) |control| {
        if (std.mem.startsWith(u8, bytes, control.utf8)) return control;
    }
    return null;
}

/// Location of a literal control in the original byte buffer.
pub const Occurrence = struct { offset: usize, control: Control };

/// Iterate matches without decoding, modifying, or allocating source bytes.
pub const Iterator = struct {
    bytes: []const u8,
    offset: usize = 0,

    /// Return the next control, retaining original byte offsets.
    pub fn next(self: *Iterator) ?Occurrence {
        while (self.offset < self.bytes.len) {
            // Every encoding in this policy starts with D8 or E2. Skip exact
            // match-free blocks; inspect candidates against the full buffer so
            // encodings crossing a block boundary are never missed.
            if (self.bytes.len - self.offset >= 16) {
                const block: @Vector(16, u8) = self.bytes[self.offset..][0..16].*;
                const candidates = @select(bool, block == @as(@Vector(16, u8), @splat(0xD8)), @as(@Vector(16, bool), @splat(true)), block == @as(@Vector(16, u8), @splat(0xE2)));
                if (std.simd.firstTrue(candidates)) |index| {
                    self.offset += index;
                } else {
                    self.offset += 16;
                    continue;
                }
            }
            const offset = self.offset;
            if (at(self.bytes[offset..])) |control| {
                self.offset += control.utf8.len;
                return .{ .offset = offset, .control = control };
            }
            self.offset += 1;
        }
        return null;
    }
};

/// Write visible ASCII markers instead of active directional controls.
pub fn writeVisible(writer: *std.Io.Writer, bytes: []const u8) error{WriteFailed}!void {
    var iter = Iterator{ .bytes = bytes };
    var start: usize = 0;
    while (iter.next()) |match| {
        try writer.writeAll(bytes[start..match.offset]);
        try writer.writeAll(match.control.visible);
        start = iter.offset;
    }
    try writer.writeAll(bytes[start..]);
}

test "bidi policy preserves ordinary text and detects controls after malformed bytes" {
    const gpa = std.testing.allocator;
    for (controls) |control| {
        const bytes = try std.mem.concat(gpa, u8, &.{ "\xff\x00", control.utf8, "abc" });
        defer gpa.free(bytes);
        var iter = Iterator{ .bytes = bytes };
        const occurrence = iter.next().?;
        try std.testing.expectEqual(@as(usize, 2), occurrence.offset);
        try std.testing.expectEqual(control.codepoint, occurrence.control.codepoint);
        try std.testing.expect(iter.next() == null);
        var output = std.Io.Writer.Allocating.init(gpa);
        defer output.deinit();
        try writeVisible(&output.writer, bytes);
        try std.testing.expect(std.mem.find(u8, output.written(), control.visible) != null);
        var safe = Iterator{ .bytes = output.written() };
        try std.testing.expect(safe.next() == null);
        for (1..control.utf8.len) |length| try std.testing.expect(at(control.utf8[0..length]) == null);
    }
    const ordinary = "hello שלום مرحبا 😀 \\u(202E)";
    var iter = Iterator{ .bytes = ordinary };
    try std.testing.expect(iter.next() == null);
    var output = std.Io.Writer.Allocating.init(gpa);
    defer output.deinit();
    try writeVisible(&output.writer, ordinary);
    try std.testing.expectEqualStrings(ordinary, output.written());
}

/// Display wrapper for formatted diagnostic and debug strings.
pub const Display = struct {
    bytes: []const u8,
    /// Format text without emitting active directional controls.
    pub fn format(self: Display, writer: *std.Io.Writer) error{WriteFailed}!void {
        try writeVisible(writer, self.bytes);
    }
};

test "bidi scan finds every control across block boundaries" {
    for (controls) |control| {
        for (0..48) |offset| {
            var bytes: [64]u8 = @splat('a');
            @memcpy(bytes[offset..][0..control.utf8.len], control.utf8);
            var iter = Iterator{ .bytes = &bytes };
            try std.testing.expectEqual(offset, iter.next().?.offset);
            try std.testing.expect(iter.next() == null);
        }
    }
}
