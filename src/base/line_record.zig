//! Reading and writing the line-oriented text records the compiler keeps on
//! disk, such as package dependency sidecars and install manifests.
//!
//! A record is a sequence of `\n`-terminated lines. Numbers are decimal with
//! no sign and no leading zeros. Arbitrary bytes are written as
//! `<len>:<bytes>`, so they may contain anything, spaces and newlines
//! included, without escaping. Parsing is strict: every reader method returns
//! null (or false) on any deviation, and the caller treats the whole record
//! as absent.

const std = @import("std");

/// Write `bytes` as `<len>:<bytes>`.
pub fn writeLengthPrefixed(writer: *std.Io.Writer, bytes: []const u8) std.Io.Writer.Error!void {
    try writer.print("{d}:", .{bytes.len});
    try writer.writeAll(bytes);
}

/// A cursor over a record's bytes.
pub const Reader = struct {
    bytes: []const u8,
    pos: usize = 0,

    pub fn remaining(self: *const Reader) usize {
        return self.bytes.len - self.pos;
    }

    /// Consume `text` if the record continues with it.
    pub fn literal(self: *Reader, text: []const u8) bool {
        if (!std.mem.startsWith(u8, self.bytes[self.pos..], text)) return false;
        self.pos += text.len;
        return true;
    }

    pub fn byte(self: *Reader) ?u8 {
        if (self.remaining() == 0) return null;
        defer self.pos += 1;
        return self.bytes[self.pos];
    }

    /// The bytes before the next `terminator`, consuming the terminator.
    pub fn until(self: *Reader, terminator: u8) ?[]const u8 {
        const rest = self.bytes[self.pos..];
        const end = std.mem.findScalar(u8, rest, terminator) orelse return null;
        self.pos += end + 1;
        return rest[0..end];
    }

    /// A decimal number with no sign and no leading zeros, followed by `terminator`.
    pub fn decimal(self: *Reader, comptime T: type, terminator: u8) ?T {
        const text = self.until(terminator) orelse return null;
        if (text.len == 0) return null;
        if (text[0] == '0' and text.len > 1) return null;
        var value: T = 0;
        for (text) |c| {
            if (c < '0' or c > '9') return null;
            value = std.math.mul(T, value, 10) catch return null;
            value = std.math.add(T, value, c - '0') catch return null;
        }
        return value;
    }

    /// `<len>:` followed by exactly `len` bytes and then `terminator`. The
    /// result borrows from the record's bytes.
    pub fn lengthPrefixed(self: *Reader, terminator: u8) ?[]const u8 {
        const len = self.decimal(usize, ':') orelse return null;
        if (len >= self.remaining()) return null;
        const text = self.bytes[self.pos..][0..len];
        self.pos += len;
        if (self.bytes[self.pos] != terminator) return null;
        self.pos += 1;
        return text;
    }
};

test "length-prefixed bytes round-trip whatever they contain" {
    const values = [_][]const u8{ "", "plain", "with space", "a\nb", "3:abc", "\x00\xff\r\t" };
    var buffer: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer buffer.deinit();
    for (values) |value| {
        try writeLengthPrefixed(&buffer.writer, value);
        try buffer.writer.writeByte('\n');
    }

    var reader = Reader{ .bytes = buffer.written() };
    for (values) |value| {
        try std.testing.expectEqualStrings(value, reader.lengthPrefixed('\n').?);
    }
    try std.testing.expectEqual(@as(usize, 0), reader.remaining());
}

test "decimals are canonical and range-checked" {
    const accepted = [_]struct { text: []const u8, value: u8 }{
        .{ .text = "0\n", .value = 0 },
        .{ .text = "7\n", .value = 7 },
        .{ .text = "255\n", .value = 255 },
    };
    for (accepted) |case| {
        var reader = Reader{ .bytes = case.text };
        try std.testing.expectEqual(case.value, reader.decimal(u8, '\n').?);
    }

    const rejected = [_][]const u8{ "\n", "00\n", "07\n", "+7\n", "-7\n", "1_0\n", " 7\n", "256\n", "7", "0x7\n" };
    for (rejected) |text| {
        var reader = Reader{ .bytes = text };
        try std.testing.expect(reader.decimal(u8, '\n') == null);
    }
}

test "length-prefixed bytes reject wrong lengths and terminators" {
    const rejected = [_][]const u8{ "", "3:ab\n", "2:abc\n", "3:abc", "3:abc ", "03:abc\n", ":abc\n", "3abc\n" };
    for (rejected) |text| {
        var reader = Reader{ .bytes = text };
        try std.testing.expect(reader.lengthPrefixed('\n') == null);
    }
}
