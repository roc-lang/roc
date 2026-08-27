const std = @import("std");
const LineIndex = @import("lsp").line_index.LineIndex;
const Position = @import("lsp").line_index.Position;

test "LineIndex converts UTF-16 positions and clamps columns" {
    const source = "aé😀\nnext";
    const starts = [_]u32{ 0, 8 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(@as(?u32, 3), index.utf16ToByte(0, 2, .nearest));
    try std.testing.expectEqual(@as(?u32, 7), index.utf16ToByte(0, 3, .nearest));
    try std.testing.expect(index.utf16ToByte(0, 3, .exact) == null);
    try std.testing.expectEqual(@as(?u32, 7), index.utf16ToByte(0, 99, .exact));
    try std.testing.expectEqual(Position{ .line = 0, .character = 2 }, index.byteToUtf16(3).?);
    try std.testing.expectEqual(Position{ .line = 0, .character = 4 }, index.byteToUtf16(7).?);
}

test "LineIndex normalizes CRLF offsets and rejects malformed edit boundaries" {
    const source = "a\r\nb\xffc";
    const starts = [_]u32{ 0, 3 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(Position{ .line = 0, .character = 1 }, index.byteToUtf16(1).?);
    try std.testing.expectEqual(Position{ .line = 1, .character = 2 }, index.byteToUtf16(5).?);
    try std.testing.expect(index.utf16ToByte(1, 2, .exact) == null);
    try std.testing.expectEqual(@as(?u32, 5), index.utf16ToByte(1, 2, .nearest));
}

test "LineIndex round-trips ASCII LF offsets" {
    const source = "one\ntwo";
    const starts = [_]u32{ 0, 4 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    for (0..source.len + 1) |offset| {
        const position = index.byteToUtf16(@intCast(offset)).?;
        try std.testing.expectEqual(@as(?u32, @intCast(offset)), index.utf16ToByte(position.line, position.character, .exact));
    }
}

test "LineIndex maps empty and ASCII documents" {
    const empty_starts = [_]u32{0};
    const empty = LineIndex{ .source = "", .line_starts = &empty_starts };
    try std.testing.expectEqual(Position{ .line = 0, .character = 0 }, empty.byteToUtf16(0).?);

    const source = "abc\ndef";
    const starts = [_]u32{ 0, 4 };
    const index = LineIndex{ .source = source, .line_starts = &starts };
    try std.testing.expectEqual(Position{ .line = 1, .character = 2 }, index.byteToUtf16(6).?);
    try std.testing.expectEqual(@as(?u32, 6), index.utf16ToByte(1, 2, .exact));
}
