const std = @import("std");
const LineIndex = @import("lsp").line_index.LineIndex;
const Position = @import("lsp").line_index.Position;
const Range = @import("lsp").line_index.Range;
const lsp_position = @import("lsp").position;

test "LineIndex converts UTF-16 positions and byte offsets" {
    const source = "aé😀\nnext";
    const starts = [_]u32{ 0, 8 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(@as(?u32, 3), index.utf16ToByte(0, 2, .nearest));
    try std.testing.expectEqual(@as(?u32, 7), index.utf16ToByte(0, 3, .nearest));
    try std.testing.expect(index.utf16ToByte(0, 3, .exact) == null);
    try std.testing.expectEqual(Position{ .line = 0, .character = 2 }, index.byteToUtf16(3).?);
    try std.testing.expectEqual(Position{ .line = 0, .character = 4 }, index.byteToUtf16(7).?);
}

test "LineIndex normalizes CRLF and LF terminator offsets" {
    const source = "a\r\nb\n";
    const starts = [_]u32{ 0, 3, 5 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(Position{ .line = 0, .character = 1 }, index.byteToUtf16(1).?);
    try std.testing.expectEqual(Position{ .line = 0, .character = 1 }, index.byteToUtf16(2).?);
    try std.testing.expectEqual(Position{ .line = 1, .character = 1 }, index.byteToUtf16(4).?);
}

test "LineIndex clamps columns without accepting missing lines" {
    const source = "abc\ndef";
    const starts = [_]u32{ 0, 4 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(@as(?u32, 3), index.utf16ToByte(0, 99, .nearest));
    try std.testing.expectEqual(@as(?u32, 3), index.utf16ToByte(0, 99, .exact));
    try std.testing.expect(index.utf16ToByte(2, 0, .nearest) == null);
}

test "LineIndex round-trips ASCII LF offsets" {
    const source = "hello\nworld\ntest";
    const starts = [_]u32{ 0, 6, 12 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    for ([_]u32{ 0, 3, 5, 6, 10, 11, 12, 15 }) |offset| {
        const lsp_pos = index.byteToUtf16(offset) orelse return error.UnexpectedNull;
        try std.testing.expectEqual(@as(?u32, offset), index.utf16ToByte(lsp_pos.line, lsp_pos.character, .exact));
    }
}

test "LineIndex maps ASCII LF offsets to expected positions" {
    const source = "abc\ndef";
    const starts = [_]u32{ 0, 4 };
    const index = LineIndex{ .source = source, .line_starts = &starts };
    try std.testing.expectEqual(Position{ .line = 0, .character = 0 }, index.byteToUtf16(0).?);
    try std.testing.expectEqual(Position{ .line = 0, .character = 2 }, index.byteToUtf16(2).?);
    try std.testing.expectEqual(Position{ .line = 0, .character = 3 }, index.byteToUtf16(3).?);
    try std.testing.expectEqual(Position{ .line = 1, .character = 0 }, index.byteToUtf16(4).?);
    try std.testing.expectEqual(Position{ .line = 1, .character = 1 }, index.byteToUtf16(5).?);
    try std.testing.expectEqual(Position{ .line = 1, .character = 2 }, index.byteToUtf16(6).?);
    try std.testing.expectEqual(@as(?u32, 6), index.utf16ToByte(1, 2, .exact));
}

test "LineIndex maps an empty source offset" {
    const starts = [_]u32{0};
    const index = LineIndex{ .source = "", .line_starts = &starts };

    try std.testing.expectEqual(Position{ .line = 0, .character = 0 }, index.byteToUtf16(0).?);
}

test "LineIndex tolerates invalid UTF-8 for queries and rejects it for edits" {
    const source = "a\xffb";
    const starts = [_]u32{0};
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(Position{ .line = 0, .character = 2 }, index.byteToUtf16(2).?);
    try std.testing.expectEqual(@as(?u32, 2), index.utf16ToByte(0, 2, .nearest));
    try std.testing.expect(index.utf16ToByte(0, 2, .exact) == null);
}

test "LineIndex borrows source and starts from LineOffsets" {
    const source = "one\ntwo";
    const offsets = try lsp_position.buildLineOffsets(std.testing.allocator, source);
    defer offsets.deinit();

    const index = LineIndex.fromLineOffsets(&offsets);
    try std.testing.expectEqualStrings(source, index.source);
    try std.testing.expectEqualSlices(u32, offsets.offsets, index.line_starts);
    try std.testing.expectEqual(Position{ .line = 1, .character = 1 }, index.byteToUtf16(5).?);
}

test "LineIndex converts byte ranges and UTF-16 lengths" {
    const source = "aé😀\nnext";
    const starts = [_]u32{ 0, 8 };
    const index = LineIndex{ .source = source, .line_starts = &starts };

    try std.testing.expectEqual(Range{
        .start = .{ .line = 0, .character = 1 },
        .end = .{ .line = 0, .character = 2 },
    }, index.rangeFromBytes(1, 3).?);
    try std.testing.expectEqual(Range{
        .start = .{ .line = 0, .character = 2 },
        .end = .{ .line = 1, .character = 0 },
    }, index.rangeFromBytes(3, 8).?);
    try std.testing.expect(index.rangeFromBytes(8, 3) == null);
    try std.testing.expect(index.rangeFromBytes(0, 99) == null);

    try std.testing.expectEqual(@as(?u32, 3), index.utf16Length(1, 7));
    try std.testing.expect(index.utf16Length(1, 8) == null);
    try std.testing.expect(index.utf16Length(7, 1) == null);
    try std.testing.expect(index.utf16Length(0, 99) == null);
}
