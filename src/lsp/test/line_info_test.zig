//! Tests for line information utilities.

const std = @import("std");
const line_info = @import("../line_info.zig");
const LineInfo = line_info.LineInfo;
const computeLineStarts = line_info.computeLineStarts;

test "computeLineStarts empty string" {
    const allocator = std.testing.allocator;
    const starts = try computeLineStarts(allocator, "");
    defer allocator.free(starts);

    // Empty string still has line 0 starting at offset 0
    try std.testing.expectEqual(@as(usize, 1), starts.len);
    try std.testing.expectEqual(@as(u32, 0), starts[0]);
}

test "computeLineStarts single line" {
    const allocator = std.testing.allocator;
    const starts = try computeLineStarts(allocator, "hello");
    defer allocator.free(starts);

    try std.testing.expectEqual(@as(usize, 1), starts.len);
    try std.testing.expectEqual(@as(u32, 0), starts[0]);
}

test "computeLineStarts multiple lines" {
    const allocator = std.testing.allocator;
    // "a\nb\nc" -> lines at offsets 0, 2, 4
    const starts = try computeLineStarts(allocator, "a\nb\nc");
    defer allocator.free(starts);

    try std.testing.expectEqual(@as(usize, 3), starts.len);
    try std.testing.expectEqual(@as(u32, 0), starts[0]); // "a\n" starts at 0
    try std.testing.expectEqual(@as(u32, 2), starts[1]); // "b\n" starts at 2
    try std.testing.expectEqual(@as(u32, 4), starts[2]); // "c" starts at 4
}

test "computeLineStarts with CRLF" {
    const allocator = std.testing.allocator;
    // "a\r\nb" -> lines at offsets 0, 3 (CRLF is 2 bytes, line 1 starts after both)
    const starts = try computeLineStarts(allocator, "a\r\nb");
    defer allocator.free(starts);

    try std.testing.expectEqual(@as(usize, 2), starts.len);
    try std.testing.expectEqual(@as(u32, 0), starts[0]);
    try std.testing.expectEqual(@as(u32, 3), starts[1]); // After \r\n
}

test "computeLineStarts trailing newline" {
    const allocator = std.testing.allocator;
    // "a\n" -> lines at offsets 0, 2 (empty line 1 after newline)
    const starts = try computeLineStarts(allocator, "a\n");
    defer allocator.free(starts);

    try std.testing.expectEqual(@as(usize, 2), starts.len);
    try std.testing.expectEqual(@as(u32, 0), starts[0]);
    try std.testing.expectEqual(@as(u32, 2), starts[1]);
}

test "positionFromOffset at line start" {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, "abc\ndef");
    defer info.deinit();

    // Offset 0 -> line 0, character 0
    const pos = info.positionFromOffset(0) orelse return error.UnexpectedNull;
    try std.testing.expectEqual(@as(u32, 0), pos.line);
    try std.testing.expectEqual(@as(u32, 0), pos.character);
}

test "positionFromOffset middle of line" {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, "abc\ndef");
    defer info.deinit();

    // Offset 2 -> line 0, character 2 (the 'c')
    const pos = info.positionFromOffset(2) orelse return error.UnexpectedNull;
    try std.testing.expectEqual(@as(u32, 0), pos.line);
    try std.testing.expectEqual(@as(u32, 2), pos.character);
}

test "positionFromOffset after newline" {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, "abc\ndef");
    defer info.deinit();

    // Offset 4 -> line 1, character 0 (the 'd')
    // "abc\n" is 4 bytes, so offset 4 is start of line 1
    const pos = info.positionFromOffset(4) orelse return error.UnexpectedNull;
    try std.testing.expectEqual(@as(u32, 1), pos.line);
    try std.testing.expectEqual(@as(u32, 0), pos.character);
}

test "positionFromOffset second line middle" {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, "abc\ndef");
    defer info.deinit();

    // Offset 5 -> line 1, character 1 (the 'e')
    const pos = info.positionFromOffset(5) orelse return error.UnexpectedNull;
    try std.testing.expectEqual(@as(u32, 1), pos.line);
    try std.testing.expectEqual(@as(u32, 1), pos.character);
}

test "offsetFromPosition round-trips" {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, "hello\nworld\ntest");
    defer info.deinit();

    // Test various offsets
    const offsets = [_]u32{ 0, 3, 5, 6, 10, 12, 15 };
    for (offsets) |offset| {
        const pos = info.positionFromOffset(offset) orelse return error.UnexpectedNull;
        const back = info.offsetFromPosition(pos) orelse return error.UnexpectedNull;
        try std.testing.expectEqual(offset, back);
    }
}

test "positionFromOffset handles empty source" {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, "");
    defer info.deinit();

    // Offset 0 in empty string -> line 0, character 0
    const pos = info.positionFromOffset(0) orelse return error.UnexpectedNull;
    try std.testing.expectEqual(@as(u32, 0), pos.line);
    try std.testing.expectEqual(@as(u32, 0), pos.character);
}
