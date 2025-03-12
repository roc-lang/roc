//! This module provides helpers for calculating position for diagnostics
//! including the start and end line and column information
const std = @import("std");
const Allocator = std.mem.Allocator;

start_line: u32,
start_col: u32,
end_line: u32,
end_col: u32,
line_text: []const u8,

const DiagnosticPosition = @This();

/// Finds the line index for a given position in the source
fn lineIdx(line_starts: std.ArrayList(u32), pos: u32) u32 {
    for (line_starts.items[1..], 0..) |n, i| {
        if (pos < n) {
            return @intCast(i);
        }
    }
    return @intCast(line_starts.items.len - 1);
}

/// Gets the column number for a position on a given line
fn columnIdx(line_starts: std.ArrayList(u32), line: u32, pos: u32) !u32 {
    const line_start: u32 = @intCast(line_starts.items[line]);
    if (pos < line_start) {
        return error.InvalidPosition;
    }
    return pos - line_start;
}

/// Returns the source text for a given line
fn getLineText(source: []const u8, line_starts: std.ArrayList(u32), line_idx: u32) []const u8 {
    const line_start_offset = line_starts.items[line_idx];
    const line_end_offset = if (line_idx + 1 < line_starts.items.len)
        line_starts.items[line_idx + 1]
    else
        source.len;

    return source[line_start_offset..line_end_offset];
}

/// Record the offsets for each newline in the source
pub fn findLineStarts(gpa: Allocator, source: []const u8) !std.ArrayList(u32) {
    var line_starts = std.ArrayList(u32).init(gpa);

    // if the source is empty, return an empty list of line starts
    if (source.len == 0) {
        return line_starts;
    }

    // the first line starts at offset 0
    try line_starts.append(0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (source) |c| {
        if (c == '\n') {
            // next line starts after the newline in the current position
            try line_starts.append(pos + 1);
        }
        pos += 1;
    }

    return line_starts;
}

/// Returns the position info for a diagnostic
pub fn position(source: []const u8, line_starts: std.ArrayList(u32), begin: u32, end: u32) !DiagnosticPosition {
    if (begin > end) {
        return error.OutOfOrder;
    }
    if (begin > source.len) {
        return error.BeginTooLarge;
    }
    if (end > source.len) {
        return error.EndTooLarge;
    }

    const start_line = lineIdx(line_starts, begin);
    const start_col = try columnIdx(line_starts, start_line, begin);
    const end_line = lineIdx(line_starts, end);
    const end_col = try columnIdx(line_starts, end_line, end);
    const line_text = getLineText(source, line_starts, start_line);

    return .{
        .start_line = start_line,
        .start_col = start_col,
        .end_line = end_line,
        .end_col = end_col,
        .line_text = line_text,
    };
}

test "lineIdx" {
    const gpa = std.testing.allocator;
    var line_starts = std.ArrayList(u32).init(gpa);
    defer line_starts.deinit();

    // Simple test case with lines at positions 0, 10, 20
    try line_starts.append(0);
    try line_starts.append(10);
    try line_starts.append(20);
    try line_starts.append(30);

    try std.testing.expectEqual(@as(u32, 0), lineIdx(line_starts, 0));
    try std.testing.expectEqual(@as(u32, 0), lineIdx(line_starts, 5));
    try std.testing.expectEqual(@as(u32, 0), lineIdx(line_starts, 9));
    try std.testing.expectEqual(@as(u32, 1), lineIdx(line_starts, 10));
    try std.testing.expectEqual(@as(u32, 1), lineIdx(line_starts, 15));
    try std.testing.expectEqual(@as(u32, 1), lineIdx(line_starts, 19));
    try std.testing.expectEqual(@as(u32, 2), lineIdx(line_starts, 20));
    try std.testing.expectEqual(@as(u32, 2), lineIdx(line_starts, 25));
    try std.testing.expectEqual(@as(u32, 2), lineIdx(line_starts, 29));
    try std.testing.expectEqual(@as(u32, 3), lineIdx(line_starts, 30));
    try std.testing.expectEqual(@as(u32, 3), lineIdx(line_starts, 35));
}

test "columnIdx" {
    const gpa = std.testing.allocator;
    var line_starts = std.ArrayList(u32).init(gpa);
    defer line_starts.deinit();

    try line_starts.append(0);
    try line_starts.append(10);
    try line_starts.append(20);

    try std.testing.expectEqual(@as(u32, 0), columnIdx(line_starts, 0, 0));
    try std.testing.expectEqual(@as(u32, 5), columnIdx(line_starts, 0, 5));
    try std.testing.expectEqual(@as(u32, 9), columnIdx(line_starts, 0, 9));

    try std.testing.expectEqual(@as(u32, 0), columnIdx(line_starts, 1, 10));
    try std.testing.expectEqual(@as(u32, 5), columnIdx(line_starts, 1, 15));
}

test "getLineText" {
    const gpa = std.testing.allocator;
    var line_starts = std.ArrayList(u32).init(gpa);
    defer line_starts.deinit();

    const source = "line0\nline1\nline2";

    try line_starts.append(0);
    try line_starts.append(6); // After "line0\n"
    try line_starts.append(12); // After "line1\n"

    try std.testing.expectEqualStrings("line0\n", getLineText(source, line_starts, 0));
    try std.testing.expectEqualStrings("line1\n", getLineText(source, line_starts, 1));
    try std.testing.expectEqualStrings("line2", getLineText(source, line_starts, 2));
}

test "get" {
    const gpa = std.testing.allocator;
    var line_starts = std.ArrayList(u32).init(gpa);
    defer line_starts.deinit();

    const source = "line0\nline1\nline2";

    try line_starts.append(0);
    try line_starts.append(6); // After "line0\n"
    try line_starts.append(12); // After "line1\n"

    const info1 = try position(source, line_starts, 2, 4); // "ne" in line0
    try std.testing.expectEqual(@as(u32, 0), info1.start_line);
    try std.testing.expectEqual(@as(u32, 2), info1.start_col);
    try std.testing.expectEqual(@as(u32, 0), info1.end_line);
    try std.testing.expectEqual(@as(u32, 4), info1.end_col);
    try std.testing.expectEqualStrings("line0\n", info1.line_text);

    const info2 = try position(source, line_starts, 8, 10); // "ne" in line1
    try std.testing.expectEqual(@as(u32, 1), info2.start_line);
    try std.testing.expectEqual(@as(u32, 2), info2.start_col);
    try std.testing.expectEqual(@as(u32, 1), info2.end_line);
    try std.testing.expectEqual(@as(u32, 4), info2.end_col);
    try std.testing.expectEqualStrings("line1\n", info2.line_text);
}
