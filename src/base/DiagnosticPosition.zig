//! This module provides helpers for calculating position for diagnostics
//! including the start and end line and column information
const std = @import("std");
const Allocator = std.mem.Allocator;

// byte indexes into the source text
start_line_idx: u32,
start_col_idx: u32,
end_line_idx: u32,
end_col_idx: u32,
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

/// Gets the column index for a position on a given line
fn columnIdx(line_starts: std.ArrayList(u32), line: u32, pos: u32) !u32 {
    const line_start: u32 = @intCast(line_starts.items[line]);
    if (pos < line_start) {
        return error.InvalidPosition;
    }
    return pos - line_start;
}

/// Returns the source text for a given line index
fn getLineText(source: []const u8, line_starts: std.ArrayList(u32), line_idx: u32) []const u8 {
    const line_start_offset = line_starts.items[line_idx];
    const line_end_offset = if (line_idx + 1 < line_starts.items.len)
        line_starts.items[line_idx + 1]
    else
        source.len;

    return source[line_start_offset..line_end_offset];
}

/// Record the offsets for the start of each line in the source code
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

/// Returns position info for a given start and end index offset
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

    const start_line_idx = lineIdx(line_starts, begin);
    const start_col_idx = try columnIdx(line_starts, start_line_idx, begin);
    const end_line_idx = lineIdx(line_starts, end);
    const end_col_idx = try columnIdx(line_starts, end_line_idx, end);
    const line_text = getLineText(source, line_starts, start_line_idx);

    return .{
        .start_line_idx = start_line_idx,
        .start_col_idx = start_col_idx,
        .end_line_idx = end_line_idx,
        .end_col_idx = end_col_idx,
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

    try std.testing.expectEqual(0, lineIdx(line_starts, 0));
    try std.testing.expectEqual(0, lineIdx(line_starts, 5));
    try std.testing.expectEqual(0, lineIdx(line_starts, 9));
    try std.testing.expectEqual(1, lineIdx(line_starts, 10));
    try std.testing.expectEqual(1, lineIdx(line_starts, 15));
    try std.testing.expectEqual(1, lineIdx(line_starts, 19));
    try std.testing.expectEqual(2, lineIdx(line_starts, 20));
    try std.testing.expectEqual(2, lineIdx(line_starts, 25));
    try std.testing.expectEqual(2, lineIdx(line_starts, 29));
    try std.testing.expectEqual(3, lineIdx(line_starts, 30));
    try std.testing.expectEqual(3, lineIdx(line_starts, 35));
}

test "columnIdx" {
    const gpa = std.testing.allocator;
    var line_starts = std.ArrayList(u32).init(gpa);
    defer line_starts.deinit();

    try line_starts.append(0);
    try line_starts.append(10);
    try line_starts.append(20);

    try std.testing.expectEqual(0, columnIdx(line_starts, 0, 0));
    try std.testing.expectEqual(5, columnIdx(line_starts, 0, 5));
    try std.testing.expectEqual(9, columnIdx(line_starts, 0, 9));

    try std.testing.expectEqual(0, columnIdx(line_starts, 1, 10));
    try std.testing.expectEqual(5, columnIdx(line_starts, 1, 15));
}

test "getLineText" {
    const gpa = std.testing.allocator;
    var line_starts = std.ArrayList(u32).init(gpa);
    defer line_starts.deinit();

    const source = "line0\nline1\nline2";

    try line_starts.append(0);
    try line_starts.append(6);
    try line_starts.append(12);

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
    try line_starts.append(6);
    try line_starts.append(12);

    const info1 = try position(source, line_starts, 2, 4);
    try std.testing.expectEqual(0, info1.start_line_idx);
    try std.testing.expectEqual(2, info1.start_col_idx);
    try std.testing.expectEqual(0, info1.end_line_idx);
    try std.testing.expectEqual(4, info1.end_col_idx);
    try std.testing.expectEqualStrings("line0\n", info1.line_text);

    const info2 = try position(source, line_starts, 8, 10);
    try std.testing.expectEqual(1, info2.start_line_idx);
    try std.testing.expectEqual(2, info2.start_col_idx);
    try std.testing.expectEqual(1, info2.end_line_idx);
    try std.testing.expectEqual(4, info2.end_col_idx);
    try std.testing.expectEqualStrings("line1\n", info2.line_text);
}
