//! This module provides helpers for calculating region information for diagnostics
//! including the start and end line and column information
//!
//! We only store simple position offsets in the AST and intermediate representation's (IR)
//! as this is more compact, and then when we need to we can calculate the line and column information
//! using line_starts and the offsets.
const std = @import("std");
const collections = @import("collections");
const Allocator = std.mem.Allocator;

const SafeList = collections.SafeList;

// byte indexes into the source text
start_line_idx: u32,
start_col_idx: u32,
end_line_idx: u32,
end_col_idx: u32,

const RegionInfo = @This();

/// Finds the line index for a given position in the source
pub fn lineIdx(line_starts: []const u32, pos: u32) u32 {
    for (line_starts[1..], 0..) |n, i| {
        if (pos < n) {
            return @intCast(i);
        }
    }
    return @intCast(line_starts.len - 1);
}

/// Gets the column index for a position on a given line
pub fn columnIdx(line_starts: []const u32, line: u32, pos: u32) !u32 {
    const line_start: u32 = @intCast(line_starts[line]);
    if (pos < line_start) {
        return error.InvalidPosition;
    }
    return pos - line_start;
}

/// Returns the source text for a given line index
pub fn getLineText(source: []const u8, line_starts: []const u32, start_line_idx: u32, end_line_idx: u32) []const u8 {
    if (source.len == 0 or start_line_idx >= line_starts.len or start_line_idx > end_line_idx) {
        return "";
    }

    var line_start_offset: usize = 0;
    if (start_line_idx != 0) {
        line_start_offset = line_starts[start_line_idx];
    }

    var line_end_offset: usize = source.len;
    if (end_line_idx + 1 < line_starts.len) {
        line_end_offset = line_starts[end_line_idx + 1] - 1;
    }
    return source[line_start_offset..line_end_offset];
}

/// Record the offsets for the start of each line in the source code
pub fn findLineStarts(gpa: Allocator, source: []const u8) !collections.SafeList(u32) {
    var line_starts = try collections.SafeList(u32).initCapacity(gpa, 256);

    // if the source is empty, return an empty list of line starts
    if (source.len == 0) {
        return line_starts;
    }

    // the first line starts at offset 0
    _ = try line_starts.append(gpa, 0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (source) |c| {
        if (c == '\n') {
            // next line starts after the newline in the current position
            _ = try line_starts.append(gpa, pos + 1);
        }
        pos += 1;
    }

    return line_starts;
}

/// Returns position info for a given start and end index offset
pub fn position(source: []const u8, line_starts: []const u32, begin: u32, end: u32) !RegionInfo {
    if (begin > end) {
        return error.OutOfOrder;
    }
    if (begin > source.len) {
        return error.BeginTooLarge;
    }
    if (end > source.len) {
        return error.EndTooLarge;
    }
    if (line_starts.len == 0) {
        return error.NoLineStarts;
    }

    const start_line_idx = lineIdx(line_starts, begin);
    const start_col_idx = try columnIdx(line_starts, start_line_idx, begin);
    const end_line_idx = lineIdx(line_starts, end);
    const end_col_idx = try columnIdx(line_starts, end_line_idx, end);

    return .{
        .start_line_idx = start_line_idx,
        .start_col_idx = start_col_idx,
        .end_line_idx = end_line_idx,
        .end_col_idx = end_col_idx,
    };
}

/// Calculate line text for this region on demand
pub fn calculateLineText(self: RegionInfo, source: []const u8, line_starts: []const u32) []const u8 {
    return getLineText(source, line_starts, self.start_line_idx, self.end_line_idx);
}

test "lineIdx" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    // Simple test case with lines at positions 0, 10, 20
    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 10);
    _ = try line_starts.append(gpa, 20);
    _ = try line_starts.append(gpa, 30);

    try std.testing.expectEqual(0, RegionInfo.lineIdx(line_starts.items.items, 0));
    try std.testing.expectEqual(0, RegionInfo.lineIdx(line_starts.items.items, 5));
    try std.testing.expectEqual(0, RegionInfo.lineIdx(line_starts.items.items, 9));
    try std.testing.expectEqual(1, RegionInfo.lineIdx(line_starts.items.items, 10));
    try std.testing.expectEqual(1, RegionInfo.lineIdx(line_starts.items.items, 15));
    try std.testing.expectEqual(1, RegionInfo.lineIdx(line_starts.items.items, 19));
    try std.testing.expectEqual(2, RegionInfo.lineIdx(line_starts.items.items, 20));
    try std.testing.expectEqual(2, RegionInfo.lineIdx(line_starts.items.items, 25));
    try std.testing.expectEqual(2, RegionInfo.lineIdx(line_starts.items.items, 29));
    try std.testing.expectEqual(3, RegionInfo.lineIdx(line_starts.items.items, 30));
    try std.testing.expectEqual(3, RegionInfo.lineIdx(line_starts.items.items, 35));
}

test "columnIdx" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 10);
    _ = try line_starts.append(gpa, 20);

    try std.testing.expectEqual(0, RegionInfo.columnIdx(line_starts.items.items, 0, 0));
    try std.testing.expectEqual(5, RegionInfo.columnIdx(line_starts.items.items, 0, 5));
    try std.testing.expectEqual(9, RegionInfo.columnIdx(line_starts.items.items, 0, 9));

    try std.testing.expectEqual(0, RegionInfo.columnIdx(line_starts.items.items, 1, 10));
    try std.testing.expectEqual(5, RegionInfo.columnIdx(line_starts.items.items, 1, 15));
}

test "getLineText" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    const source = "line0\nline1\nline2";

    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 6);
    _ = try line_starts.append(gpa, 12);

    try std.testing.expectEqualStrings("line0", RegionInfo.getLineText(source, line_starts.items.items, 0, 0));
    try std.testing.expectEqualStrings("line1", RegionInfo.getLineText(source, line_starts.items.items, 1, 1));
    try std.testing.expectEqualStrings("line0\nline1", RegionInfo.getLineText(source, line_starts.items.items, 0, 1));
    try std.testing.expectEqualStrings("line2", RegionInfo.getLineText(source, line_starts.items.items, 2, 2));
}

test "get" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    const source = "line0\nline1\nline2";

    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 6);
    _ = try line_starts.append(gpa, 12);

    const info1 = try RegionInfo.position(source, line_starts.items.items, 2, 4);
    try std.testing.expectEqual(0, info1.start_line_idx);
    try std.testing.expectEqual(2, info1.start_col_idx);
    try std.testing.expectEqual(0, info1.end_line_idx);
    try std.testing.expectEqual(4, info1.end_col_idx);
    try std.testing.expectEqualStrings("line0", info1.calculateLineText(source, line_starts.items.items));

    const info2 = try RegionInfo.position(source, line_starts.items.items, 8, 10);
    try std.testing.expectEqual(1, info2.start_line_idx);
    try std.testing.expectEqual(2, info2.start_col_idx);
    try std.testing.expectEqual(1, info2.end_line_idx);
    try std.testing.expectEqual(4, info2.end_col_idx);
    try std.testing.expectEqualStrings("line1", info2.calculateLineText(source, line_starts.items.items));
}
