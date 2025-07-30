//! This module provides helpers for calculating region information for diagnostics
//! including the start and end line and column information
//!
//! We only store simple position offsets in the AST and intermediate representation's (IR)
//! as this is more compact, and then when we need to we can calculate the line and column information
//! using line_starts and the offsets.
const std = @import("std");
const collections = @import("collections");
const Allocator = std.mem.Allocator;

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
