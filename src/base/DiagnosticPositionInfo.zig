const std = @import("std");
const Allocator = std.mem.Allocator;

start_line: u32,
start_col: u32,
end_line: u32,
end_col: u32,
line_text: []const u8,

const DiagnosticPositionInfo = @This();

/// Finds the line number for a given position in the source
fn lineNum(newlines: std.ArrayList(u32), pos: u32) u32 {
    if (newlines.items.len == 0) {
        return 0;
    }

    var lineno: u32 = 0;

    while (lineno + 1 < newlines.items.len) {
        if (newlines.items[lineno + 1] > pos) {
            return lineno;
        }
        lineno += 1;
    }

    return lineno;
}

/// Gets the column number for a position on a given line
fn columnNum(newlines: std.ArrayList(u32), line: u32, pos: u32) !u32 {
    const line_start: u32 = @intCast(newlines.items[line]);
    if (pos < line_start) {
        return error.InvalidPosition;
    }
    return pos - line_start;
}

/// Returns the source text for a given line
fn getLineText(source: []const u8, newlines: std.ArrayList(u32), line: u32) []const u8 {
    const line_start = newlines.items[line];
    const line_end = if (line + 1 < newlines.items.len)
        newlines.items[line + 1]
    else
        source.len;

    return source[line_start..line_end];
}

pub fn countNewlines(gpa: Allocator, source: []const u8) !std.ArrayList(u32) {
    var newlines = std.ArrayList(u32).init(gpa);

    try newlines.append(0);

    // Find all newlines in the source
    var pos: u32 = 0;
    for (source) |c| {
        if (c == '\n') {
            try newlines.append(pos + 1); // Position after the newline
        }
        pos += 1;
    }

    return newlines;
}

/// Returns the position info for a diagnostic
pub fn get(source: []const u8, newlines: std.ArrayList(u32), begin: u32, end: u32) !DiagnosticPositionInfo {
    if (begin > end) {
        return error.OutOfOrder;
    }
    if (begin > source.len) {
        return error.BeginTooLarge;
    }
    if (end > source.len) {
        return error.EndTooLarge;
    }

    const start_line = lineNum(newlines, begin);
    const start_col = try columnNum(newlines, start_line, begin);
    const end_line = lineNum(newlines, end);
    const end_col = try columnNum(newlines, end_line, end);
    const line_text = getLineText(source, newlines, start_line);

    return .{
        .start_line = start_line,
        .start_col = start_col,
        .end_line = end_line,
        .end_col = end_col,
        .line_text = line_text,
    };
}

test "lineNum" {
    const gpa = std.testing.allocator;
    var newlines = std.ArrayList(u32).init(gpa);
    defer newlines.deinit();

    // Simple test case with lines at positions 0, 10, 20
    try newlines.append(0);
    try newlines.append(10);
    try newlines.append(20);
    try newlines.append(30);

    try std.testing.expectEqual(@as(u32, 0), lineNum(newlines, 0));
    try std.testing.expectEqual(@as(u32, 0), lineNum(newlines, 5));
    try std.testing.expectEqual(@as(u32, 0), lineNum(newlines, 9));
    try std.testing.expectEqual(@as(u32, 1), lineNum(newlines, 10));
    try std.testing.expectEqual(@as(u32, 1), lineNum(newlines, 15));
    try std.testing.expectEqual(@as(u32, 1), lineNum(newlines, 19));
    try std.testing.expectEqual(@as(u32, 2), lineNum(newlines, 20));
    try std.testing.expectEqual(@as(u32, 2), lineNum(newlines, 25));
    try std.testing.expectEqual(@as(u32, 2), lineNum(newlines, 29));
    try std.testing.expectEqual(@as(u32, 3), lineNum(newlines, 30));
    try std.testing.expectEqual(@as(u32, 3), lineNum(newlines, 35));
}

test "columnNum" {
    const gpa = std.testing.allocator;
    var newlines = std.ArrayList(u32).init(gpa);
    defer newlines.deinit();

    try newlines.append(0);
    try newlines.append(10);
    try newlines.append(20);

    try std.testing.expectEqual(@as(u32, 0), columnNum(newlines, 0, 0));
    try std.testing.expectEqual(@as(u32, 5), columnNum(newlines, 0, 5));
    try std.testing.expectEqual(@as(u32, 9), columnNum(newlines, 0, 9));

    try std.testing.expectEqual(@as(u32, 0), columnNum(newlines, 1, 10));
    try std.testing.expectEqual(@as(u32, 5), columnNum(newlines, 1, 15));
}

test "getLineText" {
    const gpa = std.testing.allocator;
    var newlines = std.ArrayList(u32).init(gpa);
    defer newlines.deinit();

    const source = "line0\nline1\nline2";

    try newlines.append(0);
    try newlines.append(6); // After "line0\n"
    try newlines.append(12); // After "line1\n"

    try std.testing.expectEqualStrings("line0\n", getLineText(source, newlines, 0));
    try std.testing.expectEqualStrings("line1\n", getLineText(source, newlines, 1));
    try std.testing.expectEqualStrings("line2", getLineText(source, newlines, 2));
}

test "get" {
    const gpa = std.testing.allocator;
    var newlines = std.ArrayList(u32).init(gpa);
    defer newlines.deinit();

    const source = "line0\nline1\nline2";

    try newlines.append(0);
    try newlines.append(6); // After "line0\n"
    try newlines.append(12); // After "line1\n"

    const info1 = get(source, newlines, 2, 4); // "ne" in line0
    try std.testing.expectEqual(@as(u32, 0), info1.start_line);
    try std.testing.expectEqual(@as(u32, 2), info1.start_col);
    try std.testing.expectEqual(@as(u32, 0), info1.end_line);
    try std.testing.expectEqual(@as(u32, 4), info1.end_col);
    try std.testing.expectEqualStrings("line0\n", info1.line_text);

    const info2 = get(source, newlines, 8, 10); // "ne" in line1
    try std.testing.expectEqual(@as(u32, 1), info2.start_line);
    try std.testing.expectEqual(@as(u32, 2), info2.start_col);
    try std.testing.expectEqual(@as(u32, 1), info2.end_line);
    try std.testing.expectEqual(@as(u32, 4), info2.end_col);
    try std.testing.expectEqualStrings("line1\n", info2.line_text);
}
