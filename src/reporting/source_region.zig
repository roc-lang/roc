//! Utilities for formatting source code regions in diagnostic reports

const std = @import("std");
const testing = std.testing;

/// Extract specific lines from source text.
/// Lines are 1-indexed (first line is line 1).
pub fn extractLines(source: []const u8, start_line: u32, end_line: u32) []const u8 {
    if (source.len == 0 or start_line == 0 or start_line > end_line) {
        return "";
    }

    var current_line: u32 = 1;
    var start_idx: ?usize = null;
    var end_idx: ?usize = null;

    // If we're looking for line 1, start at index 0
    if (start_line == 1) {
        start_idx = 0;
    }

    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            current_line += 1;

            // Found the start of our desired line range
            if (current_line == start_line and start_idx == null) {
                start_idx = i + 1;
            }

            // Found the end of our desired line range
            if (current_line > end_line) {
                end_idx = i;
                break;
            }
        }
    }

    // If we haven't found the start, the line number is too high
    if (start_idx == null) {
        return "";
    }

    // If we haven't found the end, use the end of the source
    if (end_idx == null) {
        end_idx = source.len;
    }

    return source[start_idx.?..end_idx.?];
}

/// Calculate the width needed to display a line number
pub fn calculateLineNumberWidth(max_line: u32) u32 {
    if (max_line == 0) return 1;
    return @intCast(std.math.log10(max_line) + 1);
}

/// Format a line number with the given width
pub fn formatLineNumber(writer: anytype, line_num: u32, width: u32) !void {
    try writer.print("{d: >[1]}", .{ line_num, width });
}

/// Calculate the length of an underline for a region
/// For single-line regions, returns end_column - start_column
/// Minimum length is 1
pub fn calculateUnderlineLength(start_column: u32, end_column: u32) u32 {
    if (end_column >= start_column) {
        return end_column - start_column;
    }
    return 1;
}

/// Print spaces for indentation
pub fn printSpaces(writer: anytype, count: u32) !void {
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll(" ");
    }
}

// ===== TESTS =====

test "extractLines - single line" {
    const source = "line 1\nline 2\nline 3";
    try testing.expectEqualStrings("line 2", extractLines(source, 2, 2));
}

test "extractLines - multiple lines" {
    const source = "line 1\nline 2\nline 3\nline 4";
    try testing.expectEqualStrings("line 2\nline 3", extractLines(source, 2, 3));
}

test "extractLines - first line" {
    const source = "line 1\nline 2\nline 3";
    try testing.expectEqualStrings("line 1", extractLines(source, 1, 1));
}

test "extractLines - last line without newline" {
    const source = "line 1\nline 2\nline 3";
    try testing.expectEqualStrings("line 3", extractLines(source, 3, 3));
}

test "extractLines - last line with newline" {
    const source = "line 1\nline 2\nline 3\n";
    try testing.expectEqualStrings("line 3", extractLines(source, 3, 3));
}

test "extractLines - line number too high" {
    const source = "line 1\nline 2";
    try testing.expectEqualStrings("", extractLines(source, 5, 5));
}

test "extractLines - empty source" {
    try testing.expectEqualStrings("", extractLines("", 1, 1));
}

test "extractLines - invalid range" {
    const source = "line 1\nline 2";
    try testing.expectEqualStrings("", extractLines(source, 2, 1));
    try testing.expectEqualStrings("", extractLines(source, 0, 1));
}

test "calculateLineNumberWidth" {
    try testing.expectEqual(@as(u32, 1), calculateLineNumberWidth(0));
    try testing.expectEqual(@as(u32, 1), calculateLineNumberWidth(1));
    try testing.expectEqual(@as(u32, 1), calculateLineNumberWidth(9));
    try testing.expectEqual(@as(u32, 2), calculateLineNumberWidth(10));
    try testing.expectEqual(@as(u32, 2), calculateLineNumberWidth(99));
    try testing.expectEqual(@as(u32, 3), calculateLineNumberWidth(100));
    try testing.expectEqual(@as(u32, 3), calculateLineNumberWidth(999));
    try testing.expectEqual(@as(u32, 4), calculateLineNumberWidth(1000));
    try testing.expectEqual(@as(u32, 4), calculateLineNumberWidth(9999));
    try testing.expectEqual(@as(u32, 5), calculateLineNumberWidth(10000));
}

test "formatLineNumber" {
    var buffer: [100]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    // Test width 1
    try formatLineNumber(writer, 5, 1);
    try testing.expectEqualStrings("5", stream.getWritten());

    // Test width 3
    stream.reset();
    try formatLineNumber(writer, 5, 3);
    try testing.expectEqualStrings("  5", stream.getWritten());

    // Test width 4 with large number
    stream.reset();
    try formatLineNumber(writer, 1234, 4);
    try testing.expectEqualStrings("1234", stream.getWritten());
}

test "calculateUnderlineLength" {
    // Normal case: end > start
    try testing.expectEqual(@as(u32, 5), calculateUnderlineLength(1, 6));

    // Single character
    try testing.expectEqual(@as(u32, 0), calculateUnderlineLength(1, 1));

    // Edge case: end < start (should return 1)
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(5, 3));

    // Two character identifier
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(3, 4));
}

test "printSpaces" {
    var buffer: [100]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try printSpaces(writer, 0);
    try testing.expectEqualStrings("", stream.getWritten());

    stream.reset();
    try printSpaces(writer, 5);
    try testing.expectEqualStrings("     ", stream.getWritten());
}

test "integration - format source region" {
    const source = "module []\n\nx = 1\n\nfoo = |_| {\n    x = 2\n    {}\n}";

    // Extract line 3 (x = 1)
    const line3 = extractLines(source, 3, 3);
    try testing.expectEqualStrings("x = 1", line3);

    // Extract line 6 (    x = 2)
    const line6 = extractLines(source, 6, 6);
    try testing.expectEqualStrings("    x = 2", line6);

    // For "x" identifier at column 1, length should be 1
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(1, 2));

    // For "x" identifier at column 5, length should be 1
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(5, 6));
}

test "real-world identifier underline calculations" {
    // Single character identifier "x" at column 1
    // Columns: 1234...
    // Text:    x = 1
    //          ^ (column 1-2, should underline 1 char)
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(1, 2));

    // Multi-character identifier "someVariable" at column 1
    // Columns: 123456789012345...
    // Text:    someVariable = 1
    //          ^^^^^^^^^^^^ (columns 1-13, should underline 12 chars)
    try testing.expectEqual(@as(u32, 12), calculateUnderlineLength(1, 13));

    // Identifier "foo" at column 5
    // Columns: 12345678...
    // Text:        foo = bar
    //              ^^^ (columns 5-8, should underline 3 chars)
    try testing.expectEqual(@as(u32, 3), calculateUnderlineLength(5, 8));

    // Edge case: single column span (e.g., single char at end of identifier)
    try testing.expectEqual(@as(u32, 0), calculateUnderlineLength(10, 10));
}
