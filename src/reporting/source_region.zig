//! Utilities for formatting source code regions in diagnostic reports

const std = @import("std");
const testing = std.testing;

/// Calculate the width needed to display a line number
pub fn calculateLineNumberWidth(max_line: u32) u32 {
    if (max_line == 0) return 1;
    return @intCast(std.math.log10(max_line) + 1);
}

/// Format a line number with the given width
pub fn formatLineNumber(writer: anytype, line_num: u32, width: u32) error{WriteFailed}!void {
    try writer.print("{d: >[1]}", .{ line_num, width });
}

/// Calculate the length of an underline for a region
/// For single-line regions, returns end_column - start_column
/// Minimum length is 1
pub fn calculateUnderlineLength(start_column: u32, end_column: u32) u32 {
    if (end_column > start_column) {
        return end_column - start_column;
    }
    return 1;
}

/// The number of terminal columns a single Unicode codepoint occupies:
/// 0 for combining marks / zero-width characters, 2 for East Asian Wide and
/// Fullwidth characters (and most emoji), 1 otherwise.
fn codepointWidth(cp: u21) usize {
    // Zero-width: combining diacriticals, zero-width spaces/joiners, and
    // variation selectors.
    if ((cp >= 0x0300 and cp <= 0x036F) or
        (cp >= 0x0483 and cp <= 0x0489) or
        (cp >= 0x1160 and cp <= 0x11FF) or // Hangul Jungseong/Jongseong (conjoining)
        (cp >= 0x1AB0 and cp <= 0x1AFF) or
        (cp >= 0x1DC0 and cp <= 0x1DFF) or
        (cp >= 0x200B and cp <= 0x200F) or
        (cp >= 0x2060 and cp <= 0x206F) or // word joiner / invisible operators / format chars
        (cp >= 0x20D0 and cp <= 0x20FF) or
        (cp >= 0xFE00 and cp <= 0xFE0F) or
        (cp >= 0xFE20 and cp <= 0xFE2F) or
        cp == 0xFEFF) // zero-width no-break space (BOM)
    {
        return 0;
    }
    // Wide: East Asian Wide / Fullwidth ranges and the common emoji blocks.
    if ((cp >= 0x1100 and cp <= 0x115F) or
        (cp >= 0x2E80 and cp <= 0x303E) or
        (cp >= 0x3041 and cp <= 0x33FF) or
        (cp >= 0x3400 and cp <= 0x4DBF) or
        (cp >= 0x4E00 and cp <= 0x9FFF) or
        (cp >= 0xA000 and cp <= 0xA4CF) or
        (cp >= 0xAC00 and cp <= 0xD7A3) or
        (cp >= 0xF900 and cp <= 0xFAFF) or
        (cp >= 0xFE10 and cp <= 0xFE19) or
        (cp >= 0xFE30 and cp <= 0xFE6F) or
        (cp >= 0xFF00 and cp <= 0xFF60) or
        (cp >= 0xFFE0 and cp <= 0xFFE6) or
        (cp >= 0x1F1E6 and cp <= 0x1F1FF) or // regional indicator symbols (flags)
        (cp >= 0x1F300 and cp <= 0x1FAFF) or
        (cp >= 0x20000 and cp <= 0x3FFFD))
    {
        return 2;
    }
    return 1;
}

/// The number of terminal columns `bytes` occupies when rendered, accounting
/// for multi-byte UTF-8, wide/zero-width characters, and tabs (which the box
/// renderer renders as a single space). Roc reports columns as byte offsets,
/// so callers map byte spans to display columns through this.
pub fn displayWidth(bytes: []const u8) usize {
    var w: usize = 0;
    var i: usize = 0;
    while (i < bytes.len) {
        const first = bytes[i];
        if (first == '\t') {
            w += 1;
            i += 1;
            continue;
        }
        const seq_len = std.unicode.utf8ByteSequenceLength(first) catch {
            w += 1;
            i += 1;
            continue;
        };
        if (i + seq_len > bytes.len) {
            w += 1;
            i += 1;
            continue;
        }
        const cp = std.unicode.utf8Decode(bytes[i .. i + seq_len]) catch {
            w += 1;
            i += 1;
            continue;
        };
        w += codepointWidth(cp);
        i += seq_len;
    }
    return w;
}

/// Print spaces for indentation
pub fn printSpaces(writer: anytype, count: u32) error{WriteFailed}!void {
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll(" ");
    }
}

/// Print leading whitespace from source line, preserving tabs.
/// Copies exact whitespace characters (tabs/spaces) from the line,
/// and uses spaces for non-whitespace characters.
/// This ensures underlines align correctly when the source contains tabs.
///
/// Parameters:
/// - writer: The output writer
/// - line: The source line text
/// - target_column: The 1-based column to print up to (exclusive)
pub fn printLeadingWhitespace(writer: anytype, line: []const u8, target_column: u32) error{WriteFailed}!void {
    if (target_column <= 1) return;

    const chars_to_print = target_column - 1;
    var i: u32 = 0;
    while (i < chars_to_print) : (i += 1) {
        if (i < line.len) {
            const char = line[i];
            if (char == '\t') {
                // Preserve tabs exactly
                try writer.writeAll("\t");
            } else if (char == ' ') {
                // Preserve spaces exactly
                try writer.writeAll(" ");
            } else {
                // Non-whitespace: use a space to maintain width
                try writer.writeAll(" ");
            }
        } else {
            // Past end of line: use spaces
            try writer.writeAll(" ");
        }
    }
}

// TESTS

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
    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    // Test width 1
    try formatLineNumber(&writer.writer, 5, 1);
    try testing.expectEqualStrings("5", writer.written());

    // Test width 3
    writer.clearRetainingCapacity();
    try formatLineNumber(&writer.writer, 5, 3);
    try testing.expectEqualStrings("  5", writer.written());

    // Test width 4 with large number
    writer.clearRetainingCapacity();
    try formatLineNumber(&writer.writer, 1234, 4);
    try testing.expectEqualStrings("1234", writer.written());
}

test "displayWidth" {
    // ASCII: one column per byte.
    try testing.expectEqual(@as(usize, 5), displayWidth("hello"));
    // Tab counts as a single column (rendered as one space).
    try testing.expectEqual(@as(usize, 4), displayWidth("\tfoo"));
    // 2-byte char (é) is one display column.
    try testing.expectEqual(@as(usize, 4), displayWidth("caf\u{00E9}"));
    // Wide CJK characters are two columns each.
    try testing.expectEqual(@as(usize, 4), displayWidth("\u{4E16}\u{754C}"));
    // Combining mark adds zero width (e + combining acute = 1 column).
    try testing.expectEqual(@as(usize, 1), displayWidth("e\u{0301}"));
    // Emoji is two columns.
    try testing.expectEqual(@as(usize, 2), displayWidth("\u{1F600}"));
}

test "calculateUnderlineLength" {
    // Normal case: end > start
    try testing.expectEqual(@as(u32, 5), calculateUnderlineLength(1, 6));

    // Single character
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(1, 1));

    // Edge case: end < start (should return 1)
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(5, 3));

    // Single character identifier
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(3, 4));
}

test "printSpaces" {
    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try printSpaces(&writer.writer, 0);
    try testing.expectEqualStrings("", writer.written());

    writer.clearRetainingCapacity();
    try printSpaces(&writer.writer, 5);
    try testing.expectEqualStrings("     ", writer.written());
}

test "integration - format source region" {
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
    try testing.expectEqual(@as(u32, 1), calculateUnderlineLength(10, 10));
}
