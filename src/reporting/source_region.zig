//! Utilities for formatting source code regions in diagnostic reports

const std = @import("std");
const testing = std.testing;

const document = @import("document.zig");

/// Allocation-free source layout consumed by every rendering style.
pub const Layout = struct {
    display: document.SourceCodeDisplayRegion,
    underlines: []const document.UnderlineRegion,
    highlight_source: bool,
    line_number_width: u32,
    /// Build the source layout from explicit coordinates.
    pub fn init(display: document.SourceCodeDisplayRegion, underlines: []const document.UnderlineRegion, highlight_source: bool) Layout {
        return .{ .display = display, .underlines = underlines, .highlight_source = highlight_source, .line_number_width = calculateLineNumberWidth(display.end_line) };
    }
    /// Iterate source rows without allocating.
    pub fn lines(self: Layout) Lines {
        return .{ .text = std.mem.splitScalar(u8, self.display.line_text, '\n'), .number = self.display.start_line, .underlines = self.underlines };
    }
};
/// Iterator over numbered source rows.
pub const Lines = struct {
    text: std.mem.SplitIterator(u8, .scalar),
    number: u32,
    underlines: []const document.UnderlineRegion,
    /// Return the next computed item, or null at the end.
    pub fn next(self: *Lines) ?Line {
        const text = self.text.next() orelse return null;
        const line = Line{ .text = text, .number = self.number, .underlines = self.underlines };
        self.number += 1;
        return line;
    }
};
/// A source row and its explicit underline regions.
pub const Line = struct {
    text: []const u8,
    number: u32,
    underlines: []const document.UnderlineRegion,
    /// Iterate caret spans belonging to this row.
    pub fn spans(self: Line) Spans {
        return .{ .remaining = self.underlines, .line_number = self.number };
    }
    /// Whether this row has a single-line underline.
    pub fn hasCarets(self: Line) bool {
        var iter = self.spans();
        return iter.next() != null;
    }
};
/// Computed caret run and the preceding cursor position.
pub const Span = struct { previous_column: u32, start_column: u32, length: u32, annotation: document.Annotation };
/// Iterator selecting single-line underline regions in document order.
pub const Spans = struct {
    remaining: []const document.UnderlineRegion,
    line_number: u32,
    column: u32 = 1,
    /// Return the next computed item, or null at the end.
    pub fn next(self: *Spans) ?Span {
        while (self.remaining.len > 0) {
            const region = self.remaining[0];
            self.remaining = self.remaining[1..];
            if (!underlineAppliesToLine(region.start_line, region.end_line, self.line_number)) continue;
            const span = Span{ .previous_column = self.column, .start_column = region.start_column, .length = calculateUnderlineLength(region.start_column, region.end_column), .annotation = region.annotation };
            self.column = region.end_column;
            return span;
        }
        return null;
    }
};

/// Report snippets expand leading tabs, dedent all rows equally, and transform
/// underline coordinates alongside source text.
pub const Snippet = struct {
    rows: std.array_list.Managed([]u8),
    common: usize,
    start_column: u32,
    end_column: u32,
    /// Build the source layout from explicit coordinates.
    pub fn init(gpa: std.mem.Allocator, text: []const u8, start_column: u32, end_column: u32) std.mem.Allocator.Error!Snippet {
        var rows = std.array_list.Managed([]u8).init(gpa);
        errdefer {
            for (rows.items) |row| gpa.free(row);
            rows.deinit();
        }
        var common: usize = std.math.maxInt(usize);
        var first_expanded: usize = 0;
        var first_original: usize = 0;
        var iter = std.mem.splitScalar(u8, text, '\n');
        while (iter.next()) |source_line| {
            var original: usize = 0;
            var expanded: usize = 0;
            while (original < source_line.len and (source_line[original] == ' ' or source_line[original] == '\t')) : (original += 1) {
                expanded += if (source_line[original] == '\t') 4 else 1;
            }
            if (rows.items.len == 0) {
                first_expanded = expanded;
                first_original = original;
            }
            if (original < source_line.len) common = @min(common, expanded);
            const row = try gpa.alloc(u8, expanded + source_line.len - original);
            errdefer gpa.free(row);
            @memset(row[0..expanded], ' ');
            @memcpy(row[expanded..], source_line[original..]);
            try rows.append(row);
        }
        if (common == std.math.maxInt(usize)) common = 0;
        const delta = @as(i64, @intCast(first_expanded)) - @as(i64, @intCast(common)) - @as(i64, @intCast(first_original));
        return .{ .rows = rows, .common = common, .start_column = @intCast(@max(@as(i64, start_column) + delta, 1)), .end_column = @intCast(@max(@as(i64, end_column) + delta, 1)) };
    }
    /// Release the prepared source rows.
    pub fn deinit(self: *Snippet) void {
        for (self.rows.items) |row| self.rows.allocator.free(row);
        self.rows.deinit();
    }
    /// Return a row with the common indentation removed.
    pub fn line(self: Snippet, row: []const u8) []const u8 {
        return row[@min(self.common, row.len)..];
    }
    /// Map byte coordinates to displayed columns, retaining one caret for empty spans.
    pub fn caret(self: Snippet, text: []const u8) struct { padding: u32, length: usize } {
        const start = @min(@as(usize, self.start_column -| 1), text.len);
        const end = @max(@min(@as(usize, self.end_column -| 1), text.len), start);
        return .{ .padding = @intCast(displayWidth(text[0..start])), .length = @max(displayWidth(text[start..end]), 1) };
    }
};

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

/// Whether an underline region draws a caret row under the source line
/// numbered `line_num`: only single-line regions on exactly that line do.
pub fn underlineAppliesToLine(start_line: u32, end_line: u32, line_num: u32) bool {
    return start_line == line_num and start_line == end_line;
}

/// Write the padding that precedes an underline span on a caret row, advancing
/// from `col_position` (1-based; 1 means nothing has been written on this row
/// yet) up to `start_column`. The row's first gap mirrors the source line's own
/// characters via `printLeadingWhitespace`—preserving tabs so the carets land
/// under a tabbed line—while gaps between spans are plain spaces.
pub fn printUnderlineGap(writer: anytype, line: []const u8, col_position: u32, start_column: u32) error{WriteFailed}!void {
    if (start_column <= col_position) return;
    if (col_position == 1) {
        try printLeadingWhitespace(writer, line, start_column);
    } else {
        try printSpaces(writer, start_column - col_position);
    }
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
/// for multi-byte UTF-8, wide/zero-width characters, and tabs (which the terminal
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

test "report snippet transforms tabs and Unicode underline columns together" {
    var snippet = try Snippet.init(testing.allocator, "\t  café\n\t    next", 4, 9);
    defer snippet.deinit();
    try testing.expectEqualStrings("café", snippet.line(snippet.rows.items[0]));
    try testing.expectEqualStrings("  next", snippet.line(snippet.rows.items[1]));
    const caret = snippet.caret(snippet.line(snippet.rows.items[0]));
    try testing.expectEqual(@as(u32, 0), caret.padding);
    try testing.expectEqual(@as(usize, 4), caret.length);
    snippet.start_column = 20;
    snippet.end_column = 2;
    const reversed = snippet.caret(snippet.line(snippet.rows.items[0]));
    try testing.expectEqual(@as(u32, 4), reversed.padding);
    try testing.expectEqual(@as(usize, 1), reversed.length);
}

test "layout selects single-line caret spans and advances source rows" {
    const regions = [_]document.UnderlineRegion{
        .{ .start_line = 8, .end_line = 8, .start_column = 2, .end_column = 4, .annotation = .error_highlight },
        .{ .start_line = 8, .end_line = 8, .start_column = 6, .end_column = 6, .annotation = .warning_highlight },
        .{ .start_line = 8, .end_line = 9, .start_column = 1, .end_column = 5, .annotation = .error_highlight },
    };
    const layout = Layout.init(.{ .filename = null, .start_line = 8, .end_line = 9, .start_column = 1, .end_column = 5, .line_text = "\tvalue\nnext", .region_annotation = .source_region }, &regions, false);
    var lines = layout.lines();
    const first = lines.next().?;
    try testing.expectEqual(@as(u32, 8), first.number);
    var spans = first.spans();
    const a = spans.next().?;
    try testing.expectEqual(@as(u32, 1), a.previous_column);
    try testing.expectEqual(@as(u32, 2), a.length);
    const b = spans.next().?;
    try testing.expectEqual(@as(u32, 4), b.previous_column);
    try testing.expectEqual(@as(u32, 1), b.length);
    try testing.expectEqual(document.Annotation.warning_highlight, b.annotation);
    try testing.expect(spans.next() == null);
    const second = lines.next().?;
    try testing.expectEqual(@as(u32, 9), second.number);
    try testing.expect(!second.hasCarets());
    try testing.expect(lines.next() == null);
}
