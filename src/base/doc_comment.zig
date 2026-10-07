//! What a Roc doc comment is, and how the block documenting a definition is
//! gathered from source text.
//!
//! A doc comment line starts with `##`. A line starting with `###` is a
//! section-header comment: it documents nothing and is never part of a doc
//! block. Hover, completion and generated documentation all read doc comments
//! through this module, so they cannot disagree about what a definition's
//! documentation is.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Returns true if `trimmed` is a doc comment line: starts with `##` but not `###`.
pub fn isDocCommentLine(trimmed: []const u8) bool {
    if (trimmed.len < 2) return false;
    if (trimmed[0] != '#' or trimmed[1] != '#') return false;
    // Make sure it's not ### (section header)
    if (trimmed.len >= 3 and trimmed[2] == '#') return false;
    return true;
}

/// Strips the leading `##` and one optional following space from a line that
/// already begins with `##`, returning the doc comment content.
pub fn stripPrefix(line: []const u8) []const u8 {
    // Skip the ## prefix
    var start: usize = 2;

    // Skip a single space after ## if present (standard formatting)
    if (start < line.len and line[start] == ' ') {
        start += 1;
    }

    return line[start..];
}

/// The doc comment block documenting one definition.
pub const Block = struct {
    /// Byte offset at which the block's first line starts.
    start: u32,
    /// The content of each doc line from top to bottom, without its `##`
    /// prefix. Every line is a slice of the source the block was gathered from.
    lines: [][]const u8,

    /// Free the line list. The lines themselves belong to the source.
    pub fn deinit(self: Block, gpa: Allocator) void {
        gpa.free(self.lines);
    }
};

/// Gather the doc comment block for the definition starting at
/// `def_start_offset`, or null when it has none.
///
/// The block is the unbroken run of doc comment lines directly above the
/// definition's line:
///
/// - Blank lines between the definition and the block are skipped.
/// - A blank line above a doc line ends the block, so an earlier `##`
///   paragraph separated from it by a blank line is not part of it.
/// - Any other line ends the block: code, a `#` comment, and a `###` section
///   header alike. A `###` line is never documentation, so it neither joins
///   the block nor is skipped over to reach doc lines above it.
///
/// Only whitespace may precede the definition on its own line; a definition
/// that starts partway through a line of code has no doc block.
pub fn gatherBlockBefore(gpa: Allocator, source: []const u8, def_start_offset: u32) Allocator.Error!?Block {
    if (def_start_offset == 0 or def_start_offset > source.len) return null;

    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(gpa);

    var start: u32 = 0;
    var pos: usize = def_start_offset;

    // Step back over the definition's indentation to the end of the line above.
    while (pos > 0 and (source[pos - 1] == ' ' or source[pos - 1] == '\t' or source[pos - 1] == '\r')) {
        pos -= 1;
    }
    if (pos > 0 and source[pos - 1] == '\n') {
        pos -= 1;
        while (pos > 0 and source[pos - 1] == '\r') {
            pos -= 1;
        }
    }

    while (pos > 0) {
        var line_start = pos;
        while (line_start > 0 and source[line_start - 1] != '\n') {
            line_start -= 1;
        }

        const trimmed = std.mem.trimStart(u8, source[line_start..pos], " \t");
        if (isDocCommentLine(trimmed)) {
            // Lines are visited bottom-up, so the last one recorded is the
            // block's first line.
            start = @intCast(line_start);
            try lines.append(gpa, stripPrefix(trimmed));
        } else if (trimmed.len == 0) {
            if (lines.items.len > 0) break;
        } else {
            break;
        }

        if (line_start == 0) break;
        pos = line_start - 1;
        while (pos > 0 and source[pos - 1] == '\r') {
            pos -= 1;
        }
    }

    if (lines.items.len == 0) return null;

    std.mem.reverse([]const u8, lines.items);
    return .{ .start = start, .lines = try lines.toOwnedSlice(gpa) };
}

fn expectBlock(source: []const u8, def_text: []const u8, expected: ?[]const []const u8) (Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    const gpa = std.testing.allocator;
    const def_start: u32 = @intCast(std.mem.find(u8, source, def_text).?);
    const block = try gatherBlockBefore(gpa, source, def_start);
    defer if (block) |found| found.deinit(gpa);

    const expected_lines = expected orelse return std.testing.expect(block == null);
    try std.testing.expect(block != null);
    try std.testing.expectEqual(expected_lines.len, block.?.lines.len);
    for (expected_lines, block.?.lines) |expected_line, line| {
        try std.testing.expectEqualStrings(expected_line, line);
    }
}

test "gatherBlockBefore: the block is the run of doc lines above the definition" {
    try expectBlock("## one\n## two\nfoo = 1", "foo", &.{ "one", "two" });
    try expectBlock("## one\n\n\nfoo = 1", "foo", &.{"one"});
    try expectBlock("    ## indented\n    foo = 1", "foo", &.{"indented"});
    try expectBlock("## one\r\n## two\r\nfoo = 1", "foo", &.{ "one", "two" });
    try expectBlock("##\n##no space\nfoo = 1", "foo", &.{ "", "no space" });
    try expectBlock("foo = 1", "foo", null);
    try expectBlock("bar = 2\nfoo = 1", "foo", null);
}

test "gatherBlockBefore: a section header is not documentation" {
    // The header ends the block from above, and blocks reading down to it.
    try expectBlock("## a\n### b\n## c\nfoo = 1", "foo", &.{"c"});
    try expectBlock("### header\nfoo = 1", "foo", null);
    try expectBlock("## a\n### header\nfoo = 1", "foo", null);
}

test "gatherBlockBefore: blank lines, comments and code end the block" {
    try expectBlock("## earlier\n\n## doc\nfoo = 1", "foo", &.{"doc"});
    try expectBlock("# note\n## doc\nfoo = 1", "foo", &.{"doc"});
    try expectBlock("## doc\n# note\nfoo = 1", "foo", null);
    try expectBlock("## for bar\nbar = 2\nfoo = 1", "foo", null);
    try expectBlock("## doc\nbar = 2; foo = 1", "foo", null);
}

test "gatherBlockBefore: reports where the block starts" {
    const gpa = std.testing.allocator;
    const source = "x = 0\n\n## one\n## two\nfoo = 1";
    const block = (try gatherBlockBefore(gpa, source, @intCast(std.mem.find(u8, source, "foo").?))).?;
    defer block.deinit(gpa);
    try std.testing.expectEqual(@as(u32, @intCast(std.mem.find(u8, source, "## one").?)), block.start);
}

test "isDocCommentLine: various cases" {
    try std.testing.expect(isDocCommentLine("## doc"));
    try std.testing.expect(isDocCommentLine("##"));
    try std.testing.expect(isDocCommentLine("##doc"));
    try std.testing.expect(!isDocCommentLine("# comment"));
    try std.testing.expect(!isDocCommentLine("### header"));
    try std.testing.expect(!isDocCommentLine(""));
    try std.testing.expect(!isDocCommentLine("#"));
}

test "stripPrefix: with and without space and empty content" {
    try std.testing.expectEqualStrings("doc", stripPrefix("## doc"));
    try std.testing.expectEqualStrings("doc", stripPrefix("##doc"));
    try std.testing.expectEqualStrings("", stripPrefix("##"));
    try std.testing.expectEqualStrings("", stripPrefix("## "));
    try std.testing.expectEqualStrings(" doc", stripPrefix("##  doc"));
}
