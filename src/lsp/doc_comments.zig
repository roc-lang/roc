//! Doc Comment Extraction Utility
//!
//! This module provides utilities for extracting documentation comments from Roc source code.
//! In Roc, doc comments use the `##` prefix (not `###` which is a section header comment).
//! Multiple consecutive doc comment lines are joined together.
//!
//! Since the tokenizer strips all comments, this module extracts doc comments on-demand
//! from the preserved source text using region information.

const std = @import("std");
const can = @import("can");
const CIR = can.CIR;
const NodeStore = can.NodeStore;
const Allocator = std.mem.Allocator;

/// Extracts doc comments preceding a given byte offset in source code.
/// Returns an allocated string containing the doc comment text (with `## ` prefixes stripped),
/// or null if no doc comments are found.
///
/// Doc comments in Roc use `##` prefix (not `###` which is just a section header comment).
/// Multiple consecutive doc comment lines are joined with newlines.
///
/// The caller owns the returned memory and must free it with the provided allocator.
pub fn extractDocCommentBefore(allocator: Allocator, source: []const u8, offset: u32) !?[]const u8 {
    if (source.len == 0 or offset == 0) return null;

    // Clamp offset to source bounds
    const safe_offset = @min(offset, @as(u32, @intCast(source.len)));

    // Find the start of the line containing the definition
    const def_line_start = findLineStart(source, safe_offset);

    // Now scan backwards from the definition line to find doc comments
    // We need to find lines that start with `##` (but not `###`)
    var doc_lines: std.ArrayListUnmanaged([]const u8) = .empty;
    defer doc_lines.deinit(allocator);

    var current_pos = def_line_start;

    // Skip backwards through any blank lines or whitespace-only lines between
    // the definition and doc comments
    while (current_pos > 0) {
        // Find the start of the previous line
        const prev_line_end = skipBackwardsToNewline(source, current_pos);
        if (prev_line_end == 0) break;

        const prev_line_start = findLineStart(source, prev_line_end - 1);
        const prev_line = source[prev_line_start..current_pos];

        // Check if this line is a doc comment, whitespace-only, or something else
        const trimmed = std.mem.trim(u8, prev_line, " \t\r\n");

        if (trimmed.len == 0) {
            // Empty/whitespace line - continue scanning backwards
            // But only allow one blank line gap between doc comments and definition
            current_pos = prev_line_start;
            continue;
        }

        if (isDocCommentLine(trimmed)) {
            // Found a doc comment line - extract the content
            const content = extractDocContent(trimmed);
            try doc_lines.append(allocator, content);
            current_pos = prev_line_start;
        } else if (isRegularComment(trimmed)) {
            // Regular comment (single #) or section header (###) - stop searching
            break;
        } else if (isTypeAnnotation(trimmed)) {
            // Type annotation line - skip over it and continue scanning backwards
            // This handles cases like:
            //   ## Doc comment
            //   add : I64, I64 -> I64
            //   add = |a, b| a + b
            current_pos = prev_line_start;
        } else {
            // Non-comment, non-whitespace content - stop searching
            break;
        }
    }

    if (doc_lines.items.len == 0) return null;

    // Reverse the lines (we collected them bottom-to-top)
    std.mem.reverse([]const u8, doc_lines.items);

    // Join all doc comment lines with newlines
    return try std.mem.join(allocator, "\n", doc_lines.items);
}

/// Checks if a trimmed line is a type annotation (has ':' but no '=')
/// Type annotations in Roc look like: `add : I64, I64 -> I64`
fn isTypeAnnotation(trimmed: []const u8) bool {
    // Look for ':' character
    const colon_pos = std.mem.indexOfScalar(u8, trimmed, ':') orelse return false;

    // Make sure there's no '=' after the colon (which would indicate a definition like `x : I64 = 42`)
    const equals_pos = std.mem.indexOfScalarPos(u8, trimmed, colon_pos, '=');
    return equals_pos == null;
}

/// Checks if a trimmed line is a doc comment (starts with ## but not ###)
fn isDocCommentLine(trimmed: []const u8) bool {
    if (trimmed.len < 2) return false;
    if (trimmed[0] != '#' or trimmed[1] != '#') return false;
    // Make sure it's not ### (section header)
    if (trimmed.len >= 3 and trimmed[2] == '#') return false;
    return true;
}

/// Checks if a line is a regular comment (single #) or section header (###)
fn isRegularComment(trimmed: []const u8) bool {
    if (trimmed.len == 0) return false;
    if (trimmed[0] != '#') return false;
    // Single # is regular comment
    if (trimmed.len == 1) return true;
    if (trimmed[1] != '#') return true;
    // ### is section header
    if (trimmed.len >= 3 and trimmed[2] == '#') return true;
    return false;
}

/// Extracts the content from a doc comment line, stripping the ## prefix
fn extractDocContent(line: []const u8) []const u8 {
    // Skip the ## prefix
    var start: usize = 2;

    // Skip a single space after ## if present (standard formatting)
    if (start < line.len and line[start] == ' ') {
        start += 1;
    }

    return line[start..];
}

/// Finds the start of the line containing the given position
fn findLineStart(source: []const u8, pos: u32) u32 {
    if (pos == 0) return 0;

    var i = pos;
    while (i > 0) {
        if (source[i - 1] == '\n') {
            return i;
        }
        i -= 1;
    }
    return 0;
}

/// Skips backwards from a position to find the newline before the current line
/// Returns the position just before the newline, or 0 if at start of file
fn skipBackwardsToNewline(source: []const u8, pos: u32) u32 {
    if (pos == 0) return 0;

    var i = pos;
    // Skip any trailing whitespace/newlines at current position
    while (i > 0 and (source[i - 1] == '\n' or source[i - 1] == '\r')) {
        i -= 1;
    }
    return i;
}

// CIR-aware doc offset helpers

/// Compute the source offset where doc comments should be found for a Def.
/// Prefers annotation region (doc comments precede type annotations),
/// otherwise falls back to pattern region.
pub fn docOffsetForDef(store: *const NodeStore, def: CIR.Def) u32 {
    if (def.annotation) |anno_idx|
        return store.getAnnotationRegion(anno_idx).start.offset;
    return store.getPatternRegion(def.pattern).start.offset;
}

/// Compute the source offset where doc comments should be found for a Statement.
/// Handles annotation-bearing statements (s_decl, s_var) by preferring the
/// annotation region, and falls back to the statement region for other types.
pub fn docOffsetForStatement(store: *const NodeStore, stmt: CIR.Statement, stmt_idx: CIR.Statement.Idx) u32 {
    return switch (stmt) {
        .s_decl => |d| if (d.anno) |a|
            store.getAnnotationRegion(a).start.offset
        else
            store.getPatternRegion(d.pattern).start.offset,
        .s_var => |v| if (v.anno) |a|
            store.getAnnotationRegion(a).start.offset
        else
            store.getPatternRegion(v.pattern_idx).start.offset,
        else => store.getStatementRegion(stmt_idx).start.offset,
    };
}

/// Extract doc comments for a Def (combines offset computation + source extraction).
pub fn extractDocForDef(allocator: Allocator, source: []const u8, store: *const NodeStore, def: CIR.Def) !?[]const u8 {
    return extractDocCommentBefore(allocator, source, docOffsetForDef(store, def));
}

/// Extract doc comments for a Statement (combines offset computation + source extraction).
pub fn extractDocForStatement(allocator: Allocator, source: []const u8, store: *const NodeStore, stmt: CIR.Statement, stmt_idx: CIR.Statement.Idx) !?[]const u8 {
    return extractDocCommentBefore(allocator, source, docOffsetForStatement(store, stmt, stmt_idx));
}

// Unit Tests

test "extractDocCommentBefore: single line doc comment" {
    const allocator = std.testing.allocator;
    const source = "## This is a doc comment\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 25); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("This is a doc comment", result.?);
}

test "extractDocCommentBefore: multi-line doc comment" {
    const allocator = std.testing.allocator;
    const source = "## Line 1\n## Line 2\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 20); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("Line 1\nLine 2", result.?);
}

test "extractDocCommentBefore: no doc comment" {
    const allocator = std.testing.allocator;
    const source = "# Regular comment\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 18); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result == null);
}

test "extractDocCommentBefore: ignores ### section headers" {
    const allocator = std.testing.allocator;
    const source = "### Section Header\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 19); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result == null);
}

test "extractDocCommentBefore: handles blank line between doc and definition" {
    const allocator = std.testing.allocator;
    const source = "## A doc comment\n\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 18); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("A doc comment", result.?);
}

test "extractDocCommentBefore: stops at non-doc content" {
    const allocator = std.testing.allocator;
    const source = "bar = 1\n## Doc for foo\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 23); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("Doc for foo", result.?);
}

test "extractDocCommentBefore: handles doc with no space after ##" {
    const allocator = std.testing.allocator;
    const source = "##NoSpace\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 10); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("NoSpace", result.?);
}

test "extractDocCommentBefore: handles empty doc comment" {
    const allocator = std.testing.allocator;
    const source = "##\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 3); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("", result.?);
}

test "extractDocCommentBefore: first definition in file" {
    const allocator = std.testing.allocator;
    const source = "## First definition\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 20); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("First definition", result.?);
}

test "extractDocCommentBefore: definition at start of file (no docs)" {
    const allocator = std.testing.allocator;
    const source = "foo = 42";
    const result = try extractDocCommentBefore(allocator, source, 0);
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result == null);
}

test "extractDocCommentBefore: stops at regular comment before doc" {
    const allocator = std.testing.allocator;
    const source = "# Just a comment\n## Doc comment\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, 32); // offset of "foo"
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    // Should only get the doc comment, not the regular comment
    try std.testing.expectEqualStrings("Doc comment", result.?);
}

test "extractDocCommentBefore: handles type annotation between doc and definition" {
    const allocator = std.testing.allocator;
    const source =
        \\## Adds two numbers together.
        \\## Returns the sum.
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
    ;
    // Find offset of the definition line (not the type annotation)
    const offset: u32 = @intCast(std.mem.indexOf(u8, source, "add = |a, b|") orelse unreachable);
    const result = try extractDocCommentBefore(allocator, source, offset);
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("Adds two numbers together.\nReturns the sum.", result.?);
}

test "extractDocCommentBefore: complex multi-line with formatting" {
    const allocator = std.testing.allocator;
    const source =
        \\## Returns the length of a string.
        \\## 
        \\## Example:
        \\## ```roc
        \\## Str.len("hello") == 5
        \\## ```
        \\len : Str -> U64
    ;
    // Find offset of "len"
    const offset: u32 = @intCast(std.mem.indexOf(u8, source, "len : Str") orelse unreachable);
    const result = try extractDocCommentBefore(allocator, source, offset);
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    const expected =
        \\Returns the length of a string.
        \\
        \\Example:
        \\```roc
        \\Str.len("hello") == 5
        \\```
    ;
    try std.testing.expectEqualStrings(expected, result.?);
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

test "isRegularComment: various cases" {
    try std.testing.expect(isRegularComment("# comment"));
    try std.testing.expect(isRegularComment("#"));
    try std.testing.expect(isRegularComment("### header"));
    try std.testing.expect(!isRegularComment("## doc"));
    try std.testing.expect(!isRegularComment("code"));
    try std.testing.expect(!isRegularComment(""));
}

test "isTypeAnnotation: various cases" {
    try std.testing.expect(isTypeAnnotation("add : I64, I64 -> I64"));
    try std.testing.expect(isTypeAnnotation("len : Str -> U64"));
    try std.testing.expect(isTypeAnnotation("identity : a -> a"));
    try std.testing.expect(!isTypeAnnotation("add = |a, b| a + b"));
    try std.testing.expect(!isTypeAnnotation("x : I64 = 42")); // definition with type, not just annotation
    try std.testing.expect(!isTypeAnnotation("## doc comment"));
    try std.testing.expect(!isTypeAnnotation("no colon here"));
}
