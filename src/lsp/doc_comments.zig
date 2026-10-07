//! Doc comments for hover and completion.
//!
//! The tokenizer drops comments, so documentation is read on demand from the
//! preserved source text. What counts as a definition's doc block is decided
//! by `base.doc_comment.gatherBlockBefore`, the same gatherer the documentation
//! generator uses.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const CIR = can.CIR;
const NodeStore = can.NodeStore;
const Allocator = std.mem.Allocator;

/// The doc comment block of the definition at `offset`, joined with newlines
/// and with bidirectional control characters made visible, or null if the
/// definition has none.
///
/// `offset` may be where the definition's pattern starts even when a type
/// annotation line sits between the doc block and the pattern.
///
/// The caller owns the returned memory and must free it with the provided allocator.
pub fn extractDocCommentBefore(allocator: Allocator, source: []const u8, offset: u32) Allocator.Error!?[]const u8 {
    const def_start = aboveAnnotationLines(source, @min(offset, @as(u32, @intCast(source.len))));
    const block = (try base.doc_comment.gatherBlockBefore(allocator, source, def_start)) orelse return null;
    defer block.deinit(allocator);

    var visible = std.Io.Writer.Allocating.init(allocator);
    defer visible.deinit();
    for (block.lines, 0..) |line, i| {
        if (i != 0) visible.writer.writeByte('\n') catch return error.OutOfMemory;
        base.bidi.writeVisible(&visible.writer, line) catch return error.OutOfMemory;
    }
    return try visible.toOwnedSlice();
}

/// Move `offset` up over the type annotation lines directly above its line.
///
/// Hover and completion sometimes know only where a binding's pattern starts,
/// not whether the binding is annotated:
///
///     ## Doc comment
///     add : I64, I64 -> I64
///     add = |a, b| a + b
///
/// The doc block of `add` sits above the annotation, so that is where
/// gathering has to start.
fn aboveAnnotationLines(source: []const u8, offset: u32) u32 {
    var current = offset;
    while (true) {
        const line_start = findLineStart(source, current);
        if (line_start == 0) return current;
        const above_start = findLineStart(source, line_start - 1);
        const above = std.mem.trim(u8, source[above_start .. line_start - 1], " \t\r");
        if (!isTypeAnnotation(above)) return current;
        current = above_start;
    }
}

/// Checks if a trimmed line is a type annotation (has ':' but no '=')
/// Type annotations in Roc look like: `add : I64, I64 -> I64`
fn isTypeAnnotation(trimmed: []const u8) bool {
    // A comment that mentions a colon is not an annotation.
    if (trimmed.len == 0 or trimmed[0] == '#') return false;

    // Look for ':' character
    const colon_pos = std.mem.findScalar(u8, trimmed, ':') orelse return false;

    // Make sure there's no '=' after the colon (which would indicate a definition like `x : I64 = 42`)
    const equals_pos = std.mem.findScalarPos(u8, trimmed, colon_pos, '=');
    return equals_pos == null;
}

/// Finds the start of the line containing the given position
fn findLineStart(source: []const u8, pos: u32) u32 {
    var i = pos;
    while (i > 0) {
        if (source[i - 1] == '\n') {
            return i;
        }
        i -= 1;
    }
    return 0;
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
        .s_var_uninitialized => |v| if (v.anno) |a|
            store.getAnnotationRegion(a).start.offset
        else
            store.getPatternRegion(v.pattern_idx).start.offset,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_alias_decl,
        .s_nominal_decl,
        .s_where_alias_decl,
        .s_type_anno,
        .s_type_var_alias,
        .s_runtime_error,
        => store.getStatementRegion(stmt_idx).start.offset,
    };
}

/// Extract doc comments for a Def (combines offset computation + source extraction).
pub fn extractDocForDef(allocator: Allocator, source: []const u8, store: *const NodeStore, def: CIR.Def) Allocator.Error!?[]const u8 {
    return extractDocCommentBefore(allocator, source, docOffsetForDef(store, def));
}

/// Extract doc comments for a Statement (combines offset computation + source extraction).
pub fn extractDocForStatement(allocator: Allocator, source: []const u8, store: *const NodeStore, stmt: CIR.Statement, stmt_idx: CIR.Statement.Idx) Allocator.Error!?[]const u8 {
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
    const offset: u32 = @intCast(std.mem.find(u8, source, "add = |a, b|") orelse unreachable);
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
    const offset: u32 = @intCast(std.mem.find(u8, source, "len : Str") orelse unreachable);
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

test "extractDocCommentBefore: agrees with the documentation generator about section headers" {
    const allocator = std.testing.allocator;
    const source = "## a\n### b\n## c\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, @intCast(std.mem.find(u8, source, "foo").?));
    defer if (result) |r| allocator.free(r);

    try std.testing.expectEqualStrings("c", result.?);
}

test "extractDocCommentBefore: a blank line ends the doc block" {
    const allocator = std.testing.allocator;
    const source = "## About something else.\n\n## Doc comment\nfoo = 42";
    const result = try extractDocCommentBefore(allocator, source, @intCast(std.mem.find(u8, source, "foo").?));
    defer if (result) |r| allocator.free(r);

    try std.testing.expectEqualStrings("Doc comment", result.?);
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

test "bidi doc comments display controls visibly" {
    const source = "## documentation \u{202e}\nvalue = 1\n";
    const doc = (try extractDocCommentBefore(std.testing.allocator, source, @intCast(std.mem.find(u8, source, "value").?))).?;
    defer std.testing.allocator.free(doc);
    try std.testing.expect(std.mem.find(u8, doc, "<U+202E RLO>") != null);
}
