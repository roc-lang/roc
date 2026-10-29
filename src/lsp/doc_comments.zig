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
const Binding = @import("scope_map.zig").Binding;
const Allocator = std.mem.Allocator;

/// The doc comment block of the definition at `offset`, joined with newlines
/// and with bidirectional control characters made visible, or null if the
/// definition has none.
///
/// `offset` is where the definition starts: its type annotation when it has
/// one, otherwise its pattern.
///
/// The caller owns the returned memory and must free it with the provided allocator.
pub fn extractDocCommentBefore(allocator: Allocator, source: []const u8, offset: u32) Allocator.Error!?[]const u8 {
    const block = (try base.doc_comment.gatherBlockBefore(allocator, source, @min(offset, @as(u32, @intCast(source.len))))) orelse return null;
    defer block.deinit(allocator);

    var visible = std.Io.Writer.Allocating.init(allocator);
    defer visible.deinit();
    for (block.lines, 0..) |line, i| {
        if (i != 0) visible.writer.writeByte('\n') catch return error.OutOfMemory;
        base.bidi.writeVisible(&visible.writer, line) catch return error.OutOfMemory;
    }
    return try visible.toOwnedSlice();
}

// CIR-aware doc offset helpers

/// Where a binding declared by `pattern` with optional annotation `anno`
/// starts: the annotated name when the annotation records one, which sits on
/// the annotation line above the pattern, otherwise the pattern itself.
fn bindingStart(store: *const NodeStore, anno: ?CIR.Annotation.Idx, pattern: CIR.Pattern.Idx) u32 {
    if (anno) |anno_idx| {
        if (store.getAnnotation(anno_idx).name_region) |name_region| return name_region.start.offset;
    }
    return store.getPatternRegion(pattern).start.offset;
}

/// Compute the source offset where doc comments should be found for a Def.
pub fn docOffsetForDef(store: *const NodeStore, def: CIR.Def) u32 {
    return bindingStart(store, def.annotation, def.pattern);
}

/// Compute the source offset where doc comments should be found for a Statement.
/// Declarations start where `bindingStart` says; other statements start at
/// their own region.
pub fn docOffsetForStatement(store: *const NodeStore, stmt: CIR.Statement, stmt_idx: CIR.Statement.Idx) u32 {
    return switch (stmt) {
        .s_decl => |d| bindingStart(store, d.anno, d.pattern),
        .s_var => |v| bindingStart(store, v.anno, v.pattern_idx),
        .s_var_uninitialized => |v| bindingStart(store, v.anno, v.pattern_idx),
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

/// Extract doc comments for a scope binding from the declaration that owns it.
/// Parameters and pattern binders have no doc comments of their own.
pub fn extractDocForBinding(allocator: Allocator, source: []const u8, store: *const NodeStore, owner: Binding.Owner) Allocator.Error!?[]const u8 {
    return switch (owner) {
        .def => |def_idx| try extractDocForDef(allocator, source, store, store.getDef(def_idx)),
        .statement => |stmt_idx| try extractDocForStatement(allocator, source, store, store.getSourceStatement(stmt_idx), stmt_idx),
        .parameter, .binder => null,
    };
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

test "extractDocCommentBefore: an annotated definition starts at its annotation" {
    const allocator = std.testing.allocator;
    const source =
        \\## Adds two numbers together.
        \\## Returns the sum.
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
    ;
    // Find offset of the type annotation, where the definition starts
    const offset: u32 = @intCast(std.mem.find(u8, source, "add : I64") orelse return error.TestUnexpectedResult);
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
    const offset: u32 = @intCast(std.mem.find(u8, source, "len : Str") orelse return error.TestUnexpectedResult);
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

test "bidi doc comments display controls visibly" {
    const source = "## documentation \u{202e}\nvalue = 1\n";
    const doc = (try extractDocCommentBefore(std.testing.allocator, source, @intCast(std.mem.find(u8, source, "value").?))).?;
    defer std.testing.allocator.free(doc);
    try std.testing.expect(std.mem.find(u8, doc, "<U+202E RLO>") != null);
}
