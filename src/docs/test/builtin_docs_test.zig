//! Tests for the generated documentation of the compiler's own Builtin module.

const std = @import("std");
const can = @import("can");
const compiled_builtins = @import("compiled_builtins");

const DocModel = @import("../DocModel.zig");
const extract = @import("../extract.zig");

fn childNamed(entries: []const DocModel.DocEntry, name: []const u8) ?*const DocModel.DocEntry {
    for (entries) |*entry| {
        if (std.mem.eql(u8, entry.name, name)) return entry;
    }
    return null;
}

fn expectTags(entry: *const DocModel.DocEntry, expected: []const []const u8) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    const signature = entry.type_signature orelse return error.TestUnexpectedResult;
    const tags = signature.tag_union.tags;
    try std.testing.expectEqual(expected.len, tags.len);
    for (expected, tags) |expected_name, tag| {
        try std.testing.expectEqualStrings(expected_name, tag.name);
    }
}

test "Builtin docs present a declaration published through an alias under the alias" {
    const gpa = std.testing.allocator;
    var builtin_module = try can.BuiltinStatic.moduleView(gpa, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
    defer builtin_module.deinit();

    var docs = try extract.extractBuiltinDocs(gpa, builtin_module.env, "module", "Builtin.roc");
    defer docs.deinit(gpa);

    const str = childNamed(docs.entries, "Str") orelse return error.TestUnexpectedResult;

    const utf8 = childNamed(str.children, "Utf8Problem") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(DocModel.DocEntryKind.nominal, utf8.kind);
    try expectTags(utf8, &.{
        "InvalidStartByte",
        "UnexpectedEndOfSequence",
        "ExpectedContinuation",
        "OverlongEncoding",
        "CodepointTooLarge",
        "EncodesSurrogateHalf",
    });
    try std.testing.expectEqual(@as(usize, 1), utf8.children.len);
    try std.testing.expectEqualStrings("is_eq", utf8.children[0].name);

    const utf16 = childNamed(str.children, "Utf16Problem") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(DocModel.DocEntryKind.nominal, utf16.kind);
    try expectTags(utf16, &.{ "UnpairedHighSurrogate", "UnpairedLowSurrogate", "UnexpectedEndOfSequence" });
    try std.testing.expect(childNamed(utf16.children, "is_eq") != null);

    const utf32 = childNamed(str.children, "Utf32Problem") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(DocModel.DocEntryKind.nominal, utf32.kind);
    try expectTags(utf32, &.{ "CodePointTooLarge", "SurrogateCodePoint", "UnexpectedEndOfSequence" });
    try std.testing.expect(childNamed(utf32.children, "is_eq") != null);

    // The published declarations have no entry and no mention of their own.
    var rendered: std.Io.Writer.Allocating = .init(gpa);
    defer rendered.deinit();
    try docs.writeToSExpr(&rendered.writer, 0);
    try std.testing.expect(std.mem.find(u8, rendered.written(), "Utf8Problem") != null);
    try std.testing.expectEqual(@as(?usize, null), std.mem.find(u8, rendered.written(), "StrUtf"));
}
