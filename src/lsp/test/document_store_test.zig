//! Tests for the LSP document store.

const std = @import("std");
const DocumentStore = @import("lsp").document_store.DocumentStore;

test "document store upserts and retrieves documents" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "hello");
    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 1), doc.version);
    try std.testing.expectEqualStrings("hello", doc.text);
}

test "document store applies incremental changes" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "hello world");
    try store.applyRangeReplacement(
        "file:///test",
        2,
        .{ .start_line = 0, .start_character = 6, .end_line = 0, .end_character = 11 },
        "roc",
    );

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 2), doc.version);
    try std.testing.expectEqualStrings("hello roc", doc.text);
}

test "document store applies batched incremental changes" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "first");

    const changes = [_]DocumentStore.ContentChange{
        .{
            .text = "\nsecond line",
            .range = .{
                .start_line = 0,
                .start_character = 5,
                .end_line = 0,
                .end_character = 5,
            },
        },
        .{
            .text = "SECOND",
            .range = .{
                .start_line = 1,
                .start_character = 0,
                .end_line = 1,
                .end_character = 6,
            },
        },
    };

    try store.applyContentChanges("file:///test", 3, &changes);

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 3), doc.version);
    try std.testing.expectEqualStrings("first\nSECOND line", doc.text);
}

test "document store refuses a character past the end of its line" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "abc\ndefghijkl\nxyz");

    // Line 0 holds three characters. A client that miscounts and asks for
    // column 6 must be refused: resolving it against the rest of the document
    // would land inside line 1 and the edit would delete the newline with it.
    try std.testing.expectError(error.InvalidPosition, store.applyRangeReplacement(
        "file:///test",
        2,
        .{ .start_line = 0, .start_character = 6, .end_line = 0, .end_character = 6 },
        "!",
    ));

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 1), doc.version);
    try std.testing.expectEqualStrings("abc\ndefghijkl\nxyz", doc.text);
}

test "document store edits the end of a CRLF line" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "abc\r\ndef");

    // Column 3 is the end of line 0's content: the `\r` belongs to the EOL, not
    // to the line, so an insert there stays in front of it.
    try store.applyRangeReplacement(
        "file:///test",
        2,
        .{ .start_line = 0, .start_character = 3, .end_line = 0, .end_character = 3 },
        "!",
    );

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqualStrings("abc!\r\ndef", doc.text);
}
