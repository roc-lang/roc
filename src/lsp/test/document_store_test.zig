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

test "document store applies UTF-16 incremental changes" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "aé😀z");
    try store.applyRangeReplacement(
        "file:///test",
        2,
        .{ .start_line = 0, .start_character = 2, .end_line = 0, .end_character = 4 },
        "roc",
    );

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqualStrings("aérocz", doc.text);
}

test "document store preserves the original document after a later batch change fails" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "first");
    const changes = [_]DocumentStore.ContentChange{
        .{ .text = "!", .range = .{ .start_line = 0, .start_character = 5, .end_line = 0, .end_character = 5 } },
        .{ .text = "?", .range = .{ .start_line = 9, .start_character = 0, .end_line = 9, .end_character = 0 } },
    };

    try std.testing.expectError(error.InvalidPosition, store.applyContentChanges("file:///test", 2, &changes));
    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 1), doc.version);
    try std.testing.expectEqualStrings("first", doc.text);
}

test "document store rejects positions beyond u32" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "text");
    try std.testing.expectError(error.InvalidPosition, store.applyRangeReplacement(
        "file:///test",
        2,
        .{
            .start_line = std.math.maxInt(usize),
            .start_character = 0,
            .end_line = std.math.maxInt(usize),
            .end_character = 0,
        },
        "!",
    ));
}

test "document store upsert leaves no partial entry on allocation failure" {
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 2 });
    var store = DocumentStore.init(failing.allocator());
    defer store.deinit();

    try std.testing.expectError(error.OutOfMemory, store.upsert("file:///test", 1, "text"));
    try std.testing.expect(store.get("file:///test") == null);
}

test "document store updates without allocating another URI" {
    var store = DocumentStore.init(std.testing.allocator);
    defer store.deinit();
    try store.upsert("file:///test", 1, "old");

    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 2 });
    store.allocator = failing.allocator();
    try store.upsert("file:///test", 2, "new");
    store.allocator = std.testing.allocator;

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 2), doc.version);
    try std.testing.expectEqualStrings("new", doc.text);
}

test "document store frees temporary text when copying line starts fails" {
    var store = DocumentStore.init(std.testing.allocator);
    defer store.deinit();
    try store.upsert("file:///test", 1, "text");

    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 1 });
    store.allocator = failing.allocator();
    try std.testing.expectError(error.OutOfMemory, store.applyContentChanges("file:///test", 2, &.{.{ .text = "next" }}));
    store.allocator = std.testing.allocator;

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 1), doc.version);
    try std.testing.expectEqualStrings("text", doc.text);
}

test "document store cleans up after allocating temporary copies" {
    var store = DocumentStore.init(std.testing.allocator);
    defer store.deinit();
    try store.upsert("file:///test", 1, "text");

    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 2 });
    store.allocator = failing.allocator();
    defer store.allocator = std.testing.allocator;

    try std.testing.expectError(error.OutOfMemory, store.applyContentChanges("file:///test", 2, &.{.{ .text = "next" }}));

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 1), doc.version);
    try std.testing.expectEqualStrings("text", doc.text);
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

test "document store clamps an incremental edit to its line end" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "abc\ndefghijkl\nxyz");

    // Line 0 holds three characters. A position beyond it resolves to the end
    // of that line, never into line 1.
    try store.applyRangeReplacement(
        "file:///test",
        2,
        .{ .start_line = 0, .start_character = 6, .end_line = 0, .end_character = 6 },
        "!",
    );

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqual(@as(i64, 2), doc.version);
    try std.testing.expectEqualStrings("abc!\ndefghijkl\nxyz", doc.text);
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

test "document store clamps an incremental edit to a CRLF line end" {
    const allocator = std.testing.allocator;
    var store = DocumentStore.init(allocator);
    defer store.deinit();

    try store.upsert("file:///test", 1, "abc\r\ndef");
    try store.applyRangeReplacement(
        "file:///test",
        2,
        .{ .start_line = 0, .start_character = 99, .end_line = 0, .end_character = 99 },
        "!",
    );

    const doc = store.get("file:///test") orelse return error.MissingDocument;
    try std.testing.expectEqualStrings("abc!\r\ndef", doc.text);
}
