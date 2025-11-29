const std = @import("std");
const DocumentStore = @import("../document_store.zig").DocumentStore;

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
