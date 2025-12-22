//! Tests for LSP protocol types including JSON-RPC identifiers and initialization parameters.

const std = @import("std");
const protocol = @import("../protocol.zig");

test "JsonId round-trips" {
    const allocator = std.testing.allocator;

    var id_int = try protocol.JsonId.fromJsonValue(allocator, .{ .integer = 42 });
    defer id_int.deinit(allocator);
    try std.testing.expectEqual(protocol.JsonId{ .integer = 42 }, id_int);

    var id_str = try protocol.JsonId.fromJsonValue(allocator, .{ .string = "abc" });
    defer id_str.deinit(allocator);
    try std.testing.expectEqualStrings("abc", id_str.string);

    var clone = try id_str.clone(allocator);
    defer clone.deinit(allocator);
    try std.testing.expectEqualStrings("abc", clone.string);
}

test "InitializeParams parses fields" {
    const allocator = std.testing.allocator;
    const payload =
        \\{
        \\  "processId": 7,
        \\  "rootUri": "file:///tmp",
        \\  "clientInfo": { "name": "roc-editor", "version": "0.1" },
        \\  "capabilities": { "textDocumentSync": 1 }
        \\}
    ;

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    var params = try protocol.InitializeParams.fromJson(allocator, parsed.value);
    defer params.deinit(allocator);

    try std.testing.expectEqual(@as(?i64, 7), params.process_id);
    try std.testing.expect(params.root_uri != null);
    try std.testing.expectEqualStrings("file:///tmp", params.root_uri.?);

    try std.testing.expect(params.client_info != null);
    try std.testing.expectEqualStrings("roc-editor", params.client_info.?.name);
    try std.testing.expectEqualStrings("0.1", params.client_info.?.version.?);

    try std.testing.expect(params.capabilities_json != null);
    try std.testing.expect(std.mem.indexOf(u8, params.capabilities_json.?, "textDocumentSync") != null);
}
