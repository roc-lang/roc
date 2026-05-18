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

test "SemanticTokensParams parses textDocument.uri" {
    const allocator = std.testing.allocator;
    const payload =
        \\{"textDocument":{"uri":"file:///test.roc"}}
    ;

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    var params = try protocol.SemanticTokensParams.fromJson(allocator, parsed.value);
    defer params.deinit(allocator);

    try std.testing.expectEqualStrings("file:///test.roc", params.textDocument.uri);
}

test "TextDocumentIdentifier parses uri" {
    const allocator = std.testing.allocator;
    const payload =
        \\{"uri":"file:///path/to/file.roc"}
    ;

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    var doc = try protocol.TextDocumentIdentifier.fromJson(allocator, parsed.value);
    defer doc.deinit(allocator);

    try std.testing.expectEqualStrings("file:///path/to/file.roc", doc.uri);
}

test "SemanticTokens serializes data array" {
    const allocator = std.testing.allocator;
    const tokens = protocol.SemanticTokens{
        .data = &[_]u32{ 0, 0, 5, 7, 0, 0, 6, 3, 3, 0 },
    };

    var writer: std.io.Writer.Allocating = .init(allocator);
    defer writer.deinit();
    std.json.Stringify.value(tokens, .{}, &writer.writer) catch return error.OutOfMemory;
    const output = try writer.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "\"data\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[0,0,5,7,0,0,6,3,3,0]") != null);
}

test "empty SemanticTokens serializes correctly" {
    const allocator = std.testing.allocator;
    const tokens = protocol.SemanticTokens{
        .data = &[_]u32{},
    };

    var writer: std.io.Writer.Allocating = .init(allocator);
    defer writer.deinit();
    std.json.Stringify.value(tokens, .{}, &writer.writer) catch return error.OutOfMemory;
    const output = try writer.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "\"data\":[]") != null);
}
