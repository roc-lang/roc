//! Tests for LSP protocol types including JSON-RPC identifiers and initialization parameters.

const std = @import("std");
const protocol = @import("lsp").protocol;

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

test "JsonId enforces the LSP integer range" {
    const allocator = std.testing.allocator;

    var minimum = try protocol.JsonId.fromJsonValue(allocator, .{ .integer = std.math.minInt(i32) });
    defer minimum.deinit(allocator);
    var maximum = try protocol.JsonId.fromJsonValue(allocator, .{ .integer = std.math.maxInt(i32) });
    defer maximum.deinit(allocator);

    try std.testing.expectError(
        error.InvalidIdType,
        protocol.JsonId.fromJsonValue(allocator, .{ .integer = @as(i64, std.math.minInt(i32)) - 1 }),
    );
    try std.testing.expectError(
        error.InvalidIdType,
        protocol.JsonId.fromJsonValue(allocator, .{ .integer = @as(i64, std.math.maxInt(i32)) + 1 }),
    );
}

test "ErrorCode serializes the LSP protocol values" {
    const Case = struct {
        code: protocol.ErrorCode,
        expected: []const u8,
    };
    const cases = [_]Case{
        .{ .code = .parse_error, .expected = "-32700" },
        .{ .code = .invalid_request, .expected = "-32600" },
        .{ .code = .method_not_found, .expected = "-32601" },
        .{ .code = .invalid_params, .expected = "-32602" },
        .{ .code = .internal_error, .expected = "-32603" },
        .{ .code = .server_not_initialized, .expected = "-32002" },
        .{ .code = .request_failed, .expected = "-32803" },
        .{ .code = .server_cancelled, .expected = "-32802" },
        .{ .code = .content_modified, .expected = "-32801" },
        .{ .code = .request_cancelled, .expected = "-32800" },
    };

    for (cases) |case| {
        var writer: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer writer.deinit();
        try std.json.Stringify.value(case.code, .{}, &writer.writer);
        try std.testing.expectEqualStrings(case.expected, writer.written());
    }
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
    try std.testing.expect(std.mem.find(u8, params.capabilities_json.?, "textDocumentSync") != null);
}

test "InitializeParams rejects missing and malformed required fields" {
    const allocator = std.testing.allocator;
    const invalid_payloads = [_][]const u8{
        \\{"rootUri":null,"capabilities":{}}
        ,
        \\{"processId":null,"capabilities":{}}
        ,
        \\{"processId":null,"rootUri":null}
        ,
        \\{"processId":2147483648,"rootUri":null,"capabilities":{}}
        ,
        \\{"processId":-2147483649,"rootUri":null,"capabilities":{}}
        ,
        \\{"processId":null,"rootUri":42,"capabilities":{}}
        ,
        \\{"processId":null,"rootUri":null,"capabilities":[]}
        ,
        \\{"processId":null,"rootUri":null,"capabilities":{},"clientInfo":{"name":"test","version":null}}
        ,
    };

    for (invalid_payloads) |payload| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
        defer parsed.deinit();
        try std.testing.expectError(error.InvalidParams, protocol.InitializeParams.fromJson(allocator, parsed.value));
    }
}

test "InitializeParams cleans up partial allocations after OOM" {
    const allocator = std.testing.allocator;
    const payload =
        \\{"processId":1,"rootUri":"file:///tmp","capabilities":{},"clientInfo":{"name":"test"}}
    ;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    var failing_allocator = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 1 });
    try std.testing.expectError(
        error.OutOfMemory,
        protocol.InitializeParams.fromJson(failing_allocator.allocator(), parsed.value),
    );
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

    var writer: std.Io.Writer.Allocating = .init(allocator);
    defer writer.deinit();
    std.json.Stringify.value(tokens, .{}, &writer.writer) catch return error.OutOfMemory;
    const output = try writer.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expect(std.mem.find(u8, output, "\"data\"") != null);
    try std.testing.expect(std.mem.find(u8, output, "[0,0,5,7,0,0,6,3,3,0]") != null);
}

test "empty SemanticTokens serializes correctly" {
    const allocator = std.testing.allocator;
    const tokens = protocol.SemanticTokens{
        .data = &[_]u32{},
    };

    var writer: std.Io.Writer.Allocating = .init(allocator);
    defer writer.deinit();
    std.json.Stringify.value(tokens, .{}, &writer.writer) catch return error.OutOfMemory;
    const output = try writer.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expect(std.mem.find(u8, output, "\"data\":[]") != null);
}
