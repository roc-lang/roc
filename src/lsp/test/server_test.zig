const std = @import("std");
const server_module = @import("../server.zig");
const protocol = @import("../protocol.zig");
const transport_module = @import("../transport.zig");

fn frame(allocator: std.mem.Allocator, body: []const u8) ![]u8 {
    return try std.fmt.allocPrint(allocator, "Content-Length: {d}\r\n\r\n{s}", .{ body.len, body });
}

fn collectResponses(allocator: std.mem.Allocator, bytes: []const u8) ![][]u8 {
    var reader = std.io.fixedBufferStream(bytes);
    var sink_storage: [1]u8 = undefined;
    var sink = std.io.fixedBufferStream(&sink_storage);

    const ReaderType = @TypeOf(reader.reader());
    const WriterType = @TypeOf(sink.writer());
    var transport = transport_module.Transport(ReaderType, WriterType).init(allocator, reader.reader(), sink.writer(), null);

    var responses = std.ArrayList([]u8){};
    errdefer {
        for (responses.items) |body| allocator.free(body);
        responses.deinit(allocator);
    }

    while (true) {
        const message = transport.readMessage() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        try responses.append(allocator, message);
    }

    return responses.toOwnedSlice(allocator);
}

fn lifecycleInput(allocator: std.mem.Allocator) ![]u8 {
    const messages = [_][]const u8{
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":7,"rootUri":"file:///tmp","clientInfo":{"name":"test-client","version":"1.0.0"},"capabilities":{}}}
        ,
        \\{"jsonrpc":"2.0","method":"initialized","params":{}}
        ,
        \\{"jsonrpc":"2.0","id":2,"method":"shutdown"}
        ,
        \\{"jsonrpc":"2.0","method":"exit"}
        ,
    };

    var builder = std.ArrayList(u8){};
    errdefer builder.deinit(allocator);

    inline for (messages) |body| {
        const framed = try frame(allocator, body);
        defer allocator.free(framed);
        try builder.appendSlice(allocator, framed);
    }

    const data = try builder.toOwnedSlice(allocator);
    builder.deinit(allocator);
    return data;
}

test "server handles initialize/shutdown/exit handshake" {
    const allocator = std.testing.allocator;
    const input_bytes = try lifecycleInput(allocator);
    defer allocator.free(input_bytes);

    var reader_stream = std.io.fixedBufferStream(input_bytes);
    var writer_buffer: [4096]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null);
    defer server.deinit();

    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    try std.testing.expectEqual(@as(usize, 2), responses.len);

    {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, responses[0], .{});
        defer parsed.deinit();
        const result = parsed.value.object.get("result") orelse return error.MissingResult;
        const server_info = result.object.get("serverInfo") orelse return error.MissingServerInfo;
        const name_value = server_info.object.get("name") orelse return error.MissingServerName;
        try std.testing.expectEqualStrings("roc-lsp", name_value.string);
    }

    {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, responses[1], .{});
        defer parsed.deinit();
        const result = parsed.value.object.get("result") orelse return error.MissingResult;
        try std.testing.expect(result == .null);
    }
}

test "server rejects re-initialization requests" {
    const allocator = std.testing.allocator;
    const init =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    ;
    const reinit =
        \\{"jsonrpc":"2.0","id":3,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    ;
    const shutdown =
        \\{"jsonrpc":"2.0","id":2,"method":"shutdown"}
    ;
    const exit =
        \\{"jsonrpc":"2.0","method":"exit"}
    ;

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);

    for (&[_][]const u8{ init, reinit, shutdown, exit }) |body| {
        const framed = try frame(allocator, body);
        defer allocator.free(framed);
        try builder.appendSlice(allocator, framed);
    }

    const input_bytes = try builder.toOwnedSlice(allocator);
    defer allocator.free(input_bytes);

    var reader_stream = std.io.fixedBufferStream(input_bytes);
    var writer_buffer: [4096]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    try std.testing.expectEqual(@as(usize, 3), responses.len);

    var parsed_error = try std.json.parseFromSlice(std.json.Value, allocator, responses[1], .{});
    defer parsed_error.deinit();
    const error_obj = parsed_error.value.object.get("error") orelse return error.ExpectedError;
    try std.testing.expect(error_obj.object.get("code").?.integer == @intFromEnum(protocol.ErrorCode.invalid_request));
}
