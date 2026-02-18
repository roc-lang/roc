//! Tests for the LSP server lifecycle and request handling.

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
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
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
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
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

test "server tracks documents on didOpen/didChange" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "test.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app main = 0"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);
    const change_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":2}},"contentChanges":[{{"text":"app main = 42","range":{{"start":{{"line":0,"character":0}},"end":{{"line":0,"character":12}}}}}}]}}}}
    , .{file_uri});
    defer allocator.free(change_body);
    const change_msg = try frame(allocator, change_body);
    defer allocator.free(change_msg);

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);
    try builder.ensureTotalCapacity(allocator, open_msg.len + change_msg.len);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, change_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [8192]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    defer server.deinit();
    try server.run();

    const maybe_doc = server.getDocumentForTesting(file_uri);
    try std.testing.expect(maybe_doc != null);
    const doc = maybe_doc.?;
    try std.testing.expectEqualStrings("app main = 42", doc.text);
    try std.testing.expectEqual(@as(i64, 2), doc.version);
}

test "server applies sequential incremental changes in a single didChange" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "test.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"first"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const change_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":2}},"contentChanges":[
        \\  {{"text":"\nsecond line","range":{{"start":{{"line":0,"character":5}},"end":{{"line":0,"character":5}}}}}},
        \\  {{"text":"SECOND","range":{{"start":{{"line":1,"character":0}},"end":{{"line":1,"character":6}}}}}}
        \\]}}}}
    , .{file_uri});
    defer allocator.free(change_body);
    const change_msg = try frame(allocator, change_body);
    defer allocator.free(change_msg);

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);
    try builder.ensureTotalCapacity(allocator, open_msg.len + change_msg.len);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, change_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [8192]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    defer server.deinit();
    try server.run();

    const maybe_doc = server.getDocumentForTesting(file_uri);
    try std.testing.expect(maybe_doc != null);
    const doc = maybe_doc.?;
    try std.testing.expectEqualStrings("first\nSECOND line", doc.text);
    try std.testing.expectEqual(@as(i64, 2), doc.version);
}

test "server handles burst of incremental didChange messages" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "test.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":""}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Simulate rapid typing with multiple change events back-to-back.
    const change_body_1 = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":2}},"contentChanges":[
        \\  {{"text":"abc","range":{{"start":{{"line":0,"character":0}},"end":{{"line":0,"character":0}}}}}},
        \\  {{"text":" def","range":{{"start":{{"line":0,"character":3}},"end":{{"line":0,"character":3}}}}}}
        \\]}}}}
    , .{file_uri});
    defer allocator.free(change_body_1);
    const change_msg_1 = try frame(allocator, change_body_1);
    defer allocator.free(change_msg_1);

    const change_body_2 = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":3}},"contentChanges":[
        \\  {{"text":"\nline","range":{{"start":{{"line":0,"character":7}},"end":{{"line":0,"character":7}}}}}},
        \\  {{"text":"DEF","range":{{"start":{{"line":0,"character":4}},"end":{{"line":0,"character":7}}}}}}
        \\]}}}}
    , .{file_uri});
    defer allocator.free(change_body_2);
    const change_msg_2 = try frame(allocator, change_body_2);
    defer allocator.free(change_msg_2);

    const change_body_3 = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":4}},"contentChanges":[
        \\  {{"text":"!","range":{{"start":{{"line":1,"character":4}},"end":{{"line":1,"character":4}}}}}},
        \\  {{"text":"\nDONE","range":{{"start":{{"line":1,"character":5}},"end":{{"line":1,"character":5}}}}}}
        \\]}}}}
    , .{file_uri});
    defer allocator.free(change_body_3);
    const change_msg_3 = try frame(allocator, change_body_3);
    defer allocator.free(change_msg_3);

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);
    try builder.ensureTotalCapacity(allocator, open_msg.len + change_msg_1.len + change_msg_2.len + change_msg_3.len);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, change_msg_1);
    try builder.appendSlice(allocator, change_msg_2);
    try builder.appendSlice(allocator, change_msg_3);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [8192]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    defer server.deinit();
    try server.run();

    const maybe_doc = server.getDocumentForTesting(file_uri);
    try std.testing.expect(maybe_doc != null);
    const doc = maybe_doc.?;
    try std.testing.expectEqualStrings("abc DEF\nline!\nDONE", doc.text);
    try std.testing.expectEqual(@as(i64, 4), doc.version);
}

fn uriFromPath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return @import("../uri.zig").pathToUri(allocator, path);
}

test "server responds to semantic tokens request" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "test.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    // Full lifecycle: init -> initialized -> didOpen -> semanticTokens -> shutdown -> exit
    const init_body =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    ;
    const init_msg = try frame(allocator, init_body);
    defer allocator.free(init_msg);

    const initialized_body =
        \\{"jsonrpc":"2.0","method":"initialized","params":{}}
    ;
    const initialized_msg = try frame(allocator, initialized_body);
    defer allocator.free(initialized_msg);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = 42"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const tokens_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/semanticTokens/full","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(tokens_body);
    const tokens_msg = try frame(allocator, tokens_body);
    defer allocator.free(tokens_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":3,"method":"shutdown"}
    ;
    const shutdown_msg = try frame(allocator, shutdown_body);
    defer allocator.free(shutdown_msg);

    const exit_body =
        \\{"jsonrpc":"2.0","method":"exit"}
    ;
    const exit_msg = try frame(allocator, exit_body);
    defer allocator.free(exit_msg);

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, tokens_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [16384]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Should have: initialize response, semanticTokens response, shutdown response
    // (plus any diagnostics notifications from didOpen)
    try std.testing.expect(responses.len >= 3);

    // Find the semantic tokens response (id: 2)
    var found_tokens_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const data = result.object.get("data") orelse continue;
        try std.testing.expect(data == .array);
        // "x = 42" should produce tokens for x (variable), = (operator), 42 (number)
        // Each token is 5 integers, so we expect at least 15 integers
        try std.testing.expect(data.array.items.len >= 15);
        found_tokens_response = true;
        break;
    }
    try std.testing.expect(found_tokens_response);
}

test "server returns error for semantic tokens on unknown document" {
    const allocator = std.testing.allocator;

    const init_body =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    ;
    const init_msg = try frame(allocator, init_body);
    defer allocator.free(init_msg);

    const initialized_body =
        \\{"jsonrpc":"2.0","method":"initialized","params":{}}
    ;
    const initialized_msg = try frame(allocator, initialized_body);
    defer allocator.free(initialized_msg);

    const tokens_body =
        \\{"jsonrpc":"2.0","id":2,"method":"textDocument/semanticTokens/full","params":{"textDocument":{"uri":"file:///unknown.roc"}}}
    ;
    const tokens_msg = try frame(allocator, tokens_body);
    defer allocator.free(tokens_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":3,"method":"shutdown"}
    ;
    const shutdown_msg = try frame(allocator, shutdown_body);
    defer allocator.free(shutdown_msg);

    const exit_body =
        \\{"jsonrpc":"2.0","method":"exit"}
    ;
    const exit_msg = try frame(allocator, exit_body);
    defer allocator.free(exit_msg);

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, tokens_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [8192]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Find the error response (id: 2)
    var found_error = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const error_obj = parsed.value.object.get("error") orelse continue;
        try std.testing.expect(error_obj.object.get("code") != null);
        try std.testing.expect(error_obj.object.get("message") != null);
        found_error = true;
        break;
    }
    try std.testing.expect(found_error);
}

test "server returns empty tokens for empty document" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "empty.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const init_body =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    ;
    const init_msg = try frame(allocator, init_body);
    defer allocator.free(init_msg);

    const initialized_body =
        \\{"jsonrpc":"2.0","method":"initialized","params":{}}
    ;
    const initialized_msg = try frame(allocator, initialized_body);
    defer allocator.free(initialized_msg);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":""}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const tokens_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/semanticTokens/full","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(tokens_body);
    const tokens_msg = try frame(allocator, tokens_body);
    defer allocator.free(tokens_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":3,"method":"shutdown"}
    ;
    const shutdown_msg = try frame(allocator, shutdown_body);
    defer allocator.free(shutdown_msg);

    const exit_body =
        \\{"jsonrpc":"2.0","method":"exit"}
    ;
    const exit_msg = try frame(allocator, exit_body);
    defer allocator.free(exit_msg);

    var builder = std.ArrayList(u8){};
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, tokens_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [16384]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Find the semantic tokens response (id: 2)
    var found_tokens_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const data = result.object.get("data") orelse continue;
        try std.testing.expect(data == .array);
        // Empty document should produce empty tokens array
        try std.testing.expectEqual(@as(usize, 0), data.array.items.len);
        found_tokens_response = true;
        break;
    }
    try std.testing.expect(found_tokens_response);
}
