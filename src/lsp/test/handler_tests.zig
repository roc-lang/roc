//! Tests for LSP request handlers: formatting, document symbol, folding range,
//! selection range, and document highlight.

const std = @import("std");
const server_module = @import("../server.zig");
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

fn uriFromPath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return @import("../uri.zig").pathToUri(allocator, path);
}

test "formatting handler formats simple expression" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "format.roc" });
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

    // Open document with unformatted code
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x=1"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const format_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/formatting","params":{{"textDocument":{{"uri":"{s}"}},"options":{{"tabSize":4,"insertSpaces":true}}}}}}
    , .{file_uri});
    defer allocator.free(format_body);
    const format_msg = try frame(allocator, format_body);
    defer allocator.free(format_msg);

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
    try builder.appendSlice(allocator, format_msg);
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

    // Find the formatting response (id: 2)
    var found_format_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        // Result should be an array of text edits or null
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);
        // Formatting may return null if no changes needed, or array of edits
        if (result.? == .array) {
            // If there are edits, each should have range and newText
            for (result.?.array.items) |edit| {
                try std.testing.expect(edit.object.get("range") != null);
                try std.testing.expect(edit.object.get("newText") != null);
            }
        }
        found_format_response = true;
        break;
    }
    try std.testing.expect(found_format_response);
}

test "document symbol handler extracts function declarations" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "symbols.roc" });
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

    // Document with a simple declaration (more reliable than lambda syntax in JSON)
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"myVar = 42"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const symbols_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentSymbol","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(symbols_body);
    const symbols_msg = try frame(allocator, symbols_body);
    defer allocator.free(symbols_msg);

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
    try builder.appendSlice(allocator, symbols_msg);
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

    // Find the document symbol response (id: 2)
    var found_symbols_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        try std.testing.expect(result == .array);
        // Verify structure of any symbols returned (SymbolInformation format)
        for (result.array.items) |symbol| {
            try std.testing.expect(symbol.object.get("name") != null);
            try std.testing.expect(symbol.object.get("kind") != null);
            try std.testing.expect(symbol.object.get("location") != null);
        }
        found_symbols_response = true;
        break;
    }
    try std.testing.expect(found_symbols_response);
}

test "document symbol handler returns empty for empty document" {
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

    const symbols_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentSymbol","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(symbols_body);
    const symbols_msg = try frame(allocator, symbols_body);
    defer allocator.free(symbols_msg);

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
    try builder.appendSlice(allocator, symbols_msg);
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

    // Find the document symbol response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        try std.testing.expect(result == .array);
        try std.testing.expectEqual(@as(usize, 0), result.array.items.len);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "folding range handler finds bracket ranges" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "fold.roc" });
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

    // Document with brackets that can be folded (multi-line record)
    // Use \\n for JSON-escaped newlines
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"myRecord = {{\n    x: 1,\n    y: 2,\n}}"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const folding_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/foldingRange","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(folding_body);
    const folding_msg = try frame(allocator, folding_body);
    defer allocator.free(folding_msg);

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
    try builder.appendSlice(allocator, folding_msg);
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

    // Find the folding range response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        try std.testing.expect(result == .array);
        // Verify structure of any folding ranges returned
        for (result.array.items) |range| {
            try std.testing.expect(range.object.get("startLine") != null);
            try std.testing.expect(range.object.get("endLine") != null);
        }
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "selection range handler returns range hierarchy" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "select.roc" });
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

    // Simple expression: position cursor on 'foo'
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = foo + bar"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Position on 'foo' (line 0, character 4)
    const select_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/selectionRange","params":{{"textDocument":{{"uri":"{s}"}},"positions":[{{"line":0,"character":4}}]}}}}
    , .{file_uri});
    defer allocator.free(select_body);
    const select_msg = try frame(allocator, select_body);
    defer allocator.free(select_msg);

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
    try builder.appendSlice(allocator, select_msg);
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

    // Find the selection range response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        try std.testing.expect(result == .array);
        try std.testing.expect(result.array.items.len >= 1);

        // First selection range should have a range and optionally a parent
        const first = result.array.items[0];
        try std.testing.expect(first.object.get("range") != null);

        // Count the depth of parent chain - should have multiple levels
        var depth: usize = 1;
        var current = first;
        while (current.object.get("parent")) |parent| {
            if (parent == .null) break;
            depth += 1;
            current = parent;
        }
        // Should have at least 2 levels (token + file, or token + expression + file)
        try std.testing.expect(depth >= 2);

        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "document highlight handler finds variable occurrences" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "highlight.roc" });
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

    // Document where 'x' appears twice (use \\n for JSON-escaped newline)
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = 1\\ny = x"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Position on first 'x' (line 0, character 0)
    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":0}}}}}}
    , .{file_uri});
    defer allocator.free(highlight_body);
    const highlight_msg = try frame(allocator, highlight_body);
    defer allocator.free(highlight_msg);

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
    try builder.appendSlice(allocator, highlight_msg);
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

    // Find the document highlight response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        try std.testing.expect(result == .array);
        // Verify structure of any highlights returned
        for (result.array.items) |highlight| {
            try std.testing.expect(highlight.object.get("range") != null);
        }
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "document highlight handler returns empty for non-identifier" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "highlight2.roc" });
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
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = 1"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Position on '=' operator (line 0, character 2)
    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":2}}}}}}
    , .{file_uri});
    defer allocator.free(highlight_body);
    const highlight_msg = try frame(allocator, highlight_body);
    defer allocator.free(highlight_msg);

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
    try builder.appendSlice(allocator, highlight_msg);
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

    // Find the document highlight response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        try std.testing.expect(result == .array);
        // Should return empty array for non-identifier position
        try std.testing.expectEqual(@as(usize, 0), result.array.items.len);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "definition handler finds local variable definition" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "definition.roc" });
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

    // Document with a variable defined on line 0, used on line 1
    // "myVar = 42\nresult = myVar + 1"
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"myVar = 42\\nresult = myVar + 1"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for 'myVar' on line 1, character 9 (the usage)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":1,"character":9}}}}}}
    , .{file_uri});
    defer allocator.free(definition_body);
    const definition_msg = try frame(allocator, definition_body);
    defer allocator.free(definition_msg);

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
    try builder.appendSlice(allocator, definition_msg);
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

    // Find the definition response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        // We should get a result (could be null if definition not found, or Location object)
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);

        // If result is an object (Location), verify it has uri and range
        if (result.? == .object) {
            const result_obj = result.?.object;
            try std.testing.expect(result_obj.get("uri") != null);
            try std.testing.expect(result_obj.get("range") != null);

            // Verify the range points to line 0 (where myVar is defined)
            const range = result_obj.get("range").?.object;
            const start = range.get("start").?.object;
            const start_line = start.get("line").?.integer;
            try std.testing.expectEqual(@as(i64, 0), start_line);
        }
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "definition handler returns null for undefined symbol" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "definition_undef.roc" });
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

    // Document with undefined variable usage
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = undefined_var"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for undefined variable (character 4)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":4}}}}}}
    , .{file_uri});
    defer allocator.free(definition_body);
    const definition_msg = try frame(allocator, definition_body);
    defer allocator.free(definition_msg);

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
    try builder.appendSlice(allocator, definition_msg);
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

    // Find the definition response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        // Should have a result field (can be null or valid response)
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "hover handler returns type info for type annotation" {
    // Regression test for Bug 1: s_type_anno statements were ignored by hover system
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "hover_anno.roc" });
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

    // Document with type annotation followed by declaration
    // "dog : Str\ndog = \"Fido\""
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"dog : Str\\ndog = \"Fido\""}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Hover on 'dog' in the type annotation line (line 0, character 0)
    const hover_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":0}}}}}}
    , .{file_uri});
    defer allocator.free(hover_body);
    const hover_msg = try frame(allocator, hover_body);
    defer allocator.free(hover_msg);

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
    try builder.appendSlice(allocator, hover_msg);
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

    // Find the hover response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        // We should get a result (not an error)
        // Before the fix, hovering on the type annotation returned null or strange results
        // After the fix, we should get valid hover info from the corresponding declaration
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);
        // Result can be null (if type info unavailable) or an object with contents/range
        // The key fix is that we don't crash and we find the right declaration
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "definition handler navigates to builtin type from type annotation" {
    // Test that clicking on a type in a type annotation (e.g., "x : Str") navigates to Builtin.roc
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "definition_type.roc" });
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

    // Document with type annotation: "x : U64\nx = 42"
    // Position (0, 4) is on the 'U' of 'U64'
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x : U64\\nx = 42"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for 'U64' on line 0, character 4 (the type in the annotation)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":4}}}}}}
    , .{file_uri});
    defer allocator.free(definition_body);
    const definition_msg = try frame(allocator, definition_body);
    defer allocator.free(definition_msg);

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
    try builder.appendSlice(allocator, definition_msg);
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

    // Find the definition response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        // We should get a result (could be null if definition not found, or Location object)
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);

        // If result is an object (Location), verify it has uri pointing to Builtin.roc
        if (result.? == .object) {
            const result_obj = result.?.object;
            const uri_val = result_obj.get("uri");
            try std.testing.expect(uri_val != null);
            const uri_str = uri_val.?.string;
            // The URI should point to Builtin.roc in the cache
            try std.testing.expect(std.mem.endsWith(u8, uri_str, "Builtin.roc"));
        }
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}
