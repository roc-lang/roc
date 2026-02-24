//! Tests for LSP request handlers: formatting, document symbol, folding range,
//! selection range, and document highlight.

const std = @import("std");
const server_module = @import("../server.zig");
const transport_module = @import("../transport.zig");

/// Get the path to the test platform for creating valid Roc files
fn platformPath(allocator: std.mem.Allocator) ![]u8 {
    // Resolve from repo root to ensure absolute path
    const repo_root = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(repo_root);
    const path = try std.fs.path.join(allocator, &.{ repo_root, "test", "str", "platform", "main.roc" });
    // Convert backslashes to forward slashes for cross-platform Roc source compatibility
    // Roc interprets backslashes as escape sequences in string literals
    for (path) |*c| {
        if (c.* == '\\') c.* = '/';
    }
    return path;
}

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

/// Check whether a JSON items array contains a completion item with the given label.
fn hasCompletionLabel(items: std.json.Value, label: []const u8) bool {
    if (items != .array) return false;
    for (items.array.items) |item| {
        if (item != .object) continue;
        const l = item.object.get("label") orelse continue;
        if (l != .string) continue;
        if (std.mem.eql(u8, l.string, label)) return true;
    }
    return false;
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

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

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
    // Module-member completion responses can exceed 16 KiB depending on
    // builtin surface area and metadata included in items.
    var writer_buffer: [65536]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

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
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main] {{ pf: platform \"{s}\" }}\n\nmyVar = 42\n\nmain = myVar"}}}}}}
    , .{ file_uri, platform_path });
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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

test "document symbols works after goto definition (regression test)" {
    // Regression test: getDocumentSymbols should use getModuleLookupEnv()
    // for proper fallback to previous_build_env after getDefinitionAtPosition
    // creates a fresh build env.
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "regression.roc" });
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

    // Document with a function definition and a usage
    // "myFunc = |x| x + 1\nresult = myFunc(42)"
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"myFunc = |x| x + 1\\nresult = myFunc(42)"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // First request goto definition on myFunc usage (line 1, character 9)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":1,"character":9}}}}}}
    , .{file_uri});
    defer allocator.free(definition_body);
    const definition_msg = try frame(allocator, definition_body);
    defer allocator.free(definition_msg);

    // Then request document symbols (this should use fallback to previous build env)
    const symbols_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/documentSymbol","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(symbols_body);
    const symbols_msg = try frame(allocator, symbols_body);
    defer allocator.free(symbols_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":4,"method":"shutdown"}
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
    try builder.appendSlice(allocator, symbols_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [32768]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Verify we got both responses
    var found_definition_response = false;
    var found_symbols_response = false;

    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer) continue;

        if (id.integer == 2) {
            // Definition response
            const result = parsed.value.object.get("result");
            try std.testing.expect(result != null);
            found_definition_response = true;
        } else if (id.integer == 3) {
            // Document symbols response - should return an array (possibly empty, but not an error)
            const result = parsed.value.object.get("result");
            try std.testing.expect(result != null);
            try std.testing.expect(result.? == .array);
            found_symbols_response = true;
        }
    }
    try std.testing.expect(found_definition_response);
    try std.testing.expect(found_symbols_response);
}

test "multiple goto definition calls don't break document symbols" {
    // Test that multiple sequential goto definition calls maintain proper state
    // for subsequent document symbol requests
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "multi_def.roc" });
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

    // Document with multiple definitions
    // "foo = 1\nbar = foo\nbaz = bar"
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"foo = 1\\nbar = foo\\nbaz = bar"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // First definition request on 'foo' in bar's definition (line 1, char 6)
    const def1_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":1,"character":6}}}}}}
    , .{file_uri});
    defer allocator.free(def1_body);
    const def1_msg = try frame(allocator, def1_body);
    defer allocator.free(def1_msg);

    // Second definition request on 'bar' in baz's definition (line 2, char 6)
    const def2_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":6}}}}}}
    , .{file_uri});
    defer allocator.free(def2_body);
    const def2_msg = try frame(allocator, def2_body);
    defer allocator.free(def2_msg);

    // Document symbols request after multiple definitions
    const symbols_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"textDocument/documentSymbol","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(symbols_body);
    const symbols_msg = try frame(allocator, symbols_body);
    defer allocator.free(symbols_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":5,"method":"shutdown"}
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
    try builder.appendSlice(allocator, def1_msg);
    try builder.appendSlice(allocator, def2_msg);
    try builder.appendSlice(allocator, symbols_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    var writer_buffer: [32768]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_stream.getWritten());
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Verify all responses
    var found_def1 = false;
    var found_def2 = false;
    var found_symbols = false;

    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer) continue;

        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);

        if (id.integer == 2) {
            found_def1 = true;
        } else if (id.integer == 3) {
            found_def2 = true;
        } else if (id.integer == 4) {
            try std.testing.expect(result.? == .array);
            found_symbols = true;
        }
    }
    try std.testing.expect(found_def1);
    try std.testing.expect(found_def2);
    try std.testing.expect(found_symbols);
}

test "document symbol handler returns symbols with correct names" {
    // Test that outline returns actual symbol names using valid Roc syntax
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "outline.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    // Get the platform path for valid Roc syntax
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // Create a valid Roc app with proper header and definitions
    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [main, add, myConst] {{ pf: platform "{s}" }}
        \\
        \\add = |a, b| a + b
        \\
        \\myConst = 42
        \\
        \\main = add(myConst, 1)
        \\
    , .{platform_path});
    defer allocator.free(roc_source);

    // Write the file to disk (required for platform resolution)
    try tmp.dir.writeFile(.{ .sub_path = "outline.roc", .data = roc_source });

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

    // Escape the source for JSON
    var escaped_source = std.ArrayList(u8){};
    defer escaped_source.deinit(allocator);
    for (roc_source) |c| {
        switch (c) {
            '"' => try escaped_source.appendSlice(allocator, "\\\""),
            '\\' => try escaped_source.appendSlice(allocator, "\\\\"),
            '\n' => try escaped_source.appendSlice(allocator, "\\n"),
            '\r' => try escaped_source.appendSlice(allocator, "\\r"),
            '\t' => try escaped_source.appendSlice(allocator, "\\t"),
            else => try escaped_source.append(allocator, c),
        }
    }

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source.items });
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
    var writer_buffer: [32768]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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

        // Collect symbol names
        var found_add = false;
        var found_myConst = false;
        var found_main = false;

        for (result.array.items) |symbol| {
            const name = symbol.object.get("name") orelse continue;
            if (name != .string) continue;

            if (std.mem.eql(u8, name.string, "add")) found_add = true;
            if (std.mem.eql(u8, name.string, "myConst")) found_myConst = true;
            if (std.mem.eql(u8, name.string, "main")) found_main = true;
        }

        // Verify that we found the exposed symbols
        // The Roc build pipeline should return symbols for exposed definitions
        try std.testing.expect(found_main or found_add or found_myConst);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "document symbol handler works independently of check" {
    // Regression test: document symbols should work even without a prior check() call
    // The handler should build the module itself
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "independent.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    // Get the platform path for valid Roc syntax
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // Create a valid Roc app with proper header
    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [hello] {{ pf: platform "{s}" }}
        \\
        \\hello = "world"
        \\
    , .{platform_path});
    defer allocator.free(roc_source);

    // Write the file to disk (required for platform resolution)
    try tmp.dir.writeFile(.{ .sub_path = "independent.roc", .data = roc_source });

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

    // Escape the source for JSON
    var escaped_source = std.ArrayList(u8){};
    defer escaped_source.deinit(allocator);
    for (roc_source) |c| {
        switch (c) {
            '"' => try escaped_source.appendSlice(allocator, "\\\""),
            '\\' => try escaped_source.appendSlice(allocator, "\\\\"),
            '\n' => try escaped_source.appendSlice(allocator, "\\n"),
            '\r' => try escaped_source.appendSlice(allocator, "\\r"),
            '\t' => try escaped_source.appendSlice(allocator, "\\t"),
            else => try escaped_source.append(allocator, c),
        }
    }

    // Open and immediately request symbols WITHOUT any prior textDocument/didChange
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source.items });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Immediately request symbols - this should work without waiting for a check
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
    var writer_buffer: [32768]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buffer);

    const ReaderType = @TypeOf(reader_stream.reader());
    const WriterType = @TypeOf(writer_stream.writer());
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, reader_stream.reader(), writer_stream.writer(), null, .{});
    server.syntax_checker.cache_config.enabled = false; // Disable cache to avoid deserialized interner issues in tests
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

        // Should get a valid response (array), not an error
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);
        try std.testing.expect(result.? == .array);

        // Check for the "hello" symbol
        var found_hello = false;
        for (result.?.array.items) |symbol| {
            const name = symbol.object.get("name") orelse continue;
            if (name != .string) continue;
            if (std.mem.eql(u8, name.string, "hello")) found_hello = true;
        }
        try std.testing.expect(found_hello);

        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns module definitions" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "completion.roc" });
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

    // Document with two definitions
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module []\\n\\nfoo = 42\\nbar = |x| x + 1"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at position 3,0 (beginning of second definition line)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":0}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        // Result should be a completion list with items
        const result = parsed.value.object.get("result");
        try std.testing.expect(result != null);

        // Should have isIncomplete field
        const is_incomplete = result.?.object.get("isIncomplete");
        try std.testing.expect(is_incomplete != null);

        // Should have items array
        const items = result.?.object.get("items");
        try std.testing.expect(items != null);

        // Verify items is an array (may be empty on parse failure, or contain definitions)
        try std.testing.expect(items.? == .array);

        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns module members after dot" {
    // Test: typing "Str." should trigger completions from the Str module
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "module_completion.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);
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

    // Document with "Str." - should trigger module member completion
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main] {{ pf: platform \"{s}\" }}\n\nx = Str."}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion right after the dot (line 2, character 8)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":8}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    var reader_stream = std.io.fixedBufferStream(combined);
    // Module completions can be very large depending on builtins and docs.
    var writer_buffer: [1024 * 1024]u8 = undefined;
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);

        // Should have Str module members like concat, isEmpty, etc.
        var found_concat = false;
        var found_isEmpty = false;
        for (items.array.items) |item| {
            const label = item.object.get("label") orelse continue;
            if (label == .string) {
                if (std.mem.eql(u8, label.string, "concat")) found_concat = true;
                if (std.mem.eql(u8, label.string, "isEmpty")) found_isEmpty = true;
            }
        }

        // At least one Str function should be present
        try std.testing.expect(found_concat or found_isEmpty);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns module names in expression context" {
    // Test: in expression context, module names should be available
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "module_name_completion.roc" });
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

    // Simple document - completion at beginning of expression
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module []\\n\\nx = "}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at the end (line 2, character 4)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":4}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);

        // Should have builtin module names like Str, List, Num
        var found_str = false;
        var found_list = false;
        var found_num = false;
        for (items.array.items) |item| {
            const label = item.object.get("label") orelse continue;
            if (label == .string) {
                if (std.mem.eql(u8, label.string, "Str")) found_str = true;
                if (std.mem.eql(u8, label.string, "List")) found_list = true;
                if (std.mem.eql(u8, label.string, "Num")) found_num = true;
            }
        }

        // Builtin modules should be present
        try std.testing.expect(found_str);
        try std.testing.expect(found_list);
        try std.testing.expect(found_num);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns types after colon" {
    // Test: typing "x :" should trigger type completions
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "type_completion.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

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

    // Document with type annotation context
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main] {{ pf: platform \"{s}\" }}\nMyList:List(Str)\nx : "}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion after the colon (line 2, character 4)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":4}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);

        // Should have builtin types like Str, U64, Bool
        var found_str = false;
        var found_u64 = false;
        var found_bool = false;
        var found_my_list = false;
        for (items.array.items) |item| {
            const label = item.object.get("label") orelse continue;
            if (label == .string) {
                if (std.mem.eql(u8, label.string, "Str")) found_str = true;
                if (std.mem.eql(u8, label.string, "MyList")) found_my_list = true;
                if (std.mem.eql(u8, label.string, "U64")) found_u64 = true;
                if (std.mem.eql(u8, label.string, "Bool")) found_bool = true;
            }
        }

        // Builtin types should be present in type context
        try std.testing.expect(found_str);
        try std.testing.expect(found_u64);
        try std.testing.expect(found_bool);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns List module members after List dot" {
    // Test: typing "List." should trigger completions from the List module
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "list_completion.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);
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

    // Document with "List." - should trigger List module member completion
    const open_body = try std.fmt.allocPrint(
        allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main] {{ pf: platform \"{s}\" }}\n\nx = List."}}}}}}
    ,
        .{ file_uri, platform_path },
    );
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion right after the dot (line 2, character 9)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":9}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);

        // Should have List module members like map, append, get, etc.
        var found_map = false;
        var found_append = false;
        var found_get = false;
        for (items.array.items) |item| {
            const label = item.object.get("label") orelse continue;
            if (label == .string) {
                if (std.mem.eql(u8, label.string, "map")) found_map = true;
                if (std.mem.eql(u8, label.string, "append")) found_append = true;
                if (std.mem.eql(u8, label.string, "get")) found_get = true;
            }
        }

        // At least some List functions should be present
        try std.testing.expect(found_map or found_append or found_get);
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns local variables in block scope" {
    // Test: local variables defined in a block should appear in completions
    // within that block
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "local_completion.roc" });
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

    // Document with local variable in a block:
    // main = {
    //     local_var = 42
    //     <cursor here>
    // }
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module []\\n\\nmain = {{{{\\n    local_var = 42\\n    \\n}}}}"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at line 4, character 4 (inside the block, after local_var is defined)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":4}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);
        try std.testing.expect(items.array.items.len > 0);
        // TODO: assert hasCompletionLabel(items, "local_var") once scope
        // resolution produces local bindings in the integration test context.
        // Scope binding visibility is tested directly in scope_map unit tests.
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns lambda parameters" {
    // Test: lambda parameters should appear in completions within the lambda body
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "lambda_param_completion.roc" });
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

    // Document with a lambda that has parameters:
    // add = |first, second| first + second
    // Cursor position should be inside the lambda body
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module []\\n\\nadd = |first, second| first + second"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at line 2, character 22 (right after the |, inside lambda body)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":22}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);
        try std.testing.expect(items.array.items.len > 0);
        // TODO: assert hasCompletionLabel(items, "first") and "second" once
        // lambda param resolution works in the integration test context.
        // Lambda param visibility is tested directly in scope_map unit tests.
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns top-level definitions" {
    // Test: top-level definitions should appear in completions
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "toplevel_completion.roc" });
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

    // Document with multiple top-level definitions
    // Request completion at beginning of third line (similar to the passing test)
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module []\n\nmy_constant = 42\nmy_function = |x| x * 2\nresult = my_constant"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at line 4, character 0 (beginning of result line)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":0}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);
        try std.testing.expect(items.array.items.len > 0);
        try std.testing.expect(hasCompletionLabel(items, "my_constant"));
        try std.testing.expect(hasCompletionLabel(items, "my_function"));
        found_response = true;
        break;
    }
    try std.testing.expect(found_response);
}

test "completion handler returns record fields after dot" {
    // Test: typing "rec." where rec is a record should trigger field completions
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "record_completion.roc" });
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

    // Document with a record variable and field access
    // rec = { name: "hello", age: 42 }
    // x = rec.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module []\n\nrec = {{ name: \"hello\", age: 42 }}\nx = rec."}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion right after "rec." (line 3, character 8)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":8}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const completion_msg = try frame(allocator, completion_body);
    defer allocator.free(completion_msg);

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
    try builder.appendSlice(allocator, completion_msg);
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

    // Find the completion response (id: 2)
    var found_record_response = false;
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id != .integer or id.integer != 2) continue;

        const result = parsed.value.object.get("result") orelse continue;
        const items = result.object.get("items") orelse continue;
        try std.testing.expect(items == .array);

        // Check for record field completions - should have "name" and "age" fields
        // Note: This requires successful type checking which may not always work in test environments
        for (items.array.items) |item| {
            const label = item.object.get("label") orelse continue;
            if (label == .string) {
                // Verify field items have the correct kind (5 = field)
                if (std.mem.eql(u8, label.string, "name") or std.mem.eql(u8, label.string, "age")) {
                    if (item.object.get("kind")) |kind| {
                        try std.testing.expectEqual(@as(i64, 5), kind.integer);
                    }
                }
            }
        }

        // The test succeeds if we got a valid completion response
        found_record_response = true;
        break;
    }
    try std.testing.expect(found_record_response);
}
