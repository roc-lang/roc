//! Unit tests for LSP request handlers.
//!
//! These tests may exercise parsing, formatting, token extraction, and JSON
//! routing, but they must not use real compiler checking. They run through the
//! server with TestSyntaxDriver, which never creates SyntaxChecker or BuildEnv.

const std = @import("std");
const server_module = @import("lsp").server;
const helpers = @import("helpers.zig");
const TestSyntaxDriver = @import("test_syntax_driver.zig").TestSyntaxDriver;

const collectResponses = helpers.collectResponses;
const uriFromPath = helpers.uriFromPath;

const UnitTestError = std.mem.Allocator.Error ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Reader.Error ||
    std.json.ParseError(std.json.Scanner) ||
    helpers.HelperError ||
    error{
        HeaderTooLong,
        InvalidHeader,
        MissingContentLength,
        MissingResponse,
        PayloadTooLarge,
    };

fn TestServer(comptime ReaderType: type, comptime WriterType: type) type {
    return server_module.ServerWithSyntaxDriver(ReaderType, WriterType, TestSyntaxDriver);
}

const RunResult = struct {
    responses: [][]u8,
    check_calls: usize,
    highlight_calls: usize,
    document_symbol_calls: usize,
};

fn tempFileUri(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, filename: []const u8) UnitTestError![]u8 {
    const tmp_path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, filename });
    defer allocator.free(file_path);
    return try uriFromPath(allocator, file_path);
}

fn requestInput(
    allocator: std.mem.Allocator,
    file_uri: []const u8,
    open_text_json: []const u8,
    request_body: []const u8,
) std.mem.Allocator.Error![]u8 {
    const init_body =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    ;
    const initialized_body =
        \\{"jsonrpc":"2.0","method":"initialized","params":{}}
    ;
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, open_text_json });
    defer allocator.free(open_body);
    const shutdown_body =
        \\{"jsonrpc":"2.0","id":3,"method":"shutdown"}
    ;
    const exit_body =
        \\{"jsonrpc":"2.0","method":"exit"}
    ;

    return try helpers.framedInput(allocator, &.{
        init_body,
        initialized_body,
        open_body,
        request_body,
        shutdown_body,
        exit_body,
    });
}

fn runUnitServer(allocator: std.mem.Allocator, input: []const u8, writer_buffer: []u8) UnitTestError!RunResult {
    const reader_stream: std.Io.Reader = .fixed(input);
    const writer_stream: std.Io.Writer = .fixed(writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try TestServer(ReaderType, WriterType).init(allocator, std.testing.io, reader_stream, writer_stream, null, .{});
    defer server.deinit();

    try server.run();
    return .{
        .responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]),
        .check_calls = server.syntax_checker.check_calls,
        .highlight_calls = server.syntax_checker.highlight_calls,
        .document_symbol_calls = server.syntax_checker.document_symbol_calls,
    };
}

fn freeRun(allocator: std.mem.Allocator, result: RunResult) void {
    helpers.freeResponses(allocator, result.responses);
}

fn responseWithId(allocator: std.mem.Allocator, responses: [][]u8, wanted_id: i64) UnitTestError![]const u8 {
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        defer parsed.deinit();
        const id = parsed.value.object.get("id") orelse continue;
        if (id == .integer and id.integer == wanted_id) {
            return response;
        }
    }
    return error.MissingResponse;
}

test "didChange handler handles out-of-bounds float values without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "change_float.roc");
    defer allocator.free(file_uri);

    const change_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":1e100}},"contentChanges":[{{"text":"x","range":{{"start":{{"line":1e100,"character":0}},"end":{{"line":1,"character":0}}}}}}]}}}}
    , .{file_uri});
    defer allocator.free(change_body);
    const input = try requestInput(allocator, file_uri, "x = 1", change_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
}

test "didChange handler handles valid float range indices correctly" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "change_valid_float.roc");
    defer allocator.free(file_uri);

    const change_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":2.0}},"contentChanges":[{{"text":"x","range":{{"start":{{"line":0.0,"character":0.0}},"end":{{"line":0.0,"character":1.0}}}}}}]}}}}
    , .{file_uri});
    defer allocator.free(change_body);
    const input = try requestInput(allocator, file_uri, "x = 1", change_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
}

test "didOpen handler handles out-of-bounds float version without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "open_float.roc");
    defer allocator.free(file_uri);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1e100,"text":"x = 1"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const input = try requestInput(allocator, file_uri, "x = 1", open_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
}

test "formatting handler formats simple expression with test syntax driver" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "format.roc");
    defer allocator.free(file_uri);

    const format_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/formatting","params":{{"textDocument":{{"uri":"{s}"}},"options":{{"tabSize":4,"insertSpaces":true}}}}}}
    , .{file_uri});
    defer allocator.free(format_body);
    const input = try requestInput(allocator, file_uri, "x=1", format_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
    try std.testing.expectEqual(@as(usize, 1), run.check_calls);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    if (result == .array) {
        for (result.array.items) |edit| {
            try std.testing.expect(edit.object.get("range") != null);
            try std.testing.expect(edit.object.get("newText") != null);
        }
    } else {
        try std.testing.expect(result == .null);
    }
}

test "document symbol handler returns empty with test syntax driver" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "symbols.roc");
    defer allocator.free(file_uri);

    const symbols_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentSymbol","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(symbols_body);
    const input = try requestInput(allocator, file_uri, "main = 1", symbols_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
    try std.testing.expectEqual(@as(usize, 1), run.check_calls);
    try std.testing.expectEqual(@as(usize, 1), run.document_symbol_calls);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    try std.testing.expect(result == .array);
    try std.testing.expectEqual(@as(usize, 0), result.array.items.len);
}

test "folding range handler finds bracket ranges with test syntax driver" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "fold.roc");
    defer allocator.free(file_uri);

    const folding_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/foldingRange","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(folding_body);
    const input = try requestInput(allocator, file_uri, "myRecord = {\\n    x: 1,\\n    y: 2,\\n}", folding_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
    try std.testing.expectEqual(@as(usize, 1), run.check_calls);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    try std.testing.expect(result == .array);
    for (result.array.items) |range| {
        try std.testing.expect(range.object.get("startLine") != null);
        try std.testing.expect(range.object.get("endLine") != null);
    }
}

test "folding range handler reports true line numbers past 4096 lines" {
    // The handlers used to build their own line tables into a fixed
    // `[4096]u32`, silently stopping there. Every position past line 4096 in a
    // longer file was then reported as line 4095.
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "long.roc");
    defer allocator.free(file_uri);

    const blank_lines = 5000;

    // A foldable record well past the old cap. The document text is embedded in
    // a JSON string, so newlines are escaped.
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(allocator);
    try source.appendSlice(allocator, "x = 1");
    for (0..blank_lines) |_| {
        try source.appendSlice(allocator, "\\n");
    }
    try source.appendSlice(allocator, "y = {\\n    a: 1,\\n}");

    const folding_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/foldingRange","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{file_uri});
    defer allocator.free(folding_body);
    const input = try requestInput(allocator, file_uri, source.items, folding_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    try std.testing.expect(result == .array);
    try std.testing.expect(result.array.items.len >= 1);

    // `y = {` sits on the line after the blanks, and its `}` two lines later.
    const range = result.array.items[0];
    const start_line = range.object.get("startLine") orelse return error.MissingResult;
    const end_line = range.object.get("endLine") orelse return error.MissingResult;
    try std.testing.expect(start_line == .integer);
    try std.testing.expect(end_line == .integer);
    try std.testing.expectEqual(@as(i64, blank_lines), start_line.integer);
    try std.testing.expectEqual(@as(i64, blank_lines + 2), end_line.integer);
}

test "selection range handler returns range hierarchy with test syntax driver" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "select.roc");
    defer allocator.free(file_uri);

    const select_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/selectionRange","params":{{"textDocument":{{"uri":"{s}"}},"positions":[{{"line":0,"character":4}}]}}}}
    , .{file_uri});
    defer allocator.free(select_body);
    const input = try requestInput(allocator, file_uri, "x = foo + bar", select_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
    try std.testing.expectEqual(@as(usize, 1), run.check_calls);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    try std.testing.expect(result == .array);
    try std.testing.expect(result.array.items.len >= 1);
    try std.testing.expect(result.array.items[0].object.get("range") != null);
}

test "document highlight handler finds token occurrences with test syntax driver" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "highlight.roc");
    defer allocator.free(file_uri);

    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":0}}}}}}
    , .{file_uri});
    defer allocator.free(highlight_body);
    const input = try requestInput(allocator, file_uri, "x = 1\\ny = x", highlight_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
    try std.testing.expectEqual(@as(usize, 1), run.check_calls);
    try std.testing.expectEqual(@as(usize, 1), run.highlight_calls);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    try std.testing.expect(result == .array);
    try std.testing.expect(result.array.items.len >= 2);
    for (result.array.items) |highlight| {
        try std.testing.expect(highlight.object.get("range") != null);
    }
}

test "document highlight handler returns empty for non-identifier with test syntax driver" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "highlight2.roc");
    defer allocator.free(file_uri);

    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":2}}}}}}
    , .{file_uri});
    defer allocator.free(highlight_body);
    const input = try requestInput(allocator, file_uri, "x = 1", highlight_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);
    try std.testing.expectEqual(@as(usize, 1), run.check_calls);
    try std.testing.expectEqual(@as(usize, 1), run.highlight_calls);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result") orelse return error.MissingResult;
    try std.testing.expect(result == .array);
    try std.testing.expectEqual(@as(usize, 0), result.array.items.len);
}

test "hover handler returns invalid_params error for negative position coordinates without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "hover_negative.roc");
    defer allocator.free(file_uri);

    const hover_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":-1,"character":-1}}}}}}
    , .{file_uri});
    defer allocator.free(hover_body);
    const input = try requestInput(allocator, file_uri, "x = 1", hover_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error") orelse return error.MissingError;
    try std.testing.expect(err_obj == .object);
    const code = err_obj.object.get("code") orelse return error.MissingCode;
    try std.testing.expectEqual(@as(i64, -32602), code.integer);
}

test "completion handler returns invalid_params error for negative position coordinates without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "completion_negative.roc");
    defer allocator.free(file_uri);

    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":-1,"character":-1}}}}}}
    , .{file_uri});
    defer allocator.free(completion_body);
    const input = try requestInput(allocator, file_uri, "x = 1", completion_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error") orelse return error.MissingError;
    try std.testing.expect(err_obj == .object);
    const code = err_obj.object.get("code") orelse return error.MissingCode;
    try std.testing.expectEqual(@as(i64, -32602), code.integer);
}

test "definition handler returns invalid_params error for negative position coordinates without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "def_negative.roc");
    defer allocator.free(file_uri);

    const def_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":-1,"character":-1}}}}}}
    , .{file_uri});
    defer allocator.free(def_body);
    const input = try requestInput(allocator, file_uri, "x = 1", def_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error") orelse return error.MissingError;
    try std.testing.expect(err_obj == .object);
    const code = err_obj.object.get("code") orelse return error.MissingCode;
    try std.testing.expectEqual(@as(i64, -32602), code.integer);
}

test "document highlight handler returns invalid_params error for negative position coordinates without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "highlight_negative.roc");
    defer allocator.free(file_uri);

    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":-1,"character":-1}}}}}}
    , .{file_uri});
    defer allocator.free(highlight_body);
    const input = try requestInput(allocator, file_uri, "x = 1", highlight_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error") orelse return error.MissingError;
    try std.testing.expect(err_obj == .object);
    const code = err_obj.object.get("code") orelse return error.MissingCode;
    try std.testing.expectEqual(@as(i64, -32602), code.integer);
}

test "selection range handler returns invalid_params error for negative position coordinates without panicking" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file_uri = try tempFileUri(allocator, &tmp, "select_negative.roc");
    defer allocator.free(file_uri);

    const select_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/selectionRange","params":{{"textDocument":{{"uri":"{s}"}},"positions":[{{"line":-1,"character":-1}}]}}}}
    , .{file_uri});
    defer allocator.free(select_body);
    const input = try requestInput(allocator, file_uri, "x = 1", select_body);
    defer allocator.free(input);

    var writer_buffer: [16384]u8 = undefined;
    const run = try runUnitServer(allocator, input, &writer_buffer);
    defer freeRun(allocator, run);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, try responseWithId(allocator, run.responses, 2), .{});
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error") orelse return error.MissingError;
    try std.testing.expect(err_obj == .object);
    const code = err_obj.object.get("code") orelse return error.MissingCode;
    try std.testing.expectEqual(@as(i64, -32602), code.integer);
}
