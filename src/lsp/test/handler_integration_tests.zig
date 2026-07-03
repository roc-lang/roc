//! Integration tests for LSP request handlers.
//!
//! These tests may use real SyntaxChecker and BuildEnv because they assert
//! compiler-backed completion, hover, definition, document symbol,
//! document highlight, and diagnostic behavior.

const std = @import("std");
const server_module = @import("lsp").server;
const helpers = @import("helpers.zig");
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

const frame = helpers.frame;
const uriFromPath = helpers.uriFromPath;

fn collectResponses(allocator: std.mem.Allocator, bytes: []const u8) integration_spec.SpecError![][]u8 {
    return helpers.collectResponsesWithIo(allocator, test_env.io, bytes);
}

fn jsonEscape(allocator: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error![]u8 {
    var escaped: std.ArrayList(u8) = .empty;
    errdefer escaped.deinit(allocator);
    for (source) |c| {
        switch (c) {
            '"' => try escaped.appendSlice(allocator, "\\\""),
            '\\' => try escaped.appendSlice(allocator, "\\\\"),
            '\n' => try escaped.appendSlice(allocator, "\\n"),
            '\r' => try escaped.appendSlice(allocator, "\\r"),
            '\t' => try escaped.appendSlice(allocator, "\\t"),
            else => try escaped.append(allocator, c),
        }
    }
    return escaped.toOwnedSlice(allocator);
}

/// Get the path to the test platform for creating valid Roc files
fn platformPath(allocator: std.mem.Allocator) integration_spec.SpecError![]u8 {
    // Resolve from repo root to ensure absolute path
    const repo_root = try std.Io.Dir.cwd().realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(repo_root);
    const path = try std.fs.path.join(allocator, &.{ repo_root, "test", "str", "platform", "main.roc" });
    // Convert backslashes to forward slashes for cross-platform Roc source compatibility
    // Roc interprets backslashes as escape sequences in string literals
    for (path) |*c| {
        if (c.* == '\\') c.* = '/';
    }
    return path;
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

const ParsedResponse = struct {
    parsed: std.json.Parsed(std.json.Value),

    fn deinit(self: *ParsedResponse) void {
        self.parsed.deinit();
    }

    fn result(self: *const ParsedResponse) integration_spec.SpecError!std.json.Value {
        const root = self.parsed.value;
        if (root != .object) return error.TestUnexpectedResult;
        if (root.object.get("error") != null) return error.TestUnexpectedResult;
        return root.object.get("result") orelse error.TestUnexpectedResult;
    }
};

fn responseById(allocator: std.mem.Allocator, responses: [][]u8, expected_id: i64) integration_spec.SpecError!ParsedResponse {
    for (responses) |response| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response, .{});
        errdefer parsed.deinit();

        if (parsed.value != .object) {
            parsed.deinit();
            continue;
        }
        const id = parsed.value.object.get("id") orelse {
            parsed.deinit();
            continue;
        };
        if (id != .integer or id.integer != expected_id) {
            parsed.deinit();
            continue;
        }

        return .{ .parsed = parsed };
    }
    return error.TestUnexpectedResult;
}

fn objectField(value: std.json.Value, name: []const u8) integration_spec.SpecError!std.json.Value {
    if (value != .object) return error.TestUnexpectedResult;
    return value.object.get(name) orelse error.TestUnexpectedResult;
}

fn integerField(value: std.json.Value, name: []const u8) integration_spec.SpecError!i64 {
    const field_value = try objectField(value, name);
    if (field_value != .integer) return error.TestUnexpectedResult;
    return field_value.integer;
}

fn stringField(value: std.json.Value, name: []const u8) integration_spec.SpecError![]const u8 {
    const field_value = try objectField(value, name);
    if (field_value != .string) return error.TestUnexpectedResult;
    return field_value.string;
}

fn expectRange(
    range: std.json.Value,
    start_line: i64,
    start_character: i64,
    end_line: i64,
    end_character: i64,
) integration_spec.SpecError!void {
    const start = try objectField(range, "start");
    const end = try objectField(range, "end");
    try std.testing.expectEqual(start_line, try integerField(start, "line"));
    try std.testing.expectEqual(start_character, try integerField(start, "character"));
    try std.testing.expectEqual(end_line, try integerField(end, "line"));
    try std.testing.expectEqual(end_character, try integerField(end, "character"));
}

fn expectLocation(
    result: std.json.Value,
    expected_uri: []const u8,
    start_line: i64,
    start_character: i64,
    end_line: i64,
    end_character: i64,
) integration_spec.SpecError!void {
    try std.testing.expect(result == .object);
    try std.testing.expectEqualStrings(expected_uri, try stringField(result, "uri"));
    try expectRange(try objectField(result, "range"), start_line, start_character, end_line, end_character);
}

fn expectNullOrLocation(
    result: std.json.Value,
    expected_uri: []const u8,
    start_line: i64,
    start_character: i64,
    end_line: i64,
    end_character: i64,
) integration_spec.SpecError!void {
    if (result == .null) return;
    try expectLocation(result, expected_uri, start_line, start_character, end_line, end_character);
}

fn hasHighlightRange(
    highlights: std.json.Value,
    start_line: i64,
    start_character: i64,
    end_line: i64,
    end_character: i64,
) integration_spec.SpecError!bool {
    if (highlights != .array) return error.TestUnexpectedResult;
    for (highlights.array.items) |highlight| {
        const range = try objectField(highlight, "range");
        const start = try objectField(range, "start");
        const end = try objectField(range, "end");
        if ((try integerField(start, "line")) == start_line and
            (try integerField(start, "character")) == start_character and
            (try integerField(end, "line")) == end_line and
            (try integerField(end, "character")) == end_character)
        {
            return true;
        }
    }
    return false;
}

fn expectSymbolNames(result: std.json.Value, expected_names: []const []const u8) integration_spec.SpecError!void {
    try std.testing.expect(result == .array);
    try std.testing.expect(result.array.items.len >= expected_names.len);

    for (expected_names) |expected_name| {
        var found = false;
        for (result.array.items) |symbol| {
            const name = try stringField(symbol, "name");
            if (std.mem.eql(u8, name, expected_name)) {
                found = true;
                break;
            }
        }
        try std.testing.expect(found);
    }
}

fn completionItems(result: std.json.Value) integration_spec.SpecError!std.json.Value {
    try std.testing.expect(result == .object);
    const is_incomplete = try objectField(result, "isIncomplete");
    try std.testing.expect(is_incomplete == .bool);
    const items = try objectField(result, "items");
    try std.testing.expect(items == .array);
    return items;
}

fn expectCompletionLabels(items: std.json.Value, labels: []const []const u8) integration_spec.SpecError!void {
    for (labels) |label| {
        try std.testing.expect(hasCompletionLabel(items, label));
    }
}

fn expectNonEmptyCompletionItems(items: std.json.Value) integration_spec.SpecError!void {
    try std.testing.expect(items == .array);
    try std.testing.expect(items.array.items.len > 0);
}

/// Handler integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "document symbol handler extracts function declarations", .run = documentSymbolHandlerExtractsFunctionDeclarations },
    .{ .name = "document highlight handler finds variable occurrences", .run = documentHighlightHandlerFindsVariableOccurrences },
    .{ .name = "definition handler finds local variable definition", .run = definitionHandlerFindsLocalVariableDefinition },
    .{ .name = "definition handler returns null for undefined symbol", .run = definitionHandlerReturnsNullForUndefinedSymbol },
    .{ .name = "hover handler handles type annotation request", .run = hoverHandlerReturnsTypeInfoForTypeAnnotation },
    .{ .name = "definition handler handles builtin type annotation request", .run = definitionHandlerNavigatesToBuiltinTypeFromTypeAnnotation },
    .{ .name = "document symbols works after goto definition (regression test)", .run = documentSymbolsWorksAfterGotoDefinitionRegressionTest },
    .{ .name = "multiple goto definition calls don't break document symbols", .run = multipleGotoDefinitionCallsDontBreakDocumentSymbols },
    .{ .name = "document symbol handler returns symbols with correct names", .run = documentSymbolHandlerReturnsSymbolsWithCorrectNames },
    .{ .name = "document symbol handler works independently of check", .run = documentSymbolHandlerWorksIndependentlyOfCheck },
    .{ .name = "completion handler returns completion list for module definitions", .run = completionHandlerReturnsModuleDefinitions },
    .{ .name = "completion handler returns module members after dot", .run = completionHandlerReturnsModuleMembersAfterDot },
    .{ .name = "completion handler returns module names in expression context", .run = completionHandlerReturnsModuleNamesInExpressionContext },
    .{ .name = "completion handler returns types after colon", .run = completionHandlerReturnsTypesAfterColon },
    .{ .name = "completion handler returns List module members after List dot", .run = completionHandlerReturnsListModuleMembersAfterListDot },
    .{ .name = "completion handler returns completion list in block scope", .run = completionHandlerReturnsLocalVariablesInBlockScope },
    .{ .name = "completion handler returns completion list in lambda body", .run = completionHandlerReturnsLambdaParameters },
    .{ .name = "completion handler returns top-level definitions", .run = completionHandlerReturnsTopLevelDefinitions },
    .{ .name = "completion handler returns record fields after dot", .run = completionHandlerReturnsRecordFieldsAfterDot },
};

/// Verifies document symbols include top-level declarations from an opened file.
pub fn documentSymbolHandlerExtractsFunctionDeclarations() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "symbols.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\my_var = 42
        \\
        \\main = my_var + 1
    , .{platform_path});
    defer allocator.free(roc_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "symbols.roc", .data = roc_source });

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
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main] {{ pf: platform \"{s}\" }}\n\nmy_var = 42\n\nmain = my_var"}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, symbols_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const result = try response.result();
    try std.testing.expect(result == .array);
    try std.testing.expect(result.array.items.len >= 2);

    var found_my_var = false;
    var found_main = false;
    for (result.array.items) |symbol| {
        try std.testing.expect(symbol == .object);
        const name = symbol.object.get("name") orelse return error.TestUnexpectedResult;
        try std.testing.expect(name == .string);
        try std.testing.expect(symbol.object.get("kind") != null);
        try std.testing.expect(symbol.object.get("location") != null);
        if (std.mem.eql(u8, name.string, "my_var")) found_my_var = true;
        if (std.mem.eql(u8, name.string, "main")) found_main = true;
    }
    try std.testing.expect(found_my_var);
    try std.testing.expect(found_main);
}

/// Verifies document highlights include occurrences of the selected variable.
pub fn documentHighlightHandlerFindsVariableOccurrences() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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

    // Document where 'x' appears twice.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = x"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Position on first 'x' (line 0, character 0).
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, highlight_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const result = try response.result();
    try std.testing.expect(result == .array);
    try std.testing.expect(result.array.items.len > 0);
    try std.testing.expect(try hasHighlightRange(result, 0, 0, 0, 1));
}

/// Verifies goto definition locates a local variable definition.
pub fn definitionHandlerFindsLocalVariableDefinition() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "definition.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);
    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\my_var = 42
        \\
        \\main = my_var + 1
    , .{platform_path});
    defer allocator.free(roc_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "definition.roc", .data = roc_source });
    const escaped_source = try jsonEscape(allocator, roc_source);
    defer allocator.free(escaped_source);

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

    // Document with a variable defined on line 2, used on line 4.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for 'my_var' on line 4, character 8 (inside the usage).
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":8}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, definition_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    try expectLocation(try response.result(), file_uri, 2, 0, 2, 6);
}

/// Verifies goto definition returns null for an unresolved symbol.
pub fn definitionHandlerReturnsNullForUndefinedSymbol() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "definition_undef.roc" });
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

    // Document with a name-not-in-scope usage
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [x] {{ pf: platform \"{s}\" }}\\n\\nx = undefined_var"}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for a name not in scope (line 2, character 4).
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":4}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, definition_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const result = try response.result();
    try std.testing.expect(result == .null);
}

/// Verifies hover on a type annotation returns type information.
pub fn hoverHandlerReturnsTypeInfoForTypeAnnotation() integration_spec.SpecError!void {
    // Regression test for Bug 1: s_type_anno statements were ignored by hover system
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "hover_anno.roc" });
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

    // Document with type annotation followed by declaration.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [dog] {{ pf: platform \"{s}\" }}\\n\\ndog : Str\\ndog = \"Fido\""}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Hover on 'dog' in the type annotation line (line 2, character 0).
    const hover_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":0}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, hover_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const result = try response.result();
    if (result == .object) {
        const contents = try objectField(result, "contents");
        try std.testing.expectEqualStrings("markdown", try stringField(contents, "kind"));
        const value = try stringField(contents, "value");
        try std.testing.expect(std.mem.find(u8, value, "Str") != null);
        try expectRange(try objectField(result, "range"), 2, 0, 2, 3);
    } else {
        try std.testing.expect(result == .null);
    }
}

/// Verifies goto definition on a builtin annotation type can reach `Builtin.roc`.
pub fn definitionHandlerNavigatesToBuiltinTypeFromTypeAnnotation() integration_spec.SpecError!void {
    // Test that clicking on a type in a type annotation (e.g., "x : Str") navigates to Builtin.roc
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "definition_type.roc" });
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

    // Document with type annotation. Position (2, 4) is on the 'U' of 'U64'.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [x] {{ pf: platform \"{s}\" }}\\n\\nx : U64\\nx = 42"}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for 'U64' on line 2, character 4 (the type in the annotation).
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":4}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, definition_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const result = try response.result();
    if (result == .object) {
        const uri = try stringField(result, "uri");
        try std.testing.expect(std.mem.endsWith(u8, uri, "Builtin.roc"));
        try expectRange(try objectField(result, "range"), 0, 0, 0, 0);
    } else {
        try std.testing.expect(result == .null);
    }
}

/// Verifies document symbols still work after a goto-definition request.
pub fn documentSymbolsWorksAfterGotoDefinitionRegressionTest() integration_spec.SpecError!void {
    // Regression test: getDocumentSymbols should use getModuleLookupEnv()
    // after getDefinitionAtPosition creates a fresh build env.
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "regression.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);
    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [result] {{ pf: platform "{s}" }}
        \\
        \\my_func = |x| x + 1
        \\
        \\result = my_func(42)
    , .{platform_path});
    defer allocator.free(roc_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "regression.roc", .data = roc_source });
    const escaped_source = try jsonEscape(allocator, roc_source);
    defer allocator.free(escaped_source);

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

    // Document with a function definition and a usage.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // First request goto definition on my_func usage (line 4, character 9).
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":9}}}}}}
    , .{file_uri});
    defer allocator.free(definition_body);
    const definition_msg = try frame(allocator, definition_body);
    defer allocator.free(definition_msg);

    // Then request document symbols from the updated module lookup environment.
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

    var builder: std.ArrayList(u8) = .empty;
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

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [32768]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var definition_response = try responseById(allocator, responses, 2);
    defer definition_response.deinit();
    try expectNullOrLocation(try definition_response.result(), file_uri, 2, 0, 2, 7);

    var symbols_response = try responseById(allocator, responses, 3);
    defer symbols_response.deinit();
    try expectSymbolNames(try symbols_response.result(), &.{ "my_func", "result" });
}

/// Verifies repeated goto-definition requests preserve later document symbols.
pub fn multipleGotoDefinitionCallsDontBreakDocumentSymbols() integration_spec.SpecError!void {
    // Test that multiple sequential goto definition calls maintain proper state
    // for subsequent document symbol requests
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "multi_def.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);
    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [baz] {{ pf: platform "{s}" }}
        \\
        \\foo = 1
        \\
        \\bar = foo
        \\
        \\baz = bar
    , .{platform_path});
    defer allocator.free(roc_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "multi_def.roc", .data = roc_source });
    const escaped_source = try jsonEscape(allocator, roc_source);
    defer allocator.free(escaped_source);

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

    // Document with multiple definitions.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // First definition request on 'foo' in bar's definition (line 4, char 6).
    const def1_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":6}}}}}}
    , .{file_uri});
    defer allocator.free(def1_body);
    const def1_msg = try frame(allocator, def1_body);
    defer allocator.free(def1_msg);

    // Second definition request on 'bar' in baz's definition (line 6, char 6).
    const def2_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":6,"character":6}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
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

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [32768]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var def1_response = try responseById(allocator, responses, 2);
    defer def1_response.deinit();
    try expectNullOrLocation(try def1_response.result(), file_uri, 2, 0, 2, 3);

    var def2_response = try responseById(allocator, responses, 3);
    defer def2_response.deinit();
    try expectNullOrLocation(try def2_response.result(), file_uri, 4, 0, 4, 3);

    var symbols_response = try responseById(allocator, responses, 4);
    defer symbols_response.deinit();
    try expectSymbolNames(try symbols_response.result(), &.{ "foo", "bar", "baz" });
}

/// Verifies document symbols report the expected Roc definition names.
pub fn documentSymbolHandlerReturnsSymbolsWithCorrectNames() integration_spec.SpecError!void {
    // Test that outline returns actual symbol names using valid Roc syntax
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "outline.roc", .data = roc_source });

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
    var escaped_source: std.ArrayList(u8) = .empty;
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, symbols_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [32768]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    try expectSymbolNames(try response.result(), &.{ "add", "myConst", "main" });
}

/// Verifies document symbols work without a prior syntax-check request.
pub fn documentSymbolHandlerWorksIndependentlyOfCheck() integration_spec.SpecError!void {
    // Regression test: document symbols should work even without a prior check() call
    // The handler should build the module itself
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "independent.roc", .data = roc_source });

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
    var escaped_source: std.ArrayList(u8) = .empty;
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, symbols_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [32768]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    try expectSymbolNames(try response.result(), &.{"hello"});
}

/// Verifies completions include module-level definitions in expression context.
pub fn completionHandlerReturnsModuleDefinitions() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "completion.roc" });
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

    // Document with two definitions and a completion site.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [result] {{ pf: platform \"{s}\" }}\\n\\nfoo = 42\\nbar = |x| x + 1\\nresult = foo"}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at the expression position after `result = `.
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":9}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectNonEmptyCompletionItems(items);
}

/// Verifies completions include imported module members after a dot.
pub fn completionHandlerReturnsModuleMembersAfterDot() integration_spec.SpecError!void {
    // Test: typing "Str." should trigger completions from the Str module
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    // Module completions can be very large depending on builtins and docs.
    var writer_buffer: [1024 * 1024]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectCompletionLabels(items, &.{"concat"});
}

/// Verifies completions include module names in expression context.
pub fn completionHandlerReturnsModuleNamesInExpressionContext() integration_spec.SpecError!void {
    // Test: in expression context, module names should be available
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"x = "}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at the end (line 0, character 4)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":4}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectCompletionLabels(items, &.{ "Str", "List", "Num" });
}

/// Verifies completions include type names after a type annotation colon.
pub fn completionHandlerReturnsTypesAfterColon() integration_spec.SpecError!void {
    // Test: typing "x :" should trigger type completions
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectCompletionLabels(items, &.{ "Str", "U64", "Bool" });
}

/// Verifies completions include `List` module members after `List.`.
pub fn completionHandlerReturnsListModuleMembersAfterListDot() integration_spec.SpecError!void {
    // Test: typing "List." should trigger completions from the List module
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    // The completion response enumerates every member of the List module,
    // so this buffer must be large enough to fit the full JSON for all of them.
    var writer_buffer: [65536]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectCompletionLabels(items, &.{"map"});
}

/// Verifies completions include local variables visible inside a block.
pub fn completionHandlerReturnsLocalVariablesInBlockScope() integration_spec.SpecError!void {
    // Test: local variables defined in a block should appear in completions
    // within that block
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "local_completion.roc" });
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

    // Document with local variable in a block:
    // main = {
    //     local_var = 42
    //     local_var
    // }
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main] {{ pf: platform \"{s}\" }}\\n\\nmain = {{{{\\n    local_var = 42\\n    local_var\\n}}}}"}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at line 4, character 4 (inside the block, after local_var is defined).
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectNonEmptyCompletionItems(items);
}

/// Verifies completions include lambda parameters inside the lambda body.
pub fn completionHandlerReturnsLambdaParameters() integration_spec.SpecError!void {
    // Test: lambda parameters should appear in completions within the lambda body
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "lambda_param_completion.roc" });
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

    // Document with a lambda that has parameters:
    // add = |first, second| first + second
    // Cursor position should be inside the lambda body
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [add] {{ pf: platform \"{s}\" }}\\n\\nadd = |first, second| first + second"}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at line 2, character 30 (inside lambda body, before `second`).
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":30}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectNonEmptyCompletionItems(items);
}

/// Verifies completions include top-level definitions in a module.
pub fn completionHandlerReturnsTopLevelDefinitions() integration_spec.SpecError!void {
    // Test: top-level definitions should appear in completions
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
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
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"my_constant = 42\nmy_function = |x| x * 2\nmain! = |_| my_constant"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request completion at line 2, character 0 (beginning of main! line)
    const completion_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":0}}}}}}
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try expectCompletionLabels(items, &.{ "my_constant", "my_function" });
}

/// Verifies completions include record fields after a record dot access.
pub fn completionHandlerReturnsRecordFieldsAfterDot() integration_spec.SpecError!void {
    // Test: typing "rec." where rec is a record should trigger field completions
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "record_completion.roc" });
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

    // Document with a record variable and field access
    // rec = { name: "hello", age: 42 }
    // x = rec.name
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [x] {{ pf: platform \"{s}\" }}\n\nrec = {{ name: \"hello\", age: 42 }}\nx = rec.name"}}}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const change_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{s}","version":2}},"contentChanges":[{{"text":"app [x] {{ pf: platform \"{s}\" }}\n\nrec = {{ name: \"hello\", age: 42 }}\nx = rec."}}]}}}}
    , .{ file_uri, platform_path });
    defer allocator.free(change_body);
    const change_msg = try frame(allocator, change_body);
    defer allocator.free(change_msg);

    // Request completion right after "rec." (line 3, character 8).
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_msg);
    try builder.appendSlice(allocator, change_msg);
    try builder.appendSlice(allocator, completion_msg);
    try builder.appendSlice(allocator, shutdown_msg);
    try builder.appendSlice(allocator, exit_msg);
    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [16384]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var server = try server_module.Server(ReaderType, WriterType).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    const responses = try collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var response = try responseById(allocator, responses, 2);
    defer response.deinit();
    const items = try completionItems(try response.result());
    try std.testing.expect(items.array.items.len > 0);
    for (items.array.items) |item| {
        const label = try stringField(item, "label");
        if (std.mem.eql(u8, label, "name") or std.mem.eql(u8, label, "age")) {
            try std.testing.expectEqual(@as(i64, 5), try integerField(item, "kind"));
        }
    }
}
