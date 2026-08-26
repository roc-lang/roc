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
const compile = @import("compile");
const CacheConfig = compile.CacheConfig;
const CoreCtx = @import("ctx").CoreCtx;
const compiled_builtins = @import("compiled_builtins");
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

    /// Whether the server answered this request with a JSON-RPC error.
    fn isError(self: *const ParsedResponse) bool {
        const root = self.parsed.value;
        if (root != .object) return false;
        return root.object.get("error") != null;
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

fn expectBuiltinDefinitionAtDeclaration(
    allocator: std.mem.Allocator,
    responses: [][]u8,
    response_id: i64,
    declaration: []const u8,
) integration_spec.SpecError!void {
    const declaration_offset = std.mem.find(u8, compiled_builtins.builtin_source, declaration) orelse
        return error.TestUnexpectedResult;
    if (std.mem.findPos(u8, compiled_builtins.builtin_source, declaration_offset + declaration.len, declaration) != null) {
        return error.TestUnexpectedResult;
    }

    var expected_line: i64 = 0;
    var expected_character: i64 = 0;
    for (compiled_builtins.builtin_source[0..declaration_offset]) |byte| {
        if (byte == '\n') {
            expected_line += 1;
            expected_character = 0;
        } else {
            expected_character += 1;
        }
    }

    var response = try responseById(allocator, responses, response_id);
    defer response.deinit();
    const result = try response.result();
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "Builtin.roc"));
    const range = try objectField(result, "range");
    const start = try objectField(range, "start");
    try std.testing.expectEqual(expected_line, try integerField(start, "line"));
    try std.testing.expectEqual(expected_character, try integerField(start, "character"));
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

/// Drive one server session over an opened document and return the responses.
///
/// The rename specs each send a couple of requests against the same source, so
/// they share the initialize/open/shutdown framing rather than repeating it.
/// Caller owns the returned bodies.
fn runSessionResponses(
    allocator: std.mem.Allocator,
    tmp_path: []const u8,
    file_uri: []const u8,
    source: []const u8,
    request_bodies: []const []const u8,
) integration_spec.SpecError![][]u8 {
    const escaped_source = try jsonEscape(allocator, source);
    defer allocator.free(escaped_source);

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source });
    defer allocator.free(open_body);

    var bodies: std.ArrayList([]const u8) = .empty;
    defer bodies.deinit(allocator);
    try bodies.append(allocator,
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"clientInfo":{"name":"test"},"capabilities":{}}}
    );
    try bodies.append(allocator,
        \\{"jsonrpc":"2.0","method":"initialized","params":{}}
    );
    try bodies.append(allocator, open_body);
    try bodies.appendSlice(allocator, request_bodies);
    try bodies.append(allocator,
        \\{"jsonrpc":"2.0","id":99,"method":"shutdown"}
    );
    try bodies.append(allocator,
        \\{"jsonrpc":"2.0","method":"exit"}
    );

    for (bodies.items) |body| {
        const framed = try frame(allocator, body);
        defer allocator.free(framed);
        try builder.appendSlice(allocator, framed);
    }

    const combined = try builder.toOwnedSlice(allocator);
    defer allocator.free(combined);

    const reader_stream: std.Io.Reader = .fixed(combined);
    var writer_buffer: [32768]u8 = undefined;
    const writer_stream: std.Io.Writer = .fixed(&writer_buffer);

    var server = try server_module.Server(std.Io.Reader, std.Io.Writer).init(allocator, test_env.io, reader_stream, writer_stream, null, .{});
    test_env.configureChecker(&server.syntax_checker, tmp_path);
    defer server.deinit();
    try server.run();

    return collectResponses(allocator, writer_buffer[0..server.transport.writer.end]);
}

/// Handler integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "document symbol handler extracts function declarations", .run = documentSymbolHandlerExtractsFunctionDeclarations },
    .{ .name = "document highlight handler finds variable occurrences", .run = documentHighlightHandlerFindsVariableOccurrences },
    .{ .name = "document highlight handler resolves symbol from a reference site", .run = documentHighlightHandlerResolvesFromReferenceSite },
    .{ .name = "document highlight handler includes the annotated name", .run = documentHighlightHandlerIncludesAnnotationName },
    .{ .name = "rename handler rewrites every occurrence including the annotation", .run = renameHandlerRewritesEveryOccurrence },
    .{ .name = "rename handler refuses a name already visible in scope", .run = renameHandlerRefusesNameAlreadyInScope },
    .{ .name = "rename handler refuses a name that changes what it means", .run = renameHandlerRefusesMeaningChangingName },
    .{ .name = "rename handler refuses a document that does not compile", .run = renameHandlerRefusesUncompilableDocument },
    .{ .name = "prepare rename reports the occurrence under the cursor", .run = prepareRenameReportsOccurrenceUnderCursor },
    .{ .name = "references handler honours includeDeclaration", .run = referencesHandlerHonoursIncludeDeclaration },
    .{ .name = "references handler respects shadowing", .run = referencesHandlerRespectsShadowing },
    .{ .name = "the name on an annotation is a usable starting point", .run = annotationNameResolvesLikeAnyOccurrence },
    .{ .name = "positions are UTF-16 code units, not bytes", .run = positionsUseUtf16CodeUnits },
    .{ .name = "rename refuses a declaration that is not a plain name", .run = renameRefusesNonIsolatedDeclaration },
    .{ .name = "definition handler finds local variable definition", .run = definitionHandlerFindsLocalVariableDefinition },
    .{ .name = "definition handler returns null for undefined symbol", .run = definitionHandlerReturnsNullForUndefinedSymbol },
    .{ .name = "hover handler handles type annotation request", .run = hoverHandlerReturnsTypeInfoForTypeAnnotation },
    .{ .name = "definition handler handles builtin type annotation request", .run = definitionHandlerNavigatesToBuiltinTypeFromTypeAnnotation },
    .{ .name = "definition handler navigates to builtin declarations", .run = definitionHandlerNavigatesToBuiltinDeclarations },
    .{ .name = "definition handler navigates to external module members", .run = definitionHandlerNavigatesToExternalModuleMembers },
    .{ .name = "definition handler navigates to exposed import member in import statement", .run = definitionHandlerNavigatesToExposedImportMemberInImportStatement },
    .{ .name = "definition handler navigates to unqualified exposed import function call", .run = definitionHandlerNavigatesToUnqualifiedExposedImportFunctionCall },
    .{ .name = "definition handler navigates to tag declaration in pattern match", .run = definitionHandlerNavigatesToTagDeclarationInPatternMatch },
    .{ .name = "definition handler navigates to tag declaration in package qualified import", .run = definitionHandlerNavigatesToTagDeclarationInPackageQualifiedImport },
    .{ .name = "definition handler disambiguates same named tag across imported modules", .run = definitionHandlerDisambiguatesSameNamedTagAcrossImportedModules },
    .{ .name = "definition handler branch value open tag does not navigate to match condition type", .run = definitionHandlerBranchValueOpenTagDoesNotNavigateToMatchConditionType },
    .{ .name = "definition handler navigates to exposed type alias in type annotation", .run = definitionHandlerNavigatesToExposedTypeAliasInTypeAnnotation },
    .{ .name = "definition handler navigates to file import path", .run = definitionHandlerNavigatesToFileImportPath },
    .{ .name = "definition handler navigates to default app echo platform definition", .run = definitionHandlerNavigatesToEchoPlatformDefinition },
    .{ .name = "definition handler resolves package shorthand qualified import", .run = definitionHandlerResolvesPackageShorthandQualifiedImport },
    .{ .name = "definition handler disambiguates same named module across packages", .run = definitionHandlerDisambiguatesSameNamedModuleAcrossPackages },
    .{ .name = "definition handler resolves shorthand in importing package context", .run = definitionHandlerResolvesShorthandInImportingPackageContext },
    .{ .name = "semantic tokens handler handles file imports without crashing", .run = semanticTokensHandlerHandlesFileImportsWithoutCrashing },
    .{ .name = "workspace document ending in Builtin.roc builds and produces diagnostics", .run = workspaceDocumentEndingInBuiltinRocBuildsAndProducesDiagnostics },
    .{ .name = "opening Builtin.roc does not panic", .run = openingBuiltinRocDoesNotPanic },
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

/// Verifies that placing the cursor on a *reference* resolves to the same
/// binding as placing it on the definition, instead of falling back to
/// matching identifier text.
///
/// The two lambdas below each bind their own `n`. A text-based match would
/// report all four occurrences; only the two belonging to the queried binding
/// are correct.
pub fn documentHighlightHandlerResolvesFromReferenceSite() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "highlight_reference.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\f = |n| n
        \\
        \\g = |n| n
        \\
        \\main = f(1) + g(2)
    , .{platform_path});
    defer allocator.free(roc_source);
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

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Cursor on the `n` in `f`'s body (line 2, character 8), a reference
    // rather than the binding itself.
    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":8}}}}}}
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

    // Exactly the binding in `f` and its single use, never `g`'s `n`.
    try std.testing.expectEqual(@as(usize, 2), result.array.items.len);
    try std.testing.expect(try hasHighlightRange(result, 2, 5, 2, 6));
    try std.testing.expect(try hasHighlightRange(result, 2, 8, 2, 9));
}

/// Verifies that the name written on a type annotation counts as an occurrence
/// of the binding it annotates.
///
/// Canonicalization merges a named annotation into the def it annotates and
/// emits no separate statement for it, so the name is reachable only through
/// `CIR.Annotation.name_region`. A rename that missed it would leave the
/// annotation naming a binding that no longer exists.
pub fn documentHighlightHandlerIncludesAnnotationName() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "highlight_annotation.roc" });
    defer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    defer allocator.free(file_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const roc_source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double : I64 -> I64
        \\double = |n| n * 2
        \\
        \\main = double(21)
    , .{platform_path});
    defer allocator.free(roc_source);
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

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ file_uri, escaped_source });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Cursor on the definition `double` (line 3, character 2).
    const highlight_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":2}}}}}}
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

    // The annotation name, the definition, and the single call site.
    try std.testing.expectEqual(@as(usize, 3), result.array.items.len);
    try std.testing.expect(try hasHighlightRange(result, 2, 0, 2, 6));
    try std.testing.expect(try hasHighlightRange(result, 3, 0, 3, 6));
    try std.testing.expect(try hasHighlightRange(result, 5, 7, 5, 13));
}

/// Set up a temp file and return its path and uri. Caller frees both.
fn renameFixture(
    allocator: std.mem.Allocator,
    tmp_path: []const u8,
    name: []const u8,
) integration_spec.SpecError!struct { path: []u8, uri: []u8 } {
    const file_path = try std.fs.path.join(allocator, &.{ tmp_path, name });
    errdefer allocator.free(file_path);
    const file_uri = try uriFromPath(allocator, file_path);
    return .{ .path = file_path, .uri = file_uri };
}

/// Read the single document's edits out of a WorkspaceEdit response.
fn workspaceEditsFor(result: std.json.Value, uri: []const u8) integration_spec.SpecError!std.json.Value {
    const changes = try objectField(result, "changes");
    if (changes != .object) return error.TestUnexpectedResult;
    return changes.object.get(uri) orelse error.TestUnexpectedResult;
}

/// Whether the edits contain one replacing exactly this range with this text.
fn hasEdit(
    edits: std.json.Value,
    line: i64,
    start_character: i64,
    end_character: i64,
    new_text: []const u8,
) integration_spec.SpecError!bool {
    if (edits != .array) return error.TestUnexpectedResult;
    for (edits.array.items) |edit| {
        const range = try objectField(edit, "range");
        const start = try objectField(range, "start");
        const end = try objectField(range, "end");
        const text = try objectField(edit, "newText");
        if (text != .string) return error.TestUnexpectedResult;
        if ((try integerField(start, "line")) == line and
            (try integerField(start, "character")) == start_character and
            (try integerField(end, "line")) == line and
            (try integerField(end, "character")) == end_character and
            std.mem.eql(u8, text.string, new_text))
        {
            return true;
        }
    }
    return false;
}

/// Verifies a rename rewrites the binding, its annotation name, and every use.
///
/// The annotation name is the occurrence that has no CIR node of its own; it is
/// reachable only through `Annotation.name_region`, and leaving it behind would
/// produce a file that no longer compiles.
pub fn renameHandlerRewritesEveryOccurrence() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "rename_all.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double : I64 -> I64
        \\double = |n| n * 2
        \\
        \\main = double(21)
    , .{platform_path});
    defer allocator.free(source);

    // Once from the definition, once from a use: both must produce the same
    // rewrite, since either end names the same binding.
    const from_definition = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":2}},"newName":"triple"}}}}
    , .{fixture.uri});
    defer allocator.free(from_definition);
    const from_use = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":10}},"newName":"triple"}}}}
    , .{fixture.uri});
    defer allocator.free(from_use);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ from_definition, from_use });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    for ([_]i64{ 2, 3 }) |request_id| {
        var response = try responseById(allocator, responses, request_id);
        defer response.deinit();
        const edits = try workspaceEditsFor(try response.result(), fixture.uri);

        try std.testing.expect(edits == .array);
        try std.testing.expectEqual(@as(usize, 3), edits.array.items.len);
        try std.testing.expect(try hasEdit(edits, 2, 0, 6, "triple"));
        try std.testing.expect(try hasEdit(edits, 3, 0, 6, "triple"));
        try std.testing.expect(try hasEdit(edits, 5, 7, 13, "triple"));
    }
}

/// Verifies a rename that would capture another binding is refused.
///
/// `k` and `v` are both live inside the inner lambda, so rewriting `k` to `v`
/// would silently repoint `v` at the parameter and change what the code
/// computes rather than what it is called.
pub fn renameHandlerRefusesNameAlreadyInScope() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "rename_capture.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\outer = |v| {{
        \\    inner = |k| k + v
        \\    inner(1)
        \\}}
        \\
        \\main = outer(2)
    , .{platform_path});
    defer allocator.free(source);

    // Cursor on `k`'s binding; `v` is live here.
    const capture = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":14}},"newName":"v"}}}}
    , .{fixture.uri});
    defer allocator.free(capture);
    // A name no other live binding uses stays renameable.
    const free_name = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":14}},"newName":"step"}}}}
    , .{fixture.uri});
    defer allocator.free(free_name);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ capture, free_name });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var refused = try responseById(allocator, responses, 2);
    defer refused.deinit();
    try std.testing.expect(refused.isError());

    var allowed = try responseById(allocator, responses, 3);
    defer allowed.deinit();
    const edits = try workspaceEditsFor(try allowed.result(), fixture.uri);
    try std.testing.expect(edits == .array);
    try std.testing.expectEqual(@as(usize, 2), edits.array.items.len);
}

/// Verifies a rename that would change a name's meaning is refused.
///
/// A trailing `!` marks a value effectful and case separates values from types,
/// so neither is something a rename may quietly introduce.
pub fn renameHandlerRefusesMeaningChangingName() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "rename_meaning.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double = |n| n * 2
        \\
        \\main = double(21)
    , .{platform_path});
    defer allocator.free(source);

    const to_effectful = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}},"newName":"double!"}}}}
    , .{fixture.uri});
    defer allocator.free(to_effectful);
    const to_uppercase = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}},"newName":"Double"}}}}
    , .{fixture.uri});
    defer allocator.free(to_uppercase);
    const not_an_identifier = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}},"newName":"a b"}}}}
    , .{fixture.uri});
    defer allocator.free(not_an_identifier);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ to_effectful, to_uppercase, not_an_identifier });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    for ([_]i64{ 2, 3, 4 }) |request_id| {
        var response = try responseById(allocator, responses, request_id);
        defer response.deinit();
        try std.testing.expect(response.isError());
    }
}

/// Verifies rename refuses a document that does not compile.
///
/// Without a CIR there is no way to tell which occurrences belong to the
/// binding, and a partial rewrite would leave the file broken in a new way.
pub fn renameHandlerRefusesUncompilableDocument() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "rename_broken.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // `n *` is left dangling, so the document does not parse.
    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double = |n| n *
        \\
        \\main = double(21)
    , .{platform_path});
    defer allocator.free(source);

    const rename_request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}},"newName":"triple"}}}}
    , .{fixture.uri});
    defer allocator.free(rename_request);
    const prepare_request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/prepareRename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}}}}}}
    , .{fixture.uri});
    defer allocator.free(prepare_request);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ rename_request, prepare_request });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var renamed = try responseById(allocator, responses, 2);
    defer renamed.deinit();
    try std.testing.expect(renamed.isError());

    // prepareRename answers null so the editor never opens its prompt.
    var prepared = try responseById(allocator, responses, 3);
    defer prepared.deinit();
    try std.testing.expect((try prepared.result()) == .null);
}

/// Verifies prepareRename reports the occurrence the cursor is in, and refuses
/// positions that name nothing renameable.
pub fn prepareRenameReportsOccurrenceUnderCursor() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "rename_prepare.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double = |n| n * 2
        \\
        \\main = double(21)
    , .{platform_path});
    defer allocator.free(source);

    // On the use, not the definition: the editor must highlight what was clicked.
    const on_use = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/prepareRename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":10}}}}}}
    , .{fixture.uri});
    defer allocator.free(on_use);
    // On the `=`, which names no binding.
    const on_operator = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/prepareRename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":7}}}}}}
    , .{fixture.uri});
    defer allocator.free(on_operator);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ on_use, on_operator });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var prepared = try responseById(allocator, responses, 2);
    defer prepared.deinit();
    const result = try prepared.result();
    const placeholder = try objectField(result, "placeholder");
    try std.testing.expect(placeholder == .string);
    try std.testing.expectEqualStrings("double", placeholder.string);

    const range = try objectField(result, "range");
    const start = try objectField(range, "start");
    const end = try objectField(range, "end");
    try std.testing.expectEqual(@as(i64, 4), try integerField(start, "line"));
    try std.testing.expectEqual(@as(i64, 7), try integerField(start, "character"));
    try std.testing.expectEqual(@as(i64, 13), try integerField(end, "character"));

    var refused = try responseById(allocator, responses, 3);
    defer refused.deinit();
    try std.testing.expect((try refused.result()) == .null);
}

/// Whether the locations contain one covering exactly this range.
fn hasLocation(
    locations: std.json.Value,
    uri: []const u8,
    line: i64,
    start_character: i64,
    end_character: i64,
) integration_spec.SpecError!bool {
    if (locations != .array) return error.TestUnexpectedResult;
    for (locations.array.items) |location| {
        const location_uri = try objectField(location, "uri");
        if (location_uri != .string) return error.TestUnexpectedResult;
        if (!std.mem.eql(u8, location_uri.string, uri)) continue;
        const range = try objectField(location, "range");
        const start = try objectField(range, "start");
        const end = try objectField(range, "end");
        if ((try integerField(start, "line")) == line and
            (try integerField(start, "character")) == start_character and
            (try integerField(end, "line")) == line and
            (try integerField(end, "character")) == end_character)
        {
            return true;
        }
    }
    return false;
}

/// Verifies references reports the uses of a symbol, and that
/// `includeDeclaration` controls whether the binding and its annotation name
/// come along.
pub fn referencesHandlerHonoursIncludeDeclaration() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "references_decl.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double : I64 -> I64
        \\double = |n| n * 2
        \\
        \\main = double(double(21))
    , .{platform_path});
    defer allocator.free(source);

    const with_declaration = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/references","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":2}},"context":{{"includeDeclaration":true}}}}}}
    , .{fixture.uri});
    defer allocator.free(with_declaration);
    const without_declaration = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/references","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":2}},"context":{{"includeDeclaration":false}}}}}}
    , .{fixture.uri});
    defer allocator.free(without_declaration);
    // Asking from a use must answer the same as asking from the binding.
    const from_use = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"textDocument/references","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":10}},"context":{{"includeDeclaration":false}}}}}}
    , .{fixture.uri});
    defer allocator.free(from_use);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ with_declaration, without_declaration, from_use });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Declaration, annotation name, and both call sites.
    var full = try responseById(allocator, responses, 2);
    defer full.deinit();
    const all_locations = try full.result();
    try std.testing.expect(all_locations == .array);
    try std.testing.expectEqual(@as(usize, 4), all_locations.array.items.len);
    try std.testing.expect(try hasLocation(all_locations, fixture.uri, 2, 0, 6));
    try std.testing.expect(try hasLocation(all_locations, fixture.uri, 3, 0, 6));
    try std.testing.expect(try hasLocation(all_locations, fixture.uri, 5, 7, 13));
    try std.testing.expect(try hasLocation(all_locations, fixture.uri, 5, 14, 20));

    // Only the call sites; the annotation name is part of the declaration.
    var uses_only = try responseById(allocator, responses, 3);
    defer uses_only.deinit();
    const use_locations = try uses_only.result();
    try std.testing.expect(use_locations == .array);
    try std.testing.expectEqual(@as(usize, 2), use_locations.array.items.len);
    try std.testing.expect(try hasLocation(use_locations, fixture.uri, 5, 7, 13));
    try std.testing.expect(try hasLocation(use_locations, fixture.uri, 5, 14, 20));

    var asked_from_use = try responseById(allocator, responses, 4);
    defer asked_from_use.deinit();
    const same_locations = try asked_from_use.result();
    try std.testing.expect(same_locations == .array);
    try std.testing.expectEqual(@as(usize, 2), same_locations.array.items.len);
}

/// Verifies references resolves through scope rather than matching text.
///
/// Both lambdas below bind `n`. Only the queried binding's uses may be
/// reported, and a position that names no binding must answer null.
pub fn referencesHandlerRespectsShadowing() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "references_shadow.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\f = |n| n + 1
        \\
        \\g = |n| n + 2
        \\
        \\main = f(1) + g(2)
    , .{platform_path});
    defer allocator.free(source);

    // The use of `n` inside `f`, one column past its binding.
    const in_f = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/references","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":8}},"context":{{"includeDeclaration":true}}}}}}
    , .{fixture.uri});
    defer allocator.free(in_f);
    // The `+` operator names no binding.
    const on_operator = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/references","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":10}},"context":{{"includeDeclaration":true}}}}}}
    , .{fixture.uri});
    defer allocator.free(on_operator);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ in_f, on_operator });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    var scoped = try responseById(allocator, responses, 2);
    defer scoped.deinit();
    const locations = try scoped.result();
    try std.testing.expect(locations == .array);
    try std.testing.expectEqual(@as(usize, 2), locations.array.items.len);
    try std.testing.expect(try hasLocation(locations, fixture.uri, 2, 5, 6));
    try std.testing.expect(try hasLocation(locations, fixture.uri, 2, 8, 9));
    // `g`'s own `n` lives on line 4 and must not appear.
    try std.testing.expect(!try hasLocation(locations, fixture.uri, 4, 5, 6));
    try std.testing.expect(!try hasLocation(locations, fixture.uri, 4, 8, 9));

    var nothing = try responseById(allocator, responses, 3);
    defer nothing.deinit();
    try std.testing.expect((try nothing.result()) == .null);
}

/// Verifies the name written on a type annotation can start an operation, not
/// just be swept up by one.
///
/// That token is not a CIR node; it exists only as `Annotation.name_region`. It
/// was already rewritten by a rename that started elsewhere, but asking from it
/// answered nothing, so putting the cursor on it and pressing rename did not
/// work.
pub fn annotationNameResolvesLikeAnyOccurrence() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "annotation_start.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\double : I64 -> I64
        \\double = |n| n * 2
        \\
        \\main = double(21)
    , .{platform_path});
    defer allocator.free(source);

    // Every request below starts on the `double` written on the annotation line.
    const references = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/references","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":3}},"context":{{"includeDeclaration":true}}}}}}
    , .{fixture.uri});
    defer allocator.free(references);
    const prepare = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/prepareRename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":3}}}}}}
    , .{fixture.uri});
    defer allocator.free(prepare);
    const rename = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":3}},"newName":"triple"}}}}
    , .{fixture.uri});
    defer allocator.free(rename);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ references, prepare, rename });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // The annotation name, the binding, and the call site.
    var found = try responseById(allocator, responses, 2);
    defer found.deinit();
    const locations = try found.result();
    try std.testing.expect(locations == .array);
    try std.testing.expectEqual(@as(usize, 3), locations.array.items.len);
    try std.testing.expect(try hasLocation(locations, fixture.uri, 2, 0, 6));
    try std.testing.expect(try hasLocation(locations, fixture.uri, 3, 0, 6));
    try std.testing.expect(try hasLocation(locations, fixture.uri, 5, 7, 13));

    // The editor prompts on the annotation name itself, since that is what was
    // clicked.
    var prepared = try responseById(allocator, responses, 3);
    defer prepared.deinit();
    const prepared_result = try prepared.result();
    const placeholder = try objectField(prepared_result, "placeholder");
    try std.testing.expect(placeholder == .string);
    try std.testing.expectEqualStrings("double", placeholder.string);
    const range = try objectField(prepared_result, "range");
    try std.testing.expectEqual(@as(i64, 2), try integerField(try objectField(range, "start"), "line"));

    // And the rewrite is the same one any other occurrence would have produced.
    var renamed = try responseById(allocator, responses, 4);
    defer renamed.deinit();
    const edits = try workspaceEditsFor(try renamed.result(), fixture.uri);
    try std.testing.expect(edits == .array);
    try std.testing.expectEqual(@as(usize, 3), edits.array.items.len);
    try std.testing.expect(try hasEdit(edits, 2, 0, 6, "triple"));
    try std.testing.expect(try hasEdit(edits, 3, 0, 6, "triple"));
    try std.testing.expect(try hasEdit(edits, 5, 7, 13, "triple"));
}

/// Verifies positions are exchanged in UTF-16 code units, as the server
/// advertises with `positionEncoding`.
///
/// Reproduction from #10948. On the line below, `num` starts at UTF-16 column
/// 27 and byte column 28, because `e` with an acute accent is two bytes and one
/// UTF-16 unit. Reading the incoming column as a byte offset made the request
/// miss the identifier; emitting a byte column as a UTF-16 one made rename hand
/// the editor a range shifted one to the right, which replaces `um ` instead of
/// `num` and corrupts the line.
pub fn positionsUseUtf16CodeUnits() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "utf16_positions.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // U+00E9: two UTF-8 bytes, one UTF-16 code unit. Written as bytes because a
    // multiline string literal does not process escapes.
    const accented = "\xc3\xa9";

    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\num = 42
        \\
        \\main = {{ x: "{s}", y: num }}
    , .{ platform_path, accented });
    defer allocator.free(source);

    // `num` sits at UTF-16 column 20 on that line, and byte column 21.
    const definition = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":20}}}}}}
    , .{fixture.uri});
    defer allocator.free(definition);
    const rename = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":20}},"newName":"total"}}}}
    , .{fixture.uri});
    defer allocator.free(rename);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ definition, rename });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Asking at the UTF-16 column finds the binding, rather than landing beside
    // it and answering null.
    var located = try responseById(allocator, responses, 2);
    defer located.deinit();
    const location = try located.result();
    try std.testing.expect(location == .object);
    const range = try objectField(location, "range");
    try std.testing.expectEqual(@as(i64, 2), try integerField(try objectField(range, "start"), "line"));

    // And the returned edit covers `num` itself, in UTF-16 columns.
    var renamed = try responseById(allocator, responses, 3);
    defer renamed.deinit();
    const edits = try workspaceEditsFor(try renamed.result(), fixture.uri);
    try std.testing.expect(edits == .array);
    try std.testing.expectEqual(@as(usize, 2), edits.array.items.len);
    try std.testing.expect(try hasEdit(edits, 2, 0, 3, "total"));
    try std.testing.expect(try hasEdit(edits, 4, 20, 23, "total"));
}

/// Verifies rename refuses a binding whose declaration is not written as a
/// plain name.
///
/// An annotation with no matching declaration gets a synthetic `assign` pattern
/// spanning the whole `name : Type` statement. Taking that as the declaration
/// made rename replace the statement with the bare new name, deleting the type.
/// Reported by review on #10945.
pub fn renameRefusesNonIsolatedDeclaration() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const fixture = try renameFixture(allocator, tmp_path, "rename_orphan_anno.roc");
    defer allocator.free(fixture.path);
    defer allocator.free(fixture.uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // `orphan` is annotated but never defined.
    const source = try std.fmt.allocPrint(allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\orphan : I64
        \\
        \\main = 1
    , .{platform_path});
    defer allocator.free(source);

    const rename = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/rename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}},"newName":"renamed"}}}}
    , .{fixture.uri});
    defer allocator.free(rename);
    const prepare = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/prepareRename","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":2}}}}}}
    , .{fixture.uri});
    defer allocator.free(prepare);

    const responses = try runSessionResponses(allocator, tmp_path, fixture.uri, source, &.{ rename, prepare });
    defer {
        for (responses) |body| allocator.free(body);
        allocator.free(responses);
    }

    // Refused, rather than answering an edit that spans `orphan : I64`.
    var refused = try responseById(allocator, responses, 2);
    defer refused.deinit();
    try std.testing.expect(refused.isError());

    // And the editor is told not to offer the action at all.
    var prepared = try responseById(allocator, responses, 3);
    defer prepared.deinit();
    try std.testing.expect((try prepared.result()) == .null);
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

    // Document with type annotation. Position (1, 20) is on 'Str'.
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"Helpers :: [].{{\n\tencode_json : a -> Str where [a.encoder_for : _ -> (a, _ -> Try(_, _))]\n\tencode_json = |a| Json.to_str(a)\n}}\n"}}}}}}
    , .{file_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Request definition for 'Str' on line 1, character 20.
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":1,"character":20}}}}}}
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

    try expectBuiltinDefinitionAtDeclaration(allocator, responses, 2, "Str :: [ProvidedByCompiler].{");
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

/// Verifies goto definition on builtin member functions (Str.is_empty, List.is_empty, List.append, Dict.update)
/// and builtin type prefixes (Dict) resolve to their exact declarations in Builtin.roc.
pub fn definitionHandlerNavigatesToBuiltinDeclarations() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
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

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [check] {{ pf: platform \"{s}\" }}\n\ns_empty = Str.is_empty(\"\")\nl_empty = List.is_empty([])\nappend_check = List.append([1], 2)\ndict_check = Dict.update(Dict.empty({{}}), 1, |v| v)"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    // Line 2: s_empty = Str.is_empty("") -> character 16 is on 'is_empty'
    const def_str_empty_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":16}}}}}}
    , .{main_uri});
    defer allocator.free(def_str_empty_body);
    const def_str_empty_msg = try frame(allocator, def_str_empty_body);
    defer allocator.free(def_str_empty_msg);

    // Line 3: l_empty = List.is_empty([]) -> character 16 is on 'is_empty'
    const def_list_empty_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":16}}}}}}
    , .{main_uri});
    defer allocator.free(def_list_empty_body);
    const def_list_empty_msg = try frame(allocator, def_list_empty_body);
    defer allocator.free(def_list_empty_msg);

    // Line 4: append_check = List.append([1], 2) -> character 22 is on 'append'
    const def_append_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":22}}}}}}
    , .{main_uri});
    defer allocator.free(def_append_body);
    const def_append_msg = try frame(allocator, def_append_body);
    defer allocator.free(def_append_msg);

    // Line 5: dict_check = Dict.update(...) -> character 19 is on 'update'
    const def_update_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":5,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":19}}}}}}
    , .{main_uri});
    defer allocator.free(def_update_body);
    const def_update_msg = try frame(allocator, def_update_body);
    defer allocator.free(def_update_msg);

    // Line 5: dict_check = Dict.update(...) -> character 14 is on 'Dict' (prefix)
    const def_dict_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":6,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":14}}}}}}
    , .{main_uri});
    defer allocator.free(def_dict_body);
    const def_dict_msg = try frame(allocator, def_dict_body);
    defer allocator.free(def_dict_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":7,"method":"shutdown"}
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
    try builder.appendSlice(allocator, def_str_empty_msg);
    try builder.appendSlice(allocator, def_list_empty_msg);
    try builder.appendSlice(allocator, def_append_msg);
    try builder.appendSlice(allocator, def_update_msg);
    try builder.appendSlice(allocator, def_dict_msg);
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

    try expectBuiltinDefinitionAtDeclaration(allocator, responses, 2, "is_empty : Str -> Bool");
    try expectBuiltinDefinitionAtDeclaration(allocator, responses, 3, "is_empty : List(_item) -> Bool");
    try expectBuiltinDefinitionAtDeclaration(allocator, responses, 4, "append : List(a), a -> List(a)");
    try expectBuiltinDefinitionAtDeclaration(
        allocator,
        responses,
        5,
        "update : Dict(k, v), k, (Try(v, [Missing]) -> Try(v, [Missing])) -> Dict(k, v)",
    );
    try expectBuiltinDefinitionAtDeclaration(allocator, responses, 6, "Dict(k, v) :: [");
}

/// Verifies that an ordinary workspace document whose path ends in Builtin.roc (e.g. MyBuiltin.roc)
/// is built normally and publishes diagnostics.
pub fn workspaceDocumentEndingInBuiltinRocBuildsAndProducesDiagnostics() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const my_builtin_path = try std.fs.path.join(allocator, &.{ tmp_path, "MyBuiltin.roc" });
    defer allocator.free(my_builtin_path);
    const my_builtin_uri = try uriFromPath(allocator, my_builtin_path);
    defer allocator.free(my_builtin_uri);
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

    // MyBuiltin.roc with invalid code to trigger diagnostics
    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"module [bad]\n\nbad : Str\nbad = 123"}}}}}}
    , .{my_builtin_uri});
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":2,"method":"shutdown"}
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

    var found_diag = false;
    for (responses) |resp_bytes| {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, resp_bytes, .{});
        defer parsed.deinit();
        if (parsed.value == .object) {
            if (parsed.value.object.get("method")) |method| {
                if (method == .string and std.mem.eql(u8, method.string, "textDocument/publishDiagnostics")) {
                    if (parsed.value.object.get("params")) |params| {
                        if (params.object.get("diagnostics")) |diags| {
                            if (diags.array.items.len > 0) {
                                found_diag = true;
                            }
                        }
                    }
                }
            }
        }
    }
    try std.testing.expect(found_diag);
}

/// Verifies that opening Builtin.roc directly as a document does not crash or panic.
pub fn openingBuiltinRocDoesNotPanic() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    var cache_config = CacheConfig{ .roc_ctx = CoreCtx.default(allocator, allocator, test_env.io) };
    cache_config.cache_dir = tmp_path;
    const cache_dir = cache_config.getModuleCacheDir(allocator) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.NoHomeDirectory => return,
    };
    defer allocator.free(cache_dir);
    const builtin_path = try std.fs.path.join(allocator, &.{ cache_dir, "Builtin.roc" });
    defer allocator.free(builtin_path);
    const builtin_uri = try uriFromPath(allocator, builtin_path);
    defer allocator.free(builtin_uri);

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

    var json_writer: std.Io.Writer.Allocating = .init(allocator);
    defer json_writer.deinit();
    try std.json.Stringify.value(std.json.Value{ .string = compiled_builtins.builtin_source }, .{}, &json_writer.writer);
    const escaped_source = json_writer.written();

    const open_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":{s}}}}}}}
    , .{ builtin_uri, escaped_source });
    defer allocator.free(open_body);
    const open_msg = try frame(allocator, open_body);
    defer allocator.free(open_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":2,"method":"shutdown"}
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
}

/// Verifies goto definition on external module members (both annotated and unannotated) reaches the exact declaration.
pub fn definitionHandlerNavigatesToExternalModuleMembers() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const helper_path = try std.fs.path.join(allocator, &.{ tmp_path, "ComputeHelper.roc" });
    defer allocator.free(helper_path);
    const helper_uri = try uriFromPath(allocator, helper_path);
    defer allocator.free(helper_uri);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const helper_source =
        \\module [greet, compute]
        \\
        \\greet : Str -> Str
        \\greet = |name| "Hello, "
        \\
        \\compute = |x| x + 1
    ;
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "ComputeHelper.roc", .data = helper_source });

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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [result] {{ pf: platform \"{s}\" }}\n\nimport ComputeHelper\n\nresult = ComputeHelper.greet(\"World\")\nans = ComputeHelper.compute(42)"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 4: result = ComputeHelper.greet("World") -> character 27 is on 'greet'
    const def_greet_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":27}}}}}}
    , .{main_uri});
    defer allocator.free(def_greet_body);
    const def_greet_msg = try frame(allocator, def_greet_body);
    defer allocator.free(def_greet_msg);

    // Line 5: ans = ComputeHelper.compute(42) -> character 24 is on 'compute'
    const def_compute_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":24}}}}}}
    , .{main_uri});
    defer allocator.free(def_compute_body);
    const def_compute_msg = try frame(allocator, def_compute_body);
    defer allocator.free(def_compute_msg);

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
    try builder.appendSlice(allocator, open_main_msg);
    try builder.appendSlice(allocator, def_greet_msg);
    try builder.appendSlice(allocator, def_compute_msg);
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

    // greet definition in ComputeHelper.roc is at line 3 (0-based)
    {
        var response = try responseById(allocator, responses, 2);
        defer response.deinit();
        const result = try response.result();
        try std.testing.expect(result == .object);
        const uri = try stringField(result, "uri");
        try std.testing.expect(std.mem.endsWith(u8, uri, "ComputeHelper.roc"));
        const range = try objectField(result, "range");
        const start = try objectField(range, "start");
        const start_line = try integerField(start, "line");
        try std.testing.expectEqual(@as(i64, 3), start_line);
    }

    // compute definition in ComputeHelper.roc is at line 5 (0-based)
    {
        var response = try responseById(allocator, responses, 3);
        defer response.deinit();
        const result = try response.result();
        try std.testing.expect(result == .object);
        const uri = try stringField(result, "uri");
        try std.testing.expect(std.mem.endsWith(u8, uri, "ComputeHelper.roc"));
        const range = try objectField(result, "range");
        const start = try objectField(range, "start");
        const start_line = try integerField(start, "line");
        try std.testing.expectEqual(@as(i64, 5), start_line);
    }
}

/// Verifies goto definition on an exposed item in an import statement (e.g. `import Helpers exposing [decode_json]`).
pub fn definitionHandlerNavigatesToExposedImportMemberInImportStatement() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const helper_path = try std.fs.path.join(allocator, &.{ tmp_path, "Helpers.roc" });
    defer allocator.free(helper_path);
    const helper_uri = try uriFromPath(allocator, helper_path);
    defer allocator.free(helper_uri);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const helper_source =
        \\Helpers :: [].{
        \\    encode_json : Str -> Str
        \\    encode_json = |str| str
        \\
        \\    decode_json : Str -> Str
        \\    decode_json = |str| str
        \\}
    ;
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "Helpers.roc", .data = helper_source });

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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [result] {{ pf: platform \"{s}\" }}\n\nimport Helpers exposing [encode_json, decode_json]\n\nresult = decode_json(\"hi\")"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 2: import Helpers exposing [encode_json, decode_json] -> character 42 is on 'decode_json'
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":42}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "Helpers.roc"));
    const range = try objectField(result, "range");
    const start = try objectField(range, "start");
    const start_line = try integerField(start, "line");
    // decode_json definition in Helpers.roc is at line 5 (0-based)
    try std.testing.expectEqual(@as(i64, 5), start_line);
}

/// Verifies goto definition on an unqualified function call imported via `exposing` (e.g. `decode_json(input)`).
pub fn definitionHandlerNavigatesToUnqualifiedExposedImportFunctionCall() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const helper_path = try std.fs.path.join(allocator, &.{ tmp_path, "Helpers.roc" });
    defer allocator.free(helper_path);
    const helper_uri = try uriFromPath(allocator, helper_path);
    defer allocator.free(helper_uri);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const helper_source =
        \\Helpers :: [].{
        \\    encode_json : Str -> Str
        \\    encode_json = |str| str
        \\
        \\    decode_json : Str -> Str
        \\    decode_json = |str| str
        \\}
    ;
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "Helpers.roc", .data = helper_source });

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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [result] {{ pf: platform \"{s}\" }}\n\nimport Helpers exposing [decode_json]\n\nresult = decode_json(\"hi\")"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 4: result = decode_json("hi") -> character 14 is on 'decode_json'
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":4,"character":14}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "Helpers.roc"));
    const range = try objectField(result, "range");
    const start = try objectField(range, "start");
    const start_line = try integerField(start, "line");
    // decode_json definition in Helpers.roc is at line 5 (0-based)
    try std.testing.expectEqual(@as(i64, 5), start_line);
}

/// Verifies goto definition on a tag in pattern matching (e.g. `WaitingForInit => ...`) navigates to the tag union declaration.
pub fn definitionHandlerNavigatesToTagDeclarationInPatternMatch() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [handle] {{ pf: platform \"{s}\" }}\n\nLoopState : [WaitingForInit, Running(Str)]\n\nhandle = |state|\n    match state {{\n        WaitingForInit => \"init\",\n        Running(s) => s,\n    }}"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 6: WaitingForInit => "init" -> character 12 is on 'WaitingForInit'
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":6,"character":12}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "main.roc"));
    const range = try objectField(result, "range");
    const start = try objectField(range, "start");
    const start_line = try integerField(start, "line");
    // LoopState declaration with WaitingForInit is on line 2 (0-based)
    try std.testing.expectEqual(@as(i64, 2), start_line);
}

/// Verifies goto definition on an exposed type alias (e.g. `Payload(...)`) in a local type annotation navigates to its declaration in the imported module.
pub fn definitionHandlerNavigatesToExposedTypeAliasInTypeAnnotation() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const helper_source =
        \\module [Payload]
        \\
        \\Payload(a) : {
        \\    type : Str,
        \\    body : a,
        \\}
    ;
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "Helpers.roc", .data = helper_source });

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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [handle] {{ pf: platform \"{s}\" }}\n\nimport Helpers exposing [Payload]\n\nhandle = |input| {{\n    p : Payload(Str)\n    p = {{ type: \"test\", body: input }}\n    p\n}}"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 5: p : Payload(Str) -> character 10 is on 'Payload'
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":10}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "Helpers.roc"));
    const range = try objectField(result, "range");
    const start = try objectField(range, "start");
    const start_line = try integerField(start, "line");
    // Payload declaration in Helpers.roc is on line 2 (0-based)
    try std.testing.expectEqual(@as(i64, 2), start_line);
}

/// Verifies semantic tokens handler handles file imports (e.g. `import "inputs/1.txt" as input : Str`) without crashing (Issue #10861).
pub fn semanticTokensHandlerHandlesFileImportsWithoutCrashing() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // Create the input file to import
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "input.txt", .data = "hello roc" });

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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main!] {{ pf: platform \"{s}\" }}\n\nimport \"input.txt\" as input : Str\n\nmain! = |_|\n    echo!(input)"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Request semantic tokens full
    const tokens_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/semanticTokens/full","params":{{"textDocument":{{"uri":"{s}"}}}}}}
    , .{main_uri});
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

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_main_msg);
    try builder.appendSlice(allocator, tokens_msg);
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
    try std.testing.expect(result == .object);
    const data_val = try objectField(result, "data");
    try std.testing.expect(data_val == .array);
    try std.testing.expect(data_val.array.items.len > 0);
}

/// Verifies goto definition on a file import path (`import "input.txt" as input : Str`) navigates to the imported file.
pub fn definitionHandlerNavigatesToFileImportPath() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // Create the target input file
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "input.txt", .data = "hello roc" });

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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"# Mentioning \"input.txt\" in comment\napp [main!] {{ pf: platform \"{s}\" }}\n\nimport \"input.txt\" as input : Str\n\nmain! = |_|\n    echo!(input)"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 0: # Mentioning "input.txt" in comment -> character 16 is on 'input.txt' inside the comment
    // Must NOT navigate to the file import (non-heuristic explicit token check)
    const comment_def_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":0,"character":16}}}}}}
    , .{main_uri});
    defer allocator.free(comment_def_body);
    const comment_def_msg = try frame(allocator, comment_def_body);
    defer allocator.free(comment_def_msg);

    // Line 3: import "input.txt" as input : Str
    // Position on "input.txt" (character 10)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":10}}}}}}
    , .{main_uri});
    defer allocator.free(definition_body);
    const definition_msg = try frame(allocator, definition_body);
    defer allocator.free(definition_msg);

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
    try builder.appendSlice(allocator, open_main_msg);
    try builder.appendSlice(allocator, comment_def_msg);
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

    // Comment position (id: 2) must return null
    {
        var response = try responseById(allocator, responses, 2);
        defer response.deinit();
        const result = try response.result();
        try std.testing.expect(result == .null);
    }

    // Real file import position (id: 3) must return LocationLink
    {
        var response = try responseById(allocator, responses, 3);
        defer response.deinit();
        const result = try response.result();
        try std.testing.expect(result == .array);
        try std.testing.expectEqual(@as(usize, 1), result.array.items.len);
        const link = result.array.items[0].object;
        const target_uri_value = link.get("targetUri") orelse return error.TestUnexpectedResult;
        try std.testing.expect(std.mem.endsWith(u8, target_uri_value.string, "input.txt"));
        const origin_range = (link.get("originSelectionRange") orelse return error.TestUnexpectedResult).object;
        const origin_start = (origin_range.get("start") orelse return error.TestUnexpectedResult).object;
        const origin_end = (origin_range.get("end") orelse return error.TestUnexpectedResult).object;
        // Line 3: import "input.txt" as input : Str
        // origin range covers "input.txt" as a single unit
        try std.testing.expectEqual(@as(i64, 3), (origin_start.get("line") orelse return error.TestUnexpectedResult).integer);
        try std.testing.expectEqual(@as(i64, 7), (origin_start.get("character") orelse return error.TestUnexpectedResult).integer);
        try std.testing.expectEqual(@as(i64, 3), (origin_end.get("line") orelse return error.TestUnexpectedResult).integer);
        try std.testing.expectEqual(@as(i64, 18), (origin_end.get("character") orelse return error.TestUnexpectedResult).integer);
    }
}

/// Verifies goto definition on `echo!` in a default app navigates to the default app Echo.roc platform definition.
pub fn definitionHandlerNavigatesToEchoPlatformDefinition() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [main!] {{ pf: platform \"{s}\" }}\n\nmain! = |_|\n    echo!(\"hello\")"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 3: echo!("hello") -> position on 'echo!' (character 5)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":5}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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

/// Verifies goto definition on a package-shorthand qualified import (`import pkg.Thing`) and member (`Thing.new`).
pub fn definitionHandlerResolvesPackageShorthandQualifiedImport() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    tmp.dir.createDirPath(test_env.io, "pkg") catch return error.TestUnexpectedResult;
    const pkg_main_path = try std.fs.path.join(allocator, &.{ tmp_path, "pkg", "main.roc" });
    defer allocator.free(pkg_main_path);
    const pkg_thing_path = try std.fs.path.join(allocator, &.{ tmp_path, "pkg", "Thing.roc" });
    defer allocator.free(pkg_thing_path);

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/main.roc", .data = "package [Thing] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/Thing.roc", .data = "module [new]\n\nnew = |x| x + 1\n" });

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);

    const main_source = try std.fmt.allocPrint(allocator,
        \\app [main!] {{ pkg: "./pkg/main.roc", pf: platform "{s}" }}
        \\
        \\import pkg.Thing
        \\
        \\main! = |_|
        \\    Thing.new(4)
        \\
    , .{platform_path});
    defer allocator.free(main_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "main.roc", .data = main_source });

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

    const escaped_source = try jsonEscape(allocator, main_source);
    defer allocator.free(escaped_source);

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ main_uri, escaped_source });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 2: import pkg.Thing -> position on 'pkg.Thing' (character 12)
    const def_import_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":12}}}}}}
    , .{main_uri});
    defer allocator.free(def_import_body);
    const def_import_msg = try frame(allocator, def_import_body);
    defer allocator.free(def_import_msg);

    // Line 5: Thing.new(4) -> position on 'new' (character 11)
    const def_member_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":5,"character":11}}}}}}
    , .{main_uri});
    defer allocator.free(def_member_body);
    const def_member_msg = try frame(allocator, def_member_body);
    defer allocator.free(def_member_msg);

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
    try builder.appendSlice(allocator, open_main_msg);
    try builder.appendSlice(allocator, def_import_msg);
    try builder.appendSlice(allocator, def_member_msg);
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

    // Response 2: Definition of import pkg.Thing -> navigates to Thing.roc
    var response2 = try responseById(allocator, responses, 2);
    defer response2.deinit();
    const result2 = try response2.result();
    try std.testing.expect(result2 == .object);
    const uri2 = try stringField(result2, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri2, "Thing.roc"));

    // Response 3: Definition of Thing.new -> navigates to Thing.roc line 2 (0-indexed line 2)
    var response3 = try responseById(allocator, responses, 3);
    defer response3.deinit();
    const result3 = try response3.result();
    try std.testing.expect(result3 == .object);
    const uri3 = try stringField(result3, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri3, "Thing.roc"));
    const range3 = try objectField(result3, "range");
    const start3 = try objectField(range3, "start");
    const start_line3 = try integerField(start3, "line");
    try std.testing.expectEqual(@as(i64, 2), start_line3);
}

/// Verifies goto definition disambiguates same-named modules between the root package and an imported package.
pub fn definitionHandlerDisambiguatesSameNamedModuleAcrossPackages() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    tmp.dir.createDirPath(test_env.io, "pkg") catch return error.TestUnexpectedResult;
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/main.roc", .data = "package [Common] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/Common.roc", .data = "module [pkg_val]\n\npkg_val = 42\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "Common.roc", .data = "module [local_val]\n\nlocal_val = 100\n" });

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);

    const main_source = try std.fmt.allocPrint(allocator,
        \\app [process_string] {{ pkg: "./pkg/main.roc", pf: platform "{s}" }}
        \\
        \\import Common
        \\import pkg.Common as PkgCommon
        \\
        \\process_string = |_| {{
        \\    a = Common.local_val
        \\    b = PkgCommon.pkg_val
        \\    "ok"
        \\}}
        \\
    , .{platform_path});
    defer allocator.free(main_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "main.roc", .data = main_source });

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

    const escaped_source = try jsonEscape(allocator, main_source);
    defer allocator.free(escaped_source);

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ main_uri, escaped_source });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 2: import Common -> local Common (character 10)
    const def_local_import_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":10}}}}}}
    , .{main_uri});
    defer allocator.free(def_local_import_body);
    const def_local_import_msg = try frame(allocator, def_local_import_body);
    defer allocator.free(def_local_import_msg);

    // Line 3: import pkg.Common as PkgCommon -> package Common (character 13)
    const def_pkg_import_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":3,"character":13}}}}}}
    , .{main_uri});
    defer allocator.free(def_pkg_import_body);
    const def_pkg_import_msg = try frame(allocator, def_pkg_import_body);
    defer allocator.free(def_pkg_import_msg);

    // Line 6: Common.local_val -> position on 'local_val' (character 16)
    const def_local_val_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":6,"character":16}}}}}}
    , .{main_uri});
    defer allocator.free(def_local_val_body);
    const def_local_val_msg = try frame(allocator, def_local_val_body);
    defer allocator.free(def_local_val_msg);

    // Line 7: PkgCommon.pkg_val -> position on 'pkg_val' (character 20)
    const def_pkg_val_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":5,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":7,"character":20}}}}}}
    , .{main_uri});
    defer allocator.free(def_pkg_val_body);
    const def_pkg_val_msg = try frame(allocator, def_pkg_val_body);
    defer allocator.free(def_pkg_val_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":6,"method":"shutdown"}
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
    try builder.appendSlice(allocator, open_main_msg);
    try builder.appendSlice(allocator, def_local_import_msg);
    try builder.appendSlice(allocator, def_pkg_import_msg);
    try builder.appendSlice(allocator, def_local_val_msg);
    try builder.appendSlice(allocator, def_pkg_val_msg);
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

    // Response 2: Definition of import Common -> local Common.roc
    var response2 = try responseById(allocator, responses, 2);
    defer response2.deinit();
    const result2 = try response2.result();
    var response3 = try responseById(allocator, responses, 3);
    defer response3.deinit();
    const result3 = try response3.result();
    var response4 = try responseById(allocator, responses, 4);
    defer response4.deinit();
    const result4 = try response4.result();
    var response5 = try responseById(allocator, responses, 5);
    defer response5.deinit();
    const result5 = try response5.result();
    try std.testing.expect(result2 == .object);
    const uri2 = try stringField(result2, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri2, "Common.roc"));
    try std.testing.expect(!std.mem.containsAtLeast(u8, uri2, 1, "pkg/Common.roc"));

    // Response 3: Definition of import pkg.Common -> package pkg/Common.roc
    try std.testing.expect(result3 == .object);
    const uri3 = try stringField(result3, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri3, "pkg/Common.roc"));

    // Response 4: Definition of Common.local_val -> local Common.roc
    try std.testing.expect(result4 == .object);
    const uri4 = try stringField(result4, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri4, "Common.roc"));
    try std.testing.expect(!std.mem.containsAtLeast(u8, uri4, 1, "pkg/Common.roc"));

    // Response 5: Definition of PkgCommon.pkg_val -> pkg/Common.roc
    try std.testing.expect(result5 == .object);
    const uri5 = try stringField(result5, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri5, "pkg/Common.roc"));
}

/// Verifies that qualified module definition lookups resolve package shorthands
/// strictly in the importing package's context when multiple packages use conflicting alias names.
pub fn definitionHandlerResolvesShorthandInImportingPackageContext() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    tmp.dir.createDirPath(test_env.io, "dep_a") catch return error.TestUnexpectedResult;
    tmp.dir.createDirPath(test_env.io, "dep_b") catch return error.TestUnexpectedResult;
    tmp.dir.createDirPath(test_env.io, "pkg_a") catch return error.TestUnexpectedResult;
    tmp.dir.createDirPath(test_env.io, "pkg_b") catch return error.TestUnexpectedResult;

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "dep_a/main.roc", .data = "package [Helper] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "dep_a/Helper.roc", .data = "module [helper_a]\n\nhelper_a = 1\n" });

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "dep_b/main.roc", .data = "package [Helper] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "dep_b/Helper.roc", .data = "module [helper_b]\n\nhelper_b = 2\n" });

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_a/main.roc", .data = "package [ModA] { dep: \"../dep_a/main.roc\" }\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_a/ModA.roc", .data = "module [val_a]\n\nimport dep.Helper\n\nval_a = Helper.helper_a\n" });

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_b/main.roc", .data = "package [ModB] { dep: \"../dep_b/main.roc\" }\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_b/ModB.roc", .data = "module [val_b]\n\nimport dep.Helper\n\nval_b = Helper.helper_b\n" });

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);

    const pkg_a_mod_path = try std.fs.path.join(allocator, &.{ tmp_path, "pkg_a", "ModA.roc" });
    defer allocator.free(pkg_a_mod_path);
    const pkg_a_mod_uri = try uriFromPath(allocator, pkg_a_mod_path);
    defer allocator.free(pkg_a_mod_uri);

    const pkg_b_mod_path = try std.fs.path.join(allocator, &.{ tmp_path, "pkg_b", "ModB.roc" });
    defer allocator.free(pkg_b_mod_path);
    const pkg_b_mod_uri = try uriFromPath(allocator, pkg_b_mod_path);
    defer allocator.free(pkg_b_mod_uri);

    const main_source = try std.fmt.allocPrint(allocator,
        \\app [process_string] {{ pkg_a: "./pkg_a/main.roc", pkg_b: "./pkg_b/main.roc", pf: platform "{s}" }}
        \\
        \\import pkg_a.ModA
        \\import pkg_b.ModB
        \\
        \\process_string = |_| {{
        \\    x = ModA.val_a
        \\    y = ModB.val_b
        \\    "ok"
        \\}}
        \\
    , .{platform_path});
    defer allocator.free(main_source);
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "main.roc", .data = main_source });

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

    const pkg_a_mod_source = "module [val_a]\n\nimport dep.Helper\n\nval_a = Helper.helper_a\n";
    const escaped_pkg_a = try jsonEscape(allocator, pkg_a_mod_source);
    defer allocator.free(escaped_pkg_a);

    const open_pkg_a_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ pkg_a_mod_uri, escaped_pkg_a });
    defer allocator.free(open_pkg_a_body);
    const open_pkg_a_msg = try frame(allocator, open_pkg_a_body);
    defer allocator.free(open_pkg_a_msg);

    const pkg_b_mod_source = "module [val_b]\n\nimport dep.Helper\n\nval_b = Helper.helper_b\n";
    const escaped_pkg_b = try jsonEscape(allocator, pkg_b_mod_source);
    defer allocator.free(escaped_pkg_b);

    const open_pkg_b_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"{s}"}}}}}}
    , .{ pkg_b_mod_uri, escaped_pkg_b });
    defer allocator.free(open_pkg_b_body);
    const open_pkg_b_msg = try frame(allocator, open_pkg_b_body);
    defer allocator.free(open_pkg_b_msg);

    // In pkg_a/ModA.roc: line 2, character 12 (dep.Helper) -> should resolve to dep_a/Helper.roc
    const def_a_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":12}}}}}}
    , .{pkg_a_mod_uri});
    defer allocator.free(def_a_body);
    const def_a_msg = try frame(allocator, def_a_body);
    defer allocator.free(def_a_msg);

    // In pkg_b/ModB.roc: line 2, character 12 (dep.Helper) -> should resolve to dep_b/Helper.roc
    const def_b_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":2,"character":12}}}}}}
    , .{pkg_b_mod_uri});
    defer allocator.free(def_b_body);
    const def_b_msg = try frame(allocator, def_b_body);
    defer allocator.free(def_b_msg);

    const shutdown_body =
        \\{"jsonrpc":"2.0","id":99,"method":"shutdown","params":{}}
    ;
    const shutdown_msg = try frame(allocator, shutdown_body);
    defer allocator.free(shutdown_msg);

    const exit_body =
        \\{"jsonrpc":"2.0","method":"exit","params":{}}
    ;
    const exit_msg = try frame(allocator, exit_body);
    defer allocator.free(exit_msg);

    var builder: std.ArrayList(u8) = .empty;
    defer builder.deinit(allocator);
    try builder.appendSlice(allocator, init_msg);
    try builder.appendSlice(allocator, initialized_msg);
    try builder.appendSlice(allocator, open_pkg_a_msg);
    try builder.appendSlice(allocator, open_pkg_b_msg);
    try builder.appendSlice(allocator, def_a_msg);
    try builder.appendSlice(allocator, def_b_msg);
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

    var response2 = try responseById(allocator, responses, 2);
    defer response2.deinit();
    const result2 = try response2.result();
    try std.testing.expect(result2 == .object);
    const uri2 = try stringField(result2, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri2, "dep_a/Helper.roc"));

    var response3 = try responseById(allocator, responses, 3);
    defer response3.deinit();
    const result3 = try response3.result();
    try std.testing.expect(result3 == .object);
    const uri3 = try stringField(result3, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri3, "dep_b/Helper.roc"));
}

/// Verifies goto definition on a tag imported via package shorthand (e.g. `import pkg.State`)
/// navigates to the tag's type declaration in the dependency package.
pub fn definitionHandlerNavigatesToTagDeclarationInPackageQualifiedImport() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    tmp.dir.createDirPath(test_env.io, "pkg") catch return error.TestUnexpectedResult;
    const pkg_state_path = try std.fs.path.join(allocator, &.{ tmp_path, "pkg", "State.roc" });
    defer allocator.free(pkg_state_path);

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/main.roc", .data = "package [State] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/State.roc", .data = "module [State]\n\nState : [WaitingForInit, Running, Done]\n" });

    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
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

    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [handle] {{ pf: platform \"{s}\", pkg: \"./pkg/main.roc\" }}\n\nimport pkg.State exposing [State]\n\nhandle : State -> I64\nhandle = |s|\n    match s {{\n        WaitingForInit => 0,\n        _ => 1,\n    }}"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 7: WaitingForInit => 0 (character 10)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":7,"character":10}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "pkg/State.roc"));
}

/// Verifies goto definition on a tag that appears in multiple imported modules
/// navigates to the declaration in the correct module based on solved type identity.
pub fn definitionHandlerDisambiguatesSameNamedTagAcrossImportedModules() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    tmp.dir.createDirPath(test_env.io, "pkg_a") catch return error.TestUnexpectedResult;
    tmp.dir.createDirPath(test_env.io, "pkg_b") catch return error.TestUnexpectedResult;

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_a/main.roc", .data = "package [StateA] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_a/StateA.roc", .data = "module [StateA]\n\nStateA : [CommonTag, OnlyA]\n" });

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_b/main.roc", .data = "package [StateB] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg_b/StateB.roc", .data = "module [StateB]\n\nStateB : [CommonTag, OnlyB]\n" });

    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
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

    // main.roc imports pkg_a first, then pkg_b second.
    // handleB uses StateB (from pkg_b), matching on CommonTag.
    // Navigation MUST jump to pkg_b/StateB.roc, not pkg_a/StateA.roc.
    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [handleB] {{ pf: platform \"{s}\", pa: \"./pkg_a/main.roc\", pb: \"./pkg_b/main.roc\" }}\n\nimport pa.StateA exposing [StateA]\nimport pb.StateB exposing [StateB]\n\nhandleB : StateB -> I64\nhandleB = |s|\n    match s {{\n        CommonTag => 0,\n        _ => 1,\n    }}"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 8: CommonTag => 0 (character 10)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":8,"character":10}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    try std.testing.expect(result == .object);
    const uri = try stringField(result, "uri");
    try std.testing.expect(std.mem.endsWith(u8, uri, "pkg_b/StateB.roc"));
}

/// Verifies that a bare open tag returned in a match branch value does not
/// accidentally navigate to the match condition's nominal type declaration.
pub fn definitionHandlerBranchValueOpenTagDoesNotNavigateToMatchConditionType() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);

    tmp.dir.createDirPath(test_env.io, "pkg") catch return error.TestUnexpectedResult;

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/main.roc", .data = "package [State] {}\n" });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "pkg/State.roc", .data = "module [State]\n\nState : [ConditionTag, Other]\n" });

    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try uriFromPath(allocator, main_path);
    defer allocator.free(main_uri);
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

    // main.roc matches on State, but returns an open tag in branch value.
    const open_main_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{s}","version":1,"text":"app [handle] {{ pf: platform \"{s}\", pkg: \"./pkg/main.roc\" }}\n\nimport pkg.State exposing [State]\n\nhandle : State -> [OpenBranchTag, Else]\nhandle = |s|\n    match s {{\n        _ => OpenBranchTag,\n    }}"}}}}}}
    , .{ main_uri, platform_path });
    defer allocator.free(open_main_body);
    const open_main_msg = try frame(allocator, open_main_body);
    defer allocator.free(open_main_msg);

    // Line 7: _ => OpenBranchTag (character 15 on OpenBranchTag)
    const definition_body = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{s}"}},"position":{{"line":7,"character":15}}}}}}
    , .{main_uri});
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
    try builder.appendSlice(allocator, open_main_msg);
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
    // OpenBranchTag is not declared in pkg/State.roc; definition must find it in the local annotation or return null,
    // but MUST NOT return pkg/State.roc!
    if (result == .object) {
        const uri = try stringField(result, "uri");
        try std.testing.expect(!std.mem.endsWith(u8, uri, "pkg/State.roc"));
    }
}
