//! Self-contained WebAssembly REPL with a versioned JSON request protocol.

const std = @import("std");
const builtin = @import("builtin");
const eval = @import("eval");
const reporting = @import("reporting");
const CoreCtx = @import("ctx").CoreCtx;
const ReplSession = @import("ReplSession.zig");

const Allocator = std.mem.Allocator;
const protocol_version: u32 = 1;
var allocator: Allocator = std.heap.wasm_allocator;
var repl_session: ?ReplSession = null;
var session_revision: u32 = 0;

/// Silence logging in the freestanding wasm build; the host only sees the
/// JSON protocol on stdout-equivalent buffers, never Zig log output.
pub const std_options: std.Options = .{
    .log_level = .warn,
    .logFn = if (builtin.target.os.tag == .freestanding) quietLog else std.log.defaultLog,
};

fn quietLog(comptime _: std.log.Level, comptime _: @TypeOf(.enum_literal), comptime _: []const u8, _: anytype) void {}

const ModuleInput = struct {
    name: []const u8,
    source: []const u8,
};

const Params = struct {
    source: ?[]const u8 = null,
    cursor: ?u32 = null,
    modules: ?[]const ModuleInput = null,
};

const Request = struct {
    protocol: u32,
    id: std.json.Value,
    op: []const u8,
    params: ?Params = null,
};

const Diagnostic = struct {
    code: []const u8,
    severity: []const u8 = "error",
    message: []const u8,
    region: ?Region = null,
};

const Region = struct {
    start: u32,
    end: u32,
};

const Event = union(enum) {
    runtime: struct { kind: []const u8, message: []const u8 },
    effect: struct { name: []const u8, payload: []const u8 },

    pub fn jsonStringify(self: Event, json: anytype) !void {
        try json.beginObject();
        switch (self) {
            .runtime => |event| {
                try json.objectField("kind");
                try json.write(event.kind);
                try json.objectField("message");
                try json.write(event.message);
            },
            .effect => |event| {
                try json.objectField("kind");
                try json.write("effect");
                try json.objectField("name");
                try json.write(event.name);
                try json.objectField("payload");
                try json.write(event.payload);
            },
        }
        try json.endObject();
    }
};

const Crash = struct {
    message: []const u8,
};

const SnippetResult = struct {
    source: []const u8,
    kind: ?[]const u8,
    definition_kind: ?[]const u8 = null,
    name: ?[]const u8 = null,
    status: []const u8,
    committed: bool,
    revision: u32,
    value: ?[]const u8 = null,
    type: ?[]const u8 = null,
    crash: ?Crash = null,
    diagnostics: []const Diagnostic = &.{},
    events: []const Event = &.{},
};

const Completion = struct {
    label: []const u8,
    insert_text: []const u8,
    kind: []const u8,
    detail: ?[]const u8,
};

const StateDefinition = struct {
    name: []const u8,
    source: []const u8,
    kind: []const u8,
};

const StateModule = struct {
    name: []const u8,
    source: []const u8,
};

const SnippetMetadata = struct {
    kind: ?[]const u8 = null,
    definition_kind: ?[]const u8 = null,
    name: ?[]const u8 = null,
};

fn ensureSession() !*ReplSession {
    if (repl_session == null) {
        const roc_ctx = CoreCtx.default(allocator, allocator, @as(std.Io, undefined));
        repl_session = try ReplSession.initVirtual(allocator, roc_ctx, .interpreter, .lss);
    }
    return &repl_session.?;
}

fn reportingConfig() reporting.ReportingConfig {
    return reporting.ReportingConfig.initForTesting();
}

fn jsonResponse(value: anytype) Allocator.Error![]u8 {
    var writer: std.Io.Writer.Allocating = .init(allocator);
    errdefer writer.deinit();
    std.json.Stringify.value(value, .{}, &writer.writer) catch return error.OutOfMemory;
    writer.writer.flush() catch return error.OutOfMemory;
    return writer.toOwnedSlice();
}

fn okResponse(id: std.json.Value, result: anytype) Allocator.Error![]u8 {
    return jsonResponse(.{
        .protocol = protocol_version,
        .id = id,
        .ok = true,
        .result = result,
    });
}

fn errorResponse(id: std.json.Value, code: []const u8, message: []const u8) Allocator.Error![]u8 {
    return jsonResponse(.{
        .protocol = protocol_version,
        .id = id,
        .ok = false,
        .@"error" = .{ .code = code, .message = message },
    });
}

fn requiredSource(request: Request) ?[]const u8 {
    return if (request.params) |params| params.source else null;
}

fn arenaDupe(arena: Allocator, bytes: []const u8) Allocator.Error![]const u8 {
    return try arena.dupe(u8, bytes);
}

fn copyEvents(arena: Allocator, session: *ReplSession) Allocator.Error![]const Event {
    const host_events = session.takeEvents();
    defer {
        for (host_events) |*event| event.deinit(allocator);
        allocator.free(host_events);
    }
    const events = try arena.alloc(Event, host_events.len);
    for (host_events, 0..) |event, index| {
        events[index] = switch (event) {
            .dbg => |message| .{ .runtime = .{ .kind = "dbg", .message = try arenaDupe(arena, message) } },
            .expect_failed => |message| .{ .runtime = .{ .kind = "expect_failed", .message = try arenaDupe(arena, message) } },
            .crashed => |message| .{ .runtime = .{ .kind = "crashed", .message = try arenaDupe(arena, message) } },
            .effect => |effect| .{ .effect = .{
                .name = try arenaDupe(arena, effect.name),
                .payload = try arenaDupe(arena, effect.payload),
            } },
        };
    }
    return events;
}

fn definitionKindName(kind: ReplSession.DefinitionKind, file_import: bool) []const u8 {
    return switch (kind) {
        .value => "value",
        .annotation => "annotation",
        .type_decl => "type",
        .import => if (file_import) "file_import" else "import",
    };
}

fn inputMetadata(info: ReplSession.InputInfo) SnippetMetadata {
    return switch (info.kind) {
        .expression => .{ .kind = "expression" },
        .definition => .{
            .kind = "definition",
            .definition_kind = definitionKindName(info.definition_kind, info.file_import),
            .name = info.name,
        },
    };
}

fn advanceRevision() error{RevisionExhausted}!u32 {
    if (session_revision == std.math.maxInt(u32)) return error.RevisionExhausted;
    session_revision += 1;
    return session_revision;
}

fn appendDiagnostic(
    arena: Allocator,
    results: *std.ArrayList(SnippetResult),
    source: []const u8,
    metadata: SnippetMetadata,
    code: []const u8,
    message: []const u8,
) Allocator.Error!void {
    const owned_message = try arenaDupe(arena, message);
    const diagnostics = try arena.alloc(Diagnostic, 1);
    diagnostics[0] = .{ .code = code, .message = owned_message };
    try results.append(arena, .{
        .source = try arenaDupe(arena, source),
        .kind = metadata.kind,
        .definition_kind = metadata.definition_kind,
        .name = if (metadata.name) |name| try arenaDupe(arena, name) else null,
        .status = "diagnostic",
        .committed = false,
        .revision = session_revision,
        .diagnostics = diagnostics,
    });
}

fn definitionType(arena: Allocator, session: *ReplSession, name: []const u8) ![]const u8 {
    const items = try session.completionItems();
    defer session.freeCompletionItems(items);
    for (items) |item| {
        if (item.kind != .value or !std.mem.eql(u8, item.label, name)) continue;
        const detail = item.detail orelse return error.DefinitionTypeUnavailable;
        return arenaDupe(arena, detail);
    }
    return error.DefinitionTypeUnavailable;
}

fn expressionType(arena: Allocator, session: *ReplSession, source: []const u8) ![]const u8 {
    const inspected = try session.inspectExpressionType(source, reportingConfig());
    defer inspected.deinit(allocator);
    return switch (inspected) {
        .output => |type_name| arenaDupe(arena, type_name),
        .diagnostic, .runtime_crash, .none, .exit => error.ExpressionTypeUnavailable,
    };
}

fn parseDiagnosticMessage(arena: Allocator, session: *ReplSession, source: []const u8) ![]const u8 {
    const step_result = try session.stepLanguageWithConfig(source, reportingConfig());
    defer step_result.deinit(allocator);
    return switch (step_result) {
        .diagnostic => |diagnostic| arenaDupe(arena, diagnostic.message),
        .expression, .definition, .runtime_crash, .none => error.ParseDiagnosticUnavailable,
    };
}

fn evaluate(request: Request, arena: Allocator) ![]u8 {
    const source = requiredSource(request) orelse return error.MissingSource;
    const session = try ensureSession();
    const statements = try session.splitInputIntoStatements(source);
    defer session.freeStatementSlices(statements);

    var results = std.ArrayList(SnippetResult).empty;
    defer results.deinit(arena);

    var completed = true;
    var stop_reason: ?[]const u8 = null;
    var committed_count: usize = 0;

    for (statements) |statement| {
        const step_result = try session.stepLanguageWithConfig(statement, reportingConfig());
        defer step_result.deinit(allocator);
        switch (step_result) {
            .expression => |output| try results.append(arena, .{
                .source = try arenaDupe(arena, statement),
                .kind = "expression",
                .status = "ok",
                .committed = false,
                .revision = session_revision,
                .value = try arenaDupe(arena, output),
                .type = try expressionType(arena, session, statement),
                .events = try copyEvents(arena, session),
            }),
            .definition => |definition| {
                const revision = try advanceRevision();
                committed_count += 1;
                const type_name = if (definition.kind == .value)
                    try definitionType(arena, session, definition.name)
                else
                    null;
                try results.append(arena, .{
                    .source = try arenaDupe(arena, statement),
                    .kind = "definition",
                    .definition_kind = definitionKindName(definition.kind, definition.file_import),
                    .name = try arenaDupe(arena, definition.name),
                    .status = "ok",
                    .committed = true,
                    .revision = revision,
                    .type = type_name,
                    .events = try copyEvents(arena, session),
                });
            },
            .diagnostic => |diagnostic| {
                const metadata = if (diagnostic.input) |info| inputMetadata(info) else SnippetMetadata{};
                const code = switch (diagnostic.kind) {
                    .incomplete_input => "incomplete_input",
                    .parse_error => "parse_error",
                    .compile_error => "compile_error",
                    .unsupported_file_import => "unsupported_file_import",
                };
                try appendDiagnostic(arena, &results, statement, metadata, code, diagnostic.message);
                completed = false;
                stop_reason = "diagnostic";
                break;
            },
            .runtime_crash => |message| {
                try results.append(arena, .{
                    .source = try arenaDupe(arena, statement),
                    .kind = "expression",
                    .status = "crashed",
                    .committed = false,
                    .revision = session_revision,
                    .crash = .{ .message = try arenaDupe(arena, message) },
                    .events = try copyEvents(arena, session),
                });
                completed = false;
                stop_reason = "crash";
                break;
            },
            .none => return error.UnexpectedEmptyResult,
        }
    }

    return okResponse(request.id, .{
        .snippets = results.items,
        .completed = completed,
        .stop_reason = stop_reason,
        .committed_count = committed_count,
        .revision = session_revision,
    });
}

fn analyze(request: Request, arena: Allocator) ![]u8 {
    const source = requiredSource(request) orelse return error.MissingSource;
    const session = try ensureSession();

    return switch (try session.inputStatus(source)) {
        .complete => |info| blk: {
            const metadata = inputMetadata(info);
            break :blk okResponse(request.id, .{
                .status = "complete",
                .kind = metadata.kind,
                .definition_kind = metadata.definition_kind,
                .name = metadata.name,
                .diagnostics = @as([]const Diagnostic, &.{}),
                .revision = session_revision,
            });
        },
        .incomplete => okResponse(request.id, .{
            .status = "incomplete",
            .kind = @as(?[]const u8, null),
            .definition_kind = @as(?[]const u8, null),
            .name = @as(?[]const u8, null),
            .diagnostics = @as([]const Diagnostic, &.{}),
            .revision = session_revision,
        }),
        .invalid => blk: {
            const diagnostics = try arena.alloc(Diagnostic, 1);
            diagnostics[0] = .{
                .code = "parse_error",
                .message = try parseDiagnosticMessage(arena, session, source),
            };
            break :blk okResponse(request.id, .{
                .status = "invalid",
                .kind = @as(?[]const u8, null),
                .definition_kind = @as(?[]const u8, null),
                .name = @as(?[]const u8, null),
                .diagnostics = diagnostics,
                .revision = session_revision,
            });
        },
    };
}

fn inspectDiagnosticResponse(
    request: Request,
    arena: Allocator,
    source: []const u8,
    code: []const u8,
    message: []const u8,
) ![]u8 {
    const diagnostics = try arena.alloc(Diagnostic, 1);
    diagnostics[0] = .{ .code = code, .message = try arenaDupe(arena, message) };
    return okResponse(request.id, .{
        .status = "diagnostic",
        .source = source,
        .type = @as(?[]const u8, null),
        .diagnostics = diagnostics,
        .revision = session_revision,
    });
}

fn inspect(request: Request, arena: Allocator) ![]u8 {
    const source = requiredSource(request) orelse return error.MissingSource;
    const session = try ensureSession();
    switch (try session.inputStatus(source)) {
        .incomplete => return inspectDiagnosticResponse(
            request,
            arena,
            source,
            "incomplete_input",
            "The expression is incomplete.",
        ),
        .invalid => return inspectDiagnosticResponse(
            request,
            arena,
            source,
            "parse_error",
            try parseDiagnosticMessage(arena, session, source),
        ),
        .complete => |info| if (info.kind != .expression) return inspectDiagnosticResponse(
            request,
            arena,
            source,
            "expected_expression",
            "inspect accepts an expression, not a definition.",
        ),
    }

    const inspected = try session.inspectExpressionType(source, reportingConfig());
    defer inspected.deinit(allocator);
    return switch (inspected) {
        .output => |type_name| okResponse(request.id, .{
            .status = "ok",
            .source = source,
            .type = type_name,
            .diagnostics = @as([]const Diagnostic, &.{}),
            .revision = session_revision,
        }),
        .diagnostic => |message| inspectDiagnosticResponse(request, arena, source, "type_error", message),
        .runtime_crash, .none, .exit => errorResponse(request.id, "inspect_failed", "Expression inspection did not produce a type."),
    };
}

fn isIdentifierByte(byte: u8) bool {
    return (byte >= 'a' and byte <= 'z') or
        (byte >= 'A' and byte <= 'Z') or
        (byte >= '0' and byte <= '9') or
        byte == '_';
}

fn identifierPrefixStart(source: []const u8, cursor: usize) usize {
    var start = cursor;
    while (start > 0 and isIdentifierByte(source[start - 1])) start -= 1;
    return start;
}

fn complete(request: Request, arena: Allocator) ![]u8 {
    const source = requiredSource(request) orelse return error.MissingSource;
    const params = request.params orelse return error.MissingCursor;
    const cursor_u32 = params.cursor orelse return error.MissingCursor;
    const cursor: usize = @intCast(cursor_u32);
    if (cursor > source.len or !std.unicode.utf8ValidateSlice(source[0..cursor])) return error.InvalidCursor;

    const session = try ensureSession();
    const session_items = try session.completionItems();
    defer session.freeCompletionItems(session_items);

    const replacement_start = identifierPrefixStart(source, cursor);
    const prefix = source[replacement_start..cursor];
    var items = std.ArrayList(Completion).empty;
    defer items.deinit(arena);
    for (session_items) |item| {
        if (!std.mem.startsWith(u8, item.label, prefix)) continue;
        const label = try arenaDupe(arena, item.label);
        try items.append(arena, .{
            .label = label,
            .insert_text = label,
            .kind = definitionKindName(item.kind, false),
            .detail = if (item.detail) |detail| try arenaDupe(arena, detail) else null,
        });
    }
    return okResponse(request.id, .{
        .items = items.items,
        .is_incomplete = false,
        .details_available = session.completionDetailsAvailable(),
        .prefix = prefix,
        .replacement = Region{ .start = @intCast(replacement_start), .end = cursor_u32 },
        .cursor = cursor_u32,
        .offset_unit = "utf8_bytes",
        .revision = session_revision,
    });
}

fn getState(request: Request, arena: Allocator) ![]u8 {
    const session = try ensureSession();
    const definition_source = try session.definitionsSource();
    defer allocator.free(definition_source);
    const stored = try session.storedDefinitions();
    defer session.freeStoredDefinitions(stored);
    const definitions = try arena.alloc(StateDefinition, stored.len);
    for (stored, 0..) |definition, index| {
        definitions[index] = .{
            .name = try arenaDupe(arena, definition.name),
            .source = try arenaDupe(arena, definition.source),
            .kind = definitionKindName(definition.kind, definition.file_import),
        };
    }
    const stored_modules = try session.storedVirtualModules(allocator);
    defer session.freeStoredVirtualModules(allocator, stored_modules);
    const modules = try arena.alloc(StateModule, stored_modules.len);
    for (stored_modules, 0..) |module, index| {
        modules[index] = .{
            .name = try arenaDupe(arena, module.name),
            .source = try arenaDupe(arena, module.source),
        };
    }
    return okResponse(request.id, .{
        .revision = session_revision,
        .definitions = definitions,
        .definition_source = definition_source,
        .modules = modules,
        .has_pending_annotation = session.hasPendingAnnotation(),
    });
}

fn setModules(request: Request, arena: Allocator) ![]u8 {
    const params = request.params orelse return error.MissingModules;
    const inputs = params.modules orelse return error.MissingModules;
    const modules = try arena.alloc(eval.Inspected.ModuleSource, inputs.len);
    for (inputs, 0..) |module, index| {
        modules[index] = .{ .name = module.name, .source = module.source };
    }
    const session = try ensureSession();
    const cleared_definition_count = session.definitionCount();
    try session.replaceVirtualModules(modules);
    const revision = try advanceRevision();
    const module_names = try arena.alloc([]const u8, inputs.len);
    for (inputs, 0..) |module, index| module_names[index] = module.name;
    return okResponse(request.id, .{
        .module_count = modules.len,
        .module_names = module_names,
        .cleared_definition_count = cleared_definition_count,
        .revision = revision,
    });
}

fn capabilities(request: Request) Allocator.Error![]u8 {
    return okResponse(request.id, .{
        .session_model = "one_session_per_wasm_instance",
        .protocol_version = protocol_version,
        .operations = &.{ "capabilities", "eval", "analyze", "complete", "inspect", "get_state", "clear", "set_modules" },
        .text_encoding = "utf8",
        .offset_unit = "utf8_bytes",
        .revision_scope = "wasm_instance",
        .revision_bits = 32,
        .completion_scope = "session_definitions",
        .features = .{
            .stateful_definitions = true,
            .state_revision = true,
            .parser_backed_multiline = true,
            .batch_commit = "left_to_right_until_failure",
            .structured_diagnostics = true,
            .diagnostic_scope = "blocking_only",
            .diagnostic_regions = "optional",
            .ordered_runtime_events = true,
            .virtual_modules = true,
            .cli_commands = false,
            .presentation_strings = false,
            .filesystem = false,
            .network = false,
            .platform_effects = "one_way_events",
            .effect_module = eval.InspectedRun.repl_effect_module_name,
            .effect_function = "emit!",
            .effect_payload_encoding = "caller_defined_utf8",
            .effect_responses = false,
            .host_managed_history = true,
            .host_managed_cancellation = true,
        },
    });
}

fn handleRequest(request: Request, arena: Allocator) ![]u8 {
    if (request.protocol != protocol_version) {
        return errorResponse(request.id, "unsupported_protocol", "This module supports protocol version 1.");
    }
    if (std.mem.eql(u8, request.op, "capabilities")) return capabilities(request);
    if (std.mem.eql(u8, request.op, "eval")) return evaluate(request, arena);
    if (std.mem.eql(u8, request.op, "analyze")) return analyze(request, arena);
    if (std.mem.eql(u8, request.op, "complete")) return complete(request, arena);
    if (std.mem.eql(u8, request.op, "inspect")) return inspect(request, arena);
    if (std.mem.eql(u8, request.op, "get_state")) return getState(request, arena);
    if (std.mem.eql(u8, request.op, "set_modules")) return setModules(request, arena);
    if (std.mem.eql(u8, request.op, "clear")) {
        const session = try ensureSession();
        const removed_definition_count = session.definitionCount();
        session.clear();
        const changed = removed_definition_count > 0;
        const revision = if (changed) try advanceRevision() else session_revision;
        return okResponse(request.id, .{
            .changed = changed,
            .removed_definition_count = removed_definition_count,
            .revision = revision,
        });
    }
    return errorResponse(request.id, "unknown_operation", "Unknown REPL operation.");
}

fn processJson(bytes: []const u8) Allocator.Error![]u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const parsed = std.json.parseFromSlice(Request, arena, bytes, .{}) catch {
        return errorResponse(.null, "invalid_json", "Request must be a valid REPL protocol JSON object.");
    };
    const request = parsed.value;
    return handleRequest(request, arena) catch |err| switch (err) {
        error.MissingSource => errorResponse(request.id, "missing_source", "This operation requires params.source."),
        error.MissingCursor => errorResponse(request.id, "missing_cursor", "complete requires params.cursor as a UTF-8 byte offset."),
        error.InvalidCursor => errorResponse(request.id, "invalid_cursor", "The completion cursor must be a UTF-8 boundary within params.source."),
        error.MissingModules => errorResponse(request.id, "missing_modules", "set_modules requires params.modules."),
        error.DuplicateVirtualModule => errorResponse(request.id, "duplicate_module", "Virtual module names must be unique."),
        error.ReservedVirtualModule => errorResponse(request.id, "reserved_module", "Repl is provided by the REPL and cannot be replaced."),
        else => errorResponse(request.id, "internal_error", "The REPL could not complete this request."),
    };
}

fn storeResponse(bytes: []const u8) u32 {
    const storage = allocator.alloc(u8, bytes.len + 4) catch return 0;
    const len: u32 = @intCast(bytes.len);
    storage[0] = @truncate(len);
    storage[1] = @truncate(len >> 8);
    storage[2] = @truncate(len >> 16);
    storage[3] = @truncate(len >> 24);
    @memcpy(storage[4..], bytes);
    return @intCast(@intFromPtr(storage.ptr));
}

fn responseLength(ptr: u32) u32 {
    const bytes: [*]const u8 = @ptrFromInt(ptr);
    return @as(u32, bytes[0]) |
        (@as(u32, bytes[1]) << 8) |
        (@as(u32, bytes[2]) << 16) |
        (@as(u32, bytes[3]) << 24);
}

/// Allocate request bytes in the module's linear memory.
export fn roc_repl_alloc(len: u32) u32 {
    const bytes = allocator.alloc(u8, len) catch return 0;
    return @intCast(@intFromPtr(bytes.ptr));
}

/// Free a request allocation returned by `roc_repl_alloc`.
export fn roc_repl_free(ptr: u32, len: u32) void {
    if (ptr == 0) return;
    const bytes: [*]u8 = @ptrFromInt(ptr);
    allocator.free(bytes[0..len]);
}

/// Process one UTF-8 JSON request. The return points at a four-byte little-endian
/// payload length followed by the UTF-8 JSON response.
export fn roc_repl_process(ptr: u32, len: u32) u32 {
    if (ptr == 0) return 0;
    const request: [*]const u8 = @ptrFromInt(ptr);
    const response = processJson(request[0..len]) catch return 0;
    defer allocator.free(response);
    return storeResponse(response);
}

/// Free a response returned by `roc_repl_process`.
export fn roc_repl_free_response(ptr: u32) void {
    if (ptr == 0) return;
    const len = responseLength(ptr);
    const bytes: [*]u8 = @ptrFromInt(ptr);
    allocator.free(bytes[0 .. @as(usize, len) + 4]);
}
