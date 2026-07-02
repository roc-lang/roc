//! WASM module for the Roc playground.
//!
//! This module provides a state machine interface for the Roc compiler.
//!
//! State Machine:
//! 1. START: Initialize module, return compiler version
//! 2. READY: Receive Roc source, compile through all stages, return "LOADED" with diagnostics
//! 3. LOADED: Handle queries for tokens, AST, CIR, types, etc. Handle reset to go back to READY
//! 4. REPL_ACTIVE: Handle REPL interactions with stateful evaluation
//!
//! "Keep going" Strategy:
//! The playground uses a "keep going" approach - all compiler stages run even when there
//! are errors in earlier stages. This provides type information and later-stage errors
//! even when there are syntax errors. Malformed nodes are used to represent invalid code,
//! allowing the compiler to continue through all stages.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const build_options = @import("build_options");
const parse = @import("parse");
const reporting = @import("reporting");
const eval = @import("eval");
const lir = @import("lir");
const types = @import("types");
const can = @import("can");
const CoreCtx = can.CoreCtx;
const check = @import("check");
const unbundle = @import("unbundle");
const fmt = @import("fmt");
const WasmFilesystem = @import("WasmFilesystem.zig");

// WASM filesystem context — module-level so it persists across compilations.
var wasm_ctx: WasmFilesystem.WasmContext = .{};
const layout = @import("layout");
const compiled_builtins = @import("compiled_builtins");

const Can = can.Can;
const Check = check.Check;
const SExprTree = base.SExprTree;
const ModuleEnv = can.ModuleEnv;
const LoadedBuiltinModule = eval.builtin_static.BuiltinModuleView;
const Allocator = std.mem.Allocator;
const AST = parse.AST;

var allocator: Allocator = std.heap.wasm_allocator;

const PlaygroundCompileError =
    Allocator.Error ||
    fmt.FormatAstError ||
    eval.test_helpers.TestHelperError ||
    check.CheckedArtifact.CompileTimeFinalizer.Error ||
    error{
        ErrFinalizingHTMLWriter,
        Internal,
        TypeCheckError,
        WriteFailed,
    };

const PlaygroundEvaluateTestsError = PlaygroundCompileError || lir.CheckedPipeline.LowerResourceError || eval.LirInterpreter.Error;

/// Playground-specific std options, including a freestanding-safe log sink.
pub const std_options: std.Options = .{
    .log_level = .warn,
    .logFn = logFn,
};

const logFn = if (builtin.target.os.tag == .freestanding)
    struct {
        fn log(comptime _: std.log.Level, comptime _: @TypeOf(.enum_literal), comptime _: []const u8, _: anytype) void {}
    }.log
else
    struct {
        fn log(comptime level: std.log.Level, comptime scope: @TypeOf(.enum_literal), comptime format: []const u8, args: anytype) void {
            std.log.defaultLog(level, scope, format, args);
        }
    }.log;

const State = enum {
    START,
    READY,
    LOADED,
    REPL_ACTIVE,
};

/// Message types for communication with the host
const MessageType = enum {
    INIT,
    LOAD_SOURCE,
    QUERY_TOKENS,
    QUERY_AST,
    QUERY_CIR,
    QUERY_TYPES,
    QUERY_FORMATTED,
    GET_HOVER_INFO,
    EVALUATE_TESTS,
    RESET,
    INIT_REPL,
    REPL_STEP,
    CLEAR_REPL,

    pub fn fromString(str: []const u8) ?MessageType {
        if (std.mem.eql(u8, str, "INIT")) return .INIT;
        if (std.mem.eql(u8, str, "LOAD_SOURCE")) return .LOAD_SOURCE;
        if (std.mem.eql(u8, str, "QUERY_TOKENS")) return .QUERY_TOKENS;
        if (std.mem.eql(u8, str, "QUERY_AST")) return .QUERY_AST;
        if (std.mem.eql(u8, str, "QUERY_CIR")) return .QUERY_CIR;
        if (std.mem.eql(u8, str, "QUERY_TYPES")) return .QUERY_TYPES;
        if (std.mem.eql(u8, str, "QUERY_FORMATTED")) return .QUERY_FORMATTED;
        if (std.mem.eql(u8, str, "GET_HOVER_INFO")) return .GET_HOVER_INFO;
        if (std.mem.eql(u8, str, "EVALUATE_TESTS")) return .EVALUATE_TESTS;
        if (std.mem.eql(u8, str, "RESET")) return .RESET;
        if (std.mem.eql(u8, str, "INIT_REPL")) return .INIT_REPL;
        if (std.mem.eql(u8, str, "REPL_STEP")) return .REPL_STEP;
        if (std.mem.eql(u8, str, "CLEAR_REPL")) return .CLEAR_REPL;
        return null;
    }
};

/// Response status
const ResponseStatus = enum {
    SUCCESS,
    ERROR,
    INVALID_STATE,
    INVALID_MESSAGE,

    pub fn toString(self: ResponseStatus) []const u8 {
        return switch (self) {
            .SUCCESS => "SUCCESS",
            .ERROR => "ERROR",
            .INVALID_STATE => "INVALID_STATE",
            .INVALID_MESSAGE => "INVALID_MESSAGE",
        };
    }
};

/// Diagnostic region information for frontend integration
const DiagnosticRegion = struct {
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
};

/// Diagnostic information for frontend integration
const DiagnosticSeverity = enum { @"error", warning, info };

const Diagnostic = struct {
    severity: DiagnosticSeverity,
    message: []const u8,
    region: DiagnosticRegion,
};

/// Compiler stage data
const CompilerStageData = struct {
    module_env: *ModuleEnv,
    owned_source: ?[]const u8 = null,
    parse_ast: ?*parse.AST = null,
    solver: ?Check = null,
    bool_stmt: ?can.CIR.Statement.Idx = null,
    builtin_types: ?eval.BuiltinTypes = null,
    imported_modules: ?[]const *const ModuleEnv = null,
    auto_imported_types: ?*std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType) = null,

    // Pre-canonicalization HTML representations
    tokens_html: ?[]const u8 = null,
    ast_html: ?[]const u8 = null,
    formatted_code: ?[]const u8 = null,

    // Diagnostic reports from each stage
    tokenize_reports: std.array_list.Managed(reporting.Report),
    parse_reports: std.array_list.Managed(reporting.Report),
    can_reports: std.array_list.Managed(reporting.Report),
    type_reports: std.array_list.Managed(reporting.Report),

    pub fn init(alloc: Allocator, module_env: *ModuleEnv) CompilerStageData {
        return CompilerStageData{
            .module_env = module_env,
            .tokenize_reports = std.array_list.Managed(reporting.Report).init(alloc),
            .parse_reports = std.array_list.Managed(reporting.Report).init(alloc),
            .can_reports = std.array_list.Managed(reporting.Report).init(alloc),
            .type_reports = std.array_list.Managed(reporting.Report).init(alloc),
        };
    }

    pub fn deinit(self: *CompilerStageData) void {
        // Deinit solver first, as it may hold references to other data
        if (self.solver) |*s| {
            s.deinit();
        }

        if (self.auto_imported_types) |map| {
            map.deinit();
            allocator.destroy(map);
        }

        if (self.imported_modules) |modules| {
            allocator.free(modules);
        }

        // Free pre-generated HTML
        if (self.tokens_html) |html| allocator.free(html);
        if (self.ast_html) |html| allocator.free(html);
        if (self.formatted_code) |code| allocator.free(code);

        // Deinit reports, which may reference data in the AST or ModuleEnv
        for (self.tokenize_reports.items) |*report| {
            report.deinit();
        }
        self.tokenize_reports.deinit();

        for (self.parse_reports.items) |*report| {
            report.deinit();
        }
        self.parse_reports.deinit();

        for (self.can_reports.items) |*report| {
            report.deinit();
        }
        self.can_reports.deinit();

        for (self.type_reports.items) |*report| {
            report.deinit();
        }
        self.type_reports.deinit();

        // Deinit the AST, which depends on the ModuleEnv's allocator and source
        if (self.parse_ast) |ast| {
            ast.deinit();
        }

        // Finally, deinit the ModuleEnv and free its memory
        self.module_env.deinit();
        allocator.destroy(self.module_env);

        if (self.owned_source) |source| {
            allocator.free(source);
        }
    }
};

/// Global state machine
var current_state: State = .START;
var compiler_data: ?CompilerStageData = null;
var cached_builtin_module: ?LoadedBuiltinModule = null;

/// REPL state management
const ReplDefinitionKind = enum {
    value,
    type_annotation,
    type_declaration,
    import,
    file_import,
};

const ReplDefinition = struct {
    kind: ReplDefinitionKind,
    name: []u8,
    source: []u8,

    fn deinit(self: *ReplDefinition, alloc: Allocator) void {
        alloc.free(self.name);
        alloc.free(self.source);
    }
};

const ReplSession = struct {
    definitions: std.ArrayList(ReplDefinition) = .empty,

    fn deinit(self: *ReplSession, alloc: Allocator) void {
        for (self.definitions.items) |*definition| {
            definition.deinit(alloc);
        }
        self.definitions.deinit(alloc);
    }

    fn clear(self: *ReplSession, alloc: Allocator) void {
        for (self.definitions.items) |*definition| {
            definition.deinit(alloc);
        }
        self.definitions.clearRetainingCapacity();
    }

    fn hasDefinition(self: *const ReplSession, kind: ReplDefinitionKind, name: []const u8) bool {
        for (self.definitions.items) |definition| {
            if (definition.kind == kind and std.mem.eql(u8, definition.name, name)) return true;
        }
        return false;
    }

    fn upsertDefinition(
        self: *ReplSession,
        alloc: Allocator,
        kind: ReplDefinitionKind,
        name: []const u8,
        source: []const u8,
    ) Allocator.Error!void {
        const owned_name = try alloc.dupe(u8, name);
        errdefer alloc.free(owned_name);
        const owned_source = try alloc.dupe(u8, source);
        errdefer alloc.free(owned_source);

        for (self.definitions.items) |*definition| {
            if (definition.kind == kind and std.mem.eql(u8, definition.name, name)) {
                definition.deinit(alloc);
                definition.* = .{
                    .kind = kind,
                    .name = owned_name,
                    .source = owned_source,
                };
                return;
            }
        }

        try self.definitions.append(alloc, .{
            .kind = kind,
            .name = owned_name,
            .source = owned_source,
        });
    }
};

var repl_session: ?ReplSession = null;

/// REPL result types
const ReplTryType = enum {
    expression,
    definition,
    @"error",

    pub fn jsonStringify(self: ReplTryType, writer: anytype) error{WriteFailed}!void {
        try writer.writeAll("\"");
        try writer.writeAll(@tagName(self));
        try writer.writeAll("\"");
    }
};

/// REPL error stages
const ReplErrorStage = enum {
    parse,
    canonicalize,
    typecheck,
    layout,
    evaluation,
    interpreter,
    runtime,
    unknown,

    pub fn jsonStringify(self: ReplErrorStage, writer: anytype) error{WriteFailed}!void {
        try writer.writeAll("\"");
        try writer.writeAll(@tagName(self));
        try writer.writeAll("\"");
    }
};

/// Structured REPL result
const ReplStepResult = struct {
    output: []const u8,
    try_type: ReplTryType,
    error_stage: ?ReplErrorStage = null,
    error_details: ?[]const u8 = null,
    compiler_available: bool = true,
};

/// Host-managed buffers for better memory management
var host_message_buffer: ?[]u8 = null;
var host_response_buffer: ?[]u8 = null;
var last_error: ?[:0]const u8 = null;

/// In-memory debug log buffer for WASM
var debug_log_buffer: [4096]u8 = undefined;
var debug_log_pos: usize = 0;
var debug_log_oom: bool = false;

fn resetGlobalState() void {
    if (compiler_data) |*data| {
        data.deinit();
        compiler_data = null;
    }
    cleanupReplState();
}

/// Writes a formatted string to the in-memory debug log.
fn logDebug(comptime format: []const u8, args: anytype) void {
    if (debug_log_oom) {
        // Already OOM, do nothing.
        return;
    }

    if (debug_log_pos >= debug_log_buffer.len) {
        // Buffer is already full. Can't write anything more.
        // This state should only be reached if clearDebugLog isn't called often enough.
        debug_log_oom = true;
        return;
    }

    const available_space = debug_log_buffer.len - debug_log_pos;
    const target_slice = debug_log_buffer[debug_log_pos..];

    const formatted_slice: []u8 = std.fmt.bufPrint(target_slice, format, args) catch {
        // bufPrint failed. This is usually due to insufficient space.
        // Try to log an "out of space" indicator.
        const oom_msg = "\n[DEBUG LOG OOM]\n";
        if (available_space >= oom_msg.len) {
            @memcpy(target_slice[0..oom_msg.len], oom_msg);
            debug_log_pos += oom_msg.len;
        } else if (available_space > 0) {
            // Not even enough space for the full OOM message. Write what we can.
            const truncated_msg = "[OOM]";
            const bytes_to_write = @min(available_space, truncated_msg.len);
            @memcpy(target_slice[0..bytes_to_write], truncated_msg[0..bytes_to_write]);
            debug_log_pos += bytes_to_write;
        }
        // If there's no space for even "[OOM]", we can't do anything.
        debug_log_oom = true;
        return;
    };

    debug_log_pos += formatted_slice.len;

    // Ensure the log is always null-terminated if it's not empty,
    // overwriting the previous null terminator if necessary.
    // This provides a clear string boundary for the host.
    if (debug_log_pos > 0 and debug_log_pos <= debug_log_buffer.len) {
        if (debug_log_pos == debug_log_buffer.len) {
            // Buffer is exactly full. Overwrite the last character with null terminator.
            debug_log_buffer[debug_log_pos - 1] = 0;
        } else {
            // Space remains. Place null terminator after the last written byte.
            debug_log_buffer[debug_log_pos] = 0;
        }
    }
}

/// Export the debug log buffer and its length for the host to read.
export fn getDebugLogBuffer() [*]const u8 {
    return &debug_log_buffer;
}

export fn getDebugLogLength() usize {
    return debug_log_pos;
}

/// Clears the debug log buffer.
export fn clearDebugLog() void {
    @memset(debug_log_buffer[0..debug_log_pos], 0); // Optional: clear old data
    debug_log_pos = 0;
    debug_log_oom = false;
}

fn getCachedBuiltinModule() (Allocator.Error || error{ CorruptEmbeddedBuiltins, Internal })!*LoadedBuiltinModule {
    if (cached_builtin_module == null) {
        logDebug("compileSource: Creating Builtin module view\n", .{});
        cached_builtin_module = try eval.builtin_static.moduleView(
            allocator,
            compiled_builtins.builtin_bin[0..],
            "Builtin",
            compiled_builtins.builtin_source,
        );
        logDebug("compileSource: Builtin module view ready\n", .{});
    } else {
        logDebug("compileSource: Reusing cached Builtin module\n", .{});
    }

    return &cached_builtin_module.?;
}

/// Error codes returned to host
pub const WasmError = enum(u8) {
    success = 0,
    invalid_json = 1,
    missing_message_type = 2,
    unknown_message_type = 3,
    invalid_state_for_message = 4,
    response_buffer_too_small = 5,
    internal_error = 6,
};

/// Errors that can occur during response writing
const ResponseWriteError = error{
    OutOfBufferSpace,
    WriteFailed,
    OutOfMemory,
};

fn cleanupReplState() void {
    if (repl_session) |*session| {
        session.deinit(allocator);
    }
    repl_session = null;
}

/// Initialize the WASM module in START state
export fn init() void {
    if (compiler_data) |*data| {
        data.deinit();
        compiler_data = null;
    }

    // Clean up REPL state
    cleanupReplState();

    // Initialize buffer pointers to null
    host_message_buffer = null;
    host_response_buffer = null;
    last_error = null;
}

/// Allocate a buffer for incoming messages from the host.
/// Returns null on allocation failure.
export fn allocateMessageBuffer(size: usize) ?[*]u8 {
    if (host_message_buffer) |buf| {
        allocator.free(buf);
        host_message_buffer = null;
    }
    host_message_buffer = allocator.alloc(u8, size) catch return null;
    return host_message_buffer.?.ptr;
}

/// Allocate a buffer for responses to the host.
/// Returns null on allocation failure.
export fn allocateResponseBuffer(size: usize) ?[*]u8 {
    if (host_response_buffer) |buf| {
        allocator.free(buf);
        host_response_buffer = null;
    }
    host_response_buffer = allocator.alloc(u8, size) catch return null;
    return host_response_buffer.?.ptr;
}

/// Free the message buffer
export fn freeMessageBuffer() void {
    if (host_message_buffer) |buf| {
        allocator.free(buf);
        host_message_buffer = null;
    }
}

/// Free the response buffer
export fn freeResponseBuffer() void {
    if (host_response_buffer) |buf| {
        allocator.free(buf);
        host_response_buffer = null;
    }
}

/// Get the last error message
export fn getLastError() [*:0]const u8 {
    if (last_error) |err| {
        // A sentinel-terminated slice [:0]const u8 should coerce to [*:0]const u8.
        // If not, std.mem.span can be used: return std.mem.span(err);
        return err;
    }
    // An empty string literal is also sentinel-terminated.
    return "";
}

/// Process a message and write the response into the provided buffer.
/// The response format is: [u32: length_of_json_data][u8: json_data...]
/// Returns a WasmError code.
export fn processMessage(message_ptr: [*]const u8, message_len: usize, response_ptr: [*]u8, response_len: usize) u8 {
    const message_slice = message_ptr[0..message_len];
    const response_slice = response_ptr[0..response_len];

    // Check if buffer is large enough for the length prefix (u32)
    if (response_slice.len < @sizeOf(u32)) {
        return @intFromEnum(WasmError.response_buffer_too_small);
    }

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, message_slice, .{}) catch {
        // Write error response. This will also write the length prefix.
        writeErrorResponse(response_slice, ResponseStatus.ERROR, "Invalid JSON message") catch return @intFromEnum(WasmError.response_buffer_too_small);
        return @intFromEnum(WasmError.success);
    };
    defer parsed.deinit();

    const root = parsed.value;
    const message_type_str = root.object.get("type") orelse {
        writeErrorResponse(response_slice, ResponseStatus.INVALID_MESSAGE, "Missing message type") catch return @intFromEnum(WasmError.response_buffer_too_small);
        return @intFromEnum(WasmError.success);
    };

    const message_type = MessageType.fromString(message_type_str.string) orelse {
        writeErrorResponse(response_slice, ResponseStatus.INVALID_MESSAGE, "Unknown message type") catch return @intFromEnum(WasmError.response_buffer_too_small);
        return @intFromEnum(WasmError.success);
    };

    // Handle message based on current state
    const result = switch (current_state) {
        .START => handleStartState(message_type, root, response_slice),
        .READY => handleReadyState(message_type, root, response_slice),
        .LOADED => handleLoadedState(message_type, root, response_slice),
        .REPL_ACTIVE => handleReplState(message_type, root, response_slice),
    };

    return if (result) |_| @intFromEnum(WasmError.success) else |err| switch (err) {
        error.OutOfBufferSpace => @intFromEnum(WasmError.response_buffer_too_small),
        error.WriteFailed, error.OutOfMemory => @intFromEnum(WasmError.internal_error),
    };
}

/// Handle messages in START state
fn handleStartState(message_type: MessageType, _: std.json.Value, response_buffer: []u8) ResponseWriteError!void {
    switch (message_type) {
        .INIT => {
            current_state = .READY;
            const compiler_version = build_options.compiler_version;
            try writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            try writeErrorResponse(response_buffer, .INVALID_STATE, "INVALID_STATE");
        },
    }
}

/// Handle messages in READY state
fn handleReadyState(message_type: MessageType, root: std.json.Value, response_buffer: []u8) ResponseWriteError!void {
    switch (message_type) {
        .LOAD_SOURCE => {
            const source_value = root.object.get("source") orelse {
                try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing source");
                return;
            };

            const source = source_value.string;

            // Extract optional filename and derive module name
            const filename = if (root.object.get("filename")) |filename_value|
                filename_value.string
            else
                "main.roc";

            // Set filename in filesystem for file operations
            wasm_ctx.setFilename(allocator, filename);

            // Derive module name from filename (strip .roc extension)
            const module_name = if (std.mem.endsWith(u8, filename, ".roc"))
                filename[0 .. filename.len - 4]
            else
                filename;

            // Clean up previous compilation if any
            if (compiler_data) |*data| {
                data.deinit();
                compiler_data = null;
            }

            // Compile the source through all stages
            const result = compileSource(source, module_name) catch |err| {
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };

            compiler_data = result;
            current_state = .LOADED;

            // Return success with diagnostics
            try writeLoadedResponse(response_buffer, result);
        },
        .INIT_REPL => {
            if (compiler_data) |*data| {
                data.deinit();
                compiler_data = null;
            }
            cleanupReplState();
            repl_session = .{};
            current_state = .REPL_ACTIVE;
            try writeReplInitResponse(response_buffer);
        },
        .RESET => {
            resetGlobalState();

            current_state = .READY;

            const compiler_version = build_options.compiler_version;
            try writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            try writeErrorResponse(response_buffer, .INVALID_STATE, "INVALID_STATE");
        },
    }
}

/// Handle messages in LOADED state
fn handleLoadedState(message_type: MessageType, message_json: std.json.Value, response_buffer: []u8) ResponseWriteError!void {
    const data = compiler_data orelse {
        try writeErrorResponse(response_buffer, .ERROR, "Compiler data not loaded (internal error)");
        return;
    };

    switch (message_type) {
        .QUERY_TOKENS => {
            try writeTokensResponse(response_buffer, data);
        },
        .QUERY_AST => {
            try writeParseAstResponse(response_buffer, data);
        },
        .QUERY_CIR => {
            try writeCanCirResponse(response_buffer, data);
        },
        .QUERY_TYPES => {
            try writeTypesResponse(response_buffer, data);
        },
        .QUERY_FORMATTED => {
            try writeFormattedResponse(response_buffer, data);
        },
        .GET_HOVER_INFO => {
            try writeHoverInfoResponse(response_buffer, data, message_json);
        },
        .EVALUATE_TESTS => {
            try writeEvaluateTestsResponse(response_buffer, data);
        },
        .RESET => {
            resetGlobalState();

            current_state = .READY;

            const compiler_version = build_options.compiler_version;
            try writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            try writeErrorResponse(response_buffer, .INVALID_STATE, "INVALID_STATE");
        },
    }
}

/// Handle messages in REPL_ACTIVE state.
/// This function processes REPL-specific messages including REPL_STEP, CLEAR_REPL,
/// RESET, and compiler queries (QUERY_CIR, QUERY_TYPES, GET_HOVER_INFO).
/// The REPL instance must be initialized before calling this function.
/// Returns an error if the response buffer is too small or if internal errors occur.
fn handleReplState(message_type: MessageType, root: std.json.Value, response_buffer: []u8) ResponseWriteError!void {
    const session = if (repl_session) |*session| session else {
        try writeErrorResponse(response_buffer, .ERROR, "REPL not initialized");
        return;
    };

    switch (message_type) {
        .REPL_STEP => {
            const input_value = root.object.get("input") orelse {
                try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing input for REPL_STEP");
                return;
            };

            try runReplStep(session, input_value.string, response_buffer);
        },
        .CLEAR_REPL => {
            session.clear(allocator);
            if (compiler_data) |*data| {
                data.deinit();
                compiler_data = null;
            }
            try writeReplClearResponse(response_buffer);
        },
        .RESET => {
            resetGlobalState();

            current_state = .READY;

            const compiler_version = build_options.compiler_version;
            try writeSuccessResponse(response_buffer, compiler_version, null);
        },
        .QUERY_CIR => {
            const data = compiler_data orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };
            try writeCanCirResponse(response_buffer, data);
        },
        .QUERY_TYPES => {
            const data = compiler_data orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };
            try writeTypesResponse(response_buffer, data);
        },
        .GET_HOVER_INFO => {
            const data = compiler_data orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };
            try writeHoverInfoResponse(response_buffer, data, root);
        },
        .QUERY_TOKENS => {
            const data = compiler_data orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };
            try writeTokensResponse(response_buffer, data);
        },
        .QUERY_AST => {
            const data = compiler_data orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };
            try writeParseAstResponse(response_buffer, data);
        },
        .QUERY_FORMATTED => {
            const data = compiler_data orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };
            try writeFormattedResponse(response_buffer, data);
        },
        else => {
            try writeErrorResponse(response_buffer, .INVALID_STATE, "INVALID_STATE");
        },
    }
}

const ReplInputKind = enum {
    definition,
    expression,
};

const ReplDefinitionIdentity = struct {
    kind: ReplDefinitionKind,
    name: []const u8,
};

fn resolveReplInputKind(line: []const u8) std.mem.Allocator.Error!?ReplInputKind {
    var env = try ModuleEnv.init(allocator, line);
    defer env.deinit();
    env.common.source = line;
    try env.common.calcLineStarts(allocator);

    const ast = try parse.statement(allocator, &env.common);
    defer ast.deinit();
    if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) return null;

    const statement = ast.store.getStatement(@enumFromInt(ast.root_node_idx));
    return switch (statement) {
        .expr => .expression,
        .decl,
        .@"var",
        .import,
        .file_import,
        .type_decl,
        .type_anno,
        => .definition,
        .malformed => null,
        .crash,
        .dbg,
        .expect,
        .@"for",
        .@"while",
        .@"return",
        .@"break",
        => .expression,
    };
}

fn replDefinitionIdentity(line: []const u8) std.mem.Allocator.Error!?ReplDefinitionIdentity {
    var env = try ModuleEnv.init(allocator, line);
    defer env.deinit();
    env.common.source = line;
    try env.common.calcLineStarts(allocator);

    const ast = try parse.statement(allocator, &env.common);
    defer ast.deinit();
    if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) return null;

    const statement = ast.store.getStatement(@enumFromInt(ast.root_node_idx));
    return switch (statement) {
        .decl => |decl| blk: {
            const pattern = ast.store.getPattern(decl.pattern);
            break :blk switch (pattern) {
                .ident => |ident| .{ .kind = .value, .name = ast.resolve(ident.ident_tok) },
                .var_ident => |ident| .{ .kind = .value, .name = ast.resolve(ident.ident_tok) },
                else => null,
            };
        },
        .@"var" => |var_decl| .{ .kind = .value, .name = ast.resolve(var_decl.name) },
        .type_anno => |anno| .{ .kind = .type_annotation, .name = ast.resolve(anno.name) },
        .type_decl => |decl| blk: {
            const header = ast.store.getTypeHeader(decl.header) catch break :blk null;
            break :blk .{ .kind = .type_declaration, .name = ast.resolve(header.name) };
        },
        .import => |import| .{
            .kind = .import,
            .name = ast.resolveImportModulePath(import.module_name_tok, import.qualifier_tok, import.exposes),
        },
        .file_import => |file_import| .{ .kind = .file_import, .name = ast.resolve(file_import.name_tok) },
        else => null,
    };
}

fn writeDefinitionsWithReplacement(
    writer: *std.Io.Writer,
    session: *const ReplSession,
    replacement: ?ReplDefinitionIdentity,
    replacement_source: ?[]const u8,
) error{WriteFailed}!void {
    var replaced = false;
    for (session.definitions.items) |definition| {
        if (replacement) |identity| {
            if (definition.kind == identity.kind and std.mem.eql(u8, definition.name, identity.name)) {
                try writer.writeAll(replacement_source.?);
                try writer.writeAll("\n");
                replaced = true;
                continue;
            }
        }

        try writer.writeAll(definition.source);
        try writer.writeAll("\n");
    }

    if (!replaced) {
        if (replacement_source) |source| {
            try writer.writeAll(source);
            try writer.writeAll("\n");
        }
    }
}

fn buildReplModuleSource(
    session: *const ReplSession,
    replacement: ?ReplDefinitionIdentity,
    replacement_source: ?[]const u8,
    main_expr: ?[]const u8,
) (Allocator.Error || error{WriteFailed})![]u8 {
    var source_writer: std.Io.Writer.Allocating = .init(allocator);
    errdefer source_writer.deinit();

    // REPL snippets are evaluated as an internal module. Without an explicit
    // header, production validation treats the synthetic source as a type
    // module/default-app candidate and rejects ordinary REPL definitions like
    // `x = 42`.
    try source_writer.writer.writeAll("module []\n\n");
    try writeDefinitionsWithReplacement(&source_writer.writer, session, replacement, replacement_source);
    if (main_expr) |expr| {
        try source_writer.writer.print("main = {s}\n", .{expr});
    }
    try source_writer.writer.flush();

    return source_writer.toOwnedSlice();
}

const ReplCompiledModule = struct {
    lowered: eval.test_helpers.LoweredProgram,

    fn deinit(self: *@This()) void {
        self.lowered.deinit(allocator);
    }
};

fn findDefByName(module_env: *const ModuleEnv, name: []const u8) ?can.CIR.Def.Idx {
    for (module_env.store.sliceDefs(module_env.all_defs)) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        const pattern = module_env.store.getPattern(def.pattern);
        const ident = switch (pattern) {
            .assign => |assign| assign.ident,
            .as => |as_pattern| as_pattern.ident,
            else => continue,
        };
        if (std.mem.eql(u8, module_env.getIdent(ident), name)) return def_idx;
    }
    return null;
}

fn compileReplInspectedModule(source: []const u8) PlaygroundCompileError!ReplCompiledModule {
    var checked_module = try compileCheckedReplModuleSource(source);
    errdefer checked_module.deinit();

    const builtin_module = try getCachedBuiltinModule();

    var source_modules = [_]check.TypedCIR.Modules.SourceModule{
        .{ .precompiled = checked_module.module_env },
        .{ .precompiled = builtin_module.env },
    };
    var typed_cir_modules = try check.TypedCIR.Modules.init(allocator, &source_modules);
    defer typed_cir_modules.deinit();

    var builtin_artifact = try check.CheckedArtifact.publishFromTypedModule(
        allocator,
        &typed_cir_modules,
        1,
        .{
            .module_env_storage = .{ .static_builtin = builtin_module.env },
            .compile_time_finalizer = eval.CompileTimeFinalization.finalizer(),
        },
    );
    errdefer builtin_artifact.deinitRetainingModuleEnv(allocator);

    var publish_imports = [_]check.CheckedArtifact.PublishImportArtifact{.{
        .module_idx = 1,
        .key = builtin_artifact.key,
        .view = check.CheckedArtifact.importedView(&builtin_artifact),
    }};

    const main_def = findDefByName(checked_module.module_env, "main") orelse return error.TypeCheckError;
    var explicit_roots = [_]check.CheckedArtifact.ExplicitRootRequestInput{.{
        .kind = .dev_expr,
        .source = .{ .def = main_def },
        .abi = .roc,
        .exposure = .private,
    }};

    var root_artifact = try check.CheckedArtifact.publishFromTypedModule(
        allocator,
        &typed_cir_modules,
        0,
        .{
            .module_env_storage = .{ .checked_source = checked_module.module_env },
            .imports = &publish_imports,
            .explicit_roots = &explicit_roots,
            .compile_time_finalizer = eval.CompileTimeFinalization.finalizer(),
        },
    );
    errdefer root_artifact.deinitRetainingModuleEnv(allocator);

    var import_artifacts = [_]check.CheckedArtifact.CheckedModuleArtifact{builtin_artifact};
    const lowered = try eval.test_helpers.lowerCheckedModuleSetToLir(allocator, @as(std.Io, undefined), &root_artifact, &import_artifacts, .u32);

    root_artifact.deinitRetainingModuleEnv(allocator);
    import_artifacts[0].deinitRetainingModuleEnv(allocator);
    checked_module.deinit();

    return .{ .lowered = lowered };
}

fn hasBlockingReports(reports: std.array_list.Managed(reporting.Report)) bool {
    for (reports.items) |report| {
        switch (report.severity) {
            .runtime_error, .fatal => return true,
            .info, .warning => {},
        }
    }
    return false;
}

fn compileCheckedReplModuleSource(source: []const u8) PlaygroundCompileError!CompilerStageData {
    var data = try compileSource(source, "main");
    errdefer data.deinit();

    if (hasBlockingReports(data.tokenize_reports) or
        hasBlockingReports(data.parse_reports) or
        hasBlockingReports(data.can_reports) or
        hasBlockingReports(data.type_reports))
    {
        return error.TypeCheckError;
    }

    return data;
}

fn replaceCompilerData(new_data: CompilerStageData) void {
    if (compiler_data) |*existing| {
        existing.deinit();
        compiler_data = null;
    }

    compiler_data = new_data;
}

fn replaceCompilerDataFromReplSource(source: []const u8) PlaygroundCompileError!void {
    replaceCompilerData(try compileSource(source, "main"));
}

fn writeReplStaticError(response_buffer: []u8, message: []const u8, stage: ReplErrorStage) ResponseWriteError!void {
    try writeReplStepResultJson(response_buffer, .{
        .output = message,
        .try_type = .@"error",
        .error_stage = stage,
        .error_details = message,
    });
}

fn runReplDefinition(
    session: *ReplSession,
    input: []const u8,
    response_buffer: []u8,
) ResponseWriteError!void {
    const identity = replDefinitionIdentity(input) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    } orelse {
        try writeReplStaticError(response_buffer, "REPL definitions must bind a top-level identifier", .canonicalize);
        return;
    };

    const defines_main = identity.kind == .value and std.mem.eql(u8, identity.name, "main");
    const validation_main_expr: ?[]const u8 = if (defines_main or session.hasDefinition(.value, "main")) null else "\"\"";
    const validation_source = buildReplModuleSource(session, identity, input, validation_main_expr) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    };
    defer allocator.free(validation_source);

    var checked_module = compileCheckedReplModuleSource(validation_source) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .typecheck);
        return;
    };
    var module_committed = false;
    defer if (!module_committed) checked_module.deinit();

    session.upsertDefinition(allocator, identity.kind, identity.name, input) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    };

    const output = std.fmt.allocPrint(allocator, "assigned `{s}`", .{identity.name}) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    };
    defer allocator.free(output);

    replaceCompilerData(checked_module);
    module_committed = true;

    try writeReplStepResultJson(response_buffer, .{
        .output = output,
        .try_type = .definition,
        .compiler_available = true,
    });
}

fn runReplExpression(
    session: *ReplSession,
    input: []const u8,
    response_buffer: []u8,
) ResponseWriteError!void {
    const main_source = std.fmt.allocPrint(allocator, "main = || Str.inspect(({s}))", .{input}) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    };
    defer allocator.free(main_source);

    const source = buildReplModuleSource(
        session,
        .{ .kind = .value, .name = "main" },
        main_source,
        null,
    ) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    };
    defer allocator.free(source);

    var compiled = compileReplInspectedModule(source) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .typecheck);
        return;
    };
    defer compiled.deinit();

    const output = eval.test_helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .interpreter);
        return;
    };
    defer allocator.free(output);

    const compiler_available = if (replaceCompilerDataFromReplSource(source)) true else |err| blk: {
        logDebug("REPL expression display compile failed: {}\n", .{err});
        break :blk false;
    };

    try writeReplStepResultJson(response_buffer, .{
        .output = output,
        .try_type = .expression,
        .compiler_available = compiler_available,
    });
}

fn runReplStep(session: *ReplSession, input: []const u8, response_buffer: []u8) ResponseWriteError!void {
    const trimmed = std.mem.trim(u8, input, " \t\r\n");
    if (trimmed.len == 0) {
        try writeReplStaticError(response_buffer, "UNEXPECTED TOKEN", .parse);
        return;
    }

    const input_kind = resolveReplInputKind(trimmed) catch |err| {
        try writeReplStaticError(response_buffer, @errorName(err), .runtime);
        return;
    } orelse {
        try writeReplStaticError(response_buffer, "UNEXPECTED TOKEN", .parse);
        return;
    };

    switch (input_kind) {
        .definition => try runReplDefinition(session, trimmed, response_buffer),
        .expression => try runReplExpression(session, trimmed, response_buffer),
    }
}

/// Compile source through all compiler stages.
/// module_name should be the filename without the .roc extension (e.g., "Person" for "Person.roc")
fn compileSource(source: []const u8, module_name: []const u8) PlaygroundCompileError!CompilerStageData {
    // Handle empty input gracefully to prevent crashes
    if (source.len == 0) {
        // Return empty compiler stage data for completely empty input
        var module_env = try allocator.create(ModuleEnv);
        errdefer allocator.destroy(module_env);

        const owned_source = try allocator.dupe(u8, source);
        errdefer allocator.free(owned_source);

        module_env.* = try ModuleEnv.init(allocator, owned_source);
        errdefer module_env.deinit();
        try module_env.common.calcLineStarts(module_env.gpa);
        var result = CompilerStageData.init(allocator, module_env);
        result.owned_source = owned_source;
        return result;
    }

    const trimmed_source = std.mem.trim(u8, source, " \t\n\r");
    if (trimmed_source.len == 0) {
        // Return empty compiler stage data for whitespace-only input
        var module_env = try allocator.create(ModuleEnv);
        errdefer allocator.destroy(module_env);

        const owned_source = try allocator.dupe(u8, source);
        errdefer allocator.free(owned_source);

        module_env.* = try ModuleEnv.init(allocator, owned_source);
        errdefer module_env.deinit();
        try module_env.common.calcLineStarts(module_env.gpa);
        var result = CompilerStageData.init(allocator, module_env);
        result.owned_source = owned_source;
        return result;
    }

    wasm_ctx.setSource(allocator, source);

    const stable_source = try allocator.dupe(u8, source);
    errdefer allocator.free(stable_source);

    logDebug("compileSource: Starting compilation (source len={})\n", .{stable_source.len});

    // Initialize the ModuleEnv
    var module_env = try allocator.create(ModuleEnv);
    errdefer allocator.destroy(module_env);

    module_env.* = try ModuleEnv.init(allocator, stable_source);
    errdefer module_env.deinit();
    try module_env.common.calcLineStarts(module_env.gpa);
    logDebug("compileSource: ModuleEnv initialized\n", .{});

    var result = CompilerStageData.init(allocator, module_env);
    result.owned_source = stable_source;

    // Stage 1: Parse (includes tokenization)
    logDebug("compileSource: Starting parse stage\n", .{});
    const parse_ast = try parse.file(allocator, &module_env.common);
    result.parse_ast = parse_ast;
    logDebug("compileSource: Parse complete\n", .{});

    // Generate and store HTML before canonicalization corrupts the AST/tokens
    logDebug("compileSource: Starting HTML generation\n", .{});
    var local_arena = base.SingleThreadArena.init(allocator);
    defer local_arena.deinit();
    const temp_alloc = local_arena.allocator();

    // Generate Tokens HTML
    logDebug("compileSource: Generating tokens HTML\n", .{});
    var tokens_writer: std.Io.Writer.Allocating = .init(temp_alloc);
    AST.tokensToHtml(parse_ast, &module_env.common, &tokens_writer.writer) catch |err| {
        logDebug("compileSource: tokensToHtml failed: {}\n", .{err});
    };
    logDebug("compileSource: Tokens HTML generated, duping to main allocator\n", .{});
    result.tokens_html = allocator.dupe(u8, tokens_writer.written()) catch |err| {
        logDebug("compileSource: failed to dupe tokens_html: {}\n", .{err});
        return err;
    };
    logDebug("compileSource: Tokens HTML complete\n", .{});

    // Generate AST HTML
    logDebug("compileSource: Generating AST HTML\n", .{});
    var ast_writer: std.Io.Writer.Allocating = .init(temp_alloc);
    const file = parse_ast.store.getFile();

    var tree = SExprTree.init(temp_alloc);

    logDebug("compileSource: Call pushToSExprTree\n", .{});
    try file.pushToSExprTree(module_env.gpa, &module_env.common, parse_ast, &tree);

    logDebug("compileSource: Call toHtml\n", .{});
    try tree.toHtml(&ast_writer.writer, .include_linecol);
    logDebug("compileSource: AST HTML generated\n", .{});

    result.ast_html = allocator.dupe(u8, ast_writer.written()) catch |err| {
        logDebug("compileSource: failed to dupe ast_html: {}\n", .{err});
        return err;
    };
    logDebug("compileSource: AST HTML complete\n", .{});

    // Generate formatted code
    logDebug("compileSource: Generating formatted code\n", .{});
    var formatted_code_buffer: std.Io.Writer.Allocating = .init(temp_alloc);
    defer formatted_code_buffer.deinit();
    fmt.formatAst(parse_ast.*, &formatted_code_buffer.writer) catch |err| {
        logDebug("compileSource: formatAst failed: {}\n", .{err});
        return err;
    };
    logDebug("compileSource: Formatted code generated\n", .{});

    result.formatted_code = allocator.dupe(u8, formatted_code_buffer.written()) catch |err| {
        logDebug("compileSource: failed to dupe formatted_code: {}\n", .{err});
        return err;
    };
    logDebug("compileSource: Formatted code complete\n", .{});

    // Collect tokenize diagnostics with additional error handling
    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        const report = parse_ast.tokenizeDiagnosticToReport(diagnostic, allocator, null) catch {
            // Log the error and continue processing other diagnostics
            // This prevents crashes on malformed diagnostics or empty input
            continue;
        };
        try result.tokenize_reports.append(report);
    }

    // Collect parse diagnostics with additional error handling
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        const report = parse_ast.parseDiagnosticToReport(&module_env.common, diagnostic, allocator, "main.roc") catch {
            // Log the error and continue processing other diagnostics
            // This prevents crashes on malformed diagnostics or empty input
            continue;
        };
        try result.parse_reports.append(report);
    }

    // Stage 2: Canonicalization (always run, even with parse errors)
    // The canonicalizer handles malformed parse nodes and continues processing
    const env = result.module_env;
    try env.initCIRFields(module_name);

    // Builtin is immutable for the lifetime of the WASM instance, so every
    // compile consumes the same explicit Builtin module context.

    logDebug("compileSource: Loading builtin indices\n", .{});
    const builtin_indices = compiled_builtins.builtinIndices(can.CIR);
    logDebug("compileSource: Builtin indices loaded, bool_type={}\n", .{@intFromEnum(builtin_indices.bool_type)});

    const builtin_module = try getCachedBuiltinModule();

    // Get builtin statement indices from the builtin module
    // Use builtin_indices directly - these are the correct statement indices
    logDebug("compileSource: Getting builtin statement indices\n", .{});
    const bool_stmt_in_builtin_module = builtin_indices.bool_type;
    const try_stmt_in_builtin_module = builtin_indices.try_type;

    logDebug("compileSource: Using Bool statement from Builtin module, idx={}\n", .{@intFromEnum(bool_stmt_in_builtin_module)});
    logDebug("compileSource: Using Result statement from Builtin module, idx={}\n", .{@intFromEnum(try_stmt_in_builtin_module)});
    logDebug("compileSource: Builtin injection complete\n", .{});

    // Store bool_stmt and builtin_types in result for later use (e.g., in test runner)
    result.bool_stmt = bool_stmt_in_builtin_module;
    result.builtin_types = eval.BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);

    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const module_builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("main")),
        .bool_stmt = bool_stmt_in_builtin_module,
        .try_stmt = try_stmt_in_builtin_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    logDebug("compileSource: Starting canonicalization\n", .{});
    const roc_ctx = CoreCtx.default(allocator, allocator, @as(std.Io, undefined));
    var czer = try Can.initModule(roc_ctx, env, result.parse_ast.?, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
    });
    defer czer.deinit();

    czer.canonicalizeFile() catch |err| {
        logDebug("compileSource: canonicalizeFile failed: {}\n", .{err});
        // If we're out of memory here, the state is likely unstable.
        // Propagate this error up to halt compilation gracefully.
        return err;
    };

    czer.validateForChecking() catch |err| {
        logDebug("compileSource: validateForChecking failed: {}\n", .{err});
        return err;
    };
    logDebug("compileSource: Canonicalization complete\n", .{});

    // Copy the modified AST back into the main result to ensure state consistency
    result.parse_ast = parse_ast;

    // Collect canonicalization diagnostics
    const diagnostics = try env.getDiagnostics();

    // Process and store CAN diagnostics
    for (diagnostics) |diagnostic| {
        const report = env.diagnosticToReport(diagnostic, allocator, "main.roc") catch {
            // Log the error and continue processing other diagnostics
            // This prevents crashes on malformed diagnostics or empty input
            continue;
        };
        try result.can_reports.append(report);
    }

    // Stage 3: Type checking (always run if we have CIR, even with canonicalization errors)
    // The type checker works with malformed canonical nodes to provide partial type information
    logDebug("compileSource: Starting type checking\n", .{});
    {
        const type_can_ir = result.module_env;
        const imported_envs = try allocator.alloc(*const ModuleEnv, 2);
        errdefer allocator.free(imported_envs);
        imported_envs[0] = type_can_ir;
        imported_envs[1] = builtin_module.env;
        result.imported_modules = imported_envs;

        const auto_imported_types = try allocator.create(std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType));
        errdefer allocator.destroy(auto_imported_types);
        auto_imported_types.* = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
        errdefer auto_imported_types.deinit();

        try Can.populateModuleEnvs(auto_imported_types, type_can_ir, builtin_module.env, builtin_indices);
        result.auto_imported_types = auto_imported_types;

        // Resolve imports - map each import to its index in imported_envs
        type_can_ir.imports.clearResolvedModules();
        try type_can_ir.imports.resolveImportsByExactModuleName(type_can_ir, imported_envs);
        type_can_ir.imports.markUnresolvedImportsFailedBeforeChecking();

        // Use pointer to the stored CIR to ensure solver references valid memory
        result.solver = try Check.init(allocator, &type_can_ir.types, type_can_ir, imported_envs, auto_imported_types, &type_can_ir.store.regions, module_builtin_ctx);
        var solver = &result.solver.?;
        solver.fixupTypeWriter();

        solver.checkFile() catch |check_err| {
            logDebug("compileSource: checkFile failed: {}\n", .{check_err});
            solver.deinit();
            result.solver = null; // Prevent double deinit in CompilerStageData.deinit
            return check_err;
        };
        logDebug("compileSource: Type checking complete\n", .{});

        // Collect type checking problems and convert them to reports using ReportBuilder
        var report_builder = check.report.ReportBuilder.init(
            allocator,
            result.module_env,
            type_can_ir,
            &solver.snapshots,
            &solver.problems,
            "main.roc",
            imported_envs,
            &solver.import_mapping,
            &solver.regions,
            null,
        ) catch |err| {
            // On allocation failure, return result with current reports
            logDebug("compileSource: ReportBuilder.init failed: {}\n", .{err});
            return result;
        };
        defer report_builder.deinit();

        for (solver.problems.problems.items) |type_problem| {
            const report = report_builder.build(type_problem) catch |build_err| {
                logDebug("compileSource: report_builder.build failed: {}\n", .{build_err});
                return build_err;
            };
            result.type_reports.append(report) catch |append_err| {
                logDebug("compileSource: append TYPE report failed: {}\n", .{append_err});
                return append_err;
            };
        }
    }

    logDebug("compileSource: Compilation complete\n", .{});
    return result;
}

/// Writer that tracks bytes written and fails if buffer is exceeded.
const ResponseWriter = struct {
    buffer: []u8,
    pos: usize = 0,
    interface: std.Io.Writer,

    const Self = @This();
    pub const Error = ResponseWriteError;

    fn init(buffer: []u8) Self {
        var result = Self{
            .buffer = buffer,
            .pos = 0,
            .interface = undefined,
        };
        result.interface = .{
            .vtable = &.{
                .drain = drain,
            },
            .buffer = &.{}, // Use internal buffer tracking
        };
        return result;
    }

    fn drain(w: *std.Io.Writer, data: []const []const u8, _: usize) std.Io.Writer.Error!usize {
        const self: *Self = @alignCast(@fieldParentPtr("interface", w));
        var total: usize = 0;
        for (data) |bytes| {
            const n = self.write(bytes) catch return std.Io.Writer.Error.WriteFailed;
            total += n;
        }
        return total;
    }

    fn write(self: *Self, bytes: []const u8) Error!usize {
        if (self.pos + bytes.len > self.buffer.len) {
            return error.OutOfBufferSpace;
        }
        @memcpy(self.buffer[self.pos..][0..bytes.len], bytes);
        self.pos += bytes.len;
        return bytes.len;
    }

    /// Finalizes the response by writing the length prefix at the beginning.
    /// This assumes all data has been written *after* the length prefix space.
    fn finalize(self: *Self) ResponseWriteError!void {
        if (self.pos < @sizeOf(u32)) return error.OutOfBufferSpace; // Not enough space for length prefix itself
        const data_len = self.pos - @sizeOf(u32);
        std.mem.writeInt(u32, self.buffer[0..@sizeOf(u32)], @intCast(data_len), .little);
    }
};

/// Write an error response.
fn writeErrorResponse(response_buffer: []u8, status: ResponseStatus, message: []const u8) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    // Advance past length prefix, will be written by finalize
    resp_writer.pos = @sizeOf(u32);

    const w = &resp_writer.interface;
    try w.print("{{\"status\":\"{s}\",\"message\":\"", .{status.toString()});
    try writeJsonString(w, message);
    try w.writeAll("\"}");

    try resp_writer.finalize();
}

/// Write a success response
fn writeSuccessResponse(response_buffer: []u8, message: []const u8, data: ?[]const u8) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);

    const w = &resp_writer.interface;
    try w.writeAll("{\"status\":\"SUCCESS\",\"message\":\"");
    try writeJsonString(w, message);
    try w.writeAll("\"");

    if (data) |d| {
        try w.writeAll(",\"data\":");
        try w.writeAll(d);
    }

    try w.writeAll("}");

    try resp_writer.finalize();
}

/// Write response for LOADED state with diagnostics
fn writeLoadedResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);

    const w = &resp_writer.interface;

    // TIER 1: Extract diagnostics for VISUAL INDICATORS (gutter markers, squiggly lines)
    var diagnostics = std.array_list.Managed(Diagnostic).init(allocator);
    defer diagnostics.deinit();
    try extractDiagnosticsFromReports(&diagnostics, data.tokenize_reports);
    try extractDiagnosticsFromReports(&diagnostics, data.parse_reports);
    try extractDiagnosticsFromReports(&diagnostics, data.can_reports);
    try extractDiagnosticsFromReports(&diagnostics, data.type_reports);

    // TIER 2: Count ALL diagnostics from reports (for SUMMARY display)
    var total_errors: u32 = 0;
    var total_warnings: u32 = 0;
    const tokenize_counts = countDiagnostics(data.tokenize_reports.items);
    const parse_counts = countDiagnostics(data.parse_reports.items);
    const can_counts = countDiagnostics(data.can_reports.items);
    const type_counts = countDiagnostics(data.type_reports.items);
    total_errors += tokenize_counts.errors + parse_counts.errors + can_counts.errors + type_counts.errors;
    total_warnings += tokenize_counts.warnings + parse_counts.warnings + can_counts.warnings + type_counts.warnings;

    try w.writeAll("{\"status\":\"SUCCESS\",\"message\":\"LOADED\",\"diagnostics\":{");
    try w.print("\"summary\":{{\"errors\":{},\"warnings\":{}}},", .{ total_errors, total_warnings });
    try w.print("\"debug_counts\":{{\"tokenize\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}},\"parse\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}},\"can\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}},\"type\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}}}},", .{ tokenize_counts.errors, tokenize_counts.warnings, data.tokenize_reports.items.len, parse_counts.errors, parse_counts.warnings, data.parse_reports.items.len, can_counts.errors, can_counts.warnings, data.can_reports.items.len, type_counts.errors, type_counts.warnings, data.type_reports.items.len });

    try w.writeAll("\"list\":[");
    if (diagnostics.items.len > 0) {
        for (diagnostics.items, 0..) |diagnostic, i| {
            if (i > 0) try w.writeAll(",");
            try writeDiagnosticJson(w, diagnostic);
        }
    }
    try w.writeAll("],");

    try w.writeAll("\"html\":\"");

    // Collect HTML in a buffer first, then escape it for JSON
    var html_buffer: [65536]u8 = undefined;
    var html_writer = std.Io.Writer.fixed(&html_buffer);

    if (data.tokenize_reports.items.len > 0) {
        for (data.tokenize_reports.items) |report| {
            writeDiagnosticHtml(&html_writer, report) catch return error.OutOfBufferSpace;
        }
    }
    if (data.parse_reports.items.len > 0) {
        for (data.parse_reports.items) |report| {
            writeDiagnosticHtml(&html_writer, report) catch return error.OutOfBufferSpace;
        }
    }
    if (data.can_reports.items.len > 0) {
        for (data.can_reports.items) |report| {
            writeDiagnosticHtml(&html_writer, report) catch return error.OutOfBufferSpace;
        }
    }
    if (data.type_reports.items.len > 0) {
        for (data.type_reports.items) |report| {
            writeDiagnosticHtml(&html_writer, report) catch return error.OutOfBufferSpace;
        }
    }

    const html_content = html_writer.buffer[0..html_writer.end];
    try writeJsonString(w, html_content);
    try w.writeAll("\"}}");

    try resp_writer.finalize();
}

/// Write REPL initialization response
fn writeReplInitResponse(response_buffer: []u8) ResponseWriteError!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"message\":\"REPL initialized\",\"repl_info\":{");
    try w.print("\"compiler_version\":\"{s}\",", .{build_options.compiler_version});
    try w.writeAll("\"state\":\"REPL_ACTIVE\"");
    try w.writeAll("}}");

    try resp_writer.finalize();
}

/// Write REPL step result as JSON
fn writeReplStepResultJson(response_buffer: []u8, result: ReplStepResult) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"result\":{");

    // Output field
    try w.writeAll("\"output\":\"");
    try writeJsonString(w, result.output);
    try w.writeAll("\"");

    // Type field (using enum's jsonStringify)
    try w.writeAll(",\"type\":");
    try result.try_type.jsonStringify(w);

    // Error-specific fields
    if (result.error_stage) |stage| {
        try w.writeAll(",\"error_stage\":");
        try stage.jsonStringify(w);
    }

    if (result.error_details) |details| {
        try w.writeAll(",\"error_details\":\"");
        try writeJsonString(w, details);
        try w.writeAll("\"");
    }

    // Compiler availability
    try w.print(",\"compiler_available\":{}", .{result.compiler_available});

    try w.writeAll("}}");
    try resp_writer.finalize();
}

/// Write REPL clear response
fn writeReplClearResponse(response_buffer: []u8) ResponseWriteError!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"message\":\"REPL cleared\",\"repl_info\":{");
    try w.print("\"compiler_version\":\"{s}\",", .{build_options.compiler_version});
    try w.writeAll("\"state\":\"REPL_ACTIVE\"");
    try w.writeAll("}}");

    try resp_writer.finalize();
}

/// Write tokens response with direct HTML generation
fn writeTokensResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    if (data.tokens_html) |html| {
        try writeJsonString(w, html);
    } else {
        try writeJsonString(w, "Tokens not available");
    }

    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write parse AST response in S-expression format
fn writeParseAstResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    if (data.ast_html) |html| {
        try writeJsonString(w, html);
    } else {
        try writeJsonString(w, "Parse AST not available");
    }

    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write formatted response with formatted Roc code
fn writeFormattedResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    if (data.formatted_code) |formatted| {
        try writeJsonString(w, formatted);
    } else {
        try writeJsonString(w, "Formatted code not available");
    }

    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write canonicalized CIR response in S-expression format
fn writeCanCirResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    const cir = data.module_env;
    var local_arena = base.SingleThreadArena.init(allocator);
    defer local_arena.deinit();
    var sexpr_writer_allocating: std.Io.Writer.Allocating = .init(local_arena.allocator());

    var tree = SExprTree.init(local_arena.allocator());
    defer tree.deinit();

    const defs_count = cir.store.sliceDefs(cir.all_defs).len;
    const stmts_count = cir.store.sliceStatements(cir.all_statements).len;

    if (defs_count == 0 and stmts_count == 0) {
        const debug_begin = tree.beginNode();
        try tree.pushStaticAtom("empty-cir-debug");
        try tree.pushStaticAtom("no-defs-or-statements");
        const debug_attrs = tree.beginNode();
        try tree.endNode(debug_begin, debug_attrs);
    }

    const mutable_cir = @constCast(cir);
    try ModuleEnv.pushToSExprTree(mutable_cir, null, &tree);
    tree.toHtml(&sexpr_writer_allocating.writer, .include_linecol) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.WriteFailed,
    };
    try sexpr_writer_allocating.writer.flush();

    try writeJsonString(w, sexpr_writer_allocating.written());
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

fn collectPlaygroundTestRootRequests(
    alloc: Allocator,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) Allocator.Error![]check.CheckedArtifact.RootRequest {
    var roots = std.ArrayList(check.CheckedArtifact.RootRequest).empty;
    errdefer roots.deinit(alloc);

    for (artifact.root_requests.requests) |root| {
        if (root.kind != .test_expect) continue;
        try roots.append(alloc, root);
    }

    return try roots.toOwnedSlice(alloc);
}

fn argLayoutsForProc(
    alloc: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) Allocator.Error![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try alloc.alloc(layout.Idx, arg_ids.len);
    errdefer alloc.free(arg_layouts);

    for (arg_ids, 0..) |local_id, i| {
        arg_layouts[i] = store.getLocal(local_id).layout_idx;
    }

    return arg_layouts;
}

fn buildEvaluateTestsHtml(data: CompilerStageData) PlaygroundEvaluateTestsError![]u8 {
    var resources = try eval.test_helpers.parseAndCanonicalizeProgramPublishedRoots(
        allocator,
        .module,
        data.module_env.common.source,
        &.{},
    );
    defer eval.test_helpers.cleanupParseAndCanonical(allocator, resources);

    const test_roots = try collectPlaygroundTestRootRequests(allocator, &resources.checked_artifact);
    defer allocator.free(test_roots);

    var html_writer_allocating: std.Io.Writer.Allocating = .init(allocator);
    errdefer html_writer_allocating.deinit();
    const html_writer = &html_writer_allocating.writer;

    try html_writer.writeAll("<div class=\"test-results\">");
    if (test_roots.len == 0) {
        try html_writer.writeAll("<p>No tests found</p></div>");
        try html_writer.flush();
        return html_writer_allocating.toOwnedSlice();
    }

    var import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, resources.import_artifacts.len);
    defer allocator.free(import_views);
    for (resources.import_artifacts, 0..) |*artifact, i| {
        import_views[i] = check.CheckedArtifact.importedView(artifact);
    }

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = test_roots },
        .{
            .target_usize = .u32,
        },
    );
    defer lowered.deinit();

    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interpreter = try eval.LirInterpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interpreter.deinit();

    var passed: u32 = 0;
    var failed: u32 = 0;
    try html_writer.writeAll("<ul>");
    for (lowered.lir_result.root_procs.items, lowered.lir_result.root_metadata.items) |root_proc, metadata| {
        if (metadata.kind != .test_expect) continue;

        const proc = lowered.lir_result.store.getProcSpec(root_proc);
        const arg_layouts = try argLayoutsForProc(allocator, &lowered.lir_result.store, root_proc);
        defer allocator.free(arg_layouts);

        const eval_result = interpreter.eval(.{
            .proc_id = root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = proc.ret_layout,
        }) catch |err| {
            failed += 1;
            try html_writer.print("<li class=\"failed\">FAILED: {s}</li>", .{@errorName(err)});
            continue;
        };

        const ok = switch (eval_result) {
            .value => |value| blk: {
                const result = value.read(u8) != 0;
                interpreter.dropValue(value, proc.ret_layout);
                break :blk result;
            },
        };

        if (ok) {
            passed += 1;
            try html_writer.writeAll("<li class=\"passed\">PASSED</li>");
        } else {
            failed += 1;
            try html_writer.writeAll("<li class=\"failed\">FAILED</li>");
        }
    }
    try html_writer.writeAll("</ul>");
    try html_writer.print("<p>{} passed, {} failed</p></div>", .{ passed, failed });
    try html_writer.flush();

    return html_writer_allocating.toOwnedSlice();
}

fn writeEvaluateTestsResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    const html = buildEvaluateTestsHtml(data) catch |err| {
        try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
        return;
    };
    defer allocator.free(html);

    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");
    try writeJsonString(w, html);
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

const HoverInfo = struct {
    name: []const u8,
    type_str: []const u8,
    definition_region: DiagnosticRegion,
    docs: ?[]const u8,

    pub fn deinit(self: *HoverInfo, alloc: Allocator) void {
        alloc.free(self.type_str);
        if (self.docs) |d| {
            alloc.free(d);
        }
    }
};

/// Write hover info response for a specific position
fn writeHoverInfoResponse(response_buffer: []u8, data: CompilerStageData, message_json: std.json.Value) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    const line_val = message_json.object.get("line") orelse {
        try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing line parameter");
        return;
    };
    const ch_val = message_json.object.get("ch") orelse {
        try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing ch parameter");
        return;
    };
    const identifier_val = message_json.object.get("identifier") orelse {
        try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing identifier parameter");
        return;
    };

    const line_num = switch (line_val) {
        .integer => |i| @as(u32, @intCast(i)) - 1, // Convert from 1-based to 0-based index
        else => {
            try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid line parameter");
            return;
        },
    };
    const ch_num = switch (ch_val) {
        .integer => |i| @as(u32, @intCast(i)) - 1, // Convert from 1-based to 0-based
        else => {
            try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid ch parameter");
            return;
        },
    };
    const ident_str = switch (identifier_val) {
        .string => |s| s,
        else => {
            try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid identifier parameter");
            return;
        },
    };

    if (data.solver == null) {
        try writeErrorResponse(response_buffer, .ERROR, "Type checking not completed.");
        return;
    }

    const source = data.module_env.common.source;
    const line_starts = data.module_env.common.line_starts.items.items;

    if (line_num >= line_starts.len) {
        try writeErrorResponse(response_buffer, .ERROR, "Line number out of range");
        return;
    }
    const line_start = line_starts[line_num];
    const byte_offset = line_start + ch_num;

    if (byte_offset >= source.len) {
        try writeErrorResponse(response_buffer, .ERROR, "Position out of range");
        return;
    }

    var maybe_hover_info = findHoverInfoAtPosition(data, byte_offset, ident_str) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {
            try writeErrorResponse(response_buffer, .ERROR, "Failed to find hover information");
            return;
        },
    };

    try w.writeAll("{\"status\":\"SUCCESS\",\"hover_info\":");
    if (maybe_hover_info) |*hover_info| {
        defer hover_info.deinit(allocator);

        try w.writeAll("{\"name\":\"");
        try writeJsonString(w, hover_info.name);
        try w.writeAll("\",\"type_str\":\"");
        try writeJsonString(w, hover_info.type_str);
        try w.writeAll("\",\"definition_region\":{");
        try w.print("\"start_line\":{d},\"start_column\":{d},\"end_line\":{d},\"end_column\":{d}", .{
            hover_info.definition_region.start_line,
            hover_info.definition_region.start_column,
            hover_info.definition_region.end_line,
            hover_info.definition_region.end_column,
        });
        try w.writeAll("}");
        try w.writeAll(",\"docs\":");
        if (hover_info.docs) |docs| {
            try w.writeAll("\"");
            try writeJsonString(w, docs);
            try w.writeAll("\"");
        } else {
            try w.writeAll("null");
        }
        try w.writeAll("}"); // closes hover_info
    } else {
        try w.writeAll("null");
    }
    try w.writeAll("}"); // closes root
    try resp_writer.finalize();
}

/// Find hover information for an identifier at a specific byte position
fn findHoverInfoAtPosition(data: CompilerStageData, byte_offset: u32, identifier: []const u8) (Allocator.Error || error{WriteFailed})!?HoverInfo {
    const cir = data.module_env;
    const local_allocator = allocator;

    const all_defs = cir.store.sliceDefs(cir.all_defs);

    for (all_defs) |def_idx| {
        const def = cir.store.getDef(def_idx);
        const pattern_region = cir.store.getPatternRegion(def.pattern);

        if (byte_offset >= pattern_region.start.offset and byte_offset < pattern_region.end.offset) {
            const pattern = cir.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |assign| {
                    const ident_text = cir.getIdent(assign.ident);
                    if (std.mem.eql(u8, ident_text, identifier)) {
                        // 1. Get type string
                        const owned_type_str = if (def.annotation) |annotation_idx| blk: {
                            const annotation = cir.store.getAnnotation(annotation_idx);
                            const anno_region = cir.store.getTypeAnnoRegion(annotation.anno);
                            break :blk try local_allocator.dupe(u8, cir.getSource(anno_region));
                        } else blk: {
                            var type_writer = try data.module_env.initTypeWriter();
                            defer type_writer.deinit();

                            try type_writer.write(ModuleEnv.varFrom(def.pattern), .wrap);
                            break :blk try local_allocator.dupe(u8, type_writer.get());
                        };

                        // 2. Get definition region
                        const def_region_loc = cir.store.getPatternRegion(def.pattern);
                        const region_info = cir.calcRegionInfo(def_region_loc);
                        const def_region = DiagnosticRegion{
                            .start_line = region_info.start_line_idx + 1,
                            .start_column = region_info.start_col_idx + 1,
                            .end_line = region_info.end_line_idx + 1,
                            .end_column = region_info.end_col_idx + 1,
                        };

                        // 3. Get docs (not yet implemented)
                        const docs: ?[]const u8 = null;

                        return HoverInfo{
                            .name = ident_text,
                            .type_str = owned_type_str,
                            .definition_region = def_region,
                            .docs = docs,
                        };
                    }
                },
                else => {},
            }
        }
    }
    return null;
}

/// Write types response in S-expression format
fn writeTypesResponse(response_buffer: []u8, data: CompilerStageData) (Allocator.Error || error{ OutOfBufferSpace, WriteFailed })!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    if (data.solver == null) {
        try writeErrorResponse(response_buffer, .ERROR, "Type checking not completed.");
        return;
    }

    const cir = data.module_env;
    var local_arena = base.SingleThreadArena.init(allocator);
    defer local_arena.deinit();
    var sexpr_writer_allocating: std.Io.Writer.Allocating = .init(local_arena.allocator());
    var tree = SExprTree.init(local_arena.allocator());
    defer tree.deinit();

    const mutable_cir = @constCast(cir);
    mutable_cir.pushTypesToSExprTree(null, &tree) catch |err| {
        const error_msg = switch (err) {
            error.OutOfMemory => "Out of memory while generating types",
            error.WriteFailed => "Write failed while generating types",
        };
        try writeErrorResponse(response_buffer, .ERROR, error_msg);
        return;
    };
    tree.toHtml(&sexpr_writer_allocating.writer, .include_linecol) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.WriteFailed,
    };
    try sexpr_writer_allocating.writer.flush();

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");
    try writeJsonString(w, sexpr_writer_allocating.written());
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write a diagnostic as JSON
fn writeDiagnosticHtml(writer: *std.Io.Writer, report: reporting.Report) (Allocator.Error || error{WriteFailed})!void {
    try reporting.renderReportToHtml(&report, writer, reporting.ReportingConfig.initHtml());
}

fn countDiagnostics(reports: []reporting.Report) struct { errors: u32, warnings: u32 } {
    var errors: u32 = 0;
    var warnings: u32 = 0;
    for (reports) |report| {
        switch (report.severity) {
            .info => {},
            .warning => warnings += 1,
            .runtime_error, .fatal => errors += 1,
        }
    }
    return .{ .errors = errors, .warnings = warnings };
}

fn extractDiagnosticsFromReports(
    diagnostics: *std.array_list.Managed(Diagnostic),
    reports: std.array_list.Managed(reporting.Report),
) Allocator.Error!void {
    var count: usize = 0;
    const max_diagnostics = 100;
    for (reports.items) |*report| {
        if (count >= max_diagnostics) break;
        const region_info = report.getRegionInfo() orelse continue;
        if (region_info.start_line_idx == 0 and region_info.start_col_idx == 0 and
            region_info.end_line_idx == 0 and region_info.end_col_idx == 0)
        {
            continue;
        }
        // Titles are authored in title case; shout them to ALL CAPS here to
        // match the box/HTML/LSP renderers. Owned by the report so the message
        // lives as long as the borrowed title would have.
        const message = try report.addOwnedString(report.title);
        for (@constCast(message)) |*c| c.* = std.ascii.toUpper(c.*);
        const diagnostic_severity = switch (report.severity) {
            .info => DiagnosticSeverity.info,
            .warning => DiagnosticSeverity.warning,
            .runtime_error => DiagnosticSeverity.@"error",
            .fatal => DiagnosticSeverity.@"error",
        };
        try diagnostics.append(Diagnostic{
            .severity = diagnostic_severity,
            .message = message,
            .region = DiagnosticRegion{
                .start_line = region_info.start_line_idx,
                .start_column = region_info.start_col_idx,
                .end_line = region_info.end_line_idx,
                .end_column = region_info.end_col_idx,
            },
        });
        count += 1;
    }
}

fn writeDiagnosticJson(writer: anytype, diagnostic: Diagnostic) error{WriteFailed}!void {
    try writer.print("{{\"severity\":\"{s}\",\"message\":\"", .{@tagName(diagnostic.severity)});
    try writeJsonString(writer, diagnostic.message);
    try writer.print("\",\"region\":{{\"start_line\":{d},\"start_column\":{d},\"end_line\":{d},\"end_column\":{d}}}}}", .{
        diagnostic.region.start_line, diagnostic.region.start_column,
        diagnostic.region.end_line,   diagnostic.region.end_column,
    });
}

/// Write a string with JSON escaping (without surrounding quotes)
fn writeJsonString(writer: *std.Io.Writer, str: []const u8) error{WriteFailed}!void {
    try std.json.Stringify.encodeJsonStringChars(str, .{}, writer);
}

/// Processes a message and returns a pointer to a null-terminated, dynamically allocated JSON response string.
/// The caller (host) is responsible for freeing this string via `freeWasmString`.
/// The returned pointer points to the start of the JSON data. The actual allocation includes a
/// length prefix, so the host must use the custom `freeWasmString` function.
/// Returns null on failure.
export fn processAndRespond(message_ptr: [*]const u8, message_len: usize) ?[*:0]u8 {
    // Allocate a temporary buffer on the heap to avoid a stack overflow.
    var temp_response_buffer = allocator.alloc(u8, 131072) catch {
        return createSimpleErrorJson("Failed to allocate temporary response buffer");
    };
    defer allocator.free(temp_response_buffer);

    const result_code = processMessage(message_ptr, message_len, temp_response_buffer.ptr, temp_response_buffer.len);

    // If processMessage itself failed, create a dynamic error string using the length-prefix pattern.
    if (result_code != 0) {
        const error_message = switch (@as(WasmError, @enumFromInt(result_code))) {
            .invalid_json => "Invalid JSON message",
            .missing_message_type => "Missing message type",
            .unknown_message_type => "Unknown message type",
            .invalid_state_for_message => "Invalid state for message",
            .response_buffer_too_small => "Response buffer too small",
            .internal_error => "Internal WASM error during processing",
            .success => unreachable,
        };
        return createSimpleErrorJson(error_message);
    }

    // Extract the actual JSON response from the temp buffer (skip the length prefix)
    const response_len = std.mem.readInt(u32, temp_response_buffer[0..4], .little);
    const response_data_from_temp = temp_response_buffer[4..][0..response_len];

    // Handle invalid response length from processMessage
    if (response_len == 0 or response_len >= temp_response_buffer.len - 4) {
        return createSimpleErrorJson("Invalid response format from processMessage");
    }

    // Allocate the final buffer for the host, including space for our length prefix and null terminator.
    const total_len_for_final_alloc = @sizeOf(u32) + response_len + 1;
    const final_buffer = allocator.alloc(u8, total_len_for_final_alloc) catch {
        return createSimpleErrorJson("Failed to allocate final response buffer");
    };

    // Write the length prefix (length of the JSON data only) into the final buffer.
    std.mem.writeInt(u32, final_buffer[0..@sizeOf(u32)], @intCast(response_len), .little);

    // Define the destination for the data, right after the length prefix.
    const data_dest_slice = final_buffer[@sizeOf(u32)..];

    // Copy the JSON data from the temp buffer and null-terminate it.
    @memcpy(data_dest_slice[0..response_len], response_data_from_temp);
    final_buffer[@sizeOf(u32) + response_len] = 0;

    // Return a pointer to the start of the *data*, not the length prefix.
    const data_ptr: [*:0]u8 = @ptrCast(data_dest_slice.ptr);
    return data_ptr;
}

/// Frees a string that was allocated with a length-prefix by `processAndRespond`.
export fn freeWasmString(ptr: [*]u8) void {
    // The received pointer `ptr` points to the start of the data.
    // The actual allocation starts `sizeOf(u32)` bytes before it.
    const prefix_ptr = ptr - @sizeOf(u32);

    // Read the length as individual bytes to avoid alignment issues
    const json_len = std.mem.readInt(u32, @ptrCast(prefix_ptr), .little);

    // Calculate the total allocated size: [u32 length] + [data] + [u8 null terminator]
    const total_len = @sizeOf(u32) + json_len + 1;

    // Reconstruct the original slice that was allocated.
    const original_slice = prefix_ptr[0..total_len];

    // Free the original slice.
    allocator.free(original_slice);
}

/// Helper to create a simple error JSON string, following the length-prefix allocation pattern.
fn createSimpleErrorJson(error_message: []const u8) ?[*:0]u8 {
    // 1. Format the string into a temporary buffer to determine its length.
    var fmt_buf: [4096]u8 = undefined;
    const json_str = std.fmt.bufPrint(&fmt_buf, "{{\"status\":\"ERROR\",\"message\":\"{s}\"}}", .{error_message}) catch return null;
    const json_len = json_str.len;

    // 2. Allocate memory for [u32: length][u8...: data][u8: null terminator]
    const total_len = @sizeOf(u32) + json_len + 1;
    const final_buffer = allocator.alloc(u8, total_len) catch return null;

    // 3. Write the length prefix (the length of the JSON data only)
    std.mem.writeInt(u32, final_buffer[0..@sizeOf(u32)], @intCast(json_len), .little);

    // 4. Copy the JSON data
    const data_ptr = final_buffer.ptr + @sizeOf(u32);
    @memcpy(final_buffer[@sizeOf(u32)..][0..json_len], json_str);

    // 5. Null-terminate
    final_buffer[@sizeOf(u32) + json_len] = 0;

    // 6. Return a pointer to the start of the *data*, not the length prefix.
    return @ptrCast(data_ptr);
}

/// Get current state for debugging
export fn getCurrentState() u32 {
    return switch (current_state) {
        .START => 0,
        .READY => 1,
        .LOADED => 2,
        .REPL_ACTIVE => 3,
    };
}

/// Placeholder function to validate a base58 hash string (for bundle/unbundle functionality)
/// (This is just to make sure the unbundle module is successfully linked in the playground build,
/// so that when we're ready to actually use it, we know it will be building successfully on wasm.)
export fn validateBase58Hash(hash_ptr: [*]const u8, hash_len: usize) bool {
    const hash_str = hash_ptr[0..hash_len];

    // Try to validate the hash using unbundle's validation function
    const result = unbundle.validateBase58Hash(hash_str) catch return false;

    // If we got a valid hash back, return 1
    if (result) |_| {
        return true;
    } else {
        return false;
    }
}

/// Unbundle a tar.zst archive from memory, returning extracted files as JSON.
/// The response is a JSON object with either:
/// - {"success": true, "files": [{"path": "...", "content": "..."}], "directories": ["..."]}
/// - {"success": false, "error": "..."}
///
/// Returns 0 on success, 1 on error (response buffer too small), 2 on unbundle error.
export fn unbundleToBuffer(
    compressed_ptr: [*]const u8,
    compressed_len: usize,
    hash_ptr: [*]const u8, // 32-byte BLAKE3 hash
    response_ptr: [*]u8,
    response_len: usize,
) u8 {
    const compressed = compressed_ptr[0..compressed_len];
    const expected_hash = hash_ptr[0..32].*;

    // Create a fixed buffer reader from the compressed data
    var fixed_reader = std.Io.Reader.fixed(compressed);

    // Create buffer extract writer for in-memory extraction
    var buffer_writer = unbundle.BufferExtractWriter.init(allocator);
    defer buffer_writer.deinit();

    // Perform unbundling
    _ = unbundle.unbundleStream(
        allocator,
        &fixed_reader,
        buffer_writer.extractWriter(),
        &expected_hash,
        null,
        .{},
    ) catch |err| {
        // Write error response
        return writeUnbundleErrorResponse(response_ptr[0..response_len], err);
    };

    // Write success response with file list as JSON
    return writeUnbundleSuccessResponse(response_ptr[0..response_len], &buffer_writer);
}

fn writeUnbundleErrorResponse(response: []u8, err: unbundle.UnbundleError) u8 {
    const error_msg = switch (err) {
        error.DecompressionFailed => "Decompression failed",
        error.ExpandedSizeLimitExceeded => "Expanded size limit exceeded",
        error.InvalidTarHeader => "Invalid tar header",
        error.UnexpectedEndOfStream => "Unexpected end of stream",
        error.FileCreateFailed => "File create failed",
        error.DirectoryCreateFailed => "Directory create failed",
        error.FileWriteFailed => "File write failed",
        error.HashMismatch => "Hash mismatch - archive may be corrupted",
        error.InvalidFilename => "Invalid filename in archive",
        error.FileTooLarge => "File too large",
        error.InvalidPath => "Invalid path in archive",
        error.NoDataExtracted => "No data extracted from archive",
        error.ChecksumFailure => "Checksum failure",
        error.DictionaryIdFlagUnsupported => "Dictionary ID flag unsupported",
        error.MalformedBlock => "Malformed block in archive",
        error.MalformedFrame => "Malformed frame in archive",
        error.WriteFailed => "Write failed",
        error.ReadFailed => "Read failed",
        error.EndOfStream => "End of stream",
        error.OutOfMemory => "Out of memory",
    };

    _ = std.fmt.bufPrint(response, "{{\"success\":false,\"error\":\"{s}\"}}", .{error_msg}) catch {
        return 1; // Response buffer too small
    };
    return 2; // Unbundle error
}

fn writeUnbundleSuccessResponse(response: []u8, buffer_writer: *unbundle.BufferExtractWriter) u8 {
    var writer = std.Io.Writer.fixed(response);

    writer.writeAll("{\"success\":true,\"files\":[") catch return 1;

    var first_file = true;
    var iter = buffer_writer.files.iterator();
    while (iter.next()) |entry| {
        if (!first_file) {
            writer.writeAll(",") catch return 1;
        }
        first_file = false;

        writer.writeAll("{\"path\":") catch return 1;
        std.json.Stringify.encodeJsonString(entry.key_ptr.*, .{}, &writer) catch return 1;
        writer.writeAll(",\"content\":") catch return 1;
        std.json.Stringify.encodeJsonString(entry.value_ptr.items, .{}, &writer) catch return 1;
        writer.writeAll("}") catch return 1;
    }

    writer.writeAll("],\"directories\":[") catch return 1;

    var first_dir = true;
    for (buffer_writer.directories.items) |dir| {
        if (!first_dir) {
            writer.writeAll(",") catch return 1;
        }
        first_dir = false;

        std.json.Stringify.encodeJsonString(dir, .{}, &writer) catch return 1;
    }

    writer.writeAll("]}") catch return 1;
    return 0; // Success
}
