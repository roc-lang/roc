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
const base = @import("base");
const builtins = @import("builtins");
const build_options = @import("build_options");
const parse = @import("parse");
const reporting = @import("reporting");
const repl = @import("repl");
const eval = @import("eval");
const types = @import("types");
const compile = @import("compile");
const can = @import("can");
const check = @import("check");
const unbundle = @import("unbundle");
const fmt = @import("fmt");
const WasmFilesystem = @import("WasmFilesystem.zig");
const layout = @import("layout");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");

const CrashContext = eval.CrashContext;

const Can = can.Can;
const Check = check.Check;
const SExprTree = base.SExprTree;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const problem = check.problem;
const AST = parse.AST;
const Repl = repl.Repl;
const RocOps = builtins.host_abi.RocOps;
const TestRunner = eval.TestRunner;

// A fixed-size buffer to act as the heap inside the WASM linear memory.
var wasm_heap_memory: [64 * 1024 * 1024]u8 = undefined; // 64MB heap
var fba: std.heap.FixedBufferAllocator = undefined;
var allocator: Allocator = undefined;

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
    parse_ast: ?parse.AST = null,
    solver: ?Check = null,
    bool_stmt: ?can.CIR.Statement.Idx = null,
    builtin_types: ?eval.BuiltinTypes = null,

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
        if (self.parse_ast) |*ast| {
            ast.deinit(allocator);
        }

        // Finally, deinit the ModuleEnv and free its memory
        self.module_env.deinit();
        allocator.destroy(self.module_env);
    }
};

/// Global state machine
var current_state: State = .START;
var compiler_data: ?CompilerStageData = null;

/// REPL state management
const ReplSession = struct {
    repl: *Repl,
    crash_ctx: *CrashContext,
    roc_ops: *RocOps,
};

var repl_session: ?ReplSession = null;

/// REPL result types
const ReplResultType = enum {
    expression,
    definition,
    @"error",

    pub fn jsonStringify(self: ReplResultType, writer: anytype) !void {
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

    pub fn jsonStringify(self: ReplErrorStage, writer: anytype) !void {
        try writer.writeAll("\"");
        try writer.writeAll(@tagName(self));
        try writer.writeAll("\"");
    }
};

/// Structured REPL result
const ReplStepResult = struct {
    output: []const u8,
    result_type: ReplResultType,
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

/// Reset all global state and allocator
fn resetGlobalState() void {
    // Make sure everything is null
    compiler_data = null;
    cleanupReplState();
    host_message_buffer = null;
    host_response_buffer = null;

    // Reset allocator to clear all allocations
    fba.reset();
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
            @memcpy(target_slice[0..truncated_msg.len], truncated_msg);
            debug_log_pos += truncated_msg.len;
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
};

/// Clean up REPL state and free associated memory.
/// This function safely deallocates the REPL instance and RocOps, then sets them to null.
/// It's called during RESET operations and module initialization.
fn cleanupReplState() void {
    if (repl_session) |session| {
        session.repl.deinit();
        allocator.destroy(session.repl);
        allocator.destroy(session.roc_ops);
        session.crash_ctx.deinit();
        allocator.destroy(session.crash_ctx);
        repl_session = null;
    }
}

/// Create WASM-compatible RocOps for REPL initialization.
/// This function allocates and initializes a RocOps structure with WASM-specific
/// memory management functions. The returned pointer must be freed by the caller.
/// Returns an error if allocation fails.
fn createWasmRocOps(crash_ctx: *CrashContext) !*RocOps {
    const roc_ops = try allocator.create(RocOps);
    roc_ops.* = RocOps{
        .env = @as(*anyopaque, @ptrCast(crash_ctx)),
        .roc_alloc = wasmRocAlloc,
        .roc_dealloc = wasmRocDealloc,
        .roc_realloc = wasmRocRealloc,
        .roc_dbg = wasmRocDbg,
        .roc_expect_failed = wasmRocExpectFailed,
        .roc_crashed = wasmRocCrashed,
        .host_fns = undefined, // Not used in playground
    };
    return roc_ops;
}

fn wasmRocAlloc(alloc_args: *builtins.host_abi.RocAlloc, _: *anyopaque) callconv(.c) void {
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const result = allocator.rawAlloc(alloc_args.length, align_enum, @returnAddress());
    if (result) |ptr| {
        alloc_args.answer = ptr;
    } else {
        // In WASM, we can't use null pointers, so we'll just crash
        // This is a limitation of the WASM target
        unreachable;
    }
}

fn wasmRocDealloc(dealloc_args: *builtins.host_abi.RocDealloc, _: *anyopaque) callconv(.c) void {
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(dealloc_args.alignment)));
    // For WASM, we need to handle this carefully since we can't create slices from raw pointers
    // We'll use a dummy slice for now - this is a limitation of the WASM target
    const dummy_slice = @as([*]u8, @ptrCast(dealloc_args.ptr))[0..0];
    allocator.rawFree(dummy_slice, align_enum, @returnAddress());
}

fn wasmRocRealloc(realloc_args: *builtins.host_abi.RocRealloc, _: *anyopaque) callconv(.c) void {
    // For WASM, we'll just allocate new memory for now
    // A proper implementation would need to handle reallocation carefully
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(realloc_args.alignment)));
    const result = allocator.rawAlloc(realloc_args.new_length, align_enum, @returnAddress());
    if (result) |ptr| {
        realloc_args.answer = ptr;
    } else {
        // In WASM, we can't use null pointers, so we'll just crash
        // This is a limitation of the WASM target
        unreachable;
    }
}

fn wasmRocDbg(dbg_args: *const builtins.host_abi.RocDbg, _: *anyopaque) callconv(.c) void {
    // No-op in WASM playground
    _ = dbg_args;
}

fn wasmRocExpectFailed(expect_failed_args: *const builtins.host_abi.RocExpectFailed, _: *anyopaque) callconv(.c) void {
    // No-op in WASM playground
    _ = expect_failed_args;
}

fn wasmRocCrashed(crashed_args: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) void {
    const ctx: *CrashContext = @ptrCast(@alignCast(env));
    ctx.recordCrash(crashed_args.utf8_bytes[0..crashed_args.len]) catch |err| {
        std.debug.panic("failed to record crash in wasm playground: {}", .{err});
    };
}

/// Initialize the WASM module in START state
export fn init() void {
    // For the very first initialization, we can reset the allocator
    fba = std.heap.FixedBufferAllocator.init(&wasm_heap_memory);
    allocator = fba.allocator();

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
        error.WriteFailed => @intFromEnum(WasmError.internal_error),
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

            // Clean up previous compilation if any
            if (compiler_data) |*data| {
                data.deinit();
                compiler_data = null;
            }

            // Compile the source through all stages
            const result = compileSource(source) catch |err| {
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };

            compiler_data = result;
            current_state = .LOADED;

            // Return success with diagnostics
            try writeLoadedResponse(response_buffer, result);
        },
        .INIT_REPL => {
            // Clean up any existing REPL state
            cleanupReplState();

            const crash_ctx = allocator.create(CrashContext) catch |err| {
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };
            crash_ctx.* = CrashContext.init(allocator);

            const roc_ops = createWasmRocOps(crash_ctx) catch |err| {
                allocator.destroy(crash_ctx);
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };

            const repl_ptr = allocator.create(Repl) catch |err| {
                allocator.destroy(roc_ops);
                crash_ctx.deinit();
                allocator.destroy(crash_ctx);
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };

            repl_ptr.* = Repl.init(allocator, roc_ops, crash_ctx) catch |err| {
                allocator.destroy(roc_ops);
                crash_ctx.deinit();
                allocator.destroy(crash_ctx);
                allocator.destroy(repl_ptr);
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };

            repl_session = .{ .repl = repl_ptr, .crash_ctx = crash_ctx, .roc_ops = roc_ops };
            current_state = .REPL_ACTIVE;

            // Return success with REPL info
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
    const session = repl_session orelse {
        try writeErrorResponse(response_buffer, .ERROR, "REPL not initialized");
        return;
    };
    const repl_ptr = session.repl;
    const crash_ctx = session.crash_ctx;

    switch (message_type) {
        .REPL_STEP => {
            const input_value = root.object.get("input") orelse {
                try writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing input for REPL_STEP");
                return;
            };
            const input = input_value.string;

            const result = repl_ptr.step(input) catch |err| {
                // Handle hard errors (like OOM) that aren't caught by the REPL
                // Create a static error message to avoid allocation issues
                const error_msg = @errorName(err);
                const step_result = ReplStepResult{
                    .output = error_msg,
                    .result_type = .@"error",
                    .error_stage = .runtime,
                    .error_details = error_msg,
                };
                try writeReplStepResultJson(response_buffer, step_result);
                return;
            };
            defer allocator.free(result);

            if (crash_ctx.state == .crashed) {
                const crash_details = crash_ctx.crashMessage();
                crash_ctx.reset();

                const step_result = ReplStepResult{
                    .output = result,
                    .result_type = .@"error",
                    .error_stage = .evaluation,
                    .error_details = crash_details,
                };
                try writeReplStepResultJson(response_buffer, step_result);
                return;
            }

            // Parse the result to determine type and extract error information
            const step_result = parseReplResult(result);
            try writeReplStepResultJson(response_buffer, step_result);
        },
        .CLEAR_REPL => {
            // Clear REPL definitions but keep REPL active
            // Clear all definitions from the hashmap
            var iterator = repl_ptr.definitions.iterator();
            while (iterator.next()) |kv| {
                repl_ptr.allocator.free(kv.key_ptr.*);
                repl_ptr.allocator.free(kv.value_ptr.*);
            }
            repl_ptr.definitions.clearRetainingCapacity();
            try writeReplClearResponse(response_buffer);
        },
        .RESET => {
            resetGlobalState();

            current_state = .READY;

            const compiler_version = build_options.compiler_version;
            try writeSuccessResponse(response_buffer, compiler_version, null);
        },
        .QUERY_CIR => {
            // For REPL mode, we need to generate CIR from the REPL's last module env
            const module_env = repl_ptr.getLastModuleEnv() orelse {
                try writeErrorResponse(response_buffer, .ERROR, "No REPL evaluation has occurred yet");
                return;
            };

            // Write CIR response directly using the REPL's module env
            try writeReplCanCirResponse(response_buffer, module_env);
        },
        .QUERY_TYPES, .QUERY_FORMATTED, .GET_HOVER_INFO => {
            // These queries need parse/type information which isn't readily available in REPL mode
            try writeErrorResponse(response_buffer, .ERROR, "Parse/type queries not available in REPL mode");
        },
        else => {
            try writeErrorResponse(response_buffer, .INVALID_STATE, "Invalid message type for REPL state");
        },
    }
}

/// Compile source through all compiler stages.
fn compileSource(source: []const u8) !CompilerStageData {
    // Handle empty input gracefully to prevent crashes
    if (source.len == 0) {
        // Return empty compiler stage data for completely empty input
        var module_env = try allocator.create(ModuleEnv);

        module_env.* = try ModuleEnv.init(allocator, source);
        try module_env.common.calcLineStarts(module_env.gpa);
        return CompilerStageData.init(allocator, module_env);
    }

    const trimmed_source = std.mem.trim(u8, source, " \t\n\r");
    if (trimmed_source.len == 0) {
        // Return empty compiler stage data for whitespace-only input
        var module_env = try allocator.create(ModuleEnv);

        module_env.* = try ModuleEnv.init(allocator, source);
        try module_env.common.calcLineStarts(module_env.gpa);
        return CompilerStageData.init(allocator, module_env);
    }

    // Set up the source in WASM filesystem
    WasmFilesystem.setSource(allocator, source);

    logDebug("compileSource: Starting compilation (source len={})\n", .{source.len});

    // Initialize the ModuleEnv
    var module_env = try allocator.create(ModuleEnv);

    module_env.* = try ModuleEnv.init(allocator, source);
    try module_env.common.calcLineStarts(module_env.gpa);
    logDebug("compileSource: ModuleEnv initialized\n", .{});

    var result = CompilerStageData.init(allocator, module_env);

    // Stage 1: Parse (includes tokenization)
    logDebug("compileSource: Starting parse stage\n", .{});
    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    result.parse_ast = parse_ast;
    logDebug("compileSource: Parse complete\n", .{});

    // Generate and store HTML before canonicalization corrupts the AST/tokens
    logDebug("compileSource: Starting HTML generation\n", .{});
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    const temp_alloc = local_arena.allocator();

    // Generate Tokens HTML
    logDebug("compileSource: Generating tokens HTML\n", .{});
    var tokens_writer: std.Io.Writer.Allocating = .init(temp_alloc);
    AST.tokensToHtml(&parse_ast, &module_env.common, &tokens_writer.writer) catch |err| {
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
    try file.pushToSExprTree(module_env.gpa, &module_env.common, &parse_ast, &tree);

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
    fmt.formatAst(parse_ast, &formatted_code_buffer.writer) catch |err| {
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
        const report = parse_ast.tokenizeDiagnosticToReport(diagnostic, allocator) catch {
            // Log the error and continue processing other diagnostics
            // This prevents crashes on malformed diagnostics or empty input
            continue;
        };
        result.tokenize_reports.append(report) catch continue;
    }

    // Collect parse diagnostics with additional error handling
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        const report = parse_ast.parseDiagnosticToReport(&module_env.common, diagnostic, allocator, "main.roc") catch {
            // Log the error and continue processing other diagnostics
            // This prevents crashes on malformed diagnostics or empty input
            continue;
        };
        result.parse_reports.append(report) catch continue;
    }

    // Stage 2: Canonicalization (always run, even with parse errors)
    // The canonicalizer handles malformed parse nodes and continues processing
    const env = result.module_env;
    try env.initCIRFields(allocator, "main");

    // Load builtin modules and inject Bool and Result type declarations
    // (following the pattern from eval.zig and TestEnv.zig)
    const LoadedModule = struct {
        env: *ModuleEnv,
        buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
        gpa: Allocator,

        fn deinit(self: *@This()) void {
            self.env.imports.map.deinit(self.gpa);
            self.gpa.free(self.buffer);
            self.gpa.destroy(self.env);
        }

        fn loadCompiledModule(gpa: Allocator, bin_data: []const u8, module_name_param: []const u8, module_source: []const u8) !@This() {
            const CompactWriter = collections.CompactWriter;
            const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
            @memcpy(buffer, bin_data);

            logDebug("loadCompiledModule: bin_data.len={}, @sizeOf(ModuleEnv.Serialized)={}\n", .{ bin_data.len, @sizeOf(ModuleEnv.Serialized) });

            const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));

            // Log the raw all_statements value to see what we're reading
            logDebug("loadCompiledModule: raw all_statements.span.start={}, .len={}\n", .{
                serialized_ptr.all_statements.span.start,
                serialized_ptr.all_statements.span.len,
            });

            const module_env_ptr = try gpa.create(ModuleEnv);
            errdefer gpa.destroy(module_env_ptr);

            const base_ptr = @intFromPtr(buffer.ptr);

            logDebug("loadCompiledModule: About to deserialize common\n", .{});
            const deserialized_common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), module_source).*;
            logDebug("loadCompiledModule: common deserialized successfully\n", .{});

            logDebug("loadCompiledModule: About to deserialize types\n", .{});
            const deserialized_types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*;
            logDebug("loadCompiledModule: types deserialized successfully\n", .{});

            logDebug("loadCompiledModule: About to deserialize external_decls\n", .{});
            const deserialized_external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*;
            logDebug("loadCompiledModule: external_decls deserialized successfully\n", .{});

            logDebug("loadCompiledModule: About to deserialize imports\n", .{});
            const deserialized_imports = serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa).*;
            logDebug("loadCompiledModule: imports deserialized successfully\n", .{});

            logDebug("loadCompiledModule: About to deserialize store\n", .{});
            const deserialized_store_ptr = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa);
            const deserialized_store = deserialized_store_ptr.*;
            logDebug("loadCompiledModule: store deserialized successfully\n", .{});

            logDebug("loadCompiledModule: All deserialized, constructing ModuleEnv\n", .{});
            module_env_ptr.* = ModuleEnv{
                .gpa = gpa,
                .common = deserialized_common,
                .types = deserialized_types,
                .module_kind = serialized_ptr.module_kind,
                .all_defs = serialized_ptr.all_defs,
                .all_statements = serialized_ptr.all_statements,
                .exports = serialized_ptr.exports,
                .builtin_statements = serialized_ptr.builtin_statements,
                .external_decls = deserialized_external_decls,
                .imports = deserialized_imports,
                .module_name = module_name_param,
                .diagnostics = serialized_ptr.diagnostics,
                .store = deserialized_store,
                .evaluation_order = null,
            };
            logDebug("loadCompiledModule: ModuleEnv constructed successfully\n", .{});

            logDebug("loadCompiledModule: Returning LoadedModule\n", .{});
            return .{ .env = module_env_ptr, .buffer = buffer, .gpa = gpa };
        }
    };

    logDebug("compileSource: Loading builtin indices\n", .{});
    const builtin_indices = blk: {
        const aligned_buffer = try allocator.alignedAlloc(u8, @enumFromInt(@alignOf(can.CIR.BuiltinIndices)), compiled_builtins.builtin_indices_bin.len);
        defer allocator.free(aligned_buffer);
        @memcpy(aligned_buffer, compiled_builtins.builtin_indices_bin);
        const indices_ptr = @as(*const can.CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
        break :blk indices_ptr.*;
    };
    logDebug("compileSource: Builtin indices loaded, bool_type={}\n", .{@intFromEnum(builtin_indices.bool_type)});

    logDebug("compileSource: Loading Bool module\n", .{});
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try LoadedModule.loadCompiledModule(allocator, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    logDebug("compileSource: Bool module loaded\n", .{});

    logDebug("compileSource: Loading Result module\n", .{});
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try LoadedModule.loadCompiledModule(allocator, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();
    logDebug("compileSource: Result module loaded\n", .{});

    // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
    // Use builtin_indices directly - these are the correct statement indices
    logDebug("compileSource: Getting Bool and Result statement indices from builtin_indices\n", .{});
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const result_stmt_in_result_module = builtin_indices.result_type;

    logDebug("compileSource: Using Bool statement from Bool module, idx={}\n", .{@intFromEnum(bool_stmt_in_bool_module)});
    logDebug("compileSource: Using Result statement from Result module, idx={}\n", .{@intFromEnum(result_stmt_in_result_module)});
    logDebug("compileSource: Builtin injection complete\n", .{});

    // Store bool_stmt and builtin_types in result for later use (e.g., in test runner)
    result.bool_stmt = bool_stmt_in_bool_module;
    result.builtin_types = eval.BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("main")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .result_stmt = result_stmt_in_result_module,
    };

    // Create module_envs map for canonicalization (enables qualified calls)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs_map.deinit();
    const bool_ident = try module_env.insertIdent(base.Ident.for_text("Bool"));
    const result_ident = try module_env.insertIdent(base.Ident.for_text("Result"));
    try module_envs_map.put(bool_ident, .{ .env = bool_module.env });
    try module_envs_map.put(result_ident, .{ .env = result_module.env });

    logDebug("compileSource: Starting canonicalization\n", .{});
    var czer = try Can.init(env, &result.parse_ast.?, &module_envs_map);
    defer czer.deinit();

    czer.canonicalizeFile() catch |err| {
        logDebug("compileSource: canonicalizeFile failed: {}\n", .{err});
        if (err == error.OutOfMemory) {
            // If we're out of memory here, the state is likely unstable.
            // Propagate this error up to halt compilation gracefully.
            return err;
        }
    };

    czer.validateForChecking() catch |err| {
        logDebug("compileSource: validateForChecking failed: {}\n", .{err});
        if (err == error.OutOfMemory) {
            return err;
        }
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
        result.can_reports.append(report) catch continue;
    }

    // Stage 3: Type checking (always run if we have CIR, even with canonicalization errors)
    // The type checker works with malformed canonical nodes to provide partial type information
    logDebug("compileSource: Starting type checking\n", .{});
    {
        const type_can_ir = result.module_env;
        const empty_modules: []const *ModuleEnv = &.{};
        // Use pointer to the stored CIR to ensure solver references valid memory
        var solver = try Check.init(allocator, &type_can_ir.types, type_can_ir, empty_modules, &type_can_ir.store.regions, module_common_idents);
        result.solver = solver;

        solver.checkFile() catch |check_err| {
            logDebug("compileSource: checkFile failed: {}\n", .{check_err});
            if (check_err == error.OutOfMemory) {
                // OOM during type checking is critical.
                // Deinit solver and propagate error.
                solver.deinit();
                result.solver = null; // Prevent double deinit in CompilerStageData.deinit
                return check_err;
            }
        };
        logDebug("compileSource: Type checking complete\n", .{});

        // Collect type checking problems and convert them to reports using ReportBuilder
        var report_builder = problem.ReportBuilder.init(
            allocator,
            result.module_env,
            type_can_ir,
            &solver.snapshots,
            "main.roc",
            &.{}, // other_modules - empty for playground
        );
        defer report_builder.deinit();

        for (solver.problems.problems.items) |type_problem| {
            const report = report_builder.build(type_problem) catch |build_err| {
                logDebug("compileSource: report_builder.build failed: {}\n", .{build_err});
                if (build_err == error.OutOfMemory) return build_err;
                continue;
            };
            result.type_reports.append(report) catch |append_err| {
                logDebug("compileSource: append TYPE report failed: {}\n", .{append_err});
                if (append_err == error.OutOfMemory) return append_err;
            };
        }
    }

    logDebug("compileSource: Compilation complete\n", .{});
    return result;
}

/// Helper to write a response with a u32 length prefix.
fn writeResponseWithLength(response_buffer: []u8, json_payload: []const u8) ResponseWriteError!void {
    if (response_buffer.len < @sizeOf(u32) + json_payload.len) {
        return error.OutOfBufferSpace;
    }

    // Write length prefix (little-endian)
    std.mem.writeInt(u32, response_buffer[0..@sizeOf(u32)], @intCast(json_payload.len), .little);

    // Write JSON payload
    @memcpy(response_buffer[@sizeOf(u32)..][0..json_payload.len], json_payload);
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

    fn drain(w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        _ = splat;
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
fn writeErrorResponse(response_buffer: []u8, status: ResponseStatus, message: []const u8) ResponseWriteError!void {
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
fn writeSuccessResponse(response_buffer: []u8, message: []const u8, data: ?[]const u8) ResponseWriteError!void {
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
fn writeLoadedResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);

    const w = &resp_writer.interface;

    // TIER 1: Extract diagnostics for VISUAL INDICATORS (gutter markers, squiggly lines)
    var diagnostics = std.array_list.Managed(Diagnostic).init(allocator);
    defer diagnostics.deinit();
    extractDiagnosticsFromReports(&diagnostics, data.tokenize_reports) catch {};
    extractDiagnosticsFromReports(&diagnostics, data.parse_reports) catch {};
    extractDiagnosticsFromReports(&diagnostics, data.can_reports) catch {};
    extractDiagnosticsFromReports(&diagnostics, data.type_reports) catch {};

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
    var html_writer = std.io.Writer.fixed(&html_buffer);

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

/// Parse REPL result string to determine type and extract error information
fn parseReplResult(result: []const u8) ReplStepResult {
    // Check for known error patterns
    if (std.mem.startsWith(u8, result, "Parse error:")) {
        return ReplStepResult{
            .output = result,
            .result_type = .@"error",
            .error_stage = .parse,
            .error_details = if (result.len > 13) result[13..] else null,
        };
    } else if (std.mem.indexOf(u8, result, "Canonicalize") != null) {
        return ReplStepResult{
            .output = result,
            .result_type = .@"error",
            .error_stage = .canonicalize,
            .error_details = extractErrorDetails(result),
        };
    } else if (std.mem.indexOf(u8, result, "Type check") != null) {
        return ReplStepResult{
            .output = result,
            .result_type = .@"error",
            .error_stage = .typecheck,
            .error_details = extractErrorDetails(result),
        };
    } else if (std.mem.indexOf(u8, result, "Layout") != null) {
        return ReplStepResult{
            .output = result,
            .result_type = .@"error",
            .error_stage = .layout,
            .error_details = extractErrorDetails(result),
        };
    } else if (std.mem.startsWith(u8, result, "Evaluation error:")) {
        return ReplStepResult{
            .output = result,
            .result_type = .@"error",
            .error_stage = .evaluation,
            .error_details = if (result.len > 17) result[17..] else null,
        };
    } else if (std.mem.indexOf(u8, result, "Interpreter") != null) {
        return ReplStepResult{
            .output = result,
            .result_type = .@"error",
            .error_stage = .interpreter,
            .error_details = extractErrorDetails(result),
        };
    } else if (std.mem.startsWith(u8, result, "assigned")) {
        // Definition success
        return ReplStepResult{
            .output = result,
            .result_type = .definition,
        };
    } else {
        // Expression result
        return ReplStepResult{
            .output = result,
            .result_type = .expression,
        };
    }
}

/// Extract error details from an error message (part after ": ")
fn extractErrorDetails(message: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, message, ": ")) |idx| {
        return message[idx + 2 ..];
    }
    return null;
}

/// Write REPL step result as JSON
fn writeReplStepResultJson(response_buffer: []u8, result: ReplStepResult) ResponseWriteError!void {
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
    try result.result_type.jsonStringify(w);

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
fn writeTokensResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
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
fn writeParseAstResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
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
fn writeFormattedResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
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

/// Write canonicalized CIR response for REPL mode using ModuleEnv directly
fn writeReplCanCirResponse(response_buffer: []u8, module_env: *ModuleEnv) ResponseWriteError!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    var sexpr_writer_allocating: std.Io.Writer.Allocating = .init(local_arena.allocator());
    var tree = SExprTree.init(local_arena.allocator());
    defer tree.deinit();

    const defs_count = module_env.store.sliceDefs(module_env.all_defs).len;
    const stmts_count = module_env.store.sliceStatements(module_env.all_statements).len;

    if (defs_count == 0 and stmts_count == 0) {
        const debug_begin = tree.beginNode();
        tree.pushStaticAtom("empty-cir-debug") catch {};
        tree.pushStaticAtom("no-defs-or-statements") catch {};
        const debug_attrs = tree.beginNode();
        tree.endNode(debug_begin, debug_attrs) catch {};
    }

    const mutable_cir = @constCast(module_env);
    ModuleEnv.pushToSExprTree(mutable_cir, null, &tree) catch {};
    tree.toHtml(&sexpr_writer_allocating.writer, .include_linecol) catch {};
    sexpr_writer_allocating.writer.flush() catch {};

    try writeJsonString(w, sexpr_writer_allocating.written());
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write canonicalized CIR response in S-expression format
fn writeCanCirResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    const cir = data.module_env;
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    var sexpr_writer_allocating: std.Io.Writer.Allocating = .init(local_arena.allocator());

    var tree = SExprTree.init(local_arena.allocator());
    defer tree.deinit();

    const defs_count = cir.store.sliceDefs(cir.all_defs).len;
    const stmts_count = cir.store.sliceStatements(cir.all_statements).len;

    if (defs_count == 0 and stmts_count == 0) {
        const debug_begin = tree.beginNode();
        tree.pushStaticAtom("empty-cir-debug") catch {};
        tree.pushStaticAtom("no-defs-or-statements") catch {};
        const debug_attrs = tree.beginNode();
        tree.endNode(debug_begin, debug_attrs) catch {};
    }

    const mutable_cir = @constCast(cir);
    ModuleEnv.pushToSExprTree(mutable_cir, null, &tree) catch {};
    tree.toHtml(&sexpr_writer_allocating.writer, .include_linecol) catch {};
    sexpr_writer_allocating.writer.flush() catch {};

    try writeJsonString(w, sexpr_writer_allocating.written());
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

fn writeEvaluateTestsResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {

    // use arena for test evaluation
    const env = data.module_env;
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();

    // Check if builtin_types is available
    const builtin_types_for_tests = data.builtin_types orelse {
        try writeErrorResponse(response_buffer, .ERROR, "Builtin types not available for test evaluation.");
        return;
    };

    // Create interpreter infrastructure for test evaluation
    var test_runner = TestRunner.init(local_arena.allocator(), env, builtin_types_for_tests) catch {
        try writeErrorResponse(response_buffer, .ERROR, "Failed to initialize test runner.");
        return;
    };
    defer test_runner.deinit();

    _ = test_runner.eval_all() catch {
        try writeErrorResponse(response_buffer, .ERROR, "Failed to evaluate tests.");
        return;
    };

    var html_writer_allocating: std.Io.Writer.Allocating = .init(local_arena.allocator());

    test_runner.write_html_report(&html_writer_allocating.writer) catch {
        try writeErrorResponse(response_buffer, .ERROR, "Failed to generate test report.");
        return;
    };

    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    try writeJsonString(w, html_writer_allocating.written());

    try w.writeAll("\"}");
    try resp_writer.finalize();
    return;
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
fn writeHoverInfoResponse(response_buffer: []u8, data: CompilerStageData, message_json: std.json.Value) ResponseWriteError!void {
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

    var maybe_hover_info = findHoverInfoAtPosition(data, byte_offset, ident_str) catch {
        try writeErrorResponse(response_buffer, .ERROR, "Failed to find hover information");
        return;
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
fn findHoverInfoAtPosition(data: CompilerStageData, byte_offset: u32, identifier: []const u8) !?HoverInfo {
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
                        var type_writer = try data.module_env.initTypeWriter();
                        defer type_writer.deinit();

                        const def_var = @as(types.Var, @enumFromInt(@intFromEnum(def_idx)));
                        try type_writer.write(def_var);
                        const type_str_from_writer = type_writer.get();
                        const owned_type_str = try local_allocator.dupe(u8, type_str_from_writer);

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
fn writeTypesResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
    var resp_writer = ResponseWriter.init(response_buffer);
    resp_writer.pos = @sizeOf(u32);
    const w = &resp_writer.interface;

    if (data.solver == null) {
        try writeErrorResponse(response_buffer, .ERROR, "Type checking not completed.");
        return;
    }

    const cir = data.module_env;
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    var sexpr_writer_allocating: std.Io.Writer.Allocating = .init(local_arena.allocator());
    var tree = SExprTree.init(local_arena.allocator());
    defer tree.deinit();

    const mutable_cir = @constCast(cir);
    mutable_cir.pushTypesToSExprTree(null, &tree) catch |err| {
        const error_msg = switch (err) {
            error.OutOfMemory => "Out of memory while generating types",
            // Add other specific error messages if pushTypesToSExprTree can return other errors
        };
        try writeErrorResponse(response_buffer, .ERROR, error_msg);
        return;
    };
    tree.toHtml(&sexpr_writer_allocating.writer, .include_linecol) catch {};
    sexpr_writer_allocating.writer.flush() catch {};

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");
    try writeJsonString(w, sexpr_writer_allocating.written());
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write a diagnostic as JSON
fn writeDiagnosticHtml(writer: *std.io.Writer, report: reporting.Report) !void {
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
) !void {
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
        const message = report.title;
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

fn writeDiagnosticJson(writer: anytype, diagnostic: Diagnostic) !void {
    try writer.print("{{\"severity\":\"{s}\",\"message\":\"", .{@tagName(diagnostic.severity)});
    try writeJsonString(writer, diagnostic.message);
    try writer.print("\",\"region\":{{\"start_line\":{d},\"start_column\":{d},\"end_line\":{d},\"end_column\":{d}}}}}", .{
        diagnostic.region.start_line, diagnostic.region.start_column,
        diagnostic.region.end_line,   diagnostic.region.end_column,
    });
}

/// Write a string with JSON escaping
fn writeJsonString(writer: *std.io.Writer, str: []const u8) !void {
    for (str) |char| {
        switch (char) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            // Control characters U+0000 to U+001F
            0...8, 11, 12, 14...31 => try writer.print("\\u{x:0>4}", .{char}),
            else => try writer.writeByte(char),
        }
    }
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
    var temp_buffer = std.array_list.Managed(u8).init(allocator);
    defer temp_buffer.deinit();
    temp_buffer.writer().print("{{\"status\":\"ERROR\",\"message\":\"{s}\"}}", .{error_message}) catch return null;
    const json_len = temp_buffer.items.len;

    // 2. Allocate memory for [u32: length][u8...: data][u8: null terminator]
    const total_len = @sizeOf(u32) + json_len + 1;
    const final_buffer = allocator.alloc(u8, total_len) catch return null;

    // 3. Write the length prefix (the length of the JSON data only)
    std.mem.writeInt(u32, final_buffer[0..@sizeOf(u32)], @intCast(json_len), .little);

    // 4. Copy the JSON data
    const data_ptr = final_buffer.ptr + @sizeOf(u32);
    @memcpy(final_buffer[@sizeOf(u32)..][0..json_len], temp_buffer.items);

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
