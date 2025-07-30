//! WASM module for the Roc playground.
//!
//! This module provides a state machine interface for the Roc compiler.
//!
//! State Machine:
//! 1. START: Initialize module, return compiler version
//! 2. READY: Receive Roc source, compile through all stages, return "LOADED" with diagnostics
//! 3. LOADED: Handle queries for tokens, AST, CIR, types, etc. Handle reset to go back to READY
//!
//! "Keep going" Strategy:
//! The playground uses a "keep going" approach - all compiler stages run even when there
//! are errors in earlier stages. This provides type information and later-stage errors
//! even when there are syntax errors. Malformed nodes are used to represent invalid code,
//! allowing the compiler to continue through all stages.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const parse = @import("parse");
const reporting = @import("reporting");
const types = @import("types");
const compile = @import("compile");
const Can = @import("can");
const Check = @import("check");

const WasmFilesystem = @import("playground/WasmFilesystem.zig");

const SExprTree = base.SExprTree;
const ModuleEnv = compile.ModuleEnv;
const Allocator = std.mem.Allocator;
const problem = Check.problem;
const AST = parse.AST;

// A fixed-size buffer to act as the heap inside the WASM linear memory.
var wasm_heap_memory: [2 * 1024 * 1024]u8 = undefined; // 2MB heap
var fba: std.heap.FixedBufferAllocator = undefined;
var allocator: Allocator = undefined;

const State = enum {
    START,
    READY,
    LOADED,
};

/// Message types for communication with the host
const MessageType = enum {
    INIT,
    LOAD_SOURCE,
    QUERY_TOKENS,
    QUERY_AST,
    QUERY_CIR,
    QUERY_TYPES,
    GET_TYPE_INFO,
    RESET,

    pub fn fromString(str: []const u8) ?MessageType {
        if (std.mem.eql(u8, str, "INIT")) return .INIT;
        if (std.mem.eql(u8, str, "LOAD_SOURCE")) return .LOAD_SOURCE;
        if (std.mem.eql(u8, str, "QUERY_TOKENS")) return .QUERY_TOKENS;
        if (std.mem.eql(u8, str, "QUERY_AST")) return .QUERY_AST;
        if (std.mem.eql(u8, str, "QUERY_CIR")) return .QUERY_CIR;
        if (std.mem.eql(u8, str, "QUERY_TYPES")) return .QUERY_TYPES;
        if (std.mem.eql(u8, str, "GET_TYPE_INFO")) return .GET_TYPE_INFO;
        if (std.mem.eql(u8, str, "RESET")) return .RESET;
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

    // Diagnostic reports from each stage
    tokenize_reports: std.ArrayList(reporting.Report),
    parse_reports: std.ArrayList(reporting.Report),
    can_reports: std.ArrayList(reporting.Report),
    type_reports: std.ArrayList(reporting.Report),

    pub fn init(alloc: Allocator, module_env: *ModuleEnv) CompilerStageData {
        return CompilerStageData{
            .module_env = module_env,
            .tokenize_reports = std.ArrayList(reporting.Report).init(alloc),
            .parse_reports = std.ArrayList(reporting.Report).init(alloc),
            .can_reports = std.ArrayList(reporting.Report).init(alloc),
            .type_reports = std.ArrayList(reporting.Report).init(alloc),
        };
    }

    pub fn deinit(self: *CompilerStageData) void {
        if (self.parse_ast) |*ast| {
            ast.deinit(allocator);
        }

        if (self.solver) |*s| {
            s.deinit();
        }

        // Free diagnostic reports and their ArrayLists
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

        // Free the ModuleEnv and its source
        self.module_env.deinit();
        allocator.destroy(self.module_env);
    }
};

/// Global state machine
var current_state: State = .START;
var compiler_data: ?CompilerStageData = null;

/// Host-managed buffers for better memory management
var host_message_buffer: ?[]u8 = null;
var host_response_buffer: ?[]u8 = null;
var last_error: ?[:0]const u8 = null;

/// In-memory debug log buffer for WASM
var debug_log_buffer: [4096]u8 = undefined;
var debug_log_pos: usize = 0;
var debug_log_oom: bool = false;

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
};

/// Initialize the WASM module in START state
export fn init() void {
    fba = std.heap.FixedBufferAllocator.init(&wasm_heap_memory);
    allocator = fba.allocator();

    if (compiler_data) |*data| {
        data.deinit();
        compiler_data = null;
    }

    // Clean up any existing buffers
    if (host_message_buffer) |buf| {
        allocator.free(buf);
        host_message_buffer = null;
    }
    if (host_response_buffer) |buf| {
        allocator.free(buf);
        host_response_buffer = null;
    }
    if (last_error) |err| {
        allocator.free(err);
        last_error = null;
    }
}

/// Allocate a buffer for incoming messages from the host.
/// Returns null on allocation failure.
export fn allocateMessageBuffer(size: usize) ?[*]u8 {
    if (host_message_buffer) |buf| {
        allocator.free(buf);
    }
    host_message_buffer = allocator.alloc(u8, size) catch return null;
    return host_message_buffer.?.ptr;
}

/// Allocate a buffer for responses to the host.
/// Returns null on allocation failure.
export fn allocateResponseBuffer(size: usize) ?[*]u8 {
    if (host_response_buffer) |buf| {
        allocator.free(buf);
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
    };

    return if (result) |_| @intFromEnum(WasmError.success) else |err| switch (err) {
        error.OutOfBufferSpace => @intFromEnum(WasmError.response_buffer_too_small),
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
            // Compile and return result
            const result = compileSource(source) catch |err| {
                try writeErrorResponse(response_buffer, .ERROR, @errorName(err));
                return;
            };

            compiler_data = result;
            current_state = .LOADED;

            // Return success with diagnostics
            try writeLoadedResponse(response_buffer, result);
        },
        .RESET => {
            // Already in READY state, just acknowledge
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
        .GET_TYPE_INFO => {
            try writeTypeInfoResponse(response_buffer, data, message_json);
        },
        .RESET => {
            // Clean up and go back to READY
            if (compiler_data) |*old_data| {
                old_data.deinit();
                compiler_data = null;
            }
            current_state = .READY;

            const compiler_version = build_options.compiler_version;
            try writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            try writeErrorResponse(response_buffer, .INVALID_STATE, "INVALID_STATE");
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
        try module_env.calcLineStarts();
        return CompilerStageData.init(allocator, module_env);
    }

    const trimmed_source = std.mem.trim(u8, source, " \t\n\r");
    if (trimmed_source.len == 0) {
        // Return empty compiler stage data for whitespace-only input
        var module_env = try allocator.create(ModuleEnv);
        module_env.* = try ModuleEnv.init(allocator, source);
        try module_env.calcLineStarts();
        return CompilerStageData.init(allocator, module_env);
    }

    // Set up the source in WASM filesystem
    WasmFilesystem.setSource(allocator, source);

    // Initialize the ModuleEnv
    var module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);
    try module_env.calcLineStarts();

    var result = CompilerStageData.init(allocator, module_env);

    // Stage 1: Parse (includes tokenization)
    var parse_ast = try parse.parse(module_env);
    result.parse_ast = parse_ast;

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
        const report = parse_ast.parseDiagnosticToReport(module_env.*, diagnostic, allocator, "main.roc") catch {
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

    var czer = try Can.init(env, &result.parse_ast.?, null);
    defer czer.deinit();

    czer.canonicalizeFile() catch |err| {
        logDebug("compileSource: canonicalizeFile failed: {}\n", .{err});
        if (err == error.OutOfMemory) {
            // If we're out of memory here, the state is likely unstable.
            // Propagate this error up to halt compilation gracefully.
            return err;
        }
    };

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
    {
        const type_can_ir = result.module_env;
        const empty_modules: []const *ModuleEnv = &.{};
        // Use pointer to the stored CIR to ensure solver references valid memory
        var solver = try Check.init(allocator, &type_can_ir.types, type_can_ir, empty_modules, &type_can_ir.store.regions);
        result.solver = solver;

        solver.checkDefs() catch |check_err| {
            logDebug("compileSource: checkDefs failed: {}\n", .{check_err});
            if (check_err == error.OutOfMemory) {
                // OOM during type checking is critical.
                // Deinit solver and propagate error.
                solver.deinit();
                result.solver = null; // Prevent double deinit in CompilerStageData.deinit
                return check_err;
            }
        };

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

    const Self = @This();
    pub const Error = ResponseWriteError;
    pub const Writer = std.io.Writer(*Self, Error, write);

    fn write(self: *Self, bytes: []const u8) Error!usize {
        if (self.pos + bytes.len > self.buffer.len) {
            return error.OutOfBufferSpace;
        }
        @memcpy(self.buffer[self.pos..][0..bytes.len], bytes);
        self.pos += bytes.len;
        return bytes.len;
    }

    fn writer(self: *Self) Writer {
        return .{ .context = self };
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
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    // Advance past length prefix, will be written by finalize
    resp_writer.pos = @sizeOf(u32);

    const w = resp_writer.writer();
    try w.print("{{\"status\":\"{s}\",\"message\":\"", .{status.toString()});
    try writeJsonString(w, message);
    try w.writeAll("\"}");

    try resp_writer.finalize();
}

/// Write a success response
fn writeSuccessResponse(response_buffer: []u8, message: []const u8, data: ?[]const u8) ResponseWriteError!void {
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);

    const w = resp_writer.writer();
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
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);

    const w = resp_writer.writer();

    // TIER 1: Extract diagnostics for VISUAL INDICATORS (gutter markers, squiggly lines)
    var diagnostics = std.ArrayList(Diagnostic).init(allocator);
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
    var html_stream = std.io.fixedBufferStream(&html_buffer);
    const html_writer = html_stream.writer();

    if (data.tokenize_reports.items.len > 0) {
        for (data.tokenize_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return error.OutOfBufferSpace;
        }
    }
    if (data.parse_reports.items.len > 0) {
        for (data.parse_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return error.OutOfBufferSpace;
        }
    }
    if (data.can_reports.items.len > 0) {
        for (data.can_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return error.OutOfBufferSpace;
        }
    }
    if (data.type_reports.items.len > 0) {
        for (data.type_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return error.OutOfBufferSpace;
        }
    }

    const html_content = html_stream.getWritten();
    try writeJsonString(w, html_content);
    try w.writeAll("\"}}");

    try resp_writer.finalize();
}

/// Write tokens response with direct HTML generation
fn writeTokensResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);
    const w = resp_writer.writer();

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    if (data.parse_ast) |ast| {
        var local_arena = std.heap.ArenaAllocator.init(allocator);
        defer local_arena.deinit();

        var html_buffer = std.ArrayList(u8).init(local_arena.allocator());
        defer html_buffer.deinit();
        const html_writer = html_buffer.writer().any();

        AST.tokensToHtml(&ast, data.module_env, html_writer) catch |err| {
            logDebug("writeTokensResponse: tokensToHtml failed with error: {}\n", .{err});
            try writeJsonString(w, "Error generating tokens HTML");
            try w.writeAll("\"}");
            try resp_writer.finalize();
            return;
        };

        try writeJsonString(w, html_buffer.items);
    } else {
        try writeJsonString(w, "Tokens not available");
    }

    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write parse AST response in S-expression format
fn writeParseAstResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);
    const w = resp_writer.writer();

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    if (data.parse_ast) |ast| {
        var local_arena = std.heap.ArenaAllocator.init(allocator);
        defer local_arena.deinit();

        var sexpr_buffer = std.ArrayList(u8).init(local_arena.allocator());
        defer sexpr_buffer.deinit();
        const sexpr_writer = sexpr_buffer.writer().any();

        var mut_ast = ast;
        var success = true;
        AST.toSExprHtml(&mut_ast, data.module_env.*, sexpr_writer) catch |err| {
            logDebug("writeParseAstResponse: toSExprHtml failed with error: {}\n", .{err});
            success = false;
        };

        if (success) {
            try writeJsonString(w, sexpr_buffer.items);
        } else {
            try writeJsonString(w, "Error generating AST HTML");
        }
    } else {
        try writeJsonString(w, "Parse AST not available");
    }

    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write canonicalized CIR response in S-expression format
fn writeCanCirResponse(response_buffer: []u8, data: CompilerStageData) ResponseWriteError!void {
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);
    const w = resp_writer.writer();

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");

    const cir = data.module_env;
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    var sexpr_buffer = std.ArrayList(u8).init(local_arena.allocator());
    defer sexpr_buffer.deinit();
    const sexpr_writer = sexpr_buffer.writer().any();

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
    tree.toHtml(sexpr_writer) catch {};

    try writeJsonString(w, sexpr_buffer.items);
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write type info response for a specific position
fn writeTypeInfoResponse(response_buffer: []u8, data: CompilerStageData, message_json: std.json.Value) ResponseWriteError!void {
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);
    const w = resp_writer.writer();

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
        .integer => |i| @as(u32, @intCast(i)),
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

    const source = data.module_env.source;
    const line_starts = data.module_env.line_starts.items.items;

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

    var maybe_owned_type_info: ?[]const u8 = null;
    defer if (maybe_owned_type_info) |owned_info| allocator.free(owned_info);

    maybe_owned_type_info = findTypeInfoAtPosition(data, byte_offset, ident_str) catch {
        try writeErrorResponse(response_buffer, .ERROR, "Failed to find type information");
        return;
    };

    try w.writeAll("{\"status\":\"SUCCESS\",\"type_info\":{\"type\":\"");
    if (maybe_owned_type_info) |info| {
        try writeJsonString(w, info);
    } else {
        try w.writeAll("null");
    }
    try w.writeAll("\"}}");
    try resp_writer.finalize();
}

/// Find type information for an identifier at a specific byte position
fn findTypeInfoAtPosition(data: CompilerStageData, byte_offset: u32, identifier: []const u8) !?[]const u8 {
    const cir = data.module_env;
    const local_allocator = allocator; // Use the global WASM allocator for duplication

    var type_writer = types.writers.TypeWriter.init(allocator, @ptrCast(cir)) catch return null;
    defer type_writer.deinit();

    const all_defs = cir.store.sliceDefs(cir.all_defs);

    for (all_defs) |def_idx| {
        const def = cir.store.getDef(def_idx);
        const pattern_region = cir.store.getPatternRegion(def.pattern);

        if (byte_offset >= pattern_region.start.offset and byte_offset < pattern_region.end.offset) {
            const pattern = cir.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |assign| {
                    const ident_text = cir.idents.getText(assign.ident);
                    if (std.mem.eql(u8, ident_text, identifier)) {
                        const def_var = @as(types.Var, @enumFromInt(@intFromEnum(def_idx)));
                        type_writer.write(def_var) catch return null;
                        const type_str_from_writer = type_writer.get();

                        // Duplicate the string so it can outlive the TypeWriter
                        const owned_type_str = local_allocator.dupe(u8, type_str_from_writer) catch return null;
                        return owned_type_str;
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
    var resp_writer = ResponseWriter{ .buffer = response_buffer };
    resp_writer.pos = @sizeOf(u32);
    const w = resp_writer.writer();

    if (data.solver == null) {
        try writeErrorResponse(response_buffer, .ERROR, "Type checking not completed.");
        return;
    }

    const cir = data.module_env;
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    var sexpr_buffer = std.ArrayList(u8).init(local_arena.allocator());
    defer sexpr_buffer.deinit();
    const sexpr_writer = sexpr_buffer.writer().any();

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
    tree.toHtml(sexpr_writer) catch {};

    try w.writeAll("{\"status\":\"SUCCESS\",\"data\":\"");
    try writeJsonString(w, sexpr_buffer.items);
    try w.writeAll("\"}");
    try resp_writer.finalize();
}

/// Write a diagnostic as JSON
fn writeDiagnosticHtml(writer: anytype, report: reporting.Report) !void {
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
    diagnostics: *std.ArrayList(Diagnostic),
    reports: std.ArrayList(reporting.Report),
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
fn writeJsonString(writer: anytype, str: []const u8) !void {
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
    const len_ptr: [*]const u32 = @ptrCast(@alignCast(ptr - @sizeOf(u32)));
    const original_alloc_ptr: [*]u8 = @ptrCast(@constCast(len_ptr));

    // Read the length of the JSON data.
    const json_len = len_ptr[0];

    // Calculate the total allocated size: [u32 length] + [data] + [u8 null terminator]
    const total_len = @sizeOf(u32) + json_len + 1;

    // Reconstruct the original slice that was allocated.
    const original_slice = original_alloc_ptr[0..total_len];

    // Free the original slice.
    allocator.free(original_slice);
}

/// Helper to create a simple error JSON string, following the length-prefix allocation pattern.
fn createSimpleErrorJson(error_message: []const u8) ?[*:0]u8 {
    // 1. Format the string into a temporary buffer to determine its length.
    var temp_buffer = std.ArrayList(u8).init(allocator);
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
    };
}
