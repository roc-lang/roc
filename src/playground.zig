//! Main WASM module for the Roc playground.
//! This module provides a state machine interface between JavaScript and the Roc compiler.
//!
//! State Machine:
//! 1. START: Initialize module, return compiler version
//! 2. READY: Receive Roc source, compile through all stages, return "LOADED" with diagnostics
//! 3. LOADED: Handle queries for tokens, AST, MODULE_ENV, types, etc. Handle reset to go back to READY
//!
//! Compilation Strategy:
//! The playground uses Roc's "keep going" approach - all compiler stages run even when there
//! are errors in earlier stages. This provides better user experience by showing type information
//! and later-stage errors even when there are syntax errors. Malformed nodes are used to
//! represent invalid code, allowing the compiler to continue through all stages.

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const parse = @import("check/parse.zig");
const build_options = @import("build_options");
const can = @import("check/canonicalize.zig");
const check_types = @import("check/check_types.zig");
const WasmFilesystem = @import("playground/WasmFilesystem.zig");
const reporting = @import("reporting.zig");
const snapshot = @import("snapshot.zig");
const types = @import("types");
const problem = @import("check/check_types/problem.zig");

const SExprTree = base.SExprTree;
const ModuleEnv = compile.ModuleEnv;
const Allocator = std.mem.Allocator;

const allocator: Allocator = .{
    .ptr = undefined,
    .vtable = &std.heap.WasmAllocator.vtable,
};

const State = enum {
    START,
    READY,
    LOADED,
};

/// Message types for communication with the playground
const MessageType = enum {
    INIT,
    LOAD_SOURCE,
    QUERY_TOKENS,
    QUERY_AST,
    QUERY_MODULE_ENV,
    QUERY_TYPES,
    GET_TYPE_INFO,
    RESET,

    pub fn fromString(str: []const u8) ?MessageType {
        if (std.mem.eql(u8, str, "INIT")) return .INIT;
        if (std.mem.eql(u8, str, "LOAD_SOURCE")) return .LOAD_SOURCE;
        if (std.mem.eql(u8, str, "QUERY_TOKENS")) return .QUERY_TOKENS;
        if (std.mem.eql(u8, str, "QUERY_AST")) return .QUERY_AST;
        if (std.mem.eql(u8, str, "QUERY_MODULE_ENV")) return .QUERY_MODULE_ENV;
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
    can_ir: ?can.MODULE_ENV = null,
    solver: ?check_types = null,

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

        if (self.can_ir) |*ir| {
            ir.deinit();
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

/// Initialize the WASM module - always starts in START state
export fn init() void {
    if (compiler_data) |*data| {
        data.deinit();
        compiler_data = null;
    }
}

/// Process a message and return response length
export fn processMessage(message_ptr: [*]const u8, message_len: usize, response_ptr: [*]u8, response_len: usize) usize {
    const message = message_ptr[0..message_len];
    const response_buffer = response_ptr[0..response_len];

    // Parse the message as JSON
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, message, .{}) catch {
        return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid JSON message");
    };
    defer parsed.deinit();

    const root = parsed.value;
    const message_type_str = root.object.get("type") orelse {
        return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing message type");
    };

    const message_type = MessageType.fromString(message_type_str.string) orelse {
        return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Unknown message type");
    };

    // Handle message based on current state
    return switch (current_state) {
        .START => handleStartState(message_type, root, response_buffer),
        .READY => handleReadyState(message_type, root, response_buffer),
        .LOADED => handleLoadedState(message_type, root, response_buffer),
    };
}

/// Handle messages in START state
fn handleStartState(message_type: MessageType, _: std.json.Value, response_buffer: []u8) usize {
    switch (message_type) {
        .INIT => {
            current_state = .READY;
            const compiler_version = build_options.compiler_version;
            return writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            return writeErrorResponse(response_buffer, .INVALID_STATE, "Can only handle INIT in START state");
        },
    }
}

/// Handle messages in READY state
fn handleReadyState(message_type: MessageType, root: std.json.Value, response_buffer: []u8) usize {
    switch (message_type) {
        .LOAD_SOURCE => {
            const source_value = root.object.get("source") orelse {
                return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing source in LOAD_SOURCE message");
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
                return writeErrorResponse(response_buffer, .ERROR, @errorName(err));
            };

            compiler_data = result;
            current_state = .LOADED;

            // Return success with diagnostics
            return writeLoadedResponse(response_buffer, result);
        },
        .RESET => {
            // Already in READY state, just acknowledge
            const compiler_version = build_options.compiler_version;
            return writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            return writeErrorResponse(response_buffer, .INVALID_STATE, "Can only handle LOAD_SOURCE or RESET in READY state");
        },
    }
}

/// Handle messages in LOADED state
fn handleLoadedState(message_type: MessageType, message_json: std.json.Value, response_buffer: []u8) usize {
    const data = compiler_data.?;

    switch (message_type) {
        .QUERY_TOKENS => {
            return writeTokensResponse(response_buffer, data);
        },
        .QUERY_AST => {
            return writeParseAstResponse(response_buffer, data);
        },
        .QUERY_MODULE_ENV => {
            return writeCanCirResponse(response_buffer, data);
        },
        .QUERY_TYPES => {
            return writeTypesResponse(response_buffer, data);
        },
        .GET_TYPE_INFO => {
            return writeTypeInfoResponse(response_buffer, data, message_json);
        },
        .RESET => {
            // Clean up and go back to READY
            if (compiler_data) |*old_data| {
                old_data.deinit();
                compiler_data = null;
            }
            current_state = .READY;

            const compiler_version = build_options.compiler_version;
            return writeSuccessResponse(response_buffer, compiler_version, null);
        },
        else => {
            return writeErrorResponse(response_buffer, .INVALID_STATE, "Invalid message type for LOADED state");
        },
    }
}

/// Compile source through all compiler stages using Roc's "keep going" strategy.
/// All stages run even when there are errors in earlier stages, using malformed nodes
/// to represent invalid code and allow continued compilation.
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
        const report = parse_ast.parseDiagnosticToReport(module_env, diagnostic, allocator, "main.roc") catch {
            // Log the error and continue processing other diagnostics
            // This prevents crashes on malformed diagnostics or empty input
            continue;
        };
        result.parse_reports.append(report) catch continue;
    }

    // Stage 2: Canonicalization (always run, even with parse errors)
    // The canonicalizer handles malformed parse nodes and continues processing
    // Initialize MODULE_ENV directly in result to ensure stable pointer
    result.can_ir = try can.MODULE_ENV.init(allocator, "main");
    var can_ir = &result.can_ir.?;

    var canonicalizer = try can.init(can_ir, &parse_ast, null);
    defer canonicalizer.deinit();

    canonicalizer.canonicalizeFile() catch {};

    // Debug: Log the spans after canonicalization
    const defs_count = can_ir.store.sliceDefs(can_ir.all_defs).len;
    const stmts_count = can_ir.store.sliceStatements(can_ir.all_statements).len;
    _ = defs_count;
    _ = stmts_count;

    // Collect canonicalization diagnostics
    const can_diags = can_ir.getDiagnostics() catch &.{};
    for (can_diags) |diag| {
        const report = can_ir.diagnosticToReport(diag, allocator, "main.roc") catch continue;
        result.can_reports.append(report) catch continue;
    }

    // Stage 3: Type checking (always run if we have MODULE_ENV, even with canonicalization errors)
    // The type checker works with malformed canonical nodes to provide partial type information
    if (result.can_ir) |*type_can_ir| {
        const empty_modules: []const *can.MODULE_ENV = &.{};
        // Use pointer to the stored MODULE_ENV to ensure solver references valid memory
        var solver = try check_types.init(allocator, &module_env.types, type_can_ir, empty_modules, &type_can_ir.store.regions);
        result.solver = solver;

        solver.checkDefs() catch {};

        // Collect type checking problems and convert them to reports using ReportBuilder
        var report_builder = problem.ReportBuilder.init(
            allocator,
            module_env,
            type_can_ir,
            &solver.snapshots,
            "main.roc",
            &.{}, // other_modules - empty for playground
        );
        defer report_builder.deinit();

        var iter = solver.problems.problems.iterIndices();
        while (iter.next()) |idx| {
            const type_problem = solver.problems.problems.get(idx);
            const report = report_builder.build(type_problem) catch continue;
            result.type_reports.append(report) catch continue;
        }
    }

    return result;
}

/// Write a success response
fn writeSuccessResponse(response_buffer: []u8, message: []const u8, data: ?[]const u8) usize {
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    writer.writeAll("{\"status\":\"SUCCESS\",\"message\":\"") catch return 0;
    writeJsonString(writer, message) catch return 0;
    writer.writeAll("\"") catch return 0;

    if (data) |d| {
        writer.writeAll(",\"data\":") catch return 0;
        writer.writeAll(d) catch return 0;
    }

    writer.writeAll("}") catch return 0;
    return stream.getWritten().len;
}

/// Write an error response
fn writeErrorResponse(response_buffer: []u8, status: ResponseStatus, message: []const u8) usize {
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    writer.print("{{\"status\":\"{s}\",\"message\":\"", .{status.toString()}) catch return 0;
    writeJsonString(writer, message) catch return 0;
    writer.writeAll("\"}") catch return 0;

    return stream.getWritten().len;
}

/// Write response for LOADED state with diagnostics
///
/// This function implements a two-tier diagnostic system:
/// 1. SUMMARY: Counts ALL diagnostics from reports (for "Found X errors, Y warnings" display)
/// 2. VISUAL INDICATORS: Only diagnostics with region info (for gutter markers & squiggly lines)
///
/// The summary provides the complete diagnostic picture, while visual indicators are limited
/// to diagnostics that can be precisely positioned in the editor.
fn writeLoadedResponse(response_buffer: []u8, data: CompilerStageData) usize {
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    // TIER 1: Extract diagnostics for VISUAL INDICATORS (gutter markers, squiggly lines)
    // This is a filtered subset - only includes diagnostics that have region information
    // for precise positioning in the editor. Limited to 100 items to avoid UI performance issues.
    var diagnostics = std.ArrayList(Diagnostic).init(allocator);
    defer diagnostics.deinit();

    // Extract diagnostics from all stages with additional safeguards
    extractDiagnosticsFromReports(&diagnostics, data.tokenize_reports) catch {
        // Log error but continue - don't fail the entire response
    };
    extractDiagnosticsFromReports(&diagnostics, data.parse_reports) catch {};
    extractDiagnosticsFromReports(&diagnostics, data.can_reports) catch {};
    extractDiagnosticsFromReports(&diagnostics, data.type_reports) catch {};

    // TIER 2: Count ALL diagnostics from reports (for SUMMARY display)
    // This includes ALL diagnostics regardless of whether they have region info.
    // Used for the "Found X errors, Y warnings" message shown to users.
    var total_errors: u32 = 0;
    var total_warnings: u32 = 0;

    const tokenize_counts = countDiagnostics(data.tokenize_reports.items);
    const parse_counts = countDiagnostics(data.parse_reports.items);
    const can_counts = countDiagnostics(data.can_reports.items);
    const type_counts = countDiagnostics(data.type_reports.items);

    total_errors += tokenize_counts.errors + parse_counts.errors + can_counts.errors + type_counts.errors;
    total_warnings += tokenize_counts.warnings + parse_counts.warnings + can_counts.warnings + type_counts.warnings;

    writer.writeAll("{\"status\":\"SUCCESS\",\"message\":\"LOADED\",\"diagnostics\":{") catch return 0;

    // Write summary with stage breakdown for debugging
    writer.print("\"summary\":{{\"errors\":{},\"warnings\":{}}},", .{ total_errors, total_warnings }) catch return 0;

    // Add debug information showing counts by stage and report counts
    writer.print("\"debug_counts\":{{\"tokenize\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}},\"parse\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}},\"can\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}},\"type\":{{\"errors\":{},\"warnings\":{},\"total_reports\":{}}}}},", .{ tokenize_counts.errors, tokenize_counts.warnings, data.tokenize_reports.items.len, parse_counts.errors, parse_counts.warnings, data.parse_reports.items.len, can_counts.errors, can_counts.warnings, data.can_reports.items.len, type_counts.errors, type_counts.warnings, data.type_reports.items.len }) catch return 0;

    // Write structured diagnostics array with safeguards
    writer.writeAll("\"list\":[") catch return 0;
    if (diagnostics.items.len > 0) {
        for (diagnostics.items, 0..) |diagnostic, i| {
            if (i > 0) writer.writeAll(",") catch return 0;
            writeDiagnosticJson(writer, diagnostic) catch {
                // Skip malformed diagnostic but continue with others
                continue;
            };
        }
    }
    writer.writeAll("],") catch return 0;

    // Write HTML diagnostics
    writer.writeAll("\"html\":\"") catch return 0;

    // Collect HTML in a buffer first, then escape it for JSON
    var html_buffer: [32768]u8 = undefined;
    var html_stream = std.io.fixedBufferStream(&html_buffer);
    const html_writer = html_stream.writer();

    // Write diagnostics by stage if any exist
    if (data.tokenize_reports.items.len > 0) {
        for (data.tokenize_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return 0;
        }
    }

    if (data.parse_reports.items.len > 0) {
        for (data.parse_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return 0;
        }
    }

    if (data.can_reports.items.len > 0) {
        for (data.can_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return 0;
        }
    }

    if (data.type_reports.items.len > 0) {
        for (data.type_reports.items) |report| {
            writeDiagnosticHtml(html_writer, report) catch return 0;
        }
    }

    // Write the HTML buffer with proper JSON escaping
    const html_content = html_stream.getWritten();
    writeJsonString(writer, html_content) catch return 0;
    writer.writeAll("\"}}") catch return 0;

    return stream.getWritten().len;
}

/// Generate an interactive source range span for the playground
fn writeSourceRangeSpan(writer: anytype, region: base.Region, source: []const u8, line_starts: []const u32) !void {
    const region_info = base.RegionInfo.position(source, line_starts, region.start.offset, region.end.offset) catch {
        // Fallback to byte offsets if line calculation fails
        try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\">@{d}-{d}</span>", .{ region.start.offset, region.end.offset, region.start.offset, region.end.offset });
        return;
    };

    // Generate interactive span with line/column info
    try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\" data-start-line=\"{d}\" data-start-col=\"{d}\" data-end-line=\"{d}\" data-end-col=\"{d}\">@{d}.{d}-{d}.{d}</span>", .{ region.start.offset, region.end.offset, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1 });
}

/// Write tokens response with direct HTML generation
fn writeTokensResponse(response_buffer: []u8, data: CompilerStageData) usize {
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    writer.writeAll("{\"status\":\"SUCCESS\",\"data\":\"") catch return 0;

    // Generate tokens HTML directly
    if (data.parse_ast) |ast| {
        // Create a new arena for this query
        var local_arena = std.heap.ArenaAllocator.init(allocator);
        defer local_arena.deinit();
        const arena_allocator = local_arena.allocator();

        var html_buffer = std.ArrayList(u8).init(arena_allocator);
        defer html_buffer.deinit();
        var html_writer = html_buffer.writer();

        // Write HTML container
        html_writer.writeAll("<div class=\"token-list\">") catch return 0;

        // Generate tokens from AST
        const token_tags = ast.tokens.tokens.items(.tag);
        const token_extras = ast.tokens.tokens.items(.extra);

        for (token_tags, token_extras, 0..) |tag, _, i| {
            const token_name = @tagName(tag);
            const region = ast.tokens.resolve(i);

            // Determine CSS class based on token type
            var css_class: []const u8 = "default";
            if (std.mem.startsWith(u8, token_name, "Kw")) {
                css_class = "keyword";
            } else if (std.mem.indexOf(u8, token_name, "Ident")) |_| {
                css_class = "identifier";
            } else if (std.mem.startsWith(u8, token_name, "Op")) {
                css_class = "operator";
            } else if (std.mem.indexOf(u8, token_name, "String")) |_| {
                css_class = "string";
            } else if (std.mem.indexOf(u8, token_name, "Number")) |_| {
                css_class = "number";
            } else if (std.mem.indexOf(u8, token_name, "Open")) |_| {
                css_class = "punctuation";
            } else if (std.mem.indexOf(u8, token_name, "Close")) |_| {
                css_class = "punctuation";
            } else if (std.mem.eql(u8, token_name, "EndOfFile")) {
                css_class = "eof";
            }

            // Write token HTML with interactive source range
            html_writer.print("<span class=\"token {s}\">", .{css_class}) catch return 0;
            html_writer.print("<span class=\"token-type\">{s}</span>", .{token_name}) catch return 0;
            html_writer.writeAll(" ") catch return 0;
            writeSourceRangeSpan(html_writer, region, data.module_env.source, data.module_env.line_starts.items.items) catch return 0;
            html_writer.writeAll("</span>") catch return 0;
        }

        html_writer.writeAll("</div>") catch return 0;

        // Write the HTML content as JSON string
        writeJsonString(writer, html_buffer.items) catch return 0;
    } else {
        return writeErrorResponse(response_buffer, .ERROR, "Tokens not available");
    }

    writer.writeAll("\"}") catch return 0;
    return stream.getWritten().len;
}

/// Write parse AST response in S-expression format
fn writeParseAstResponse(response_buffer: []u8, data: CompilerStageData) usize {
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    writer.writeAll("{\"status\":\"SUCCESS\",\"data\":\"") catch return 0;

    // Write AST in S-expression format
    if (data.parse_ast) |ast| {
        // Create a new arena for this query
        var local_arena = std.heap.ArenaAllocator.init(allocator);
        defer local_arena.deinit();
        const arena_allocator = local_arena.allocator();

        var mut_ast = ast;
        var sexpr_buffer = std.ArrayList(u8).init(arena_allocator);
        defer sexpr_buffer.deinit();

        var sexpr_writer = sexpr_buffer.writer();

        parse.AST.toSExprHtml(&mut_ast, data.module_env, sexpr_writer.any()) catch {
            return writeErrorResponse(response_buffer, .ERROR, "Failed to generate AST S-expression");
        };

        writeJsonString(writer, sexpr_buffer.items) catch return 0;
    } else {
        return writeErrorResponse(response_buffer, .ERROR, "Parse AST not available");
    }

    writer.writeAll("\"}") catch return 0;
    return stream.getWritten().len;
}

/// Write canonicalized MODULE_ENV response in S-expression format
fn writeCanCirResponse(response_buffer: []u8, data: CompilerStageData) usize {
    // Check for error conditions first
    if (data.can_ir == null) {
        // MODULE_ENV is only created if parsing succeeded, so if it's null, there were parse errors
        if (data.parse_reports.items.len > 0) {
            return writeErrorResponse(response_buffer, .ERROR, "Canonicalization skipped due to parsing errors.");
        } else {
            return writeErrorResponse(response_buffer, .ERROR, "Canonicalization failed");
        }
    }

    // We have MODULE_ENV data, proceed with generation
    const cir = data.can_ir.?;

    // Create a new arena for this query
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    const arena_allocator = local_arena.allocator();

    var sexpr_buffer = std.ArrayList(u8).init(arena_allocator);
    defer sexpr_buffer.deinit();

    const sexpr_writer = sexpr_buffer.writer().any();

    var tree = SExprTree.init(arena_allocator);
    defer tree.deinit();

    // Debug: Add counts to the output
    const defs_count = cir.store.sliceDefs(cir.all_defs).len;
    const stmts_count = cir.store.sliceStatements(cir.all_statements).len;

    if (defs_count == 0 and stmts_count == 0) {
        // If truly empty, add a debug node
        const debug_begin = tree.beginNode();
        tree.pushStaticAtom("empty-cir-debug") catch {};
        tree.pushStaticAtom("no-defs-or-statements") catch {};
        const debug_attrs = tree.beginNode();
        tree.endNode(debug_begin, debug_attrs) catch {};
    }

    const mutable_cir = @constCast(&cir);
    can.MODULE_ENV.pushToSExprTree(mutable_cir, null, &tree) catch {};
    tree.toHtml(sexpr_writer) catch {};

    // Success case - write the response
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    writer.writeAll("{\"status\":\"SUCCESS\",\"data\":\"") catch return 0;
    writeJsonString(writer, sexpr_buffer.items) catch return 0;
    writer.writeAll("\"}") catch return 0;
    return stream.getWritten().len;
}

/// Write type info response for a specific position
fn writeTypeInfoResponse(response_buffer: []u8, data: CompilerStageData, message_json: std.json.Value) usize {
    // Parse the position from the message
    const line = message_json.object.get("line") orelse {
        return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing line parameter");
    };
    const ch = message_json.object.get("ch") orelse {
        return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing ch parameter");
    };
    const identifier = message_json.object.get("identifier") orelse {
        return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Missing identifier parameter");
    };

    const line_num = switch (line) {
        .integer => |i| @as(u32, @intCast(i)),
        else => return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid line parameter"),
    };
    const ch_num = switch (ch) {
        .integer => |i| @as(u32, @intCast(i)),
        else => return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid ch parameter"),
    };
    const ident_str = switch (identifier) {
        .string => |s| s,
        else => return writeErrorResponse(response_buffer, .INVALID_MESSAGE, "Invalid identifier parameter"),
    };

    // Check for error conditions first
    if (data.can_ir == null) {
        return writeErrorResponse(response_buffer, .ERROR, "Canonicalization not completed.");
    }

    if (data.solver == null) {
        return writeErrorResponse(response_buffer, .ERROR, "Type checking not completed.");
    }

    // Convert line/ch to byte offset
    const source = data.module_env.source;
    const line_starts = data.module_env.line_starts.items.items;

    if (line_num >= line_starts.len) {
        return writeErrorResponse(response_buffer, .ERROR, "Line number out of range");
    }

    const line_start = line_starts[line_num];
    const byte_offset = line_start + ch_num;

    if (byte_offset >= source.len) {
        return writeErrorResponse(response_buffer, .ERROR, "Position out of range");
    }

    // Find type information for the identifier at this position
    const type_info = findTypeInfoAtPosition(data, byte_offset, ident_str) catch {
        return writeErrorResponse(response_buffer, .ERROR, "Failed to find type information");
    };

    // Write the response
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    if (type_info) |info| {
        writer.writeAll("{\"status\":\"SUCCESS\",\"type_info\":{\"type\":\"") catch return 0;
        writeJsonString(writer, info) catch return 0;
        writer.writeAll("\"}}") catch return 0;
    } else {
        writer.writeAll("{\"status\":\"SUCCESS\",\"type_info\":null}") catch return 0;
    }

    return stream.getWritten().len;
}

/// Find type information for an identifier at a specific byte position
fn findTypeInfoAtPosition(data: CompilerStageData, byte_offset: u32, identifier: []const u8) !?[]const u8 {
    const cir = data.can_ir.?;
    const gpa = cir.gpa;

    // Create TypeWriter for converting types to strings
    var type_writer = compile.type_writers.TypeWriter.init(gpa, cir.env) catch return null;
    defer type_writer.deinit();

    // Iterate through all definitions to find one that contains this position
    const all_defs = cir.store.sliceDefs(cir.all_defs);

    for (all_defs) |def_idx| {
        const def = cir.store.getDef(def_idx);

        // Check if this definition's pattern contains the byte offset
        const pattern_region = cir.store.getPatternRegion(def.pattern);

        if (byte_offset >= pattern_region.start.offset and byte_offset < pattern_region.end.offset) {
            // Check if this pattern matches our identifier
            const pattern = cir.store.getPattern(def.pattern);

            switch (pattern) {
                .assign => |assign| {
                    // Get the identifier text
                    const ident_text = cir.idents.getText(assign.ident);

                    // Check if this matches our target identifier
                    if (std.mem.eql(u8, ident_text, identifier)) {
                        // Get the type variable for this definition
                        const def_var = @as(types.Var, @enumFromInt(@intFromEnum(def_idx)));

                        // Write the type to string
                        type_writer.write(def_var) catch return null;

                        // Return a copy of the type string
                        const type_str = type_writer.get();
                        const owned_str = gpa.dupe(u8, type_str) catch return null;
                        return owned_str;
                    }
                },
                else => {
                    // For other pattern types, we could implement additional matching logic
                    // For now, skip non-assign patterns
                },
            }
        }
    }

    return null;
}

/// Write types response in S-expression format
fn writeTypesResponse(response_buffer: []u8, data: CompilerStageData) usize {
    // Check for error conditions first
    if (data.can_ir == null) {
        return writeErrorResponse(response_buffer, .ERROR, "Canonicalization not completed.");
    }

    if (data.solver == null) {
        return writeErrorResponse(response_buffer, .ERROR, "Type checking not completed.");
    }

    // We have both MODULE_ENV and solver data, proceed with type generation
    const cir = data.can_ir.?;

    // Create a new arena for this query to avoid allocation issues
    var local_arena = std.heap.ArenaAllocator.init(allocator);
    defer local_arena.deinit();
    const arena_allocator = local_arena.allocator();

    var sexpr_buffer = std.ArrayList(u8).init(arena_allocator);
    defer sexpr_buffer.deinit();

    const sexpr_writer = sexpr_buffer.writer().any();

    var tree = SExprTree.init(arena_allocator);
    defer tree.deinit();

    // Use the same as snapshot system
    const mutable_cir = @constCast(&cir);
    mutable_cir.pushTypesToSExprTree(null, &tree) catch |err| {
        // If type generation fails, return an error message
        const error_msg = switch (err) {
            error.OutOfMemory => "Out of memory while generating types",
        };
        return writeErrorResponse(response_buffer, .ERROR, error_msg);
    };

    tree.toHtml(sexpr_writer) catch {};

    // Success case - write the response
    var stream = std.io.fixedBufferStream(response_buffer);
    var writer = stream.writer();

    writer.writeAll("{\"status\":\"SUCCESS\",\"data\":\"") catch return 0;
    writeJsonString(writer, sexpr_buffer.items) catch return 0;
    writer.writeAll("\"}") catch return 0;
    return stream.getWritten().len;
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

/// Extract diagnostics from reports for VISUAL INDICATORS only.
///
/// This function filters reports to only include those with region information
/// that can be precisely positioned in the editor for gutter markers and squiggly lines.
///
/// Reports without region info are SKIPPED here but are still counted in the summary.
/// This is intentional - the summary should show the complete diagnostic picture,
/// while visual indicators are limited to locationally-precise diagnostics.
fn extractDiagnosticsFromReports(
    diagnostics: *std.ArrayList(Diagnostic),
    reports: std.ArrayList(reporting.Report),
) !void {
    var count: usize = 0;
    const max_diagnostics = 100; // Limit visual indicators to avoid UI performance issues

    for (reports.items) |*report| {
        if (count >= max_diagnostics) break;

        // Try to extract region info from the report
        // If no region info is available, skip this report for visual indicators
        // (it will still be counted in the summary statistics)
        const region_info = report.getRegionInfo() orelse continue;

        // Additional validation for empty or invalid regions
        if (region_info.start_line_idx == 0 and region_info.start_col_idx == 0 and
            region_info.end_line_idx == 0 and region_info.end_col_idx == 0)
        {
            continue; // Skip invalid zero regions
        }

        // Get the title as the message
        const message = report.title;

        // Convert report severity to diagnostic severity
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
        diagnostic.region.start_line,
        diagnostic.region.start_column,
        diagnostic.region.end_line,
        diagnostic.region.end_column,
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
            else => try writer.writeByte(char),
        }
    }
}

/// Allocate memory that can be accessed from JavaScript
export fn allocate(size: usize) ?[*]u8 {
    const memory = allocator.alloc(u8, size) catch return null;
    return memory.ptr;
}

/// Free memory that was allocated with allocate()
export fn deallocate(ptr: [*]u8, size: usize) void {
    const memory = ptr[0..size];
    allocator.free(memory);
}

/// Get current state for debugging
export fn getCurrentState() u32 {
    return switch (current_state) {
        .START => 0,
        .READY => 1,
        .LOADED => 2,
    };
}

/// Clean up all resources
export fn cleanup() void {
    if (compiler_data) |*data| {
        data.deinit();
        compiler_data = null;
    }

    // Clean up global source
    if (WasmFilesystem.global_source) |source| {
        if (WasmFilesystem.global_allocator) |alloc| {
            alloc.free(source);
        }
        WasmFilesystem.global_source = null;
        WasmFilesystem.global_allocator = null;
    }
}
