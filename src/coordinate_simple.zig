//! Simplified coordination system for single-file processing.
//!
//! This provides a streamlined approach for `roc check` on single files,
//! focusing only on parsing and canonicalization without complex dependency resolution.

const std = @import("std");
const base = @import("base.zig");
const parse = @import("check/parse.zig");
const canonicalize = @import("check/canonicalize.zig");
const reporting = @import("reporting.zig");
const Filesystem = @import("coordinate/Filesystem.zig");

const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;
const Scope = canonicalize.Scope;

/// Result of processing source code, containing both CIR and Reports
/// for proper diagnostic reporting.
pub const ProcessResult = struct {
    cir: *CIR,
    reports: []reporting.Report,

    pub fn deinit(self: *ProcessResult, gpa: std.mem.Allocator) void {
        for (self.reports) |*report| {
            report.deinit();
        }
        gpa.free(self.reports);
        self.cir.deinit();
        gpa.destroy(self.cir);
    }
};

/// Process a single file and return diagnostics.
pub fn checkFile(
    gpa: std.mem.Allocator,
    fs: Filesystem,
    filepath: []const u8,
) ![]const CIR.Diagnostic {
    // Read the file content
    const source = fs.readFile(filepath, gpa) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.AccessDenied => return error.AccessDenied,
        else => return error.FileReadError,
    };
    defer gpa.free(source);

    return checkSource(gpa, source);
}

/// Process a single file and return both CIR and diagnostics for proper reporting.
pub fn processFile(
    gpa: std.mem.Allocator,
    fs: Filesystem,
    filepath: []const u8,
) !ProcessResult {
    // Read the file content
    const source = fs.readFile(filepath, gpa) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.AccessDenied => return error.AccessDenied,
        else => return error.FileReadError,
    };
    defer gpa.free(source);

    return processSource(gpa, source);
}

/// Process source code directly and return diagnostics.
/// Useful for testing or when you already have the source in memory.
pub fn checkSource(
    gpa: std.mem.Allocator,
    source: []const u8,
) ![]const CIR.Diagnostic {
    // Initialize the ModuleEnv
    var module_env = ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&module_env, source);
    defer parse_ast.deinit(gpa);

    // Check for parse errors - following "Inform Don't Block" principle,
    // we continue processing even with parse errors
    if (parse_ast.hasErrors()) {
        // For now, we'll continue processing even with parse errors
        // The diagnostics system will handle reporting these properly
    }

    // Initialize CIR - this transfers ownership of module_env
    var can_ir = CIR.init(&module_env);
    defer can_ir.deinit();

    // Create scope for semantic analysis
    var scope = Scope.init(can_ir.env.gpa);
    defer scope.deinit(can_ir.env.gpa);

    // Canonicalize the AST
    var canonicalizer = canonicalize.init(&can_ir, &parse_ast, &scope);
    canonicalizer.canonicalize_file();

    // Return diagnostics - the caller owns the returned slice
    return can_ir.getDiagnostics();
}

/// Process source code directly and return both CIR and Reports for proper reporting.
/// Useful when you need access to the CIR for diagnostic-to-report conversion.
pub fn processSource(
    gpa: std.mem.Allocator,
    source: []const u8,
) !ProcessResult {
    // Initialize the ModuleEnv
    var module_env = ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&module_env, source);
    defer parse_ast.deinit(gpa);

    var reports = std.ArrayList(reporting.Report).init(gpa);
    defer reports.deinit();

    // Handle parse errors first - convert to Reports
    if (parse_ast.hasErrors()) {
        // Handle tokenize diagnostics
        for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
            const report = parse_ast.tokenizeDiagnosticToReport(diagnostic, gpa) catch continue;
            reports.append(report) catch continue;
        }

        // Handle parser diagnostics
        for (parse_ast.parse_diagnostics.items) |diagnostic| {
            const report = parse_ast.parseDiagnosticToReport(diagnostic, gpa) catch continue;
            reports.append(report) catch continue;
        }
    }

    // Create CIR on heap first to avoid memory issues with diagnostics
    const cir_ptr = gpa.create(CIR) catch return error.OutOfMemory;
    errdefer gpa.destroy(cir_ptr);

    // Initialize CIR - this transfers ownership of module_env
    cir_ptr.* = CIR.init(&module_env);
    // Note: module_env is now owned by cir_ptr, don't defer deinit it

    // Create scope for semantic analysis
    var scope = Scope.init(cir_ptr.env.gpa);
    defer scope.deinit(cir_ptr.env.gpa);

    // Canonicalize the AST
    var canonicalizer = canonicalize.init(cir_ptr, &parse_ast, &scope);
    canonicalizer.canonicalize_file();

    // Get diagnostics from the heap-allocated CIR and convert to Reports
    const diagnostics = cir_ptr.getDiagnostics();
    for (diagnostics) |diagnostic| {
        const report = cir_ptr.diagnosticToReport(diagnostic, gpa) catch continue;
        reports.append(report) catch continue;
    }

    return ProcessResult{
        .cir = cir_ptr,
        .reports = reports.toOwnedSlice() catch return error.OutOfMemory,
    };
}

/// Process a file and render diagnostics directly to a writer.
/// This avoids memory management issues by not returning CIR or diagnostics.
pub fn renderDiagnostics(
    gpa: std.mem.Allocator,
    fs: Filesystem,
    filepath: []const u8,
    writer: anytype,
) !bool {
    // Read the file content
    const source = fs.readFile(filepath, gpa) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.AccessDenied => return error.AccessDenied,
        else => return error.FileReadError,
    };
    defer gpa.free(source);

    return renderDiagnosticsFromSource(gpa, source, filepath, writer);
}

/// Process source code and render diagnostics directly to a writer.
pub fn renderDiagnosticsFromSource(
    gpa: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    writer: anytype,
) !bool {
    _ = filename; // TODO: Use filename in diagnostic reports

    // Process source and get Reports
    var result = processSource(gpa, source) catch |err| {
        try writer.print("Error processing source: {}\n", .{err});
        return true; // Assume there are errors if processing fails
    };
    defer result.deinit(gpa);

    const has_errors = result.reports.len > 0;

    // Render all reports
    for (result.reports) |*report| {
        // Render using plain text for now to avoid terminal escape issues
        reporting.renderReportToPlainText(report, writer) catch |render_err| {
            try writer.print("Error rendering diagnostic report: {}\n", .{render_err});
            try writer.print("Report title: {s}\n", .{report.title});
        };
    }

    return has_errors;
}
