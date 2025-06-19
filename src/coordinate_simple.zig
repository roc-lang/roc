//! Simplified coordination system for single-file processing.

const std = @import("std");
const base = @import("base.zig");
const parse = @import("check/parse.zig");
const canonicalize = @import("check/canonicalize.zig");
const reporting = @import("reporting.zig");
const Filesystem = @import("coordinate/Filesystem.zig");

const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;

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

    return processSource(gpa, source, filepath);
}

/// Process source code directly and return diagnostics.
/// Useful for testing or when you already have the source in memory.
pub fn checkSource(
    gpa: std.mem.Allocator,
    source: []const u8,
) ![]const CIR.Diagnostic {
    // Initialize the ModuleEnv
    var module_env = ModuleEnv.init(gpa);
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
    // Canonicalize the AST
    var canonicalizer = canonicalize.init(&can_ir, &parse_ast);
    defer canonicalizer.deinit();
    canonicalizer.canonicalize_file();

    // Return diagnostics - the caller owns the returned slice
    return can_ir.getDiagnostics();
}

/// Process source code directly and return both CIR and Reports for proper reporting.
/// Useful when you need access to the CIR for diagnostic-to-report conversion.
pub fn processSource(
    gpa: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
) !ProcessResult {
    // Initialize the ModuleEnv
    var module_env = ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&module_env, source);
    defer parse_ast.deinit(gpa);

    // Create an arraylist for capturing diagnostic reports.
    var reports = std.ArrayList(reporting.Report).init(gpa);
    defer reports.deinit();

    // Get tokenize diagnostic Reports
    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        const report = parse_ast.tokenizeDiagnosticToReport(diagnostic, gpa) catch continue;
        reports.append(report) catch continue;
    }

    // Get parser diagnostic Reports
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        const report = parse_ast.parseDiagnosticToReport(diagnostic, gpa) catch continue;
        reports.append(report) catch continue;
    }

    // Initialize the Can IR
    var cir = CIR.init(&module_env);

    // Create scope for semantic analysis
    // Canonicalize the AST
    var canonicalizer = canonicalize.init(&cir, &parse_ast);
    defer canonicalizer.deinit();
    canonicalizer.canonicalize_file();

    // Get diagnostic Reports from CIR
    const diagnostics = cir.getDiagnostics();
    for (diagnostics) |diagnostic| {
        const report = cir.diagnosticToReport(diagnostic, gpa, source, filename) catch continue;
        reports.append(report) catch continue;
    }

    return ProcessResult{
        .cir = &cir,
        .reports = reports.toOwnedSlice() catch return error.OutOfMemory,
    };
}
