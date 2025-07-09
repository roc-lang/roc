//! Simplified coordination system for single-file processing.

const std = @import("std");
const base = @import("base.zig");
const tracy = @import("tracy.zig");
const parse = @import("check/parse.zig");
const canonicalize = @import("check/canonicalize.zig");
const Solver = @import("check/check_types.zig");
const types_problem_mod = @import("check/check_types/problem.zig");
const reporting = @import("reporting.zig");
const Filesystem = @import("coordinate/Filesystem.zig");

const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;

/// Timing information for different compilation phases
pub const TimingInfo = struct {
    tokenize_parse_ns: u64,
    canonicalize_ns: u64,
    canonicalize_diagnostics_ns: u64,
    type_checking_ns: u64,
    check_diagnostics_ns: u64,
};

/// Result of processing source code, containing both CIR and Reports
/// for proper diagnostic reporting.
///
/// This struct owns:
/// - The source text (which the reports reference but don't own)
/// - The CIR data
/// - The reports
///
/// The reports contain references to the source text, so ProcessResult
/// must outlive any usage of the reports.
pub const ProcessResult = struct {
    cir: *CIR,
    reports: []reporting.Report,
    source: []const u8,
    timing: ?TimingInfo = null,

    pub fn deinit(self: *ProcessResult, gpa: std.mem.Allocator) void {
        for (self.reports) |*report| {
            report.deinit();
        }
        gpa.free(self.reports);
        gpa.free(self.source);
        self.cir.deinit();
        gpa.destroy(self.cir);
    }
};

/// Process a single file and return both CIR and diagnostics for proper reporting.
///
/// This function reads the file and transfers ownership of the allocated memory
/// directly to ProcessResult, avoiding an unnecessary copy. This is an optimization
/// since source files can be large and the compiler processes many files.
pub fn processFile(
    gpa: std.mem.Allocator,
    fs: Filesystem,
    filepath: []const u8,
) !ProcessResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Read the file content
    const source = fs.readFile(filepath, gpa) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.AccessDenied => return error.AccessDenied,
        else => return error.FileReadError,
    };

    // Note: We transfer ownership of source to ProcessResult, avoiding an unnecessary copy
    return try processSourceInternal(gpa, source, filepath, true, false);
}

/// Process a single file with timing information.
pub fn processFileWithTiming(
    gpa: std.mem.Allocator,
    fs: Filesystem,
    filepath: []const u8,
) !ProcessResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Read the file content
    const source = fs.readFile(filepath, gpa) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.AccessDenied => return error.AccessDenied,
        else => return error.FileReadError,
    };

    // Note: We transfer ownership of source to ProcessResult, avoiding an unnecessary copy
    return try processSourceInternal(gpa, source, filepath, true, true);
}

/// Process source code directly and return both CIR and reports for proper reporting.
///
/// Unlike processFile, this function must clone the source since the caller
/// retains ownership of the input. Use this when you already have source text
/// in memory (e.g., from tests, REPL, or other tools).
///
/// The returned ProcessResult owns its own copy of the source.
pub fn processSource(
    gpa: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
) !ProcessResult {
    return try processSourceInternal(gpa, source, filename, false, false);
}

/// Internal helper that processes source code and produces a ProcessResult.
///
/// The take_ownership parameter controls memory management:
/// - true: Transfer ownership of 'source' to ProcessResult (no allocation)
/// - false: Clone 'source' so ProcessResult has its own copy
///
/// The collect_timing parameter controls whether to collect timing information:
/// - true: Collect timing information for each compilation phase
/// - false: Skip timing collection for faster processing
///
/// This design allows processFile to avoid an unnecessary copy while
/// processSource can safely work with borrowed memory.
fn processSourceInternal(
    gpa: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    take_ownership: bool,
    collect_timing: bool,
) !ProcessResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var timing_info: ?TimingInfo = null;
    var timer: ?std.time.Timer = null;

    if (collect_timing) {
        timer = std.time.Timer.start() catch null;
        timing_info = TimingInfo{
            .tokenize_parse_ns = 0,
            .canonicalize_ns = 0,
            .canonicalize_diagnostics_ns = 0,
            .type_checking_ns = 0,
            .check_diagnostics_ns = 0,
        };
    }

    // Initialize the ModuleEnv
    var module_env = ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Calculate line starts for region info
    try module_env.calcLineStarts(source);

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
        const report = parse_ast.parseDiagnosticToReport(diagnostic, gpa, "<source>") catch continue;
        reports.append(report) catch continue;
    }

    if (collect_timing and timer != null and timing_info != null) {
        timing_info.?.tokenize_parse_ns = timer.?.read();
        timer.?.reset();
    }

    // Initialize the Can IR (heap-allocated)
    var cir = try gpa.create(CIR);
    cir.* = CIR.init(&module_env);

    // Create scope for semantic analysis
    // Canonicalize the AST
    var canonicalizer = try canonicalize.init(cir, &parse_ast);
    defer canonicalizer.deinit();
    try canonicalizer.canonicalizeFile();

    if (collect_timing and timer != null and timing_info != null) {
        timing_info.?.canonicalize_ns = timer.?.read();
        timer.?.reset();
    }

    // Get diagnostic Reports from CIR
    const diagnostics = cir.getDiagnostics();
    defer gpa.free(diagnostics);
    for (diagnostics) |diagnostic| {
        const report = cir.diagnosticToReport(diagnostic, gpa, source, filename) catch continue;
        reports.append(report) catch continue;
    }

    if (collect_timing and timer != null and timing_info != null) {
        timing_info.?.canonicalize_diagnostics_ns = timer.?.read();
        timer.?.reset();
    }

    // Type checking
    var solver = try Solver.init(gpa, &module_env.types, cir);
    defer solver.deinit();

    // Check for type errors
    try solver.checkDefs();

    if (collect_timing and timer != null and timing_info != null) {
        timing_info.?.type_checking_ns = timer.?.read();
        timer.?.reset();
    }

    // Ensure ProcessResult owns the source
    // We have two cases:
    // 1. processFile already allocated the source memory - we take ownership to avoid a copy
    // 2. processSource borrows the caller's source - we must clone it
    // This optimization matters because source files can be large and we process many of them.
    const owned_source = if (take_ownership)
        source // Transfer existing ownership (no allocation)
    else
        try gpa.dupe(u8, source); // Clone to get our own copy

    // Get type checking diagnostic Reports
    var report_builder = types_problem_mod.ReportBuilder.init(
        gpa,
        &module_env,
        cir,
        &solver.snapshots,
        owned_source,
        filename,
    );
    defer report_builder.deinit();

    var problems_itr = solver.problems.problems.iterIndices();
    while (problems_itr.next()) |problem_idx| {
        const problem = solver.problems.problems.get(problem_idx);
        const report = report_builder.build(problem) catch continue;
        reports.append(report) catch continue;
    }

    if (collect_timing and timer != null and timing_info != null) {
        timing_info.?.check_diagnostics_ns = timer.?.read();
    }

    return ProcessResult{
        .cir = cir,
        .reports = reports.toOwnedSlice() catch return error.OutOfMemory,
        .source = owned_source,
        .timing = timing_info,
    };
}
