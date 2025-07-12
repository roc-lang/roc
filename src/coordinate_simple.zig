//! Simplified coordination system for single-file processing.

const std = @import("std");
const base = @import("base.zig");
const tracy = @import("tracy.zig");
const parse = @import("check/parse.zig");
const canonicalize = @import("check/canonicalize.zig");
const Solver = @import("check/check_types.zig");
const types_problem_mod = @import("check/check_types/problem.zig");
const reporting = @import("reporting.zig");
const Filesystem = @import("fs/Filesystem.zig");
const build_options = @import("build_options");

const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;
const cache_mod = @import("cache/mod.zig");
const CacheManager = cache_mod.CacheManager;
const CacheConfig = cache_mod.CacheConfig;

const CacheResult = cache_mod.CacheResult;
const CacheHit = cache_mod.CacheHit;

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
    timing: ?TimingInfo = null,
    error_count: u32 = 0,
    warning_count: u32 = 0,
    was_cached: bool = false,

    pub fn deinit(self: *ProcessResult, gpa: std.mem.Allocator) void {
        for (self.reports) |*report| {
            report.deinit();
        }
        gpa.free(self.reports);

        // Clean up the heap-allocated ModuleEnv (only when loaded from cache)
        if (self.was_cached) {
            self.cir.env.deinit();
            gpa.destroy(self.cir.env);
        }

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
    cache_manager: ?*CacheManager,
    collect_timing: bool,
) !ProcessResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Read the file content
    const source = fs.readFile(filepath, gpa) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.AccessDenied => return error.AccessDenied,
        else => return error.FileReadError,
    };

    const config = ProcessConfig{
        .collect_timing = collect_timing,
    };

    // If caching is enabled, try cache first
    if (cache_manager) |cache| {
        // Duplicate filepath and compiler_version since cache.lookup takes ownership
        const filepath_copy = try gpa.dupe(u8, filepath);
        errdefer gpa.free(filepath_copy);

        const compiler_version = try gpa.dupe(u8, getCompilerVersion());
        errdefer gpa.free(compiler_version);

        // Check cache - loadFromCache takes ownership of source, filepath_copy, and compiler_version
        const cache_result = cache.loadFromCache(source, filepath_copy, compiler_version);

        switch (cache_result) {
            .hit => |process_result| {
                // Cache hit! Ownership of source, filepath_copy, and compiler_version
                // has been transferred to the cached result
                return process_result;
            },
            .miss => |returned| {
                // Cache miss - we get back ownership of source, filepath_copy and compiler_version
                gpa.free(returned.module_path); // Free the filepath_copy
                defer gpa.free(returned.compiler_version);

                // Process normally with returned source and original filepath
                var process_result = try processSourceInternal(gpa, returned.source, filepath, config);
                process_result.was_cached = false;

                // Store in cache (don't fail compilation if cache store fails)
                cache.store(returned.source, returned.compiler_version, &process_result) catch |err| {
                    std.log.debug("Failed to store cache for {s}: {}", .{ filepath, err });
                };

                return process_result;
            },
            .invalid => |returned| {
                // Cache invalid - we get back ownership of source, filepath_copy and compiler_version
                gpa.free(returned.module_path); // Free the filepath_copy
                defer gpa.free(returned.compiler_version);

                // Process normally with returned source and original filepath
                var process_result = try processSourceInternal(gpa, returned.source, filepath, config);
                process_result.was_cached = false;

                // Store in cache (don't fail compilation if cache store fails)
                cache.store(returned.source, returned.compiler_version, &process_result) catch |err| {
                    std.log.debug("Failed to store cache for {s}: {}", .{ filepath, err });
                };

                return process_result;
            },
        }
    }

    // No caching - process normally
    return try processSourceInternal(gpa, source, filepath, config);
}

/// Process source code directly and return both CIR and reports for proper reporting.
///
/// Unlike processFile, this function must clone the source since the caller
/// retains ownership of the input. Use this when you already have source text
/// in memory (e.g., from tests, REPL, or other tools).
///
/// The source is duplicated and owned by the ModuleEnv in the returned ProcessResult.
///
/// `processSource` is used by the fuzzer.
pub fn processSource(
    gpa: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
) !ProcessResult {
    return try processSourceInternal(gpa, source, filename, .{ .collect_timing = false });
}

/// Configuration for processSourceInternal
pub const ProcessConfig = struct {
    collect_timing: bool = false,
};

/// Helper function to collect timing information and reset timer
fn collectTiming(config: ProcessConfig, timer: *?std.time.Timer, timing_info: *?TimingInfo, field: []const u8) void {
    if (config.collect_timing and timer.* != null and timing_info.* != null) {
        const elapsed = timer.*.?.read();
        if (std.mem.eql(u8, field, "tokenize_parse_ns")) {
            timing_info.*.?.tokenize_parse_ns = elapsed;
        } else if (std.mem.eql(u8, field, "canonicalize_ns")) {
            timing_info.*.?.canonicalize_ns = elapsed;
        } else if (std.mem.eql(u8, field, "canonicalize_diagnostics_ns")) {
            timing_info.*.?.canonicalize_diagnostics_ns = elapsed;
        } else if (std.mem.eql(u8, field, "type_checking_ns")) {
            timing_info.*.?.type_checking_ns = elapsed;
        } else if (std.mem.eql(u8, field, "check_diagnostics_ns")) {
            timing_info.*.?.check_diagnostics_ns = elapsed;
        }
        timer.*.?.reset();
    }
}

/// Internal helper that processes source code and produces a ProcessResult.
///
/// The config.collect_timing parameter controls whether to collect timing information:
/// - true: Collect timing information for each compilation phase
/// - false: Skip timing collection for faster processing
///
/// The source is always duplicated for the ModuleEnv, which owns the copy.
fn processSourceInternal(
    gpa: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    config: ProcessConfig,
) !ProcessResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var timing_info: ?TimingInfo = null;
    var timer: ?std.time.Timer = null;

    if (config.collect_timing) {
        timer = std.time.Timer.start() catch null;
        timing_info = TimingInfo{
            .tokenize_parse_ns = 0,
            .canonicalize_ns = 0,
            .canonicalize_diagnostics_ns = 0,
            .type_checking_ns = 0,
            .check_diagnostics_ns = 0,
        };
    }

    // Initialize the ModuleEnv (heap-allocated for ownership transfer)
    var module_env = try gpa.create(ModuleEnv);

    // Always duplicate source and filename since ModuleEnv owns them
    const owned_source_for_env = try gpa.dupe(u8, source);
    const owned_filename = try gpa.dupe(u8, filename);

    module_env.* = ModuleEnv.init(gpa, owned_source_for_env, owned_filename);

    // Calculate line starts for region info
    try module_env.*.calcLineStarts(source);

    // Parse the source code
    var parse_ast = parse.parse(module_env, source);
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
        const report = parse_ast.parseDiagnosticToReport(module_env, diagnostic, gpa, "<source>") catch continue;
        reports.append(report) catch continue;
    }

    collectTiming(config, &timer, &timing_info, "tokenize_parse_ns");

    // Initialize the Can IR (heap-allocated)
    var cir = try gpa.create(CIR);
    // Extract module name from filename (remove path and extension)
    const basename = std.fs.path.basename(filename);
    const module_name = if (std.mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
        basename[0..dot_idx]
    else
        basename;
    cir.* = CIR.init(module_env, module_name);

    // Create scope for semantic analysis
    // Canonicalize the AST
    var canonicalizer = try canonicalize.init(cir, &parse_ast, null);
    defer canonicalizer.deinit();
    try canonicalizer.canonicalizeFile();

    collectTiming(config, &timer, &timing_info, "canonicalize_ns");

    // Get diagnostic Reports from CIR
    const diagnostics = cir.getDiagnostics();
    defer gpa.free(diagnostics);
    for (diagnostics) |diagnostic| {
        const report = cir.diagnosticToReport(diagnostic, gpa, source, filename) catch continue;
        reports.append(report) catch continue;
    }

    collectTiming(config, &timer, &timing_info, "canonicalize_diagnostics_ns");

    // Type checking
    const empty_modules: []const *CIR = &.{};
    var solver = try Solver.init(gpa, &module_env.types, cir, empty_modules);
    defer solver.deinit();

    // Check for type errors
    try solver.checkDefs();

    collectTiming(config, &timer, &timing_info, "type_checking_ns");

    // Get type checking diagnostic Reports
    var report_builder = types_problem_mod.ReportBuilder.init(
        gpa,
        module_env,
        cir,
        &solver.snapshots,
        module_env.source,
        filename,
        empty_modules,
    );
    defer report_builder.deinit();

    var problems_itr = solver.problems.problems.iterIndices();
    while (problems_itr.next()) |problem_idx| {
        const problem = solver.problems.problems.get(problem_idx);
        const report = report_builder.build(problem) catch continue;
        reports.append(report) catch continue;
    }

    collectTiming(config, &timer, &timing_info, "check_diagnostics_ns");

    const final_reports = reports.toOwnedSlice() catch return error.OutOfMemory;

    // Count errors and warnings
    var error_count: u32 = 0;
    var warning_count: u32 = 0;
    for (final_reports) |report| {
        switch (report.severity) {
            .info => {}, // Informational messages don't affect error/warning counts
            .runtime_error, .fatal => error_count += 1,
            .warning => warning_count += 1,
        }
    }

    return ProcessResult{
        .cir = cir,
        .reports = final_reports,
        .timing = timing_info,
        .error_count = error_count,
        .warning_count = warning_count,
        .was_cached = false,
    };
}

/// Get a compiler version string for cache key generation.
/// Uses the build-time compiler version that includes git commit SHA.
fn getCompilerVersion() []const u8 {
    return build_options.compiler_version;
}
