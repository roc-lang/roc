//! Snapshot testing infrastructure for the Roc compiler.
//!
//! This module provides functionality to generate and validate snapshot tests
//! that capture the compiler's behavior at each stage of compilation. Snapshots
//! help ensure the compiler continues to behave as expected by showing the
//! output of tokenization, parsing, canonicalization, type checking etc for
//! the given Roc code snippet.

const std = @import("std");

/// Application IO instance, initialized from `std.process.Init` in `main`.
/// Use this instead of `app_io` for all application IO.
var app_io: std.Io = undefined;

const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const types = @import("types");
const reporting = @import("reporting");
const check = @import("check");
const compile = @import("compile");
const lir = @import("lir");
const layout = @import("layout");
const backend = @import("backend");
const fmt = @import("fmt");
const eval_mod = @import("eval");
const docs_mod = @import("docs");
const tracy = @import("tracy");
const sljmp = @import("sljmp");

/// Custom panic handler that enables catching Zig panics (e.g., `unreachable` in
/// the dev backend) during snapshot testing. When `panic_jmp` is set, longjmps back
/// to the saved point instead of aborting; otherwise falls through to the default handler.
pub const panic = std.debug.FullPanic(panicHandler);

threadlocal var panic_jmp: ?*sljmp.JmpBuf = null;
threadlocal var panic_msg: ?[]const u8 = null;
/// Set by signal handlers (SIGALRM/SIGSEGV) when a longjmp interrupts an
/// allocation.  Once true the GPA mutex is permanently locked and any
/// alloc/free through it will deadlock, so all further GPA use must be
/// skipped for the rest of this thread's lifetime.
threadlocal var gpa_poisoned: bool = false;

fn panicHandler(msg: []const u8, ret_addr: ?usize) noreturn {
    if (panic_jmp) |jmp| {
        panic_msg = msg;
        if (verbose_log) {
            std.debug.print("  PANIC STACK: {s}\n", .{msg});
            if (ret_addr) |addr| {
                std.debug.print("  return address: 0x{x}\n", .{addr});
            }
            std.debug.dumpCurrentStackTrace(.{ .first_address = ret_addr });
        }
        panic_jmp = null; // prevent re-entry
        sljmp.longjmp(jmp, 1);
    }
    // No protection active — use default behavior.
    std.debug.defaultPanic(msg, @returnAddress());
}

const CacheModule = compile.manager.CacheModule;
const roc_target = @import("roc_target");
const CoreCtx = compile.CoreCtx;
const CommonEnv = base.CommonEnv;
const Check = check.Check;
const CIR = can.CIR;
const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const SExprTree = base.SExprTree;
const LineColMode = base.SExprTree.LineColMode;
const single_module = compile.single_module;
const AST = parse.AST;
const Report = reporting.Report;
const parallel = base.parallel;

var verbose_log: bool = false;
var prng = std.Random.DefaultPrng.init(1234567890);
var snapshot_temp_counter = std.atomic.Value(usize).init(0);

const rand = prng.random();

const SnapshotError =
    Allocator.Error ||
    Error ||
    fmt.FormatAstError ||
    eval_mod.BuiltinModules.InitError ||
    eval_mod.test_helpers.TestHelperError ||
    compile.build.InitError ||
    compile.build.BuildError ||
    compile.build.CompileDiscoveredError ||
    compile.package.TypeCheckModuleError ||
    docs_mod.render_html.RenderError ||
    std.Thread.SpawnError ||
    std.process.Args.ToSliceError ||
    std.process.CurrentPathError ||
    std.Io.Dir.AccessError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.DeleteFileError ||
    std.Io.Dir.Iterator.Error ||
    std.Io.Dir.OpenError ||
    std.Io.Dir.ReadFileAllocError ||
    std.Io.Dir.StatFileError ||
    std.Io.Dir.WriteFileError ||
    std.Io.File.OpenError ||
    std.Io.File.ReadPositionalError ||
    std.Io.File.Reader.Error ||
    std.Io.File.StatError ||
    std.Io.File.Writer.Error ||
    error{
        BuiltinModuleLeakedInSnapshots,
        BufferTooSmall,
        CacheRoundTripValidationFailed,
        CacheVersionHashMismatch,
        ErrFinalizingHTMLWriter,
        InvalidMagicNumber,
        MissingBuiltinModule,
        MonoFormattingFailed,
        MonoValidationFailed,
        NoSpaceLeft,
        NotDir,
        ParseFailed,
        SnapshotValidationFailed,
        TempDirUnavailable,
        TypeCheckError,
        WriteFailed,
    };

/// Logs a message if verbose logging is enabled.
fn log(comptime fmt_str: []const u8, args: anytype) void {
    if (verbose_log) {
        std.log.debug(fmt_str, args);
    }
}

/// Returns an absolute, normalized path without calling libc `realpath`.
fn absolutePathAlloc(io: std.Io, sub_path: []const u8, allocator: Allocator) SnapshotError![]u8 {
    if (std.fs.path.isAbsolute(sub_path)) {
        return std.fs.path.resolve(allocator, &.{sub_path});
    }

    const cwd = try currentPathAllocSafe(io, allocator);
    defer allocator.free(cwd);
    return std.fs.path.resolve(allocator, &.{ cwd, sub_path });
}

fn currentPathAllocSafe(io: std.Io, allocator: Allocator) SnapshotError![]u8 {
    var buffer: [std.fs.max_path_bytes]u8 = @splat(0);
    const n = try std.process.currentPath(io, &buffer);
    return allocator.dupe(u8, buffer[0..n]);
}

/// Always logs a warning message.
fn warn(comptime fmt_str: []const u8, args: anytype) void {
    std.log.warn(fmt_str, args);
}

fn getTempRoot(allocator: Allocator) (Allocator.Error || error{TempDirUnavailable})![]u8 {
    const names: []const [:0]const u8 = if (comptime @import("builtin").os.tag == .windows)
        &.{ "TEMP", "TMP" }
    else
        &.{ "TMPDIR", "TEMP", "TMP" };

    for (names) |name| {
        if (std.c.getenv(name.ptr)) |value_z| {
            const value = std.mem.sliceTo(value_z, 0);
            if (value.len != 0) return allocator.dupe(u8, value);
        }
    }

    return error.TempDirUnavailable;
}

fn makeSnapshotTempDir(allocator: Allocator, prefix: []const u8, output_path: []const u8) SnapshotError![]u8 {
    const temp_root = try getTempRoot(allocator);
    defer allocator.free(temp_root);

    try std.Io.Dir.cwd().createDirPath(app_io, temp_root);
    const temp_root_abs = if (std.fs.path.isAbsolute(temp_root))
        try allocator.dupe(u8, temp_root)
    else
        try absolutePathAlloc(app_io, temp_root, allocator);
    defer allocator.free(temp_root_abs);

    const counter = snapshot_temp_counter.fetchAdd(1, .monotonic);
    const dirname = try std.fmt.allocPrint(allocator, "{s}_{d}_{d}", .{
        prefix,
        @as(u64, @intCast(@intFromPtr(output_path.ptr))),
        counter,
    });
    defer allocator.free(dirname);

    return std.fs.path.join(allocator, &.{ temp_root_abs, dirname });
}

const UpdateCommand = enum {
    /// Update the section to match the actual output/problems.
    update,
    /// Check that the section matches the actual output/problems.
    check,
    /// Don't do anything with the section.
    none,
};

/// Represents a problem entry from PROBLEMS section
const ProblemEntry = struct {
    problem_type: []const u8,
    file: []const u8,
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,

    fn format(self: ProblemEntry, writer: anytype) (Allocator.Error || error{WriteFailed})!void {
        try writer.print("{s} - {s}:{d}:{d}:{d}:{d}", .{
            self.problem_type,
            self.file,
            self.start_line,
            self.start_col,
            self.end_line,
            self.end_col,
        });
    }
};

/// Generate EXPECTED content from problems
fn generateExpectedContent(allocator: std.mem.Allocator, problems: []const ProblemEntry) (Allocator.Error || error{WriteFailed})![]const u8 {
    if (problems.len == 0) {
        return try allocator.dupe(u8, "NIL");
    }

    var buffer: std.Io.Writer.Allocating = .init(allocator);
    errdefer buffer.deinit();

    for (problems, 0..) |problem, i| {
        if (i > 0) {
            buffer.writer.writeAll("\n") catch return error.OutOfMemory;
        }
        try problem.format(&buffer.writer);
    }

    return buffer.toOwnedSlice();
}

/// Generate all reports from the compilation pipeline
fn generateAllReports(
    allocator: std.mem.Allocator,
    parse_ast: *AST,
    can_ir: *ModuleEnv,
    solver: *Check,
    snapshot_path: []const u8,
    module_env: *ModuleEnv,
) Allocator.Error!std.array_list.Managed(reporting.Report) {
    var reports = std.array_list.Managed(reporting.Report).init(allocator);
    errdefer reports.deinit();

    // Generate tokenize reports
    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        const report = parse_ast.tokenizeDiagnosticToReport(diagnostic, allocator, snapshot_path) catch |err| {
            std.debug.panic("Failed to create tokenize report for snapshot {s}: {s}", .{ snapshot_path, @errorName(err) });
        };
        try reports.append(report);
    }

    // Generate parse reports
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        const report = parse_ast.parseDiagnosticToReport(&module_env.common, diagnostic, allocator, snapshot_path) catch |err| {
            std.debug.panic("Failed to create parse report for snapshot {s}: {s}", .{ snapshot_path, @errorName(err) });
        };
        try reports.append(report);
    }

    // Generate canonicalization reports
    const diagnostics = try can_ir.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        const report = can_ir.diagnosticToReport(diagnostic, allocator, snapshot_path) catch |err| {
            std.debug.panic("Failed to create canonicalization report for snapshot {s}: {s}", .{ snapshot_path, @errorName(err) });
        };
        try reports.append(report);
    }

    _ = try solver.problems.flushPendingStaticExhaustiveness(allocator);

    // Generate type checking reports
    for (solver.problems.problems.items) |problem| {
        const empty_modules: []const *ModuleEnv = &.{};
        var report_builder = try check.ReportBuilder.init(
            allocator,
            module_env,
            can_ir,
            &solver.snapshots,
            &solver.problems,
            snapshot_path,
            empty_modules,
            &solver.import_mapping,
            &solver.regions,
            null,
        );
        defer report_builder.deinit();

        const report = report_builder.build(problem) catch |err| {
            std.debug.panic("Failed to create type checking report for snapshot {s}: {s}", .{ snapshot_path, @errorName(err) });
        };
        try reports.append(report);
    }

    return reports;
}

/// Render reports to PROBLEMS section format (markdown and HTML)
fn renderReportsToProblemsSection(output: *DualOutput, reports: *const std.array_list.Managed(reporting.Report)) error{WriteFailed}!void {
    // HTML PROBLEMS section
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <div class="problems">
        );
    }

    if (reports.items.len == 0) {
        try output.md_writer.writer.writeAll("NIL\n");
        if (output.html_writer) |writer| {
            try writer.writer.writeAll("                    <p>NIL</p>\n");
        }
        log("reported NIL problems", .{});
    } else {
        // Render all reports in order, as plain-text boxes.
        for (reports.items) |*report| {
            reporting.renderReportToBoxPlain(report, &output.md_writer.writer, reporting.ReportingConfig.initMarkdown()) catch |err| {
                std.debug.panic("Failed to render report: {s}", .{@errorName(err)});
            };

            if (output.html_writer) |writer| {
                try writer.writer.writeAll("                    <div class=\"problem\">");
                report.render(&writer.writer, .markdown) catch |err| {
                    std.debug.panic("Failed to render report to HTML: {s}", .{@errorName(err)});
                };
                try writer.writer.writeAll("</div>\n");
            }
        }
    }

    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                </div>
            \\
        );
    }
}

/// For snapshot files, the EXPECTED `file` field is just the basename.
fn sanitiseSnapshotPath(path: []const u8) []const u8 {
    if (std.mem.find(u8, path, "/snapshots/") != null or
        std.mem.find(u8, path, "\\snapshots\\") != null)
    {
        return std.fs.path.basename(path);
    }
    return path;
}

/// A report title may be compound (e.g. "NOT IMPLEMENTED - UNDEFINED
/// VARIABLE"); EXPECTED uses only the final segment after the last " - ".
fn lastTitleSegment(title: []const u8) []const u8 {
    if (std.mem.findLast(u8, title, " - ")) |i| {
        return std.mem.trim(u8, title[i + 3 ..], " \t\r\n");
    }
    return std.mem.trim(u8, title, " \t\r\n");
}

/// Dupe `s` with ASCII letters uppercased. The EXPECTED section shouts titles
/// in ALL CAPS, matching the box renderer, even though titles are authored in
/// title case.
fn asciiUpperDupe(allocator: std.mem.Allocator, s: []const u8) Allocator.Error![]u8 {
    const out = try allocator.dupe(u8, s);
    for (out) |*c| c.* = std.ascii.toUpper(c.*);
    return out;
}

const RegionLoc = struct { file: []const u8, sl: u32, sc: u32, el: u32, ec: u32 };

/// Pull the first source region out of a report for the EXPECTED location.
fn reportRegionLoc(report: *const reporting.Report) RegionLoc {
    for (report.document.elements.items) |el| {
        switch (el) {
            .source_code_region => |r| return .{
                .file = sanitiseSnapshotPath(r.filename orelse ""),
                .sl = r.start_line,
                .sc = r.start_column,
                .el = r.end_line,
                .ec = r.end_column,
            },
            .source_code_with_underlines => |d| {
                const dr = d.display_region;
                return .{
                    .file = sanitiseSnapshotPath(dr.filename orelse ""),
                    .sl = dr.start_line,
                    .sc = dr.start_column,
                    .el = dr.start_line,
                    .ec = dr.start_column,
                };
            },
            else => {},
        }
    }
    return .{ .file = "", .sl = 0, .sc = 0, .el = 0, .ec = 0 };
}

/// Render reports to EXPECTED section format, generated directly from the
/// report data (title + first source region) rather than by parsing the
/// rendered PROBLEMS output.
fn renderReportsToExpectedContent(allocator: std.mem.Allocator, reports: *const std.array_list.Managed(reporting.Report)) (Allocator.Error || error{WriteFailed})![]const u8 {
    if (reports.items.len == 0) {
        return try allocator.dupe(u8, "NIL");
    }

    var entries = std.array_list.Managed(ProblemEntry).init(allocator);
    defer {
        for (entries.items) |e| {
            allocator.free(e.problem_type);
            allocator.free(e.file);
        }
        entries.deinit();
    }

    for (reports.items) |*report| {
        const loc = reportRegionLoc(report);
        try entries.append(.{
            .problem_type = try asciiUpperDupe(allocator, lastTitleSegment(report.title)),
            .file = try allocator.dupe(u8, loc.file),
            .start_line = loc.sl,
            .start_col = loc.sc,
            .end_line = loc.el,
            .end_col = loc.ec,
        });
    }

    return try generateExpectedContent(allocator, entries.items);
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .{
    .backing_allocator = std.heap.page_allocator,
};

const default_snapshot_max_threads = 4;

fn defaultSnapshotThreadCount() usize {
    const cpu_count = std.Thread.getCpuCount() catch 1;
    return @min(cpu_count, default_snapshot_max_threads);
}

/// cli entrypoint for snapshot tool
pub fn main(init: std.process.Init) SnapshotError!void {
    app_io = init.io;

    // Always use the debug allocator with the snapshot tool to help find allocation bugs.
    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa = debug_allocator.allocator();
    defer {
        const mem_state = debug_allocator.deinit();
        std.debug.assert(mem_state == .ok);
    }

    // Wrap with Tracy for allocation profiling when enabled
    if (tracy.enable_allocation) {
        gpa_tracy = tracy.tracyAllocator(gpa);
        gpa = gpa_tracy.allocator();
    }

    // `toSlice` packs slices and contents into a single arena allocation; freeing
    // through a debug GPA panics because the returned slice does not match any
    // original `alloc()` call. Use the process arena (cleaned up at exit) instead.
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    var snapshot_paths = std.array_list.Managed([]const u8).init(gpa);
    defer snapshot_paths.deinit();

    var maybe_fuzz_corpus_path: ?[]const u8 = null;
    var expect_fuzz_corpus_path: bool = false;
    var generate_html: bool = false;
    var debug_mode: bool = false;
    var max_threads: usize = 0;
    var expect_threads: bool = false;
    var expected_section_command = UpdateCommand.none;
    var output_section_command = UpdateCommand.none;
    var trace_eval: bool = false;
    var linecol_mode: LineColMode = .skip_linecol;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_log = true;
        } else if (std.mem.eql(u8, arg, "--html")) {
            generate_html = true;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_mode = true;
        } else if (std.mem.eql(u8, arg, "--trace-eval")) {
            trace_eval = true;
        } else if (std.mem.eql(u8, arg, "--linecol")) {
            linecol_mode = .include_linecol;
        } else if (std.mem.eql(u8, arg, "--threads")) {
            if (max_threads != 0) {
                std.log.err("`--threads` should only be specified once.", .{});
                std.process.exit(1);
            }
            expect_threads = true;
        } else if (std.mem.eql(u8, arg, "--check-expected")) {
            if (expected_section_command != .none) {
                std.log.err("`--check-expected` and `--update-expected` are mutually exclusive and should only be specified once.", .{});
                std.process.exit(1);
            }
            expected_section_command = .check;
        } else if (std.mem.eql(u8, arg, "--update-expected")) {
            if (expected_section_command != .none) {
                std.log.err("`--check-expected` and `--update-expected` are mutually exclusive and should only be specified once.", .{});
                std.process.exit(1);
            }
            expected_section_command = .update;
        } else if (std.mem.eql(u8, arg, "--check-output")) {
            if (output_section_command != .none) {
                std.log.err("`--check-output` and `--update-output` are mutually exclusive and should only be specified once.", .{});
                std.process.exit(1);
            }
            output_section_command = .check;
        } else if (std.mem.eql(u8, arg, "--update-output")) {
            if (output_section_command != .none) {
                std.log.err("`--check-output` and `--update-output` are mutually exclusive and should only be specified once.", .{});
                std.process.exit(1);
            }
            output_section_command = .update;
        } else if (std.mem.eql(u8, arg, "--fuzz-corpus")) {
            if (maybe_fuzz_corpus_path != null) {
                std.log.err("`--fuzz-corpus` should only be specified once.", .{});
                std.process.exit(1);
            }
            expect_fuzz_corpus_path = true;
        } else if (expect_fuzz_corpus_path) {
            maybe_fuzz_corpus_path = arg;
            expect_fuzz_corpus_path = false;
        } else if (expect_threads) {
            max_threads = std.fmt.parseInt(usize, arg, 10) catch |err| {
                std.log.err("Invalid thread count '{s}': {s}", .{ arg, @errorName(err) });
                std.process.exit(1);
            };
            expect_threads = false;
        } else if (std.mem.eql(u8, arg, "--help")) {
            const usage =
                \\Usage: roc snapshot [options] [snapshot_paths...]
                \\
                \\Options:
                \\  --verbose       Enable verbose logging
                \\  --html          Generate HTML output files
                \\  --debug         Disable per-thread arenas to surface allocation bugs
                \\  --trace-eval    Enable interpreter trace output (only works with single REPL snapshot)
                \\  --linecol       Include line/column information in output
                \\  --threads <n>   Number of threads to use (0 = auto, capped at 4; 1 = single-threaded). Default: 0.
                \\  --check-expected     Validate that EXPECTED/DEV OUTPUT sections match actual output
                \\  --update-expected    Update EXPECTED/DEV OUTPUT sections with actual output
                \\  --fuzz-corpus <path>  Specify the path to the fuzz corpus
                \\
                \\Arguments:
                \\  snapshot_paths  Paths to snapshot files or directories
            ;
            std.log.debug(usage, .{});
            std.process.exit(0);
        } else {
            try snapshot_paths.append(arg);
        }
    }

    if (expect_fuzz_corpus_path) {
        std.log.err("Expected fuzz corpus path, but none was provided", .{});
        std.process.exit(1);
    }

    if (expect_threads) {
        std.log.err("Expected thread count, but none was provided", .{});
        std.process.exit(1);
    }

    // Force single-threaded mode in debug mode
    if (debug_mode and max_threads == 0) {
        max_threads = 1;
    }

    if (max_threads == 0) {
        // Each snapshot can compile and evaluate a complete program. Bound auto
        // mode so peak memory stays stable on machines with many cores.
        max_threads = defaultSnapshotThreadCount();
    }

    // Validate --trace-eval flag usage
    if (trace_eval) {
        if (snapshot_paths.items.len == 0) {
            std.log.err("--trace-eval requires exactly one snapshot file to be specified", .{});
            std.process.exit(1);
        }
        if (snapshot_paths.items.len > 1) {
            std.log.err("--trace-eval can only be used with a single snapshot file. Got {} files.", .{snapshot_paths.items.len});
            std.log.err("Usage: roc snapshot --trace-eval <path_to_single_repl_snapshot.md>", .{});
            std.process.exit(1);
        }
    }

    // Load builtin modules using the same code path as roc check
    const builtin_modules_ptr = try gpa.create(eval_mod.BuiltinModules);
    defer gpa.destroy(builtin_modules_ptr);

    builtin_modules_ptr.* = try eval_mod.BuiltinModules.init(gpa);
    defer builtin_modules_ptr.deinit();

    const cwd = try currentPathAllocSafe(app_io, gpa);
    defer gpa.free(cwd);

    const config = Config{
        .maybe_fuzz_corpus_path = maybe_fuzz_corpus_path,
        .generate_html = generate_html,
        .expected_section_command = expected_section_command,
        .output_section_command = output_section_command,
        .trace_eval = trace_eval,
        .linecol_mode = linecol_mode,
        .builtin_module = builtin_modules_ptr.builtin_module.env,
        .builtin_indices = builtin_modules_ptr.builtin_indices,
        .builtin_modules_ref = builtin_modules_ptr,
        .cwd = cwd,
    };

    if (config.maybe_fuzz_corpus_path != null) {
        log("copying SOURCE from snapshots to: {s}", .{config.maybe_fuzz_corpus_path.?});
        try std.Io.Dir.cwd().createDirPath(app_io, config.maybe_fuzz_corpus_path.?);
    }
    const snapshots_dir = "test/snapshots";

    // Stage 1: Collect work items
    var work_list = WorkList.init(gpa);
    defer {
        // Clean up any remaining work items
        for (work_list.items) |work_item| {
            gpa.free(work_item.path);
        }
        work_list.deinit();
    }

    if (snapshot_paths.items.len > 0) {
        for (snapshot_paths.items) |path| {
            try collectWorkItems(gpa, path, &work_list);
        }
    } else {
        // process all files in snapshots_dir
        try collectWorkItems(gpa, snapshots_dir, &work_list);
    }

    // Stage 2: Process work items (in parallel or single-threaded)
    const result = try processWorkItems(gpa, work_list, max_threads, debug_mode, &config);

    if (result.failed > 0) {
        std.log.err("Failed to process {d} snapshots.", .{result.failed});
        std.process.exit(1);
    }
}

fn checkSnapshotExpectations(gpa: Allocator) SnapshotError!bool {
    // Load builtin modules using the same code path as roc check
    const builtin_modules_ptr = try gpa.create(eval_mod.BuiltinModules);
    defer gpa.destroy(builtin_modules_ptr);

    builtin_modules_ptr.* = try eval_mod.BuiltinModules.init(gpa);
    defer builtin_modules_ptr.deinit();

    const cwd = try currentPathAllocSafe(app_io, gpa);
    defer gpa.free(cwd);

    const config = Config{
        .maybe_fuzz_corpus_path = null,
        .generate_html = false,
        .expected_section_command = .check,
        .output_section_command = .check,
        .disable_updates = true,
        .builtin_module = builtin_modules_ptr.builtin_module.env,
        .builtin_indices = builtin_modules_ptr.builtin_indices,
        .builtin_modules_ref = builtin_modules_ptr,
        .cwd = cwd,
    };
    const snapshots_dir = "test/snapshots";
    var work_list = WorkList.init(gpa);
    defer {
        for (work_list.items) |work_item| {
            gpa.free(work_item.path);
        }
        work_list.deinit();
    }
    try collectWorkItems(gpa, snapshots_dir, &work_list);

    const result = try processWorkItems(gpa, work_list, defaultSnapshotThreadCount(), false, &config);
    return result.failed == 0;
}

/// Check if a file has a valid snapshot extension
fn isSnapshotFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".md") and !std.mem.endsWith(u8, path, "README.md");
}

fn isMultiFileSnapshot(path: []const u8) bool {
    return std.mem.endsWith(u8, path, "_package") or
        std.mem.endsWith(u8, path, "_platform") or
        std.mem.endsWith(u8, path, "_app");
}

fn getMultiFileSnapshotType(path: []const u8) NodeType {
    if (std.mem.endsWith(u8, path, "_package")) return .package;
    if (std.mem.endsWith(u8, path, "_platform")) return .platform;
    if (std.mem.endsWith(u8, path, "_app")) return .app;
    return .file; // unreachable if isMultiFileSnapshot was checked first
}

fn processMultiFileSnapshot(allocator: Allocator, dir_path: []const u8, config: *const Config) SnapshotError!bool {
    var success: bool = true;
    log("Processing multi-file snapshot directory: {s}", .{dir_path});

    var dir = std.Io.Dir.cwd().openDir(app_io, dir_path, .{ .iterate = true }) catch |err| {
        std.log.err("Failed to open directory {s}: {}", .{ dir_path, err });
        return false;
    };
    defer dir.close(app_io);

    // First, collect EXPECTED sections from existing .md files
    var expected_sections = std.StringHashMap([]const u8).init(allocator);
    defer {
        var iter = expected_sections.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*);
        }
        expected_sections.deinit();
    }

    var iterator = dir.iterate();
    while (try iterator.next(app_io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
            const full_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
            defer allocator.free(full_path);

            if (std.Io.Dir.cwd().readFileAlloc(app_io, full_path, allocator, .limited(1024 * 1024))) |content| {
                defer allocator.free(content);

                // Extract EXPECTED section
                const expected_header = "# EXPECTED\n";
                if (std.mem.find(u8, content, expected_header)) |start_idx| {
                    const content_start = start_idx + expected_header.len;

                    // Find the next section header
                    var end_idx = content.len;
                    var search_idx = content_start;
                    while (search_idx < content.len - 2) {
                        if (content[search_idx] == '\n' and
                            content[search_idx + 1] == '#' and
                            content[search_idx + 2] == ' ')
                        {
                            end_idx = search_idx + 1;
                            break;
                        }
                        search_idx += 1;
                    }

                    const expected_section = std.mem.trim(u8, content[content_start..end_idx], " \t\r\n");
                    try expected_sections.put(try allocator.dupe(u8, entry.name), try allocator.dupe(u8, expected_section));
                }
            } else |_| {}
        }
    }

    // Delete existing .md files
    if (!config.disable_updates) {
        iterator = dir.iterate();
        var files_to_delete = std.array_list.Managed([]u8).init(allocator);
        defer {
            for (files_to_delete.items) |file_path| {
                allocator.free(file_path);
            }
            files_to_delete.deinit();
        }

        while (try iterator.next(app_io)) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
                const file_path = try allocator.dupe(u8, entry.name);
                try files_to_delete.append(file_path);
            }
        }

        for (files_to_delete.items) |file_name| {
            dir.deleteFile(app_io, file_name) catch |err| {
                warn("Failed to delete {s}: {}", .{ file_name, err });
            };
        }
    }

    // Find all .roc files and generate snapshots for each
    iterator = dir.iterate();
    const snapshot_type = getMultiFileSnapshotType(dir_path);

    while (try iterator.next(app_io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            const roc_file_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
            defer allocator.free(roc_file_path);

            // Generate snapshot file name (replace .roc with .md)
            const base_name = entry.name[0 .. entry.name.len - 4]; // remove .roc
            const snapshot_file_name = try std.fmt.allocPrint(allocator, "{s}.md", .{base_name});
            defer allocator.free(snapshot_file_name);
            const snapshot_file_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, snapshot_file_name });
            defer allocator.free(snapshot_file_path);

            // Read the .roc file content
            const roc_content = std.Io.Dir.cwd().readFileAlloc(app_io, roc_file_path, allocator, .limited(1024 * 1024)) catch |err| {
                warn("Failed to read {s}: {}", .{ roc_file_path, err });
                continue;
            };
            defer allocator.free(roc_content);

            // Create meta section
            const type_name = switch (snapshot_type) {
                .package => "package",
                .platform => "platform",
                .app => "app",
                else => "file",
            };
            const meta = Meta{
                .description = try std.fmt.allocPrint(allocator, "{s} module from {s}", .{ base_name, type_name }),
                .node_type = snapshot_type,
            };
            defer allocator.free(meta.description);

            // Get preserved EXPECTED section if it exists
            const expected_content = expected_sections.get(snapshot_file_name);

            // Process the .roc file as a snapshot
            success = try processRocFileAsSnapshotWithExpected(allocator, snapshot_file_path, roc_content, meta, expected_content, config) and success;
        }
    }

    return success;
}

fn processSnapshotContent(
    allocator: Allocator,
    content: Content,
    output_path: []const u8,
    config: *const Config,
) SnapshotError!bool {
    var success = true;
    log("Generating snapshot for: {s}", .{output_path});

    // Skip snapshots marked with skip=true in META
    if (content.meta.skip) {
        log("Skipping snapshot (skip=true): {s}", .{output_path});
        return true;
    }

    // Handle REPL snapshots separately
    if (content.meta.node_type == .repl) {
        return processReplSnapshot(allocator, content, output_path, config);
    }

    // Handle dev_object snapshots separately (multi-file, cross-compilation)
    if (content.meta.node_type == .dev_object) {
        return processDevObjectSnapshot(allocator, content, output_path, config);
    }

    // Handle docs snapshots separately (multi-file, doc extraction)
    if (content.meta.node_type == .docs) {
        return processDocsSnapshot(allocator, content, output_path, config);
    }

    // Process the content through the compilation pipeline
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();

    // Extract module name from custom filename if provided, otherwise from output path
    const module_name = if (content.meta.filename) |custom_filename|
        // Strip .roc extension if present
        if (std.mem.findScalarLast(u8, custom_filename, '.')) |dot_idx|
            custom_filename[0..dot_idx]
        else
            custom_filename
    else blk: {
        const basename = std.fs.path.basename(output_path);
        break :blk if (std.mem.findScalarLast(u8, basename, '.')) |dot_idx|
            basename[0..dot_idx]
        else
            basename;
    };

    // Map snapshot node type to parse mode
    const parse_mode: single_module.ParseMode = switch (content.meta.node_type) {
        .file, .mono, .package, .platform, .app, .snippet => .file,
        .expr => .expr,
        .statement => .statement,
        .header => .header,
        .repl, .dev_object, .docs => unreachable, // Handled above
    };

    // Create ModuleEnv (caller manages memory)
    var module_env = try single_module.ModuleEnv.init(allocator, content.source);
    defer module_env.deinit();
    var can_ir = &module_env;

    // Parse using the unified compile_module interface
    const parse_ast = try single_module.parseSingleModule(
        allocator,
        can_ir,
        parse_mode,
        .{ .module_name = module_name },
    );
    defer parse_ast.deinit();

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try can_ir.insertIdent(base.Ident.for_text(module_name)),
        .bool_stmt = config.builtin_indices.bool_type,
        .try_stmt = config.builtin_indices.try_type,
        .str_stmt = config.builtin_indices.str_type,
        .builtin_module = config.builtin_module,
        .builtin_indices = config.builtin_indices,
    };

    var maybe_expr_idx: ?Can.CanonicalizedExpr = null;

    switch (content.meta.node_type) {
        .file, .package, .platform, .app => {
            // All file types that use canonicalizeFile() will use the combined function below
        },
        .snippet, .mono => {
            // Snippet and mono tests are full modules
            const builtin_env = config.builtin_module orelse return error.MissingBuiltinModule;

            const roc_ctx = CoreCtx.default(allocator, allocator, app_io);
            var czer = try Can.initModule(roc_ctx, can_ir, parse_ast, .{
                .builtin_types = .{
                    .builtin_module_env = builtin_env,
                    .builtin_indices = config.builtin_indices,
                },
            });
            defer czer.deinit();
            try czer.canonicalizeFile();
        },
        .header => {
            // TODO: implement canonicalize_header when available
        },
        .expr, .statement => {
            // Expr and statement tests use different canonicalization methods
            const builtin_env = config.builtin_module orelse return error.MissingBuiltinModule;

            const roc_ctx = CoreCtx.default(allocator, allocator, app_io);
            var czer = try Can.initModule(roc_ctx, can_ir, parse_ast, .{
                .builtin_types = .{
                    .builtin_module_env = builtin_env,
                    .builtin_indices = config.builtin_indices,
                },
            });
            defer czer.deinit();

            switch (content.meta.node_type) {
                .expr => {
                    const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
                    maybe_expr_idx = try czer.canonicalizeExpr(expr_idx);
                },
                .statement => {
                    const ast_stmt_idx: AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
                    try czer.canonicalizeStatementForSnapshot(ast_stmt_idx);
                },
                else => unreachable,
            }
            if (content.meta.include_canonicalize_diagnostics) {
                try can_ir.publishScratchDiagnostics();
            }
        },
        .repl, .dev_object, .docs => unreachable, // Handled above
    }

    // Assert that everything is in-sync
    can_ir.debugAssertArraysInSync();

    // Compute dependency-based evaluation order if not already set
    // Skip for .file/.package/.platform/.app tests because canonicalizeAndTypeCheckModule
    // will call canonicalizeFile which sets it. Only do this for .expr/.statement which
    // don't call canonicalizeFile.
    const needs_evaluation_order = switch (content.meta.node_type) {
        .expr, .statement, .mono => true,
        .file, .package, .platform, .app, .snippet, .repl, .header, .dev_object, .docs => false,
    };

    if (needs_evaluation_order and can_ir.evaluation_order == null) {
        const DependencyGraph = @import("can").DependencyGraph;
        var graph = try DependencyGraph.buildDependencyGraph(
            can_ir,
            can_ir.all_defs,
            allocator,
        );
        defer graph.deinit();

        const eval_order = try DependencyGraph.computeSCCs(&graph, allocator);
        // IMPORTANT: Use can_ir.gpa here, not allocator, because ModuleEnv.deinit()
        // will free this with self.gpa. They must match to avoid memory leaks.
        const eval_order_ptr = try can_ir.gpa.create(DependencyGraph.EvaluationOrder);
        eval_order_ptr.* = eval_order;
        can_ir.evaluation_order = eval_order_ptr;
    }

    // Types - include Set, Dict, Bool, Try, and Str modules in the order they appear in imports
    // The order MUST match the import order in can_ir.imports because module_idx in external
    // type references is based on the import index
    var builtin_modules = std.array_list.Managed(*const ModuleEnv).init(allocator);
    defer builtin_modules.deinit();

    // Build builtin_modules array in the same order as can_ir.imports
    // Dict and Set are now nested inside Builtin, so we only have one module to add
    const import_count = can_ir.imports.imports.items.items.len;
    for (can_ir.imports.imports.items.items[0..import_count]) |str_idx| {
        const import_name = can_ir.getString(str_idx);

        // Match the import name to the corresponding loaded builtin module
        if (CIR.Import.isCompilerBuiltinImportName(import_name)) {
            if (config.builtin_module) |builtin_env| {
                try builtin_modules.append(builtin_env);
            }
        }
    }

    // Use the shared type checking function to ensure identical behavior with roc check
    // We need to keep module_envs alive until after we're done with the checker (for type printing)
    var module_envs_for_repl_expr: ?std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType) = null;
    defer if (module_envs_for_repl_expr) |*envs| envs.deinit();

    var module_envs_for_snippet: ?std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType) = null;
    defer if (module_envs_for_snippet) |*envs| envs.deinit();

    var module_envs_for_file: ?std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType) = null;
    defer if (module_envs_for_file) |*envs| envs.deinit();

    var solver = if (maybe_expr_idx) |expr_idx| blk: {
        // For REPL/expr tests, create module_envs for type checking
        var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);

        if (config.builtin_module) |builtin_env| {
            try Can.populateModuleEnvs(&module_envs, can_ir, builtin_env, config.builtin_indices);
        }
        can_ir.imports.clearResolvedModules();
        try can_ir.imports.resolveImportsByExactModuleName(can_ir, builtin_modules.items);
        can_ir.imports.markUnresolvedImportsFailedBeforeChecking();

        var checker = try Check.init(
            allocator,
            &can_ir.types,
            can_ir,
            builtin_modules.items,
            &module_envs,
            &can_ir.store.regions,
            builtin_ctx,
        );
        checker.fixupTypeWriter();
        try checker.checkExprRepl(expr_idx.idx);
        module_envs_for_repl_expr = module_envs; // Keep alive
        break :blk checker;
    } else switch (content.meta.node_type) {
        .file, .package, .platform, .app => blk: {
            // For file types, use the combined canonicalize+typecheck function.
            // This ensures the SAME module_envs map is used for both phases (just like REPL tests)
            // For file tests, canonicalization happens INSIDE canonicalizeAndTypeCheckModule,
            // so can_ir.imports is still empty at this point. We can't use builtin_modules
            // (which is built from can_ir.imports). Instead, just pass builtin_env directly.
            const builtin_env = config.builtin_module orelse unreachable;
            // Cast from *const ModuleEnv to *ModuleEnv (function signature requires non-const pointer)
            const builtin_env_nonconst: *ModuleEnv = @constCast(builtin_env);
            const imported_envs_for_file: []const *ModuleEnv = &[_]*ModuleEnv{builtin_env_nonconst};

            // Initialize module_envs_for_file and pass it to the function
            // This way it stays alive until the defer at line 1249
            module_envs_for_file = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);

            const roc_ctx_for_check = CoreCtx.default(allocator, allocator, app_io);
            const checker = try compile.PackageEnv.canonicalizeAndTypeCheckModule(
                roc_ctx_for_check,
                allocator,
                can_ir,
                parse_ast,
                builtin_env,
                config.builtin_indices,
                imported_envs_for_file,
                &module_envs_for_file.?,
                std.fs.path.dirname(output_path),
                if (content.meta.include_module_validation_diagnostics) .checking else .none,
            );
            break :blk checker;
        },
        .snippet, .statement, .header, .expr, .mono => blk: {
            // For snippet/statement/header/expr/mono tests, type check the already-canonicalized IR
            // Note: .expr and .mono can reach here if canonicalizeExpr returned null (error during canonicalization)
            var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);

            if (config.builtin_module) |builtin_env| {
                try Can.populateModuleEnvs(&module_envs, can_ir, builtin_env, config.builtin_indices);
            }
            can_ir.imports.clearResolvedModules();
            try can_ir.imports.resolveImportsByExactModuleName(can_ir, builtin_modules.items);
            can_ir.imports.markUnresolvedImportsFailedBeforeChecking();

            var checker = try Check.init(
                allocator,
                &can_ir.types,
                can_ir,
                builtin_modules.items,
                &module_envs,
                &can_ir.store.regions,
                builtin_ctx,
            );
            checker.fixupTypeWriter();
            try checker.checkFile();
            module_envs_for_snippet = module_envs; // Keep alive
            break :blk checker;
        },
        .repl, .dev_object, .docs => unreachable, // Should never reach here - handled earlier
    };
    defer solver.deinit();

    // Assert that we have regions for every type variable
    solver.debugAssertArraysInSync();

    // Cache round-trip validation - ensure ModuleCache serialization/deserialization works
    {
        // Generate original S-expression for comparison
        var original_tree = SExprTree.init(allocator);
        defer original_tree.deinit();
        try ModuleEnv.pushToSExprTree(can_ir, null, &original_tree);

        var original_sexpr_aw: std.Io.Writer.Allocating = .init(allocator);
        defer original_sexpr_aw.deinit();
        try original_tree.toStringPretty(&original_sexpr_aw.writer, .skip_linecol);
        const original_sexpr_items = original_sexpr_aw.writer.buffer[0..original_sexpr_aw.writer.end];

        // Create arena for serialization
        var cache_arena = base.SingleThreadArena.init(allocator);
        defer cache_arena.deinit();

        // Create and serialize MmapCache
        const cache_data = try CacheModule.create(allocator, cache_arena.allocator(), can_ir, can_ir, 0, 0);
        defer allocator.free(cache_data);

        // Deserialize back
        var loaded_cache = try CacheModule.fromMappedMemory(cache_data);

        // Create arena for restore operation to handle temporary allocations
        var restore_arena = base.SingleThreadArena.init(allocator);
        defer restore_arena.deinit();

        // Restore ModuleEnv
        const restored_env = try loaded_cache.restore(restore_arena.allocator(), module_name, content.source);
        // Note: restored_env points to data within the cache, so we don't free it

        // Generate S-expression from restored ModuleEnv
        var restored_tree = SExprTree.init(allocator);
        defer restored_tree.deinit();
        try ModuleEnv.pushToSExprTree(restored_env, null, &restored_tree);

        var restored_sexpr_aw: std.Io.Writer.Allocating = .init(allocator);
        defer restored_sexpr_aw.deinit();
        try restored_tree.toStringPretty(&restored_sexpr_aw.writer, .skip_linecol);
        const restored_sexpr_items = restored_sexpr_aw.writer.buffer[0..restored_sexpr_aw.writer.end];

        // Compare S-expressions - crash if they don't match
        if (!std.mem.eql(u8, original_sexpr_items, restored_sexpr_items)) {
            std.log.err("Cache round-trip validation failed for snapshot: {s}", .{output_path});
            std.log.err("Original and restored CIR S-expressions don't match!", .{});
            std.log.err("This indicates a bug in MmapCache serialization/deserialization.", .{});
            std.log.err("Original S-expression:\n{s}", .{original_sexpr_items});
            std.log.err("Restored S-expression:\n{s}", .{restored_sexpr_items});
            return error.CacheRoundTripValidationFailed;
        }
    }

    // Lambda lifting and lambda set inference are now handled during CIR→MIR and MIR→LIR lowering

    // TODO: Run constant folding for mono tests once ComptimeEvaluator is available in zig-16 branch.
    // if (content.meta.node_type == .mono) { ... }

    // Buffer all output in memory before writing files
    var md_buffer_unmanaged = std.ArrayList(u8).empty;
    var md_writer_allocating: std.Io.Writer.Allocating = .fromArrayList(allocator, &md_buffer_unmanaged);
    defer md_buffer_unmanaged.deinit(allocator);

    var html_buffer_unmanaged: ?std.ArrayList(u8) = if (config.generate_html) std.ArrayList(u8).empty else null;
    var html_writer_allocating: ?std.Io.Writer.Allocating = if (config.generate_html) .fromArrayList(allocator, &html_buffer_unmanaged.?) else null;
    defer {
        if (html_buffer_unmanaged) |*buf| buf.deinit(allocator);
    }

    var output = DualOutput.init(allocator, &md_writer_allocating, if (html_writer_allocating) |*hw| hw else null);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate reports once and use for both EXPECTED and PROBLEMS sections
    var generated_reports = try generateAllReports(allocator, parse_ast, can_ir, &solver, output_path, can_ir);
    defer {
        for (generated_reports.items) |*report| {
            report.deinit();
        }
        generated_reports.deinit();
    }

    // Generate all sections
    // For mono tests, the order is: META, SOURCE, MONO, FORMATTED, then the rest
    // For other tests, the order is: META, SOURCE, EXPECTED, PROBLEMS, TOKENS, PARSE, FORMATTED, CANONICALIZE, TYPES
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);

    if (content.meta.node_type == .mono) {
        // Mono tests: MONO and FORMATTED come right after SOURCE
        try generateMonoSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx), output_path, config);
        try generateFormattedSection(&output, &content, parse_ast);
        success = try generateExpectedSection(&output, output_path, &content, &generated_reports, config) and success;
        try generateProblemsSection(&output, &generated_reports);
        try generateTokensSection(&output, parse_ast, &content, can_ir, config.linecol_mode);
        try generateParseSection(&output, &content, parse_ast, &can_ir.common, config.linecol_mode);
        try generateCanonicalizeSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx), config.linecol_mode);
        try generateTypesSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx), config.linecol_mode);
    } else {
        // Other tests: standard order
        success = try generateExpectedSection(&output, output_path, &content, &generated_reports, config) and success;
        try generateProblemsSection(&output, &generated_reports);
        try generateTokensSection(&output, parse_ast, &content, can_ir, config.linecol_mode);
        try generateParseSection(&output, &content, parse_ast, &can_ir.common, config.linecol_mode);
        try generateFormattedSection(&output, &content, parse_ast);
        try generateCanonicalizeSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx), config.linecol_mode);
        try generateTypesSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx), config.linecol_mode);
    }

    try generateHtmlClosing(&output);

    // Transfer contents from writer back to buffer before writing
    md_buffer_unmanaged = md_writer_allocating.toArrayList();
    if (html_writer_allocating) |*hw| html_buffer_unmanaged.? = hw.toArrayList();

    if (!config.disable_updates) {
        // Write the markdown file
        const md_file = std.Io.Dir.cwd().createFile(app_io, output_path, .{}) catch |err| {
            std.log.err("Failed to create {s}: {}", .{ output_path, err });
            return false;
        };
        defer md_file.close(app_io);

        try md_file.writeStreamingAll(app_io, md_buffer_unmanaged.items);

        if (html_buffer_unmanaged) |*buf| {
            writeHtmlFile(allocator, output_path, buf) catch |err| {
                warn("Failed to write HTML file for {s}: {}", .{ output_path, err });
            };
        }
    }
    return success;
}

fn processRocFileAsSnapshotWithExpected(
    allocator: Allocator,
    output_path: []const u8,
    roc_content: []const u8,
    meta: Meta,
    expected_content: ?[]const u8,
    config: *const Config,
) SnapshotError!bool {
    // Create content structure
    const content = Content{
        .meta = meta,
        .source = roc_content,
        .expected = expected_content,
        .output = null,
        .formatted = null,
        .dev_output = null,
        .docs_output = null,
        .has_canonicalize = true,
    };

    return try processSnapshotContent(allocator, content, output_path, config);
}

const Config = struct {
    maybe_fuzz_corpus_path: ?[]const u8,
    generate_html: bool,
    expected_section_command: UpdateCommand,
    output_section_command: UpdateCommand,
    disable_updates: bool = false, // Disable updates for check mode
    trace_eval: bool = false,
    linecol_mode: LineColMode = .skip_linecol, // Include line/column info in output
    // Compiled Builtin module (contains nested Bool, Try, Str, Dict, Set)
    builtin_module: ?*const ModuleEnv = null,
    builtin_indices: CIR.BuiltinIndices,
    // Borrowed pre-published Builtin artifact; lets REPL snapshots skip
    // re-publishing the stdlib on every line.
    builtin_modules_ref: ?*eval_mod.BuiltinModules = null,
    cwd: []const u8,
};

const ProcessResult = struct {
    success: usize,
    failed: usize,
};

const WorkItem = struct {
    path: []const u8,
    kind: enum {
        snapshot_file,
        multi_file_snapshot,
    },
};

const WorkList = std.array_list.Managed(WorkItem);

const ProcessContext = struct {
    work_list: *WorkList,
    config: *const Config,
    success_count: parallel.AtomicUsize,
    failed_count: parallel.AtomicUsize,
};

/// Worker function that processes a single work item
fn processWorkItem(allocator: Allocator, context: *ProcessContext, item_id: usize) void {
    const work_item = context.work_list.items[item_id];
    const success = switch (work_item.kind) {
        .snapshot_file => processSnapshotFile(allocator, work_item.path, context.config) catch |err| blk: {
            std.debug.print("Snapshot processing error in {s}: {s}\n", .{ work_item.path, @errorName(err) });
            break :blk false;
        },
        .multi_file_snapshot => blk: {
            const res = processMultiFileSnapshot(allocator, work_item.path, context.config) catch |err| {
                std.debug.print("Snapshot processing error in {s}: {s}\n", .{ work_item.path, @errorName(err) });
                break :blk false;
            };
            break :blk res;
        },
    };

    if (success) {
        _ = context.success_count.fetchAdd(1, .monotonic);
    } else {
        std.debug.print("Snapshot failed: {s}\n", .{work_item.path});
        _ = context.failed_count.fetchAdd(1, .monotonic);
    }
}

/// Stage 2: Process work items in parallel using the parallel utility
fn processWorkItems(gpa: Allocator, work_list: WorkList, max_threads: usize, debug: bool, config: *const Config) std.Thread.SpawnError!ProcessResult {
    if (work_list.items.len == 0) {
        return ProcessResult{ .success = 0, .failed = 0 };
    }

    var context = ProcessContext{
        .work_list = @constCast(&work_list),
        .config = config,
        .success_count = parallel.AtomicUsize.init(0),
        .failed_count = parallel.AtomicUsize.init(0),
    };

    // Use per-thread arena allocators for snapshot processing
    const options = parallel.ProcessOptions{
        .max_threads = max_threads,
        .use_per_thread_arenas = !debug,
    };

    try parallel.process(
        ProcessContext,
        &context,
        processWorkItem,
        gpa,
        work_list.items.len,
        options,
    );

    return ProcessResult{
        .success = context.success_count.load(.monotonic),
        .failed = context.failed_count.load(.monotonic),
    };
}

/// Stage 1: Walk directory tree and collect work items
fn collectWorkItems(gpa: Allocator, path: []const u8, work_list: *WorkList) SnapshotError!void {
    const canonical_path = absolutePathAlloc(app_io, path, gpa) catch |err| {
        std.log.err("failed to resolve path '{s}': {s}", .{ path, @errorName(err) });
        return;
    };
    defer gpa.free(canonical_path);

    // Try to open as directory first
    if (std.Io.Dir.cwd().openDir(app_io, canonical_path, .{ .iterate = true })) |dir_handle| {
        var dir = dir_handle;
        defer dir.close(app_io);

        // It's a directory
        if (isMultiFileSnapshot(canonical_path)) {
            const path_copy = try gpa.dupe(u8, canonical_path);
            try work_list.append(WorkItem{
                .path = path_copy,
                .kind = .multi_file_snapshot,
            });
        } else {
            var dir_iterator = dir.iterate();
            while (try dir_iterator.next(app_io)) |entry| {
                // Skip hidden files and special directories
                if (entry.name[0] == '.') continue;

                const full_path = try std.fs.path.join(gpa, &[_][]const u8{ canonical_path, entry.name });
                defer gpa.free(full_path);

                if (entry.kind == .directory) {
                    try collectWorkItems(gpa, full_path, work_list);
                } else if (entry.kind == .file and isSnapshotFile(entry.name)) {
                    const path_copy = try gpa.dupe(u8, full_path);
                    try work_list.append(WorkItem{
                        .path = path_copy,
                        .kind = .snapshot_file,
                    });
                }
            }
        }
    } else |dir_err| switch (dir_err) {
        // Not a directory, try as file
        error.NotDir => {
            if (isSnapshotFile(canonical_path)) {
                const path_copy = try gpa.dupe(u8, canonical_path);
                try work_list.append(WorkItem{
                    .path = path_copy,
                    .kind = .snapshot_file,
                });
            } else {
                std.log.err("file '{s}' is not a snapshot file (must end with .md)", .{canonical_path});
            }
        },
        else => std.log.err("failed to access path '{s}': {s}", .{ canonical_path, @errorName(dir_err) }),
    }
}

/// Represents the different sections of a snapshot file.
const Section = union(enum) {
    meta,
    source,
    expected,
    output,
    formatted,
    parse,
    canonicalize,
    tokens,
    problems,
    types,
    mono,
    dev_output,
    docs,

    pub const META = "# META\n~~~ini\n";
    pub const SOURCE = "# SOURCE\n~~~roc\n";
    pub const SOURCE_MULTI = "# SOURCE\n";
    pub const EXPECTED = "# EXPECTED\n";
    pub const OUTPUT = "# OUTPUT\n";
    pub const FORMATTED = "# FORMATTED\n~~~roc\n";
    pub const PARSE = "# PARSE\n~~~clojure\n";
    pub const CANONICALIZE = "# CANONICALIZE\n~~~clojure\n";
    pub const TOKENS = "# TOKENS\n~~~zig\n";
    pub const PROBLEMS = "# PROBLEMS\n";
    pub const TYPES = "# TYPES\n~~~clojure\n";
    pub const MONO = "# MONO\n~~~roc\n";
    pub const DEV_OUTPUT = "# DEV OUTPUT\n~~~ini\n";
    pub const DOCS = "# DOCS\n~~~clojure\n";

    pub const SECTION_END = "~~~\n";

    fn fromString(str: []const u8) ?Section {
        if (std.mem.startsWith(u8, str, META)) return .meta;
        // Check for single-file SOURCE first (has ~~~roc), then multi-file SOURCE
        if (std.mem.startsWith(u8, str, SOURCE)) return .source;
        if (std.mem.startsWith(u8, str, SOURCE_MULTI)) return .source;
        if (std.mem.startsWith(u8, str, EXPECTED)) return .expected;
        if (std.mem.startsWith(u8, str, OUTPUT)) return .output;
        if (std.mem.startsWith(u8, str, FORMATTED)) return .formatted;
        if (std.mem.startsWith(u8, str, PARSE)) return .parse;
        if (std.mem.startsWith(u8, str, CANONICALIZE)) return .canonicalize;
        if (std.mem.startsWith(u8, str, TYPES)) return .types;
        if (std.mem.startsWith(u8, str, TOKENS)) return .tokens;
        if (std.mem.startsWith(u8, str, PROBLEMS)) return .problems;
        if (std.mem.startsWith(u8, str, DEV_OUTPUT)) return .dev_output;
        if (std.mem.startsWith(u8, str, DOCS)) return .docs;
        if (std.mem.startsWith(u8, str, MONO)) return .mono;
        return null;
    }

    /// Check if the SOURCE section at this position uses multi-file format (## sub-headings)
    fn isMultiFileSource(str: []const u8) bool {
        return std.mem.startsWith(u8, str, SOURCE_MULTI) and !std.mem.startsWith(u8, str, SOURCE);
    }

    fn asString(self: Section) []const u8 {
        return switch (self) {
            .meta => META,
            .source => SOURCE,
            .expected => EXPECTED,
            .output => OUTPUT,
            .formatted => FORMATTED,
            .parse => PARSE,
            .canonicalize => CANONICALIZE,
            .tokens => TOKENS,
            .problems => PROBLEMS,
            .types => TYPES,
            .mono => MONO,
            .dev_output => DEV_OUTPUT,
            .docs => DOCS,
        };
    }

    /// Captures the start and end positions of a section within the file content
    const Range = struct {
        start: usize,
        end: usize,

        fn empty() Range {
            return .{
                .start = 0,
                .end = 0,
            };
        }

        fn extract(self: Range, content: []const u8) []const u8 {
            if (self.end < self.start) @panic("invalid range");
            return std.mem.trimEnd(u8, content[self.start..self.end], "\n");
        }
    };
};

/// The type of node to parse
pub const NodeType = enum {
    file,
    header,
    expr,
    statement,
    package,
    platform,
    app,
    repl,
    snippet,
    mono,
    dev_object,
    docs,

    pub const HEADER = "header";
    pub const EXPR = "expr";
    pub const STMT = "statement";
    pub const FILE = "file";
    pub const PACKAGE = "package";
    pub const PLATFORM = "platform";
    pub const APP = "app";
    pub const REPL = "repl";
    pub const SNIPPET = "snippet";
    pub const MONO = "mono";
    pub const DEV_OBJECT = "dev_object";
    pub const DOCS_TYPE = "docs";

    fn fromString(str: []const u8) Error!NodeType {
        if (std.mem.eql(u8, str, HEADER)) return .header;
        if (std.mem.eql(u8, str, EXPR)) return .expr;
        if (std.mem.eql(u8, str, STMT)) return .statement;
        if (std.mem.eql(u8, str, FILE)) return .file;
        if (std.mem.eql(u8, str, PACKAGE)) return .package;
        if (std.mem.eql(u8, str, PLATFORM)) return .platform;
        if (std.mem.eql(u8, str, APP)) return .app;
        if (std.mem.eql(u8, str, REPL)) return .repl;
        if (std.mem.eql(u8, str, SNIPPET)) return .snippet;
        if (std.mem.eql(u8, str, MONO)) return .mono;
        if (std.mem.eql(u8, str, DEV_OBJECT)) return .dev_object;
        if (std.mem.eql(u8, str, DOCS_TYPE)) return .docs;
        return Error.InvalidNodeType;
    }

    fn toString(self: NodeType) []const u8 {
        return switch (self) {
            .file => "file",
            .header => "header",
            .expr => "expr",
            .statement => "statement",
            .package => "package",
            .platform => "platform",
            .app => "app",
            .repl => "repl",
            .snippet => "snippet",
            .mono => "mono",
            .dev_object => "dev_object",
            .docs => "docs",
        };
    }
};

const Meta = struct {
    description: []const u8,
    node_type: NodeType,
    filename: ?[]const u8 = null,
    skip: bool = false,
    include_canonicalize_diagnostics: bool = false,
    include_module_validation_diagnostics: bool = false,

    const DESC_START: []const u8 = "description=";
    const TYPE_START: []const u8 = "type=";
    const SKIP_START: []const u8 = "skip=";
    const CANONICALIZE_DIAGNOSTICS_START: []const u8 = "canonicalize_diagnostics=";
    const MODULE_VALIDATION_DIAGNOSTICS_START: []const u8 = "module_validation_diagnostics=";

    fn fromString(text: []const u8) Error!Meta {
        var lines = std.mem.splitScalar(u8, text, '\n');
        var desc: []const u8 = "";
        var node_type: NodeType = .file;
        var filename: ?[]const u8 = null;
        var skip: bool = false;
        var include_canonicalize_diagnostics: bool = false;
        var include_module_validation_diagnostics: bool = false;
        while (true) {
            var line = lines.next() orelse break;
            if (std.mem.startsWith(u8, line, DESC_START)) {
                desc = line[(DESC_START.len)..];
            } else if (std.mem.startsWith(u8, line, TYPE_START)) {
                const ty = line[(TYPE_START.len)..];
                // Check if there's a colon indicating a custom filename
                if (std.mem.findScalar(u8, ty, ':')) |colon_idx| {
                    node_type = try NodeType.fromString(ty[0..colon_idx]);
                    filename = ty[colon_idx + 1 ..];
                } else {
                    node_type = try NodeType.fromString(ty);
                }
            } else if (std.mem.startsWith(u8, line, SKIP_START)) {
                skip = std.mem.eql(u8, line[(SKIP_START.len)..], "true");
            } else if (std.mem.startsWith(u8, line, CANONICALIZE_DIAGNOSTICS_START)) {
                include_canonicalize_diagnostics = std.mem.eql(u8, line[(CANONICALIZE_DIAGNOSTICS_START.len)..], "true");
            } else if (std.mem.startsWith(u8, line, MODULE_VALIDATION_DIAGNOSTICS_START)) {
                include_module_validation_diagnostics = std.mem.eql(u8, line[(MODULE_VALIDATION_DIAGNOSTICS_START.len)..], "true");
            }
        }

        return .{
            .description = desc,
            .node_type = node_type,
            .filename = filename,
            .skip = skip,
            .include_canonicalize_diagnostics = include_canonicalize_diagnostics,
            .include_module_validation_diagnostics = include_module_validation_diagnostics,
        };
    }

    fn format(self: Meta, writer: anytype) error{WriteFailed}!void {
        try writer.writeAll(DESC_START);
        try writer.writeAll(self.description);
        try writer.writeAll("\n");
        try writer.writeAll(TYPE_START);
        try writer.writeAll(self.node_type.toString());
        if (self.filename) |fname| {
            try writer.writeAll(":");
            try writer.writeAll(fname);
        }
        if (self.skip) {
            try writer.writeAll("\n");
            try writer.writeAll(SKIP_START);
            try writer.writeAll("true");
        }
        if (self.include_canonicalize_diagnostics) {
            try writer.writeAll("\n");
            try writer.writeAll(CANONICALIZE_DIAGNOSTICS_START);
            try writer.writeAll("true");
        }
        if (self.include_module_validation_diagnostics) {
            try writer.writeAll("\n");
            try writer.writeAll(MODULE_VALIDATION_DIAGNOSTICS_START);
            try writer.writeAll("true");
        }
    }

    test "Meta.fromString - only description" {
        const meta = try Meta.fromString(
            \\description=Hello world
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
    }
    test "Meta.fromString - desc and file type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=file
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .file);
    }
    test "Meta.fromString - desc and expr type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=expr
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .expr);
    }
    test "Meta.fromString - desc and statement type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=statement
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .statement);
    }
    test "Meta.fromString - desc and header type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=header
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .header);
    }
    test "Meta.fromString - desc and invalid type" {
        const meta = Meta.fromString(
            \\description=Hello world
            \\type=foobar
        );
        try std.testing.expectError(Error.InvalidNodeType, meta);
    }
};

/// Content of a snapshot file, references the Metadata and Source sections etc
pub const Content = struct {
    meta: Meta,
    source: []const u8,
    expected: ?[]const u8,
    output: ?[]const u8,
    formatted: ?[]const u8,
    dev_output: ?[]const u8,
    docs_output: ?[]const u8,
    has_canonicalize: bool,

    fn from_ranges(ranges: std.AutoHashMap(Section, Section.Range), content: []const u8) Error!Content {
        var source: []const u8 = undefined;
        var expected: ?[]const u8 = undefined;
        var output: ?[]const u8 = undefined;
        var formatted: ?[]const u8 = undefined;
        var dev_output: ?[]const u8 = undefined;
        var docs_output: ?[]const u8 = undefined;
        var has_canonicalize: bool = false;

        if (ranges.get(.source)) |value| {
            // trailing newlines are part of the source
            source = content[value.start..value.end];
        } else {
            return Error.MissingSnapshotSource;
        }

        if (ranges.get(.expected)) |value| {
            expected = value.extract(content);
        } else {
            expected = null;
        }

        if (ranges.get(.output)) |value| {
            output = value.extract(content);
        } else {
            output = null;
        }

        if (ranges.get(.formatted)) |value| {
            formatted = value.extract(content);
        } else {
            formatted = null;
        }

        if (ranges.get(.dev_output)) |value| {
            dev_output = value.extract(content);
        } else {
            dev_output = null;
        }

        if (ranges.get(.docs)) |value| {
            docs_output = value.extract(content);
        } else {
            docs_output = null;
        }

        if (ranges.get(.canonicalize)) |_| {
            has_canonicalize = true;
        }

        if (ranges.get(.meta)) |value| {
            const meta_txt = value.extract(content);
            const meta = try Meta.fromString(meta_txt);
            return Content{
                .meta = meta,
                .source = source,
                .expected = expected,
                .output = output,
                .formatted = formatted,
                .dev_output = dev_output,
                .docs_output = docs_output,
                .has_canonicalize = has_canonicalize,
            };
        } else {
            return Error.MissingSnapshotHeader;
        }
    }
};

const Error = error{ MissingSnapshotHeader, MissingSnapshotSource, InvalidNodeType, BadSectionHeader };

/// Dual output writers for markdown and HTML generation
pub const DualOutput = struct {
    md_writer: *std.Io.Writer.Allocating,
    html_writer: ?*std.Io.Writer.Allocating,
    gpa: Allocator,

    pub fn init(gpa: Allocator, md_writer: *std.Io.Writer.Allocating, html_writer: ?*std.Io.Writer.Allocating) DualOutput {
        return .{
            .md_writer = md_writer,
            .html_writer = html_writer,
            .gpa = gpa,
        };
    }

    fn begin_section(self: *DualOutput, name: []const u8) error{WriteFailed}!void {
        try self.md_writer.writer.print("# {s}\n", .{name});
        if (self.html_writer) |writer| {
            try writer.writer.print(
                \\        <div class="section" data-section="{s}">
                \\            <div class="section-content">
            , .{name});
        }
    }

    fn end_section(self: *DualOutput) error{WriteFailed}!void {
        if (self.html_writer) |writer| {
            try writer.writer.writeAll(
                \\            </div>
                \\        </div>
            );
        }
    }

    fn begin_code_block(self: *DualOutput, language: []const u8) error{WriteFailed}!void {
        try self.md_writer.writer.print("~~~{s}\n", .{language});
    }

    fn end_code_block(self: *DualOutput) error{WriteFailed}!void {
        try self.md_writer.writer.writeAll("~~~\n");
    }
};

/// Helper function to escape HTML characters
fn escapeHtmlChar(writer: anytype, char: u8) error{WriteFailed}!void {
    switch (char) {
        '<' => try writer.writeAll("&lt;"),
        '>' => try writer.writeAll("&gt;"),
        '&' => try writer.writeAll("&amp;"),
        '"' => try writer.writeAll("&quot;"),
        '\'' => try writer.writeAll("&#x27;"),
        else => try writer.writeByte(char),
    }
}

/// Generate META section for both markdown and HTML
fn generateMetaSection(output: *DualOutput, content: *const Content) error{WriteFailed}!void {
    try output.begin_section("META");
    try output.begin_code_block("ini");
    try content.meta.format(&output.md_writer.writer);
    try output.md_writer.writer.writeAll("\n");

    // HTML META section
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <div class="meta-info">
            \\                    <p><strong>Description:</strong>
        );
        try writer.writer.writeAll(content.meta.description);
        try writer.writer.writeAll("</p>\n                    <p><strong>Type:</strong> ");
        try writer.writer.writeAll(content.meta.node_type.toString());
        try writer.writer.writeAll(
            \\</p>
            \\                </div>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate SOURCE section for both markdown and HTML
fn generateSourceSection(output: *DualOutput, content: *const Content) error{WriteFailed}!void {
    try output.begin_section("SOURCE");
    try output.begin_code_block("roc");
    try output.md_writer.writer.writeAll(content.source);
    if (content.source.len == 0 or content.source[content.source.len - 1] != '\n') {
        try output.md_writer.writer.writeAll("\n");
    }

    // HTML SOURCE section - encode source as JavaScript string
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <div class="source-code" id="source-display">
            \\                </div>
            \\                <script>
            \\                window.rocSourceCode =
        );

        // Escape the source code for JavaScript string literal
        try writer.writer.writeAll("`");
        for (content.source) |char| {
            switch (char) {
                '`' => try writer.writer.writeAll("\\`"),
                '\\' => try writer.writer.writeAll("\\\\"),
                '$' => try writer.writer.writeAll("\\$"),
                '\n' => try writer.writer.writeAll("\\n"),
                '\r' => try writer.writer.writeAll("\\r"),
                '\t' => try writer.writer.writeAll("\\t"),
                else => try writer.writer.writeByte(char),
            }
        }
        try writer.writer.writeAll(
            \\`;
            \\      </script>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate EXPECTED section for both markdown and HTML
fn generateExpectedSection(
    output: *DualOutput,
    snapshot_path: []const u8,
    content: *const Content,
    reports: *const std.array_list.Managed(reporting.Report),
    config: *const Config,
) (Allocator.Error || error{WriteFailed})!bool {
    try output.begin_section("EXPECTED");
    var success = true;

    var expected_content: ?[]const u8 = null;
    defer if (expected_content) |e| output.gpa.free(e);

    const new_content = try renderReportsToExpectedContent(output.gpa, reports);
    defer output.gpa.free(new_content);
    switch (config.expected_section_command) {
        .update => {
            // Generate EXPECTED content using shared report generation
            expected_content = new_content;
        },
        .check => {
            // Use existing expected content or NIL
            if (content.expected) |expected| {
                expected_content = try output.gpa.dupe(u8, expected);
            } else {
                expected_content = try output.gpa.dupe(u8, "NIL");
            }

            if (!std.mem.eql(u8, new_content, expected_content.?)) {
                // If the new content differs, we need to update the expected section
                std.debug.print("Mismatch in EXPECTED section for {s}\n", .{snapshot_path});
                std.debug.print("Expected:\n{s}\n", .{expected_content.?});
                std.debug.print("Generated:\n{s}\n", .{new_content});
                std.debug.print("Hint: use `zig build run-snapshot-tool -- --update-expected` to automatically update the expectations.\n", .{});

                success = false;
            }
        },
        .none => {
            // Use existing expected content or NIL
            if (content.expected) |expected| {
                expected_content = try output.gpa.dupe(u8, expected);
            } else {
                expected_content = try output.gpa.dupe(u8, "NIL");
            }

            if (!std.mem.eql(u8, new_content, expected_content.?)) {
                // If the new content differs,
                std.debug.print("Warning: Mismatch in EXPECTED section for {s}\n", .{snapshot_path});
                std.debug.print("Hint: use `-- --check-expected` to give a more detailed report.\n", .{});
            }
        },
    }

    // Write the expected content (either generated or existing)
    if (expected_content) |expected| {
        try output.md_writer.writer.writeAll(expected);
        try output.md_writer.writer.writeByte('\n');

        // HTML EXPECTED section
        if (output.html_writer) |writer| {
            try writer.writer.writeAll(
                \\                <div class="expected">
            );

            // For HTML, escape the expected content
            for (expected) |char| {
                switch (char) {
                    '<' => try writer.writer.writeAll("&lt;"),
                    '>' => try writer.writer.writeAll("&gt;"),
                    '&' => try writer.writer.writeAll("&amp;"),
                    '"' => try writer.writer.writeAll("&quot;"),
                    '\'' => try writer.writer.writeAll("&#39;"),
                    else => try writer.writer.writeByte(char),
                }
            }

            try writer.writer.writeAll(
                \\
                \\                </div>
                \\
            );
        }
    }

    try output.end_section();

    return success;
}

/// Generate PROBLEMS section for both markdown and HTML using shared report generation
fn generateProblemsSection(output: *DualOutput, reports: *const std.array_list.Managed(reporting.Report)) error{WriteFailed}!void {
    try output.begin_section("PROBLEMS");
    try renderReportsToProblemsSection(output, reports);
    try output.end_section();
}

/// Generate TOKENS section for both markdown and HTML
pub fn generateTokensSection(output: *DualOutput, parse_ast: *AST, _: *const Content, module_env: *ModuleEnv, linecol_mode: LineColMode) (Allocator.Error || error{WriteFailed})!void {
    try output.begin_section("TOKENS");
    try output.begin_code_block("zig");

    // HTML TOKENS section - encode tokens as JavaScript array
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <div class="token-list" id="tokens-display">
            \\                </div>
            \\                <script>
            \\                window.rocTokens = [
        );
    }

    var tokenizedBuffer = parse_ast.tokens;
    const tokens = tokenizedBuffer.tokens.items(.tag);
    for (tokens, 0..) |tok, i| {
        const region = tokenizedBuffer.resolve(@intCast(i));
        const info = module_env.calcRegionInfo(region);

        // Markdown token output
        if (linecol_mode == .include_linecol) {
            try output.md_writer.writer.print("{s}({d}:{d}-{d}:{d}),", .{
                @tagName(tok),
                // add one to display numbers instead of index
                info.start_line_idx + 1,
                info.start_col_idx + 1,
                info.end_line_idx + 1,
                info.end_col_idx + 1,
            });
        } else {
            try output.md_writer.writer.print("{s},", .{@tagName(tok)});
        }

        if (i + 1 < tokenizedBuffer.tokens.len) {
            const next_region = tokenizedBuffer.resolve(@intCast(i + 1));
            if (source_contains_newline_in_range(parse_ast.env.source, @min(region.end.offset, next_region.start.offset), @max(region.end.offset, next_region.start.offset))) {
                try output.md_writer.writer.writeAll("\n");
            }
        }

        // HTML token output as JavaScript array element: [token_kind_str, start_byte, end_byte]
        if (output.html_writer) |writer| {
            try writer.writer.print("                    [\"{s}\", {d}, {d}]", .{
                @tagName(tok),
                region.start.offset,
                region.end.offset,
            });

            // Add comma except for last token
            if (i < tokens.len - 1) {
                try writer.writer.writeAll(",");
            }
        }

        if (output.html_writer) |writer| {
            try writer.writer.writeAll(" ");
        }
    }

    try output.md_writer.writer.writeAll("\n");

    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                ];
            \\                </script>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

fn source_contains_newline_in_range(source: []const u8, start: usize, end: usize) bool {
    for (source[start..end]) |c| {
        if (c == '\n') return true;
    }
    return false;
}

/// Generate PARSE2 section using SExprTree for both markdown and HTML
fn generateParseSection(output: *DualOutput, content: *const Content, parse_ast: *AST, env: *CommonEnv, linecol_mode: LineColMode) (Allocator.Error || error{ WriteFailed, ErrFinalizingHTMLWriter })!void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();

    // Generate SExprTree node based on content type
    switch (content.meta.node_type) {
        .file => {
            const file = parse_ast.store.getFile();
            try file.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .header => {
            const header = parse_ast.store.getHeader(@enumFromInt(parse_ast.root_node_idx));
            try header.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .expr => {
            const expr = parse_ast.store.getExpr(@enumFromInt(parse_ast.root_node_idx));
            try expr.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .mono => {
            const file = parse_ast.store.getFile();
            try file.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .statement => {
            const stmt = parse_ast.store.getStatement(@enumFromInt(parse_ast.root_node_idx));
            try stmt.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .package => {
            const file = parse_ast.store.getFile();
            try file.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .platform => {
            const file = parse_ast.store.getFile();
            try file.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .app => {
            const file = parse_ast.store.getFile();
            try file.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .repl => {
            // REPL doesn't use parse trees
            return;
        },
        .snippet => {
            const file = parse_ast.store.getFile();
            try file.pushToSExprTree(output.gpa, env, parse_ast, &tree);
        },
        .dev_object, .docs => unreachable, // Handled separately
    }

    // Only generate section if we have content on the stack
    if (tree.stack.items.len > 0) {
        try output.begin_section("PARSE");
        try output.begin_code_block("clojure");

        try tree.toStringPretty(&output.md_writer.writer, linecol_mode);
        try output.md_writer.writer.writeAll("\n");

        // Generate HTML output with syntax highlighting
        if (output.html_writer) |writer| {
            try writer.writer.writeAll(
                \\                <pre class="ast-parse">
            );

            try tree.toHtml(&writer.writer, linecol_mode);

            try writer.writer.writeAll(
                \\</pre>
                \\
            );
        }

        try output.end_code_block();
        try output.end_section();
    }
}

/// Generate FORMATTED section for both markdown and HTML
fn generateFormattedSection(output: *DualOutput, content: *const Content, parse_ast: *AST) SnapshotError!void {
    var formatted: std.Io.Writer.Allocating = .init(output.gpa);
    defer formatted.deinit();

    switch (content.meta.node_type) {
        .file => {
            try fmt.formatAst(parse_ast.*, &formatted.writer);
        },
        .header => {
            try fmt.formatHeader(parse_ast.*, &formatted.writer);
            try formatted.writer.writeByte('\n');
        },
        .expr => {
            try fmt.formatExpr(parse_ast.*, &formatted.writer);
            try formatted.writer.writeByte('\n');
        },
        .mono => {
            try fmt.formatAst(parse_ast.*, &formatted.writer);
        },
        .statement => {
            try fmt.formatStatement(parse_ast.*, &formatted.writer);
            try formatted.writer.writeByte('\n');
        },
        .package => {
            try fmt.formatAst(parse_ast.*, &formatted.writer);
        },
        .platform => {
            try fmt.formatAst(parse_ast.*, &formatted.writer);
        },
        .app => {
            try fmt.formatAst(parse_ast.*, &formatted.writer);
        },
        .repl => {
            // REPL doesn't use formatting
            return;
        },
        .snippet => {
            try fmt.formatAst(parse_ast.*, &formatted.writer);
        },
        .dev_object, .docs => unreachable, // Handled separately
    }

    const is_changed = !std.mem.eql(u8, formatted.written(), content.source);
    const display_content = if (is_changed) formatted.written() else "NO CHANGE\n";

    try output.begin_section("FORMATTED");
    try output.begin_code_block("roc");

    try output.md_writer.writer.writeAll(display_content);

    // HTML FORMATTED section
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <pre>
        );

        // Escape HTML in formatted content
        for (display_content) |char| {
            try escapeHtmlChar(&writer.writer, char);
        }

        try writer.writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate CANONICALIZE section for both markdown and HTML
fn generateCanonicalizeSection(output: *DualOutput, can_ir: *ModuleEnv, maybe_expr_idx: ?CIR.Expr.Idx, linecol_mode: LineColMode) (Allocator.Error || error{ WriteFailed, ErrFinalizingHTMLWriter })!void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();
    try can_ir.pushToSExprTree(maybe_expr_idx, &tree);

    try output.begin_section("CANONICALIZE");
    try output.begin_code_block("clojure");

    try tree.toStringPretty(&output.md_writer.writer, linecol_mode);
    try output.md_writer.writer.writeAll("\n");

    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <pre>
        );
        try tree.toHtml(&writer.writer, linecol_mode);
        try writer.writer.writeAll(
            \\</pre>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section for both markdown and HTML
fn generateTypesSection(output: *DualOutput, can_ir: *ModuleEnv, maybe_expr_idx: ?CIR.Expr.Idx, linecol_mode: LineColMode) (Allocator.Error || error{ WriteFailed, ErrFinalizingHTMLWriter })!void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();
    try can_ir.pushTypesToSExprTree(maybe_expr_idx, &tree);

    try output.begin_section("TYPES");
    try output.begin_code_block("clojure");
    try tree.toStringPretty(&output.md_writer.writer, linecol_mode);
    try output.md_writer.writer.writeAll("\n");

    // HTML TYPES section
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <pre>
        );
        try tree.toHtml(&writer.writer, linecol_mode);
        try writer.writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Compute the correct type for a closure-transformed expression.
/// Instead of copying the old function type, this builds the appropriate type
/// based on the new expression's structure (e.g., tag union for closures).
fn computeTransformedExprType(
    can_ir: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!types.Var {
    const expr_var = ModuleEnv.varFrom(expr_idx);

    // Ensure type var exists for this expression
    if (@intFromEnum(expr_var) >= can_ir.types.len()) {
        const current_len: usize = @intCast(can_ir.types.len());
        const needed_len: usize = @intCast(@intFromEnum(expr_var) + 1);
        var i: usize = current_len;
        while (i < needed_len) : (i += 1) {
            try can_ir.types.fresh();
        }
    }

    const expr = can_ir.store.getExpr(expr_idx);

    switch (expr) {
        .e_tag => |tag| {
            // A tag expression (e.g., Closure_1({ y: y }))
            // Type: [TagName(PayloadType)]
            const tag_args = can_ir.store.exprSlice(tag.args);
            if (tag_args.len == 1) {
                // Single arg is the capture record - compute its type
                const record_expr_idx = tag_args[0];
                const record_type = try computeTransformedExprType(can_ir, record_expr_idx);

                // Build tag union type: [TagName(record_type)]
                const tag_type = try can_ir.types.mkTag(tag.name, &[_]types.Var{record_type});
                const ext_var = try can_ir.types.fresh();
                const content = try can_ir.types.mkTagUnion(&[_]types.Tag{tag_type}, ext_var);
                try can_ir.types.setVarContent(expr_var, content);
            } else if (tag_args.len == 0) {
                // Tag with no payload
                const tag_type = try can_ir.types.mkTag(tag.name, &[_]types.Var{});
                const ext_var = try can_ir.types.fresh();
                const content = try can_ir.types.mkTagUnion(&[_]types.Tag{tag_type}, ext_var);
                try can_ir.types.setVarContent(expr_var, content);
            }
            return expr_var;
        },
        .e_record => |record| {
            // Record expression - build record type from field types
            const fields_slice = can_ir.store.sliceRecordFields(record.fields);

            // Build record fields for the type
            var type_fields = std.ArrayList(types.RecordField).empty;
            defer type_fields.deinit(can_ir.gpa);

            for (fields_slice) |field_idx| {
                const field = can_ir.store.getRecordField(field_idx);
                // Get the type of the field value expression
                const field_type = try computeTransformedExprType(can_ir, field.value);
                try type_fields.append(can_ir.gpa, .{ .name = field.name, .var_ = field_type });
            }

            const fields_range = try can_ir.types.appendRecordFields(type_fields.items);
            const ext_var = try can_ir.types.fresh();
            const content = types.Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };
            try can_ir.types.setVarContent(expr_var, content);
            return expr_var;
        },
        .e_lambda => |lambda| {
            // Lambda expression - build function type
            // Get argument types from patterns
            const arg_patterns = can_ir.store.slicePatterns(lambda.args);

            var arg_types = std.ArrayList(types.Var).empty;
            defer arg_types.deinit(can_ir.gpa);

            for (arg_patterns) |pattern_idx| {
                try arg_types.append(can_ir.gpa, ModuleEnv.varFrom(pattern_idx));
            }

            // Get return type from body (recursively compute it)
            const ret_type = try computeTransformedExprType(can_ir, lambda.body);

            // Build function type
            const args_range = try can_ir.types.appendVars(arg_types.items);
            const func = types.Func{ .args = args_range, .ret = ret_type, .needs_instantiation = false };
            const content = types.Content{ .structure = .{ .fn_pure = func } };
            try can_ir.types.setVarContent(expr_var, content);
            return expr_var;
        },
        .e_lookup_local => |local| {
            // Lookup - find the definition for this pattern and use its expression's type
            // This ensures we get the transformed type, not the original pattern type
            const defs = can_ir.store.sliceDefs(can_ir.all_defs);
            for (defs) |def_idx| {
                const def = can_ir.store.getDef(def_idx);
                if (def.pattern == local.pattern_idx) {
                    // Found the definition - recursively compute the expression's type
                    return try computeTransformedExprType(can_ir, def.expr);
                }
            }
            // Fallback to pattern's type if definition not found
            // Check if the pattern's type variable is within the type store's bounds
            // (patterns created after type checking, like lifted function patterns, need fresh type vars)
            const pattern_var = ModuleEnv.varFrom(local.pattern_idx);
            if (@intFromEnum(pattern_var) >= can_ir.types.len()) {
                // Create fresh type variables up to this index
                const current_len: usize = @intCast(can_ir.types.len());
                const needed_len: usize = @intCast(@intFromEnum(pattern_var) + 1);
                var i: usize = current_len;
                while (i < needed_len) : (i += 1) {
                    try can_ir.types.fresh();
                }
            }
            return pattern_var;
        },
        .e_call => |call| {
            // Function call - the type is the return type of the function being called
            // First compute the function's type
            const func_type = try computeTransformedExprType(can_ir, call.func);
            const func_resolved = can_ir.types.resolveVar(func_type);

            // If it's a function type, set the call's type to the return type
            // This is important for newly created call expressions (from closure transform)
            // which may not have had their type vars initialized during type checking
            if (func_resolved.desc.content == .structure) {
                const flat_type = func_resolved.desc.content.structure;
                switch (flat_type) {
                    .fn_pure, .fn_effectful, .fn_unbound => |func| {
                        // Set the call expression's type to match the function's return type
                        const ret_resolved = can_ir.types.resolveVar(func.ret);
                        try can_ir.types.setVarContent(expr_var, ret_resolved.desc.content);
                        return expr_var;
                    },
                    else => {},
                }
            }
            // Fall back to original expression type
            return expr_var;
        },
        .e_num => {
            // Numeric literal - use the original expression's type (with numeral constraint)
            return expr_var;
        },
        .e_block => |block| {
            // Block - the type is the type of the final expression
            return try computeTransformedExprType(can_ir, block.final_expr);
        },
        .e_binop => |binop| {
            // Binary operation - the result type is typically the same as the operand types
            // For arithmetic operations, use the left operand's type (both should be the same)
            return try computeTransformedExprType(can_ir, binop.lhs);
        },
        .e_match => |match_expr| {
            // Match expression - the type is the type of the branch bodies
            // All branches should have the same type, so we use the first branch
            const branches = can_ir.store.sliceMatchBranches(match_expr.branches);
            if (branches.len > 0) {
                const first_branch = can_ir.store.getMatchBranch(branches[0]);
                return try computeTransformedExprType(can_ir, first_branch.value);
            }
            return expr_var;
        },
        else => {
            // For other expressions, use the original type from type-checking
            return expr_var;
        },
    }
}

/// Get the defaulted (monomorphized) type string for an expression.
/// This defaults flex vars with from_numeral constraint to Dec.
/// Uses a seen set for cycle detection.
fn getDefaultedTypeString(allocator: std.mem.Allocator, can_ir: *ModuleEnv, type_var: types.Var) (Allocator.Error || error{WriteFailed})![]const u8 {
    var seen = std.ArrayList(types.Var).empty;
    defer seen.deinit(allocator);
    return getDefaultedTypeStringWithSeen(allocator, can_ir, type_var, &seen, true);
}

fn getDefaultedTypeStringWithSeen(
    allocator: std.mem.Allocator,
    can_ir: *ModuleEnv,
    type_var: types.Var,
    seen: *std.ArrayList(types.Var),
    is_top_level: bool,
) (Allocator.Error || error{WriteFailed})![]const u8 {
    const resolved = can_ir.types.resolveVar(type_var);

    // Check for cycle - use "_" (type wildcard) for cyclic references
    // This is valid Roc syntax, unlike "..." which was causing parse errors
    for (seen.items) |seen_var| {
        if (seen_var == resolved.var_) {
            return allocator.dupe(u8, "_");
        }
    }

    // Add to seen set
    try seen.append(allocator, resolved.var_);
    defer _ = seen.pop();

    switch (resolved.desc.content) {
        .flex => {
            // Fall through to TypeWriter. `finalizeLiteralDefaults` already
            // committed concrete `Dec` to the store for any defaulted numeral
            // before MONO renders, so the live store is the source of truth —
            // no display-time `Dec` substitution is needed here.
        },
        .structure => |flat_type| {
            switch (flat_type) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    // For top-level function types, let TypeWriter handle it
                    // so that where clauses are properly included
                    if (is_top_level) {
                        // Fall through to TypeWriter at the end
                    } else {
                        // For nested function types (e.g., in record fields), build manually
                        // Use Roc syntax: a, b -> c (not curried a -> b -> c)
                        var result = std.array_list.Managed(u8).init(allocator);
                        errdefer result.deinit();

                        const arg_vars = can_ir.types.sliceVars(func.args);
                        for (arg_vars, 0..) |arg_var, i| {
                            if (i > 0) try result.appendSlice(", ");
                            const arg_type = try getDefaultedTypeStringWithSeen(allocator, can_ir, arg_var, seen, false);
                            defer allocator.free(arg_type);
                            try result.appendSlice(arg_type);
                        }

                        try result.appendSlice(" -> ");

                        // Check if return type is also a function - if so, wrap in parens
                        const ret_resolved = can_ir.types.resolveVar(func.ret);
                        const ret_is_fn = ret_resolved.desc.content == .structure and
                            (ret_resolved.desc.content.structure == .fn_pure or
                                ret_resolved.desc.content.structure == .fn_effectful or
                                ret_resolved.desc.content.structure == .fn_unbound);

                        const ret_type = try getDefaultedTypeStringWithSeen(allocator, can_ir, func.ret, seen, false);
                        defer allocator.free(ret_type);

                        if (ret_is_fn) {
                            try result.append('(');
                            try result.appendSlice(ret_type);
                            try result.append(')');
                        } else {
                            try result.appendSlice(ret_type);
                        }

                        return result.toOwnedSlice();
                    }
                },
                .tag_union => |tag_union| {
                    // Emit tag union as closed union (without extension variable)
                    var result = std.array_list.Managed(u8).init(allocator);
                    errdefer result.deinit();

                    try result.append('[');
                    const tags_slice = can_ir.types.getTagsSlice(tag_union.tags);
                    for (tags_slice.items(.name), tags_slice.items(.args), 0..) |tag_name_idx, tag_args, i| {
                        if (i > 0) try result.appendSlice(", ");

                        const tag_name = can_ir.getIdent(tag_name_idx);
                        try result.appendSlice(tag_name);

                        // Add payload types if any
                        const arg_vars = can_ir.types.sliceVars(tag_args);
                        if (arg_vars.len > 0) {
                            try result.append('(');
                            for (arg_vars, 0..) |arg_var, j| {
                                if (j > 0) try result.appendSlice(", ");
                                const arg_type = try getDefaultedTypeStringWithSeen(allocator, can_ir, arg_var, seen, false);
                                defer allocator.free(arg_type);
                                try result.appendSlice(arg_type);
                            }
                            try result.append(')');
                        }
                    }
                    try result.append(']');

                    return result.toOwnedSlice();
                },
                .record => |record| {
                    // Emit record as closed record (without extension variable)
                    var result = std.array_list.Managed(u8).init(allocator);
                    errdefer result.deinit();

                    try result.appendSlice("{ ");
                    const fields_slice = can_ir.types.getRecordFieldsSlice(record.fields);
                    const field_names = fields_slice.items(.name);
                    const field_vars = fields_slice.items(.var_);
                    for (field_names, field_vars, 0..) |field_name_idx, field_var, i| {
                        if (i > 0) try result.appendSlice(", ");

                        const field_name = can_ir.getIdent(field_name_idx);
                        try result.appendSlice(field_name);
                        try result.appendSlice(" : ");

                        const field_type = try getDefaultedTypeStringWithSeen(allocator, can_ir, field_var, seen, false);
                        defer allocator.free(field_type);
                        try result.appendSlice(field_type);
                    }
                    try result.appendSlice(" }");

                    return result.toOwnedSlice();
                },
                else => {},
            }
        },
        else => {},
    }

    // Use TypeWriter for all other cases - it has proper cycle detection.
    var type_writer = try can_ir.initTypeWriter();
    defer type_writer.deinit();

    if (is_top_level) {
        try type_writer.write(type_var, .one_line);
    } else {
        try type_writer.writeWithoutConstraints(type_var);
    }

    // Copy the result since type_writer will be deinitialized
    return allocator.dupe(u8, type_writer.get());
}

/// Check if a pattern is a top-level definition.
/// Top-level captures are always in scope and should not be lifted as closure parameters.
fn isTopLevelPattern(can_ir: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) bool {
    const defs = can_ir.store.sliceDefs(can_ir.all_defs);
    for (defs) |def_idx| {
        const def = can_ir.store.getDef(def_idx);
        if (def.pattern == pattern_idx) {
            return true;
        }
    }
    return false;
}

/// Get the monomorphized type string for an expression.
/// For closures, this includes capture types as leading function arguments.
fn getMonoTypeString(allocator: std.mem.Allocator, can_ir: *ModuleEnv, expr_idx: CIR.Expr.Idx) Allocator.Error![]const u8 {
    const expr = can_ir.store.getExpr(expr_idx);

    // For blocks, get the type of the final expression (what the block evaluates to)
    if (expr == .e_block) {
        return getMonoTypeString(allocator, can_ir, expr.e_block.final_expr);
    }

    // Handle closures specially - include capture types in the function type
    if (expr == .e_closure) {
        const closure = expr.e_closure;
        const captures = can_ir.store.sliceCaptures(closure.captures);

        // Get the lambda's function type
        const lambda_var = ModuleEnv.varFrom(closure.lambda_idx);
        const resolved = can_ir.types.resolveVar(lambda_var);

        // Check if this is a function type
        if (resolved.desc.content == .structure) {
            const flat_type = resolved.desc.content.structure;
            if (flat_type == .fn_pure or flat_type == .fn_effectful or flat_type == .fn_unbound) {
                const func = switch (flat_type) {
                    .fn_pure => |f| f,
                    .fn_effectful => |f| f,
                    .fn_unbound => |f| f,
                    else => unreachable,
                };

                var result = std.array_list.Managed(u8).init(allocator);
                errdefer result.deinit();

                // First, add non-top-level capture types (top-level captures are always in scope)
                var emitted_captures: u32 = 0;
                for (captures) |capture_idx| {
                    const capture = can_ir.store.getCapture(capture_idx);

                    // Skip top-level captures - they're always in scope
                    if (isTopLevelPattern(can_ir, capture.pattern_idx)) continue;

                    if (emitted_captures > 0) try result.appendSlice(", ");
                    emitted_captures += 1;

                    const capture_var = ModuleEnv.varFrom(capture.pattern_idx);
                    const capture_type = try getDefaultedTypeString(allocator, can_ir, capture_var);
                    defer allocator.free(capture_type);
                    try result.appendSlice(capture_type);
                }

                // Then add the lambda's own argument types
                const arg_vars = can_ir.types.sliceVars(func.args);
                for (arg_vars, 0..) |arg_var, i| {
                    if (emitted_captures > 0 or i > 0) try result.appendSlice(", ");
                    const arg_type = try getDefaultedTypeString(allocator, can_ir, arg_var);
                    defer allocator.free(arg_type);
                    try result.appendSlice(arg_type);
                }

                try result.appendSlice(" -> ");

                // Get the return type - if the body is also a closure, recursively process it
                const lambda_expr = can_ir.store.getExpr(closure.lambda_idx);
                std.debug.assert(lambda_expr == .e_lambda);
                const body_expr = can_ir.store.getExpr(lambda_expr.e_lambda.body);

                const is_nested_function = body_expr == .e_closure;
                const ret_type = if (is_nested_function)
                    // Recursively process nested closures to include their captures
                    try getMonoTypeString(allocator, can_ir, lambda_expr.e_lambda.body)
                else
                    try getDefaultedTypeString(allocator, can_ir, func.ret);
                defer allocator.free(ret_type);

                // Nested function types need parens in Roc
                if (is_nested_function) try result.appendSlice("(");
                try result.appendSlice(ret_type);
                if (is_nested_function) try result.appendSlice(")");

                return result.toOwnedSlice();
            }
        }
    }

    // For non-closures, just use the regular type defaulting
    const expr_var = ModuleEnv.varFrom(expr_idx);
    return getDefaultedTypeString(allocator, can_ir, expr_var);
}

/// Validate that the MONO output is valid Roc code by parsing, canonicalizing, and type-checking it.
/// Returns true if validation passed, false if there were errors.
fn validateMonoOutput(allocator: Allocator, mono_source: []const u8, source_path: []const u8, config: *const Config) bool {
    // Create a module environment for validation
    var validation_env = ModuleEnv.init(allocator, mono_source) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to create validation environment: {}", .{ source_path, err });
        return false;
    };
    defer validation_env.deinit();

    // Calculate line starts for error reporting
    validation_env.common.calcLineStarts(allocator) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to calculate line starts: {}", .{ source_path, err });
        return false;
    };

    // Parse the MONO output as a headerless type module
    const validation_ast = parse.file(allocator, &validation_env.common) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Parse failed: {}", .{ source_path, err });
        return false;
    };
    defer validation_ast.deinit();

    // Check for parse errors
    if (validation_ast.hasErrors()) {
        const tokenize_errs = validation_ast.tokenize_diagnostics.items.len;
        const parse_errs = validation_ast.parse_diagnostics.items.len;
        std.log.err("MONO PARSE ERROR in {s}: {d} tokenize error(s), {d} parse error(s) in generated MONO output:", .{ source_path, tokenize_errs, parse_errs });
        for (validation_ast.tokenize_diagnostics.items) |diag| {
            const tag_name = @tagName(diag.tag);
            std.log.err("  - tokenize: {s}", .{tag_name});
        }
        for (validation_ast.parse_diagnostics.items) |diag| {
            const tag_name = @tagName(diag.tag);
            std.log.err("  - parse: {s}", .{tag_name});
        }
        std.log.err("MONO source that failed to parse:\n{s}", .{mono_source});
        return false;
    }

    // Initialize CIR fields for canonicalization
    validation_env.initCIRFields("mono_validation") catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to init CIR fields: {}", .{ source_path, err });
        return false;
    };

    const builtin_env = config.builtin_module orelse {
        std.log.err("MONO VALIDATION ERROR in {s}: Missing builtin module context", .{source_path});
        return false;
    };

    // Canonicalize the parsed MONO output
    const mono_roc_ctx = CoreCtx.default(allocator, allocator, app_io);
    var czer = Can.initModule(mono_roc_ctx, &validation_env, validation_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_env,
            .builtin_indices = config.builtin_indices,
        },
    }) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to initialize canonicalizer: {}", .{ source_path, err });
        return false;
    };
    defer czer.deinit();

    czer.canonicalizeFile() catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Canonicalization failed: {}", .{ source_path, err });
        return false;
    };

    // Check for canonicalization diagnostics (skip warnings, only fail on errors)
    const can_diagnostics = validation_env.getDiagnostics() catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to get diagnostics: {}", .{ source_path, err });
        return false;
    };
    defer allocator.free(can_diagnostics);

    // Count only actual errors, not warnings
    var error_count: usize = 0;
    for (can_diagnostics) |diagnostic| {
        switch (diagnostic) {
            .shadowing_warning,
            .unreachable_string_pattern_capture,
            => {}, // Skip warnings
            else => error_count += 1,
        }
    }

    if (error_count > 0) {
        std.log.err("MONO CANONICALIZATION ERROR in {s}: {d} error(s) in generated MONO output:", .{ source_path, error_count });
        for (can_diagnostics) |diagnostic| {
            switch (diagnostic) {
                .shadowing_warning,
                .unreachable_string_pattern_capture,
                => {}, // Skip warnings in output too
                else => {
                    const tag_name = @tagName(diagnostic);
                    std.log.err("  - {s}", .{tag_name});
                },
            }
        }
        std.log.err("MONO source that failed canonicalization:\n{s}", .{mono_source});
        return false;
    }

    // Type-check the canonicalized MONO output
    // Create a BuiltinContext using the config's builtin information
    const module_name = validation_env.insertIdent(base.Ident.for_text("mono_validation")) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to insert module name: {}", .{ source_path, err });
        return false;
    };

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = module_name,
        .bool_stmt = config.builtin_indices.bool_type,
        .try_stmt = config.builtin_indices.try_type,
        .str_stmt = config.builtin_indices.str_type,
        .builtin_module = config.builtin_module,
        .builtin_indices = config.builtin_indices,
    };

    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs_map.deinit();

    Can.populateModuleEnvs(&module_envs_map, &validation_env, builtin_env, config.builtin_indices) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to populate auto-imported types: {}", .{ source_path, err });
        return false;
    };

    const imported_modules: []const *const ModuleEnv = &.{builtin_env};
    validation_env.imports.clearResolvedModules();
    validation_env.imports.resolveImportsByExactModuleName(&validation_env, imported_modules) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to resolve imports: {}", .{ source_path, err });
        return false;
    };
    validation_env.imports.markUnresolvedImportsFailedBeforeChecking();

    var checker = Check.init(
        allocator,
        &validation_env.types,
        &validation_env,
        imported_modules,
        &module_envs_map,
        &validation_env.store.regions,
        builtin_ctx,
    ) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to initialize type checker: {}", .{ source_path, err });
        return false;
    };
    checker.fixupTypeWriter();
    defer checker.deinit();

    checker.checkFile() catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Type checking failed: {}", .{ source_path, err });
        return false;
    };

    // Check for type-checking problems
    const type_problems = checker.problems.problems.items;
    if (type_problems.len > 0) {
        std.log.err("TYPE ERROR IN GENERATED ROC SOURCE in {s}: {d} type error(s) in generated MONO output:", .{ source_path, type_problems.len });
        for (type_problems) |problem| {
            const tag_name = @tagName(problem);
            std.log.err("  - {s}", .{tag_name});
        }
        std.log.err("MONO source that failed type-checking:\n{s}", .{mono_source});
        return false;
    }

    return true;
}

/// Validate that the MONO output is properly formatted.
/// Returns true if the code is already properly formatted, false if there are formatting differences.
/// If formatting differs, logs the differences as an error.
fn validateMonoFormatting(allocator: Allocator, mono_source: []const u8, source_path: []const u8) bool {
    // Parse and format the code
    const formatted = parseAndFormat(allocator, mono_source) catch |err| {
        std.log.err("MONO FORMATTING ERROR in {s}: Failed to format: {}", .{ source_path, err });
        return false;
    };
    defer allocator.free(formatted);

    // Compare formatted with original
    if (std.mem.eql(u8, mono_source, formatted)) {
        return true; // Already properly formatted
    }

    // Code is not properly formatted - report the difference
    std.log.err("MONO FORMATTING ERROR in {s}: MONO section is not properly formatted.", .{source_path});
    std.log.err("=== ORIGINAL ===", .{});
    std.log.err("{s}", .{mono_source});
    std.log.err("=== FORMATTED (expected) ===", .{});
    std.log.err("{s}", .{formatted});
    std.log.err("=== END DIFF ===", .{});

    return false;
}

/// Parse Roc source and return formatted output.
fn parseAndFormat(gpa: std.mem.Allocator, input: []const u8) SnapshotError![]const u8 {
    var module_env = try ModuleEnv.init(gpa, input);
    defer module_env.deinit();

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();

    // Check for parse errors - if there are any, we can't format
    if (parse_ast.hasErrors()) {
        return error.ParseFailed;
    }

    var result: std.Io.Writer.Allocating = .init(gpa);
    errdefer result.deinit();
    try fmt.formatAst(parse_ast.*, &result.writer);

    return try result.toOwnedSlice();
}

/// Check if a type string contains type variables (single lowercase letters like 'a', 'b').
/// This indicates a polymorphic type that hasn't been fully monomorphized.
fn typeStringIsPolymorphic(type_str: []const u8) bool {
    var i: usize = 0;
    while (i < type_str.len) : (i += 1) {
        const c = type_str[i];
        if (c >= 'a' and c <= 'z') {
            // Check if this is a standalone single letter (not part of a word)
            const prev_is_ident = i > 0 and (std.ascii.isAlphanumeric(type_str[i - 1]) or type_str[i - 1] == '_');
            const next_is_ident = i + 1 < type_str.len and (std.ascii.isAlphanumeric(type_str[i + 1]) or type_str[i + 1] == '_');
            if (!prev_is_ident and !next_is_ident) {
                return true;
            }
        }
    }
    return false;
}

/// Check if an identifier name appears as a reference in the given text.
/// Checks for whole-word matches (not substrings of other identifiers).
fn isIdentReferencedIn(name: []const u8, text: []const u8) bool {
    if (name.len == 0) return false;
    var pos: usize = 0;
    while (std.mem.findPos(u8, text, pos, name)) |idx| {
        const before_ok = idx == 0 or (!std.ascii.isAlphanumeric(text[idx - 1]) and text[idx - 1] != '_');
        const after_idx = idx + name.len;
        const after_ok = after_idx >= text.len or (!std.ascii.isAlphanumeric(text[after_idx]) and text[after_idx] != '_');
        if (before_ok and after_ok) return true;
        pos = idx + 1;
    }
    return false;
}

/// Generate MONO section for mono tests - emits monomorphized type module
fn generateMonoSection(output: *DualOutput, can_ir: *ModuleEnv, _: ?CIR.Expr.Idx, source_path: []const u8, config: *const Config) (Allocator.Error || error{ WriteFailed, NoSpaceLeft, MonoValidationFailed, MonoFormattingFailed })!void {
    // First, build the mono source in a buffer for validation
    var mono_buffer = std.ArrayList(u8).empty;
    defer mono_buffer.deinit(output.gpa);

    // Emit all top-level definitions (no module header - type modules are headerless)
    var emitter = can.RocEmitter.init(output.gpa, can_ir);
    defer emitter.deinit();

    const defs = can_ir.store.sliceDefs(can_ir.all_defs);

    // Two-pass approach: first emit all defs to collect their text,
    // then skip unreferenced polymorphic defs (dead code from constant folding).
    const DefInfo = struct {
        pattern_output: []const u8,
        expr_output: []const u8,
        type_str: []const u8,
        is_polymorphic: bool,
    };
    var def_infos = std.ArrayList(DefInfo).empty;
    defer {
        for (def_infos.items) |info| {
            output.gpa.free(info.pattern_output);
            output.gpa.free(info.expr_output);
            output.gpa.free(info.type_str);
        }
        def_infos.deinit(output.gpa);
    }

    for (defs) |def_idx| {
        const def = can_ir.store.getDef(def_idx);

        // Emit the pattern (left side of =)
        emitter.reset();
        try emitter.emitPattern(def.pattern);
        const pattern_output = try output.gpa.dupe(u8, emitter.getOutput());

        // Emit the expression (right side of =)
        emitter.reset();
        try emitter.emitExpr(def.expr);
        const expr_output = try output.gpa.dupe(u8, emitter.getOutput());

        // Use the pattern's type from type checking (not computed from expression)
        const pattern_type = ModuleEnv.varFrom(def.pattern);
        const type_str = try getDefaultedTypeString(output.gpa, can_ir, pattern_type);

        // Check if the type is polymorphic (contains type variables like 'a', 'b', etc.)
        const is_polymorphic = typeStringIsPolymorphic(type_str);

        try def_infos.append(output.gpa, .{
            .pattern_output = pattern_output,
            .expr_output = expr_output,
            .type_str = type_str,
            .is_polymorphic = is_polymorphic,
        });
    }

    // Build a combined string of all non-polymorphic expressions to check references
    var all_exprs = std.ArrayList(u8).empty;
    defer all_exprs.deinit(output.gpa);
    for (def_infos.items) |info| {
        if (!info.is_polymorphic) {
            try all_exprs.appendSlice(output.gpa, info.expr_output);
            try all_exprs.append(output.gpa, '\n');
        }
    }

    for (def_infos.items) |info| {
        // Skip polymorphic defs that are unreferenced by any other def's expression.
        // These are dead code from constant folding (e.g., func was called but the
        // result was folded to a constant, leaving func's polymorphic type unresolvable).
        if (info.is_polymorphic) {
            if (!isIdentReferencedIn(info.pattern_output, all_exprs.items)) continue;
        }

        // Only monomorphic definitions get explicit annotations here. This
        // snapshot emitter reconstructs source for tooling; constrained
        // polymorphic helper types can contain static-dispatch constraints that
        // are not valid as standalone generated source annotations.
        if (!info.is_polymorphic) {
            try mono_buffer.appendSlice(output.gpa, info.pattern_output);
            try mono_buffer.appendSlice(output.gpa, " : ");
            try mono_buffer.appendSlice(output.gpa, info.type_str);
            try mono_buffer.appendSlice(output.gpa, "\n");
        }
        try mono_buffer.appendSlice(output.gpa, info.pattern_output);
        try mono_buffer.appendSlice(output.gpa, " = ");
        try mono_buffer.appendSlice(output.gpa, info.expr_output);
        try mono_buffer.appendSlice(output.gpa, "\n\n");
    }

    // Trim trailing newline (we added one too many at the end)
    if (mono_buffer.items.len > 0 and mono_buffer.items[mono_buffer.items.len - 1] == '\n') {
        _ = mono_buffer.pop();
    }

    // Validate the MONO output for both non-closure and closure transforms
    if (!validateMonoOutput(output.gpa, mono_buffer.items, source_path, config)) {
        return error.MonoValidationFailed;
    }
    if (!validateMonoFormatting(output.gpa, mono_buffer.items, source_path)) {
        return error.MonoFormattingFailed;
    }

    // Write the validated output to the section
    try output.begin_section("MONO");
    try output.begin_code_block("roc");
    try output.md_writer.writer.writeAll(mono_buffer.items);

    // HTML MONO section
    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <pre>
        );
        // Re-emit for HTML (simplified - just copy the markdown content idea)
        // For now, just show a placeholder
        try writer.writer.writeAll("(see markdown)\n");
        try writer.writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate HTML document structure and JavaScript
fn generateHtmlWrapper(output: *DualOutput, content: *const Content) error{WriteFailed}!void {
    const writer = output.html_writer orelse return;

    // Write HTML document structure
    try writer.writer.writeAll(
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\    <meta charset="UTF-8">
        \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\    <title>Roc Snapshot:
    );
    try writer.writer.writeAll(content.meta.description);
    try writer.writer.writeAll(
        \\</title>
        \\    <style>
        \\
    );
    try writer.writer.writeAll(@embedFile("snapshot.css"));
    try writer.writer.writeAll(
        \\    </style>
        \\</head>
        \\<body>
        \\    <!-- Two-column layout (main and only view) -->
        \\    <div class="two-column-layout">
        \\        <div class="left-pane">
        \\            <div class="pane-header">
        \\                <select class="section-dropdown" id="left-selector" onchange="switchLeftPane()">
        \\                    <option value="META">META</option>
        \\                    <option value="SOURCE" selected>SOURCE</option>
        \\                </select>
        \\            </div>
        \\            <div class="pane-content" id="left-pane-content">
        \\                <!-- Left pane content will be shown here -->
        \\            </div>
        \\        </div>
        \\        <div class="right-pane">
        \\            <div class="pane-header">
        \\                <select class="section-dropdown" id="right-selector" onchange="switchRightPane()">
        \\                    <option value="TOKENS" selected>TOKENS</option>
        \\                    <option value="PARSE">PARSE</option>
        \\                    <option value="FORMATTED">FORMATTED</option>
        \\                    <option value="CANONICALIZE">CANONICALIZE</option>
        \\                    <option value="TYPES">TYPES</option>
        \\                </select>
        \\            </div>
        \\            <div class="pane-content" id="right-pane-content">
        \\                <!-- Right pane content will be shown here -->
        \\            </div>
        \\        </div>
        \\    </div>
        \\
        \\    <!-- Hidden sections for data storage -->
        \\    <div id="data-sections" style="display: none;">
    );
}

/// Generate HTML closing tags and JavaScript
fn generateHtmlClosing(output: *DualOutput) error{WriteFailed}!void {
    const writer = output.html_writer orelse return;

    // Close data sections container and add JavaScript
    try writer.writer.writeAll(
        \\    </div>
        \\
        \\    <script>
    );
    // Embed remaining snapshot.js directly into the HTML
    try writer.writer.writeAll(@embedFile("snapshot.js"));
    try writer.writer.writeAll(
        \\    </script>
        \\</body>
        \\</html>
        \\
    );
}

/// Write HTML buffer to file
fn writeHtmlFile(gpa: Allocator, snapshot_path: []const u8, html_buffer: *std.ArrayList(u8)) (Allocator.Error || error{WriteFailed})!void {
    // Convert .md path to .html path
    const html_path = blk: {
        if (std.mem.endsWith(u8, snapshot_path, ".md")) {
            const base_path = snapshot_path[0 .. snapshot_path.len - 3];
            break :blk try std.fmt.allocPrint(gpa, "{s}.html", .{base_path});
        } else {
            break :blk try std.fmt.allocPrint(gpa, "{s}.html", .{snapshot_path});
        }
    };
    defer gpa.free(html_path);

    // Write HTML file
    var html_file = std.Io.Dir.cwd().createFile(app_io, html_path, .{}) catch |err| {
        log("failed to create HTML file '{s}': {s}", .{ html_path, @errorName(err) });
        return;
    };
    defer html_file.close(app_io);
    var html_writer_buffer: [4096]u8 = undefined;
    var html_writer = html_file.writer(app_io, &html_writer_buffer);
    try html_writer.interface.writeAll(html_buffer.items);
    try html_writer.interface.flush();

    log("generated HTML version: {s}", .{html_path});
}

/// New unified processSnapshotFile function that generates both markdown and HTML simultaneously
fn processSnapshotFileUnified(gpa: Allocator, snapshot_path: []const u8, config: *const Config) (Allocator.Error || Error || error{WriteFailed})!bool {
    // Log the file path that was written to
    log("processing snapshot file: {s}", .{snapshot_path});

    const @"1Mb" = 1024 * 1024;
    const file_content = std.Io.Dir.cwd().readFileAlloc(app_io, snapshot_path, gpa, .limited(@"1Mb")) catch |err| {
        std.log.err("failed to read file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer gpa.free(file_content);

    // Check our file starts with the metadata section
    if (!std.mem.startsWith(u8, file_content, "# META")) {
        std.log.err("file '{s}' is not a valid snapshot file", .{snapshot_path});
        std.log.err("snapshot files must start with '# META'", .{});
        if (file_content.len > 0) {
            const first_line_end = std.mem.findScalar(u8, file_content, '\n') orelse @min(file_content.len, 50);
            const first_line = file_content[0..first_line_end];
            std.log.err("file starts with: '{s}'", .{first_line});
        }
        return false;
    }

    // Parse the file to find section boundaries
    const content = extractSections(gpa, file_content) catch |err| {
        switch (err) {
            Error.MissingSnapshotHeader => {
                std.log.err("file '{s}' is missing the META section header", .{snapshot_path});
                std.log.err("add a META section like: ~~~META\\ndescription=My test\\ntype=expr\\n", .{});
                return false;
            },
            Error.MissingSnapshotSource => {
                std.log.err("file '{s}' is missing the SOURCE section", .{snapshot_path});
                std.log.err("add a SOURCE section like: ~~~SOURCE\\nyour_roc_code_here\\n", .{});
                return false;
            },
            Error.BadSectionHeader => {
                std.log.err("file '{s}' has an invalid section header", .{snapshot_path});
                std.log.err("section headers must be like: ~~~META, ~~~SOURCE, etc.", .{});
                return false;
            },
            else => return err,
        }
    };

    // Validate trace-eval flag usage
    if (config.trace_eval and content.meta.node_type != .repl) {
        std.log.err("--trace-eval can only be used with REPL snapshots (type=repl), but '{s}' has type={s}", .{ snapshot_path, content.meta.node_type.toString() });
        std.process.exit(1);
    }

    // Process the content through the shared compilation pipeline
    const success = processSnapshotContent(gpa, content, snapshot_path, config) catch |err| {
        log("failed to process snapshot content: {s}", .{@errorName(err)});
        return false;
    };

    // If flag --fuzz-corpus is passed, write the SOURCE to our corpus
    if (config.maybe_fuzz_corpus_path) |path| {
        // For REPL snapshots, write each expression (without ») as a separate corpus file
        if (content.meta.node_type == .repl) {
            var parts = std.mem.splitSequence(u8, content.source, "»");
            // Skip the first part (before the first »)
            _ = parts.next();

            while (parts.next()) |part| {
                const trimmed = std.mem.trim(u8, part, " \t\r\n");
                if (trimmed.len > 0) {
                    try writeCorpusFile(gpa, path, trimmed, rand);
                }
            }
        } else {
            try writeCorpusFile(gpa, path, content.source, rand);
        }
    }

    return success;
}

/// Write a single source to the fuzz corpus with a random filename
fn writeCorpusFile(gpa: Allocator, path: []const u8, source: []const u8, rng: std.Random) (Allocator.Error || error{WriteFailed})!void {
    const rand_file_name = [_][]const u8{
        path,
        &[_]u8{
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            rng.intRangeAtMost(u8, 'a', 'z'),
            '.',
            'r',
            'o',
            'c',
        },
    };

    const corpus_file_path = try std.fs.path.join(gpa, &rand_file_name);
    defer gpa.free(corpus_file_path);

    var corpus_file = std.Io.Dir.cwd().createFile(app_io, corpus_file_path, .{}) catch |err| {
        std.log.err("failed to create file in '{s}': {s}", .{ path, @errorName(err) });
        return;
    };
    defer corpus_file.close(app_io);

    var write_buffer: [4096]u8 = undefined;
    var corpus_writer = corpus_file.writer(app_io, &write_buffer);
    const writer = &corpus_writer.interface;
    try writer.writeAll(source);
    try writer.flush();
}

fn processSnapshotFile(gpa: Allocator, snapshot_path: []const u8, config: *const Config) (Allocator.Error || Error || error{WriteFailed})!bool {
    return processSnapshotFileUnified(gpa, snapshot_path, config);
}

/// Extracts the sections from a snapshot file
pub fn extractSections(gpa: Allocator, content: []const u8) (Allocator.Error || Error)!Content {
    var ranges = std.AutoHashMap(Section, Section.Range).init(gpa);
    defer ranges.deinit();

    // Find all section headers and their positions
    var idx: usize = 0;
    while (idx < content.len) {
        // Look for section headers
        if (idx == 0 or (idx > 0 and content[idx - 1] == '\n')) {
            if (Section.fromString(content[idx..])) |section| {
                // Only process META, SOURCE, OUTPUT, EXPECTED, and DEV_OUTPUT sections
                if (section == .meta or section == .source or section == .expected or section == .output or section == .dev_output or section == .docs) {
                    // Determine header length - for multi-file SOURCE (no ~~~roc after # SOURCE),
                    // the header is just "# SOURCE\n"
                    const is_multi_file_source = section == .source and Section.isMultiFileSource(content[idx..]);
                    const header_len = if (is_multi_file_source) Section.SOURCE_MULTI.len else section.asString().len;
                    const start = idx + header_len;

                    // Find the end of this section
                    var end = content.len;

                    // For sections with ~~~ delimiters (META, single-file SOURCE, DEV_OUTPUT)
                    if (section == .meta or (section == .source and !is_multi_file_source) or section == .dev_output or section == .docs) {
                        // Find the closing ~~~
                        var search_idx = start;
                        while (search_idx < content.len - 3) {
                            if (content[search_idx] == '~' and
                                content[search_idx + 1] == '~' and
                                content[search_idx + 2] == '~')
                            {
                                // Set end to the position of ~~~, not after it
                                end = search_idx;
                                break;
                            }
                            search_idx += 1;
                        }
                    } else {
                        // For sections without ~~~ delimiters (EXPECTED, OUTPUT)
                        // or multi-file SOURCE (delimited by next top-level # section)
                        var search_idx = start;
                        while (search_idx < content.len) {
                            if (search_idx == 0 or (search_idx > 0 and content[search_idx - 1] == '\n')) {
                                // Match top-level section headers (# FOO) but NOT ## sub-headings
                                if (content[search_idx] == '#' and
                                    search_idx + 1 < content.len and
                                    content[search_idx + 1] == ' ')
                                {
                                    end = search_idx;
                                    break;
                                }
                            }
                            search_idx += 1;
                        }
                    }

                    try ranges.put(section, .{ .start = start, .end = end });

                    // Skip to the end of this section
                    idx = end;
                    continue;
                }
            }
        }
        idx += 1;
    }

    return try Content.from_ranges(ranges, content);
}

// Docs Snapshot Processing

/// Process a docs snapshot: parse multi-file source, compile with BuildEnv,
/// extract documentation from compiled modules, and serialize to S-expressions.
fn processDocsSnapshot(
    allocator: Allocator,
    content: Content,
    output_path: []const u8,
    config: *const Config,
) SnapshotError!bool {
    log("Processing docs snapshot: {s}", .{output_path});

    // 1. Parse multi-file source
    const source_files = try parseMultiFileSource(allocator, content.source);
    defer allocator.free(source_files);

    if (source_files.len == 0) {
        std.log.err("docs snapshot has no source files (need ## filename.roc sub-headings)", .{});
        return false;
    }

    // 2. Write source files to a temp directory
    const tmp_dir_name = makeSnapshotTempDir(allocator, "roc_snapshot_docs", output_path) catch |err| {
        std.log.err("Failed to resolve temp directory for {s}: {}", .{ output_path, err });
        return false;
    };
    defer allocator.free(tmp_dir_name);

    std.Io.Dir.cwd().createDirPath(app_io, tmp_dir_name) catch |err| {
        std.log.err("Failed to create temp directory {s}: {}", .{ tmp_dir_name, err });
        return false;
    };
    defer std.Io.Dir.cwd().deleteTree(app_io, tmp_dir_name) catch {};

    // Find the app file (first .roc file, or explicitly "app.roc")
    var app_filename: ?[]const u8 = null;
    for (source_files) |sf| {
        const sub_path = try std.fs.path.join(allocator, &.{ tmp_dir_name, sf.filename });
        defer allocator.free(sub_path);
        std.Io.Dir.cwd().writeFile(app_io, .{
            .sub_path = sub_path,
            .data = sf.content,
        }) catch |err| {
            std.log.err("Failed to write {s}: {}", .{ sf.filename, err });
            return false;
        };
        if (std.mem.eql(u8, sf.filename, "app.roc") or app_filename == null) {
            app_filename = sf.filename;
        }
    }

    const app_path = try std.fs.path.join(allocator, &.{ tmp_dir_name, app_filename.? });
    defer allocator.free(app_path);

    // 3. Build with BuildEnv
    const BuildEnv = compile.BuildEnv;
    const native_target = roc_target.RocTarget.detectNative();

    var build_env = BuildEnv.init(allocator, .single_threaded, 1, native_target, config.cwd, app_io) catch |err| {
        std.log.err("Failed to init BuildEnv: {}", .{err});
        return false;
    };
    defer build_env.deinit();
    build_env.setFinalizeExecutableArtifacts(false);

    build_env.build(app_path) catch |err| {
        std.log.err("BuildEnv.build failed for {s}: {}", .{ app_path, err });
        return false;
    };

    // 4. Get compiled modules and extract docs
    const modules = build_env.getCompiledModules(allocator) catch |err| {
        std.log.err("Failed to get compiled modules: {}", .{err});
        return false;
    };
    defer allocator.free(modules);

    if (modules.len == 0) {
        std.log.err("No modules were compiled", .{});
        return false;
    }

    // Extract docs from each compiled module
    var module_docs_list = std.ArrayList(docs_mod.DocModel.ModuleDocs).empty;
    defer {
        for (module_docs_list.items) |*md| md.deinit(allocator);
        module_docs_list.deinit(allocator);
    }

    for (modules) |mod| {
        // Docs show the alias the root uses for a package, not its internal
        // identity name (full URL or absolute path).
        const display_pkg_name = build_env.rootAliasForPackage(mod.package_name) orelse mod.package_name;
        var mod_docs = docs_mod.extract.extractModuleDocs(allocator, mod.semantic.env, display_pkg_name, mod.path) catch |err| {
            std.log.err("Failed to extract docs from module {s}: {}", .{ mod.name, err });
            continue;
        };
        errdefer mod_docs.deinit(allocator);
        // Override the module name with the clean name from CompiledModuleInfo
        const clean_name = try allocator.dupe(u8, mod.name);
        allocator.free(mod_docs.name);
        mod_docs.name = clean_name;
        try module_docs_list.append(allocator, mod_docs);
    }

    // Modules are collected in package hash-map order, which is not
    // deterministic; docs output must be.
    std.mem.sort(docs_mod.DocModel.ModuleDocs, module_docs_list.items, {}, docs_mod.DocModel.moduleDocsLessThan);

    // Build PackageDocs
    const package_name = try allocator.dupe(u8, "test-app");
    const modules_owned = try allocator.dupe(docs_mod.DocModel.ModuleDocs, module_docs_list.items);
    // Clear the list so deinit doesn't double-free
    module_docs_list.clearRetainingCapacity();

    var package_docs = docs_mod.DocModel.PackageDocs{
        .name = package_name,
        .modules = modules_owned,
    };
    defer package_docs.deinit(allocator);

    // 5. Serialize to S-expression
    var sexpr_buffer = std.ArrayList(u8).empty;
    defer sexpr_buffer.deinit(allocator);
    var sexpr_writer: std.Io.Writer.Allocating = .fromArrayList(allocator, &sexpr_buffer);

    try package_docs.writeToSExpr(&sexpr_writer.writer);

    sexpr_buffer = sexpr_writer.toArrayList();
    const new_docs_text = sexpr_buffer.items;

    // 6. Compare against existing DOCS section and decide what to write
    var success = true;
    const write_new_docs = blk: {
        if (content.docs_output == null) {
            // First run - always write new docs
            break :blk true;
        }
        switch (config.expected_section_command) {
            .update => break :blk true,
            .check => {
                const existing_trimmed = std.mem.trimEnd(u8, content.docs_output.?, " \t\r\n");
                const new_trimmed = std.mem.trimEnd(u8, new_docs_text, " \t\r\n");
                if (!std.mem.eql(u8, existing_trimmed, new_trimmed)) {
                    std.debug.print("\nDOCS mismatch in {s}\n\n", .{output_path});
                    std.debug.print("Expected:\n{s}\n\nActual:\n{s}\n", .{ existing_trimmed, new_trimmed });
                    std.debug.print("\nHint: use `zig build run-snapshot-tool -- --update-expected` to update DOCS output.\n\n", .{});
                    success = false;
                }
                break :blk false;
            },
            .none => {
                const existing_trimmed = std.mem.trimEnd(u8, content.docs_output.?, " \t\r\n");
                const new_trimmed = std.mem.trimEnd(u8, new_docs_text, " \t\r\n");
                if (!std.mem.eql(u8, existing_trimmed, new_trimmed)) {
                    std.debug.print("\nDOCS warning: output changed in {s}\n", .{output_path});
                    std.debug.print("Hint: use `zig build run-snapshot-tool -- --check-expected` to see details, or `--update-expected` to update.\n\n", .{});
                }
                break :blk false;
            },
        }
    };

    // 7. Generate output file
    var md_buffer = std.ArrayList(u8).empty;
    defer md_buffer.deinit(allocator);
    var md_writer: std.Io.Writer.Allocating = .fromArrayList(allocator, &md_buffer);

    // META section
    try md_writer.writer.writeAll(Section.META);
    try content.meta.format(&md_writer.writer);
    try md_writer.writer.writeAll("\n" ++ Section.SECTION_END);

    // SOURCE section (preserve original multi-file format)
    try md_writer.writer.writeAll(Section.SOURCE_MULTI);
    try md_writer.writer.writeAll(content.source);
    // Ensure trailing newline before next section
    if (content.source.len > 0 and content.source[content.source.len - 1] != '\n') {
        try md_writer.writer.writeByte('\n');
    }

    // DOCS section
    try md_writer.writer.writeAll(Section.DOCS);
    if (write_new_docs) {
        try md_writer.writer.writeAll(new_docs_text);
    } else {
        // Preserve existing DOCS content
        try md_writer.writer.writeAll(content.docs_output.?);
        // Ensure trailing newline
        if (content.docs_output.?.len > 0 and content.docs_output.?[content.docs_output.?.len - 1] != '\n') {
            try md_writer.writer.writeByte('\n');
        }
    }
    try md_writer.writer.writeAll(Section.SECTION_END);

    // Transfer from writer to buffer
    md_buffer = md_writer.toArrayList();

    // Write the output file
    const md_file = std.Io.Dir.cwd().createFile(app_io, output_path, .{}) catch |err| {
        std.log.err("Failed to create {s}: {}", .{ output_path, err });
        return false;
    };
    defer md_file.close(app_io);

    try md_file.writeStreamingAll(app_io, md_buffer.items);
    return success;
}

// Dev Object Snapshot Processing

/// Represents a single source file extracted from a multi-file SOURCE section
const SourceFile = struct {
    filename: []const u8,
    content: []const u8,
};

/// Parse multi-file SOURCE section with ## filename.roc sub-headings.
///
/// Format:
///   ## app.roc
///   ~~~roc
///   ...code...
///   ~~~
///   ## platform.roc
///   ~~~roc
///   ...code...
///   ~~~
fn parseMultiFileSource(allocator: Allocator, source_text: []const u8) Allocator.Error![]SourceFile {
    var files = std.ArrayList(SourceFile).empty;
    errdefer files.deinit(allocator);

    var idx: usize = 0;
    while (idx < source_text.len) {
        // Look for "## " at line start
        const is_line_start = idx == 0 or source_text[idx - 1] == '\n';
        if (is_line_start and idx + 3 < source_text.len and
            source_text[idx] == '#' and source_text[idx + 1] == '#' and source_text[idx + 2] == ' ')
        {
            // Extract filename (rest of line)
            const name_start = idx + 3;
            const name_end = std.mem.findScalarPos(u8, source_text, name_start, '\n') orelse source_text.len;
            const filename = std.mem.trim(u8, source_text[name_start..name_end], " \t\r");
            idx = name_end;

            // Find ~~~roc block
            const roc_marker = "~~~roc\n";
            if (std.mem.findPos(u8, source_text, idx, roc_marker)) |roc_start| {
                const content_start = roc_start + roc_marker.len;
                // Find closing ~~~
                if (std.mem.findPos(u8, source_text, content_start, "~~~")) |content_end| {
                    try files.append(allocator, .{
                        .filename = filename,
                        .content = source_text[content_start..content_end],
                    });
                    idx = content_end + 3;
                    continue;
                }
            }
        }
        idx += 1;
    }

    return files.toOwnedSlice(allocator);
}

/// Result of cross-compiling for a single target
const TargetHashResult = struct {
    target_name: []const u8,
    hash_hex: [64]u8, // Blake3 produces 256-bit (32 byte) hash = 64 hex chars
    supported: bool,
};

/// Compare two hash text blocks (target=hash lines) for equality,
/// ignoring trailing whitespace differences.
fn hashTextMatches(existing: []const u8, new: []const u8) bool {
    const existing_trimmed = std.mem.trimEnd(u8, existing, " \t\r\n");
    const new_trimmed = std.mem.trimEnd(u8, new, " \t\r\n");
    return std.mem.eql(u8, existing_trimmed, new_trimmed);
}

/// Print a table showing which targets have hash mismatches.
fn printHashMismatchTable(existing: []const u8, new: []const u8) void {
    std.debug.print("  {s:<18} | {s:<64} | {s}\n", .{ "target", "expected", "actual" });
    std.debug.print("  {s:-<18} | {s:-<64} | {s:-<64}\n", .{ "", "", "" });

    // Parse existing hashes into a map-like iteration
    var new_lines = std.mem.splitScalar(u8, std.mem.trimEnd(u8, new, " \t\r\n"), '\n');
    var existing_lines = std.mem.splitScalar(u8, std.mem.trimEnd(u8, existing, " \t\r\n"), '\n');

    while (new_lines.next()) |new_line| {
        const new_eq = std.mem.findScalar(u8, new_line, '=') orelse continue;
        const target = new_line[0..new_eq];
        const new_hash = new_line[new_eq + 1 ..];

        // Find matching target in existing
        existing_lines.reset();
        var old_hash: ?[]const u8 = null;
        while (existing_lines.next()) |existing_line| {
            const ex_eq = std.mem.findScalar(u8, existing_line, '=') orelse continue;
            if (std.mem.eql(u8, existing_line[0..ex_eq], target)) {
                old_hash = existing_line[ex_eq + 1 ..];
                break;
            }
        }

        if (old_hash) |oh| {
            if (std.mem.eql(u8, oh, new_hash)) {
                std.debug.print("  {s:<18} | (matches)\n", .{target});
            } else {
                std.debug.print("  {s:<18} | {s:<64} | {s}\n", .{ target, oh, new_hash });
            }
        } else {
            std.debug.print("  {s:<18} | (new)                                                            | {s}\n", .{ target, new_hash });
        }
    }
}

fn snapshotRootRequestByOrder(
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    order: u32,
) check.CheckedArtifact.RootRequest {
    for (root_artifact.root_requests.requests) |request| {
        if (request.order == order) return request;
    }
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("snapshot invariant violated: missing root request order {d}", .{order});
    }
    unreachable;
}

fn snapshotProvidedEntrypointName(
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    root: check.CheckedArtifact.RootRequest,
) []const u8 {
    const def_idx = switch (root.source) {
        .def => |def| def,
        else => {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("snapshot invariant violated: exported platform root is not a definition", .{});
            }
            unreachable;
        },
    };
    const top_level = root_artifact.top_level_values.lookupByDef(def_idx) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("snapshot invariant violated: exported platform root has no published top-level value", .{});
        }
        unreachable;
    };

    for (root_artifact.provides_requires.provides) |entry| {
        if (entry.source_name == top_level.source_name) {
            return root_artifact.canonical_names.externalSymbolNameText(entry.ffi_symbol);
        }
    }

    if (@import("builtin").mode == .Debug) {
        std.debug.panic(
            "snapshot invariant violated: exported platform root has no published FFI symbol",
            .{},
        );
    }
    unreachable;
}

fn snapshotNativeEntrypoints(
    allocator: Allocator,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) Allocator.Error![]backend.Entrypoint {
    const root_procs = lowered.lir_result.root_procs.items;
    const root_metadata = lowered.lir_result.root_metadata.items;
    if (root_procs.len != root_metadata.len) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "snapshot invariant violated: root metadata mismatch roots={d} metadata={d}",
                .{ root_procs.len, root_metadata.len },
            );
        }
        unreachable;
    }

    var entrypoints = std.ArrayList(backend.Entrypoint).empty;
    errdefer {
        for (entrypoints.items) |entrypoint| {
            allocator.free(entrypoint.symbol_name);
            allocator.free(entrypoint.arg_layouts);
        }
        entrypoints.deinit(allocator);
    }

    for (root_procs, root_metadata) |root_proc, metadata| {
        if (metadata.abi != .platform or metadata.exposure != .exported) continue;
        const root = snapshotRootRequestByOrder(root_artifact, metadata.order);
        if (root.kind != .provided_export) continue;

        const proc_spec = lowered.lir_result.store.getProcSpec(root_proc);
        const arg_locals = lowered.lir_result.store.getLocalSpan(proc_spec.args);
        const arg_layouts = try allocator.alloc(layout.Idx, arg_locals.len);
        var arg_layouts_owned = true;
        errdefer if (arg_layouts_owned) allocator.free(arg_layouts);

        for (arg_locals, 0..) |local_id, i| {
            arg_layouts[i] = lowered.lir_result.store.getLocal(local_id).layout_idx;
        }

        const entrypoint_name = snapshotProvidedEntrypointName(root_artifact, root);
        const symbol_name = try allocator.dupe(u8, entrypoint_name);
        var symbol_name_owned = true;
        errdefer if (symbol_name_owned) allocator.free(symbol_name);

        try entrypoints.append(allocator, .{
            .symbol_name = symbol_name,
            .proc = root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = proc_spec.ret_layout,
        });
        arg_layouts_owned = false;
        symbol_name_owned = false;
    }

    return try entrypoints.toOwnedSlice(allocator);
}

fn snapshotHasProvidedProcedureExports(
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) bool {
    for (root_artifact.provided_exports.exports) |provided| {
        switch (provided) {
            .procedure => return true,
            .data => {},
        }
    }
    return false;
}

fn snapshotHasProvidedDataExports(
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) bool {
    for (root_artifact.provided_exports.exports) |provided| {
        switch (provided) {
            .data => return true,
            .procedure => {},
        }
    }
    return false;
}

/// Process a dev_object snapshot: parse multi-file source, compile with BuildEnv,
/// lower through checked artifacts to LIR, cross-compile for all targets, and
/// record blake3 hashes.
fn processDevObjectSnapshot(
    allocator: Allocator,
    content: Content,
    output_path: []const u8,
    config: *const Config,
) SnapshotError!bool {
    log("Processing dev_object snapshot: {s}", .{output_path});

    const source_files = try parseMultiFileSource(allocator, content.source);
    defer allocator.free(source_files);

    if (source_files.len == 0) {
        std.log.err("dev_object snapshot has no source files (need ## filename.roc sub-headings)", .{});
        return false;
    }

    const tmp_dir_name = makeSnapshotTempDir(allocator, "roc_snapshot_dev", output_path) catch |err| {
        std.log.err("Failed to resolve temp directory for {s}: {}", .{ output_path, err });
        return false;
    };
    defer allocator.free(tmp_dir_name);

    std.Io.Dir.cwd().createDirPath(app_io, tmp_dir_name) catch |err| {
        std.log.err("Failed to create temp directory {s}: {}", .{ tmp_dir_name, err });
        return false;
    };
    defer std.Io.Dir.cwd().deleteTree(app_io, tmp_dir_name) catch {};

    var app_filename: ?[]const u8 = null;
    for (source_files) |sf| {
        const sub_path = try std.fs.path.join(allocator, &.{ tmp_dir_name, sf.filename });
        defer allocator.free(sub_path);
        std.Io.Dir.cwd().writeFile(app_io, .{
            .sub_path = sub_path,
            .data = sf.content,
        }) catch |err| {
            std.log.err("Failed to write {s}: {}", .{ sf.filename, err });
            return false;
        };
        if (std.mem.eql(u8, sf.filename, "app.roc") or app_filename == null) {
            app_filename = sf.filename;
        }
    }

    const app_path = try std.fs.path.join(allocator, &.{ tmp_dir_name, app_filename.? });
    defer allocator.free(app_path);

    const BuildEnv = compile.BuildEnv;
    const native_target = roc_target.RocTarget.detectNative();

    var build_env = BuildEnv.init(allocator, .single_threaded, 1, native_target, config.cwd, app_io) catch |err| {
        std.log.err("Failed to init BuildEnv: {}", .{err});
        return false;
    };
    defer build_env.deinit();

    build_env.build(app_path) catch |err| {
        std.log.err("BuildEnv.build failed for {s}: {}", .{ app_path, err });
        return false;
    };

    const modules = build_env.getModulesInSerializationOrder(allocator) catch |err| {
        std.log.err("Failed to get compiled modules: {}", .{err});
        return false;
    };
    defer allocator.free(modules);

    if (modules.len == 0) {
        std.log.err("No modules were compiled", .{});
        return false;
    }

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(allocator, root_artifact);
    defer allocator.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(allocator, root_artifact);
    defer allocator.free(relation_artifacts);

    const has_procedure_exports = snapshotHasProvidedProcedureExports(root_artifact);
    const has_data_exports = snapshotHasProvidedDataExports(root_artifact);
    if (!has_procedure_exports and !has_data_exports) {
        std.log.err("Failed to produce any exported platform entrypoints or data symbols", .{});
        return false;
    }

    const RocTarget = roc_target.RocTarget;
    const Blake3 = std.crypto.hash.Blake3;
    const roc_target_fields = @typeInfo(RocTarget).@"enum".fields;

    var hash_results: [roc_target_fields.len]TargetHashResult = undefined;
    var object_compiler = backend.ObjectFileCompiler.init(allocator);

    inline for (roc_target_fields, 0..) |field, i| {
        const target: RocTarget = @enumFromInt(field.value);
        hash_results[i].target_name = field.name;

        target_snapshot: {
            const arch = target.toCpuArch();
            if (arch != .x86_64 and arch != .aarch64 and arch != .aarch64_be) {
                hash_results[i].hash_hex = undefined;
                hash_results[i].supported = false;
                break :target_snapshot;
            }

            const target_usize: base.target.TargetUsize = switch (target.ptrBitWidth()) {
                32 => .u32,
                64 => .u64,
                else => {
                    hash_results[i].hash_hex = undefined;
                    hash_results[i].supported = false;
                    break :target_snapshot;
                },
            };
            const build_roots = try lir.CheckedPipeline.selectPlatformExportRoots(allocator, root_artifact.root_requests.runtime_requests);
            defer allocator.free(build_roots);

            var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
                allocator,
                .{
                    .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
                    .imports = imported_artifacts,
                },
                .{ .requests = build_roots, .include_static_data_exports = true },
                .{
                    .target_usize = target_usize,
                },
            );
            defer lowered.deinit();

            const entrypoints = try snapshotNativeEntrypoints(allocator, root_artifact, &lowered);
            defer {
                for (entrypoints) |entrypoint| {
                    allocator.free(entrypoint.symbol_name);
                    allocator.free(entrypoint.arg_layouts);
                }
                allocator.free(entrypoints);
            }
            const static_data_exports = compile.static_data_exports.buildProvidedDataExports(
                allocator,
                .{
                    .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
                    .imports = imported_artifacts,
                },
                &lowered,
                target,
            ) catch |err| {
                std.log.err("Failed to materialize static data exports for {s}: {}", .{ field.name, err });
                hash_results[i].hash_hex = undefined;
                hash_results[i].supported = false;
                break :target_snapshot;
            };
            defer compile.static_data_exports.deinitProvidedDataExports(allocator, static_data_exports);

            if (object_compiler.compileToObjectFile(
                &lowered.lir_result.store,
                &lowered.lir_result.layouts,
                entrypoints,
                static_data_exports,
                lowered.lir_result.store.getProcSpecs(),
                target,
            )) |result| {
                var hasher = Blake3.init(.{});
                hasher.update(result.object_bytes);
                var hash: [32]u8 = undefined;
                hasher.final(&hash);
                hash_results[i].hash_hex = std.fmt.bytesToHex(hash, .lower);
                hash_results[i].supported = true;
                result.allocator.free(result.object_bytes);
            } else |_| {
                hash_results[i].hash_hex = undefined;
                hash_results[i].supported = false;
            }
        }
    }

    var md_buffer = std.ArrayList(u8).empty;
    defer md_buffer.deinit(allocator);
    var md_writer: std.Io.Writer.Allocating = .fromArrayList(allocator, &md_buffer);

    try md_writer.writer.writeAll(Section.META);
    try content.meta.format(&md_writer.writer);
    try md_writer.writer.writeAll("\n" ++ Section.SECTION_END);

    try md_writer.writer.writeAll(Section.SOURCE_MULTI);
    try md_writer.writer.writeAll(content.source);
    if (content.source.len > 0 and content.source[content.source.len - 1] != '\n') {
        try md_writer.writer.writeByte('\n');
    }

    try md_writer.writer.writeAll(Section.MONO);
    for (modules) |mod| {
        const mod_env = mod.semantic.env;
        const mod_name = base.module_path.getModuleName(mod_env.module_name);

        var emitter = can.RocEmitter.init(allocator, mod_env);
        defer emitter.deinit();

        const defs = mod_env.store.sliceDefs(mod_env.all_defs);
        if (defs.len == 0) continue;

        try md_writer.writer.writeAll("# ");
        try md_writer.writer.writeAll(mod_name);
        try md_writer.writer.writeByte('\n');

        for (defs) |def_idx| {
            const def = mod_env.store.getDef(def_idx);

            emitter.reset();
            try emitter.emitPattern(def.pattern);
            const pattern_str = try allocator.dupe(u8, emitter.getOutput());
            defer allocator.free(pattern_str);

            emitter.reset();
            try emitter.emitExpr(def.expr);

            try md_writer.writer.writeAll(pattern_str);
            try md_writer.writer.writeAll(" = ");
            try md_writer.writer.writeAll(emitter.getOutput());
            try md_writer.writer.writeByte('\n');
        }
        try md_writer.writer.writeByte('\n');
    }
    try md_writer.writer.writeAll(Section.SECTION_END);

    var new_hash_buf = std.ArrayList(u8).empty;
    defer new_hash_buf.deinit(allocator);
    for (&hash_results) |result| {
        try new_hash_buf.appendSlice(allocator, result.target_name);
        try new_hash_buf.append(allocator, '=');
        if (result.supported) {
            try new_hash_buf.appendSlice(allocator, &result.hash_hex);
        } else {
            try new_hash_buf.appendSlice(allocator, "NOT_IMPLEMENTED");
        }
        try new_hash_buf.append(allocator, '\n');
    }
    const new_hash_text = new_hash_buf.items;

    var success = true;
    const write_new_hashes = blk: {
        if (content.dev_output == null) break :blk true;
        switch (config.expected_section_command) {
            .update => break :blk true,
            .check => {
                if (!hashTextMatches(content.dev_output.?, new_hash_text)) {
                    std.debug.print("\nDEV OUTPUT mismatch in {s}\n\n", .{output_path});
                    printHashMismatchTable(content.dev_output.?, new_hash_text);
                    std.debug.print("\nHint: use `zig build run-snapshot-tool -- --update-expected` to update DEV OUTPUT hashes.\n\n", .{});
                    success = false;
                }
                break :blk false;
            },
            .none => {
                if (!hashTextMatches(content.dev_output.?, new_hash_text)) {
                    std.debug.print("\nDEV OUTPUT warning: hashes changed in {s}\n", .{output_path});
                    std.debug.print("Hint: use `zig build run-snapshot-tool -- --check-expected` to see details, or `--update-expected` to update.\n\n", .{});
                }
                break :blk false;
            },
        }
    };

    try md_writer.writer.writeAll(Section.DEV_OUTPUT);
    if (write_new_hashes) {
        try md_writer.writer.writeAll(new_hash_text);
    } else {
        try md_writer.writer.writeAll(content.dev_output.?);
        if (content.dev_output.?.len > 0 and content.dev_output.?[content.dev_output.?.len - 1] != '\n') {
            try md_writer.writer.writeByte('\n');
        }
    }
    try md_writer.writer.writeAll(Section.SECTION_END);

    md_buffer = md_writer.toArrayList();

    // Write the output file
    const md_file = std.Io.Dir.cwd().createFile(app_io, output_path, .{}) catch |err| {
        std.log.err("Failed to create {s}: {}", .{ output_path, err });
        return false;
    };
    defer md_file.close(app_io);

    try md_file.writeStreamingAll(app_io, md_buffer.items);
    return success;
}

// REPL Snapshot Processing

const SnapshotReplDefinitionKind = enum {
    value,
    type_annotation,
    type_declaration,
    import,
    file_import,
};

const SnapshotReplDefinition = struct {
    kind: SnapshotReplDefinitionKind,
    name: []u8,
    source: []u8,

    fn deinit(self: *SnapshotReplDefinition, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.source);
    }
};

const SnapshotReplSession = struct {
    definitions: std.ArrayList(SnapshotReplDefinition) = .empty,

    fn deinit(self: *SnapshotReplSession, allocator: Allocator) void {
        for (self.definitions.items) |*definition| {
            definition.deinit(allocator);
        }
        self.definitions.deinit(allocator);
    }

    fn hasDefinition(self: *const SnapshotReplSession, kind: SnapshotReplDefinitionKind, name: []const u8) bool {
        for (self.definitions.items) |definition| {
            if (definition.kind == kind and std.mem.eql(u8, definition.name, name)) return true;
        }
        return false;
    }

    fn upsertDefinition(
        self: *SnapshotReplSession,
        allocator: Allocator,
        kind: SnapshotReplDefinitionKind,
        name: []const u8,
        source: []const u8,
    ) Allocator.Error!void {
        const owned_name = try allocator.dupe(u8, name);
        errdefer allocator.free(owned_name);
        const owned_source = try allocator.dupe(u8, source);
        errdefer allocator.free(owned_source);

        for (self.definitions.items) |*definition| {
            if (definition.kind == kind and std.mem.eql(u8, definition.name, name)) {
                definition.deinit(allocator);
                definition.* = .{
                    .kind = kind,
                    .name = owned_name,
                    .source = owned_source,
                };
                return;
            }
        }

        try self.definitions.append(allocator, .{
            .kind = kind,
            .name = owned_name,
            .source = owned_source,
        });
    }
};

const SnapshotReplInputKind = enum {
    definition,
    expression,
    statement_expression,
};

const SnapshotReplDefinitionIdentity = struct {
    kind: SnapshotReplDefinitionKind,
    name: []const u8,
};

const SnapshotReplParsedLine = struct {
    allocator: Allocator,
    // Heap-allocated so its address stays stable: `ast.env` is a `*CommonEnv`
    // that points into this `ModuleEnv`, and would dangle if the struct were
    // copied by value out of a stack frame.
    module_env: *ModuleEnv,
    ast: *AST,
    statement: AST.Statement.Idx,

    fn deinit(self: *@This()) void {
        self.ast.deinit();
        self.module_env.deinit();
        self.allocator.destroy(self.module_env);
    }
};

fn parseSnapshotReplLineAsFile(allocator: Allocator, line: []const u8) Allocator.Error!?SnapshotReplParsedLine {
    const module_env = try allocator.create(ModuleEnv);
    errdefer allocator.destroy(module_env);
    module_env.* = try ModuleEnv.init(allocator, line);
    errdefer module_env.deinit();
    module_env.common.source = line;

    const ast = try single_module.parseSingleModule(
        allocator,
        module_env,
        .file,
        .{ .module_name = "repl" },
    );
    errdefer ast.deinit();
    if (ast.hasErrors()) {
        ast.deinit();
        module_env.deinit();
        allocator.destroy(module_env);
        return null;
    }

    const file = ast.store.getFile();
    const statements = ast.store.statementSlice(file.statements);
    if (statements.len != 1) {
        ast.deinit();
        module_env.deinit();
        allocator.destroy(module_env);
        return null;
    }

    return .{
        .allocator = allocator,
        .module_env = module_env,
        .ast = ast,
        .statement = statements[0],
    };
}

fn parseSnapshotReplLineAsStatement(allocator: Allocator, line: []const u8) Allocator.Error!?AST.Statement {
    var env = try ModuleEnv.init(allocator, line);
    defer env.deinit();
    env.common.source = line;
    try env.common.calcLineStarts(allocator);

    const ast = try parse.statement(allocator, &env.common);
    defer ast.deinit();
    if (ast.hasErrors()) return null;

    return ast.store.getStatement(@enumFromInt(ast.root_node_idx));
}

fn resolveSnapshotReplInputKind(allocator: Allocator, line: []const u8) Allocator.Error!?SnapshotReplInputKind {
    var maybe_file_parse = try parseSnapshotReplLineAsFile(allocator, line);
    const statement = if (maybe_file_parse) |*parsed| blk: {
        defer parsed.deinit();
        const file_statement = parsed.ast.store.getStatement(parsed.statement);
        switch (file_statement) {
            .decl,
            .@"var",
            .import,
            .file_import,
            .type_decl,
            .type_anno,
            => break :blk file_statement,
            else => {},
        }
        break :blk (try parseSnapshotReplLineAsStatement(allocator, line)) orelse file_statement;
    } else (try parseSnapshotReplLineAsStatement(allocator, line)) orelse return null;

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
        => .statement_expression,
    };
}

fn snapshotReplDefinitionIdentity(allocator: Allocator, line: []const u8) Allocator.Error!?SnapshotReplDefinitionIdentity {
    var parsed = (try parseSnapshotReplLineAsFile(allocator, line)) orelse return null;
    defer parsed.deinit();

    const ast = parsed.ast;
    const statement = ast.store.getStatement(parsed.statement);
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

fn writeSnapshotReplDefinitionsWithReplacement(
    writer: *std.Io.Writer,
    session: *const SnapshotReplSession,
    replacement: ?SnapshotReplDefinitionIdentity,
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

fn buildSnapshotReplModuleSource(
    allocator: Allocator,
    session: *const SnapshotReplSession,
    replacement: ?SnapshotReplDefinitionIdentity,
    replacement_source: ?[]const u8,
) (Allocator.Error || error{WriteFailed})![]u8 {
    var source_writer: std.Io.Writer.Allocating = .init(allocator);
    errdefer source_writer.deinit();

    try writeSnapshotReplDefinitionsWithReplacement(&source_writer.writer, session, replacement, replacement_source);
    try source_writer.writer.flush();

    return source_writer.toOwnedSlice();
}

fn compileSnapshotReplInspectedModule(
    allocator: Allocator,
    source: []const u8,
    config: *const Config,
) SnapshotError!eval_mod.test_helpers.CompiledProgram {
    if (config.builtin_modules_ref) |bm| {
        return eval_mod.test_helpers.compileInspectedProgramWithBuiltin(
            allocator,
            app_io,
            .module,
            source,
            &.{},
            .{
                .env = bm.builtin_module.env,
                .indices = bm.builtin_indices,
                .artifact = &bm.checked_artifact,
            },
        );
    }
    return eval_mod.test_helpers.compileInspectedProgram(allocator, app_io, .module, source, &.{});
}

fn compileSnapshotReplInspectedExpr(allocator: Allocator, source: []const u8) SnapshotError!eval_mod.test_helpers.CompiledProgram {
    return eval_mod.test_helpers.compileInspectedProgram(allocator, app_io, .expr, source, &.{});
}

fn renderSnapshotReplTypeProblems(
    allocator: Allocator,
    source_kind: eval_mod.test_helpers.SourceKind,
    source: []const u8,
    config: *const Config,
) SnapshotError![]const u8 {
    const builtin_env = config.builtin_module orelse return error.MissingBuiltinModule;

    var module_env = try single_module.ModuleEnv.init(allocator, source);
    defer module_env.deinit();
    var can_ir = &module_env;

    const parse_mode: single_module.ParseMode = switch (source_kind) {
        // REPL expression lines are identified through the statement parser once
        // they are known not to be definitions. The diagnostic renderer must use
        // that same shape instead of reparsing through expression-only or file
        // mode, both of which accept different syntax at their roots.
        .expr => .statement,
        .module => .file,
    };
    const parse_ast = try single_module.parseSingleModule(
        allocator,
        can_ir,
        parse_mode,
        .{ .module_name = "repl" },
    );
    defer parse_ast.deinit();

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try can_ir.insertIdent(base.Ident.for_text("repl")),
        .bool_stmt = config.builtin_indices.bool_type,
        .try_stmt = config.builtin_indices.try_type,
        .str_stmt = config.builtin_indices.str_type,
        .builtin_module = config.builtin_module,
        .builtin_indices = config.builtin_indices,
    };

    const roc_ctx_repl = CoreCtx.default(allocator, allocator, app_io);
    var czer = try Can.initModule(roc_ctx_repl, can_ir, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_env,
            .builtin_indices = config.builtin_indices,
        },
    });
    defer czer.deinit();

    const repl_expr = switch (source_kind) {
        .expr => blk: {
            const statement_idx: AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
            const statement = parse_ast.store.getStatement(statement_idx);
            const expr_idx = switch (statement) {
                .expr => |expr_stmt| expr_stmt.expr,
                else => break :blk null,
            };
            break :blk try czer.canonicalizeExpr(expr_idx);
        },
        .module => blk: {
            try czer.canonicalizeFile();
            break :blk null;
        },
    };
    if (source_kind == .expr and can_ir.store.scratch != null) {
        can_ir.diagnostics = try can_ir.store.diagnosticSpanFrom(0);
    }

    var imported_envs = std.array_list.Managed(*const ModuleEnv).init(allocator);
    defer imported_envs.deinit();
    for (can_ir.imports.imports.items.items) |str_idx| {
        const import_name = can_ir.getString(str_idx);
        if (CIR.Import.isCompilerBuiltinImportName(import_name)) {
            try imported_envs.append(builtin_env);
        }
    }
    if (imported_envs.items.len == 0) {
        try imported_envs.append(builtin_env);
    }

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();
    try Can.populateModuleEnvs(&module_envs, can_ir, builtin_env, config.builtin_indices);

    can_ir.imports.clearResolvedModules();
    try can_ir.imports.resolveImportsByExactModuleName(can_ir, imported_envs.items);
    can_ir.imports.markUnresolvedImportsFailedBeforeChecking();

    var checker = try Check.init(
        allocator,
        &can_ir.types,
        can_ir,
        imported_envs.items,
        &module_envs,
        &can_ir.store.regions,
        builtin_ctx,
    );
    checker.fixupTypeWriter();
    defer checker.deinit();

    const check_result: SnapshotError!void = switch (source_kind) {
        .expr => if (repl_expr) |expr| checker.checkExprRepl(expr.idx) else {},
        .module => checker.checkFile(),
    };
    check_result catch |err| switch (err) {
        error.TypeCheckError => {},
        else => return err,
    };

    var reports = try generateAllReports(allocator, parse_ast, can_ir, &checker, "repl", can_ir);
    defer {
        for (reports.items) |*report| {
            report.deinit();
        }
        reports.deinit();
    }
    if (reports.items.len == 0) {
        const diagnostics = try can_ir.getDiagnostics();
        defer allocator.free(diagnostics);
        std.debug.print(
            "REPL diagnostic rendering invariant violated: TypeCheckError produced no reports. source_kind={s} tokenize={d} parse={d} canonicalize={d} check={d}\nsource:\n{s}\n",
            .{
                switch (source_kind) {
                    .expr => "expr",
                    .module => "module",
                },
                parse_ast.tokenize_diagnostics.items.len,
                parse_ast.parse_diagnostics.items.len,
                diagnostics.len,
                checker.problems.problems.items.len,
                source,
            },
        );
        return error.TypeCheckError;
    }

    var rendered: std.Io.Writer.Allocating = .init(allocator);
    errdefer rendered.deinit();
    for (reports.items) |report| {
        try report.render(&rendered.writer, .markdown);
    }
    const raw = try rendered.toOwnedSlice();
    const trimmed = std.mem.trimEnd(u8, raw, "\r\n");
    if (trimmed.len == raw.len) return raw;
    const out = try allocator.dupe(u8, trimmed);
    allocator.free(raw);
    return out;
}

fn snapshotReplDefinitionStep(
    allocator: Allocator,
    session: *SnapshotReplSession,
    input: []const u8,
    config: *const Config,
) SnapshotError![]const u8 {
    const maybe_identity = try snapshotReplDefinitionIdentity(allocator, input);
    const identity = maybe_identity orelse
        return try allocator.dupe(u8, "Parse error: REPL definitions must bind a top-level identifier");
    if (identity.kind == .type_annotation) {
        return try allocator.dupe(u8, "Parse error: Type annotations are not supported in the REPL yet");
    }

    const defines_main = identity.kind == .value and std.mem.eql(u8, identity.name, "main");
    const validation_main_source = if (defines_main or session.hasDefinition(.value, "main"))
        null
    else
        "main = \"\"";

    const validation_base = try buildSnapshotReplModuleSource(
        allocator,
        session,
        identity,
        input,
    );
    defer allocator.free(validation_base);

    const validation_with_main = if (validation_main_source) |main_source| blk: {
        var source_writer: std.Io.Writer.Allocating = .init(allocator);
        errdefer source_writer.deinit();
        try source_writer.writer.writeAll(validation_base);
        try source_writer.writer.writeAll(main_source);
        try source_writer.writer.writeAll("\n");
        try source_writer.writer.flush();
        break :blk try source_writer.toOwnedSlice();
    } else validation_base;
    defer if (validation_main_source != null) allocator.free(validation_with_main);

    var compiled = compileSnapshotReplInspectedModule(allocator, validation_with_main, config) catch |err| {
        return switch (err) {
            error.TypeCheckError => renderSnapshotReplTypeProblems(allocator, .module, validation_with_main, config),
            else => try std.fmt.allocPrint(allocator, "{s}", .{@errorName(err)}),
        };
    };
    compiled.deinit(allocator);

    try session.upsertDefinition(allocator, identity.kind, identity.name, input);
    return try std.fmt.allocPrint(allocator, "assigned `{s}`", .{identity.name});
}

fn compileAndEvaluateSnapshotReplExpr(
    allocator: Allocator,
    input: []const u8,
    config: *const Config,
) SnapshotError![]const u8 {
    var expr_compiled = compileSnapshotReplInspectedExpr(allocator, input) catch |expr_err| {
        return switch (expr_err) {
            error.TypeCheckError => renderSnapshotReplTypeProblems(allocator, .expr, input, config),
            else => try std.fmt.allocPrint(allocator, "{s}", .{@errorName(expr_err)}),
        };
    };
    defer expr_compiled.deinit(allocator);

    return eval_mod.test_helpers.lirInterpreterInspectedStr(allocator, &expr_compiled.lowered) catch |eval_err| {
        return try std.fmt.allocPrint(allocator, "{s}", .{@errorName(eval_err)});
    };
}

fn snapshotReplExpressionStep(
    allocator: Allocator,
    session: *SnapshotReplSession,
    input: []const u8,
    config: *const Config,
    statement_body: bool,
) SnapshotError![]const u8 {
    const main_source = if (statement_body)
        try std.fmt.allocPrint(allocator, "main = {{\n    {s}\n}}", .{input})
    else
        try std.fmt.allocPrint(allocator, "main = {s}", .{input});
    defer allocator.free(main_source);

    const source = try buildSnapshotReplModuleSource(
        allocator,
        session,
        .{ .kind = .value, .name = "main" },
        main_source,
    );
    defer allocator.free(source);

    const use_expr_fallback = !statement_body and session.definitions.items.len == 0;
    var compiled = compileSnapshotReplInspectedModule(allocator, source, config) catch |err| {
        switch (err) {
            error.TypeCheckError => {
                const module_problems = renderSnapshotReplTypeProblems(allocator, .module, source, config) catch |render_err| {
                    if (use_expr_fallback) {
                        switch (render_err) {
                            error.TypeCheckError => return compileAndEvaluateSnapshotReplExpr(allocator, input, config),
                            else => {},
                        }
                    }
                    return render_err;
                };
                errdefer allocator.free(module_problems);

                if (use_expr_fallback) {
                    // These titles are matched against markdown output, which
                    // preserves the authored title case (the box/snapshot output
                    // shouts them to ALL CAPS, but this is the markdown render).
                    const is_top_level_wrapper_problem =
                        std.mem.find(u8, module_problems, "Effectful Top Level Value") != null or
                        std.mem.find(u8, module_problems, "Polymorphic Value") != null;
                    if (is_top_level_wrapper_problem) {
                        allocator.free(module_problems);
                        return compileAndEvaluateSnapshotReplExpr(allocator, input, config);
                    }

                    allocator.free(module_problems);
                    return renderSnapshotReplTypeProblems(allocator, .expr, input, config);
                }

                return module_problems;
            },
            else => return try std.fmt.allocPrint(allocator, "{s}", .{@errorName(err)}),
        }
    };
    defer compiled.deinit(allocator);

    return eval_mod.test_helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered) catch |err| {
        return try std.fmt.allocPrint(allocator, "{s}", .{@errorName(err)});
    };
}

fn snapshotReplStep(
    allocator: Allocator,
    session: *SnapshotReplSession,
    input: []const u8,
    config: *const Config,
) SnapshotError![]const u8 {
    const trimmed = std.mem.trim(u8, input, " \t\r\n");
    if (trimmed.len == 0) return try allocator.dupe(u8, "Parse error: UNEXPECTED TOKEN");

    const maybe_input_kind = try resolveSnapshotReplInputKind(allocator, trimmed);
    const input_kind = maybe_input_kind orelse
        return try allocator.dupe(u8, "Parse error: UNEXPECTED TOKEN");

    return switch (input_kind) {
        .definition => snapshotReplDefinitionStep(allocator, session, trimmed, config),
        .expression => snapshotReplExpressionStep(allocator, session, trimmed, config, false),
        .statement_expression => snapshotReplExpressionStep(allocator, session, trimmed, config, true),
    };
}

fn processReplSnapshot(allocator: Allocator, content: Content, output_path: []const u8, config: *const Config) SnapshotError!bool {
    if (gpa_poisoned) return false;

    var success = true;
    log("Processing REPL snapshot: {s}", .{output_path});

    var md_buffer_unmanaged = std.ArrayList(u8).empty;
    var md_writer_allocating: std.Io.Writer.Allocating = .fromArrayList(allocator, &md_buffer_unmanaged);
    defer if (!gpa_poisoned) md_buffer_unmanaged.deinit(allocator);

    var html_buffer_unmanaged: ?std.ArrayList(u8) = if (config.generate_html) std.ArrayList(u8).empty else null;
    var html_writer_allocating: ?std.Io.Writer.Allocating = if (config.generate_html) .fromArrayList(allocator, &html_buffer_unmanaged.?) else null;
    defer if (!gpa_poisoned) {
        if (html_buffer_unmanaged) |*buf| buf.deinit(allocator);
    };

    var output = DualOutput.init(allocator, &md_writer_allocating, if (html_writer_allocating) |*hw| hw else null);

    try generateHtmlWrapper(&output, &content);
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    success = try generateReplOutputSection(&output, output_path, &content, config) and success;
    try generateReplProblemsSection(&output, &content);
    try generateHtmlClosing(&output);

    md_buffer_unmanaged = md_writer_allocating.toArrayList();
    if (html_writer_allocating) |*hw| html_buffer_unmanaged.? = hw.toArrayList();

    if (!config.disable_updates) {
        // Write the markdown file
        const md_file = std.Io.Dir.cwd().createFile(app_io, output_path, .{}) catch |err| {
            std.log.err("Failed to create {s}: {}", .{ output_path, err });
            return false;
        };
        defer md_file.close(app_io);

        try md_file.writeStreamingAll(app_io, md_buffer_unmanaged.items);

        if (html_buffer_unmanaged) |*buf| {
            writeHtmlFile(allocator, output_path, buf) catch |err| {
                warn("Failed to write HTML file for {s}: {}", .{ output_path, err });
            };
        }
    }

    return success;
}

fn generateReplOutputSection(output: *DualOutput, snapshot_path: []const u8, content: *const Content, config: *const Config) SnapshotError!bool {
    if (gpa_poisoned) return false;

    var success = true;
    var inputs = std.array_list.Managed([]const u8).init(output.gpa);
    defer if (!gpa_poisoned) inputs.deinit();

    var parts = std.mem.splitSequence(u8, content.source, "»");
    _ = parts.next();
    while (parts.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t\r\n");
        if (trimmed.len > 0) {
            try inputs.append(trimmed);
        }
    }

    var session = SnapshotReplSession{};
    defer if (!gpa_poisoned) session.deinit(output.gpa);

    var actual_outputs = std.array_list.Managed([]const u8).init(output.gpa);
    defer if (!gpa_poisoned) {
        for (actual_outputs.items) |item| {
            output.gpa.free(item);
        }
        actual_outputs.deinit();
    };

    for (inputs.items) |input| {
        const repl_output = try snapshotReplStep(output.gpa, &session, input, config);
        try actual_outputs.append(repl_output);
    }

    switch (config.output_section_command) {
        .update => {
            try output.begin_section("OUTPUT");
            for (actual_outputs.items, 0..) |repl_output, i| {
                if (i > 0) {
                    try output.md_writer.writer.writeAll("---\n");
                }
                try output.md_writer.writer.writeAll(repl_output);
                try output.md_writer.writer.writeByte('\n');

                if (output.html_writer) |writer| {
                    if (i > 0) {
                        try writer.writer.writeAll("                <hr>\n");
                    }
                    try writer.writer.writeAll("                <div class=\"repl-output\">");
                    for (repl_output) |char| {
                        try escapeHtmlChar(&writer.writer, char);
                    }
                    try writer.writer.writeAll("</div>\n");
                }
            }
            try output.end_section();
        },
        .check, .none => {
            const emit_error = config.output_section_command == .check;

            if (content.output) |expected| {
                try output.begin_section("OUTPUT");
                var expected_outputs = std.array_list.Managed([]const u8).init(output.gpa);
                defer expected_outputs.deinit();

                var expected_lines = std.mem.splitSequence(u8, expected, "\n---\n");
                while (expected_lines.next()) |output_str| {
                    const trimmed = std.mem.trim(u8, output_str, " \t\r\n");
                    if (trimmed.len > 0) {
                        try expected_outputs.append(trimmed);
                    }
                }

                if (actual_outputs.items.len != expected_outputs.items.len) {
                    std.debug.print("REPL output count mismatch: got {} outputs, expected {} in {s}\n", .{
                        actual_outputs.items.len,
                        expected_outputs.items.len,
                        snapshot_path,
                    });
                    success = success and !emit_error;
                } else {
                    for (actual_outputs.items, expected_outputs.items, 0..) |actual, expected_output, i| {
                        if (!std.mem.eql(u8, actual, expected_output)) {
                            success = success and !emit_error;
                            std.debug.print(
                                "REPL output mismatch at index {}: got '{s}', expected '{s}' in {s}\n",
                                .{ i, actual, expected_output, snapshot_path },
                            );
                        }
                    }
                }

                for (expected_outputs.items, 0..) |expected_output, i| {
                    if (i > 0) {
                        try output.md_writer.writer.writeAll("---\n");
                    }
                    try output.md_writer.writer.writeAll(expected_output);
                    try output.md_writer.writer.writeByte('\n');

                    if (output.html_writer) |writer| {
                        if (i > 0) {
                            try writer.writer.writeAll("                <hr>\n");
                        }
                        try writer.writer.writeAll("                <div class=\"repl-output\">");
                        for (expected_output) |char| {
                            try escapeHtmlChar(&writer.writer, char);
                        }
                        try writer.writer.writeAll("</div>\n");
                    }
                }
                try output.end_section();
            } else {
                try output.begin_section("OUTPUT");
                for (actual_outputs.items, 0..) |repl_output, i| {
                    if (i > 0) {
                        try output.md_writer.writer.writeAll("---\n");
                    }
                    try output.md_writer.writer.writeAll(repl_output);
                    try output.md_writer.writer.writeByte('\n');

                    if (output.html_writer) |writer| {
                        if (i > 0) {
                            try writer.writer.writeAll("                <hr>\n");
                        }
                        try writer.writer.writeAll("                <div class=\"repl-output\">");
                        for (repl_output) |char| {
                            try escapeHtmlChar(&writer.writer, char);
                        }
                        try writer.writer.writeAll("</div>\n");
                    }
                }
                try output.end_section();
            }
        },
    }

    return success;
}

fn generateReplProblemsSection(output: *DualOutput, _: *const Content) error{WriteFailed}!void {
    try output.begin_section("PROBLEMS");
    try output.md_writer.writer.writeAll("NIL\n");

    if (output.html_writer) |writer| {
        try writer.writer.writeAll(
            \\                <div class="problems">
            \\                    <p>NIL</p>
            \\                </div>
            \\
        );
    }

    try output.end_section();
}

test "snapshot validation" {
    app_io = std.testing.io;
    const allocator = std.testing.allocator;
    if (!try checkSnapshotExpectations(allocator)) {
        return error.SnapshotValidationFailed;
    }
}

test "no Builtin module leaks in snapshots" {
    // IMPORTANT: The "Builtin" module is an implementation detail that should NEVER
    // appear in user-facing error messages. We consolidate all builtin types (Bool,
    // Try, Dict, Set, Str) into a single Builtin module so they can have cyclic
    // dependencies with each other. However, users should only see the type names
    // (e.g., "Dict", "Bool") not qualified names like "Builtin.Dict" or references
    // to the Builtin module in error messages.
    //
    // This test searches all snapshot files for the string "Builtin" (case-sensitive)
    // to detect any leaks. We use case-sensitive search because lowercase "builtin"
    // appears in harmless contexts like "(builtin)" annotations in debug output.

    const allocator = std.testing.allocator;

    // Find all snapshot files
    var snapshots_dir = try std.Io.Dir.cwd().openDir(std.testing.io, "test/snapshots", .{ .iterate = true });
    defer snapshots_dir.close(std.testing.io);

    var files_with_builtin: std.array_list.Managed([]const u8) = .{ .allocator = allocator, .items = &.{}, .capacity = 0 };
    defer {
        for (files_with_builtin.items) |path| {
            allocator.free(path);
        }
        files_with_builtin.deinit();
    }

    // Recursively search for .md files
    try searchDirectoryForBuiltin(allocator, &snapshots_dir, "", &files_with_builtin);

    if (files_with_builtin.items.len > 0) {
        std.debug.print("\n\n❌ FOUND 'Builtin' IN SNAPSHOT FILES (implementation detail leaked!):\n", .{});
        for (files_with_builtin.items) |path| {
            std.debug.print("  - test/snapshots/{s}\n", .{path});
        }
        std.debug.print("\nThe Builtin module should never appear in user-facing error messages.\n", .{});
        std.debug.print("Users should see type names like 'Dict', 'Bool', etc., not 'Builtin.Dict'.\n\n", .{});
        return error.BuiltinModuleLeakedInSnapshots;
    }
}

fn searchDirectoryForBuiltin(
    allocator: std.mem.Allocator,
    dir: *std.Io.Dir,
    relative_path: []const u8,
    files_with_builtin: *std.array_list.Managed([]const u8),
) SnapshotError!void {
    var iter = dir.iterate();
    while (try iter.next(std.testing.io)) |entry| {
        const full_path = if (relative_path.len > 0)
            try std.fmt.allocPrint(allocator, "{s}/{s}", .{ relative_path, entry.name })
        else
            try allocator.dupe(u8, entry.name);
        defer allocator.free(full_path);

        switch (entry.kind) {
            .directory => {
                var subdir = try dir.openDir(std.testing.io, entry.name, .{ .iterate = true });
                defer subdir.close(std.testing.io);
                try searchDirectoryForBuiltin(allocator, &subdir, full_path, files_with_builtin);
            },
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".md")) {
                    const content = try dir.readFileAlloc(std.testing.io, entry.name, allocator, .limited(10 * 1024 * 1024));
                    defer allocator.free(content);

                    // Search for "Builtin" (case-sensitive)
                    if (std.mem.find(u8, content, "Builtin")) |_| {
                        try files_with_builtin.append(try allocator.dupe(u8, full_path));
                    }
                }
            },
            else => {},
        }
    }
}

test "TODO: cross-module function calls - fibonacci" {}

test "TODO: cross-module function calls - nested_ifs" {}

test "TODO: cross-module function calls - repl_boolean_expressions" {}

test "TODO: cross-module function calls - string_edge_cases" {}

test "TODO: cross-module function calls - string_equality_basic" {}

test "TODO: cross-module function calls - string_interpolation_comparison" {}

test "TODO: cross-module function calls - string_multiline_comparison" {}

test "TODO: cross-module function calls - string_ordering_unsupported" {}
