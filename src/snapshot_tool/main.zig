//! Snapshot testing infrastructure for the Roc compiler.
//!
//! This module provides functionality to generate and validate snapshot tests
//! that capture the compiler's behavior at each stage of compilation. Snapshots
//! help ensure the compiler continues to behave as expected by showing the
//! output of tokenization, parsing, canonicalization, type checking etc for
//! the given Roc code snippet.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const types = @import("types");
const reporting = @import("reporting");
const check = @import("check");
const builtins = @import("builtins");
const compile = @import("compile");
const fmt = @import("fmt");
const repl = @import("repl");
const eval_mod = @import("eval");
const tracy = @import("tracy");

const Repl = repl.Repl;
const CrashContext = eval_mod.CrashContext;
const roc_target = @import("roc_target");
const Allocators = base.Allocators;
const CommonEnv = base.CommonEnv;
const Check = check.Check;
const CIR = can.CIR;
const Can = can.Can;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocOps = builtins.host_abi.RocOps;
const RocDbg = builtins.host_abi.RocDbg;
const ModuleEnv = can.ModuleEnv;
const LambdaLifter = can.LambdaLifter;
const Allocator = std.mem.Allocator;
const SExprTree = base.SExprTree;
const LineColMode = base.SExprTree.LineColMode;
const CacheModule = compile.CacheModule;
const single_module = compile.single_module;
const AST = parse.AST;
const Report = reporting.Report;
const parallel = base.parallel;

var verbose_log: bool = false;
var prng = std.Random.DefaultPrng.init(1234567890);

const rand = prng.random();

/// Logs a message if verbose logging is enabled.
fn log(comptime fmt_str: []const u8, args: anytype) void {
    if (verbose_log) {
        std.log.debug(fmt_str, args);
    }
}

/// Always logs a warning message.
fn warn(comptime fmt_str: []const u8, args: anytype) void {
    std.log.warn(fmt_str, args);
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

    fn format(self: ProblemEntry, writer: anytype) !void {
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

/// Parse a PROBLEMS entry to extract problem type and location
fn parseProblemEntry(allocator: std.mem.Allocator, content: []const u8, start_idx: usize) !?struct { entry: ?ProblemEntry, next_idx: usize } {
    var idx = start_idx;

    // Skip whitespace and empty lines
    while (idx < content.len and (content[idx] == ' ' or content[idx] == '\t' or content[idx] == '\n' or content[idx] == '\r')) {
        idx += 1;
    }

    if (idx >= content.len) return null;

    // Check if we're at the start of a problem header
    if (idx + 2 > content.len or !std.mem.eql(u8, content[idx .. idx + 2], "**")) {
        return null;
    }

    // Find the end of the problem type
    const type_start = idx + 2;
    const type_end_search = std.mem.indexOfPos(u8, content, type_start, "**");
    if (type_end_search == null) return null;
    const type_end = type_end_search.?;

    // Check if this is a problem header (all uppercase, no lowercase letters)
    const potential_type = content[type_start..type_end];
    var has_lowercase = false;
    for (potential_type) |c| {
        if (c >= 'a' and c <= 'z') {
            has_lowercase = true;
            break;
        }
    }

    // If it has lowercase letters, this is not a problem header
    if (has_lowercase) {
        return null;
    }

    var problem_type = std.mem.trim(u8, potential_type, " \t\r\n");

    // Handle compound error types like "NOT IMPLEMENTED - UNDEFINED VARIABLE"
    // We only want the last part after the last " - "
    if (std.mem.lastIndexOf(u8, problem_type, " - ")) |dash_idx| {
        problem_type = std.mem.trim(u8, problem_type[dash_idx + 3 ..], " \t\r\n");
    }

    // Skip past the closing ** of the problem type
    var current_idx = type_end + 2;

    // Skip the rest of the line after the problem type
    while (current_idx < content.len and content[current_idx] != '\n') {
        current_idx += 1;
    }
    if (current_idx < content.len) current_idx += 1; // Skip the newline

    // Now look for optional location on the next few lines
    // We'll look until we hit another problem header or end of content
    var location_file: []const u8 = "";
    var location_start_line: u32 = 0;
    var location_start_col: u32 = 0;
    var location_end_line: u32 = 0;
    var location_end_col: u32 = 0;
    var found_location = false;

    while (current_idx < content.len) {
        // Skip whitespace at start of line
        const line_start = current_idx;
        while (current_idx < content.len and (content[current_idx] == ' ' or content[current_idx] == '\t')) {
            current_idx += 1;
        }

        // Check if this line starts with ** (potential new problem or location)
        if (current_idx + 2 <= content.len and std.mem.eql(u8, content[current_idx .. current_idx + 2], "**")) {
            const inner_start = current_idx + 2;
            const inner_end_search = std.mem.indexOfPos(u8, content, inner_start, "**");

            if (inner_end_search) |inner_end| {
                const inner_content = content[inner_start..inner_end];

                // Check if this is a new problem header (no lowercase)
                var inner_has_lowercase = false;
                for (inner_content) |c| {
                    if (c >= 'a' and c <= 'z') {
                        inner_has_lowercase = true;
                        break;
                    }
                }

                if (!inner_has_lowercase) {
                    // This is a new problem header, we're done
                    break;
                }

                // Check if this looks like a location (contains .md: pattern)
                if (std.mem.indexOf(u8, inner_content, ".md:")) |_| {
                    var location = inner_content;
                    // Strip trailing colon and whitespace
                    location = std.mem.trimRight(u8, location, ": \t");

                    // Count colons to determine format
                    var colon_count: usize = 0;
                    for (location) |c| {
                        if (c == ':') colon_count += 1;
                    }

                    if (colon_count == 2) {
                        // Format: file:line:col
                        var parts = std.mem.tokenizeScalar(u8, location, ':');

                        if (parts.next()) |file| {
                            if (parts.next()) |line_str| {
                                if (parts.next()) |col_str| {
                                    if (std.fmt.parseInt(u32, line_str, 10) catch null) |line| {
                                        if (std.fmt.parseInt(u32, col_str, 10) catch null) |col| {
                                            location_file = file;
                                            location_start_line = line;
                                            location_start_col = col;
                                            location_end_line = line;
                                            location_end_col = col;
                                            found_location = true;
                                            current_idx = inner_end + 2;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    } else if (colon_count == 4) {
                        // Format: file:start_line:start_col:end_line:end_col
                        var parts = std.mem.tokenizeScalar(u8, location, ':');

                        if (parts.next()) |file| {
                            if (parts.next()) |start_line_str| {
                                if (parts.next()) |start_col_str| {
                                    if (parts.next()) |end_line_str| {
                                        if (parts.next()) |end_col_str| {
                                            if (std.fmt.parseInt(u32, start_line_str, 10) catch null) |start_line| {
                                                if (std.fmt.parseInt(u32, start_col_str, 10) catch null) |start_col| {
                                                    if (std.fmt.parseInt(u32, end_line_str, 10) catch null) |end_line| {
                                                        if (std.fmt.parseInt(u32, end_col_str, 10) catch null) |end_col| {
                                                            location_file = file;
                                                            location_start_line = start_line;
                                                            location_start_col = start_col;
                                                            location_end_line = end_line;
                                                            location_end_col = end_col;
                                                            found_location = true;
                                                            current_idx = inner_end + 2;
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Skip to next line
        current_idx = line_start;
        while (current_idx < content.len and content[current_idx] != '\n') {
            current_idx += 1;
        }
        if (current_idx < content.len) current_idx += 1;
    }

    // Find the next problem header to determine where this problem ends
    var next_idx = current_idx;
    while (next_idx < content.len) {
        // Skip whitespace at start of line
        while (next_idx < content.len and (content[next_idx] == ' ' or content[next_idx] == '\t')) {
            next_idx += 1;
        }

        if (next_idx + 2 <= content.len and std.mem.eql(u8, content[next_idx .. next_idx + 2], "**")) {
            const check_start = next_idx + 2;
            const check_end = std.mem.indexOfPos(u8, content, check_start, "**");

            if (check_end) |end| {
                const check_content = content[check_start..end];

                // Check if this is a problem header (no lowercase)
                var check_has_lowercase = false;
                for (check_content) |c| {
                    if (c >= 'a' and c <= 'z') {
                        check_has_lowercase = true;
                        break;
                    }
                }

                if (!check_has_lowercase) {
                    // Found next problem header
                    break;
                }
            }
        }

        // Move to next character
        next_idx += 1;
    }

    return .{ .entry = ProblemEntry{
        .problem_type = try allocator.dupe(u8, problem_type),
        .file = try allocator.dupe(u8, location_file),
        .start_line = location_start_line,
        .start_col = location_start_col,
        .end_line = location_end_line,
        .end_col = location_end_col,
    }, .next_idx = next_idx };
}

/// Parse all problems from PROBLEMS section
fn parseProblemsSection(allocator: std.mem.Allocator, content: []const u8) !std.array_list.Managed(ProblemEntry) {
    var problems = std.array_list.Managed(ProblemEntry).init(allocator);
    errdefer {
        for (problems.items) |p| {
            allocator.free(p.problem_type);
            allocator.free(p.file);
        }
        problems.deinit();
    }

    // Check if the entire content is just NIL
    const trimmed_content = std.mem.trim(u8, content, " \t\r\n");
    if (std.mem.eql(u8, trimmed_content, "NIL")) {
        // NIL means there are no problems
        return problems;
    }

    var idx: usize = 0;
    while (idx < content.len) {
        const result = try parseProblemEntry(allocator, content, idx);
        if (result) |r| {
            if (r.entry) |entry| {
                try problems.append(entry);
            }
            idx = r.next_idx;
        } else {
            break;
        }
    }

    return problems;
}

/// Generate EXPECTED content from problems
fn generateExpectedContent(allocator: std.mem.Allocator, problems: []const ProblemEntry) ![]const u8 {
    if (problems.len == 0) {
        return try allocator.dupe(u8, "NIL");
    }

    var buffer = std.array_list.Managed(u8).init(allocator);
    errdefer buffer.deinit();

    for (problems, 0..) |problem, i| {
        if (i > 0) {
            try buffer.append('\n');
        }
        try problem.format(buffer.writer());
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
) !std.array_list.Managed(reporting.Report) {
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

    // Generate type checking reports
    for (solver.problems.problems.items) |problem| {
        const empty_modules: []const *ModuleEnv = &.{};
        var report_builder = check.ReportBuilder.init(
            allocator,
            module_env,
            can_ir,
            &solver.snapshots,
            &solver.problems,
            snapshot_path,
            empty_modules,
            &solver.import_mapping,
            &solver.regions,
        ) catch continue;
        defer report_builder.deinit();

        const report = report_builder.build(problem) catch |err| {
            std.debug.panic("Failed to create type checking report for snapshot {s}: {s}", .{ snapshot_path, @errorName(err) });
        };
        try reports.append(report);
    }

    return reports;
}

/// Render reports to PROBLEMS section format (markdown and HTML)
fn renderReportsToProblemsSection(output: *DualOutput, reports: *const std.array_list.Managed(reporting.Report)) !void {
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
        // Render all reports in order
        for (reports.items) |report| {
            report.render(&output.md_writer.writer, .markdown) catch |err| {
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

/// Render reports to EXPECTED section format (parsed problem entries)
fn renderReportsToExpectedContent(allocator: std.mem.Allocator, reports: *const std.array_list.Managed(reporting.Report)) ![]const u8 {
    if (reports.items.len == 0) {
        return try allocator.dupe(u8, "NIL");
    }

    // Render all reports to markdown and then parse the problems
    var problems_buffer_unmanaged = std.ArrayList(u8).empty;
    var problems_writer_allocating: std.Io.Writer.Allocating = .fromArrayList(allocator, &problems_buffer_unmanaged);
    defer problems_buffer_unmanaged.deinit(allocator);

    // Render all reports to markdown
    for (reports.items) |report| {
        report.render(&problems_writer_allocating.writer, .markdown) catch |err| {
            std.debug.panic("Failed to render report for EXPECTED: {s}", .{@errorName(err)});
        };
    }

    // Transfer contents from writer back to buffer before parsing
    problems_buffer_unmanaged = problems_writer_allocating.toArrayList();

    // Parse the rendered problems and convert to EXPECTED format
    // TODO: rather than parsing markdown, we should directly generate EXPECTED format from the reports
    var parsed_problems = try parseProblemsSection(allocator, problems_buffer_unmanaged.items);
    defer {
        for (parsed_problems.items) |p| {
            allocator.free(p.problem_type);
            allocator.free(p.file);
        }
        parsed_problems.deinit();
    }

    return try generateExpectedContent(allocator, parsed_problems.items);
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .{
    .backing_allocator = std.heap.c_allocator,
};

/// cli entrypoint for snapshot tool
pub fn main() !void {
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

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

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
                \\  --debug         Use GeneralPurposeAllocator for debugging (default: c_allocator)
                \\  --trace-eval    Enable interpreter trace output (only works with single REPL snapshot)
                \\  --linecol       Include line/column information in output
                \\  --threads <n>   Number of threads to use (0 = auto-detect, 1 = single-threaded). Default: 0.
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

    const config = Config{
        .maybe_fuzz_corpus_path = maybe_fuzz_corpus_path,
        .generate_html = generate_html,
        .expected_section_command = expected_section_command,
        .output_section_command = output_section_command,
        .trace_eval = trace_eval,
        .linecol_mode = linecol_mode,
        .builtin_module = builtin_modules_ptr.builtin_module.env,
        .builtin_indices = builtin_modules_ptr.builtin_indices,
    };

    if (config.maybe_fuzz_corpus_path != null) {
        log("copying SOURCE from snapshots to: {s}", .{config.maybe_fuzz_corpus_path.?});
        try std.fs.cwd().makePath(config.maybe_fuzz_corpus_path.?);
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

fn checkSnapshotExpectations(gpa: Allocator) !bool {
    // Load builtin modules using the same code path as roc check
    const builtin_modules_ptr = try gpa.create(eval_mod.BuiltinModules);
    defer gpa.destroy(builtin_modules_ptr);

    builtin_modules_ptr.* = try eval_mod.BuiltinModules.init(gpa);
    defer builtin_modules_ptr.deinit();

    const config = Config{
        .maybe_fuzz_corpus_path = null,
        .generate_html = false,
        .expected_section_command = .check,
        .output_section_command = .check,
        .disable_updates = true,
        .builtin_module = builtin_modules_ptr.builtin_module.env,
        .builtin_indices = builtin_modules_ptr.builtin_indices,
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

    var fail_count: usize = 0;

    for (work_list.items) |work_item| {
        const success = switch (work_item.kind) {
            .snapshot_file => processSnapshotFile(gpa, work_item.path, &config) catch false,
            .multi_file_snapshot => blk: {
                const res = processMultiFileSnapshot(gpa, work_item.path, &config) catch {
                    break :blk false;
                };
                break :blk res;
            },
        };
        if (!success) {
            fail_count += 1;
        }
    }
    return fail_count == 0;
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
    return .file; // fallback, shouldn't happen if isMultiFileSnapshot was checked first
}

fn processMultiFileSnapshot(allocator: Allocator, dir_path: []const u8, config: *const Config) !bool {
    var success: bool = true;
    log("Processing multi-file snapshot directory: {s}", .{dir_path});

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        std.log.err("Failed to open directory {s}: {}", .{ dir_path, err });
        return false;
    };
    defer dir.close();

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
    while (try iterator.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
            const full_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
            defer allocator.free(full_path);

            if (std.fs.cwd().readFileAlloc(allocator, full_path, 1024 * 1024)) |content| {
                defer allocator.free(content);

                // Extract EXPECTED section
                const expected_header = "# EXPECTED\n";
                if (std.mem.indexOf(u8, content, expected_header)) |start_idx| {
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

        while (try iterator.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
                const file_path = try allocator.dupe(u8, entry.name);
                try files_to_delete.append(file_path);
            }
        }

        for (files_to_delete.items) |file_name| {
            dir.deleteFile(file_name) catch |err| {
                warn("Failed to delete {s}: {}", .{ file_name, err });
            };
        }
    }

    // Find all .roc files and generate snapshots for each
    iterator = dir.iterate();
    const snapshot_type = getMultiFileSnapshotType(dir_path);

    while (try iterator.next()) |entry| {
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
            const roc_content = std.fs.cwd().readFileAlloc(allocator, roc_file_path, 1024 * 1024) catch |err| {
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
) !bool {
    var success = true;
    log("Generating snapshot for: {s}", .{output_path});

    // Handle REPL snapshots separately
    if (content.meta.node_type == .repl) {
        return processReplSnapshot(allocator, content, output_path, config);
    }

    // Handle dev_object snapshots separately (multi-file, cross-compilation)
    if (content.meta.node_type == .dev_object) {
        return processDevObjectSnapshot(allocator, content, output_path, config);
    }

    // Process the content through the compilation pipeline
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    // Extract module name from custom filename if provided, otherwise from output path
    const module_name = if (content.meta.filename) |custom_filename|
        // Strip .roc extension if present
        if (std.mem.lastIndexOfScalar(u8, custom_filename, '.')) |dot_idx|
            custom_filename[0..dot_idx]
        else
            custom_filename
    else blk: {
        const basename = std.fs.path.basename(output_path);
        break :blk if (std.mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
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
        .repl, .dev_object => unreachable, // Handled above
    };

    // Create ModuleEnv (caller manages memory)
    var module_env = try single_module.ModuleEnv.init(allocator, content.source);
    defer module_env.deinit();
    var can_ir = &module_env;

    // Create allocators for parsing
    var allocators: single_module.Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    // Parse using the unified compile_module interface
    const parse_ast = try single_module.parseSingleModule(
        &allocators,
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
            var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
            defer module_envs.deinit();

            if (config.builtin_module) |builtin_env| {
                try Can.populateModuleEnvs(&module_envs, can_ir, builtin_env, config.builtin_indices);
            }

            var czer = try Can.init(&allocators, can_ir, parse_ast, &module_envs);
            defer czer.deinit();
            try czer.canonicalizeFile();
        },
        .header => {
            // TODO: implement canonicalize_header when available
        },
        .expr, .statement => {
            // Expr and statement tests use different canonicalization methods
            // Auto-inject builtin types (Bool, Try, List, Dict, Set, Str, and numeric types) as available imports
            var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
            defer module_envs.deinit();

            if (config.builtin_module) |builtin_env| {
                try Can.populateModuleEnvs(&module_envs, can_ir, builtin_env, config.builtin_indices);
            }

            var czer = try Can.init(&allocators, can_ir, parse_ast, &module_envs);
            defer czer.deinit();

            switch (content.meta.node_type) {
                .expr => {
                    const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
                    maybe_expr_idx = try czer.canonicalizeExpr(expr_idx);
                },
                .statement => {
                    const ast_stmt_idx: AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
                    const can_stmt_result = try czer.canonicalizeBlockStatement(czer.parse_ir.store.getStatement(ast_stmt_idx), &.{}, 0);
                    if (can_stmt_result.canonicalized_stmt) |can_stmt| {
                        // Manually track scratch statements because we aren't using the file entrypoint
                        const scratch_statements_start = can_ir.store.scratch.?.statements.top();
                        try can_ir.store.addScratchStatement(can_stmt.idx);
                        can_ir.all_statements = try can_ir.store.statementSpanFrom(scratch_statements_start);
                    }
                },
                else => unreachable,
            }
        },
        .repl, .dev_object => unreachable, // Handled above
    }

    // Assert that everything is in-sync
    can_ir.debugAssertArraysInSync();

    // Compute dependency-based evaluation order if not already set
    // Skip for .file/.package/.platform/.app tests because canonicalizeAndTypeCheckModule
    // will call canonicalizeFile which sets it. Only do this for .expr/.statement which
    // don't call canonicalizeFile.
    const needs_evaluation_order = switch (content.meta.node_type) {
        .expr, .statement, .mono => true,
        .file, .package, .platform, .app, .snippet, .repl, .header, .dev_object => false,
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
        if (std.mem.eql(u8, import_name, "Builtin")) {
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

        var checker = try Check.init(
            allocator,
            &can_ir.types,
            can_ir,
            builtin_modules.items,
            &module_envs,
            &can_ir.store.regions,
            builtin_ctx,
        );
        _ = try checker.checkExprRepl(expr_idx.idx);
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

            var checker = try compile.PackageEnv.canonicalizeAndTypeCheckModule(
                &allocators,
                allocator,
                can_ir,
                parse_ast,
                builtin_env,
                config.builtin_indices,
                imported_envs_for_file,
                &module_envs_for_file.?,
            );
            // For app modules, numeric defaults were deferred by canonicalizeAndTypeCheckModule.
            // Since snapshot tests don't have platform requirements, finalize them here.
            if (can_ir.defer_numeric_defaults) {
                try checker.finalizeNumericDefaults();
            }
            break :blk checker;
        },
        .snippet, .statement, .header, .expr, .mono => blk: {
            // For snippet/statement/header/expr/mono tests, type check the already-canonicalized IR
            // Note: .expr and .mono can reach here if canonicalizeExpr returned null (error during canonicalization)
            var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);

            if (config.builtin_module) |builtin_env| {
                try Can.populateModuleEnvs(&module_envs, can_ir, builtin_env, config.builtin_indices);
            }

            var checker = try Check.init(
                allocator,
                &can_ir.types,
                can_ir,
                builtin_modules.items,
                &module_envs,
                &can_ir.store.regions,
                builtin_ctx,
            );
            // For app modules, defer numeric defaults (they'll be finalized below).
            // This matches the behavior in compile_package.zig.
            if (can_ir.defer_numeric_defaults) {
                try checker.checkFileSkipNumericDefaults();
                // Finalize numeric defaults now since there's no platform requirements check
                try checker.finalizeNumericDefaults();
            } else {
                try checker.checkFile();
            }
            module_envs_for_snippet = module_envs; // Keep alive
            break :blk checker;
        },
        .repl, .dev_object => unreachable, // Should never reach here - handled earlier
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

        var original_sexpr = std.array_list.Managed(u8).init(allocator);
        defer original_sexpr.deinit();
        try original_tree.toStringPretty(original_sexpr.writer().any(), .skip_linecol);

        // Create arena for serialization
        var cache_arena = std.heap.ArenaAllocator.init(allocator);
        defer cache_arena.deinit();

        // Create and serialize MmapCache
        const cache_data = try CacheModule.create(allocator, cache_arena.allocator(), can_ir, can_ir, 0, 0);
        defer allocator.free(cache_data);

        // Deserialize back
        var loaded_cache = try CacheModule.fromMappedMemory(cache_data);

        // Create arena for restore operation to handle temporary allocations
        var restore_arena = std.heap.ArenaAllocator.init(allocator);
        defer restore_arena.deinit();

        // Restore ModuleEnv
        const restored_env = try loaded_cache.restore(restore_arena.allocator(), module_name, content.source);
        // Note: restored_env points to data within the cache, so we don't free it

        // Generate S-expression from restored ModuleEnv
        var restored_tree = SExprTree.init(allocator);
        defer restored_tree.deinit();
        try ModuleEnv.pushToSExprTree(restored_env, null, &restored_tree);

        var restored_sexpr = std.array_list.Managed(u8).init(allocator);
        defer restored_sexpr.deinit();
        try restored_tree.toStringPretty(restored_sexpr.writer().any(), .skip_linecol);

        // Compare S-expressions - crash if they don't match
        if (!std.mem.eql(u8, original_sexpr.items, restored_sexpr.items)) {
            std.log.err("Cache round-trip validation failed for snapshot: {s}", .{output_path});
            std.log.err("Original and restored CIR S-expressions don't match!", .{});
            std.log.err("This indicates a bug in MmapCache serialization/deserialization.", .{});
            std.log.err("Original S-expression:\n{s}", .{original_sexpr.items});
            std.log.err("Restored S-expression:\n{s}", .{restored_sexpr.items});
            return error.CacheRoundTripValidationFailed;
        }
    }

    // Run closure transformation for mono tests
    // This transforms closures to tags and generates dispatch match expressions
    var has_closure_transforms = false;

    // Lambda lifting storage - will be populated if we have closures
    var lifter: ?LambdaLifter = null;
    defer {
        if (lifter) |*l| l.deinit();
    }

    if (content.meta.node_type == .mono) {
        const ClosureTransformer = can.ClosureTransformer;
        var transformer = ClosureTransformer.init(allocator, can_ir);
        defer transformer.deinit();

        // First pass: mark all top-level patterns
        // Top-level constants don't need to be captured since they're always in scope
        const defs = can_ir.store.sliceDefs(can_ir.all_defs);
        for (defs) |def_idx| {
            const def = can_ir.store.getDef(def_idx);
            try transformer.markTopLevel(def.pattern);
        }

        // Second pass: transform all top-level definitions
        for (defs) |def_idx| {
            const def = can_ir.store.getDef(def_idx);

            // Get name hint from pattern
            const pattern = can_ir.store.getPattern(def.pattern);
            const name_hint: ?base.Ident.Idx = switch (pattern) {
                .assign => |a| a.ident,
                else => null,
            };

            // Transform the definition expression
            const old_expr = def.expr;
            const result = try transformer.transformExprWithLambdaSet(def.expr, name_hint);

            // If the expression changed, compute the correct type for the new expression
            // based on its structure (e.g., tag union for closure transforms)
            // Note: We don't modify the pattern's type here to avoid breaking type unification.
            // Instead, the type will be computed from the expression in generateMonoSection.
            if (old_expr != result.expr) {
                _ = try computeTransformedExprType(can_ir, result.expr);
            }

            // Track the lambda set for this pattern
            if (result.lambda_set) |lambda_set| {
                try transformer.pattern_lambda_sets.put(def.pattern, lambda_set);
                has_closure_transforms = true;
            }

            // If the expression is a lambda with a return set, track what it returns when called
            // This allows propagating closure information through calls to this lambda
            if (transformer.lambda_return_sets.get(result.expr)) |return_set| {
                // Clone the return set to avoid double-free issues
                const cloned = try return_set.clone(allocator);
                try transformer.pattern_lambda_return_sets.put(def.pattern, cloned);
            }

            // Update the definition to use the transformed expression
            can_ir.store.setDefExpr(def_idx, result.expr);
        }

        // Also check if any closures were transformed (even nested inside pure lambdas)
        // This is important because nested closures may not produce a lambda_set at the top level
        has_closure_transforms = has_closure_transforms or transformer.closures.count() > 0;

        // Validate that all lambda sets have been fully resolved.
        // Any remaining unspecialized closures indicate a failure to resolve static dispatch implementations.
        const validation_result = transformer.validateAllResolved();
        if (!validation_result.is_valid) {
            // Log the validation failure
            if (validation_result.first_error) |err| {
                std.log.err("Lambda set validation failed: {d} unresolved closures. First error: {s}", .{
                    validation_result.unresolved_count,
                    @tagName(err.kind),
                });
            } else {
                std.log.err("Lambda set validation failed: {d} unresolved closures", .{
                    validation_result.unresolved_count,
                });
            }
            return error.UnresolvedLambdaSets;
        }

        // Phase 4 & 5: Lambda lifting - extract closures to top-level function definitions
        // After the ClosureTransformer has identified all closures, use LambdaLifter
        // to create lifted function definitions for each one.
        const has_any_closures = transformer.closures.count() > 0 or
            transformer.pattern_lambda_sets.count() > 0;

        if (has_any_closures) {
            lifter = LambdaLifter.init(allocator, can_ir, &transformer.top_level_patterns);

            // Iterate over all transformed closures (closures with captures)
            var closure_iter = transformer.closures.iterator();
            while (closure_iter.next()) |entry| {
                const closure_idx = entry.key_ptr.*;
                const closure_info = entry.value_ptr.*;

                // Lift this closure to a top-level function
                try lifter.?.liftClosure(closure_idx, closure_info.tag_name);
            }

            // Also lift closures from pattern_lambda_sets (pure lambdas converted to tags)
            var lambda_set_iter = transformer.pattern_lambda_sets.iterator();
            while (lambda_set_iter.next()) |entry| {
                const lambda_set = entry.value_ptr;
                for (lambda_set.closures.items) |closure_info| {
                    // Check if this closure was already lifted via transformer.closures
                    // (closures with captures are in both places)
                    var already_lifted = false;
                    var check_iter = transformer.closures.iterator();
                    while (check_iter.next()) |check_entry| {
                        if (check_entry.value_ptr.tag_name == closure_info.tag_name) {
                            already_lifted = true;
                            break;
                        }
                    }

                    if (!already_lifted) {
                        // Lift this pure lambda using the info-based method
                        try lifter.?.liftFromInfo(closure_info);
                    }
                }
            }
        }
    }

    // Run constant folding for mono tests (skip if we have closure transformations)
    // This evaluates expressions at compile time and folds results back into the CIR.
    // We skip this when closures have been transformed because the comptime evaluator
    // doesn't yet know how to handle the closure tag format.
    if (content.meta.node_type == .mono and !has_closure_transforms) {
        if (config.builtin_module) |builtin_env| {
            const BuiltinTypes = eval_mod.BuiltinTypes;
            const ComptimeEvaluator = eval_mod.ComptimeEvaluator;
            const builtin_types = BuiltinTypes.init(config.builtin_indices, builtin_env, builtin_env, builtin_env);
            const imported_envs: []const *const ModuleEnv = builtin_modules.items;
            var comptime_evaluator = try ComptimeEvaluator.init(allocator, can_ir, imported_envs, &solver.problems, builtin_types, builtin_env, &solver.import_mapping, roc_target.RocTarget.detectNative());
            defer comptime_evaluator.deinit();

            // First evaluate any top-level defs
            _ = try comptime_evaluator.evalAll();

            // Then evaluate and fold the standalone expression if present
            if (Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx)) |expr_idx| {
                _ = try comptime_evaluator.evalAndFoldExpr(expr_idx);
            }
        }
    }

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
        const lifted_funcs = if (lifter) |l| l.getLiftedFunctions() else &[_]LambdaLifter.LiftedFunction{};
        try generateMonoSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx), output_path, config, lifted_funcs);
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
        const md_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
            std.log.err("Failed to create {s}: {}", .{ output_path, err });
            return false;
        };
        defer md_file.close();

        try md_file.writeAll(md_buffer_unmanaged.items);

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
) !bool {
    // Create content structure
    const content = Content{
        .meta = meta,
        .source = roc_content,
        .expected = expected_content,
        .output = null,
        .formatted = null,
        .dev_output = null,
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
        .snapshot_file => processSnapshotFile(allocator, work_item.path, context.config) catch false,
        .multi_file_snapshot => blk: {
            const res = processMultiFileSnapshot(allocator, work_item.path, context.config) catch {
                break :blk false;
            };
            break :blk res;
        },
    };

    if (success) {
        _ = context.success_count.fetchAdd(1, .monotonic);
    } else {
        _ = context.failed_count.fetchAdd(1, .monotonic);
    }
}

/// Stage 2: Process work items in parallel using the parallel utility
fn processWorkItems(gpa: Allocator, work_list: WorkList, max_threads: usize, debug: bool, config: *const Config) !ProcessResult {
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
fn collectWorkItems(gpa: Allocator, path: []const u8, work_list: *WorkList) !void {
    const canonical_path = std.fs.cwd().realpathAlloc(gpa, path) catch |err| {
        std.log.err("failed to resolve path '{s}': {s}", .{ path, @errorName(err) });
        return;
    };
    defer gpa.free(canonical_path);

    // Try to open as directory first
    if (std.fs.cwd().openDir(canonical_path, .{ .iterate = true })) |dir_handle| {
        var dir = dir_handle;
        defer dir.close();

        // It's a directory
        if (isMultiFileSnapshot(canonical_path)) {
            const path_copy = try gpa.dupe(u8, canonical_path);
            try work_list.append(WorkItem{
                .path = path_copy,
                .kind = .multi_file_snapshot,
            });
        } else {
            var dir_iterator = dir.iterate();
            while (try dir_iterator.next()) |entry| {
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
            return std.mem.trimRight(u8, content[self.start..self.end], "\n");
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

    fn fromString(str: []const u8) !NodeType {
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
        };
    }
};

const Meta = struct {
    description: []const u8,
    node_type: NodeType,
    filename: ?[]const u8 = null,

    const DESC_START: []const u8 = "description=";
    const TYPE_START: []const u8 = "type=";

    fn fromString(text: []const u8) Error!Meta {
        var lines = std.mem.splitScalar(u8, text, '\n');
        var desc: []const u8 = "";
        var node_type: NodeType = .file;
        var filename: ?[]const u8 = null;
        while (true) {
            var line = lines.next() orelse break;
            if (std.mem.startsWith(u8, line, DESC_START)) {
                desc = line[(DESC_START.len)..];
            } else if (std.mem.startsWith(u8, line, TYPE_START)) {
                const ty = line[(TYPE_START.len)..];
                // Check if there's a colon indicating a custom filename
                if (std.mem.indexOfScalar(u8, ty, ':')) |colon_idx| {
                    node_type = try NodeType.fromString(ty[0..colon_idx]);
                    filename = ty[colon_idx + 1 ..];
                } else {
                    node_type = try NodeType.fromString(ty);
                }
            }
        }

        return .{
            .description = desc,
            .node_type = node_type,
            .filename = filename,
        };
    }

    fn format(self: Meta, writer: anytype) !void {
        try writer.writeAll(DESC_START);
        try writer.writeAll(self.description);
        try writer.writeAll("\n");
        try writer.writeAll(TYPE_START);
        try writer.writeAll(self.node_type.toString());
        if (self.filename) |fname| {
            try writer.writeAll(":");
            try writer.writeAll(fname);
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
    has_canonicalize: bool,

    fn from_ranges(ranges: std.AutoHashMap(Section, Section.Range), content: []const u8) Error!Content {
        var source: []const u8 = undefined;
        var expected: ?[]const u8 = undefined;
        var output: ?[]const u8 = undefined;
        var formatted: ?[]const u8 = undefined;
        var dev_output: ?[]const u8 = undefined;
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

    fn begin_section(self: *DualOutput, name: []const u8) !void {
        try self.md_writer.writer.print("# {s}\n", .{name});
        if (self.html_writer) |writer| {
            try writer.writer.print(
                \\        <div class="section" data-section="{s}">
                \\            <div class="section-content">
            , .{name});
        }
    }

    fn end_section(self: *DualOutput) !void {
        if (self.html_writer) |writer| {
            try writer.writer.writeAll(
                \\            </div>
                \\        </div>
            );
        }
    }

    fn begin_code_block(self: *DualOutput, language: []const u8) !void {
        try self.md_writer.writer.print("~~~{s}\n", .{language});
    }

    fn end_code_block(self: *DualOutput) !void {
        try self.md_writer.writer.writeAll("~~~\n");
    }
};

/// Helper function to escape HTML characters
fn escapeHtmlChar(writer: anytype, char: u8) !void {
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
fn generateMetaSection(output: *DualOutput, content: *const Content) !void {
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
fn generateSourceSection(output: *DualOutput, content: *const Content) !void {
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
) !bool {
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
                std.debug.print("Hint: use `zig build snapshot -- --update-expected` to automatically update the expectations.\n", .{});

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
fn generateProblemsSection(output: *DualOutput, reports: *const std.array_list.Managed(reporting.Report)) !void {
    try output.begin_section("PROBLEMS");
    try renderReportsToProblemsSection(output, reports);
    try output.end_section();
}

/// Generate TOKENS section for both markdown and HTML
pub fn generateTokensSection(output: *DualOutput, parse_ast: *AST, _: *const Content, module_env: *ModuleEnv, linecol_mode: LineColMode) !void {
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
fn generateParseSection(output: *DualOutput, content: *const Content, parse_ast: *AST, env: *CommonEnv, linecol_mode: LineColMode) !void {
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
        .dev_object => unreachable, // Handled separately
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
fn generateFormattedSection(output: *DualOutput, content: *const Content, parse_ast: *AST) !void {
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
        .dev_object => unreachable, // Handled separately
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
fn generateCanonicalizeSection(output: *DualOutput, can_ir: *ModuleEnv, maybe_expr_idx: ?CIR.Expr.Idx, linecol_mode: LineColMode) !void {
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
fn generateTypesSection(output: *DualOutput, can_ir: *ModuleEnv, maybe_expr_idx: ?CIR.Expr.Idx, linecol_mode: LineColMode) !void {
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
) !types.Var {
    const expr_var = ModuleEnv.varFrom(expr_idx);

    // Ensure type var exists for this expression
    if (@intFromEnum(expr_var) >= can_ir.types.len()) {
        const current_len: usize = @intCast(can_ir.types.len());
        const needed_len: usize = @intCast(@intFromEnum(expr_var) + 1);
        var i: usize = current_len;
        while (i < needed_len) : (i += 1) {
            _ = try can_ir.types.fresh();
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
                    _ = try can_ir.types.fresh();
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
fn getDefaultedTypeString(allocator: std.mem.Allocator, can_ir: *ModuleEnv, type_var: types.Var) ![]const u8 {
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
) ![]const u8 {
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
        .flex => |flex| {
            // Check if this flex var has a from_numeral constraint
            const constraints = can_ir.types.sliceStaticDispatchConstraints(flex.constraints);
            for (constraints) |constraint| {
                if (constraint.origin == .from_numeral) {
                    return allocator.dupe(u8, "Dec");
                }
            }
            // No numeral constraint - fall through to TypeWriter
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

    // Use TypeWriter for all other cases - it has proper cycle detection
    var type_writer = try can_ir.initTypeWriter();
    defer type_writer.deinit();

    // Enable numeral defaulting for MONO output - flex vars with from_numeral
    // constraint should display as "Dec" instead of showing the constraint
    type_writer.setDefaultNumeralsToDec(true);

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
fn getMonoTypeString(allocator: std.mem.Allocator, can_ir: *ModuleEnv, expr_idx: CIR.Expr.Idx) ![]const u8 {
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
    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    const validation_ast = parse.parse(&allocators, &validation_env.common) catch |err| {
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

    // Set up module_envs with builtin types if available
    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    if (config.builtin_module) |builtin_env| {
        Can.populateModuleEnvs(&module_envs, &validation_env, builtin_env, config.builtin_indices) catch |err| {
            std.log.err("MONO VALIDATION ERROR in {s}: Failed to populate module envs: {}", .{ source_path, err });
            return false;
        };
    }

    // Canonicalize the parsed MONO output
    var czer = Can.init(&allocators, &validation_env, validation_ast, &module_envs) catch |err| {
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

    // Count only actual errors, not warnings (shadowing_warning is just a warning)
    var error_count: usize = 0;
    for (can_diagnostics) |diagnostic| {
        switch (diagnostic) {
            .shadowing_warning => {}, // Skip warnings
            else => error_count += 1,
        }
    }

    if (error_count > 0) {
        std.log.err("MONO CANONICALIZATION ERROR in {s}: {d} error(s) in generated MONO output:", .{ source_path, error_count });
        for (can_diagnostics) |diagnostic| {
            switch (diagnostic) {
                .shadowing_warning => {}, // Skip warnings in output too
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

    var checker = Check.init(
        allocator,
        &validation_env.types,
        &validation_env,
        &.{}, // No imported modules
        &module_envs,
        &validation_env.store.regions,
        builtin_ctx,
    ) catch |err| {
        std.log.err("MONO VALIDATION ERROR in {s}: Failed to initialize type checker: {}", .{ source_path, err });
        return false;
    };
    defer checker.deinit();

    // For app modules, defer numeric defaults (they'll be finalized below).
    // This matches the behavior in compile_package.zig.
    if (validation_env.defer_numeric_defaults) {
        checker.checkFileSkipNumericDefaults() catch |err| {
            std.log.err("MONO VALIDATION ERROR in {s}: Type checking failed: {}", .{ source_path, err });
            return false;
        };
        // Finalize numeric defaults now since there's no platform requirements check
        checker.finalizeNumericDefaults() catch |err| {
            std.log.err("MONO VALIDATION ERROR in {s}: Numeric defaults finalization failed: {}", .{ source_path, err });
            return false;
        };
    } else {
        checker.checkFile() catch |err| {
            std.log.err("MONO VALIDATION ERROR in {s}: Type checking failed: {}", .{ source_path, err });
            return false;
        };
    }

    // Check for type-checking problems
    const type_problems = checker.problems.problems.items;
    if (type_problems.len > 0) {
        std.log.err("MONO TYPE ERROR in {s}: {d} type error(s) in generated MONO output:", .{ source_path, type_problems.len });
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
fn parseAndFormat(gpa: std.mem.Allocator, input: []const u8) ![]const u8 {
    var module_env = try ModuleEnv.init(gpa, input);
    defer module_env.deinit();

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const parse_ast = try parse.parse(&allocators, &module_env.common);
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

/// Generate MONO section for mono tests - emits monomorphized type module
fn generateMonoSection(output: *DualOutput, can_ir: *ModuleEnv, _: ?CIR.Expr.Idx, source_path: []const u8, config: *const Config, lifted_functions: []const LambdaLifter.LiftedFunction) !void {
    // First, build the mono source in a buffer for validation
    var mono_buffer = std.ArrayList(u8).empty;
    defer mono_buffer.deinit(output.gpa);

    // Emit all top-level definitions (no module header - type modules are headerless)
    var emitter = can.RocEmitter.init(output.gpa, can_ir);
    defer emitter.deinit();

    // Phase 5: Output lifted function definitions first (before regular definitions)
    // These are the closures that have been lifted to top-level functions.
    // Dispatch now calls these functions instead of inlining the lambda bodies.
    for (lifted_functions) |lifted_fn| {
        // Get the function name and convert to a valid Roc identifier
        const fn_name = can_ir.getIdent(lifted_fn.name);

        // Convert # prefix to 'c' (for compiler-generated closures like #1_foo -> c1_foo)
        // or uppercase first char to lowercase for backwards compatibility
        var fn_name_lower = try output.gpa.alloc(u8, fn_name.len);
        defer output.gpa.free(fn_name_lower);
        @memcpy(fn_name_lower, fn_name);
        if (fn_name_lower.len > 0 and fn_name_lower[0] == '#') {
            fn_name_lower[0] = 'c';
        } else if (fn_name_lower.len > 0 and fn_name_lower[0] >= 'A' and fn_name_lower[0] <= 'Z') {
            fn_name_lower[0] = fn_name_lower[0] + ('a' - 'A');
        }

        // Output as a proper definition
        try mono_buffer.appendSlice(output.gpa, fn_name_lower);
        try mono_buffer.appendSlice(output.gpa, " = |");

        // Emit the original lambda arguments
        const args = can_ir.store.slicePatterns(lifted_fn.args);
        for (args, 0..) |arg_pattern, i| {
            if (i > 0) {
                try mono_buffer.appendSlice(output.gpa, ", ");
            }
            emitter.reset();
            try emitter.emitPattern(arg_pattern);
            try mono_buffer.appendSlice(output.gpa, emitter.getOutput());
        }

        // Add the captures parameter if there are captures
        if (lifted_fn.captures_pattern) |captures_pat| {
            if (args.len > 0) {
                try mono_buffer.appendSlice(output.gpa, ", ");
            }
            emitter.reset();
            try emitter.emitPattern(captures_pat);
            try mono_buffer.appendSlice(output.gpa, emitter.getOutput());
        }

        try mono_buffer.appendSlice(output.gpa, "| ");

        // Emit the transformed body
        emitter.reset();
        try emitter.emitExpr(lifted_fn.body);
        try mono_buffer.appendSlice(output.gpa, emitter.getOutput());
        try mono_buffer.appendSlice(output.gpa, "\n\n");
    }

    const defs = can_ir.store.sliceDefs(can_ir.all_defs);
    const has_lifted_functions = lifted_functions.len > 0;
    for (defs) |def_idx| {
        const def = can_ir.store.getDef(def_idx);

        // Emit the pattern (left side of =)
        emitter.reset();
        try emitter.emitPattern(def.pattern);
        const pattern_output = try output.gpa.dupe(u8, emitter.getOutput());
        defer output.gpa.free(pattern_output);

        // Emit the expression (right side of =)
        emitter.reset();
        try emitter.emitExpr(def.expr);

        // For closure transforms, skip type annotations since computing them correctly
        // is complex (transformed expressions have new indices without proper type vars).
        // For non-closure transforms, emit type annotations using the pattern's type.
        if (has_lifted_functions) {
            // Skip type annotations - just emit: name = expr
            try mono_buffer.appendSlice(output.gpa, pattern_output);
            try mono_buffer.appendSlice(output.gpa, " = ");
            try mono_buffer.appendSlice(output.gpa, emitter.getOutput());
            try mono_buffer.appendSlice(output.gpa, "\n\n");
        } else {
            // Use the pattern's type from type checking (not computed from expression)
            const pattern_type = ModuleEnv.varFrom(def.pattern);
            const type_str = try getDefaultedTypeString(output.gpa, can_ir, pattern_type);
            defer output.gpa.free(type_str);

            // Build the mono source: name : Type\nname = expr\n
            try mono_buffer.appendSlice(output.gpa, pattern_output);
            try mono_buffer.appendSlice(output.gpa, " : ");
            try mono_buffer.appendSlice(output.gpa, type_str);
            try mono_buffer.appendSlice(output.gpa, "\n");
            try mono_buffer.appendSlice(output.gpa, pattern_output);
            try mono_buffer.appendSlice(output.gpa, " = ");
            try mono_buffer.appendSlice(output.gpa, emitter.getOutput());
            try mono_buffer.appendSlice(output.gpa, "\n\n");
        }
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
fn generateHtmlWrapper(output: *DualOutput, content: *const Content) !void {
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
fn generateHtmlClosing(output: *DualOutput) !void {
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
fn writeHtmlFile(gpa: Allocator, snapshot_path: []const u8, html_buffer: *std.ArrayList(u8)) !void {
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
    var html_file = std.fs.cwd().createFile(html_path, .{}) catch |err| {
        log("failed to create HTML file '{s}': {s}", .{ html_path, @errorName(err) });
        return;
    };
    defer html_file.close();
    var html_writer_buffer: [4096]u8 = undefined;
    var html_writer = html_file.writer(&html_writer_buffer);
    try html_writer.interface.writeAll(html_buffer.items);
    try html_writer.interface.flush();

    log("generated HTML version: {s}", .{html_path});
}

/// New unified processSnapshotFile function that generates both markdown and HTML simultaneously
fn processSnapshotFileUnified(gpa: Allocator, snapshot_path: []const u8, config: *const Config) !bool {
    // Log the file path that was written to
    log("processing snapshot file: {s}", .{snapshot_path});

    const @"1Mb" = 1024 * 1024;
    const file_content = std.fs.cwd().readFileAlloc(gpa, snapshot_path, @"1Mb") catch |err| {
        std.log.err("failed to read file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer gpa.free(file_content);

    // Check our file starts with the metadata section
    if (!std.mem.startsWith(u8, file_content, "# META")) {
        std.log.err("file '{s}' is not a valid snapshot file", .{snapshot_path});
        std.log.err("snapshot files must start with '# META'", .{});
        if (file_content.len > 0) {
            const first_line_end = std.mem.indexOfScalar(u8, file_content, '\n') orelse @min(file_content.len, 50);
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
fn writeCorpusFile(gpa: Allocator, path: []const u8, source: []const u8, rng: std.Random) !void {
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

    var corpus_file = std.fs.cwd().createFile(corpus_file_path, .{}) catch |err| {
        std.log.err("failed to create file in '{s}': {s}", .{ path, @errorName(err) });
        return;
    };
    defer corpus_file.close();

    var write_buffer: [4096]u8 = undefined;
    var corpus_writer = corpus_file.writer(&write_buffer);
    const writer = &corpus_writer.interface;
    try writer.writeAll(source);
    try writer.flush();
}

fn processSnapshotFile(gpa: Allocator, snapshot_path: []const u8, config: *const Config) !bool {
    return processSnapshotFileUnified(gpa, snapshot_path, config);
}

/// Extracts the sections from a snapshot file
pub fn extractSections(gpa: Allocator, content: []const u8) !Content {
    var ranges = std.AutoHashMap(Section, Section.Range).init(gpa);
    defer ranges.deinit();

    // Find all section headers and their positions
    var idx: usize = 0;
    while (idx < content.len) {
        // Look for section headers
        if (idx == 0 or (idx > 0 and content[idx - 1] == '\n')) {
            if (Section.fromString(content[idx..])) |section| {
                // Only process META, SOURCE, OUTPUT, EXPECTED, and DEV_OUTPUT sections
                if (section == .meta or section == .source or section == .expected or section == .output or section == .dev_output) {
                    // Determine header length - for multi-file SOURCE (no ~~~roc after # SOURCE),
                    // the header is just "# SOURCE\n"
                    const is_multi_file_source = section == .source and Section.isMultiFileSource(content[idx..]);
                    const header_len = if (is_multi_file_source) Section.SOURCE_MULTI.len else section.asString().len;
                    const start = idx + header_len;

                    // Find the end of this section
                    var end = content.len;

                    // For sections with ~~~ delimiters (META, single-file SOURCE, DEV_OUTPUT)
                    if (section == .meta or (section == .source and !is_multi_file_source) or section == .dev_output) {
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
fn parseMultiFileSource(allocator: Allocator, source_text: []const u8) ![]SourceFile {
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
            const name_end = std.mem.indexOfScalarPos(u8, source_text, name_start, '\n') orelse source_text.len;
            const filename = std.mem.trim(u8, source_text[name_start..name_end], " \t\r");
            idx = name_end;

            // Find ~~~roc block
            const roc_marker = "~~~roc\n";
            if (std.mem.indexOfPos(u8, source_text, idx, roc_marker)) |roc_start| {
                const content_start = roc_start + roc_marker.len;
                // Find closing ~~~
                if (std.mem.indexOfPos(u8, source_text, content_start, "~~~")) |content_end| {
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
    const existing_trimmed = std.mem.trimRight(u8, existing, " \t\r\n");
    const new_trimmed = std.mem.trimRight(u8, new, " \t\r\n");
    return std.mem.eql(u8, existing_trimmed, new_trimmed);
}

/// Print a table showing which targets have hash mismatches.
fn printHashMismatchTable(existing: []const u8, new: []const u8) void {
    std.debug.print("  {s:<18} | {s:<64} | {s}\n", .{ "target", "expected", "actual" });
    std.debug.print("  {s:-<18} | {s:-<64} | {s:-<64}\n", .{ "", "", "" });

    // Parse existing hashes into a map-like iteration
    var new_lines = std.mem.splitScalar(u8, std.mem.trimRight(u8, new, " \t\r\n"), '\n');
    var existing_lines = std.mem.splitScalar(u8, std.mem.trimRight(u8, existing, " \t\r\n"), '\n');

    while (new_lines.next()) |new_line| {
        const new_eq = std.mem.indexOfScalar(u8, new_line, '=') orelse continue;
        const target = new_line[0..new_eq];
        const new_hash = new_line[new_eq + 1 ..];

        // Find matching target in existing
        existing_lines.reset();
        var old_hash: ?[]const u8 = null;
        while (existing_lines.next()) |existing_line| {
            const ex_eq = std.mem.indexOfScalar(u8, existing_line, '=') orelse continue;
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

/// Process a dev_object snapshot: parse multi-file source, compile with BuildEnv,
/// lower to Mono IR, cross-compile for all targets, and record blake3 hashes.
fn processDevObjectSnapshot(
    allocator: Allocator,
    content: Content,
    output_path: []const u8,
    config: *const Config,
) !bool {
    log("Processing dev_object snapshot: {s}", .{output_path});

    // 1. Parse multi-file source
    const source_files = try parseMultiFileSource(allocator, content.source);
    defer allocator.free(source_files);

    if (source_files.len == 0) {
        std.log.err("dev_object snapshot has no source files (need ## filename.roc sub-headings)", .{});
        return false;
    }

    // 2. Write source files to a temp directory
    var tmp_dir_name_buf: [256]u8 = undefined;
    const tmp_dir_name = std.fmt.bufPrint(&tmp_dir_name_buf, "/tmp/roc_snapshot_dev_{d}", .{
        @as(u64, @intCast(@intFromPtr(output_path.ptr))),
    }) catch return false;

    std.fs.cwd().makePath(tmp_dir_name) catch |err| {
        std.log.err("Failed to create temp directory {s}: {}", .{ tmp_dir_name, err });
        return false;
    };
    defer std.fs.cwd().deleteTree(tmp_dir_name) catch {};

    // Find the app file (first .roc file, or explicitly "app.roc")
    var app_filename: ?[]const u8 = null;
    for (source_files) |sf| {
        const sub_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir_name, sf.filename });
        defer allocator.free(sub_path);
        std.fs.cwd().writeFile(.{
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

    const app_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir_name, app_filename.? });
    defer allocator.free(app_path);

    // 3. Build with BuildEnv
    const BuildEnv = compile.BuildEnv;
    const native_target = roc_target.RocTarget.detectNative();

    var build_env = BuildEnv.init(allocator, .single_threaded, 1, native_target) catch |err| {
        std.log.err("Failed to init BuildEnv: {}", .{err});
        return false;
    };
    defer build_env.deinit();

    build_env.build(app_path) catch |err| {
        std.log.err("BuildEnv.build failed for {s}: {}", .{ app_path, err });
        return false;
    };

    // Get compiled modules
    const modules = build_env.getCompiledModules(allocator) catch |err| {
        std.log.err("Failed to get compiled modules: {}", .{err});
        return false;
    };
    defer allocator.free(modules);

    if (modules.len == 0) {
        std.log.err("No modules were compiled", .{});
        return false;
    }

    // Find platform and app modules
    const platform_idx = BuildEnv.findPrimaryModuleIndex(modules) orelse {
        std.log.err("No platform module found", .{});
        return false;
    };
    const platform_module = modules[platform_idx];

    // 4. Build module envs array (Builtin first)
    const builtin_env = build_env.builtin_modules.builtin_module.env;
    var all_module_envs = try allocator.alloc(*ModuleEnv, modules.len + 1);
    defer allocator.free(all_module_envs);
    all_module_envs[0] = builtin_env;
    for (modules, 0..) |mod, i| {
        all_module_envs[i + 1] = mod.env;
    }

    // Re-resolve imports
    for (all_module_envs[1..]) |module| {
        module.imports.resolveImports(module, all_module_envs);
    }

    const compiled_module_envs = all_module_envs[1..];

    // 5. Closure pipeline
    for (compiled_module_envs) |module| {
        if (!module.is_lambda_lifted) {
            var top_level_patterns = std.AutoHashMap(can.CIR.Pattern.Idx, void).init(allocator);
            defer top_level_patterns.deinit();

            const stmts = module.store.sliceStatements(module.all_statements);
            for (stmts) |stmt_idx| {
                const stmt = module.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        top_level_patterns.put(decl.pattern, {}) catch {};
                    },
                    else => {},
                }
            }

            var lifter = can.LambdaLifter.init(allocator, module, &top_level_patterns);
            defer lifter.deinit();
            module.is_lambda_lifted = true;
        }
    }

    // Lambda set inference
    var lambda_inference = can.LambdaSetInference.init(allocator);
    defer lambda_inference.deinit();

    var mutable_envs = try allocator.alloc(*ModuleEnv, compiled_module_envs.len);
    defer allocator.free(mutable_envs);
    for (compiled_module_envs, 0..) |env, i| {
        mutable_envs[i] = env;
    }
    lambda_inference.inferAll(mutable_envs) catch {
        std.log.err("Lambda set inference failed", .{});
        return false;
    };

    // Closure transformer
    for (mutable_envs) |module| {
        if (!module.is_defunctionalized) {
            var transformer = can.ClosureTransformer.initWithInference(allocator, module, &lambda_inference);
            defer transformer.deinit();
            module.is_defunctionalized = true;
        }
    }

    // 6. Process hosted functions
    const mono = @import("mono");
    var hosted_functions = mono.Lower.HostedFunctionMap.init(allocator);
    defer hosted_functions.deinit();

    {
        const HostedCompiler = can.HostedCompiler;
        var all_hosted_fns = std.ArrayList(HostedCompiler.HostedFunctionInfo).empty;
        defer all_hosted_fns.deinit(allocator);

        for (modules) |mod| {
            if (!mod.is_platform_sibling) continue;

            var module_fns = HostedCompiler.collectAndSortHostedFunctions(mod.env) catch continue;
            defer module_fns.deinit(mod.env.gpa);

            for (module_fns.items) |fn_info| {
                const name_copy = allocator.dupe(u8, fn_info.name_text) catch continue;
                mod.env.gpa.free(fn_info.name_text);
                all_hosted_fns.append(allocator, .{
                    .symbol_name = fn_info.symbol_name,
                    .expr_idx = fn_info.expr_idx,
                    .name_text = name_copy,
                }) catch {
                    allocator.free(name_copy);
                    continue;
                };
            }
        }

        if (all_hosted_fns.items.len > 0) {
            const SortContext = struct {
                pub fn lessThan(_: void, a: HostedCompiler.HostedFunctionInfo, b: HostedCompiler.HostedFunctionInfo) bool {
                    return std.mem.order(u8, a.name_text, b.name_text) == .lt;
                }
            };
            std.mem.sort(HostedCompiler.HostedFunctionInfo, all_hosted_fns.items, {}, SortContext.lessThan);

            // Deduplicate
            var write_idx: usize = 0;
            for (all_hosted_fns.items, 0..) |fn_info, read_idx| {
                if (write_idx == 0 or !std.mem.eql(u8, all_hosted_fns.items[write_idx - 1].name_text, fn_info.name_text)) {
                    if (write_idx != read_idx) {
                        all_hosted_fns.items[write_idx] = fn_info;
                    }
                    write_idx += 1;
                } else {
                    allocator.free(fn_info.name_text);
                }
            }
            all_hosted_fns.shrinkRetainingCapacity(write_idx);

            // Register hosted functions
            for (modules, 0..) |mod, global_module_idx| {
                if (!mod.is_platform_sibling) continue;
                const plat_env = mod.env;

                const mod_all_defs = plat_env.store.sliceDefs(plat_env.all_defs);
                for (mod_all_defs) |def_idx| {
                    const def = plat_env.store.getDef(def_idx);
                    const expr = plat_env.store.getExpr(def.expr);

                    if (expr == .e_hosted_lambda) {
                        const hosted = expr.e_hosted_lambda;
                        const local_name = plat_env.getIdent(hosted.symbol_name);
                        const plat_module_name = base.module_path.getModuleName(plat_env.module_name);
                        const qualified_name = std.fmt.allocPrint(allocator, "{s}.{s}", .{ plat_module_name, local_name }) catch continue;
                        defer allocator.free(qualified_name);

                        const stripped_name = if (std.mem.endsWith(u8, qualified_name, "!"))
                            qualified_name[0 .. qualified_name.len - 1]
                        else
                            qualified_name;

                        for (all_hosted_fns.items, 0..) |fn_info, idx| {
                            if (std.mem.eql(u8, fn_info.name_text, stripped_name)) {
                                const hosted_index: u32 = @intCast(idx);
                                const expr_node_idx = @as(@TypeOf(plat_env.store.nodes).Idx, @enumFromInt(@intFromEnum(def.expr)));
                                var expr_node = plat_env.store.nodes.get(expr_node_idx);
                                var payload = expr_node.getPayload().expr_hosted_lambda;
                                payload.index = hosted_index;
                                expr_node.setPayload(.{ .expr_hosted_lambda = payload });
                                plat_env.store.nodes.set(expr_node_idx, expr_node);

                                const mod_idx: u16 = @intCast(global_module_idx + 1);
                                hosted_functions.put(mono.Lower.hostedFunctionKey(mod_idx, @intFromEnum(def_idx)), hosted_index) catch {};
                                hosted_functions.put(mono.Lower.hostedFunctionKey(mod_idx, @intFromEnum(def.pattern)), hosted_index) catch {};
                                hosted_functions.put(mono.Lower.hostedFunctionKey(mod_idx, @intFromEnum(def.expr)), hosted_index) catch {};
                                break;
                            }
                        }
                    }
                }
            }

            for (all_hosted_fns.items) |fn_info| {
                allocator.free(fn_info.name_text);
            }
        }
    }

    // 7. Create layout store
    const layout_mod = @import("layout");
    const builtin_str = if (all_module_envs.len > 0) all_module_envs[0].idents.builtin_str else null;

    var layout_store = layout_mod.Store.init(all_module_envs, builtin_str, allocator, base.target.TargetUsize.native) catch {
        std.log.err("Failed to create layout store", .{});
        return false;
    };
    defer layout_store.deinit();

    // 8. Find app module index and lower to Mono IR
    var app_module_idx: ?u32 = null;
    for (modules, 0..) |mod, i| {
        if (mod.is_app) {
            app_module_idx = @intCast(i + 1);
            break;
        }
    }

    var mono_store = mono.MonoExprStore.init(allocator);
    defer mono_store.deinit();

    var lowerer = mono.Lower.init(allocator, &mono_store, all_module_envs, &lambda_inference, &layout_store, app_module_idx, &hosted_functions);
    defer lowerer.deinit();

    // Use provides entries from build pipeline (centralized in CompiledModuleInfo)
    const backend = @import("backend");
    var entrypoints = std.ArrayList(backend.Entrypoint).empty;
    defer {
        for (entrypoints.items) |ep| {
            allocator.free(ep.symbol_name);
        }
        entrypoints.deinit(allocator);
    }

    const provides_entries = platform_module.provides_entries;
    if (provides_entries.len == 0) {
        std.log.err("No provides entries found in platform module", .{});
        return false;
    }

    // Match provides entries to platform defs and lower them
    const platform_defs = platform_module.env.store.sliceDefs(platform_module.env.all_defs);
    for (provides_entries) |entry| {
        for (platform_defs) |def_idx| {
            const def = platform_module.env.store.getDef(def_idx);
            const pattern = platform_module.env.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |assign| {
                    const ident_name = platform_module.env.getIdent(assign.ident);
                    if (std.mem.eql(u8, ident_name, entry.roc_ident)) {
                        const mono_expr_id = lowerer.lowerExpr(@intCast(platform_idx + 1), def.expr) catch continue;
                        const type_var = can.ModuleEnv.varFrom(def.expr);
                        var scope = types.TypeScope.init(allocator);
                        defer scope.deinit();
                        const ret_layout = layout_store.fromTypeVar(@intCast(platform_idx + 1), type_var, &scope, null) catch continue;

                        const symbol_name = std.fmt.allocPrint(allocator, "roc__{s}", .{entry.ffi_symbol}) catch continue;
                        entrypoints.append(allocator, .{
                            .symbol_name = symbol_name,
                            .body_expr = mono_expr_id,
                            .arg_layouts = &[_]layout_mod.Idx{},
                            .ret_layout = ret_layout,
                        }) catch continue;
                        break;
                    }
                },
                else => {},
            }
        }
    }

    if (entrypoints.items.len == 0) {
        std.log.err("No entrypoints found in platform module", .{});
        return false;
    }

    // 9. RC insertion
    var rc_pass = try mono.RcInsert.RcInsertPass.init(allocator, &mono_store, &layout_store);
    defer rc_pass.deinit();

    for (entrypoints.items) |*ep| {
        ep.body_expr = rc_pass.insertRcOps(ep.body_expr) catch ep.body_expr;
    }

    const procs = mono_store.getProcs();

    // 10. Cross-compile for all targets and hash
    const RocTarget = roc_target.RocTarget;
    const Blake3 = std.crypto.hash.Blake3;
    const roc_target_fields = @typeInfo(RocTarget).@"enum".fields;

    var hash_results: [roc_target_fields.len]TargetHashResult = undefined;

    var object_compiler = backend.ObjectFileCompiler.init(allocator);

    inline for (roc_target_fields, 0..) |field, i| {
        const target: RocTarget = @enumFromInt(field.value);
        hash_results[i].target_name = field.name;

        const arch = target.toCpuArch();
        if (arch == .x86_64 or arch == .aarch64 or arch == .aarch64_be) {
            if (object_compiler.compileToObjectFile(
                &mono_store,
                &layout_store,
                entrypoints.items,
                procs,
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
        } else {
            hash_results[i].hash_hex = undefined;
            hash_results[i].supported = false;
        }
    }

    // 11. Generate output file
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

    // MONO section - emit CIR representation of all module defs
    try md_writer.writer.writeAll(Section.MONO);
    {
        for (modules) |mod| {
            const mod_env = mod.env;
            const mod_name = base.module_path.getModuleName(mod_env.module_name);

            var emitter = can.RocEmitter.init(allocator, mod_env);
            defer emitter.deinit();

            const defs = mod_env.store.sliceDefs(mod_env.all_defs);
            if (defs.len == 0) continue;

            // Module header comment
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
    }
    try md_writer.writer.writeAll(Section.SECTION_END);

    // DEV OUTPUT section - build new hash text
    var new_hash_buf = std.ArrayList(u8).empty;
    defer new_hash_buf.deinit(allocator);
    for (&hash_results) |result| {
        new_hash_buf.appendSlice(allocator, result.target_name) catch return false;
        new_hash_buf.append(allocator, '=') catch return false;
        if (result.supported) {
            new_hash_buf.appendSlice(allocator, &result.hash_hex) catch return false;
        } else {
            new_hash_buf.appendSlice(allocator, "NOT_IMPLEMENTED") catch return false;
        }
        new_hash_buf.append(allocator, '\n') catch return false;
    }
    const new_hash_text = new_hash_buf.items;

    // Compare against existing DEV OUTPUT and decide what to write
    var success = true;
    const write_new_hashes = blk: {
        if (content.dev_output == null) {
            // First run - always write new hashes
            break :blk true;
        }
        switch (config.expected_section_command) {
            .update => break :blk true,
            .check => {
                if (!hashTextMatches(content.dev_output.?, new_hash_text)) {
                    std.debug.print("\nDEV OUTPUT mismatch in {s}\n\n", .{output_path});
                    printHashMismatchTable(content.dev_output.?, new_hash_text);
                    std.debug.print("\nHint: use `zig build snapshot -- --update-expected` to update DEV OUTPUT hashes.\n\n", .{});
                    success = false;
                }
                break :blk false;
            },
            .none => {
                if (!hashTextMatches(content.dev_output.?, new_hash_text)) {
                    std.debug.print("\nDEV OUTPUT warning: hashes changed in {s}\n", .{output_path});
                    std.debug.print("Hint: use `zig build snapshot -- --check-expected` to see details, or `--update-expected` to update.\n\n", .{});
                }
                break :blk false;
            },
        }
    };

    try md_writer.writer.writeAll(Section.DEV_OUTPUT);
    if (write_new_hashes) {
        try md_writer.writer.writeAll(new_hash_text);
    } else {
        // Preserve existing DEV OUTPUT content
        try md_writer.writer.writeAll(content.dev_output.?);
        // Ensure trailing newline
        if (content.dev_output.?.len > 0 and content.dev_output.?[content.dev_output.?.len - 1] != '\n') {
            try md_writer.writer.writeByte('\n');
        }
    }
    try md_writer.writer.writeAll(Section.SECTION_END);

    // Transfer from writer to buffer
    md_buffer = md_writer.toArrayList();

    // Write the output file
    const md_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
        std.log.err("Failed to create {s}: {}", .{ output_path, err });
        return false;
    };
    defer md_file.close();

    try md_file.writeAll(md_buffer.items);
    return success;
}

// REPL Snapshot Processing

fn processReplSnapshot(allocator: Allocator, content: Content, output_path: []const u8, config: *const Config) !bool {
    var success = true;
    log("Processing REPL snapshot: {s}", .{output_path});

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

    // Generate all sections
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    success = try generateReplOutputSection(&output, output_path, &content, config) and success;
    try generateReplProblemsSection(&output, &content);
    try generateHtmlClosing(&output);

    // Transfer contents from writer back to buffer before writing
    md_buffer_unmanaged = md_writer_allocating.toArrayList();
    if (html_writer_allocating) |*hw| html_buffer_unmanaged.? = hw.toArrayList();

    if (!config.disable_updates) {
        // Write the markdown file
        const md_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
            std.log.err("Failed to create {s}: {}", .{ output_path, err });
            return false;
        };
        defer md_file.close();

        try md_file.writeAll(md_buffer_unmanaged.items);

        if (html_buffer_unmanaged) |*buf| {
            writeHtmlFile(allocator, output_path, buf) catch |err| {
                warn("Failed to write HTML file for {s}: {}", .{ output_path, err });
            };
        }
    }

    return success;
}

fn generateReplOutputSection(output: *DualOutput, snapshot_path: []const u8, content: *const Content, config: *const Config) !bool {
    var success = true;
    // Parse REPL inputs from the source using » as delimiter
    var inputs = std.array_list.Managed([]const u8).init(output.gpa);
    defer inputs.deinit();

    // Split by the » character, each section is a separate REPL input
    var parts = std.mem.splitSequence(u8, content.source, "»");

    // Skip the first part (before the first »)
    _ = parts.next();

    while (parts.next()) |part| {
        // Trim whitespace and newlines
        const trimmed = std.mem.trim(u8, part, " \t\r\n");
        if (trimmed.len > 0) {
            try inputs.append(trimmed);
        }
    }

    var snapshot_ops = SnapshotOps.init(output.gpa);
    defer snapshot_ops.deinit();

    // Initialize REPL
    var repl_instance = try Repl.init(output.gpa, snapshot_ops.get_ops(), snapshot_ops.crashContextPtr());
    defer repl_instance.deinit();

    // Enable debug snapshots for CAN/TYPES generation
    repl_instance.enableDebugSnapshots();

    // Enable tracing if requested
    // if (config.trace_eval) {
    //     repl_instance.setTraceWriter(stderrWriter());
    // }

    // Process each input and generate output
    var actual_outputs = std.array_list.Managed([]const u8).init(output.gpa);
    defer {
        for (actual_outputs.items) |item| {
            output.gpa.free(item);
        }
        actual_outputs.deinit();
    }

    for (inputs.items) |input| {
        const repl_output = try repl_instance.step(input);
        try actual_outputs.append(repl_output);
    }

    switch (config.output_section_command) {
        .update => {
            try output.begin_section("OUTPUT");
            // Write actual outputs
            for (actual_outputs.items, 0..) |repl_output, i| {
                if (i > 0) {
                    try output.md_writer.writer.writeAll("---\n");
                }
                try output.md_writer.writer.writeAll(repl_output);
                try output.md_writer.writer.writeByte('\n');

                // HTML output
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

            // Compare with expected output if provided
            if (content.output) |expected| {
                try output.begin_section("OUTPUT");
                // Parse expected outputs
                var expected_outputs = std.array_list.Managed([]const u8).init(output.gpa);
                defer expected_outputs.deinit();

                var expected_lines = std.mem.splitSequence(u8, expected, "\n---\n");
                while (expected_lines.next()) |output_str| {
                    const trimmed = std.mem.trim(u8, output_str, " \t\r\n");
                    if (trimmed.len > 0) {
                        try expected_outputs.append(trimmed);
                    }
                }

                // Verify the outputs match
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

                // Write the old outputs back to the file
                for (expected_outputs.items, 0..) |expected_output, i| {
                    if (i > 0) {
                        try output.md_writer.writer.writeAll("---\n");
                    }
                    try output.md_writer.writer.writeAll(expected_output);
                    try output.md_writer.writer.writeByte('\n');

                    // HTML output
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
                // No existing OUTPUT section - generate one for new snapshots
                try output.begin_section("OUTPUT");
                for (actual_outputs.items, 0..) |repl_output, i| {
                    if (i > 0) {
                        try output.md_writer.writer.writeAll("---\n");
                    }
                    try output.md_writer.writer.writeAll(repl_output);
                    try output.md_writer.writer.writeByte('\n');

                    // HTML output
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

                // No validation needed for new snapshots - they should have outputs
            }
        },
    }

    return success;
}

fn generateReplProblemsSection(output: *DualOutput, _: *const Content) !void {
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
    var snapshots_dir = try std.fs.cwd().openDir("test/snapshots", .{ .iterate = true });
    defer snapshots_dir.close();

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
    dir: *std.fs.Dir,
    relative_path: []const u8,
    files_with_builtin: *std.array_list.Managed([]const u8),
) !void {
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        const full_path = if (relative_path.len > 0)
            try std.fmt.allocPrint(allocator, "{s}/{s}", .{ relative_path, entry.name })
        else
            try allocator.dupe(u8, entry.name);
        defer allocator.free(full_path);

        switch (entry.kind) {
            .directory => {
                var subdir = try dir.openDir(entry.name, .{ .iterate = true });
                defer subdir.close();
                try searchDirectoryForBuiltin(allocator, &subdir, full_path, files_with_builtin);
            },
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".md")) {
                    const file = try dir.openFile(entry.name, .{});
                    defer file.close();

                    const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
                    defer allocator.free(content);

                    // Search for "Builtin" (case-sensitive)
                    if (std.mem.indexOf(u8, content, "Builtin")) |_| {
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

test "LambdaLifter" {
    std.testing.refAllDecls(LambdaLifter);
}

/// An implementation of RocOps for snapshot testing.
pub const SnapshotOps = struct {
    allocator: std.mem.Allocator,
    crash: CrashContext,
    roc_ops: RocOps,

    pub fn init(allocator: std.mem.Allocator) SnapshotOps {
        return SnapshotOps{
            .allocator = allocator,
            .crash = CrashContext.init(allocator),
            .roc_ops = RocOps{
                .env = undefined, // will be set below
                .roc_alloc = snapshotRocAlloc,
                .roc_dealloc = snapshotRocDealloc,
                .roc_realloc = snapshotRocRealloc,
                .roc_dbg = snapshotRocDbg,
                .roc_expect_failed = snapshotRocExpectFailed,
                .roc_crashed = snapshotRocCrashed,
                .hosted_fns = .{ .count = 0, .fns = undefined }, // Not used in snapshots
            },
        };
    }

    pub fn deinit(self: *SnapshotOps) void {
        self.crash.deinit();
    }

    pub fn get_ops(self: *SnapshotOps) *RocOps {
        self.roc_ops.env = @ptrCast(self);
        self.crash.reset();
        return &self.roc_ops;
    }

    pub fn crashContextPtr(self: *SnapshotOps) *CrashContext {
        return &self.crash;
    }
};

fn snapshotRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const snapshot_env: *SnapshotOps = @ptrCast(@alignCast(env));

    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = snapshot_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        std.debug.panic("Out of memory during snapshotRocAlloc", .{});
    };

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn snapshotRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const snapshot_env: *SnapshotOps = @ptrCast(@alignCast(env));

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));

    // Read the total size from metadata
    const total_size = size_ptr.*;

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);

    // Calculate alignment
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    snapshot_env.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn snapshotRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const snapshot_env: *SnapshotOps = @ptrCast(@alignCast(env));

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = realloc_args.new_length + size_storage_bytes;

    // Perform reallocation
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = snapshot_env.allocator.realloc(old_slice, new_total_size) catch {
        std.debug.panic("Out of memory during snapshotRocRealloc", .{});
    };

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn snapshotRocDbg(_: *const RocDbg, _: *anyopaque) callconv(.c) void {
    @panic("snapshotRocDbg not implemented yet");
}

fn snapshotRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const snapshot_env: *SnapshotOps = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    // Format and record the message
    const formatted = std.fmt.allocPrint(snapshot_env.allocator, "Expect failed: {s}", .{trimmed}) catch {
        std.debug.panic("failed to allocate snapshot expect failure message", .{});
    };
    snapshot_env.crash.recordCrash(formatted) catch |err| {
        snapshot_env.allocator.free(formatted);
        std.debug.panic("failed to store snapshot expect failure: {}", .{err});
    };
}

fn snapshotRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const snapshot_env: *SnapshotOps = @ptrCast(@alignCast(env));
    snapshot_env.crash.recordCrash(crashed_args.utf8_bytes[0..crashed_args.len]) catch |err| {
        std.debug.panic("failed to store snapshot crash message: {}", .{err});
    };
}
