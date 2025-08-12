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

const Repl = @import("repl").Repl;
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
const Allocator = std.mem.Allocator;
const SExprTree = base.SExprTree;
const CacheModule = compile.CacheModule;
const AST = parse.AST;
const Report = reporting.Report;
const types_problem_mod = check.problem;
const tokenize = parse.tokenize;
const parallel = base.parallel;

var verbose_log: bool = false;
var prng = std.Random.DefaultPrng.init(1234567890);

const rand = prng.random();

/// Logs a message if verbose logging is enabled.
fn log(comptime fmt_str: []const u8, args: anytype) void {
    if (verbose_log) {
        std.log.info(fmt_str, args);
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
fn parseProblemsSection(allocator: std.mem.Allocator, content: []const u8) !std.ArrayList(ProblemEntry) {
    var problems = std.ArrayList(ProblemEntry).init(allocator);
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

    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    for (problems, 0..) |problem, i| {
        if (i > 0) {
            try buffer.append('\n');
        }
        try problem.format(buffer.writer());
    }

    return buffer.toOwnedSlice();
}

/// Parse an EXPECTED line like "UNEXPECTED TOKEN IN EXPRESSION - record_field_update_error.md:1:10:1:15"
fn parseExpectedLine(allocator: std.mem.Allocator, line: []const u8) !?ProblemEntry {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) {
        return null;
    }

    // Find the separator " - "
    const separator = " - ";
    const sep_index = std.mem.indexOf(u8, line, separator) orelse return null;

    const problem_type = std.mem.trim(u8, line[0..sep_index], " \t");
    const location = std.mem.trim(u8, line[sep_index + separator.len ..], " \t\r\n");

    // Handle special case where location is just ":0:0:0:0" (no file)
    if (std.mem.startsWith(u8, location, ":")) {
        // Skip the first colon
        var parts = std.mem.tokenizeScalar(u8, location[1..], ':');

        const start_line_str = parts.next() orelse return null;
        const start_col_str = parts.next() orelse return null;
        const end_line_str = parts.next() orelse return null;
        const end_col_str = parts.next() orelse return null;

        return ProblemEntry{
            .problem_type = try allocator.dupe(u8, problem_type),
            .file = try allocator.dupe(u8, ""),
            .start_line = try std.fmt.parseInt(u32, start_line_str, 10),
            .start_col = try std.fmt.parseInt(u32, start_col_str, 10),
            .end_line = try std.fmt.parseInt(u32, end_line_str, 10),
            .end_col = try std.fmt.parseInt(u32, end_col_str, 10),
        };
    }

    // Parse location "file.md:start_line:start_col:end_line:end_col"
    var parts = std.mem.tokenizeScalar(u8, location, ':');

    const file = parts.next() orelse return null;
    const start_line_str = parts.next() orelse return null;
    const start_col_str = parts.next() orelse return null;
    const end_line_str = parts.next() orelse return null;
    const end_col_str = parts.next() orelse return null;

    return ProblemEntry{
        .problem_type = try allocator.dupe(u8, problem_type),
        .file = try allocator.dupe(u8, file),
        .start_line = try std.fmt.parseInt(u32, start_line_str, 10),
        .start_col = try std.fmt.parseInt(u32, start_col_str, 10),
        .end_line = try std.fmt.parseInt(u32, end_line_str, 10),
        .end_col = try std.fmt.parseInt(u32, end_col_str, 10),
    };
}

/// Parse all problems from EXPECTED section
fn parseExpectedSection(allocator: std.mem.Allocator, content: []const u8) !std.ArrayList(ProblemEntry) {
    var problems = std.ArrayList(ProblemEntry).init(allocator);
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
        // NIL means we expect no problems
        return problems;
    }

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const problem = try parseExpectedLine(allocator, line);
        if (problem) |p| {
            try problems.append(p);
        }
    }

    return problems;
}

/// Compare two problem entries for equality
fn problemsEqual(a: ProblemEntry, b: ProblemEntry) bool {
    return std.mem.eql(u8, a.problem_type, b.problem_type) and
        std.mem.eql(u8, a.file, b.file) and
        a.start_line == b.start_line and
        a.start_col == b.start_col and
        a.end_line == b.end_line and
        a.end_col == b.end_col;
}

/// Generate all reports from the compilation pipeline
fn generateAllReports(
    allocator: std.mem.Allocator,
    parse_ast: *AST,
    can_ir: *ModuleEnv,
    solver: *Check,
    snapshot_path: []const u8,
    module_env: *ModuleEnv,
) !std.ArrayList(reporting.Report) {
    var reports = std.ArrayList(reporting.Report).init(allocator);
    errdefer reports.deinit();

    // Generate tokenize reports
    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        const report = parse_ast.tokenizeDiagnosticToReport(diagnostic, allocator) catch |err| {
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
        var report_builder = types_problem_mod.ReportBuilder.init(
            allocator,
            module_env,
            can_ir,
            &solver.snapshots,
            snapshot_path,
            empty_modules,
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
fn renderReportsToProblemsSection(output: *DualOutput, reports: *const std.ArrayList(reporting.Report)) !void {
    // HTML PROBLEMS section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="problems">
        );
    }

    if (reports.items.len == 0) {
        try output.md_writer.writeAll("NIL\n");
        if (output.html_writer) |writer| {
            try writer.writeAll("                    <p>NIL</p>\n");
        }
        log("reported NIL problems", .{});
    } else {
        // Render all reports in order
        for (reports.items) |report| {
            report.render(output.md_writer.any(), .markdown) catch |err| {
                std.debug.panic("Failed to render report: {s}", .{@errorName(err)});
            };

            if (output.html_writer) |writer| {
                try writer.writeAll("                    <div class=\"problem\">");
                report.render(writer.any(), .markdown) catch |err| {
                    std.debug.panic("Failed to render report to HTML: {s}", .{@errorName(err)});
                };
                try writer.writeAll("</div>\n");
            }
        }
    }

    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                </div>
            \\
        );
    }
}

/// Render reports to EXPECTED section format (parsed problem entries)
fn renderReportsToExpectedContent(allocator: std.mem.Allocator, reports: *const std.ArrayList(reporting.Report)) ![]const u8 {
    if (reports.items.len == 0) {
        return try allocator.dupe(u8, "NIL");
    }

    // Render all reports to markdown and then parse the problems
    var problems_buffer = std.ArrayList(u8).init(allocator);
    defer problems_buffer.deinit();

    // Render all reports to markdown
    for (reports.items) |report| {
        report.render(problems_buffer.writer().any(), .markdown) catch |err| {
            std.debug.panic("Failed to render report for EXPECTED: {s}", .{@errorName(err)});
        };
    }

    // Parse the rendered problems and convert to EXPECTED format
    // TODO: rather than parsing markdown, we should directly generate EXPECTED format from the reports
    var parsed_problems = try parseProblemsSection(allocator, problems_buffer.items);
    defer {
        for (parsed_problems.items) |p| {
            allocator.free(p.problem_type);
            allocator.free(p.file);
        }
        parsed_problems.deinit();
    }

    return try generateExpectedContent(allocator, parsed_problems.items);
}

/// Helper function to extract section content only
fn extractSectionContent(content: []const u8, section_name: []const u8) ?[]const u8 {
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "# {s}\n", .{section_name}) catch return null;
    const start_idx = std.mem.indexOf(u8, content, header) orelse return null;
    const content_start = start_idx + header.len;

    // Find the next section header
    var next_section_idx = content.len;
    var search_idx = content_start;
    while (search_idx < content.len - 2) {
        if (content[search_idx] == '\n' and
            content[search_idx + 1] == '#' and
            content[search_idx + 2] == ' ')
        {
            next_section_idx = search_idx + 1;
            break;
        }
        search_idx += 1;
    }

    return std.mem.trim(u8, content[content_start..next_section_idx], " \t\r\n");
}

/// Helper function to get section info with start/end positions
fn extractSectionInfo(content: []const u8, section_name: []const u8) ?struct { start: usize, end: usize } {
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "# {s}\n", .{section_name}) catch return null;
    const start_idx = std.mem.indexOf(u8, content, header) orelse return null;

    // Find the next section header
    var next_section_idx = content.len;
    var search_idx = start_idx + header.len;
    while (search_idx < content.len - 2) {
        if (content[search_idx] == '\n' and
            content[search_idx + 1] == '#' and
            content[search_idx + 2] == ' ')
        {
            next_section_idx = search_idx + 1;
            break;
        }
        search_idx += 1;
    }

    return .{ .start = start_idx, .end = next_section_idx };
}

/// cli entrypoint for snapshot tool
pub fn main() !void {
    // Use GeneralPurposeAllocator for command-line parsing and general work
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    var snapshot_paths = std.ArrayList([]const u8).init(gpa);
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

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_log = true;
        } else if (std.mem.eql(u8, arg, "--html")) {
            generate_html = true;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_mode = true;
        } else if (std.mem.eql(u8, arg, "--trace-eval")) {
            trace_eval = true;
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
                \\  --threads <n>   Number of threads to use (0 = auto-detect, 1 = single-threaded). Default: 0.
                \\  --check-expected     Validate that EXPECTED sections match PROBLEMS sections
                \\  --update-expected    Update EXPECTED sections based on PROBLEMS sections
                \\  --fuzz-corpus <path>  Specify the path to the fuzz corpus
                \\
                \\Arguments:
                \\  snapshot_paths  Paths to snapshot files or directories
            ;
            std.log.info(usage, .{});
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

    const config = Config{
        .maybe_fuzz_corpus_path = maybe_fuzz_corpus_path,
        .generate_html = generate_html,
        .expected_section_command = expected_section_command,
        .output_section_command = output_section_command,
        .trace_eval = trace_eval,
    };

    if (config.maybe_fuzz_corpus_path != null) {
        log("copying SOURCE from snapshots to: {s}", .{config.maybe_fuzz_corpus_path.?});
        try std.fs.cwd().makePath(config.maybe_fuzz_corpus_path.?);
    }
    const snapshots_dir = "test/snapshots";
    var timer = std.time.Timer.start() catch unreachable;

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

    const collect_duration_ms = timer.read() / std.time.ns_per_ms;
    log("collected {d} work items in {d} ms", .{ work_list.items.len, collect_duration_ms });

    // Stage 2: Process work items (in parallel or single-threaded)
    const result = try processWorkItems(gpa, work_list, max_threads, debug_mode, &config);

    const duration_ms = timer.read() / std.time.ns_per_ms;

    std.log.info(
        "collected {d} items in {d} ms, processed {d} snapshots in {d} ms.",
        .{ work_list.items.len, collect_duration_ms, result.success, duration_ms },
    );

    if (result.failed > 0) {
        std.log.err("Failed to process {d} snapshots.", .{result.failed});
        std.process.exit(1);
    }
}

fn checkSnapshotExpectations(gpa: Allocator) !bool {
    const config = Config{
        .maybe_fuzz_corpus_path = null,
        .generate_html = false,
        .expected_section_command = .check,
        .output_section_command = .check,
        .disable_updates = true,
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
        var files_to_delete = std.ArrayList([]u8).init(allocator);
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

fn processRocFileAsSnapshot(
    allocator: Allocator,
    output_path: []const u8,
    roc_content: []const u8,
    meta: Meta,
    config: *const Config,
) !bool {
    // Try to read existing EXPECTED section if the file exists
    var expected_content: ?[]const u8 = null;
    defer if (expected_content) |content| allocator.free(content);

    if (std.fs.cwd().readFileAlloc(allocator, output_path, 1024 * 1024)) |existing_content| {
        defer allocator.free(existing_content);

        // Extract EXPECTED section manually since extractSections is defined later
        const expected_header = "# EXPECTED\n";
        if (std.mem.indexOf(u8, existing_content, expected_header)) |start_idx| {
            const content_start = start_idx + expected_header.len;

            // Find the next section header
            var end_idx = existing_content.len;
            var search_idx = content_start;
            while (search_idx < existing_content.len - 2) {
                if (existing_content[search_idx] == '\n' and
                    existing_content[search_idx + 1] == '#' and
                    existing_content[search_idx + 2] == ' ')
                {
                    end_idx = search_idx + 1;
                    break;
                }
                search_idx += 1;
            }

            const expected_section = std.mem.trim(u8, existing_content[content_start..end_idx], " \t\r\n");
            expected_content = try allocator.dupe(u8, expected_section);
        }
    } else |_| {
        // File doesn't exist yet, that's fine
    }

    try processRocFileAsSnapshotWithExpected(allocator, output_path, roc_content, meta, expected_content, config);
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

    // Process the content through the compilation pipeline
    var module_env = try ModuleEnv.init(allocator, content.source);
    defer module_env.deinit();

    // Calculate line starts for source location tracking
    try module_env.common.calcLineStarts(allocator);

    // Parse the source code based on node type
    var parse_ast: AST = switch (content.meta.node_type) {
        .file => try parse.parse(&module_env.common, allocator),
        .header => try parse.parseHeader(&module_env.common, allocator),
        .expr => try parse.parseExpr(&module_env.common, allocator),
        .statement => try parse.parseStatement(&module_env.common, allocator),
        .package => try parse.parse(&module_env.common, allocator),
        .platform => try parse.parse(&module_env.common, allocator),
        .app => try parse.parse(&module_env.common, allocator),
        .repl => unreachable, // Handled above
    };
    defer parse_ast.deinit(allocator);

    parse_ast.store.emptyScratch();

    // Extract module name from output path
    const basename = std.fs.path.basename(output_path);
    const module_name = if (std.mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
        basename[0..dot_idx]
    else
        basename;
    var can_ir = &module_env; // ModuleEnv contains the canonical IR
    try can_ir.initCIRFields(allocator, module_name);

    var czer = try Can.init(can_ir, &parse_ast, null);
    defer czer.deinit();

    var maybe_expr_idx: ?Can.CanonicalizedExpr = null;

    switch (content.meta.node_type) {
        .file => try czer.canonicalizeFile(),
        .header => {
            // TODO: implement canonicalize_header when available
        },
        .expr => {
            const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
            maybe_expr_idx = try czer.canonicalizeExpr(expr_idx);
        },
        .statement => {
            // Manually track scratch statements because we aren't using the file entrypoint
            const stmt_idx: AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
            const scratch_statements_start = can_ir.store.scratch_statements.top();
            _ = try czer.canonicalizeStatement(stmt_idx);
            can_ir.all_statements = try can_ir.store.statementSpanFrom(scratch_statements_start);
        },
        .package => try czer.canonicalizeFile(),
        .platform => try czer.canonicalizeFile(),
        .app => try czer.canonicalizeFile(),
        .repl => unreachable, // Handled above
    }

    // Assert that everything is in-sync
    can_ir.debugAssertArraysInSync();

    // Types
    const empty_modules: []const *ModuleEnv = &.{};
    var solver = try Check.init(
        allocator,
        &can_ir.types,
        can_ir,
        empty_modules,
        &can_ir.store.regions,
    );
    defer solver.deinit();

    // Assert that we have regions for every type variable
    solver.debugAssertArraysInSync();

    if (maybe_expr_idx) |expr_idx| {
        _ = try solver.checkExpr(expr_idx.idx);
    } else {
        try solver.checkDefs();
    }

    // Cache round-trip validation - ensure ModuleCache serialization/deserialization works
    {
        // Generate original S-expression for comparison
        var original_tree = SExprTree.init(allocator);
        defer original_tree.deinit();
        try ModuleEnv.pushToSExprTree(can_ir, null, &original_tree);

        var original_sexpr = std.ArrayList(u8).init(allocator);
        defer original_sexpr.deinit();
        try original_tree.toStringPretty(original_sexpr.writer().any());

        // Create arena for serialization
        var cache_arena = std.heap.ArenaAllocator.init(allocator);
        defer cache_arena.deinit();

        // Create and serialize MmapCache
        const cache_data = try CacheModule.create(allocator, cache_arena.allocator(), &module_env, can_ir, 0, 0);
        defer allocator.free(cache_data);

        // Deserialize back
        var loaded_cache = try CacheModule.fromMappedMemory(cache_data);

        // Restore ModuleEnv
        const restored_env = try loaded_cache.restore(allocator, module_name, content.source);
        // Note: restored_env points to data within the cache, so we don't free it

        // Generate S-expression from restored ModuleEnv
        var restored_tree = SExprTree.init(allocator);
        defer restored_tree.deinit();
        try ModuleEnv.pushToSExprTree(restored_env, null, &restored_tree);

        var restored_sexpr = std.ArrayList(u8).init(allocator);
        defer restored_sexpr.deinit();
        try restored_tree.toStringPretty(restored_sexpr.writer().any());

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

    // Buffer all output in memory before writing files
    var md_buffer = std.ArrayList(u8).init(allocator);
    defer md_buffer.deinit();

    var html_buffer = if (config.generate_html) std.ArrayList(u8).init(allocator) else null;
    defer if (html_buffer) |*buf| buf.deinit();

    var output = DualOutput.init(allocator, &md_buffer, if (html_buffer) |*buf| buf else null);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate reports once and use for both EXPECTED and PROBLEMS sections
    var generated_reports = try generateAllReports(allocator, &parse_ast, can_ir, &solver, output_path, &module_env);
    defer {
        for (generated_reports.items) |*report| {
            report.deinit();
        }
        generated_reports.deinit();
    }

    // Generate all sections
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    success = try generateExpectedSection(&output, output_path, &content, &generated_reports, config) and success;
    try generateProblemsSection(&output, &generated_reports);
    try generateTokensSection(&output, &parse_ast, &content, &module_env);
    try generateParseSection(&output, &content, &parse_ast, &module_env.common);
    try generateFormattedSection(&output, &content, &parse_ast);
    try generateCanonicalizeSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx));
    try generateTypesSection(&output, can_ir, Can.CanonicalizedExpr.maybe_expr_get_idx(maybe_expr_idx));

    try generateHtmlClosing(&output);

    if (!config.disable_updates) {
        // Write the markdown file
        const md_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
            std.log.err("Failed to create {s}: {}", .{ output_path, err });
            return false;
        };
        defer md_file.close();

        try md_file.writeAll(md_buffer.items);

        if (html_buffer) |*buf| {
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

const WorkList = std.ArrayList(WorkItem);

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
    } else |dir_err| {
        // Not a directory, try as file
        if (dir_err == error.NotDir) {
            if (isSnapshotFile(canonical_path)) {
                const path_copy = try gpa.dupe(u8, canonical_path);
                try work_list.append(WorkItem{
                    .path = path_copy,
                    .kind = .snapshot_file,
                });
            } else {
                std.log.err("file '{s}' is not a snapshot file (must end with .md)", .{canonical_path});
            }
        } else {
            std.log.err("failed to access path '{s}': {s}", .{ canonical_path, @errorName(dir_err) });
        }
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

    pub const META = "# META\n~~~ini\n";
    pub const SOURCE = "# SOURCE\n~~~roc\n";
    pub const EXPECTED = "# EXPECTED\n";
    pub const OUTPUT = "# OUTPUT\n";
    pub const FORMATTED = "# FORMATTED\n~~~roc\n";
    pub const PARSE = "# PARSE\n~~~clojure\n";
    pub const CANONICALIZE = "# CANONICALIZE\n~~~clojure\n";
    pub const TOKENS = "# TOKENS\n~~~zig\n";
    pub const PROBLEMS = "# PROBLEMS\n";
    pub const TYPES = "# TYPES\n~~~clojure\n";

    pub const SECTION_END = "~~~\n";

    fn fromString(str: []const u8) ?Section {
        if (std.mem.startsWith(u8, str, META)) return .meta;
        if (std.mem.startsWith(u8, str, SOURCE)) return .source;
        if (std.mem.startsWith(u8, str, EXPECTED)) return .expected;
        if (std.mem.startsWith(u8, str, OUTPUT)) return .output;
        if (std.mem.startsWith(u8, str, FORMATTED)) return .formatted;
        if (std.mem.startsWith(u8, str, PARSE)) return .parse;
        if (std.mem.startsWith(u8, str, CANONICALIZE)) return .canonicalize;
        if (std.mem.startsWith(u8, str, TYPES)) return .types;
        if (std.mem.startsWith(u8, str, TOKENS)) return .tokens;
        if (std.mem.startsWith(u8, str, PROBLEMS)) return .problems;
        return null;
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

    pub const HEADER = "header";
    pub const EXPR = "expr";
    pub const STMT = "statement";
    pub const FILE = "file";
    pub const PACKAGE = "package";
    pub const PLATFORM = "platform";
    pub const APP = "app";
    pub const REPL = "repl";

    fn fromString(str: []const u8) !NodeType {
        if (std.mem.eql(u8, str, HEADER)) return .header;
        if (std.mem.eql(u8, str, EXPR)) return .expr;
        if (std.mem.eql(u8, str, STMT)) return .statement;
        if (std.mem.eql(u8, str, FILE)) return .file;
        if (std.mem.eql(u8, str, PACKAGE)) return .package;
        if (std.mem.eql(u8, str, PLATFORM)) return .platform;
        if (std.mem.eql(u8, str, APP)) return .app;
        if (std.mem.eql(u8, str, REPL)) return .repl;
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
        };
    }
};

const Meta = struct {
    description: []const u8,
    node_type: NodeType,

    const DESC_START: []const u8 = "description=";
    const TYPE_START: []const u8 = "type=";

    fn fromString(text: []const u8) Error!Meta {
        var lines = std.mem.splitScalar(u8, text, '\n');
        var desc: []const u8 = "";
        var node_type: NodeType = .file;
        while (true) {
            var line = lines.next() orelse break;
            if (std.mem.startsWith(u8, line, DESC_START)) {
                desc = line[(DESC_START.len)..];
            } else if (std.mem.startsWith(u8, line, TYPE_START)) {
                const ty = line[(TYPE_START.len)..];
                node_type = try NodeType.fromString(ty);
            }
        }

        return .{
            .description = desc,
            .node_type = node_type,
        };
    }

    fn format(self: Meta, writer: anytype) !void {
        try writer.writeAll(DESC_START);
        try writer.writeAll(self.description);
        try writer.writeAll("\n");
        try writer.writeAll(TYPE_START);
        try writer.writeAll(self.node_type.toString());
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
        try std.testing.expectEqual(meta, Error.InvalidNodeType);
    }
};

/// Content of a snapshot file, references the Metadata and Source sections etc
pub const Content = struct {
    meta: Meta,
    source: []const u8,
    expected: ?[]const u8,
    output: ?[]const u8,
    formatted: ?[]const u8,
    has_canonicalize: bool,

    fn from_ranges(ranges: std.AutoHashMap(Section, Section.Range), content: []const u8) Error!Content {
        var source: []const u8 = undefined;
        var expected: ?[]const u8 = undefined;
        var output: ?[]const u8 = undefined;
        var formatted: ?[]const u8 = undefined;
        var has_canonicalize: bool = false;

        if (ranges.get(.source)) |value| {
            source = value.extract(content);
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
    md_writer: std.ArrayList(u8).Writer,
    html_writer: ?std.ArrayList(u8).Writer,
    gpa: Allocator,

    pub fn init(gpa: Allocator, md_buffer: *std.ArrayList(u8), html_buffer: ?*std.ArrayList(u8)) DualOutput {
        return .{
            .md_writer = md_buffer.writer(),
            .html_writer = if (html_buffer) |buf| buf.writer() else null,
            .gpa = gpa,
        };
    }

    fn begin_section(self: *DualOutput, name: []const u8) !void {
        try self.md_writer.print("# {s}\n", .{name});
        if (self.html_writer) |writer| {
            try writer.print(
                \\        <div class="section" data-section="{s}">
                \\            <div class="section-content">
            , .{name});
        }
    }

    fn end_section(self: *DualOutput) !void {
        if (self.html_writer) |writer| {
            try writer.writeAll(
                \\            </div>
                \\        </div>
            );
        }
    }

    fn begin_code_block(self: *DualOutput, language: []const u8) !void {
        try self.md_writer.print("~~~{s}\n", .{language});
    }

    fn end_code_block(self: *DualOutput) !void {
        try self.md_writer.writeAll("~~~\n");
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
    try content.meta.format(output.md_writer);
    try output.md_writer.writeAll("\n");

    // HTML META section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="meta-info">
            \\                    <p><strong>Description:</strong>
        );
        try writer.writeAll(content.meta.description);
        try writer.writeAll("</p>\n                    <p><strong>Type:</strong> ");
        try writer.writeAll(content.meta.node_type.toString());
        try writer.writeAll(
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
    try output.md_writer.writeAll(content.source);
    if (content.source.len == 0 or content.source[content.source.len - 1] != '\n') {
        try output.md_writer.writeAll("\n");
    }

    // HTML SOURCE section - encode source as JavaScript string
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="source-code" id="source-display">
            \\                </div>
            \\                <script>
            \\                window.rocSourceCode =
        );

        // Escape the source code for JavaScript string literal
        try writer.writeAll("`");
        for (content.source) |char| {
            switch (char) {
                '`' => try writer.writeAll("\\`"),
                '\\' => try writer.writeAll("\\\\"),
                '$' => try writer.writeAll("\\$"),
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                else => try writer.writeByte(char),
            }
        }
        try writer.writeAll(
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
    reports: *const std.ArrayList(reporting.Report),
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
                std.debug.print("Hint: use `--update-expected` to automatically update the expectations", .{});

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
                std.debug.print("Hint: use `--check-expected` to give a more detailed report", .{});
            }
        },
    }

    // Write the expected content (either generated or existing)
    if (expected_content) |expected| {
        try output.md_writer.writeAll(expected);
        try output.md_writer.writeByte('\n');

        // HTML EXPECTED section
        if (output.html_writer) |writer| {
            try writer.writeAll(
                \\                <div class="expected">
            );

            // For HTML, escape the expected content
            for (expected) |char| {
                switch (char) {
                    '<' => try writer.writeAll("&lt;"),
                    '>' => try writer.writeAll("&gt;"),
                    '&' => try writer.writeAll("&amp;"),
                    '"' => try writer.writeAll("&quot;"),
                    '\'' => try writer.writeAll("&#39;"),
                    else => try writer.writeByte(char),
                }
            }

            try writer.writeAll(
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
fn generateProblemsSection(output: *DualOutput, reports: *const std.ArrayList(reporting.Report)) !void {
    try output.begin_section("PROBLEMS");
    try renderReportsToProblemsSection(output, reports);
    try output.end_section();
}

/// Generate TOKENS section for both markdown and HTML
pub fn generateTokensSection(output: *DualOutput, parse_ast: *AST, _: *const Content, module_env: *ModuleEnv) !void {
    try output.begin_section("TOKENS");
    try output.begin_code_block("zig");

    // HTML TOKENS section - encode tokens as JavaScript array
    if (output.html_writer) |writer| {
        try writer.writeAll(
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
        try output.md_writer.print("{s}({d}:{d}-{d}:{d}),", .{
            @tagName(tok),
            // add one to display numbers instead of index
            info.start_line_idx + 1,
            info.start_col_idx + 1,
            info.end_line_idx + 1,
            info.end_col_idx + 1,
        });

        if (i + 1 < tokenizedBuffer.tokens.len) {
            const next_region = tokenizedBuffer.resolve(@intCast(i + 1));
            if (source_contains_newline_in_range(parse_ast.env.source, @min(region.end.offset, next_region.start.offset), @max(region.end.offset, next_region.start.offset))) {
                try output.md_writer.writeAll("\n");
            }
        }

        // HTML token output as JavaScript array element: [token_kind_str, start_byte, end_byte]
        if (output.html_writer) |writer| {
            try writer.print("                    [\"{s}\", {d}, {d}]", .{
                @tagName(tok),
                region.start.offset,
                region.end.offset,
            });

            // Add comma except for last token
            if (i < tokens.len - 1) {
                try writer.writeAll(",");
            }
        }

        if (output.html_writer) |writer| {
            try writer.writeAll(" ");
        }
    }

    try output.md_writer.writeAll("\n");

    if (output.html_writer) |writer| {
        try writer.writeAll(
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
fn generateParseSection(output: *DualOutput, content: *const Content, parse_ast: *AST, env: *CommonEnv) !void {
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
    }

    // Only generate section if we have content on the stack
    if (tree.stack.items.len > 0) {
        try output.begin_section("PARSE");
        try output.begin_code_block("clojure");

        try tree.toStringPretty(output.md_writer.any());
        try output.md_writer.writeAll("\n");

        // Generate HTML output with syntax highlighting
        if (output.html_writer) |writer| {
            try writer.writeAll(
                \\                <pre class="ast-parse">
            );

            try tree.toHtml(writer.any());

            try writer.writeAll(
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
    var formatted = std.ArrayList(u8).init(output.gpa);
    defer formatted.deinit();

    switch (content.meta.node_type) {
        .file => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .header => {
            try fmt.formatHeader(parse_ast.*, formatted.writer().any());
        },
        .expr => {
            try fmt.formatExpr(parse_ast.*, formatted.writer().any());
        },
        .statement => {
            try fmt.formatStatement(parse_ast.*, formatted.writer().any());
        },
        .package => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .platform => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .app => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .repl => {
            // REPL doesn't use formatting
            return;
        },
    }

    const is_changed = !std.mem.eql(u8, formatted.items, content.source);
    const display_content = if (is_changed) formatted.items else "NO CHANGE";

    try output.begin_section("FORMATTED");
    try output.begin_code_block("roc");

    try output.md_writer.writeAll(display_content);
    try output.md_writer.writeAll("\n");

    // HTML FORMATTED section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <pre>
        );

        // Escape HTML in formatted content
        for (display_content) |char| {
            try escapeHtmlChar(writer, char);
        }

        try writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate CANONICALIZE section for both markdown and HTML
fn generateCanonicalizeSection(output: *DualOutput, can_ir: *ModuleEnv, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();
    try can_ir.pushToSExprTree(maybe_expr_idx, &tree);

    try output.begin_section("CANONICALIZE");
    try output.begin_code_block("clojure");

    try tree.toStringPretty(output.md_writer.any());
    try output.md_writer.writeAll("\n");

    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <pre>
        );
        try tree.toHtml(writer.any());
        try writer.writeAll(
            \\</pre>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section for both markdown and HTML
fn generateTypesSection(output: *DualOutput, can_ir: *ModuleEnv, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();
    try can_ir.pushTypesToSExprTree(maybe_expr_idx, &tree);

    try output.begin_section("TYPES");
    try output.begin_code_block("clojure");
    try tree.toStringPretty(output.md_writer.any());
    try output.md_writer.writeAll("\n");

    // HTML TYPES section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <pre>
        );
        try tree.toHtml(writer.any());
        try writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section displaying types store for both markdown and HTML
/// This is used for debugging.
fn generateTypesStoreSection(gpa: std.mem.Allocator, output: *DualOutput, can_ir: *ModuleEnv) !void {
    var solved = std.ArrayList(u8).init(output.gpa);
    defer solved.deinit();

    try types.writers.SExprWriter.allVarsToSExprStr(solved.writer().any(), gpa, can_ir.env);

    // Markdown TYPES section
    try output.md_writer.writeAll(Section.TYPES);
    try output.md_writer.writeAll(solved.items);
    try output.md_writer.writeAll("\n");
    try output.md_writer.writeAll(Section.SECTION_END[0 .. Section.SECTION_END.len - 1]);

    // HTML TYPES section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\        <div class="section">
            \\            <div class="section-header">TYPES</div>
            \\            <div class="section-content">
            \\                <pre>
        );

        // Escape HTML in types content
        for (solved.items) |char| {
            try escapeHtmlChar(writer, char);
        }

        try writer.writeAll(
            \\</pre>
            \\            </div>
            \\        </div>
            \\
        );
    }
}

/// Generate HTML document structure and JavaScript
fn generateHtmlWrapper(output: *DualOutput, content: *const Content) !void {
    const writer = output.html_writer orelse return;

    // Write HTML document structure
    try writer.writeAll(
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\    <meta charset="UTF-8">
        \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\    <title>Roc Snapshot:
    );
    try writer.writeAll(content.meta.description);
    try writer.writeAll(
        \\</title>
        \\    <style>
        \\
    );
    try writer.writeAll(@embedFile("snapshot.css"));
    try writer.writeAll(
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
    try writer.writeAll(
        \\    </div>
        \\
        \\    <script>
    );
    // Embed remaining snapshot.js directly into the HTML
    try writer.writeAll(@embedFile("snapshot.js"));
    try writer.writeAll(
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
    try html_file.writer().writeAll(html_buffer.items);

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
        const rand_file_name = [_][]const u8{
            path,
            &[_]u8{
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                '.',
                'r',
                'o',
                'c',
            },
        };

        const corpus_file_path = try std.fs.path.join(gpa, &rand_file_name);
        defer gpa.free(corpus_file_path);

        var corpus_file = std.fs.cwd().createFile(corpus_file_path, .{}) catch |err| {
            std.log.err("failed to create file in '{s}': {s}", .{ config.maybe_fuzz_corpus_path.?, @errorName(err) });
            return false;
        };
        defer corpus_file.close();

        try corpus_file.writer().writeAll(content.source);
    }

    return success;
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
                // Only process META, SOURCE, OUTPUT, and EXPECTED sections
                if (section == .meta or section == .source or section == .expected or section == .output) {
                    const header_len = section.asString().len;
                    const start = idx + header_len;

                    // Find the end of this section
                    var end = content.len;

                    // For sections with ~~~ delimiters (META and SOURCE)
                    if (section == .meta or section == .source) {
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
                        // Find the next section header
                        var search_idx = start;
                        while (search_idx < content.len) {
                            if (search_idx == 0 or (search_idx > 0 and content[search_idx - 1] == '\n')) {
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

fn processReplSnapshot(allocator: Allocator, content: Content, output_path: []const u8, config: *const Config) !bool {
    var success = true;
    log("Processing REPL snapshot: {s}", .{output_path});

    // Buffer all output in memory before writing files
    var md_buffer = std.ArrayList(u8).init(allocator);
    defer md_buffer.deinit();

    var html_buffer = if (config.generate_html) std.ArrayList(u8).init(allocator) else null;
    defer if (html_buffer) |*buf| buf.deinit();

    var output = DualOutput.init(allocator, &md_buffer, if (html_buffer) |*buf| buf else null);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate all sections
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    success = try generateReplOutputSection(&output, output_path, &content, config) and success;
    try generateReplProblemsSection(&output, &content);
    try generateReplCanonicalizeSection(&output, &content);
    try generateReplTypesSection(&output, &content);

    try generateHtmlClosing(&output);

    if (!config.disable_updates) {
        // Write the markdown file
        const md_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
            std.log.err("Failed to create {s}: {}", .{ output_path, err });
            return false;
        };
        defer md_file.close();

        try md_file.writeAll(md_buffer.items);

        if (html_buffer) |*buf| {
            writeHtmlFile(allocator, output_path, buf) catch |err| {
                warn("Failed to write HTML file for {s}: {}", .{ output_path, err });
            };
        }
    }

    return success;
}

fn generateReplOutputSection(output: *DualOutput, snapshot_path: []const u8, content: *const Content, config: *const Config) !bool {
    var success = true;
    // Parse REPL inputs from the source
    var lines = std.mem.tokenizeScalar(u8, content.source, '\n');
    var inputs = std.ArrayList([]const u8).init(output.gpa);
    defer inputs.deinit();

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len > 2 and std.mem.startsWith(u8, trimmed, "» ")) {
            try inputs.append(trimmed[2..]);
        }
    }

    var snapshot_ops = SnapshotOps.init(output.gpa);
    defer snapshot_ops.deinit();

    // Initialize REPL
    var repl_instance = try Repl.init(output.gpa, snapshot_ops.get_ops());
    defer repl_instance.deinit();

    // Enable tracing if requested
    if (config.trace_eval) {
        repl_instance.setTraceWriter(std.io.getStdErr().writer().any());
    }

    // Process each input and generate output
    var actual_outputs = std.ArrayList([]const u8).init(output.gpa);
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
                    try output.md_writer.writeAll("---\n");
                }
                try output.md_writer.writeAll(repl_output);
                try output.md_writer.writeByte('\n');

                // HTML output
                if (output.html_writer) |writer| {
                    if (i > 0) {
                        try writer.writeAll("                <hr>\n");
                    }
                    try writer.writeAll("                <div class=\"repl-output\">");
                    for (repl_output) |char| {
                        try escapeHtmlChar(writer, char);
                    }
                    try writer.writeAll("</div>\n");
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
                var expected_outputs = std.ArrayList([]const u8).init(output.gpa);
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
                            std.debug.print("REPL output mismatch at index {}: got '{s}', expected '{s}' in {s}\n", .{
                                i,
                                actual,
                                expected_output,
                                snapshot_path,
                            });
                        }
                    }
                }

                // Write the old outputs back to the file
                for (expected_outputs.items, 0..) |expected_output, i| {
                    if (i > 0) {
                        try output.md_writer.writeAll("---\n");
                    }
                    try output.md_writer.writeAll(expected_output);
                    try output.md_writer.writeByte('\n');

                    // HTML output
                    if (output.html_writer) |writer| {
                        if (i > 0) {
                            try writer.writeAll("                <hr>\n");
                        }
                        try writer.writeAll("                <div class=\"repl-output\">");
                        for (expected_output) |char| {
                            try escapeHtmlChar(writer, char);
                        }
                        try writer.writeAll("</div>\n");
                    }
                }
                try output.end_section();
            } else {
                // No existing OUTPUT section - generate one for new snapshots
                try output.begin_section("OUTPUT");
                for (actual_outputs.items, 0..) |repl_output, i| {
                    if (i > 0) {
                        try output.md_writer.writeAll("---\n");
                    }
                    try output.md_writer.writeAll(repl_output);
                    try output.md_writer.writeByte('\n');

                    // HTML output
                    if (output.html_writer) |writer| {
                        if (i > 0) {
                            try writer.writeAll("                <hr>\n");
                        }
                        try writer.writeAll("                <div class=\"repl-output\">");
                        for (repl_output) |char| {
                            try escapeHtmlChar(writer, char);
                        }
                        try writer.writeAll("</div>\n");
                    }
                }
                try output.end_section();

                if (actual_outputs.items.len != 0 and emit_error) {
                    std.debug.print("REPL output count mismatch: got {} outputs, expected {} in {s}\n", .{
                        actual_outputs.items.len,
                        0,
                        snapshot_path,
                    });
                    success = false;
                }
            }
        },
    }

    return success;
}

fn generateReplProblemsSection(output: *DualOutput, content: *const Content) !void {
    _ = content;
    try output.begin_section("PROBLEMS");
    try output.md_writer.writeAll("NIL\n");

    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="problems">
            \\                    <p>NIL</p>
            \\                </div>
            \\
        );
    }

    try output.end_section();
}

/// Generate CANONICALIZE section for REPL snapshots
fn generateReplCanonicalizeSection(output: *DualOutput, content: *const Content) !void {
    // Parse REPL inputs from the source
    var lines = std.mem.tokenizeScalar(u8, content.source, '\n');
    var inputs = std.ArrayList([]const u8).init(output.gpa);
    defer inputs.deinit();

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len > 2 and std.mem.startsWith(u8, trimmed, "» ")) {
            try inputs.append(trimmed[2..]);
        }
    }

    try output.begin_section("CANONICALIZE");
    try output.begin_code_block("clojure");

    // Generate canonical forms for each input
    for (inputs.items, 0..) |input, i| {
        if (i > 0) {
            try output.md_writer.writeAll("---\n");
        }

        // Create a temporary ModuleEnv for this input
        var module_env = ModuleEnv.init(output.gpa, input) catch |err| {
            try output.md_writer.print("Error creating module env: {s}\n", .{@errorName(err)});
            continue;
        };
        defer module_env.deinit();

        // Calculate line starts for source location tracking
        module_env.common.calcLineStarts(output.gpa) catch |err| {
            try output.md_writer.print("Error calculating line starts: {s}\n", .{@errorName(err)});
            continue;
        };

        // Parse the input as an expression
        var parse_ast = parse.parseExpr(&module_env.common, output.gpa) catch |err| {
            try output.md_writer.print("Parse error: {s}\n", .{@errorName(err)});
            continue;
        };
        defer parse_ast.deinit(output.gpa);

        // Initialize canonicalization
        try module_env.initCIRFields(output.gpa, "repl");
        var czer = Can.init(&module_env, &parse_ast, null) catch |err| {
            try output.md_writer.print("Can init error: {s}\n", .{@errorName(err)});
            continue;
        };
        defer czer.deinit();

        // Canonicalize the expression
        const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const maybe_canonical_expr = czer.canonicalizeExpr(expr_idx) catch |err| {
            try output.md_writer.print("Canonicalize error: {s}\n", .{@errorName(err)});
            continue;
        };

        if (maybe_canonical_expr) |canonical_expr| {
            // Generate S-expression tree for the canonical form
            var tree = SExprTree.init(output.gpa);
            defer tree.deinit();

            module_env.pushToSExprTree(canonical_expr.idx, &tree) catch |err| {
                try output.md_writer.print("S-expr tree error: {s}\n", .{@errorName(err)});
                continue;
            };

            tree.toStringPretty(output.md_writer.any()) catch |err| {
                try output.md_writer.print("S-expr format error: {s}\n", .{@errorName(err)});
                continue;
            };
            try output.md_writer.writeAll("\n");

            // HTML output
            if (output.html_writer) |writer| {
                if (i > 0) {
                    try writer.writeAll("                <hr>\n");
                }
                try writer.writeAll("                <div class=\"repl-canonical\">");
                tree.toHtml(writer.any()) catch |err| {
                    try writer.print("S-expr HTML error: {s}", .{@errorName(err)});
                };
                try writer.writeAll("</div>\n");
            }
        } else {
            try output.md_writer.writeAll("Failed to canonicalize\n");

            if (output.html_writer) |writer| {
                if (i > 0) {
                    try writer.writeAll("                <hr>\n");
                }
                try writer.writeAll("                <div class=\"repl-canonical\">Failed to canonicalize</div>\n");
            }
        }
    }

    try output.end_code_block();
    try output.end_section();
}

fn generateReplTypesSection(output: *DualOutput, content: *const Content) !void {
    // Parse REPL inputs from the source
    var lines = std.mem.tokenizeScalar(u8, content.source, '\n');
    var inputs = std.ArrayList([]const u8).init(output.gpa);
    defer inputs.deinit();

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len > 2 and std.mem.startsWith(u8, trimmed, "» ")) {
            try inputs.append(trimmed[2..]);
        }
    }

    try output.begin_section("TYPES");
    try output.begin_code_block("clojure");

    // Generate types for each input
    for (inputs.items, 0..) |input, i| {
        if (i > 0) {
            try output.md_writer.writeAll("---\n");
        }

        // Create a temporary ModuleEnv for this input
        var module_env = ModuleEnv.init(output.gpa, input) catch |err| {
            try output.md_writer.print("Error creating module env: {s}\n", .{@errorName(err)});
            continue;
        };
        defer module_env.deinit();

        // Calculate line starts for source location tracking
        module_env.common.calcLineStarts(output.gpa) catch |err| {
            try output.md_writer.print("Error calculating line starts: {s}\n", .{@errorName(err)});
            return;
        };

        // Parse the input as an expression
        var parse_ast = parse.parseExpr(&module_env.common, output.gpa) catch |err| {
            try output.md_writer.print("Parse error: {s}\n", .{@errorName(err)});
            continue;
        };
        defer parse_ast.deinit(output.gpa);

        // Initialize canonicalization
        try module_env.initCIRFields(output.gpa, "repl");
        var czer = Can.init(&module_env, &parse_ast, null) catch |err| {
            try output.md_writer.print("Can init error: {s}\n", .{@errorName(err)});
            continue;
        };
        defer czer.deinit();

        // Canonicalize the expression
        const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const maybe_canonical_expr = czer.canonicalizeExpr(expr_idx) catch |err| {
            try output.md_writer.print("Canonicalize error: {s}\n", .{@errorName(err)});
            continue;
        };

        if (maybe_canonical_expr) |canonical_expr| {
            // Initialize type checking
            const empty_modules: []const *ModuleEnv = &.{};
            var solver = Check.init(output.gpa, &module_env.types, &module_env, empty_modules, &module_env.store.regions) catch |err| {
                try output.md_writer.print("Type checker init error: {s}\n", .{@errorName(err)});
                continue;
            };
            defer solver.deinit();

            // Check the expression
            _ = solver.checkExpr(canonical_expr.idx) catch |err| {
                try output.md_writer.print("Type check error: {s}\n", .{@errorName(err)});
                continue;
            };

            // Generate S-expression tree for the types
            var tree = SExprTree.init(output.gpa);
            defer tree.deinit();

            module_env.pushTypesToSExprTree(canonical_expr.idx, &tree) catch |err| {
                try output.md_writer.print("Types S-expr tree error: {s}\n", .{@errorName(err)});
                continue;
            };

            tree.toStringPretty(output.md_writer.any()) catch |err| {
                try output.md_writer.print("Types S-expr format error: {s}\n", .{@errorName(err)});
                continue;
            };
            try output.md_writer.writeAll("\n");

            // HTML output
            if (output.html_writer) |writer| {
                if (i > 0) {
                    try writer.writeAll("                <hr>\n");
                }
                try writer.writeAll("                <div class=\"repl-types\">");
                tree.toHtml(writer.any()) catch |err| {
                    try writer.print("Types S-expr HTML error: {s}", .{@errorName(err)});
                };
                try writer.writeAll("</div>\n");
            }
        } else {
            try output.md_writer.writeAll("Failed to canonicalize\n");

            if (output.html_writer) |writer| {
                if (i > 0) {
                    try writer.writeAll("                <hr>\n");
                }
                try writer.writeAll("                <div class=\"repl-types\">Failed to canonicalize</div>\n");
            }
        }
    }

    try output.end_code_block();
    try output.end_section();
}

test "snapshot validation" {
    const allocator = std.testing.allocator;
    if (!try checkSnapshotExpectations(allocator)) {
        return error.SnapshotValidationFailed;
    }
}

/// An implementation of RocOps for snapshot testing.
pub const SnapshotOps = struct {
    allocator: std.mem.Allocator,
    roc_ops: RocOps,

    pub fn init(allocator: std.mem.Allocator) SnapshotOps {
        return SnapshotOps{
            .allocator = allocator,
            .roc_ops = RocOps{
                .env = undefined, // will be set below
                .roc_alloc = snapshotRocAlloc,
                .roc_dealloc = snapshotRocDealloc,
                .roc_realloc = snapshotRocRealloc,
                .roc_dbg = snapshotRocDbg,
                .roc_expect_failed = snapshotRocExpectFailed,
                .roc_crashed = snapshotRocCrashed,
                .host_fns = undefined, // Not used in snapshots
            },
        };
    }

    pub fn deinit(self: *SnapshotOps) void {
        _ = self;
        // nothing to do here?
    }

    pub fn get_ops(self: *SnapshotOps) *RocOps {
        self.roc_ops.env = @ptrCast(self);
        return &self.roc_ops;
    }
};

fn snapshotRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
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

fn snapshotRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
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

fn snapshotRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
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

fn snapshotRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = dbg_args;
    _ = env;
    @panic("snapshotRocDbg not implemented yet");
}

fn snapshotRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = expect_args;
    _ = env;
    @panic("snapshotRocExpectFailed not implemented yet");
}

fn snapshotRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    _ = env;
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    std.log.err("Test program crashed: {s}", .{msg_slice});
}
