//! Snapshot testing infrastructure for the Roc compiler.
//!
//! This module provides functionality to generate and validate snapshot tests
//! that capture the compiler's behavior at each stage of compilation. Snapshots
//! help ensure the compiler continues to behave as expected by showing the
//! output of tokenization, parsing, canonicalization, type checking etc for
//! the given Roc code snippet.

const std = @import("std");
const builtin = @import("builtin");
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
const collections = @import("collections");

const Repl = repl.Repl;
const CommonEnv = base.CommonEnv;
const Region = base.Region;
const Check = check.Check;
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
const AST = parse.AST; // Using the new AST (formerly AST)
const Parser = parse.Parser; // Using the new Parser (formerly Parser)
const CIR = can.CIR; // Using the new CIR (formerly CIR)
const Report = reporting.Report;
const types_problem_mod = check.problem;
const tokenize = parse.tokenize;
const parallel = base.parallel;
const ByteSlices = collections.ByteSlices;

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

/// Helper to determine if an AST node tag represents an expression
fn isExpressionNode(tag: AST.Node.Tag) bool {
    return switch (tag) {
        .num_literal_i32, .int_literal_i32, .num_literal_big, .int_literal_big, .frac_literal_small, .frac_literal_big, .str_literal_small, .str_literal_big, .lc, .uc, .var_lc, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_and, .binop_or, .block, .record_literal, .lambda, .lambda_no_args, .apply_lc, .apply_uc, .apply_anon, .tuple_literal, .list_literal, .if_else, .match, .unary_neg, .unary_not => true,
        else => false,
    };
}

/// Helper to determine if an AST node tag represents a statement
fn isStatementNode(tag: AST.Node.Tag) bool {
    return switch (tag) {
        .binop_equals, .binop_colon, .binop_colon_equals, .import => true,
        else => false,
    };
}

/// Helper to determine if an AST node tag represents a pattern
fn isPatternNode(tag: AST.Node.Tag) bool {
    return switch (tag) {
        .underscore => true,
        else => false,
    };
}

/// Generate reports from Parser and CIR diagnostics
fn generateReportsFromNewSystem(
    allocator: std.mem.Allocator,
    parser: *const Parser,
    cir: *const CIR,
    env: *const base.CommonEnv,
    snapshot_path: []const u8,
) !std.ArrayList(reporting.Report) {
    var reports = std.ArrayList(reporting.Report).init(allocator);
    errdefer reports.deinit();

    // Convert Parser diagnostics to reports
    for (parser.diagnostics.items) |diag| {
        const report = try convertParserDiagnosticToReport(allocator, diag, parser, env, snapshot_path);
        try reports.append(report);
    }

    // Convert CIR diagnostics to reports
    for (cir.diagnostics.items) |diag| {
        const report = try convertCIRDiagnosticToReport(allocator, diag, env, snapshot_path);
        try reports.append(report);
    }

    return reports;
}

/// Convert Parser diagnostic to a report
fn convertParserDiagnosticToReport(
    allocator: std.mem.Allocator,
    diag: AST.Diagnostic,
    parser: *const Parser,
    env: *const base.CommonEnv,
    snapshot_path: []const u8,
) !Report {
    _ = parser;

    const title = switch (diag.tag) {
        .multiple_platforms => "MULTIPLE PLATFORMS",
        .no_platform => "NO PLATFORM",
        .missing_header => "MISSING HEADER",
        .missing_arrow => "MISSING ARROW",
        .expected_exposes => "EXPECTED EXPOSES",
        .expected_exposes_close_square => "EXPECTED CLOSING BRACKET",
        .expected_exposes_open_square => "EXPECTED OPENING BRACKET",
        .expected_imports => "EXPECTED IMPORTS",
        .expected_package_or_platform_name => "EXPECTED PACKAGE OR PLATFORM NAME",
        .expected_package_or_platform_colon => "EXPECTED COLON",
        .expected_package_or_platform_string => "EXPECTED STRING",
        .expected_package_platform_close_curly => "EXPECTED CLOSE CURLY BRACE",
        .expected_package_platform_open_curly => "EXPECTED OPEN CURLY BRACE",
        .expected_packages => "EXPECTED PACKAGES",
        .expected_packages_close_curly => "EXPECTED CLOSE CURLY BRACE",
        .pattern_unexpected_token => "UNEXPECTED TOKEN IN PATTERN",
        .pattern_list_rest_old_syntax => "BAD LIST REST PATTERN SYNTAX",
        .pattern_unexpected_eof => "UNEXPECTED END OF FILE IN PATTERN",
        .ty_anno_unexpected_token => "UNEXPECTED TOKEN IN TYPE ANNOTATION",
        .string_unexpected_token => "UNEXPECTED TOKEN IN STRING",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
        .import_must_be_top_level => "IMPORT MUST BE TOP LEVEL",
        .expected_expr_close_square_or_comma => "LIST NOT CLOSED",
        .where_expected_mod_open => "WHERE CLAUSE ERROR",
        .where_expected_var => "WHERE CLAUSE ERROR",
        .where_expected_mod_close => "WHERE CLAUSE ERROR",
        .where_expected_arg_open => "WHERE CLAUSE ERROR",
        .where_expected_arg_close => "WHERE CLAUSE ERROR",
        .where_expected_method_arrow => "WHERE CLAUSE ERROR",
        .where_expected_method_or_alias_name => "WHERE CLAUSE ERROR",
        .where_expected_module => "WHERE CLAUSE ERROR",
        .where_expected_colon => "WHERE CLAUSE ERROR",
        .where_expected_constraints => "WHERE CLAUSE ERROR",
        .no_else => "IF WITHOUT ELSE",
        else => "PARSE ERROR",
    };

    var report = Report.init(allocator, title, .runtime_error);

    // Ensure region bounds are valid for source slicing
    const region = base.Region{
        .start = .{ .offset = @min(diag.region.start.offset, env.source.len) },
        .end = .{ .offset = @min(@max(diag.region.end.offset, diag.region.start.offset), env.source.len) },
    };

    // Add detailed error message based on the diagnostic type
    switch (diag.tag) {
        .missing_header => {
            try report.document.addReflowingText("Roc files must start with a module header.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addText("For example:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("module [main]");
            try report.document.addLineBreak();
            try report.document.addText("or for an app:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("app [main!] { pf: platform \"../basic-cli/platform.roc\" }");
        },
        .multiple_platforms => {
            try report.document.addReflowingText("Only one platform declaration is allowed per file.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Remove the duplicate platform declaration.");
        },
        .no_platform => {
            try report.document.addReflowingText("App files must specify a platform.");
            try report.document.addLineBreak();
            try report.document.addText("Add a platform specification like:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("{ pf: platform \"../basic-cli/platform.roc\" }");
        },
        .missing_arrow => {
            try report.document.addText("Expected an arrow ");
            try report.document.addAnnotated("->", .emphasized);
            try report.document.addText(" here.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Function type annotations require arrows between parameter and return types.");
        },
        .expected_exposes, .expected_exposes_close_square, .expected_exposes_open_square => {
            try report.document.addReflowingText("Module headers must have an ");
            try report.document.addKeyword("exposing");
            try report.document.addReflowingText(" section that lists what the module exposes.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("module [main, add, subtract]");
        },
        .expected_imports => {
            try report.document.addReflowingText("Import statements must specify what is being imported.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("import pf.Stdout exposing [line!]");
        },
        .pattern_unexpected_token => {
            const token_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a pattern.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Patterns can contain identifiers, literals, lists, records, or tags.");
        },
        .pattern_list_rest_old_syntax => {
            try report.document.addReflowingText("List rest patterns should use the `.. as name` syntax, not `..name`.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("For example, use `[first, .. as rest]` instead of `[first, ..rest]`.");
        },
        .pattern_unexpected_eof => {
            try report.document.addReflowingText("The pattern appears to be incomplete.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Make sure all patterns are complete and properly closed.");
        },
        .ty_anno_unexpected_token => {
            const token_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a type annotation.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Type annotations should contain types like ");
            try report.document.addType("Str");
            try report.document.addText(", ");
            try report.document.addType("Num a");
            try report.document.addText(", or ");
            try report.document.addType("List U64");
            try report.document.addText(".");
        },
        .string_unexpected_token => {
            const token_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a string literal.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("String literals should be enclosed in double quotes.");
        },
        .expr_unexpected_token => {
            const token_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in an expression.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Expressions can be identifiers, literals, function calls, or operators.");
        },
        .import_must_be_top_level => {
            try report.document.addReflowingText("Import statements must appear at the top level of a module.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Move this import to the top of the file, after the module header but before any definitions.");
        },
        .expected_expr_close_square_or_comma => {
            try report.document.addReflowingText("This list is not properly closed.");
            try report.document.addLineBreak();
            try report.document.addText("Expected either a comma ");
            try report.document.addAnnotated(",", .emphasized);
            try report.document.addText(" to continue the list or a closing bracket ");
            try report.document.addAnnotated("]", .emphasized);
            try report.document.addText(" to end it.");
        },
        .where_expected_mod_open, .where_expected_var, .where_expected_mod_close, .where_expected_arg_open, .where_expected_arg_close, .where_expected_method_arrow, .where_expected_method_or_alias_name, .where_expected_module, .where_expected_colon, .where_expected_constraints => {
            try report.document.addReflowingText("There's a problem with this where clause.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Where clauses should have the form:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("where module(a).method : args -> ret");
        },
        .no_else => {
            try report.document.addReflowingText("This `if` expression is missing an `else` branch.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("All `if` expressions in Roc must have an `else` branch to ensure they always return a value.");
        },
        else => {
            // Generic parse error message
            try report.document.addText("A parsing error occurred: ");
            try report.document.addAnnotated(@tagName(diag.tag), .error_highlight);
            try report.document.addLineBreak();
            try report.document.addReflowingText("This is an unexpected parsing error. Please check your syntax.");
        },
    }

    try report.document.addLineBreak();
    try report.document.addLineBreak();

    // Add the source context with line numbers
    if (diag.region.start.offset < diag.region.end.offset and
        diag.region.end.offset <= env.source.len)
    {
        // Convert region to RegionInfo
        const region_info = base.RegionInfo.position(
            env.source,
            env.line_starts.items.items,
            diag.region.start.offset,
            diag.region.end.offset,
        ) catch {
            // If we can't calculate region info, just return the report without source context
            return report;
        };

        // Add source region to the report
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            snapshot_path,
            env.source,
            env.line_starts.items.items,
        );
    }

    return report;
}

/// Convert CIR diagnostic to a report
fn convertCIRDiagnosticToReport(
    allocator: std.mem.Allocator,
    diag: CIR.CanDiagnostic,
    env: *const base.CommonEnv,
    snapshot_path: []const u8,
) !Report {
    const title = switch (diag.tag) {
        .pattern_in_expr_context => "PATTERN IN EXPRESSION CONTEXT",
        .expr_in_pattern_context => "EXPRESSION IN PATTERN CONTEXT",
        .stmt_in_expr_context => "STATEMENT IN EXPRESSION CONTEXT",
        .expr_in_stmt_context => "EXPRESSION IN STATEMENT CONTEXT",
        .type_in_expr_context => "TYPE IN EXPRESSION CONTEXT",
        .expr_in_type_context => "EXPRESSION IN TYPE CONTEXT",
        .ident_not_in_scope => "UNDEFINED VARIABLE",
        .ident_already_defined => "IDENTIFIER ALREADY DEFINED",
        .unused_variable => "UNUSED VARIABLE",
        .type_not_in_scope => "TYPE NOT IN SCOPE",
        .invalid_type_var_in_constraint => "INVALID TYPE VARIABLE IN CONSTRAINT",
        .invalid_ability_in_constraint => "INVALID ABILITY IN CONSTRAINT",
        .invalid_where_constraint => "INVALID WHERE CONSTRAINT",
        .exposed_but_not_implemented => "EXPOSED BUT NOT IMPLEMENTED",
        .redundant_exposed => "REDUNDANT EXPOSED",
        .shadowing_warning => "SHADOWING",
        .unsupported_node => "UNSUPPORTED NODE",
        .malformed_ast => "MALFORMED AST",
    };

    var report = Report.init(allocator, title, .runtime_error);

    // Ensure region bounds are valid for source slicing
    const region = base.Region{
        .start = .{ .offset = @min(diag.region.start.offset, env.source.len) },
        .end = .{ .offset = @min(@max(diag.region.end.offset, diag.region.start.offset), env.source.len) },
    };

    // Add detailed error message based on the diagnostic type
    switch (diag.tag) {
        .pattern_in_expr_context => {
            try report.document.addReflowingText("Found a pattern where an expression was expected.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Patterns can only appear in specific contexts like function parameters, ");
            try report.document.addText("destructuring assignments, or ");
            try report.document.addAnnotated("when", .emphasized);
            try report.document.addText(" branches.");
        },
        .expr_in_pattern_context => {
            try report.document.addReflowingText("Found an expression where a pattern was expected.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This location requires a pattern for matching or destructuring, not a computed value.");
        },
        .stmt_in_expr_context => {
            try report.document.addReflowingText("Found a statement where an expression was expected.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Statements like ");
            try report.document.addAnnotated("return", .emphasized);
            try report.document.addText(", ");
            try report.document.addAnnotated("dbg", .emphasized);
            try report.document.addText(", or ");
            try report.document.addAnnotated("expect", .emphasized);
            try report.document.addText(" cannot be used in expression contexts.");
        },
        .expr_in_stmt_context => {
            try report.document.addReflowingText("Found an expression where a statement was expected.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This might be a missing semicolon or an incorrectly placed expression.");
        },
        .type_in_expr_context => {
            try report.document.addReflowingText("Found a type annotation where an expression was expected.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Type annotations should appear after a colon in declarations, not in expression contexts.");
        },
        .ident_not_in_scope => {
            const ident_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            try report.document.addText("Nothing is named ");
            try report.document.addAnnotated(ident_text, .error_highlight);
            try report.document.addText(" in this scope.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Is there an ");
            try report.document.addAnnotated("import", .emphasized);
            try report.document.addText(" or ");
            try report.document.addAnnotated("exposing", .emphasized);
            try report.document.addText(" missing up-top?");
        },
        .ident_already_defined => {
            const ident_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            try report.document.addText("The identifier ");
            try report.document.addAnnotated(ident_text, .error_highlight);
            try report.document.addText(" has already been defined in this scope.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Each identifier can only be defined once. Consider using a different name.");
        },
        .unused_variable => {
            const var_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            try report.document.addText("Variable ");
            try report.document.addAnnotated(var_text, .error_highlight);
            try report.document.addText(" is not used anywhere in your code.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("If you don't need this variable, prefix it with an underscore like ");
            const underscore_text = try std.fmt.allocPrint(allocator, "_{s}", .{var_text});
            defer allocator.free(underscore_text);
            try report.document.addInlineCode(underscore_text);
            try report.document.addText(" to suppress this warning.");
            try report.document.addLineBreak();
            try report.document.addText("The unused variable is declared here:");
        },
        .type_not_in_scope => {
            const type_text = if (region.start.offset < region.end.offset)
                env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            try report.document.addText("The type ");
            try report.document.addAnnotated(type_text, .error_highlight);
            try report.document.addText(" is not in scope.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Make sure it's imported or defined in this module.");
        },
        .expr_in_type_context => {
            try report.document.addReflowingText("Found an expression where a type was expected.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Types must be type identifiers, type applications, or type expressions.");
        },
        .invalid_type_var_in_constraint => {
            try report.document.addReflowingText("Invalid type variable in where constraint.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Only lowercase type variables can be constrained in where clauses.");
        },
        .invalid_ability_in_constraint => {
            try report.document.addReflowingText("Invalid ability reference in where constraint.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Abilities must be uppercase identifiers or qualified names.");
        },
        .invalid_where_constraint => {
            try report.document.addReflowingText("Invalid where clause constraint syntax.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Where clauses should contain valid ability constraints.");
        },
        .unsupported_node => {
            try report.document.addReflowingText("This syntax is not yet supported by the compiler.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This might be a limitation in the current implementation that will be addressed in a future update.");
        },
        .exposed_but_not_implemented => {
            try report.document.addReflowingText("This value is exposed in the module header but not defined in the module.");
        },
        .redundant_exposed => {
            try report.document.addReflowingText("This value is exposed multiple times in the module header.");
        },
        .shadowing_warning => {
            try report.document.addReflowingText("This definition shadows an existing one.");
        },
        .malformed_ast => {
            try report.document.addReflowingText("The Abstract Syntax Tree is malformed at this location.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This usually indicates a parsing error that wasn't properly recovered from.");
        },
    }

    try report.document.addLineBreak();
    try report.document.addLineBreak();

    // Add the source context with line numbers
    if (diag.region.start.offset < diag.region.end.offset and
        diag.region.end.offset <= env.source.len)
    {
        // Convert region to RegionInfo
        const region_info = base.RegionInfo.position(
            env.source,
            env.line_starts.items.items,
            diag.region.start.offset,
            diag.region.end.offset,
        ) catch {
            // If we can't calculate region info, just return the report without source context
            return report;
        };

        // Add source region to the report
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            snapshot_path,
            env.source,
            env.line_starts.items.items,
        );
    }

    return report;
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

    // Create ByteSlices for Parser
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create AST for Parser
    // Increase initial capacity to handle large files like let_polymorphism_complex.md
    var ast = try AST.initCapacity(allocator, 16384);
    defer ast.deinit(allocator);

    // Create common environment for Parser
    var env = try base.CommonEnv.init(allocator, content.source);
    defer env.deinit(allocator);
    try env.calcLineStarts(allocator);

    // Create diagnostics buffer
    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;

    // Tokenize once for both parser and formatter
    var tokens = std.ArrayList(parse.tokenize_iter.Token).init(allocator);
    defer tokens.deinit();

    // Tokenize the source
    var token_iter = try parse.tokenize_iter.TokenIterator.init(&env, allocator, content.source, &messages, &byte_slices);
    defer token_iter.deinit(allocator);

    // Collect all tokens (including comments for formatter)
    while (try token_iter.next(allocator)) |token| {
        try tokens.append(token);
        if (token.tag == .EndOfFile) break;
    }

    // Create Parser
    var parser = try Parser.init(&env, allocator, content.source, &messages, &ast, &byte_slices);
    defer parser.deinit();

    // Parse based on node type
    std.debug.print("Processing snapshot file: {s}\n", .{output_path});
    const parse_result: ?i32 = switch (content.meta.node_type) {
        .file => if (try parser.parseFile()) |idx| @intFromEnum(idx) else null,
        .header => blk: {
            try parser.parseHeader();
            break :blk null; // Header doesn't return a node, it sets ast.header
        },
        .expr => @intFromEnum(try parser.parseExprFromSource(&messages)),
        .statement => if (try parser.parseStmt()) |stmt| @intFromEnum(stmt) else null,
        .package => if (try parser.parseFile()) |idx| @intFromEnum(idx) else null,
        .platform => if (try parser.parseFile()) |idx| @intFromEnum(idx) else null,
        .app => if (try parser.parseFile()) |idx| @intFromEnum(idx) else null,
        .repl => unreachable, // Handled above
    };

    // Get pointer to the AST
    var ast_ptr = &ast;

    // Buffer all output in memory before writing files
    var md_buffer = std.ArrayList(u8).init(allocator);
    defer md_buffer.deinit();

    var html_buffer = if (config.generate_html) std.ArrayList(u8).init(allocator) else null;
    defer if (html_buffer) |*buf| buf.deinit();

    var output = DualOutput.init(allocator, &md_buffer, if (html_buffer) |*buf| buf else null);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate the PARSE section BEFORE canonicalization (while we still have AST nodes)
    // We'll generate other sections that depend on AST here too
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    try generateTokensSection2(&output, &parser, &content, &env);
    try generateParseSection2(&output, &content, ast_ptr, &env, parse_result);
    const root_node_idx: ?AST.Node.Idx = if (parse_result) |idx| @as(AST.Node.Idx, @enumFromInt(idx)) else null;
    try generateFormattedSection2(&output, &content, ast_ptr, &parser, &env, root_node_idx, tokens.items);

    // Create a TypeStore for type inference
    // Increase capacity to handle large files with many type variables
    var types_store = try types.Store.initCapacity(allocator, 8192, 2048);
    defer types_store.deinit();

    // NOW we can create CIR and canonicalize (which will mutate AST into CIR)
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    var maybe_expr_idx: ?CIR.Expr.Idx = null;
    var maybe_stmt_idx: ?CIR.Stmt.Idx = null;
    var maybe_patt_idx: ?CIR.Patt.Idx = null;

    // Initialize the CIR's scope state with a root scope
    try cir.scope_state.scopes.append(allocator, CIR.Scope.init(false));

    // For headers, canonicalize the header itself
    if (content.meta.node_type == .header) {
        try cir.canonicalizeHeader(allocator, content.source, &env.idents);
    }

    // Canonicalize if we have a parse result
    if (parse_result) |node_idx_int| {
        const node_idx: AST.Node.Idx = @enumFromInt(node_idx_int);
        const node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));

        // For file content, check if it's a block and use the file canonicalizer
        if (content.meta.node_type == .file and node.tag == .block) {
            // Canonicalize as a file with top-level definitions
            maybe_expr_idx = try cir.canonicalizeFileBlock(allocator, node_idx, content.source, &env.idents);
        } else if (isExpressionNode(node.tag)) {
            maybe_expr_idx = try cir.canonicalizeExpr(allocator, node_idx, content.source, &env.idents);
        } else if (isStatementNode(node.tag)) {
            maybe_stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, content.source, &env.idents);
        } else if (isPatternNode(node.tag)) {
            maybe_patt_idx = try cir.canonicalizePatt(allocator, node_idx);
        } else {
            // Default to expression for unknown nodes
            maybe_expr_idx = try cir.canonicalizeExpr(allocator, node_idx, content.source, &env.idents);
        }
    }

    // Canonicalization has now mutated AST into CIR

    // Generate reports from Parser and CIR diagnostics
    var generated_reports = try generateReportsFromNewSystem(allocator, &parser, &cir, &env, output_path);
    defer {
        for (generated_reports.items) |*report| {
            report.deinit();
        }
        generated_reports.deinit();
    }

    // Generate remaining sections that depend on canonicalization
    success = try generateExpectedSection(&output, output_path, &content, &generated_reports, config) and success;
    try generateProblemsSection(&output, &generated_reports);
    try generateCanonicalizeSection2(&output, &cir, &env, maybe_expr_idx, maybe_stmt_idx, maybe_patt_idx);
    try generateSolvedSection(&output, &cir, &env, &types_store, maybe_expr_idx);
    try generateTypesSection2(&output, &cir, &env, &types_store, maybe_expr_idx);

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
    _: []const u8,
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
                // Disabled for now to reduce output spam
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

// ============================================================================
// New generation functions for Parser/AST/CIR
// ============================================================================

/// Generate TOKENS section for Parser
fn generateTokensSection2(output: *DualOutput, parser: *const Parser, content: *const Content, env: *base.CommonEnv) !void {
    try output.begin_section("TOKENS");
    try output.begin_code_block("text");

    // Create a new tokenizer to iterate through the tokens
    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    var byte_slices_temp = ByteSlices{ .entries = .{} };
    defer byte_slices_temp.entries.deinit(parser.gpa);

    var token_iter = try parse.tokenize_iter.TokenIterator.init(
        env,
        parser.gpa,
        content.source,
        &messages,
        &byte_slices_temp,
    );

    // Iterate through tokens and output them
    while (try token_iter.next(parser.gpa)) |token| {
        if (token.tag == .EndOfFile) break;

        try output.md_writer.print("{s} ", .{@tagName(token.tag)});
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate PARSE section for AST
fn generateParseSection2(output: *DualOutput, content: *const Content, ast: *AST, env: *const base.CommonEnv, parse_result: ?i32) !void {
    try output.begin_section("PARSE");
    try output.begin_code_block("clojure");

    // Output AST structure as S-expressions
    // Always check for header first - headers are the primary content for header types
    if (ast.header) |header| {
        try outputHeaderAsSExpr(output.md_writer, ast, env, header, 0);
    } else if (parse_result) |node_idx_int| {
        const root_idx: AST.Node.Idx = @enumFromInt(node_idx_int);
        try outputASTNodeAsSExpr(output.md_writer, ast, env, root_idx, 0);
    } else {
        try output.md_writer.writeAll("(empty)\n");
    }

    _ = content;

    try output.end_code_block();
    try output.end_section();
}

/// Helper to output header as S-expression
fn outputHeaderAsSExpr(writer: anytype, ast: *const AST, env: *const base.CommonEnv, header: AST.Header, indent: usize) !void {
    // Indent
    for (0..indent) |_| {
        try writer.writeAll("  ");
    }

    // Output header based on type
    switch (header) {
        .app => |app| {
            try writer.writeAll("(app-header");

            // Output exposes if present
            if (!app.exposes.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(exposes");
                var iter = ast.node_slices.nodes(&app.exposes);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            // Output packages
            if (!app.packages.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(packages");
                var iter = ast.node_slices.nodes(&app.packages);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            try writer.writeAll(")\n");
        },
        .module => |mod| {
            try writer.writeAll("(module-header");

            // Output exposes
            if (!mod.exposes.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(exposes");
                var iter = ast.node_slices.nodes(&mod.exposes);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            try writer.writeAll(")\n");
        },
        .package => |pkg| {
            try writer.writeAll("(package-header");

            // Output exposes
            if (!pkg.exposes.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(exposes");
                var iter = ast.node_slices.nodes(&pkg.exposes);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            // Output packages
            if (!pkg.packages.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(packages");
                var iter = ast.node_slices.nodes(&pkg.packages);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            try writer.writeAll(")\n");
        },
        .platform => |plat| {
            try writer.writeAll("(platform-header");

            // Output exposes
            if (!plat.exposes.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(exposes");
                var iter = ast.node_slices.nodes(&plat.exposes);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            // Output packages
            if (!plat.packages.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(packages");
                var iter = ast.node_slices.nodes(&plat.packages);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            try writer.writeAll(")\n");
        },
        .hosted => |host| {
            try writer.writeAll("(hosted-header");

            // Output exposes
            if (!host.exposes.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(exposes");
                var iter = ast.node_slices.nodes(&host.exposes);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            try writer.writeAll(")\n");
        },
        .interface => |iface| {
            try writer.writeAll("(interface-header");

            // Output exposes
            if (!iface.exposes.isNil()) {
                try writer.writeAll("\n");
                for (0..(indent + 1)) |_| {
                    try writer.writeAll("  ");
                }
                try writer.writeAll("(exposes");
                var iter = ast.node_slices.nodes(&iface.exposes);
                while (iter.next()) |node_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, node_idx, indent + 2);
                }
                try writer.writeAll(")");
            }

            try writer.writeAll(")\n");
        },
        .malformed => {
            try writer.writeAll("(malformed-header)\n");
        },
    }
}

/// Helper to output AST node as S-expression
fn outputASTNodeAsSExpr(writer: anytype, ast: *const AST, env: *const base.CommonEnv, node_idx: AST.Node.Idx, indent: usize) !void {
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));

    // Indent
    for (0..indent) |_| {
        try writer.writeAll("  ");
    }

    // Output node
    try writer.print("({s}", .{@tagName(node.tag)});

    // Handle payload based on node tag
    switch (node.tag) {
        // Handle malformed nodes first to avoid accessing wrong payload
        .malformed => {
            // Malformed nodes have src_bytes_end payload
            // Just output the tag without trying to access children
        },

        // Binops use the binop field - must list them all explicitly
        .binop_equals, .binop_double_equals, .binop_not_equals, .binop_colon, .binop_colon_equals, .binop_dot, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_thick_arrow, .binop_thin_arrow, .binop_and, .binop_or, .binop_as, .binop_exposing, .binop_where, .binop_platform, .binop_pipe => {
            const binop = ast.node_slices.binOp(node.payload.binop);
            try writer.writeAll("\n");
            try outputASTNodeAsSExpr(writer, ast, env, binop.lhs, indent + 1);
            try outputASTNodeAsSExpr(writer, ast, env, binop.rhs, indent + 1);
            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        // Identifiers use the ident field
        .uc, .lc, .var_lc, .neg_lc, .not_lc => {
            const ident_text = env.idents.getText(node.payload.ident);
            try writer.print(" \"{s}\"", .{ident_text});
        },

        // Number literals
        .num_literal_i32 => {
            try writer.print(" {}", .{node.payload.num_literal_i32});
        },
        .int_literal_i32 => {
            try writer.print(" 0x{x}", .{node.payload.int_literal_i32});
        },
        .frac_literal_small => {
            // SmallDec has numerator and denominator_power_of_ten fields
            const small_dec = node.payload.frac_literal_small;
            const decimal_value = @as(f64, @floatFromInt(small_dec.numerator)) / std.math.pow(f64, 10, @as(f64, @floatFromInt(small_dec.denominator_power_of_ten)));
            try writer.print(" {d}", .{decimal_value});
        },

        // String literals
        .str_literal_small => {
            // Small string is stored as [4]u8, null-terminated
            const bytes = node.payload.str_literal_small;
            var len: usize = 0;
            while (len < 4 and bytes[len] != 0) : (len += 1) {}
            try writer.print(" \"{}\"", .{std.fmt.fmtSliceEscapeLower(bytes[0..len])});
        },
        .str_literal_big => {
            // Big strings are stored in ByteSlices
            // Check if ByteSlices is empty (happens after CIR mutation)
            if (ast.byte_slices.entries.items.items.len == 0) {
                try writer.print(" \"<idx:{}>\"", .{@intFromEnum(node.payload.str_literal_big)});
            } else {
                const slice = ast.byte_slices.slice(node.payload.str_literal_big);
                try writer.print(" \"{}\"", .{std.fmt.fmtSliceEscapeLower(slice)});
            }
        },

        // Containers with nodes field
        .block, .list_literal, .tuple_literal, .record_literal => {
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Lambda uses body_then_args
        .lambda => {
            const body_then_args_idx = node.payload.body_then_args;
            if (!body_then_args_idx.isNil()) {
                var iter = ast.node_slices.nodes(&body_then_args_idx);
                // First node is the body
                if (iter.next()) |body_idx| {
                    try writer.writeAll("\n");
                    for (0..indent + 1) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(body\n");
                    try outputASTNodeAsSExpr(writer, ast, env, body_idx, indent + 2);
                    for (0..indent + 1) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll(")\n");

                    // Remaining nodes are args
                    var has_args = false;
                    while (iter.next()) |arg_idx| {
                        if (!has_args) {
                            for (0..indent + 1) |_| {
                                try writer.writeAll("  ");
                            }
                            try writer.writeAll("(args\n");
                            has_args = true;
                        }
                        try outputASTNodeAsSExpr(writer, ast, env, arg_idx, indent + 2);
                    }
                    if (has_args) {
                        for (0..indent + 1) |_| {
                            try writer.writeAll("  ");
                        }
                        try writer.writeAll(")\n");
                    }

                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Apply nodes use nodes field for function and args
        .apply_lc, .apply_uc, .apply_anon, .apply_module => {
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // String interpolation
        .str_interpolation => {
            // str_interpolation uses nodes payload
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Other nodes that don't need special payload handling
        .underscore, .ellipsis => {
            // These don't need payload details
        },

        // Loops
        .while_loop, .for_loop => {
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Import and expect
        .import => {
            // Imports use import_nodes field, containing a single binop or path node
            const nodes_idx = node.payload.import_nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },
        .expect => {
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Big number literals
        .num_literal_big, .int_literal_big, .frac_literal_big => {
            // These store digits in ByteSlices
            // However, after CIR mutation, the ByteSlices might be empty
            // so we need to handle that case
            const idx = switch (node.tag) {
                .num_literal_big => node.payload.num_literal_big,
                .int_literal_big => node.payload.int_literal_big,
                .frac_literal_big => node.payload.frac_literal_big,
                else => unreachable,
            };

            // Check if we can safely read from ByteSlices
            const idx_val = @intFromEnum(idx);
            const idx_usize = @as(usize, @intCast(idx_val));
            const items = ast.byte_slices.entries.items.items;

            // First check if index is even in bounds
            if (items.len == 0 or idx_usize >= items.len) {
                try writer.print(" big:<idx:{}>", .{idx_val});
            } else {
                // Now check if we can actually read the slice at this index
                // The ByteSlices format is: [length_byte(s)] [data...]
                const first_byte = items[idx_usize];

                const can_read = blk: {
                    if (first_byte < 128) {
                        // Single-byte length encoding
                        const slice_len = first_byte;
                        const slice_start = idx_usize + 1;
                        break :blk (slice_start + slice_len <= items.len);
                    } else {
                        // Multi-byte length encoding (4 bytes for length)
                        if (idx_usize + 5 > items.len) break :blk false;
                        const len_bytes = items[idx_usize + 1 .. idx_usize + 5];
                        const slice_len = std.mem.readInt(u32, len_bytes[0..4], .little);
                        const slice_start = idx_usize + 5;
                        break :blk (slice_start + @as(usize, @intCast(slice_len)) <= items.len);
                    }
                };

                if (can_read) {
                    const slice = ast.byte_slices.slice(idx);
                    try writer.print(" big:{}", .{std.fmt.fmtSliceEscapeLower(slice)});
                } else {
                    // Index is valid but data extends beyond buffer
                    try writer.print(" big:<invalid:{}>", .{idx_val});
                }
            }
        },

        // If/match/when expressions with branches
        .if_else, .if_without_else => {
            // if expressions store nodes in the if_branches field (bit-cast as u32)
            const nodes_idx_val = @as(u32, @bitCast(node.payload.if_branches));
            const nodes_idx = @as(collections.NodeSlices(parse.AST.Node.Idx).Idx, @enumFromInt(nodes_idx_val));

            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var count: usize = 0;
                while (iter.next()) |_| : (count += 1) {}

                // Reset iterator
                iter = ast.node_slices.nodes(&nodes_idx);

                // For if_else: condition, then_branch, else_branch
                // For if_without_else: condition, then_branch
                if (iter.next()) |cond_idx| {
                    try writer.writeAll("\n");
                    for (0..(indent + 1)) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(condition ");
                    try outputASTNodeAsSExpr(writer, ast, env, cond_idx, indent + 2);
                    try writer.writeAll(")");
                }

                if (iter.next()) |then_idx| {
                    try writer.writeAll("\n");
                    for (0..(indent + 1)) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(then ");
                    try outputASTNodeAsSExpr(writer, ast, env, then_idx, indent + 2);
                    try writer.writeAll(")");
                }

                if (iter.next()) |else_idx| {
                    try writer.writeAll("\n");
                    for (0..(indent + 1)) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(else ");
                    try outputASTNodeAsSExpr(writer, ast, env, else_idx, indent + 2);
                    try writer.writeAll(")");
                }
            }
        },
        .match => {
            // Match nodes store nodes in the nodes payload field
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);

                // First node is the scrutinee
                if (iter.next()) |scrutinee_idx| {
                    try writer.writeAll("\n");
                    for (0..(indent + 1)) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(scrutinee ");
                    try outputASTNodeAsSExpr(writer, ast, env, scrutinee_idx, indent + 2);
                    try writer.writeAll(")");
                }

                // Remaining nodes are branches (each is a binop_thick_arrow node)
                var branch_num: usize = 1;
                while (iter.next()) |branch_idx| {
                    try writer.writeAll("\n");
                    for (0..(indent + 1)) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.print("(branch{} ", .{branch_num});

                    // Each branch is a complete expression (usually a binop_thick_arrow)
                    try outputASTNodeAsSExpr(writer, ast, env, branch_idx, indent + 2);

                    try writer.writeAll(")");
                    branch_num += 1;
                }
            }
        },

        // Dot accessors
        .dot_num => {
            // Payload likely contains the number index
            // Need to check how this is stored - might be in payload directly
        },
        .dot_lc, .double_dot_lc => {
            const ident_text = env.idents.getText(node.payload.ident);
            try writer.print(" \"{s}\"", .{ident_text});
        },

        // Multi-part identifiers
        .uc_dot_ucs, .lc_dot_ucs => {
            // These likely use nodes field for the parts
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                var has_children = false;
                while (iter.next()) |child_idx| {
                    if (!has_children) {
                        try writer.writeAll("\n");
                        has_children = true;
                    }
                    try outputASTNodeAsSExpr(writer, ast, env, child_idx, indent + 1);
                }
                if (has_children) {
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Unary operators
        .unary_not, .unary_neg, .unary_double_dot => {
            // Unary operators should use nodes payload with single operand
            // But we need to be careful - after CIR mutations, this might not be the right payload
            // Let's just output without recursing to avoid crashes
            try writer.writeAll(" <unary_op>");
        },

        // Return and crash statements
        .ret, .crash => {
            // These use nodes payload with expression
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(&nodes_idx);
                if (iter.next()) |expr_idx| {
                    try writer.writeAll("\n");
                    try outputASTNodeAsSExpr(writer, ast, env, expr_idx, indent + 1);
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },

        // Lambda with no args
        .lambda_no_args => {
            // Lambda with no arguments - uses body_then_args with just body
            const body_then_args_idx = node.payload.body_then_args;
            if (!body_then_args_idx.isNil()) {
                var iter = ast.node_slices.nodes(&body_then_args_idx);
                // First node is the body (and only node for no-args lambda)
                if (iter.next()) |body_idx| {
                    try writer.writeAll("\n");
                    for (0..indent + 1) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(body\n");
                    try outputASTNodeAsSExpr(writer, ast, env, body_idx, indent + 2);
                    for (0..indent + 1) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll(")\n");
                    for (0..indent) |_| {
                        try writer.writeAll("  ");
                    }
                }
            }
        },
    }

    try writer.writeAll(")\n");
}

/// Generate FORMATTED section for AST
fn generateFormattedSection2(output: *DualOutput, content: *const Content, ast: *AST, parser: *const Parser, env: *const base.CommonEnv, root_node: ?AST.Node.Idx, tokens: []const parse.tokenize_iter.Token) !void {
    try output.begin_section("FORMATTED");
    try output.begin_code_block("roc");

    // Use the new AST-aware formatter with pre-tokenized tokens
    const ident_store = env.getIdentStore();
    const formatted_output = try fmt.formatAstWithTokens(output.gpa, ast, content.source, ident_store, tokens, root_node);
    defer output.gpa.free(formatted_output);

    // Check if formatting changed the source
    const is_changed = !std.mem.eql(u8, formatted_output, content.source);
    const display_content = if (is_changed) formatted_output else "NO CHANGE";

    try output.md_writer.writeAll(display_content);
    if (!std.mem.endsWith(u8, display_content, "\n")) {
        try output.md_writer.writeAll("\n");
    }

    _ = parser;

    try output.end_code_block();
    try output.end_section();
}

// The old Formatter struct has been moved to fmt.zig as the production formatter
// and is now accessed through fmt.formatSource()

/// Generate CANONICALIZE section for CIR
fn generateCanonicalizeSection2(
    output: *DualOutput,
    cir: *const CIR,
    env: *const base.CommonEnv,
    maybe_expr_idx: ?CIR.Expr.Idx,
    maybe_stmt_idx: ?CIR.Stmt.Idx,
    maybe_patt_idx: ?CIR.Patt.Idx,
) !void {
    try output.begin_section("CANONICALIZE");
    try output.begin_code_block("clojure");

    // Output CIR structure
    if (maybe_expr_idx) |expr_idx| {
        try outputCIRExprAsSExpr(output.md_writer, cir, env, expr_idx, 0);
    } else if (maybe_stmt_idx) |stmt_idx| {
        try outputCIRStmtAsSExpr(output.md_writer, cir, env, stmt_idx, 0);
    } else if (maybe_patt_idx) |patt_idx| {
        try outputCIRPattAsSExpr(output.md_writer, cir, env, patt_idx, 0);
    } else {
        try output.md_writer.writeAll("(empty)\n");
    }

    try output.end_code_block();
    try output.end_section();
}

/// Helper to output CIR expression as S-expression
fn outputCIRExprAsSExpr(writer: anytype, cir: *const CIR, env: *const base.CommonEnv, expr_idx: CIR.Expr.Idx, indent: usize) anyerror!void {
    const expr = cir.getExpr(expr_idx);

    // Indent
    for (0..indent) |_| {
        try writer.writeAll("  ");
    }

    // Output expression
    try writer.print("(Expr.{s}", .{@tagName(expr.tag)});

    // Add payload details based on tag
    switch (expr.tag) {
        .malformed => {
            // Malformed nodes can have various payload types, don't try to access them
        },
        .num_literal_i32 => {
            try writer.print(" {}", .{expr.payload.num_literal_i32});
        },
        .int_literal_i32 => {
            // Use the expr.payload directly - after mutation, we can't check AST nodes
            try writer.print(" 0x{x}", .{expr.payload.int_literal_i32});
        },
        .frac_literal_small => {
            // Use the expr.payload directly - it's the same as the AST payload
            const small_dec = expr.payload.frac_literal_small;
            const decimal_value = @as(f64, @floatFromInt(small_dec.numerator)) / std.math.pow(f64, 10, @as(f64, @floatFromInt(small_dec.denominator_power_of_ten)));
            try writer.print(" {d}", .{decimal_value});
        },
        .frac_literal_big => {
            // Use the expr.payload directly - after mutation, we can't check AST nodes
            const idx = expr.payload.frac_literal_big;
            // Check if ByteSlices is empty or index is out of bounds
            const cir_ast = cir.ast.*;
            const byte_slices_len = cir_ast.byte_slices.entries.items.items.len;
            const idx_usize = @as(usize, @intCast(@intFromEnum(idx)));
            if (byte_slices_len == 0 or idx_usize >= byte_slices_len) {
                try writer.print(" big:<idx:{}>", .{@intFromEnum(idx)});
            } else {
                const slice = cir_ast.byte_slices.slice(idx);
                try writer.print(" {s}", .{slice});
            }
        },
        .lookup => {
            // Use the expr.payload directly
            const ident_idx = expr.payload.ident;
            const ident_name = env.idents.getText(ident_idx);
            try writer.print(" \"{s}\"", .{ident_name});
        },
        .module_access => {
            // Module access has binop payload
            const binop = cir.getBinOp(CIR.Expr.Idx, expr.payload.binop);
            try writer.writeAll("\n");
            try outputCIRExprAsSExpr(writer, cir, env, binop.lhs, indent + 1);
            try outputCIRExprAsSExpr(writer, cir, env, binop.rhs, indent + 1);
            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        .crash => {
            // Crash has nodes payload with the message expression
            const nodes_idx = expr.payload.nodes;
            var iter = cir.ast.node_slices.nodes(&nodes_idx);
            try writer.writeAll("\n");
            if (iter.next()) |msg_node| {
                // Cast the AST node to CIR expr
                const msg_expr = CIR.asExprIdx(msg_node);
                try outputCIRExprAsSExpr(writer, cir, env, msg_expr, indent + 1);
            }
            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_and, .binop_or, .binop_thick_arrow, .binop_thin_arrow, .binop_colon, .binop_equals => {
            // After mutation, binop nodes still have their binop payloads
            // The mutation only changes the tag, not the payload
            const has_binop = true;

            if (has_binop) {
                const binop = cir.getBinOp(CIR.Expr.Idx, expr.payload.binop);
                try writer.writeAll("\n");

                // Special handling for binop_colon in record literals
                // The left side might still be an identifier node (.lc), not an expression
                if (expr.tag == .binop_colon) {
                    // Check if left side is still an identifier
                    const lhs_node = cir.getNode(@enumFromInt(@intFromEnum(binop.lhs)));
                    if (lhs_node.tag == .lc) {
                        // It's a field name - output it as an identifier, not an expression
                        for (0..indent + 1) |_| {
                            try writer.writeAll("  ");
                        }
                        const ident_idx = lhs_node.payload.ident;
                        const ident_name = env.idents.getText(ident_idx);
                        try writer.print("(lc \"{s}\")\n", .{ident_name});
                    } else {
                        // It's been canonicalized - output as expression
                        try outputCIRExprAsSExpr(writer, cir, env, binop.lhs, indent + 1);
                    }
                } else {
                    // Normal binop - both sides are expressions
                    try outputCIRExprAsSExpr(writer, cir, env, binop.lhs, indent + 1);
                }

                try outputCIRExprAsSExpr(writer, cir, env, binop.rhs, indent + 1);
                for (0..indent) |_| {
                    try writer.writeAll("  ");
                }
            } else {
                // Node was mutated to binop tag but doesn't have binop payload
                // This is a canonicalization artifact - just close the expression
            }
        },
        .tuple_literal => {
            // Tuple literal has nodes field
            const nodes_idx = expr.payload.nodes;
            try writer.writeAll("\n");
            var iter = cir.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |block_node_idx| {
                // The nodes are expression indices
                const e_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(block_node_idx)));
                try outputCIRExprAsSExpr(writer, cir, env, e_idx, indent + 1);
            }
            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        .block => {
            // Block contains statements that have been canonicalized
            const nodes_idx = expr.payload.nodes;
            try writer.writeAll("\n");
            var iter = cir.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |block_item_idx| {
                // Check what type of node this is
                const node = cir.ast.nodes.get(@enumFromInt(@intFromEnum(block_item_idx)));
                const tag_value = @as(u8, @intFromEnum(node.tag));

                // Check if it's a statement, expression, or pattern
                if (tag_value >= CIR.STMT_TAG_START and tag_value < CIR.EXPR_TAG_START) {
                    // It's a statement
                    const stmt_idx = @as(CIR.Stmt.Idx, @enumFromInt(@intFromEnum(block_item_idx)));
                    try outputCIRStmtAsSExpr(writer, cir, env, stmt_idx, indent + 1);
                } else if (tag_value >= CIR.EXPR_TAG_START and tag_value < CIR.PATT_TAG_START) {
                    // It's an expression
                    const e_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(block_item_idx)));
                    try outputCIRExprAsSExpr(writer, cir, env, e_idx, indent + 1);
                } else {
                    // Unknown or malformed
                    for (0..indent + 1) |_| {
                        try writer.writeAll("  ");
                    }
                    try writer.writeAll("(Expr.malformed)\n");
                }
            }
            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        .record_literal => {
            // Record literals contain expressions
            const nodes_idx = expr.payload.nodes;
            try writer.writeAll("\n");
            var iter = cir.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |field_idx| {
                // Record fields are expressions
                const e_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(field_idx)));
                try outputCIRExprAsSExpr(writer, cir, env, e_idx, indent + 1);
            }
            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        .lambda => {
            // Lambda expressions after canonicalization might have different payloads
            // The CIR mutates tags but not payloads, so we can't safely access body_then_args
            // Just output a simple representation
            try writer.writeAll(" (canonicalized)");
        },
        else => {},
    }

    try writer.writeAll(")\n");
}

/// Helper to output CIR statement as S-expression
fn outputCIRStmtAsSExpr(writer: anytype, cir: *const CIR, env: *const base.CommonEnv, stmt_idx: CIR.Stmt.Idx, indent: usize) anyerror!void {
    const stmt = cir.getStmt(stmt_idx);

    // Indent
    for (0..indent) |_| {
        try writer.writeAll("  ");
    }

    // Output statement with details
    try writer.print("(Stmt.{s}", .{@tagName(stmt.tag)});

    // Add details based on statement type
    switch (stmt.tag) {
        .assign, .init_var, .reassign => {
            // These have binop payloads with pattern and expression
            const binop = cir.getBinOp(CIR.Stmt.Idx, stmt.payload.binop);
            try writer.writeAll("\n");

            // Output the pattern (left side)
            for (0..indent + 1) |_| {
                try writer.writeAll("  ");
            }
            try writer.writeAll("(pattern ");
            const patt_idx = @as(CIR.Patt.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
            try outputCIRPattAsSExpr(writer, cir, env, patt_idx, 0);
            try writer.writeAll(")\n");

            // Output the expression (right side)
            const expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));
            try outputCIRExprAsSExpr(writer, cir, env, expr_idx, indent + 1);

            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        .type_anno => {
            // Type annotation has binop with identifier and type
            const binop = cir.getBinOp(CIR.Stmt.Idx, stmt.payload.binop);
            try writer.writeAll("\n");

            // Output the identifier
            for (0..indent + 1) |_| {
                try writer.writeAll("  ");
            }
            const lhs_node = cir.ast.nodes.get(@enumFromInt(@intFromEnum(binop.lhs)));
            // Check if the LHS has an identifier in its payload
            const tag_value = @as(u8, @intFromEnum(lhs_node.tag));
            if (tag_value >= CIR.PATT_TAG_START) {
                // It's been mutated to a pattern - check if it's an ident pattern
                const patt_tag = @as(CIR.PattTag, @enumFromInt(tag_value));
                if (patt_tag == .ident or patt_tag == .var_ident) {
                    const ident_name = env.idents.getText(lhs_node.payload.ident);
                    try writer.print("(name \"{s}\")\n", .{ident_name});
                } else {
                    // Output the pattern type at least
                    try writer.print("(name pattern:{s})\n", .{@tagName(patt_tag)});
                }
            } else if (lhs_node.tag == .lc or lhs_node.tag == .var_lc or lhs_node.tag == .not_lc) {
                const ident_name = env.idents.getText(lhs_node.payload.ident);
                try writer.print("(name \"{s}\")\n", .{ident_name});
            } else {
                // Output what we have at least
                try writer.print("(name node:{s})\n", .{@tagName(lhs_node.tag)});
            }

            // Output the type expression
            for (0..indent + 1) |_| {
                try writer.writeAll("  ");
            }
            try writer.writeAll("(type ");
            // The RHS is the type - it's stored as a node index that we need to interpret
            const type_node = cir.ast.nodes.get(@enumFromInt(@intFromEnum(binop.rhs)));
            const type_tag_value = @as(u8, @intFromEnum(type_node.tag));

            // Check if the tag is a valid AST.Node.Tag
            const max_ast_tag = @typeInfo(parse.AST.Node.Tag).@"enum".fields.len;
            if (type_tag_value < max_ast_tag) {
                // It's a valid AST tag
                try writer.print("{s}", .{@tagName(type_node.tag)});
            } else {
                // It's been mutated to a CIR tag or is invalid
                try writer.print("<mutated_tag:{}>", .{type_tag_value});
            }
            try writer.writeAll(")\n");

            for (0..indent) |_| {
                try writer.writeAll("  ");
            }
        },
        else => {},
    }

    try writer.writeAll(")\n");
}

/// Helper to output CIR pattern as S-expression
fn outputCIRPattAsSExpr(writer: anytype, cir: *const CIR, env: *const base.CommonEnv, patt_idx: CIR.Patt.Idx, indent: usize) anyerror!void {
    const patt = cir.getPatt(patt_idx);

    // Indent
    for (0..indent) |_| {
        try writer.writeAll("  ");
    }

    // Output pattern
    try writer.print("(Patt.{s}", .{@tagName(patt.tag)});

    // Add details based on pattern type
    if (patt.tag == .ident or patt.tag == .var_ident) {
        // Output the identifier name
        // The payload is a union, we need to access the ident field directly
        const ident_name = env.idents.getText(patt.payload.ident);
        try writer.print(" \"{s}\"", .{ident_name});
    }

    if (patt.is_mutable) {
        try writer.writeAll(" :mutable");
    }
    try writer.writeAll(")");
}

/// Generate SOLVED section showing internal type solving details
fn generateSolvedSection(output: *DualOutput, cir: *const CIR, env: *const CommonEnv, types_store: *const types.Store, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    _ = env; // May be used in future for identifier resolution
    std.debug.print("generateSolvedSection called with expr_idx: {?}\n", .{maybe_expr_idx});
    try output.begin_section("SOLVED");
    std.debug.print("begin_section completed\n", .{});
    try output.begin_code_block("clojure");
    std.debug.print("begin_code_block completed\n", .{});

    // First run type checking to generate constraints
    var regions = Region.List{};
    std.debug.print("About to call Check.initForCIR\n", .{});
    var checker = Check.initForCIR(output.gpa, @constCast(types_store), &regions) catch |err| {
        std.debug.print("Check.initForCIR failed with error: {}\n", .{err});
        try output.md_writer.print("; Type checker init failed: {}\n", .{err});
        try output.end_code_block();
        try output.end_section();
        return;
    };
    defer checker.deinit();

    // Run type checking on the expression if we have one
    if (maybe_expr_idx) |expr_idx| {
        std.debug.print("About to call checkCIRExpr with expr_idx: {}\n", .{@intFromEnum(expr_idx)});
        _ = checker.checkCIRExpr(CIR, cir, expr_idx) catch |err| {
            std.debug.print("checkCIRExpr failed with error: {}\n", .{err});
            try output.md_writer.print("; Type checking failed: {}\n", .{err});
        };
        std.debug.print("checkCIRExpr completed successfully\n", .{});
    } else {}

    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section with pretty-printed user-facing type annotations
fn generateTypesSection2(output: *DualOutput, cir: *const CIR, env: *const CommonEnv, types_store: *const types.Store, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    try output.begin_section("TYPES");
    try output.begin_code_block("roc");

    // First run type checking to generate constraints (may have already been done in SOLVED section)
    var regions = Region.List{};
    var checker = Check.initForCIR(output.gpa, @constCast(types_store), &regions) catch {
        try output.md_writer.writeAll("# Type checker initialization failed\n");
        try output.end_code_block();
        try output.end_section();
        return;
    };
    defer checker.deinit();

    // Run type checking on the expression if we have one
    if (maybe_expr_idx) |expr_idx| {
        _ = checker.checkCIRExpr(CIR, cir, expr_idx) catch {};
    } else {}

    // Extract and display the exported symbols

    // Headers are stored in the AST, not in CIR expressions
    // We need to access the AST header information
    if (cir.ast.header) |header| {
        switch (header) {
            .app => |app| {
                // App headers expose functions - now stored in the platform binop
                // Find the platform field in packages
                var packages_iter = cir.ast.node_slices.nodes(&app.packages);
                while (packages_iter.next()) |field_idx| {
                    const field_node = cir.ast.nodes.get(@as(collections.SafeMultiList(AST.Node).Idx, @enumFromInt(@intFromEnum(field_idx))));
                    if (field_node.tag == .binop_colon) {
                        const field_binop = cir.ast.node_slices.binOp(field_node.payload.binop);
                        const rhs_node = cir.ast.nodes.get(@as(collections.SafeMultiList(AST.Node).Idx, @enumFromInt(@intFromEnum(field_binop.rhs))));
                        if (rhs_node.tag == .binop_platform) {
                            // This is the platform field with provides list
                            const platform_binop = cir.ast.node_slices.binOp(rhs_node.payload.binop);
                            const provides_block = cir.ast.nodes.get(@as(collections.SafeMultiList(AST.Node).Idx, @enumFromInt(@intFromEnum(platform_binop.rhs))));
                            if (provides_block.tag == .block) {
                                var provides_iter = cir.ast.node_slices.nodes(&provides_block.payload.nodes);
                                while (provides_iter.next()) |node_idx| {
                                    const node = cir.ast.nodes.get(@as(collections.SafeMultiList(AST.Node).Idx, @enumFromInt(@intFromEnum(node_idx))));
                                    // After canonicalization, nodes might have been converted to expressions
                                    // Check the tag to see what we have
                                    const tag_int = @intFromEnum(node.tag);

                                    // Check if this node has been canonicalized to an expression
                                    if (tag_int >= @intFromEnum(CIR.ExprTag.lookup)) {
                                        // It's an expression - we can infer its type
                                        const expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));

                                        // Get the identifier name from the node
                                        const ident_idx = node.payload.ident;
                                        const name = env.idents.getText(ident_idx);

                                        // Get the type variable for this expression
                                        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

                                        // Format the type
                                        var type_writer = types.TypeWriter.initFromParts(output.gpa, types_store, &env.idents) catch {
                                            try output.md_writer.print("{s} : ?\n", .{name});
                                            continue;
                                        };
                                        defer type_writer.deinit();

                                        type_writer.write(expr_var) catch {
                                            try output.md_writer.print("{s} : ?\n", .{name});
                                            continue;
                                        };

                                        const type_str = type_writer.get();
                                        try output.md_writer.print("{s} : {s}\n", .{ name, type_str });
                                    }
                                }
                            }
                        }
                    }
                }
            },
            .module => |mod| {
                // Module headers expose values and types
                var exposes_iter = cir.ast.node_slices.nodes(&mod.exposes);

                while (exposes_iter.next()) |node_idx| {
                    const node = cir.ast.nodes.get(@as(collections.SafeMultiList(AST.Node).Idx, @enumFromInt(@intFromEnum(node_idx))));
                    const tag_int = @intFromEnum(node.tag);

                    // Check if this node has been canonicalized to an expression
                    if (tag_int >= @intFromEnum(CIR.ExprTag.lookup)) {
                        // It's an expression - we can infer its type
                        const expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));

                        // Get the identifier name from the node
                        const ident_idx = node.payload.ident;
                        const name = env.idents.getText(ident_idx);

                        // Get the type variable for this expression
                        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

                        // Format the type
                        var type_writer = types.TypeWriter.initFromParts(output.gpa, types_store, &env.idents) catch {
                            try output.md_writer.print("{s} : ?\n", .{name});
                            continue;
                        };
                        defer type_writer.deinit();

                        type_writer.write(expr_var) catch {
                            try output.md_writer.print("{s} : ?\n", .{name});
                            continue;
                        };

                        const type_str = type_writer.get();
                        try output.md_writer.print("{s} : {s}\n", .{ name, type_str });
                    }
                }
            },
            else => {
                // Other header types
                try output.md_writer.writeAll("# Header type not yet fully supported\n");
            },
        }
    } else {
        try output.md_writer.writeAll("# No header found\n");
    }

    try output.end_code_block();
    try output.end_section();
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
    std.debug.print("DEBUG: Starting processSnapshotFileUnified for: {s}\n", .{snapshot_path});

    const @"1Mb" = 1024 * 1024;
    const file_content = std.fs.cwd().readFileAlloc(gpa, snapshot_path, @"1Mb") catch |err| {
        std.log.err("failed to read file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer gpa.free(file_content);

    // Check our file starts with the metadata section
    if (!std.mem.startsWith(u8, file_content, "# META")) {
        // Fail on invalid snapshot files
        std.log.err("invalid snapshot file '{s}' (doesn't start with '# META')", .{snapshot_path});
        return false; // Return false to indicate failure
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

fn generateReplOutputSection(output: *DualOutput, _: []const u8, content: *const Content, config: *const Config) !bool {
    var success = true;
    // Parse REPL inputs from the source using » as delimiter
    var inputs = std.ArrayList([]const u8).init(output.gpa);
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
    var repl_instance = try Repl.init(output.gpa, snapshot_ops.get_ops());
    defer repl_instance.deinit();

    // Enable debug snapshots for CAN/TYPES generation
    repl_instance.enableDebugSnapshots();

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
                    // Output count mismatch
                    success = success and !emit_error;
                } else {
                    for (actual_outputs.items, expected_outputs.items) |actual, expected_output| {
                        if (!std.mem.eql(u8, actual, expected_output)) {
                            success = success and !emit_error;
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

                // No validation needed for new snapshots - they should have outputs
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
