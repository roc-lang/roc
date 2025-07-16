//! Tests for validating that EXPECTED sections in snapshots match their PROBLEMS sections.
//! This ensures that the expected problems documented in each snapshot file accurately
//! reflect the actual problems reported by the compiler.

const std = @import("std");
const testing = std.testing;
const snapshot_mod = @import("snapshot.zig");
const NodeType = snapshot_mod.NodeType;
const base = @import("base.zig");
const RegionInfo = base.RegionInfo;
const parse = @import("check/parse.zig");
const canonicalize = @import("check/canonicalize.zig");
const check_types = @import("check/check_types.zig");
const CIR = @import("check/canonicalize/CIR.zig");
const eval = @import("eval/interpreter.zig");
const stack = @import("eval/stack.zig");
const layout_store = @import("layout/store.zig");
const reporting = @import("reporting.zig");

/// Represents a problem entry from either EXPECTED or PROBLEMS section
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

    fn equals(self: ProblemEntry, other: ProblemEntry) bool {
        return std.mem.eql(u8, self.problem_type, other.problem_type) and
            std.mem.eql(u8, self.file, other.file) and
            self.start_line == other.start_line and
            self.start_col == other.start_col and
            self.end_line == other.end_line and
            self.end_col == other.end_col;
    }
};

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

/// Parse a PROBLEMS entry
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

/// Extract section content between headers
fn extractSection(content: []const u8, section_name: []const u8) ?[]const u8 {
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

/// Compare expected and actual problems for a single snapshot
fn validateSnapshotProblems(allocator: std.mem.Allocator, path: []const u8) !void {
    const content = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(content);

    // Extract META section to get the type
    const meta_section = extractSection(content, "META") orelse {
        std.debug.print("\n❌ {s}:\n", .{std.fs.path.basename(path)});
        std.debug.print("  Missing META section\n", .{});
        return error.MissingMetaSection;
    };

    // Parse the META section to get the node type
    const node_type = blk: {
        var lines = std.mem.tokenizeScalar(u8, meta_section, '\n');
        while (lines.next()) |line| {
            const trimmed_line = std.mem.trim(u8, line, " \t\r");
            if (std.mem.startsWith(u8, trimmed_line, "type=")) {
                const type_str = trimmed_line["type=".len..];
                // Parse the type string manually since fromString is not public
                if (std.mem.eql(u8, type_str, NodeType.HEADER)) break :blk NodeType.header;
                if (std.mem.eql(u8, type_str, NodeType.EXPR)) break :blk NodeType.expr;
                if (std.mem.eql(u8, type_str, NodeType.STMT)) break :blk NodeType.statement;
                if (std.mem.eql(u8, type_str, NodeType.FILE)) break :blk NodeType.file;
                if (std.mem.eql(u8, type_str, NodeType.PACKAGE)) break :blk NodeType.package;
                if (std.mem.eql(u8, type_str, NodeType.PLATFORM)) break :blk NodeType.platform;
                if (std.mem.eql(u8, type_str, NodeType.APP)) break :blk NodeType.app;
                if (std.mem.eql(u8, type_str, NodeType.REPL)) break :blk NodeType.repl;
                // Unknown type
                std.debug.print("\n❌ {s}:\n", .{std.fs.path.basename(path)});
                std.debug.print("  Error: Unknown type '{s}' in META section\n", .{type_str});
                std.debug.print("  Valid types are: file, header, expr, statement, package, platform, app, repl\n", .{});
                return error.InvalidNodeType;
            }
        }
        break :blk NodeType.file; // default to file if type not specified
    };

    const expected_section = extractSection(content, "EXPECTED") orelse {
        // EXPECTED section is required for all snapshots
        std.debug.print("\n❌ {s}:\n", .{std.fs.path.basename(path)});
        std.debug.print("  Missing EXPECTED section\n", .{});
        return error.MissingExpectedSection;
    };

    const problems_section = extractSection(content, "PROBLEMS") orelse {
        // PROBLEMS section is required for all snapshots
        std.debug.print("\n❌ {s}:\n", .{std.fs.path.basename(path)});
        std.debug.print("  Missing PROBLEMS section\n", .{});
        return error.MissingProblemsSection;
    };

    // Check if PROBLEMS is exactly NIL
    const problems_is_nil = std.mem.eql(u8, std.mem.trim(u8, problems_section, " \t\r\n"), "NIL");

    // If PROBLEMS is NIL and type is not repl, EXPECTED must also be NIL
    if (problems_is_nil and node_type != .repl) {
        const expected_is_nil = std.mem.eql(u8, std.mem.trim(u8, expected_section, " \t\r\n"), "NIL");
        if (!expected_is_nil) {
            std.debug.print("\n❌ {s}:\n", .{std.fs.path.basename(path)});
            std.debug.print("  Error: EXPECTED must be NIL when PROBLEMS is NIL (for type={s})\n", .{@tagName(node_type)});
            std.debug.print("  Hint: EXPECTED should show expected problems, not expected output.\n", .{});
            std.debug.print("  If you want to evaluate the expression, use type=repl instead.\n", .{});
            return error.SnapshotValidationFailed;
        }
    }

    var expected = try parseExpectedSection(allocator, expected_section);
    defer {
        for (expected.items) |p| {
            allocator.free(p.problem_type);
            allocator.free(p.file);
        }
        expected.deinit();
    }

    var actual = try parseProblemsSection(allocator, problems_section);
    defer {
        for (actual.items) |p| {
            allocator.free(p.problem_type);
            allocator.free(p.file);
        }
        actual.deinit();
    }

    const snapshot_name = std.fs.path.basename(path);
    var mismatches = std.ArrayList([]const u8).init(allocator);
    defer {
        for (mismatches.items) |m| allocator.free(m);
        mismatches.deinit();
    }

    // Check for problems in EXPECTED but not in PROBLEMS
    for (expected.items) |exp| {
        var found = false;
        for (actual.items) |act| {
            if (exp.equals(act)) {
                found = true;
                break;
            }
        }

        if (!found) {
            const msg = try std.fmt.allocPrint(allocator, "Expected problem not found: {s} - {s}:{d}:{d}:{d}:{d}", .{
                exp.problem_type,
                exp.file,
                exp.start_line,
                exp.start_col,
                exp.end_line,
                exp.end_col,
            });
            try mismatches.append(msg);
        }
    }

    // Check for problems in PROBLEMS but not in EXPECTED
    for (actual.items) |act| {
        var found = false;
        for (expected.items) |exp| {
            if (act.equals(exp)) {
                found = true;
                break;
            }
        }

        if (!found) {
            const msg = try std.fmt.allocPrint(allocator, "Unexpected problem found: {s} - {s}:{d}:{d}:{d}:{d}", .{
                act.problem_type,
                act.file,
                act.start_line,
                act.start_col,
                act.end_line,
                act.end_col,
            });
            try mismatches.append(msg);
        }
    }

    if (mismatches.items.len > 0) {
        std.debug.print("\n❌ {s}:\n", .{snapshot_name});
        for (mismatches.items) |msg| {
            std.debug.print("  {s}\n", .{msg});
        }
        std.debug.print("Note, you can update the expectations with `zig build update-expected`.", .{});
        return error.SnapshotValidationFailed;
    }
}

fn collectSnapshotFiles(allocator: std.mem.Allocator) !std.ArrayList([]const u8) {
    var snapshot_files = std.ArrayList([]const u8).init(allocator);
    errdefer {
        for (snapshot_files.items) |f| allocator.free(f);
        snapshot_files.deinit();
    }

    var snapshots_dir = try std.fs.cwd().openDir("src/snapshots", .{ .iterate = true });
    defer snapshots_dir.close();

    var walker = try snapshots_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.path, ".md") and !std.mem.endsWith(u8, entry.path, "README.md")) {
            const full_path = try std.fs.path.join(allocator, &.{ "src/snapshots", entry.path });
            try snapshot_files.append(full_path);
        }
    }

    // Sort files for consistent output
    std.mem.sort([]const u8, snapshot_files.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);

    return snapshot_files;
}

test "snapshot validation" {
    const allocator = testing.allocator;

    var snapshot_files = try collectSnapshotFiles(allocator);
    defer {
        for (snapshot_files.items) |f| allocator.free(f);
        snapshot_files.deinit();
    }

    var total_failures: usize = 0;
    var failed_files = std.ArrayList([]const u8).init(allocator);
    defer failed_files.deinit();

    // Validate each snapshot
    for (snapshot_files.items) |snapshot_path| {
        validateSnapshotProblems(allocator, snapshot_path) catch |err| {
            if (err == error.SnapshotValidationFailed or err == error.MissingExpectedSection) {
                total_failures += 1;
                try failed_files.append(snapshot_path);
            } else {
                return err;
            }
        };
    }

    if (total_failures > 0) {
        std.debug.print("\n\n========================================\n", .{});
        std.debug.print("Snapshot validation summary: {} files with mismatches out of {} total\n", .{
            total_failures,
            snapshot_files.items.len,
        });
        std.debug.print("========================================\n\n", .{});
        return error.SnapshotValidationFailed;
    }
}

test "snapshot evaluate top-level `expect` statements" {
    const allocator = testing.allocator;

    var snapshot_files = try collectSnapshotFiles(allocator);
    defer {
        for (snapshot_files.items) |f| allocator.free(f);
        snapshot_files.deinit();
    }

    var total_failures: usize = 0;
    var total_expects: usize = 0;
    var total_skipped: usize = 0;
    var failed_files = std.ArrayList([]const u8).init(allocator);
    defer failed_files.deinit();

    // Evaluate expects in each snapshot
    for (snapshot_files.items) |snapshot_path| {
        const result = evaluateSnapshotExpects(allocator, snapshot_path) catch |err| {
            if (err == error.ExpectEvaluationFailed) {
                total_failures += 1;
                try failed_files.append(snapshot_path);
                continue;
            } else {
                return err;
            }
        };
        total_expects += result.expect_count;
        total_skipped += result.skipped_count;
    }

    if (total_failures > 0) {
        std.debug.print("\n\n========================================\n", .{});
        std.debug.print("Expect evaluation summary: {} files with failed expects out of {} total\n", .{
            total_failures,
            snapshot_files.items.len,
        });
        std.debug.print("========================================\n\n", .{});
        return error.ExpectEvaluationFailed;
    }
}

const EvaluationResult = struct {
    expect_count: usize,
    skipped_count: usize,
};

fn evaluateSnapshotExpects(allocator: std.mem.Allocator, snapshot_path: []const u8) !EvaluationResult {
    _ = allocator;
    _ = snapshot_path;
    // TODO: Re-enable after fixing flex variable issue
    return EvaluationResult{ .expect_count = 0, .skipped_count = 0 };
}

fn buildExpectFailureReport(
    allocator: std.mem.Allocator,
    snapshot_path: []const u8,
    source_code: []const u8,
    expect_expr_idx: CIR.Expr.Idx,
    cir: *CIR,
) !reporting.Report {
    var report = reporting.Report.init(allocator, "EXPECT FAILED", .runtime_error);

    try report.document.addReflowingText("This ");
    try report.document.addAnnotated("expect", .inline_code);
    try report.document.addReflowingText(" statement evaluated to ");
    try report.document.addAnnotated("False", .error_highlight);
    try report.document.addReflowingText(" but was expected to be ");
    try report.document.addAnnotated("True", .suggestion);
    try report.document.addReflowingText(".");
    try report.document.addLineBreak();
    try report.document.addLineBreak();

    try report.document.addReflowingText("The failing expect statement is in: ");
    try report.document.addAnnotated(snapshot_path, .inline_code);
    try report.document.addLineBreak();
    try report.document.addLineBreak();

    // Get the region of the expect expression for highlighting
    const expect_region = cir.store.getExprRegion(expect_expr_idx);

    // Set temporary source for region calculation
    cir.temp_source_for_sexpr = source_code;
    defer cir.temp_source_for_sexpr = null;

    const region_info = cir.calcRegionInfo(expect_region);

    try report.document.addSourceRegion(
        region_info,
        .error_highlight,
        snapshot_path,
    );

    return report;
}

// Unit tests
test "parseExpectedLine - valid line" {
    const line = "UNEXPECTED TOKEN IN EXPRESSION - test.md:1:10:1:15";
    const result = try parseExpectedLine(testing.allocator, line);
    defer if (result) |r| {
        testing.allocator.free(r.problem_type);
        testing.allocator.free(r.file);
    };

    try testing.expect(result != null);
    try testing.expectEqualStrings("UNEXPECTED TOKEN IN EXPRESSION", result.?.problem_type);
    try testing.expectEqualStrings("test.md", result.?.file);
    try testing.expectEqual(@as(u32, 1), result.?.start_line);
    try testing.expectEqual(@as(u32, 10), result.?.start_col);
    try testing.expectEqual(@as(u32, 1), result.?.end_line);
    try testing.expectEqual(@as(u32, 15), result.?.end_col);
}

test "parseExpectedSection - NIL" {
    const content = "NIL";
    var problems = try parseExpectedSection(testing.allocator, content);
    defer {
        for (problems.items) |p| {
            testing.allocator.free(p.problem_type);
            testing.allocator.free(p.file);
        }
        problems.deinit();
    }
    try testing.expect(problems.items.len == 0);
}

test "extractSection - EXPECTED section" {
    const content =
        \\# META
        \\some meta content
        \\# EXPECTED
        \\expected content
        \\more expected
        \\# PROBLEMS
        \\problems content
    ;

    const expected = extractSection(content, "EXPECTED");
    try testing.expect(expected != null);
    try testing.expectEqualStrings("expected content\nmore expected", expected.?);
}

test "extractSection - PROBLEMS section" {
    const content =
        \\# META
        \\some meta content
        \\# EXPECTED
        \\NIL
        \\# PROBLEMS
        \\**UNDEFINED VARIABLE**
        \\Nothing is named `x` in this scope.
        \\
        \\**test.md:1:1:1:2:**
        \\```roc
        \\x
        \\```
    ;

    const problems = extractSection(content, "PROBLEMS");
    try testing.expect(problems != null);
    try testing.expect(std.mem.indexOf(u8, problems.?, "UNDEFINED VARIABLE") != null);
}
