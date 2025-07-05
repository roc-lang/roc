//! Tests for validating that EXPECTED sections in snapshots match their PROBLEMS sections.
//! This ensures that the expected problems documented in each snapshot file accurately
//! reflect the actual problems reported by the compiler.

const std = @import("std");
const testing = std.testing;
const snapshot_mod = @import("snapshot.zig");

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
    if (std.mem.eql(u8, trimmed, "NIL") or trimmed.len == 0) {
        return null;
    }

    // Find the separator " - "
    const separator = " - ";
    const sep_index = std.mem.indexOf(u8, line, separator) orelse return null;

    const problem_type = std.mem.trim(u8, line[0..sep_index], " \t");
    const location = std.mem.trim(u8, line[sep_index + separator.len ..], " \t\r\n");

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

    // Skip to next problem header
    while (idx < content.len) {
        if (idx + 2 <= content.len and std.mem.eql(u8, content[idx .. idx + 2], "**")) {
            break;
        }
        idx += 1;
    }

    if (idx >= content.len) return null;

    // Find the end of the problem type
    const type_start = idx + 2;
    const type_end_search = std.mem.indexOfPos(u8, content, type_start, "**");
    if (type_end_search == null) return null;
    const type_end = type_end_search.?;
    const problem_type = std.mem.trim(u8, content[type_start..type_end], " \t\r\n");

    // Look for the location pattern **file:line:col:line:col:**
    var search_idx = type_end + 2;
    while (search_idx < content.len) {
        if (search_idx + 2 <= content.len and std.mem.eql(u8, content[search_idx .. search_idx + 2], "**")) {
            const loc_start = search_idx + 2;
            const loc_end_search = std.mem.indexOfPos(u8, content, loc_start, ":**");
            if (loc_end_search) |loc_end| {
                const location = content[loc_start..loc_end];

                // Check if this looks like a location (has 4 colons)
                var colon_count: usize = 0;
                for (location) |c| {
                    if (c == ':') colon_count += 1;
                }

                if (colon_count == 4) {
                    // This is a location, parse it
                    var parts = std.mem.tokenizeScalar(u8, location, ':');

                    const file = parts.next() orelse {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const start_line_str = parts.next() orelse {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const start_col_str = parts.next() orelse {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const end_line_str = parts.next() orelse {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const end_col_str = parts.next() orelse {
                        search_idx = loc_end + 3;
                        continue;
                    };

                    // Try to parse numbers
                    const start_line = std.fmt.parseInt(u32, start_line_str, 10) catch {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const start_col = std.fmt.parseInt(u32, start_col_str, 10) catch {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const end_line = std.fmt.parseInt(u32, end_line_str, 10) catch {
                        search_idx = loc_end + 3;
                        continue;
                    };
                    const end_col = std.fmt.parseInt(u32, end_col_str, 10) catch {
                        search_idx = loc_end + 3;
                        continue;
                    };

                    const entry = ProblemEntry{
                        .problem_type = try allocator.dupe(u8, problem_type),
                        .file = try allocator.dupe(u8, file),
                        .start_line = start_line,
                        .start_col = start_col,
                        .end_line = end_line,
                        .end_col = end_col,
                    };

                    return .{ .entry = entry, .next_idx = loc_end + 3 };
                }
            }
        }
        search_idx += 1;
    }

    // No location found for this problem type, move to end of content
    return .{ .entry = null, .next_idx = content.len };
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

    if (std.mem.eql(u8, std.mem.trim(u8, content, " \t\r\n"), "NIL")) {
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

    const expected_section = extractSection(content, "EXPECTED") orelse {
        // EXPECTED section is required for all snapshots
        std.debug.print("\n❌ {s}:\n", .{std.fs.path.basename(path)});
        std.debug.print("  Missing EXPECTED section\n", .{});
        return error.MissingExpectedSection;
    };

    const problems_section = extractSection(content, "PROBLEMS") orelse {
        // No PROBLEMS section is also OK
        return;
    };

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
        return error.SnapshotValidationFailed;
    }
}

test "snapshot validation" {
    const allocator = testing.allocator;

    // Collect all snapshot files
    var snapshot_files = std.ArrayList([]const u8).init(allocator);
    defer {
        for (snapshot_files.items) |f| allocator.free(f);
        snapshot_files.deinit();
    }

    var snapshots_dir = try std.fs.cwd().openDir("src/snapshots", .{ .iterate = true });
    defer snapshots_dir.close();

    var walker = try snapshots_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.path, ".md")) {
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

test "parseExpectedLine - NIL" {
    const line = "NIL";
    const result = try parseExpectedLine(testing.allocator, line);
    try testing.expect(result == null);
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
