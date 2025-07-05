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
    if (trimmed.len == 0) {
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

    // Skip whitespace and empty lines
    while (idx < content.len and (content[idx] == ' ' or content[idx] == '\t' or content[idx] == '\n' or content[idx] == '\r')) {
        idx += 1;
    }

    // Check if we're at the start of a problem header
    if (idx + 2 > content.len or !std.mem.eql(u8, content[idx .. idx + 2], "**")) {
        // Not a problem header, skip to next one
        while (idx < content.len) {
            if (idx + 2 <= content.len and std.mem.eql(u8, content[idx .. idx + 2], "**") and
                (idx == 0 or content[idx - 1] == '\n'))
            {
                break;
            }
            idx += 1;
        }
    }

    if (idx >= content.len) return null;

    // Find the end of the problem type
    const type_start = idx + 2;
    const type_end_search = std.mem.indexOfPos(u8, content, type_start, "**");
    if (type_end_search == null) return null;
    const type_end = type_end_search.?;
    var problem_type = std.mem.trim(u8, content[type_start..type_end], " \t\r\n");

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

    // Now look for a location pattern on its own line
    // Pattern: **file:line:col:line:col:** or **file:line:col:**
    while (current_idx < content.len) {
        // Skip whitespace at start of line
        while (current_idx < content.len and (content[current_idx] == ' ' or content[current_idx] == '\t')) {
            current_idx += 1;
        }

        // Check if this line starts with **
        if (current_idx + 2 <= content.len and std.mem.eql(u8, content[current_idx .. current_idx + 2], "**")) {
            const loc_start = current_idx + 2;

            // Find the ending **
            const loc_end_search = std.mem.indexOfPos(u8, content, loc_start, "**");
            if (loc_end_search == null) {
                // No closing **, skip this line
                while (current_idx < content.len and content[current_idx] != '\n') {
                    current_idx += 1;
                }
                if (current_idx < content.len) current_idx += 1;
                continue;
            }

            const loc_end = loc_end_search.?;
            var location = content[loc_start..loc_end];

            // Strip trailing colon and whitespace
            location = std.mem.trimRight(u8, location, ": \t");

            // Check if this looks like a file location
            if (std.mem.indexOf(u8, location, ".md:")) |_| {
                // Count colons to determine format
                var colon_count: usize = 0;
                for (location) |c| {
                    if (c == ':') colon_count += 1;
                }

                // Handle formats with extra text after location (e.g., "file:1:2:3:4: - SOME TEXT")
                if (std.mem.indexOf(u8, location, ": - ")) |extra_idx| {
                    location = location[0..extra_idx];
                    // Recount colons
                    colon_count = 0;
                    for (location) |c| {
                        if (c == ':') colon_count += 1;
                    }
                }

                if (colon_count == 2) {
                    // Format: file:line:col
                    var parts = std.mem.tokenizeScalar(u8, location, ':');

                    const file = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const line_str = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const col_str = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };

                    // Try to parse numbers
                    const line = std.fmt.parseInt(u32, line_str, 10) catch {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const col = std.fmt.parseInt(u32, col_str, 10) catch {
                        current_idx = loc_end + 2;
                        continue;
                    };

                    const entry = ProblemEntry{
                        .problem_type = try allocator.dupe(u8, problem_type),
                        .file = try allocator.dupe(u8, file),
                        .start_line = line,
                        .start_col = col,
                        .end_line = line,
                        .end_col = col,
                    };

                    // Find the end of this problem entry (next problem or end of content)
                    var next_idx = loc_end + 2;
                    while (next_idx < content.len) {
                        if (next_idx + 2 <= content.len and
                            std.mem.eql(u8, content[next_idx .. next_idx + 2], "**") and
                            (next_idx == 0 or content[next_idx - 1] == '\n'))
                        {
                            // Check if this is the start of a new problem (not part of message)
                            const peek_end = std.mem.indexOfPos(u8, content, next_idx + 2, "**");
                            if (peek_end) |pe| {
                                const peek_content = content[next_idx + 2 .. pe];
                                // If it contains "VARIABLE" or "ERROR" or other problem keywords, it's likely a new problem
                                if (std.mem.indexOf(u8, peek_content, "VARIABLE") != null or
                                    std.mem.indexOf(u8, peek_content, "ERROR") != null or
                                    std.mem.indexOf(u8, peek_content, "INVALID") != null or
                                    std.mem.indexOf(u8, peek_content, "UNEXPECTED") != null or
                                    std.mem.indexOf(u8, peek_content, "IMPLEMENTED") != null or
                                    std.mem.indexOf(u8, peek_content, "TYPE") != null or
                                    std.mem.indexOf(u8, peek_content, "PATTERN") != null or
                                    std.mem.indexOf(u8, peek_content, "STATEMENT") != null)
                                {
                                    break;
                                }
                            }
                        }
                        next_idx += 1;
                    }

                    return .{ .entry = entry, .next_idx = next_idx };
                } else if (colon_count == 4) {
                    // Format: file:start_line:start_col:end_line:end_col
                    var parts = std.mem.tokenizeScalar(u8, location, ':');

                    const file = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const start_line_str = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const start_col_str = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const end_line_str = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const end_col_str = parts.next() orelse {
                        current_idx = loc_end + 2;
                        continue;
                    };

                    // Try to parse numbers
                    const start_line = std.fmt.parseInt(u32, start_line_str, 10) catch {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const start_col = std.fmt.parseInt(u32, start_col_str, 10) catch {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const end_line = std.fmt.parseInt(u32, end_line_str, 10) catch {
                        current_idx = loc_end + 2;
                        continue;
                    };
                    const end_col = std.fmt.parseInt(u32, end_col_str, 10) catch {
                        current_idx = loc_end + 2;
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

                    // Find the end of this problem entry
                    var next_idx = loc_end + 2;
                    while (next_idx < content.len) {
                        if (next_idx + 2 <= content.len and
                            std.mem.eql(u8, content[next_idx .. next_idx + 2], "**") and
                            (next_idx == 0 or content[next_idx - 1] == '\n'))
                        {
                            // Check if this is the start of a new problem
                            const peek_end = std.mem.indexOfPos(u8, content, next_idx + 2, "**");
                            if (peek_end) |pe| {
                                const peek_content = content[next_idx + 2 .. pe];
                                if (std.mem.indexOf(u8, peek_content, "VARIABLE") != null or
                                    std.mem.indexOf(u8, peek_content, "ERROR") != null or
                                    std.mem.indexOf(u8, peek_content, "INVALID") != null or
                                    std.mem.indexOf(u8, peek_content, "UNEXPECTED") != null or
                                    std.mem.indexOf(u8, peek_content, "IMPLEMENTED") != null or
                                    std.mem.indexOf(u8, peek_content, "TYPE") != null or
                                    std.mem.indexOf(u8, peek_content, "PATTERN") != null or
                                    std.mem.indexOf(u8, peek_content, "STATEMENT") != null)
                                {
                                    break;
                                }
                            }
                        }
                        next_idx += 1;
                    }

                    return .{ .entry = entry, .next_idx = next_idx };
                }
            }
        }

        // Skip to next line
        while (current_idx < content.len and content[current_idx] != '\n') {
            current_idx += 1;
        }
        if (current_idx < content.len) current_idx += 1;

        // Check if we've hit another problem header or gone too far
        if (current_idx < content.len - 2) {
            // Peek ahead to see if we're at a new problem
            var peek_idx = current_idx;
            while (peek_idx < content.len and (content[peek_idx] == ' ' or content[peek_idx] == '\t')) {
                peek_idx += 1;
            }
            if (peek_idx + 2 <= content.len and std.mem.eql(u8, content[peek_idx .. peek_idx + 2], "**")) {
                const peek_end = std.mem.indexOfPos(u8, content, peek_idx + 2, "**");
                if (peek_end) |pe| {
                    const peek_content = content[peek_idx + 2 .. pe];
                    if (std.mem.indexOf(u8, peek_content, "VARIABLE") != null or
                        std.mem.indexOf(u8, peek_content, "ERROR") != null or
                        std.mem.indexOf(u8, peek_content, "INVALID") != null or
                        std.mem.indexOf(u8, peek_content, "UNEXPECTED") != null or
                        std.mem.indexOf(u8, peek_content, "IMPLEMENTED") != null or
                        std.mem.indexOf(u8, peek_content, "TYPE") != null or
                        std.mem.indexOf(u8, peek_content, "PATTERN") != null or
                        std.mem.indexOf(u8, peek_content, "STATEMENT") != null)
                    {
                        // This is a new problem, stop looking for location
                        return .{ .entry = null, .next_idx = current_idx };
                    }
                }
            }
        }
    }

    // No location found, return null entry but advance past this problem
    return .{ .entry = null, .next_idx = current_idx };
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
