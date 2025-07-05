const std = @import("std");
const base = @import("base.zig");

const verbose_log = false;

fn log(comptime fmt: []const u8, args: anytype) void {
    if (verbose_log) {
        std.debug.print("[update_expected] " ++ fmt ++ "\n", args);
    }
}

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

/// Extract section content between headers
fn extractSection(content: []const u8, section_name: []const u8) ?struct { content: []const u8, start: usize, end: usize } {
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

    return .{
        .content = std.mem.trim(u8, content[content_start..next_section_idx], " \t\r\n"),
        .start = start_idx,
        .end = next_section_idx,
    };
}

/// Parse a PROBLEMS entry to extract problem type and location
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

/// Update a single snapshot file
fn updateSnapshotFile(allocator: std.mem.Allocator, path: []const u8) !bool {
    const content = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(content);

    // Extract PROBLEMS section
    const problems_info = extractSection(content, "PROBLEMS");
    if (problems_info == null) {
        log("No PROBLEMS section found in {s}", .{path});
        return false;
    }

    // Parse problems
    var problems = try parseProblemsSection(allocator, problems_info.?.content);
    defer {
        for (problems.items) |p| {
            allocator.free(p.problem_type);
            allocator.free(p.file);
        }
        problems.deinit();
    }

    // Generate new EXPECTED content
    const expected_content = try generateExpectedContent(allocator, problems.items);
    defer allocator.free(expected_content);

    // Find or create EXPECTED section
    const expected_info = extractSection(content, "EXPECTED");

    var new_content = std.ArrayList(u8).init(allocator);
    defer new_content.deinit();

    if (expected_info) |info| {
        // Replace existing EXPECTED section
        try new_content.appendSlice(content[0..info.start]);
        try new_content.appendSlice("# EXPECTED\n");
        try new_content.appendSlice(expected_content);
        try new_content.append('\n');
        try new_content.appendSlice(content[info.end..]);
    } else {
        // Insert EXPECTED section after SOURCE section
        const source_info = extractSection(content, "SOURCE");
        if (source_info == null) {
            log("No SOURCE section found in {s}", .{path});
            return false;
        }

        // Find the end of the SOURCE section (including closing ~~~)
        var source_end = source_info.?.end;
        // Back up to find the ~~~ line
        var idx = source_info.?.start + "# SOURCE\n".len;
        while (idx < content.len) {
            if (idx + 3 < content.len and std.mem.eql(u8, content[idx .. idx + 3], "~~~")) {
                // Find the end of this line
                var line_end = idx + 3;
                while (line_end < content.len and content[line_end] != '\n') {
                    line_end += 1;
                }
                if (line_end < content.len) {
                    line_end += 1; // Include the newline
                }
                source_end = line_end;
                break;
            }
            idx += 1;
        }

        try new_content.appendSlice(content[0..source_end]);
        try new_content.appendSlice("# EXPECTED\n");
        try new_content.appendSlice(expected_content);
        try new_content.append('\n');
        try new_content.appendSlice(content[source_end..]);
    }

    // Write the updated content back
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(new_content.items);

    return true;
}

fn processPath(allocator: std.mem.Allocator, path: []const u8) !u32 {
    var count: u32 = 0;

    var dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.path, ".md")) {
            const full_path = try std.fs.path.join(allocator, &.{ path, entry.path });
            defer allocator.free(full_path);

            if (try updateSnapshotFile(allocator, full_path)) {
                count += 1;
                std.debug.print("Updated: {s}\n", .{full_path});
            }
        }
    }

    return count;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const path = if (args.len > 1) args[1] else "src/snapshots";

    std.debug.print("Updating EXPECTED sections based on PROBLEMS in: {s}\n", .{path});

    const count = try processPath(allocator, path);

    std.debug.print("\nUpdated {} snapshot files\n", .{count});
}
