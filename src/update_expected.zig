//! Tool for updating EXPECTED sections in snapshot files based on their PROBLEMS sections.
//! This ensures that snapshot test expectations stay in sync with actual compiler output.

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

/// Main entry point for the update-expected tool
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
