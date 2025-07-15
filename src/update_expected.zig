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

    // Check if path is a file or directory
    const stat = std.fs.cwd().statFile(path) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("Path not found: {s}\n", .{path});
            return 0;
        },
        else => return err,
    };

    if (stat.kind == .file) {
        // Process single file
        if (std.mem.endsWith(u8, path, ".md")) {
            if (try updateSnapshotFile(allocator, path)) {
                count += 1;
                std.debug.print("Updated: {s}\n", .{path});
            }
        }
    } else if (stat.kind == .directory) {
        // Process directory
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
