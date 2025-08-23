const std = @import("std");
const testing = std.testing;
const base = @import("base");
const collections = @import("collections");
const AST2 = @import("../AST2.zig");
const Parser2 = @import("../Parser2.zig");
const tokenize_iter = @import("../tokenize_iter.zig");

/// Metadata from snapshot files
const SnapshotMeta = struct {
    description: []const u8,
    type: []const u8, // "file" or "expr"
};

/// Result of parsing a snapshot
const SnapshotParseResult = struct {
    filename: []const u8,
    meta: SnapshotMeta,
    source: []const u8,
    existing_problems: []const u8,
    new_problems: []const u8,
    parse_succeeded: bool,
};

/// Parse metadata section from snapshot
fn parseMetadata(allocator: std.mem.Allocator, meta_content: []const u8) !SnapshotMeta {
    var description: []const u8 = try allocator.dupe(u8, "");
    var snapshot_type: []const u8 = try allocator.dupe(u8, "file");

    var lines = std.mem.splitScalar(u8, meta_content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (std.mem.startsWith(u8, trimmed, "description=")) {
            allocator.free(description);
            description = try allocator.dupe(u8, trimmed[12..]);
        } else if (std.mem.startsWith(u8, trimmed, "type=")) {
            allocator.free(snapshot_type);
            snapshot_type = try allocator.dupe(u8, trimmed[5..]);
        }
    }

    return SnapshotMeta{
        .description = description,
        .type = snapshot_type,
    };
}

/// Extract a section from snapshot content
fn extractSection(allocator: std.mem.Allocator, content: []const u8, section_name: []const u8) ![]const u8 {
    const section_header = try std.fmt.allocPrint(allocator, "# {s}", .{section_name});
    defer allocator.free(section_header);

    const start_idx = std.mem.indexOf(u8, content, section_header) orelse return try allocator.dupe(u8, "");
    const content_after_header = content[start_idx + section_header.len ..];

    // Find the next section or end of file
    // Sections start with "# " followed by an uppercase letter (not comments which start with "#")
    var end_idx = content_after_header.len;
    var search_pos: usize = 0;
    while (std.mem.indexOfPos(u8, content_after_header, search_pos, "\n# ")) |potential_section| {
        // Check if this is an actual section header (next char should be uppercase)
        if (potential_section + 3 < content_after_header.len) {
            const next_char = content_after_header[potential_section + 3];
            if (next_char >= 'A' and next_char <= 'Z') {
                end_idx = potential_section;
                break;
            }
        }
        search_pos = potential_section + 1;
    }

    const section_content = std.mem.trim(u8, content_after_header[0..end_idx], " \t\r\n");

    // Remove code fences if present
    if (std.mem.startsWith(u8, section_content, "~~~")) {
        var lines = std.mem.splitScalar(u8, section_content, '\n');
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        var inside_fence = false;
        while (lines.next()) |line| {
            if (std.mem.startsWith(u8, line, "~~~")) {
                inside_fence = !inside_fence;
                continue;
            }
            if (inside_fence) {
                try result.appendSlice(line);
                try result.append('\n');
            }
        }

        const final_content = try result.toOwnedSlice();
        const trimmed = std.mem.trim(u8, final_content, "\n");
        const result_copy = try allocator.dupe(u8, trimmed);
        allocator.free(final_content);
        return result_copy;
    }

    return try allocator.dupe(u8, section_content);
}

/// Parse source using our new Parser2/TokenIterator pipeline
fn parseWithNewPipeline(allocator: std.mem.Allocator, source: []const u8, snapshot_type: []const u8) ![]const u8 {
    var ast = AST2.initCapacity(allocator, 100) catch return try allocator.dupe(u8, "INIT_ERROR");
    defer ast.deinit(allocator);

    // Create a CommonEnv for tokenization
    var env = base.CommonEnv.init(allocator, source) catch return try allocator.dupe(u8, "ENV_ERROR");
    defer env.deinit(allocator);

    // Create diagnostics buffer
    var messages: [128]tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Parse using new Parser2 with TokenIterator
    var parser = Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices) catch return try allocator.dupe(u8, "PARSER_INIT_ERROR");
    defer parser.deinit();

    if (std.mem.eql(u8, snapshot_type, "file")) {
        _ = parser.parseFile() catch null;
    } else {
        _ = parser.parseExpr() catch {};
    }

    // Collect parsing errors from diagnostics
    var problems = std.ArrayList(u8).init(allocator);
    defer problems.deinit();

    for (parser.diagnostics.items) |diagnostic| {
        // Get the actual token at the error position
        const start_offset = diagnostic.region.start.offset;
        const end_offset = diagnostic.region.end.offset;

        // Find the line and column
        var line: usize = 1;
        var col: usize = 1;
        for (source[0..start_offset]) |ch| {
            if (ch == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        // Extract the problem token/text
        const problem_len = if (end_offset > start_offset)
            @min(20, end_offset - start_offset)
        else
            0;
        const problem_text_slice = if (start_offset < source.len and problem_len > 0)
            source[start_offset..@min(source.len, start_offset + problem_len)]
        else
            "<EOF>";

        // Format the diagnostic similar to existing format but with details
        const problem_text = switch (diagnostic.tag) {
            .expr_unexpected_token => "Unexpected token in expression",
            .pattern_unexpected_token => "Unexpected token in pattern",
            .header_expected_open_square => "Expected '[' in header",
            .header_expected_close_square => "Expected ']' in header",
            .expected_expr_close_curly => "Expected '}' to close expression",
            .expected_colon_after_pat_field_name => "Expected ':' after pattern field name",
            .exposed_item_unexpected_token => "Unexpected token in exposed item",
            .expected_exposes => "Expected 'exposes'",
            else => "Unknown parsing error",
        };

        try problems.writer().print("**PARSE ERROR** at line {d}, column {d}\n", .{ line, col });
        try problems.writer().print("{s}\n", .{problem_text});
        try problems.writer().print("Found: '{s}'\n\n", .{problem_text_slice});
    }

    if (problems.items.len == 0) {
        return try allocator.dupe(u8, "NIL");
    }

    return try problems.toOwnedSlice();
}

/// Process a single snapshot file
fn processSnapshot(allocator: std.mem.Allocator, filepath: []const u8) !SnapshotParseResult {
    const filename = std.fs.path.basename(filepath);

    // Read the snapshot file
    const file_content = std.fs.cwd().readFileAlloc(allocator, filepath, 1024 * 1024) catch |err| {
        std.debug.print("Failed to read {s}: {}\n", .{ filepath, err });
        return SnapshotParseResult{
            .filename = try allocator.dupe(u8, filename),
            .meta = SnapshotMeta{ .description = try allocator.dupe(u8, "ERROR"), .type = try allocator.dupe(u8, "file") },
            .source = try allocator.dupe(u8, ""),
            .existing_problems = try allocator.dupe(u8, ""),
            .new_problems = try allocator.dupe(u8, "FILE_READ_ERROR"),
            .parse_succeeded = false,
        };
    };
    defer allocator.free(file_content);

    // Extract sections
    const meta_content = extractSection(allocator, file_content, "META") catch try allocator.dupe(u8, "");
    defer allocator.free(meta_content);
    const meta = parseMetadata(allocator, meta_content) catch SnapshotMeta{ .description = try allocator.dupe(u8, "PARSE_ERROR"), .type = try allocator.dupe(u8, "file") };

    const source = extractSection(allocator, file_content, "SOURCE") catch try allocator.dupe(u8, "");
    const existing_problems = extractSection(allocator, file_content, "PROBLEMS") catch try allocator.dupe(u8, "");

    // Parse with new pipeline
    const new_problems = parseWithNewPipeline(allocator, source, meta.type) catch try allocator.dupe(u8, "PARSE_PIPELINE_ERROR");

    return SnapshotParseResult{
        .filename = try allocator.dupe(u8, filename),
        .meta = meta,
        .source = source,
        .existing_problems = existing_problems,
        .new_problems = new_problems,
        .parse_succeeded = !std.mem.containsAtLeast(u8, new_problems, 1, "ERROR"),
    };
}

/// Check if a problem text is a parse error
fn isParseError(problem_text: []const u8) bool {
    // Parse errors contain "PARSE ERROR" or parsing-related keywords
    return std.mem.containsAtLeast(u8, problem_text, 1, "PARSE ERROR") or
        std.mem.containsAtLeast(u8, problem_text, 1, "parsing error") or
        std.mem.containsAtLeast(u8, problem_text, 1, "syntax error");
}

/// Filter problems to only include parse errors
fn filterParseErrors(allocator: std.mem.Allocator, problems: []const u8) ![]const u8 {
    if (problems.len == 0) return try allocator.dupe(u8, "");

    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    // Split by double newlines to get individual problems
    var problem_iter = std.mem.splitSequence(u8, problems, "\n\n");
    var found_parse_errors = false;

    while (problem_iter.next()) |problem| {
        const trimmed = std.mem.trim(u8, problem, " \t\r\n");
        if (trimmed.len > 0 and isParseError(trimmed)) {
            if (found_parse_errors) {
                try result.appendSlice("\n\n");
            }
            try result.appendSlice(trimmed);
            found_parse_errors = true;
        }
    }

    return try result.toOwnedSlice();
}

/// Compare PROBLEMS sections and categorize differences
fn compareProblemsSections(allocator: std.mem.Allocator, existing: []const u8, new: []const u8) !struct {
    missing_in_new: []const u8, // Problems in existing but not in new
    new_problems: []const u8, // Problems in new but not in existing
    same_problems: bool, // True if problems are the same
} {
    // Filter to only parse errors
    const existing_filtered = try filterParseErrors(allocator, existing);
    defer allocator.free(existing_filtered);
    const new_filtered = try filterParseErrors(allocator, new);
    defer allocator.free(new_filtered);

    // Normalize the problems (remove whitespace differences, NIL handling)
    const existing_normalized = if (std.mem.eql(u8, std.mem.trim(u8, existing_filtered, " \t\r\n"), "NIL") or existing_filtered.len == 0)
        ""
    else
        std.mem.trim(u8, existing_filtered, " \t\r\n");

    const new_normalized = if (std.mem.eql(u8, std.mem.trim(u8, new_filtered, " \t\r\n"), "NIL") or new_filtered.len == 0)
        ""
    else
        std.mem.trim(u8, new_filtered, " \t\r\n");

    const same = std.mem.eql(u8, existing_normalized, new_normalized);

    var missing_in_new = std.ArrayList(u8).init(allocator);
    var new_problems = std.ArrayList(u8).init(allocator);

    if (!same) {
        if (existing_normalized.len > 0 and new_normalized.len == 0) {
            try missing_in_new.appendSlice(existing_normalized);
        } else if (existing_normalized.len == 0 and new_normalized.len > 0) {
            try new_problems.appendSlice(new_normalized);
        } else if (existing_normalized.len > 0 and new_normalized.len > 0) {
            // Both have problems but they're different
            try missing_in_new.appendSlice("OLD: ");
            try missing_in_new.appendSlice(existing_normalized);
            try new_problems.appendSlice("NEW: ");
            try new_problems.appendSlice(new_normalized);
        }
    }

    return .{
        .missing_in_new = try missing_in_new.toOwnedSlice(),
        .new_problems = try new_problems.toOwnedSlice(),
        .same_problems = same,
    };
}

test "snapshot comparison - new Parser2 vs existing snapshots" {
    const allocator = testing.allocator;

    // Find all snapshot files
    const snapshot_dir = "/Users/rtfeldman/code/roc3/test/snapshots";
    var dir = std.fs.cwd().openDir(snapshot_dir, .{ .iterate = true }) catch {
        std.debug.print("Could not open snapshots directory: {s}\n", .{snapshot_dir});
        return;
    };
    defer dir.close();

    var walker = dir.walk(allocator) catch {
        std.debug.print("Could not walk snapshots directory\n", .{});
        return;
    };
    defer walker.deinit();

    var results = std.ArrayList(SnapshotParseResult).init(allocator);
    defer {
        for (results.items) |result| {
            allocator.free(result.filename);
            allocator.free(result.meta.description);
            allocator.free(result.meta.type);
            allocator.free(result.source);
            allocator.free(result.existing_problems);
            allocator.free(result.new_problems);
        }
        results.deinit();
    }

    // Process all .md files
    var file_count: u32 = 0;
    while (walker.next() catch null) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".md")) {
            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ snapshot_dir, entry.path });
            defer allocator.free(full_path);

            // Only print progress every 50 files to reduce output
            // if ((file_count + 1) % 50 == 0 or file_count == 0) {
            //     std.debug.print("Processing file {}: {s}\n", .{ file_count + 1, entry.path });
            // }
            const result = processSnapshot(allocator, full_path) catch {
                // std.debug.print("  Error processing file\n", .{});
                continue;
            };
            try results.append(result);
            file_count += 1;
        }
    }

    // std.debug.print("\n=== SNAPSHOT COMPARISON RESULTS ===\n", .{});
    // std.debug.print("Processed {d} snapshot files\n\n", .{file_count});

    var same_count: u32 = 0;
    var different_count: u32 = 0;
    var parse_failures: u32 = 0;

    for (results.items) |result| {
        const comparison = compareProblemsSections(allocator, result.existing_problems, result.new_problems) catch continue;
        defer allocator.free(comparison.missing_in_new);
        defer allocator.free(comparison.new_problems);

        if (!result.parse_succeeded) {
            parse_failures += 1;
            if (parse_failures <= 5) {
                // std.debug.print("PARSE FAILURE #{} in {s}:\n", .{ parse_failures, result.filename });
                // std.debug.print("  New problems: {s}\n", .{result.new_problems});
            }
        }

        if (comparison.same_problems) {
            same_count += 1;
        } else {
            different_count += 1;
            // std.debug.print("DIFF in {s}:\n", .{result.filename});
            // std.debug.print("  Type: {s}\n", .{result.meta.type});
            // std.debug.print("  Description: {s}\n", .{result.meta.description});

            if (comparison.missing_in_new.len > 0) {
                // std.debug.print("  MISSING IN NEW:\n    {s}\n", .{comparison.missing_in_new});
            }
            if (comparison.new_problems.len > 0) {
                // std.debug.print("  NEW PROBLEMS:\n    {s}\n", .{comparison.new_problems});
            }
            // std.debug.print("\n", .{});
        }
    }

    // std.debug.print("=== SUMMARY ===\n", .{});
    // std.debug.print("Same problems: {d}\n", .{same_count});
    // std.debug.print("Different problems: {d}\n", .{different_count});
    // std.debug.print("Parse failures: {d}\n", .{parse_failures});
    // std.debug.print("Total processed: {d}\n", .{file_count});
}
