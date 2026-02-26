const std = @import("std");

const Allocator = std.mem.Allocator;
const PathList = std.ArrayList([]u8);

const max_file_bytes: usize = 16 * 1024 * 1024;

const TermColor = struct {
    pub const red = "\x1b[0;31m";
    pub const green = "\x1b[0;32m";
    pub const reset = "\x1b[0m";
};

pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_state = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_state.interface;

    var found_errors = false;

    // Lint 1: Check for separator comments (// ====, // ----, // ────, etc.)
    try stdout.print("Checking for separator comments...\n", .{});

    {
        var zig_files = PathList{};
        defer freePathList(&zig_files, gpa);

        // Scan src/, build.zig, and test/ (not ci/ since zig_lints.zig mentions the pattern)
        try walkTree(gpa, "src", &zig_files);
        try walkTree(gpa, "test", &zig_files);

        // Add build.zig directly
        try zig_files.append(gpa, try gpa.dupe(u8, "build.zig"));

        for (zig_files.items) |file_path| {
            const errors = try checkSeparatorComments(gpa, file_path);
            defer gpa.free(errors);

            if (errors.len > 0) {
                try stdout.print("{s}", .{errors});
                found_errors = true;
            }
        }

        if (found_errors) {
            try stdout.print("\n", .{});
            try stdout.print("Horizontal line separator comments are not allowed (e.g. // ====, // ----, // ────).\n", .{});
            try stdout.print("Do not attempt to work around this by using different characters. Just don't put horizontal lines in the code.\n", .{});
            try stdout.print("\n", .{});
            try stdout.flush();
            std.process.exit(1);
        }
    }

    // Lint 2: Forbid catch-all arms in enum/tagged-union switches
    try stdout.print("Checking for forbidden catch-all switch arms ('else =>' and '_ =>')...\n", .{});

    {
        var src_zig_files = PathList{};
        defer freePathList(&src_zig_files, gpa);

        try walkTree(gpa, "src", &src_zig_files);

        for (src_zig_files.items) |file_path| {
            // Skip vendored files (adapted from upstream projects).
            if (std.mem.endsWith(u8, file_path, "backend/llvm/Builder.zig")) continue;

            const errors = try checkCatchAllSwitchArms(gpa, file_path);
            defer gpa.free(errors);

            if (errors.len > 0) {
                try stdout.print("{s}", .{errors});
                found_errors = true;
            }
        }

        if (found_errors) {
            try stdout.print("\n", .{});
            try stdout.print("Catch-all switch arms ('else =>' and '_ =>') are banned for enum/tagged-union switches in src/.\n", .{});
            try stdout.print("The only acceptable fix is to explicitly enumerate every enum/tagged-union variant in the switch.\n", .{});
            try stdout.print("Do not use catch-all arms to bypass exhaustiveness.\n", .{});
            try stdout.print("\n", .{});
            try stdout.flush();
            std.process.exit(1);
        }
    }

    // Lint 3: Check for pub declarations without doc comments
    try stdout.print("Checking for pub declarations without doc comments...\n", .{});

    var zig_files = PathList{};
    defer freePathList(&zig_files, gpa);

    try walkTree(gpa, "src", &zig_files);

    for (zig_files.items) |file_path| {
        const errors = try checkPubDocComments(gpa, file_path);
        defer gpa.free(errors);

        if (errors.len > 0) {
            try stdout.print("{s}", .{errors});
            found_errors = true;
        }
    }

    if (found_errors) {
        try stdout.print("\n", .{});
        try stdout.print("Please add doc comments to the spots listed above, they make the code easier to understand for everyone.\n", .{});
        try stdout.print("\n", .{});
        try stdout.flush();
        std.process.exit(1);
    }

    // Lint 4: Check for top level comments in new Zig files
    try stdout.print("Checking for top level comments in new Zig files...\n", .{});

    var new_zig_files = try getNewZigFiles(gpa);
    defer {
        for (new_zig_files.items) |path| {
            gpa.free(path);
        }
        new_zig_files.deinit(gpa);
    }

    if (new_zig_files.items.len == 0) {
        try stdout.print("{s}[OK]{s} All lints passed!\n", .{ TermColor.green, TermColor.reset });
        try stdout.flush();
        return;
    }

    var failed_files = PathList{};
    defer freePathList(&failed_files, gpa);

    for (new_zig_files.items) |file_path| {
        if (!try fileHasTopLevelComment(gpa, file_path)) {
            try stdout.print("Error: {s} is missing top level comment (//!)\n", .{file_path});
            try failed_files.append(gpa, try gpa.dupe(u8, file_path));
        }
    }

    if (failed_files.items.len > 0) {
        try stdout.print("\n", .{});
        try stdout.print("The following files are missing a top level comment:\n", .{});
        for (failed_files.items) |path| {
            try stdout.print("    {s}\n", .{path});
        }
        try stdout.print("\n", .{});
        try stdout.print("Add a //! comment that explains the purpose of the file BEFORE any other code.\n", .{});
        try stdout.flush();
        std.process.exit(1);
    }

    try stdout.print("{s}[OK]{s} All lints passed!\n", .{ TermColor.green, TermColor.reset });
    try stdout.flush();
}

fn walkTree(allocator: Allocator, dir_path: []const u8, zig_files: *PathList) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .sym_link) continue;

        const next_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });

        switch (entry.kind) {
            .directory => {
                // Skip .zig-cache directories
                if (std.mem.eql(u8, entry.name, ".zig-cache")) {
                    allocator.free(next_path);
                    continue;
                }
                defer allocator.free(next_path);
                try walkTree(allocator, next_path, zig_files);
            },
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".zig")) {
                    try zig_files.append(allocator, next_path);
                } else {
                    allocator.free(next_path);
                }
            },
            else => allocator.free(next_path),
        }
    }
}

fn checkSeparatorComments(allocator: Allocator, file_path: []const u8) ![]u8 {
    const source = readSourceFile(allocator, file_path) catch |err| switch (err) {
        // Skip files we can't read
        error.FileNotFound => return try allocator.dupe(u8, ""),
        else => return err,
    };
    defer allocator.free(source);

    var errors = std.ArrayList(u8){};
    errdefer errors.deinit(allocator);

    var line_num: usize = 1;
    var lines = std.mem.splitScalar(u8, source, '\n');

    while (lines.next()) |line| {
        defer line_num += 1;

        // Trim leading whitespace
        const trimmed = std.mem.trimLeft(u8, line, " \t");

        // Check if line starts with // and is a separator comment
        // Separator comments are lines like "// ====", "// ----", "// ────────"
        // We detect them by checking if, after the //, the line consists only of
        // whitespace, separator characters, and letters (for section titles)
        if (std.mem.startsWith(u8, trimmed, "//")) {
            const after_slashes = trimmed[2..];
            if (isSeparatorComment(after_slashes)) {
                try errors.writer(allocator).print("{s}:{d}: horizontal line separator comment not allowed\n", .{ file_path, line_num });
            }
        }
    }

    return errors.toOwnedSlice(allocator);
}

/// Checks if a line (after the //) is a horizontal line separator comment.
/// These are lines consisting of repeated separator characters (=, -, ─)
/// with optional whitespace and a section title between them.
/// Examples that should match:
///   " ===="
///   " ===== Section ====="
///   " ----"
///   " ---- Title ----"
///   " ──────────"
/// Examples that should NOT match:
///   " 2. Stdout contains "=====""
///   " This is a normal comment about ===="
///   " --something"
///   " ┌─────────────┐"  (box art with corners)
///   " └───────┬─────┘"  (box art with corners)
fn isSeparatorComment(after_slashes: []const u8) bool {
    // Trim whitespace
    const content = std.mem.trim(u8, after_slashes, " \t");
    if (content.len == 0) return false;

    // Check for box-drawing horizontal line (U+2500 "─", encoded as 0xE2 0x94 0x80 in UTF-8).
    // Only flag if the line has 4+ consecutive ─ but NO box-drawing corners/intersections
    // (┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼ │ etc.), which would indicate it's part of a diagram.
    if (std.mem.indexOf(u8, content, "\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80") != null) {
        if (!containsBoxDrawingCorner(content)) return true;
    }

    // For ASCII separators, must contain 4+ repeated chars and start with them
    const sep_char: u8 = if (std.mem.indexOf(u8, content, "====") != null)
        '='
    else if (std.mem.indexOf(u8, content, "----") != null)
        '-'
    else
        return false;

    // Must start with the separator character
    if (content[0] != sep_char) return false;

    // Find where the leading separator chars end
    var i: usize = 0;
    while (i < content.len and content[i] == sep_char) : (i += 1) {}

    // Everything after leading separators should be whitespace, letters, or trailing separators
    while (i < content.len) : (i += 1) {
        const c = content[i];
        if (c == sep_char or c == ' ' or c == '\t' or (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z')) {
            continue;
        }
        // Found a character that's not allowed in separator comments
        return false;
    }

    return true;
}

/// Returns true if the content contains any box-drawing corner or intersection
/// character (anything in the U+2500 block that isn't U+2500 "─" itself).
/// These characters (┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼ │ etc.) indicate the line is part
/// of a diagram, not a separator.
fn containsBoxDrawingCorner(content: []const u8) bool {
    var i: usize = 0;
    while (i + 2 < content.len) : (i += 1) {
        if (content[i] == 0xE2 and content[i + 1] == 0x94) {
            // U+2500 block: 0xE2 0x94 0x80-0xBF
            // U+2500 (─) is 0x80 — skip it, everything else is a corner/intersection
            if (content[i + 2] != 0x80) return true;
            i += 2; // skip the rest of this UTF-8 sequence
        } else if (content[i] == 0xE2 and content[i + 1] == 0x95) {
            // U+2540-U+257F block: 0xE2 0x95 0x80-0xBF (double-line box drawing)
            return true;
        }
    }
    return false;
}

fn checkCatchAllSwitchArms(allocator: Allocator, file_path: []const u8) ![]u8 {
    const source = readSourceFile(allocator, file_path) catch |err| switch (err) {
        error.FileNotFound => return try allocator.dupe(u8, ""),
        else => return err,
    };
    defer allocator.free(source);

    // Collect all lines so we can look backwards from each catch-all arm.
    var all_lines = std.ArrayList([]const u8){};
    defer all_lines.deinit(allocator);

    var splitter = std.mem.splitScalar(u8, source, '\n');
    while (splitter.next()) |line| {
        try all_lines.append(allocator, line);
    }

    var errors = std.ArrayList(u8){};
    errdefer errors.deinit(allocator);

    for (all_lines.items, 0..) |line, idx| {
        const trimmed = std.mem.trimLeft(u8, line, " \t");

        if (!isCatchAllSwitchArm(trimmed)) continue;

        // Allow suppressing this lint with an inline comment for non-exhaustive enums
        // where `_ =>` is genuinely required by Zig.
        if (std.mem.indexOf(u8, line, "// zig-lint-allow: catch-all") != null) continue;

        // Look at sibling arms above to determine if this is an enum switch.
        // If any sibling arm starts with '.' (an enum literal), flag it.
        if (isEnumSwitch(all_lines.items, idx)) {
            try errors.writer(allocator).print("{s}:{d}: catch-all switch arms ('else =>' and '_ =>') are not allowed; explicitly enumerate every enum/tagged-union variant\n", .{ file_path, idx + 1 });
        }
    }

    return errors.toOwnedSlice(allocator);
}

/// Scan backwards from a catch-all arm line to find sibling switch arms.
/// Returns true if any sibling arm starts with '.' (enum literal syntax),
/// indicating this is an enum/tagged-union switch that should be exhaustive.
fn isEnumSwitch(lines: []const []const u8, arm_idx: usize) bool {
    const arm_indent = indentLen(lines[arm_idx]);

    // Walk backwards looking for arms at the same indentation level.
    var i: usize = arm_idx;
    while (i > 0) {
        i -= 1;
        const prev = lines[i];
        const prev_trimmed = std.mem.trimLeft(u8, prev, " \t");

        // Skip blank lines and comment-only lines.
        if (prev_trimmed.len == 0 or std.mem.startsWith(u8, prev_trimmed, "//")) continue;

        const prev_indent = indentLen(prev);

        // If we've dedented past the switch arms, we've left the switch body.
        if (prev_indent < arm_indent) break;

        // Only look at lines at the same indent level (sibling arms).
        if (prev_indent != arm_indent) continue;

        // A sibling arm starting with '.' is an enum literal.
        if (prev_trimmed[0] == '.') return true;
    }

    return false;
}

fn isCatchAllSwitchArm(trimmed: []const u8) bool {
    if (startsWithArmPrefix(trimmed, "else")) return true;
    if (startsWithArmPrefix(trimmed, "_")) return true;
    return false;
}

fn startsWithArmPrefix(trimmed: []const u8, prefix: []const u8) bool {
    if (!std.mem.startsWith(u8, trimmed, prefix)) return false;

    var i = prefix.len;
    while (i < trimmed.len and (trimmed[i] == ' ' or trimmed[i] == '\t')) : (i += 1) {}

    return std.mem.startsWith(u8, trimmed[i..], "=>");
}

fn indentLen(line: []const u8) usize {
    var n: usize = 0;
    for (line) |c| {
        if (c == ' ' or c == '\t') {
            n += 1;
        } else break;
    }
    return n;
}

fn checkPubDocComments(allocator: Allocator, file_path: []const u8) ![]u8 {
    const source = readSourceFile(allocator, file_path) catch |err| switch (err) {
        // Skip files we can't read
        error.FileNotFound => return try allocator.dupe(u8, ""),
        else => return err,
    };
    defer allocator.free(source);

    var errors = std.ArrayList(u8){};
    errdefer errors.deinit(allocator);

    var line_num: usize = 1;
    var prev_line: []const u8 = "";
    var lines = std.mem.splitScalar(u8, source, '\n');

    while (lines.next()) |line| {
        defer {
            prev_line = line;
            line_num += 1;
        }

        // Check if line starts with "pub " (no leading whitespace - only top-level declarations)
        if (!std.mem.startsWith(u8, line, "pub ")) continue;

        // Check if previous line is a doc comment (allow indented doc comments)
        const prev_trimmed = std.mem.trimLeft(u8, prev_line, " \t");
        if (std.mem.startsWith(u8, prev_trimmed, "///")) continue;

        // Skip exceptions: init, deinit, @import, and pub const re-exports
        // Note: "pub.*fn init\(" in bash matches "init" anywhere in function name
        if (std.mem.indexOf(u8, line, "fn init") != null) continue;
        if (std.mem.indexOf(u8, line, "fn deinit") != null) continue;
        if (std.mem.indexOf(u8, line, "@import") != null) continue;

        // Check for pub const re-exports (e.g., "pub const Foo = bar.Baz;")
        if (isReExport(line)) continue;

        try errors.writer(allocator).print("{s}:{d}: pub declaration without doc comment `///`\n", .{ file_path, line_num });
    }

    return errors.toOwnedSlice(allocator);
}

fn isReExport(line: []const u8) bool {
    // Match pattern: pub const X = lowercase.something;
    // This detects re-exports like "pub const Foo = bar.Baz;"
    if (!std.mem.startsWith(u8, line, "pub const ")) return false;

    // Find the '=' sign
    const eq_pos = std.mem.indexOf(u8, line, "=") orelse return false;
    const after_eq = std.mem.trimLeft(u8, line[eq_pos + 1 ..], " \t");

    // Check if it starts with a lowercase letter (module reference)
    if (after_eq.len == 0) return false;
    const first_char = after_eq[0];
    if (first_char < 'a' or first_char > 'z') return false;

    // Check if it contains a dot and ends with semicolon (but not a function call)
    if (std.mem.indexOf(u8, after_eq, ".") == null) return false;
    if (std.mem.indexOf(u8, after_eq, "(") != null) return false;
    if (!std.mem.endsWith(u8, std.mem.trimRight(u8, after_eq, " \t"), ";")) return false;

    return true;
}

fn getNewZigFiles(allocator: Allocator) !PathList {
    var result = PathList{};
    errdefer {
        for (result.items) |path| {
            allocator.free(path);
        }
        result.deinit(allocator);
    }

    // Run git diff to get new files
    var child = std.process.Child.init(&.{ "git", "diff", "--name-only", "--diff-filter=A", "origin/main", "HEAD", "--", "src/" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Ignore;

    _ = child.spawn() catch {
        // Git not available or not in a repo - return empty list
        return result;
    };

    const stdout = child.stdout orelse return result;
    const output = stdout.readToEndAlloc(allocator, max_file_bytes) catch return result;
    defer allocator.free(output);

    const term = child.wait() catch return result;
    if (term.Exited != 0) return result;

    // Parse output line by line
    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (!std.mem.endsWith(u8, line, ".zig")) continue;

        try result.append(allocator, try allocator.dupe(u8, line));
    }

    return result;
}

fn fileHasTopLevelComment(allocator: Allocator, file_path: []const u8) !bool {
    const source = readSourceFile(allocator, file_path) catch |err| switch (err) {
        // File was deleted but still shows in git diff - skip it
        error.FileNotFound => return true,
        else => return err,
    };
    defer allocator.free(source);

    return std.mem.indexOf(u8, source, "//!") != null;
}

fn readSourceFile(allocator: Allocator, path: []const u8) ![:0]u8 {
    return try std.fs.cwd().readFileAllocOptions(
        allocator,
        path,
        max_file_bytes,
        null,
        std.mem.Alignment.of(u8),
        0,
    );
}

fn freePathList(list: *PathList, allocator: Allocator) void {
    for (list.items) |path| {
        allocator.free(path);
    }
    list.deinit(allocator);
}
