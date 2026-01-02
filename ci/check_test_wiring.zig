const std = @import("std");

const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const PathList = std.ArrayList([]u8);

const max_file_bytes: usize = 16 * 1024 * 1024;

const test_file_exclusions = [_][]const u8{
    // Copied from Zig stdlib; tests are tested upstream
    "src/backend/llvm/BitcodeReader.zig",
};

const TermColor = struct {
    pub const red = "\x1b[0;31m";
    pub const green = "\x1b[0;32m";
    pub const yellow = "\x1b[1;33m";
    pub const reset = "\x1b[0m";
};

pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_state = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_state.interface;

    try stdout.print("Checking test wiring in src/ directory...\n\n", .{});

    try stdout.print("Step 1: Finding all potential test files...\n", .{});
    var test_files = PathList{};
    defer freePathList(&test_files, gpa);

    var mod_files = PathList{};
    defer freePathList(&mod_files, gpa);

    try walkTree(gpa, "src", &test_files, &mod_files);
    try stdout.print("Found {d} potential test files\n\n", .{test_files.items.len});

    // Some tests are wired through build.zig rather than mod.zig files.
    // For example, the CLI tests are driven via src/cli/main.zig and
    // src/cli/test/roc_subcommands.zig test roots.
    //
    // To avoid false positives, we:
    // - Treat src/cli/main.zig as an additional aggregator when scanning @import()
    //   statements for wired test files.
    // - Treat src/cli/test/fx_platform_test.zig as an aggregator since it imports
    //   fx_test_specs.zig which contains shared test specifications.
    if (fileExists("src/cli/main.zig")) {
        try mod_files.append(gpa, try gpa.dupe(u8, "src/cli/main.zig"));
    }
    if (fileExists("src/cli/test/fx_platform_test.zig")) {
        try mod_files.append(gpa, try gpa.dupe(u8, "src/cli/test/fx_platform_test.zig"));
    }
    if (fileExists("src/cli/test/test_runner.zig")) {
        try mod_files.append(gpa, try gpa.dupe(u8, "src/cli/test/test_runner.zig"));
    }
    if (fileExists("src/cli/cli_error.zig")) {
        try mod_files.append(gpa, try gpa.dupe(u8, "src/cli/cli_error.zig"));
    }
    if (fileExists("src/snapshot_tool/main.zig")) {
        try mod_files.append(gpa, try gpa.dupe(u8, "src/snapshot_tool/main.zig"));
    }

    if (test_files.items.len == 0) {
        try stdout.print("{s}[OK]{s} No test files found to check\n", .{ TermColor.green, TermColor.reset });
        try stdout.flush();
        return;
    }

    try stdout.print("Step 2: Extracting test references from mod.zig files...\n", .{});
    var referenced = std.StringHashMap(void).init(gpa);
    defer {
        var it = referenced.keyIterator();
        while (it.next()) |key| {
            gpa.free(@constCast(key.*));
        }
        referenced.deinit();
    }

    for (mod_files.items) |mod_path| {
        try collectModImports(gpa, mod_path, &referenced);
    }
    // Also treat test roots declared in build.zig (b.addTest root_source_file)
    // as valid wiring for the corresponding files (e.g. src/cli/main.zig and
    // src/cli/test/roc_subcommands.zig).
    try markBuildTestRootsAsReferenced(gpa, &referenced);

    try stdout.print(
        "Found {d} file references in mod.zig files and build.zig test roots\n\n",
        .{referenced.count()},
    );

    try stdout.print("Step 3: Checking if all test files are properly wired...\n\n", .{});
    var unwired = PathList{};
    defer freePathList(&unwired, gpa);

    for (test_files.items) |test_path| {
        const key: []const u8 = test_path;
        if (!referenced.contains(key)) {
            try unwired.append(gpa, try gpa.dupe(u8, key));
        }
    }

    if (unwired.items.len > 0) {
        std.mem.sort([]u8, unwired.items, {}, lessThanPath);
        try stdout.print(
            "{s}[ERR]{s} Found {d} test file(s) that are NOT wired through mod.zig:\n\n",
            .{ TermColor.red, TermColor.reset, unwired.items.len },
        );

        for (unwired.items) |path| {
            const path_text: []const u8 = path;
            try stdout.print("  {s}[MISSING]{s} {s}\n", .{ TermColor.red, TermColor.reset, path_text });
            try printSuggestion(gpa, stdout, path_text);
            try stdout.print("\n", .{});
        }

        try stdout.print("{s}[ERR]{s} Test wiring issues found. Please fix the issues above.\n\n", .{
            TermColor.red,
            TermColor.reset,
        });
        try stdout.print("To fix:\n", .{});
        try stdout.print("1. Add missing std.testing.refAllDecls() calls to the appropriate mod.zig files\n", .{});
        try stdout.print("2. Ensure all modules with tests are listed in src/build/modules.zig test_configs\n\n", .{});
    } else {
        try stdout.print("{s}[OK]{s} All tests are properly wired!\n\n", .{ TermColor.green, TermColor.reset });
    }

    if (unwired.items.len > 0) {
        try stdout.flush();
        std.process.exit(1);
    }

    try stdout.flush();
}

/// Normalize path separators to forward slashes for consistent cross-platform comparison.
/// This is important because:
/// 1. Zig @import paths always use forward slashes
/// 2. We need consistent path comparison between walked files and mod.zig imports
fn normalizePath(allocator: Allocator, path: []u8) ![]u8 {
    if (comptime @import("builtin").os.tag == .windows) {
        const normalized = try allocator.dupe(u8, path);
        for (normalized) |*c| {
            if (c.* == '\\') c.* = '/';
        }
        allocator.free(path);
        return normalized;
    }
    return path;
}

fn walkTree(
    allocator: Allocator,
    dir_path: []const u8,
    test_files: *PathList,
    mod_files: *PathList,
) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .sym_link) continue;

        const joined_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
        const next_path = try normalizePath(allocator, joined_path);

        switch (entry.kind) {
            .directory => {
                defer allocator.free(next_path);
                try walkTree(allocator, next_path, test_files, mod_files);
            },
            .file => {
                try handleFile(allocator, next_path, entry.name, test_files, mod_files);
            },
            else => allocator.free(next_path),
        }
    }
}

fn handleFile(
    allocator: Allocator,
    path: []u8,
    file_name: []const u8,
    test_files: *PathList,
    mod_files: *PathList,
) !void {
    if (!std.mem.endsWith(u8, file_name, ".zig")) {
        allocator.free(path);
        return;
    }

    if (std.mem.eql(u8, file_name, "mod.zig")) {
        try mod_files.append(allocator, path);
        return;
    }

    if (shouldSkipTestFile(path)) {
        allocator.free(path);
        return;
    }

    if (try fileHasTestDecl(allocator, path)) {
        try test_files.append(allocator, path);
        return;
    }

    allocator.free(path);
}

fn shouldSkipTestFile(path: []const u8) bool {
    for (test_file_exclusions) |excluded| {
        if (std.mem.eql(u8, path, excluded)) return true;
    }
    return false;
}

fn fileHasTestDecl(allocator: Allocator, path: []const u8) !bool {
    const source = try readSourceFile(allocator, path);
    defer allocator.free(source);
    var tree = try Ast.parse(allocator, source, .zig);
    defer tree.deinit(allocator);

    const tags = tree.nodes.items(.tag);
    for (tags) |tag| {
        if (tag == .test_decl) {
            return true;
        }
    }

    return false;
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

fn collectModImports(
    allocator: Allocator,
    mod_path: []const u8,
    referenced: *std.StringHashMap(void),
) !void {
    const source = try readSourceFile(allocator, mod_path);
    defer allocator.free(source);

    var tree = try Ast.parse(allocator, source, .zig);
    defer tree.deinit(allocator);

    const tags = tree.tokens.items(.tag);
    var idx: usize = 0;
    while (idx < tree.tokens.len) : (idx += 1) {
        if (tags[idx] != .builtin) continue;
        const token_index = @as(Ast.TokenIndex, @intCast(idx));
        if (!std.mem.eql(u8, tree.tokenSlice(token_index), "@import")) continue;

        const import_path = try extractImportPath(allocator, &tree, idx) orelse continue;
        defer allocator.free(import_path);

        if (!std.mem.endsWith(u8, import_path, ".zig")) continue;

        const resolved = try resolveImportPath(allocator, mod_path, import_path);
        if (referenced.contains(resolved)) {
            allocator.free(resolved);
        } else {
            try referenced.put(resolved, {});
        }
    }
}

fn extractImportPath(
    allocator: Allocator,
    tree: *const Ast,
    builtin_token_index: usize,
) !?[]u8 {
    var cursor = builtin_token_index + 1;
    if (cursor >= tree.tokens.len) return null;
    if (tree.tokenTag(@intCast(cursor)) != .l_paren) return null;

    cursor += 1;
    if (cursor >= tree.tokens.len) return null;
    const str_token_index = @as(Ast.TokenIndex, @intCast(cursor));
    const tag = tree.tokenTag(str_token_index);
    if (tag != .string_literal) return null;

    const literal = tree.tokenSlice(str_token_index);
    if (literal.len < 2) return null;
    return try allocator.dupe(u8, literal[1 .. literal.len - 1]);
}

fn resolveImportPath(
    allocator: Allocator,
    mod_path: []const u8,
    import_path: []const u8,
) ![]u8 {
    const mod_dir = std.fs.path.dirname(mod_path) orelse ".";
    return std.fs.path.resolvePosix(allocator, &.{ mod_dir, import_path });
}

/// Mark files that are used as test roots in build.zig as "wired".
///
/// In addition to mod.zig imports, some tests are hooked up via explicit
/// `b.addTest` calls in build.zig (for example the CLI tests). Any Zig
/// file that is used as a `root_source_file = b.path("...")` in such a
/// test configuration should not be reported as missing wiring.
fn markBuildTestRootsAsReferenced(
    allocator: Allocator,
    referenced: *std.StringHashMap(void),
) !void {
    const build_path = "build.zig";
    if (!fileExists(build_path)) return;

    const source = try readSourceFile(allocator, build_path);
    defer allocator.free(source);

    const pattern = ".root_source_file = b.path(\"";
    var search_index: usize = 0;

    while (std.mem.indexOfPos(u8, source, search_index, pattern)) |match_pos| {
        const literal_start = match_pos + pattern.len;
        var cursor = literal_start;

        // Find end of the string literal.
        while (cursor < source.len and source[cursor] != '"') : (cursor += 1) {}
        if (cursor >= source.len) break;

        const rel_path = source[literal_start..cursor];

        // Only consider Zig source files under src/ as potential test roots.
        if (!std.mem.endsWith(u8, rel_path, ".zig")) {
            search_index = cursor + 1;
            continue;
        }
        if (!std.mem.startsWith(u8, rel_path, "src/")) {
            search_index = cursor + 1;
            continue;
        }

        const key = try allocator.dupe(u8, rel_path);
        if (referenced.contains(key)) {
            allocator.free(key);
        } else {
            try referenced.put(key, {});
        }

        search_index = cursor + 1;
    }
}

fn lessThanPath(_: void, lhs: []u8, rhs: []u8) bool {
    const l: []const u8 = lhs;
    const r: []const u8 = rhs;
    return std.mem.lessThan(u8, l, r);
}

fn printSuggestion(
    allocator: Allocator,
    writer: anytype,
    test_path: []const u8,
) !void {
    const maybe_mod = try findNearestMod(allocator, test_path);
    if (maybe_mod) |mod_path| {
        defer allocator.free(mod_path);

        const mod_dir = std.fs.path.dirname(mod_path) orelse ".";
        const relative = try std.fs.path.relativePosix(allocator, mod_dir, test_path);
        defer allocator.free(relative);

        try writer.print("    {s}[HINT]{s} Should be added to {s}\n", .{
            TermColor.yellow,
            TermColor.reset,
            mod_path,
        });
        try writer.print(
            "    {s}[HINT]{s} Add: std.testing.refAllDecls(@import(\"{s}\"));\n",
            .{ TermColor.yellow, TermColor.reset, relative },
        );
    } else {
        try writer.print(
            "    {s}[HINT]{s} No nearby mod.zig found for this test file\n",
            .{ TermColor.yellow, TermColor.reset },
        );
    }
}

fn findNearestMod(allocator: Allocator, file_path: []const u8) !?[]u8 {
    var current_dir_opt = std.fs.path.dirname(file_path);
    while (current_dir_opt) |current_dir| {
        const joined = try std.fs.path.join(allocator, &.{ current_dir, "mod.zig" });
        const candidate = try normalizePath(allocator, joined);
        if (fileExists(candidate)) {
            return candidate;
        }
        allocator.free(candidate);
        current_dir_opt = std.fs.path.dirname(current_dir);
    }
    return null;
}

fn fileExists(path: []const u8) bool {
    _ = std.fs.cwd().statFile(path) catch return false;
    return true;
}

fn freePathList(list: *PathList, allocator: Allocator) void {
    for (list.items) |path| {
        allocator.free(path);
    }
    list.deinit(allocator);
}
