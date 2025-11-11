const std = @import("std");

const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const PathList = std.ArrayList([]u8);

const max_file_bytes: usize = 16 * 1024 * 1024;

const test_exclusions = [_][]const u8{
    "src/cli",
    "src/watch",
    "src/snapshot_tool",
};

const test_file_exclusions = [_][]const u8{
    // TODO: This test got out of sync and is not straightforward to fix
    "src/eval/test/low_level_interp_test.zig",
    // TODO: remove this, fix is in another PR
    "src/check/test/custom_num_type_test.zig",
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
    try stdout.print("Found {d} file references in mod.zig files\n\n", .{referenced.count()});

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

        const next_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });

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

    if (shouldSkipTestPath(path)) {
        allocator.free(path);
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

fn shouldSkipTestPath(path: []const u8) bool {
    for (test_exclusions) |prefix| {
        if (hasDirPrefix(path, prefix)) return true;
    }
    return false;
}

fn shouldSkipTestFile(path: []const u8) bool {
    for (test_file_exclusions) |excluded| {
        if (std.mem.eql(u8, path, excluded)) return true;
    }
    return false;
}

fn hasDirPrefix(path: []const u8, prefix: []const u8) bool {
    if (!std.mem.startsWith(u8, path, prefix)) return false;
    return path.len == prefix.len or path[prefix.len] == '/';
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
        const candidate = try std.fs.path.join(allocator, &.{ current_dir, "mod.zig" });
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
