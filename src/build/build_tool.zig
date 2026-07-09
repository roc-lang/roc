const std = @import("std");
const builtin = @import("builtin");

const CheckFailed = error{CheckFailed};
const sep60 = "============================================================";
const sep80 = "================================================================================";

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 2) {
        std.debug.print("usage: build_tool <command> [args]\n", .{});
        std.process.exit(2);
    }

    const command = args[1];
    const command_args = args[2..];

    if (std.mem.eql(u8, command, "remove-dir-tree")) {
        if (command_args.len != 1) return usage(command, "<path>");
        return removeDirTree(io, command_args[0]);
    } else if (std.mem.eql(u8, command, "fix-archive-padding")) {
        if (command_args.len != 1) return usage(command, "<archive>");
        return fixArchivePadding(io, command_args[0]);
    } else if (std.mem.eql(u8, command, "clear-roc-cache")) {
        if (command_args.len != 0) return usage(command, "");
        return clearRocCache(allocator, io, init.environ_map);
    } else if (std.mem.eql(u8, command, "print-build-success")) {
        if (command_args.len != 0) return usage(command, "");
        std.debug.print("Build succeeded!\n", .{});
    } else if (std.mem.eql(u8, command, "tests-summary")) {
        if (command_args.len != 0) return usage(command, "");
        std.debug.print("All selected Zig test steps passed.\n", .{});
    } else if (std.mem.eql(u8, command, "check-type-checker-patterns")) {
        if (command_args.len != 0) return usage(command, "");
        return checkTypeCheckerPatterns(allocator, io);
    } else if (std.mem.eql(u8, command, "check-enum-from-int-zero")) {
        if (command_args.len != 0) return usage(command, "");
        return checkEnumFromIntZero(allocator, io);
    } else if (std.mem.eql(u8, command, "check-unused-suppression")) {
        if (command_args.len != 0) return usage(command, "");
        return checkUnusedSuppression(allocator, io);
    } else if (std.mem.eql(u8, command, "check-postcheck-architecture")) {
        if (command_args.len != 0) return usage(command, "");
        return runPerlCheck(io, &.{ "perl", "ci/check_postcheck_architecture.pl" }, "Post-check architecture check failed. Run 'perl ci/check_postcheck_architecture.pl' to see details.", "ci/check_postcheck_architecture.pl terminated abnormally");
    } else if (std.mem.eql(u8, command, "semantic-audit")) {
        if (command_args.len != 0) return usage(command, "");
        return runPerlCheck(io, &.{ "perl", "ci/semantic_audit.pl" }, "Semantic audit failed. Run 'perl ci/semantic_audit.pl' to see details.", "ci/semantic_audit.pl terminated abnormally");
    } else if (std.mem.eql(u8, command, "check-panic-usage")) {
        if (command_args.len != 0) return usage(command, "");
        return checkPanicUsage(allocator, io);
    } else if (std.mem.eql(u8, command, "check-cli-global-stdio")) {
        if (command_args.len != 0) return usage(command, "");
        return checkCliGlobalStdio(allocator, io);
    } else if (std.mem.eql(u8, command, "checkfx-inner")) {
        if (command_args.len != 0) return usage(command, "");
        return checkFxPlatformTestCoverage(allocator, io);
    } else if (std.mem.eql(u8, command, "coverage-summary")) {
        if (command_args.len != 4) return usage(command, "<coverage-dir> <exe-name> <label> <min-coverage>");
        const min_coverage = try std.fmt.parseFloat(f64, command_args[3]);
        return coverageSummary(allocator, io, command_args[0], command_args[1], command_args[2], min_coverage);
    } else if (std.mem.eql(u8, command, "coverage-unsupported")) {
        if (command_args.len != 0) return usage(command, "");
        std.debug.print("\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        std.debug.print("COVERAGE NOT SUPPORTED\n", .{});
        std.debug.print(sep60 ++ "\n\n", .{});
        std.debug.print("kcov parser coverage is currently enabled only on Linux ARM64.\n", .{});
        std.debug.print("Current platform: {s}\n\n", .{@tagName(builtin.target.os.tag)});
        std.debug.print(sep60 ++ "\n", .{});
    } else {
        std.debug.print("unknown build_tool command: {s}\n", .{command});
        std.process.exit(2);
    }
}

fn usage(command: []const u8, args: []const u8) noreturn {
    std.debug.print("usage: build_tool {s}", .{command});
    if (args.len != 0) std.debug.print(" {s}", .{args});
    std.debug.print("\n", .{});
    std.process.exit(2);
}

fn fail(comptime format: []const u8, args: anytype) CheckFailed {
    std.debug.print(format ++ "\n", args);
    return error.CheckFailed;
}

fn removeDirTree(io: std.Io, path: []const u8) !void {
    std.Io.Dir.cwd().deleteTree(io, path) catch {};
}

fn clearRocCache(allocator: std.mem.Allocator, io: std.Io, environ_map: *std.process.Environ.Map) !void {
    const cache_dir = getCacheDir(allocator, environ_map.*) catch |err| {
        std.debug.print("Warning: Could not determine cache directory: {s}\n", .{@errorName(err)});
        return;
    };
    defer allocator.free(cache_dir);

    std.Io.Dir.cwd().access(io, cache_dir, .{}) catch {
        std.debug.print("Roc cache not found (nothing to clear)\n", .{});
        return;
    };

    std.Io.Dir.cwd().deleteTree(io, cache_dir) catch |err| {
        std.debug.print("Warning: Could not clear cache at {s}: {s}\n", .{ cache_dir, @errorName(err) });
        return;
    };

    std.debug.print("Cleared roc cache at {s}\n", .{cache_dir});
}

fn getCacheDir(allocator: std.mem.Allocator, environ_map: std.process.Environ.Map) ![]u8 {
    const cache_dir_name = switch (builtin.os.tag) {
        .windows => "Roc",
        else => "roc",
    };

    if (environ_map.get("XDG_CACHE_HOME")) |xdg_cache| {
        return std.fs.path.join(allocator, &[_][]const u8{ xdg_cache, cache_dir_name });
    }

    const home_env = switch (builtin.os.tag) {
        .windows => "APPDATA",
        else => "HOME",
    };

    const home_dir = environ_map.get(home_env) orelse return error.NoHomeDirectory;

    return switch (builtin.os.tag) {
        .linux => std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", cache_dir_name }),
        .macos => std.fs.path.join(allocator, &[_][]const u8{ home_dir, "Library", "Caches", cache_dir_name }),
        .windows => std.fs.path.join(allocator, &[_][]const u8{ home_dir, cache_dir_name }),
        else => std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", cache_dir_name }),
    };
}

fn fixArchivePadding(io: std.Io, archive_path: []const u8) !void {
    const file = std.Io.Dir.cwd().openFile(io, archive_path, .{ .mode = .read_write }) catch {
        return;
    };
    defer file.close(io);

    const stat = try file.stat(io);
    var file_size = stat.size;

    if (file_size % 2 == 1) {
        try file.writePositionalAll(io, "\n", file_size);
        file_size += 1;
    }

    var header_buf: [8]u8 = undefined;
    _ = try file.readPositionalAll(io, &header_buf, 0);
    if (!std.mem.eql(u8, &header_buf, "!<arch>\n")) {
        std.debug.print("Warning: Invalid archive magic in {s}\n", .{archive_path});
        return;
    }

    var offset: u64 = 8;
    while (offset + 60 <= file_size) {
        var size_buf: [10]u8 = undefined;
        _ = try file.readPositionalAll(io, &size_buf, offset + 48);

        var size: u64 = 0;
        for (size_buf) |c| {
            if (c >= '0' and c <= '9') {
                size = size * 10 + (c - '0');
            } else break;
        }

        offset += 60 + size;
        if (size % 2 == 1) offset += 1;

        if (offset == file_size) break;

        if (offset > file_size) {
            const missing = offset - file_size;
            const padding = "\n\n";
            try file.writePositionalAll(io, padding[0..@min(missing, 2)], file_size);
            break;
        }
    }
}

fn runPerlCheck(
    io: std.Io,
    argv: []const []const u8,
    failure_message: []const u8,
    abnormal_message: []const u8,
) !void {
    if (builtin.os.tag == .windows) {
        std.debug.print("Skipping {s} on Windows (perl not available)\n", .{argv[1]});
        return;
    }

    var child = try std.process.spawn(io, .{ .argv = argv });
    const term = try child.wait(io);

    switch (term) {
        .exited => |code| {
            if (code != 0) return fail("{s}", .{failure_message});
        },
        else => return fail("{s}", .{abnormal_message}),
    }
}

const vendored_zig_marker = "Adapted from the Zig compiler";

const Violation = struct {
    file_path: []const u8,
    line_number: usize,
    line_content: []const u8,
    pattern: ?[]const u8 = null,
};

fn printViolations(violations: []const Violation) void {
    for (violations) |violation| {
        if (violation.pattern) |pattern| {
            std.debug.print("  {s}:{d}: found `{s}` in: {s}\n", .{
                violation.file_path,
                violation.line_number,
                pattern,
                violation.line_content,
            });
        } else {
            std.debug.print("  {s}:{d}: {s}\n", .{
                violation.file_path,
                violation.line_number,
                violation.line_content,
            });
        }
    }
}

fn checkTypeCheckerPatterns(allocator: std.mem.Allocator, io: std.Io) !void {
    var violations = std.ArrayList(Violation).empty;
    defer violations.deinit(allocator);

    const dirs_to_scan = [_][]const u8{ "src/check", "src/layout", "src/eval" };
    for (dirs_to_scan) |dir_path| {
        var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| {
            return fail("Failed to open {s} directory: {}", .{ dir_path, err });
        };
        defer dir.close(io);

        try scanDirectoryForTypeCheckerPatterns(allocator, io, dir, dir_path, &violations);
    }

    if (violations.items.len > 0) {
        std.debug.print("\n", .{});
        std.debug.print(sep80 ++ "\n", .{});
        std.debug.print("FORBIDDEN PATTERN DETECTED\n", .{});
        std.debug.print(sep80 ++ "\n\n", .{});
        std.debug.print(
            \\Code in src/canonicalize/, src/check/, src/layout/, and src/eval/ must NOT do raw string comparison or manipulation.
            \\
            \\WHY THIS RULE EXISTS:
            \\  We NEVER do string or byte comparisons because:
            \\
            \\  1. PERFORMANCE: String comparisons take O(n) time where n is the string
            \\     length. These code paths can involve many comparisons, so this adds up.
            \\
            \\  2. BRITTLENESS: String comparisons make the code sensitive to changes it
            \\     shouldn't care about (e.g., how identifiers are rendered, whitespace,
            \\     formatting). This leads to subtle bugs.
            \\
            \\WHAT TO DO INSTEAD:
            \\  Always compare indices rather than strings:
            \\
            \\  - For identifiers: Compare Ident.Idx values (interned string indices)
            \\  - For types: Compare type variable indices or node store indices
            \\  - For expressions: Compare Expr.Idx values from the node store
            \\
            \\VIOLATIONS FOUND:
            \\
        , .{});
        printViolations(violations.items);
        std.debug.print("\n" ++ sep80 ++ "\n", .{});
        return fail("Found {d} forbidden patterns (raw string comparison or manipulation).", .{violations.items.len});
    }
}

const ExcludedRange = struct { file: []const u8, start: usize, end: usize };

const type_checker_excluded_ranges = [_]ExcludedRange{
    .{ .file = "Check.zig", .start = 5530, .end = 5547 },
    .{ .file = "store.zig", .start = 340, .end = 355 },
    .{ .file = "cir_to_lir.zig", .start = 110, .end = 115 },
};

fn isInRange(file_path: []const u8, line_number: usize, ranges: []const ExcludedRange) bool {
    for (ranges) |range| {
        if (std.mem.endsWith(u8, file_path, range.file) and line_number >= range.start and line_number <= range.end) {
            return true;
        }
    }
    return false;
}

fn scanDirectoryForTypeCheckerPatterns(
    allocator: std.mem.Allocator,
    io: std.Io,
    dir: std.Io.Dir,
    path_prefix: []const u8,
    violations: *std.ArrayList(Violation),
) !void {
    var walker = try dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;
        if (std.mem.endsWith(u8, entry.path, "_test.zig")) continue;
        if (std.mem.find(u8, entry.path, "test/") != null) continue;
        if (std.mem.startsWith(u8, entry.path, "test")) continue;
        if (std.mem.endsWith(u8, entry.path, "test_runner.zig")) continue;

        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

        const content = dir.readFileAlloc(io, entry.path, allocator, .limited(10 * 1024 * 1024)) catch continue;
        defer allocator.free(content);

        var line_number: usize = 1;
        var line_start: usize = 0;

        for (content, 0..) |char, i| {
            if (char != '\n') continue;
            const line = content[line_start..i];
            const trimmed = std.mem.trim(u8, line, " \t");

            defer {
                line_number += 1;
                line_start = i + 1;
            }

            if (std.mem.startsWith(u8, trimmed, "//")) continue;

            if (std.mem.find(u8, line, "std.mem.")) |idx| {
                const after_match = line[idx + 8 ..];
                const is_allowed =
                    std.mem.startsWith(u8, after_match, "Allocator") or
                    std.mem.startsWith(u8, after_match, "Alignment") or
                    std.mem.startsWith(u8, after_match, "sort") or
                    std.mem.startsWith(u8, after_match, "asBytes") or
                    std.mem.startsWith(u8, after_match, "reverse") or
                    std.mem.startsWith(u8, after_match, "alignForward") or
                    std.mem.startsWith(u8, after_match, "order") or
                    std.mem.startsWith(u8, after_match, "copyForwards");

                if (!is_allowed and !isInRange(full_path, line_number, &type_checker_excluded_ranges)) {
                    try violations.append(allocator, .{
                        .file_path = full_path,
                        .line_number = line_number,
                        .line_content = try allocator.dupe(u8, trimmed),
                    });
                }
            }

            if (std.mem.find(u8, line, "findByString") != null and !isInRange(full_path, line_number, &type_checker_excluded_ranges)) {
                try violations.append(allocator, .{ .file_path = full_path, .line_number = line_number, .line_content = try allocator.dupe(u8, trimmed) });
            }
            if (std.mem.find(u8, line, "findIdent") != null and !isInRange(full_path, line_number, &type_checker_excluded_ranges)) {
                try violations.append(allocator, .{ .file_path = full_path, .line_number = line_number, .line_content = try allocator.dupe(u8, trimmed) });
            }
            if (std.mem.find(u8, line, "getMethodIdent") != null and !isInRange(full_path, line_number, &type_checker_excluded_ranges)) {
                try violations.append(allocator, .{ .file_path = full_path, .line_number = line_number, .line_content = try allocator.dupe(u8, trimmed) });
            }
        }
    }
}

fn checkEnumFromIntZero(allocator: std.mem.Allocator, io: std.Io) !void {
    var violations = std.ArrayList(Violation).empty;
    defer violations.deinit(allocator);

    var dir = std.Io.Dir.cwd().openDir(io, "src", .{ .iterate = true }) catch |err| {
        return fail("Failed to open src directory: {}", .{err});
    };
    defer dir.close(io);

    try scanDirectoryForEnumFromIntZero(allocator, io, dir, "src", &violations);

    if (violations.items.len > 0) {
        std.debug.print("\n", .{});
        std.debug.print(sep80 ++ "\n", .{});
        std.debug.print("FORBIDDEN PATTERN: @enumFromInt(0)\n", .{});
        std.debug.print(sep80 ++ "\n\n", .{});
        std.debug.print(
            \\Using @enumFromInt(0) is forbidden in this codebase.
            \\
            \\WHAT TO DO INSTEAD:
            \\  If you need a placeholder value that you believe will never be read,
            \\  use `undefined` instead, with a comment explaining why it is correct.
            \\
            \\VIOLATIONS FOUND:
            \\
        , .{});
        printViolations(violations.items);
        std.debug.print("\n" ++ sep80 ++ "\n", .{});
        return fail("Found {d} uses of @enumFromInt(0).", .{violations.items.len});
    }
}

fn scanDirectoryForEnumFromIntZero(
    allocator: std.mem.Allocator,
    io: std.Io,
    dir: std.Io.Dir,
    path_prefix: []const u8,
    violations: *std.ArrayList(Violation),
) !void {
    var walker = try dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

        const content = dir.readFileAlloc(io, entry.path, allocator, .limited(10 * 1024 * 1024)) catch continue;
        defer allocator.free(content);

        if (std.mem.find(u8, content, vendored_zig_marker) != null) continue;

        var line_number: usize = 1;
        var line_start: usize = 0;

        for (content, 0..) |char, i| {
            if (char != '\n') continue;
            const line = content[line_start..i];
            const trimmed = std.mem.trim(u8, line, " \t");

            defer {
                line_number += 1;
                line_start = i + 1;
            }

            if (std.mem.startsWith(u8, trimmed, "//")) continue;

            if (std.mem.find(u8, line, "@enumFromInt(0)") != null) {
                try violations.append(allocator, .{ .file_path = full_path, .line_number = line_number, .line_content = try allocator.dupe(u8, trimmed) });
            }
        }
    }
}

fn checkUnusedSuppression(allocator: std.mem.Allocator, io: std.Io) !void {
    var violations = std.ArrayList(Violation).empty;
    defer violations.deinit(allocator);

    var dir = std.Io.Dir.cwd().openDir(io, "src", .{ .iterate = true }) catch |err| {
        return fail("Failed to open src/ directory: {}", .{err});
    };
    defer dir.close(io);

    try scanDirectoryForUnusedSuppression(allocator, io, dir, "src", &violations);

    if (violations.items.len > 0) {
        std.debug.print("\n", .{});
        std.debug.print(sep80 ++ "\n", .{});
        std.debug.print("UNUSED VARIABLE SUPPRESSION DETECTED\n", .{});
        std.debug.print(sep80 ++ "\n\n", .{});
        std.debug.print(
            \\In this codebase, we do NOT use `_ = variable;` to suppress unused warnings.
            \\
            \\VIOLATIONS FOUND:
            \\
        , .{});
        printViolations(violations.items);
        std.debug.print("\n" ++ sep80 ++ "\n", .{});
        return fail("Found {d} unused variable suppression patterns (`_ = identifier;`).", .{violations.items.len});
    }
}

fn scanDirectoryForUnusedSuppression(
    allocator: std.mem.Allocator,
    io: std.Io,
    dir: std.Io.Dir,
    path_prefix: []const u8,
    violations: *std.ArrayList(Violation),
) !void {
    var walker = try dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

        const content = dir.readFileAlloc(io, entry.path, allocator, .limited(10 * 1024 * 1024)) catch continue;
        defer allocator.free(content);

        if (std.mem.find(u8, content, vendored_zig_marker) != null) continue;

        var line_number: usize = 1;
        var line_start: usize = 0;

        for (content, 0..) |char, i| {
            if (char != '\n') continue;
            const line = content[line_start..i];
            const trimmed = std.mem.trim(u8, line, " \t");

            defer {
                line_number += 1;
                line_start = i + 1;
            }

            if (isUnusedSuppression(trimmed)) {
                try violations.append(allocator, .{ .file_path = full_path, .line_number = line_number, .line_content = try allocator.dupe(u8, trimmed) });
            }
        }
    }
}

fn isUnusedSuppression(line: []const u8) bool {
    if (!std.mem.startsWith(u8, line, "_ = ")) return false;
    if (!std.mem.endsWith(u8, line, ";")) return false;

    const identifier = line[4 .. line.len - 1];
    if (identifier.len == 0) return false;

    for (identifier) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '.') return false;
    }

    return true;
}

const panic_scan_files = [_][]const u8{
    "src/eval/interpreter.zig",
};

const panic_scan_dirs = [_][]const u8{
    "src/builtins",
};

const panic_excluded_files = [_][]const u8{
    "fuzz_sort.zig",
};

const panic_allowlist_patterns = [_][]const u8{
    "trace_modules",
};

const panic_excluded_ranges = [_]ExcludedRange{
    .{ .file = "utils.zig", .start = 60, .end = 214 },
    .{ .file = "Check.zig", .start = 5530, .end = 5547 },
};

fn isPanicExcludedFile(file_name: []const u8) bool {
    for (panic_excluded_files) |excluded| {
        if (std.mem.eql(u8, file_name, excluded)) return true;
    }
    return false;
}

fn isPanicAllowlisted(line: []const u8) bool {
    for (panic_allowlist_patterns) |pattern| {
        if (std.mem.find(u8, line, pattern) != null) return true;
    }
    return false;
}

fn scanPanicFile(allocator: std.mem.Allocator, io: std.Io, file_path: []const u8, violations: *std.ArrayList(Violation)) !void {
    const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(50 * 1024 * 1024)) catch |err| {
        std.debug.print("Warning: Failed to read {s}: {}\n", .{ file_path, err });
        return;
    };
    defer allocator.free(content);

    var line_number: usize = 1;
    var line_start: usize = 0;

    for (content, 0..) |char, i| {
        if (char != '\n') continue;
        const line = content[line_start..i];
        const trimmed = std.mem.trim(u8, line, " \t");

        defer {
            line_number += 1;
            line_start = i + 1;
        }

        if (std.mem.startsWith(u8, trimmed, "//")) continue;

        const has_panic = std.mem.find(u8, line, "@panic(") != null;
        const has_debug_panic = std.mem.find(u8, line, "std.debug.panic") != null;

        if ((has_panic or has_debug_panic) and !isPanicAllowlisted(line) and !isInRange(file_path, line_number, &panic_excluded_ranges)) {
            try violations.append(allocator, .{
                .file_path = try allocator.dupe(u8, file_path),
                .line_number = line_number,
                .line_content = try allocator.dupe(u8, trimmed),
            });
        }
    }
}

fn checkPanicUsage(allocator: std.mem.Allocator, io: std.Io) !void {
    var violations = std.ArrayList(Violation).empty;
    defer violations.deinit(allocator);

    for (panic_scan_files) |file_path| {
        try scanPanicFile(allocator, io, file_path, &violations);
    }

    for (panic_scan_dirs) |dir_path| {
        var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| {
            std.debug.print("Warning: Failed to open directory {s}: {}\n", .{ dir_path, err });
            continue;
        };
        defer dir.close(io);

        var iter = dir.iterate();
        while (try iter.next(io)) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".zig") and !isPanicExcludedFile(entry.name)) {
                const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
                defer allocator.free(full_path);
                try scanPanicFile(allocator, io, full_path, &violations);
            }
        }
    }

    if (violations.items.len > 0) {
        std.debug.print("\n", .{});
        std.debug.print(sep80 ++ "\n", .{});
        std.debug.print("FORBIDDEN PATTERN: @panic / std.debug.panic in runtime code\n", .{});
        std.debug.print(sep80 ++ "\n\n", .{});
        std.debug.print(
            \\Using @panic or std.debug.panic is forbidden in interpreter and builtins.
            \\
            \\VIOLATIONS FOUND:
            \\
        , .{});
        printViolations(violations.items);
        std.debug.print("\n" ++ sep80 ++ "\n", .{});
        return fail("Found {d} uses of @panic or std.debug.panic in runtime code.", .{violations.items.len});
    }
}

fn checkCliGlobalStdio(allocator: std.mem.Allocator, io: std.Io) !void {
    var violations = std.ArrayList(Violation).empty;
    defer violations.deinit(allocator);

    const file_path = "src/cli/main.zig";
    const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(10 * 1024 * 1024)) catch |err| {
        return fail("Failed to read {s}: {}", .{ file_path, err });
    };
    defer allocator.free(content);

    var line_number: usize = 1;
    var line_start: usize = 0;

    for (content, 0..) |char, i| {
        if (char != '\n') continue;
        const line = content[line_start..i];
        const trimmed = std.mem.trim(u8, line, " \t");

        defer {
            line_number += 1;
            line_start = i + 1;
        }

        const forbidden_patterns = [_][]const u8{
            "std.io.getStdOut()",
            "std.io.getStdErr()",
            "std.fs.File.stdout()",
            "std.fs.File.stderr()",
        };

        for (forbidden_patterns) |pattern| {
            if (std.mem.find(u8, trimmed, pattern) != null) {
                try violations.append(allocator, .{
                    .file_path = file_path,
                    .line_number = line_number,
                    .line_content = try allocator.dupe(u8, trimmed),
                    .pattern = pattern,
                });
            }
        }
    }

    if (violations.items.len > 0) {
        std.debug.print("\n", .{});
        std.debug.print(sep80 ++ "\n", .{});
        std.debug.print("GLOBAL STDIO USAGE DETECTED IN CLI\n", .{});
        std.debug.print(sep80 ++ "\n\n", .{});
        std.debug.print("In the CLI code, we use context-based I/O, not global stdio functions.\n\n", .{});
        std.debug.print("VIOLATIONS FOUND:\n\n", .{});
        printViolations(violations.items);
        std.debug.print("\n" ++ sep80 ++ "\n", .{});
        return fail("Found {d} global stdio usage(s) in CLI code.", .{violations.items.len});
    }
}

fn checkFxPlatformTestCoverage(allocator: std.mem.Allocator, io: std.Io) !void {
    std.debug.print("---- checking fx platform test coverage ----\n", .{});

    var fx_dir = try std.Io.Dir.cwd().openDir(io, "test/fx", .{ .iterate = true });
    defer fx_dir.close(io);

    var roc_files = std.ArrayList([]const u8).empty;
    defer {
        for (roc_files.items) |file| allocator.free(file);
        roc_files.deinit(allocator);
    }

    var dir_iter = fx_dir.iterate();
    while (try dir_iter.next(io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            try roc_files.append(allocator, try allocator.dupe(u8, entry.name));
        }
    }

    std.mem.sort([]const u8, roc_files.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    var tested_files = std.StringHashMap(void).init(allocator);
    defer {
        var key_iter = tested_files.keyIterator();
        while (key_iter.next()) |key| allocator.free(key.*);
        tested_files.deinit();
    }

    const test_files_to_scan = [_][]const u8{
        "src/cli/test/fx_platform_test.zig",
        "src/cli/test/fx_test_specs.zig",
    };

    for (test_files_to_scan) |test_file_path| {
        const test_file_contents = std.Io.Dir.cwd().readFileAlloc(io, test_file_path, allocator, .limited(1024 * 1024)) catch |err| {
            std.debug.print("Warning: Could not read {s}: {}\n", .{ test_file_path, err });
            continue;
        };
        defer allocator.free(test_file_contents);

        var line_iter = std.mem.splitScalar(u8, test_file_contents, '\n');
        while (line_iter.next()) |line| {
            var search_start: usize = 0;
            while (std.mem.findPos(u8, line, search_start, "test/fx/")) |idx| {
                const rest_of_line = line[idx..];
                if (std.mem.find(u8, rest_of_line, ".roc")) |roc_pos| {
                    const full_path = rest_of_line[0 .. roc_pos + 4];
                    const filename = full_path["test/fx/".len..];
                    if (std.mem.find(u8, filename, "/") == null) {
                        try tested_files.put(try allocator.dupe(u8, filename), {});
                    }
                }
                search_start = idx + 1;
            }
        }
    }

    var missing_tests = std.ArrayList([]const u8).empty;
    defer missing_tests.deinit(allocator);

    for (roc_files.items) |roc_file| {
        if (!tested_files.contains(roc_file)) {
            try missing_tests.append(allocator, roc_file);
        }
    }

    if (missing_tests.items.len > 0) {
        std.debug.print("\nERROR: The following .roc files in test/fx/ do not have tests:\n", .{});
        for (missing_tests.items) |missing_file| {
            std.debug.print("  - {s}\n", .{missing_file});
        }
        std.debug.print("\nPlease add tests in fx_platform_test.zig or fx_test_specs.zig, or remove these files from test/fx/.\n", .{});
        return fail("{d} .roc file(s) in test/fx/ are missing tests", .{missing_tests.items.len});
    }

    std.debug.print("All {d} .roc files in test/fx/ have tests.\n", .{roc_files.items.len});
}

fn coverageSummary(
    allocator: std.mem.Allocator,
    io: std.Io,
    coverage_dir: []const u8,
    exe_name: []const u8,
    label: []const u8,
    min_coverage: f64,
) !void {
    const json_path = try std.fmt.allocPrint(allocator, "{s}/{s}/coverage.json", .{ coverage_dir, exe_name });
    defer allocator.free(json_path);

    const json_content = std.Io.Dir.cwd().readFileAlloc(io, json_path, allocator, .limited(10 * 1024 * 1024)) catch |err| {
        std.debug.print("\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        std.debug.print("COVERAGE ERROR\n", .{});
        std.debug.print(sep60 ++ "\n\n", .{});
        std.debug.print("Could not open coverage JSON at {s}: {}\n", .{ json_path, err });
        std.debug.print("\nMake sure kcov is installed:\n", .{});
        std.debug.print("  - Linux: apt install kcov\n", .{});
        std.debug.print("  - macOS: brew install kcov\n\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        return;
    };
    defer allocator.free(json_content);

    const result = try parseCoverageJson(allocator, json_content, label, coverage_dir);

    if (result.total_lines == 0) {
        std.debug.print("\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        std.debug.print("COVERAGE ERROR: NO DATA CAPTURED\n", .{});
        std.debug.print(sep60 ++ "\n\n", .{});
        std.debug.print("kcov reported 0 total lines - coverage data was not captured.\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        return fail("kcov failed to capture coverage data (0 total lines)", .{});
    }

    if (result.percent < min_coverage) {
        std.debug.print("\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        std.debug.print("COVERAGE CHECK FAILED\n", .{});
        std.debug.print(sep60 ++ "\n\n", .{});
        std.debug.print("{s} coverage is {d:.2}%, minimum required is {d:.2}%\n", .{ label, result.percent, min_coverage });
        std.debug.print("Add more tests to improve coverage before merging.\n\n", .{});
        std.debug.print(sep60 ++ "\n", .{});
        return fail("{s} coverage {d:.2}% is below minimum {d:.2}%", .{ label, result.percent, min_coverage });
    }
}

const CoverageResult = struct {
    percent: f64,
    total_lines: u64,
};

const UncoveredFile = struct {
    file: []const u8,
    uncovered_lines: u64,
    total_lines: u64,
    percent: f64,
};

fn parseCoverageJson(allocator: std.mem.Allocator, json_content: []const u8, label: []const u8, coverage_dir: []const u8) !CoverageResult {
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_content, .{});
    defer parsed.deinit();

    const root = parsed.value;

    const total_lines: u64 = blk: {
        const val = root.object.get("total_lines") orelse break :blk 0;
        if (val != .integer) break :blk 0;
        break :blk @intCast(val.integer);
    };
    const covered_lines: u64 = blk: {
        const val = root.object.get("covered_lines") orelse break :blk 0;
        if (val != .integer) break :blk 0;
        break :blk @intCast(val.integer);
    };

    var uncovered_files = std.ArrayList(UncoveredFile).empty;
    defer {
        for (uncovered_files.items) |uf| allocator.free(uf.file);
        uncovered_files.deinit(allocator);
    }

    if (root.object.get("files")) |files_val| {
        if (files_val == .array) {
            for (files_val.array.items) |file_obj| {
                if (file_obj != .object) continue;

                const filename_val = file_obj.object.get("file") orelse continue;
                if (filename_val != .string) continue;
                const filename = filename_val.string;

                if (std.mem.find(u8, filename, "src/parse") == null) continue;
                if (std.mem.find(u8, filename, "/test/") != null) continue;

                const percent_val = file_obj.object.get("percent_covered") orelse continue;
                if (percent_val != .string) continue;

                const covered_str = file_obj.object.get("covered_lines") orelse continue;
                const total_str = file_obj.object.get("total_lines") orelse continue;
                if (covered_str != .string or total_str != .string) continue;

                const file_covered = std.fmt.parseInt(u64, covered_str.string, 10) catch 0;
                const file_total = std.fmt.parseInt(u64, total_str.string, 10) catch 0;
                const file_uncovered = file_total - file_covered;

                if (file_uncovered > 0) {
                    try uncovered_files.append(allocator, .{
                        .file = try allocator.dupe(u8, filename),
                        .uncovered_lines = file_uncovered,
                        .total_lines = file_total,
                        .percent = std.fmt.parseFloat(f64, percent_val.string) catch 0.0,
                    });
                }
            }
        }
    }

    const uncovered_lines = total_lines - covered_lines;
    const percent = if (total_lines > 0)
        @as(f64, @floatFromInt(covered_lines)) / @as(f64, @floatFromInt(total_lines)) * 100.0
    else
        0.0;

    std.debug.print("\n", .{});
    std.debug.print(sep60 ++ "\n", .{});
    std.debug.print("{s} CODE COVERAGE SUMMARY\n", .{label});
    std.debug.print(sep60 ++ "\n\n", .{});
    std.debug.print("Total lines:     {d}\n", .{total_lines});
    std.debug.print("Covered lines:   {d}\n", .{covered_lines});
    std.debug.print("Uncovered lines: {d}\n", .{uncovered_lines});
    std.debug.print("Coverage:        {d:.2}%\n\n", .{percent});

    if (uncovered_files.items.len > 0) {
        std.debug.print("Files with uncovered lines:\n", .{});

        std.mem.sort(UncoveredFile, uncovered_files.items, {}, struct {
            fn lessThan(_: void, a: UncoveredFile, b: UncoveredFile) bool {
                return a.uncovered_lines > b.uncovered_lines;
            }
        }.lessThan);

        for (uncovered_files.items) |uf| {
            const basename = std.fs.path.basename(uf.file);
            std.debug.print("  {s}: {d:.1}% covered ({d}/{d} lines uncovered)\n", .{
                basename,
                uf.percent,
                uf.uncovered_lines,
                uf.total_lines,
            });
        }
    }

    std.debug.print("\n" ++ sep60 ++ "\n", .{});
    std.debug.print("Full HTML report: {s}/index.html\n", .{coverage_dir});
    std.debug.print(sep60 ++ "\n", .{});

    return .{ .percent = percent, .total_lines = total_lines };
}
