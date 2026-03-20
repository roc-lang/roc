//! Utilities for CLI tests using the actual roc binary.

const std = @import("std");
var next_cache_dir_id: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

pub const roc_binary_path = if (@import("builtin").os.tag == .windows) ".\\zig-out\\bin\\roc.exe" else "./zig-out/bin/roc";

/// Result of executing a Roc command during testing.
/// Contains the captured output streams and process termination status.
pub const RocResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

fn createIsolatedTestCacheDir(allocator: std.mem.Allocator) ![]u8 {
    const cache_dir_id = next_cache_dir_id.fetchAdd(1, .monotonic);
    const cache_leaf = try std.fmt.allocPrint(allocator, "{d}-{d}", .{
        @as(u64, @intCast(std.time.nanoTimestamp())),
        cache_dir_id,
    });
    defer allocator.free(cache_leaf);

    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);

    const cache_rel = try std.fs.path.join(allocator, &.{ ".zig-cache", "roc-test-cache", cache_leaf });
    defer allocator.free(cache_rel);

    std.fs.cwd().makePath(cache_rel) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    return std.fs.path.join(allocator, &.{ cwd_path, cache_rel });
}

/// Build an environment map for a test Roc subprocess.
/// Unless the caller already set `ROC_CACHE_DIR`, this gives the subprocess a
/// unique cache root so CLI tests do not share cache state accidentally.
pub fn buildIsolatedTestEnvMap(
    allocator: std.mem.Allocator,
    extra_env: ?*const std.process.EnvMap,
) !std.process.EnvMap {
    var env_map = try std.process.getEnvMap(allocator);
    errdefer env_map.deinit();

    if (extra_env) |extra| {
        var it = extra.iterator();
        while (it.next()) |entry| {
            try env_map.put(entry.key_ptr.*, entry.value_ptr.*);
        }
    }

    if (env_map.get("ROC_CACHE_DIR") == null) {
        const cache_dir = try createIsolatedTestCacheDir(allocator);
        defer allocator.free(cache_dir);
        try env_map.put("ROC_CACHE_DIR", cache_dir);
    }

    return env_map;
}

fn runChild(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    cwd_path: []const u8,
    extra_env: ?*const std.process.EnvMap,
) !RocResult {
    var env_map = try buildIsolatedTestEnvMap(allocator, extra_env);
    defer env_map.deinit();

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
        .cwd = cwd_path,
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024, // 10MB
    });

    return RocResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .term = result.term,
    };
}

/// Helper to run roc with arguments that don't require a test file
pub fn runRocCommand(allocator: std.mem.Allocator, args: []const []const u8) !RocResult {
    return runRocCommandWithEnv(allocator, args, null);
}

/// Run a roc CLI command with optional extra environment variables.
pub fn runRocCommandWithEnv(
    allocator: std.mem.Allocator,
    args: []const []const u8,
    extra_env: ?*const std.process.EnvMap,
) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer allocator.free(roc_path);

    // Build argv: [roc_path, ...args]
    const argv = try std.mem.concat(allocator, []const u8, &.{
        &.{roc_path},
        args,
    });
    defer allocator.free(argv);

    return runChild(allocator, argv, cwd_path, extra_env);
}

/// Helper to set up and run roc with arbitrary arguments
pub fn runRoc(allocator: std.mem.Allocator, args: []const []const u8, test_file_path: []const u8) !RocResult {
    return runRocWithEnv(allocator, args, test_file_path, null);
}

/// Run roc on a test file with optional extra environment variables.
pub fn runRocWithEnv(
    allocator: std.mem.Allocator,
    args: []const []const u8,
    test_file_path: []const u8,
    extra_env: ?*const std.process.EnvMap,
) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer allocator.free(roc_path);

    const test_file = try std.fs.path.join(allocator, &.{ cwd_path, test_file_path });
    defer allocator.free(test_file);

    // Build argv: [roc_path, ...args, test_file]
    const argv = try std.mem.concat(allocator, []const u8, &.{
        &.{roc_path},
        args,
        &.{test_file},
    });
    defer allocator.free(argv);

    return runChild(allocator, argv, cwd_path, extra_env);
}

/// Runs a roc app with --test mode using the given IO spec.
/// Spec format: "0<stdin|1>stdout|2>stderr" (pipe-separated)
/// Returns success if the app's IO matches the spec exactly.
pub fn runRocTest(allocator: std.mem.Allocator, roc_file: []const u8, spec: []const u8) !RocResult {
    return runRocCommand(allocator, &.{ roc_file, "--", "--test", spec });
}

/// Check if a run result indicates success (exit code 0).
/// Also checks for GPA memory errors in stderr.
pub fn checkSuccess(result: RocResult) !void {
    if (std.mem.indexOf(u8, result.stderr, "error(gpa):") != null) {
        std.debug.print("Memory error detected (GPA)\n", .{});
        std.debug.print("STDOUT: {s}\n", .{result.stdout});
        std.debug.print("STDERR: {s}\n", .{result.stderr});
        return error.MemoryError;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{result.stdout});
                std.debug.print("STDERR: {s}\n", .{result.stderr});
                return error.RunFailed;
            }
        },
        .Signal => |sig| {
            std.debug.print("Process terminated by signal: {}\n", .{sig});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{result.term});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.RunFailed;
        },
    }
}

/// Check if a run result indicates failure (non-zero exit code).
/// Verifies the process exited cleanly with a non-zero code, NOT that it crashed.
pub fn checkFailure(result: RocResult) !void {
    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("ERROR: roc succeeded but we expected it to fail\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        .Signal => |sig| {
            std.debug.print("ERROR: Process crashed with signal {} (expected clean failure with non-zero exit code)\n", .{sig});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("ERROR: Process terminated abnormally: {} (expected clean failure with non-zero exit code)\n", .{result.term});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.RunFailed;
        },
    }
}

/// Check if a test mode run succeeded (exit code 0).
/// Also checks for GPA memory errors.
pub fn checkTestSuccess(result: RocResult) !void {
    if (std.mem.indexOf(u8, result.stderr, "error(gpa):") != null) {
        std.debug.print("Memory error detected (GPA)\n", .{});
        std.debug.print("STDERR: {s}\n", .{result.stderr});
        return error.MemoryError;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Test failed with exit code {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{result.stderr});
                return error.TestFailed;
            }
        },
        .Signal => |sig| {
            std.debug.print("Process terminated by signal: {}\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("Test terminated abnormally: {}\n", .{result.term});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.TestFailed;
        },
    }
}

/// Helper to run roc with stdin input (for REPL testing)
pub fn runRocWithStdin(allocator: std.mem.Allocator, args: []const []const u8, stdin_input: []const u8) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer allocator.free(roc_path);

    // Build argv: [roc_path, ...args]
    const argv = try std.mem.concat(allocator, []const u8, &.{
        &.{roc_path},
        args,
    });
    defer allocator.free(argv);

    // Run roc with stdin pipe
    var child = std.process.Child.init(argv, allocator);
    var env_map = try buildIsolatedTestEnvMap(allocator, null);
    defer env_map.deinit();
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.cwd = cwd_path;
    child.env_map = &env_map;

    try child.spawn();

    // Write input to stdin and close it
    try child.stdin.?.writeAll(stdin_input);
    child.stdin.?.close();
    child.stdin = null;

    // Collect output before waiting
    const stdout = try child.stdout.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    errdefer allocator.free(stdout);
    const stderr = try child.stderr.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    errdefer allocator.free(stderr);

    // Wait for completion
    const term = try child.wait();

    return RocResult{
        .stdout = stdout,
        .stderr = stderr,
        .term = term,
    };
}
