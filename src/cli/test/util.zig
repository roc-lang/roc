//! Utilities for CLI tests using the actual roc binary.

const std = @import("std");
const builtin = @import("builtin");

/// Pick an IO suitable for the build context.
///
/// `std.Options.debug_io` defaults to a Threaded instance whose allocator is `.failing`,
/// so it cannot spawn child processes (the spawn path arena-allocates argv/envp before
/// fork()). In tests we use `std.testing.io`, which is initialized with `testing.allocator`.
const io: std.Io = if (builtin.is_test) std.testing.io else std.Options.debug_io;

var next_cache_dir_id: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

/// Absolute cache directory paths reserved for a single CLI test subprocess.
pub const IsolatedCacheDirs = struct {
    roc_cache_dir: []u8,
    zig_local_cache_dir: []u8,

    pub fn deinit(self: IsolatedCacheDirs, allocator: std.mem.Allocator) void {
        allocator.free(self.zig_local_cache_dir);
        allocator.free(self.roc_cache_dir);
    }
};

pub const roc_binary_path = if (@import("builtin").os.tag == .windows) ".\\zig-out\\bin\\roc.exe" else "./zig-out/bin/roc";

/// Result of executing a Roc command during testing.
/// Contains the captured output streams and process termination status.
pub const RocResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

/// Create unique Roc and Zig local cache directories for one CLI test subprocess.
pub fn createIsolatedTestCacheDirs(allocator: std.mem.Allocator) !IsolatedCacheDirs {
    const cache_dir_id = next_cache_dir_id.fetchAdd(1, .monotonic);
    // Get a nanosecond timestamp for uniqueness across runs
    var ts: std.c.timespec = undefined;
    _ = std.c.clock_gettime(.MONOTONIC, &ts);
    const nano_ts: u64 = @intCast(ts.sec * std.time.ns_per_s + ts.nsec);
    const cache_leaf = try std.fmt.allocPrint(allocator, "{d}-{d}", .{
        nano_ts,
        cache_dir_id,
    });
    defer allocator.free(cache_leaf);

    const cwd_path = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
    defer allocator.free(cwd_path);

    const cache_root_rel = try std.fs.path.join(allocator, &.{ ".zig-cache", "roc-test-cache", cache_leaf });
    defer allocator.free(cache_root_rel);

    std.Io.Dir.cwd().createDirPath(io, cache_root_rel) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const roc_cache_rel = try std.fs.path.join(allocator, &.{ cache_root_rel, "roc-cache" });
    defer allocator.free(roc_cache_rel);
    try std.Io.Dir.cwd().createDirPath(io, roc_cache_rel);

    const zig_local_cache_rel = try std.fs.path.join(allocator, &.{ cache_root_rel, "zig-local-cache" });
    defer allocator.free(zig_local_cache_rel);
    try std.Io.Dir.cwd().createDirPath(io, zig_local_cache_rel);

    return .{
        .roc_cache_dir = try std.fs.path.join(allocator, &.{ cwd_path, roc_cache_rel }),
        .zig_local_cache_dir = try std.fs.path.join(allocator, &.{ cwd_path, zig_local_cache_rel }),
    };
}

/// Build an environment map for a test Roc subprocess.
/// Unless the caller already set them, this gives the subprocess unique Roc and
/// Zig local cache roots so concurrent CLI tests cannot share cache state.
pub fn buildIsolatedTestEnvMap(
    allocator: std.mem.Allocator,
    extra_env: ?*const std.process.Environ.Map,
) !std.process.Environ.Map {
    const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
    const environ: std.process.Environ = .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    var env_map = try environ.createMap(allocator);
    errdefer env_map.deinit();

    if (extra_env) |extra| {
        var it = extra.iterator();
        while (it.next()) |entry| {
            try env_map.put(entry.key_ptr.*, entry.value_ptr.*);
        }
    }

    if (env_map.get("ROC_CACHE_DIR") == null or env_map.get("ZIG_LOCAL_CACHE_DIR") == null) {
        const cache_dirs = try createIsolatedTestCacheDirs(allocator);
        defer cache_dirs.deinit(allocator);

        if (env_map.get("ROC_CACHE_DIR") == null) {
            try env_map.put("ROC_CACHE_DIR", cache_dirs.roc_cache_dir);
        }

        if (env_map.get("ZIG_LOCAL_CACHE_DIR") == null) {
            try env_map.put("ZIG_LOCAL_CACHE_DIR", cache_dirs.zig_local_cache_dir);
        }
    }

    return env_map;
}

fn runChild(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    cwd_path: []const u8,
    extra_env: ?*const std.process.Environ.Map,
) !RocResult {
    var env_map = try buildIsolatedTestEnvMap(allocator, extra_env);
    defer env_map.deinit();

    const result = try std.process.run(allocator, io, .{
        .argv = argv,
        .cwd = .{ .path = cwd_path },
        .environ_map = &env_map,
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
    extra_env: ?*const std.process.Environ.Map,
) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
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
    extra_env: ?*const std.process.Environ.Map,
) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
    defer allocator.free(cwd_path);
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer allocator.free(roc_path);

    const test_file = if (std.fs.path.isAbsolute(test_file_path))
        try allocator.dupe(u8, test_file_path)
    else
        try std.fs.path.join(allocator, &.{ cwd_path, test_file_path });
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
    if (std.mem.find(u8, result.stderr, "error(gpa):") != null) {
        std.debug.print("Memory error detected (GPA)\n", .{});
        std.debug.print("STDOUT: {s}\n", .{result.stdout});
        std.debug.print("STDERR: {s}\n", .{result.stderr});
        return error.MemoryError;
    }

    switch (result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{result.stdout});
                std.debug.print("STDERR: {s}\n", .{result.stderr});
                return error.RunFailed;
            }
        },
        .signal => |sig| {
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
        .exited => |code| {
            if (code == 0) {
                std.debug.print("ERROR: roc succeeded but we expected it to fail\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        .signal => |sig| {
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
    if (std.mem.find(u8, result.stderr, "error(gpa):") != null) {
        std.debug.print("Memory error detected (GPA)\n", .{});
        std.debug.print("STDERR: {s}\n", .{result.stderr});
        return error.MemoryError;
    }

    switch (result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("Test failed with exit code {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{result.stderr});
                return error.TestFailed;
            }
        },
        .signal => |sig| {
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
    const cwd_path = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
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
    var env_map = try buildIsolatedTestEnvMap(allocator, null);
    defer env_map.deinit();

    var child = try std.process.spawn(io, .{
        .argv = argv,
        .stdin = .pipe,
        .stdout = .pipe,
        .stderr = .pipe,
        .cwd = .{ .path = cwd_path },
        .environ_map = &env_map,
    });
    defer child.kill(io);

    // Write input to stdin and close it
    child.stdin.?.writeStreamingAll(io, stdin_input) catch {};
    child.stdin.?.close(io);
    child.stdin = null;

    // Collect output using MultiReader
    var multi_reader_buffer: std.Io.File.MultiReader.Buffer(2) = undefined;
    var multi_reader: std.Io.File.MultiReader = undefined;
    multi_reader.init(allocator, io, multi_reader_buffer.toStreams(), &.{ child.stdout.?, child.stderr.? });
    defer multi_reader.deinit();

    while (multi_reader.fill(64, .none)) |_| {} else |err| switch (err) {
        error.EndOfStream => {},
        else => |e| return e,
    }
    try multi_reader.checkAnyError();

    const term = try child.wait(io);

    const stdout = try multi_reader.toOwnedSlice(0);
    errdefer allocator.free(stdout);
    const stderr = try multi_reader.toOwnedSlice(1);
    errdefer allocator.free(stderr);

    return RocResult{
        .stdout = stdout,
        .stderr = stderr,
        .term = term,
    };
}
