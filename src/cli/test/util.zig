//! Utilities for CLI tests using the actual roc binary.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

/// Pick an IO suitable for the build context.
///
/// `std.Options.debug_io` defaults to a Threaded instance whose allocator is `.failing`,
/// so it cannot spawn child processes (the spawn path arena-allocates argv/envp before
/// fork()). In tests we use `std.testing.io`, which is initialized with `testing.allocator`.
const io: std.Io = if (builtin.is_test) std.testing.io else std.Options.debug_io;

fn milliTimestamp() i64 {
    return std.Io.Timestamp.now(io, .awake).toMilliseconds();
}

var next_cache_dir_id: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

/// Default timeout for CLI test child processes that would otherwise run unbounded.
pub const default_child_timeout_ms: u64 = 5 * std.time.ms_per_min;

/// Absolute cache directory paths reserved for a single CLI test subprocess.
pub const IsolatedCacheDirs = struct {
    cache_root_dir: []u8,
    roc_cache_dir: []u8,
    zig_local_cache_dir: []u8,

    pub fn cleanup(self: IsolatedCacheDirs) void {
        std.Io.Dir.cwd().deleteTree(io, self.cache_root_dir) catch {};
    }

    pub fn cleanupAndDeinit(self: IsolatedCacheDirs, allocator: std.mem.Allocator) void {
        self.cleanup();
        self.deinit(allocator);
    }

    pub fn deinit(self: IsolatedCacheDirs, allocator: std.mem.Allocator) void {
        allocator.free(self.zig_local_cache_dir);
        allocator.free(self.roc_cache_dir);
        allocator.free(self.cache_root_dir);
    }
};

/// Process environment map plus any test-owned cache directories it references.
pub const IsolatedTestEnv = struct {
    env_map: std.process.Environ.Map,
    cache_dirs: ?IsolatedCacheDirs,

    pub fn deinit(self: *IsolatedTestEnv, allocator: std.mem.Allocator) void {
        self.env_map.deinit();
        if (self.cache_dirs) |cache_dirs| {
            cache_dirs.cleanupAndDeinit(allocator);
            self.cache_dirs = null;
        }
    }
};

pub const roc_binary_path = if (@import("builtin").os.tag == .windows) ".\\zig-out\\bin\\roc.exe" else "./zig-out/bin/roc";

fn reserveTestCacheRoot(allocator: std.mem.Allocator) anyerror![]u8 {
    const cache_parent_rel = try std.fs.path.join(allocator, &.{ ".zig-cache", "roc-test-cache" });
    defer allocator.free(cache_parent_rel);

    try std.Io.Dir.cwd().createDirPath(io, cache_parent_rel);

    while (true) {
        const cache_dir_id = next_cache_dir_id.fetchAdd(1, .monotonic);
        // Zig 0.16 removed std.time.nanoTimestamp and std.crypto.random. This is
        // test code, so seed a PRNG from the test seed mixed with the per-call
        // monotonic counter to produce a unique-ish temp-dir suffix.
        var prng = std.Random.DefaultPrng.init(@as(u64, std.testing.random_seed) ^ cache_dir_id);
        const random = prng.random().int(u64);
        const cache_leaf = try std.fmt.allocPrint(allocator, "{d}-{x}-{d}", .{
            cache_dir_id,
            random,
            cache_dir_id,
        });
        defer allocator.free(cache_leaf);

        const cache_root_rel = try std.fs.path.join(allocator, &.{ cache_parent_rel, cache_leaf });
        errdefer allocator.free(cache_root_rel);

        std.Io.Dir.cwd().createDir(io, cache_root_rel, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {
                allocator.free(cache_root_rel);
                continue;
            },
            else => return err,
        };

        return cache_root_rel;
    }
}

/// Result of executing a Roc command during testing.
/// Contains the captured output streams and process termination status.
pub const RocResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

/// Options for running a child process from CLI integration tests.
pub const ChildRunOptions = struct {
    cwd: ?[]const u8 = null,
    env_map: ?*const std.process.Environ.Map = null,
    max_output_bytes: usize = 10 * 1024 * 1024,
    stdin: ?[]const u8 = null,
    timeout_ms: u64 = default_child_timeout_ms,
};

fn terminateChildGroup(child_id: std.process.Child.Id) void {
    switch (builtin.os.tag) {
        .windows => {
            const kernel32 = struct {
                extern "kernel32" fn TerminateProcess(hProcess: std.os.windows.HANDLE, uExitCode: c_uint) callconv(.winapi) i32;
            };
            _ = kernel32.TerminateProcess(child_id, 1);
        },
        .wasi => {},
        else => {
            const pid: std.posix.pid_t = child_id;
            std.posix.kill(-pid, std.posix.SIG.KILL) catch {
                std.posix.kill(pid, std.posix.SIG.KILL) catch {};
            };
        },
    }
}

fn appendTimeoutMessage(
    allocator: std.mem.Allocator,
    stderr: *std.ArrayList(u8),
    argv: []const []const u8,
    timeout_ms: u64,
) error{WriteFailed}!void {
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, stderr);
    defer stderr.* = aw.toArrayList();
    try aw.writer.print(
        "\nstuck: child command timed out after {d}ms:",
        .{timeout_ms},
    );
    for (argv) |arg| {
        try aw.writer.print(" {s}", .{arg});
    }
    try aw.writer.writeAll("\n");
}

/// Run a child process with captured output and a watchdog timeout.
///
/// Uses the module-level `io` (the threaded IO usable for spawning child
/// processes in tests). Preserves the process-group kill semantics from the
/// original implementation so timed-out children take their descendants with
/// them.
pub fn runChildWithTimeout(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    options: ChildRunOptions,
) anyerror!std.process.RunResult {
    const Watch = struct {
        child_id: std.process.Child.Id,
        timeout_ms: u64,
        timed_out: std.atomic.Value(bool),
        done: std.atomic.Value(bool),

        fn run(self: *@This()) void {
            if (self.timeout_ms == 0) return;
            const start_ms = milliTimestamp();
            while (!self.done.load(.acquire)) {
                std.Io.sleep(io, std.Io.Duration.fromMilliseconds(100), .awake) catch {};
                if (self.done.load(.acquire)) return;

                const elapsed_ms: u64 = @intCast(@max(0, milliTimestamp() - start_ms));
                if (elapsed_ms >= self.timeout_ms) {
                    self.timed_out.store(true, .release);
                    terminateChildGroup(self.child_id);
                    return;
                }
            }
        }
    };

    var child = try std.process.spawn(io, .{
        .argv = argv,
        .stdin = if (options.stdin == null) .ignore else .pipe,
        .stdout = .pipe,
        .stderr = .pipe,
        .cwd = if (options.cwd) |path| .{ .path = path } else .inherit,
        .environ_map = options.env_map,
        // Put the child in its own process group so the watchdog can signal
        // the whole group (child + any grandchildren) on timeout.
        .pgid = switch (builtin.os.tag) {
            .windows, .wasi => null,
            else => 0,
        },
    });
    errdefer child.kill(io);

    // The watchdog signals the child's process group; it needs the child id.
    const child_pid: ?std.process.Child.Id = child.id;

    var watch = Watch{
        .child_id = child_pid orelse undefined,
        .timeout_ms = if (child_pid == null) 0 else options.timeout_ms,
        .timed_out = std.atomic.Value(bool).init(false),
        .done = std.atomic.Value(bool).init(false),
    };
    const watch_thread = if (watch.timeout_ms == 0)
        null
    else
        try std.Thread.spawn(.{}, Watch.run, .{&watch});
    defer {
        watch.done.store(true, .release);
        if (watch_thread) |thread| thread.join();
    }

    if (options.stdin) |stdin_input| {
        child.stdin.?.writeStreamingAll(io, stdin_input) catch {};
        child.stdin.?.close(io);
        child.stdin = null;
    }

    var multi_reader_buffer: std.Io.File.MultiReader.Buffer(2) = undefined;
    var multi_reader: std.Io.File.MultiReader = undefined;
    multi_reader.init(allocator, io, multi_reader_buffer.toStreams(), &.{ child.stdout.?, child.stderr.? });
    defer multi_reader.deinit();

    const stdout_reader = multi_reader.reader(0);
    const stderr_reader = multi_reader.reader(1);

    while (multi_reader.fill(64, .none)) |_| {
        if (stdout_reader.buffered().len > options.max_output_bytes) return error.StreamTooLong;
        if (stderr_reader.buffered().len > options.max_output_bytes) return error.StreamTooLong;
    } else |err| switch (err) {
        error.EndOfStream => {},
        else => |e| return e,
    }
    try multi_reader.checkAnyError();

    const term = try child.wait(io);

    var stdout: std.ArrayList(u8) = .fromOwnedSlice(try multi_reader.toOwnedSlice(0));
    errdefer stdout.deinit(allocator);
    var stderr: std.ArrayList(u8) = .fromOwnedSlice(try multi_reader.toOwnedSlice(1));
    errdefer stderr.deinit(allocator);

    if (watch.timed_out.load(.acquire)) {
        try appendTimeoutMessage(allocator, &stderr, argv, options.timeout_ms);
    }

    return .{
        .stdout = try stdout.toOwnedSlice(allocator),
        .stderr = try stderr.toOwnedSlice(allocator),
        .term = term,
    };
}

/// Create unique Roc and Zig local cache directories for one CLI test subprocess.
pub fn createIsolatedTestCacheDirs(allocator: std.mem.Allocator) anyerror!IsolatedCacheDirs {
    const cwd_path = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
    defer allocator.free(cwd_path);

    const cache_root_rel = try reserveTestCacheRoot(allocator);
    defer allocator.free(cache_root_rel);

    const roc_cache_rel = try std.fs.path.join(allocator, &.{ cache_root_rel, "roc-cache" });
    defer allocator.free(roc_cache_rel);
    try std.Io.Dir.cwd().createDirPath(io, roc_cache_rel);

    const zig_local_cache_rel = try std.fs.path.join(allocator, &.{ cache_root_rel, "zig-local-cache" });
    defer allocator.free(zig_local_cache_rel);
    try std.Io.Dir.cwd().createDirPath(io, zig_local_cache_rel);

    return .{
        .cache_root_dir = try std.fs.path.join(allocator, &.{ cwd_path, cache_root_rel }),
        .roc_cache_dir = try std.fs.path.join(allocator, &.{ cwd_path, roc_cache_rel }),
        .zig_local_cache_dir = try std.fs.path.join(allocator, &.{ cwd_path, zig_local_cache_rel }),
    };
}

/// Copy the current process environment and overlay any caller-provided entries.
pub fn buildProcessEnvMap(
    allocator: std.mem.Allocator,
    extra_env: ?*const std.process.Environ.Map,
) anyerror!std.process.Environ.Map {
    // In Zig 0.16, Environ.Block is GlobalBlock on Windows (read from PEB on use) and
    // PosixBlock on POSIX (must point at std.c.environ).
    const environ: std.process.Environ = if (builtin.os.tag == .windows) .{
        .block = .global,
    } else blk: {
        const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
        break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    };
    var env_map = try environ.createMap(allocator);
    errdefer env_map.deinit();

    if (extra_env) |extra| {
        var it = extra.iterator();
        while (it.next()) |entry| {
            try env_map.put(entry.key_ptr.*, entry.value_ptr.*);
        }
    }

    return env_map;
}

/// Build an environment map for a test Roc subprocess.
/// Unless the caller already set them, this gives the subprocess unique Roc,
/// URL package, and Zig local cache roots so concurrent CLI tests cannot share cache state.
pub fn buildIsolatedTestEnv(
    allocator: std.mem.Allocator,
    extra_env: ?*const std.process.Environ.Map,
) anyerror!IsolatedTestEnv {
    var env_map = try buildProcessEnvMap(allocator, extra_env);
    errdefer env_map.deinit();
    var cache_dirs: ?IsolatedCacheDirs = null;
    errdefer if (cache_dirs) |dirs| {
        dirs.cleanupAndDeinit(allocator);
    };

    if (env_map.get("ROC_CACHE_DIR") == null or
        env_map.get("XDG_CACHE_HOME") == null or
        env_map.get("ZIG_LOCAL_CACHE_DIR") == null)
    {
        const dirs = try createIsolatedTestCacheDirs(allocator);
        cache_dirs = dirs;

        if (env_map.get("ROC_CACHE_DIR") == null) {
            try env_map.put("ROC_CACHE_DIR", dirs.roc_cache_dir);
        }

        if (env_map.get("XDG_CACHE_HOME") == null) {
            try env_map.put("XDG_CACHE_HOME", dirs.roc_cache_dir);
        }

        if (env_map.get("ZIG_LOCAL_CACHE_DIR") == null) {
            try env_map.put("ZIG_LOCAL_CACHE_DIR", dirs.zig_local_cache_dir);
        }
    }

    return .{
        .env_map = env_map,
        .cache_dirs = cache_dirs,
    };
}

fn runChild(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    cwd_path: []const u8,
    extra_env: ?*const std.process.Environ.Map,
) anyerror!RocResult {
    var env = try buildIsolatedTestEnv(allocator, extra_env);
    defer env.deinit(allocator);

    const result = try runChildWithTimeout(allocator, argv, .{
        .cwd = cwd_path,
        .env_map = &env.env_map,
        .max_output_bytes = 10 * 1024 * 1024, // 10MB
    });

    return RocResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .term = result.term,
    };
}

/// Helper to run roc with arguments that don't require a test file
pub fn runRocCommand(allocator: std.mem.Allocator, args: []const []const u8) anyerror!RocResult {
    return runRocCommandWithEnv(allocator, args, null);
}

/// Run a roc CLI command with optional extra environment variables.
pub fn runRocCommandWithEnv(
    allocator: std.mem.Allocator,
    args: []const []const u8,
    extra_env: ?*const std.process.Environ.Map,
) anyerror!RocResult {
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
pub fn runRoc(allocator: std.mem.Allocator, args: []const []const u8, test_file_path: []const u8) anyerror!RocResult {
    return runRocWithEnv(allocator, args, test_file_path, null);
}

/// Run roc on a test file with optional extra environment variables.
pub fn runRocWithEnv(
    allocator: std.mem.Allocator,
    args: []const []const u8,
    test_file_path: []const u8,
    extra_env: ?*const std.process.Environ.Map,
) anyerror!RocResult {
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
pub fn runRocTest(allocator: std.mem.Allocator, roc_file: []const u8, spec: []const u8) anyerror!RocResult {
    return runRocCommand(allocator, &.{ roc_file, "--", "--test", spec });
}

/// Check if a run result indicates success (exit code 0).
/// Also checks for GPA memory errors in stderr.
pub fn checkSuccess(result: RocResult) anyerror!void {
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
pub fn checkFailure(result: RocResult) anyerror!void {
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
pub fn checkTestSuccess(result: RocResult) anyerror!void {
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
pub fn runRocWithStdin(allocator: std.mem.Allocator, args: []const []const u8, stdin_input: []const u8) anyerror!RocResult {
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

    var env = try buildIsolatedTestEnv(allocator, null);
    defer env.deinit(allocator);
    const result = try runChildWithTimeout(allocator, argv, .{
        .cwd = cwd_path,
        .env_map = &env.env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .stdin = stdin_input,
    });

    return RocResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .term = result.term,
    };
}
