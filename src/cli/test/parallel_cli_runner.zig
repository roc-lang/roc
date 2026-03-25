//! Parallel CLI test runner for Roc platform integration tests.
//!
//! Replaces the 5 sequential test_runner invocations in `zig build test-cli`
//! with a single binary that runs all platform tests in parallel using a
//! fork-based process pool (modeled after src/eval/test/parallel_runner.zig).
//!
//! Usage:
//!   parallel_cli_runner <roc_binary> [options]
//!
//! Options:
//!   --filter <pattern>   Run only tests whose name contains <pattern>
//!   --threads <N>        Max concurrent child processes (default: CPU count)
//!   --timeout <ms>       Per-test timeout in ms (default: 60000)
//!   --verbose            Print PASS results and timing details

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;
const Allocator = std.mem.Allocator;

const platform_config = @import("platform_config.zig");
const fx_test_specs = @import("fx_test_specs.zig");

const Timer = std.time.Timer;
const has_fork = (builtin.os.tag != .windows);

// ---------------------------------------------------------------------------
// Test spec types
// ---------------------------------------------------------------------------

/// A single CLI test operation — one atomic unit of work.
const CliTestSpec = struct {
    /// Human-readable name, e.g. "fx/hello_world.roc [dev]"
    name: []const u8,
    /// Path to .roc file (relative to project root)
    roc_file: []const u8,
    /// Platform name (for display grouping)
    platform: []const u8,
    /// Backend: null = interpreter, "dev" = dev backend
    backend: ?[]const u8,
    /// What kind of test to run
    test_kind: TestKind,

    const TestKind = union(enum) {
        /// Build natively and run; check exit code 0
        native_run,
        /// Build natively, run with --test <spec>; check exit code 0
        io_spec: []const u8,
    };
};

/// Which platform/backend combos to test (mirrors build.zig's 5 invocations).
const RunConfig = struct {
    platform_name: []const u8,
    backend: ?[]const u8,
};

const run_configs = [_]RunConfig{
    .{ .platform_name = "int", .backend = null },
    .{ .platform_name = "str", .backend = null },
    .{ .platform_name = "int", .backend = "dev" },
    .{ .platform_name = "str", .backend = "dev" },
    .{ .platform_name = "fx", .backend = "dev" },
};

// ---------------------------------------------------------------------------
// Spec generation
// ---------------------------------------------------------------------------

fn buildTestSpecs(allocator: Allocator, filters: []const []const u8) ![]const CliTestSpec {
    var specs: std.ArrayListUnmanaged(CliTestSpec) = .empty;

    for (&run_configs) |cfg| {
        const platform = platform_config.findPlatform(cfg.platform_name) orelse continue;

        switch (platform.test_apps) {
            .single => |app_name| {
                const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
                const name = try fmtTestName(allocator, roc_file, cfg.backend);
                if (matchesFilters(name, roc_file, filters)) {
                    try specs.append(allocator, .{
                        .name = name,
                        .roc_file = roc_file,
                        .platform = platform.name,
                        .backend = cfg.backend,
                        .test_kind = .native_run,
                    });
                }
            },
            .spec_list => |io_specs| {
                for (io_specs) |spec| {
                    const name = try fmtTestName(allocator, spec.roc_file, cfg.backend);
                    if (matchesFilters(name, spec.roc_file, filters)) {
                        try specs.append(allocator, .{
                            .name = name,
                            .roc_file = spec.roc_file,
                            .platform = platform.name,
                            .backend = cfg.backend,
                            .test_kind = .{ .io_spec = spec.io_spec },
                        });
                    }
                }
            },
            .simple_list => |simple_specs| {
                for (simple_specs) |spec| {
                    const name = try fmtTestName(allocator, spec.roc_file, cfg.backend);
                    if (matchesFilters(name, spec.roc_file, filters)) {
                        try specs.append(allocator, .{
                            .name = name,
                            .roc_file = spec.roc_file,
                            .platform = platform.name,
                            .backend = cfg.backend,
                            .test_kind = .native_run,
                        });
                    }
                }
            },
        }
    }

    return specs.toOwnedSlice(allocator);
}

fn fmtTestName(allocator: Allocator, roc_file: []const u8, backend: ?[]const u8) ![]const u8 {
    if (backend) |b| {
        return std.fmt.allocPrint(allocator, "{s} [{s}]", .{ roc_file, b });
    }
    return std.fmt.allocPrint(allocator, "{s}", .{roc_file});
}

/// Check if a test matches any of the given filters. Matches against both
/// the formatted name (e.g. "test/fx/hello_world.roc [dev]") and the raw
/// roc_file path (e.g. "test/fx/hello_world.roc"), so filters from
/// roc_subcommands_test naming also work here.
fn matchesFilters(name: []const u8, roc_file: []const u8, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |f| {
        if (std.mem.indexOf(u8, name, f) != null) return true;
        if (std.mem.indexOf(u8, roc_file, f) != null) return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// Wire protocol (child → parent via pipe)
// ---------------------------------------------------------------------------

const TestStatus = enum(u8) {
    pass = 0,
    fail = 1,
    skip = 2,
    timeout = 3,
    crash = 4,
};

/// Fixed-size binary header. Native byte order (same machine).
const WireHeader = extern struct {
    status: u8,
    duration_ns: u64,
    exit_code: u32,
    stderr_len: u32,
    stdout_len: u32,
    message_len: u32,
};

const TestResult = struct {
    status: TestStatus,
    duration_ns: u64 = 0,
    exit_code: u32 = 0,
    stderr_capture: ?[]const u8 = null,
    stdout_capture: ?[]const u8 = null,
    message: ?[]const u8 = null,
};

fn serializeResult(fd: posix.fd_t, result: TestResult) void {
    const stderr_data = result.stderr_capture orelse "";
    const stdout_data = result.stdout_capture orelse "";
    const message_data = result.message orelse "";

    // Truncate to avoid pipe buffer issues
    const max_capture = 8192;
    const stderr_out = stderr_data[0..@min(stderr_data.len, max_capture)];
    const stdout_out = stdout_data[0..@min(stdout_data.len, max_capture)];
    const message_out = message_data[0..@min(message_data.len, max_capture)];

    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .duration_ns = result.duration_ns,
        .exit_code = result.exit_code,
        .stderr_len = @intCast(stderr_out.len),
        .stdout_len = @intCast(stdout_out.len),
        .message_len = @intCast(message_out.len),
    };

    writeAll(fd, std.mem.asBytes(&header));
    writeAll(fd, stderr_out);
    writeAll(fd, stdout_out);
    writeAll(fd, message_out);
}

fn deserializeResult(buf: []const u8, gpa: Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);

    const stderr_capture = readStr(buf, &offset, header.stderr_len, gpa);
    const stdout_capture = readStr(buf, &offset, header.stdout_len, gpa);
    const message = readStr(buf, &offset, header.message_len, gpa);

    return .{
        .status = @enumFromInt(header.status),
        .duration_ns = header.duration_ns,
        .exit_code = header.exit_code,
        .stderr_capture = stderr_capture,
        .stdout_capture = stdout_capture,
        .message = message,
    };
}

fn readStr(buf: []const u8, offset: *usize, len: u32, gpa: Allocator) ?[]const u8 {
    if (len == 0) return null;
    const end = offset.* + len;
    if (end > buf.len) return null;
    const slice = buf[offset.*..end];
    offset.* = end;
    return gpa.dupe(u8, slice) catch null;
}

fn writeAll(fd: posix.fd_t, data: []const u8) void {
    var written: usize = 0;
    while (written < data.len) {
        written += posix.write(fd, data[written..]) catch return;
    }
}

// ---------------------------------------------------------------------------
// Child test execution
// ---------------------------------------------------------------------------

var next_cache_id: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

fn createIsolatedCacheDir(allocator: Allocator) ![]u8 {
    const cache_id = next_cache_id.fetchAdd(1, .monotonic);
    const cache_leaf = try std.fmt.allocPrint(allocator, "{d}-{d}", .{
        @as(u64, @intCast(std.time.nanoTimestamp())),
        cache_id,
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

fn removeCacheDir(allocator: Allocator, cache_dir: []const u8) void {
    // Extract the relative part after cwd for cleanup
    _ = allocator;
    std.fs.cwd().deleteTree(cache_dir) catch {};
}

/// Run a single CLI test. Called in the forked child process.
fn runSingleTest(allocator: Allocator, spec: CliTestSpec, roc_binary: []const u8) TestResult {
    var timer = Timer.start() catch return .{ .status = .crash, .message = "no clock" };

    // Create isolated cache directory
    const cache_dir = createIsolatedCacheDir(allocator) catch
        return .{ .status = .crash, .message = "failed to create cache dir" };
    defer removeCacheDir(allocator, cache_dir);

    // Unique output name based on pid to avoid collisions.
    // Needs ./ prefix so it's found as executable on Linux.
    const pid = std.c.getpid();
    const output_name = std.fmt.allocPrint(allocator, "./.test_output_{d}", .{pid}) catch
        return .{ .status = .crash, .message = "OOM" };
    defer {
        std.fs.cwd().deleteFile(output_name) catch {};
    }

    // Build env with isolated cache
    var env_map = std.process.getEnvMap(allocator) catch
        return .{ .status = .crash, .message = "failed to get env" };
    defer env_map.deinit();
    env_map.put("ROC_CACHE_DIR", cache_dir) catch
        return .{ .status = .crash, .message = "failed to set env" };

    // Step 1: Build
    const output_arg = std.fmt.allocPrint(allocator, "--output={s}", .{output_name}) catch
        return .{ .status = .crash, .message = "OOM" };

    var build_argv_buf: [5][]const u8 = undefined;
    var argc: usize = 0;
    build_argv_buf[argc] = roc_binary;
    argc += 1;
    build_argv_buf[argc] = "build";
    argc += 1;
    build_argv_buf[argc] = output_arg;
    argc += 1;
    if (spec.backend) |b| {
        const backend_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{b}) catch
            return .{ .status = .crash, .message = "OOM" };
        build_argv_buf[argc] = backend_arg;
        argc += 1;
    }
    build_argv_buf[argc] = spec.roc_file;
    argc += 1;

    const build_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = build_argv_buf[0..argc],
        .env_map = &env_map,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "build spawn error: {}", .{err}) catch "build spawn error";
        return .{ .status = .fail, .duration_ns = timer.read(), .message = msg };
    };

    if (!isSuccess(build_result.term)) {
        return .{
            .status = .fail,
            .duration_ns = timer.read(),
            .exit_code = exitCode(build_result.term),
            .stderr_capture = build_result.stderr,
            .stdout_capture = build_result.stdout,
            .message = "build failed",
        };
    }
    allocator.free(build_result.stdout);
    allocator.free(build_result.stderr);

    // Verify binary was created
    std.fs.cwd().access(output_name, .{}) catch {
        return .{
            .status = .fail,
            .duration_ns = timer.read(),
            .message = "build succeeded but binary not created",
        };
    };

    // Step 2: Run
    switch (spec.test_kind) {
        .native_run => {
            const run_result = std.process.Child.run(.{
                .allocator = allocator,
                .argv = &[_][]const u8{output_name},
            }) catch |err| {
                const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
                return .{ .status = .fail, .duration_ns = timer.read(), .message = msg };
            };

            if (hasMemoryErrors(run_result.stderr)) |mem_msg| {
                return .{
                    .status = .fail,
                    .duration_ns = timer.read(),
                    .exit_code = exitCode(run_result.term),
                    .stderr_capture = run_result.stderr,
                    .stdout_capture = run_result.stdout,
                    .message = mem_msg,
                };
            }

            if (!isSuccess(run_result.term)) {
                return .{
                    .status = .fail,
                    .duration_ns = timer.read(),
                    .exit_code = exitCode(run_result.term),
                    .stderr_capture = run_result.stderr,
                    .stdout_capture = run_result.stdout,
                    .message = "run failed",
                };
            }

            allocator.free(run_result.stdout);
            allocator.free(run_result.stderr);
        },
        .io_spec => |io_spec| {
            const run_result = std.process.Child.run(.{
                .allocator = allocator,
                .argv = &[_][]const u8{ output_name, "--test", io_spec },
            }) catch |err| {
                const msg = std.fmt.allocPrint(allocator, "io_spec run spawn error: {}", .{err}) catch "run spawn error";
                return .{ .status = .fail, .duration_ns = timer.read(), .message = msg };
            };

            if (hasMemoryErrors(run_result.stderr)) |mem_msg| {
                return .{
                    .status = .fail,
                    .duration_ns = timer.read(),
                    .exit_code = exitCode(run_result.term),
                    .stderr_capture = run_result.stderr,
                    .stdout_capture = run_result.stdout,
                    .message = mem_msg,
                };
            }

            if (!isSuccess(run_result.term)) {
                return .{
                    .status = .fail,
                    .duration_ns = timer.read(),
                    .exit_code = exitCode(run_result.term),
                    .stderr_capture = run_result.stderr,
                    .stdout_capture = run_result.stdout,
                    .message = "io_spec test failed",
                };
            }

            allocator.free(run_result.stdout);
            allocator.free(run_result.stderr);
        },
    }

    return .{
        .status = .pass,
        .duration_ns = timer.read(),
    };
}

fn isSuccess(term: std.process.Child.Term) bool {
    return switch (term) {
        .Exited => |code| code == 0,
        else => false,
    };
}

fn exitCode(term: std.process.Child.Term) u32 {
    return switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => |sig| @as(u32, sig) | 0x80000000,
        else => 0xFFFFFFFF,
    };
}

fn hasMemoryErrors(stderr: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, stderr, "error(gpa):") != null)
        return "memory error detected";
    if (std.mem.indexOf(u8, stderr, "allocation(s) not freed") != null)
        return "memory leak detected";
    return null;
}

// ---------------------------------------------------------------------------
// Process pool
// ---------------------------------------------------------------------------

const ChildSlot = struct {
    pid: posix.pid_t,
    pipe_fd: posix.fd_t,
    test_index: usize,
    start_time_ms: i64,
    buf: std.ArrayListUnmanaged(u8),
    timed_out: bool,
};

var global_slots: ?[]?ChildSlot = null;

fn sigintHandler(_: c_int) callconv(.c) void {
    const slots = global_slots orelse return;
    for (slots) |slot_opt| {
        if (slot_opt) |slot| {
            // Kill entire process group (child + its subprocesses)
            posix.kill(-slot.pid, posix.SIG.KILL) catch {};
        }
    }
    const default_action = posix.Sigaction{
        .handler = .{ .handler = posix.SIG.DFL },
        .mask = posix.sigemptyset(),
        .flags = 0,
    };
    posix.sigaction(posix.SIG.INT, &default_action, null);
    _ = std.c.raise(posix.SIG.INT);
}

fn launchChild(
    slot: *?ChildSlot,
    tests: []const CliTestSpec,
    test_idx: usize,
    roc_binary: []const u8,
) bool {
    if (comptime !has_fork) return false;

    const pipe_fds = posix.pipe() catch return false;

    const pid = posix.fork() catch {
        posix.close(pipe_fds[0]);
        posix.close(pipe_fds[1]);
        return false;
    };

    if (pid == 0) {
        // === Child process ===
        posix.close(pipe_fds[0]);

        // Create new process group so timeout kills clean up subprocesses
        _ = std.c.setsid();

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const allocator = arena.allocator();

        const result = runSingleTest(allocator, tests[test_idx], roc_binary);
        serializeResult(pipe_fds[1], result);
        posix.close(pipe_fds[1]);
        std.c._exit(0);
    }

    // === Parent ===
    posix.close(pipe_fds[1]);
    slot.* = .{
        .pid = pid,
        .pipe_fd = pipe_fds[0],
        .test_index = test_idx,
        .start_time_ms = std.time.milliTimestamp(),
        .buf = .empty,
        .timed_out = false,
    };
    return true;
}

fn reapChild(slot: *?ChildSlot, results: []TestResult, gpa: Allocator) void {
    var s = slot.* orelse return;
    slot.* = null;

    drainPipe(s.pipe_fd, &s.buf);
    posix.close(s.pipe_fd);

    const wait_result = posix.waitpid(s.pid, 0);
    const term_signal: u8 = @truncate(wait_result.status & 0x7f);

    if (s.timed_out or term_signal == 9) {
        results[s.test_index] = .{ .status = .timeout };
    } else if (term_signal != 0) {
        results[s.test_index] = .{ .status = .crash };
    } else {
        results[s.test_index] = deserializeResult(s.buf.items, gpa) orelse
            .{ .status = .crash };
    }

    s.buf.deinit(std.heap.page_allocator);
}

fn drainPipe(fd: posix.fd_t, buf: *std.ArrayListUnmanaged(u8)) void {
    var read_buf: [4096]u8 = undefined;
    while (true) {
        const n = posix.read(fd, &read_buf) catch break;
        if (n == 0) break;
        buf.appendSlice(std.heap.page_allocator, read_buf[0..n]) catch break;
    }
}

fn processPoolMain(
    tests: []const CliTestSpec,
    results: []TestResult,
    max_children: usize,
    timeout_ms: u64,
    gpa: Allocator,
    roc_binary: []const u8,
) void {
    if (comptime !has_fork) {
        runTestsSequential(tests, results, gpa, roc_binary);
        return;
    }

    const slots = gpa.alloc(?ChildSlot, max_children) catch {
        std.debug.print("fatal: failed to allocate process pool slots\n", .{});
        return;
    };
    defer gpa.free(slots);
    @memset(slots, null);

    // Install SIGINT handler
    global_slots = slots;
    defer global_slots = null;
    const sa = posix.Sigaction{
        .handler = .{ .handler = &sigintHandler },
        .mask = posix.sigemptyset(),
        .flags = 0,
    };
    posix.sigaction(posix.SIG.INT, &sa, null);

    const poll_fds = gpa.alloc(posix.pollfd, max_children) catch return;
    defer gpa.free(poll_fds);
    const poll_map = gpa.alloc(usize, max_children) catch return;
    defer gpa.free(poll_map);

    const is_tty = posix.isatty(2);

    var next_test: usize = 0;
    var completed: usize = 0;
    var progress_timer = Timer.start() catch unreachable;
    var last_progress_ns: u64 = 0;

    // Fill initial slots
    for (slots) |*slot| {
        if (next_test >= tests.len) break;
        if (!launchChild(slot, tests, next_test, roc_binary)) {
            results[next_test] = .{ .status = .crash };
            completed += 1;
        }
        next_test += 1;
    }

    // Main event loop
    while (completed < tests.len) {
        var n_poll: usize = 0;
        for (slots, 0..) |slot, i| {
            if (slot != null) {
                poll_fds[n_poll] = .{
                    .fd = slot.?.pipe_fd,
                    .events = posix.POLL.IN | posix.POLL.HUP,
                    .revents = 0,
                };
                poll_map[n_poll] = i;
                n_poll += 1;
            }
        }
        if (n_poll == 0) break;

        _ = posix.poll(poll_fds[0..n_poll], 500) catch 0;

        for (poll_fds[0..n_poll], 0..) |pfd, pi| {
            const slot_idx = poll_map[pi];
            if (pfd.revents & posix.POLL.IN != 0) {
                var read_buf: [4096]u8 = undefined;
                const n = posix.read(pfd.fd, &read_buf) catch 0;
                if (n > 0) {
                    if (slots[slot_idx]) |*s| {
                        s.buf.appendSlice(std.heap.page_allocator, read_buf[0..n]) catch {};
                    }
                }
            }
            if (pfd.revents & (posix.POLL.HUP | posix.POLL.ERR | posix.POLL.NVAL) != 0) {
                reapChild(&slots[slot_idx], results, gpa);
                completed += 1;

                if (next_test < tests.len) {
                    if (!launchChild(&slots[slot_idx], tests, next_test, roc_binary)) {
                        results[next_test] = .{ .status = .crash };
                        completed += 1;
                    }
                    next_test += 1;
                }
            }
        }

        // Check timeouts
        if (timeout_ms > 0) {
            const now = std.time.milliTimestamp();
            for (slots) |*slot_opt| {
                if (slot_opt.*) |*slot| {
                    const elapsed: u64 = @intCast(@max(0, now - slot.start_time_ms));
                    if (elapsed > timeout_ms) {
                        slot.timed_out = true;
                        const test_name = if (slot.test_index < tests.len) tests[slot.test_index].name else "?";
                        std.debug.print("\n  HANG  {s}  ({d}ms) — killing\n", .{ test_name, elapsed });
                        // Kill entire process group
                        posix.kill(-slot.pid, posix.SIG.KILL) catch {};
                    }
                }
            }
        }

        // Progress line every ~1s (only on tty to avoid polluting CI logs)
        const progress_elapsed = progress_timer.read();
        if (progress_elapsed - last_progress_ns >= 1_000_000_000) {
            last_progress_ns = progress_elapsed;
            const wall_s = @as(f64, @floatFromInt(progress_elapsed)) / 1_000_000_000.0;
            if (is_tty) {
                std.debug.print("\r  progress: {d}/{d} done, {d:.1}s elapsed", .{
                    completed, tests.len, wall_s,
                });
            }
        }
    }

    if (is_tty) {
        // Clear progress line
        std.debug.print("\r{s}\r", .{" " ** 72});
    }
}

/// Sequential fallback for platforms without fork (Windows).
fn runTestsSequential(
    tests: []const CliTestSpec,
    results: []TestResult,
    _: Allocator,
    roc_binary: []const u8,
) void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    for (tests, 0..) |spec, i| {
        _ = arena.reset(.retain_capacity);
        results[i] = runSingleTest(arena.allocator(), spec, roc_binary);
    }
}

// ---------------------------------------------------------------------------
// Statistics
// ---------------------------------------------------------------------------

const TimingStats = struct {
    min: u64,
    max: u64,
    mean: u64,
    median: u64,
    std_dev: u64,
    p95: u64,
    total: u64,
    count: usize,
};

fn computeTimingStats(values: []u64) ?TimingStats {
    if (values.len == 0) return null;

    std.mem.sort(u64, values, {}, struct {
        fn lessThan(_: void, a: u64, b: u64) bool {
            return a < b;
        }
    }.lessThan);

    var total: u128 = 0;
    for (values) |v| total += v;

    const mean: u64 = @intCast(total / values.len);
    const median = values[values.len / 2];
    const p95_idx = @min(values.len - 1, (values.len * 95 + 99) / 100);
    const p95 = values[p95_idx];

    var sum_sq_diff: f64 = 0;
    for (values) |v| {
        const diff = @as(f64, @floatFromInt(v)) - @as(f64, @floatFromInt(mean));
        sum_sq_diff += diff * diff;
    }
    const variance = sum_sq_diff / @as(f64, @floatFromInt(values.len));
    const std_dev: u64 = @intFromFloat(@sqrt(variance));

    return .{
        .min = values[0],
        .max = values[values.len - 1],
        .mean = mean,
        .median = median,
        .std_dev = std_dev,
        .p95 = p95,
        .total = @intCast(@min(total, std.math.maxInt(u64))),
        .count = values.len,
    };
}

fn nsToMs(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000.0;
}

// ---------------------------------------------------------------------------
// Output
// ---------------------------------------------------------------------------

fn printResults(
    tests: []const CliTestSpec,
    results: []const TestResult,
    verbose: bool,
    gpa: Allocator,
    wall_ns: u64,
    max_children: usize,
) void {
    var passed: usize = 0;
    var failed: usize = 0;
    var crashed: usize = 0;
    var skipped: usize = 0;
    var timed_out: usize = 0;

    // Print failures/crashes/timeouts (always), passes (verbose only)
    for (tests, 0..) |tc, i| {
        const r = results[i];
        const ms = nsToMs(r.duration_ns);

        switch (r.status) {
            .pass => {
                passed += 1;
                if (verbose) {
                    std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                }
            },
            .fail => {
                failed += 1;
                std.debug.print("  FAIL  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                if (r.exit_code != 0) {
                    if (r.exit_code & 0x80000000 != 0) {
                        std.debug.print("        signal {d}\n", .{r.exit_code & 0x7FFFFFFF});
                    } else {
                        std.debug.print("        exit code {d}\n", .{r.exit_code});
                    }
                }
                printCapturedOutput("stderr", r.stderr_capture);
                printCapturedOutput("stdout", r.stdout_capture);
                printRepro(tc.name);
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                printCapturedOutput("stderr", r.stderr_capture);
                printRepro(tc.name);
            },
            .timeout => {
                timed_out += 1;
                std.debug.print("  HANG  {s}\n", .{tc.name});
                printRepro(tc.name);
            },
            .skip => {
                skipped += 1;
                if (verbose) {
                    std.debug.print("  SKIP  {s}\n", .{tc.name});
                }
            },
        }
    }

    // Summary line
    const wall_ms = nsToMs(wall_ns);
    std.debug.print("\n{d} passed, {d} failed", .{ passed, failed });
    if (crashed > 0) std.debug.print(", {d} crashed", .{crashed});
    if (timed_out > 0) std.debug.print(", {d} hung", .{timed_out});
    if (skipped > 0) std.debug.print(", {d} skipped", .{skipped});
    std.debug.print(" ({d} total) in {d:.0}ms using {d} worker(s)\n", .{
        tests.len, wall_ms, max_children,
    });

    // Timing summary
    printTimingSummary(gpa, tests, results);
}

fn printCapturedOutput(label: []const u8, capture: ?[]const u8) void {
    const data = capture orelse return;
    if (data.len == 0) return;

    var lines = std.mem.splitScalar(u8, data, '\n');
    var line_count: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (line_count == 0) {
            std.debug.print("        {s}: {s}\n", .{ label, line });
        } else if (line_count < 5) {
            std.debug.print("        {s}\n", .{line});
        } else {
            std.debug.print("        ... ({s} truncated)\n", .{label});
            break;
        }
        line_count += 1;
    }
}

fn printRepro(test_name: []const u8) void {
    std.debug.print("        Repro: zig build test-cli -- --test-filter \"{s}\"\n\n", .{test_name});
}

fn printTimingSummary(gpa: Allocator, tests: []const CliTestSpec, results: []const TestResult) void {
    // Collect timing values for all tests that ran
    var durations: std.ArrayListUnmanaged(u64) = .empty;
    defer durations.deinit(gpa);
    for (results) |r| {
        if (r.duration_ns > 0) {
            durations.append(gpa, r.duration_ns) catch continue;
        }
    }

    if (computeTimingStats(durations.items)) |s| {
        std.debug.print("\n=== Timing Summary (ms) ===\n", .{});
        std.debug.print("  {s:<8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8}   {s:>3}\n", .{
            "Phase", "Min", "Max", "Mean", "Median", "StdDev", "P95", "Total", "N",
        });
        std.debug.print("  {s:-<8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8}   {s:->3}\n", .{
            "", "", "", "", "", "", "", "", "",
        });
        std.debug.print("  {s:<8} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1}   {d:>3}\n", .{
            "total",
            nsToMs(s.min),
            nsToMs(s.max),
            nsToMs(s.mean),
            nsToMs(s.median),
            nsToMs(s.std_dev),
            nsToMs(s.p95),
            nsToMs(s.total),
            s.count,
        });
    }

    // Slowest 5 tests
    const TopEntry = struct {
        idx: usize,
        duration_ns: u64,
    };
    var top_buf: std.ArrayListUnmanaged(TopEntry) = .empty;
    defer top_buf.deinit(gpa);
    for (results, 0..) |r, i| {
        if (r.duration_ns > 0) {
            top_buf.append(gpa, .{ .idx = i, .duration_ns = r.duration_ns }) catch continue;
        }
    }
    std.mem.sort(TopEntry, top_buf.items, {}, struct {
        fn lessThan(_: void, a: TopEntry, b: TopEntry) bool {
            return a.duration_ns > b.duration_ns; // descending
        }
    }.lessThan);

    const show_count = @min(5, top_buf.items.len);
    if (show_count > 0) {
        std.debug.print("\n  Slowest {d} tests:\n", .{show_count});
        for (top_buf.items[0..show_count], 1..) |entry, rank| {
            const ms = nsToMs(entry.duration_ns);
            std.debug.print("    {d}. {s}  ({d:.1}ms)\n", .{ rank, tests[entry.idx].name, ms });
        }
    }
}

// ---------------------------------------------------------------------------
// CLI argument parsing
// ---------------------------------------------------------------------------

const CliArgs = struct {
    roc_binary: []const u8,
    filters: []const []const u8 = &.{},
    max_threads: ?usize = null,
    timeout_ms: u64 = 60_000,
    verbose: bool = false,
};

fn parseArgs(allocator: Allocator) !CliArgs {
    const raw_args = try std.process.argsAlloc(allocator);
    // Don't free — we reference slices from it.

    if (raw_args.len < 2) {
        std.debug.print(
            \\Usage: parallel_cli_runner <roc_binary> [options]
            \\
            \\Options:
            \\  --filter <pattern>   Run tests matching pattern (repeatable)
            \\  --threads <N>        Max concurrent workers (default: CPU count)
            \\  --timeout <ms>       Per-test timeout in ms (default: 60000)
            \\  --verbose            Show PASS results with timing
            \\
        , .{});
        std.process.exit(1);
    }

    var filters: std.ArrayListUnmanaged([]const u8) = .empty;
    var args = CliArgs{ .roc_binary = raw_args[1] };
    var i: usize = 2;
    while (i < raw_args.len) : (i += 1) {
        const arg = raw_args[i];
        if (std.mem.eql(u8, arg, "--filter")) {
            i += 1;
            if (i < raw_args.len) try filters.append(allocator, raw_args[i]);
        } else if (std.mem.eql(u8, arg, "--verbose")) {
            args.verbose = true;
        } else if (std.mem.eql(u8, arg, "--threads")) {
            i += 1;
            if (i < raw_args.len) {
                args.max_threads = std.fmt.parseInt(usize, raw_args[i], 10) catch null;
            }
        } else if (std.mem.eql(u8, arg, "--timeout")) {
            i += 1;
            if (i < raw_args.len) {
                args.timeout_ms = std.fmt.parseInt(u64, raw_args[i], 10) catch 60_000;
            }
        }
    }

    args.filters = try filters.toOwnedSlice(allocator);
    return args;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    // Arena for data that lives the entire run (args, test specs).
    var spec_arena = std.heap.ArenaAllocator.init(gpa);
    defer spec_arena.deinit();

    const args = try parseArgs(spec_arena.allocator());

    // Build flat test spec array
    const tests = try buildTestSpecs(spec_arena.allocator(), args.filters);
    if (tests.len == 0) {
        // Silent exit — this runner is one part of the test-cli umbrella step,
        // so a filter targeting roc_subcommands_test or glue_test legitimately
        // matches zero tests here.
        return;
    }

    // Determine worker count
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const max_children = args.max_threads orelse @min(cpu_count, tests.len);

    // Print banner
    std.debug.print("=== CLI Test Runner ===\n", .{});
    std.debug.print("{d} tests, {d} workers, {d}s timeout\n\n", .{
        tests.len,
        max_children,
        args.timeout_ms / 1000,
    });

    // Allocate results
    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash });

    // Run
    var wall_timer = Timer.start() catch @panic("no clock");
    processPoolMain(tests, results, max_children, args.timeout_ms, gpa, args.roc_binary);
    const wall_ns = wall_timer.read();

    // Report
    printResults(tests, results, args.verbose, gpa, wall_ns, max_children);

    // Free captured strings from deserialized results (gpa-owned via readStr).
    for (results) |r| {
        if (r.stderr_capture) |s| gpa.free(s);
        if (r.stdout_capture) |s| gpa.free(s);
        if (r.message) |m| gpa.free(m);
    }

    // Exit with failure if any tests failed
    var any_failure = false;
    for (results) |r| {
        switch (r.status) {
            .fail, .crash, .timeout => {
                any_failure = true;
                break;
            },
            else => {},
        }
    }
    if (any_failure) std.process.exit(1);
}
