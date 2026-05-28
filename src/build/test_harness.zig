//! Shared runtime harness for parallel test runners.
//!
//! Provides a comptime-generic fork-based process pool, timing statistics,
//! pipe I/O helpers, and standardized CLI argument parsing. Used by:
//! - src/eval/test/parallel_runner.zig (eval expression tests)
//! - src/cli/test/parallel_cli_runner.zig (platform integration tests)
//!
//! The pool forks child processes, each running one test. Results are
//! serialized over a pipe and collected by the single-threaded parent.

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;
const Allocator = std.mem.Allocator;

pub const Timer = std.time.Timer;
/// Whether the platform supports `fork` for child process spawning.
pub const has_fork = (builtin.os.tag != .windows);

// Windows JobObject bindings (used by runChildPool to ensure that killing the
// parent test runner tears down every still-running worker). The standard
// library doesn't expose these.
const job_object = if (builtin.os.tag == .windows) struct {
    const windows = std.os.windows;
    const HANDLE = windows.HANDLE;
    const BOOL = windows.BOOL;
    const DWORD = windows.DWORD;

    const JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE: DWORD = 0x00002000;

    const JOBOBJECT_BASIC_LIMIT_INFORMATION = extern struct {
        PerProcessUserTimeLimit: windows.LARGE_INTEGER,
        PerJobUserTimeLimit: windows.LARGE_INTEGER,
        LimitFlags: DWORD,
        MinimumWorkingSetSize: usize,
        MaximumWorkingSetSize: usize,
        ActiveProcessLimit: DWORD,
        Affinity: usize,
        PriorityClass: DWORD,
        SchedulingClass: DWORD,
    };

    const IO_COUNTERS = extern struct {
        ReadOperationCount: u64,
        WriteOperationCount: u64,
        OtherOperationCount: u64,
        ReadTransferCount: u64,
        WriteTransferCount: u64,
        OtherTransferCount: u64,
    };

    const JOBOBJECT_EXTENDED_LIMIT_INFORMATION = extern struct {
        BasicLimitInformation: JOBOBJECT_BASIC_LIMIT_INFORMATION,
        IoInfo: IO_COUNTERS,
        ProcessMemoryLimit: usize,
        JobMemoryLimit: usize,
        PeakProcessMemoryUsed: usize,
        PeakJobMemoryUsed: usize,
    };

    // JobObjectInfoClass values; we only need ExtendedLimitInformation = 9.
    const JobObjectExtendedLimitInformation: c_int = 9;

    extern "kernel32" fn CreateJobObjectW(
        lpJobAttributes: ?*anyopaque,
        lpName: ?[*:0]const u16,
    ) callconv(.winapi) ?HANDLE;
    extern "kernel32" fn SetInformationJobObject(
        hJob: HANDLE,
        JobObjectInfoClass: c_int,
        lpJobObjectInfo: *anyopaque,
        cbJobObjectInfoLength: DWORD,
    ) callconv(.winapi) BOOL;
    extern "kernel32" fn AssignProcessToJobObject(
        hJob: HANDLE,
        hProcess: HANDLE,
    ) callconv(.winapi) BOOL;

    fn create() ?HANDLE {
        const job = CreateJobObjectW(null, null) orelse return null;
        var info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std.mem.zeroes(JOBOBJECT_EXTENDED_LIMIT_INFORMATION);
        info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        if (SetInformationJobObject(
            job,
            JobObjectExtendedLimitInformation,
            &info,
            @sizeOf(JOBOBJECT_EXTENDED_LIMIT_INFORMATION),
        ) == 0) {
            windows.CloseHandle(job);
            return null;
        }
        return job;
    }

    fn assign(job: HANDLE, child_handle: HANDLE) void {
        _ = AssignProcessToJobObject(job, child_handle);
    }

    fn close(job: HANDLE) void {
        windows.CloseHandle(job);
    }
} else struct {};

// Pipe I/O helpers

/// Write all bytes to fd, looping on partial writes.
pub fn writeAll(fd: posix.fd_t, data: []const u8) void {
    var written: usize = 0;
    while (written < data.len) {
        written += posix.write(fd, data[written..]) catch return;
    }
}

/// Read a string of given length from buffer, advancing offset. Dupe into gpa.
pub fn readStr(buf: []const u8, offset: *usize, len: u32, gpa: Allocator) ?[]const u8 {
    if (len == 0) return null;
    const end = offset.* + len;
    if (end > buf.len) return null;
    const slice = buf[offset.*..end];
    offset.* = end;
    return gpa.dupe(u8, slice) catch null;
}

// Timing statistics

/// Aggregated timing statistics for a set of measurements.
pub const TimingStats = struct {
    min: u64,
    max: u64,
    mean: u64,
    median: u64,
    std_dev: u64,
    p95: u64,
    total: u64,
    count: usize,
};

/// Compute min, max, mean, median, stddev, p95, and total from a slice of nanosecond values.
pub fn computeTimingStats(values: []u64) ?TimingStats {
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

/// Convert nanoseconds to milliseconds.
pub fn nsToMs(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000.0;
}

/// Print a single row of timing statistics, or dashes if no data is available.
pub fn printStatsRow(label: []const u8, stats: ?TimingStats) void {
    if (stats) |s| {
        std.debug.print("  {s:<8} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1} {d:>8.1}   {d:>3}\n", .{
            label,
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
}

/// Print the header row for timing statistics output.
pub fn printStatsHeader() void {
    std.debug.print("  {s:<8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8}   {s:>3}\n", .{
        "Phase", "Min", "Max", "Mean", "Median", "StdDev", "P95", "Total", "N",
    });
    std.debug.print("  {s:-<8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8}   {s:->3}\n", .{
        "", "", "", "", "", "", "", "", "",
    });
}

/// Print the N slowest tests by duration. Caller provides a getName callback
/// to extract the display name from the test spec.
pub fn printSlowestN(
    comptime Spec: type,
    specs: []const Spec,
    durations: []const u64,
    n: usize,
    gpa: Allocator,
    comptime getName: fn (Spec) []const u8,
) void {
    const TopEntry = struct {
        idx: usize,
        duration_ns: u64,
    };
    var top_buf: std.ArrayListUnmanaged(TopEntry) = .empty;
    defer top_buf.deinit(gpa);
    for (durations, 0..) |d, i| {
        if (d > 0) {
            top_buf.append(gpa, .{ .idx = i, .duration_ns = d }) catch continue;
        }
    }
    std.mem.sort(TopEntry, top_buf.items, {}, struct {
        fn lessThan(_: void, a: TopEntry, b: TopEntry) bool {
            return a.duration_ns > b.duration_ns; // descending
        }
    }.lessThan);

    const show_count = @min(n, top_buf.items.len);
    if (show_count > 0) {
        std.debug.print("\n  Slowest {d} tests:\n", .{show_count});
        for (top_buf.items[0..show_count], 1..) |entry, rank| {
            const ms = nsToMs(entry.duration_ns);
            std.debug.print("    {d}. {s}  ({d:.1}ms)\n", .{ rank, getName(specs[entry.idx]), ms });
        }
    }
}

// CLI argument parsing

/// Common CLI arguments shared across parallel test runners.
pub const StandardArgs = struct {
    filters: []const []const u8 = &.{},
    max_threads: ?usize = null,
    timeout_ms: u64 = 120_000,
    timeout_provided: bool = false,
    verbose: bool = false,
    help_requested: bool = false,
    /// When set, the runner runs a single test (by index after filters) and
    /// serializes its result to stdout. Used by the Windows Child-based
    /// parallel executor in `ProcessPool.runChildPool` for Phase-2 retry.
    worker_index: ?usize = null,
    /// When set alongside `worker_index`, restrict the run to a single named
    /// backend within that test. The runner interprets this string.
    worker_backend: ?[]const u8 = null,
    /// Persistent worker mode: the runner reads test indices from stdin (one
    /// decimal per line), runs each, writes a u32-length-prefixed serialized
    /// result to stdout, and loops until stdin EOFs. Used by the Windows
    /// Child-based executor to amortize process-boot cost across many tests.
    worker_stream: bool = false,
    /// Remaining positional args (runner-specific)
    positional: []const []const u8 = &.{},
};

fn parseStandardArgsFromSlice(raw_args: []const []const u8, allocator: Allocator) !StandardArgs {
    var filters: std.ArrayListUnmanaged([]const u8) = .empty;
    var positional: std.ArrayListUnmanaged([]const u8) = .empty;
    var args = StandardArgs{};

    // Skip argv[0] (program name)
    var i: usize = 1;
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
                const parsed = std.fmt.parseInt(usize, raw_args[i], 10) catch null;
                args.max_threads = if (parsed) |value|
                    if (value > 0) value else null
                else
                    null;
            }
        } else if (std.mem.eql(u8, arg, "--timeout")) {
            i += 1;
            if (i < raw_args.len) {
                args.timeout_provided = true;
                args.timeout_ms = std.fmt.parseInt(u64, raw_args[i], 10) catch 120_000;
            }
        } else if (std.mem.eql(u8, arg, "--worker")) {
            i += 1;
            if (i < raw_args.len) {
                args.worker_index = std.fmt.parseInt(usize, raw_args[i], 10) catch null;
            }
        } else if (std.mem.eql(u8, arg, "--worker-backend")) {
            i += 1;
            if (i < raw_args.len) {
                args.worker_backend = raw_args[i];
            }
        } else if (std.mem.eql(u8, arg, "--worker-stream")) {
            args.worker_stream = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            args.help_requested = true;
        } else if (!std.mem.startsWith(u8, arg, "--")) {
            try positional.append(allocator, arg);
        }
    }

    args.filters = try filters.toOwnedSlice(allocator);
    args.positional = try positional.toOwnedSlice(allocator);
    return args;
}

/// Parse standard harness flags from argv.
pub fn parseStandardArgs(allocator: Allocator) !StandardArgs {
    const raw_args = try std.process.argsAlloc(allocator);
    // Don't free — we reference slices from it.
    return parseStandardArgsFromSlice(raw_args, allocator);
}

test "parseStandardArgsFromSlice preserves help and explicit timeout" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const args = try parseStandardArgsFromSlice(&.{
        "runner",
        "--help",
        "--timeout",
        "60000",
    }, arena.allocator());

    try std.testing.expect(args.help_requested);
    try std.testing.expect(args.timeout_provided);
    try std.testing.expectEqual(@as(u64, 60_000), args.timeout_ms);
}

test "parseStandardArgsFromSlice treats threads zero as default and keeps repeatable filters" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const args = try parseStandardArgsFromSlice(&.{
        "runner",
        "roc-binary",
        "--threads",
        "0",
        "--filter",
        "alpha",
        "--filter",
        "beta",
    }, arena.allocator());

    try std.testing.expect(args.max_threads == null);
    try std.testing.expectEqual(@as(usize, 2), args.filters.len);
    try std.testing.expectEqualStrings("alpha", args.filters[0]);
    try std.testing.expectEqualStrings("beta", args.filters[1]);
    try std.testing.expectEqual(@as(usize, 1), args.positional.len);
    try std.testing.expectEqualStrings("roc-binary", args.positional[0]);
}

test "parseStandardArgsFromSlice parses --worker and --worker-backend without polluting positional" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const args = try parseStandardArgsFromSlice(&.{
        "runner",
        "roc-binary",
        "--worker",
        "7",
        "--worker-backend",
        "dev",
    }, arena.allocator());

    try std.testing.expectEqual(@as(?usize, 7), args.worker_index);
    try std.testing.expect(args.worker_backend != null);
    try std.testing.expectEqualStrings("dev", args.worker_backend.?);
    try std.testing.expectEqual(@as(usize, 1), args.positional.len);
    try std.testing.expectEqualStrings("roc-binary", args.positional[0]);
}

// Process pool (comptime-generic)

/// Configuration for the process pool. The runner provides type-specific
/// callbacks for test execution, serialization, and deserialization.
pub fn PoolConfig(comptime Spec: type, comptime Result: type) type {
    return struct {
        /// Run one test in the forked child. Called with an arena allocator
        /// and the same timeout budget enforced by the parent watchdog.
        runTest: *const fn (Allocator, Spec, u64) Result,
        /// Serialize a result to the pipe fd.
        serialize: *const fn (posix.fd_t, Result) void,
        /// Deserialize a result from the accumulated pipe buffer.
        deserialize: *const fn ([]const u8, Allocator) ?Result,
        /// Default result for crash/timeout (before deserialization).
        default_result: Result,
        /// Result to use for timeout.
        timeout_result: Result,
        /// Stabilize any arena-owned data for the no-fork sequential fallback.
        stabilizeResult: *const fn (Allocator, Result) Result,
        /// Extract test name from spec (for timeout messages).
        getName: *const fn (Spec) []const u8,
        /// Use setsid() + kill(-pid) for process group cleanup.
        /// Enable when children spawn subprocesses (e.g., roc build).
        use_process_groups: bool = false,
        /// Extra parent-watchdog time after the child-visible timeout budget.
        /// Runners that enforce finer-grained timeouts inside the child use
        /// this to let the child serialize its attributed timeout result.
        timeout_report_grace_ms: u64 = 0,
        /// On Windows, reuse child runner processes across tests. Disable this
        /// for runners whose tests need a fresh runner process per spec, so
        /// each logical test has one process boundary and one result frame.
        windows_persistent_workers: bool = true,
        /// Called from the parent thread right before launching each test.
        /// Use for "RUN <name>" logging — keeps it coherent across N workers.
        onTestStarted: ?*const fn (Spec) void = null,
    };
}

/// Comptime-generic fork-based process pool.
pub fn ProcessPool(comptime Spec: type, comptime Result: type, comptime cfg: PoolConfig(Spec, Result)) type {
    return struct {
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
                    if (cfg.use_process_groups) {
                        posix.kill(-slot.pid, posix.SIG.KILL) catch {};
                    } else {
                        posix.kill(slot.pid, posix.SIG.KILL) catch {};
                    }
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

        fn launchChild(slot: *?ChildSlot, specs: []const Spec, test_idx: usize, timeout_ms: u64) bool {
            if (comptime !has_fork) return false;

            if (cfg.onTestStarted) |cb| cb(specs[test_idx]);

            const pipe_fds = posix.pipe() catch return false;

            const pid = posix.fork() catch {
                posix.close(pipe_fds[0]);
                posix.close(pipe_fds[1]);
                return false;
            };

            if (pid == 0) {
                // === Child process ===
                posix.close(pipe_fds[0]);

                if (cfg.use_process_groups) {
                    _ = std.c.setsid();
                }

                var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                const allocator = arena.allocator();

                const result = cfg.runTest(allocator, specs[test_idx], timeout_ms);
                cfg.serialize(pipe_fds[1], result);
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

        fn reapChild(slot: *?ChildSlot, results: []Result, gpa: Allocator) void {
            var s = slot.* orelse return;
            slot.* = null;

            drainPipe(s.pipe_fd, &s.buf);
            posix.close(s.pipe_fd);

            const wait_result = posix.waitpid(s.pid, 0);
            const term_signal: u8 = @truncate(wait_result.status & 0x7f);

            if (s.timed_out or term_signal == 9) {
                results[s.test_index] = cfg.timeout_result;
            } else if (term_signal != 0) {
                results[s.test_index] = cfg.default_result;
            } else {
                results[s.test_index] = cfg.deserialize(s.buf.items, gpa) orelse
                    cfg.default_result;
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

        /// Run tests using a fork-based process pool.
        /// On Windows, dispatches to a `std.process.Child`-based pool when
        /// `worker_argv_template` is provided (the runner knows how to re-invoke
        /// itself as a single-test worker); otherwise falls back to sequential
        /// in-process execution.
        pub fn run(
            specs: []const Spec,
            results: []Result,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            worker_argv_template: ?[]const []const u8,
        ) void {
            if (comptime !has_fork) {
                if (worker_argv_template) |tmpl| {
                    runChildPool(specs, results, max_children, timeout_ms, gpa, tmpl);
                } else {
                    runSequential(specs, results, gpa, timeout_ms);
                }
                return;
            }
            // On POSIX, children are forked in-place — the runtime template is
            // unused. The parameter is still in the signature for a uniform API.
            _ = &worker_argv_template;

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
                if (next_test >= specs.len) break;
                if (!launchChild(slot, specs, next_test, timeout_ms)) {
                    results[next_test] = cfg.default_result;
                    completed += 1;
                }
                next_test += 1;
            }

            // Main event loop
            while (completed < specs.len) {
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

                        if (next_test < specs.len) {
                            if (!launchChild(&slots[slot_idx], specs, next_test, timeout_ms)) {
                                results[next_test] = cfg.default_result;
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
                            const kill_after_ms = timeout_ms +| cfg.timeout_report_grace_ms;
                            if (elapsed > kill_after_ms and !slot.timed_out) {
                                slot.timed_out = true;
                                const test_name = cfg.getName(specs[slot.test_index]);
                                std.debug.print("\n  HANG  {s}  ({d}ms) — killing\n", .{ test_name, elapsed });
                                if (cfg.use_process_groups) {
                                    posix.kill(-slot.pid, posix.SIG.KILL) catch {};
                                } else {
                                    posix.kill(slot.pid, posix.SIG.KILL) catch {};
                                }
                            }
                        }
                    }
                }

                // Progress line every ~1s (tty only)
                const progress_elapsed = progress_timer.read();
                if (progress_elapsed - last_progress_ns >= 1_000_000_000) {
                    last_progress_ns = progress_elapsed;
                    if (is_tty) {
                        const wall_s = @as(f64, @floatFromInt(progress_elapsed)) / 1_000_000_000.0;
                        std.debug.print("\r  progress: {d}/{d} done, {d:.1}s elapsed", .{
                            completed, specs.len, wall_s,
                        });
                    }
                }
            }

            if (is_tty) {
                std.debug.print("\r{s}\r", .{" " ** 72});
            }
        }

        /// Sequential fallback for platforms without fork (Windows).
        /// Effectively unused under the Child-based path; kept as defense in
        /// depth for callers that don't build a `worker_argv_template`.
        fn runSequential(specs: []const Spec, results: []Result, gpa: Allocator, timeout_ms: u64) void {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            for (specs, 0..) |spec, i| {
                _ = arena.reset(.retain_capacity);
                if (cfg.onTestStarted) |cb| cb(spec);
                const unstable_result = cfg.runTest(arena.allocator(), spec, timeout_ms);
                results[i] = cfg.stabilizeResult(gpa, unstable_result);
            }
        }

        // Windows-only Child-based parallel executor. See test_harness.zig
        // module doc and `Pool.run` for the dispatch logic.

        /// Outcome of `spawnSingleWorker`. Callers distinguish ok/crash/timeout
        /// so they can synthesize placeholder results instead of reading
        /// undefined memory from a `default_result`.
        pub const SingleWorkerOutcome = union(enum) {
            ok: Result,
            crashed: void,
            timed_out: void,
        };

        const ActiveChild = struct {
            child: *std.process.Child,
            start_ms: i64,
            timed_out: bool,
        };

        const ChildPoolState = struct {
            next_test: std.atomic.Value(usize),
            slots: []?ActiveChild,
            slots_mutex: std.Thread.Mutex,
            watchdog_done: std.atomic.Value(bool),
            template: []const []const u8,
            job: ?if (builtin.os.tag == .windows) std.os.windows.HANDLE else void,
            timeout_ms: u64,
            gpa: Allocator,
            specs: []const Spec,
            results: []Result,
        };

        fn runChildPool(
            specs: []const Spec,
            results: []Result,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            template: []const []const u8,
        ) void {
            if (comptime builtin.os.tag != .windows) {
                runSequential(specs, results, gpa, timeout_ms);
                return;
            }
            if (!cfg.windows_persistent_workers) {
                runChildPoolSingleShot(specs, results, max_children, timeout_ms, gpa, template);
                return;
            }

            const job = job_object.create();
            defer if (job) |h| job_object.close(h);

            const slots = gpa.alloc(?ActiveChild, max_children) catch {
                std.debug.print("fatal: failed to allocate child-pool slots\n", .{});
                return;
            };
            defer gpa.free(slots);
            @memset(slots, null);

            var state = ChildPoolState{
                .next_test = std.atomic.Value(usize).init(0),
                .slots = slots,
                .slots_mutex = .{},
                .watchdog_done = std.atomic.Value(bool).init(false),
                .template = template,
                .job = job,
                .timeout_ms = timeout_ms,
                .gpa = gpa,
                .specs = specs,
                .results = results,
            };

            const threads = gpa.alloc(std.Thread, max_children) catch return;
            defer gpa.free(threads);

            var spawned: usize = 0;
            for (threads, 0..) |*t, i| {
                t.* = std.Thread.spawn(.{}, workerThread, .{ &state, i }) catch break;
                spawned += 1;
            }

            const watchdog = std.Thread.spawn(.{}, watchdogThread, .{&state}) catch null;

            for (threads[0..spawned]) |t| t.join();

            state.watchdog_done.store(true, .release);
            if (watchdog) |wd| wd.join();
        }

        const SingleShotChildPoolState = struct {
            next_test: std.atomic.Value(usize),
            template: []const []const u8,
            timeout_ms: u64,
            gpa: Allocator,
            specs: []const Spec,
            results: []Result,
        };

        fn runChildPoolSingleShot(
            specs: []const Spec,
            results: []Result,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            template: []const []const u8,
        ) void {
            var state = SingleShotChildPoolState{
                .next_test = std.atomic.Value(usize).init(0),
                .template = template,
                .timeout_ms = timeout_ms,
                .gpa = gpa,
                .specs = specs,
                .results = results,
            };

            const threads = gpa.alloc(std.Thread, max_children) catch return;
            defer gpa.free(threads);

            var spawned: usize = 0;
            for (threads) |*t| {
                t.* = std.Thread.spawn(.{}, singleShotWorkerThread, .{&state}) catch break;
                spawned += 1;
            }

            for (threads[0..spawned]) |t| t.join();
        }

        fn singleShotWorkerThread(state: *SingleShotChildPoolState) void {
            while (true) {
                const idx = state.next_test.fetchAdd(1, .monotonic);
                if (idx >= state.specs.len) return;

                if (cfg.onTestStarted) |cb| cb(state.specs[idx]);

                state.results[idx] = switch (spawnSingleWorker(state.gpa, state.template, idx, &.{}, state.timeout_ms)) {
                    .ok => |result| result,
                    .timed_out => cfg.timeout_result,
                    .crashed => cfg.default_result,
                };
            }
        }

        /// One persistent Child per worker thread. Spawns once in
        /// `--worker-stream` mode, then loops: pull next test index from the
        /// shared atomic counter, write `"<idx>\n"` to stdin, read a `u32`
        /// length + that many bytes from stdout, pass to `cfg.deserialize`.
        /// Closes stdin when the counter is exhausted; the worker reads EOF
        /// and exits cleanly. Amortizes Child-spawn + main-bootstrap +
        /// builtin-load cost across many tests on the same worker.
        ///
        /// Crash recovery is intentionally minimal: any I/O error mid-stream
        /// marks the in-flight test as crashed and exits the thread. Other
        /// workers continue; the runner's Phase-2 retry pass handles
        /// per-backend attribution for any test that lost its details.
        fn workerThread(state: *ChildPoolState, slot_idx: usize) void {
            const gpa = state.gpa;

            var argv: std.ArrayListUnmanaged([]const u8) = .empty;
            defer argv.deinit(gpa);
            argv.appendSlice(gpa, state.template) catch return;
            argv.append(gpa, "--worker-stream") catch return;

            var child = std.process.Child.init(argv.items, gpa);
            child.stdin_behavior = .Pipe;
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Inherit;
            child.spawn() catch return;

            if (comptime builtin.os.tag == .windows) {
                if (state.job) |h| job_object.assign(h, child.id);
            }

            defer {
                if (child.stdin) |stdin| stdin.close();
                child.stdin = null;
                _ = child.wait() catch {};
            }

            while (true) {
                const idx = state.next_test.fetchAdd(1, .monotonic);
                if (idx >= state.specs.len) return;

                if (cfg.onTestStarted) |cb| cb(state.specs[idx]);

                state.slots_mutex.lock();
                state.slots[slot_idx] = ActiveChild{
                    .child = &child,
                    .start_ms = std.time.milliTimestamp(),
                    .timed_out = false,
                };
                state.slots_mutex.unlock();

                const cmd = std.fmt.allocPrint(gpa, "{d}\n", .{idx}) catch {
                    state.results[idx] = cfg.default_result;
                    return;
                };
                defer gpa.free(cmd);

                if (fileWriteAll(child.stdin.?, cmd)) |_| {} else |_| {
                    state.results[idx] = cfg.default_result;
                    return;
                }

                var length_bytes: [4]u8 = undefined;
                if (fileReadExactly(child.stdout.?, &length_bytes)) |_| {} else |_| {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                }
                const length = std.mem.readInt(u32, &length_bytes, .little);

                const payload = gpa.alloc(u8, length) catch {
                    state.results[idx] = cfg.default_result;
                    return;
                };
                defer gpa.free(payload);
                if (fileReadExactly(child.stdout.?, payload)) |_| {} else |_| {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                }

                state.slots_mutex.lock();
                const timed_out = if (state.slots[slot_idx]) |s| s.timed_out else false;
                state.slots[slot_idx] = null;
                state.slots_mutex.unlock();

                state.results[idx] = if (timed_out)
                    cfg.timeout_result
                else
                    cfg.deserialize(payload, gpa) orelse cfg.default_result;
            }
        }

        fn handleReadFailure(state: *ChildPoolState, slot_idx: usize) Result {
            state.slots_mutex.lock();
            const timed_out = if (state.slots[slot_idx]) |s| s.timed_out else false;
            state.slots[slot_idx] = null;
            state.slots_mutex.unlock();
            return if (timed_out) cfg.timeout_result else cfg.default_result;
        }

        fn fileWriteAll(file: std.fs.File, bytes: []const u8) !void {
            var off: usize = 0;
            while (off < bytes.len) {
                off += try file.write(bytes[off..]);
            }
        }

        fn fileReadExactly(file: std.fs.File, out: []u8) !void {
            var off: usize = 0;
            while (off < out.len) {
                const n = try file.read(out[off..]);
                if (n == 0) return error.UnexpectedEof;
                off += n;
            }
        }

        fn watchdogThread(state: *ChildPoolState) void {
            while (!state.watchdog_done.load(.acquire)) {
                std.Thread.sleep(100 * std.time.ns_per_ms);
                if (state.timeout_ms == 0) continue;

                const now = std.time.milliTimestamp();
                state.slots_mutex.lock();
                defer state.slots_mutex.unlock();
                for (state.slots) |*slot_opt| {
                    if (slot_opt.*) |*slot| {
                        const elapsed: u64 = @intCast(@max(0, now - slot.start_ms));
                        const kill_after_ms = state.timeout_ms +| cfg.timeout_report_grace_ms;
                        if (elapsed > kill_after_ms and !slot.timed_out) {
                            slot.timed_out = true;
                            _ = slot.child.kill() catch {};
                        }
                    }
                }
            }
        }

        /// Spawn one Child worker for a specific test index, with optional
        /// extra args (e.g. `--worker-backend dev`), and return its outcome.
        /// Used by runner Phase-2 retry: re-run a failing test once per
        /// backend to attribute the crash.
        pub fn spawnSingleWorker(
            gpa: Allocator,
            template: []const []const u8,
            test_index: usize,
            extra_args: []const []const u8,
            timeout_ms: u64,
        ) SingleWorkerOutcome {
            if (comptime builtin.os.tag != .windows) {
                return .crashed; // Not supported off-Windows (POSIX uses fork path).
            }

            const idx_str = std.fmt.allocPrint(gpa, "{d}", .{test_index}) catch return .crashed;
            defer gpa.free(idx_str);

            var argv: std.ArrayListUnmanaged([]const u8) = .empty;
            defer argv.deinit(gpa);
            argv.appendSlice(gpa, template) catch return .crashed;
            argv.append(gpa, "--worker") catch return .crashed;
            argv.append(gpa, idx_str) catch return .crashed;
            argv.appendSlice(gpa, extra_args) catch return .crashed;

            var child = std.process.Child.init(argv.items, gpa);
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Inherit;
            child.spawn() catch return .crashed;

            // Foreground watchdog: a thread that kills the child if it runs
            // over budget. Pairs with the synchronous read-then-wait below.
            const Watch = struct {
                child_ptr: *std.process.Child,
                deadline_ms: i64,
                timed_out: std.atomic.Value(bool),
                done: std.atomic.Value(bool),

                fn run(self: *@This()) void {
                    while (!self.done.load(.acquire)) {
                        std.Thread.sleep(100 * std.time.ns_per_ms);
                        if (self.done.load(.acquire)) return;
                        if (std.time.milliTimestamp() >= self.deadline_ms) {
                            self.timed_out.store(true, .release);
                            _ = self.child_ptr.kill() catch {};
                            return;
                        }
                    }
                }
            };

            const kill_after_ms = timeout_ms +| cfg.timeout_report_grace_ms;
            var watch = Watch{
                .child_ptr = &child,
                .deadline_ms = std.time.milliTimestamp() + @as(i64, @intCast(kill_after_ms)),
                .timed_out = std.atomic.Value(bool).init(false),
                .done = std.atomic.Value(bool).init(false),
            };
            const watch_thread = if (timeout_ms > 0)
                std.Thread.spawn(.{}, Watch.run, .{&watch}) catch null
            else
                null;

            var buf: std.ArrayListUnmanaged(u8) = .empty;
            defer buf.deinit(gpa);
            var read_buf: [4096]u8 = undefined;
            while (true) {
                const n = child.stdout.?.read(&read_buf) catch break;
                if (n == 0) break;
                buf.appendSlice(gpa, read_buf[0..n]) catch break;
            }

            const term = child.wait() catch std.process.Child.Term{ .Unknown = 0 };

            watch.done.store(true, .release);
            if (watch_thread) |t| t.join();

            if (watch.timed_out.load(.acquire)) return .timed_out;

            return switch (term) {
                .Exited => |code| if (code == 0) blk: {
                    const r = cfg.deserialize(buf.items, gpa) orelse break :blk SingleWorkerOutcome{ .crashed = {} };
                    break :blk SingleWorkerOutcome{ .ok = r };
                } else .crashed,
                else => .crashed,
            };
        }
    };
}
