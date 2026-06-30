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
const build_options = @import("build_options");
const posix = std.posix;
const Allocator = std.mem.Allocator;

/// Monotonic timer replacement for std.time.Timer (removed in Zig 0.16).
pub const Timer = struct {
    start_ns: u64,

    pub const Error = error{UnsupportedClock};

    pub fn start() Error!Timer {
        return .{ .start_ns = monotonicNs() };
    }

    /// Returns elapsed nanoseconds since start.
    pub fn read(self: *Timer) u64 {
        return monotonicNs() - self.start_ns;
    }

    /// Returns elapsed nanoseconds and resets the timer.
    pub fn lap(self: *Timer) u64 {
        const now = monotonicNs();
        const elapsed = now - self.start_ns;
        self.start_ns = now;
        return elapsed;
    }
};

/// Returns a monotonic timestamp in nanoseconds.
pub fn monotonicNs() u64 {
    if (builtin.os.tag == .linux) {
        var ts: std.os.linux.timespec = undefined;
        _ = std.os.linux.clock_gettime(.MONOTONIC, &ts);
        return @as(u64, @intCast(ts.sec)) * 1_000_000_000 + @as(u64, @intCast(ts.nsec));
    } else if (builtin.os.tag == .macos or builtin.os.tag == .freebsd) {
        var ts: std.c.timespec = undefined;
        _ = std.c.clock_gettime(std.c.CLOCK.MONOTONIC, &ts);
        return @as(u64, @intCast(ts.sec)) * 1_000_000_000 + @as(u64, @intCast(ts.nsec));
    } else if (builtin.os.tag == .windows) {
        const k32 = struct {
            extern "kernel32" fn QueryPerformanceCounter(lpPerformanceCount: *i64) callconv(.winapi) std.os.windows.BOOL;
            extern "kernel32" fn QueryPerformanceFrequency(lpFrequency: *i64) callconv(.winapi) std.os.windows.BOOL;
        };
        var counter: i64 = undefined;
        var freq: i64 = undefined;
        if (k32.QueryPerformanceCounter(&counter) == .FALSE) unreachable;
        if (k32.QueryPerformanceFrequency(&freq) == .FALSE) unreachable;
        return @intCast(@divTrunc(@as(i128, counter) * std.time.ns_per_s, @as(i128, freq)));
    } else {
        @compileError("unsupported monotonic clock for test harness");
    }
}

fn testProgressIntervalNs(is_tty: bool) u64 {
    if (is_tty) return std.time.ns_per_s;

    const interval_ms = build_options.test_progress_interval_ms;
    if (interval_ms == 0) return 0;

    return std.math.mul(u64, interval_ms, std.time.ns_per_ms) catch {
        std.debug.print("invalid -Dtest-progress-interval-ms={d}: value is too large\n", .{interval_ms});
        return 0;
    };
}

fn printPoolProgress(is_tty: bool, completed: usize, total: usize, elapsed_ns: u64) void {
    const wall_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;
    if (is_tty) {
        std.debug.print("\r  progress: {d}/{d} done, {d:.1}s elapsed", .{
            completed, total, wall_s,
        });
    } else {
        std.debug.print("  progress: {d}/{d} done, {d:.1}s elapsed\n", .{
            completed, total, wall_s,
        });
    }
}

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
        ) == .FALSE) {
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

fn terminateProcess(child_id: std.process.Child.Id) void {
    if (builtin.os.tag == .windows) {
        const k32 = struct {
            extern "kernel32" fn TerminateProcess(
                hProcess: std.os.windows.HANDLE,
                uExitCode: c_uint,
            ) callconv(.winapi) std.os.windows.BOOL;
        };
        _ = k32.TerminateProcess(child_id, 1);
        return;
    }

    const pid: std.posix.pid_t = child_id;
    posixKill(pid, posix.SIG.KILL) catch {};
}

// POSIX compatibility helpers (removed from std.posix in Zig 0.16)
//
// Many POSIX functions moved out of std.posix into std.c (C bindings) or
// std.os.linux (raw syscalls) in Zig 0.16.  These thin wrappers restore the
// error-union API we relied on previously.

/// stdoutFd: cross-platform stdout file descriptor as a posix.fd_t.
///
/// With link_libc=true, posix.fd_t resolves to std.c.fd_t, which is a HANDLE on
/// Windows and i32 elsewhere. std.posix.STDOUT_FILENO is a comptime_int (1),
/// which works on POSIX but cannot coerce to HANDLE on Windows.
pub fn stdoutFd() posix.fd_t {
    if (builtin.os.tag == .windows) {
        const k32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: u32) callconv(.winapi) ?std.os.windows.HANDLE;
        };
        const STD_OUTPUT_HANDLE: u32 = @bitCast(@as(i32, -11));
        return k32.GetStdHandle(STD_OUTPUT_HANDLE).?;
    }
    return std.posix.STDOUT_FILENO;
}

/// stdinFd: cross-platform stdin file descriptor; see `stdoutFd`.
pub fn stdinFd() posix.fd_t {
    if (builtin.os.tag == .windows) {
        const k32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: u32) callconv(.winapi) ?std.os.windows.HANDLE;
        };
        const STD_INPUT_HANDLE: u32 = @bitCast(@as(i32, -10));
        return k32.GetStdHandle(STD_INPUT_HANDLE).?;
    }
    return std.posix.STDIN_FILENO;
}

/// milliTimestamp: replacement for std.time.milliTimestamp (removed in Zig 0.16).
pub fn milliTimestamp() i64 {
    if (builtin.os.tag == .linux) {
        var ts: std.os.linux.timespec = undefined;
        _ = std.os.linux.clock_gettime(.MONOTONIC, &ts);
        const ns = @as(u64, @intCast(ts.sec)) * 1_000_000_000 + @as(u64, @intCast(ts.nsec));
        return @as(i64, @intCast(ns / 1_000_000));
    }
    if (builtin.os.tag == .windows) {
        const k32 = struct {
            extern "kernel32" fn QueryPerformanceCounter(lpPerformanceCount: *i64) callconv(.winapi) std.os.windows.BOOL;
            extern "kernel32" fn QueryPerformanceFrequency(lpFrequency: *i64) callconv(.winapi) std.os.windows.BOOL;
        };
        var counter: i64 = undefined;
        var freq: i64 = undefined;
        _ = k32.QueryPerformanceCounter(&counter);
        _ = k32.QueryPerformanceFrequency(&freq);
        // counter * 1000 would overflow within ~30 minutes of uptime on a 10MHz QPF;
        // divide freq down first so the multiplication can't blow.
        return @divTrunc(counter, @divTrunc(freq, 1000));
    }
    // POSIX (macOS, BSD, etc.) via libc.
    var ts: std.c.timespec = undefined;
    _ = std.c.clock_gettime(.MONOTONIC, &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

/// pipe: returns [2]fd_t or error.
pub fn pipe() error{PipeFailed}![2]posix.fd_t {
    var fds: [2]posix.fd_t = undefined;
    const rc = std.c.pipe(&fds);
    if (rc != 0) return error.PipeFailed;
    return fds;
}

/// close: closes a file descriptor (ignores errors).
pub fn closeFd(fd: posix.fd_t) void {
    _ = std.c.close(fd);
}

/// Mark a file descriptor as close-on-exec so subprocesses do not inherit it.
pub fn setCloseOnExec(fd: posix.fd_t) void {
    const flags = std.c.fcntl(fd, std.c.F.GETFD);
    if (flags == -1) return;
    _ = std.c.fcntl(fd, std.c.F.SETFD, flags | std.c.FD_CLOEXEC);
}

/// fork: wrapper around std.c.fork that returns pid_t or error.
pub fn fork() error{ForkFailed}!posix.pid_t {
    const pid = std.c.fork();
    if (pid < 0) return error.ForkFailed;
    return pid;
}

/// Holds the process id and exit status from waitpid.
pub const WaitResult = struct { pid: posix.pid_t, status: u32 };
/// Waits for a child process and returns its pid and raw status.
pub fn waitpid(pid: posix.pid_t, flags: c_int) WaitResult {
    var status: c_int = 0;
    const result = std.c.waitpid(pid, &status, flags);
    return .{ .pid = result, .status = @bitCast(status) };
}

/// posixRead: read from fd, returns bytes read or error.
pub fn posixRead(fd: posix.fd_t, buf: []u8) error{ReadFailed}!usize {
    if (builtin.os.tag == .windows) {
        // See writeAll: fd_t is HANDLE on Windows-libc and std.c.read takes a C fd int,
        // so ReadFile is the only correct path here.
        const k32 = struct {
            extern "kernel32" fn ReadFile(
                hFile: std.os.windows.HANDLE,
                lpBuffer: [*]u8,
                nNumberOfBytesToRead: u32,
                lpNumberOfBytesRead: ?*u32,
                lpOverlapped: ?*anyopaque,
            ) callconv(.winapi) std.os.windows.BOOL;
        };
        const chunk: u32 = @intCast(@min(buf.len, std.math.maxInt(u32)));
        var n_read: u32 = 0;
        if (k32.ReadFile(fd, buf.ptr, chunk, &n_read, null) == .FALSE) {
            // ERROR_BROKEN_PIPE (109) means clean EOF — surface as 0 bytes, not an error.
            const err = std.os.windows.GetLastError();
            if (err == .BROKEN_PIPE) return 0;
            return error.ReadFailed;
        }
        return n_read;
    }
    const rc = std.c.read(fd, buf.ptr, buf.len);
    if (rc < 0) return error.ReadFailed;
    return @intCast(rc);
}

/// posixKill: send signal to pid.
fn posixKill(pid: posix.pid_t, sig: posix.SIG) error{KillFailed}!void {
    const rc = std.c.kill(pid, sig);
    if (rc != 0) return error.KillFailed;
}

/// posixPoll: poll file descriptors.
fn posixPoll(fds: []posix.pollfd, timeout: c_int) error{PollFailed}!usize {
    const rc = std.c.poll(fds.ptr, @intCast(fds.len), timeout);
    if (rc < 0) return error.PollFailed;
    return @intCast(rc);
}

// End POSIX compatibility helpers

// Pipe I/O helpers

/// Write all bytes to fd, looping on partial writes.
pub fn writeAll(fd: posix.fd_t, data: []const u8) void {
    var written: usize = 0;
    if (builtin.os.tag == .windows) {
        // posix.fd_t is a HANDLE on Windows-libc; std.c.write takes a C fd int (not a HANDLE)
        // so calling it with a HANDLE pointer silently writes nothing. Use WriteFile directly.
        const k32 = struct {
            extern "kernel32" fn WriteFile(
                hFile: std.os.windows.HANDLE,
                lpBuffer: [*]const u8,
                nNumberOfBytesToWrite: u32,
                lpNumberOfBytesWritten: ?*u32,
                lpOverlapped: ?*anyopaque,
            ) callconv(.winapi) std.os.windows.BOOL;
        };
        while (written < data.len) {
            const chunk: u32 = @intCast(@min(data.len - written, std.math.maxInt(u32)));
            var n_written: u32 = 0;
            if (k32.WriteFile(fd, data.ptr + written, chunk, &n_written, null) == .FALSE) return;
            if (n_written == 0) return;
            written += n_written;
        }
        return;
    }
    while (written < data.len) {
        const rc = std.c.write(fd, data.ptr + written, data.len - written);
        if (rc < 0) return;
        written += @intCast(rc);
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
    include_llvm: bool = false,
    help_requested: bool = false,
    stats_json_path: ?[]const u8 = null,
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

/// Aggregate status counts for one harness run.
pub const StatsSummary = struct {
    total: usize = 0,
    passed: usize = 0,
    failed: usize = 0,
    skipped: usize = 0,
    crashed: usize = 0,
    timed_out: usize = 0,
};

/// Extra key/value detail attached to one stats event.
pub const StatsData = struct {
    key: []const u8,
    value: []const u8,
};

/// Completed timing span emitted by a runner.
pub const StatsEvent = struct {
    id: []const u8,
    parent_id: ?[]const u8 = null,
    kind: []const u8,
    name: []const u8,
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    worker_index: ?usize = null,
    data: []const StatsData = &.{},

    /// Returns the non-negative event duration in nanoseconds.
    pub fn durationNs(self: StatsEvent) u64 {
        return self.end_ns -| self.start_ns;
    }
};

/// Top-level stats payload written by one runner.
pub const RunnerStats = struct {
    runner: []const u8,
    summary: StatsSummary,
    events: []const StatsEvent,
};

/// Parent-recorded execution span for one process-pool test.
pub const PoolSpan = struct {
    test_index: usize,
    worker_index: usize,
    start_ns: u64,
    end_ns: u64,

    pub fn durationNs(self: PoolSpan) u64 {
        return self.end_ns -| self.start_ns;
    }
};

/// Returns true when a stats status should be treated as failure detail.
pub fn statusIsFailure(status: []const u8) bool {
    return !std.mem.eql(u8, status, "pass") and
        !std.mem.eql(u8, status, "passed") and
        !std.mem.eql(u8, status, "skip") and
        !std.mem.eql(u8, status, "skipped");
}

/// Returns true when a worker argv flag consumes the following value.
pub fn workerTemplateArgConsumesValue(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--worker") or
        std.mem.eql(u8, arg, "--worker-backend") or
        std.mem.eql(u8, arg, "--stats-json");
}

/// Returns true when a worker argv flag should be removed by itself.
pub fn workerTemplateDropsFlag(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--worker-stream");
}

/// Writes a self-contained runner stats JSON file.
pub fn writeRunnerStatsJson(
    allocator: Allocator,
    io: std.Io,
    path: []const u8,
    stats: RunnerStats,
) !void {
    if (std.fs.path.dirname(path)) |dir| {
        std.Io.Dir.cwd().access(io, dir, .{}) catch |err| switch (err) {
            error.FileNotFound => try std.Io.Dir.cwd().createDirPath(io, dir),
            else => return err,
        };
    }

    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"runner\": ");
    try appendJsonString(&out, allocator, stats.runner);
    try out.appendSlice(allocator, ",\n  \"summary\": {");
    try appendSummaryField(&out, allocator, "total", stats.summary.total, true);
    try appendSummaryField(&out, allocator, "passed", stats.summary.passed, false);
    try appendSummaryField(&out, allocator, "failed", stats.summary.failed, false);
    try appendSummaryField(&out, allocator, "skipped", stats.summary.skipped, false);
    try appendSummaryField(&out, allocator, "crashed", stats.summary.crashed, false);
    try appendSummaryField(&out, allocator, "timed_out", stats.summary.timed_out, false);
    try out.appendSlice(allocator, "\n  },\n  \"events\": [\n");

    for (stats.events, 0..) |event, i| {
        if (i > 0) try out.appendSlice(allocator, ",\n");
        try out.appendSlice(allocator, "    {\n      \"id\": ");
        try appendJsonString(&out, allocator, event.id);
        try out.appendSlice(allocator, ",\n      \"parent_id\": ");
        if (event.parent_id) |parent_id| {
            try appendJsonString(&out, allocator, parent_id);
        } else {
            try out.appendSlice(allocator, "null");
        }
        try out.appendSlice(allocator, ",\n      \"kind\": ");
        try appendJsonString(&out, allocator, event.kind);
        try out.appendSlice(allocator, ",\n      \"name\": ");
        try appendJsonString(&out, allocator, event.name);
        try out.appendSlice(allocator, ",\n      \"status\": ");
        try appendJsonString(&out, allocator, event.status);
        try appendNumberField(&out, allocator, "start_ns", event.start_ns);
        try appendNumberField(&out, allocator, "end_ns", event.end_ns);
        try appendNumberField(&out, allocator, "duration_ns", event.durationNs());
        if (event.worker_index) |worker_index| {
            try appendNumberField(&out, allocator, "worker_index", @intCast(worker_index));
        }
        if (event.data.len > 0) {
            try out.appendSlice(allocator, ",\n      \"data\": {");
            for (event.data, 0..) |data, data_i| {
                if (data_i > 0) try out.appendSlice(allocator, ",");
                try out.appendSlice(allocator, "\n        ");
                try appendJsonString(&out, allocator, data.key);
                try out.appendSlice(allocator, ": ");
                try appendJsonString(&out, allocator, data.value);
            }
            try out.appendSlice(allocator, "\n      }");
        }
        try out.appendSlice(allocator, "\n    }");
    }

    try out.appendSlice(allocator, "\n  ]\n}\n");

    var file = try std.Io.Dir.cwd().createFile(io, path, .{});
    defer file.close(io);
    try file.writeStreamingAll(io, out.items);
}

fn appendSummaryField(out: *std.ArrayList(u8), allocator: Allocator, name: []const u8, value: usize, first: bool) !void {
    if (!first) try out.appendSlice(allocator, ",");
    try out.appendSlice(allocator, "\n    ");
    try appendJsonString(out, allocator, name);
    try out.appendSlice(allocator, ": ");
    try appendU64(out, allocator, @intCast(value));
}

fn appendNumberField(out: *std.ArrayList(u8), allocator: Allocator, name: []const u8, value: u64) !void {
    try out.appendSlice(allocator, ",\n      ");
    try appendJsonString(out, allocator, name);
    try out.appendSlice(allocator, ": ");
    try appendU64(out, allocator, value);
}

fn appendU64(out: *std.ArrayList(u8), allocator: Allocator, value: u64) !void {
    const text = try std.fmt.allocPrint(allocator, "{d}", .{value});
    defer allocator.free(text);
    try out.appendSlice(allocator, text);
}

fn appendJsonString(out: *std.ArrayList(u8), allocator: Allocator, value: []const u8) !void {
    try out.append(allocator, '"');
    for (value) |byte| {
        switch (byte) {
            '"' => try out.appendSlice(allocator, "\\\""),
            '\\' => try out.appendSlice(allocator, "\\\\"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            0x08 => try out.appendSlice(allocator, "\\b"),
            0x0c => try out.appendSlice(allocator, "\\f"),
            else => {
                if (byte < 0x20) {
                    const escaped = try std.fmt.allocPrint(allocator, "\\u{x:0>4}", .{byte});
                    defer allocator.free(escaped);
                    try out.appendSlice(allocator, escaped);
                } else {
                    try out.append(allocator, byte);
                }
            },
        }
    }
    try out.append(allocator, '"');
}

/// Parse standard harness flags from an argv-style slice.
pub fn parseStandardArgsFromSlice(raw_args: []const []const u8, allocator: Allocator) !StandardArgs {
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
        } else if (std.mem.eql(u8, arg, "--llvm") or std.mem.eql(u8, arg, "--include-llvm")) {
            args.include_llvm = true;
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
        } else if (std.mem.eql(u8, arg, "--stats-json")) {
            i += 1;
            if (i < raw_args.len) {
                args.stats_json_path = raw_args[i];
            }
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
pub fn parseStandardArgs(allocator: Allocator, process_args: std.process.Args) !StandardArgs {
    const raw_args = try process_args.toSlice(allocator);
    // Don't free — we reference slices from it.
    // Cast from []const [:0]const u8 to []const []const u8.
    const raw_args_plain: []const []const u8 = @ptrCast(raw_args);
    return parseStandardArgsFromSlice(raw_args_plain, allocator);
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

test "parseStandardArgsFromSlice parses llvm aliases" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const args = try parseStandardArgsFromSlice(&.{
        "runner",
        "--include-llvm",
    }, arena.allocator());

    try std.testing.expect(args.include_llvm);
    try std.testing.expectEqual(@as(usize, 0), args.positional.len);
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

test "parseStandardArgsFromSlice parses stats flags without polluting positional" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const args = try parseStandardArgsFromSlice(&.{
        "runner",
        "roc-binary",
        "--stats-json",
        "zig-out/minici/raw/eval.json",
    }, arena.allocator());

    try std.testing.expect(args.stats_json_path != null);
    try std.testing.expectEqualStrings("zig-out/minici/raw/eval.json", args.stats_json_path.?);
    try std.testing.expectEqual(@as(usize, 1), args.positional.len);
    try std.testing.expectEqualStrings("roc-binary", args.positional[0]);
}

test "worker template strips stats and worker flags" {
    try std.testing.expect(workerTemplateArgConsumesValue("--worker"));
    try std.testing.expect(workerTemplateArgConsumesValue("--worker-backend"));
    try std.testing.expect(workerTemplateArgConsumesValue("--stats-json"));
    try std.testing.expect(workerTemplateDropsFlag("--worker-stream"));
    try std.testing.expect(!workerTemplateArgConsumesValue("--filter"));
}

// Process pool (comptime-generic)

/// Configuration for the process pool. The runner provides type-specific
/// callbacks for test execution, serialization, and deserialization.
pub fn PoolConfig(comptime Spec: type, comptime Result: type) type {
    return struct {
        /// Run one test in the forked child. Called with an arena allocator
        /// and the same timeout budget enforced by the parent watchdog.
        runTest: *const fn (std.Io, Allocator, Spec, u64) Result,
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
            worker_index: usize,
            start_ns: u64,
            start_time_ms: i64,
            buf: std.ArrayListUnmanaged(u8),
            timed_out: bool,
        };

        var global_slots: ?[]?ChildSlot = null;

        fn recordSpan(
            spans: ?[]?PoolSpan,
            test_index: usize,
            worker_index: usize,
            start_ns: u64,
            end_ns: u64,
        ) void {
            const target_spans = spans orelse return;
            if (test_index >= target_spans.len) return;
            target_spans[test_index] = .{
                .test_index = test_index,
                .worker_index = worker_index,
                .start_ns = start_ns,
                .end_ns = @max(start_ns, end_ns),
            };
        }

        fn sigintHandler(_: posix.SIG) callconv(.c) void {
            const slots = global_slots orelse return;
            for (slots) |slot_opt| {
                if (slot_opt) |slot| {
                    if (cfg.use_process_groups) {
                        posixKill(-slot.pid, posix.SIG.KILL) catch {};
                    } else {
                        posixKill(slot.pid, posix.SIG.KILL) catch {};
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

        fn launchChild(
            io: std.Io,
            slot: *?ChildSlot,
            specs: []const Spec,
            test_idx: usize,
            worker_index: usize,
            timeout_ms: u64,
            pool_start_ns: u64,
            spans: ?[]?PoolSpan,
        ) bool {
            if (comptime !has_fork) return false;

            const start_ns = monotonicNs() -| pool_start_ns;
            if (cfg.onTestStarted) |cb| cb(specs[test_idx]);

            const pipe_fds = pipe() catch {
                recordSpan(spans, test_idx, worker_index, start_ns, monotonicNs() -| pool_start_ns);
                return false;
            };

            const pid = fork() catch {
                closeFd(pipe_fds[0]);
                closeFd(pipe_fds[1]);
                recordSpan(spans, test_idx, worker_index, start_ns, monotonicNs() -| pool_start_ns);
                return false;
            };

            if (pid == 0) {
                // === Child process ===
                closeFd(pipe_fds[0]);
                setCloseOnExec(pipe_fds[1]);

                if (cfg.use_process_groups) {
                    _ = std.c.setsid();
                }

                var arena = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
                const allocator = arena.allocator();

                const result = cfg.runTest(io, allocator, specs[test_idx], timeout_ms);
                cfg.serialize(pipe_fds[1], result);
                closeFd(pipe_fds[1]);
                std.c._exit(0);
            }

            // === Parent ===
            closeFd(pipe_fds[1]);
            slot.* = .{
                .pid = pid,
                .pipe_fd = pipe_fds[0],
                .test_index = test_idx,
                .worker_index = worker_index,
                .start_ns = start_ns,
                .start_time_ms = milliTimestamp(),
                .buf = .empty,
                .timed_out = false,
            };
            return true;
        }

        fn reapChild(slot: *?ChildSlot, results: []Result, gpa: Allocator, pool_start_ns: u64, spans: ?[]?PoolSpan) void {
            var s = slot.* orelse return;
            slot.* = null;

            drainPipe(s.pipe_fd, &s.buf);
            closeFd(s.pipe_fd);

            const wait_result = waitpid(s.pid, 0);
            const term_signal: u8 = @truncate(wait_result.status & 0x7f);

            if (s.timed_out or term_signal == 9) {
                results[s.test_index] = cfg.timeout_result;
            } else if (term_signal != 0) {
                results[s.test_index] = cfg.default_result;
            } else {
                results[s.test_index] = cfg.deserialize(s.buf.items, gpa) orelse
                    cfg.default_result;
            }

            recordSpan(spans, s.test_index, s.worker_index, s.start_ns, monotonicNs() -| pool_start_ns);
            s.buf.deinit(std.heap.smp_allocator);
        }

        fn drainPipe(fd: posix.fd_t, buf: *std.ArrayListUnmanaged(u8)) void {
            var read_buf: [4096]u8 = undefined;
            while (true) {
                const n = posixRead(fd, &read_buf) catch break;
                if (n == 0) break;
                buf.appendSlice(std.heap.smp_allocator, read_buf[0..n]) catch break;
            }
        }

        /// Run tests using a fork-based process pool.
        /// On Windows, dispatches to a `std.process.Child`-based pool when
        /// `worker_argv_template` is provided (the runner knows how to re-invoke
        /// itself as a single-test worker); otherwise falls back to sequential
        /// in-process execution.
        pub fn run(
            io: std.Io,
            specs: []const Spec,
            results: []Result,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            worker_argv_template: ?[]const []const u8,
        ) void {
            runWithSpans(io, specs, results, null, max_children, timeout_ms, gpa, worker_argv_template);
        }

        pub fn runWithSpans(
            io: std.Io,
            specs: []const Spec,
            results: []Result,
            spans: ?[]?PoolSpan,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            worker_argv_template: ?[]const []const u8,
        ) void {
            const pool_start_ns = monotonicNs();
            if (comptime !has_fork) {
                if (worker_argv_template) |tmpl| {
                    runChildPool(io, specs, results, spans, max_children, timeout_ms, gpa, tmpl, pool_start_ns);
                } else {
                    runSequential(io, specs, results, spans, gpa, timeout_ms, pool_start_ns);
                }
                return;
            }
            // On POSIX, children are forked in-place; the runtime template is
            // unused. It remains in the signature for a uniform API.
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

            const is_tty = std.Io.File.stderr().isTty(io) catch false;
            const progress_interval_ns = testProgressIntervalNs(is_tty);

            var next_test: usize = 0;
            var completed: usize = 0;
            var progress_timer = Timer.start() catch unreachable;
            var last_progress_ns: u64 = 0;

            // Fill initial slots
            for (slots, 0..) |*slot, worker_index| {
                if (next_test >= specs.len) break;
                if (!launchChild(io, slot, specs, next_test, worker_index, timeout_ms, pool_start_ns, spans)) {
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

                _ = posixPoll(poll_fds[0..n_poll], 500) catch 0;

                for (poll_fds[0..n_poll], 0..) |pfd, pi| {
                    const slot_idx = poll_map[pi];
                    if (pfd.revents & posix.POLL.IN != 0) {
                        var read_buf: [4096]u8 = undefined;
                        const n = posixRead(pfd.fd, &read_buf) catch 0;
                        if (n > 0) {
                            if (slots[slot_idx]) |*s| {
                                s.buf.appendSlice(std.heap.smp_allocator, read_buf[0..n]) catch {};
                            }
                        }
                    }
                    if (pfd.revents & (posix.POLL.HUP | posix.POLL.ERR | posix.POLL.NVAL) != 0) {
                        reapChild(&slots[slot_idx], results, gpa, pool_start_ns, spans);
                        completed += 1;

                        if (next_test < specs.len) {
                            if (!launchChild(io, &slots[slot_idx], specs, next_test, slot_idx, timeout_ms, pool_start_ns, spans)) {
                                results[next_test] = cfg.default_result;
                                completed += 1;
                            }
                            next_test += 1;
                        }
                    }
                }

                // Check timeouts
                if (timeout_ms > 0) {
                    const now = milliTimestamp();
                    for (slots) |*slot_opt| {
                        if (slot_opt.*) |*slot| {
                            const elapsed: u64 = @intCast(@max(0, now - slot.start_time_ms));
                            const kill_after_ms = timeout_ms +| cfg.timeout_report_grace_ms;
                            if (elapsed > kill_after_ms and !slot.timed_out) {
                                slot.timed_out = true;
                                const test_name = cfg.getName(specs[slot.test_index]);
                                std.debug.print("\n  HANG  {s}  ({d}ms) — killing\n", .{ test_name, elapsed });
                                if (cfg.use_process_groups) {
                                    posixKill(-slot.pid, posix.SIG.KILL) catch {};
                                } else {
                                    posixKill(slot.pid, posix.SIG.KILL) catch {};
                                }
                            }
                        }
                    }
                }

                // TTY progress updates in-place. Non-TTY progress is opt-in via
                // -Dtest-progress-interval-ms and prints one factual line per interval.
                const progress_elapsed = progress_timer.read();
                if (progress_interval_ns != 0 and progress_elapsed - last_progress_ns >= progress_interval_ns) {
                    last_progress_ns = progress_elapsed;
                    printPoolProgress(is_tty, completed, specs.len, progress_elapsed);
                }
            }

            if (is_tty) {
                std.debug.print("\r{s}\r", .{" " ** 72});
            }
        }

        /// Sequential fallback for platforms without fork (Windows).
        /// Effectively unused under the Child-based path; kept as defense in
        /// depth for callers that don't build a `worker_argv_template`.
        fn runSequential(
            io: std.Io,
            specs: []const Spec,
            results: []Result,
            spans: ?[]?PoolSpan,
            gpa: Allocator,
            timeout_ms: u64,
            pool_start_ns: u64,
        ) void {
            var arena = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
            defer arena.deinit();
            for (specs, 0..) |spec, i| {
                _ = arena.reset(.retain_capacity);
                const start_ns = monotonicNs() -| pool_start_ns;
                if (cfg.onTestStarted) |cb| cb(spec);
                const unstable_result = cfg.runTest(io, arena.allocator(), spec, timeout_ms);
                results[i] = cfg.stabilizeResult(gpa, unstable_result);
                recordSpan(spans, i, 0, start_ns, monotonicNs() -| pool_start_ns);
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
            test_index: usize,
            worker_index: usize,
            start_ns: u64,
            start_ms: i64,
            timed_out: bool,
        };

        const ChildPoolState = struct {
            io: std.Io,
            next_test: std.atomic.Value(usize),
            slots: []?ActiveChild,
            slots_mutex: std.Io.Mutex,
            watchdog_done: std.atomic.Value(bool),
            template: []const []const u8,
            job: ?if (builtin.os.tag == .windows) std.os.windows.HANDLE else void,
            timeout_ms: u64,
            gpa: Allocator,
            specs: []const Spec,
            results: []Result,
            spans: ?[]?PoolSpan,
            pool_start_ns: u64,
        };

        fn runChildPool(
            io: std.Io,
            specs: []const Spec,
            results: []Result,
            spans: ?[]?PoolSpan,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            template: []const []const u8,
            pool_start_ns: u64,
        ) void {
            if (comptime builtin.os.tag != .windows) {
                runSequential(io, specs, results, spans, gpa, timeout_ms, pool_start_ns);
                return;
            }
            if (!cfg.windows_persistent_workers) {
                runChildPoolSingleShot(io, specs, results, spans, max_children, timeout_ms, gpa, template, pool_start_ns);
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
                .io = io,
                .next_test = std.atomic.Value(usize).init(0),
                .slots = slots,
                .slots_mutex = .init,
                .watchdog_done = std.atomic.Value(bool).init(false),
                .template = template,
                .job = job,
                .timeout_ms = timeout_ms,
                .gpa = gpa,
                .specs = specs,
                .results = results,
                .spans = spans,
                .pool_start_ns = pool_start_ns,
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
            io: std.Io,
            next_test: std.atomic.Value(usize),
            template: []const []const u8,
            timeout_ms: u64,
            gpa: Allocator,
            specs: []const Spec,
            results: []Result,
            spans: ?[]?PoolSpan,
            pool_start_ns: u64,
        };

        fn runChildPoolSingleShot(
            io: std.Io,
            specs: []const Spec,
            results: []Result,
            spans: ?[]?PoolSpan,
            max_children: usize,
            timeout_ms: u64,
            gpa: Allocator,
            template: []const []const u8,
            pool_start_ns: u64,
        ) void {
            var state = SingleShotChildPoolState{
                .io = io,
                .next_test = std.atomic.Value(usize).init(0),
                .template = template,
                .timeout_ms = timeout_ms,
                .gpa = gpa,
                .specs = specs,
                .results = results,
                .spans = spans,
                .pool_start_ns = pool_start_ns,
            };

            const threads = gpa.alloc(std.Thread, max_children) catch return;
            defer gpa.free(threads);

            var spawned: usize = 0;
            for (threads, 0..) |*t, worker_index| {
                t.* = std.Thread.spawn(.{}, singleShotWorkerThread, .{ &state, worker_index }) catch break;
                spawned += 1;
            }

            for (threads[0..spawned]) |t| t.join();
        }

        fn singleShotWorkerThread(state: *SingleShotChildPoolState, worker_index: usize) void {
            while (true) {
                const idx = state.next_test.fetchAdd(1, .monotonic);
                if (idx >= state.specs.len) return;

                const start_ns = monotonicNs() -| state.pool_start_ns;
                if (cfg.onTestStarted) |cb| cb(state.specs[idx]);

                state.results[idx] = switch (spawnSingleWorker(state.io, state.gpa, state.template, idx, &.{}, state.timeout_ms)) {
                    .ok => |result| result,
                    .timed_out => cfg.timeout_result,
                    .crashed => cfg.default_result,
                };
                recordSpan(state.spans, idx, worker_index, start_ns, monotonicNs() -| state.pool_start_ns);
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
            const io = state.io;

            var argv: std.ArrayListUnmanaged([]const u8) = .empty;
            defer argv.deinit(gpa);
            argv.appendSlice(gpa, state.template) catch return;
            argv.append(gpa, "--worker-stream") catch return;

            var child = std.process.spawn(io, .{
                .argv = argv.items,
                .stdin = .pipe,
                .stdout = .pipe,
                .stderr = .inherit,
            }) catch return;

            if (comptime builtin.os.tag == .windows) {
                if (state.job) |h| {
                    if (child.id) |cid| job_object.assign(h, cid);
                }
            }

            defer {
                if (child.stdin) |stdin| stdin.close(io);
                child.stdin = null;
                if (child.id != null) _ = child.wait(io) catch {};
            }

            while (true) {
                const idx = state.next_test.fetchAdd(1, .monotonic);
                if (idx >= state.specs.len) return;

                const start_ns = monotonicNs() -| state.pool_start_ns;
                if (cfg.onTestStarted) |cb| cb(state.specs[idx]);

                state.slots_mutex.lockUncancelable(io);
                state.slots[slot_idx] = ActiveChild{
                    .child = &child,
                    .test_index = idx,
                    .worker_index = slot_idx,
                    .start_ns = start_ns,
                    .start_ms = milliTimestamp(),
                    .timed_out = false,
                };
                state.slots_mutex.unlock(io);

                const cmd = std.fmt.allocPrint(gpa, "{d}\n", .{idx}) catch {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                };
                defer gpa.free(cmd);

                const child_stdin = child.stdin orelse {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                };
                if (fileWriteAll(io, child_stdin, cmd)) |_| {} else |_| {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                }

                var length_bytes: [4]u8 = undefined;
                const child_stdout = child.stdout orelse {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                };
                if (fileReadExactly(io, child_stdout, &length_bytes)) |_| {} else |_| {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                }
                const length = std.mem.readInt(u32, &length_bytes, .little);

                const payload = gpa.alloc(u8, length) catch {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                };
                defer gpa.free(payload);
                if (fileReadExactly(io, child_stdout, payload)) |_| {} else |_| {
                    state.results[idx] = handleReadFailure(state, slot_idx);
                    return;
                }

                state.slots_mutex.lockUncancelable(io);
                const timed_out = if (state.slots[slot_idx]) |s| s.timed_out else false;
                state.slots[slot_idx] = null;
                state.slots_mutex.unlock(io);

                state.results[idx] = if (timed_out)
                    cfg.timeout_result
                else
                    cfg.deserialize(payload, gpa) orelse cfg.default_result;
                recordSpan(state.spans, idx, slot_idx, start_ns, monotonicNs() -| state.pool_start_ns);
            }
        }

        fn handleReadFailure(state: *ChildPoolState, slot_idx: usize) Result {
            const io = state.io;
            state.slots_mutex.lockUncancelable(io);
            const maybe_slot = state.slots[slot_idx];
            const timed_out = if (maybe_slot) |s| s.timed_out else false;
            state.slots[slot_idx] = null;
            state.slots_mutex.unlock(io);
            if (maybe_slot) |slot| {
                recordSpan(state.spans, slot.test_index, slot.worker_index, slot.start_ns, monotonicNs() -| state.pool_start_ns);
            }
            return if (timed_out) cfg.timeout_result else cfg.default_result;
        }

        fn fileWriteAll(io: std.Io, file: std.Io.File, bytes: []const u8) !void {
            try file.writeStreamingAll(io, bytes);
        }

        fn fileReadExactly(io: std.Io, file: std.Io.File, out: []u8) !void {
            var off: usize = 0;
            while (off < out.len) {
                const n = file.readStreaming(io, &.{out[off..]}) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEof,
                    else => return err,
                };
                if (n == 0) return error.UnexpectedEof;
                off += n;
            }
        }

        fn watchdogThread(state: *ChildPoolState) void {
            const io = state.io;
            while (!state.watchdog_done.load(.acquire)) {
                // Swallow cancel: watchdog cleanup happens on the next tick.
                std.Io.sleep(io, std.Io.Duration.fromMilliseconds(100), .awake) catch {};
                if (state.timeout_ms == 0) continue;

                const now = milliTimestamp();
                state.slots_mutex.lockUncancelable(io);
                defer state.slots_mutex.unlock(io);
                for (state.slots) |*slot_opt| {
                    if (slot_opt.*) |*slot| {
                        const elapsed: u64 = @intCast(@max(0, now - slot.start_ms));
                        const kill_after_ms = state.timeout_ms +| cfg.timeout_report_grace_ms;
                        if (elapsed > kill_after_ms and !slot.timed_out) {
                            slot.timed_out = true;
                            if (slot.child.id) |id| terminateProcess(id);
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
            io: std.Io,
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

            var child = std.process.spawn(io, .{
                .argv = argv.items,
                .stdout = .pipe,
                .stderr = .inherit,
            }) catch return .crashed;

            // Foreground watchdog: a thread that kills the child if it runs
            // over budget. Pairs with the synchronous read-then-wait below.
            const Watch = struct {
                io: std.Io,
                child_ptr: *std.process.Child,
                deadline_ms: i64,
                timed_out: std.atomic.Value(bool),
                done: std.atomic.Value(bool),

                fn run(self: *@This()) void {
                    while (!self.done.load(.acquire)) {
                        // Swallow cancel: outer loop re-checks `done` and the deadline.
                        std.Io.sleep(self.io, std.Io.Duration.fromMilliseconds(100), .awake) catch {};
                        if (self.done.load(.acquire)) return;
                        if (milliTimestamp() >= self.deadline_ms) {
                            self.timed_out.store(true, .release);
                            if (self.child_ptr.id) |id| terminateProcess(id);
                            return;
                        }
                    }
                }
            };

            const kill_after_ms = timeout_ms +| cfg.timeout_report_grace_ms;
            var watch = Watch{
                .io = io,
                .child_ptr = &child,
                .deadline_ms = milliTimestamp() + @as(i64, @intCast(kill_after_ms)),
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
                const n = child.stdout.?.readStreaming(io, &.{&read_buf}) catch |err| switch (err) {
                    error.EndOfStream => break,
                    else => break,
                };
                if (n == 0) break;
                buf.appendSlice(gpa, read_buf[0..n]) catch break;
            }

            const term = if (child.id != null)
                child.wait(io) catch std.process.Child.Term{ .unknown = 0 }
            else
                std.process.Child.Term{ .unknown = 0 };

            watch.done.store(true, .release);
            if (watch_thread) |t| t.join();

            if (watch.timed_out.load(.acquire)) return .timed_out;

            return switch (term) {
                .exited => |code| if (code == 0) blk: {
                    const r = cfg.deserialize(buf.items, gpa) orelse break :blk SingleWorkerOutcome{ .crashed = {} };
                    break :blk SingleWorkerOutcome{ .ok = r };
                } else .crashed,
                else => .crashed,
            };
        }
    };
}
