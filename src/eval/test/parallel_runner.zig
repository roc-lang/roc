//! Parallel eval test runner.
//!
//! Runs eval tests in parallel using a fork-based process pool, exercising
//! every enabled backend on every test case and comparing their results via
//! Str.inspect string comparison.
//!
//! ## Architecture overview
//!
//! Each test goes through a front-end (parse, canonicalize, type-check)
//! and is then evaluated by up to four independent backends:
//!
//!   1. **Interpreter** — walks the LIR directly.
//!   2. **Dev backend** — lowers LIR to native machine code.
//!   3. **WASM backend** — statement-only LIR compiled to wasm.
//!   4. **LLVM backend** — lowers statement-only LIR to LLVM bitcode when
//!      the runner is invoked with `--llvm`.
//!
//! ALL backends run via Str.inspect and must produce identical output strings.
//! This catches bugs where a backend produces a value of the right type but
//! wrong content.
//!
//! ## Process pool
//!
//! A single-threaded parent process manages up to N concurrent child
//! processes (one per test). The parent runs the frontend once, lowers through
//! checked modules to an ARC-inserted LIR image, and allocates that
//! image in shared memory. Children inherit or map that LIR image and run
//! backend evaluation only; they never inspect CIR, checked modules, or
//! post-check IRs. Children write only outcome text/metadata back through a pipe. The
//! parent multiplexes pipe reads using poll().
//!
//! This avoids the fork-in-multithreaded-process hazard: forking from
//! a threaded parent risks inheriting locked glibc mutexes, causing
//! deadlocks in child processes. With a single-threaded parent, all
//! forks are safe.
//!
//! ## Per-backend crash isolation
//!
//! Within each child process, individual backend evaluations still run
//! in nested forked subprocesses via `forkAndEval`. Since the child is
//! single-threaded, these nested forks are safe. If one backend crashes,
//! the others still produce results.
//!
//! ## Hang detection
//!
//! Integrated into the parent's poll() loop. If a child has been running
//! longer than the timeout (default 30s for interpreter/dev/wasm, 7 minutes
//! for LLVM), the parent SIGKILLs it. No separate watchdog thread is needed.
//!
//! ## Usage
//!
//!   zig build test-eval [-- [--filter <pattern>] [--threads <N>] [--timeout <ms>] [--verbose] [--llvm]]

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const coverage_options = @import("coverage_options");
const eval = @import("eval");

/// When true (set via `zig build coverage-eval`), the runner:
/// - Only builds/runs the interpreter backend (dev/wasm are DCE'd)
/// - Runs eval in-process (no fork) so kcov can trace it
/// - Forces single-threaded execution
const coverage_mode: bool = coverage_options.coverage;

const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            std.debug.print("[eval-test] " ++ fmt ++ "\n", args);
        }
    }
};

const helpers = eval.test_helpers;
const LoweredProgram = helpers.LoweredProgram;

const posix = std.posix;

/// Process-wide `std.Io` initialized from `main(init)` and consumed by helpers
/// that don't get an `io` parameter (e.g. pool worker callbacks).
var app_io: std.Io = undefined;

fn milliTimestamp() i64 {
    return std.Io.Timestamp.now(app_io, .awake).toMilliseconds();
}

// Test definition modules
const eval_tests = @import("eval_tests.zig");

//
// Public types (imported by test definition files)
//

/// A single data-driven eval test: source expression, expected result, and optional backend skips.
pub const TestCase = struct {
    name: []const u8,
    source: []const u8,
    source_kind: helpers.SourceKind = .expr,
    imports: []const helpers.ModuleSource = &.{},
    expected: Expected,
    skip: Skip = .{},
    /// Known compiler-bug repros are opt-in so ordinary `zig build test-eval`
    /// stays green while still letting bug hunts use the eval pipeline directly.
    known_bug: bool = false,

    pub const Expected = union(enum) {
        inspect_str: []const u8,
        allocations_at_most: AllocationExpectation,
        problem: void,
        crash: void,
        problem_and_crash: void,

        pub fn display(self: Expected) ?[]const u8 {
            return switch (self) {
                .inspect_str => |value| value,
                .allocations_at_most => |value| value.output,
                .problem => null,
                .crash => null,
                .problem_and_crash => null,
            };
        }
    };

    pub const AllocationExpectation = struct {
        output: []const u8,
        max_allocations: u32,
    };

    pub const Skip = packed struct {
        interpreter: bool = false,
        dev: bool = false,
        wasm: bool = false,
        llvm: bool = false,
    };
};

//
// Test outcome
//

/// Per-backend outcome detail, stored for reporting.
const BackendDetail = struct {
    status: Status,
    /// Str.inspect output (owned by arena, only valid for .pass/.wrong_value)
    value: ?[]const u8 = null,
    duration_ns: u64 = 0,

    const Status = enum { pass, fail, wrong_value, timeout, skip, not_implemented, not_run };
};

const NUM_BACKENDS = 4; // interpreter, dev, wasm, llvm
const BACKEND_NAMES = [NUM_BACKENDS][]const u8{ "interpreter", "dev", "wasm", "llvm" };
const LLVM_BACKEND_INDEX = 3;
/// Ubuntu ARM Nix CI measured these LLVM crash-test evaluations at roughly
/// 305-310s. Use a 7 minute LLVM-only budget so that slow LLVM codegen can
/// finish while the other backends keep the normal eval timeout.
const LLVM_BACKEND_TIMEOUT_MS: u64 = 420_000;
const BACKEND_TIMEOUT_REPORT_GRACE_MS: u64 = 5_000;
const FORKED_BACKEND_KILL_GRACE_MS: i64 = 5_000;
const FORKED_BACKEND_KILL_POLL_NS: u64 = 10 * std.time.ns_per_ms;
const LLVM_EVAL_LOCK_POLL_NS: u64 = 10 * std.time.ns_per_ms;
const DEV_BACKEND_IMPLEMENTED = eval.backendAvailable(.dev);
const WASM_BACKEND_IMPLEMENTED = true;
const LLVM_BACKEND_IMPLEMENTED = eval.backendAvailable(.llvm);

/// Set from `cli.verbose` in `main` after arg parsing. Read by `onTestStarted`,
/// which is registered as a comptime Pool callback and can't take a closure.
var verbose_logging: bool = false;

/// Set from `main` to the selected process-pool size before any worker runs.
/// POSIX child workers inherit this value through fork; Windows worker
/// processes recompute it from the same CLI args before entering worker mode.
var llvm_eval_slot_count: usize = 1;

/// Set from CLI parsing. LLVM eval is opt-in because it dominates eval test
/// runtime; CI runs a dedicated LLVM-enabled lane for backend coverage.
var include_llvm_backend: bool = false;

const TestOutcome = struct {
    status: Status,
    message: ?[]const u8 = null,
    timings: EvalTimings = .{},
    /// True only after backend execution has produced every backend row.
    has_backend_details: bool,
    /// Per-backend details (interpreter, dev, wasm, llvm). Valid only when
    /// `has_backend_details` is true.
    backends: [NUM_BACKENDS]BackendDetail,
    /// The expected Str.inspect string (for inspect_str tests), or null.
    expected_str: ?[]const u8 = null,

    const Status = enum { pass, fail, crash, skip, timeout };
};

const EvalTimings = struct {
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
    interpreter_ns: u64 = 0,
    dev_ns: u64 = 0,
    wasm_ns: u64 = 0,
    llvm_ns: u64 = 0,
};

const TestResult = struct {
    status: TestOutcome.Status,
    message: ?[]const u8,
    duration_ns: u64,
    timings: EvalTimings,
    has_backend_details: bool,
    backends: [NUM_BACKENDS]BackendDetail,
    expected_str: ?[]const u8 = null,
};

const harness = @import("test_harness");
const Timer = harness.Timer;

/// Fixed-size binary header for child-to-parent result serialization.
/// Native byte order (same machine, no cross-endian concern).
const WireHeader = extern struct {
    status: u8,
    backend_statuses: [NUM_BACKENDS]u8,
    backend_durations: [NUM_BACKENDS]u64,
    parse_ns: u64,
    canonicalize_ns: u64,
    typecheck_ns: u64,
    interpreter_ns: u64,
    dev_ns: u64,
    wasm_ns: u64,
    llvm_ns: u64,
    duration_ns: u64,
    has_backend_details: u8,
    message_len: u32,
    expected_str_len: u32,
    backend_value_lens: [NUM_BACKENDS]u32,
};

//
// Fork-based process isolation for backend evaluation
//

const has_fork = builtin.os.tag != .windows;

const BackendEvalFn = *const fn (std.mem.Allocator, *const LoweredProgram) anyerror![]u8;
const BackendEvalWithStatsFn = *const fn (std.mem.Allocator, *const LoweredProgram) anyerror!helpers.EvalRunResult;

/// Result of a forked backend evaluation.
const ForkResult = union(enum) {
    /// Child exited 0 and wrote result string to pipe.
    success: []const u8,
    /// Child exited non-zero (eval function returned an error).
    child_error: []const u8,
    /// Child exceeded the backend timeout and was killed.
    timed_out: void,
    /// Child was killed by a signal (e.g. SIGSEGV=11, SIGKILL=9).
    signal_death: u8,
    /// fork() or pipe() syscall failed.
    fork_failed: void,
};

const Deadline = struct {
    timer: Timer,
    timeout_ns: u64,

    fn init(timeout_ms: u64) Deadline {
        return .{
            .timer = Timer.start() catch unreachable,
            .timeout_ns = timeout_ms * std.time.ns_per_ms,
        };
    }

    fn remainingMs(self: *Deadline) u64 {
        const elapsed_ns = self.timer.read();
        if (elapsed_ns >= self.timeout_ns) return 0;
        return (self.timeout_ns - elapsed_ns) / std.time.ns_per_ms;
    }

    fn expired(self: *Deadline) bool {
        return self.timer.read() >= self.timeout_ns;
    }
};

fn remainingPollTimeoutMs(deadline: ?*Deadline) i32 {
    const active = deadline orelse return -1;
    const remaining = active.remainingMs();
    if (remaining == 0) return 0;
    return @intCast(@min(remaining, std.math.maxInt(i32)));
}

fn killForkedBackend(pid: posix.pid_t) void {
    posix.kill(-pid, posix.SIG.KILL) catch {
        posix.kill(pid, posix.SIG.KILL) catch {};
    };
}

fn reapForkedBackendAfterKill(pid: posix.pid_t) bool {
    const deadline_ms = milliTimestamp() + FORKED_BACKEND_KILL_GRACE_MS;
    while (true) {
        const wait_result = harness.waitpid(pid, posix.W.NOHANG);
        if (wait_result.pid == pid) return true;
        if (milliTimestamp() >= deadline_ms) return false;
        std.Io.sleep(app_io, std.Io.Duration.fromNanoseconds(FORKED_BACKEND_KILL_POLL_NS), .awake) catch {};
    }
}

fn killAndReapForkedBackend(pid: posix.pid_t) void {
    killForkedBackend(pid);
    _ = reapForkedBackendAfterKill(pid);
}

fn drainClosedPipe(fd: posix.fd_t, buf: *std.ArrayListUnmanaged(u8)) bool {
    var read_buf: [4096]u8 = undefined;
    while (true) {
        const bytes_read = posix.read(fd, &read_buf) catch return false;
        if (bytes_read == 0) return true;
        buf.appendSlice(std.heap.page_allocator, read_buf[0..bytes_read]) catch return false;
    }
}

/// Result of a forked backend evaluation that includes host-observed allocation stats.
const ForkStatsResult = union(enum) {
    success: helpers.EvalRunResult,
    child_error: []const u8,
    signal_death: u8,
    fork_failed: void,
};

/// Fork a child process to evaluate a backend, communicating the result via pipe.
///
/// The child calls `eval_fn(page_allocator, lowered_lir_image)`, where
/// `lowered_lir_image` is already a zero-copy view over ARC-inserted LIR
/// allocated in shared memory. Backend children must not inspect CIR, checked
/// modules, or post-check IRs; they write only the resulting string to the pipe and
/// `_exit(0)`. On error they `_exit(1)`.
///
/// The parent reads the pipe until EOF (important: before waitpid to avoid pipe
/// buffer deadlock), then reaps the child.
fn forkAndEval(
    eval_fn: BackendEvalFn,
    lowered: *const LoweredProgram,
    timeout_ms: u64,
    inherited_fd_to_close: ?posix.fd_t,
) ForkResult {
    if (comptime !has_fork or coverage_mode) {
        const result = eval_fn(std.heap.page_allocator, lowered) catch |err| {
            return .{ .child_error = @errorName(err) };
        };
        return .{ .success = result };
    }

    // std.process.getEnvVarOwned was removed in Zig 0.16; use std.c.getenv instead.
    const disable_fork = std.c.getenv("ROC_EVAL_NO_FORK") != null;
    if (disable_fork) {
        const result = eval_fn(std.heap.page_allocator, lowered) catch |err| {
            return .{ .child_error = @errorName(err) };
        };
        return .{ .success = result };
    }

    const pipe_fds = harness.pipe() catch {
        return .{ .fork_failed = {} };
    };
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = harness.fork() catch {
        harness.closeFd(pipe_read);
        harness.closeFd(pipe_write);
        return .{ .fork_failed = {} };
    };

    if (fork_result == 0) {
        // === Child process ===
        harness.closeFd(pipe_read);
        if (inherited_fd_to_close) |fd| harness.closeFd(fd);
        _ = std.c.setsid();

        // Arena batches allocations into fewer mmap calls; child _exit()s
        // immediately so the OS reclaims everything — no deinit needed.
        var child_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const child_alloc = child_arena.allocator();
        const result_str = eval_fn(child_alloc, lowered) catch |err| {
            // Write error name to pipe so parent can report it, then exit 2
            // to distinguish "error with name" from other failures.
            const name = @errorName(err);
            harness.writeAll(pipe_write, name);
            harness.closeFd(pipe_write);
            std.c._exit(2);
        };
        // Write the result string to the pipe.
        harness.writeAll(pipe_write, result_str);

        harness.closeFd(pipe_write);
        std.c._exit(0);
    }

    // === Parent process ===
    harness.closeFd(pipe_write);
    defer harness.closeFd(pipe_read);

    // Read pipe FIRST (before waitpid) to avoid deadlock when child output
    // exceeds the pipe buffer (~64KB). Use poll() so a hung backend child is
    // attributed here instead of waiting for the outer per-test watchdog.
    var result_buf: std.ArrayListUnmanaged(u8) = .empty;
    var read_buf: [4096]u8 = undefined;
    var read_error = false;
    var deadline = if (timeout_ms > 0) Deadline.init(timeout_ms) else null;
    const deadline_ptr: ?*Deadline = if (deadline) |*active| active else null;
    while (true) {
        var poll_fds = [_]posix.pollfd{.{
            .fd = pipe_read,
            .events = posix.POLL.IN | posix.POLL.HUP | posix.POLL.ERR | posix.POLL.NVAL,
            .revents = 0,
        }};
        const poll_timeout = remainingPollTimeoutMs(deadline_ptr);
        if (poll_timeout == 0) {
            killAndReapForkedBackend(fork_result);
            result_buf.deinit(std.heap.page_allocator);
            return .{ .timed_out = {} };
        }

        const poll_count = posix.poll(&poll_fds, poll_timeout) catch {
            read_error = true;
            break;
        };
        if (poll_count == 0) {
            killAndReapForkedBackend(fork_result);
            result_buf.deinit(std.heap.page_allocator);
            return .{ .timed_out = {} };
        }

        const revents = poll_fds[0].revents;
        if (revents & posix.POLL.IN != 0) {
            const bytes_read = posix.read(pipe_read, &read_buf) catch {
                read_error = true;
                break;
            };
            if (bytes_read == 0) break;
            result_buf.appendSlice(std.heap.page_allocator, read_buf[0..bytes_read]) catch {
                read_error = true;
                break;
            };
        }
        if (revents & posix.POLL.HUP != 0) {
            if (!drainClosedPipe(pipe_read, &result_buf)) read_error = true;
            break;
        }
        if (revents & (posix.POLL.ERR | posix.POLL.NVAL) != 0) {
            read_error = true;
            break;
        }
    }

    if (read_error) {
        killAndReapForkedBackend(fork_result);
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = "ChildExecFailed" };
    }

    // Now reap the child.
    const wait_result = harness.waitpid(fork_result, 0);

    const status = wait_result.status;
    const termination_signal: u8 = @truncate(status & 0x7f);

    if (termination_signal != 0) {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .signal_death = termination_signal };
    }

    const exit_code: u8 = @truncate((status >> 8) & 0xff);
    if (exit_code == 2) {
        // Child wrote error name to pipe and exited 2.
        const owned = result_buf.toOwnedSlice(std.heap.page_allocator) catch {
            result_buf.deinit(std.heap.page_allocator);
            return .{ .child_error = "ChildExecFailed" };
        };
        return .{ .child_error = owned };
    }
    if (exit_code != 0) {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = "ChildExecFailed" };
    }

    // Success — return the string read from the pipe.
    const owned = result_buf.toOwnedSlice(std.heap.page_allocator) catch {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = "ChildExecFailed" };
    };
    return .{ .success = owned };
}

const LlvmEvalPermit = struct {
    file: std.Io.File,

    fn acquire() !LlvmEvalPermit {
        var queue_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const queue_path = try llvmEvalQueueLockPath(&queue_path_buf);
        var queue_file = try openLlvmEvalLockFile(queue_path);
        defer queue_file.close(app_io);
        try queue_file.lock(app_io, .exclusive);
        defer queue_file.unlock(app_io);

        while (true) {
            if (try acquireLlvmEvalSlot()) |permit| {
                return permit;
            }

            try std.Io.sleep(app_io, std.Io.Duration.fromNanoseconds(LLVM_EVAL_LOCK_POLL_NS), .awake);
        }
    }

    fn release(self: *LlvmEvalPermit) void {
        self.file.unlock(app_io);
        self.file.close(app_io);
    }
};

fn acquireLlvmEvalSlot() !?LlvmEvalPermit {
    for (0..llvm_eval_slot_count) |slot| {
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const path = try llvmEvalSlotLockPath(&path_buf, slot);
        var file = try openLlvmEvalLockFile(path);
        errdefer file.close(app_io);

        if (try file.tryLock(app_io, .exclusive)) {
            return .{ .file = file };
        }

        file.close(app_io);
    }

    return null;
}

fn openLlvmEvalLockFile(path: []const u8) !std.Io.File {
    return std.Io.Dir.createFileAbsolute(app_io, path, .{
        .read = true,
        .truncate = false,
    });
}

fn llvmEvalLockPrefix() []const u8 {
    return if (builtin.os.tag == .windows)
        "C:\\Windows\\Temp\\roc_eval_llvm_"
    else
        "/tmp/roc_eval_llvm_";
}

fn llvmEvalSlotLockPath(buf: *[std.fs.max_path_bytes]u8, slot: usize) ![]const u8 {
    return std.fmt.bufPrint(buf, "{s}slot_{d}.lock", .{ llvmEvalLockPrefix(), slot });
}

fn llvmEvalQueueLockPath(buf: *[std.fs.max_path_bytes]u8) ![]const u8 {
    return std.fmt.bufPrint(buf, "{s}queue.lock", .{llvmEvalLockPrefix()});
}

fn llvmEvalInheritedFd(permit: *const LlvmEvalPermit) ?posix.fd_t {
    if (comptime !has_fork) return null;
    return permit.file.handle;
}

fn runBackendEval(
    index: usize,
    eval_fn: BackendEvalFn,
    lowered: *const LoweredProgram,
    timeout_ms: u64,
) !ForkResult {
    if (index == LLVM_BACKEND_INDEX) {
        var permit = try LlvmEvalPermit.acquire();
        defer permit.release();
        return forkAndEval(eval_fn, lowered, timeout_ms, llvmEvalInheritedFd(&permit));
    }

    return forkAndEval(eval_fn, lowered, timeout_ms, null);
}

fn forkAndEvalWithStats(
    eval_fn: BackendEvalWithStatsFn,
    lowered: *const LoweredProgram,
) ForkStatsResult {
    if (comptime !has_fork or coverage_mode) {
        const result = eval_fn(std.heap.page_allocator, lowered) catch |err| {
            return .{ .child_error = @errorName(err) };
        };
        return .{ .success = result };
    }

    const disable_fork = std.c.getenv("ROC_EVAL_NO_FORK") != null;
    if (disable_fork) {
        const result = eval_fn(std.heap.page_allocator, lowered) catch |err| {
            return .{ .child_error = @errorName(err) };
        };
        return .{ .success = result };
    }

    const pipe_fds = harness.pipe() catch {
        return .{ .fork_failed = {} };
    };
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = harness.fork() catch {
        harness.closeFd(pipe_read);
        harness.closeFd(pipe_write);
        return .{ .fork_failed = {} };
    };

    if (fork_result == 0) {
        harness.closeFd(pipe_read);

        var child_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const child_alloc = child_arena.allocator();
        const result = eval_fn(child_alloc, lowered) catch |err| {
            harness.writeAll(pipe_write, @errorName(err));
            harness.closeFd(pipe_write);
            std.c._exit(2);
        };

        const header: [4]u8 = @bitCast(result.allocation_count);
        harness.writeAll(pipe_write, &header);
        harness.writeAll(pipe_write, result.output);

        harness.closeFd(pipe_write);
        std.c._exit(0);
    }

    harness.closeFd(pipe_write);

    var result_buf: std.ArrayListUnmanaged(u8) = .empty;
    var read_buf: [4096]u8 = undefined;
    var read_error = false;
    while (true) {
        const bytes_read = posix.read(pipe_read, &read_buf) catch {
            read_error = true;
            break;
        };
        if (bytes_read == 0) break;
        result_buf.appendSlice(std.heap.page_allocator, read_buf[0..bytes_read]) catch {
            read_error = true;
            break;
        };
    }
    harness.closeFd(pipe_read);

    const wait_result = harness.waitpid(fork_result, 0);
    const status = wait_result.status;
    const termination_signal: u8 = @truncate(status & 0x7f);

    if (termination_signal != 0) {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .signal_death = termination_signal };
    }

    const exit_code: u8 = @truncate((status >> 8) & 0xff);
    if (exit_code == 2) {
        const owned = result_buf.toOwnedSlice(std.heap.page_allocator) catch {
            result_buf.deinit(std.heap.page_allocator);
            return .{ .child_error = "ChildExecFailed" };
        };
        return .{ .child_error = owned };
    }
    if (exit_code != 0 or read_error or result_buf.items.len < 4) {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = "ChildExecFailed" };
    }

    const allocation_count: u32 = @bitCast(result_buf.items[0..4].*);
    const output = std.heap.page_allocator.dupe(u8, result_buf.items[4..]) catch {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = "ChildExecFailed" };
    };
    result_buf.deinit(std.heap.page_allocator);
    return .{ .success = .{
        .output = output,
        .allocation_count = allocation_count,
    } };
}

//
// Parse and canonicalize (shared by all backends)
//

//
// Test execution — unified interpreter + backend comparison
//

fn runSingleTest(allocator: std.mem.Allocator, tc: TestCase, timeout_ms: u64) TestOutcome {
    // If every backend is skipped, still validate the front-end so we catch
    // syntax errors in skipped tests rather than silently ignoring them.
    if (tc.skip.interpreter and tc.skip.dev and tc.skip.wasm and tc.skip.llvm) {
        const timings = switch (tc.expected) {
            .inspect_str => blk: {
                var compiled = helpers.compileInspectedProgram(allocator, app_io, tc.source_kind, tc.source, tc.imports) catch {
                    return .{
                        .status = .fail,
                        .message = "INVALID_SYNTAX — skipped inspect test has parse/check/lower errors",
                        .has_backend_details = false,
                        .backends = undefined,
                    };
                };
                defer compiled.deinit(allocator);
                break :blk EvalTimings{
                    .parse_ns = compiled.resources.parse_ns,
                    .canonicalize_ns = compiled.resources.canonicalize_ns,
                    .typecheck_ns = compiled.resources.typecheck_ns,
                };
            },
            .allocations_at_most => blk: {
                var compiled = helpers.compileProgram(allocator, app_io, tc.source_kind, tc.source, tc.imports) catch {
                    return .{
                        .status = .fail,
                        .message = "INVALID_SYNTAX — skipped allocation test has parse/check/lower errors",
                        .has_backend_details = false,
                        .backends = undefined,
                    };
                };
                defer compiled.deinit(allocator);
                break :blk EvalTimings{
                    .parse_ns = compiled.resources.parse_ns,
                    .canonicalize_ns = compiled.resources.canonicalize_ns,
                    .typecheck_ns = compiled.resources.typecheck_ns,
                };
            },
            .crash, .problem_and_crash => blk: {
                var compiled = helpers.compileInspectedProgram(allocator, app_io, tc.source_kind, tc.source, tc.imports) catch {
                    return .{
                        .status = .fail,
                        .message = "INVALID_SYNTAX — skipped crash test has parse/check/lower errors",
                        .has_backend_details = false,
                        .backends = undefined,
                    };
                };
                defer compiled.deinit(allocator);
                break :blk EvalTimings{
                    .parse_ns = compiled.resources.parse_ns,
                    .canonicalize_ns = compiled.resources.canonicalize_ns,
                    .typecheck_ns = compiled.resources.typecheck_ns,
                };
            },
            .problem => blk: {
                var resources = helpers.parseAndCheckProgramForProblems(allocator, tc.source_kind, tc.source, tc.imports) catch {
                    return .{
                        .status = .pass,
                        .timings = .{},
                        .has_backend_details = false,
                        .backends = undefined,
                    };
                };
                defer resources.deinit(allocator);
                break :blk EvalTimings{
                    .parse_ns = resources.main.parse_ns,
                    .canonicalize_ns = resources.main.canonicalize_ns,
                    .typecheck_ns = resources.main.typecheck_ns,
                };
            },
        };
        return .{
            .status = .skip,
            .timings = timings,
            .has_backend_details = false,
            .backends = undefined,
        };
    }

    var deadline = if (timeout_ms > 0) Deadline.init(timeout_ms) else null;
    const deadline_ptr: ?*Deadline = if (deadline) |*active| active else null;

    const outcome = runSingleTestInner(allocator, tc, deadline_ptr) catch |err| {
        return .{
            .status = .fail,
            .message = @errorName(err),
            .has_backend_details = false,
            .backends = undefined,
        };
    };

    // Any skipped backend means the test didn't get full coverage — report as skip.
    if (outcome.status == .pass and hasAnySkip(tc.skip)) {
        var backends: [NUM_BACKENDS]BackendDetail = undefined;
        if (outcome.has_backend_details) backends = outcome.backends;
        return .{
            .status = .skip,
            .message = outcome.message,
            .timings = outcome.timings,
            .has_backend_details = outcome.has_backend_details,
            .backends = backends,
        };
    }
    return outcome;
}

fn hasAnySkip(skip: TestCase.Skip) bool {
    return skip.interpreter or skip.dev or skip.wasm or skip.llvm;
}

fn shouldSkipLlvm(test_skip: bool) bool {
    return test_skip or !include_llvm_backend;
}

fn backendImplemented(index: usize) bool {
    return switch (index) {
        0 => true,
        1 => DEV_BACKEND_IMPLEMENTED,
        2 => WASM_BACKEND_IMPLEMENTED,
        3 => LLVM_BACKEND_IMPLEMENTED,
        else => unreachable,
    };
}

fn initBackendRows(skips: [NUM_BACKENDS]bool) [NUM_BACKENDS]BackendDetail {
    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    for (&backends, 0..) |*backend, i| {
        backend.* = if (!backendImplemented(i))
            .{ .status = .not_implemented }
        else if (skips[i])
            .{ .status = .skip }
        else
            .{ .status = .not_run };
    }
    return backends;
}

fn remainingBackendBudgetMs(deadline: ?*Deadline) u64 {
    const active = deadline orelse return 0;
    return active.remainingMs();
}

fn deadlineExpired(deadline: ?*Deadline) bool {
    return if (deadline) |active| active.expired() else false;
}

fn backendUsesStandardTimeout(index: usize) bool {
    return index != LLVM_BACKEND_INDEX;
}

fn backendTimeoutBudgetMs(index: usize, standard_deadline: ?*Deadline) u64 {
    if (standard_deadline == null) return 0;
    if (index == LLVM_BACKEND_INDEX) return LLVM_BACKEND_TIMEOUT_MS;
    return remainingBackendBudgetMs(standard_deadline);
}

fn runSingleTestInner(allocator: std.mem.Allocator, tc: TestCase, deadline: ?*Deadline) !TestOutcome {
    return switch (tc.expected) {
        .inspect_str => runInspectTest(allocator, tc.source_kind, tc.source, tc.imports, tc.expected, tc.skip, deadline),
        .allocations_at_most => |expected| runAllocationTest(allocator, tc.source_kind, tc.source, tc.imports, expected, tc.skip),
        .problem => runTestProblem(allocator, tc.source_kind, tc.source, tc.imports),
        .crash => runCrashTest(allocator, tc.source_kind, tc.source, tc.imports, tc.skip, false, deadline),
        .problem_and_crash => runCrashTest(allocator, tc.source_kind, tc.source, tc.imports, tc.skip, true, deadline),
    };
}

fn runAllocationTest(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
    expected: TestCase.AllocationExpectation,
    skip: TestCase.Skip,
) !TestOutcome {
    var compiled = try helpers.compileProgram(allocator, app_io, source_kind, src, imports);
    defer compiled.deinit(allocator);

    const timings = EvalTimings{
        .parse_ns = compiled.resources.parse_ns,
        .canonicalize_ns = compiled.resources.canonicalize_ns,
        .typecheck_ns = compiled.resources.typecheck_ns,
    };

    const skips = if (comptime coverage_mode)
        [NUM_BACKENDS]bool{ skip.interpreter, true, true, true }
    else
        [NUM_BACKENDS]bool{ skip.interpreter, skip.dev, skip.wasm, shouldSkipLlvm(skip.llvm) };

    const eval_fns = [NUM_BACKENDS]BackendEvalWithStatsFn{
        helpers.lirInterpreterStrWithStats,
        helpers.devEvaluatorStrWithStats,
        helpers.wasmEvaluatorStrWithStats,
        helpers.devEvaluatorStrWithStats, // llvm placeholder
    };

    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    var first_ok: ?[]const u8 = null;
    var any_failure = false;
    var first_message: ?[]const u8 = null;

    for (0..NUM_BACKENDS) |i| {
        if (i == 1 and !DEV_BACKEND_IMPLEMENTED) {
            backends[i] = .{ .status = .not_implemented };
            continue;
        }
        if (i == 2 and !WASM_BACKEND_IMPLEMENTED) {
            backends[i] = .{ .status = .not_implemented };
            continue;
        }
        if (i == 3 and !LLVM_BACKEND_IMPLEMENTED) {
            backends[i] = .{ .status = .not_implemented };
            continue;
        }
        if (skips[i]) {
            backends[i] = .{ .status = .skip };
            continue;
        }

        var timer = Timer.start() catch unreachable;
        const lowered = if (i == 2) &compiled.wasm_lowered else &compiled.lowered;
        const fork_result = forkAndEvalWithStats(eval_fns[i], lowered);
        const dur = timer.read();

        switch (fork_result) {
            .success => |result| {
                const value_ok = std.mem.eql(u8, expected.output, result.output);
                const agreement_ok = if (first_ok) |fok| std.mem.eql(u8, fok, result.output) else true;
                const allocation_ok = result.allocation_count <= expected.max_allocations;

                if (!value_ok or !agreement_ok) {
                    backends[i] = .{ .status = .wrong_value, .value = result.output, .duration_ns = dur };
                    if (first_message == null) {
                        first_message = try std.fmt.allocPrint(
                            allocator,
                            "{s} output mismatch: expected \"{s}\", got \"{s}\"",
                            .{ BACKEND_NAMES[i], expected.output, result.output },
                        );
                    }
                    any_failure = true;
                } else if (!allocation_ok) {
                    backends[i] = .{
                        .status = .fail,
                        .value = try std.fmt.allocPrint(allocator, "allocations: {d}", .{result.allocation_count}),
                        .duration_ns = dur,
                    };
                    if (first_message == null) {
                        first_message = try std.fmt.allocPrint(
                            allocator,
                            "{s} allocated {d} time(s), expected at most {d}",
                            .{ BACKEND_NAMES[i], result.allocation_count, expected.max_allocations },
                        );
                    }
                    any_failure = true;
                } else {
                    backends[i] = .{
                        .status = .pass,
                        .value = try std.fmt.allocPrint(allocator, "{s} (allocations: {d})", .{ result.output, result.allocation_count }),
                        .duration_ns = dur,
                    };
                    if (first_ok == null) first_ok = result.output;
                }
            },
            .child_error => |err_name| {
                backends[i] = .{ .status = .fail, .value = err_name, .duration_ns = dur };
                any_failure = true;
            },
            .signal_death => |sig| {
                var sig_buf: [32]u8 = undefined;
                const sig_str = std.fmt.bufPrint(&sig_buf, "signal: {d}", .{sig}) catch "signal: ?";
                backends[i] = .{ .status = .fail, .value = allocator.dupe(u8, sig_str) catch "signal", .duration_ns = dur };
                any_failure = true;
            },
            .fork_failed => {
                backends[i] = .{ .status = .fail, .value = "ForkFailed", .duration_ns = dur };
                any_failure = true;
            },
        }
    }

    const final_timings = EvalTimings{
        .parse_ns = timings.parse_ns,
        .canonicalize_ns = timings.canonicalize_ns,
        .typecheck_ns = timings.typecheck_ns,
        .interpreter_ns = backends[0].duration_ns,
        .dev_ns = backends[1].duration_ns,
        .wasm_ns = backends[2].duration_ns,
        .llvm_ns = backends[3].duration_ns,
    };

    return .{
        .status = if (any_failure) .fail else .pass,
        .message = first_message,
        .timings = final_timings,
        .has_backend_details = true,
        .backends = backends,
        .expected_str = expected.output,
    };
}

fn runInspectTest(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
    expected: TestCase.Expected,
    skip: TestCase.Skip,
    deadline: ?*Deadline,
) !TestOutcome {
    var compiled = try helpers.compileInspectedProgram(allocator, app_io, source_kind, src, imports);
    defer compiled.deinit(allocator);

    const timings = EvalTimings{
        .parse_ns = compiled.resources.parse_ns,
        .canonicalize_ns = compiled.resources.canonicalize_ns,
        .typecheck_ns = compiled.resources.typecheck_ns,
    };

    const display_expected = expected.display();
    const skips = if (comptime coverage_mode)
        [NUM_BACKENDS]bool{ skip.interpreter, true, true, true }
    else
        [NUM_BACKENDS]bool{ skip.interpreter, skip.dev, skip.wasm, shouldSkipLlvm(skip.llvm) };

    const eval_fns = [NUM_BACKENDS]BackendEvalFn{
        helpers.lirInterpreterInspectedStr,
        helpers.devEvaluatorInspectedStr,
        helpers.wasmEvaluatorInspectedStr,
        helpers.llvmEvaluatorInspectedStr,
    };

    var backends = initBackendRows(skips);
    var first_ok: ?[]const u8 = null;
    var any_failure = false;
    var any_timeout = false;

    for (0..NUM_BACKENDS) |i| {
        if (backends[i].status != .not_run) {
            continue;
        }
        if (backendUsesStandardTimeout(i) and deadlineExpired(deadline)) {
            backends[i] = .{ .status = .timeout };
            any_timeout = true;
            break;
        }

        trace.log("starting backend {s} for inspected source {s}", .{ BACKEND_NAMES[i], src });
        var timer = Timer.start() catch unreachable;
        const lowered = if (i == 2) &compiled.wasm_lowered else &compiled.lowered;
        const fork_result = runBackendEval(i, eval_fns[i], lowered, backendTimeoutBudgetMs(i, deadline)) catch |err|
            ForkResult{ .child_error = @errorName(err) };
        const dur = timer.read();
        trace.log("finished backend {s} for inspected source {s} in {d}ns", .{ BACKEND_NAMES[i], src, dur });

        switch (fork_result) {
            .success => |str| {
                const expected_str = switch (expected) {
                    .inspect_str => |value| value,
                    .allocations_at_most => unreachable,
                    .problem => unreachable,
                    .crash => unreachable,
                    .problem_and_crash => unreachable,
                };
                const value_ok = std.mem.eql(u8, expected_str, str);
                const agreement_ok = if (first_ok) |fok| std.mem.eql(u8, fok, str) else true;

                if (!value_ok or !agreement_ok) {
                    backends[i] = .{ .status = .wrong_value, .value = str, .duration_ns = dur };
                    any_failure = true;
                } else {
                    backends[i] = .{ .status = .pass, .value = str, .duration_ns = dur };
                    if (first_ok == null) first_ok = str;
                }
            },
            .child_error => |err_name| {
                backends[i] = .{ .status = .fail, .value = err_name, .duration_ns = dur };
                any_failure = true;
            },
            .timed_out => {
                backends[i] = .{ .status = .timeout, .duration_ns = dur };
                any_timeout = true;
                break;
            },
            .signal_death => |sig| {
                var sig_buf: [32]u8 = undefined;
                const sig_str = std.fmt.bufPrint(&sig_buf, "signal: {d}", .{sig}) catch "signal: ?";
                backends[i] = .{ .status = .fail, .value = allocator.dupe(u8, sig_str) catch "signal", .duration_ns = dur };
                any_failure = true;
            },
            .fork_failed => {
                backends[i] = .{ .status = .fail, .value = "ForkFailed", .duration_ns = dur };
                any_failure = true;
            },
        }
    }

    const final_timings = EvalTimings{
        .parse_ns = timings.parse_ns,
        .canonicalize_ns = timings.canonicalize_ns,
        .typecheck_ns = timings.typecheck_ns,
        .interpreter_ns = backends[0].duration_ns,
        .dev_ns = backends[1].duration_ns,
        .wasm_ns = backends[2].duration_ns,
        .llvm_ns = backends[3].duration_ns,
    };

    if (any_timeout) {
        return .{
            .status = .timeout,
            .timings = final_timings,
            .has_backend_details = true,
            .backends = backends,
            .expected_str = display_expected,
        };
    }
    if (any_failure) {
        return .{
            .status = .fail,
            .timings = final_timings,
            .has_backend_details = true,
            .backends = backends,
            .expected_str = display_expected,
        };
    }
    return .{
        .status = .pass,
        .timings = final_timings,
        .has_backend_details = true,
        .backends = backends,
    };
}

fn runTestProblem(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
) !TestOutcome {
    var timer = Timer.start() catch unreachable;
    var resources = helpers.parseAndCheckProgramForProblems(allocator, source_kind, src, imports) catch {
        // Parse or canonicalize error means a problem was found — that's a pass.
        const elapsed = timer.read();
        return .{
            .status = .pass,
            .timings = .{ .parse_ns = elapsed },
            .has_backend_details = false,
            .backends = undefined,
        };
    };
    defer resources.deinit(allocator);

    const can_diags = try resources.main.module_env.getDiagnostics();
    defer allocator.free(can_diags);
    const type_problems = resources.main.checker.problems.problems.items.len;
    const has_problems = can_diags.len + type_problems > 0;

    const timings = EvalTimings{
        .parse_ns = resources.main.parse_ns,
        .canonicalize_ns = resources.main.canonicalize_ns,
        .typecheck_ns = resources.main.typecheck_ns,
    };
    if (has_problems) {
        return .{
            .status = .pass,
            .timings = timings,
            .has_backend_details = false,
            .backends = undefined,
        };
    }
    return .{
        .status = .fail,
        .message = "expected problems but none found",
        .timings = timings,
        .has_backend_details = false,
        .backends = undefined,
    };
}

fn runCrashTest(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
    skip: TestCase.Skip,
    require_problems: bool,
    deadline: ?*Deadline,
) !TestOutcome {
    var compiled = try helpers.compileInspectedProgram(allocator, app_io, source_kind, src, imports);
    defer compiled.deinit(allocator);

    const can_diags = try compiled.resources.module_env.getDiagnostics();
    defer allocator.free(can_diags);
    const type_problems = compiled.resources.checker.problems.problems.items.len;
    var can_errors: usize = 0;
    for (can_diags) |diag| {
        if (canDiagnosticIsError(diag)) can_errors += 1;
    }
    const has_problems = can_errors + type_problems > 0;
    if (require_problems and !has_problems) {
        return .{
            .status = .fail,
            .message = "expected compile-time problems before runtime crash",
            .timings = .{
                .parse_ns = compiled.resources.parse_ns,
                .canonicalize_ns = compiled.resources.canonicalize_ns,
                .typecheck_ns = compiled.resources.typecheck_ns,
            },
            .has_backend_details = false,
            .backends = undefined,
        };
    }

    if (!require_problems and has_problems) {
        if (@import("builtin").mode == .Debug) {
            std.debug.print("runCrashTest compile-time problems:\n", .{});
            for (can_diags) |diag| {
                std.debug.print("  can: {s}\n", .{@tagName(diag)});
            }
            for (compiled.resources.checker.problems.problems.items) |problem| {
                std.debug.print("  type: {s}\n", .{@tagName(problem)});
            }
        }
        return .{
            .status = .fail,
            .message = "unexpected compile-time problems in runtime crash test",
            .timings = .{
                .parse_ns = compiled.resources.parse_ns,
                .canonicalize_ns = compiled.resources.canonicalize_ns,
                .typecheck_ns = compiled.resources.typecheck_ns,
            },
            .has_backend_details = false,
            .backends = undefined,
        };
    }

    const timings = EvalTimings{
        .parse_ns = compiled.resources.parse_ns,
        .canonicalize_ns = compiled.resources.canonicalize_ns,
        .typecheck_ns = compiled.resources.typecheck_ns,
    };

    const skips = if (comptime coverage_mode)
        [NUM_BACKENDS]bool{ skip.interpreter, true, true, true }
    else
        [NUM_BACKENDS]bool{ skip.interpreter, skip.dev, skip.wasm, shouldSkipLlvm(skip.llvm) };

    const eval_fns = [NUM_BACKENDS]BackendEvalFn{
        helpers.lirInterpreterInspectedStr,
        helpers.devEvaluatorInspectedStr,
        helpers.wasmEvaluatorInspectedStr,
        helpers.llvmEvaluatorInspectedStr,
    };

    var backends = initBackendRows(skips);
    var any_failure = false;
    var any_timeout = false;

    for (0..NUM_BACKENDS) |i| {
        if (backends[i].status != .not_run) {
            continue;
        }
        if (backendUsesStandardTimeout(i) and deadlineExpired(deadline)) {
            backends[i] = .{ .status = .timeout };
            any_timeout = true;
            break;
        }

        var timer = Timer.start() catch unreachable;
        const lowered = if (i == 2) &compiled.wasm_lowered else &compiled.lowered;
        const fork_result = runBackendEval(i, eval_fns[i], lowered, backendTimeoutBudgetMs(i, deadline)) catch |err|
            ForkResult{ .child_error = @errorName(err) };
        const dur = timer.read();

        switch (fork_result) {
            .child_error => |err_name| {
                if (std.mem.eql(u8, err_name, "Crash")) {
                    backends[i] = .{ .status = .pass, .value = err_name, .duration_ns = dur };
                } else {
                    backends[i] = .{ .status = .fail, .value = err_name, .duration_ns = dur };
                    any_failure = true;
                }
            },
            .success => |value| {
                backends[i] = .{ .status = .wrong_value, .value = value, .duration_ns = dur };
                any_failure = true;
            },
            .timed_out => {
                backends[i] = .{ .status = .timeout, .duration_ns = dur };
                any_timeout = true;
                break;
            },
            .signal_death => |sig| {
                var sig_buf: [32]u8 = undefined;
                const sig_str = std.fmt.bufPrint(&sig_buf, "signal: {d}", .{sig}) catch "signal: ?";
                backends[i] = .{ .status = .fail, .value = allocator.dupe(u8, sig_str) catch "signal", .duration_ns = dur };
                any_failure = true;
            },
            .fork_failed => {
                backends[i] = .{ .status = .fail, .value = "ForkFailed", .duration_ns = dur };
                any_failure = true;
            },
        }
    }

    const final_timings = EvalTimings{
        .parse_ns = timings.parse_ns,
        .canonicalize_ns = timings.canonicalize_ns,
        .typecheck_ns = timings.typecheck_ns,
        .interpreter_ns = backends[0].duration_ns,
        .dev_ns = backends[1].duration_ns,
        .wasm_ns = backends[2].duration_ns,
        .llvm_ns = backends[3].duration_ns,
    };

    if (any_timeout) {
        return .{
            .status = .timeout,
            .timings = final_timings,
            .has_backend_details = true,
            .backends = backends,
        };
    }
    if (any_failure) {
        return .{
            .status = .fail,
            .timings = final_timings,
            .has_backend_details = true,
            .backends = backends,
        };
    }
    return .{
        .status = .pass,
        .timings = final_timings,
        .has_backend_details = true,
        .backends = backends,
    };
}

fn canDiagnosticIsError(diag: anytype) bool {
    return switch (diag) {
        .shadowing_warning,
        .unused_variable,
        .used_underscore_variable,
        .type_shadowed_warning,
        .unused_type_var_name,
        .type_var_marked_unused,
        .underscore_in_type_declaration,
        .module_header_deprecated,
        .deprecated_number_suffix,
        => false,
        else => true,
    };
}

//
// Serialization — child-to-parent result protocol
//

/// Build the wire bytes for a TestOutcome into an in-memory buffer. Used by
/// both worker modes: one-shot streams the bytes directly to stdout, persistent
/// mode prefixes them with a u32 length so the parent can frame results.
fn serializeOutcomeToBuffer(
    buf: *std.ArrayListUnmanaged(u8),
    gpa: std.mem.Allocator,
    outcome: TestOutcome,
    duration_ns: u64,
) !void {
    var header: WireHeader = .{
        .status = @intFromEnum(outcome.status),
        .backend_statuses = undefined,
        .backend_durations = undefined,
        .parse_ns = outcome.timings.parse_ns,
        .canonicalize_ns = outcome.timings.canonicalize_ns,
        .typecheck_ns = outcome.timings.typecheck_ns,
        .interpreter_ns = outcome.timings.interpreter_ns,
        .dev_ns = outcome.timings.dev_ns,
        .wasm_ns = outcome.timings.wasm_ns,
        .llvm_ns = outcome.timings.llvm_ns,
        .duration_ns = duration_ns,
        .has_backend_details = if (outcome.has_backend_details) 1 else 0,
        .message_len = if (outcome.message) |m| @intCast(m.len) else 0,
        .expected_str_len = if (outcome.expected_str) |e| @intCast(e.len) else 0,
        .backend_value_lens = undefined,
    };
    if (outcome.has_backend_details) {
        for (0..NUM_BACKENDS) |i| {
            header.backend_statuses[i] = @intFromEnum(outcome.backends[i].status);
            header.backend_durations[i] = outcome.backends[i].duration_ns;
            header.backend_value_lens[i] = if (outcome.backends[i].value) |v| @intCast(v.len) else 0;
        }
    }

    try buf.appendSlice(gpa, std.mem.asBytes(&header));
    if (outcome.message) |m| try buf.appendSlice(gpa, m);
    if (outcome.expected_str) |e| try buf.appendSlice(gpa, e);
    if (outcome.has_backend_details) {
        for (outcome.backends) |bd| {
            if (bd.value) |v| try buf.appendSlice(gpa, v);
        }
    }
}

/// Serialize a TestOutcome to fd (one-shot worker mode, parent reads to EOF).
fn serializeOutcome(fd: posix.fd_t, outcome: TestOutcome, duration_ns: u64) void {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(std.heap.page_allocator);
    serializeOutcomeToBuffer(&buf, std.heap.page_allocator, outcome, duration_ns) catch return;
    harness.writeAll(fd, buf.items);
}

/// Serialize a TestOutcome to fd in stream mode: writes a `u32` length prefix
/// before the wire bytes so the parent can frame multiple results.
fn serializeOutcomeStreamed(fd: posix.fd_t, outcome: TestOutcome, duration_ns: u64) void {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(std.heap.page_allocator);
    serializeOutcomeToBuffer(&buf, std.heap.page_allocator, outcome, duration_ns) catch return;

    const length: u32 = @intCast(buf.items.len);
    harness.writeAll(fd, std.mem.asBytes(&length));
    harness.writeAll(fd, buf.items);
}

/// Deserialize a TestResult from an accumulated pipe buffer.
fn deserializeOutcome(buf: []const u8, gpa: std.mem.Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);

    const message = harness.readStr(buf, &offset, header.message_len, gpa);
    const expected_str = harness.readStr(buf, &offset, header.expected_str_len, gpa);

    const has_backend_details = header.has_backend_details != 0;
    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    if (has_backend_details) {
        for (0..NUM_BACKENDS) |i| {
            const value = harness.readStr(buf, &offset, header.backend_value_lens[i], gpa);
            backends[i] = .{
                .status = @enumFromInt(header.backend_statuses[i]),
                .value = value,
                .duration_ns = header.backend_durations[i],
            };
        }
    }

    return .{
        .status = @enumFromInt(header.status),
        .message = message,
        .duration_ns = header.duration_ns,
        .timings = .{
            .parse_ns = header.parse_ns,
            .canonicalize_ns = header.canonicalize_ns,
            .typecheck_ns = header.typecheck_ns,
            .interpreter_ns = header.interpreter_ns,
            .dev_ns = header.dev_ns,
            .wasm_ns = header.wasm_ns,
            .llvm_ns = header.llvm_ns,
        },
        .has_backend_details = has_backend_details,
        .backends = backends,
        .expected_str = expected_str,
    };
}

//
// Process pool (via harness)
//

/// Wrapper for the harness ProcessPool: runs a single test, captures timing,
/// and serializes via the eval wire protocol.
/// The "RUN <name>" log is emitted by the parent via `onTestStarted` (gated
/// on --verbose) so it stays coherent across N workers; see `Pool` config below.
fn runTestForPool(allocator: std.mem.Allocator, tc: TestCase, timeout_ms: u64) TestResult {
    var timer = Timer.start() catch unreachable;
    const outcome = runSingleTest(allocator, tc, timeout_ms);
    const duration = timer.read();
    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    if (outcome.has_backend_details) backends = outcome.backends;
    return .{
        .status = outcome.status,
        .message = outcome.message,
        .duration_ns = duration,
        .timings = outcome.timings,
        .has_backend_details = outcome.has_backend_details,
        .backends = backends,
        .expected_str = outcome.expected_str,
    };
}

fn onTestStarted(tc: TestCase) void {
    if (verbose_logging) std.debug.print("RUN  {s}\n", .{tc.name});
}

/// Restrict a `TestCase.Skip` to a single named backend for Phase-2 crash
/// attribution: when the parent sees a Phase-1 failure, it respawns the test
/// once per backend with `--worker-backend <name>` so we can pin down which
/// backend was responsible.
fn applyBackendIsolation(skip: *TestCase.Skip, name: []const u8) void {
    skip.interpreter = !std.mem.eql(u8, name, "interpreter");
    skip.dev = !std.mem.eql(u8, name, "dev");
    skip.wasm = !std.mem.eql(u8, name, "wasm");
    skip.llvm = !std.mem.eql(u8, name, "llvm");
}

/// Build argv used by the Windows ChildProcessPool to spawn worker copies of
/// this runner. Starts with `selfExePath`, then preserves every original arg
/// *except* `--worker N` / `--worker-backend NAME` (the harness appends those
/// per-worker; we strip any pre-existing instance so we don't double-add).
fn buildWorkerArgvTemplate(io: std.Io, arena: std.mem.Allocator, process_args: std.process.Args) ![]const []const u8 {
    // std.fs.selfExePath was removed in Zig 0.16; use std.process.executablePathAlloc instead.
    const self_path = try std.process.executablePathAlloc(io, arena);

    const raw = try process_args.toSlice(arena);
    const original_args: []const []const u8 = @ptrCast(raw);

    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    try argv.append(arena, self_path);

    var i: usize = 1;
    while (i < original_args.len) : (i += 1) {
        const arg = original_args[i];
        if (std.mem.eql(u8, arg, "--worker") or std.mem.eql(u8, arg, "--worker-backend")) {
            i += 1; // also skip the value
            continue;
        }
        if (std.mem.eql(u8, arg, "--worker-stream")) {
            continue; // no value
        }
        try argv.append(arena, arg);
    }

    return try argv.toOwnedSlice(arena);
}

/// Phase-2 retry: re-run each failing/crashing/timed-out test once per
/// backend with `--worker-backend <name>`, so we can attribute the crash.
/// Only runs on Windows; POSIX uses fork-per-backend already (forkAndEval).
///
/// The allocator passed here MUST match the one used by the cleanup loop in
/// main() (the GPA) so that gpa.free on bd.value in cleanup matches the
/// allocator that duped the bytes during deserialize.
fn retryFailedForAttribution(
    io: std.Io,
    gpa: std.mem.Allocator,
    results: []TestResult,
    worker_argv_template: []const []const u8,
    hang_timeout_ms: u64,
) void {
    for (results, 0..) |*r, idx| {
        const needs_retry = r.status == .fail or r.status == .crash or r.status == .timeout;
        if (!needs_retry) continue;

        var attributed: [NUM_BACKENDS]BackendDetail = undefined;
        if (r.has_backend_details) {
            attributed = r.backends;
        } else {
            for (&attributed) |*b| b.* = .{ .status = .fail };
        }

        for (BACKEND_NAMES, 0..) |name, bi| {
            // Skip backends that aren't implemented at compile time — no
            // point retrying. (When Phase-1 set has_backend_details=true,
            // these rows are already populated correctly; when it didn't,
            // the placeholder is .fail and we'd otherwise spuriously retry.)
            if (bi == 1 and !DEV_BACKEND_IMPLEMENTED) {
                attributed[bi] = .{ .status = .not_implemented };
                continue;
            }
            if (bi == 2 and !WASM_BACKEND_IMPLEMENTED) {
                attributed[bi] = .{ .status = .not_implemented };
                continue;
            }
            if (bi == 3 and !LLVM_BACKEND_IMPLEMENTED) {
                attributed[bi] = .{ .status = .not_implemented };
                continue;
            }
            if (bi == LLVM_BACKEND_INDEX and !include_llvm_backend) {
                attributed[bi] = .{ .status = .skip };
                continue;
            }
            // Skip backends that already passed cleanly in Phase 1.
            if (r.has_backend_details and attributed[bi].status == .pass) continue;
            // Skip backends marked NOT_IMPLEMENTED / SKIP up front.
            if (attributed[bi].status == .not_implemented or attributed[bi].status == .skip) continue;

            const outcome = Pool.spawnSingleWorker(
                io,
                gpa,
                worker_argv_template,
                idx,
                &.{ "--worker-backend", name },
                hang_timeout_ms,
            );
            // Dupe synthesized error strings into the gpa so the main()
            // cleanup loop (which frees `bd.value` via `gpa.free`) sees
            // uniformly gpa-owned strings and doesn't try to free a literal.
            attributed[bi] = switch (outcome) {
                .ok => |single| if (single.has_backend_details)
                    single.backends[bi]
                else
                    BackendDetail{ .status = .fail, .value = gpa.dupe(u8, "no detail returned") catch null },
                .crashed => BackendDetail{ .status = .fail, .value = gpa.dupe(u8, "isolated worker crashed") catch null },
                .timed_out => BackendDetail{ .status = .timeout },
            };
        }

        r.backends = attributed;
        r.has_backend_details = true;
        // r.status is intentionally not modified. The test did crash/fail in
        // Phase 1; Phase 2 only refines per-backend attribution.
    }
}

fn serializeResultForPool(fd: posix.fd_t, result: TestResult) void {
    // Re-pack into the existing wire format (outcome + duration).
    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    if (result.has_backend_details) backends = result.backends;
    const outcome = TestOutcome{
        .status = result.status,
        .message = result.message,
        .timings = result.timings,
        .has_backend_details = result.has_backend_details,
        .backends = backends,
        .expected_str = result.expected_str,
    };
    serializeOutcome(fd, outcome, result.duration_ns);
}

/// Streamed variant for persistent worker mode: writes a `u32` length prefix
/// before the wire bytes so the parent can frame multiple results sharing
/// the same stdout pipe.
fn serializeResultStreamed(fd: posix.fd_t, result: TestResult) void {
    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    if (result.has_backend_details) backends = result.backends;
    const outcome = TestOutcome{
        .status = result.status,
        .message = result.message,
        .timings = result.timings,
        .has_backend_details = result.has_backend_details,
        .backends = backends,
        .expected_str = result.expected_str,
    };
    serializeOutcomeStreamed(fd, outcome, result.duration_ns);
}

fn getTestName(tc: TestCase) []const u8 {
    return tc.name;
}

fn dupeOptional(gpa: std.mem.Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| gpa.dupe(u8, slice) catch null else null;
}

fn stabilizeResult(gpa: std.mem.Allocator, result: TestResult) TestResult {
    var stable_backends: [NUM_BACKENDS]BackendDetail = undefined;
    if (result.has_backend_details) {
        stable_backends = result.backends;
        for (&stable_backends) |*backend| {
            backend.value = dupeOptional(gpa, backend.value);
        }
    }

    return .{
        .status = result.status,
        .message = dupeOptional(gpa, result.message),
        .duration_ns = result.duration_ns,
        .timings = result.timings,
        .has_backend_details = result.has_backend_details,
        .backends = stable_backends,
        .expected_str = dupeOptional(gpa, result.expected_str),
    };
}

const default_result: TestResult = .{
    .status = .crash,
    .message = null,
    .duration_ns = 0,
    .timings = .{},
    .has_backend_details = false,
    .backends = undefined,
};
const timeout_result: TestResult = .{
    .status = .timeout,
    .message = null,
    .duration_ns = 0,
    .timings = .{},
    .has_backend_details = false,
    .backends = undefined,
};

const Pool = harness.ProcessPool(TestCase, TestResult, .{
    .runTest = &runTestForPool,
    .serialize = &serializeResultForPool,
    .deserialize = &deserializeOutcome,
    .default_result = default_result,
    .timeout_result = timeout_result,
    .stabilizeResult = &stabilizeResult,
    .getName = getTestName,
    // Backend children enforce the real backend timeout. The outer worker gets
    // enough extra time for the LLVM-only budget plus a short cleanup/reporting
    // window, so it can serialize the backend row that timed out instead of
    // being killed at the same instant.
    .timeout_report_grace_ms = LLVM_BACKEND_TIMEOUT_MS + BACKEND_TIMEOUT_REPORT_GRACE_MS,
    .onTestStarted = &onTestStarted,
});

//
// Test collection
//

fn collectTests() []const TestCase {
    return &eval_tests.tests;
}

//
// CLI parsing
//

// CLI parsing uses harness.parseStandardArgs for consistent flag handling.
// The eval runner accepts the standard flags: --filter, --threads, --timeout, --verbose, --help,
// plus `--llvm` and the positional marker `known-bugs` to include opt-in
// compiler-bug repros.

fn printHelp() void {
    const help =
        \\Roc Eval Test Runner
        \\
        \\Runs eval tests across enabled backends (interpreter, dev, wasm, and
        \\opt-in llvm) in parallel
        \\and compares results via Str.inspect. Each backend evaluation runs in
        \\a forked child process for crash isolation.
        \\
        \\USAGE:
        \\  zig build test-eval               Run with defaults.
        \\  zig build test-eval -- <OPTIONS>   Pass options (the -- is required
        \\                                     because zig build consumes flags
        \\                                     before the separator).
        \\  ./zig-out/bin/eval-test-runner [<OPTIONS>]
        \\
        \\OPTIONS:
        \\  -h, --help            Show this help message and exit.
        \\  --filter <PATTERN>    Run only tests whose name or source contains PATTERN.
        \\  --threads <N>         Max concurrent child processes (default: number of CPU cores).
        \\  --verbose             Print PASS and SKIP results (default: only FAIL/CRASH).
        \\  --timeout <MS>        Hang timeout in ms for parse/interp/dev/wasm.
        \\                        Default: 30000, 120000 on musl.
        \\                        LLVM uses a separate 420000ms backend budget.
        \\                        LLVM eval lock slots match the worker count.
        \\  --llvm                Include the LLVM backend. Default: skip LLVM.
        \\  known-bugs            Include opt-in known compiler-bug repro tests.
        \\
        \\COVERAGE:
        \\  Use `zig build coverage-eval` to build with coverage instrumentation.
        \\  This compiles with -Dcoverage=true, which at comptime: skips dev/wasm
        \\  backends (DCE), disables fork isolation, and forces single-threaded.
        \\  See CONTRIBUTING/eval_coverage.md for details.
        \\
        \\TIMING:
        \\  Every test is instrumented with per-phase monotonic timing (std.time.Timer):
        \\    parse    - builtin loading + source parsing
        \\    can      - canonicalization (CIR generation)
        \\    check    - type checking / constraint solving
        \\    interp   - interpreter evaluation
        \\    dev      - dev backend codegen + native execution
        \\    wasm     - wasm backend codegen + bytebox execution
        \\    llvm     - LLVM backend codegen + native execution
        \\
        \\  A performance summary table is printed after all tests with min, max,
        \\  mean, median, standard deviation, P95, and total for each phase, plus
        \\  the 5 slowest tests with full breakdowns.
        \\
        \\BACKEND COVERAGE:
        \\  The baseline goal is 100% of enabled backends testing 100% of tests.
        \\  Tests may use `skip = .{ .wasm = true }` etc. to disable specific
        \\  backends, but any test with a skip reports as SKIP rather than PASS
        \\  to keep partial coverage visible. LLVM coverage is opt-in via --llvm.
        \\
        \\  Test outcomes:
        \\    PASS  - all backends ran and agreed
        \\    FAIL  - value mismatch or backend disagreement
        \\    CRASH - segfault or panic in generated code (detected via fork isolation)
        \\    HANG  - test or backend exceeded the per-test timeout
        \\    SKIP  - one or more backends were skipped
        \\
        \\DEBUGGING:
        \\  Build with trace flags to get detailed per-operation output for filtered tests:
        \\
        \\    zig build test-eval -Dtrace-eval=true -- --filter "test name"
        \\      Traces the cor-style lowering pipeline and interpreter eval loop.
        \\      Shows each work item dispatched, low-level op executed, and continuation applied.
        \\
        \\    zig build test-eval -Dtrace-refcount=true -- --filter "test name"
        \\      Traces all refcount operations: alloc, dealloc, realloc, incref, decref, free.
        \\      Shows pointer addresses, sizes, and list/str metadata for each RC operation.
        \\
        \\  Both flags are comptime — they are compiled out when disabled (zero overhead).
        \\  Combine with --filter and --threads 1 for readable single-test output.
        \\
        \\EXIT CODE:
        \\  0 if all tests pass or skip, 1 if any test fails or crashes.
        \\
    ;
    std.debug.print("{s}", .{help});
}

/// Write per-backend detail lines for failed/crashed tests.
/// Format:
///   FAIL  test name  (92.2ms total)
///       expected:       16 : I64
///       interpreter:    PASS (12.0ms)
///       dev:            PASS (41.3ms)
///       wasm:           FAIL 'WasmExecFailed' (25.2ms)
///       llvm:           PASS (38.7ms)
fn writeFailureDetail(r: TestResult) void {
    if (r.expected_str) |es| {
        std.debug.print("        expected:       {s}\n", .{es});
    }
    if (!r.has_backend_details) {
        std.debug.print("        backend results: not produced; compilation/lowering did not complete\n", .{});
        return;
    }
    for (r.backends, 0..) |bd, i| {
        const name = BACKEND_NAMES[i];
        const ms = @as(f64, @floatFromInt(bd.duration_ns)) / 1_000_000.0;
        switch (bd.status) {
            .pass => {
                std.debug.print("        {s}:{s}PASS ({d:.1}ms)\n", .{ name, padding(name.len), ms });
            },
            .fail => {
                std.debug.print("        {s}:{s}FAIL", .{ name, padding(name.len) });
                if (bd.value) |v| std.debug.print(" '{s}'", .{v});
                if (bd.duration_ns > 0) std.debug.print(" ({d:.1}ms)", .{ms});
                std.debug.print("\n", .{});
            },
            .wrong_value => {
                std.debug.print("        {s}:{s}WRONG", .{ name, padding(name.len) });
                if (bd.value) |v| std.debug.print(" got '{s}'", .{v});
                if (bd.duration_ns > 0) std.debug.print(" ({d:.1}ms)", .{ms});
                std.debug.print("\n", .{});
            },
            .timeout => {
                std.debug.print("        {s}:{s}TIMEOUT", .{ name, padding(name.len) });
                if (bd.duration_ns > 0) std.debug.print(" ({d:.1}ms)", .{ms});
                std.debug.print("\n", .{});
            },
            .skip => std.debug.print("        {s}:{s}SKIP\n", .{ name, padding(name.len) }),
            .not_implemented => std.debug.print("        {s}:{s}NOT_IMPLEMENTED\n", .{ name, padding(name.len) }),
            .not_run => std.debug.print("        {s}:{s}NOT_RUN\n", .{ name, padding(name.len) }),
        }
    }
}

/// Right-pad backend name to align status columns.
fn padding(name_len: usize) []const u8 {
    const pad = "                "; // 16 spaces
    const target = 16; // "interpreter:" is 12 chars + 4 padding
    return if (name_len + 1 < target) pad[0 .. target - name_len - 1] else " ";
}

/// Write compact timing breakdown for PASS output (verbose mode).
fn writeTimingBreakdown(t: EvalTimings) void {
    std.debug.print("  [", .{});
    const fields = [_]struct { name: []const u8, ns: u64 }{
        .{ .name = "parse", .ns = t.parse_ns },
        .{ .name = "can", .ns = t.canonicalize_ns },
        .{ .name = "check", .ns = t.typecheck_ns },
        .{ .name = "interp", .ns = t.interpreter_ns },
        .{ .name = "dev", .ns = t.dev_ns },
        .{ .name = "wasm", .ns = t.wasm_ns },
        .{ .name = "llvm", .ns = t.llvm_ns },
    };
    var first = true;
    for (fields) |f| {
        if (f.ns > 0) {
            if (!first) std.debug.print(" ", .{});
            first = false;
            std.debug.print("{s}:{d:.1}", .{ f.name, @as(f64, @floatFromInt(f.ns)) / 1_000_000.0 });
        }
    }
    std.debug.print("]\n", .{});
}

//
// Statistics
//

const nsToMs = harness.nsToMs;
const computeTimingStats = harness.computeTimingStats;

fn printPerformanceSummary(gpa: std.mem.Allocator, tests: []const TestCase, results: []const TestResult) !void {
    // Collect per-phase timing arrays (only include tests that ran that phase, i.e. ns > 0)
    var parse_times: std.ArrayListUnmanaged(u64) = .empty;
    defer parse_times.deinit(gpa);
    var can_times: std.ArrayListUnmanaged(u64) = .empty;
    defer can_times.deinit(gpa);
    var check_times: std.ArrayListUnmanaged(u64) = .empty;
    defer check_times.deinit(gpa);
    var interp_times: std.ArrayListUnmanaged(u64) = .empty;
    defer interp_times.deinit(gpa);
    var dev_times: std.ArrayListUnmanaged(u64) = .empty;
    defer dev_times.deinit(gpa);
    var wasm_times: std.ArrayListUnmanaged(u64) = .empty;
    defer wasm_times.deinit(gpa);
    var llvm_times: std.ArrayListUnmanaged(u64) = .empty;
    defer llvm_times.deinit(gpa);

    for (results) |r| {
        const t = r.timings;
        if (t.parse_ns > 0) try parse_times.append(gpa, t.parse_ns);
        if (t.canonicalize_ns > 0) try can_times.append(gpa, t.canonicalize_ns);
        if (t.typecheck_ns > 0) try check_times.append(gpa, t.typecheck_ns);
        if (t.interpreter_ns > 0) try interp_times.append(gpa, t.interpreter_ns);
        if (t.dev_ns > 0) try dev_times.append(gpa, t.dev_ns);
        if (t.wasm_ns > 0) try wasm_times.append(gpa, t.wasm_ns);
        if (t.llvm_ns > 0) try llvm_times.append(gpa, t.llvm_ns);
    }

    std.debug.print("\n=== Performance Summary (ms) ===\n", .{});
    harness.printStatsHeader();
    harness.printStatsRow("parse", computeTimingStats(parse_times.items));
    harness.printStatsRow("can", computeTimingStats(can_times.items));
    harness.printStatsRow("check", computeTimingStats(check_times.items));
    harness.printStatsRow("interp", computeTimingStats(interp_times.items));
    harness.printStatsRow("dev", computeTimingStats(dev_times.items));
    harness.printStatsRow("wasm", computeTimingStats(wasm_times.items));
    harness.printStatsRow("llvm", computeTimingStats(llvm_times.items));

    // Slowest 5 tests by total duration
    const TopEntry = struct {
        idx: usize,
        duration_ns: u64,
    };
    var top_buf: std.ArrayListUnmanaged(TopEntry) = .empty;
    defer top_buf.deinit(gpa);
    for (results, 0..) |r, i| {
        try top_buf.append(gpa, .{ .idx = i, .duration_ns = r.duration_ns });
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
            const r = results[entry.idx];
            const tc = tests[entry.idx];
            const ms = nsToMs(r.duration_ns);
            std.debug.print("    {d}. {s}  ({d:.1}ms)", .{ rank, tc.name, ms });
            writeTimingBreakdown(r.timings);
        }
    }
}

//
// Main
//

/// Worker boot-path instrumentation. Set the env var `ROC_EVAL_TIME_WORKER=1`
/// to dump per-phase timestamps from inside a worker process. Used to figure
/// out where the ~70ms per-Child overhead is going on Windows; disabled by
/// default so it adds no cost.
const WorkerTrace = struct {
    enabled: bool,
    start_ns: u64,
    last_ns: u64,

    fn init() WorkerTrace {
        const enabled = blk: {
            const v = std.c.getenv("ROC_EVAL_TIME_WORKER");
            if (v) |_| {
                break :blk true;
            }
            break :blk false;
        };
        const now = std.Io.Timestamp.now(std.Options.debug_io, .real).nanoseconds;
        const start_ns: u64 = @intCast(@max(0, now));
        return .{ .enabled = enabled, .start_ns = start_ns, .last_ns = start_ns };
    }

    fn stamp(self: *WorkerTrace, label: []const u8) void {
        if (!self.enabled) return;
        const now: u64 = @intCast(@max(0, std.Io.Timestamp.now(std.Options.debug_io, .real).nanoseconds));
        const since_start_us = (now -| self.start_ns) / 1_000;
        const since_last_us = (now -| self.last_ns) / 1_000;
        self.last_ns = now;
        std.debug.print("[worker {d}us +{d}us] {s}\n", .{ since_start_us, since_last_us, label });
    }
};

fn effectiveHangTimeoutMs(cli: harness.StandardArgs, max_children: usize) u64 {
    if (cli.timeout_provided and cli.timeout_ms > 0) return cli.timeout_ms;
    if (builtin.abi == .musl) return 120_000;
    return if (max_children <= 1) 10_000 else 30_000;
}

fn effectiveMaxChildren(cli: harness.StandardArgs, cpu_count: usize, test_count: usize) usize {
    const requested = cli.max_threads orelse @min(cpu_count, test_count);
    return @max(1, requested);
}

fn hasPositionalArg(args: []const []const u8, target: []const u8) bool {
    for (args) |arg| {
        if (std.mem.eql(u8, arg, target)) return true;
    }
    return false;
}

/// Entry point for the parallel eval test runner.
pub fn main(init: std.process.Init) !void {
    var trace_worker = WorkerTrace.init();
    trace_worker.stamp("main entry");

    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    trace_worker.stamp("gpa init");

    const io = init.io;
    app_io = io;

    var args_arena = std.heap.ArenaAllocator.init(gpa);
    defer args_arena.deinit();
    const cli = try harness.parseStandardArgs(args_arena.allocator(), init.minimal.args);
    trace_worker.stamp("parseStandardArgs");

    if (cli.help_requested) {
        printHelp();
        return;
    }

    verbose_logging = cli.verbose;
    include_llvm_backend = cli.include_llvm;

    const all_tests = collectTests();
    trace_worker.stamp("collectTests");
    const include_known_bugs = hasPositionalArg(cli.positional, "known-bugs");

    // Apply filters (support multiple --filter values)
    var filtered_buf: std.ArrayListUnmanaged(TestCase) = .empty;
    defer filtered_buf.deinit(gpa);

    if (cli.filters.len > 0) {
        for (all_tests) |tc| {
            if (tc.known_bug and !include_known_bugs) continue;
            for (cli.filters) |pattern| {
                if (std.mem.find(u8, tc.name, pattern) != null or
                    std.mem.find(u8, tc.source, pattern) != null)
                {
                    try filtered_buf.append(gpa, tc);
                    break;
                }
            }
        }
    } else {
        for (all_tests) |tc| {
            if (tc.known_bug and !include_known_bugs) continue;
            try filtered_buf.append(gpa, tc);
        }
    }
    trace_worker.stamp("filter pass");

    const tests = filtered_buf.items;
    if (tests.len == 0) {
        if (cli.filters.len == 0) {
            std.debug.print("No eval tests found.\n", .{});
        }
        return;
    }

    const cpu_count = std.Thread.getCpuCount() catch 1;
    const max_children: usize = effectiveMaxChildren(cli, cpu_count, tests.len);
    llvm_eval_slot_count = max_children;

    // Worker mode: the parent spawned us with `--worker <idx>` (and optionally
    // `--worker-backend <name>`) to run a single test, serialize the result to
    // stdout, and exit. Used on Windows where the harness runs N worker
    // processes in parallel instead of forking. The child re-applies the same
    // filters so idx is stable between parent and child.
    if (cli.worker_index) |idx| {
        if (idx >= tests.len) std.process.exit(2);
        var tc = tests[idx];
        if (cli.worker_backend) |name| applyBackendIsolation(&tc.skip, name);
        const worker_timeout_ms: u64 = if (cli.timeout_provided and cli.timeout_ms > 0) cli.timeout_ms else 30_000;

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        trace_worker.stamp("pre runSingleTest");
        var timer = Timer.start() catch unreachable;
        const outcome = runSingleTest(arena.allocator(), tc, worker_timeout_ms);
        const duration = timer.read();
        trace_worker.stamp("post runSingleTest");
        var backends: [NUM_BACKENDS]BackendDetail = undefined;
        if (outcome.has_backend_details) backends = outcome.backends;
        const result = TestResult{
            .status = outcome.status,
            .message = outcome.message,
            .duration_ns = duration,
            .timings = outcome.timings,
            .has_backend_details = outcome.has_backend_details,
            .backends = backends,
            .expected_str = outcome.expected_str,
        };
        serializeResultForPool(harness.stdoutFd(), result);
        trace_worker.stamp("serialize done");
        return;
    }

    // Persistent worker mode: read test indices from stdin (one decimal per
    // line), run each, write a u32-length-prefixed result to stdout, loop
    // until stdin EOFs. Amortizes the per-Child process-boot cost across
    // many tests on the same worker.
    if (cli.worker_stream) {
        const worker_timeout_ms: u64 = if (cli.timeout_provided and cli.timeout_ms > 0) cli.timeout_ms else 30_000;
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const stdout_handle = harness.stdoutFd();
        const stdin_handle = harness.stdinFd();

        var line_buf: [32]u8 = undefined;
        outer: while (true) {
            var line_len: usize = 0;
            while (true) {
                if (line_len >= line_buf.len) break :outer; // malformed
                const n = harness.posixRead(stdin_handle, line_buf[line_len .. line_len + 1]) catch break :outer;
                if (n == 0) break :outer; // EOF — parent done
                if (line_buf[line_len] == '\n') break;
                line_len += 1;
            }
            const idx = std.fmt.parseInt(usize, line_buf[0..line_len], 10) catch continue;
            if (idx >= tests.len) continue;

            _ = arena.reset(.retain_capacity);

            var timer = Timer.start() catch unreachable;
            const outcome = runSingleTest(arena.allocator(), tests[idx], worker_timeout_ms);
            const duration = timer.read();
            var backends: [NUM_BACKENDS]BackendDetail = undefined;
            if (outcome.has_backend_details) backends = outcome.backends;
            const result = TestResult{
                .status = outcome.status,
                .message = outcome.message,
                .duration_ns = duration,
                .timings = outcome.timings,
                .has_backend_details = outcome.has_backend_details,
                .backends = backends,
                .expected_str = outcome.expected_str,
            };
            serializeResultStreamed(stdout_handle, result);
        }
        return;
    }

    // In Zig 0.16, std.process.getEnvVarOwned was removed.
    // Use Environ.getPosix on POSIX (no alloc). On Windows, the matching
    // getPosix variant currently compiles incorrectly in the stdlib, and the fork
    // path doesn't apply anyway (Windows uses the Child pool), so the
    // env var has no effect there.
    const disable_fork_env: ?[:0]const u8 = if (builtin.os.tag == .windows)
        null
    else
        std.process.Environ.getPosix(init.minimal.environ, "ROC_EVAL_NO_FORK");

    // Coverage mode and ROC_EVAL_NO_FORK use a simple single-threaded loop: no
    // outer fork, no watchdog, no threads. ROC_EVAL_NO_FORK is also consumed by
    // forkAndEval below, so backend calls run in-process too.
    if (coverage_mode or disable_fork_env != null) {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        var passed: usize = 0;
        var failed: usize = 0;
        var skipped: usize = 0;
        var wall_timer = Timer.start() catch unreachable;

        for (tests, 0..) |tc, i| {
            _ = arena.reset(.retain_capacity);

            const outcome = runSingleTest(arena.allocator(), tc, 0);

            switch (outcome.status) {
                .pass => passed += 1,
                .skip => skipped += 1,
                else => {
                    failed += 1;
                    std.debug.print("  FAIL  {s}", .{tc.name});
                    if (outcome.message) |msg| std.debug.print(": {s}", .{msg});
                    std.debug.print("\n", .{});
                },
            }

            // Overwrite progress line in-place.
            std.debug.print("\r  [{d}/{d}]", .{ i + 1, tests.len });
        }
        std.debug.print("\n", .{});

        const wall_ms = @as(f64, @floatFromInt(wall_timer.read())) / 1_000_000.0;
        std.debug.print("\n{d} passed, {d} failed, {d} skipped ({d} total) in {d:.0}ms\n", .{
            passed, failed, skipped, tests.len, wall_ms,
        });
        return;
    }

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    for (results) |*result| {
        result.* = default_result;
    }

    var wall_timer = Timer.start() catch unreachable;

    // Native musl CI has enough process-startup variance for the larger shared
    // harness default to be more reliable, especially for heavy boundary tests.
    const hang_timeout_ms: u64 = effectiveHangTimeoutMs(cli, max_children);

    // Build a worker_argv_template so Windows can spawn `Child` workers that
    // re-invoke this binary with `--worker <idx>`. On POSIX the template is
    // unused (fork path doesn't re-exec) but we build it uniformly.
    const worker_argv_template = try buildWorkerArgvTemplate(io, args_arena.allocator(), init.minimal.args);

    Pool.run(io, tests, results, max_children, hang_timeout_ms, gpa, worker_argv_template);

    // Phase-2 retry: on Windows, a Phase-1 worker that crashed kills the
    // whole worker before per-backend details land in the wire payload. For
    // any failing/crashing/timed-out test, respawn it once per backend with
    // `--worker-backend <name>` to attribute the crash. Common (passing) case
    // pays zero retry cost. Skipped on POSIX where forkAndEval already
    // attributes crashes per-backend within the worker.
    if (builtin.os.tag == .windows) {
        retryFailedForAttribution(io, gpa, results, worker_argv_template, hang_timeout_ms);
    }

    const wall_elapsed = wall_timer.read();

    var passed: usize = 0;
    var failed: usize = 0;
    var crashed: usize = 0;
    var skipped: usize = 0;
    var timed_out: usize = 0;

    std.debug.print("\n=== Eval Test Results ===\n", .{});

    for (tests, 0..) |tc, i| {
        const r = results[i];
        const ms = @as(f64, @floatFromInt(r.duration_ns)) / 1_000_000.0;
        const t = r.timings;

        switch (r.status) {
            .pass => {
                passed += 1;
                if (cli.verbose) {
                    std.debug.print("  PASS  {s}  ({d:.1}ms)", .{ tc.name, ms });
                    writeTimingBreakdown(t);
                }
            },
            .fail => {
                failed += 1;
                std.debug.print("  FAIL  {s}  ({d:.1}ms total)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                writeFailureDetail(r);
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms total)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                writeFailureDetail(r);
            },
            .timeout => {
                timed_out += 1;
                std.debug.print("  HANG  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                writeFailureDetail(r);
            },
            .skip => {
                skipped += 1;
                if (cli.verbose) {
                    std.debug.print("  SKIP  {s}\n", .{tc.name});
                }
            },
        }
    }

    // Free GPA-duped messages
    for (results) |r| {
        if (r.message) |msg| {
            gpa.free(msg);
        }
        if (r.has_backend_details) {
            for (r.backends) |bd| {
                if (bd.value) |v| gpa.free(v);
            }
        }
        if (r.expected_str) |es| gpa.free(es);
    }

    if (tests.len > 0) {
        printPerformanceSummary(gpa, tests, results) catch {};
    }

    const wall_ms = @as(f64, @floatFromInt(wall_elapsed)) / 1_000_000.0;
    if (timed_out > 0) {
        std.debug.print("\n{d} passed, {d} failed, {d} crashed, {d} hung, {d} skipped ({d} total) in {d:.0}ms using {d} process(es)\n", .{
            passed, failed, crashed, timed_out, skipped, tests.len, wall_ms, max_children,
        });
    } else {
        std.debug.print("\n{d} passed, {d} failed, {d} crashed, {d} skipped ({d} total) in {d:.0}ms using {d} process(es)\n", .{
            passed, failed, crashed, skipped, tests.len, wall_ms, max_children,
        });
    }

    if (failed > 0 or crashed > 0 or timed_out > 0) {
        std.process.exit(1);
    }
}
