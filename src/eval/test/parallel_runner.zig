//! Parallel eval test runner.
//!
//! Runs eval tests in parallel using a fork-based process pool, exercising
//! every backend on every test case and comparing their results via
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
//!   4. **LLVM backend** — currently not implemented for statement-only LIR.
//!
//! ALL backends run via Str.inspect and must produce identical output strings.
//! This catches bugs where a backend produces a value of the right type but
//! wrong content.
//!
//! ## Process pool
//!
//! A single-threaded parent process manages up to N concurrent child
//! processes (one per test). Each child runs the full test pipeline
//! (frontend + all backend evaluations) and serializes results back
//! through a pipe. The parent multiplexes pipe reads using poll().
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
//! longer than the timeout (default 30s), the parent SIGKILLs it. No
//! separate watchdog thread is needed.
//!
//! ## Usage
//!
//!   zig build test-eval [-- [--filter <pattern>] [--threads <N>] [--timeout <ms>] [--verbose]]

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

    pub const Expected = union(enum) {
        inspect_str: []const u8,
        problem: void,
        crash: void,
        problem_and_crash: void,

        pub fn display(self: Expected) ?[]const u8 {
            return switch (self) {
                .inspect_str => |value| value,
                .problem => null,
                .crash => null,
                .problem_and_crash => null,
            };
        }
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

    const Status = enum { pass, fail, wrong_value, skip, not_implemented };
};

const NUM_BACKENDS = 4; // interpreter, dev, wasm, llvm
const BACKEND_NAMES = [NUM_BACKENDS][]const u8{ "interpreter", "dev", "wasm", "llvm" };
const WASM_BACKEND_IMPLEMENTED = true;
const LLVM_BACKEND_IMPLEMENTED = false;

const TestOutcome = struct {
    status: Status,
    message: ?[]const u8 = null,
    timings: EvalTimings = .{},
    /// Per-backend details (interpreter, dev, wasm, llvm). Populated by inspect-string execution.
    backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{.{ .status = .not_implemented }} ** NUM_BACKENDS,
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
    backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{.{ .status = .not_implemented }} ** NUM_BACKENDS,
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
    message_len: u32,
    expected_str_len: u32,
    backend_value_lens: [NUM_BACKENDS]u32,
};

//
// Fork-based process isolation for backend evaluation
//

const has_fork = builtin.os.tag != .windows;

const BackendEvalFn = *const fn (std.mem.Allocator, *const LoweredProgram) anyerror![]u8;

/// Result of a forked backend evaluation.
const ForkResult = union(enum) {
    /// Child exited 0 and wrote result string to pipe.
    success: []const u8,
    /// Child exited non-zero (eval function returned an error).
    child_error: []const u8,
    /// Child was killed by a signal (e.g. SIGSEGV=11, SIGKILL=9).
    signal_death: u8,
    /// fork() or pipe() syscall failed.
    fork_failed: void,
};

/// Fork a child process to evaluate a backend, communicating the result via pipe.
///
/// The child calls `eval_fn(page_allocator, module_env, expr_idx, builtin_env)`,
/// writes the resulting string to the pipe, and `_exit(0)`. On error it `_exit(1)`.
///
/// The parent reads the pipe until EOF (important: before waitpid to avoid pipe
/// buffer deadlock), then reaps the child.
fn forkAndEval(
    eval_fn: BackendEvalFn,
    lowered: *const LoweredProgram,
) ForkResult {
    if (comptime !has_fork or coverage_mode) {
        const result = eval_fn(std.heap.page_allocator, lowered) catch |err| {
            return .{ .child_error = @errorName(err) };
        };
        return .{ .success = result };
    }

    const pipe_fds = posix.pipe() catch {
        return .{ .fork_failed = {} };
    };
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = posix.fork() catch {
        posix.close(pipe_read);
        posix.close(pipe_write);
        return .{ .fork_failed = {} };
    };

    if (fork_result == 0) {
        // === Child process ===
        posix.close(pipe_read);

        // Arena batches allocations into fewer mmap calls; child _exit()s
        // immediately so the OS reclaims everything — no deinit needed.
        var child_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const child_alloc = child_arena.allocator();
        const result_str = eval_fn(child_alloc, lowered) catch |err| {
            // Write error name to pipe so parent can report it, then exit 2
            // to distinguish "error with name" from other failures.
            const name = @errorName(err);
            var w: usize = 0;
            while (w < name.len) {
                w += posix.write(pipe_write, name[w..]) catch break;
            }
            posix.close(pipe_write);
            std.c._exit(2);
        };

        // Write the result string to the pipe.
        var written: usize = 0;
        while (written < result_str.len) {
            written += posix.write(pipe_write, result_str[written..]) catch {
                posix.close(pipe_write);
                std.c._exit(1);
            };
        }

        posix.close(pipe_write);
        std.c._exit(0);
    }

    // === Parent process ===
    posix.close(pipe_write);

    // Read pipe FIRST (before waitpid) to avoid deadlock when child output
    // exceeds the pipe buffer (~64KB). The read returns EOF when the child
    // exits and the write end is closed.
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
    posix.close(pipe_read);

    // Now reap the child.
    const wait_result = posix.waitpid(fork_result, 0);

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
    if (exit_code != 0 or read_error) {
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

//
// Parse and canonicalize (shared by all backends)
//

//
// Test execution — unified interpreter + backend comparison
//

fn runSingleTest(allocator: std.mem.Allocator, tc: TestCase) TestOutcome {
    // If every backend is skipped, still validate the front-end so we catch
    // syntax errors in skipped tests rather than silently ignoring them.
    if (tc.skip.interpreter and tc.skip.dev and tc.skip.wasm) {
        const timings = switch (tc.expected) {
            .inspect_str => blk: {
                var compiled = helpers.compileInspectedProgram(allocator, tc.source_kind, tc.source, tc.imports) catch {
                    return .{ .status = .fail, .message = "INVALID_SYNTAX — skipped inspect test has parse/check/lower errors" };
                };
                defer compiled.deinit(allocator);
                break :blk EvalTimings{
                    .parse_ns = compiled.resources.parse_ns,
                    .canonicalize_ns = compiled.resources.canonicalize_ns,
                    .typecheck_ns = compiled.resources.typecheck_ns,
                };
            },
            .crash, .problem_and_crash => blk: {
                var compiled = helpers.compileInspectedProgram(allocator, tc.source_kind, tc.source, tc.imports) catch {
                    return .{ .status = .fail, .message = "INVALID_SYNTAX — skipped crash test has parse/check/lower errors" };
                };
                defer compiled.deinit(allocator);
                break :blk EvalTimings{
                    .parse_ns = compiled.resources.parse_ns,
                    .canonicalize_ns = compiled.resources.canonicalize_ns,
                    .typecheck_ns = compiled.resources.typecheck_ns,
                };
            },
            .problem => blk: {
                const resources = helpers.parseAndCanonicalizeProgram(allocator, tc.source_kind, tc.source, tc.imports) catch {
                    return .{ .status = .pass, .timings = .{} };
                };
                defer helpers.cleanupParseAndCanonical(allocator, resources);
                break :blk EvalTimings{
                    .parse_ns = resources.parse_ns,
                    .canonicalize_ns = resources.canonicalize_ns,
                    .typecheck_ns = resources.typecheck_ns,
                };
            },
        };
        return .{ .status = .skip, .timings = timings };
    }

    const outcome = runSingleTestInner(allocator, tc) catch |err| {
        return .{ .status = .fail, .message = @errorName(err) };
    };

    // Any skipped backend means the test didn't get full coverage — report as skip.
    if (outcome.status == .pass and hasAnySkip(tc.skip)) {
        return .{ .status = .skip, .message = outcome.message, .timings = outcome.timings };
    }
    return outcome;
}

fn hasAnySkip(skip: TestCase.Skip) bool {
    return skip.interpreter or skip.dev or skip.wasm or skip.llvm;
}

fn runSingleTestInner(allocator: std.mem.Allocator, tc: TestCase) !TestOutcome {
    return switch (tc.expected) {
        .inspect_str => runInspectTest(allocator, tc.source_kind, tc.source, tc.imports, tc.expected, tc.skip),
        .problem => runTestProblem(allocator, tc.source_kind, tc.source, tc.imports),
        .crash => runCrashTest(allocator, tc.source_kind, tc.source, tc.imports, tc.skip, false),
        .problem_and_crash => runCrashTest(allocator, tc.source_kind, tc.source, tc.imports, tc.skip, true),
    };
}

fn runInspectTest(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
    expected: TestCase.Expected,
    skip: TestCase.Skip,
) !TestOutcome {
    var compiled = try helpers.compileInspectedProgram(allocator, source_kind, src, imports);
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
        [NUM_BACKENDS]bool{ skip.interpreter, skip.dev, skip.wasm, false };

    const eval_fns = [NUM_BACKENDS]BackendEvalFn{
        helpers.lirInterpreterInspectedStr,
        helpers.devEvaluatorInspectedStr,
        helpers.wasmEvaluatorInspectedStr,
        helpers.devEvaluatorInspectedStr, // llvm placeholder
    };

    var backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{.{ .status = .not_implemented }} ** NUM_BACKENDS;
    var first_ok: ?[]const u8 = null;
    var any_failure = false;

    for (0..NUM_BACKENDS) |i| {
        if (i == 2 and !WASM_BACKEND_IMPLEMENTED) continue;
        if (i == 3 and !LLVM_BACKEND_IMPLEMENTED) continue;
        if (skips[i]) {
            backends[i] = .{ .status = .skip };
            continue;
        }

        trace.log("starting backend {s} for inspected source {s}", .{ BACKEND_NAMES[i], src });
        var timer = Timer.start() catch unreachable;
        const fork_result = forkAndEval(eval_fns[i], &compiled.lowered);
        const dur = timer.read();
        trace.log("finished backend {s} for inspected source {s} in {d}ns", .{ BACKEND_NAMES[i], src, dur });

        switch (fork_result) {
            .success => |str| {
                const expected_str = switch (expected) {
                    .inspect_str => |value| value,
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

    if (any_failure) {
        return .{ .status = .fail, .timings = final_timings, .backends = backends, .expected_str = display_expected };
    }
    return .{ .status = .pass, .timings = final_timings, .backends = backends };
}

fn runTestProblem(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
) !TestOutcome {
    var timer = Timer.start() catch unreachable;
    const resources = helpers.parseAndCanonicalizeProgram(allocator, source_kind, src, imports) catch {
        // Parse or canonicalize error means a problem was found — that's a pass.
        const elapsed = timer.read();
        return .{ .status = .pass, .timings = .{ .parse_ns = elapsed } };
    };
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    const can_diags = try resources.module_env.getDiagnostics();
    defer allocator.free(can_diags);
    const type_problems = resources.checker.problems.problems.items.len;
    const has_problems = can_diags.len + type_problems > 0;

    const timings = EvalTimings{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
    };
    if (has_problems) {
        return .{ .status = .pass, .timings = timings };
    }
    return .{ .status = .fail, .message = "expected problems but none found", .timings = timings };
}

fn runCrashTest(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    src: []const u8,
    imports: []const helpers.ModuleSource,
    skip: TestCase.Skip,
    require_problems: bool,
) !TestOutcome {
    var compiled = try helpers.compileInspectedProgram(allocator, source_kind, src, imports);
    defer compiled.deinit(allocator);

    const can_diags = try compiled.resources.module_env.getDiagnostics();
    defer allocator.free(can_diags);
    const type_problems = compiled.resources.checker.problems.problems.items.len;
    const has_problems = can_diags.len + type_problems > 0;

    if (require_problems and !has_problems) {
        return .{
            .status = .fail,
            .message = "expected compile-time problems before runtime crash",
            .timings = .{
                .parse_ns = compiled.resources.parse_ns,
                .canonicalize_ns = compiled.resources.canonicalize_ns,
                .typecheck_ns = compiled.resources.typecheck_ns,
            },
        };
    }

    if (!require_problems and has_problems) {
        return .{
            .status = .fail,
            .message = "unexpected compile-time problems in runtime crash test",
            .timings = .{
                .parse_ns = compiled.resources.parse_ns,
                .canonicalize_ns = compiled.resources.canonicalize_ns,
                .typecheck_ns = compiled.resources.typecheck_ns,
            },
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
        [NUM_BACKENDS]bool{ skip.interpreter, skip.dev, skip.wasm, false };

    const eval_fns = [NUM_BACKENDS]BackendEvalFn{
        helpers.lirInterpreterInspectedStr,
        helpers.devEvaluatorInspectedStr,
        helpers.wasmEvaluatorInspectedStr,
        helpers.devEvaluatorInspectedStr, // llvm placeholder
    };

    var backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{.{ .status = .not_implemented }} ** NUM_BACKENDS;
    var any_failure = false;

    for (0..NUM_BACKENDS) |i| {
        if (i == 2 and !WASM_BACKEND_IMPLEMENTED) continue;
        if (i == 3 and !LLVM_BACKEND_IMPLEMENTED) continue;
        if (skips[i]) {
            backends[i] = .{ .status = .skip };
            continue;
        }

        var timer = Timer.start() catch unreachable;
        const fork_result = forkAndEval(eval_fns[i], &compiled.lowered);
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

    if (any_failure) {
        return .{ .status = .fail, .timings = final_timings, .backends = backends };
    }
    return .{ .status = .pass, .timings = final_timings, .backends = backends };
}

//
// Serialization — child-to-parent result protocol
//

/// Serialize a TestOutcome + duration to a pipe file descriptor.
/// Called in child process after runSingleTest returns.
fn serializeOutcome(fd: posix.fd_t, outcome: TestOutcome, duration_ns: u64) void {
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
        .message_len = if (outcome.message) |m| @intCast(m.len) else 0,
        .expected_str_len = if (outcome.expected_str) |e| @intCast(e.len) else 0,
        .backend_value_lens = undefined,
    };
    for (0..NUM_BACKENDS) |i| {
        header.backend_statuses[i] = @intFromEnum(outcome.backends[i].status);
        header.backend_durations[i] = outcome.backends[i].duration_ns;
        header.backend_value_lens[i] = if (outcome.backends[i].value) |v| @intCast(v.len) else 0;
    }

    // Write header
    harness.writeAll(fd, std.mem.asBytes(&header));

    // Write variable-length strings
    if (outcome.message) |m| harness.writeAll(fd, m);
    if (outcome.expected_str) |e| harness.writeAll(fd, e);
    for (outcome.backends) |bd| {
        if (bd.value) |v| harness.writeAll(fd, v);
    }
}

/// Deserialize a TestResult from an accumulated pipe buffer.
fn deserializeOutcome(buf: []const u8, gpa: std.mem.Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);

    const message = harness.readStr(buf, &offset, header.message_len, gpa);
    const expected_str = harness.readStr(buf, &offset, header.expected_str_len, gpa);

    var backends: [NUM_BACKENDS]BackendDetail = undefined;
    for (0..NUM_BACKENDS) |i| {
        const value = harness.readStr(buf, &offset, header.backend_value_lens[i], gpa);
        backends[i] = .{
            .status = @enumFromInt(header.backend_statuses[i]),
            .value = value,
            .duration_ns = header.backend_durations[i],
        };
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
        .backends = backends,
        .expected_str = expected_str,
    };
}

//
// Process pool (via harness)
//

/// Wrapper for the harness ProcessPool: runs a single test, captures timing,
/// and serializes via the eval wire protocol.
fn runTestForPool(allocator: std.mem.Allocator, tc: TestCase) TestResult {
    var timer = Timer.start() catch unreachable;
    const outcome = runSingleTest(allocator, tc);
    const duration = timer.read();
    return .{
        .status = outcome.status,
        .message = outcome.message,
        .duration_ns = duration,
        .timings = outcome.timings,
        .backends = outcome.backends,
        .expected_str = outcome.expected_str,
    };
}

fn serializeResultForPool(fd: posix.fd_t, result: TestResult) void {
    // Re-pack into the existing wire format (outcome + duration).
    const outcome = TestOutcome{
        .status = result.status,
        .message = result.message,
        .timings = result.timings,
        .backends = result.backends,
        .expected_str = result.expected_str,
    };
    serializeOutcome(fd, outcome, result.duration_ns);
}

fn getTestName(tc: TestCase) []const u8 {
    return tc.name;
}

fn dupeOptional(gpa: std.mem.Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| gpa.dupe(u8, slice) catch null else null;
}

fn stabilizeResult(gpa: std.mem.Allocator, result: TestResult) TestResult {
    var stable_backends = result.backends;
    for (&stable_backends) |*backend| {
        backend.value = dupeOptional(gpa, backend.value);
    }

    return .{
        .status = result.status,
        .message = dupeOptional(gpa, result.message),
        .duration_ns = result.duration_ns,
        .timings = result.timings,
        .backends = stable_backends,
        .expected_str = dupeOptional(gpa, result.expected_str),
    };
}

const default_result: TestResult = .{ .status = .crash, .message = null, .duration_ns = 0, .timings = .{} };
const timeout_result: TestResult = .{ .status = .timeout, .message = null, .duration_ns = 0, .timings = .{} };

const Pool = harness.ProcessPool(TestCase, TestResult, .{
    .runTest = &runTestForPool,
    .serialize = &serializeResultForPool,
    .deserialize = &deserializeOutcome,
    .default_result = default_result,
    .timeout_result = timeout_result,
    .stabilizeResult = &stabilizeResult,
    .getName = getTestName,
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
// The eval runner accepts the standard flags: --filter, --threads, --timeout, --verbose, --help.

fn printHelp() void {
    const help =
        \\Roc Eval Test Runner
        \\
        \\Runs eval tests across backends (interpreter, dev, wasm, llvm) in parallel
        \\and compares results via Str.inspect. Each backend evaluation runs in
        \\a forked child process for crash isolation.
        \\(WASM and LLVM backends are currently marked NOT_IMPLEMENTED until
        \\ statement-only code generation is implemented for each.)
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
        \\  --timeout <MS>        Per-test hang timeout in ms (default: 30000).
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
        \\
        \\  A performance summary table is printed after all tests with min, max,
        \\  mean, median, standard deviation, P95, and total for each phase, plus
        \\  the 5 slowest tests with full breakdowns.
        \\
        \\BACKEND COVERAGE:
        \\  The baseline goal is 100% of backends testing 100% of tests. Tests may
        \\  use `skip = .{ .wasm = true }` etc. to disable specific backends, but
        \\  any test with a skip reports as SKIP rather than PASS to keep partial
        \\  coverage visible.
        \\
        \\  Test outcomes:
        \\    PASS  - all backends ran and agreed
        \\    FAIL  - value mismatch or backend disagreement
        \\    CRASH - segfault or panic in generated code (detected via fork isolation)
        \\    HANG  - test exceeded the per-test timeout (killed by watchdog)
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
///       llvm:           NOT_IMPLEMENTED
fn writeFailureDetail(r: TestResult) void {
    if (r.expected_str) |es| {
        std.debug.print("        expected:       {s}\n", .{es});
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
            .skip => std.debug.print("        {s}:{s}SKIP\n", .{ name, padding(name.len) }),
            .not_implemented => std.debug.print("        {s}:{s}NOT_IMPLEMENTED\n", .{ name, padding(name.len) }),
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

    for (results) |r| {
        const t = r.timings;
        if (t.parse_ns > 0) try parse_times.append(gpa, t.parse_ns);
        if (t.canonicalize_ns > 0) try can_times.append(gpa, t.canonicalize_ns);
        if (t.typecheck_ns > 0) try check_times.append(gpa, t.typecheck_ns);
        if (t.interpreter_ns > 0) try interp_times.append(gpa, t.interpreter_ns);
        if (t.dev_ns > 0) try dev_times.append(gpa, t.dev_ns);
        if (t.wasm_ns > 0) try wasm_times.append(gpa, t.wasm_ns);
    }

    std.debug.print("\n=== Performance Summary (ms) ===\n", .{});
    harness.printStatsHeader();
    harness.printStatsRow("parse", computeTimingStats(parse_times.items));
    harness.printStatsRow("can", computeTimingStats(can_times.items));
    harness.printStatsRow("check", computeTimingStats(check_times.items));
    harness.printStatsRow("interp", computeTimingStats(interp_times.items));
    harness.printStatsRow("dev", computeTimingStats(dev_times.items));
    harness.printStatsRow("wasm", computeTimingStats(wasm_times.items));

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

/// Entry point for the parallel eval test runner.
pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var args_arena = std.heap.ArenaAllocator.init(gpa);
    defer args_arena.deinit();
    const cli = try harness.parseStandardArgs(args_arena.allocator());

    if (cli.help_requested) {
        printHelp();
        return;
    }

    const all_tests = collectTests();

    // Apply filters (support multiple --filter values)
    var filtered_buf: std.ArrayListUnmanaged(TestCase) = .empty;
    defer filtered_buf.deinit(gpa);

    if (cli.filters.len > 0) {
        for (all_tests) |tc| {
            for (cli.filters) |pattern| {
                if (std.mem.indexOf(u8, tc.name, pattern) != null or
                    std.mem.indexOf(u8, tc.source, pattern) != null)
                {
                    try filtered_buf.append(gpa, tc);
                    break;
                }
            }
        }
    } else {
        try filtered_buf.appendSlice(gpa, all_tests);
    }

    const tests = filtered_buf.items;
    if (tests.len == 0) {
        if (cli.filters.len == 0) {
            std.debug.print("No eval tests found.\n", .{});
        }
        return;
    }

    // Coverage mode: simple single-threaded loop, no fork, no watchdog, no threads.
    // Just run each test with the interpreter and print progress to stdout.
    if (comptime coverage_mode) {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        var passed: usize = 0;
        var failed: usize = 0;
        var skipped: usize = 0;
        var wall_timer = Timer.start() catch unreachable;

        for (tests, 0..) |tc, i| {
            _ = arena.reset(.retain_capacity);

            const outcome = runSingleTest(arena.allocator(), tc);

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

    const cpu_count = std.Thread.getCpuCount() catch 1;
    const max_children: usize = cli.max_threads orelse @min(cpu_count, tests.len);

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash, .message = null, .duration_ns = 0, .timings = .{} });

    var wall_timer = Timer.start() catch unreachable;

    // Default timeout: 30s under parallel load, 10s with single child.
    const hang_timeout_ms: u64 = if (cli.timeout_provided and cli.timeout_ms > 0)
        cli.timeout_ms
    else if (max_children <= 1)
        10_000
    else
        30_000;

    Pool.run(tests, results, max_children, hang_timeout_ms, gpa);

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
        for (r.backends) |bd| {
            if (bd.value) |v| gpa.free(v);
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
