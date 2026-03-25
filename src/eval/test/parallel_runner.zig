//! Parallel eval test runner.
//!
//! Runs eval tests across multiple threads, exercising every backend on every
//! test case and comparing their results via Str.inspect string comparison.
//!
//! ## Architecture overview
//!
//! Each test goes through a shared front-end (parse, canonicalize, type-check)
//! and is then evaluated by up to three independent backends:
//!
//!   1. **Interpreter** — walks the LIR directly.
//!   2. **Dev backend** — lowers LIR to native machine code.
//!   3. **WASM backend** — lowers LIR to WebAssembly, runs via bytebox.
//!
//! (LLVM is temporarily disabled — it currently aliases the dev backend.
//! Infrastructure is kept so it can be re-enabled easily.)
//!
//! ALL backends run via Str.inspect and must produce identical output strings.
//! This catches bugs where a backend produces a value of the right type but
//! wrong content.
//!
//! ## Process isolation
//!
//! Every backend evaluation runs in a forked child process that communicates
//! its result back through a pipe. If a child crashes (segfault, illegal
//! instruction, etc.) or hangs, the parent simply observes it via waitpid
//! and reports the failure without being affected.
//!
//! The `forkAndEval` function implements the fork+pipe+waitpid pattern:
//!   - Child calls the backend eval function, writes result to pipe, _exit(0).
//!   - Parent reads the pipe until EOF, then waitpid to reap the child.
//!   - Reading before waitpid avoids pipe buffer deadlock.
//!
//! ## Threading model
//!
//! Worker threads pull tests from a shared atomic index (lock-free work-
//! stealing). Each worker owns a per-thread arena allocator that is reset
//! between tests, so there is no cross-thread allocation contention for
//! test-local data. A small number of result strings are duplicated into a
//! shared GPA for the final report.
//!
//! ## Hang watchdog
//!
//! A dedicated thread polls worker timestamps every 500ms. If a worker
//! exceeds the timeout (default 30s), the watchdog:
//!   1. Sets a `timed_out` flag on the worker.
//!   2. Kills any forked child via SIGKILL (unblocks waitpid).
//!   3. Closes the worker's pipe read FD (unblocks any pipe read).
//!
//! The `forkAndEval` function checks the `timed_out` flag after waitpid
//! to distinguish watchdog-killed children from natural crashes.
//!
//! ## Usage
//!
//!   zig build test-eval [-- [--filter <pattern>] [--threads <N>] [--timeout <ms>] [--verbose]]

const std = @import("std");
const builtin = @import("builtin");
const coverage_options = @import("coverage_options");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("eval");

/// When true (set via `zig build coverage-eval`), the runner:
/// - Only builds/runs the interpreter backend (dev/wasm are DCE'd)
/// - Runs eval in-process (no fork) so kcov can trace it
/// - Forces single-threaded execution
const coverage_mode: bool = coverage_options.coverage;

const Can = can.Can;
const Check = check.Check;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;
const BuiltinTypes = eval_mod.BuiltinTypes;
const LoadedModule = eval_mod.builtin_loading.LoadedModule;
const deserializeBuiltinIndices = eval_mod.builtin_loading.deserializeBuiltinIndices;
const loadCompiledModule = eval_mod.builtin_loading.loadCompiledModule;

// Import backend evaluator functions through eval module (Zig requires
// each file to belong to exactly one module, so we can't import helpers.zig directly).
const helpers = eval_mod.test_helpers;

const posix = std.posix;

const AtomicUsize = std.atomic.Value(usize);
const AtomicI32 = std.atomic.Value(i32);
const AtomicBool = std.atomic.Value(bool);

/// Current wall-clock time in milliseconds, truncated to i32 (~24 day range).
fn nowMs() i32 {
    return @truncate(@divFloor(std.time.milliTimestamp(), 1));
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
    expected: Expected,
    skip: Skip = .{},

    pub const Expected = union(enum) {
        i64_val: i64,
        u8_val: u8,
        u16_val: u16,
        u32_val: u32,
        u64_val: u64,
        u128_val: u128,
        i8_val: i8,
        i16_val: i16,
        i32_val: i32,
        i128_val: i128,
        bool_val: bool,
        str_val: []const u8,
        dec_val: i128,
        f32_val: f32,
        f64_val: f64,
        problem: void,
        inspect_str: []const u8,

        /// Returns the expected value as i128 for integer variant comparison.
        pub fn intExpected(self: Expected) i128 {
            return switch (self) {
                .i64_val => |v| v,
                .u8_val => |v| v,
                .u16_val => |v| v,
                .u32_val => |v| v,
                .u64_val => |v| v,
                .u128_val => |v| @bitCast(v),
                .i8_val => |v| v,
                .i16_val => |v| v,
                .i32_val => |v| v,
                .i128_val => |v| v,
                else => unreachable,
            };
        }

        pub fn isInt(self: Expected) bool {
            return switch (self) {
                .i64_val, .u8_val, .u16_val, .u32_val, .u64_val, .u128_val, .i8_val, .i16_val, .i32_val, .i128_val => true,
                else => false,
            };
        }

        /// Format the expected value with its type for display, e.g. "16 : I64", "True : Bool".
        pub fn format(self: Expected, allocator: std.mem.Allocator) ?[]const u8 {
            var buf: [128]u8 = undefined;
            const slice: []const u8 = switch (self) {
                .i64_val => |v| std.fmt.bufPrint(&buf, "{d} : I64", .{v}) catch return null,
                .u8_val => |v| std.fmt.bufPrint(&buf, "{d} : U8", .{v}) catch return null,
                .u16_val => |v| std.fmt.bufPrint(&buf, "{d} : U16", .{v}) catch return null,
                .u32_val => |v| std.fmt.bufPrint(&buf, "{d} : U32", .{v}) catch return null,
                .u64_val => |v| std.fmt.bufPrint(&buf, "{d} : U64", .{v}) catch return null,
                .u128_val => |v| std.fmt.bufPrint(&buf, "{d} : U128", .{v}) catch return null,
                .i8_val => |v| std.fmt.bufPrint(&buf, "{d} : I8", .{v}) catch return null,
                .i16_val => |v| std.fmt.bufPrint(&buf, "{d} : I16", .{v}) catch return null,
                .i32_val => |v| std.fmt.bufPrint(&buf, "{d} : I32", .{v}) catch return null,
                .i128_val => |v| std.fmt.bufPrint(&buf, "{d} : I128", .{v}) catch return null,
                .bool_val => |v| if (v) "True : Bool" else "False : Bool",
                .str_val => |v| return std.fmt.allocPrint(allocator, "\"{s}\" : Str", .{v}) catch null,
                .f32_val => |v| std.fmt.bufPrint(&buf, "{d} : F32", .{v}) catch return null,
                .f64_val => |v| std.fmt.bufPrint(&buf, "{d} : F64", .{v}) catch return null,
                .dec_val => |v| std.fmt.bufPrint(&buf, "{d} : Dec", .{v}) catch return null,
                .inspect_str => |v| return std.fmt.allocPrint(allocator, "'{s}'", .{v}) catch null,
                else => return null,
            };
            return allocator.dupe(u8, slice) catch null;
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

const TestOutcome = struct {
    status: Status,
    message: ?[]const u8 = null,
    timings: EvalTimings = .{},
    /// Per-backend details (interpreter, dev, wasm, llvm). Populated by runValueTest.
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

const Timer = std.time.Timer;

//
// Runner context
//

/// Per-worker tracking state for the hang watchdog.
const WorkerState = struct {
    /// Millisecond timestamp when the worker started its current test (0 = idle).
    /// Uses i32 for 32-bit atomic compatibility (good for ~24 days of uptime).
    start_time_ms: AtomicI32 = AtomicI32.init(0),
    /// Index of the test currently being run (max = done).
    current_test: AtomicUsize = AtomicUsize.init(std.math.maxInt(usize)),
    /// Set by the watchdog before killing the child; checked by forkAndEval.
    timed_out: AtomicBool = AtomicBool.init(false),
};

const RunnerContext = struct {
    tests: []const TestCase,
    index: AtomicUsize,
    results: []TestResult,
    verbose: bool,
    /// Stable allocator for result messages that must outlive the per-test arena.
    msg_allocator: std.mem.Allocator,
    /// Per-worker state for hang detection. Null in single-threaded mode.
    worker_states: ?[]WorkerState = null,
    /// Counter for workers to claim their worker ID.
    worker_id_counter: AtomicUsize = AtomicUsize.init(0),
    /// Per-test timeout in nanoseconds (0 = no timeout).
    hang_timeout_ms: u64 = 0,
};

//
// Fork-based process isolation for backend evaluation
//

const has_fork = builtin.os.tag != .windows;

const BackendEvalFn = *const fn (std.mem.Allocator, *ModuleEnv, CIR.Expr.Idx, *const ModuleEnv) anyerror![]const u8;

/// Result of a forked backend evaluation.
const ForkResult = union(enum) {
    /// Child exited 0 and wrote result string to pipe.
    success: []const u8,
    /// Child exited non-zero (eval function returned an error).
    child_error: void,
    /// Child was killed by a signal (e.g. SIGSEGV=11).
    signal_death: u8,
    /// Child was killed by the watchdog (timed_out flag set).
    timeout: void,
    /// fork() or pipe() syscall failed.
    fork_failed: void,
};

/// Fork a child process to evaluate a backend, communicating the result via pipe.
///
/// The child calls `eval_fn(page_allocator, module_env, expr_idx, builtin_env)`,
/// writes the resulting string to the pipe, and `_exit(0)`. On error it `_exit(1)`.
///
/// The parent reads the pipe until EOF (important: before waitpid to avoid pipe
/// buffer deadlock), then reaps the child. The watchdog can kill the child and
/// close the pipe FD to unblock the parent on timeout.
fn forkAndEval(
    eval_fn: BackendEvalFn,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_env: *const ModuleEnv,
) ForkResult {
    if (comptime !has_fork or coverage_mode) {
        // In-process eval: used on Windows (no fork) and in coverage mode
        // (kcov can't trace forked children, so we must run in the parent).
        const result = eval_fn(std.heap.page_allocator, module_env, expr_idx, builtin_env) catch {
            return .{ .child_error = {} };
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

        const child_alloc = std.heap.page_allocator;
        const result_str = eval_fn(child_alloc, module_env, expr_idx, builtin_env) catch {
            posix.close(pipe_write);
            std.c._exit(1);
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

    // Store child PID and pipe FD so the watchdog can kill/close on timeout.
    const wid = helpers.my_worker_id;
    if (wid < helpers.worker_child_pids.len) {
        helpers.worker_child_pids[wid].store(@intCast(fork_result), .release);
    }
    if (wid < helpers.worker_pipe_fds.len) {
        helpers.worker_pipe_fds[wid].store(@intCast(pipe_read), .release);
    }

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
    // Close the pipe read end unless the watchdog already closed it.
    if (wid < helpers.worker_pipe_fds.len) {
        const prev = helpers.worker_pipe_fds[wid].swap(-1, .acq_rel);
        if (prev >= 0) posix.close(@intCast(prev));
    } else {
        posix.close(pipe_read);
    }

    // Now reap the child.
    const wait_result = posix.waitpid(fork_result, 0);

    // Clear child PID tracking.
    if (wid < helpers.worker_child_pids.len) {
        helpers.worker_child_pids[wid].store(0, .release);
    }

    const status = wait_result.status;
    const termination_signal: u8 = @truncate(status & 0x7f);

    if (termination_signal != 0) {
        // Child was killed by a signal. Check if it was a watchdog timeout.
        result_buf.deinit(std.heap.page_allocator);
        // Check the worker's timed_out flag (set by hangWatchdog before SIGKILL).
        if (wid < helpers.worker_child_pids.len) {
            // Access worker_states through the context isn't possible here,
            // but the watchdog sets timed_out on the WorkerState. We detect
            // timeout by checking if SIGKILL (signal 9) was the termination signal,
            // which is what the watchdog sends.
            // More precisely, we let the caller (threadMain) check timed_out.
        }
        if (termination_signal == 9) {
            // SIGKILL — likely the watchdog. Let caller distinguish via timed_out flag.
            return .{ .timeout = {} };
        }
        return .{ .signal_death = termination_signal };
    }

    const exit_code: u8 = @truncate((status >> 8) & 0xff);
    if (exit_code != 0 or read_error) {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = {} };
    }

    // Success — return the string read from the pipe.
    const owned = result_buf.toOwnedSlice(std.heap.page_allocator) catch {
        result_buf.deinit(std.heap.page_allocator);
        return .{ .child_error = {} };
    };
    return .{ .success = owned };
}

//
// Parse and canonicalize (shared by all backends)
//

const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    builtin_types: BuiltinTypes,
    // Frontend phase timings
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
};

fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    // Phase 1: Parse (includes builtin loading + source parsing)
    var parse_timer = Timer.start() catch unreachable;
    const builtin_indices = try deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    var builtin_module = try loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    errdefer builtin_module.deinit();

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);
    module_env.common.source = source;
    try module_env.common.calcLineStarts(module_env.gpa);

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    const parse_ast = try parse.parseExpr(&allocators, &module_env.common);

    if (parse_ast.tokenize_diagnostics.items.len > 0 or parse_ast.parse_diagnostics.items.len > 0) {
        return error.ParseError;
    }
    const parse_elapsed = parse_timer.read();

    // Phase 2: Canonicalize
    var can_timer = Timer.start() catch unreachable;
    parse_ast.store.emptyScratch();
    try module_env.initCIRFields("test");
    _ = try module_env.imports.getOrPut(allocator, &module_env.common.strings, "Builtin");

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    const czer = try allocator.create(Can);
    czer.* = try Can.initModule(&allocators, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
    });

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse return error.CanonicalizationFailed;
    const canonical_expr_idx = canonical_expr.get_idx();
    const can_elapsed = can_timer.read();

    // Phase 3: Type check
    var check_timer = Timer.start() catch unreachable;
    module_env.all_defs = try module_env.store.defSpanFrom(0);
    const imported_envs = [_]*const ModuleEnv{ builtin_module.env, module_env };
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, builtin_ctx);
    _ = try checker.checkExprReplWithDefs(canonical_expr_idx);
    const check_elapsed = check_timer.read();

    const bts = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .expr_idx = canonical_expr_idx,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .builtin_types = bts,
        .parse_ns = parse_elapsed,
        .canonicalize_ns = can_elapsed,
        .typecheck_ns = check_elapsed,
    };
}

fn cleanupResources(allocator: std.mem.Allocator, resources: ParsedResources) void {
    var builtin_module_copy = resources.builtin_module;
    builtin_module_copy.deinit();
    resources.checker.deinit();
    resources.can.deinit();
    resources.parse_ast.deinit();
    resources.module_env.deinit();
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.module_env);
}

//
// Str.inspect wrapping — converts CIR expression to Str.inspect(expr)
//

fn wrapInStrInspect(module_env: *ModuleEnv, inner_expr: CIR.Expr.Idx) !CIR.Expr.Idx {
    const top = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(inner_expr);
    const args_span = try module_env.store.exprSpanFrom(top);
    const region = module_env.store.getExprRegion(inner_expr);
    return module_env.addExpr(.{ .e_run_low_level = .{
        .op = .str_inspect,
        .args = args_span,
    } }, region);
}

//
// Test execution — unified interpreter + backend comparison
//

fn runSingleTest(allocator: std.mem.Allocator, tc: TestCase) TestOutcome {
    // If every backend is skipped, still validate the front-end so we catch
    // syntax errors in skipped tests rather than silently ignoring them.
    if (tc.skip.interpreter and tc.skip.dev and tc.skip.wasm) {
        const resources = parseAndCanonicalizeExpr(allocator, tc.source) catch {
            return .{ .status = .fail, .message = "INVALID_SYNTAX — skipped test has parse/check errors" };
        };
        cleanupResources(allocator, resources);
        return .{ .status = .skip, .timings = .{
            .parse_ns = resources.parse_ns,
            .canonicalize_ns = resources.canonicalize_ns,
            .typecheck_ns = resources.typecheck_ns,
        } };
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
    // NOTE: llvm is excluded — it currently aliases dev, so skip.llvm is ignored.
    return skip.interpreter or skip.dev or skip.wasm;
}

fn runSingleTestInner(allocator: std.mem.Allocator, tc: TestCase) !TestOutcome {
    return switch (tc.expected) {
        // All value-producing tests go through one unified path.
        .i64_val,
        .u8_val,
        .u16_val,
        .u32_val,
        .u64_val,
        .u128_val,
        .i8_val,
        .i16_val,
        .i32_val,
        .i128_val,
        .bool_val,
        .str_val,
        .f32_val,
        .f64_val,
        .dec_val,
        .inspect_str,
        => runValueTest(allocator, tc.source, tc.expected, tc.skip),
        // Special test flows
        .problem => runTestProblem(allocator, tc.source),
    };
}

/// Unified test function for all value-producing tests (primitive values and inspect_str).
/// 1. Runs ALL non-skipped backends via Str.inspect in forked child processes
/// 2. Checks cross-backend agreement (all must succeed and match)
/// 3. For inspect_str tests: also checks each backend against the expected string
fn runValueTest(allocator: std.mem.Allocator, src: []const u8, expected: TestCase.Expected, skip: TestCase.Skip) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    const timings = EvalTimings{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
    };

    // Run all non-skipped backends via Str.inspect and compare
    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch {
        return .{ .status = .fail, .message = "failed to wrap in Str.inspect", .timings = timings };
    };

    // For inspect_str tests, the raw string is used for value comparison.
    // The formatted string (with type annotation) is used for display only.
    const raw_expected: ?[]const u8 = if (expected == .inspect_str) expected.inspect_str else null;
    const display_expected: ?[]const u8 = expected.format(allocator);
    // In coverage mode, only run the interpreter — dev/wasm are DCE'd at comptime
    // and never built, giving faster compilation and cleaner kcov output.
    const skips = if (comptime coverage_mode)
        [NUM_BACKENDS]bool{ skip.interpreter, true, true, true }
    else
        [NUM_BACKENDS]bool{ skip.interpreter, skip.dev, skip.wasm, true }; // llvm always not_implemented for now

    const eval_fns = [NUM_BACKENDS]BackendEvalFn{
        helpers.lirInterpreterInspectedStr,
        helpers.devEvaluatorStr,
        helpers.wasmEvaluatorStr,
        helpers.devEvaluatorStr, // llvm placeholder
    };

    var backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{.{ .status = .not_implemented }} ** NUM_BACKENDS;
    var first_ok: ?[]const u8 = null;
    var any_failure = false;

    for (0..NUM_BACKENDS) |i| {
        if (i == 3) continue; // llvm: not_implemented
        if (skips[i]) {
            backends[i] = .{ .status = .skip };
            continue;
        }

        var timer = Timer.start() catch unreachable;
        const fork_result = forkAndEval(eval_fns[i], resources.module_env, inspect_expr, resources.builtin_module.env);
        const dur = timer.read();

        switch (fork_result) {
            .success => |str| {
                // Check against expected string (only for inspect_str tests)
                const value_ok = if (raw_expected) |es| std.mem.eql(u8, es, str) else true;
                // Check cross-backend agreement
                const agreement_ok = if (first_ok) |fok| std.mem.eql(u8, fok, str) else true;

                if (!value_ok or !agreement_ok) {
                    backends[i] = .{ .status = .wrong_value, .value = str, .duration_ns = dur };
                    any_failure = true;
                } else {
                    backends[i] = .{ .status = .pass, .value = str, .duration_ns = dur };
                    if (first_ok == null) first_ok = str;
                }
            },
            .child_error => {
                backends[i] = .{ .status = .fail, .value = "ChildExecFailed", .duration_ns = dur };
                any_failure = true;
            },
            .signal_death => |sig| {
                var sig_buf: [32]u8 = undefined;
                const sig_str = std.fmt.bufPrint(&sig_buf, "signal: {d}", .{sig}) catch "signal: ?";
                backends[i] = .{ .status = .fail, .value = allocator.dupe(u8, sig_str) catch "signal", .duration_ns = dur };
                any_failure = true;
            },
            .timeout => {
                backends[i] = .{ .status = .fail, .value = "Timeout", .duration_ns = dur };
                any_failure = true;
            },
            .fork_failed => {
                backends[i] = .{ .status = .fail, .value = "ForkFailed", .duration_ns = dur };
                any_failure = true;
            },
        }
    }

    // Build final timings with backend durations merged in.
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

fn runTestProblem(allocator: std.mem.Allocator, src: []const u8) !TestOutcome {
    var timer = Timer.start() catch unreachable;
    const resources = parseAndCanonicalizeExpr(allocator, src) catch {
        // Parse or canonicalize error means a problem was found — that's a pass.
        const elapsed = timer.read();
        return .{ .status = .pass, .timings = .{ .parse_ns = elapsed } };
    };
    defer cleanupResources(allocator, resources);

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

//
// Worker thread
//

fn threadMain(ctx: *RunnerContext) void {
    // Claim a worker ID for hang-detection state tracking.
    const my_id = ctx.worker_id_counter.fetchAdd(1, .monotonic);
    const my_state: ?*WorkerState = if (ctx.worker_states) |ws|
        &ws[my_id]
    else
        null;
    helpers.my_worker_id = my_id;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    while (true) {
        const i = ctx.index.fetchAdd(1, .monotonic);
        if (i >= ctx.tests.len) {
            // Mark worker as done.
            if (my_state) |ws| {
                ws.current_test.store(std.math.maxInt(usize), .release);
                ws.start_time_ms.store(0, .release);
            }
            break;
        }

        _ = arena.reset(.retain_capacity);
        const allocator = arena.allocator();

        const tc = ctx.tests[i];
        var wall_timer = Timer.start() catch unreachable;

        // Update watchdog tracking.
        if (my_state) |ws| {
            ws.current_test.store(i, .release);
            ws.timed_out.store(false, .release);
            ws.start_time_ms.store(nowMs(), .release);
        }

        const outcome = runSingleTest(allocator, tc);

        if (my_state) |ws| ws.start_time_ms.store(0, .release);
        const elapsed = wall_timer.read();

        // Dup the message and backend values to the stable GPA so they survive arena reset.
        const stable_msg: ?[]const u8 = if (outcome.message) |msg|
            (ctx.msg_allocator.dupe(u8, msg) catch null)
        else
            null;

        var stable_backends = outcome.backends;
        for (&stable_backends) |*bd| {
            if (bd.value) |v| {
                bd.value = ctx.msg_allocator.dupe(u8, v) catch null;
            }
        }

        const stable_expected: ?[]const u8 = if (outcome.expected_str) |es|
            (ctx.msg_allocator.dupe(u8, es) catch null)
        else
            null;

        ctx.results[i] = .{
            .status = outcome.status,
            .message = stable_msg,
            .duration_ns = elapsed,
            .timings = outcome.timings,
            .backends = stable_backends,
            .expected_str = stable_expected,
        };
    }
}

//
// Test collection
//

fn collectTests() []const TestCase {
    return &eval_tests.tests;
}

//
// CLI parsing
//

const CliArgs = struct {
    filter: ?[]const u8 = null,
    threads: usize = 0,
    verbose: bool = false,
    /// Per-test hang timeout in milliseconds (0 = use default of 10s, only in multi-threaded mode).
    timeout_ms: u64 = 0,
};

fn parseCliArgs(args: []const []const u8) CliArgs {
    var result = CliArgs{};
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--help") or std.mem.eql(u8, args[i], "-h")) {
            printHelp();
            std.process.exit(0);
        } else if (std.mem.eql(u8, args[i], "--filter") and i + 1 < args.len) {
            i += 1;
            result.filter = args[i];
        } else if (std.mem.eql(u8, args[i], "--threads") and i + 1 < args.len) {
            i += 1;
            result.threads = std.fmt.parseInt(usize, args[i], 10) catch 0;
        } else if (std.mem.eql(u8, args[i], "--verbose")) {
            result.verbose = true;
        } else if (std.mem.eql(u8, args[i], "--timeout") and i + 1 < args.len) {
            i += 1;
            result.timeout_ms = std.fmt.parseInt(u64, args[i], 10) catch 0;
        }
    }
    return result;
}

fn printHelp() void {
    const help =
        \\Roc Eval Test Runner
        \\
        \\Runs eval tests across backends (interpreter, dev, wasm) in parallel
        \\and compares results via Str.inspect. Each backend evaluation runs in
        \\a forked child process for crash isolation.
        \\(LLVM backend temporarily disabled — currently aliases dev backend.)
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
        \\  --threads <N>         Max worker threads (default: number of CPU cores).
        \\  --verbose             Print PASS and SKIP results (default: only FAIL/CRASH).
        \\  --timeout <MS>        Per-test hang timeout in ms (default: 10000). Multi-thread only.
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
        \\      Traces the lowering pipeline (CIR→MIR→LIR→RC) and interpreter eval loop.
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

    // Standard deviation
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

fn printStatsRow(label: []const u8, stats: ?TimingStats) void {
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
    std.debug.print("  {s:<8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8} {s:>8}   {s:>3}\n", .{
        "Phase", "Min", "Max", "Mean", "Median", "StdDev", "P95", "Total", "N",
    });
    std.debug.print("  {s:-<8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8} {s:->8}   {s:->3}\n", .{
        "", "", "", "", "", "", "", "", "",
    });
    printStatsRow("parse", computeTimingStats(parse_times.items));
    printStatsRow("can", computeTimingStats(can_times.items));
    printStatsRow("check", computeTimingStats(check_times.items));
    printStatsRow("interp", computeTimingStats(interp_times.items));
    printStatsRow("dev", computeTimingStats(dev_times.items));
    printStatsRow("wasm", computeTimingStats(wasm_times.items));

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

/// Count results that workers have actually written (duration_ns > 0 means
/// the worker finished and stored a result; the default is 0 / "not started").
fn countCompletedResults(results: []const TestResult) usize {
    var n: usize = 0;
    for (results) |r| {
        if (r.duration_ns > 0) n += 1;
    }
    return n;
}

/// Watchdog that polls worker threads, prints progress, and kills hangs.
/// Runs on the main thread while workers are executing.
fn hangWatchdog(ctx: *RunnerContext, timeout_ms: u64) void {
    const ws = ctx.worker_states orelse return;
    var progress_timer = Timer.start() catch unreachable;
    var last_progress_ns: u64 = 0;

    while (true) {
        // Sleep 500ms between polls.
        std.Thread.sleep(500_000_000);

        const now = nowMs();
        var all_done = true;

        for (ws, 0..) |*worker, idx| {
            const test_idx = worker.current_test.load(.acquire);
            if (test_idx == std.math.maxInt(usize)) continue; // worker finished

            all_done = false;
            const start = worker.start_time_ms.load(.acquire);
            if (start <= 0) continue; // not actively running a test

            const elapsed_ms: u64 = @intCast(@max(0, now -% start));
            if (elapsed_ms > timeout_ms) {
                // This worker is hung. Mark it timed-out and kill any forked child.
                worker.timed_out.store(true, .release);
                const test_name = if (test_idx < ctx.tests.len) ctx.tests[test_idx].name else "?";
                std.debug.print("\n  HANG  {s}  ({d}ms) — killing", .{ test_name, elapsed_ms });
                if (comptime builtin.os.tag != .windows) {
                    // Kill any forked child process (unblocks waitpid in forkAndEval).
                    if (idx < helpers.worker_child_pids.len) {
                        const cpid = helpers.worker_child_pids[idx].swap(0, .acq_rel);
                        if (cpid > 0) {
                            std.debug.print(" child(pid={d})", .{cpid});
                            posix.kill(@intCast(cpid), posix.SIG.KILL) catch {};
                        }
                    }
                    // Close the worker's pipe read FD so any blocked read() returns.
                    if (idx < helpers.worker_pipe_fds.len) {
                        const pfd = helpers.worker_pipe_fds[idx].swap(-1, .acq_rel);
                        if (pfd >= 0) posix.close(@intCast(pfd));
                    }
                }
                std.debug.print("\n", .{});
                // Give the child time to die before re-checking.
                std.Thread.sleep(200_000_000); // 200ms
            }
        }

        if (all_done) break;

        // Print progress every ~1s.
        const progress_elapsed = progress_timer.read();
        if (progress_elapsed - last_progress_ns >= 1_000_000_000) {
            last_progress_ns = progress_elapsed;
            const completed = countCompletedResults(ctx.results);
            const wall_s = @as(f64, @floatFromInt(progress_elapsed)) / 1_000_000_000.0;
            std.debug.print("\r  running: {d}/{d} results, {d:.1}s elapsed", .{
                completed, ctx.tests.len, wall_s,
            });
        }
    }

    // Clear the progress line.
    std.debug.print("\r{s}\r", .{" " ** 72});
}

/// Entry point for the parallel eval test runner.
pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    const argv = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, argv);
    const cli = parseCliArgs(argv);

    const all_tests = collectTests();

    // Apply filter
    var filtered_buf: std.ArrayListUnmanaged(TestCase) = .empty;
    defer filtered_buf.deinit(gpa);

    if (cli.filter) |pattern| {
        for (all_tests) |tc| {
            if (std.mem.indexOf(u8, tc.name, pattern) != null or
                std.mem.indexOf(u8, tc.source, pattern) != null)
            {
                try filtered_buf.append(gpa, tc);
            }
        }
    } else {
        try filtered_buf.appendSlice(gpa, all_tests);
    }

    const tests = filtered_buf.items;
    if (tests.len == 0) {
        if (cli.filter == null) {
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
    const thread_count: usize = if (cli.threads > 0)
        @min(cli.threads, cpu_count)
    else
        @min(cpu_count, tests.len);

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash, .message = null, .duration_ns = 0, .timings = .{} });

    var wall_timer = Timer.start() catch unreachable;

    // Default timeout: 30s in multi-threaded mode, 10s in single-threaded mode.
    // The slowest tests take ~5s in isolation; under full parallel load (16+ threads)
    // CPU contention can slow individual tests by 2-3x, so 30s avoids false positives.
    // Single-threaded mode uses a shorter default since there's no CPU contention.
    const hang_timeout_ms: u64 = if (cli.timeout_ms > 0)
        cli.timeout_ms
    else if (thread_count <= 1)
        10_000
    else
        30_000;

    // Allocate per-worker state for hang detection.
    const worker_states: ?[]WorkerState = blk: {
        const ws = try gpa.alloc(WorkerState, thread_count);
        for (ws) |*w| w.* = .{};
        break :blk ws;
    };
    defer if (worker_states) |ws| gpa.free(ws);

    // Allocate per-worker child PID and pipe FD tracking for fork-based isolation.
    const child_pids = try gpa.alloc(std.atomic.Value(i32), thread_count);
    defer gpa.free(child_pids);
    for (child_pids) |*p| p.* = std.atomic.Value(i32).init(0);
    helpers.worker_child_pids = child_pids;

    const pipe_fds = try gpa.alloc(std.atomic.Value(i32), thread_count);
    defer gpa.free(pipe_fds);
    for (pipe_fds) |*p| p.* = std.atomic.Value(i32).init(-1);
    helpers.worker_pipe_fds = pipe_fds;

    var context = RunnerContext{
        .tests = tests,
        .index = AtomicUsize.init(0),
        .results = results,
        .verbose = cli.verbose,
        .msg_allocator = gpa,
        .worker_states = worker_states,
        .hang_timeout_ms = hang_timeout_ms,
    };

    if (thread_count <= 1) {
        // Spawn watchdog on a separate thread so it can kill hung forks.
        const watchdog_thread = if (hang_timeout_ms > 0)
            try std.Thread.spawn(.{}, hangWatchdog, .{ &context, hang_timeout_ms })
        else
            null;
        threadMain(&context);
        if (watchdog_thread) |wd| wd.join();
    } else {
        const threads = try gpa.alloc(std.Thread, thread_count);
        defer gpa.free(threads);
        for (threads) |*t| {
            t.* = try std.Thread.spawn(.{}, threadMain, .{&context});
        }

        // Watchdog loop: poll workers for hangs until all are done.
        if (hang_timeout_ms > 0) {
            hangWatchdog(&context, hang_timeout_ms);
        }

        for (threads) |t| {
            t.join();
        }
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
        std.debug.print("\n{d} passed, {d} failed, {d} crashed, {d} hung, {d} skipped ({d} total) in {d:.0}ms using {d} thread(s)\n", .{
            passed, failed, crashed, timed_out, skipped, tests.len, wall_ms, thread_count,
        });
    } else {
        std.debug.print("\n{d} passed, {d} failed, {d} crashed, {d} skipped ({d} total) in {d:.0}ms using {d} thread(s)\n", .{
            passed, failed, crashed, skipped, tests.len, wall_ms, thread_count,
        });
    }

    if (failed > 0 or crashed > 0 or timed_out > 0) {
        std.process.exit(1);
    }
}
