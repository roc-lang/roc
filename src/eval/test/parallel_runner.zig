//! Parallel eval test runner.
//!
//! A standalone binary that runs eval tests across multiple threads using a
//! work-stealing job queue. Each test runs the interpreter, dev backend,
//! and wasm backend, then compares all results via Str.inspect string
//! comparison. (LLVM backend is temporarily disabled — it currently aliases
//! the dev backend. Infrastructure is kept so it can be re-enabled easily.)
//!
//! Crash protection (setjmp/longjmp + signal handlers) allows the runner to
//! recover from segfaults and continue.
//!
//! Usage:
//!   zig build test-eval [-- [--filter <pattern>] [--threads <N>] [--verbose]]

const std = @import("std");
const builtin = @import("builtin");
const sljmp = @import("sljmp");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("eval");
const LirProgram = eval_mod.LirProgram;
const LirInterpreter = eval_mod.LirInterpreter;

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

extern "c" fn pthread_kill(thread: std.c.pthread_t, sig: c_int) c_int;

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
        err_val: anyerror,
        problem: void,
        type_mismatch_crash: void,
        dev_only_str: []const u8,
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
    };

    pub const Skip = packed struct {
        interpreter: bool = false,
        dev: bool = false,
        wasm: bool = false,
        llvm: bool = false,
    };
};

//
// Crash protection
//
// TODO: The signal handler uses _setjmp/_longjmp which is technically
// undefined behavior in POSIX (only sigsetjmp/siglongjmp are defined for
// use in signal handlers). In practice this works on Linux/macOS/BSDs and
// is used by many projects (libsigsegv, GHC), but the sljmp module should
// be extended to support sigsetjmp/siglongjmp for correctness.
//

/// Override the default panic handler to support crash recovery via setjmp/longjmp.
pub const panic = std.debug.FullPanic(panicHandler);

threadlocal var panic_jmp: ?*sljmp.JmpBuf = null;
threadlocal var panic_msg: ?[]const u8 = null;

fn panicHandler(msg: []const u8, _: ?usize) noreturn {
    if (panic_jmp) |jmp| {
        panic_msg = msg;
        panic_jmp = null;
        sljmp.longjmp(jmp, 1);
    }
    std.debug.defaultPanic(msg, @returnAddress());
}

fn crashSignalHandler(sig: i32) callconv(.c) void {
    if (panic_jmp) |jmp| {
        panic_msg = if (sig == posix.SIG.USR1)
            "timed out (possible infinite loop)"
        else
            "signal: segfault or illegal instruction in generated code";
        panic_jmp = null;
        sljmp.longjmp(jmp, if (sig == posix.SIG.USR1) 3 else 2);
    }
    // No jmp_buf — restore defaults and re-raise so the process terminates.
    const dfl = posix.Sigaction{
        .handler = .{ .handler = posix.SIG.DFL },
        .mask = posix.sigemptyset(),
        .flags = 0,
    };
    posix.sigaction(posix.SIG.SEGV, &dfl, null);
    posix.sigaction(posix.SIG.BUS, &dfl, null);
    posix.sigaction(posix.SIG.ILL, &dfl, null);
}

fn installCrashSignalHandlers() void {
    if (comptime builtin.os.tag == .windows) return;

    // Block the handled signals during handler execution to prevent
    // re-entrance. After longjmp recovery we manually unblock them.
    var handler_mask = posix.sigemptyset();
    posix.sigaddset(&handler_mask, posix.SIG.SEGV);
    posix.sigaddset(&handler_mask, posix.SIG.BUS);
    posix.sigaddset(&handler_mask, posix.SIG.ILL);
    posix.sigaddset(&handler_mask, posix.SIG.USR1);

    const sa = posix.Sigaction{
        .handler = .{ .handler = &crashSignalHandler },
        .mask = handler_mask,
        .flags = 0,
    };
    posix.sigaction(posix.SIG.SEGV, &sa, null);
    posix.sigaction(posix.SIG.BUS, &sa, null);
    posix.sigaction(posix.SIG.ILL, &sa, null);
    posix.sigaction(posix.SIG.USR1, &sa, null);
}

/// After longjmp from a signal handler, the caught signal remains blocked
/// (because _setjmp/_longjmp don't restore the signal mask). Unblock so
/// future crashes are still caught.
fn unblockCrashSignals() void {
    if (comptime builtin.os.tag == .windows) return;

    var unblock = posix.sigemptyset();
    posix.sigaddset(&unblock, posix.SIG.SEGV);
    posix.sigaddset(&unblock, posix.SIG.BUS);
    posix.sigaddset(&unblock, posix.SIG.ILL);
    posix.sigaddset(&unblock, posix.SIG.USR1);
    _ = posix.system.sigprocmask(posix.SIG.UNBLOCK, &unblock, null);
}

//
// Test outcome
//

const TestOutcome = struct {
    status: Status,
    message: ?[]const u8 = null,
    timings: EvalTimings = .{},

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
    /// Set by the watchdog before sending SIGUSR1; checked by crash recovery.
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
// Backend comparison helpers
//

/// Per-backend result for comparison reporting.
const BackendResult = struct {
    name: []const u8,
    value: union(enum) {
        ok: []const u8,
        err: []const u8,
    },
};

/// Compare all backend Str.inspect results. Returns null if they all agree, or an error message.
fn compareBackendResults(
    allocator: std.mem.Allocator,
    backends: []const BackendResult,
) ?[]const u8 {
    // Collect all successful results
    var ok_count: usize = 0;
    var first_ok: ?[]const u8 = null;
    for (backends) |br| {
        if (br.value == .ok) {
            ok_count += 1;
            if (first_ok == null) first_ok = br.value.ok;
        }
    }

    if (ok_count < 2) return null; // can't compare with fewer than 2 successes

    // All backends produce Str.inspect output — direct byte comparison is correct.
    var mismatch = false;
    for (backends) |br| {
        if (br.value == .ok) {
            if (!std.mem.eql(u8, first_ok.?, br.value.ok)) {
                mismatch = true;
                break;
            }
        }
    }

    if (!mismatch) return null;

    // Build mismatch message (exclude skipped backends)
    var msg_buf: std.ArrayListUnmanaged(u8) = .empty;
    const writer = msg_buf.writer(allocator);
    writer.print("Backend mismatch:", .{}) catch {};
    for (backends) |br| {
        switch (br.value) {
            .ok => |s| writer.print(" {s}='{s}'", .{ br.name, s }) catch {},
            .err => |e| {
                if (!std.mem.eql(u8, e, "skipped")) {
                    writer.print(" {s}=err({s})", .{ br.name, e }) catch {};
                }
            },
        }
    }
    return msg_buf.toOwnedSlice(allocator) catch "Backend mismatch (OOM building details)";
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
        // Normal value tests: interpret, check value, compare all backends
        .i64_val, .u8_val, .u16_val, .u32_val, .u64_val, .u128_val, .i8_val, .i16_val, .i32_val, .i128_val, .bool_val, .str_val, .f32_val, .f64_val, .dec_val => runNormalTest(allocator, tc.source, tc.expected, tc.skip),
        // Special tests with unique flows
        .err_val => |expected_err| runTestError(allocator, tc.source, expected_err),
        .problem => runTestProblem(allocator, tc.source),
        .type_mismatch_crash => runTestTypeMismatchCrash(allocator, tc.source),
        .dev_only_str => |expected_str| runTestDevOnlyStr(allocator, tc.source, expected_str, tc.skip),
        .inspect_str => |expected_str| runTestInspectStr(allocator, tc.source, expected_str, tc.skip),
    };
}

/// Unified test function for all value-producing tests.
/// Parses, interprets (via LIR pipeline), checks the value against expected,
/// then compares all backends via Str.inspect.
fn runNormalTest(allocator: std.mem.Allocator, src: []const u8, expected: TestCase.Expected, skip: TestCase.Skip) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var fe_timings = EvalTimings{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
    };

    // If interpreter is not skipped, do typed value checking via LIR pipeline
    if (!skip.interpreter) {
        var interp_timer = Timer.start() catch unreachable;
        const interp_result = helpers.lirInterpreterEval(allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env) catch |err| {
            fe_timings.interpreter_ns = interp_timer.read();
            return .{ .status = .fail, .message = @errorName(err), .timings = fe_timings };
        };
        fe_timings.interpreter_ns = interp_timer.read();
        defer interp_result.deinit(allocator);

        // Check interpreter result against expected value
        const interp_i128 = interp_result.asI128();
        switch (expected) {
            .i64_val, .u8_val, .u16_val, .u32_val, .u64_val, .u128_val, .i8_val, .i16_val, .i32_val, .i128_val => {
                if (interp_i128 == null or interp_i128.? != expected.intExpected()) {
                    return .{ .status = .fail, .message = "integer value mismatch", .timings = fe_timings };
                }
            },
            .bool_val => |exp| {
                switch (interp_result) {
                    .bool_val => |b| if (b != exp) return .{ .status = .fail, .message = "boolean value mismatch", .timings = fe_timings },
                    else => if ((interp_i128 != null and interp_i128.? != 0) != exp) {
                        return .{ .status = .fail, .message = "boolean value mismatch", .timings = fe_timings };
                    },
                }
            },
            .str_val => |exp| {
                switch (interp_result) {
                    .str => |s| if (!std.mem.eql(u8, exp, s)) return .{ .status = .fail, .message = "string value mismatch", .timings = fe_timings },
                    else => return .{ .status = .fail, .message = "expected string from interpreter", .timings = fe_timings },
                }
            },
            .f32_val => |exp| {
                switch (interp_result) {
                    .float_f32 => |v| if (@abs(v - exp) > 0.0001) return .{ .status = .fail, .message = "f32 value mismatch", .timings = fe_timings },
                    else => return .{ .status = .fail, .message = "expected f32 from interpreter", .timings = fe_timings },
                }
            },
            .f64_val => |exp| {
                switch (interp_result) {
                    .float_f64 => |v| if (@abs(v - exp) > 0.000000001) return .{ .status = .fail, .message = "f64 value mismatch", .timings = fe_timings },
                    else => return .{ .status = .fail, .message = "expected f64 from interpreter", .timings = fe_timings },
                }
            },
            .dec_val => |exp| {
                switch (interp_result) {
                    .dec => |v| if (v != exp) return .{ .status = .fail, .message = "Dec value mismatch", .timings = fe_timings },
                    else => return .{ .status = .fail, .message = "expected Dec from interpreter", .timings = fe_timings },
                }
            },
            else => unreachable,
        }
    }

    // Compare all backends via Str.inspect (interpreter included as a backend)
    var outcome = compareAllBackends(allocator, resources, skip);
    outcome.timings.parse_ns = resources.parse_ns;
    outcome.timings.canonicalize_ns = resources.canonicalize_ns;
    outcome.timings.typecheck_ns = resources.typecheck_ns;
    if (fe_timings.interpreter_ns > 0) outcome.timings.interpreter_ns = fe_timings.interpreter_ns;
    return outcome;
}

fn runTestError(allocator: std.mem.Allocator, src: []const u8, expected_err: anyerror) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var interp_timer = Timer.start() catch unreachable;

    // Lower CIR → LIR (errors here count as interpreter errors)
    var lir_prog = LirProgram.init(allocator, base.target.TargetUsize.native);
    defer lir_prog.deinit();
    const all_module_envs = [_]*ModuleEnv{ @constCast(resources.builtin_module.env), resources.module_env };

    var lower_result = lir_prog.lowerExpr(resources.module_env, resources.expr_idx, &all_module_envs, null) catch |err| {
        const interp_ns = interp_timer.read();
        const timings = EvalTimings{ .parse_ns = resources.parse_ns, .canonicalize_ns = resources.canonicalize_ns, .typecheck_ns = resources.typecheck_ns, .interpreter_ns = interp_ns };
        if (err == expected_err) return .{ .status = .pass, .timings = timings };
        return .{ .status = .fail, .message = "wrong error during lowering", .timings = timings };
    };
    defer lower_result.deinit();

    var interp = LirInterpreter.init(allocator, &lower_result.lir_store, lower_result.layout_store, null) catch |err| {
        const interp_ns = interp_timer.read();
        const timings = EvalTimings{ .parse_ns = resources.parse_ns, .canonicalize_ns = resources.canonicalize_ns, .typecheck_ns = resources.typecheck_ns, .interpreter_ns = interp_ns };
        if (err == expected_err) return .{ .status = .pass, .timings = timings };
        return .{ .status = .fail, .message = "wrong error during init", .timings = timings };
    };
    defer interp.deinit();

    _ = interp.eval(lower_result.final_expr_id) catch |err| {
        const interp_ns = interp_timer.read();
        const timings = EvalTimings{ .parse_ns = resources.parse_ns, .canonicalize_ns = resources.canonicalize_ns, .typecheck_ns = resources.typecheck_ns, .interpreter_ns = interp_ns };
        if (err == expected_err) return .{ .status = .pass, .timings = timings };
        return .{ .status = .fail, .message = "wrong error returned", .timings = timings };
    };
    const interp_ns = interp_timer.read();

    // LIR interpreter handles failed expects by setting a message rather than erroring.
    // Check for this case and treat it as error.Crash.
    const expect_is_crash = switch (expected_err) {
        error.Crash => true,
        else => false,
    };
    if (interp.getExpectMessage() != null and expect_is_crash) {
        return .{ .status = .pass, .timings = .{
            .parse_ns = resources.parse_ns,
            .canonicalize_ns = resources.canonicalize_ns,
            .typecheck_ns = resources.typecheck_ns,
            .interpreter_ns = interp_ns,
        } };
    }

    return .{ .status = .fail, .message = "expected error but evaluation succeeded", .timings = .{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
        .interpreter_ns = interp_ns,
    } };
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

fn runTestTypeMismatchCrash(allocator: std.mem.Allocator, src: []const u8) !TestOutcome {
    const resources = parseAndCanonicalizeExpr(allocator, src) catch {
        return .{ .status = .pass };
    };
    defer cleanupResources(allocator, resources);

    var interp_timer = Timer.start() catch unreachable;

    var lir_prog = LirProgram.init(allocator, base.target.TargetUsize.native);
    defer lir_prog.deinit();
    const all_module_envs = [_]*ModuleEnv{ @constCast(resources.builtin_module.env), resources.module_env };

    var lower_result = lir_prog.lowerExpr(resources.module_env, resources.expr_idx, &all_module_envs, null) catch {
        const interp_ns = interp_timer.read();
        return .{ .status = .pass, .timings = .{ .parse_ns = resources.parse_ns, .canonicalize_ns = resources.canonicalize_ns, .typecheck_ns = resources.typecheck_ns, .interpreter_ns = interp_ns } };
    };
    defer lower_result.deinit();

    var interp = LirInterpreter.init(allocator, &lower_result.lir_store, lower_result.layout_store, null) catch {
        const interp_ns = interp_timer.read();
        return .{ .status = .pass, .timings = .{ .parse_ns = resources.parse_ns, .canonicalize_ns = resources.canonicalize_ns, .typecheck_ns = resources.typecheck_ns, .interpreter_ns = interp_ns } };
    };
    defer interp.deinit();

    _ = interp.eval(lower_result.final_expr_id) catch {
        const interp_ns = interp_timer.read();
        return .{ .status = .pass, .timings = .{ .parse_ns = resources.parse_ns, .canonicalize_ns = resources.canonicalize_ns, .typecheck_ns = resources.typecheck_ns, .interpreter_ns = interp_ns } };
    };
    const interp_ns = interp_timer.read();

    return .{ .status = .fail, .message = "expected crash but evaluation succeeded", .timings = .{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
        .interpreter_ns = interp_ns,
    } };
}

/// Run a test that only checks the dev backend output (no interpreter comparison).
fn runTestDevOnlyStr(allocator: std.mem.Allocator, src: []const u8, expected_str: []const u8, skip: TestCase.Skip) !TestOutcome {
    if (skip.dev) {
        return .{ .status = .skip };
    }

    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    const fe_timings = EvalTimings{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
    };

    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch {
        return .{ .status = .fail, .message = "failed to wrap in Str.inspect", .timings = fe_timings };
    };

    var dev_timer = Timer.start() catch unreachable;
    const dev_str = helpers.devEvaluatorStr(allocator, resources.module_env, inspect_expr, resources.builtin_module.env) catch |err| {
        const dev_ns = dev_timer.read();
        var timings = fe_timings;
        timings.dev_ns = dev_ns;
        return .{ .status = .fail, .message = @errorName(err), .timings = timings };
    };
    const dev_ns = dev_timer.read();
    defer allocator.free(dev_str);

    var timings = fe_timings;
    timings.dev_ns = dev_ns;
    if (!std.mem.eql(u8, expected_str, dev_str)) {
        return .{ .status = .fail, .message = "dev_only_str value mismatch", .timings = timings };
    }
    return .{ .status = .pass, .timings = timings };
}

/// Run a test that checks each non-skipped backend's Str.inspect output against
/// an expected string. Used for records, tuples, lists, and other composite types.
/// All non-skipped backends must produce the expected result to pass.
fn runTestInspectStr(allocator: std.mem.Allocator, src: []const u8, expected_str: []const u8, skip: TestCase.Skip) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var timings = EvalTimings{
        .parse_ns = resources.parse_ns,
        .canonicalize_ns = resources.canonicalize_ns,
        .typecheck_ns = resources.typecheck_ns,
    };

    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch {
        return .{ .status = .fail, .message = "failed to wrap in Str.inspect", .timings = timings };
    };

    // Check each non-skipped backend against expected_str
    if (!skip.interpreter) {
        const result = runBackend(allocator, "interpreter", helpers.lirInterpreterInspectedStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .interpreter_ns);
        defer if (result.value == .ok) allocator.free(result.value.ok);
        switch (result.value) {
            .ok => |s| if (!std.mem.eql(u8, expected_str, s)) return .{ .status = .fail, .message = "interpreter inspect_str mismatch", .timings = timings },
            .err => |e| return .{ .status = .fail, .message = e, .timings = timings },
        }
    }

    if (!skip.dev) {
        const result = runBackend(allocator, "dev", helpers.devEvaluatorStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .dev_ns);
        defer if (result.value == .ok) allocator.free(result.value.ok);
        switch (result.value) {
            .ok => |s| if (!std.mem.eql(u8, expected_str, s)) return .{ .status = .fail, .message = "dev inspect_str mismatch", .timings = timings },
            .err => |e| return .{ .status = .fail, .message = e, .timings = timings },
        }
    }

    if (!skip.wasm) {
        const result = runBackend(allocator, "wasm", helpers.wasmEvaluatorStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .wasm_ns);
        defer if (result.value == .ok) allocator.free(result.value.ok);
        switch (result.value) {
            .ok => |s| if (!std.mem.eql(u8, expected_str, s)) return .{ .status = .fail, .message = "wasm inspect_str mismatch", .timings = timings },
            .err => |e| return .{ .status = .fail, .message = e, .timings = timings },
        }
    }

    return .{ .status = .pass, .timings = timings };
}

//
// Cross-backend comparison — the core of this runner
//

/// Run a single compiled backend via Str.inspect and return a BackendResult.
fn runBackend(
    allocator: std.mem.Allocator,
    comptime name: []const u8,
    comptime evalFn: fn (std.mem.Allocator, *ModuleEnv, CIR.Expr.Idx, *const ModuleEnv) anyerror![]const u8,
    module_env: *ModuleEnv,
    inspect_expr: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
    timings: *EvalTimings,
    comptime timing_field: enum { interpreter_ns, dev_ns, wasm_ns, llvm_ns },
) BackendResult {
    var timer = Timer.start() catch unreachable;
    const result: BackendResult = blk: {
        const str = evalFn(allocator, module_env, inspect_expr, builtin_module_env) catch |err| {
            break :blk BackendResult{ .name = name, .value = .{ .err = @errorName(err) } };
        };
        break :blk BackendResult{ .name = name, .value = .{ .ok = str } };
    };
    @field(timings, @tagName(timing_field)) = timer.read();
    return result;
}

/// Run all backends (interpreter, dev, wasm) on the same expression via Str.inspect,
/// then compare results. Returns .pass if all backends agree, .fail with mismatch details.
/// NOTE: LLVM backend is temporarily disabled — it currently aliases the dev
/// backend (see helpers.llvmEvaluatorStr). Re-enable here when LLVM is fixed.
fn compareAllBackends(allocator: std.mem.Allocator, resources: ParsedResources, skip: TestCase.Skip) TestOutcome {
    var timings = EvalTimings{};

    // Wrap the expression in Str.inspect for all backends
    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch {
        return .{ .status = .pass };
    };

    // Run each backend (or skip).
    // Thread safety: each backend evaluator creates fresh instances per call.
    // The wasm evaluator's host-side heap pointer (wasm_heap_ptr) is threadlocal,
    // and bytebox ModuleInstances are per-call, so no cross-thread state is shared.
    const interp_result: BackendResult = if (skip.interpreter)
        BackendResult{ .name = "interpreter", .value = .{ .err = "skipped" } }
    else
        runBackend(allocator, "interpreter", helpers.lirInterpreterInspectedStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .interpreter_ns);
    defer if (interp_result.value == .ok) allocator.free(interp_result.value.ok);

    const dev_result: BackendResult = if (skip.dev)
        BackendResult{ .name = "dev", .value = .{ .err = "skipped" } }
    else
        runBackend(allocator, "dev", helpers.devEvaluatorStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .dev_ns);
    defer if (dev_result.value == .ok) allocator.free(dev_result.value.ok);

    const wasm_result: BackendResult = if (skip.wasm)
        BackendResult{ .name = "wasm", .value = .{ .err = "skipped" } }
    else
        runBackend(allocator, "wasm", helpers.wasmEvaluatorStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .wasm_ns);
    defer if (wasm_result.value == .ok) allocator.free(wasm_result.value.ok);

    // LLVM backend disabled — currently just aliases dev. See helpers.llvmEvaluatorStr.
    // When re-enabling, uncomment this and add llvm_result to all_backends below.
    // const llvm_result: BackendResult = if (skip.llvm)
    //     BackendResult{ .name = "llvm", .value = .{ .err = "skipped" } }
    // else
    //     runBackend(allocator, "llvm", helpers.llvmEvaluatorStr, resources.module_env, inspect_expr, resources.builtin_module.env, &timings, .llvm_ns);
    // defer if (llvm_result.value == .ok) allocator.free(llvm_result.value.ok);

    const all_backends = [_]BackendResult{
        interp_result,
        dev_result,
        wasm_result,
    };

    if (compareBackendResults(allocator, &all_backends)) |msg| {
        return .{ .status = .fail, .message = msg, .timings = timings };
    }

    return .{ .status = .pass, .timings = timings };
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

        // Set up crash protection
        var jmp_buf: sljmp.JmpBuf = undefined;
        panic_jmp = &jmp_buf;
        panic_msg = null;

        const jmp_result = sljmp.setjmp(&jmp_buf);
        if (jmp_result != 0) {
            panic_jmp = null;
            // Signal was blocked during the handler; unblock for future crashes.
            unblockCrashSignals();
            // Check if this was a watchdog timeout (jmp_result == 3) or a real crash.
            const was_timeout = if (my_state) |ws| ws.timed_out.swap(false, .acquire) else false;
            const elapsed = wall_timer.read();
            const raw_msg = panic_msg orelse "unknown crash";
            // Dup to GPA so all result messages are GPA-owned (freed uniformly in main).
            const stable_msg = ctx.msg_allocator.dupe(u8, raw_msg) catch raw_msg;
            ctx.results[i] = .{
                .status = if (was_timeout or jmp_result == 3) .timeout else .crash,
                .message = stable_msg,
                .duration_ns = elapsed,
                .timings = .{},
            };
            if (my_state) |ws| ws.start_time_ms.store(0, .release);
            continue;
        }

        const outcome = runSingleTest(allocator, tc);

        panic_jmp = null;
        if (my_state) |ws| ws.start_time_ms.store(0, .release);
        const elapsed = wall_timer.read();

        // Dup the message to the stable GPA so it survives arena reset.
        // All messages in results must be GPA-owned (freed uniformly in main).
        const stable_msg: ?[]const u8 = if (outcome.message) |msg|
            (ctx.msg_allocator.dupe(u8, msg) catch null)
        else
            null;

        ctx.results[i] = .{
            .status = outcome.status,
            .message = stable_msg,
            .duration_ns = elapsed,
            .timings = outcome.timings,
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
    coverage: bool = false,
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
        } else if (std.mem.eql(u8, args[i], "--coverage")) {
            result.coverage = true;
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
        \\and compares results via Str.inspect. Crash protection via setjmp/longjmp
        \\allows the runner to recover from segfaults and continue.
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
        \\  --coverage            Coverage mode: single-threaded, no fork. Use with kcov.
        \\  --timeout <MS>        Per-test hang timeout in ms (default: 10000). Multi-thread only.
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
        \\    CRASH - segfault or panic in generated code (recovered via signal handler)
        \\    HANG  - test exceeded the per-test timeout (killed by watchdog)
        \\    SKIP  - one or more backends were skipped
        \\
        \\EXIT CODE:
        \\  0 if all tests pass or skip, 1 if any test fails or crashes.
        \\
    ;
    std.debug.print("{s}", .{help});
}

//
// Timing display helpers
//

fn writeTimingBreakdown(t: EvalTimings) void {
    const fields = [_]struct { name: []const u8, ns: u64 }{
        .{ .name = "parse", .ns = t.parse_ns },
        .{ .name = "can", .ns = t.canonicalize_ns },
        .{ .name = "check", .ns = t.typecheck_ns },
        .{ .name = "interp", .ns = t.interpreter_ns },
        .{ .name = "dev", .ns = t.dev_ns },
        .{ .name = "wasm", .ns = t.wasm_ns },
    };
    var has_any = false;
    for (fields) |f| {
        if (f.ns > 0) {
            has_any = true;
            break;
        }
    }
    if (!has_any) {
        std.debug.print("\n", .{});
        return;
    }
    std.debug.print("  [", .{});
    var first = true;
    for (fields) |f| {
        if (f.ns > 0) {
            if (!first) std.debug.print(" ", .{});
            first = false;
            const fms = @as(f64, @floatFromInt(f.ns)) / 1_000_000.0;
            std.debug.print("{s}:{d:.1}", .{ f.name, fms });
        }
    }
    std.debug.print("]\n", .{});
}

/// Print per-backend status summary for failed/crashed tests.
/// Uses timing info and skip flags to infer what happened in each backend.
fn writeBackendSummary(t: EvalTimings, skip: TestCase.Skip) void {
    const Backend = struct { name: []const u8, skipped: bool, ran: bool };
    const backends = [_]Backend{
        .{ .name = "interp", .skipped = skip.interpreter, .ran = t.interpreter_ns > 0 },
        .{ .name = "dev", .skipped = skip.dev, .ran = t.dev_ns > 0 },
        .{ .name = "wasm", .skipped = skip.wasm, .ran = t.wasm_ns > 0 },
    };
    std.debug.print("        backends:", .{});
    for (backends) |b| {
        if (b.skipped) {
            std.debug.print(" {s}=skip", .{b.name});
        } else if (b.ran) {
            std.debug.print(" {s}=ran({d:.1}ms)", .{ b.name, @as(f64, @floatFromInt(
                if (std.mem.eql(u8, b.name, "interp")) t.interpreter_ns else if (std.mem.eql(u8, b.name, "dev")) t.dev_ns else t.wasm_ns,
            )) / 1_000_000.0 });
        } else {
            std.debug.print(" {s}=not_reached", .{b.name});
        }
    }
    std.debug.print("\n", .{});
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
fn hangWatchdog(ctx: *RunnerContext, threads: []std.Thread, timeout_ms: u64) void {
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
                // This worker is hung. Mark it timed-out and kill it.
                worker.timed_out.store(true, .release);
                const test_name = if (test_idx < ctx.tests.len) ctx.tests[test_idx].name else "?";
                std.debug.print("\n  HANG  {s}  ({d}ms) — killing", .{ test_name, elapsed_ms });
                if (comptime builtin.os.tag != .windows) {
                    // Kill any forked child process first (unblocks waitpid).
                    if (idx < helpers.worker_child_pids.len) {
                        const cpid = helpers.worker_child_pids[idx].swap(0, .acq_rel);
                        if (cpid > 0) {
                            std.debug.print(" child(pid={d})", .{cpid});
                            posix.kill(@intCast(cpid), posix.SIG.KILL) catch {};
                        }
                    }
                    // Then signal the worker thread to longjmp out.
                    const handle = threads[idx].getHandle();
                    _ = pthread_kill(handle, posix.SIG.USR1);
                }
                std.debug.print("\n", .{});
                // Give the worker time to recover before re-checking.
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

    // Coverage mode: disable fork (kcov can't trace forked children) and
    // force single-threaded so kcov sees deterministic execution.
    if (cli.coverage) {
        helpers.force_no_fork = true;
    }

    installCrashSignalHandlers();

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
        std.debug.print("No tests matched filter.\n", .{});
        return;
    }

    const cpu_count = std.Thread.getCpuCount() catch 1;
    const thread_count: usize = if (cli.coverage)
        1
    else if (cli.threads > 0)
        @min(cli.threads, cpu_count)
    else
        @min(cpu_count, tests.len);

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash, .message = null, .duration_ns = 0, .timings = .{} });

    var wall_timer = Timer.start() catch unreachable;

    // Default timeout: 5s in multi-threaded mode, disabled in single-threaded/coverage.
    const hang_timeout_ms: u64 = if (thread_count <= 1)
        0
    else if (cli.timeout_ms > 0)
        cli.timeout_ms
    else
        10_000; // 10 seconds

    // Allocate per-worker state for hang detection (multi-threaded only).
    const worker_states: ?[]WorkerState = if (thread_count > 1) blk: {
        const ws = try gpa.alloc(WorkerState, thread_count);
        for (ws) |*w| w.* = .{};
        break :blk ws;
    } else null;
    defer if (worker_states) |ws| gpa.free(ws);

    // Allocate per-worker child PID tracking for fork-based isolation.
    const child_pids = try gpa.alloc(std.atomic.Value(i32), thread_count);
    defer gpa.free(child_pids);
    for (child_pids) |*p| p.* = std.atomic.Value(i32).init(0);
    helpers.worker_child_pids = child_pids;

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
        threadMain(&context);
    } else {
        const threads = try gpa.alloc(std.Thread, thread_count);
        defer gpa.free(threads);
        for (threads) |*t| {
            t.* = try std.Thread.spawn(.{}, threadMain, .{&context});
        }

        // Watchdog loop: poll workers for hangs until all are done.
        if (hang_timeout_ms > 0) {
            hangWatchdog(&context, threads, hang_timeout_ms);
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
                std.debug.print("  FAIL  {s}  ({d:.1}ms)", .{ tc.name, ms });
                writeTimingBreakdown(t);
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                writeBackendSummary(t, tc.skip);
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms)", .{ tc.name, ms });
                writeTimingBreakdown(t);
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
                writeBackendSummary(t, tc.skip);
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
            // Only free messages that were duped to the GPA (not static strings).
            // We duped all messages conservatively, so free them all. Static string
            // dups are harmless tiny allocations.
            gpa.free(msg);
        }
    }

    // Performance summary (skip in coverage mode — kcov instrumentation skews timings)
    if (cli.coverage) {
        std.debug.print("\n  (timings omitted — coverage mode; kcov instrumentation affects measurements)\n", .{});
    } else if (tests.len > 0) {
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
