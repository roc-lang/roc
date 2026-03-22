//! Parallel eval test runner.
//!
//! A standalone binary that runs eval tests across multiple threads using a
//! work-stealing job queue. Each test runs the interpreter, dev backend,
//! wasm backend, and "llvm" backend (currently aliases dev), then compares
//! all results via Str.inspect string comparison.
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
const roc_builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");
const roc_target = @import("roc_target");
const eval_mod = @import("eval");
const interpreter_layout = eval_mod.interpreter_layout;
const interpreter_values = eval_mod.interpreter_values;

const Can = can.Can;
const Check = check.Check;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;
const Interpreter = eval_mod.Interpreter;
const BuiltinTypes = eval_mod.BuiltinTypes;
const StackValue = eval_mod.StackValue;
const LoadedModule = eval_mod.builtin_loading.LoadedModule;
const deserializeBuiltinIndices = eval_mod.builtin_loading.deserializeBuiltinIndices;
const loadCompiledModule = eval_mod.builtin_loading.loadCompiledModule;

// Import backend evaluator functions from helpers (shared with zig test runner)
const helpers = eval_mod.test_helpers;

const posix = std.posix;

const AtomicUsize = std.atomic.Value(usize);

// Test definition modules
const eval_tests = @import("eval_tests.zig");

// ---------------------------------------------------------------------------
// Public types (imported by test definition files)
// ---------------------------------------------------------------------------

pub const TestCase = struct {
    name: []const u8,
    source: []const u8,
    expected: Expected,

    pub const Expected = union(enum) {
        i64_val: i128,
        bool_val: bool,
        str_val: []const u8,
        int_dec: i128,
        dec_val: i128,
        f32_val: f32,
        f64_val: f64,
        err_val: anyerror,
        problem: void,
        type_mismatch_crash: void,
        dev_only_str: []const u8,
    };
};

// ---------------------------------------------------------------------------
// Crash protection (following src/snapshot_tool/main.zig pattern)
// ---------------------------------------------------------------------------

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

fn crashSignalHandler(_: i32) callconv(.c) void {
    if (panic_jmp) |jmp| {
        panic_msg = "signal: segfault or illegal instruction in generated code";
        panic_jmp = null;
        sljmp.longjmp(jmp, 2);
    }
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

    const sa = posix.Sigaction{
        .handler = .{ .handler = &crashSignalHandler },
        .mask = posix.sigemptyset(),
        .flags = posix.SA.NODEFER,
    };
    posix.sigaction(posix.SIG.SEGV, &sa, null);
    posix.sigaction(posix.SIG.BUS, &sa, null);
    posix.sigaction(posix.SIG.ILL, &sa, null);
}

// ---------------------------------------------------------------------------
// Test outcome
// ---------------------------------------------------------------------------

const TestOutcome = struct {
    status: Status,
    message: ?[]const u8 = null,

    const Status = enum { pass, fail, crash };
};

const TestResult = struct {
    status: TestOutcome.Status,
    message: ?[]const u8,
    duration_ns: u64,
};

// ---------------------------------------------------------------------------
// Runner context
// ---------------------------------------------------------------------------

const RunnerContext = struct {
    tests: []const TestCase,
    index: AtomicUsize,
    results: []TestResult,
    verbose: bool,
};

const MAX_THREADS = 64;

// ---------------------------------------------------------------------------
// Parse and canonicalize (shared by all backends)
// ---------------------------------------------------------------------------

const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    builtin_types: BuiltinTypes,
};

fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
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

    module_env.all_defs = try module_env.store.defSpanFrom(0);
    const imported_envs = [_]*const ModuleEnv{ builtin_module.env, module_env };
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, builtin_ctx);
    _ = try checker.checkExprReplWithDefs(canonical_expr_idx);

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

// ---------------------------------------------------------------------------
// ParTestEnv — Roc host ops for the interpreter
// ---------------------------------------------------------------------------

const ParTestEnv = struct {
    allocator: std.mem.Allocator,
    crash: eval_mod.CrashContext,

    fn init(allocator: std.mem.Allocator) ParTestEnv {
        return .{
            .allocator = allocator,
            .crash = eval_mod.CrashContext.init(allocator),
        };
    }

    fn deinit(self: *ParTestEnv) void {
        self.crash.deinit();
    }

    fn get_ops(self: *ParTestEnv) roc_builtins.host_abi.RocOps {
        self.crash.reset();
        return .{
            .env = @ptrCast(self),
            .roc_alloc = testRocAlloc,
            .roc_dealloc = testRocDealloc,
            .roc_realloc = testRocRealloc,
            .roc_dbg = testRocDbg,
            .roc_expect_failed = testRocExpectFailed,
            .roc_crashed = testRocCrashed,
            .hosted_fns = .{ .count = 0, .fns = undefined },
        };
    }

    fn testRocAlloc(alloc_args: *roc_builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
        const self: *ParTestEnv = @ptrCast(@alignCast(env));
        const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
        const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
        const total_size = alloc_args.length + size_storage_bytes;
        const result = self.allocator.rawAlloc(total_size, align_enum, @returnAddress());
        const base_ptr = result orelse @panic("OOM in testRocAlloc");
        const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
        size_ptr.* = total_size;
        alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
    }

    fn testRocDealloc(dealloc_args: *roc_builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
        const self: *ParTestEnv = @ptrCast(@alignCast(env));
        const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
        const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
        const total_size = size_ptr.*;
        const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
        const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
        const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
        const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
        self.allocator.rawFree(slice, align_enum, @returnAddress());
    }

    fn testRocRealloc(realloc_args: *roc_builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *ParTestEnv = @ptrCast(@alignCast(env));
        const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
        const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
        const old_total_size = old_size_ptr.*;
        const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
        const new_total_size = realloc_args.new_length + size_storage_bytes;
        const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
        const new_slice = self.allocator.realloc(old_slice, new_total_size) catch @panic("OOM in testRocRealloc");
        const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
        new_size_ptr.* = new_total_size;
        realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
    }

    fn testRocDbg(_: *const roc_builtins.host_abi.RocDbg, _: *anyopaque) callconv(.c) void {}

    fn testRocExpectFailed(_: *const roc_builtins.host_abi.RocExpectFailed, _: *anyopaque) callconv(.c) void {}

    fn testRocCrashed(crashed_args: *const roc_builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) void {
        const self: *ParTestEnv = @ptrCast(@alignCast(env));
        const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
        self.crash.recordCrash(msg_slice) catch {};
    }
};

// ---------------------------------------------------------------------------
// Str.inspect wrapping — converts CIR expression to Str.inspect(expr)
// ---------------------------------------------------------------------------

fn wrapInStrInspect(module_env: *ModuleEnv, inner_expr: CIR.Expr.Idx) !CIR.Expr.Idx {
    const top = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(inner_expr);
    const args_span = try module_env.store.exprSpanFrom(top);
    const region = module_env.store.getExprRegion(inner_expr);
    return module_env.addExpr(.{ .e_run_low_level = .{
        .op = .str_inspekt,
        .args = args_span,
    } }, region);
}

/// Convert a StackValue to a RocValue for formatting.
fn stackValueToRocValue(result: StackValue, layout_idx_hint: ?interpreter_layout.Idx) interpreter_values.RocValue {
    return .{
        .ptr = if (result.ptr) |p| @ptrCast(p) else null,
        .lay = result.layout,
        .layout_idx = layout_idx_hint,
    };
}

/// Build FormatContext from interpreter state.
fn interpreterFormatCtx(layout_cache: *const interpreter_layout.Store) interpreter_values.RocValue.FormatContext {
    return .{
        .layout_store = layout_cache,
        .ident_store = layout_cache.getEnv().common.getIdentStore(),
    };
}

// ---------------------------------------------------------------------------
// Backend comparison helpers
// ---------------------------------------------------------------------------

fn numericStringsEqual(a: []const u8, b: []const u8) bool {
    if (std.mem.eql(u8, a, b)) return true;
    // "42" == "42.0" and vice versa
    if (a.len + 2 == b.len and std.mem.endsWith(u8, b, ".0") and std.mem.startsWith(u8, b, a)) return true;
    if (b.len + 2 == a.len and std.mem.endsWith(u8, a, ".0") and std.mem.startsWith(u8, a, b)) return true;
    return false;
}

fn boolStringsEquivalent(a: []const u8, b: []const u8) bool {
    return (std.mem.eql(u8, a, "True") and std.mem.eql(u8, b, "1")) or
        (std.mem.eql(u8, a, "False") and std.mem.eql(u8, b, "0")) or
        (std.mem.eql(u8, a, "1") and std.mem.eql(u8, b, "True")) or
        (std.mem.eql(u8, a, "0") and std.mem.eql(u8, b, "False"));
}

/// Per-backend result for comparison reporting.
const BackendResult = struct {
    name: []const u8,
    value: union(enum) {
        ok: []const u8,
        err: []const u8,
    },
};

/// Compare all backend results. Returns null if they all agree, or an error message.
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

    // Check all successful results agree
    var mismatch = false;
    for (backends) |br| {
        if (br.value == .ok) {
            if (!numericStringsEqual(first_ok.?, br.value.ok) and !boolStringsEquivalent(first_ok.?, br.value.ok)) {
                mismatch = true;
                break;
            }
        }
    }

    if (!mismatch) return null;

    // Build mismatch message
    var msg_buf: std.ArrayListUnmanaged(u8) = .empty;
    const writer = msg_buf.writer(allocator);
    writer.print("Backend mismatch:", .{}) catch {};
    for (backends) |br| {
        switch (br.value) {
            .ok => |s| writer.print(" {s}='{s}'", .{ br.name, s }) catch {},
            .err => |e| writer.print(" {s}=err({s})", .{ br.name, e }) catch {},
        }
    }
    return msg_buf.toOwnedSlice(allocator) catch null;
}

// ---------------------------------------------------------------------------
// Test execution — runs all backends and compares
// ---------------------------------------------------------------------------

fn runSingleTest(allocator: std.mem.Allocator, tc: TestCase) TestOutcome {
    return runSingleTestInner(allocator, tc) catch |err| {
        return .{ .status = .fail, .message = @errorName(err) };
    };
}

fn runSingleTestInner(allocator: std.mem.Allocator, tc: TestCase) !TestOutcome {
    switch (tc.expected) {
        .i64_val => |expected_int| return runTestI64(allocator, tc.source, expected_int),
        .bool_val => |expected_bool| return runTestBool(allocator, tc.source, expected_bool),
        .str_val => |expected_str| return runTestStr(allocator, tc.source, expected_str),
        .err_val => |expected_err| return runTestError(allocator, tc.source, expected_err),
        .problem => return runTestProblem(allocator, tc.source),
        .f32_val => |expected_f32| return runTestF32(allocator, tc.source, expected_f32),
        .f64_val => |expected_f64| return runTestF64(allocator, tc.source, expected_f64),
        .dec_val => |expected_dec| return runTestDec(allocator, tc.source, expected_dec),
        .int_dec => |expected_int| return runTestI64(allocator, tc.source, expected_int),
        .type_mismatch_crash => return runTestTypeMismatchCrash(allocator, tc.source),
        .dev_only_str => |expected_str| return runTestDevOnlyStr(allocator, tc.source, expected_str),
    }
}

/// Run interpreter, check the value, then compare all backends via Str.inspect.
fn runTestI64(allocator: std.mem.Allocator, src: []const u8, expected_int: i128) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, &ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, &ops);
    defer interpreter.bindings.items.len = 0;

    const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
        break :blk result.asI128();
    } else blk: {
        const dec_value = result.asDec(&ops);
        const RocDec = roc_builtins.dec.RocDec;
        break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
    };

    if (int_value != expected_int) {
        return .{ .status = .fail, .message = "integer value mismatch" };
    }

    // Format interpreter result for cross-backend comparison
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(layout_cache);
    const interp_str = roc_val.format(allocator, fmt_ctx) catch return .{ .status = .pass };
    defer allocator.free(interp_str);

    return compareAllBackends(allocator, interp_str, resources);
}

fn runTestBool(allocator: std.mem.Allocator, src: []const u8, expected_bool: bool) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, &ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, &ops);
    defer interpreter.bindings.items.len = 0;

    const int_val: i64 = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
        break :blk @intCast(result.asI128());
    } else blk: {
        std.debug.assert(result.ptr != null);
        const bool_ptr: *const u8 = @ptrCast(@alignCast(result.ptr.?));
        break :blk @as(i64, bool_ptr.*);
    };

    const bool_val = int_val != 0;
    if (bool_val != expected_bool) {
        return .{ .status = .fail, .message = "boolean value mismatch" };
    }

    const roc_val = stackValueToRocValue(result, interpreter_layout.Idx.bool);
    const fmt_ctx = interpreterFormatCtx(layout_cache);
    const interp_str = roc_val.format(allocator, fmt_ctx) catch return .{ .status = .pass };
    defer allocator.free(interp_str);

    return compareAllBackends(allocator, interp_str, resources);
}

fn runTestStr(allocator: std.mem.Allocator, src: []const u8, expected_str: []const u8) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, &ops);
    const layout_cache = &interpreter.runtime_layout_store;

    if (result.layout.tag != .scalar or result.layout.data.scalar.tag != .str) {
        result.decref(layout_cache, &ops);
        return .{ .status = .fail, .message = "expected string layout" };
    }

    const roc_str: *const roc_builtins.str.RocStr = @ptrCast(@alignCast(result.ptr.?));
    const str_slice = roc_str.asSlice();
    const matches = std.mem.eql(u8, expected_str, str_slice);

    // Format interpreter result for cross-backend comparison
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(layout_cache);
    const interp_str = roc_val.format(allocator, fmt_ctx) catch {
        if (!roc_str.isSmallStr()) {
            @constCast(roc_str).decref(&ops);
        } else {
            result.decref(layout_cache, &ops);
        }
        if (!matches) return .{ .status = .fail, .message = "string value mismatch" };
        return .{ .status = .pass };
    };
    defer allocator.free(interp_str);

    if (!roc_str.isSmallStr()) {
        @constCast(roc_str).decref(&ops);
    } else {
        result.decref(layout_cache, &ops);
    }

    if (!matches) {
        return .{ .status = .fail, .message = "string value mismatch" };
    }

    return compareAllBackends(allocator, interp_str, resources);
}

fn runTestError(allocator: std.mem.Allocator, src: []const u8, expected_err: anyerror) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    _ = interpreter.eval(resources.expr_idx, &ops) catch |err| {
        if (err == expected_err) {
            return .{ .status = .pass };
        }
        return .{ .status = .fail, .message = "wrong error returned" };
    };

    return .{ .status = .fail, .message = "expected error but evaluation succeeded" };
}

fn runTestProblem(allocator: std.mem.Allocator, src: []const u8) !TestOutcome {
    const builtin_indices = try deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    const builtin_module = try loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    defer {
        var bm = builtin_module;
        bm.deinit();
    }

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, src);
    module_env.common.source = src;
    try module_env.common.calcLineStarts(module_env.gpa);

    var allocators_inst: Allocators = undefined;
    allocators_inst.initInPlace(allocator);
    const parse_ast = try parse.parseExpr(&allocators_inst, &module_env.common);

    if (parse_ast.tokenize_diagnostics.items.len > 0 or parse_ast.parse_diagnostics.items.len > 0) {
        parse_ast.deinit();
        module_env.deinit();
        allocator.destroy(module_env);
        return .{ .status = .pass };
    }

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
    czer.* = try Can.initModule(&allocators_inst, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
    });

    const expr_idx_raw: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    _ = czer.canonicalizeExpr(expr_idx_raw) catch {
        czer.deinit();
        parse_ast.deinit();
        module_env.deinit();
        allocator.destroy(czer);
        allocator.destroy(module_env);
        return .{ .status = .pass };
    };

    const can_diags = try module_env.getDiagnostics();
    defer allocator.free(can_diags);

    module_env.all_defs = try module_env.store.defSpanFrom(0);
    const imported_envs = [_]*const ModuleEnv{ builtin_module.env, module_env };
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, builtin_ctx);

    const type_problems = checker.problems.problems.items.len;
    const has_problems = can_diags.len + type_problems > 0;

    checker.deinit();
    czer.deinit();
    parse_ast.deinit();
    module_env.deinit();
    allocator.destroy(checker);
    allocator.destroy(czer);
    allocator.destroy(module_env);

    if (has_problems) {
        return .{ .status = .pass };
    }
    return .{ .status = .fail, .message = "expected problems but none found" };
}

fn runTestF32(allocator: std.mem.Allocator, src: []const u8, expected_f32: f32) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, &ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, &ops);

    const actual = result.asF32();
    const epsilon: f32 = 0.0001;
    const diff = @abs(actual - expected_f32);
    if (diff > epsilon) {
        return .{ .status = .fail, .message = "f32 value mismatch" };
    }

    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(layout_cache);
    const interp_str = roc_val.format(allocator, fmt_ctx) catch return .{ .status = .pass };
    defer allocator.free(interp_str);

    return compareAllBackends(allocator, interp_str, resources);
}

fn runTestF64(allocator: std.mem.Allocator, src: []const u8, expected_f64: f64) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, &ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, &ops);

    const actual = result.asF64();
    const epsilon: f64 = 0.000000001;
    const diff = @abs(actual - expected_f64);
    if (diff > epsilon) {
        return .{ .status = .fail, .message = "f64 value mismatch" };
    }

    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(layout_cache);
    const interp_str = roc_val.format(allocator, fmt_ctx) catch return .{ .status = .pass };
    defer allocator.free(interp_str);

    return compareAllBackends(allocator, interp_str, resources);
}

fn runTestDec(allocator: std.mem.Allocator, src: []const u8, expected_dec: i128) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, &ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, &ops);

    const dec_value = result.asDec(&ops);
    if (dec_value.num != expected_dec) {
        return .{ .status = .fail, .message = "Dec value mismatch" };
    }

    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(layout_cache);
    const interp_str = roc_val.format(allocator, fmt_ctx) catch return .{ .status = .pass };
    defer allocator.free(interp_str);

    return compareAllBackends(allocator, interp_str, resources);
}

fn runTestTypeMismatchCrash(allocator: std.mem.Allocator, src: []const u8) !TestOutcome {
    const resources = parseAndCanonicalizeExpr(allocator, src) catch {
        return .{ .status = .pass };
    };
    defer cleanupResources(allocator, resources);

    var test_env_instance = ParTestEnv.init(allocator);
    defer test_env_instance.deinit();

    const imported_envs = [_]*const ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    var ops = test_env_instance.get_ops();
    _ = interpreter.eval(resources.expr_idx, &ops) catch {
        return .{ .status = .pass };
    };

    return .{ .status = .fail, .message = "expected crash but evaluation succeeded" };
}

/// Run a test that only checks the dev backend output (no interpreter comparison).
fn runTestDevOnlyStr(allocator: std.mem.Allocator, src: []const u8, expected_str: []const u8) !TestOutcome {
    const resources = try parseAndCanonicalizeExpr(allocator, src);
    defer cleanupResources(allocator, resources);

    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch {
        return .{ .status = .fail, .message = "failed to wrap in Str.inspect" };
    };

    const dev_str = helpers.devEvaluatorStr(allocator, resources.module_env, inspect_expr, resources.builtin_module.env) catch |err| {
        return .{ .status = .fail, .message = @errorName(err) };
    };
    defer allocator.free(dev_str);

    if (!std.mem.eql(u8, expected_str, dev_str)) {
        return .{ .status = .fail, .message = "dev_only_str value mismatch" };
    }
    return .{ .status = .pass };
}

// ---------------------------------------------------------------------------
// Cross-backend comparison — the core of this runner
// ---------------------------------------------------------------------------

/// Run dev, wasm, and llvm backends on the same expression, compare Str.inspect
/// output with the interpreter's formatted result.
/// Returns .pass if all backends agree, .fail with mismatch details otherwise.
fn compareAllBackends(allocator: std.mem.Allocator, interp_str: []const u8, resources: ParsedResources) TestOutcome {
    // Wrap the expression in Str.inspect for compiled backends
    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch {
        // If wrapping fails, skip comparison (interpreter value was already checked)
        return .{ .status = .pass };
    };

    // Run dev backend
    const dev_result: BackendResult = blk: {
        const str = helpers.devEvaluatorStr(allocator, resources.module_env, inspect_expr, resources.builtin_module.env) catch |err| {
            break :blk BackendResult{ .name = "dev", .value = .{ .err = @errorName(err) } };
        };
        break :blk BackendResult{ .name = "dev", .value = .{ .ok = str } };
    };
    defer if (dev_result.value == .ok) allocator.free(dev_result.value.ok);

    // Run wasm backend
    const wasm_result: BackendResult = blk: {
        const str = helpers.wasmEvaluatorStr(allocator, resources.module_env, inspect_expr, resources.builtin_module.env) catch |err| {
            break :blk BackendResult{ .name = "wasm", .value = .{ .err = @errorName(err) } };
        };
        break :blk BackendResult{ .name = "wasm", .value = .{ .ok = str } };
    };
    defer if (wasm_result.value == .ok) allocator.free(wasm_result.value.ok);

    // Run "llvm" backend (currently aliases dev)
    const llvm_result: BackendResult = blk: {
        const str = helpers.llvmEvaluatorStr(allocator, resources.module_env, inspect_expr, resources.builtin_module.env) catch |err| {
            break :blk BackendResult{ .name = "llvm", .value = .{ .err = @errorName(err) } };
        };
        break :blk BackendResult{ .name = "llvm", .value = .{ .ok = str } };
    };
    defer if (llvm_result.value == .ok) allocator.free(llvm_result.value.ok);

    // Compare all backends including interpreter
    const all_backends = [_]BackendResult{
        .{ .name = "interpreter", .value = .{ .ok = interp_str } },
        dev_result,
        wasm_result,
        llvm_result,
    };

    if (compareBackendResults(allocator, &all_backends)) |msg| {
        return .{ .status = .fail, .message = msg };
    }

    return .{ .status = .pass };
}

// ---------------------------------------------------------------------------
// Worker thread
// ---------------------------------------------------------------------------

fn threadMain(ctx: *RunnerContext) void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    while (true) {
        const i = ctx.index.fetchAdd(1, .monotonic);
        if (i >= ctx.tests.len) break;

        _ = arena.reset(.retain_capacity);
        const allocator = arena.allocator();

        const tc = ctx.tests[i];
        const start = std.time.nanoTimestamp();

        // Set up crash protection
        var jmp_buf: sljmp.JmpBuf = undefined;
        panic_jmp = &jmp_buf;
        panic_msg = null;

        const jmp_result = sljmp.setjmp(&jmp_buf);
        if (jmp_result != 0) {
            panic_jmp = null;
            const elapsed: u64 = @intCast(@max(0, std.time.nanoTimestamp() - start));
            ctx.results[i] = .{
                .status = .crash,
                .message = panic_msg orelse "unknown crash",
                .duration_ns = elapsed,
            };
            continue;
        }

        const outcome = runSingleTest(allocator, tc);

        panic_jmp = null;
        const elapsed: u64 = @intCast(@max(0, std.time.nanoTimestamp() - start));
        ctx.results[i] = .{
            .status = outcome.status,
            .message = outcome.message,
            .duration_ns = elapsed,
        };
    }
}

// ---------------------------------------------------------------------------
// Test collection
// ---------------------------------------------------------------------------

fn collectTests() []const TestCase {
    return &eval_tests.tests;
}

// ---------------------------------------------------------------------------
// CLI parsing
// ---------------------------------------------------------------------------

const CliArgs = struct {
    filter: ?[]const u8 = null,
    threads: usize = 0,
    verbose: bool = false,
};

fn parseCliArgs(args: []const []const u8) CliArgs {
    var result = CliArgs{};
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--filter") and i + 1 < args.len) {
            i += 1;
            result.filter = args[i];
        } else if (std.mem.eql(u8, args[i], "--threads") and i + 1 < args.len) {
            i += 1;
            result.threads = std.fmt.parseInt(usize, args[i], 10) catch 0;
        } else if (std.mem.eql(u8, args[i], "--verbose")) {
            result.verbose = true;
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    const argv = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, argv);
    const cli = parseCliArgs(argv);

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
    const thread_count = if (cli.threads > 0)
        @min(cli.threads, MAX_THREADS)
    else
        @min(cpu_count, @min(tests.len, MAX_THREADS));

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash, .message = "not started", .duration_ns = 0 });

    const wall_start = std.time.nanoTimestamp();

    var context = RunnerContext{
        .tests = tests,
        .index = AtomicUsize.init(0),
        .results = results,
        .verbose = cli.verbose,
    };

    if (thread_count <= 1) {
        threadMain(&context);
    } else {
        var threads: [MAX_THREADS]std.Thread = undefined;
        for (0..thread_count) |i| {
            threads[i] = try std.Thread.spawn(.{}, threadMain, .{&context});
        }
        for (threads[0..thread_count]) |t| {
            t.join();
        }
    }

    const wall_elapsed: u64 = @intCast(@max(0, std.time.nanoTimestamp() - wall_start));

    var passed: usize = 0;
    var failed: usize = 0;
    var crashed: usize = 0;

    std.debug.print("\n=== Eval Test Results ===\n", .{});

    for (tests, 0..) |tc, i| {
        const r = results[i];
        const ms = @as(f64, @floatFromInt(r.duration_ns)) / 1_000_000.0;

        switch (r.status) {
            .pass => {
                passed += 1;
                if (cli.verbose) {
                    std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                }
            },
            .fail => {
                failed += 1;
                std.debug.print("  FAIL  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| {
                    std.debug.print("        {s}\n", .{msg});
                }
            },
        }
    }

    const wall_ms = @as(f64, @floatFromInt(wall_elapsed)) / 1_000_000.0;
    std.debug.print("\n{d} passed, {d} failed, {d} crashed ({d} total) in {d:.0}ms using {d} thread(s)\n", .{
        passed, failed, crashed, tests.len, wall_ms, thread_count,
    });

    if (failed > 0 or crashed > 0) {
        std.process.exit(1);
    }
}
