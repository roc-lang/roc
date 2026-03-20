//! Unified evaluation runner for Roc expressions.
//!
//! Dispatches to the appropriate backend (interpreter, dev, or wasm) based on
//! a comptime `EvalBackend` parameter, enabling dead-code elimination when
//! only a single backend is needed.
//!
//! Consolidates the duplicated `runViaDev()` that previously lived in both
//! `cli/main.zig` and `glue/glue.zig`.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const layout = @import("layout");
const builtins = @import("builtins");
const roc_target = @import("roc_target");

const eval_mod = @import("mod.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const RocOps = builtins.host_abi.RocOps;
const DevEvaluator = eval_mod.DevEvaluator;
const ExecutableMemory = eval_mod.ExecutableMemory;
const BuiltinModules = eval_mod.BuiltinModules;
const EvalBackend = eval_mod.EvalBackend;

pub const RunError = error{
    EvalFailed,
    CompilationFailed,
    OutOfMemory,
};

/// Run a compiled Roc entrypoint expression via the given backend.
///
/// The `comptime eval_backend` parameter enables dead-code elimination:
/// when embedding libroc with only one backend, the other backends' code
/// (and all their transitive dependencies) are eliminated at compile time.
///
/// All backends write their result into `result_ptr`.
pub fn run(
    comptime eval_backend: EvalBackend,
    gpa: Allocator,
    platform_env: *ModuleEnv,
    builtin_modules: ?*const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
    target: roc_target.RocTarget,
) RunError!void {
    switch (eval_backend) {
        .dev, .llvm => try runViaDev(
            gpa,
            platform_env,
            all_module_envs,
            app_module_env,
            entrypoint_expr,
            roc_ops,
            args_ptr,
            result_ptr,
        ),
        .interpreter, .wasm => try runViaInterpreter(
            gpa,
            platform_env,
            builtin_modules orelse return error.EvalFailed,
            all_module_envs,
            app_module_env,
            entrypoint_expr,
            roc_ops,
            args_ptr,
            result_ptr,
            target,
        ),
    }
}

/// Runtime dispatch wrapper for CLI / REPL where the backend is selected at
/// runtime via command-line flags. Calls `run` with a comptime backend for
/// each variant, so the compiler still generates specialized code.
pub fn runtimeRun(
    eval_backend: EvalBackend,
    gpa: Allocator,
    platform_env: *ModuleEnv,
    builtin_modules: ?*const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
    target: roc_target.RocTarget,
) RunError!void {
    switch (eval_backend) {
        inline else => |comptime_backend| try run(
            comptime_backend,
            gpa,
            platform_env,
            builtin_modules,
            all_module_envs,
            app_module_env,
            entrypoint_expr,
            roc_ops,
            args_ptr,
            result_ptr,
            target,
        ),
    }
}

// ──────────────────────────────────────────────────────────────
// Backend implementations (private)
// ──────────────────────────────────────────────────────────────

/// Run via the dev backend: JIT-compile CIR to native code and execute.
fn runViaDev(
    gpa: Allocator,
    platform_env: *ModuleEnv,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
) RunError!void {
    var dev_eval = DevEvaluator.init(gpa, null) catch {
        return error.EvalFailed;
    };
    defer dev_eval.deinit();

    // Resolve entrypoint layouts from the CIR expression's type
    const layout_store_ptr = dev_eval.ensureGlobalLayoutStore(all_module_envs) catch return error.EvalFailed;
    const module_idx: u32 = for (all_module_envs, 0..) |env, i| {
        if (env == platform_env) break @intCast(i);
    } else return error.EvalFailed;

    const expr_type_var = ModuleEnv.varFrom(entrypoint_expr);
    const resolved_type = platform_env.types.resolveVar(expr_type_var);
    const maybe_func = resolved_type.desc.content.unwrapFunc();

    var arg_layouts_buf: [16]layout.Idx = undefined;
    var arg_layouts_len: usize = 0;
    var ret_layout: layout.Idx = undefined;

    if (maybe_func) |func| {
        const arg_vars = platform_env.types.sliceVars(func.args);
        var type_scope = types.TypeScope.init(gpa);
        defer type_scope.deinit();
        for (arg_vars, 0..) |arg_var, i| {
            arg_layouts_buf[i] = layout_store_ptr.fromTypeVar(module_idx, arg_var, &type_scope, null) catch return error.EvalFailed;
        }
        arg_layouts_len = arg_vars.len;
        ret_layout = layout_store_ptr.fromTypeVar(module_idx, func.ret, &type_scope, null) catch return error.EvalFailed;
    } else {
        var type_scope = types.TypeScope.init(gpa);
        defer type_scope.deinit();
        ret_layout = layout_store_ptr.fromTypeVar(module_idx, expr_type_var, &type_scope, null) catch return error.EvalFailed;
    }

    const arg_layouts: []const layout.Idx = arg_layouts_buf[0..arg_layouts_len];

    // Generate native code using the RocCall ABI entrypoint wrapper
    var code_result = dev_eval.generateEntrypointCode(
        platform_env,
        entrypoint_expr,
        all_module_envs,
        app_module_env,
        arg_layouts,
        ret_layout,
    ) catch {
        return error.EvalFailed;
    };
    defer code_result.deinit();

    if (code_result.code.len == 0) {
        return error.EvalFailed;
    }

    // Make the generated code executable and run it
    var executable = ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset) catch {
        return error.EvalFailed;
    };
    defer executable.deinit();

    // Use the DevEvaluator's RocOps (with setjmp/longjmp crash protection)
    // so roc_crashed returns an error rather than calling std.process.exit(1).
    // Splice in the caller's hosted functions so the generated code can call them.
    dev_eval.roc_ops.hosted_fns = roc_ops.hosted_fns;

    dev_eval.callRocABIWithCrashProtection(&executable, result_ptr, args_ptr) catch |err| switch (err) {
        error.RocCrashed => return error.EvalFailed,
        error.Segfault => return error.EvalFailed,
    };
}

/// Run via the LIR interpreter.
fn runViaInterpreter(
    gpa: Allocator,
    platform_env: *ModuleEnv,
    _: *const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
    target: roc_target.RocTarget,
) RunError!void {
    const const_module_envs: []const *ModuleEnv = @ptrCast(all_module_envs);

    // Create LIR lowering pipeline
    _ = target;
    const target_usize: base.target.TargetUsize = if (builtin.cpu.arch == .wasm32) .u32 else .u64;
    var lir_program = eval_mod.LirProgram.init(gpa, target_usize);
    defer lir_program.deinit();

    // Resolve arg/ret layouts from the CIR expression's type
    const layout_store_ptr = lir_program.prepareLayoutStores(const_module_envs) catch return error.CompilationFailed;
    const module_idx: u32 = for (all_module_envs, 0..) |env, i| {
        if (env == platform_env) break @intCast(i);
    } else return error.EvalFailed;

    const expr_type_var = ModuleEnv.varFrom(entrypoint_expr);
    const resolved_type = platform_env.types.resolveVar(expr_type_var);
    const maybe_func = resolved_type.desc.content.unwrapFunc();

    var arg_layouts_buf: [16]layout.Idx = undefined;
    var arg_layouts_len: usize = 0;
    var ret_layout: layout.Idx = undefined;

    if (maybe_func) |func| {
        const arg_vars = platform_env.types.sliceVars(func.args);
        var type_scope = types.TypeScope.init(gpa);
        defer type_scope.deinit();
        for (arg_vars, 0..) |arg_var, i| {
            arg_layouts_buf[i] = layout_store_ptr.fromTypeVar(module_idx, arg_var, &type_scope, null) catch return error.CompilationFailed;
        }
        arg_layouts_len = arg_vars.len;
        ret_layout = layout_store_ptr.fromTypeVar(module_idx, func.ret, &type_scope, null) catch return error.CompilationFailed;
    } else {
        var type_scope = types.TypeScope.init(gpa);
        defer type_scope.deinit();
        ret_layout = layout_store_ptr.fromTypeVar(module_idx, expr_type_var, &type_scope, null) catch return error.CompilationFailed;
    }

    const arg_layouts: []const layout.Idx = arg_layouts_buf[0..arg_layouts_len];

    // Build TypeScope for platform requires types (maps flex vars to app types)
    var platform_type_scope: ?types.TypeScope = if (app_module_env) |ae|
        eval_mod.cir_to_lir.buildPlatformTypeScope(gpa, platform_env, ae) catch return error.CompilationFailed
    else
        null;
    defer if (platform_type_scope) |*ts| ts.deinit();

    // Lower CIR to LIR.
    // - Zero-arg functions: wrap in call at MIR level so the LIR executes the body.
    // - Functions with args: lower as lambda; evalEntrypoint calls it with args.
    // - Non-functions: lower directly.
    const is_zero_arg_func = maybe_func != null and arg_layouts_len == 0;
    var lower_result = lir_program.lowerEntrypointExpr(
        platform_env,
        entrypoint_expr,
        const_module_envs,
        app_module_env,
        is_zero_arg_func,
        if (platform_type_scope) |*ts| ts else null,
    ) catch return error.CompilationFailed;
    defer lower_result.deinit();

    // Create LIR interpreter and evaluate
    var interp = eval_mod.LirInterpreter.init(gpa, &lower_result.lir_store, lower_result.layout_store, null);
    defer interp.deinit();

    interp.evalEntrypoint(
        lower_result.final_expr_id,
        arg_layouts,
        ret_layout,
        roc_ops,
        args_ptr,
        result_ptr,
    ) catch |err| {
        if (comptime builtin.os.tag != .freestanding) {
            std.debug.print("LIR interpreter error: {}\n", .{err});
        }
        return error.EvalFailed;
    };
}
