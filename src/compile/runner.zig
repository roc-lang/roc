//! High-level helpers for running compiled Roc apps.
//! Provides a unified `run` entry point that dispatches to the correct evaluator
//! (interpreter, native JIT via dev backend, or WASM bytecode generation).

const std = @import("std");
const can = @import("can");
const eval = @import("eval");
const roc_target = @import("roc_target");

const builtins = @import("builtins");
const layout = @import("layout");
const ModuleEnv = can.ModuleEnv;
const Interpreter = eval.Interpreter;
const BuiltinModules = eval.BuiltinModules;
const DevEvaluator = eval.DevEvaluator;
const ExecutableMemory = eval.ExecutableMemory;
const WasmEvaluator = eval.WasmEvaluator;
const RocOps = builtins.host_abi.RocOps;
const import_mapping_mod = @import("types").import_mapping;

/// Backend selection enum (re-exported so callers only need `compile`).
pub const EvalBackend = eval.EvalBackend;

/// Outcome of `run`.
/// - `.executed` — interpreter or native JIT ran; result written to `result_ptr`.
/// - `.wasm`     — WASM bytes produced; caller must call `.deinit()` when done.
pub const RunResult = union(enum) {
    executed: void,
    wasm: eval.WasmCodeResult,
};

/// Unified entry point for all evaluation backends.
///
/// Dispatches to the interpreter, native JIT (dev/llvm), or WASM code generator
/// depending on `backend`.  `builtin_modules` and `target` are only consumed by
/// the interpreter path; the other paths accept them but ignore them so callers
/// never need conditional logic.
pub fn run(
    backend: EvalBackend,
    gpa: std.mem.Allocator,
    platform_env: *ModuleEnv,
    builtin_modules: *const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: can.CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
    target: roc_target.RocTarget,
) !RunResult {
    switch (backend) {
        .interpreter => {
            try runViaInterpreter(
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
            );
            return .executed;
        },
        .dev, .llvm => {
            if (comptime ExecutableMemory == void) return error.UnsupportedOnFreestanding;
            try runViaDev(
                gpa,
                platform_env,
                all_module_envs,
                app_module_env,
                entrypoint_expr,
                roc_ops,
                args_ptr,
                result_ptr,
            );
            return .executed;
        },
        .wasm => {
            var wasm_eval = WasmEvaluator.init(gpa) catch return error.WasmEvaluatorFailed;
            const all_module_envs_const: []const *ModuleEnv = all_module_envs;
            const wasm_result = wasm_eval.generateWasm(platform_env, entrypoint_expr, all_module_envs_const, app_module_env) catch return error.WasmEvaluatorFailed;
            return .{ .wasm = wasm_result };
        },
    }
}

/// Run a compiled Roc entrypoint expression through the interpreter.
///
/// This encapsulates interpreter initialization, for-clause type mapping setup,
/// and expression evaluation. The caller provides the RocOps (with hosted functions)
/// and argument/result buffers.
pub fn runViaInterpreter(
    gpa: std.mem.Allocator,
    platform_env: *ModuleEnv,
    builtin_modules: *const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: can.CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
    target: roc_target.RocTarget,
) !void {
    const builtin_types = builtin_modules.asBuiltinTypes();
    const builtin_module_env_ptr = builtin_modules.builtin_module.env;

    var empty_import_mapping = import_mapping_mod.ImportMapping.init(gpa);
    defer empty_import_mapping.deinit();

    const const_module_envs: []const *const ModuleEnv = @ptrCast(all_module_envs);

    var interpreter = Interpreter.init(
        gpa,
        platform_env,
        builtin_types,
        builtin_module_env_ptr,
        const_module_envs,
        &empty_import_mapping,
        app_module_env,
        null,
        target,
    ) catch return error.CompilationFailed;
    defer interpreter.deinitAndFreeOtherEnvs();

    interpreter.setupForClauseTypeMappings(platform_env) catch {};

    interpreter.evaluateExpression(
        entrypoint_expr,
        result_ptr,
        roc_ops,
        args_ptr,
    ) catch |err| {
        if (comptime !@import("threading.zig").is_freestanding) {
            std.debug.print("Interpreter error: {}\n", .{err});
        }
        return error.InterpreterFailed;
    };
}

/// Run a compiled Roc entrypoint through the dev backend (native code generation).
/// Resolves entrypoint layouts, JIT-compiles CIR to native code via DevEvaluator,
/// and executes via the RocCall ABI.
fn runViaDev(
    gpa: std.mem.Allocator,
    platform_env: *ModuleEnv,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: can.CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: ?*anyopaque,
    result_ptr: *anyopaque,
) !void {
    const types = @import("types");

    var dev_eval = DevEvaluator.init(gpa, null) catch {
        return error.DevEvaluatorFailed;
    };
    defer dev_eval.deinit();

    // Resolve entrypoint layouts from the CIR expression's type
    const layout_store_ptr = try dev_eval.ensureGlobalLayoutStore(all_module_envs);
    const module_idx: u32 = for (all_module_envs, 0..) |env, i| {
        if (env == platform_env) break @intCast(i);
    } else return error.DevEvaluatorFailed;

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
            arg_layouts_buf[i] = layout_store_ptr.fromTypeVar(module_idx, arg_var, &type_scope, null) catch return error.DevEvaluatorFailed;
        }
        arg_layouts_len = arg_vars.len;
        ret_layout = layout_store_ptr.fromTypeVar(module_idx, func.ret, &type_scope, null) catch return error.DevEvaluatorFailed;
    } else {
        var type_scope = types.TypeScope.init(gpa);
        defer type_scope.deinit();
        ret_layout = layout_store_ptr.fromTypeVar(module_idx, expr_type_var, &type_scope, null) catch return error.DevEvaluatorFailed;
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
        return error.DevEvaluatorFailed;
    };
    defer code_result.deinit();

    if (code_result.code.len == 0) {
        return error.DevEvaluatorFailed;
    }

    // Make the generated code executable and run it
    var executable = ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset) catch {
        return error.DevEvaluatorFailed;
    };
    defer executable.deinit();

    // Use the DevEvaluator's RocOps (with setjmp/longjmp crash protection)
    // so roc_crashed returns an error rather than calling std.process.exit(1).
    dev_eval.roc_ops.env = roc_ops.env;
    dev_eval.roc_ops.hosted_fns = roc_ops.hosted_fns;

    dev_eval.callRocABIWithCrashProtection(&executable, result_ptr, args_ptr) catch |err| switch (err) {
        error.RocCrashed => return error.DevEvaluatorFailed,
        error.Segfault => return error.DevEvaluatorFailed,
    };
}
