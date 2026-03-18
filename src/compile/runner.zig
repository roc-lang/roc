//! High-level helpers for running compiled Roc apps.
//! Thin wrapper around eval.runner for backwards compatibility.

const std = @import("std");
const can = @import("can");
const eval = @import("eval");
const roc_target = @import("roc_target");
const builtins = @import("builtins");

const ModuleEnv = can.ModuleEnv;
const BuiltinModules = eval.BuiltinModules;
const RocOps = builtins.host_abi.RocOps;

/// Run a compiled Roc entrypoint expression through the interpreter.
pub fn runViaInterpreter(
    gpa: std.mem.Allocator,
    platform_env: *ModuleEnv,
    builtin_modules: *const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: can.CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: *anyopaque,
    result_ptr: *anyopaque,
    target: roc_target.RocTarget,
) !void {
    eval.runner.run(
        .interpreter,
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
    ) catch |err| switch (err) {
        error.EvalFailed => return error.InterpreterFailed,
        error.CompilationFailed => return error.CompilationFailed,
        error.OutOfMemory => return error.OutOfMemory,
    };
}
