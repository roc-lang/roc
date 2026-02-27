//! High-level helpers for running compiled Roc apps through the interpreter.
//! This avoids each call site needing to know about ImportMapping, interpreter init, etc.

const std = @import("std");
const can = @import("can");
const eval = @import("eval");
const roc_target = @import("roc_target");

const builtins = @import("builtins");
const ModuleEnv = can.ModuleEnv;
const Interpreter = eval.Interpreter;
const BuiltinModules = eval.BuiltinModules;
const RocOps = builtins.host_abi.RocOps;
const import_mapping_mod = @import("types").import_mapping;

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
    args_ptr: *anyopaque,
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
        std.debug.print("Interpreter error: {}\n", .{err});
        return error.InterpreterFailed;
    };
}
