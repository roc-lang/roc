//! High-level helpers for running compiled Roc apps.
//! Thin wrapper around eval.runner for backwards compatibility.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const eval = @import("eval");
const ir = @import("ir");
const layout = @import("layout");
const lir = @import("lir");
const lambdamono = @import("lambdamono");
const lambdasolved = @import("lambdasolved");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const builtins = @import("builtins");

const ModuleEnv = can.ModuleEnv;
const BuiltinModules = eval.BuiltinModules;
const RocOps = builtins.host_abi.RocOps;
const Symbol = @import("symbol").Symbol;

/// Run a compiled Roc entrypoint expression through the interpreter.
pub fn runViaInterpreter(
    gpa: std.mem.Allocator,
    entrypoint_env: *ModuleEnv,
    builtin_modules: *const BuiltinModules,
    all_module_envs: []*ModuleEnv,
    app_module_env: ?*ModuleEnv,
    entrypoint_expr: can.CIR.Expr.Idx,
    roc_ops: *RocOps,
    args_ptr: *anyopaque,
    result_ptr: *anyopaque,
) !void {
    if (comptime builtin.target.os.tag == .freestanding) {
        return error.InterpreterFailed;
    }

    const source_modules = try gpa.alloc(check.TypedCIR.Modules.SourceModule, all_module_envs.len);
    defer gpa.free(source_modules);
    for (all_module_envs, 0..) |module_env, i| {
        source_modules[i] = .{ .precompiled = module_env };
    }

    var typed_cir_modules = try check.TypedCIR.Modules.publish(gpa, source_modules);
    defer typed_cir_modules.deinit();

    const builtin_env = builtin_modules.builtin_module.env;
    var builtin_module_idx: ?u32 = null;
    var entrypoint_module_idx: ?u32 = null;
    for (all_module_envs, 0..) |module_env, i| {
        if (module_env == builtin_env) builtin_module_idx = @intCast(i);
        if (module_env == entrypoint_env) entrypoint_module_idx = @intCast(i);
    }

    const builtin_idx = builtin_module_idx orelse return error.BuiltinModuleNotFound;
    const entry_idx = entrypoint_module_idx orelse return error.EntrypointModuleNotFound;

    var app_module_idx: ?u32 = null;
    if (app_module_env) |app_env| {
        for (all_module_envs, 0..) |module_env, i| {
            if (module_env == app_env) {
                app_module_idx = @intCast(i);
                break;
            }
        }
    }

    var mono_lowerer = try monotype.Lower.Lowerer.init(gpa, &typed_cir_modules, builtin_idx, app_module_idx);
    defer mono_lowerer.deinit();
    const defs = entrypoint_env.store.sliceDefs(entrypoint_env.all_defs);
    var entry_def: ?can.CIR.Def.Idx = null;
    for (defs) |def_idx| {
        const def = entrypoint_env.store.getDef(def_idx);
        if (def.expr == entrypoint_expr) {
            entry_def = def_idx;
            break;
        }
    }
    const entry_def_idx = entry_def orelse return error.EntrypointDefNotFound;

    const entry_symbol = try mono_lowerer.specializeTopLevelDef(entry_idx, entry_def_idx);
    const mono = try mono_lowerer.run(entry_idx);

    const lifted = try monotype_lifted.Lower.run(gpa, mono);
    const solved = try lambdasolved.Lower.run(gpa, lifted);
    const executable = try lambdamono.Lower.runWithEntrypoints(gpa, solved, &.{entry_symbol});
    const lowered_ir = try ir.Lower.run(gpa, executable);

    var lir_result = try lir.FromIr.run(
        gpa,
        all_module_envs,
        null,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lir_result.deinit();
    const proc_id = lir_result.proc_ids_by_symbol.get(entry_symbol.raw()) orelse return error.EntrypointProcNotFound;
    const proc = lir_result.store.getProcSpec(proc_id);

    const arg_locals = lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try gpa.alloc(layout.Idx, arg_locals.len);
    defer gpa.free(arg_layouts);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = lir_result.store.getLocal(local_id).layout_idx;
    }

    var interp = try eval.Interpreter.init(gpa, &lir_result.store, &lir_result.layouts, roc_ops);
    defer interp.deinit();

    const eval_result = interp.eval(.{
        .proc_id = proc_id,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = args_ptr,
        .ret_ptr = result_ptr,
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.RuntimeError, error.DivisionByZero, error.Crash => return error.InterpreterFailed,
    };

    interp.dropValue(eval_result.value, proc.ret_layout);
}
