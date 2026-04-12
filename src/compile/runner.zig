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
    platform_env: *ModuleEnv,
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
    _ = app_module_env;

    const source_modules = try gpa.alloc(check.TypedCIR.Modules.SourceModule, all_module_envs.len);
    defer gpa.free(source_modules);
    for (all_module_envs, 0..) |module_env, i| {
        source_modules[i] = .{ .precompiled = module_env };
    }

    var typed_cir_modules = try check.TypedCIR.Modules.publish(gpa, source_modules);
    defer typed_cir_modules.deinit();

    const builtin_env = builtin_modules.builtin_module.env;
    var builtin_module_idx: ?u32 = null;
    var platform_module_idx: ?u32 = null;
    for (all_module_envs, 0..) |module_env, i| {
        if (module_env == builtin_env) builtin_module_idx = @intCast(i);
        if (module_env == platform_env) platform_module_idx = @intCast(i);
    }

    const builtin_idx = builtin_module_idx orelse return error.CompilationFailed;
    const platform_idx = platform_module_idx orelse return error.CompilationFailed;

    var mono_lowerer = try monotype.Lower.Lowerer.init(gpa, &typed_cir_modules, builtin_idx);
    defer mono_lowerer.deinit();
    const mono = try mono_lowerer.run(platform_idx);

    const lifted = try monotype_lifted.Lower.run(gpa, mono);
    defer @constCast(&lifted).deinit();
    const solved = try lambdasolved.Lower.run(gpa, lifted);
    defer @constCast(&solved).deinit();
    const executable = try lambdamono.Lower.run(gpa, solved);
    defer @constCast(&executable).deinit();
    const lowered_ir = try ir.Lower.run(gpa, executable);
    defer @constCast(&lowered_ir).deinit();

    var lir_result = try lir.FromIr.run(
        gpa,
        all_module_envs,
        null,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lir_result.deinit();

    const defs = platform_env.store.sliceDefs(platform_env.all_defs);
    var entry_def: ?can.CIR.Def.Idx = null;
    for (defs) |def_idx| {
        const def = platform_env.store.getDef(def_idx);
        if (def.expr == entrypoint_expr) {
            entry_def = def_idx;
            break;
        }
    }
    const entry_def_idx = entry_def orelse return error.CompilationFailed;

    var entry_symbol: ?Symbol = null;
    for (lowered_ir.symbols.entries.items, 0..) |entry, i| {
        switch (entry.origin) {
            .top_level_def => |origin| {
                if (origin.module_idx == platform_idx and origin.def_idx == @intFromEnum(entry_def_idx)) {
                    entry_symbol = Symbol.fromRaw(@intCast(i));
                    break;
                }
            },
            else => {},
        }
    }
    const symbol = entry_symbol orelse return error.CompilationFailed;
    const proc_id = lir_result.proc_ids_by_symbol.get(symbol.raw()) orelse return error.CompilationFailed;
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
