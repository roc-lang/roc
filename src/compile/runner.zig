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

pub const LoweringModuleEnvs = struct {
    allocator: std.mem.Allocator,
    all_module_envs: []*ModuleEnv,
    all_module_envs_const: []const *const ModuleEnv,
    builtin_module_idx: u32,
    primary_module_idx: u32,
    app_module_idx: ?u32,
    builtin_str: ?base.Ident.Idx,

    pub fn deinit(self: *LoweringModuleEnvs) void {
        self.allocator.free(self.all_module_envs_const);
        self.allocator.free(self.all_module_envs);
    }
};

pub fn buildLoweringModuleEnvs(
    allocator: std.mem.Allocator,
    builtin_env: *ModuleEnv,
    primary_env: *ModuleEnv,
    app_env: ?*ModuleEnv,
    imported_envs: ?[]const *const ModuleEnv,
) !LoweringModuleEnvs {
    var module_envs = std.ArrayList(*ModuleEnv).empty;
    defer module_envs.deinit(allocator);

    try module_envs.append(allocator, builtin_env);
    if (imported_envs) |envs| {
        for (envs) |env| {
            const mutable_env = @constCast(env);
            var seen = false;
            for (module_envs.items) |existing| {
                if (existing == mutable_env) {
                    seen = true;
                    break;
                }
            }
            if (!seen) try module_envs.append(allocator, mutable_env);
        }
    }

    if (!containsModuleEnv(module_envs.items, primary_env)) {
        try module_envs.append(allocator, primary_env);
    }
    if (app_env) |env| {
        if (!containsModuleEnv(module_envs.items, env)) {
            try module_envs.append(allocator, env);
        }
    }

    const all_module_envs = try module_envs.toOwnedSlice(allocator);
    errdefer allocator.free(all_module_envs);

    const all_module_envs_const = try allocator.alloc(*const ModuleEnv, all_module_envs.len);
    errdefer allocator.free(all_module_envs_const);
    for (all_module_envs, 0..) |module_env, i| {
        all_module_envs_const[i] = module_env;
    }

    for (all_module_envs) |module_env| {
        module_env.imports.resolveImports(module_env, all_module_envs_const);
    }

    const builtin_module_idx: u32 = 0;
    const primary_module_idx: u32 = findModuleEnvIdx(all_module_envs, primary_env) orelse return error.EntrypointModuleNotFound;
    const resolved_app_module_idx = if (app_env) |env|
        findModuleEnvIdx(all_module_envs, env)
    else
        null;

    return .{
        .allocator = allocator,
        .all_module_envs = all_module_envs,
        .all_module_envs_const = all_module_envs_const,
        .builtin_module_idx = builtin_module_idx,
        .primary_module_idx = primary_module_idx,
        .app_module_idx = resolved_app_module_idx,
        .builtin_str = builtin_env.idents.builtin_str,
    };
}

fn containsModuleEnv(module_envs: []const *ModuleEnv, target: *const ModuleEnv) bool {
    return findModuleEnvIdx(module_envs, target) != null;
}

pub fn findModuleEnvIdx(module_envs: []const *ModuleEnv, target: *const ModuleEnv) ?u32 {
    for (module_envs, 0..) |module_env, i| {
        if (module_env == target) return @intCast(i);
    }
    return null;
}

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
    const builtin_idx = findModuleEnvIdx(all_module_envs, builtin_env) orelse return error.BuiltinModuleNotFound;
    const entry_idx = findModuleEnvIdx(all_module_envs, entrypoint_env) orelse return error.EntrypointModuleNotFound;
    const app_module_idx = if (app_module_env) |app_env|
        findModuleEnvIdx(all_module_envs, app_env)
    else
        null;

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
        builtin_env.idents.builtin_str,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lir_result.deinit();
    try lir.Ownership.inferProcResultContracts(gpa, &lir_result.store, &lir_result.layouts);
    try lir.RcInsert.run(gpa, &lir_result.store, &lir_result.layouts);
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
