//! Dev backend evaluator for testing and CLI expect execution.
//!
//! This compiles a single CIR expression by lowering it through the
//! monotype → LIR pipeline and then running the dev backend codegen.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const backend = @import("backend");
const ir = @import("ir");
const lambdamono = @import("lambdamono");
const lambdasolved = @import("lambdasolved");
const lir = @import("lir");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LayoutIdx = @import("layout").Idx;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");

pub const DevEvaluator = struct {
    allocator: std.mem.Allocator,
    runtime_env: RuntimeHostEnv,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        CodeGenFailed,
        RocCrashed,
        ModuleEnvNotFound,
    };

    pub const CodeResult = struct {
        code: []const u8,
        allocator: std.mem.Allocator,
        result_layout: LayoutIdx,
        entry_offset: usize,

        pub fn deinit(self: *CodeResult) void {
            if (self.code.len > 0) {
                self.allocator.free(self.code);
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator, _: ?@import("io").Io) Error!DevEvaluator {
        return DevEvaluator{
            .allocator = allocator,
            .runtime_env = RuntimeHostEnv.init(allocator),
        };
    }

    pub fn deinit(self: *DevEvaluator) void {
        self.runtime_env.deinit();
    }

    pub fn prepareModulesForCodegen(_: *DevEvaluator, _: []*ModuleEnv) Error!void {}

    pub fn getCrashMessage(self: *const DevEvaluator) ?[]const u8 {
        return switch (self.runtime_env.crashState()) {
            .did_not_crash => null,
            .crashed => |msg| msg,
        };
    }

    pub fn callWithCrashProtection(
        self: *DevEvaluator,
        executable: *const backend.ExecutableMemory,
        result_ptr: *anyopaque,
    ) Error!void {
        self.runtime_env.resetObservation();
        var crash_boundary = self.runtime_env.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.RocCrashed;

        executable.callRocABI(@ptrCast(self.runtime_env.get_ops()), result_ptr, null);
        if (self.runtime_env.terminationState() == .crashed) return error.RocCrashed;
    }

    pub fn callRocABIWithCrashProtection(
        self: *DevEvaluator,
        executable: *const backend.ExecutableMemory,
        result_ptr: *anyopaque,
        args_ptr: ?*anyopaque,
    ) Error!void {
        self.runtime_env.resetObservation();
        var crash_boundary = self.runtime_env.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.RocCrashed;

        executable.callRocABI(@ptrCast(self.runtime_env.get_ops()), result_ptr, args_ptr);
        if (self.runtime_env.terminationState() == .crashed) return error.RocCrashed;
    }

    pub fn generateCode(
        self: *DevEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        _: ?*ModuleEnv,
    ) Error!CodeResult {
        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const builtin_idx = findBuiltinModuleIdx(all_module_envs) orelse return error.ModuleEnvNotFound;

        const source_modules = try self.allocator.alloc(check.TypedCIR.Modules.SourceModule, all_module_envs.len);
        defer self.allocator.free(source_modules);
        for (all_module_envs, 0..) |env, i| {
            source_modules[i] = .{ .precompiled = env };
        }

        var typed_cir_modules = try check.TypedCIR.Modules.publish(self.allocator, source_modules);
        defer typed_cir_modules.deinit();

        var mono_lowerer = try monotype.Lower.Lowerer.init(self.allocator, &typed_cir_modules, builtin_idx);
        defer mono_lowerer.deinit();
        const mono = try mono_lowerer.runRootExpr(@intCast(module_idx), expr_idx);

        const lifted = try monotype_lifted.Lower.run(self.allocator, mono);
        defer @constCast(&lifted).deinit();
        const solved = try lambdasolved.Lower.run(self.allocator, lifted);
        defer @constCast(&solved).deinit();
        const executable = try lambdamono.Lower.run(self.allocator, solved);
        defer @constCast(&executable).deinit();
        const lowered_ir = try ir.Lower.run(self.allocator, executable);
        defer @constCast(&lowered_ir).deinit();

        var lir_result = try lir.FromIr.run(
            self.allocator,
            all_module_envs,
            null,
            base.target.TargetUsize.native,
            lowered_ir,
        );
        defer lir_result.deinit();
        if (lir_result.root_procs.items.len == 0) return error.CompilationFailed;

        const entry_proc = lir_result.root_procs.items[lir_result.root_procs.items.len - 1];
        const entry_spec = lir_result.store.getProcSpec(entry_proc);

        var codegen = try backend.HostLirCodeGen.init(
            self.allocator,
            &lir_result.store,
            &lir_result.layouts,
            null,
        );
        defer codegen.deinit();
        try codegen.compileAllProcSpecs(lir_result.store.getProcSpecs());

        const wrapper = try codegen.generateEntrypointWrapper(
            "roc_eval_dev_expr",
            entry_proc,
            &.{},
            entry_spec.ret_layout,
        );

        const generated = codegen.getGeneratedCode();
        const code_copy = try self.allocator.dupe(u8, generated);

        return CodeResult{
            .code = code_copy,
            .allocator = self.allocator,
            .result_layout = entry_spec.ret_layout,
            .entry_offset = wrapper.offset,
        };
    }
};

fn findBuiltinModuleIdx(all_module_envs: []const *ModuleEnv) ?u32 {
    for (all_module_envs, 0..) |env, i| {
        if (env.idents.builtin_module.eql(env.display_module_name_idx)) return @intCast(i);
    }
    return null;
}

fn findModuleEnvIdx(all_module_envs: []const *ModuleEnv, target: *const ModuleEnv) ?u32 {
    for (all_module_envs, 0..) |env, i| {
        if (env == target) return @intCast(i);
    }
    return null;
}
