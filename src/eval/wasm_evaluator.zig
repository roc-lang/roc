//! WebAssembly Backend Evaluator
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to MIR (globally unique symbols)
//! 5. Lowering MIR to LIR
//! 6. Running RC insertion
//! 7. Generating WebAssembly bytecode
//!
//! The wasm bytes are NOT executed here — execution via bytebox happens
//! in the test infrastructure (test/helpers.zig) to keep the bytebox
//! dependency out of the compiler proper.

const std = @import("std");
const can = @import("can");
const layout = @import("layout");
const mir = @import("mir");
const lir = @import("lir");
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

const MIR = mir.MIR;
const LirStore = lir.LirStore;
const WasmCodeGen = backend.wasm.WasmCodeGen;

/// Result of wasm code generation
pub const WasmCodeResult = struct {
    wasm_bytes: []const u8,
    result_layout: layout.Idx,
    tuple_len: usize,
    has_imports: bool = false,
    allocator: Allocator,

    pub fn deinit(self: *WasmCodeResult) void {
        if (self.wasm_bytes.len > 0) {
            self.allocator.free(self.wasm_bytes);
        }
    }
};

/// WebAssembly evaluator — produces wasm bytes from CIR expressions.
pub const WasmEvaluator = struct {
    allocator: Allocator,
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    global_layout_store: ?*layout.Store = null,
    global_type_layout_resolver: ?*layout.TypeLayoutResolver = null,
    /// Configurable wasm stack size in bytes (default 1MB).
    wasm_stack_bytes: u32 = 1024 * 1024,

    pub const Error = error{
        OutOfMemory,
        RuntimeError,
    };

    pub fn init(allocator: Allocator) Error!WasmEvaluator {
        const compiled_builtins = @import("compiled_builtins");

        const builtin_indices = builtin_loading.deserializeBuiltinIndices(
            allocator,
            compiled_builtins.builtin_indices_bin,
        ) catch return error.OutOfMemory;

        const builtin_module = builtin_loading.loadCompiledModule(
            allocator,
            compiled_builtins.builtin_bin,
            "Builtin",
            compiled_builtins.builtin_source,
        ) catch return error.OutOfMemory;

        return WasmEvaluator{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
        };
    }

    pub fn deinit(self: *WasmEvaluator) void {
        if (self.global_type_layout_resolver) |resolver| {
            resolver.deinit();
            self.allocator.destroy(resolver);
        }
        if (self.global_layout_store) |ls| {
            ls.deinit();
            self.allocator.destroy(ls);
        }
        self.builtin_module.deinit();
    }

    fn ensureGlobalLayoutStore(self: *WasmEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        if (self.global_layout_store) |ls| return ls;

        var builtin_str: ?@import("base").Ident.Idx = null;
        for (all_module_envs) |env| {
            if (isBuiltinModuleEnv(env)) {
                builtin_str = env.idents.builtin_str;
                break;
            }
        }

        const base = @import("base");
        const ls = self.allocator.create(layout.Store) catch return error.OutOfMemory;
        ls.* = layout.Store.init(all_module_envs, builtin_str, self.allocator, base.target.TargetUsize.u32) catch {
            self.allocator.destroy(ls);
            return error.OutOfMemory;
        };

        self.global_layout_store = ls;
        return ls;
    }

    fn ensureGlobalTypeLayoutResolver(self: *WasmEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.TypeLayoutResolver {
        if (self.global_type_layout_resolver) |resolver| return resolver;

        const layout_store = try self.ensureGlobalLayoutStore(all_module_envs);
        const resolver = self.allocator.create(layout.TypeLayoutResolver) catch return error.OutOfMemory;
        resolver.* = layout.TypeLayoutResolver.init(layout_store);
        self.global_type_layout_resolver = resolver;
        return resolver;
    }

    /// Generate wasm bytes for a CIR expression.
    pub fn generateWasm(
        self: *WasmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
    ) Error!WasmCodeResult {
        // Other evaluators may have resolved this module's imports against a
        // different module ordering. Refresh them here so CIR external lookups
        // line up with the slice we are about to hand to MIR lowering.
        module_env.imports.resolveImports(module_env, all_module_envs);

        // Find module index
        var module_idx: u32 = 0;
        for (all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                module_idx = @intCast(i);
                break;
            }
        }

        // Get layout store (wasm32 target)
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);
        layout_store_ptr.setModuleEnvs(all_module_envs);
        const type_layout_resolver_ptr = try self.ensureGlobalTypeLayoutResolver(all_module_envs);

        // In REPL sessions, module type stores get fresh type variables on each evaluation,
        // but the shared type-layout resolver persists. Clear stale type-side caches.
        type_layout_resolver_ptr.resetModuleCache(all_module_envs);

        // Lower CIR -> MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        var monomorphization = mir.Monomorphize.runExpr(
            self.allocator,
            all_module_envs,
            &module_env.types,
            module_idx,
            null,
            expr_idx,
        ) catch return error.OutOfMemory;
        defer monomorphization.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            &monomorphization,
            all_module_envs,
            &module_env.types,
            module_idx,
            null, // app_module_idx - not used for Wasm evaluation
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        const mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.RuntimeError;
        };

        const mir_mod = @import("mir");
        var mir_analyses = try mir_mod.Analyses.init(self.allocator, &mir_store, &.{mir_expr_id});
        defer mir_analyses.deinit();

        // Lower MIR -> LIR
        var lir_store = LirStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &mir_analyses);
        defer mir_to_lir.deinit();

        const root_proc_id = mir_to_lir.lower(mir_expr_id) catch {
            return error.RuntimeError;
        };
        // Run RC insertion over the full lowered proc graph before codegen.
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        try rc_pass.insertRcOpsForAllProcs();

        // Determine result layout
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = lir_store.getProcSpec(root_proc_id).ret_layout;

        // Detect tuple length
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        // Generate wasm module
        var codegen = WasmCodeGen.init(self.allocator, &lir_store, layout_store_ptr);
        codegen.wasm_stack_bytes = self.wasm_stack_bytes;
        defer codegen.deinit();

        const gen_result = codegen.generateModule(root_proc_id, result_layout) catch {
            return error.RuntimeError;
        };

        return WasmCodeResult{
            .wasm_bytes = gen_result.wasm_bytes,
            .result_layout = gen_result.result_layout,
            .tuple_len = tuple_len,
            .has_imports = gen_result.has_imports,
            .allocator = self.allocator,
        };
    }
};
