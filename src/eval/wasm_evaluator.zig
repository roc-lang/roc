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
const LirExprStore = lir.LirExprStore;
const LirExprId = lir.LirExprId;
const LirExpr = lir.LirExpr;
const WasmCodeGen = backend.wasm.WasmCodeGen;

/// Extract the result layout from a LIR expression.
/// Mirrors the logic in dev_evaluator.zig.
fn lirExprResultLayout(store: *const LirExprStore, expr_id: LirExprId) layout.Idx {
    const expr: LirExpr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |b| b.result_layout,
        .if_then_else => |ite| ite.result_layout,
        .match_expr => |w| w.result_layout,
        .dbg => |d| d.result_layout,
        .expect => |e| e.result_layout,
        .proc_call => |c| c.ret_layout,
        .low_level => |ll| ll.ret_layout,
        .early_return => |er| er.ret_layout,
        .lookup => |l| l.layout_idx,
        .cell_load => |l| l.layout_idx,
        .struct_ => |s| s.struct_layout,
        .tag => |t| t.union_layout,
        .zero_arg_tag => |z| z.union_layout,
        .struct_access => |sa| sa.field_layout,
        .nominal => |n| n.nominal_layout,
        .discriminant_switch => |ds| ds.result_layout,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .bool,
        .dec_literal => .dec,
        .str_literal => .str,
        .i64_literal => |i| i.layout_idx,
        .i128_literal => |i| i.layout_idx,
        .list => |l| l.list_layout,
        .empty_list => |l| l.list_layout,
        .hosted_call => |hc| hc.ret_layout,
        .str_concat, .int_to_str, .float_to_str, .dec_to_str, .str_escape_and_quote => .str,
        .tag_payload_access => |tpa| tpa.payload_layout,
        .for_loop, .while_loop, .incref, .decref, .free => .zst,
        .crash => |c| c.ret_layout,
        .runtime_error => |re| re.ret_layout,
        .break_expr => {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "LIR/eval invariant violated: lirExprResultLayout called on break_expr",
                    .{},
                );
            }
            unreachable;
        },
    };
}

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

    /// The default entrypoint name used by the eval/REPL pipeline.
    /// Real builds derive this from the platform's `provides` section.
    pub const default_entrypoint_name = "roc__main_for_host_1_exposed";

    /// Generate wasm bytes for a CIR expression.
    /// `entrypoint_name` is the RocCall export name, derived from the platform's
    /// `provides` section. Use `default_entrypoint_name` for eval/REPL.
    pub fn generateWasm(
        self: *WasmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        entrypoint_name: []const u8,
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

        // Run lambda set inference
        const mir_mod = @import("mir");
        var lambda_set_store = mir_mod.LambdaSet.infer(self.allocator, &mir_store, all_module_envs) catch return error.OutOfMemory;
        defer lambda_set_store.deinit(self.allocator);

        // Lower MIR -> LIR
        var lir_store = LirExprStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &lambda_set_store, module_env.idents.true_tag);
        defer mir_to_lir.deinit();

        const lir_expr_id = mir_to_lir.lower(mir_expr_id) catch {
            return error.RuntimeError;
        };
        // Run RC insertion pass on the LIR
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        const final_expr_id = rc_pass.insertRcOps(lir_expr_id) catch lir_expr_id;

        // Run RC insertion pass on all function definitions (symbol_defs)
        lir.RcInsert.insertRcOpsIntoSymbolDefsBestEffort(self.allocator, &lir_store, layout_store_ptr);

        // Determine result layout
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = lirExprResultLayout(&lir_store, final_expr_id);

        // Detect tuple length
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        // Generate wasm module
        // TODO(Phase 12): Merge roc_builtins.o and populate real BuiltinSymbols.
        // For now, use undefined — eval tests need builtins merged before this works.
        const builtin_syms: backend.wasm.WasmModule.BuiltinSymbols = undefined;
        var codegen = WasmCodeGen.init(self.allocator, &lir_store, layout_store_ptr, builtin_syms);
        codegen.wasm_stack_bytes = self.wasm_stack_bytes;
        defer codegen.deinit();

        const gen_result = codegen.generateModule(final_expr_id, result_layout, entrypoint_name) catch {
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
