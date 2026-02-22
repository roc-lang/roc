//! WebAssembly Backend Evaluator
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to Mono IR (globally unique symbols)
//! 5. Generating WebAssembly bytecode
//!
//! The wasm bytes are NOT executed here — execution via bytebox happens
//! in the test infrastructure (test/helpers.zig) to keep the bytebox
//! dependency out of the compiler proper.

const std = @import("std");
const can = @import("can");
const layout = @import("layout");
const types = @import("types");
const mono = @import("mono");
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;
const WasmCodeGen = backend.wasm.WasmCodeGen;

/// Extract the result layout from a Mono IR expression.
/// Mirrors the logic in dev_evaluator.zig.
fn monoExprResultLayout(store: *const MonoExprStore, expr_id: mono.MonoIR.MonoExprId) ?layout.Idx {
    const MonoExpr = mono.MonoIR.MonoExpr;
    const expr: MonoExpr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |b| monoExprResultLayout(store, b.final_expr),
        .if_then_else => |ite| ite.result_layout,
        .match_expr => |w| w.result_layout,
        .dbg => |d| d.result_layout,
        .expect => |e| e.result_layout,
        .binop => |b| b.result_layout,
        .unary_minus => |um| um.result_layout,
        .call => |c| c.ret_layout,
        .low_level => |ll| ll.ret_layout,
        .early_return => |er| er.ret_layout,
        .lookup => |l| l.layout_idx,
        .record => |r| r.record_layout,
        .tuple => |t| t.tuple_layout,
        .tag => |t| t.union_layout,
        .zero_arg_tag => |z| z.union_layout,
        .field_access => |fa| fa.field_layout,
        .tuple_access => |ta| ta.elem_layout,
        .closure => |c| c.closure_layout,
        .nominal => |n| monoExprResultLayout(store, n.backing_expr) orelse n.nominal_layout,
        .i64_literal => .i64,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .bool,
        .i128_literal => .i128,
        .dec_literal => .dec,
        .str_literal => .str,
        .unary_not => .bool,
        // Expressions whose result layout is handled by the fromTypeVar fallback
        .for_loop,
        .while_loop,
        .list,
        .empty_list,
        .empty_record,
        .lambda,
        .crash,
        .runtime_error,
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        .discriminant_switch,
        .tag_payload_access,
        .hosted_call,
        .incref,
        .decref,
        .free,
        => null,
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
        if (self.global_layout_store) |ls| {
            ls.deinit();
            self.allocator.destroy(ls);
        }
        self.builtin_module.deinit();
    }

    fn ensureGlobalLayoutStore(self: *WasmEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        if (self.global_layout_store) |ls| return ls;

        const builtin_str = if (all_module_envs.len > 0)
            all_module_envs[0].idents.builtin_str
        else
            null;

        const base = @import("base");
        const ls = self.allocator.create(layout.Store) catch return error.OutOfMemory;
        ls.* = layout.Store.init(all_module_envs, builtin_str, self.allocator, base.target.TargetUsize.u32) catch {
            self.allocator.destroy(ls);
            return error.OutOfMemory;
        };

        self.global_layout_store = ls;
        return ls;
    }

    /// Generate wasm bytes for a CIR expression.
    pub fn generateWasm(
        self: *WasmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
    ) Error!WasmCodeResult {
        // Create Mono IR store
        var mono_store = MonoExprStore.init(self.allocator);
        defer mono_store.deinit();

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

        // Lower CIR -> Mono IR
        var lowerer = MonoLower.init(self.allocator, &mono_store, all_module_envs, null, layout_store_ptr, null, null);
        defer lowerer.deinit();

        const lowered_expr_id = lowerer.lowerExpr(module_idx, expr_idx) catch {
            return error.RuntimeError;
        };

        // Run RC insertion pass
        var rc_pass = mono.RcInsert.RcInsertPass.init(self.allocator, &mono_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        const mono_expr_id = rc_pass.insertRcOps(lowered_expr_id) catch lowered_expr_id;

        // Determine result layout
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = monoExprResultLayout(&mono_store, mono_expr_id) orelse blk: {
            const type_var = can.ModuleEnv.varFrom(expr_idx);
            var type_scope = types.TypeScope.init(self.allocator);
            defer type_scope.deinit();
            break :blk layout_store_ptr.fromTypeVar(module_idx, type_var, &type_scope, null) catch {
                return error.RuntimeError;
            };
        };

        // Detect tuple length
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        // Generate wasm module
        var codegen = WasmCodeGen.init(self.allocator, &mono_store, layout_store_ptr);
        codegen.wasm_stack_bytes = self.wasm_stack_bytes;
        defer codegen.deinit();

        const gen_result = codegen.generateModule(mono_expr_id, result_layout) catch {
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
