//! LLVM-based Evaluator for Roc expressions
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to MIR (monomorphized intermediate representation)
//! 5. Lowering MIR to LIR (low-level IR with globally unique symbols)
//! 6. Reference counting insertion
//! 7. Generating LLVM bitcode via MonoLlvmCodeGen
//! 8. Compiling bitcode to a native object file via LLVM
//! 9. Extracting the .text section from the object file
//! 10. Executing the generated code in mmap'd memory
//!
//! This mirrors the dev backend pipeline, except the code generation
//! produces LLVM IR which is then compiled through LLVM's backend.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mir = @import("mir");
const MIR = mir.MIR;
const lir = @import("lir");
const LirExprStore = lir.LirExprStore;
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");
const builtins = @import("builtins");
const i128h = builtins.compiler_rt_128;
const eval_mod = @import("mod.zig");

const RocEnv = @import("roc_env.zig").RocEnv;
const createRocOps = @import("roc_env.zig").createRocOps;

const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

// LLVM code generation and compilation are accessed via the "llvm_compile"
// anonymous import, but only inside function bodies (lazy evaluation) to
// avoid breaking builds that don't link LLVM (e.g., playground wasm).

// Host ABI types
const RocOps = builtins.host_abi.RocOps;

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

/// Extract the result layout from a LIR expression for use as the overall
/// expression result layout. For blocks and other compound expressions where the
/// lowerer may have computed the layout from a CIR type variable with Content.err,
/// this follows through to the final/inner expression to find the true layout.
fn lirExprResultLayout(store: *const LirExprStore, expr_id: lir.LirExprId) ?layout.Idx {
    const LirExpr = lir.LirExpr;
    const expr: LirExpr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |b| lirExprResultLayout(store, b.final_expr),
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
        .closure => |c| store.getClosureData(c).closure_layout,
        .nominal => |n| lirExprResultLayout(store, n.backing_expr) orelse n.nominal_layout,
        .discriminant_switch => |ds| ds.result_layout,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .bool,
        .dec_literal => .dec,
        .str_literal => .str,
        .i64_literal => .i64,
        .i128_literal => .i128,
        .unary_not => .bool,
        .list,
        .empty_list,
        .empty_record,
        .lambda,
        .crash,
        .runtime_error,
        .hosted_call,
        .incref,
        .decref,
        .free,
        .for_loop,
        .while_loop,
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        .tag_payload_access,
        .break_expr,
        => null,
    };
}

/// LLVM-based evaluator for Roc expressions
///
/// Orchestrates the full compilation pipeline:
/// - Initializes with builtin modules
/// - Parses, canonicalizes, and type-checks expressions
/// - Lowers CIR to MIR, then MIR to LIR
/// - Generates LLVM bitcode
/// - Compiles to native object file
/// - Extracts and executes native code
pub const LlvmEvaluator = struct {
    allocator: Allocator,

    /// Loaded builtin module (Bool, Result, etc.)
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,

    /// RocOps environment for RC operations.
    /// Heap-allocated to ensure stable pointer for the roc_ops reference.
    roc_env: *RocEnv,

    /// RocOps instance for passing to generated code.
    /// Contains function pointers for allocation, deallocation, and error handling.
    roc_ops: RocOps,

    /// Global layout store shared across compilations (cached).
    global_layout_store: ?*layout.Store = null,

    pub const Error = error{
        OutOfMemory,
        Crash,
        RuntimeError,
        ParseError,
        CanonicalizeError,
        TypeError,
        ExecutionError,
        CompilationFailed,
    };

    /// Initialize the evaluator with builtin modules
    pub fn init(allocator: Allocator) Error!LlvmEvaluator {
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

        // Heap-allocate the RocOps environment so the pointer remains stable
        const roc_env = allocator.create(RocEnv) catch return error.OutOfMemory;
        roc_env.* = RocEnv.init(allocator);
        const roc_ops = createRocOps(roc_env);

        return LlvmEvaluator{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
            .roc_env = roc_env,
            .roc_ops = roc_ops,
        };
    }

    /// Clean up resources
    pub fn deinit(self: *LlvmEvaluator) void {
        if (self.global_layout_store) |ls| {
            ls.deinit();
            self.allocator.destroy(ls);
        }
        self.roc_env.deinit();
        self.allocator.destroy(self.roc_env);
        self.builtin_module.deinit();
    }

    /// Get or create the global layout store for resolving layouts of composite types.
    fn ensureGlobalLayoutStore(self: *LlvmEvaluator, all_module_envs: []const *ModuleEnv, builtin_module_env: ?*const ModuleEnv) Error!*layout.Store {
        if (self.global_layout_store) |ls| return ls;

        const builtin_str = if (all_module_envs.len > 0)
            all_module_envs[0].idents.builtin_str
        else
            null;

        const ls = self.allocator.create(layout.Store) catch return error.OutOfMemory;
        ls.* = layout.Store.initWithBuiltinEnv(all_module_envs, builtin_str, builtin_module_env, self.allocator, base.target.TargetUsize.native) catch {
            self.allocator.destroy(ls);
            return error.OutOfMemory;
        };

        self.global_layout_store = ls;
        return ls;
    }

    /// Result of code generation
    pub const CodeResult = struct {
        code: []const u8,
        allocator: Allocator,
        result_layout: LayoutIdx,
        /// Reference to the global layout store (owned by LlvmEvaluator, not this struct)
        layout_store: ?*layout.Store = null,
        entry_offset: usize = 0,

        pub fn deinit(self: *CodeResult) void {
            if (self.code.len > 0) {
                self.allocator.free(self.code);
            }
            // Note: layout_store is owned by LlvmEvaluator, not cleaned up here
        }
    };

    /// Generate code for a CIR expression (full pipeline)
    ///
    /// This runs the complete pipeline:
    /// 1. Lowering CIR to MIR
    /// 2. Lowering MIR to LIR
    /// 3. Reference counting insertion
    /// 4. Generating LLVM bitcode
    /// 5. Compiling bitcode to native object file
    /// 6. Extracting .text section with entry point
    pub fn generateCode(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        builtin_module_env: ?*const ModuleEnv,
    ) Error!CodeResult {
        // Find the module index for this module
        var module_idx: u32 = 0;
        for (all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                module_idx = @intCast(i);
                break;
            }
        }

        // Get or create the global layout store
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs, builtin_module_env);

        // In REPL sessions, module type stores get fresh type variables on each evaluation,
        // but the global layout store persists. Clear stale cached layouts.
        layout_store_ptr.resetModuleCache(all_module_envs);

        // 1. Lower CIR to MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            all_module_envs,
            &module_env.types,
            module_idx,
            null, // app_module_idx - not used for JIT evaluation
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        const mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.CompilationFailed;
        };

        // 2. Lower MIR to LIR
        var lir_store = LirExprStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, module_env, all_module_envs, module_env.idents.true_tag);
        defer mir_to_lir.deinit();

        const lir_expr_id = mir_to_lir.lower(mir_expr_id) catch {
            return error.CompilationFailed;
        };

        // 3. RC insertion pass on the LIR
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        const final_expr_id = rc_pass.insertRcOps(lir_expr_id) catch lir_expr_id;

        // Run RC insertion on all function definitions (symbol_defs)
        {
            var def_iter = lir_store.symbol_defs.iterator();
            while (def_iter.next()) |entry| {
                var fn_rc = lir.RcInsert.RcInsertPass.init(
                    self.allocator,
                    &lir_store,
                    layout_store_ptr,
                ) catch continue;
                defer fn_rc.deinit();
                entry.value_ptr.* = fn_rc.insertRcOps(entry.value_ptr.*) catch entry.value_ptr.*;
            }
        }

        // 4. Determine result layout from LIR expression.
        const result_layout = lirExprResultLayout(&lir_store, final_expr_id) orelse blk: {
            // Fallback: resolve from the CIR type variable
            const type_var = can.ModuleEnv.varFrom(expr_idx);
            var type_scope = types.TypeScope.init(self.allocator);
            defer type_scope.deinit();
            break :blk layout_store_ptr.fromTypeVar(module_idx, type_var, &type_scope, null) catch return error.OutOfMemory;
        };

        // 5. Generate LLVM bitcode
        const llvm_compile = @import("llvm_compile");
        const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

        var codegen = MonoLlvmCodeGen.init(self.allocator, &lir_store);
        defer codegen.deinit();

        // Provide layout store for composite types (records, tuples)
        codegen.layout_store = layout_store_ptr;

        var gen_result = codegen.generateCode(final_expr_id, result_layout) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.CompilationFailed => unreachable,
        };
        defer gen_result.deinit();

        // 6. Compile bitcode to object file
        // Use optimizations in release builds (CI), no optimizations in debug builds.
        const opt_level: llvm_compile.bindings.CodeGenOptLevel = if (builtin.mode == .Debug) .None else .Default;
        const object_bytes = llvm_compile.compileToObject(
            self.allocator,
            gen_result.bitcode,
            .{ .function_sections = false, .opt_level = opt_level },
        ) catch return error.CompilationFailed;
        defer self.allocator.free(object_bytes);

        // 7. Extract code, apply ELF relocations, and find entry point
        const object_reader = backend.dev.object_reader;
        const relocated = object_reader.extractAndRelocateElf(self.allocator, object_bytes) catch return error.CompilationFailed;
        const code_copy = relocated.code;

        return CodeResult{
            .code = code_copy,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .layout_store = layout_store_ptr,
            .entry_offset = relocated.entry_offset,
        };
    }
};

// All builtins are now called directly via LLVM function declarations.
// builtins.bc is linked into the LLVM module, so all builtin functions
// are available by name — no wrapper functions or function pointers needed.

// Tests

test "llvm evaluator initialization" {
    var evaluator = LlvmEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();
}
