//! LLVM-based Evaluator for Roc expressions
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to Mono IR (globally unique symbols)
//! 5. Generating LLVM bitcode via MonoLlvmCodeGen
//! 6. Compiling bitcode to a native object file via LLVM
//! 7. Extracting the .text section from the object file
//! 8. Executing the generated code in mmap'd memory
//!
//! This mirrors the dev backend pipeline, except the code generation
//! produces LLVM IR which is then compiled through LLVM's backend.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mono = @import("mono");
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");
const builtins = @import("builtins");

const RocEnv = @import("roc_env.zig").RocEnv;
const createRocOps = @import("roc_env.zig").createRocOps;
const layout_resolve = @import("layout_resolve.zig");
const getExprLayoutWithTypeEnv = layout_resolve.getExprLayoutWithTypeEnv;
const codegen_prepare = @import("codegen_prepare.zig");

const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

// Mono IR types
const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;

// LLVM code generation and compilation are accessed via the "llvm_compile"
// anonymous import, but only inside function bodies (lazy evaluation) to
// avoid breaking builds that don't link LLVM (e.g., playground wasm).

// Host ABI types
const RocOps = builtins.host_abi.RocOps;

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

/// Extract the result layout from a Mono IR expression for use as the overall
/// expression result layout. For blocks and other compound expressions where the
/// lowerer may have computed the layout from a CIR type variable with Content.err,
/// this follows through to the final/inner expression to find the true layout.
fn monoExprResultLayout(store: *const MonoExprStore, expr_id: mono.MonoIR.MonoExprId) ?layout.Idx {
    const MonoExpr = mono.MonoIR.MonoExpr;
    const expr: MonoExpr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |b| monoExprResultLayout(store, b.final_expr),
        .if_then_else => |ite| ite.result_layout,
        .when => |w| w.result_layout,
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
        .nominal => |n| n.nominal_layout,
        .i64_literal => .i64,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .bool,
        .i128_literal => .i128,
        .dec_literal => .dec,
        .str_literal => .str,
        .unary_not => .bool,
        else => null,
    };
}

/// LLVM-based evaluator for Roc expressions
///
/// Orchestrates the full compilation pipeline:
/// - Initializes with builtin modules
/// - Parses, canonicalizes, and type-checks expressions
/// - Lowers to Mono IR
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

    /// When true, disables LLVM optimization passes (uses OptLevel.None).
    /// This is a bool rather than the LLVM type to preserve the lazy import
    /// pattern — non-LLVM builds don't need the LLVM bindings at struct scope.
    disable_optimizations: bool = false,

    /// Global layout store shared across compilations (cached).
    global_layout_store: ?*layout.Store = null,

    pub const Error = error{
        OutOfMemory,
        UnsupportedType,
        UnsupportedExpression,
        Crash,
        RuntimeError,
        ParseError,
        CanonicalizeError,
        TypeError,
        ExecutionError,
        CompilationFailed,
        UnsupportedLayout,
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
    /// 1. Pre-codegen transforms (lambda lifting, closure transformation)
    /// 2. Lowering CIR to Mono IR
    /// 3. Generating LLVM bitcode
    /// 4. Compiling bitcode to native object file
    /// 5. Extracting .text section with entry point
    pub fn generateCode(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        builtin_module_env: ?*const ModuleEnv,
    ) Error!CodeResult {
        // 1. Run pre-codegen transforms
        var mutable_envs = [_]*ModuleEnv{module_env};
        const inference = codegen_prepare.prepareModulesForCodegen(
            self.allocator,
            &mutable_envs,
        ) catch return error.OutOfMemory;
        defer {
            inference.deinit();
            self.allocator.destroy(inference);
        }

        // 2. Create Mono IR store and lower
        var mono_store = MonoExprStore.init(self.allocator);
        defer mono_store.deinit();

        // Find the module index for this module
        var module_idx: u16 = 0;
        for (all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                module_idx = @intCast(i);
                break;
            }
        }

        // Get or create the global layout store
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs, builtin_module_env);

        // Create the lowerer with the layout store
        var lowerer = MonoLower.init(self.allocator, &mono_store, all_module_envs, null, layout_store_ptr);
        defer lowerer.deinit();

        // Lower the CIR expression to Mono IR
        const mono_expr_id = lowerer.lowerExpr(module_idx, expr_idx) catch {
            return error.UnsupportedExpression;
        };

        // 3. Determine result layout from Mono IR expression.
        // Prefer the layout embedded in the Mono expression because the CIR
        // type variable can have Content.err for expressions involving the `?`
        // operator at top level (where the Err branch desugars to runtime_error).
        const result_layout = monoExprResultLayout(&mono_store, mono_expr_id) orelse blk: {
            // Fallback: resolve from the CIR type variable
            const type_var = can.ModuleEnv.varFrom(expr_idx);
            var type_scope = types.TypeScope.init(self.allocator);
            defer type_scope.deinit();
            break :blk layout_store_ptr.fromTypeVar(module_idx, type_var, &type_scope, null) catch {
                return error.UnsupportedExpression;
            };
        };

        // 4. Generate LLVM bitcode
        const llvm_compile = @import("llvm_compile");
        const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

        var codegen = MonoLlvmCodeGen.init(self.allocator, &mono_store);
        defer codegen.deinit();

        // Provide addresses of Dec builtins for indirect calls
        codegen.dec_mul_addr = @intFromPtr(&builtins.dec.mulSaturatedC);
        codegen.dec_div_addr = @intFromPtr(&builtins.dec.divC);
        codegen.dec_div_trunc_addr = @intFromPtr(&builtins.dec.divTruncC);
        codegen.alloc_with_refcount_addr = @intFromPtr(&builtins.utils.allocateWithRefcountC);
        codegen.list_append_addr = @intFromPtr(&builtins.list.listAppendSafeC);
        codegen.memcpy_addr = @intFromPtr(&builtins.list.copy_fallback);
        codegen.list_with_capacity_addr = @intFromPtr(&builtins.list.listWithCapacityC);
        codegen.list_append_unsafe_addr = @intFromPtr(&builtins.list.listAppendUnsafeC);
        codegen.rc_none_addr = @intFromPtr(&builtins.list.rcNone);
        codegen.list_prepend_addr = @intFromPtr(&builtins.list.listPrepend);

        // Provide layout store for composite types (records, tuples)
        codegen.layout_store = layout_store_ptr;

        var gen_result = codegen.generateCode(mono_expr_id, result_layout) catch {
            return error.UnsupportedExpression;
        };
        defer gen_result.deinit();

        // 5. Compile bitcode to object file
        const object_bytes = llvm_compile.compileToObject(
            self.allocator,
            gen_result.bitcode,
            .{ .function_sections = false, .opt_level = if (self.disable_optimizations) .None else .Default },
        ) catch return error.CompilationFailed;
        defer self.allocator.free(object_bytes);

        // 6. Extract .text section and find entry point
        const object_reader = backend.object_reader;
        const code_info = object_reader.extractCodeSectionWithEntry(object_bytes) catch return error.CompilationFailed;

        // 7. Copy code (it's a slice into object_bytes which we're about to free)
        const code_copy = self.allocator.dupe(u8, code_info.code) catch return error.OutOfMemory;

        return CodeResult{
            .code = code_copy,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .layout_store = layout_store_ptr,
            .entry_offset = code_info.entry_offset,
        };
    }
};

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
