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
//! 8. Compiling bitcode to a temporary shared library via LLVM + LLD
//! 9. Loading the shared library with the platform dynamic loader
//! 10. Calling the generated entrypoint from the loaded library
//!
//! This mirrors the dev backend pipeline, except the code generation
//! produces LLVM IR which is then compiled through LLVM's backend and
//! executed via a temporary shared library.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mir = @import("mir");
const MIR = mir.MIR;
const lir = @import("lir");
const LirStore = lir.LirStore;
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");
const builtins = @import("builtins");

const RocEnv = @import("roc_env.zig").RocEnv;
const createRocOps = @import("roc_env.zig").createRocOps;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

// LLVM code generation and compilation are accessed via the "llvm_compile"
// anonymous import, but only inside function bodies (lazy evaluation) to
// avoid breaking builds that don't link LLVM (e.g., playground wasm).

// Host ABI types
const RocOps = builtins.host_abi.RocOps;
const LlvmEntryFn = *const fn (*anyopaque, *anyopaque) callconv(.c) void;

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

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

    /// Shared type-side resolver layered on top of the global layout store.
    global_type_layout_resolver: ?*layout.TypeLayoutResolver = null,

    pub const Error = error{
        OutOfMemory,
        Crash,
        RuntimeError,
        ParseError,
        CanonicalizeError,
        TypeError,
        ExecutionError,
        CompilationFailed,
        ModuleEnvNotFound,
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
        if (self.global_type_layout_resolver) |resolver| {
            resolver.deinit();
            self.allocator.destroy(resolver);
        }
        if (self.global_layout_store) |ls| {
            ls.deinit();
            self.allocator.destroy(ls);
        }
        self.roc_env.deinit();
        self.allocator.destroy(self.roc_env);
        self.builtin_module.deinit();
    }

    /// Get or create the global layout store for resolving layouts of composite types.
    fn ensureGlobalLayoutStore(self: *LlvmEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        if (self.global_layout_store) |ls| return ls;

        var builtin_str: ?base.Ident.Idx = null;
        for (all_module_envs) |env| {
            if (isBuiltinModuleEnv(env)) {
                builtin_str = env.idents.builtin_str;
                break;
            }
        }

        const ls = self.allocator.create(layout.Store) catch return error.OutOfMemory;
        ls.* = layout.Store.init(all_module_envs, builtin_str, self.allocator, base.target.TargetUsize.native) catch {
            self.allocator.destroy(ls);
            return error.OutOfMemory;
        };

        self.global_layout_store = ls;
        return ls;
    }

    fn ensureGlobalTypeLayoutResolver(self: *LlvmEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.TypeLayoutResolver {
        if (self.global_type_layout_resolver) |resolver| return resolver;

        const layout_store = try self.ensureGlobalLayoutStore(all_module_envs);
        const resolver = self.allocator.create(layout.TypeLayoutResolver) catch return error.OutOfMemory;
        resolver.* = layout.TypeLayoutResolver.init(layout_store);
        self.global_type_layout_resolver = resolver;
        return resolver;
    }

    /// Result of code generation
    pub const CodeResult = struct {
        library: std.DynLib,
        library_path: [:0]const u8,
        entry_fn: LlvmEntryFn,
        allocator: Allocator,
        result_layout: LayoutIdx,
        /// Reference to the global layout store (owned by LlvmEvaluator, not this struct)
        layout_store: ?*layout.Store = null,

        pub fn deinit(self: *CodeResult) void {
            self.library.close();
            std.fs.cwd().deleteFile(self.library_path) catch {};
            self.allocator.free(self.library_path);
            // Note: layout_store is owned by LlvmEvaluator, not cleaned up here
        }

        pub fn callWithResultPtrAndRocOps(self: *const CodeResult, result_ptr: *anyopaque, roc_ops: *anyopaque) void {
            self.entry_fn(result_ptr, roc_ops);
        }
    };

    /// Generate code for a CIR expression (full pipeline)
    ///
    /// This runs the complete pipeline:
    /// 1. Lowering CIR to MIR
    /// 2. Lowering MIR to LIR
    /// 3. Reference counting insertion
    /// 4. Generating LLVM bitcode
    /// 5. Compiling bitcode to a temporary shared library
    /// 6. Loading the shared library and resolving roc_eval
    pub fn generateCode(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        _: ?*const ModuleEnv,
    ) Error!CodeResult {
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }

        module_env.imports.resolveImports(module_env, all_module_envs);

        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;

        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);
        layout_store_ptr.setModuleEnvs(all_module_envs);
        const type_layout_resolver_ptr = try self.ensureGlobalTypeLayoutResolver(all_module_envs);

        // In REPL sessions, module type stores get fresh type variables on each evaluation,
        // but the shared type-layout resolver persists. Clear stale type-side caches.
        type_layout_resolver_ptr.resetModuleCache(all_module_envs);

        // 1. Lower CIR to MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        // TODO: implement LLVM code generation for statement-only LIR.
        var monomorphization = mir.Monomorphize.runExpr(
            self.allocator,
            all_module_envs,
            &module_env.types,
            module_idx,
            null, // app_module_idx - not used for JIT evaluation
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
            null, // app_module_idx - not used for JIT evaluation
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        const root_const_id = mir_lower.lowerRootConst(expr_idx) catch {
            return error.CompilationFailed;
        };

        const mir_mod = @import("mir");
        var mir_analyses = try mir_mod.Analyses.init(self.allocator, &mir_store, all_module_envs, module_idx, &.{root_const_id});
        defer mir_analyses.deinit();

        // 2. Lower MIR to LIR
        var lir_store = LirStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &mir_analyses);
        defer mir_to_lir.deinit();

        const root_proc_id = mir_to_lir.lower(root_const_id) catch {
            return error.CompilationFailed;
        };

        // 3. RC insertion pass over the full lowered proc graph.
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        try rc_pass.insertRcOpsForAllProcs();

        // 4. Determine result layout from LIR expression.
        const result_layout = lir_store.getProcSpec(root_proc_id).ret_layout;

        // 5. Generate LLVM bitcode
        const llvm_compile = @import("llvm_compile");
        const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

        var codegen = MonoLlvmCodeGen.init(self.allocator, &lir_store);
        defer codegen.deinit();

        // Provide layout store for composite types (records, tuples)
        codegen.layout_store = layout_store_ptr;

        var gen_result = codegen.generateCode(root_proc_id, result_layout) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.CompilationFailed => unreachable,
        };
        defer gen_result.deinit();

        // 6. Compile bitcode to a temporary shared library.
        // The evaluator is a parity/verification path, so prioritize fast compile
        // turnaround over optimized machine code.
        const opt_level: llvm_compile.bindings.CodeGenOptLevel = .None;
        const library_path = llvm_compile.compileToSharedLibrary(
            self.allocator,
            gen_result.bitcode,
            .{ .function_sections = false, .opt_level = opt_level },
        ) catch return error.CompilationFailed;
        errdefer {
            std.fs.cwd().deleteFile(library_path) catch {};
            self.allocator.free(library_path);
        }

        var library = std.DynLib.open(library_path) catch return error.CompilationFailed;
        errdefer library.close();

        const entry_fn = library.lookup(LlvmEntryFn, "roc_eval") orelse
            library.lookup(LlvmEntryFn, "_roc_eval") orelse
            return error.CompilationFailed;

        return CodeResult{
            .library = library,
            .library_path = library_path,
            .entry_fn = entry_fn,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .layout_store = layout_store_ptr,
        };
    }

    fn findModuleEnvIdx(all_module_envs: []const *ModuleEnv, module_env: *ModuleEnv) ?u32 {
        for (all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                return @intCast(i);
            }
        }

        return null;
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
