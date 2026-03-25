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
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mir = @import("mir");
const MIR = mir.MIR;
const lir = @import("lir");
const LirExprStore = lir.LirExprStore;
const lir_program_mod = @import("cir_to_lir.zig");
const LirProgram = lir_program_mod.LirProgram;
const buildPlatformTypeScope = lir_program_mod.buildPlatformTypeScope;
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");
const builtins = @import("builtins");

const Io = @import("io").Io;
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
const LlvmRocCallEntryFn = *const fn (*anyopaque, *anyopaque, ?*anyopaque) callconv(.c) void;

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

/// Extract the result layout from a LIR expression.
/// This is total for value-producing expressions and unit-valued RC/loop nodes.
fn lirExprResultLayout(store: *const LirExprStore, expr_id: lir.LirExprId) layout.Idx {
    const LirExpr = lir.LirExpr;
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
        .tag_payload_access => |tpa| tpa.payload_layout,
        .for_loop, .while_loop, .incref, .decref, .free => .zst,
        .crash => |c| c.ret_layout,
        .runtime_error => |re| re.ret_layout,
        .break_expr => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "LIR/eval invariant violated: lirExprResultLayout called on break_expr",
                    .{},
                );
            }
            unreachable;
        },

        // String-producing operations always return Str layout
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        => .str,
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
    pub fn init(allocator: Allocator, io: ?Io) Error!LlvmEvaluator {
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
        roc_env.* = RocEnv.init(allocator, io);
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
    pub fn ensureGlobalLayoutStore(self: *LlvmEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
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
        temp_dir_path: [:0]const u8,
        library_path: [:0]const u8,
        entry_fn: LlvmEntryFn,
        allocator: Allocator,
        result_layout: LayoutIdx,
        /// Reference to the global layout store (owned by LlvmEvaluator, not this struct)
        layout_store: ?*layout.Store = null,

        pub fn deinit(self: *CodeResult) void {
            self.library.close();
            const llvm_compile = @import("llvm_compile");
            llvm_compile.deleteTempArtifactDir(self.temp_dir_path);
            self.allocator.free(self.temp_dir_path);
            self.allocator.free(self.library_path);
            // Note: layout_store is owned by LlvmEvaluator, not cleaned up here
        }

        pub fn callWithResultPtrAndRocOps(self: *const CodeResult, result_ptr: *anyopaque, roc_ops: *anyopaque) void {
            self.entry_fn(result_ptr, roc_ops);
        }
    };

    pub const EntrypointCodeResult = struct {
        library: std.DynLib,
        temp_dir_path: [:0]const u8,
        library_path: [:0]const u8,
        entry_fn: LlvmRocCallEntryFn,
        allocator: Allocator,

        pub fn deinit(self: *EntrypointCodeResult) void {
            self.library.close();
            const llvm_compile = @import("llvm_compile");
            llvm_compile.deleteTempArtifactDir(self.temp_dir_path);
            self.allocator.free(self.temp_dir_path);
            self.allocator.free(self.library_path);
        }

        pub fn callRocABI(self: *const EntrypointCodeResult, roc_ops: *anyopaque, ret_ptr: *anyopaque, args_ptr: ?*anyopaque) void {
            self.entry_fn(roc_ops, ret_ptr, args_ptr);
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

        const mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.CompilationFailed;
        };

        const mir_mod = @import("mir");
        var lambda_set_store = mir_mod.LambdaSet.infer(self.allocator, &mir_store, all_module_envs) catch return error.OutOfMemory;
        defer lambda_set_store.deinit(self.allocator);

        // 2. Lower MIR to LIR
        var lir_store = LirExprStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &lambda_set_store, module_env.idents.true_tag);
        defer mir_to_lir.deinit();

        const lir_expr_id = mir_to_lir.lower(mir_expr_id) catch {
            return error.CompilationFailed;
        };

        // 3. RC insertion pass on the LIR
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        const final_expr_id = rc_pass.insertRcOps(lir_expr_id) catch lir_expr_id;

        // Run RC insertion on all function definitions (symbol_defs)
        lir.RcInsert.insertRcOpsIntoSymbolDefsBestEffort(self.allocator, &lir_store, layout_store_ptr);

        // 4. Determine result layout from LIR expression.
        const result_layout = lirExprResultLayout(&lir_store, final_expr_id);

        // 5. Generate LLVM bitcode
        const llvm_compile = @import("llvm_compile");
        const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

        var codegen = MonoLlvmCodeGen.init(self.allocator, &lir_store);
        defer codegen.deinit();

        // Provide layout store for composite types (records, tuples)
        codegen.layout_store = layout_store_ptr;

        var gen_result = codegen.generateCode(final_expr_id, result_layout) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.CompilationFailed => return error.CompilationFailed,
        };
        defer gen_result.deinit();

        // 6. Compile bitcode to a temporary shared library.
        // The evaluator is a parity/verification path, so prioritize fast compile
        // turnaround over optimized machine code.
        const opt_level: llvm_compile.bindings.CodeGenOptLevel = .None;
        var artifact = llvm_compile.compileToSharedLibrary(
            self.allocator,
            gen_result.bitcode,
            .{ .function_sections = false, .opt_level = opt_level },
        ) catch return error.CompilationFailed;
        errdefer artifact.deinit();

        var library = std.DynLib.open(artifact.library_path) catch return error.CompilationFailed;
        errdefer library.close();

        const entry_fn = library.lookup(LlvmEntryFn, "roc_eval") orelse
            library.lookup(LlvmEntryFn, "_roc_eval") orelse
            return error.CompilationFailed;

        return CodeResult{
            .library = library,
            .temp_dir_path = artifact.temp_dir_path,
            .library_path = artifact.library_path,
            .entry_fn = entry_fn,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .layout_store = layout_store_ptr,
        };
    }

    pub fn generateEntrypointCode(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
        platform_to_app_idents: *const std.AutoHashMap(base.Ident.Idx, base.Ident.Idx),
    ) Error!EntrypointCodeResult {
        var lir_program = LirProgram.init(self.allocator, base.target.TargetUsize.native);
        defer lir_program.deinit();

        var platform_type_scope = if (app_module_env) |app_env|
            try buildPlatformTypeScope(self.allocator, module_env, app_env, platform_to_app_idents)
        else
            null;
        defer if (platform_type_scope) |*ts| ts.deinit();

        var lower_result = lir_program.lowerEntrypointExpr(
            module_env,
            expr_idx,
            all_module_envs,
            app_module_env,
            arg_layouts.len == 0,
            if (platform_type_scope) |*ts| ts else null,
        ) catch |err| return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.RuntimeError => error.CompilationFailed,
            error.ModuleEnvNotFound => error.ModuleEnvNotFound,
        };
        defer lower_result.deinit();

        const entry_ret_stmt = lower_result.lir_store.addCFStmt(.{ .ret = .{ .value = lower_result.final_expr_id } }) catch {
            return error.OutOfMemory;
        };
        const entry_proc_id = lower_result.lir_store.addProcSpec(.{
            .name = lir.Symbol.none,
            .args = lir.LirPatternSpan.empty(),
            .arg_layouts = lir.LayoutIdxSpan.empty(),
            .body = entry_ret_stmt,
            .ret_layout = ret_layout,
            .closure_data_layout = null,
            .is_self_recursive = .not_self_recursive,
        }) catch {
            return error.OutOfMemory;
        };

        const llvm_compile = @import("llvm_compile");
        const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

        var codegen = MonoLlvmCodeGen.init(self.allocator, &lower_result.lir_store);
        defer codegen.deinit();
        codegen.layout_store = lower_result.layout_store;

        const Entrypoint = struct {
            symbol_name: []const u8,
            proc: lir.LIR.LirProcSpecId,
            arg_layouts: []const layout.Idx,
            ret_layout: layout.Idx,
        };
        const entrypoints = [_]Entrypoint{.{
            .symbol_name = "roc_entrypoint",
            .proc = entry_proc_id,
            .arg_layouts = arg_layouts,
            .ret_layout = ret_layout,
        }};

        var bitcode_result = codegen.generateEntrypointModule("roc_entrypoint_module", &entrypoints) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.CompilationFailed => return error.CompilationFailed,
        };
        defer bitcode_result.deinit();

        var artifact = llvm_compile.compileToSharedLibrary(
            self.allocator,
            bitcode_result.bitcode,
            .{ .function_sections = false, .opt_level = .None },
        ) catch return error.CompilationFailed;
        errdefer artifact.deinit();

        var library = std.DynLib.open(artifact.library_path) catch return error.CompilationFailed;
        errdefer library.close();

        const entry_fn = library.lookup(LlvmRocCallEntryFn, "roc_entrypoint") orelse
            library.lookup(LlvmRocCallEntryFn, "_roc_entrypoint") orelse
            return error.CompilationFailed;

        return EntrypointCodeResult{
            .library = library,
            .temp_dir_path = artifact.temp_dir_path,
            .library_path = artifact.library_path,
            .entry_fn = entry_fn,
            .allocator = self.allocator,
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
    var evaluator = LlvmEvaluator.init(std.testing.allocator, null) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();
}
