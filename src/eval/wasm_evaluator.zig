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
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");
const lir_program_mod = @import("cir_to_lir.zig");
const LirProgram = lir_program_mod.LirProgram;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;
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
    /// Shared LIR lowering pipeline (layout store, type resolver, CIR→MIR→LIR→RC).
    lir_program: LirProgram,
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
            .lir_program = LirProgram.init(allocator, base.target.TargetUsize.u32),
        };
    }

    pub fn deinit(self: *WasmEvaluator) void {
        self.lir_program.deinit();
        self.builtin_module.deinit();
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
        // Lower CIR → MIR → LIR → RC via shared pipeline
        var lower_result = self.lir_program.lowerExpr(
            module_env,
            expr_idx,
            all_module_envs,
            null, // app_module_env - not used for Wasm evaluation
        ) catch |err| return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.RuntimeError => error.RuntimeError,
            error.ModuleEnvNotFound => error.RuntimeError,
        };
        defer lower_result.deinit();

        // Generate wasm module
        var codegen = WasmCodeGen.init(self.allocator, &lower_result.lir_store, lower_result.layout_store);
        codegen.wasm_stack_bytes = self.wasm_stack_bytes;
        defer codegen.deinit();

        const gen_result = codegen.generateModule(lower_result.final_expr_id, lower_result.result_layout, entrypoint_name) catch {
            return error.RuntimeError;
        };

        return WasmCodeResult{
            .wasm_bytes = gen_result.wasm_bytes,
            .result_layout = gen_result.result_layout,
            .tuple_len = lower_result.tuple_len,
            .has_imports = gen_result.has_imports,
            .allocator = self.allocator,
        };
    }
};
