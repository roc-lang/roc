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
        self.allocator.destroy(self.roc_env);
        self.builtin_module.deinit();
    }

    /// Result of code generation
    pub const CodeResult = struct {
        code: []const u8,
        allocator: Allocator,
        result_layout: LayoutIdx,
        entry_offset: usize = 0,

        pub fn deinit(self: *CodeResult) void {
            if (self.code.len > 0) {
                self.allocator.free(self.code);
            }
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

        // Create the lowerer
        var lowerer = MonoLower.init(self.allocator, &mono_store, all_module_envs, null, null);
        defer lowerer.deinit();

        // Lower the CIR expression to Mono IR
        const mono_expr_id = lowerer.lowerExpr(module_idx, expr_idx) catch {
            return error.UnsupportedExpression;
        };

        // 3. Determine result layout (using shared module)
        var type_env = std.AutoHashMap(u32, layout.Idx).init(self.allocator);
        defer type_env.deinit();
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = getExprLayoutWithTypeEnv(
            self.allocator,
            module_env,
            cir_expr,
            &type_env,
        );

        // 4. Generate LLVM bitcode
        const llvm_compile = @import("llvm_compile");
        const MonoLlvmCodeGen = llvm_compile.MonoLlvmCodeGen;

        var codegen = MonoLlvmCodeGen.init(self.allocator, &mono_store);
        defer codegen.deinit();

        const procs = mono_store.getProcs();
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch return error.UnsupportedExpression;
        }

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
