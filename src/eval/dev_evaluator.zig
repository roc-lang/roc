//! Dev Backend Evaluator
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to Mono IR (globally unique symbols)
//! 5. Generating native machine code (x86_64/aarch64)
//! 6. Executing the generated code
//!
//! Code generation uses Mono IR with globally unique MonoSymbol references,
//! eliminating cross-module index collisions.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const backend = @import("backend");
const mono = @import("mono");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins");
const layout_resolve = @import("layout_resolve.zig");
const getExprLayoutWithTypeEnv = layout_resolve.getExprLayoutWithTypeEnv;
const roc_env_mod = @import("roc_env.zig");
const RocEnv = roc_env_mod.RocEnv;
const createRocOps = roc_env_mod.createRocOps;
const codegen_prepare = @import("codegen_prepare.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

// Host ABI types for RocOps
const RocOps = builtins.host_abi.RocOps;

// Mono IR types
const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;

// Static data interner for string literals
const StaticDataInterner = backend.StaticDataInterner;
const JitStaticBackend = StaticDataInterner.JitStaticBackend;

// Re-export from shared module
pub const list_i64_layout = layout_resolve.list_i64_layout;

// RocEnv and createRocOps are imported from roc_env.zig

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

/// Dev backend evaluator
///
/// Orchestrates the compilation pipeline:
/// - Initializes with builtin modules
/// - Parses, canonicalizes, and type-checks expressions
///
/// NOTE: Native code generation is not currently implemented.
pub const DevEvaluator = struct {
    allocator: Allocator,

    /// Loaded builtin module (Bool, Result, etc.)
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,

    /// Backend for static data allocation (arena-based for JIT)
    /// Heap-allocated to ensure stable pointer for the interner's Backend reference
    jit_backend: *JitStaticBackend,

    /// Global interner for static data (string literals, etc.)
    /// Lives for the duration of the evaluator session, enabling deduplication
    /// across multiple evaluations in a REPL session.
    static_interner: StaticDataInterner,

    /// RocOps environment for RC operations.
    /// Heap-allocated to ensure stable pointer for the roc_ops reference.
    roc_env: *RocEnv,

    /// RocOps instance for passing to generated code.
    /// Contains function pointers for allocation, deallocation, and error handling.
    /// Required for proper RC tracking (incref/decref operations).
    roc_ops: RocOps,

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
    };

    /// Initialize the evaluator with builtin modules
    pub fn init(allocator: Allocator) Error!DevEvaluator {
        // Load compiled builtins
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

        // Heap-allocate the JIT backend so the pointer remains stable
        const jit_backend = allocator.create(JitStaticBackend) catch return error.OutOfMemory;
        jit_backend.* = JitStaticBackend.init(allocator);

        // Initialize the interner with a pointer to the heap-allocated backend
        const static_interner = StaticDataInterner.init(allocator, jit_backend.backend());

        // Heap-allocate the RocOps environment so the pointer remains stable
        const roc_env = allocator.create(RocEnv) catch return error.OutOfMemory;
        roc_env.* = RocEnv.init(allocator);
        const roc_ops = createRocOps(roc_env);

        return DevEvaluator{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
            .jit_backend = jit_backend,
            .static_interner = static_interner,
            .roc_env = roc_env,
            .roc_ops = roc_ops,
        };
    }

    /// Clean up resources
    pub fn deinit(self: *DevEvaluator) void {
        self.static_interner.deinit();
        self.jit_backend.deinit();
        self.allocator.destroy(self.jit_backend);
        self.allocator.destroy(self.roc_env);
        self.builtin_module.deinit();
    }

    /// Prepare modules for code generation by running the closure pipeline.
    ///
    /// This runs:
    /// 1. LambdaLifter on each module (extracts closure bodies)
    /// 2. LambdaSetInference across ALL modules (assigns global names)
    /// 3. ClosureTransformer on each module (uses inference results)
    ///
    /// Returns the LambdaSetInference, which should be kept alive during codegen.
    pub fn prepareModulesForCodegen(
        self: *DevEvaluator,
        modules: []*ModuleEnv,
    ) Error!*can.LambdaSetInference {
        return codegen_prepare.prepareModulesForCodegen(self.allocator, modules) catch return error.OutOfMemory;
    }

    /// Result of code generation
    pub const CodeResult = struct {
        code: []const u8,
        allocator: Allocator,
        result_layout: LayoutIdx,
        tuple_len: usize = 1,
        crash_message: ?[]const u8 = null,
        /// Offset from start of code where execution should begin
        /// (procedures may be compiled before the main expression)
        entry_offset: usize = 0,

        pub fn deinit(self: *CodeResult) void {
            if (self.code.len > 0) {
                self.allocator.free(self.code);
            }
        }
    };

    /// Generate code for a CIR expression
    ///
    /// This lowers CIR to Mono IR and then generates native machine code.
    pub fn generateCode(
        self: *DevEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
    ) Error!CodeResult {
        // Create a Mono IR store for lowered expressions
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

        // Determine the result layout
        var type_env = std.AutoHashMap(u32, LayoutIdx).init(self.allocator);
        defer type_env.deinit();
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = getExprLayoutWithTypeEnv(self.allocator, module_env, cir_expr, &type_env);

        // Detect tuple expressions to set tuple_len
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        // Create the code generator
        var codegen = backend.MonoExprCodeGen.init(
            self.allocator,
            &mono_store,
            &self.static_interner,
        );
        defer codegen.deinit();

        // Compile all procedures first (for recursive functions)
        // This ensures recursive closures are compiled as complete procedures
        // before we generate calls to them.
        const procs = mono_store.getProcs();
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch {
                return error.UnsupportedExpression;
            };
        }

        // Generate code for the expression
        const gen_result = codegen.generateCode(mono_expr_id, result_layout, tuple_len) catch {
            return error.UnsupportedExpression;
        };

        return CodeResult{
            .code = gen_result.code,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .tuple_len = tuple_len,
            .entry_offset = gen_result.entry_offset,
        };
    }

    /// Generate native code from source code string (full pipeline)
    ///
    /// NOTE: Native code generation is not currently implemented.
    /// This function exists to maintain the API but always returns an error.
    pub fn generateCodeFromSource(_: *DevEvaluator, _: []const u8) Error!CodeResult {
        return error.UnsupportedExpression;
    }

    /// Result of evaluation
    pub const EvalResult = union(enum) {
        i64_val: i64,
        u64_val: u64,
        f64_val: f64,
        i128_val: i128,
        u128_val: u128,
        str_val: []const u8, // String contents (caller owns memory)

        pub fn format(self_val: EvalResult, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self_val) {
                .i64_val => |v| try writer.print("{}", .{v}),
                .u64_val => |v| try writer.print("{}", .{v}),
                .f64_val => |v| try writer.print("{d}", .{v}),
                .i128_val => |v| try writer.print("{}", .{v}),
                .u128_val => |v| try writer.print("{}", .{v}),
                .str_val => |v| try writer.print("\"{s}\"", .{v}),
            }
        }
    };

    /// RocStr constants
    const ROCSTR_SIZE: usize = 24;
    const SMALL_STR_MASK: u8 = 0x80;

    /// Evaluate source code and return the result
    pub fn evaluate(self: *DevEvaluator, source: []const u8) Error!EvalResult {
        var code_result = try self.generateCodeFromSource(source);
        defer code_result.deinit();

        if (code_result.code.len == 0) {
            if (code_result.crash_message != null) {
                return error.Crash;
            }
            return error.UnsupportedExpression;
        }

        var executable = backend.ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset) catch return error.ExecutionError;
        defer executable.deinit();

        return switch (code_result.result_layout) {
            .i64, .i8, .i16, .i32 => blk: {
                var result: i64 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .i64_val = result };
            },
            .u64, .u8, .u16, .u32, .bool => blk: {
                var result: u64 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .u64_val = result };
            },
            .f64 => blk: {
                var result: f64 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .f64_val = result };
            },
            .f32 => blk: {
                // F32 stores 4 bytes, read as f32 then convert to f64 for display
                var result: f32 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .f64_val = @floatCast(result) };
            },
            .i128 => blk: {
                var result: i128 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .i128_val = result };
            },
            .u128 => blk: {
                var result: u128 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .u128_val = result };
            },
            .dec => blk: {
                var result: i128 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                break :blk EvalResult{ .i128_val = result };
            },
            .str => blk: {
                // RocStr is 24 bytes: { bytes: *u8, length: usize, capacity: usize }
                var roc_str_bytes: [ROCSTR_SIZE]u8 = undefined;
                executable.callWithResultPtrAndRocOps(@ptrCast(&roc_str_bytes), @constCast(&self.roc_ops));

                // Check if it's a small string (high bit of last byte is set)
                const len_byte = roc_str_bytes[ROCSTR_SIZE - 1];
                if (len_byte & SMALL_STR_MASK != 0) {
                    // Small string: length is in the last byte with mask removed
                    const len = len_byte & ~SMALL_STR_MASK;
                    // Copy the string data to newly allocated memory
                    const str_copy = self.allocator.dupe(u8, roc_str_bytes[0..len]) catch return error.OutOfMemory;
                    break :blk EvalResult{ .str_val = str_copy };
                } else {
                    // Big string: first 8 bytes are pointer to data, next 8 bytes are length
                    const bytes_ptr: *const [*]const u8 = @ptrCast(@alignCast(&roc_str_bytes[0]));
                    const length_ptr: *const u64 = @ptrCast(@alignCast(&roc_str_bytes[8]));

                    const data_ptr = bytes_ptr.*;
                    const length = length_ptr.*;

                    // Handle the seamless slice bit (high bit of length indicates seamless slice)
                    const SEAMLESS_SLICE_BIT: u64 = @as(u64, @bitCast(@as(i64, std.math.minInt(i64))));
                    const actual_length = length & ~SEAMLESS_SLICE_BIT;

                    if (actual_length == 0) {
                        break :blk EvalResult{ .str_val = "" };
                    }

                    // Copy the string data from the heap-allocated memory
                    const str_copy = self.allocator.dupe(u8, data_ptr[0..actual_length]) catch return error.OutOfMemory;
                    break :blk EvalResult{ .str_val = str_copy };
                }
            },
            else => return error.UnsupportedType,
        };
    }
};

// Layout resolution functions are imported from layout_resolve.zig

// Tests

test "dev evaluator initialization" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();
}
