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

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

// Host ABI types for RocOps
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

// Mono IR types
const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;

// Static data interner for string literals
const StaticDataInterner = backend.StaticDataInterner;
const JitStaticBackend = StaticDataInterner.JitStaticBackend;

/// Environment for RocOps in the DevEvaluator.
/// Manages arena-backed allocation where free() is a no-op.
/// This enables proper RC tracking for in-place mutation optimization
/// while arenas handle actual memory deallocation.
const DevRocEnv = struct {
    allocator: Allocator,

    fn init(allocator: Allocator) DevRocEnv {
        return .{ .allocator = allocator };
    }

    /// Allocation function for RocOps.
    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));

        // Allocate memory with the requested alignment
        const ptr = switch (roc_alloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, .@"1", roc_alloc.length),
            2 => self.allocator.alignedAlloc(u8, .@"2", roc_alloc.length),
            4 => self.allocator.alignedAlloc(u8, .@"4", roc_alloc.length),
            8 => self.allocator.alignedAlloc(u8, .@"8", roc_alloc.length),
            16 => self.allocator.alignedAlloc(u8, .@"16", roc_alloc.length),
            else => @panic("DevRocEnv: Unsupported alignment"),
        } catch {
            @panic("DevRocEnv: Allocation failed");
        };

        roc_alloc.answer = @ptrCast(ptr.ptr);
    }

    /// Deallocation function for RocOps.
    /// This is a NO-OP because arenas manage actual memory deallocation.
    /// RC operations still work to track uniqueness for in-place mutation.
    fn rocDeallocFn(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
        // Intentional no-op: arena manages actual deallocation
        // This allows RC to track uniqueness while arena handles cleanup
    }

    /// Reallocation function for RocOps.
    /// For arena-based allocation, we just allocate new memory.
    /// Since free is a no-op, we don't need to worry about freeing the old allocation.
    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));

        // Allocate new memory with the requested alignment
        const new_ptr = switch (roc_realloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, .@"1", roc_realloc.new_length),
            2 => self.allocator.alignedAlloc(u8, .@"2", roc_realloc.new_length),
            4 => self.allocator.alignedAlloc(u8, .@"4", roc_realloc.new_length),
            8 => self.allocator.alignedAlloc(u8, .@"8", roc_realloc.new_length),
            16 => self.allocator.alignedAlloc(u8, .@"16", roc_realloc.new_length),
            else => @panic("DevRocEnv: Unsupported alignment"),
        } catch {
            @panic("DevRocEnv: Reallocation failed");
        };

        // Copy old data from the existing allocation
        // Note: The old pointer is in roc_realloc.answer
        // We copy new_length bytes (assuming the old allocation was at least that large)
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        @memcpy(new_ptr[0..roc_realloc.new_length], old_ptr[0..roc_realloc.new_length]);

        // Return the new pointer
        roc_realloc.answer = @ptrCast(new_ptr.ptr);
    }

    /// Debug output function.
    fn rocDbgFn(roc_dbg: *const RocDbg, _: *anyopaque) callconv(.c) void {
        // On freestanding (WASM), skip debug output to avoid thread locking
        if (builtin.os.tag != .freestanding) {
            const msg = roc_dbg.utf8_bytes[0..roc_dbg.len];
            std.debug.print("[dbg] {s}\n", .{msg});
        }
    }

    /// Expect failed function.
    fn rocExpectFailedFn(_: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
        // On freestanding (WASM), skip debug output to avoid thread locking
        if (builtin.os.tag != .freestanding) {
            std.debug.print("[expect failed]\n", .{});
        }
    }

    /// Crash function.
    fn rocCrashedFn(roc_crashed: *const RocCrashed, _: *anyopaque) callconv(.c) noreturn {
        // On freestanding (WASM), just panic without debug output to avoid thread locking
        if (builtin.os.tag == .freestanding) {
            @panic("Roc crashed");
        } else {
            const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
            std.debug.print("Roc crashed: {s}\n", .{msg});
            unreachable;
        }
    }
};

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
    roc_env: *DevRocEnv,

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
        const roc_env = allocator.create(DevRocEnv) catch return error.OutOfMemory;
        roc_env.* = DevRocEnv.init(allocator);

        // Create RocOps with function pointers to the DevRocEnv handlers
        // Use a static dummy array for hosted_fns since count=0 means no hosted functions
        // This avoids undefined behavior from using `undefined` for the pointer
        const empty_hosted_fns = struct {
            fn dummyHostedFn(_: *RocOps, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
            var empty: [1]builtins.host_abi.HostedFn = .{&dummyHostedFn};
        };
        const roc_ops = RocOps{
            .env = @ptrCast(roc_env),
            .roc_alloc = &DevRocEnv.rocAllocFn,
            .roc_dealloc = &DevRocEnv.rocDeallocFn,
            .roc_realloc = &DevRocEnv.rocReallocFn,
            .roc_dbg = &DevRocEnv.rocDbgFn,
            .roc_expect_failed = &DevRocEnv.rocExpectFailedFn,
            .roc_crashed = &DevRocEnv.rocCrashedFn,
            .hosted_fns = .{ .count = 0, .fns = &empty_hosted_fns.empty },
        };

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
        // 1. Run LambdaLifter on each module (extracts closure bodies)
        for (modules) |module| {
            if (!module.is_lambda_lifted) {
                var top_level_patterns = std.AutoHashMap(can.CIR.Pattern.Idx, void).init(self.allocator);
                defer top_level_patterns.deinit();

                // Mark top-level patterns from all_statements
                const stmts = module.store.sliceStatements(module.all_statements);
                for (stmts) |stmt_idx| {
                    const stmt = module.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |decl| {
                            top_level_patterns.put(decl.pattern, {}) catch {};
                        },
                        else => {},
                    }
                }

                var lifter = can.LambdaLifter.init(self.allocator, module, &top_level_patterns);
                defer lifter.deinit();
                module.is_lambda_lifted = true;
            }
        }

        // 2. Run Lambda Set Inference across ALL modules (assigns global names)
        const inference = self.allocator.create(can.LambdaSetInference) catch return error.OutOfMemory;
        inference.* = can.LambdaSetInference.init(self.allocator);
        inference.inferAll(modules) catch return error.OutOfMemory;

        // 3. Run ClosureTransformer on each module (uses inference results)
        for (modules) |module| {
            if (!module.is_defunctionalized) {
                var transformer = can.ClosureTransformer.initWithInference(self.allocator, module, inference);
                defer transformer.deinit();
                module.is_defunctionalized = true;
            }
        }

        return inference;
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

        // Create a layout store for resolving layouts of composite types
        var layout_store = layout.Store.init(module_env, &module_env.types, module_env.idents.builtin_str) catch {
            return error.OutOfMemory;
        };
        defer layout_store.deinit();

        // Create the lowerer with the layout store
        var lowerer = MonoLower.init(self.allocator, &mono_store, all_module_envs, null, &layout_store);
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

        // Create the code generator with the layout store
        var codegen = backend.MonoExprCodeGen.init(
            self.allocator,
            &mono_store,
            &layout_store,
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

/// Get the Layout for a CIR expression
fn getExprLayoutWithTypeEnv(allocator: Allocator, module_env: *ModuleEnv, expr: CIR.Expr, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    return switch (expr) {
        .e_num => |num| switch (num.kind) {
            .i8, .i16, .i32, .i64, .num_unbound, .int_unbound => .i64,
            .u8, .u16, .u32, .u64 => .u64,
            .i128 => .i128,
            .u128 => .u128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
        },
        .e_frac_f32 => .f32,
        .e_frac_f64 => .f64,
        .e_dec, .e_dec_small => .dec,
        .e_typed_int => |ti| getTypedIntLayout(module_env, ti.type_name),
        .e_binop => |binop| getBinopLayout(allocator, module_env, binop, type_env),
        .e_unary_minus => |unary| blk: {
            const inner_expr = module_env.store.getExpr(unary.expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, inner_expr, type_env);
        },
        .e_nominal => |nom| blk: {
            const backing_expr = module_env.store.getExpr(nom.backing_expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, backing_expr, type_env);
        },
        .e_nominal_external => |nom| blk: {
            const backing_expr = module_env.store.getExpr(nom.backing_expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, backing_expr, type_env);
        },
        .e_if => |if_expr| blk: {
            const else_expr = module_env.store.getExpr(if_expr.final_else);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, else_expr, type_env);
        },
        .e_block => |block| getBlockLayout(allocator, module_env, block, type_env),
        .e_lookup_local => |lookup| blk: {
            const pattern_key = @intFromEnum(lookup.pattern_idx);
            break :blk type_env.get(pattern_key) orelse .i64;
        },
        .e_str, .e_str_segment => .str,
        .e_call => |call| blk: {
            const func_expr = module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lambda => |lambda| {
                    const body_expr = module_env.store.getExpr(lambda.body);
                    switch (body_expr) {
                        .e_lookup_local => {
                            const args = module_env.store.sliceExpr(call.args);
                            if (args.len > 0) {
                                const arg_expr = module_env.store.getExpr(args[0]);
                                break :blk getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                            }
                        },
                        else => {},
                    }
                    break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
                },
                .e_closure => |closure| {
                    const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
                    switch (lambda_expr) {
                        .e_lambda => |lambda| {
                            const body_expr = module_env.store.getExpr(lambda.body);
                            switch (body_expr) {
                                .e_lookup_local => {
                                    const args = module_env.store.sliceExpr(call.args);
                                    if (args.len > 0) {
                                        const arg_expr = module_env.store.getExpr(args[0]);
                                        break :blk getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                                    }
                                },
                                else => {},
                            }
                            break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
                        },
                        else => {},
                    }
                    break :blk .i64;
                },
                .e_lookup_local => {
                    const args = module_env.store.sliceExpr(call.args);
                    if (args.len > 0) {
                        const arg_expr = module_env.store.getExpr(args[0]);
                        const arg_layout = getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                        if (arg_layout == .str) {
                            break :blk .str;
                        }
                    }
                    break :blk .i64;
                },
                else => {},
            }
            break :blk .i64;
        },
        .e_dot_access => |dot| blk: {
            // Get the receiver expression and check if it's a record
            const receiver_expr = module_env.store.getExpr(dot.receiver);
            switch (receiver_expr) {
                .e_record => |rec| {
                    // Find the field with the matching name
                    const fields = module_env.store.sliceRecordFields(rec.fields);
                    for (fields) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        if (@as(u32, @bitCast(field.name)) == @as(u32, @bitCast(dot.field_name))) {
                            const field_expr = module_env.store.getExpr(field.value);
                            break :blk getExprLayoutWithTypeEnv(allocator, module_env, field_expr, type_env);
                        }
                    }
                },
                else => {},
            }
            break :blk .i64;
        },
        else => .i64,
    };
}

/// Get the result type for a block
fn getBlockLayout(allocator: Allocator, module_env: *ModuleEnv, block: anytype, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    var type_annos = std.AutoHashMap(base.Ident.Idx, LayoutIdx).init(allocator);
    defer type_annos.deinit();

    const stmts = module_env.store.sliceStatements(block.stmts);

    // First pass: collect type annotations
    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_type_anno => |ta| {
                const type_anno = module_env.store.getTypeAnno(ta.anno);
                switch (type_anno) {
                    .apply => |apply| {
                        const result_layout = layoutFromLocalOrExternal(apply.base);
                        type_annos.put(ta.name, result_layout) catch {};
                    },
                    .lookup => |lookup| {
                        const result_layout = layoutFromLocalOrExternal(lookup.base);
                        type_annos.put(ta.name, result_layout) catch {};
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    // Second pass: process declarations
    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                const pattern_key = @intFromEnum(decl.pattern);
                if (decl.anno) |anno_idx| {
                    const result_layout = getAnnotationLayout(module_env, anno_idx);
                    type_env.put(pattern_key, result_layout) catch {};
                } else {
                    const pattern = module_env.store.getPattern(decl.pattern);
                    var found_annotation = false;
                    switch (pattern) {
                        .assign => |assign| {
                            if (type_annos.get(assign.ident)) |anno_layout| {
                                type_env.put(pattern_key, anno_layout) catch {};
                                found_annotation = true;
                            }
                        },
                        else => {},
                    }
                    if (!found_annotation) {
                        const decl_expr = module_env.store.getExpr(decl.expr);
                        const inferred_layout = getExprLayoutWithTypeEnv(allocator, module_env, decl_expr, type_env);
                        type_env.put(pattern_key, inferred_layout) catch {};
                    }
                }
            },
            else => {},
        }
    }

    const final_expr = module_env.store.getExpr(block.final_expr);
    return getExprLayoutWithTypeEnv(allocator, module_env, final_expr, type_env);
}

/// Get the result type from an annotation
fn getAnnotationLayout(module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) LayoutIdx {
    const anno = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(anno.anno);
    switch (type_anno) {
        .apply => |apply| return layoutFromLocalOrExternal(apply.base),
        .lookup => |lookup| return layoutFromLocalOrExternal(lookup.base),
        else => return .i64,
    }
}

/// Get the result type for a typed integer
fn getTypedIntLayout(module_env: *ModuleEnv, type_name: base.Ident.Idx) LayoutIdx {
    const idents = &module_env.idents;
    if (type_name == idents.u8_type or
        type_name == idents.u16_type or
        type_name == idents.u32_type or
        type_name == idents.u64_type)
    {
        return .u64;
    } else if (type_name == idents.u128_type) {
        return .u128;
    } else if (type_name == idents.i128_type) {
        return .i128;
    } else if (type_name == idents.f32_type) {
        return .f32;
    } else if (type_name == idents.f64_type) {
        return .f64;
    } else if (type_name == idents.dec_type) {
        return .dec;
    }
    return .i64;
}

/// Get the result type for a binary operation
fn getBinopLayout(allocator: Allocator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    switch (binop.op) {
        .lt, .gt, .le, .ge, .eq, .ne, .@"and", .@"or" => return .i64,
        else => {},
    }
    const lhs_expr = module_env.store.getExpr(binop.lhs);
    return getExprLayoutWithTypeEnv(allocator, module_env, lhs_expr, type_env);
}

/// Convert a LocalOrExternal to LayoutIdx
fn layoutFromLocalOrExternal(loe: CIR.TypeAnno.LocalOrExternal) LayoutIdx {
    switch (loe) {
        .builtin => |b| return layoutFromBuiltin(b),
        .local, .external => return .i64,
    }
}

/// Convert a Builtin type enum to LayoutIdx
fn layoutFromBuiltin(b: CIR.TypeAnno.Builtin) LayoutIdx {
    return switch (b) {
        .u8, .u16, .u32, .u64 => .u64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        else => .i64,
    };
}

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
