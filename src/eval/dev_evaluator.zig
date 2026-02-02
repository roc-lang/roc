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
const types = @import("types");
const backend = @import("backend");
const mono = @import("mono");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins");

// Cross-platform setjmp/longjmp for crash recovery.
const sljmp = @import("sljmp");
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

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
const MemoryBackend = StaticDataInterner.MemoryBackend;

/// Extract the result layout from a Mono IR expression for use as the overall
/// expression result layout. For blocks and other compound expressions where the
/// lowerer may have computed the layout from a CIR type variable with Content.err,
/// this follows through to the final/inner expression to find the true layout.
fn monoExprResultLayout(store: *const MonoExprStore, expr_id: mono.MonoIR.MonoExprId) ?layout.Idx {
    const MonoExpr = mono.MonoIR.MonoExpr;
    const expr: MonoExpr = store.getExpr(expr_id);
    return switch (expr) {
        // For blocks, the result_layout may be wrong if derived from a CIR type variable
        // with Content.err. Follow through to the final expression instead.
        .block => |b| monoExprResultLayout(store, b.final_expr),
        // Same for if/match — the result_layout may be wrong, but branch bodies
        // should have the correct layout from their own type variables.
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
        // Note: .list and .empty_list store element layout, not the overall list layout.
        // They are handled by the fromTypeVar fallback.
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

/// Environment for RocOps in the DevEvaluator.
/// Manages arena-backed allocation where free() is a no-op.
/// This enables proper RC tracking for in-place mutation optimization
/// while arenas handle actual memory deallocation.
const DevRocEnv = struct {
    allocator: Allocator,
    /// Track allocations to know their sizes for deallocation
    allocations: std.AutoHashMap(usize, AllocInfo),
    /// Set to true when roc_crashed is called during execution.
    crashed: bool = false,
    /// The crash message (duped from the callback argument).
    crash_message: ?[]const u8 = null,
    /// Jump buffer for unwinding from roc_crashed back to the call site.
    jmp_buf: JmpBuf = undefined,

    const AllocInfo = struct {
        len: usize,
        alignment: usize,
    };

    fn init(allocator: Allocator) DevRocEnv {
        return .{
            .allocator = allocator,
            .allocations = std.AutoHashMap(usize, AllocInfo).init(allocator),
        };
    }

    fn deinit(self: *DevRocEnv) void {
        // Free all tracked allocations before deiniting the map
        var iter = self.allocations.iterator();
        while (iter.next()) |entry| {
            const ptr_addr = entry.key_ptr.*;
            const alloc_info = entry.value_ptr.*;
            const slice_ptr: [*]u8 = @ptrFromInt(ptr_addr);

            switch (alloc_info.alignment) {
                1 => self.allocator.free(@as([*]align(1) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
                2 => self.allocator.free(@as([*]align(2) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
                4 => self.allocator.free(@as([*]align(4) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
                8 => self.allocator.free(@as([*]align(8) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
                16 => self.allocator.free(@as([*]align(16) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
                else => {},
            }
        }
        self.allocations.deinit();
        if (self.crash_message) |msg| self.allocator.free(msg);
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

        // Track the allocation so we can free it later
        self.allocations.put(@intFromPtr(ptr.ptr), .{
            .len = roc_alloc.length,
            .alignment = roc_alloc.alignment,
        }) catch {
            @panic("DevRocEnv: Failed to track allocation");
        };

        roc_alloc.answer = @ptrCast(ptr.ptr);
    }

    /// Deallocation function for RocOps.
    /// Frees memory allocated by rocAllocFn.
    fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));

        // Get the pointer from the dealloc request
        const ptr_addr = @intFromPtr(roc_dealloc.ptr);

        // Look up the allocation info
        const alloc_info = self.allocations.get(ptr_addr) orelse return;
        _ = self.allocations.remove(ptr_addr);

        // Create a slice from the pointer for freeing
        const slice_ptr: [*]u8 = @ptrCast(roc_dealloc.ptr);

        // Free with the appropriate alignment
        switch (alloc_info.alignment) {
            1 => self.allocator.free(@as([*]align(1) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
            2 => self.allocator.free(@as([*]align(2) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
            4 => self.allocator.free(@as([*]align(4) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
            8 => self.allocator.free(@as([*]align(8) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
            16 => self.allocator.free(@as([*]align(16) u8, @alignCast(slice_ptr))[0..alloc_info.len]),
            else => unreachable, // allocator must support freeing with the alignment it allocated
        }
    }

    /// Reallocation function for RocOps.
    /// Allocates new memory, copies data, frees old memory, and tracks the new allocation.
    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));

        const old_ptr_addr = @intFromPtr(roc_realloc.answer);

        // Look up the old allocation to get its size for the copy
        const old_alloc_info = self.allocations.get(old_ptr_addr);
        const old_length = if (old_alloc_info) |info| info.len else roc_realloc.new_length;
        const copy_length = @min(old_length, roc_realloc.new_length);

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
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        @memcpy(new_ptr[0..copy_length], old_ptr[0..copy_length]);

        // Free the old allocation if it was tracked
        if (old_alloc_info) |info| {
            _ = self.allocations.remove(old_ptr_addr);
            switch (info.alignment) {
                1 => self.allocator.free(@as([*]align(1) u8, @alignCast(old_ptr))[0..info.len]),
                2 => self.allocator.free(@as([*]align(2) u8, @alignCast(old_ptr))[0..info.len]),
                4 => self.allocator.free(@as([*]align(4) u8, @alignCast(old_ptr))[0..info.len]),
                8 => self.allocator.free(@as([*]align(8) u8, @alignCast(old_ptr))[0..info.len]),
                16 => self.allocator.free(@as([*]align(16) u8, @alignCast(old_ptr))[0..info.len]),
                else => {},
            }
        }

        // Track the new allocation
        self.allocations.put(@intFromPtr(new_ptr.ptr), .{
            .len = roc_realloc.new_length,
            .alignment = roc_realloc.alignment,
        }) catch {
            @panic("DevRocEnv: Failed to track reallocation");
        };

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

    /// Crash function — records the crash and longjmps back to the call site.
    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));
        self.crashed = true;
        const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
        if (self.crash_message) |old| self.allocator.free(old);
        self.crash_message = self.allocator.dupe(u8, msg) catch null;
        // Unwind the stack back to the setjmp call site.
        longjmp(&self.jmp_buf, 1);
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

    /// Backend for static data allocation (arena-based for in-memory compilation)
    /// Heap-allocated to ensure stable pointer for the interner's Backend reference
    memory_backend: *MemoryBackend,

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

    /// Global layout store shared across all modules.
    /// Created lazily on first code generation and reused for subsequent calls.
    /// This ensures layout indices are consistent across cross-module calls.
    global_layout_store: ?*layout.Store = null,

    /// Cached all_module_envs slice for layout store initialization.
    /// Set during generateCode and used by ensureGlobalLayoutStore.
    cached_module_envs: ?[]const *ModuleEnv = null,

    pub const Error = error{
        OutOfMemory,
        UnsupportedType,
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

        // Heap-allocate the memory backend so the pointer remains stable
        const memory_backend = allocator.create(MemoryBackend) catch return error.OutOfMemory;
        memory_backend.* = MemoryBackend.init(allocator);

        // Initialize the interner with a pointer to the heap-allocated backend
        const static_interner = StaticDataInterner.init(allocator, memory_backend.backend());

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
            .memory_backend = memory_backend,
            .static_interner = static_interner,
            .roc_env = roc_env,
            .roc_ops = roc_ops,
            .global_layout_store = null,
            .cached_module_envs = null,
        };
    }

    /// Get or create the global layout store.
    /// The global layout store uses all module type stores for cross-module layout computation.
    fn ensureGlobalLayoutStore(self: *DevEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        // If we already have a global layout store, return it
        if (self.global_layout_store) |ls| return ls;

        // Get builtin_str from module 0 (should be the builtin module)
        const builtin_str = if (all_module_envs.len > 0)
            all_module_envs[0].idents.builtin_str
        else
            null;

        // Create the global layout store
        const ls = self.allocator.create(layout.Store) catch return error.OutOfMemory;
        ls.* = layout.Store.init(all_module_envs, builtin_str, self.allocator, base.target.TargetUsize.native) catch {
            self.allocator.destroy(ls);
            return error.OutOfMemory;
        };

        self.global_layout_store = ls;
        self.cached_module_envs = all_module_envs;
        return ls;
    }

    /// Returns the crash message if roc_crashed was called during execution.
    pub fn getCrashMessage(self: *const DevEvaluator) ?[]const u8 {
        if (self.roc_env.crashed) return self.roc_env.crash_message orelse "roc_crashed called (no message)";
        return null;
    }

    /// Execute compiled code with crash protection.
    /// Uses setjmp/longjmp: if roc_crashed is called during execution,
    /// the stack unwinds back here and this returns error.RocCrashed.
    pub fn callWithCrashProtection(self: *DevEvaluator, executable: *const backend.ExecutableMemory, result_ptr: *anyopaque) error{RocCrashed}!void {
        self.roc_env.crashed = false;
        if (setjmp(&self.roc_env.jmp_buf) != 0) {
            // Returned via longjmp from rocCrashedFn.
            return error.RocCrashed;
        }
        executable.callWithResultPtrAndRocOps(result_ptr, @constCast(&self.roc_ops));
    }

    /// Clean up resources
    pub fn deinit(self: *DevEvaluator) void {
        // Clean up the global layout store if it exists
        if (self.global_layout_store) |ls| {
            ls.deinit();
            self.allocator.destroy(ls);
        }
        self.static_interner.deinit();
        self.memory_backend.deinit();
        self.allocator.destroy(self.memory_backend);
        self.roc_env.deinit();
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
        /// Reference to the global layout store (owned by DevEvaluator, not this struct)
        layout_store: ?*layout.Store = null,
        tuple_len: usize = 1,
        crash_message: ?[]const u8 = null,
        /// Offset from start of code where execution should begin
        /// (procedures may be compiled before the main expression)
        entry_offset: usize = 0,

        pub fn deinit(self: *CodeResult) void {
            if (self.code.len > 0) {
                self.allocator.free(self.code);
            }
            // Note: layout_store is owned by DevEvaluator, not cleaned up here
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

        // Get or create the global layout store for resolving layouts of composite types
        // This is a single store shared across all modules for cross-module correctness
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);

        // Create the lowerer with the layout store
        var lowerer = MonoLower.init(self.allocator, &mono_store, all_module_envs, null, layout_store_ptr);
        defer lowerer.deinit();

        // Lower CIR expression to Mono IR
        const lowered_expr_id = lowerer.lowerExpr(module_idx, expr_idx) catch {
            return error.RuntimeError;
        };

        // Run RC insertion pass on the Mono IR
        var rc_pass = mono.RcInsert.RcInsertPass.init(self.allocator, &mono_store, layout_store_ptr);
        defer rc_pass.deinit();
        const mono_expr_id = rc_pass.insertRcOps(lowered_expr_id) catch lowered_expr_id;

        // Determine the result layout from the Mono IR expression.
        // We prefer the layout embedded in the Mono expression because the CIR
        // type variable can have Content.err for expressions involving the `?`
        // operator at top level (where the Err branch desugars to runtime_error).
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = monoExprResultLayout(&mono_store, mono_expr_id) orelse blk: {
            // Fallback: resolve from the CIR type variable
            const type_var = can.ModuleEnv.varFrom(expr_idx);
            var type_scope = types.TypeScope.init(self.allocator);
            defer type_scope.deinit();
            break :blk layout_store_ptr.fromTypeVar(module_idx, type_var, &type_scope, null) catch {
                return error.RuntimeError;
            };
        };

        // Detect tuple expressions to set tuple_len
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        // Create the code generator with the layout store
        var codegen = backend.MonoExprCodeGen.init(
            self.allocator,
            &mono_store,
            layout_store_ptr,
            &self.static_interner,
        );
        defer codegen.deinit();

        // Compile all procedures first (for recursive functions)
        // This ensures recursive closures are compiled as complete procedures
        // before we generate calls to them.
        const procs = mono_store.getProcs();
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch {
                return error.RuntimeError;
            };
        }

        // Generate code for the expression
        const gen_result = codegen.generateCode(mono_expr_id, result_layout, tuple_len) catch {
            return error.RuntimeError;
        };

        return CodeResult{
            .code = gen_result.code,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .layout_store = layout_store_ptr,
            .tuple_len = tuple_len,
            .entry_offset = gen_result.entry_offset,
        };
    }

    /// Generate native code from source code string (full pipeline)
    ///
    /// NOTE: Native code generation is not currently implemented.
    /// This function exists to maintain the API but always returns an error.
    pub fn generateCodeFromSource(_: *DevEvaluator, _: []const u8) Error!CodeResult {
        return error.RuntimeError;
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
            return error.RuntimeError;
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
