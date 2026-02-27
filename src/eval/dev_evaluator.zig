//! Dev Backend Evaluator
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to MIR (monomorphized intermediate representation)
//! 5. Lowering MIR to LIR (low-level IR with globally unique symbols)
//! 6. Reference counting insertion
//! 7. Generating native machine code (x86_64/aarch64)
//! 8. Executing the generated code
//!
//! Code generation uses LIR with globally unique Symbol references,
//! eliminating cross-module index collisions.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const types = @import("types");
const backend = @import("backend");
const mir = @import("mir");
const MIR = mir.MIR;
const lir = @import("lir");
const LirExprStore = lir.LirExprStore;
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins");
const i128h = builtins.compiler_rt_128;

// Cross-platform setjmp/longjmp for crash recovery.
const sljmp = @import("sljmp");
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

// Windows SEH (Structured Exception Handling) support for catching segfaults.
// On Windows, we use Vectored Exception Handling (VEH) to catch access violations
// and longjmp back to the caller, similar to how Unix uses fork() for isolation.
const WindowsSEH = if (builtin.os.tag == .windows) struct {
    const windows = std.os.windows;

    // EXCEPTION_RECORD structure for accessing exception info
    const EXCEPTION_RECORD = extern struct {
        ExceptionCode: u32,
        ExceptionFlags: u32,
        ExceptionRecord: ?*EXCEPTION_RECORD,
        ExceptionAddress: ?*anyopaque,
        NumberParameters: u32,
        ExceptionInformation: [15]usize,
    };

    // EXCEPTION_POINTERS structure passed to VEH handlers
    const EXCEPTION_POINTERS = extern struct {
        ExceptionRecord: *EXCEPTION_RECORD,
        ContextRecord: *anyopaque,
    };

    // Exception codes
    const EXCEPTION_ACCESS_VIOLATION: u32 = 0xC0000005;
    const EXCEPTION_STACK_OVERFLOW: u32 = 0xC00000FD;
    const EXCEPTION_ILLEGAL_INSTRUCTION: u32 = 0xC000001D;
    const EXCEPTION_PRIV_INSTRUCTION: u32 = 0xC0000096;
    const EXCEPTION_IN_PAGE_ERROR: u32 = 0xC0000006;

    // Return values for exception handlers
    const EXCEPTION_CONTINUE_SEARCH: c_long = 0;

    // Thread-local storage for the jump buffer - allows nested/concurrent calls
    threadlocal var current_jmp_buf: ?*JmpBuf = null;
    threadlocal var exception_code: u32 = 0;

    // VEH handler that catches access violations and longjmps back
    fn vehHandler(exception_info: *EXCEPTION_POINTERS) callconv(.winapi) c_long {
        const code = exception_info.ExceptionRecord.ExceptionCode;

        // Check if this is a crash we want to catch
        const is_crash = switch (code) {
            EXCEPTION_ACCESS_VIOLATION,
            EXCEPTION_STACK_OVERFLOW,
            EXCEPTION_ILLEGAL_INSTRUCTION,
            EXCEPTION_PRIV_INSTRUCTION,
            EXCEPTION_IN_PAGE_ERROR,
            => true,
            else => false,
        };

        if (is_crash) {
            if (current_jmp_buf) |jmp| {
                exception_code = code;
                // Clear the jump buffer before longjmp to prevent re-entry
                current_jmp_buf = null;
                // longjmp with value 2 to distinguish from roc_crashed (which uses 1)
                longjmp(jmp, 2);
            }
        }

        // Let other handlers process this exception
        return EXCEPTION_CONTINUE_SEARCH;
    }

    // External Windows API functions
    extern "kernel32" fn AddVectoredExceptionHandler(
        first: c_ulong,
        handler: *const fn (*EXCEPTION_POINTERS) callconv(.winapi) c_long,
    ) callconv(.winapi) ?*anyopaque;

    extern "kernel32" fn RemoveVectoredExceptionHandler(
        handle: *anyopaque,
    ) callconv(.winapi) c_ulong;

    /// Install the VEH handler and set the jump buffer for crash recovery
    pub fn install(jmp_buf: *JmpBuf) ?*anyopaque {
        current_jmp_buf = jmp_buf;
        exception_code = 0;
        // Add as first handler (1) to ensure we catch exceptions before others
        return AddVectoredExceptionHandler(1, vehHandler);
    }

    /// Remove the VEH handler
    pub fn remove(handle: ?*anyopaque) void {
        current_jmp_buf = null;
        if (handle) |h| {
            _ = RemoveVectoredExceptionHandler(h);
        }
    }

    /// Get the exception code that triggered the crash
    pub fn getExceptionCode() u32 {
        return exception_code;
    }

    /// Format exception code as a human-readable string
    pub fn formatException(code: u32) []const u8 {
        return switch (code) {
            EXCEPTION_ACCESS_VIOLATION => "EXCEPTION_ACCESS_VIOLATION (segfault)",
            EXCEPTION_STACK_OVERFLOW => "EXCEPTION_STACK_OVERFLOW",
            EXCEPTION_ILLEGAL_INSTRUCTION => "EXCEPTION_ILLEGAL_INSTRUCTION",
            EXCEPTION_PRIV_INSTRUCTION => "EXCEPTION_PRIV_INSTRUCTION",
            EXCEPTION_IN_PAGE_ERROR => "EXCEPTION_IN_PAGE_ERROR",
            else => "Unknown exception",
        };
    }
} else struct {
    // Stub for non-Windows platforms
    pub fn install(_: *JmpBuf) ?*anyopaque {
        return null;
    }
    pub fn remove(_: ?*anyopaque) void {}
    pub fn getExceptionCode() u32 {
        return 0;
    }
    pub fn formatException(_: u32) []const u8 {
        return "N/A";
    }
};

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

// Static data interner for string literals
const StaticDataInterner = backend.StaticDataInterner;
const MemoryBackend = StaticDataInterner.MemoryBackend;

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
        .call => |c| c.ret_layout,
        .low_level => |ll| ll.ret_layout,
        .early_return => |er| er.ret_layout,
        .lookup => |l| l.layout_idx,
        .struct_ => |s| s.struct_layout,
        .tag => |t| t.union_layout,
        .zero_arg_tag => |z| z.union_layout,
        .struct_access => |sa| sa.field_layout,
        .closure => |c| store.getClosureData(c).closure_layout,
        .nominal => |n| lirExprResultLayout(store, n.backing_expr) orelse n.nominal_layout,
        .discriminant_switch => |ds| ds.result_layout,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .bool,
        .dec_literal => .dec,
        .str_literal => .str,
        .i64_literal => .i64,
        // i128_literal is used for both I128 and U128 values (stored as @bitCast i128).
        // Don't hardcode .i128 here — let the fallback resolve from the CIR type variable
        // to get the correct layout (.i128 vs .u128).
        .i128_literal => null,
        .list,
        .empty_list,
        .lambda,
        .crash,
        .runtime_error,
        .hosted_call,
        .incref,
        .decref,
        .free,
        .for_loop,
        .while_loop,
        .tag_payload_access,
        .break_expr,
        => null,

        // String-producing operations always return Str layout
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        => .str,
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

    /// Per-thread static allocator state for alloc/realloc functions.
    /// WORKAROUND: Uses a static buffer instead of self.allocator.alignedAlloc.
    /// There's a mysterious crash when using allocator vtable calls from inside
    /// lambdas - it works the first time but crashes on subsequent calls from
    /// inside lambda execution contexts. The root cause appears to be related
    /// to vtable calls from C-calling-convention functions into Zig code.
    /// Using a static buffer completely avoids the vtable call.
    /// NOTE: threadlocal is required because snapshot tests run in parallel —
    /// without it, concurrent threads corrupt each other's allocations.
    const StaticAlloc = struct {
        threadlocal var buffer: [1024 * 1024]u8 align(16) = undefined; // 1MB static buffer
        threadlocal var offset: usize = 0;
        // Track allocation sizes for realloc (simple array of ptr -> size pairs)
        const max_allocs = 4096;
        threadlocal var alloc_ptrs: [max_allocs]usize = [_]usize{0} ** max_allocs;
        threadlocal var alloc_sizes: [max_allocs]usize = [_]usize{0} ** max_allocs;
        threadlocal var alloc_count: usize = 0;

        fn recordAlloc(ptr: usize, size: usize) void {
            if (alloc_count < max_allocs) {
                alloc_ptrs[alloc_count] = ptr;
                alloc_sizes[alloc_count] = size;
                alloc_count += 1;
            }
        }

        fn getAllocSize(ptr: usize) usize {
            // Search backwards since recent allocations are more likely to be reallocated
            var i: usize = alloc_count;
            while (i > 0) {
                i -= 1;
                if (alloc_ptrs[i] == ptr) {
                    return alloc_sizes[i];
                }
            }
            return 0;
        }

        fn reset() void {
            offset = 0;
            alloc_count = 0;
        }
    };

    /// Allocation function for RocOps.
    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        // Align the offset to the requested alignment
        const alignment = roc_alloc.alignment;
        const mask = alignment - 1;
        const aligned_offset = (StaticAlloc.offset + mask) & ~mask;

        if (aligned_offset + roc_alloc.length > StaticAlloc.buffer.len) {
            const self: *DevRocEnv = @ptrCast(@alignCast(env));
            self.crashed = true;
            if (self.crash_message) |old| self.allocator.free(old);
            self.crash_message = self.allocator.dupe(u8, "static buffer overflow in alloc") catch null;
            longjmp(&self.jmp_buf, 1);
        }

        const ptr: [*]u8 = @ptrCast(&StaticAlloc.buffer[aligned_offset]);
        StaticAlloc.offset = aligned_offset + roc_alloc.length;

        // Track this allocation for realloc
        StaticAlloc.recordAlloc(@intFromPtr(ptr), roc_alloc.length);

        roc_alloc.answer = @ptrCast(ptr);
    }

    /// Deallocation function for RocOps.
    /// Currently a no-op since we use a static buffer for allocations.
    fn rocDeallocFn(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
        // Static buffer doesn't support deallocation - this is a no-op
    }

    /// Reallocation function for RocOps.
    /// With static buffer, we allocate new space and copy data (old space is not reclaimed).
    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        // Align the offset to the requested alignment
        const alignment = roc_realloc.alignment;
        const mask = alignment - 1;
        const aligned_offset = (StaticAlloc.offset + mask) & ~mask;

        if (aligned_offset + roc_realloc.new_length > StaticAlloc.buffer.len) {
            const self: *DevRocEnv = @ptrCast(@alignCast(env));
            self.crashed = true;
            if (self.crash_message) |old| self.allocator.free(old);
            self.crash_message = self.allocator.dupe(u8, "static buffer overflow in realloc") catch null;
            longjmp(&self.jmp_buf, 1);
        }

        const new_ptr: [*]u8 = @ptrCast(&StaticAlloc.buffer[aligned_offset]);
        StaticAlloc.offset = aligned_offset + roc_realloc.new_length;

        // Track this new allocation
        StaticAlloc.recordAlloc(@intFromPtr(new_ptr), roc_realloc.new_length);

        // Copy old data to new location (only copy the old size, not new size)
        // Use @memmove because in a bump allocator the old and new regions
        // can be adjacent/overlapping when the old alloc was the most recent.
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        const old_size = StaticAlloc.getAllocSize(@intFromPtr(old_ptr));
        const copy_len = @min(old_size, roc_realloc.new_length);
        if (copy_len > 0) {
            @memmove(new_ptr[0..copy_len], old_ptr[0..copy_len]);
        }

        // Return the new pointer
        roc_realloc.answer = @ptrCast(new_ptr);
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
    ///
    /// This function provides two levels of protection:
    /// 1. roc_crashed calls (e.g., divide by zero) - caught via setjmp/longjmp, returns RocCrashed
    /// 2. Segfaults (access violations) - caught via Windows VEH on Windows, returns Segfault
    ///
    /// On Unix, segfault protection is handled at a higher level via fork() in the test harness.
    /// On Windows, we use Vectored Exception Handling (VEH) since fork() is not available.
    pub fn callWithCrashProtection(self: *DevEvaluator, executable: *const backend.ExecutableMemory, result_ptr: *anyopaque) error{ RocCrashed, Segfault }!void {
        self.roc_env.crashed = false;

        if (comptime builtin.mode == .Debug) {
            builtins.utils.DebugRefcountTracker.enable();
        }
        defer if (comptime builtin.mode == .Debug) {
            _ = builtins.utils.DebugRefcountTracker.reportLeaks();
            builtins.utils.DebugRefcountTracker.disable();
        };

        // On Windows, install the VEH handler to catch segfaults
        const veh_handle = WindowsSEH.install(&self.roc_env.jmp_buf);
        defer WindowsSEH.remove(veh_handle);

        const jmp_result = setjmp(&self.roc_env.jmp_buf);
        if (jmp_result != 0) {
            if (jmp_result == 2) {
                // Returned via longjmp from VEH handler (segfault)
                const code = WindowsSEH.getExceptionCode();
                std.debug.print("\nSegfault caught: {s} (code 0x{X:0>8})\n", .{ WindowsSEH.formatException(code), code });
                return error.Segfault;
            } else {
                // Returned via longjmp from rocCrashedFn (value 1)
                return error.RocCrashed;
            }
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

                const defs = module.store.sliceDefs(module.all_defs);

                // Track top-level patterns so closure analysis doesn't treat them as captures.
                for (defs) |def_idx| {
                    const def = module.store.getDef(def_idx);
                    transformer.markTopLevel(def.pattern) catch return error.OutOfMemory;
                }

                // Analyze all top-level defs with lambda-set tracking. We intentionally do
                // not mutate def expressions in this landing pass; this stage enforces
                // resolved lambda-set invariants before MIR/LIR lowering.
                for (defs) |def_idx| {
                    const def = module.store.getDef(def_idx);
                    const pattern = module.store.getPattern(def.pattern);
                    const name_hint: ?base.Ident.Idx = switch (pattern) {
                        .assign => |a| a.ident,
                        else => null,
                    };
                    const result = transformer.transformExprWithLambdaSet(def.expr, name_hint) catch return error.OutOfMemory;
                    if (result.lambda_set) |lambda_set| {
                        transformer.pattern_lambda_sets.put(def.pattern, lambda_set) catch return error.OutOfMemory;
                    }
                    if (transformer.lambda_return_sets.get(result.expr)) |return_set| {
                        const cloned = return_set.clone(self.allocator) catch return error.OutOfMemory;
                        transformer.pattern_lambda_return_sets.put(def.pattern, cloned) catch return error.OutOfMemory;
                    }
                }

                const validation_result = transformer.validateAllResolved();
                if (!validation_result.is_valid) {
                    if (validation_result.first_error) |err| {
                        std.log.err(
                            "Lambda-set validation failed for module '{s}': unresolved={d}, first={s}",
                            .{ module.module_name, validation_result.unresolved_count, @tagName(err.kind) },
                        );
                    } else {
                        std.log.err(
                            "Lambda-set validation failed for module '{s}': unresolved={d}",
                            .{ module.module_name, validation_result.unresolved_count },
                        );
                    }
                    return error.RuntimeError;
                }

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
        // Reset the static bump allocator so each evaluation starts fresh
        DevRocEnv.StaticAlloc.reset();

        // Find the module index for this module
        var module_idx: u32 = 0;
        for (all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                module_idx = @intCast(i);
                break;
            }
        }

        // Get or create the global layout store for resolving layouts of composite types
        // This is a single store shared across all modules for cross-module correctness
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);

        // In REPL sessions, module type stores get fresh type variables on each evaluation,
        // but the global layout store persists. Clear stale cached layouts.
        layout_store_ptr.resetModuleCache(all_module_envs);

        // Lower CIR to MIR
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
            return error.RuntimeError;
        };

        // Lower MIR to LIR
        var lir_store = LirExprStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, all_module_envs, module_env.idents.true_tag);
        defer mir_to_lir.deinit();

        const lir_expr_id = mir_to_lir.lower(mir_expr_id) catch {
            return error.RuntimeError;
        };

        // Run RC insertion pass on the LIR
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        const final_expr_id = rc_pass.insertRcOps(lir_expr_id) catch lir_expr_id;

        // Run RC insertion pass on all function definitions (symbol_defs)
        // so that lambda bodies get proper incref/decref annotations.
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

        // Determine the result layout from the LIR expression.
        // We prefer the layout embedded in the LIR expression because the CIR
        // type variable can have Content.err for expressions involving the `?`
        // operator at top level (where the Err branch desugars to runtime_error).
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = lirExprResultLayout(&lir_store, final_expr_id) orelse blk: {
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
        // Use HostLirCodeGen since we're executing on the host machine
        var codegen = backend.HostLirCodeGen.init(
            self.allocator,
            &lir_store,
            layout_store_ptr,
            &self.static_interner,
        ) catch return error.OutOfMemory;
        defer codegen.deinit();

        // Compile all procedures first (for recursive functions)
        // This ensures recursive closures are compiled as complete procedures
        // before we generate calls to them.
        const procs = lir_store.getProcs();
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch {
                return error.RuntimeError;
            };
        }

        // Generate code for the expression
        const gen_result = codegen.generateCode(final_expr_id, result_layout, tuple_len) catch {
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
                .f64_val => |v| {
                    var float_buf: [400]u8 = undefined;
                    try writer.writeAll(i128h.f64_to_str(&float_buf, v));
                },
                .i128_val => |v| {
                    var buf: [40]u8 = undefined;
                    try writer.writeAll(i128h.i128_to_str(&buf, v).str);
                },
                .u128_val => |v| {
                    var buf: [40]u8 = undefined;
                    try writer.writeAll(i128h.u128_to_str(&buf, v).str);
                },
                .str_val => |v| try writer.print("\"{s}\"", .{v}),
            }
        }
    };

    /// RocStr constants
    const RocStr = builtins.str.RocStr;
    const ROCSTR_SIZE: usize = @sizeOf(RocStr);
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
                    // Big string: first usize bytes are pointer to data, next usize bytes are length
                    const bytes_ptr: *const [*]const u8 = @ptrCast(@alignCast(&roc_str_bytes[0]));
                    const length_ptr: *const usize = @ptrCast(@alignCast(&roc_str_bytes[@sizeOf(usize)]));

                    const data_ptr = bytes_ptr.*;
                    const length = length_ptr.*;

                    // Handle the seamless slice bit (high bit of length indicates seamless slice)
                    const SEAMLESS_SLICE_BIT: usize = @as(usize, @bitCast(@as(isize, std.math.minInt(isize))));
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
