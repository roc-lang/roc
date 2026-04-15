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
const RocIo = @import("io").RocIo;
const can = @import("can");
const types = @import("types");
const layout = @import("layout");
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

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

/// Build a TypeScope mapping platform for-clause aliases to app concrete types.
/// Returns null if the module has no for-clause aliases (non-platform modules or
/// platforms without type parameters like `model`).
fn buildPlatformTypeScope(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    app_module_env: *ModuleEnv,
) ?types.TypeScope {
    const all_aliases = module_env.for_clause_aliases.items.items;
    if (all_aliases.len == 0) return null;

    var type_scope = types.TypeScope.init(allocator);
    type_scope.scopes.append(types.VarMap.init(allocator)) catch {
        type_scope.deinit();
        return null;
    };
    const rigid_scope = &type_scope.scopes.items[0];

    for (module_env.requires_types.items.items) |required_type| {
        const type_aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
        for (type_aliases_slice) |alias| {
            const alias_stmt = module_env.store.getStatement(alias.alias_stmt_idx);
            std.debug.assert(alias_stmt == .s_alias_decl);
            const alias_body_var = can.ModuleEnv.varFrom(alias_stmt.s_alias_decl.anno);
            const alias_stmt_var = can.ModuleEnv.varFrom(alias.alias_stmt_idx);
            // Cross-module ident lookup: translate platform alias name to app ident store
            // via insertIdent (get-or-create) since ident indices are module-local.
            const alias_name_str = module_env.getIdent(alias.alias_name);
            const app_alias_name = app_module_env.common.insertIdent(allocator, base.Ident.for_text(alias_name_str)) catch continue;
            const app_var = findTypeAliasBodyVar(app_module_env, app_alias_name) orelse continue;
            rigid_scope.put(alias_body_var, app_var) catch continue;
            rigid_scope.put(alias_stmt_var, app_var) catch continue;
        }
    }

    return type_scope;
}

fn findTypeAliasBodyVar(module_env: *const ModuleEnv, name: base.Ident.Idx) ?types.Var {
    const stmts_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (stmts_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_alias_decl => |alias_decl| {
                const header = module_env.store.getTypeHeader(alias_decl.header);
                if (header.relative_name.eql(name)) {
                    return can.ModuleEnv.varFrom(alias_decl.anno);
                }
            },
            else => {},
        }
    }
    return null;
}

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
    /// Set to true when roc_expect_failed is called during execution.
    /// Inline expect failures are non-fatal, but the host uses this flag
    /// to report a non-zero exit status after the program finishes.
    inline_expect_failed: bool = false,
    /// The crash message (duped from the callback argument).
    crash_message: ?[]const u8 = null,
    /// Jump buffer for unwinding from roc_crashed back to the call site.
    jmp_buf: JmpBuf = undefined,
    /// Io context for routing [dbg] output
    roc_io: RocIo = RocIo.default(std.Io.Threaded.global_single_threaded.io()),

    const AllocInfo = struct {
        len: usize,
        alignment: usize,
    };

    fn init(allocator: Allocator, roc_io: ?RocIo) DevRocEnv {
        return .{
            .allocator = allocator,
            .allocations = std.AutoHashMap(usize, AllocInfo).init(allocator),
            .roc_io = roc_io orelse RocIo.default(std.Io.Threaded.global_single_threaded.io()),
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
    fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));
        const msg = roc_dbg.utf8_bytes[0..roc_dbg.len];
        var buf: [256]u8 = undefined;
        const line = std.fmt.bufPrint(&buf, "[dbg] {s}\n", .{msg}) catch "[dbg] (message too long)\n";
        self.roc_io.writeStderr(line) catch {};
    }

    /// Expect failed function.
    fn rocExpectFailedFn(_: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
        const self: *DevRocEnv = @ptrCast(@alignCast(env));
        self.inline_expect_failed = true;
        self.roc_io.writeStderr("[expect failed]\n") catch {};
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

    /// Shared type-side resolver layered on top of the global layout store.
    global_type_layout_resolver: ?*layout.TypeLayoutResolver = null,

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
        ModuleEnvNotFound,
    };

    /// Initialize the evaluator with builtin modules
    pub fn init(allocator: Allocator, roc_io: ?RocIo) Error!DevEvaluator {
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
        roc_env.* = DevRocEnv.init(allocator, roc_io);

        // Create RocOps with function pointers to the DevRocEnv handlers
        // Use a static dummy array for hosted_fns since count=0 means no hosted functions
        // This avoids undefined behavior from using `undefined` for the pointer
        const empty_hosted_fns = struct {
            fn dummyHostedFn(_: *RocOps, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
            var empty: [1]builtins.host_abi.HostedFn = .{builtins.host_abi.hostedFn(&dummyHostedFn)};
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
            .global_type_layout_resolver = null,
            .cached_module_envs = null,
        };
    }

    /// Get or create the global layout store.
    /// The global layout store uses all module type stores for cross-module layout computation.
    pub fn ensureGlobalLayoutStore(self: *DevEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        // If we already have a global layout store, return it
        if (self.global_layout_store) |ls| return ls;

        var builtin_str: ?base.Ident.Idx = null;
        for (all_module_envs) |env| {
            if (isBuiltinModuleEnv(env)) {
                builtin_str = env.idents.builtin_str;
                break;
            }
        }

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

    fn ensureGlobalTypeLayoutResolver(self: *DevEvaluator, all_module_envs: []const *ModuleEnv) Error!*layout.TypeLayoutResolver {
        if (self.global_type_layout_resolver) |resolver| return resolver;

        const layout_store = try self.ensureGlobalLayoutStore(all_module_envs);
        const resolver = self.allocator.create(layout.TypeLayoutResolver) catch return error.OutOfMemory;
        resolver.* = layout.TypeLayoutResolver.init(layout_store);
        self.global_type_layout_resolver = resolver;
        return resolver;
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

    /// Execute compiled code with crash protection using the RocCall ABI.
    ///
    /// Like callWithCrashProtection, but for entrypoint functions that take
    /// arguments via the RocCall ABI: fn(roc_ops, ret_ptr, args_ptr).
    /// Uses the DevEvaluator's own RocOps (with setjmp/longjmp crash handling)
    /// so that roc_crashed returns an error instead of exiting the process.
    ///
    /// Callers should set `self.roc_ops.hosted_fns` before calling if the
    /// entrypoint needs hosted functions.
    pub fn callRocABIWithCrashProtection(self: *DevEvaluator, executable: *const backend.ExecutableMemory, result_ptr: *anyopaque, args_ptr: ?*anyopaque) error{ RocCrashed, Segfault }!void {
        self.roc_env.crashed = false;

        const veh_handle = WindowsSEH.install(&self.roc_env.jmp_buf);
        defer WindowsSEH.remove(veh_handle);

        const jmp_result = setjmp(&self.roc_env.jmp_buf);
        if (jmp_result != 0) {
            if (jmp_result == 2) {
                const code = WindowsSEH.getExceptionCode();
                std.debug.print("\nSegfault caught: {s} (code 0x{X:0>8})\n", .{ WindowsSEH.formatException(code), code });
                return error.Segfault;
            } else {
                return error.RocCrashed;
            }
        }
        executable.callRocABI(@ptrCast(@constCast(&self.roc_ops)), result_ptr, args_ptr);
    }

    /// Clean up resources
    pub fn deinit(self: *DevEvaluator) void {
        if (self.global_type_layout_resolver) |resolver| {
            resolver.deinit();
            self.allocator.destroy(resolver);
        }
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

    /// Prepare modules for code generation.
    ///
    /// Lambda lifting and lambda set inference will be handled during
    /// CIR→MIR and MIR→LIR lowering respectively.
    pub fn prepareModulesForCodegen(
        _: *DevEvaluator,
        _: []*ModuleEnv,
    ) Error!void {}

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
    /// `all_module_envs` must use the same module ordering as `resolveImports`
    /// for `module_env` so external lookup indices line up with MIR lowering.
    pub fn generateCode(
        self: *DevEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
    ) Error!CodeResult {
        if (comptime backend.HostLirCodeGen == void) return error.RuntimeError;

        // Reset the static bump allocator so each evaluation starts fresh
        DevRocEnv.StaticAlloc.reset();

        // MIR lowering may need to translate structural identifiers between
        // modules (e.g. record fields in cross-module specializations). Cached
        // modules deserialize with read-only interners, so enable runtime
        // inserts up front for all participating modules.
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }

        // Other evaluators may have resolved imports against a different module
        // ordering. Refresh all modules here so CIR external lookups line up
        // with the slice we are about to hand to MIR lowering. Monomorphize
        // follows cross-module calls, so every module's resolved indices must
        // be consistent with all_module_envs.
        for (all_module_envs) |env| {
            env.imports.resolveImports(env, all_module_envs);
        }

        // Find the module index for this module
        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const app_module_idx = if (app_module_env) |env|
            findModuleEnvIdx(all_module_envs, env) orelse return error.ModuleEnvNotFound
        else
            null;

        // Get or create the global layout store for resolving layouts of composite types
        // This is a single store shared across all modules for cross-module correctness
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);
        layout_store_ptr.setModuleEnvs(all_module_envs);
        const type_layout_resolver_ptr = try self.ensureGlobalTypeLayoutResolver(all_module_envs);

        // In REPL sessions, module type stores get fresh type variables on each evaluation,
        // but the shared type-layout resolver persists. Clear stale type-side caches.
        type_layout_resolver_ptr.resetModuleCache(all_module_envs);

        // Build platform type scope for cross-module type resolution (e.g., Model → { value: I64 })
        var platform_type_scope = if (app_module_env) |app_env|
            buildPlatformTypeScope(self.allocator, module_env, app_env)
        else
            null;
        defer if (platform_type_scope) |*ts| ts.deinit();

        // Lower CIR to MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        var monomorphization = if (platform_type_scope) |*ts|
            mir.Monomorphize.runExprWithTypeScope(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
                module_idx,
                ts,
                app_module_idx.?,
            ) catch return error.OutOfMemory
        else
            mir.Monomorphize.runExpr(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
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
            app_module_idx,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        if (platform_type_scope) |*ts| {
            mir_lower.setTypeScope(module_idx, ts, app_module_idx.?) catch return error.OutOfMemory;
        }

        const mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.RuntimeError;
        };

        // Run lambda set inference
        const mir_mod = @import("mir");
        var lambda_set_store = mir_mod.LambdaSet.infer(self.allocator, &mir_store, all_module_envs) catch return error.OutOfMemory;
        defer lambda_set_store.deinit(self.allocator);

        // Lower MIR to LIR
        var lir_store = LirExprStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &lambda_set_store, module_env.idents.true_tag);
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
        lir.RcInsert.insertRcOpsIntoSymbolDefsBestEffort(self.allocator, &lir_store, layout_store_ptr);

        // Determine the result layout from the lowered LIR expression.
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = lirExprResultLayout(&lir_store, final_expr_id);

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
        const procs = lir_store.getProcSpecs();
        if (procs.len > 0) {
            codegen.compileAllProcSpecs(procs) catch {
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

    /// Generate code for an entrypoint using the RocCall ABI: fn(roc_ops, ret_ptr, args_ptr).
    ///
    /// Uses `generateEntrypointWrapper` which handles argument unpacking from args_ptr
    /// and invokes a synthetic entrypoint proc using the RocCall ABI.
    /// This is for `roc run` where the host passes its own RocOps.
    pub fn generateEntrypointCode(
        self: *DevEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
    ) Error!CodeResult {
        if (comptime backend.HostLirCodeGen == void) return error.RuntimeError;

        // Reset the static bump allocator so each evaluation starts fresh
        DevRocEnv.StaticAlloc.reset();

        // Enable runtime inserts for all participating modules
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }

        // Refresh imports for all modules so cross-module lookups in
        // Monomorphize use indices consistent with all_module_envs.
        for (all_module_envs) |env| {
            env.imports.resolveImports(env, all_module_envs);
        }

        // Find the module index for this module
        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const app_module_idx = if (app_module_env) |env|
            findModuleEnvIdx(all_module_envs, env) orelse return error.ModuleEnvNotFound
        else
            null;

        // Get or create the global layout store for resolving layouts of composite types
        // This is a single store shared across all modules for cross-module correctness
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);
        layout_store_ptr.setModuleEnvs(all_module_envs);
        const type_layout_resolver_ptr = try self.ensureGlobalTypeLayoutResolver(all_module_envs);

        // In REPL sessions, module type stores get fresh type variables on each evaluation,
        // but the shared type-layout resolver persists. Clear stale type-side caches.
        type_layout_resolver_ptr.resetModuleCache(all_module_envs);

        // Build platform type scope for cross-module type resolution (e.g., Model → { value: I64 })
        var platform_type_scope = if (app_module_env) |app_env|
            buildPlatformTypeScope(self.allocator, module_env, app_env)
        else
            null;
        defer if (platform_type_scope) |*ts| ts.deinit();

        // Lower CIR → MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        var monomorphization = if (platform_type_scope) |*ts|
            mir.Monomorphize.runExprWithTypeScope(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
                module_idx,
                ts,
                app_module_idx.?,
            ) catch return error.OutOfMemory
        else
            mir.Monomorphize.runExpr(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
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
            app_module_idx,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        if (platform_type_scope) |*ts| {
            mir_lower.setTypeScope(module_idx, ts, app_module_idx.?) catch return error.OutOfMemory;
        }

        const mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.RuntimeError;
        };

        // Run lambda set inference
        const mir_mod = @import("mir");
        var lambda_set_store = mir_mod.LambdaSet.infer(self.allocator, &mir_store, all_module_envs) catch return error.OutOfMemory;
        defer lambda_set_store.deinit(self.allocator);

        // Lower MIR to LIR
        var lir_store = LirExprStore.init(self.allocator);
        defer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &lambda_set_store, module_env.idents.true_tag);
        defer mir_to_lir.deinit();

        const entry_proc = mir_to_lir.lowerEntrypointProc(mir_expr_id, arg_layouts, ret_layout) catch {
            return error.RuntimeError;
        };

        lir.RcInsert.insertRcOpsIntoSymbolDefsBestEffort(self.allocator, &lir_store, layout_store_ptr);

        // Create codegen
        var codegen = backend.HostLirCodeGen.init(
            self.allocator,
            &lir_store,
            layout_store_ptr,
            &self.static_interner,
        ) catch return error.OutOfMemory;
        defer codegen.deinit();

        // Compile all procedures first
        const procs = lir_store.getProcSpecs();
        if (procs.len > 0) {
            codegen.compileAllProcSpecs(procs) catch {
                return error.RuntimeError;
            };
        }

        // Generate entrypoint wrapper using RocCall ABI
        const exported = codegen.generateEntrypointWrapper("", entry_proc, arg_layouts, ret_layout) catch {
            return error.RuntimeError;
        };

        // Patch cross-proc call sites
        codegen.patchPendingCalls() catch {
            return error.RuntimeError;
        };

        // Get the generated code
        const all_code = codegen.getGeneratedCode();
        const code_copy = self.allocator.dupe(u8, all_code) catch return error.OutOfMemory;

        return CodeResult{
            .code = code_copy,
            .allocator = self.allocator,
            .result_layout = ret_layout,
            .layout_store = layout_store_ptr,
            .tuple_len = 1,
            .entry_offset = exported.offset,
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
            .u64, .u8, .u16, .u32 => blk: {
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
            else => blk: {
                const layout_store = code_result.layout_store orelse return error.UnsupportedType;
                const result_layout = layout_store.getLayout(code_result.result_layout);
                if (result_layout.tag == .tag_union) {
                    const tu_data = layout_store.getTagUnionData(result_layout.getTagUnion().idx);
                    if (tu_data.discriminant_offset == 0 and tu_data.size <= @sizeOf(u64)) {
                        var result: u64 = 0;
                        executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&self.roc_ops));
                        break :blk EvalResult{ .u64_val = result };
                    }
                }
                return error.UnsupportedType;
            },
        };
    }
};

// Tests

test "dev evaluator initialization" {
    var runner = DevEvaluator.init(std.testing.allocator, null) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();
}
