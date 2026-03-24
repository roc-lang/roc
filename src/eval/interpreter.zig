//! Expression Interpreter
//!
//! Evaluates post-RC LIR expressions directly, producing concrete runtime values.
//! Consumes the same lowered IR used by the dev and wasm code generators.
//!
//! Design principles:
//! - Stack-safe iterative evaluation via WorkStack + ValueStack
//! - Values are raw (pointer, layout) pairs — no runtime type variables
//! - RC ops (incref/decref/free) are executed literally from LIR
//! - Symbol-based environment with flat ArrayList bindings
//! - Follow the LIR control flow exactly

const std = @import("std");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const lir_value = @import("value.zig");
const lir_program_mod = @import("cir_to_lir.zig");
const builtins = @import("builtins");
const sljmp = @import("sljmp");
const Io = @import("io").Io;
const work_stack = @import("work_stack.zig");
const FlatBinding = work_stack.FlatBinding;
const build_options = @import("build_options");

/// Comptime-gated tracing for the interpreter eval loop.
/// Enabled via `-Dtrace-eval=true`. Zero cost when disabled.
const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            std.debug.print("[interp] " ++ fmt ++ "\n", args);
        }
    }
};

/// Comptime-gated tracing for refcount operations.
/// Enabled via `-Dtrace-refcount=true`. Zero cost when disabled.
const trace_rc = struct {
    const enabled = if (@hasDecl(build_options, "trace_refcount")) build_options.trace_refcount else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            std.debug.print("[rc] " ++ fmt ++ "\n", args);
        }
    }
};

const Allocator = std.mem.Allocator;
const LirExprStore = lir.LirExprStore;
const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const LirProcSpecId = lir.LirProcSpecId;
const LirProcSpec = lir.LirProcSpec;
const CFStmtId = lir.CFStmtId;
const Symbol = lir.Symbol;
const Layout = layout_mod.Layout;
const Value = lir_value.Value;
const LayoutHelper = lir_value.LayoutHelper;
const RocDec = builtins.dec.RocDec;
const dev_wrappers = builtins.dev_wrappers;
const i128h = builtins.compiler_rt_128;

// Builtin types for direct dispatch
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const UpdateMode = builtins.utils.UpdateMode;
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

/// Environment for RocOps in the interpreter.
/// Uses a thread-local static buffer for allocation (same pattern as DevRocEnv)
/// to avoid Zig allocator vtable issues from C-calling-convention callbacks.
const InterpreterRocEnv = struct {
    allocator: Allocator,
    io: Io,
    crashed: bool = false,
    crash_message: ?[]const u8 = null,
    runtime_error_message: ?[]const u8 = null,
    expect_message: ?[]const u8 = null,
    jmp_buf: JmpBuf = undefined,
    forwarded_memory_env: *anyopaque = undefined,
    forwarded_roc_alloc: ?*const fn (*RocAlloc, *anyopaque) callconv(.c) void = null,
    forwarded_roc_dealloc: ?*const fn (*RocDealloc, *anyopaque) callconv(.c) void = null,
    forwarded_roc_realloc: ?*const fn (*RocRealloc, *anyopaque) callconv(.c) void = null,

    /// Thread-local static buffer for allocations from builtins.
    const StaticAlloc = struct {
        threadlocal var buffer: [1024 * 1024]u8 align(16) = undefined;
        threadlocal var offset: usize = 0;
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
            var i: usize = alloc_count;
            while (i > 0) {
                i -= 1;
                if (alloc_ptrs[i] == ptr) return alloc_sizes[i];
            }
            return 0;
        }

        fn reset() void {
            offset = 0;
            alloc_count = 0;
        }
    };

    fn init(allocator: Allocator, io: Io) InterpreterRocEnv {
        return .{ .allocator = allocator, .io = io };
    }

    fn deinit(self: *InterpreterRocEnv) void {
        if (self.crash_message) |msg| self.allocator.free(msg);
        if (self.expect_message) |msg| self.allocator.free(msg);
    }

    /// Reset the static buffer — call once at the start of a full evaluation.
    fn resetForEval(self: *InterpreterRocEnv) void {
        self.crashed = false;
        if (self.crash_message) |msg| self.allocator.free(msg);
        self.crash_message = null;
        self.runtime_error_message = null;
        if (self.expect_message) |msg| self.allocator.free(msg);
        self.expect_message = null;
        StaticAlloc.reset();
    }

    /// Reset just the crash state before calling a builtin that might crash.
    fn resetCrash(self: *InterpreterRocEnv) void {
        self.crashed = false;
    }

    fn forwardMemoryOpsFrom(self: *InterpreterRocEnv, caller_roc_ops: *RocOps) void {
        self.forwarded_memory_env = caller_roc_ops.env;
        self.forwarded_roc_alloc = caller_roc_ops.roc_alloc;
        self.forwarded_roc_dealloc = caller_roc_ops.roc_dealloc;
        self.forwarded_roc_realloc = caller_roc_ops.roc_realloc;
    }

    fn resetForwardedMemoryOps(self: *InterpreterRocEnv) void {
        self.forwarded_roc_alloc = null;
        self.forwarded_roc_dealloc = null;
        self.forwarded_roc_realloc = null;
    }

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        if (self.forwarded_roc_alloc) |forwarded_roc_alloc| {
            forwarded_roc_alloc(roc_alloc, self.forwarded_memory_env);
            trace_rc.log("alloc(fwd): ptr=0x{x} size={d} align={d}", .{ @intFromPtr(roc_alloc.answer), roc_alloc.length, roc_alloc.alignment });
            return;
        }

        const alignment = roc_alloc.alignment;
        const mask = alignment - 1;
        const aligned_offset = (StaticAlloc.offset + mask) & ~mask;
        if (aligned_offset + roc_alloc.length > StaticAlloc.buffer.len) {
            self.crashed = true;
            if (self.crash_message) |old| self.allocator.free(old);
            self.crash_message = self.allocator.dupe(u8, "static buffer overflow in alloc") catch null;
            longjmp(&self.jmp_buf, 1);
        }
        const ptr: [*]u8 = @ptrCast(&StaticAlloc.buffer[aligned_offset]);
        StaticAlloc.offset = aligned_offset + roc_alloc.length;
        StaticAlloc.recordAlloc(@intFromPtr(ptr), roc_alloc.length);
        roc_alloc.answer = @ptrCast(ptr);
        trace_rc.log("alloc: ptr=0x{x} size={d} align={d} buf_offset={d}", .{ @intFromPtr(ptr), roc_alloc.length, alignment, StaticAlloc.offset });
    }

    fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        trace_rc.log("dealloc: ptr=0x{x} align={d}", .{ @intFromPtr(roc_dealloc.ptr), roc_dealloc.alignment });
        if (self.forwarded_roc_dealloc) |forwarded_roc_dealloc| {
            forwarded_roc_dealloc(roc_dealloc, self.forwarded_memory_env);
        }
    }

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        if (self.forwarded_roc_realloc) |forwarded_roc_realloc| {
            forwarded_roc_realloc(roc_realloc, self.forwarded_memory_env);
            trace_rc.log("realloc(fwd): old=0x{x} new=0x{x} size={d}", .{ @intFromPtr(roc_realloc.answer), @intFromPtr(roc_realloc.answer), roc_realloc.new_length });
            return;
        }

        const alignment = roc_realloc.alignment;
        const mask = alignment - 1;
        const aligned_offset = (StaticAlloc.offset + mask) & ~mask;
        if (aligned_offset + roc_realloc.new_length > StaticAlloc.buffer.len) {
            self.crashed = true;
            if (self.crash_message) |old| self.allocator.free(old);
            self.crash_message = self.allocator.dupe(u8, "static buffer overflow in realloc") catch null;
            longjmp(&self.jmp_buf, 1);
        }
        const new_ptr: [*]u8 = @ptrCast(&StaticAlloc.buffer[aligned_offset]);
        StaticAlloc.offset = aligned_offset + roc_realloc.new_length;
        StaticAlloc.recordAlloc(@intFromPtr(new_ptr), roc_realloc.new_length);
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        const old_size = StaticAlloc.getAllocSize(@intFromPtr(old_ptr));
        const copy_len = @min(old_size, roc_realloc.new_length);
        if (copy_len > 0) {
            @memmove(new_ptr[0..copy_len], old_ptr[0..copy_len]);
        }
        roc_realloc.answer = @ptrCast(new_ptr);
        trace_rc.log("realloc: old=0x{x} new=0x{x} old_size={d} new_size={d} align={d}", .{ @intFromPtr(old_ptr), @intFromPtr(new_ptr), old_size, roc_realloc.new_length, alignment });
    }

    fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const msg = roc_dbg.utf8_bytes[0..roc_dbg.len];
        var buf: [256]u8 = undefined;
        const line = std.fmt.bufPrint(&buf, "[dbg] {s}\n", .{msg}) catch "[dbg] (message too long)\n";
        self.io.writeStderr(line) catch {};
    }

    fn rocExpectFailedFn(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const source = expect_args.utf8_bytes[0..expect_args.len];
        if (self.expect_message == null) {
            self.expect_message = self.allocator.dupe(u8, source) catch null;
        }
    }

    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        self.crashed = true;
        const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
        if (self.crash_message) |old| self.allocator.free(old);
        self.crash_message = self.allocator.dupe(u8, msg) catch null;
        longjmp(&self.jmp_buf, 1);
    }
};

/// Interprets LIR expressions by walking the expression tree and evaluating directly.
pub const LirInterpreter = struct {
    const max_call_depth: usize = 1024;
    const stack_overflow_message =
        "This Roc program overflowed its stack memory. This usually means there is very deep or infinite recursion somewhere in the code.";
    const infinite_while_loop_message =
        "This while loop's condition evaluated to True at compile time, " ++
        "and the loop body has no break or return statement, " ++
        "which would cause an infinite loop. " ++
        "Use a mutable variable for the condition, or add a break/return.";
    const division_by_zero_message = "Division by zero";

    allocator: Allocator,
    store: *const LirExprStore,
    layout_store: *const layout_mod.Store,
    helper: LayoutHelper,

    /// Symbol → (value pointer, size) bindings.
    /// Flat list scanned from end; save-length/trim replaces HashMap clone on calls.
    bindings: FlatBindingList,

    /// Mutable cells: symbol → pointer to current value.
    cells: std.AutoHashMap(u64, Binding),

    /// Top-level def cache: symbol → evaluated value.
    top_level_cache: std.AutoHashMap(u64, Binding),

    /// Set of symbols currently being evaluated (cycle detection).
    evaluating: std.AutoHashMap(u64, void),

    /// Arena for interpreter-allocated memory (temporaries, copies).
    arena: std.heap.ArenaAllocator,

    /// RocOps environment for builtin dispatch.
    roc_env: *InterpreterRocEnv,
    roc_ops: RocOps,

    /// Guard to reset the static buffer only once per top-level eval.
    eval_active: bool = false,

    /// When executing an entrypoint in `roc run --allow-errors`, tolerate
    /// compile-placeholder runtime_error nodes by materializing zero/default values
    /// instead of aborting the whole program immediately.
    recover_runtime_placeholders: bool = false,

    /// Bound recursive function-call depth so the interpreter reports a Roc crash
    /// instead of overflowing the native stack.
    call_depth: usize = 0,

    /// Comptime evaluation enables this to reject statically-infinite while loops.
    detect_infinite_while_loops: bool = false,

    /// Current lambda params — used by evalHostedCall to collect implicit args
    /// when the hosted_call has 0 explicit args (same pattern as dev backend).
    current_lambda_params: ?lir.LirPatternSpan = null,

    /// When running via evalEntrypoint, points to the platform's RocOps.
    /// Hosted functions must receive this (not the interpreter's own RocOps)
    /// because they cast ops.env to the platform's HostEnv type.
    caller_roc_ops: ?*RocOps = null,

    /// Join point registry for tail-recursive CF statement evaluation.
    join_points: JoinPointMap = .{},

    // ── Stack-safe eval engine fields ──

    /// Work stack for the stack-safe eval engine (LIFO queue of pending work).
    work_stack: WorkStack = .empty,

    /// Value stack for the stack-safe eval engine (LIFO results from evaluated expressions).
    value_stack: ValueStack = .empty,

    /// Non-local control flow state for the stack-safe eval engine.
    unwinding: Unwinding = .none,

    const Unwinding = union(enum) {
        none,
        early_return: Value,
        break_expr,
    };

    const WorkStack = std.ArrayListUnmanaged(work_stack.WorkItem);
    const ValueStack = std.ArrayListUnmanaged(Value);

    const JoinPointMap = std.AutoHashMapUnmanaged(u32, JoinPointInfo);

    const JoinPointInfo = struct {
        params: lir.LirPatternSpan,
        param_layouts: lir.LayoutIdxSpan,
        body: CFStmtId,
    };

    const FlatBindingList = std.array_list.AlignedManaged(FlatBinding, null);

    pub const Error = error{
        OutOfMemory,
        RuntimeError,
        DivisionByZero,
        Crash,
    };

    const Binding = struct {
        val: Value,
        size: u32,
    };

    /// Look up a binding by symbol, scanning from the end of the flat list.
    /// Most-recent binding (closest to end) wins, providing correct scoping.
    pub fn lookupBinding(self: *const LirInterpreter, symbol: u64) ?Binding {
        var i = self.bindings.items.len;
        while (i > 0) {
            i -= 1;
            if (self.bindings.items[i].symbol == symbol) {
                return .{ .val = self.bindings.items[i].val, .size = self.bindings.items[i].size };
            }
        }
        return null;
    }

    /// Result of evaluating an expression.
    /// Normal evaluation produces a value. Control flow is signaled as variants.
    pub const EvalResult = union(enum) {
        value: Value,
        early_return: Value,
        break_expr: void,
    };

    pub fn init(
        allocator: Allocator,
        store: *const LirExprStore,
        layout_store: *const layout_mod.Store,
        io: ?Io,
    ) Allocator.Error!LirInterpreter {
        const roc_env = try allocator.create(InterpreterRocEnv);
        roc_env.* = InterpreterRocEnv.init(allocator, io orelse Io.default());

        const empty_hosted_fns = struct {
            fn dummyHostedFn(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
            var empty: [1]builtins.host_abi.HostedFn = .{builtins.host_abi.hostedFn(&dummyHostedFn)};
        };

        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .helper = LayoutHelper.init(layout_store),
            .bindings = FlatBindingList.init(allocator),
            .cells = std.AutoHashMap(u64, Binding).init(allocator),
            .top_level_cache = std.AutoHashMap(u64, Binding).init(allocator),
            .evaluating = std.AutoHashMap(u64, void).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .roc_env = roc_env,
            .roc_ops = RocOps{
                .env = @ptrCast(roc_env),
                .roc_alloc = &InterpreterRocEnv.rocAllocFn,
                .roc_dealloc = &InterpreterRocEnv.rocDeallocFn,
                .roc_realloc = &InterpreterRocEnv.rocReallocFn,
                .roc_dbg = &InterpreterRocEnv.rocDbgFn,
                .roc_expect_failed = &InterpreterRocEnv.rocExpectFailedFn,
                .roc_crashed = &InterpreterRocEnv.rocCrashedFn,
                .hosted_fns = .{ .count = 0, .fns = &empty_hosted_fns.empty },
            },
        };
    }

    pub fn deinit(self: *LirInterpreter) void {
        self.roc_env.deinit();
        self.allocator.destroy(self.roc_env);
        self.arena.deinit();
        self.evaluating.deinit();
        self.top_level_cache.deinit();
        self.cells.deinit();
        self.bindings.deinit();
        self.join_points.deinit(self.allocator);
        self.work_stack.deinit(self.allocator);
        self.value_stack.deinit(self.allocator);
    }

    /// Get the crash message from the last evaluation (if any).
    /// The message is owned by the interpreter and valid until the next eval or deinit.
    pub fn getCrashMessage(self: *const LirInterpreter) ?[]const u8 {
        return self.roc_env.crash_message;
    }

    pub fn getRuntimeErrorMessage(self: *const LirInterpreter) ?[]const u8 {
        return self.roc_env.runtime_error_message;
    }

    pub fn getExpectMessage(self: *const LirInterpreter) ?[]const u8 {
        return self.roc_env.expect_message;
    }

    fn runtimeError(self: *LirInterpreter, message: []const u8) Error {
        self.roc_env.runtime_error_message = message;
        return error.RuntimeError;
    }

    fn divisionByZero(self: *LirInterpreter) Error {
        self.roc_env.runtime_error_message = division_by_zero_message;
        return error.DivisionByZero;
    }

    fn triggerCrash(self: *LirInterpreter, message: []const u8) Error {
        if (self.roc_env.crash_message) |old| self.allocator.free(old);
        self.roc_env.crash_message = self.allocator.dupe(u8, message) catch null;
        self.roc_env.crashed = true;
        return error.Crash;
    }

    /// Allocate memory for a value of the given layout.
    fn alloc(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error!Value {
        const size = self.helper.sizeOf(layout_idx);
        if (size == 0) return Value.zst;
        const slice = self.arena.allocator().alloc(u8, size) catch return error.OutOfMemory;
        @memset(slice, 0);
        return Value.fromSlice(slice);
    }

    /// Allocate raw bytes.
    fn allocBytes(self: *LirInterpreter, size: usize) Error!Value {
        if (size == 0) return Value.zst;
        const slice = self.arena.allocator().alloc(u8, size) catch return error.OutOfMemory;
        @memset(slice, 0);
        return Value.fromSlice(slice);
    }

    fn placeholderValueForLayout(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error!Value {
        if (layout_idx == .zst) return Value.zst;
        if (layout_idx == .str) return self.makeRocStr("");
        return self.alloc(layout_idx);
    }

    /// Allocate heap data through roc_ops with a refcount header.
    /// Use this for data that RocList.bytes or RocStr.bytes will point to,
    /// so builtins can safely call isUnique()/decref() on it.
    fn allocRocData(self: *LirInterpreter, data_bytes: usize, element_alignment: u32) Error![*]u8 {
        return self.allocRocDataWithRc(data_bytes, element_alignment, false);
    }

    fn allocRocDataWithRc(self: *LirInterpreter, data_bytes: usize, element_alignment: u32, elements_refcounted: bool) Error![*]u8 {
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        return builtins.utils.allocateWithRefcount(data_bytes, element_alignment, elements_refcounted, &self.roc_ops);
    }

    // Entrypoint evaluation (for roc run / interpreter shim)

    /// Evaluate an entrypoint expression, handling function calls with args.
    ///
    /// If the expression is a proc_call, it is called with arguments
    /// extracted from `arg_ptr` (a packed tuple of arg values). Otherwise the
    /// expression is evaluated directly. The result is copied to `ret_ptr`.
    ///
    /// `caller_roc_ops` provides hosted functions and runtime memory ops from
    /// the platform; the interpreter splices them into its own RocOps adapter
    /// while preserving interpreter-local crash/expect/dbg handling.
    pub fn evalEntrypoint(
        self: *LirInterpreter,
        final_expr_id: LirExprId,
        arg_layouts: []const layout_mod.Idx,
        ret_layout: layout_mod.Idx,
        caller_roc_ops: *RocOps,
        arg_ptr: ?*anyopaque,
        ret_ptr: *anyopaque,
    ) Error!void {
        // Splice in the caller's runtime-facing pieces while keeping
        // interpreter-local handlers for crash/expect/dbg.
        const prev_hosted_fns = self.roc_ops.hosted_fns;
        self.roc_ops.hosted_fns = caller_roc_ops.hosted_fns;
        self.roc_env.forwardMemoryOpsFrom(caller_roc_ops);
        self.caller_roc_ops = caller_roc_ops;
        const prev_recover_runtime_placeholders = self.recover_runtime_placeholders;
        self.recover_runtime_placeholders = true;
        defer {
            self.roc_env.resetForwardedMemoryOps();
            self.roc_ops.hosted_fns = prev_hosted_fns;
            self.caller_roc_ops = null;
            self.recover_runtime_placeholders = prev_recover_runtime_placeholders;
        }

        // Ensure eval state is initialized (matches the guard in self.eval()).
        if (!self.eval_active) {
            self.roc_env.resetForEval();
            self.eval_active = true;
        }

        // Check if the expression is a proc_call that needs argument extraction from host.
        const final_expr = self.store.getExpr(final_expr_id);
        const is_proc_call = (final_expr == .proc_call);

        if (is_proc_call) {
            // Function entrypoint: call the proc with args from arg_ptr.
            const pc = final_expr.proc_call;
            const proc_spec = self.store.getProcSpec(pc.proc);

            // Extract arguments from the packed arg tuple.
            // The host packs args as a struct sorted by alignment (descending),
            // then by original index (ascending) -- matching the Roc ABI.
            // Proc params are in semantic (signature) order, so we compute
            // each arg's byte offset in the sorted layout and extract accordingly.
            var args_buf: [16]Value = undefined;
            const arg_count = arg_layouts.len;
            if (arg_ptr) |aptr| {
                const arg_bytes = @as([*]u8, @ptrCast(aptr));

                // Build sorted index order (by alignment descending, index ascending)
                var sorted_indices: [16]usize = undefined;
                for (0..arg_count) |i| sorted_indices[i] = i;
                for (0..arg_count) |i| {
                    for (i + 1..arg_count) |j| {
                        const i_al = self.helper.sizeAlignOf(arg_layouts[sorted_indices[i]]).alignment.toByteUnits();
                        const j_al = self.helper.sizeAlignOf(arg_layouts[sorted_indices[j]]).alignment.toByteUnits();
                        if (j_al > i_al or (j_al == i_al and sorted_indices[j] < sorted_indices[i])) {
                            const tmp = sorted_indices[i];
                            sorted_indices[i] = sorted_indices[j];
                            sorted_indices[j] = tmp;
                        }
                    }
                }

                // Compute byte offset for each arg in sorted order, then extract
                var arg_offsets: [16]usize = undefined;
                var byte_offset: usize = 0;
                for (sorted_indices[0..arg_count]) |orig_idx| {
                    const sa = self.helper.sizeAlignOf(arg_layouts[orig_idx]);
                    const al = sa.alignment.toByteUnits();
                    byte_offset = std.mem.alignForward(usize, byte_offset, al);
                    arg_offsets[orig_idx] = byte_offset;
                    byte_offset += sa.size;
                }

                // Extract each arg at its computed offset
                for (0..arg_count) |i| {
                    const sa = self.helper.sizeAlignOf(arg_layouts[i]);
                    if (sa.size > 0) {
                        const copy = try self.allocBytes(sa.size);
                        @memcpy(copy.ptr[0..sa.size], arg_bytes[arg_offsets[i] .. arg_offsets[i] + sa.size]);
                        args_buf[i] = copy;
                    } else {
                        args_buf[i] = Value.zst;
                    }
                }
            }

            const call_result = try self.evalProcStackSafe(proc_spec, args_buf[0..arg_count]);
            const ret_val = switch (call_result) {
                .value => |v| v,
                .early_return => |v| v,
                .break_expr => return error.RuntimeError,
            };

            const ret_size = self.helper.sizeOf(ret_layout);
            if (ret_size > 0 and !ret_val.isZst()) {
                @memcpy(@as([*]u8, @ptrCast(ret_ptr))[0..ret_size], ret_val.readBytes(ret_size));
            }
        } else {
            // Non-function expression: evaluate directly.
            const result = try self.eval(final_expr_id);
            const val = switch (result) {
                .value => |v| v,
                .early_return => |v| v,
                .break_expr => return error.RuntimeError,
            };

            const ret_size = self.helper.sizeOf(ret_layout);
            if (ret_size > 0 and !val.isZst()) {
                @memcpy(@as([*]u8, @ptrCast(ret_ptr))[0..ret_size], val.readBytes(ret_size));
            }
        }

        // After successful evaluation, check for failed expect assertions.
        // evalExpect stores the message but does not error — we surface it here
        // so the host crash handler can report it and exit non-zero.
        if (self.roc_env.expect_message) |expect_msg| {
            const crash_msg = std.fmt.allocPrint(self.allocator, "Roc crashed: expect failed: {s}", .{expect_msg}) catch "Roc crashed: expect failed";
            if (self.roc_env.crash_message) |old| self.allocator.free(old);
            self.roc_env.crash_message = crash_msg;
            return error.Crash;
        }
    }

    // Expression evaluation

    /// Evaluate a LIR expression, returning its value.
    /// Thin wrapper around evalStackSafe that initializes eval state on the first call.
    pub fn eval(self: *LirInterpreter, initial_expr_id: LirExprId) Error!EvalResult {
        // Initialize eval state on first call (not on re-entrant calls from evalLowLevel etc.)
        if (!self.eval_active) {
            self.roc_env.resetForEval();
            self.eval_active = true;
        }
        return self.evalStackSafe(initial_expr_id);
    }

    /// Evaluate an expression, expecting a normal value (not control flow).
    fn evalValue(self: *LirInterpreter, expr_id: LirExprId) Error!Value {
        const result = try self.eval(expr_id);
        return switch (result) {
            .value => |v| v,
            .early_return => |v| v,
            .break_expr => error.RuntimeError,
        };
    }

    fn exprInvolvesMutableCell(self: *const LirInterpreter, expr_id: LirExprId) bool {
        const expr = self.store.getExpr(expr_id);
        return switch (expr) {
            .cell_load => true,
            .block => |block| blk: {
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| if (self.exprInvolvesMutableCell(binding.expr)) break :blk true,
                        .cell_init, .cell_store => |binding| if (self.exprInvolvesMutableCell(binding.expr)) break :blk true,
                        .cell_drop => {},
                    }
                }
                break :blk self.exprInvolvesMutableCell(block.final_expr);
            },
            .if_then_else => |ite| blk: {
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    if (self.exprInvolvesMutableCell(branch.cond) or self.exprInvolvesMutableCell(branch.body)) break :blk true;
                }
                break :blk self.exprInvolvesMutableCell(ite.final_else);
            },
            .match_expr => |match_expr| blk: {
                if (self.exprInvolvesMutableCell(match_expr.value)) break :blk true;
                for (self.store.getMatchBranches(match_expr.branches)) |branch| {
                    if ((!branch.guard.isNone() and self.exprInvolvesMutableCell(branch.guard)) or self.exprInvolvesMutableCell(branch.body)) break :blk true;
                }
                break :blk false;
            },
            .for_loop => |loop| self.exprInvolvesMutableCell(loop.list_expr) or self.exprInvolvesMutableCell(loop.body),
            .while_loop => |loop| self.exprInvolvesMutableCell(loop.cond) or self.exprInvolvesMutableCell(loop.body),
            .proc_call => |pc| blk: {
                for (self.store.getExprSpan(pc.args)) |arg| {
                    if (self.exprInvolvesMutableCell(arg)) break :blk true;
                }
                break :blk false;
            },
            .low_level => |ll| blk: {
                for (self.store.getExprSpan(ll.args)) |arg| {
                    if (self.exprInvolvesMutableCell(arg)) break :blk true;
                }
                break :blk false;
            },
            .list => |list_expr| blk: {
                for (self.store.getExprSpan(list_expr.elems)) |elem| {
                    if (self.exprInvolvesMutableCell(elem)) break :blk true;
                }
                break :blk false;
            },
            .struct_ => |s| blk: {
                for (self.store.getExprSpan(s.fields)) |field| {
                    if (self.exprInvolvesMutableCell(field)) break :blk true;
                }
                break :blk false;
            },
            .tag => |t| blk: {
                for (self.store.getExprSpan(t.args)) |arg| {
                    if (self.exprInvolvesMutableCell(arg)) break :blk true;
                }
                break :blk false;
            },
            .expect => |e| self.exprInvolvesMutableCell(e.cond) or self.exprInvolvesMutableCell(e.body),
            .dbg => |d| self.exprInvolvesMutableCell(d.expr),
            .nominal => |n| self.exprInvolvesMutableCell(n.backing_expr),
            .str_concat => |parts| blk: {
                for (self.store.getExprSpan(parts)) |part| {
                    if (self.exprInvolvesMutableCell(part)) break :blk true;
                }
                break :blk false;
            },
            .int_to_str => |its| self.exprInvolvesMutableCell(its.value),
            .float_to_str => |fts| self.exprInvolvesMutableCell(fts.value),
            .dec_to_str => |arg| self.exprInvolvesMutableCell(arg),
            .str_escape_and_quote => |arg| self.exprInvolvesMutableCell(arg),
            .discriminant_switch => |ds| blk: {
                if (self.exprInvolvesMutableCell(ds.value)) break :blk true;
                for (self.store.getExprSpan(ds.branches)) |branch| {
                    if (self.exprInvolvesMutableCell(branch)) break :blk true;
                }
                break :blk false;
            },
            .tag_payload_access => |tpa| self.exprInvolvesMutableCell(tpa.value),
            .hosted_call => |hc| blk: {
                for (self.store.getExprSpan(hc.args)) |arg| {
                    if (self.exprInvolvesMutableCell(arg)) break :blk true;
                }
                break :blk false;
            },
            .incref => |rc| self.exprInvolvesMutableCell(rc.value),
            .decref => |rc| self.exprInvolvesMutableCell(rc.value),
            .free => |rc| self.exprInvolvesMutableCell(rc.value),
            else => false,
        };
    }

    fn exprHasLoopExit(self: *const LirInterpreter, expr_id: LirExprId) bool {
        const expr = self.store.getExpr(expr_id);
        return switch (expr) {
            .early_return, .break_expr => true,
            .for_loop, .while_loop => false,
            .block => |block| blk: {
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| if (self.exprHasLoopExit(binding.expr)) break :blk true,
                        .cell_init, .cell_store => |binding| if (self.exprHasLoopExit(binding.expr)) break :blk true,
                        .cell_drop => {},
                    }
                }
                break :blk self.exprHasLoopExit(block.final_expr);
            },
            .if_then_else => |ite| blk: {
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    if (self.exprHasLoopExit(branch.cond) or self.exprHasLoopExit(branch.body)) break :blk true;
                }
                break :blk self.exprHasLoopExit(ite.final_else);
            },
            .match_expr => |match_expr| blk: {
                if (self.exprHasLoopExit(match_expr.value)) break :blk true;
                for (self.store.getMatchBranches(match_expr.branches)) |branch| {
                    if ((!branch.guard.isNone() and self.exprHasLoopExit(branch.guard)) or self.exprHasLoopExit(branch.body)) break :blk true;
                }
                break :blk false;
            },
            .proc_call => |pc| blk: {
                for (self.store.getExprSpan(pc.args)) |arg| {
                    if (self.exprHasLoopExit(arg)) break :blk true;
                }
                break :blk false;
            },
            .low_level => |ll| blk: {
                for (self.store.getExprSpan(ll.args)) |arg| {
                    if (self.exprHasLoopExit(arg)) break :blk true;
                }
                break :blk false;
            },
            .list => |list_expr| blk: {
                for (self.store.getExprSpan(list_expr.elems)) |elem| {
                    if (self.exprHasLoopExit(elem)) break :blk true;
                }
                break :blk false;
            },
            .struct_ => |s| blk: {
                for (self.store.getExprSpan(s.fields)) |field| {
                    if (self.exprHasLoopExit(field)) break :blk true;
                }
                break :blk false;
            },
            .tag => |t| blk: {
                for (self.store.getExprSpan(t.args)) |arg| {
                    if (self.exprHasLoopExit(arg)) break :blk true;
                }
                break :blk false;
            },
            .expect => |e| self.exprHasLoopExit(e.cond) or self.exprHasLoopExit(e.body),
            .dbg => |d| self.exprHasLoopExit(d.expr),
            .nominal => |n| self.exprHasLoopExit(n.backing_expr),
            .str_concat => |parts| blk: {
                for (self.store.getExprSpan(parts)) |part| {
                    if (self.exprHasLoopExit(part)) break :blk true;
                }
                break :blk false;
            },
            .int_to_str => |its| self.exprHasLoopExit(its.value),
            .float_to_str => |fts| self.exprHasLoopExit(fts.value),
            .dec_to_str => |arg| self.exprHasLoopExit(arg),
            .str_escape_and_quote => |arg| self.exprHasLoopExit(arg),
            .discriminant_switch => |ds| blk: {
                if (self.exprHasLoopExit(ds.value)) break :blk true;
                for (self.store.getExprSpan(ds.branches)) |branch| {
                    if (self.exprHasLoopExit(branch)) break :blk true;
                }
                break :blk false;
            },
            .tag_payload_access => |tpa| self.exprHasLoopExit(tpa.value),
            .hosted_call => |hc| blk: {
                for (self.store.getExprSpan(hc.args)) |arg| {
                    if (self.exprHasLoopExit(arg)) break :blk true;
                }
                break :blk false;
            },
            .incref => |rc| self.exprHasLoopExit(rc.value),
            .decref => |rc| self.exprHasLoopExit(rc.value),
            .free => |rc| self.exprHasLoopExit(rc.value),
            else => false,
        };
    }

    // Literals

    fn evalI64Literal(self: *LirInterpreter, value: i64, layout_idx: layout_mod.Idx) Error!Value {
        const val = try self.alloc(layout_idx);
        const size = self.helper.sizeOf(layout_idx);
        const bits: u64 = @bitCast(value);
        switch (size) {
            1 => val.write(u8, @truncate(bits)),
            2 => val.write(u16, @truncate(bits)),
            4 => val.write(u32, @truncate(bits)),
            8 => val.write(u64, bits),
            else => return error.RuntimeError,
        }
        return val;
    }

    fn evalI128Literal(self: *LirInterpreter, value: i128, layout_idx: layout_mod.Idx) Error!Value {
        const val = try self.alloc(layout_idx);
        val.write(i128, value);
        return val;
    }

    fn evalF64Literal(self: *LirInterpreter, value: f64) Error!Value {
        const val = try self.alloc(.f64);
        val.write(f64, value);
        return val;
    }

    fn evalF32Literal(self: *LirInterpreter, value: f32) Error!Value {
        const val = try self.alloc(.f32);
        val.write(f32, value);
        return val;
    }

    fn evalDecLiteral(self: *LirInterpreter, value: i128) Error!Value {
        const val = try self.alloc(.dec);
        val.write(i128, value);
        return val;
    }

    fn evalStrLiteral(self: *LirInterpreter, idx: base.StringLiteral.Idx) Error!Value {
        const str_bytes = self.store.getString(idx);
        return self.makeRocStr(str_bytes);
    }

    fn evalBoolLiteral(self: *LirInterpreter, b: bool) Error!Value {
        const val = try self.alloc(.bool);
        val.write(u8, if (b) 1 else 0);
        return val;
    }

    // String helpers (RocStr construction)

    fn makeRocStr(self: *LirInterpreter, bytes: []const u8) Error!Value {
        const rs = builtins.str.RocStr.fromSlice(bytes, &self.roc_ops);
        return self.rocStrToValue(rs, .str);
    }

    /// Read the bytes from a RocStr value.
    /// Note: we cannot simply do `valueToRocStr(val).asSlice()` because for
    /// small strings `asSlice` returns a pointer into the RocStr struct itself,
    /// which would be a dangling stack reference. Instead, for small strings we
    /// return a slice of `val.ptr` (the arena-backed Value buffer where the
    /// inline data actually lives).
    fn readRocStr(_: *LirInterpreter, val: Value) []const u8 {
        const rs = valueToRocStr(val);
        if (rs.isSmallStr()) {
            return val.ptr[0..rs.len()];
        }
        return rs.asSlice();
    }

    // Lookup

    fn evalLookup(self: *LirInterpreter, symbol: Symbol, layout_idx: layout_mod.Idx) Error!Value {
        // Check local bindings first
        if (self.lookupBinding(symbol.raw())) |binding| {
            return binding.val;
        }

        // Check top-level cache
        if (self.top_level_cache.get(symbol.raw())) |binding| {
            return binding.val;
        }

        // Try evaluating as a top-level def
        if (self.store.getSymbolDef(symbol)) |def_expr_id| {
            // Cycle detection
            if (self.evaluating.contains(symbol.raw())) {
                return error.RuntimeError;
            }
            self.evaluating.put(symbol.raw(), {}) catch return error.OutOfMemory;
            defer _ = self.evaluating.remove(symbol.raw());

            const result = try self.eval(def_expr_id);
            const val = switch (result) {
                .value => |v| v,
                else => return error.RuntimeError,
            };

            const size = self.helper.sizeOf(layout_idx);
            self.top_level_cache.put(symbol.raw(), .{ .val = val, .size = size }) catch return error.OutOfMemory;
            return val;
        }
        return error.RuntimeError;
    }

    fn evalCellLoad(self: *LirInterpreter, symbol: Symbol, layout_idx: layout_mod.Idx) Error!Value {
        if (self.cells.get(symbol.raw())) |binding| {
            // Copy the cell's current value
            const size = self.helper.sizeOf(layout_idx);
            const copy = try self.allocBytes(size);
            copy.copyFrom(binding.val, size);
            return copy;
        }
        return error.RuntimeError;
    }

    // Pattern binding

    fn bindPattern(self: *LirInterpreter, pattern_id: LirPatternId, val: Value) Error!void {
        const pat = self.store.getPattern(pattern_id);
        switch (pat) {
            .bind => |b| {
                const size = self.helper.sizeOf(b.layout_idx);
                self.bindings.append(.{ .symbol = b.symbol.raw(), .val = val, .size = size }) catch return error.OutOfMemory;
            },
            .wildcard => {}, // Nothing to bind
            .struct_ => |s| {
                const fields = self.store.getPatternSpan(s.fields);
                for (fields, 0..) |field_pat_id, i| {
                    const field_offset = self.helper.structFieldOffset(s.struct_layout, @intCast(i));
                    const field_val = val.offset(field_offset);
                    try self.bindPattern(field_pat_id, field_val);
                }
            },
            .tag => |t| {
                const args = self.store.getPatternSpan(t.args);
                for (args, 0..) |arg_pat_id, i| {
                    const arg_val = self.tagPayloadArgValueForPattern(
                        val,
                        t.union_layout,
                        t.discriminant,
                        @intCast(i),
                        arg_pat_id,
                    );
                    try self.bindPattern(arg_pat_id, arg_val);
                }
            },
            .as_pattern => |ap| {
                // Bind the name
                const size = self.helper.sizeOf(ap.layout_idx);
                self.bindings.append(.{ .symbol = ap.symbol.raw(), .val = val, .size = size }) catch return error.OutOfMemory;
                // Also bind the inner pattern
                try self.bindPattern(ap.inner, val);
            },
            .int_literal, .float_literal, .str_literal => {}, // Literal patterns don't bind
            .list => |list_pat| {
                const prefix = self.store.getPatternSpan(list_pat.prefix);
                const suffix = self.store.getPatternSpan(list_pat.suffix);
                const total_len = valueToRocList(val).len();
                const fixed_len = prefix.len + suffix.len;

                if (list_pat.rest.isNone()) {
                    if (total_len != fixed_len) return error.RuntimeError;
                } else if (total_len < fixed_len) {
                    return error.RuntimeError;
                }

                for (prefix, 0..) |elem_pat_id, i| {
                    const elem_val = try self.listElementValue(val, list_pat.list_layout, list_pat.elem_layout, i);
                    try self.bindPattern(elem_pat_id, elem_val);
                }

                for (suffix, 0..) |elem_pat_id, i| {
                    const elem_idx = total_len - suffix.len + i;
                    const elem_val = try self.listElementValue(val, list_pat.list_layout, list_pat.elem_layout, elem_idx);
                    try self.bindPattern(elem_pat_id, elem_val);
                }

                if (!list_pat.rest.isNone()) {
                    const rest_len = total_len - fixed_len;
                    const rest_val = try self.listSliceValue(val, list_pat.list_layout, prefix.len, rest_len);
                    try self.bindPattern(list_pat.rest, rest_val);
                }
            },
        }
    }

    /// Check if a value matches a pattern.
    fn matchPattern(self: *LirInterpreter, pattern_id: LirPatternId, val: Value) Error!bool {
        const pat = self.store.getPattern(pattern_id);
        return switch (pat) {
            .bind, .wildcard, .as_pattern => true,
            .int_literal => |lit| blk: {
                const size = self.helper.sizeOf(lit.layout_idx);
                break :blk switch (size) {
                    1 => val.read(i8) == @as(i8, @intCast(lit.value)),
                    2 => val.read(i16) == @as(i16, @intCast(lit.value)),
                    4 => val.read(i32) == @as(i32, @intCast(lit.value)),
                    8 => val.read(i64) == @as(i64, @intCast(lit.value)),
                    16 => val.read(i128) == lit.value,
                    else => false,
                };
            },
            .float_literal => |lit| val.read(f64) == lit.value,
            .str_literal => |idx| blk: {
                const expected = self.store.getString(idx);
                const actual = self.readRocStr(val);
                break :blk rocStrEqualSlices(actual, expected);
            },
            .tag => |t| blk: {
                const tag_base = self.resolveTagUnionBaseValue(val, t.union_layout);
                const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
                if (disc != t.discriminant) break :blk false;
                // Check payload patterns
                const args = self.store.getPatternSpan(t.args);
                for (args, 0..) |arg_pat_id, i| {
                    const arg_val = self.tagPayloadArgValueForPattern(
                        val,
                        t.union_layout,
                        t.discriminant,
                        @intCast(i),
                        arg_pat_id,
                    );
                    if (!try self.matchPattern(arg_pat_id, arg_val)) break :blk false;
                }
                break :blk true;
            },
            .struct_ => |s| blk: {
                const fields = self.store.getPatternSpan(s.fields);
                for (fields, 0..) |field_pat_id, i| {
                    const field_offset = self.helper.structFieldOffset(s.struct_layout, @intCast(i));
                    const field_val = val.offset(field_offset);
                    if (!try self.matchPattern(field_pat_id, field_val)) break :blk false;
                }
                break :blk true;
            },
            .list => |list_pat| blk: {
                const prefix = self.store.getPatternSpan(list_pat.prefix);
                const suffix = self.store.getPatternSpan(list_pat.suffix);
                const total_len = valueToRocList(val).len();
                const fixed_len = prefix.len + suffix.len;

                if (list_pat.rest.isNone()) {
                    if (total_len != fixed_len) break :blk false;
                } else if (total_len < fixed_len) {
                    break :blk false;
                }

                for (prefix, 0..) |elem_pat_id, i| {
                    const elem_val = try self.listElementValue(val, list_pat.list_layout, list_pat.elem_layout, i);
                    if (!try self.matchPattern(elem_pat_id, elem_val)) break :blk false;
                }

                for (suffix, 0..) |elem_pat_id, i| {
                    const elem_idx = total_len - suffix.len + i;
                    const elem_val = try self.listElementValue(val, list_pat.list_layout, list_pat.elem_layout, elem_idx);
                    if (!try self.matchPattern(elem_pat_id, elem_val)) break :blk false;
                }

                if (!list_pat.rest.isNone()) {
                    const rest_len = total_len - fixed_len;
                    const rest_val = try self.listSliceValue(val, list_pat.list_layout, prefix.len, rest_len);
                    if (!try self.matchPattern(list_pat.rest, rest_val)) break :blk false;
                }

                break :blk true;
            },
        };
    }

    // Aggregates

    fn evalZeroArgTag(self: *LirInterpreter, z: anytype) Error!Value {
        const val = try self.alloc(z.union_layout);
        self.helper.writeTagDiscriminant(val, z.union_layout, z.discriminant);
        return val;
    }

    fn evalEmptyList(self: *LirInterpreter, l: anytype) Error!Value {
        // RocList with all zeros = empty list
        return self.alloc(l.list_layout);
    }

    // Function calls — all go through the stack-safe engine via enterFunction/evalProcStackSafe.

    // Reference counting

    const RcOp = layout_mod.RcOp;

    /// Perform a reference count operation on a value using the layout-driven
    /// RC helper plan.  This walks structs, tag unions, boxes, etc. recursively
    /// so the interpreter's refcounting matches what the dev backend emits.
    fn performRc(self: *LirInterpreter, op: RcOp, val: Value, layout_idx: layout_mod.Idx, count: u16) void {
        trace.log("performRc: op={s} layout={any} val.ptr={*} count={d}", .{ @tagName(op), layout_idx, val.ptr, count });
        const resolver = layout_mod.RcHelperResolver.init(self.layout_store);
        const key = resolver.makeKey(op, layout_idx);
        self.performRcPlan(resolver.plan(key), &resolver, val, count);
    }

    fn performRcPlan(self: *LirInterpreter, rc_plan: layout_mod.RcHelperPlan, resolver: *const layout_mod.RcHelperResolver, val: Value, count: u16) void {
        trace.log("performRcPlan: plan={s} val.ptr={*}", .{ @tagName(rc_plan), val.ptr });
        const utils = builtins.utils;
        switch (rc_plan) {
            .noop => {},
            .str_incref => {
                const rs = valueToRocStr(val);
                trace_rc.log("str_incref: bytes=0x{x} len={d} cap={d} count={d}", .{ @intFromPtr(rs.bytes), rs.length, rs.capacity_or_alloc_ptr, count });
                rs.incref(count, &self.roc_ops);
            },
            .str_decref => {
                const rs = valueToRocStr(val);
                trace_rc.log("str_decref: bytes=0x{x} len={d} cap={d}", .{ @intFromPtr(rs.bytes), rs.length, rs.capacity_or_alloc_ptr });
                rs.decref(&self.roc_ops);
            },
            .str_free => {
                const rs = valueToRocStr(val);
                trace_rc.log("str_free: bytes=0x{x} len={d} cap={d}", .{ @intFromPtr(rs.bytes), rs.length, rs.capacity_or_alloc_ptr });
                rs.decref(&self.roc_ops);
            },
            .list_incref => {
                const rl = valueToRocList(val);
                const has_child = false; // incref doesn't recurse into elements
                trace_rc.log("list_incref: bytes=0x{x} len={d} cap={d} count={d}", .{ @intFromPtr(rl.bytes), rl.len(), rl.capacity_or_alloc_ptr, count });
                rl.incref(@intCast(count), has_child, &self.roc_ops);
            },
            .list_decref => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                const alloc_ptr = rl.getAllocationDataPtr(&self.roc_ops);
                trace_rc.log("list_decref: bytes=0x{x} len={d} cap={d} alloc_ptr=0x{x} has_child={any} elem_align={d}", .{
                    @intFromPtr(rl.bytes), rl.len(), rl.capacity_or_alloc_ptr,
                    @intFromPtr(alloc_ptr),
                    has_child, list_plan.elem_alignment,
                });
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    &self.roc_ops,
                );
            },
            .list_free => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                const alloc_ptr = rl.getAllocationDataPtr(&self.roc_ops);
                trace_rc.log("list_free: bytes=0x{x} len={d} cap={d} alloc_ptr=0x{x} has_child={any}", .{
                    @intFromPtr(rl.bytes), rl.len(), rl.capacity_or_alloc_ptr,
                    @intFromPtr(alloc_ptr), has_child,
                });
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    &self.roc_ops,
                );
            },
            .box_incref => {
                const alloc_ptr = val.read(?[*]u8);
                utils.increfDataPtrC(alloc_ptr, @intCast(count), &self.roc_ops);
            },
            .box_decref => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                utils.decrefDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
            },
            .box_free => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
            },
            .struct_ => |struct_plan| {
                const field_count = resolver.structFieldCount(struct_plan);
                var i: u32 = 0;
                while (i < field_count) : (i += 1) {
                    const field_plan = resolver.structFieldPlan(struct_plan, i) orelse continue;
                    const field_val = Value{ .ptr = val.ptr + field_plan.offset };
                    self.performRcPlan(resolver.plan(field_plan.child), resolver, field_val, count);
                }
            },
            .tag_union => {
                // Tag unions with heap-allocated payloads need discriminant-based dispatch.
                // TODO: implement full tag union RC walking
            },
            .closure => |child_key| {
                self.performRcPlan(resolver.plan(child_key), resolver, val, count);
            },
        }
    }

    // Crash / dbg / expect

    fn renderExpectExpr(self: *LirInterpreter, expr_id: LirExprId) Error![]const u8 {
        const arena = self.arena.allocator();
        const expr = self.store.getExpr(expr_id);

        return switch (expr) {
            .block => |block| try self.renderExpectExpr(block.final_expr),
            .i64_literal => |lit| std.fmt.allocPrint(arena, "{d}", .{lit.value}) catch return error.OutOfMemory,
            .i128_literal => |lit| std.fmt.allocPrint(arena, "{d}", .{lit.value}) catch return error.OutOfMemory,
            .f64_literal => |lit| try self.renderCompactFloat(@as(f64, lit)),
            .f32_literal => |lit| try self.renderCompactFloat(@as(f64, lit)),
            .dec_literal => |lit| blk: {
                const dec = RocDec{ .num = lit };
                if (@rem(lit, RocDec.one_point_zero_i128) == 0) {
                    break :blk std.fmt.allocPrint(arena, "{d}", .{dec.toWholeInt()}) catch return error.OutOfMemory;
                }
                var buf: [RocDec.max_str_length]u8 = undefined;
                break :blk std.fmt.allocPrint(arena, "{s}", .{dec.format_to_buf(&buf)}) catch return error.OutOfMemory;
            },
            .bool_literal => |lit| if (lit) "True" else "False",
            .str_literal => |idx| std.fmt.allocPrint(arena, "\"{s}\"", .{self.store.getString(idx)}) catch return error.OutOfMemory,
            .lookup => |lookup| blk: {
                if (self.lookupBinding(lookup.symbol.raw())) |binding| {
                    break :blk try self.renderExpectValue(binding.val, lookup.layout_idx);
                }
                if (self.top_level_cache.get(lookup.symbol.raw())) |binding| {
                    break :blk try self.renderExpectValue(binding.val, lookup.layout_idx);
                }
                break :blk std.fmt.allocPrint(arena, "sym#{d}", .{lookup.symbol.raw()}) catch return error.OutOfMemory;
            },
            .nominal => |nom| try self.renderExpectExpr(nom.backing_expr),
            .low_level => |ll| blk: {
                const op_text = switch (ll.op) {
                    .num_is_eq => "==",
                    .num_is_gt => ">",
                    .num_is_gte => ">=",
                    .num_is_lt => "<",
                    .num_is_lte => "<=",
                    .num_plus => "+",
                    .num_minus => "-",
                    .num_times => "*",
                    else => break :blk std.fmt.allocPrint(arena, "{s}", .{@tagName(ll.op)}) catch return error.OutOfMemory,
                };

                const args = self.store.getExprSpan(ll.args);
                if (args.len != 2) {
                    break :blk std.fmt.allocPrint(arena, "{s}", .{@tagName(ll.op)}) catch return error.OutOfMemory;
                }

                const lhs = try self.renderExpectExpr(args[0]);
                const rhs = try self.renderExpectExpr(args[1]);
                break :blk std.fmt.allocPrint(arena, "{s} {s} {s}", .{ lhs, op_text, rhs }) catch return error.OutOfMemory;
            },
            else => "expect failed",
        };
    }

    fn renderExpectValue(self: *LirInterpreter, value: Value, layout_idx: layout_mod.Idx) Error![]const u8 {
        const arena = self.arena.allocator();
        if (layout_idx == .bool) {
            return if (value.read(u8) != 0) "True" else "False";
        }

        const layout_val = self.layout_store.getLayout(layout_idx);

        return switch (layout_val.tag) {
            .scalar => switch (layout_val.data.scalar.tag) {
                .int => switch (self.helper.sizeOf(layout_idx)) {
                    1 => std.fmt.allocPrint(arena, "{d}", .{value.read(i8)}) catch return error.OutOfMemory,
                    2 => std.fmt.allocPrint(arena, "{d}", .{value.read(i16)}) catch return error.OutOfMemory,
                    4 => std.fmt.allocPrint(arena, "{d}", .{value.read(i32)}) catch return error.OutOfMemory,
                    8 => std.fmt.allocPrint(arena, "{d}", .{value.read(i64)}) catch return error.OutOfMemory,
                    16 => std.fmt.allocPrint(arena, "{d}", .{value.read(i128)}) catch return error.OutOfMemory,
                    else => "expect failed",
                },
                .str => std.fmt.allocPrint(arena, "\"{s}\"", .{self.readRocStr(value)}) catch return error.OutOfMemory,
                .frac => switch (self.helper.sizeOf(layout_idx)) {
                    4 => try self.renderCompactFloat(@as(f64, value.read(f32))),
                    8 => try self.renderCompactFloat(value.read(f64)),
                    16 => blk: {
                        const dec = RocDec{ .num = value.read(i128) };
                        if (@rem(dec.num, RocDec.one_point_zero_i128) == 0) {
                            break :blk std.fmt.allocPrint(arena, "{d}", .{dec.toWholeInt()}) catch return error.OutOfMemory;
                        }
                        var buf: [RocDec.max_str_length]u8 = undefined;
                        break :blk std.fmt.allocPrint(arena, "{s}", .{dec.format_to_buf(&buf)}) catch return error.OutOfMemory;
                    },
                    else => "expect failed",
                },
            },
            else => "expect failed",
        };
    }

    fn renderCompactFloat(self: *LirInterpreter, value: f64) Error![]const u8 {
        var buf: [400]u8 = undefined;
        const slice = i128h.f64_to_str(&buf, value);
        return self.arena.allocator().dupe(u8, slice) catch error.OutOfMemory;
    }

    fn isRecoverableStringPlaceholder(self: *LirInterpreter, expr_id: LirExprId) bool {
        return switch (self.store.getExpr(expr_id)) {
            .runtime_error => true,
            .block => |block| self.isRecoverableStringPlaceholder(block.final_expr),
            .nominal => |nom| self.isRecoverableStringPlaceholder(nom.backing_expr),
            .dbg => |dbg_expr| self.isRecoverableStringPlaceholder(dbg_expr.expr),
            else => false,
        };
    }

    // Hosted function calls

    fn evalHostedCall(self: *LirInterpreter, hc: anytype) Error!Value {
        const args_exprs = self.store.getExprSpan(hc.args);

        // Collect argument values and layouts.
        // When explicit args are empty, fall back to the enclosing lambda's bound
        // parameters (same pattern as the dev backend's collectImplicitHostedCallArgs).
        const ArgInfo = struct { val: Value, layout: layout_mod.Idx };
        var collected_args = std.ArrayList(ArgInfo).empty;
        defer collected_args.deinit(self.allocator);

        if (args_exprs.len > 0) {
            // Explicit args: evaluate each one
            for (args_exprs) |arg_id| {
                const arg_val = try self.evalValue(arg_id);
                const arg_layout = lir_program_mod.lirExprResultLayout(self.store, arg_id);
                collected_args.append(self.allocator, .{ .val = arg_val, .layout = arg_layout }) catch return error.OutOfMemory;
            }
        } else if (self.current_lambda_params) |lambda_params| {
            // Implicit args: read from enclosing lambda's bound parameters
            for (self.store.getPatternSpan(lambda_params)) |pat_id| {
                const pat = self.store.getPattern(pat_id);
                switch (pat) {
                    .bind => |bind| {
                        if (self.lookupBinding(bind.symbol.raw())) |binding| {
                            collected_args.append(self.allocator, .{
                                .val = binding.val,
                                .layout = bind.layout_idx,
                            }) catch return error.OutOfMemory;
                        }
                    },
                    .wildcard => {},
                    else => {},
                }
            }
        }

        // Marshal arguments into a contiguous buffer
        var total_args_size: usize = 0;
        for (collected_args.items) |arg| {
            const sa = self.helper.sizeAlignOf(arg.layout);
            total_args_size = std.mem.alignForward(usize, total_args_size, sa.alignment.toByteUnits());
            total_args_size += sa.size;
        }

        const args_buf_size = @max(total_args_size, 8);
        const args_buf = self.arena.allocator().alloc(u8, args_buf_size) catch return error.OutOfMemory;
        @memset(args_buf, 0);

        var offset: usize = 0;
        for (collected_args.items) |arg| {
            const sa = self.helper.sizeAlignOf(arg.layout);
            offset = std.mem.alignForward(usize, offset, sa.alignment.toByteUnits());
            if (sa.size > 0 and !arg.val.isZst()) {
                @memcpy(args_buf[offset .. offset + sa.size], arg.val.readBytes(sa.size));
            }
            offset += sa.size;
        }

        // Allocate return buffer
        const ret_size = self.helper.sizeOf(hc.ret_layout);
        var ret_buf: [64]u8 align(16) = undefined;
        @memset(ret_buf[0..@max(ret_size, 1)], 0);

        // Call: hosted_fn(roc_ops, ret_ptr, args_ptr)
        // Pass the caller's RocOps so the hosted function gets the platform's env
        // (the host casts ops.env to its own HostEnv type).
        const hosted_fn = self.roc_ops.hosted_fns.fns[hc.index];
        self.roc_env.resetCrash();
        const ops_for_host: *RocOps = self.caller_roc_ops orelse &self.roc_ops;
        hosted_fn(@ptrCast(ops_for_host), @ptrCast(&ret_buf), @ptrCast(args_buf.ptr));

        if (self.roc_env.crashed) return error.Crash;

        // Copy result into interpreter value
        if (ret_size == 0) return Value.zst;
        const result = try self.alloc(hc.ret_layout);
        @memcpy(result.ptr[0..ret_size], ret_buf[0..ret_size]);
        return result;
    }

    // Low-level operations — direct builtin dispatch

    /// Resolve the result layout of a LIR expression.
    fn exprLayout(self: *LirInterpreter, expr_id: LirExprId) layout_mod.Idx {
        return lir_program_mod.lirExprResultLayout(self.store, expr_id);
    }

    // ── Value ↔ RocStr/RocList marshaling ──

    fn valueToRocStr(val: Value) RocStr {
        var rs: RocStr = undefined;
        @memcpy(std.mem.asBytes(&rs), val.ptr[0..@sizeOf(RocStr)]);
        return rs;
    }

    fn rocStrToValue(self: *LirInterpreter, rs: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        @memcpy(val.ptr[0..@sizeOf(RocStr)], std.mem.asBytes(&rs));
        return val;
    }

    fn valueToRocList(val: Value) RocList {
        var rl: RocList = undefined;
        @memcpy(std.mem.asBytes(&rl), val.ptr[0..@sizeOf(RocList)]);
        return rl;
    }

    fn rocListToValue(self: *LirInterpreter, rl: RocList, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));
        return val;
    }

    const ListElemInfo = struct { alignment: u32, width: usize, rc: bool };

    fn listElemInfo(self: *LirInterpreter, list_layout: layout_mod.Idx) ListElemInfo {
        const l = self.layout_store.getLayout(list_layout);
        if (l.tag == .list) {
            const elem_idx = l.data.list;
            const sa = self.helper.sizeAlignOf(elem_idx);
            return .{
                .alignment = @intCast(sa.alignment.toByteUnits()),
                .width = sa.size,
                .rc = self.helper.containsRefcounted(elem_idx),
            };
        }
        return .{ .alignment = 1, .width = 0, .rc = false };
    }

    fn listElemLayout(self: *LirInterpreter, list_layout: layout_mod.Idx) layout_mod.Idx {
        const l = self.layout_store.getLayout(list_layout);
        if (l.tag == .list) return l.data.list;
        return .zst;
    }

    fn listElementValue(
        self: *LirInterpreter,
        list_val: Value,
        list_layout: layout_mod.Idx,
        elem_layout: layout_mod.Idx,
        index: usize,
    ) Error!Value {
        const rl = valueToRocList(list_val);
        if (index >= rl.len()) return error.RuntimeError;

        const info = self.listElemInfo(list_layout);
        if (info.width == 0) {
            return try self.alloc(elem_layout);
        }

        const bytes = rl.bytes orelse return error.RuntimeError;
        return .{ .ptr = bytes + index * info.width };
    }

    fn listSliceValue(
        self: *LirInterpreter,
        list_val: Value,
        list_layout: layout_mod.Idx,
        start: usize,
        len: usize,
    ) Error!Value {
        const rl = valueToRocList(list_val);
        if (len == 0 or start >= rl.len()) {
            return self.rocListToValue(RocList.empty(), list_layout);
        }

        const keep_len = @min(len, rl.len() - start);
        const info = self.listElemInfo(list_layout);

        if (info.width == 0) {
            return self.rocListToValue(.{
                .bytes = rl.bytes,
                .length = keep_len,
                .capacity_or_alloc_ptr = keep_len,
            }, list_layout);
        }

        if (start == 0 and keep_len == rl.len()) {
            rl.incref(1, info.rc, &self.roc_ops);
            return self.rocListToValue(rl, list_layout);
        }

        const source_ptr = rl.bytes orelse return error.RuntimeError;
        rl.incref(1, info.rc, &self.roc_ops);

        const list_alloc_ptr = (@intFromPtr(source_ptr) >> 1) | builtins.list.SEAMLESS_SLICE_BIT;
        const slice_alloc_ptr = rl.capacity_or_alloc_ptr;
        const slice_mask = rl.seamlessSliceMask();
        const alloc_ptr = (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);

        return self.rocListToValue(.{
            .bytes = source_ptr + start * info.width,
            .length = keep_len,
            .capacity_or_alloc_ptr = alloc_ptr,
        }, list_layout);
    }

    // ── Builtin call with crash recovery ──

    fn callBuiltinStr1(self: *LirInterpreter, comptime func: anytype, a: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = func(a, &self.roc_ops);
        return self.rocStrToValue(result, ret_layout);
    }

    fn callBuiltinStr2(self: *LirInterpreter, comptime func: anytype, a: RocStr, b: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = func(a, b, &self.roc_ops);
        return self.rocStrToValue(result, ret_layout);
    }

    fn unwrapSingleFieldPayloadLayout(self: *LirInterpreter, layout_idx: layout_mod.Idx) ?layout_mod.Idx {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layout_store.getStructData(layout_val.data.struct_.idx);
        const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return field.layout;
    }

    fn evalLowLevel(self: *LirInterpreter, ll: anytype) Error!Value {
        const arg_exprs = self.store.getExprSpan(ll.args);
        var args: [8]Value = undefined;
        const n = @min(arg_exprs.len, 8);
        for (0..n) |i| {
            args[i] = try self.evalValue(arg_exprs[i]);
        }

        // Determine argument layout for numeric ops (operand type, not return type)
        const arg_layout: layout_mod.Idx = if (arg_exprs.len > 0)
            self.exprLayout(arg_exprs[0])
        else
            ll.ret_layout;

        return switch (ll.op) {
            // ── String ops ──
            .str_is_eq => blk: {
                const result = builtins.str.strEqual(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_concat => self.callBuiltinStr2(builtins.str.strConcatC, valueToRocStr(args[0]), valueToRocStr(args[1]), ll.ret_layout),
            .str_contains => blk: {
                const result = builtins.str.strContains(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_starts_with => blk: {
                const result = builtins.str.startsWith(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_ends_with => blk: {
                const result = builtins.str.endsWith(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_trim => self.callBuiltinStr1(builtins.str.strTrim, valueToRocStr(args[0]), ll.ret_layout),
            .str_trim_start => self.callBuiltinStr1(builtins.str.strTrimStart, valueToRocStr(args[0]), ll.ret_layout),
            .str_trim_end => self.callBuiltinStr1(builtins.str.strTrimEnd, valueToRocStr(args[0]), ll.ret_layout),
            .str_with_ascii_lowercased => self.callBuiltinStr1(builtins.str.strWithAsciiLowercased, valueToRocStr(args[0]), ll.ret_layout),
            .str_with_ascii_uppercased => self.callBuiltinStr1(builtins.str.strWithAsciiUppercased, valueToRocStr(args[0]), ll.ret_layout),
            .str_caseless_ascii_equals => blk: {
                const result = builtins.str.strCaselessAsciiEquals(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_repeat => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.repeatC(valueToRocStr(args[0]), args[1].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_drop_prefix => self.callBuiltinStr2(builtins.str.strDropPrefix, valueToRocStr(args[0]), valueToRocStr(args[1]), ll.ret_layout),
            .str_drop_suffix => self.callBuiltinStr2(builtins.str.strDropSuffix, valueToRocStr(args[0]), valueToRocStr(args[1]), ll.ret_layout),
            .str_count_utf8_bytes => blk: {
                const result = builtins.str.countUtf8Bytes(valueToRocStr(args[0]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u64, result);
                break :blk val;
            },
            .str_to_utf8 => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.strToUtf8C(valueToRocStr(args[0]), &self.roc_ops);
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .str_from_utf8 => blk: {
                // str_from_utf8(list) -> Result Str [BadUtf8 {index: U64, problem: Utf8Problem}]
                // The C builtin returns FromUtf8Try (a flat struct).
                // Convert to the Roc tag union layout using layout-resolved offsets,
                // following the same pattern as the dev backend (LirCodeGen.zig).
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8C(valueToRocList(args[0]), UpdateMode.Immutable, &self.roc_ops);

                const ret_layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (ret_layout_val.tag != .tag_union) {
                    return self.runtimeError("str_from_utf8 expected a tag union return layout");
                }
                const tu_data = self.layout_store.getTagUnionData(ret_layout_val.data.tag_union.idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);

                // Discover Ok (Str payload) and Err variant indices from the layout.
                var ok_disc: ?u16 = null;
                var err_disc: ?u16 = null;
                var err_record_idx: ?layout_mod.StructIdx = null;
                for (0..variants.len) |i| {
                    const v_payload = variants.get(@intCast(i)).payload_layout;
                    const candidate = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                    if (candidate == .str) {
                        ok_disc = @intCast(i);
                    } else {
                        err_disc = @intCast(i);
                        const err_layout = self.layout_store.getLayout(candidate);
                        err_record_idx = switch (err_layout.tag) {
                            .struct_ => err_layout.data.struct_.idx,
                            .tag_union => inner: {
                                const inner_tu = self.layout_store.getTagUnionData(err_layout.data.tag_union.idx);
                                const inner_v = self.layout_store.getTagUnionVariants(inner_tu);
                                if (inner_v.len == 0) break :inner null;
                                const inner_payload = inner_v.get(0).payload_layout;
                                const unwrapped = self.unwrapSingleFieldPayloadLayout(inner_payload) orelse inner_payload;
                                const inner_layout = self.layout_store.getLayout(unwrapped);
                                if (inner_layout.tag == .struct_) break :inner inner_layout.data.struct_.idx;
                                break :inner null;
                            },
                            else => null,
                        };
                    }
                }

                const val = try self.alloc(ll.ret_layout);
                @memset(val.ptr[0..tu_data.size], 0);

                const resolved_ok = ok_disc orelse return self.runtimeError("str_from_utf8: no Ok variant in layout");
                const resolved_err = err_disc orelse return self.runtimeError("str_from_utf8: no Err variant in layout");
                const rec_idx = err_record_idx orelse return self.runtimeError("str_from_utf8: could not resolve error record layout");

                if (result.is_ok) {
                    @memcpy(val.ptr[0..@sizeOf(RocStr)], std.mem.asBytes(&result.string));
                    self.helper.writeTagDiscriminant(val, ll.ret_layout, resolved_ok);
                } else {
                    const index_off = self.layout_store.getStructFieldOffsetByOriginalIndex(rec_idx, 0);
                    const problem_off = self.layout_store.getStructFieldOffsetByOriginalIndex(rec_idx, 1);
                    val.offset(index_off).write(u64, result.byte_index);
                    val.offset(problem_off).write(u8, @intFromEnum(result.problem_code));
                    self.helper.writeTagDiscriminant(val, ll.ret_layout, resolved_err);
                }
                break :blk val;
            },
            .str_from_utf8_lossy => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8Lossy(valueToRocList(args[0]), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_split_on => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.strSplitOn(valueToRocStr(args[0]), valueToRocStr(args[1]), &self.roc_ops);
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .str_join_with => self.evalStrJoinWith(args[0], args[1], ll.ret_layout),
            .str_with_capacity => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.withCapacityC(args[0].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_reserve => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.reserveC(valueToRocStr(args[0]), args[1].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_release_excess_capacity => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.strReleaseExcessCapacity(&self.roc_ops, valueToRocStr(args[0]));
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_inspect => blk: {
                // str_inspect is identity on strings (already formatted)
                break :blk args[0];
            },

            // ── Numeric to_str ops ──
            .u8_to_str => self.numToStr(u8, args[0], ll.ret_layout),
            .i8_to_str => self.numToStr(i8, args[0], ll.ret_layout),
            .u16_to_str => self.numToStr(u16, args[0], ll.ret_layout),
            .i16_to_str => self.numToStr(i16, args[0], ll.ret_layout),
            .u32_to_str => self.numToStr(u32, args[0], ll.ret_layout),
            .i32_to_str => self.numToStr(i32, args[0], ll.ret_layout),
            .u64_to_str => self.numToStr(u64, args[0], ll.ret_layout),
            .i64_to_str => self.numToStr(i64, args[0], ll.ret_layout),
            .u128_to_str => self.numToStr(u128, args[0], ll.ret_layout),
            .i128_to_str => self.numToStr(i128, args[0], ll.ret_layout),
            .dec_to_str => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.dec.to_str(dec, &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .f32_to_str => blk: {
                var buf: [400]u8 = undefined;
                const slice = i128h.f64_to_str(&buf, @as(f64, args[0].read(f32)));
                break :blk self.makeRocStr(slice);
            },
            .f64_to_str => blk: {
                var buf: [400]u8 = undefined;
                const slice = i128h.f64_to_str(&buf, args[0].read(f64));
                break :blk self.makeRocStr(slice);
            },
            .num_to_str => blk: {
                // Generic num_to_str uses arg layout to determine type
                const size = self.helper.sizeOf(arg_layout);
                const l = self.layout_store.getLayout(arg_layout);
                const is_float = l.tag == .scalar and l.data.scalar.tag == .frac;
                if (isDec(arg_layout)) {
                    const dec = RocDec{ .num = args[0].read(i128) };
                    self.roc_env.resetCrash();
                    const sj = setjmp(&self.roc_env.jmp_buf);
                    if (sj != 0) return error.Crash;
                    const result = builtins.dec.to_str(dec, &self.roc_ops);
                    break :blk self.rocStrToValue(result, ll.ret_layout);
                } else if (is_float) {
                    var buf: [400]u8 = undefined;
                    const slice = switch (size) {
                        4 => i128h.f64_to_str(&buf, @as(f64, args[0].read(f32))),
                        else => i128h.f64_to_str(&buf, args[0].read(f64)),
                    };
                    break :blk self.makeRocStr(slice);
                } else {
                    break :blk self.numToStrByLayout(args[0], arg_layout, ll.ret_layout);
                }
            },

            // ── List ops ──
            .list_len => blk: {
                const rl = valueToRocList(args[0]);
                const val = try self.alloc(ll.ret_layout);
                val.write(u64, @intCast(rl.len()));
                break :blk val;
            },
            .list_get_unsafe => blk: {
                const rl = valueToRocList(args[0]);
                const idx = args[1].read(u64);
                const info = self.listElemInfo(arg_layout);
                if (info.width == 0 or rl.bytes == null) break :blk try self.alloc(ll.ret_layout);
                const elem_ptr = rl.bytes.? + @as(usize, @intCast(idx)) * info.width;
                const val = try self.allocBytes(info.width);
                @memcpy(val.ptr[0..info.width], elem_ptr[0..info.width]);
                break :blk val;
            },
            .list_append_unsafe => blk: {
                // The Roc List.append function emits list_append_unsafe directly.
                // Use the safe listAppend which reserves capacity first,
                // matching the dev codegen (LirCodeGen) behavior.
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listAppend(
                    valueToRocList(args[0]),
                    info.alignment,
                    @ptrCast(args[1].ptr),
                    info.width,
                    info.rc,
                    null,
                    &builtins.utils.rcNone,
                    .InPlace,
                    &builtins.list.copy_fallback,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_concat => blk: {
                const info = self.listElemInfo(arg_layout);
                trace.log("list_concat: elem_width={d} align={d} rc={any}", .{ info.width, info.alignment, info.rc });
                if (info.width == 0) {
                    const list_a = valueToRocList(args[0]);
                    const list_b = valueToRocList(args[1]);
                    const total_len = list_a.len() + list_b.len();
                    const result = RocList{
                        .bytes = null,
                        .length = total_len,
                        .capacity_or_alloc_ptr = total_len,
                    };
                    break :blk self.rocListToValue(result, ll.ret_layout);
                }
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listConcat(
                    valueToRocList(args[0]),
                    valueToRocList(args[1]),
                    info.alignment,
                    info.width,
                    info.rc,
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_prepend => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const copy_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) void = &(struct {
                    fn f(_: ?[*]u8, _: ?[*]u8) callconv(.c) void {}
                }).f;
                const result = builtins.list.listPrepend(
                    valueToRocList(args[0]),
                    info.alignment,
                    @ptrCast(args[1].ptr),
                    info.width,
                    info.rc,
                    null,
                    &builtins.utils.rcNone,
                    copy_fn,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_sublist => blk: {
                if (arg_exprs.len != 2) {
                    return self.runtimeError("list_sublist expected 2 arguments");
                }

                const info = self.listElemInfo(arg_layout);
                const record_layout = self.exprLayout(arg_exprs[1]);
                const record_layout_val = self.layout_store.getLayout(record_layout);
                if (record_layout_val.tag != .struct_) {
                    return self.runtimeError("list_sublist expected a { start, len } record");
                }

                const record_idx = record_layout_val.data.struct_.idx;
                const len_field_off = self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 0);
                const start_field_off = self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 1);
                const start = args[1].offset(start_field_off).read(u64);
                const len = args[1].offset(len_field_off).read(u64);

                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listSublist(
                    valueToRocList(args[0]),
                    info.alignment,
                    info.width,
                    info.rc,
                    start,
                    len,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_drop_at => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listDropAt(
                    valueToRocList(args[0]),
                    info.alignment,
                    info.width,
                    info.rc,
                    args[1].read(u64),
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_set => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const copy_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) void = &(struct {
                    fn f(_: ?[*]u8, _: ?[*]u8) callconv(.c) void {}
                }).f;
                // listReplace writes old element into out_element
                const old_elem = try self.allocBytes(info.width);
                const result = builtins.list.listReplace(
                    valueToRocList(args[0]),
                    info.alignment,
                    args[1].read(u64),
                    @ptrCast(args[2].ptr),
                    info.width,
                    info.rc,
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    @ptrCast(old_elem.ptr),
                    copy_fn,
                    &self.roc_ops,
                );
                // ret_layout is a struct { list, old_element }
                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&result));
                @memcpy(val.ptr[@sizeOf(RocList)..][0..info.width], old_elem.ptr[0..info.width]);
                break :blk val;
            },
            .list_with_capacity => blk: {
                const elem_layout = self.listElemLayout(ll.ret_layout);
                const sa = self.helper.sizeAlignOf(elem_layout);
                const elems_rc = self.helper.containsRefcounted(elem_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listWithCapacity(
                    args[0].read(u64),
                    @intCast(sa.alignment.toByteUnits()),
                    sa.size,
                    elems_rc,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_reserve => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listReserve(
                    valueToRocList(args[0]),
                    info.alignment,
                    args[1].read(u64),
                    info.width,
                    info.rc,
                    null,
                    &builtins.utils.rcNone,
                    UpdateMode.Immutable,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_release_excess_capacity => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listReleaseExcessCapacity(
                    valueToRocList(args[0]),
                    info.alignment,
                    info.width,
                    info.rc,
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    UpdateMode.Immutable,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_first => self.evalListFirst(args[0], arg_layout, ll.ret_layout),
            .list_last => self.evalListLast(args[0], arg_layout, ll.ret_layout),
            .list_drop_first => self.evalListDropFirst(args[0], arg_layout, ll.ret_layout),
            .list_drop_last => self.evalListDropLast(args[0], arg_layout, ll.ret_layout),
            .list_take_first => self.evalListTakeFirst(args[0], args[1], arg_layout, ll.ret_layout),
            .list_take_last => self.evalListTakeLast(args[0], args[1], arg_layout, ll.ret_layout),
            .list_contains => self.evalListContains(args[0], args[1], arg_layout, ll.ret_layout),
            .list_reverse => self.evalListReverse(args[0], arg_layout, ll.ret_layout),
            .list_sort_with => blk: {
                break :blk try self.evalListSortWith(args[0], arg_layout, ll.ret_layout, ll.callable_proc);
            },
            .list_split_first => self.evalListSplitFirst(args[0], arg_layout, ll.ret_layout),
            .list_split_last => self.evalListSplitLast(args[0], arg_layout, ll.ret_layout),

            // ── Arithmetic ──
            .num_plus => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .add),
            .num_minus => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .sub),
            .num_times => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .mul),
            .num_div_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .div),
            .num_div_trunc_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .div_trunc),
            .num_rem_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .rem),
            .num_mod_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .mod),
            .num_negate => self.numUnaryOp(args[0], ll.ret_layout, arg_layout, .negate),
            .num_abs => self.numUnaryOp(args[0], ll.ret_layout, arg_layout, .abs),
            .num_abs_diff => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .abs_diff),
            .num_pow => self.evalNumPow(args[0], args[1], ll.ret_layout, arg_layout),
            .num_sqrt => self.evalNumSqrt(args[0], ll.ret_layout, arg_layout),
            .num_log => self.evalNumLog(args[0], ll.ret_layout, arg_layout),
            .num_round => self.evalNumRound(args[0], ll.ret_layout, arg_layout),
            .num_floor => self.evalNumFloor(args[0], ll.ret_layout, arg_layout),
            .num_ceiling => self.evalNumCeiling(args[0], ll.ret_layout, arg_layout),

            // ── Bitwise shifts ──
            .num_shift_left_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shl),
            .num_shift_right_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shr),
            .num_shift_right_zf_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shr_zf),

            // ── Comparison ──
            .num_is_eq => self.numCmpOp(args[0], args[1], arg_layout, .eq),
            .num_is_lt => self.numCmpOp(args[0], args[1], arg_layout, .lt),
            .num_is_lte => self.numCmpOp(args[0], args[1], arg_layout, .lte),
            .num_is_gt => self.numCmpOp(args[0], args[1], arg_layout, .gt),
            .num_is_gte => self.numCmpOp(args[0], args[1], arg_layout, .gte),
            .compare => self.evalCompare(args[0], args[1], arg_layout, ll.ret_layout),

            // ── Boolean ──
            .bool_not => blk: {
                const val = try self.alloc(.bool);
                val.write(u8, if (args[0].read(u8) == 0) 1 else 0);
                break :blk val;
            },

            // ── Numeric parsing ──
            .num_from_str => blk: {
                const ret_layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (ret_layout_val.tag != .tag_union) {
                    return self.runtimeError("num_from_str expected a tag union return layout");
                }

                const tu_data = self.layout_store.getTagUnionData(ret_layout_val.data.tag_union.idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                var payload_idx: ?layout_mod.Idx = null;
                for (0..variants.len) |i| {
                    const v_payload = variants.get(@intCast(i)).payload_layout;
                    const candidate_payload = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                    const payload_layout = self.layout_store.getLayout(candidate_payload);
                    switch (payload_layout.tag) {
                        .scalar => {
                            payload_idx = candidate_payload;
                            break;
                        },
                        else => {},
                    }
                    if (candidate_payload == .dec) {
                        payload_idx = candidate_payload;
                        break;
                    }
                }

                const ok_payload_idx = payload_idx orelse return self.runtimeError("num_from_str missing numeric payload layout");
                const result = try self.alloc(ll.ret_layout);
                const roc_str = valueToRocStr(args[0]);

                if (ok_payload_idx == .dec) {
                    dev_wrappers.roc_builtins_dec_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        tu_data.discriminant_offset,
                    );
                    break :blk result;
                }

                if (ok_payload_idx == .f32 or ok_payload_idx == .f64) {
                    dev_wrappers.roc_builtins_float_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        if (ok_payload_idx == .f32) 4 else 8,
                        tu_data.discriminant_offset,
                    );
                    break :blk result;
                }

                const int_width: u8 = switch (ok_payload_idx) {
                    .u8, .i8 => 1,
                    .u16, .i16 => 2,
                    .u32, .i32 => 4,
                    .u64, .i64 => 8,
                    .u128, .i128 => 16,
                    else => return self.runtimeError("num_from_str unsupported integer payload layout"),
                };
                const is_signed: bool = switch (ok_payload_idx) {
                    .i8, .i16, .i32, .i64, .i128 => true,
                    .u8, .u16, .u32, .u64, .u128 => false,
                    else => return self.runtimeError("num_from_str unsupported integer signedness"),
                };

                dev_wrappers.roc_builtins_int_from_str(
                    result.ptr,
                    roc_str.bytes,
                    roc_str.length,
                    roc_str.capacity_or_alloc_ptr,
                    int_width,
                    is_signed,
                    tu_data.discriminant_offset,
                );
                break :blk result;
            },
            .num_from_numeral => args[0], // identity

            // ── Numeric conversions ──
            .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128, .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128 => self.numWiden(u8, args[0], ll.ret_layout),
            .u8_to_f32, .u8_to_f64 => self.intToFloat(u8, args[0], ll.ret_layout),
            .u8_to_dec => self.intToDec(u8, args[0], ll.ret_layout),
            .u8_to_i8_wrap => self.numTruncate(u8, i8, args[0], ll.ret_layout),
            .u8_to_i8_try => self.numTry(u8, i8, args[0], ll.ret_layout),

            .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128 => self.numWiden(i8, args[0], ll.ret_layout),
            .i8_to_u8_wrap => self.numTruncate(i8, u8, args[0], ll.ret_layout),
            .i8_to_u8_try => self.numTry(i8, u8, args[0], ll.ret_layout),
            .i8_to_u16_wrap => self.numTruncateWiden(i8, i16, u16, args[0], ll.ret_layout),
            .i8_to_u16_try => self.numTry(i8, u16, args[0], ll.ret_layout),
            .i8_to_u32_wrap => self.numTruncateWiden(i8, i32, u32, args[0], ll.ret_layout),
            .i8_to_u32_try => self.numTry(i8, u32, args[0], ll.ret_layout),
            .i8_to_u64_wrap => self.numTruncateWiden(i8, i64, u64, args[0], ll.ret_layout),
            .i8_to_u64_try => self.numTry(i8, u64, args[0], ll.ret_layout),
            .i8_to_u128_wrap => self.numTruncateWiden(i8, i128, u128, args[0], ll.ret_layout),
            .i8_to_u128_try => self.numTry(i8, u128, args[0], ll.ret_layout),
            .i8_to_f32, .i8_to_f64 => self.intToFloat(i8, args[0], ll.ret_layout),
            .i8_to_dec => self.intToDec(i8, args[0], ll.ret_layout),

            .u16_to_i32, .u16_to_i64, .u16_to_i128, .u16_to_u32, .u16_to_u64, .u16_to_u128 => self.numWiden(u16, args[0], ll.ret_layout),
            .u16_to_i8_wrap => self.numTruncate(u16, i8, args[0], ll.ret_layout),
            .u16_to_i8_try => self.numTry(u16, i8, args[0], ll.ret_layout),
            .u16_to_i16_wrap => self.numTruncate(u16, i16, args[0], ll.ret_layout),
            .u16_to_i16_try => self.numTry(u16, i16, args[0], ll.ret_layout),
            .u16_to_u8_wrap => self.numTruncate(u16, u8, args[0], ll.ret_layout),
            .u16_to_u8_try => self.numTry(u16, u8, args[0], ll.ret_layout),
            .u16_to_f32, .u16_to_f64 => self.intToFloat(u16, args[0], ll.ret_layout),
            .u16_to_dec => self.intToDec(u16, args[0], ll.ret_layout),

            .i16_to_i32, .i16_to_i64, .i16_to_i128 => self.numWiden(i16, args[0], ll.ret_layout),
            .i16_to_i8_wrap => self.numTruncate(i16, i8, args[0], ll.ret_layout),
            .i16_to_i8_try => self.numTry(i16, i8, args[0], ll.ret_layout),
            .i16_to_u8_wrap => self.numTruncate(i16, u8, args[0], ll.ret_layout),
            .i16_to_u8_try => self.numTry(i16, u8, args[0], ll.ret_layout),
            .i16_to_u16_wrap => self.numTruncate(i16, u16, args[0], ll.ret_layout),
            .i16_to_u16_try => self.numTry(i16, u16, args[0], ll.ret_layout),
            .i16_to_u32_wrap => self.numTruncateWiden(i16, i32, u32, args[0], ll.ret_layout),
            .i16_to_u32_try => self.numTry(i16, u32, args[0], ll.ret_layout),
            .i16_to_u64_wrap => self.numTruncateWiden(i16, i64, u64, args[0], ll.ret_layout),
            .i16_to_u64_try => self.numTry(i16, u64, args[0], ll.ret_layout),
            .i16_to_u128_wrap => self.numTruncateWiden(i16, i128, u128, args[0], ll.ret_layout),
            .i16_to_u128_try => self.numTry(i16, u128, args[0], ll.ret_layout),
            .i16_to_f32, .i16_to_f64 => self.intToFloat(i16, args[0], ll.ret_layout),
            .i16_to_dec => self.intToDec(i16, args[0], ll.ret_layout),

            .u32_to_i64, .u32_to_i128, .u32_to_u64, .u32_to_u128 => self.numWiden(u32, args[0], ll.ret_layout),
            .u32_to_i8_wrap => self.numTruncate(u32, i8, args[0], ll.ret_layout),
            .u32_to_i8_try => self.numTry(u32, i8, args[0], ll.ret_layout),
            .u32_to_i16_wrap => self.numTruncate(u32, i16, args[0], ll.ret_layout),
            .u32_to_i16_try => self.numTry(u32, i16, args[0], ll.ret_layout),
            .u32_to_i32_wrap => self.numTruncate(u32, i32, args[0], ll.ret_layout),
            .u32_to_i32_try => self.numTry(u32, i32, args[0], ll.ret_layout),
            .u32_to_u8_wrap => self.numTruncate(u32, u8, args[0], ll.ret_layout),
            .u32_to_u8_try => self.numTry(u32, u8, args[0], ll.ret_layout),
            .u32_to_u16_wrap => self.numTruncate(u32, u16, args[0], ll.ret_layout),
            .u32_to_u16_try => self.numTry(u32, u16, args[0], ll.ret_layout),
            .u32_to_f32, .u32_to_f64 => self.intToFloat(u32, args[0], ll.ret_layout),
            .u32_to_dec => self.intToDec(u32, args[0], ll.ret_layout),

            .i32_to_i64, .i32_to_i128 => self.numWiden(i32, args[0], ll.ret_layout),
            .i32_to_i8_wrap => self.numTruncate(i32, i8, args[0], ll.ret_layout),
            .i32_to_i8_try => self.numTry(i32, i8, args[0], ll.ret_layout),
            .i32_to_i16_wrap => self.numTruncate(i32, i16, args[0], ll.ret_layout),
            .i32_to_i16_try => self.numTry(i32, i16, args[0], ll.ret_layout),
            .i32_to_u8_wrap => self.numTruncate(i32, u8, args[0], ll.ret_layout),
            .i32_to_u8_try => self.numTry(i32, u8, args[0], ll.ret_layout),
            .i32_to_u16_wrap => self.numTruncate(i32, u16, args[0], ll.ret_layout),
            .i32_to_u16_try => self.numTry(i32, u16, args[0], ll.ret_layout),
            .i32_to_u32_wrap => self.numTruncate(i32, u32, args[0], ll.ret_layout),
            .i32_to_u32_try => self.numTry(i32, u32, args[0], ll.ret_layout),
            .i32_to_u64_wrap => self.numTruncateWiden(i32, i64, u64, args[0], ll.ret_layout),
            .i32_to_u64_try => self.numTry(i32, u64, args[0], ll.ret_layout),
            .i32_to_u128_wrap => self.numTruncateWiden(i32, i128, u128, args[0], ll.ret_layout),
            .i32_to_u128_try => self.numTry(i32, u128, args[0], ll.ret_layout),
            .i32_to_f32, .i32_to_f64 => self.intToFloat(i32, args[0], ll.ret_layout),
            .i32_to_dec => self.intToDec(i32, args[0], ll.ret_layout),

            .u64_to_i128, .u64_to_u128 => self.numWiden(u64, args[0], ll.ret_layout),
            .u64_to_i8_wrap => self.numTruncate(u64, i8, args[0], ll.ret_layout),
            .u64_to_i8_try => self.numTry(u64, i8, args[0], ll.ret_layout),
            .u64_to_i16_wrap => self.numTruncate(u64, i16, args[0], ll.ret_layout),
            .u64_to_i16_try => self.numTry(u64, i16, args[0], ll.ret_layout),
            .u64_to_i32_wrap => self.numTruncate(u64, i32, args[0], ll.ret_layout),
            .u64_to_i32_try => self.numTry(u64, i32, args[0], ll.ret_layout),
            .u64_to_i64_wrap => self.numTruncate(u64, i64, args[0], ll.ret_layout),
            .u64_to_i64_try => self.numTry(u64, i64, args[0], ll.ret_layout),
            .u64_to_u8_wrap => self.numTruncate(u64, u8, args[0], ll.ret_layout),
            .u64_to_u8_try => self.numTry(u64, u8, args[0], ll.ret_layout),
            .u64_to_u16_wrap => self.numTruncate(u64, u16, args[0], ll.ret_layout),
            .u64_to_u16_try => self.numTry(u64, u16, args[0], ll.ret_layout),
            .u64_to_u32_wrap => self.numTruncate(u64, u32, args[0], ll.ret_layout),
            .u64_to_u32_try => self.numTry(u64, u32, args[0], ll.ret_layout),
            .u64_to_f32, .u64_to_f64 => self.intToFloat(u64, args[0], ll.ret_layout),
            .u64_to_dec => self.intToDec(u64, args[0], ll.ret_layout),

            .i64_to_i128 => self.numWiden(i64, args[0], ll.ret_layout),
            .i64_to_i8_wrap => self.numTruncate(i64, i8, args[0], ll.ret_layout),
            .i64_to_i8_try => self.numTry(i64, i8, args[0], ll.ret_layout),
            .i64_to_i16_wrap => self.numTruncate(i64, i16, args[0], ll.ret_layout),
            .i64_to_i16_try => self.numTry(i64, i16, args[0], ll.ret_layout),
            .i64_to_i32_wrap => self.numTruncate(i64, i32, args[0], ll.ret_layout),
            .i64_to_i32_try => self.numTry(i64, i32, args[0], ll.ret_layout),
            .i64_to_u8_wrap => self.numTruncate(i64, u8, args[0], ll.ret_layout),
            .i64_to_u8_try => self.numTry(i64, u8, args[0], ll.ret_layout),
            .i64_to_u16_wrap => self.numTruncate(i64, u16, args[0], ll.ret_layout),
            .i64_to_u16_try => self.numTry(i64, u16, args[0], ll.ret_layout),
            .i64_to_u32_wrap => self.numTruncate(i64, u32, args[0], ll.ret_layout),
            .i64_to_u32_try => self.numTry(i64, u32, args[0], ll.ret_layout),
            .i64_to_u64_wrap => self.numTruncate(i64, u64, args[0], ll.ret_layout),
            .i64_to_u64_try => self.numTry(i64, u64, args[0], ll.ret_layout),
            .i64_to_u128_wrap => self.numTruncateWiden(i64, i128, u128, args[0], ll.ret_layout),
            .i64_to_u128_try => self.numTry(i64, u128, args[0], ll.ret_layout),
            .i64_to_f32, .i64_to_f64 => self.intToFloat(i64, args[0], ll.ret_layout),
            .i64_to_dec => self.intToDec(i64, args[0], ll.ret_layout),

            .u128_to_i8_wrap => self.numTruncate(u128, i8, args[0], ll.ret_layout),
            .u128_to_i8_try => self.numTry(u128, i8, args[0], ll.ret_layout),
            .u128_to_i16_wrap => self.numTruncate(u128, i16, args[0], ll.ret_layout),
            .u128_to_i16_try => self.numTry(u128, i16, args[0], ll.ret_layout),
            .u128_to_i32_wrap => self.numTruncate(u128, i32, args[0], ll.ret_layout),
            .u128_to_i32_try => self.numTry(u128, i32, args[0], ll.ret_layout),
            .u128_to_i64_wrap => self.numTruncate(u128, i64, args[0], ll.ret_layout),
            .u128_to_i64_try => self.numTry(u128, i64, args[0], ll.ret_layout),
            .u128_to_i128_wrap => self.numTruncate(u128, i128, args[0], ll.ret_layout),
            .u128_to_i128_try => self.numTry(u128, i128, args[0], ll.ret_layout),
            .u128_to_u8_wrap => self.numTruncate(u128, u8, args[0], ll.ret_layout),
            .u128_to_u8_try => self.numTry(u128, u8, args[0], ll.ret_layout),
            .u128_to_u16_wrap => self.numTruncate(u128, u16, args[0], ll.ret_layout),
            .u128_to_u16_try => self.numTry(u128, u16, args[0], ll.ret_layout),
            .u128_to_u32_wrap => self.numTruncate(u128, u32, args[0], ll.ret_layout),
            .u128_to_u32_try => self.numTry(u128, u32, args[0], ll.ret_layout),
            .u128_to_u64_wrap => self.numTruncate(u128, u64, args[0], ll.ret_layout),
            .u128_to_u64_try => self.numTry(u128, u64, args[0], ll.ret_layout),
            .u128_to_f32, .u128_to_f64 => self.intToFloat(u128, args[0], ll.ret_layout),
            .u128_to_dec_try_unsafe => self.intToDec(u128, args[0], ll.ret_layout),

            .i128_to_i8_wrap => self.numTruncate(i128, i8, args[0], ll.ret_layout),
            .i128_to_i8_try => self.numTry(i128, i8, args[0], ll.ret_layout),
            .i128_to_i16_wrap => self.numTruncate(i128, i16, args[0], ll.ret_layout),
            .i128_to_i16_try => self.numTry(i128, i16, args[0], ll.ret_layout),
            .i128_to_i32_wrap => self.numTruncate(i128, i32, args[0], ll.ret_layout),
            .i128_to_i32_try => self.numTry(i128, i32, args[0], ll.ret_layout),
            .i128_to_i64_wrap => self.numTruncate(i128, i64, args[0], ll.ret_layout),
            .i128_to_i64_try => self.numTry(i128, i64, args[0], ll.ret_layout),
            .i128_to_u8_wrap => self.numTruncate(i128, u8, args[0], ll.ret_layout),
            .i128_to_u8_try => self.numTry(i128, u8, args[0], ll.ret_layout),
            .i128_to_u16_wrap => self.numTruncate(i128, u16, args[0], ll.ret_layout),
            .i128_to_u16_try => self.numTry(i128, u16, args[0], ll.ret_layout),
            .i128_to_u32_wrap => self.numTruncate(i128, u32, args[0], ll.ret_layout),
            .i128_to_u32_try => self.numTry(i128, u32, args[0], ll.ret_layout),
            .i128_to_u64_wrap => self.numTruncate(i128, u64, args[0], ll.ret_layout),
            .i128_to_u64_try => self.numTry(i128, u64, args[0], ll.ret_layout),
            .i128_to_u128_wrap => self.numTruncate(i128, u128, args[0], ll.ret_layout),
            .i128_to_u128_try => self.numTry(i128, u128, args[0], ll.ret_layout),
            .i128_to_f32, .i128_to_f64 => self.intToFloat(i128, args[0], ll.ret_layout),
            .i128_to_dec_try_unsafe => self.intToDec(i128, args[0], ll.ret_layout),

            // Float → int (truncating)
            .f32_to_i8_trunc => self.floatToInt(f32, i8, args[0], ll.ret_layout),
            .f32_to_i16_trunc => self.floatToInt(f32, i16, args[0], ll.ret_layout),
            .f32_to_i32_trunc => self.floatToInt(f32, i32, args[0], ll.ret_layout),
            .f32_to_i64_trunc => self.floatToInt(f32, i64, args[0], ll.ret_layout),
            .f32_to_i128_trunc => self.floatToInt(f32, i128, args[0], ll.ret_layout),
            .f32_to_u8_trunc => self.floatToInt(f32, u8, args[0], ll.ret_layout),
            .f32_to_u16_trunc => self.floatToInt(f32, u16, args[0], ll.ret_layout),
            .f32_to_u32_trunc => self.floatToInt(f32, u32, args[0], ll.ret_layout),
            .f32_to_u64_trunc => self.floatToInt(f32, u64, args[0], ll.ret_layout),
            .f32_to_u128_trunc => self.floatToInt(f32, u128, args[0], ll.ret_layout),
            .f32_to_f64 => self.floatWiden(f32, f64, args[0], ll.ret_layout),
            // Float → int (try)
            .f32_to_i8_try_unsafe => self.floatToIntTry(f32, i8, args[0], ll.ret_layout),
            .f32_to_i16_try_unsafe => self.floatToIntTry(f32, i16, args[0], ll.ret_layout),
            .f32_to_i32_try_unsafe => self.floatToIntTry(f32, i32, args[0], ll.ret_layout),
            .f32_to_i64_try_unsafe => self.floatToIntTry(f32, i64, args[0], ll.ret_layout),
            .f32_to_i128_try_unsafe => self.floatToIntTry(f32, i128, args[0], ll.ret_layout),
            .f32_to_u8_try_unsafe => self.floatToIntTry(f32, u8, args[0], ll.ret_layout),
            .f32_to_u16_try_unsafe => self.floatToIntTry(f32, u16, args[0], ll.ret_layout),
            .f32_to_u32_try_unsafe => self.floatToIntTry(f32, u32, args[0], ll.ret_layout),
            .f32_to_u64_try_unsafe => self.floatToIntTry(f32, u64, args[0], ll.ret_layout),
            .f32_to_u128_try_unsafe => self.floatToIntTry(f32, u128, args[0], ll.ret_layout),

            .f64_to_i8_trunc => self.floatToInt(f64, i8, args[0], ll.ret_layout),
            .f64_to_i16_trunc => self.floatToInt(f64, i16, args[0], ll.ret_layout),
            .f64_to_i32_trunc => self.floatToInt(f64, i32, args[0], ll.ret_layout),
            .f64_to_i64_trunc => self.floatToInt(f64, i64, args[0], ll.ret_layout),
            .f64_to_i128_trunc => self.floatToInt(f64, i128, args[0], ll.ret_layout),
            .f64_to_u8_trunc => self.floatToInt(f64, u8, args[0], ll.ret_layout),
            .f64_to_u16_trunc => self.floatToInt(f64, u16, args[0], ll.ret_layout),
            .f64_to_u32_trunc => self.floatToInt(f64, u32, args[0], ll.ret_layout),
            .f64_to_u64_trunc => self.floatToInt(f64, u64, args[0], ll.ret_layout),
            .f64_to_u128_trunc => self.floatToInt(f64, u128, args[0], ll.ret_layout),
            .f64_to_f32_wrap => self.floatNarrow(f64, f32, args[0], ll.ret_layout),
            .f64_to_i8_try_unsafe => self.floatToIntTry(f64, i8, args[0], ll.ret_layout),
            .f64_to_i16_try_unsafe => self.floatToIntTry(f64, i16, args[0], ll.ret_layout),
            .f64_to_i32_try_unsafe => self.floatToIntTry(f64, i32, args[0], ll.ret_layout),
            .f64_to_i64_try_unsafe => self.floatToIntTry(f64, i64, args[0], ll.ret_layout),
            .f64_to_i128_try_unsafe => self.floatToIntTry(f64, i128, args[0], ll.ret_layout),
            .f64_to_u8_try_unsafe => self.floatToIntTry(f64, u8, args[0], ll.ret_layout),
            .f64_to_u16_try_unsafe => self.floatToIntTry(f64, u16, args[0], ll.ret_layout),
            .f64_to_u32_try_unsafe => self.floatToIntTry(f64, u32, args[0], ll.ret_layout),
            .f64_to_u64_try_unsafe => self.floatToIntTry(f64, u64, args[0], ll.ret_layout),
            .f64_to_u128_try_unsafe => self.floatToIntTry(f64, u128, args[0], ll.ret_layout),
            .f64_to_f32_try_unsafe => blk: {
                const sv = args[0].read(f64);
                const val = try self.alloc(ll.ret_layout);
                if (!std.math.isNan(sv) and !std.math.isInf(sv) and
                    sv <= std.math.floatMax(f32) and sv >= -std.math.floatMax(f32))
                {
                    val.write(f32, @floatCast(sv));
                    val.offset(4).write(u8, 1);
                } else {
                    val.offset(4).write(u8, 0);
                }
                break :blk val;
            },

            // Dec → numeric
            .dec_to_i8_trunc => self.decToInt(i8, args[0], ll.ret_layout),
            .dec_to_i16_trunc => self.decToInt(i16, args[0], ll.ret_layout),
            .dec_to_i32_trunc => self.decToInt(i32, args[0], ll.ret_layout),
            .dec_to_i64_trunc => self.decToInt(i64, args[0], ll.ret_layout),
            .dec_to_i128_trunc => self.decToInt(i128, args[0], ll.ret_layout),
            .dec_to_u8_trunc => self.decToInt(u8, args[0], ll.ret_layout),
            .dec_to_u16_trunc => self.decToInt(u16, args[0], ll.ret_layout),
            .dec_to_u32_trunc => self.decToInt(u32, args[0], ll.ret_layout),
            .dec_to_u64_trunc => self.decToInt(u64, args[0], ll.ret_layout),
            .dec_to_u128_trunc => self.decToInt(u128, args[0], ll.ret_layout),
            .dec_to_i8_try_unsafe => self.decToIntTry(i8, args[0], ll.ret_layout),
            .dec_to_i16_try_unsafe => self.decToIntTry(i16, args[0], ll.ret_layout),
            .dec_to_i32_try_unsafe => self.decToIntTry(i32, args[0], ll.ret_layout),
            .dec_to_i64_try_unsafe => self.decToIntTry(i64, args[0], ll.ret_layout),
            .dec_to_i128_try_unsafe => self.decToIntTry(i128, args[0], ll.ret_layout),
            .dec_to_u8_try_unsafe => self.decToIntTry(u8, args[0], ll.ret_layout),
            .dec_to_u16_try_unsafe => self.decToIntTry(u16, args[0], ll.ret_layout),
            .dec_to_u32_try_unsafe => self.decToIntTry(u32, args[0], ll.ret_layout),
            .dec_to_u64_try_unsafe => self.decToIntTry(u64, args[0], ll.ret_layout),
            .dec_to_u128_try_unsafe => self.decToIntTry(u128, args[0], ll.ret_layout),
            .dec_to_f32_wrap => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                const val = try self.alloc(ll.ret_layout);
                val.write(f32, @floatCast(dec.toF64()));
                break :blk val;
            },
            .dec_to_f32_try_unsafe => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                const val = try self.alloc(ll.ret_layout);
                if (builtins.dec.toF32Try(dec)) |f| {
                    val.write(f32, f);
                    val.offset(4).write(u8, 1); // is_ok
                } else {
                    val.write(f32, 0);
                    val.offset(4).write(u8, 0);
                }
                break :blk val;
            },
            .dec_to_f64 => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                const val = try self.alloc(ll.ret_layout);
                val.write(f64, dec.toF64());
                break :blk val;
            },

            // ── Box ops ──
            .box_box => try self.evalBoxBox(args[0], ll.ret_layout),
            .box_unbox => try self.evalBoxUnbox(args[0], ll.ret_layout),

            // ── Crash ──
            .crash => return error.Crash,
        };
    }

    const NumOp = enum { add, sub, mul, div, div_trunc, rem, mod, negate, abs, abs_diff };
    const CmpOp = enum { eq, lt, lte, gt, gte };
    const ShiftOp = enum { shl, shr, shr_zf };

    /// Determine if a layout index represents a Dec type.
    fn isDec(layout_idx: layout_mod.Idx) bool {
        return layout_idx == .dec;
    }

    /// Determine if a layout index represents an unsigned integer.
    fn isUnsigned(layout_idx: layout_mod.Idx) bool {
        return switch (layout_idx) {
            .u8, .u16, .u32, .u64, .u128 => true,
            else => false,
        };
    }

    fn numBinOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: NumOp) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        const is_division_like = op == .div or op == .div_trunc or op == .rem or op == .mod;

        if (is_division_like) {
            switch (size) {
                1 => {
                    if (isUnsigned(arg_layout)) {
                        if (b.read(u8) == 0) return self.divisionByZero();
                    } else if (b.read(i8) == 0) return self.divisionByZero();
                },
                2 => {
                    if (isUnsigned(arg_layout)) {
                        if (b.read(u16) == 0) return self.divisionByZero();
                    } else if (b.read(i16) == 0) return self.divisionByZero();
                },
                4 => {
                    const l = self.layout_store.getLayout(arg_layout);
                    if (!(l.tag == .scalar and l.data.scalar.tag == .frac)) {
                        if (isUnsigned(arg_layout)) {
                            if (b.read(u32) == 0) return self.divisionByZero();
                        } else if (b.read(i32) == 0) return self.divisionByZero();
                    }
                },
                8 => {
                    const l = self.layout_store.getLayout(arg_layout);
                    if (!(l.tag == .scalar and l.data.scalar.tag == .frac)) {
                        if (isUnsigned(arg_layout)) {
                            if (b.read(u64) == 0) return self.divisionByZero();
                        } else if (b.read(i64) == 0) return self.divisionByZero();
                    }
                },
                16 => {
                    if (isDec(arg_layout)) {
                        if (b.read(i128) == 0) return self.divisionByZero();
                    } else if (isUnsigned(arg_layout)) {
                        if (b.read(u128) == 0) return self.divisionByZero();
                    } else if (b.read(i128) == 0) return self.divisionByZero();
                },
                else => {},
            }
        }

        switch (size) {
            1 => {
                if (isUnsigned(arg_layout)) {
                    val.write(u8, intBinOp(u8, a.read(u8), b.read(u8), op));
                } else {
                    val.write(i8, intBinOp(i8, a.read(i8), b.read(i8), op));
                }
            },
            2 => {
                if (isUnsigned(arg_layout)) {
                    val.write(u16, intBinOp(u16, a.read(u16), b.read(u16), op));
                } else {
                    val.write(i16, intBinOp(i16, a.read(i16), b.read(i16), op));
                }
            },
            4 => {
                const l = self.layout_store.getLayout(arg_layout);
                if (l.tag == .scalar and l.data.scalar.tag == .frac) {
                    val.write(f32, floatBinOp(f32, a.read(f32), b.read(f32), op));
                } else if (isUnsigned(arg_layout)) {
                    val.write(u32, intBinOp(u32, a.read(u32), b.read(u32), op));
                } else {
                    val.write(i32, intBinOp(i32, a.read(i32), b.read(i32), op));
                }
            },
            8 => {
                const l = self.layout_store.getLayout(arg_layout);
                if (l.tag == .scalar and l.data.scalar.tag == .frac) {
                    val.write(f64, floatBinOp(f64, a.read(f64), b.read(f64), op));
                } else if (isUnsigned(arg_layout)) {
                    val.write(u64, intBinOp(u64, a.read(u64), b.read(u64), op));
                } else {
                    val.write(i64, intBinOp(i64, a.read(i64), b.read(i64), op));
                }
            },
            16 => {
                if (isDec(arg_layout)) {
                    val.write(i128, self.decBinOp(a.read(i128), b.read(i128), op));
                } else if (isUnsigned(arg_layout)) {
                    val.write(u128, intBinOp(u128, a.read(u128), b.read(u128), op));
                } else {
                    val.write(i128, intBinOp(i128, a.read(i128), b.read(i128), op));
                }
            },
            else => {},
        }
        return val;
    }

    fn numUnaryOp(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: NumOp) Error!Value {
        return self.numBinOp(a, a, ret_layout, arg_layout, op);
    }

    fn numCmpOp(self: *LirInterpreter, a: Value, b: Value, arg_layout: layout_mod.Idx, op: CmpOp) Error!Value {
        const val = try self.alloc(.bool);
        const size = self.helper.sizeOf(arg_layout);

        const result: bool = switch (size) {
            1 => if (isUnsigned(arg_layout))
                cmpOp(u8, a.read(u8), b.read(u8), op)
            else
                cmpOp(i8, a.read(i8), b.read(i8), op),
            2 => if (isUnsigned(arg_layout))
                cmpOp(u16, a.read(u16), b.read(u16), op)
            else
                cmpOp(i16, a.read(i16), b.read(i16), op),
            4 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOp(f32, a.read(f32), b.read(f32), op)
                else if (isUnsigned(arg_layout))
                    cmpOp(u32, a.read(u32), b.read(u32), op)
                else
                    cmpOp(i32, a.read(i32), b.read(i32), op);
            },
            8 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOp(f64, a.read(f64), b.read(f64), op)
                else if (isUnsigned(arg_layout))
                    cmpOp(u64, a.read(u64), b.read(u64), op)
                else
                    cmpOp(i64, a.read(i64), b.read(i64), op);
            },
            16 => if (isUnsigned(arg_layout))
                cmpOp(u128, a.read(u128), b.read(u128), op)
            else
                cmpOp(i128, a.read(i128), b.read(i128), op),
            else => false,
        };
        val.write(u8, if (result) 1 else 0);
        return val;
    }

    fn evalCompare(self: *LirInterpreter, a: Value, b: Value, arg_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        // Returns 0=LT, 1=EQ, 2=GT
        const result: u8 = switch (size) {
            1 => if (isUnsigned(arg_layout))
                cmpOrder(u8, a.read(u8), b.read(u8))
            else
                cmpOrder(i8, a.read(i8), b.read(i8)),
            2 => if (isUnsigned(arg_layout))
                cmpOrder(u16, a.read(u16), b.read(u16))
            else
                cmpOrder(i16, a.read(i16), b.read(i16)),
            4 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOrder(f32, a.read(f32), b.read(f32))
                else if (isUnsigned(arg_layout))
                    cmpOrder(u32, a.read(u32), b.read(u32))
                else
                    cmpOrder(i32, a.read(i32), b.read(i32));
            },
            8 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOrder(f64, a.read(f64), b.read(f64))
                else if (isUnsigned(arg_layout))
                    cmpOrder(u64, a.read(u64), b.read(u64))
                else
                    cmpOrder(i64, a.read(i64), b.read(i64));
            },
            16 => if (isUnsigned(arg_layout))
                cmpOrder(u128, a.read(u128), b.read(u128))
            else
                cmpOrder(i128, a.read(i128), b.read(i128)),
            else => 1, // EQ as default
        };
        val.write(u8, result);
        return val;
    }

    fn numShiftOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: ShiftOp) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        switch (size) {
            1 => if (isUnsigned(arg_layout))
                val.write(u8, shiftOp(u8, a.read(u8), b.read(u8), op))
            else
                val.write(i8, shiftOp(i8, a.read(i8), b.read(u8), op)),
            2 => if (isUnsigned(arg_layout))
                val.write(u16, shiftOp(u16, a.read(u16), b.read(u8), op))
            else
                val.write(i16, shiftOp(i16, a.read(i16), b.read(u8), op)),
            4 => if (isUnsigned(arg_layout))
                val.write(u32, shiftOp(u32, a.read(u32), b.read(u8), op))
            else
                val.write(i32, shiftOp(i32, a.read(i32), b.read(u8), op)),
            8 => if (isUnsigned(arg_layout))
                val.write(u64, shiftOp(u64, a.read(u64), b.read(u8), op))
            else
                val.write(i64, shiftOp(i64, a.read(i64), b.read(u8), op)),
            16 => if (isUnsigned(arg_layout))
                val.write(u128, shiftOp(u128, a.read(u128), b.read(u8), op))
            else
                val.write(i128, shiftOp(i128, a.read(i128), b.read(u8), op)),
            else => {},
        }
        return val;
    }

    fn evalNumPow(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        const l = self.layout_store.getLayout(arg_layout);
        if (isDec(arg_layout)) {
            self.roc_env.resetCrash();
            const sj = setjmp(&self.roc_env.jmp_buf);
            if (sj != 0) return error.Crash;
            val.write(i128, builtins.dec.powC(RocDec{ .num = a.read(i128) }, RocDec{ .num = b.read(i128) }, &self.roc_ops));
        } else if (l.tag == .scalar and l.data.scalar.tag == .frac) {
            if (size == 4)
                val.write(f32, std.math.pow(f32, a.read(f32), b.read(f32)))
            else
                val.write(f64, std.math.pow(f64, a.read(f64), b.read(f64)));
        } else {
            // Integer power — use wrapping multiply loop
            val.write(i128, intPow(a.read(i128), b.read(i128)));
        }
        return val;
    }

    fn evalNumSqrt(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            // Dec sqrt: convert to f64, sqrt, convert back
            const dec = RocDec{ .num = a.read(i128) };
            const f = @sqrt(dec.toF64());
            val.write(i128, (RocDec{ .num = builtins.dec.fromF64C(f, &self.roc_ops) }).num);
        } else if (size == 4)
            val.write(f32, @sqrt(a.read(f32)))
        else
            val.write(f64, @sqrt(a.read(f64)));
        return val;
    }

    fn evalNumLog(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            val.write(i128, builtins.dec.logC(RocDec{ .num = a.read(i128) }));
        } else if (size == 4)
            val.write(f32, @log(a.read(f32)))
        else
            val.write(f64, @log(a.read(f64)));
        return val;
    }

    fn evalNumRound(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            // Dec round: divide by scale, round
            const dec = RocDec{ .num = a.read(i128) };
            const f = @round(dec.toF64());
            val.write(i128, @as(i128, @intFromFloat(f)));
        } else if (size == 4)
            val.write(i32, @as(i32, @intFromFloat(@round(a.read(f32)))))
        else
            val.write(i64, @as(i64, @intFromFloat(@round(a.read(f64)))));
        return val;
    }

    fn evalNumFloor(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            const dec = RocDec{ .num = a.read(i128) };
            const f = @floor(dec.toF64());
            val.write(i128, @as(i128, @intFromFloat(f)));
        } else if (size == 4)
            val.write(i32, @as(i32, @intFromFloat(@floor(a.read(f32)))))
        else
            val.write(i64, @as(i64, @intFromFloat(@floor(a.read(f64)))));
        return val;
    }

    fn evalNumCeiling(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            const dec = RocDec{ .num = a.read(i128) };
            const f = @ceil(dec.toF64());
            val.write(i128, @as(i128, @intFromFloat(f)));
        } else if (size == 4)
            val.write(i32, @as(i32, @intFromFloat(@ceil(a.read(f32)))))
        else
            val.write(i64, @as(i64, @intFromFloat(@ceil(a.read(f64)))));
        return val;
    }

    // ── Numeric conversion helpers ──

    fn numWiden(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const ret_size = self.helper.sizeOf(ret_layout);
        const sv = arg.read(Src);
        switch (ret_size) {
            1 => val.write(if (@typeInfo(Src).int.signedness == .signed) i8 else u8, @intCast(sv)),
            2 => val.write(if (@typeInfo(Src).int.signedness == .signed) i16 else u16, @intCast(sv)),
            4 => val.write(if (@typeInfo(Src).int.signedness == .signed) i32 else u32, @intCast(sv)),
            8 => val.write(if (@typeInfo(Src).int.signedness == .signed) i64 else u64, @intCast(sv)),
            16 => val.write(if (@typeInfo(Src).int.signedness == .signed) i128 else u128, @intCast(sv)),
            else => {},
        }
        return val;
    }

    fn numTruncate(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        // Truncate to same-width as Dst, then bitcast if signedness differs
        const DstBits = @typeInfo(Dst).int.bits;
        std.debug.assert(@typeInfo(Src).int.bits >= DstBits);
        const SameSigned = std.meta.Int(@typeInfo(Src).int.signedness, DstBits);
        const truncated: SameSigned = @truncate(sv);
        val.write(Dst, @bitCast(truncated));
        return val;
    }

    fn numTruncateWiden(self: *LirInterpreter, comptime Src: type, comptime Mid: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const mid: Mid = @intCast(arg.read(Src));
        val.write(Dst, @bitCast(mid));
        return val;
    }

    fn numTry(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        const dst_size = @sizeOf(Dst);
        if (std.math.cast(Dst, sv)) |dv| {
            val.write(Dst, dv);
            val.offset(dst_size).write(u8, 1); // is_ok = true
        } else {
            val.offset(dst_size).write(u8, 0); // is_ok = false
        }
        return val;
    }

    fn intToFloat(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const ret_size = self.helper.sizeOf(ret_layout);
        const sv = arg.read(Src);
        if (ret_size == 4)
            val.write(f32, @floatFromInt(sv))
        else
            val.write(f64, @floatFromInt(sv));
        return val;
    }

    fn intToDec(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        const scale: i128 = 1_000_000_000_000_000_000; // 10^18
        val.write(i128, @as(i128, @intCast(sv)) *% scale);
        return val;
    }

    fn floatToInt(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        if (std.math.isNan(sv) or std.math.isInf(sv)) {
            val.write(Dst, 0);
        } else {
            val.write(Dst, @intFromFloat(sv));
        }
        return val;
    }

    fn floatToIntTry(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        const dst_size = @sizeOf(Dst);
        const min_val = comptime @as(Src, @floatFromInt(std.math.minInt(Dst)));
        const max_val = comptime @as(Src, @floatFromInt(std.math.maxInt(Dst)));
        if (!std.math.isNan(sv) and !std.math.isInf(sv)) {
            const truncated: Src = @trunc(sv);
            if (truncated >= min_val and truncated <= max_val) {
                val.write(Dst, @intFromFloat(truncated));
                val.offset(dst_size).write(u8, 1);
                return val;
            }
        }
        val.offset(dst_size).write(u8, 0);
        return val;
    }

    fn floatWiden(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        val.write(Dst, @as(Dst, arg.read(Src)));
        return val;
    }

    fn floatNarrow(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        val.write(Dst, @floatCast(arg.read(Src)));
        return val;
    }

    fn decToInt(self: *LirInterpreter, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const dec = RocDec{ .num = arg.read(i128) };
        val.write(Dst, builtins.dec.toIntWrap(Dst, dec));
        return val;
    }

    fn decToIntTry(self: *LirInterpreter, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const dec = RocDec{ .num = arg.read(i128) };
        const dst_size = @sizeOf(Dst);
        if (builtins.dec.toIntTry(Dst, dec)) |dv| {
            val.write(Dst, dv);
            val.offset(dst_size).write(u8, 1);
        } else {
            val.offset(dst_size).write(u8, 0);
        }
        return val;
    }

    fn numToStr(self: *LirInterpreter, comptime T: type, arg: Value, _: layout_mod.Idx) Error!Value {
        const arena = self.arena.allocator();
        const formatted = std.fmt.allocPrint(arena, "{d}", .{arg.read(T)}) catch return error.OutOfMemory;
        return try self.makeRocStr(formatted);
    }

    fn numToStrByLayout(self: *LirInterpreter, arg: Value, arg_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const size = self.helper.sizeOf(arg_layout);
        return switch (size) {
            1 => if (isUnsigned(arg_layout)) self.numToStr(u8, arg, ret_layout) else self.numToStr(i8, arg, ret_layout),
            2 => if (isUnsigned(arg_layout)) self.numToStr(u16, arg, ret_layout) else self.numToStr(i16, arg, ret_layout),
            4 => if (isUnsigned(arg_layout)) self.numToStr(u32, arg, ret_layout) else self.numToStr(i32, arg, ret_layout),
            8 => if (isUnsigned(arg_layout)) self.numToStr(u64, arg, ret_layout) else self.numToStr(i64, arg, ret_layout),
            16 => if (isUnsigned(arg_layout)) self.numToStr(u128, arg, ret_layout) else self.numToStr(i128, arg, ret_layout),
            else => self.makeRocStr("0"),
        };
    }

    // ── List operation helpers ──

    fn evalListFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            // Result tag union: payload at 0, discriminant after
            @memcpy(val.ptr[0..info.width], rl.bytes.?[0..info.width]);
            self.helper.writeTagDiscriminant(val, ret_layout, 1); // Ok tag
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0); // Err tag
        }
        return val;
    }

    fn evalListLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            const last_offset = (rl.len() - 1) * info.width;
            @memcpy(val.ptr[0..info.width], rl.bytes.?[last_offset..][0..info.width]);
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    fn evalListDropFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const info = self.listElemInfo(list_layout);
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            valueToRocList(list_arg),
            info.alignment,
            info.width,
            info.rc,
            1,
            std.math.maxInt(u64),
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListDropLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const len = rl.len();
        if (len == 0) return self.rocListToValue(rl, ret_layout);
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            info.rc,
            0,
            len - 1,
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeFirst(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const info = self.listElemInfo(list_layout);
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            valueToRocList(list_arg),
            info.alignment,
            info.width,
            info.rc,
            0,
            count_arg.read(u64),
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeLast(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const len = rl.len();
        const take = count_arg.read(u64);
        const start = if (take >= len) 0 else len - @as(usize, @intCast(take));
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            info.rc,
            @intCast(start),
            take,
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListContains(self: *LirInterpreter, list_arg: Value, elem_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        var found = false;
        if (rl.bytes != null and info.width > 0) {
            for (0..rl.len()) |i| {
                const elem_ptr = rl.bytes.? + i * info.width;
                if (rawBytesEqual(elem_ptr[0..info.width], elem_arg.ptr[0..info.width])) {
                    found = true;
                    break;
                }
            }
        }
        val.write(u8, if (found) 1 else 0);
        return val;
    }

    fn evalListReverse(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        if (rl.len() <= 1 or rl.bytes == null or info.width == 0)
            return self.rocListToValue(rl, ret_layout);
        // Clone and reverse in-place
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const new_list = builtins.list.shallowClone(rl, rl.len(), info.width, info.alignment, info.rc, &self.roc_ops);
        if (new_list.bytes) |bytes| {
            var lo: usize = 0;
            var hi: usize = new_list.len() - 1;
            const tmp = self.arena.allocator().alloc(u8, info.width) catch return error.OutOfMemory;
            while (lo < hi) {
                @memcpy(tmp, bytes[lo * info.width ..][0..info.width]);
                @memcpy(bytes[lo * info.width ..][0..info.width], bytes[hi * info.width ..][0..info.width]);
                @memcpy(bytes[hi * info.width ..][0..info.width], tmp);
                lo += 1;
                hi -= 1;
            }
        }
        return self.rocListToValue(new_list, ret_layout);
    }

    fn evalListSortWith(self: *LirInterpreter, list_val: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, comparator_proc_id: LirProcSpecId) Error!Value {
        const rl = valueToRocList(list_val);
        const info = self.listElemInfo(list_layout);
        const list_len = rl.len();

        if (list_len < 2 or rl.bytes == null or info.width == 0)
            return self.rocListToValue(rl, ret_layout);

        if (comparator_proc_id.isNone()) return error.RuntimeError;

        // Look up the comparator proc spec
        const comparator = self.store.getProcSpec(comparator_proc_id);

        // Clone the list data for in-place sorting
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const new_list = builtins.list.shallowClone(rl, rl.len(), info.width, info.alignment, info.rc, &self.roc_ops);
        const sorted_bytes = new_list.bytes orelse return self.rocListToValue(new_list, ret_layout);

        // Insertion sort using the comparator proc
        const tmp = self.arena.allocator().alloc(u8, info.width) catch return error.OutOfMemory;

        var i: usize = 1;
        while (i < list_len) : (i += 1) {
            // Save element[i] to temp
            @memcpy(tmp, sorted_bytes[i * info.width ..][0..info.width]);
            const temp_val = Value{ .ptr = tmp.ptr };

            // Shift elements right until we find the insertion point
            var j: usize = i;
            while (j > 0) {
                const elem_prev = Value{ .ptr = sorted_bytes + (j - 1) * info.width };

                // Call comparator(temp, elem[j-1])
                const call_args = [2]Value{ temp_val, elem_prev };
                const result = try self.evalProcStackSafe(comparator, &call_args);
                const cmp_val = switch (result) {
                    .value => |v| v,
                    else => return error.RuntimeError,
                };

                // Tag discriminants (alphabetical): EQ=0, GT=1, LT=2
                const disc = cmp_val.read(u8);
                if (disc != 2) break; // not LT, stop shifting

                // Shift element[j-1] to element[j]
                @memcpy(sorted_bytes[j * info.width ..][0..info.width], sorted_bytes[(j - 1) * info.width ..][0..info.width]);
                j -= 1;
            }
            // Insert temp at position j
            @memcpy(sorted_bytes[j * info.width ..][0..info.width], tmp);
        }

        return self.rocListToValue(new_list, ret_layout);
    }

    fn evalListSplitFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            // Ok: { first_elem, rest_list }
            @memcpy(val.ptr[0..info.width], rl.bytes.?[0..info.width]);
            // Rest list starts at offset info.width
            self.roc_env.resetCrash();
            const sj = setjmp(&self.roc_env.jmp_buf);
            if (sj != 0) return error.Crash;
            const rest = builtins.list.listSublist(
                rl,
                info.alignment,
                info.width,
                info.rc,
                1,
                std.math.maxInt(u64),
                null,
                &builtins.utils.rcNone,
                &self.roc_ops,
            );
            // Write rest list after the element, aligned to list alignment
            const list_offset = std.mem.alignForward(usize, info.width, @alignOf(RocList));
            @memcpy(val.ptr[list_offset..][0..@sizeOf(RocList)], std.mem.asBytes(&rest));
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    fn evalListSplitLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            // Ok: { last_elem, rest_list }
            const last_offset = (rl.len() - 1) * info.width;
            @memcpy(val.ptr[0..info.width], rl.bytes.?[last_offset..][0..info.width]);
            self.roc_env.resetCrash();
            const sj = setjmp(&self.roc_env.jmp_buf);
            if (sj != 0) return error.Crash;
            const rest = builtins.list.listSublist(
                rl,
                info.alignment,
                info.width,
                info.rc,
                0,
                rl.len() - 1,
                null,
                &builtins.utils.rcNone,
                &self.roc_ops,
            );
            const list_offset = std.mem.alignForward(usize, info.width, @alignOf(RocList));
            @memcpy(val.ptr[list_offset..][0..@sizeOf(RocList)], std.mem.asBytes(&rest));
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    /// Generic integer binary operation.
    fn intBinOp(comptime T: type, av: T, bv: T, op: NumOp) T {
        return switch (op) {
            .add => av +% bv,
            .sub => av -% bv,
            .mul => av *% bv,
            .negate => if (@typeInfo(T).int.signedness == .signed) -%av else -%av,
            .abs => if (@typeInfo(T).int.signedness == .signed)
                (if (av < 0) -%av else av)
            else
                av,
            .abs_diff => if (@typeInfo(T).int.signedness == .signed)
                (if (av > bv) av -% bv else bv -% av)
            else
                (if (av > bv) av - bv else bv - av),
            .div, .div_trunc => if (bv != 0) @divTrunc(av, bv) else 0,
            .rem => if (bv != 0) @rem(av, bv) else 0,
            .mod => if (bv != 0) @mod(av, bv) else 0,
        };
    }

    /// Generic float binary operation.
    fn floatBinOp(comptime T: type, av: T, bv: T, op: NumOp) T {
        return switch (op) {
            .add => av + bv,
            .sub => av - bv,
            .mul => av * bv,
            .negate => -av,
            .abs => @abs(av),
            .abs_diff => @abs(av - bv),
            .div, .div_trunc => av / bv,
            .rem, .mod => @rem(av, bv),
        };
    }

    /// Dec (fixed-point i128 with 10^18 scale) binary operation.
    fn decBinOp(self: *LirInterpreter, av: i128, bv: i128, op: NumOp) i128 {
        return switch (op) {
            .add => av +% bv,
            .sub => av -% bv,
            .negate => -%av,
            .abs => if (av < 0) -%av else av,
            .abs_diff => if (av > bv) av -% bv else bv -% av,
            .mul => blk: {
                const result = RocDec.mulWithOverflow(RocDec{ .num = av }, RocDec{ .num = bv });
                break :blk result.value.num;
            },
            .div => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                break :blk builtins.dec.divC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
            },
            .div_trunc => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                break :blk builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
            },
            .rem => blk: {
                // Dec rem: a - trunc(a/b) * b
                if (bv == 0) break :blk @as(i128, 0);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                const div_result = builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
                const mul_result = RocDec.mulWithOverflow(RocDec{ .num = div_result }, RocDec{ .num = bv });
                break :blk av -% mul_result.value.num;
            },
            .mod => blk: {
                if (bv == 0) break :blk @as(i128, 0);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                const div_result = builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
                const mul_result = RocDec.mulWithOverflow(RocDec{ .num = div_result }, RocDec{ .num = bv });
                const remainder = av -% mul_result.value.num;
                // Mod adjusts sign to match divisor
                if (remainder == 0) break :blk @as(i128, 0);
                if ((remainder > 0) != (bv > 0))
                    break :blk remainder +% bv
                else
                    break :blk remainder;
            },
        };
    }

    /// Generic comparison operation.
    fn cmpOp(comptime T: type, av: T, bv: T, op: CmpOp) bool {
        return switch (op) {
            .eq => av == bv,
            .lt => av < bv,
            .lte => av <= bv,
            .gt => av > bv,
            .gte => av >= bv,
        };
    }

    fn cmpOrder(comptime T: type, av: T, bv: T) u8 {
        if (av < bv) return 0; // LT
        if (av == bv) return 1; // EQ
        return 2; // GT
    }

    fn shiftOp(comptime T: type, av: T, amount: u8, op: ShiftOp) T {
        const Bits = std.math.Log2Int(T);
        const max_bits = @typeInfo(T).int.bits;
        if (amount >= max_bits) return 0;
        const shift: Bits = @intCast(amount);
        return switch (op) {
            .shl => av << shift,
            .shr => av >> shift,
            .shr_zf => blk: {
                const U = std.meta.Int(.unsigned, max_bits);
                break :blk @bitCast(@as(U, @bitCast(av)) >> shift);
            },
        };
    }

    fn intPow(base_val: i128, exp: i128) i128 {
        if (exp <= 0) return 1;
        var result: i128 = 1;
        var b = base_val;
        var e = exp;
        while (e > 0) {
            if (e & 1 != 0) result = result *% b;
            b = b *% b;
            e >>= 1;
        }
        return result;
    }

    // String operations

    fn evalStrJoinWith(self: *LirInterpreter, list_arg: Value, sep_arg: Value, _: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const sep = self.readRocStr(sep_arg);
        const count = rl.len();
        if (count == 0) return self.makeRocStr("");

        // Read each RocStr element from the list
        const str_size = @sizeOf(RocStr);
        var total_len: usize = 0;
        var parts = std.array_list.AlignedManaged([]const u8, null).init(self.allocator);
        defer parts.deinit();
        for (0..count) |i| {
            const elem_ptr = rl.bytes.? + i * str_size;
            const elem_val = Value{ .ptr = elem_ptr };
            const s = self.readRocStr(elem_val);
            total_len += s.len;
            parts.append(s) catch return error.OutOfMemory;
        }
        total_len += sep.len * (count - 1);

        const buf = self.arena.allocator().alloc(u8, total_len) catch return error.OutOfMemory;
        var offset: usize = 0;
        for (parts.items, 0..) |s, i| {
            @memcpy(buf[offset..][0..s.len], s);
            offset += s.len;
            if (i < parts.items.len - 1) {
                @memcpy(buf[offset..][0..sep.len], sep);
                offset += sep.len;
            }
        }
        return self.makeRocStr(buf);
    }

    fn rawBytesEqual(a: []const u8, b: []const u8) bool {
        if (a.len != b.len) return false;
        for (a, b) |lhs, rhs| {
            if (lhs != rhs) return false;
        }
        return true;
    }

    fn rocStrEqualSlices(a: []const u8, b: []const u8) bool {
        return dev_wrappers.roc_builtins_str_equal(
            if (a.len == 0) null else @constCast(a.ptr),
            a.len,
            a.len,
            if (b.len == 0) null else @constCast(b.ptr),
            b.len,
            b.len,
        );
    }

    // Layout helpers

    /// Get the layout of the i-th field in a struct layout.
    fn fieldLayoutOf(self: *LirInterpreter, struct_layout: layout_mod.Idx, field_idx: u32) layout_mod.Idx {
        const l = self.layout_store.getLayout(struct_layout);
        if (l.tag != .struct_) return .zst;
        const sd = self.layout_store.getStructData(l.data.struct_.idx);
        const fields = self.layout_store.struct_fields.sliceRange(sd.getFields());
        if (field_idx < fields.len) {
            return fields.get(field_idx).layout;
        }
        return .zst;
    }

    fn readBoxedDataPointer(self: *const LirInterpreter, boxed: Value) ?[*]u8 {
        const target_usize = self.layout_store.targetUsize();
        const raw_ptr: usize = if (target_usize.size() == 8)
            boxed.read(usize)
        else
            boxed.read(u32);

        if (raw_ptr == 0) return null;
        return @ptrFromInt(raw_ptr);
    }

    const ResolvedTagUnionBase = struct {
        value: Value,
        layout: layout_mod.Idx,
    };

    fn resolveTagUnionBaseValue(
        self: *LirInterpreter,
        union_val: Value,
        union_layout: layout_mod.Idx,
    ) ResolvedTagUnionBase {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const inner_layout = union_layout_val.data.box;
            const data_ptr = self.readBoxedDataPointer(union_val) orelse {
                return .{ .value = Value.zst, .layout = inner_layout };
            };
            return .{
                .value = .{ .ptr = data_ptr },
                .layout = inner_layout,
            };
        }

        return .{
            .value = union_val,
            .layout = union_layout,
        };
    }

    /// Get the payload layout for a given tag discriminant.
    fn tagPayloadLayout(self: *LirInterpreter, union_layout: layout_mod.Idx, discriminant: u16) layout_mod.Idx {
        const l = self.layout_store.getLayout(union_layout);
        return switch (l.tag) {
            .tag_union => blk: {
                const tu_data = self.layout_store.getTagUnionData(l.data.tag_union.idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            .box => blk: {
                const inner_layout = self.layout_store.getLayout(l.data.box);
                if (inner_layout.tag != .tag_union) break :blk .zst;
                const tu_data = self.layout_store.getTagUnionData(inner_layout.data.tag_union.idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            else => .zst,
        };
    }

    fn tagPayloadArgValue(
        self: *LirInterpreter,
        union_val: Value,
        union_layout: layout_mod.Idx,
        discriminant: u16,
        arg_index: u32,
    ) struct { value: Value, layout: layout_mod.Idx } {
        const tag_base = self.resolveTagUnionBaseValue(union_val, union_layout);
        const payload_layout = self.tagPayloadLayout(union_layout, discriminant);
        const payload_layout_val = self.layout_store.getLayout(payload_layout);
        if (payload_layout_val.tag == .struct_) {
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                payload_layout_val.data.struct_.idx,
                arg_index,
            );
            const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                payload_layout_val.data.struct_.idx,
                arg_index,
            );
            return .{
                .value = tag_base.value.offset(field_offset),
                .layout = field_layout,
            };
        }
        return .{
            .value = tag_base.value,
            .layout = payload_layout,
        };
    }

    fn tagPayloadArgValueForPattern(
        self: *LirInterpreter,
        union_val: Value,
        union_layout: layout_mod.Idx,
        discriminant: u16,
        arg_index: u32,
        pattern_id: LirPatternId,
    ) Value {
        const payload = self.tagPayloadArgValue(union_val, union_layout, discriminant, arg_index);
        const expected_layout = self.patternLayout(pattern_id) orelse return payload.value;
        return self.normalizeValueToLayout(payload.value, payload.layout, expected_layout);
    }

    fn patternLayout(self: *const LirInterpreter, pattern_id: LirPatternId) ?layout_mod.Idx {
        const pat = self.store.getPattern(pattern_id);
        return switch (pat) {
            .bind => |b| b.layout_idx,
            .wildcard => |w| w.layout_idx,
            .int_literal => |lit| lit.layout_idx,
            .float_literal => |lit| lit.layout_idx,
            .str_literal => .str,
            .tag => |t| t.union_layout,
            .struct_ => |s| s.struct_layout,
            .list => |l| l.list_layout,
            .as_pattern => |ap| ap.layout_idx,
        };
    }

    fn normalizeValueToLayout(
        self: *const LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Value {
        if (actual_layout == expected_layout) return value;

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        switch (actual_layout_val.tag) {
            .box => {
                if (actual_layout_val.data.box == expected_layout) {
                    const data_ptr = self.readBoxedDataPointer(value) orelse return Value.zst;
                    return .{ .ptr = data_ptr };
                }
            },
            .box_of_zst => if (expected_layout == .zst) return Value.zst,
            else => {},
        }

        return value;
    }

    fn getLayout(self: *LirInterpreter, idx: layout_mod.Idx) Layout {
        return self.layout_store.getLayout(idx);
    }

    fn evalBoxBox(self: *LirInterpreter, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box_of_zst => return Value.zst,
            .box => {
                const elem_layout = ret_layout_val.data.box;
                const elem_size = self.helper.sizeOf(elem_layout);
                const elem_align = self.helper.sizeAlignOf(elem_layout).alignment.toByteUnits();
                const data_ptr = try self.allocRocData(elem_size, @intCast(elem_align));
                if (elem_size > 0) {
                    @memcpy(data_ptr[0..elem_size], arg.ptr[0..elem_size]);
                }

                const boxed = try self.alloc(ret_layout);
                const target_usize = self.layout_store.targetUsize();
                if (target_usize.size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            else => return error.RuntimeError,
        }
    }

    fn evalBoxUnbox(self: *LirInterpreter, boxed: Value, ret_layout: layout_mod.Idx) Error!Value {
        if (ret_layout == .zst) return Value.zst;

        const data_ptr = self.readBoxedDataPointer(boxed) orelse return Value.zst;
        const result = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(ret_layout);
        if (size > 0) {
            result.copyFrom(.{ .ptr = data_ptr }, size);
        }
        return result;
    }

    // ═══════════════════════════════════════════════════════════════════
    //  Stack-safe eval engine
    //
    //  All evaluation goes through an explicit work_stack + value_stack.
    //  Entry points:
    //    evalStackSafe     — evaluate an expression
    //    evalProcStackSafe — call a proc (used by evalEntrypoint, sort)
    //  Both seed the work stack then delegate to runWorkLoop.
    // ═══════════════════════════════════════════════════════════════════

    const Continuation = work_stack.Continuation;
    const WorkItem = work_stack.WorkItem;

    // ── Stack helpers ──

    fn pushWork(self: *LirInterpreter, item: WorkItem) Error!void {
        self.work_stack.append(self.allocator, item) catch return error.OutOfMemory;
    }

    fn pushValue(self: *LirInterpreter, val: Value) Error!void {
        self.value_stack.append(self.allocator, val) catch return error.OutOfMemory;
    }

    fn popValue(self: *LirInterpreter) Value {
        return self.value_stack.pop().?;
    }

    fn popValues(self: *LirInterpreter, count: usize) Error![]Value {
        if (count == 0) return &[_]Value{};
        const buf = self.arena.allocator().alloc(Value, count) catch return error.OutOfMemory;
        var i: usize = count;
        while (i > 0) {
            i -= 1;
            buf[i] = self.value_stack.pop().?;
        }
        return buf;
    }

    /// Schedule: push continuation first (bottom), then eval_expr (top).
    /// eval_expr fires first, pushes result to value_stack,
    /// then continuation fires and reads the result.
    fn scheduleEvalThen(self: *LirInterpreter, cont: Continuation, expr_id: LirExprId) Error!void {
        try self.pushWork(.{ .apply_continuation = cont });
        try self.pushWork(.{ .eval_expr = expr_id });
    }

    // ── Main loop ──

    /// Stack-safe expression evaluator.
    /// Pushes work items onto an explicit stack instead of recursing.
    pub fn evalStackSafe(self: *LirInterpreter, initial_expr_id: LirExprId) Error!EvalResult {
        // Save outer stack depths to support re-entrancy (e.g., evalLowLevel calling
        // self.eval() for simple args while the stack-safe engine is active).
        const outer_work_len = self.work_stack.items.len;
        _ = self.value_stack.items.len; // outer_value_len reserved for future assertions
        const saved_unwinding = self.unwinding;
        self.unwinding = .none;

        // Seed: return_result continuation (bottom), then the initial expression (top)
        try self.pushWork(.{ .apply_continuation = .return_result });
        try self.pushWork(.{ .eval_expr = initial_expr_id });

        return self.runWorkLoop(outer_work_len, saved_unwinding);
    }

    /// Core work loop: pops and dispatches work items until the stack returns
    /// to `outer_work_len` (i.e. the `return_result` sentinel fires).
    /// Shared by `evalStackSafe` (expression entry) and `evalProcStackSafe`
    /// (function-call entry).
    fn runWorkLoop(self: *LirInterpreter, outer_work_len: usize, saved_unwinding: Unwinding) Error!EvalResult {
        while (self.work_stack.items.len > outer_work_len) {
            const item = self.work_stack.pop().?;

            // Unwinding mode: skip non-boundary items until we hit a frame boundary
            switch (self.unwinding) {
                .none => {},
                .early_return => |ret_val| {
                    switch (item) {
                        .apply_continuation => |cont| switch (cont) {
                            .call_cleanup => |cleanup| {
                                self.bindings.shrinkRetainingCapacity(cleanup.saved_bindings_len);
                                self.current_lambda_params = cleanup.saved_lambda_params;
                                self.value_stack.shrinkRetainingCapacity(cleanup.saved_value_stack_len);
                                self.call_depth -= 1;
                                try self.pushValue(ret_val);
                                self.unwinding = .none;
                                continue; // skip normal dispatch
                            },
                            .return_result => {
                                self.unwinding = saved_unwinding;
                                return .{ .early_return = ret_val };
                            },
                            else => continue,
                        },
                        else => continue,
                    }
                },
                .break_expr => {
                    switch (item) {
                        .apply_continuation => |cont| switch (cont) {
                            .for_loop_body_done => |fl| {
                                self.value_stack.shrinkRetainingCapacity(fl.saved_value_stack_len);
                                try self.pushValue(Value.zst);
                                self.unwinding = .none;
                                continue; // skip normal dispatch
                            },
                            .while_loop_body_done => |wl| {
                                self.value_stack.shrinkRetainingCapacity(wl.saved_value_stack_len);
                                try self.pushValue(Value.zst);
                                self.unwinding = .none;
                                continue; // skip normal dispatch
                            },
                            .call_cleanup => |cleanup| {
                                self.bindings.shrinkRetainingCapacity(cleanup.saved_bindings_len);
                                self.current_lambda_params = cleanup.saved_lambda_params;
                                self.value_stack.shrinkRetainingCapacity(cleanup.saved_value_stack_len);
                                self.call_depth -= 1;
                                try self.pushValue(Value.zst);
                                self.unwinding = .none;
                                continue; // skip normal dispatch
                            },
                            .return_result => {
                                self.unwinding = saved_unwinding;
                                return .{ .break_expr = {} };
                            },
                            else => continue,
                        },
                        else => continue,
                    }
                },
            }

            // Normal dispatch
            switch (item) {
                .eval_expr => |expr_id| {
                    if (comptime trace.enabled) {
                        const expr = self.store.getExpr(expr_id);
                        trace.log("eval_expr {any}: {s}", .{ expr_id, @tagName(expr) });
                    }
                    try self.scheduleExprEval(expr_id);
                },
                .eval_cf_stmt => |stmt_id| try self.scheduleCFStmtEval(stmt_id),
                .apply_continuation => |cont| {
                    trace.log("apply_continuation: {s}", .{@tagName(cont)});
                    if (try self.applyContinuation(cont)) |result| {
                        self.unwinding = saved_unwinding;
                        return result;
                    }
                },
            }
        }

        // Should not reach here — return_result should have fired
        self.unwinding = saved_unwinding;
        return error.RuntimeError;
    }

    // ── Expression scheduling ──

    /// Schedule evaluation of a LIR expression.
    /// Pushes work items to evaluate the expression and its sub-expressions.
    fn scheduleExprEval(self: *LirInterpreter, expr_id: LirExprId) Error!void {
        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            // ── Immediate (push value directly) ──

            .i64_literal => |lit| try self.pushValue(try self.evalI64Literal(lit.value, lit.layout_idx)),
            .i128_literal => |lit| try self.pushValue(try self.evalI128Literal(lit.value, lit.layout_idx)),
            .f64_literal => |v| try self.pushValue(try self.evalF64Literal(v)),
            .f32_literal => |v| try self.pushValue(try self.evalF32Literal(v)),
            .dec_literal => |v| try self.pushValue(try self.evalDecLiteral(v)),
            .str_literal => |idx| try self.pushValue(try self.evalStrLiteral(idx)),
            .bool_literal => |b| try self.pushValue(try self.evalBoolLiteral(b)),
            .lookup => |l| try self.pushValue(try self.evalLookup(l.symbol, l.layout_idx)),
            .cell_load => |l| try self.pushValue(try self.evalCellLoad(l.cell, l.layout_idx)),
            .zero_arg_tag => |z| try self.pushValue(try self.evalZeroArgTag(z)),
            .empty_list => |l| try self.pushValue(try self.evalEmptyList(l)),

            // ── Nominal (tail-unwrap) ──
            .nominal => |n| try self.pushWork(.{ .eval_expr = n.backing_expr }),

            // ── Unary sub-expression (push unary_then + eval_expr) ──

            .struct_access => |sa| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .struct_access = .{
                    .struct_layout = sa.struct_layout,
                    .field_layout = sa.field_layout,
                    .field_idx = sa.field_idx,
                } } }, sa.struct_expr);
            },
            .tag_payload_access => |tpa| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .tag_payload_access = .{
                    .union_layout = tpa.union_layout,
                    .payload_layout = tpa.payload_layout,
                } } }, tpa.value);
            },
            .dbg => |d| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .dbg_stmt = .{
                    .result_layout = d.result_layout,
                } } }, d.expr);
            },
            .expect => |e| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .expect_cond = .{
                    .cond_expr_id = e.cond,
                    .result_layout = e.result_layout,
                } } }, e.cond);
            },
            .incref => |ir| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .incref = .{
                    .layout_idx = ir.layout_idx,
                    .count = ir.count,
                } } }, ir.value);
            },
            .decref => |dr| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .decref = .{
                    .layout_idx = dr.layout_idx,
                } } }, dr.value);
            },
            .free => |f| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .free = .{
                    .layout_idx = f.layout_idx,
                } } }, f.value);
            },
            .int_to_str => |its| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .int_to_str = .{
                    .int_precision = its.int_precision,
                } } }, its.value);
            },
            .float_to_str => |fts| {
                try self.scheduleEvalThen(.{ .unary_then = .{ .float_to_str = .{
                    .float_precision = fts.float_precision,
                } } }, fts.value);
            },
            .dec_to_str => |dts| {
                try self.scheduleEvalThen(.{ .unary_then = .dec_to_str }, dts);
            },
            .str_escape_and_quote => |seq| {
                try self.scheduleEvalThen(.{ .unary_then = .str_escape_and_quote }, seq);
            },

            // ── Multi-arg collect ──

            .proc_call => |pc| {
                const arg_exprs = self.store.getExprSpan(pc.args);
                if (arg_exprs.len == 0) {
                    // Zero-arg call: enter function directly
                    const proc_spec = self.store.getProcSpec(pc.proc);
                    try self.enterFunction(proc_spec, &[_]Value{});
                } else {
                    try self.scheduleEvalThen(.{ .call_collect_args = .{
                        .proc = pc.proc,
                        .args = pc.args,
                        .next_arg_idx = 0,
                    } }, arg_exprs[0]);
                }
            },
            .struct_ => |s| {
                const field_exprs = self.store.getExprSpan(s.fields);
                if (field_exprs.len == 0) {
                    try self.pushValue(try self.alloc(s.struct_layout));
                } else {
                    try self.scheduleEvalThen(.{ .struct_collect = .{
                        .struct_layout = s.struct_layout,
                        .fields = s.fields,
                        .next_field_idx = 0,
                    } }, field_exprs[0]);
                }
            },
            .tag => |t| {
                const arg_exprs = self.store.getExprSpan(t.args);
                if (arg_exprs.len == 0) {
                    try self.pushValue(try self.evalZeroArgTag(.{
                        .discriminant = t.discriminant,
                        .union_layout = t.union_layout,
                    }));
                } else {
                    try self.scheduleEvalThen(.{ .tag_collect = .{
                        .discriminant = t.discriminant,
                        .union_layout = t.union_layout,
                        .args = t.args,
                        .next_arg_idx = 0,
                    } }, arg_exprs[0]);
                }
            },
            .list => |l| {
                const elem_exprs = self.store.getExprSpan(l.elems);
                if (elem_exprs.len == 0) {
                    try self.pushValue(try self.evalEmptyList(.{
                        .list_layout = l.list_layout,
                        .elem_layout = l.elem_layout,
                    }));
                } else {
                    try self.scheduleEvalThen(.{ .list_collect = .{
                        .list_layout = l.list_layout,
                        .elem_layout = l.elem_layout,
                        .elems = l.elems,
                        .next_elem_idx = 0,
                    } }, elem_exprs[0]);
                }
            },
            .str_concat => |sc| {
                const parts = self.store.getExprSpan(sc);
                if (parts.len == 0) {
                    try self.pushValue(try self.makeRocStr(""));
                } else {
                    try self.scheduleEvalThen(.{ .str_concat_collect = .{
                        .parts = sc,
                        .next_part_idx = 0,
                    } }, parts[0]);
                }
            },
            .low_level => |ll| {
                // Low-level ops evaluate their own args (always simple lookups/literals,
                // bounded depth). Call existing helper directly.
                const value = self.evalLowLevel(ll) catch |err| switch (err) {
                    error.RuntimeError => {
                        if (self.getRuntimeErrorMessage() == null) {
                            const msg = std.fmt.allocPrint(
                                self.arena.allocator(),
                                "RuntimeError in low-level op {s}",
                                .{@tagName(ll.op)},
                            ) catch return error.OutOfMemory;
                            return self.runtimeError(msg);
                        }
                        return error.RuntimeError;
                    },
                    else => return err,
                };
                try self.pushValue(value);
            },
            .hosted_call => |hc| {
                // Hosted calls use complex arg marshaling — call existing helper directly.
                // The helper only calls self.eval() for arg sub-expressions which are simple
                // lookups/literals, so recursion depth is bounded.
                const value = try self.evalHostedCall(hc);
                try self.pushValue(value);
            },

            // ── Control flow ──

            .if_then_else => |ite| {
                const branches = self.store.getIfBranches(ite.branches);
                if (branches.len == 0) {
                    try self.pushWork(.{ .eval_expr = ite.final_else });
                } else {
                    try self.scheduleEvalThen(.{ .if_branch = .{
                        .branches = ite.branches,
                        .current_branch_idx = 0,
                        .final_else = ite.final_else,
                        .result_layout = ite.result_layout,
                    } }, branches[0].cond);
                }
            },
            .match_expr => |m| {
                try self.scheduleEvalThen(.{ .match_dispatch = .{
                    .branches = m.branches,
                    .result_layout = m.result_layout,
                } }, m.value);
            },
            .discriminant_switch => |ds| {
                try self.scheduleEvalThen(.{ .discriminant_switch_dispatch = .{
                    .union_layout = ds.union_layout,
                    .branches = ds.branches,
                    .result_layout = ds.result_layout,
                } }, ds.value);
            },
            .block => |b| {
                const stmts = self.store.getStmts(b.stmts);
                // Find first non-cell_drop statement to schedule
                const first_real_idx = self.findFirstRealStmt(stmts, 0);
                if (first_real_idx) |idx| {
                    const stmt_expr_id = self.stmtExprId(stmts[idx]);
                    try self.scheduleEvalThen(.{ .block_stmt = .{
                        .stmts = b.stmts,
                        .current_stmt_idx = @intCast(idx),
                        .final_expr = b.final_expr,
                    } }, stmt_expr_id);
                } else {
                    // No real statements, just evaluate the final expression
                    try self.pushWork(.{ .eval_expr = b.final_expr });
                }
            },
            .for_loop => |fl| {
                try self.scheduleEvalThen(.{ .for_loop_eval_list = .{
                    .elem_layout = fl.elem_layout,
                    .elem_pattern = fl.elem_pattern,
                    .body = fl.body,
                } }, fl.list_expr);
            },
            .while_loop => |wl| {
                const check_infinite = self.detect_infinite_while_loops and
                    !self.exprInvolvesMutableCell(wl.cond) and
                    !self.exprHasLoopExit(wl.body);
                try self.scheduleEvalThen(.{ .while_loop_check = .{
                    .cond = wl.cond,
                    .body = wl.body,
                    .infinite_loop_check = check_infinite,
                } }, wl.cond);
            },

            // ── Inline ──

            .early_return => |er| {
                try self.scheduleEvalThen(.early_return_wrap, er.expr);
            },
            .break_expr => {
                self.unwinding = .break_expr;
                try self.pushValue(Value.zst);
            },
            .crash => |c| {
                const msg = self.store.getString(c.msg);
                if (self.roc_env.crash_message) |old| self.allocator.free(old);
                self.roc_env.crash_message = self.allocator.dupe(u8, msg) catch null;
                return error.Crash;
            },
            .runtime_error => |runtime_error_expr| {
                if (self.recover_runtime_placeholders) {
                    try self.pushValue(try self.placeholderValueForLayout(runtime_error_expr.ret_layout));
                } else {
                    return error.RuntimeError;
                }
            },
        }
    }

    // ── CF statement scheduling ──

    /// Schedule evaluation of a CF statement chain.
    fn scheduleCFStmtEval(self: *LirInterpreter, stmt_id: CFStmtId) Error!void {
        if (stmt_id.isNone()) {
            try self.pushValue(Value.zst);
            return;
        }
        const stmt = self.store.getCFStmt(stmt_id);
        switch (stmt) {
            .let_stmt => |ls| {
                try self.scheduleEvalThen(.{ .cf_let_bind = .{
                    .pattern = ls.pattern,
                    .next = ls.next,
                } }, ls.value);
            },
            .ret => |r| {
                // Result stays on value_stack for call_cleanup to pick up
                try self.pushWork(.{ .eval_expr = r.value });
            },
            .join => |j| {
                // Register the join point body, then schedule the remainder
                self.join_points.put(self.allocator, @intFromEnum(j.id), .{
                    .params = j.params,
                    .param_layouts = j.param_layouts,
                    .body = j.body,
                }) catch return error.OutOfMemory;
                try self.scheduleCFStmtEval(j.remainder);
            },
            .jump => |j| {
                const jump_args = self.store.getExprSpan(j.args);
                if (jump_args.len == 0) {
                    // No args: just schedule the join point body
                    const jp = self.join_points.get(@intFromEnum(j.target)) orelse return error.RuntimeError;
                    try self.pushWork(.{ .eval_cf_stmt = jp.body });
                } else {
                    try self.scheduleEvalThen(.{ .cf_jump_collect_args = .{
                        .target = j.target,
                        .args = j.args,
                        .next_arg_idx = 0,
                    } }, jump_args[0]);
                }
            },
            .expr_stmt => |es| {
                try self.scheduleEvalThen(.{ .cf_expr_stmt_next = .{
                    .next = es.next,
                } }, es.value);
            },
            .switch_stmt => |ss| {
                try self.scheduleEvalThen(.{ .cf_switch_dispatch = .{
                    .cond_layout = ss.cond_layout,
                    .branches = ss.branches,
                    .default_branch = ss.default_branch,
                    .ret_layout = ss.ret_layout,
                } }, ss.cond);
            },
            .match_stmt => |ms| {
                try self.scheduleEvalThen(.{ .cf_match_dispatch = .{
                    .value_layout = ms.value_layout,
                    .branches = ms.branches,
                    .ret_layout = ms.ret_layout,
                } }, ms.value);
            },
        }
    }

    // ── Continuation application ──

    /// Apply a continuation. Returns non-null to stop the main loop.
    fn applyContinuation(self: *LirInterpreter, cont: Continuation) Error!?EvalResult {
        switch (cont) {
            .return_result => {
                const val = self.popValue();
                return .{ .value = val };
            },

            // ── Function calls ──

            .call_collect_args => |cca| {
                const arg_exprs = self.store.getExprSpan(cca.args);
                const next_idx = cca.next_arg_idx + 1;
                if (next_idx < arg_exprs.len) {
                    // More args to evaluate
                    try self.scheduleEvalThen(.{ .call_collect_args = .{
                        .proc = cca.proc,
                        .args = cca.args,
                        .next_arg_idx = next_idx,
                    } }, arg_exprs[next_idx]);
                } else {
                    // All args collected — pop them and enter function
                    const args = try self.popValues(arg_exprs.len);
                    const proc_spec = self.store.getProcSpec(cca.proc);
                    try self.enterFunction(proc_spec, args);
                }
                return null;
            },
            .call_cleanup => |cleanup| {
                // Pop result value
                const result = self.popValue();
                // Restore bindings and lambda params
                self.bindings.shrinkRetainingCapacity(cleanup.saved_bindings_len);
                self.current_lambda_params = cleanup.saved_lambda_params;
                self.call_depth -= 1;
                // Push result back
                try self.pushValue(result);
                return null;
            },

            // ── Aggregate construction ──

            .struct_collect => |sc| {
                const field_exprs = self.store.getExprSpan(sc.fields);
                const next_idx = sc.next_field_idx + 1;
                if (next_idx < field_exprs.len) {
                    // More fields to evaluate
                    try self.scheduleEvalThen(.{ .struct_collect = .{
                        .struct_layout = sc.struct_layout,
                        .fields = sc.fields,
                        .next_field_idx = next_idx,
                    } }, field_exprs[next_idx]);
                } else {
                    // All fields collected — build struct
                    const vals = try self.popValues(field_exprs.len);
                    const struct_val = try self.alloc(sc.struct_layout);
                    for (vals, 0..) |field_val, i| {
                        const field_offset = self.helper.structFieldOffset(sc.struct_layout, @intCast(i));
                        const field_layout = self.fieldLayoutOf(sc.struct_layout, @intCast(i));
                        const field_size = self.helper.sizeOf(field_layout);
                        if (field_size > 0) {
                            struct_val.offset(field_offset).copyFrom(field_val, field_size);
                        }
                    }
                    try self.pushValue(struct_val);
                }
                return null;
            },
            .tag_collect => |tc| {
                const arg_exprs = self.store.getExprSpan(tc.args);
                const next_idx = tc.next_arg_idx + 1;
                if (next_idx < arg_exprs.len) {
                    // More args to evaluate
                    try self.scheduleEvalThen(.{ .tag_collect = .{
                        .discriminant = tc.discriminant,
                        .union_layout = tc.union_layout,
                        .args = tc.args,
                        .next_arg_idx = next_idx,
                    } }, arg_exprs[next_idx]);
                } else {
                    // All args collected — build tag
                    const vals = try self.popValues(arg_exprs.len);
                    const tag_val = try self.alloc(tc.union_layout);
                    self.helper.writeTagDiscriminant(tag_val, tc.union_layout, tc.discriminant);

                    const payload_layout = self.tagPayloadLayout(tc.union_layout, tc.discriminant);
                    const payload_layout_val = self.layout_store.getLayout(payload_layout);

                    if (payload_layout_val.tag != .struct_) {
                        // Single-field payload
                        if (vals.len == 1) {
                            const payload_size = self.helper.sizeOf(payload_layout);
                            if (payload_size > 0) {
                                tag_val.copyFrom(vals[0], payload_size);
                            }
                        }
                    } else {
                        // Multi-field struct payload
                        for (vals, 0..) |arg_val, i| {
                            const field_layout_idx = self.layout_store.getStructFieldLayoutByOriginalIndex(
                                payload_layout_val.data.struct_.idx,
                                @intCast(i),
                            );
                            const field_size = self.helper.sizeOf(field_layout_idx);
                            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                                payload_layout_val.data.struct_.idx,
                                @intCast(i),
                            );
                            if (field_size > 0) {
                                tag_val.offset(field_offset).copyFrom(arg_val, field_size);
                            }
                        }
                    }
                    try self.pushValue(tag_val);
                }
                return null;
            },
            .list_collect => |lc| {
                const elem_exprs = self.store.getExprSpan(lc.elems);
                const next_idx = lc.next_elem_idx + 1;
                if (next_idx < elem_exprs.len) {
                    // More elements to evaluate
                    try self.scheduleEvalThen(.{ .list_collect = .{
                        .list_layout = lc.list_layout,
                        .elem_layout = lc.elem_layout,
                        .elems = lc.elems,
                        .next_elem_idx = next_idx,
                    } }, elem_exprs[next_idx]);
                } else {
                    // All elements collected — build list
                    const vals = try self.popValues(elem_exprs.len);
                    const elem_size = self.helper.sizeOf(lc.elem_layout);
                    const count = elem_exprs.len;

                    if (elem_size == 0) {
                        // ZST list
                        try self.pushValue(try self.rocListToValue(.{
                            .bytes = null,
                            .length = count,
                            .capacity_or_alloc_ptr = count,
                        }, lc.list_layout));
                    } else {
                        // Allocate element storage through roc_ops
                        const total_elem_bytes = elem_size * count;
                        const sa = self.helper.sizeAlignOf(lc.elem_layout);
                        const elem_alignment: u32 = @intCast(sa.alignment.toByteUnits());
                        const elems_rc = self.helper.containsRefcounted(lc.elem_layout);
                        const elem_data = try self.allocRocDataWithRc(total_elem_bytes, elem_alignment, elems_rc);
                        const elem_mem = elem_data[0..total_elem_bytes];
                        @memset(elem_mem, 0);

                        for (vals, 0..) |elem_val, i| {
                            const dest_offset = i * elem_size;
                            @memcpy(elem_mem[dest_offset..][0..elem_size], elem_val.ptr[0..elem_size]);
                        }

                        try self.pushValue(try self.rocListToValue(.{
                            .bytes = elem_mem.ptr,
                            .length = count,
                            .capacity_or_alloc_ptr = count,
                        }, lc.list_layout));
                    }
                }
                return null;
            },
            .str_concat_collect => |scc| {
                const parts = self.store.getExprSpan(scc.parts);
                const next_idx = scc.next_part_idx + 1;
                if (next_idx < parts.len) {
                    // More parts to evaluate
                    try self.scheduleEvalThen(.{ .str_concat_collect = .{
                        .parts = scc.parts,
                        .next_part_idx = next_idx,
                    } }, parts[next_idx]);
                } else {
                    // All parts collected — concatenate
                    const vals = try self.popValues(parts.len);
                    var total_len: usize = 0;
                    for (vals) |part_val| {
                        total_len += self.readRocStr(part_val).len;
                    }
                    const buf = self.arena.allocator().alloc(u8, total_len) catch return error.OutOfMemory;
                    var offset: usize = 0;
                    for (vals) |part_val| {
                        const s = self.readRocStr(part_val);
                        @memcpy(buf[offset..][0..s.len], s);
                        offset += s.len;
                    }
                    try self.pushValue(try self.makeRocStr(buf));
                }
                return null;
            },

            // ── Expression-level control flow ──

            .if_branch => |ib| {
                const branches = self.store.getIfBranches(ib.branches);
                const cond_val = self.popValue();
                if (cond_val.read(u8) != 0) {
                    // Condition is true: evaluate the branch body
                    try self.pushWork(.{ .eval_expr = branches[ib.current_branch_idx].body });
                } else {
                    // Condition is false: try next branch or final else
                    const next_branch = ib.current_branch_idx + 1;
                    if (next_branch < branches.len) {
                        try self.scheduleEvalThen(.{ .if_branch = .{
                            .branches = ib.branches,
                            .current_branch_idx = next_branch,
                            .final_else = ib.final_else,
                            .result_layout = ib.result_layout,
                        } }, branches[next_branch].cond);
                    } else {
                        try self.pushWork(.{ .eval_expr = ib.final_else });
                    }
                }
                return null;
            },
            .match_dispatch => |md| {
                const match_val = self.popValue();
                const match_branches = self.store.getMatchBranches(md.branches);
                for (match_branches, 0..) |branch, idx| {
                    const matched = try self.matchPattern(branch.pattern, match_val);
                    if (matched) {
                        try self.bindPattern(branch.pattern, match_val);
                        if (!branch.guard.isNone()) {
                            // Has a guard: evaluate it
                            try self.scheduleEvalThen(.{ .match_guard_check = .{
                                .match_val = match_val,
                                .branches = md.branches,
                                .current_branch_idx = @intCast(idx),
                                .result_layout = md.result_layout,
                            } }, branch.guard);
                            return null;
                        }
                        try self.pushWork(.{ .eval_expr = branch.body });
                        return null;
                    }
                }
                return error.RuntimeError;
            },
            .match_guard_check => |mgc| {
                const guard_val = self.popValue();
                if (guard_val.read(u8) != 0) {
                    // Guard passed: evaluate branch body
                    const match_branches = self.store.getMatchBranches(mgc.branches);
                    try self.pushWork(.{ .eval_expr = match_branches[mgc.current_branch_idx].body });
                } else {
                    // Guard failed: try remaining branches
                    const match_branches = self.store.getMatchBranches(mgc.branches);
                    const start = mgc.current_branch_idx + 1;
                    var i: u16 = start;
                    while (i < match_branches.len) : (i += 1) {
                        const branch = match_branches[i];
                        const matched = try self.matchPattern(branch.pattern, mgc.match_val);
                        if (matched) {
                            try self.bindPattern(branch.pattern, mgc.match_val);
                            if (!branch.guard.isNone()) {
                                try self.scheduleEvalThen(.{ .match_guard_check = .{
                                    .match_val = mgc.match_val,
                                    .branches = mgc.branches,
                                    .current_branch_idx = i,
                                    .result_layout = mgc.result_layout,
                                } }, branch.guard);
                                return null;
                            }
                            try self.pushWork(.{ .eval_expr = branch.body });
                            return null;
                        }
                    }
                    return error.RuntimeError;
                }
                return null;
            },
            .discriminant_switch_dispatch => |dsd| {
                const switch_val = self.popValue();
                const disc = self.helper.readTagDiscriminant(switch_val, dsd.union_layout);
                const disc_branches = self.store.getExprSpan(dsd.branches);
                if (disc < disc_branches.len) {
                    try self.pushWork(.{ .eval_expr = disc_branches[disc] });
                } else {
                    return error.RuntimeError;
                }
                return null;
            },
            .block_stmt => |bs| {
                const stmts = self.store.getStmts(bs.stmts);
                const stmt = stmts[bs.current_stmt_idx];
                const stmt_val = self.popValue();

                // Apply the statement's binding effect
                switch (stmt) {
                    .decl, .mutate => |binding| try self.bindPattern(binding.pattern, stmt_val),
                    .cell_init => |cb| {
                        const size = self.helper.sizeOf(cb.layout_idx);
                        self.cells.put(cb.cell.raw(), .{ .val = stmt_val, .size = size }) catch return error.OutOfMemory;
                    },
                    .cell_store => |cb| {
                        const size = self.helper.sizeOf(cb.layout_idx);
                        if (self.cells.getPtr(cb.cell.raw())) |entry| {
                            entry.val = stmt_val;
                            entry.size = size;
                        } else {
                            self.cells.put(cb.cell.raw(), .{ .val = stmt_val, .size = size }) catch return error.OutOfMemory;
                        }
                    },
                    .cell_drop => {},
                }

                // Find the next real statement to schedule
                const next_real_idx = self.findFirstRealStmt(stmts, bs.current_stmt_idx + 1);
                if (next_real_idx) |next_idx| {
                    const next_expr_id = self.stmtExprId(stmts[next_idx]);
                    try self.scheduleEvalThen(.{ .block_stmt = .{
                        .stmts = bs.stmts,
                        .current_stmt_idx = @intCast(next_idx),
                        .final_expr = bs.final_expr,
                    } }, next_expr_id);
                } else {
                    // No more statements: evaluate the final expression
                    try self.pushWork(.{ .eval_expr = bs.final_expr });
                }
                return null;
            },
            .early_return_wrap => {
                const val = self.popValue();
                self.unwinding = .{ .early_return = val };
                return null;
            },

            // ── Loops ──

            .for_loop_eval_list => |fl| {
                const list_val = self.popValue();
                const elem_size = self.helper.sizeOf(fl.elem_layout);
                const rl = valueToRocList(list_val);
                const count = rl.len();

                if (count == 0) {
                    try self.pushValue(Value.zst);
                } else {
                    const data: [*]u8 = @ptrCast(rl.bytes orelse {
                        try self.pushValue(Value.zst);
                        return null;
                    });
                    // Bind first element
                    const elem_val = if (elem_size > 0)
                        Value{ .ptr = data }
                    else
                        Value.zst;
                    try self.bindPattern(fl.elem_pattern, elem_val);
                    // Schedule body + continuation
                    try self.scheduleEvalThen(.{ .for_loop_body_done = .{
                        .list_val = list_val,
                        .elem_layout = fl.elem_layout,
                        .elem_pattern = fl.elem_pattern,
                        .body = fl.body,
                        .current_idx = 0,
                        .count = @intCast(count),
                        .saved_value_stack_len = @intCast(self.value_stack.items.len),
                    } }, fl.body);
                }
                return null;
            },
            .for_loop_body_done => |fl| {
                // Discard body result
                _ = self.popValue();
                const next_idx = fl.current_idx + 1;
                if (next_idx < fl.count) {
                    // More iterations
                    const elem_size = self.helper.sizeOf(fl.elem_layout);
                    const rl = valueToRocList(fl.list_val);
                    const data: [*]u8 = @ptrCast(rl.bytes orelse {
                        try self.pushValue(Value.zst);
                        return null;
                    });
                    const elem_val = if (elem_size > 0)
                        Value{ .ptr = data + next_idx * elem_size }
                    else
                        Value.zst;
                    try self.bindPattern(fl.elem_pattern, elem_val);
                    try self.scheduleEvalThen(.{ .for_loop_body_done = .{
                        .list_val = fl.list_val,
                        .elem_layout = fl.elem_layout,
                        .elem_pattern = fl.elem_pattern,
                        .body = fl.body,
                        .current_idx = next_idx,
                        .count = fl.count,
                        .saved_value_stack_len = fl.saved_value_stack_len,
                    } }, fl.body);
                } else {
                    // Done iterating
                    try self.pushValue(Value.zst);
                }
                return null;
            },
            .while_loop_check => |wlc| {
                const cond_val = self.popValue();
                const cond_is_true = cond_val.read(u8) != 0;
                if (wlc.infinite_loop_check and cond_is_true) {
                    return self.triggerCrash(infinite_while_loop_message);
                }
                if (!cond_is_true) {
                    try self.pushValue(Value.zst);
                } else {
                    try self.scheduleEvalThen(.{ .while_loop_body_done = .{
                        .cond = wlc.cond,
                        .body = wlc.body,
                        .infinite_loop_check = wlc.infinite_loop_check,
                        .saved_value_stack_len = @intCast(self.value_stack.items.len),
                    } }, wlc.body);
                }
                return null;
            },
            .while_loop_body_done => |wlbd| {
                // Discard body result, re-check condition
                _ = self.popValue();
                try self.scheduleEvalThen(.{ .while_loop_check = .{
                    .cond = wlbd.cond,
                    .body = wlbd.body,
                    .infinite_loop_check = wlbd.infinite_loop_check,
                } }, wlbd.cond);
                return null;
            },

            // ── Unary ──

            .unary_then => |ut| {
                const val = self.popValue();
                switch (ut) {
                    .struct_access => |sa| {
                        const field_offset = self.helper.structFieldOffset(sa.struct_layout, sa.field_idx);
                        try self.pushValue(val.offset(field_offset));
                    },
                    .tag_payload_access => |tpa| {
                        const tag_base = self.resolveTagUnionBaseValue(val, tpa.union_layout);
                        const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
                        const actual_payload_layout = self.tagPayloadLayout(tpa.union_layout, disc);
                        try self.pushValue(self.normalizeValueToLayout(tag_base.value, actual_payload_layout, tpa.payload_layout));
                    },
                    .dbg_stmt => |ds| {
                        const dbg_msg = try self.renderExpectValue(val, ds.result_layout);
                        self.roc_ops.dbg(dbg_msg);
                        try self.pushValue(val);
                    },
                    .expect_cond => |ec| {
                        if (val.read(u8) == 0) {
                            if (self.roc_env.expect_message == null) {
                                const msg = try self.renderExpectExpr(ec.cond_expr_id);
                                self.roc_env.expect_message = self.allocator.dupe(u8, msg) catch return error.OutOfMemory;
                            }
                        }
                        try self.pushValue(Value.zst);
                    },
                    .incref => |ir| {
                        self.performRc(.incref, val, ir.layout_idx, ir.count);
                        try self.pushValue(Value.zst);
                    },
                    .decref => |dr| {
                        self.performRc(.decref, val, dr.layout_idx, 0);
                        try self.pushValue(Value.zst);
                    },
                    .free => |f| {
                        self.performRc(.free, val, f.layout_idx, 0);
                        try self.pushValue(Value.zst);
                    },
                    .int_to_str => |its| {
                        const arena = self.arena.allocator();
                        const formatted: []const u8 = switch (its.int_precision) {
                            .u8 => std.fmt.allocPrint(arena, "{d}", .{val.read(u8)}) catch return error.OutOfMemory,
                            .i8 => std.fmt.allocPrint(arena, "{d}", .{val.read(i8)}) catch return error.OutOfMemory,
                            .u16 => std.fmt.allocPrint(arena, "{d}", .{val.read(u16)}) catch return error.OutOfMemory,
                            .i16 => std.fmt.allocPrint(arena, "{d}", .{val.read(i16)}) catch return error.OutOfMemory,
                            .u32 => std.fmt.allocPrint(arena, "{d}", .{val.read(u32)}) catch return error.OutOfMemory,
                            .i32 => std.fmt.allocPrint(arena, "{d}", .{val.read(i32)}) catch return error.OutOfMemory,
                            .u64 => std.fmt.allocPrint(arena, "{d}", .{val.read(u64)}) catch return error.OutOfMemory,
                            .i64 => std.fmt.allocPrint(arena, "{d}", .{val.read(i64)}) catch return error.OutOfMemory,
                            .u128 => std.fmt.allocPrint(arena, "{d}", .{val.read(u128)}) catch return error.OutOfMemory,
                            .i128 => std.fmt.allocPrint(arena, "{d}", .{val.read(i128)}) catch return error.OutOfMemory,
                        };
                        try self.pushValue(try self.makeRocStr(formatted));
                    },
                    .float_to_str => |fts| {
                        var buf: [400]u8 = undefined;
                        const slice: []const u8 = switch (fts.float_precision) {
                            .f32 => i128h.f64_to_str(&buf, @as(f64, val.read(f32))),
                            .f64 => i128h.f64_to_str(&buf, val.read(f64)),
                            .dec => blk: {
                                const dec = RocDec{ .num = val.read(i128) };
                                var dec_buf: [RocDec.max_str_length]u8 = undefined;
                                break :blk dec.format_to_buf(&dec_buf);
                            },
                        };
                        try self.pushValue(try self.makeRocStr(slice));
                    },
                    .dec_to_str => {
                        const dec = RocDec{ .num = val.read(i128) };
                        var buf: [RocDec.max_str_length]u8 = undefined;
                        const slice = dec.format_to_buf(&buf);
                        try self.pushValue(try self.makeRocStr(slice));
                    },
                    .str_escape_and_quote => {
                        const s = self.readRocStr(val);
                        var escaped = std.ArrayListUnmanaged(u8){};
                        escaped.append(self.allocator, '"') catch return error.OutOfMemory;
                        for (s) |ch| {
                            switch (ch) {
                                '\\' => escaped.appendSlice(self.allocator, "\\\\") catch return error.OutOfMemory,
                                '"' => escaped.appendSlice(self.allocator, "\\\"") catch return error.OutOfMemory,
                                else => escaped.append(self.allocator, ch) catch return error.OutOfMemory,
                            }
                        }
                        escaped.append(self.allocator, '"') catch return error.OutOfMemory;
                        const result = try self.makeRocStr(escaped.items);
                        escaped.deinit(self.allocator);
                        try self.pushValue(result);
                    },
                }
                return null;
            },

            // ── Multi-arg builtins ──

            .low_level_collect_args => {
                // Low-level ops are evaluated inline in scheduleExprEval
                unreachable;
            },
            .hosted_call_collect_args => {
                // Hosted calls are evaluated inline in scheduleExprEval
                unreachable;
            },

            // ── CF statement continuations ──

            .cf_let_bind => |clb| {
                const val = self.popValue();
                try self.bindPattern(clb.pattern, val);
                try self.scheduleCFStmtEval(clb.next);
                return null;
            },
            .cf_expr_stmt_next => |cesn| {
                // Discard the expression value
                _ = self.popValue();
                try self.scheduleCFStmtEval(cesn.next);
                return null;
            },
            .cf_switch_dispatch => |csd| {
                const cond_val = self.popValue();
                const disc = self.helper.readTagDiscriminant(cond_val, csd.cond_layout);
                const branches = self.store.getCFSwitchBranches(csd.branches);
                var found = false;
                for (branches) |branch| {
                    if (branch.value == disc) {
                        try self.scheduleCFStmtEval(branch.body);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    try self.scheduleCFStmtEval(csd.default_branch);
                }
                return null;
            },
            .cf_match_dispatch => |cmd| {
                const match_val = self.popValue();
                const match_branches = self.store.getCFMatchBranches(cmd.branches);
                var matched = false;
                for (match_branches) |branch| {
                    if (try self.matchPattern(branch.pattern, match_val)) {
                        try self.bindPattern(branch.pattern, match_val);
                        try self.scheduleCFStmtEval(branch.body);
                        matched = true;
                        break;
                    }
                }
                if (!matched) {
                    return error.RuntimeError;
                }
                return null;
            },
            .cf_jump_collect_args => |cjca| {
                const jump_args = self.store.getExprSpan(cjca.args);
                const next_idx = cjca.next_arg_idx + 1;
                if (next_idx < jump_args.len) {
                    try self.scheduleEvalThen(.{ .cf_jump_collect_args = .{
                        .target = cjca.target,
                        .args = cjca.args,
                        .next_arg_idx = next_idx,
                    } }, jump_args[next_idx]);
                } else {
                    // All args collected — bind to join point params and schedule body
                    const vals = try self.popValues(jump_args.len);
                    const jp = self.join_points.get(@intFromEnum(cjca.target)) orelse return error.RuntimeError;
                    const jp_params = self.store.getPatternSpan(jp.params);
                    const count = @min(jp_params.len, vals.len);
                    for (0..count) |i| {
                        try self.bindPattern(jp_params[i], vals[i]);
                    }
                    try self.pushWork(.{ .eval_cf_stmt = jp.body });
                }
                return null;
            },

            // ── Sort (placeholder) ──

            .sort_compare_step => {
                // TODO: wire up sort in a later phase
                return error.RuntimeError;
            },
        }
    }

    // ── Internal helpers for the stack-safe engine ──

    /// Enter a function call: push call_cleanup, bind params, schedule body.
    /// Does not run the body — the caller's work loop processes the scheduled items.
    fn enterFunction(self: *LirInterpreter, proc_spec: lir.LirProcSpec, args: []const Value) Error!void {
        if (self.call_depth >= max_call_depth) {
            return self.triggerCrash(stack_overflow_message);
        }

        const params = self.store.getPatternSpan(proc_spec.args);
        self.call_depth += 1;

        // Save state
        const saved_bindings_len: u32 = @intCast(self.bindings.items.len);
        const saved_lambda_params = self.current_lambda_params;
        self.current_lambda_params = proc_spec.args;

        // Push call_cleanup continuation (will fire after the body completes)
        try self.pushWork(.{ .apply_continuation = .{ .call_cleanup = .{
            .saved_bindings_len = saved_bindings_len,
            .saved_lambda_params = saved_lambda_params,
            .saved_value_stack_len = @intCast(self.value_stack.items.len),
        } } });

        // Bind parameters
        const param_count = @min(params.len, args.len);
        for (0..param_count) |i| {
            try self.bindPattern(params[i], args[i]);
        }

        // Schedule the CF statement body
        try self.pushWork(.{ .eval_cf_stmt = proc_spec.body });
    }

    /// Call a proc and run to completion, returning the result.
    /// Used by evalEntrypoint (host entry) and evalListSortWith (sort comparator).
    fn evalProcStackSafe(self: *LirInterpreter, proc_spec: lir.LirProcSpec, args: []const Value) Error!EvalResult {
        const outer_work_len = self.work_stack.items.len;
        const saved_unwinding = self.unwinding;
        self.unwinding = .none;

        try self.pushWork(.{ .apply_continuation = .return_result });
        try self.enterFunction(proc_spec, args);

        return self.runWorkLoop(outer_work_len, saved_unwinding);
    }

    /// Find the index of the first non-cell_drop statement at or after `start`.
    fn findFirstRealStmt(_: *const LirInterpreter, stmts: []const lir.LIR.LirStmt, start: usize) ?usize {
        var i = start;
        while (i < stmts.len) : (i += 1) {
            switch (stmts[i]) {
                .cell_drop => continue,
                else => return i,
            }
        }
        return null;
    }

    /// Get the expression ID from a statement (for scheduling).
    fn stmtExprId(_: *const LirInterpreter, stmt: lir.LIR.LirStmt) LirExprId {
        return switch (stmt) {
            .decl, .mutate => |binding| binding.expr,
            .cell_init, .cell_store => |cb| cb.expr,
            .cell_drop => unreachable, // findFirstRealStmt skips these
        };
    }
};
