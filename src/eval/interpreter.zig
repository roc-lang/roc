//! Statement-only LIR interpreter.
//!
//! Evaluates proc-root, post-RC LIR directly, producing concrete runtime values.
//! All evaluation follows explicit `CFStmt` control flow and explicit RC ops.
//!
//! Ownership boundary:
//! - builtin/runtime callbacks in this file may perform primitive-internal RC
//! - explicit `.incref` / `.decref` / `.free` statement handlers may execute RC
//! - all ordinary eval paths are forbidden from deciding ownership policy

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const LIR = lir.LIR;
const LirStore = lir.LirStore;
const ownership_boundary = lir.OwnershipBoundary;
const lir_value = @import("value.zig");
const builtins = @import("builtins");
const sljmp = @import("sljmp");
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
const LirProcSpecId = LIR.LirProcSpecId;
const LirProcSpec = LIR.LirProcSpec;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LocalSpan = LIR.LocalSpan;
const Symbol = LIR.Symbol;
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

/// Environment for interpreter-managed RocOps forwarding.
///
/// The interpreter always evaluates with the RocOps it was initialized with.
/// These callbacks forward the caller's alloc/dealloc/realloc/dbg/expect/crash
/// hooks while retaining local bookkeeping for crash and expect messages so
/// hosts that care can inspect the last message after evaluation.
const InterpreterRocEnv = struct {
    allocator: Allocator,
    crashed: bool = false,
    crash_message: ?[]const u8 = null,
    runtime_error_message: ?[]const u8 = null,
    expect_message: ?[]const u8 = null,
    jmp_buf: JmpBuf = undefined,
    active_jmp_buf: ?*JmpBuf = null,
    caller_roc_ops: *RocOps,

    fn init(allocator: Allocator, caller_roc_ops: *RocOps) InterpreterRocEnv {
        return .{
            .allocator = allocator,
            .caller_roc_ops = caller_roc_ops,
        };
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
    }

    /// Reset just the crash state before calling a builtin that might crash.
    fn resetCrash(self: *InterpreterRocEnv) void {
        self.crashed = false;
    }

    fn installJumpBuf(self: *InterpreterRocEnv, jmp_buf: *JmpBuf) ?*JmpBuf {
        const prev = self.active_jmp_buf;
        self.active_jmp_buf = jmp_buf;
        return prev;
    }

    fn restoreJumpBuf(self: *InterpreterRocEnv, prev: ?*JmpBuf) void {
        self.active_jmp_buf = prev;
    }

    fn currentRocOps(self: *InterpreterRocEnv) *RocOps {
        return self.caller_roc_ops;
    }

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        ownership_boundary.builtinRuntimeInternal("interpreter.rocAllocFn");
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_alloc(roc_alloc, caller_roc_ops.env);
        trace_rc.log("alloc(fwd): ptr=0x{x} size={d} align={d}", .{ @intFromPtr(roc_alloc.answer), roc_alloc.length, roc_alloc.alignment });
    }

    fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
        ownership_boundary.builtinRuntimeInternal("interpreter.rocDeallocFn");
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        trace_rc.log("dealloc: ptr=0x{x} align={d}", .{ @intFromPtr(roc_dealloc.ptr), roc_dealloc.alignment });
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_dealloc(roc_dealloc, caller_roc_ops.env);
    }

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        ownership_boundary.builtinRuntimeInternal("interpreter.rocReallocFn");
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const caller_roc_ops = self.currentRocOps();
        const old_ptr = roc_realloc.answer;
        caller_roc_ops.roc_realloc(roc_realloc, caller_roc_ops.env);
        trace_rc.log("realloc(fwd): old=0x{x} new=0x{x} size={d}", .{ @intFromPtr(old_ptr), @intFromPtr(roc_realloc.answer), roc_realloc.new_length });
    }

    fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_dbg(roc_dbg, caller_roc_ops.env);
    }

    fn rocExpectFailedFn(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_expect_failed(expect_args, caller_roc_ops.env);
        const source = expect_args.utf8_bytes[0..expect_args.len];
        if (self.expect_message == null) {
            self.expect_message = self.allocator.dupe(u8, source) catch null;
        }
    }

    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_crashed(roc_crashed, caller_roc_ops.env);
        self.crashed = true;
        const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
        if (self.crash_message) |old| self.allocator.free(old);
        self.crash_message = self.allocator.dupe(u8, msg) catch null;
        const active_jmp_buf = self.active_jmp_buf orelse {
            std.debug.print(
                "LIR/interpreter invariant violated: roc_crashed fired without an active jump buffer\n",
                .{},
            );
            std.process.abort();
        };
        self.active_jmp_buf = null;
        longjmp(active_jmp_buf, 1);
    }
};

/// Interprets statement-only LIR procs directly.
pub const Interpreter = struct {
    const LirInterpreter = @This();
    const max_call_depth: usize = 1024;
    const stack_overflow_message =
        "This Roc program overflowed its stack memory. This usually means there is very deep or infinite recursion somewhere in the code.";
    const division_by_zero_message = "Division by zero";

    allocator: Allocator,
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    helper: LayoutHelper,
    /// Arena for interpreter-allocated memory (temporaries, copies).
    arena: std.heap.ArenaAllocator,
    /// RocOps environment for builtin dispatch.
    roc_env: *InterpreterRocEnv,
    roc_ops: RocOps,
    /// Bound recursive function-call depth so the interpreter reports a Roc crash
    /// instead of overflowing the native stack.
    call_depth: usize = 0,
    /// Kept for compatibility with existing callers; strongest-form LIR has no while loops.
    detect_infinite_while_loops: bool = false,

    const JoinPointMap = std.AutoHashMapUnmanaged(u32, JoinPointInfo);

    const JoinPointInfo = struct {
        params: LocalSpan,
        body: CFStmtId,
    };

    pub const Error = error{
        OutOfMemory,
        RuntimeError,
        DivisionByZero,
        Crash,
    };

    const CrashBoundary = struct {
        env: *InterpreterRocEnv,
        prev_jmp_buf: ?*JmpBuf,

        fn init(env: *InterpreterRocEnv) CrashBoundary {
            env.resetCrash();
            return .{
                .env = env,
                .prev_jmp_buf = env.installJumpBuf(&env.jmp_buf),
            };
        }

        fn deinit(self: *CrashBoundary) void {
            self.env.restoreJumpBuf(self.prev_jmp_buf);
        }

        fn set(self: *CrashBoundary) c_int {
            return setjmp(&self.env.jmp_buf);
        }
    };

    fn enterCrashBoundary(self: *LirInterpreter) CrashBoundary {
        return CrashBoundary.init(self.roc_env);
    }

    const LocalSlot = struct {
        assigned: bool = false,
        val: Value,
    };

    const Frame = struct {
        proc_id: LirProcSpecId,
        ret_layout: layout_mod.Idx,
        locals: []LocalSlot,
        join_points: JoinPointMap = .{},

        fn deinit(self: *Frame, allocator: Allocator) void {
            self.join_points.deinit(allocator);
            allocator.free(self.locals);
        }

        fn setLocal(self: *Frame, local_id: LocalId, value: Value) void {
            const slot = &self.locals[@intFromEnum(local_id)];
            slot.* = .{
                .assigned = true,
                .val = value,
            };
        }

    };

    const ExecOutcome = union(enum) {
        returned: LocalId,
        scope_exit,
        loop_continue,
    };

    pub const EvalResult = union(enum) {
        value: Value,
    };

    pub const EvalRequest = struct {
        proc_id: LirProcSpecId,
        arg_layouts: []const layout_mod.Idx = &.{},
        ret_layout: ?layout_mod.Idx = null,
        arg_ptr: ?*anyopaque = null,
        ret_ptr: ?*anyopaque = null,
    };

    pub fn init(
        allocator: Allocator,
        store: *const LirStore,
        layout_store: *const layout_mod.Store,
        caller_roc_ops: *RocOps,
    ) Allocator.Error!LirInterpreter {
        const roc_env = try allocator.create(InterpreterRocEnv);
        roc_env.* = InterpreterRocEnv.init(allocator, caller_roc_ops);

        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .helper = LayoutHelper.init(layout_store),
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
                .hosted_fns = caller_roc_ops.hosted_fns,
            },
        };
    }

    pub fn deinit(self: *LirInterpreter) void {
        self.roc_env.deinit();
        self.allocator.destroy(self.roc_env);
        self.arena.deinit();
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

    /// Release ownership of an evaluated result value.
    /// Decrements reference counts for any heap-allocated data (strings, lists, boxes)
    /// according to the value's layout. No-op for non-refcounted types (ints, bools, etc).
    pub fn dropValue(self: *LirInterpreter, val: Value, layout_idx: layout_mod.Idx) void {
        self.performInterpreterApiRc(.decref, val, layout_idx, 0);
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
        self.roc_ops.crash(message);
        unreachable;
    }

    fn invariantFailed(_: *const LirInterpreter, comptime fmt: []const u8, args: anytype) noreturn {
        if (builtin.mode == .Debug) {
            std.debug.print(fmt, args);
            std.debug.print("\n", .{});
            std.debug.assert(false);
        }
        unreachable;
    }

    fn invariantFailedError(self: *const LirInterpreter, comptime fmt: []const u8, args: anytype) Error {
        self.invariantFailed(fmt, args);
    }

    fn currentRocOps(self: *LirInterpreter) *RocOps {
        return self.roc_env.currentRocOps();
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

    /// Allocate heap data through roc_ops with a refcount header.
    /// Use this for data that RocList.bytes or RocStr.bytes will point to,
    /// so builtins can safely call isUnique()/decref() on it.
    fn allocRocDataWithRc(self: *LirInterpreter, data_bytes: usize, element_alignment: u32, elements_refcounted: bool) Error![*]u8 {
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        return builtins.utils.allocateWithRefcount(data_bytes, element_alignment, elements_refcounted, &self.roc_ops);
    }

    fn marshalAbiArgs(self: *LirInterpreter, arg_ptr: ?*anyopaque, arg_layouts: []const layout_mod.Idx) Error![]Value {
        const arg_count = arg_layouts.len;
        if (arg_count == 0) return &.{};

        const args_buf = try self.arena.allocator().alloc(Value, arg_count);
        if (arg_ptr == null) {
            for (args_buf, arg_layouts) |*slot, arg_layout| {
                slot.* = if (self.helper.sizeOf(arg_layout) == 0)
                    Value.zst
                else
                    try self.alloc(arg_layout);
            }
            return args_buf;
        }

        const arg_bytes = @as([*]u8, @ptrCast(arg_ptr.?));
        var sorted_indices = try self.arena.allocator().alloc(usize, arg_count);
        for (0..arg_count) |i| sorted_indices[i] = i;

        for (0..arg_count) |i| {
            for (i + 1..arg_count) |j| {
                const i_align = self.helper.sizeAlignOf(arg_layouts[sorted_indices[i]]).alignment.toByteUnits();
                const j_align = self.helper.sizeAlignOf(arg_layouts[sorted_indices[j]]).alignment.toByteUnits();
                if (j_align > i_align or (j_align == i_align and sorted_indices[j] < sorted_indices[i])) {
                const tmp = sorted_indices[i];
                sorted_indices[i] = sorted_indices[j];
                sorted_indices[j] = tmp;
                }
            }
        }

        var arg_offsets = try self.arena.allocator().alloc(usize, arg_count);
        var byte_offset: usize = 0;
        for (sorted_indices) |orig_idx| {
            const sa = self.helper.sizeAlignOf(arg_layouts[orig_idx]);
            const byte_align = sa.alignment.toByteUnits();
            byte_offset = std.mem.alignForward(usize, byte_offset, byte_align);
            arg_offsets[orig_idx] = byte_offset;
            byte_offset += sa.size;
        }

        for (0..arg_count) |i| {
            const sa = self.helper.sizeAlignOf(arg_layouts[i]);
            if (sa.size == 0) {
                args_buf[i] = Value.zst;
                continue;
            }

            const copy = try self.allocBytes(sa.size);
            @memcpy(copy.ptr[0..sa.size], arg_bytes[arg_offsets[i] .. arg_offsets[i] + sa.size]);
            args_buf[i] = copy;
        }
        return args_buf;
    }

    /// Evaluate a proc-root LIR program using the RocOps bound at initialization time.
    pub fn eval(self: *LirInterpreter, request: EvalRequest) Error!EvalResult {
        self.roc_env.resetForEval();

        var eval_jmp_buf: JmpBuf = undefined;
        const prev_jmp_buf = self.roc_env.installJumpBuf(&eval_jmp_buf);
        defer self.roc_env.restoreJumpBuf(prev_jmp_buf);
        const sj = setjmp(&eval_jmp_buf);
        if (sj != 0) return error.Crash;

        const args = try self.marshalAbiArgs(request.arg_ptr, request.arg_layouts);
        const proc_ret_layout = self.store.getProcSpec(request.proc_id).ret_layout;
        const result_value = try self.evalProcById(request.proc_id, args, request.arg_layouts);
        const ret_layout = request.ret_layout orelse proc_ret_layout;
        const normalized_result = try self.coerceExplicitRefValueToLayout(result_value, proc_ret_layout, ret_layout);

        if (request.ret_ptr) |ret_ptr| {
            const ret_size = self.helper.sizeOf(ret_layout);
            if (ret_size > 0 and !normalized_result.isZst()) {
                @memcpy(@as([*]u8, @ptrCast(ret_ptr))[0..ret_size], normalized_result.readBytes(ret_size));
            }
        }

        return .{ .value = normalized_result };
    }

    fn evalProcById(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
    ) Error!Value {
        const proc_spec = self.store.getProcSpec(proc_id);
        return self.evalProcSpec(proc_id, proc_spec, args, arg_layouts);
    }

    const DebugVisitedValue = struct {
        ptr: usize,
        layout_idx: layout_mod.Idx,
    };

    fn setLocalChecked(
        self: *LirInterpreter,
        frame: *Frame,
        stmt_id: ?CFStmtId,
        local_id: LocalId,
        value: Value,
    ) void {
        if (builtin.mode == .Debug) {
            const layout_idx = self.store.getLocal(local_id).layout_idx;
            var visited = std.ArrayList(DebugVisitedValue).empty;
            defer visited.deinit(self.allocator);
            self.debugAssertValueMatchesLayout(frame.proc_id, stmt_id, local_id, value, layout_idx, &visited);
        }

        frame.setLocal(local_id, value);
    }

    fn getLocalChecked(self: *LirInterpreter, frame: *const Frame, local_id: LocalId) Error!Value {
        const slot = frame.locals[@intFromEnum(local_id)];
        if (!slot.assigned) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: local {d} was used before assignment in proc {d}",
                .{ @intFromEnum(local_id), @intFromEnum(frame.proc_id) },
            );
        }
        return slot.val;
    }

    fn debugAssertValueMatchesLayout(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        stmt_id: ?CFStmtId,
        local_id: LocalId,
        value: Value,
        layout_idx: layout_mod.Idx,
        visited: *std.ArrayList(DebugVisitedValue),
    ) void {
        if (builtin.mode != .Debug) return;
        if (comptime builtin.target.os.tag == .freestanding) return;

        const layout_val = self.layout_store.getLayout(layout_idx);
        switch (layout_val.tag) {
            .scalar => {
                if (layout_idx == .str) {
                    const str = valueToRocStr(value);
                    if (!str.isSmallStr() and str.len() > 0 and str.bytes == null) {
                        self.debugValueShapePanic(
                            proc_id,
                            stmt_id,
                            local_id,
                            layout_idx,
                            "non-small RocStr had null bytes pointer",
                        );
                    }
                }
            },
            .zst, .box_of_zst => return,
            .box => {
                const data_ptr = self.readBoxedDataPointer(value) orelse self.debugValueShapePanic(
                    proc_id,
                    stmt_id,
                    local_id,
                    layout_idx,
                    "boxed value had null data pointer",
                );

                const key = DebugVisitedValue{
                    .ptr = @intFromPtr(data_ptr),
                    .layout_idx = layout_idx,
                };
                for (visited.items) |entry| {
                    if (entry.ptr == key.ptr and entry.layout_idx == key.layout_idx) return;
                }
                visited.append(self.allocator, key) catch {
                    self.invariantFailed("LIR/interpreter invariant violated: out of memory while validating value shape", .{});
                };
                self.debugAssertValueMatchesLayout(
                    proc_id,
                    stmt_id,
                    local_id,
                    .{ .ptr = data_ptr },
                    layout_val.data.box,
                    visited,
                );
            },
            .list => {
                const list = valueToRocList(value);
                if (list.len() > 0 and list.bytes == null) {
                    self.debugValueShapePanic(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        "non-empty list had null bytes pointer",
                    );
                }
                if (list.len() == 0 or list.bytes == null) return;

                const elem_layout = layout_val.data.list;
                const elem_size = self.helper.sizeOf(elem_layout);
                if (elem_size == 0) return;

                for (0..list.len()) |i| {
                    self.debugAssertValueMatchesLayout(
                        proc_id,
                        stmt_id,
                        local_id,
                        .{ .ptr = list.bytes.? + i * elem_size },
                        elem_layout,
                        visited,
                    );
                }
            },
            .list_of_zst => {
                const list = valueToRocList(value);
                if (list.len() > 0 and list.capacity_or_alloc_ptr == 0) {
                    self.debugValueShapePanic(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        "non-empty list_of_zst had zero capacity marker",
                    );
                }
            },
            .struct_ => {
                const struct_info = self.layout_store.getStructInfo(layout_val);
                for (0..struct_info.fields.len) |i| {
                    const field = struct_info.fields.get(@intCast(i));
                    const field_offset = self.layout_store.getStructFieldOffset(layout_val.data.struct_.idx, @intCast(i));
                    self.debugAssertValueMatchesLayout(
                        proc_id,
                        stmt_id,
                        local_id,
                        value.offset(field_offset),
                        field.layout,
                        visited,
                    );
                }
            },
            .tag_union => {
                const disc = self.helper.readTagDiscriminant(value, layout_idx);
                const tag_union_info = self.layout_store.getTagUnionInfo(layout_val);
                if (disc >= tag_union_info.variants.len) {
                    self.debugValueShapePanic(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        "tag union discriminant was out of range",
                    );
                }

                const payload_layout = tag_union_info.variants.get(disc).payload_layout;
                if (self.helper.sizeOf(payload_layout) == 0) return;

                self.debugAssertValueMatchesLayout(
                    proc_id,
                    stmt_id,
                    local_id,
                    value,
                    payload_layout,
                    visited,
                );
            },
            .closure => {
                self.debugValueShapePanic(
                    proc_id,
                    stmt_id,
                    local_id,
                    layout_idx,
                    "closure value reached interpreter recursive validator unexpectedly",
                );
            },
        }
    }

    fn debugValueShapePanic(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        stmt_id: ?CFStmtId,
        local_id: LocalId,
        layout_idx: layout_mod.Idx,
        comptime reason: []const u8,
    ) noreturn {
        if (comptime builtin.target.os.tag == .freestanding) {
            @trap();
        } else {
            if (stmt_id) |id| {
                const center = @as(usize, @intFromEnum(id));
                const stmt_count = self.store.cf_stmts.items.len;
                const start = center -| 20;
                const end = @min(stmt_count, center + 21);
                std.debug.print("LIR/interpreter stmt window around failing stmt {d}:\n", .{@intFromEnum(id)});
                for (start..end) |i| {
                    const window_id: CFStmtId = @enumFromInt(@as(u32, @intCast(i)));
                    std.debug.print("  stmt {d}: {any}\n", .{ i, self.store.getCFStmt(window_id) });
                }

                switch (self.store.getCFStmt(id)) {
                    .assign_call => |assign| {
                        const callee_proc = self.store.getProcSpec(assign.proc);
                        std.debug.print(
                            "LIR/interpreter failing assign_call callee proc {d}: name={d} body={any} ret_layout={d} hosted={any}\n",
                            .{
                                @intFromEnum(assign.proc),
                                callee_proc.name.raw(),
                                callee_proc.body,
                                @intFromEnum(callee_proc.ret_layout),
                                callee_proc.hosted,
                            },
                        );
                        if (callee_proc.body) |body| {
                            self.debugPrintStmtChain(body, 20);
                        }
                    },
                    else => {},
                }

                self.invariantFailed(
                    "LIR/interpreter invariant violated: proc {d} stmt {d}={any} assigned local {d} layout {d} invalid value shape: {s}",
                    .{
                        @intFromEnum(proc_id),
                        @intFromEnum(id),
                        self.store.getCFStmt(id),
                        @intFromEnum(local_id),
                        @intFromEnum(layout_idx),
                        reason,
                    },
                );
            }

            self.invariantFailed(
                "LIR/interpreter invariant violated: proc {d} assigned local {d} layout {d} invalid value shape: {s}",
                .{
                    @intFromEnum(proc_id),
                    @intFromEnum(local_id),
                    @intFromEnum(layout_idx),
                    reason,
                },
            );
        }
        unreachable;
    }

    fn debugPrintStmtChain(self: *LirInterpreter, start_stmt: CFStmtId, limit: usize) void {
        if (comptime builtin.target.os.tag == .freestanding) return;
        std.debug.print(
            "LIR/interpreter stmt chain from {d}:\n",
            .{@intFromEnum(start_stmt)},
        );

        var current = start_stmt;
        var remaining = limit;
        while (remaining > 0) : (remaining -= 1) {
            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_symbol => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_ref => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_literal => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_call => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_call_indirect => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_low_level => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_list => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_struct => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_tag => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .set_local => |assign| std.debug.print(
                    "  stmt {d}: {any} target_layout={d} target_layout_data={any}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                        self.layout_store.getLayout(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                else => std.debug.print("  stmt {d}: {any}\n", .{ @intFromEnum(current), stmt }),
            }
            current = switch (stmt) {
                .assign_symbol => |assign| assign.next,
                .assign_ref => |assign| assign.next,
                .assign_literal => |assign| assign.next,
                .assign_call => |assign| assign.next,
                .assign_call_indirect => |assign| assign.next,
                .assign_low_level => |assign| assign.next,
                .assign_list => |assign| assign.next,
                .assign_struct => |assign| assign.next,
                .assign_tag => |assign| assign.next,
                .set_local => |assign| assign.next,
                .debug => |stmt_next| stmt_next.next,
                .expect => |stmt_next| stmt_next.next,
                .incref => |stmt_next| stmt_next.next,
                .decref => |stmt_next| stmt_next.next,
                .free => |stmt_next| stmt_next.next,
                .borrow_scope => |scope| scope.body,
                .join => |join_stmt| join_stmt.body,
                .switch_stmt,
                .runtime_error,
                .scope_exit,
                .for_list,
                .jump,
                .ret,
                .crash,
                .loop_continue,
                => break,
            };
        }
    }

    fn debugPrintIndexedLayoutShape(self: *LirInterpreter, index: usize, layout_idx: layout_mod.Idx) void {
        std.debug.print("  field[{d}] layout tree:\n", .{index});
        var visited = std.ArrayList(u32).empty;
        defer visited.deinit(self.allocator);
        self.debugPrintLayoutShapeLines(layout_idx, 2, &visited);
    }

    fn debugPrintLayoutShape(self: *LirInterpreter, label: []const u8, layout_idx: layout_mod.Idx) void {
        std.debug.print("{s}:\n", .{label});
        var visited = std.ArrayList(u32).empty;
        defer visited.deinit(self.allocator);
        self.debugPrintLayoutShapeLines(layout_idx, 1, &visited);
    }

    fn debugPrintLayoutShapeLines(
        self: *LirInterpreter,
        layout_idx: layout_mod.Idx,
        indent: usize,
        visited: *std.ArrayList(u32),
    ) void {
        for (visited.items) |existing| {
            if (existing == @intFromEnum(layout_idx)) {
                std.debug.print("{s}{d} (cycle)\n", .{ debugIndent(indent), @intFromEnum(layout_idx) });
                return;
            }
        }

        visited.append(self.allocator, @intFromEnum(layout_idx)) catch return;
        defer _ = visited.pop();

        const layout_val = self.layout_store.getLayout(layout_idx);
        std.debug.print("{s}{d}: {s}\n", .{ debugIndent(indent), @intFromEnum(layout_idx), @tagName(layout_val.tag) });
        switch (layout_val.tag) {
            .scalar, .zst, .box_of_zst, .list_of_zst => {},
            .box => self.debugPrintLayoutShapeLines(layout_val.data.box, indent + 1, visited),
            .list => self.debugPrintLayoutShapeLines(layout_val.data.list, indent + 1, visited),
            .closure => self.debugPrintLayoutShapeLines(layout_val.data.closure.captures_layout_idx, indent + 1, visited),
            .struct_ => {
                const info = self.layout_store.getStructInfo(layout_val);
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(@intCast(i));
                    std.debug.print("{s}field[{d}] semantic_index={d}\n", .{ debugIndent(indent + 1), i, field.index });
                    self.debugPrintLayoutShapeLines(field.layout, indent + 2, visited);
                }
            },
            .tag_union => {
                const info = self.layout_store.getTagUnionInfo(layout_val);
                for (0..info.variants.len) |i| {
                    const variant = info.variants.get(@intCast(i));
                    std.debug.print("{s}variant[{d}]\n", .{ debugIndent(indent + 1), i });
                    self.debugPrintLayoutShapeLines(variant.payload_layout, indent + 2, visited);
                }
            },
        }
    }

    fn debugIndent(indent: usize) []const u8 {
        const spaces = "                                ";
        return spaces[0..@min(indent * 2, spaces.len)];
    }

    fn evalProcSpec(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        proc_spec: LirProcSpec,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
    ) Error!Value {
        if (self.call_depth >= max_call_depth) {
            return self.triggerCrash(stack_overflow_message);
        }
        if (args.len != arg_layouts.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} received {d} args but {d} arg layouts",
                .{ proc_spec.name.raw(), args.len, arg_layouts.len },
            );
        }

        if (proc_spec.hosted) |hosted| {
            const param_layouts = try self.localLayoutsFromSpan(proc_spec.args);
            const normalized_args = try self.arena.allocator().alloc(Value, args.len);
            for (args, arg_layouts, param_layouts, 0..) |arg, arg_layout, param_layout, i| {
                normalized_args[i] = try self.coerceExplicitRefValueToLayout(arg, arg_layout, param_layout);
            }
            return self.callHostedProc(hosted, normalized_args, param_layouts, proc_spec.ret_layout);
        }

        trace.log(
            "enter proc={d} name={d} depth={d} args={d}",
            .{
                @intFromEnum(proc_id),
                proc_spec.name.raw(),
                self.call_depth,
                args.len,
            },
        );
        self.call_depth += 1;
        defer self.call_depth -= 1;

        var frame = try self.initFrame(proc_id, proc_spec);
        defer frame.deinit(self.allocator);

        const params = self.store.getLocalSpan(proc_spec.args);
        if (params.len != args.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} expected {d} args but got {d}",
                .{ proc_spec.name.raw(), params.len, args.len },
            );
        }
        if (params.len != arg_layouts.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} expected {d} arg layouts but got {d}",
                .{ proc_spec.name.raw(), params.len, arg_layouts.len },
            );
        }

        for (params, args, arg_layouts) |param, arg, arg_layout| {
            self.setLocalChecked(
                &frame,
                null,
                param,
                try self.coerceExplicitRefValueToLayout(
                    arg,
                    arg_layout,
                    self.store.getLocal(param).layout_idx,
                ),
            );
        }
        const outcome = try self.execStmtChain(&frame, self.requireProcBody(proc_id, proc_spec), null);
        return switch (outcome) {
            .returned => |ret_local| blk: {
                trace.log(
                    "return proc={d} name={d} depth={d}",
                    .{ @intFromEnum(proc_id), proc_spec.name.raw(), self.call_depth },
                );
                const raw_result = try self.getLocalChecked(&frame, ret_local);
                const raw_layout = self.store.getLocal(ret_local).layout_idx;
                if (builtin.mode == .Debug) {
                    var visited = std.ArrayList(DebugVisitedValue).empty;
                    defer visited.deinit(self.allocator);
                    self.debugAssertValueMatchesLayout(proc_id, null, ret_local, raw_result, raw_layout, &visited);
                }
                const coerced_result = try self.coerceExplicitRefValueToLayout(
                    raw_result,
                    raw_layout,
                    proc_spec.ret_layout,
                );
                if (builtin.mode == .Debug) {
                    var visited = std.ArrayList(DebugVisitedValue).empty;
                    defer visited.deinit(self.allocator);
                    self.debugAssertValueMatchesLayout(proc_id, null, ret_local, coerced_result, proc_spec.ret_layout, &visited);
                }
                break :blk coerced_result;
            },
            .scope_exit => return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} terminated via scope_exit",
                .{proc_spec.name.raw()},
            ),
            .loop_continue => return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} terminated via loop_continue",
                .{proc_spec.name.raw()},
            ),
        };
    }

    fn initFrame(self: *LirInterpreter, proc_id: LirProcSpecId, proc_spec: LirProcSpec) Error!Frame {
        const locals = try self.allocator.alloc(LocalSlot, self.store.locals.items.len);
        @memset(locals, .{ .assigned = false, .val = Value.zst });
        for (locals, 0..) |*slot, idx| {
            const local_id: LocalId = @enumFromInt(@as(u32, @intCast(idx)));
            const layout_idx = self.store.getLocal(local_id).layout_idx;
            if (self.layout_store.getLayout(layout_idx).tag == .zst) {
                slot.assigned = true;
            }
        }

        var frame = Frame{
            .proc_id = proc_id,
            .ret_layout = proc_spec.ret_layout,
            .locals = locals,
        };
        try self.collectJoinPoints(&frame.join_points, self.requireProcBody(proc_id, proc_spec));
        return frame;
    }

    fn requireProcBody(self: *LirInterpreter, proc_id: LirProcSpecId, proc_spec: LirProcSpec) CFStmtId {
        return proc_spec.body orelse self.invariantFailed(
            "LIR/interpreter invariant violated: non-hosted proc {d} missing statement body",
            .{@intFromEnum(proc_id)},
        );
    }

    fn collectJoinPoints(self: *LirInterpreter, join_points: *JoinPointMap, stmt_id: CFStmtId) Error!void {
        const stmt = self.store.getCFStmt(stmt_id);
        switch (stmt) {
            .assign_symbol => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_ref => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_literal => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_call => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_call_indirect => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_low_level => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_list => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_struct => |assign| try self.collectJoinPoints(join_points, assign.next),
            .assign_tag => |assign| try self.collectJoinPoints(join_points, assign.next),
            .set_local => |assign| try self.collectJoinPoints(join_points, assign.next),
            .debug => |debug_stmt| try self.collectJoinPoints(join_points, debug_stmt.next),
            .expect => |expect_stmt| try self.collectJoinPoints(join_points, expect_stmt.next),
            .incref => |inc| try self.collectJoinPoints(join_points, inc.next),
            .decref => |dec| try self.collectJoinPoints(join_points, dec.next),
            .free => |free_stmt| try self.collectJoinPoints(join_points, free_stmt.next),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.collectJoinPoints(join_points, branch.body);
                }
                try self.collectJoinPoints(join_points, switch_stmt.default_branch);
            },
            .borrow_scope => |scope_stmt| {
                try self.collectJoinPoints(join_points, scope_stmt.body);
                try self.collectJoinPoints(join_points, scope_stmt.remainder);
            },
            .for_list => |for_stmt| {
                try self.collectJoinPoints(join_points, for_stmt.body);
                try self.collectJoinPoints(join_points, for_stmt.next);
            },
            .join => |join_stmt| {
                try join_points.put(self.allocator, @intFromEnum(join_stmt.id), .{
                    .params = join_stmt.params,
                    .body = join_stmt.body,
                });
                try self.collectJoinPoints(join_points, join_stmt.body);
                try self.collectJoinPoints(join_points, join_stmt.remainder);
            },
            .runtime_error,
            .scope_exit,
            .loop_continue,
            .jump,
            .ret,
            .crash,
            => {},
        }
    }

    fn execStmtChain(
        self: *LirInterpreter,
        frame: *Frame,
        start_stmt: CFStmtId,
        stop_scope: ?LIR.BorrowScopeId,
    ) Error!ExecOutcome {
        var current = start_stmt;
        while (true) {
            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_symbol => |assign| {
                    self.setLocalChecked(frame, current, assign.target, try self.evalAssignSymbol(assign.symbol, self.store.getLocal(assign.target).layout_idx));
                    current = assign.next;
                },
                .assign_ref => |assign| {
                    const target_layout = self.store.getLocal(assign.target).layout_idx;
                    const value = try self.evalAssignRef(frame, assign.op, assign.result, target_layout);
                    self.setLocalChecked(frame, current, assign.target, value);
                    current = assign.next;
                },
                .assign_literal => |assign| {
                    self.setLocalChecked(frame, current, assign.target, try self.evalLiteral(assign.value));
                    current = assign.next;
                },
                .assign_call => |assign| {
                    const arg_locals = self.store.getLocalSpan(assign.args);
                    const arg_values = try self.collectLocalValues(frame, arg_locals);
                    const result = try self.evalProcById(assign.proc, arg_values, try self.localLayouts(arg_locals));
                    self.setLocalChecked(
                        frame,
                        current,
                        assign.target,
                        try self.coerceExplicitRefValueToLayout(
                            result,
                            self.store.getProcSpec(assign.proc).ret_layout,
                            self.store.getLocal(assign.target).layout_idx,
                        ),
                    );
                    current = assign.next;
                },
                .assign_call_indirect => |assign| {
                    const arg_locals = self.store.getLocalSpan(assign.args);
                    const arg_values = try self.collectLocalValues(frame, arg_locals);
                    const result = try self.evalIndirectCall(
                        frame,
                        assign.closure,
                        arg_values,
                        try self.localLayouts(arg_locals),
                        assign.capture_layout,
                    );
                    self.setLocalChecked(
                        frame,
                        current,
                        assign.target,
                        try self.coerceExplicitRefValueToLayout(
                            result.value,
                            result.layout,
                            self.store.getLocal(assign.target).layout_idx,
                        ),
                    );
                    current = assign.next;
                },
                .assign_low_level => |assign| {
                    const arg_locals = self.store.getLocalSpan(assign.args);
                    const arg_values = try self.collectLocalValues(frame, arg_locals);
                    const arg_layouts = try self.localLayouts(arg_locals);
                    self.setLocalChecked(frame, current, assign.target, try self.evalLowLevel(.{
                        .op = assign.op,
                        .args = arg_values,
                        .arg_layouts = arg_layouts,
                        .ret_layout = self.store.getLocal(assign.target).layout_idx,
                        .callable_proc = null,
                    }));
                    current = assign.next;
                },
                .assign_list => |assign| {
                    self.setLocalChecked(frame, current, assign.target, try self.evalListLiteral(frame, assign.elems, self.store.getLocal(assign.target).layout_idx));
                    current = assign.next;
                },
                .assign_struct => |assign| {
                    self.setLocalChecked(frame, current, assign.target, try self.evalStructLiteral(frame, assign.fields, self.store.getLocal(assign.target).layout_idx));
                    current = assign.next;
                },
                .assign_tag => |assign| {
                    self.setLocalChecked(frame, current, assign.target, try self.evalTagLiteral(frame, assign.discriminant, assign.payload, self.store.getLocal(assign.target).layout_idx));
                    current = assign.next;
                },
                .set_local => |assign| {
                    const target_layout = self.store.getLocal(assign.target).layout_idx;
                    const normalized = try self.coerceExplicitRefValueToLayout(
                        try self.getLocalChecked(frame, assign.value),
                        self.store.getLocal(assign.value).layout_idx,
                        target_layout,
                    );
                    self.setLocalChecked(
                        frame,
                        current,
                        assign.target,
                        try self.materializeLocalValue(normalized, target_layout),
                    );
                    current = assign.next;
                },
                .debug => |debug_stmt| {
                    self.roc_ops.dbg(self.readRocStr(try self.getLocalChecked(frame, debug_stmt.message)));
                    current = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    const cond_local = expect_stmt.condition;
                    const cond_value = try self.readSwitchValue(
                        try self.getLocalChecked(frame, cond_local),
                        self.store.getLocal(cond_local).layout_idx,
                    );
                    if (cond_value == 0) {
                        self.roc_ops.expectFailed("expect failed");
                    }
                    current = expect_stmt.next;
                },
                .runtime_error => {
                    if (builtin.mode == .Debug) {
                        std.debug.print(
                            "LIR/interpreter runtime_error in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 12);
                    }
                    return self.runtimeError("RuntimeError");
                },
                .incref => |inc| {
                    if (builtin.mode == .Debug and !frame.locals[@intFromEnum(inc.value)].assigned) {
                        std.debug.print(
                            "LIR/interpreter invariant violated before incref: local {d} unassigned in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(inc.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 20);
                    }
                    self.performExplicitRcStmt(
                        .incref,
                        try self.getLocalChecked(frame, inc.value),
                        self.store.getLocal(inc.value).layout_idx,
                        inc.count,
                    );
                    current = inc.next;
                },
                .decref => |dec| {
                    if (builtin.mode == .Debug and !frame.locals[@intFromEnum(dec.value)].assigned) {
                        std.debug.print(
                            "LIR/interpreter invariant violated before decref: local {d} unassigned in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(dec.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 20);
                    }
                    self.performExplicitRcStmt(
                        .decref,
                        try self.getLocalChecked(frame, dec.value),
                        self.store.getLocal(dec.value).layout_idx,
                        0,
                    );
                    current = dec.next;
                },
                .free => |free_stmt| {
                    if (builtin.mode == .Debug and !frame.locals[@intFromEnum(free_stmt.value)].assigned) {
                        std.debug.print(
                            "LIR/interpreter invariant violated before free: local {d} unassigned in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(free_stmt.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 20);
                    }
                    self.performExplicitRcStmt(
                        .free,
                        try self.getLocalChecked(frame, free_stmt.value),
                        self.store.getLocal(free_stmt.value).layout_idx,
                        0,
                    );
                    current = free_stmt.next;
                },
                .switch_stmt => |switch_stmt| {
                    const cond_value = try self.readSwitchValue(
                        try self.getLocalChecked(frame, switch_stmt.cond),
                        self.store.getLocal(switch_stmt.cond).layout_idx,
                    );
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    if (trace.enabled) {
                        trace.log(
                            "switch: cond_local={d} layout={any} value={d} branches={d} default={d}",
                            .{
                                @intFromEnum(switch_stmt.cond),
                                self.store.getLocal(switch_stmt.cond).layout_idx,
                                cond_value,
                                branches.len,
                                @intFromEnum(switch_stmt.default_branch),
                            },
                        );
                        for (branches) |branch| {
                            trace.log("  branch value={d} body={d}", .{ branch.value, @intFromEnum(branch.body) });
                        }
                    }
                    var target = switch_stmt.default_branch;
                    for (branches) |branch| {
                        if (branch.value == cond_value) {
                            target = branch.body;
                            break;
                        }
                    }
                    return try self.execStmtChain(frame, target, stop_scope);
                },
                .borrow_scope => |scope_stmt| {
                    const outcome = try self.execStmtChain(frame, scope_stmt.body, scope_stmt.id);
                    switch (outcome) {
                        .returned => |ret_local| return .{ .returned = ret_local },
                        .scope_exit => current = scope_stmt.remainder,
                        .loop_continue => return self.invariantFailedError(
                            "LIR/interpreter invariant violated: loop_continue escaped borrow scope in proc {d}",
                            .{@intFromEnum(frame.proc_id)},
                        ),
                    }
                },
                .scope_exit => |scope_stmt| {
                    if (stop_scope == null or stop_scope.? != scope_stmt.id) {
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: unexpected scope_exit {d} in proc {d}",
                            .{ @intFromEnum(scope_stmt.id), @intFromEnum(frame.proc_id) },
                        );
                    }
                    return .scope_exit;
                },
                .for_list => |for_stmt| {
                    const iterable = try self.getLocalChecked(frame, for_stmt.iterable);
                    const list_layout = self.store.getLocal(for_stmt.iterable).layout_idx;
                    const resolved_iterable = self.resolveListBaseValue(iterable, list_layout);
                    const actual_elem_layout = for_stmt.iterable_elem_layout;
                    const elem_layout = self.store.getLocal(for_stmt.elem).layout_idx;
                    const info = self.listElemInfo(list_layout);
                    const rl = valueToRocList(resolved_iterable.value);

                    var i: usize = 0;
                    while (i < rl.len()) : (i += 1) {
                        const normalized_elem = if (info.width == 0 or rl.bytes == null)
                            try self.coerceExplicitRefValueToLayout(Value.zst, actual_elem_layout, elem_layout)
                        else
                            try self.coerceExplicitRefValueToLayout(
                                .{ .ptr = rl.bytes.? + i * info.width },
                                actual_elem_layout,
                                elem_layout,
                            );
                        const elem_value = try self.materializeLocalValue(normalized_elem, elem_layout);

                        self.setLocalChecked(frame, current, for_stmt.elem, elem_value);
                        const outcome = try self.execStmtChain(frame, for_stmt.body, null);
                        switch (outcome) {
                            .returned => |ret_local| return .{ .returned = ret_local },
                            .loop_continue => {},
                            .scope_exit => return self.invariantFailedError(
                                "LIR/interpreter invariant violated: unexpected scope_exit escaped for_list body in proc {d}",
                                .{@intFromEnum(frame.proc_id)},
                            ),
                        }
                    }

                    current = for_stmt.next;
                },
                .loop_continue => return .loop_continue,
                .join => |join_stmt| {
                    current = join_stmt.remainder;
                },
                .jump => |jump_stmt| {
                    const join_info = frame.join_points.get(@intFromEnum(jump_stmt.target)) orelse self.invariantFailed(
                        "LIR/interpreter invariant violated: missing join point {d} in proc {d}",
                        .{ @intFromEnum(jump_stmt.target), @intFromEnum(frame.proc_id) },
                    );
                    const arg_locals = self.store.getLocalSpan(jump_stmt.args);
                    const arg_values = try self.collectLocalValues(frame, arg_locals);
                    const params = self.store.getLocalSpan(join_info.params);
                    if (params.len != arg_values.len) {
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: jump to join point {d} passed {d} args but target expects {d}",
                            .{ @intFromEnum(jump_stmt.target), arg_values.len, params.len },
                        );
                    }
                    for (params, arg_values, arg_locals) |param, arg, arg_local| {
                        self.setLocalChecked(
                            frame,
                            current,
                            param,
                            try self.coerceExplicitRefValueToLayout(
                                arg,
                                self.store.getLocal(arg_local).layout_idx,
                                self.store.getLocal(param).layout_idx,
                            ),
                        );
                    }
                    current = join_info.body;
                },
                .ret => |ret_stmt| return .{ .returned = ret_stmt.value },
                .crash => |crash_stmt| return self.triggerCrash(self.store.getString(crash_stmt.msg)),
            }
        }
    }

    fn debugDumpProc(self: *LirInterpreter, proc_id: LirProcSpecId) void {
        if (builtin.mode != .Debug) return;
        const proc_spec = self.store.getProcSpec(proc_id);
        const body = proc_spec.body orelse {
            std.debug.print("  proc {d} has no body\n", .{ @intFromEnum(proc_id) });
            return;
        };

        std.debug.print(
            "  proc {d} body={d} ret_layout={d}\n",
            .{ @intFromEnum(proc_id), @intFromEnum(body), @intFromEnum(proc_spec.ret_layout) },
        );
        const args = self.store.getLocalSpan(proc_spec.args);
        if (args.len > 0) {
            std.debug.print("  args:", .{});
            for (args) |arg| {
                const layout_idx = self.store.getLocal(arg).layout_idx;
                std.debug.print(" {d}:{d}", .{ @intFromEnum(arg), @intFromEnum(layout_idx) });
            }
            std.debug.print("\n", .{});
        }
        const local_count = self.store.locals.items.len;
        if (local_count > 0) {
            std.debug.print("  locals:\n", .{});
            for (self.store.locals.items, 0..) |local, idx| {
                const layout_idx = local.layout_idx;
                const layout_val = self.layout_store.getLayout(layout_idx);
                std.debug.print(
                    "    local {d}: layout={d} tag={s}",
                    .{ idx, @intFromEnum(layout_idx), @tagName(layout_val.tag) },
                );
                if (layout_val.tag == .tag_union) {
                    const tu_info = self.layout_store.getTagUnionInfo(layout_val);
                    std.debug.print(" variants={d}", .{tu_info.variants.len});
                }
                std.debug.print("\n", .{});
            }
        }

        var visited = std.AutoHashMap(CFStmtId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayListUnmanaged(CFStmtId){};
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, body) catch return;

        while (stack.items.len > 0) {
            const stmt_id = stack.pop().?;
            if (visited.contains(stmt_id)) continue;
            visited.put(stmt_id, {}) catch return;
            const stmt = self.store.getCFStmt(stmt_id);
            switch (stmt) {
                .assign_symbol => |assign| {
                    std.debug.print("    {d}: assign_symbol target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_ref => |assign| {
                    std.debug.print("    {d}: assign_ref target={d} op={any} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        assign.op,
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_literal => |assign| {
                    std.debug.print("    {d}: assign_literal target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_call => |assign| {
                    std.debug.print("    {d}: assign_call proc={d} target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.proc),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_call_indirect => |assign| {
                    std.debug.print("    {d}: assign_call_indirect target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_low_level => |assign| {
                    std.debug.print("    {d}: assign_low_level target={d} op={s} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @tagName(assign.op),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_list => |assign| {
                    std.debug.print("    {d}: assign_list target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_struct => |assign| {
                    std.debug.print("    {d}: assign_struct target={d} fields=", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                    });
                    for (self.store.getLocalSpan(assign.fields)) |field_local| {
                        std.debug.print("{d} ", .{@intFromEnum(field_local)});
                    }
                    std.debug.print("next={d}\n", .{
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .assign_tag => |assign| {
                    std.debug.print("    {d}: assign_tag target={d} discrim={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        assign.discriminant,
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .set_local => |assign| {
                    std.debug.print("    {d}: set_local target={d} value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.value),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.allocator, assign.next) catch return;
                },
                .debug => |debug_stmt| {
                    std.debug.print("    {d}: debug next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(debug_stmt.next),
                    });
                    stack.append(self.allocator, debug_stmt.next) catch return;
                },
                .expect => |expect_stmt| {
                    std.debug.print("    {d}: expect cond={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(expect_stmt.condition),
                        @intFromEnum(expect_stmt.next),
                    });
                    stack.append(self.allocator, expect_stmt.next) catch return;
                },
                .runtime_error => {
                    std.debug.print("    {d}: runtime_error\n", .{ @intFromEnum(stmt_id) });
                },
                .incref => |inc| {
                    std.debug.print("    {d}: incref value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(inc.value),
                        @intFromEnum(inc.next),
                    });
                    stack.append(self.allocator, inc.next) catch return;
                },
                .decref => |dec| {
                    std.debug.print("    {d}: decref value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(dec.value),
                        @intFromEnum(dec.next),
                    });
                    stack.append(self.allocator, dec.next) catch return;
                },
                .free => |dec| {
                    std.debug.print("    {d}: free value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(dec.value),
                        @intFromEnum(dec.next),
                    });
                    stack.append(self.allocator, dec.next) catch return;
                },
                .switch_stmt => |switch_stmt| {
                    std.debug.print("    {d}: switch cond={d} default={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(switch_stmt.cond),
                        @intFromEnum(switch_stmt.default_branch),
                    });
                    stack.append(self.allocator, switch_stmt.default_branch) catch return;
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    for (branches) |branch| {
                        std.debug.print("        branch {d} -> {d}\n", .{
                            branch.value,
                            @intFromEnum(branch.body),
                        });
                        stack.append(self.allocator, branch.body) catch return;
                    }
                },
                .borrow_scope => |scope| {
                    std.debug.print("    {d}: borrow_scope body={d} remainder={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(scope.body),
                        @intFromEnum(scope.remainder),
                    });
                    stack.append(self.allocator, scope.body) catch return;
                    stack.append(self.allocator, scope.remainder) catch return;
                },
                .scope_exit => |scope| {
                    std.debug.print("    {d}: scope_exit id={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(scope.id),
                    });
                },
                .for_list => |for_list| {
                    std.debug.print("    {d}: for_list elem={d} iterable={d} body={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(for_list.elem),
                        @intFromEnum(for_list.iterable),
                        @intFromEnum(for_list.body),
                        @intFromEnum(for_list.next),
                    });
                    stack.append(self.allocator, for_list.body) catch return;
                    stack.append(self.allocator, for_list.next) catch return;
                },
                .loop_continue => {
                    std.debug.print("    {d}: loop_continue\n", .{ @intFromEnum(stmt_id) });
                },
                .join => |join| {
                    std.debug.print("    {d}: join body={d} remainder={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(join.body),
                        @intFromEnum(join.remainder),
                    });
                    stack.append(self.allocator, join.body) catch return;
                    stack.append(self.allocator, join.remainder) catch return;
                },
                .jump => |jump| {
                    std.debug.print("    {d}: jump target={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(jump.target),
                    });
                },
                .ret => |ret| {
                    std.debug.print("    {d}: ret value={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(ret.value),
                    });
                },
                .crash => |crash| {
                    std.debug.print("    {d}: crash msg={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(crash.msg),
                    });
                },
            }
        }
    }

    fn collectLocalValues(self: *LirInterpreter, frame: *const Frame, locals: []const LocalId) Error![]Value {
        if (locals.len == 0) return &.{};
        const values = try self.arena.allocator().alloc(Value, locals.len);
        for (locals, 0..) |local_id, i| {
            values[i] = try self.getLocalChecked(frame, local_id);
        }
        return values;
    }

    fn localLayouts(self: *LirInterpreter, locals: []const LocalId) Error![]layout_mod.Idx {
        if (locals.len == 0) return &.{};
        const layouts = try self.arena.allocator().alloc(layout_mod.Idx, locals.len);
        for (locals, 0..) |local_id, i| layouts[i] = self.store.getLocal(local_id).layout_idx;
        return layouts;
    }

    fn localLayoutsFromSpan(self: *LirInterpreter, locals: LocalSpan) Error![]const layout_mod.Idx {
        const local_ids = self.store.getLocalSpan(locals);
        const layouts = try self.arena.allocator().alloc(layout_mod.Idx, local_ids.len);
        for (local_ids, 0..) |local_id, i| layouts[i] = self.store.getLocal(local_id).layout_idx;
        return layouts;
    }

    const IndirectCallResult = struct {
        value: Value,
        layout: layout_mod.Idx,
    };

    fn readSwitchValue(self: *LirInterpreter, value: Value, layout_idx: layout_mod.Idx) Error!u64 {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .tag_union => {
                if (self.helper.sizeOf(layout_idx) == 0) return 0;
                const tu_info = self.layout_store.getTagUnionInfo(layout_val);
                return tu_info.readDiscriminant(value.ptr);
            },
            else => switch (self.helper.sizeOf(layout_idx)) {
                0 => 0,
                1 => value.read(u8),
                2 => value.read(u16),
                4 => value.read(u32),
                8 => value.read(u64),
                else => return self.invariantFailedError(
                    "LIR/interpreter invariant violated: switch condition layout {d} is not a supported scalar width",
                    .{@intFromEnum(layout_idx)},
                ),
            },
        };
    }

    fn evalAssignSymbol(self: *LirInterpreter, symbol: Symbol, _: layout_mod.Idx) Error!Value {
        return self.invariantFailedError(
            "LIR/interpreter TODO: assign_symbol for symbol {d} is not implemented yet",
            .{symbol.raw()},
        );
    }

    fn materializeRefResult(
        self: *LirInterpreter,
        value: Value,
        result: LIR.ResultSemantics,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        if (result != .fresh) return value;

        const size = self.helper.sizeOf(target_layout);
        if (size == 0 or value.isZst()) return Value.zst;

        const copy = try self.alloc(target_layout);
        copy.copyFrom(value, size);
        return copy;
    }

    fn materializeLocalValue(
        self: *LirInterpreter,
        value: Value,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const size = self.helper.sizeOf(target_layout);
        if (size == 0 or value.isZst()) return Value.zst;

        const storage = try self.alloc(target_layout);
        storage.copyFrom(value, size);
        return storage;
    }

    fn evalAssignRef(
        self: *LirInterpreter,
        frame: *const Frame,
        op: LIR.RefOp,
        result: LIR.ResultSemantics,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        return switch (op) {
            .local => |source| blk: {
                const local_value = try self.coerceExplicitRefValueToLayout(
                    try self.getLocalChecked(frame, source),
                    self.store.getLocal(source).layout_idx,
                    target_layout,
                );
                break :blk try self.materializeRefResult(local_value, result, target_layout);
            },
            .field => |field| blk: {
                const source_val = try self.getLocalChecked(frame, field.source);
                const source_layout = self.store.getLocal(field.source).layout_idx;
                const struct_base = self.resolveStructBaseValue(source_val, source_layout);
                const struct_layout_val = self.layout_store.getLayout(struct_base.layout);
                const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                    struct_layout_val.data.struct_.idx,
                    field.field_idx,
                );
                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                    struct_layout_val.data.struct_.idx,
                    field.field_idx,
                );
                const field_value = try self.coerceExplicitRefValueToLayout(
                    struct_base.value.offset(field_offset),
                    actual_field_layout,
                    target_layout,
                );
                const target_layout_val = self.layout_store.getLayout(target_layout);
                if (builtin.mode == .Debug and
                    self.helper.sizeOf(target_layout) > 0 and
                    target_layout_val.tag != .box_of_zst and
                    field_value.isZst())
                {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: field projection source_local={d} source_layout={d} base_layout={d} field_idx={d} actual_field_layout={d} target_layout={d} normalized to ZST",
                        .{
                            @intFromEnum(field.source),
                            @intFromEnum(source_layout),
                            @intFromEnum(struct_base.layout),
                            field.field_idx,
                            @intFromEnum(actual_field_layout),
                            @intFromEnum(target_layout),
                        },
                    );
                }
                break :blk try self.materializeRefResult(field_value, result, target_layout);
            },
            .tag_payload => |payload| blk: {
                const source_val = try self.getLocalChecked(frame, payload.source);
                const source_layout = self.store.getLocal(payload.source).layout_idx;
                const tag_base = self.resolveTagUnionBaseValue(source_val, source_layout);
                const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
                if (builtin.mode == .Debug and disc != payload.tag_discriminant) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: tag payload access expected discriminant {d} but observed {d}",
                        .{ payload.tag_discriminant, disc },
                    );
                }
                const actual_payload_layout = self.tagPayloadLayout(source_layout, payload.tag_discriminant);
                const payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
                switch (payload_layout_val.tag) {
                    .struct_ => {
                        const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                            payload_layout_val.data.struct_.idx,
                            payload.payload_idx,
                        );
                        const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                            payload_layout_val.data.struct_.idx,
                            payload.payload_idx,
                        );
                        const payload_value = try self.coerceExplicitRefValueToLayout(
                            tag_base.value.offset(field_offset),
                            actual_field_layout,
                            target_layout,
                        );
                        break :blk try self.materializeRefResult(payload_value, result, target_layout);
                    },
                    else => {
                        if (builtin.mode == .Debug and payload.payload_idx != 0) {
                            self.invariantFailed(
                                "LIR/interpreter invariant violated: scalar tag payload access requested payload_idx {d} from non-struct payload layout {d}",
                                .{ payload.payload_idx, @intFromEnum(actual_payload_layout) },
                            );
                        }
                        const payload_value = try self.coerceExplicitRefValueToLayout(tag_base.value, actual_payload_layout, target_layout);
                        break :blk try self.materializeRefResult(payload_value, result, target_layout);
                    },
                }
            },
            .tag_payload_struct => |payload| blk: {
                const source_val = try self.getLocalChecked(frame, payload.source);
                const source_layout = self.store.getLocal(payload.source).layout_idx;
                const tag_base = self.resolveTagUnionBaseValue(source_val, source_layout);
                const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
                if (builtin.mode == .Debug and disc != payload.tag_discriminant) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: tag payload struct access expected discriminant {d} but observed {d}",
                        .{ payload.tag_discriminant, disc },
                    );
                }
                const actual_payload_layout = self.tagPayloadLayout(source_layout, payload.tag_discriminant);
                const payload_value = try self.coerceExplicitRefValueToLayout(tag_base.value, actual_payload_layout, target_layout);
                break :blk try self.materializeRefResult(payload_value, result, target_layout);
            },
            .list_reinterpret => |list_bridge| blk: {
                const bridged = try self.coerceExplicitListValueToLayout(
                    try self.getLocalChecked(frame, list_bridge.backing_ref),
                    self.store.getLocal(list_bridge.backing_ref).layout_idx,
                    target_layout,
                );
                break :blk try self.materializeRefResult(bridged, result, target_layout);
            },
            .nominal => |nominal| blk: {
                const bridged = try self.coerceExplicitNominalValueToLayout(
                    try self.getLocalChecked(frame, nominal.backing_ref),
                    self.store.getLocal(nominal.backing_ref).layout_idx,
                    target_layout,
                );
                break :blk try self.materializeRefResult(bridged, result, target_layout);
            },
            .discriminant => |discriminant| blk: {
                const source_val = try self.getLocalChecked(frame, discriminant.source);
                const source_layout = self.store.getLocal(discriminant.source).layout_idx;
                const tag_base = self.resolveTagUnionBaseValue(source_val, source_layout);
                const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
                const disc_value = try self.alloc(target_layout);
                switch (self.helper.sizeOf(target_layout)) {
                    1 => disc_value.write(u8, @intCast(disc)),
                    2 => disc_value.write(u16, disc),
                    4 => disc_value.write(u32, disc),
                    8 => disc_value.write(u64, disc),
                    else => self.invariantFailed(
                        "LIR/interpreter invariant violated: discriminant local has unsupported layout {d}",
                        .{@intFromEnum(target_layout)},
                    ),
                }
                break :blk try self.materializeRefResult(disc_value, result, target_layout);
            },
        };
    }

    fn evalLiteral(self: *LirInterpreter, literal: LIR.LiteralValue) Error!Value {
        return switch (literal) {
            .i64_literal => |lit| self.evalI64Literal(lit.value, lit.layout_idx),
            .i128_literal => |lit| self.evalI128Literal(lit.value, lit.layout_idx),
            .f64_literal => |value| self.evalF64Literal(value),
            .f32_literal => |value| self.evalF32Literal(value),
            .dec_literal => |value| self.evalDecLiteral(value),
            .str_literal => |idx| self.evalStrLiteral(idx),
            .bool_literal => |value| self.evalBoolLiteral(value),
            .null_ptr => self.evalNullPtrLiteral(),
            .proc_ref => |proc_id| self.evalProcRefLiteral(proc_id),
        };
    }

    fn evalNullPtrLiteral(self: *LirInterpreter) Error!Value {
        const val = try self.alloc(.opaque_ptr);
        switch (self.layout_store.targetUsize().size()) {
            4 => val.write(u32, 0),
            8 => val.write(usize, 0),
            else => unreachable,
        }
        return val;
    }

    fn evalProcRefLiteral(self: *LirInterpreter, proc_id: LIR.LirProcSpecId) Error!Value {
        const val = try self.alloc(.opaque_ptr);
        const encoded: usize = @intFromEnum(proc_id) + 1;
        switch (self.layout_store.targetUsize().size()) {
            4 => val.write(u32, @intCast(encoded)),
            8 => val.write(usize, encoded),
            else => unreachable,
        }
        return val;
    }

    fn decodeProcRef(self: *LirInterpreter, value: Value) LIR.LirProcSpecId {
        const encoded: usize = switch (self.layout_store.targetUsize().size()) {
            4 => value.read(u32),
            8 => value.read(usize),
            else => unreachable,
        };
        if (encoded == 0) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: attempted indirect call through null function pointer",
                .{},
            );
        }
        return @enumFromInt(@as(u32, @intCast(encoded - 1)));
    }

    fn evalIndirectCall(
        self: *LirInterpreter,
        frame: *Frame,
        closure_local: LocalId,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
        capture_layout: ?layout_mod.Idx,
    ) Error!IndirectCallResult {
        const closure_layout = self.store.getLocal(closure_local).layout_idx;
        const closure_value = try self.getLocalChecked(frame, closure_local);
        const closure_base = self.resolveStructBaseValue(closure_value, closure_layout);
        const closure_layout_val = self.layout_store.getLayout(closure_base.layout);
        if (closure_layout_val.tag != .struct_) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: indirect call closure local {d} does not have struct layout",
                .{@intFromEnum(closure_local)},
            );
        }

        const fn_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(closure_layout_val.data.struct_.idx, 0);
        const fn_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(closure_layout_val.data.struct_.idx, 0);
        if (fn_layout != .opaque_ptr) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: indirect call closure field 0 layout {d} is not opaque_ptr",
                .{@intFromEnum(fn_layout)},
            );
        }

        const proc_id = self.decodeProcRef(closure_base.value.offset(fn_offset));
        const proc_spec = self.store.getProcSpec(proc_id);
        const proc_args = self.store.getLocalSpan(proc_spec.args);
        const has_capture_arg = capture_layout != null;

        if (!has_capture_arg and proc_args.len == args.len) {
            return .{
                .value = try self.evalProcById(proc_id, args, arg_layouts),
                .layout = proc_spec.ret_layout,
            };
        }

        if (!has_capture_arg or proc_args.len != args.len + 1) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: indirect callee {d} expects {d} args, but call site provided {d} or {d} with captures",
                .{ @intFromEnum(proc_id), proc_args.len, args.len, args.len + 1 },
            );
        }

        const capture_layout_idx = capture_layout.?;
        const capture_value = blk: {
            if (capture_layout_idx == .zst) break :blk Value.zst;

            const capture_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(closure_layout_val.data.struct_.idx, 1);
            const capture_ptr_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(closure_layout_val.data.struct_.idx, 1);
            const capture_ptr_value = self.normalizeValueToLayout(
                closure_base.value.offset(capture_offset),
                capture_ptr_layout,
                capture_ptr_layout,
            );
            const capture_ptr = self.readBoxedDataPointer(capture_ptr_value) orelse self.invariantFailed(
                "LIR/interpreter invariant violated: indirect call capture pointer is null",
                .{},
            );
            const result = try self.alloc(capture_layout_idx);
            const size = self.helper.sizeOf(capture_layout_idx);
            if (size > 0) {
                result.copyFrom(.{ .ptr = capture_ptr }, size);
            }
            break :blk result;
        };

        const all_args = try self.arena.allocator().alloc(Value, args.len + 1);
        @memcpy(all_args[0..args.len], args);
        all_args[args.len] = capture_value;
        const all_arg_layouts = try self.arena.allocator().alloc(layout_mod.Idx, arg_layouts.len + 1);
        @memcpy(all_arg_layouts[0..arg_layouts.len], arg_layouts);
        all_arg_layouts[arg_layouts.len] = capture_layout_idx;
        return .{
            .value = try self.evalProcById(proc_id, all_args, all_arg_layouts),
            .layout = proc_spec.ret_layout,
        };
    }

    const AllocatedStruct = struct {
        outer: Value,
        base: Value,
        base_layout: layout_mod.Idx,
    };

    fn allocStructValue(self: *LirInterpreter, struct_layout: layout_mod.Idx) Error!AllocatedStruct {
        const struct_layout_val = self.layout_store.getLayout(struct_layout);
        switch (struct_layout_val.tag) {
            .zst => return .{
                .outer = Value.zst,
                .base = Value.zst,
                .base_layout = .zst,
            },
            .box_of_zst => return .{
                .outer = Value.zst,
                .base = Value.zst,
                .base_layout = .zst,
            },
            .box => {
                const box_info = self.layout_store.getBoxInfo(struct_layout_val);
                const data_ptr = try self.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_refcounted,
                );
                @memset(data_ptr[0..box_info.elem_size], 0);
                const boxed = try self.alloc(struct_layout);
                if (self.layout_store.targetUsize().size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return .{
                    .outer = boxed,
                    .base = .{ .ptr = data_ptr },
                    .base_layout = struct_layout_val.data.box,
                };
            },
            .struct_ => {
                const outer = try self.alloc(struct_layout);
                return .{
                    .outer = outer,
                    .base = outer,
                    .base_layout = struct_layout,
                };
            },
            else => self.invariantFailed(
                "LIR/interpreter invariant violated: assign_struct target layout {d} is not a struct or boxed struct",
                .{@intFromEnum(struct_layout)},
            ),
        }
    }

    fn evalStructLiteral(self: *LirInterpreter, frame: *const Frame, fields: LocalSpan, struct_layout: layout_mod.Idx) Error!Value {
        const field_locals = self.store.getLocalSpan(fields);
        const allocated = try self.allocStructValue(struct_layout);
        const base_layout_val = self.layout_store.getLayout(allocated.base_layout);
        if (base_layout_val.tag != .struct_) {
            if (field_locals.len != 0) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: boxed/zst struct literal for layout {d} had {d} fields but no struct base layout",
                    .{ @intFromEnum(struct_layout), field_locals.len },
                );
            }
            return allocated.outer;
        }
        const expected_info = self.layout_store.getStructInfo(base_layout_val);
        if (builtin.mode == .Debug and field_locals.len != expected_info.fields.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: struct literal for layout {d} had {d} fields but layout expects {d}",
                .{ @intFromEnum(struct_layout), field_locals.len, expected_info.fields.len },
            );
        }
        for (field_locals, 0..) |field_local, i| {
            const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                base_layout_val.data.struct_.idx,
                @intCast(i),
            );
            const field_size = self.helper.sizeOf(field_layout);
            if (field_size == 0) continue;
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                base_layout_val.data.struct_.idx,
                @intCast(i),
            );
            const field_value = try self.coerceExplicitRefValueToLayout(
                try self.getLocalChecked(frame, field_local),
                self.store.getLocal(field_local).layout_idx,
                field_layout,
            );
            if (builtin.mode == .Debug and field_value.isZst()) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: struct field local {d} in proc {d} had ZST value for non-ZST layout {d} (local_layout={d}, local_layout_data={any}, field_layout_data={any}, struct_layout_data={any}, field index {d} of struct layout {d})",
                    .{
                        @intFromEnum(field_local),
                        @intFromEnum(frame.proc_id),
                        @intFromEnum(field_layout),
                        @intFromEnum(self.store.getLocal(field_local).layout_idx),
                        self.layout_store.getLayout(self.store.getLocal(field_local).layout_idx),
                        self.layout_store.getLayout(field_layout),
                        self.layout_store.getLayout(struct_layout),
                        i,
                        @intFromEnum(struct_layout),
                    },
                );
            }
            allocated.base.offset(field_offset).copyFrom(field_value, field_size);
        }
        return allocated.outer;
    }

    const AllocatedTag = struct {
        outer: Value,
        base: Value,
        base_layout: layout_mod.Idx,
    };

    fn allocTagValue(self: *LirInterpreter, union_layout: layout_mod.Idx) Error!AllocatedTag {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const box_info = self.layout_store.getBoxInfo(union_layout_val);
            const data_ptr = try self.allocRocDataWithRc(
                box_info.elem_size,
                box_info.elem_alignment,
                box_info.contains_refcounted,
            );
            @memset(data_ptr[0..box_info.elem_size], 0);
            const boxed = try self.alloc(union_layout);
            if (self.layout_store.targetUsize().size() == 8) {
                boxed.write(usize, @intFromPtr(data_ptr));
            } else {
                boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
            }
            return .{
                .outer = boxed,
                .base = .{ .ptr = data_ptr },
                .base_layout = union_layout_val.data.box,
            };
        }

        const outer = try self.alloc(union_layout);
        return .{
            .outer = outer,
            .base = outer,
            .base_layout = union_layout,
        };
    }

    fn evalTagLiteral(
        self: *LirInterpreter,
        frame: *const Frame,
        discriminant: u16,
        payload_local: ?LocalId,
        union_layout: layout_mod.Idx,
    ) Error!Value {
        const allocated = try self.allocTagValue(union_layout);
        self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, discriminant);

        const payload_layout = self.tagPayloadLayout(union_layout, discriminant);
        if (payload_local) |local| {
            const payload_size = self.helper.sizeOf(payload_layout);
            if (payload_size > 0) {
                const payload_value = try self.coerceExplicitRefValueToLayout(
                    try self.getLocalChecked(frame, local),
                    self.store.getLocal(local).layout_idx,
                    payload_layout,
                );
                allocated.base.copyFrom(payload_value, payload_size);
            }
        }

        return allocated.outer;
    }

    fn evalListLiteral(self: *LirInterpreter, frame: *const Frame, elems: LocalSpan, list_layout: layout_mod.Idx) Error!Value {
        const elem_layout = self.listElemLayout(list_layout);
        const elem_size = self.helper.sizeOf(elem_layout);
        const elem_locals = self.store.getLocalSpan(elems);
        if (elem_locals.len == 0) {
            return self.rocListToValue(.{
                .bytes = null,
                .length = 0,
                .capacity_or_alloc_ptr = 0,
            }, list_layout);
        }
        if (elem_size == 0) {
            var crash_boundary = self.enterCrashBoundary();
            defer crash_boundary.deinit();
            const sj = crash_boundary.set();
            if (sj != 0) return error.Crash;
            const sa = self.helper.sizeAlignOf(elem_layout);
            const elems_rc = self.builtinInternalContainsRefcounted("interpreter.assign_list.zst_elem_rc", elem_layout);
            const list = builtins.list.RocList.list_allocate(
                @intCast(sa.alignment.toByteUnits()),
                elem_locals.len,
                elem_size,
                elems_rc,
                &self.roc_ops,
            );
            return self.rocListToValue(list, list_layout);
        }

        const total_elem_bytes = elem_size * elem_locals.len;
        const sa = self.helper.sizeAlignOf(elem_layout);
        const elem_alignment: u32 = @intCast(sa.alignment.toByteUnits());
        const elems_rc = self.builtinInternalContainsRefcounted("interpreter.assign_list.elem_rc", elem_layout);
        const elem_data = try self.allocRocDataWithRc(total_elem_bytes, elem_alignment, elems_rc);
        const elem_layout_val = self.layout_store.getLayout(elem_layout);
        for (elem_locals, 0..) |elem_local, i| {
            const offset = i * elem_size;
            const elem_value = try self.coerceExplicitRefValueToLayout(
                try self.getLocalChecked(frame, elem_local),
                self.store.getLocal(elem_local).layout_idx,
                elem_layout,
            );
            if (builtin.mode == .Debug and elem_layout_val.tag == .box and self.readBoxedDataPointer(elem_value) == null) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: list literal source local {d} in proc {d} had null boxed element for list elem layout {d}",
                    .{ @intFromEnum(elem_local), @intFromEnum(frame.proc_id), @intFromEnum(elem_layout) },
                );
            }
            @memcpy(elem_data[offset..][0..elem_size], elem_value.readBytes(elem_size));
            if (builtin.mode == .Debug and elem_layout_val.tag == .box and self.readBoxedDataPointer(.{ .ptr = elem_data + offset }) == null) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: list literal wrote null boxed element at index {d} from local {d} in proc {d} for elem layout {d}",
                    .{ i, @intFromEnum(elem_local), @intFromEnum(frame.proc_id), @intFromEnum(elem_layout) },
                );
            }
        }

        return self.rocListToValue(.{
            .bytes = elem_data,
            .length = elem_locals.len,
            .capacity_or_alloc_ptr = elem_locals.len,
        }, list_layout);
    }

    fn callHostedProc(
        self: *LirInterpreter,
        hosted: LIR.HostedProc,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
        ret_layout: layout_mod.Idx,
    ) Error!Value {
        var total_args_size: usize = 0;
        for (arg_layouts) |arg_layout| {
            const sa = self.helper.sizeAlignOf(arg_layout);
            total_args_size = std.mem.alignForward(usize, total_args_size, sa.alignment.toByteUnits());
            total_args_size += sa.size;
        }

        const args_buf_size = @max(total_args_size, 8);
        const args_buf = try self.arena.allocator().alloc(u8, args_buf_size);
        @memset(args_buf, 0);

        var offset: usize = 0;
        for (args, arg_layouts) |arg, arg_layout| {
            const sa = self.helper.sizeAlignOf(arg_layout);
            offset = std.mem.alignForward(usize, offset, sa.alignment.toByteUnits());
            if (sa.size > 0 and !arg.isZst()) {
                @memcpy(args_buf[offset .. offset + sa.size], arg.readBytes(sa.size));
            }
            offset += sa.size;
        }

        const ret_size = self.helper.sizeOf(ret_layout);
        const ret_buf = try self.arena.allocator().alloc(u8, @max(ret_size, 1));
        @memset(ret_buf, 0);

        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;

        const hosted_fn = self.roc_ops.hosted_fns.fns[hosted.index];
        const ops_for_host = self.currentRocOps();
        hosted_fn(@ptrCast(ops_for_host), @ptrCast(ret_buf.ptr), @ptrCast(args_buf.ptr));

        if (self.roc_env.crashed) return error.Crash;
        if (ret_size == 0) return Value.zst;

        const result = try self.alloc(ret_layout);
        @memcpy(result.ptr[0..ret_size], ret_buf[0..ret_size]);
        return result;
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
        return self.makeStaticRocStrLiteral(str_bytes);
    }

    fn evalBoolLiteral(self: *LirInterpreter, b: bool) Error!Value {
        const val = try self.alloc(.bool);
        val.write(u8, if (b) 1 else 0);
        return val;
    }

    // String helpers (RocStr construction)

    fn makeStaticRocStrLiteral(self: *LirInterpreter, bytes: []const u8) Error!Value {
        if (RocStr.fitsInSmallStr(bytes.len)) {
            const small = RocStr.fromSliceSmall(bytes);
            return self.rocStrToValue(small, .str);
        }

        const total_bytes = @sizeOf(usize) + bytes.len;
        const storage = switch (@alignOf(usize)) {
            1 => self.arena.allocator().alignedAlloc(u8, .@"1", total_bytes),
            2 => self.arena.allocator().alignedAlloc(u8, .@"2", total_bytes),
            4 => self.arena.allocator().alignedAlloc(u8, .@"4", total_bytes),
            8 => self.arena.allocator().alignedAlloc(u8, .@"8", total_bytes),
            16 => self.arena.allocator().alignedAlloc(u8, .@"16", total_bytes),
            else => @compileError("unsupported usize alignment for static string literal allocation"),
        } catch return error.OutOfMemory;

        const refcount_ptr: *isize = @ptrCast(@alignCast(storage.ptr));
        refcount_ptr.* = builtins.utils.REFCOUNT_STATIC_DATA;

        const data_slice = storage[@sizeOf(usize)..];
        @memcpy(data_slice[0..bytes.len], bytes);

        const rs = RocStr{
            .bytes = @ptrCast(data_slice.ptr),
            .length = bytes.len,
            .capacity_or_alloc_ptr = bytes.len,
        };
        return self.rocStrToValue(rs, .str);
    }

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

    // Function calls — all go through the stack-safe engine via enterFunction/evalProcStackSafe.

    // Reference counting

    const RcOp = layout_mod.RcOp;

    /// Perform a reference count operation on a value using the layout-driven
    /// RC helper plan.  This walks structs, tag unions, boxes, etc. recursively
    /// so the interpreter's refcounting matches what the dev backend emits.
    fn performRawRc(self: *LirInterpreter, op: RcOp, val: Value, layout_idx: layout_mod.Idx, count: u16) void {
        trace.log("performRawRc: op={s} layout={any} val.ptr={*} count={d}", .{ @tagName(op), layout_idx, val.ptr, count });
        const resolver = layout_mod.RcHelperResolver.init(self.layout_store);
        const key = resolver.makeKey(op, layout_idx);
        self.performRawRcPlan(resolver.plan(key), &resolver, val, count);
    }

    fn performExplicitRcStmt(self: *LirInterpreter, op: RcOp, val: Value, layout_idx: layout_mod.Idx, count: u16) void {
        ownership_boundary.explicitLirRcExecution("interpreter.performExplicitRcStmt");
        self.performRawRc(op, val, layout_idx, count);
    }

    fn performBuiltinInternalRc(
        self: *LirInterpreter,
        comptime site: []const u8,
        op: RcOp,
        val: Value,
        layout_idx: layout_mod.Idx,
        count: u16,
    ) void {
        ownership_boundary.builtinRuntimeInternal(site);
        self.performRawRc(op, val, layout_idx, count);
    }

    fn performInterpreterApiRc(self: *LirInterpreter, op: RcOp, val: Value, layout_idx: layout_mod.Idx, count: u16) void {
        ownership_boundary.explicitLirRcExecution("interpreter.performInterpreterApiRc");
        self.performRawRc(op, val, layout_idx, count);
    }

    fn performForbiddenOrdinaryRc(
        self: *LirInterpreter,
        comptime site: []const u8,
        op: RcOp,
        val: Value,
        layout_idx: layout_mod.Idx,
        count: u16,
    ) void {
        ownership_boundary.forbiddenInterpreterOrdinaryRc(site);
        self.performRawRc(op, val, layout_idx, count);
    }

    fn builtinInternalContainsRefcounted(self: *LirInterpreter, comptime site: []const u8, layout_idx: layout_mod.Idx) bool {
        ownership_boundary.builtinRuntimeInternal(site);
        return self.helper.containsRefcounted(layout_idx);
    }

    fn forbiddenOrdinaryContainsRefcounted(self: *LirInterpreter, comptime site: []const u8, layout_idx: layout_mod.Idx) bool {
        ownership_boundary.forbiddenOrdinaryOwnershipDecision(site);
        return self.helper.containsRefcounted(layout_idx);
    }

    fn performRawRcPlan(self: *LirInterpreter, rc_plan: layout_mod.RcHelperPlan, resolver: *const layout_mod.RcHelperResolver, val: Value, count: u16) void {
        trace.log("performRawRcPlan: plan={s} val.ptr={*}", .{ @tagName(rc_plan), val.ptr });
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
                    @intFromPtr(rl.bytes),  rl.len(),  rl.capacity_or_alloc_ptr,
                    @intFromPtr(alloc_ptr), has_child, list_plan.elem_alignment,
                });
                // Before freeing the list, decref all child elements (mirrors RocList.decref logic)
                if (list_plan.child) |child_key| {
                    if (rl.isUnique(&self.roc_ops)) {
                        self.decrefListElements(rl, list_plan, child_key, resolver, count);
                    }
                }
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
                    @intFromPtr(rl.bytes),  rl.len(),  rl.capacity_or_alloc_ptr,
                    @intFromPtr(alloc_ptr), has_child,
                });
                // Before freeing the list, decref all child elements (mirrors RocList.decref logic)
                if (list_plan.child) |child_key| {
                    if (rl.isUnique(&self.roc_ops)) {
                        self.decrefListElements(rl, list_plan, child_key, resolver, count);
                    }
                }
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
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, &self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.decrefDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRawRcPlan(resolver.plan(child_key), resolver, child_val, count);
                    }
                }
                utils.decrefDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
            },
            .box_free => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, &self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRawRcPlan(resolver.plan(child_key), resolver, child_val, count);
                    }
                }
                utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
            },
            .struct_ => |struct_plan| {
                const field_count = resolver.structFieldCount(struct_plan);
                var i: u32 = 0;
                while (i < field_count) : (i += 1) {
                    const field_plan = resolver.structFieldPlan(struct_plan, i) orelse continue;
                    const field_val = Value{ .ptr = val.ptr + field_plan.offset };
                    self.performRawRcPlan(resolver.plan(field_plan.child), resolver, field_val, count);
                }
            },
            .tag_union => |tag_plan| {
                const variant_count = resolver.tagUnionVariantCount(tag_plan);
                if (variant_count == 0) return;

                const disc: u32 = blk: {
                    const tu_data = self.layout_store.getTagUnionData(tag_plan.tag_union_idx);
                    break :blk switch (tu_data.discriminant_size) {
                        0 => 0,
                        1 => val.offset(tu_data.discriminant_offset).read(u8),
                        2 => val.offset(tu_data.discriminant_offset).read(u16),
                        else => return,
                    };
                };
                trace_rc.log("tag_union rc: disc={d} variant_count={d}", .{ disc, variant_count });

                if (disc < variant_count) {
                    if (resolver.tagUnionVariantPlan(tag_plan, disc)) |child_key| {
                        // Payload is always at offset 0 in the tag union.
                        self.performRawRcPlan(resolver.plan(child_key), resolver, val, count);
                    }
                }
            },
            .closure => |child_key| {
                self.performRawRcPlan(resolver.plan(child_key), resolver, val, count);
            },
        }
    }

    /// Iterate through list elements and recursively decref each child.
    /// This mirrors the element cleanup logic in RocList.decref.
    fn decrefListElements(
        self: *LirInterpreter,
        rl: builtins.list.RocList,
        list_plan: layout_mod.RcListPlan,
        child_key: layout_mod.RcHelperKey,
        resolver: *const layout_mod.RcHelperResolver,
        count: u16,
    ) void {
        if (rl.getAllocationDataPtr(&self.roc_ops)) |source| {
            const elem_count = rl.getAllocationElementCount(true, &self.roc_ops);
            const child_plan = resolver.plan(child_key);
            var i: usize = 0;
            while (i < elem_count) : (i += 1) {
                const element_ptr = source + i * list_plan.elem_width;
                const element_val = Value{ .ptr = element_ptr };
                self.performRawRcPlan(child_plan, resolver, element_val, count);
            }
        }
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

    const ResolvedListBase = struct {
        value: Value,
        layout: layout_mod.Idx,
    };

    fn resolveListBaseValue(
        self: *LirInterpreter,
        list_val: Value,
        list_layout: layout_mod.Idx,
    ) ResolvedListBase {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@intFromEnum(list_layout)},
        );
        return .{
            .value = self.normalizeValueToLayout(list_val, list_layout, resolved_layout),
            .layout = resolved_layout,
        };
    }

    fn valueToRocListForLayout(
        self: *LirInterpreter,
        list_val: Value,
        list_layout: layout_mod.Idx,
    ) RocList {
        return valueToRocList(self.resolveListBaseValue(list_val, list_layout).value);
    }

    fn rocListToValue(self: *LirInterpreter, rl: RocList, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box => {
                const box_info = self.layout_store.getBoxInfo(ret_layout_val);
                const data_ptr = try self.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_refcounted,
                );
                @memcpy(data_ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));

                const boxed = try self.alloc(ret_layout);
                const target_usize = self.layout_store.targetUsize();
                if (target_usize.size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            .box_of_zst => return Value.zst,
            else => {
                const val = try self.alloc(ret_layout);
                @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));
                return val;
            },
        }
    }

    const ListElemInfo = struct { alignment: u32, width: usize };

    const ListElementPairStruct = struct {
        list_offset: usize,
        list_layout: layout_mod.Idx,
        elem_offset: usize,
        elem_layout: layout_mod.Idx,
    };

    const ListElementRcContext = struct {
        interp: *LirInterpreter,
        elem_layout: layout_mod.Idx,
    };

    fn listElemInfo(self: *LirInterpreter, list_layout: layout_mod.Idx) ListElemInfo {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@intFromEnum(list_layout)},
        );
        const l = self.layout_store.getLayout(resolved_layout);
        if (l.tag == .list) {
            const elem_idx = l.data.list;
            const sa = self.helper.sizeAlignOf(elem_idx);
            return .{
                .alignment = @intCast(sa.alignment.toByteUnits()),
                .width = sa.size,
            };
        }
        return .{ .alignment = 1, .width = 0 };
    }

    fn builtinListElemRc(self: *LirInterpreter, list_layout: layout_mod.Idx) bool {
        return self.builtinInternalContainsRefcounted("interpreter.builtinListElemRc", self.listElemLayout(list_layout));
    }

    fn listElemLayout(self: *LirInterpreter, list_layout: layout_mod.Idx) layout_mod.Idx {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@intFromEnum(list_layout)},
        );
        const l = self.layout_store.getLayout(resolved_layout);
        if (l.tag == .list) return l.data.list;
        return .zst;
    }

    fn listElementIncref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
        if (element == null) return;
        const ctx_ptr = context orelse unreachable;
        const ctx: *const ListElementRcContext = @ptrCast(@alignCast(ctx_ptr));
        ctx.interp.performBuiltinInternalRc("interpreter.listElementIncref", .incref, .{ .ptr = element.? }, ctx.elem_layout, 1);
    }

    fn listElementDecref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
        if (element == null) return;
        const ctx_ptr = context orelse unreachable;
        const ctx: *const ListElementRcContext = @ptrCast(@alignCast(ctx_ptr));
        ctx.interp.performBuiltinInternalRc("interpreter.listElementDecref", .decref, .{ .ptr = element.? }, ctx.elem_layout, 1);
    }

    // ── Builtin call with crash recovery ──

    fn callBuiltinStr1(self: *LirInterpreter, comptime func: anytype, a: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = func(a, &self.roc_ops);
        return self.rocStrToValue(result, ret_layout);
    }

    fn callBuiltinStr2(self: *LirInterpreter, comptime func: anytype, a: RocStr, b: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
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

    const LowLevelEvalInput = struct {
        op: LIR.LowLevel,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
        ret_layout: layout_mod.Idx,
        callable_proc: ?LirProcSpecId = null,
    };

    fn evalLowLevel(self: *LirInterpreter, ll: LowLevelEvalInput) Error!Value {
        const args = ll.args;

        // Determine argument layout for numeric ops (operand type, not return type)
        const arg_layout: layout_mod.Idx = if (ll.arg_layouts.len > 0)
            ll.arg_layouts[0]
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
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
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
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.strToUtf8C(valueToRocStr(args[0]), &self.roc_ops);
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .str_from_utf8 => blk: {
                // str_from_utf8(list) -> Result Str [BadUtf8 {index: U64, problem: Utf8Problem}]
                // The C builtin returns FromUtf8Try (a flat struct).
                // Convert to the Roc tag union layout using layout-resolved offsets,
                // following the same pattern as the dev backend (LirCodeGen.zig).
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8C(self.valueToRocListForLayout(args[0], arg_layout), UpdateMode.Immutable, &self.roc_ops);

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
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8Lossy(self.valueToRocListForLayout(args[0], arg_layout), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_split_on => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.strSplitOn(valueToRocStr(args[0]), valueToRocStr(args[1]), &self.roc_ops);
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .str_join_with => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.strJoinWithC(self.valueToRocListForLayout(args[0], arg_layout), valueToRocStr(args[1]), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_with_capacity => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.withCapacityC(args[0].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_reserve => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.reserveC(valueToRocStr(args[0]), args[1].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_release_excess_capacity => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.strReleaseExcessCapacity(&self.roc_ops, valueToRocStr(args[0]));
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_inspect => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var result: RocStr = undefined;
                const roc_str = valueToRocStr(args[0]);
                dev_wrappers.roc_builtins_str_escape_and_quote(
                    &result,
                    roc_str.bytes,
                    roc_str.length,
                    roc_str.capacity_or_alloc_ptr,
                    &self.roc_ops,
                );
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },

            // ── Numeric to_str ops ──
            .u8_to_str => self.numToStr(u8, args[0], ll.ret_layout),
            .i8_to_str => self.numToStr(i8, args[0], ll.ret_layout),
            .u16_to_str => self.numToStr(u16, args[0], ll.ret_layout),
            .i16_to_str => self.numToStr(i16, args[0], ll.ret_layout),
            .u32_to_str => self.numToStr(u32, args[0], ll.ret_layout),
            .i32_to_str => self.numToStr(i32, args[0], ll.ret_layout),
            .u64_to_str => self.numToStr(u64, args[0], ll.ret_layout),
            .i64_to_str => blk: {
                trace.log("i64_to_str: arg={d} ret_layout={any}", .{ args[0].read(i64), ll.ret_layout });
                break :blk self.numToStr(i64, args[0], ll.ret_layout);
            },
            .u128_to_str => self.numToStr(u128, args[0], ll.ret_layout),
            .i128_to_str => self.numToStr(i128, args[0], ll.ret_layout),
            .dec_to_str => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
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
                    var crash_boundary = self.enterCrashBoundary();
                    defer crash_boundary.deinit();
                    const sj = crash_boundary.set();
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
                const rl = self.valueToRocListForLayout(args[0], arg_layout);
                const val = try self.alloc(ll.ret_layout);
                val.write(u64, @intCast(rl.len()));
                break :blk val;
            },
            .list_get_unsafe => blk: {
                const rl = self.valueToRocListForLayout(args[0], arg_layout);
                const idx = args[1].read(u64);
                const info = self.listElemInfo(arg_layout);
                if (info.width == 0 or rl.bytes == null) break :blk try self.alloc(ll.ret_layout);
                const elem_ptr = rl.bytes.? + @as(usize, @intCast(idx)) * info.width;
                const val = try self.allocBytes(info.width);
                @memcpy(val.ptr[0..info.width], elem_ptr[0..info.width]);
                if (self.builtinInternalContainsRefcounted("interpreter.list_get_unsafe.ret_rc", ll.ret_layout)) {
                    self.performBuiltinInternalRc("interpreter.list_get_unsafe.ret_incref", .incref, val, ll.ret_layout, 1);
                }
                break :blk val;
            },
            .list_append_unsafe => blk: {
                const info = self.listElemInfo(arg_layout);
                const list_val = self.valueToRocListForLayout(args[0], arg_layout);
                const result = builtins.list.listAppendUnsafe(
                    list_val,
                    @ptrCast(args[1].ptr),
                    info.width,
                    &builtins.list.copy_fallback,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_concat => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                const list_a = self.valueToRocListForLayout(args[0], arg_layout);
                const list_b = self.valueToRocListForLayout(args[1], arg_layout);
                if (info.width == 0) {
                    const total_len = list_a.len() + list_b.len();
                    const result = RocList{
                        .bytes = null,
                        .length = total_len,
                        .capacity_or_alloc_ptr = total_len,
                    };
                    break :blk self.rocListToValue(result, ll.ret_layout);
                }
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) {
                    return error.Crash;
                }
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listConcat(
                    list_a,
                    list_b,
                    info.alignment,
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_prepend => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const copy_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) void = &(struct {
                    fn f(_: ?[*]u8, _: ?[*]u8) callconv(.c) void {}
                }).f;
                const result = builtins.list.listPrepend(
                    self.valueToRocListForLayout(args[0], arg_layout),
                    info.alignment,
                    @ptrCast(args[1].ptr),
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    copy_fn,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_sublist => blk: {
                if (args.len != 2 or ll.arg_layouts.len != 2) {
                    return self.runtimeError("list_sublist expected 2 arguments");
                }

                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                const record_layout = ll.arg_layouts[1];
                const record_layout_val = self.layout_store.getLayout(record_layout);
                if (record_layout_val.tag != .struct_) {
                    return self.runtimeError("list_sublist expected a { start, len } record");
                }

                const record_idx = record_layout_val.data.struct_.idx;
                const len_field_off = self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 0);
                const start_field_off = self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 1);
                const start = args[1].offset(start_field_off).read(u64);
                const len = args[1].offset(len_field_off).read(u64);
                const source_list = self.valueToRocListForLayout(args[0], arg_layout);

                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listSublist(
                    source_list,
                    info.alignment,
                    info.width,
                    elems_rc,
                    start,
                    len,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_drop_at => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listDropAt(
                    self.valueToRocListForLayout(args[0], arg_layout),
                    info.alignment,
                    info.width,
                    elems_rc,
                    args[1].read(u64),
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_set => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const copy_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) void = &(struct {
                    fn f(_: ?[*]u8, _: ?[*]u8) callconv(.c) void {}
                }).f;
                // listReplace writes old element into out_element
                const old_elem = try self.allocBytes(info.width);
                const result = builtins.list.listReplace(
                    self.valueToRocListForLayout(args[0], arg_layout),
                    info.alignment,
                    args[1].read(u64),
                    @ptrCast(args[2].ptr),
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    @ptrCast(old_elem.ptr),
                    copy_fn,
                    &self.roc_ops,
                );
                const val = try self.alloc(ll.ret_layout);
                const pair = self.resolveListElementPairStruct(ll.ret_layout);
                const result_value = try self.rocListToValue(result, pair.list_layout);
                try self.writeStructFieldValue(val, pair.list_offset, pair.list_layout, result_value, pair.list_layout);
                try self.writeStructFieldValue(val, pair.elem_offset, pair.elem_layout, old_elem, self.listElemLayout(arg_layout));
                break :blk val;
            },
            .list_with_capacity => blk: {
                const elem_layout = self.listElemLayout(ll.ret_layout);
                const sa = self.helper.sizeAlignOf(elem_layout);
                const elems_rc = self.builtinInternalContainsRefcounted("interpreter.list_with_capacity.elem_rc", elem_layout);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
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
                const elems_rc = self.builtinListElemRc(arg_layout);
                const list_val = self.valueToRocListForLayout(args[0], arg_layout);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listReserve(
                    list_val,
                    info.alignment,
                    args[1].read(u64),
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    UpdateMode.Immutable,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_release_excess_capacity => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listReleaseExcessCapacity(
                    self.valueToRocListForLayout(args[0], arg_layout),
                    info.alignment,
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
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
            .list_reverse => self.evalListReverse(args[0], arg_layout, ll.ret_layout),
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
            .u8_from_str,
            .i8_from_str,
            .u16_from_str,
            .i16_from_str,
            .u32_from_str,
            .i32_from_str,
            .u64_from_str,
            .i64_from_str,
            .u128_from_str,
            .i128_from_str,
            .dec_from_str,
            .f32_from_str,
            .f64_from_str,
            => blk: {
                const parse_spec = ll.op.numericParseSpec() orelse
                    return self.runtimeError("typed from_str low-level missing numeric parse spec");
                const ret_layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (ret_layout_val.tag != .tag_union) {
                    return self.runtimeError("typed from_str expected a tag union return layout");
                }

                const tu_data = self.layout_store.getTagUnionData(ret_layout_val.data.tag_union.idx);
                const result = try self.alloc(ll.ret_layout);
                const roc_str = valueToRocStr(args[0]);

                switch (parse_spec) {
                    .dec => dev_wrappers.roc_builtins_dec_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        tu_data.discriminant_offset,
                    ),
                    .float => |float| dev_wrappers.roc_builtins_float_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        float.width_bytes,
                        tu_data.discriminant_offset,
                    ),
                    .int => |int| dev_wrappers.roc_builtins_int_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        int.width_bytes,
                        int.signed,
                        tu_data.discriminant_offset,
                    ),
                }
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

    fn resolveListElementPairStruct(self: *LirInterpreter, struct_layout: layout_mod.Idx) ListElementPairStruct {
        const struct_layout_val = self.layout_store.getLayout(struct_layout);
        if (struct_layout_val.tag != .struct_) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: expected struct layout for list/element pair, got layout {d} ({s})",
                .{ @intFromEnum(struct_layout), @tagName(struct_layout_val.tag) },
            );
        }

        const struct_info = self.layout_store.getStructInfo(struct_layout_val);
        if (struct_info.fields.len != 2) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: expected 2-field struct layout {d} for list/element pair, found {d} fields",
                .{ @intFromEnum(struct_layout), struct_info.fields.len },
            );
        }

        var pair: ?ListElementPairStruct = null;
        var found_list = false;
        var found_elem = false;
        for (0..struct_info.fields.len) |i| {
            const field_layout = self.layout_store.getStructFieldLayout(struct_layout_val.data.struct_.idx, @intCast(i));
            const field_offset = self.layout_store.getStructFieldOffset(struct_layout_val.data.struct_.idx, @intCast(i));
            const field_layout_val = self.layout_store.getLayout(field_layout);
            const is_list = field_layout_val.tag == .list or field_layout_val.tag == .list_of_zst;
            if (is_list) {
                if (found_list) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: struct layout {d} had multiple list fields in list/element pair lowering",
                        .{@intFromEnum(struct_layout)},
                    );
                }
                found_list = true;
                pair = if (pair) |existing| .{
                    .list_offset = field_offset,
                    .list_layout = field_layout,
                    .elem_offset = existing.elem_offset,
                    .elem_layout = existing.elem_layout,
                } else .{
                    .list_offset = field_offset,
                    .list_layout = field_layout,
                    .elem_offset = 0,
                    .elem_layout = .zst,
                };
            } else {
                if (found_elem) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: struct layout {d} had multiple non-list fields in list/element pair lowering",
                        .{@intFromEnum(struct_layout)},
                    );
                }
                found_elem = true;
                pair = if (pair) |existing| .{
                    .list_offset = existing.list_offset,
                    .list_layout = existing.list_layout,
                    .elem_offset = field_offset,
                    .elem_layout = field_layout,
                } else .{
                    .list_offset = 0,
                    .list_layout = undefined,
                    .elem_offset = field_offset,
                    .elem_layout = field_layout,
                };
            }
        }

        const resolved = pair orelse self.invariantFailed(
            "LIR/interpreter invariant violated: struct layout {d} did not resolve a list/element pair shape",
            .{@intFromEnum(struct_layout)},
        );
        if (!found_list or !found_elem) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: struct layout {d} missing list or element field in list/element pair shape",
                .{@intFromEnum(struct_layout)},
            );
        }
        return resolved;
    }

    fn writeStructFieldValue(
        self: *LirInterpreter,
        struct_base: Value,
        field_offset: usize,
        expected_layout: layout_mod.Idx,
        actual_value: Value,
        actual_layout: layout_mod.Idx,
    ) Error!void {
        const field_size = self.helper.sizeOf(expected_layout);
        if (field_size == 0) return;
        const coerced = try self.coerceExplicitRefValueToLayout(actual_value, actual_layout, expected_layout);
        struct_base.offset(field_offset).copyFrom(coerced, field_size);
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

        trace.log("numBinOp: op={s} arg_layout={any} ret_layout={any} size={d}", .{
            @tagName(op),
            arg_layout,
            ret_layout,
            size,
        });

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
                    trace.log("numBinOp f64: a={d} b={d}", .{ a.read(f64), b.read(f64) });
                    val.write(f64, floatBinOp(f64, a.read(f64), b.read(f64), op));
                } else if (isUnsigned(arg_layout)) {
                    trace.log("numBinOp u64: a={d} b={d}", .{ a.read(u64), b.read(u64) });
                    val.write(u64, intBinOp(u64, a.read(u64), b.read(u64), op));
                } else {
                    trace.log("numBinOp i64: a={d} b={d}", .{ a.read(i64), b.read(i64) });
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
        const layout_val = self.layout_store.getLayout(arg_layout);

        if (op == .eq and switch (layout_val.tag) {
            .zst, .struct_, .list, .list_of_zst, .tag_union => true,
            .scalar => layout_val.data.scalar.tag == .str,
            else => false,
        }) {
            val.write(u8, if (try self.valuesEqual(a, b, arg_layout)) 1 else 0);
            return val;
        }

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
                break :blk if (layout_val.tag == .scalar and layout_val.data.scalar.tag == .frac)
                    cmpOp(f32, a.read(f32), b.read(f32), op)
                else if (isUnsigned(arg_layout))
                    cmpOp(u32, a.read(u32), b.read(u32), op)
                else
                    cmpOp(i32, a.read(i32), b.read(i32), op);
            },
            8 => blk: {
                break :blk if (layout_val.tag == .scalar and layout_val.data.scalar.tag == .frac)
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
            else => return self.invariantFailedError(
                "LIR/interpreter invariant violated: non-equality compare on unsupported layout {d} size={d}",
                .{ @intFromEnum(arg_layout), size },
            ),
        };
        val.write(u8, if (result) 1 else 0);
        return val;
    }

    fn valuesEqual(self: *LirInterpreter, a: Value, b: Value, layout_idx: layout_mod.Idx) Error!bool {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .zst => true,
            .scalar => switch (layout_val.data.scalar.tag) {
                .str => builtins.str.strEqual(valueToRocStr(a), valueToRocStr(b)),
                .frac => switch (self.helper.sizeOf(layout_idx)) {
                    4 => a.read(f32) == b.read(f32),
                    8 => a.read(f64) == b.read(f64),
                    16 => a.read(i128) == b.read(i128),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: fractional layout {d} has unsupported size {d}",
                        .{ @intFromEnum(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .int => switch (self.helper.sizeOf(layout_idx)) {
                    1 => if (isUnsigned(layout_idx)) a.read(u8) == b.read(u8) else a.read(i8) == b.read(i8),
                    2 => if (isUnsigned(layout_idx)) a.read(u16) == b.read(u16) else a.read(i16) == b.read(i16),
                    4 => if (isUnsigned(layout_idx)) a.read(u32) == b.read(u32) else a.read(i32) == b.read(i32),
                    8 => if (isUnsigned(layout_idx)) a.read(u64) == b.read(u64) else a.read(i64) == b.read(i64),
                    16 => if (isUnsigned(layout_idx)) a.read(u128) == b.read(u128) else a.read(i128) == b.read(i128),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: scalar layout {d} has unsupported size {d}",
                        .{ @intFromEnum(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .opaque_ptr => switch (self.helper.sizeOf(layout_idx)) {
                    4 => a.read(u32) == b.read(u32),
                    8 => a.read(usize) == b.read(usize),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: opaque pointer layout {d} has unsupported size {d}",
                        .{ @intFromEnum(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
            },
            .box_of_zst => true,
            .box => blk: {
                const a_ptr = self.readBoxedDataPointer(a);
                const b_ptr = self.readBoxedDataPointer(b);
                if (a_ptr == null or b_ptr == null) break :blk a_ptr == null and b_ptr == null;
                break :blk try self.valuesEqual(.{ .ptr = a_ptr.? }, .{ .ptr = b_ptr.? }, layout_val.data.box);
            },
            .struct_ => blk: {
                const struct_data = self.layout_store.getStructData(layout_val.data.struct_.idx);
                const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                var field_index: usize = 0;
                while (field_index < fields.len) : (field_index += 1) {
                    const field = fields.get(@intCast(field_index));
                    const field_layout = field.layout;
                    const field_size = self.helper.sizeOf(field_layout);
                    if (field_size == 0) continue;
                    const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                        layout_val.data.struct_.idx,
                        field.index,
                    );
                    if (!try self.valuesEqual(a.offset(field_offset), b.offset(field_offset), field_layout)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => blk: {
                const a_base = self.resolveTagUnionBaseValue(a, layout_idx);
                const b_base = self.resolveTagUnionBaseValue(b, layout_idx);
                const a_disc = self.helper.readTagDiscriminant(a_base.value, a_base.layout);
                const b_disc = self.helper.readTagDiscriminant(b_base.value, b_base.layout);
                if (a_disc != b_disc) break :blk false;
                const payload_layout = self.tagPayloadLayout(a_base.layout, a_disc);
                if (self.helper.sizeOf(payload_layout) == 0) break :blk true;
                break :blk try self.valuesEqual(a_base.value, b_base.value, payload_layout);
            },
            .list_of_zst => self.valueToRocListForLayout(a, layout_idx).len() == self.valueToRocListForLayout(b, layout_idx).len(),
            .list => blk: {
                const a_list = self.valueToRocListForLayout(a, layout_idx);
                const b_list = self.valueToRocListForLayout(b, layout_idx);
                if (a_list.len() != b_list.len()) break :blk false;
                const elem_layout = self.listElemLayout(layout_idx);
                const elem_size = self.helper.sizeOf(elem_layout);
                if (elem_size == 0) break :blk true;
                const a_bytes = a_list.bytes orelse break :blk b_list.bytes == null;
                const b_bytes = b_list.bytes orelse break :blk false;
                var i: usize = 0;
                while (i < a_list.len()) : (i += 1) {
                    const offset = i * elem_size;
                    if (!try self.valuesEqual(.{ .ptr = a_bytes + offset }, .{ .ptr = b_bytes + offset }, elem_layout)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .closure => return self.invariantFailedError(
                "LIR/interpreter invariant violated: function equality survived lowering",
                .{},
            ),
        };
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
            var crash_boundary = self.enterCrashBoundary();
            defer crash_boundary.deinit();
            const sj = crash_boundary.set();
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
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
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
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
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
        const elems_rc = self.builtinListElemRc(list_layout);
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            self.valueToRocListForLayout(list_arg, list_layout),
            info.alignment,
            info.width,
            elems_rc,
            1,
            std.math.maxInt(u64),
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListDropLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const len = rl.len();
        if (len == 0) return self.rocListToValue(rl, ret_layout);
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            elems_rc,
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
        const elems_rc = self.builtinListElemRc(list_layout);
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            self.valueToRocListForLayout(list_arg, list_layout),
            info.alignment,
            info.width,
            elems_rc,
            0,
            count_arg.read(u64),
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeLast(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const len = rl.len();
        const take = count_arg.read(u64);
        const start = if (take >= len) 0 else len - @as(usize, @intCast(take));
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            elems_rc,
            @intCast(start),
            take,
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListReverse(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        if (rl.len() <= 1 or rl.bytes == null or info.width == 0)
            return self.rocListToValue(rl, ret_layout);
        // Clone and reverse in-place
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const new_list = builtins.list.shallowClone(rl, rl.len(), info.width, info.alignment, elems_rc, &self.roc_ops);
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

    fn evalListSplitFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const elem_layout = self.listElemLayout(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            const payload_layout = self.tagPayloadLayout(ret_layout, 1);
            const pair = self.resolveListElementPairStruct(payload_layout);
            const first_elem = Value{ .ptr = rl.bytes.? };
            try self.writeStructFieldValue(
                val,
                pair.elem_offset,
                pair.elem_layout,
                first_elem,
                elem_layout,
            );
            if (self.builtinInternalContainsRefcounted("interpreter.list_split_first.elem_rc", elem_layout)) {
                self.performBuiltinInternalRc("interpreter.list_split_first.elem_incref", .incref, first_elem, elem_layout, 1);
            }
            // Rest list starts at offset info.width
            var crash_boundary = self.enterCrashBoundary();
            defer crash_boundary.deinit();
            const sj = crash_boundary.set();
            if (sj != 0) return error.Crash;
            const rest = builtins.list.listSublist(
                rl,
                info.alignment,
                info.width,
                elems_rc,
                1,
                std.math.maxInt(u64),
                null,
                &builtins.utils.rcNone,
                &self.roc_ops,
            );
            const rest_value = try self.rocListToValue(rest, pair.list_layout);
            try self.writeStructFieldValue(val, pair.list_offset, pair.list_layout, rest_value, pair.list_layout);
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    fn evalListSplitLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const elem_layout = self.listElemLayout(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            const payload_layout = self.tagPayloadLayout(ret_layout, 1);
            const pair = self.resolveListElementPairStruct(payload_layout);
            const last_offset = (rl.len() - 1) * info.width;
            const last_elem = Value{ .ptr = rl.bytes.? + last_offset };
            try self.writeStructFieldValue(
                val,
                pair.elem_offset,
                pair.elem_layout,
                last_elem,
                elem_layout,
            );
            if (self.builtinInternalContainsRefcounted("interpreter.list_split_last.elem_rc", elem_layout)) {
                self.performBuiltinInternalRc("interpreter.list_split_last.elem_incref", .incref, last_elem, elem_layout, 1);
            }
            var crash_boundary = self.enterCrashBoundary();
            defer crash_boundary.deinit();
            const sj = crash_boundary.set();
            if (sj != 0) return error.Crash;
            const rest = builtins.list.listSublist(
                rl,
                info.alignment,
                info.width,
                elems_rc,
                0,
                rl.len() - 1,
                null,
                &builtins.utils.rcNone,
                &self.roc_ops,
            );
            const rest_value = try self.rocListToValue(rest, pair.list_layout);
            try self.writeStructFieldValue(val, pair.list_offset, pair.list_layout, rest_value, pair.list_layout);
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
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) break :blk @as(i128, 0);
                break :blk builtins.dec.divC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
            },
            .div_trunc => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) break :blk @as(i128, 0);
                break :blk builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
            },
            .rem => blk: {
                // Dec rem: a - trunc(a/b) * b
                if (bv == 0) break :blk @as(i128, 0);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) break :blk @as(i128, 0);
                const div_result = builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
                const mul_result = RocDec.mulWithOverflow(RocDec{ .num = div_result }, RocDec{ .num = bv });
                break :blk av -% mul_result.value.num;
            },
            .mod => blk: {
                if (bv == 0) break :blk @as(i128, 0);
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
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

    // Layout helpers

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

    const ResolvedStructBase = struct {
        value: Value,
        layout: layout_mod.Idx,
    };

    fn resolveStructBaseValue(
        self: *LirInterpreter,
        struct_val: Value,
        struct_layout: layout_mod.Idx,
    ) ResolvedStructBase {
        const struct_layout_val = self.layout_store.getLayout(struct_layout);
        switch (struct_layout_val.tag) {
            .box => {
                const inner_layout = struct_layout_val.data.box;
                const inner_layout_val = self.layout_store.getLayout(inner_layout);
                if (inner_layout_val.tag != .struct_) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: field projection source layout {d} boxes non-struct layout {d}",
                        .{ @intFromEnum(struct_layout), @intFromEnum(inner_layout) },
                    );
                }
                const data_ptr = self.readBoxedDataPointer(struct_val) orelse self.invariantFailed(
                    "LIR/interpreter invariant violated: boxed struct layout {d} had null data pointer for inner layout {d}",
                    .{ @intFromEnum(struct_layout), @intFromEnum(inner_layout) },
                );
                return .{
                    .value = .{ .ptr = data_ptr },
                    .layout = inner_layout,
                };
            },
            .struct_ => return .{
                .value = struct_val,
                .layout = struct_layout,
            },
            else => self.invariantFailed(
                "LIR/interpreter invariant violated: field projection source layout {d} is not a struct or boxed struct",
                .{@intFromEnum(struct_layout)},
            ),
        }
    }

    fn resolveTagUnionBaseValue(
        self: *LirInterpreter,
        union_val: Value,
        union_layout: layout_mod.Idx,
    ) ResolvedTagUnionBase {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const inner_layout = union_layout_val.data.box;
            const data_ptr = self.readBoxedDataPointer(union_val) orelse self.invariantFailed(
                "LIR/interpreter invariant violated: boxed tag union layout {d} had null data pointer for inner layout {d}",
                .{ @intFromEnum(union_layout), @intFromEnum(inner_layout) },
            );
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
                    const data_ptr = self.readBoxedDataPointer(value) orelse self.invariantFailed(
                        "LIR/interpreter invariant violated: expected boxed layout {d} to contain data for inner layout {d}, but observed null box pointer",
                        .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                    );
                    return .{ .ptr = data_ptr };
                }
            },
            .box_of_zst => if (expected_layout == .zst) return Value.zst,
            else => {},
        }

        return value;
    }

    // Temporary forbidden helper island.
    //
    // These aggregate-rebuilding coercions still exist only because earlier LIR
    // does not yet fully encode fresh aggregate ownership for all pack/rebuild
    // paths. Ordinary execution must not reach them; explicit bridge helpers
    // and the ownership-boundary lint keep them isolated until they are
    // replaced by fact-driven LIR ownership.
    fn coerceForbiddenAggregateBoundaryValueToLayout(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        ownership_boundary.forbiddenOrdinaryOwnershipDecision("interpreter.coerceForbiddenAggregateBoundaryValueToLayout");
        if (actual_layout == expected_layout) {
            return value;
        }

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        switch (expected_layout_val.tag) {
            .box => {
                return try self.coerceForbiddenAggregateValueIntoBox(value, actual_layout, expected_layout);
            },
            .box_of_zst => {
                if (self.helper.sizeOf(actual_layout) == 0) return Value.zst;
            },
            .struct_ => {
                if (actual_layout_val.tag == .struct_ or actual_layout_val.tag == .box) {
                    return try self.coerceForbiddenAggregateStructValue(value, actual_layout, expected_layout);
                }
            },
            .tag_union => {
                if (actual_layout_val.tag == .tag_union or actual_layout_val.tag == .box) {
                    return try self.coerceForbiddenAggregateTagUnionValue(value, actual_layout, expected_layout);
                }
            },
            else => {},
        }

        return self.normalizeValueToLayout(value, actual_layout, expected_layout);
    }

    fn coerceExplicitListValueToLayout(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (builtin.mode == .Debug) {
            const actual_layout_val = self.layout_store.getLayout(actual_layout);
            const expected_layout_val = self.layout_store.getLayout(expected_layout);
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            if (!actual_is_list or !expected_is_list) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit list bridge expected list layouts, got actual={d} expected={d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                );
            }
        }

        return value;
    }

    fn coerceExplicitNominalValueToLayout(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (builtin.mode == .Debug) {
            const actual_layout_val = self.layout_store.getLayout(actual_layout);
            const expected_layout_val = self.layout_store.getLayout(expected_layout);
            const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst;
            const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            if (actual_is_box != expected_is_box or actual_is_list or expected_is_list) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit nominal bridge expected non-list layouts on the same side of physical boxing, got actual={d} ({s}) expected={d} ({s})",
                    .{
                        @intFromEnum(actual_layout),
                        @tagName(actual_layout_val.tag),
                        @intFromEnum(expected_layout),
                        @tagName(expected_layout_val.tag),
                    },
                );
            }
        }
        return value;
    }

    fn coerceExplicitRefValueToLayout(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (actual_layout == expected_layout) return value;

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
        const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
        if (actual_is_list or expected_is_list) {
            return try self.coerceExplicitListValueToLayout(value, actual_layout, expected_layout);
        }

        const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst;
        const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
        if (actual_is_box or expected_is_box) {
            return try self.coerceExplicitNominalValueToLayout(value, actual_layout, expected_layout);
        }

        if (builtin.mode == .Debug and
            (actual_layout_val.tag == .struct_ or expected_layout_val.tag == .struct_ or
                actual_layout_val.tag == .tag_union or expected_layout_val.tag == .tag_union))
        {
            self.invariantFailed(
                "LIR/interpreter invariant violated: explicit ref bridge reached aggregate coercion path actual={d} ({s}) expected={d} ({s})",
                .{
                    @intFromEnum(actual_layout),
                    @tagName(actual_layout_val.tag),
                    @intFromEnum(expected_layout),
                    @tagName(expected_layout_val.tag),
                },
            );
        }

        return self.normalizeValueToLayout(value, actual_layout, expected_layout);
    }

    fn coerceForbiddenAggregateValueIntoBox(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_box_layout: layout_mod.Idx,
    ) Error!Value {
        ownership_boundary.forbiddenOrdinaryOwnershipDecision("interpreter.coerceForbiddenAggregateValueIntoBox");
        const expected_layout_val = self.layout_store.getLayout(expected_box_layout);
        switch (expected_layout_val.tag) {
            .box_of_zst => return Value.zst,
            .box => {
                const inner_layout = expected_layout_val.data.box;
                const inner_value = try self.coerceForbiddenAggregateBoundaryValueToLayout(value, actual_layout, inner_layout);
                const box_info = self.layout_store.getBoxInfo(expected_layout_val);
                const data_ptr = try self.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_refcounted,
                );
                if (box_info.elem_size > 0) {
                    (Value{ .ptr = data_ptr }).copyFrom(inner_value, box_info.elem_size);
                    if (self.forbiddenOrdinaryContainsRefcounted("interpreter.coerceForbiddenAggregateValueIntoBox.inner_rc", inner_layout)) {
                        self.performForbiddenOrdinaryRc("interpreter.coerceForbiddenAggregateValueIntoBox.inner_incref", .incref, .{ .ptr = data_ptr }, inner_layout, 1);
                    }
                    if (builtin.mode == .Debug and @intFromEnum(expected_box_layout) == 29) {
                        std.debug.print(
                            "coerceValueIntoBox actual_layout={d} expected_box={d} inner_layout={d} data_ptr=0x{x} bytes:",
                            .{
                                @intFromEnum(actual_layout),
                                @intFromEnum(expected_box_layout),
                                @intFromEnum(inner_layout),
                                @intFromPtr(data_ptr),
                            },
                        );
                        for ((Value{ .ptr = data_ptr }).readBytes(@min(box_info.elem_size, @as(u32, 40)))) |byte| {
                            std.debug.print(" {x:0>2}", .{byte});
                        }
                        std.debug.print("\n", .{});
                    }
                }
                const boxed = try self.alloc(expected_box_layout);
                if (self.layout_store.targetUsize().size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            else => unreachable,
        }
    }

    fn coerceForbiddenAggregateStructValue(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        ownership_boundary.forbiddenOrdinaryOwnershipDecision("interpreter.coerceForbiddenAggregateStructValue");
        const actual_base = self.resolveStructBaseValue(value, actual_layout);
        const expected_alloc = try self.allocStructValue(expected_layout);

        const actual_layout_val = self.layout_store.getLayout(actual_base.layout);
        const expected_layout_val = self.layout_store.getLayout(expected_alloc.base_layout);
        if (actual_layout_val.tag != .struct_ or expected_layout_val.tag != .struct_) {
            return self.normalizeValueToLayout(value, actual_layout, expected_layout);
        }

        const actual_info = self.layout_store.getStructInfo(actual_layout_val);
        const expected_info = self.layout_store.getStructInfo(expected_layout_val);
        if (actual_info.fields.len != expected_info.fields.len) {
            return self.normalizeValueToLayout(value, actual_layout, expected_layout);
        }

        if (builtin.mode == .Debug and
            ((@intFromEnum(actual_layout) == 19 and @intFromEnum(expected_layout) == 26) or
                (@intFromEnum(actual_layout) == 20 and @intFromEnum(expected_layout) == 30)))
        {
            std.debug.print(
                "coerceStructValue actual_layout={d} expected_layout={d} base_ptr=0x{x}\n",
                .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout), @intFromPtr(actual_base.value.ptr) },
            );
        }

        for (0..expected_info.fields.len) |i| {
            const expected_field = expected_info.fields.get(@intCast(i));
            const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_layout_val.data.struct_.idx, expected_field.index);
            const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_layout_val.data.struct_.idx, expected_field.index);
            const expected_field_size = self.helper.sizeOf(expected_field_layout);
            if (expected_field_size == 0) continue;

            const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_layout_val.data.struct_.idx, expected_field.index);
            const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_layout_val.data.struct_.idx, expected_field.index);
            const actual_field_value = actual_base.value.offset(actual_field_offset);
            const coerced_field = try self.coerceForbiddenAggregateBoundaryValueToLayout(actual_field_value, actual_field_layout, expected_field_layout);
            if (builtin.mode == .Debug and
                ((@intFromEnum(actual_layout) == 19 and @intFromEnum(expected_layout) == 26) or
                    (@intFromEnum(actual_layout) == 20 and @intFromEnum(expected_layout) == 30)))
            {
                std.debug.print(
                    "  field[{d}] actual_layout={d} expected_layout={d} actual_off={d} expected_off={d} size={d}\n",
                    .{
                        i,
                        @intFromEnum(actual_field_layout),
                        @intFromEnum(expected_field_layout),
                        actual_field_offset,
                        expected_field_offset,
                        expected_field_size,
                    },
                );
                std.debug.print("    actual bytes:", .{});
                for (actual_field_value.readBytes(@min(self.helper.sizeOf(actual_field_layout), @as(u32, 32)))) |byte| {
                    std.debug.print(" {x:0>2}", .{byte});
                }
                std.debug.print("\n", .{});
                std.debug.print("    coerced bytes:", .{});
                for (coerced_field.readBytes(@min(expected_field_size, @as(u32, 32)))) |byte| {
                    std.debug.print(" {x:0>2}", .{byte});
                }
                std.debug.print("\n", .{});
            }
            expected_alloc.base.offset(expected_field_offset).copyFrom(coerced_field, expected_field_size);
        }

        if (self.forbiddenOrdinaryContainsRefcounted("interpreter.coerceForbiddenAggregateStructValue.result_rc", expected_alloc.base_layout)) {
            self.performForbiddenOrdinaryRc("interpreter.coerceForbiddenAggregateStructValue.result_incref", .incref, expected_alloc.base, expected_alloc.base_layout, 1);
        }

        return expected_alloc.outer;
    }

    fn coerceForbiddenAggregateTagUnionValue(
        self: *LirInterpreter,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        ownership_boundary.forbiddenOrdinaryOwnershipDecision("interpreter.coerceForbiddenAggregateTagUnionValue");
        const actual_base = self.resolveTagUnionBaseValue(value, actual_layout);
        const actual_base_layout_val = self.layout_store.getLayout(actual_base.layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const expected_base_layout = switch (expected_layout_val.tag) {
            .box => expected_layout_val.data.box,
            else => expected_layout,
        };
        const expected_base_layout_val = self.layout_store.getLayout(expected_base_layout);
        if (actual_base_layout_val.tag != .tag_union or expected_base_layout_val.tag != .tag_union) {
            return self.normalizeValueToLayout(value, actual_layout, expected_layout);
        }

        const discriminant = self.helper.readTagDiscriminant(actual_base.value, actual_base.layout);
        const allocated = try self.allocTagValue(expected_layout);
        self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, discriminant);

        const actual_payload_layout = self.tagPayloadLayout(actual_base.layout, discriminant);
        const expected_payload_layout = self.tagPayloadLayout(allocated.base_layout, discriminant);
        const payload_size = self.helper.sizeOf(expected_payload_layout);
        if (payload_size > 0) {
            const coerced_payload = try self.coerceForbiddenAggregateBoundaryValueToLayout(actual_base.value, actual_payload_layout, expected_payload_layout);
            allocated.base.copyFrom(coerced_payload, payload_size);
        }

        if (builtin.mode == .Debug and @intFromEnum(actual_base.layout) == 21 and @intFromEnum(expected_base_layout) == 28) {
            std.debug.print(
                "coerceTagUnionValue actual_layout={d} expected_layout={d} disc={d} payload_layout={d} payload_size={d} base_ptr=0x{x} bytes:",
                .{
                    @intFromEnum(actual_base.layout),
                    @intFromEnum(expected_base_layout),
                    discriminant,
                    @intFromEnum(expected_payload_layout),
                    payload_size,
                    @intFromPtr(allocated.base.ptr),
                },
            );
            for (allocated.base.readBytes(@min(self.helper.sizeOf(expected_base_layout), @as(u32, 40)))) |byte| {
                std.debug.print(" {x:0>2}", .{byte});
            }
            std.debug.print("\n", .{});
        }

        if (self.forbiddenOrdinaryContainsRefcounted("interpreter.coerceForbiddenAggregateTagUnionValue.result_rc", allocated.base_layout)) {
            self.performForbiddenOrdinaryRc("interpreter.coerceForbiddenAggregateTagUnionValue.result_incref", .incref, allocated.base, allocated.base_layout, 1);
        }

        return allocated.outer;
    }

    fn getLayout(self: *LirInterpreter, idx: layout_mod.Idx) Layout {
        return self.layout_store.getLayout(idx);
    }

    fn evalBoxBox(self: *LirInterpreter, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box_of_zst => return Value.zst,
            .box => {
                const box_info = self.layout_store.getBoxInfo(ret_layout_val);
                const elem_size = box_info.elem_size;
                const elem_align = box_info.elem_alignment;
                const data_ptr = try self.allocRocDataWithRc(elem_size, elem_align, box_info.contains_refcounted);
                if (elem_size > 0) {
                    @memcpy(data_ptr[0..elem_size], arg.ptr[0..elem_size]);
                }
                if (self.builtinInternalContainsRefcounted("interpreter.evalBoxBox.arg_rc", box_info.elem_layout_idx)) {
                    self.performBuiltinInternalRc("interpreter.evalBoxBox.arg_incref", .incref, arg, box_info.elem_layout_idx, 1);
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
        if (self.builtinInternalContainsRefcounted("interpreter.evalBoxUnbox.result_rc", ret_layout)) {
            self.performBuiltinInternalRc("interpreter.evalBoxUnbox.result_incref", .incref, result, ret_layout, 1);
        }

        return result;
    }

    // ═══════════════════════════════════════════════════════════════════
};
