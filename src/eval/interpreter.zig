//! Statement-only LIR interpreter.
//!
//! Evaluates proc-root, post-RC LIR directly, producing concrete runtime values.
//! All evaluation follows explicit `CFStmt` control flow and explicit RC ops.
//!
//! RC boundary:
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
const lir_value = @import("value.zig");
const host_trampoline = @import("host_trampoline.zig");
const builtins = @import("builtins");
const sljmp = @import("sljmp");
const build_options = @import("build_options");
const is_freestanding = builtin.target.os.tag == .freestanding;

/// Comptime-gated tracing for the interpreter eval loop.
/// Enabled via `-Dtrace-eval=true`. Zero cost when disabled.
const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            debugPrint("[interp] " ++ fmt ++ "\n", args);
        }
    }
};

/// Comptime-gated tracing for refcount operations.
/// Enabled via `-Dtrace-refcount=true`. Zero cost when disabled.
const trace_rc = struct {
    const enabled = if (@hasDecl(build_options, "trace_refcount")) build_options.trace_refcount else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            debugPrint("[rc] " ++ fmt ++ "\n", args);
        }
    }
};

const debugPrint = if (is_freestanding)
    struct {
        fn print(comptime _: []const u8, _: anytype) void {}
    }.print
else
    struct {
        fn print(comptime fmt: []const u8, args: anytype) void {
            std.debug.print(fmt, args);
        }
    }.print;

const Allocator = std.mem.Allocator;
const LirProcSpecId = LIR.LirProcSpecId;
const LirProcSpec = LIR.LirProcSpec;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LocalSpan = LIR.LocalSpan;
const Layout = layout_mod.Layout;
const Value = lir_value.Value;
const LayoutHelper = lir_value.LayoutHelper;
const RocDec = builtins.dec.RocDec;
const dev_wrappers = builtins.dev_wrappers;

// Builtin types for direct dispatch
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;
const UpdateMode = builtins.utils.UpdateMode;
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

/// Failed inline `expect` observed during one interpreter evaluation.
pub const ExpectFailure = struct {
    message: []const u8,
    loc: base.SourceLoc,
};

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
    expect_failures: std.ArrayList(ExpectFailure) = .empty,
    expect_err_message: ?[]const u8 = null,
    expect_err_region: ?base.Region = null,
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
        self.clearExpectFailures();
        self.expect_failures.deinit(self.allocator);
        if (self.expect_err_message) |msg| self.allocator.free(msg);
    }

    /// Reset the static buffer — call once at the start of a full evaluation.
    fn resetForEval(self: *InterpreterRocEnv) void {
        self.crashed = false;
        if (self.crash_message) |msg| self.allocator.free(msg);
        self.crash_message = null;
        self.runtime_error_message = null;
        if (self.expect_message) |msg| self.allocator.free(msg);
        self.expect_message = null;
        self.clearExpectFailures();
        if (self.expect_err_message) |msg| self.allocator.free(msg);
        self.expect_err_message = null;
        self.expect_err_region = null;
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

    fn recordCrash(self: *InterpreterRocEnv, msg: []const u8) void {
        self.crashed = true;
        if (self.crash_message) |old| self.allocator.free(old);
        self.crash_message = self.allocator.dupe(u8, msg) catch null;
    }

    fn clearExpectFailures(self: *InterpreterRocEnv) void {
        for (self.expect_failures.items) |failure| {
            self.allocator.free(failure.message);
        }
        self.expect_failures.clearRetainingCapacity();
    }

    fn recordExpectFailure(self: *InterpreterRocEnv, msg: []const u8, loc: base.SourceLoc) Allocator.Error!void {
        const owned_msg = try self.allocator.dupe(u8, msg);
        errdefer self.allocator.free(owned_msg);
        try self.expect_failures.append(self.allocator, .{
            .message = owned_msg,
            .loc = loc,
        });
    }

    fn reportCrash(self: *InterpreterRocEnv, msg: []const u8) void {
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_crashed(caller_roc_ops, msg.ptr, msg.len);
        self.recordCrash(msg);
    }

    /// The host allocators signal OOM by returning a null pointer (see
    /// `host_abi.RocOps.roc_alloc`). Turn that into a Roc crash that unwinds to
    /// the eval boundary via the active jump buffer, instead of letting it abort.
    fn crashAllocationFailed(self: *InterpreterRocEnv) noreturn {
        self.reportCrash("ran out of memory");
        const active_jmp_buf = self.active_jmp_buf orelse {
            debugPrint(
                "LIR/interpreter invariant violated: allocation failed without an active jump buffer\n",
                .{},
            );
            if (is_freestanding) {
                @trap();
            } else {
                std.process.abort();
            }
        };
        self.active_jmp_buf = null;
        longjmp(active_jmp_buf, 1);
    }

    fn rocAllocFn(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(ops.env));
        const caller_roc_ops = self.currentRocOps();
        const ptr = caller_roc_ops.roc_alloc(caller_roc_ops, length, alignment) orelse self.crashAllocationFailed();
        trace_rc.log("alloc(fwd): ptr=0x{x} size={d} align={d}", .{ @intFromPtr(ptr), length, alignment });
        return ptr;
    }

    fn rocDeallocFn(ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(ops.env));
        trace_rc.log("dealloc: ptr=0x{x} align={d}", .{ @intFromPtr(ptr), alignment });
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_dealloc(caller_roc_ops, ptr, alignment);
    }

    fn rocReallocFn(ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(ops.env));
        const caller_roc_ops = self.currentRocOps();
        const old_ptr = ptr;
        const new_ptr = caller_roc_ops.roc_realloc(caller_roc_ops, ptr, new_length, alignment) orelse self.crashAllocationFailed();
        trace_rc.log("realloc(fwd): old=0x{x} new=0x{x} size={d}", .{ @intFromPtr(old_ptr), @intFromPtr(new_ptr), new_length });
        return new_ptr;
    }

    fn rocDbgFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(ops.env));
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_dbg(caller_roc_ops, bytes, len);
    }

    fn rocExpectFailedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(ops.env));
        const caller_roc_ops = self.currentRocOps();
        caller_roc_ops.roc_expect_failed(caller_roc_ops, bytes, len);
        const source = bytes[0..len];
        if (self.expect_message == null) {
            self.expect_message = self.allocator.dupe(u8, source) catch null;
        }
    }

    fn rocCrashedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(ops.env));
        const msg = bytes[0..len];
        self.reportCrash(msg);
        const active_jmp_buf = self.active_jmp_buf orelse {
            debugPrint(
                "LIR/interpreter invariant violated: roc_crashed fired without an active jump buffer\n",
                .{},
            );
            if (is_freestanding) {
                @trap();
            } else {
                std.process.abort();
            }
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
    /// Debug value-shape validation stops descending past this many nested
    /// values; deeper structures are legal (TRMC builds arbitrarily long
    /// lists) but walking them would overflow the native stack.
    const max_debug_value_depth: usize = 64;
    /// ... and stops after visiting this many heap cells in one walk: a wide
    /// balanced tree fits entirely inside the depth cap, and re-walking it on
    /// every assignment turns O(n) programs quadratic.
    const max_debug_value_visits: usize = 16;
    pub const erased_callable_context_alignment: usize = builtins.erased_callable.capture_alignment;

    pub const ErasedCallableInterpreterContext = extern struct {
        interpreter: *LirInterpreter,
        proc_id: u32,
        capture_layout_plus_one: u32,
        capture_value_offset: u32,
        padding: u32,
    };

    pub const erased_callable_context_capture_offset: usize =
        std.mem.alignForward(usize, @sizeOf(ErasedCallableInterpreterContext), erased_callable_context_alignment);

    allocator: Allocator,
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    helper: LayoutHelper,
    /// Arena for interpreter-allocated memory (temporaries, copies).
    arena: base.SingleThreadArena,
    /// RocOps environment for builtin dispatch.
    roc_env: *InterpreterRocEnv,
    roc_ops: RocOps,
    frame_plans: []FramePlan,
    rc_presence: []RcPresence,
    rc_plans: std.AutoHashMapUnmanaged(u64, layout_mod.RcHelperPlan) = .{},
    struct_field_plans: std.AutoHashMapUnmanaged(u64, ?layout_mod.RcFieldPlan) = .{},
    tag_variant_plans: std.AutoHashMapUnmanaged(u64, ?layout_mod.RcHelperKey) = .{},
    /// Bound recursive function-call depth so the interpreter reports a Roc crash
    /// instead of overflowing the native stack.
    call_depth: usize = 0,
    /// Active proc call stack for the current evaluation.
    call_stack: std.ArrayList(LirProcSpecId),
    /// Call stack captured at the first failed exit in the current evaluation.
    failed_call_stack: std.ArrayList(LirProcSpecId),
    /// Source location of the LIR statement currently being interpreted.
    active_stmt_loc: base.SourceLoc = base.SourceLoc.none,
    /// Checked source region of the LIR statement currently being interpreted.
    active_stmt_region: base.Region = base.Region.zero(),
    /// Source location captured when the current evaluation first failed.
    failed_stmt_loc: base.SourceLoc = base.SourceLoc.none,
    /// Checked source region captured when the current evaluation first failed.
    failed_stmt_region: base.Region = base.Region.zero(),
    comptime_branch_hits: std.ArrayList(ComptimeBranchHit),
    comptime_failed_site: ?LIR.ComptimeSiteId = null,

    const RcPresence = enum(u2) {
        unknown,
        active,
        no,
        yes,
    };

    pub const Error = error{
        OutOfMemory,
        RuntimeError,
        ComptimeExhaustiveness,
        DivisionByZero,
        Crash,
        ExpectErr,
    };

    pub const ComptimeBranchHit = struct {
        site: LIR.ComptimeSiteId,
        branch_index: u32,
    };

    const CrashBoundary = struct {
        env: *InterpreterRocEnv,
        prev_jmp_buf: ?*JmpBuf,

        fn init(env: *InterpreterRocEnv) CrashBoundary {
            env.resetCrash();
            return .{
                .env = env,
                .prev_jmp_buf = if (sljmp.supported) env.installJumpBuf(&env.jmp_buf) else null,
            };
        }

        fn deinit(self: *CrashBoundary) void {
            if (sljmp.supported) {
                self.env.restoreJumpBuf(self.prev_jmp_buf);
            }
        }

        inline fn set(self: *CrashBoundary) c_int {
            if (sljmp.supported) {
                return setjmp(&self.env.jmp_buf);
            }
            return 0;
        }
    };

    fn enterCrashBoundary(self: *LirInterpreter) CrashBoundary {
        return CrashBoundary.init(self.roc_env);
    }

    const LocalSlot = struct {
        assigned: bool = false,
        val: Value,
    };

    const FramePlan = struct {
        locals: []const LocalId,
        join_points: []const LIR.JoinPoint,
        free_slots: std.ArrayListUnmanaged([]LocalSlot) = .empty,

        fn deinit(self: *FramePlan, allocator: Allocator) void {
            while (self.free_slots.pop()) |slots| {
                allocator.free(slots);
            }
            self.free_slots.deinit(allocator);
        }

        fn acquireSlots(self: *FramePlan, allocator: Allocator) Allocator.Error![]LocalSlot {
            if (self.free_slots.pop()) |slots| return slots;
            return try allocator.alloc(LocalSlot, self.locals.len);
        }

        fn releaseSlots(self: *FramePlan, allocator: Allocator, slots: []LocalSlot) void {
            self.free_slots.append(allocator, slots) catch {
                allocator.free(slots);
            };
        }

        fn slotIndex(self: *const FramePlan, local_id: LocalId) ?usize {
            var low: usize = 0;
            var high: usize = self.locals.len;
            const target = @intFromEnum(local_id);
            while (low < high) {
                const mid = low + (high - low) / 2;
                const current = @intFromEnum(self.locals[mid]);
                if (current == target) return mid;
                if (current < target) {
                    low = mid + 1;
                } else {
                    high = mid;
                }
            }
            return null;
        }

        fn joinPoint(self: *const FramePlan, join_point_id: LIR.JoinPointId) ?LIR.JoinPoint {
            var low: usize = 0;
            var high: usize = self.join_points.len;
            const target = @intFromEnum(join_point_id);
            while (low < high) {
                const mid = low + (high - low) / 2;
                const current = @intFromEnum(self.join_points[mid].id);
                if (current == target) return self.join_points[mid];
                if (current < target) {
                    low = mid + 1;
                } else {
                    high = mid;
                }
            }
            return null;
        }
    };

    const Frame = struct {
        proc_id: LirProcSpecId,
        ret_layout: layout_mod.Idx,
        plan: *FramePlan,
        slots: []LocalSlot,

        fn deinit(self: *Frame, allocator: Allocator) void {
            self.plan.releaseSlots(allocator, self.slots);
        }

        fn slotIndex(self: *const Frame, local_id: LocalId) usize {
            if (self.plan.slotIndex(local_id)) |index| return index;
            if (builtin.mode == .Debug) {
                debugPrint(
                    "LIR/interpreter invariant violated: proc {d} frame plan does not contain local {d}\n",
                    .{ @intFromEnum(self.proc_id), @intFromEnum(local_id) },
                );
            }
            unreachable;
        }

        fn isAssigned(self: *const Frame, local_id: LocalId) bool {
            return self.slots[self.slotIndex(local_id)].assigned;
        }

        fn setLocal(self: *Frame, local_id: LocalId, value: Value) void {
            const slot = &self.slots[self.slotIndex(local_id)];
            slot.* = .{
                .assigned = true,
                .val = value,
            };
        }
    };

    const ExecOutcome = union(enum) {
        returned: LocalId,
        loop_continue,
        loop_break,
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
        const frame_plans = try buildFramePlans(allocator, store);
        errdefer deinitFramePlans(allocator, frame_plans);

        const rc_presence = try allocator.alloc(RcPresence, layout_store.layoutCount());
        errdefer allocator.free(rc_presence);
        @memset(rc_presence, .unknown);

        var rc_plans: std.AutoHashMapUnmanaged(u64, layout_mod.RcHelperPlan) = .{};
        errdefer rc_plans.deinit(allocator);
        var struct_field_plans: std.AutoHashMapUnmanaged(u64, ?layout_mod.RcFieldPlan) = .{};
        errdefer struct_field_plans.deinit(allocator);
        var tag_variant_plans: std.AutoHashMapUnmanaged(u64, ?layout_mod.RcHelperKey) = .{};
        errdefer tag_variant_plans.deinit(allocator);
        try reserveRcCaches(allocator, layout_store, &rc_plans, &struct_field_plans, &tag_variant_plans);

        const roc_env = try allocator.create(InterpreterRocEnv);
        roc_env.* = InterpreterRocEnv.init(allocator, caller_roc_ops);

        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .helper = LayoutHelper.init(layout_store),
            .arena = base.SingleThreadArena.init(allocator),
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
            .frame_plans = frame_plans,
            .rc_presence = rc_presence,
            .rc_plans = rc_plans,
            .struct_field_plans = struct_field_plans,
            .tag_variant_plans = tag_variant_plans,
            .call_stack = .empty,
            .failed_call_stack = .empty,
            .comptime_branch_hits = .empty,
        };
    }

    pub fn deinit(self: *LirInterpreter) void {
        self.comptime_branch_hits.deinit(self.evalAllocator());
        self.failed_call_stack.deinit(self.evalAllocator());
        self.call_stack.deinit(self.evalAllocator());
        self.roc_env.deinit();
        self.allocator.destroy(self.roc_env);
        self.arena.deinit();
        self.tag_variant_plans.deinit(self.allocator);
        self.struct_field_plans.deinit(self.allocator);
        self.rc_plans.deinit(self.allocator);
        self.allocator.free(self.rc_presence);
        deinitFramePlans(self.allocator, self.frame_plans);
    }

    fn deinitFramePlans(allocator: Allocator, frame_plans: []FramePlan) void {
        for (frame_plans) |*plan| {
            plan.deinit(allocator);
        }
        allocator.free(frame_plans);
    }

    fn buildFramePlans(allocator: Allocator, store: *const LirStore) Allocator.Error![]FramePlan {
        const proc_specs = store.getProcSpecs();
        const frame_plans = try allocator.alloc(FramePlan, proc_specs.len);
        var initialized: usize = 0;
        errdefer {
            for (frame_plans[0..initialized]) |*plan| {
                plan.deinit(allocator);
            }
            allocator.free(frame_plans);
        }

        for (proc_specs, 0..) |proc_spec, i| {
            frame_plans[i] = buildFramePlan(store, proc_spec);
            initialized += 1;
        }

        return frame_plans;
    }

    fn buildFramePlan(store: *const LirStore, proc_spec: LirProcSpec) FramePlan {
        return .{
            .locals = store.getLocalSpan(proc_spec.frame_locals),
            .join_points = store.getJoinPointSpan(proc_spec.join_points),
        };
    }

    fn reserveRcCaches(
        allocator: Allocator,
        layout_store: *const layout_mod.Store,
        rc_plans: *std.AutoHashMapUnmanaged(u64, layout_mod.RcHelperPlan),
        struct_field_plans: *std.AutoHashMapUnmanaged(u64, ?layout_mod.RcFieldPlan),
        tag_variant_plans: *std.AutoHashMapUnmanaged(u64, ?layout_mod.RcHelperKey),
    ) Allocator.Error!void {
        const layout_count = layout_store.layoutCount();
        try rc_plans.ensureTotalCapacity(allocator, try cacheCapacity(layout_count));

        var struct_field_count: usize = 0;
        var tag_variant_count: usize = 0;
        for (0..layout_count) |raw| {
            const layout_idx: layout_mod.Idx = @enumFromInt(raw);
            const layout_val = layout_store.getLayout(layout_idx);
            switch (layout_val.tag) {
                .struct_ => {
                    struct_field_count += layout_store.getStructData(layout_val.getStruct().idx).fields.count;
                },
                .tag_union => {
                    const tu_data = layout_store.getTagUnionData(layout_val.getTagUnion().idx);
                    tag_variant_count += layout_store.getTagUnionVariants(tu_data).len;
                },
                else => {},
            }
        }

        try struct_field_plans.ensureTotalCapacity(allocator, try cacheCapacity(struct_field_count));
        try tag_variant_plans.ensureTotalCapacity(allocator, try cacheCapacity(tag_variant_count));
    }

    fn cacheCapacity(count: usize) Allocator.Error!u32 {
        const capacity = std.math.mul(usize, count, 3) catch return error.OutOfMemory;
        return std.math.cast(u32, capacity) orelse error.OutOfMemory;
    }

    fn evalAllocator(self: *LirInterpreter) Allocator {
        return self.arena.allocator();
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

    pub fn getExpectFailures(self: *const LirInterpreter) []const ExpectFailure {
        return self.roc_env.expect_failures.items;
    }

    /// The failure message from a `?` operator that evaluated an Err inside a
    /// top-level expect, if the last evaluation failed that way.
    /// Owned by the interpreter and valid until the next eval or deinit.
    pub fn getExpectErrMessage(self: *const LirInterpreter) ?[]const u8 {
        return self.roc_env.expect_err_message;
    }

    /// The source region of the `?` whose Err failed the expect.
    pub fn getExpectErrRegion(self: *const LirInterpreter) ?base.Region {
        return self.roc_env.expect_err_region;
    }

    pub fn getFailedCallStack(self: *const LirInterpreter) []const LirProcSpecId {
        return self.failed_call_stack.items;
    }

    pub fn getFailedSourceLoc(self: *const LirInterpreter) ?base.SourceLoc {
        if (self.failed_stmt_loc.hasLocation()) return self.failed_stmt_loc;
        return null;
    }

    pub fn getFailedCheckedRegion(self: *const LirInterpreter) ?base.Region {
        if (self.failed_stmt_loc.hasLocation()) return self.failed_stmt_region;
        return null;
    }

    pub fn getComptimeFailedSite(self: *const LirInterpreter) ?LIR.ComptimeSiteId {
        return self.comptime_failed_site;
    }

    pub fn getComptimeBranchHits(self: *const LirInterpreter) []const ComptimeBranchHit {
        return self.comptime_branch_hits.items;
    }

    fn recordFailedCallStackIfUnset(self: *LirInterpreter) Allocator.Error!void {
        if (self.failed_call_stack.items.len != 0) return;
        try self.failed_call_stack.appendSlice(self.evalAllocator(), self.call_stack.items);
    }

    fn recordActiveFailureLocIfUnset(self: *LirInterpreter) void {
        if (self.failed_stmt_loc.hasLocation()) return;
        if (self.active_stmt_loc.hasLocation()) {
            self.failed_stmt_loc = self.active_stmt_loc;
            self.failed_stmt_region = self.active_stmt_region;
        }
    }

    fn recordCallerFailureLocForSourcelessCallee(
        self: *LirInterpreter,
        call_loc: base.SourceLoc,
        call_region: base.Region,
    ) void {
        if (!call_loc.hasLocation()) return;
        if (!self.failed_stmt_loc.hasLocation()) {
            self.failed_stmt_loc = call_loc;
            self.failed_stmt_region = call_region;
        }
    }

    fn recordCallerFailureLocForCalleeError(
        self: *LirInterpreter,
        call_loc: base.SourceLoc,
        call_region: base.Region,
        err: Error,
    ) void {
        switch (err) {
            error.Crash,
            error.DivisionByZero,
            error.RuntimeError,
            => self.recordCallerFailureLocForSourcelessCallee(call_loc, call_region),
            error.OutOfMemory,
            error.ComptimeExhaustiveness,
            error.ExpectErr,
            => {},
        }
    }

    /// Release ownership of an evaluated result value.
    /// Decrements reference counts for any heap-allocated data (strings, lists, boxes)
    /// according to the value's layout. No-op for non-refcounted types (ints, bools, etc).
    pub fn dropValue(self: *LirInterpreter, val: Value, layout_idx: layout_mod.Idx) void {
        self.performInterpreterApiRc(.decref, val, layout_idx, 0);
    }

    fn runtimeError(self: *LirInterpreter, message: []const u8) Error {
        self.recordActiveFailureLocIfUnset();
        self.roc_env.runtime_error_message = message;
        return error.RuntimeError;
    }

    fn comptimeExhaustivenessFailed(self: *LirInterpreter, site: LIR.ComptimeSiteId) Error {
        self.comptime_failed_site = site;
        return error.ComptimeExhaustiveness;
    }

    fn divisionByZeroMessageForLayout(self: *const LirInterpreter, layout_idx: layout_mod.Idx) []const u8 {
        return switch (layout_idx) {
            .u8 => "U8 division by zero",
            .i8 => "I8 division by zero",
            .u16 => "U16 division by zero",
            .i16 => "I16 division by zero",
            .u32 => "U32 division by zero",
            .i32 => "I32 division by zero",
            .u64 => "U64 division by zero",
            .i64 => "I64 division by zero",
            .u128 => "U128 division by zero",
            .i128 => "I128 division by zero",
            .dec => "Dec division by zero",
            else => self.invariantFailed(
                "LIR/interpreter invariant violated: division by zero reported for non-crashing numeric layout {d}",
                .{@intFromEnum(layout_idx)},
            ),
        };
    }

    fn divisionByZero(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error {
        return self.triggerCrash(self.divisionByZeroMessageForLayout(layout_idx));
    }

    fn triggerCrash(self: *LirInterpreter, message: []const u8) Error {
        self.recordActiveFailureLocIfUnset();
        self.roc_env.reportCrash(message);
        return error.Crash;
    }

    fn invariantFailed(_: *const LirInterpreter, comptime fmt: []const u8, args: anytype) noreturn {
        if (builtin.mode == .Debug) {
            debugPrint(fmt, args);
            debugPrint("\n", .{});
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
        const sa = self.helper.sizeAlignOf(layout_idx);
        if (sa.size == 0) return Value.zst;
        const slice = try self.allocAlignedByteSlice(sa.size, sa.alignment);
        return Value.fromSlice(slice);
    }

    fn allocAlignedBytes(self: *LirInterpreter, size: usize, alignment: layout_mod.RocAlignment) Error!Value {
        if (size == 0) return Value.zst;
        return Value.fromSlice(try self.allocAlignedByteSlice(size, alignment));
    }

    fn allocAlignedByteSlice(self: *LirInterpreter, size: usize, alignment: layout_mod.RocAlignment) Error![]u8 {
        const slice = switch (alignment) {
            .@"1" => self.arena.allocator().alignedAlloc(u8, .@"1", size),
            .@"2" => self.arena.allocator().alignedAlloc(u8, .@"2", size),
            .@"4" => self.arena.allocator().alignedAlloc(u8, .@"4", size),
            .@"8" => self.arena.allocator().alignedAlloc(u8, .@"8", size),
            .@"16" => self.arena.allocator().alignedAlloc(u8, .@"16", size),
            _ => unreachable,
        } catch return error.OutOfMemory;
        @memset(slice, 0);
        return slice;
    }

    fn poisonUninitializedValue(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error!Value {
        const sa = self.helper.sizeAlignOf(layout_idx);
        if (sa.size == 0) return Value.zst;
        const slice = try self.allocAlignedByteSlice(sa.size, sa.alignment);
        @memset(slice, 0xAA);
        return Value.fromSlice(slice);
    }

    fn maxRocAlignment(a: layout_mod.RocAlignment, b: layout_mod.RocAlignment) layout_mod.RocAlignment {
        return if (@intFromEnum(a) >= @intFromEnum(b)) a else b;
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

            const copy = try self.allocAlignedBytes(sa.size, sa.alignment);
            @memcpy(copy.ptr[0..sa.size], arg_bytes[arg_offsets[i] .. arg_offsets[i] + sa.size]);
            args_buf[i] = copy;
        }
        return args_buf;
    }

    /// Look up the platform entrypoint by ordinal, build its argument layout
    /// list from the proc spec, and run it with the RocOps bound at init.
    ///
    /// Returns `error.EntrypointNotFound` if no entrypoint matches `ordinal`.
    /// Other errors come from `eval`.
    pub fn runEntrypoint(
        self: *LirInterpreter,
        view: *const lir.LirImage.ProgramView,
        ordinal: u32,
        arg_ptr: ?*anyopaque,
        ret_ptr: ?*anyopaque,
    ) (Error || error{EntrypointNotFound})!EvalResult {
        var entrypoint: ?lir.LirImage.PlatformEntrypoint = null;
        for (view.platform_entrypoints) |candidate| {
            if (candidate.ordinal == ordinal) {
                entrypoint = candidate;
                break;
            }
        }
        const selected = entrypoint orelse return error.EntrypointNotFound;

        const proc = view.store.getProcSpec(selected.root_proc);
        const arg_ids = view.store.getLocalSpan(proc.args);
        const arg_layouts = try self.allocator.alloc(layout_mod.Idx, arg_ids.len);
        defer self.allocator.free(arg_layouts);
        for (arg_ids, 0..) |local_id, i| {
            arg_layouts[i] = view.store.locals.items[@intFromEnum(local_id)].layout_idx;
        }

        return self.eval(.{
            .proc_id = selected.root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = proc.ret_layout,
            .arg_ptr = arg_ptr,
            .ret_ptr = ret_ptr,
        });
    }

    /// Evaluate a proc-root LIR program using the RocOps bound at initialization time.
    pub fn eval(self: *LirInterpreter, request: EvalRequest) Error!EvalResult {
        self.roc_env.resetForEval();
        self.call_stack.clearRetainingCapacity();
        self.failed_call_stack.clearRetainingCapacity();
        self.active_stmt_loc = base.SourceLoc.none;
        self.active_stmt_region = base.Region.zero();
        self.failed_stmt_loc = base.SourceLoc.none;
        self.failed_stmt_region = base.Region.zero();
        self.comptime_branch_hits.clearRetainingCapacity();
        self.comptime_failed_site = null;

        if (sljmp.supported) {
            var eval_jmp_buf: JmpBuf = undefined;
            const prev_jmp_buf = self.roc_env.installJumpBuf(&eval_jmp_buf);
            defer self.roc_env.restoreJumpBuf(prev_jmp_buf);
            const sj = setjmp(&eval_jmp_buf);
            if (sj != 0) {
                self.recordActiveFailureLocIfUnset();
                self.recordFailedCallStackIfUnset() catch {};
                return error.Crash;
            }
        }

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

    const DebugValuePathStep = union(enum) {
        box_payload: layout_mod.Idx,
        list_elem: struct {
            index: usize,
            elem_layout: layout_mod.Idx,
        },
        struct_field: struct {
            sorted_index: usize,
            semantic_index: u16,
            field_layout: layout_mod.Idx,
        },
        tag_payload: struct {
            tag_index: usize,
            payload_layout: layout_mod.Idx,
        },
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
            defer visited.deinit(self.evalAllocator());
            self.debugAssertValueMatchesLayout(frame.proc_id, stmt_id, local_id, value, layout_idx, &visited);
        }

        frame.setLocal(local_id, value);
    }

    fn getLocalChecked(self: *LirInterpreter, frame: *const Frame, local_id: LocalId) Error!Value {
        const slot = frame.slots[frame.slotIndex(local_id)];
        if (!slot.assigned) {
            if (comptime builtin.target.os.tag != .freestanding) {
                const proc = self.store.getProcSpec(frame.proc_id);
                debugPrint(
                    "LIR/interpreter unassigned local in proc {d}: name={d} body={any} local={d} layout={d}\n",
                    .{
                        @intFromEnum(frame.proc_id),
                        proc.name.raw(),
                        proc.body,
                        @intFromEnum(local_id),
                        @intFromEnum(self.store.getLocal(local_id).layout_idx),
                    },
                );
                const params = self.store.getLocalSpan(proc.args);
                debugPrint("  proc params:", .{});
                for (params) |param| {
                    debugPrint(" {d}:layout={d}", .{
                        @intFromEnum(param),
                        @intFromEnum(self.store.getLocal(param).layout_idx),
                    });
                }
                debugPrint("\n", .{});
                if (proc.body) |body| self.debugPrintStmtChain(body, 80);
            }
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
        var path_buf: [96]DebugValuePathStep = undefined;
        self.debugAssertValueMatchesLayoutAt(proc_id, stmt_id, local_id, value, layout_idx, visited, &path_buf, 0);
    }

    fn debugAssertValueMatchesLayoutAt(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        stmt_id: ?CFStmtId,
        local_id: LocalId,
        value: Value,
        layout_idx: layout_mod.Idx,
        visited: *std.ArrayList(DebugVisitedValue),
        path_buf: []DebugValuePathStep,
        path_len: usize,
    ) void {
        if (builtin.mode != .Debug) return;
        if (comptime builtin.target.os.tag == .freestanding) return;
        // Bound the walk: stop descending into very deep structures
        // (e.g. long TRMC-built lists), since this walk recurses natively, and
        // stop after a bounded number of heap cells so wide structures don't
        // make every assignment O(structure size).
        if (path_len >= max_debug_value_depth) return;
        if (visited.items.len >= max_debug_value_visits) return;

        const layout_val = self.layout_store.getLayout(layout_idx);
        switch (layout_val.tag) {
            .scalar => {
                if (layout_idx == .str) {
                    const str = valueToRocStr(value);
                    if (!str.isSmallStr() and str.len() > 0 and str.bytes == null) {
                        self.debugValueShapePanicAt(
                            proc_id,
                            stmt_id,
                            local_id,
                            layout_idx,
                            path_buf[0..path_len],
                            "non-small RocStr had null bytes pointer",
                        );
                    }
                }
            },
            .zst, .box_of_zst => return,
            // Compiler-internal pointers (TRMC holes) are opaque here: the slot they
            // point at may be a not-yet-filled hole, so there is nothing to validate.
            .ptr => return,
            .box => {
                const data_ptr = self.readBoxedDataPointer(value) orelse {
                    // Inside a TRMC-transformed proc a null box pointer is a
                    // legal in-flight hole (zero-filled cells await their child
                    // value); everywhere else it is a real bug.
                    if (self.store.getProcSpec(proc_id).tail_transform == .trmc) return;
                    self.debugValueShapePanicAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        path_buf[0..path_len],
                        "boxed value had null data pointer",
                    );
                };

                const key = DebugVisitedValue{
                    .ptr = @intFromPtr(data_ptr),
                    .layout_idx = layout_idx,
                };
                for (visited.items) |entry| {
                    if (entry.ptr == key.ptr and entry.layout_idx == key.layout_idx) return;
                }
                visited.append(self.evalAllocator(), key) catch {
                    self.invariantFailed("LIR/interpreter invariant violated: out of memory while validating value shape", .{});
                };
                var next_len = path_len;
                if (next_len < path_buf.len) {
                    path_buf[next_len] = .{ .box_payload = layout_val.getIdx() };
                    next_len += 1;
                }
                self.debugAssertValueMatchesLayoutAt(
                    proc_id,
                    stmt_id,
                    local_id,
                    .{ .ptr = data_ptr },
                    layout_val.getIdx(),
                    visited,
                    path_buf,
                    next_len,
                );
            },
            .erased_callable => {
                const data_ptr = self.readBoxedDataPointer(value) orelse self.debugValueShapePanicAt(
                    proc_id,
                    stmt_id,
                    local_id,
                    layout_idx,
                    path_buf[0..path_len],
                    "boxed erased callable had null payload pointer",
                );
                _ = builtins.erased_callable.payloadPtr(data_ptr);
            },
            .list => {
                if (value.isZst()) {
                    self.debugValueShapePanicAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        path_buf[0..path_len],
                        "list value used ZST sentinel instead of RocList bytes",
                    );
                }
                const list = valueToRocList(value);
                if (list.len() > 0 and list.bytes == null) {
                    self.debugValueShapePanicAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        path_buf[0..path_len],
                        "non-empty list had null bytes pointer",
                    );
                }
                if (list.len() == 0 or list.bytes == null) return;

                const elem_layout = layout_val.getIdx();
                const elem_size = self.helper.sizeOf(elem_layout);
                if (elem_size == 0) return;

                for (0..list.len()) |i| {
                    var next_len = path_len;
                    if (next_len < path_buf.len) {
                        path_buf[next_len] = .{ .list_elem = .{
                            .index = i,
                            .elem_layout = elem_layout,
                        } };
                        next_len += 1;
                    }
                    self.debugAssertValueMatchesLayoutAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        .{ .ptr = list.bytes.? + i * elem_size },
                        elem_layout,
                        visited,
                        path_buf,
                        next_len,
                    );
                }
            },
            .list_of_zst => {
                if (value.isZst()) {
                    self.debugValueShapePanicAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        path_buf[0..path_len],
                        "list_of_zst value used ZST sentinel instead of RocList bytes",
                    );
                }
            },
            .struct_ => {
                const struct_info = self.layout_store.getStructInfo(layout_val);
                for (0..struct_info.fields.len) |i| {
                    const field = struct_info.fields.get(@intCast(i));
                    // Padding spacers hold uninitialized bytes; there is no value
                    // to validate against their (size-only) layout.
                    if (field.is_padding) continue;
                    const field_offset = self.layout_store.getStructFieldOffset(layout_val.getStruct().idx, @intCast(i));
                    var next_len = path_len;
                    if (next_len < path_buf.len) {
                        path_buf[next_len] = .{ .struct_field = .{
                            .sorted_index = i,
                            .semantic_index = field.index,
                            .field_layout = field.layout,
                        } };
                        next_len += 1;
                    }
                    self.debugAssertValueMatchesLayoutAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        value.offset(field_offset),
                        field.layout,
                        visited,
                        path_buf,
                        next_len,
                    );
                }
            },
            .tag_union => {
                if (value.isZst() and self.helper.sizeOf(layout_idx) > 0) {
                    self.debugValueShapePanicAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        path_buf[0..path_len],
                        "tag union value used ZST sentinel for nonzero tag layout",
                    );
                }
                const disc = self.helper.readTagDiscriminant(value, layout_idx);
                const tag_union_info = self.layout_store.getTagUnionInfo(layout_val);
                if (disc >= tag_union_info.variants.len) {
                    self.debugValueShapePanicAt(
                        proc_id,
                        stmt_id,
                        local_id,
                        layout_idx,
                        path_buf[0..path_len],
                        "tag union discriminant was out of range",
                    );
                }

                const payload_layout = tag_union_info.variants.get(disc).payload_layout;
                if (self.helper.sizeOf(payload_layout) == 0) return;

                var next_len = path_len;
                if (next_len < path_buf.len) {
                    path_buf[next_len] = .{ .tag_payload = .{
                        .tag_index = disc,
                        .payload_layout = payload_layout,
                    } };
                    next_len += 1;
                }
                self.debugAssertValueMatchesLayoutAt(
                    proc_id,
                    stmt_id,
                    local_id,
                    value,
                    payload_layout,
                    visited,
                    path_buf,
                    next_len,
                );
            },
            .closure => {
                self.debugValueShapePanicAt(
                    proc_id,
                    stmt_id,
                    local_id,
                    layout_idx,
                    path_buf[0..path_len],
                    "closure value reached interpreter recursive validator unexpectedly",
                );
            },
        }
    }

    fn debugValueShapePanicAt(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        stmt_id: ?CFStmtId,
        local_id: LocalId,
        layout_idx: layout_mod.Idx,
        path: []const DebugValuePathStep,
        comptime reason: []const u8,
    ) noreturn {
        if (comptime builtin.target.os.tag != .freestanding) {
            debugPrint("LIR/interpreter value path:", .{});
            for (path) |step| {
                switch (step) {
                    .box_payload => |payload_layout| debugPrint(" .box(layout={d})", .{@intFromEnum(payload_layout)}),
                    .list_elem => |list| debugPrint(" [{d}:layout={d}]", .{ list.index, @intFromEnum(list.elem_layout) }),
                    .struct_field => |field| debugPrint(" .field(sorted={d}, semantic={d}, layout={d})", .{ field.sorted_index, field.semantic_index, @intFromEnum(field.field_layout) }),
                    .tag_payload => |tag| debugPrint(" .tag_payload(index={d}, layout={d})", .{ tag.tag_index, @intFromEnum(tag.payload_layout) }),
                }
            }
            debugPrint("\n", .{});
            var visited_layouts = std.ArrayList(u32).empty;
            defer visited_layouts.deinit(self.evalAllocator());
            debugPrint("LIR/interpreter local layout tree:\n", .{});
            self.debugPrintLayoutShapeLines(self.store.getLocal(local_id).layout_idx, 0, &visited_layouts);
        }
        self.debugValueShapePanic(proc_id, stmt_id, local_id, layout_idx, reason);
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
                debugPrint("LIR/interpreter stmt window around failing stmt {d}:\n", .{@intFromEnum(id)});
                for (start..end) |i| {
                    const window_id: CFStmtId = @enumFromInt(@as(u32, @intCast(i)));
                    debugPrint("  stmt {d}: {any}\n", .{ i, self.store.getCFStmt(window_id) });
                }

                switch (self.store.getCFStmt(id)) {
                    .assign_call => |assign| {
                        const callee_proc = self.store.getProcSpec(assign.proc);
                        debugPrint(
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
        debugPrint(
            "LIR/interpreter stmt chain from {d}:\n",
            .{@intFromEnum(start_stmt)},
        );

        var current = start_stmt;
        var remaining = limit;
        while (remaining > 0) : (remaining -= 1) {
            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_literal => |assign| {
                    const layout_idx = self.store.getLocal(assign.target).layout_idx;
                    const layout_val = self.layout_store.getLayout(layout_idx);
                    debugPrint(
                        "  stmt {d}: {any} target_layout={d} tag={s} size={d}\n",
                        .{
                            @intFromEnum(current),
                            stmt,
                            @intFromEnum(layout_idx),
                            @tagName(layout_val.tag),
                            self.helper.sizeOf(layout_idx),
                        },
                    );
                },
                .assign_call => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_call_erased => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_packed_erased_fn => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_low_level => |assign| {
                    debugPrint(
                        "  stmt {d}: {any} target_layout={d} args=",
                        .{
                            @intFromEnum(current),
                            stmt,
                            @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                        },
                    );
                    for (self.store.getLocalSpan(assign.args)) |arg_local| {
                        debugPrint("{d}:layout={d} ", .{
                            @intFromEnum(arg_local),
                            @intFromEnum(self.store.getLocal(arg_local).layout_idx),
                        });
                    }
                    debugPrint("\n", .{});
                },
                .assign_list => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_struct => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .assign_tag => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                .set_local => |assign| debugPrint(
                    "  stmt {d}: {any} target_layout={d} target_layout_data={any}\n",
                    .{
                        @intFromEnum(current),
                        stmt,
                        @intFromEnum(self.store.getLocal(assign.target).layout_idx),
                        self.layout_store.getLayout(self.store.getLocal(assign.target).layout_idx),
                    },
                ),
                else => debugPrint("  stmt {d}: {any}\n", .{ @intFromEnum(current), stmt }),
            }
            current = switch (stmt) {
                .assign_ref => |assign| assign.next,
                .assign_literal => |assign| assign.next,
                .init_uninitialized => |uninit| uninit.next,
                .assign_call => |assign| assign.next,
                .assign_call_erased => |assign| assign.next,
                .assign_packed_erased_fn => |assign| assign.next,
                .assign_low_level => |assign| assign.next,
                .assign_list => |assign| assign.next,
                .assign_struct => |assign| assign.next,
                .assign_tag => |assign| assign.next,
                .set_local => |assign| assign.next,
                .debug => |stmt_next| stmt_next.next,
                .expect => |stmt_next| stmt_next.next,
                .comptime_branch_taken => |marker| marker.next,
                .incref => |stmt_next| stmt_next.next,
                .decref => |stmt_next| stmt_next.next,
                .decref_if_initialized => |stmt_next| stmt_next.next,
                .free => |stmt_next| stmt_next.next,
                .join => |join_stmt| join_stmt.body,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .jump,
                .ret,
                .crash,
                .expect_err,
                .loop_continue,
                .loop_break,
                => break,
            };
        }
    }

    fn debugPrintLayoutShapeLines(
        self: *LirInterpreter,
        layout_idx: layout_mod.Idx,
        indent: usize,
        visited: *std.ArrayList(u32),
    ) void {
        for (visited.items) |existing| {
            if (existing == @intFromEnum(layout_idx)) {
                debugPrint("{s}{d} (cycle)\n", .{ debugIndent(indent), @intFromEnum(layout_idx) });
                return;
            }
        }

        visited.append(self.evalAllocator(), @intFromEnum(layout_idx)) catch return;
        defer _ = visited.pop();

        const layout_val = self.layout_store.getLayout(layout_idx);
        debugPrint("{s}{d}: {s}\n", .{ debugIndent(indent), @intFromEnum(layout_idx), @tagName(layout_val.tag) });
        switch (layout_val.tag) {
            .scalar, .zst, .box_of_zst, .list_of_zst, .erased_callable => {},
            .box, .ptr => self.debugPrintLayoutShapeLines(layout_val.getIdx(), indent + 1, visited),
            .list => self.debugPrintLayoutShapeLines(layout_val.getIdx(), indent + 1, visited),
            .closure => self.debugPrintLayoutShapeLines(layout_val.getClosure().captures_layout_idx, indent + 1, visited),
            .struct_ => {
                const info = self.layout_store.getStructInfo(layout_val);
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(@intCast(i));
                    debugPrint("{s}field[{d}] semantic_index={d}\n", .{ debugIndent(indent + 1), i, field.index });
                    self.debugPrintLayoutShapeLines(field.layout, indent + 2, visited);
                }
            },
            .tag_union => {
                const info = self.layout_store.getTagUnionInfo(layout_val);
                for (0..info.variants.len) |i| {
                    const variant = info.variants.get(@intCast(i));
                    debugPrint("{s}variant[{d}]\n", .{ debugIndent(indent + 1), i });
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
        try self.call_stack.append(self.evalAllocator(), proc_id);
        defer _ = self.call_stack.pop();
        errdefer self.recordFailedCallStackIfUnset() catch {};

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
            if (args.len != param_layouts.len) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: hosted proc {d} received {d} args but has {d} param layouts",
                    .{ proc_spec.name.raw(), args.len, param_layouts.len },
                );
            }
            const normalized_args = try self.arena.allocator().alloc(Value, args.len);
            for (args, arg_layouts, param_layouts, 0..) |arg, arg_layout, param_layout, i| {
                normalized_args[i] = try self.coerceExplicitRefValueToLayout(arg, arg_layout, param_layout);
            }
            return self.callHostedProc(proc_id, hosted, normalized_args, param_layouts, proc_spec.ret_layout);
        }

        trace.log(
            "enter proc={d} name={d} depth={d} args={d} ret_layout={d}",
            .{
                @intFromEnum(proc_id),
                proc_spec.name.raw(),
                self.call_depth,
                args.len,
                @intFromEnum(proc_spec.ret_layout),
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

        for (params, args, arg_layouts, 0..) |param, arg, arg_layout, i| {
            const param_layout = self.store.getLocal(param).layout_idx;
            if (proc_spec.abi == .erased_callable and i + 1 == params.len) {
                if (param_layout != .opaque_ptr or arg_layout != .opaque_ptr) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: erased callable proc {d} hidden capture parameter was not opaque_ptr",
                        .{@intFromEnum(proc_id)},
                    );
                }
            }

            if (builtin.mode == .Debug and arg_layout != param_layout) {
                const actual_layout_val = self.layout_store.getLayout(arg_layout);
                const expected_layout_val = self.layout_store.getLayout(param_layout);
                if (actual_layout_val.tag == .struct_ or expected_layout_val.tag == .struct_ or
                    actual_layout_val.tag == .tag_union or expected_layout_val.tag == .tag_union)
                {
                    debugPrint(
                        "LIR/interpreter invariant violated before proc arg coercion: proc={d} name={d} arg_index={d} actual_layout={d} ({s}) expected_layout={d} ({s}) param_local={d}\n",
                        .{
                            @intFromEnum(proc_id),
                            proc_spec.name.raw(),
                            i,
                            @intFromEnum(arg_layout),
                            @tagName(actual_layout_val.tag),
                            @intFromEnum(param_layout),
                            @tagName(expected_layout_val.tag),
                            @intFromEnum(param),
                        },
                    );
                    debugPrint("  call stack:", .{});
                    for (self.call_stack.items) |stack_proc| {
                        debugPrint(" {d}", .{@intFromEnum(stack_proc)});
                    }
                    debugPrint("\n", .{});
                    for (self.call_stack.items) |stack_proc| {
                        self.debugDumpProc(stack_proc);
                    }
                }
            }

            const coerced = try self.coerceExplicitRefValueToLayout(
                arg,
                arg_layout,
                param_layout,
            );
            self.setLocalChecked(
                &frame,
                null,
                param,
                try self.materializeLocalValue(coerced, param_layout),
            );
        }
        const body = self.requireProcBody(proc_id, proc_spec);
        if (trace.enabled) self.debugPrintStmtChain(body, 32);
        const outcome = try self.execStmtChain(&frame, body);
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
                    defer visited.deinit(self.evalAllocator());
                    self.debugAssertValueMatchesLayout(proc_id, null, ret_local, raw_result, raw_layout, &visited);
                }
                const coerced_result = try self.coerceExplicitRefValueToLayout(
                    raw_result,
                    raw_layout,
                    proc_spec.ret_layout,
                );
                if (builtin.mode == .Debug) {
                    var visited = std.ArrayList(DebugVisitedValue).empty;
                    defer visited.deinit(self.evalAllocator());
                    self.debugAssertValueMatchesLayout(proc_id, null, ret_local, coerced_result, proc_spec.ret_layout, &visited);
                }
                break :blk try self.materializeLocalValue(coerced_result, proc_spec.ret_layout);
            },
            .loop_continue => return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} terminated via loop_continue",
                .{proc_spec.name.raw()},
            ),
            .loop_break => return self.invariantFailedError(
                "LIR/interpreter invariant violated: proc {d} terminated via loop_break",
                .{proc_spec.name.raw()},
            ),
        };
    }

    fn initFrame(self: *LirInterpreter, proc_id: LirProcSpecId, proc_spec: LirProcSpec) Error!Frame {
        const plan = &self.frame_plans[@intFromEnum(proc_id)];
        const slots = try plan.acquireSlots(self.allocator);
        @memset(slots, .{ .assigned = false, .val = Value.zst });
        for (slots, plan.locals) |*slot, local_id| {
            const layout_idx = self.store.getLocal(local_id).layout_idx;
            if (self.layout_store.getLayout(layout_idx).tag == .zst) {
                slot.assigned = true;
            }
        }

        return .{
            .proc_id = proc_id,
            .ret_layout = proc_spec.ret_layout,
            .plan = plan,
            .slots = slots,
        };
    }

    fn requireProcBody(self: *LirInterpreter, proc_id: LirProcSpecId, proc_spec: LirProcSpec) CFStmtId {
        return proc_spec.body orelse self.invariantFailed(
            "LIR/interpreter invariant violated: non-hosted proc {d} missing statement body",
            .{@intFromEnum(proc_id)},
        );
    }

    fn execStmtChain(
        self: *LirInterpreter,
        frame: *Frame,
        start_stmt: CFStmtId,
    ) Error!ExecOutcome {
        var current = start_stmt;
        while (true) {
            const stmt = self.store.getCFStmt(current);
            self.active_stmt_loc = self.store.stmtLoc(current);
            self.active_stmt_region = self.store.stmtRegion(current);
            switch (stmt) {
                .assign_ref => |assign| {
                    const target_layout = self.store.getLocal(assign.target).layout_idx;
                    const value = try self.evalAssignRef(frame, assign.op, target_layout);
                    self.setLocalChecked(frame, current, assign.target, value);
                    current = assign.next;
                },
                .assign_literal => |assign| {
                    self.setLocalChecked(frame, current, assign.target, try self.evalLiteral(assign.value, self.store.getLocal(assign.target).layout_idx));
                    current = assign.next;
                },
                .init_uninitialized => |uninit| {
                    frame.setLocal(
                        uninit.target,
                        try self.poisonUninitializedValue(self.store.getLocal(uninit.target).layout_idx),
                    );
                    current = uninit.next;
                },
                .assign_call => |assign| {
                    const arg_locals = self.store.getLocalSpan(assign.args);
                    const arg_values = try self.collectLocalValues(frame, arg_locals);
                    const arg_layouts = try self.localLayouts(arg_locals);
                    const call_loc = self.active_stmt_loc;
                    const call_region = self.active_stmt_region;
                    const result = self.evalProcById(assign.proc, arg_values, arg_layouts) catch |err| {
                        self.recordCallerFailureLocForCalleeError(call_loc, call_region, err);
                        return err;
                    };
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
                .assign_call_erased => |assign| {
                    const arg_locals = self.store.getLocalSpan(assign.args);
                    const arg_values = try self.collectLocalValues(frame, arg_locals);
                    const call_loc = self.active_stmt_loc;
                    const call_region = self.active_stmt_region;
                    const result = self.evalErasedCall(
                        frame,
                        assign.closure,
                        arg_values,
                        try self.localLayouts(arg_locals),
                        self.store.getLocal(assign.target).layout_idx,
                    ) catch |err| {
                        self.recordCallerFailureLocForCalleeError(call_loc, call_region, err);
                        return err;
                    };
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
                .assign_packed_erased_fn => |assign| {
                    self.setLocalChecked(
                        frame,
                        current,
                        assign.target,
                        try self.evalPackedErasedFn(frame, assign, self.store.getLocal(assign.target).layout_idx),
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
                        .unique_args = assign.unique_args,
                        .interchangeable = assign.interchangeable,
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
                    self.setLocalChecked(frame, current, assign.target, try self.evalTagLiteral(
                        frame,
                        assign.variant_index,
                        assign.discriminant,
                        assign.payload,
                        self.store.getLocal(assign.target).layout_idx,
                    ));
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
                        try self.roc_env.recordExpectFailure("expect failed", self.store.stmtLoc(current));
                        self.roc_ops.expectFailed("expect failed");
                    }
                    current = expect_stmt.next;
                },
                .runtime_error => {
                    return self.runtimeError("RuntimeError");
                },
                .comptime_exhaustiveness_failed => |failed| {
                    return self.comptimeExhaustivenessFailed(failed.site);
                },
                .comptime_branch_taken => |marker| {
                    try self.comptime_branch_hits.append(self.evalAllocator(), .{
                        .site = marker.site,
                        .branch_index = marker.branch_index,
                    });
                    current = marker.next;
                },
                .incref => |inc| {
                    if (builtin.mode == .Debug and !frame.isAssigned(inc.value)) {
                        debugPrint(
                            "LIR/interpreter invariant violated before incref: local {d} unassigned in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(inc.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 20);
                    }
                    trace_rc.log("stmt incref: proc={d} stmt={d} local={d} layout={d} count={d} ptr=0x{x}", .{
                        @intFromEnum(frame.proc_id),
                        @intFromEnum(current),
                        @intFromEnum(inc.value),
                        @intFromEnum(self.store.getLocal(inc.value).layout_idx),
                        inc.count,
                        @intFromPtr((try self.getLocalChecked(frame, inc.value)).ptr),
                    });
                    self.performExplicitRcStmt(
                        inc.rc,
                        try self.getLocalChecked(frame, inc.value),
                        inc.count,
                        inc.atomicity,
                    );
                    current = inc.next;
                },
                .decref => |dec| {
                    if (builtin.mode == .Debug and !frame.isAssigned(dec.value)) {
                        debugPrint(
                            "LIR/interpreter invariant violated before decref: local {d} unassigned in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(dec.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 20);
                    }
                    trace_rc.log("stmt decref: proc={d} stmt={d} local={d} layout={d} ptr=0x{x}", .{
                        @intFromEnum(frame.proc_id),
                        @intFromEnum(current),
                        @intFromEnum(dec.value),
                        @intFromEnum(self.store.getLocal(dec.value).layout_idx),
                        @intFromPtr((try self.getLocalChecked(frame, dec.value)).ptr),
                    });
                    self.performExplicitRcStmt(
                        dec.rc,
                        try self.getLocalChecked(frame, dec.value),
                        0,
                        dec.atomicity,
                    );
                    current = dec.next;
                },
                .decref_if_initialized => |dec| {
                    const cond_value = try self.readSwitchValue(
                        try self.getLocalChecked(frame, dec.cond),
                        self.store.getLocal(dec.cond).layout_idx,
                    );
                    if ((cond_value & dec.cond_mask) == dec.cond_mask) {
                        if (builtin.mode == .Debug and !frame.isAssigned(dec.value)) {
                            debugPrint(
                                "LIR/interpreter invariant violated before decref_if_initialized: local {d} unassigned in proc {d} at stmt {d}\n",
                                .{ @intFromEnum(dec.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                            );
                            self.debugDumpProc(frame.proc_id);
                            self.debugPrintStmtChain(current, 20);
                        }
                        trace_rc.log("stmt decref_if_initialized: proc={d} stmt={d} cond={d} mask=0x{x} local={d} layout={d} ptr=0x{x}", .{
                            @intFromEnum(frame.proc_id),
                            @intFromEnum(current),
                            @intFromEnum(dec.cond),
                            dec.cond_mask,
                            @intFromEnum(dec.value),
                            @intFromEnum(self.store.getLocal(dec.value).layout_idx),
                            @intFromPtr((try self.getLocalChecked(frame, dec.value)).ptr),
                        });
                        self.performExplicitRcStmt(
                            dec.rc,
                            try self.getLocalChecked(frame, dec.value),
                            0,
                            dec.atomicity,
                        );
                    }
                    current = dec.next;
                },
                .free => |free_stmt| {
                    if (builtin.mode == .Debug and !frame.isAssigned(free_stmt.value)) {
                        debugPrint(
                            "LIR/interpreter invariant violated before free: local {d} unassigned in proc {d} at stmt {d}\n",
                            .{ @intFromEnum(free_stmt.value), @intFromEnum(frame.proc_id), @intFromEnum(current) },
                        );
                        self.debugDumpProc(frame.proc_id);
                        self.debugPrintStmtChain(current, 20);
                    }
                    trace_rc.log("stmt free: proc={d} stmt={d} local={d} layout={d} ptr=0x{x}", .{
                        @intFromEnum(frame.proc_id),
                        @intFromEnum(current),
                        @intFromEnum(free_stmt.value),
                        @intFromEnum(self.store.getLocal(free_stmt.value).layout_idx),
                        @intFromPtr((try self.getLocalChecked(frame, free_stmt.value)).ptr),
                    });
                    self.performExplicitRcStmt(
                        free_stmt.rc,
                        try self.getLocalChecked(frame, free_stmt.value),
                        0,
                        free_stmt.atomicity,
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
                    current = target;
                },
                .switch_initialized_payload => |switch_stmt| {
                    const cond_value = try self.readSwitchValue(
                        try self.getLocalChecked(frame, switch_stmt.cond),
                        self.store.getLocal(switch_stmt.cond).layout_idx,
                    );
                    if (trace.enabled) {
                        trace.log(
                            "switch_initialized_payload: cond_local={d} mask=0x{x} payload_local={d} value={d} initialized={d} uninitialized={d}",
                            .{
                                @intFromEnum(switch_stmt.cond),
                                switch_stmt.cond_mask,
                                @intFromEnum(switch_stmt.payload),
                                cond_value,
                                @intFromEnum(switch_stmt.initialized_branch),
                                @intFromEnum(switch_stmt.uninitialized_branch),
                            },
                        );
                    }
                    current = if ((cond_value & switch_stmt.cond_mask) == switch_stmt.cond_mask)
                        switch_stmt.initialized_branch
                    else
                        switch_stmt.uninitialized_branch;
                },
                .str_match => |str_match| {
                    current = try self.execStrMatch(frame, current, str_match);
                },
                .str_match_set => |str_match_set| {
                    current = try self.execStrMatchSet(frame, current, str_match_set);
                },
                .loop_continue => return .loop_continue,
                .loop_break => return .loop_break,
                .join => |join_stmt| {
                    current = join_stmt.remainder;
                },
                .jump => |jump_stmt| {
                    const join_point = frame.plan.joinPoint(jump_stmt.target) orelse self.invariantFailed(
                        "LIR/interpreter invariant violated: missing join point {d} in proc {d}",
                        .{ @intFromEnum(jump_stmt.target), @intFromEnum(frame.proc_id) },
                    );
                    current = join_point.body;
                },
                .ret => |ret_stmt| return .{ .returned = ret_stmt.value },
                .crash => |crash_stmt| return self.triggerCrash(self.store.getString(crash_stmt.msg)),
                .expect_err => |expect_err_stmt| {
                    const message_value = try self.getLocalChecked(frame, expect_err_stmt.message);
                    const message = self.readRocStr(message_value);
                    if (self.roc_env.expect_err_message) |old| self.roc_env.allocator.free(old);
                    self.roc_env.expect_err_message = self.roc_env.allocator.dupe(u8, message) catch null;
                    self.roc_env.expect_err_region = expect_err_stmt.region;
                    // The statement consumes the message's ownership unit.
                    self.dropValue(message_value, self.store.getLocal(expect_err_stmt.message).layout_idx);
                    return error.ExpectErr;
                },
            }
        }
    }

    fn debugDumpProc(self: *LirInterpreter, proc_id: LirProcSpecId) void {
        const proc_spec = self.store.getProcSpec(proc_id);
        const body = proc_spec.body orelse {
            debugPrint("  proc {d} has no body\n", .{@intFromEnum(proc_id)});
            return;
        };

        debugPrint(
            "  proc {d} name={d} body={d} ret_layout={d}\n",
            .{
                @intFromEnum(proc_id),
                proc_spec.name.raw(),
                @intFromEnum(body),
                @intFromEnum(proc_spec.ret_layout),
            },
        );
        const args = self.store.getLocalSpan(proc_spec.args);
        if (args.len > 0) {
            debugPrint("  args:", .{});
            for (args) |arg| {
                const layout_idx = self.store.getLocal(arg).layout_idx;
                debugPrint(" {d}:{d}", .{ @intFromEnum(arg), @intFromEnum(layout_idx) });
            }
            debugPrint("\n", .{});
        }
        const local_count = self.store.locals.items.len;
        if (local_count > 0) {
            debugPrint("  locals:\n", .{});
            for (self.store.locals.items, 0..) |local, idx| {
                const layout_idx = local.layout_idx;
                const layout_val = self.layout_store.getLayout(layout_idx);
                debugPrint(
                    "    local {d}: layout={d} tag={s}",
                    .{ idx, @intFromEnum(layout_idx), @tagName(layout_val.tag) },
                );
                if (layout_val.tag == .list) {
                    debugPrint(" elem={d}", .{@intFromEnum(layout_val.getIdx())});
                }
                if (layout_val.tag == .tag_union) {
                    const tu_info = self.layout_store.getTagUnionInfo(layout_val);
                    debugPrint(" variants={d}", .{tu_info.variants.len});
                }
                debugPrint("\n", .{});
            }
        }

        var visited = std.AutoHashMap(CFStmtId, void).init(self.evalAllocator());
        defer visited.deinit();
        var stack = std.ArrayListUnmanaged(CFStmtId).empty;
        defer stack.deinit(self.evalAllocator());
        stack.append(self.evalAllocator(), body) catch return;

        while (stack.items.len > 0) {
            const stmt_id = stack.pop().?;
            if (visited.contains(stmt_id)) continue;
            visited.put(stmt_id, {}) catch return;
            const stmt = self.store.getCFStmt(stmt_id);
            switch (stmt) {
                .assign_ref => |assign| {
                    debugPrint("    {d}: assign_ref target={d} op={any} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        assign.op,
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_literal => |assign| {
                    debugPrint("    {d}: assign_literal target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .init_uninitialized => |uninit| {
                    debugPrint("    {d}: init_uninitialized target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(uninit.target),
                        @intFromEnum(uninit.next),
                    });
                    stack.append(self.evalAllocator(), uninit.next) catch return;
                },
                .assign_call => |assign| {
                    debugPrint("    {d}: assign_call proc={d} target={d} args=", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.proc),
                        @intFromEnum(assign.target),
                    });
                    for (self.store.getLocalSpan(assign.args)) |arg_local| {
                        debugPrint("{d} ", .{@intFromEnum(arg_local)});
                    }
                    debugPrint("next={d}\n", .{@intFromEnum(assign.next)});
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_call_erased => |assign| {
                    debugPrint("    {d}: assign_call_erased target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_packed_erased_fn => |assign| {
                    debugPrint("    {d}: assign_packed_erased_fn target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_low_level => |assign| {
                    debugPrint("    {d}: assign_low_level target={d} op={s} args=", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @tagName(assign.op),
                    });
                    for (self.store.getLocalSpan(assign.args)) |arg_local| {
                        debugPrint("{d} ", .{@intFromEnum(arg_local)});
                    }
                    debugPrint("next={d}\n", .{@intFromEnum(assign.next)});
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_list => |assign| {
                    debugPrint("    {d}: assign_list target={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_struct => |assign| {
                    debugPrint("    {d}: assign_struct target={d} fields=", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                    });
                    for (self.store.getLocalSpan(assign.fields)) |field_local| {
                        debugPrint("{d} ", .{@intFromEnum(field_local)});
                    }
                    debugPrint("next={d}\n", .{
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .assign_tag => |assign| {
                    debugPrint("    {d}: assign_tag target={d} variant={d} discrim={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        assign.variant_index,
                        assign.discriminant,
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .set_local => |assign| {
                    debugPrint("    {d}: set_local target={d} value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(assign.target),
                        @intFromEnum(assign.value),
                        @intFromEnum(assign.next),
                    });
                    stack.append(self.evalAllocator(), assign.next) catch return;
                },
                .debug => |debug_stmt| {
                    debugPrint("    {d}: debug next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(debug_stmt.next),
                    });
                    stack.append(self.evalAllocator(), debug_stmt.next) catch return;
                },
                .expect => |expect_stmt| {
                    debugPrint("    {d}: expect cond={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(expect_stmt.condition),
                        @intFromEnum(expect_stmt.next),
                    });
                    stack.append(self.evalAllocator(), expect_stmt.next) catch return;
                },
                .runtime_error => {
                    debugPrint("    {d}: runtime_error\n", .{@intFromEnum(stmt_id)});
                },
                .comptime_exhaustiveness_failed => |failed| {
                    debugPrint("    {d}: comptime_exhaustiveness_failed site={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(failed.site),
                    });
                },
                .comptime_branch_taken => |marker| {
                    debugPrint("    {d}: comptime_branch_taken site={d} branch={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(marker.site),
                        marker.branch_index,
                        @intFromEnum(marker.next),
                    });
                    stack.append(self.evalAllocator(), marker.next) catch return;
                },
                .incref => |inc| {
                    debugPrint("    {d}: incref value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(inc.value),
                        @intFromEnum(inc.next),
                    });
                    stack.append(self.evalAllocator(), inc.next) catch return;
                },
                .decref => |dec| {
                    debugPrint("    {d}: decref value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(dec.value),
                        @intFromEnum(dec.next),
                    });
                    stack.append(self.evalAllocator(), dec.next) catch return;
                },
                .decref_if_initialized => |dec| {
                    debugPrint("    {d}: decref_if_initialized cond={d} mask=0x{x} value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(dec.cond),
                        dec.cond_mask,
                        @intFromEnum(dec.value),
                        @intFromEnum(dec.next),
                    });
                    stack.append(self.evalAllocator(), dec.next) catch return;
                },
                .free => |dec| {
                    debugPrint("    {d}: free value={d} next={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(dec.value),
                        @intFromEnum(dec.next),
                    });
                    stack.append(self.evalAllocator(), dec.next) catch return;
                },
                .switch_stmt => |switch_stmt| {
                    debugPrint("    {d}: switch cond={d} default={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(switch_stmt.cond),
                        @intFromEnum(switch_stmt.default_branch),
                    });
                    stack.append(self.evalAllocator(), switch_stmt.default_branch) catch return;
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    for (branches) |branch| {
                        debugPrint("        branch {d} -> {d}\n", .{
                            branch.value,
                            @intFromEnum(branch.body),
                        });
                        stack.append(self.evalAllocator(), branch.body) catch return;
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    debugPrint("    {d}: switch_initialized_payload cond={d} mask=0x{x} payload={d} initialized={d} uninitialized={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(switch_stmt.cond),
                        switch_stmt.cond_mask,
                        @intFromEnum(switch_stmt.payload),
                        @intFromEnum(switch_stmt.initialized_branch),
                        @intFromEnum(switch_stmt.uninitialized_branch),
                    });
                    stack.append(self.evalAllocator(), switch_stmt.initialized_branch) catch return;
                    stack.append(self.evalAllocator(), switch_stmt.uninitialized_branch) catch return;
                },
                .str_match => |str_match| {
                    debugPrint("    {d}: str_match source={d} on_match={d} on_miss={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(str_match.source),
                        @intFromEnum(str_match.on_match),
                        @intFromEnum(str_match.on_miss),
                    });
                    stack.append(self.evalAllocator(), str_match.on_match) catch return;
                    stack.append(self.evalAllocator(), str_match.on_miss) catch return;
                },
                .str_match_set => |str_match_set| {
                    debugPrint("    {d}: str_match_set source={d} arms={d} on_miss={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(str_match_set.source),
                        str_match_set.arms.len,
                        @intFromEnum(str_match_set.on_miss),
                    });
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        stack.append(self.evalAllocator(), arm.on_match) catch return;
                    }
                    stack.append(self.evalAllocator(), str_match_set.on_miss) catch return;
                },
                .loop_continue => {
                    debugPrint("    {d}: loop_continue\n", .{@intFromEnum(stmt_id)});
                },
                .loop_break => {
                    debugPrint("    {d}: loop_break\n", .{@intFromEnum(stmt_id)});
                },
                .join => |join| {
                    debugPrint("    {d}: join id={d} params=", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(join.id),
                    });
                    for (self.store.getLocalSpan(join.params)) |param_local| {
                        debugPrint("{d} ", .{@intFromEnum(param_local)});
                    }
                    debugPrint("body={d} remainder={d}\n", .{
                        @intFromEnum(join.body),
                        @intFromEnum(join.remainder),
                    });
                    stack.append(self.evalAllocator(), join.body) catch return;
                    stack.append(self.evalAllocator(), join.remainder) catch return;
                },
                .jump => |jump| {
                    debugPrint("    {d}: jump target={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(jump.target),
                    });
                },
                .ret => |ret| {
                    debugPrint("    {d}: ret value={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(ret.value),
                    });
                },
                .crash => |crash| {
                    debugPrint("    {d}: crash msg={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(crash.msg),
                    });
                },
                .expect_err => |expect_err_stmt| {
                    debugPrint("    {d}: expect_err message={d}\n", .{
                        @intFromEnum(stmt_id),
                        @intFromEnum(expect_err_stmt.message),
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

    const ErasedCallResult = struct {
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
                else => {
                    if (builtin.mode == .Debug) {
                        const layout_val_dbg = self.layout_store.getLayout(layout_idx);
                        debugPrint(
                            "LIR/interpreter bad switch layout idx={d} tag={s} size={d}\n",
                            .{ @intFromEnum(layout_idx), @tagName(layout_val_dbg.tag), self.helper.sizeOf(layout_idx) },
                        );
                    }
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: switch condition layout {d} is not a supported scalar width",
                        .{@intFromEnum(layout_idx)},
                    );
                },
            },
        };
    }

    fn materializeLocalValue(
        self: *LirInterpreter,
        value: Value,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const size = self.helper.sizeOf(target_layout);
        if (size == 0) return Value.zst;

        const storage = try self.alloc(target_layout);
        if (!value.isZst()) {
            storage.copyFrom(value, size);
        }
        return storage;
    }

    fn evalAssignRef(
        self: *LirInterpreter,
        frame: *const Frame,
        op: LIR.RefOp,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        return switch (op) {
            .local => |source| blk: {
                const local_value = try self.coerceExplicitRefValueToLayout(
                    try self.getLocalChecked(frame, source),
                    self.store.getLocal(source).layout_idx,
                    target_layout,
                );
                break :blk try self.materializeLocalValue(local_value, target_layout);
            },
            .field => |field| blk: {
                const source_val = try self.getLocalChecked(frame, field.source);
                const source_layout = self.store.getLocal(field.source).layout_idx;
                const struct_base = self.resolveStructBaseValue(source_val, source_layout);
                const struct_layout_val = self.layout_store.getLayout(struct_base.layout);
                const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                    struct_layout_val.getStruct().idx,
                    field.field_idx,
                );
                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                    struct_layout_val.getStruct().idx,
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
                break :blk try self.materializeLocalValue(field_value, target_layout);
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
                const actual_payload_layout = self.tagPayloadLayout(source_layout, payload.variant_index);
                const payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
                switch (payload_layout_val.tag) {
                    .struct_ => {
                        const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                            payload_layout_val.getStruct().idx,
                            payload.payload_idx,
                        );
                        const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                            payload_layout_val.getStruct().idx,
                            payload.payload_idx,
                        );
                        const payload_value = try self.coerceExplicitRefValueToLayout(
                            tag_base.value.offset(field_offset),
                            actual_field_layout,
                            target_layout,
                        );
                        break :blk try self.materializeLocalValue(payload_value, target_layout);
                    },
                    else => {
                        if (builtin.mode == .Debug and payload.payload_idx != 0) {
                            self.invariantFailed(
                                "LIR/interpreter invariant violated: scalar tag payload access requested payload_idx {d} from non-struct payload layout {d}",
                                .{ payload.payload_idx, @intFromEnum(actual_payload_layout) },
                            );
                        }
                        const payload_value = try self.coerceExplicitRefValueToLayout(tag_base.value, actual_payload_layout, target_layout);
                        break :blk try self.materializeLocalValue(payload_value, target_layout);
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
                const actual_payload_layout = self.tagPayloadLayout(source_layout, payload.variant_index);
                const payload_value = try self.coerceExplicitRefValueToLayout(tag_base.value, actual_payload_layout, target_layout);
                break :blk try self.materializeLocalValue(payload_value, target_layout);
            },
            .list_reinterpret => |list_reinterpret| blk: {
                const reinterpreted = try self.coerceExplicitListValueToLayout(
                    try self.getLocalChecked(frame, list_reinterpret.backing_ref),
                    self.store.getLocal(list_reinterpret.backing_ref).layout_idx,
                    target_layout,
                );
                break :blk try self.materializeLocalValue(reinterpreted, target_layout);
            },
            .nominal => |nominal| blk: {
                const reinterpreted = try self.coerceExplicitNominalValueToLayout(
                    try self.getLocalChecked(frame, nominal.backing_ref),
                    self.store.getLocal(nominal.backing_ref).layout_idx,
                    target_layout,
                );
                break :blk try self.materializeLocalValue(reinterpreted, target_layout);
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
                break :blk try self.materializeLocalValue(disc_value, target_layout);
            },
        };
    }

    fn evalLiteral(self: *LirInterpreter, literal: LIR.LiteralValue, target_layout: layout_mod.Idx) Error!Value {
        return switch (literal) {
            .i64_literal => |lit| self.evalI64Literal(lit.value, lit.layout_idx),
            .i128_literal => |lit| self.evalI128Literal(lit.value, lit.layout_idx),
            .f64_literal => |value| self.evalF64Literal(value),
            .f32_literal => |value| self.evalF32Literal(value),
            .dec_literal => |value| self.evalDecLiteral(value),
            .str_literal => |idx| self.evalStrLiteral(idx),
            .bytes_literal => |idx| self.evalBytesLiteral(idx, target_layout),
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

    pub fn erasedCallableInterpreterContextFromCapture(capture_ptr: ?[*]u8) *ErasedCallableInterpreterContext {
        return @ptrCast(@alignCast(capture_ptr orelse unreachable));
    }

    pub fn erasedCallableInterpreterContextFromPayload(data_ptr: [*]u8) *ErasedCallableInterpreterContext {
        return erasedCallableInterpreterContextFromCapture(builtins.erased_callable.capturePtr(data_ptr));
    }

    pub fn erasedCallableInterpreterProcId(data_ptr: [*]u8) LIR.LirProcSpecId {
        const context = erasedCallableInterpreterContextFromPayload(data_ptr);
        return @enumFromInt(context.proc_id);
    }

    pub fn erasedCallableInterpreterCaptureValuePtr(data_ptr: [*]u8) [*]u8 {
        const context = erasedCallableInterpreterContextFromPayload(data_ptr);
        return builtins.erased_callable.capturePtr(data_ptr) + context.capture_value_offset;
    }

    fn argsStructSizeAlign(self: *LirInterpreter, arg_layouts: []const layout_mod.Idx) layout_mod.SizeAlign {
        var size: u32 = 0;
        var max_align: usize = 1;
        for (arg_layouts) |arg_layout| {
            const size_align = self.helper.sizeAlignOf(arg_layout);
            const arg_align: u32 = @intCast(@max(size_align.alignment.toByteUnits(), 1));
            size = std.mem.alignForward(u32, size, arg_align);
            size += size_align.size;
            max_align = @max(max_align, size_align.alignment.toByteUnits());
        }
        return .{ .size = @intCast(size), .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(max_align)) };
    }

    fn argsStructOffset(self: *LirInterpreter, arg_layouts: []const layout_mod.Idx, index: usize) u32 {
        var offset: u32 = 0;
        for (arg_layouts[0..index]) |arg_layout| {
            const size_align = self.helper.sizeAlignOf(arg_layout);
            const arg_align: u32 = @intCast(@max(size_align.alignment.toByteUnits(), 1));
            offset = std.mem.alignForward(u32, offset, arg_align);
            offset += size_align.size;
        }
        const current = self.helper.sizeAlignOf(arg_layouts[index]);
        return std.mem.alignForward(u32, offset, @intCast(@max(current.alignment.toByteUnits(), 1)));
    }

    fn interpreterErasedCallableTrampoline(
        ops: *RocOps,
        ret: ?[*]u8,
        args: ?[*]const u8,
        capture: ?[*]u8,
    ) callconv(.c) void {
        const context = erasedCallableInterpreterContextFromCapture(capture);
        context.interpreter.callInterpreterErasedCallable(context, ops, ret, args) catch |err| switch (err) {
            error.OutOfMemory => ops.crash("LIR/interpreter erased callable trampoline ran out of memory"),
            error.RuntimeError => ops.crash("LIR/interpreter erased callable trampoline hit runtime error"),
            error.ComptimeExhaustiveness => ops.crash("LIR/interpreter erased callable trampoline hit compile-time exhaustiveness marker"),
            error.DivisionByZero => ops.crash("LIR/interpreter erased callable trampoline hit division by zero"),
            error.Crash => ops.crash("LIR/interpreter erased callable trampoline hit Roc crash"),
            // expect_err statements only occur in top-level expect test
            // roots, never in callable bodies.
            error.ExpectErr => unreachable,
        };
    }

    fn interpreterErasedCallableOnDrop(capture: ?[*]u8, _: *RocOps) callconv(.c) void {
        const context = erasedCallableInterpreterContextFromCapture(capture);
        const capture_layout: layout_mod.Idx = if (context.capture_layout_plus_one == 0)
            return
        else
            @enumFromInt(context.capture_layout_plus_one - 1);
        if (capture_layout == .zst) return;
        const capture_value_ptr = (capture orelse unreachable) + context.capture_value_offset;
        context.interpreter.performRawRc(.decref, .{ .ptr = capture_value_ptr }, capture_layout, 1);
    }

    fn callInterpreterErasedCallable(
        self: *LirInterpreter,
        context: *ErasedCallableInterpreterContext,
        _: *RocOps,
        ret: ?[*]u8,
        args: ?[*]const u8,
    ) Error!void {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(context.proc_id);
        const proc_spec = self.store.getProcSpec(proc_id);
        const proc_arg_locals = self.store.getLocalSpan(proc_spec.args);
        if (proc_arg_locals.len == 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: erased callable proc {d} has no hidden capture argument",
                .{@intFromEnum(proc_id)},
            );
        }

        const explicit_arg_count = proc_arg_locals.len - 1;
        var proc_args = try self.arena.allocator().alloc(Value, proc_arg_locals.len);
        var proc_arg_layouts = try self.arena.allocator().alloc(layout_mod.Idx, proc_arg_locals.len);

        for (proc_arg_locals[0..explicit_arg_count], 0..) |local, i| {
            const arg_layout = self.store.getLocal(local).layout_idx;
            proc_arg_layouts[i] = arg_layout;
            const size = self.helper.sizeOf(arg_layout);
            if (size == 0) {
                proc_args[i] = Value.zst;
            } else {
                const raw_args = args orelse {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: erased callable proc {d} expected args payload",
                        .{@intFromEnum(proc_id)},
                    );
                };
                proc_args[i] = .{ .ptr = @constCast(raw_args + self.argsStructOffset(proc_arg_layouts[0..explicit_arg_count], i)) };
            }
        }

        const capture_value_ptr: [*]u8 = @ptrCast(@as([*]u8, @ptrCast(context)) + context.capture_value_offset);
        proc_args[explicit_arg_count] = try self.allocPointerIntValue(@intFromPtr(capture_value_ptr));
        proc_arg_layouts[explicit_arg_count] = .opaque_ptr;

        const result = try self.evalProcById(proc_id, proc_args, proc_arg_layouts);
        const ret_size = self.helper.sizeOf(proc_spec.ret_layout);
        if (ret_size > 0) {
            const ret_ptr = ret orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: erased callable proc {d} returned non-ZST result without result storage",
                    .{@intFromEnum(proc_id)},
                );
            };
            @memcpy(ret_ptr[0..ret_size], result.ptr[0..ret_size]);
        }
    }

    fn evalErasedCall(
        self: *LirInterpreter,
        frame: *Frame,
        closure_local: LocalId,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
        ret_layout: layout_mod.Idx,
    ) Error!ErasedCallResult {
        const closure_layout = self.store.getLocal(closure_local).layout_idx;
        const closure_value = try self.getLocalChecked(frame, closure_local);
        const closure_layout_val = self.layout_store.getLayout(closure_layout);
        if (closure_layout_val.tag != .erased_callable) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: erased call closure local {d} does not have erased_callable layout",
                .{@intFromEnum(closure_local)},
            );
        }

        const closure_ptr = self.readBoxedDataPointer(closure_value) orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: erased call closure local {d} has null payload",
                .{@intFromEnum(closure_local)},
            );
        };

        const payload = builtins.erased_callable.payloadPtr(closure_ptr);
        const arg_size_align = self.argsStructSizeAlign(arg_layouts);
        const arg_bytes = if (args.len == 0)
            null
        else blk: {
            const bytes = try self.arena.allocator().alloc(u8, if (arg_size_align.size == 0) 1 else arg_size_align.size);
            var offset: u32 = 0;
            for (args, arg_layouts) |arg_value, arg_layout| {
                const size_align = self.helper.sizeAlignOf(arg_layout);
                const arg_align: u32 = @intCast(@max(size_align.alignment.toByteUnits(), 1));
                offset = std.mem.alignForward(u32, offset, arg_align);
                if (size_align.size > 0) {
                    @memcpy(bytes[offset..][0..size_align.size], arg_value.ptr[0..size_align.size]);
                }
                offset += size_align.size;
            }
            break :blk bytes;
        };

        if (@intFromPtr(payload.callable_fn_ptr) == @intFromPtr(&interpreterErasedCallableTrampoline)) {
            const proc_id = erasedCallableInterpreterProcId(closure_ptr);
            const proc_ret_layout = self.store.getProcSpec(proc_id).ret_layout;
            if (proc_ret_layout != ret_layout) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: erased callable proc {d} returned layout {d}, call site expected {d}",
                    .{ @intFromEnum(proc_id), @intFromEnum(proc_ret_layout), @intFromEnum(ret_layout) },
                );
            }
        }

        const result = try self.alloc(ret_layout);
        const ret_size = self.helper.sizeOf(ret_layout);
        const ret_ptr: ?[*]u8 = if (ret_size == 0) null else result.ptr;

        payload.callable_fn_ptr(
            &self.roc_ops,
            ret_ptr,
            if (arg_bytes) |bytes| @ptrCast(bytes.ptr) else null,
            builtins.erased_callable.capturePtr(closure_ptr),
        );

        return .{
            .value = if (ret_size == 0) Value.zst else result,
            .layout = ret_layout,
        };
    }

    fn evalPackedErasedFn(
        self: *LirInterpreter,
        frame: *Frame,
        assign: anytype,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const has_capture = assign.capture != null;
        if (has_capture != (assign.capture_layout != null)) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: packed erased fn capture/layout presence differed",
                .{},
            );
        }

        const capture_value_size: usize = if (assign.capture_layout) |capture_layout|
            self.helper.sizeOf(capture_layout)
        else
            0;
        if (assign.capture_layout) |capture_layout| {
            const capture_align = self.helper.sizeAlignOf(capture_layout).alignment.toByteUnits();
            if (capture_align > builtins.erased_callable.capture_alignment) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: erased callable capture layout alignment {d} exceeds fixed capture alignment {d}",
                    .{ capture_align, builtins.erased_callable.capture_alignment },
                );
            }
        }
        const capture_size = erased_callable_context_capture_offset + capture_value_size;
        const data_ptr = try self.allocRocDataWithRc(
            builtins.erased_callable.payloadSize(capture_size),
            builtins.erased_callable.payload_alignment,
            builtins.erased_callable.allocation_has_refcounted_children,
        );

        const on_drop: ?builtins.erased_callable.OnDropFn = switch (assign.on_drop) {
            .none => null,
            .rc_helper => &interpreterErasedCallableOnDrop,
            .interpreter_context_drop => &interpreterErasedCallableOnDrop,
        };
        const payload = builtins.erased_callable.payloadPtr(data_ptr);
        payload.* = .{
            .callable_fn_ptr = &interpreterErasedCallableTrampoline,
            .on_drop = on_drop,
        };

        const context = erasedCallableInterpreterContextFromPayload(data_ptr);
        context.* = .{
            .interpreter = self,
            .proc_id = @intFromEnum(assign.proc),
            .capture_layout_plus_one = if (assign.capture_layout) |layout_idx| @intFromEnum(layout_idx) + 1 else 0,
            .capture_value_offset = @intCast(erased_callable_context_capture_offset),
            .padding = 0,
        };

        if (assign.capture) |capture_local| {
            const capture_layout = assign.capture_layout orelse unreachable;
            const capture_value = try self.getLocalChecked(frame, capture_local);
            const capture_ptr = erasedCallableInterpreterCaptureValuePtr(data_ptr);
            const size = self.helper.sizeOf(capture_layout);
            if (size > 0) {
                @memcpy(capture_ptr[0..size], capture_value.ptr[0..size]);
            }
        }

        const result = try self.alloc(target_layout);
        self.writeBoxedDataPointer(result, data_ptr);
        return result;
    }

    const AllocatedStruct = struct {
        outer: Value,
        base: Value,
        base_layout: layout_mod.Idx,
    };

    const BoxAllocInfo = struct {
        elem_layout: layout_mod.Idx,
        elem_size: u32,
        elem_alignment: u32,
        contains_rc: bool,
    };

    fn boxAllocInfo(self: *LirInterpreter, box_layout: Layout) BoxAllocInfo {
        return switch (box_layout.tag) {
            .box => blk: {
                const elem_layout = box_layout.getIdx();
                const elem_layout_val = self.layout_store.getLayout(elem_layout);
                break :blk .{
                    .elem_layout = elem_layout,
                    .elem_size = self.layout_store.layoutSize(elem_layout_val),
                    .elem_alignment = @intCast(elem_layout_val.alignment(self.layout_store.targetUsize()).toByteUnits()),
                    .contains_rc = self.layoutContainsRc(elem_layout),
                };
            },
            .box_of_zst => .{
                .elem_layout = .zst,
                .elem_size = 0,
                .elem_alignment = 1,
                .contains_rc = false,
            },
            else => self.invariantFailed(
                "LIR/interpreter invariant violated: expected box layout, got {s}",
                .{@tagName(box_layout.tag)},
            ),
        };
    }

    fn allocStructValue(self: *LirInterpreter, struct_layout: layout_mod.Idx) Error!AllocatedStruct {
        const struct_layout_val = self.layout_store.getLayout(struct_layout);
        switch (struct_layout_val.tag) {
            .zst => return .{
                .outer = Value.zst,
                .base = Value.zst,
                .base_layout = .zst,
            },
            .box_of_zst => return .{
                .outer = try self.allocBoxOfZstValue(struct_layout),
                .base = Value.zst,
                .base_layout = .zst,
            },
            .box => {
                const box_info = self.boxAllocInfo(struct_layout_val);
                const data_ptr = try self.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_rc,
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
                    .base_layout = struct_layout_val.getIdx(),
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
        var expected_field_count: usize = 0;
        for (0..expected_info.fields.len) |i| {
            const field = expected_info.fields.get(@intCast(i));
            // Padding spacers carry indices past every named field and are never
            // constructed, so they must not raise the expected named-field count.
            if (field.is_padding) continue;
            expected_field_count = @max(expected_field_count, @as(usize, @intCast(field.index)) + 1);
        }
        if (builtin.mode == .Debug and field_locals.len < expected_field_count) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: struct literal for layout {d} had {d} fields but layout expects {d}",
                .{ @intFromEnum(struct_layout), field_locals.len, expected_field_count },
            );
        }
        for (field_locals, 0..) |field_local, i| {
            const field_size = self.layout_store.getStructFieldSizeByOriginalIndex(
                base_layout_val.getStruct().idx,
                @intCast(i),
            );
            if (field_size == 0) continue;
            const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                base_layout_val.getStruct().idx,
                @intCast(i),
            );
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                base_layout_val.getStruct().idx,
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
            const box_info = self.boxAllocInfo(union_layout_val);
            const data_ptr = try self.allocRocDataWithRc(
                box_info.elem_size,
                box_info.elem_alignment,
                box_info.contains_rc,
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
                .base_layout = union_layout_val.getIdx(),
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
        variant_index: u16,
        discriminant: u16,
        payload_local: ?LocalId,
        union_layout: layout_mod.Idx,
    ) Error!Value {
        const allocated = try self.allocTagValue(union_layout);
        if (self.helper.sizeOf(allocated.base_layout) > 0) {
            self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, discriminant);
        } else if (builtin.mode == .Debug and discriminant != 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: nonzero discriminant {d} for zero-sized tag layout {d}",
                .{ discriminant, @intFromEnum(allocated.base_layout) },
            );
        }

        const payload_layout = self.tagPayloadLayout(union_layout, variant_index);
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
            return self.rocListToValue(canonicalZstList(0), list_layout);
        }
        if (elem_size == 0) {
            return self.rocListToValue(canonicalZstList(elem_locals.len), list_layout);
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
            .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(elem_locals.len),
        }, list_layout);
    }

    fn callHostedProc(
        self: *LirInterpreter,
        proc_id: LirProcSpecId,
        hosted: LIR.HostedProc,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
        ret_layout: layout_mod.Idx,
    ) Error!Value {
        // Pack arguments into a buffer in Roc layout order, recording each argument's offset
        // so the C-ABI trampoline can scatter them into registers.
        var total_args_size: usize = 0;
        var args_alignment: layout_mod.RocAlignment = .@"1";
        const arg_offsets = try self.allocator.alloc(u32, arg_layouts.len);
        defer self.allocator.free(arg_offsets);
        for (arg_layouts, arg_offsets) |arg_layout, *arg_offset| {
            const sa = self.helper.sizeAlignOf(arg_layout);
            args_alignment = maxRocAlignment(args_alignment, sa.alignment);
            total_args_size = std.mem.alignForward(usize, total_args_size, sa.alignment.toByteUnits());
            arg_offset.* = @intCast(total_args_size);
            total_args_size += sa.size;
        }

        const args_buf_size = @max(total_args_size, 8);
        const args_buf = try self.allocAlignedByteSlice(args_buf_size, args_alignment);

        for (args, arg_layouts, arg_offsets) |arg, arg_layout, arg_offset| {
            const sa = self.helper.sizeAlignOf(arg_layout);
            if (sa.size > 0 and !arg.isZst()) {
                @memcpy(args_buf[arg_offset .. arg_offset + sa.size], arg.readBytes(sa.size));
            }
        }

        const ret_sa = self.helper.sizeAlignOf(ret_layout);
        const ret_buf = try self.allocAlignedByteSlice(@max(ret_sa.size, 1), ret_sa.alignment);

        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;

        if (hosted.dispatch_index >= self.roc_ops.hosted_fns.count) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: hosted call index {d} out of bounds for proc {d}",
                .{ hosted.dispatch_index, @intFromEnum(proc_id) },
            );
        }

        const hosted_fn = self.roc_ops.hosted_fns.fns[hosted.dispatch_index];

        if (comptime host_trampoline.available) {
            // Call the hosted function with the platform C ABI via the fixed register-image
            // trampoline (no runtime code generation).
            var arena_state = std.heap.ArenaAllocator.init(self.allocator);
            defer arena_state.deinit();
            host_trampoline.call(
                self.layout_store,
                arena_state.allocator(),
                @ptrCast(hosted_fn),
                arg_layouts,
                ret_layout,
                args_buf.ptr,
                arg_offsets,
                ret_buf.ptr,
            ) catch |err| return self.invariantFailedError(
                "hosted call C-ABI lowering failed for proc {d}: {s}",
                .{ @intFromEnum(proc_id), @errorName(err) },
            );
        } else {
            // Architectures without a register-image trampoline (e.g. wasm32, where
            // a dynamic-signature call cannot be synthesized) call hosted functions
            // through a uniform `(args_buf, ret_buf)` ABI instead. The arguments are
            // already packed contiguously above in Roc layout order, so the host reads
            // each one from `args_buf` at its layout offset and writes the return value
            // into `ret_buf`. Platforms register their hosted functions in this shape
            // when `host_trampoline.available` is false (see the echo platform).
            const uniform: *const fn ([*]u8, [*]u8) callconv(.c) void = @ptrCast(hosted_fn);
            uniform(args_buf.ptr, ret_buf.ptr);
        }

        if (self.roc_env.crashed) return error.Crash;
        if (ret_sa.size == 0) return Value.zst;

        const result = try self.alloc(ret_layout);
        @memcpy(result.ptr[0..ret_sa.size], ret_buf[0..ret_sa.size]);
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
        const size = self.helper.sizeOf(layout_idx);
        const bits: u128 = @bitCast(value);
        switch (size) {
            1 => val.write(u8, @truncate(bits)),
            2 => val.write(u16, @truncate(bits)),
            4 => val.write(u32, @truncate(bits)),
            8 => val.write(u64, @truncate(bits)),
            16 => val.write(i128, value),
            else => return error.RuntimeError,
        }
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

    fn evalStrLiteral(self: *LirInterpreter, literal: LIR.StrLiteral) Error!Value {
        return self.makeStaticRocStrLiteralView(
            self.store.getStringLiteralBacking(literal),
            literal.offset,
            literal.len,
        );
    }

    fn evalBytesLiteral(self: *LirInterpreter, literal: LIR.StrLiteral, target_layout: layout_mod.Idx) Error!Value {
        return self.makeStaticRocListLiteralView(
            self.store.getStringLiteralBacking(literal),
            literal.offset,
            literal.len,
            target_layout,
        );
    }

    // String helpers (RocStr construction)

    fn makeStaticRocStrLiteralView(self: *LirInterpreter, backing: []const u8, offset: u32, len: u32) Error!Value {
        const offset_usize: usize = offset;
        const len_usize: usize = len;
        if (offset_usize > backing.len or len_usize > backing.len - offset_usize) {
            self.invariantFailed("LIR/interpreter invariant violated: string literal view exceeded backing bytes", .{});
        }

        const bytes = backing[offset_usize..][0..len_usize];
        const whole_backing = offset_usize == 0 and len_usize == backing.len;
        if (backing.len < @sizeOf(RocStr) and RocStr.fitsInSmallStr(bytes.len)) {
            const small = RocStr.fromSliceSmall(bytes);
            return self.rocStrToValue(small, .str);
        }

        if (builtin.mode == .Debug) {
            const data_addr = @intFromPtr(backing.ptr);
            if (data_addr % @alignOf(isize) != 0) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: static string literal backing is not refcount-aligned",
                    .{},
                );
            }
            const refcount_ptr: *const isize = @ptrCast(@alignCast(backing.ptr - @sizeOf(isize)));
            if (refcount_ptr.* != builtins.utils.REFCOUNT_STATIC_DATA) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: static string literal missing static refcount",
                    .{},
                );
            }
        }

        const rs = RocStr{
            .bytes = @ptrCast(@constCast(bytes.ptr)),
            .capacity_or_alloc_ptr = if (whole_backing)
                RocStr.encodeCapacity(bytes.len)
            else
                RocStr.encodeSliceAllocationPtr(@ptrCast(@constCast(backing.ptr))),
            .length = bytes.len,
        };
        return self.rocStrToValue(rs, .str);
    }

    fn makeStaticRocListLiteralView(self: *LirInterpreter, backing: []const u8, offset: u32, len: u32, target_layout: layout_mod.Idx) Error!Value {
        const offset_usize: usize = offset;
        const len_usize: usize = len;
        if (offset_usize > backing.len or len_usize > backing.len - offset_usize) {
            self.invariantFailed("LIR/interpreter invariant violated: byte-list literal view exceeded backing bytes", .{});
        }

        if (len_usize == 0) {
            return self.rocListToValue(RocList.empty(), target_layout);
        }

        if (builtin.mode == .Debug) {
            const data_addr = @intFromPtr(backing.ptr);
            if (data_addr % @alignOf(isize) != 0) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: static byte-list literal backing is not refcount-aligned",
                    .{},
                );
            }
            const refcount_ptr: *const isize = @ptrCast(@alignCast(backing.ptr - @sizeOf(isize)));
            if (refcount_ptr.* != builtins.utils.REFCOUNT_STATIC_DATA) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: static byte-list literal missing static refcount",
                    .{},
                );
            }
        }

        const bytes = backing[offset_usize..][0..len_usize];
        const whole_backing = offset_usize == 0 and len_usize == backing.len;
        const rl = RocList{
            .bytes = @ptrCast(@constCast(bytes.ptr)),
            .length = bytes.len,
            .capacity_or_alloc_ptr = if (whole_backing)
                RocList.encodeCapacity(bytes.len)
            else
                RocList.encodeSliceAllocationPtr(@ptrCast(@constCast(backing.ptr))),
        };
        return self.rocListToValue(rl, target_layout);
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

    fn execStrMatch(
        self: *LirInterpreter,
        frame: *Frame,
        stmt_id: CFStmtId,
        str_match: anytype,
    ) Error!CFStmtId {
        const source_value = try self.getLocalChecked(frame, str_match.source);
        const source_rs = valueToRocStr(source_value);
        const source_bytes = self.readRocStr(source_value);
        return if (try self.execStrMatchArm(frame, stmt_id, source_rs, source_bytes, str_match))
            str_match.on_match
        else
            str_match.on_miss;
    }

    fn execStrMatchSet(
        self: *LirInterpreter,
        frame: *Frame,
        stmt_id: CFStmtId,
        str_match_set: anytype,
    ) Error!CFStmtId {
        const source_value = try self.getLocalChecked(frame, str_match_set.source);
        const source_rs = valueToRocStr(source_value);
        const source_bytes = self.readRocStr(source_value);
        for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
            if (try self.execStrMatchArm(frame, stmt_id, source_rs, source_bytes, arm)) {
                return arm.on_match;
            }
        }
        return str_match_set.on_miss;
    }

    fn execStrMatchArm(
        self: *LirInterpreter,
        frame: *Frame,
        stmt_id: CFStmtId,
        source_rs: RocStr,
        source_bytes: []const u8,
        arm: anytype,
    ) Error!bool {
        const prefix = self.store.getStringLiteral(arm.prefix);
        if (!LIR.strMatchPrefixMatches(source_bytes, prefix)) return false;

        var cursor: usize = prefix.len;
        const steps = self.store.getStrMatchSteps(arm.steps);
        for (steps, 0..) |step, step_i| {
            const delimiter = self.store.getStringLiteral(step.delimiter);
            const is_final_tail_capture = arm.end == .tail and step_i + 1 == steps.len and delimiter.len == 0;
            const result = LIR.strMatchStep(source_bytes, cursor, delimiter, is_final_tail_capture) orelse return false;
            cursor = result.next_cursor;

            switch (step.capture) {
                .discard => {},
                .view => |local| {
                    self.setLocalChecked(
                        frame,
                        stmt_id,
                        local,
                        try self.makeStrCaptureValue(source_rs, source_bytes, result.capture_start, result.capture_end),
                    );
                },
            }
        }

        return LIR.strMatchEndMatches(source_bytes.len, cursor, arm.end);
    }

    fn makeStrCaptureValue(
        self: *LirInterpreter,
        source_rs: RocStr,
        source_bytes: []const u8,
        start: usize,
        end: usize,
    ) Error!Value {
        if (start > end or end > source_bytes.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: str_match capture range [{d}, {d}) outside source length {d}",
                .{ start, end, source_bytes.len },
            );
        }

        if (source_rs.isSmallStr()) {
            return self.rocStrToValue(RocStr.fromSliceSmall(source_bytes[start..end]), .str);
        }

        const source_ptr = source_rs.bytes orelse self.invariantFailed(
            "LIR/interpreter invariant violated: non-small str_match source had null bytes",
            .{},
        );
        const alloc_ptr = if (source_rs.isSeamlessSlice())
            source_rs.capacity_or_alloc_ptr
        else
            RocStr.encodeSliceAllocationPtr(source_ptr);
        return self.rocStrToValue(.{
            .bytes = source_ptr + start,
            .capacity_or_alloc_ptr = alloc_ptr,
            .length = end - start,
        }, .str);
    }

    // Function calls — all go through the stack-safe engine via enterFunction/evalProcStackSafe.

    // Reference counting

    const RcOp = layout_mod.RcOp;
    const RcAtomicity = builtins.utils.RcAtomicity;

    fn runtimeRcAtomicity(atomicity: LIR.RcAtomicity) RcAtomicity {
        return switch (atomicity) {
            .atomic => .atomic,
            .single_thread => .single_thread,
        };
    }

    fn performRawRc(self: *LirInterpreter, op: RcOp, val: Value, layout_idx: layout_mod.Idx, count: u16) void {
        trace.log("performRawRc: op={s} layout={any} val.ptr={*} count={d}", .{ @tagName(op), layout_idx, val.ptr, count });
        const helper = self.rcHelperForLayout(op, layout_idx);
        self.performRcHelperIfNeeded(helper, val, count, .atomic);
    }

    fn performExplicitRcStmt(
        self: *LirInterpreter,
        helper: layout_mod.RcHelper,
        val: Value,
        count: u16,
        atomicity: LIR.RcAtomicity,
    ) void {
        self.performRcHelperRequired(helper, val, count, runtimeRcAtomicity(atomicity));
    }

    fn performBuiltinInternalRc(
        self: *LirInterpreter,
        comptime _: []const u8,
        op: RcOp,
        val: Value,
        layout_idx: layout_mod.Idx,
        count: u16,
    ) void {
        self.performRawRc(op, val, layout_idx, count);
    }

    fn performInterpreterApiRc(self: *LirInterpreter, op: RcOp, val: Value, layout_idx: layout_mod.Idx, count: u16) void {
        self.performRawRc(op, val, layout_idx, count);
    }

    fn builtinInternalContainsRefcounted(self: *LirInterpreter, comptime _: []const u8, layout_idx: layout_mod.Idx) bool {
        return self.layoutContainsRc(layout_idx);
    }

    fn cachedRcPlan(self: *LirInterpreter, helper: layout_mod.RcHelperKey) layout_mod.RcHelperPlan {
        const id = helper.encode();
        if (self.rc_plans.get(id)) |plan| return plan;

        const plan = self.buildRcPlan(helper);
        self.rc_plans.putAssumeCapacity(id, plan);
        return plan;
    }

    fn buildRcPlan(self: *LirInterpreter, helper: layout_mod.RcHelperKey) layout_mod.RcHelperPlan {
        if (!self.layoutContainsRc(helper.layout_idx)) return .noop;

        const l = self.layout_store.getLayout(helper.layout_idx);
        return switch (l.tag) {
            // ptr is never refcounted, so the early return above already handled it.
            .zst, .ptr => .noop,
            .scalar => if (l.getScalar().tag == .str)
                switch (helper.op) {
                    .incref => .str_incref,
                    .decref => .str_decref,
                    .free => .str_free,
                }
            else
                .noop,
            .list, .list_of_zst => switch (helper.op) {
                .incref => .{ .list_incref = self.rcListPlan(helper.layout_idx) },
                .decref => .{ .list_decref = self.rcListPlan(helper.layout_idx) },
                .free => .{ .list_free = self.rcListPlan(helper.layout_idx) },
            },
            .box, .box_of_zst => switch (helper.op) {
                .incref => .box_incref,
                .decref => .{ .box_decref = self.rcBoxPlan(helper.layout_idx) },
                .free => .{ .box_free = self.rcBoxPlan(helper.layout_idx) },
            },
            .erased_callable => switch (helper.op) {
                .incref => .erased_callable_incref,
                .decref => .erased_callable_decref,
                .free => .erased_callable_free,
            },
            .struct_ => .{ .struct_ = .{
                .struct_idx = l.getStruct().idx,
                .child_op = nestedDropOp(helper.op),
            } },
            .tag_union => .{ .tag_union = .{
                .tag_union_idx = l.getTagUnion().idx,
                .child_op = nestedDropOp(helper.op),
            } },
            .closure => .{ .closure = .{
                .op = nestedDropOp(helper.op),
                .layout_idx = l.getClosure().captures_layout_idx,
            } },
        };
    }

    fn nestedDropOp(op: RcOp) RcOp {
        return switch (op) {
            .incref => .incref,
            .decref, .free => .decref,
        };
    }

    fn rcHelperForLayout(self: *LirInterpreter, op: RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .closure => self.rcHelperForLayout(nestedDropOp(op), layout_val.getClosure().captures_layout_idx),
            else => .{ .op = op, .layout_idx = layout_idx },
        };
    }

    fn performRcHelperIfNeeded(self: *LirInterpreter, helper: layout_mod.RcHelper, val: Value, count: u16, atomicity: RcAtomicity) void {
        const plan = self.cachedRcPlan(helper);
        if (plan == .noop) return;
        self.performRawRcPlan(plan, val, count, atomicity);
    }

    fn performRcHelperRequired(self: *LirInterpreter, helper: layout_mod.RcHelper, val: Value, count: u16, atomicity: RcAtomicity) void {
        const plan = self.cachedRcPlan(helper);
        if (plan == .noop) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: explicit RC statement used noop helper for layout {d}",
                .{@intFromEnum(helper.layout_idx)},
            );
        }
        self.performRawRcPlan(plan, val, count, atomicity);
    }

    fn cachedStructFieldPlan(
        self: *LirInterpreter,
        struct_plan: layout_mod.RcStructPlan,
        field_index: u32,
    ) ?layout_mod.RcFieldPlan {
        const id = helperChildPlanId(@intCast(struct_plan.struct_idx.int_idx), struct_plan.child_op, field_index);
        if (self.struct_field_plans.get(id)) |plan| return plan;

        const field_layout_idx = self.layout_store.getStructFieldLayout(struct_plan.struct_idx, field_index);
        const plan: ?layout_mod.RcFieldPlan = if (self.layout_store.getStructFieldIsPadding(struct_plan.struct_idx, field_index) or
            !self.layoutContainsRc(field_layout_idx) or
            self.layout_store.getStructFieldSize(struct_plan.struct_idx, field_index) == 0)
            null
        else
            .{
                .offset = self.layout_store.getStructFieldOffset(struct_plan.struct_idx, field_index),
                .child = .{
                    .op = struct_plan.child_op,
                    .layout_idx = field_layout_idx,
                },
            };
        self.struct_field_plans.putAssumeCapacity(id, plan);
        return plan;
    }

    fn cachedTagVariantPlan(
        self: *LirInterpreter,
        tag_plan: layout_mod.RcTagUnionPlan,
        variant_index: u32,
    ) ?layout_mod.RcHelperKey {
        const id = helperChildPlanId(@intCast(tag_plan.tag_union_idx.int_idx), tag_plan.child_op, variant_index);
        if (self.tag_variant_plans.get(id)) |plan| return plan;

        const tu_data = self.layout_store.getTagUnionData(tag_plan.tag_union_idx);
        const variants = self.layout_store.getTagUnionVariants(tu_data);
        const payload_layout_idx = variants.get(variant_index).payload_layout;
        const plan: ?layout_mod.RcHelperKey = if (!self.layoutContainsRc(payload_layout_idx) or
            self.layout_store.layoutSizeAlign(self.layout_store.getLayout(payload_layout_idx)).size == 0)
            null
        else
            .{
                .op = tag_plan.child_op,
                .layout_idx = payload_layout_idx,
            };
        self.tag_variant_plans.putAssumeCapacity(id, plan);
        return plan;
    }

    fn helperChildPlanId(parent_idx: u32, child_op: layout_mod.RcOp, child_index: u32) u64 {
        return (@as(u64, parent_idx) << 34) |
            (@as(u64, @intFromEnum(child_op)) << 32) |
            @as(u64, child_index);
    }

    fn rcListPlan(self: *LirInterpreter, list_layout_idx: layout_mod.Idx) layout_mod.RcListPlan {
        const list_layout = self.layout_store.getLayout(list_layout_idx);
        const runtime_elem_layout_idx: ?layout_mod.Idx = switch (list_layout.tag) {
            .list => self.layout_store.runtimeRepresentationLayoutIdx(list_layout.getIdx()),
            .list_of_zst => null,
            else => unreachable,
        };
        if (runtime_elem_layout_idx) |elem_idx| {
            const elem_layout = self.layout_store.getLayout(elem_idx);
            return .{
                .elem_alignment = @intCast(elem_layout.alignment(self.layout_store.targetUsize()).toByteUnits()),
                .elem_width = self.layout_store.layoutSize(elem_layout),
                .child = if (self.layoutContainsRc(elem_idx))
                    .{
                        .op = .decref,
                        .layout_idx = elem_idx,
                    }
                else
                    null,
            };
        }

        return .{
            .elem_alignment = 1,
            .elem_width = 0,
            .child = null,
        };
    }

    fn rcBoxPlan(self: *LirInterpreter, box_layout_idx: layout_mod.Idx) layout_mod.RcBoxPlan {
        const box_layout = self.layout_store.getLayout(box_layout_idx);
        const runtime_elem_layout_idx: ?layout_mod.Idx = switch (box_layout.tag) {
            .box => self.layout_store.runtimeRepresentationLayoutIdx(box_layout.getIdx()),
            .box_of_zst => null,
            else => unreachable,
        };
        if (runtime_elem_layout_idx) |elem_idx| {
            const elem_layout = self.layout_store.getLayout(elem_idx);
            return .{
                .elem_alignment = @intCast(elem_layout.alignment(self.layout_store.targetUsize()).toByteUnits()),
                .child = if (self.layoutContainsRc(elem_idx))
                    .{
                        .op = .decref,
                        .layout_idx = elem_idx,
                    }
                else
                    null,
            };
        }

        return .{
            .elem_alignment = 1,
            .child = null,
        };
    }

    fn layoutContainsRc(self: *LirInterpreter, layout_idx: layout_mod.Idx) bool {
        const raw = @intFromEnum(layout_idx);
        switch (self.rc_presence[raw]) {
            .yes => return true,
            .no => return false,
            .active => return true,
            .unknown => {},
        }

        const layout_val = self.layout_store.getLayout(layout_idx);
        const direct = switch (layout_val.tag) {
            .scalar => layout_val.getScalar().tag == .str,
            .list, .list_of_zst, .box, .box_of_zst, .erased_callable => true,
            .zst, .ptr => false,
            .struct_, .tag_union, .closure => null,
        };
        if (direct) |result| {
            self.rc_presence[raw] = if (result) .yes else .no;
            return result;
        }

        self.rc_presence[raw] = .active;
        const contains = switch (layout_val.tag) {
            .struct_ => blk: {
                const sd = self.layout_store.getStructData(layout_val.getStruct().idx);
                for (0..sd.fields.count) |i| {
                    // Padding spacers hold uninitialized bytes, never a refcounted value.
                    if (self.layout_store.getStructFieldIsPadding(layout_val.getStruct().idx, @intCast(i))) continue;
                    const field_layout = self.layout_store.getStructFieldLayout(layout_val.getStruct().idx, @intCast(i));
                    if (self.layoutContainsRc(field_layout)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => blk: {
                const tu_data = self.layout_store.getTagUnionData(layout_val.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                for (0..variants.len) |i| {
                    if (self.layoutContainsRc(variants.get(@intCast(i)).payload_layout)) break :blk true;
                }
                break :blk false;
            },
            .closure => self.layoutContainsRc(layout_val.getClosure().captures_layout_idx),
            .scalar, .list, .list_of_zst, .box, .box_of_zst, .erased_callable, .zst, .ptr => unreachable,
        };
        self.rc_presence[raw] = if (contains) .yes else .no;
        return contains;
    }

    fn performRawRcPlan(self: *LirInterpreter, rc_plan: layout_mod.RcHelperPlan, val: Value, count: u16, atomicity: RcAtomicity) void {
        trace.log("performRawRcPlan: plan={s} val.ptr={*}", .{ @tagName(rc_plan), val.ptr });
        const utils = builtins.utils;
        switch (rc_plan) {
            .noop => {},
            .str_incref => {
                const rs = valueToRocStr(val);
                trace_rc.log("str_incref: bytes=0x{x} len={d} cap={d} count={d}", .{ @intFromPtr(rs.bytes), rs.length, rs.capacity_or_alloc_ptr, count });
                rs.increfWithAtomicity(count, atomicity, &self.roc_ops);
            },
            .str_decref => {
                const rs = valueToRocStr(val);
                trace_rc.log("str_decref: bytes=0x{x} len={d} cap={d}", .{ @intFromPtr(rs.bytes), rs.length, rs.capacity_or_alloc_ptr });
                rs.decrefWithAtomicity(atomicity, &self.roc_ops);
            },
            .str_free => {
                const rs = valueToRocStr(val);
                trace_rc.log("str_free: bytes=0x{x} len={d} cap={d}", .{ @intFromPtr(rs.bytes), rs.length, rs.capacity_or_alloc_ptr });
                rs.decrefWithAtomicity(atomicity, &self.roc_ops);
            },
            .list_incref => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                trace_rc.log("list_incref: bytes=0x{x} len={d} cap={d} count={d} has_child={any}", .{
                    @intFromPtr(rl.bytes),
                    rl.len(),
                    rl.capacity_or_alloc_ptr,
                    count,
                    has_child,
                });
                rl.increfWithAtomicity(@intCast(count), has_child, atomicity, &self.roc_ops);
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
                        self.decrefListElements(rl, list_plan, child_key, count, atomicity);
                    }
                }
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    atomicity,
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
                        self.decrefListElements(rl, list_plan, child_key, count, atomicity);
                    }
                }
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    atomicity,
                    &self.roc_ops,
                );
            },
            .box_incref => {
                const alloc_ptr = val.read(?[*]u8);
                utils.increfDataPtr(alloc_ptr, @intCast(count), atomicity, &self.roc_ops);
            },
            .box_decref => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, &self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.decrefDataPtr(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, atomicity, &self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRawRcPlan(self.cachedRcPlan(child_key), child_val, count, atomicity);
                    }
                }
                utils.decrefDataPtr(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, atomicity, &self.roc_ops);
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
                        self.performRawRcPlan(self.cachedRcPlan(child_key), child_val, count, atomicity);
                    }
                }
                utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, &self.roc_ops);
            },
            .erased_callable_incref => {
                const alloc_ptr = val.read(?[*]u8);
                builtins.utils.increfDataPtr(alloc_ptr, @intCast(count), atomicity, &self.roc_ops);
            },
            .erased_callable_decref => {
                const alloc_ptr = val.read(?[*]u8);
                self.performErasedCallableFinalDropIfUnique(alloc_ptr, .decref, count);
                builtins.utils.decrefDataPtr(
                    alloc_ptr,
                    builtins.erased_callable.payload_alignment,
                    builtins.erased_callable.allocation_has_refcounted_children,
                    atomicity,
                    &self.roc_ops,
                );
            },
            .erased_callable_free => {
                const alloc_ptr = val.read(?[*]u8);
                self.performErasedCallableFinalDrop(alloc_ptr, .free, count);
                builtins.utils.freeDataPtrC(
                    alloc_ptr,
                    builtins.erased_callable.payload_alignment,
                    builtins.erased_callable.allocation_has_refcounted_children,
                    &self.roc_ops,
                );
            },
            .struct_ => |struct_plan| {
                const field_count = self.layout_store.rcHelperStructFieldCount(struct_plan);
                var i: u32 = 0;
                while (i < field_count) : (i += 1) {
                    const field_plan = self.cachedStructFieldPlan(struct_plan, i) orelse continue;
                    const field_val = Value{ .ptr = val.ptr + field_plan.offset };
                    self.performRawRcPlan(self.cachedRcPlan(field_plan.child), field_val, count, atomicity);
                }
            },
            .tag_union => |tag_plan| {
                const variant_count = self.layout_store.rcHelperTagUnionVariantCount(tag_plan);
                if (variant_count == 0) return;

                const disc: u32 = blk: {
                    const tu_data = self.layout_store.getTagUnionData(tag_plan.tag_union_idx);
                    const disc_offset = tu_data.discriminant_offset.get(self.layout_store.targetUsize());
                    break :blk switch (tu_data.discriminant_size) {
                        0 => 0,
                        1 => val.offset(disc_offset).read(u8),
                        2 => val.offset(disc_offset).read(u16),
                        else => return,
                    };
                };
                trace_rc.log("tag_union rc: disc={d} variant_count={d}", .{ disc, variant_count });

                if (disc < variant_count) {
                    if (self.cachedTagVariantPlan(tag_plan, disc)) |child_key| {
                        // Payload is always at offset 0 in the tag union.
                        self.performRawRcPlan(self.cachedRcPlan(child_key), val, count, atomicity);
                    }
                }
            },
            .closure => |child_key| {
                self.performRawRcPlan(self.cachedRcPlan(child_key), val, count, atomicity);
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
        count: u16,
        atomicity: RcAtomicity,
    ) void {
        if (rl.getAllocationDataPtr(&self.roc_ops)) |source| {
            const elem_count = rl.getAllocationElementCount(true, &self.roc_ops);
            const child_plan = self.cachedRcPlan(child_key);
            var i: usize = 0;
            while (i < elem_count) : (i += 1) {
                const element_ptr = source + i * list_plan.elem_width;
                const element_val = Value{ .ptr = element_ptr };
                self.performRawRcPlan(child_plan, element_val, count, atomicity);
            }
        }
    }

    fn performErasedCallableFinalDropIfUnique(
        self: *LirInterpreter,
        data_ptr: ?[*]u8,
        op: layout_mod.RcOp,
        count: u16,
    ) void {
        if (data_ptr == null) return;
        if (!builtins.utils.isUnique(data_ptr, &self.roc_ops)) return;
        self.performErasedCallableFinalDrop(data_ptr, op, count);
    }

    /// Runs the erased callable's capture cleanup. The `on_drop` slot is
    /// filled at closure creation, which is not an RC statement and makes no
    /// thread-confinement claim, so capture refcount updates behind it always
    /// run atomically, even when the callable's own RC statement is
    /// single-thread (atomic is always sound).
    fn performErasedCallableFinalDrop(
        self: *LirInterpreter,
        data_ptr: ?[*]u8,
        _: layout_mod.RcOp,
        _: u16,
    ) void {
        const ptr = data_ptr orelse return;
        const payload = builtins.erased_callable.payloadPtr(ptr);
        if (payload.on_drop) |on_drop| {
            on_drop(builtins.erased_callable.capturePtr(ptr), &self.roc_ops);
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
                const box_info = self.boxAllocInfo(ret_layout_val);
                const data_ptr = try self.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_rc,
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
            .box_of_zst => return try self.allocBoxOfZstValue(ret_layout),
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
            const elem_idx = l.getIdx();
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
        if (l.tag == .list) return l.getIdx();
        return .zst;
    }

    fn canonicalZstList(len: usize) RocList {
        return .{
            .bytes = null,
            .length = len,
            .capacity_or_alloc_ptr = 0,
        };
    }

    fn zstSublistLen(size: usize, start_u64: u64, len_u64: u64) usize {
        if (size == 0 or len_u64 == 0 or start_u64 >= @as(u64, @intCast(size))) return 0;

        const start: usize = @intCast(start_u64);
        const size_minus_start = size - start;
        return @as(usize, @intCast(@min(len_u64, @as(u64, @intCast(size_minus_start)))));
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

    /// Call a unary string builtin whose first argument carries the op's
    /// runtime uniqueness check; `.InPlace` skips it.
    fn callBuiltinStr1(self: *LirInterpreter, comptime func: anytype, a: RocStr, update_mode: UpdateMode, ret_layout: layout_mod.Idx) Error!Value {
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = func(a, update_mode, &self.roc_ops);
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

    /// Call a binary string builtin whose first argument carries the op's
    /// runtime uniqueness check; `.InPlace` skips it.
    fn callBuiltinStr2Mode(self: *LirInterpreter, comptime func: anytype, a: RocStr, b: RocStr, update_mode: UpdateMode, ret_layout: layout_mod.Idx) Error!Value {
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = func(a, b, update_mode, &self.roc_ops);
        return self.rocStrToValue(result, ret_layout);
    }

    fn unwrapSingleFieldPayloadLayout(self: *LirInterpreter, layout_idx: layout_mod.Idx) ?layout_mod.Idx {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
        const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return field.layout;
    }

    /// Locate the BadUtf8 variant inside an err tag union that may have been extended via `?`
    /// to include additional error tags. The Str.from_utf8 layout contract represents BadUtf8
    /// as a record whose original-index 0 field is the U64 byte index and whose original-index
    /// 1 field is the one-byte UTF-8 problem tag.
    fn findBadUtf8Variant(
        self: *LirInterpreter,
        inner_tu: *const layout_mod.TagUnionData,
    ) ?struct { disc: u16, struct_idx: layout_mod.StructIdx } {
        const inner_v = self.layout_store.getTagUnionVariants(inner_tu);
        for (0..inner_v.len) |i| {
            const inner_payload = inner_v.get(@intCast(i)).payload_layout;
            const unwrapped = self.unwrapSingleFieldPayloadLayout(inner_payload) orelse inner_payload;
            const inner_layout = self.layout_store.getLayout(unwrapped);
            if (inner_layout.tag != .struct_) continue;

            const struct_idx = inner_layout.getStruct().idx;
            const struct_data = self.layout_store.getStructData(struct_idx);
            const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
            if (fields.len != 2) continue;

            var has_index_field = false;
            var has_problem_field = false;
            for (0..fields.len) |fi| {
                const field = fields.get(fi);
                const field_layout = self.layout_store.getLayout(field.layout);
                const field_size = self.layout_store.layoutSize(field_layout);
                switch (field.index) {
                    0 => has_index_field = field_size == 8,
                    1 => has_problem_field = field_size == 1,
                    else => {},
                }
            }
            if (has_index_field and has_problem_field) {
                return .{ .disc = @intCast(i), .struct_idx = struct_idx };
            }
        }
        return null;
    }

    const LowLevelEvalInput = struct {
        op: LIR.LowLevel,
        args: []const Value,
        arg_layouts: []const layout_mod.Idx,
        ret_layout: layout_mod.Idx,
        callable_proc: ?LirProcSpecId = null,
        /// The statement's statically-proven-unique argument mask; bit i set
        /// means argument i's runtime uniqueness check is redundant and the
        /// in-place path may be taken unconditionally.
        unique_args: u64 = 0,
        /// For `list_map_can_reuse`: per-width interchangeability of the input
        /// and output element layouts. On a width whose bit is false the
        /// in-place branch is statically dead, so the op yields 0 without the
        /// runtime uniqueness check. Ignored by every other op.
        interchangeable: layout_mod.WidthValues(bool) = layout_mod.WidthValues(bool).both(true, true),
    };

    fn lowLevelArgLayout(self: *const LirInterpreter, ll: LowLevelEvalInput, index: usize) Error!layout_mod.Idx {
        if (index < ll.arg_layouts.len) return ll.arg_layouts[index];

        return self.invariantFailedError(
            "LIR/interpreter invariant violated: low-level op {s} missing arg layout {d}",
            .{ @tagName(ll.op), index },
        );
    }

    fn writeHasherValue(self: *LirInterpreter, ret_layout: layout_mod.Idx, seed: u64) Error!Value {
        const val = try self.alloc(ret_layout);
        val.write(u64, seed);
        return val;
    }

    fn hasherDomain(op: LIR.LowLevel) builtins.hash.HasherDomain {
        return switch (op) {
            .hasher_write_bool => .bool,
            .hasher_write_u8 => .u8,
            .hasher_write_u16 => .u16,
            .hasher_write_u32 => .u32,
            .hasher_write_u64 => .u64,
            .hasher_write_u128 => .u128,
            .hasher_write_i8 => .i8,
            .hasher_write_i16 => .i16,
            .hasher_write_i32 => .i32,
            .hasher_write_i64 => .i64,
            .hasher_write_i128 => .i128,
            .hasher_write_f32 => .f32,
            .hasher_write_f64 => .f64,
            .hasher_write_dec => .dec,
            .hasher_write_bytes => .bytes,
            .hasher_write_str => .str,
            else => unreachable,
        };
    }

    fn hasherU64Width(op: LIR.LowLevel) u8 {
        return switch (op) {
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_i8,
            => 1,
            .hasher_write_u16,
            .hasher_write_i16,
            => 2,
            .hasher_write_u32,
            .hasher_write_i32,
            .hasher_write_f32,
            => 4,
            .hasher_write_u64,
            .hasher_write_i64,
            .hasher_write_f64,
            => 8,
            else => unreachable,
        };
    }

    fn byteListSlice(self: *LirInterpreter, list_val: Value, list_layout: layout_mod.Idx) Error![]const u8 {
        const list = self.valueToRocListForLayout(list_val, list_layout);
        if (list.bytes) |bytes| return bytes[0..list.len()];
        if (list.len() == 0) return &.{};

        return self.invariantFailedError(
            "LIR/interpreter invariant violated: non-empty byte list had null bytes",
            .{},
        );
    }

    /// Select the update mode for a builtin whose first argument carries the
    /// op's runtime uniqueness check: `.InPlace` when ARC emission proved the
    /// check redundant, `.Immutable` (checked) otherwise.
    fn updateModeForArg0(unique_args: u64) UpdateMode {
        return if ((unique_args & 1) != 0) .InPlace else .Immutable;
    }

    /// Like `updateModeForArg0`, for the op's second checked argument.
    fn updateModeForArg1(unique_args: u64) UpdateMode {
        return if ((unique_args & 2) != 0) .InPlace else .Immutable;
    }

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
            .str_is_eq_static_small => blk: {
                const result = builtins.str.strEqualStaticSmall(
                    valueToRocStr(args[0]),
                    args[1].read(u64),
                    args[2].read(u64),
                    args[3].read(u64),
                    args[4].read(u64),
                );
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_static_small_word_eq => blk: {
                const result = builtins.str.strStaticSmallWordEq(
                    valueToRocStr(args[0]),
                    args[1].read(u64),
                    args[2].read(u64),
                    args[3].read(u64),
                );
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_static_small_word_caseless_eq => blk: {
                const result = builtins.str.strStaticSmallWordCaselessEq(
                    valueToRocStr(args[0]),
                    args[1].read(u64),
                    args[2].read(u64),
                    args[3].read(u64),
                );
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_concat => self.callBuiltinStr2Mode(builtins.str.strConcatC, valueToRocStr(args[0]), valueToRocStr(args[1]), updateModeForArg0(ll.unique_args), ll.ret_layout),
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
            .str_trim => self.callBuiltinStr1(builtins.str.strTrim, valueToRocStr(args[0]), updateModeForArg0(ll.unique_args), ll.ret_layout),
            .str_trim_start => self.callBuiltinStr1(builtins.str.strTrimStart, valueToRocStr(args[0]), updateModeForArg0(ll.unique_args), ll.ret_layout),
            .str_trim_end => self.callBuiltinStr1(builtins.str.strTrimEnd, valueToRocStr(args[0]), updateModeForArg0(ll.unique_args), ll.ret_layout),
            .str_with_ascii_lowercased => self.callBuiltinStr1(builtins.str.strWithAsciiLowercased, valueToRocStr(args[0]), updateModeForArg0(ll.unique_args), ll.ret_layout),
            .str_with_ascii_uppercased => self.callBuiltinStr1(builtins.str.strWithAsciiUppercased, valueToRocStr(args[0]), updateModeForArg0(ll.unique_args), ll.ret_layout),
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
            .str_find_first => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.findFirst(valueToRocStr(args[0]), valueToRocStr(args[1]), &self.roc_ops);

                const layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (layout_val.tag != .struct_) {
                    return self.runtimeError("str_find_first expected a record return layout");
                }
                const record_idx = layout_val.getStruct().idx;
                const fields = self.layout_store.struct_fields.sliceRange(self.layout_store.getStructData(record_idx).getFields());
                if (fields.len != 3 or
                    self.layout_store.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str or
                    self.layout_store.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .str or
                    self.layout_store.getStructFieldLayoutByOriginalIndex(record_idx, 2) != .bool)
                {
                    return self.runtimeError("str_find_first expected fields after Str, before Str, found Bool");
                }

                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 0)).ptr[0..@sizeOf(RocStr)], std.mem.asBytes(&result.after));
                @memcpy(val.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 1)).ptr[0..@sizeOf(RocStr)], std.mem.asBytes(&result.before));
                val.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 2)).write(u8, if (result.found) 1 else 0);
                break :blk val;
            },
            .str_drop_prefix_caseless_ascii => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.strDropPrefixCaselessAscii(valueToRocStr(args[0]), valueToRocStr(args[1]), &self.roc_ops);

                const layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (layout_val.tag != .struct_) {
                    return self.runtimeError("str_drop_prefix_caseless_ascii expected a record return layout");
                }
                const record_idx = layout_val.getStruct().idx;
                const fields = self.layout_store.struct_fields.sliceRange(self.layout_store.getStructData(record_idx).getFields());
                if (fields.len != 2 or
                    self.layout_store.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str or
                    self.layout_store.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .bool)
                {
                    return self.runtimeError("str_drop_prefix_caseless_ascii expected fields after Str, found Bool");
                }

                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 0)).ptr[0..@sizeOf(RocStr)], std.mem.asBytes(&result.after));
                val.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 1)).write(u8, if (result.found) 1 else 0);
                break :blk val;
            },
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
                // str_from_utf8(list) -> Try(Str, [BadUtf8 {index: U64, problem: Utf8Problem}, ..])
                // The C builtin returns FromUtf8Try (a flat struct). We convert it to the Roc
                // tag union layout using layout-resolved offsets. Note the err tag union has an
                // open extension (`..`), so at the call site it may be unified with other error
                // tags from `?` chaining. We must locate the BadUtf8 variant inside that
                // (possibly multi-variant) inner tag union and write the inner discriminant
                // when there is more than one inner variant.
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8C(self.valueToRocListForLayout(args[0], arg_layout), UpdateMode.Immutable, &self.roc_ops);

                const ret_layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (ret_layout_val.tag != .tag_union) {
                    return self.runtimeError("str_from_utf8 expected a tag union return layout");
                }
                const tu_data = self.layout_store.getTagUnionData(ret_layout_val.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);

                // Discover Ok (Str payload) and Err variant indices from the layout.
                var ok_disc: ?u16 = null;
                var err_disc: ?u16 = null;
                var err_record_idx: ?layout_mod.StructIdx = null;
                var inner_tu_data_opt: ?*const layout_mod.TagUnionData = null;
                var inner_bad_utf8_disc: u16 = 0;
                for (0..variants.len) |i| {
                    const v_payload = variants.get(@intCast(i)).payload_layout;
                    const candidate = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                    if (candidate == .str) {
                        ok_disc = @intCast(i);
                    } else {
                        err_disc = @intCast(i);
                        const err_layout = self.layout_store.getLayout(candidate);
                        switch (err_layout.tag) {
                            .struct_ => err_record_idx = err_layout.getStruct().idx,
                            .tag_union => {
                                const inner_tu = self.layout_store.getTagUnionData(err_layout.getTagUnion().idx);
                                inner_tu_data_opt = inner_tu;
                                const found = self.findBadUtf8Variant(inner_tu);
                                if (found) |info| {
                                    err_record_idx = info.struct_idx;
                                    inner_bad_utf8_disc = info.disc;
                                }
                            },
                            else => {},
                        }
                    }
                }

                const val = try self.alloc(ll.ret_layout);
                @memset(val.ptr[0..tu_data.size.get(self.layout_store.targetUsize())], 0);

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
                    if (inner_tu_data_opt) |inner_tu| {
                        // The inner tag union sits at offset 0 of the Err payload, which is at
                        // offset 0 of the outer tag union. Write its discriminant in place.
                        inner_tu.writeDiscriminant(val.ptr, inner_bad_utf8_disc, self.layout_store.targetUsize());
                    }
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
                const result = builtins.str.reserveC(valueToRocStr(args[0]), args[1].read(u64), updateModeForArg0(ll.unique_args), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_release_excess_capacity => self.callBuiltinStr1(builtins.str.strReleaseExcessCapacity, valueToRocStr(args[0]), updateModeForArg0(ll.unique_args), ll.ret_layout),
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
                const bits: u64 = @as(u64, @as(u32, @bitCast(args[0].read(f32))));
                const result = builtins.str.floatToStrFromBits(bits, true, &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .f64_to_str => blk: {
                const bits: u64 = @bitCast(args[0].read(f64));
                const result = builtins.str.floatToStrFromBits(bits, false, &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .f32_to_bits => blk: {
                const val = try self.alloc(ll.ret_layout);
                val.write(u32, @bitCast(args[0].read(f32)));
                break :blk val;
            },
            .f32_from_bits => blk: {
                const val = try self.alloc(ll.ret_layout);
                val.write(f32, @bitCast(args[0].read(u32)));
                break :blk val;
            },
            .f64_to_bits => blk: {
                const val = try self.alloc(ll.ret_layout);
                val.write(u64, @bitCast(args[0].read(f64)));
                break :blk val;
            },
            .f64_from_bits => blk: {
                const val = try self.alloc(ll.ret_layout);
                val.write(f64, @bitCast(args[0].read(u64)));
                break :blk val;
            },
            .num_to_str => blk: {
                // Generic num_to_str uses arg layout to determine type
                const size = self.helper.sizeOf(arg_layout);
                const l = self.layout_store.getLayout(arg_layout);
                const is_float = l.tag == .scalar and l.getScalar().tag == .frac;
                if (isDec(arg_layout)) {
                    const dec = RocDec{ .num = args[0].read(i128) };
                    var crash_boundary = self.enterCrashBoundary();
                    defer crash_boundary.deinit();
                    const sj = crash_boundary.set();
                    if (sj != 0) return error.Crash;
                    const result = builtins.dec.to_str(dec, &self.roc_ops);
                    break :blk self.rocStrToValue(result, ll.ret_layout);
                } else if (is_float) {
                    const bits: u64 = switch (size) {
                        4 => @as(u64, @as(u32, @bitCast(args[0].read(f32)))),
                        else => @bitCast(args[0].read(f64)),
                    };
                    const result = builtins.str.floatToStrFromBits(bits, size == 4, &self.roc_ops);
                    break :blk self.rocStrToValue(result, ll.ret_layout);
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
                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.ptr[0..info.width], elem_ptr[0..info.width]);
                break :blk val;
            },
            .list_append_unsafe => blk: {
                const info = self.listElemInfo(arg_layout);
                const list_val = self.valueToRocListForLayout(args[0], arg_layout);
                if (info.width == 0) {
                    break :blk self.rocListToValue(canonicalZstList(list_val.len() + 1), ll.ret_layout);
                }
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
                    break :blk self.rocListToValue(canonicalZstList(total_len), ll.ret_layout);
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
                    updateModeForArg0(ll.unique_args),
                    updateModeForArg1(ll.unique_args),
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_prepend => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                const list_val = self.valueToRocListForLayout(args[0], arg_layout);
                if (info.width == 0) {
                    break :blk self.rocListToValue(canonicalZstList(list_val.len() + 1), ll.ret_layout);
                }
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listPrepend(
                    list_val,
                    info.alignment,
                    @ptrCast(args[1].ptr),
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    updateModeForArg0(ll.unique_args),
                    &builtins.list.copy_fallback,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_swap => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                const list_val = self.valueToRocListForLayout(args[0], arg_layout);
                if (info.width == 0) {
                    // ZST elements: swap is a no-op on observable contents; length unchanged.
                    break :blk self.rocListToValue(canonicalZstList(list_val.len()), ll.ret_layout);
                }
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listSwap(
                    list_val,
                    info.alignment,
                    info.width,
                    args[1].read(u64),
                    args[2].read(u64),
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    updateModeForArg0(ll.unique_args),
                    &builtins.list.copy_fallback,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_map_can_reuse => blk: {
                const val = try self.alloc(ll.ret_layout);
                if (!ll.interchangeable.get(self.layout_store.targetUsize())) {
                    // The in-place branch is statically dead on this width.
                    val.write(u8, 0);
                    break :blk val;
                }
                const rl = self.valueToRocListForLayout(args[0], arg_layout);
                val.write(u8, if (builtins.list.listMapCanReuse(rl, &self.roc_ops)) 1 else 0);
                break :blk val;
            },
            .list_map_cast_unsafe => blk: {
                const rl = self.valueToRocListForLayout(args[0], arg_layout);
                break :blk self.rocListToValue(rl, ll.ret_layout);
            },
            .list_map_extract_unsafe => blk: {
                // Same data movement as list_get_unsafe; ownership of the
                // element transfers out of the buffer, which is RC metadata
                // rather than runtime behavior.
                const rl = self.valueToRocListForLayout(args[0], arg_layout);
                const idx = args[1].read(u64);
                const info = self.listElemInfo(arg_layout);
                if (info.width == 0) break :blk try self.alloc(ll.ret_layout);
                const elem_ptr = rl.bytes.? + @as(usize, @intCast(idx)) * info.width;
                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.ptr[0..info.width], elem_ptr[0..info.width]);
                break :blk val;
            },
            .list_map_write_unsafe => blk: {
                const rl = self.valueToRocListForLayout(args[0], arg_layout);
                const idx = args[1].read(u64);
                const info = self.listElemInfo(arg_layout);
                if (info.width == 0) break :blk self.rocListToValue(rl, ll.ret_layout);
                const elem_ptr = rl.bytes.? + @as(usize, @intCast(idx)) * info.width;
                @memcpy(elem_ptr[0..info.width], args[2].ptr[0..info.width]);
                break :blk self.rocListToValue(rl, ll.ret_layout);
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

                const record_idx = record_layout_val.getStruct().idx;
                const len_field_off = self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 0);
                const start_field_off = self.layout_store.getStructFieldOffsetByOriginalIndex(record_idx, 1);
                const start = args[1].offset(start_field_off).read(u64);
                const len = args[1].offset(len_field_off).read(u64);
                const source_list = self.valueToRocListForLayout(args[0], arg_layout);
                if (info.width == 0) {
                    const result_len = zstSublistLen(source_list.len(), start, len);
                    break :blk self.rocListToValue(canonicalZstList(result_len), ll.ret_layout);
                }

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
                    updateModeForArg0(ll.unique_args),
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_drop_at => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                const source_list = self.valueToRocListForLayout(args[0], arg_layout);
                if (info.width == 0) {
                    const len = source_list.len();
                    const drop_index = args[1].read(u64);
                    const result_len = if (drop_index >= @as(u64, @intCast(len))) len else len -| 1;
                    break :blk self.rocListToValue(canonicalZstList(result_len), ll.ret_layout);
                }
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listDropAt(
                    source_list,
                    info.alignment,
                    info.width,
                    elems_rc,
                    args[1].read(u64),
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    updateModeForArg0(ll.unique_args),
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_replace_unsafe => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);

                // The return layout is a 2-field record { list : List(a), value : a }.
                // Disambiguate the two fields by their layout tag (one is a list, one is the element).
                const ret_layout_val = self.layout_store.getLayout(ll.ret_layout);
                if (ret_layout_val.tag != .struct_) return self.runtimeError("list_replace_unsafe: expected struct return layout");
                const rec_idx = ret_layout_val.getStruct().idx;
                const f0_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(rec_idx, 0);
                const f0_layout_val = self.layout_store.getLayout(f0_layout);
                const f0_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(rec_idx, 0);
                const f1_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(rec_idx, 1);
                const f1_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(rec_idx, 1);
                const f0_is_list = f0_layout_val.tag == .list or f0_layout_val.tag == .list_of_zst;
                const list_field_off = if (f0_is_list) f0_offset else f1_offset;
                const value_field_off = if (f0_is_list) f1_offset else f0_offset;
                const list_field_layout = if (f0_is_list) f0_layout else f1_layout;

                const val = try self.alloc(ll.ret_layout);

                if (info.width == 0) {
                    // ZST element: list is unchanged, value field is zero-sized so we don't write to it.
                    const source_list = self.valueToRocListForLayout(args[0], arg_layout);
                    const list_val_inner = try self.rocListToValue(canonicalZstList(source_list.len()), list_field_layout);
                    @memcpy(val.offset(list_field_off).ptr[0..@sizeOf(RocList)], list_val_inner.ptr[0..@sizeOf(RocList)]);
                    break :blk val;
                }

                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };

                // listReplace writes the displaced (old) element into the out_element slot.
                // Aim that slot directly at the value field of the result record.
                const value_dest_ptr: [*]u8 = @ptrCast(val.offset(value_field_off).ptr);

                const result_list = if (updateModeForArg0(ll.unique_args) == .InPlace)
                    builtins.list.listReplaceInPlace(
                        self.valueToRocListForLayout(args[0], arg_layout),
                        args[1].read(u64),
                        @ptrCast(args[2].ptr),
                        info.width,
                        value_dest_ptr,
                        &builtins.list.copy_fallback,
                    )
                else
                    builtins.list.listReplace(
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
                        value_dest_ptr,
                        &builtins.list.copy_fallback,
                        &self.roc_ops,
                    );

                // Write the resulting list into the list field of the record.
                const list_val_inner = try self.rocListToValue(result_list, list_field_layout);
                @memcpy(val.offset(list_field_off).ptr[0..@sizeOf(RocList)], list_val_inner.ptr[0..@sizeOf(RocList)]);

                break :blk val;
            },
            .list_set => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                if (info.width == 0) {
                    const source_list = self.valueToRocListForLayout(args[0], arg_layout);
                    break :blk self.rocListToValue(canonicalZstList(source_list.len()), ll.ret_layout);
                }
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                // listReplace requires a scratch slot for the old element; we discard it here
                // because list_set returns only the new list (replace semantics return a pair).
                const old_elem = try self.allocAlignedBytes(info.width, layout_mod.RocAlignment.fromByteUnits(@intCast(info.alignment)));
                const result = if (updateModeForArg0(ll.unique_args) == .InPlace)
                    builtins.list.listReplaceInPlace(
                        self.valueToRocListForLayout(args[0], arg_layout),
                        args[1].read(u64),
                        @ptrCast(args[2].ptr),
                        info.width,
                        @ptrCast(old_elem.ptr),
                        &builtins.list.copy_fallback,
                    )
                else
                    builtins.list.listReplace(
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
                        &builtins.list.copy_fallback,
                        &self.roc_ops,
                    );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_with_capacity => blk: {
                const elem_layout = self.listElemLayout(ll.ret_layout);
                const sa = self.helper.sizeAlignOf(elem_layout);
                if (sa.size == 0) {
                    break :blk self.rocListToValue(canonicalZstList(0), ll.ret_layout);
                }
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
                if (info.width == 0) {
                    break :blk self.rocListToValue(canonicalZstList(list_val.len()), ll.ret_layout);
                }
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
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    updateModeForArg0(ll.unique_args),
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_release_excess_capacity => blk: {
                const info = self.listElemInfo(arg_layout);
                const elems_rc = self.builtinListElemRc(arg_layout);
                const list_val = self.valueToRocListForLayout(args[0], arg_layout);
                if (info.width == 0) {
                    break :blk self.rocListToValue(canonicalZstList(list_val.len()), ll.ret_layout);
                }
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                var elem_rc_ctx = ListElementRcContext{
                    .interp = self,
                    .elem_layout = self.listElemLayout(arg_layout),
                };
                const result = builtins.list.listReleaseExcessCapacity(
                    list_val,
                    info.alignment,
                    info.width,
                    elems_rc,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
                    if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
                    if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
                    updateModeForArg0(ll.unique_args),
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_first => self.evalListFirst(args[0], arg_layout, ll.ret_layout),
            .list_last => self.evalListLast(args[0], arg_layout, ll.ret_layout),
            .list_drop_first => self.evalListDropFirst(args[0], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),
            .list_drop_last => self.evalListDropLast(args[0], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),
            .list_take_first => self.evalListTakeFirst(args[0], args[1], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),
            .list_take_last => self.evalListTakeLast(args[0], args[1], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),
            .list_reverse => self.evalListReverse(args[0], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),
            .list_split_first => self.evalListSplitFirst(args[0], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),
            .list_split_last => self.evalListSplitLast(args[0], arg_layout, ll.ret_layout, updateModeForArg0(ll.unique_args)),

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
            .num_sin => self.evalNumFloatUnaryMath(args[0], ll.ret_layout, arg_layout, .sin),
            .num_cos => self.evalNumFloatUnaryMath(args[0], ll.ret_layout, arg_layout, .cos),
            .num_tan => self.evalNumFloatUnaryMath(args[0], ll.ret_layout, arg_layout, .tan),
            .num_asin => self.evalNumFloatUnaryMath(args[0], ll.ret_layout, arg_layout, .asin),
            .num_acos => self.evalNumFloatUnaryMath(args[0], ll.ret_layout, arg_layout, .acos),
            .num_atan => self.evalNumFloatUnaryMath(args[0], ll.ret_layout, arg_layout, .atan),
            .num_log => self.evalNumLog(args[0], ll.ret_layout, arg_layout),
            .num_round => self.evalNumRound(args[0], ll.ret_layout, arg_layout),
            .num_floor => self.evalNumFloor(args[0], ll.ret_layout, arg_layout),
            .num_ceiling => self.evalNumCeiling(args[0], ll.ret_layout, arg_layout),

            // ── Bitwise shifts ──
            .num_shift_left_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shl),
            .num_shift_right_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shr),
            .num_shift_right_zf_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shr_zf),

            // ── Bitwise logical ──
            .num_bitwise_and => self.numBitwiseOp(args[0], args[1], ll.ret_layout, arg_layout, .@"and"),
            .num_bitwise_or => self.numBitwiseOp(args[0], args[1], ll.ret_layout, arg_layout, .@"or"),
            .num_bitwise_xor => self.numBitwiseOp(args[0], args[1], ll.ret_layout, arg_layout, .xor),
            .num_bitwise_not => self.numBitwiseOp(args[0], args[0], ll.ret_layout, arg_layout, .not),

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

            // ── Hasher ──
            .dict_pseudo_seed => self.writeHasherValue(ll.ret_layout, builtins.utils.dictPseudoSeed()),
            .hasher_finish => self.writeHasherValue(ll.ret_layout, builtins.hash.hasher_finish(args[0].read(u64))),
            .hasher_write_bool => blk: {
                const seed = args[0].read(u64);
                const value: u64 = if (try self.readBoolValue(args[1], try self.lowLevelArgLayout(ll, 1))) 1 else 0;
                const next = builtins.hash.hasher_write_u64(seed, @intFromEnum(hasherDomain(ll.op)), value, hasherU64Width(ll.op));
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            => blk: {
                const seed = args[0].read(u64);
                const value: u64 = switch (ll.op) {
                    .hasher_write_u8 => args[1].read(u8),
                    .hasher_write_u16 => args[1].read(u16),
                    .hasher_write_u32 => args[1].read(u32),
                    .hasher_write_u64 => args[1].read(u64),
                    .hasher_write_i8 => @as(u64, @as(u8, @bitCast(args[1].read(i8)))),
                    .hasher_write_i16 => @as(u64, @as(u16, @bitCast(args[1].read(i16)))),
                    .hasher_write_i32 => @as(u64, @as(u32, @bitCast(args[1].read(i32)))),
                    .hasher_write_i64 => @bitCast(args[1].read(i64)),
                    else => unreachable,
                };
                const next = builtins.hash.hasher_write_u64(seed, @intFromEnum(hasherDomain(ll.op)), value, hasherU64Width(ll.op));
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },
            .hasher_write_f32 => blk: {
                const seed = args[0].read(u64);
                const value = args[1].read(f32);
                const bits: u64 = if (value == 0.0) 0 else @as(u64, @as(u32, @bitCast(value)));
                const next = builtins.hash.hasher_write_u64(seed, @intFromEnum(hasherDomain(ll.op)), bits, hasherU64Width(ll.op));
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },
            .hasher_write_f64 => blk: {
                const seed = args[0].read(u64);
                const value = args[1].read(f64);
                const bits: u64 = if (value == 0.0) 0 else @bitCast(value);
                const next = builtins.hash.hasher_write_u64(seed, @intFromEnum(hasherDomain(ll.op)), bits, hasherU64Width(ll.op));
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },
            .hasher_write_u128,
            .hasher_write_i128,
            .hasher_write_dec,
            => blk: {
                const seed = args[0].read(u64);
                const bits: u128 = @bitCast(args[1].read(i128));
                const low: u64 = @truncate(bits);
                const high: u64 = @truncate(bits >> 64);
                const next = builtins.hash.hasher_write_u128(seed, @intFromEnum(hasherDomain(ll.op)), low, high);
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },
            .hasher_write_bytes => blk: {
                const seed = args[0].read(u64);
                const bytes = try self.byteListSlice(args[1], try self.lowLevelArgLayout(ll, 1));
                const next = builtins.hash.hasher_write_bytes(seed, @intFromEnum(hasherDomain(ll.op)), bytes.ptr, bytes.len);
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },
            .hasher_write_str => blk: {
                const seed = args[0].read(u64);
                var str = valueToRocStr(args[1]);
                const bytes = str.asSlice();
                const next = builtins.hash.hasher_write_bytes(seed, @intFromEnum(hasherDomain(ll.op)), bytes.ptr, bytes.len);
                break :blk self.writeHasherValue(ll.ret_layout, next);
            },

            // ── Crypto ──
            .crypto_sha256_hash_bytes,
            .crypto_blake3_hash_bytes,
            => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const bytes = try self.byteListSlice(args[0], try self.lowLevelArgLayout(ll, 0));
                const result = switch (ll.op) {
                    .crypto_sha256_hash_bytes => builtins.crypto.sha256HashBytes(bytes.ptr, bytes.len, &self.roc_ops),
                    .crypto_blake3_hash_bytes => builtins.crypto.blake3HashBytes(bytes.ptr, bytes.len, &self.roc_ops),
                    else => unreachable,
                };
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .crypto_sha256_hasher_empty,
            .crypto_blake3_hasher_empty,
            => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const result = switch (ll.op) {
                    .crypto_sha256_hasher_empty => builtins.crypto.sha256HasherEmpty(&self.roc_ops),
                    .crypto_blake3_hasher_empty => builtins.crypto.blake3HasherEmpty(&self.roc_ops),
                    else => unreachable,
                };
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .crypto_sha256_hasher_write,
            .crypto_blake3_hasher_write,
            => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const state = try self.byteListSlice(args[0], try self.lowLevelArgLayout(ll, 0));
                const bytes = try self.byteListSlice(args[1], try self.lowLevelArgLayout(ll, 1));
                const result = switch (ll.op) {
                    .crypto_sha256_hasher_write => builtins.crypto.sha256HasherWrite(state.ptr, state.len, bytes.ptr, bytes.len, &self.roc_ops),
                    .crypto_blake3_hasher_write => builtins.crypto.blake3HasherWrite(state.ptr, state.len, bytes.ptr, bytes.len, &self.roc_ops),
                    else => unreachable,
                };
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .crypto_sha256_hasher_finish,
            .crypto_blake3_hasher_finish,
            => blk: {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const state = try self.byteListSlice(args[0], try self.lowLevelArgLayout(ll, 0));
                const result = switch (ll.op) {
                    .crypto_sha256_hasher_finish => builtins.crypto.sha256HasherFinish(state.ptr, state.len, &self.roc_ops),
                    .crypto_blake3_hasher_finish => builtins.crypto.blake3HasherFinish(state.ptr, state.len, &self.roc_ops),
                    else => unreachable,
                };
                break :blk self.rocListToValue(result, ll.ret_layout);
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

                const tu_data = self.layout_store.getTagUnionData(ret_layout_val.getTagUnion().idx);
                const result = try self.alloc(ll.ret_layout);
                const roc_str = valueToRocStr(args[0]);

                switch (parse_spec) {
                    .dec => dev_wrappers.roc_builtins_dec_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        tu_data.discriminant_offset.get(self.layout_store.targetUsize()),
                    ),
                    .float => |float| dev_wrappers.roc_builtins_float_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        float.width_bytes,
                        tu_data.discriminant_offset.get(self.layout_store.targetUsize()),
                    ),
                    .int => |int| dev_wrappers.roc_builtins_int_from_str(
                        result.ptr,
                        roc_str.bytes,
                        roc_str.length,
                        roc_str.capacity_or_alloc_ptr,
                        int.width_bytes,
                        int.signed,
                        tu_data.discriminant_offset.get(self.layout_store.targetUsize()),
                    ),
                }
                break :blk result;
            },
            .num_from_numeral => try self.evalNumFromNumeral(args[0], arg_layout, ll.ret_layout),

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
            .u128_to_dec_try_unsafe => self.intToDecTry(u128, args[0], ll.ret_layout),

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
            .i128_to_dec_try_unsafe => self.intToDecTry(i128, args[0], ll.ret_layout),

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
                if (!std.math.isNan(sv) and !std.math.isInf(sv) and
                    sv <= std.math.floatMax(f32) and sv >= -std.math.floatMax(f32))
                {
                    break :blk try self.writeLowLevelTryRecord(f32, ll.ret_layout, @floatCast(sv));
                } else {
                    break :blk try self.writeLowLevelTryRecord(f32, ll.ret_layout, null);
                }
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
                if (builtins.dec.toF32Try(dec)) |f| {
                    break :blk try self.writeLowLevelTryRecord(f32, ll.ret_layout, f);
                } else {
                    break :blk try self.writeLowLevelTryRecord(f32, ll.ret_layout, null);
                }
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
            .erased_capture_load => try self.evalErasedCaptureLoad(args[0], ll.ret_layout),
            .ptr_alloca => try self.evalPtrAlloca(ll.ret_layout),
            .box_alloc_zeroed => try self.evalBoxAllocZeroed(ll.ret_layout),
            .ptr_store => try self.evalPtrStore(args[0], args[1], ll.arg_layouts[1]),
            .ptr_load => try self.evalPtrLoad(args[0], ll.ret_layout),
            .ptr_cast => try self.evalPtrCast(args[0], ll.ret_layout),

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
            const field_layout = self.layout_store.getStructFieldLayout(struct_layout_val.getStruct().idx, @intCast(i));
            const field_offset = self.layout_store.getStructFieldOffset(struct_layout_val.getStruct().idx, @intCast(i));
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
    const BitwiseOp = enum { @"and", @"or", xor, not };
    const NumericOperandKind = union(enum) {
        unsigned_int: u16,
        signed_int: u16,
        float: u16,
        dec,
    };

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

    fn numericOperandKind(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error!NumericOperandKind {
        return switch (layout_idx) {
            .u8 => .{ .unsigned_int = 8 },
            .u16 => .{ .unsigned_int = 16 },
            .u32 => .{ .unsigned_int = 32 },
            .u64 => .{ .unsigned_int = 64 },
            .u128 => .{ .unsigned_int = 128 },
            .i8 => .{ .signed_int = 8 },
            .i16 => .{ .signed_int = 16 },
            .i32 => .{ .signed_int = 32 },
            .i64 => .{ .signed_int = 64 },
            .i128 => .{ .signed_int = 128 },
            .f32 => .{ .float = 32 },
            .f64 => .{ .float = 64 },
            .dec => .dec,
            else => self.invariantFailedError(
                "LIR/interpreter invariant violated: numeric low-level op used non-numeric layout {d} ({s})",
                .{ @intFromEnum(layout_idx), @tagName(self.layout_store.getLayout(layout_idx).tag) },
            ),
        };
    }

    fn numBinOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: NumOp) Error!Value {
        const val = try self.alloc(ret_layout);
        const kind = try self.numericOperandKind(arg_layout);
        const is_division_like = op == .div or op == .div_trunc or op == .rem or op == .mod;

        trace.log("numBinOp: op={s} arg_layout={any} ret_layout={any}", .{
            @tagName(op),
            arg_layout,
            ret_layout,
        });

        if (is_division_like) {
            switch (kind) {
                .unsigned_int => |bits| switch (bits) {
                    8 => if (b.read(u8) == 0) return self.divisionByZero(arg_layout),
                    16 => if (b.read(u16) == 0) return self.divisionByZero(arg_layout),
                    32 => if (b.read(u32) == 0) return self.divisionByZero(arg_layout),
                    64 => if (b.read(u64) == 0) return self.divisionByZero(arg_layout),
                    128 => if (b.read(u128) == 0) return self.divisionByZero(arg_layout),
                    else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported unsigned integer width {d}", .{bits}),
                },
                .signed_int => |bits| switch (bits) {
                    8 => if (b.read(i8) == 0) return self.divisionByZero(arg_layout),
                    16 => if (b.read(i16) == 0) return self.divisionByZero(arg_layout),
                    32 => if (b.read(i32) == 0) return self.divisionByZero(arg_layout),
                    64 => if (b.read(i64) == 0) return self.divisionByZero(arg_layout),
                    128 => if (b.read(i128) == 0) return self.divisionByZero(arg_layout),
                    else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported signed integer width {d}", .{bits}),
                },
                .dec => if (b.read(i128) == 0) return self.divisionByZero(arg_layout),
                .float => {},
            }
        }

        switch (kind) {
            .unsigned_int => |bits| switch (bits) {
                8 => val.write(u8, try self.intBinOp(u8, a.read(u8), b.read(u8), op)),
                16 => val.write(u16, try self.intBinOp(u16, a.read(u16), b.read(u16), op)),
                32 => val.write(u32, try self.intBinOp(u32, a.read(u32), b.read(u32), op)),
                64 => val.write(u64, try self.intBinOp(u64, a.read(u64), b.read(u64), op)),
                128 => val.write(u128, try self.intBinOp(u128, a.read(u128), b.read(u128), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported unsigned integer width {d}", .{bits}),
            },
            .signed_int => |bits| switch (bits) {
                8 => val.write(i8, try self.intBinOp(i8, a.read(i8), b.read(i8), op)),
                16 => val.write(i16, try self.intBinOp(i16, a.read(i16), b.read(i16), op)),
                32 => val.write(i32, try self.intBinOp(i32, a.read(i32), b.read(i32), op)),
                64 => val.write(i64, try self.intBinOp(i64, a.read(i64), b.read(i64), op)),
                128 => val.write(i128, try self.intBinOp(i128, a.read(i128), b.read(i128), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported signed integer width {d}", .{bits}),
            },
            .float => |bits| switch (bits) {
                32 => val.write(f32, floatBinOp(f32, a.read(f32), b.read(f32), op)),
                64 => val.write(f64, floatBinOp(f64, a.read(f64), b.read(f64), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float width {d}", .{bits}),
            },
            .dec => val.write(i128, try self.decBinOp(a.read(i128), b.read(i128), op)),
        }
        return val;
    }

    fn numUnaryOp(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: NumOp) Error!Value {
        return self.numBinOp(a, a, ret_layout, arg_layout, op);
    }

    fn numCmpOp(self: *LirInterpreter, a: Value, b: Value, arg_layout: layout_mod.Idx, op: CmpOp) Error!Value {
        const val = try self.alloc(.bool);
        const layout_val = self.layout_store.getLayout(arg_layout);

        if (op == .eq and switch (layout_val.tag) {
            .zst, .struct_, .list, .list_of_zst, .tag_union => true,
            .scalar => layout_val.getScalar().tag == .str,
            else => false,
        }) {
            val.write(u8, if (try self.valuesEqual(a, b, arg_layout)) 1 else 0);
            return val;
        }

        const result: bool = switch (try self.numericOperandKind(arg_layout)) {
            .unsigned_int => |bits| switch (bits) {
                8 => cmpOp(u8, a.read(u8), b.read(u8), op),
                16 => cmpOp(u16, a.read(u16), b.read(u16), op),
                32 => cmpOp(u32, a.read(u32), b.read(u32), op),
                64 => cmpOp(u64, a.read(u64), b.read(u64), op),
                128 => cmpOp(u128, a.read(u128), b.read(u128), op),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported unsigned integer compare width {d}", .{bits}),
            },
            .signed_int => |bits| switch (bits) {
                8 => cmpOp(i8, a.read(i8), b.read(i8), op),
                16 => cmpOp(i16, a.read(i16), b.read(i16), op),
                32 => cmpOp(i32, a.read(i32), b.read(i32), op),
                64 => cmpOp(i64, a.read(i64), b.read(i64), op),
                128 => cmpOp(i128, a.read(i128), b.read(i128), op),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported signed integer compare width {d}", .{bits}),
            },
            .float => |bits| switch (bits) {
                32 => cmpOp(f32, a.read(f32), b.read(f32), op),
                64 => cmpOp(f64, a.read(f64), b.read(f64), op),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float compare width {d}", .{bits}),
            },
            .dec => cmpOp(i128, a.read(i128), b.read(i128), op),
        };
        val.write(u8, if (result) 1 else 0);
        return val;
    }

    fn valuesEqual(self: *LirInterpreter, a: Value, b: Value, layout_idx: layout_mod.Idx) Error!bool {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .zst => true,
            .scalar => switch (layout_val.getScalar().tag) {
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
                break :blk try self.valuesEqual(.{ .ptr = a_ptr.? }, .{ .ptr = b_ptr.? }, layout_val.getIdx());
            },
            .erased_callable => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on erased callable layout {d} survived lowering",
                .{@intFromEnum(layout_idx)},
            ),
            .ptr => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on compiler-internal ptr layout {d}",
                .{@intFromEnum(layout_idx)},
            ),
            .struct_ => blk: {
                const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
                const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                var field_index: usize = 0;
                while (field_index < fields.len) : (field_index += 1) {
                    const field = fields.get(@intCast(field_index));
                    // Padding spacers hold uninitialized bytes; they are not part
                    // of a value's identity and must never be compared.
                    if (field.is_padding) continue;
                    const field_layout = field.layout;
                    const field_size = self.helper.sizeOf(field_layout);
                    if (field_size == 0) continue;
                    const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                        layout_val.getStruct().idx,
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
        // Runtime tag order for [LT, EQ, GT]: EQ=0, GT=1, LT=2.
        const result: u8 = switch (try self.numericOperandKind(arg_layout)) {
            .unsigned_int => |bits| switch (bits) {
                8 => cmpOrder(u8, a.read(u8), b.read(u8)),
                16 => cmpOrder(u16, a.read(u16), b.read(u16)),
                32 => cmpOrder(u32, a.read(u32), b.read(u32)),
                64 => cmpOrder(u64, a.read(u64), b.read(u64)),
                128 => cmpOrder(u128, a.read(u128), b.read(u128)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported unsigned integer compare width {d}", .{bits}),
            },
            .signed_int => |bits| switch (bits) {
                8 => cmpOrder(i8, a.read(i8), b.read(i8)),
                16 => cmpOrder(i16, a.read(i16), b.read(i16)),
                32 => cmpOrder(i32, a.read(i32), b.read(i32)),
                64 => cmpOrder(i64, a.read(i64), b.read(i64)),
                128 => cmpOrder(i128, a.read(i128), b.read(i128)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported signed integer compare width {d}", .{bits}),
            },
            .float => |bits| switch (bits) {
                32 => cmpOrder(f32, a.read(f32), b.read(f32)),
                64 => cmpOrder(f64, a.read(f64), b.read(f64)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float compare width {d}", .{bits}),
            },
            .dec => cmpOrder(i128, a.read(i128), b.read(i128)),
        };
        val.write(u8, result);
        return val;
    }

    fn numShiftOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: ShiftOp) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .unsigned_int => |bits| switch (bits) {
                8 => val.write(u8, shiftOp(u8, a.read(u8), b.read(u8), op)),
                16 => val.write(u16, shiftOp(u16, a.read(u16), b.read(u8), op)),
                32 => val.write(u32, shiftOp(u32, a.read(u32), b.read(u8), op)),
                64 => val.write(u64, shiftOp(u64, a.read(u64), b.read(u8), op)),
                128 => val.write(u128, shiftOp(u128, a.read(u128), b.read(u8), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported unsigned integer shift width {d}", .{bits}),
            },
            .signed_int => |bits| switch (bits) {
                8 => val.write(i8, shiftOp(i8, a.read(i8), b.read(u8), op)),
                16 => val.write(i16, shiftOp(i16, a.read(i16), b.read(u8), op)),
                32 => val.write(i32, shiftOp(i32, a.read(i32), b.read(u8), op)),
                64 => val.write(i64, shiftOp(i64, a.read(i64), b.read(u8), op)),
                128 => val.write(i128, shiftOp(i128, a.read(i128), b.read(u8), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported signed integer shift width {d}", .{bits}),
            },
            .float, .dec => return self.invariantFailedError(
                "LIR/interpreter invariant violated: shift used non-integer layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    fn numBitwiseOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: BitwiseOp) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .unsigned_int => |bits| switch (bits) {
                8 => val.write(u8, bitwiseOp(u8, a.read(u8), b.read(u8), op)),
                16 => val.write(u16, bitwiseOp(u16, a.read(u16), b.read(u16), op)),
                32 => val.write(u32, bitwiseOp(u32, a.read(u32), b.read(u32), op)),
                64 => val.write(u64, bitwiseOp(u64, a.read(u64), b.read(u64), op)),
                128 => val.write(u128, bitwiseOp(u128, a.read(u128), b.read(u128), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported unsigned integer bitwise width {d}", .{bits}),
            },
            .signed_int => |bits| switch (bits) {
                8 => val.write(i8, bitwiseOp(i8, a.read(i8), b.read(i8), op)),
                16 => val.write(i16, bitwiseOp(i16, a.read(i16), b.read(i16), op)),
                32 => val.write(i32, bitwiseOp(i32, a.read(i32), b.read(i32), op)),
                64 => val.write(i64, bitwiseOp(i64, a.read(i64), b.read(i64), op)),
                128 => val.write(i128, bitwiseOp(i128, a.read(i128), b.read(i128), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported signed integer bitwise width {d}", .{bits}),
            },
            .float, .dec => return self.invariantFailedError(
                "LIR/interpreter invariant violated: bitwise used non-integer layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    fn evalNumPow(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .dec => {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                val.write(i128, builtins.dec.powC(RocDec{ .num = a.read(i128) }, RocDec{ .num = b.read(i128) }, &self.roc_ops));
            },
            .float => |bits| switch (bits) {
                32 => val.write(f32, std.math.pow(f32, a.read(f32), b.read(f32))),
                64 => val.write(f64, std.math.pow(f64, a.read(f64), b.read(f64))),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float pow width {d}", .{bits}),
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_pow survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    fn evalNumSqrt(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .dec => {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                val.write(i128, builtins.dec.sqrtC(RocDec{ .num = a.read(i128) }, &self.roc_ops));
            },
            .float => |bits| switch (bits) {
                32 => val.write(f32, @sqrt(a.read(f32))),
                64 => val.write(f64, @sqrt(a.read(f64))),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float sqrt width {d}", .{bits}),
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_sqrt survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    fn evalNumLog(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .dec => {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                val.write(i128, builtins.dec.logC(RocDec{ .num = a.read(i128) }, &self.roc_ops));
            },
            .float => |bits| switch (bits) {
                32 => val.write(f32, @log(a.read(f32))),
                64 => val.write(f64, @log(a.read(f64))),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float log width {d}", .{bits}),
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_log survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    const FloatUnaryMathOp = enum {
        sin,
        cos,
        tan,
        asin,
        acos,
        atan,
    };

    fn floatUnaryMath(comptime F: type, value: F, comptime op: FloatUnaryMathOp) F {
        return switch (op) {
            .sin => std.math.sin(value),
            .cos => std.math.cos(value),
            .tan => std.math.tan(value),
            .asin => std.math.asin(value),
            .acos => std.math.acos(value),
            .atan => std.math.atan(value),
        };
    }

    fn evalNumFloatUnaryMath(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, comptime op: FloatUnaryMathOp) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .float => |bits| switch (bits) {
                32 => val.write(f32, floatUnaryMath(f32, a.read(f32), op)),
                64 => val.write(f64, floatUnaryMath(f64, a.read(f64), op)),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float {s} width {d}", .{ @tagName(op), bits }),
            },
            .dec => {
                var crash_boundary = self.enterCrashBoundary();
                defer crash_boundary.deinit();
                const sj = crash_boundary.set();
                if (sj != 0) return error.Crash;
                const dec = RocDec{ .num = a.read(i128) };
                const result = switch (op) {
                    .sin => builtins.dec.sinC(dec, &self.roc_ops),
                    .cos => builtins.dec.cosC(dec, &self.roc_ops),
                    .tan => builtins.dec.tanC(dec, &self.roc_ops),
                    .asin => builtins.dec.asinC(dec, &self.roc_ops),
                    .acos => builtins.dec.acosC(dec, &self.roc_ops),
                    .atan => builtins.dec.atanC(dec, &self.roc_ops),
                };
                val.write(i128, result);
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_{s} survived lowering for layout {d}",
                .{ @tagName(op), @intFromEnum(arg_layout) },
            ),
        }
        return val;
    }

    fn evalNumRound(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .dec => {
                const dec = RocDec{ .num = a.read(i128) };
                val.write(i128, RocDec.round(dec, &self.roc_ops).num);
            },
            .float => |bits| switch (bits) {
                32 => val.write(f32, @round(a.read(f32))),
                64 => val.write(f64, @round(a.read(f64))),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float round width {d}", .{bits}),
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_round survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    fn evalNumFloor(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .dec => return self.invariantFailedError(
                "LIR/interpreter invariant violated: Dec num_floor survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
            .float => |bits| switch (bits) {
                32 => val.write(f32, @floor(a.read(f32))),
                64 => val.write(f64, @floor(a.read(f64))),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float floor width {d}", .{bits}),
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_floor survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
        return val;
    }

    fn evalNumCeiling(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        switch (try self.numericOperandKind(arg_layout)) {
            .dec => return self.invariantFailedError(
                "LIR/interpreter invariant violated: Dec num_ceiling survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
            .float => |bits| switch (bits) {
                32 => val.write(f32, @ceil(a.read(f32))),
                64 => val.write(f64, @ceil(a.read(f64))),
                else => return self.invariantFailedError("LIR/interpreter invariant violated: unsupported float ceiling width {d}", .{bits}),
            },
            .signed_int, .unsigned_int => return self.invariantFailedError(
                "LIR/interpreter invariant violated: integer num_ceiling survived lowering for layout {d}",
                .{@intFromEnum(arg_layout)},
            ),
        }
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
            else => return self.invariantFailedError(
                "LIR/interpreter invariant violated: numeric widen target layout {d} has unsupported size {d}",
                .{ @intFromEnum(ret_layout), ret_size },
            ),
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

    const LowLevelTryRecord = struct {
        success_offset: u32,
        value_offset: u32,
    };

    const NumeralValue = struct {
        is_negative: bool,
        digits_before_pt: []const u8,
        digits_after_pt: []const u8,
        digits_after_pt_count: u64,
    };

    const NumeralResult = struct {
        ok_discriminant: u16,
        ok_payload_layout: layout_mod.Idx,
        err_discriminant: u16,
        err_payload_layout: layout_mod.Idx,
        target: NumericOperandKind,
    };

    fn evalNumFromNumeral(self: *LirInterpreter, numeral_arg: Value, numeral_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const numeral = try self.readNumeralValue(numeral_arg, numeral_layout);
        const result = try self.numeralResult(ret_layout);
        const text = try self.numeralDecimalText(numeral);

        const payload = (try self.parseNumeralPayload(text, result.ok_payload_layout, result.target)) orelse
            return try self.writeNumeralErr(ret_layout, result);

        return try self.writeNumeralOk(ret_layout, result, payload);
    }

    fn readNumeralValue(self: *LirInterpreter, numeral_arg: Value, numeral_layout: layout_mod.Idx) Error!NumeralValue {
        const tag_base = self.resolveTagUnionBaseValue(numeral_arg, numeral_layout);
        const tag_layout = self.layout_store.getLayout(tag_base.layout);
        if (tag_layout.tag != .tag_union) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: num_from_numeral argument layout {d} was not a tag union",
                .{@intFromEnum(tag_base.layout)},
            );
        }

        const discriminant = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
        const record_layout_idx = self.tagPayloadLayout(tag_base.layout, @intCast(discriminant));
        const record_base = self.resolveStructBaseValue(tag_base.value, record_layout_idx);
        const record_layout = self.layout_store.getLayout(record_base.layout);
        if (record_layout.tag != .struct_) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: Numeral payload layout {d} was not a record",
                .{@intFromEnum(record_base.layout)},
            );
        }

        const struct_info = self.layout_store.getStructInfo(record_layout);
        if (struct_info.fields.len != 4) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: Numeral record layout {d} had {d} fields",
                .{ @intFromEnum(record_base.layout), struct_info.fields.len },
            );
        }

        // Numeral record fields are stored in lexicographic order by name:
        // digits_after_pt, digits_after_pt_count, digits_before_pt, is_negative.
        const after = try self.readNumeralU8List(record_base.value, record_base.layout, 0);
        const after_count_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(record_layout.getStruct().idx, 1);
        if (after_count_layout != .u64) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: Numeral digits_after_pt_count layout {d} was not U64",
                .{@intFromEnum(after_count_layout)},
            );
        }
        const after_count_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(record_layout.getStruct().idx, 1);
        const before = try self.readNumeralU8List(record_base.value, record_base.layout, 2);
        const neg_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(record_layout.getStruct().idx, 3);
        const neg_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(record_layout.getStruct().idx, 3);

        return .{
            .is_negative = try self.readBoolValue(record_base.value.offset(neg_offset), neg_layout),
            .digits_before_pt = before,
            .digits_after_pt = after,
            .digits_after_pt_count = record_base.value.offset(after_count_offset).read(u64),
        };
    }

    fn readNumeralU8List(
        self: *LirInterpreter,
        record_base: Value,
        record_layout: layout_mod.Idx,
        field_index: u32,
    ) Error![]const u8 {
        const record_layout_val = self.layout_store.getLayout(record_layout);
        const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(record_layout_val.getStruct().idx, field_index);
        const elem = self.listElemLayout(field_layout);
        if (elem != .u8) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: Numeral digit field {d} element layout {d} was not U8",
                .{ field_index, @intFromEnum(elem) },
            );
        }
        const offset = self.layout_store.getStructFieldOffsetByOriginalIndex(record_layout_val.getStruct().idx, field_index);
        const list = self.valueToRocListForLayout(record_base.offset(offset), field_layout);
        if (list.len() == 0) return &.{};
        const bytes = list.bytes orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: non-empty Numeral digit list had null bytes",
                .{},
            );
        };
        return bytes[0..list.len()];
    }

    fn readBoolValue(self: *LirInterpreter, value: Value, bool_layout: layout_mod.Idx) Error!bool {
        if (bool_layout == .bool) return value.read(u8) != 0;

        const layout_val = self.layout_store.getLayout(bool_layout);
        if (layout_val.tag == .tag_union) {
            return self.helper.readTagDiscriminant(value, bool_layout) != 0;
        }

        return self.invariantFailedError(
            "LIR/interpreter invariant violated: Bool value used layout {d} ({s})",
            .{ @intFromEnum(bool_layout), @tagName(layout_val.tag) },
        );
    }

    fn numeralResult(self: *LirInterpreter, ret_layout: layout_mod.Idx) Error!NumeralResult {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        if (ret_layout_val.tag != .tag_union) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: num_from_numeral return layout {d} was not Try tag union",
                .{@intFromEnum(ret_layout)},
            );
        }

        const tu_data = self.layout_store.getTagUnionData(ret_layout_val.getTagUnion().idx);
        const variants = self.layout_store.getTagUnionVariants(tu_data);
        var ok_discriminant: ?u16 = null;
        var ok_payload_layout: layout_mod.Idx = .none;
        var err_discriminant: ?u16 = null;
        var err_payload_layout: layout_mod.Idx = .none;
        var target: ?NumericOperandKind = null;

        for (0..variants.len) |i| {
            const payload_layout = variants.get(@intCast(i)).payload_layout;
            const candidate = self.unwrapSingleFieldPayloadLayout(payload_layout) orelse payload_layout;
            if (self.numericOperandKindOrNull(candidate)) |kind| {
                if (ok_discriminant != null) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: num_from_numeral return layout {d} had multiple numeric payload variants",
                        .{@intFromEnum(ret_layout)},
                    );
                }
                ok_discriminant = @intCast(i);
                ok_payload_layout = candidate;
                target = kind;
            } else {
                if (err_discriminant != null) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: num_from_numeral return layout {d} had multiple non-numeric payload variants",
                        .{@intFromEnum(ret_layout)},
                    );
                }
                err_discriminant = @intCast(i);
                err_payload_layout = payload_layout;
            }
        }

        return .{
            .ok_discriminant = ok_discriminant orelse return self.invariantFailedError(
                "LIR/interpreter invariant violated: num_from_numeral return layout {d} had no numeric Ok payload",
                .{@intFromEnum(ret_layout)},
            ),
            .ok_payload_layout = ok_payload_layout,
            .err_discriminant = err_discriminant orelse return self.invariantFailedError(
                "LIR/interpreter invariant violated: num_from_numeral return layout {d} had no Err payload",
                .{@intFromEnum(ret_layout)},
            ),
            .err_payload_layout = err_payload_layout,
            .target = target orelse unreachable,
        };
    }

    fn numericOperandKindOrNull(_: *LirInterpreter, layout_idx: layout_mod.Idx) ?NumericOperandKind {
        return switch (layout_idx) {
            .u8 => .{ .unsigned_int = 8 },
            .u16 => .{ .unsigned_int = 16 },
            .u32 => .{ .unsigned_int = 32 },
            .u64 => .{ .unsigned_int = 64 },
            .u128 => .{ .unsigned_int = 128 },
            .i8 => .{ .signed_int = 8 },
            .i16 => .{ .signed_int = 16 },
            .i32 => .{ .signed_int = 32 },
            .i64 => .{ .signed_int = 64 },
            .i128 => .{ .signed_int = 128 },
            .f32 => .{ .float = 32 },
            .f64 => .{ .float = 64 },
            .dec => .dec,
            else => null,
        };
    }

    fn numeralDecimalText(self: *LirInterpreter, numeral: NumeralValue) Error![]const u8 {
        const allocator = self.arena.allocator();
        const before = try self.base256DecimalText(numeral.digits_before_pt, 1);
        const after_min_digits: usize = std.math.cast(usize, numeral.digits_after_pt_count) orelse
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: Numeral digits_after_pt_count exceeded host usize",
                .{},
            );
        const after = if (after_min_digits == 0)
            &[_]u8{}
        else
            try self.base256DecimalText(numeral.digits_after_pt, after_min_digits);

        const sign_len: usize = @intFromBool(numeral.is_negative);
        const dot_len: usize = @intFromBool(after_min_digits > 0);
        const total_len = sign_len + before.len + dot_len + after.len;
        const text = try allocator.alloc(u8, total_len);
        var offset: usize = 0;
        if (numeral.is_negative) {
            text[offset] = '-';
            offset += 1;
        }
        @memcpy(text[offset..][0..before.len], before);
        offset += before.len;
        if (after_min_digits > 0) {
            text[offset] = '.';
            offset += 1;
            @memcpy(text[offset..][0..after.len], after);
        }
        return text;
    }

    fn base256DecimalText(self: *LirInterpreter, bytes_be: []const u8, min_digits: usize) Error![]const u8 {
        const allocator = self.arena.allocator();
        var first_nonzero: usize = 0;
        while (first_nonzero < bytes_be.len and bytes_be[first_nonzero] == 0) : (first_nonzero += 1) {}

        if (first_nonzero == bytes_be.len) {
            const len = @max(min_digits, 1);
            const out = try allocator.alloc(u8, len);
            @memset(out, '0');
            return out;
        }

        var current = try allocator.dupe(u8, bytes_be[first_nonzero..]);
        var digits_rev = std.ArrayList(u8).empty;
        defer digits_rev.deinit(allocator);

        while (current.len > 0) {
            var quotient = try allocator.alloc(u8, current.len);
            var quotient_len: usize = 0;
            var remainder: u16 = 0;
            for (current) |byte| {
                const value = remainder * 256 + byte;
                const digit: u8 = @intCast(value / 10);
                remainder = value % 10;
                if (digit != 0 or quotient_len != 0) {
                    quotient[quotient_len] = digit;
                    quotient_len += 1;
                }
            }
            try digits_rev.append(allocator, '0' + @as(u8, @intCast(remainder)));
            current = quotient[0..quotient_len];
        }

        const digit_count = digits_rev.items.len;
        const total_len = @max(digit_count, min_digits);
        const out = try allocator.alloc(u8, total_len);
        const pad = total_len - digit_count;
        @memset(out[0..pad], '0');
        for (digits_rev.items, 0..) |digit, i| {
            out[pad + digit_count - 1 - i] = digit;
        }
        return out;
    }

    fn parseNumeralPayload(
        self: *LirInterpreter,
        text: []const u8,
        payload_layout: layout_mod.Idx,
        target: NumericOperandKind,
    ) Error!?Value {
        return switch (target) {
            .unsigned_int => |bits| switch (bits) {
                8 => try self.parseNumeralIntPayload(u8, text, payload_layout),
                16 => try self.parseNumeralIntPayload(u16, text, payload_layout),
                32 => try self.parseNumeralIntPayload(u32, text, payload_layout),
                64 => try self.parseNumeralIntPayload(u64, text, payload_layout),
                128 => try self.parseNumeralIntPayload(u128, text, payload_layout),
                else => self.invariantFailedError("LIR/interpreter invariant violated: unsupported num_from_numeral unsigned width {d}", .{bits}),
            },
            .signed_int => |bits| switch (bits) {
                8 => try self.parseNumeralIntPayload(i8, text, payload_layout),
                16 => try self.parseNumeralIntPayload(i16, text, payload_layout),
                32 => try self.parseNumeralIntPayload(i32, text, payload_layout),
                64 => try self.parseNumeralIntPayload(i64, text, payload_layout),
                128 => try self.parseNumeralIntPayload(i128, text, payload_layout),
                else => self.invariantFailedError("LIR/interpreter invariant violated: unsupported num_from_numeral signed width {d}", .{bits}),
            },
            .float => |bits| switch (bits) {
                32 => try self.parseNumeralFloatPayload(f32, text, payload_layout),
                64 => try self.parseNumeralFloatPayload(f64, text, payload_layout),
                else => self.invariantFailedError("LIR/interpreter invariant violated: unsupported num_from_numeral float width {d}", .{bits}),
            },
            .dec => try self.parseNumeralDecPayload(text, payload_layout),
        };
    }

    fn parseNumeralIntPayload(self: *LirInterpreter, comptime T: type, text: []const u8, payload_layout: layout_mod.Idx) Error!?Value {
        const parsed = std.fmt.parseInt(T, text, 10) catch return null;
        const value = try self.alloc(payload_layout);
        value.write(T, parsed);
        return value;
    }

    fn parseNumeralFloatPayload(self: *LirInterpreter, comptime T: type, text: []const u8, payload_layout: layout_mod.Idx) Error!?Value {
        const parsed = std.fmt.parseFloat(T, text) catch return null;
        const value = try self.alloc(payload_layout);
        value.write(T, parsed);
        return value;
    }

    fn parseNumeralDecPayload(self: *LirInterpreter, text: []const u8, payload_layout: layout_mod.Idx) Error!?Value {
        const parsed = RocDec.fromNonemptySlice(text) orelse return null;
        const value = try self.alloc(payload_layout);
        value.write(i128, parsed.num);
        return value;
    }

    fn writeNumeralOk(self: *LirInterpreter, ret_layout: layout_mod.Idx, result: NumeralResult, payload: Value) Error!Value {
        const value = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(ret_layout);
        if (size > 0) @memset(value.ptr[0..size], 0);
        try self.writeVariantPayloadValue(value, self.tagPayloadLayout(ret_layout, result.ok_discriminant), payload, result.ok_payload_layout);
        self.helper.writeTagDiscriminant(value, ret_layout, result.ok_discriminant);
        return value;
    }

    fn writeNumeralErr(self: *LirInterpreter, ret_layout: layout_mod.Idx, result: NumeralResult) Error!Value {
        const value = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(ret_layout);
        if (size > 0) @memset(value.ptr[0..size], 0);
        const err_value = try self.invalidNumeralValue(result.err_payload_layout);
        try self.writeVariantPayloadValue(value, result.err_payload_layout, err_value, result.err_payload_layout);
        self.helper.writeTagDiscriminant(value, ret_layout, result.err_discriminant);
        return value;
    }

    fn invalidNumeralValue(self: *LirInterpreter, err_layout: layout_mod.Idx) Error!Value {
        const err_layout_val = self.layout_store.getLayout(err_layout);
        switch (err_layout_val.tag) {
            .tag_union => {
                const info = self.layout_store.getTagUnionInfo(err_layout_val);
                if (info.variants.len != 1) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: num_from_numeral Err payload layout {d} had {d} variants without an explicit InvalidNumeral discriminant",
                        .{ @intFromEnum(err_layout), info.variants.len },
                    );
                }
                const allocated = try self.allocTagValue(err_layout);
                const payload_layout = self.tagPayloadLayout(err_layout, 0);
                const message = try self.makeRocStr("invalid numeric literal");
                try self.writeVariantPayloadValue(allocated.base, payload_layout, message, .str);
                self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, 0);
                return allocated.outer;
            },
            .struct_ => {
                const unwrapped = self.unwrapSingleFieldPayloadLayout(err_layout) orelse {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: num_from_numeral Err payload layout {d} was not an error tag union",
                        .{@intFromEnum(err_layout)},
                    );
                };
                const inner = try self.invalidNumeralValue(unwrapped);
                const value = try self.alloc(err_layout);
                const struct_idx = err_layout_val.getStruct().idx;
                const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
                try self.writeStructFieldValue(value, field_offset, unwrapped, inner, unwrapped);
                return value;
            },
            .scalar => if (err_layout == .str) {
                return try self.makeRocStr("invalid numeric literal");
            } else return self.invariantFailedError(
                "LIR/interpreter invariant violated: num_from_numeral Err payload scalar layout {d} was not Str",
                .{@intFromEnum(err_layout)},
            ),
            else => return self.invariantFailedError(
                "LIR/interpreter invariant violated: num_from_numeral Err payload layout {d} had unsupported tag {s}",
                .{ @intFromEnum(err_layout), @tagName(err_layout_val.tag) },
            ),
        }
    }

    fn writeVariantPayloadValue(
        self: *LirInterpreter,
        destination: Value,
        variant_payload_layout: layout_mod.Idx,
        payload: Value,
        payload_layout: layout_mod.Idx,
    ) Error!void {
        if (self.helper.sizeOf(variant_payload_layout) == 0) return;
        if (variant_payload_layout == payload_layout) {
            destination.copyFrom(payload, self.helper.sizeOf(variant_payload_layout));
            return;
        }
        if (self.unwrapSingleFieldPayloadLayout(variant_payload_layout)) |field_layout| {
            const variant_layout_val = self.layout_store.getLayout(variant_payload_layout);
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(variant_layout_val.getStruct().idx, 0);
            return self.writeStructFieldValue(destination, field_offset, field_layout, payload, payload_layout);
        }
        const coerced = try self.coerceExplicitRefValueToLayout(payload, payload_layout, variant_payload_layout);
        destination.copyFrom(coerced, self.helper.sizeOf(variant_payload_layout));
    }

    fn lowLevelTryRecord(self: *LirInterpreter, ret_layout: layout_mod.Idx) Error!LowLevelTryRecord {
        const layout_val = self.layout_store.getLayout(ret_layout);
        if (layout_val.tag != .struct_) {
            return self.runtimeError("low-level try record expected a struct return layout");
        }

        const struct_idx = layout_val.getStruct().idx;
        const struct_info = self.layout_store.getStructInfo(layout_val);
        if (struct_info.fields.len != 2) {
            return self.runtimeError("low-level try record expected exactly two fields");
        }

        return .{
            .success_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
            .value_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
        };
    }

    fn writeLowLevelTryRecord(self: *LirInterpreter, comptime Payload: type, ret_layout: layout_mod.Idx, maybe_payload: ?Payload) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(ret_layout);
        if (size > 0) {
            @memset(val.ptr[0..size], 0);
        }
        const fields = try self.lowLevelTryRecord(ret_layout);
        if (maybe_payload) |payload| {
            val.offset(fields.value_offset).write(Payload, payload);
            val.offset(fields.success_offset).write(u8, 1);
        } else {
            val.offset(fields.success_offset).write(u8, 0);
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
            val.write(Dst, floatToIntWrap(Src, Dst, sv));
        }
        return val;
    }

    fn floatToIntWrap(comptime Src: type, comptime Dst: type, sv: Src) Dst {
        const truncated = @trunc(sv);
        const int_info = @typeInfo(Dst).int;

        if (int_info.bits <= 64) {
            const U = std.meta.Int(.unsigned, int_info.bits);
            const modulus: Src = @floatFromInt(@as(u128, 1) << int_info.bits);
            var remainder = @mod(truncated, modulus);
            if (remainder < 0) remainder += modulus;
            if (remainder >= modulus) remainder = 0;
            const unsigned: U = @intFromFloat(remainder);
            return @bitCast(unsigned);
        }

        const min_val: Src = if (int_info.signedness == .signed)
            @floatFromInt(std.math.minInt(Dst))
        else
            0;
        const max_val: Src = @floatFromInt(std.math.maxInt(Dst));
        if (truncated >= min_val and truncated <= max_val) {
            return @intFromFloat(truncated);
        }
        return 0;
    }

    fn floatToIntTry(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const sv = arg.read(Src);
        if (builtins.numeric_conversions.floatToIntTry(Src, Dst, sv)) |value| {
            return self.writeLowLevelTryRecord(Dst, ret_layout, value);
        }
        return self.writeLowLevelTryRecord(Dst, ret_layout, null);
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
        const dec = RocDec{ .num = arg.read(i128) };
        if (builtins.dec.toIntTry(Dst, dec)) |dv| {
            return self.writeLowLevelTryRecord(Dst, ret_layout, dv);
        } else {
            return self.writeLowLevelTryRecord(Dst, ret_layout, null);
        }
    }

    fn intToDecTry(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const sv = arg.read(Src);
        const maybe_dec = switch (@typeInfo(Src).int.signedness) {
            .signed => RocDec.fromWholeInt(@intCast(sv)),
            .unsigned => blk: {
                if (sv > @as(Src, @intCast(std.math.maxInt(i128)))) break :blk null;
                break :blk RocDec.fromWholeInt(@intCast(sv));
            },
        };
        if (maybe_dec) |dec| {
            return self.writeLowLevelTryRecord(i128, ret_layout, dec.num);
        }
        return self.writeLowLevelTryRecord(i128, ret_layout, null);
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
        if (rl.len() > 0 and info.width == 0) {
            self.helper.writeTagDiscriminant(val, ret_layout, 1); // Ok tag
        } else if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
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
        if (rl.len() > 0 and info.width == 0) {
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            const last_offset = (rl.len() - 1) * info.width;
            @memcpy(val.ptr[0..info.width], rl.bytes.?[last_offset..][0..info.width]);
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    fn evalListDropFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        if (info.width == 0) {
            return self.rocListToValue(canonicalZstList(zstSublistLen(rl.len(), 1, std.math.maxInt(u64))), ret_layout);
        }
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            elems_rc,
            1,
            std.math.maxInt(u64),
            null,
            &builtins.utils.rcNone,
            update_mode,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListDropLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const len = rl.len();
        if (info.width == 0) {
            return self.rocListToValue(canonicalZstList(if (len == 0) 0 else len - 1), ret_layout);
        }
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
            update_mode,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeFirst(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        if (info.width == 0) {
            return self.rocListToValue(canonicalZstList(zstSublistLen(rl.len(), 0, count_arg.read(u64))), ret_layout);
        }
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
            count_arg.read(u64),
            null,
            &builtins.utils.rcNone,
            update_mode,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeLast(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const len = rl.len();
        const take = count_arg.read(u64);
        const start = if (take >= len) 0 else len - @as(usize, @intCast(take));
        if (info.width == 0) {
            return self.rocListToValue(canonicalZstList(zstSublistLen(len, @intCast(start), take)), ret_layout);
        }
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
            update_mode,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListReverse(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        if (info.width == 0) return self.rocListToValue(canonicalZstList(rl.len()), ret_layout);
        var crash_boundary = self.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return error.Crash;
        var elem_rc_ctx = ListElementRcContext{
            .interp = self,
            .elem_layout = self.listElemLayout(list_layout),
        };
        const result = builtins.list.listReverse(
            rl,
            info.alignment,
            info.width,
            elems_rc,
            if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
            if (elems_rc) &listElementIncref else &builtins.utils.rcNone,
            if (elems_rc) @ptrCast(&elem_rc_ctx) else null,
            if (elems_rc) &listElementDecref else &builtins.utils.rcNone,
            update_mode,
            &builtins.list.copy_fallback,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListSplitFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const elem_layout = self.listElemLayout(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and info.width == 0) {
            const payload_layout = self.tagPayloadLayout(ret_layout, 1);
            const pair = self.resolveListElementPairStruct(payload_layout);
            try self.writeStructFieldValue(val, pair.elem_offset, pair.elem_layout, Value.zst, elem_layout);
            const rest_value = try self.rocListToValue(canonicalZstList(rl.len() - 1), pair.list_layout);
            try self.writeStructFieldValue(val, pair.list_offset, pair.list_layout, rest_value, pair.list_layout);
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
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
                update_mode,
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

    fn evalListSplitLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx, update_mode: UpdateMode) Error!Value {
        const rl = self.valueToRocListForLayout(list_arg, list_layout);
        const info = self.listElemInfo(list_layout);
        const elems_rc = self.builtinListElemRc(list_layout);
        const elem_layout = self.listElemLayout(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and info.width == 0) {
            const payload_layout = self.tagPayloadLayout(ret_layout, 1);
            const pair = self.resolveListElementPairStruct(payload_layout);
            try self.writeStructFieldValue(val, pair.elem_offset, pair.elem_layout, Value.zst, elem_layout);
            const rest_value = try self.rocListToValue(canonicalZstList(rl.len() - 1), pair.list_layout);
            try self.writeStructFieldValue(val, pair.list_offset, pair.list_layout, rest_value, pair.list_layout);
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
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
                update_mode,
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
    fn intBinOp(self: *LirInterpreter, comptime T: type, av: T, bv: T, op: NumOp) Error!T {
        return switch (op) {
            .add => checkedIntAdd(T, av, bv) orelse return self.triggerCrash("Integer addition overflowed!"),
            .sub => checkedIntSub(T, av, bv) orelse return self.triggerCrash("Integer subtraction overflowed!"),
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
            .div, .div_trunc => if (bv != 0) (if (signedMinDivOverflow(T, av, bv)) av else @divTrunc(av, bv)) else 0,
            .rem => if (bv != 0) (if (signedMinDivOverflow(T, av, bv)) 0 else @rem(av, bv)) else 0,
            .mod => if (bv != 0) (if (signedMinDivOverflow(T, av, bv)) 0 else @mod(av, bv)) else 0,
        };
    }

    fn checkedIntAdd(comptime T: type, av: T, bv: T) ?T {
        const result = @addWithOverflow(av, bv);
        if (result[1] != 0) return null;
        return result[0];
    }

    fn checkedIntSub(comptime T: type, av: T, bv: T) ?T {
        const result = @subWithOverflow(av, bv);
        if (result[1] != 0) return null;
        return result[0];
    }

    fn signedMinDivOverflow(comptime T: type, av: T, bv: T) bool {
        if (@typeInfo(T).int.signedness != .signed) return false;
        return av == std.math.minInt(T) and bv == -1;
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
            .div => av / bv,
            .div_trunc => @trunc(av / bv),
            .rem, .mod => @rem(av, bv),
        };
    }

    /// Dec (fixed-point i128 with 10^18 scale) binary operation.
    fn decBinOp(self: *LirInterpreter, av: i128, bv: i128, op: NumOp) Error!i128 {
        return switch (op) {
            .add => av +% bv,
            .sub => av -% bv,
            .negate => -%av,
            .abs => if (av < 0) -%av else av,
            .abs_diff => if (av > bv) av -% bv else bv -% av,
            .mul => blk: {
                const result = RocDec.mulWithOverflow(RocDec{ .num = av }, RocDec{ .num = bv });
                if (result.has_overflowed) return self.triggerCrash("Decimal multiplication overflowed!");
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
        if (av == bv) return 0; // EQ
        if (av > bv) return 1; // GT
        return 2; // LT
    }

    fn shiftOp(comptime T: type, av: T, amount: u8, op: ShiftOp) T {
        const Bits = std.math.Log2Int(T);
        const max_bits = @typeInfo(T).int.bits;
        if (amount >= max_bits) {
            return switch (op) {
                .shr => if (@typeInfo(T).int.signedness == .signed and av < 0) @as(T, -1) else 0,
                .shl, .shr_zf => 0,
            };
        }
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

    fn bitwiseOp(comptime T: type, av: T, bv: T, op: BitwiseOp) T {
        return switch (op) {
            .@"and" => av & bv,
            .@"or" => av | bv,
            .xor => av ^ bv,
            .not => ~av,
        };
    }

    // String operations

    // Layout helpers

    fn readPointerInt(self: *const LirInterpreter, value: Value) usize {
        return switch (self.layout_store.targetUsize().size()) {
            4 => value.read(u32),
            8 => value.read(usize),
            else => unreachable,
        };
    }

    fn writePointerInt(self: *const LirInterpreter, value: Value, raw_ptr: usize) void {
        switch (self.layout_store.targetUsize().size()) {
            4 => value.write(u32, @intCast(raw_ptr)),
            8 => value.write(usize, raw_ptr),
            else => unreachable,
        }
    }

    fn allocPointerIntValue(self: *LirInterpreter, raw_ptr: usize) Error!Value {
        const value = try self.alloc(.opaque_ptr);
        self.writePointerInt(value, raw_ptr);
        return value;
    }

    fn readBoxedDataPointer(self: *const LirInterpreter, boxed: Value) ?[*]u8 {
        const raw_ptr = self.readPointerInt(boxed);

        if (raw_ptr == 0) return null;
        return @ptrFromInt(raw_ptr);
    }

    fn writeBoxedDataPointer(self: *const LirInterpreter, boxed: Value, data_ptr: ?[*]u8) void {
        const raw_ptr: usize = if (data_ptr) |ptr| @intFromPtr(ptr) else 0;
        self.writePointerInt(boxed, raw_ptr);
    }

    fn allocBoxOfZstValue(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error!Value {
        const boxed = try self.alloc(layout_idx);
        const target_usize = self.layout_store.targetUsize();
        if (target_usize.size() == 8) {
            boxed.write(usize, 0);
        } else {
            boxed.write(u32, 0);
        }
        return boxed;
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
                const inner_layout = struct_layout_val.getIdx();
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
            const inner_layout = union_layout_val.getIdx();
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
                const tu_data = self.layout_store.getTagUnionData(l.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            .box => blk: {
                const inner_layout = self.layout_store.getLayout(l.getIdx());
                if (inner_layout.tag != .tag_union) break :blk .zst;
                const tu_data = self.layout_store.getTagUnionData(inner_layout.getTagUnion().idx);
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
                if (actual_layout_val.getIdx() == expected_layout) {
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
                    "LIR/interpreter invariant violated: explicit list reinterpret expected list layouts, got actual={d} expected={d}",
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
            const actual_is_erased_ptr = actual_layout_val.tag == .scalar and actual_layout_val.getScalar().tag == .opaque_ptr;
            const expected_is_erased_ptr = expected_layout_val.tag == .scalar and expected_layout_val.getScalar().tag == .opaque_ptr;
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            const boxing_compatible =
                (actual_is_box == expected_is_box) or
                (actual_is_box and expected_is_erased_ptr) or
                (expected_is_box and actual_is_erased_ptr);
            if (!boxing_compatible or actual_is_list or expected_is_list) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit nominal reinterpret expected non-list layouts on the same side of layout boxing, got actual={d} ({s}) expected={d} ({s})",
                    .{
                        @intFromEnum(actual_layout),
                        @tagName(actual_layout_val.tag),
                        @intFromEnum(expected_layout),
                        @tagName(expected_layout_val.tag),
                    },
                );
            }
        }
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (expected_layout_val.tag == .box_of_zst) {
            return try self.allocBoxOfZstValue(expected_layout);
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
                "LIR/interpreter invariant violated: explicit ref reinterpret reached aggregate coercion path actual={d} ({s}) expected={d} ({s})",
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

    fn getLayout(self: *LirInterpreter, idx: layout_mod.Idx) Layout {
        return self.layout_store.getLayout(idx);
    }

    fn evalBoxBox(self: *LirInterpreter, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box_of_zst => return try self.allocBoxOfZstValue(ret_layout),
            .box => {
                const box_info = self.boxAllocInfo(ret_layout_val);
                const elem_size = box_info.elem_size;
                const elem_align = box_info.elem_alignment;
                const data_ptr = try self.allocRocDataWithRc(elem_size, elem_align, box_info.contains_rc);
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

    fn evalErasedCaptureLoad(self: *LirInterpreter, capture_ptr: Value, ret_layout: layout_mod.Idx) Error!Value {
        if (ret_layout == .zst) return Value.zst;

        const result = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(ret_layout);
        if (size > 0) {
            const raw_capture_ptr = self.readPointerInt(capture_ptr);
            if (builtin.mode == .Debug and raw_capture_ptr == 0) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: erased capture load received a null capture pointer for non-ZST layout {d}",
                    .{@intFromEnum(ret_layout)},
                );
            }
            result.copyFrom(.{ .ptr = @ptrFromInt(raw_capture_ptr) }, size);
        }

        return result;
    }

    /// ptr_alloca: reserve a zeroed frame slot for the ptr layout's element and
    /// yield its address. The slot lives in the eval arena, which outlives the
    /// frame — fine, since TRMC emits at most one alloca per proc invocation.
    fn evalPtrAlloca(self: *LirInterpreter, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        if (builtin.mode == .Debug and ret_layout_val.tag != .ptr) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: ptr_alloca target had layout {s}, expected ptr",
                .{@tagName(ret_layout_val.tag)},
            );
        }
        const sa = self.helper.sizeAlignOf(ret_layout_val.getIdx());
        // allocAlignedByteSlice zero-fills, so the slot reads as null holes until written.
        const slot = try self.allocAlignedByteSlice(@max(sa.size, 1), sa.alignment);
        const result = try self.alloc(ret_layout);
        self.writePointerInt(result, @intFromPtr(slot.ptr));
        return result;
    }

    /// box_alloc_zeroed: a box_box whose payload is all zeroes — heap cell with
    /// rc=1 and a zero-filled payload (so any box fields inside read as null).
    fn evalBoxAllocZeroed(self: *LirInterpreter, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        if (ret_layout_val.tag == .box_of_zst) return try self.allocBoxOfZstValue(ret_layout);

        const box_info = self.boxAllocInfo(ret_layout_val);
        const data_ptr = try self.allocRocDataWithRc(box_info.elem_size, box_info.elem_alignment, box_info.contains_rc);
        if (box_info.elem_size > 0) {
            @memset(data_ptr[0..box_info.elem_size], 0);
        }
        const boxed = try self.alloc(ret_layout);
        self.writeBoxedDataPointer(boxed, data_ptr);
        return boxed;
    }

    /// ptr_store: copy sizeOf(value layout) bytes from the value into *ptr.
    fn evalPtrStore(self: *LirInterpreter, ptr_val: Value, value: Value, value_layout: layout_mod.Idx) Error!Value {
        const size = self.helper.sizeOf(value_layout);
        if (size > 0) {
            const raw_ptr = self.readPointerInt(ptr_val);
            if (builtin.mode == .Debug and raw_ptr == 0) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: ptr_store received a null pointer for non-ZST layout {d}",
                    .{@intFromEnum(value_layout)},
                );
            }
            const dest: [*]u8 = @ptrFromInt(raw_ptr);
            @memcpy(dest[0..size], value.ptr[0..size]);
        }
        return Value.zst;
    }

    /// ptr_load: copy sizeOf(target layout) bytes out of *ptr.
    fn evalPtrLoad(self: *LirInterpreter, ptr_val: Value, ret_layout: layout_mod.Idx) Error!Value {
        if (ret_layout == .zst) return Value.zst;

        const result = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(ret_layout);
        if (size > 0) {
            const raw_ptr = self.readPointerInt(ptr_val);
            if (builtin.mode == .Debug and raw_ptr == 0) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: ptr_load received a null pointer for non-ZST layout {d}",
                    .{@intFromEnum(ret_layout)},
                );
            }
            result.copyFrom(.{ .ptr = @ptrFromInt(raw_ptr) }, size);
        }
        return result;
    }

    /// ptr_cast: identity bits (box(T) -> ptr(T) or ptr -> ptr).
    fn evalPtrCast(self: *LirInterpreter, ptr_val: Value, ret_layout: layout_mod.Idx) Error!Value {
        const result = try self.alloc(ret_layout);
        self.writePointerInt(result, self.readPointerInt(ptr_val));
        return result;
    }

    // ═══════════════════════════════════════════════════════════════════
};
