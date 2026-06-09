//! Debug-only borrow certifier for ARC-complete LIR.
//!
//! The certifier re-checks emitted RC statements against the ownership rules
//! that ARC insertion is supposed to uphold, using only the emitted LIR and
//! the ARC-stage signature table:
//!
//! - every use of a refcounted local happens while its value is provably live
//!   (it carries an ownership unit, or it borrows from a value that does)
//! - every `decref`/`free` releases a unit that exists (no double-free, and
//!   never a release of a borrowed value)
//! - every ownership unit is released or transferred exactly once on every
//!   path (no leaks at `ret`, `crash`, or `runtime_error`)
//! - all jumps to one join point agree on which locals carry units, how local
//!   names alias one value, and where borrows take their liveness from
//!
//! Ownership units are tracked per value, not per local: a local-to-local
//! assignment makes both names share one value, and the RC statements on
//! either name act on that shared value's balance. Payload reads produce
//! fresh values that borrow from their source value. Aggregate construction
//! moves one unit per refcounted operand occurrence into the aggregate; the
//! emitted trailing increfs restore the operands' own units, so aggregate
//! consumption tolerates a transiently negative balance and relies on the
//! per-path terminal balance check to flag a missing restore.
//!
//! A certification failure is a compiler bug in ARC insertion. The production
//! entry point panics in debug builds; release builds never run the certifier.

const std = @import("std");
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

pub const CertifyError = error{ OutOfMemory, Certification };

/// Holds the first violation message for test inspection.
pub const Diagnostic = struct {
    buffer: [512]u8 = undefined,
    len: usize = 0,
    /// Local implicated by the violation, for failure-context dumps.
    context_local: ?LIR.LocalId = null,
    /// Proc containing the violation.
    context_proc: ?LIR.LirProcSpecId = null,

    pub fn message(self: *const Diagnostic) []const u8 {
        return self.buffer[0..self.len];
    }

    fn set(self: *Diagnostic, comptime fmt: []const u8, args: anytype) void {
        self.len = (std.fmt.bufPrint(&self.buffer, fmt, args) catch {
            self.len = self.buffer.len;
            return;
        }).len;
    }
};

/// Certifies every proc body in the store. Returns `error.Certification`
/// with `diag` filled on the first violation.
pub fn certifyStore(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
    diag: *Diagnostic,
) CertifyError!void {
    var rc_local = try allocator.alloc(bool, store.locals.items.len);
    defer allocator.free(rc_local);
    for (store.locals.items, 0..) |local, index| {
        rc_local[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }

    var certifier = Certifier{
        .allocator = allocator,
        .store = store,
        .sigs = sigs,
        .rc_local = rc_local,
        .lender_arena = std.heap.ArenaAllocator.init(allocator),
        .records = std.AutoHashMap(LIR.JoinPointId, JoinRecord).init(allocator),
        .memo = std.AutoHashMap(MemoEntry, void).init(allocator),
        .repr_scratch = std.AutoHashMap(ValueId, u32).init(allocator),
        .join_bodies = std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId).init(allocator),
        .scan_visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator),
        .diag = diag,
    };
    defer certifier.deinit();

    for (store.proc_specs.items, 0..) |proc, index| {
        const body = proc.body orelse continue;
        try certifier.certifyProc(@enumFromInt(@as(u32, @intCast(index))), proc, body);
    }
}

/// Production wrapper: certifies and panics on violation. Callers gate this
/// behind debug builds; release builds never run the certifier.
pub fn certifyStoreOrPanic(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
) Allocator.Error!void {
    var diag = Diagnostic{};
    certifyStore(allocator, store, layouts, sigs, &diag) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.Certification => {
            if (diag.context_proc) |proc_id| {
                dumpFailureContext(store, proc_id, diag.context_local);
            }
            std.debug.panic("ARC borrow certifier: {s}", .{diag.message()});
        },
    };
}

/// Prints every statement of the failing proc that mentions the implicated
/// local, plus all join/jump structure, as panic context.
fn dumpFailureContext(store: *const LirStore, proc_id: LIR.LirProcSpecId, local: ?LIR.LocalId) void {
    const proc = store.getProcSpec(proc_id);
    std.debug.print("\nARC certifier failure context: proc={d}", .{@intFromEnum(proc_id)});
    if (local) |l| {
        std.debug.print(" local={d} layout={d}", .{
            @intFromEnum(l),
            @intFromEnum(store.getLocal(l).layout_idx),
        });
    }
    std.debug.print("\n  args:", .{});
    for (store.getLocalSpan(proc.args)) |arg| std.debug.print(" {d}", .{@intFromEnum(arg)});
    std.debug.print("\n", .{});

    var reachable = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator);
    defer reachable.deinit();
    if (proc.body) |body| {
        var walk = std.ArrayList(LIR.CFStmtId).empty;
        defer walk.deinit(store.allocator);
        walk.append(store.allocator, body) catch return;
        while (walk.pop()) |current| {
            if (reachable.contains(current)) continue;
            reachable.put(current, {}) catch return;
            switch (store.getCFStmt(current)) {
                .runtime_error, .loop_continue, .loop_break, .jump, .ret, .crash => {},
                .switch_stmt => |s| {
                    for (store.getCFSwitchBranches(s.branches)) |branch| {
                        walk.append(store.allocator, branch.body) catch return;
                    }
                    walk.append(store.allocator, s.default_branch) catch return;
                },
                .join => |j| {
                    walk.append(store.allocator, j.body) catch return;
                    walk.append(store.allocator, j.remainder) catch return;
                },
                inline .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free => |s| {
                    walk.append(store.allocator, s.next) catch return;
                },
            }
        }
    }

    for (store.cf_stmts.items, 0..) |stmt, index| {
        if (!reachable.contains(@enumFromInt(@as(u32, @intCast(index))))) continue;
        const mentions = if (local) |l| stmtMentionsLocal(store, stmt, l) else false;
        const structural = switch (stmt) {
            .join, .jump => true,
            else => false,
        };
        if (!mentions and !structural) continue;
        std.debug.print("  stmt {d}: {s}", .{ index, @tagName(stmt) });
        switch (stmt) {
            .join => |j| std.debug.print(" id={d} body={d} remainder={d}", .{
                @intFromEnum(j.id), @intFromEnum(j.body), @intFromEnum(j.remainder),
            }),
            .jump => |j| std.debug.print(" target={d}", .{@intFromEnum(j.target)}),
            .assign_ref => |a| std.debug.print(" target={d} op={s} next={d}", .{
                @intFromEnum(a.target), @tagName(a.op), @intFromEnum(a.next),
            }),
            .set_local => |a| std.debug.print(" target={d} value={d} mode={s} next={d}", .{
                @intFromEnum(a.target), @intFromEnum(a.value), @tagName(a.mode), @intFromEnum(a.next),
            }),
            .incref => |rc| std.debug.print(" value={d} next={d}", .{ @intFromEnum(rc.value), @intFromEnum(rc.next) }),
            .decref => |rc| std.debug.print(" value={d} next={d}", .{ @intFromEnum(rc.value), @intFromEnum(rc.next) }),
            .free => |rc| std.debug.print(" value={d} next={d}", .{ @intFromEnum(rc.value), @intFromEnum(rc.next) }),
            .assign_call => |a| std.debug.print(" target={d} next={d}", .{ @intFromEnum(a.target), @intFromEnum(a.next) }),
            .assign_low_level => |a| std.debug.print(" target={d} op={s} next={d}", .{
                @intFromEnum(a.target), @tagName(a.op), @intFromEnum(a.next),
            }),
            .ret => |r| std.debug.print(" value={d}", .{@intFromEnum(r.value)}),
            inline .assign_literal, .assign_list, .assign_struct, .assign_tag, .assign_call_erased, .assign_packed_erased_fn => |a| std.debug.print(" target={d} next={d}", .{ @intFromEnum(a.target), @intFromEnum(a.next) }),
            else => {},
        }
        std.debug.print("\n", .{});
    }
}

fn stmtMentionsLocal(store: *const LirStore, stmt: LIR.CFStmt, needle: LIR.LocalId) bool {
    return switch (stmt) {
        .assign_ref => |a| a.target == needle or refOpReadsLocal(a.op, needle),
        .assign_literal => |a| a.target == needle,
        .assign_call => |a| a.target == needle or spanHasLocal(store, a.args, needle),
        .assign_call_erased => |a| a.target == needle or a.closure == needle or spanHasLocal(store, a.args, needle),
        .assign_packed_erased_fn => |a| a.target == needle or (a.capture != null and a.capture.? == needle),
        .assign_low_level => |a| a.target == needle or spanHasLocal(store, a.args, needle),
        .assign_list => |a| a.target == needle or spanHasLocal(store, a.elems, needle),
        .assign_struct => |a| a.target == needle or spanHasLocal(store, a.fields, needle),
        .assign_tag => |a| a.target == needle or (a.payload != null and a.payload.? == needle),
        .set_local => |a| a.target == needle or a.value == needle,
        .debug => |d| d.message == needle,
        .expect => |e| e.condition == needle,
        .incref => |rc| rc.value == needle,
        .decref => |rc| rc.value == needle,
        .free => |rc| rc.value == needle,
        .switch_stmt => |s| s.cond == needle,
        .ret => |r| r.value == needle,
        .join, .jump, .crash, .runtime_error, .loop_continue, .loop_break => false,
    };
}

fn spanHasLocal(store: *const LirStore, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
    for (store.getLocalSpan(span)) |local| {
        if (local == needle) return true;
    }
    return false;
}

const ValueId = u32;
const no_value: ValueId = std.math.maxInt(u32);
const no_dense: u32 = std.math.maxInt(u32);

/// Immutable per-value data shared by every forked state in one proc walk.
const ValueInfo = struct {
    /// First local bound to this value; used for stable cross-path naming.
    origin: LIR.LocalId,
    /// Values this value borrows from. The borrow is live only while every
    /// lender is reachable-live.
    lenders: []const ValueId,
    /// True for borrowed proc parameters: live for the whole call by ABI.
    always_live: bool,
};

/// One forked ownership state along a control-flow path.
const State = struct {
    allocator: Allocator,
    /// Value bound to each store local; `no_value` when unbound.
    local_value: []ValueId,
    /// Ownership units per value. Values created after a fork are absent in
    /// sibling states; absent means zero.
    balance: std.ArrayList(i32),
    /// Aggregate value currently holding a moved-in unit of this value, or
    /// `no_value`. Keeps consumed operands live until the holder dies.
    holder: std.ArrayList(ValueId),

    fn init(allocator: Allocator, local_count: usize) Allocator.Error!State {
        const local_value = try allocator.alloc(ValueId, local_count);
        @memset(local_value, no_value);
        return .{
            .allocator = allocator,
            .local_value = local_value,
            .balance = .empty,
            .holder = .empty,
        };
    }

    fn deinit(self: *State) void {
        self.allocator.free(self.local_value);
        self.balance.deinit(self.allocator);
        self.holder.deinit(self.allocator);
    }

    fn clone(self: *const State) Allocator.Error!State {
        const local_value = try self.allocator.dupe(ValueId, self.local_value);
        errdefer self.allocator.free(local_value);
        var balance = try self.balance.clone(self.allocator);
        errdefer balance.deinit(self.allocator);
        const holder = try self.holder.clone(self.allocator);
        return .{
            .allocator = self.allocator,
            .local_value = local_value,
            .balance = balance,
            .holder = holder,
        };
    }

    fn valueOf(self: *const State, local: LIR.LocalId) ValueId {
        return self.local_value[@intFromEnum(local)];
    }

    fn bindValue(self: *State, local: LIR.LocalId, value: ValueId) void {
        self.local_value[@intFromEnum(local)] = value;
    }

    fn balanceOf(self: *const State, value: ValueId) i32 {
        if (value >= self.balance.items.len) return 0;
        return self.balance.items[value];
    }

    fn holderOf(self: *const State, value: ValueId) ValueId {
        if (value >= self.holder.items.len) return no_value;
        return self.holder.items[value];
    }

    fn growToValue(self: *State, value: ValueId) Allocator.Error!void {
        while (self.balance.items.len <= value) {
            try self.balance.append(self.allocator, 0);
        }
        while (self.holder.items.len <= value) {
            try self.holder.append(self.allocator, no_value);
        }
    }

    fn addBalance(self: *State, value: ValueId, delta: i32) Allocator.Error!void {
        try self.growToValue(value);
        self.balance.items[value] += delta;
    }

    fn setHolder(self: *State, value: ValueId, holder_value: ValueId) Allocator.Error!void {
        try self.growToValue(value);
        self.holder.items[value] = holder_value;
    }
};

/// Per-proc-local quotient-state entry used to compare states at join points
/// and to deduplicate walks of shared statement chains. Indices are dense
/// proc-local positions, not store local ids.
const LocalSummary = struct {
    class: LocalClass,
    /// Lowest dense position bound to the same value (alias-set representative).
    repr: u32,
    /// Ownership units on the value (owned class only).
    balance: u32,
    /// For borrowed locals: dense position of the local anchoring the first
    /// unit-carrying value in the lender/holder chain. Equal to `repr` for
    /// ABI-borrowed parameters, which are self-anchored.
    lender_repr: u32,
};

const LocalClass = enum(u8) {
    unbound,
    owned,
    borrowed,
};

const JoinRecord = struct {
    body: LIR.CFStmtId,
    params: LIR.LocalSpan,
    /// Locals whose entry state the join body relies on: the join's
    /// parameters plus every local the body subtree reads before rebinding.
    /// Jump states must agree only on these; everything else was settled
    /// before the jump.
    relevant: std.bit_set.DynamicBitSetUnmanaged,
    /// Agreed state at every jump to this join; met with each further jump.
    expected: ?[]LocalSummary,
    body_scheduled: bool,
};

const MemoEntry = struct {
    stmt: u32,
    digest: u64,
};

const WorkItem = union(enum) {
    segment: Segment,
    join_body: LIR.JoinPointId,
};

const Segment = struct {
    cursor: LIR.CFStmtId,
    state: State,
};

const Certifier = struct {
    allocator: Allocator,
    store: *const LirStore,
    sigs: arc_sig.SigTable,
    rc_local: []const bool,
    values: std.ArrayList(ValueInfo) = .empty,
    lender_arena: std.heap.ArenaAllocator,
    records: std.AutoHashMap(LIR.JoinPointId, JoinRecord),
    memo: std.AutoHashMap(MemoEntry, void),
    summary_scratch: std.ArrayList(LocalSummary) = .empty,
    repr_scratch: std.AutoHashMap(ValueId, u32),
    /// Dense position per store local for the proc being certified, or
    /// `no_dense` for locals the proc never mentions.
    local_dense: std.ArrayList(u32) = .empty,
    /// Store local id per dense position.
    proc_locals: std.ArrayList(LIR.LocalId) = .empty,
    /// Join bodies of the proc being certified, for jump-following scans.
    join_bodies: std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId),
    /// Scratch bitset over store locals, reused by join-relevance extension.
    relevant_scratch: std.bit_set.DynamicBitSetUnmanaged = .{},
    /// Scratch storage reused by reachability scans.
    scan_visited: std.AutoHashMap(LIR.CFStmtId, void),
    scan_stack: std.ArrayList(LIR.CFStmtId) = .empty,
    diag: *Diagnostic,
    current_proc: LIR.LirProcSpecId = @enumFromInt(0),
    current_sig: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    current_stmt: LIR.CFStmtId = @enumFromInt(0),

    fn deinit(self: *Certifier) void {
        self.values.deinit(self.allocator);
        self.lender_arena.deinit();
        self.clearRecords();
        self.records.deinit();
        self.memo.deinit();
        self.summary_scratch.deinit(self.allocator);
        self.repr_scratch.deinit();
        self.local_dense.deinit(self.allocator);
        self.proc_locals.deinit(self.allocator);
        self.join_bodies.deinit();
        self.relevant_scratch.deinit(self.allocator);
        self.scan_visited.deinit();
        self.scan_stack.deinit(self.allocator);
    }

    fn clearRecords(self: *Certifier) void {
        var iter = self.records.valueIterator();
        while (iter.next()) |record| {
            if (record.expected) |expected| self.allocator.free(expected);
            record.relevant.deinit(self.allocator);
        }
        self.records.clearRetainingCapacity();
    }

    fn fail(self: *Certifier, comptime fmt: []const u8, args: anytype) error{Certification} {
        const full_args = .{
            @intFromEnum(self.current_proc),
            @intFromEnum(self.current_stmt),
        } ++ args;
        self.diag.set("proc={d} stmt={d}: " ++ fmt, full_args);
        return error.Certification;
    }

    fn isRc(self: *const Certifier, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.rc_local.len) return false;
        return self.rc_local[index];
    }

    fn denseOf(self: *const Certifier, local: LIR.LocalId) u32 {
        const index = @intFromEnum(local);
        if (index >= self.local_dense.items.len) return no_dense;
        return self.local_dense.items[index];
    }

    fn newValue(
        self: *Certifier,
        origin: LIR.LocalId,
        lenders: []const ValueId,
        always_live: bool,
    ) Allocator.Error!ValueId {
        const id: ValueId = @intCast(self.values.items.len);
        const stored_lenders: []const ValueId = if (lenders.len == 0)
            &.{}
        else
            try self.lender_arena.allocator().dupe(ValueId, lenders);
        try self.values.append(self.allocator, .{
            .origin = origin,
            .lenders = stored_lenders,
            .always_live = always_live,
        });
        return id;
    }

    fn bindFresh(
        self: *Certifier,
        state: *State,
        local: LIR.LocalId,
        units: i32,
        lenders: []const ValueId,
    ) CertifyError!ValueId {
        const value = try self.newValue(local, lenders, false);
        try state.addBalance(value, units);
        state.bindValue(local, value);
        return value;
    }

    /// Reports whether the value is reachable-live: it carries a unit, is an
    /// ABI-borrowed parameter, sits inside a live holder, or borrows from
    /// values that are all reachable-live.
    fn valueIsLive(self: *const Certifier, state: *const State, value: ValueId) bool {
        var stack_buffer: [64]ValueId = undefined;
        var stack_len: usize = 1;
        stack_buffer[0] = value;
        var checked: usize = 0;

        // A value with lenders is live only if every lender is live, so the
        // walk verifies all reachable requirements rather than searching for
        // one live anchor.
        while (stack_len > 0) {
            stack_len -= 1;
            const current = stack_buffer[stack_len];
            checked += 1;
            if (checked > 256) return false;
            if (current >= self.values.items.len) return false;
            const info = self.values.items[current];
            if (info.always_live) continue;
            if (state.balanceOf(current) > 0) continue;
            const holder = state.holderOf(current);
            if (holder != no_value) {
                if (stack_len >= stack_buffer.len) return false;
                stack_buffer[stack_len] = holder;
                stack_len += 1;
                continue;
            }
            if (info.lenders.len == 0) return false;
            for (info.lenders) |lender| {
                if (stack_len >= stack_buffer.len) return false;
                stack_buffer[stack_len] = lender;
                stack_len += 1;
            }
        }
        return true;
    }

    fn requireLive(self: *Certifier, state: *const State, local: LIR.LocalId) CertifyError!ValueId {
        if (!self.isRc(local)) return no_value;
        const value = state.valueOf(local);
        if (value == no_value) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("use of unbound refcounted local {d}", .{@intFromEnum(local)});
        }
        if (!self.valueIsLive(state, value)) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("use of dead refcounted local {d}", .{@intFromEnum(local)});
        }
        return value;
    }

    /// Strict consumption: a transferred unit must exist when it leaves this
    /// proc's hands (call arguments, consumed low-level arguments, returns).
    fn consumeUnit(self: *Certifier, state: *State, value: ValueId, local: LIR.LocalId) CertifyError!void {
        if (value == no_value) return;
        if (state.balanceOf(value) < 1) {
            return self.fail("consumed local {d} without an ownership unit", .{@intFromEnum(local)});
        }
        try state.addBalance(value, -1);
    }

    /// Aggregate consumption: one unit moves into the holder. The emitted
    /// trailing incref restores the operand's own unit, so the balance may go
    /// transiently negative here; the per-path terminal balance check flags a
    /// missing restore.
    fn consumeIntoHolder(
        _: *Certifier,
        state: *State,
        value: ValueId,
        holder_value: ValueId,
    ) CertifyError!void {
        if (value == no_value) return;
        try state.addBalance(value, -1);
        if (holder_value != no_value) {
            try state.setHolder(value, holder_value);
        }
    }

    fn checkLeaks(self: *Certifier, state: *const State) CertifyError!void {
        for (state.balance.items, 0..) |units, value_index| {
            if (units == 0) continue;
            const origin = self.values.items[value_index].origin;
            if (units > 0) {
                return self.fail(
                    "leaked {d} ownership unit(s) of value originating at local {d}",
                    .{ units, @intFromEnum(origin) },
                );
            }
            return self.fail(
                "negative ownership balance for value originating at local {d}",
                .{@intFromEnum(origin)},
            );
        }
    }

    /// Builds the per-proc-local quotient summary of a state into scratch
    /// storage. The returned slice is invalidated by the next call.
    fn summarize(self: *Certifier, state: *const State) Allocator.Error![]const LocalSummary {
        self.repr_scratch.clearRetainingCapacity();
        self.summary_scratch.clearRetainingCapacity();
        try self.summary_scratch.ensureTotalCapacity(self.allocator, self.proc_locals.items.len);

        for (self.proc_locals.items, 0..) |local, dense| {
            if (!self.isRc(local)) continue;
            const value = state.valueOf(local);
            if (value == no_value) continue;
            const entry = try self.repr_scratch.getOrPut(value);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
        }

        for (self.proc_locals.items) |local| {
            var summary = LocalSummary{ .class = .unbound, .repr = 0, .balance = 0, .lender_repr = 0 };
            if (self.isRc(local)) {
                const value = state.valueOf(local);
                if (value != no_value) {
                    const repr = self.repr_scratch.get(value) orelse 0;
                    const units = state.balanceOf(value);
                    if (units > 0) {
                        summary = .{ .class = .owned, .repr = repr, .balance = @intCast(units), .lender_repr = 0 };
                    } else if (self.valueIsLive(state, value)) {
                        summary = .{
                            .class = .borrowed,
                            .repr = repr,
                            .balance = 0,
                            .lender_repr = self.liveAnchorRepr(state, value),
                        };
                    }
                }
            }
            self.summary_scratch.appendAssumeCapacity(summary);
        }

        return self.summary_scratch.items;
    }

    /// Returns the dense position anchoring the first unit-carrying (or
    /// ABI-borrowed) value reached through lender/holder links, for stable
    /// cross-path naming of where a borrow takes its liveness from.
    fn liveAnchorRepr(self: *const Certifier, state: *const State, value: ValueId) u32 {
        var current = value;
        var steps: usize = 0;
        while (steps < 256) : (steps += 1) {
            if (current >= self.values.items.len) return 0;
            const info = self.values.items[current];
            if (info.always_live or state.balanceOf(current) > 0) {
                if (self.repr_scratch.get(current)) |repr| return repr;
                return self.denseOf(info.origin);
            }
            const holder = state.holderOf(current);
            if (holder != no_value) {
                current = holder;
                continue;
            }
            if (info.lenders.len == 0) return 0;
            current = info.lenders[0];
        }
        return 0;
    }

    fn summaryDigest(cursor: LIR.CFStmtId, summary: []const LocalSummary) u64 {
        var hasher = std.hash.Wyhash.init(0x6172635f63657274);
        hasher.update(std.mem.asBytes(&cursor));
        for (summary) |entry| {
            hasher.update(std.mem.asBytes(&entry.class));
            hasher.update(std.mem.asBytes(&entry.repr));
            hasher.update(std.mem.asBytes(&entry.balance));
            hasher.update(std.mem.asBytes(&entry.lender_repr));
        }
        return hasher.final();
    }

    const MeetOutcome = enum {
        equal,
        weakened,
        conflict,
    };

    /// Computes the meet of the join's agreed entry state with one more
    /// jump's state, in place.
    ///
    /// A name live on one side and unbound on the other weakens to unbound:
    /// the body is re-certified under the weaker assumption, so a body that
    /// relies on the name is flagged when it reads or releases it. Names live
    /// on both sides must agree exactly on ownership units and class, and
    /// alias partitions are compared structurally (two names share a value on
    /// one side iff they share on the other), because representative indices
    /// drift when stale alias names drop out on one path.
    fn meetSummaries(
        self: *Certifier,
        expected: []LocalSummary,
        incoming: []const LocalSummary,
        conflict_dense: *usize,
    ) Allocator.Error!MeetOutcome {
        if (expected.len != incoming.len) {
            conflict_dense.* = 0;
            return .conflict;
        }

        // Compare classes over names live on both sides, building a
        // partition correspondence keyed by expected-side representative.
        self.repr_scratch.clearRetainingCapacity();
        for (expected, incoming, 0..) |left, right, dense| {
            if (left.class == .unbound or right.class == .unbound) continue;
            if (left.class != right.class or left.balance != right.balance) {
                conflict_dense.* = dense;
                return .conflict;
            }
            const pair = try self.repr_scratch.getOrPut(left.repr);
            if (!pair.found_existing) {
                pair.value_ptr.* = right.repr;
            } else if (pair.value_ptr.* != right.repr) {
                conflict_dense.* = dense;
                return .conflict;
            }
        }
        // Borrow anchors must correspond through the same partition map when
        // the anchor class is live on both sides.
        for (expected, incoming, 0..) |left, right, dense| {
            if (left.class != .borrowed or right.class != .borrowed) continue;
            if (self.repr_scratch.get(left.lender_repr)) |mapped| {
                if (mapped != right.lender_repr) {
                    conflict_dense.* = dense;
                    return .conflict;
                }
            }
        }

        // Weaken one-sided names to unbound.
        var weakened = false;
        for (expected, incoming) |*left, right| {
            const left_live = left.class != .unbound;
            const right_live = right.class != .unbound;
            if (left_live == right_live) continue;
            if (left_live) {
                left.* = .{ .class = .unbound, .repr = 0, .balance = 0, .lender_repr = 0 };
            }
            weakened = true;
        }
        if (!weakened) return .equal;

        // Re-canonicalize representatives among the remaining live names,
        // and drop borrows whose anchor class lost every live name.
        self.repr_scratch.clearRetainingCapacity();
        for (expected, 0..) |*entry, dense| {
            if (entry.class == .unbound) continue;
            const pair = try self.repr_scratch.getOrPut(entry.repr);
            if (!pair.found_existing) pair.value_ptr.* = @intCast(dense);
            entry.repr = pair.value_ptr.*;
        }
        for (expected) |*entry| {
            if (entry.class != .borrowed) continue;
            if (self.repr_scratch.get(entry.lender_repr)) |mapped| {
                entry.lender_repr = mapped;
            } else if (entry.lender_repr != entry.repr) {
                entry.* = .{ .class = .unbound, .repr = 0, .balance = 0, .lender_repr = 0 };
            }
        }
        return .weakened;
    }

    /// Rebuilds a fresh state from an agreed join-entry summary. Alias sets
    /// share one fresh value; borrows are re-linked to the fresh value of the
    /// local their liveness anchored on.
    fn stateFromSummary(self: *Certifier, summary: []const LocalSummary) CertifyError!State {
        var state = try State.init(self.allocator, self.store.locals.items.len);
        errdefer state.deinit();

        for (summary, 0..) |entry, dense| {
            if (entry.class != .owned or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            _ = try self.bindFresh(&state, local, @intCast(entry.balance), &.{});
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .owned or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueOf(self.proc_locals.items[entry.repr]));
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            if (entry.lender_repr == dense) {
                // Self-anchored borrow: an ABI-borrowed parameter, live for
                // the whole call.
                const value = try self.newValue(local, &.{}, true);
                try state.growToValue(value);
                state.bindValue(local, value);
                continue;
            }
            const anchor_dense: usize = entry.lender_repr;
            if (anchor_dense >= self.proc_locals.items.len) {
                return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
            }
            const anchor_local = self.proc_locals.items[anchor_dense];
            const lender = state.valueOf(anchor_local);
            if (lender == no_value) {
                return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
            }
            _ = try self.bindFresh(&state, local, 0, &.{lender});
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueOf(self.proc_locals.items[entry.repr]));
        }
        return state;
    }

    fn collectProcLocals(self: *Certifier, proc: LIR.LirProcSpec, body: LIR.CFStmtId) Allocator.Error!void {
        self.proc_locals.clearRetainingCapacity();
        self.local_dense.clearRetainingCapacity();
        try self.local_dense.ensureTotalCapacity(self.allocator, self.store.locals.items.len);
        self.local_dense.items.len = self.store.locals.items.len;
        @memset(self.local_dense.items, no_dense);

        for (self.store.getLocalSpan(proc.args)) |param| {
            try self.noteProcLocal(param);
        }

        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, body);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    try self.noteProcLocal(assign.target);
                    switch (assign.op) {
                        .local => |source| try self.noteProcLocal(source),
                        .discriminant => |op| try self.noteProcLocal(op.source),
                        .field => |op| try self.noteProcLocal(op.source),
                        .tag_payload => |op| try self.noteProcLocal(op.source),
                        .tag_payload_struct => |op| try self.noteProcLocal(op.source),
                        .list_reinterpret => |op| try self.noteProcLocal(op.backing_ref),
                        .nominal => |op| try self.noteProcLocal(op.backing_ref),
                    }
                    try stack.append(self.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    try self.noteProcLocal(assign.target);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_call => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.closure);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.capture) |capture| try self.noteProcLocal(capture);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_list => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.elems);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.fields);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.payload) |payload| try self.noteProcLocal(payload);
                    try stack.append(self.allocator, assign.next);
                },
                .set_local => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.value);
                    try stack.append(self.allocator, assign.next);
                },
                .debug => |debug_stmt| {
                    try self.noteProcLocal(debug_stmt.message);
                    try stack.append(self.allocator, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    try self.noteProcLocal(expect_stmt.condition);
                    try stack.append(self.allocator, expect_stmt.next);
                },
                .incref => |rc| {
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .decref => |rc| {
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .free => |rc| {
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .switch_stmt => |switch_stmt| {
                    try self.noteProcLocal(switch_stmt.cond);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.allocator, branch.body);
                    }
                    try stack.append(self.allocator, switch_stmt.default_branch);
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.allocator, continuation);
                    }
                },
                .join => |join_stmt| {
                    try self.noteProcLocalSpan(join_stmt.params);
                    try self.join_bodies.put(join_stmt.id, join_stmt.body);
                    try stack.append(self.allocator, join_stmt.body);
                    try stack.append(self.allocator, join_stmt.remainder);
                },
                .ret => |ret_stmt| try self.noteProcLocal(ret_stmt.value),
                .jump, .crash, .runtime_error, .loop_continue, .loop_break => {},
            }
        }
    }

    /// Mirrors ARC insertion's liveness scan: reports whether `needle` is
    /// read starting from `start` before being rebound on that path. Jumps
    /// are followed into join bodies.
    fn usedBeforeRebind(self: *Certifier, start: LIR.CFStmtId, needle: LIR.LocalId) Allocator.Error!bool {
        self.scan_visited.clearRetainingCapacity();
        self.scan_stack.clearRetainingCapacity();
        try self.scan_stack.append(self.allocator, start);

        while (self.scan_stack.pop()) |current| {
            if (self.scan_visited.contains(current)) continue;
            try self.scan_visited.put(current, {});

            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    if (refOpReadsLocal(assign.op, needle)) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_call => |assign| {
                    if (self.spanReadsLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    if (assign.closure == needle or self.spanReadsLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture != null and assign.capture.? == needle) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    if (self.spanReadsLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_list => |assign| {
                    if (self.spanReadsLocal(assign.elems, needle)) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    if (self.spanReadsLocal(assign.fields, needle)) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload != null and assign.payload.? == needle) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.value == needle) return true;
                    if (assign.target == needle) continue;
                    try self.scan_stack.append(self.allocator, assign.next);
                },
                .debug => |debug_stmt| {
                    if (debug_stmt.message == needle) return true;
                    try self.scan_stack.append(self.allocator, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    if (expect_stmt.condition == needle) return true;
                    try self.scan_stack.append(self.allocator, expect_stmt.next);
                },
                .incref => |rc| {
                    if (rc.value == needle) return true;
                    try self.scan_stack.append(self.allocator, rc.next);
                },
                .decref => |rc| {
                    if (rc.value == needle) return true;
                    try self.scan_stack.append(self.allocator, rc.next);
                },
                .free => |rc| {
                    if (rc.value == needle) return true;
                    try self.scan_stack.append(self.allocator, rc.next);
                },
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.cond == needle) return true;
                    if (switch_stmt.continuation) |continuation| {
                        try self.scan_stack.append(self.allocator, continuation);
                    }
                    try self.scan_stack.append(self.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try self.scan_stack.append(self.allocator, branch.body);
                    }
                },
                .join => |join_stmt| {
                    try self.scan_stack.append(self.allocator, join_stmt.remainder);
                    try self.scan_stack.append(self.allocator, join_stmt.body);
                },
                .jump => |jump_stmt| {
                    if (self.join_bodies.get(jump_stmt.target)) |target_body| {
                        try self.scan_stack.append(self.allocator, target_body);
                    }
                },
                .ret => |ret_stmt| {
                    if (ret_stmt.value == needle) return true;
                },
                .runtime_error, .crash, .loop_continue, .loop_break => {},
            }
        }
        return false;
    }

    fn spanReadsLocal(self: *const Certifier, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
        for (self.store.getLocalSpan(span)) |local| {
            if (local == needle) return true;
        }
        return false;
    }

    /// Computes the join's relevant-local set: its parameters plus every
    /// refcounted proc local the body subtree reads before rebinding.
    fn computeJoinRelevant(
        self: *Certifier,
        params: LIR.LocalSpan,
        body: LIR.CFStmtId,
    ) CertifyError!std.bit_set.DynamicBitSetUnmanaged {
        var relevant = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.store.locals.items.len);
        errdefer relevant.deinit(self.allocator);
        for (self.store.getLocalSpan(params)) |param| {
            if (self.isRc(param)) relevant.set(@intFromEnum(param));
        }
        for (self.proc_locals.items) |local| {
            if (!self.isRc(local)) continue;
            if (relevant.isSet(@intFromEnum(local))) continue;
            if (try self.usedBeforeRebind(body, local)) {
                relevant.set(@intFromEnum(local));
            }
        }
        return relevant;
    }

    /// Returns the first unit-carrying or ABI-borrowed value reached through
    /// lender/holder links, or `no_value` when the chain is dead.
    fn liveAnchorValue(self: *const Certifier, state: *const State, value: ValueId) ValueId {
        var current = value;
        var steps: usize = 0;
        while (steps < 256) : (steps += 1) {
            if (current >= self.values.items.len) return no_value;
            const info = self.values.items[current];
            if (info.always_live or state.balanceOf(current) > 0) return current;
            const holder = state.holderOf(current);
            if (holder != no_value) {
                current = holder;
                continue;
            }
            if (info.lenders.len == 0) return no_value;
            current = info.lenders[0];
        }
        return no_value;
    }

    /// Builds the jump-state summary restricted to the join's relevant
    /// locals, extending relevance through the lender anchors of relevant
    /// borrows, and verifies every outstanding ownership unit is carried into
    /// the join through a relevant local.
    fn summarizeForJoin(
        self: *Certifier,
        state: *const State,
        record: *const JoinRecord,
        join_id: LIR.JoinPointId,
    ) CertifyError![]const LocalSummary {
        // Seed the working relevant set from the record.
        var index: usize = 0;
        while (index < self.relevant_scratch.capacity()) : (index += 1) {
            self.relevant_scratch.setValue(index, record.relevant.isSet(index));
        }

        // Extend through borrow anchors: a relevant borrowed local keeps its
        // lender's carrier local live, so the carrier joins the agreement.
        var changed = true;
        var rounds: usize = 0;
        while (changed and rounds < 64) : (rounds += 1) {
            changed = false;
            for (self.proc_locals.items) |local| {
                const local_index = @intFromEnum(local);
                if (!self.relevant_scratch.isSet(local_index)) continue;
                if (!self.isRc(local)) continue;
                const value = state.valueOf(local);
                if (value == no_value) continue;
                if (state.balanceOf(value) > 0) continue;
                const anchor = self.liveAnchorValue(state, value);
                if (anchor == no_value) continue;
                // Find a carrier local for the anchor value.
                var carrier: u32 = no_dense;
                for (self.proc_locals.items) |candidate| {
                    if (state.valueOf(candidate) == anchor) {
                        carrier = @intFromEnum(candidate);
                        break;
                    }
                }
                if (carrier == no_dense) continue;
                if (!self.relevant_scratch.isSet(carrier)) {
                    self.relevant_scratch.set(carrier);
                    changed = true;
                }
            }
        }

        // Build the restricted summary.
        self.repr_scratch.clearRetainingCapacity();
        self.summary_scratch.clearRetainingCapacity();
        try self.summary_scratch.ensureTotalCapacity(self.allocator, self.proc_locals.items.len);

        for (self.proc_locals.items, 0..) |local, dense| {
            if (!self.isRc(local)) continue;
            if (!self.relevant_scratch.isSet(@intFromEnum(local))) continue;
            const value = state.valueOf(local);
            if (value == no_value) continue;
            const entry = try self.repr_scratch.getOrPut(value);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
        }

        for (self.proc_locals.items) |local| {
            var summary = LocalSummary{ .class = .unbound, .repr = 0, .balance = 0, .lender_repr = 0 };
            if (self.isRc(local) and self.relevant_scratch.isSet(@intFromEnum(local))) {
                const value = state.valueOf(local);
                if (value != no_value) {
                    const repr = self.repr_scratch.get(value) orelse 0;
                    const units = state.balanceOf(value);
                    if (units > 0) {
                        summary = .{ .class = .owned, .repr = repr, .balance = @intCast(units), .lender_repr = 0 };
                    } else if (self.valueIsLive(state, value)) {
                        summary = .{
                            .class = .borrowed,
                            .repr = repr,
                            .balance = 0,
                            .lender_repr = self.liveAnchorRepr(state, value),
                        };
                    }
                }
            }
            self.summary_scratch.appendAssumeCapacity(summary);
        }

        // Every outstanding ownership unit must be carried into the join by
        // a relevant local; anything else can never be released again.
        for (state.balance.items, 0..) |units, value_index| {
            if (units == 0) continue;
            const origin = self.values.items[value_index].origin;
            if (units < 0) {
                return self.fail(
                    "negative ownership balance for value originating at local {d} at jump to join {d}",
                    .{ @intFromEnum(origin), @intFromEnum(join_id) },
                );
            }
            if (!self.repr_scratch.contains(@intCast(value_index))) {
                return self.fail(
                    "ownership unit of value originating at local {d} not carried into join {d}",
                    .{ @intFromEnum(origin), @intFromEnum(join_id) },
                );
            }
        }

        return self.summary_scratch.items;
    }

    fn noteProcLocal(self: *Certifier, local: LIR.LocalId) Allocator.Error!void {
        const index = @intFromEnum(local);
        if (index >= self.local_dense.items.len) return;
        if (self.local_dense.items[index] != no_dense) return;
        self.local_dense.items[index] = @intCast(self.proc_locals.items.len);
        try self.proc_locals.append(self.allocator, local);
    }

    fn noteProcLocalSpan(self: *Certifier, span: LIR.LocalSpan) Allocator.Error!void {
        for (self.store.getLocalSpan(span)) |local| {
            try self.noteProcLocal(local);
        }
    }

    fn certifyProc(
        self: *Certifier,
        proc_id: LIR.LirProcSpecId,
        proc: LIR.LirProcSpec,
        body: LIR.CFStmtId,
    ) CertifyError!void {
        self.current_proc = proc_id;
        self.current_sig = self.sigs.get(proc_id);
        self.values.clearRetainingCapacity();
        _ = self.lender_arena.reset(.retain_capacity);
        self.clearRecords();
        self.memo.clearRetainingCapacity();
        self.join_bodies.clearRetainingCapacity();
        try self.collectProcLocals(proc, body);
        try self.relevant_scratch.resize(self.allocator, self.store.locals.items.len, false);

        var state = try State.init(self.allocator, self.store.locals.items.len);
        {
            errdefer state.deinit();
            for (self.store.getLocalSpan(proc.args), 0..) |param, index| {
                if (!self.isRc(param)) continue;
                switch (self.current_sig.paramMode(index)) {
                    .owned => _ = try self.bindFresh(&state, param, 1, &.{}),
                    .borrowed => {
                        const value = try self.newValue(param, &.{}, true);
                        try state.growToValue(value);
                        state.bindValue(param, value);
                    },
                }
            }
        }

        var work = std.ArrayList(WorkItem).empty;
        defer {
            while (work.pop()) |item| {
                switch (item) {
                    .segment => |segment| {
                        var owned_state = segment.state;
                        owned_state.deinit();
                    },
                    .join_body => {},
                }
            }
            work.deinit(self.allocator);
        }

        try work.append(self.allocator, .{ .segment = .{ .cursor = body, .state = state } });
        while (work.pop()) |item| {
            switch (item) {
                .segment => |segment| try self.runSegment(&work, segment),
                .join_body => |join_id| try self.scheduleJoinBody(&work, join_id),
            }
        }
    }

    fn scheduleJoinBody(self: *Certifier, work: *std.ArrayList(WorkItem), join_id: LIR.JoinPointId) CertifyError!void {
        const record = self.records.getPtr(join_id) orelse return;
        const expected = record.expected orelse return;
        var body_state = try self.stateFromSummary(expected);
        errdefer body_state.deinit();
        try work.append(self.allocator, .{ .segment = .{ .cursor = record.body, .state = body_state } });
    }

    fn runSegment(self: *Certifier, work: *std.ArrayList(WorkItem), segment: Segment) CertifyError!void {
        var state = segment.state;
        defer state.deinit();
        var cursor = segment.cursor;

        while (true) {
            self.current_stmt = cursor;

            const summary = try self.summarize(&state);
            const memo_entry = MemoEntry{ .stmt = @intFromEnum(cursor), .digest = summaryDigest(cursor, summary) };
            const seen = try self.memo.getOrPut(memo_entry);
            if (seen.found_existing) return;

            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                _ = try self.requireLive(&state, source);
                                if (self.isRc(assign.target)) {
                                    state.bindValue(assign.target, state.valueOf(source));
                                }
                            }
                        },
                        .discriminant => |op| _ = try self.requireLive(&state, op.source),
                        .field => |op| try self.bindPayloadRead(&state, assign.target, op.source),
                        .tag_payload => |op| try self.bindPayloadRead(&state, assign.target, op.source),
                        .tag_payload_struct => |op| try self.bindPayloadRead(&state, assign.target, op.source),
                        .list_reinterpret => |op| try self.bindSameValue(&state, assign.target, op.backing_ref),
                        .nominal => |op| try self.bindSameValue(&state, assign.target, op.backing_ref),
                    }
                    cursor = assign.next;
                },
                .assign_literal => |assign| {
                    if (self.isRc(assign.target)) {
                        _ = try self.bindFresh(&state, assign.target, 1, &.{});
                    }
                    cursor = assign.next;
                },
                .assign_call => |assign| {
                    try self.applyCall(&state, assign.target, self.sigs.get(assign.proc), assign.args);
                    cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    _ = try self.requireLive(&state, assign.closure);
                    try self.applyCall(&state, assign.target, arc_sig.RcSig.all_owned, assign.args);
                    cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    const capture_value = if (assign.capture) |capture|
                        try self.requireLive(&state, capture)
                    else
                        no_value;
                    if (self.isRc(assign.target)) {
                        const target_value = try self.bindFresh(&state, assign.target, 1, &.{});
                        if (assign.capture != null) {
                            try self.consumeIntoHolder(&state, capture_value, target_value);
                        }
                    } else if (assign.capture != null) {
                        try self.consumeIntoHolder(&state, capture_value, no_value);
                    }
                    cursor = assign.next;
                },
                .assign_low_level => |assign| {
                    try self.applyLowLevel(&state, assign);
                    cursor = assign.next;
                },
                .assign_list => |assign| {
                    try self.applyAggregate(&state, assign.target, self.store.getLocalSpan(assign.elems));
                    cursor = assign.next;
                },
                .assign_struct => |assign| {
                    try self.applyAggregate(&state, assign.target, self.store.getLocalSpan(assign.fields));
                    cursor = assign.next;
                },
                .assign_tag => |assign| {
                    if (assign.payload) |payload| {
                        try self.applyAggregate(&state, assign.target, &.{payload});
                    } else {
                        try self.applyAggregate(&state, assign.target, &.{});
                    }
                    cursor = assign.next;
                },
                .set_local => |assign| {
                    if (assign.target != assign.value) {
                        _ = try self.requireLive(&state, assign.value);
                        if (self.isRc(assign.target)) {
                            state.bindValue(assign.target, state.valueOf(assign.value));
                        }
                    }
                    cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    _ = try self.requireLive(&state, debug_stmt.message);
                    cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    _ = try self.requireLive(&state, expect_stmt.condition);
                    cursor = expect_stmt.next;
                },
                .incref => |rc| {
                    if (!self.isRc(rc.value)) {
                        return self.fail("incref of non-refcounted local {d}", .{@intFromEnum(rc.value)});
                    }
                    const value = try self.requireLive(&state, rc.value);
                    try state.addBalance(value, rc.count);
                    cursor = rc.next;
                },
                .decref => |rc| {
                    try self.applyRelease(&state, rc.value);
                    cursor = rc.next;
                },
                .free => |rc| {
                    try self.applyRelease(&state, rc.value);
                    cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    _ = try self.requireLive(&state, switch_stmt.cond);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        var branch_state = try state.clone();
                        errdefer branch_state.deinit();
                        try work.append(self.allocator, .{ .segment = .{ .cursor = branch.body, .state = branch_state } });
                    }
                    var default_state = try state.clone();
                    errdefer default_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = switch_stmt.default_branch, .state = default_state } });
                    return;
                },
                .join => |join_stmt| {
                    const record = try self.records.getOrPut(join_stmt.id);
                    if (record.found_existing) {
                        if (record.value_ptr.body != join_stmt.body) {
                            return self.fail("join {d} redefined with a different body", .{@intFromEnum(join_stmt.id)});
                        }
                    } else {
                        record.value_ptr.* = .{
                            .body = join_stmt.body,
                            .params = join_stmt.params,
                            .relevant = try self.computeJoinRelevant(join_stmt.params, join_stmt.body),
                            .expected = null,
                            .body_scheduled = false,
                        };
                    }
                    if (!record.value_ptr.body_scheduled) {
                        record.value_ptr.body_scheduled = true;
                        try work.append(self.allocator, .{ .join_body = join_stmt.id });
                    }
                    cursor = join_stmt.remainder;
                },
                .jump => |jump_stmt| {
                    const record = self.records.getPtr(jump_stmt.target) orelse {
                        return self.fail("jump to join {d} before its definition", .{@intFromEnum(jump_stmt.target)});
                    };
                    const jump_summary = try self.summarizeForJoin(&state, record, jump_stmt.target);
                    if (record.expected) |expected| {
                        var conflict_dense: usize = 0;
                        switch (try self.meetSummaries(expected, jump_summary, &conflict_dense)) {
                            .equal => {},
                            .weakened => {
                                // The body must hold under the weaker entry
                                // assumption; certify it again.
                                try work.append(self.allocator, .{ .join_body = jump_stmt.target });
                            },
                            .conflict => {
                                const local = self.proc_locals.items[conflict_dense];
                                const left = expected[conflict_dense];
                                const right = jump_summary[conflict_dense];
                                self.diag.context_local = local;
                                self.diag.context_proc = self.current_proc;
                                return self.fail(
                                    "jumps to join {d} disagree on ownership of local {d}: " ++
                                        "({s} repr={d} units={d} anchor={d}) vs ({s} repr={d} units={d} anchor={d})",
                                    .{
                                        @intFromEnum(jump_stmt.target),
                                        @intFromEnum(local),
                                        @tagName(left.class),
                                        left.repr,
                                        left.balance,
                                        left.lender_repr,
                                        @tagName(right.class),
                                        right.repr,
                                        right.balance,
                                        right.lender_repr,
                                    },
                                );
                            },
                        }
                    } else {
                        record.expected = try self.allocator.dupe(LocalSummary, jump_summary);
                    }
                    return;
                },
                .ret => |ret_stmt| {
                    if (self.isRc(ret_stmt.value)) {
                        const value = try self.requireLive(&state, ret_stmt.value);
                        switch (self.current_sig.ret_mode) {
                            .owned => try self.consumeUnit(&state, value, ret_stmt.value),
                            .borrowed => {},
                        }
                    }
                    try self.checkLeaks(&state);
                    return;
                },
                .crash => {
                    try self.checkLeaks(&state);
                    return;
                },
                .runtime_error => {
                    try self.checkLeaks(&state);
                    return;
                },
                .loop_continue, .loop_break => {
                    // Control returns to an enclosing iteration engine that
                    // owns the kept values; per-path balance checking resumes
                    // at the statements that follow the engine.
                    return;
                },
            }
        }
    }

    fn bindPayloadRead(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId) CertifyError!void {
        const source_value = try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        if (source_value == no_value) {
            return self.fail(
                "payload read into refcounted local {d} from non-refcounted source {d}",
                .{ @intFromEnum(target), @intFromEnum(source) },
            );
        }
        _ = try self.bindFresh(state, target, 0, &.{source_value});
    }

    fn bindSameValue(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId) CertifyError!void {
        const source_value = try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        if (source_value == no_value) {
            return self.fail(
                "reinterpret into refcounted local {d} from non-refcounted source {d}",
                .{ @intFromEnum(target), @intFromEnum(source) },
            );
        }
        state.bindValue(target, source_value);
    }

    fn applyRelease(self: *Certifier, state: *State, local: LIR.LocalId) CertifyError!void {
        if (!self.isRc(local)) {
            return self.fail("release of non-refcounted local {d}", .{@intFromEnum(local)});
        }
        const value = state.valueOf(local);
        if (value == no_value) {
            return self.fail("release of unbound local {d}", .{@intFromEnum(local)});
        }
        if (state.balanceOf(value) < 1) {
            return self.fail("release of local {d} without an ownership unit", .{@intFromEnum(local)});
        }
        try state.addBalance(value, -1);
    }

    fn applyCall(
        self: *Certifier,
        state: *State,
        target: LIR.LocalId,
        callee_sig: arc_sig.RcSig,
        args: LIR.LocalSpan,
    ) CertifyError!void {
        const arg_locals = self.store.getLocalSpan(args);

        var arg_values_buffer: [64]ValueId = undefined;
        for (arg_locals, 0..) |arg, index| {
            const value = try self.requireLive(state, arg);
            if (index < arg_values_buffer.len) arg_values_buffer[index] = value;
        }

        for (arg_locals, 0..) |arg, index| {
            if (!self.isRc(arg)) continue;
            switch (callee_sig.paramMode(index)) {
                .owned => {
                    const value = if (index < arg_values_buffer.len)
                        arg_values_buffer[index]
                    else
                        state.valueOf(arg);
                    try self.consumeUnit(state, value, arg);
                },
                .borrowed => {},
            }
        }

        if (self.isRc(target)) {
            switch (callee_sig.ret_mode) {
                .owned => _ = try self.bindFresh(state, target, 1, &.{}),
                .borrowed => {
                    var lenders_buffer: [64]ValueId = undefined;
                    var lender_count: usize = 0;
                    for (arg_locals, 0..) |arg, index| {
                        if (index >= 64) break;
                        const bit = @as(u64, 1) << @as(u6, @intCast(index));
                        if ((callee_sig.ret_lenders & bit) == 0) continue;
                        if (!self.isRc(arg)) continue;
                        const value = arg_values_buffer[index];
                        if (value == no_value) continue;
                        lenders_buffer[lender_count] = value;
                        lender_count += 1;
                    }
                    _ = try self.bindFresh(state, target, 0, lenders_buffer[0..lender_count]);
                },
            }
        }
    }

    fn applyLowLevel(self: *Certifier, state: *State, assign: anytype) CertifyError!void {
        const arg_locals = self.store.getLocalSpan(assign.args);

        var arg_values_buffer: [64]ValueId = undefined;
        for (arg_locals, 0..) |arg, index| {
            const value = try self.requireLive(state, arg);
            if (index < arg_values_buffer.len) arg_values_buffer[index] = value;
        }

        // Consumed positions transfer one unit each into the op.
        for (arg_locals, 0..) |arg, index| {
            if (index >= 64) break;
            if (!self.isRc(arg)) continue;
            const bit = @as(u64, 1) << @as(u6, @intCast(index));
            if ((assign.rc_effect.consume_args & bit) == 0) continue;
            try self.consumeUnit(state, arg_values_buffer[index], arg);
        }

        var target_value: ValueId = no_value;
        if (self.isRc(assign.target)) {
            if (assign.rc_effect.retain_result) {
                // The result reads payload data out of the op's refcounted
                // arguments; it borrows until the trailing incref lands.
                var lenders_buffer: [64]ValueId = undefined;
                var lender_count: usize = 0;
                for (arg_locals, 0..) |arg, index| {
                    if (index >= lenders_buffer.len) break;
                    if (!self.isRc(arg)) continue;
                    lenders_buffer[lender_count] = arg_values_buffer[index];
                    lender_count += 1;
                }
                if (lender_count == 0) {
                    // The payload source is implicit (the executing frame's
                    // capture environment); it is live for the whole call.
                    target_value = try self.newValue(assign.target, &.{}, true);
                    try state.growToValue(target_value);
                    state.bindValue(assign.target, target_value);
                } else {
                    target_value = try self.bindFresh(state, assign.target, 0, lenders_buffer[0..lender_count]);
                }
            } else {
                target_value = try self.bindFresh(state, assign.target, 1, &.{});
            }
        }

        // Retained positions are stored inside the result; the trailing
        // incref restores the unit the op moved into its result.
        for (arg_locals, 0..) |arg, index| {
            if (index >= 64) break;
            if (!self.isRc(arg)) continue;
            const bit = @as(u64, 1) << @as(u6, @intCast(index));
            if ((assign.rc_effect.retain_args & bit) == 0) continue;
            try self.consumeIntoHolder(state, arg_values_buffer[index], target_value);
        }
    }

    fn applyAggregate(
        self: *Certifier,
        state: *State,
        target: LIR.LocalId,
        operands: []const LIR.LocalId,
    ) CertifyError!void {
        var operand_values_buffer: [64]ValueId = undefined;
        for (operands, 0..) |operand, index| {
            const value = try self.requireLive(state, operand);
            if (index < operand_values_buffer.len) operand_values_buffer[index] = value;
        }

        var target_value: ValueId = no_value;
        if (self.isRc(target)) {
            target_value = try self.bindFresh(state, target, 1, &.{});
        }

        for (operands, 0..) |operand, index| {
            if (!self.isRc(operand)) continue;
            const value = if (index < operand_values_buffer.len)
                operand_values_buffer[index]
            else
                state.valueOf(operand);
            try self.consumeIntoHolder(state, value, target_value);
        }
    }
};

fn refOpReadsLocal(op: LIR.RefOp, needle: LIR.LocalId) bool {
    return switch (op) {
        .local => |local| local == needle,
        .discriminant => |ref| ref.source == needle,
        .field => |ref| ref.source == needle,
        .tag_payload => |ref| ref.source == needle,
        .tag_payload_struct => |ref| ref.source == needle,
        .list_reinterpret => |ref| ref.backing_ref == needle,
        .nominal => |ref| ref.backing_ref == needle,
    };
}

test "certifier declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;

const CertifyTest = struct {
    allocator: Allocator,
    store: LirStore,
    layouts: layout_mod.Store,
    pair_str: layout_mod.Idx,
    diag: Diagnostic = .{},

    fn init(allocator: Allocator) Allocator.Error!CertifyTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();
        const pair_str = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = .str },
            .{ .index = 1, .layout = .str },
        });
        return .{
            .allocator = allocator,
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .pair_str = pair_str,
        };
    }

    fn deinit(self: *CertifyTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *CertifyTest, layout_idx: layout_mod.Idx) Allocator.Error!LIR.LocalId {
        return try self.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn rcHelper(op: layout_mod.RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        return .{ .op = op, .layout_idx = layout_idx };
    }

    fn assignStr(self: *CertifyTest, target: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .str_literal = try self.store.insertStringView("cert", 0, 4) },
            .next = next,
        } });
    }

    fn assignI64(self: *CertifyTest, target: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
            .next = next,
        } });
    }

    fn decrefStmt(self: *CertifyTest, value: LIR.LocalId, layout_idx: layout_mod.Idx, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .decref = .{
            .value = value,
            .rc = rcHelper(.decref, layout_idx),
            .next = next,
        } });
    }

    fn increfStmt(self: *CertifyTest, value: LIR.LocalId, layout_idx: layout_mod.Idx, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .incref = .{
            .value = value,
            .rc = rcHelper(.incref, layout_idx),
            .next = next,
        } });
    }

    fn ret(self: *CertifyTest, value: LIR.LocalId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .ret = .{ .value = value } });
    }

    fn addProc(self: *CertifyTest, args: []const LIR.LocalId, body: LIR.CFStmtId, ret_layout: layout_mod.Idx) Allocator.Error!LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(args),
            .body = body,
            .ret_layout = ret_layout,
        });
    }

    fn certify(self: *CertifyTest) CertifyError!void {
        return certifyStore(self.allocator, &self.store, &self.layouts, arc_sig.SigTable.all_owned, &self.diag);
    }

    fn certifyWith(self: *CertifyTest, sigs: arc_sig.SigTable) CertifyError!void {
        return certifyStore(self.allocator, &self.store, &self.layouts, sigs, &self.diag);
    }
};

test "certify accepts owned binding released once" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const release = try f.decrefStmt(value, .str, ret);
    const result_assign = try f.assignI64(result, release);
    const body = try f.assignStr(value, result_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags a leaked binding" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const body = try f.assignStr(value, result_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "leaked") != null);
}

test "certify flags a double release" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const second = try f.decrefStmt(value, .str, ret);
    const first = try f.decrefStmt(value, .str, second);
    const result_assign = try f.assignI64(result, first);
    const body = try f.assignStr(value, result_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify flags use after release" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const release = try f.decrefStmt(value, .str, ret);
    const body = try f.assignStr(value, release);
    _ = try f.addProc(&.{}, body, .str);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "dead refcounted local") != null);
}

test "certify accepts an aliased value released through either name" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const release = try f.decrefStmt(alias, .str, ret);
    const result_assign = try f.assignI64(result, release);
    const alias_stmt = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = original },
        .next = result_assign,
    } });
    const body = try f.assignStr(original, alias_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags releasing an aliased value through both names" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const second = try f.decrefStmt(original, .str, ret);
    const first = try f.decrefStmt(alias, .str, second);
    const result_assign = try f.assignI64(result, first);
    const alias_stmt = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = original },
        .next = result_assign,
    } });
    const body = try f.assignStr(original, alias_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
}

test "certify accepts a payload borrow used while the owner is live" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const a = try f.local(.str);
    const b = try f.local(.str);

    // assign a; assign b; incref a; incref b; pair = {a, b}; decref a;
    // decref b; field = pair.0 (borrow, no incref); expect field;
    // result = 1; decref pair; ret result
    const ret = try f.ret(result);
    const release_pair = try f.decrefStmt(pair, f.pair_str, ret);
    const result_assign = try f.assignI64(result, release_pair);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = use_field,
    } });
    const release_b = try f.decrefStmt(b, .str, field_read);
    const release_a = try f.decrefStmt(a, .str, release_b);
    const pair_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ a, b }),
        .next = release_a,
    } });
    const incref_b = try f.increfStmt(b, .str, pair_assign);
    const incref_a = try f.increfStmt(a, .str, incref_b);
    const assign_b = try f.assignStr(b, incref_a);
    const body = try f.assignStr(a, assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags a payload borrow used after the owner dies" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const release_pair = try f.decrefStmt(pair, f.pair_str, use_field);
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = release_pair,
    } });
    const a = try f.local(.str);
    const b = try f.local(.str);
    const release_b = try f.decrefStmt(b, .str, field_read);
    const release_a = try f.decrefStmt(a, .str, release_b);
    const pair_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ a, b }),
        .next = release_a,
    } });
    const incref_b = try f.increfStmt(b, .str, pair_assign);
    const incref_a = try f.increfStmt(a, .str, incref_b);
    const assign_b = try f.assignStr(b, incref_a);
    const body = try f.assignStr(a, assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "dead refcounted local") != null);
}

test "certify flags an incref-restored payload borrow only when over-released" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const release_field = try f.decrefStmt(field, .str, use_field);
    const release_pair = try f.decrefStmt(pair, f.pair_str, release_field);
    const incref_field = try f.increfStmt(field, .str, release_pair);
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = incref_field,
    } });
    const a = try f.local(.str);
    const b = try f.local(.str);
    const release_b = try f.decrefStmt(b, .str, field_read);
    const release_a = try f.decrefStmt(a, .str, release_b);
    const pair_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ a, b }),
        .next = release_a,
    } });
    const incref_b = try f.increfStmt(b, .str, pair_assign);
    const incref_a = try f.increfStmt(a, .str, incref_b);
    const assign_b = try f.assignStr(b, incref_a);
    const body = try f.assignStr(a, assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    // The borrow took its own unit via incref before the owner died and the
    // use-then-release order is sound: incref field, release pair, release
    // field after use. The chain above releases field BEFORE its use, which
    // must fail.
    try testing.expectError(error.Certification, f.certify());
}

test "certify flags an unreleased owned argument consumed twice" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const target = try f.local(.i64);
    const callee = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = null,
        .ret_layout = .i64,
    });
    const ret = try f.ret(target);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = target,
        .proc = callee,
        .args = try f.store.addLocalSpan(&.{ value, value }),
        .next = ret,
    } });
    const body = try f.assignStr(value, call);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify accepts a doubly-consumed argument with one incref" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const target = try f.local(.i64);
    const callee = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = null,
        .ret_layout = .i64,
    });
    const ret = try f.ret(target);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = target,
        .proc = callee,
        .args = try f.store.addLocalSpan(&.{ value, value }),
        .next = ret,
    } });
    const retain = try f.increfStmt(value, .str, call);
    const body = try f.assignStr(value, retain);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags release of a borrowed parameter" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release = try f.decrefStmt(param, .str, result_assign);
    _ = try f.addProc(&.{param}, release, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try testing.expectError(error.Certification, f.certifyWith(.{ .sigs = &sigs }));
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify accepts a borrowed parameter used without RC statements" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_param = try f.store.addCFStmt(.{ .expect = .{
        .condition = param,
        .next = result_assign,
    } });
    _ = try f.addProc(&.{param}, use_param, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try f.certifyWith(.{ .sigs = &sigs });
}

test "certify flags an owned parameter that is never released" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignI64(result, ret);
    _ = try f.addProc(&.{param}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "leaked") != null);
}

test "certify flags branches that disagree at a join" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const join_id: LIR.JoinPointId = @enumFromInt(0);
    const ret = try f.ret(result);
    const release_in_body = try f.decrefStmt(value, .str, ret);
    const result_assign = try f.assignI64(result, release_in_body);

    const jump_a = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_b = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    // Branch B releases the value before jumping; branch A does not.
    const branch_b = try f.decrefStmt(value, .str, jump_b);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_a },
        }),
        .default_branch = branch_b,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = result_assign,
        .remainder = switch_stmt,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    const body = try f.assignStr(value, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    // The disagreement weakens the join's entry assumption to unbound, and
    // re-certifying the body flags the release of the unbound name.
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "unbound") != null);
}

test "certify accepts agreeing jumps through a join" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const join_id: LIR.JoinPointId = @enumFromInt(0);
    const ret = try f.ret(result);
    const release_in_body = try f.decrefStmt(value, .str, ret);
    const result_assign = try f.assignI64(result, release_in_body);

    const jump_a = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_b = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_a },
        }),
        .default_branch = jump_b,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = result_assign,
        .remainder = switch_stmt,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    const body = try f.assignStr(value, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}
