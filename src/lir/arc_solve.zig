//! ARC borrow inference over ownership-neutral LIR.
//!
//! Solving runs before RC statement emission and decides, for every
//! refcounted local, whether its binding is owned (it carries exactly one
//! ownership unit that emission must move or release) or borrowed (it is an
//! alias into another value and emits no RC statements at all), and for every
//! proc, its ownership signature: which refcounted parameter positions are
//! borrowed and whether the return borrows from parameters.
//!
//! A binding solves to borrowed when all of the following hold:
//!
//! - its single defining statement is borrow-capable: a payload read
//!   (`assign_ref` with `.field`/`.tag_payload`/`.tag_payload_struct`), a
//!   local alias (`.local`, `.list_reinterpret`, `.nominal`), a low-level op
//!   whose `RcEffect.result_borrows_args` names exactly one refcounted
//!   argument, or a call whose return borrows exactly one refcounted argument
//! - no occurrence of the binding demands ownership: it is never an owned
//!   call-argument position, a consumed or retained low-level argument, an
//!   aggregate or capture operand, a `set_local` source, or an owned return
//! - the lender chain resolves to a leader local that is bound exactly once:
//!   either an owned local (emission extends its lifetime past the borrow
//!   group's last use) or a borrowed parameter (live for the whole call)
//!
//! Signatures solve interprocedurally in two phases. Phase A iterates
//! parameter modes to a fixpoint with returns pessimistically owned:
//! parameters start borrowed and flip to owned when any occurrence demands a
//! unit, so the borrowed set only shrinks and iteration terminates. Calls in
//! tail position to procs in the same call-graph strongly-connected component
//! demand ownership of their arguments so emission never needs a statement
//! after the call. Phase B then marks returns borrowed when every returned
//! value is a borrow anchored on a borrowed parameter, and re-solves binding
//! modes so callers may borrow such results.
//!
//! Pinned signatures are ABI contracts and never solve: root procs, hosted
//! procs, erased-callable procs, bodyless procs, and procs whose address
//! escapes through a `proc_ref` literal or packed erased callable.
//!
//! Everything else stays owned, which is always sound. The solution is
//! ARC-stage-local and is dropped when insertion ends.

const std = @import("std");
const core = @import("lir_core");
const arc_sig = @import("arc_sig.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

pub const SolveError = std.mem.Allocator.Error;

const no_local: u32 = std.math.maxInt(u32);

/// Per-local binding-mode solution, liveness groups, and per-proc ownership
/// signatures. A group is one leader local together with every borrowed
/// local whose liveness anchors on it; emission keeps the leader's ownership
/// unit alive until the last use of any group member.
pub const Solution = struct {
    allocator: Allocator,
    /// Bit set => the local's binding is borrowed (including borrowed
    /// parameters, which anchor their own groups and live for the whole
    /// call).
    borrowed: std.bit_set.DynamicBitSetUnmanaged,
    /// Owned leader anchoring each local's liveness; the local itself when
    /// the binding is owned or is a borrowed parameter.
    leader: []u32,
    /// Flat group-member storage indexed through `member_offsets`. Every
    /// local's group contains at least itself.
    member_offsets: []u32,
    member_lens: []u32,
    members: []u32,
    /// Solved ownership signature per proc.
    sigs: []arc_sig.RcSig,
    /// Bit set => the local is a join parameter. Join parameters carry one
    /// unit into the join body at every jump; their releases belong to the
    /// body, so emission must not end their lifetime from use scans alone.
    join_param: std.bit_set.DynamicBitSetUnmanaged,

    pub fn deinit(self: *Solution) void {
        self.borrowed.deinit(self.allocator);
        self.allocator.free(self.leader);
        self.allocator.free(self.member_offsets);
        self.allocator.free(self.member_lens);
        self.allocator.free(self.members);
        self.allocator.free(self.sigs);
        self.join_param.deinit(self.allocator);
    }

    pub fn isJoinParam(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.join_param.isSet(index);
    }

    pub fn isBorrowed(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.borrowed.isSet(index);
    }

    pub fn leaderOf(self: *const Solution, local: LIR.LocalId) LIR.LocalId {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return local;
        return @enumFromInt(self.leader[index]);
    }

    /// Members of the leader's liveness group, including the leader.
    pub fn groupMembers(self: *const Solution, leader: LIR.LocalId) []const u32 {
        const index = @intFromEnum(leader);
        if (index >= self.member_offsets.len) return &.{};
        const offset = self.member_offsets[index];
        const len = self.member_lens[index];
        return self.members[offset..][0..len];
    }

    pub fn sigTable(self: *const Solution) arc_sig.SigTable {
        return .{ .sigs = self.sigs };
    }

    pub fn sigOf(self: *const Solution, proc: LIR.LirProcSpecId) arc_sig.RcSig {
        return self.sigTable().get(proc);
    }
};

const DefKind = union(enum) {
    none,
    multi,
    fresh,
    borrow_capable: u32,
};

const Solver = struct {
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: []arc_sig.RcSig,
    pinned: std.bit_set.DynamicBitSetUnmanaged,
    /// Call-graph SCC id per proc, for the tail-call rule.
    scc: []u32,
    defs: []DefKind,
    /// Ownership demands per local. Returns never demand: a returned borrow
    /// pays one retain at the return when the signature's return is owned.
    demand: []bool,
    /// Parameter position per local when the local is a proc parameter
    /// (positions >= 64 are recorded as owned-only).
    param_position: []u32,
    /// Proc owning each parameter local.
    param_proc: []u32,
    /// Join parameters discovered during collection.
    join_param: std.bit_set.DynamicBitSetUnmanaged,
    visited: std.AutoHashMap(LIR.CFStmtId, void),
    stack: std.ArrayList(LIR.CFStmtId),

};

/// Solves binding modes and proc signatures for every local in the store.
pub fn solve(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    roots: []const LIR.LirProcSpecId,
) SolveError!Solution {
    const local_count = store.locals.items.len;
    const proc_count = store.proc_specs.items.len;

    var solver = Solver{
        .allocator = allocator,
        .store = store,
        .rc_local = rc_local,
        .sigs = try allocator.alloc(arc_sig.RcSig, proc_count),
        .pinned = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count),
        .scc = try allocator.alloc(u32, proc_count),
        .defs = try allocator.alloc(DefKind, local_count),
        .demand = try allocator.alloc(bool, local_count),
        .param_position = try allocator.alloc(u32, local_count),
        .param_proc = try allocator.alloc(u32, local_count),
        .join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
        .visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator),
        .stack = std.ArrayList(LIR.CFStmtId).empty,
    };
    var solver_sigs_kept = false;
    defer {
        if (solver_sigs_kept) {
            // Ownership of sigs moved into the Solution.
            solver.sigs = &.{};
        }
        solver.pinned.deinit(allocator);
        allocator.free(solver.scc);
        allocator.free(solver.defs);
        allocator.free(solver.demand);
        allocator.free(solver.param_position);
        allocator.free(solver.param_proc);
        if (!solver_sigs_kept) solver.join_param.deinit(allocator);
        solver.visited.deinit();
        solver.stack.deinit(allocator);
        if (!solver_sigs_kept) allocator.free(solver.sigs);
    }

    @memset(solver.param_position, no_local);
    @memset(solver.param_proc, no_local);

    try computePins(&solver, roots);
    try computeSccs(&solver);

    // Phase A: parameter-mode fixpoint with returns pessimistically owned.
    // Start non-pinned refcounted parameter positions borrowed; demands can
    // only flip positions to owned, so the borrowed set shrinks each round.
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        var sig = arc_sig.RcSig.all_owned;
        if (!solver.pinned.isSet(proc_index)) {
            for (store.getLocalSpan(proc.args), 0..) |param, position| {
                const param_index = @intFromEnum(param);
                solver.param_position[param_index] = @intCast(position);
                solver.param_proc[param_index] = @intCast(proc_index);
                if (position < 64 and rc_local[param_index]) {
                    sig = sig.withBorrowedParam(position);
                }
            }
        }
        solver.sigs[proc_index] = sig;
    }

    var rounds: usize = 0;
    while (true) : (rounds += 1) {
        if (rounds > 64 * proc_count + 1) {
            solveInvariant("ARC signature solving did not converge");
        }
        try collectAll(&solver, .returns_owned);

        var changed = false;
        for (store.proc_specs.items, 0..) |proc, proc_index| {
            if (solver.pinned.isSet(proc_index)) continue;
            var sig = solver.sigs[proc_index];
            for (store.getLocalSpan(proc.args), 0..) |param, position| {
                if (position >= 64) break;
                if (sig.paramMode(position) == .owned) continue;
                const param_index = @intFromEnum(param);
                const stays_borrowed = !solver.demand[param_index] and
                    solver.defs[param_index] != .multi;
                if (!stays_borrowed) {
                    sig.borrowed_params &= ~(@as(u64, 1) << @as(u6, @intCast(position)));
                    changed = true;
                }
            }
            solver.sigs[proc_index] = sig;
        }
        if (!changed) break;
    }

    // Phase B: returns become borrowed when every returned value is a borrow
    // anchored on a borrowed parameter of this proc.
    try collectAll(&solver, .returns_owned);
    {
        var binding = try resolveBindings(&solver, local_count);
        defer binding.deinit(allocator);

        for (store.proc_specs.items, 0..) |proc, proc_index| {
            if (solver.pinned.isSet(proc_index)) continue;
            const body = proc.body orelse continue;
            if (try retLenders(&solver, &binding, proc_index, body)) |lenders| {
                solver.sigs[proc_index].ret_mode = .borrowed;
                solver.sigs[proc_index].ret_lenders = lenders;
            }
        }
    }

    // Final binding solve with the solved signatures: borrowed-return call
    // results become borrow-capable, and returned borrows of borrowed
    // parameters lose their return demand.
    try collectAll(&solver, .returns_solved);
    var binding = try resolveBindings(&solver, local_count);
    errdefer binding.deinit(allocator);

    var solution = Solution{
        .allocator = allocator,
        .borrowed = binding.borrowed,
        .leader = binding.leader,
        .member_offsets = &.{},
        .member_lens = &.{},
        .members = &.{},
        .sigs = solver.sigs,
        .join_param = solver.join_param,
    };
    solver_sigs_kept = true;
    errdefer {
        solution.borrowed.deinit(allocator);
        allocator.free(solution.leader);
        allocator.free(solution.sigs);
        solution.join_param.deinit(allocator);
    }

    // Build flat group-member lists: every local lists at least itself;
    // borrowed locals are appended to their leader's group.
    const member_offsets = try allocator.alloc(u32, local_count);
    errdefer allocator.free(member_offsets);
    const member_lens = try allocator.alloc(u32, local_count);
    errdefer allocator.free(member_lens);
    @memset(member_lens, 0);

    for (0..local_count) |index| {
        member_lens[solution.leader[index]] += 1;
    }
    var offset: u32 = 0;
    for (0..local_count) |index| {
        member_offsets[index] = offset;
        offset += member_lens[index];
    }
    const members = try allocator.alloc(u32, local_count);
    errdefer allocator.free(members);
    var fill = try allocator.alloc(u32, local_count);
    defer allocator.free(fill);
    @memset(fill, 0);
    for (0..local_count) |index| {
        const leader = solution.leader[index];
        members[member_offsets[leader] + fill[leader]] = @intCast(index);
        fill[leader] += 1;
    }

    solution.member_offsets = member_offsets;
    solution.member_lens = member_lens;
    solution.members = members;
    return solution;
}

const BindingResult = struct {
    borrowed: std.bit_set.DynamicBitSetUnmanaged,
    leader: []u32,

    fn deinit(self: *BindingResult, allocator: Allocator) void {
        self.borrowed.deinit(allocator);
        allocator.free(self.leader);
    }
};

/// Resolves each local's lender chain against the current defs/demands.
/// A chain link stays borrowed only if the link itself qualifies and the
/// chain bottoms out at a once-bound leader that is either owned or a
/// borrowed parameter (which is live for the whole call).
fn resolveBindings(solver: *Solver, local_count: usize) SolveError!BindingResult {
    const allocator = solver.allocator;
    var borrowed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer borrowed.deinit(allocator);
    const leader = try allocator.alloc(u32, local_count);
    errdefer allocator.free(leader);
    for (0..local_count) |index| {
        leader[index] = @intCast(index);
    }

    // Borrowed parameters are themselves borrowed bindings anchoring their
    // own groups.
    for (0..local_count) |index| {
        if (paramIsBorrowed(solver, @intCast(index))) {
            borrowed.set(index);
        }
    }

    var chain = std.ArrayList(u32).empty;
    defer chain.deinit(allocator);
    var resolved = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer resolved.deinit(allocator);
    var on_chain = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer on_chain.deinit(allocator);

    for (0..local_count) |start_index| {
        if (resolved.isSet(start_index)) continue;
        if (paramIsBorrowed(solver, @intCast(start_index))) {
            resolved.set(start_index);
            continue;
        }
        chain.clearRetainingCapacity();
        var cursor: u32 = @intCast(start_index);

        const chain_leader: u32 = while (true) {
            if (paramIsBorrowed(solver, cursor)) break cursor;
            if (resolved.isSet(cursor)) break leader[cursor];
            if (on_chain.isSet(cursor)) break cursor;
            if (!borrowQualifies(solver, cursor)) break cursor;
            on_chain.set(cursor);
            try chain.append(allocator, cursor);
            cursor = solver.defs[cursor].borrow_capable;
        };

        const leader_once_bound = paramIsBorrowed(solver, chain_leader) or switch (solver.defs[chain_leader]) {
            .fresh, .borrow_capable => true,
            .none, .multi => false,
        };
        const leader_is_anchor = solver.rc_local[chain_leader] and leader_once_bound and
            (!borrowed.isSet(chain_leader) or paramIsBorrowed(solver, chain_leader));

        for (chain.items) |link| {
            on_chain.unset(link);
            resolved.set(link);
            if (leader_is_anchor and link != chain_leader) {
                borrowed.set(link);
                leader[link] = chain_leader;
            } else {
                leader[link] = link;
            }
        }
        resolved.set(chain_leader);
    }

    return .{ .borrowed = borrowed, .leader = leader };
}

fn paramIsBorrowed(solver: *const Solver, local_index: u32) bool {
    const proc_index = solver.param_proc[local_index];
    if (proc_index == no_local) return false;
    const position = solver.param_position[local_index];
    if (position >= 64) return false;
    return solver.sigs[proc_index].paramMode(position) == .borrowed;
}

fn borrowQualifies(solver: *const Solver, index: u32) bool {
    if (!solver.rc_local[index]) return false;
    if (solver.demand[index]) return false;
    return switch (solver.defs[index]) {
        .borrow_capable => true,
        .none, .multi, .fresh => false,
    };
}

/// Reports the borrowed-parameter lender mask when every `ret` in the body
/// returns a borrow anchored on a borrowed parameter of this proc, ignoring
/// the return occurrence's own demand. Returns null when any path returns an
/// owned or foreign value.
fn retLenders(
    solver: *Solver,
    binding: *const BindingResult,
    proc_index: usize,
    body: LIR.CFStmtId,
) SolveError!?u64 {
    const allocator = solver.allocator;
    const store = solver.store;
    var lenders: u64 = 0;
    var saw_ret = false;

    solver.visited.clearRetainingCapacity();
    solver.stack.clearRetainingCapacity();
    try solver.stack.append(allocator, body);
    while (solver.stack.pop()) |current| {
        if (solver.visited.contains(current)) continue;
        try solver.visited.put(current, {});
        switch (store.getCFStmt(current)) {
            .ret => |ret_stmt| {
                saw_ret = true;
                const value_index = @intFromEnum(ret_stmt.value);
                if (!solver.rc_local[value_index]) continue;
                // The returned value must be a borrow (or borrowed param)
                // whose leader is a borrowed parameter of this proc, and its
                // only ownership demand may be the return itself.
                const leader = binding.leader[value_index];
                const anchored = binding.borrowed.isSet(value_index) or value_index == leader;
                if (!anchored) return null;
                if (!paramIsBorrowed(solver, leader)) return null;
                if (solver.param_proc[leader] != proc_index) return null;
                if (solver.demand[value_index]) return null;
                const position = solver.param_position[leader];
                if (position >= 64) return null;
                lenders |= @as(u64, 1) << @as(u6, @intCast(position));
            },
            .switch_stmt => |s| {
                for (store.getCFSwitchBranches(s.branches)) |branch| {
                    try solver.stack.append(allocator, branch.body);
                }
                try solver.stack.append(allocator, s.default_branch);
                if (s.continuation) |continuation| {
                    try solver.stack.append(allocator, continuation);
                }
            },
            .join => |j| {
                try solver.stack.append(allocator, j.body);
                try solver.stack.append(allocator, j.remainder);
            },
            inline .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free => |s| {
                try solver.stack.append(allocator, s.next);
            },
            .jump, .crash, .runtime_error, .loop_continue, .loop_break => {},
        }
    }

    if (!saw_ret) return null;
    if (lenders == 0) return null;
    return lenders;
}



const RetTreatment = enum {
    returns_owned,
    returns_solved,
};

fn collectAll(solver: *Solver, ret_treatment: RetTreatment) SolveError!void {
    @memset(solver.defs, .none);
    @memset(solver.demand, false);

    const store = solver.store;
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        const body = proc.body orelse continue;
        for (store.getLocalSpan(proc.args)) |param| {
            noteDef(solver.defs, param, .fresh);
        }
        solver.visited.clearRetainingCapacity();
        solver.stack.clearRetainingCapacity();
        try solver.stack.append(solver.allocator, body);
        while (solver.stack.pop()) |current| {
            if (solver.visited.contains(current)) continue;
            try solver.visited.put(current, {});
            try collectStmt(solver, @intCast(proc_index), ret_treatment, current);
        }
    }
}

fn noteDef(defs: []DefKind, local: LIR.LocalId, kind: DefKind) void {
    const index = @intFromEnum(local);
    defs[index] = switch (defs[index]) {
        .none => kind,
        .multi, .fresh, .borrow_capable => .multi,
    };
}

fn noteDemand(solver: *Solver, local: LIR.LocalId) void {
    solver.demand[@intFromEnum(local)] = true;
}

fn collectStmt(
    solver: *Solver,
    proc_index: u32,
    ret_treatment: RetTreatment,
    current: LIR.CFStmtId,
) SolveError!void {
    const store = solver.store;
    const allocator = solver.allocator;
    switch (store.getCFStmt(current)) {
        .assign_ref => |assign| {
            switch (assign.op) {
                .local => |source| {
                    if (assign.target != source) {
                        noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(source) });
                    } else {
                        noteDef(solver.defs, assign.target, .multi);
                    }
                },
                .discriminant => noteDef(solver.defs, assign.target, .fresh),
                .field => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .tag_payload => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .tag_payload_struct => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .list_reinterpret => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.backing_ref) }),
                .nominal => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.backing_ref) }),
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_literal => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            try solver.stack.append(allocator, assign.next);
        },
        .assign_call => |assign| {
            const callee_sig = solver.sigs[@intFromEnum(assign.proc)];
            const args = store.getLocalSpan(assign.args);

            // Tail-position calls within one call-graph component demand
            // ownership of their arguments so emission never needs a
            // release after the call.
            const same_scc = solver.scc[@intFromEnum(assign.proc)] == solver.scc[proc_index];
            const tail_call = same_scc and isTailCall(store, assign.target, assign.next);

            const borrowed_ret_source = if (ret_treatment == .returns_solved and callee_sig.ret_mode == .borrowed)
                callRetBorrowSource(solver, callee_sig, args)
            else
                no_local;
            if (borrowed_ret_source != no_local) {
                noteDef(solver.defs, assign.target, .{ .borrow_capable = borrowed_ret_source });
            } else {
                noteDef(solver.defs, assign.target, .fresh);
            }

            for (args, 0..) |arg, position| {
                if (!solver.rc_local[@intFromEnum(arg)]) continue;
                if (!tail_call and callee_sig.paramMode(position) == .borrowed) continue;
                noteDemand(solver, arg);
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_call_erased => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            noteDemand(solver, assign.closure);
            for (store.getLocalSpan(assign.args)) |arg| {
                if (!solver.rc_local[@intFromEnum(arg)]) continue;
                noteDemand(solver, arg);
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_packed_erased_fn => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            if (assign.capture) |capture| noteDemand(solver, capture);
            try solver.stack.append(allocator, assign.next);
        },
        .assign_low_level => |assign| {
            const args = store.getLocalSpan(assign.args);
            const borrow_source = lowLevelBorrowSource(solver.rc_local, assign.rc_effect, args);
            if (assign.rc_effect.retain_result and borrow_source != no_local) {
                noteDef(solver.defs, assign.target, .{ .borrow_capable = borrow_source });
            } else {
                noteDef(solver.defs, assign.target, .fresh);
            }
            for (args, 0..) |arg, index| {
                if (!solver.rc_local[@intFromEnum(arg)]) continue;
                if (index >= 64) {
                    noteDemand(solver, arg);
                    continue;
                }
                const bit = @as(u64, 1) << @as(u6, @intCast(index));
                if ((assign.rc_effect.consume_args & bit) != 0 or
                    (assign.rc_effect.retain_args & bit) != 0)
                {
                    noteDemand(solver, arg);
                }
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_list => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            for (store.getLocalSpan(assign.elems)) |elem| {
                noteDemand(solver, elem);
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_struct => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            for (store.getLocalSpan(assign.fields)) |field| {
                noteDemand(solver, field);
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_tag => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            if (assign.payload) |payload| noteDemand(solver, payload);
            try solver.stack.append(allocator, assign.next);
        },
        .set_local => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            if (assign.target != assign.value) noteDemand(solver, assign.value);
            try solver.stack.append(allocator, assign.next);
        },
        .debug => |debug_stmt| try solver.stack.append(allocator, debug_stmt.next),
        .expect => |expect_stmt| try solver.stack.append(allocator, expect_stmt.next),
        .incref => |rc| try solver.stack.append(allocator, rc.next),
        .decref => |rc| try solver.stack.append(allocator, rc.next),
        .free => |rc| try solver.stack.append(allocator, rc.next),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try solver.stack.append(allocator, branch.body);
            }
            try solver.stack.append(allocator, switch_stmt.default_branch);
            if (switch_stmt.continuation) |continuation| {
                try solver.stack.append(allocator, continuation);
            }
        },
        .join => |join_stmt| {
            // Join parameters are written at every jump; they stay owned.
            for (store.getLocalSpan(join_stmt.params)) |param| {
                noteDef(solver.defs, param, .multi);
                solver.join_param.set(@intFromEnum(param));
            }
            try solver.stack.append(allocator, join_stmt.body);
            try solver.stack.append(allocator, join_stmt.remainder);
        },
        .ret => {},
        .jump, .crash, .runtime_error, .loop_continue, .loop_break => {},
    }
}

/// A call is in tail position when the very next statement returns the call
/// result.
fn isTailCall(store: *const LirStore, target: LIR.LocalId, next: LIR.CFStmtId) bool {
    return switch (store.getCFStmt(next)) {
        .ret => |ret_stmt| ret_stmt.value == target,
        else => false,
    };
}

/// Returns the single refcounted argument a borrowed-return call result may
/// borrow from, or `no_local` when the lender mask names zero or several
/// refcounted arguments (the caller then keeps the result owned and retains
/// it after the call).
fn callRetBorrowSource(solver: *const Solver, callee_sig: arc_sig.RcSig, args: []const LIR.LocalId) u32 {
    var source: u32 = no_local;
    for (args, 0..) |arg, position| {
        if (position >= 64) break;
        const bit = @as(u64, 1) << @as(u6, @intCast(position));
        if ((callee_sig.ret_lenders & bit) == 0) continue;
        const arg_index = @intFromEnum(arg);
        if (arg_index >= solver.rc_local.len or !solver.rc_local[arg_index]) continue;
        if (source != no_local and source != arg_index) return no_local;
        source = arg_index;
    }
    return source;
}

/// Returns the single refcounted argument named by `result_borrows_args`, or
/// `no_local` when the mask names zero or several refcounted arguments.
fn lowLevelBorrowSource(
    rc_local: []const bool,
    rc_effect: LIR.LowLevel.RcEffect,
    args: []const LIR.LocalId,
) u32 {
    if (rc_effect.result_borrows_args == 0) return no_local;
    var source: u32 = no_local;
    for (args, 0..) |arg, index| {
        if (index >= 64) break;
        const bit = @as(u64, 1) << @as(u6, @intCast(index));
        if ((rc_effect.result_borrows_args & bit) == 0) continue;
        const arg_index = @intFromEnum(arg);
        if (arg_index >= rc_local.len or !rc_local[arg_index]) continue;
        if (source != no_local and source != arg_index) return no_local;
        source = arg_index;
    }
    return source;
}

fn computePins(solver: *Solver, roots: []const LIR.LirProcSpecId) SolveError!void {
    const store = solver.store;
    for (roots) |root| {
        solver.pinned.set(@intFromEnum(root));
    }
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        if (proc.body == null or proc.hosted != null or proc.abi == .erased_callable) {
            solver.pinned.set(proc_index);
        }
    }
    // Procs whose address escapes are callable through paths the solver
    // cannot see; they keep the all-owned ABI.
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_literal => |assign| switch (assign.value) {
                .proc_ref => |proc| solver.pinned.set(@intFromEnum(proc)),
                else => {},
            },
            .assign_packed_erased_fn => |assign| solver.pinned.set(@intFromEnum(assign.proc)),
            else => {},
        }
    }
}

/// Tarjan strongly-connected components over the direct-call graph.
fn computeSccs(solver: *Solver) SolveError!void {
    const allocator = solver.allocator;
    const store = solver.store;
    const proc_count = store.proc_specs.items.len;

    // Collect direct-call edges.
    var edges = std.ArrayList([2]u32).empty;
    defer edges.deinit(allocator);
    for (store.proc_specs.items, 0..) |proc, caller| {
        const body = proc.body orelse continue;
        solver.visited.clearRetainingCapacity();
        solver.stack.clearRetainingCapacity();
        try solver.stack.append(allocator, body);
        while (solver.stack.pop()) |current| {
            if (solver.visited.contains(current)) continue;
            try solver.visited.put(current, {});
            switch (store.getCFStmt(current)) {
                .assign_call => |assign| {
                    try edges.append(allocator, .{ @intCast(caller), @intFromEnum(assign.proc) });
                    try solver.stack.append(allocator, assign.next);
                },
                .switch_stmt => |s| {
                    for (store.getCFSwitchBranches(s.branches)) |branch| {
                        try solver.stack.append(allocator, branch.body);
                    }
                    try solver.stack.append(allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try solver.stack.append(allocator, continuation);
                    }
                },
                .join => |j| {
                    try solver.stack.append(allocator, j.body);
                    try solver.stack.append(allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free => |s| {
                    try solver.stack.append(allocator, s.next);
                },
                .jump, .ret, .crash, .runtime_error, .loop_continue, .loop_break => {},
            }
        }
    }

    // Adjacency lists.
    const out_lens = try allocator.alloc(u32, proc_count);
    defer allocator.free(out_lens);
    @memset(out_lens, 0);
    for (edges.items) |edge| out_lens[edge[0]] += 1;
    const out_offsets = try allocator.alloc(u32, proc_count);
    defer allocator.free(out_offsets);
    var total: u32 = 0;
    for (out_lens, 0..) |len, index| {
        out_offsets[index] = total;
        total += len;
    }
    const out_edges = try allocator.alloc(u32, total);
    defer allocator.free(out_edges);
    const fill = try allocator.alloc(u32, proc_count);
    defer allocator.free(fill);
    @memset(fill, 0);
    for (edges.items) |edge| {
        out_edges[out_offsets[edge[0]] + fill[edge[0]]] = edge[1];
        fill[edge[0]] += 1;
    }

    // Iterative Tarjan.
    const unvisited: u32 = std.math.maxInt(u32);
    const index_of = try allocator.alloc(u32, proc_count);
    defer allocator.free(index_of);
    @memset(index_of, unvisited);
    const low_link = try allocator.alloc(u32, proc_count);
    defer allocator.free(low_link);
    var on_stack = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count);
    defer on_stack.deinit(allocator);
    var scc_stack = std.ArrayList(u32).empty;
    defer scc_stack.deinit(allocator);

    const Frame = struct {
        node: u32,
        edge_cursor: u32,
    };
    var frames = std.ArrayList(Frame).empty;
    defer frames.deinit(allocator);

    var next_index: u32 = 0;
    var next_scc: u32 = 0;

    for (0..proc_count) |start| {
        if (index_of[start] != unvisited) continue;
        try frames.append(allocator, .{ .node = @intCast(start), .edge_cursor = 0 });
        index_of[start] = next_index;
        low_link[start] = next_index;
        next_index += 1;
        try scc_stack.append(allocator, @intCast(start));
        on_stack.set(start);

        while (frames.items.len > 0) {
            const frame = &frames.items[frames.items.len - 1];
            const node = frame.node;
            const edge_count = out_lens[node];
            if (frame.edge_cursor < edge_count) {
                const child = out_edges[out_offsets[node] + frame.edge_cursor];
                frame.edge_cursor += 1;
                if (index_of[child] == unvisited) {
                    index_of[child] = next_index;
                    low_link[child] = next_index;
                    next_index += 1;
                    try scc_stack.append(allocator, child);
                    on_stack.set(child);
                    try frames.append(allocator, .{ .node = child, .edge_cursor = 0 });
                } else if (on_stack.isSet(child)) {
                    low_link[node] = @min(low_link[node], index_of[child]);
                }
                continue;
            }
            // Node finished.
            _ = frames.pop();
            if (frames.items.len > 0) {
                const parent = frames.items[frames.items.len - 1].node;
                low_link[parent] = @min(low_link[parent], low_link[node]);
            }
            if (low_link[node] == index_of[node]) {
                while (true) {
                    const member = scc_stack.pop() orelse solveInvariant("ARC SCC stack underflow");
                    on_stack.unset(member);
                    solver.scc[member] = next_scc;
                    if (member == node) break;
                }
                next_scc += 1;
            }
        }
    }
}

fn solveInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
