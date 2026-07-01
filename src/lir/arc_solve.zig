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
//! modes so callers may borrow such results. After signatures settle,
//! unique returns solve to a fixpoint with the born-unique analysis: a
//! proc's return is unique when every `ret` returns a born-unique value
//! surviving to the return with no other holder, and a direct-call result
//! of a unique-returning callee is itself a unique birth in its caller.
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

/// Errors that can occur while constructing the ARC solver's internal tables.
pub const SolveError = std.mem.Allocator.Error;

const no_local: u32 = std.math.maxInt(u32);

/// Presence-bit condition guarding a payload local whose storage may not be
/// initialized on every path into a join.
pub const MaybeUninitializedCondition = struct {
    /// Local containing the presence bitset word.
    local: LIR.LocalId,
    /// Bits that must all be set before the payload local is initialized.
    mask: u64,
};

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
    /// Source local of each pure same-value alias, or `no_local`.
    alias_source: []u32,
    /// Solved ownership signature per proc.
    sigs: []arc_sig.RcSig,
    /// Bit set => the local is a join parameter. Join parameters carry one
    /// unit into the join body at every jump; their releases belong to the
    /// body, so emission must not end their lifetime from use scans alone.
    join_param: std.bit_set.DynamicBitSetUnmanaged,
    /// Bit set => the local is a join parameter whose initial value may be
    /// uninitialized. These locals are released only after an explicit
    /// initialized-payload refinement.
    maybe_uninitialized_join_param: std.bit_set.DynamicBitSetUnmanaged,
    /// Condition local for each maybe-uninitialized join parameter, or
    /// `no_local` when the local is not maybe-uninitialized.
    maybe_uninitialized_condition: []u32,
    /// Presence mask for each maybe-uninitialized join parameter.
    maybe_uninitialized_condition_mask: []u64,
    /// Bit set => the local may hold an allocation the host can also touch,
    /// so its RC statements need atomic count updates.
    visible: std.bit_set.DynamicBitSetUnmanaged,
    /// Bit set => the local's value's outermost allocation provably has
    /// count 1 at the local's definition and no statement can add another
    /// holder afterward.
    unique: std.bit_set.DynamicBitSetUnmanaged,
    /// Bit set => some occurrence can add another holder to the local's
    /// value (or consume it a second time). A parameter a variant's demand
    /// vector seeds born-unique stays unique through its body only when
    /// this bit is clear.
    unique_destroyed: std.bit_set.DynamicBitSetUnmanaged,
    /// Bit set => the proc's signature is pinned by ABI (roots, hosted,
    /// erased-callable, bodyless, and address-escaping procs). Pinned procs
    /// are never mode-specialized.
    pinned: std.bit_set.DynamicBitSetUnmanaged,

    pub fn deinit(self: *Solution) void {
        self.borrowed.deinit(self.allocator);
        self.allocator.free(self.leader);
        self.allocator.free(self.member_offsets);
        self.allocator.free(self.member_lens);
        self.allocator.free(self.members);
        self.allocator.free(self.alias_source);
        self.allocator.free(self.sigs);
        self.join_param.deinit(self.allocator);
        self.maybe_uninitialized_join_param.deinit(self.allocator);
        self.allocator.free(self.maybe_uninitialized_condition);
        self.allocator.free(self.maybe_uninitialized_condition_mask);
        self.visible.deinit(self.allocator);
        self.unique.deinit(self.allocator);
        self.unique_destroyed.deinit(self.allocator);
        self.pinned.deinit(self.allocator);
    }

    pub fn isJoinParam(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.join_param.isSet(index);
    }

    pub fn maybeUninitializedCondition(self: *const Solution, local: LIR.LocalId) ?MaybeUninitializedCondition {
        const index = @intFromEnum(local);
        if (index >= self.maybe_uninitialized_condition.len) return null;
        if (!self.maybe_uninitialized_join_param.isSet(index)) return null;
        const condition = self.maybe_uninitialized_condition[index];
        if (condition == no_local) return null;
        return .{ .local = @enumFromInt(condition), .mask = self.maybe_uninitialized_condition_mask[index] };
    }

    pub fn isBorrowed(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.borrowed.isSet(index);
    }

    /// True when RC statements touching this local's value must use atomic
    /// count updates: the value may hold an allocation a host thread can
    /// also touch.
    pub fn isVisible(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return true;
        return self.visible.isSet(index);
    }

    /// True when the local's value was born with its outermost allocation at
    /// count 1 and no statement can add another holder, so a runtime
    /// uniqueness check that consumes this local's unit is redundant.
    pub fn isUnique(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.unique.isSet(index);
    }

    /// True when some occurrence can add another holder to the local's
    /// value (or consume it a second time), so a born-unique seed on this
    /// local would not survive to a consuming use.
    pub fn isUniqueDestroyed(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return true;
        return self.unique_destroyed.isSet(index);
    }

    /// True when the proc's signature is pinned by ABI and must never be
    /// weakened or specialized.
    pub fn isPinnedProc(self: *const Solution, proc: LIR.LirProcSpecId) bool {
        const index = @intFromEnum(proc);
        if (index >= self.pinned.capacity()) return true;
        return self.pinned.isSet(index);
    }

    pub fn leaderOf(self: *const Solution, local: LIR.LocalId) LIR.LocalId {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return local;
        return @enumFromInt(self.leader[index]);
    }

    /// Local whose ownership unit can be moved by an occurrence of `local`.
    /// Borrowed pure same-value aliases move their source's unit. Owned pure
    /// aliases already have their own retained unit, and field/payload borrows
    /// are not the same value as their liveness leader.
    pub fn unitLocalOf(self: *const Solution, local: LIR.LocalId) LIR.LocalId {
        if (!self.isBorrowed(local)) return local;
        var cursor = @intFromEnum(local);
        var steps: usize = 0;
        while (cursor < self.alias_source.len and self.alias_source[cursor] != no_local) {
            cursor = self.alias_source[cursor];
            steps += 1;
            if (steps > self.alias_source.len) solveInvariant("ARC alias-source chain contained a cycle");
        }
        return @enumFromInt(cursor);
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
    /// Source local of each pure same-value alias (`.local`,
    /// `.list_reinterpret`, `.nominal`), or `no_local`. A demand on an alias
    /// is a demand on its source: the consuming occurrence takes the chain's
    /// single unit, so the whole chain must be owned for the unit to move
    /// through instead of paying a retain/release pair.
    alias_source: []u32,
    /// Parameter position per local when the local is a proc parameter
    /// (positions >= 64 are recorded as owned-only).
    param_position: []u32,
    /// Proc owning each parameter local.
    param_proc: []u32,
    /// Join parameters discovered during collection.
    join_param: std.bit_set.DynamicBitSetUnmanaged,
    maybe_uninitialized_join_param: std.bit_set.DynamicBitSetUnmanaged,
    maybe_uninitialized_condition: []u32,
    maybe_uninitialized_condition_mask: []u64,
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
        .alias_source = try allocator.alloc(u32, local_count),
        .param_position = try allocator.alloc(u32, local_count),
        .param_proc = try allocator.alloc(u32, local_count),
        .join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
        .maybe_uninitialized_join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
        .maybe_uninitialized_condition = try allocator.alloc(u32, local_count),
        .maybe_uninitialized_condition_mask = try allocator.alloc(u64, local_count),
        .visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator),
        .stack = std.ArrayList(LIR.CFStmtId).empty,
    };
    var solver_sigs_kept = false;
    var solver_alias_source_kept = false;
    defer {
        if (solver_sigs_kept) {
            // Ownership of sigs moved into the Solution.
            solver.sigs = &.{};
        }
        if (solver_alias_source_kept) {
            // Ownership of alias_source moved into the Solution.
            solver.alias_source = &.{};
        }
        if (!solver_sigs_kept) solver.pinned.deinit(allocator);
        allocator.free(solver.scc);
        allocator.free(solver.defs);
        allocator.free(solver.demand);
        allocator.free(solver.alias_source);
        allocator.free(solver.param_position);
        allocator.free(solver.param_proc);
        if (!solver_sigs_kept) solver.join_param.deinit(allocator);
        if (!solver_sigs_kept) solver.maybe_uninitialized_join_param.deinit(allocator);
        if (!solver_sigs_kept) allocator.free(solver.maybe_uninitialized_condition);
        if (!solver_sigs_kept) allocator.free(solver.maybe_uninitialized_condition_mask);
        solver.visited.deinit();
        solver.stack.deinit(allocator);
        if (!solver_sigs_kept) allocator.free(solver.sigs);
    }

    @memset(solver.param_position, no_local);
    @memset(solver.param_proc, no_local);
    @memset(solver.maybe_uninitialized_condition, no_local);
    @memset(solver.maybe_uninitialized_condition_mask, 0);

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

    var visible = try computeVisibility(allocator, store, rc_local, &solver.pinned);
    errdefer visible.deinit(allocator);

    // Unique returns solve to a fixpoint against the born-unique analysis.
    // `ret_unique` bits start false and only ever flip to true: a flip adds
    // unique births at that callee's call results, and the destroy rules do
    // not depend on `ret_unique`, so the unique set only grows. Every round
    // either flips at least one proc or the loop stops, the final round
    // recomputes uniqueness under the final signatures, and the round count
    // is bounded by the longest unique-return dependency chain.
    var uniqueness = try computeUniqueness(allocator, store, rc_local, .{ .sigs = solver.sigs });
    {
        errdefer uniqueness.deinit(allocator);
        var unique_rounds: usize = 0;
        while (true) : (unique_rounds += 1) {
            if (unique_rounds > proc_count + 1) {
                solveInvariant("ARC unique-return solving did not converge");
            }
            var changed = false;
            for (store.proc_specs.items, 0..) |proc, proc_index| {
                if (solver.pinned.isSet(proc_index)) continue;
                if (solver.sigs[proc_index].ret_unique) continue;
                const body = proc.body orelse continue;
                if (try retAllUnique(&solver, &uniqueness.unique, body)) {
                    solver.sigs[proc_index].ret_unique = true;
                    changed = true;
                }
            }
            if (!changed) break;
            const next = try computeUniqueness(allocator, store, rc_local, .{ .sigs = solver.sigs });
            uniqueness.deinit(allocator);
            uniqueness = next;
        }
    }
    // Emission consumes the final bit and the destroyed set (for variant
    // parameter seeds); the born-unique origin set is re-derived by the
    // certifier.
    uniqueness.born_unique.deinit(allocator);
    errdefer uniqueness.unique.deinit(allocator);
    errdefer uniqueness.destroyed.deinit(allocator);

    var solution = Solution{
        .allocator = allocator,
        .borrowed = binding.borrowed,
        .leader = binding.leader,
        .member_offsets = &.{},
        .member_lens = &.{},
        .members = &.{},
        .alias_source = solver.alias_source,
        .sigs = solver.sigs,
        .join_param = solver.join_param,
        .maybe_uninitialized_join_param = solver.maybe_uninitialized_join_param,
        .maybe_uninitialized_condition = solver.maybe_uninitialized_condition,
        .maybe_uninitialized_condition_mask = solver.maybe_uninitialized_condition_mask,
        .visible = visible,
        .unique = uniqueness.unique,
        .unique_destroyed = uniqueness.destroyed,
        .pinned = solver.pinned,
    };
    solver_sigs_kept = true;
    solver_alias_source_kept = true;
    errdefer {
        solution.borrowed.deinit(allocator);
        allocator.free(solution.leader);
        allocator.free(solution.alias_source);
        allocator.free(solution.sigs);
        solution.join_param.deinit(allocator);
        solution.maybe_uninitialized_join_param.deinit(allocator);
        allocator.free(solution.maybe_uninitialized_condition);
        allocator.free(solution.maybe_uninitialized_condition_mask);
        solution.visible.deinit(allocator);
        solution.unique.deinit(allocator);
        solution.unique_destroyed.deinit(allocator);
        solution.pinned.deinit(allocator);
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
            .switch_initialized_payload => |s| {
                try solver.stack.append(allocator, s.initialized_branch);
                try solver.stack.append(allocator, s.uninitialized_branch);
            },
            .str_match => |s| {
                try solver.stack.append(allocator, s.on_match);
                try solver.stack.append(allocator, s.on_miss);
            },
            .str_match_set => |s| {
                for (store.getStrMatchArms(s.arms)) |arm| {
                    try solver.stack.append(allocator, arm.on_match);
                }
                try solver.stack.append(allocator, s.on_miss);
            },
            .join => |j| {
                try solver.stack.append(allocator, j.body);
                try solver.stack.append(allocator, j.remainder);
            },
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                try solver.stack.append(allocator, s.next);
            },
            .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
        }
    }

    if (!saw_ret) return null;
    if (lenders == 0) return null;
    return lenders;
}

/// True when every `ret` in the body returns a refcounted value whose
/// unique bit is set: born unique and surviving to the return, which is the
/// value's single consuming use. A body without a `ret` reports false; a
/// never-returning proc's unique-return bit is never consulted.
fn retAllUnique(
    solver: *Solver,
    unique: *const std.bit_set.DynamicBitSetUnmanaged,
    body: LIR.CFStmtId,
) SolveError!bool {
    const allocator = solver.allocator;
    const store = solver.store;
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
                if (value_index >= solver.rc_local.len or !solver.rc_local[value_index]) return false;
                if (!unique.isSet(value_index)) return false;
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
            .switch_initialized_payload => |s| {
                try solver.stack.append(allocator, s.initialized_branch);
                try solver.stack.append(allocator, s.uninitialized_branch);
            },
            .str_match => |s| {
                try solver.stack.append(allocator, s.on_match);
                try solver.stack.append(allocator, s.on_miss);
            },
            .str_match_set => |s| {
                for (store.getStrMatchArms(s.arms)) |arm| {
                    try solver.stack.append(allocator, arm.on_match);
                }
                try solver.stack.append(allocator, s.on_miss);
            },
            .join => |j| {
                try solver.stack.append(allocator, j.body);
                try solver.stack.append(allocator, j.remainder);
            },
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                try solver.stack.append(allocator, s.next);
            },
            .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
        }
    }

    return saw_ret;
}

const RetTreatment = enum {
    returns_owned,
    returns_solved,
};

fn collectAll(solver: *Solver, ret_treatment: RetTreatment) SolveError!void {
    @memset(solver.defs, .none);
    @memset(solver.demand, false);
    @memset(solver.alias_source, no_local);

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

    propagateAliasDemands(solver);
}

/// Records a pure same-value alias edge. A local bound more than once stops
/// propagating (its def degrades to `.multi` and it never borrows anyway).
fn noteAlias(solver: *Solver, target: LIR.LocalId, source: LIR.LocalId) void {
    const index = @intFromEnum(target);
    solver.alias_source[index] = if (solver.alias_source[index] == no_local and
        solver.defs[index] != .multi)
        @intFromEnum(source)
    else
        no_local;
}

/// Demands on aliases are demands on their sources, transitively: the chain
/// shares one value whose single unit should move through the chain to the
/// consuming occurrence rather than the alias paying a retain while the
/// source's unit is separately released.
fn propagateAliasDemands(solver: *Solver) void {
    for (0..solver.demand.len) |start| {
        if (!solver.demand[start]) continue;
        var cursor: u32 = @intCast(start);
        while (true) {
            // A multi-bound alias names different values over time; its
            // recorded edge is not a same-value link.
            switch (solver.defs[cursor]) {
                .multi => break,
                else => {},
            }
            const source = solver.alias_source[cursor];
            if (source == no_local or solver.demand[source]) break;
            solver.demand[source] = true;
            cursor = source;
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
                        noteAlias(solver, assign.target, source);
                    } else {
                        noteDef(solver.defs, assign.target, .multi);
                    }
                },
                .discriminant => noteDef(solver.defs, assign.target, .fresh),
                .field => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .tag_payload => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .tag_payload_struct => |op| noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .list_reinterpret => |op| {
                    noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.backing_ref) });
                    noteAlias(solver, assign.target, op.backing_ref);
                },
                .nominal => |op| {
                    noteDef(solver.defs, assign.target, .{ .borrow_capable = @intFromEnum(op.backing_ref) });
                    noteAlias(solver, assign.target, op.backing_ref);
                },
            }
            try solver.stack.append(allocator, assign.next);
        },
        .assign_literal => |assign| {
            noteDef(solver.defs, assign.target, .fresh);
            try solver.stack.append(allocator, assign.next);
        },
        .init_uninitialized => |init| {
            try solver.stack.append(allocator, init.next);
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
        // The failure report takes ownership of the message.
        .expect_err => |expect_err_stmt| noteDemand(solver, expect_err_stmt.message),
        .expect => |expect_stmt| try solver.stack.append(allocator, expect_stmt.next),
        .comptime_branch_taken => |marker| try solver.stack.append(allocator, marker.next),
        .incref => |rc| try solver.stack.append(allocator, rc.next),
        .decref => |rc| try solver.stack.append(allocator, rc.next),
        .decref_if_initialized => |rc| {
            noteDemand(solver, rc.value);
            try solver.stack.append(allocator, rc.next);
        },
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
        .switch_initialized_payload => |switch_stmt| {
            try solver.stack.append(allocator, switch_stmt.initialized_branch);
            try solver.stack.append(allocator, switch_stmt.uninitialized_branch);
        },
        .str_match => |str_match| {
            for (store.getStrMatchSteps(str_match.steps)) |step| {
                switch (step.capture) {
                    .discard => {},
                    .view => |local| noteDef(solver.defs, local, .{ .borrow_capable = @intFromEnum(str_match.source) }),
                }
            }
            try solver.stack.append(allocator, str_match.on_match);
            try solver.stack.append(allocator, str_match.on_miss);
        },
        .str_match_set => |str_match_set| {
            for (store.getStrMatchArms(str_match_set.arms)) |arm| {
                for (store.getStrMatchSteps(arm.steps)) |step| {
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| noteDef(solver.defs, local, .{ .borrow_capable = @intFromEnum(str_match_set.source) }),
                    }
                }
                try solver.stack.append(allocator, arm.on_match);
            }
            try solver.stack.append(allocator, str_match_set.on_miss);
        },
        .join => |join_stmt| {
            // Join parameters are written at every jump; they stay owned.
            for (store.getLocalSpan(join_stmt.params)) |param| {
                noteDef(solver.defs, param, .multi);
                solver.join_param.set(@intFromEnum(param));
            }
            const maybe_uninitialized_params = store.getLocalSpan(join_stmt.maybe_uninitialized_params);
            const maybe_uninitialized_conditions = store.getLocalSpan(join_stmt.maybe_uninitialized_conditions);
            const maybe_uninitialized_condition_masks = store.getU64Span(join_stmt.maybe_uninitialized_condition_masks);
            if (maybe_uninitialized_params.len != maybe_uninitialized_conditions.len or maybe_uninitialized_params.len != maybe_uninitialized_condition_masks.len) {
                solveInvariant("maybe-uninitialized join metadata arity mismatch");
            }
            for (maybe_uninitialized_params, maybe_uninitialized_conditions, maybe_uninitialized_condition_masks) |param, condition, mask| {
                const param_index = @intFromEnum(param);
                solver.maybe_uninitialized_join_param.set(@intFromEnum(param));
                solver.maybe_uninitialized_condition[param_index] = @intFromEnum(condition);
                solver.maybe_uninitialized_condition_mask[param_index] = mask;
            }
            try solver.stack.append(allocator, join_stmt.body);
            try solver.stack.append(allocator, join_stmt.remainder);
        },
        .ret => {},
        .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
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
    fillPinnedProcs(solver.store, roots, &solver.pinned);
}

/// Computes the pinned-proc set over a freshly allocated bit set; the
/// certifier mirrors the visibility analysis from this.
pub fn computePinnedProcs(
    allocator: Allocator,
    store: *const LirStore,
    roots: []const LIR.LirProcSpecId,
) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    var pinned = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.proc_specs.items.len);
    errdefer pinned.deinit(allocator);
    fillPinnedProcs(store, roots, &pinned);
    return pinned;
}

fn fillPinnedProcs(
    store: *const LirStore,
    roots: []const LIR.LirProcSpecId,
    pinned: *std.bit_set.DynamicBitSetUnmanaged,
) void {
    for (roots) |root| {
        pinned.set(@intFromEnum(root));
    }
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        if (proc.body == null or proc.hosted != null or proc.abi == .erased_callable) {
            pinned.set(proc_index);
        }
    }
    // Procs whose address escapes are callable through paths the solver
    // cannot see; they keep the all-owned ABI.
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_literal => |assign| switch (assign.value) {
                .proc_ref => |proc| pinned.set(@intFromEnum(proc)),
                else => {},
            },
            .assign_packed_erased_fn => |assign| pinned.set(@intFromEnum(assign.proc)),
            else => {},
        }
    }
}

/// Marks every local that may hold a host-visible allocation: a may-bit
/// propagated to a fixpoint over same-value, containment, call, and
/// low-level sharing edges, seeded from pinned procs' parameters and
/// returns and from call shapes the solver cannot see into. RC statements
/// on unmarked locals may update counts without atomics, because no other
/// thread can ever hold their allocations.
pub fn computeVisibility(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    pinned: *const std.bit_set.DynamicBitSetUnmanaged,
) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    const local_count = store.locals.items.len;
    const proc_count = store.proc_specs.items.len;

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);

    var visible = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer visible.deinit(allocator);
    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);

    // Per-proc return values, for linking call results to callee returns.
    const ret_values = try allocator.alloc(std.ArrayList(u32), proc_count);
    defer {
        for (ret_values) |*list| list.deinit(allocator);
        allocator.free(ret_values);
    }
    @memset(ret_values, .empty);
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        const body = proc.body orelse continue;
        visited.clearRetainingCapacity();
        stack.clearRetainingCapacity();
        try stack.append(allocator, body);
        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            switch (store.getCFStmt(current)) {
                .ret => |ret_stmt| try ret_values[proc_index].append(allocator, @intFromEnum(ret_stmt.value)),
                .switch_stmt => |stmt| {
                    for (store.getCFSwitchBranches(stmt.branches)) |branch| {
                        try stack.append(allocator, branch.body);
                    }
                    try stack.append(allocator, stmt.default_branch);
                    if (stmt.continuation) |continuation| {
                        try stack.append(allocator, continuation);
                    }
                },
                .switch_initialized_payload => |stmt| {
                    try stack.append(allocator, stmt.initialized_branch);
                    try stack.append(allocator, stmt.uninitialized_branch);
                },
                .str_match => |stmt| {
                    try stack.append(allocator, stmt.on_match);
                    try stack.append(allocator, stmt.on_miss);
                },
                .str_match_set => |stmt| {
                    for (store.getStrMatchArms(stmt.arms)) |arm| {
                        try stack.append(allocator, arm.on_match);
                    }
                    try stack.append(allocator, stmt.on_miss);
                },
                .join => |stmt| {
                    try stack.append(allocator, stmt.body);
                    try stack.append(allocator, stmt.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| {
                    try stack.append(allocator, stmt.next);
                },
                .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
    }

    const seedLocal = struct {
        fn go(
            set: *std.bit_set.DynamicBitSetUnmanaged,
            list: *std.ArrayList(u32),
            alloc: Allocator,
            rc: []const bool,
            index: u32,
        ) SolveError!void {
            if (index >= rc.len or !rc[index]) return;
            if (set.isSet(index)) return;
            set.set(index);
            try list.append(alloc, index);
        }
    }.go;

    // Seeds: every pinned proc's parameters and returned values reach the
    // host or a caller the solver cannot see.
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        if (!pinned.isSet(proc_index)) continue;
        for (store.getLocalSpan(proc.args)) |param| {
            try seedLocal(&visible, &work, allocator, rc_local, @intFromEnum(param));
        }
        for (ret_values[proc_index].items) |value| {
            try seedLocal(&visible, &work, allocator, rc_local, value);
        }
    }

    // Same-allocation edges, collected flat: unreachable statements only add
    // edges that widen the visible set, which is sound.
    var edges = std.ArrayList([2]u32).empty;
    defer edges.deinit(allocator);
    const addEdge = struct {
        fn go(
            list: *std.ArrayList([2]u32),
            alloc: Allocator,
            rc: []const bool,
            a: u32,
            b: u32,
        ) SolveError!void {
            if (a >= rc.len or !rc[a]) return;
            if (b >= rc.len or !rc[b]) return;
            if (a == b) return;
            try list.append(alloc, .{ a, b });
            try list.append(alloc, .{ b, a });
        }
    }.go;

    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_ref => |assign| {
                const target = @intFromEnum(assign.target);
                switch (assign.op) {
                    .local => |source| try addEdge(&edges, allocator, rc_local, target, @intFromEnum(source)),
                    .list_reinterpret => |op| try addEdge(&edges, allocator, rc_local, target, @intFromEnum(op.backing_ref)),
                    .nominal => |op| try addEdge(&edges, allocator, rc_local, target, @intFromEnum(op.backing_ref)),
                    .field => |op| try addEdge(&edges, allocator, rc_local, target, @intFromEnum(op.source)),
                    .tag_payload => |op| try addEdge(&edges, allocator, rc_local, target, @intFromEnum(op.source)),
                    .tag_payload_struct => |op| try addEdge(&edges, allocator, rc_local, target, @intFromEnum(op.source)),
                    .discriminant => {},
                }
            },
            .assign_struct => |assign| {
                for (store.getLocalSpan(assign.fields)) |field| {
                    try addEdge(&edges, allocator, rc_local, @intFromEnum(assign.target), @intFromEnum(field));
                }
            },
            .assign_list => |assign| {
                for (store.getLocalSpan(assign.elems)) |elem| {
                    try addEdge(&edges, allocator, rc_local, @intFromEnum(assign.target), @intFromEnum(elem));
                }
            },
            .assign_tag => |assign| {
                if (assign.payload) |payload| {
                    try addEdge(&edges, allocator, rc_local, @intFromEnum(assign.target), @intFromEnum(payload));
                }
            },
            .assign_packed_erased_fn => |assign| {
                if (assign.capture) |capture| {
                    try addEdge(&edges, allocator, rc_local, @intFromEnum(assign.target), @intFromEnum(capture));
                }
            },
            .str_match => |str_match| {
                for (store.getStrMatchSteps(str_match.steps)) |step| {
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| try addEdge(&edges, allocator, rc_local, @intFromEnum(local), @intFromEnum(str_match.source)),
                    }
                }
            },
            .str_match_set => |str_match_set| {
                for (store.getStrMatchArms(str_match_set.arms)) |arm| {
                    for (store.getStrMatchSteps(arm.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| try addEdge(&edges, allocator, rc_local, @intFromEnum(local), @intFromEnum(str_match_set.source)),
                        }
                    }
                }
            },
            .set_local => |assign| {
                try addEdge(&edges, allocator, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.value));
            },
            .assign_call => |assign| {
                const callee = store.proc_specs.items[@intFromEnum(assign.proc)];
                const args = store.getLocalSpan(assign.args);
                if (callee.body == null) {
                    // No body to flow through: everything at the boundary is
                    // host-visible.
                    for (args) |arg| {
                        try seedLocal(&visible, &work, allocator, rc_local, @intFromEnum(arg));
                    }
                    try seedLocal(&visible, &work, allocator, rc_local, @intFromEnum(assign.target));
                } else {
                    const params = store.getLocalSpan(callee.args);
                    for (args, 0..) |arg, position| {
                        if (position >= params.len) break;
                        try addEdge(&edges, allocator, rc_local, @intFromEnum(arg), @intFromEnum(params[position]));
                    }
                    for (ret_values[@intFromEnum(assign.proc)].items) |value| {
                        try addEdge(&edges, allocator, rc_local, @intFromEnum(assign.target), value);
                    }
                }
            },
            .assign_call_erased => |assign| {
                // The callee is unknown; the boundary is treated like a
                // pinned signature.
                try seedLocal(&visible, &work, allocator, rc_local, @intFromEnum(assign.closure));
                for (store.getLocalSpan(assign.args)) |arg| {
                    try seedLocal(&visible, &work, allocator, rc_local, @intFromEnum(arg));
                }
                try seedLocal(&visible, &work, allocator, rc_local, @intFromEnum(assign.target));
            },
            .assign_low_level => |assign| {
                const target = @intFromEnum(assign.target);
                if (assign.op == .erased_capture_load) {
                    // The loaded capture shares the callable's allocation
                    // through the executing frame, which value flow cannot
                    // see; erased-callable procs are pinned, so the capture
                    // is host-visible by construction.
                    try seedLocal(&visible, &work, allocator, rc_local, target);
                    continue;
                }
                const effect = assign.rc_effect;
                const args = store.getLocalSpan(assign.args);
                const share_mask = effect.result_aliases_consumed_args |
                    effect.result_borrows_args |
                    effect.retain_args |
                    effect.result_shares_args;
                if (share_mask != 0) {
                    for (args, 0..) |arg, position| {
                        if (position >= 64) break;
                        const bit = @as(u64, 1) << @as(u6, @intCast(position));
                        if ((share_mask & bit) == 0) continue;
                        try addEdge(&edges, allocator, rc_local, target, @intFromEnum(arg));
                    }
                } else if (effect.consume_args == 0) {
                    // The masks say nothing about this op; a refcounted
                    // result conservatively shares every refcounted
                    // argument's allocation.
                    for (args) |arg| {
                        try addEdge(&edges, allocator, rc_local, target, @intFromEnum(arg));
                    }
                }
            },
            else => {},
        }
    }

    // Adjacency lists over the collected edges.
    const out_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(out_lens);
    @memset(out_lens, 0);
    for (edges.items) |edge| out_lens[edge[0]] += 1;
    const out_offsets = try allocator.alloc(u32, local_count);
    defer allocator.free(out_offsets);
    var total: u32 = 0;
    for (out_lens, 0..) |len, index| {
        out_offsets[index] = total;
        total += len;
    }
    const out_edges = try allocator.alloc(u32, total);
    defer allocator.free(out_edges);
    const fill = try allocator.alloc(u32, local_count);
    defer allocator.free(fill);
    @memset(fill, 0);
    for (edges.items) |edge| {
        out_edges[out_offsets[edge[0]] + fill[edge[0]]] = edge[1];
        fill[edge[0]] += 1;
    }

    // Propagate to a fixpoint.
    while (work.pop()) |index| {
        const start = out_offsets[index];
        const len = out_lens[index];
        for (out_edges[start .. start + len]) |neighbor| {
            if (visible.isSet(neighbor)) continue;
            visible.set(neighbor);
            try work.append(allocator, neighbor);
        }
    }

    return visible;
}

/// Result of the born-unique analysis, one bit triple per local.
pub const Uniqueness = struct {
    /// Bit set => every definition of the local binds a value whose
    /// outermost allocation originated at a unique birth: a fresh aggregate
    /// or literal assignment, a low-level op whose `RcEffect` marks its
    /// result unique, a direct call whose callee's signature returns
    /// unique, or a pure same-value alias of a born-unique source. This is
    /// the origin property alone, independent of the holder accounting in
    /// `destroyed`, which keeps it stable across emission's statement
    /// cloning so the certifier can re-derive it from the final store.
    born_unique: std.bit_set.DynamicBitSetUnmanaged,
    /// Bit set => born unique and no statement can add another holder, so
    /// the count is still 1 at the local's single consuming use.
    unique: std.bit_set.DynamicBitSetUnmanaged,
    /// Bit set => some occurrence can add another holder (or consume the
    /// value a second time). Emission consults this for parameters a
    /// variant's demand vector seeds born-unique: the seed survives the
    /// body only when this bit is clear.
    destroyed: std.bit_set.DynamicBitSetUnmanaged,

    /// Frees all three bit sets.
    pub fn deinit(self: *Uniqueness, allocator: Allocator) void {
        self.born_unique.deinit(allocator);
        self.unique.deinit(allocator);
        self.destroyed.deinit(allocator);
    }
};

/// Marks every local whose value's outermost allocation provably has count 1
/// at the local's definition with nothing later adding a holder: born unique
/// by a fresh allocation or a direct call to a unique-returning callee,
/// destroyed by any occurrence that can create another handle to the
/// allocation — an incref, an aggregate or capture operand, a `set_local`
/// value or target, or a second consuming use. Consuming uses (a consumed
/// low-level argument, an owned-position direct-call argument, a return)
/// take the value's single unit with them, so the first one preserves
/// uniqueness and any further one destroys it; borrowed-position call
/// arguments and erased-call arguments conservatively destroy. A pure
/// same-value alias (`.local`, `.list_reinterpret`, `.nominal` — not
/// payload reads, which name interior allocations of a possibly-shared
/// outer value) inherits uniqueness: its definition is the chain's
/// consuming use of the source, so the source's single unit moves through
/// to the target, and any other occurrence of the source — consuming,
/// holder-adding, or a mere read, before or after, since the analysis is
/// flow-insensitive — destroys the target's uniqueness (a read elsewhere
/// forces emission to give the alias its own unit, holding the count above
/// 1). A multi-bound alias target never inherits. Variant parameter seeds
/// are not applied here: variants share parameter locals with their source
/// proc, so emission and the certifier overlay `RcSig.unique_params` per
/// proc. Statement iteration is flat, so unreachable statements only
/// destroy uniqueness, which is conservative-sound.
pub fn computeUniqueness(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
) SolveError!Uniqueness {
    const local_count = store.locals.items.len;

    var born = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer born.deinit(allocator);
    // A definition that is not a unique birth or a pure same-value alias
    // (parameters, payload reads, foreign calls, join params) poisons the
    // local outright.
    var foreign_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer foreign_def.deinit(allocator);
    var destroyed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer destroyed.deinit(allocator);
    var consumed_once = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer consumed_once.deinit(allocator);
    // Non-consuming, non-holder-adding reads (payload reads, borrowed
    // low-level arguments, expect/debug/switch operands). They never destroy
    // a local's own uniqueness — emission's path-sensitive facts cover the
    // checked argument itself — but they block alias inheritance, because a
    // source read anywhere keeps the source live past the alias definition.
    var borrow_used = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer borrow_used.deinit(allocator);
    var has_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer has_def.deinit(allocator);
    var multi_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer multi_def.deinit(allocator);

    // Single pure-alias source per target (`no_local` when the local is not
    // an alias target), plus the list of distinct alias targets to settle.
    const alias_source = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_source);
    @memset(alias_source, no_local);
    var alias_targets = std.ArrayList(u32).empty;
    defer alias_targets.deinit(allocator);

    const Marks = struct {
        rc: []const bool,

        fn noteBirth(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return;
            set.set(index);
        }

        fn destroy(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return;
            set.set(index);
        }

        fn noteUse(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return;
            set.set(index);
        }

        fn trackDef(
            self: @This(),
            seen: *std.bit_set.DynamicBitSetUnmanaged,
            multi: *std.bit_set.DynamicBitSetUnmanaged,
            local: LIR.LocalId,
        ) void {
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return;
            if (seen.isSet(index)) {
                multi.set(index);
            } else {
                seen.set(index);
            }
        }

        fn consume(
            self: @This(),
            once: *std.bit_set.DynamicBitSetUnmanaged,
            dead: *std.bit_set.DynamicBitSetUnmanaged,
            local: LIR.LocalId,
        ) void {
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return;
            if (once.isSet(index)) {
                dead.set(index);
            } else {
                once.set(index);
            }
        }
    };
    const marks = Marks{ .rc = rc_local };

    const Alias = struct {
        /// Records a pure same-value alias definition. The definition is the
        /// chain's consuming use of the source; a non-refcounted or
        /// self-referential source poisons the target, and distinct alias
        /// definitions binding different sources never inherit.
        fn record(
            m: Marks,
            alloc: Allocator,
            sources: []u32,
            targets: *std.ArrayList(u32),
            foreign: *std.bit_set.DynamicBitSetUnmanaged,
            once: *std.bit_set.DynamicBitSetUnmanaged,
            dead: *std.bit_set.DynamicBitSetUnmanaged,
            target: LIR.LocalId,
            source: LIR.LocalId,
        ) SolveError!void {
            const target_index = @intFromEnum(target);
            if (target_index >= m.rc.len or !m.rc[target_index]) return;
            const source_index = @intFromEnum(source);
            if (source_index >= m.rc.len or !m.rc[source_index] or source_index == target_index) {
                foreign.set(target_index);
                return;
            }
            m.consume(once, dead, source);
            if (sources[target_index] == no_local) {
                sources[target_index] = @intCast(source_index);
                try targets.append(alloc, @intCast(target_index));
            } else if (sources[target_index] != source_index) {
                foreign.set(target_index);
            }
        }
    };

    for (store.proc_specs.items) |proc| {
        for (store.getLocalSpan(proc.args)) |param| {
            marks.trackDef(&has_def, &multi_def, param);
            marks.destroy(&foreign_def, param);
        }
    }

    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_ref => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                switch (assign.op) {
                    .local => |source| try Alias.record(marks, allocator, alias_source, &alias_targets, &foreign_def, &consumed_once, &destroyed, assign.target, source),
                    .list_reinterpret => |op| try Alias.record(marks, allocator, alias_source, &alias_targets, &foreign_def, &consumed_once, &destroyed, assign.target, op.backing_ref),
                    .nominal => |op| try Alias.record(marks, allocator, alias_source, &alias_targets, &foreign_def, &consumed_once, &destroyed, assign.target, op.backing_ref),
                    .discriminant => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        marks.noteUse(&borrow_used, op.source);
                    },
                    .field => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        marks.noteUse(&borrow_used, op.source);
                    },
                    .tag_payload => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        marks.noteUse(&borrow_used, op.source);
                    },
                    .tag_payload_struct => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        marks.noteUse(&borrow_used, op.source);
                    },
                }
            },
            .assign_literal => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                switch (assign.value) {
                    // Static string and byte-list literals view backing whose
                    // count is the static sentinel, never 1, so it is not a
                    // unique birth and must never take an in-place path.
                    .str_literal, .bytes_literal => marks.destroy(&foreign_def, assign.target),
                    else => marks.noteBirth(&born, assign.target),
                }
            },
            .assign_call => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                const callee_sig = sigs.get(assign.proc);
                if (callee_sig.ret_unique) {
                    marks.noteBirth(&born, assign.target);
                } else {
                    marks.destroy(&foreign_def, assign.target);
                }
                for (store.getLocalSpan(assign.args), 0..) |arg, position| {
                    if (callee_sig.paramMode(position) == .owned) {
                        // The callee receives the argument's single unit;
                        // passing it is one consuming use, exactly like a
                        // consumed low-level argument.
                        marks.consume(&consumed_once, &destroyed, arg);
                    } else {
                        // A borrowed-position argument stays with the
                        // caller while the callee reads it; conservatively
                        // treat the call as another holder.
                        marks.destroy(&destroyed, arg);
                    }
                }
            },
            .assign_call_erased => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                marks.destroy(&destroyed, assign.closure);
                for (store.getLocalSpan(assign.args)) |arg| {
                    marks.destroy(&destroyed, arg);
                }
            },
            .assign_packed_erased_fn => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.capture) |capture| marks.destroy(&destroyed, capture);
            },
            .str_match => |str_match| {
                marks.noteUse(&borrow_used, str_match.source);
                for (store.getStrMatchSteps(str_match.steps)) |step| {
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| {
                            marks.trackDef(&has_def, &multi_def, local);
                            marks.destroy(&foreign_def, local);
                        },
                    }
                }
            },
            .str_match_set => |str_match_set| {
                marks.noteUse(&borrow_used, str_match_set.source);
                for (store.getStrMatchArms(str_match_set.arms)) |arm| {
                    for (store.getStrMatchSteps(arm.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| {
                                marks.trackDef(&has_def, &multi_def, local);
                                marks.destroy(&foreign_def, local);
                            },
                        }
                    }
                }
            },
            .assign_low_level => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                if (assign.rc_effect.result_unique) {
                    marks.noteBirth(&born, assign.target);
                } else {
                    marks.destroy(&foreign_def, assign.target);
                }
                for (store.getLocalSpan(assign.args), 0..) |arg, position| {
                    if (position >= 64) {
                        marks.destroy(&destroyed, arg);
                        continue;
                    }
                    const bit = @as(u64, 1) << @as(u6, @intCast(position));
                    var read_only = true;
                    if ((assign.rc_effect.consume_args & bit) != 0) {
                        marks.consume(&consumed_once, &destroyed, arg);
                        read_only = false;
                    }
                    if ((assign.rc_effect.retain_args & bit) != 0) {
                        marks.destroy(&destroyed, arg);
                        read_only = false;
                    }
                    if (read_only) {
                        marks.noteUse(&borrow_used, arg);
                    }
                }
            },
            .assign_list => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                for (store.getLocalSpan(assign.elems)) |elem| {
                    marks.destroy(&destroyed, elem);
                }
            },
            .assign_struct => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                for (store.getLocalSpan(assign.fields)) |field| {
                    marks.destroy(&destroyed, field);
                }
            },
            .assign_tag => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                if (assign.payload) |payload| marks.destroy(&destroyed, payload);
            },
            .set_local => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                marks.destroy(&destroyed, assign.target);
                marks.destroy(&destroyed, assign.value);
            },
            .incref => |rc| marks.destroy(&destroyed, rc.value),
            .join => |join_stmt| {
                for (store.getLocalSpan(join_stmt.params)) |param| {
                    marks.trackDef(&has_def, &multi_def, param);
                    marks.destroy(&foreign_def, param);
                }
            },
            // Returning is the value's consuming use: the unit moves to the
            // caller, which feeds the per-proc unique-return solve.
            .ret => |ret_stmt| marks.consume(&consumed_once, &destroyed, ret_stmt.value),
            .debug => |debug_stmt| marks.noteUse(&borrow_used, debug_stmt.message),
            // The failure report is the message's consuming use.
            .expect_err => |expect_err_stmt| marks.consume(&consumed_once, &destroyed, expect_err_stmt.message),
            .expect => |expect_stmt| marks.noteUse(&borrow_used, expect_stmt.condition),
            .init_uninitialized => {},
            .comptime_branch_taken => {},
            .switch_stmt => |switch_stmt| marks.noteUse(&borrow_used, switch_stmt.cond),
            .switch_initialized_payload => |switch_stmt| marks.noteUse(&borrow_used, switch_stmt.cond),
            .decref_if_initialized => |rc| marks.noteUse(&borrow_used, rc.cond),
            .decref, .free, .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
        }
    }

    // born_unique: every definition is a birth or a settled pure alias, and
    // no foreign definition. unique: born unique with no holder-adding
    // occurrence anywhere.
    var foreign_iter = foreign_def.iterator(.{});
    while (foreign_iter.next()) |index| born.unset(index);

    // An alias target's origin derives from its source, so a birth bit set
    // by another of its definitions must not stand on its own (the alias
    // definition may bind a non-unique value); and a multi-bound alias
    // target never inherits.
    for (alias_targets.items) |target| {
        born.unset(target);
        if (multi_def.isSet(target)) destroyed.set(target);
    }

    // Pure-alias chains settle to a fixpoint. The scan above fixed every
    // input (foreign poisons, holder-adding destroys, reads, multi-bound
    // defs), so the loop's bits flip monotonically: a target's birth only
    // turns on once its source has settled born, and destroys only
    // accumulate down the chain. Each round either flips at least one bit
    // or the loop stops, so the round count is bounded by two flips per
    // alias target.
    var alias_rounds: usize = 0;
    while (true) : (alias_rounds += 1) {
        if (alias_rounds > 2 * alias_targets.items.len + 1) {
            solveInvariant("ARC alias uniqueness solving did not converge");
        }
        var changed = false;
        for (alias_targets.items) |target| {
            const source = alias_source[target];
            if (!foreign_def.isSet(target) and !born.isSet(target) and born.isSet(source)) {
                born.set(target);
                changed = true;
            }
            // Any other occurrence of the source — a holder-adding or
            // second consuming use (destroyed) or a mere read
            // (borrow_used) — keeps the source live past the alias
            // definition, so the shared allocation's count exceeds 1 at
            // the target's consuming use.
            if (!destroyed.isSet(target) and
                (destroyed.isSet(source) or borrow_used.isSet(source)))
            {
                destroyed.set(target);
                changed = true;
            }
        }
        if (!changed) break;
    }

    var unique = try born.clone(allocator);
    errdefer unique.deinit(allocator);
    var destroyed_iter = destroyed.iterator(.{});
    while (destroyed_iter.next()) |index| unique.unset(index);

    return .{ .born_unique = born, .unique = unique, .destroyed = destroyed };
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
                .switch_initialized_payload => |s| {
                    try solver.stack.append(allocator, s.initialized_branch);
                    try solver.stack.append(allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try solver.stack.append(allocator, s.on_match);
                    try solver.stack.append(allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    for (store.getStrMatchArms(s.arms)) |arm| {
                        try solver.stack.append(allocator, arm.on_match);
                    }
                    try solver.stack.append(allocator, s.on_miss);
                },
                .join => |j| {
                    try solver.stack.append(allocator, j.body);
                    try solver.stack.append(allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try solver.stack.append(allocator, s.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
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
