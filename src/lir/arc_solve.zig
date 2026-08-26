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
//!   local alias (`.local`, `.list_reinterpret`, `.nominal`) whose source and
//!   target use the same explicit Boxy RC descriptor, a low-level op
//!   whose `RcEffect.result_borrows_args` names exactly one refcounted
//!   argument, or a call whose return borrows exactly one refcounted argument
//! - no occurrence of the binding demands ownership: it is never an owned
//!   call-argument position, a consumed or retained low-level argument, an
//!   aggregate or capture operand, a `set_local` source, or an owned return
//! - the lender chain resolves to a leader local that is bound exactly once:
//!   either an owned local (emission extends its lifetime past the borrow
//!   group's last use) or a borrowed parameter (live for the whole call)
//!
//! Signatures solve interprocedurally in two phases. Phase A uses exact
//! reverse dependencies to take parameter modes to a fixpoint with returns
//! pessimistically owned: parameters start borrowed and flip to owned when
//! any occurrence demands a unit, so every parameter bit is queued at most
//! once. Calls in tail position to procs in the same call-graph
//! strongly-connected component demand ownership of their arguments so
//! emission never needs a statement after the call. Phase B then marks
//! returns borrowed when every returned value is a borrow anchored on a
//! borrowed parameter, and re-solves binding modes so callers may borrow such
//! results. After signatures settle,
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
const builtin = @import("builtin");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Errors that can occur while constructing the ARC solver's internal tables.
pub const SolveError = std.mem.Allocator.Error;

/// Debug-only count of exact per-resource outcome work rows, used to pin the
/// polynomial state domain in scaling tests.
pub var outcome_solver_iterations: u64 = 0;

const no_local: u32 = std.math.maxInt(u32);

/// Presence-bit condition guarding a payload local whose storage may not be
/// initialized on every path into a join.
pub const MaybeUninitializedCondition = struct {
    /// Local containing the presence bitset word.
    local: LIR.LocalId,
    /// Bits that must all be set before the payload local is initialized.
    mask: u64,
};

/// Producer-authored join-body fact collected while the solver walks one
/// ownership-neutral procedure. ARC emission consumes these facts directly
/// when resolving jumps; it never rediscovers joins from the graph.
pub const JoinBody = struct {
    id: LIR.JoinPointId,
    body: LIR.CFStmtId,
    jump_count: u32 = 0,
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
    /// Bit set => this borrowed local is a direct-call result and therefore
    /// needs its own value-use liveness bit in emission.
    borrowed_call_result: std.bit_set.DynamicBitSetUnmanaged,
    /// Owned leader anchoring each local's liveness; the local itself when
    /// the binding is owned or is a borrowed parameter.
    leader: []u32,
    /// Source local of each pure same-value alias, or `no_local`.
    alias_source: []u32,
    /// Solved ownership signature per proc.
    sigs: []arc_sig.RcSig,
    /// Flat complete outcome rows referenced by `RcSig.outcomes`.
    outcomes: []arc_sig.Outcome,
    /// Optional outcome-conditioned calling convention available for each
    /// source proc. Base signatures remain unconditional; eligible direct
    /// call sites explicitly demand one of these spans and therefore select
    /// a separately emitted variant.
    available_outcome_spans: []arc_sig.OutcomeSpan,
    /// Entry-parameter units that escape through this exact return/jump
    /// boundary under a proved outcome, indexed by ownership-neutral stmt.
    restitution_params_by_stmt: []arc_sig.ParamMask,
    /// Parameter positions whose values can reach a consuming low-level
    /// runtime uniqueness check in this proc's ownership-neutral body.
    unique_seed_masks: []arc_sig.ParamMask,
    /// Flat join-body facts per source proc, indexed through the adjacent
    /// offsets and lengths.
    join_body_offsets: []u32,
    join_body_lens: []u32,
    join_bodies: []JoinBody,
    /// Compact join and jump-site indices assigned by the sole structural
    /// lift, indexed directly by ownership-neutral statement id.
    join_index_by_stmt: []u32,
    jump_target_join_index_by_stmt: []u32,
    jump_site_index_by_stmt: []u32,
    switch_count_by_proc: []u32,
    switch_index_by_stmt: []u32,
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
        self.borrowed_call_result.deinit(self.allocator);
        self.allocator.free(self.leader);
        self.allocator.free(self.alias_source);
        self.allocator.free(self.sigs);
        self.allocator.free(self.outcomes);
        self.allocator.free(self.available_outcome_spans);
        self.allocator.free(self.restitution_params_by_stmt);
        self.allocator.free(self.unique_seed_masks);
        self.allocator.free(self.join_body_offsets);
        self.allocator.free(self.join_body_lens);
        self.allocator.free(self.join_bodies);
        self.allocator.free(self.join_index_by_stmt);
        self.allocator.free(self.jump_target_join_index_by_stmt);
        self.allocator.free(self.jump_site_index_by_stmt);
        self.allocator.free(self.switch_count_by_proc);
        self.allocator.free(self.switch_index_by_stmt);
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

    pub fn isBorrowedCallResult(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.borrowed_call_result.isSet(index);
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

    pub fn sigTable(self: *const Solution) arc_sig.SigTable {
        return .{ .sigs = self.sigs, .outcomes = self.outcomes };
    }

    pub fn sigOf(self: *const Solution, proc: LIR.LirProcSpecId) arc_sig.RcSig {
        return self.sigTable().get(proc);
    }

    pub fn availableOutcomeSpanOf(self: *const Solution, proc: LIR.LirProcSpecId) arc_sig.OutcomeSpan {
        const index = @intFromEnum(proc);
        if (index >= self.available_outcome_spans.len) return .empty;
        return self.available_outcome_spans[index];
    }

    pub fn uniqueSeedMaskOf(self: *const Solution, proc: LIR.LirProcSpecId) arc_sig.ParamMask {
        const index = @intFromEnum(proc);
        if (index >= self.unique_seed_masks.len) solveInvariant("ARC uniqueness-seed lookup exceeded the solved proc table");
        return self.unique_seed_masks[index];
    }

    pub fn restitutionParamsAt(self: *const Solution, stmt: LIR.CFStmtId) arc_sig.ParamMask {
        const index = @intFromEnum(stmt);
        if (index >= self.restitution_params_by_stmt.len) return 0;
        return self.restitution_params_by_stmt[index];
    }

    pub fn joinBodiesOf(self: *const Solution, proc: LIR.LirProcSpecId) []const JoinBody {
        const index = @intFromEnum(proc);
        if (index >= self.join_body_offsets.len) return &.{};
        const offset = self.join_body_offsets[index];
        const len = self.join_body_lens[index];
        return self.join_bodies[offset..][0..len];
    }

    pub fn joinIndexOfStmt(self: *const Solution, stmt: LIR.CFStmtId) u32 {
        const index = @intFromEnum(stmt);
        if (index >= self.join_index_by_stmt.len or self.join_index_by_stmt[index] == no_local) {
            solveInvariant("ARC statement did not have a lifted join index");
        }
        return self.join_index_by_stmt[index];
    }

    pub fn jumpSiteIndexOf(self: *const Solution, stmt: LIR.CFStmtId) u32 {
        const index = @intFromEnum(stmt);
        if (index >= self.jump_site_index_by_stmt.len or self.jump_site_index_by_stmt[index] == no_local) {
            solveInvariant("ARC jump did not have a lifted contribution index");
        }
        return self.jump_site_index_by_stmt[index];
    }

    pub fn jumpTargetJoinIndexOf(self: *const Solution, stmt: LIR.CFStmtId) u32 {
        const index = @intFromEnum(stmt);
        if (index >= self.jump_target_join_index_by_stmt.len or self.jump_target_join_index_by_stmt[index] == no_local) {
            solveInvariant("ARC jump did not have a lifted target-join index");
        }
        return self.jump_target_join_index_by_stmt[index];
    }

    pub fn switchCountOf(self: *const Solution, proc: LIR.LirProcSpecId) u32 {
        const index = @intFromEnum(proc);
        if (index >= self.switch_count_by_proc.len) solveInvariant("ARC requested switch count for an unknown source procedure");
        return self.switch_count_by_proc[index];
    }

    pub fn switchIndexOfStmt(self: *const Solution, stmt: LIR.CFStmtId) u32 {
        const index = @intFromEnum(stmt);
        if (index >= self.switch_index_by_stmt.len or self.switch_index_by_stmt[index] == no_local) {
            solveInvariant("ARC continuation switch did not have a lifted compact index");
        }
        return self.switch_index_by_stmt[index];
    }
};

const DefKind = union(enum) {
    none,
    multi,
    fresh,
    borrow_capable: u32,
};

/// Compute whether each LIR local's committed representation contains RC state.
pub fn computeLocalContainsRefcounted(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
) SolveError![]bool {
    const local_count = store.localCount();
    if (boxy_rc_descs.len != 0 and boxy_rc_descs.len != local_count) {
        solveInvariant("ARC Boxy descriptor table did not cover every local");
    }
    const contains = try allocator.alloc(bool, local_count);
    errdefer allocator.free(contains);
    for (0..local_count) |index| {
        const local_id: LIR.LocalId = @enumFromInt(@as(u32, @intCast(index)));
        const local = store.getLocal(local_id);
        contains[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }

    var changed = true;
    while (changed) {
        changed = false;
        for (0..store.cfStmtCount()) |stmt_index| {
            const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .assign_ref) {
                const assign = stmt.assign_ref;
                switch (assign.op) {
                    .local => |source| changed = markLocalRcIfSourceRc(contains, assign.target, source) or changed,
                    .nominal => |op| changed = markLocalRcIfSourceRc(contains, assign.target, op.backing_ref) or changed,
                    .list_reinterpret => |op| changed = markLocalRcIfSourceRc(contains, assign.target, op.backing_ref) or changed,
                    .field, .tag_payload, .tag_payload_struct, .discriminant => {},
                }
            } else if (stmt == .assign_list) {
                const assign = stmt.assign_list;
                changed = markLocalRcIfSpanContainsRc(store, contains, assign.target, assign.elems) or changed;
            } else if (stmt == .assign_struct) {
                const assign = stmt.assign_struct;
                changed = markLocalRcIfSpanContainsRc(store, contains, assign.target, assign.fields) or changed;
            } else if (stmt == .assign_tag) {
                const assign = stmt.assign_tag;
                if (assign.payload) |payload| changed = markLocalRcIfSourceRc(contains, assign.target, payload) or changed;
                if (assign.target_desc != null) changed = markLocalRc(contains, assign.target) or changed;
            } else if (stmt == .assign_boxy_box) {
                changed = markLocalRc(contains, stmt.assign_boxy_box.target) or changed;
            } else if (stmt == .assign_boxy_reuse_box) {
                changed = markLocalRc(contains, stmt.assign_boxy_reuse_box.target) or changed;
            } else if (stmt == .assign_boxy_tag) {
                changed = markLocalRc(contains, stmt.assign_boxy_tag.target) or changed;
            }
        }
    }
    return contains;
}

fn markLocalRc(contains: []bool, local: LIR.LocalId) bool {
    const index = @intFromEnum(local);
    if (index >= contains.len or contains[index]) return false;
    contains[index] = true;
    return true;
}

fn markLocalRcIfSourceRc(contains: []bool, target: LIR.LocalId, source: LIR.LocalId) bool {
    const source_index = @intFromEnum(source);
    if (source_index >= contains.len or !contains[source_index]) return false;
    return markLocalRc(contains, target);
}

fn markLocalRcIfSpanContainsRc(store: *const LirStore, contains: []bool, target: LIR.LocalId, span: LIR.LocalSpan) bool {
    const locals = store.getLocalSpan(span);
    for (0..GuardedList.borrowLen(locals)) |span_index| {
        const local_index = @intFromEnum(GuardedList.at(locals, span_index));
        if (local_index < contains.len and contains[local_index]) return markLocalRc(contains, target);
    }
    return false;
}

/// Dense module-wide domain of locals that participate in ARC equations.
/// The producer-provided `rc_local` table is the exact membership rule; the
/// reverse table expands the final solution to LocalId-indexed consumer data
/// once, after every fixed point has settled.
const ArcLocalDomain = struct {
    local_to_arc: []u32,
    arc_to_local: []u32,

    fn init(allocator: Allocator, rc_local: []const bool) SolveError!ArcLocalDomain {
        const local_to_arc = try allocator.alloc(u32, rc_local.len);
        errdefer allocator.free(local_to_arc);
        @memset(local_to_arc, no_local);

        var count: usize = 0;
        for (rc_local) |is_rc| count += @intFromBool(is_rc);
        const arc_to_local = try allocator.alloc(u32, count);
        errdefer allocator.free(arc_to_local);

        var next: u32 = 0;
        for (rc_local, 0..) |is_rc, local_index| {
            if (!is_rc) continue;
            local_to_arc[local_index] = next;
            arc_to_local[next] = @intCast(local_index);
            next += 1;
        }
        return .{ .local_to_arc = local_to_arc, .arc_to_local = arc_to_local };
    }

    fn deinit(self: *ArcLocalDomain, allocator: Allocator) void {
        allocator.free(self.local_to_arc);
        allocator.free(self.arc_to_local);
    }

    fn indexOf(self: *const ArcLocalDomain, local: LIR.LocalId) ?u32 {
        const local_index = @intFromEnum(local);
        if (local_index >= self.local_to_arc.len) return null;
        const index = self.local_to_arc[local_index];
        return if (index == no_local) null else index;
    }

    fn indexOfRaw(self: *const ArcLocalDomain, local_index: u32) ?u32 {
        if (local_index >= self.local_to_arc.len) return null;
        const index = self.local_to_arc[local_index];
        return if (index == no_local) null else index;
    }

    fn localAt(self: *const ArcLocalDomain, arc_index: u32) u32 {
        if (arc_index >= self.arc_to_local.len) solveInvariant("ARC-local index exceeded its explicit domain");
        return self.arc_to_local[arc_index];
    }
};

const DirectCallFact = struct {
    caller: u32,
    callee: LIR.LirProcSpecId,
    args: LIR.LocalSpan,
    target: LIR.LocalId,
    tail: bool,
};

/// One structurally distinct direct-call statement. A neutral LIR body can
/// back more than one proc spec, so caller-sensitive call-graph facts live in
/// `DirectCallFact` while definition and occurrence facts are counted once
/// here, exactly like every other shared statement fact.
const UniqueCallFact = struct {
    callee: LIR.LirProcSpecId,
    args: LIR.LocalSpan,
    target: LIR.LocalId,
};

const BindingFact = union(enum) {
    fresh: LIR.LocalId,
    multi: LIR.LocalId,
    borrow: struct { target: LIR.LocalId, source: LIR.LocalId },
    alias: struct { target: LIR.LocalId, source: LIR.LocalId },
    demand: LIR.LocalId,
};

const VisibilityFact = union(enum) {
    link: struct { a: LIR.LocalId, b: LIR.LocalId },
    seed: LIR.LocalId,
};

const UniqueFact = union(enum) {
    birth: LIR.LocalId,
    foreign: LIR.LocalId,
    alias: struct { target: LIR.LocalId, source: LIR.LocalId },
    join_target: LIR.LocalId,
    join_incoming: struct { target: LIR.LocalId, source: LIR.LocalId },
    consume: LIR.LocalId,
    destroy: LIR.LocalId,
    read: LIR.LocalId,
};

const UniqueJoinIncoming = struct {
    target: u32,
    source: u32,
};

const ParamUseFact = struct {
    key: u32,
    argument: u32,
};

const PendingJump = struct {
    proc: u32,
    stmt: LIR.CFStmtId,
    target: LIR.JoinPointId,
};

const Solver = struct {
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
    consume_dead_boxes: bool,
    domain: *const ArcLocalDomain,
    sigs: []arc_sig.RcSig,
    unique_seed_masks: []arc_sig.ParamMask,
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
    /// (positions beyond the signature mask are recorded as owned-only).
    param_position: []u32,
    /// Proc owning each parameter local.
    param_proc: []u32,
    /// Join parameters discovered during collection.
    join_param: std.bit_set.DynamicBitSetUnmanaged,
    maybe_uninitialized_join_param: std.bit_set.DynamicBitSetUnmanaged,
    maybe_uninitialized_condition: []u32,
    maybe_uninitialized_condition_mask: []u64,
    /// Exact reverse dependencies from one callee parameter position to the
    /// caller argument locals whose ownership demand changes when that
    /// position flips from borrowed to owned.
    param_uses: std.ArrayList(ParamUseFact),
    /// Static ARC facts projected during the sole reachable-statement lift.
    /// Later analyses consume these typed facts without decoding LIR again.
    binding_facts: std.ArrayList(BindingFact),
    visibility_facts: std.ArrayList(VisibilityFact),
    unique_facts: std.ArrayList(UniqueFact),
    address_taken: std.bit_set.DynamicBitSetUnmanaged,
    /// Reachable returned locals and joins, partitioned by source proc.
    /// This is the sole structural walk of the ownership-neutral CFG. Every
    /// analysis below projects its exact facts from this shared inventory.
    proc_stmts: []std.ArrayList(LIR.CFStmtId),
    proc_returns: []std.ArrayList(u32),
    proc_join_bodies: []std.ArrayList(JoinBody),
    join_index_by_stmt: []u32,
    jump_target_join_index_by_stmt: []u32,
    jump_site_index_by_stmt: []u32,
    switch_count_by_proc: []u32,
    switch_index_by_stmt: []u32,
    pending_jumps: std.ArrayList(PendingJump),
    /// Reachable direct calls, retained for the return-mode binding update
    /// and unique-return dependency solve.
    direct_calls: std.ArrayList(DirectCallFact),
    unique_calls: std.ArrayList(UniqueCallFact),
    stack: std.ArrayList(LIR.CFStmtId),
};

fn inferenceRcEffect(solver: *const Solver, op: anytype, declared: anytype) @TypeOf(declared) {
    if (!solver.consume_dead_boxes and op == .box_unbox) return op.arcBorrowedResultVariant().?.rcEffect();
    return op.arcInferenceRcEffect(declared);
}

/// Solves binding modes and proc signatures for every local in the store.
pub fn solve(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
    roots: []const LIR.LirProcSpecId,
    consume_dead_boxes: bool,
) SolveError!Solution {
    const local_count = store.localCount();
    const proc_count = store.procSpecCount();
    if (boxy_rc_descs.len != 0 and boxy_rc_descs.len != local_count) {
        solveInvariant("ARC Boxy descriptor table did not cover every local");
    }
    var domain = try ArcLocalDomain.init(allocator, rc_local);
    defer domain.deinit(allocator);
    const arc_local_count = domain.arc_to_local.len;

    var solver = Solver{
        .allocator = allocator,
        .store = store,
        .rc_local = rc_local,
        .boxy_rc_descs = boxy_rc_descs,
        .consume_dead_boxes = consume_dead_boxes,
        .domain = &domain,
        .sigs = try allocator.alloc(arc_sig.RcSig, proc_count),
        .unique_seed_masks = try allocator.alloc(arc_sig.ParamMask, proc_count),
        .pinned = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count),
        .scc = try allocator.alloc(u32, proc_count),
        .defs = try allocator.alloc(DefKind, arc_local_count),
        .demand = try allocator.alloc(bool, arc_local_count),
        .alias_source = try allocator.alloc(u32, arc_local_count),
        .param_position = try allocator.alloc(u32, arc_local_count),
        .param_proc = try allocator.alloc(u32, arc_local_count),
        .join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, arc_local_count),
        .maybe_uninitialized_join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, arc_local_count),
        .maybe_uninitialized_condition = try allocator.alloc(u32, arc_local_count),
        .maybe_uninitialized_condition_mask = try allocator.alloc(u64, arc_local_count),
        .param_uses = .empty,
        .binding_facts = .empty,
        .visibility_facts = .empty,
        .unique_facts = .empty,
        .address_taken = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count),
        .proc_stmts = try allocator.alloc(std.ArrayList(LIR.CFStmtId), proc_count),
        .proc_returns = try allocator.alloc(std.ArrayList(u32), proc_count),
        .proc_join_bodies = try allocator.alloc(std.ArrayList(JoinBody), proc_count),
        .join_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount()),
        .jump_target_join_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount()),
        .jump_site_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount()),
        .switch_count_by_proc = try allocator.alloc(u32, proc_count),
        .switch_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount()),
        .pending_jumps = .empty,
        .direct_calls = .empty,
        .unique_calls = .empty,
        .stack = std.ArrayList(LIR.CFStmtId).empty,
    };
    @memset(solver.proc_stmts, .empty);
    @memset(solver.proc_returns, .empty);
    @memset(solver.proc_join_bodies, .empty);
    @memset(solver.join_index_by_stmt, no_local);
    @memset(solver.jump_target_join_index_by_stmt, no_local);
    @memset(solver.jump_site_index_by_stmt, no_local);
    @memset(solver.switch_count_by_proc, 0);
    @memset(solver.switch_index_by_stmt, no_local);
    var solver_sigs_kept = false;
    var solver_join_indices_kept = false;
    defer {
        if (solver_sigs_kept) {
            // Ownership of proc-indexed solution tables moved into Solution.
            solver.sigs = &.{};
            solver.unique_seed_masks = &.{};
        }
        if (!solver_sigs_kept) solver.pinned.deinit(allocator);
        allocator.free(solver.scc);
        allocator.free(solver.defs);
        allocator.free(solver.demand);
        allocator.free(solver.alias_source);
        allocator.free(solver.param_position);
        allocator.free(solver.param_proc);
        solver.join_param.deinit(allocator);
        solver.maybe_uninitialized_join_param.deinit(allocator);
        allocator.free(solver.maybe_uninitialized_condition);
        allocator.free(solver.maybe_uninitialized_condition_mask);
        solver.param_uses.deinit(allocator);
        solver.binding_facts.deinit(allocator);
        solver.visibility_facts.deinit(allocator);
        solver.unique_facts.deinit(allocator);
        solver.address_taken.deinit(allocator);
        for (solver.proc_stmts) |*stmts| stmts.deinit(allocator);
        allocator.free(solver.proc_stmts);
        for (solver.proc_returns) |*returns| returns.deinit(allocator);
        allocator.free(solver.proc_returns);
        for (solver.proc_join_bodies) |*joins| joins.deinit(allocator);
        allocator.free(solver.proc_join_bodies);
        if (!solver_join_indices_kept) {
            allocator.free(solver.join_index_by_stmt);
            allocator.free(solver.jump_target_join_index_by_stmt);
            allocator.free(solver.jump_site_index_by_stmt);
            allocator.free(solver.switch_count_by_proc);
            allocator.free(solver.switch_index_by_stmt);
        }
        solver.pending_jumps.deinit(allocator);
        solver.direct_calls.deinit(allocator);
        solver.unique_calls.deinit(allocator);
        solver.stack.deinit(allocator);
        if (!solver_sigs_kept) allocator.free(solver.sigs);
        if (!solver_sigs_kept) allocator.free(solver.unique_seed_masks);
    }

    @memset(solver.param_position, no_local);
    @memset(solver.param_proc, no_local);
    @memset(solver.unique_seed_masks, 0);
    @memset(solver.maybe_uninitialized_condition, no_local);
    @memset(solver.maybe_uninitialized_condition_mask, 0);

    try liftReachableStatements(&solver);
    resolveJumpIndices(&solver);
    try computePins(&solver, roots);
    try computeSccs(&solver);

    // Phase A: parameter-mode fixpoint with returns pessimistically owned.
    // Start non-pinned refcounted parameter positions borrowed; demands can
    // only flip positions to owned, so the borrowed set shrinks with each
    // queued change.
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        var sig = arc_sig.RcSig.all_owned;
        if (!solver.pinned.isSet(proc_index)) {
            const params = store.getLocalSpan(proc.args);
            for (0..GuardedList.borrowLen(params)) |position| {
                const param = GuardedList.at(params, position);
                const param_index = domain.indexOf(param) orelse continue;
                solver.param_position[param_index] = @intCast(position);
                solver.param_proc[param_index] = @intCast(proc_index);
                if (position < arc_sig.tracked_param_count) {
                    sig = sig.withBorrowedParam(position);
                }
            }
        }
        solver.sigs[proc_index] = sig;
    }

    // Collect graph facts exactly once with the optimistic parameter modes.
    // Every later signature change has an explicit reverse dependency: one
    // callee parameter position points to precisely the caller arguments it
    // newly demands. Because parameter bits only flip borrowed -> owned, a
    // simple worklist reaches the same least fixpoint without rescanning any
    // procedure body.
    try collectAll(&solver);
    try solveParameterModes(&solver);

    // Phase B: returns become borrowed when every returned value is a borrow
    // anchored on a borrowed parameter of this proc.
    var binding = try resolveBindings(&solver);
    defer binding.deinit(allocator);
    for (0..store.procSpecCount()) |proc_index| {
        if (solver.pinned.isSet(proc_index)) continue;
        if (retLenders(&solver, &binding, proc_index)) |lenders| {
            solver.sigs[proc_index].ret_mode = .borrowed;
            solver.sigs[proc_index].ret_lenders = lenders;
        }
    }

    // Final binding solve with the solved signatures: borrowed-return call
    // results become borrow-capable, and returned borrows of borrowed
    // parameters lose their return demand.
    var changed_call_results = std.ArrayList(u32).empty;
    defer changed_call_results.deinit(allocator);
    try updateDirectCallResultDefs(&solver, &changed_call_results);
    try updateBindingsAfterReturns(&solver, &binding, changed_call_results.items);
    if (builtin.mode == .Debug) {
        var independently_bound = try resolveBindings(&solver);
        defer independently_bound.deinit(allocator);
        if (!binding.borrowed.eql(independently_bound.borrowed) or
            !std.mem.eql(u32, binding.leader, independently_bound.leader))
        {
            solveInvariant("incremental return binding update disagreed with independent solve");
        }
    }

    var visible = try computeVisibilityFromFacts(allocator, &solver);
    errdefer visible.deinit(allocator);
    if (builtin.mode == .Debug) {
        var independently_visible = try computeVisibilityFromLift(allocator, store, rc_local, &solver.pinned, solver.proc_stmts, solver.proc_returns);
        defer independently_visible.deinit(allocator);
        if (!visible.eql(independently_visible)) solveInvariant("typed visibility facts disagreed with independent LIR analysis");
    }

    // Unique returns form a second monotone dependency graph: proc-return
    // bits feed direct-call result births, births feed exact alias targets,
    // and newly unique locals feed only the procs that return them. Collect
    // origin facts alongside the one statement scan and settle that graph by
    // worklist; no uniqueness rescan is needed.
    var unique_origins = try UniqueOriginFacts.init(allocator, &domain, proc_count);
    defer unique_origins.deinit();
    var dense_uniqueness = try computeUniquenessFromFacts(allocator, &solver, &unique_origins);
    {
        errdefer dense_uniqueness.deinit(allocator);
        try solveUniqueReturnModes(&solver, &dense_uniqueness, &unique_origins);
    }
    // Emission consumes the final bit and the destroyed set (for variant
    // parameter seeds); the born-unique origin set is re-derived by the
    // certifier.
    dense_uniqueness.born_unique.deinit(allocator);
    defer dense_uniqueness.unique.deinit(allocator);
    defer dense_uniqueness.destroyed.deinit(allocator);

    var unique = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer unique.deinit(allocator);
    var unique_destroyed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer unique_destroyed.deinit(allocator);
    for (domain.arc_to_local, 0..) |local, arc_index| {
        if (dense_uniqueness.unique.isSet(arc_index)) unique.set(local);
        if (dense_uniqueness.destroyed.isSet(arc_index)) unique_destroyed.set(local);
    }
    if (builtin.mode == .Debug) {
        var independently_unique = try computeUniquenessDetailed(allocator, store, rc_local, .{ .sigs = solver.sigs }, null, solver.proc_stmts, null, null, null, solver.consume_dead_boxes);
        defer independently_unique.deinit(allocator);
        if (!unique.eql(independently_unique.unique) or !unique_destroyed.eql(independently_unique.destroyed)) {
            solveInvariant("typed uniqueness facts disagreed with independent LIR analysis");
        }
    }
    const join_body_offsets = try allocator.alloc(u32, proc_count);
    errdefer allocator.free(join_body_offsets);
    const join_body_lens = try allocator.alloc(u32, proc_count);
    errdefer allocator.free(join_body_lens);
    var join_body_count: u32 = 0;
    for (solver.proc_join_bodies, 0..) |joins, proc_index| {
        join_body_offsets[proc_index] = join_body_count;
        join_body_lens[proc_index] = @intCast(joins.items.len);
        join_body_count += @intCast(joins.items.len);
    }
    const join_bodies = try allocator.alloc(JoinBody, join_body_count);
    errdefer allocator.free(join_bodies);
    for (solver.proc_join_bodies, 0..) |joins, proc_index| {
        const start = join_body_offsets[proc_index];
        @memcpy(join_bodies[start..][0..joins.items.len], joins.items);
    }

    // Expand the dense ARC solution exactly once for LocalId-indexed stage
    // consumers. Non-ARC locals are identity leaders with no ownership bits.
    var borrowed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer borrowed.deinit(allocator);
    var borrowed_call_result = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer borrowed_call_result.deinit(allocator);
    const leader = try allocator.alloc(u32, local_count);
    errdefer allocator.free(leader);
    const alias_source = try allocator.alloc(u32, local_count);
    errdefer allocator.free(alias_source);
    const maybe_uninitialized_condition = try allocator.alloc(u32, local_count);
    errdefer allocator.free(maybe_uninitialized_condition);
    const maybe_uninitialized_condition_mask = try allocator.alloc(u64, local_count);
    errdefer allocator.free(maybe_uninitialized_condition_mask);
    var join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer join_param.deinit(allocator);
    var maybe_uninitialized_join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer maybe_uninitialized_join_param.deinit(allocator);
    for (leader, 0..) |*entry, index| entry.* = @intCast(index);
    @memset(alias_source, no_local);
    @memset(maybe_uninitialized_condition, no_local);
    @memset(maybe_uninitialized_condition_mask, 0);
    for (domain.arc_to_local, 0..) |local_index, arc_index| {
        if (binding.borrowed.isSet(arc_index)) borrowed.set(local_index);
        leader[local_index] = domain.localAt(binding.leader[arc_index]);
        const source = solver.alias_source[arc_index];
        if (source != no_local) alias_source[local_index] = domain.localAt(source);
        if (solver.join_param.isSet(arc_index)) join_param.set(local_index);
        if (solver.maybe_uninitialized_join_param.isSet(arc_index)) {
            maybe_uninitialized_join_param.set(local_index);
            maybe_uninitialized_condition[local_index] = solver.maybe_uninitialized_condition[arc_index];
            maybe_uninitialized_condition_mask[local_index] = solver.maybe_uninitialized_condition_mask[arc_index];
        }
    }
    for (solver.unique_calls.items) |call| {
        const target = domain.indexOf(call.target) orelse continue;
        if (binding.borrowed.isSet(target)) borrowed_call_result.set(@intFromEnum(call.target));
    }

    var solution = Solution{
        .allocator = allocator,
        .borrowed = borrowed,
        .borrowed_call_result = borrowed_call_result,
        .leader = leader,
        .alias_source = alias_source,
        .sigs = solver.sigs,
        .outcomes = &.{},
        .available_outcome_spans = try allocator.alloc(arc_sig.OutcomeSpan, proc_count),
        .restitution_params_by_stmt = try allocator.alloc(arc_sig.ParamMask, store.cfStmtCount()),
        .unique_seed_masks = solver.unique_seed_masks,
        .join_body_offsets = join_body_offsets,
        .join_body_lens = join_body_lens,
        .join_bodies = join_bodies,
        .join_index_by_stmt = solver.join_index_by_stmt,
        .jump_target_join_index_by_stmt = solver.jump_target_join_index_by_stmt,
        .jump_site_index_by_stmt = solver.jump_site_index_by_stmt,
        .switch_count_by_proc = solver.switch_count_by_proc,
        .switch_index_by_stmt = solver.switch_index_by_stmt,
        .join_param = join_param,
        .maybe_uninitialized_join_param = maybe_uninitialized_join_param,
        .maybe_uninitialized_condition = maybe_uninitialized_condition,
        .maybe_uninitialized_condition_mask = maybe_uninitialized_condition_mask,
        .visible = visible,
        .unique = unique,
        .unique_destroyed = unique_destroyed,
        .pinned = solver.pinned,
    };
    @memset(solution.restitution_params_by_stmt, 0);
    @memset(solution.available_outcome_spans, .empty);
    solver_sigs_kept = true;
    solver_join_indices_kept = true;
    errdefer {
        solution.borrowed.deinit(allocator);
        solution.borrowed_call_result.deinit(allocator);
        allocator.free(solution.leader);
        allocator.free(solution.alias_source);
        allocator.free(solution.sigs);
        allocator.free(solution.outcomes);
        allocator.free(solution.available_outcome_spans);
        allocator.free(solution.restitution_params_by_stmt);
        allocator.free(solution.unique_seed_masks);
        allocator.free(solution.join_body_offsets);
        allocator.free(solution.join_body_lens);
        allocator.free(solution.join_bodies);
        allocator.free(solution.join_index_by_stmt);
        allocator.free(solution.jump_target_join_index_by_stmt);
        allocator.free(solution.jump_site_index_by_stmt);
        allocator.free(solution.switch_count_by_proc);
        allocator.free(solution.switch_index_by_stmt);
        solution.join_param.deinit(allocator);
        solution.maybe_uninitialized_join_param.deinit(allocator);
        allocator.free(solution.maybe_uninitialized_condition);
        allocator.free(solution.maybe_uninitialized_condition_mask);
        solution.visible.deinit(allocator);
        solution.unique.deinit(allocator);
        solution.unique_destroyed.deinit(allocator);
        solution.pinned.deinit(allocator);
    }

    try computeOutcomeRestitution(allocator, store, layouts, rc_local, consume_dead_boxes, &solution);

    return solution;
}

const OutcomeWalkState = struct {
    stmt: u32,
    present: bool,
    discriminant: u32,
};

const OutcomeAccum = struct {
    remaining_on_all_paths: arc_sig.ParamMask,
};

const OutcomeBitAccum = struct {
    present_on_all_paths: bool,
};

/// Primary value binding written by one ownership-neutral statement. The
/// outcome domain deliberately recognizes only `assign_tag` as a result
/// discriminant witness; every other write to that returned local kills the
/// witness before the statement transfer runs.
fn outcomeBindingTarget(stmt: LIR.CFStmt) ?LIR.LocalId {
    return switch (stmt) {
        inline .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .set_local,
        => |binding| binding.target,
        .store_struct => |store_stmt| store_stmt.dest,
        .store_tag => |store_stmt| store_stmt.dest,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .switch_stmt,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .boxy_tag_match,
        .join,
        .jump,
        .ret,
        .crash,
        .loop_continue,
        .loop_break,
        => null,
    };
}

fn outcomeLessThan(_: void, lhs: arc_sig.Outcome, rhs: arc_sig.Outcome) bool {
    return lhs.discriminant < rhs.discriminant;
}

fn outcomeLocalIsParam(
    solution: *const Solution,
    param: LIR.LocalId,
    local: LIR.LocalId,
) bool {
    // Outcome solving asks which exact entry value a pure same-value alias can
    // move on this path, before emission chooses retain versus move. Follow
    // the producer-authored alias relation even when the path-insensitive base
    // binding is owned; the outcome mask then makes that path's final-use
    // decision explicit to emission.
    var index = @intFromEnum(local);
    var steps: usize = 0;
    while (index < solution.alias_source.len and solution.alias_source[index] != no_local) {
        index = solution.alias_source[index];
        steps += 1;
        if (steps > solution.alias_source.len) solveInvariant("ARC outcome alias-source chain contained a cycle");
    }
    return index == @intFromEnum(param);
}

fn consumeOutcomeLocal(
    solution: *const Solution,
    param: LIR.LocalId,
    present: *bool,
    local: LIR.LocalId,
) bool {
    if (!outcomeLocalIsParam(solution, param, local)) return true;
    if (!present.*) return false;
    present.* = false;
    return true;
}

fn consumeOutcomeSpan(
    store: *const LirStore,
    solution: *const Solution,
    param: LIR.LocalId,
    present: *bool,
    span: LIR.LocalSpan,
) bool {
    const locals = store.getLocalSpan(span);
    for (0..GuardedList.borrowLen(locals)) |index| {
        if (!consumeOutcomeLocal(solution, param, present, GuardedList.at(locals, index))) return false;
    }
    return true;
}

fn consumeOutcomeTransfer(
    solution: *const Solution,
    param: LIR.LocalId,
    present: *bool,
    local: LIR.LocalId,
    mode: LIR.BoxyTransferMode,
) bool {
    return switch (mode) {
        .borrow, .copy => true,
        .move => consumeOutcomeLocal(solution, param, present, local),
    };
}

/// Derive the initial closed outcome-conditioned calling convention declared
/// in design.md. This analysis is deliberately exact: one unsupported normal
/// control/transfer shape rejects the whole proc and leaves its outcome span
/// empty.
fn computeOutcomeRestitution(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    consume_dead_boxes: bool,
    solution: *Solution,
) SolveError!void {
    var all_outcomes = std.ArrayList(arc_sig.Outcome).empty;
    errdefer all_outcomes.deinit(allocator);

    const escape_discriminants = try allocator.alloc(u32, store.cfStmtCount());
    defer allocator.free(escape_discriminants);
    const escape_masks = try allocator.alloc(arc_sig.ParamMask, store.cfStmtCount());
    defer allocator.free(escape_masks);
    const bit_escape_discriminants = try allocator.alloc(u32, store.cfStmtCount());
    defer allocator.free(bit_escape_discriminants);
    const bit_escape_present = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(bit_escape_present);
    const ambiguous_discriminant = no_local - 1;

    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        if (solution.isPinnedProc(proc_id)) continue;
        const proc = store.getProcSpec(proc_id);
        const body = proc.body orelse continue;
        if (layouts.getLayout(proc.ret_layout).tag != .tag_union) continue;

        var proc_stmts = std.ArrayList(LIR.CFStmtId).empty;
        defer proc_stmts.deinit(allocator);
        try collectProcStatements(allocator, store, body, &proc_stmts);
        var returned_local: ?LIR.LocalId = null;
        var return_shape_valid = true;
        for (proc_stmts.items) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt != .ret) continue;
            const local = stmt.ret.value;
            if (returned_local) |expected| {
                if (local != expected) {
                    return_shape_valid = false;
                    break;
                }
            } else {
                returned_local = local;
            }
        }
        if (!return_shape_valid or returned_local == null) continue;
        const ret_local = returned_local.?;
        const ret_index = @intFromEnum(ret_local);
        if (ret_index >= rc_local.len or !rc_local[ret_index]) continue;

        @memset(escape_discriminants, no_local);
        @memset(escape_masks, 0);
        var initial: arc_sig.ParamMask = 0;
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const bit = arc_sig.paramBit(position) orelse break;
            if (solution.sigs[proc_index].paramMode(position) != .owned) continue;
            const param = GuardedList.at(params, position);
            const local_index = @intFromEnum(param);
            if (local_index >= rc_local.len or !rc_local[local_index]) continue;
            initial |= bit;
        }
        if (initial == 0) continue;

        var joins = collections.DenseMap(LIR.JoinPointId, LIR.CFStmtId).init(allocator);
        defer joins.deinit();
        for (proc_stmts.items) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt != .join) continue;
            const join_point = stmt.join;
            const entry = try joins.getOrPut(join_point.id);
            if (entry.found_existing) solveInvariant("ARC outcome solve saw duplicate join ids");
            entry.value_ptr.* = join_point.body;
        }

        var accum = std.AutoHashMap(u16, OutcomeAccum).init(allocator);
        defer accum.deinit();
        var valid = true;
        var solved_param_count: usize = 0;
        for (0..GuardedList.borrowLen(params)) |param_position| {
            const param_bit = arc_sig.paramBit(param_position) orelse break;
            if ((initial & param_bit) == 0) continue;
            const active_param = GuardedList.at(params, param_position);
            @memset(bit_escape_discriminants, no_local);
            @memset(bit_escape_present, false);
            var bit_accum = std.AutoHashMap(u16, OutcomeBitAccum).init(allocator);
            defer bit_accum.deinit();
            var stack = std.ArrayList(OutcomeWalkState).empty;
            defer stack.deinit(allocator);
            var seen = std.AutoHashMap(OutcomeWalkState, void).init(allocator);
            defer seen.deinit();
            try stack.append(allocator, .{
                .stmt = @intFromEnum(body),
                .present = true,
                .discriminant = no_local,
            });
            while (stack.pop()) |walk| {
                const seen_entry = try seen.getOrPut(walk);
                if (seen_entry.found_existing) continue;
                if (@import("builtin").mode == .Debug) outcome_solver_iterations += 1;
                const current: LIR.CFStmtId = @enumFromInt(walk.stmt);
                const stmt = store.getCFStmt(current);
                var next_state = walk;
                if (outcomeBindingTarget(stmt)) |target| {
                    if (target == ret_local) next_state.discriminant = no_local;
                }

                const pushNext = struct {
                    fn go(list: *std.ArrayList(OutcomeWalkState), alloc: Allocator, state: OutcomeWalkState, next: LIR.CFStmtId) Allocator.Error!void {
                        var updated = state;
                        updated.stmt = @intFromEnum(next);
                        try list.append(alloc, updated);
                    }
                }.go;

                switch (stmt) {
                    .assign_ref => |assign| {
                        if (assign.target == active_param) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_literal => |assign| try pushNext(&stack, allocator, next_state, assign.next),
                    .init_uninitialized => |assign| {
                        if (assign.target == active_param) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_call => |assign| {
                        const callee_sig = solution.sigOf(assign.proc);
                        const args = store.getLocalSpan(assign.args);
                        for (0..GuardedList.borrowLen(args)) |position| {
                            if (callee_sig.paramMode(position) != .owned) continue;
                            if (!consumeOutcomeLocal(solution, active_param, &next_state.present, GuardedList.at(args, position))) {
                                valid = false;
                                break;
                            }
                        }
                        if (!valid) break;
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_call_erased, .assign_call_dict => {
                        valid = false;
                        break;
                    },
                    .assign_packed_erased_fn => |assign| {
                        if (assign.capture) |capture| {
                            if (!consumeOutcomeLocal(solution, active_param, &next_state.present, capture)) {
                                valid = false;
                                break;
                            }
                        }
                        if (assign.reuse) |reuse| {
                            if (!consumeOutcomeLocal(solution, active_param, &next_state.present, reuse)) {
                                valid = false;
                                break;
                            }
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_desc_ref => |assign| try pushNext(&stack, allocator, next_state, assign.next),
                    .assign_boxy_dict_ref => |assign| try pushNext(&stack, allocator, next_state, assign.next),
                    .assign_boxy_box => |assign| {
                        if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.payload, assign.payload_mode)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_reuse_box => |assign| {
                        if (!consumeOutcomeLocal(solution, active_param, &next_state.present, assign.source)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_unbox => |assign| {
                        if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.source, assign.source_mode)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_adapt => |assign| {
                        if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.source, assign.source_mode)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_inspect => |assign| {
                        if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.source, assign.source_mode)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_eq => |assign| {
                        if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.lhs, assign.source_mode) or
                            !consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.rhs, assign.source_mode))
                        {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_tag => |assign| {
                        if (assign.payload) |payload| {
                            if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, payload, assign.payload_mode)) {
                                valid = false;
                                break;
                            }
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_boxy_tag_payload => |assign| {
                        if (!consumeOutcomeTransfer(solution, active_param, &next_state.present, assign.source, assign.source_mode)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_low_level => |assign| {
                        const effect = if (!consume_dead_boxes and assign.op == .box_unbox)
                            assign.op.arcBorrowedResultVariant().?.rcEffect()
                        else
                            assign.op.arcInferenceRcEffect(assign.rc_effect);
                        const args = store.getLocalSpan(assign.args);
                        for (0..GuardedList.borrowLen(args)) |position| {
                            if (position >= 64) {
                                valid = false;
                                break;
                            }
                            const bit = @as(u64, 1) << @as(u6, @intCast(position));
                            if ((effect.consume_args & bit) == 0) continue;
                            if (!consumeOutcomeLocal(solution, active_param, &next_state.present, GuardedList.at(args, position))) {
                                valid = false;
                                break;
                            }
                        }
                        if (!valid) break;
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_list => |assign| {
                        if (!consumeOutcomeSpan(store, solution, active_param, &next_state.present, assign.elems)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_struct => |assign| {
                        if (!consumeOutcomeSpan(store, solution, active_param, &next_state.present, assign.fields)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .assign_tag => |assign| {
                        if (assign.payload) |payload| {
                            if (!consumeOutcomeLocal(solution, active_param, &next_state.present, payload)) {
                                valid = false;
                                break;
                            }
                        }
                        if (assign.target == active_param) {
                            valid = false;
                            break;
                        }
                        if (assign.target == ret_local) next_state.discriminant = assign.discriminant;
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .store_struct => |assign| {
                        if (!consumeOutcomeSpan(store, solution, active_param, &next_state.present, assign.fields)) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .store_tag => |assign| {
                        if (assign.payload) |payload| {
                            if (!consumeOutcomeLocal(solution, active_param, &next_state.present, payload)) {
                                valid = false;
                                break;
                            }
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .set_local => |assign| {
                        if (assign.target != assign.value and
                            !consumeOutcomeLocal(solution, active_param, &next_state.present, assign.value))
                        {
                            valid = false;
                            break;
                        }
                        if (assign.target == active_param) {
                            valid = false;
                            break;
                        }
                        try pushNext(&stack, allocator, next_state, assign.next);
                    },
                    .debug => |debug_stmt| try pushNext(&stack, allocator, next_state, debug_stmt.next),
                    .expect => |expect_stmt| try pushNext(&stack, allocator, next_state, expect_stmt.next),
                    .comptime_branch_taken => |marker| try pushNext(&stack, allocator, next_state, marker.next),
                    .incref, .decref, .decref_if_initialized, .free => {
                        valid = false;
                        break;
                    },
                    .switch_stmt => |switch_stmt| {
                        const branches = store.getCFSwitchBranches(switch_stmt.branches);
                        for (0..GuardedList.borrowLen(branches)) |index| {
                            try pushNext(&stack, allocator, next_state, GuardedList.at(branches, index).body);
                        }
                        try pushNext(&stack, allocator, next_state, switch_stmt.default_branch);
                    },
                    .switch_initialized_payload => |switch_stmt| {
                        try pushNext(&stack, allocator, next_state, switch_stmt.initialized_branch);
                        try pushNext(&stack, allocator, next_state, switch_stmt.uninitialized_branch);
                    },
                    .str_match => |str_match| {
                        try pushNext(&stack, allocator, next_state, str_match.on_match);
                        try pushNext(&stack, allocator, next_state, str_match.on_miss);
                    },
                    .str_match_set => |str_match_set| {
                        const arms = store.getStrMatchArms(str_match_set.arms);
                        for (0..GuardedList.borrowLen(arms)) |index| {
                            try pushNext(&stack, allocator, next_state, GuardedList.at(arms, index).on_match);
                        }
                        try pushNext(&stack, allocator, next_state, str_match_set.on_miss);
                    },
                    .boxy_tag_match => |tag_match| {
                        try pushNext(&stack, allocator, next_state, tag_match.on_match);
                        try pushNext(&stack, allocator, next_state, tag_match.on_miss);
                    },
                    .join => |join_stmt| try pushNext(&stack, allocator, next_state, join_stmt.remainder),
                    .jump => |jump_stmt| {
                        const target = joins.get(jump_stmt.target) orelse {
                            valid = false;
                            break;
                        };
                        const target_stmt = store.getCFStmt(target);
                        if (next_state.discriminant != no_local and
                            !(target_stmt == .ret and target_stmt.ret.value == ret_local))
                        {
                            valid = false;
                            break;
                        }
                        if (target_stmt == .ret and target_stmt.ret.value == ret_local and next_state.discriminant != no_local) {
                            const stmt_index = @intFromEnum(current);
                            const old = bit_escape_discriminants[stmt_index];
                            if (old == no_local) {
                                bit_escape_discriminants[stmt_index] = next_state.discriminant;
                                bit_escape_present[stmt_index] = next_state.present;
                            } else if (old == next_state.discriminant) {
                                bit_escape_present[stmt_index] = bit_escape_present[stmt_index] and next_state.present;
                            } else {
                                bit_escape_discriminants[stmt_index] = ambiguous_discriminant;
                                bit_escape_present[stmt_index] = false;
                            }
                        }
                        try pushNext(&stack, allocator, next_state, target);
                    },
                    .ret => |ret_stmt| {
                        if (ret_stmt.value != ret_local or next_state.discriminant == no_local) {
                            valid = false;
                            break;
                        }
                        const discriminant: u16 = @intCast(next_state.discriminant);
                        const entry = try bit_accum.getOrPut(discriminant);
                        if (entry.found_existing) {
                            entry.value_ptr.present_on_all_paths = entry.value_ptr.present_on_all_paths and next_state.present;
                        } else {
                            entry.value_ptr.* = .{ .present_on_all_paths = next_state.present };
                        }
                        const stmt_index = @intFromEnum(current);
                        const old = bit_escape_discriminants[stmt_index];
                        if (old == no_local) {
                            bit_escape_discriminants[stmt_index] = discriminant;
                            bit_escape_present[stmt_index] = next_state.present;
                        } else if (old == discriminant) {
                            bit_escape_present[stmt_index] = bit_escape_present[stmt_index] and next_state.present;
                        } else {
                            bit_escape_discriminants[stmt_index] = ambiguous_discriminant;
                            bit_escape_present[stmt_index] = false;
                        }
                    },
                    .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .crash => {},
                    .loop_continue, .loop_break => {
                        valid = false;
                        break;
                    },
                }
                if (!valid) break;
            }
            if (!valid) break;

            if (solved_param_count == 0) {
                var bit_iter = bit_accum.iterator();
                while (bit_iter.next()) |entry| {
                    try accum.put(entry.key_ptr.*, .{
                        .remaining_on_all_paths = if (entry.value_ptr.present_on_all_paths) param_bit else 0,
                    });
                }
            } else {
                if (bit_accum.count() != accum.count()) {
                    valid = false;
                    break;
                }
                var combined_iter = accum.iterator();
                while (combined_iter.next()) |entry| {
                    const bit_result = bit_accum.get(entry.key_ptr.*) orelse {
                        valid = false;
                        break;
                    };
                    if (bit_result.present_on_all_paths) entry.value_ptr.remaining_on_all_paths |= param_bit;
                }
                if (!valid) break;
            }

            for (bit_escape_discriminants, 0..) |discriminant, stmt_index| {
                if (discriminant == no_local) continue;
                const old = escape_discriminants[stmt_index];
                if (old == no_local) {
                    escape_discriminants[stmt_index] = discriminant;
                } else if (old != discriminant) {
                    escape_discriminants[stmt_index] = ambiguous_discriminant;
                    escape_masks[stmt_index] = 0;
                    continue;
                }
                if (discriminant != ambiguous_discriminant and bit_escape_present[stmt_index]) {
                    escape_masks[stmt_index] |= param_bit;
                }
            }
            solved_param_count += 1;
        }

        if (!valid or accum.count() == 0) {
            continue;
        }

        const start = all_outcomes.items.len;
        var iter = accum.iterator();
        while (iter.next()) |entry| {
            try all_outcomes.append(allocator, .{
                .discriminant = entry.key_ptr.*,
                .restituted_params = entry.value_ptr.remaining_on_all_paths,
            });
        }
        std.mem.sort(arc_sig.Outcome, all_outcomes.items[start..], {}, outcomeLessThan);
        if (start > std.math.maxInt(u32) or all_outcomes.items.len - start > std.math.maxInt(u32)) {
            solveInvariant("ARC outcome table exceeded its span representation");
        }
        solution.available_outcome_spans[proc_index] = .{
            .start = @intCast(start),
            .len = @intCast(all_outcomes.items.len - start),
        };
        for (escape_discriminants, 0..) |discriminant, stmt_index| {
            if (discriminant == no_local or discriminant == ambiguous_discriminant) continue;
            const outcome = accum.get(@intCast(discriminant)) orelse
                solveInvariant("ARC outcome escape named an unreturned discriminant");
            solution.restitution_params_by_stmt[stmt_index] = escape_masks[stmt_index] & outcome.remaining_on_all_paths;
        }
    }

    solution.outcomes = try all_outcomes.toOwnedSlice(allocator);
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
fn resolveBindings(solver: *Solver) SolveError!BindingResult {
    const allocator = solver.allocator;
    const local_count = solver.domain.arc_to_local.len;
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

        const leader_once_bound = paramIsBorrowed(solver, chain_leader) or
            leaderIsInitializedJoinParam(solver, chain_leader) or
            switch (solver.defs[chain_leader]) {
                .fresh, .borrow_capable => true,
                .none, .multi => false,
            };
        const leader_is_anchor = leader_once_bound and
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
    if (position >= arc_sig.tracked_param_count) return false;
    return solver.sigs[proc_index].paramMode(position) == .borrowed;
}

fn borrowQualifies(solver: *const Solver, index: u32) bool {
    if (solver.demand[index]) return false;
    return switch (solver.defs[index]) {
        .borrow_capable => true,
        .none, .multi, .fresh => false,
    };
}

/// A join parameter carries exactly one ownership unit into the join body at
/// every jump and holds it live across the whole body (released on exit paths,
/// transferred on back edges), so it anchors borrows just like an owned local
/// bound once: a borrow anchored on it is live for the whole body. Emission
/// keeps a join parameter's unit alive through the body already (its releases
/// belong to the join traversal, not to per-use death scans), so anchoring a
/// borrow here emits no retain/release pair. A maybe-uninitialized join
/// parameter may hold no unit on some entry, so it cannot anchor a borrow.
fn leaderIsInitializedJoinParam(solver: *const Solver, index: u32) bool {
    return solver.join_param.isSet(index) and !solver.maybe_uninitialized_join_param.isSet(index);
}

/// Reports the borrowed-parameter lender mask when every `ret` in the body
/// returns a borrow anchored on a borrowed parameter of this proc, ignoring
/// the return occurrence's own demand. Returns null when any path returns an
/// owned or foreign value.
fn retLenders(
    solver: *const Solver,
    binding: *const BindingResult,
    proc_index: usize,
) ?arc_sig.ParamMask {
    var lenders: arc_sig.ParamMask = 0;
    const returns = solver.proc_returns[proc_index].items;
    if (returns.len == 0) return null;
    for (returns) |value_local| {
        const value_index = solver.domain.indexOfRaw(value_local) orelse continue;
        // The returned value must be a borrow (or borrowed param) whose
        // leader is a borrowed parameter of this proc, and its only
        // ownership demand may be the return itself.
        const leader = binding.leader[value_index];
        const anchored = binding.borrowed.isSet(value_index) or value_index == leader;
        if (!anchored) return null;
        if (!paramIsBorrowed(solver, leader)) return null;
        if (solver.param_proc[leader] != proc_index) return null;
        if (solver.demand[value_index]) return null;
        const position = solver.param_position[leader];
        const bit = arc_sig.paramBit(position) orelse return null;
        lenders |= bit;
    }

    if (lenders == 0) return null;
    return lenders;
}

const UniqueReturnWork = struct {
    solver: *Solver,
    uniqueness: *Uniqueness,
    origins: *UniqueOriginFacts,
    remaining_returns: []u32,
    return_blocked: []const bool,
    return_offsets: []const u32,
    return_lens: []const u32,
    return_edges: []const u32,
    alias_offsets: []const u32,
    alias_lens: []const u32,
    alias_edges: []const u32,
    join_offsets: []const u32,
    join_lens: []const u32,
    join_edges: []const u32,
    join_incoming_counts: []const u32,
    join_remaining: []u32,
    proc_work: *std.ArrayList(u32),
    born_work: *std.ArrayList(u32),

    fn seedProc(self: *@This(), proc_index: u32) SolveError!void {
        if (self.return_blocked[proc_index]) return;
        if (self.solver.proc_returns[proc_index].items.len == 0) return;
        if (self.solver.pinned.isSet(proc_index)) return;
        if (self.solver.sigs[proc_index].ret_unique) return;
        self.solver.sigs[proc_index].ret_unique = true;
        try self.proc_work.append(self.solver.allocator, proc_index);
    }

    fn noteUnique(self: *@This(), local: u32) SolveError!void {
        const start = self.return_offsets[local];
        const end = start + self.return_lens[local];
        for (self.return_edges[start..end]) |proc_index| {
            if (self.remaining_returns[proc_index] == 0) {
                solveInvariant("ARC unique-return dependency was satisfied twice");
            }
            self.remaining_returns[proc_index] -= 1;
            if (self.remaining_returns[proc_index] == 0) try self.seedProc(proc_index);
        }
    }

    fn attemptBorn(self: *@This(), local: u32) SolveError!void {
        if (self.uniqueness.born_unique.isSet(local)) return;
        if (self.origins.static_foreign.isSet(local)) return;
        if (self.origins.remaining_nonunique_calls[local] != 0) return;

        if (self.origins.join_targets.isSet(local)) {
            if (self.join_incoming_counts[local] == 0 or self.join_remaining[local] != 0) return;
        } else {
            const source = self.origins.alias_source[local];
            if (source != no_local) {
                if (!self.uniqueness.born_unique.isSet(source)) return;
            } else if (!self.origins.static_birth.isSet(local) and self.origins.call_count[local] == 0) {
                return;
            }
        }

        self.uniqueness.born_unique.set(local);
        try self.born_work.append(self.solver.allocator, local);
        if (!self.uniqueness.destroyed.isSet(local) and !self.uniqueness.unique.isSet(local)) {
            self.uniqueness.unique.set(local);
            try self.noteUnique(local);
        }
    }

    fn run(self: *@This()) SolveError!void {
        while (self.proc_work.items.len != 0 or self.born_work.items.len != 0) {
            while (self.proc_work.pop()) |proc_index| {
                for (self.origins.call_targets_by_callee[proc_index].items) |target| {
                    if (self.origins.remaining_nonunique_calls[target] == 0) {
                        solveInvariant("ARC unique call dependency was satisfied twice");
                    }
                    self.origins.remaining_nonunique_calls[target] -= 1;
                    if (self.origins.remaining_nonunique_calls[target] == 0) try self.attemptBorn(target);
                }
            }
            while (self.born_work.pop()) |source| {
                const start = self.alias_offsets[source];
                const end = start + self.alias_lens[source];
                for (self.alias_edges[start..end]) |target| try self.attemptBorn(target);
                const join_start = self.join_offsets[source];
                const join_end = join_start + self.join_lens[source];
                for (self.join_edges[join_start..join_end]) |target| {
                    if (self.join_remaining[target] == 0) {
                        solveInvariant("ARC unique-return join dependency was satisfied twice");
                    }
                    self.join_remaining[target] -= 1;
                    if (self.join_remaining[target] == 0) try self.attemptBorn(target);
                }
            }
        }
    }
};

/// Settles unique-return bits over exact return, direct-call, and pure-alias
/// dependencies. Every proc and local bit is enqueued at most once.
fn solveUniqueReturnModes(
    solver: *Solver,
    uniqueness: *Uniqueness,
    origins: *UniqueOriginFacts,
) SolveError!void {
    const allocator = solver.allocator;
    const local_count = solver.domain.arc_to_local.len;
    const proc_count = solver.sigs.len;

    const alias_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_lens);
    @memset(alias_lens, 0);
    for (origins.alias_targets.items) |target| alias_lens[origins.alias_source[target]] += 1;
    const alias_offsets = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_offsets);
    var alias_count: u32 = 0;
    for (alias_lens, 0..) |len, index| {
        alias_offsets[index] = alias_count;
        alias_count += len;
    }
    const alias_edges = try allocator.alloc(u32, alias_count);
    defer allocator.free(alias_edges);
    const alias_fill = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_fill);
    @memset(alias_fill, 0);
    for (origins.alias_targets.items) |target| {
        const source = origins.alias_source[target];
        alias_edges[alias_offsets[source] + alias_fill[source]] = target;
        alias_fill[source] += 1;
    }

    const join_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(join_lens);
    @memset(join_lens, 0);
    const join_incoming_counts = try allocator.alloc(u32, local_count);
    defer allocator.free(join_incoming_counts);
    @memset(join_incoming_counts, 0);
    const join_remaining = try allocator.alloc(u32, local_count);
    defer allocator.free(join_remaining);
    @memset(join_remaining, 0);
    for (origins.join_incoming.items) |incoming| {
        join_lens[incoming.source] += 1;
        join_incoming_counts[incoming.target] += 1;
        if (!uniqueness.born_unique.isSet(incoming.source)) join_remaining[incoming.target] += 1;
    }
    const join_offsets = try allocator.alloc(u32, local_count);
    defer allocator.free(join_offsets);
    var join_count: u32 = 0;
    for (join_lens, 0..) |len, index| {
        join_offsets[index] = join_count;
        join_count += len;
    }
    const join_edges = try allocator.alloc(u32, join_count);
    defer allocator.free(join_edges);
    const join_fill = try allocator.alloc(u32, local_count);
    defer allocator.free(join_fill);
    @memset(join_fill, 0);
    for (origins.join_incoming.items) |incoming| {
        join_edges[join_offsets[incoming.source] + join_fill[incoming.source]] = incoming.target;
        join_fill[incoming.source] += 1;
    }

    const return_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(return_lens);
    @memset(return_lens, 0);
    const remaining_returns = try allocator.alloc(u32, proc_count);
    defer allocator.free(remaining_returns);
    @memset(remaining_returns, 0);
    const return_blocked = try allocator.alloc(bool, proc_count);
    defer allocator.free(return_blocked);
    @memset(return_blocked, false);
    var return_edge_count: u32 = 0;
    for (solver.proc_returns, 0..) |returns, proc_index| {
        for (returns.items) |local| {
            const arc_local = solver.domain.indexOfRaw(local) orelse {
                return_blocked[proc_index] = true;
                continue;
            };
            if (uniqueness.unique.isSet(arc_local)) continue;
            remaining_returns[proc_index] += 1;
            return_lens[arc_local] += 1;
            return_edge_count += 1;
        }
    }
    const return_offsets = try allocator.alloc(u32, local_count);
    defer allocator.free(return_offsets);
    var return_offset: u32 = 0;
    for (return_lens, 0..) |len, index| {
        return_offsets[index] = return_offset;
        return_offset += len;
    }
    const return_edges = try allocator.alloc(u32, return_edge_count);
    defer allocator.free(return_edges);
    const return_fill = try allocator.alloc(u32, local_count);
    defer allocator.free(return_fill);
    @memset(return_fill, 0);
    for (solver.proc_returns, 0..) |returns, proc_index| {
        for (returns.items) |local| {
            const arc_local = solver.domain.indexOfRaw(local) orelse continue;
            if (uniqueness.unique.isSet(arc_local)) continue;
            return_edges[return_offsets[arc_local] + return_fill[arc_local]] = @intCast(proc_index);
            return_fill[arc_local] += 1;
        }
    }

    var proc_work = std.ArrayList(u32).empty;
    defer proc_work.deinit(allocator);
    var born_work = std.ArrayList(u32).empty;
    defer born_work.deinit(allocator);
    var work = UniqueReturnWork{
        .solver = solver,
        .uniqueness = uniqueness,
        .origins = origins,
        .remaining_returns = remaining_returns,
        .return_blocked = return_blocked,
        .return_offsets = return_offsets,
        .return_lens = return_lens,
        .return_edges = return_edges,
        .alias_offsets = alias_offsets,
        .alias_lens = alias_lens,
        .alias_edges = alias_edges,
        .join_offsets = join_offsets,
        .join_lens = join_lens,
        .join_edges = join_edges,
        .join_incoming_counts = join_incoming_counts,
        .join_remaining = join_remaining,
        .proc_work = &proc_work,
        .born_work = &born_work,
    };
    var join_target_iter = origins.join_targets.iterator(.{});
    while (join_target_iter.next()) |target| try work.attemptBorn(@intCast(target));
    for (0..proc_count) |proc_index| {
        if (remaining_returns[proc_index] == 0) try work.seedProc(@intCast(proc_index));
    }
    try work.run();
}

/// Lifts each procedure's reachable ownership-neutral statements exactly
/// once. The lists are the producer-authored CFG projected into a stable
/// per-procedure inventory; pins, call SCCs, binding/signature facts,
/// visibility, uniqueness, returns, and joins all consume this same lift.
fn liftReachableStatements(solver: *Solver) SolveError!void {
    var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(solver.allocator, solver.store.cfStmtCount());
    defer seen.deinit(solver.allocator);
    var facts_seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(solver.allocator, solver.store.cfStmtCount());
    defer facts_seen.deinit(solver.allocator);

    for (0..solver.store.procSpecCount()) |proc_index| {
        const proc = solver.store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const params = solver.store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |param_index| {
            const param = GuardedList.at(params, param_index);
            try solver.unique_facts.append(solver.allocator, .{ .foreign = param });
        }
        // Bodyless procedures still have ABI parameter definitions. They
        // contribute no reachable statements, but their params are foreign
        // uniqueness origins just like the independent whole-store model.
        const body = proc.body orelse continue;
        for (0..GuardedList.borrowLen(params)) |param_index| {
            try solver.binding_facts.append(solver.allocator, .{ .fresh = GuardedList.at(params, param_index) });
        }
        const stmts = &solver.proc_stmts[proc_index];
        solver.stack.clearRetainingCapacity();
        try solver.stack.append(solver.allocator, body);
        while (solver.stack.pop()) |current| {
            const stmt_index = @intFromEnum(current);
            if (seen.isSet(stmt_index)) continue;
            seen.set(stmt_index);
            try stmts.append(solver.allocator, current);
            const stmt = solver.store.getCFStmt(current);
            try liftProcStmtFacts(solver, @intCast(proc_index), proc.args, current);
            if (!facts_seen.isSet(stmt_index)) {
                facts_seen.set(stmt_index);
                try liftSharedStmtFacts(solver, current);
            }
            try appendStructuralSuccessors(solver.allocator, solver.store, &solver.stack, stmt);
        }
        for (stmts.items) |stmt| seen.unset(@intFromEnum(stmt));
    }
}

/// Projects facts whose identity includes the proc spec using a neutral body.
/// The same statement may be visited here for several procs; statement-local
/// definition and occurrence facts are separately lifted exactly once below.
fn liftProcStmtFacts(
    solver: *Solver,
    proc_index: u32,
    proc_params: LIR.LocalSpan,
    current: LIR.CFStmtId,
) SolveError!void {
    switch (solver.store.getCFStmt(current)) {
        .assign_call => |assign| try solver.direct_calls.append(solver.allocator, .{
            .caller = proc_index,
            .callee = assign.proc,
            .args = assign.args,
            .target = assign.target,
            .tail = blk: {
                const next = solver.store.getCFStmt(assign.next);
                break :blk next == .ret and next.ret.value == assign.target;
            },
        }),
        .assign_low_level => |assign| {
            const rc_effect = inferenceRcEffect(solver, assign.op, assign.rc_effect);
            solver.unique_seed_masks[proc_index] |= lowLevelUniqueSeedMask(
                solver.store,
                proc_params,
                assign.args,
                rc_effect,
            );
        },
        .ret => |ret_stmt| try solver.proc_returns[proc_index].append(solver.allocator, @intFromEnum(ret_stmt.value)),
        .join => |join_stmt| {
            const joins = &solver.proc_join_bodies[proc_index];
            const join_index: u32 = @intCast(joins.items.len);
            const stmt_index = @intFromEnum(current);
            if (solver.join_index_by_stmt[stmt_index] == no_local) {
                solver.join_index_by_stmt[stmt_index] = join_index;
            } else if (solver.join_index_by_stmt[stmt_index] != join_index) {
                solveInvariant("shared ARC join statement had different structural indices across proc specs");
            }
            try joins.append(solver.allocator, .{
                .id = join_stmt.id,
                .body = join_stmt.body,
            });
        },
        .jump => |jump_stmt| try solver.pending_jumps.append(solver.allocator, .{
            .proc = proc_index,
            .stmt = current,
            .target = jump_stmt.target,
        }),
        .switch_stmt => |switch_stmt| if (switch_stmt.continuation != null) {
            const stmt_index = @intFromEnum(current);
            const switch_index = solver.switch_count_by_proc[proc_index];
            if (solver.switch_index_by_stmt[stmt_index] == no_local) {
                solver.switch_index_by_stmt[stmt_index] = switch_index;
            } else if (solver.switch_index_by_stmt[stmt_index] != switch_index) {
                solveInvariant("shared ARC switch statement had different structural indices across proc specs");
            }
            solver.switch_count_by_proc[proc_index] += 1;
        },
        .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .boxy_tag_match,
        .assign_call_dict,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .loop_continue,
        .loop_break,
        .crash,
        => {},
    }
}

fn lowLevelUniqueSeedMask(
    store: *const LirStore,
    params_span: LIR.LocalSpan,
    args_span: LIR.LocalSpan,
    rc_effect: LIR.LowLevel.RcEffect,
) arc_sig.ParamMask {
    const check_mask = rc_effect.may_runtime_uniqueness_check_args & rc_effect.consume_args;
    if (check_mask == 0) return 0;

    const params = store.getLocalSpan(params_span);
    const args = store.getLocalSpan(args_span);
    var mask: arc_sig.ParamMask = 0;
    for (0..GuardedList.borrowLen(args)) |arg_position| {
        if (arg_position >= 64) break;
        if ((check_mask & (@as(u64, 1) << @as(u6, @intCast(arg_position)))) == 0) continue;
        const arg = GuardedList.at(args, arg_position);
        for (0..@min(GuardedList.borrowLen(params), arc_sig.tracked_param_count)) |param_position| {
            if (arg != GuardedList.at(params, param_position)) continue;
            mask |= arc_sig.paramBit(param_position).?;
            break;
        }
    }
    return mask;
}

/// Resolves every lifted jump to the compact join index and per-join
/// contribution slot assigned by the same structural lift. Join ids are
/// producer identities, not an indexing domain; downstream ARC code only
/// consumes these dense indices.
fn resolveJumpIndices(solver: *Solver) void {
    for (solver.pending_jumps.items) |pending| {
        const joins = &solver.proc_join_bodies[pending.proc];
        var target_index: ?u32 = null;
        for (joins.items, 0..) |join, join_index| {
            if (join.id != pending.target) continue;
            if (target_index != null) solveInvariant("ARC lift found duplicate join ids in one procedure");
            target_index = @intCast(join_index);
        }
        const join_index = target_index orelse solveInvariant("ARC jump targeted a join absent from its lifted procedure");
        const join = &joins.items[join_index];
        const stmt_index = @intFromEnum(pending.stmt);
        if (solver.jump_target_join_index_by_stmt[stmt_index] == no_local) {
            solver.jump_target_join_index_by_stmt[stmt_index] = join_index;
            solver.jump_site_index_by_stmt[stmt_index] = join.jump_count;
        } else if (solver.jump_target_join_index_by_stmt[stmt_index] != join_index or
            solver.jump_site_index_by_stmt[stmt_index] != join.jump_count)
        {
            solveInvariant("shared ARC jump statement had different structural indices across proc specs");
        }
        join.jump_count += 1;
    }
}

fn appendStructuralSuccessors(
    allocator: Allocator,
    store: *const LirStore,
    stack: *std.ArrayList(LIR.CFStmtId),
    stmt: LIR.CFStmt,
) SolveError!void {
    switch (stmt) {
        .switch_stmt => |switch_stmt| {
            const branches = store.getCFSwitchBranches(switch_stmt.branches);
            for (0..GuardedList.borrowLen(branches)) |branch_index| {
                try stack.append(allocator, GuardedList.at(branches, branch_index).body);
            }
            try stack.append(allocator, switch_stmt.default_branch);
            if (switch_stmt.continuation) |continuation| try stack.append(allocator, continuation);
        },
        .switch_initialized_payload => |switch_stmt| {
            try stack.append(allocator, switch_stmt.initialized_branch);
            try stack.append(allocator, switch_stmt.uninitialized_branch);
        },
        .str_match => |str_match| {
            try stack.append(allocator, str_match.on_match);
            try stack.append(allocator, str_match.on_miss);
        },
        .str_match_set => |str_match_set| {
            const arms = store.getStrMatchArms(str_match_set.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                try stack.append(allocator, GuardedList.at(arms, arm_index).on_match);
            }
            try stack.append(allocator, str_match_set.on_miss);
        },
        .boxy_tag_match => |tag_match| {
            try stack.append(allocator, tag_match.on_match);
            try stack.append(allocator, tag_match.on_miss);
        },
        .join => |join_stmt| {
            try stack.append(allocator, join_stmt.body);
            try stack.append(allocator, join_stmt.remainder);
        },
        inline .assign_ref,
        .assign_literal,
        .init_uninitialized,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        => |linear| try stack.append(allocator, linear.next),
        .jump,
        .ret,
        .crash,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .loop_continue,
        .loop_break,
        => {},
    }
}

/// Exact reachable statement set used by independent ARC certifier mirrors.
/// The main solver retains the stronger per-proc inventory from its one lift.
fn reachableStatementSet(
    allocator: Allocator,
    store: *const LirStore,
    only_proc: ?LIR.LirProcSpecId,
) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    var reachable = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.cfStmtCount());
    errdefer reachable.deinit(allocator);
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);

    if (only_proc) |proc_id| {
        if (store.getProcSpec(proc_id).body) |body| try stack.append(allocator, body);
    } else {
        for (0..store.procSpecCount()) |proc_index| {
            const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
            if (proc.body) |body| try stack.append(allocator, body);
        }
    }
    while (stack.pop()) |current| {
        const stmt_index = @intFromEnum(current);
        if (reachable.isSet(stmt_index)) continue;
        reachable.set(stmt_index);
        try appendStructuralSuccessors(allocator, store, &stack, store.getCFStmt(current));
    }
    return reachable;
}

/// Collect one procedure's exact reachable statement inventory without a
/// store-wide statement bitset. Final-LIR certifiers reuse this inventory for
/// proc-local analyses whose sibling variants share source LocalIds.
pub fn collectProcStatements(
    allocator: Allocator,
    store: *const LirStore,
    body: LIR.CFStmtId,
    stmts: *std.ArrayList(LIR.CFStmtId),
) SolveError!void {
    stmts.clearRetainingCapacity();
    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, body);

    while (stack.pop()) |current| {
        const entry = try visited.getOrPut(current);
        if (entry.found_existing) continue;
        try stmts.append(allocator, current);
        try appendStructuralSuccessors(allocator, store, &stack, store.getCFStmt(current));
    }
}

fn collectAll(solver: *Solver) SolveError!void {
    @memset(solver.defs, .none);
    @memset(solver.demand, false);
    @memset(solver.alias_source, no_local);

    for (solver.binding_facts.items) |fact| switch (fact) {
        .fresh => |local| noteDef(solver, local, .fresh),
        .multi => |local| noteDef(solver, local, .multi),
        .borrow => |borrow| noteBorrowDef(solver, borrow.target, borrow.source),
        .alias => |alias| noteAlias(solver, alias.target, alias.source),
        .demand => |local| noteDemand(solver, local),
    };

    // Direct-call demands are the only binding facts that depend on the
    // solved call SCCs and the current optimistic parameter signatures.
    for (solver.direct_calls.items) |call| {
        const callee_sig = solver.sigs[@intFromEnum(call.callee)];
        const args = solver.store.getLocalSpan(call.args);
        const same_scc = solver.scc[@intFromEnum(call.callee)] == solver.scc[call.caller];
        const tail_call = same_scc and call.tail;
        for (0..GuardedList.borrowLen(args)) |position| {
            const arg = GuardedList.at(args, position);
            const argument = solver.domain.indexOf(arg) orelse continue;
            if (!tail_call and position < arc_sig.tracked_param_count) {
                const key = @intFromEnum(call.callee) * arc_sig.tracked_param_count + position;
                try solver.param_uses.append(solver.allocator, .{
                    .key = @intCast(key),
                    .argument = argument,
                });
            }
            if (!tail_call and callee_sig.paramMode(position) == .borrowed) continue;
            noteDemand(solver, arg);
        }
    }

    propagateAliasDemands(solver);
}

/// Settles the borrowed-parameter lattice from the facts collected above.
/// A work item is one exact `(callee, parameter position)` bit that just
/// became owned. Its adjacency list contains only the caller argument locals
/// whose demand depends on that bit.
fn solveParameterModes(solver: *Solver) SolveError!void {
    // Compact the collected edge facts into dense offsets. This preserves
    // exact dependency lookup without one allocation-capable list object for
    // every possible proc/parameter pair.
    const key_count = solver.sigs.len * arc_sig.tracked_param_count;
    const offsets = try solver.allocator.alloc(u32, key_count + 1);
    defer solver.allocator.free(offsets);
    @memset(offsets, 0);
    for (solver.param_uses.items) |use| offsets[use.key + 1] += 1;
    for (1..offsets.len) |index| offsets[index] += offsets[index - 1];

    const edges = try solver.allocator.alloc(u32, solver.param_uses.items.len);
    defer solver.allocator.free(edges);
    const fill = try solver.allocator.dupe(u32, offsets[0..key_count]);
    defer solver.allocator.free(fill);
    for (solver.param_uses.items) |use| {
        edges[fill[use.key]] = use.argument;
        fill[use.key] += 1;
    }

    var work = std.ArrayList(u32).empty;
    defer work.deinit(solver.allocator);

    // Static demands, alias-propagated demands, and multi-definition params
    // seed the worklist.
    for (0..solver.demand.len) |local_index| {
        try flipParamIfRequired(solver, @intCast(local_index), &work);
    }

    while (work.pop()) |key| {
        for (edges[offsets[key]..offsets[key + 1]]) |arg| {
            try demandAliasChain(solver, arg, &work);
        }
    }
}

fn flipParamIfRequired(solver: *Solver, local_index: u32, work: *std.ArrayList(u32)) SolveError!void {
    const proc_index = solver.param_proc[local_index];
    if (proc_index == no_local) return;
    const position = solver.param_position[local_index];
    if (position >= arc_sig.tracked_param_count) return;
    var sig = &solver.sigs[proc_index];
    if (sig.paramMode(position) == .owned) return;
    const required = solver.demand[local_index] or solver.defs[local_index] == .multi;
    if (!required) return;
    sig.borrowed_params &= ~arc_sig.paramBit(position).?;
    try work.append(solver.allocator, proc_index * arc_sig.tracked_param_count + position);
}

/// Adds one ownership demand and propagates it through the exact pure-alias
/// chain. Every newly demanded parameter bit is queued immediately.
fn demandAliasChain(solver: *Solver, start: u32, work: *std.ArrayList(u32)) SolveError!void {
    var cursor = start;
    while (true) {
        if (solver.demand[cursor]) return;
        solver.demand[cursor] = true;
        try flipParamIfRequired(solver, cursor, work);
        if (solver.defs[cursor] == .multi) return;
        const source = solver.alias_source[cursor];
        if (source == no_local) return;
        cursor = source;
    }
}

/// Changes only the definition facts whose kind depends on solved return
/// modes. A non-multi direct-call target has exactly one definition, so its
/// phase-A `.fresh` fact can be replaced directly; multi-bound targets stay
/// `.multi` under every return signature.
fn updateDirectCallResultDefs(solver: *Solver, changed: *std.ArrayList(u32)) SolveError!void {
    for (solver.direct_calls.items) |call| {
        const target = solver.domain.indexOf(call.target) orelse continue;
        if (solver.defs[target] == .multi) continue;
        const callee_sig = solver.sigs[@intFromEnum(call.callee)];
        const args = solver.store.getLocalSpan(call.args);
        const source = if (callee_sig.ret_mode == .borrowed)
            callRetBorrowSource(solver, callee_sig, args)
        else
            no_local;
        const updated: DefKind = if (source == no_local)
            .fresh
        else
            .{ .borrow_capable = source };
        if (!std.meta.eql(solver.defs[target], updated)) {
            solver.defs[target] = updated;
            try changed.append(solver.allocator, target);
        }
    }
}

/// Re-resolves exactly the call-result bindings whose definition changed
/// after return modes settled, plus their transitive borrow dependents. The
/// first binding solution remains authoritative everywhere outside this
/// reverse dependency closure.
fn updateBindingsAfterReturns(
    solver: *Solver,
    binding: *BindingResult,
    changed: []const u32,
) SolveError!void {
    if (changed.len == 0) return;
    const allocator = solver.allocator;
    const local_count = solver.domain.arc_to_local.len;

    const dependent_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(dependent_lens);
    @memset(dependent_lens, 0);
    for (solver.defs) |def| switch (def) {
        .borrow_capable => |source| dependent_lens[source] += 1,
        .none, .multi, .fresh => {},
    };
    const dependent_offsets = try allocator.alloc(u32, local_count + 1);
    defer allocator.free(dependent_offsets);
    dependent_offsets[0] = 0;
    for (dependent_lens, 0..) |len, index| dependent_offsets[index + 1] = dependent_offsets[index] + len;
    const dependents = try allocator.alloc(u32, dependent_offsets[local_count]);
    defer allocator.free(dependents);
    const fill = try allocator.dupe(u32, dependent_offsets[0..local_count]);
    defer allocator.free(fill);
    for (solver.defs, 0..) |def, target| switch (def) {
        .borrow_capable => |source| {
            dependents[fill[source]] = @intCast(target);
            fill[source] += 1;
        },
        .none, .multi, .fresh => {},
    };

    var affected = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer affected.deinit(allocator);
    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    for (changed) |local| {
        if (affected.isSet(local)) continue;
        affected.set(local);
        try work.append(allocator, local);
    }
    while (work.pop()) |source| {
        for (dependents[dependent_offsets[source]..dependent_offsets[source + 1]]) |target| {
            if (affected.isSet(target)) continue;
            affected.set(target);
            try work.append(allocator, target);
        }
    }

    var affected_iter = affected.iterator(.{});
    while (affected_iter.next()) |index| {
        binding.borrowed.unset(index);
        binding.leader[index] = @intCast(index);
    }

    var resolved = try std.bit_set.DynamicBitSetUnmanaged.initFull(allocator, local_count);
    defer resolved.deinit(allocator);
    resolved.setIntersection(affected);
    resolved.toggleAll();
    var on_chain = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer on_chain.deinit(allocator);
    var chain = std.ArrayList(u32).empty;
    defer chain.deinit(allocator);

    affected_iter = affected.iterator(.{});
    while (affected_iter.next()) |start| {
        if (resolved.isSet(start)) continue;
        if (paramIsBorrowed(solver, @intCast(start))) {
            binding.borrowed.set(start);
            resolved.set(start);
            continue;
        }
        chain.clearRetainingCapacity();
        var cursor: u32 = @intCast(start);
        const chain_leader: u32 = while (true) {
            if (paramIsBorrowed(solver, cursor)) break cursor;
            if (resolved.isSet(cursor)) break binding.leader[cursor];
            if (on_chain.isSet(cursor)) break cursor;
            if (!borrowQualifies(solver, cursor)) break cursor;
            on_chain.set(cursor);
            try chain.append(allocator, cursor);
            cursor = solver.defs[cursor].borrow_capable;
        };
        const leader_once_bound = paramIsBorrowed(solver, chain_leader) or
            leaderIsInitializedJoinParam(solver, chain_leader) or
            switch (solver.defs[chain_leader]) {
                .fresh, .borrow_capable => true,
                .none, .multi => false,
            };
        const leader_is_anchor = leader_once_bound and
            (!binding.borrowed.isSet(chain_leader) or paramIsBorrowed(solver, chain_leader));
        for (chain.items) |link| {
            on_chain.unset(link);
            resolved.set(link);
            if (leader_is_anchor and link != chain_leader) {
                binding.borrowed.set(link);
                binding.leader[link] = chain_leader;
            } else {
                binding.leader[link] = link;
            }
        }
        resolved.set(chain_leader);
    }
}

/// Records a pure same-value alias edge. A local bound more than once stops
/// propagating (its def degrades to `.multi` and it never borrows anyway).
fn noteAlias(solver: *Solver, target: LIR.LocalId, source: LIR.LocalId) void {
    const index = solver.domain.indexOf(target) orelse return;
    const source_index = solver.domain.indexOf(source) orelse
        solveInvariant("ARC pure-alias source was outside the ARC-local domain");
    solver.alias_source[index] = if (solver.alias_source[index] == no_local and
        solver.defs[index] != .multi)
        source_index
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
            if (solver.defs[cursor] == .multi) break;
            const source = solver.alias_source[cursor];
            if (source == no_local or solver.demand[source]) break;
            solver.demand[source] = true;
            cursor = source;
        }
    }
}

fn noteDef(solver: *Solver, local: LIR.LocalId, kind: DefKind) void {
    const index = solver.domain.indexOf(local) orelse return;
    solver.defs[index] = switch (solver.defs[index]) {
        .none => kind,
        .multi, .fresh, .borrow_capable => .multi,
    };
}

fn noteBorrowDef(solver: *Solver, target: LIR.LocalId, source: LIR.LocalId) void {
    const source_index = solver.domain.indexOf(source) orelse {
        if (solver.domain.indexOf(target) != null) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "ARC borrow source was outside the ARC-local domain: target={d} source={d} target_rc={} source_rc={}",
                    .{
                        @intFromEnum(target),
                        @intFromEnum(source),
                        solver.rc_local[@intFromEnum(target)],
                        solver.rc_local[@intFromEnum(source)],
                    },
                );
            }
            unreachable;
        }
        return;
    };
    noteDef(solver, target, .{ .borrow_capable = source_index });
}

fn noteDemand(solver: *Solver, local: LIR.LocalId) void {
    const index = solver.domain.indexOf(local) orelse return;
    solver.demand[index] = true;
}

fn liftVisibilityLink(solver: *Solver, a: LIR.LocalId, b: LIR.LocalId) SolveError!void {
    if (a == b) return;
    if (solver.domain.indexOf(a) == null or solver.domain.indexOf(b) == null) return;
    try solver.visibility_facts.append(solver.allocator, .{ .link = .{ .a = a, .b = b } });
}

fn liftVisibilitySeed(solver: *Solver, local: LIR.LocalId) SolveError!void {
    if (solver.domain.indexOf(local) == null) return;
    try solver.visibility_facts.append(solver.allocator, .{ .seed = local });
}

fn liftBoxyDescRead(solver: *Solver, desc: LIR.BoxyDescRef) SolveError!void {
    const local = desc.localOrNull() orelse return;
    try solver.binding_facts.append(solver.allocator, .{ .demand = local });
    try solver.unique_facts.append(solver.allocator, .{ .read = local });
}

fn liftBoxyTransfer(solver: *Solver, local: LIR.LocalId, mode: LIR.BoxyTransferMode) SolveError!void {
    switch (mode) {
        .borrow => try solver.unique_facts.append(solver.allocator, .{ .read = local }),
        .copy => try solver.unique_facts.append(solver.allocator, .{ .destroy = local }),
        .move => {
            try solver.binding_facts.append(solver.allocator, .{ .demand = local });
            try solver.unique_facts.append(solver.allocator, .{ .consume = local });
        },
    }
}

fn liftSharedStmtFacts(solver: *Solver, current: LIR.CFStmtId) SolveError!void {
    const store = solver.store;
    const allocator = solver.allocator;
    switch (store.getCFStmt(current)) {
        .assign_ref => |assign| {
            switch (assign.op) {
                .local => |source| {
                    if (assign.target != source) {
                        try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = source } });
                        try solver.binding_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = source } });
                        if (!aliasPreservesBoxyRcDescriptor(solver, assign.target, source)) {
                            try solver.binding_facts.append(allocator, .{ .demand = assign.target });
                        }
                    } else {
                        try solver.binding_facts.append(allocator, .{ .multi = assign.target });
                    }
                },
                .discriminant => try solver.binding_facts.append(allocator, .{ .fresh = assign.target }),
                .field => |op| try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = op.source } }),
                .tag_payload => |op| try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = op.source } }),
                .tag_payload_struct => |op| try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = op.source } }),
                .list_reinterpret => |op| {
                    try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = op.backing_ref } });
                    try solver.binding_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = op.backing_ref } });
                    if (!aliasPreservesBoxyRcDescriptor(solver, assign.target, op.backing_ref)) {
                        try solver.binding_facts.append(allocator, .{ .demand = assign.target });
                    }
                },
                .nominal => |op| {
                    try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = op.backing_ref } });
                    try solver.binding_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = op.backing_ref } });
                    if (!aliasPreservesBoxyRcDescriptor(solver, assign.target, op.backing_ref)) {
                        try solver.binding_facts.append(allocator, .{ .demand = assign.target });
                    }
                },
            }
            switch (assign.op) {
                .local => |source| {
                    try liftVisibilityLink(solver, assign.target, source);
                    try solver.unique_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = source } });
                },
                .list_reinterpret => |op| {
                    try liftVisibilityLink(solver, assign.target, op.backing_ref);
                    try solver.unique_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = op.backing_ref } });
                },
                .nominal => |op| {
                    try liftVisibilityLink(solver, assign.target, op.backing_ref);
                    try solver.unique_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = op.backing_ref } });
                },
                .field => |op| {
                    try liftVisibilityLink(solver, assign.target, op.source);
                    try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
                    try solver.unique_facts.append(allocator, .{ .read = op.source });
                },
                .tag_payload => |op| {
                    try liftVisibilityLink(solver, assign.target, op.source);
                    try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
                    try solver.unique_facts.append(allocator, .{ .read = op.source });
                },
                .tag_payload_struct => |op| {
                    try liftVisibilityLink(solver, assign.target, op.source);
                    try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
                    try solver.unique_facts.append(allocator, .{ .read = op.source });
                },
                .discriminant => |op| {
                    try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
                    try solver.unique_facts.append(allocator, .{ .read = op.source });
                },
            }
        },
        .assign_literal => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            if (assign.value == .proc_ref) {
                solver.address_taken.set(@intFromEnum(assign.value.proc_ref));
            } else if (assign.value == .str_literal or assign.value == .static_data or assign.value == .bytes_literal) {
                try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            }
            switch (assign.value) {
                .str_literal, .static_data, .bytes_literal => {},
                .i64_literal,
                .i128_literal,
                .f64_literal,
                .f32_literal,
                .dec_literal,
                .boxy_dynamic_num_literal,
                .boxy_dynamic_frac_literal,
                .null_ptr,
                .proc_ref,
                => try solver.unique_facts.append(allocator, .{ .birth = assign.target }),
            }
        },
        .init_uninitialized => {},
        .assign_call => |assign| {
            try solver.unique_calls.append(allocator, .{
                .callee = assign.proc,
                .args = assign.args,
                .target = assign.target,
            });
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            if (assign.result_desc) |desc| {
                try liftBoxyDescRead(solver, desc);
                if (desc.localOrNull()) |local| try liftVisibilitySeed(solver, local);
            }
            if (assign.out_desc) |out_desc| {
                try solver.binding_facts.append(allocator, .{ .fresh = out_desc });
                try solver.unique_facts.append(allocator, .{ .foreign = out_desc });
                try liftVisibilitySeed(solver, out_desc);
            }
            const args = store.getLocalSpan(assign.args);
            const callee = store.getProcSpec(assign.proc);
            if (callee.body == null) {
                for (0..GuardedList.borrowLen(args)) |arg_index| {
                    try liftVisibilitySeed(solver, GuardedList.at(args, arg_index));
                }
                try liftVisibilitySeed(solver, assign.target);
            } else {
                const params = store.getLocalSpan(callee.args);
                for (0..GuardedList.borrowLen(args)) |position| {
                    if (position >= params.len) break;
                    try liftVisibilityLink(solver, GuardedList.at(args, position), GuardedList.at(params, position));
                }
            }
        },
        .assign_call_erased => |assign| {
            if (!LIR.erasedCallReuseFieldsMatch(assign)) {
                solveInvariant("erased call reuse flag and ownership source disagreed");
            }
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            if (assign.result_desc) |desc| {
                try liftBoxyDescRead(solver, desc);
                if (desc.localOrNull()) |local| try liftVisibilitySeed(solver, local);
            }
            if (assign.out_desc) |out_desc| {
                try solver.binding_facts.append(allocator, .{ .fresh = out_desc });
                try solver.unique_facts.append(allocator, .{ .foreign = out_desc });
                try liftVisibilitySeed(solver, out_desc);
            }
            if (assign.reuse_source) |reuse_source| {
                try solver.binding_facts.append(allocator, .{ .demand = reuse_source });
            } else {
                try solver.binding_facts.append(allocator, .{ .demand = assign.closure });
            }
            const args = store.getLocalSpan(assign.args);
            for (0..GuardedList.borrowLen(args)) |index| {
                const arg = GuardedList.at(args, index);
                try solver.binding_facts.append(allocator, .{ .demand = arg });
                try liftVisibilitySeed(solver, arg);
            }
            const arg_descs = store.getLocalSpan(assign.arg_descs);
            for (0..GuardedList.borrowLen(arg_descs)) |index| {
                const arg_desc = GuardedList.at(arg_descs, index);
                try solver.binding_facts.append(allocator, .{ .demand = arg_desc });
                try solver.unique_facts.append(allocator, .{ .read = arg_desc });
                try liftVisibilitySeed(solver, arg_desc);
            }
            try liftVisibilitySeed(solver, assign.closure);
            try liftVisibilitySeed(solver, assign.target);
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            if (assign.reuse_source) |reuse_source| {
                try solver.unique_facts.append(allocator, .{ .consume = reuse_source });
            } else {
                try solver.unique_facts.append(allocator, .{ .destroy = assign.closure });
            }
            for (0..GuardedList.borrowLen(args)) |index| {
                try solver.unique_facts.append(allocator, .{ .destroy = GuardedList.at(args, index) });
            }
        },
        .assign_packed_erased_fn => |assign| {
            solver.address_taken.set(@intFromEnum(assign.proc));
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            if (assign.capture) |capture| try solver.binding_facts.append(allocator, .{ .demand = capture });
            if (assign.reuse) |reuse| try solver.binding_facts.append(allocator, .{ .demand = reuse });
            if (assign.capture) |capture| try liftVisibilityLink(solver, assign.target, capture);
            if (assign.result_desc) |desc| try liftBoxyDescRead(solver, desc);
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            if (assign.capture) |capture| try solver.unique_facts.append(allocator, .{ .destroy = capture });
            if (assign.reuse) |reuse| try solver.unique_facts.append(allocator, .{ .consume = reuse });
        },
        .assign_boxy_desc_ref => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try liftBoxyDescRead(solver, assign.desc);
            if (assign.tag_residual_for) |desc| try liftBoxyDescRead(solver, desc);
            const captures = store.getLocalSpan(assign.captures);
            for (0..GuardedList.borrowLen(captures)) |index| {
                const local = GuardedList.at(captures, index);
                try solver.binding_facts.append(allocator, .{ .demand = local });
                try solver.unique_facts.append(allocator, .{ .read = local });
            }
        },
        .assign_boxy_dict_ref => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            if (assign.dict.localOrNull()) |local| {
                try solver.binding_facts.append(allocator, .{ .demand = local });
                try solver.unique_facts.append(allocator, .{ .read = local });
            }
        },
        .assign_boxy_box => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyTransfer(solver, assign.payload, assign.payload_mode);
            if (assign.source_desc) |desc| try liftBoxyDescRead(solver, desc);
            if (assign.payload_desc) |desc| try liftBoxyDescRead(solver, desc);
            try liftVisibilityLink(solver, assign.target, assign.payload);
        },
        .assign_boxy_reuse_box => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.binding_facts.append(allocator, .{ .demand = assign.source });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try solver.unique_facts.append(allocator, .{ .consume = assign.source });
            try liftBoxyDescRead(solver, assign.desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_boxy_unbox => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try liftBoxyTransfer(solver, assign.source, assign.source_mode);
            try liftBoxyDescRead(solver, assign.source_desc);
            if (assign.target_desc) |desc| try liftBoxyDescRead(solver, desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_boxy_adapt => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try liftBoxyTransfer(solver, assign.source, assign.source_mode);
            if (assign.source_desc) |desc| try liftBoxyDescRead(solver, desc);
            if (assign.target_desc) |desc| try liftBoxyDescRead(solver, desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_boxy_inspect => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyTransfer(solver, assign.source, assign.source_mode);
            try liftBoxyDescRead(solver, assign.source_desc);
        },
        .assign_boxy_eq => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyTransfer(solver, assign.lhs, assign.source_mode);
            try liftBoxyTransfer(solver, assign.rhs, assign.source_mode);
            try liftBoxyDescRead(solver, assign.source_desc);
        },
        .assign_boxy_tag => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyDescRead(solver, assign.target_desc);
            if (assign.payload) |payload| {
                try liftBoxyTransfer(solver, payload, assign.payload_mode);
                try liftVisibilityLink(solver, assign.target, payload);
            }
            if (assign.payload_desc) |desc| try liftBoxyDescRead(solver, desc);
        },
        .assign_boxy_tag_payload => |assign| {
            switch (assign.source_mode) {
                .borrow => try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = assign.source } }),
                .copy, .move => try solver.binding_facts.append(allocator, .{ .fresh = assign.target }),
            }
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            if (assign.target_desc) |target_desc| {
                try solver.binding_facts.append(allocator, .{ .fresh = target_desc });
                try solver.unique_facts.append(allocator, .{ .foreign = target_desc });
            }
            try liftBoxyTransfer(solver, assign.source, assign.source_mode);
            try liftBoxyDescRead(solver, assign.source_desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_call_dict => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            if (assign.dict.localOrNull()) |local| {
                try solver.binding_facts.append(allocator, .{ .demand = local });
                try solver.unique_facts.append(allocator, .{ .read = local });
            }
            if (assign.result_desc) |desc| try liftBoxyDescRead(solver, desc);
            const args = store.getLocalSpan(assign.args);
            for (0..GuardedList.borrowLen(args)) |index| {
                const arg = GuardedList.at(args, index);
                try solver.binding_facts.append(allocator, .{ .demand = arg });
                try solver.unique_facts.append(allocator, .{ .destroy = arg });
                try liftVisibilitySeed(solver, arg);
            }
            const arg_descs = store.getLocalSpan(assign.arg_descs);
            for (0..GuardedList.borrowLen(arg_descs)) |index| {
                const arg_desc = GuardedList.at(arg_descs, index);
                try solver.binding_facts.append(allocator, .{ .demand = arg_desc });
                try solver.unique_facts.append(allocator, .{ .read = arg_desc });
                try liftVisibilitySeed(solver, arg_desc);
            }
            const hidden_args = store.getLocalSpan(assign.hidden_args);
            for (0..GuardedList.borrowLen(hidden_args)) |index| {
                const arg = GuardedList.at(hidden_args, index);
                try solver.binding_facts.append(allocator, .{ .demand = arg });
                try solver.unique_facts.append(allocator, .{ .destroy = arg });
                try liftVisibilitySeed(solver, arg);
            }
            try liftVisibilitySeed(solver, assign.target);
        },
        .assign_low_level => |assign| {
            const rc_effect = inferenceRcEffect(solver, assign.op, assign.rc_effect);
            const args = store.getLocalSpan(assign.args);
            const borrow_source = lowLevelBorrowSource(solver.domain, rc_effect, args);
            if (rc_effect.retain_result and borrow_source != no_local) {
                const source: LIR.LocalId = @enumFromInt(solver.domain.localAt(borrow_source));
                try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = assign.target, .source = source } });
            } else {
                try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            }
            for (0..GuardedList.borrowLen(args)) |index| {
                const arg = GuardedList.at(args, index);
                if (index >= 64) {
                    try solver.binding_facts.append(allocator, .{ .demand = arg });
                    continue;
                }
                const bit = @as(u64, 1) << @as(u6, @intCast(index));
                if ((rc_effect.consume_args & bit) != 0 or
                    (rc_effect.retain_args & bit) != 0)
                {
                    try solver.binding_facts.append(allocator, .{ .demand = arg });
                }
            }
            if (assign.op == .erased_capture_load) {
                try liftVisibilitySeed(solver, assign.target);
            } else {
                const share_mask = rc_effect.result_aliases_consumed_args |
                    rc_effect.result_borrows_args |
                    rc_effect.retain_args |
                    rc_effect.result_shares_args;
                if (share_mask != 0) {
                    for (0..GuardedList.borrowLen(args)) |position| {
                        if (position >= 64) break;
                        const bit = @as(u64, 1) << @as(u6, @intCast(position));
                        if ((share_mask & bit) == 0) continue;
                        try liftVisibilityLink(solver, assign.target, GuardedList.at(args, position));
                    }
                } else if (rc_effect.consume_args == 0) {
                    for (0..GuardedList.borrowLen(args)) |arg_index| {
                        try liftVisibilityLink(solver, assign.target, GuardedList.at(args, arg_index));
                    }
                }
            }
            try solver.unique_facts.append(allocator, if (rc_effect.result_unique)
                .{ .birth = assign.target }
            else
                .{ .foreign = assign.target });
            for (0..GuardedList.borrowLen(args)) |position| {
                const arg = GuardedList.at(args, position);
                if (position >= 64) {
                    try solver.unique_facts.append(allocator, .{ .destroy = arg });
                    continue;
                }
                const bit = @as(u64, 1) << @as(u6, @intCast(position));
                var read_only = true;
                if ((rc_effect.consume_args & bit) != 0) {
                    try solver.unique_facts.append(allocator, .{ .consume = arg });
                    read_only = false;
                }
                if ((rc_effect.retain_args & bit) != 0) {
                    try solver.unique_facts.append(allocator, .{ .destroy = arg });
                    read_only = false;
                }
                if (read_only) try solver.unique_facts.append(allocator, .{ .read = arg });
            }
        },
        .assign_list => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            const elems = store.getLocalSpan(assign.elems);
            for (0..GuardedList.borrowLen(elems)) |index| {
                const elem = GuardedList.at(elems, index);
                try solver.binding_facts.append(allocator, .{ .demand = elem });
                try liftVisibilityLink(solver, assign.target, elem);
                try solver.unique_facts.append(allocator, .{ .destroy = elem });
            }
        },
        .assign_struct => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            if (assign.contents_desc) |desc| {
                try liftBoxyDescRead(solver, desc);
                if (desc.localOrNull()) |local| try liftVisibilityLink(solver, assign.target, local);
            }
            const fields = store.getLocalSpan(assign.fields);
            for (0..GuardedList.borrowLen(fields)) |index| {
                const field = GuardedList.at(fields, index);
                try solver.binding_facts.append(allocator, .{ .demand = field });
                try liftVisibilityLink(solver, assign.target, field);
                try solver.unique_facts.append(allocator, .{ .destroy = field });
            }
        },
        .assign_tag => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            if (assign.target_desc) |desc| {
                try liftBoxyDescRead(solver, desc);
                if (desc.localOrNull()) |local| try liftVisibilityLink(solver, assign.target, local);
            }
            if (assign.payload) |payload| {
                try solver.binding_facts.append(allocator, .{ .demand = payload });
                try liftVisibilityLink(solver, assign.target, payload);
                try solver.unique_facts.append(allocator, .{ .destroy = payload });
            }
        },
        .store_struct => |assign| {
            try solver.binding_facts.append(allocator, .{ .demand = assign.dest });
            const fields = store.getLocalSpan(assign.fields);
            for (0..GuardedList.borrowLen(fields)) |index| {
                const field = GuardedList.at(fields, index);
                try solver.binding_facts.append(allocator, .{ .demand = field });
                try solver.unique_facts.append(allocator, .{ .destroy = field });
            }
        },
        .store_tag => |assign| {
            try solver.binding_facts.append(allocator, .{ .demand = assign.dest });
            if (assign.payload) |payload| {
                try solver.binding_facts.append(allocator, .{ .demand = payload });
                try solver.unique_facts.append(allocator, .{ .destroy = payload });
            }
        },
        .set_local => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            if (assign.target != assign.value) try solver.binding_facts.append(allocator, .{ .demand = assign.value });
            try liftVisibilityLink(solver, assign.target, assign.value);
            switch (assign.mode) {
                .initialize_join_param => if (assign.target != assign.value) {
                    try solver.unique_facts.append(allocator, .{ .join_incoming = .{
                        .target = assign.target,
                        .source = assign.value,
                    } });
                },
                .replace_existing, .initialize_join_result => {
                    try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
                    try solver.unique_facts.append(allocator, .{ .destroy = assign.target });
                    try solver.unique_facts.append(allocator, .{ .destroy = assign.value });
                },
            }
        },
        .debug => |debug_stmt| try solver.unique_facts.append(allocator, .{ .read = debug_stmt.message }),
        // The failure report takes ownership of the message.
        .expect_err => |expect_err_stmt| {
            try solver.binding_facts.append(allocator, .{ .demand = expect_err_stmt.message });
            try solver.unique_facts.append(allocator, .{ .consume = expect_err_stmt.message });
        },
        .expect => |expect_stmt| try solver.unique_facts.append(allocator, .{ .read = expect_stmt.condition }),
        .comptime_branch_taken => {},
        .incref => |rc| try solver.unique_facts.append(allocator, .{ .destroy = rc.value }),
        .decref => {},
        .decref_if_initialized => |rc| {
            try solver.binding_facts.append(allocator, .{ .demand = rc.value });
            try solver.unique_facts.append(allocator, .{ .read = rc.cond });
        },
        .free => {},
        .switch_stmt => |switch_stmt| {
            try solver.unique_facts.append(allocator, .{ .read = switch_stmt.cond });
        },
        .switch_initialized_payload => |switch_stmt| try solver.unique_facts.append(allocator, .{ .read = switch_stmt.cond }),
        .str_match => |str_match| {
            try solver.unique_facts.append(allocator, .{ .read = str_match.source });
            const steps = store.getStrMatchSteps(str_match.steps);
            for (0..GuardedList.borrowLen(steps)) |step_index| {
                const step = GuardedList.at(steps, step_index);
                switch (step.capture) {
                    .discard => {},
                    .view => |local| {
                        try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = local, .source = str_match.source } });
                        try liftVisibilityLink(solver, local, str_match.source);
                        try solver.unique_facts.append(allocator, .{ .foreign = local });
                    },
                }
            }
        },
        .boxy_tag_match => |tag_match| {
            try solver.unique_facts.append(allocator, .{ .read = tag_match.source });
            try liftBoxyDescRead(solver, tag_match.source_desc);
        },
        .str_match_set => |str_match_set| {
            try solver.unique_facts.append(allocator, .{ .read = str_match_set.source });
            const arms = store.getStrMatchArms(str_match_set.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                const arm = GuardedList.at(arms, arm_index);
                const steps = store.getStrMatchSteps(arm.steps);
                for (0..GuardedList.borrowLen(steps)) |step_index| {
                    const step = GuardedList.at(steps, step_index);
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| {
                            try solver.binding_facts.append(allocator, .{ .borrow = .{ .target = local, .source = str_match_set.source } });
                            try liftVisibilityLink(solver, local, str_match_set.source);
                            try solver.unique_facts.append(allocator, .{ .foreign = local });
                        },
                    }
                }
            }
        },
        .join => |join_stmt| {
            // Join parameters are written at every jump; they stay owned.
            const params = store.getLocalSpan(join_stmt.params);
            for (0..GuardedList.borrowLen(params)) |param_index| {
                const param = GuardedList.at(params, param_index);
                try solver.binding_facts.append(allocator, .{ .multi = param });
                if (solver.domain.indexOf(param)) |arc_index| solver.join_param.set(arc_index);
                try solver.unique_facts.append(allocator, .{ .join_target = param });
            }
            const maybe_uninitialized_params = store.getLocalSpan(join_stmt.maybe_uninitialized_params);
            const maybe_uninitialized_conditions = store.getLocalSpan(join_stmt.maybe_uninitialized_conditions);
            const maybe_uninitialized_condition_masks = store.getU64Span(join_stmt.maybe_uninitialized_condition_masks);
            if (maybe_uninitialized_params.len != maybe_uninitialized_conditions.len or maybe_uninitialized_params.len != maybe_uninitialized_condition_masks.len) {
                solveInvariant("maybe-uninitialized join metadata arity mismatch");
            }
            for (0..GuardedList.borrowLen(maybe_uninitialized_params)) |index| {
                const param = GuardedList.at(maybe_uninitialized_params, index);
                const condition = GuardedList.at(maybe_uninitialized_conditions, index);
                const mask = GuardedList.at(maybe_uninitialized_condition_masks, index);
                const param_index = solver.domain.indexOf(param) orelse continue;
                solver.maybe_uninitialized_join_param.set(param_index);
                solver.maybe_uninitialized_condition[param_index] = @intFromEnum(condition);
                solver.maybe_uninitialized_condition_mask[param_index] = mask;
            }
        },
        .ret => |ret_stmt| try solver.unique_facts.append(allocator, .{ .consume = ret_stmt.value }),
        .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| {
            try solver.binding_facts.append(allocator, .{ .demand = message });
            try solver.unique_facts.append(allocator, .{ .consume = message });
        },
        .jump => {},
        .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
    }
}

fn aliasPreservesBoxyRcDescriptor(
    solver: *const Solver,
    target: LIR.LocalId,
    source: LIR.LocalId,
) bool {
    if (solver.boxy_rc_descs.len == 0) return true;
    return std.meta.eql(
        solver.boxy_rc_descs[@intFromEnum(target)],
        solver.boxy_rc_descs[@intFromEnum(source)],
    );
}

/// Returns the single refcounted argument a borrowed-return call result may
/// borrow from, or `no_local` when the lender mask names zero or several
/// refcounted arguments (the caller then keeps the result owned and retains
/// it after the call).
fn callRetBorrowSource(solver: *const Solver, callee_sig: arc_sig.RcSig, args: anytype) u32 {
    var source: u32 = no_local;
    for (0..GuardedList.borrowLen(args)) |position| {
        const arg = GuardedList.at(args, position);
        const bit = arc_sig.paramBit(position) orelse break;
        if ((callee_sig.ret_lenders & bit) == 0) continue;
        const arg_index = solver.domain.indexOf(arg) orelse continue;
        if (source != no_local and source != arg_index) return no_local;
        source = arg_index;
    }
    return source;
}

/// Returns the single refcounted argument named by `result_borrows_args`, or
/// `no_local` when the mask names zero or several refcounted arguments.
fn lowLevelBorrowSource(
    domain: *const ArcLocalDomain,
    rc_effect: LIR.LowLevel.RcEffect,
    args: anytype,
) u32 {
    if (rc_effect.result_borrows_args == 0) return no_local;
    var source: u32 = no_local;
    for (0..GuardedList.borrowLen(args)) |index| {
        const arg = GuardedList.at(args, index);
        if (index >= 64) break;
        const bit = @as(u64, 1) << @as(u6, @intCast(index));
        if ((rc_effect.result_borrows_args & bit) == 0) continue;
        const arg_index = domain.indexOf(arg) orelse continue;
        if (source != no_local and source != arg_index) return no_local;
        source = arg_index;
    }
    return source;
}

fn computePins(solver: *Solver, roots: []const LIR.LirProcSpecId) SolveError!void {
    fillPinnedProcContracts(solver.store, roots, &solver.pinned);
    solver.pinned.setUnion(solver.address_taken);
}

/// Computes the pinned-proc set over a freshly allocated bit set; the
/// certifier mirrors the visibility analysis from this.
pub fn computePinnedProcs(
    allocator: Allocator,
    store: *const LirStore,
    roots: []const LIR.LirProcSpecId,
) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    var pinned = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.procSpecCount());
    errdefer pinned.deinit(allocator);
    fillPinnedProcContracts(store, roots, &pinned);
    var reachable = try reachableStatementSet(allocator, store, null);
    defer reachable.deinit(allocator);
    var iter = reachable.iterator(.{});
    while (iter.next()) |stmt_index| {
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        if (stmt == .assign_literal and stmt.assign_literal.value == .proc_ref) {
            pinned.set(@intFromEnum(stmt.assign_literal.value.proc_ref));
        } else if (stmt == .assign_packed_erased_fn) {
            pinned.set(@intFromEnum(stmt.assign_packed_erased_fn.proc));
        }
    }
    return pinned;
}

fn fillPinnedProcContracts(
    store: *const LirStore,
    roots: []const LIR.LirProcSpecId,
    pinned: *std.bit_set.DynamicBitSetUnmanaged,
) void {
    for (roots) |root| {
        pinned.set(@intFromEnum(root));
    }
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        if (proc.body == null or proc.hosted != null or proc.abi == .erased_callable) {
            pinned.set(proc_index);
        }
    }
}

fn computeVisibilityFromFacts(
    allocator: Allocator,
    solver: *const Solver,
) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    const domain = solver.domain;
    const arc_count = domain.arc_to_local.len;
    const parent = try allocator.alloc(u32, arc_count);
    defer allocator.free(parent);
    const rank = try allocator.alloc(u8, arc_count);
    defer allocator.free(rank);
    for (parent, 0..) |*entry, index| entry.* = @intCast(index);
    @memset(rank, 0);

    const Sets = struct {
        fn root(parents: []u32, start: u32) u32 {
            var current = start;
            while (parents[current] != current) current = parents[current];
            const result = current;
            current = start;
            while (parents[current] != current) {
                const next = parents[current];
                parents[current] = result;
                current = next;
            }
            return result;
        }

        fn merge(parents: []u32, ranks: []u8, a: u32, b: u32) void {
            var a_root = root(parents, a);
            var b_root = root(parents, b);
            if (a_root == b_root) return;
            if (ranks[a_root] < ranks[b_root]) {
                const tmp = a_root;
                a_root = b_root;
                b_root = tmp;
            }
            parents[b_root] = a_root;
            if (ranks[a_root] == ranks[b_root]) ranks[a_root] += 1;
        }
    };

    var seeds = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, arc_count);
    defer seeds.deinit(allocator);
    for (solver.visibility_facts.items) |fact| switch (fact) {
        .link => |link| Sets.merge(parent, rank, domain.indexOf(link.a).?, domain.indexOf(link.b).?),
        .seed => |local| seeds.set(domain.indexOf(local).?),
    };

    // Direct-call return flow depends only on the lifted call and return
    // facts, but returns may be encountered after their callers during the
    // structural walk, so connect them after the lift is complete.
    for (solver.unique_calls.items) |call| {
        if (solver.store.getProcSpec(call.callee).body == null) continue;
        const target = domain.indexOf(call.target) orelse continue;
        for (solver.proc_returns[@intFromEnum(call.callee)].items) |return_local| {
            const returned = domain.indexOfRaw(return_local) orelse continue;
            Sets.merge(parent, rank, target, returned);
        }
    }

    // Pinned parameters and returns are the remaining producer-authored
    // visibility seeds.
    for (0..solver.store.procSpecCount()) |proc_index| {
        if (!solver.pinned.isSet(proc_index)) continue;
        const proc = solver.store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const params = solver.store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |param_index| {
            if (domain.indexOf(GuardedList.at(params, param_index))) |index| seeds.set(index);
        }
        for (solver.proc_returns[proc_index].items) |return_local| {
            if (domain.indexOfRaw(return_local)) |index| seeds.set(index);
        }
    }

    var visible_roots = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, arc_count);
    defer visible_roots.deinit(allocator);
    var seed_iter = seeds.iterator(.{});
    while (seed_iter.next()) |index| visible_roots.set(Sets.root(parent, @intCast(index)));

    var visible = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, solver.store.localCount());
    errdefer visible.deinit(allocator);
    for (domain.arc_to_local, 0..) |local, arc_index| {
        if (visible_roots.isSet(Sets.root(parent, @intCast(arc_index)))) visible.set(local);
    }
    return visible;
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
    return computeVisibilityFromLift(allocator, store, rc_local, pinned, null, null);
}

fn computeVisibilityFromLift(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    pinned: *const std.bit_set.DynamicBitSetUnmanaged,
    proc_stmts: ?[]const std.ArrayList(LIR.CFStmtId),
    lifted_returns: ?[]const std.ArrayList(u32),
) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    const local_count = store.localCount();
    const proc_count = store.procSpecCount();

    var reachable = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.cfStmtCount());
    defer reachable.deinit(allocator);
    if (proc_stmts) |by_proc| {
        for (by_proc) |stmts| for (stmts.items) |stmt| reachable.set(@intFromEnum(stmt));
    }

    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);

    var visible = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer visible.deinit(allocator);

    // Visibility flow is equality over an undirected sharing relation. Keep
    // that relation directly as disjoint sets instead of first doubling every
    // edge, compacting the doubled edges into CSR, and then rediscovering the
    // same connected components with a propagation worklist.
    const parent = try allocator.alloc(u32, local_count);
    defer allocator.free(parent);
    const rank = try allocator.alloc(u8, local_count);
    defer allocator.free(rank);
    for (parent, 0..) |*entry, index| entry.* = @intCast(index);
    @memset(rank, 0);

    const Sets = struct {
        fn root(parents: []u32, start: u32) u32 {
            var current = start;
            while (parents[current] != current) current = parents[current];
            const result = current;
            current = start;
            while (parents[current] != current) {
                const next = parents[current];
                parents[current] = result;
                current = next;
            }
            return result;
        }

        fn merge(parents: []u32, ranks: []u8, a: u32, b: u32) void {
            var a_root = root(parents, a);
            var b_root = root(parents, b);
            if (a_root == b_root) return;
            if (ranks[a_root] < ranks[b_root]) {
                const tmp = a_root;
                a_root = b_root;
                b_root = tmp;
            }
            parents[b_root] = a_root;
            if (ranks[a_root] == ranks[b_root]) ranks[a_root] += 1;
        }
    };

    // Per-proc return values, for linking call results to callee returns.
    const ret_values = try allocator.alloc(std.ArrayList(u32), proc_count);
    defer {
        for (ret_values) |*list| list.deinit(allocator);
        allocator.free(ret_values);
    }
    @memset(ret_values, .empty);
    if (lifted_returns) |returns_by_proc| {
        for (returns_by_proc, 0..) |returns, proc_index| {
            try ret_values[proc_index].appendSlice(allocator, returns.items);
        }
    } else {
        for (0..store.procSpecCount()) |proc_index| {
            const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
            const body = proc.body orelse continue;
            visited.clearRetainingCapacity();
            stack.clearRetainingCapacity();
            try stack.append(allocator, body);
            while (stack.pop()) |current| {
                if (visited.contains(current)) continue;
                try visited.put(current, {});
                reachable.set(@intFromEnum(current));
                switch (store.getCFStmt(current)) {
                    .ret => |ret_stmt| try ret_values[proc_index].append(allocator, @intFromEnum(ret_stmt.value)),
                    .switch_stmt => |stmt| {
                        const branches = store.getCFSwitchBranches(stmt.branches);
                        for (0..GuardedList.borrowLen(branches)) |branch_index| {
                            const branch = GuardedList.at(branches, branch_index);
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
                        const arms = store.getStrMatchArms(stmt.arms);
                        for (0..GuardedList.borrowLen(arms)) |arm_index| {
                            const arm = GuardedList.at(arms, arm_index);
                            try stack.append(allocator, arm.on_match);
                        }
                        try stack.append(allocator, stmt.on_miss);
                    },
                    .boxy_tag_match => |stmt| {
                        try stack.append(allocator, stmt.on_match);
                        try stack.append(allocator, stmt.on_miss);
                    },
                    .join => |stmt| {
                        try stack.append(allocator, stmt.body);
                        try stack.append(allocator, stmt.remainder);
                    },
                    inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| {
                        try stack.append(allocator, stmt.next);
                    },
                    .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
                }
            }
        }
    }

    const seedLocal = struct {
        fn go(
            set: *std.bit_set.DynamicBitSetUnmanaged,
            rc: []const bool,
            index: u32,
        ) void {
            if (index >= rc.len or !rc[index]) return;
            set.set(index);
        }
    }.go;

    // Seeds: every pinned proc's parameters and returned values reach the
    // host or a caller the solver cannot see.
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        if (!pinned.isSet(proc_index)) continue;
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |param_index| {
            const param = GuardedList.at(params, param_index);
            seedLocal(&visible, rc_local, @intFromEnum(param));
        }
        for (ret_values[proc_index].items) |value| {
            seedLocal(&visible, rc_local, value);
        }
    }

    // Same-allocation relations. Unreachable statements only add relations
    // that widen the visible set, which is sound.
    const addEdge = struct {
        fn go(
            parents: []u32,
            ranks: []u8,
            rc: []const bool,
            a: u32,
            b: u32,
        ) void {
            if (a >= rc.len or !rc[a]) return;
            if (b >= rc.len or !rc[b]) return;
            if (a == b) return;
            Sets.merge(parents, ranks, a, b);
        }
    }.go;

    for (0..store.cfStmtCount()) |stmt_index| {
        if (!reachable.isSet(stmt_index)) continue;
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        switch (stmt) {
            .assign_ref => |assign| {
                const target = @intFromEnum(assign.target);
                switch (assign.op) {
                    .local => |source| addEdge(parent, rank, rc_local, target, @intFromEnum(source)),
                    .list_reinterpret => |op| addEdge(parent, rank, rc_local, target, @intFromEnum(op.backing_ref)),
                    .nominal => |op| addEdge(parent, rank, rc_local, target, @intFromEnum(op.backing_ref)),
                    .field => |op| addEdge(parent, rank, rc_local, target, @intFromEnum(op.source)),
                    .tag_payload => |op| addEdge(parent, rank, rc_local, target, @intFromEnum(op.source)),
                    .tag_payload_struct => |op| addEdge(parent, rank, rc_local, target, @intFromEnum(op.source)),
                    .discriminant => {},
                }
            },
            .assign_struct => |assign| {
                if (assign.contents_desc) |contents_desc| {
                    if (contents_desc.localOrNull()) |local| {
                        addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(local));
                    }
                }
                const fields = store.getLocalSpan(assign.fields);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    const field = GuardedList.at(fields, index);
                    addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(field));
                }
            },
            .assign_list => |assign| {
                const elems = store.getLocalSpan(assign.elems);
                for (0..GuardedList.borrowLen(elems)) |index| {
                    const elem = GuardedList.at(elems, index);
                    addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(elem));
                }
            },
            .assign_tag => |assign| {
                if (assign.target_desc) |target_desc| if (target_desc.localOrNull()) |local| {
                    addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(local));
                };
                if (assign.payload) |payload| {
                    addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(payload));
                }
            },
            .store_struct, .store_tag => {},
            .assign_packed_erased_fn => |assign| {
                if (assign.capture) |capture| {
                    addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(capture));
                }
            },
            .assign_boxy_box => |assign| {
                addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.payload));
            },
            .assign_boxy_reuse_box => |assign| {
                addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.source));
            },
            .assign_boxy_unbox => |assign| {
                addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.source));
            },
            .assign_boxy_adapt => |assign| {
                addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.source));
            },
            .assign_boxy_tag => |assign| {
                if (assign.payload) |payload| {
                    addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(payload));
                }
            },
            .assign_boxy_tag_payload => |assign| {
                addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.source));
            },
            .assign_call_dict => |assign| {
                if (assign.dict.localOrNull()) |local| seedLocal(&visible, rc_local, @intFromEnum(local));
                if (assign.result_desc) |desc| if (desc.localOrNull()) |local| {
                    seedLocal(&visible, rc_local, @intFromEnum(local));
                };
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |index| {
                    seedLocal(&visible, rc_local, @intFromEnum(GuardedList.at(args, index)));
                }
                const arg_descs = store.getLocalSpan(assign.arg_descs);
                for (0..GuardedList.borrowLen(arg_descs)) |index| {
                    seedLocal(&visible, rc_local, @intFromEnum(GuardedList.at(arg_descs, index)));
                }
                const hidden_args = store.getLocalSpan(assign.hidden_args);
                for (0..GuardedList.borrowLen(hidden_args)) |index| {
                    seedLocal(&visible, rc_local, @intFromEnum(GuardedList.at(hidden_args, index)));
                }
                seedLocal(&visible, rc_local, @intFromEnum(assign.target));
            },
            .str_match => |str_match| {
                const steps = store.getStrMatchSteps(str_match.steps);
                for (0..GuardedList.borrowLen(steps)) |step_index| {
                    const step = GuardedList.at(steps, step_index);
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| addEdge(parent, rank, rc_local, @intFromEnum(local), @intFromEnum(str_match.source)),
                    }
                }
            },
            .str_match_set => |str_match_set| {
                const arms = store.getStrMatchArms(str_match_set.arms);
                for (0..GuardedList.borrowLen(arms)) |arm_index| {
                    const arm = GuardedList.at(arms, arm_index);
                    const steps = store.getStrMatchSteps(arm.steps);
                    for (0..GuardedList.borrowLen(steps)) |step_index| {
                        const step = GuardedList.at(steps, step_index);
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| addEdge(parent, rank, rc_local, @intFromEnum(local), @intFromEnum(str_match_set.source)),
                        }
                    }
                }
            },
            .set_local => |assign| {
                addEdge(parent, rank, rc_local, @intFromEnum(assign.target), @intFromEnum(assign.value));
            },
            .assign_call => |assign| {
                const callee = store.getProcSpec(assign.proc);
                const args = store.getLocalSpan(assign.args);
                if (assign.result_desc) |result_desc| {
                    if (result_desc.localOrNull()) |local| {
                        seedLocal(&visible, rc_local, @intFromEnum(local));
                    }
                }
                if (assign.out_desc) |out_desc| seedLocal(&visible, rc_local, @intFromEnum(out_desc));
                if (callee.body == null) {
                    // No body to flow through: everything at the boundary is
                    // host-visible.
                    for (0..GuardedList.borrowLen(args)) |arg_index| {
                        const arg = GuardedList.at(args, arg_index);
                        seedLocal(&visible, rc_local, @intFromEnum(arg));
                    }
                    seedLocal(&visible, rc_local, @intFromEnum(assign.target));
                } else {
                    const params = store.getLocalSpan(callee.args);
                    for (0..GuardedList.borrowLen(args)) |position| {
                        const arg = GuardedList.at(args, position);
                        if (position >= params.len) break;
                        addEdge(parent, rank, rc_local, @intFromEnum(arg), @intFromEnum(GuardedList.at(params, position)));
                    }
                    for (ret_values[@intFromEnum(assign.proc)].items) |value| {
                        addEdge(parent, rank, rc_local, @intFromEnum(assign.target), value);
                    }
                }
            },
            .assign_call_erased => |assign| {
                // The callee is unknown; the boundary is treated like a
                // pinned signature.
                seedLocal(&visible, rc_local, @intFromEnum(assign.closure));
                if (assign.reuse_source) |reuse_source| seedLocal(&visible, rc_local, @intFromEnum(reuse_source));
                if (assign.result_desc) |desc| if (desc.localOrNull()) |local| {
                    seedLocal(&visible, rc_local, @intFromEnum(local));
                };
                if (assign.out_desc) |out_desc| seedLocal(&visible, rc_local, @intFromEnum(out_desc));
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |arg_index| {
                    const arg = GuardedList.at(args, arg_index);
                    seedLocal(&visible, rc_local, @intFromEnum(arg));
                }
                const arg_descs = store.getLocalSpan(assign.arg_descs);
                for (0..GuardedList.borrowLen(arg_descs)) |arg_index| {
                    seedLocal(&visible, rc_local, @intFromEnum(GuardedList.at(arg_descs, arg_index)));
                }
                seedLocal(&visible, rc_local, @intFromEnum(assign.target));
            },
            .assign_low_level => |assign| {
                const target = @intFromEnum(assign.target);
                if (assign.op == .erased_capture_load) {
                    // The loaded capture shares the callable's allocation
                    // through the executing frame, which value flow cannot
                    // see; erased-callable procs are pinned, so the capture
                    // is host-visible by construction.
                    seedLocal(&visible, rc_local, target);
                    continue;
                }
                const effect = assign.op.arcInferenceRcEffect(assign.rc_effect);
                const args = store.getLocalSpan(assign.args);
                const share_mask = effect.result_aliases_consumed_args |
                    effect.result_borrows_args |
                    effect.retain_args |
                    effect.result_shares_args;
                if (share_mask != 0) {
                    for (0..GuardedList.borrowLen(args)) |position| {
                        const arg = GuardedList.at(args, position);
                        if (position >= 64) break;
                        const bit = @as(u64, 1) << @as(u6, @intCast(position));
                        if ((share_mask & bit) == 0) continue;
                        addEdge(parent, rank, rc_local, target, @intFromEnum(arg));
                    }
                } else if (effect.consume_args == 0) {
                    // The masks say nothing about this op; a refcounted
                    // result conservatively shares every refcounted
                    // argument's allocation.
                    for (0..GuardedList.borrowLen(args)) |arg_index| {
                        const arg = GuardedList.at(args, arg_index);
                        addEdge(parent, rank, rc_local, target, @intFromEnum(arg));
                    }
                }
            },
            .init_uninitialized,
            .assign_literal,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .boxy_tag_match,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            => {},
        }
    }

    // A component is visible exactly when it contains a visibility seed.
    var visible_roots = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer visible_roots.deinit(allocator);
    var seed_iter = visible.iterator(.{});
    while (seed_iter.next()) |index| visible_roots.set(Sets.root(parent, @intCast(index)));
    visible.unsetAll();
    for (rc_local, 0..) |is_rc, index| {
        if (is_rc and visible_roots.isSet(Sets.root(parent, @intCast(index)))) visible.set(index);
    }

    return visible;
}

/// Result of the born-unique analysis, one bit triple per local.
pub const Uniqueness = struct {
    /// Bit set => every definition of the local binds a value whose
    /// outermost allocation originated at a unique birth: a fresh aggregate
    /// or non-static literal assignment, a low-level op whose `RcEffect` marks its
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

/// Definition-origin dependencies used only while solving unique-return
/// signature bits. Holder-destroy facts live in `Uniqueness.destroyed` and
/// are signature-independent; these tables describe the monotone origins
/// whose truth can grow when a callee becomes unique-returning.
const UniqueOriginFacts = struct {
    allocator: Allocator,
    domain: *const ArcLocalDomain,
    static_birth: std.bit_set.DynamicBitSetUnmanaged,
    static_foreign: std.bit_set.DynamicBitSetUnmanaged,
    /// Set after the first definition of each refcounted local. A second
    /// definition makes the origin foreign: flow-insensitive uniqueness may
    /// not choose one of several runtime births.
    has_def: std.bit_set.DynamicBitSetUnmanaged,
    call_count: []u32,
    remaining_nonunique_calls: []u32,
    alias_source: []u32,
    alias_targets: std.ArrayList(u32),
    join_targets: std.bit_set.DynamicBitSetUnmanaged,
    join_incoming: std.ArrayList(UniqueJoinIncoming),
    call_targets_by_callee: []std.ArrayList(u32),

    fn init(allocator: Allocator, domain: *const ArcLocalDomain, proc_count: usize) SolveError!UniqueOriginFacts {
        const local_count = domain.arc_to_local.len;
        var static_birth = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer static_birth.deinit(allocator);
        var static_foreign = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer static_foreign.deinit(allocator);
        var has_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer has_def.deinit(allocator);
        const call_count = try allocator.alloc(u32, local_count);
        errdefer allocator.free(call_count);
        const remaining_nonunique_calls = try allocator.alloc(u32, local_count);
        errdefer allocator.free(remaining_nonunique_calls);
        const alias_source = try allocator.alloc(u32, local_count);
        errdefer allocator.free(alias_source);
        var join_targets = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer join_targets.deinit(allocator);
        const call_targets_by_callee = try allocator.alloc(std.ArrayList(u32), proc_count);
        errdefer allocator.free(call_targets_by_callee);

        const result = UniqueOriginFacts{
            .allocator = allocator,
            .domain = domain,
            .static_birth = static_birth,
            .static_foreign = static_foreign,
            .has_def = has_def,
            .call_count = call_count,
            .remaining_nonunique_calls = remaining_nonunique_calls,
            .alias_source = alias_source,
            .alias_targets = .empty,
            .join_targets = join_targets,
            .join_incoming = .empty,
            .call_targets_by_callee = call_targets_by_callee,
        };
        @memset(result.call_count, 0);
        @memset(result.remaining_nonunique_calls, 0);
        @memset(result.alias_source, no_local);
        @memset(result.call_targets_by_callee, .empty);
        return result;
    }

    fn deinit(self: *UniqueOriginFacts) void {
        self.static_birth.deinit(self.allocator);
        self.static_foreign.deinit(self.allocator);
        self.has_def.deinit(self.allocator);
        self.allocator.free(self.call_count);
        self.allocator.free(self.remaining_nonunique_calls);
        self.allocator.free(self.alias_source);
        self.alias_targets.deinit(self.allocator);
        self.join_targets.deinit(self.allocator);
        self.join_incoming.deinit(self.allocator);
        for (self.call_targets_by_callee) |*targets| targets.deinit(self.allocator);
        self.allocator.free(self.call_targets_by_callee);
    }

    fn noteDefinition(self: *UniqueOriginFacts, local: LIR.LocalId) ?u32 {
        const index = self.domain.indexOf(local) orelse return null;
        if (self.has_def.isSet(index)) {
            self.static_foreign.set(index);
        } else {
            self.has_def.set(index);
        }
        return @intCast(index);
    }

    fn noteBirth(self: *UniqueOriginFacts, local: LIR.LocalId) void {
        const index = self.noteDefinition(local) orelse return;
        self.static_birth.set(index);
    }

    fn noteForeign(self: *UniqueOriginFacts, local: LIR.LocalId) void {
        const index = self.noteDefinition(local) orelse return;
        self.static_foreign.set(index);
    }

    fn noteAlias(self: *UniqueOriginFacts, target: LIR.LocalId, source: LIR.LocalId) SolveError!void {
        const target_index = self.noteDefinition(target) orelse return;
        const source_index = self.domain.indexOf(source) orelse {
            self.static_foreign.set(target_index);
            return;
        };
        if (source_index == target_index) {
            self.static_foreign.set(target_index);
            return;
        }
        if (self.alias_source[target_index] == no_local) {
            self.alias_source[target_index] = @intCast(source_index);
            try self.alias_targets.append(self.allocator, @intCast(target_index));
        } else if (self.alias_source[target_index] != source_index) {
            self.static_foreign.set(target_index);
        }
    }

    fn noteJoinTarget(self: *UniqueOriginFacts, local: LIR.LocalId) void {
        const index = self.noteDefinition(local) orelse return;
        self.join_targets.set(index);
    }

    fn noteJoinIncoming(self: *UniqueOriginFacts, target: LIR.LocalId, source: LIR.LocalId) SolveError!void {
        const target_index = self.domain.indexOf(target) orelse return;
        const source_index = self.domain.indexOf(source) orelse {
            self.static_foreign.set(target_index);
            return;
        };
        if (target_index == source_index) return;
        try self.join_incoming.append(self.allocator, .{
            .target = target_index,
            .source = source_index,
        });
    }

    fn noteCall(self: *UniqueOriginFacts, callee: LIR.LirProcSpecId, target: LIR.LocalId) SolveError!void {
        const target_index = self.noteDefinition(target) orelse return;
        self.call_count[target_index] += 1;
        self.remaining_nonunique_calls[target_index] += 1;
        try self.call_targets_by_callee[@intFromEnum(callee)].append(self.allocator, @intCast(target_index));
    }
};

fn collectUniqueOriginStmt(facts: *UniqueOriginFacts, store: *const LirStore, stmt: LIR.CFStmt, consume_dead_boxes: bool) SolveError!void {
    switch (stmt) {
        .assign_ref => |assign| switch (assign.op) {
            .local => |source| try facts.noteAlias(assign.target, source),
            .list_reinterpret => |op| try facts.noteAlias(assign.target, op.backing_ref),
            .nominal => |op| try facts.noteAlias(assign.target, op.backing_ref),
            .discriminant, .field, .tag_payload, .tag_payload_struct => facts.noteForeign(assign.target),
        },
        .assign_literal => |assign| switch (assign.value) {
            .str_literal, .static_data, .bytes_literal => facts.noteForeign(assign.target),
            .i64_literal,
            .i128_literal,
            .f64_literal,
            .f32_literal,
            .dec_literal,
            .boxy_dynamic_num_literal,
            .boxy_dynamic_frac_literal,
            .null_ptr,
            .proc_ref,
            => facts.noteBirth(assign.target),
        },
        .assign_call => |assign| {
            try facts.noteCall(assign.proc, assign.target);
            if (assign.out_desc) |out_desc| facts.noteForeign(out_desc);
        },
        .assign_call_erased => |assign| {
            facts.noteForeign(assign.target);
            if (assign.out_desc) |out_desc| facts.noteForeign(out_desc);
        },
        .assign_packed_erased_fn => |assign| facts.noteBirth(assign.target),
        .assign_boxy_desc_ref => |assign| facts.noteForeign(assign.target),
        .assign_boxy_dict_ref => |assign| facts.noteForeign(assign.target),
        .assign_boxy_box => |assign| facts.noteBirth(assign.target),
        .assign_boxy_reuse_box => |assign| facts.noteForeign(assign.target),
        .assign_boxy_unbox => |assign| facts.noteForeign(assign.target),
        .assign_boxy_adapt => |assign| facts.noteForeign(assign.target),
        .assign_boxy_inspect => |assign| facts.noteBirth(assign.target),
        .assign_boxy_eq => |assign| facts.noteBirth(assign.target),
        .assign_boxy_tag => |assign| facts.noteBirth(assign.target),
        .assign_boxy_tag_payload => |assign| {
            facts.noteForeign(assign.target);
            if (assign.target_desc) |target_desc| facts.noteForeign(target_desc);
        },
        .assign_call_dict => |assign| facts.noteForeign(assign.target),
        .assign_low_level => |assign| if ((if (!consume_dead_boxes and assign.op == .box_unbox) assign.op.arcBorrowedResultVariant().?.rcEffect() else assign.op.arcInferenceRcEffect(assign.rc_effect)).result_unique)
            facts.noteBirth(assign.target)
        else
            facts.noteForeign(assign.target),
        .assign_list => |assign| facts.noteBirth(assign.target),
        .assign_struct => |assign| facts.noteBirth(assign.target),
        .assign_tag => |assign| facts.noteBirth(assign.target),
        .set_local => |assign| switch (assign.mode) {
            .initialize_join_param => try facts.noteJoinIncoming(assign.target, assign.value),
            .replace_existing, .initialize_join_result => facts.noteForeign(assign.target),
        },
        .str_match => |str_match| {
            const steps = store.getStrMatchSteps(str_match.steps);
            for (0..GuardedList.borrowLen(steps)) |step_index| switch (GuardedList.at(steps, step_index).capture) {
                .discard => {},
                .view => |local| facts.noteForeign(local),
            };
        },
        .str_match_set => |str_match_set| {
            const arms = store.getStrMatchArms(str_match_set.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                const steps = store.getStrMatchSteps(GuardedList.at(arms, arm_index).steps);
                for (0..GuardedList.borrowLen(steps)) |step_index| switch (GuardedList.at(steps, step_index).capture) {
                    .discard => {},
                    .view => |local| facts.noteForeign(local),
                };
            }
        },
        .join => |join_stmt| {
            const params = store.getLocalSpan(join_stmt.params);
            for (0..GuardedList.borrowLen(params)) |param_index| facts.noteJoinTarget(GuardedList.at(params, param_index));
        },
        .init_uninitialized,
        .store_struct,
        .store_tag,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .switch_stmt,
        .switch_initialized_payload,
        .boxy_tag_match,
        .loop_continue,
        .loop_break,
        .jump,
        .ret,
        .crash,
        => {},
    }
}

fn settleUniqueOriginDependencies(
    allocator: Allocator,
    born: *std.bit_set.DynamicBitSetUnmanaged,
    foreign: *const std.bit_set.DynamicBitSetUnmanaged,
    multi_def: *const std.bit_set.DynamicBitSetUnmanaged,
    destroyed: *std.bit_set.DynamicBitSetUnmanaged,
    read: *const std.bit_set.DynamicBitSetUnmanaged,
    alias_source: []const u32,
    alias_targets: []const u32,
    join_targets: *const std.bit_set.DynamicBitSetUnmanaged,
    join_incoming: []const UniqueJoinIncoming,
) SolveError!void {
    const local_count = alias_source.len;

    const alias_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_lens);
    @memset(alias_lens, 0);
    for (alias_targets) |target| alias_lens[alias_source[target]] += 1;
    const alias_offsets = try allocator.alloc(u32, local_count + 1);
    defer allocator.free(alias_offsets);
    alias_offsets[0] = 0;
    for (alias_lens, 0..) |len, index| alias_offsets[index + 1] = alias_offsets[index] + len;
    const alias_edges = try allocator.alloc(u32, alias_targets.len);
    defer allocator.free(alias_edges);
    const alias_fill = try allocator.dupe(u32, alias_offsets[0..local_count]);
    defer allocator.free(alias_fill);
    for (alias_targets) |target| {
        const source = alias_source[target];
        alias_edges[alias_fill[source]] = target;
        alias_fill[source] += 1;
    }

    const join_lens = try allocator.alloc(u32, local_count);
    defer allocator.free(join_lens);
    @memset(join_lens, 0);
    const join_remaining = try allocator.alloc(u32, local_count);
    defer allocator.free(join_remaining);
    @memset(join_remaining, 0);
    for (join_incoming) |incoming| {
        join_lens[incoming.source] += 1;
        join_remaining[incoming.target] += 1;
    }
    const join_offsets = try allocator.alloc(u32, local_count + 1);
    defer allocator.free(join_offsets);
    join_offsets[0] = 0;
    for (join_lens, 0..) |len, index| join_offsets[index + 1] = join_offsets[index] + len;
    const join_edges = try allocator.alloc(u32, join_incoming.len);
    defer allocator.free(join_edges);
    const join_fill = try allocator.dupe(u32, join_offsets[0..local_count]);
    defer allocator.free(join_fill);
    for (join_incoming) |incoming| {
        join_edges[join_fill[incoming.source]] = incoming.target;
        join_fill[incoming.source] += 1;
    }

    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    var queued = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer queued.deinit(allocator);
    var born_seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer born_seen.deinit(allocator);
    const enqueue = struct {
        fn go(
            alloc: Allocator,
            items: *std.ArrayList(u32),
            in_items: *std.bit_set.DynamicBitSetUnmanaged,
            source: u32,
        ) SolveError!void {
            if (in_items.isSet(source)) return;
            in_items.set(source);
            try items.append(alloc, source);
        }
    }.go;

    for (0..local_count) |source| {
        if (born.isSet(source) or destroyed.isSet(source) or read.isSet(source)) {
            try enqueue(allocator, &work, &queued, @intCast(source));
        }
    }
    while (work.pop()) |source| {
        queued.unset(source);
        const newly_born = born.isSet(source) and !born_seen.isSet(source);
        if (newly_born) born_seen.set(source);

        for (alias_edges[alias_offsets[source]..alias_offsets[source + 1]]) |target| {
            var changed = false;
            if (!foreign.isSet(target) and !multi_def.isSet(target) and
                !born.isSet(target) and born.isSet(source))
            {
                born.set(target);
                changed = true;
            }
            if (!destroyed.isSet(target) and (destroyed.isSet(source) or read.isSet(source))) {
                destroyed.set(target);
                changed = true;
            }
            if (changed) try enqueue(allocator, &work, &queued, target);
        }

        for (join_edges[join_offsets[source]..join_offsets[source + 1]]) |target| {
            var changed = false;
            if (newly_born) {
                if (join_remaining[target] == 0) solveInvariant("ARC join uniqueness incoming edge was satisfied twice");
                join_remaining[target] -= 1;
                if (join_targets.isSet(target) and join_remaining[target] == 0 and
                    !foreign.isSet(target) and !multi_def.isSet(target) and !born.isSet(target))
                {
                    born.set(target);
                    changed = true;
                }
            }
            if (!destroyed.isSet(target) and (destroyed.isSet(source) or read.isSet(source))) {
                destroyed.set(target);
                changed = true;
            }
            if (changed) try enqueue(allocator, &work, &queued, target);
        }
    }
}

fn computeUniquenessFromFacts(
    allocator: Allocator,
    solver: *const Solver,
    origins: *UniqueOriginFacts,
) SolveError!Uniqueness {
    const domain = solver.domain;
    const local_count = domain.arc_to_local.len;
    var born = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer born.deinit(allocator);
    var foreign = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer foreign.deinit(allocator);
    var destroyed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer destroyed.deinit(allocator);
    var consumed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer consumed.deinit(allocator);
    var read = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer read.deinit(allocator);
    var has_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer has_def.deinit(allocator);
    var multi_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer multi_def.deinit(allocator);
    const alias_source = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_source);
    @memset(alias_source, no_local);
    var alias_targets = std.ArrayList(u32).empty;
    defer alias_targets.deinit(allocator);
    var join_incoming = std.ArrayList(UniqueJoinIncoming).empty;
    defer join_incoming.deinit(allocator);
    var join_targets = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer join_targets.deinit(allocator);

    const Marks = struct {
        fn trackDef(
            seen: *std.bit_set.DynamicBitSetUnmanaged,
            multi: *std.bit_set.DynamicBitSetUnmanaged,
            index: u32,
        ) void {
            if (seen.isSet(index)) multi.set(index) else seen.set(index);
        }

        fn consume(
            once: *std.bit_set.DynamicBitSetUnmanaged,
            dead: *std.bit_set.DynamicBitSetUnmanaged,
            index: u32,
        ) void {
            if (once.isSet(index)) dead.set(index) else once.set(index);
        }
    };

    for (solver.unique_facts.items) |fact| switch (fact) {
        .birth => |local| if (domain.indexOf(local)) |index| {
            Marks.trackDef(&has_def, &multi_def, index);
            born.set(index);
            origins.noteBirth(local);
        },
        .foreign => |local| if (domain.indexOf(local)) |index| {
            Marks.trackDef(&has_def, &multi_def, index);
            foreign.set(index);
            origins.noteForeign(local);
        },
        .alias => |alias| if (domain.indexOf(alias.target)) |target| {
            Marks.trackDef(&has_def, &multi_def, target);
            const source = domain.indexOf(alias.source) orelse {
                foreign.set(target);
                origins.noteForeign(alias.target);
                continue;
            };
            Marks.consume(&consumed, &destroyed, source);
            if (source == target) {
                foreign.set(target);
            } else if (alias_source[target] == no_local) {
                alias_source[target] = source;
                try alias_targets.append(allocator, target);
            } else if (alias_source[target] != source) {
                foreign.set(target);
            }
            try origins.noteAlias(alias.target, alias.source);
        },
        .join_target => |local| if (domain.indexOf(local)) |target| {
            Marks.trackDef(&has_def, &multi_def, target);
            join_targets.set(target);
            origins.noteJoinTarget(local);
        },
        .join_incoming => |incoming| if (domain.indexOf(incoming.target)) |target| {
            const source = domain.indexOf(incoming.source) orelse {
                foreign.set(target);
                origins.static_foreign.set(target);
                continue;
            };
            if (target == source) continue;
            try join_incoming.append(allocator, .{ .target = target, .source = source });
            Marks.consume(&consumed, &destroyed, source);
            try origins.noteJoinIncoming(incoming.target, incoming.source);
        },
        .consume => |local| if (domain.indexOf(local)) |index| Marks.consume(&consumed, &destroyed, index),
        .destroy => |local| if (domain.indexOf(local)) |index| destroyed.set(index),
        .read => |local| if (domain.indexOf(local)) |index| read.set(index),
    };

    // Direct-call facts are static, but their return origins and argument
    // occurrences consume the final signature table.
    for (solver.unique_calls.items) |call| {
        const sig = solver.sigs[@intFromEnum(call.callee)];
        if (domain.indexOf(call.target)) |target| {
            Marks.trackDef(&has_def, &multi_def, target);
            if (sig.ret_unique) born.set(target) else foreign.set(target);
            try origins.noteCall(call.callee, call.target);
        }
        const args = solver.store.getLocalSpan(call.args);
        for (0..GuardedList.borrowLen(args)) |position| {
            const arg = domain.indexOf(GuardedList.at(args, position)) orelse continue;
            if (sig.paramMode(position) == .owned) {
                Marks.consume(&consumed, &destroyed, arg);
            } else {
                destroyed.set(arg);
            }
        }
    }

    var foreign_iter = foreign.iterator(.{});
    while (foreign_iter.next()) |index| born.unset(index);
    var multi_iter = multi_def.iterator(.{});
    while (multi_iter.next()) |index| born.unset(index);
    for (alias_targets.items) |target| {
        born.unset(target);
        if (multi_def.isSet(target)) destroyed.set(target);
    }

    try settleUniqueOriginDependencies(
        allocator,
        &born,
        &foreign,
        &multi_def,
        &destroyed,
        &read,
        alias_source,
        alias_targets.items,
        &join_targets,
        join_incoming.items,
    );

    var unique = try born.clone(allocator);
    errdefer unique.deinit(allocator);
    var destroyed_iter = destroyed.iterator(.{});
    while (destroyed_iter.next()) |index| unique.unset(index);
    return .{ .born_unique = born, .unique = unique, .destroyed = destroyed };
}

/// Marks every local whose value's outermost allocation provably has count 1
/// at the local's definition with nothing later adding a holder: born unique
/// by a fresh allocation or a direct call to a unique-returning callee,
/// destroyed by any occurrence in the analyzed procedure set that can create another handle to the
/// allocation—an incref, an aggregate or capture operand, a `set_local`
/// value or target, or a second consuming use. Consuming uses (a consumed
/// low-level argument, an owned-position direct-call argument, a return)
/// take the value's single unit with them, so the first one preserves
/// uniqueness and any further one destroys it; borrowed-position call
/// arguments and erased-call arguments conservatively destroy. A pure
/// same-value alias (`.local`, `.list_reinterpret`, `.nominal`—not
/// payload reads, which name interior allocations of a possibly-shared
/// outer value) inherits uniqueness: its definition is the chain's
/// consuming use of the source, so the source's single unit moves through
/// to the target, and any other occurrence of the source—consuming,
/// holder-adding, or a mere read, before or after, since the analysis is
/// flow-insensitive—destroys the target's uniqueness (a read elsewhere
/// forces emission to give the alias its own unit, holding the count above
/// 1). A multi-bound alias target never inherits. Variant parameter seeds
/// are not applied here: emission and the certifier overlay
/// `RcSig.unique_params` per proc. Only reachable statements contribute;
/// the solver consumes its shared per-proc lift, while final-LIR
/// certification analyzes one emitted proc at a time because base and
/// specialized bodies deliberately share every source LocalId.
pub fn computeUniqueness(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
) SolveError!Uniqueness {
    return computeUniquenessDetailed(allocator, store, rc_local, sigs, null, null, null, null, null, true);
}

const ProcUniquenessDomain = struct {
    local_to_dense: []const u32,
    count: usize,

    fn indexOf(self: ProcUniquenessDomain, local: LIR.LocalId) ?u32 {
        const raw = @intFromEnum(local);
        if (raw >= self.local_to_dense.len) return null;
        const dense = self.local_to_dense[raw];
        return if (dense == no_local) null else dense;
    }
};

/// Re-derives uniqueness from exactly one emitted procedure's explicit
/// statement and reference-counted-local inventories. ARC variants deliberately
/// share source LocalIds, so sibling definitions cannot affect this proof; the
/// dense proc domain also avoids store-wide allocation or clearing per proc.
pub fn computeProcUniqueness(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    proc: LIR.LirProcSpecId,
    stmts: []const LIR.CFStmtId,
    local_to_dense: []const u32,
    dense_local_count: usize,
) SolveError!Uniqueness {
    return computeUniquenessDetailed(
        allocator,
        store,
        rc_local,
        sigs,
        null,
        null,
        proc,
        stmts,
        .{ .local_to_dense = local_to_dense, .count = dense_local_count },
        true,
    );
}

fn computeUniquenessDetailed(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    origin_facts: ?*UniqueOriginFacts,
    proc_stmts: ?[]const std.ArrayList(LIR.CFStmtId),
    only_proc: ?LIR.LirProcSpecId,
    exact_stmts: ?[]const LIR.CFStmtId,
    proc_domain: ?ProcUniquenessDomain,
    consume_dead_boxes: bool,
) SolveError!Uniqueness {
    const local_count = if (proc_domain) |domain| domain.count else store.localCount();

    var reachable: std.bit_set.DynamicBitSetUnmanaged = .{};
    defer reachable.deinit(allocator);
    if (exact_stmts == null) {
        reachable = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.cfStmtCount());
        if (proc_stmts) |by_proc| {
            if (only_proc) |proc_id| {
                for (by_proc[@intFromEnum(proc_id)].items) |stmt| reachable.set(@intFromEnum(stmt));
            } else {
                for (by_proc) |stmts| for (stmts.items) |stmt| reachable.set(@intFromEnum(stmt));
            }
        } else {
            reachable.deinit(allocator);
            reachable = try reachableStatementSet(allocator, store, only_proc);
        }
    }

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
    // a local's own uniqueness—emission's path-sensitive facts cover the
    // checked argument itself—but they block alias inheritance, because a
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
    var join_incoming = std.ArrayList(UniqueJoinIncoming).empty;
    defer join_incoming.deinit(allocator);
    var join_targets = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer join_targets.deinit(allocator);

    const Marks = struct {
        rc: []const bool,
        domain: ?ProcUniquenessDomain,

        fn indexOf(self: @This(), local: LIR.LocalId) ?u32 {
            if (self.domain) |domain| return domain.indexOf(local);
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return null;
            return @intCast(index);
        }

        fn noteBirth(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = self.indexOf(local) orelse return;
            set.set(index);
        }

        fn destroy(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = self.indexOf(local) orelse return;
            set.set(index);
        }

        fn noteUse(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = self.indexOf(local) orelse return;
            set.set(index);
        }

        fn trackDef(
            self: @This(),
            seen: *std.bit_set.DynamicBitSetUnmanaged,
            multi: *std.bit_set.DynamicBitSetUnmanaged,
            local: LIR.LocalId,
        ) void {
            const index = self.indexOf(local) orelse return;
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
            const index = self.indexOf(local) orelse return;
            if (once.isSet(index)) {
                dead.set(index);
            } else {
                once.set(index);
            }
        }

        fn transfer(
            self: @This(),
            once: *std.bit_set.DynamicBitSetUnmanaged,
            dead: *std.bit_set.DynamicBitSetUnmanaged,
            reads: *std.bit_set.DynamicBitSetUnmanaged,
            local: LIR.LocalId,
            mode: LIR.BoxyTransferMode,
        ) void {
            switch (mode) {
                .move => self.consume(once, dead, local),
                .copy => self.destroy(dead, local),
                .borrow => self.noteUse(reads, local),
            }
        }
    };
    const marks = Marks{ .rc = rc_local, .domain = proc_domain };

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
            const target_index = m.indexOf(target) orelse return;
            const source_index = m.indexOf(source) orelse {
                foreign.set(target_index);
                return;
            };
            if (source_index == target_index) {
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

    for (0..store.procSpecCount()) |proc_index| {
        if (only_proc) |proc_id| {
            if (proc_index != @intFromEnum(proc_id)) continue;
        }
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |param_index| {
            const param = GuardedList.at(params, param_index);
            marks.trackDef(&has_def, &multi_def, param);
            marks.destroy(&foreign_def, param);
            if (origin_facts) |facts| facts.noteForeign(param);
        }
    }

    var reachable_iter = reachable.iterator(.{});
    var exact_stmt_index: usize = 0;
    stmt_loop: while (true) {
        const stmt_index = if (exact_stmts) |stmts| blk: {
            if (exact_stmt_index == stmts.len) break :stmt_loop;
            defer exact_stmt_index += 1;
            break :blk @intFromEnum(stmts[exact_stmt_index]);
        } else reachable_iter.next() orelse break;
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        if (origin_facts) |facts| try collectUniqueOriginStmt(facts, store, stmt, consume_dead_boxes);
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
                    // Static-backed literals view backing whose count is the
                    // static sentinel, never 1, so they are not unique births
                    // and must never take in-place paths.
                    .str_literal, .static_data, .bytes_literal => marks.destroy(&foreign_def, assign.target),
                    .i64_literal,
                    .i128_literal,
                    .f64_literal,
                    .f32_literal,
                    .dec_literal,
                    .boxy_dynamic_num_literal,
                    .boxy_dynamic_frac_literal,
                    .null_ptr,
                    .proc_ref,
                    => marks.noteBirth(&born, assign.target),
                }
            },
            .assign_call => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                if (assign.result_desc) |result_desc| {
                    if (result_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                }
                if (assign.out_desc) |out_desc| {
                    marks.trackDef(&has_def, &multi_def, out_desc);
                    marks.destroy(&foreign_def, out_desc);
                }
                const callee_sig = sigs.get(assign.proc);
                if (callee_sig.ret_unique) {
                    marks.noteBirth(&born, assign.target);
                } else {
                    marks.destroy(&foreign_def, assign.target);
                }
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |position| {
                    const arg = GuardedList.at(args, position);
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
                if (assign.result_desc) |result_desc| {
                    if (result_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                }
                if (assign.out_desc) |out_desc| {
                    marks.trackDef(&has_def, &multi_def, out_desc);
                    marks.destroy(&foreign_def, out_desc);
                }
                if (assign.reuse_source) |reuse_source| {
                    marks.consume(&consumed_once, &destroyed, reuse_source);
                } else {
                    marks.destroy(&destroyed, assign.closure);
                }
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |index| {
                    const arg = GuardedList.at(args, index);
                    marks.destroy(&destroyed, arg);
                }
                const arg_descs = store.getLocalSpan(assign.arg_descs);
                for (0..GuardedList.borrowLen(arg_descs)) |index| {
                    marks.noteUse(&borrow_used, GuardedList.at(arg_descs, index));
                }
            },
            .assign_packed_erased_fn => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                if (assign.capture) |capture| marks.destroy(&destroyed, capture);
                if (assign.result_desc) |result_desc| {
                    if (result_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                }
                if (assign.reuse) |reuse| marks.consume(&consumed_once, &destroyed, reuse);
            },
            .assign_boxy_desc_ref => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                if (assign.tag_residual_for) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                const captures = store.getLocalSpan(assign.captures);
                for (0..GuardedList.borrowLen(captures)) |index| {
                    const local = GuardedList.at(captures, index);
                    marks.noteUse(&borrow_used, local);
                }
            },
            .assign_boxy_dict_ref => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.dict.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_box => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.payload, assign.payload_mode);
                if (assign.source_desc) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                if (assign.payload_desc) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_reuse_box => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                marks.consume(&consumed_once, &destroyed, assign.source);
                if (assign.desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_unbox => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.source, assign.source_mode);
                if (assign.source_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                if (assign.target_desc) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_adapt => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.source, assign.source_mode);
                if (assign.source_desc) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                if (assign.target_desc) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_inspect => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.source, assign.source_mode);
                if (assign.source_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_eq => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.lhs, assign.source_mode);
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.rhs, assign.source_mode);
                if (assign.source_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_tag => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                if (assign.target_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                if (assign.payload) |payload| marks.transfer(&consumed_once, &destroyed, &borrow_used, payload, assign.payload_mode);
                if (assign.payload_desc) |desc| if (desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_boxy_tag_payload => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.target_desc) |local| {
                    marks.trackDef(&has_def, &multi_def, local);
                    marks.destroy(&foreign_def, local);
                }
                marks.transfer(&consumed_once, &destroyed, &borrow_used, assign.source, assign.source_mode);
                if (assign.source_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .boxy_tag_match => |tag_match| {
                marks.noteUse(&borrow_used, tag_match.source);
                if (tag_match.source_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
            },
            .assign_call_dict => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.dict.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                if (assign.result_desc) |result_desc| if (result_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |index| {
                    const arg = GuardedList.at(args, index);
                    marks.destroy(&destroyed, arg);
                }
                const arg_descs = store.getLocalSpan(assign.arg_descs);
                for (0..GuardedList.borrowLen(arg_descs)) |index| {
                    marks.noteUse(&borrow_used, GuardedList.at(arg_descs, index));
                }
                const hidden_args = store.getLocalSpan(assign.hidden_args);
                for (0..GuardedList.borrowLen(hidden_args)) |index| {
                    const arg = GuardedList.at(hidden_args, index);
                    marks.destroy(&destroyed, arg);
                }
            },
            .str_match => |str_match| {
                marks.noteUse(&borrow_used, str_match.source);
                const steps = store.getStrMatchSteps(str_match.steps);
                for (0..GuardedList.borrowLen(steps)) |step_index| {
                    const step = GuardedList.at(steps, step_index);
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
                const arms = store.getStrMatchArms(str_match_set.arms);
                for (0..GuardedList.borrowLen(arms)) |arm_index| {
                    const arm = GuardedList.at(arms, arm_index);
                    const steps = store.getStrMatchSteps(arm.steps);
                    for (0..GuardedList.borrowLen(steps)) |step_index| {
                        const step = GuardedList.at(steps, step_index);
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
                const rc_effect = if (!consume_dead_boxes and assign.op == .box_unbox)
                    assign.op.arcBorrowedResultVariant().?.rcEffect()
                else
                    assign.op.arcInferenceRcEffect(assign.rc_effect);
                marks.trackDef(&has_def, &multi_def, assign.target);
                if (rc_effect.result_unique) {
                    marks.noteBirth(&born, assign.target);
                } else {
                    marks.destroy(&foreign_def, assign.target);
                }
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |position| {
                    const arg = GuardedList.at(args, position);
                    if (position >= 64) {
                        marks.destroy(&destroyed, arg);
                        continue;
                    }
                    const bit = @as(u64, 1) << @as(u6, @intCast(position));
                    var read_only = true;
                    if ((rc_effect.consume_args & bit) != 0) {
                        marks.consume(&consumed_once, &destroyed, arg);
                        read_only = false;
                    }
                    if ((rc_effect.retain_args & bit) != 0) {
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
                const elems = store.getLocalSpan(assign.elems);
                for (0..GuardedList.borrowLen(elems)) |index| {
                    const elem = GuardedList.at(elems, index);
                    marks.destroy(&destroyed, elem);
                }
            },
            .assign_struct => |assign| {
                if (assign.contents_desc) |contents_desc| {
                    if (contents_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                }
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                const fields = store.getLocalSpan(assign.fields);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    const field = GuardedList.at(fields, index);
                    marks.destroy(&destroyed, field);
                }
            },
            .assign_tag => |assign| {
                if (assign.target_desc) |target_desc| if (target_desc.localOrNull()) |local| marks.noteUse(&borrow_used, local);
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                if (assign.payload) |payload| marks.destroy(&destroyed, payload);
            },
            .store_struct => |assign| {
                const fields = store.getLocalSpan(assign.fields);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    const field = GuardedList.at(fields, index);
                    marks.destroy(&destroyed, field);
                }
            },
            .store_tag => |assign| {
                if (assign.payload) |payload| marks.destroy(&destroyed, payload);
            },
            .set_local => |assign| {
                switch (assign.mode) {
                    .initialize_join_param => {
                        const target = marks.indexOf(assign.target);
                        const source = marks.indexOf(assign.value);
                        if (target != null and source != null) {
                            if (target.? != source.?) {
                                try join_incoming.append(allocator, .{ .target = target.?, .source = source.? });
                                marks.consume(&consumed_once, &destroyed, assign.value);
                            }
                        } else {
                            marks.destroy(&foreign_def, assign.target);
                        }
                    },
                    .replace_existing, .initialize_join_result => {
                        marks.trackDef(&has_def, &multi_def, assign.target);
                        marks.destroy(&foreign_def, assign.target);
                        marks.destroy(&destroyed, assign.target);
                        marks.destroy(&destroyed, assign.value);
                    },
                }
            },
            .incref => |rc| marks.destroy(&destroyed, rc.value),
            .join => |join_stmt| {
                const params = store.getLocalSpan(join_stmt.params);
                for (0..GuardedList.borrowLen(params)) |param_index| {
                    const param = GuardedList.at(params, param_index);
                    marks.trackDef(&has_def, &multi_def, param);
                    if (marks.indexOf(param)) |target| join_targets.set(target);
                }
            },
            // Returning is the value's consuming use: the unit moves to the
            // caller, which feeds the per-proc unique-return solve.
            .ret => |ret_stmt| marks.consume(&consumed_once, &destroyed, ret_stmt.value),
            .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| {
                marks.consume(&consumed_once, &destroyed, message);
            },
            .debug => |debug_stmt| marks.noteUse(&borrow_used, debug_stmt.message),
            // The failure report is the message's consuming use.
            .expect_err => |expect_err_stmt| marks.consume(&consumed_once, &destroyed, expect_err_stmt.message),
            .expect => |expect_stmt| marks.noteUse(&borrow_used, expect_stmt.condition),
            .init_uninitialized => {},
            .comptime_branch_taken => {},
            .switch_stmt => |switch_stmt| marks.noteUse(&borrow_used, switch_stmt.cond),
            .switch_initialized_payload => |switch_stmt| marks.noteUse(&borrow_used, switch_stmt.cond),
            .decref_if_initialized => |rc| marks.noteUse(&borrow_used, rc.cond),
            .decref, .free, .jump, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
        }
    }

    // born_unique: every definition is a birth or a settled pure alias, and
    // no foreign definition. unique: born unique with no holder-adding
    // occurrence anywhere.
    var foreign_iter = foreign_def.iterator(.{});
    while (foreign_iter.next()) |index| born.unset(index);

    // A flow-insensitive uniqueness bit denotes one concrete allocation
    // origin. Even when every definition is individually fresh, a local with
    // several definitions can name different runtime allocations and does
    // not have one statically trackable birth.
    var multi_iter = multi_def.iterator(.{});
    while (multi_iter.next()) |index| born.unset(index);

    // An alias target's origin derives from its source, so a birth bit set
    // by another of its definitions must not stand on its own (the alias
    // definition may bind a non-unique value); and a multi-bound alias
    // target never inherits.
    for (alias_targets.items) |target| {
        born.unset(target);
        if (multi_def.isSet(target)) destroyed.set(target);
    }

    try settleUniqueOriginDependencies(
        allocator,
        &born,
        &foreign_def,
        &multi_def,
        &destroyed,
        &borrow_used,
        alias_source,
        alias_targets.items,
        &join_targets,
        join_incoming.items,
    );

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
    const proc_count = store.procSpecCount();

    // Project direct-call edges from the shared typed fact lift.
    var edges = std.ArrayList([2]u32).empty;
    defer edges.deinit(allocator);
    for (solver.direct_calls.items) |call| {
        try edges.append(allocator, .{ call.caller, @intFromEnum(call.callee) });
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
