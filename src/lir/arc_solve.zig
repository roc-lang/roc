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
//! once. Phase B then marks returns borrowed when every returned value is a
//! borrow anchored on a borrowed parameter, and re-solves binding modes so
//! callers may borrow such results. Same-SCC tail calls retain their ordinary
//! mode constraints and record the exact borrowed caller parameter, if any,
//! whose entry lifetime contains each argument occurrence. Emission transfers
//! ownership only when that anchor is absent or owned in the current variant,
//! so frame replacement preserves external borrows without leaving cleanup
//! after the call. After signatures settle,
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
const body_clone = @import("body_clone.zig");
const task_executor = @import("base").post_check_task_executor;

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Errors that can occur while constructing the ARC solver's internal tables.
pub const SolveError = std.mem.Allocator.Error;

/// Debug-only count of exact per-resource outcome work rows, used to pin the
/// polynomial state domain in scaling tests.
pub var outcome_solver_iterations: u64 = 0;

/// Debug-only number of scratch entries initialized or scanned by restitution.
pub var outcome_scratch_entries: u64 = 0;

// Deterministic work accounting for solver tests; compiled out of production.
const UseOrder = @import("use_order.zig").UseOrder;
threadlocal var uniqueness_analysis_rounds: usize = 0;

/// Exact work performed by uniqueness settlements, accumulated by the coordinator.
pub const UniquenessMetrics = struct {
    settlements: u64 = 0,
    components: u64 = 0,
    component_runs: u64 = 0,
    task_submitted: u64 = 0,
    task_committed: u64 = 0,
    signature_waves: u64 = 0,
    signature_changes: u64 = 0,
    statement_visits: u64 = 0,
    local_visits: u64 = 0,

    pub fn add(self: *UniquenessMetrics, other: UniquenessMetrics) void {
        inline for (std.meta.fields(UniquenessMetrics)) |field| {
            @field(self, field.name) +|= @field(other, field.name);
        }
    }
};

/// A null executor uses the identical component solver on the calling thread.
pub const UniquenessOptions = struct {
    executor: ?*const task_executor.Executor = null,
    metrics: ?*UniquenessMetrics = null,
};

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

const no_param_anchor: u8 = std.math.maxInt(u8);

/// Exact lifetime fact for one direct call to a callee in the same call-graph
/// SCC whose result is immediately returned by the caller. Each represented
/// argument either names the caller parameter whose borrowed entry lifetime
/// contains the recursive call, or has no such anchor and must transfer an
/// ownership unit before the caller frame can be replaced.
pub const TailCallLifetime = struct {
    stmt: LIR.CFStmtId,
    anchor_params: [arc_sig.tracked_param_count]u8,
    carriers: [arc_sig.tracked_param_count]u32,

    pub fn anchorParam(self: *const TailCallLifetime, position: usize) ?usize {
        if (position >= arc_sig.tracked_param_count) return null;
        const anchor = self.anchor_params[position];
        return if (anchor == no_param_anchor) null else anchor;
    }

    pub fn carrier(self: *const TailCallLifetime, position: usize) ?LIR.LocalId {
        if (position >= arc_sig.tracked_param_count) return null;
        const local = self.carriers[position];
        return if (local == no_local) null else @enumFromInt(local);
    }

    pub fn argumentOutlivesScc(
        self: *const TailCallLifetime,
        position: usize,
        caller_sig: arc_sig.RcSig,
    ) bool {
        const anchor = self.anchorParam(position) orelse return false;
        return caller_sig.paramMode(anchor) == .borrowed;
    }
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
    /// Pure list descriptor aliases with no allocation-dependent uses.
    /// Their saved representation survives independently of the buffer.
    representation_alias: std.bit_set.DynamicBitSetUnmanaged,
    /// Owned leader anchoring each local's liveness; the local itself when
    /// the binding is owned or is a borrowed parameter.
    leader: []u32,
    /// Source local of each pure same-value alias, or `no_local`.
    alias_source: []u32,
    /// Immediate lender of each solved-borrowed local: the local whose value
    /// it borrows through (its alias source, the container of its field or
    /// payload read, or the argument a borrowed call result borrows from), or
    /// `no_local` for owned bindings and borrowed parameters.
    borrow_source: []u32,
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
    /// Same-SCC tail-call lifetime facts, partitioned by source proc. These
    /// preserve the exact SCC-entry lender for each represented argument so
    /// variants can re-evaluate the constraint under their demanded ABI.
    tail_call_offsets: []u32,
    tail_call_lens: []u32,
    tail_calls: []TailCallLifetime,
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
    /// Bit set => the local's origin is a unique birth under the condition
    /// in `unique_conds`: fresh allocation, or carried by unit transfers
    /// from births and from the parameters the condition names.
    unique_born: std.bit_set.DynamicBitSetUnmanaged,
    /// For each born local, the tracked parameter positions of its proc that
    /// must be seeded born-unique for the birth to hold; zero for a birth
    /// that holds in every emission of the proc.
    unique_conds: []arc_sig.ParamMask,
    /// Flat conditional-return rows referenced by `RcSig.ret_conditions`.
    ret_conditions: []arc_sig.RetCondition,
    /// Bit set => the proc's signature is pinned by ABI (roots, hosted,
    /// erased-callable, bodyless, and address-escaping procs). Pinned procs
    /// are never mode-specialized.
    pinned: std.bit_set.DynamicBitSetUnmanaged,
    /// Store-shaped settlement inputs built by the first uniqueness
    /// settlement and reused by every later one on this solution.
    uniqueness_structure: ?*UniquenessStructure = null,

    pub fn deinit(self: *Solution) void {
        self.borrowed.deinit(self.allocator);
        self.borrowed_call_result.deinit(self.allocator);
        self.representation_alias.deinit(self.allocator);
        self.allocator.free(self.leader);
        self.allocator.free(self.alias_source);
        self.allocator.free(self.borrow_source);
        self.allocator.free(self.sigs);
        self.allocator.free(self.outcomes);
        self.allocator.free(self.ret_conditions);
        self.allocator.free(self.available_outcome_spans);
        self.allocator.free(self.restitution_params_by_stmt);
        self.allocator.free(self.unique_seed_masks);
        self.allocator.free(self.tail_call_offsets);
        self.allocator.free(self.tail_call_lens);
        self.allocator.free(self.tail_calls);
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
        self.unique_born.deinit(self.allocator);
        self.allocator.free(self.unique_conds);
        self.pinned.deinit(self.allocator);
        if (self.uniqueness_structure) |structure| structure.destroy();
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

    pub fn isRepresentationAlias(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        return index < self.representation_alias.capacity() and self.representation_alias.isSet(index);
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

    /// True when the local's value is unique in an emission of its proc
    /// whose demand vector seeds the parameter positions in `seeds`
    /// born-unique: its birth holds under those seeds and nothing adds a
    /// holder.
    pub fn isUniqueUnder(self: *const Solution, local: LIR.LocalId, seeds: arc_sig.ParamMask) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        if (!self.unique_born.isSet(index) or self.unique_destroyed.isSet(index)) return false;
        return (self.unique_conds[index] & ~seeds) == 0;
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
        var cursor = @intFromEnum(local);
        var steps: usize = 0;
        // Every owned binding has an independent unit, even when its value
        // came from another local. Only borrowed links forward that unit.
        while (cursor < self.alias_source.len and
            self.isBorrowed(@enumFromInt(cursor)) and
            self.alias_source[cursor] != no_local)
        {
            cursor = self.alias_source[cursor];
            steps += 1;
            if (steps > self.alias_source.len) solveInvariant("ARC alias-source chain contained a cycle");
        }
        return @enumFromInt(cursor);
    }

    /// The local a solved-borrowed local borrows its value through, or null
    /// for owned bindings and borrowed parameters.
    pub fn borrowSourceOf(self: *const Solution, local: LIR.LocalId) ?LIR.LocalId {
        const index = @intFromEnum(local);
        if (index >= self.borrow_source.len) return null;
        const source = self.borrow_source[index];
        return if (source == no_local) null else @enumFromInt(source);
    }

    pub fn sigTable(self: *const Solution) arc_sig.SigTable {
        return .{ .sigs = self.sigs, .outcomes = self.outcomes, .ret_conditions = self.ret_conditions };
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

    pub fn tailCallsOf(self: *const Solution, proc: LIR.LirProcSpecId) []const TailCallLifetime {
        const index = @intFromEnum(proc);
        if (index >= self.tail_call_offsets.len) return &.{};
        const offset = self.tail_call_offsets[index];
        const len = self.tail_call_lens[index];
        return self.tail_calls[offset..][0..len];
    }

    pub fn tailCallAt(self: *const Solution, proc: LIR.LirProcSpecId, stmt: LIR.CFStmtId) ?*const TailCallLifetime {
        const tail_calls = self.tailCallsOf(proc);
        const stmt_index = @intFromEnum(stmt);
        var left: usize = 0;
        var right = tail_calls.len;
        while (left < right) {
            const middle = left + (right - left) / 2;
            const middle_index = @intFromEnum(tail_calls[middle].stmt);
            if (middle_index < stmt_index) {
                left = middle + 1;
            } else if (middle_index > stmt_index) {
                right = middle;
            } else {
                return &tail_calls[middle];
            }
        }
        return null;
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

/// Classify committed representations once, in local order. Descriptors select
/// dynamic RC behavior; their presence does not imply an ownership resource.
/// Constructors and aliases preserve the RC shape already committed in layouts,
/// including nested erased boxes, so classification requires no value-flow solve.
/// Capture views participate here as borrow anchors; emission excludes them in
/// its separate table. Certification independently classifies the final store.
pub fn computeLocalContainsRefcounted(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
) SolveError![]bool {
    const local_count = store.localCount();
    const contains = try allocator.alloc(bool, local_count);
    errdefer allocator.free(contains);
    for (0..local_count) |index| {
        const local_id: LIR.LocalId = @enumFromInt(@as(u32, @intCast(index)));
        const local = store.getLocal(local_id);
        contains[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }

    return contains;
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
    stmt: LIR.CFStmtId,
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
    stmt: LIR.CFStmtId,
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
    /// A pure same-value alias definition at `stmt`.
    alias: struct { target: LIR.LocalId, source: LIR.LocalId, stmt: LIR.CFStmtId },
    join_target: LIR.LocalId,
    /// `set target := source` on an edge into a join, at `stmt`.
    join_incoming: struct { target: LIR.LocalId, source: LIR.LocalId, stmt: LIR.CFStmtId },
    /// A consuming use of `local` at `stmt`.
    consume: struct { local: LIR.LocalId, stmt: LIR.CFStmtId },
    destroy: LIR.LocalId,
    read: LIR.LocalId,
    representation_read: LIR.LocalId,
};

const UniqueJoinIncoming = struct {
    target: u32,
    source: u32,
};

/// One pure same-value alias definition, resolved after the statement scan.
const AliasDef = struct {
    target: u32,
    source: u32,
    stmt: u32,
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
    return solveWithOptions(allocator, store, layouts, rc_local, boxy_rc_descs, roots, consume_dead_boxes, .{});
}

/// Solves ARC with optional component-parallel uniqueness execution.
pub fn solveWithOptions(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
    roots: []const LIR.LirProcSpecId,
    consume_dead_boxes: bool,
    options: UniquenessOptions,
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
        .sigs = &.{},
        .pinned = .{},
        .scc = &.{},
        .defs = &.{},
        .demand = &.{},
        .alias_source = &.{},
        .param_position = &.{},
        .param_proc = &.{},
        .join_param = .{},
        .maybe_uninitialized_join_param = .{},
        .maybe_uninitialized_condition = &.{},
        .maybe_uninitialized_condition_mask = &.{},
        .param_uses = .empty,
        .binding_facts = .empty,
        .visibility_facts = .empty,
        .unique_facts = .empty,
        .address_taken = .{},
        .proc_stmts = &.{},
        .proc_returns = &.{},
        .proc_join_bodies = &.{},
        .join_index_by_stmt = &.{},
        .jump_target_join_index_by_stmt = &.{},
        .jump_site_index_by_stmt = &.{},
        .switch_count_by_proc = &.{},
        .switch_index_by_stmt = &.{},
        .pending_jumps = .empty,
        .direct_calls = .empty,
        .unique_calls = .empty,
        .stack = std.ArrayList(LIR.CFStmtId).empty,
    };
    defer {
        solver.pinned.deinit(allocator);
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
        allocator.free(solver.join_index_by_stmt);
        allocator.free(solver.jump_target_join_index_by_stmt);
        allocator.free(solver.jump_site_index_by_stmt);
        allocator.free(solver.switch_count_by_proc);
        allocator.free(solver.switch_index_by_stmt);
        solver.pending_jumps.deinit(allocator);
        solver.direct_calls.deinit(allocator);
        solver.unique_calls.deinit(allocator);
        solver.stack.deinit(allocator);
        allocator.free(solver.sigs);
    }

    // Register the sole owner before allocating, including partially built
    // nested inventories. Moved result tables are reset to empty at handoff.
    solver.sigs = try allocator.alloc(arc_sig.RcSig, proc_count);
    solver.pinned = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count);
    solver.scc = try allocator.alloc(u32, proc_count);
    solver.defs = try allocator.alloc(DefKind, arc_local_count);
    solver.demand = try allocator.alloc(bool, arc_local_count);
    solver.alias_source = try allocator.alloc(u32, arc_local_count);
    solver.param_position = try allocator.alloc(u32, arc_local_count);
    solver.param_proc = try allocator.alloc(u32, arc_local_count);
    solver.join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, arc_local_count);
    solver.maybe_uninitialized_join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, arc_local_count);
    solver.maybe_uninitialized_condition = try allocator.alloc(u32, arc_local_count);
    solver.maybe_uninitialized_condition_mask = try allocator.alloc(u64, arc_local_count);
    solver.address_taken = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count);
    solver.proc_stmts = try allocator.alloc(std.ArrayList(LIR.CFStmtId), proc_count);
    @memset(solver.proc_stmts, .empty);
    solver.proc_returns = try allocator.alloc(std.ArrayList(u32), proc_count);
    @memset(solver.proc_returns, .empty);
    solver.proc_join_bodies = try allocator.alloc(std.ArrayList(JoinBody), proc_count);
    @memset(solver.proc_join_bodies, .empty);
    solver.join_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount());
    solver.jump_target_join_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount());
    solver.jump_site_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount());
    solver.switch_count_by_proc = try allocator.alloc(u32, proc_count);
    solver.switch_index_by_stmt = try allocator.alloc(u32, store.cfStmtCount());
    @memset(solver.join_index_by_stmt, no_local);
    @memset(solver.jump_target_join_index_by_stmt, no_local);
    @memset(solver.jump_site_index_by_stmt, no_local);
    @memset(solver.switch_count_by_proc, 0);
    @memset(solver.switch_index_by_stmt, no_local);
    @memset(solver.param_position, no_local);
    @memset(solver.param_proc, no_local);
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
        if (proc.external) {
            // The cache entry was compiled with this signature; the program
            // that links it calls it exactly so.
            sig = .{
                .borrowed_params = @intCast(proc.rc_borrowed_params),
                .ret_mode = if (proc.rc_ret_borrowed) .borrowed else .owned,
                .ret_lenders = @intCast(proc.rc_ret_lenders),
            };
        } else if (!solver.pinned.isSet(proc_index)) {
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
    var representation_aliases = try solveRepresentationAliases(&solver, layouts);
    defer representation_aliases.deinit(allocator);
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

    // Allocation-level cleanup ends at this ownership-transfer boundary.
    // Later failures are handled exclusively by Solution.deinit.
    var solution = solution: {
        var tail_call_table = try buildTailCallTable(&solver, &binding);
        errdefer tail_call_table.deinit(allocator);

        var visible = try computeVisibilityFromFacts(allocator, &solver);
        errdefer visible.deinit(allocator);
        if (builtin.mode == .Debug) {
            var independently_visible = try computeVisibilityFromLift(allocator, store, rc_local, &solver.pinned, solver.proc_stmts, solver.proc_returns);
            defer independently_visible.deinit(allocator);
            if (!visible.eql(independently_visible)) solveInvariant("typed visibility facts disagreed with independent LIR analysis");
        }

        // Uniqueness is settled below against the solved signatures, once the
        // solution's tables exist for the settle to fill.
        var unique = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer unique.deinit(allocator);
        var unique_destroyed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer unique_destroyed.deinit(allocator);
        var unique_born = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer unique_born.deinit(allocator);
        const unique_conds = try allocator.alloc(arc_sig.ParamMask, local_count);
        errdefer allocator.free(unique_conds);
        @memset(unique_conds, 0);
        const unique_seed_masks = try allocator.alloc(arc_sig.ParamMask, proc_count);
        errdefer allocator.free(unique_seed_masks);
        @memset(unique_seed_masks, 0);
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
        var representation_alias = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
        errdefer representation_alias.deinit(allocator);
        const leader = try allocator.alloc(u32, local_count);
        errdefer allocator.free(leader);
        const alias_source = try allocator.alloc(u32, local_count);
        errdefer allocator.free(alias_source);
        const borrow_source = try allocator.alloc(u32, local_count);
        errdefer allocator.free(borrow_source);
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
        @memset(borrow_source, no_local);
        @memset(maybe_uninitialized_condition, no_local);
        @memset(maybe_uninitialized_condition_mask, 0);
        for (domain.arc_to_local, 0..) |local_index, arc_index| {
            if (binding.borrowed.isSet(arc_index)) {
                borrowed.set(local_index);
                switch (solver.defs[arc_index]) {
                    .borrow_capable => |lender| borrow_source[local_index] = domain.localAt(lender),
                    .none, .multi, .fresh => {},
                }
            }
            leader[local_index] = domain.localAt(binding.leader[arc_index]);
            if (representation_aliases.isSet(arc_index)) {
                representation_alias.set(local_index);
                borrowed.set(local_index);
                leader[local_index] = local_index;
                borrow_source[local_index] = no_local;
            }
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

        const available_outcome_spans = try allocator.alloc(arc_sig.OutcomeSpan, proc_count);
        errdefer allocator.free(available_outcome_spans);
        const restitution_params_by_stmt = try allocator.alloc(arc_sig.ParamMask, store.cfStmtCount());
        errdefer allocator.free(restitution_params_by_stmt);
        @memset(restitution_params_by_stmt, 0);
        @memset(available_outcome_spans, .empty);
        break :solution Solution{
            .allocator = allocator,
            .borrowed = borrowed,
            .borrowed_call_result = borrowed_call_result,
            .representation_alias = representation_alias,
            .leader = leader,
            .alias_source = alias_source,
            .borrow_source = borrow_source,
            .sigs = solver.sigs,
            .outcomes = &.{},
            .available_outcome_spans = available_outcome_spans,
            .restitution_params_by_stmt = restitution_params_by_stmt,
            .unique_seed_masks = unique_seed_masks,
            .tail_call_offsets = tail_call_table.offsets,
            .tail_call_lens = tail_call_table.lens,
            .tail_calls = tail_call_table.facts,
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
            .unique_born = unique_born,
            .unique_conds = unique_conds,
            .ret_conditions = &.{},
            .pinned = solver.pinned,
        };
    };
    solver.sigs = &.{};
    solver.pinned = .{};
    solver.join_index_by_stmt = &.{};
    solver.jump_target_join_index_by_stmt = &.{};
    solver.jump_site_index_by_stmt = &.{};
    solver.switch_count_by_proc = &.{};
    solver.switch_index_by_stmt = &.{};
    errdefer solution.deinit();

    try settleUniquenessWithOptions(allocator, store, layouts, rc_local, &solution, .none, consume_dead_boxes, options);
    try computeOutcomeRestitution(allocator, store, layouts, rc_local, consume_dead_boxes, &solution, solver.proc_stmts);

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
    statements_by_proc: []const std.ArrayList(LIR.CFStmtId),
) SolveError!void {
    var all_outcomes = std.ArrayList(arc_sig.Outcome).empty;
    errdefer all_outcomes.deinit(allocator);

    // Reuse capacity across procedures, but address and initialize only the
    // active procedure's explicit statement inventory. The structural lift
    // has already collected it; restitution performs no reachability walk.
    var statement_indices = collections.DenseMap(LIR.CFStmtId, u32).init(allocator);
    defer statement_indices.deinit();
    var discriminant_buffer = std.ArrayList(u32).empty;
    defer discriminant_buffer.deinit(allocator);
    var mask_buffer = std.ArrayList(arc_sig.ParamMask).empty;
    defer mask_buffer.deinit(allocator);
    var bit_discriminant_buffer = std.ArrayList(u32).empty;
    defer bit_discriminant_buffer.deinit(allocator);
    var bit_present_buffer = std.ArrayList(bool).empty;
    defer bit_present_buffer.deinit(allocator);
    const ambiguous_discriminant = no_local - 1;

    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        if (solution.isPinnedProc(proc_id)) continue;
        const proc = store.getProcSpec(proc_id);
        const body = proc.body orelse continue;
        if (layouts.getLayout(proc.ret_layout).tag != .tag_union) continue;

        const proc_stmts = statements_by_proc[proc_index].items;
        var returned_local: ?LIR.LocalId = null;
        var return_shape_valid = true;
        for (proc_stmts) |stmt_id| {
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

        statement_indices.clearRetainingCapacity();
        for (proc_stmts, 0..) |stmt, index| try statement_indices.putNoClobber(stmt, @intCast(index));
        try discriminant_buffer.resize(allocator, proc_stmts.len);
        try mask_buffer.resize(allocator, proc_stmts.len);
        try bit_discriminant_buffer.resize(allocator, proc_stmts.len);
        try bit_present_buffer.resize(allocator, proc_stmts.len);
        const escape_discriminants = discriminant_buffer.items;
        const escape_masks = mask_buffer.items;
        const bit_escape_discriminants = bit_discriminant_buffer.items;
        const bit_escape_present = bit_present_buffer.items;
        @memset(escape_discriminants, no_local);
        @memset(escape_masks, 0);
        if (@import("builtin").mode == .Debug) outcome_scratch_entries += proc_stmts.len + escape_discriminants.len + escape_masks.len;

        var joins = collections.DenseMap(LIR.JoinPointId, LIR.CFStmtId).init(allocator);
        defer joins.deinit();
        for (proc_stmts) |stmt_id| {
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
            if (@import("builtin").mode == .Debug) outcome_scratch_entries += bit_escape_discriminants.len + bit_escape_present.len;
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
                        // Stored arguments may move their entry unit into the
                        // result instead of retaining it, just like aggregate
                        // operands. Neither transfer can promise restitution.
                        const transferred_args = effect.consume_args | effect.retain_args;
                        const args = store.getLocalSpan(assign.args);
                        for (0..GuardedList.borrowLen(args)) |position| {
                            if (position >= 64) {
                                valid = false;
                                break;
                            }
                            const bit = @as(u64, 1) << @as(u6, @intCast(position));
                            if ((transferred_args & bit) == 0) continue;
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
                            const stmt_index = statement_indices.get(current) orelse
                                solveInvariant("ARC outcome escape was outside its lifted procedure inventory");
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
                        const stmt_index = statement_indices.get(current) orelse
                            solveInvariant("ARC outcome escape was outside its lifted procedure inventory");
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

            if (@import("builtin").mode == .Debug) outcome_scratch_entries += bit_escape_discriminants.len;
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
        if (@import("builtin").mode == .Debug) outcome_scratch_entries += escape_discriminants.len;
        for (escape_discriminants, 0..) |discriminant, stmt_index| {
            if (discriminant == no_local or discriminant == ambiguous_discriminant) continue;
            const outcome = accum.get(@intCast(discriminant)) orelse
                solveInvariant("ARC outcome escape named an unreturned discriminant");
            solution.restitution_params_by_stmt[@intFromEnum(proc_stmts[stmt_index])] = escape_masks[stmt_index] & outcome.remaining_on_all_paths;
        }
    }

    solution.outcomes = try all_outcomes.toOwnedSlice(allocator);
}

/// Allocation demand over the already-lifted occurrence inventory. Each local
/// enters the worklist once, and pure alias edges are followed once. Only
/// independently copied list descriptors qualify; projections still require
/// their container at the read, and call/join boundaries keep their contracts.
fn solveRepresentationAliases(solver: *const Solver, layouts: *const layout_mod.Store) SolveError!std.bit_set.DynamicBitSetUnmanaged {
    const allocator = solver.allocator;
    const domain = solver.domain;
    const count = domain.arc_to_local.len;
    var aliases = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, count);
    errdefer aliases.deinit(allocator);
    var required = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, count);
    defer required.deinit(allocator);
    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    for (solver.alias_source, 0..) |source, target| {
        if (source == no_local or solver.defs[target] != .borrow_capable or solver.join_param.isSet(target)) continue;
        const target_layout = layouts.getLayout(solver.store.getLocal(@enumFromInt(domain.localAt(@intCast(target)))).layout_idx);
        const source_layout = layouts.getLayout(solver.store.getLocal(@enumFromInt(domain.localAt(source))).layout_idx);
        if ((target_layout.tag == .list or target_layout.tag == .list_of_zst) and
            (source_layout.tag == .list or source_layout.tag == .list_of_zst)) aliases.set(target);
    }
    const Seed = struct {
        fn add(s: *const Solver, bits: *std.bit_set.DynamicBitSetUnmanaged, queue: *std.ArrayList(u32), local: LIR.LocalId) SolveError!void {
            const index = s.domain.indexOf(local) orelse return;
            if (bits.isSet(index)) return;
            bits.set(index);
            try queue.append(s.allocator, index);
        }
    };
    for (solver.unique_facts.items) |fact| switch (fact) {
        .destroy, .read, .join_target => |local| try Seed.add(solver, &required, &work, local),
        .consume => |consume| try Seed.add(solver, &required, &work, consume.local),
        .alias => |alias| {
            const target = domain.indexOf(alias.target);
            if (target == null or !aliases.isSet(target.?)) try Seed.add(solver, &required, &work, alias.source);
        },
        .join_incoming => |incoming| try Seed.add(solver, &required, &work, incoming.source),
        .birth, .foreign, .representation_read => {},
    };
    for (solver.unique_calls.items) |call| {
        const args = solver.store.getLocalSpan(call.args);
        for (0..GuardedList.borrowLen(args)) |index| try Seed.add(solver, &required, &work, GuardedList.at(args, index));
    }
    while (work.pop()) |target| {
        if (!aliases.isSet(target)) continue;
        const source = solver.alias_source[target];
        if (source != no_local) try Seed.add(solver, &required, &work, @enumFromInt(domain.localAt(source)));
    }
    required.toggleAll();
    aliases.setIntersection(required);
    return aliases;
}

const BindingResult = struct {
    borrowed: std.bit_set.DynamicBitSetUnmanaged,
    leader: []u32,

    fn deinit(self: *BindingResult, allocator: Allocator) void {
        self.borrowed.deinit(allocator);
        allocator.free(self.leader);
    }
};

const TailCallTable = struct {
    offsets: []u32,
    lens: []u32,
    facts: []TailCallLifetime,

    fn deinit(self: *TailCallTable, allocator: Allocator) void {
        allocator.free(self.offsets);
        allocator.free(self.lens);
        allocator.free(self.facts);
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

/// Builds the exact lifetime boundary facts for recursive tail calls after
/// parameter modes and borrowed-return lenders are final. A borrowed argument
/// outlives frame replacement exactly when its solved lender is a borrowed
/// entry parameter of this caller. Every other represented RC argument names
/// the local that must carry an ownership unit into a mandatory callee
/// variant.
fn buildTailCallTable(
    solver: *const Solver,
    binding: *const BindingResult,
) SolveError!TailCallTable {
    const allocator = solver.allocator;
    const proc_count = solver.sigs.len;
    const offsets = try allocator.alloc(u32, proc_count);
    errdefer allocator.free(offsets);
    const lens = try allocator.alloc(u32, proc_count);
    errdefer allocator.free(lens);
    @memset(lens, 0);

    for (solver.direct_calls.items) |call| {
        if (!call.tail) continue;
        if (solver.scc[call.caller] != solver.scc[@intFromEnum(call.callee)]) continue;
        lens[call.caller] += 1;
    }

    var fact_count: u32 = 0;
    for (lens, 0..) |len, proc_index| {
        offsets[proc_index] = fact_count;
        fact_count += len;
    }
    const facts = try allocator.alloc(TailCallLifetime, fact_count);
    errdefer allocator.free(facts);
    const fill = try allocator.dupe(u32, offsets);
    defer allocator.free(fill);

    for (solver.direct_calls.items) |call| {
        if (!call.tail) continue;
        if (solver.scc[call.caller] != solver.scc[@intFromEnum(call.callee)]) continue;

        var fact = TailCallLifetime{
            .stmt = call.stmt,
            .anchor_params = [_]u8{no_param_anchor} ** arc_sig.tracked_param_count,
            .carriers = [_]u32{no_local} ** arc_sig.tracked_param_count,
        };
        const args = solver.store.getLocalSpan(call.args);
        for (0..@min(GuardedList.borrowLen(args), arc_sig.tracked_param_count)) |position| {
            const argument = solver.domain.indexOf(GuardedList.at(args, position)) orelse continue;
            if (!binding.borrowed.isSet(argument)) continue;
            fact.carriers[position] = tailArgumentCarrier(solver, binding, argument);
            const leader = binding.leader[argument];
            if (!paramIsBorrowed(solver, leader)) continue;
            if (solver.param_proc[leader] != call.caller) continue;
            const anchor = solver.param_position[leader];
            if (anchor >= arc_sig.tracked_param_count) continue;
            fact.anchor_params[position] = @intCast(anchor);
        }

        facts[fill[call.caller]] = fact;
        fill[call.caller] += 1;
    }

    for (offsets, lens) |offset, len| {
        const start: usize = @intCast(offset);
        const count: usize = @intCast(len);
        std.mem.sort(TailCallLifetime, facts[start..][0..count], {}, tailCallLifetimeLessThan);
    }

    return .{ .offsets = offsets, .lens = lens, .facts = facts };
}

fn tailCallLifetimeLessThan(_: void, left: TailCallLifetime, right: TailCallLifetime) bool {
    return @intFromEnum(left.stmt) < @intFromEnum(right.stmt);
}

/// Mirrors `Solution.unitLocalOf` in the dense solver domain. Borrowed pure
/// aliases transfer their source's unit; other borrowed definitions need an
/// owned override on their own binding when a tail-call lifetime escapes.
fn tailArgumentCarrier(solver: *const Solver, binding: *const BindingResult, argument: u32) u32 {
    var cursor = argument;
    var steps: usize = 0;
    while (binding.borrowed.isSet(cursor) and solver.alias_source[cursor] != no_local) {
        cursor = solver.alias_source[cursor];
        steps += 1;
        if (steps > solver.alias_source.len) solveInvariant("ARC tail-call carrier alias chain contained a cycle");
    }
    return solver.domain.localAt(cursor);
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
            try liftProcStmtFacts(solver, @intCast(proc_index), current);
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
    current: LIR.CFStmtId,
) SolveError!void {
    switch (solver.store.getCFStmt(current)) {
        .assign_call => |assign| try solver.direct_calls.append(solver.allocator, .{
            .caller = proc_index,
            .stmt = current,
            .callee = assign.proc,
            .args = assign.args,
            .target = assign.target,
            .tail = blk: {
                const next = solver.store.getCFStmt(assign.next);
                break :blk next == .ret and next.ret.value == assign.target;
            },
        }),
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
        .assign_low_level,
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

const appendStructuralSuccessors = @import("use_order.zig").appendStructuralSuccessors;

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
    // current optimistic parameter signatures. Tailness never changes the
    // ownership relation: same-SCC tail arguments get a separate exact
    // lifetime fact after borrowed-return lenders settle.
    for (solver.direct_calls.items) |call| {
        const callee_sig = solver.sigs[@intFromEnum(call.callee)];
        const args = solver.store.getLocalSpan(call.args);
        for (0..GuardedList.borrowLen(args)) |position| {
            const arg = GuardedList.at(args, position);
            const argument = solver.domain.indexOf(arg) orelse continue;
            if (position < arc_sig.tracked_param_count) {
                const key = @intFromEnum(call.callee) * arc_sig.tracked_param_count + position;
                try solver.param_uses.append(solver.allocator, .{
                    .key = @intCast(key),
                    .argument = argument,
                });
            }
            if (callee_sig.paramMode(position) == .borrowed) continue;
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

fn liftBoxyTransfer(solver: *Solver, local: LIR.LocalId, mode: LIR.BoxyTransferMode, stmt: LIR.CFStmtId) SolveError!void {
    switch (mode) {
        .borrow => try solver.unique_facts.append(solver.allocator, .{ .read = local }),
        .copy => try solver.unique_facts.append(solver.allocator, .{ .destroy = local }),
        .move => {
            try solver.binding_facts.append(solver.allocator, .{ .demand = local });
            try solver.unique_facts.append(solver.allocator, .{ .consume = .{ .local = local, .stmt = stmt } });
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
                    try solver.unique_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = source, .stmt = current } });
                },
                .list_reinterpret => |op| {
                    try liftVisibilityLink(solver, assign.target, op.backing_ref);
                    try solver.unique_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = op.backing_ref, .stmt = current } });
                },
                .nominal => |op| {
                    try liftVisibilityLink(solver, assign.target, op.backing_ref);
                    try solver.unique_facts.append(allocator, .{ .alias = .{ .target = assign.target, .source = op.backing_ref, .stmt = current } });
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
                .stmt = current,
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
                try solver.unique_facts.append(allocator, .{ .consume = .{ .local = reuse_source, .stmt = current } });
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
            if (assign.reuse) |reuse| try solver.unique_facts.append(allocator, .{ .consume = .{ .local = reuse, .stmt = current } });
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
            const captures = store.getLocalSpan(assign.captures);
            for (0..GuardedList.borrowLen(captures)) |index| {
                const local = GuardedList.at(captures, index);
                try solver.binding_facts.append(allocator, .{ .demand = local });
                try solver.unique_facts.append(allocator, .{ .read = local });
            }
        },
        .assign_boxy_box => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyTransfer(solver, assign.payload, assign.payload_mode, current);
            if (assign.source_desc) |desc| try liftBoxyDescRead(solver, desc);
            if (assign.payload_desc) |desc| try liftBoxyDescRead(solver, desc);
            try liftVisibilityLink(solver, assign.target, assign.payload);
        },
        .assign_boxy_reuse_box => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.binding_facts.append(allocator, .{ .demand = assign.source });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try solver.unique_facts.append(allocator, .{ .consume = .{ .local = assign.source, .stmt = current } });
            try liftBoxyDescRead(solver, assign.desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_boxy_unbox => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try liftBoxyTransfer(solver, assign.source, assign.source_mode, current);
            try liftBoxyDescRead(solver, assign.source_desc);
            if (assign.target_desc) |desc| try liftBoxyDescRead(solver, desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_boxy_adapt => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .foreign = assign.target });
            try liftBoxyTransfer(solver, assign.source, assign.source_mode, current);
            if (assign.source_desc) |desc| try liftBoxyDescRead(solver, desc);
            if (assign.target_desc) |desc| try liftBoxyDescRead(solver, desc);
            try liftVisibilityLink(solver, assign.target, assign.source);
        },
        .assign_boxy_inspect => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyTransfer(solver, assign.source, assign.source_mode, current);
            try liftBoxyDescRead(solver, assign.source_desc);
        },
        .assign_boxy_eq => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyTransfer(solver, assign.lhs, assign.source_mode, current);
            try liftBoxyTransfer(solver, assign.rhs, assign.source_mode, current);
            try liftBoxyDescRead(solver, assign.source_desc);
        },
        .assign_boxy_tag => |assign| {
            try solver.binding_facts.append(allocator, .{ .fresh = assign.target });
            try solver.unique_facts.append(allocator, .{ .birth = assign.target });
            try liftBoxyDescRead(solver, assign.target_desc);
            if (assign.payload) |payload| {
                try liftBoxyTransfer(solver, payload, assign.payload_mode, current);
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
            try liftBoxyTransfer(solver, assign.source, assign.source_mode, current);
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
                    try solver.unique_facts.append(allocator, .{ .consume = .{ .local = arg, .stmt = current } });
                    read_only = false;
                }
                if ((rc_effect.retain_args & bit) != 0) {
                    try solver.unique_facts.append(allocator, .{ .destroy = arg });
                    read_only = false;
                }
                if (read_only) try solver.unique_facts.append(allocator, if ((assign.op.representationArgs() & bit) != 0)
                    .{ .representation_read = arg }
                else
                    .{ .read = arg });
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
                try solver.unique_facts.append(allocator, .{ .consume = .{ .local = field, .stmt = current } });
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
                try solver.unique_facts.append(allocator, .{ .consume = .{ .local = payload, .stmt = current } });
            }
        },
        .store_struct => |assign| {
            try solver.binding_facts.append(allocator, .{ .demand = assign.dest });
            const fields = store.getLocalSpan(assign.fields);
            for (0..GuardedList.borrowLen(fields)) |index| {
                const field = GuardedList.at(fields, index);
                try solver.binding_facts.append(allocator, .{ .demand = field });
                try solver.unique_facts.append(allocator, .{ .consume = .{ .local = field, .stmt = current } });
            }
        },
        .store_tag => |assign| {
            try solver.binding_facts.append(allocator, .{ .demand = assign.dest });
            if (assign.payload) |payload| {
                try solver.binding_facts.append(allocator, .{ .demand = payload });
                try solver.unique_facts.append(allocator, .{ .consume = .{ .local = payload, .stmt = current } });
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
                        .stmt = current,
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
            try solver.unique_facts.append(allocator, .{ .consume = .{ .local = expect_err_stmt.message, .stmt = current } });
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
        .ret => |ret_stmt| try solver.unique_facts.append(allocator, .{ .consume = .{ .local = ret_stmt.value, .stmt = current } }),
        .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| {
            try solver.binding_facts.append(allocator, .{ .demand = message });
            try solver.unique_facts.append(allocator, .{ .consume = .{ .local = message, .stmt = current } });
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
    /// Per local, the refcounted fields of an aggregate value whose stored
    /// allocation has count 1 and no other holder: stored from a unique
    /// local as its single consuming use, and reachable through payload
    /// views and pure aliases. Bit i names original struct field i, or a
    /// tag union's single payload at bit 0.
    field_masks: []u64,
    /// Per born local, the tracked parameter positions of its proc that must
    /// be seeded born-unique for the birth to hold: a parameter's own
    /// position, carried along every unit transfer from it; zero for a
    /// birth that holds in every emission.
    conds: []arc_sig.ParamMask,
    /// The same conditions for the per-field origins in `field_masks`.
    field_conds: FieldConds,
    /// Bit set => some statement consumes the local's value (moves its
    /// unit, or would retain to do so).
    consumed: std.bit_set.DynamicBitSetUnmanaged,

    pub fn deinit(self: *Uniqueness, allocator: Allocator) void {
        self.born_unique.deinit(allocator);
        self.unique.deinit(allocator);
        self.destroyed.deinit(allocator);
        allocator.free(self.field_masks);
        allocator.free(self.conds);
        self.field_conds.deinit(allocator);
        self.consumed.deinit(allocator);
    }
};

/// Conditions on per-field unique origins: a container with any field input
/// owns sixty-four parameter masks, one per field, at `base[container]`.
pub const FieldConds = struct {
    base: []u32,
    conds: []arc_sig.ParamMask,

    pub fn get(self: FieldConds, container: u32, field: u32) arc_sig.ParamMask {
        const base = self.base[container];
        if (base == no_local) return 0;
        return self.conds[base + field];
    }

    pub fn deinit(self: *FieldConds, allocator: Allocator) void {
        allocator.free(self.base);
        allocator.free(self.conds);
    }
};

/// Where the independent analysis learns which field reads move a stored
/// unit. A take is the one field read that consumes a dying container's
/// unit for that field, so only a take can carry a field's uniqueness to
/// its result.
pub const TakeSource = union(enum) {
    /// No read takes: field reads never inherit.
    none,
    /// The `take_kind` emission stamped on each `assign_ref`.
    stamped,
    /// The statements of committed unconditional takes, before emission.
    set: *const std.bit_set.DynamicBitSetUnmanaged,
};

const FieldStoreEdge = struct { source: u32, container: u32, field: u32, stmt: u32 };
const MaskAliasEdge = struct { source: u32, target: u32 };
const FieldReadEdge = struct { container: u32, target: u32, field: u32, stmt: u32, take: bool };
const SeedMask = struct { local: u32, mask: u64 };
/// One call site's use of a callee's conditional-return row: the part of
/// the result the row names is born exactly when every argument in
/// `call_args[args_start..][0..args_len]` is, under their joined conditions.
const CallEdge = struct { target: u32, field: u8, args_start: u32, args_len: u32, stmt: u32 };

/// Field-level unique origins fed to the settle worklist alongside the
/// alias and join edges.
const FieldEdges = struct {
    stores: []const FieldStoreEdge = &.{},
    aliases: []const MaskAliasEdge = &.{},
    reads: []const FieldReadEdge = &.{},
    seeds: []const SeedMask = &.{},
    calls: []const CallEdge = &.{},
    call_args: []const u32 = &.{},
};

/// Rows of edge indices grouped by one key local.
const EdgeRows = struct {
    offsets: []u32,
    items: []u32,

    fn build(allocator: Allocator, local_count: usize, keys: []const u32) Allocator.Error!EdgeRows {
        const offsets = try allocator.alloc(u32, local_count + 1);
        errdefer allocator.free(offsets);
        @memset(offsets, 0);
        for (keys) |key| offsets[key + 1] += 1;
        for (0..local_count) |index| offsets[index + 1] += offsets[index];
        const items = try allocator.alloc(u32, keys.len);
        errdefer allocator.free(items);
        const fill = try allocator.dupe(u32, offsets[0..local_count]);
        defer allocator.free(fill);
        for (keys, 0..) |key, index| {
            items[fill[key]] = @intCast(index);
            fill[key] += 1;
        }
        return .{ .offsets = offsets, .items = items };
    }

    fn row(self: EdgeRows, key: u32) []const u32 {
        return self.items[self.offsets[key]..self.offsets[key + 1]];
    }

    fn deinit(self: *EdgeRows, allocator: Allocator) void {
        allocator.free(self.offsets);
        allocator.free(self.items);
    }
};

/// One consuming use of a local, positioned at the statement that performs
/// it. Whether it destroys the local's uniqueness is decided once every use
/// is known, by asking whether any other use can still execute after it.
const ConsumeAt = struct {
    index: u32,
    stmt: u32,
};

/// One transfer edge (alias definition or join-parameter initialization)
/// whose target is foreign when the source is used again after `stmt`.
const EdgeCheck = struct { source: LIR.LocalId, stmt: u32, target: u32 };

/// Decides every transfer edge's liveness with one marking per source local.
fn settleEdgeChecks(order: *UseOrder, checks: []EdgeCheck, ctx: anytype) SolveError!void {
    std.mem.sort(EdgeCheck, checks, {}, struct {
        fn lessThan(_: void, a: EdgeCheck, b: EdgeCheck) bool {
            return @intFromEnum(a.source) < @intFromEnum(b.source);
        }
    }.lessThan);
    for (checks) |check| {
        if (try order.usesAfter(check.stmt, check.source)) ctx.dead(check.target);
    }
}

/// A consuming use takes the value's single unit; a second consume that can
/// still execute after it finds the unit gone and destroys the local's
/// uniqueness. Reads after a consume are left to emission, whose facts for
/// the checked argument itself are path-sensitive.
fn destroyOrderedConsumes(
    allocator: Allocator,
    order: *UseOrder,
    consumes: []const ConsumeAt,
    index_to_local: []const u32,
    destroyed: *std.bit_set.DynamicBitSetUnmanaged,
) SolveError!void {
    // Consume statements grouped per local, sorted for membership tests.
    const sorted = try allocator.dupe(ConsumeAt, consumes);
    defer allocator.free(sorted);
    std.mem.sort(ConsumeAt, sorted, {}, struct {
        fn lessThan(_: void, a: ConsumeAt, b: ConsumeAt) bool {
            return a.index < b.index or (a.index == b.index and a.stmt < b.stmt);
        }
    }.lessThan);
    const stmts = try allocator.alloc(u32, sorted.len);
    defer allocator.free(stmts);
    for (sorted, 0..) |consume, position| stmts[position] = consume.stmt;
    var start: usize = 0;
    while (start < sorted.len) {
        var end = start;
        while (end < sorted.len and sorted[end].index == sorted[start].index) end += 1;
        const index = sorted[start].index;
        if (!destroyed.isSet(index)) {
            const among = stmts[start..end];
            // One statement consuming the local twice, or a consume that
            // can run again after another, holds a second unit. One walk
            // from every consume finds any consume that follows another.
            var twice = false;
            for (among[1..], among[0 .. among.len - 1]) |stmt, previous| twice = twice or stmt == previous;
            const local: LIR.LocalId = @enumFromInt(index_to_local[index]);
            if (!twice) try order.markAmong(local, among);
            for (among) |stmt| {
                if (twice or order.after(stmt, local)) {
                    destroyed.set(index);
                    break;
                }
            }
        }
        start = end;
    }
}

/// The consuming-use relation is independent of return uniqueness. Its exact
/// input inventory is the invalidation key: changing a parameter mode or view
/// can change that inventory, while changing a conditional return row cannot.
const ConsumptionProof = struct {
    consumes: std.ArrayList(ConsumeAt) = .empty,
    destroyed: std.bit_set.DynamicBitSetUnmanaged = .{},
    valid: bool = false,
    rebuilds: usize = 0,

    fn deinit(self: *ConsumptionProof, allocator: Allocator) void {
        self.consumes.deinit(allocator);
        self.destroyed.deinit(allocator);
    }

    fn apply(
        self: *ConsumptionProof,
        allocator: Allocator,
        order: *UseOrder,
        consumes: []const ConsumeAt,
        index_to_local: []const u32,
        destroyed: *std.bit_set.DynamicBitSetUnmanaged,
    ) SolveError!void {
        var same = self.valid and self.consumes.items.len == consumes.len;
        if (same) for (self.consumes.items, consumes) |old, new| {
            if (old.index != new.index or old.stmt != new.stmt) {
                same = false;
                break;
            }
        };
        if (!same) {
            self.valid = false;
            if (self.destroyed.bit_length != index_to_local.len) {
                try self.destroyed.resize(allocator, index_to_local.len, false);
            }
            self.destroyed.setRangeValue(.{ .start = 0, .end = index_to_local.len }, false);
            self.consumes.clearRetainingCapacity();
            try self.consumes.appendSlice(allocator, consumes);
            // Compute just consumption's contribution; holder-adding uses
            // remain signature-dependent and are combined on every round.
            try destroyOrderedConsumes(allocator, order, consumes, index_to_local, &self.destroyed);
            self.valid = true;
            if (builtin.is_test) self.rebuilds += 1;
        }
        destroyed.setUnion(self.destroyed);
    }
};

/// Settles unique origins, their parameter conditions, per-field origins,
/// and holder-adding deadness over the alias, join, field, and call edges
/// as one greatest fixpoint.
///
/// Every derived local (an alias target, a join parameter, a committed take,
/// or a call result a conditional-return row describes) starts born under
/// the empty condition and is lowered by re-evaluation: an alias takes its
/// source's value, a join parameter the meet of its incoming edges, a take
/// the container's field, and a call part the meet of the arguments its row
/// names. A meet is unborn when any input is unborn and otherwise requires
/// every input's parameters. Values only descend, so a cycle with no
/// contradiction—a loop that hands one unit around through a callee that
/// returns its parameter—keeps the birth its entry edge brings, which is the
/// inductive fact the runtime obeys. Deadness ascends along the same edges:
/// a second holder of a source is a second holder of everything the source
/// flows into.
fn settleUniqueOrigins(
    allocator: Allocator,
    born: *std.bit_set.DynamicBitSetUnmanaged,
    conds: []arc_sig.ParamMask,
    foreign: *const std.bit_set.DynamicBitSetUnmanaged,
    multi_def: *const std.bit_set.DynamicBitSetUnmanaged,
    multi_ok: *const std.bit_set.DynamicBitSetUnmanaged,
    destroyed: *std.bit_set.DynamicBitSetUnmanaged,
    alias_source: []const u32,
    alias_targets: []const u32,
    join_incoming: []const UniqueJoinIncoming,
    field_edges: FieldEdges,
    masks: []u64,
    dead_masks: []u64,
    field_conds: *FieldConds,
) SolveError!void {
    const local_count = alias_source.len;

    // Dependents of a local: its alias targets, the join parameters it
    // initializes, the containers it is stored into, and the call edges it
    // is an argument of. Inputs of a container: its stores, the containers
    // it views, its call field edges, and its unconditional seeds.
    const store_keys = try allocator.alloc(u32, field_edges.stores.len);
    defer allocator.free(store_keys);
    for (field_edges.stores, 0..) |edge, index| store_keys[index] = edge.source;
    var store_rows = try EdgeRows.build(allocator, local_count, store_keys);
    defer store_rows.deinit(allocator);
    for (field_edges.stores, 0..) |edge, index| store_keys[index] = edge.container;
    var store_inputs = try EdgeRows.build(allocator, local_count, store_keys);
    defer store_inputs.deinit(allocator);
    const mask_alias_keys = try allocator.alloc(u32, field_edges.aliases.len);
    defer allocator.free(mask_alias_keys);
    for (field_edges.aliases, 0..) |edge, index| mask_alias_keys[index] = edge.source;
    var mask_alias_rows = try EdgeRows.build(allocator, local_count, mask_alias_keys);
    defer mask_alias_rows.deinit(allocator);
    for (field_edges.aliases, 0..) |edge, index| mask_alias_keys[index] = edge.target;
    var mask_alias_inputs = try EdgeRows.build(allocator, local_count, mask_alias_keys);
    defer mask_alias_inputs.deinit(allocator);
    const read_keys = try allocator.alloc(u32, field_edges.reads.len);
    defer allocator.free(read_keys);
    for (field_edges.reads, 0..) |edge, index| read_keys[index] = edge.container;
    var read_rows = try EdgeRows.build(allocator, local_count, read_keys);
    defer read_rows.deinit(allocator);
    for (field_edges.reads, 0..) |edge, index| read_keys[index] = edge.target;
    var read_inputs = try EdgeRows.build(allocator, local_count, read_keys);
    defer read_inputs.deinit(allocator);
    const seed_keys = try allocator.alloc(u32, field_edges.seeds.len);
    defer allocator.free(seed_keys);
    for (field_edges.seeds, 0..) |seed, index| seed_keys[index] = seed.local;
    var seed_inputs = try EdgeRows.build(allocator, local_count, seed_keys);
    defer seed_inputs.deinit(allocator);
    const call_keys = try allocator.alloc(u32, field_edges.calls.len);
    defer allocator.free(call_keys);
    for (field_edges.calls, 0..) |edge, index| call_keys[index] = edge.target;
    var call_inputs = try EdgeRows.build(allocator, local_count, call_keys);
    defer call_inputs.deinit(allocator);
    // The arguments of the live call edges, compacted, each with its edge.
    var call_arg_total: usize = 0;
    for (field_edges.calls) |edge| call_arg_total += edge.args_len;
    const call_arg_keys = try allocator.alloc(u32, call_arg_total);
    defer allocator.free(call_arg_keys);
    const call_arg_edges = try allocator.alloc(u32, call_arg_total);
    defer allocator.free(call_arg_edges);
    var call_arg_fill: usize = 0;
    for (field_edges.calls, 0..) |edge, edge_index| {
        for (field_edges.call_args[edge.args_start..][0..edge.args_len]) |arg| {
            call_arg_keys[call_arg_fill] = arg;
            call_arg_edges[call_arg_fill] = @intCast(edge_index);
            call_arg_fill += 1;
        }
    }
    var call_arg_rows = try EdgeRows.build(allocator, local_count, call_arg_keys);
    defer call_arg_rows.deinit(allocator);

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

    const join_keys = try allocator.alloc(u32, join_incoming.len);
    defer allocator.free(join_keys);
    for (join_incoming, 0..) |incoming, index| join_keys[index] = incoming.source;
    var join_rows = try EdgeRows.build(allocator, local_count, join_keys);
    defer join_rows.deinit(allocator);
    for (join_incoming, 0..) |incoming, index| join_keys[index] = incoming.target;
    var join_inputs = try EdgeRows.build(allocator, local_count, join_keys);
    defer join_inputs.deinit(allocator);

    // Optimistic start: every derived local is born under no condition,
    // every container with an input has every field, and nothing is dead
    // beyond what the occurrence scan found.
    @memset(masks, 0);
    @memset(dead_masks, 0);
    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    var queued = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer queued.deinit(allocator);
    var mask_work = std.ArrayList(u32).empty;
    defer mask_work.deinit(allocator);
    var mask_queued = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer mask_queued.deinit(allocator);
    var container_count: u32 = 0;
    for (0..local_count) |index| {
        const local: u32 = @intCast(index);
        const derived = alias_source[local] != no_local or join_inputs.row(local).len != 0 or
            read_inputs.row(local).len != 0 or call_inputs.row(local).len != 0;
        if (derived) {
            if (foreign.isSet(local) or (multi_def.isSet(local) and !multi_ok.isSet(local))) {
                born.unset(local);
            } else {
                born.set(local);
                conds[local] = 0;
            }
            queued.set(local);
            try work.append(allocator, local);
        }
        // A container keeps per-field origins only when every definition of
        // it is accounted for, as its own birth is: a definition that feeds
        // no field inputs (a procedure parameter that is also a tail loop's
        // join parameter, or a replaced value) brings fields of unknown origin.
        const all_defs_tracked = !multi_def.isSet(local) or multi_ok.isSet(local);
        const has_fields = all_defs_tracked and (store_inputs.row(local).len != 0 or mask_alias_inputs.row(local).len != 0 or
            seed_inputs.row(local).len != 0 or call_inputs.row(local).len != 0);
        if (has_fields) {
            masks[local] = std.math.maxInt(u64);
            field_conds.base[local] = container_count * 64;
            container_count += 1;
            mask_queued.set(local);
            try mask_work.append(allocator, local);
        }
    }
    field_conds.conds = try allocator.alloc(arc_sig.ParamMask, @as(usize, container_count) * 64);
    @memset(field_conds.conds, 0);

    const Meet = struct {
        /// The condition under which every input so far is born, or null
        /// once one input is unborn.
        value: ?arc_sig.ParamMask = 0,
        any: bool = false,

        fn fromLocal(self: *@This(), b: *const std.bit_set.DynamicBitSetUnmanaged, c: []const arc_sig.ParamMask, index: u32) void {
            self.any = true;
            if (self.value) |value| self.value = if (b.isSet(index)) value | c[index] else null;
        }

        fn fromConstant(self: *@This()) void {
            self.any = true;
        }

        fn fromSlot(self: *@This(), m: []const u64, fc: *const FieldConds, container: u32, field: u32) void {
            self.any = true;
            const bit = @as(u64, 1) << @as(u6, @intCast(field));
            if (self.value) |value| self.value = if ((m[container] & bit) != 0) value | fc.get(container, field) else null;
        }

        fn isBorn(self: @This()) bool {
            return self.any and self.value != null;
        }
    };

    while (work.items.len != 0 or mask_work.items.len != 0) {
        while (mask_work.pop()) |container| {
            mask_queued.unset(container);
            // Containers without tracked field origins keep an empty mask.
            if (field_conds.base[container] == no_local) continue;
            var meets: [64]Meet = @splat(Meet{});
            var dead: u64 = dead_masks[container];
            for (store_inputs.row(container)) |edge_index| {
                const edge = field_edges.stores[edge_index];
                meets[edge.field].fromLocal(born, conds, edge.source);
                if (destroyed.isSet(edge.source)) dead |= @as(u64, 1) << @as(u6, @intCast(edge.field));
            }
            for (mask_alias_inputs.row(container)) |edge_index| {
                const source = field_edges.aliases[edge_index].source;
                for (0..64) |field| meets[field].fromSlot(masks, field_conds, source, @intCast(field));
                dead |= dead_masks[source];
            }
            for (seed_inputs.row(container)) |seed_index| {
                const seed_mask = field_edges.seeds[seed_index].mask;
                for (0..64) |field| {
                    if ((seed_mask & (@as(u64, 1) << @as(u6, @intCast(field)))) != 0) meets[field].fromConstant();
                }
            }
            for (call_inputs.row(container)) |edge_index| {
                const edge = field_edges.calls[edge_index];
                if (edge.field == arc_sig.RetCondition.whole_value) continue;
                const meet = &meets[edge.field];
                for (field_edges.call_args[edge.args_start..][0..edge.args_len]) |arg| {
                    meet.fromLocal(born, conds, arg);
                    if (destroyed.isSet(arg)) dead |= @as(u64, 1) << @as(u6, @intCast(edge.field));
                }
            }
            var mask: u64 = 0;
            var changed = dead != dead_masks[container];
            for (meets, 0..) |meet, field| {
                if (!meet.isBorn()) continue;
                const bit = @as(u64, 1) << @as(u6, @intCast(field));
                mask |= bit;
                const slot = field_conds.base[container] + field;
                if (field_conds.conds[slot] != meet.value.?) {
                    field_conds.conds[slot] = meet.value.?;
                    changed = true;
                }
            }
            if (mask != masks[container]) changed = true;
            if (!changed) continue;
            masks[container] = mask;
            dead_masks[container] = dead;
            for (mask_alias_rows.row(container)) |edge_index| {
                const target = field_edges.aliases[edge_index].target;
                if (!mask_queued.isSet(target)) {
                    mask_queued.set(target);
                    try mask_work.append(allocator, target);
                }
            }
            for (read_rows.row(container)) |edge_index| {
                const target = field_edges.reads[edge_index].target;
                if (!queued.isSet(target)) {
                    queued.set(target);
                    try work.append(allocator, target);
                }
            }
        }
        const local = work.pop() orelse continue;
        queued.unset(local);

        var meet = Meet{};
        var dead = destroyed.isSet(local);
        if (alias_source[local] != no_local) {
            const source = alias_source[local];
            meet.fromLocal(born, conds, source);
            dead = dead or destroyed.isSet(source);
        } else if (join_inputs.row(local).len != 0) {
            for (join_inputs.row(local)) |edge_index| {
                const source = join_incoming[edge_index].source;
                meet.fromLocal(born, conds, source);
                dead = dead or destroyed.isSet(source);
            }
        } else if (read_inputs.row(local).len != 0) {
            for (read_inputs.row(local)) |edge_index| {
                const read = field_edges.reads[edge_index];
                meet.fromSlot(masks, field_conds, read.container, read.field);
                dead = dead or (dead_masks[read.container] & (@as(u64, 1) << @as(u6, @intCast(read.field)))) != 0;
            }
        } else {
            for (call_inputs.row(local)) |edge_index| {
                const edge = field_edges.calls[edge_index];
                if (edge.field != arc_sig.RetCondition.whole_value) continue;
                for (field_edges.call_args[edge.args_start..][0..edge.args_len]) |arg| {
                    meet.fromLocal(born, conds, arg);
                    dead = dead or destroyed.isSet(arg);
                }
            }
        }
        const stays_born = meet.isBorn() and !foreign.isSet(local) and (!multi_def.isSet(local) or multi_ok.isSet(local));
        var changed = false;
        if (born.isSet(local) != stays_born) {
            born.setValue(local, stays_born);
            changed = true;
        }
        if (stays_born and conds[local] != meet.value.?) {
            conds[local] = meet.value.?;
            changed = true;
        }
        if (dead and !destroyed.isSet(local)) {
            destroyed.set(local);
            changed = true;
        }
        if (!changed) continue;
        for (alias_edges[alias_offsets[local]..alias_offsets[local + 1]]) |target| {
            if (!queued.isSet(target)) {
                queued.set(target);
                try work.append(allocator, target);
            }
        }
        for (join_rows.row(local)) |edge_index| {
            const target = join_incoming[edge_index].target;
            if (!queued.isSet(target)) {
                queued.set(target);
                try work.append(allocator, target);
            }
        }
        for (store_rows.row(local)) |edge_index| {
            const container = field_edges.stores[edge_index].container;
            if (!mask_queued.isSet(container)) {
                mask_queued.set(container);
                try mask_work.append(allocator, container);
            }
        }
        for (call_arg_rows.row(local)) |slot| {
            const edge = field_edges.calls[call_arg_edges[slot]];
            if (edge.field == arc_sig.RetCondition.whole_value) {
                if (!queued.isSet(edge.target)) {
                    queued.set(edge.target);
                    try work.append(allocator, edge.target);
                }
            } else if (!mask_queued.isSet(edge.target)) {
                mask_queued.set(edge.target);
                try mask_work.append(allocator, edge.target);
            }
        }
    }
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
/// 1). A multi-bound alias target never inherits. Parameters are born on
/// the condition that their position is seeded, and the condition travels
/// with every transfer, so one analysis answers for every emission of a
/// proc: emission and the certifier test a local's condition against the
/// `RcSig.unique_params` of the variant at hand. Only reachable statements contribute;
/// the solver consumes its shared per-proc lift, while final-LIR
/// certification analyzes one emitted proc at a time because base and
/// specialized bodies deliberately share every source LocalId.
pub fn computeUniqueness(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    layouts: *const layout_mod.Store,
) SolveError!Uniqueness {
    return computeUniquenessDetailed(allocator, store, rc_local, sigs, null, null, null, null, true, layouts, .none, null, null, null);
}

const ProcUniquenessDomain = struct {
    local_to_dense: []const u32,
    count: usize,
    locals: ?[]const u32 = null,
    procs: ?[]const u32 = null,
    owners: ?[]const u32 = null,
    component: u32 = 0,

    fn indexOf(self: ProcUniquenessDomain, local: LIR.LocalId) ?u32 {
        const raw = @intFromEnum(local);
        if (raw >= self.local_to_dense.len) return null;
        const dense = self.local_to_dense[raw];
        if (dense != no_local) if (self.owners) |owners| {
            std.debug.assert(owners[raw] == self.component);
        };
        return if (dense == no_local) null else dense;
    }
};

/// Re-derives uniqueness from exactly one emitted procedure's explicit
/// statement and reference-counted-local inventories. ARC variants deliberately
/// share source LocalIds, so sibling definitions cannot affect this proof. The
/// dense proc domain and the caller's reusable `order_scratch` keep each proc's
/// work proportional to its own body, with no store-wide allocation or clearing.
pub fn computeProcUniqueness(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    proc: LIR.LirProcSpecId,
    stmts: []const LIR.CFStmtId,
    local_to_dense: []const u32,
    dense_local_count: usize,
    layouts: *const layout_mod.Store,
    order_scratch: *UseOrderScratch,
) SolveError!Uniqueness {
    return computeUniquenessDetailed(
        allocator,
        store,
        rc_local,
        sigs,
        null,
        proc,
        stmts,
        .{ .local_to_dense = local_to_dense, .count = dense_local_count },
        true,
        layouts,
        .stamped,
        null,
        null,
        order_scratch,
    );
}

/// Store-indexed tables that `computeProcUniqueness` reuses across procedures.
pub const UseOrderScratch = UseOrder.Scratch;

/// Control flow and ordered-use topology depend on the body, not on the
/// signatures being settled. Keep them outside the signature fixed point.
/// Body changes require a new workspace; signature and take changes require
/// a fresh lattice analysis, not a fresh topology.
const UniquenessWorkspace = struct {
    allocator: Allocator,
    stmts: []LIR.CFStmtId,
    order: UseOrder,
    use_answers: std.AutoHashMap(UseOrder.UseQuery, bool),
    consumption: ConsumptionProof = .{},
    scratch: std.heap.ArenaAllocator,
    iterations: usize = 0,

    fn init(allocator: Allocator, store: *const LirStore, proc_stmts: []const std.ArrayList(LIR.CFStmtId)) SolveError!UniquenessWorkspace {
        const lists = try allocator.alloc([]const LIR.CFStmtId, proc_stmts.len);
        defer allocator.free(lists);
        var reachable = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.cfStmtCount());
        defer reachable.deinit(allocator);
        for (proc_stmts, lists) |proc, *list| {
            list.* = proc.items;
            for (proc.items) |stmt| reachable.set(@intFromEnum(stmt));
        }
        const stmts = try allocator.alloc(LIR.CFStmtId, reachable.count());
        errdefer allocator.free(stmts);
        var iter = reachable.iterator(.{});
        var index: usize = 0;
        while (iter.next()) |raw| : (index += 1) stmts[index] = @enumFromInt(@as(u32, @intCast(raw)));
        return .{
            .allocator = allocator,
            .stmts = stmts,
            .order = try UseOrder.init(allocator, store, lists),
            .use_answers = std.AutoHashMap(UseOrder.UseQuery, bool).init(allocator),
            .scratch = std.heap.ArenaAllocator.init(allocator),
        };
    }

    fn deinit(self: *UniquenessWorkspace) void {
        self.scratch.deinit();
        self.use_answers.deinit();
        self.consumption.deinit(self.allocator);
        self.order.deinit();
        self.allocator.free(self.stmts);
    }

    fn analyze(
        self: *UniquenessWorkspace,
        store: *const LirStore,
        rc_local: []const bool,
        sigs: arc_sig.SigTable,
        consume_dead_boxes: bool,
        layouts: *const layout_mod.Store,
        takes: TakeSource,
        borrowed: *const std.bit_set.DynamicBitSetUnmanaged,
    ) SolveError!Uniqueness {
        // No result from the previous round survives into this call.
        _ = self.scratch.reset(.retain_capacity);
        if (builtin.is_test) {
            self.iterations += 1;
            uniqueness_analysis_rounds += 1;
        }
        self.order.use_answers = &self.use_answers;
        return computeUniquenessDetailed(self.scratch.allocator(), store, rc_local, sigs, null, null, self.stmts, null, consume_dead_boxes, layouts, takes, borrowed, self, null);
    }
};

/// Settles uniqueness for the whole store against the solved signatures and
/// the given take source, iterating with the unique-return bits and the
/// conditional-return rows of every signature until they stop changing.
///
/// A proc's return is unique when every returned local is born unique and
/// never given another holder; the union of the returned locals' conditions
/// is the row's parameters, or, when empty, the unconditional bit. Returned
/// aggregates get the same per field. Rows only ever gain parts or lose
/// parameters between iterations, since a new row adds edges and adds no
/// contradiction, so the loop terminates. The solver settles without takes
/// and emission settles again against the committed takes; both leave the
/// verdict, the per-local conditions, and the per-proc seed masks in the
/// solution.
pub fn settleUniqueness(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    solution: *Solution,
    takes: TakeSource,
    consume_dead_boxes: bool,
) SolveError!void {
    return settleUniquenessWithOptions(allocator, store, layouts, rc_local, solution, takes, consume_dead_boxes, .{});
}

/// Ownership components, unlike call SCCs, contain every procedure which can
/// affect the same base-analysis local or statement. Calls only invalidate.
/// Store-shaped inputs of a uniqueness settlement: every procedure's
/// statement list, the partition of procedures into components that share a
/// local or a statement, dense per-component numbering of locals and
/// statements, and the whole-store use-order topology. None of it depends on
/// the takes or the signatures a settlement starts from, so one solution
/// builds it once and every settlement on that solution reuses it.
const UniquenessStructure = struct {
    memory: std.heap.ArenaAllocator,
    store: *const LirStore,
    rc_local: []const bool,
    stmt_count: usize,
    local_count: usize,
    proc_count: usize,
    proc_stmts: []std.ArrayList(LIR.CFStmtId),
    returns: []std.ArrayList(LIR.LocalId),
    callers: []std.ArrayList(u32),
    local_owner: []u32,
    stmt_owner: []u32,
    proc_component: []u32,
    components: std.ArrayList(UniquenessComponent),
    local_to_dense: []u32,
    stmt_to_dense: []u32,
    topology: UseOrder.Topology,

    fn build(allocator: Allocator, store: *const LirStore, rc_local: []const bool) SolveError!*UniquenessStructure {
        const self = try allocator.create(UniquenessStructure);
        errdefer allocator.destroy(self);
        self.* = .{
            .memory = std.heap.ArenaAllocator.init(allocator),
            .store = store,
            .rc_local = &.{},
            .stmt_count = store.cfStmtCount(),
            .local_count = store.localCount(),
            .proc_count = store.procSpecCount(),
            .proc_stmts = &.{},
            .returns = &.{},
            .callers = &.{},
            .local_owner = &.{},
            .stmt_owner = &.{},
            .proc_component = &.{},
            .components = .empty,
            .local_to_dense = &.{},
            .stmt_to_dense = &.{},
            .topology = undefined,
        };
        errdefer self.memory.deinit();
        const arena = self.memory.allocator();
        // The caller's table may be freed before the next settlement; the
        // structure keeps its own copy for the reuse check.
        self.rc_local = try arena.dupe(bool, rc_local);
        const proc_count = store.procSpecCount();
        const proc_stmts = try arena.alloc(std.ArrayList(LIR.CFStmtId), proc_count);
        @memset(proc_stmts, .empty);
        const returns = try arena.alloc(std.ArrayList(LIR.LocalId), proc_count);
        @memset(returns, .empty);
        const callers = try arena.alloc(std.ArrayList(u32), proc_count);
        @memset(callers, .empty);
        const lists = try arena.alloc([]const LIR.CFStmtId, proc_count);
        const parents = try arena.alloc(u32, proc_count);
        for (parents, 0..) |*parent, index| parent.* = @intCast(index);
        const local_owner = try arena.alloc(u32, store.localCount());
        @memset(local_owner, no_local);
        const stmt_owner = try arena.alloc(u32, store.cfStmtCount());
        @memset(stmt_owner, no_local);
        var partition = UniquenessPartition{ .parents = parents, .local_owner = local_owner, .rc_local = rc_local };
        for (0..proc_count) |proc_index| {
            partition.proc = @intCast(proc_index);
            const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
            const args = store.getLocalSpan(proc.args);
            for (0..GuardedList.borrowLen(args)) |position| partition.local(GuardedList.at(args, position));
            if (proc.body) |body| try collectProcStatements(arena, store, body, &proc_stmts[proc_index]);
            lists[proc_index] = proc_stmts[proc_index].items;
            for (proc_stmts[proc_index].items) |stmt_id| {
                const raw = @intFromEnum(stmt_id);
                if (stmt_owner[raw] == no_local) stmt_owner[raw] = @intCast(proc_index) else partition.unite(@intCast(proc_index), stmt_owner[raw]);
                const stmt = store.getCFStmt(stmt_id);
                body_clone.forEachStmtRead(store, stmt, &partition, UniquenessPartition.local);
                body_clone.forEachStmtDef(store, stmt, &partition, UniquenessPartition.local);
                if (stmt == .ret) try returns[proc_index].append(arena, stmt.ret.value);
                if (stmt == .assign_call) try callers[@intFromEnum(stmt.assign_call.proc)].append(arena, @intCast(proc_index));
            }
        }
        const proc_component = try arena.alloc(u32, proc_count);
        const root_component = try arena.alloc(u32, proc_count);
        @memset(root_component, no_local);
        var components = std.ArrayList(UniquenessComponent).empty;
        for (0..proc_count) |index| {
            const root = partition.root(@intCast(index));
            if (root_component[root] == no_local) {
                root_component[root] = @intCast(components.items.len);
                try components.append(arena, .{});
            }
            const component = root_component[root];
            proc_component[index] = component;
            try components.items[component].procs.append(arena, @intCast(index));
        }
        const local_to_dense = try arena.alloc(u32, store.localCount());
        @memset(local_to_dense, no_local);
        for (local_owner, 0..) |*owner, raw| {
            if (owner.* == no_local) continue;
            owner.* = proc_component[owner.*];
            const component = &components.items[owner.*];
            local_to_dense[raw] = @intCast(component.locals.items.len);
            try component.locals.append(arena, @intCast(raw));
        }
        const topology = try UseOrder.initTopology(arena, store, lists, .{ .owned = .without_marks });
        const stmt_to_dense = try arena.alloc(u32, store.cfStmtCount());
        @memset(stmt_to_dense, no_local);
        // Ascending statement ids reproduce the legacy whole-store deduplication.
        for (stmt_owner, 0..) |*owner, raw| {
            if (owner.* == no_local) continue;
            owner.* = proc_component[owner.*];
            const component = &components.items[owner.*];
            stmt_to_dense[raw] = @intCast(component.stmts.items.len);
            try component.stmts.append(arena, @enumFromInt(@as(u32, @intCast(raw))));
            if (topology.unresolved.isSet(raw)) try component.unresolved.append(arena, @intCast(raw));
        }
        self.proc_stmts = proc_stmts;
        self.returns = returns;
        self.callers = callers;
        self.local_owner = local_owner;
        self.stmt_owner = stmt_owner;
        self.proc_component = proc_component;
        self.components = components;
        self.local_to_dense = local_to_dense;
        self.stmt_to_dense = stmt_to_dense;
        self.topology = topology;
        return self;
    }

    fn destroy(self: *UniquenessStructure) void {
        for (self.components.items) |component| if (component.result) |result| result.deinit();
        const allocator = self.memory.child_allocator;
        self.memory.deinit();
        allocator.destroy(self);
    }
};

const UniquenessComponent = struct {
    procs: std.ArrayList(u32) = .empty,
    locals: std.ArrayList(u32) = .empty,
    stmts: std.ArrayList(LIR.CFStmtId) = .empty,
    unresolved: std.ArrayList(u32) = .empty,
    result: ?*UniquenessComponentResult = null,
};

const UniquenessProcUpdate = struct {
    sig: arc_sig.RcSig,
    rows: []const arc_sig.RetCondition,
    seeds: arc_sig.ParamMask,
};

/// A result's arena belongs to the allocator of the executing worker, never
/// its scratch allocator. Clean components retain these exact verdicts.
const UniquenessComponentResult = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    workspace: UniquenessWorkspace,
    uniqueness: Uniqueness,
    updates: []UniquenessProcUpdate,

    fn deinit(self: *@This()) void {
        const allocator = self.allocator;
        self.workspace.scratch.deinit();
        self.arena.deinit();
        allocator.destroy(self);
    }
};

const UniquenessComponentTask = struct {
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    solution: *const Solution,
    takes: TakeSource,
    consume_dead_boxes: bool,
    component: *UniquenessComponent,
    domain: ProcUniquenessDomain,
    topology: *const UseOrder.Topology,
    stmt_to_dense: []const u32,
    stmt_owner: []const u32,
    proc_stmts: []const std.ArrayList(LIR.CFStmtId),
    returns: []const std.ArrayList(LIR.LocalId),
    failed: bool = false,
    completed: bool = false,
    analysis_started: bool = false,

    fn run(context: *anyopaque, worker: task_executor.Worker) ?*anyopaque {
        const self: *@This() = @ptrCast(@alignCast(context));
        self.execute(worker.allocator) catch {
            self.failed = true;
        };
        return self;
    }

    fn countWork(self: *const @This(), metrics: *UniquenessMetrics) void {
        if (!self.analysis_started) return;
        metrics.component_runs += 1;
        metrics.statement_visits += @intCast(self.component.stmts.items.len);
        metrics.local_visits += @intCast(self.component.locals.items.len);
    }

    fn execute(self: *@This(), allocator: Allocator) SolveError!void {
        const result = self.component.result orelse create: {
            const owner = try allocator.create(UniquenessComponentResult);
            errdefer allocator.destroy(owner);
            owner.* = .{
                .allocator = allocator,
                .arena = std.heap.ArenaAllocator.init(allocator),
                .workspace = undefined,
                .uniqueness = undefined,
                .updates = undefined,
            };
            errdefer owner.arena.deinit();
            const persistent = owner.arena.allocator();
            const order = UseOrder{
                .allocator = persistent,
                .topology = self.topology.*,
                .component = .{
                    .stmt_to_dense = self.stmt_to_dense,
                    .stmt_owner = self.stmt_owner,
                    .id = self.domain.component,
                    .unresolved = self.component.unresolved.items,
                },
                .mark_gen = try persistent.alloc(u32, self.component.stmts.items.len),
                .generation = 0,
                .marked_local = no_local,
                .marked_uses = false,
                .work = .empty,
            };
            @memset(order.mark_gen, 0);
            owner.workspace = .{
                .allocator = persistent,
                .stmts = self.component.stmts.items,
                .order = order,
                .use_answers = std.AutoHashMap(UseOrder.UseQuery, bool).init(persistent),
                .scratch = std.heap.ArenaAllocator.init(allocator),
            };
            owner.workspace.order.use_answers = &owner.workspace.use_answers;
            self.component.result = owner;
            break :create owner;
        };
        // Cached topology queries survive; no prior lattice fact survives.
        _ = result.workspace.scratch.reset(.retain_capacity);
        const arena = result.workspace.scratch.allocator();
        self.analysis_started = true;
        result.uniqueness = try computeUniquenessDetailed(
            arena,
            self.store,
            self.rc_local,
            self.solution.sigTable(),
            null,
            null,
            self.component.stmts.items,
            self.domain,
            self.consume_dead_boxes,
            self.layouts,
            self.takes,
            &self.solution.borrowed,
            &result.workspace,
            null,
        );
        result.updates = try arena.alloc(UniquenessProcUpdate, self.component.procs.items.len);
        for (self.component.procs.items, result.updates) |proc, *update| {
            update.* = try self.derive(arena, proc, result.uniqueness);
        }
    }

    /// This is the base signature derivation, not the emitted-proc certifier.
    /// Unconditional capabilities accumulate; conditional rows are recomputed.
    fn derive(self: *const @This(), allocator: Allocator, proc_index: u32, uniqueness: Uniqueness) SolveError!UniquenessProcUpdate {
        var sig = self.solution.sigs[proc_index];
        var rows = std.ArrayList(arc_sig.RetCondition).empty;
        const pinned = self.solution.pinned.isSet(proc_index);
        if (!pinned) {
            const proc = self.store.getProcSpec(@enumFromInt(proc_index));
            const params = self.store.getLocalSpan(proc.args);
            var read_only: arc_sig.ParamMask = 0;
            for (0..@min(GuardedList.borrowLen(params), arc_sig.tracked_param_count)) |position| {
                if (sig.paramMode(position) != .borrowed) continue;
                const dense = self.domain.indexOf(GuardedList.at(params, position)) orelse continue;
                if (uniqueness.destroyed.isSet(dense) or uniqueness.consumed.isSet(dense)) continue;
                read_only |= arc_sig.paramBit(position).?;
            }
            sig.read_only_params = read_only;
        }
        const returns = self.returns[proc_index].items;
        if (!pinned and returns.len != 0) {
            var all_born = true;
            var whole_params: arc_sig.ParamMask = 0;
            var fields: u64 = std.math.maxInt(u64);
            var field_params: [64]arc_sig.ParamMask = @splat(0);
            for (returns) |local| {
                const dense = self.domain.indexOf(local) orelse {
                    all_born = false;
                    fields = 0;
                    break;
                };
                if (!uniqueness.born_unique.isSet(dense) or uniqueness.destroyed.isSet(dense)) {
                    all_born = false;
                } else {
                    whole_params |= uniqueness.conds[dense];
                }
                fields &= uniqueness.field_masks[dense];
                var born_fields = std.bit_set.IntegerBitSet(64){ .mask = uniqueness.field_masks[dense] };
                var iter = born_fields.iterator(.{});
                while (iter.next()) |field| field_params[field] |= uniqueness.field_conds.get(dense, @intCast(field));
            }
            if (all_born) {
                if (whole_params == 0) sig.ret_unique = true else try rows.append(allocator, .{
                    .field = arc_sig.RetCondition.whole_value,
                    .params = whole_params,
                });
            }
            var returned_fields = std.bit_set.IntegerBitSet(64){ .mask = fields };
            var iter = returned_fields.iterator(.{});
            while (iter.next()) |field| {
                if (field_params[field] == 0) {
                    sig.ret_unique_fields |= @as(u64, 1) << @as(u6, @intCast(field));
                } else {
                    try rows.append(allocator, .{ .field = @intCast(field), .params = field_params[field] });
                }
            }
        }
        var seeds: arc_sig.ParamMask = 0;
        for (self.proc_stmts[proc_index].items) |stmt| {
            const node = self.store.getCFStmt(stmt);
            if (node != .assign_low_level) continue;
            const assign = node.assign_low_level;
            const effect = if (!self.consume_dead_boxes and assign.op == .box_unbox)
                assign.op.arcBorrowedResultVariant().?.rcEffect()
            else
                assign.op.arcInferenceRcEffect(assign.rc_effect);
            const args = self.store.getLocalSpan(assign.args);
            for (0..@min(GuardedList.borrowLen(args), 64)) |position| {
                if ((effect.may_runtime_uniqueness_check_args & (@as(u64, 1) << @as(u6, @intCast(position)))) == 0) continue;
                const dense = self.domain.indexOf(GuardedList.at(args, position)) orelse continue;
                if (!uniqueness.born_unique.isSet(dense) or uniqueness.destroyed.isSet(dense)) continue;
                seeds |= uniqueness.conds[dense];
            }
        }
        return .{ .sig = sig, .rows = rows.items, .seeds = seeds };
    }
};

const UniquenessPartition = struct {
    parents: []u32,
    local_owner: []u32,
    rc_local: []const bool,
    proc: u32 = 0,

    fn root(self: *@This(), node: u32) u32 {
        var current = node;
        while (self.parents[current] != current) {
            self.parents[current] = self.parents[self.parents[current]];
            current = self.parents[current];
        }
        return current;
    }

    fn unite(self: *@This(), left: u32, right: u32) void {
        const a = self.root(left);
        const b = self.root(right);
        self.parents[@max(a, b)] = @min(a, b);
    }

    fn local(self: *@This(), id: LIR.LocalId) void {
        const raw = @intFromEnum(id);
        if (raw >= self.rc_local.len or !self.rc_local[raw]) return;
        if (self.local_owner[raw] == no_local) self.local_owner[raw] = self.proc else self.unite(self.proc, self.local_owner[raw]);
    }
};

fn sameRetConditions(a: []const arc_sig.RetCondition, b: []const arc_sig.RetCondition) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| if (left.field != right.field or left.params != right.params) return false;
    return true;
}

fn runUniquenessTasks(
    contexts: []UniquenessComponentTask,
    executor: *const task_executor.Executor,
    metrics: *UniquenessMetrics,
) SolveError!void {
    var session = executor.begin();
    var submitted: usize = 0;
    var received: usize = 0;
    var failed = false;
    while (received < submitted or (!failed and submitted < contexts.len)) {
        while (!failed and submitted < contexts.len and session.canSubmit()) {
            session.submit(.{ .id = submitted, .context = &contexts[submitted], .run = UniquenessComponentTask.run }) catch {
                failed = true;
                break;
            };
            submitted += 1;
            metrics.task_submitted += 1;
        }
        if (received < submitted) {
            const completion = session.receive();
            if (completion.id >= submitted or completion.worker_id >= executor.worker_count) solveInvariant("uniqueness received invalid task identity");
            const context = &contexts[completion.id];
            if (context.completed or completion.value != @as(*anyopaque, @ptrCast(context))) solveInvariant("uniqueness received invalid task completion");
            context.completed = true;
            context.countWork(metrics);
            received += 1;
            if (context.failed) failed = true;
        }
    }
    session.end();
    if (failed) return error.OutOfMemory;
}

/// Exact ownership components run against frozen signatures in deterministic
/// waves. Only callers of semantically changed signatures enter the next wave.
/// Every invocation starts all components afresh, including post-take
/// settlement; only the store-shaped structure is retained between them.
pub fn settleUniquenessWithOptions(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    solution: *Solution,
    takes: TakeSource,
    consume_dead_boxes: bool,
    options: UniquenessOptions,
) SolveError!void {
    var memory = std.heap.ArenaAllocator.init(allocator);
    defer memory.deinit();
    const arena = memory.allocator();
    const proc_count = store.procSpecCount();
    const structure = solution.uniqueness_structure orelse blk: {
        const built = try UniquenessStructure.build(allocator, store, rc_local);
        solution.uniqueness_structure = built;
        break :blk built;
    };
    if (structure.store != store or structure.stmt_count != store.cfStmtCount() or
        structure.local_count != store.localCount() or structure.proc_count != proc_count or
        !sameResourceTable(structure.rc_local, rc_local))
    {
        solveInvariant("uniqueness settlement reused a structure built for a different store");
    }
    const callers = structure.callers;
    const local_owner = structure.local_owner;
    const stmt_owner = structure.stmt_owner;
    const proc_component = structure.proc_component;
    const components = &structure.components;
    const local_to_dense = structure.local_to_dense;
    const topology = &structure.topology;
    const stmt_to_dense = structure.stmt_to_dense;
    const proc_stmts = structure.proc_stmts;
    const returns = structure.returns;
    defer for (components.items) |*component| if (component.result) |result| {
        result.deinit();
        component.result = null;
    };
    const contexts = try arena.alloc(UniquenessComponentTask, components.items.len);
    var dirty = try std.bit_set.DynamicBitSetUnmanaged.initFull(arena, components.items.len);
    var local_metrics: UniquenessMetrics = .{};
    const metrics = options.metrics orelse &local_metrics;
    metrics.settlements += 1;
    metrics.components += @intCast(components.items.len);
    const proc_rows = try arena.alloc([]const arc_sig.RetCondition, proc_count);
    for (solution.sigs, proc_rows) |sig, *row| row.* = solution.sigTable().retConditionsOf(sig);
    var flat_rows = std.ArrayList(arc_sig.RetCondition).empty;
    defer flat_rows.deinit(allocator);
    while (dirty.count() != 0) {
        metrics.signature_waves += 1;
        if (builtin.is_test) uniqueness_analysis_rounds += 1;
        var count: usize = 0;
        var iter = dirty.iterator(.{});
        while (iter.next()) |id| {
            const component = &components.items[id];
            // Rows in proc_rows are replaced below before publishing; workers
            // read the frozen global row table, never another owner's scratch.
            contexts[count] = .{
                .store = store,
                .layouts = layouts,
                .rc_local = rc_local,
                .solution = solution,
                .takes = takes,
                .consume_dead_boxes = consume_dead_boxes,
                .component = component,
                .topology = topology,
                .domain = .{
                    .local_to_dense = local_to_dense,
                    .count = component.locals.items.len,
                    .locals = component.locals.items,
                    .procs = component.procs.items,
                    .owners = local_owner,
                    .component = @intCast(id),
                },
                .stmt_to_dense = stmt_to_dense,
                .stmt_owner = stmt_owner,
                .proc_stmts = proc_stmts,
                .returns = returns,
            };
            count += 1;
        }
        dirty.setRangeValue(.{ .start = 0, .end = components.items.len }, false);
        if (options.executor) |executor| {
            try runUniquenessTasks(contexts[0..count], executor, metrics);
        } else {
            for (contexts[0..count]) |*context| {
                defer context.countWork(metrics);
                try context.execute(allocator);
            }
        }
        for (contexts[0..count]) |context| if (context.failed) return error.OutOfMemory;
        for (contexts[0..count]) |context| {
            const result = context.component.result.?;
            if (options.executor != null) metrics.task_committed += 1;
            for (context.component.procs.items, result.updates) |proc, update| {
                const old = solution.sigs[proc];
                const changed = old.read_only_params != update.sig.read_only_params or
                    old.ret_unique != update.sig.ret_unique or old.ret_unique_fields != update.sig.ret_unique_fields or
                    !sameRetConditions(solution.sigTable().retConditionsOf(old), update.rows);
                solution.sigs[proc].read_only_params = update.sig.read_only_params;
                solution.sigs[proc].ret_unique = update.sig.ret_unique;
                solution.sigs[proc].ret_unique_fields = update.sig.ret_unique_fields;
                solution.unique_seed_masks[proc] = update.seeds;
                proc_rows[proc] = update.rows;
                if (changed) {
                    metrics.signature_changes += 1;
                    for (callers[proc].items) |caller| dirty.set(proc_component[caller]);
                }
            }
        }
        flat_rows.clearRetainingCapacity();
        for (proc_rows, solution.sigs) |row, *sig| {
            sig.ret_conditions = .{ .start = @intCast(flat_rows.items.len), .len = @intCast(row.len) };
            try flat_rows.appendSlice(allocator, row);
        }
        const published = try allocator.dupe(arc_sig.RetCondition, flat_rows.items);
        allocator.free(solution.ret_conditions);
        solution.ret_conditions = published;
    }
    solution.unique.setRangeValue(.{ .start = 0, .end = solution.unique.bit_length }, false);
    solution.unique_destroyed.setRangeValue(.{ .start = 0, .end = solution.unique_destroyed.bit_length }, false);
    solution.unique_born.setRangeValue(.{ .start = 0, .end = solution.unique_born.bit_length }, false);
    @memset(solution.unique_conds, 0);
    for (components.items) |component| {
        const verdict = component.result.?.uniqueness;
        for (component.locals.items, 0..) |raw, dense| {
            solution.unique.setValue(raw, verdict.unique.isSet(dense));
            solution.unique_destroyed.setValue(raw, verdict.destroyed.isSet(dense));
            solution.unique_born.setValue(raw, verdict.born_unique.isSet(dense));
            solution.unique_conds[raw] = verdict.conds[dense];
        }
    }
}

/// Whether two per-local refcount tables describe the same locals.
fn sameResourceTable(retained: []const bool, requested: []const bool) bool {
    if (retained.len != requested.len) return false;
    for (retained, requested) |left, right| if (left != right) return false;
    return true;
}

/// Whole-store reference implementation, deliberately unavailable to production.
fn settleUniquenessOracle(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    solution: *Solution,
    takes: TakeSource,
    consume_dead_boxes: bool,
) SolveError!void {
    comptime std.debug.assert(builtin.is_test);
    const proc_count = store.procSpecCount();
    var proc_stmts = try allocator.alloc(std.ArrayList(LIR.CFStmtId), proc_count);
    defer allocator.free(proc_stmts);
    @memset(proc_stmts, .empty);
    defer for (proc_stmts) |*list| list.deinit(allocator);
    // The locals each proc returns, for the signature bits.
    var returns = try allocator.alloc(std.ArrayList(LIR.LocalId), proc_count);
    defer allocator.free(returns);
    @memset(returns, .empty);
    defer for (returns) |*list| list.deinit(allocator);
    for (0..proc_count) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const body = proc.body orelse continue;
        try collectProcStatements(allocator, store, body, &proc_stmts[proc_index]);
        for (proc_stmts[proc_index].items) |stmt| {
            const node = store.getCFStmt(stmt);
            if (node == .ret) try returns[proc_index].append(allocator, node.ret.value);
        }
    }

    var workspace = try UniquenessWorkspace.init(allocator, store, proc_stmts);
    defer workspace.deinit();
    var rows = std.ArrayList(arc_sig.RetCondition).empty;
    defer rows.deinit(allocator);
    while (true) {
        const uniqueness = try workspace.analyze(store, rc_local, solution.sigTable(), consume_dead_boxes, layouts, takes, &solution.borrowed);
        var changed = false;
        rows.clearRetainingCapacity();
        for (returns, 0..) |list, proc_index| {
            const sig = &solution.sigs[proc_index];
            const old_rows = solution.sigTable().retConditionsOf(sig.*);
            const row_start = rows.items.len;
            // Borrowed positions the body only reads add no holder to the
            // caller's argument.
            if (!solution.pinned.isSet(proc_index)) {
                const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
                const params = store.getLocalSpan(proc.args);
                var read_only: arc_sig.ParamMask = 0;
                for (0..@min(GuardedList.borrowLen(params), arc_sig.tracked_param_count)) |position| {
                    if (sig.paramMode(position) != .borrowed) continue;
                    const raw = @intFromEnum(GuardedList.at(params, position));
                    if (raw >= rc_local.len or !rc_local[raw]) continue;
                    if (uniqueness.destroyed.isSet(raw) or uniqueness.consumed.isSet(raw)) continue;
                    read_only |= arc_sig.paramBit(position).?;
                }
                if (read_only != sig.read_only_params) {
                    sig.read_only_params = read_only;
                    changed = true;
                }
            }
            if (!solution.pinned.isSet(proc_index) and list.items.len != 0) {
                var all_born = true;
                var whole_params: arc_sig.ParamMask = 0;
                var fields: u64 = std.math.maxInt(u64);
                var field_params: [64]arc_sig.ParamMask = @splat(0);
                for (list.items) |local| {
                    const raw = @intFromEnum(local);
                    if (raw >= rc_local.len or !rc_local[raw]) {
                        all_born = false;
                        fields = 0;
                        break;
                    }
                    if (!uniqueness.born_unique.isSet(raw) or uniqueness.destroyed.isSet(raw)) {
                        all_born = false;
                    } else {
                        whole_params |= uniqueness.conds[raw];
                    }
                    fields &= uniqueness.field_masks[raw];
                    var born_fields = std.bit_set.IntegerBitSet(64){ .mask = uniqueness.field_masks[raw] };
                    var born_field_iter = born_fields.iterator(.{});
                    while (born_field_iter.next()) |field| field_params[field] |= uniqueness.field_conds.get(@intCast(raw), @intCast(field));
                }
                if (all_born) {
                    if (whole_params == 0) {
                        if (!sig.ret_unique) {
                            sig.ret_unique = true;
                            changed = true;
                        }
                    } else {
                        try rows.append(allocator, .{ .field = arc_sig.RetCondition.whole_value, .params = whole_params });
                    }
                }
                var returned_fields = std.bit_set.IntegerBitSet(64){ .mask = fields };
                var returned_field_iter = returned_fields.iterator(.{});
                while (returned_field_iter.next()) |field| {
                    const bit = @as(u64, 1) << @as(u6, @intCast(field));
                    if (field_params[field] == 0) {
                        if ((sig.ret_unique_fields & bit) == 0) {
                            sig.ret_unique_fields |= bit;
                            changed = true;
                        }
                    } else {
                        try rows.append(allocator, .{ .field = @intCast(field), .params = field_params[field] });
                    }
                }
            }
            const new_rows = rows.items[row_start..];
            if (new_rows.len != old_rows.len) {
                changed = true;
            } else {
                for (new_rows, old_rows) |new_row, old_row| {
                    if (new_row.field != old_row.field or new_row.params != old_row.params) changed = true;
                }
            }
            sig.ret_conditions = .{ .start = @intCast(row_start), .len = @intCast(new_rows.len) };
        }
        const table = try allocator.dupe(arc_sig.RetCondition, rows.items);
        allocator.free(solution.ret_conditions);
        solution.ret_conditions = table;
        if (changed) continue;

        // Parameter positions whose seed would let a runtime check the
        // body performs on a value carried from them go check-free.
        @memset(solution.unique_seed_masks, 0);
        for (proc_stmts, 0..) |stmts, proc_index| {
            for (stmts.items) |stmt| {
                const node = store.getCFStmt(stmt);
                if (node != .assign_low_level) continue;
                const assign = node.assign_low_level;
                const rc_effect = if (!consume_dead_boxes and assign.op == .box_unbox)
                    assign.op.arcBorrowedResultVariant().?.rcEffect()
                else
                    assign.op.arcInferenceRcEffect(assign.rc_effect);
                const check_mask = rc_effect.may_runtime_uniqueness_check_args;
                if (check_mask == 0) continue;
                const args = store.getLocalSpan(assign.args);
                for (0..@min(GuardedList.borrowLen(args), 64)) |position| {
                    if ((check_mask & (@as(u64, 1) << @as(u6, @intCast(position)))) == 0) continue;
                    const raw = @intFromEnum(GuardedList.at(args, position));
                    if (raw >= rc_local.len or !rc_local[raw]) continue;
                    if (!uniqueness.born_unique.isSet(raw) or uniqueness.destroyed.isSet(raw)) continue;
                    solution.unique_seed_masks[proc_index] |= uniqueness.conds[raw];
                }
            }
        }

        var unique = try uniqueness.unique.clone(allocator);
        errdefer unique.deinit(allocator);
        var destroyed = try uniqueness.destroyed.clone(allocator);
        errdefer destroyed.deinit(allocator);
        var born = try uniqueness.born_unique.clone(allocator);
        errdefer born.deinit(allocator);
        const conds = try allocator.dupe(arc_sig.ParamMask, uniqueness.conds);
        solution.unique.deinit(allocator);
        solution.unique_destroyed.deinit(allocator);
        solution.unique_born.deinit(allocator);
        allocator.free(solution.unique_conds);
        solution.unique = unique;
        solution.unique_destroyed = destroyed;
        solution.unique_born = born;
        solution.unique_conds = conds;
        return;
    }
}

fn computeUniquenessDetailed(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    proc_stmts: ?[]const std.ArrayList(LIR.CFStmtId),
    only_proc: ?LIR.LirProcSpecId,
    exact_stmts: ?[]const LIR.CFStmtId,
    proc_domain: ?ProcUniquenessDomain,
    consume_dead_boxes: bool,
    layouts: *const layout_mod.Store,
    takes: TakeSource,
    borrowed: ?*const std.bit_set.DynamicBitSetUnmanaged,
    workspace: ?*UniquenessWorkspace,
    order_scratch: ?*UseOrder.Scratch,
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
    // Consuming uses, resolved against `UseOrder` once every use is known:
    // a consume destroys uniqueness only when another consume of the local
    // can still execute after it.
    var consumes = std.ArrayList(ConsumeAt).empty;
    defer consumes.deinit(allocator);
    var has_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer has_def.deinit(allocator);
    var multi_def = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer multi_def.deinit(allocator);
    // Definitions and unique births per local. A join result cell that
    // every arm assigns a fresh value has several definitions and as many
    // births (plus the join that declares it); whichever arm ran, the
    // value is born.
    const def_counts = try allocator.alloc(u32, local_count);
    defer allocator.free(def_counts);
    @memset(def_counts, 0);
    const birth_counts = try allocator.alloc(u32, local_count);
    defer allocator.free(birth_counts);
    @memset(birth_counts, 0);
    // Join statements declaring each local as a parameter; a result cell
    // may be the parameter of several nested joins.
    const join_decl_counts = try allocator.alloc(u32, local_count);
    defer allocator.free(join_decl_counts);
    @memset(join_decl_counts, 0);

    // Single pure-alias source per target (`no_local` when the local is not
    // an alias target), plus the list of distinct alias targets to settle.
    const alias_source = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_source);
    @memset(alias_source, no_local);
    const alias_stmt = try allocator.alloc(u32, local_count);
    defer allocator.free(alias_stmt);
    @memset(alias_stmt, no_local);
    var alias_targets = std.ArrayList(u32).empty;
    defer alias_targets.deinit(allocator);
    // A solved-borrowed alias is a view of its source rather than a holder:
    // it moves no unit, so it is not a consuming use of the source, while a
    // consuming use of the view retains the source's allocation and counts
    // as a consume of the source at that statement.
    const view_source = try allocator.alloc(u32, local_count);
    defer allocator.free(view_source);
    @memset(view_source, no_local);
    var view_defs = std.ArrayList(AliasDef).empty;
    defer view_defs.deinit(allocator);
    var join_incoming = std.ArrayList(UniqueJoinIncoming).empty;
    defer join_incoming.deinit(allocator);
    var join_incoming_stmts = std.ArrayList(u32).empty;
    defer join_incoming_stmts.deinit(allocator);
    var join_targets = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer join_targets.deinit(allocator);
    var field_stores = std.ArrayList(FieldStoreEdge).empty;
    defer field_stores.deinit(allocator);
    var mask_aliases = std.ArrayList(MaskAliasEdge).empty;
    defer mask_aliases.deinit(allocator);
    var field_reads = std.ArrayList(FieldReadEdge).empty;
    defer field_reads.deinit(allocator);
    var seed_masks = std.ArrayList(SeedMask).empty;
    defer seed_masks.deinit(allocator);
    var call_edges = std.ArrayList(CallEdge).empty;
    defer call_edges.deinit(allocator);
    var call_args = std.ArrayList(u32).empty;
    defer call_args.deinit(allocator);
    // The condition each born local's birth holds under; a parameter's
    // birth needs its own position seeded.
    const conds = try allocator.alloc(arc_sig.ParamMask, local_count);
    errdefer allocator.free(conds);
    @memset(conds, 0);
    // Locals an emitted `incref` retains: a retained field read holds a
    // unit of the field's allocation.
    var retained = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer retained.deinit(allocator);

    const Marks = struct {
        rc: []const bool,
        domain: ?ProcUniquenessDomain,
        defs: []u32,
        births: []u32,

        fn indexOf(self: @This(), local: LIR.LocalId) ?u32 {
            const index = @intFromEnum(local);
            if (index >= self.rc.len or !self.rc[index]) return null;
            if (self.domain) |domain| {
                const dense = domain.indexOf(local);
                if (domain.owners != null) std.debug.assert(dense != null);
                return dense;
            }
            return @intCast(index);
        }

        fn noteBirth(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
            const index = self.indexOf(local) orelse return;
            set.set(index);
            self.births[index] += 1;
        }

        fn destroy(self: @This(), set: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
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
            self.defs[index] += 1;
            if (seen.isSet(index)) {
                multi.set(index);
            } else {
                seen.set(index);
            }
        }

        fn consumeAt(
            self: @This(),
            alloc: Allocator,
            list: *std.ArrayList(ConsumeAt),
            local: LIR.LocalId,
            stmt: u32,
        ) SolveError!void {
            const index = self.indexOf(local) orelse return;
            try list.append(alloc, .{ .index = index, .stmt = stmt });
        }

        fn transfer(
            self: @This(),
            alloc: Allocator,
            list: *std.ArrayList(ConsumeAt),
            dead: *std.bit_set.DynamicBitSetUnmanaged,
            local: LIR.LocalId,
            mode: LIR.BoxyTransferMode,
            stmt: u32,
        ) SolveError!void {
            switch (mode) {
                .move => try self.consumeAt(alloc, list, local, stmt),
                .copy => self.destroy(dead, local),
                .borrow => {},
            }
        }
    };
    const marks = Marks{ .rc = rc_local, .domain = proc_domain, .defs = def_counts, .births = birth_counts };

    const Alias = struct {
        /// Records a pure same-value alias definition at `stmt`. The
        /// definition is the chain's consuming use of the source, judged
        /// against the source's other uses by `UseOrder` when the chain
        /// settles; a non-refcounted or self-referential source poisons the
        /// target. Which alias definitions inherit is decided once the
        /// scan knows the join result cells, since an alias assigned into
        /// a cell is one of the cell's incoming edges.
        fn record(
            m: Marks,
            alloc: Allocator,
            defs: *std.ArrayList(AliasDef),
            foreign: *std.bit_set.DynamicBitSetUnmanaged,
            consumed: *std.ArrayList(ConsumeAt),
            target: LIR.LocalId,
            source: LIR.LocalId,
            stmt: u32,
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
            try consumed.append(alloc, .{ .index = @intCast(source_index), .stmt = stmt });
            try defs.append(alloc, .{ .target = @intCast(target_index), .source = @intCast(source_index), .stmt = stmt });
        }
    };
    var alias_defs = std.ArrayList(AliasDef).empty;
    defer alias_defs.deinit(allocator);

    const component_procs = if (proc_domain) |domain| domain.procs else null;
    for (0..if (component_procs) |procs| procs.len else store.procSpecCount()) |proc_slot| {
        const proc_index = if (component_procs) |procs| procs[proc_slot] else proc_slot;
        if (only_proc) |proc_id| {
            if (proc_index != @intFromEnum(proc_id)) continue;
        }
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |param_index| {
            const param = GuardedList.at(params, param_index);
            marks.trackDef(&has_def, &multi_def, param);
            // A tracked parameter is born unique on the condition that its
            // own position is seeded: a call site that proved its dying
            // argument unique hands over that argument's single unit.
            const seed = arc_sig.paramBit(param_index) orelse {
                marks.destroy(&foreign_def, param);
                continue;
            };
            if (marks.indexOf(param)) |index| {
                born.set(index);
                conds[index] = seed;
            }
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
        switch (stmt) {
            .assign_ref => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                const alias_of: ?LIR.LocalId = switch (assign.op) {
                    .local => |source| source,
                    .list_reinterpret => |op| op.backing_ref,
                    .nominal => |op| op.backing_ref,
                    .discriminant, .field, .tag_payload, .tag_payload_struct => null,
                };
                const is_view = alias_of != null and borrowed != null and
                    borrowed.?.isSet(@intFromEnum(assign.target));
                if (is_view) {
                    marks.destroy(&foreign_def, assign.target);
                    if (marks.indexOf(assign.target)) |target| if (marks.indexOf(alias_of.?)) |source| {
                        if (source != target) {
                            view_source[target] = source;
                            try view_defs.append(allocator, .{ .target = target, .source = source, .stmt = @intCast(stmt_index) });
                        }
                    };
                }
                switch (assign.op) {
                    .local => |source| if (!is_view) try Alias.record(marks, allocator, &alias_defs, &foreign_def, &consumes, assign.target, source, @intCast(stmt_index)),
                    .list_reinterpret => |op| if (!is_view) try Alias.record(marks, allocator, &alias_defs, &foreign_def, &consumes, assign.target, op.backing_ref, @intCast(stmt_index)),
                    .nominal => |op| if (!is_view) try Alias.record(marks, allocator, &alias_defs, &foreign_def, &consumes, assign.target, op.backing_ref, @intCast(stmt_index)),
                    // A payload or discriminant read names an interior
                    // value of a possibly-shared outer one; a field read
                    // inherits only through a take, decided below.
                    .discriminant => marks.destroy(&foreign_def, assign.target),
                    .field => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        try noteFieldRead(&field_reads, marks, takes, store, allocator, assign.target, op.source, op.field_idx, @intCast(stmt_index));
                    },
                    .tag_payload => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        try noteFieldRead(&field_reads, marks, takes, store, allocator, assign.target, op.source, op.payload_idx, @intCast(stmt_index));
                    },
                    .tag_payload_struct => |op| {
                        marks.destroy(&foreign_def, assign.target);
                        if (marks.indexOf(assign.target)) |target| if (marks.indexOf(op.source)) |source| {
                            try mask_aliases.append(allocator, .{ .source = source, .target = target });
                        };
                    },
                }
                // A same-value alias of an aggregate views the same stored
                // fields.
                if (assign.op == .local) {
                    if (marks.indexOf(assign.target)) |target| if (marks.indexOf(assign.op.local)) |source| if (target != source) {
                        try mask_aliases.append(allocator, .{ .source = source, .target = target });
                    };
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
                if (assign.out_desc) |out_desc| {
                    marks.trackDef(&has_def, &multi_def, out_desc);
                    marks.destroy(&foreign_def, out_desc);
                }
                const callee_sig = sigs.get(assign.proc);
                const rows = sigs.retConditionsOf(callee_sig);
                var whole_conditional = false;
                for (rows) |row| whole_conditional = whole_conditional or row.field == arc_sig.RetCondition.whole_value;
                if (callee_sig.ret_unique) {
                    marks.noteBirth(&born, assign.target);
                } else if (!whole_conditional) {
                    marks.destroy(&foreign_def, assign.target);
                }
                if (callee_sig.ret_unique_fields != 0) {
                    if (marks.indexOf(assign.target)) |target| try seed_masks.append(allocator, .{ .local = target, .mask = callee_sig.ret_unique_fields });
                }
                const args = store.getLocalSpan(assign.args);
                // Each row is an edge from the arguments it names to the
                // part of the result it names; an argument outside the
                // refcounted domain leaves that part unborn.
                if (marks.indexOf(assign.target)) |target| for (rows) |row| {
                    const args_start: u32 = @intCast(call_args.items.len);
                    var complete = true;
                    for (0..arc_sig.tracked_param_count) |position| {
                        if ((row.params & (arc_sig.paramBit(position) orelse unreachable)) == 0) continue;
                        const arg_index = if (position < GuardedList.borrowLen(args)) marks.indexOf(GuardedList.at(args, position)) else null;
                        const index = arg_index orelse {
                            complete = false;
                            break;
                        };
                        try call_args.append(allocator, index);
                    }
                    if (!complete) {
                        call_args.shrinkRetainingCapacity(args_start);
                        continue;
                    }
                    try call_edges.append(allocator, .{
                        .target = target,
                        .field = row.field,
                        .args_start = args_start,
                        .args_len = @intCast(call_args.items.len - args_start),
                        .stmt = @intCast(stmt_index),
                    });
                };
                for (0..GuardedList.borrowLen(args)) |position| {
                    const arg = GuardedList.at(args, position);
                    if (callee_sig.paramMode(position) == .owned) {
                        // The callee receives the argument's single unit;
                        // passing it is one consuming use, exactly like a
                        // consumed low-level argument.
                        try marks.consumeAt(allocator, &consumes, arg, @intCast(stmt_index));
                    } else if ((callee_sig.read_only_params & (arc_sig.paramBit(position) orelse 0)) == 0) {
                        // A borrowed-position argument stays with the
                        // caller while the callee uses it; unless the
                        // callee only reads the position, it may retain a
                        // holder that outlives the call.
                        marks.destroy(&destroyed, arg);
                    }
                }
            },
            .assign_call_erased => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.out_desc) |out_desc| {
                    marks.trackDef(&has_def, &multi_def, out_desc);
                    marks.destroy(&foreign_def, out_desc);
                }
                if (assign.reuse_source) |reuse_source| {
                    try marks.consumeAt(allocator, &consumes, reuse_source, @intCast(stmt_index));
                } else {
                    marks.destroy(&destroyed, assign.closure);
                }
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |index| {
                    const arg = GuardedList.at(args, index);
                    marks.destroy(&destroyed, arg);
                }
            },
            .assign_packed_erased_fn => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                if (assign.capture) |capture| marks.destroy(&destroyed, capture);
                if (assign.reuse) |reuse| try marks.consumeAt(allocator, &consumes, reuse, @intCast(stmt_index));
            },
            .assign_boxy_desc_ref => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
            },
            .assign_boxy_dict_ref => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
            },
            .assign_boxy_box => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                try marks.transfer(allocator, &consumes, &destroyed, assign.payload, assign.payload_mode, @intCast(stmt_index));
            },
            .assign_boxy_reuse_box => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                try marks.consumeAt(allocator, &consumes, assign.source, @intCast(stmt_index));
            },
            .assign_boxy_unbox => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                try marks.transfer(allocator, &consumes, &destroyed, assign.source, assign.source_mode, @intCast(stmt_index));
            },
            .assign_boxy_adapt => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                try marks.transfer(allocator, &consumes, &destroyed, assign.source, assign.source_mode, @intCast(stmt_index));
            },
            .assign_boxy_inspect => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                try marks.transfer(allocator, &consumes, &destroyed, assign.source, assign.source_mode, @intCast(stmt_index));
            },
            .assign_boxy_eq => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                try marks.transfer(allocator, &consumes, &destroyed, assign.lhs, assign.source_mode, @intCast(stmt_index));
                try marks.transfer(allocator, &consumes, &destroyed, assign.rhs, assign.source_mode, @intCast(stmt_index));
            },
            .assign_boxy_tag => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                if (assign.payload) |payload| try marks.transfer(allocator, &consumes, &destroyed, payload, assign.payload_mode, @intCast(stmt_index));
            },
            .assign_boxy_tag_payload => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                if (assign.target_desc) |local| {
                    marks.trackDef(&has_def, &multi_def, local);
                    marks.destroy(&foreign_def, local);
                }
                try marks.transfer(allocator, &consumes, &destroyed, assign.source, assign.source_mode, @intCast(stmt_index));
            },
            .boxy_tag_match => {},
            .assign_call_dict => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.destroy(&foreign_def, assign.target);
                const args = store.getLocalSpan(assign.args);
                for (0..GuardedList.borrowLen(args)) |index| {
                    const arg = GuardedList.at(args, index);
                    marks.destroy(&destroyed, arg);
                }
                const hidden_args = store.getLocalSpan(assign.hidden_args);
                for (0..GuardedList.borrowLen(hidden_args)) |index| {
                    const arg = GuardedList.at(hidden_args, index);
                    marks.destroy(&destroyed, arg);
                }
            },
            .str_match => |str_match| {
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
                const args = store.getLocalSpan(assign.args);
                // An op that neither allocates nor checks and whose result
                // is the one consumed argument's own unit passes that value
                // through unchanged: the result is an alias of the argument.
                // A slicing op also hands the unit on, but as a new outer
                // value that must keep its check.
                const pass_through: ?usize = if (!rc_effect.result_unique and !rc_effect.may_allocate and
                    rc_effect.may_runtime_uniqueness_check_args == 0 and rc_effect.result_shares_args == 0 and
                    @popCount(rc_effect.result_aliases_consumed_args) == 1 and
                    (rc_effect.result_aliases_consumed_args & rc_effect.consume_args) != 0 and
                    @ctz(rc_effect.result_aliases_consumed_args) < GuardedList.borrowLen(args))
                    @ctz(rc_effect.result_aliases_consumed_args)
                else
                    null;
                if (rc_effect.result_unique) {
                    marks.noteBirth(&born, assign.target);
                } else if (pass_through) |position| {
                    try Alias.record(marks, allocator, &alias_defs, &foreign_def, &consumes, assign.target, GuardedList.at(args, position), @intCast(stmt_index));
                } else {
                    marks.destroy(&foreign_def, assign.target);
                }
                for (0..GuardedList.borrowLen(args)) |position| {
                    const arg = GuardedList.at(args, position);
                    if (position >= 64) {
                        marks.destroy(&destroyed, arg);
                        continue;
                    }
                    const bit = @as(u64, 1) << @as(u6, @intCast(position));
                    var read_only = true;
                    if ((rc_effect.consume_args & bit) != 0) {
                        if (pass_through != position) try marks.consumeAt(allocator, &consumes, arg, @intCast(stmt_index));
                        read_only = false;
                    }
                    if ((rc_effect.retain_args & bit) != 0) {
                        marks.destroy(&destroyed, arg);
                        read_only = false;
                    }
                    if (read_only) {}
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
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                const fields = store.getLocalSpan(assign.fields);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    const field = GuardedList.at(fields, index);
                    // Storing into a fresh aggregate is the operand's
                    // consuming use; the field then holds its unit.
                    try marks.consumeAt(allocator, &consumes, field, @intCast(stmt_index));
                    if (index >= 64) continue;
                    if (marks.indexOf(assign.target)) |container| if (marks.indexOf(field)) |source| {
                        try field_stores.append(allocator, .{ .source = source, .container = container, .field = @intCast(index), .stmt = @intCast(stmt_index) });
                    };
                }
            },
            .assign_tag => |assign| {
                marks.trackDef(&has_def, &multi_def, assign.target);
                marks.noteBirth(&born, assign.target);
                // A variant without a refcounted payload has every payload
                // field vacuously unique: a payload read through another
                // variant's view never sees this value, so such a return
                // must not veto the fields the other variants do carry.
                if (marks.indexOf(assign.target)) |container| {
                    const rc_payload = if (assign.payload) |payload| marks.indexOf(payload) != null else false;
                    if (!rc_payload) try seed_masks.append(allocator, .{ .local = container, .mask = std.math.maxInt(u64) });
                }
                if (assign.payload) |payload| {
                    try marks.consumeAt(allocator, &consumes, payload, @intCast(stmt_index));
                    if (marks.indexOf(assign.target)) |container| if (marks.indexOf(payload)) |source| {
                        // A struct payload keeps its own field places
                        // behind the union; any other payload is the
                        // union's single field.
                        const payload_layout = layouts.getLayout(store.getLocal(payload).layout_idx);
                        if (payload_layout.tag == .struct_) {
                            try mask_aliases.append(allocator, .{ .source = source, .target = container });
                        } else {
                            try field_stores.append(allocator, .{ .source = source, .container = container, .field = 0, .stmt = @intCast(stmt_index) });
                        }
                    };
                }
            },
            .store_struct => |assign| {
                const fields = store.getLocalSpan(assign.fields);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    const field = GuardedList.at(fields, index);
                    try marks.consumeAt(allocator, &consumes, field, @intCast(stmt_index));
                }
            },
            .store_tag => |assign| {
                if (assign.payload) |payload| try marks.consumeAt(allocator, &consumes, payload, @intCast(stmt_index));
            },
            .set_local => |assign| {
                switch (assign.mode) {
                    .initialize_join_param => {
                        const target = marks.indexOf(assign.target);
                        const source = marks.indexOf(assign.value);
                        if (target != null and source != null) {
                            if (target.? != source.?) {
                                try join_incoming.append(allocator, .{ .target = target.?, .source = source.? });
                                try join_incoming_stmts.append(allocator, @intCast(stmt_index));
                                try marks.consumeAt(allocator, &consumes, assign.value, @intCast(stmt_index));
                                // The parameter receives the value with its
                                // stored fields; a record handed through a
                                // join keeps its per-field origins.
                                try mask_aliases.append(allocator, .{ .source = source.?, .target = target.? });
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
            .incref => |rc| {
                marks.destroy(&destroyed, rc.value);
                marks.destroy(&retained, rc.value);
            },
            .join => |join_stmt| {
                const params = store.getLocalSpan(join_stmt.params);
                for (0..GuardedList.borrowLen(params)) |param_index| {
                    const param = GuardedList.at(params, param_index);
                    marks.trackDef(&has_def, &multi_def, param);
                    if (marks.indexOf(param)) |target| {
                        join_targets.set(target);
                        join_decl_counts[target] += 1;
                    }
                }
            },
            // Returning is the value's consuming use: the unit moves to the
            // caller, which feeds the per-proc unique-return solve.
            .ret => |ret_stmt| try marks.consumeAt(allocator, &consumes, ret_stmt.value, @intCast(stmt_index)),
            .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| {
                try marks.consumeAt(allocator, &consumes, message, @intCast(stmt_index));
            },
            .debug => {},
            // The failure report is the message's consuming use.
            .expect_err => |expect_err_stmt| try marks.consumeAt(allocator, &consumes, expect_err_stmt.message, @intCast(stmt_index)),
            .expect => {},
            .init_uninitialized => {},
            .comptime_branch_taken => {},
            .switch_stmt => {},
            .switch_initialized_payload => {},
            .decref_if_initialized => {},
            .decref, .free, .jump, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
        }
    }

    // A view that some statement consumes is retained there and hands the
    // retained unit on, as a returned borrowed alias does; it follows the
    // alias rule, taking its source's unit at the definition when that is
    // the source's last use. A view that is only read stays a read.
    var consumed_views = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer consumed_views.deinit(allocator);
    for (consumes.items) |consume| {
        if (view_source[consume.index] != no_local) consumed_views.set(consume.index);
    }
    for (view_defs.items) |def| {
        if (!consumed_views.isSet(def.target)) continue;
        view_source[def.target] = no_local;
        foreign_def.unset(def.target);
        try consumes.append(allocator, .{ .index = def.source, .stmt = def.stmt });
        try alias_defs.append(allocator, .{ .target = def.target, .source = def.source, .stmt = def.stmt });
    }

    // An alias assigned into a join result cell is one of the cell's
    // incoming edges, alongside its explicit initializations; any other
    // alias target inherits from a single source, and distinct alias
    // definitions binding different sources never inherit. Several
    // definitions aliasing one source bind the same value whichever runs --
    // emission unshares a statement suffix that several paths reach into one
    // copy per path -- so each is its own transfer edge from that source.
    const cell_edge_counts = try allocator.alloc(u32, local_count);
    defer allocator.free(cell_edge_counts);
    @memset(cell_edge_counts, 0);
    const same_source_alias_counts = try allocator.alloc(u32, local_count);
    defer allocator.free(same_source_alias_counts);
    @memset(same_source_alias_counts, 0);
    var repeated_alias_edges = std.ArrayList(AliasDef).empty;
    defer repeated_alias_edges.deinit(allocator);
    for (alias_defs.items) |def| {
        if (join_targets.isSet(def.target)) {
            try join_incoming.append(allocator, .{ .target = def.target, .source = def.source });
            try join_incoming_stmts.append(allocator, def.stmt);
            cell_edge_counts[def.target] += 1;
            continue;
        }
        if (alias_source[def.target] == no_local) {
            alias_source[def.target] = def.source;
            alias_stmt[def.target] = def.stmt;
            try alias_targets.append(allocator, def.target);
            same_source_alias_counts[def.target] = 1;
        } else if (alias_source[def.target] != def.source) {
            foreign_def.set(def.target);
        } else {
            same_source_alias_counts[def.target] += 1;
            try repeated_alias_edges.append(allocator, def);
        }
    }
    // A local with several definitions keeps a tracked origin only when
    // every definition is a birth or, for a join result cell, an incoming
    // edge (the joins declaring the cell counted as well): whichever ran,
    // the cell's value is accounted for. Any other definition among
    // several leaves the origin untracked.
    var multi_ok = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer multi_ok.deinit(allocator);
    var multi_ok_iter = multi_def.iterator(.{});
    while (multi_ok_iter.next()) |index| {
        if (birth_counts[index] + join_decl_counts[index] + cell_edge_counts[index] == def_counts[index] or
            same_source_alias_counts[index] == def_counts[index])
        {
            multi_ok.set(index);
        }
    }

    // A read-only view's holder-adding occurrences belong to the value it
    // views.
    for (consumes.items) |*consume| {
        var root = consume.index;
        var steps: usize = 0;
        while (view_source[root] != no_local) : (steps += 1) {
            root = view_source[root];
            if (steps > local_count) solveInvariant("ARC uniqueness view chain contained a cycle");
        }
        consume.index = root;
    }
    for (0..local_count) |index| {
        if (view_source[index] == no_local or !destroyed.isSet(index)) continue;
        var root: u32 = @intCast(index);
        var steps: usize = 0;
        while (view_source[root] != no_local) : (steps += 1) {
            root = view_source[root];
            if (steps > local_count) solveInvariant("ARC uniqueness view chain contained a cycle");
        }
        destroyed.set(root);
    }

    // born_unique: every definition is a birth or a settled pure alias, and
    // no foreign definition. unique: born unique with no holder-adding
    // occurrence anywhere.
    var foreign_iter = foreign_def.iterator(.{});
    while (foreign_iter.next()) |index| born.unset(index);

    var multi_iter = multi_def.iterator(.{});
    while (multi_iter.next()) |index| {
        if (!multi_ok.isSet(index)) born.unset(index);
    }

    // An alias target's origin derives from its source, so a birth bit set
    // by another of its definitions must not stand on its own (the alias
    // definition may bind a non-unique value); and a multi-bound alias
    // target inherits only when every definition aliases its one source.
    for (alias_targets.items) |target| {
        born.unset(target);
        if (multi_def.isSet(target) and !multi_ok.isSet(target)) destroyed.set(target);
    }

    // Dense index back to the local it names, for the ordered-use queries.
    const index_to_local = try allocator.alloc(u32, local_count);
    defer allocator.free(index_to_local);
    if (proc_domain) |domain| {
        if (domain.locals) |locals| {
            @memcpy(index_to_local, locals);
        } else {
            for (domain.local_to_dense, 0..) |dense, raw| {
                if (dense != no_local) index_to_local[dense] = @intCast(raw);
            }
        }
    } else {
        for (index_to_local, 0..) |*slot, raw| slot.* = @intCast(raw);
    }

    var owned_order: ?UseOrder = null;
    defer if (owned_order) |*order| order.deinit();
    const order = if (workspace) |cached| &cached.order else blk: {
        if (exact_stmts) |stmts| {
            const lists = [_][]const LIR.CFStmtId{stmts};
            owned_order = if (order_scratch) |scratch|
                try UseOrder.initInScratch(allocator, store, &lists, scratch)
            else
                try UseOrder.init(allocator, store, &lists);
            break :blk &owned_order.?;
        }
        if (proc_stmts) |by_proc| {
            const lists = try allocator.alloc([]const LIR.CFStmtId, by_proc.len);
            defer allocator.free(lists);
            var len: usize = 0;
            for (by_proc, 0..) |stmts, proc_index| {
                if (only_proc) |proc_id| {
                    if (proc_index != @intFromEnum(proc_id)) continue;
                }
                lists[len] = stmts.items;
                len += 1;
            }
            owned_order = try UseOrder.init(allocator, store, lists[0..len]);
            break :blk &owned_order.?;
        }
        owned_order = try UseOrder.initFromStore(allocator, store, only_proc);
        break :blk &owned_order.?;
    };
    {
        var checks = std.ArrayList(EdgeCheck).empty;
        defer checks.deinit(allocator);
        for (alias_targets.items) |target| {
            try checks.append(allocator, .{ .source = @enumFromInt(index_to_local[alias_source[target]]), .stmt = alias_stmt[target], .target = target });
        }
        for (repeated_alias_edges.items) |def| {
            try checks.append(allocator, .{ .source = @enumFromInt(index_to_local[def.source]), .stmt = def.stmt, .target = def.target });
        }
        for (join_incoming.items, join_incoming_stmts.items) |incoming, stmt| {
            try checks.append(allocator, .{ .source = @enumFromInt(index_to_local[incoming.source]), .stmt = stmt, .target = incoming.target });
        }
        try settleEdgeChecks(order, checks.items, struct {
            foreign: *std.bit_set.DynamicBitSetUnmanaged,
            fn dead(ctx: @This(), target: u32) void {
                ctx.foreign.set(target);
            }
        }{ .foreign = &foreign_def });
    }
    if (workspace) |cached| {
        try cached.consumption.apply(cached.allocator, order, consumes.items, index_to_local, &destroyed);
    } else {
        try destroyOrderedConsumes(allocator, order, consumes.items, index_to_local, &destroyed);
    }

    // A stored field holds the source's unit only when the store is the
    // source's last use.
    var live_stores = std.ArrayList(FieldStoreEdge).empty;
    defer live_stores.deinit(allocator);
    std.mem.sort(FieldStoreEdge, field_stores.items, {}, struct {
        fn lessThan(_: void, a: FieldStoreEdge, b: FieldStoreEdge) bool {
            return a.source < b.source;
        }
    }.lessThan);
    for (field_stores.items) |edge| {
        if (try order.usesAfter(edge.stmt, @enumFromInt(index_to_local[edge.source]))) continue;
        try live_stores.append(allocator, edge);
    }
    // A call carries an argument's unit into the callee only when the call
    // is the argument's last use; otherwise the callee holds a retained
    // copy and the row's part of the result has a second holder.
    var live_calls = std.ArrayList(CallEdge).empty;
    defer live_calls.deinit(allocator);
    for (call_edges.items) |edge| {
        var live = true;
        for (call_args.items[edge.args_start..][0..edge.args_len]) |arg| {
            if (try order.usesAfter(edge.stmt, @enumFromInt(index_to_local[arg]))) {
                live = false;
                break;
            }
        }
        if (live) try live_calls.append(allocator, edge);
    }
    // A take inherits the field's uniqueness unless a copy of the
    // container or of the field, holding its own unit, can reach it: a
    // consuming use of the container (which retains every field the
    // container keeps using), or another read of the same field that was
    // retained.
    var eligible_reads = std.ArrayList(FieldReadEdge).empty;
    defer eligible_reads.deinit(allocator);
    if (field_reads.items.len != 0) {
        const consume_keys = try allocator.alloc(u32, consumes.items.len);
        defer allocator.free(consume_keys);
        for (consumes.items, 0..) |consume, index| consume_keys[index] = consume.index;
        var consume_rows = try EdgeRows.build(allocator, local_count, consume_keys);
        defer consume_rows.deinit(allocator);
        const read_keys = try allocator.alloc(u32, field_reads.items.len);
        defer allocator.free(read_keys);
        for (field_reads.items, 0..) |read, index| read_keys[index] = read.container;
        var read_rows = try EdgeRows.build(allocator, local_count, read_keys);
        defer read_rows.deinit(allocator);
        var killed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, field_reads.items.len);
        defer killed.deinit(allocator);
        var sources = std.ArrayList(u32).empty;
        defer sources.deinit(allocator);
        // One walk per container from its consumes, and one per field from
        // the field's retained reads, stamps everything they reach.
        for (0..local_count) |container_index| {
            const reads = read_rows.row(@intCast(container_index));
            var any_take = false;
            for (reads) |read_index| any_take = any_take or field_reads.items[read_index].take;
            if (!any_take) continue;
            const container: LIR.LocalId = @enumFromInt(index_to_local[container_index]);
            // Marks: the container's takes. A consume of the container
            // from which a take follows kills the container's takes.
            sources.clearRetainingCapacity();
            for (reads) |read_index| if (field_reads.items[read_index].take) try sources.append(allocator, field_reads.items[read_index].stmt);
            try order.markAmong(container, sources.items);
            var container_killed = false;
            for (consume_rows.row(@intCast(container_index))) |consume_index| {
                if (order.after(consumes.items[consume_index].stmt, container)) {
                    container_killed = true;
                    break;
                }
            }
            if (container_killed) {
                for (reads) |read_index| if (field_reads.items[read_index].take) killed.set(read_index);
                continue;
            }
            // Per field: a retained read of the field from which a take of
            // that field follows kills the field's takes.
            for (reads) |take_index| {
                const take = field_reads.items[take_index];
                if (!take.take or killed.isSet(take_index)) continue;
                sources.clearRetainingCapacity();
                for (reads) |read_index| {
                    const read = field_reads.items[read_index];
                    if (read.take and read.field == take.field) try sources.append(allocator, read.stmt);
                }
                try order.markAmong(container, sources.items);
                var field_killed = false;
                for (reads) |other_index| {
                    const other = field_reads.items[other_index];
                    if (other.take or other.field != take.field) continue;
                    const holds_unit = if (borrowed) |modes| !modes.isSet(index_to_local[other.target]) else switch (takes) {
                        .stamped => retained.isSet(other.target),
                        .none, .set => true,
                    };
                    if (holds_unit and order.after(other.stmt, container)) {
                        field_killed = true;
                        break;
                    }
                }
                if (field_killed) {
                    for (reads) |read_index| {
                        const read = field_reads.items[read_index];
                        if (read.take and read.field == take.field) killed.set(read_index);
                    }
                }
            }
        }
        for (field_reads.items, 0..) |read, index| {
            if (!read.take or killed.isSet(index) or multi_def.isSet(read.target) or read.field >= 64) continue;
            foreign_def.unset(read.target);
            try eligible_reads.append(allocator, read);
        }
    }

    const field_masks = try allocator.alloc(u64, local_count);
    errdefer allocator.free(field_masks);
    const dead_masks = try allocator.alloc(u64, local_count);
    defer allocator.free(dead_masks);
    var field_conds = FieldConds{ .base = try allocator.alloc(u32, local_count), .conds = &.{} };
    errdefer field_conds.deinit(allocator);
    @memset(field_conds.base, no_local);
    try settleUniqueOrigins(
        allocator,
        &born,
        conds,
        &foreign_def,
        &multi_def,
        &multi_ok,
        &destroyed,
        alias_source,
        alias_targets.items,
        join_incoming.items,
        .{
            .stores = live_stores.items,
            .aliases = mask_aliases.items,
            .reads = eligible_reads.items,
            .seeds = seed_masks.items,
            .calls = live_calls.items,
            .call_args = call_args.items,
        },
        field_masks,
        dead_masks,
        &field_conds,
    );
    for (field_masks, dead_masks) |*mask, dead| mask.* &= ~dead;

    var consumed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    errdefer consumed.deinit(allocator);
    for (consumes.items) |consume| consumed.set(consume.index);

    // unique: born under no condition, with no holder-adding occurrence.
    var unique = try born.clone(allocator);
    errdefer unique.deinit(allocator);
    var destroyed_iter = destroyed.iterator(.{});
    while (destroyed_iter.next()) |index| unique.unset(index);
    var born_iter = born.iterator(.{});
    while (born_iter.next()) |index| {
        if (conds[index] != 0) unique.unset(index);
    }

    return .{ .born_unique = born, .unique = unique, .destroyed = destroyed, .field_masks = field_masks, .conds = conds, .field_conds = field_conds, .consumed = consumed };
}

/// Records a field or payload read for the take-conditioned inheritance
/// settled after the order is known.
fn noteFieldRead(
    reads: *std.ArrayList(FieldReadEdge),
    marks: anytype,
    takes: TakeSource,
    store: *const LirStore,
    allocator: Allocator,
    target_local: LIR.LocalId,
    container_local: LIR.LocalId,
    field: u32,
    stmt: u32,
) SolveError!void {
    const target = marks.indexOf(target_local) orelse return;
    const container = marks.indexOf(container_local) orelse return;
    const take = switch (takes) {
        .none => false,
        .stamped => store.getCFStmt(@enumFromInt(stmt)).assign_ref.take_kind == .take,
        .set => |set| set.isSet(stmt),
    };
    try reads.append(allocator, .{ .container = container, .target = target, .field = field, .stmt = stmt, .take = take });
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

/// Small ownership-neutral fixtures for exercising the solver without emission.
const UniquenessTest = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    list: layout_mod.Idx,
    pair: layout_mod.Idx,

    fn init() SolveError!UniquenessTest {
        var layouts = try layout_mod.Store.init(std.testing.allocator, .u64);
        errdefer layouts.deinit();
        const list = try layouts.insertList(.i64);
        const pair = try layouts.putStructFields(&.{
            .{ .index = 0, .layout = list },
            .{ .index = 1, .layout = list },
        });
        return .{ .store = LirStore.init(std.testing.allocator), .layouts = layouts, .list = list, .pair = pair };
    }

    fn deinit(self: *@This()) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *@This(), layout: layout_mod.Idx) SolveError!LIR.LocalId {
        return self.store.addLocal(.{ .layout_idx = layout });
    }

    fn ret(self: *@This(), value: LIR.LocalId) SolveError!LIR.CFStmtId {
        return self.store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
    }

    fn call(self: *@This(), target: LIR.LocalId, callee: LIR.LirProcSpecId, args: []const LIR.LocalId, next: LIR.CFStmtId) SolveError!LIR.CFStmtId {
        return self.store.addCFStmt(.{ .assign_call = .{ .target = target, .proc = callee, .args = try self.store.addLocalSpan(args), .next = next } }, .test_fixture);
    }

    fn proc(self: *@This(), args: []const LIR.LocalId, body: ?LIR.CFStmtId, layout: layout_mod.Idx) SolveError!LIR.LirProcSpecId {
        return self.store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(self.store.procSpecCount())),
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(args),
            .body = body,
            .ret_layout = layout,
        }, .none);
    }

    fn expectSame(expected: Uniqueness, actual: Uniqueness) error{TestExpectedEqual}!void {
        const testing = std.testing;
        try testing.expectEqualSlices(u64, expected.field_masks, actual.field_masks);
        try testing.expectEqualSlices(arc_sig.ParamMask, expected.conds, actual.conds);
        for (0..expected.conds.len) |index| {
            try testing.expectEqual(expected.born_unique.isSet(index), actual.born_unique.isSet(index));
            try testing.expectEqual(expected.unique.isSet(index), actual.unique.isSet(index));
            try testing.expectEqual(expected.destroyed.isSet(index), actual.destroyed.isSet(index));
            try testing.expectEqual(expected.consumed.isSet(index), actual.consumed.isSet(index));
            for (0..64) |field| try testing.expectEqual(expected.field_conds.get(@intCast(index), @intCast(field)), actual.field_conds.get(@intCast(index), @intCast(field)));
        }
    }
};

test "ARC public solve cleans up every coordinator allocation failure" {
    var f = try UniquenessTest.init();
    defer f.deinit();
    const param = try f.local(f.list);
    const callee = try f.proc(&.{param}, try f.ret(param), f.list);
    const argument = try f.local(f.list);
    const result = try f.local(f.list);
    const caller = try f.proc(&.{argument}, try f.call(result, callee, &.{argument}, try f.ret(result)), f.list);
    const rc_local = try std.testing.allocator.alloc(bool, f.store.localCount());
    defer std.testing.allocator.free(rc_local);
    @memset(rc_local, true);

    const Probe = struct {
        fn run(allocator: Allocator, fixture: *const UniquenessTest, rc: []const bool, root: LIR.LirProcSpecId) SolveError!void {
            var solution = try solve(allocator, &fixture.store, &fixture.layouts, rc, &.{}, &.{root}, true);
            defer solution.deinit();
        }
    };
    // Inputs outlive the entire sweep: every injected failure belongs to
    // public solver construction, analysis, publication, or result handoff.
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Probe.run, .{ &f, rc_local, caller });
}

test "ordered-use inventory transfer cleans up on allocation failure" {
    var f = try UniquenessTest.init();
    defer f.deinit();
    const local = try f.local(f.list);
    _ = try f.proc(&.{local}, try f.ret(local), f.list);
    const Probe = struct {
        fn run(allocator: Allocator, store: *const LirStore) SolveError!void {
            var order = try UseOrder.initFromStore(allocator, store, null);
            defer order.deinit();
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Probe.run, .{&f.store});
}

fn expectEmptyUseOrderScratch(scratch: *const UseOrder.Scratch) error{TestExpectedEqual}!void {
    for (scratch.jump_join) |join| try std.testing.expectEqual(no_local, join);
    for (scratch.mark_gen) |mark| try std.testing.expectEqual(@as(u32, 0), mark);
    for ([_]*const UseOrder.RowIndex{ &scratch.reads_index, &scratch.defs_index, &scratch.preds_index }) |index| {
        for (index.len) |len| try std.testing.expectEqual(@as(u32, 0), len);
    }
    try std.testing.expectEqual(@as(usize, 0), scratch.seen.count());
    try std.testing.expectEqual(@as(usize, 0), scratch.unresolved.count());
}

test "per-procedure use orders in a shared scratch match standalone orders and restore it" {
    const testing = std.testing;
    const allocator = testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    // ARC variants share source locals while owning distinct statements.
    const x = try f.local(f.list);
    const y = try f.local(f.list);
    const cond = try f.local(.bool);
    const locals = [_]LIR.LocalId{ x, y, cond };
    var bodies: [4]LIR.CFStmtId = undefined;
    for (bodies[0..3], 0..) |*body, index| {
        const id: LIR.JoinPointId = @enumFromInt(index);
        const left = try f.store.addCFStmt(.{ .assign_ref = .{ .target = y, .op = .{ .local = x }, .next = try f.store.addCFStmt(.{ .jump = .{ .target = id } }, .test_fixture) } }, .test_fixture);
        const right = try f.store.addCFStmt(.{ .assign_ref = .{ .target = y, .op = .{ .local = x }, .next = try f.store.addCFStmt(.{ .jump = .{ .target = id } }, .test_fixture) } }, .test_fixture);
        const branch = try f.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try f.store.addCFSwitchBranches(&.{.{ .value = 1, .body = left }}),
            .default_branch = right,
        } }, .test_fixture);
        body.* = try f.store.addCFStmt(.{ .join = .{ .id = id, .params = LIR.LocalSpan.empty(), .body = try f.ret(y), .remainder = branch } }, .test_fixture);
    }
    // A jump to no join in its procedure has unknown successors.
    bodies[3] = try f.store.addCFStmt(.{ .assign_ref = .{ .target = y, .op = .{ .local = x }, .next = try f.store.addCFStmt(.{ .jump = .{ .target = @enumFromInt(9) } }, .test_fixture) } }, .test_fixture);
    var lists: [4]std.ArrayList(LIR.CFStmtId) = @splat(.empty);
    defer for (&lists) |*list| list.deinit(allocator);
    for (bodies, &lists) |body, *list| {
        _ = try f.proc(&.{ x, cond }, body, f.list);
        try collectProcStatements(allocator, &f.store, body, list);
    }

    var scratch = try UseOrder.Scratch.init(allocator, &f.store);
    defer scratch.deinit(allocator);
    for (0..2) |_| {
        for (&lists) |*list| {
            const inventories = [_][]const LIR.CFStmtId{list.items};
            var shared = try UseOrder.initInScratch(allocator, &f.store, &inventories, &scratch);
            var standalone = try UseOrder.init(allocator, &f.store, &inventories);
            defer standalone.deinit();
            try testing.expectEqualSlices(u32, standalone.topology.unresolved_list, shared.topology.unresolved_list);
            for (list.items) |stmt_id| {
                const stmt = @intFromEnum(stmt_id);
                for (locals) |local| {
                    try testing.expectEqual(standalone.reads(stmt, local), shared.reads(stmt, local));
                    try testing.expectEqual(standalone.defines(stmt, local), shared.defines(stmt, local));
                    try testing.expectEqual(try standalone.usesAfter(stmt, local), try shared.usesAfter(stmt, local));
                }
            }
            shared.deinit();
            try expectEmptyUseOrderScratch(&scratch);
        }
    }

    // A build that fails part way leaves no entries behind either.
    const Probe = struct {
        fn run(probe_allocator: Allocator, store: *const LirStore, tables: *UseOrder.Scratch, inventories: []const []const LIR.CFStmtId) (SolveError || error{TestExpectedEqual})!void {
            try expectEmptyUseOrderScratch(tables);
            var order = try UseOrder.initInScratch(probe_allocator, store, inventories, tables);
            order.deinit();
        }
    };
    const inventories = [_][]const LIR.CFStmtId{ lists[0].items, lists[3].items };
    try testing.checkAllAllocationFailures(allocator, Probe.run, .{ &f.store, &scratch, @as([]const []const LIR.CFStmtId, &inventories) });
    try expectEmptyUseOrderScratch(&scratch);
}

test "uniqueness workspace reuses topology and exact use queries across signature changes" {
    const testing = std.testing;
    const allocator = testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const param = try f.local(f.list);
    const result = try f.local(f.list);
    const alias = try f.local(f.list);
    const single_field = try f.layouts.putStructFields(&.{.{ .index = 0, .layout = f.list }});
    const pair = try f.local(single_field);
    const taken = try f.local(f.list);
    const proc = try f.proc(&.{param}, null, f.list);
    const ret = try f.ret(taken);
    const read = try f.store.addCFStmt(.{ .assign_ref = .{ .target = taken, .op = .{ .field = .{ .source = pair, .field_idx = 0 } }, .next = ret } }, .test_fixture);
    const store = try f.store.addCFStmt(.{ .assign_struct = .{ .target = pair, .fields = try f.store.addLocalSpan(&.{alias}), .next = read } }, .test_fixture);
    const alias_stmt = try f.store.addCFStmt(.{ .assign_ref = .{ .target = alias, .op = .{ .local = result }, .next = store } }, .test_fixture);
    const call = try f.call(result, proc, &.{param}, alias_stmt);
    f.store.setProcSpecBody(proc, call);
    var statements = [_]std.ArrayList(LIR.CFStmtId){.empty};
    defer statements[0].deinit(allocator);
    try collectProcStatements(allocator, &f.store, call, &statements[0]);
    var workspace = try UniquenessWorkspace.init(allocator, &f.store, &statements);
    defer workspace.deinit();
    const topology = workspace.order.topology.preds.stmts.ptr;
    var borrowed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, 5);
    defer borrowed.deinit(allocator);
    var takes = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, f.store.cfStmtCount());
    defer takes.deinit(allocator);
    takes.set(@intFromEnum(read));
    const rc = [_]bool{true} ** 5;
    var sigs = [_]arc_sig.RcSig{.all_owned};
    const conditions = [_]arc_sig.RetCondition{.{ .field = arc_sig.RetCondition.whole_value, .params = 1 }};
    for (0..3) |round| {
        // A recursive conditional return adds an exact argument dependency;
        // later an unconditional birth removes that condition.
        if (round == 1) sigs[0].ret_conditions = .{ .start = 0, .len = 1 };
        if (round == 2) {
            sigs[0].ret_conditions = .empty;
            sigs[0].ret_unique = true;
        }
        const table = arc_sig.SigTable{ .sigs = &sigs, .ret_conditions = &conditions };
        const actual = try workspace.analyze(&f.store, &rc, table, true, &f.layouts, .{ .set = &takes }, &borrowed);
        var expected = try computeUniquenessDetailed(allocator, &f.store, &rc, table, null, null, null, null, true, &f.layouts, .{ .set = &takes }, &borrowed, null, null);
        defer expected.deinit(allocator);
        try UniquenessTest.expectSame(expected, actual);
        try testing.expectEqual(topology, workspace.order.topology.preds.stmts.ptr);
        try testing.expectEqual(round != 0, actual.born_unique.isSet(@intFromEnum(taken)));
        if (round == 1) try testing.expectEqual(@as(arc_sig.ParamMask, 1), actual.conds[@intFromEnum(taken)]);
        if (round == 2) try testing.expect(actual.unique.isSet(@intFromEnum(taken)));
        // Evict the current generation's local, not the immutable answers.
        // Re-querying all recorded edges must not walk a single predecessor.
        workspace.order.marked_local = no_local;
        const visits = workspace.order.backward_visits;
        var answers = workspace.use_answers.iterator();
        while (answers.next()) |entry| {
            const key = entry.key_ptr.*;
            try testing.expectEqual(entry.value_ptr.*, try workspace.order.usesAfter(@intFromEnum(key.stmt), key.local));
        }
        try testing.expectEqual(visits, workspace.order.backward_visits);
    }
    try testing.expectEqual(@as(usize, 3), workspace.iterations);
    try testing.expectEqual(@as(usize, 1), workspace.consumption.rebuilds);
    // A separate post-take proof cannot reuse the pre-take lattice verdict.
    const without_take = try workspace.analyze(&f.store, &rc, .{ .sigs = &sigs }, true, &f.layouts, .none, &borrowed);
    try testing.expect(!without_take.born_unique.isSet(@intFromEnum(taken)));
}

test "uniqueness workspace reanalyzes read-only signatures through borrowed views" {
    const testing = std.testing;
    const allocator = testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const source = try f.local(f.list);
    const view = try f.local(f.list);
    const ignored = try f.local(.i64);
    const param = try f.local(f.list);
    const callee = try f.proc(&.{param}, null, .i64);
    const ret = try f.ret(source);
    const call = try f.call(ignored, callee, &.{view}, ret);
    const alias = try f.store.addCFStmt(.{ .assign_ref = .{ .target = view, .op = .{ .local = source }, .next = call } }, .test_fixture);
    const body = try f.store.addCFStmt(.{ .assign_list = .{ .target = source, .elems = try f.store.addLocalSpan(&.{}), .next = alias } }, .test_fixture);
    _ = try f.proc(&.{}, body, f.list);
    var statements = [_]std.ArrayList(LIR.CFStmtId){ .empty, .empty };
    defer for (&statements) |*stmts| stmts.deinit(allocator);
    try collectProcStatements(allocator, &f.store, body, &statements[1]);
    var workspace = try UniquenessWorkspace.init(allocator, &f.store, &statements);
    defer workspace.deinit();
    const rc = [_]bool{ true, true, false, true };
    var borrowed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, rc.len);
    defer borrowed.deinit(allocator);
    borrowed.set(@intFromEnum(view));
    var sigs = [_]arc_sig.RcSig{ .{ .borrowed_params = 1 }, .all_owned };
    for ([_]arc_sig.ParamMask{ 0, 1, 0 }) |read_only| {
        sigs[0].read_only_params = read_only;
        const table = arc_sig.SigTable{ .sigs = &sigs };
        const actual = try workspace.analyze(&f.store, &rc, table, true, &f.layouts, .none, &borrowed);
        var expected = try computeUniquenessDetailed(allocator, &f.store, &rc, table, null, null, null, null, true, &f.layouts, .none, &borrowed, null, null);
        defer expected.deinit(allocator);
        try UniquenessTest.expectSame(expected, actual);
        try testing.expectEqual(read_only != 0, actual.unique.isSet(@intFromEnum(source)));
        try testing.expect(!actual.consumed.isSet(@intFromEnum(view)));
    }
    try testing.expectEqual(@as(usize, 1), workspace.consumption.rebuilds);
    sigs[0].read_only_params = 1;
    for ([_]bool{ true, false, true }, 1..) |is_view, rebuilds| {
        borrowed.setValue(@intFromEnum(view), is_view);
        const table = arc_sig.SigTable{ .sigs = &sigs };
        const actual = try workspace.analyze(&f.store, &rc, table, true, &f.layouts, .none, &borrowed);
        var reference = try computeUniquenessDetailed(allocator, &f.store, &rc, table, null, null, null, null, true, &f.layouts, .none, &borrowed, null, null);
        defer reference.deinit(allocator);
        try UniquenessTest.expectSame(reference, actual);
        // An owned alias adds a holder before the source's return. Restoring
        // the borrowed view must remove that obsolete consumption contribution.
        try testing.expectEqual(is_view, actual.unique.isSet(@intFromEnum(source)));
        try testing.expectEqual(!is_view, actual.destroyed.isSet(@intFromEnum(source)));
        try testing.expect(!actual.consumed.isSet(@intFromEnum(view)));
        try testing.expectEqual(rebuilds, workspace.consumption.rebuilds);
    }
    sigs[0].borrowed_params = 0;
    const table = arc_sig.SigTable{ .sigs = &sigs };
    const owned = try workspace.analyze(&f.store, &rc, table, true, &f.layouts, .none, &borrowed);
    var expected = try computeUniquenessDetailed(allocator, &f.store, &rc, table, null, null, null, null, true, &f.layouts, .none, &borrowed, null, null);
    defer expected.deinit(allocator);
    try UniquenessTest.expectSame(expected, owned);
    try testing.expectEqual(@as(usize, 4), workspace.consumption.rebuilds);
    try testing.expect(owned.consumed.isSet(@intFromEnum(view)));
    try testing.expect(!owned.unique.isSet(@intFromEnum(source)));
}

/// Builds `list = []` followed by `alias = list` on each of two exclusive
/// arms, or, with `sequential`, twice on one path, and returns whether
/// `alias` keeps `list`'s unique birth.
fn aliasBornUniqueAfterRepeatedDefinitions(sequential: bool) SolveError!bool {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const flag = try f.local(.u64);
    const list = try f.local(f.list);
    const alias = try f.local(f.list);
    const ret = try f.ret(alias);
    const body = if (sequential) blk: {
        const second = try f.store.addCFStmt(.{ .assign_ref = .{ .target = alias, .op = .{ .local = list }, .next = ret } }, .test_fixture);
        break :blk try f.store.addCFStmt(.{ .assign_ref = .{ .target = alias, .op = .{ .local = list }, .next = second } }, .test_fixture);
    } else blk: {
        const first = try f.store.addCFStmt(.{ .assign_ref = .{ .target = alias, .op = .{ .local = list }, .next = ret } }, .test_fixture);
        const second = try f.store.addCFStmt(.{ .assign_ref = .{ .target = alias, .op = .{ .local = list }, .next = try f.ret(alias) } }, .test_fixture);
        const branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 1, .body = first }});
        break :blk try f.store.addCFStmt(.{ .switch_stmt = .{
            .cond = flag,
            .branches = branches,
            .default_branch = second,
            .default_is_cold = false,
            .continuation = null,
        } }, .test_fixture);
    };
    const entry = try f.store.addCFStmt(.{ .assign_list = .{ .target = list, .elems = LIR.LocalSpan.empty(), .next = body } }, .test_fixture);
    _ = try f.proc(&.{flag}, entry, f.list);
    const rc = [_]bool{ false, true, true };
    var sigs = [_]arc_sig.RcSig{.all_owned};
    var borrowed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, rc.len);
    defer borrowed.deinit(allocator);
    var uniqueness = try computeUniquenessDetailed(allocator, &f.store, &rc, .{ .sigs = &sigs }, null, null, null, null, true, &f.layouts, .none, &borrowed, null, null);
    defer uniqueness.deinit(allocator);
    return uniqueness.born_unique.isSet(@intFromEnum(alias));
}

test "uniqueness: identical alias definitions on exclusive paths keep their source's birth" {
    // Emission unshares a suffix two paths reach into one copy per path, so
    // the emitted procedure binds one alias target on each arm.
    try std.testing.expect(try aliasBornUniqueAfterRepeatedDefinitions(false));
}

test "uniqueness: identical alias definitions on one path lose their source's birth" {
    // The second definition reads the source after the first moved its unit.
    try std.testing.expect(!try aliasBornUniqueAfterRepeatedDefinitions(true));
}

test "uniqueness fixed point propagates fresh returns through a call diamond and chain" {
    const testing = std.testing;
    const allocator = testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const fresh = try f.local(f.list);
    const leaf_ret = try f.ret(fresh);
    const leaf_body = try f.store.addCFStmt(.{ .assign_list = .{ .target = fresh, .elems = try f.store.addLocalSpan(&.{}), .next = leaf_ret } }, .test_fixture);
    const leaf = try f.proc(&.{}, leaf_body, f.list);
    var branches: [2]LIR.LirProcSpecId = undefined;
    for (&branches) |*branch| {
        const result = try f.local(f.list);
        branch.* = try f.proc(&.{}, try f.call(result, leaf, &.{}, try f.ret(result)), f.list);
    }
    const left = try f.local(f.list);
    const right = try f.local(f.list);
    const pair = try f.local(f.pair);
    const ret = try f.ret(pair);
    const make_pair = try f.store.addCFStmt(.{ .assign_struct = .{ .target = pair, .fields = try f.store.addLocalSpan(&.{ left, right }), .next = ret } }, .test_fixture);
    const right_call = try f.call(right, branches[1], &.{}, make_pair);
    var chain = try f.proc(&.{}, try f.call(left, branches[0], &.{}, right_call), f.pair);
    for (0..8) |_| {
        const result = try f.local(f.pair);
        chain = try f.proc(&.{}, try f.call(result, chain, &.{}, try f.ret(result)), f.pair);
    }
    const rc = try allocator.alloc(bool, f.store.localCount());
    defer allocator.free(rc);
    @memset(rc, true);
    const builds_before = @import("use_order.zig").topology_builds;
    const rounds_before = uniqueness_analysis_rounds;
    var solution = try solve(allocator, &f.store, &f.layouts, rc, &.{}, &.{}, true);
    defer solution.deinit();
    try testing.expectEqual(@as(usize, 1), @import("use_order.zig").topology_builds - builds_before);
    try testing.expect(uniqueness_analysis_rounds - rounds_before > 8);
    for (solution.sigs) |sig| try testing.expect(sig.ret_unique);
    try testing.expectEqual(@as(u64, 3), solution.sigOf(chain).ret_unique_fields);
    try testing.expect(solution.isUnique(fresh));
    // Even settled signatures must receive the mandatory post-take analysis,
    // over the structure the first settlement retained on the solution.
    const settled_rounds = uniqueness_analysis_rounds;
    try settleUniqueness(allocator, &f.store, &f.layouts, rc, &solution, .stamped, true);
    try testing.expectEqual(@as(usize, 1), @import("use_order.zig").topology_builds - builds_before);
    try testing.expectEqual(@as(usize, 1), uniqueness_analysis_rounds - settled_rounds);
    try testing.expectEqual(@as(u64, 3), solution.sigOf(chain).ret_unique_fields);
    UniquenessOracleState.resetCapabilities(&solution);
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, rc, &solution, .none, &metrics);
}

/// Only the fields settlement owns are copied; binding/topology facts remain
/// frozen and borrowed from the fixture's solved base solution.
const UniquenessOracleState = struct {
    solution: Solution,

    fn init(source: *const Solution) SolveError!@This() {
        const allocator = std.testing.allocator;
        var result = source.*;
        result.sigs = try allocator.dupe(arc_sig.RcSig, source.sigs);
        errdefer allocator.free(result.sigs);
        result.ret_conditions = try allocator.dupe(arc_sig.RetCondition, source.ret_conditions);
        errdefer allocator.free(result.ret_conditions);
        result.unique_seed_masks = try allocator.dupe(arc_sig.ParamMask, source.unique_seed_masks);
        errdefer allocator.free(result.unique_seed_masks);
        result.unique = try source.unique.clone(allocator);
        errdefer result.unique.deinit(allocator);
        result.unique_born = try source.unique_born.clone(allocator);
        errdefer result.unique_born.deinit(allocator);
        result.unique_destroyed = try source.unique_destroyed.clone(allocator);
        errdefer result.unique_destroyed.deinit(allocator);
        result.unique_conds = try allocator.dupe(arc_sig.ParamMask, source.unique_conds);
        return .{ .solution = result };
    }

    fn deinit(self: *@This()) void {
        const allocator = std.testing.allocator;
        allocator.free(self.solution.sigs);
        allocator.free(self.solution.ret_conditions);
        allocator.free(self.solution.unique_seed_masks);
        self.solution.unique.deinit(allocator);
        self.solution.unique_born.deinit(allocator);
        self.solution.unique_destroyed.deinit(allocator);
        allocator.free(self.solution.unique_conds);
    }

    fn resetCapabilities(solution: *Solution) void {
        for (solution.sigs) |*sig| {
            sig.read_only_params = 0;
            sig.ret_unique = false;
            sig.ret_unique_fields = 0;
            sig.ret_conditions = .empty;
        }
    }

    fn compare(f: *const UniquenessTest, rc: []const bool, solution: *Solution, takes: TakeSource, metrics: *UniquenessMetrics) (SolveError || error{TestExpectedEqual})!void {
        var oracle = try init(solution);
        defer oracle.deinit();
        try settleUniquenessOracle(std.testing.allocator, &f.store, &f.layouts, rc, &oracle.solution, takes, true);
        try settleUniquenessWithOptions(std.testing.allocator, &f.store, &f.layouts, rc, solution, takes, true, .{ .metrics = metrics });
        // Includes every signature field and exact flat span/row ordering.
        try std.testing.expectEqualDeep(oracle.solution.sigs, solution.sigs);
        try std.testing.expectEqualDeep(oracle.solution.ret_conditions, solution.ret_conditions);
        try std.testing.expectEqualSlices(arc_sig.ParamMask, oracle.solution.unique_seed_masks, solution.unique_seed_masks);
        try std.testing.expectEqualSlices(arc_sig.ParamMask, oracle.solution.unique_conds, solution.unique_conds);
        for (0..solution.unique.bit_length) |raw| {
            try std.testing.expectEqual(oracle.solution.unique.isSet(raw), solution.unique.isSet(raw));
            try std.testing.expectEqual(oracle.solution.unique_born.isSet(raw), solution.unique_born.isSet(raw));
            try std.testing.expectEqual(oracle.solution.unique_destroyed.isSet(raw), solution.unique_destroyed.isSet(raw));
        }
    }
};

test "component uniqueness preserves shared RC definitions shared statements and bodyless argument positions" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const shared = try f.local(f.list);
    const bodyless = try f.local(f.list);
    const scalar = try f.local(.i64);
    for (0..2) |_| {
        const body = try f.store.addCFStmt(.{ .assign_list = .{
            .target = shared,
            .elems = try f.store.addLocalSpan(&.{}),
            .next = try f.ret(shared),
        } }, .test_fixture);
        _ = try f.proc(&.{}, body, f.list);
    }
    _ = try f.proc(&.{ scalar, shared }, null, f.list);
    _ = try f.proc(&.{bodyless}, null, f.list);
    // A shared scalar is not an ownership overlap.
    for (0..2) |_| _ = try f.proc(&.{scalar}, try f.ret(scalar), .i64);
    // Sharing a statement is an overlap even without an RC local.
    const shared_body = try f.ret(scalar);
    for (0..2) |_| _ = try f.proc(&.{scalar}, shared_body, .i64);
    // The last proc's shared argument position wins, just as in the global
    // parameter scan; component formation must not reorder these definitions.
    _ = try f.proc(&.{shared}, null, f.list);
    const rc = [_]bool{ true, true, false };
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    UniquenessOracleState.resetCapabilities(&solution);
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expectEqual(@as(u64, 5), metrics.components);
    try std.testing.expectEqual(@as(u64, 5), metrics.component_runs);
    try std.testing.expectEqual(@as(u64, 2), metrics.local_visits);
    try std.testing.expectEqual(@as(u64, 7), metrics.statement_visits);
    try std.testing.expect(solution.unique_born.isSet(@intFromEnum(bodyless)));
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.unique_conds[@intFromEnum(bodyless)]);
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.unique_conds[@intFromEnum(shared)]);
    // Pinned capabilities are contracts, but their conditional rows follow
    // the legacy rule and are cleared rather than inferred.
    allocator.free(solution.ret_conditions);
    solution.ret_conditions = try allocator.dupe(arc_sig.RetCondition, &.{.{ .field = 0, .params = 1 }});
    solution.sigs[3].ret_conditions = .{ .start = 0, .len = 1 };
    solution.sigs[3].read_only_params = 1;
    solution.sigs[3].ret_unique = true;
    solution.sigs[3].ret_unique_fields = 1;
    metrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expectEqual(@as(u32, 0), solution.sigs[3].ret_conditions.len);
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.sigs[3].read_only_params);
    try std.testing.expect(solution.sigs[3].ret_unique);
    try std.testing.expectEqual(@as(u64, 1), solution.sigs[3].ret_unique_fields);
}

test "component uniqueness revisits only dirty callers along a long chain" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const leaf = try f.local(f.list);
    const birth = try f.store.addCFStmt(.{ .assign_list = .{
        .target = leaf,
        .elems = try f.store.addLocalSpan(&.{}),
        .next = try f.ret(leaf),
    } }, .test_fixture);
    var callee = try f.proc(&.{}, birth, f.list);
    for (0..6) |_| {
        const local = try f.local(f.list);
        callee = try f.proc(&.{}, try f.call(local, callee, &.{}, try f.ret(local)), f.list);
    }
    for (0..10) |_| {
        const local = try f.local(f.list);
        const body = try f.store.addCFStmt(.{ .assign_list = .{
            .target = local,
            .elems = try f.store.addLocalSpan(&.{}),
            .next = try f.ret(local),
        } }, .test_fixture);
        _ = try f.proc(&.{}, body, f.list);
    }
    const rc = [_]bool{true} ** 17;
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    UniquenessOracleState.resetCapabilities(&solution);
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expectEqual(@as(u64, 17), metrics.components);
    try std.testing.expectEqual(@as(u64, 7), metrics.signature_waves);
    try std.testing.expectEqual(@as(u64, 23), metrics.component_runs);
    try std.testing.expectEqual(@as(u64, 23), metrics.local_visits);
    try std.testing.expectEqual(@as(u64, 46), metrics.statement_visits);
    for (solution.sigs) |sig| try std.testing.expect(sig.ret_unique);
    metrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .stamped, &metrics);
    try std.testing.expectEqual(@as(u64, 17), metrics.component_runs);
    try std.testing.expectEqual(@as(u64, 1), metrics.signature_waves);
}

test "component uniqueness ignores offset-only return condition changes" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    var params: [4]LIR.LocalId = undefined;
    var procs: [4]LIR.LirProcSpecId = undefined;
    for (&params, &procs) |*param, *proc| {
        param.* = try f.local(f.list);
        proc.* = try f.proc(&.{param.*}, null, f.list);
    }
    f.store.setProcSpecBody(procs[1], try f.ret(params[1]));
    f.store.setProcSpecBody(procs[2], try f.ret(params[2]));
    for ([_]usize{ 0, 3 }, [_]usize{ 2, 1 }) |caller, callee| {
        const result = try f.local(f.list);
        f.store.setProcSpecBody(procs[caller], try f.call(result, procs[callee], &.{params[caller]}, try f.ret(result)));
    }
    const rc = [_]bool{true} ** 6;
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    // Owned inputs transfer their unit into the conditional-return edge.
    for (solution.sigs) |*sig| {
        sig.borrowed_params = 0;
        sig.ret_mode = .owned;
    }
    UniquenessOracleState.resetCapabilities(&solution);
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expectEqual(@as(u64, 4), metrics.components);
    try std.testing.expectEqual(@as(u64, 2), metrics.signature_waves);
    try std.testing.expectEqual(@as(u64, 6), metrics.component_runs);
    for (solution.sigs, 0..) |sig, index| {
        try std.testing.expectEqual(@as(u32, @intCast(index)), sig.ret_conditions.start);
        try std.testing.expectEqual(@as(u32, 1), sig.ret_conditions.len);
    }
}

test "component uniqueness read-only return refinement revives a borrowed-view lender" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const source = try f.local(f.list);
    const view = try f.local(f.list);
    const ignored = try f.local(.i64);
    const param = try f.local(f.list);
    const scalar = try f.local(.i64);
    const callee_body = try f.store.addCFStmt(.{ .assign_literal = .{
        .target = scalar,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } },
        .next = try f.ret(scalar),
    } }, .test_fixture);
    const callee = try f.proc(&.{param}, callee_body, .i64);
    const call = try f.call(ignored, callee, &.{view}, try f.ret(source));
    const alias = try f.store.addCFStmt(.{ .assign_ref = .{ .target = view, .op = .{ .local = source }, .next = call } }, .test_fixture);
    const body = try f.store.addCFStmt(.{ .assign_list = .{
        .target = source,
        .elems = try f.store.addLocalSpan(&.{}),
        .next = alias,
    } }, .test_fixture);
    _ = try f.proc(&.{}, body, f.list);
    const rc = [_]bool{ true, true, false, true, false };
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    try std.testing.expect(solution.borrowed.isSet(@intFromEnum(view)));
    UniquenessOracleState.resetCapabilities(&solution);
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expectEqual(@as(u64, 2), metrics.components);
    try std.testing.expectEqual(@as(u64, 3), metrics.component_runs);
    try std.testing.expectEqual(@as(u64, 2), metrics.signature_waves);
    try std.testing.expect(solution.isUnique(source));
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.sigs[@intFromEnum(callee)].read_only_params);
}

test "component uniqueness recursive conditional fields restart after committed takes" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const param = try f.local(f.list);
    const called = try f.local(f.list);
    const pair = try f.local(f.pair);
    const taken = try f.local(f.list);
    const scalar = try f.local(.i64);
    const proc = try f.proc(&.{param}, null, f.list);
    const ret = try f.ret(taken);
    const read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = taken,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = ret,
    } }, .test_fixture);
    const aggregate = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ called, scalar }),
        .next = read,
    } }, .test_fixture);
    f.store.setProcSpecBody(proc, try f.call(called, proc, &.{param}, aggregate));
    // The aggregate-producing peer exercises semantic field rows separately.
    const field_param = try f.local(f.list);
    const returned_pair = try f.local(f.pair);
    const field_body = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = returned_pair,
        .fields = try f.store.addLocalSpan(&.{ field_param, scalar }),
        .next = try f.ret(returned_pair),
    } }, .test_fixture);
    const field_proc = try f.proc(&.{field_param}, field_body, f.pair);
    const rc = [_]bool{ true, true, true, true, false, true, true };
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    for (solution.sigs) |*sig| {
        sig.borrowed_params = 0;
        sig.ret_mode = .owned;
    }
    UniquenessOracleState.resetCapabilities(&solution);
    // A previously proved recursive contract is an input to this settlement.
    allocator.free(solution.ret_conditions);
    solution.ret_conditions = try allocator.dupe(arc_sig.RetCondition, &.{.{
        .field = arc_sig.RetCondition.whole_value,
        .params = 1,
    }});
    solution.sigs[@intFromEnum(proc)].ret_conditions = .{ .start = 0, .len = 1 };
    var takes = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, f.store.cfStmtCount());
    defer takes.deinit(allocator);
    takes.set(@intFromEnum(read));
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .{ .set = &takes }, &metrics);
    try std.testing.expectEqual(@as(u64, 2), metrics.components);
    try std.testing.expect(solution.unique_born.isSet(@intFromEnum(taken)));
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.unique_conds[@intFromEnum(taken)]);
    const field_rows = solution.sigTable().retConditionsOf(solution.sigs[@intFromEnum(field_proc)]);
    try std.testing.expectEqual(@as(usize, 1), field_rows.len);
    try std.testing.expectEqual(@as(u32, 0), field_rows[0].field);
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), field_rows[0].params);
    metrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expect(!solution.unique_born.isSet(@intFromEnum(taken)));
    // Clearing the recursive row dirties its own component, not the field peer.
    try std.testing.expectEqual(@as(u64, 3), metrics.component_runs);
    try std.testing.expectEqual(@as(u64, 2), metrics.signature_waves);
}

test "component uniqueness inventories join parameters incoming transfers and nonzero seeds without frames" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    const param = try f.local(f.list);
    const joined = try f.local(f.list);
    const result = try f.local(f.list);
    const mutate = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = result,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.store.addLocalSpan(&.{joined}),
        .next = try f.ret(result),
    } }, .test_fixture);
    var join_ids = body_clone.JoinParamIndex.init(allocator);
    defer join_ids.deinit();
    const join_id = join_ids.freshJoinPoint();
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } }, .test_fixture);
    const incoming = try f.store.addCFStmt(.{ .set_local = .{
        .target = joined,
        .value = param,
        .mode = .initialize_join_param,
        .next = jump,
    } }, .test_fixture);
    const body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{joined}),
        .body = mutate,
        .remainder = incoming,
    } }, .test_fixture);
    const proc = try f.proc(&.{param}, body, f.list);
    const rc = [_]bool{true} ** 3;
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    UniquenessOracleState.resetCapabilities(&solution);
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .none, &metrics);
    try std.testing.expectEqual(@as(u64, 1), metrics.components);
    try std.testing.expectEqual(@as(u64, 3), metrics.local_visits);
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.unique_seed_masks[@intFromEnum(proc)]);
    try std.testing.expectEqual(@as(arc_sig.ParamMask, 1), solution.unique_conds[@intFromEnum(joined)]);
}

test "uniqueness gives a tail loop parameter no field origins from its back edge alone" {
    const allocator = std.testing.allocator;
    var f = try UniquenessTest.init();
    defer f.deinit();
    // A tail-recursive procedure's parameter doubles as its loop's join
    // parameter: the entry edge carries the caller's value implicitly, and
    // only the back edge stores fresh fields into it.
    const param = try f.local(f.pair);
    const flag = try f.local(.u8);
    const taken = try f.local(f.list);
    const fresh = try f.local(f.list);
    const other = try f.local(f.list);
    const next = try f.local(f.pair);
    var join_ids = body_clone.JoinParamIndex.init(allocator);
    defer join_ids.deinit();
    const join_id = join_ids.freshJoinPoint();
    const read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = taken,
        .op = .{ .field = .{ .source = param, .field_idx = 0 } },
        .next = try f.ret(taken),
    } }, .test_fixture);
    const back_edge = try f.store.addCFStmt(.{ .set_local = .{
        .target = param,
        .value = next,
        .mode = .initialize_join_param,
        .next = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } }, .test_fixture),
    } }, .test_fixture);
    const rebuild = try f.store.addCFStmt(.{ .assign_list = .{
        .target = fresh,
        .elems = try f.store.addLocalSpan(&.{}),
        .next = try f.store.addCFStmt(.{ .assign_list = .{
            .target = other,
            .elems = try f.store.addLocalSpan(&.{}),
            .next = try f.store.addCFStmt(.{ .assign_struct = .{
                .target = next,
                .fields = try f.store.addLocalSpan(&.{ fresh, other }),
                .next = back_edge,
            } }, .test_fixture),
        } }, .test_fixture),
    } }, .test_fixture);
    const body = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = flag,
        .branches = try f.store.addCFSwitchBranches(&.{.{ .value = 1, .body = read }}),
        .default_branch = rebuild,
        .continuation = null,
    } }, .test_fixture);
    const loop = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{param}),
        .body = body,
        .remainder = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } }, .test_fixture),
    } }, .test_fixture);
    _ = try f.proc(&.{ param, flag }, loop, f.list);
    const rc = [_]bool{ true, false, true, true, true, true };
    var solution = try solve(allocator, &f.store, &f.layouts, &rc, &.{}, &.{}, true);
    defer solution.deinit();
    UniquenessOracleState.resetCapabilities(&solution);
    var takes = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, f.store.cfStmtCount());
    defer takes.deinit(allocator);
    takes.set(@intFromEnum(read));
    var metrics: UniquenessMetrics = .{};
    try UniquenessOracleState.compare(&f, &rc, &solution, .{ .set = &takes }, &metrics);
    // On the first iteration the field is whatever the caller stored, so a
    // take of it is not a unique birth under any seed.
    try std.testing.expect(!solution.unique_born.isSet(@intFromEnum(taken)));
}
