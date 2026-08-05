//! Elides always-safe checks by proving unsigned value-range facts.
//!
//! Fully checked Roc lowers each safety decision into ordinary LIR: a bounds
//! test is a comparison feeding a switch whose failing arm produces the error
//! value, and overflow-checked arithmetic is a `*_checked` low-level op that
//! the backends expand into an overflow branch plus a crash. When a dominating
//! branch already implies a check cannot fail (a decode loop's margin test
//! `cursor + 16 <= len` implies every eight-byte read at `cursor` is in
//! bounds), the check is pure overhead on every iteration.
//!
//! This pass proves such implications and rewrites only what it proves:
//!
//! - a comparison whose outcome is implied becomes a constant `Bool` tag
//! - a switch on a constant condition becomes its surviving branch
//! - a checked arithmetic op that cannot overflow becomes its wrapping form
//!
//! Anything the prover cannot justify keeps its checks, so the failure mode of
//! a weak proof is missing speedup, never unsoundness.
//!
//! Facts come from three sources. A branch edge asserts its condition: inside
//! the taken arm of `switch` on `a <= b`, that ordering holds. The
//! continuation of a surviving `*_checked` op asserts exactness: control only
//! reaches it when the operation did not wrap, so its result equals the
//! mathematical sum, which is precisely the no-overflow knowledge that plain
//! wrapping ops cannot carry. Bit operations assert constant ranges: masking
//! with a literal bounds the result by that literal.
//!
//! Soundness rests on dominance by construction. Facts and value bindings are
//! collected along single-predecessor statement chains and dropped at every
//! merge (any statement with more than one predecessor, including join bodies
//! entered by multiple jumps). A rewrite therefore only ever happens at a
//! statement dominated by every branch that contributed a fact to its proof.
//! Loop-carried join parameters get fresh unknown values in the loop body, so
//! only facts re-established inside the body (like a margin test re-checked
//! every iteration) apply to them. The pass runs proof rounds to a fixpoint
//! because folding a branch can leave a join body with a single remaining
//! jump, which lets facts flow through it on the next round.
//!
//! Only fixed-width unsigned integers up to 64 bits participate. List lengths
//! are modeled as opaque terms keyed by the list value they measure, so
//! repeated `list_len` reads of the same unmodified list unify.
//!
//! Short-circuit boolean conditions lower as a join whose single Bool
//! parameter feeds a switch: each operand arm writes the parameter and jumps,
//! and the merged value is re-tested. That merge would kill every fact a
//! condition establishes (a `while` loop's margin test lowers this way, with
//! the guarded loop body behind the re-test). The pass therefore threads such
//! joins before proving: the arm switch moves to each jump site, targeting
//! two new parameterless joins that wrap the original arms. Sites that wrote
//! a constant fold to direct jumps, and the site that wrote a real comparison
//! now branches on it directly, so its true edge dominates the guarded arm
//! and the comparison's facts flow there without any merge in between.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const CheckedArithmetic = core.CheckedArithmetic;
const GuardedList = LirStore.GuardedList;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const JoinPointId = LIR.JoinPointId;

/// Allocation failure raised while proving and rewriting.
pub const ResourceError = Allocator.Error;

/// Bound on proof rounds per proc. Each round can only fold branches that
/// exist, so rounds converge; this bound is a backstop, not a tuning knob.
const max_rounds: u32 = 8;
/// Bound on collected facts along one path.
const max_facts: usize = 512;
/// Bound on symbolic value nodes per proc round.
const max_nodes: usize = 1 << 14;
/// Bound on nodes touched by one inequality query.
const query_visit_cap: usize = 64;

/// Prove and rewrite qualifying checks in every proc.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var pass = Pass.init(store, layouts);
    defer pass.deinit();

    const proc_count = store.procSpecCount();
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        try pass.transformProc(@enumFromInt(proc_index));
    }
}

/// Identifier of one symbolic value node.
const NodeId = u32;

/// One symbolic value. A node is either a root (its own `root`, carrying
/// inclusive unsigned bounds in `lo`/`hi`) or a bounded affine offset from a
/// root: control reaching the defining statement guarantees
/// `root + off_lo <= value <= root + off_hi` with no wraparound. An exact
/// derivation has equal offsets; a cursor advanced by a masked amount keeps
/// its root with a widened offset window.
const Node = struct {
    root: NodeId,
    off_lo: i128,
    off_hi: i128,
    lo: i128,
    hi: i128,
};

/// Comparison kinds whose branch edges yield ordering facts.
const PredOp = enum { lt, lte, gt, gte };

/// The comparison that defined a Bool local, kept so a later switch on that
/// local can assert the comparison (or its negation) along each arm.
const Pred = struct {
    op: PredOp,
    a: NodeId,
    b: NodeId,
};

/// Symbolic knowledge about one local at one program point.
const Binding = struct {
    node: NodeId,
    pred: ?Pred = null,
};

/// Where an ordering fact's justification lives.
const FactOrigin = union(enum) {
    /// Asserted by taking one arm of this switch statement.
    branch: CFStmtId,
    /// Survived a merge meet whose incoming copies had differing origins.
    /// Its truth rests on the meet, not on a single dominating edge.
    meet,
};

/// One ordering fact between root nodes: `value(a) <= value(b) + c`.
const Fact = struct {
    a: NodeId,
    b: NodeId,
    c: i128,
    origin: FactOrigin,
};

/// Saved path-environment entry for backtracking.
const Undo = struct {
    local: LocalId,
    prev: ?Binding,
};

/// One pending single-predecessor walk continuation.
const Frame = struct {
    stmt: CFStmtId,
    facts_len: usize,
    undo_len: usize,
    /// Ordering fact asserted by the branch edge leading here, if any.
    edge_fact: ?Fact,
};

/// One jump statement and its target, collected during the pre-scan.
const JumpRecord = struct {
    target: JoinPointId,
    stmt: CFStmtId,
};

/// Debug-only record of one applied rewrite: the statement changed, the
/// root-level claim its proof established, and the path facts available when
/// it was proven. Certified independently at the end of the round.
const ProofRecord = struct {
    stmt: CFStmtId,
    claim_a: NodeId,
    claim_b: NodeId,
    claim_m: i128,
    facts_start: u32,
    facts_len: u32,
};

/// Meet of one local's value across a merge's incoming edges: every edge
/// binds the local within this window of the same root, or the meet is
/// invalid and the local starts unknown.
const EnvMeet = struct {
    local: LocalId,
    root: NodeId,
    off_lo: i128,
    off_hi: i128,
    valid: bool,
};

/// Bound on locals carried through one merge's environment meet.
const merge_env_cap: usize = 64;

/// Accumulated meet state of one merge head: the facts present on every
/// captured incoming edge, and the per-local value meet of the path
/// environment. A merge only seeds its region when every predecessor was
/// captured, so a missing edge (a loop back edge captured mid-walk, say)
/// keeps the merge at bottom for the round.
const MergeState = struct {
    captures: u32,
    facts: std.ArrayList(Fact),
    env: std.ArrayList(EnvMeet),
};

const Pass = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    allocator: Allocator,

    // Per-proc, per-round state. Reset by `resetRound`.
    nodes: std.ArrayList(Node),
    facts: std.ArrayList(Fact),
    global_env: std.AutoHashMap(LocalId, Binding),
    path_env: std.AutoHashMap(LocalId, Binding),
    undo: std.ArrayList(Undo),
    len_terms: std.AutoHashMap(NodeId, NodeId),
    assign_counts: std.AutoHashMap(LocalId, u32),
    pred_counts: std.AutoHashMap(CFStmtId, u32),
    jump_counts: std.AutoHashMap(JoinPointId, u32),
    join_stmts: std.AutoHashMap(JoinPointId, CFStmtId),
    visited: std.AutoHashMap(CFStmtId, void),
    region_seen: std.AutoHashMap(CFStmtId, void),
    regions: std.ArrayList(CFStmtId),
    frames: std.ArrayList(Frame),
    joins_in_order: std.ArrayList(CFStmtId),
    jump_records: std.ArrayList(JumpRecord),
    merge_states: std.AutoHashMap(CFStmtId, MergeState),
    body_joins: std.AutoHashMap(CFStmtId, JoinPointId),
    max_join_id: u32,
    scratch: std.ArrayList(CFStmtId),
    query_best: std.AutoHashMap(NodeId, i128),
    rewrites: u32,
    // Debug-only certification state; unused (and empty) in release builds.
    proof_records: std.ArrayList(ProofRecord),
    proof_facts: std.ArrayList(Fact),
    last_claim: ?struct { a: NodeId, b: NodeId, m: i128 },

    fn init(store: *LirStore, layouts: *const layout_mod.Store) Pass {
        const allocator = store.allocator;
        return .{
            .store = store,
            .layouts = layouts,
            .allocator = allocator,
            .nodes = .empty,
            .facts = .empty,
            .global_env = std.AutoHashMap(LocalId, Binding).init(allocator),
            .path_env = std.AutoHashMap(LocalId, Binding).init(allocator),
            .undo = .empty,
            .len_terms = std.AutoHashMap(NodeId, NodeId).init(allocator),
            .assign_counts = std.AutoHashMap(LocalId, u32).init(allocator),
            .pred_counts = std.AutoHashMap(CFStmtId, u32).init(allocator),
            .jump_counts = std.AutoHashMap(JoinPointId, u32).init(allocator),
            .join_stmts = std.AutoHashMap(JoinPointId, CFStmtId).init(allocator),
            .visited = std.AutoHashMap(CFStmtId, void).init(allocator),
            .region_seen = std.AutoHashMap(CFStmtId, void).init(allocator),
            .regions = .empty,
            .frames = .empty,
            .joins_in_order = .empty,
            .jump_records = .empty,
            .merge_states = std.AutoHashMap(CFStmtId, MergeState).init(allocator),
            .body_joins = std.AutoHashMap(CFStmtId, JoinPointId).init(allocator),
            .max_join_id = 0,
            .scratch = .empty,
            .query_best = std.AutoHashMap(NodeId, i128).init(allocator),
            .rewrites = 0,
            .proof_records = .empty,
            .proof_facts = .empty,
            .last_claim = null,
        };
    }

    fn deinit(self: *Pass) void {
        self.nodes.deinit(self.allocator);
        self.facts.deinit(self.allocator);
        self.global_env.deinit();
        self.path_env.deinit();
        self.undo.deinit(self.allocator);
        self.len_terms.deinit();
        self.assign_counts.deinit();
        self.pred_counts.deinit();
        self.jump_counts.deinit();
        self.join_stmts.deinit();
        self.visited.deinit();
        self.region_seen.deinit();
        self.regions.deinit(self.allocator);
        self.frames.deinit(self.allocator);
        self.joins_in_order.deinit(self.allocator);
        self.jump_records.deinit(self.allocator);
        self.clearMergeStates();
        self.merge_states.deinit();
        self.body_joins.deinit();
        self.scratch.deinit(self.allocator);
        self.query_best.deinit();
        self.proof_records.deinit(self.allocator);
        self.proof_facts.deinit(self.allocator);
    }

    fn resetRound(self: *Pass) void {
        self.nodes.clearRetainingCapacity();
        self.facts.clearRetainingCapacity();
        self.global_env.clearRetainingCapacity();
        self.path_env.clearRetainingCapacity();
        self.undo.clearRetainingCapacity();
        self.len_terms.clearRetainingCapacity();
        self.assign_counts.clearRetainingCapacity();
        self.pred_counts.clearRetainingCapacity();
        self.jump_counts.clearRetainingCapacity();
        self.join_stmts.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
        self.region_seen.clearRetainingCapacity();
        self.regions.clearRetainingCapacity();
        self.frames.clearRetainingCapacity();
        self.joins_in_order.clearRetainingCapacity();
        self.jump_records.clearRetainingCapacity();
        self.clearMergeStates();
        self.body_joins.clearRetainingCapacity();
        self.max_join_id = 0;
        self.scratch.clearRetainingCapacity();
        self.proof_records.clearRetainingCapacity();
        self.proof_facts.clearRetainingCapacity();
        self.last_claim = null;
        self.rewrites = 0;
    }

    // Layout helpers

    fn trackedIntMax(layout_idx: layout_mod.Idx) ?i128 {
        return switch (layout_idx) {
            .u8 => std.math.maxInt(u8),
            .u16 => std.math.maxInt(u16),
            .u32 => std.math.maxInt(u32),
            .u64 => std.math.maxInt(u64),
            else => null,
        };
    }

    fn localLayout(self: *const Pass, local: LocalId) layout_mod.Idx {
        return self.store.getLocal(local).layout_idx;
    }

    // Node table

    fn addNode(self: *Pass, node: Node) ResourceError!?NodeId {
        if (self.nodes.items.len >= max_nodes) return null;
        const id: NodeId = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return id;
    }

    fn freshRoot(self: *Pass, lo: i128, hi: i128) ResourceError!?NodeId {
        const id: NodeId = @intCast(self.nodes.items.len);
        if (self.nodes.items.len >= max_nodes) return null;
        try self.nodes.append(self.allocator, .{ .root = id, .off_lo = 0, .off_hi = 0, .lo = lo, .hi = hi });
        return id;
    }

    fn constNode(self: *Pass, value: i128) ResourceError!?NodeId {
        return self.freshRoot(value, value);
    }

    fn unknownFor(self: *Pass, layout_idx: layout_mod.Idx) ResourceError!?NodeId {
        const hi = trackedIntMax(layout_idx) orelse std.math.maxInt(u64);
        return self.freshRoot(0, hi);
    }

    /// Exact affine derivation: `value == base + delta` with no wraparound,
    /// justified by the caller (checked-op survival or a proven bound).
    fn derived(self: *Pass, base: NodeId, delta: i128) ResourceError!?NodeId {
        return self.derivedRange(base, delta, delta);
    }

    /// Bounded affine derivation: `base + dlo <= value <= base + dhi` with no
    /// wraparound, justified by the caller.
    fn derivedRange(self: *Pass, base: NodeId, dlo: i128, dhi: i128) ResourceError!?NodeId {
        const b = self.nodes.items[base];
        return self.addNode(.{
            .root = b.root,
            .off_lo = b.off_lo + dlo,
            .off_hi = b.off_hi + dhi,
            .lo = 0,
            .hi = 0,
        });
    }

    fn rootOf(self: *const Pass, id: NodeId) NodeId {
        return self.nodes.items[id].root;
    }

    fn offLoOf(self: *const Pass, id: NodeId) i128 {
        return self.nodes.items[id].off_lo;
    }

    fn offHiOf(self: *const Pass, id: NodeId) i128 {
        return self.nodes.items[id].off_hi;
    }

    /// Inclusive absolute bounds of a node's value.
    fn absLoOf(self: *const Pass, id: NodeId) i128 {
        const node = self.nodes.items[id];
        return self.nodes.items[node.root].lo + node.off_lo;
    }

    fn absHiOf(self: *const Pass, id: NodeId) i128 {
        const node = self.nodes.items[id];
        return self.nodes.items[node.root].hi + node.off_hi;
    }

    fn constValueOf(self: *const Pass, id: NodeId) ?i128 {
        const node = self.nodes.items[id];
        if (node.off_lo != node.off_hi) return null;
        const root = self.nodes.items[node.root];
        if (root.lo == root.hi) return root.lo + node.off_lo;
        return null;
    }

    // Fact base and inequality queries

    fn addFact(self: *Pass, fact: Fact) ResourceError!void {
        if (self.facts.items.len >= max_facts) return;
        try self.facts.append(self.allocator, fact);
    }

    /// Fact form of `value(a) <= value(b) + k`, normalized to roots. The
    /// widest offsets keep the root-level fact sound for any value in either
    /// node's window.
    fn orderingFact(self: *const Pass, a: NodeId, b: NodeId, k: i128, origin: FactOrigin) Fact {
        return .{
            .a = self.rootOf(a),
            .b = self.rootOf(b),
            .c = k + self.offHiOf(b) - self.offLoOf(a),
            .origin = origin,
        };
    }

    /// Tightest provable constant upper bound of a root node, following fact
    /// edges forward: from `r <= x + c` and a bound on `x`, `r` is bounded.
    fn hiConstOfRoot(self: *Pass, start: NodeId) ResourceError!i128 {
        self.query_best.clearRetainingCapacity();
        try self.query_best.put(start, 0);
        var best: i128 = self.nodes.items[start].hi;
        var steps: usize = 0;
        var changed = true;
        while (changed and steps < query_visit_cap) : (steps += 1) {
            changed = false;
            for (self.facts.items) |fact| {
                const acc = self.query_best.get(fact.a) orelse continue;
                const next_acc = acc + fact.c;
                const known = self.query_best.get(fact.b);
                if (known == null or next_acc < known.?) {
                    if (self.query_best.count() >= query_visit_cap and known == null) continue;
                    try self.query_best.put(fact.b, next_acc);
                    const through = self.nodes.items[fact.b].hi + next_acc;
                    if (through < best) best = through;
                    changed = true;
                }
            }
        }
        return best;
    }

    /// Tightest provable constant lower bound of a root node, following fact
    /// edges backward: from `x <= r + c` and a bound on `x`, `r` is bounded.
    fn loConstOfRoot(self: *Pass, start: NodeId) ResourceError!i128 {
        self.query_best.clearRetainingCapacity();
        try self.query_best.put(start, 0);
        var best: i128 = self.nodes.items[start].lo;
        var steps: usize = 0;
        var changed = true;
        while (changed and steps < query_visit_cap) : (steps += 1) {
            changed = false;
            for (self.facts.items) |fact| {
                const acc = self.query_best.get(fact.b) orelse continue;
                const next_acc = acc + fact.c;
                const known = self.query_best.get(fact.a);
                if (known == null or next_acc < known.?) {
                    if (self.query_best.count() >= query_visit_cap and known == null) continue;
                    try self.query_best.put(fact.a, next_acc);
                    const through = self.nodes.items[fact.a].lo - next_acc;
                    if (through > best) best = through;
                    changed = true;
                }
            }
        }
        return best;
    }

    /// Proves `value(a) <= value(b) + k`, or returns false when unprovable.
    /// The narrowest offsets make the root-level goal imply the node-level
    /// one for any value in either node's window.
    fn proveLe(self: *Pass, a: NodeId, b: NodeId, k: i128) ResourceError!bool {
        const ra = self.rootOf(a);
        const rb = self.rootOf(b);
        const m = k + self.offLoOf(b) - self.offHiOf(a);
        if (builtin.mode == .Debug) self.last_claim = .{ .a = ra, .b = rb, .m = m };
        if (ra == rb) return m >= 0;

        // Reach rb from ra along fact edges with accumulated slack <= m.
        self.query_best.clearRetainingCapacity();
        try self.query_best.put(ra, 0);
        var steps: usize = 0;
        var changed = true;
        while (changed and steps < query_visit_cap) : (steps += 1) {
            changed = false;
            for (self.facts.items) |fact| {
                const acc = self.query_best.get(fact.a) orelse continue;
                const next_acc = acc + fact.c;
                const known = self.query_best.get(fact.b);
                if (known == null or next_acc < known.?) {
                    if (self.query_best.count() >= query_visit_cap and known == null) continue;
                    try self.query_best.put(fact.b, next_acc);
                    changed = true;
                }
            }
        }
        if (self.query_best.get(rb)) |acc| {
            if (acc <= m) return true;
        }

        // Constant route: every value of ra is at most every value of rb + m.
        const hi_a = try self.hiConstOfRoot(ra);
        const lo_b = try self.loConstOfRoot(rb);
        return hi_a <= lo_b + m;
    }

    // Environments

    fn isSingleAssign(self: *const Pass, local: LocalId) bool {
        return (self.assign_counts.get(local) orelse 0) <= 1;
    }

    fn lookup(self: *const Pass, local: LocalId) ?Binding {
        if (self.isSingleAssign(local)) return self.global_env.get(local);
        return self.path_env.get(local);
    }

    fn bind(self: *Pass, local: LocalId, binding: Binding) ResourceError!void {
        if (self.isSingleAssign(local)) {
            try self.global_env.put(local, binding);
            return;
        }
        const prev = try self.path_env.fetchPut(local, binding);
        try self.undo.append(self.allocator, .{
            .local = local,
            .prev = if (prev) |entry| entry.value else null,
        });
    }

    /// Value node for a local, materializing and binding a fresh root the
    /// first time an unbound local is read so later reads of the same
    /// unchanged local unify with it. The binding is path-scoped for
    /// reassignable locals, so it never outlives the value it names.
    fn valueOf(self: *Pass, local: LocalId) ResourceError!?NodeId {
        if (self.lookup(local)) |binding| return binding.node;
        const node = (try self.unknownFor(self.localLayout(local))) orelse return null;
        try self.bind(local, .{ .node = node });
        return node;
    }

    fn bindFresh(self: *Pass, local: LocalId) ResourceError!void {
        const node = (try self.unknownFor(self.localLayout(local))) orelse return;
        try self.bind(local, .{ .node = node });
    }

    fn rewindTo(self: *Pass, facts_len: usize, undo_len: usize) ResourceError!void {
        self.facts.shrinkRetainingCapacity(facts_len);
        while (self.undo.items.len > undo_len) {
            const entry = self.undo.pop().?;
            if (entry.prev) |prev| {
                try self.path_env.put(entry.local, prev);
            } else {
                _ = self.path_env.remove(entry.local);
            }
        }
    }

    // Pre-scan: predecessor counts, jump counts, and assignment counts.

    fn prescanProc(self: *Pass, proc: LIR.LirProcSpec) ResourceError!void {
        const args = self.store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(args)) |i| {
            try self.bumpAssign(GuardedList.at(args, i));
        }

        self.scratch.clearRetainingCapacity();
        var seen = std.AutoHashMap(CFStmtId, void).init(self.allocator);
        defer seen.deinit();

        try self.scratch.append(self.allocator, proc.body.?);
        try self.bumpPred(proc.body.?);
        while (self.scratch.pop()) |current| {
            if (seen.contains(current)) continue;
            try seen.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .init_uninitialized => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_ref => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_literal => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_call => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_call_erased => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_packed_erased_fn => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_low_level => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_list => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_struct => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_tag => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .store_struct => |s| {
                    try self.bumpAssign(s.dest);
                    try self.edgeTo(s.next);
                },
                .store_tag => |s| {
                    try self.bumpAssign(s.dest);
                    try self.edgeTo(s.next);
                },
                .set_local => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .debug => |s| try self.edgeTo(s.next),
                .expect => |s| try self.edgeTo(s.next),
                .comptime_branch_taken => |s| try self.edgeTo(s.next),
                .incref => |s| try self.edgeTo(s.next),
                .decref => |s| try self.edgeTo(s.next),
                .decref_if_initialized => |s| try self.edgeTo(s.next),
                .free => |s| try self.edgeTo(s.next),
                .switch_stmt => |s| {
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..GuardedList.borrowLen(branches)) |i| {
                        try self.edgeTo(GuardedList.at(branches, i).body);
                    }
                    try self.edgeTo(s.default_branch);
                    // The continuation is release-placement metadata, not a
                    // control edge: the arms flow into it through their own
                    // chains, which are the edges counted here.
                },
                .switch_initialized_payload => |s| {
                    try self.edgeTo(s.initialized_branch);
                    try self.edgeTo(s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.edgeTo(s.on_match);
                    try self.edgeTo(s.on_miss);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..GuardedList.borrowLen(arms)) |i| {
                        try self.edgeTo(GuardedList.at(arms, i).on_match);
                    }
                    try self.edgeTo(s.on_miss);
                },
                .join => |s| {
                    try self.join_stmts.put(s.id, current);
                    try self.joins_in_order.append(self.allocator, current);
                    if (@intFromEnum(s.id) + 1 > self.max_join_id) {
                        self.max_join_id = @intFromEnum(s.id) + 1;
                    }
                    try self.edgeTo(s.remainder);
                    // The body is only entered through jumps, so it is only
                    // scanned once a reachable jump to it appears. Scanning it
                    // eagerly would let jumps inside dead arms inflate the
                    // predecessor counts of live statements.
                },
                .jump => |s| {
                    const count = try self.jump_counts.getOrPut(s.target);
                    if (!count.found_existing) count.value_ptr.* = 0;
                    count.value_ptr.* += 1;
                    try self.jump_records.append(self.allocator, .{ .target = s.target, .stmt = current });
                    // The join definition dominates its jumps, so its body is
                    // already known by the time the first jump appears.
                    if (count.value_ptr.* == 1) {
                        if (self.join_stmts.get(s.target)) |join_stmt| {
                            const body = self.store.getCFStmt(join_stmt).join.body;
                            try self.body_joins.put(body, s.target);
                            if (!seen.contains(body)) try self.scratch.append(self.allocator, body);
                        }
                    }
                },
                .ret, .crash, .runtime_error, .expect_err, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
    }

    fn edgeTo(self: *Pass, stmt: CFStmtId) ResourceError!void {
        try self.bumpPred(stmt);
        try self.scratch.append(self.allocator, stmt);
    }

    fn bumpPred(self: *Pass, stmt: CFStmtId) ResourceError!void {
        const entry = try self.pred_counts.getOrPut(stmt);
        if (!entry.found_existing) entry.value_ptr.* = 0;
        entry.value_ptr.* += 1;
    }

    fn bumpAssign(self: *Pass, local: LocalId) ResourceError!void {
        const entry = try self.assign_counts.getOrPut(local);
        if (!entry.found_existing) entry.value_ptr.* = 0;
        entry.value_ptr.* += 1;
    }

    fn predCount(self: *const Pass, stmt: CFStmtId) u32 {
        return self.pred_counts.get(stmt) orelse 0;
    }

    fn clearMergeStates(self: *Pass) void {
        var it = self.merge_states.valueIterator();
        while (it.next()) |state| {
            state.facts.deinit(self.allocator);
            state.env.deinit(self.allocator);
        }
        self.merge_states.clearRetainingCapacity();
    }

    /// A binding is worth carrying through a merge when it says something a
    /// fresh unknown would not: a derived offset window, a narrowed root
    /// range, or a root some collected fact mentions. Plain temporaries fail
    /// all three, which keeps merge meets small in large procs.
    fn captureWorthy(self: *const Pass, node_id: NodeId) bool {
        const node = self.nodes.items[node_id];
        if (node.root != node_id) return true;
        if (node.lo != 0 or node.hi != std.math.maxInt(u64)) return true;
        for (self.facts.items) |fact| {
            if (fact.a == node.root or fact.b == node.root) return true;
        }
        return false;
    }

    /// Capture the current path state into a merge head's meet: facts keep
    /// only what every captured edge established, and each path-bound local
    /// keeps a common root with a widened offset window.
    fn captureMergeEdge(self: *Pass, head: CFStmtId) ResourceError!void {
        const entry = try self.merge_states.getOrPut(head);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{ .captures = 0, .facts = .empty, .env = .empty };
        }
        const state = entry.value_ptr;

        if (state.captures == 0) {
            try state.facts.appendSlice(self.allocator, self.facts.items);
            var it = self.path_env.iterator();
            while (it.next()) |kv| {
                if (state.env.items.len >= merge_env_cap) break;
                if (!self.captureWorthy(kv.value_ptr.node)) continue;
                const node = self.nodes.items[kv.value_ptr.node];
                try state.env.append(self.allocator, .{
                    .local = kv.key_ptr.*,
                    .root = node.root,
                    .off_lo = node.off_lo,
                    .off_hi = node.off_hi,
                    .valid = true,
                });
            }
        } else {
            // Intersect facts: keep only entries this edge also carries.
            var keep: usize = 0;
            for (state.facts.items) |fact| {
                var present = false;
                var same_origin = false;
                for (self.facts.items) |mine| {
                    if (mine.a == fact.a and mine.b == fact.b and mine.c == fact.c) {
                        present = true;
                        same_origin = std.meta.eql(mine.origin, fact.origin);
                        break;
                    }
                }
                if (present) {
                    state.facts.items[keep] = fact;
                    if (!same_origin) state.facts.items[keep].origin = .meet;
                    keep += 1;
                }
            }
            state.facts.shrinkRetainingCapacity(keep);

            for (state.env.items) |*meet| {
                if (!meet.valid) continue;
                const binding = self.path_env.get(meet.local);
                if (binding) |b| {
                    const node = self.nodes.items[b.node];
                    if (node.root == meet.root) {
                        meet.off_lo = @min(meet.off_lo, node.off_lo);
                        meet.off_hi = @max(meet.off_hi, node.off_hi);
                    } else {
                        meet.valid = false;
                    }
                } else {
                    meet.valid = false;
                }
            }
        }
        state.captures += 1;
    }

    /// Expected incoming edge count of a merge head: jumps for a join body,
    /// counted predecessors otherwise.
    fn mergeExpected(self: *const Pass, head: CFStmtId) u32 {
        if (self.body_joins.get(head)) |id| return self.jumpCount(id);
        return self.predCount(head);
    }

    fn mergeIncomplete(self: *const Pass, head: CFStmtId) bool {
        const expected = self.mergeExpected(head);
        if (expected <= 1) return false;
        const state = self.merge_states.get(head) orelse return true;
        return state.captures < expected;
    }

    /// Seed a merge-head region from its meet when every incoming edge was
    /// captured this round. Facts hold because they were present on all
    /// edges; met locals bind to their windows.
    fn seedFromMerge(self: *Pass, head: CFStmtId) ResourceError!void {
        const state = self.merge_states.getPtr(head) orelse return;
        const expected = self.mergeExpected(head);
        if (state.captures != expected or expected == 0) return;

        try self.facts.appendSlice(self.allocator, state.facts.items);

        for (state.env.items) |meet| {
            if (!meet.valid) continue;
            const node = (try self.addNode(.{
                .root = meet.root,
                .off_lo = meet.off_lo,
                .off_hi = meet.off_hi,
                .lo = 0,
                .hi = 0,
            })) orelse continue;
            try self.bind(meet.local, .{ .node = node });
        }
    }

    /// Thread joins whose single Bool parameter is immediately re-tested by
    /// their body. Every jump site becomes a switch on the parameter targeting
    /// two fresh parameterless joins that wrap the original arms, so each
    /// site's own knowledge of the parameter reaches the arms directly.
    /// Returns the number of joins threaded.
    fn threadBoolJoins(self: *Pass) ResourceError!u32 {
        var threaded: u32 = 0;
        for (self.joins_in_order.items) |join_stmt| {
            const join = switch (self.store.getCFStmt(join_stmt)) {
                .join => |j| j,
                else => continue,
            };
            const params = self.store.getLocalSpan(join.params);
            if (GuardedList.borrowLen(params) != 1) continue;
            if (!join.maybe_uninitialized_params.isEmpty()) continue;
            const param = GuardedList.at(params, 0);
            if (self.localLayout(param) != .bool) continue;

            const body_switch = switch (self.store.getCFStmt(join.body)) {
                .switch_stmt => |sw| sw,
                else => continue,
            };
            if (body_switch.cond != param) continue;

            // Resolve the true and false arms from the Bool switch shape.
            var true_arm = body_switch.default_branch;
            var false_arm = body_switch.default_branch;
            var shape_ok = true;
            const branches = self.store.getCFSwitchBranches(body_switch.branches);
            for (0..GuardedList.borrowLen(branches)) |i| {
                const branch = GuardedList.at(branches, i);
                switch (branch.value) {
                    0 => false_arm = branch.body,
                    1 => true_arm = branch.body,
                    else => shape_ok = false,
                }
            }
            if (!shape_ok) continue;

            const true_id: JoinPointId = @enumFromInt(self.max_join_id);
            const false_id: JoinPointId = @enumFromInt(self.max_join_id + 1);
            self.max_join_id += 2;

            const empty_params = try self.store.addLocalSpan(&.{});
            const false_join = try self.store.addCFStmt(.{ .join = .{
                .id = false_id,
                .params = empty_params,
                .body = false_arm,
                .remainder = join.remainder,
            } });
            self.store.getCFStmtPtr(join_stmt).* = .{ .join = .{
                .id = true_id,
                .params = empty_params,
                .body = true_arm,
                .remainder = false_join,
            } };

            for (self.jump_records.items) |record| {
                if (record.target != join.id) continue;
                // A site already rewritten for an earlier join no longer
                // holds a jump; skip anything that changed shape.
                switch (self.store.getCFStmt(record.stmt)) {
                    .jump => |jump| if (jump.target != join.id) continue,
                    else => continue,
                }
                const true_jump = try self.store.addCFStmt(.{ .jump = .{ .target = true_id } });
                const false_jump = try self.store.addCFStmt(.{ .jump = .{ .target = false_id } });
                const site_branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = true_jump }});
                self.store.getCFStmtPtr(record.stmt).* = .{ .switch_stmt = .{
                    .cond = param,
                    .branches = site_branches,
                    .default_branch = false_jump,
                    .continuation = null,
                } };
            }

            threaded += 1;
            self.rewrites += 1;
        }
        return threaded;
    }

    fn jumpCount(self: *const Pass, id: JoinPointId) u32 {
        return self.jump_counts.get(id) orelse 0;
    }

    /// Debug-only: snapshot the deciding proof of a rewrite for independent
    /// certification at the end of the round.
    fn recordProof(self: *Pass, stmt: CFStmtId) ResourceError!void {
        if (builtin.mode != .Debug) return;
        const claim = self.last_claim orelse return;
        const start: u32 = @intCast(self.proof_facts.items.len);
        try self.proof_facts.appendSlice(self.allocator, self.facts.items);
        try self.proof_records.append(self.allocator, .{
            .stmt = stmt,
            .claim_a = claim.a,
            .claim_b = claim.b,
            .claim_m = claim.m,
            .facts_start = start,
            .facts_len = @intCast(self.facts.items.len),
        });
    }

    /// Debug-only certification of every rewrite the round applied, using
    /// machinery independent of the prover's walk: an iterative dominator
    /// computation over the statement graph checks that each branch-origin
    /// fact's switch dominates the rewritten statement, and a transitive
    /// closure over the snapshot facts re-derives the claim. A failure is a
    /// compiler bug in the pass, never a property of the compiled program.
    fn certifyRound(self: *Pass, body: CFStmtId) ResourceError!void {
        if (builtin.mode != .Debug) return;
        if (self.proof_records.items.len == 0) return;

        var doms = try RangeProveCertify.dominators(self.allocator, self.store, body);
        defer doms.deinit();

        for (self.proof_records.items) |record| {
            const facts = self.proof_facts.items[record.facts_start..][0..record.facts_len];
            for (facts) |fact| {
                switch (fact.origin) {
                    .branch => |origin_stmt| {
                        if (!doms.dominates(origin_stmt, record.stmt)) {
                            std.debug.panic(
                                "range_prove certification failed: fact from s{d} does not dominate rewritten s{d}",
                                .{ @intFromEnum(origin_stmt), @intFromEnum(record.stmt) },
                            );
                        }
                    },
                    .meet => {},
                }
            }
            if (!RangeProveCertify.implies(self.allocator, facts, self.nodes.items, record.claim_a, record.claim_b, record.claim_m)) {
                std.debug.panic(
                    "range_prove certification failed: claim at s{d} does not follow from its facts",
                    .{@intFromEnum(record.stmt)},
                );
            }
        }
    }

    // Proc driver

    fn transformProc(self: *Pass, proc_id: LIR.LirProcSpecId) ResourceError!void {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body == null or proc.hosted != null) return;

        var round: u32 = 0;
        while (round < max_rounds) : (round += 1) {
            self.resetRound();
            try self.prescanProc(proc);
            // Threading restructures control flow, so a round that threads
            // stops there and the next round re-derives the graph facts.
            if (try self.threadBoolJoins() == 0) {
                try self.walkRegions(proc.body.?);
                try self.certifyRound(proc.body.?);
            }
            if (self.rewrites == 0) return;
        }
    }

    fn enqueueRegion(self: *Pass, head: CFStmtId) ResourceError!void {
        if (self.region_seen.contains(head)) return;
        try self.region_seen.put(head, {});
        try self.regions.append(self.allocator, head);
    }

    fn walkRegions(self: *Pass, body: CFStmtId) ResourceError!void {
        try self.enqueueRegion(body);
        var region_index: usize = 0;
        var defer_streak: usize = 0;
        while (region_index < self.regions.items.len) : (region_index += 1) {
            const head = self.regions.items[region_index];
            // A merge walked before all its incoming edges are captured
            // would start at bottom even though the missing edges are in
            // regions still queued. Defer it to the back until its captures
            // complete. The streak is measured against the pending count so
            // a full cycle of deferrals with no progress forces the next
            // head to walk; a head that can never complete (a loop body
            // waiting on its own back edge) proceeds at bottom that way.
            const pending = self.regions.items.len - region_index;
            if (self.mergeIncomplete(head) and defer_streak <= pending) {
                try self.regions.append(self.allocator, head);
                defer_streak += 1;
                continue;
            }
            defer_streak = 0;
            self.path_env.clearRetainingCapacity();
            self.undo.clearRetainingCapacity();
            self.facts.clearRetainingCapacity();
            self.frames.clearRetainingCapacity();
            try self.seedFromMerge(head);
            try self.frames.append(self.allocator, .{
                .stmt = head,
                .facts_len = self.facts.items.len,
                .undo_len = self.undo.items.len,
                .edge_fact = null,
            });
            try self.walkRegion(head);
        }
    }

    fn walkRegion(self: *Pass, head: CFStmtId) ResourceError!void {
        while (self.frames.pop()) |frame| {
            try self.rewindTo(frame.facts_len, frame.undo_len);
            if (frame.edge_fact) |fact| try self.addFact(fact);

            var current = frame.stmt;
            walk: while (true) {
                if (current != head and self.predCount(current) > 1) {
                    // Merge point: only facts held by every incoming edge may
                    // cross, so capture this edge into the merge's meet and
                    // let the merge head start its own region.
                    try self.captureMergeEdge(current);
                    try self.enqueueRegion(current);
                    break :walk;
                }
                if (self.visited.contains(current)) break :walk;

                switch (self.store.getCFStmt(current)) {
                    .assign_ref => |s| {
                        try self.visited.put(current, {});
                        switch (s.op) {
                            .local => |src| {
                                if (try self.valueOf(src)) |node| {
                                    try self.bind(s.target, .{ .node = node, .pred = if (self.lookup(src)) |b| b.pred else null });
                                } else {
                                    try self.bindFresh(s.target);
                                }
                            },
                            else => try self.bindFresh(s.target),
                        }
                        current = s.next;
                    },
                    .assign_literal => |s| {
                        try self.visited.put(current, {});
                        try self.modelLiteral(s.target, s.value);
                        current = s.next;
                    },
                    .assign_tag => |s| {
                        try self.visited.put(current, {});
                        if (self.localLayout(s.target) == .bool and s.payload == null) {
                            if (try self.constNode(s.discriminant)) |node| {
                                try self.bind(s.target, .{ .node = node });
                            } else {
                                try self.bindFresh(s.target);
                            }
                        } else {
                            try self.bindFresh(s.target);
                        }
                        current = s.next;
                    },
                    .assign_low_level => |s| {
                        try self.visited.put(current, {});
                        // A compare may be rewritten to a constant tag in
                        // place; `s` is a pre-rewrite copy, so its `next`
                        // stays valid either way.
                        try self.modelLowLevel(current, s);
                        current = s.next;
                    },
                    .set_local => |s| {
                        try self.visited.put(current, {});
                        if (try self.valueOf(s.value)) |node| {
                            try self.bind(s.target, .{ .node = node, .pred = if (self.lookup(s.value)) |b| b.pred else null });
                        } else {
                            try self.bindFresh(s.target);
                        }
                        current = s.next;
                    },
                    .init_uninitialized => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_call => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_call_erased => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_packed_erased_fn => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_list => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_struct => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .store_struct => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.dest);
                        current = s.next;
                    },
                    .store_tag => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.dest);
                        current = s.next;
                    },
                    .debug => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .expect => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .comptime_branch_taken => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .incref => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .decref => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .decref_if_initialized => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .free => |s| {
                        try self.visited.put(current, {});
                        current = s.next;
                    },
                    .switch_stmt => |s| {
                        if (try self.foldSwitch(current, s)) {
                            // The statement now holds the surviving branch's
                            // content; reinterpret it under the same facts.
                            continue :walk;
                        }
                        try self.visited.put(current, {});
                        try self.pushSwitchArms(s, current);
                        break :walk;
                    },
                    .switch_initialized_payload => |s| {
                        try self.visited.put(current, {});
                        try self.pushFrame(s.initialized_branch, null);
                        try self.pushFrame(s.uninitialized_branch, null);
                        break :walk;
                    },
                    .str_match => |s| {
                        try self.visited.put(current, {});
                        try self.pushFrame(s.on_match, null);
                        try self.pushFrame(s.on_miss, null);
                        break :walk;
                    },
                    .str_match_set => |s| {
                        try self.visited.put(current, {});
                        const arms = self.store.getStrMatchArms(s.arms);
                        for (0..GuardedList.borrowLen(arms)) |i| {
                            try self.pushFrame(GuardedList.at(arms, i).on_match, null);
                        }
                        try self.pushFrame(s.on_miss, null);
                        break :walk;
                    },
                    .join => |s| {
                        try self.visited.put(current, {});
                        if (self.jumpCount(s.id) > 1) {
                            try self.enqueueRegion(s.body);
                        }
                        current = s.remainder;
                    },
                    .jump => |s| {
                        try self.visited.put(current, {});
                        if (self.join_stmts.get(s.target)) |join_stmt| {
                            const join = self.store.getCFStmt(join_stmt).join;
                            if (self.jumpCount(s.target) == 1) {
                                if (!self.visited.contains(join.body)) {
                                    // Sole entry into the join body: bindings
                                    // for its freshly written parameters and
                                    // the path facts flow through.
                                    current = join.body;
                                    continue :walk;
                                }
                            } else {
                                try self.captureMergeEdge(join.body);
                            }
                        }
                        break :walk;
                    },
                    .ret, .crash, .runtime_error, .expect_err, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {
                        try self.visited.put(current, {});
                        break :walk;
                    },
                }
            }
        }
    }

    fn pushFrame(self: *Pass, stmt: CFStmtId, edge_fact: ?Fact) ResourceError!void {
        try self.frames.append(self.allocator, .{
            .stmt = stmt,
            .facts_len = self.facts.items.len,
            .undo_len = self.undo.items.len,
            .edge_fact = edge_fact,
        });
    }

    /// Push switch arms, asserting the condition's comparison along Bool
    /// edges: the `1` arm asserts it and the `0`/default arm asserts its
    /// negation.
    fn pushSwitchArms(self: *Pass, s: anytype, switch_stmt: CFStmtId) ResourceError!void {
        const pred: ?Pred = if (self.lookup(s.cond)) |binding| binding.pred else null;
        const cond_is_bool = self.localLayout(s.cond) == .bool;

        const branches = self.store.getCFSwitchBranches(s.branches);
        const branch_count = GuardedList.borrowLen(branches);

        var default_fact: ?Fact = null;
        if (pred != null and cond_is_bool and branch_count == 1) {
            const only = GuardedList.at(branches, 0);
            if (only.value == 1) {
                default_fact = self.predFact(pred.?, false, switch_stmt);
            } else if (only.value == 0) {
                default_fact = self.predFact(pred.?, true, switch_stmt);
            }
        }
        try self.pushFrame(s.default_branch, default_fact);

        for (0..branch_count) |i| {
            const branch = GuardedList.at(branches, i);
            var edge_fact: ?Fact = null;
            if (pred != null and cond_is_bool) {
                if (branch.value == 1) {
                    edge_fact = self.predFact(pred.?, true, switch_stmt);
                } else if (branch.value == 0) {
                    edge_fact = self.predFact(pred.?, false, switch_stmt);
                }
            }
            try self.pushFrame(branch.body, edge_fact);
        }
    }

    /// Ordering fact asserted when a comparison holds (or fails, for the
    /// negated edge). Unsigned only: `a < b` failing means `b <= a`.
    fn predFact(self: *const Pass, pred: Pred, holds: bool, switch_stmt: CFStmtId) Fact {
        const origin = FactOrigin{ .branch = switch_stmt };
        return switch (pred.op) {
            .lt => if (holds)
                self.orderingFact(pred.a, pred.b, -1, origin)
            else
                self.orderingFact(pred.b, pred.a, 0, origin),
            .lte => if (holds)
                self.orderingFact(pred.a, pred.b, 0, origin)
            else
                self.orderingFact(pred.b, pred.a, -1, origin),
            .gt => if (holds)
                self.orderingFact(pred.b, pred.a, -1, origin)
            else
                self.orderingFact(pred.a, pred.b, 0, origin),
            .gte => if (holds)
                self.orderingFact(pred.b, pred.a, 0, origin)
            else
                self.orderingFact(pred.a, pred.b, -1, origin),
        };
    }

    /// Fold a switch whose condition is a known constant, splicing the
    /// surviving branch's first statement over the switch. Returns whether a
    /// fold happened.
    fn foldSwitch(self: *Pass, stmt: CFStmtId, s: anytype) ResourceError!bool {
        const binding = self.lookup(s.cond) orelse return false;
        const value = self.constValueOf(binding.node) orelse return false;
        if (value < 0) return false;

        var survivor = s.default_branch;
        const branches = self.store.getCFSwitchBranches(s.branches);
        for (0..GuardedList.borrowLen(branches)) |i| {
            const branch = GuardedList.at(branches, i);
            if (branch.value == value) {
                survivor = branch.body;
                break;
            }
        }
        const replacement = self.store.getCFStmt(survivor);
        self.store.getCFStmtPtr(stmt).* = replacement;
        self.rewrites += 1;
        return true;
    }

    // Statement value modeling and check rewrites

    fn modelLiteral(self: *Pass, target: LocalId, value: LIR.LiteralValue) ResourceError!void {
        const literal: ?i128 = switch (value) {
            .i64_literal => |lit| if (lit.value >= 0) lit.value else null,
            .i128_literal => |lit| if (lit.value >= 0) lit.value else null,
            else => null,
        };
        if (literal) |v| {
            if (trackedIntMax(self.localLayout(target)) != null) {
                if (try self.constNode(v)) |node| {
                    try self.bind(target, .{ .node = node });
                    return;
                }
            }
        }
        try self.bindFresh(target);
    }

    fn modelLowLevel(self: *Pass, stmt: CFStmtId, s: anytype) ResourceError!void {
        const args = self.store.getLocalSpan(s.args);
        const arg_count = GuardedList.borrowLen(args);

        switch (s.op) {
            .list_len => {
                if (arg_count == 1) {
                    if (try self.valueOf(GuardedList.at(args, 0))) |list_node| {
                        const root = self.rootOf(list_node);
                        if (self.len_terms.get(root)) |len_node| {
                            try self.bind(s.target, .{ .node = len_node });
                            return;
                        }
                        // List lengths fit a signed 64-bit count.
                        if (try self.freshRoot(0, std.math.maxInt(i64))) |len_node| {
                            try self.len_terms.put(root, len_node);
                            try self.bind(s.target, .{ .node = len_node });
                            return;
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            .num_is_lt, .num_is_lte, .num_is_gt, .num_is_gte => {
                try self.modelCompare(stmt, s, args, arg_count);
            },
            .num_plus_checked, .num_minus_checked => {
                try self.modelCheckedArith(stmt, s, args, arg_count);
            },
            .num_plus => {
                if (try self.wrapAddExact(args, arg_count, self.localLayout(s.target))) |node| {
                    try self.bind(s.target, .{ .node = node });
                } else {
                    try self.bindFresh(s.target);
                }
            },
            .num_minus => {
                if (try self.wrapSubExact(args, arg_count)) |node| {
                    try self.bind(s.target, .{ .node = node });
                } else {
                    try self.bindFresh(s.target);
                }
            },
            .num_bitwise_and => {
                var mask: ?i128 = null;
                if (arg_count == 2) {
                    for (0..2) |i| {
                        if (try self.valueOf(GuardedList.at(args, i))) |node| {
                            if (self.constValueOf(node)) |v| {
                                if (v >= 0 and (mask == null or v < mask.?)) mask = v;
                            }
                        }
                    }
                }
                if (mask) |m| {
                    if (try self.freshRoot(0, m)) |node| {
                        try self.bind(s.target, .{ .node = node });
                        return;
                    }
                }
                try self.bindFresh(s.target);
            },
            .num_shift_right_zf_by => {
                if (arg_count == 2) {
                    if (try self.valueOf(GuardedList.at(args, 1))) |amount_node| {
                        if (self.constValueOf(amount_node)) |amount| {
                            const max = trackedIntMax(self.localLayout(s.target));
                            if (max != null and amount >= 0 and amount < 64) {
                                const shifted = max.? >> @intCast(amount);
                                if (try self.freshRoot(0, shifted)) |node| {
                                    try self.bind(s.target, .{ .node = node });
                                    return;
                                }
                            }
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            else => try self.bindFresh(s.target),
        }
    }

    fn modelCompare(self: *Pass, stmt: CFStmtId, s: anytype, args: anytype, arg_count: usize) ResourceError!void {
        if (arg_count != 2) {
            try self.bindFresh(s.target);
            return;
        }
        const lhs_local = GuardedList.at(args, 0);
        if (trackedIntMax(self.localLayout(lhs_local)) == null) {
            try self.bindFresh(s.target);
            return;
        }
        const a = (try self.valueOf(lhs_local)) orelse {
            try self.bindFresh(s.target);
            return;
        };
        const b = (try self.valueOf(GuardedList.at(args, 1))) orelse {
            try self.bindFresh(s.target);
            return;
        };
        const op: PredOp = switch (s.op) {
            .num_is_lt => .lt,
            .num_is_lte => .lte,
            .num_is_gt => .gt,
            .num_is_gte => .gte,
            else => unreachable,
        };

        // `a < b` holds when `a <= b - 1`; it fails when `b <= a`. The
        // remaining kinds reduce to those two shapes.
        const holds = switch (op) {
            .lt => try self.proveLe(a, b, -1),
            .lte => try self.proveLe(a, b, 0),
            .gt => try self.proveLe(b, a, -1),
            .gte => try self.proveLe(b, a, 0),
        };
        const fails = if (holds) false else switch (op) {
            .lt => try self.proveLe(b, a, 0),
            .lte => try self.proveLe(b, a, -1),
            .gt => try self.proveLe(a, b, 0),
            .gte => try self.proveLe(a, b, -1),
        };

        if (holds or fails) {
            try self.recordProof(stmt);
            const truth: u16 = if (holds) 1 else 0;
            self.store.getCFStmtPtr(stmt).* = .{ .assign_tag = .{
                .target = s.target,
                .variant_index = truth,
                .discriminant = truth,
                .payload = null,
                .next = s.next,
            } };
            self.rewrites += 1;
            if (try self.constNode(truth)) |node| {
                try self.bind(s.target, .{ .node = node });
            } else {
                try self.bindFresh(s.target);
            }
            return;
        }

        const bool_node = (try self.freshRoot(0, 1)) orelse {
            try self.bindFresh(s.target);
            return;
        };
        try self.bind(s.target, .{ .node = bool_node, .pred = .{ .op = op, .a = a, .b = b } });
    }

    fn modelCheckedArith(self: *Pass, stmt: CFStmtId, s: anytype, args: anytype, arg_count: usize) ResourceError!void {
        const target_layout = self.localLayout(s.target);
        const max = trackedIntMax(target_layout);
        if (arg_count != 2 or max == null) {
            try self.bindFresh(s.target);
            return;
        }
        const lhs = (try self.valueOf(GuardedList.at(args, 0))) orelse {
            try self.bindFresh(s.target);
            return;
        };
        const rhs = (try self.valueOf(GuardedList.at(args, 1))) orelse {
            try self.bindFresh(s.target);
            return;
        };

        var provable = false;
        var result: ?NodeId = null;
        switch (s.op) {
            .num_plus_checked => {
                if (self.constValueOf(rhs)) |c| {
                    if (c >= 0) {
                        // Never wraps when lhs stays at most max - c.
                        if (try self.constNode(max.? - c)) |limit| {
                            provable = try self.proveLe(lhs, limit, 0);
                        }
                        // Control past a surviving checked add proves the sum
                        // is exact regardless of whether it can be rewritten.
                        result = try self.derived(lhs, c);
                    }
                } else if (self.constValueOf(lhs)) |c| {
                    if (c >= 0) {
                        if (try self.constNode(max.? - c)) |limit| {
                            provable = try self.proveLe(rhs, limit, 0);
                        }
                        result = try self.derived(rhs, c);
                    }
                }
            },
            .num_minus_checked => {
                // Never wraps when rhs stays at most lhs.
                provable = try self.proveLe(rhs, lhs, 0);
                // Control past a surviving checked subtract proves the
                // difference is exact, so the result stays on lhs's root
                // with a window widened by rhs's absolute bounds.
                const rhs_lo = self.absLoOf(rhs);
                const rhs_hi = self.absHiOf(rhs);
                if (rhs_lo >= 0) {
                    result = try self.derivedRange(lhs, -rhs_hi, -rhs_lo);
                }
            },
            else => unreachable,
        }

        if (provable) {
            if (CheckedArithmetic.uncheckedOp(s.op)) |unchecked| {
                try self.recordProof(stmt);
                const ptr = &self.store.getCFStmtPtr(stmt).assign_low_level;
                ptr.op = unchecked;
                ptr.rc_effect = unchecked.rcEffect();
                self.rewrites += 1;
            }
        }

        if (result) |node| {
            try self.bind(s.target, .{ .node = node });
        } else {
            try self.bindFresh(s.target);
        }
    }

    /// Exact node for a wrapping add proven not to wrap, or null.
    fn wrapAddExact(self: *Pass, args: anytype, arg_count: usize, target_layout: layout_mod.Idx) ResourceError!?NodeId {
        const max = trackedIntMax(target_layout) orelse return null;
        if (arg_count != 2) return null;
        const lhs = (try self.valueOf(GuardedList.at(args, 0))) orelse return null;
        const rhs = (try self.valueOf(GuardedList.at(args, 1))) orelse return null;
        if (self.constValueOf(rhs)) |c| {
            if (c >= 0) {
                if (try self.constNode(max - c)) |limit| {
                    if (try self.proveLe(lhs, limit, 0)) return try self.derived(lhs, c);
                }
            }
        }
        if (self.constValueOf(lhs)) |c| {
            if (c >= 0) {
                if (try self.constNode(max - c)) |limit| {
                    if (try self.proveLe(rhs, limit, 0)) return try self.derived(rhs, c);
                }
            }
        }
        return null;
    }

    /// Bounded node for a wrapping subtract proven not to wrap, or null.
    fn wrapSubExact(self: *Pass, args: anytype, arg_count: usize) ResourceError!?NodeId {
        if (arg_count != 2) return null;
        const lhs = (try self.valueOf(GuardedList.at(args, 0))) orelse return null;
        const rhs = (try self.valueOf(GuardedList.at(args, 1))) orelse return null;
        const rhs_lo = self.absLoOf(rhs);
        const rhs_hi = self.absHiOf(rhs);
        if (rhs_lo < 0) return null;
        if (!try self.proveLe(rhs, lhs, 0)) return null;
        return try self.derivedRange(lhs, -rhs_hi, -rhs_lo);
    }
};

/// Debug-only certification helpers, deliberately independent of the pass's
/// own walk: dominance is answered by deleted-node reachability over a freshly
/// built successor graph, and implication by a dense all-pairs closure.
const RangeProveCertify = struct {
    const Graph = struct {
        allocator: Allocator,
        root: CFStmtId,
        succs: std.AutoHashMap(CFStmtId, []CFStmtId),

        fn deinit(self: *Graph) void {
            var it = self.succs.valueIterator();
            while (it.next()) |list| self.allocator.free(list.*);
            self.succs.deinit();
        }

        /// `a` dominates `b` when every path from the root to `b` passes
        /// through `a`: removing `a` must make `b` unreachable.
        fn dominates(self: *Graph, a: CFStmtId, b: CFStmtId) bool {
            if (a == b) return true;
            var seen = std.AutoHashMap(CFStmtId, void).init(self.allocator);
            defer seen.deinit();
            var stack = std.ArrayList(CFStmtId).empty;
            defer stack.deinit(self.allocator);
            if (self.root == a) return true;
            stack.append(self.allocator, self.root) catch return false;
            seen.put(self.root, {}) catch return false;
            while (stack.pop()) |current| {
                const succs = self.succs.get(current) orelse continue;
                for (succs) |succ| {
                    if (succ == a) continue;
                    if (succ == b) return false;
                    if (seen.contains(succ)) continue;
                    seen.put(succ, {}) catch return false;
                    stack.append(self.allocator, succ) catch return false;
                }
            }
            return true;
        }
    };

    fn dominators(allocator: Allocator, store: *const LirStore, body: CFStmtId) ResourceError!Graph {
        var graph = Graph{
            .allocator = allocator,
            .root = body,
            .succs = std.AutoHashMap(CFStmtId, []CFStmtId).init(allocator),
        };
        errdefer graph.deinit();

        var join_bodies = std.AutoHashMap(JoinPointId, CFStmtId).init(allocator);
        defer join_bodies.deinit();

        var stack = std.ArrayList(CFStmtId).empty;
        defer stack.deinit(allocator);
        var list = std.ArrayList(CFStmtId).empty;
        defer list.deinit(allocator);

        try stack.append(allocator, body);
        while (stack.pop()) |current| {
            if (graph.succs.contains(current)) continue;
            list.clearRetainingCapacity();
            switch (store.getCFStmt(current)) {
                .init_uninitialized => |t| try list.append(allocator, t.next),
                .assign_ref => |t| try list.append(allocator, t.next),
                .assign_literal => |t| try list.append(allocator, t.next),
                .assign_call => |t| try list.append(allocator, t.next),
                .assign_call_erased => |t| try list.append(allocator, t.next),
                .assign_packed_erased_fn => |t| try list.append(allocator, t.next),
                .assign_low_level => |t| try list.append(allocator, t.next),
                .assign_list => |t| try list.append(allocator, t.next),
                .assign_struct => |t| try list.append(allocator, t.next),
                .assign_tag => |t| try list.append(allocator, t.next),
                .store_struct => |t| try list.append(allocator, t.next),
                .store_tag => |t| try list.append(allocator, t.next),
                .set_local => |t| try list.append(allocator, t.next),
                .debug => |t| try list.append(allocator, t.next),
                .expect => |t| try list.append(allocator, t.next),
                .comptime_branch_taken => |t| try list.append(allocator, t.next),
                .incref => |t| try list.append(allocator, t.next),
                .decref => |t| try list.append(allocator, t.next),
                .decref_if_initialized => |t| try list.append(allocator, t.next),
                .free => |t| try list.append(allocator, t.next),
                .switch_stmt => |t| {
                    const branches = store.getCFSwitchBranches(t.branches);
                    for (0..GuardedList.borrowLen(branches)) |i| {
                        try list.append(allocator, GuardedList.at(branches, i).body);
                    }
                    try list.append(allocator, t.default_branch);
                },
                .switch_initialized_payload => |t| {
                    try list.append(allocator, t.initialized_branch);
                    try list.append(allocator, t.uninitialized_branch);
                },
                .str_match => |t| {
                    try list.append(allocator, t.on_match);
                    try list.append(allocator, t.on_miss);
                },
                .str_match_set => |t| {
                    const arms = store.getStrMatchArms(t.arms);
                    for (0..GuardedList.borrowLen(arms)) |i| {
                        try list.append(allocator, GuardedList.at(arms, i).on_match);
                    }
                    try list.append(allocator, t.on_miss);
                },
                .join => |t| {
                    try join_bodies.put(t.id, t.body);
                    try list.append(allocator, t.remainder);
                },
                .jump => |t| {
                    if (join_bodies.get(t.target)) |target_body| {
                        try list.append(allocator, target_body);
                    }
                },
                .ret, .crash, .runtime_error, .expect_err, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
            const owned = try allocator.dupe(CFStmtId, list.items);
            try graph.succs.put(current, owned);
            for (owned) |succ| try stack.append(allocator, succ);
        }
        return graph;
    }

    /// Re-derive `value(ra) <= value(rb) + m` from the snapshot facts and the
    /// root nodes' constant bounds, by dense all-pairs shortest offsets.
    fn implies(allocator: Allocator, facts: []const Fact, nodes: []const Node, ra: NodeId, rb: NodeId, m: i128) bool {
        if (ra == rb) return m >= 0;

        var roots = std.ArrayList(NodeId).empty;
        defer roots.deinit(allocator);
        var index_of = std.AutoHashMap(NodeId, usize).init(allocator);
        defer index_of.deinit();
        const add_root = struct {
            fn add(list: *std.ArrayList(NodeId), map: *std.AutoHashMap(NodeId, usize), alloc: Allocator, id: NodeId) bool {
                const entry = map.getOrPut(id) catch return false;
                if (!entry.found_existing) {
                    entry.value_ptr.* = list.items.len;
                    list.append(alloc, id) catch return false;
                }
                return true;
            }
        }.add;
        if (!add_root(&roots, &index_of, allocator, ra)) return false;
        if (!add_root(&roots, &index_of, allocator, rb)) return false;
        for (facts) |fact| {
            if (!add_root(&roots, &index_of, allocator, fact.a)) return false;
            if (!add_root(&roots, &index_of, allocator, fact.b)) return false;
        }

        const n = roots.items.len;
        const infinite = std.math.maxInt(i128);
        const dist = allocator.alloc(i128, n * n) catch return false;
        defer allocator.free(dist);
        @memset(dist, infinite);
        for (0..n) |i| dist[i * n + i] = 0;
        for (facts) |fact| {
            const i = index_of.get(fact.a).?;
            const j = index_of.get(fact.b).?;
            if (fact.c < dist[i * n + j]) dist[i * n + j] = fact.c;
        }
        for (0..n) |k| {
            for (0..n) |i| {
                if (dist[i * n + k] == infinite) continue;
                for (0..n) |j| {
                    if (dist[k * n + j] == infinite) continue;
                    const through = dist[i * n + k] + dist[k * n + j];
                    if (through < dist[i * n + j]) dist[i * n + j] = through;
                }
            }
        }

        const ia = index_of.get(ra).?;
        const ib = index_of.get(rb).?;
        if (dist[ia * n + ib] != infinite and dist[ia * n + ib] <= m) return true;

        // Constant route: the tightest reachable upper bound of ra against
        // the tightest reverse-reachable lower bound of rb.
        var hi: i128 = nodes[ra].hi;
        var lo: i128 = nodes[rb].lo;
        for (0..n) |j| {
            if (dist[ia * n + j] != infinite) {
                const through = nodes[roots.items[j]].hi;
                if (through != std.math.maxInt(i128) and through + dist[ia * n + j] < hi) {
                    hi = through + dist[ia * n + j];
                }
            }
            if (dist[j * n + ib] != infinite) {
                const through = nodes[roots.items[j]].lo - dist[j * n + ib];
                if (through > lo) lo = through;
            }
        }
        return hi <= lo + m;
    }
};
