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
const collections = @import("collections");
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
const max_rounds: u32 = 12;
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

/// One synthesized upper bound `value <= root + c` carried through a merge.
const MeetBound = struct {
    root: NodeId,
    c: i128,
};

/// Bound on synthesized upper bounds per met local.
const meet_bound_cap: usize = 6;

/// A merge bound in round-stable form: node ids die at every round reset,
/// so bounds that must cross rounds are keyed by what the roots denote.
const StableBase = union(enum) {
    /// The length of the list held by this single-assignment local.
    len_of: LocalId,
    /// The value of this single-assignment integer local.
    value_of: LocalId,
    /// An absolute constant bound.
    constant,
};

const StableBound = struct {
    base: StableBase,
    c: i128,
};

/// A round-stable lower bound on the length of a list-valued loop parameter:
/// `base + c <= len(param)`. Length facts are loop invariants—nothing on a
/// back edge re-derives them from branch conditions—so they are proved by
/// induction: a candidate discovered on the entry edges is seeded as an
/// assumption, and promoted only once a round re-derives it on every edge
/// (entry edges seed-free, back edges under the assumption). Rounds with
/// unverified assumptions in play apply no rewrites.
const LenInvariant = struct {
    base: StableBase,
    /// `base - c <= len`, matching the fact form `base <= len + c`.
    c: i128,
    status: enum(u8) { pending, verified, dead },
    /// Re-derived on every captured edge this round.
    hit: bool,
};

/// Cross-round bounds of one loop parameter, complete once every jump into
/// its join was captured in a single round.
const LoopBounds = struct {
    items: [meet_bound_cap]StableBound = undefined,
    len: usize = 0,
    len_items: [meet_bound_cap]LenInvariant = undefined,
    len_count: usize = 0,
    complete: bool = false,
};

/// Fixed-capacity list of synthesized bounds.
const MeetBounds = struct {
    items: [meet_bound_cap]MeetBound = undefined,
    len: usize = 0,

    fn append(self: *MeetBounds, bound: MeetBound) void {
        if (self.len < meet_bound_cap) {
            self.items[self.len] = bound;
            self.len += 1;
        }
    }

    fn slice(self: *const MeetBounds) []const MeetBound {
        return self.items[0..self.len];
    }
};

/// Meet of one local's value across a merge's incoming edges: every edge
/// binds the local within this window of the same root, or the window is
/// invalid and only the upper bounds that every edge can prove against a
/// common root survive (a loop cursor bounded by the same list length on the
/// entry and back edges, say).
const EnvMeet = struct {
    local: LocalId,
    root: NodeId,
    off_lo: i128,
    off_hi: i128,
    valid: bool,
    bounds: MeetBounds,
    /// Lower bounds `root + c <= len(list)` provable for a list-valued
    /// local's length term on every captured edge (c stored fact-form:
    /// `root <= len + c`, so smaller is stronger).
    len_bounds: MeetBounds,
    /// The same bounds provable on any single captured edge. An invariant
    /// candidate is born here—its entry edge proves it before any back
    /// edge can—and only graduates through per-edge verification.
    len_bounds_any: MeetBounds,
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
    /// Facts held by every captured edge arriving from OUTSIDE the merge
    /// head's own region. For a loop join these are its entry edges; a fact
    /// between round-stable single-assignment values that holds on entry is
    /// a loop invariant outright, because nothing in the loop can reassign
    /// the values it relates.
    entry_captures: u32,
    entry_facts: std.ArrayList(Fact),
};

/// One endpoint of a cross-round persisted fact, in round-stable form.
const StableTerm = union(enum) {
    /// The value of this single-assignment integer local.
    value_of: LocalId,
    /// The length of the list held by this single-assignment local.
    len_of: LocalId,
    constant: i128,
};

/// A persisted fact `value(a) <= value(b) + c` between round-stable terms,
/// re-seeded into its loop body each round (whose walk always precedes its
/// back-edge captures, so in-round meets can never deliver it).
const StableFact = struct {
    a: StableTerm,
    b: StableTerm,
    c: i128,
};

/// Bound on persisted facts per loop join.
const loop_fact_cap: usize = 16;

const LoopFacts = struct {
    items: [loop_fact_cap]StableFact = undefined,
    len: usize = 0,
};

/// Bound on persisted per-merge env locals.
const merge_env_persist_cap: usize = 64;

/// One local's stable upper bounds carried across rounds for a merge head.
const StoredEnvBound = struct {
    local: LocalId,
    bounds: [meet_bound_cap]StableBound,
    len: usize,
};

/// Last round's stabilized env meet of one merge head, seeded when the
/// region must walk before its captures complete.
const MergeEnvBounds = struct {
    items: [merge_env_persist_cap]StoredEnvBound = undefined,
    len: usize = 0,
};

const Pass = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    allocator: Allocator,

    // Per-proc, per-round state. Reset by `resetRound`.
    nodes: std.ArrayList(Node),
    facts: std.ArrayList(Fact),
    global_env: collections.DenseMap(LocalId, Binding),
    path_env: collections.DenseMap(LocalId, Binding),
    undo: std.ArrayList(Undo),
    len_terms: collections.DenseMap(NodeId, NodeId),
    assign_counts: collections.DenseMap(LocalId, u32),
    pred_counts: collections.DenseMap(CFStmtId, u32),
    jump_counts: collections.DenseMap(JoinPointId, u32),
    join_stmts: collections.DenseMap(JoinPointId, CFStmtId),
    visited: collections.DenseMap(CFStmtId, void),
    region_seen: collections.DenseMap(CFStmtId, void),
    regions: std.ArrayList(CFStmtId),
    frames: std.ArrayList(Frame),
    joins_in_order: std.ArrayList(CFStmtId),
    jump_records: std.ArrayList(JumpRecord),
    merge_states: collections.DenseMap(CFStmtId, MergeState),
    body_joins: collections.DenseMap(CFStmtId, JoinPointId),
    len_roots: collections.DenseMap(NodeId, LocalId),
    value_roots: collections.DenseMap(NodeId, LocalId),
    loop_bounds: std.AutoHashMap(u64, LoopBounds),
    loop_facts: collections.DenseMap(JoinPointId, LoopFacts),
    /// Per merge head: last round's all-edge fact intersection in stable
    /// form, seeded when the merge must walk before its captures complete
    /// (a forced loop-body or cycle-interior region). Facts held by every
    /// path in a round still hold after rewrites, which only remove paths;
    /// round one's intersections are seed-free, grounding the induction.
    merge_facts: collections.DenseMap(CFStmtId, LoopFacts),
    /// Per merge head: last round's env meet in stable form, seeded with
    /// merge_facts under the same induction.
    merge_env: collections.DenseMap(CFStmtId, MergeEnvBounds),
    /// Facts about the values of single-assignment locals, valid wherever
    /// the value is in scope, like the global env bindings they describe.
    /// Replayed into every region's fact base rather than rewound with the
    /// path.
    global_facts: std.ArrayList(Fact),
    /// Field reads unified by (struct value root, field index): reading the
    /// same field of the same struct value yields the same value, so every
    /// read site shares one node and facts proved through one site's read
    /// reach the others.
    field_values: std.AutoHashMap(u64, NodeId),
    /// The merge head whose region is currently being walked; captures into
    /// it from within are its own back or interior edges.
    current_region: ?CFStmtId,
    new_loop_bounds: bool,
    /// An unverified length invariant was seeded this round: every fact-based
    /// rewrite is deferred until the assumption is promoted or discarded.
    live_pending: bool,
    /// A provable rewrite was deferred by `live_pending`; forces another round.
    deferred_rewrites: bool,
    max_join_id: u32,
    scratch: std.ArrayList(CFStmtId),
    query_best: collections.DenseMap(NodeId, i128),
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
            .global_env = collections.DenseMap(LocalId, Binding).init(allocator),
            .path_env = collections.DenseMap(LocalId, Binding).init(allocator),
            .undo = .empty,
            .len_terms = collections.DenseMap(NodeId, NodeId).init(allocator),
            .assign_counts = collections.DenseMap(LocalId, u32).init(allocator),
            .pred_counts = collections.DenseMap(CFStmtId, u32).init(allocator),
            .jump_counts = collections.DenseMap(JoinPointId, u32).init(allocator),
            .join_stmts = collections.DenseMap(JoinPointId, CFStmtId).init(allocator),
            .visited = collections.DenseMap(CFStmtId, void).init(allocator),
            .region_seen = collections.DenseMap(CFStmtId, void).init(allocator),
            .regions = .empty,
            .frames = .empty,
            .joins_in_order = .empty,
            .jump_records = .empty,
            .merge_states = collections.DenseMap(CFStmtId, MergeState).init(allocator),
            .body_joins = collections.DenseMap(CFStmtId, JoinPointId).init(allocator),
            .len_roots = collections.DenseMap(NodeId, LocalId).init(allocator),
            .value_roots = collections.DenseMap(NodeId, LocalId).init(allocator),
            .loop_bounds = std.AutoHashMap(u64, LoopBounds).init(allocator),
            .loop_facts = collections.DenseMap(JoinPointId, LoopFacts).init(allocator),
            .merge_facts = collections.DenseMap(CFStmtId, LoopFacts).init(allocator),
            .merge_env = collections.DenseMap(CFStmtId, MergeEnvBounds).init(allocator),
            .global_facts = .empty,
            .field_values = std.AutoHashMap(u64, NodeId).init(allocator),
            .current_region = null,
            .new_loop_bounds = false,
            .live_pending = false,
            .deferred_rewrites = false,
            .max_join_id = 0,
            .scratch = .empty,
            .query_best = collections.DenseMap(NodeId, i128).init(allocator),
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
        self.len_roots.deinit();
        self.value_roots.deinit();
        self.loop_bounds.deinit();
        self.loop_facts.deinit();
        self.merge_facts.deinit();
        self.merge_env.deinit();
        self.global_facts.deinit(self.allocator);
        self.field_values.deinit();
        self.scratch.deinit(self.allocator);
        self.query_best.deinit();
        self.proof_records.deinit(self.allocator);
        self.proof_facts.deinit(self.allocator);
    }

    fn resetRound(self: *Pass) void {
        self.nodes.clearRetainingCapacity();
        self.facts.clearRetainingCapacity();
        self.global_facts.clearRetainingCapacity();
        self.field_values.clearRetainingCapacity();
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
        self.len_roots.clearRetainingCapacity();
        self.value_roots.clearRetainingCapacity();
        self.max_join_id = 0;
        self.scratch.clearRetainingCapacity();
        self.proof_records.clearRetainingCapacity();
        self.proof_facts.clearRetainingCapacity();
        self.last_claim = null;
        self.rewrites = 0;
        self.live_pending = false;
        self.deferred_rewrites = false;
    }

    // Layout helpers

    fn trackedIntMax(layout_idx: layout_mod.Idx) ?i128 {
        return switch (layout_idx) {
            .u8 => std.math.maxInt(u8),
            .u16 => std.math.maxInt(u16),
            .u32 => std.math.maxInt(u32),
            .u64 => std.math.maxInt(u64),
            .bool, .str, .i8, .i16, .i32, .i64, .u128, .i128, .f32, .f64, .dec, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => null,
            _ => null,
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
        const entry = try self.path_env.getOrPut(local);
        const prev: ?Binding = if (entry.found_existing) entry.value_ptr.* else null;
        entry.value_ptr.* = binding;
        try self.undo.append(self.allocator, .{
            .local = local,
            .prev = prev,
        });
    }

    /// Value node for a local, materializing and binding a fresh root the
    /// first time an unbound local is read so later reads of the same
    /// unchanged local unify with it. The binding is path-scoped for
    /// reassignable locals, so it never outlives the value it names.
    fn valueOf(self: *Pass, local: LocalId) ResourceError!?NodeId {
        if (self.lookup(local)) |binding| return binding.node;
        const node = (try self.unknownFor(self.localLayout(local))) orelse return null;
        // A single-assignment integer local materialized this way keeps its
        // fresh root for the whole round, so the root denotes the local's
        // value in round-stable form.
        if (trackedIntMax(self.localLayout(local)) != null and self.isSingleAssign(local)) {
            try self.value_roots.put(node, local);
        }
        try self.bind(local, .{ .node = node });
        return node;
    }

    fn bindFresh(self: *Pass, local: LocalId) ResourceError!void {
        const node = (try self.unknownFor(self.localLayout(local))) orelse return;
        // As in valueOf: a single-assignment integer local's fresh root
        // denotes its value in round-stable form.
        if (trackedIntMax(self.localLayout(local)) != null and self.isSingleAssign(local)) {
            try self.value_roots.put(node, local);
        }
        try self.bind(local, .{ .node = node });
    }

    /// Bind a field-read target, unifying with earlier reads of the same
    /// field of the same struct value so facts reach every read site.
    fn bindFieldRead(self: *Pass, target: LocalId, source: LocalId, field_idx: u16) ResourceError!void {
        const src_node = (try self.valueOf(source)) orelse return self.bindFresh(target);
        const key = (@as(u64, self.rootOf(src_node)) << 16) | field_idx;
        if (self.field_values.get(key)) |node| {
            try self.bind(target, .{ .node = node });
            return;
        }
        const node = (try self.unknownFor(self.localLayout(target))) orelse return self.bindFresh(target);
        try self.field_values.put(key, node);
        if (trackedIntMax(self.localLayout(target)) != null and self.isSingleAssign(target)) {
            try self.value_roots.put(node, target);
        }
        try self.bind(target, .{ .node = node });
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
        var seen = collections.DenseMap(CFStmtId, void).init(self.allocator);
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
                .assign_boxy_desc_ref => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_dict_ref => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_box => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_reuse_box => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_unbox => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_adapt => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_inspect => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_eq => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_tag => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_boxy_tag_payload => |s| {
                    try self.bumpAssign(s.target);
                    try self.edgeTo(s.next);
                },
                .assign_call_dict => |s| {
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
                .boxy_tag_match => |s| {
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
            state.entry_facts.deinit(self.allocator);
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
        // A list with a materialized length term carries length bounds.
        if (self.len_terms.contains(node.root)) return true;
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
            entry.value_ptr.* = .{ .captures = 0, .facts = .empty, .env = .empty, .entry_captures = 0, .entry_facts = .empty };
        }
        const state = entry.value_ptr;

        // An edge arriving from another region is an entry edge; its facts
        // meet separately so loop-invariant relations survive the back
        // edge's inability to derive them before its region is seeded.
        if (self.current_region != head) {
            if (state.entry_captures == 0) {
                try state.entry_facts.appendSlice(self.allocator, self.facts.items);
            } else {
                var keep_entry: usize = 0;
                for (state.entry_facts.items) |fact| {
                    for (self.facts.items) |mine| {
                        if (mine.a == fact.a and mine.b == fact.b and mine.c == fact.c) {
                            state.entry_facts.items[keep_entry] = fact;
                            keep_entry += 1;
                            break;
                        }
                    }
                }
                state.entry_facts.shrinkRetainingCapacity(keep_entry);
            }
            state.entry_captures += 1;
        }

        if (state.captures == 0) {
            try state.facts.appendSlice(self.allocator, self.facts.items);
            var it = self.path_env.iterator();
            while (it.next()) |kv| {
                if (state.env.items.len >= merge_env_cap) break;
                if (!self.captureWorthy(kv.value_ptr.node)) continue;
                const node = self.nodes.items[kv.value_ptr.node];
                const len_bounds = try self.localLenBounds(kv.value_ptr.node);
                try state.env.append(self.allocator, .{
                    .local = kv.key_ptr.*,
                    .root = node.root,
                    .off_lo = node.off_lo,
                    .off_hi = node.off_hi,
                    .valid = true,
                    .bounds = try self.reachableBounds(kv.value_ptr.node),
                    .len_bounds = len_bounds,
                    .len_bounds_any = len_bounds,
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
                if (!meet.valid and meet.bounds.len == 0 and meet.len_bounds.len == 0 and meet.len_bounds_any.len == 0) continue;
                const binding = self.path_env.get(meet.local);
                if (binding) |b| {
                    const node = self.nodes.items[b.node];
                    if (meet.valid) {
                        if (node.root == meet.root) {
                            meet.off_lo = @min(meet.off_lo, node.off_lo);
                            meet.off_hi = @max(meet.off_hi, node.off_hi);
                        } else {
                            meet.valid = false;
                        }
                    }
                    // Keep only the upper bounds this edge can also prove,
                    // widened to cover both edges.
                    const mine = try self.reachableBounds(b.node);
                    var kept: MeetBounds = .{};
                    for (meet.bounds.slice()) |bound| {
                        for (mine.slice()) |candidate| {
                            if (candidate.root == bound.root) {
                                kept.append(.{ .root = bound.root, .c = @max(bound.c, candidate.c) });
                                break;
                            }
                        }
                    }
                    meet.bounds = kept;
                    // Same meet for length lower bounds: keep what this edge
                    // also proves, weakened (larger c) to cover both edges.
                    const mine_len = try self.localLenBounds(b.node);
                    var kept_len: MeetBounds = .{};
                    for (meet.len_bounds.slice()) |bound| {
                        for (mine_len.slice()) |candidate| {
                            if (candidate.root == bound.root) {
                                kept_len.append(.{ .root = bound.root, .c = @max(bound.c, candidate.c) });
                                break;
                            }
                        }
                    }
                    meet.len_bounds = kept_len;
                    // The any-edge union keeps the smallest c seen for a
                    // root on any edge: the tightest bound any single edge
                    // proves.
                    for (mine_len.slice()) |candidate| {
                        var merged = false;
                        for (meet.len_bounds_any.items[0..meet.len_bounds_any.len]) |*have| {
                            if (have.root == candidate.root) {
                                have.c = @min(have.c, candidate.c);
                                merged = true;
                                break;
                            }
                        }
                        if (!merged) meet.len_bounds_any.append(candidate);
                    }
                } else {
                    meet.valid = false;
                    meet.bounds = .{};
                    meet.len_bounds = .{};
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
        // A loop body walks before its back edge can be captured, so its
        // in-round meet never completes; bounds persisted by an earlier
        // round stand in for it.
        if (self.body_joins.get(head)) |join_id| {
            const state = self.merge_states.get(head);
            const captures = if (state) |st| st.captures else 0;
            if (captures != self.jumpCount(join_id)) {
                try self.seedMergeFacts(head);
                try self.seedMergeEnv(head);
                try self.seedLoopFacts(join_id);
                if (self.join_stmts.get(join_id)) |join_stmt| {
                    const join = self.store.getCFStmt(join_stmt).join;
                    const params = self.store.getLocalSpan(join.params);
                    for (0..GuardedList.borrowLen(params)) |i| {
                        try self.seedLoopParam(join_id, GuardedList.at(params, i));
                    }
                }
                return;
            }
        }
        const state = self.merge_states.getPtr(head) orelse {
            try self.seedMergeFacts(head);
            try self.seedMergeEnv(head);
            return;
        };
        const expected = self.mergeExpected(head);
        if (state.captures != expected or expected == 0) {
            try self.seedMergeFacts(head);
            try self.seedMergeEnv(head);
            return;
        }

        try self.facts.appendSlice(self.allocator, state.facts.items);

        for (state.env.items) |meet| {
            if (meet.valid) {
                const node = (try self.addNode(.{
                    .root = meet.root,
                    .off_lo = meet.off_lo,
                    .off_hi = meet.off_hi,
                    .lo = 0,
                    .hi = 0,
                })) orelse continue;
                try self.bind(meet.local, .{ .node = node });
                continue;
            }
            if (meet.len_bounds.len > 0) {
                // A list bound to different values per edge whose length
                // lower bounds all edges prove: a fresh value with a fresh
                // length term carrying them preserves the lengths.
                const list_node = (try self.unknownFor(self.localLayout(meet.local))) orelse continue;
                const len_term = (try self.freshRoot(0, std.math.maxInt(i64))) orelse continue;
                for (meet.len_bounds.slice()) |bound| {
                    try self.addFact(.{ .a = bound.root, .b = len_term, .c = bound.c, .origin = .meet });
                }
                try self.len_terms.put(list_node, len_term);
                try self.bind(meet.local, .{ .node = list_node });
                continue;
            }
            if (meet.bounds.len == 0) continue;
            // The edges bind different values, but each proves the same upper
            // bounds; a fresh value carrying those bounds preserves them.
            const node = (try self.unknownFor(.u64)) orelse continue;
            for (meet.bounds.slice()) |bound| {
                try self.addFact(.{ .a = node, .b = bound.root, .c = bound.c, .origin = .meet });
            }
            try self.bind(meet.local, .{ .node = node });
        }
    }

    /// Key for one loop parameter's cross-round bounds.
    fn loopBoundKey(join_id: JoinPointId, local: LocalId) u64 {
        return (@as(u64, @intFromEnum(join_id)) << 32) | @intFromEnum(local);
    }

    /// Round-stable form of a bound root: a list-length term of a stable
    /// local, or a constant.
    fn stableBase(self: *const Pass, root: NodeId) ?StableBound {
        if (self.len_roots.get(root)) |list_local| {
            return .{ .base = .{ .len_of = list_local }, .c = 0 };
        }
        if (self.value_roots.get(root)) |scalar_local| {
            return .{ .base = .{ .value_of = scalar_local }, .c = 0 };
        }
        const node = self.nodes.items[root];
        if (node.lo == node.hi) return .{ .base = .constant, .c = node.lo };
        return null;
    }

    /// Persist the completed merges' parameter bounds in round-stable form so
    /// the next round can seed loop bodies, whose own walk always precedes
    /// their back-edge captures.
    /// Round-stable form of one captured length lower bound, or null when
    /// its root denotes nothing stable. The result means
    /// `stable_value(base) <= len + c`, where a constant base denotes zero
    /// (its value folded into `c`).
    fn lenStable(self: *const Pass, bound: MeetBound) ?struct { base: StableBase, c: i128 } {
        if (self.len_roots.get(bound.root)) |list_local| {
            return .{ .base = .{ .len_of = list_local }, .c = bound.c };
        }
        if (self.value_roots.get(bound.root)) |scalar_local| {
            return .{ .base = .{ .value_of = scalar_local }, .c = bound.c };
        }
        const node = self.nodes.items[bound.root];
        if (node.lo == node.hi) return .{ .base = .constant, .c = bound.c - node.lo };
        return null;
    }

    fn sameLenBase(a: StableBase, b: StableBase) bool {
        return switch (a) {
            .len_of => |la| switch (b) {
                .len_of => |lb| la == lb,
                .value_of, .constant => false,
            },
            .value_of => |la| switch (b) {
                .value_of => |lb| la == lb,
                .len_of, .constant => false,
            },
            .constant => b == .constant,
        };
    }

    /// Round-stable form of a fact endpoint root, or null.
    fn stabilizeTerm(self: *const Pass, root: NodeId) ?StableTerm {
        if (self.len_roots.get(root)) |list_local| return .{ .len_of = list_local };
        if (self.value_roots.get(root)) |scalar_local| return .{ .value_of = scalar_local };
        const node = self.nodes.items[root];
        if (node.lo == node.hi) return .{ .constant = node.lo };
        return null;
    }

    /// Persist a loop join's entry-edge facts whose endpoints are all
    /// round-stable. Such a fact relates values no loop iteration can
    /// reassign—the endpoint locals were bound before entry—so holding on
    /// every entry edge makes it hold throughout the loop.
    fn persistLoopFacts(self: *Pass, join_id: JoinPointId, state: *const MergeState) ResourceError!void {
        if (state.entry_captures == 0) return;
        const stable = self.stabilizeFacts(state.entry_facts.items);
        if (stable.len == 0) return;
        const previous = self.loop_facts.get(join_id);
        if (previous == null or previous.?.len != stable.len) self.new_loop_bounds = true;
        try self.loop_facts.put(join_id, stable);
    }

    /// Seed a loop body region with its persisted entry-invariant facts,
    /// materialized against this round's nodes.
    fn seedLoopFacts(self: *Pass, join_id: JoinPointId) ResourceError!void {
        const stored = self.loop_facts.get(join_id) orelse return;
        for (stored.items[0..stored.len]) |fact| {
            const a = (try self.materializeTerm(fact.a)) orelse continue;
            const b = (try self.materializeTerm(fact.b)) orelse continue;
            // The persisted relation is between values; restated on roots:
            // root_a <= value_a - off_lo_a and value_b <= root_b + off_hi_b.
            const c = fact.c + self.offHiOf(b) - self.offLoOf(a);
            try self.addFact(.{ .a = self.rootOf(a), .b = self.rootOf(b), .c = c, .origin = .meet });
        }
    }

    /// This round's node for a stable term: the local's value, the list
    /// local's length term, or a constant.
    fn materializeTerm(self: *Pass, term: StableTerm) ResourceError!?NodeId {
        switch (term) {
            .value_of => |scalar_local| return try self.valueOf(scalar_local),
            .len_of => |list_local| {
                const ln = (try self.valueOf(list_local)) orelse return null;
                const root = self.rootOf(ln);
                if (self.len_terms.get(root)) |len_term| return len_term;
                const fresh = (try self.freshRoot(0, std.math.maxInt(i64))) orelse return null;
                try self.len_terms.put(root, fresh);
                if (self.isSingleAssign(list_local)) {
                    try self.len_roots.put(fresh, list_local);
                }
                return fresh;
            },
            .constant => |v| return try self.constNode(v),
        }
    }

    /// Stabilize a fact list into round-stable form, keeping facts whose
    /// endpoints all denote something stable.
    fn stabilizeFacts(self: *const Pass, facts: []const Fact) LoopFacts {
        var stable = LoopFacts{};
        for (facts) |fact| {
            if (stable.len >= loop_fact_cap) break;
            const a = self.stabilizeTerm(fact.a) orelse continue;
            const b = self.stabilizeTerm(fact.b) orelse continue;
            if (a == .constant and b == .constant) continue;
            stable.items[stable.len] = .{ .a = a, .b = b, .c = fact.c };
            stable.len += 1;
        }
        return stable;
    }

    /// Persist a fully-captured merge's all-edge fact intersection for
    /// seeding when a later round must walk it before capture completes.
    fn persistMergeFacts(self: *Pass, head: CFStmtId, state: *const MergeState) ResourceError!void {
        const stable = self.stabilizeFacts(state.facts.items);
        if (stable.len == 0) return;
        if (self.merge_facts.get(head)) |previous| {
            if (previous.len != stable.len) {
                self.new_loop_bounds = true;
            } else for (previous.items[0..previous.len], stable.items[0..stable.len]) |old, new| {
                if (!std.meta.eql(old, new)) {
                    self.new_loop_bounds = true;
                    break;
                }
            }
        } else self.new_loop_bounds = true;
        try self.merge_facts.put(head, stable);
    }

    /// Seed a region walked before its captures complete with the facts
    /// every edge carried last round.
    fn seedMergeFacts(self: *Pass, head: CFStmtId) ResourceError!void {
        const stored = self.merge_facts.get(head) orelse return;
        for (stored.items[0..stored.len]) |fact| {
            const a = (try self.materializeTerm(fact.a)) orelse continue;
            const b = (try self.materializeTerm(fact.b)) orelse continue;
            const c = fact.c + self.offHiOf(b) - self.offLoOf(a);
            try self.addFact(.{ .a = self.rootOf(a), .b = self.rootOf(b), .c = c, .origin = .meet });
        }
    }

    /// Persist a fully-captured merge's env meet in round-stable form.
    fn persistMergeEnv(self: *Pass, head: CFStmtId, state: *const MergeState) ResourceError!void {
        var stable = MergeEnvBounds{};
        for (state.env.items) |meet| {
            if (stable.len >= merge_env_persist_cap) break;
            var entry = StoredEnvBound{ .local = meet.local, .bounds = undefined, .len = 0 };
            for (meet.bounds.slice()) |bound| {
                if (self.stableBase(bound.root)) |base| {
                    if (entry.len < meet_bound_cap) {
                        entry.bounds[entry.len] = .{ .base = base.base, .c = base.c + bound.c };
                        entry.len += 1;
                    }
                }
            }
            if (entry.len == 0) continue;
            stable.items[stable.len] = entry;
            stable.len += 1;
        }
        if (stable.len == 0) return;
        if (self.merge_env.get(head)) |previous| {
            if (previous.len != stable.len) {
                self.new_loop_bounds = true;
            } else for (previous.items[0..previous.len], stable.items[0..stable.len]) |old, new| {
                if (old.local != new.local or old.len != new.len) {
                    self.new_loop_bounds = true;
                    break;
                }
                for (old.bounds[0..old.len], new.bounds[0..new.len]) |ob, nb| {
                    if (!std.meta.eql(ob, nb)) {
                        self.new_loop_bounds = true;
                        break;
                    }
                }
            }
        } else self.new_loop_bounds = true;
        try self.merge_env.put(head, stable);
    }

    /// Seed the env of a region walked before its captures complete from
    /// last round's stabilized meet: each local binds to a fresh value
    /// carrying the upper bounds every edge proved.
    fn seedMergeEnv(self: *Pass, head: CFStmtId) ResourceError!void {
        const stored = self.merge_env.get(head) orelse return;
        for (stored.items[0..stored.len]) |entry| {
            const node = (try self.unknownFor(self.localLayout(entry.local))) orelse continue;
            var used = false;
            for (entry.bounds[0..entry.len]) |bound| {
                switch (bound.base) {
                    .len_of => |list_local| {
                        const term = (try self.materializeTerm(.{ .len_of = list_local })) orelse continue;
                        try self.addFact(.{ .a = node, .b = term, .c = bound.c, .origin = .meet });
                        used = true;
                    },
                    .value_of => |scalar_local| {
                        const v = (try self.valueOf(scalar_local)) orelse continue;
                        try self.addFact(.{ .a = node, .b = self.rootOf(v), .c = bound.c + self.offHiOf(v), .origin = .meet });
                        used = true;
                    },
                    .constant => {
                        const const_node = (try self.constNode(bound.c)) orelse continue;
                        try self.addFact(.{ .a = node, .b = const_node, .c = 0, .origin = .meet });
                        used = true;
                    },
                }
            }
            if (used) try self.bind(entry.local, .{ .node = node });
        }
    }

    fn persistLoopBounds(self: *Pass) ResourceError!void {
        var it = self.merge_states.iterator();
        while (it.next()) |entry| {
            const head = entry.key_ptr.*;
            const state = entry.value_ptr;
            if (!self.live_pending and state.captures == self.mergeExpected(head) and state.captures >= 2) {
                try self.persistMergeFacts(head, state);
                try self.persistMergeEnv(head, state);
            }
            const join_id = self.body_joins.get(head) orelse continue;
            if (!self.live_pending) try self.persistLoopFacts(join_id, state);
            if (state.captures != self.jumpCount(join_id) or state.captures < 2) continue;
            for (state.env.items) |meet| {
                const key = loopBoundKey(join_id, meet.local);
                if (self.live_pending) {
                    // Assumption round: the walk ran under unverified seeds,
                    // so nothing new is persisted; pending invariants that
                    // this round re-derived on every edge are marked.
                    const stored = self.loop_bounds.getPtr(key) orelse continue;
                    for (stored.len_items[0..stored.len_count]) |*item| {
                        if (item.status != .pending) continue;
                        for (meet.len_bounds.slice()) |bound| {
                            if (self.lenStable(bound)) |candidate| {
                                if (sameLenBase(candidate.base, item.base) and candidate.c <= item.c) {
                                    item.hit = true;
                                    break;
                                }
                            }
                        }
                    }
                    continue;
                }

                var stable = LoopBounds{ .complete = true };
                for (meet.bounds.slice()) |bound| {
                    if (self.stableBase(bound.root)) |base| {
                        if (stable.len < meet_bound_cap) {
                            stable.items[stable.len] = .{ .base = base.base, .c = base.c + bound.c };
                            stable.len += 1;
                        }
                    }
                }
                // Carry the invariant list forward, admitting new length
                // candidates as pending. A base that already failed
                // verification stays dead and is never re-admitted.
                if (self.loop_bounds.get(key)) |previous| {
                    stable.len_items = previous.len_items;
                    stable.len_count = previous.len_count;
                }
                for (meet.len_bounds_any.slice()) |bound| {
                    const candidate = self.lenStable(bound) orelse continue;
                    // A constant bound below one is what any length already
                    // satisfies; assuming it would cost a round for nothing.
                    if (candidate.base == .constant and candidate.c >= 0) continue;
                    var known = false;
                    for (stable.len_items[0..stable.len_count]) |item| {
                        if (sameLenBase(candidate.base, item.base)) {
                            known = true;
                            break;
                        }
                    }
                    if (!known and stable.len_count < meet_bound_cap) {
                        stable.len_items[stable.len_count] = .{
                            .base = candidate.base,
                            .c = candidate.c,
                            .status = .pending,
                            .hit = false,
                        };
                        stable.len_count += 1;
                        self.new_loop_bounds = true;
                    }
                }
                if (stable.len == 0 and stable.len_count == 0) continue;
                const previous = self.loop_bounds.get(key);
                if (previous == null or previous.?.len != stable.len) self.new_loop_bounds = true;
                try self.loop_bounds.put(key, stable);
            }
        }
    }

    /// Group resolution of the assumed length invariants at round end: the
    /// assumptions may justify one another (simultaneous induction), so all
    /// of them promote only when every one was re-derived on every edge;
    /// otherwise the failures die and the survivors retry next round without
    /// them.
    fn resolvePendingInvariants(self: *Pass) void {
        if (!self.live_pending) return;
        var any_pending = false;
        var all_hit = true;
        var it = self.loop_bounds.valueIterator();
        while (it.next()) |stored| {
            for (stored.len_items[0..stored.len_count]) |item| {
                if (item.status != .pending) continue;
                any_pending = true;
                if (!item.hit) all_hit = false;
            }
        }
        if (!any_pending) return;
        it = self.loop_bounds.valueIterator();
        while (it.next()) |stored| {
            for (stored.len_items[0..stored.len_count]) |*item| {
                if (item.status != .pending) continue;
                if (all_hit) {
                    item.status = .verified;
                } else if (!item.hit) {
                    item.status = .dead;
                }
            }
        }
        self.new_loop_bounds = true;
    }

    /// Seed a loop parameter from bounds persisted by an earlier round,
    /// materialized against this round's nodes.
    fn seedLoopParam(self: *Pass, join_id: JoinPointId, local: LocalId) ResourceError!void {
        const stored = self.loop_bounds.getPtr(loopBoundKey(join_id, local)) orelse return;
        if (!stored.complete) return;
        if (stored.len_count > 0) {
            try self.seedLenInvariants(stored, local);
            return;
        }
        const node = (try self.unknownFor(.u64)) orelse return;
        var used = false;
        for (stored.items[0..stored.len]) |bound| {
            switch (bound.base) {
                .len_of => |list_local| {
                    const list_node = (try self.valueOf(list_local)) orelse continue;
                    const root = self.rootOf(list_node);
                    const len_node = self.len_terms.get(root) orelse blk: {
                        const fresh = (try self.freshRoot(0, std.math.maxInt(i64))) orelse continue;
                        try self.len_terms.put(root, fresh);
                        try self.len_roots.put(fresh, list_local);
                        break :blk fresh;
                    };
                    try self.addFact(.{ .a = node, .b = len_node, .c = bound.c, .origin = .meet });
                    used = true;
                },
                .constant => {
                    const const_node = (try self.constNode(bound.c)) orelse continue;
                    try self.addFact(.{ .a = node, .b = const_node, .c = 0, .origin = .meet });
                    used = true;
                },
                .value_of => |scalar_local| {
                    const v = (try self.valueOf(scalar_local)) orelse continue;
                    try self.addFact(.{ .a = node, .b = self.rootOf(v), .c = bound.c + self.offHiOf(v), .origin = .meet });
                    used = true;
                },
            }
        }
        if (used) try self.bind(local, .{ .node = node });
    }

    /// Seed a list-valued loop parameter's length invariants: an opaque value
    /// node whose materialized length term carries each stored bound as a
    /// fact. Pending bounds are assumptions—seeding one makes this an
    /// assumption round, so no rewrite can rest on them before they verify.
    fn seedLenInvariants(self: *Pass, stored: *LoopBounds, local: LocalId) ResourceError!void {
        const list_node = (try self.unknownFor(self.localLayout(local))) orelse return;
        const len_term = (try self.freshRoot(0, std.math.maxInt(i64))) orelse return;
        var seeded = false;
        for (stored.len_items[0..stored.len_count]) |*item| {
            item.hit = false;
            if (item.status == .dead) continue;
            const seed: ?struct { root: NodeId, c: i128 } = switch (item.base) {
                .len_of => |list_local| blk: {
                    const ln = (try self.valueOf(list_local)) orelse break :blk null;
                    const root = self.rootOf(ln);
                    const lt = self.len_terms.get(root) orelse inner: {
                        const fresh = (try self.freshRoot(0, std.math.maxInt(i64))) orelse break :blk null;
                        try self.len_terms.put(root, fresh);
                        if (self.isSingleAssign(list_local)) {
                            try self.len_roots.put(fresh, list_local);
                        }
                        break :inner fresh;
                    };
                    break :blk .{ .root = lt, .c = item.c };
                },
                .value_of => |scalar_local| blk: {
                    const v = (try self.valueOf(scalar_local)) orelse break :blk null;
                    // The bound is on the local's value; restated against its
                    // root: root <= value - off_lo <= len + c - off_lo.
                    break :blk .{ .root = self.rootOf(v), .c = item.c - self.offLoOf(v) };
                },
                .constant => blk: {
                    const zero = (try self.constNode(0)) orelse break :blk null;
                    break :blk .{ .root = zero, .c = item.c };
                },
            };
            const resolved = seed orelse continue;
            try self.addFact(.{ .a = resolved.root, .b = len_term, .c = resolved.c, .origin = .meet });
            if (item.status == .pending) self.live_pending = true;
            seeded = true;
        }
        if (seeded) {
            try self.len_terms.put(list_node, len_term);
            try self.bind(local, .{ .node = list_node });
        }
    }

    /// Upper bounds `value <= root + c` provable for a node from the current
    /// path facts, found by walking fact edges forward from its root.
    fn reachableBounds(self: *Pass, node_id: NodeId) ResourceError!MeetBounds {
        var bounds: MeetBounds = .{};
        const node = self.nodes.items[node_id];
        bounds.append(.{ .root = node.root, .c = node.off_hi });
        self.query_best.clearRetainingCapacity();
        try self.query_best.put(node.root, 0);
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
                    bounds.append(.{ .root = fact.b, .c = next_acc + node.off_hi });
                    changed = true;
                }
            }
        }
        return bounds;
    }

    /// Lower bounds `root <= value(len_node) + c` provable from the current
    /// path facts, found by walking fact edges backward from the length
    /// term's root. Smaller `c` is the stronger claim.
    fn lenLowerBounds(self: *Pass, len_node: NodeId) ResourceError!MeetBounds {
        var bounds: MeetBounds = .{};
        const node = self.nodes.items[len_node];
        bounds.append(.{ .root = node.root, .c = -node.off_lo });
        self.query_best.clearRetainingCapacity();
        try self.query_best.put(node.root, -node.off_lo);
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
                    bounds.append(.{ .root = fact.a, .c = next_acc });
                    changed = true;
                }
            }
        }
        return bounds;
    }

    /// This round's length lower bounds for a local, when its value is a
    /// list with a materialized length term. Bounds that say nothing a fresh
    /// length would not (every length is at least zero) are dropped, so a
    /// list without a real invariant never engages the merge machinery.
    fn localLenBounds(self: *Pass, node: NodeId) ResourceError!MeetBounds {
        const len_term = self.len_terms.get(self.rootOf(node)) orelse return .{};
        const all = try self.lenLowerBounds(len_term);
        var kept: MeetBounds = .{};
        for (all.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi and root.lo - bound.c < 1) continue;
            kept.append(bound);
        }
        return kept;
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
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_low_level,
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
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .loop_continue,
                .loop_break,
                .jump,
                .ret,
                .crash,
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
                => continue,
            };
            const params = self.store.getLocalSpan(join.params);
            if (GuardedList.borrowLen(params) != 1) continue;
            if (!join.maybe_uninitialized_params.isEmpty()) continue;
            const param = GuardedList.at(params, 0);
            if (self.localLayout(param) != .bool) continue;

            const body_switch = switch (self.store.getCFStmt(join.body)) {
                .switch_stmt => |sw| sw,
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_low_level,
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
                .join,
                .jump,
                .ret,
                .crash,
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
                => continue,
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
                    .init_uninitialized,
                    .assign_ref,
                    .assign_literal,
                    .assign_call,
                    .assign_call_erased,
                    .assign_packed_erased_fn,
                    .assign_low_level,
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
                    .switch_stmt,
                    .switch_initialized_payload,
                    .str_match,
                    .str_match_set,
                    .loop_continue,
                    .loop_break,
                    .join,
                    .ret,
                    .crash,
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
                    => continue,
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

        self.loop_bounds.clearRetainingCapacity();
        self.loop_facts.clearRetainingCapacity();
        self.merge_facts.clearRetainingCapacity();
        self.merge_env.clearRetainingCapacity();
        var round: u32 = 0;
        while (round < max_rounds) : (round += 1) {
            self.resetRound();
            try self.prescanProc(proc);
            // Threading restructures control flow, so a round that threads
            // stops there and the next round re-derives the graph facts.
            self.new_loop_bounds = false;
            if (try self.threadBoolJoins() == 0) {
                try self.walkRegions(proc.body.?);
                try self.certifyRound(proc.body.?);
                try self.persistLoopBounds();
                self.resolvePendingInvariants();
            }
            if (self.rewrites == 0 and !self.new_loop_bounds and !self.deferred_rewrites) return;
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
            self.current_region = head;
            self.path_env.clearRetainingCapacity();
            self.undo.clearRetainingCapacity();
            self.facts.clearRetainingCapacity();
            self.frames.clearRetainingCapacity();
            try self.facts.appendSlice(self.allocator, self.global_facts.items);
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
                            .field => |f| {
                                try self.bindFieldRead(s.target, f.source, f.field_idx);
                            },
                            .discriminant, .tag_payload, .tag_payload_struct, .list_reinterpret, .nominal => try self.bindFresh(s.target),
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
                    .assign_boxy_desc_ref => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_dict_ref => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_box => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_reuse_box => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_unbox => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_adapt => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_inspect => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_eq => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_tag => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_boxy_tag_payload => |s| {
                        try self.visited.put(current, {});
                        try self.bindFresh(s.target);
                        current = s.next;
                    },
                    .assign_call_dict => |s| {
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
                    .boxy_tag_match => |s| {
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
            .f64_literal, .f32_literal, .dec_literal, .str_literal, .static_data, .bytes_literal, .null_ptr, .proc_ref, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal => null,
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
                    const list_local = GuardedList.at(args, 0);
                    if (try self.valueOf(list_local)) |list_node| {
                        const root = self.rootOf(list_node);
                        if (self.len_terms.get(root)) |len_node| {
                            try self.bind(s.target, .{ .node = len_node });
                            return;
                        }
                        // List lengths fit a signed 64-bit count.
                        if (try self.freshRoot(0, std.math.maxInt(i64))) |len_node| {
                            try self.len_terms.put(root, len_node);
                            if (self.isSingleAssign(list_local)) {
                                try self.len_roots.put(len_node, list_local);
                            }
                            try self.bind(s.target, .{ .node = len_node });
                            return;
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            .list_capacity => {
                // A list's capacity is a non-negative count with no tighter
                // statically known bound.
                try self.bindFresh(s.target);
            },
            .list_set, .list_set_in_place_unsafe => {
                // Replacing one element preserves the list's length on every
                // continuing path, so the result shares the input's length
                // term.
                if (arg_count == 3) {
                    if (try self.valueOf(GuardedList.at(args, 0))) |in_node| {
                        if (self.len_terms.get(self.rootOf(in_node))) |len_term| {
                            if (try self.unknownFor(self.localLayout(s.target))) |out_node| {
                                try self.len_terms.put(out_node, len_term);
                                try self.bind(s.target, .{ .node = out_node });
                                return;
                            }
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
                const bound: ?NodeId = if (mask) |m| try self.freshRoot(0, m) else try self.unknownFor(self.localLayout(s.target));
                if (bound) |node| {
                    // An unsigned AND is at most either operand, so the
                    // result chains to a dynamic mask's own bounds (a table
                    // index masked by a runtime table size, say).
                    if (arg_count == 2) {
                        for (0..2) |i| {
                            if (try self.valueOf(GuardedList.at(args, i))) |operand| {
                                const fact = Fact{
                                    .a = node,
                                    .b = self.rootOf(operand),
                                    .c = self.offHiOf(operand),
                                    .origin = .meet,
                                };
                                try self.addFact(fact);
                                // The result value outlives this path when
                                // its local is single-assignment; regions
                                // reading it through the global env replay
                                // the fact with it.
                                if (self.isSingleAssign(s.target)) {
                                    try self.global_facts.append(self.allocator, fact);
                                }
                            }
                        }
                    }
                    try self.bind(s.target, .{ .node = node });
                    return;
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
            .str_is_eq,
            .str_is_eq_static_small,
            .str_static_small_word_eq,
            .str_static_small_word_caseless_eq,
            .str_concat,
            .str_contains,
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_caseless_ascii_equals,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_starts_with,
            .str_ends_with,
            .str_repeat,
            .str_drop_prefix,
            .str_drop_prefix_caseless_ascii,
            .str_drop_suffix,
            .str_split_first,
            .str_split_last,
            .str_count_utf8_bytes,
            .str_get_utf8_byte_unsafe,
            .str_substring_unsafe,
            .str_with_capacity,
            .str_reserve,
            .str_release_excess_capacity,
            .str_to_utf8,
            .str_from_utf8_lossy,
            .str_from_utf8,
            .str_split_on,
            .str_join_with,
            .str_inspect,
            .u8_to_str,
            .i8_to_str,
            .u16_to_str,
            .i16_to_str,
            .u32_to_str,
            .i32_to_str,
            .u64_to_str,
            .i64_to_str,
            .u128_to_str,
            .i128_to_str,
            .dec_to_str,
            .f32_to_str,
            .f64_to_str,
            .list_get_unsafe,
            .list_append_unsafe,
            .list_concat,
            .list_append_range_within,
            .list_copy_range_within,
            .list_append_range_within_unsafe,
            .list_append_sublist,
            .list_append_le_bytes,
            .list_slack_unique,
            .list_owned_unique,
            .list_with_capacity,
            .list_drop_at,
            .list_sublist,
            .list_sublist_borrowed,
            .list_replace_unsafe,
            .list_swap,
            .list_prepend,
            .list_first,
            .list_last,
            .list_drop_first,
            .list_drop_last,
            .list_take_first,
            .list_take_last,
            .list_reverse,
            .list_reserve,
            .list_release_excess_capacity,
            .list_split_first,
            .list_split_last,
            .list_map_prepare_reuse,
            .list_map_can_reuse,
            .list_map_cast_unsafe,
            .list_map_extract_unsafe,
            .list_map_write_unsafe,
            .bool_not,
            .dict_pseudo_seed,
            .hasher_finish,
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_u128,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            .hasher_write_i128,
            .hasher_write_f32,
            .hasher_write_f64,
            .hasher_write_dec,
            .hasher_write_bytes,
            .hasher_write_str,
            .crypto_sha256_hash_bytes,
            .crypto_sha256_hasher_empty,
            .crypto_sha256_hasher_write,
            .crypto_sha256_hasher_finish,
            .crypto_blake3_hash_bytes,
            .crypto_blake3_hasher_empty,
            .crypto_blake3_hasher_write,
            .crypto_blake3_hasher_finish,
            .num_is_eq,
            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_plus_wrap,
            .num_minus_wrap,
            .num_times,
            .num_times_wrap,
            .num_times_checked,
            .num_div_by,
            .num_div_by_checked,
            .num_div_trunc_by,
            .num_div_trunc_by_checked,
            .num_rem_by,
            .num_rem_by_checked,
            .num_mod_by,
            .num_mod_by_checked,
            .num_negate_checked,
            .num_abs_checked,
            .num_pow,
            .num_sqrt,
            .num_sin,
            .num_cos,
            .num_tan,
            .num_asin,
            .num_acos,
            .num_atan,
            .num_log,
            .num_round,
            .num_floor,
            .num_ceiling,
            .num_to_str,
            .f32_to_bits,
            .f32_from_bits,
            .f64_to_bits,
            .f64_from_bits,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_bitwise_or,
            .num_bitwise_xor,
            .num_bitwise_not,
            .num_count_one_bits,
            .num_count_leading_zero_bits,
            .num_count_trailing_zero_bits,
            .num_from_le_bytes_unchecked,
            .simd_load_16_unchecked,
            .simd_store_16_unchecked,
            .simd_append_16,
            .simd_splat,
            .simd_get_lane_unchecked,
            .simd_with_lane_unchecked,
            .simd_to_u128_bits,
            .simd_from_u128_bits,
            .simd_add_wrap,
            .simd_sub_wrap,
            .simd_add_sat,
            .simd_sub_sat,
            .simd_neg_wrap,
            .simd_abs_wrap,
            .simd_min,
            .simd_max,
            .simd_abs_diff,
            .simd_avg_rounded,
            .simd_mul_wrap,
            .simd_mul_high,
            .simd_mul_q15_sat,
            .simd_mul_wide_lo,
            .simd_mul_wide_hi,
            .simd_dot_pairs,
            .simd_dot_pairs_sat,
            .simd_sad,
            .simd_and,
            .simd_or,
            .simd_xor,
            .simd_not,
            .simd_bit_select,
            .simd_eq_lanes,
            .simd_gt_lanes,
            .simd_gte_lanes,
            .simd_bitmask,
            .simd_shl_wrap,
            .simd_shr_wrap,
            .simd_shr_zf_wrap,
            .simd_shr_rounded,
            .simd_interleave_lo,
            .simd_interleave_hi,
            .simd_even_lanes,
            .simd_odd_lanes,
            .simd_reverse_lanes,
            .simd_table_lookup,
            .simd_concat_shift_bytes,
            .simd_widen_lo,
            .simd_widen_hi,
            .simd_pairwise_add_widen,
            .simd_narrow_wrap,
            .simd_narrow_sat,
            .simd_sum_lanes,
            .simd_sum_lanes_wrap,
            .simd_clmul_lo,
            .simd_clmul_hi,
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
            .dec_to_attos,
            .dec_from_attos,
            .f32_from_str,
            .f64_from_str,
            .u8_to_i8_wrap,
            .u8_to_i8_try,
            .u8_to_i16,
            .u8_to_i32,
            .u8_to_i64,
            .u8_to_i128,
            .u8_to_u16,
            .u8_to_u32,
            .u8_to_u64,
            .u8_to_u128,
            .u8_to_f32,
            .u8_to_f64,
            .u8_to_dec,
            .i8_to_i16,
            .i8_to_i32,
            .i8_to_i64,
            .i8_to_i128,
            .i8_to_u8_wrap,
            .i8_to_u8_try,
            .i8_to_u16_wrap,
            .i8_to_u16_try,
            .i8_to_u32_wrap,
            .i8_to_u32_try,
            .i8_to_u64_wrap,
            .i8_to_u64_try,
            .i8_to_u128_wrap,
            .i8_to_u128_try,
            .i8_to_f32,
            .i8_to_f64,
            .i8_to_dec,
            .u16_to_i8_wrap,
            .u16_to_i8_try,
            .u16_to_i16_wrap,
            .u16_to_i16_try,
            .u16_to_i32,
            .u16_to_i64,
            .u16_to_i128,
            .u16_to_u8_wrap,
            .u16_to_u8_try,
            .u16_to_u32,
            .u16_to_u64,
            .u16_to_u128,
            .u16_to_f32,
            .u16_to_f64,
            .u16_to_dec,
            .i16_to_i8_wrap,
            .i16_to_i8_try,
            .i16_to_i32,
            .i16_to_i64,
            .i16_to_i128,
            .i16_to_u8_wrap,
            .i16_to_u8_try,
            .i16_to_u16_wrap,
            .i16_to_u16_try,
            .i16_to_u32_wrap,
            .i16_to_u32_try,
            .i16_to_u64_wrap,
            .i16_to_u64_try,
            .i16_to_u128_wrap,
            .i16_to_u128_try,
            .i16_to_f32,
            .i16_to_f64,
            .i16_to_dec,
            .u32_to_i8_wrap,
            .u32_to_i8_try,
            .u32_to_i16_wrap,
            .u32_to_i16_try,
            .u32_to_i32_wrap,
            .u32_to_i32_try,
            .u32_to_i64,
            .u32_to_i128,
            .u32_to_u8_wrap,
            .u32_to_u8_try,
            .u32_to_u16_wrap,
            .u32_to_u16_try,
            .u32_to_u64,
            .u32_to_u128,
            .u32_to_f32,
            .u32_to_f64,
            .u32_to_dec,
            .i32_to_i8_wrap,
            .i32_to_i8_try,
            .i32_to_i16_wrap,
            .i32_to_i16_try,
            .i32_to_i64,
            .i32_to_i128,
            .i32_to_u8_wrap,
            .i32_to_u8_try,
            .i32_to_u16_wrap,
            .i32_to_u16_try,
            .i32_to_u32_wrap,
            .i32_to_u32_try,
            .i32_to_u64_wrap,
            .i32_to_u64_try,
            .i32_to_u128_wrap,
            .i32_to_u128_try,
            .i32_to_f32,
            .i32_to_f64,
            .i32_to_dec,
            .u64_to_i8_wrap,
            .u64_to_i8_try,
            .u64_to_i16_wrap,
            .u64_to_i16_try,
            .u64_to_i32_wrap,
            .u64_to_i32_try,
            .u64_to_i64_wrap,
            .u64_to_i64_try,
            .u64_to_i128,
            .u64_to_u8_wrap,
            .u64_to_u8_try,
            .u64_to_u16_wrap,
            .u64_to_u16_try,
            .u64_to_u32_wrap,
            .u64_to_u32_try,
            .u64_to_u128,
            .u64_to_f32,
            .u64_to_f64,
            .u64_to_dec,
            .i64_to_i8_wrap,
            .i64_to_i8_try,
            .i64_to_i16_wrap,
            .i64_to_i16_try,
            .i64_to_i32_wrap,
            .i64_to_i32_try,
            .i64_to_i128,
            .i64_to_u8_wrap,
            .i64_to_u8_try,
            .i64_to_u16_wrap,
            .i64_to_u16_try,
            .i64_to_u32_wrap,
            .i64_to_u32_try,
            .i64_to_u64_wrap,
            .i64_to_u64_try,
            .i64_to_u128_wrap,
            .i64_to_u128_try,
            .i64_to_f32,
            .i64_to_f64,
            .i64_to_dec,
            .u128_to_i8_wrap,
            .u128_to_i8_try,
            .u128_to_i16_wrap,
            .u128_to_i16_try,
            .u128_to_i32_wrap,
            .u128_to_i32_try,
            .u128_to_i64_wrap,
            .u128_to_i64_try,
            .u128_to_i128_wrap,
            .u128_to_i128_try,
            .u128_to_u8_wrap,
            .u128_to_u8_try,
            .u128_to_u16_wrap,
            .u128_to_u16_try,
            .u128_to_u32_wrap,
            .u128_to_u32_try,
            .u128_to_u64_wrap,
            .u128_to_u64_try,
            .u128_to_f32,
            .u128_to_f64,
            .u128_to_dec_try_unsafe,
            .i128_to_i8_wrap,
            .i128_to_i8_try,
            .i128_to_i16_wrap,
            .i128_to_i16_try,
            .i128_to_i32_wrap,
            .i128_to_i32_try,
            .i128_to_i64_wrap,
            .i128_to_i64_try,
            .i128_to_u8_wrap,
            .i128_to_u8_try,
            .i128_to_u16_wrap,
            .i128_to_u16_try,
            .i128_to_u32_wrap,
            .i128_to_u32_try,
            .i128_to_u64_wrap,
            .i128_to_u64_try,
            .i128_to_u128_wrap,
            .i128_to_u128_try,
            .i128_to_f32,
            .i128_to_f64,
            .i128_to_dec_try_unsafe,
            .f32_to_i8_trunc,
            .f32_to_i8_try_unsafe,
            .f32_to_i16_trunc,
            .f32_to_i16_try_unsafe,
            .f32_to_i32_trunc,
            .f32_to_i32_try_unsafe,
            .f32_to_i64_trunc,
            .f32_to_i64_try_unsafe,
            .f32_to_i128_trunc,
            .f32_to_i128_try_unsafe,
            .f32_to_u8_trunc,
            .f32_to_u8_try_unsafe,
            .f32_to_u16_trunc,
            .f32_to_u16_try_unsafe,
            .f32_to_u32_trunc,
            .f32_to_u32_try_unsafe,
            .f32_to_u64_trunc,
            .f32_to_u64_try_unsafe,
            .f32_to_u128_trunc,
            .f32_to_u128_try_unsafe,
            .f32_to_f64,
            .f64_to_i8_trunc,
            .f64_to_i8_try_unsafe,
            .f64_to_i16_trunc,
            .f64_to_i16_try_unsafe,
            .f64_to_i32_trunc,
            .f64_to_i32_try_unsafe,
            .f64_to_i64_trunc,
            .f64_to_i64_try_unsafe,
            .f64_to_i128_trunc,
            .f64_to_i128_try_unsafe,
            .f64_to_u8_trunc,
            .f64_to_u8_try_unsafe,
            .f64_to_u16_trunc,
            .f64_to_u16_try_unsafe,
            .f64_to_u32_trunc,
            .f64_to_u32_try_unsafe,
            .f64_to_u64_trunc,
            .f64_to_u64_try_unsafe,
            .f64_to_u128_trunc,
            .f64_to_u128_try_unsafe,
            .f64_to_f32_wrap,
            .f64_to_f32_try_unsafe,
            .dec_to_i8_trunc,
            .dec_to_i8_try_unsafe,
            .dec_to_i16_trunc,
            .dec_to_i16_try_unsafe,
            .dec_to_i32_trunc,
            .dec_to_i32_try_unsafe,
            .dec_to_i64_trunc,
            .dec_to_i64_try_unsafe,
            .dec_to_i128_trunc,
            .dec_to_u8_trunc,
            .dec_to_u8_try_unsafe,
            .dec_to_u16_trunc,
            .dec_to_u16_try_unsafe,
            .dec_to_u32_trunc,
            .dec_to_u32_try_unsafe,
            .dec_to_u64_trunc,
            .dec_to_u64_try_unsafe,
            .dec_to_u128_trunc,
            .dec_to_u128_try_unsafe,
            .dec_to_f32_wrap,
            .dec_to_f32_try_unsafe,
            .dec_to_f64,
            .box_box,
            .box_unbox,
            .box_prepare_update,
            .erased_capture_load,
            .ptr_alloca,
            .box_alloc_zeroed,
            .ptr_store,
            .ptr_load,
            .ptr_cast,
            .compare,
            .crash,
            => try self.bindFresh(s.target),
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
        const op: PredOp = if (s.op == .num_is_lt)
            .lt
        else if (s.op == .num_is_lte)
            .lte
        else if (s.op == .num_is_gt)
            .gt
        else if (s.op == .num_is_gte)
            .gte
        else
            unreachable;

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

        if ((holds or fails) and self.live_pending) {
            // The proof may rest on an unverified length assumption; defer
            // the fold and model the compare as undecided this round.
            self.deferred_rewrites = true;
        } else if (holds or fails) {
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
        if (s.op == .num_plus_checked) {
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
        } else if (s.op == .num_minus_checked) {
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
        } else unreachable;

        if (provable) {
            if (self.live_pending) {
                self.deferred_rewrites = true;
            } else if (CheckedArithmetic.uncheckedOp(s.op)) |unchecked| {
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
        succs: collections.DenseMap(CFStmtId, []CFStmtId),

        fn deinit(self: *Graph) void {
            var it = self.succs.valueIterator();
            while (it.next()) |list| self.allocator.free(list.*);
            self.succs.deinit();
        }

        /// `a` dominates `b` when every path from the root to `b` passes
        /// through `a`: removing `a` must make `b` unreachable.
        fn dominates(self: *Graph, a: CFStmtId, b: CFStmtId) bool {
            if (a == b) return true;
            var seen = collections.DenseMap(CFStmtId, void).init(self.allocator);
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
            .succs = collections.DenseMap(CFStmtId, []CFStmtId).init(allocator),
        };
        errdefer graph.deinit();

        var join_bodies = collections.DenseMap(JoinPointId, CFStmtId).init(allocator);
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
                .assign_boxy_desc_ref => |t| try list.append(allocator, t.next),
                .assign_boxy_dict_ref => |t| try list.append(allocator, t.next),
                .assign_boxy_box => |t| try list.append(allocator, t.next),
                .assign_boxy_reuse_box => |t| try list.append(allocator, t.next),
                .assign_boxy_unbox => |t| try list.append(allocator, t.next),
                .assign_boxy_adapt => |t| try list.append(allocator, t.next),
                .assign_boxy_inspect => |t| try list.append(allocator, t.next),
                .assign_boxy_eq => |t| try list.append(allocator, t.next),
                .assign_boxy_tag => |t| try list.append(allocator, t.next),
                .assign_boxy_tag_payload => |t| try list.append(allocator, t.next),
                .assign_call_dict => |t| try list.append(allocator, t.next),
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
                .boxy_tag_match => |t| {
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
        var index_of = collections.DenseMap(NodeId, usize).init(allocator);
        defer index_of.deinit();
        const add_root = struct {
            fn add(list: *std.ArrayList(NodeId), map: *collections.DenseMap(NodeId, usize), alloc: Allocator, id: NodeId) bool {
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
