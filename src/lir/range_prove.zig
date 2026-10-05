//! Elides always-safe checks by proving unsigned value-range facts.
//!
//! Fully checked Roc lowers each safety decision into ordinary LIR: a bounds
//! test is a comparison feeding a switch whose failing arm produces the error
//! value, and integer arithmetic carries an explicit behavior family member
//! that the backends implement directly. When a dominating
//! branch already implies a check cannot fail (a decode loop's margin test
//! `cursor + 16 <= len` implies every eight-byte read at `cursor` is in
//! bounds), the check is pure overhead on every iteration.
//!
//! This pass proves such implications and rewrites only what it proves:
//!
//! - a comparison whose outcome is implied becomes a constant `Bool` tag
//! - a switch on a constant condition becomes its surviving branch
//! - an arithmetic op proved exact becomes `*_proven_cannot_overflow`
//!
//! Anything the prover cannot justify keeps its checks, so the failure mode of
//! a weak proof is missing speedup, never unsoundness.
//!
//! Facts come from three sources. A branch edge asserts its condition: inside
//! the taken arm of `switch` on `a <= b`, that ordering holds. The
//! continuation of a surviving `*_crash_on_overflow` op asserts exactness: control only
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
//! every iteration) apply to them. Facts every entry edge of a loop carries
//! about values the loop never rebinds hold throughout it; an edge is an
//! entry edge when it comes from outside the loop's body, and a jump from a
//! join nested inside that body is a back edge like any other. The pass runs proof rounds to a fixpoint
//! because folding a branch can leave a join body with a single remaining
//! jump, which lets facts flow through it on the next round.
//!
//! Only fixed-width unsigned integers up to 64 bits participate. List lengths
//! are modeled as opaque terms keyed by the list value they measure, so
//! repeated `list_len` reads of the same unmodified list unify; appending one
//! element yields the term plus one, and reserving capacity keeps it.
//!
//! The sum of two dynamic values gets a root of its own, shared by every
//! addition of the same two operands. The fact base relates that root to each
//! operand and to every other sum sharing an operand: `a + y` and `a + z`
//! order exactly as `y` and `z` do. A read at `base + offset` inside a loop
//! whose head bounds `offset + 8 <= limit` is thereby bounded by a guard on
//! `base + limit` established once before the loop, which is the shape of a
//! word-at-a-time compare over two cursors into one buffer.
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
const invariant = @import("base").invariant;
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const CheckedArithmetic = core.CheckedArithmetic;
const BodyClone = @import("body_clone.zig");
const GuardedList = LirStore.GuardedList;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const JoinPointId = LIR.JoinPointId;

/// Allocation failure raised while proving and rewriting.
pub const ResourceError = Allocator.Error;

/// Bound on proof rounds per proc. Each round can only fold branches that
/// exist, so rounds converge; this bound is a backstop, not a tuning knob.
/// A loop lowered as several joins that jump among one another (a search
/// with a restart state, say) carries a persisted fact one join further per
/// round, so the backstop must cover a chain of a couple of dozen joins.
const max_rounds: u32 = 48;
/// Bound on collected facts along one path.
const max_facts: usize = 1024;
/// Bound on symbolic value nodes per proc round.
const max_nodes: usize = 1 << 18;
/// Bound on nodes touched by one inequality query.
const query_visit_cap: usize = 64;

/// Prove and rewrite qualifying checks in every proc.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var analysis = BodyClone.AnalysisScratch.init(store.allocator);
    defer analysis.deinit();
    var pass = try Pass.init(store, layouts, store.allocator, &analysis);
    defer pass.deinit();

    const proc_count = store.procSpecCount();
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        try pass.transformProc(@enumFromInt(proc_index));
    }
}

/// Prove one procedure with task-local scratch; rewritten LIR stays in the store.
pub fn runProc(store: *LirStore, layouts: *const layout_mod.Store, proc_id: LIR.LirProcSpecId, scratch_allocator: Allocator) ResourceError!void {
    var analysis = BodyClone.AnalysisScratch.init(scratch_allocator);
    defer analysis.deinit();
    try runProcWithScratch(store, layouts, proc_id, scratch_allocator, &analysis);
}

/// Retain counting and traversal capacity across procedures and proof rounds.
pub fn runProcWithScratch(store: *LirStore, layouts: *const layout_mod.Store, proc_id: LIR.LirProcSpecId, scratch_allocator: Allocator, analysis: *BodyClone.AnalysisScratch) ResourceError!void {
    var pass = try Pass.init(store, layouts, scratch_allocator, analysis);
    defer pass.deinit();
    try pass.transformProc(proc_id);
}

/// Identifier of one symbolic value node.
const NodeId = u32;

test "range prove ordered procedure runs match whole-store constant arithmetic and no-op" {
    const testing = std.testing;
    for ([_]bool{ false, true }) |procedure_local| {
        var store = LirStore.init(testing.allocator);
        defer store.deinit();
        var layouts = try layout_mod.Store.init(testing.allocator, .u64);
        defer layouts.deinit();
        const lhs = try store.addLocal(.{ .layout_idx = .u64 });
        const rhs = try store.addLocal(.{ .layout_idx = .u64 });
        const result = try store.addLocal(.{ .layout_idx = .u64 });
        const done = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
        const add = try store.addCFStmt(.{ .assign_low_level = .{
            .target = result,
            .op = .num_int_add_crash_on_overflow,
            .rc_effect = .none(),
            .args = try store.addLocalSpan(&.{ lhs, rhs }),
            .next = done,
        } }, .test_fixture);
        const right = try store.addCFStmt(.{ .assign_literal = .{
            .target = rhs,
            .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u64 } },
            .next = add,
        } }, .test_fixture);
        const body = try store.addCFStmt(.{ .assign_literal = .{
            .target = lhs,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = right,
        } }, .test_fixture);
        _ = try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(store.procSpecCount())),
            .name = store.freshSyntheticSymbol(),
            .args = .empty(),
            .body = body,
            .ret_layout = .u64,
        }, .none);
        const arg = try store.addLocal(.{ .layout_idx = .u64 });
        const noop_body = try store.addCFStmt(.{ .ret = .{ .value = arg } }, .test_fixture);
        const noop = try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(store.procSpecCount())),
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{arg}),
            .body = noop_body,
            .ret_layout = .u64,
        }, .none);
        if (procedure_local) {
            for (0..store.procSpecCount()) |index| {
                var scratch = std.heap.ArenaAllocator.init(testing.allocator);
                defer scratch.deinit();
                try runProc(&store, &layouts, @enumFromInt(index), scratch.allocator());
            }
        } else {
            try run(&store, &layouts);
        }
        try testing.expectEqual(.num_int_add_proven_cannot_overflow, store.getCFStmt(add).assign_low_level.op);
        try testing.expectEqual(noop_body, store.getProcSpec(noop).body.?);
        try testing.expectEqual(arg, store.getCFStmt(noop_body).ret.value);
    }
}

test "range prove retargets jumps inside unreached join bodies when threading a Bool join" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    const flag = try store.addLocal(.{ .layout_idx = .bool });
    const result = try store.addLocal(.{ .layout_idx = .u64 });
    var fixture_join_ids = BodyClone.JoinParamIndex.init(testing.allocator);
    defer fixture_join_ids.deinit();
    const bool_join = fixture_join_ids.freshJoinPoint();
    const unreached_join = fixture_join_ids.freshJoinPoint();

    // join 0(flag) = switch flag { 1 => ret, _ => ret }
    const ret_true = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
    const ret_false = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
    const bool_body = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = flag,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = ret_true }}),
        .default_branch = ret_false,
    } }, .test_fixture);
    // join 1() has no jump to it, so the prescan never scans its body.
    const unreached_jump = try store.addCFStmt(.{ .jump = .{ .target = bool_join } }, .test_fixture);
    const reached_jump = try store.addCFStmt(.{ .jump = .{ .target = bool_join } }, .test_fixture);
    const unreached = try store.addCFStmt(.{ .join = .{
        .id = unreached_join,
        .params = try store.addLocalSpan(&.{}),
        .body = unreached_jump,
        .remainder = reached_jump,
    } }, .test_fixture);
    const set_result = try store.addCFStmt(.{ .assign_literal = .{
        .target = result,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = unreached,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .join = .{
        .id = bool_join,
        .params = try store.addLocalSpan(&.{flag}),
        .body = bool_body,
        .remainder = set_result,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .identity = LIR.ProcIdentity.forTest(0),
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{flag}),
        .body = body,
        .ret_layout = .u64,
    }, .none);

    var scratch = std.heap.ArenaAllocator.init(testing.allocator);
    defer scratch.deinit();
    try runProc(&store, &layouts, proc, scratch.allocator());

    var join_ids = collections.DenseMap(LIR.JoinPointId, void).init(testing.allocator);
    defer join_ids.deinit();
    var jump_targets = std.ArrayList(LIR.JoinPointId).empty;
    defer jump_targets.deinit(testing.allocator);
    var walk = try BodyClone.ReachableStmts.initWithAllocator(&store, store.getProcSpec(proc).body.?, testing.allocator);
    defer walk.deinit();
    while (try walk.next()) |stmt| {
        const cf = store.getCFStmt(stmt);
        if (cf == .join) try join_ids.put(cf.join.id, {});
        if (cf == .jump) try jump_targets.append(testing.allocator, cf.jump.target);
    }
    try testing.expect(!join_ids.contains(bool_join));
    for (jump_targets.items) |target| try testing.expect(join_ids.contains(target));
}

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

/// Comparison kinds whose branch edges yield ordering facts. Equality
/// asserts both orderings on its holding edge; inequality tightens an
/// ordering already known to be non-strict.
const PredOp = enum {
    lt,
    lte,
    gt,
    gte,
    eq,
    ne,

    fn negated(self: PredOp) PredOp {
        return switch (self) {
            .lt => .gte,
            .lte => .gt,
            .gt => .lte,
            .gte => .lt,
            .eq => .ne,
            .ne => .eq,
        };
    }
};

/// The comparison that defined a Bool local, kept so a later switch on that
/// local can assert the comparison (or its negation) along each arm.
const Pred = struct {
    op: PredOp,
    a: NodeId,
    b: NodeId,
};

/// The arithmetic question that defined a Bool local. Its false switch edge
/// proves that the matching wrapping operation is exact on that path.
const OverflowPred = struct {
    operation: CheckedArithmetic.Operation,
    lhs: NodeId,
    rhs: NodeId,
    operand_layout: layout_mod.Idx,
    predicate_stmt: CFStmtId,
};

/// A false overflow-predicate edge that proves matching arithmetic exact.
const NoOverflowFact = struct {
    predicate: OverflowPred,
    switch_stmt: CFStmtId,
    edge_head: CFStmtId,
};

/// A same-sign constant checked chain whose defining statement can be
/// combined with the immediately following checked operation.
const ArithmeticChain = struct {
    operation: CheckedArithmetic.Operation,
    base: LocalId,
    constant: i128,
    stmt: CFStmtId,
};

/// Symbolic knowledge about one local at one program point.
const Binding = struct {
    node: NodeId,
    pred: ?Pred = null,
    overflow_pred: ?OverflowPred = null,
    arithmetic_chain: ?ArithmeticChain = null,
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
/// A fact without its origin, for recognizing one seeded twice.
const FactKey = struct {
    a: NodeId,
    b: NodeId,
    c: i128,
    assumed: u64,
};

const Fact = struct {
    a: NodeId,
    b: NodeId,
    c: i128,
    origin: FactOrigin,
    /// The round's pending length assumptions this fact rests on, as bits
    /// of their seeding order (bit 63 stands for any assumption past the
    /// first 63). A fact derived through the fact graph inherits the bits of
    /// every fact the derivation touched.
    assumed: u64 = 0,
};

/// Bit for an assumption whose index exceeds the mask; nothing resting on
/// it can verify this round.
const unknown_assumption_bit: u64 = 1 << 63;

const EdgeFact = union(enum) {
    ordering: Fact,
    no_overflow: NoOverflowFact,
    /// Both orderings between the two nodes hold along this edge.
    equal: EqualityEdge,
    /// The two nodes differ along this edge: an ordering between them known
    /// to be non-strict becomes strict.
    unequal: EqualityEdge,
};

const EqualityEdge = struct {
    a: NodeId,
    b: NodeId,
    switch_stmt: CFStmtId,
};

/// A root standing for the mathematical sum of two other roots.
const SumRoot = struct {
    root: NodeId,
    a: NodeId,
    b: NodeId,
    /// The previous sum touching this sum's root, `a`, and `b`, in that
    /// order, under `Pass.sum_heads`; `no_sum` ends each chain.
    prev: [3]u32 = .{ no_sum, no_sum, no_sum },
    /// `Pass.fact_epoch` when this sum's facts were last stated on the
    /// path; another value means they may be missing or stale.
    fresh: u32 = 0,
};
const no_sum: u32 = std.math.maxInt(u32);

/// Constant bounds of a root under the path facts, with the assumptions
/// each bound's derivation touched.
const RootBounds = struct {
    lo: i128,
    hi: i128,
    lo_assumed: u64,
    hi_assumed: u64,
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
    no_overflow_facts_len: usize,
    undo_len: usize,
    /// Path fact asserted by the branch edge leading here, if any.
    edge_fact: ?EdgeFact,
};

/// One jump statement and its target, collected during the pre-scan.
const JumpRecord = struct {
    target: JoinPointId,
    stmt: CFStmtId,
};

const ProofClaim = union(enum) {
    ordering: struct { a: NodeId, b: NodeId, m: i128 },
    no_overflow: NoOverflowFact,
};

/// Debug-only record of one applied rewrite and the path claim that justified
/// it. Certified independently at the end of the round.
const ProofRecord = struct {
    stmt: CFStmtId,
    claim: ProofClaim,
    facts_start: u32,
    facts_len: u32,
};

/// One synthesized upper bound `value <= root + c` carried through a merge.
const MeetBound = struct {
    root: NodeId,
    c: i128,
    /// Pending assumptions the bound's derivation touched.
    assumed: u64 = 0,
};

/// Best slack a bounds query has found to one root, with the pending
/// assumptions the path that found it touched.
const QueryBest = struct {
    c: i128,
    assumed: u64 = 0,
    /// On the `relaxFrom` worklist already.
    queued: bool = false,
    /// Times the root has come off the `relaxFrom` worklist.
    visits: u32 = 0,
};

/// Index links of one path fact, see `Pass.fact_links`.
const FactLink = struct {
    fwd_prev: u32,
    bwd_prev: u32,
};
const no_fact: u32 = std.math.maxInt(u32);

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
    /// Rounds in a row this bound came back weaker than it was persisted.
    grew: u8 = 0,
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
    /// Whether the bounded quantity is a list parameter's length or an
    /// integer parameter's value.
    kind: enum(u8) { length, value } = .length,
    status: enum(u8) { pending, verified, dead },
    /// Re-derived on every captured edge this round.
    hit: bool,
    /// This round's bit for the assumption, when seeded pending.
    assume_bit: u64 = 0,
    /// Assumptions the re-derivations rested on. An invariant verifies once
    /// every assumption it used is itself or has verified; resting on one
    /// that died kills it too.
    hit_deps: u64 = 0,
    /// The progress epoch in which this invariant last failed verification;
    /// it is seeded again only after a later epoch rewrites a statement or
    /// persists a new bound and reaches its fixpoint.
    died_epoch: u32 = 0,
};

/// Cross-round bounds of one loop parameter: the bounds every jump into its
/// join proved in a round, and the invariants assumed and verified across
/// rounds. Each list holds up to `meet_bound_cap`; storage follows use,
/// since a procedure persists one of these per loop parameter.
const LoopBounds = struct {
    items: []StableBound = &.{},
    len: usize = 0,
    /// Lower bounds `base <= value + c` of an integer parameter.
    lower_items: []StableBound = &.{},
    lower_len: usize = 0,
    len_items: []LenInvariant = &.{},
    len_count: usize = 0,

    fn ensureCapacity(self: *LoopBounds, allocator: Allocator, bounds: usize, lower: usize, invariants: usize) ResourceError!void {
        if (self.items.len < bounds) {
            const grown = try allocator.alloc(StableBound, bounds);
            @memcpy(grown[0..self.len], self.items[0..self.len]);
            allocator.free(self.items);
            self.items = grown;
        }
        if (self.lower_items.len < lower) {
            const grown = try allocator.alloc(StableBound, lower);
            @memcpy(grown[0..self.lower_len], self.lower_items[0..self.lower_len]);
            allocator.free(self.lower_items);
            self.lower_items = grown;
        }
        if (self.len_items.len < invariants) {
            const grown = try allocator.alloc(LenInvariant, invariants);
            @memcpy(grown[0..self.len_count], self.len_items[0..self.len_count]);
            allocator.free(self.len_items);
            self.len_items = grown;
        }
    }

    fn deinit(self: *LoopBounds, allocator: Allocator) void {
        allocator.free(self.items);
        allocator.free(self.lower_items);
        allocator.free(self.len_items);
    }

    /// Copy the live prefixes.
    fn assign(dst: *LoopBounds, allocator: Allocator, src: *const LoopBounds) ResourceError!void {
        try dst.ensureCapacity(allocator, src.len, src.lower_len, src.len_count);
        @memcpy(dst.items[0..src.len], src.items[0..src.len]);
        dst.len = src.len;
        @memcpy(dst.lower_items[0..src.lower_len], src.lower_items[0..src.lower_len]);
        dst.lower_len = src.lower_len;
        @memcpy(dst.len_items[0..src.len_count], src.len_items[0..src.len_count]);
        dst.len_count = src.len_count;
    }
};

/// Fixed-capacity list of synthesized bounds.
/// Roots a bounds query can reach before normalization; wider than a meet
/// keeps, so the constant bound among them is never crowded out.
const query_bound_cap: usize = 32;

const QueryBounds = struct {
    items: [query_bound_cap]MeetBound = undefined,
    len: usize = 0,

    fn append(self: *QueryBounds, bound: MeetBound) void {
        if (self.len < query_bound_cap) {
            self.items[self.len] = bound;
            self.len += 1;
        }
    }

    fn slice(self: *const QueryBounds) []const MeetBound {
        return self.items[0..self.len];
    }
};

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
    /// When set, the entry describes this field of the struct the local
    /// holds rather than the local itself, keyed by the struct's root in
    /// `field_values`.
    field: ?u32 = null,
    root: NodeId,
    off_lo: i128,
    off_hi: i128,
    valid: bool,
    bounds: MeetBounds,
    /// Lower bounds `root <= value + c` provable for an integer local on
    /// every captured edge, in the same fact form as `len_bounds`.
    lower: MeetBounds,
    /// The same bounds provable on any single captured edge; invariant
    /// candidates for an integer loop parameter are born here.
    lower_any: MeetBounds,
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
const merge_env_cap: usize = 128;
/// Struct fields considered when a struct local's fields meet.
const max_struct_meet_fields: u32 = 16;

/// Accumulated meet state of one merge head: the facts present on every
/// captured incoming edge, and the per-local value meet of the path
/// environment. A merge only seeds its region when every predecessor was
/// captured, so a missing edge (a loop back edge captured mid-walk, say)
/// keeps the merge at bottom for the round.
const MergeState = struct {
    /// Captured this round. A state from an earlier round keeps its lists'
    /// memory: the pass runs on an arena, where a freed list returns
    /// nothing, so the lists are cleared and refilled rather than remade.
    live: bool,
    captures: u32,
    facts: std.ArrayList(Fact),
    /// The all-edge meet of the facts in round-stable form, for persisting
    /// across rounds; `facts` keeps the raw meet for in-round seeding, which
    /// only helps when the edges share the head's region and its node ids.
    stable: LoopFacts,
    env: std.ArrayList(EnvMeet),
    /// Facts held by every captured edge arriving from OUTSIDE the merge
    /// head's own region, in round-stable form. For a loop join these are
    /// its entry edges; a fact between round-stable single-assignment values
    /// that holds on entry is a loop invariant outright, because nothing in
    /// the loop can reassign the values it relates. Entry edges may arrive
    /// from different walk regions, whose nodes for the same value differ,
    /// so the meet is taken on what the nodes denote rather than on node ids.
    entry_captures: u32,
    entry_stable: LoopFacts,
};

/// One endpoint of a cross-round persisted fact, in round-stable form.
const StableTerm = union(enum) {
    /// The value of this integer local: a single-assignment local anywhere,
    /// or a reassignable local within a loop whose body never assigns it.
    value_of: LocalId,
    /// The length of the list held by this local, under the same rule.
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
    assumed: u64 = 0,
    /// Rounds in a row this fact came back weaker than it was persisted.
    grew: u8 = 0,
};

/// Bound on persisted facts per loop join.
const loop_fact_cap: usize = 256;

/// A list of stable facts with room for `loop_fact_cap`; `items` is the
/// storage held, `len` the live prefix. Thousands of merge heads each
/// persist one, most well under the cap, so storage follows use: it grows
/// by half at least, which bounds the copies an arena keeps.
const LoopFacts = struct {
    items: []StableFact = &.{},
    len: usize = 0,

    fn ensureCapacity(self: *LoopFacts, allocator: Allocator, needed: usize) ResourceError!void {
        if (self.items.len >= needed) return;
        const grown = try allocator.alloc(StableFact, @min(loop_fact_cap, @max(needed, self.items.len + self.items.len / 2)));
        @memcpy(grown[0..self.len], self.items[0..self.len]);
        allocator.free(self.items);
        self.items = grown;
    }

    fn deinit(self: *LoopFacts, allocator: Allocator) void {
        allocator.free(self.items);
    }

    /// Copy the live prefix.
    fn assign(dst: *LoopFacts, allocator: Allocator, src: *const LoopFacts) ResourceError!void {
        try dst.ensureCapacity(allocator, src.len);
        @memcpy(dst.items[0..src.len], src.items[0..src.len]);
        dst.len = src.len;
    }
};

/// Identity of a stable fact without its bookkeeping, for the sets the
/// stabilizations and meets dedupe and intersect through.
const StableFactKey = struct {
    a: StableTerm,
    b: StableTerm,
    c: i128,
};

fn stableFactKey(fact: StableFact) StableFactKey {
    return .{ .a = fact.a, .b = fact.b, .c = fact.c };
}

/// Bound on persisted per-merge env locals.
const merge_env_persist_cap: usize = 128;

/// One local's stable upper bounds carried across rounds for a merge head.
const StoredEnvBound = struct {
    local: LocalId,
    bounds: [meet_bound_cap]StableBound,
    len: usize,
    /// Lower bounds `base <= value + c`, a constant base standing for zero.
    lower: [meet_bound_cap]StableBound,
    lower_len: usize,
};

/// Last round's stabilized env meet of one merge head, seeded when the
/// region must walk before its captures complete.
const MergeEnvBounds = struct {
    items: []StoredEnvBound = &.{},
    len: usize = 0,

    /// Storage follows use, as for `LoopFacts`.
    fn ensureCapacity(self: *MergeEnvBounds, allocator: Allocator, needed: usize) ResourceError!void {
        if (self.items.len >= needed) return;
        const grown = try allocator.alloc(StoredEnvBound, @min(merge_env_persist_cap, @max(needed, self.items.len + self.items.len / 2)));
        @memcpy(grown[0..self.len], self.items[0..self.len]);
        allocator.free(self.items);
        self.items = grown;
    }

    fn deinit(self: *MergeEnvBounds, allocator: Allocator) void {
        allocator.free(self.items);
    }

    /// Copy the live prefix.
    fn assign(dst: *MergeEnvBounds, allocator: Allocator, src: *const MergeEnvBounds) ResourceError!void {
        try dst.ensureCapacity(allocator, src.len);
        @memcpy(dst.items[0..src.len], src.items[0..src.len]);
        dst.len = src.len;
    }

    /// Whether two meets persist the same bounds for the same locals. The
    /// entries and the bounds within one are compared as sets: their order
    /// follows the walk that produced them, which a seeded fact's position
    /// can shift from round to round without any bound changing.
    fn sameBounds(previous: *const MergeEnvBounds, current: *const MergeEnvBounds) bool {
        if (previous.len != current.len) return false;
        for (current.items[0..current.len]) |new| {
            var matched = false;
            for (previous.items[0..previous.len]) |old| {
                if (old.local != new.local) continue;
                matched = sameBoundSet(old.bounds[0..old.len], new.bounds[0..new.len]) and
                    sameBoundSet(old.lower[0..old.lower_len], new.lower[0..new.lower_len]);
                break;
            }
            if (!matched) return false;
        }
        return true;
    }
};

/// Whether two short bound lists hold the same bounds in any order.
fn sameBoundSet(previous: []const StableBound, current: []const StableBound) bool {
    return previous.len == current.len and boundsWithin(current, previous) and boundsWithin(previous, current);
}

fn boundsWithin(inner: []const StableBound, outer: []const StableBound) bool {
    for (inner) |bound| {
        var found = false;
        for (outer) |other| {
            if (std.meta.eql(bound, other)) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}

const Pass = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    allocator: Allocator,

    // Per-proc, per-round state. Reset by `resetRound`.
    nodes: std.ArrayList(Node),
    facts: std.ArrayList(Fact),
    no_overflow_facts: std.ArrayList(NoOverflowFact),
    global_env: collections.DenseMap(LocalId, Binding),
    path_env: collections.DenseMap(LocalId, Binding),
    undo: std.ArrayList(Undo),
    len_terms: collections.DenseMap(NodeId, NodeId),
    assign_counts: collections.DenseMap(LocalId, u32),
    /// Source local of every `ref.local` alias, so a stable term can name
    /// the value a chain of single-assignment aliases denotes rather than
    /// whichever alias first computed it.
    alias_of: collections.DenseMap(LocalId, LocalId),
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
    /// The round's shared root for constant bounds, made on first use.
    zero_node: ?NodeId = null,
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
    /// Roots of constructed structs, whose fields `field_values` records.
    struct_roots: collections.DenseMap(NodeId, void),
    /// The `field_values` keys whose field holds a tracked integer: only
    /// those meet as scalars at a merge. A list field keeps its value's
    /// length term and must not be replaced by a met scalar.
    int_fields: std.AutoHashMap(u64, void),
    /// The node each loop parameter was bound to when its join's body was
    /// seeded this round, keyed by `loopBoundKey(join, param)`. A back edge
    /// into that same join carrying that very value brings the parameter
    /// back unchanged, so the join's entry edges' bounds hold on it too.
    seeded_param_roots: std.AutoHashMap(u64, NodeId),
    /// Roots of those nodes, so merges keep a parameter's identity even
    /// before any fact mentions it.
    seeded_param_root_set: collections.DenseMap(NodeId, void),
    /// Sum roots by operand root pair, so every addition of the same two
    /// values shares one root for the round.
    sum_roots: std.AutoHashMap(u64, NodeId),
    sums: std.ArrayList(SumRoot),
    /// Per root, the most recent sum whose root or operand it is this
    /// round, or `no_sum`: the sums' derived facts touch only these roots,
    /// so a query reaching none of them cannot be changed by deriving
    /// them. Indexed by root; grown as sums appear.
    sum_heads: std.ArrayList(u32),
    /// Sums whose facts the current query needs stated, by index.
    refresh_set: std.ArrayList(u32),
    /// Per sum, the `refresh_stamp` under which it joined `refresh_set`.
    refresh_marks: std.ArrayList(u32),
    refresh_stamp: u32 = 0,
    /// Counts the changes to the path's facts (a fact added outside a
    /// refresh, or the path rewound) that can date a sum's stated facts.
    fact_epoch: u32 = 1,
    /// Per root in the sums' closure, its bounds from `boundSumRoots`.
    root_bounds: collections.DenseMap(NodeId, RootBounds),
    /// Per path fact, whether `boundSumRoots` has gathered it.
    edge_gathered: std.ArrayList(bool),
    /// Whether the last `relaxFrom` reached a root `sum_heads` lists.
    reached_sum_end: bool = false,
    refreshing_sums: bool = false,
    /// Reassignable locals bound to a root (their value, or their list's
    /// length term) this round. A loop whose body never assigns such a
    /// local may treat entry facts about it as invariants; the binding is
    /// re-checked at use because these are not rewound with the path.
    path_value_roots: collections.DenseMap(NodeId, LocalId),
    path_len_roots: collections.DenseMap(NodeId, LocalId),
    /// (loop join id, local) pairs where the local is assigned on some path
    /// around the loop: from the join's body back to a jump to the join.
    join_assigned: std.AutoHashMap(u64, void),
    /// Per loop join, how many of its jumps are back edges (jumps on a path
    /// around the loop); the rest are entry edges.
    back_jumps: collections.DenseMap(JoinPointId, u32),
    /// The merge head whose region is currently being walked; captures into
    /// it from within are its own back or interior edges.
    current_region: ?CFStmtId,
    /// Innermost join whose body lexically contains each statement. A jump to
    /// a loop head from a region inside that loop's body is a back edge even
    /// when a nested join's region lies between; only jumps from outside the
    /// body are entry edges.
    enclosing_join: collections.DenseMap(CFStmtId, JoinPointId),
    /// For each join declared inside another join's body, that outer join.
    join_parent: collections.DenseMap(JoinPointId, JoinPointId),
    new_loop_bounds: bool,
    /// An unverified invariant was seeded this round: nothing is persisted
    /// from the round, and a rewrite whose proof rests on an assumption
    /// (see `proof_assumed`) waits until the assumption is promoted or
    /// discarded.
    live_pending: bool,
    /// A provable rewrite waited on an assumption; forces another round.
    deferred_rewrites: bool,
    /// Counts the rounds of this procedure that rewrote a statement or
    /// persisted new bounds, so a failed invariant can tell whether the
    /// facts have grown since it failed.
    progress_epoch: u32,
    max_join_id: u32,
    scratch: std.ArrayList(CFStmtId),
    /// Reachable statements and predecessors for the loop-cycle scans.
    loop_scan: LoopScan,
    query_best: collections.DenseMap(NodeId, QueryBest),
    /// Worklist of `relaxFrom`: roots whose best slack improved and whose
    /// edges are still to be relaxed.
    query_queue: std.ArrayList(NodeId),
    /// The facts under the root `relaxFrom` is relaxing, oldest last.
    query_edges: std.ArrayList(u32),
    /// Per path fact, the previous fact with the same left root and the
    /// previous with the same right root, so a query walks only the facts
    /// touching the roots it reaches. Parallel to `facts`.
    fact_links: std.ArrayList(FactLink),
    /// Per root, the most recent path fact whose left root (`fwd_heads`)
    /// or right root (`bwd_heads`) it is, or `no_fact`. Indexed by root;
    /// grown as roots appear.
    fwd_heads: std.ArrayList(u32),
    bwd_heads: std.ArrayList(u32),
    /// The facts seeded into the region so far, while `seeding`.
    fact_seen: std.AutoHashMap(FactKey, void),
    seeding: bool = false,
    /// Positions of stable facts by identity, for the stabilizations and
    /// the merge meets. Scratch, cleared by each user.
    stable_index: std.AutoHashMap(StableFactKey, u32),
    /// A stabilization's qualifying facts before the cap applies.
    stable_candidates: std.ArrayList(StableFact),
    /// The identities a head persisted last round, while a stabilization
    /// over the cap keeps them first.
    previous_keys: std.AutoHashMap(StableFactKey, void),
    /// `dropImpliedCandidates` scratch: the candidates' indices sorted by
    /// endpoints and constant, and which candidates another implies.
    candidate_order: std.ArrayList(u32),
    candidate_implied: std.ArrayList(bool),
    /// Stable-form scratch for a captured edge's facts, its entry facts,
    /// and a persisted meet under construction; heap-allocated once, as
    /// each is sized for its cap.
    stable_scratch: *LoopFacts,
    entry_scratch: *LoopFacts,
    persist_scratch: *LoopFacts,
    env_scratch: *MergeEnvBounds,
    /// A loop parameter's bounds under construction, sized for the caps.
    bounds_scratch: LoopBounds,
    /// Assumption bits of the facts the current fact-graph query relaxed
    /// through; reset by each top-level query.
    query_used: u64 = 0,
    /// Assumption bits every `proveLe` since the last reset relaxed
    /// through: a rewrite's proof rests on an unverified assumption
    /// exactly when this is nonzero after its queries, and only such a
    /// rewrite waits for the assumption's verification.
    proof_assumed: u64 = 0,
    /// Pending assumptions seeded so far this round.
    assumption_count: u8 = 0,
    rewrites: u32,
    // Debug-only certification state; unused (and empty) in release builds.
    proof_records: std.ArrayList(ProofRecord),
    proof_facts: std.ArrayList(Fact),
    last_claim: ?ProofClaim,
    read_counts: ?BodyClone.ReadCounts,
    analysis: *BodyClone.AnalysisScratch,

    fn init(store: *LirStore, layouts: *const layout_mod.Store, allocator: Allocator, analysis: *BodyClone.AnalysisScratch) ResourceError!Pass {
        const stable_scratch = try allocator.create(LoopFacts);
        errdefer allocator.destroy(stable_scratch);
        stable_scratch.* = .{};
        try stable_scratch.ensureCapacity(allocator, loop_fact_cap);
        errdefer stable_scratch.deinit(allocator);
        const entry_scratch = try allocator.create(LoopFacts);
        errdefer allocator.destroy(entry_scratch);
        entry_scratch.* = .{};
        try entry_scratch.ensureCapacity(allocator, loop_fact_cap);
        errdefer entry_scratch.deinit(allocator);
        const persist_scratch = try allocator.create(LoopFacts);
        errdefer allocator.destroy(persist_scratch);
        persist_scratch.* = .{};
        try persist_scratch.ensureCapacity(allocator, loop_fact_cap);
        errdefer persist_scratch.deinit(allocator);
        const env_scratch = try allocator.create(MergeEnvBounds);
        errdefer allocator.destroy(env_scratch);
        env_scratch.* = .{};
        try env_scratch.ensureCapacity(allocator, merge_env_persist_cap);
        errdefer env_scratch.deinit(allocator);
        var bounds_scratch = LoopBounds{};
        errdefer bounds_scratch.deinit(allocator);
        try bounds_scratch.ensureCapacity(allocator, meet_bound_cap, meet_bound_cap, meet_bound_cap);
        return .{
            .store = store,
            .layouts = layouts,
            .allocator = allocator,
            .nodes = .empty,
            .facts = .empty,
            .no_overflow_facts = .empty,
            .global_env = collections.DenseMap(LocalId, Binding).init(allocator),
            .path_env = collections.DenseMap(LocalId, Binding).init(allocator),
            .undo = .empty,
            .len_terms = collections.DenseMap(NodeId, NodeId).init(allocator),
            .assign_counts = collections.DenseMap(LocalId, u32).init(allocator),
            .alias_of = collections.DenseMap(LocalId, LocalId).init(allocator),
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
            .struct_roots = collections.DenseMap(NodeId, void).init(allocator),
            .int_fields = std.AutoHashMap(u64, void).init(allocator),
            .seeded_param_roots = std.AutoHashMap(u64, NodeId).init(allocator),
            .seeded_param_root_set = collections.DenseMap(NodeId, void).init(allocator),
            .sum_roots = std.AutoHashMap(u64, NodeId).init(allocator),
            .sum_heads = .empty,
            .refresh_set = .empty,
            .refresh_marks = .empty,
            .root_bounds = collections.DenseMap(NodeId, RootBounds).init(allocator),
            .edge_gathered = .empty,
            .sums = .empty,
            .path_value_roots = collections.DenseMap(NodeId, LocalId).init(allocator),
            .path_len_roots = collections.DenseMap(NodeId, LocalId).init(allocator),
            .join_assigned = std.AutoHashMap(u64, void).init(allocator),
            .back_jumps = collections.DenseMap(JoinPointId, u32).init(allocator),
            .current_region = null,
            .enclosing_join = collections.DenseMap(CFStmtId, JoinPointId).init(allocator),
            .join_parent = collections.DenseMap(JoinPointId, JoinPointId).init(allocator),
            .new_loop_bounds = false,
            .live_pending = false,
            .deferred_rewrites = false,
            .progress_epoch = 0,
            .max_join_id = 0,
            .scratch = .empty,
            .loop_scan = LoopScan.init(allocator),
            .query_best = collections.DenseMap(NodeId, QueryBest).init(allocator),
            .query_queue = .empty,
            .query_edges = .empty,
            .fact_links = .empty,
            .fwd_heads = .empty,
            .bwd_heads = .empty,
            .fact_seen = std.AutoHashMap(FactKey, void).init(allocator),
            .stable_index = std.AutoHashMap(StableFactKey, u32).init(allocator),
            .stable_candidates = .empty,
            .previous_keys = std.AutoHashMap(StableFactKey, void).init(allocator),
            .candidate_order = .empty,
            .candidate_implied = .empty,
            .stable_scratch = stable_scratch,
            .entry_scratch = entry_scratch,
            .persist_scratch = persist_scratch,
            .env_scratch = env_scratch,
            .bounds_scratch = bounds_scratch,
            .rewrites = 0,
            .proof_records = .empty,
            .proof_facts = .empty,
            .last_claim = null,
            .read_counts = null,
            .analysis = analysis,
        };
    }

    fn deinit(self: *Pass) void {
        self.nodes.deinit(self.allocator);
        self.facts.deinit(self.allocator);
        self.no_overflow_facts.deinit(self.allocator);
        self.global_env.deinit();
        self.path_env.deinit();
        self.undo.deinit(self.allocator);
        self.len_terms.deinit();
        self.assign_counts.deinit();
        self.alias_of.deinit();
        self.pred_counts.deinit();
        self.jump_counts.deinit();
        self.join_stmts.deinit();
        self.visited.deinit();
        self.region_seen.deinit();
        self.regions.deinit(self.allocator);
        self.frames.deinit(self.allocator);
        self.joins_in_order.deinit(self.allocator);
        self.jump_records.deinit(self.allocator);
        self.freeMergeStates();
        self.merge_states.deinit();
        self.body_joins.deinit();
        self.len_roots.deinit();
        self.value_roots.deinit();
        self.freePersisted();
        self.loop_bounds.deinit();
        self.loop_facts.deinit();
        self.merge_facts.deinit();
        self.merge_env.deinit();
        self.global_facts.deinit(self.allocator);
        self.field_values.deinit();
        self.struct_roots.deinit();
        self.int_fields.deinit();
        self.seeded_param_roots.deinit();
        self.seeded_param_root_set.deinit();
        self.sum_roots.deinit();
        self.sum_heads.deinit(self.allocator);
        self.refresh_set.deinit(self.allocator);
        self.refresh_marks.deinit(self.allocator);
        self.root_bounds.deinit();
        self.edge_gathered.deinit(self.allocator);
        self.sums.deinit(self.allocator);
        self.path_value_roots.deinit();
        self.path_len_roots.deinit();
        self.join_assigned.deinit();
        self.back_jumps.deinit();
        self.enclosing_join.deinit();
        self.join_parent.deinit();
        self.scratch.deinit(self.allocator);
        self.loop_scan.deinit(self.allocator);
        self.query_best.deinit();
        self.query_queue.deinit(self.allocator);
        self.query_edges.deinit(self.allocator);
        self.fact_links.deinit(self.allocator);
        self.fwd_heads.deinit(self.allocator);
        self.bwd_heads.deinit(self.allocator);
        self.fact_seen.deinit();
        self.stable_index.deinit();
        self.stable_candidates.deinit(self.allocator);
        self.previous_keys.deinit();
        self.candidate_order.deinit(self.allocator);
        self.candidate_implied.deinit(self.allocator);
        self.stable_scratch.deinit(self.allocator);
        self.allocator.destroy(self.stable_scratch);
        self.entry_scratch.deinit(self.allocator);
        self.allocator.destroy(self.entry_scratch);
        self.persist_scratch.deinit(self.allocator);
        self.allocator.destroy(self.persist_scratch);
        self.env_scratch.deinit(self.allocator);
        self.allocator.destroy(self.env_scratch);
        self.bounds_scratch.deinit(self.allocator);
        self.proof_records.deinit(self.allocator);
        self.proof_facts.deinit(self.allocator);
        if (self.read_counts) |*counts| counts.deinit();
    }

    fn resetRound(self: *Pass) void {
        self.assumption_count = 0;
        self.query_used = 0;
        self.nodes.clearRetainingCapacity();
        self.zero_node = null;
        self.truncateFacts(0);
        self.no_overflow_facts.clearRetainingCapacity();
        self.global_facts.clearRetainingCapacity();
        self.field_values.clearRetainingCapacity();
        self.struct_roots.clearRetainingCapacity();
        self.int_fields.clearRetainingCapacity();
        self.seeded_param_roots.clearRetainingCapacity();
        self.seeded_param_root_set.clearRetainingCapacity();
        self.sum_roots.clearRetainingCapacity();
        self.sums.clearRetainingCapacity();
        self.refresh_marks.clearRetainingCapacity();
        @memset(self.sum_heads.items, no_sum);
        self.path_value_roots.clearRetainingCapacity();
        self.path_len_roots.clearRetainingCapacity();
        self.global_env.clearRetainingCapacity();
        self.path_env.clearRetainingCapacity();
        self.undo.clearRetainingCapacity();
        self.len_terms.clearRetainingCapacity();
        self.assign_counts.clearRetainingCapacity();
        self.alias_of.clearRetainingCapacity();
        self.pred_counts.clearRetainingCapacity();
        self.jump_counts.clearRetainingCapacity();
        self.join_stmts.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
        self.region_seen.clearRetainingCapacity();
        self.regions.clearRetainingCapacity();
        self.frames.clearRetainingCapacity();
        self.joins_in_order.clearRetainingCapacity();
        self.jump_records.clearRetainingCapacity();
        self.retireMergeStates();
        self.body_joins.clearRetainingCapacity();
        self.enclosing_join.clearRetainingCapacity();
        self.join_parent.clearRetainingCapacity();
        self.join_assigned.clearRetainingCapacity();
        self.back_jumps.clearRetainingCapacity();
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

    /// Magnitude past which an accumulated slack can only mean the facts
    /// contradict one another (a negative cycle, on a path no execution
    /// takes): every genuine bound stays within a few word-widths of zero,
    /// so clamping here keeps the arithmetic downstream in range while
    /// preserving the contradiction's effect on every query.
    const slack_limit: i128 = 1 << 100;

    fn clampSlack(x: i128) i128 {
        return @max(-slack_limit, @min(slack_limit, x));
    }

    /// State the facts of the sums in `refresh_set` on the path: each
    /// operand sits below the sum by the other operand's proven floor and
    /// above it by the other's proven ceiling. A guard `pos + span <= len`
    /// followed by `span >= 5` thereby puts `pos` five below the length.
    ///
    /// The floors and ceilings are the fixpoint of the facts around these
    /// sums together with their own relations (a sum's ceiling is its
    /// operands' ceilings added, an operand's ceiling the sum's less the
    /// other operand's floor), found by `boundSumRoots`, so one derivation
    /// states each sum's facts at their tightest. Only the sums a query
    /// reaches are stated: a round's sums come from every region walked,
    /// and most touch nothing on the current path.
    fn refreshSums(self: *Pass) ResourceError!void {
        self.refreshing_sums = true;
        defer self.refreshing_sums = false;
        try self.boundSumRoots();
        for (self.refresh_set.items) |index| {
            const sum = &self.sums.items[index];
            const a = self.root_bounds.get(sum.a).?;
            const b = self.root_bounds.get(sum.b).?;
            _ = try self.addFactOnce(.{ .a = sum.a, .b = sum.root, .c = -b.lo, .origin = .meet, .assumed = b.lo_assumed });
            _ = try self.addFactOnce(.{ .a = sum.b, .b = sum.root, .c = -a.lo, .origin = .meet, .assumed = a.lo_assumed });
            _ = try self.addFactOnce(.{ .a = sum.root, .b = sum.a, .c = b.hi, .origin = .meet, .assumed = b.hi_assumed });
            _ = try self.addFactOnce(.{ .a = sum.root, .b = sum.b, .c = a.hi, .origin = .meet, .assumed = a.hi_assumed });
            sum.fresh = self.fact_epoch;
        }
    }

    /// Begin gathering sums into `refresh_set`.
    fn beginRefreshSet(self: *Pass) ResourceError!void {
        self.refresh_set.clearRetainingCapacity();
        self.refresh_stamp +%= 1;
        if (self.refresh_marks.items.len < self.sums.items.len) {
            try self.refresh_marks.appendNTimes(self.allocator, 0, self.sums.items.len - self.refresh_marks.items.len);
        }
    }

    /// Gather the sums touching `root` whose facts are not current.
    fn gatherStaleSums(self: *Pass, root: NodeId) ResourceError!void {
        if (root >= self.sum_heads.items.len) return;
        var index = self.sum_heads.items[root];
        while (index != no_sum) {
            const sum = self.sums.items[index];
            if (sum.fresh != self.fact_epoch and self.refresh_marks.items[index] != self.refresh_stamp) {
                self.refresh_marks.items[index] = self.refresh_stamp;
                try self.refresh_set.append(self.allocator, index);
            }
            index = sum.prev[sumSlot(sum, root)];
        }
    }

    /// Which of a sum's three roots `root` is, as an index into `prev`.
    fn sumSlot(sum: SumRoot, root: NodeId) usize {
        if (sum.root == root) return 0;
        if (sum.a == root) return 1;
        return 2;
    }

    /// State the facts of the stale sums touching any of `roots`.
    fn refreshSumsTouching(self: *Pass, roots: []const NodeId) ResourceError!void {
        try self.beginRefreshSet();
        for (roots) |root| try self.gatherStaleSums(root);
        if (self.refresh_set.items.len != 0) try self.refreshSums();
    }

    /// Tightest constant bounds of the `refresh_set` sums' roots, their
    /// operands, and the roots those rest on, into `root_bounds`. The roots
    /// are the closure of the sums' roots over the fact links in both
    /// directions, each
    /// fact relaxed both ways: from `a <= b + c`, `a`'s ceiling is at most
    /// `b`'s plus `c` and `b`'s floor at least `a`'s less `c`. A constant
    /// root ends the closure: its bound is its value, and a fact onward
    /// from it could only tighten that into a contradiction. The sums'
    /// relations relax alongside the facts, and everything repeats until
    /// nothing moves, at most `query_visit_cap` times, which ends it on a
    /// negative cycle. Each bound carries the assumptions of the facts
    /// that produced it.
    fn boundSumRoots(self: *Pass) ResourceError!void {
        self.root_bounds.clearRetainingCapacity();
        self.query_queue.clearRetainingCapacity();
        self.query_edges.clearRetainingCapacity();
        self.edge_gathered.clearRetainingCapacity();
        try self.edge_gathered.appendNTimes(self.allocator, false, self.facts.items.len);
        for (self.refresh_set.items) |index| {
            const sum = self.sums.items[index];
            for ([_]NodeId{ sum.root, sum.a, sum.b }) |root| {
                if (try self.ensureRootBounds(root)) try self.query_queue.append(self.allocator, root);
            }
        }
        var next: usize = 0;
        while (next < self.query_queue.items.len) : (next += 1) {
            const node = self.query_queue.items[next];
            const own = self.nodes.items[node];
            if (own.lo == own.hi) continue;
            if (node >= self.fwd_heads.items.len) continue;
            for ([_]Direction{ .forward, .backward }) |direction| {
                var index = switch (direction) {
                    .forward => self.fwd_heads.items[node],
                    .backward => self.bwd_heads.items[node],
                };
                while (index != no_fact) {
                    const fact_index = index;
                    const fact = self.facts.items[fact_index];
                    const link = self.fact_links.items[fact_index];
                    const other = switch (direction) {
                        .forward => fact.b,
                        .backward => fact.a,
                    };
                    index = switch (direction) {
                        .forward => link.fwd_prev,
                        .backward => link.bwd_prev,
                    };
                    if (try self.ensureRootBounds(other)) try self.query_queue.append(self.allocator, other);
                    // A fact between two closure roots is met from both
                    // ends and gathered once.
                    if (!self.edge_gathered.items[fact_index]) {
                        self.edge_gathered.items[fact_index] = true;
                        try self.query_edges.append(self.allocator, fact_index);
                    }
                }
            }
        }
        var passes: usize = 0;
        var changed = true;
        while (changed and passes < query_visit_cap) : (passes += 1) {
            changed = false;
            for (self.query_edges.items) |index| {
                const fact = self.facts.items[index];
                const b_bounds = self.root_bounds.get(fact.b).?;
                const a_bounds = self.root_bounds.get(fact.a).?;
                const hi_through = clampSlack(b_bounds.hi + fact.c);
                if (hi_through < a_bounds.hi) {
                    const a = self.root_bounds.getPtr(fact.a).?;
                    a.hi = hi_through;
                    a.hi_assumed = b_bounds.hi_assumed | fact.assumed;
                    changed = true;
                }
                const lo_through = clampSlack(a_bounds.lo - fact.c);
                if (lo_through > b_bounds.lo) {
                    const b = self.root_bounds.getPtr(fact.b).?;
                    b.lo = lo_through;
                    b.lo_assumed = a_bounds.lo_assumed | fact.assumed;
                    changed = true;
                }
            }
            for (self.refresh_set.items) |index| {
                if (try self.relaxSum(self.sums.items[index])) changed = true;
            }
        }
    }

    /// Relax one sum's relations between its root and operands; whether a
    /// bound moved.
    fn relaxSum(self: *Pass, sum: SumRoot) ResourceError!bool {
        const a = self.root_bounds.get(sum.a).?;
        const b = self.root_bounds.get(sum.b).?;
        const root = self.root_bounds.get(sum.root).?;
        var moved = false;
        const root_hi = clampSlack(a.hi + b.hi);
        if (root_hi < root.hi) {
            const r = self.root_bounds.getPtr(sum.root).?;
            r.hi = root_hi;
            r.hi_assumed = a.hi_assumed | b.hi_assumed;
            moved = true;
        }
        const root_lo = clampSlack(a.lo + b.lo);
        if (root_lo > root.lo) {
            const r = self.root_bounds.getPtr(sum.root).?;
            r.lo = root_lo;
            r.lo_assumed = a.lo_assumed | b.lo_assumed;
            moved = true;
        }
        const after = self.root_bounds.get(sum.root).?;
        const a_hi = clampSlack(after.hi - b.lo);
        if (a_hi < a.hi) {
            const p = self.root_bounds.getPtr(sum.a).?;
            p.hi = a_hi;
            p.hi_assumed = after.hi_assumed | b.lo_assumed;
            moved = true;
        }
        const b_hi = clampSlack(after.hi - a.lo);
        if (b_hi < b.hi) {
            const p = self.root_bounds.getPtr(sum.b).?;
            p.hi = b_hi;
            p.hi_assumed = after.hi_assumed | a.lo_assumed;
            moved = true;
        }
        const a_lo = clampSlack(after.lo - b.hi);
        if (a_lo > a.lo) {
            const p = self.root_bounds.getPtr(sum.a).?;
            p.lo = a_lo;
            p.lo_assumed = after.lo_assumed | b.hi_assumed;
            moved = true;
        }
        const b_lo = clampSlack(after.lo - a.hi);
        if (b_lo > b.lo) {
            const p = self.root_bounds.getPtr(sum.b).?;
            p.lo = b_lo;
            p.lo_assumed = after.lo_assumed | a.hi_assumed;
            moved = true;
        }
        return moved;
    }

    /// Enter a root into the closure at its own window; whether it was new.
    fn ensureRootBounds(self: *Pass, root: NodeId) ResourceError!bool {
        const gop = try self.root_bounds.getOrPut(root);
        if (gop.found_existing) return false;
        const node = self.nodes.items[root];
        gop.value_ptr.* = .{ .lo = node.lo, .hi = node.hi, .lo_assumed = 0, .hi_assumed = 0 };
        return true;
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

    /// Begin seeding a region's facts: the same fact arrives from several
    /// seeds (global facts, the raw meet, the stable meet, persisted loop
    /// facts), and a repeat would spend the fact cap and slow the merge
    /// meets, so seeds are deduplicated as they arrive.
    fn beginSeeding(self: *Pass) void {
        self.fact_seen.clearRetainingCapacity();
        self.seeding = true;
    }

    fn endSeeding(self: *Pass) void {
        self.seeding = false;
    }

    fn addFact(self: *Pass, fact: Fact) ResourceError!void {
        if (self.seeding) {
            const gop = try self.fact_seen.getOrPut(.{ .a = fact.a, .b = fact.b, .c = fact.c, .assumed = fact.assumed });
            if (gop.found_existing) return;
        }
        if (self.facts.items.len >= max_facts) return;
        try self.pushFact(fact);
        if (!self.refreshing_sums) self.fact_epoch +%= 1;
    }

    /// Append a fact to the path and link it under both of its roots.
    fn pushFact(self: *Pass, fact: Fact) ResourceError!void {
        const index: u32 = @intCast(self.facts.items.len);
        const top = @max(fact.a, fact.b);
        if (top >= self.fwd_heads.items.len) {
            try self.fwd_heads.appendNTimes(self.allocator, no_fact, top + 1 - self.fwd_heads.items.len);
            try self.bwd_heads.appendNTimes(self.allocator, no_fact, top + 1 - self.bwd_heads.items.len);
        }
        try self.facts.append(self.allocator, fact);
        try self.fact_links.append(self.allocator, .{
            .fwd_prev = self.fwd_heads.items[fact.a],
            .bwd_prev = self.bwd_heads.items[fact.b],
        });
        self.fwd_heads.items[fact.a] = index;
        self.bwd_heads.items[fact.b] = index;
    }

    /// Drop the facts above `len`, unlinking each from its roots.
    fn truncateFacts(self: *Pass, len: usize) void {
        if (self.facts.items.len > len) self.fact_epoch +%= 1;
        while (self.facts.items.len > len) {
            const fact = self.facts.pop().?;
            const link = self.fact_links.pop().?;
            self.fwd_heads.items[fact.a] = link.fwd_prev;
            self.bwd_heads.items[fact.b] = link.bwd_prev;
        }
    }

    /// Whether any path fact mentions a root.
    fn rootHasFacts(self: *const Pass, root: NodeId) bool {
        if (root >= self.fwd_heads.items.len) return false;
        return self.fwd_heads.items[root] != no_fact or self.bwd_heads.items[root] != no_fact;
    }

    const Direction = enum { forward, backward };

    /// Shortest slack from `start` to every root the path's facts reach
    /// from it, into `query_best`: forward along `a <= b + c` from `a` to
    /// `b`, or backward from `b` to `a`. At most `query_visit_cap` roots
    /// are reached, nearest first and through the path's older facts
    /// first, so the roots a region's seeded facts relate are reached
    /// ahead of the ones its own branches add and the bounds persisted
    /// from them come back the same round after round. The worklist is
    /// first in, first out, so a root comes off it at most once per pass
    /// over the roots reached so far, and without a negative cycle among
    /// them its slack settles within as many passes as there are roots: a
    /// root coming off more times than that means the facts hold a
    /// negative cycle (no execution satisfies them together), and the
    /// walk ends there with the slacks it has.
    fn relaxFrom(self: *Pass, start: NodeId, direction: Direction) ResourceError!void {
        self.query_best.clearRetainingCapacity();
        self.query_queue.clearRetainingCapacity();
        try self.query_best.put(start, .{ .c = 0, .queued = true });
        try self.query_queue.append(self.allocator, start);
        self.reached_sum_end = self.isSumEnd(start);
        var next: usize = 0;
        while (next < self.query_queue.items.len) : (next += 1) {
            const node = self.query_queue.items[next];
            const acc = self.query_best.getPtr(node).?;
            acc.queued = false;
            acc.visits += 1;
            if (acc.visits > self.query_best.count()) return;
            const acc_c = acc.c;
            const acc_assumed = acc.assumed;
            if (node >= self.fwd_heads.items.len) continue;
            // The links run newest first; gather them to walk oldest first.
            self.query_edges.clearRetainingCapacity();
            var index = switch (direction) {
                .forward => self.fwd_heads.items[node],
                .backward => self.bwd_heads.items[node],
            };
            while (index != no_fact) {
                try self.query_edges.append(self.allocator, index);
                const link = self.fact_links.items[index];
                index = switch (direction) {
                    .forward => link.fwd_prev,
                    .backward => link.bwd_prev,
                };
            }
            var edge = self.query_edges.items.len;
            while (edge > 0) {
                edge -= 1;
                const fact = self.facts.items[self.query_edges.items[edge]];
                const other = switch (direction) {
                    .forward => fact.b,
                    .backward => fact.a,
                };
                const next_acc = clampSlack(acc_c + fact.c);
                if (self.query_best.getPtr(other)) |known| {
                    if (next_acc >= known.c) continue;
                    known.c = next_acc;
                    known.assumed = acc_assumed | fact.assumed;
                    self.query_used |= fact.assumed;
                    if (known.queued) continue;
                    known.queued = true;
                } else {
                    if (self.query_best.count() >= query_visit_cap) continue;
                    try self.query_best.put(other, .{ .c = next_acc, .assumed = acc_assumed | fact.assumed, .queued = true });
                    self.query_used |= fact.assumed;
                    if (self.isSumEnd(other)) self.reached_sum_end = true;
                }
                try self.query_queue.append(self.allocator, other);
            }
        }
    }

    /// `relaxFrom` with the facts of the sums it touches stated on the
    /// path: the walk is redone after stating the stale sums among the
    /// roots it reached, until it reaches none, since a sum's facts cannot
    /// extend a walk that reaches no root of it. The sums touching the
    /// start are stated before the first walk.
    fn relaxFresh(self: *Pass, start: NodeId, direction: Direction) ResourceError!void {
        if (!self.refreshing_sums) try self.refreshSumsTouching(&.{start});
        const used_before = self.query_used;
        while (true) {
            try self.relaxFrom(start, direction);
            if (self.refreshing_sums or !self.reached_sum_end) return;
            try self.beginRefreshSet();
            var it = self.query_best.iterator();
            while (it.next()) |entry| try self.gatherStaleSums(entry.key_ptr.*);
            if (self.refresh_set.items.len == 0) return;
            try self.refreshSums();
            self.query_used = used_before;
        }
    }

    /// Tightest constant upper bound of a root from the roots a forward
    /// walk reached: the least window ceiling plus slack.
    fn hiConstReached(self: *Pass, start: NodeId) i128 {
        var best: i128 = self.nodes.items[start].hi;
        var it = self.query_best.iterator();
        while (it.next()) |entry| {
            const through = clampSlack(self.nodes.items[entry.key_ptr.*].hi + entry.value_ptr.c);
            if (through < best) best = through;
        }
        return best;
    }

    /// Tightest provable constant lower bound of a root node, following fact
    /// edges backward: from `x <= r + c` and a bound on `x`, `r` is bounded.
    fn loConstOfRoot(self: *Pass, start: NodeId) ResourceError!i128 {
        try self.relaxFresh(start, .backward);
        var best: i128 = self.nodes.items[start].lo;
        var it = self.query_best.iterator();
        while (it.next()) |entry| {
            const through = clampSlack(self.nodes.items[entry.key_ptr.*].lo - entry.value_ptr.c);
            if (through > best) best = through;
        }
        return best;
    }

    /// Proves `value(a) <= value(b) + k`, or returns false when unprovable.
    /// The narrowest offsets make the root-level goal imply the node-level
    /// one for any value in either node's window.
    fn proveLe(self: *Pass, a: NodeId, b: NodeId, k: i128) ResourceError!bool {
        const proven = try self.proveLeQuery(a, b, k);
        self.proof_assumed |= self.query_used;
        return proven;
    }

    fn proveLeQuery(self: *Pass, a: NodeId, b: NodeId, k: i128) ResourceError!bool {
        self.query_used = 0;
        const ra = self.rootOf(a);
        const rb = self.rootOf(b);
        const m = k + self.offLoOf(b) - self.offHiOf(a);
        if (builtin.mode == .Debug) self.last_claim = .{ .ordering = .{ .a = ra, .b = rb, .m = m } };
        if (ra == rb) return m >= 0;

        // Reach rb from ra along fact edges with accumulated slack <= m.
        try self.relaxFresh(ra, .forward);
        if (self.query_best.get(rb)) |acc| {
            if (acc.c <= m) return true;
        }

        // Constant route: every value of ra is at most every value of rb + m.
        const hi_a = self.hiConstReached(ra);
        const lo_b = try self.loConstOfRoot(rb);
        return hi_a <= lo_b + m;
    }

    /// Add an ordering fact the current path does not already hold; whether
    /// it was added.
    fn addFactOnce(self: *Pass, fact: Fact) ResourceError!bool {
        if (fact.a < self.fwd_heads.items.len) {
            var index = self.fwd_heads.items[fact.a];
            while (index != no_fact) {
                const have = self.facts.items[index];
                if (have.b == fact.b and have.c <= fact.c) return false;
                index = self.fact_links.items[index].fwd_prev;
            }
        }
        const before = self.facts.items.len;
        try self.addFact(fact);
        return self.facts.items.len != before;
    }

    /// Least `c` with `value(a) <= value(b) + c` provable through fact
    /// edges, or null when no fact path relates the two roots.
    fn slackLe(self: *Pass, a: NodeId, b: NodeId) ResourceError!?i128 {
        self.query_used = 0;
        const ra = self.rootOf(a);
        const rb = self.rootOf(b);
        const shift = self.offHiOf(a) - self.offLoOf(b);
        if (ra == rb) return shift;
        try self.relaxFresh(ra, .forward);
        const acc = self.query_best.get(rb) orelse return null;
        return acc.c + shift;
    }

    /// Link the sum at `index` under one of its roots.
    fn linkSum(self: *Pass, index: u32, slot: usize, root: NodeId) ResourceError!void {
        if (root >= self.sum_heads.items.len) {
            try self.sum_heads.appendNTimes(self.allocator, no_sum, root + 1 - self.sum_heads.items.len);
        }
        self.sums.items[index].prev[slot] = self.sum_heads.items[root];
        self.sum_heads.items[root] = index;
    }

    fn isSumEnd(self: *const Pass, root: NodeId) bool {
        return root < self.sum_heads.items.len and self.sum_heads.items[root] != no_sum;
    }

    /// Node for the mathematical sum of two dynamic values, or null when they
    /// share a root. The operand roots identify one sum root per round; each
    /// use relates that root to both operands and, through the current
    /// path's facts, to every other sum sharing an operand, since
    /// `a + y <= a + z + c` exactly when `y <= z + c`. The node stands for
    /// the exact sum; the caller decides whether the operation computes it.
    fn sumNode(self: *Pass, lhs: NodeId, rhs: NodeId) ResourceError!?NodeId {
        const ra = self.rootOf(lhs);
        const rb = self.rootOf(rhs);
        if (ra == rb) return null;
        const key = (@as(u64, @min(ra, rb)) << 32) | @as(u64, @max(ra, rb));
        const na = self.nodes.items[ra];
        const nb = self.nodes.items[rb];
        const root = self.sum_roots.get(key) orelse blk: {
            const root = (try self.freshRoot(na.lo + nb.lo, na.hi + nb.hi)) orelse return null;
            try self.sum_roots.put(key, root);
            const index: u32 = @intCast(self.sums.items.len);
            try self.sums.append(self.allocator, .{ .root = root, .a = ra, .b = rb });
            try self.linkSum(index, 0, root);
            try self.linkSum(index, 1, ra);
            try self.linkSum(index, 2, rb);
            break :blk root;
        };
        try self.refreshSumsTouching(&.{ root, ra, rb });
        // The sums sharing an operand are the ones chained under `ra` or
        // `rb` as an operand; one chained under both is this sum itself.
        for ([_]NodeId{ ra, rb }) |shared| {
            var index = self.sum_heads.items[shared];
            while (index != no_sum) {
                const other = self.sums.items[index];
                index = other.prev[sumSlot(other, shared)];
                if (other.root == root or other.root == shared) continue;
                const pair: [2]NodeId = if (other.a == ra)
                    .{ rb, other.b }
                else if (other.b == ra)
                    .{ rb, other.a }
                else if (other.a == rb)
                    .{ ra, other.b }
                else
                    .{ ra, other.a };
                if (try self.slackLe(pair[0], pair[1])) |c| {
                    _ = try self.addFactOnce(.{ .a = root, .b = other.root, .c = c, .origin = .meet, .assumed = self.query_used });
                }
                if (try self.slackLe(pair[1], pair[0])) |c| {
                    _ = try self.addFactOnce(.{ .a = other.root, .b = root, .c = c, .origin = .meet, .assumed = self.query_used });
                }
            }
        }
        const off_lo = self.offLoOf(lhs) + self.offLoOf(rhs);
        const off_hi = self.offHiOf(lhs) + self.offHiOf(rhs);
        if (off_lo == 0 and off_hi == 0) return root;
        return try self.addNode(.{ .root = root, .off_lo = off_lo, .off_hi = off_hi, .lo = 0, .hi = 0 });
    }

    // Environments

    fn isSingleAssign(self: *const Pass, local: LocalId) bool {
        return (self.assign_counts.get(local) orelse 0) <= 1;
    }

    /// The local a chain of single-assignment aliases denotes: lowering
    /// binds one alias per use, so the same value is read through a
    /// different local each time, and a stable term keyed by the alias
    /// would never meet itself again.
    fn stableLocalOf(self: *const Pass, local: LocalId) LocalId {
        var current = local;
        while (self.isSingleAssign(current)) {
            const source = self.alias_of.get(current) orelse break;
            if (!self.isSingleAssign(source)) break;
            current = source;
        }
        return current;
    }

    fn lookup(self: *const Pass, local: LocalId) ?Binding {
        if (self.isSingleAssign(local)) return self.global_env.get(local);
        return self.path_env.get(local);
    }

    fn bind(self: *Pass, local: LocalId, binding: Binding) ResourceError!void {
        if (self.isSingleAssign(local)) {
            // A single-assignment integer local bound to a root names that
            // root's value in round-stable form, wherever the root came
            // from: a sum of two unrelated values or an unproven checked
            // operation gets a fresh root just as an unbound read does, and
            // facts about it must persist the same way.
            if (trackedIntMax(self.localLayout(local)) != null and self.rootOf(binding.node) == binding.node) {
                const entry = try self.value_roots.getOrPut(binding.node);
                if (!entry.found_existing) entry.value_ptr.* = self.stableLocalOf(local);
            }
            try self.global_env.put(local, binding);
            return;
        }
        if (trackedIntMax(self.localLayout(local)) != null and self.rootOf(binding.node) == binding.node) {
            try self.path_value_roots.put(binding.node, local);
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
            try self.value_roots.put(node, self.stableLocalOf(local));
        }
        try self.bind(local, .{ .node = node });
        return node;
    }

    fn bindFresh(self: *Pass, local: LocalId) ResourceError!void {
        const node = (try self.unknownFor(self.localLayout(local))) orelse return;
        // As in valueOf: a single-assignment integer local's fresh root
        // denotes its value in round-stable form.
        if (trackedIntMax(self.localLayout(local)) != null and self.isSingleAssign(local)) {
            try self.value_roots.put(node, self.stableLocalOf(local));
        }
        try self.bind(local, .{ .node = node });
    }

    /// Bind a constructed struct to a fresh value whose fields are the
    /// values it was built from, so a later field read finds the field's
    /// node rather than a fresh unknown. Loop exits that carry several values
    /// out through one record depend on this to keep their bounds.
    fn modelStruct(self: *Pass, target: LocalId, fields: LIR.LocalSpan) ResourceError!void {
        const node = (try self.unknownFor(self.localLayout(target))) orelse return self.bindFresh(target);
        const field_locals = self.store.getLocalSpan(fields);
        for (0..GuardedList.borrowLen(field_locals)) |i| {
            const field_local = GuardedList.at(field_locals, i);
            if (trackedIntMax(self.localLayout(field_local)) == null) continue;
            const field_node = (try self.valueOf(field_local)) orelse continue;
            const key = (@as(u64, node) << 32) | @as(u64, @intCast(i));
            try self.field_values.put(key, field_node);
            try self.int_fields.put(key, {});
            try self.struct_roots.put(node, {});
        }
        try self.bind(target, .{ .node = node });
    }

    /// Bind a field-read target, unifying with earlier reads of the same
    /// field of the same struct value so facts reach every read site.
    fn bindFieldRead(self: *Pass, target: LocalId, source: LocalId, field_idx: u32) ResourceError!void {
        const src_node = (try self.valueOf(source)) orelse return self.bindFresh(target);
        const key = (@as(u64, self.rootOf(src_node)) << 32) | field_idx;
        if (self.field_values.get(key)) |node| {
            try self.bind(target, .{ .node = node });
            return;
        }
        const node = (try self.unknownFor(self.localLayout(target))) orelse return self.bindFresh(target);
        try self.field_values.put(key, node);
        if (trackedIntMax(self.localLayout(target)) != null) {
            try self.int_fields.put(key, {});
            if (self.isSingleAssign(target)) try self.value_roots.put(node, self.stableLocalOf(target));
        }
        try self.bind(target, .{ .node = node });
    }

    fn rewindTo(self: *Pass, facts_len: usize, no_overflow_facts_len: usize, undo_len: usize) ResourceError!void {
        self.truncateFacts(facts_len);
        self.no_overflow_facts.shrinkRetainingCapacity(no_overflow_facts_len);
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

    /// Free the storage of every persisted list, before the maps that hold
    /// them are cleared or dropped.
    fn freePersisted(self: *Pass) void {
        var bounds_it = self.loop_bounds.valueIterator();
        while (bounds_it.next()) |bounds| bounds.deinit(self.allocator);
        var loop_facts_it = self.loop_facts.valueIterator();
        while (loop_facts_it.next()) |facts| facts.deinit(self.allocator);
        var merge_facts_it = self.merge_facts.valueIterator();
        while (merge_facts_it.next()) |facts| facts.deinit(self.allocator);
        var merge_env_it = self.merge_env.valueIterator();
        while (merge_env_it.next()) |env| env.deinit(self.allocator);
    }

    /// Reserve the per-merge-head maps for every head the procedure has,
    /// so they never grow during the rounds: the pass runs on an arena,
    /// where a map that grows by doubling leaves every earlier copy
    /// allocated.
    fn reserveMergeStorage(self: *Pass) ResourceError!void {
        var heads: usize = 0;
        var it = self.pred_counts.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* > 1) heads += 1;
        }
        const joins = self.join_stmts.count();
        try self.merge_states.ensureTotalCapacity(heads + joins);
        try self.merge_facts.ensureTotalCapacity(heads + joins);
        try self.merge_env.ensureTotalCapacity(heads + joins);
        try self.loop_facts.ensureTotalCapacity(joins);
    }

    fn prescanProc(self: *Pass, proc: LIR.LirProcSpec) ResourceError!void {
        if (self.read_counts) |*counts| counts.deinit();
        self.read_counts = null;
        self.read_counts = if (proc.body) |body| try BodyClone.countReachableReadsWithScratch(self.store, body, self.analysis) else null;
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
                    if (s.op == .local) try self.alias_of.put(s.target, s.op.local);
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
                .assign_boxy_record_update => |s| {
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
                .assign_boxy_hash => |s| {
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
        try self.scanJoinNesting(proc.body.?);
        if (self.body_joins.count() != 0) try self.buildLoopScan(proc.body.?);
        var loop_it = self.body_joins.iterator();
        while (loop_it.next()) |kv| try self.scanLoopAssigned(kv.value_ptr.*, kv.key_ptr.*);
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

    /// End the round's merge states, keeping each state's lists for the
    /// next round's captures of the same head.
    fn retireMergeStates(self: *Pass) void {
        var it = self.merge_states.valueIterator();
        while (it.next()) |state| {
            state.live = false;
            state.facts.clearRetainingCapacity();
            state.env.clearRetainingCapacity();
        }
    }

    fn freeMergeStates(self: *Pass) void {
        var it = self.merge_states.valueIterator();
        while (it.next()) |state| {
            state.facts.deinit(self.allocator);
            state.env.deinit(self.allocator);
            state.stable.deinit(self.allocator);
            state.entry_stable.deinit(self.allocator);
        }
        self.merge_states.clearRetainingCapacity();
    }

    /// This round's state for a merge head, if any edge into it was
    /// captured this round.
    fn liveMergeState(self: *Pass, head: CFStmtId) ?*MergeState {
        const state = self.merge_states.getPtr(head) orelse return null;
        return if (state.live) state else null;
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
        // A constructed struct carries its fields' bounds.
        if (self.struct_roots.contains(node.root)) return true;
        // A loop parameter's value keeps its identity through merges so a
        // back edge carrying it unchanged is recognized.
        if (self.seeded_param_root_set.contains(node.root)) return true;
        return self.rootHasFacts(node.root);
    }

    /// Whether the edge being captured into `head` comes from outside the
    /// loop `head` begins. A jump from any region inside the loop body is a
    /// back edge, so the regions of joins nested in that body count as
    /// inside; a merge that begins no loop treats every other region as
    /// outside.
    fn edgeEntersLoop(self: *const Pass, head: CFStmtId) bool {
        const region = self.current_region orelse return true;
        if (region == head) return false;
        const loop_join = self.body_joins.get(head) orelse return true;
        var enclosing = self.enclosing_join.get(region);
        while (enclosing) |join_id| {
            if (join_id == loop_join) return false;
            enclosing = self.join_parent.get(join_id);
        }
        return true;
    }

    /// Record which join body lexically contains each reachable statement.
    /// Join bodies are descended directly and jump targets are not followed,
    /// so the result is the declaration nesting rather than the control-flow
    /// reachability the walk itself uses.
    fn scanJoinNesting(self: *Pass, body: CFStmtId) ResourceError!void {
        const Item = struct { stmt: CFStmtId, join: ?JoinPointId };
        var stack = std.ArrayList(Item).empty;
        defer stack.deinit(self.allocator);
        var seen = collections.DenseMap(CFStmtId, void).init(self.allocator);
        defer seen.deinit();
        var successors = std.ArrayList(CFStmtId).empty;
        defer successors.deinit(self.allocator);

        try stack.append(self.allocator, .{ .stmt = body, .join = null });
        while (stack.pop()) |item| {
            if (seen.contains(item.stmt)) continue;
            try seen.put(item.stmt, {});
            if (item.join) |join_id| try self.enclosing_join.put(item.stmt, join_id);
            switch (self.store.getCFStmt(item.stmt)) {
                .join => |s| {
                    if (item.join) |outer| try self.join_parent.put(s.id, outer);
                    try stack.append(self.allocator, .{ .stmt = s.body, .join = s.id });
                    try stack.append(self.allocator, .{ .stmt = s.remainder, .join = item.join });
                },
                .jump => {},
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
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
                .loop_continue,
                .loop_break,
                .ret,
                .crash,
                => {
                    successors.clearRetainingCapacity();
                    try BodyClone.appendSuccessorsWithAllocator(self.store, &successors, item.stmt, self.allocator);
                    for (successors.items) |next| try stack.append(self.allocator, .{ .stmt = next, .join = item.join });
                },
            }
        }
    }

    /// Every reachable statement of a procedure, numbered, with its
    /// predecessors on the successor relation the loop scans follow, the
    /// reachable jumps to each join, and the join-nesting tree numbered so
    /// that whether a join's body encloses another join is an interval test.
    /// Built once per procedure; each loop's scan reuses it.
    const LoopScan = struct {
        stmts: std.ArrayList(CFStmtId) = .empty,
        index_of: collections.DenseMap(CFStmtId, u32),
        successors: std.ArrayList(CFStmtId) = .empty,
        edges: std.ArrayList(LoopEdge) = .empty,
        pred_start: std.ArrayList(u32) = .empty,
        preds: std.ArrayList(u32) = .empty,
        jumps_to: collections.DenseMap(JoinPointId, std.ArrayList(u32)),
        join_order: collections.DenseMap(JoinPointId, JoinOrder),
        /// `mark[i] == generation` when node `i` is on the current loop's cycle.
        mark: std.ArrayList(u32) = .empty,
        generation: u32 = 0,
        pending: std.ArrayList(u32) = .empty,
        cycle: std.ArrayList(u32) = .empty,

        const LoopEdge = struct { from: u32, to: u32 };
        const JoinOrder = struct { pre: u32, post: u32 };

        fn init(allocator: Allocator) LoopScan {
            return .{
                .index_of = collections.DenseMap(CFStmtId, u32).init(allocator),
                .jumps_to = collections.DenseMap(JoinPointId, std.ArrayList(u32)).init(allocator),
                .join_order = collections.DenseMap(JoinPointId, JoinOrder).init(allocator),
            };
        }

        /// Empty the scan for another build, keeping its memory.
        fn clear(self: *LoopScan) void {
            self.stmts.clearRetainingCapacity();
            self.index_of.clearRetainingCapacity();
            self.successors.clearRetainingCapacity();
            self.edges.clearRetainingCapacity();
            self.pred_start.clearRetainingCapacity();
            self.preds.clearRetainingCapacity();
            var jumps = self.jumps_to.iterator();
            while (jumps.next()) |entry| entry.value_ptr.clearRetainingCapacity();
            self.join_order.clearRetainingCapacity();
            self.mark.clearRetainingCapacity();
            self.pending.clearRetainingCapacity();
            self.cycle.clearRetainingCapacity();
        }

        fn deinit(self: *LoopScan, allocator: Allocator) void {
            self.stmts.deinit(allocator);
            self.index_of.deinit();
            self.successors.deinit(allocator);
            self.edges.deinit(allocator);
            self.pred_start.deinit(allocator);
            self.preds.deinit(allocator);
            var jumps = self.jumps_to.iterator();
            while (jumps.next()) |entry| entry.value_ptr.deinit(allocator);
            self.jumps_to.deinit();
            self.join_order.deinit();
            self.mark.deinit(allocator);
            self.pending.deinit(allocator);
            self.cycle.deinit(allocator);
        }

        fn node(self: *LoopScan, allocator: Allocator, stmt: CFStmtId) ResourceError!u32 {
            const entry = try self.index_of.getOrPut(stmt);
            if (!entry.found_existing) {
                entry.value_ptr.* = @intCast(self.stmts.items.len);
                try self.stmts.append(allocator, stmt);
            }
            return entry.value_ptr.*;
        }

        /// Whether `outer`'s body encloses `inner`'s.
        fn encloses(self: *const LoopScan, outer: JoinPointId, inner: JoinPointId) bool {
            const o = self.join_order.get(outer) orelse return false;
            const i = self.join_order.get(inner) orelse return false;
            return o.pre <= i.pre and i.post <= o.post;
        }
    };

    /// Build `loop_scan` for a procedure whose join nesting is recorded.
    fn buildLoopScan(self: *Pass, body: CFStmtId) ResourceError!void {
        const scan = &self.loop_scan;
        const gpa = self.allocator;
        const successors = &scan.successors;
        scan.clear();
        _ = try scan.node(gpa, body);
        var cursor: u32 = 0;
        while (cursor < scan.stmts.items.len) : (cursor += 1) {
            const stmt = scan.stmts.items[cursor];
            successors.clearRetainingCapacity();
            switch (self.store.getCFStmt(stmt)) {
                .jump => |j| {
                    const target_stmt = self.join_stmts.get(j.target) orelse continue;
                    const entry = try scan.jumps_to.getOrPut(j.target);
                    if (!entry.found_existing) entry.value_ptr.* = .empty;
                    try entry.value_ptr.append(gpa, cursor);
                    try successors.append(gpa, self.store.getCFStmt(target_stmt).join.body);
                },
                .join => |j| try successors.append(gpa, j.remainder),
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
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
                .loop_continue,
                .loop_break,
                .ret,
                .crash,
                => try BodyClone.appendSuccessorsWithAllocator(self.store, successors, stmt, gpa),
            }
            for (successors.items) |next| {
                try scan.edges.append(gpa, .{ .from = cursor, .to = try scan.node(gpa, next) });
            }
        }

        const count = scan.stmts.items.len;
        try scan.pred_start.resize(gpa, count + 1);
        const pred_start = scan.pred_start.items;
        @memset(pred_start, 0);
        for (scan.edges.items) |edge| pred_start[edge.to + 1] += 1;
        for (1..pred_start.len) |i| pred_start[i] += pred_start[i - 1];
        try scan.preds.resize(gpa, scan.edges.items.len);
        try scan.pending.resize(gpa, count);
        const fill = scan.pending.items;
        @memcpy(fill, pred_start[0..count]);
        for (scan.edges.items) |edge| {
            scan.preds.items[fill[edge.to]] = edge.from;
            fill[edge.to] += 1;
        }
        scan.pending.clearRetainingCapacity();
        try scan.mark.resize(gpa, count);
        @memset(scan.mark.items, 0);

        // Number the join-nesting tree in depth-first order: a join's body
        // encloses exactly the joins numbered within its interval.
        var children = collections.DenseMap(JoinPointId, std.ArrayList(JoinPointId)).init(gpa);
        defer {
            var it = children.iterator();
            while (it.next()) |entry| entry.value_ptr.deinit(gpa);
            children.deinit();
        }
        var roots = std.ArrayList(JoinPointId).empty;
        defer roots.deinit(gpa);
        var joins = self.join_stmts.iterator();
        while (joins.next()) |entry| {
            const id = entry.key_ptr.*;
            if (self.join_parent.get(id)) |parent| {
                const kids = try children.getOrPut(parent);
                if (!kids.found_existing) kids.value_ptr.* = .empty;
                try kids.value_ptr.append(gpa, id);
            } else {
                try roots.append(gpa, id);
            }
        }
        const NestingFrame = struct { id: JoinPointId, next: u32 };
        var stack = std.ArrayList(NestingFrame).empty;
        defer stack.deinit(gpa);
        var clock: u32 = 0;
        for (roots.items) |root| {
            try scan.join_order.put(root, .{ .pre = clock, .post = clock });
            clock += 1;
            try stack.append(gpa, .{ .id = root, .next = 0 });
            while (stack.items.len != 0) {
                const top = &stack.items[stack.items.len - 1];
                const kids: []const JoinPointId = if (children.getPtr(top.id)) |list| list.items else &.{};
                if (top.next < kids.len) {
                    const child = kids[top.next];
                    top.next += 1;
                    try scan.join_order.put(child, .{ .pre = clock, .post = clock });
                    clock += 1;
                    try stack.append(gpa, .{ .id = child, .next = 0 });
                } else {
                    scan.join_order.getPtr(top.id).?.post = clock;
                    _ = stack.pop();
                }
            }
        }
    }

    /// Record the locals assigned on the cycle of loop `join_id`: statements
    /// reachable from its body from which a jump back to it is reachable.
    /// A local written only after the loop exits, even lexically inside the
    /// join's body, keeps its entry value for every iteration.
    ///
    /// The cycle is found backward from the jumps to the join that its body
    /// encloses. The body is entered only by jumps to the join, so the walk
    /// stops at the body's first statement and never passes the join
    /// statement itself; every statement it reaches lies within the body, and
    /// a statement within the body is reachable from the procedure exactly
    /// when it is reachable from the body.
    fn scanLoopAssigned(self: *Pass, join_id: JoinPointId, body: CFStmtId) ResourceError!void {
        const scan = &self.loop_scan;
        scan.generation += 1;
        const generation = scan.generation;
        scan.pending.clearRetainingCapacity();
        scan.cycle.clearRetainingCapacity();
        const header = self.join_stmts.get(join_id);
        const body_node = scan.index_of.get(body);
        if (scan.jumps_to.getPtr(join_id)) |jumps| {
            for (jumps.items) |jump| {
                const owner = self.enclosing_join.get(scan.stmts.items[jump]) orelse continue;
                if (owner != join_id and !scan.encloses(join_id, owner)) continue;
                if (scan.mark.items[jump] == generation) continue;
                scan.mark.items[jump] = generation;
                try scan.pending.append(self.allocator, jump);
            }
        }
        while (scan.pending.pop()) |current| {
            try scan.cycle.append(self.allocator, current);
            if (body_node != null and current == body_node.?) continue;
            for (scan.preds.items[scan.pred_start.items[current]..scan.pred_start.items[current + 1]]) |pred| {
                if (scan.mark.items[pred] == generation) continue;
                if (header != null and scan.stmts.items[pred] == header.?) continue;
                scan.mark.items[pred] = generation;
                try scan.pending.append(self.allocator, pred);
            }
        }
        var back: u32 = 0;
        for (scan.cycle.items) |node| {
            const stmt = scan.stmts.items[node];
            switch (self.store.getCFStmt(stmt)) {
                .jump => |j| {
                    if (j.target == join_id) back += 1;
                },
                .join,
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
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
                .loop_continue,
                .loop_break,
                .ret,
                .crash,
                => {},
            }
            if (assignedLocalOf(self.store.getCFStmt(stmt))) |local| {
                try self.join_assigned.put(joinAssignedKey(join_id, local), {});
            }
        }
        try self.back_jumps.put(join_id, back);
    }

    /// Jumps into a loop join from outside its cycle.
    fn entryJumpCount(self: *const Pass, join_id: JoinPointId) u32 {
        const back = self.back_jumps.get(join_id) orelse 0;
        const total = self.jumpCount(join_id);
        return if (total > back) total - back else 0;
    }

    /// Whether every entry edge of a loop join was captured this round, so
    /// the facts they all carry may seed its body now.
    fn entryEdgesComplete(self: *const Pass, join_id: JoinPointId, state: *const MergeState) bool {
        const entries = self.entryJumpCount(join_id);
        return entries > 0 and state.entry_captures >= entries;
    }

    fn joinAssignedKey(join_id: JoinPointId, local: LocalId) u64 {
        return (@as(u64, @intFromEnum(join_id)) << 32) | @as(u64, @intFromEnum(local));
    }

    /// The local a statement writes, if any.
    fn assignedLocalOf(stmt: LIR.CFStmt) ?LocalId {
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
            .assign_boxy_record_update,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_hash,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .assign_call_dict,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .set_local,
            => |s| s.target,
            .join,
            .jump,
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
            .str_match,
            .str_match_set,
            .boxy_tag_match,
            .loop_continue,
            .loop_break,
            .ret,
            .crash,
            => null,
        };
    }

    fn stabilizeLoopTerm(self: *const Pass, root: NodeId, join_id: JoinPointId) ?StableTerm {
        return self.stabilizeTermFor(root, join_id);
    }

    /// Stabilize the facts an entry edge carries into loop `join_id`,
    /// keeping the facts the loop persisted last round ahead of new ones
    /// when the cap bites.
    fn stabilizeLoopFacts(self: *Pass, stable: *LoopFacts, facts: []const Fact, join_id: JoinPointId) ResourceError!void {
        try self.stabilizeInto(stable, facts, .{ .loop = join_id }, self.loop_facts.getPtrConst(join_id));
    }

    const StabilizeScope = union(enum) {
        /// Terms stable anywhere on the path.
        path,
        /// Terms stable throughout this loop.
        loop: JoinPointId,
    };

    /// Stabilize a fact list into round-stable form, keeping facts whose
    /// endpoints all denote something stable. When more qualify than the
    /// cap holds, the facts `previous` persisted for the same head come
    /// first: a persisted set then changes only when a member stops
    /// holding or room opens, rather than with the order the walk happens
    /// to put the facts in, which a seed's position can shift from round
    /// to round without any fact changing.
    fn stabilizeInto(self: *Pass, stable: *LoopFacts, facts: []const Fact, scope: StabilizeScope, previous: ?*const LoopFacts) ResourceError!void {
        stable.len = 0;
        self.stable_index.clearRetainingCapacity();
        self.stable_candidates.clearRetainingCapacity();
        for (facts) |fact| {
            const a = (switch (scope) {
                .path => self.stabilizeTerm(fact.a),
                .loop => |join_id| self.stabilizeLoopTerm(fact.a, join_id),
            }) orelse continue;
            const b = (switch (scope) {
                .path => self.stabilizeTerm(fact.b),
                .loop => |join_id| self.stabilizeLoopTerm(fact.b, join_id),
            }) orelse continue;
            if (a == .constant and b == .constant) continue;
            const candidate = StableFact{ .a = a, .b = b, .c = fact.c, .assumed = fact.assumed };
            const gop = try self.stable_index.getOrPut(stableFactKey(candidate));
            if (gop.found_existing) {
                self.stable_candidates.items[gop.value_ptr.*].assumed |= candidate.assumed;
                continue;
            }
            gop.value_ptr.* = @intCast(self.stable_candidates.items.len);
            try self.stable_candidates.append(self.allocator, candidate);
        }
        // Under the cap every candidate persists, implied or not; over it,
        // an implied candidate must not take a slot from one that implies
        // it.
        if (self.stable_candidates.items.len > loop_fact_cap) try self.dropImpliedCandidates();
        const candidates = self.stable_candidates.items;
        if (candidates.len <= loop_fact_cap or previous == null) {
            const take = @min(candidates.len, loop_fact_cap);
            @memcpy(stable.items[0..take], candidates[0..take]);
            stable.len = take;
            return;
        }
        // Over the cap: the previously persisted members first, in
        // candidate order, then the rest until the cap.
        self.previous_keys.clearRetainingCapacity();
        for (previous.?.items[0..previous.?.len]) |fact| try self.previous_keys.put(stableFactKey(fact), {});
        for (candidates) |candidate| {
            if (stable.len >= loop_fact_cap) break;
            if (!self.previous_keys.contains(stableFactKey(candidate))) continue;
            stable.items[stable.len] = candidate;
            stable.len += 1;
        }
        for (candidates) |candidate| {
            if (stable.len >= loop_fact_cap) break;
            if (self.previous_keys.contains(stableFactKey(candidate))) continue;
            stable.items[stable.len] = candidate;
            stable.len += 1;
        }
    }

    /// Drop every candidate another candidate implies: the same endpoints,
    /// a constant no larger, and no assumption the implied one lacks. The
    /// survivors keep their order.
    fn dropImpliedCandidates(self: *Pass) ResourceError!void {
        const candidates = self.stable_candidates.items;
        self.candidate_order.clearRetainingCapacity();
        try self.candidate_order.ensureTotalCapacity(self.allocator, candidates.len);
        for (0..candidates.len) |i| self.candidate_order.appendAssumeCapacity(@intCast(i));
        std.mem.sortUnstable(u32, self.candidate_order.items, candidates, candidateBefore);
        self.candidate_implied.clearRetainingCapacity();
        try self.candidate_implied.appendNTimes(self.allocator, false, candidates.len);
        var any_implied = false;
        var group_start: usize = 0;
        const order = self.candidate_order.items;
        while (group_start < order.len) {
            var group_end = group_start + 1;
            while (group_end < order.len and sameEndpoints(candidates[order[group_start]], candidates[order[group_end]])) group_end += 1;
            // Within a group the constants ascend, so only earlier members
            // can imply a later one.
            for (group_start + 1..group_end) |later| {
                const implied = candidates[order[later]];
                for (group_start..later) |earlier| {
                    if (self.candidate_implied.items[order[earlier]]) continue;
                    const by = candidates[order[earlier]];
                    if (by.assumed & ~implied.assumed == 0) {
                        self.candidate_implied.items[order[later]] = true;
                        any_implied = true;
                        break;
                    }
                }
            }
            group_start = group_end;
        }
        if (!any_implied) return;
        var kept: usize = 0;
        for (candidates, 0..) |candidate, i| {
            if (self.candidate_implied.items[i]) continue;
            candidates[kept] = candidate;
            kept += 1;
        }
        self.stable_candidates.items.len = kept;
    }

    fn sameEndpoints(a: StableFact, b: StableFact) bool {
        return std.meta.eql(a.a, b.a) and std.meta.eql(a.b, b.b);
    }

    /// Candidates ordered by endpoints, then by constant ascending.
    fn candidateBefore(candidates: []const StableFact, x: u32, y: u32) bool {
        const a = candidates[x];
        const b = candidates[y];
        switch (stableTermOrder(a.a, b.a)) {
            .lt => return true,
            .gt => return false,
            .eq => {},
        }
        switch (stableTermOrder(a.b, b.b)) {
            .lt => return true,
            .gt => return false,
            .eq => {},
        }
        return a.c < b.c;
    }

    fn stableTermOrder(a: StableTerm, b: StableTerm) std.math.Order {
        const tag_a = @intFromEnum(std.meta.activeTag(a));
        const tag_b = @intFromEnum(std.meta.activeTag(b));
        if (tag_a != tag_b) return std.math.order(tag_a, tag_b);
        return switch (a) {
            .value_of, .len_of => |local| std.math.order(@intFromEnum(local), @intFromEnum(switch (b) {
                .value_of, .len_of => |other| other,
                .constant => unreachable,
            })),
            .constant => |value| std.math.order(value, b.constant),
        };
    }

    /// Whether two persisted fact lists hold the same facts, with the same
    /// assumptions and growth, in any order. A list's order follows the
    /// walk that captured it, and a merge seeded from another merge's
    /// persisted facts inherits that order a round late, so along a chain
    /// of joins the order keeps shifting while the facts stand still.
    fn sameStableFacts(self: *Pass, previous: *const LoopFacts, current: *const LoopFacts) ResourceError!bool {
        if (previous.len != current.len) return false;
        self.stable_index.clearRetainingCapacity();
        for (previous.items[0..previous.len], 0..) |fact, i| {
            const gop = try self.stable_index.getOrPut(stableFactKey(fact));
            if (!gop.found_existing) gop.value_ptr.* = @intCast(i);
        }
        for (current.items[0..current.len]) |fact| {
            const at = self.stable_index.get(stableFactKey(fact)) orelse return false;
            if (!std.meta.eql(previous.items[at], fact)) return false;
        }
        return true;
    }

    /// Keep in `kept` only the facts `other` also holds, each with both
    /// sides' assumptions.
    fn intersectStableFacts(self: *Pass, kept: *LoopFacts, other: *const LoopFacts) ResourceError!void {
        self.stable_index.clearRetainingCapacity();
        for (other.items[0..other.len], 0..) |fact, i| {
            const gop = try self.stable_index.getOrPut(stableFactKey(fact));
            if (!gop.found_existing) gop.value_ptr.* = @intCast(i);
        }
        var keep: usize = 0;
        for (kept.items[0..kept.len]) |fact| {
            const match = self.stable_index.get(stableFactKey(fact)) orelse continue;
            kept.items[keep] = fact;
            kept.items[keep].assumed |= other.items[match].assumed;
            keep += 1;
        }
        kept.len = keep;
    }

    /// Capture the current path state into a merge head's meet: facts keep
    /// only what every captured edge established, and each path-bound local
    /// keeps a common root with a widened offset window.
    fn captureMergeEdge(self: *Pass, head: CFStmtId) ResourceError!void {
        const entry = try self.merge_states.getOrPut(head);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{ .live = true, .captures = 0, .facts = .empty, .stable = .{}, .env = .empty, .entry_captures = 0, .entry_stable = .{} };
        } else if (!entry.value_ptr.live) {
            entry.value_ptr.live = true;
            entry.value_ptr.captures = 0;
            entry.value_ptr.entry_captures = 0;
            entry.value_ptr.stable.len = 0;
            entry.value_ptr.entry_stable.len = 0;
        }
        const state = entry.value_ptr;

        // An edge arriving from outside the loop is an entry edge; its facts
        // meet separately so loop-invariant relations survive the back
        // edge's inability to derive them before its region is seeded.
        const mine_stable = self.stable_scratch;
        try self.stabilizeFacts(mine_stable, self.facts.items, head);
        if (state.captures == 0) {
            try state.stable.assign(self.allocator, mine_stable);
        } else {
            try self.intersectStableFacts(&state.stable, mine_stable);
        }
        if (self.edgeEntersLoop(head)) {
            const entry_mine = if (self.body_joins.get(head)) |join_id| blk: {
                try self.stabilizeLoopFacts(self.entry_scratch, self.facts.items, join_id);
                break :blk self.entry_scratch;
            } else mine_stable;
            if (state.entry_captures == 0) {
                try state.entry_stable.assign(self.allocator, entry_mine);
            } else {
                try self.intersectStableFacts(&state.entry_stable, entry_mine);
            }
            state.entry_captures += 1;
        }

        if (state.captures == 0) {
            try state.facts.ensureTotalCapacityPrecise(self.allocator, self.facts.items.len);
            state.facts.appendSliceAssumeCapacity(self.facts.items);
            var it = self.path_env.iterator();
            while (it.next()) |kv| {
                if (state.env.items.len >= merge_env_cap) break;
                if (!self.captureWorthy(kv.value_ptr.node)) continue;
                const node = self.nodes.items[kv.value_ptr.node];
                const len_bounds = try self.localLenBounds(kv.value_ptr.node);
                const local_lower = try self.valueLowerBounds(kv.key_ptr.*, kv.value_ptr.node);
                try state.env.append(self.allocator, .{
                    .local = kv.key_ptr.*,
                    .root = node.root,
                    .off_lo = node.off_lo,
                    .off_hi = node.off_hi,
                    .valid = true,
                    .bounds = try self.reachableBounds(kv.value_ptr.node),
                    .lower = local_lower,
                    .lower_any = local_lower,
                    .len_bounds = len_bounds,
                    .len_bounds_any = len_bounds,
                });
                // A struct's integer fields meet like locals of their own, so
                // a loop exit that carries its state out through one record
                // keeps each carried value's bounds.
                var field_idx: u32 = 0;
                while (field_idx < max_struct_meet_fields) : (field_idx += 1) {
                    const field_key = (@as(u64, node.root) << 32) | field_idx;
                    if (!self.int_fields.contains(field_key)) continue;
                    const field_node = self.field_values.get(field_key) orelse continue;
                    // A field with nothing known about it would only take
                    // an environment slot from a value with bounds.
                    if (!self.captureWorthy(field_node)) continue;
                    if (state.env.items.len >= merge_env_cap) break;
                    const fnode = self.nodes.items[field_node];
                    const lower = try self.valueLowerBoundsNode(field_node);
                    try state.env.append(self.allocator, .{
                        .local = kv.key_ptr.*,
                        .field = field_idx,
                        .root = fnode.root,
                        .off_lo = fnode.off_lo,
                        .off_hi = fnode.off_hi,
                        .valid = true,
                        .bounds = try self.reachableBounds(field_node),
                        .lower = lower,
                        .lower_any = lower,
                        .len_bounds = .{},
                        .len_bounds_any = .{},
                    });
                }
            }
        } else {
            // Intersect facts: keep only entries this edge also carries.
            var keep: usize = 0;
            for (state.facts.items) |fact| {
                var present = false;
                var same_origin = false;
                // The earliest matching path fact decides the origin.
                var index = if (fact.a < self.fwd_heads.items.len) self.fwd_heads.items[fact.a] else no_fact;
                while (index != no_fact) {
                    const mine = self.facts.items[index];
                    index = self.fact_links.items[index].fwd_prev;
                    if (mine.b == fact.b and mine.c == fact.c) {
                        present = true;
                        same_origin = std.meta.eql(mine.origin, fact.origin);
                    }
                }
                if (present) {
                    state.facts.items[keep] = fact;
                    if (!same_origin) state.facts.items[keep].origin = .meet;
                    keep += 1;
                }
            }
            state.facts.shrinkRetainingCapacity(keep);

            const seed_join = self.body_joins.get(head);
            for (state.env.items) |*meet| {
                if (!meet.valid and meet.bounds.len == 0 and meet.lower.len == 0 and meet.lower_any.len == 0 and meet.len_bounds.len == 0 and meet.len_bounds_any.len == 0) continue;
                const binding = self.path_env.get(meet.local);
                const node_id: ?NodeId = if (binding) |b|
                    (if (meet.field) |field_idx| self.field_values.get((@as(u64, self.rootOf(b.node)) << 32) | field_idx) else b.node)
                else
                    null;
                if (node_id) |nid| {
                    // A loop parameter arriving at its own join as the very
                    // value that join's body was seeded with is unchanged
                    // around the loop: the entry edges' meet already
                    // describes it. Any other merge meets every edge, since
                    // its other edges may carry narrower values.
                    if (meet.field == null) {
                        if (seed_join) |join_id| {
                            if (self.seeded_param_roots.get(loopBoundKey(join_id, meet.local))) |seed_node| {
                                const seeded = self.nodes.items[seed_node];
                                const arriving = self.nodes.items[nid];
                                if (seeded.root == arriving.root and seeded.off_lo == arriving.off_lo and seeded.off_hi == arriving.off_hi) continue;
                            }
                        }
                    }
                    const node = self.nodes.items[nid];
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
                    const mine = try self.reachableBounds(nid);
                    var kept: MeetBounds = .{};
                    for (meet.bounds.slice()) |bound| {
                        for (mine.slice()) |candidate| {
                            if (candidate.root == bound.root) {
                                kept.append(.{ .root = bound.root, .c = @max(bound.c, candidate.c), .assumed = bound.assumed | candidate.assumed });
                                break;
                            }
                        }
                    }
                    meet.bounds = kept;
                    const mine_lower = if (meet.field != null) try self.valueLowerBoundsNode(nid) else try self.valueLowerBounds(meet.local, nid);
                    var kept_lower: MeetBounds = .{};
                    for (meet.lower.slice()) |bound| {
                        for (mine_lower.slice()) |candidate| {
                            if (candidate.root == bound.root) {
                                kept_lower.append(.{ .root = bound.root, .c = @max(bound.c, candidate.c), .assumed = bound.assumed | candidate.assumed });
                                break;
                            }
                        }
                    }
                    meet.lower = kept_lower;
                    for (mine_lower.slice()) |candidate| {
                        var merged = false;
                        for (meet.lower_any.items[0..meet.lower_any.len]) |*have| {
                            if (have.root == candidate.root) {
                                have.c = @min(have.c, candidate.c);
                                have.assumed |= candidate.assumed;
                                merged = true;
                                break;
                            }
                        }
                        if (!merged) meet.lower_any.append(candidate);
                    }
                    // Same meet for length lower bounds: keep what this edge
                    // also proves, weakened (larger c) to cover both edges.
                    const mine_len = try self.localLenBounds(nid);
                    var kept_len: MeetBounds = .{};
                    for (meet.len_bounds.slice()) |bound| {
                        for (mine_len.slice()) |candidate| {
                            if (candidate.root == bound.root) {
                                kept_len.append(.{ .root = bound.root, .c = @max(bound.c, candidate.c), .assumed = bound.assumed | candidate.assumed });
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
                                have.assumed |= candidate.assumed;
                                merged = true;
                                break;
                            }
                        }
                        if (!merged) meet.len_bounds_any.append(candidate);
                    }
                } else {
                    meet.valid = false;
                    meet.bounds = .{};
                    meet.lower = .{};
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
        const state = self.merge_states.getPtrConst(head) orelse return true;
        if (!state.live) return true;
        if (self.body_joins.get(head)) |join_id| {
            // A loop body can never see its back edges before it walks; it
            // waits only for its entry edges, whose common facts seed it.
            if (state.captures < expected) return !self.entryEdgesComplete(join_id, state);
        }
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
            const state = self.liveMergeState(head);
            const captures = if (state) |st| st.captures else 0;
            // Values bind before facts materialize against them, so a fact
            // about a local's length or value lands on the node the region
            // will read, not on a throwaway.
            if (captures != self.jumpCount(join_id)) {
                try self.seedMergeEnv(head);
                if (self.join_stmts.get(join_id)) |join_stmt| {
                    const join = self.store.getCFStmt(join_stmt).join;
                    const params = self.store.getLocalSpan(join.params);
                    for (0..GuardedList.borrowLen(params)) |i| {
                        const param = GuardedList.at(params, i);
                        try self.seedLoopParam(join_id, param);
                        if (self.lookup(param) == null) try self.bindFresh(param);
                        if (self.lookup(param)) |b| {
                            try self.seeded_param_roots.put(loopBoundKey(join_id, param), b.node);
                            try self.seeded_param_root_set.put(self.rootOf(b.node), {});
                        }
                    }
                }
                try self.seedMergeFacts(head);
                try self.seedLoopFacts(join_id);
                // Facts every entry edge captured this round carries about
                // values the loop never assigns hold throughout the body.
                if (state) |st| {
                    if (self.entryEdgesComplete(join_id, st)) {
                        try self.seedEnvFromMeet(st, join_id);
                        try self.seedStableFacts(&st.entry_stable);
                    }
                }
                return;
            }
        }
        const state = self.liveMergeState(head) orelse {
            try self.seedMergeEnv(head);
            try self.seedMergeFacts(head);
            return;
        };
        const expected = self.mergeExpected(head);
        if (state.captures != expected or expected == 0) {
            try self.seedMergeEnv(head);
            try self.seedMergeFacts(head);
            return;
        }

        // The raw meet carries node ids, which only mean something for edges
        // captured in the head's own region; the stable meet is what edges
        // from other regions agree on. Values bind first so the stable facts
        // materialize against the nodes the region reads.
        for (state.facts.items) |fact| try self.addFact(fact);
        try self.seedEnvFromMeet(state, null);
        try self.seedStableFacts(&state.stable);
    }

    /// The node a met integer entry denotes in the region about to walk: the
    /// shared root when every edge bound the same one, otherwise a fresh
    /// value carrying the upper and lower bounds every edge proved; null
    /// when the meet says nothing a fresh unknown would not.
    fn metScalarNode(self: *Pass, meet: EnvMeet) ResourceError!?NodeId {
        if (meet.valid) {
            return try self.addNode(.{
                .root = meet.root,
                .off_lo = meet.off_lo,
                .off_hi = meet.off_hi,
                .lo = 0,
                .hi = 0,
            });
        }
        if (meet.bounds.len == 0 and meet.lower.len == 0) return null;
        // Only a bound resting on no assumption folds into the value's own
        // range; a range carries no dependency mask, so an assumed bound
        // stays a fact that keeps naming what it rests on.
        var hi = trackedIntMax(self.localLayout(meet.local)) orelse std.math.maxInt(u64);
        for (meet.bounds.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi and bound.assumed == 0) hi = @min(hi, root.lo + bound.c);
        }
        var lo: i128 = 0;
        for (meet.lower.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi and bound.assumed == 0) lo = @max(lo, root.lo - bound.c);
        }
        if (lo > hi) lo = hi;
        const node = (try self.freshRoot(lo, hi)) orelse return null;
        for (meet.bounds.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi and bound.assumed == 0) continue;
            try self.addFact(.{ .a = node, .b = bound.root, .c = bound.c, .origin = .meet, .assumed = bound.assumed });
        }
        for (meet.lower.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi and bound.assumed == 0) continue;
            try self.addFact(.{ .a = bound.root, .b = node, .c = bound.c, .origin = .meet, .assumed = bound.assumed });
        }
        return node;
    }

    /// Bind the merged values of a completed meet in the region about to
    /// walk: a local every edge bound to one root keeps that root; a list
    /// whose edges agree on length lower bounds gets a fresh value whose
    /// length term carries them; a local whose edges agree on upper bounds
    /// gets a fresh value carrying those. With `loop_join`, only locals no
    /// path around that loop assigns are bound: the meet is over the loop's
    /// entry edges, so it describes their values throughout the loop.
    fn seedEnvFromMeet(self: *Pass, state: *const MergeState, loop_join: ?JoinPointId) ResourceError!void {
        for (state.env.items) |meet| {
            if (loop_join) |join_id| {
                if (self.join_assigned.contains(joinAssignedKey(join_id, meet.local))) continue;
            }
            if (meet.field) |field_idx| {
                // The struct's own entry precedes its fields, so the parent
                // is bound (or freshly made) by the time a field seeds.
                const node = (try self.metScalarNode(meet)) orelse continue;
                const parent = (try self.valueOf(meet.local)) orelse continue;
                const field_key = (@as(u64, self.rootOf(parent)) << 32) | field_idx;
                try self.field_values.put(field_key, node);
                try self.int_fields.put(field_key, {});
                try self.struct_roots.put(self.rootOf(parent), {});
                continue;
            }
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
                    try self.addFact(.{ .a = bound.root, .b = len_term, .c = bound.c, .origin = .meet, .assumed = bound.assumed });
                }
                try self.len_terms.put(list_node, len_term);
                try self.path_len_roots.put(len_term, meet.local);
                try self.bind(meet.local, .{ .node = list_node });
                continue;
            }
            // The edges bind different values, but each proves the same
            // bounds; a fresh value carrying those bounds preserves them.
            const node = (try self.metScalarNode(meet)) orelse continue;
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

    /// The form in which a bound on `base` with slack `c` persists, given
    /// the bounds persisted for the same value last round. A bound that
    /// comes back weaker round after round is being pushed along by the loop
    /// it describes and would never settle; after `widen_after` such rounds
    /// it is widened away rather than iterated. A bound that weakens once
    /// or twice while its loop's edges are still being discovered settles.
    fn persistedBound(old: []const StableBound, base: StableBase, c: i128) StableBound {
        for (old) |prev| {
            if (!sameLenBase(prev.base, base)) continue;
            if (prev.c >= widened_slack) return .{ .base = base, .c = widened_slack };
            if (c > prev.c) {
                const grew = prev.grew + 1;
                if (grew >= widen_after) return .{ .base = base, .c = widened_slack };
                return .{ .base = base, .c = c, .grew = grew };
            }
            return .{ .base = base, .c = c };
        }
        return .{ .base = base, .c = c };
    }

    /// Consecutive weakenings after which a persisted bound is widened away.
    const widen_after: u8 = 3;

    /// A bound persisted with this slack has been widened away: it stays in
    /// the list so the widening is remembered from round to round, and
    /// every consumer skips it.
    const widened_slack: i128 = 1 << 62;

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

    /// Round-stable form of a fact endpoint root, or null: a
    /// single-assignment local's value or length, a constant, or a
    /// reassignable local currently bound to the root. A fact about the
    /// value a reassignable local holds on an edge is a fact about whichever
    /// of those values the merge takes, so the meet over edges stays sound.
    fn stabilizeTerm(self: *const Pass, root: NodeId) ?StableTerm {
        return self.stabilizeTermFor(root, null);
    }

    /// As `stabilizeTerm`, but for facts entering the loop `loop_join`: a
    /// reassignable local qualifies only when no path around the loop
    /// assigns it, so its value at entry is its value throughout the loop.
    fn stabilizeTermFor(self: *const Pass, root: NodeId, loop_join: ?JoinPointId) ?StableTerm {
        if (self.len_roots.get(root)) |list_local| return .{ .len_of = list_local };
        if (self.value_roots.get(root)) |scalar_local| return .{ .value_of = scalar_local };
        const node = self.nodes.items[root];
        if (node.lo == node.hi) return .{ .constant = node.lo };
        if (self.path_value_roots.get(root)) |local| {
            if (loop_join == null or !self.join_assigned.contains(joinAssignedKey(loop_join.?, local))) {
                if (self.lookup(local)) |binding| {
                    if (self.rootOf(binding.node) == root) return .{ .value_of = local };
                }
            }
        }
        if (self.path_len_roots.get(root)) |local| {
            if (loop_join == null or !self.join_assigned.contains(joinAssignedKey(loop_join.?, local))) {
                if (self.lookup(local)) |binding| {
                    if (self.len_terms.get(self.rootOf(binding.node))) |term| {
                        if (term == root) return .{ .len_of = local };
                    }
                }
            }
        }
        return null;
    }

    /// Persist a loop join's entry-edge facts whose endpoints are all
    /// round-stable. Such a fact relates values no loop iteration can
    /// reassign—the endpoint locals were bound before entry—so holding on
    /// every entry edge makes it hold throughout the loop.
    fn persistLoopFacts(self: *Pass, join_id: JoinPointId, state: *const MergeState) ResourceError!void {
        if (state.entry_captures == 0) return;
        const stable = &state.entry_stable;
        if (stable.len == 0) return;
        const entry = try self.loop_facts.getOrPut(join_id);
        if (!entry.found_existing) entry.value_ptr.* = .{};
        if (!entry.found_existing or entry.value_ptr.len != stable.len) self.new_loop_bounds = true;
        try entry.value_ptr.assign(self.allocator, stable);
    }

    /// Seed a loop body region with its persisted entry-invariant facts,
    /// materialized against this round's nodes.
    fn seedLoopFacts(self: *Pass, join_id: JoinPointId) ResourceError!void {
        const stored = self.loop_facts.getPtr(join_id) orelse return;
        try self.seedStableFacts(stored);
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
                    try self.len_roots.put(fresh, self.stableLocalOf(list_local));
                } else {
                    try self.path_len_roots.put(fresh, list_local);
                }
                return fresh;
            },
            .constant => |v| return try self.constNode(v),
        }
    }

    /// Stabilize a fact list into round-stable form, keeping facts whose
    /// endpoints all denote something stable.
    fn stabilizeFacts(self: *Pass, stable: *LoopFacts, facts: []const Fact, head: CFStmtId) ResourceError!void {
        try self.stabilizeInto(stable, facts, .path, self.merge_facts.getPtrConst(head));
    }

    /// Persist a fully-captured merge's all-edge fact intersection for
    /// seeding when a later round must walk it before capture completes.
    fn persistMergeFacts(self: *Pass, head: CFStmtId, state: *const MergeState) ResourceError!void {
        const stable = self.persist_scratch;
        try stable.assign(self.allocator, &state.stable);
        if (self.merge_facts.getPtr(head)) |previous| {
            // A fact that comes back weaker round after round is widened
            // away rather than iterated, like a persisted bound.
            for (stable.items[0..stable.len]) |*fact| {
                for (previous.items[0..previous.len]) |old| {
                    if (std.meta.eql(old.a, fact.a) and std.meta.eql(old.b, fact.b)) {
                        if (old.c >= widened_slack) {
                            fact.c = widened_slack;
                        } else if (fact.c > old.c) {
                            fact.grew = old.grew + 1;
                            if (fact.grew >= widen_after) fact.c = widened_slack;
                        }
                        break;
                    }
                }
            }
        }
        if (stable.len == 0) return;
        const entry = try self.merge_facts.getOrPut(head);
        if (entry.found_existing) {
            if (!try self.sameStableFacts(entry.value_ptr, stable)) self.new_loop_bounds = true;
        } else {
            entry.value_ptr.* = .{};
            self.new_loop_bounds = true;
        }
        try entry.value_ptr.assign(self.allocator, stable);
    }

    /// Seed a region walked before its captures complete with the facts
    /// every edge carried last round.
    fn seedMergeFacts(self: *Pass, head: CFStmtId) ResourceError!void {
        const stored = self.merge_facts.getPtr(head) orelse return;
        try self.seedStableFacts(stored);
    }

    /// Add round-stable facts to the current path, materialized against
    /// this round's nodes. The persisted relation is between values;
    /// restated on roots: root_a <= value_a - off_lo_a and
    /// value_b <= root_b + off_hi_b.
    fn seedStableFacts(self: *Pass, stored: *const LoopFacts) ResourceError!void {
        for (stored.items[0..stored.len]) |fact| {
            if (fact.c >= widened_slack) continue;
            const a = (try self.materializeTerm(fact.a)) orelse continue;
            const b = (try self.materializeTerm(fact.b)) orelse continue;
            const c = fact.c + self.offHiOf(b) - self.offLoOf(a);
            try self.addFact(.{ .a = self.rootOf(a), .b = self.rootOf(b), .c = c, .origin = .meet, .assumed = fact.assumed });
        }
    }

    /// Persist a fully-captured merge's env meet in round-stable form.
    fn persistMergeEnv(self: *Pass, head: CFStmtId, state: *const MergeState) ResourceError!void {
        const stable = self.env_scratch;
        stable.len = 0;
        const previous_env = self.merge_env.getPtr(head);
        for (state.env.items) |meet| {
            if (meet.field != null) continue;
            if (stable.len >= merge_env_persist_cap) break;
            var entry = StoredEnvBound{ .local = meet.local, .bounds = undefined, .len = 0, .lower = undefined, .lower_len = 0 };
            var old_bounds: []const StableBound = &.{};
            var old_lower: []const StableBound = &.{};
            if (previous_env) |prev| {
                for (prev.items[0..prev.len]) |old| {
                    if (old.local != meet.local) continue;
                    old_bounds = old.bounds[0..old.len];
                    old_lower = old.lower[0..old.lower_len];
                    break;
                }
            }
            for (meet.bounds.slice()) |bound| {
                if (self.stableBase(bound.root)) |base| {
                    if (entry.len < meet_bound_cap) {
                        entry.bounds[entry.len] = persistedBound(old_bounds, base.base, base.c + bound.c);
                        entry.len += 1;
                    }
                }
            }
            for (meet.lower.slice()) |bound| {
                if (self.lenStable(bound)) |stable_lower| {
                    if (entry.lower_len < meet_bound_cap) {
                        entry.lower[entry.lower_len] = persistedBound(old_lower, stable_lower.base, stable_lower.c);
                        entry.lower_len += 1;
                    }
                }
            }
            if (entry.len == 0 and entry.lower_len == 0) continue;
            stable.items[stable.len] = entry;
            stable.len += 1;
        }
        if (stable.len == 0) return;
        const entry = try self.merge_env.getOrPut(head);
        if (entry.found_existing) {
            if (!entry.value_ptr.sameBounds(stable)) self.new_loop_bounds = true;
        } else {
            entry.value_ptr.* = .{};
            self.new_loop_bounds = true;
        }
        try entry.value_ptr.assign(self.allocator, stable);
    }

    /// Seed the env of a region walked before its captures complete from
    /// last round's stabilized meet: each local binds to a fresh value
    /// carrying the upper bounds every edge proved.
    fn seedMergeEnv(self: *Pass, head: CFStmtId) ResourceError!void {
        const stored = self.merge_env.getPtr(head) orelse return;
        for (stored.items[0..stored.len]) |*entry| {
            const node = (try self.metValueNode(entry.local, entry.bounds[0..entry.len], entry.lower[0..entry.lower_len])) orelse continue;
            var used = false;
            used = try self.seedLowerBounds(node, entry.lower[0..entry.lower_len]) or used;
            for (entry.bounds[0..entry.len]) |bound| {
                if (bound.c >= widened_slack) continue;
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
                    // Folded into the node's own range.
                    .constant => used = true,
                }
            }
            if (used) try self.bind(entry.local, .{ .node = node });
        }
    }

    /// A fresh value for a met local: its layout's range narrowed by the
    /// constant bounds, so the static-range readers (overflow proofs among
    /// them) see those bounds without a fact query.
    fn metValueNode(self: *Pass, local: LocalId, bounds: []const StableBound, lower: []const StableBound) ResourceError!?NodeId {
        var hi = trackedIntMax(self.localLayout(local)) orelse std.math.maxInt(u64);
        for (bounds) |bound| {
            if (bound.c >= widened_slack) continue;
            if (bound.base == .constant) hi = @min(hi, bound.c);
        }
        var lo: i128 = 0;
        for (lower) |bound| {
            if (bound.c >= widened_slack) continue;
            // `0 <= value + c` is `value >= -c`.
            if (bound.base == .constant) lo = @max(lo, -bound.c);
        }
        if (lo > hi) lo = hi;
        return try self.freshRoot(lo, hi);
    }

    /// Assert stored lower bounds `base <= value + c` on a met node against
    /// this round's nodes for the bases; constant bases are folded into the
    /// node's range already. Returns whether any bound applied.
    fn seedLowerBounds(self: *Pass, node: NodeId, lower: []const StableBound) ResourceError!bool {
        var used = false;
        for (lower) |bound| {
            if (bound.c >= widened_slack) continue;
            switch (bound.base) {
                .len_of => |list_local| {
                    const term = (try self.materializeTerm(.{ .len_of = list_local })) orelse continue;
                    try self.addFact(.{ .a = term, .b = node, .c = bound.c, .origin = .meet });
                    used = true;
                },
                .value_of => |scalar_local| {
                    const v = (try self.valueOf(scalar_local)) orelse continue;
                    // `v <= value + c` with `v = root + off`: `root <= value + c - off_lo`.
                    try self.addFact(.{ .a = self.rootOf(v), .b = node, .c = bound.c - self.offLoOf(v), .origin = .meet });
                    used = true;
                },
                .constant => used = true,
            }
        }
        return used;
    }

    fn persistLoopBounds(self: *Pass) ResourceError!void {
        var it = self.merge_states.iterator();
        while (it.next()) |entry| {
            const head = entry.key_ptr.*;
            const state = entry.value_ptr;
            if (!state.live) continue;
            if (!self.live_pending and state.captures == self.mergeExpected(head) and state.captures >= 2) {
                try self.persistMergeFacts(head, state);
                try self.persistMergeEnv(head, state);
            }
            const join_id = self.body_joins.get(head) orelse continue;
            if (!self.live_pending) try self.persistLoopFacts(join_id, state);
            if (state.captures != self.jumpCount(join_id) or state.captures < 2) continue;
            // Only the join's parameters are seeded from these bounds, so
            // only theirs are persisted; a candidate for any other local
            // would sit pending, never assumed, and its admission would
            // ask for another round for nothing.
            const join_stmt = self.join_stmts.get(join_id) orelse continue;
            const params = self.store.getLocalSpan(self.store.getCFStmt(join_stmt).join.params);
            for (state.env.items) |meet| {
                if (meet.field != null) continue;
                var is_param = false;
                for (0..GuardedList.borrowLen(params)) |i| {
                    if (GuardedList.at(params, i) == meet.local) {
                        is_param = true;
                        break;
                    }
                }
                if (!is_param) continue;
                const key = loopBoundKey(join_id, meet.local);
                if (self.live_pending) {
                    // Assumption round: the walk ran under unverified seeds,
                    // so nothing new is persisted; pending invariants that
                    // this round re-derived on every edge are marked.
                    const stored = self.loop_bounds.getPtr(key) orelse continue;
                    for (stored.len_items[0..stored.len_count]) |*item| {
                        if (item.status != .pending) continue;
                        const bounds = if (item.kind == .length) meet.len_bounds.slice() else meet.lower.slice();
                        for (bounds) |bound| {
                            if (self.lenStable(bound)) |candidate| {
                                if (sameLenBase(candidate.base, item.base) and candidate.c <= item.c) {
                                    item.hit = true;
                                    item.hit_deps |= bound.assumed;
                                    break;
                                }
                            }
                        }
                    }
                    continue;
                }

                const stable = &self.bounds_scratch;
                stable.len = 0;
                stable.lower_len = 0;
                stable.len_count = 0;
                const previous_bounds = self.loop_bounds.getPtr(key);
                const old_items: []const StableBound = if (previous_bounds) |prev| prev.items[0..prev.len] else &.{};
                const old_lower: []const StableBound = if (previous_bounds) |prev| prev.lower_items[0..prev.lower_len] else &.{};
                for (meet.bounds.slice()) |bound| {
                    if (self.stableBase(bound.root)) |base| {
                        if (stable.len < meet_bound_cap) {
                            stable.items[stable.len] = persistedBound(old_items, base.base, base.c + bound.c);
                            stable.len += 1;
                        }
                    }
                }
                for (meet.lower.slice()) |bound| {
                    if (self.lenStable(bound)) |stable_lower| {
                        if (stable.lower_len < meet_bound_cap) {
                            stable.lower_items[stable.lower_len] = persistedBound(old_lower, stable_lower.base, stable_lower.c);
                            stable.lower_len += 1;
                        }
                    }
                }
                // Carry the invariant list forward, admitting new length
                // candidates as pending. A base that already failed
                // verification is listed as dead rather than re-admitted.
                if (previous_bounds) |previous| {
                    @memcpy(stable.len_items[0..previous.len_count], previous.len_items[0..previous.len_count]);
                    stable.len_count = previous.len_count;
                }
                self.admitLenCandidates(&meet, stable);
                if (stable.len == 0 and stable.lower_len == 0 and stable.len_count == 0) continue;
                if (previous_bounds == null or previous_bounds.?.len != stable.len or previous_bounds.?.lower_len != stable.lower_len) self.new_loop_bounds = true;
                const slot = try self.loop_bounds.getOrPut(key);
                if (!slot.found_existing) slot.value_ptr.* = .{};
                try slot.value_ptr.assign(self.allocator, stable);
            }
        }
    }

    /// Admit the bounds some captured edge proves for a parameter as pending
    /// invariants of its loop, into `stored`, whose invariant list has room
    /// for `meet_bound_cap`; back edges have not re-derived a candidate
    /// yet, so the bounds every edge proves would miss it. A constant bound
    /// below one is what any length already satisfies, as is a bound slack
    /// enough to hold for any pair of values; assuming either would cost a
    /// round for nothing.
    fn admitLenCandidates(self: *Pass, meet: *const EnvMeet, stored: *LoopBounds) void {
        const is_int = trackedIntMax(self.localLayout(meet.local)) != null;
        const bounds = if (is_int) meet.lower_any.slice() else meet.len_bounds_any.slice();
        for (bounds) |bound| {
            const candidate = self.lenStable(bound) orelse continue;
            if (candidate.base == .constant and candidate.c >= 0) continue;
            if (candidate.c >= std.math.maxInt(i64)) continue;
            var known = false;
            for (stored.len_items[0..stored.len_count]) |item| {
                if (sameLenBase(candidate.base, item.base)) {
                    known = true;
                    break;
                }
            }
            if (!known and stored.len_count < meet_bound_cap) {
                stored.len_items[stored.len_count] = .{
                    .base = candidate.base,
                    .c = candidate.c,
                    .kind = if (is_int) .value else .length,
                    .status = .pending,
                    .hit = false,
                };
                stored.len_count += 1;
                self.new_loop_bounds = true;
            }
        }
    }

    /// Resolution of the assumed invariants at round end. In an assumption
    /// round, the assumptions re-derived on every edge stand together
    /// (simultaneous induction): failures die, one resting on a fallen
    /// assumption retries next round without it, and the rest verify. Once
    /// the rounds after a round that made progress reach their fixpoint,
    /// assumptions that died under an earlier epoch's facts are seeded
    /// again, since a failure only means unprovable under those facts.
    fn resolvePendingInvariants(self: *Pass) void {
        if (!self.live_pending) {
            // A round that rewrote statements or persisted new bounds opens a
            // new epoch; the rounds after it run to their fixpoint first.
            if (self.rewrites > 0 or self.new_loop_bounds) {
                self.progress_epoch += 1;
                return;
            }
            // At the fixpoint, an assumption that failed under an earlier
            // epoch's facts is worth one more try against the stronger ones;
            // one that failed under these very facts is not.
            var revive_it = self.loop_bounds.valueIterator();
            while (revive_it.next()) |stored| {
                for (stored.len_items[0..stored.len_count]) |*item| {
                    if (item.status != .dead or item.died_epoch >= self.progress_epoch) continue;
                    item.status = .pending;
                    self.new_loop_bounds = true;
                }
            }
            return;
        }
        var any_pending = false;
        var it = self.loop_bounds.valueIterator();
        while (it.next()) |stored| {
            for (stored.len_items[0..stored.len_count]) |*item| {
                if (item.status != .pending) continue;
                any_pending = true;
                if (!item.hit) {
                    item.status = .dead;
                    item.died_epoch = self.progress_epoch;
                }
            }
        }
        if (!any_pending) return;

        // An assumption verifies when every assumption its re-derivations
        // rested on is verified or is itself verifying now: the standing set
        // starts as every re-derived assumption and sheds those resting on
        // one that fell (a dead assumption, or one shed earlier), so mutually
        // supporting assumptions promote together and an assumption resting
        // on a fallen one retries next round without it.
        var standing_changed = true;
        while (standing_changed) {
            standing_changed = false;
            var standing: u64 = 0;
            var unknown_fell = false;
            it = self.loop_bounds.valueIterator();
            while (it.next()) |stored| {
                for (stored.len_items[0..stored.len_count]) |item| {
                    switch (item.status) {
                        .verified => standing |= item.assume_bit,
                        .pending => if (item.hit) {
                            standing |= item.assume_bit;
                        } else if (item.assume_bit == unknown_assumption_bit) {
                            unknown_fell = true;
                        },
                        .dead => if (item.assume_bit == unknown_assumption_bit) {
                            unknown_fell = true;
                        },
                    }
                }
            }
            if (unknown_fell) standing &= ~unknown_assumption_bit;
            it = self.loop_bounds.valueIterator();
            while (it.next()) |stored| {
                for (stored.len_items[0..stored.len_count]) |*item| {
                    if (item.status != .pending or !item.hit) continue;
                    if (item.hit_deps & ~standing == 0) continue;
                    item.hit = false;
                    standing_changed = true;
                }
            }
        }
        it = self.loop_bounds.valueIterator();
        while (it.next()) |stored| {
            for (stored.len_items[0..stored.len_count]) |*item| {
                if (item.status == .pending and item.hit) item.status = .verified;
            }
        }
        // A survivor that rested on a fallen assumption retries next round:
        // the dependency is recorded over every fact a query relaxed
        // through, so it may name an assumption the bound never needed. A
        // dead one waits for a round that makes progress before it is
        // seeded again.
        self.new_loop_bounds = true;
    }

    /// Seed a loop parameter from bounds persisted by an earlier round,
    /// materialized against this round's nodes.
    fn seedLoopParam(self: *Pass, join_id: JoinPointId, local: LocalId) ResourceError!void {
        const stored = self.loop_bounds.getPtr(loopBoundKey(join_id, local)) orelse return;
        if (stored.len_count > 0 and trackedIntMax(self.localLayout(local)) == null) {
            try self.seedLenInvariants(stored, local);
            return;
        }
        const node = (try self.metValueNode(local, stored.items[0..stored.len], stored.lower_items[0..stored.lower_len])) orelse return;
        var used = false;
        // Lower-bound invariants of an integer parameter: verified ones hold
        // outright, pending ones are this round's assumptions.
        for (stored.len_items[0..stored.len_count]) |*item| {
            item.hit = false;
            item.assume_bit = 0;
            item.hit_deps = 0;
            if (item.status == .dead) continue;
            const base_root: ?struct { root: NodeId, c: i128 } = switch (item.base) {
                .len_of => |list_local| blk: {
                    const term = (try self.materializeTerm(.{ .len_of = list_local })) orelse break :blk null;
                    break :blk .{ .root = term, .c = item.c };
                },
                .value_of => |scalar_local| blk: {
                    const v = (try self.valueOf(scalar_local)) orelse break :blk null;
                    break :blk .{ .root = self.rootOf(v), .c = item.c - self.offLoOf(v) };
                },
                .constant => blk: {
                    const zero = (try self.constNode(0)) orelse break :blk null;
                    break :blk .{ .root = zero, .c = item.c };
                },
            };
            const resolved = base_root orelse continue;
            if (item.status == .pending) {
                self.live_pending = true;
                if (self.assumption_count < 63) {
                    item.assume_bit = @as(u64, 1) << @intCast(self.assumption_count);
                    self.assumption_count += 1;
                } else {
                    item.assume_bit = unknown_assumption_bit;
                }
            }
            try self.addFact(.{ .a = resolved.root, .b = node, .c = resolved.c, .origin = .meet, .assumed = item.assume_bit });
            used = true;
        }
        used = try self.seedLowerBounds(node, stored.lower_items[0..stored.lower_len]) or used;
        for (stored.items[0..stored.len]) |bound| {
            if (bound.c >= widened_slack) continue;
            switch (bound.base) {
                .len_of => |list_local| {
                    const list_node = (try self.valueOf(list_local)) orelse continue;
                    const root = self.rootOf(list_node);
                    const len_node = self.len_terms.get(root) orelse blk: {
                        const fresh = (try self.freshRoot(0, std.math.maxInt(i64))) orelse continue;
                        try self.len_terms.put(root, fresh);
                        try self.len_roots.put(fresh, self.stableLocalOf(list_local));
                        break :blk fresh;
                    };
                    try self.addFact(.{ .a = node, .b = len_node, .c = bound.c, .origin = .meet });
                    used = true;
                },
                // Folded into the node's own range.
                .constant => used = true,
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
                            try self.len_roots.put(fresh, self.stableLocalOf(list_local));
                        } else {
                            try self.path_len_roots.put(fresh, list_local);
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
            item.assume_bit = 0;
            item.hit_deps = 0;
            if (item.status == .pending) {
                self.live_pending = true;
                if (self.assumption_count < 63) {
                    item.assume_bit = @as(u64, 1) << @intCast(self.assumption_count);
                    self.assumption_count += 1;
                } else {
                    item.assume_bit = unknown_assumption_bit;
                }
            }
            try self.addFact(.{ .a = resolved.root, .b = len_term, .c = resolved.c, .origin = .meet, .assumed = item.assume_bit });
            seeded = true;
        }
        if (seeded) {
            try self.len_terms.put(list_node, len_term);
            try self.path_len_roots.put(len_term, local);
            try self.bind(local, .{ .node = list_node });
        }
    }

    /// Upper bounds `value <= root + c` provable for a node from the current
    /// path facts, found by walking fact edges forward from its root.
    fn reachableBounds(self: *Pass, node_id: NodeId) ResourceError!MeetBounds {
        self.query_used = 0;
        const node = self.nodes.items[node_id];
        try self.relaxFresh(node.root, .forward);
        // One bound per reached root, at the tightest slack the walk found.
        var bounds: QueryBounds = .{};
        var it = self.query_best.iterator();
        while (it.next()) |entry| {
            bounds.append(.{ .root = entry.key_ptr.*, .c = entry.value_ptr.c + node.off_hi, .assumed = entry.value_ptr.assumed });
        }
        return try self.normalizeUpperBounds(bounds, node);
    }

    /// Bounds against literal values are keyed by one shared constant root,
    /// with the literal folded into `c`, so two edges that bound a value by
    /// different constants (a guard on entry, a shift's range on the back
    /// edge, say) still meet. A root's own static range is such a bound too.
    fn constantRoot(self: *Pass) ResourceError!?NodeId {
        if (self.zero_node) |id| return id;
        const id = (try self.constNode(0)) orelse return null;
        self.zero_node = id;
        return id;
    }

    fn normalizeUpperBounds(self: *Pass, bounds: QueryBounds, node: Node) ResourceError!MeetBounds {
        var out: MeetBounds = .{};
        const zero = (try self.constantRoot()) orelse {
            for (bounds.slice()) |bound| out.append(bound);
            return out;
        };
        var best: ?i128 = null;
        var best_assumed: u64 = 0;
        for (bounds.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi) {
                const c = root.lo + bound.c;
                if (best == null or c < best.?) {
                    best = c;
                    best_assumed = bound.assumed;
                }
            }
        }
        const root = self.nodes.items[node.root];
        if (root.hi < std.math.maxInt(u64)) {
            const c = root.hi + node.off_hi;
            if (best == null or c < best.?) {
                best = c;
                best_assumed = 0;
            }
        }
        // The constant bound goes first: a meet keeps only a few bounds, and
        // this is the one every edge can share.
        if (best) |c| out.append(.{ .root = zero, .c = c, .assumed = best_assumed });
        for (bounds.slice()) |bound| {
            const bound_root = self.nodes.items[bound.root];
            if (bound_root.lo != bound_root.hi) out.append(bound);
        }
        return out;
    }

    fn normalizeLowerBounds(self: *Pass, bounds: QueryBounds, node: Node) ResourceError!MeetBounds {
        var out: MeetBounds = .{};
        const zero = (try self.constantRoot()) orelse {
            for (bounds.slice()) |bound| out.append(bound);
            return out;
        };
        var best: ?i128 = null;
        var best_assumed: u64 = 0;
        for (bounds.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi) {
                const c = bound.c - root.lo;
                if (best == null or c < best.?) {
                    best = c;
                    best_assumed = bound.assumed;
                }
            }
        }
        // The node's own range floor is a constant lower bound too.
        const own = self.nodes.items[node.root];
        if (own.lo > 0) {
            const c = -(own.lo + node.off_lo);
            if (best == null or c < best.?) {
                best = c;
                best_assumed = 0;
            }
        }
        // The constant bound goes first, as in `normalizeUpperBounds`.
        if (best) |c| out.append(.{ .root = zero, .c = c, .assumed = best_assumed });
        for (bounds.slice()) |bound| {
            const bound_root = self.nodes.items[bound.root];
            if (bound_root.lo != bound_root.hi) out.append(bound);
        }
        return out;
    }

    /// Lower bounds `root <= value(len_node) + c` provable from the current
    /// path facts, found by walking fact edges backward from the length
    /// term's root. Smaller `c` is the stronger claim.
    fn lenLowerBounds(self: *Pass, len_node: NodeId) ResourceError!MeetBounds {
        self.query_used = 0;
        const node = self.nodes.items[len_node];
        try self.relaxFresh(node.root, .backward);
        // One bound per reached root, at the tightest slack the walk found.
        var bounds: QueryBounds = .{};
        var it = self.query_best.iterator();
        while (it.next()) |entry| {
            bounds.append(.{ .root = entry.key_ptr.*, .c = entry.value_ptr.c - node.off_lo, .assumed = entry.value_ptr.assumed });
        }
        return try self.normalizeLowerBounds(bounds, node);
    }

    /// Lower bounds `root <= value + c` of an integer local's value from
    /// the current path facts, dropping constant bounds no unsigned value
    /// fails (zero or below).
    fn valueLowerBounds(self: *Pass, local: LocalId, node: NodeId) ResourceError!MeetBounds {
        if (trackedIntMax(self.localLayout(local)) == null) return .{};
        return self.valueLowerBoundsNode(node);
    }

    /// `valueLowerBounds` for a node already known to hold an integer.
    fn valueLowerBoundsNode(self: *Pass, node: NodeId) ResourceError!MeetBounds {
        const all = try self.lenLowerBounds(node);
        var kept: MeetBounds = .{};
        for (all.slice()) |bound| {
            const root = self.nodes.items[bound.root];
            if (root.lo == root.hi and root.lo - bound.c < 1) continue;
            kept.append(bound);
        }
        return kept;
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
    fn threadBoolJoins(self: *Pass, body: CFStmtId) ResourceError!u32 {
        var threaded: u32 = 0;
        // Jump records cover only code the prescan reached, and it never scans
        // a join body without a reachable jump. Retargeting must still cover
        // every structural jump, or an unscanned one would keep the join id
        // this rewrite retires. Most procedures thread nothing, so the walk
        // waits for the first join that qualifies.
        var structural_jumps = std.ArrayList(JumpRecord).empty;
        defer structural_jumps.deinit(self.allocator);
        var structural_jumps_ready = false;
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
                .assign_boxy_record_update,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
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
                .assign_boxy_record_update,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
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

            if (!structural_jumps_ready) {
                var walk = try BodyClone.ReachableStmts.initWithAllocator(self.store, body, self.allocator);
                defer walk.deinit();
                while (try walk.next()) |stmt| {
                    const cf = self.store.getCFStmt(stmt);
                    if (cf == .jump) try structural_jumps.append(self.allocator, .{ .target = cf.jump.target, .stmt = stmt });
                }
                structural_jumps_ready = true;
            }

            const true_id: JoinPointId = @enumFromInt(self.max_join_id);
            const false_id: JoinPointId = @enumFromInt(self.max_join_id + 1);
            self.max_join_id += 2;

            const empty_params = try self.store.addLocalSpan(&.{});
            const join_origin = self.store.stmtOrigin(join_stmt);
            var split_join_origin = join_origin;
            split_join_origin.kind = .range_prove;
            const false_join = try self.store.addCFStmt(.{ .join = .{
                .id = false_id,
                .params = empty_params,
                .retained = join.retained,
                .body = false_arm,
                .remainder = join.remainder,
            } }, split_join_origin);
            try self.store.replaceCFStmt(join_stmt, .{ .join = .{
                .id = true_id,
                .params = empty_params,
                .retained = join.retained,
                .body = true_arm,
                .remainder = false_join,
            } }, join_origin);

            for (structural_jumps.items) |record| {
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
                    .assign_boxy_record_update,
                    .assign_boxy_reuse_box,
                    .assign_boxy_unbox,
                    .assign_boxy_adapt,
                    .assign_boxy_inspect,
                    .assign_boxy_eq,
                    .assign_boxy_hash,
                    .assign_boxy_tag,
                    .assign_boxy_tag_payload,
                    .boxy_tag_match,
                    .assign_call_dict,
                    => continue,
                }
                const site_origin = self.store.stmtOrigin(record.stmt);
                var split_site_origin = site_origin;
                split_site_origin.kind = .range_prove;
                const true_jump = try self.store.addCFStmt(.{ .jump = .{ .target = true_id } }, split_site_origin);
                const false_jump = try self.store.addCFStmt(.{ .jump = .{ .target = false_id } }, split_site_origin);
                const site_branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = true_jump }});
                try self.store.replaceCFStmt(record.stmt, .{ .switch_stmt = .{
                    .cond = param,
                    .branches = site_branches,
                    .default_branch = false_jump,
                    .continuation = null,
                } }, site_origin);
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
            .claim = claim,
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
                            invariant(
                                "range_prove certification failed: fact from s{d} does not dominate rewritten s{d}",
                                .{ @intFromEnum(origin_stmt), @intFromEnum(record.stmt) },
                            );
                        }
                    },
                    .meet => {},
                }
            }
            switch (record.claim) {
                .ordering => |claim| {
                    if (!RangeProveCertify.implies(self.allocator, facts, self.nodes.items, claim.a, claim.b, claim.m)) {
                        invariant(
                            "range_prove certification failed: claim at s{d} does not follow from its facts",
                            .{@intFromEnum(record.stmt)},
                        );
                    }
                },
                .no_overflow => |claim| {
                    if (!doms.dominates(claim.edge_head, record.stmt) or
                        !RangeProveCertify.isFalseOverflowEdge(self.store, claim))
                    {
                        invariant(
                            "range_prove certification failed: overflow claim at s{d} does not follow from its false predicate edge",
                            .{@intFromEnum(record.stmt)},
                        );
                    }
                },
            }
        }
    }

    // Proc driver

    fn transformProc(self: *Pass, proc_id: LIR.LirProcSpecId) ResourceError!void {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body == null or proc.hosted != null) return;

        self.freePersisted();
        self.loop_bounds.clearRetainingCapacity();
        self.progress_epoch = 0;
        self.loop_facts.clearRetainingCapacity();
        self.merge_facts.clearRetainingCapacity();
        self.merge_env.clearRetainingCapacity();
        var round: u32 = 0;
        while (round < max_rounds) : (round += 1) {
            self.resetRound();
            try self.prescanProc(proc);
            if (round == 0) try self.reserveMergeStorage();
            // Threading restructures control flow, so a round that threads
            // stops there and the next round re-derives the graph facts.
            self.new_loop_bounds = false;
            if (try self.threadBoolJoins(proc.body.?) == 0) {
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
            self.truncateFacts(0);
            self.no_overflow_facts.clearRetainingCapacity();
            self.frames.clearRetainingCapacity();
            self.beginSeeding();
            for (self.global_facts.items) |fact| try self.addFact(fact);
            try self.seedFromMerge(head);
            self.endSeeding();
            try self.frames.append(self.allocator, .{
                .stmt = head,
                .facts_len = self.facts.items.len,
                .no_overflow_facts_len = self.no_overflow_facts.items.len,
                .undo_len = self.undo.items.len,
                .edge_fact = null,
            });
            try self.walkRegion(head);
        }
    }

    fn walkRegion(self: *Pass, head: CFStmtId) ResourceError!void {
        while (self.frames.pop()) |frame| {
            try self.rewindTo(frame.facts_len, frame.no_overflow_facts_len, frame.undo_len);
            if (frame.edge_fact) |fact| switch (fact) {
                .ordering => |ordering| try self.addFact(ordering),
                .no_overflow => |no_overflow| try self.no_overflow_facts.append(self.allocator, no_overflow),
                .equal => |edge| try self.addEqualityEdge(edge, true),
                .unequal => |edge| try self.addEqualityEdge(edge, false),
            };

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
                                    const source_binding = self.lookup(src);
                                    try self.bind(s.target, .{
                                        .node = node,
                                        .pred = if (source_binding) |b| b.pred else null,
                                        .overflow_pred = if (source_binding) |b| b.overflow_pred else null,
                                        .arithmetic_chain = if (source_binding) |b| b.arithmetic_chain else null,
                                    });
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
                        if (std.meta.activeTag(self.store.getCFStmt(current)) == .crash) break :walk;
                        current = s.next;
                    },
                    .set_local => |s| {
                        try self.visited.put(current, {});
                        if (try self.valueOf(s.value)) |node| {
                            const source_binding = self.lookup(s.value);
                            try self.bind(s.target, .{
                                .node = node,
                                .pred = if (source_binding) |b| b.pred else null,
                                .overflow_pred = if (source_binding) |b| b.overflow_pred else null,
                                .arithmetic_chain = if (source_binding) |b| b.arithmetic_chain else null,
                            });
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
                    .assign_boxy_record_update => |s| {
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
                    .assign_boxy_hash => |s| {
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
                        try self.modelStruct(s.target, s.fields);
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

    fn pushFrame(self: *Pass, stmt: CFStmtId, edge_fact: ?EdgeFact) ResourceError!void {
        try self.frames.append(self.allocator, .{
            .stmt = stmt,
            .facts_len = self.facts.items.len,
            .no_overflow_facts_len = self.no_overflow_facts.items.len,
            .undo_len = self.undo.items.len,
            .edge_fact = edge_fact,
        });
    }

    /// Push switch arms, asserting the condition's comparison along Bool
    /// edges: the `1` arm asserts it and the `0`/default arm asserts its
    /// negation.
    fn pushSwitchArms(self: *Pass, s: anytype, switch_stmt: CFStmtId) ResourceError!void {
        const binding = self.lookup(s.cond);
        const cond_is_bool = self.localLayout(s.cond) == .bool;

        const branches = self.store.getCFSwitchBranches(s.branches);
        const branch_count = GuardedList.borrowLen(branches);

        var default_fact: ?EdgeFact = null;
        if (binding != null and cond_is_bool and branch_count == 1) {
            const only = GuardedList.at(branches, 0);
            if (only.value == 1) {
                default_fact = self.boolEdgeFact(binding.?, false, switch_stmt, s.default_branch);
            } else if (only.value == 0) {
                default_fact = self.boolEdgeFact(binding.?, true, switch_stmt, s.default_branch);
            }
        }
        try self.pushFrame(s.default_branch, default_fact);

        for (0..branch_count) |i| {
            const branch = GuardedList.at(branches, i);
            var edge_fact: ?EdgeFact = null;
            if (binding != null and cond_is_bool) {
                if (branch.value == 1) {
                    edge_fact = self.boolEdgeFact(binding.?, true, switch_stmt, branch.body);
                } else if (branch.value == 0) {
                    edge_fact = self.boolEdgeFact(binding.?, false, switch_stmt, branch.body);
                }
            }
            try self.pushFrame(branch.body, edge_fact);
        }
    }

    fn boolEdgeFact(self: *const Pass, binding: Binding, holds: bool, switch_stmt: CFStmtId, edge_head: CFStmtId) ?EdgeFact {
        if (binding.pred) |pred| {
            const edge = EqualityEdge{ .a = pred.a, .b = pred.b, .switch_stmt = switch_stmt };
            return switch (pred.op) {
                .eq => if (holds) .{ .equal = edge } else .{ .unequal = edge },
                .ne => if (holds) .{ .unequal = edge } else .{ .equal = edge },
                .lt, .lte, .gt, .gte => .{ .ordering = self.predFact(pred, holds, switch_stmt) },
            };
        }
        if (!holds) {
            if (binding.overflow_pred) |overflow_pred| return .{ .no_overflow = .{
                .predicate = overflow_pred,
                .switch_stmt = switch_stmt,
                .edge_head = edge_head,
            } };
        }
        return null;
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
            .eq, .ne => unreachable,
        };
    }

    /// Assert the facts of an equality edge: equal values order both ways;
    /// unequal values make a non-strict ordering the path already proves
    /// strict.
    fn addEqualityEdge(self: *Pass, edge: EqualityEdge, equal: bool) ResourceError!void {
        const origin = FactOrigin{ .branch = edge.switch_stmt };
        if (equal) {
            try self.addFact(self.orderingFact(edge.a, edge.b, 0, origin));
            try self.addFact(self.orderingFact(edge.b, edge.a, 0, origin));
            return;
        }
        if (try self.proveLe(edge.a, edge.b, 0)) {
            var fact = self.orderingFact(edge.a, edge.b, -1, origin);
            fact.assumed = self.query_used;
            try self.addFact(fact);
        }
        if (try self.proveLe(edge.b, edge.a, 0)) {
            var fact = self.orderingFact(edge.b, edge.a, -1, origin);
            fact.assumed = self.query_used;
            try self.addFact(fact);
        }
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
            // A non-negative literal is the same number whatever its type,
            // so a signed literal binds too: as a mask it bounds a masked
            // signed value, which the wrap rules can then carry into the
            // unsigned world.
            const layout_idx = self.localLayout(target);
            if (trackedIntMax(layout_idx) != null or isSignedInt(layout_idx)) {
                if (try self.constNode(v)) |node| {
                    try self.bind(target, .{ .node = node });
                    return;
                }
            }
        }
        try self.bindFresh(target);
    }

    fn isSignedInt(layout_idx: layout_mod.Idx) bool {
        return switch (layout_idx) {
            .i8, .i16, .i32, .i64, .i128 => true,
            .u8, .u16, .u32, .u64, .u128, .bool, .str, .f32, .f64, .dec, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => false,
            _ => false,
        };
    }

    fn modelLowLevel(self: *Pass, stmt: CFStmtId, s: anytype) ResourceError!void {
        const args = self.store.getLocalSpan(s.args);
        const arg_count = GuardedList.borrowLen(args);

        switch (s.op) {
            .simd_concat_shift_bytes => {
                std.debug.assert(arg_count == 3);
                if (s.simd_concat_count == null) {
                    if (try self.valueOf(GuardedList.at(args, 2))) |node| {
                        if (self.constValueOf(node)) |count| {
                            // Unreachable invalid-count arms can still be in
                            // LIR before their guarding switch is eliminated.
                            // A node's window never rests on an assumption,
                            // so the count needs no verification round.
                            if (count >= 0 and count <= 16) {
                                self.store.getCFStmtPtr(stmt).assign_low_level.simd_concat_count = @intCast(count);
                            }
                        }
                    }
                }
                // This annotation neither changes dataflow nor enables another
                // proof, so it does not request another range-analysis round.
                try self.bindFresh(s.target);
            },
            .list_map_prepare_reuse => {
                // Ownership transfer preserves the list value, including its
                // length. Keep the input's value identity across the transfer.
                std.debug.assert(arg_count == 1);
                if (try self.valueOf(GuardedList.at(args, 0))) |node| {
                    try self.bind(s.target, .{ .node = node });
                } else {
                    try self.bindFresh(s.target);
                }
            },
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
                                try self.len_roots.put(len_node, self.stableLocalOf(list_local));
                            } else {
                                try self.path_len_roots.put(len_node, list_local);
                            }
                            try self.bind(s.target, .{ .node = len_node });
                            return;
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            // A hint with no result to describe.
            .list_prefetch => try self.bindFresh(s.target),
            // LIR lowering splits this into an alias and `list_prefetch`.
            .list_prefetched => unreachable,
            .list_capacity => {
                // A list's capacity is a non-negative count with no tighter
                // statically known bound.
                try self.bindFresh(s.target);
            },
            .list_clear => {
                // The cleared list has no items: its length term is zero.
                if (try self.unknownFor(self.localLayout(s.target))) |out_node| {
                    if (try self.constNode(0)) |zero| {
                        try self.len_terms.put(out_node, zero);
                        try self.bind(s.target, .{ .node = out_node });
                        return;
                    }
                }
                try self.bindFresh(s.target);
            },
            .list_append_unsafe => {
                // One appended element: the result's length is the input's
                // term plus one.
                if (arg_count == 2) {
                    if (try self.valueOf(GuardedList.at(args, 0))) |in_node| {
                        if (self.len_terms.get(self.rootOf(in_node))) |len_term| {
                            if (try self.derived(len_term, 1)) |out_len| {
                                if (try self.unknownFor(self.localLayout(s.target))) |out_node| {
                                    try self.len_terms.put(out_node, out_len);
                                    try self.bind(s.target, .{ .node = out_node });
                                    return;
                                }
                            }
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            .list_with_capacity => {
                // A reserved list has no items: its length term is zero.
                if (try self.unknownFor(self.localLayout(s.target))) |out_node| {
                    if (try self.constNode(0)) |zero| {
                        try self.len_terms.put(out_node, zero);
                        try self.bind(s.target, .{ .node = out_node });
                        return;
                    }
                }
                try self.bindFresh(s.target);
            },
            .list_reserve, .list_reserve_for_append => {
                // Reserving capacity keeps every item, so the result shares
                // the input's length term.
                if (arg_count == 2) {
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
            .list_map_cast_unsafe => {
                // An in-place map's storage keeps every slot, so the cast
                // output has the input's length.
                if (arg_count == 1) {
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
            .list_set, .list_set_in_place_unsafe, .list_map_write_unsafe => {
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
            .i8_to_u8_wrap, .i8_to_u16_wrap, .i8_to_u32_wrap, .i8_to_u64_wrap, .i16_to_u8_wrap, .i16_to_u16_wrap, .i16_to_u32_wrap, .i16_to_u64_wrap, .i32_to_u8_wrap, .i32_to_u16_wrap, .i32_to_u32_wrap, .i32_to_u64_wrap, .i64_to_u8_wrap, .i64_to_u16_wrap, .i64_to_u32_wrap, .i64_to_u64_wrap => {
                // A signed value the path proves non-negative and in range
                // (a masked table node, say) is the same number afterwards.
                // Signed values are otherwise untracked, so the argument's
                // range is only ever narrower than its type when a rule here
                // established it.
                if (arg_count == 1) {
                    if (try self.valueOf(GuardedList.at(args, 0))) |node_id| {
                        const node = self.nodes.items[node_id];
                        const root = self.nodes.items[node.root];
                        const max = trackedIntMax(self.localLayout(s.target));
                        if (max != null and root.lo + node.off_lo >= 0 and root.hi + node.off_hi <= max.?) {
                            try self.bind(s.target, .{ .node = node_id });
                            return;
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            .u16_to_u8_wrap, .u32_to_u8_wrap, .u32_to_u16_wrap, .u64_to_u8_wrap, .u64_to_u16_wrap, .u64_to_u32_wrap, .u128_to_u8_wrap, .u128_to_u16_wrap, .u128_to_u32_wrap, .u128_to_u64_wrap => {
                // A narrowing that provably cannot wrap leaves the number
                // alone as well: a shift amount computed from literals, say,
                // stays a constant the shift rule can read.
                if (arg_count == 1) {
                    if (try self.valueOf(GuardedList.at(args, 0))) |node_id| {
                        const node = self.nodes.items[node_id];
                        const root = self.nodes.items[node.root];
                        const max = trackedIntMax(self.localLayout(s.target));
                        if (max != null and root.hi + node.off_hi <= max.? and root.lo + node.off_lo >= 0) {
                            try self.bind(s.target, .{ .node = node_id });
                            return;
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            .u8_to_u16, .u8_to_u32, .u8_to_u64, .u16_to_u32, .u16_to_u64, .u32_to_u64 => {
                // A widening whose target represents every value of its source
                // leaves the number alone, so the result is the argument: it
                // keeps the argument's bounds and its offsets to other values
                // rather than falling back to the wider type's whole range.
                if (arg_count == 1) {
                    if (try self.valueOf(GuardedList.at(args, 0))) |node| {
                        try self.bind(s.target, .{ .node = node });
                        return;
                    }
                }
                try self.bindFresh(s.target);
            },
            .bool_not => {
                // Negating a modeled comparison flips its kind, so a switch
                // on the negation asserts the comparison's complement.
                if (arg_count == 1) {
                    if (self.lookup(GuardedList.at(args, 0))) |binding| {
                        if (binding.pred) |pred| {
                            if (try self.freshRoot(0, 1)) |bool_node| {
                                try self.bind(s.target, .{ .node = bool_node, .pred = .{ .op = pred.op.negated(), .a = pred.a, .b = pred.b } });
                                return;
                            }
                        }
                    }
                }
                try self.bindFresh(s.target);
            },
            .num_is_eq, .num_is_lt, .num_is_lte, .num_is_gt, .num_is_gte => {
                try self.modelCompare(stmt, s, args, arg_count);
            },
            .bool_likely => {
                // The expected outcome of a branch, not a different value:
                // the result is the operand, with the comparison that
                // defined it, so a switch on it still asserts that
                // comparison along its arms.
                if (arg_count == 1) {
                    const operand = GuardedList.at(args, 0);
                    if (try self.valueOf(operand)) |node| {
                        const source = self.lookup(operand);
                        try self.bind(s.target, .{
                            .node = node,
                            .pred = if (source) |b| b.pred else null,
                            .overflow_pred = if (source) |b| b.overflow_pred else null,
                            .arithmetic_chain = if (source) |b| b.arithmetic_chain else null,
                        });
                        return;
                    }
                }
                try self.bindFresh(s.target);
            },
            .num_int_add_wrap,
            .num_int_add_crash_on_overflow,
            .num_int_add_overflows,
            .num_int_add_proven_cannot_overflow,
            .num_int_sub_wrap,
            .num_int_sub_crash_on_overflow,
            .num_int_sub_overflows,
            .num_int_sub_proven_cannot_overflow,
            .num_int_mul_wrap,
            .num_int_mul_crash_on_overflow,
            .num_int_mul_overflows,
            .num_int_mul_proven_cannot_overflow,
            => try self.modelFamilyArith(stmt, s, args, arg_count),
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
                            // A signed operand can be negative, where the
                            // result exceeds it; only unsigned operands bound
                            // the result.
                            if (trackedIntMax(self.localLayout(GuardedList.at(args, i))) == null) continue;
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
            .str_from_utf8_validated,
            .str_from_utf16_le_short,
            .str_from_utf16_be_short,
            .str_from_utf32_le_short,
            .str_from_utf32_be_short,
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
            .list_concat,
            .list_append_range_within,
            .list_copy_range_within,
            .list_append_range_within_unsafe,
            .list_append_sublist,
            .list_append_le_bytes,
            .list_slack_unique,
            .list_owned_unique,
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
            .list_sort_with,
            .list_release_excess_capacity,
            .list_split_first,
            .list_split_last,
            .list_map_can_reuse,
            .list_map_extract_unsafe,
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
            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_float_add,
            .num_float_sub,
            .num_float_mul,
            .dec_mul,
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
            .num_atan2,
            .num_sqrt,
            .num_sin,
            .num_cos,
            .num_tan,
            .num_asin,
            .num_acos,
            .num_atan,
            .num_log,
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
            .u8_from_str_prefix,
            .u8_from_utf8_prefix,
            .i8_from_str_prefix,
            .i8_from_utf8_prefix,
            .u16_from_str_prefix,
            .u16_from_utf8_prefix,
            .i16_from_str_prefix,
            .i16_from_utf8_prefix,
            .u32_from_str_prefix,
            .u32_from_utf8_prefix,
            .i32_from_str_prefix,
            .i32_from_utf8_prefix,
            .u64_from_str_prefix,
            .u64_from_utf8_prefix,
            .i64_from_str_prefix,
            .i64_from_utf8_prefix,
            .u128_from_str_prefix,
            .u128_from_utf8_prefix,
            .i128_from_str_prefix,
            .i128_from_utf8_prefix,
            .dec_from_str_prefix,
            .dec_from_utf8_prefix,
            .f32_from_str_prefix,
            .f32_from_utf8_prefix,
            .f64_from_str_prefix,
            .f64_from_utf8_prefix,
            .u8_to_i8_wrap,
            .u8_to_i8_try,
            .u8_to_i128,
            .u8_to_u128,
            .u8_to_i16,
            .u8_to_i32,
            .u8_to_i64,
            .u16_to_i32,
            .u16_to_i64,
            .u32_to_i64,
            .i8_to_i16,
            .i8_to_i32,
            .i8_to_i64,
            .i16_to_i32,
            .i16_to_i64,
            .i32_to_i64,
            .u8_to_f32,
            .u8_to_f64,
            .u8_to_dec,
            .i8_to_i128,
            .i8_to_u8_try,
            .i8_to_u16_try,
            .i8_to_u32_try,
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
            .u16_to_i128,
            .u16_to_u8_try,
            .u16_to_u128,
            .u16_to_f32,
            .u16_to_f64,
            .u16_to_dec,
            .i16_to_i8_wrap,
            .i16_to_i8_try,
            .i16_to_i128,
            .i16_to_u8_try,
            .i16_to_u16_try,
            .i16_to_u32_try,
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
            .u32_to_i128,
            .u32_to_u8_try,
            .u32_to_u16_try,
            .u32_to_u128,
            .u32_to_f32,
            .u32_to_f64,
            .u32_to_dec,
            .i32_to_i8_wrap,
            .i32_to_i8_try,
            .i32_to_i16_wrap,
            .i32_to_i16_try,
            .i32_to_i128,
            .i32_to_u8_try,
            .i32_to_u16_try,
            .i32_to_u32_try,
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
            .u64_to_u8_try,
            .u64_to_u16_try,
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
            .i64_to_u8_try,
            .i64_to_u16_try,
            .i64_to_u32_try,
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
            .u128_to_u8_try,
            .u128_to_u16_try,
            .u128_to_u32_try,
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
            .box_unbox_borrowed,
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
            .num_plus, .num_minus, .num_times => unreachable,
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
        else if (s.op == .num_is_eq)
            .eq
        else
            unreachable;

        // `a < b` holds when `a <= b - 1`; it fails when `b <= a`. The
        // remaining kinds reduce to those two shapes.
        self.proof_assumed = 0;
        const holds = switch (op) {
            .lt => try self.proveLe(a, b, -1),
            .lte => try self.proveLe(a, b, 0),
            .gt => try self.proveLe(b, a, -1),
            .gte => try self.proveLe(b, a, 0),
            .eq => try self.proveLe(a, b, 0) and try self.proveLe(b, a, 0),
            .ne => try self.proveLe(a, b, -1) or try self.proveLe(b, a, -1),
        };
        const fails = if (holds) false else switch (op) {
            .lt => try self.proveLe(b, a, 0),
            .lte => try self.proveLe(b, a, -1),
            .gt => try self.proveLe(a, b, 0),
            .gte => try self.proveLe(a, b, -1),
            .eq => try self.proveLe(a, b, -1) or try self.proveLe(b, a, -1),
            .ne => try self.proveLe(a, b, 0) and try self.proveLe(b, a, 0),
        };

        if ((holds or fails) and self.proof_assumed != 0) {
            // The proof rests on an unverified assumption; defer the fold
            // and model the compare as undecided this round.
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

    const ArithmeticProof = struct {
        proven: bool = false,
        result: ?NodeId = null,
    };

    fn modelFamilyArith(self: *Pass, stmt: CFStmtId, s: anytype, args: anytype, arg_count: usize) ResourceError!void {
        const entry = CheckedArithmetic.classify(s.op) orelse unreachable;
        const operand_layout = if (arg_count > 0) self.localLayout(GuardedList.at(args, 0)) else self.localLayout(s.target);
        const max = trackedIntMax(operand_layout);
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

        if (try self.foldSameSignConstantChain(stmt, s, args, entry, lhs, rhs, operand_layout, max.?)) return;

        self.proof_assumed = 0;
        var proof = try self.proveFamilyNoOverflow(entry.operation, lhs, rhs, operand_layout);
        if (!proof.proven) {
            if (self.pathNoOverflowFact(entry.operation, lhs, rhs, operand_layout)) |fact| {
                if (builtin.mode == .Debug) self.last_claim = .{ .no_overflow = fact };
                proof = .{
                    .proven = true,
                    .result = try self.survivingFamilyResult(entry.operation, lhs, rhs),
                };
            }
        }
        const always_overflows = self.familyAlwaysOverflows(entry.operation, lhs, rhs, max.?);

        if (entry.mode == .crash_on_overflow and always_overflows) {
            try self.rewriteAsArithmeticCrash(stmt, entry.operation);
            return;
        }

        if (entry.mode == .overflows) {
            if ((proof.proven or always_overflows) and self.proof_assumed != 0) {
                self.deferred_rewrites = true;
            } else if (proof.proven or always_overflows) {
                const truth: u16 = @intFromBool(always_overflows);
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

            const bool_node = (try self.freshRoot(0, 1)) orelse return self.bindFresh(s.target);
            try self.bind(s.target, .{ .node = bool_node, .overflow_pred = .{
                .operation = entry.operation,
                .lhs = lhs,
                .rhs = rhs,
                .operand_layout = operand_layout,
                .predicate_stmt = stmt,
            } });
            return;
        }

        if (proof.proven and entry.mode != .proven_cannot_overflow) {
            if (self.proof_assumed != 0) {
                self.deferred_rewrites = true;
            } else if (CheckedArithmetic.provenForm(s.op)) |proven| {
                try self.recordProof(stmt);
                const ptr = &self.store.getCFStmtPtr(stmt).assign_low_level;
                ptr.op = proven;
                ptr.rc_effect = proven.rcEffect();
                self.rewrites += 1;
            }
        }

        var result = proof.result;
        if (result == null and entry.mode == .crash_on_overflow) {
            // Continuing past a surviving checked operation proves its result
            // exact even if its input ranges did not prove safety beforehand.
            result = try self.survivingFamilyResult(entry.operation, lhs, rhs);
        }
        const result_node = result orelse (try self.unknownFor(self.localLayout(s.target)) orelse return);
        try self.bind(s.target, .{
            .node = result_node,
            .arithmetic_chain = if (entry.mode == .crash_on_overflow and !proof.proven)
                self.constantArithmeticChain(stmt, entry.operation, args, lhs, rhs)
            else
                null,
        });
    }

    fn constantArithmeticChain(
        self: *const Pass,
        stmt: CFStmtId,
        operation: CheckedArithmetic.Operation,
        args: anytype,
        lhs: NodeId,
        rhs: NodeId,
    ) ?ArithmeticChain {
        return switch (operation) {
            .add => if (self.constValueOf(rhs)) |constant|
                .{ .operation = .add, .base = GuardedList.at(args, 0), .constant = constant, .stmt = stmt }
            else if (self.constValueOf(lhs)) |constant|
                .{ .operation = .add, .base = GuardedList.at(args, 1), .constant = constant, .stmt = stmt }
            else
                null,
            .sub => if (self.constValueOf(rhs)) |constant|
                .{ .operation = .sub, .base = GuardedList.at(args, 0), .constant = constant, .stmt = stmt }
            else
                null,
            .mul => null,
        };
    }

    fn foldSameSignConstantChain(
        self: *Pass,
        stmt: CFStmtId,
        s: anytype,
        args: anytype,
        entry: CheckedArithmetic.FamilyEntry,
        lhs: NodeId,
        rhs: NodeId,
        operand_layout: layout_mod.Idx,
        max: i128,
    ) ResourceError!bool {
        if (entry.mode != .crash_on_overflow or (entry.operation != .add and entry.operation != .sub)) return false;

        var inner_local: LocalId = undefined;
        var outer_constant_local: LocalId = undefined;
        var outer_constant: i128 = undefined;
        if (entry.operation == .add) {
            if (self.constValueOf(rhs)) |constant| {
                inner_local = GuardedList.at(args, 0);
                outer_constant_local = GuardedList.at(args, 1);
                outer_constant = constant;
            } else if (self.constValueOf(lhs)) |constant| {
                inner_local = GuardedList.at(args, 1);
                outer_constant_local = GuardedList.at(args, 0);
                outer_constant = constant;
            } else return false;
        } else {
            outer_constant = self.constValueOf(rhs) orelse return false;
            inner_local = GuardedList.at(args, 0);
            outer_constant_local = GuardedList.at(args, 1);
        }
        if (outer_constant < 0 or outer_constant > max) return false;

        const chain = (self.lookup(inner_local) orelse return false).arithmetic_chain orelse return false;
        if (chain.operation != entry.operation or chain.constant < 0 or chain.constant > max) return false;

        const inner_stmt = self.store.getCFStmt(chain.stmt);
        if (std.meta.activeTag(inner_stmt) != .assign_low_level) return false;
        const inner = inner_stmt.assign_low_level;
        if (inner.target != inner_local) return false;
        const outer_literal_stmt = self.literalDefinitionOnPath(inner.next, stmt, outer_constant_local) orelse return false;
        const read_counts = self.read_counts orelse return false;
        if (read_counts.get(inner_local) != 1 or read_counts.get(outer_constant_local) != 1) return false;
        const inner_entry = CheckedArithmetic.classify(inner.op) orelse return false;
        if (inner_entry.operation != entry.operation or inner_entry.mode != .crash_on_overflow) return false;

        const wrapping_op = CheckedArithmetic.member(entry.operation, .wrap);
        self.store.getCFStmtPtr(chain.stmt).assign_low_level.op = wrapping_op;
        self.store.getCFStmtPtr(chain.stmt).assign_low_level.rc_effect = wrapping_op.rcEffect();

        if (outer_constant > max - chain.constant) {
            try self.rewriteAsArithmeticCrash(stmt, entry.operation);
            return true;
        }
        const combined_constant = chain.constant + outer_constant;
        self.store.getCFStmtPtr(outer_literal_stmt).assign_literal.value = .{
            .i128_literal = .{ .value = combined_constant, .layout_idx = operand_layout },
        };
        const combined_args = try self.store.addLocalSpan(&.{ chain.base, outer_constant_local });
        self.store.getCFStmtPtr(stmt).assign_low_level.args = combined_args;
        self.rewrites += 1;

        const result = (try self.survivingFamilyResult(entry.operation, lhs, rhs)) orelse
            (try self.unknownFor(self.localLayout(s.target)) orelse return true);
        try self.bind(s.target, .{ .node = result, .arithmetic_chain = .{
            .operation = entry.operation,
            .base = chain.base,
            .constant = combined_constant,
            .stmt = stmt,
        } });
        return true;
    }

    fn literalDefinitionOnPath(self: *const Pass, start: CFStmtId, target: CFStmtId, local: LocalId) ?CFStmtId {
        var current = start;
        var remaining = self.store.cfStmtCount();
        var definition: ?CFStmtId = null;
        while (remaining > 0) : (remaining -= 1) {
            if (current == target) return definition;
            const stmt = self.store.getCFStmt(current);
            if (std.meta.activeTag(stmt) != .assign_literal) return null;
            const literal = stmt.assign_literal;
            if (literal.target == local) definition = current;
            current = literal.next;
        }
        return null;
    }

    fn proveFamilyNoOverflow(
        self: *Pass,
        operation: CheckedArithmetic.Operation,
        lhs: NodeId,
        rhs: NodeId,
        operand_layout: layout_mod.Idx,
    ) ResourceError!ArithmeticProof {
        if (builtin.mode == .Debug) self.last_claim = null;
        const max = trackedIntMax(operand_layout) orelse return .{};
        const lhs_lo = self.absLoOf(lhs);
        const lhs_hi = self.absHiOf(lhs);
        const rhs_lo = self.absLoOf(rhs);
        const rhs_hi = self.absHiOf(rhs);

        switch (operation) {
            .add => {
                // This numeric proof handles two dynamic unsigned operands
                // without allocating a symbolic limit node.
                if (lhs_lo >= 0 and rhs_lo >= 0 and rhs_hi <= max and lhs_hi <= max - rhs_hi) {
                    const result = if (self.constValueOf(rhs)) |c|
                        try self.derived(lhs, c)
                    else if (self.constValueOf(lhs)) |c|
                        try self.derived(rhs, c)
                    else
                        (try self.sumNode(lhs, rhs)) orelse try self.freshRoot(lhs_lo + rhs_lo, lhs_hi + rhs_hi);
                    return .{ .proven = result != null, .result = result };
                }

                if (self.constValueOf(rhs)) |c| {
                    if (c >= 0 and c <= max) {
                        if (try self.constNode(max - c)) |limit| {
                            if (try self.proveLe(lhs, limit, 0)) return .{ .proven = true, .result = try self.derived(lhs, c) };
                        }
                    }
                } else if (self.constValueOf(lhs)) |c| {
                    if (c >= 0 and c <= max) {
                        if (try self.constNode(max - c)) |limit| {
                            if (try self.proveLe(rhs, limit, 0)) return .{ .proven = true, .result = try self.derived(rhs, c) };
                        }
                    }
                } else if (lhs_lo >= 0 and rhs_lo >= 0) {
                    // Two dynamic operands: the exact sum fits when the facts
                    // bound it, typically through another sum of a shared
                    // operand that a guard already placed below a length.
                    if (try self.sumNode(lhs, rhs)) |sum| {
                        if (try self.constNode(max)) |limit| {
                            if (try self.proveLe(sum, limit, 0)) return .{ .proven = true, .result = sum };
                        }
                    }
                }
            },
            .sub => {
                if (lhs_lo >= rhs_hi or try self.proveLe(rhs, lhs, 0)) {
                    const result = if (rhs_lo >= 0) try self.derivedRange(lhs, -rhs_hi, -rhs_lo) else null;
                    return .{ .proven = result != null, .result = result };
                }
            },
            .mul => {
                const result = try self.mulConstExactNodes(lhs, rhs, operand_layout);
                return .{ .proven = result != null, .result = result };
            },
        }
        return .{};
    }

    fn pathNoOverflowFact(
        self: *const Pass,
        operation: CheckedArithmetic.Operation,
        lhs: NodeId,
        rhs: NodeId,
        operand_layout: layout_mod.Idx,
    ) ?NoOverflowFact {
        var i = self.no_overflow_facts.items.len;
        while (i > 0) {
            i -= 1;
            const fact = self.no_overflow_facts.items[i];
            const predicate = fact.predicate;
            if (predicate.operation != operation or predicate.operand_layout != operand_layout) continue;
            if (predicate.lhs == lhs and predicate.rhs == rhs) return fact;
            if ((operation == .add or operation == .mul) and predicate.lhs == rhs and predicate.rhs == lhs) return fact;
        }
        return null;
    }

    fn survivingFamilyResult(self: *Pass, operation: CheckedArithmetic.Operation, lhs: NodeId, rhs: NodeId) ResourceError!?NodeId {
        return switch (operation) {
            .add => if (self.constValueOf(rhs)) |c|
                try self.derived(lhs, c)
            else if (self.constValueOf(lhs)) |c|
                try self.derived(rhs, c)
            else
                try self.sumNode(lhs, rhs),
            .sub => blk: {
                const rhs_lo = self.absLoOf(rhs);
                const rhs_hi = self.absHiOf(rhs);
                break :blk if (rhs_lo >= 0) try self.derivedRange(lhs, -rhs_hi, -rhs_lo) else null;
            },
            .mul => null,
        };
    }

    fn familyAlwaysOverflows(self: *const Pass, operation: CheckedArithmetic.Operation, lhs: NodeId, rhs: NodeId, max: i128) bool {
        const a = self.constValueOf(lhs) orelse return false;
        const b = self.constValueOf(rhs) orelse return false;
        return switch (operation) {
            .add => b > max or a > max - b,
            .sub => a < b,
            .mul => b != 0 and a > @divTrunc(max, b),
        };
    }

    fn rewriteAsArithmeticCrash(self: *Pass, stmt: CFStmtId, operation: CheckedArithmetic.Operation) ResourceError!void {
        const op = CheckedArithmetic.member(operation, .crash_on_overflow);
        const message = CheckedArithmetic.overflowMessage(op) orelse unreachable;
        self.store.getCFStmtPtr(stmt).* = .{ .crash = .{
            .msg = .{ .literal = try self.store.insertString(message) },
        } };
        self.rewrites += 1;
    }

    fn mulConstExactNodes(self: *Pass, lhs: NodeId, rhs: NodeId, target_layout: layout_mod.Idx) ResourceError!?NodeId {
        const max = trackedIntMax(target_layout) orelse return null;
        var factor: i128 = undefined;
        var operand: NodeId = undefined;
        if (self.constValueOf(rhs)) |c| {
            factor = c;
            operand = lhs;
        } else if (self.constValueOf(lhs)) |c| {
            factor = c;
            operand = rhs;
        } else return null;
        if (factor < 0) return null;
        const lo = self.absLoOf(operand);
        const hi = self.absHiOf(operand);
        if (lo < 0) return null;
        if (factor != 0 and hi > @divTrunc(max, factor)) return null;
        return try self.freshRoot(lo * factor, hi * factor);
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
                .assign_boxy_record_update => |t| try list.append(allocator, t.next),
                .assign_boxy_reuse_box => |t| try list.append(allocator, t.next),
                .assign_boxy_unbox => |t| try list.append(allocator, t.next),
                .assign_boxy_adapt => |t| try list.append(allocator, t.next),
                .assign_boxy_inspect => |t| try list.append(allocator, t.next),
                .assign_boxy_eq => |t| try list.append(allocator, t.next),
                .assign_boxy_hash => |t| try list.append(allocator, t.next),
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

    /// Verify that the recorded edge is the False arm of the recorded
    /// overflow predicate's Bool switch.
    fn isFalseOverflowEdge(store: *const LirStore, fact: NoOverflowFact) bool {
        const predicate_stmt = store.getCFStmt(fact.predicate.predicate_stmt);
        if (std.meta.activeTag(predicate_stmt) != .assign_low_level) return false;
        const predicate = predicate_stmt.assign_low_level;
        const entry = CheckedArithmetic.classify(predicate.op) orelse return false;
        if (entry.mode != .overflows or entry.operation != fact.predicate.operation) return false;

        const switch_stmt = store.getCFStmt(fact.switch_stmt);
        if (std.meta.activeTag(switch_stmt) != .switch_stmt) return false;
        const bool_switch = switch_stmt.switch_stmt;
        if (bool_switch.cond != predicate.target) return false;

        const branches = store.getCFSwitchBranches(bool_switch.branches);
        const branch_count = GuardedList.borrowLen(branches);
        for (0..branch_count) |index| {
            const branch = GuardedList.at(branches, index);
            if (branch.value == 0 and branch.body == fact.edge_head) return true;
        }
        return branch_count == 1 and
            GuardedList.at(branches, 0).value == 1 and
            bool_switch.default_branch == fact.edge_head;
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
