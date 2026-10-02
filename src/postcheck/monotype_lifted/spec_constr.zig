//! Make calls cheaper when they pass known-shaped values to code that
//! immediately takes those values apart.
//!
//! The most obvious case is a freshly created tag union value that immediately
//! gets pattern-matched. The same idea also applies to records and tuples whose
//! fields are read right away, and to `Stream` values that carry a known step
//! function after inlining. This shows up in recursive helpers, `Iter`/`Stream`
//! pipelines, and loops that appear after inlining. This pass turns those calls
//! into calls to workers that take the useful pieces directly.
//!
//! Here is the smallest version of the idea:
//!
//! ```roc
//! Start : { n : I64 }
//! SumState : { n : I64, acc : I64 }
//!
//! sum : Start -> I64
//! sum = |start| {
//!     var $state = { n: start.n, acc: 0 }
//!
//!     while $state.n != 0 {
//!         $state = { n: $state.n - 1, acc: $state.acc + $state.n }
//!     }
//!
//!     $state.acc
//! }
//!
//! main = sum({ n: 4 })
//! ```
//!
//! The call to `sum` passes a known `Start` record, and the loop state is always
//! a `SumState`. The function reads `start.n`, then the loop immediately reads
//! `$state.n` and `$state.acc`. This pass rewrites the call and loop so they
//! carry the useful fields directly:
//!
//! ```roc
//! sum_worker : I64 -> I64
//! sum_worker = |start_n| {
//!     var $n = start_n
//!     var $acc = 0
//!
//!     while $n != 0 {
//!         $acc = $acc + $n
//!         $n = $n - 1
//!     }
//!
//!     $acc
//! }
//!
//! main = sum_worker(4)
//! ```
//!
//! That is faster for plain, practical reasons:
//!
//! - each loop iteration carries two `I64`s directly;
//! - the loop uses `n` and `acc` directly instead of reading record fields;
//! - later compiler stages have simple values to keep in registers.
//!
//! This is Roc's version of the optimization described in
//! "Call-pattern Specialisation for Haskell Programs" by Simon Peyton Jones:
//!
//! https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/spec-constr.pdf
//!
//! The important Roc case is collection from `Iter` and `Stream`. Source code is
//! compact:
//!
//! ```roc
//! Plant : { seed : I64 }
//!
//! random_plant! : I64 => Plant
//! random_plant! = |seed| { seed }
//!
//! starting_plants! : () => List(Plant)
//! starting_plants! = || {
//!     (0.I64..=15)
//!         .iter()
//!         .stream()
//!         .map(|i| random_plant!(i * 12))
//!         .collect!()
//! }
//! ```
//!
//! After wrapper inlining exposes the `Stream` operations, the lifted program has
//! the same shape as this Roc code. The range is wrapped in a stream record; map
//! wraps that stream in another stream record; collect loops over that mapped
//! stream by calling the carried step thunk:
//!
//! ```roc
//! starting_plants! = || {
//!     range_iter = (0.I64..=15).iter()
//!
//!     source_stream = {
//!         len_if_known: Known(16),
//!         step!: ||
//!             match Iter.next(range_iter) {
//!                 Done => Done
//!                 Skip({ rest }) =>
//!                     Skip({ rest: Stream.from_iter(rest) })
//!                 One({ item, rest }) =>
//!                     One({ item, rest: Stream.from_iter(rest) })
//!             },
//!     }
//!
//!     mapped_stream = {
//!         len_if_known: source_stream.len_if_known,
//!         step!: ||
//!             match source_stream.step!() {
//!                 Done => Done
//!                 Skip({ rest }) =>
//!                     Skip({ rest: Stream.map(rest, |i| random_plant!(i * 12)) })
//!                 One({ item, rest }) =>
//!                     One({
//!                         item: random_plant!(item * 12),
//!                         rest: Stream.map(rest, |i| random_plant!(i * 12)),
//!                     })
//!             },
//!     }
//!
//!     cap = match mapped_stream.len_if_known {
//!         Known(n) => n
//!         Unknown => 0
//!     }
//!
//!     var $list = List.with_capacity(cap)
//!     var $rest = mapped_stream
//!
//!     while Bool.True {
//!         match $rest.step!() {
//!             Done => break
//!             Skip({ rest }) => {
//!                 $rest = rest
//!             }
//!             One({ item, rest }) => {
//!                 $list = list_append_unsafe($list, item)
//!                 $rest = rest
//!             }
//!         }
//!     }
//!
//!     $list
//! }
//! ```
//!
//! In that inlined form, the loop state `$rest` has a known constructor shape:
//! it is a `Stream` record whose `step!` field is the lifted function created by
//! `Stream.map`, with captures for the source step thunk and the mapping
//! function. Each `One` or `Skip` branch constructs the same mapped stream shape
//! for the next iteration. Without this pass, the compiler lowers that as a loop
//! over a single stream value, repacking stream fields and building the step
//! closure before immediately reading them again.
//!
//! This pass specializes the collect worker for the known stream shape. Written
//! in pure Roc terms, the optimized shape is:
//!
//! ```roc
//! starting_plants! = || {
//!     var $list = List.with_capacity(16)
//!     var $current = 0.I64
//!     var $last = 15.I64
//!
//!     while Bool.True {
//!         if $current > $last {
//!             break
//!         }
//!
//!         item = random_plant!($current * 12)
//!         $list = list_append_unsafe($list, item)
//!         $current = $current + 1
//!     }
//!
//!     $list
//! }
//! ```
//!
//! The real lifted IR is more explicit than that source sketch: lambdas have
//! function ids, captures are separate locals, and branches still have explicit
//! tags until later lowering. The essential change is that the reachable collect
//! worker no longer receives one `Stream(Plant)` argument. It receives the
//! stream's known fields and callable captures directly, and recursive loop
//! updates pass those fields forward instead of re-forming a stream value.
//!
//! The implementation has five parts:
//!
//! 1. Scan original lifted functions and mark argument positions read by
//!    `match`, field access, or tuple access. Direct calls propagate those marks
//!    to the caller's corresponding arguments.
//! 2. Record call patterns at direct calls. If a marked argument is an explicit
//!    `tag`, `record`, `tuple`, `nominal`, or lifted callable value, that
//!    constructor shape becomes part of the pattern.
//! 3. Reserve worker ids for the recorded patterns, then clone each source
//!    function into its workers. Constructor-shaped arguments are split into
//!    their leaves; ordinary arguments stay as normal worker arguments.
//! 4. Clone with a value environment. Known records simplify field reads, known
//!    tuples simplify tuple reads, known tags simplify matches, known callable
//!    values inline direct calls, and calls matching a recorded pattern are
//!    redirected to the worker.
//! 5. Specialize loop state in the cloned body. If a loop starts with a
//!    constructor-shaped state value, its loop parameters are split the same way
//!    function arguments are split, and `continue` values must pass the same
//!    shape's leaves.
//!
//! Callable identity is part of a call pattern. A lifted callable matches only
//! the same function id, or a specialized clone whose stored source function
//! template is the same. That keeps dispatch static while allowing this pass's
//! own callable workers to match the patterns that created them.
//!
//! Store-borrow discipline: this pass clones expressions while walking spans of
//! the same `Program` store, and cloning appends new nodes to those stores.
//! Never hold a `Program`-store borrow (any `*Span` result) across a call that
//! can append to the same store: copy the span first via `GuardedList.dupe`, or
//! read one element at a time by stable index via the `*At` accessors
//! (`branchAt`, `captureOperandAt`), which retain no borrow. The GuardedList
//! generation guard turns a violation into a Debug panic. The generation is
//! per-list, so a borrow of one store stays valid across an append to a
//! different store—only same-store appends invalidate it, so copying a span
//! whose store this walk never grows is unnecessary.

const std = @import("std");
const builtin = @import("builtin");
const TypeDigestHasher = @import("base").TypeDigestHasher;
const collections = @import("collections");

const SourceLoc = @import("base").SourceLoc;
const Region = @import("base").Region;
const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Lift = @import("lift.zig");
const Mono = @import("../monotype/ast.zig");
const Type = @import("../monotype/type.zig");
const check = @import("check");
const names = @import("check").CheckedNames;

const ExitDemand = @import("loop_exit_demand.zig");
const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;
const TaskExecutor = @import("base").post_check_task_executor;

/// Identity exhaustion is a resource failure, checked before committing shard rows.
fn checkedIdentityTotal(start: u32, count: u32) Allocator.Error!u32 {
    return std.math.add(u32, start, count) catch error.OutOfMemory;
}

/// Whether a checker-stamped compiler procedure constructs an iterator value.
/// The stamp is exact producer data; result type, callee spelling, and call
/// shape are deliberately irrelevant here.
fn isIteratorProducer(procedure: ?check.StaticDispatchRegistry.IteratorProcedureId) bool {
    return if (procedure) |exact| exact.producesIteratorValue() else false;
}

fn isExactGeneratedIteratorType(program: *const Ast.Program, ty: Type.TypeId) bool {
    const type_ = program.types.get(ty);
    if (type_ != .named) return false;
    const named = type_.named;
    if (named.def.iterator_topology == null) return false;
    const backing = named.backing orelse return false;
    return backing.authority == .generated_private;
}

fn isForcedDynamicIteratorType(program: *const Ast.Program, ty: Type.TypeId) bool {
    const type_ = program.types.get(ty);
    return type_ == .named and type_.named.def.iterator_representation == .forced_dynamic;
}

/// Exact post-SpecConstr use information for one lifted procedure.
pub const ProcedureUse = struct {
    external_calls: usize = 0,
    external_call_expr: ?Ast.ExprId = null,
    external_call_owner: ?Ast.FnId = null,
    value_refs: usize = 0,
    contains_return: bool = false,
};

/// Read-only exact procedure-use inventory produced after all SpecConstr graph
/// rewrites. Function ids index `items` directly.
pub const ProcedureUsage = struct {
    items: []const ProcedureUse = &.{},

    pub fn get(self: ProcedureUsage, fn_id: Ast.FnId) ProcedureUse {
        const index = @intFromEnum(fn_id);
        if (index >= self.items.len) {
            Common.invariant("procedure-use inventory did not contain a lifted function");
        }
        return self.items[index];
    }
};

/// Allocator-owned storage for an exact post-SpecConstr procedure-use inventory.
pub const OwnedProcedureUsage = struct {
    allocator: Allocator,
    items: []ProcedureUse,

    pub fn empty(allocator: Allocator) OwnedProcedureUsage {
        return .{ .allocator = allocator, .items = &.{} };
    }

    pub fn deinit(self: *OwnedProcedureUsage) void {
        if (self.items.len != 0) self.allocator.free(self.items);
        self.* = empty(self.allocator);
    }

    pub fn view(self: *const OwnedProcedureUsage) ProcedureUsage {
        return .{ .items = self.items };
    }
};

/// How this pass's value-aware clones inline direct calls, both when
/// rewriting original bodies in place and when writing specialization
/// workers.
///
/// `.all_calls` lets a clone inline any admissible direct call while it
/// chases known values, so general call-pattern specialization simplifies
/// across arbitrary procedure boundaries. That chase is the expensive part of
/// this pass: its cost grows with how much known-value structure flows through
/// a module's call graph, independent of the emitted-code budgets, and a
/// worker written this way can flatten a callee's whole call tree into its
/// body.
///
/// `.iterator_fusion` restricts a clone's inlining to checker-stamped
/// iterator producers and calls already inside an iterator context: the
/// subset that collapses `Iter` pipelines into scalar loops. Argument-level
/// specialization still runs in full; only the value chase narrows. Dev
/// builds select this so their post-check time and emitted program size stay
/// proportional to the iterator code the pass improves.
pub const CloneInlining = enum { all_calls, iterator_fusion };

/// Independent work separated by coordinator-owned graph mutation barriers.
pub const Phase = enum { discovery, unused_loop_results, iterator_fusion };

/// Callback counters count only executor submissions. Work totals also include
/// inline execution, which uses exactly the same private-shard boundary.
pub const ParallelMetrics = struct {
    tasks_submitted: u64 = 0,
    tasks_committed: u64 = 0,
    patterns_recorded: u64 = 0,
    patterns_admitted: u64 = 0,
    bodies_committed: u64 = 0,
    expressions_committed: u64 = 0,
    peak_retained_shards: u64 = 0,
    committed_by_phase: [3]u64 = .{ 0, 0, 0 },
    changed_by_phase: [3]u64 = .{ 0, 0, 0 },

    /// Accumulate independent pass totals without wrapping long-lived counters.
    pub fn add(self: *ParallelMetrics, other: ParallelMetrics) void {
        self.tasks_submitted +|= other.tasks_submitted;
        self.tasks_committed +|= other.tasks_committed;
        self.patterns_recorded +|= other.patterns_recorded;
        self.patterns_admitted +|= other.patterns_admitted;
        self.bodies_committed +|= other.bodies_committed;
        self.expressions_committed +|= other.expressions_committed;
        self.peak_retained_shards = @max(self.peak_retained_shards, other.peak_retained_shards);
        for (&self.committed_by_phase, other.committed_by_phase) |*total, count| total.* +|= count;
        for (&self.changed_by_phase, other.changed_by_phase) |*total, count| total.* +|= count;
    }
};

/// Scheduling and observation only; neither changes optimization decisions.
pub const Options = struct {
    executor: ?TaskExecutor.Executor = null,
    metrics_out: ?*ParallelMetrics = null,
};

/// Specialize recursive direct calls whose arguments are known constructor shapes.
pub fn run(allocator: Allocator, program: *Ast.Program, clone_inlining: CloneInlining) Common.LowerError!void {
    var procedure_usage = try runAndCollectProcedureUsage(allocator, program, clone_inlining);
    defer procedure_usage.deinit();
}

/// Run SpecConstr and retain the exact use inventory from its final graph.
pub fn runAndCollectProcedureUsage(allocator: Allocator, program: *Ast.Program, clone_inlining: CloneInlining) Common.LowerError!OwnedProcedureUsage {
    return runAndCollectProcedureUsageWithOptions(allocator, program, clone_inlining, .{});
}

/// Run with optional independent-body scheduling and retain final graph usage.
pub fn runAndCollectProcedureUsageWithOptions(allocator: Allocator, program: *Ast.Program, clone_inlining: CloneInlining, options: Options) Common.LowerError!OwnedProcedureUsage {
    if (options.metrics_out) |metrics| metrics.* = .{};
    var pass = try Pass.init(allocator, program);
    defer pass.deinit();
    pass.options = options;
    pass.clone_inlining = clone_inlining;
    return try pass.run();
}

/// Normalize constructor and callable shape only along checker-stamped iterator
/// producer pipelines. This is the `.none`-mode subset of SpecConstr: it emits
/// no call-pattern workers and does not admit unrelated calls for inlining.
pub fn runIteratorFusion(allocator: Allocator, program: *Ast.Program) Common.LowerError!void {
    return runIteratorFusionWithOptions(allocator, program, .{});
}

/// Normalize checker-stamped iterator pipelines with optional body scheduling.
pub fn runIteratorFusionWithOptions(allocator: Allocator, program: *Ast.Program, options: Options) Common.LowerError!void {
    if (options.metrics_out) |metrics| metrics.* = .{};
    var pass = try Pass.init(allocator, program);
    defer pass.deinit();
    pass.options = options;
    try pass.runIteratorFusion();
}

const Shape = union(enum) {
    any: Type.TypeId,
    tag: TagShape,
    record: RecordShape,
    tuple: TupleShape,
    nominal: NominalShape,
    callable: CallableShape,
};

const TagShape = struct {
    ty: Type.TypeId,
    name: names.TagNameId,
    payloads: []const Shape,
};

const FieldShape = struct {
    name: names.RecordFieldNameId,
    shape: Shape,
};

const RecordShape = struct {
    ty: Type.TypeId,
    fields: []const FieldShape,
};

const TupleShape = struct {
    ty: Type.TypeId,
    items: []const Shape,
};

const NominalShape = struct {
    ty: Type.TypeId,
    backing: *const Shape,
};

const CallableShape = struct {
    ty: Type.TypeId,
    fn_id: Ast.FnId,
    captures: []const Shape,
};

/// Requests retain frozen type, name, and function identities in owned shape
/// trees, never worker-generated AST identities.
/// A shape still to copy and where its copy goes.
const ShapeCopy = struct { source: Shape, target: *Shape };

fn copyShape(allocator: Allocator, shape: Shape) Allocator.Error!Shape {
    var result: Shape = undefined;
    var pending = std.ArrayList(ShapeCopy).empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, .{ .source = shape, .target = &result });
    try drainShapeCopies(allocator, &pending);
    return result;
}

fn copyShapes(allocator: Allocator, shapes: []const Shape) Allocator.Error![]const Shape {
    var pending = std.ArrayList(ShapeCopy).empty;
    defer pending.deinit(allocator);
    const copied = try copyShapeSlots(allocator, shapes, &pending);
    try drainShapeCopies(allocator, &pending);
    return copied;
}

/// Copy every pending shape into its target. Shapes nest as deeply as the
/// constructors they describe, so components are copied from a worklist.
fn drainShapeCopies(allocator: Allocator, pending: *std.ArrayList(ShapeCopy)) Allocator.Error!void {
    while (pending.pop()) |item| {
        item.target.* = switch (item.source) {
            .any => item.source,
            .tag => |tag| .{ .tag = .{ .ty = tag.ty, .name = tag.name, .payloads = try copyShapeSlots(allocator, tag.payloads, pending) } },
            .tuple => |tuple| .{ .tuple = .{ .ty = tuple.ty, .items = try copyShapeSlots(allocator, tuple.items, pending) } },
            .callable => |callable| .{ .callable = .{ .ty = callable.ty, .fn_id = callable.fn_id, .captures = try copyShapeSlots(allocator, callable.captures, pending) } },
            .nominal => |nominal| blk: {
                const backing = try allocator.create(Shape);
                try pending.append(allocator, .{ .source = nominal.backing.*, .target = backing });
                break :blk .{ .nominal = .{ .ty = nominal.ty, .backing = backing } };
            },
            .record => |record| blk: {
                const fields = try allocator.alloc(FieldShape, record.fields.len);
                for (fields, record.fields) |*field, source| {
                    field.name = source.name;
                    try pending.append(allocator, .{ .source = source.shape, .target = &field.shape });
                }
                break :blk .{ .record = .{ .ty = record.ty, .fields = fields } };
            },
        };
    }
}

fn copyShapeSlots(allocator: Allocator, shapes: []const Shape, pending: *std.ArrayList(ShapeCopy)) Allocator.Error![]const Shape {
    const copied = try allocator.alloc(Shape, shapes.len);
    for (copied, shapes) |*copy, shape| try pending.append(allocator, .{ .source = shape, .target = copy });
    return copied;
}

const ShapeProof = union(enum) {
    proven: Shape,
    disproven,
    unknown_budget_exhausted,
};

fn shapeProofIsProven(proof: ShapeProof) bool {
    return switch (proof) {
        .proven => true,
        .disproven, .unknown_budget_exhausted => false,
    };
}

/// Maximum number of `runtime_anchor.structure` / `nominal.backing` /
/// `static_data_candidate.structure` / callable-capture pointer edges any single
/// value-tree strip may follow. A
/// value can reference itself through those edges when a `.local` resolves
/// through the substitution maps to an ancestor of a recursive construction,
/// so a strip that ignored the bound would hang on a cycle. A finite value's
/// pointer-edge chain is far shorter than this cap (known values are bounded to
/// a few thousand nodes by their derivations), so reaching it means the value
/// is cyclic: the static matchers decline conservatively, and the
/// materializing and reading walks—which only ever run on values proven
/// acyclic—treat it as a compiler bug via `Common.invariant`, which is a
/// checked panic only in safety-checked builds. See design.md "Core
/// Principles" on bounded post-check walks.
const value_wrapper_strip_cap: usize = 4096;

const Value = union(enum) {
    expr: Ast.ExprId,
    runtime_anchor: RuntimeAnchorValue,
    static_data_candidate: StaticDataCandidateValue,
    tag: TagValue,
    record: RecordValue,
    tuple: TupleValue,
    nominal: NominalValue,
    callable: CallableValue,
};

/// One exact runtime value paired with finite symbolic structure known for it.
/// Materialization always reuses `runtime`; structural consumers inspect
/// `structure`. Recursive bindings use this dual representation to expose
/// constructor and callable structure without reconstructing the recursive value
/// or letting initializer-private locals escape their scope.
const RuntimeAnchorValue = struct {
    runtime: Ast.ExprId,
    structure: *const Value,
};

/// The closed source expression owns initialization. The symbolic view may be
/// rebound in a caller, but can never replace that expression's initializer.
const StaticDataCandidateValue = struct {
    ty: Type.TypeId,
    static_data: Common.StaticDataId,
    expr: Ast.ExprId,
    structure: *const Value,
};

/// Verdict of statically matching one pattern against a symbolic `Value`.
/// `unknown` means the pattern probes information the pass does not track
/// statically: an opaque `.expr` component, or a pattern form (list,
/// string, numeric literal) with no `Value` representation. An `unknown`
/// branch verdict must abort a match fold—the residual match stays in the
/// output and decides at runtime—whereas `no_match` proves the branch can
/// be skipped.
const MatchVerdict = enum { match, no_match, unknown, unknown_budget_exhausted };

fn mergeMatchUnknown(current: MatchVerdict, child: MatchVerdict) MatchVerdict {
    return switch (child) {
        .match => current,
        .no_match => .no_match,
        .unknown => if (current == .match) .unknown else current,
        .unknown_budget_exhausted => .unknown_budget_exhausted,
    };
}

/// Result of a bounded proof query. Exhaustion is deliberately distinct from
/// disproving the property: callers may decline an optimization for either,
/// but must never cache or propagate exhaustion as `disproven`.
const ProofStatus = enum {
    proven,
    disproven,
    unknown_budget_exhausted,
};

fn proofAnd(lhs: ProofStatus, rhs: ProofStatus) ProofStatus {
    if (lhs == .disproven or rhs == .disproven) return .disproven;
    if (lhs == .unknown_budget_exhausted or rhs == .unknown_budget_exhausted) return .unknown_budget_exhausted;
    return .proven;
}

const TagValue = struct {
    ty: Type.TypeId,
    name: names.TagNameId,
    payloads: []const Value,
};

const FieldValue = struct {
    name: names.RecordFieldNameId,
    value: Value,
};

const RecordValue = struct {
    ty: Type.TypeId,
    fields: []const FieldValue,
};

const TupleValue = struct {
    ty: Type.TypeId,
    items: []const Value,
};

const NominalValue = struct {
    ty: Type.TypeId,
    backing: *const Value,
};

const CaptureValue = struct {
    id: check.CheckedModule.CaptureId,
    value: Value,
};

const CallableValue = struct {
    ty: Type.TypeId,
    fn_id: Ast.FnId,
    captures: []const CaptureValue,
    iterator_step: bool = false,
};

const CallPattern = struct {
    args: []const Shape,
};

const Spec = struct {
    pattern: CallPattern,
    fn_id: ?Ast.FnId = null,
    written: bool = false,
};

const BodySize = union(enum) {
    exact: usize,
    over_limit,

    fn admits(self: BodySize) bool {
        return switch (self) {
            .exact => true,
            .over_limit => false,
        };
    }

    fn exactValue(self: BodySize) ?usize {
        return switch (self) {
            .exact => |value| value,
            .over_limit => null,
        };
    }
};

const FnSource = struct {
    /// The authoritative input body for every clone of this function. The
    /// program's function table becomes an output table once rewriting starts,
    /// so reading a body back from it would make later clones consume output
    /// emitted by earlier clones.
    body: Ast.FnBody,
    size: BodySize,
};

const FnPlan = struct {
    used_args: []bool,
    source: FnSource,
    specs: std.ArrayList(Spec),

    fn deinit(self: *FnPlan, allocator: Allocator) void {
        allocator.free(self.used_args);
        self.specs.deinit(allocator);
    }
};

/// A pattern binder paired with the monomorphic type it was bound at. A single
/// source binder is reused across every monomorphization of its binding, so the
/// binder alone does not identify a value; the type digest completes the
/// identity. This must use the digest of `typeEql`, not stored type identity:
/// one monomorphization may carry distinct checked-node provenance or a
/// transparent alias at equivalent local sites. See `Builder.sameLocalIdentity`
/// in monotype/lower.zig.
const BinderIdentity = struct {
    binder: check.CheckedModule.PatternBinderId,
    digest: names.TypeDigest,
};

const BindingTarget = union(enum) {
    local: Ast.LocalId,
    binder: BinderIdentity,
    alias: BinderIdentity,
};

const BindingChange = struct {
    key: BindingTarget,
    previous: ?Value,
};

const StrictBinding = struct {
    local: Ast.LocalId,
    ty: Type.TypeId,
    value: Ast.ExprId,
};

const PositionedBinding = union(enum) {
    strict: StrictBinding,
    /// An ordered binding with a structured pattern or a recursive anchor.
    /// Keeping a record snapshot in one statement avoids turning its width
    /// into a chain of separate expression bindings.
    statement: Ast.StmtId,
};

const BindingNode = struct {
    binding: PositionedBinding,
    previous: ?*BindingNode = null,
    next: ?*BindingNode = null,
};

const PatternLocalUseProbe = struct {
    allocator: Allocator,
    program: *const Ast.Program,
    expr: Ast.ExprId,
    found: *bool,

    pub fn bindLocal(self: PatternLocalUseProbe, local: Ast.LocalId) Allocator.Error!void {
        if (!self.found.* and try exprReferencesLocal(self.allocator, self.program, self.expr, local)) {
            self.found.* = true;
        }
    }
};

/// A linearly owned, source-ordered chain of strict bindings. Concatenation
/// consumes the appended chain; callers must not retain or reuse an appended
/// chain value. Nodes live in the pass arena, so concatenation is constant time
/// and does not copy bindings.
const BindingChain = struct {
    first: ?*BindingNode = null,
    last: ?*BindingNode = null,

    fn isEmpty(self: BindingChain) bool {
        return self.first == null;
    }

    fn mark(self: BindingChain) ?*BindingNode {
        return self.last;
    }

    fn rewind(self: *BindingChain, saved_last: ?*BindingNode) void {
        if (saved_last) |last| {
            last.next = null;
            self.last = last;
        } else {
            self.first = null;
            self.last = null;
        }
    }

    fn appendBinding(self: *BindingChain, arena: Allocator, binding: StrictBinding) Allocator.Error!void {
        const node = try arena.create(BindingNode);
        node.* = .{ .binding = .{ .strict = binding }, .previous = self.last };
        if (self.last) |last| {
            last.next = node;
        } else {
            self.first = node;
        }
        self.last = node;
    }

    fn appendStatement(self: *BindingChain, arena: Allocator, stmt: Ast.StmtId) Allocator.Error!void {
        const node = try arena.create(BindingNode);
        node.* = .{ .binding = .{ .statement = stmt }, .previous = self.last };
        if (self.last) |last| {
            last.next = node;
        } else {
            self.first = node;
        }
        self.last = node;
    }

    fn appendChain(self: *BindingChain, other: BindingChain) void {
        if (other.first == null) return;
        if (self.last) |last| {
            last.next = other.first;
            other.first.?.previous = last;
        } else {
            self.first = other.first;
        }
        self.last = other.last;
    }

    fn verify(self: BindingChain, program: *const Ast.Program) void {
        if (!std.debug.runtime_safety) return;
        var previous: ?*BindingNode = null;
        var current = self.first;
        while (current) |node| : (current = node.next) {
            std.debug.assert(node.previous == previous);
            switch (node.binding) {
                .strict => |binding| {
                    std.debug.assert(program.getLocal(binding.local).ty == binding.ty);
                    std.debug.assert(program.getExpr(binding.value).ty == binding.ty);
                },
                .statement => |stmt_id| {
                    const stmt = program.getStmt(stmt_id);
                    std.debug.assert(stmt == .let_);
                },
            }
            previous = node;
        }
        std.debug.assert(previous == self.last);
        std.debug.assert((self.first == null) == (self.last == null));
    }

    fn referencedByExpr(
        self: BindingChain,
        allocator: Allocator,
        program: *const Ast.Program,
        expr: Ast.ExprId,
    ) Allocator.Error!bool {
        var current = self.first;
        while (current) |node| : (current = node.next) {
            switch (node.binding) {
                .strict => |binding| if (try exprReferencesLocal(allocator, program, expr, binding.local)) return true,
                .statement => |stmt_id| {
                    const stmt = program.getStmt(stmt_id);
                    if (stmt != .let_) Common.invariant("binding-chain statement was not a binding");
                    var found = false;
                    try Ast.forEachBoundLocal(allocator, program, stmt.let_.pat, PatternLocalUseProbe{
                        .allocator = allocator,
                        .program = program,
                        .expr = expr,
                        .found = &found,
                    });
                    if (found) return true;
                },
            }
        }
        return false;
    }
};

/// Symbolic structure plus the strict computations that produce its opaque
/// leaves. The chain is placed exactly once before any use of `value`.
const ClonedValue = struct {
    bindings: BindingChain = .{},
    value: Value,
};

/// An emitted arm body together with the symbolic value of its result. The
/// value is what case-of-case distribution reads later, so the arm never has
/// to be re-derived from its emitted expression.
const ClonedArm = struct {
    body: Ast.ExprId,
    value: Value,
};

/// A cloned expression before its strict chain is placed: either the source
/// expression reused as it stands, or the chain and value to emit.
const ClonedParts = struct {
    reused: ?Ast.ExprId,
    bindings: BindingChain,
    value: Value,
};

const ClonedBranches = struct {
    span: Ast.Span(Ast.Branch),
    /// One result value per branch, in branch order.
    values: []const Value,
};

const ClonedIfBranches = struct {
    span: Ast.Span(Ast.IfBranch),
    /// One result value per conditional branch, in branch order; the final
    /// else is cloned separately by the caller.
    values: []const Value,
};

/// One entry of a clone's recorded-value change log.
const RecordedValueKey = union(enum) {
    arm_values: Ast.ExprId,
    block_tail: Ast.ExprId,
};

/// Half-open range of expression ids `[start, end)`.
const ExprIdRange = struct {
    start: usize,
    end: usize,
};

const ClonedStmt = struct {
    bindings: BindingChain = .{},
    stmt: ?Ast.StmtId,
};

const LoopPattern = struct {
    /// The entry shape of each carried slot, split into leaves the back edges
    /// supply. A back edge that cannot supply one leaf demotes that leaf (not
    /// the whole slot) to `.any` in place, keeping its sibling leaves split.
    values: []Shape,
    /// Set by any back edge that demoted a leaf during a split attempt. The
    /// attempt's owner reads this after cloning the body, discards the clone,
    /// and retries with the demoted leaves carried as runtime scalars.
    any_demoted: bool,
};

/// The result of supplying one loop slot's leaves from a back edge: the
/// (possibly demoted) shape and whether any leaf demoted to `.any`.
const SuppliedSlot = struct {
    shape: Shape,
    demoted: bool,
};

/// Exact live items passed from a loop's typed tuple result to the
/// continuation that consumes it. Back-edge state is deliberately unaffected:
/// a one-item exit breaks with that existing item type, while a multi-item exit
/// jumps to a typed shared continuation.
const LoopExitSelection = struct {
    source_ty: Type.TypeId,
    source_arity: usize,
    kept_types: []const Type.TypeId,
    kept_indices: []const u32,
    transfer: union(enum) {
        break_value,
        jump: struct {
            target: Ast.JoinPointId,
        },
    },
};

/// A function currently being inlined, with the number of known-constructor
/// nodes carried by the call's arguments and captures. A same-function call
/// nested inside its own inlining may re-enter only when its known-constructor
/// arguments are strictly smaller, which is what lets an adapter's step inline
/// `Iter.next` on its own inner iterator (one adapter layer smaller) while
/// still terminating: the measure strictly decreases and the base iterator's
/// step calls no further `next`.
const InlineFrame = struct {
    fn_id: Ast.FnId,
    /// Null means this acyclic entry had no finite constructor-size proof. A
    /// recursive re-entry may proceed only when both frames have exact sizes
    /// and the new measure is strictly smaller.
    known_size: ?usize,
};

const ConstructorSize = union(enum) {
    exact: usize,
    unknown_budget_exhausted,

    fn plus(lhs: ConstructorSize, rhs: ConstructorSize) ConstructorSize {
        const lhs_exact = switch (lhs) {
            .exact => |value| value,
            .unknown_budget_exhausted => return .unknown_budget_exhausted,
        };
        const rhs_exact = switch (rhs) {
            .exact => |value| value,
            .unknown_budget_exhausted => return .unknown_budget_exhausted,
        };
        return .{ .exact = std.math.add(usize, lhs_exact, rhs_exact) catch return .unknown_budget_exhausted };
    }

    fn admitExpansion(self: ConstructorSize, limit: usize) CodeGrowthAdmission {
        return switch (self) {
            .exact => |value| if (value < limit) .admitted else .denied_growth_limit,
            .unknown_budget_exhausted => .denied_unknown_measure,
        };
    }

    fn exactValue(self: ConstructorSize) ?usize {
        return switch (self) {
            .exact => |value| value,
            .unknown_budget_exhausted => null,
        };
    }
};

/// Code-growth admission is deliberately separate from rewrite-legality proof.
/// Both denial cases retain one ordinary runtime value, but neither is a claim
/// about that value's shape or substitutability.
const CodeGrowthAdmission = enum {
    admitted,
    denied_growth_limit,
    denied_unknown_measure,
};

/// Explicit generated-code fuel. It may retain the ordinary shared IR but is
/// never consulted by a rewrite-legality query.
const CodeGrowthBudget = struct {
    remaining: usize,

    fn init(limit: usize) CodeGrowthBudget {
        return .{ .remaining = limit };
    }

    fn admit(self: *CodeGrowthBudget, amount: usize) CodeGrowthAdmission {
        if (amount > self.remaining) return .denied_growth_limit;
        self.remaining -= amount;
        return .admitted;
    }
};

const SpecAdmission = enum {
    admitted,
    denied_body_size,
    denied_spec_count,
};

/// The phase that owns a clone. Loop-exit selection is deliberately separate
/// from specialization and ordinary rewrites: their full lexical clones can
/// expose another projectable loop through an inlined callee, so only the final
/// call-opaque exit-selection phase may initiate another selected exit ABI.
const ClonePurpose = enum {
    specialization,
    rewrite,
    loop_exit_selection,
};

const InlineCallMode = enum {
    all,
    iterator_fusion,
    none,

    fn admitsDirect(
        self: InlineCallMode,
        procedure: ?check.StaticDispatchRegistry.IteratorProcedureId,
        inside_iterator: bool,
    ) bool {
        return switch (self) {
            .all => true,
            .iterator_fusion => inside_iterator or isIteratorProducer(procedure),
            .none => false,
        };
    }

    fn admitsCallable(self: InlineCallMode, callable: CallableValue, inside_iterator: bool) bool {
        return switch (self) {
            .all => true,
            .iterator_fusion => inside_iterator or callable.iterator_step,
            .none => false,
        };
    }
};

/// GHC-style body-size admission for SpecConstr work. A large source body is
/// left shared instead of being cloned into a worker or inlined into callers.
/// Small iterator and stream step functions stay well below this threshold, so
/// long fusion chains can still inline transitively through many small bodies.
const spec_constr_body_expr_threshold: usize = 200;

/// Maximum number of constructor-call-pattern workers for one source function.
/// Additional patterns keep the ordinary shared call, bounding generated worker
/// count without changing any shape proof.
const spec_constr_specialization_count: usize = 3;

const ActiveJoinClone = struct {
    source: Ast.JoinPointId,
    target: Ast.JoinPointId,
};

/// One jump into a let-of-case join: the placeholder jump expression emitted
/// at the site (its argument span is patched once the join's parameters are
/// decided) and the symbolic value the site supplies for each binder slot.
const LetCaseJumpSite = struct {
    expr: Ast.ExprId,
    bindings: BindingChain,
    values: []const Value,
};

/// One join point minted while rewriting a `let` of a branching value. The
/// continuation region `body` is cloned exactly once; every arm reaches it
/// through a jump. `binding` says how the body consumes the join parameters:
/// either the let's own pattern flow-bound to the joined value, or the binder
/// locals of one branch pattern of a dispatching match.
const LetCaseJoin = struct {
    id: Ast.JoinPointId,
    binding: union(enum) {
        pattern: LetCasePatternBinding,
        locals: []const Ast.LocalId,
    },
    body: Ast.ExprId,
    sites: std.ArrayList(LetCaseJumpSite),
};

const LetCasePatternBinding = struct {
    pat: Ast.PatId,
    comptime_site: ?Ast.ComptimeSiteId,
};

/// The joins of one active let-of-case rewrite. Jump cloning consults the
/// stack of these frames so nested rewrites resolve their own targets.
const LetCaseBuild = struct {
    joins: []LetCaseJoin,
};

const CallableWorkerIdentity = struct {
    template: names.TypeDigest,
    callable_abi: names.TypeDigest,
    capture_abi: names.TypeDigest,
};

const InlineScopeRebasePair = struct {
    source: Ast.InlineScopeId,
    outer: Ast.InlineScopeId,
};

const Pass = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    program: *Ast.Program,
    plans: []FnPlan,
    symbols: Common.SymbolGen,
    /// Direct-call inlining scope for this pass's value-aware clones. See
    /// `CloneInlining`.
    clone_inlining: CloneInlining = .all_calls,
    /// Direct callers recorded per function while the first argument-use walk
    /// runs; null outside `collectArgUses`.
    arg_use_callers: ?[]std.ArrayList(Ast.FnId) = null,
    /// Per source function: whether the whole-body value clone has already
    /// satisfied value-aware call rewriting, shape demand, and known-loop
    /// scalarization. Those analyses can all request the same clone, but the
    /// clone is one normalization pass and must run at most once per body.
    whole_body_cloned: []bool,
    /// One rewritten callable body per stable Monotype template identity,
    /// exact callable-use ABI, and exact capture ABI. Lifted FnIds are transient
    /// products of traversal order; two uses may share a body only when their
    /// function representations and every CaptureId's type are identical.
    callable_workers: std.AutoHashMap(CallableWorkerIdentity, Ast.FnId),
    /// Reverse index from each rewritten callable body to its source function.
    /// This keeps later materialization rooted at the source instead of cloning
    /// an already-rewritten worker.
    callable_sources: collections.DenseMap(Ast.FnId, Ast.FnId),
    next_join_point: u32,
    /// Read-only constructor views of closed, source-owned static expressions.
    /// These contain no generated IR or caller substitutions and survive the
    /// speculative clones' arena rewinds. Binding expressions remain opaque,
    /// so initializer-private locals never become caller-visible leaves.
    static_data_structure: collections.DenseMap(Ast.ExprId, *const Value),
    options: Options = .{},
    /// Only symbolic discovery workers have a sink. Its arena survives the
    /// callback; the coordinator alone admits and durably copies requests.
    pattern_requests: ?*RequestSink = null,
    /// Phase-entry eligibility is independent of coordinator admission in earlier waves.
    discovery_admission: ?[]const SpecAdmission = null,
    borrowed_worker: bool = false,
    /// What each frozen source loop body contains, keyed by the body. One
    /// walk of an outer loop records every loop nested in it, so nested
    /// loops never rescan their bodies.
    loop_body_contents: std.AutoHashMapUnmanaged(Ast.ExprId, LoopBodyContents) = .empty,

    const PatternRequest = struct { fn_id: Ast.FnId, pattern: CallPattern };
    const RequestSink = struct {
        arena: std.heap.ArenaAllocator,
        items: std.ArrayList(PatternRequest) = .empty,
    };

    /// A bounded wave retains output independently of executor worker count.
    /// Neither plans nor the source Program change until every callback drains.
    const wave_capacity = 32;
    const Work = struct {
        source: *Pass,
        fn_id: Ast.FnId,
        phase: Phase,
        discovery_admission: ?[]const SpecAdmission = null,
        /// The function's shapes excluded it from the phase; the task runs only
        /// to verify that the phase indeed changes nothing, and is never merged.
        verify_only: bool = false,
        output: ?*Output = null,
        failure: ?Common.LowerError = null,

        fn callback(context: *anyopaque, worker: TaskExecutor.Worker) ?*anyopaque {
            const work: *Work = @ptrCast(@alignCast(context));
            work.execute(worker.allocator, worker.scratch) catch |err| {
                work.failure = err;
            };
            return null;
        }

        fn execute(self: *Work, allocator: Allocator, scratch: Allocator) Common.LowerError!void {
            const output = try allocator.create(Output);
            errdefer allocator.destroy(output);
            output.* = .{
                .allocator = allocator,
                .program = try self.source.program.cloneForSpecConstrBody(allocator, self.fn_id),
                .requests = .{ .arena = std.heap.ArenaAllocator.init(allocator) },
            };
            errdefer output.program.deinit();
            errdefer output.requests.arena.deinit();
            var pass: Pass = .{
                .allocator = scratch,
                .arena = std.heap.ArenaAllocator.init(scratch),
                .program = &output.program,
                .plans = self.source.plans,
                .symbols = self.source.symbols,
                .clone_inlining = self.source.clone_inlining,
                .whole_body_cloned = self.source.whole_body_cloned,
                .callable_workers = std.AutoHashMap(CallableWorkerIdentity, Ast.FnId).init(scratch),
                .callable_sources = self.source.callable_sources,
                .next_join_point = self.source.next_join_point,
                .static_data_structure = .init(scratch),
                .discovery_admission = self.discovery_admission,
                .borrowed_worker = true,
            };
            defer pass.arena.deinit();
            defer pass.static_data_structure.deinit();
            defer pass.callable_workers.deinit();
            defer pass.loop_body_contents.deinit(scratch);
            if (self.phase == .discovery) pass.pattern_requests = &output.requests;
            switch (self.phase) {
                .discovery => {
                    const mark = pass.markAnalysis();
                    var cloner = Cloner.initForRewrite(&pass);
                    defer cloner.deinit();
                    cloner.rewrite_call_patterns = false;
                    cloner.emit_callable_workers = false;
                    cloner.inline_calls = .iterator_fusion;
                    cloner.inline_direct_requires_known_arg = true;
                    try cloner.collectCallPatternsInExpr(self.fn_id, pass.sourceBody(self.fn_id).roc);
                    pass.rewindAnalysis(mark);
                },
                .iterator_fusion => {
                    try pass.cloneFnBodyForIteratorFusion(self.fn_id);
                    output.changed = true;
                },
                .unused_loop_results => output.changed = try pass.projectUnusedLoopResultsInFn(self.fn_id),
            }
            output.symbol_count = pass.symbols.next - self.source.symbols.next;
            output.join_count = pass.next_join_point - self.source.next_join_point;
            if (self.phase == .discovery) {
                // Discovery retains function/pattern requests, never its generated AST.
                output.program.deinit();
                output.program_live = false;
            }
            self.output = output;
        }
    };

    const Output = struct {
        allocator: Allocator,
        program: Ast.Program,
        program_live: bool = true,
        requests: RequestSink,
        changed: bool = false,
        symbol_count: u32 = 0,
        join_count: u32 = 0,

        fn deinit(self: *Output) void {
            self.requests.arena.deinit();
            if (self.program_live) self.program.deinit();
            self.allocator.destroy(self);
        }
    };

    /// Whether a function's recorded shapes admit it to a phase: the phase can
    /// only change a function whose body has the shape it rewrites. Discovery
    /// with whole-program clone inlining can meet a known-shaped argument in
    /// any inlined callee, so there only a direct call is required.
    fn phaseAdmits(self: *const Pass, phase: Phase, fn_id: Ast.FnId) bool {
        const shapes = self.program.getFn(fn_id).shapes;
        return switch (phase) {
            .discovery => switch (self.clone_inlining) {
                .all_calls => shapes.direct_call,
                .iterator_fusion => shapes.direct_call and (shapes.constructs_value or shapes.iterator_call),
            },
            .unused_loop_results => shapes.loop_tuple_result,
            .iterator_fusion => shapes.iterator_producer,
        };
    }

    fn runIndependentPhase(self: *Pass, phase: Phase, fn_count: usize) Common.LowerError!void {
        try self.program.names.prepareForReadSharing();
        try self.program.types.prepareForReadSharingQueries(&self.program.names, Type.Store.ReadSharingQueries.spec_constr);
        const admission = if (phase == .discovery) try self.allocator.alloc(SpecAdmission, self.plans.len) else null;
        defer if (admission) |snapshot| self.allocator.free(snapshot);
        if (admission) |snapshot| {
            for (snapshot, 0..) |*entry, raw| entry.* = self.newSpecAdmission(raw);
        }
        var next_fn: usize = 0;
        while (next_fn < fn_count) {
            var work: [wave_capacity]Work = undefined;
            var count: usize = 0;
            while (next_fn < fn_count and count < wave_capacity) : (next_fn += 1) {
                const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(next_fn)));
                const body = if (phase == .unused_loop_results) self.program.getFn(fn_id).body else self.sourceBody(fn_id);
                if (body == .hosted) continue;
                const admitted = self.phaseAdmits(phase, fn_id);
                if (!admitted) {
                    if (builtin.mode != .Debug) continue;
                    // Iterator fusion clones every body it is given, so the
                    // verification for it is the producer scan itself.
                    if (phase == .iterator_fusion) {
                        if (try exprContainsIteratorProducer(self.allocator, self.program, body.roc)) {
                            std.debug.panic("SpecConstr iterator_fusion excluded function {d} whose shapes {any} hide an iterator producer", .{ @intFromEnum(fn_id), self.program.getFn(fn_id).shapes });
                        }
                        continue;
                    }
                }
                work[count] = .{ .source = self, .fn_id = fn_id, .phase = phase, .discovery_admission = admission, .verify_only = !admitted };
                count += 1;
            }
            defer for (work[0..count]) |item| {
                if (item.output) |output| output.deinit();
            };
            if (count == 0) continue;
            const symbol_start = self.symbols.next;
            const join_start = self.next_join_point;
            if (self.options.executor) |executor| {
                var session = executor.begin();
                var submitted: usize = 0;
                var received: usize = 0;
                var failure: ?Allocator.Error = null;
                while (received < submitted or (failure == null and submitted < count)) {
                    while (failure == null and submitted < count and session.canSubmit()) {
                        session.submit(.{ .id = submitted, .context = &work[submitted], .run = Work.callback }) catch |err| {
                            failure = err;
                            break;
                        };
                        submitted += 1;
                        if (self.options.metrics_out) |metrics| metrics.tasks_submitted += 1;
                    }
                    if (received < submitted) {
                        const completion = session.receive();
                        received += 1;
                        if (work[completion.id].failure) |err| {
                            if (failure == null) failure = err;
                        }
                    }
                }
                session.end();
                if (failure) |err| return err;
            } else {
                for (work[0..count]) |*item| {
                    var scratch = std.heap.ArenaAllocator.init(self.allocator);
                    defer scratch.deinit();
                    try item.execute(self.allocator, scratch.allocator());
                }
            }
            if (self.options.metrics_out) |metrics| {
                metrics.peak_retained_shards = @max(metrics.peak_retained_shards, count);
            }
            // Merge only after the read barrier, in source-function/request order.
            for (work[0..count]) |item| {
                if (item.failure) |err| return err;
                const output = item.output orelse Common.invariant("SpecConstr task completed without output");
                if (item.verify_only) {
                    if (output.changed or output.requests.items.items.len != 0) {
                        std.debug.panic("SpecConstr {s} changed function {d} whose shapes {any} excluded it from the phase", .{ @tagName(phase), @intFromEnum(item.fn_id), self.program.getFn(item.fn_id).shapes });
                    }
                    if (self.options.metrics_out) |metrics| {
                        if (self.options.executor != null) {
                            metrics.tasks_committed += 1;
                            metrics.committed_by_phase[@intFromEnum(phase)] += 1;
                        }
                    }
                    continue;
                }
                var admitted: usize = 0;
                for (output.requests.items.items) |request| {
                    if (try self.admitPatternRequest(request)) admitted += 1;
                }
                if (output.changed) {
                    const symbol_end = try checkedIdentityTotal(self.symbols.next, output.symbol_count);
                    const join_end = try checkedIdentityTotal(self.next_join_point, output.join_count);
                    const before = self.program.exprCount();
                    try self.program.appendSpecConstrBody(&output.program, symbol_start, self.symbols.next - symbol_start, join_start, self.next_join_point - join_start);
                    self.symbols.next = symbol_end;
                    self.next_join_point = join_end;
                    if (phase == .iterator_fusion) self.whole_body_cloned[@intFromEnum(item.fn_id)] = true;
                    if (self.options.metrics_out) |metrics| {
                        metrics.bodies_committed += 1;
                        metrics.expressions_committed += self.program.exprCount() - before;
                    }
                }
                if (self.options.metrics_out) |metrics| {
                    if (self.options.executor != null) {
                        metrics.tasks_committed += 1;
                        metrics.committed_by_phase[@intFromEnum(phase)] += 1;
                    }
                    metrics.patterns_recorded += output.requests.items.items.len;
                    metrics.patterns_admitted += admitted;
                    if (output.changed or admitted != 0) metrics.changed_by_phase[@intFromEnum(phase)] += 1;
                }
            }
        }
    }

    fn admitPatternRequest(self: *Pass, request: PatternRequest) Allocator.Error!bool {
        const raw = @intFromEnum(request.fn_id);
        if (self.newSpecAdmission(raw) != .admitted) return false;
        for (self.plans[raw].specs.items) |spec| {
            if (try patternEql(self.program, spec.pattern, request.pattern)) return false;
        }
        const args = try self.arena.allocator().alloc(Shape, request.pattern.args.len);
        for (args, request.pattern.args) |*arg, source| arg.* = try copyShape(self.arena.allocator(), source);
        try self.plans[raw].specs.append(self.allocator, .{ .pattern = .{ .args = args } });
        return true;
    }

    const AnalysisMark = struct {
        program: Ast.Program.SpecConstrAnalysisMark,
        next_symbol: u32,
        next_join_point: u32,
    };
    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Pass {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        const plans = try allocator.alloc(FnPlan, program.fnCount());
        errdefer allocator.free(plans);
        var initialized_plans: usize = 0;
        errdefer for (plans[0..initialized_plans]) |plan| allocator.free(plan.used_args);

        for (plans, 0..) |*plan, index| {
            const fn_ = program.getFnAt(index);
            const args = program.typedLocalSpan(fn_.args);
            const used_args = try allocator.alloc(bool, args.len);
            errdefer allocator.free(used_args);
            @memset(used_args, false);
            plan.* = .{
                .used_args = used_args,
                .source = .{
                    .body = fn_.body,
                    .size = try fnBodySizeWithin(allocator, program, fn_.body, spec_constr_body_expr_threshold),
                },
                .specs = .empty,
            };
            initialized_plans += 1;
        }

        const whole_body_cloned = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(whole_body_cloned);
        @memset(whole_body_cloned, false);

        return .{
            .allocator = allocator,
            .arena = arena,
            .program = program,
            .plans = plans,
            .symbols = .{ .next = program.next_symbol },
            .whole_body_cloned = whole_body_cloned,
            .callable_workers = std.AutoHashMap(CallableWorkerIdentity, Ast.FnId).init(allocator),
            .callable_sources = collections.DenseMap(Ast.FnId, Ast.FnId).init(allocator),
            // Monotype-generated joins use the raw id of their owning
            // expression. New SpecConstr joins begin beyond the complete
            // existing expression arena, so the two producer namespaces
            // cannot collide.
            .next_join_point = @intCast(program.exprCount()),
            .static_data_structure = .init(allocator),
        };
    }

    fn freshJoinPoint(self: *Pass) Ast.JoinPointId {
        const id: Ast.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    /// Inspect explicit constructors without cloning or scheduling any work.
    /// The source expression graph is acyclic: recursive values use local
    /// references, and this reader never follows bindings or local references.
    /// Memoization visits each shared expression once. In particular, a list's
    /// elements need no visit because SpecConstr has no symbolic list shape.
    ///
    /// Constructors nest as deeply as the source writes them, so each
    /// constructor waits on an explicit frame for its components and is
    /// memoized after them, as a direct walk would.
    fn staticDataStructure(self: *Pass, root: Ast.ExprId) Allocator.Error!*const Value {
        if (self.static_data_structure.get(root)) |cached| return cached;
        const Frame = struct {
            expr_id: Ast.ExprId,
            components: []const Ast.ExprId,
            values: []Value,
            next: usize = 0,
        };
        var frames = std.ArrayList(Frame).empty;
        defer {
            for (frames.items) |frame| {
                self.allocator.free(frame.components);
                self.allocator.free(frame.values);
            }
            frames.deinit(self.allocator);
        }
        var delivered: ?*const Value = null;
        if (try self.staticDataLeaf(root)) |leaf| return leaf;
        try frames.append(self.allocator, try self.staticDataFrame(Frame, root));
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            if (delivered) |value| {
                frame.values[frame.next - 1] = value.*;
                delivered = null;
            }
            if (frame.next < frame.components.len) {
                const component = frame.components[frame.next];
                frame.next += 1;
                if (self.static_data_structure.get(component)) |cached| {
                    delivered = cached;
                } else if (try self.staticDataLeaf(component)) |leaf| {
                    delivered = leaf;
                } else {
                    try frames.ensureUnusedCapacity(self.allocator, 1);
                    frames.appendAssumeCapacity(try self.staticDataFrame(Frame, component));
                }
                continue;
            }
            const finished = frames.pop().?;
            defer {
                self.allocator.free(finished.components);
                self.allocator.free(finished.values);
            }
            const stored = try self.finishStaticDataStructure(finished.expr_id, finished.values);
            if (frames.items.len == 0) return stored;
            delivered = stored;
        }
    }

    /// The memoized structure of an expression with no constructor
    /// components, or null for a constructor.
    fn staticDataLeaf(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!?*const Value {
        const expr = self.program.getExpr(expr_id);
        const value: Value = switch (expr.data) {
            .comptime_value => .{ .expr = expr_id },
            .static_data_candidate, .tag, .tuple, .record, .nominal, .fn_ref => return null,
            // A binding/control expression owns its complete lexical scope.
            // Retaining it as an opaque leaf preserves its exact evaluation
            // without exposing initializer-private locals through the view.
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .typed_boundary,
            .list,
            .record_update,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .call_value,
            .call_proc,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => blk: {
                if (std.debug.runtime_safety) {
                    var scope: BodyLocalScope = .{
                        .program = self.program,
                        .allocator = self.allocator,
                        .fn_index = null,
                        .bound = .init(self.allocator),
                        .joins = .init(self.allocator),
                    };
                    defer scope.bound.deinit();
                    defer scope.joins.deinit();
                    try scope.walkExpr(expr_id);
                }
                break :blk .{ .expr = expr_id };
            },
        };
        return try self.storeStaticDataStructure(expr_id, value);
    }

    fn storeStaticDataStructure(self: *Pass, expr_id: Ast.ExprId, value: Value) Allocator.Error!*const Value {
        const stored = try self.arena.allocator().create(Value);
        stored.* = value;
        try self.static_data_structure.put(expr_id, stored);
        return stored;
    }

    /// A constructor's frame, listing its components in order.
    fn staticDataFrame(self: *Pass, comptime Frame: type, expr_id: Ast.ExprId) Allocator.Error!Frame {
        const expr = self.program.getExpr(expr_id);
        var components = std.ArrayList(Ast.ExprId).empty;
        errdefer components.deinit(self.allocator);
        switch (expr.data) {
            .static_data_candidate => |candidate| try components.append(self.allocator, candidate.runtime_expr),
            .tag => |tag| for (0..tag.payloads.len) |i| try components.append(self.allocator, GuardedList.at(self.program.exprSpan(tag.payloads), i)),
            .tuple => |span| for (0..span.len) |i| try components.append(self.allocator, GuardedList.at(self.program.exprSpan(span), i)),
            .record => |span| for (0..span.len) |i| try components.append(self.allocator, GuardedList.at(self.program.fieldExprSpan(span), i).value),
            .nominal => |backing| try components.append(self.allocator, backing),
            .fn_ref => |ref| for (0..ref.captures.len) |i| try components.append(self.allocator, self.program.captureOperandAt(ref.captures, i).value),
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .typed_boundary, .list, .record_update, .let_, .lambda, .def_ref, .fn_def, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
        }
        const owned = try components.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(owned);
        return .{ .expr_id = expr_id, .components = owned, .values = try self.allocator.alloc(Value, owned.len) };
    }

    /// Store a constructor's structure from its components' structures.
    fn finishStaticDataStructure(self: *Pass, expr_id: Ast.ExprId, values: []const Value) Allocator.Error!*const Value {
        const arena = self.arena.allocator();
        const expr = self.program.getExpr(expr_id);
        const value: Value = switch (expr.data) {
            .static_data_candidate => |candidate| .{ .static_data_candidate = .{
                .ty = expr.ty,
                .static_data = candidate.static_data,
                .expr = expr_id,
                .structure = blk: {
                    const structure = try arena.create(Value);
                    structure.* = values[0];
                    break :blk structure;
                },
            } },
            .tag => |tag| .{ .tag = .{ .ty = expr.ty, .name = tag.name, .payloads = try arena.dupe(Value, values) } },
            .tuple => .{ .tuple = .{ .ty = expr.ty, .items = try arena.dupe(Value, values) } },
            .record => |span| blk: {
                const fields = try arena.alloc(FieldValue, span.len);
                for (fields, values, 0..) |*field, field_value, i| {
                    field.* = .{
                        .name = GuardedList.at(self.program.fieldExprSpan(span), i).name,
                        .value = field_value,
                    };
                }
                break :blk .{ .record = .{ .ty = expr.ty, .fields = fields } };
            },
            .nominal => .{ .nominal = .{
                .ty = expr.ty,
                .backing = blk: {
                    const backing = try arena.create(Value);
                    backing.* = values[0];
                    break :blk backing;
                },
            } },
            .fn_ref => |ref| blk: {
                const captures = try arena.alloc(CaptureValue, ref.captures.len);
                for (captures, values, 0..) |*capture, capture_value, i| {
                    capture.* = .{
                        .id = self.program.captureOperandAt(ref.captures, i).id,
                        .value = capture_value,
                    };
                }
                break :blk .{ .callable = .{ .ty = expr.ty, .fn_id = ref.fn_id, .captures = captures } };
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .typed_boundary, .list, .record_update, .let_, .lambda, .def_ref, .fn_def, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
        };
        return try self.storeStaticDataStructure(expr_id, value);
    }

    fn markAnalysis(self: *Pass) AnalysisMark {
        return .{
            .program = self.program.markSpecConstrAnalysis(),
            .next_symbol = self.symbols.next,
            .next_join_point = self.next_join_point,
        };
    }

    fn rewindAnalysis(self: *Pass, mark: AnalysisMark) void {
        self.program.rewindSpecConstrAnalysis(mark.program);
        self.restoreAnalysisIds(mark);
    }

    fn restoreAnalysisIds(self: *Pass, mark: AnalysisMark) void {
        var next_symbol = mark.next_symbol;
        for (mark.program.fns..self.program.fnCount()) |index| {
            const fn_ = self.program.getFnAt(index);
            if (fn_.body != .hosted or fn_.args.len != 0) {
                Common.invariant("SpecConstr analysis emitted a non-reservation function");
            }
            next_symbol = @max(next_symbol, @intFromEnum(fn_.symbol) + 1);
        }
        self.symbols.next = next_symbol;
        self.next_join_point = mark.next_join_point;
    }

    /// The contents of a loop body with the given initial values, walked at
    /// most once for each frozen body.
    fn loopBodyContents(self: *Pass, body: Ast.ExprId, initial_values: []const Ast.ExprId) Allocator.Error!LoopBodyContents {
        if (self.loop_body_contents.get(body)) |contents| return contents;
        const uses = try initialLocalUses(self.arena.allocator(), self.program, initial_values, initial_values.len);
        const contents = try walkLoopBodyContents(self, body, uses);
        if (self.program.isFrozenExpr(body)) try self.loop_body_contents.put(self.allocator, body, contents);
        return contents;
    }

    fn deinit(self: *Pass) void {
        self.loop_body_contents.deinit(self.allocator);
        self.static_data_structure.deinit();
        self.callable_sources.deinit();
        self.callable_workers.deinit();
        self.allocator.free(self.whole_body_cloned);
        for (self.plans) |*plan| plan.deinit(self.allocator);
        self.allocator.free(self.plans);
        self.arena.deinit();
    }

    fn run(self: *Pass) Common.LowerError!OwnedProcedureUsage {
        const original_fn_count = self.plans.len;

        const capture_snapshot = try self.snapshotOriginalCaptures(original_fn_count);
        defer {
            for (capture_snapshot) |captures| self.allocator.free(captures);
            self.allocator.free(capture_snapshot);
        }

        try self.specializeBranchAppendTails(original_fn_count);
        try self.collectArgUses(original_fn_count);
        try self.collectCallPatterns(original_fn_count);
        try self.collectValueAwareCallPatterns(original_fn_count);
        try self.reserveSpecIds();
        try self.createSpecializations(original_fn_count);
        try self.rewriteExistingCalls();
        try self.rewriteAllOriginalBodies(original_fn_count);
        try self.createSpecializations(original_fn_count);
        try self.projectUnusedLoopResults();
        var procedure_usage = try self.localizeSingleUseTailRecursiveWorkers(original_fn_count);
        errdefer procedure_usage.deinit();
        try Lift.recomputeCaptures(self.allocator, self.program);
        self.verifyRewrittenCaptureGain(capture_snapshot);
        try self.verifyRewrittenBodyLocals(original_fn_count);

        self.program.next_symbol = self.symbols.next;
        return procedure_usage;
    }

    fn runIteratorFusion(self: *Pass) Common.LowerError!void {
        const original_fn_count = self.plans.len;
        const capture_snapshot = try self.snapshotOriginalCaptures(original_fn_count);
        defer {
            for (capture_snapshot) |captures| self.allocator.free(captures);
            self.allocator.free(capture_snapshot);
        }

        try self.runIndependentPhase(.iterator_fusion, original_fn_count);

        try Lift.recomputeCaptures(self.allocator, self.program);
        self.verifyRewrittenCaptureGain(capture_snapshot);
        try self.verifyRewrittenBodyLocals(original_fn_count);
        self.program.next_symbol = self.symbols.next;
    }

    /// Turn a specialized tail-recursive worker with exactly one external use
    /// into a recursive join point at that use. Specialization has already
    /// exposed the worker's constructor leaves as scalar arguments here, so
    /// moving that exact ABI into the caller preserves the specialized loop
    /// without paying an out-of-line call or duplicating any code.
    fn localizeSingleUseTailRecursiveWorkers(self: *Pass, original_fn_count: usize) Common.LowerError!OwnedProcedureUsage {
        const fn_count = self.program.fnCount();
        var program_usage = try ProgramProcedureUsage.collect(self.allocator, self.program);
        defer program_usage.deinit(self.allocator);

        for (original_fn_count..fn_count) |worker_index| {
            const worker_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(worker_index)));
            const worker = self.program.getFn(worker_id);
            if (worker.body == .hosted) continue;

            // A source-level return is relative to the worker procedure. It
            // cannot be moved across a procedure boundary until the IR gives
            // it an explicit continuation target.
            if (program_usage.fn_uses[worker_index].contains_return) continue;

            const tail = program_usage.tail_self_calls[worker_index];
            if (!tail.valid or tail.count == 0) continue;

            const uses = program_usage.fn_uses[worker_index];
            if (uses.external_calls != 1 or uses.value_refs != 0) continue;
            const call_expr = uses.external_call_expr orelse
                Common.invariant("single-use specialized worker had no external call expression");
            try self.localizeTailRecursiveWorker(worker_id, call_expr);

            // Localization clones one worker body into its caller, changing
            // downstream use edges. Collect a fresh program-wide usage snapshot
            // before considering another worker; stale use counts must never
            // authorize a second localization.
            var refreshed_usage = try ProgramProcedureUsage.collect(self.allocator, self.program);
            program_usage.deinit(self.allocator);
            program_usage = refreshed_usage;
            refreshed_usage = undefined;
        }

        return .{
            .allocator = self.allocator,
            .items = program_usage.takeFnUses(),
        };
    }

    fn localizeTailRecursiveWorker(
        self: *Pass,
        worker_id: Ast.FnId,
        call_expr_id: Ast.ExprId,
    ) Common.LowerError!void {
        const worker = self.program.getFn(worker_id);
        const worker_body = switch (worker.body) {
            .roc => |body| body,
            .hosted => Common.invariant("hosted specialized worker reached join-point localization"),
        };
        const call_expr = self.program.getExpr(call_expr_id);
        if (call_expr.data != .call_proc) Common.invariant("specialized worker use stopped being a direct call before localization");
        const call = call_expr.data.call_proc;
        if (Ast.localDirectCallee(call) != worker_id) {
            Common.invariant("specialized worker use changed callee before localization");
        }
        if (call.is_cold) return;

        const source_args = try GuardedList.dupe(self.allocator, Ast.TypedLocal, self.program.typedLocalSpan(worker.args));
        defer self.allocator.free(source_args);
        const source_captures = try GuardedList.dupe(self.allocator, Ast.TypedLocal, self.program.typedLocalSpan(worker.captures));
        defer self.allocator.free(source_captures);

        var params = std.ArrayList(Ast.TypedLocal).empty;
        defer params.deinit(self.allocator);

        var cloner = Cloner.initForRewrite(self);
        defer cloner.deinit();
        cloner.inline_calls = .none;
        cloner.rewrite_call_patterns = false;
        cloner.emit_callable_workers = false;

        for (source_args) |source_arg| {
            const local = try self.program.addLocal(self.symbols.fresh(), source_arg.ty);
            try params.append(self.allocator, .{ .local = local, .ty = source_arg.ty });
            const local_expr = try self.program.addExpr(.{ .ty = source_arg.ty, .data = .{ .local = local } });
            try cloner.subst.put(self.program, source_arg.local, .{ .expr = local_expr });
        }
        for (source_captures) |source_capture| {
            const local = try self.program.addLocal(self.symbols.fresh(), source_capture.ty);
            try params.append(self.allocator, .{ .local = local, .ty = source_capture.ty });
            const local_expr = try self.program.addExpr(.{ .ty = source_capture.ty, .data = .{ .local = local } });
            try cloner.subst.put(self.program, source_capture.local, .{ .expr = local_expr });
        }

        const cloned_body = try cloner.cloneExpr(worker_body);
        const loop_join = self.freshJoinPoint();
        try self.rewriteTailSelfCallsAsJumps(cloned_body, worker_id, worker.captures, loop_join);
        const localized_body = cloned_body;

        var initial_values = std.ArrayList(Ast.ExprId).empty;
        defer initial_values.deinit(self.allocator);
        const call_args = self.program.exprSpan(call.args);
        for (0..call_args.len) |index| try initial_values.append(self.allocator, GuardedList.at(call_args, index));
        try self.appendCaptureValuesForSlots(worker.captures, call.captures, &initial_values);
        if (initial_values.items.len != params.items.len) {
            Common.invariant("localized worker initial value count differed from join parameter count");
        }

        const initial_jump = try self.program.addExpr(.{ .ty = call_expr.ty, .data = .{ .jump = .{
            .target = loop_join,
            .args = try self.program.addExprSpan(initial_values.items),
        } } });
        self.program.setExprData(call_expr_id, .{ .join_point = .{
            .id = loop_join,
            .params = try self.program.addTypedLocalSpan(params.items),
            .body = localized_body,
            .remainder = initial_jump,
        } });
    }

    fn appendCaptureValuesForSlots(
        self: *Pass,
        slots_span: Ast.Span(Ast.TypedLocal),
        operands_span: Ast.Span(Ast.CaptureOperand),
        out: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!void {
        const slots = self.program.typedLocalSpan(slots_span);
        const operands = self.program.captureOperandSpan(operands_span);
        if (slots.len != operands.len) {
            Common.invariant("localized worker capture operand count differed from capture slot count");
        }
        for (0..slots.len) |slot_index| {
            const slot = GuardedList.at(slots, slot_index);
            const id = self.program.captureIdOfLocal(slot.local);
            var value: ?Ast.ExprId = null;
            for (0..operands.len) |operand_index| {
                const operand = GuardedList.at(operands, operand_index);
                if (operand.id == id) {
                    value = operand.value;
                    break;
                }
            }
            try out.append(self.allocator, value orelse
                Common.invariant("localized worker call omitted a keyed capture operand"));
        }
    }

    /// Rewrite only syntactic tail positions, after `tailSelfCallSummary` has
    /// proved every recursive call is in one of them. Named jumps deliberately
    /// target the new outer join even when the tail position is nested under a
    /// different loop or join point. Each self call is replaced in place, so
    /// the enclosing expressions keep their ids; tail positions are visited in
    /// source order on a work stack.
    fn rewriteTailSelfCallsAsJumps(
        self: *Pass,
        root: Ast.ExprId,
        worker_id: Ast.FnId,
        capture_slots: Ast.Span(Ast.TypedLocal),
        loop_join: Ast.JoinPointId,
    ) Common.LowerError!void {
        var tails: std.ArrayList(Ast.ExprId) = .empty;
        defer tails.deinit(self.allocator);
        try tails.append(self.allocator, root);
        while (tails.pop()) |expr_id| {
            // Tail positions are appended in source order, then reversed.
            const start = tails.items.len;
            switch (self.program.getExpr(expr_id).data) {
                .call_proc => |call| {
                    if (Ast.localDirectCallee(call) != worker_id) continue;
                    var values = std.ArrayList(Ast.ExprId).empty;
                    defer values.deinit(self.allocator);
                    const args = self.program.exprSpan(call.args);
                    for (0..args.len) |index| try values.append(self.allocator, GuardedList.at(args, index));
                    try self.appendCaptureValuesForSlots(capture_slots, call.captures, &values);
                    self.program.setExprData(expr_id, .{ .jump = .{
                        .target = loop_join,
                        .args = try self.program.addExprSpan(values.items),
                    } });
                },
                .let_ => |let_| try tails.append(self.allocator, let_.rest),
                .match_ => |match| {
                    const branches = self.program.branchSpan(match.branches);
                    for (0..branches.len) |index| try tails.append(self.allocator, GuardedList.at(branches, index).body);
                },
                .if_ => |if_| {
                    const branches = self.program.ifBranchSpan(if_.branches);
                    for (0..branches.len) |index| try tails.append(self.allocator, GuardedList.at(branches, index).body);
                    try tails.append(self.allocator, if_.final_else);
                },
                .block => |block| try tails.append(self.allocator, block.final_expr),
                .join_point => |join_point| {
                    try tails.append(self.allocator, join_point.body);
                    try tails.append(self.allocator, join_point.remainder);
                },
                .if_initialized_payload => |payload_switch| {
                    try tails.append(self.allocator, payload_switch.initialized);
                    try tails.append(self.allocator, payload_switch.uninitialized);
                },
                .try_sequence => |sequence| try tails.append(self.allocator, sequence.ok_body),
                .try_record_sequence => |sequence| try tails.append(self.allocator, sequence.ok_body),
                .comptime_branch_taken => |taken| try tails.append(self.allocator, taken.body),
                .typed_boundary => |boundary| try tails.append(self.allocator, boundary.value),
                .local,
                .unit,
                .@"unreachable",
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .static_data_candidate,
                .comptime_value,
                .list,
                .tuple,
                .record,
                .record_update,
                .tag,
                .nominal,
                .lambda,
                .def_ref,
                .fn_def,
                .fn_ref,
                .call_value,
                .low_level,
                .field_access,
                .tuple_access,
                .structural_eq,
                .structural_hash,
                .uninitialized,
                .uninitialized_payload,
                .loop_,
                .break_,
                .continue_,
                .jump,
                .return_,
                .crash,
                .checked_error,
                .comptime_exhaustiveness_failed,
                .dbg,
                .expect_err,
                .literal_rejected,
                .expect,
                => {},
            }
            std.mem.reverse(Ast.ExprId, tails.items[start..]);
        }
    }

    /// Debug-only: the capture local ids each original fn declares before any
    /// rewriting, indexed by fn. Empty outside safety-checked builds.
    fn snapshotOriginalCaptures(self: *Pass, original_fn_count: usize) Allocator.Error![]const []const Ast.LocalId {
        if (!std.debug.runtime_safety) return &.{};
        const snapshot = try self.allocator.alloc([]const Ast.LocalId, original_fn_count);
        for (0..original_fn_count) |index| {
            const captures = self.program.typedLocalSpan(self.program.getFnAt(index).captures);
            const locals = try self.allocator.alloc(Ast.LocalId, captures.len);
            for (0..captures.len) |capture_index| {
                locals[capture_index] = GuardedList.at(captures, capture_index).local;
            }
            snapshot[index] = locals;
        }
        return snapshot;
    }

    /// Debug-only: a value-substituting rewrite must never introduce a new
    /// free local, so a fn whose body was rewritten in place may not gain a
    /// capture its source did not declare. A gained capture is a reference
    /// the rewrite left resolving to a vanished binding—capture
    /// recomputation silently promotes it to a phantom argument, which
    /// misreads whatever register the caller happens to leave there.
    fn verifyRewrittenCaptureGain(self: *Pass, capture_snapshot: []const []const Ast.LocalId) void {
        if (!std.debug.runtime_safety) return;
        for (capture_snapshot, 0..) |original_captures, index| {
            if (index >= self.whole_body_cloned.len or !self.whole_body_cloned[index]) continue;
            const captures = self.program.typedLocalSpan(self.program.getFnAt(index).captures);
            for (0..captures.len) |capture_index| {
                const local = GuardedList.at(captures, capture_index).local;
                var declared = false;
                for (original_captures) |original| {
                    if (original == local) {
                        declared = true;
                        break;
                    }
                }
                if (!declared) {
                    Common.invariant("rewritten fn gained a capture its source did not declare");
                }
            }
        }
    }

    /// Debug-only: every `.local` reference in a rewritten body—mirroring
    /// the reference forms the capture walk consumes, so an
    /// `uninitialized_payload` condition is exempt exactly as it is there—
    /// must resolve to an in-body binding, a function argument, or a
    /// recomputed capture. A
    /// value-substituting rewrite that leaves a reference resolving to a
    /// vanished binding produces no diagnostic until code generation reads an
    /// undeclared register; this walk turns that whole class into a
    /// deterministic panic in every Debug suite. A body cloned or specialized
    /// by this pass is checked; original bodies left in place are the lift
    /// output, already covered by their own invariants.
    fn verifyRewrittenBodyLocals(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        if (!std.debug.runtime_safety) return;
        for (0..self.program.fnCount()) |index| {
            const rewritten = index >= original_fn_count or
                (index < self.whole_body_cloned.len and self.whole_body_cloned[index]);
            if (!rewritten) continue;
            const func = self.program.getFnAt(index);
            const body = switch (func.body) {
                .roc => |expr| expr,
                .hosted => continue,
            };
            var validator = BodyLocalScope{
                .program = self.program,
                .allocator = self.allocator,
                .fn_index = index,
                .bound = collections.DenseMap(Ast.LocalId, u32).init(self.allocator),
                .joins = collections.DenseMap(Ast.JoinPointId, u32).init(self.allocator),
            };
            defer validator.bound.deinit();
            defer validator.joins.deinit();
            const args = self.program.typedLocalSpan(func.args);
            for (0..args.len) |arg_index| try validator.bind(GuardedList.at(args, arg_index).local);
            const captures = self.program.typedLocalSpan(func.captures);
            for (0..captures.len) |capture_index| try validator.bind(GuardedList.at(captures, capture_index).local);
            try validator.walkExpr(body);
        }
    }

    /// Apply the exact branch-append tail plan where the complete lowered loop
    /// and producer-stamped append topology prove the rewrite. The matcher is
    /// total and conservative; no preliminary body classification chooses a
    /// different cloning mode.
    fn specializeBranchAppendTails(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        for (0..original_fn_count) |index| {
            const fn_ = self.program.getFnAt(index);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const specialized = (try self.peelBranchAppendBody(body)) orelse continue;
            self.program.setFnAt(index, .{
                .symbol = fn_.symbol,
                .source = fn_.source,
                .signature = fn_.signature,
                .args = fn_.args,
                .captures = fn_.captures,
                .body = .{ .roc = specialized },
                .ret = fn_.ret,
            });
            try self.refreshPreCloneSource(index);
        }
    }

    /// Replace the paired source body and size during the one authoritative
    /// pre-clone rewrite. After this phase both fields remain immutable, so a
    /// clone can never charge one body while reading another.
    fn refreshPreCloneSource(self: *Pass, index: usize) Allocator.Error!void {
        if (index >= self.plans.len) Common.invariant("SpecConstr source refresh received a generated function");
        const body = self.program.getFnAt(index).body;
        self.plans[index].source = .{
            .body = body,
            .size = try fnBodySizeWithin(self.allocator, self.program, body, spec_constr_body_expr_threshold),
        };
    }

    fn copyProcDebugName(self: *Pass, source_symbol: Common.Symbol, target_symbol: Common.Symbol) Allocator.Error!void {
        if (self.program.procDebugName(source_symbol)) |name| {
            try self.program.setProcDebugName(target_symbol, name);
        }
    }

    /// Argument uses flow from a callee to its callers: a caller's argument
    /// becomes used when it is passed in a position the callee uses. One walk
    /// over every body records the direct callers of each function and the
    /// functions whose uses it changed; afterwards only the callers of a
    /// changed function are walked again, until no walk changes anything.
    fn collectArgUses(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        const callers = try self.allocator.alloc(std.ArrayList(Ast.FnId), original_fn_count);
        defer {
            for (callers) |*list| list.deinit(self.allocator);
            self.allocator.free(callers);
        }
        for (callers) |*list| list.* = .empty;
        const changed_fns = try self.allocator.alloc(bool, original_fn_count);
        defer self.allocator.free(changed_fns);
        @memset(changed_fns, false);
        self.arg_use_callers = callers;
        defer self.arg_use_callers = null;
        for (0..original_fn_count) |index| {
            const body = switch (self.plans[index].source.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            var changed = false;
            try self.markArgUsesInExpr(fn_id, body, &changed);
            changed_fns[index] = changed;
        }
        self.arg_use_callers = null;
        var pending = std.ArrayList(Ast.FnId).empty;
        defer pending.deinit(self.allocator);
        const queued = try self.allocator.alloc(bool, original_fn_count);
        defer self.allocator.free(queued);
        @memset(queued, false);
        for (changed_fns, 0..) |changed, index| {
            if (!changed) continue;
            for (callers[index].items) |caller| {
                const raw = @intFromEnum(caller);
                if (queued[raw]) continue;
                queued[raw] = true;
                try pending.append(self.allocator, caller);
            }
        }
        while (pending.pop()) |fn_id| {
            const raw = @intFromEnum(fn_id);
            queued[raw] = false;
            const body = switch (self.plans[raw].source.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            var changed = false;
            try self.markArgUsesInExpr(fn_id, body, &changed);
            if (!changed) continue;
            for (callers[raw].items) |caller| {
                const caller_raw = @intFromEnum(caller);
                if (queued[caller_raw]) continue;
                queued[caller_raw] = true;
                try pending.append(self.allocator, caller);
            }
        }
    }

    fn collectCallPatterns(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        var index: usize = 0;
        while (index < original_fn_count) : (index += 1) {
            const body = switch (self.plans[index].source.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            if (!self.phaseAdmits(.discovery, fn_id)) continue;
            try self.collectCallPatternsInExpr(body);
        }
    }

    /// The syntax-directed collector above cannot see that a direct-call
    /// argument is known when it is first named by a `let`. Walk with the
    /// cloner's substitution environment so those calls still reserve workers.
    fn collectValueAwareCallPatterns(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        try self.runIndependentPhase(.discovery, original_fn_count);
    }

    fn reserveSpecIds(self: *Pass) Allocator.Error!void {
        for (self.plans, 0..) |*plan, source_index| {
            const source_fn = self.program.getFnAt(source_index);
            for (plan.specs.items) |*spec| {
                const symbol = self.symbols.fresh();
                const fn_id = try self.program.addFn(.{
                    .symbol = symbol,
                    .source = source_fn.source,
                    .root_identity = source_fn.root_identity,
                    .spec_constr_pattern = try patternDigest(self.program, spec.pattern),
                    .args = .empty(),
                    .captures = source_fn.captures,
                    .body = .hosted,
                    .ret = source_fn.ret,
                });
                spec.fn_id = fn_id;
                try self.copyProcDebugName(source_fn.symbol, symbol);
            }
        }
    }

    fn createSpecializations(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        var wrote_spec = true;
        while (wrote_spec) {
            wrote_spec = false;
            for (0..original_fn_count) |index| {
                const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
                var spec_index: usize = 0;
                while (spec_index < self.plans[index].specs.items.len) : (spec_index += 1) {
                    if (self.plans[index].specs.items[spec_index].written) continue;

                    self.plans[index].specs.items[spec_index].written = true;
                    try self.writeSpecialization(fn_id, spec_index);
                    wrote_spec = true;
                }
            }
        }
    }

    /// Mark the arguments of `fn_id` that `expr_id` uses in a shape-relevant
    /// position. Every effect is a monotone flag or a deduplicated caller
    /// edge for `fn_id`, so the walk visits positions in any order on its
    /// own work stack.
    fn markArgUsesInExpr(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId, changed: *bool) Allocator.Error!void {
        var stack: std.ArrayList(Ast.ExprChild) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .expr = expr_id });
        while (stack.pop()) |child| switch (child) {
            .stmt => |stmt_id| try Ast.appendStmtChildren(self.allocator, self.program, stmt_id, &stack),
            .expr => |id| {
                switch (self.program.getExpr(id).data) {
                    .comptime_value => continue,
                    .lambda,
                    .def_ref,
                    .fn_def,
                    => Common.invariant("pre-lift function expression reached call-pattern specialization"),
                    .call_proc => |call| if (Ast.localDirectCallee(call)) |callee| {
                        const callee_raw = @intFromEnum(callee);
                        if (callee_raw < self.plans.len) {
                            if (self.arg_use_callers) |callers| {
                                const list = &callers[callee_raw];
                                if (list.items.len == 0 or list.items[list.items.len - 1] != fn_id) {
                                    try list.append(self.allocator, fn_id);
                                }
                            }
                            const args = self.program.exprSpan(call.args);
                            const callee_uses = self.plans[callee_raw].used_args;
                            if (args.len != callee_uses.len) Common.invariant("direct call arity differed from lifted function arity while propagating argument uses");
                            for (0..args.len) |index| {
                                if (callee_uses[index]) self.markArgUseIfLocal(fn_id, GuardedList.at(args, index), changed);
                            }
                        }
                    },
                    .field_access => |field| self.markArgUseIfLocal(fn_id, field.receiver, changed),
                    .tuple_access => |access| self.markArgUseIfLocal(fn_id, access.tuple, changed),
                    .match_ => |match| self.markArgUseIfLocal(fn_id, match.scrutinee, changed),
                    .loop_ => |loop| {
                        // A loop-carried argument is a shape-relevant use: the
                        // split scalarizes the slot only when the entry shape is
                        // known, so a caller must expose the construction it
                        // passes here.
                        const initial_values = self.program.exprSpan(loop.initial_values);
                        for (0..initial_values.len) |index| self.markArgUseIfLocal(fn_id, GuardedList.at(initial_values, index), changed);
                    },
                    // Retained locals describe ownership liveness, not
                    // value-shape demand. They follow any substitution
                    // selected by operational consumers, but must never cause
                    // argument specialization; the standard child list
                    // excludes them.
                    .join_point => {},
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .low_level, .structural_eq, .structural_hash, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
                }
                try Ast.appendChildren(self.allocator, self.program, id, &stack);
            },
        };
    }

    fn markArgUseIfLocal(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId, changed: *bool) void {
        const local = localExpr(self.program, expr_id) orelse return;
        const args = self.program.typedLocalSpan(self.program.getFn(fn_id).args);
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (arg.local == local) {
                const used = &self.plans[@intFromEnum(fn_id)].used_args[index];
                if (!used.*) {
                    used.* = true;
                    changed.* = true;
                }
                return;
            }
        }
    }

    /// Record the call pattern of every direct call in `expr_id`, children
    /// before the call that uses them, in source order.
    fn collectCallPatternsInExpr(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!void {
        const Visitor = struct {
            pass: *Pass,

            pub fn enterExpr(visitor: @This(), id: Ast.ExprId) Allocator.Error!Ast.ExprWalk {
                return switch (visitor.pass.program.getExpr(id).data) {
                    .comptime_value => .skip,
                    .lambda,
                    .def_ref,
                    .fn_def,
                    => Common.invariant("pre-lift function expression reached call-pattern specialization"),
                    .call_proc => |call| if (Ast.localDirectCallee(call)) |callee|
                        (if (@intFromEnum(callee) < visitor.pass.plans.len) .descend_then_exit else .descend)
                    else
                        .descend,
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
                };
            }

            pub fn exitExpr(visitor: @This(), id: Ast.ExprId) Allocator.Error!void {
                const call = visitor.pass.program.getExpr(id).data.call_proc;
                const callee = Ast.localDirectCallee(call) orelse
                    Common.invariant("call-pattern walk exited a call without a direct callee");
                try visitor.pass.recordCallPattern(callee, call.args);
            }
        };
        try Ast.walkExprs(self.allocator, self.program, .{ .expr = expr_id }, Visitor{ .pass = self });
    }

    fn newSpecAdmission(self: *const Pass, raw: usize) SpecAdmission {
        if (self.discovery_admission) |snapshot| return snapshot[raw];
        if (!self.plans[raw].source.size.admits()) return .denied_body_size;
        if (self.plans[raw].specs.items.len >= spec_constr_specialization_count) return .denied_spec_count;
        return .admitted;
    }

    const InlineSourceBody = struct {
        expr: Ast.ExprId,
        size: BodySize,
    };

    fn sourceBody(self: *const Pass, fn_id: Ast.FnId) Ast.FnBody {
        const raw = @intFromEnum(fn_id);
        return if (raw < self.plans.len) self.plans[raw].source.body else self.program.getFn(fn_id).body;
    }

    /// Return the body and the work charged for cloning it as one value. Keeping
    /// these inseparable prevents a mutable output body from being admitted
    /// against an original source body's smaller size.
    fn inlineSourceBody(self: *const Pass, fn_id: Ast.FnId) Allocator.Error!?InlineSourceBody {
        const raw = @intFromEnum(fn_id);
        const body = switch (self.sourceBody(fn_id)) {
            .roc => |body| body,
            .hosted => return null,
        };
        const size = if (raw < self.plans.len)
            self.plans[raw].source.size
        else
            try exprBodySizeWithin(self.allocator, self.program, body, spec_constr_body_expr_threshold);
        return .{ .expr = body, .size = size };
    }

    fn recordCallPattern(self: *Pass, fn_id: Ast.FnId, args_span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        if (self.newSpecAdmission(raw) != .admitted) return;
        const args = try GuardedList.dupe(self.allocator, Ast.ExprId, self.program.exprSpan(args_span));
        defer self.allocator.free(args);
        const fn_args = self.program.typedLocalSpan(self.program.getFnAt(raw).args);
        if (args.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const shapes = try self.arena.allocator().alloc(Shape, args.len);
        var has_constructor = false;

        for (args, 0..) |arg, index| {
            if (self.plans[raw].used_args[index]) {
                if (try self.constructorShape(arg)) |shape| {
                    shapes[index] = shape;
                    has_constructor = true;
                    continue;
                }
            }
            shapes[index] = .{ .any = self.program.getExpr(arg).ty };
        }

        if (!has_constructor) return;

        const pattern: CallPattern = .{ .args = shapes };
        for (self.plans[raw].specs.items) |spec| {
            if (try patternEql(self.program, spec.pattern, pattern)) return;
        }

        try self.plans[raw].specs.append(self.allocator, .{
            .pattern = pattern,
        });
    }

    fn recordCallPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return;
        if (self.newSpecAdmission(raw) != .admitted) return;

        const pattern = (try self.callPatternForValues(fn_id, values)) orelse return;
        if (self.pattern_requests) |requests| {
            const arena = requests.arena.allocator();
            try requests.items.append(arena, .{
                .fn_id = fn_id,
                .pattern = .{ .args = try copyShapes(arena, pattern.args) },
            });
            return;
        }
        for (self.plans[raw].specs.items) |spec| {
            if (try patternEql(self.program, spec.pattern, pattern)) return;
        }

        try self.plans[raw].specs.append(self.allocator, .{
            .pattern = pattern,
        });
    }

    fn ensureCallPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!void {
        if (self.borrowed_worker) Common.invariant("SpecConstr body worker attempted specialization admission");
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return;
        if (!self.plans[raw].source.size.admits()) return;

        const pattern = (try self.callPatternForValues(fn_id, values)) orelse return;
        for (self.plans[raw].specs.items) |spec| {
            if (try patternEql(self.program, spec.pattern, pattern)) return;
        }
        if (self.newSpecAdmission(raw) != .admitted) return;

        const source_fn = self.program.getFnAt(raw);
        const symbol = self.symbols.fresh();
        const fn_id_reserved = try self.program.addFn(.{
            .symbol = symbol,
            .source = source_fn.source,
            .root_identity = source_fn.root_identity,
            .spec_constr_pattern = try patternDigest(self.program, pattern),
            .signature = null,
            .args = .empty(),
            .captures = source_fn.captures,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        try self.plans[raw].specs.append(self.allocator, .{
            .pattern = pattern,
            .fn_id = fn_id_reserved,
        });
        try self.copyProcDebugName(source_fn.symbol, symbol);
    }

    fn callPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!?CallPattern {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return null;

        const fn_args = self.program.typedLocalSpan(self.program.getFnAt(raw).args);
        if (values.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const shapes = try self.arena.allocator().alloc(Shape, values.len);
        var has_constructor = false;
        for (values, 0..) |value, index| {
            if (self.plans[raw].used_args[index]) {
                switch (try self.shapeFromValue(value)) {
                    .proven => |shape| {
                        shapes[index] = shape;
                        has_constructor = true;
                        continue;
                    },
                    .disproven, .unknown_budget_exhausted => {},
                }
            }
            shapes[index] = .{ .any = valueType(self.program, value) };
        }

        return if (has_constructor) .{ .args = shapes } else null;
    }

    fn writeSpecialization(self: *Pass, source_fn_id: Ast.FnId, spec_index: usize) Common.LowerError!void {
        const source_fn = self.program.getFn(source_fn_id);
        const spec = &self.plans[@intFromEnum(source_fn_id)].specs.items[spec_index];

        const spec_fn_id = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before cloning");
        const symbol = self.program.getFn(spec_fn_id).symbol;

        var cloner = Cloner.init(self, source_fn_id, spec.pattern);
        defer cloner.deinit();
        cloner.inline_calls = switch (self.clone_inlining) {
            .all_calls => .all,
            .iterator_fusion => .iterator_fusion,
        };

        try cloner.inline_stack.append(self.allocator, .{ .fn_id = source_fn_id, .known_size = null });
        defer {
            const popped = cloner.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow while writing specialization");
            if (popped.fn_id != source_fn_id) Common.invariant("call-pattern inline stack was corrupted while writing specialization");
        }

        const outer_shapes = self.program.beginFnShapes(spec_fn_id);
        const args = try cloner.buildArgs();
        const body: Ast.FnBody = switch (self.sourceBody(source_fn_id)) {
            .roc => |body_expr| .{ .roc = try cloner.cloneExpr(body_expr) },
            .hosted => Common.invariant("hosted function had a call-pattern specialization"),
        };
        const shapes = self.program.finishFnShapes(outer_shapes);

        self.program.setFn(spec_fn_id, .{
            .symbol = symbol,
            .source = source_fn.source,
            .root_identity = source_fn.root_identity,
            .spec_constr_pattern = try patternDigest(self.program, spec.pattern),
            .signature = null,
            .args = args,
            .captures = source_fn.captures,
            .body = body,
            .ret = source_fn.ret,
            .shapes = shapes,
        });
        try self.copyProcDebugName(source_fn.symbol, symbol);
    }

    fn rewriteExistingCalls(self: *Pass) Allocator.Error!void {
        const done = try self.allocator.alloc(bool, self.program.exprCount());
        defer self.allocator.free(done);
        @memset(done, false);

        // Original bodies are immutable clone sources. Their output calls are
        // rewritten when rewriteAllOriginalBodies clones them below; only the
        // already-emitted specialization bodies need this in-place update.
        const fn_count = self.program.fnCount();
        for (self.plans.len..fn_count) |index| {
            const fn_ = self.program.getFnAt(index);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            try self.rewriteCallsInExpr(body, done);
        }
    }

    /// Normalize every original body once through the demand-directed value
    /// cloner. Structural consumers decide locally which producer calls must be
    /// exposed, so pass routing never depends on whole-body shape scans.
    fn rewriteAllOriginalBodies(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        for (0..original_fn_count) |index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.cloneFnBodyInPlace(fn_id);
        }
    }

    fn aggregateLoopBindingIsPartiallyUsedInBlockTail(
        self: *Pass,
        pat_id: Ast.PatId,
        loop_id: Ast.ExprId,
        statements: Ast.ProgramSpanBorrow(Ast.StmtId, "stmt_ids"),
        tail_start: usize,
        final_expr: Ast.ExprId,
    ) Allocator.Error!bool {
        const pat_data = self.program.getPat(pat_id).data;
        if (pat_data != .bind) return false;
        const local = pat_data.bind;
        const loop_type = self.program.types.get(self.program.getExpr(loop_id).ty);
        if (loop_type != .tuple) return false;
        const items = self.program.types.span(loop_type.tuple);
        if (items.len < 2) return false;
        const used = try self.allocator.alloc(bool, items.len);
        defer self.allocator.free(used);
        @memset(used, false);
        for (tail_start..statements.len) |index| {
            if (!try collectTupleLocalDemand(self.allocator, self.program, local, .{ .stmt = GuardedList.at(statements, index) }, used)) return false;
        }
        if (!try collectTupleLocalDemand(self.allocator, self.program, local, .{ .expr = final_expr }, used)) return false;
        const used_count = std.mem.count(bool, used, &.{true});
        return used_count != 0 and used_count != items.len;
    }

    fn tuplePatternIsPartiallyUsedInBlockTail(
        self: *Pass,
        pat_id: Ast.PatId,
        statements: Ast.ProgramSpanBorrow(Ast.StmtId, "stmt_ids"),
        tail_start: usize,
        final_expr: Ast.ExprId,
    ) Allocator.Error!bool {
        const pat_data = self.program.getPat(pat_id).data;
        if (pat_data != .tuple) return false;
        const items = self.program.patSpan(pat_data.tuple);
        if (items.len < 2) return false;
        var used: usize = 0;
        for (0..items.len) |index| {
            const item_data = self.program.getPat(GuardedList.at(items, index)).data;
            if (item_data != .bind) return false;
            const local = item_data.bind;
            var count = try localUseCountInExpr(self.allocator, self.program, local, final_expr);
            for (tail_start..statements.len) |stmt_index| {
                count += try localUseCount(self.allocator, self.program, local, .{ .stmt = GuardedList.at(statements, stmt_index) });
            }
            if (count != 0) used += 1;
        }
        return used != 0 and used != items.len;
    }

    /// Whether a function body holds a `for` loop over an iterator named by an
    /// enclosing `if`/`match` binding—the branch-chosen (tier-two) shape. The
    /// loop's first carried value is an identity-style construction over a single
    /// local, and that local is bound in scope to a branch expression whose arms
    /// are the differently-shaped iterators the loop must specialize over.
    const IteratorLoopParts = struct {
        /// The local fed to the iterator constructor in the iterator slot's
        /// initial value—the branch-bound source the loop consumes.
        source_local: Ast.LocalId,
        /// The whole iterator-slot initial expression (a construction over
        /// `source_local`), reused to build the base iteration.
        iter_init: Ast.ExprId,
        /// Number of carried accumulators (0 or 1).
        carry_count: usize,
        /// The accumulator loop parameter (valid when `carry_count == 1`).
        carry_param: Ast.LocalId,
        /// The accumulator loop parameter's type (valid when `carry_count == 1`).
        carry_ty: Type.TypeId,
        /// The type each per-element application produces: the accumulator type
        /// for a fold, or a zero-sized unit for a side-effecting drive.
        value_ty: Type.TypeId,
        /// The `One(...)` payload's item pattern—bound to each pulled element.
        item_pat: Ast.PatId,
        /// The `One(...)` arm body, ending in a `continue` whose accumulator
        /// value (when carried) is the per-element result.
        one_body: Ast.ExprId,
        /// The local bound by the `One(...)` payload's `rest` field.
        rest_local: Ast.LocalId,
    };

    /// A branch arm's iterator source reduced to a shared base plus the finite
    /// items an `append` chain adds after it, in yield order.
    const ArmChain = struct {
        base: Ast.LocalId,
        items: []Ast.ExprId,
    };

    /// Rewrite a `for` over a branch-chosen `append`-style iterator into one
    /// loop over the shared base source followed by a branch-dispatched tail
    /// that replays the loop body for each appended item. The base loop is
    /// scalarized by the whole-body clone that runs afterward; the tail folds
    /// the same per-element computation over the taken arm's appended items, in
    /// exactly the unfused pull order (base elements, then appended items in arm
    /// order). Returns null (keeping the per-branch split) for any shape it
    /// cannot faithfully replay.
    fn peelBranchAppendBody(self: *Pass, body: Ast.ExprId) Common.LowerError!?Ast.ExprId {
        const body_expr = self.program.getExpr(body);
        if (body_expr.data != .block) return null;
        const block = body_expr.data.block;
        const stmts = try GuardedList.dupe(self.allocator, Ast.StmtId, self.program.stmtSpan(block.statements));
        defer self.allocator.free(stmts);

        // Locate the driving loop: a statement whose value/expression is a loop.
        // A one-carry loop that binds its result (a fold) rebinds that result
        // through the tail; a zero-carry loop driven for effect (a search) runs
        // the tail as an effect after it.
        var loop_stmt_index: ?usize = null;
        var loop_expr_id: Ast.ExprId = undefined;
        var result_local: ?Ast.LocalId = null;
        for (stmts, 0..) |stmt_id, index| {
            switch (self.program.getStmt(stmt_id)) {
                .let_ => |let_| {
                    if (self.program.getExpr(let_.value).data != .loop_) continue;
                    const pat_data = self.program.getPat(let_.pat).data;
                    if (pat_data != .bind) continue;
                    result_local = pat_data.bind;
                    loop_stmt_index = index;
                    loop_expr_id = let_.value;
                },
                .expr => |e| {
                    if (self.program.getExpr(e).data != .loop_) continue;
                    result_local = null;
                    loop_stmt_index = index;
                    loop_expr_id = e;
                },
                .uninitialized, .expect, .dbg, .return_, .crash, .checked_error => continue,
            }
            if (loop_stmt_index != null) break;
        }
        const li = loop_stmt_index orelse return null;

        const loop_parts = (try self.matchIteratorLoopParts(loop_expr_id)) orelse return null;
        if (try localUseCountInExpr(self.allocator, self.program, loop_parts.source_local, body) != 1) return null;
        // A fold's result feeds the block's final expression directly, so the
        // transformed fold value can take its place.
        if (loop_parts.carry_count == 1) {
            const rl = result_local orelse return null;
            if (localExpr(self.program, block.final_expr) != rl) return null;
            if (try localUseCountInExpr(self.allocator, self.program, rl, body) != 1) return null;
        } else if (result_local != null) {
            return null;
        }

        // Find the branch that binds the source, and confirm its arms share one
        // base source reached by unwrapping append adapter state.
        var collision_stmt_index: ?usize = null;
        var branch_expr_id: Ast.ExprId = undefined;
        for (stmts, 0..) |stmt_id, index| {
            const stmt = self.program.getStmt(stmt_id);
            if (stmt != .let_) continue;
            const let_ = stmt.let_;
            const pat_data = self.program.getPat(let_.pat).data;
            if (pat_data != .bind) continue;
            const bound = pat_data.bind;
            if (bound != loop_parts.source_local) continue;
            const value_data = self.program.getExpr(let_.value).data;
            if (value_data != .if_ and value_data != .match_) return null;
            collision_stmt_index = index;
            branch_expr_id = let_.value;
            break;
        }
        const ci = collision_stmt_index orelse return null;

        const base_local = (try self.sharedArmBase(branch_expr_id)) orelse return null;
        // This implementation replays the branch discriminator and appended
        // item skeleton after the shared base loop. That is legal only when
        // every replayed expression is structurally work-free. Opaque work is
        // left to the general ordered value rewrite, which keeps it at the
        // original branch position.
        if (try self.branchAppendPlanIsWorkFree(branch_expr_id) != .proven) return null;

        // Build the loop so its iterator slot iterates the shared base.
        const new_loop = (try self.buildLoopOverBase(loop_expr_id, base_local, loop_parts)) orelse return null;

        // A fold threads the base loop's result into the tail; a search runs the
        // tail for effect only.
        var carry_start: ?Ast.ExprId = null;
        var base_loop_stmt: Ast.StmtId = undefined;
        var result_stmt: ?Ast.StmtId = null;
        if (loop_parts.carry_count == 1) {
            const temp = try self.program.addLocal(self.symbols.fresh(), loop_parts.carry_ty);
            const temp_bind = try self.program.addPat(.{ .ty = loop_parts.carry_ty, .data = .{ .bind = temp } });
            base_loop_stmt = try self.program.addStmt(.{ .let_ = .{ .pat = temp_bind, .value = new_loop } });
            carry_start = try self.program.addExpr(.{ .ty = loop_parts.carry_ty, .data = .{ .local = temp } });
        } else {
            base_loop_stmt = try self.program.addStmt(.{ .expr = new_loop });
        }

        // The tail replays the branch structure, each arm's body replaced by the
        // per-element computation run over that arm's appended items.
        const tail = (try self.buildTailDispatch(branch_expr_id, base_local, carry_start, loop_parts)) orelse return null;

        if (loop_parts.carry_count == 1) {
            const result_let = self.program.getStmt(stmts[li]).let_;
            result_stmt = try self.program.addStmt(.{ .let_ = .{ .pat = result_let.pat, .value = tail } });
        } else {
            result_stmt = try self.program.addStmt(.{ .expr = tail });
        }

        var new_stmts = std.ArrayList(Ast.StmtId).empty;
        defer new_stmts.deinit(self.allocator);
        for (stmts, 0..) |stmt_id, index| {
            if (index == ci) continue; // the branch binding is replayed as the tail
            if (index == li) {
                try new_stmts.append(self.allocator, base_loop_stmt);
                try new_stmts.append(self.allocator, result_stmt.?);
                continue;
            }
            try new_stmts.append(self.allocator, stmt_id);
        }

        return try self.program.addExpr(.{ .ty = body_expr.ty, .data = .{ .block = .{
            .statements = try self.program.addStmtSpan(new_stmts.items),
            .final_expr = block.final_expr,
        } } });
    }

    fn stripArmBlock(self: *Pass, expr_id: Ast.ExprId) Ast.ExprId {
        var current = expr_id;
        while (true) {
            const expr = self.program.getExpr(current);
            if (expr.data != .block) return current;
            const block = expr.data.block;
            if (self.program.stmtSpan(block.statements).len != 0) return current;
            current = block.final_expr;
        }
    }

    const DirectCall = struct {
        fn_id: Ast.FnId,
        args: Ast.ProgramSpanBorrow(Ast.ExprId, "expr_ids"),
        iterator_procedure: ?check.StaticDispatchRegistry.IteratorProcedureId,
    };

    fn asDirectCall(self: *Pass, expr_id: Ast.ExprId) ?DirectCall {
        const expr = self.program.getExpr(expr_id);
        if (expr.data != .call_proc) return null;
        const call = expr.data.call_proc;
        const fn_id = Ast.localDirectCallee(call) orelse return null;
        return .{
            .fn_id = fn_id,
            .args = self.program.exprSpan(call.args),
            .iterator_procedure = call.iterator_procedure,
        };
    }

    /// Match the lowered desugared `for` loop shape, extracting the pieces the
    /// peel threads. Returns null for any other loop.
    fn matchIteratorLoopParts(self: *Pass, loop_expr_id: Ast.ExprId) Common.LowerError!?IteratorLoopParts {
        const loop = self.program.getExpr(loop_expr_id).data.loop_;
        const params = self.program.typedLocalSpan(loop.params);
        const initials = self.program.exprSpan(loop.initial_values);
        // Slot 0 is the iterator; at most one accumulator follows it.
        if (params.len < 1 or params.len > 2 or params.len != initials.len) return null;
        const carry_count = params.len - 1;

        const iter_param = GuardedList.at(params, 0).local;
        const carry_param = if (carry_count == 1) GuardedList.at(params, 1).local else undefined;

        // `Iter.iter` is an identity for a producer-authored private iterator,
        // so Monotype may lower the iterator slot's initial value directly to
        // the branch-bound local. Public/custom iterables retain the ordinary
        // one-argument construction call.
        const iter_init = GuardedList.at(initials, 0);
        const source_local = if (localExpr(self.program, iter_init)) |local|
            local
        else blk: {
            const iter_call = self.asDirectCall(iter_init) orelse return null;
            if (iter_call.args.len != 1) return null;
            break :blk localExpr(self.program, GuardedList.at(iter_call.args, 0)) orelse return null;
        };

        const match_expr = self.program.getExpr(self.stripArmBlock(loop.body));
        if (match_expr.data != .match_) return null;
        const match = match_expr.data.match_;

        // The scrutinee pulls the next item either through the public method
        // specialization or directly through a generated-private iterator's
        // producer-authored step field.
        if (self.asDirectCall(match.scrutinee)) |next_call| {
            if (next_call.args.len != 1) return null;
            if (localExpr(self.program, GuardedList.at(next_call.args, 0)) != iter_param) return null;
        } else if (!self.isExactGeneratedIteratorNextCall(match.scrutinee, iter_param)) {
            return null;
        }

        var item_pat: ?Ast.PatId = null;
        var one_body: Ast.ExprId = undefined;
        var rest_local: Ast.LocalId = undefined;
        const branches = self.program.branchSpan(match.branches);
        for (0..branches.len) |branch_index| {
            const branch = GuardedList.at(branches, branch_index);
            if (branch.guard != null or branch.bindings.len != 0) return null;
            const pat = self.program.getPat(branch.pat);
            if (pat.data != .tag) return null;
            const tag = pat.data.tag;
            const payloads = self.program.patSpan(tag.payloads);
            if (payloads.len == 0) {
                // Exhausted arm: breaks, carrying the accumulator unchanged.
                const broke = self.stripArmBlock(branch.body);
                const broke_data = self.program.getExpr(broke).data;
                if (broke_data != .break_) return null;
                const break_val = broke_data.break_;
                if (carry_count == 0) {
                    if (break_val != null) return null;
                } else {
                    const bv = break_val orelse return null;
                    if (localExpr(self.program, bv) != carry_param) return null;
                }
                continue;
            }
            if (payloads.len != 1) return null;
            const payload_data = self.program.getPat(GuardedList.at(payloads, 0)).data;
            if (payload_data != .record) return null;
            const record_fields = self.program.recordDestructSpan(payload_data.record);
            const cont = (self.tailContinueValues(branch.body)) orelse return null;
            if (cont.len != params.len) return null;
            const cont_rest = localExpr(self.program, GuardedList.at(cont, 0)) orelse return null;

            if (record_fields.len == 1) {
                // Skip arm: advances the iterator, accumulator unchanged.
                if (carry_count == 1 and localExpr(self.program, GuardedList.at(cont, 1)) != carry_param) return null;
                const only = GuardedList.at(record_fields, 0);
                if (self.bindLocalOf(only.pattern) != cont_rest) return null;
                continue;
            }
            if (record_fields.len != 2) return null;
            // One arm: yields an item and advances; its continue carries the
            // per-element accumulator result.
            var this_item_pat: ?Ast.PatId = null;
            var found_rest = false;
            for (0..record_fields.len) |field_index| {
                const field = GuardedList.at(record_fields, field_index);
                if (self.bindLocalOf(field.pattern)) |bound| {
                    if (bound == cont_rest) {
                        found_rest = true;
                        continue;
                    }
                }
                if (this_item_pat != null) return null;
                this_item_pat = field.pattern;
            }
            if (!found_rest or this_item_pat == null) return null;
            item_pat = this_item_pat;
            one_body = branch.body;
            rest_local = cont_rest;
        }

        const ip = item_pat orelse return null;
        const carry_ty = if (carry_count == 1) GuardedList.at(params, 1).ty else undefined;
        // A fold produces the accumulator type; a side-effecting drive produces
        // the loop's own (unit) result type. Reuse an existing type id—the
        // Monotype type store is frozen during this pass.
        const value_ty = if (carry_count == 1)
            carry_ty
        else
            self.program.getExpr(loop_expr_id).ty;
        return .{
            .source_local = source_local,
            .iter_init = iter_init,
            .carry_count = carry_count,
            .carry_param = carry_param,
            .carry_ty = carry_ty,
            .value_ty = value_ty,
            .item_pat = ip,
            .one_body = one_body,
            .rest_local = rest_local,
        };
    }

    fn isExactGeneratedIteratorNextCall(
        self: *Pass,
        expr_id: Ast.ExprId,
        iterator_local: Ast.LocalId,
    ) bool {
        const expr_data = self.program.getExpr(expr_id).data;
        if (expr_data != .call_value) return false;
        const call = expr_data.call_value;
        if (self.program.exprSpan(call.args).len != 0) return false;
        const callee_data = self.program.getExpr(call.callee).data;
        if (callee_data != .field_access) return false;
        const access = callee_data.field_access;
        if (localExpr(self.program, access.receiver) != iterator_local) return false;
        const iterator_ty = self.program.getLocal(iterator_local).ty;
        const iterator_type = self.program.types.get(iterator_ty);
        if (iterator_type != .named) return false;
        const named = iterator_type.named;
        const topology = named.def.iterator_topology orelse return false;
        if (access.segments.len != 1) return false;
        if (self.program.fieldAccessSegmentAt(access.segments, 0).field != topology.step_field) return false;
        const backing = named.backing orelse return false;
        if (backing.authority != .generated_private) return false;
        const backing_type = self.program.types.get(backing.ty);
        if (backing_type != .record) return false;
        const fields = self.program.types.fieldSpan(backing_type.record);
        const step_ty = typeFieldByName(fields, topology.step_field) orelse return false;
        if (!sameType(self.program, self.program.getExpr(call.callee).ty, step_ty)) return false;
        const step_type = self.program.types.get(step_ty);
        if (step_type != .func) return false;
        const function = step_type.func;
        return self.program.types.span(function.args).len == 0 and
            sameType(self.program, self.program.getExpr(expr_id).ty, function.ret);
    }

    fn bindLocalOf(self: *Pass, pat_id: Ast.PatId) ?Ast.LocalId {
        const data = self.program.getPat(pat_id).data;
        return if (data == .bind) data.bind else null;
    }

    /// The values of the `continue` at the tail position of a loop-body arm,
    /// or null when the arm's tail is not a plain `continue`.
    fn tailContinueValues(self: *Pass, expr_id: Ast.ExprId) ?Ast.ProgramSpanBorrow(Ast.ExprId, "expr_ids") {
        var current = expr_id;
        while (true) {
            const expr = self.program.getExpr(current);
            switch (expr.data) {
                .continue_ => |continue_| return self.program.exprSpan(continue_.values),
                .block => |block| current = block.final_expr,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return null,
            }
        }
    }

    /// The shared base local every arm of the source branch reduces to, or null
    /// when the arms do not share one base under append unwrapping.
    fn sharedArmBase(self: *Pass, branch_expr_id: Ast.ExprId) Common.LowerError!?Ast.LocalId {
        const expr = self.program.getExpr(branch_expr_id);
        var base: ?Ast.LocalId = null;
        if (expr.data == .if_) {
            const if_ = expr.data.if_;
            const branches = self.program.ifBranchSpan(if_.branches);
            for (0..branches.len) |branch_index| {
                const br = GuardedList.at(branches, branch_index);
                if (!try self.armBaseMatches(br.body, &base)) return null;
            }
            if (!try self.armBaseMatches(if_.final_else, &base)) return null;
        } else if (expr.data == .match_) {
            const match = expr.data.match_;
            const branches = self.program.branchSpan(match.branches);
            for (0..branches.len) |branch_index| {
                const br = GuardedList.at(branches, branch_index);
                if (br.guard != null or br.bindings.len != 0) return null;
                if (!try self.armBaseMatches(br.body, &base)) return null;
            }
        } else {
            return null;
        }
        return base;
    }

    fn armBaseMatches(self: *Pass, arm: Ast.ExprId, base: *?Ast.LocalId) Common.LowerError!bool {
        const chain = (try self.reduceArmChain(arm)) orelse return false;
        defer self.allocator.free(chain.items);
        if (base.*) |existing| {
            if (existing != chain.base) return false;
        } else {
            base.* = chain.base;
        }
        return true;
    }

    /// Reduce a branch arm's iterator source to its base local and the finite
    /// list of items appended after it, in yield order. Caller owns the items.
    fn reduceArmChain(self: *Pass, arm: Ast.ExprId) Common.LowerError!?ArmChain {
        // Walk inward from the last append to the base, collecting items in
        // reverse yield order.
        var items: std.ArrayList(Ast.ExprId) = .empty;
        errdefer items.deinit(self.allocator);
        var current = arm;
        while (true) {
            const stripped = self.stripArmBlock(current);
            if (localExpr(self.program, stripped)) |local| {
                std.mem.reverse(Ast.ExprId, items.items);
                return .{ .base = local, .items = try items.toOwnedSlice(self.allocator) };
            }
            const call = self.asDirectCall(stripped) orelse break;
            if (call.args.len != 2 or !self.callIsSuffixAppend(call)) break;
            try items.append(self.allocator, GuardedList.at(call.args, 1));
            current = GuardedList.at(call.args, 0);
        }
        items.deinit(self.allocator);
        return null;
    }

    fn branchAppendPlanIsWorkFree(self: *Pass, branch_expr_id: Ast.ExprId) Common.LowerError!ProofStatus {
        var budget: u32 = 4096;
        const data = self.program.getExpr(branch_expr_id).data;
        if (data == .if_) {
            const if_ = data.if_;
            const branches = self.program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const condition = try self.exprIsStructurallyWorkFree(branch.cond, &budget);
                if (condition != .proven) return condition;
                const body = try self.appendArmItemsAreWorkFree(branch.body, &budget);
                if (body != .proven) return body;
            }
            return try self.appendArmItemsAreWorkFree(if_.final_else, &budget);
        } else if (data == .match_) {
            const match = data.match_;
            const scrutinee = try self.exprIsStructurallyWorkFree(match.scrutinee, &budget);
            if (scrutinee != .proven) return scrutinee;
            const branches = self.program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const binding_proof = try self.stmtSpanIsStructurallyWorkFree(branch.bindings, &budget);
                if (binding_proof != .proven) return binding_proof;
                if (branch.guard) |guard| {
                    const guard_proof = try self.exprIsStructurallyWorkFree(guard, &budget);
                    if (guard_proof != .proven) return guard_proof;
                }
                const body = try self.appendArmItemsAreWorkFree(branch.body, &budget);
                if (body != .proven) return body;
            }
            return .proven;
        }
        return .disproven;
    }

    fn appendArmItemsAreWorkFree(self: *Pass, arm: Ast.ExprId, budget: *u32) Common.LowerError!ProofStatus {
        const chain = (try self.reduceArmChain(arm)) orelse return .disproven;
        defer self.allocator.free(chain.items);
        for (chain.items) |item| {
            const proof = try self.exprIsStructurallyWorkFree(item, budget);
            if (proof != .proven) return proof;
        }
        return .proven;
    }

    /// A source expression whose evaluation is only finite structural assembly,
    /// record-field reads, or tag-payload reads. In particular, this excludes
    /// every call, low-level op, loop, allocation-bearing collection literal,
    /// control transfer, and diagnostic operation. Exhaustion declines the rewrite.
    /// Subexpressions are visited in source order on a work stack; any
    /// disproven subexpression disproves the whole.
    fn exprIsStructurallyWorkFree(self: *Pass, expr_id: Ast.ExprId, budget: *u32) Allocator.Error!ProofStatus {
        var proof = ProofStatus.proven;
        var stack: std.ArrayList(Ast.ExprId) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, expr_id);
        while (stack.pop()) |id| {
            if (budget.* == 0) {
                proof = .unknown_budget_exhausted;
                continue;
            }
            budget.* -= 1;
            // Children are appended in source order, then reversed.
            const start = stack.items.len;
            switch (self.program.getExpr(id).data) {
                .local,
                .unit,
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .comptime_value,
                => {},
                .tuple => |items| try pushExprSpanWork(self.allocator, self.program, items, &stack),
                .record => |fields| {
                    const values = self.program.fieldExprSpan(fields);
                    for (0..values.len) |index| try stack.append(self.allocator, GuardedList.at(values, index).value);
                },
                .tag => |tag| try pushExprSpanWork(self.allocator, self.program, tag.payloads, &stack),
                .nominal => |backing| try stack.append(self.allocator, backing),
                .field_access => |field| try stack.append(self.allocator, field.receiver),
                .tuple_access => |access| try stack.append(self.allocator, access.tuple),
                .static_data_candidate => |candidate| try stack.append(self.allocator, candidate.runtime_expr),
                .typed_boundary => |boundary| try stack.append(self.allocator, boundary.value),
                .block => |block| {
                    if (self.program.stmtSpan(block.statements).len != 0) return .disproven;
                    try stack.append(self.allocator, block.final_expr);
                },
                .comptime_branch_taken => |taken| try stack.append(self.allocator, taken.body),
                .@"unreachable",
                .list,
                .record_update,
                .let_,
                .lambda,
                .def_ref,
                .fn_def,
                .fn_ref,
                .call_value,
                .call_proc,
                .low_level,
                .structural_eq,
                .structural_hash,
                .match_,
                .if_,
                .uninitialized,
                .uninitialized_payload,
                .if_initialized_payload,
                .try_sequence,
                .try_record_sequence,
                .loop_,
                .break_,
                .continue_,
                .join_point,
                .jump,
                .return_,
                .crash,
                .checked_error,
                .comptime_exhaustiveness_failed,
                .dbg,
                .expect_err,
                .literal_rejected,
                .expect,
                => return .disproven,
            }
            std.mem.reverse(Ast.ExprId, stack.items[start..]);
        }
        return proof;
    }

    fn stmtSpanIsStructurallyWorkFree(self: *Pass, span: Ast.Span(Ast.StmtId), budget: *u32) Allocator.Error!ProofStatus {
        const statements = self.program.stmtSpan(span);
        var proof = ProofStatus.proven;
        for (0..statements.len) |index| {
            const stmt_proof = switch (self.program.getStmt(GuardedList.at(statements, index))) {
                .let_ => |let_| if (let_.recursive) .disproven else try self.exprIsStructurallyWorkFree(let_.value, budget),
                .uninitialized => .proven,
                .expr, .expect, .dbg, .return_, .crash, .checked_error => .disproven,
            };
            proof = proofAnd(proof, stmt_proof);
            if (proof == .disproven) return .disproven;
        }
        return proof;
    }

    /// Whether this exact two-argument call is the checker-identified
    /// `Iter.append` procedure. Control-flow joins may select another private
    /// representation for its result, so the return type is not its checked
    /// procedure identity.
    fn callIsSuffixAppend(self: *Pass, call: DirectCall) bool {
        if (call.iterator_procedure != .append) return false;
        const raw = @intFromEnum(call.fn_id);
        if (raw >= self.program.fnCount()) return false;
        return self.program.typedLocalSpan(self.program.getFnAt(raw).args).len == 2;
    }

    /// Build the loop so its iterator slot iterates the shared base, keeping
    /// the accumulator slot and body unchanged.
    fn buildLoopOverBase(
        self: *Pass,
        loop_expr_id: Ast.ExprId,
        base_local: Ast.LocalId,
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        const loop_expr = self.program.getExpr(loop_expr_id);
        const loop = loop_expr.data.loop_;
        const iter_call_expr = self.program.getExpr(loop_parts.iter_init);
        const base_ty = self.program.getLocal(base_local).ty;

        // A retained source-level iterator constructor is monomorphic by this
        // stage. Reusing it with another representation is valid only when its
        // exact argument and result types already are the base type; otherwise
        // doing so would manufacture a call outside the specialization's ABI.
        if (iter_call_expr.data == .call_proc) {
            const iter_call = iter_call_expr.data.call_proc;
            const callee = Ast.localDirectCallee(iter_call) orelse return null;
            const callee_fn = self.program.getFn(callee);
            const callee_args = self.program.typedLocalSpan(callee_fn.args);
            if (callee_args.len != 1 or
                !sameType(self.program, GuardedList.at(callee_args, 0).ty, base_ty) or
                !sameType(self.program, callee_fn.ret, base_ty) or
                !sameType(self.program, iter_call_expr.ty, base_ty))
            {
                return null;
            }

            const base_ref = try self.program.addExpr(.{ .ty = base_ty, .data = .{ .local = base_local } });
            const new_iter_init = try self.program.addExpr(.{ .ty = base_ty, .data = .{ .call_proc = .{
                .callee = iter_call.callee,
                .args = try self.program.addExprSpan(&.{base_ref}),
                .iterator_procedure = iter_call.iterator_procedure,
                .captures = iter_call.captures,
                .is_cold = iter_call.is_cold,
            } } });

            const initials = try GuardedList.dupe(self.allocator, Ast.ExprId, self.program.exprSpan(loop.initial_values));
            defer self.allocator.free(initials);
            initials[0] = new_iter_init;
            return try self.program.addExpr(.{ .ty = loop_expr.ty, .data = .{ .loop_ = .{
                .params = loop.params,
                .initial_values = try self.program.addExprSpan(initials),
                .body = loop.body,
            } } });
        }

        if (localExpr(self.program, loop_parts.iter_init) == null) return null;
        return try self.buildExactGeneratedIteratorLoopOverBase(loop_expr_id, base_local, loop_parts);
    }

    /// Construct a loop over a producer-authored private iterator representation.
    /// The generated nominal carries the checker's exact iterator topology, so
    /// every field/tag read and every refined `rest` binder is selected
    /// from durable producer data rather than inferred from names or bodies.
    fn buildExactGeneratedIteratorLoopOverBase(
        self: *Pass,
        loop_expr_id: Ast.ExprId,
        base_local: Ast.LocalId,
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        const loop_expr = self.program.getExpr(loop_expr_id);
        const loop = loop_expr.data.loop_;
        const source_params = self.program.typedLocalSpan(loop.params);
        const source_initials = try GuardedList.dupe(
            self.allocator,
            Ast.ExprId,
            self.program.exprSpan(loop.initial_values),
        );
        defer self.allocator.free(source_initials);
        if (source_params.len == 0 or source_params.len != source_initials.len) return null;

        const base_ty = self.program.getLocal(base_local).ty;
        const base_type = self.program.types.get(base_ty);
        if (base_type != .named) return null;
        const base_named = base_type.named;
        if (base_named.def.iterator_representation != .minted) return null;
        const topology = base_named.def.iterator_topology orelse return null;
        const backing = base_named.backing orelse return null;
        if (backing.authority != .generated_private) return null;
        const backing_type = self.program.types.get(backing.ty);
        if (backing_type != .record) return null;
        const backing_fields = self.program.types.fieldSpan(backing_type.record);
        const step_fn_ty = typeFieldByName(backing_fields, topology.step_field) orelse return null;
        const step_fn_type = self.program.types.get(step_fn_ty);
        if (step_fn_type != .func) return null;
        const step_fn = step_fn_type.func;
        if (self.program.types.span(step_fn.args).len != 0) return null;
        const step_ty = step_fn.ret;

        const base_param = try self.program.addLocal(self.symbols.fresh(), base_ty);
        const base_param_ref = try self.program.addExpr(.{ .ty = base_ty, .data = .{ .local = base_param } });
        const step = try self.program.addExpr(.{ .ty = step_fn_ty, .data = .{ .field_access = .{
            .receiver = base_param_ref,
            .segments = try self.program.addFieldAccessSegmentSpan(&.{.{ .field = topology.step_field }}),
        } } });
        const next = try self.program.addExpr(.{ .ty = step_ty, .data = .{ .call_value = .{
            .callee = step,
            .args = Ast.Span(Ast.ExprId).empty(),
        } } });

        const source_match_expr = self.program.getExpr(self.stripArmBlock(loop.body));
        if (source_match_expr.data != .match_) return null;
        const source_match = source_match_expr.data.match_;
        const source_branches = self.program.branchSpan(source_match.branches);
        const branches = try self.allocator.alloc(Ast.Branch, source_branches.len);
        defer self.allocator.free(branches);
        for (0..source_branches.len) |index| {
            const source_branch = GuardedList.at(source_branches, index);
            if (source_branch.guard != null or source_branch.bindings.len != 0) return null;
            const source_pat = self.program.getPat(source_branch.pat);
            if (source_pat.data != .tag) return null;
            const source_tag = source_pat.data.tag;

            var renames = collections.DenseMap(Ast.LocalId, Ast.LocalId).init(self.allocator);
            defer renames.deinit();
            try renames.put(GuardedList.at(source_params, 0).local, base_param);

            const exact_pat = (try self.refineIteratorStepPattern(
                source_branch.pat,
                source_tag.name,
                step_ty,
                base_ty,
                topology,
                &renames,
            )) orelse return null;

            const body = if (source_tag.name == topology.done_tag)
                (try self.iteratorBaseDoneBody(source_branch.body, loop_parts)) orelse return null
            else
                (try self.cloneExprFresh(source_branch.body, &renames)) orelse return null;
            branches[index] = .{ .pat = exact_pat, .guard = null, .body = body };
        }

        const body = try self.program.addExpr(.{ .ty = source_match_expr.ty, .data = .{ .match_ = .{
            .scrutinee = next,
            .branches = try self.program.addBranchSpan(branches),
            .comptime_site = source_match.comptime_site,
        } } });

        const params = try self.allocator.alloc(Ast.TypedLocal, source_params.len);
        defer self.allocator.free(params);
        params[0] = .{ .local = base_param, .ty = base_ty };
        for (1..source_params.len) |index| params[index] = GuardedList.at(source_params, index);

        const initials = try self.allocator.alloc(Ast.ExprId, source_initials.len);
        defer self.allocator.free(initials);
        const base_ref = try self.program.addExpr(.{ .ty = base_ty, .data = .{ .local = base_local } });
        initials[0] = base_ref;
        for (1..source_initials.len) |index| initials[index] = GuardedList.at(source_initials, index);
        return try self.program.addExpr(.{ .ty = loop_expr.ty, .data = .{ .loop_ = .{
            .params = try self.program.addTypedLocalSpan(params),
            .initial_values = try self.program.addExprSpan(initials),
            .body = body,
        } } });
    }

    fn iteratorBaseDoneBody(
        self: *Pass,
        source_body: Ast.ExprId,
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        const source_break = self.program.getExpr(self.stripArmBlock(source_body));
        if (source_break.data != .break_) return null;
        const break_value = source_break.data.break_;
        if (loop_parts.carry_count == 0) {
            if (break_value != null) Common.invariant("zero-carry iterator loop broke with a value");
            return try self.program.addExpr(.{ .ty = source_break.ty, .data = .{ .break_ = null } });
        }
        const source = break_value orelse Common.invariant("iterator fold broke without its carried value");
        if (localExpr(self.program, source) != loop_parts.carry_param) {
            Common.invariant("iterator fold break did not carry its accumulator parameter");
        }
        const value = try self.program.addExpr(.{ .ty = loop_parts.carry_ty, .data = .{ .local = loop_parts.carry_param } });
        return try self.program.addExpr(.{ .ty = source_break.ty, .data = .{ .break_ = value } });
    }

    fn refineIteratorStepPattern(
        self: *Pass,
        source_pat_id: Ast.PatId,
        tag_name: names.TagNameId,
        step_ty: Type.TypeId,
        iterator_ty: Type.TypeId,
        topology: Type.IteratorTopology,
        renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId),
    ) Common.LowerError!?Ast.PatId {
        const source_pat = self.program.getPat(source_pat_id);
        if (source_pat.data != .tag) return null;
        const source_tag = source_pat.data.tag;
        if (source_tag.name != tag_name) return null;
        const exact_tag = typeTagByName(self.program, step_ty, tag_name) orelse return null;
        const exact_payload_tys = self.program.types.span(exact_tag.payloads);
        const source_payloads = self.program.patSpan(source_tag.payloads);

        if (tag_name == topology.done_tag) {
            if (source_payloads.len != 0 or exact_payload_tys.len != 0) return null;
            return try self.program.addPat(.{ .ty = step_ty, .data = .{ .tag = .{
                .name = tag_name,
                .payloads = Ast.Span(Ast.PatId).empty(),
            } } });
        }
        if (tag_name != topology.one_tag and tag_name != topology.skip_tag) return null;
        if (source_payloads.len != 1 or exact_payload_tys.len != 1) return null;

        const source_payload = self.program.getPat(GuardedList.at(source_payloads, 0));
        if (source_payload.data != .record) return null;
        const source_fields = self.program.recordDestructSpan(source_payload.data.record);
        const payload_ty = GuardedList.at(exact_payload_tys, 0);
        const payload_type = self.program.types.get(payload_ty);
        if (payload_type != .record) return null;
        const exact_fields = self.program.types.fieldSpan(payload_type.record);
        const exact_rest_ty = typeFieldByName(exact_fields, topology.rest_field) orelse return null;
        if (!sameType(self.program, exact_rest_ty, iterator_ty)) return null;

        const source_rest_pat = recordPatField(self.program, source_fields, topology.rest_field) orelse return null;
        const source_rest_data = self.program.getPat(source_rest_pat).data;
        if (source_rest_data != .bind) return null;
        const source_rest = source_rest_data.bind;
        const rest_local = try self.program.addLocal(self.symbols.fresh(), iterator_ty);
        try renames.put(source_rest, rest_local);
        const rest_pat = try self.program.addPat(.{ .ty = iterator_ty, .data = .{ .bind = rest_local } });

        var fields: [2]Ast.RecordDestruct = undefined;
        var field_count: usize = 0;
        if (tag_name == topology.one_tag) {
            const item_ty = typeFieldByName(exact_fields, topology.item_field) orelse return null;
            const source_item_pat = recordPatField(self.program, source_fields, topology.item_field) orelse return null;
            if (!sameType(self.program, self.program.getPat(source_item_pat).ty, item_ty)) return null;
            fields[field_count] = .{
                .name = topology.item_field,
                .pattern = (try self.clonePatFresh(source_item_pat, renames)) orelse return null,
            };
            field_count += 1;
        }
        fields[field_count] = .{ .name = topology.rest_field, .pattern = rest_pat };
        field_count += 1;
        if (source_fields.len != field_count) return null;

        const payload_pat = try self.program.addPat(.{ .ty = payload_ty, .data = .{
            .record = try self.program.addRecordDestructSpan(fields[0..field_count]),
        } });
        return try self.program.addPat(.{ .ty = step_ty, .data = .{ .tag = .{
            .name = tag_name,
            .payloads = try self.program.addPatSpan(&.{payload_pat}),
        } } });
    }

    /// Build the branch-dispatched tail: the source branch's structure, each
    /// arm's body replaced by the per-element computation run over that arm's
    /// appended items in yield order. `carry_start` is the base loop's
    /// accumulator result for a fold, or null for a side-effecting drive.
    fn buildTailDispatch(
        self: *Pass,
        branch_expr_id: Ast.ExprId,
        base_local: Ast.LocalId,
        carry_start: ?Ast.ExprId,
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        const expr = self.program.getExpr(branch_expr_id);
        if (expr.data == .if_) {
            const if_ = expr.data.if_;
            const branches = try GuardedList.dupe(self.allocator, Ast.IfBranch, self.program.ifBranchSpan(if_.branches));
            defer self.allocator.free(branches);
            var rewritten = try self.allocator.alloc(Ast.IfBranch, branches.len);
            defer self.allocator.free(rewritten);
            for (branches, 0..) |br, index| {
                const arm = (try self.buildArmTail(br.body, base_local, carry_start, loop_parts)) orelse return null;
                rewritten[index] = .{ .cond = br.cond, .body = arm };
            }
            const final_else = (try self.buildArmTail(if_.final_else, base_local, carry_start, loop_parts)) orelse return null;
            return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .if_ = .{
                .branches = try self.program.addIfBranchSpan(rewritten),
                .final_else = final_else,
            } } });
        } else if (expr.data == .match_) {
            const match = expr.data.match_;
            const branches = try GuardedList.dupe(self.allocator, Ast.Branch, self.program.branchSpan(match.branches));
            defer self.allocator.free(branches);
            var rewritten = try self.allocator.alloc(Ast.Branch, branches.len);
            defer self.allocator.free(rewritten);
            for (branches, 0..) |br, index| {
                const arm = (try self.buildArmTail(br.body, base_local, carry_start, loop_parts)) orelse return null;
                rewritten[index] = .{ .pat = br.pat, .bindings = br.bindings, .guard = br.guard, .body = arm };
            }
            return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .match_ = .{
                .scrutinee = match.scrutinee,
                .branches = try self.program.addBranchSpan(rewritten),
                .comptime_site = match.comptime_site,
            } } });
        }
        return null;
    }

    /// Run the loop's per-element computation over one arm's appended items in
    /// yield order. For a fold, thread each intermediate accumulator through a
    /// fresh binding starting from `carry_start`; for a drive, sequence the
    /// per-item effects. An arm that appends nothing yields the incoming
    /// accumulator (fold) or a no-op (drive).
    fn buildArmTail(
        self: *Pass,
        arm: Ast.ExprId,
        base_local: Ast.LocalId,
        carry_start: ?Ast.ExprId,
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        const chain = (try self.reduceArmChain(arm)) orelse return null;
        defer self.allocator.free(chain.items);
        if (chain.base != base_local) return null;

        if (chain.items.len == 0) {
            if (loop_parts.carry_count == 1) {
                const start = carry_start orelse return null;
                return start;
            }
            return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .unit });
        }

        var carry_ref = carry_start;
        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);
        for (chain.items, 0..) |item, index| {
            const step = (try self.buildBodyApplication(carry_ref, item, loop_parts)) orelse return null;
            if (index + 1 == chain.items.len) {
                if (stmts.items.len == 0) return step;
                return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .block = .{
                    .statements = try self.program.addStmtSpan(stmts.items),
                    .final_expr = step,
                } } });
            }
            if (loop_parts.carry_count == 1) {
                const fresh = try self.program.addLocal(self.symbols.fresh(), loop_parts.carry_ty);
                const bind = try self.program.addPat(.{ .ty = loop_parts.carry_ty, .data = .{ .bind = fresh } });
                try stmts.append(self.allocator, try self.program.addStmt(.{ .let_ = .{ .pat = bind, .value = step } }));
                carry_ref = try self.program.addExpr(.{ .ty = loop_parts.carry_ty, .data = .{ .local = fresh } });
            } else {
                try stmts.append(self.allocator, try self.program.addStmt(.{ .expr = step }));
            }
        }
        unreachable;
    }

    /// One application of the loop body: bind the item pattern to an appended
    /// item (and, for a fold, the accumulator parameter to the incoming
    /// accumulator), then run the per-element computation to its result. Every
    /// bound local is renamed fresh so the tail's applications and the base loop
    /// stay independent.
    fn buildBodyApplication(
        self: *Pass,
        carry_expr: ?Ast.ExprId,
        item_expr: Ast.ExprId,
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        var renames = collections.DenseMap(Ast.LocalId, Ast.LocalId).init(self.allocator);
        defer renames.deinit();

        // Guard against the accumulator flowing through the dropped iterator
        // slot: the rest binding must be read only by the continue we drop.
        if (try localUseCountInExpr(self.allocator, self.program, loop_parts.rest_local, loop_parts.one_body) != 1) return null;

        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);

        const item_pat = (try self.clonePatFresh(loop_parts.item_pat, &renames)) orelse return null;
        try stmts.append(self.allocator, try self.program.addStmt(.{ .let_ = .{ .pat = item_pat, .value = item_expr } }));

        if (loop_parts.carry_count == 1) {
            const carry = carry_expr orelse return null;
            const carry_local = try self.program.addLocal(self.symbols.fresh(), loop_parts.carry_ty);
            try renames.put(loop_parts.carry_param, carry_local);
            const carry_bind = try self.program.addPat(.{ .ty = loop_parts.carry_ty, .data = .{ .bind = carry_local } });
            try stmts.append(self.allocator, try self.program.addStmt(.{ .let_ = .{ .pat = carry_bind, .value = carry } }));
        }

        const body = (try self.cloneNewCarry(loop_parts.one_body, &renames, loop_parts)) orelse return null;

        return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .block = .{
            .statements = try self.program.addStmtSpan(stmts.items),
            .final_expr = body,
        } } });
    }

    /// Deep-clone a loop-body arm with all bound locals renamed fresh,
    /// replacing the tail `continue` with its per-element result: the
    /// accumulator value for a fold, or a unit for a side-effecting drive.
    /// Early `return`s are preserved (they exit the enclosing function the same
    /// way in the peeled tail). Returns null for constructs outside the
    /// foldable set (a nested loop, a `break`, a lambda), keeping the peel from
    /// duplicating unsupported control flow.
    fn cloneNewCarry(
        self: *Pass,
        expr_id: Ast.ExprId,
        renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId),
        loop_parts: IteratorLoopParts,
    ) Common.LowerError!?Ast.ExprId {
        const raw = (try self.runFreshClone(.{ .carry = expr_id }, renames, loop_parts)) orelse return null;
        return @enumFromInt(raw);
    }

    /// Deep-clone a pure-computation expression, applying local renames and
    /// allocating fresh locals at binding sites. Returns null for constructs
    /// outside the foldable set.
    fn cloneExprFresh(self: *Pass, expr_id: Ast.ExprId, renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.ExprId {
        const raw = (try self.runFreshClone(.{ .expr = expr_id }, renames, null)) orelse return null;
        return @enumFromInt(raw);
    }

    /// Clone a pattern, allocating a fresh local for every binding site and
    /// recording the rename. Returns null for list/string patterns, which the
    /// fold does not replay.
    fn clonePatFresh(self: *Pass, pat_id: Ast.PatId, renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.PatId {
        const raw = (try self.runFreshClone(.{ .pat = pat_id }, renames, null)) orelse return null;
        return @enumFromInt(raw);
    }

    /// One step of a fresh clone. Every `expr`, `carry`, `stmt`, and `pat`
    /// step leaves exactly one raw id on the result stack; a `finish_*` step
    /// consumes the results its children left above `base` and replaces them
    /// with the clone it builds. `abort` ends the whole clone with null.
    const FreshCloneOp = union(enum) {
        expr: Ast.ExprId,
        /// An expression in a new-carry tail position.
        carry: Ast.ExprId,
        stmt: Ast.StmtId,
        pat: Ast.PatId,
        finish_expr: FreshCloneFinish(Ast.ExprId),
        finish_carry: FreshCloneFinish(Ast.ExprId),
        finish_stmt: FreshCloneFinish(Ast.StmtId),
        finish_pat: FreshCloneFinish(Ast.PatId),
        abort,
    };

    fn FreshCloneFinish(comptime Id: type) type {
        return struct { id: Id, base: usize };
    }

    const FreshClone = struct {
        pass: *Pass,
        renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId),
        loop_parts: ?IteratorLoopParts,
        ops: std.ArrayList(FreshCloneOp) = .empty,
        results: std.ArrayList(u32) = .empty,

        fn deinit(clone: *FreshClone) void {
            clone.ops.deinit(clone.pass.allocator);
            clone.results.deinit(clone.pass.allocator);
        }

        fn op(clone: *FreshClone, item: FreshCloneOp) Allocator.Error!void {
            try clone.ops.append(clone.pass.allocator, item);
        }

        fn result(clone: *FreshClone, raw: u32) Allocator.Error!void {
            try clone.results.append(clone.pass.allocator, raw);
        }

        fn exprResult(clone: *FreshClone, expr: Ast.Expr) Allocator.Error!void {
            try clone.result(@intFromEnum(try clone.pass.program.addExpr(expr)));
        }

        fn exprSpanOps(clone: *FreshClone, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
            const exprs = clone.pass.program.exprSpan(span);
            for (0..exprs.len) |index| try clone.op(.{ .expr = GuardedList.at(exprs, index) });
        }

        fn captureOperandOps(clone: *FreshClone, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
            const operands = clone.pass.program.captureOperandSpan(span);
            for (0..operands.len) |index| try clone.op(.{ .expr = GuardedList.at(operands, index).value });
        }

        fn fieldExprOps(clone: *FreshClone, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
            const fields = clone.pass.program.fieldExprSpan(span);
            for (0..fields.len) |index| try clone.op(.{ .expr = GuardedList.at(fields, index).value });
        }

        fn patSpanOps(clone: *FreshClone, span: Ast.Span(Ast.PatId)) Allocator.Error!void {
            const pats = clone.pass.program.patSpan(span);
            for (0..pats.len) |index| try clone.op(.{ .pat = GuardedList.at(pats, index) });
        }

        /// Branch steps for a match whose arms are cloned with `arm_op`. A
        /// guarded or binding branch aborts at its own position, after the
        /// branches before it were cloned.
        fn matchBranchOps(clone: *FreshClone, branches_span: Ast.Span(Ast.Branch), comptime arm_op: std.meta.Tag(FreshCloneOp)) Allocator.Error!void {
            const branches = clone.pass.program.branchSpan(branches_span);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (branch.guard != null or branch.bindings.len != 0) {
                    try clone.op(.abort);
                    return;
                }
                try clone.op(.{ .pat = branch.pat });
                try clone.op(@unionInit(FreshCloneOp, @tagName(arm_op), branch.body));
            }
        }

        fn ifBranchOps(clone: *FreshClone, if_: anytype, comptime arm_op: std.meta.Tag(FreshCloneOp)) Allocator.Error!void {
            const branches = clone.pass.program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                try clone.op(.{ .expr = branch.cond });
                try clone.op(@unionInit(FreshCloneOp, @tagName(arm_op), branch.body));
            }
            try clone.op(@unionInit(FreshCloneOp, @tagName(arm_op), if_.final_else));
        }

        fn stmtSpanOps(clone: *FreshClone, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
            const stmts = clone.pass.program.stmtSpan(span);
            for (0..stmts.len) |index| try clone.op(.{ .stmt = GuardedList.at(stmts, index) });
        }

        fn run(clone: *FreshClone, root: FreshCloneOp) Common.LowerError!?u32 {
            try clone.op(root);
            while (clone.ops.pop()) |item| {
                // A step appends its sub-steps in order, then they are
                // reversed onto the stack.
                const start = clone.ops.items.len;
                if (!try clone.step(item)) return null;
                std.mem.reverse(FreshCloneOp, clone.ops.items[start..]);
            }
            if (clone.results.items.len != 1) Common.invariant("fresh clone did not produce exactly one result");
            return clone.results.items[0];
        }

        /// Children results, in step order, above `base`.
        fn childResults(clone: *FreshClone, base: usize) []const u32 {
            return clone.results.items[base..];
        }

        fn replaceResults(clone: *FreshClone, base: usize, raw: u32) Allocator.Error!void {
            clone.results.shrinkRetainingCapacity(base);
            try clone.result(raw);
        }

        fn step(clone: *FreshClone, item: FreshCloneOp) Common.LowerError!bool {
            switch (item) {
                .abort => return false,
                .expr => |expr_id| return clone.enterExpr(expr_id),
                .carry => |expr_id| return clone.enterCarry(expr_id),
                .stmt => |stmt_id| {
                    const base = clone.results.items.len;
                    switch (clone.pass.program.getStmt(stmt_id)) {
                        .let_ => |let_| {
                            try clone.op(.{ .expr = let_.value });
                            try clone.op(.{ .pat = let_.pat });
                        },
                        .expr => |expr| try clone.op(.{ .expr = expr }),
                        .uninitialized, .expect, .dbg, .return_, .crash, .checked_error => return false,
                    }
                    try clone.op(.{ .finish_stmt = .{ .id = stmt_id, .base = base } });
                },
                .pat => |pat_id| return clone.enterPat(pat_id),
                .finish_expr => |finish| try clone.finishExpr(finish.id, finish.base),
                .finish_carry => |finish| try clone.finishCarry(finish.id, finish.base),
                .finish_stmt => |finish| {
                    const program = clone.pass.program;
                    const children = clone.childResults(finish.base);
                    const stmt: Ast.Stmt = switch (program.getStmt(finish.id)) {
                        .let_ => |let_| .{ .let_ = .{
                            .pat = @enumFromInt(children[1]),
                            .value = @enumFromInt(children[0]),
                            .recursive = let_.recursive,
                            .comptime_site = let_.comptime_site,
                        } },
                        .expr => .{ .expr = @enumFromInt(children[0]) },
                        .uninitialized, .expect, .dbg, .return_, .crash, .checked_error => Common.invariant("fresh clone finished a statement it does not clone"),
                    };
                    try clone.replaceResults(finish.base, @intFromEnum(try program.addStmt(stmt)));
                },
                .finish_pat => |finish| try clone.finishPat(finish.id, finish.base),
            }
            return true;
        }

        fn enterExpr(clone: *FreshClone, expr_id: Ast.ExprId) Common.LowerError!bool {
            const program = clone.pass.program;
            const expr = program.getExpr(expr_id);
            const base = clone.results.items.len;
            switch (expr.data) {
                .local => |local| {
                    const renamed = clone.renames.get(local);
                    const ty = if (renamed) |fresh| program.getLocal(fresh).ty else expr.ty;
                    try clone.exprResult(.{ .ty = ty, .data = .{ .local = renamed orelse local } });
                    return true;
                },
                .unit,
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .crash,
                .checked_error,
                => {
                    try clone.exprResult(.{ .ty = expr.ty, .data = expr.data });
                    return true;
                },
                .static_data_candidate,
                .comptime_value,
                => {
                    try clone.result(@intFromEnum(expr_id));
                    return true;
                },
                .list, .tuple => |items| try clone.exprSpanOps(items),
                .record => |fields| try clone.fieldExprOps(fields),
                .tag => |tag| try clone.exprSpanOps(tag.payloads),
                .typed_boundary => |boundary| try clone.op(.{ .expr = boundary.value }),
                .nominal => |backing| try clone.op(.{ .expr = backing }),
                .fn_ref => |fn_ref| try clone.captureOperandOps(fn_ref.captures),
                .field_access => |field| try clone.op(.{ .expr = field.receiver }),
                .tuple_access => |access| try clone.op(.{ .expr = access.tuple }),
                .structural_eq => |eq| {
                    try clone.op(.{ .expr = eq.lhs });
                    try clone.op(.{ .expr = eq.rhs });
                },
                .structural_hash => |h| {
                    try clone.op(.{ .expr = h.value });
                    try clone.op(.{ .expr = h.hasher });
                },
                .low_level => |call| try clone.exprSpanOps(call.args),
                .call_proc => |call| {
                    try clone.exprSpanOps(call.args);
                    try clone.captureOperandOps(call.captures);
                },
                .call_value => |call| {
                    try clone.op(.{ .expr = call.callee });
                    try clone.exprSpanOps(call.args);
                },
                .let_ => |let_| {
                    try clone.op(.{ .expr = let_.value });
                    try clone.op(.{ .pat = let_.bind });
                    try clone.op(.{ .expr = let_.rest });
                },
                .block => |block| {
                    try clone.stmtSpanOps(block.statements);
                    try clone.op(.{ .expr = block.final_expr });
                },
                .if_ => |if_| try clone.ifBranchOps(if_, .expr),
                .match_ => |match| {
                    try clone.op(.{ .expr = match.scrutinee });
                    try clone.matchBranchOps(match.branches, .expr);
                },
                // An early return exits the enclosing function; it is preserved
                // verbatim in the peeled tail, where it fires only after the base
                // iteration completes without returning—the same order the
                // unfused loop would return in.
                .return_ => |ret| try clone.op(.{ .expr = ret.value }),
                .continue_ => |continue_| try clone.exprSpanOps(continue_.values),
                .@"unreachable",
                .record_update,
                .lambda,
                .def_ref,
                .fn_def,
                .uninitialized,
                .uninitialized_payload,
                .if_initialized_payload,
                .try_sequence,
                .try_record_sequence,
                .loop_,
                .break_,
                .join_point,
                .jump,
                .comptime_branch_taken,
                .comptime_exhaustiveness_failed,
                .dbg,
                .expect_err,
                .literal_rejected,
                .expect,
                => return false,
            }
            try clone.op(.{ .finish_expr = .{ .id = expr_id, .base = base } });
            return true;
        }

        fn finishExpr(clone: *FreshClone, expr_id: Ast.ExprId, base: usize) Allocator.Error!void {
            const program = clone.pass.program;
            const expr = program.getExpr(expr_id);
            const children = clone.childResults(base);
            const data: Ast.ExprData = switch (expr.data) {
                .list => .{ .list = try clone.exprIdSpan(children) },
                .tuple => .{ .tuple = try clone.exprIdSpan(children) },
                .record => |fields| .{ .record = try clone.fieldExprSpan(fields, children) },
                .tag => |tag| .{ .tag = .{ .name = tag.name, .payloads = try clone.exprIdSpan(children) } },
                .typed_boundary => .{ .typed_boundary = .{ .value = @enumFromInt(children[0]) } },
                .nominal => .{ .nominal = @enumFromInt(children[0]) },
                .fn_ref => |fn_ref| .{ .fn_ref = .{
                    .fn_id = fn_ref.fn_id,
                    .captures = try clone.captureOperandSpan(fn_ref.captures, children),
                } },
                .field_access => |field| .{ .field_access = .{
                    .receiver = @enumFromInt(children[0]),
                    .segments = field.segments,
                } },
                .tuple_access => |access| .{ .tuple_access = .{
                    .tuple = @enumFromInt(children[0]),
                    .elem_index = access.elem_index,
                } },
                .structural_eq => |eq| .{ .structural_eq = .{
                    .lhs = @enumFromInt(children[0]),
                    .rhs = @enumFromInt(children[1]),
                    .negated = eq.negated,
                } },
                .structural_hash => .{ .structural_hash = .{
                    .value = @enumFromInt(children[0]),
                    .hasher = @enumFromInt(children[1]),
                } },
                .low_level => |call| .{ .low_level = .{ .op = call.op, .args = try clone.exprIdSpan(children) } },
                .call_proc => |call| .{ .call_proc = .{
                    .callee = call.callee,
                    .args = try clone.exprIdSpan(children[0..call.args.len]),
                    .iterator_procedure = call.iterator_procedure,
                    .captures = try clone.captureOperandSpan(call.captures, children[call.args.len..]),
                    .is_cold = call.is_cold,
                } },
                .call_value => .{ .call_value = .{
                    .callee = @enumFromInt(children[0]),
                    .args = try clone.exprIdSpan(children[1..]),
                } },
                .let_ => |let_| .{ .let_ = .{
                    .bind = @enumFromInt(children[1]),
                    .value = @enumFromInt(children[0]),
                    .rest = @enumFromInt(children[2]),
                    .comptime_site = let_.comptime_site,
                } },
                .block => .{ .block = try clone.blockData(children) },
                .if_ => .{ .if_ = try clone.ifData(children) },
                .match_ => |match| .{ .match_ = try clone.matchData(match, children) },
                .return_ => |ret| .{ .return_ = .{ .value = @enumFromInt(children[0]), .target = ret.target } },
                .continue_ => .{ .continue_ = .{ .values = try clone.exprIdSpan(children) } },
                .local,
                .unit,
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .crash,
                .checked_error,
                .static_data_candidate,
                .comptime_value,
                .@"unreachable",
                .record_update,
                .lambda,
                .def_ref,
                .fn_def,
                .uninitialized,
                .uninitialized_payload,
                .if_initialized_payload,
                .try_sequence,
                .try_record_sequence,
                .loop_,
                .break_,
                .join_point,
                .jump,
                .comptime_branch_taken,
                .comptime_exhaustiveness_failed,
                .dbg,
                .expect_err,
                .literal_rejected,
                .expect,
                => Common.invariant("fresh clone finished an expression it does not rebuild"),
            };
            try clone.replaceResults(base, @intFromEnum(try program.addExpr(.{ .ty = expr.ty, .data = data })));
        }

        fn enterCarry(clone: *FreshClone, expr_id: Ast.ExprId) Common.LowerError!bool {
            const loop_parts = clone.loop_parts orelse Common.invariant("new-carry clone step had no loop parts");
            const program = clone.pass.program;
            const base = clone.results.items.len;
            switch (program.getExpr(expr_id).data) {
                .continue_ => |cont| {
                    const values = program.exprSpan(cont.values);
                    if (values.len != loop_parts.carry_count + 1) return false;
                    if (loop_parts.carry_count == 0) {
                        try clone.exprResult(.{ .ty = loop_parts.value_ty, .data = .unit });
                    } else {
                        try clone.op(.{ .expr = GuardedList.at(values, 1) });
                    }
                    return true;
                },
                .block => |block| {
                    try clone.stmtSpanOps(block.statements);
                    try clone.op(.{ .carry = block.final_expr });
                },
                .if_ => |if_| try clone.ifBranchOps(if_, .carry),
                .match_ => |match| {
                    try clone.op(.{ .expr = match.scrutinee });
                    try clone.matchBranchOps(match.branches, .carry);
                },
                .local,
                .unit,
                .@"unreachable",
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .static_data_candidate,
                .comptime_value,
                .typed_boundary,
                .list,
                .tuple,
                .record,
                .record_update,
                .tag,
                .nominal,
                .let_,
                .lambda,
                .def_ref,
                .fn_def,
                .fn_ref,
                .call_value,
                .call_proc,
                .low_level,
                .field_access,
                .tuple_access,
                .structural_eq,
                .structural_hash,
                .uninitialized,
                .uninitialized_payload,
                .if_initialized_payload,
                .try_sequence,
                .try_record_sequence,
                .loop_,
                .break_,
                .join_point,
                .jump,
                .return_,
                .crash,
                .checked_error,
                .comptime_branch_taken,
                .comptime_exhaustiveness_failed,
                .dbg,
                .expect_err,
                .literal_rejected,
                .expect,
                => return clone.enterExpr(expr_id),
            }
            try clone.op(.{ .finish_carry = .{ .id = expr_id, .base = base } });
            return true;
        }

        fn finishCarry(clone: *FreshClone, expr_id: Ast.ExprId, base: usize) Allocator.Error!void {
            const loop_parts = clone.loop_parts orelse Common.invariant("new-carry clone step had no loop parts");
            const program = clone.pass.program;
            const children = clone.childResults(base);
            const data: Ast.ExprData = switch (program.getExpr(expr_id).data) {
                .block => .{ .block = try clone.blockData(children) },
                .if_ => .{ .if_ = try clone.ifData(children) },
                .match_ => |match| .{ .match_ = try clone.matchData(match, children) },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => Common.invariant("new-carry clone finished an expression it does not rebuild"),
            };
            try clone.replaceResults(base, @intFromEnum(try program.addExpr(.{ .ty = loop_parts.value_ty, .data = data })));
        }

        fn enterPat(clone: *FreshClone, pat_id: Ast.PatId) Common.LowerError!bool {
            const pass = clone.pass;
            const pat = pass.program.getPat(pat_id);
            const base = clone.results.items.len;
            switch (pat.data) {
                .bind => |local| {
                    const fresh = try pass.program.addLocal(pass.symbols.fresh(), pat.ty);
                    try clone.renames.put(local, fresh);
                    try clone.result(@intFromEnum(try pass.program.addPat(.{ .ty = pat.ty, .data = .{ .bind = fresh } })));
                    return true;
                },
                .wildcard,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                => {
                    try clone.result(@intFromEnum(try pass.program.addPat(.{ .ty = pat.ty, .data = pat.data })));
                    return true;
                },
                .as => |as| try clone.op(.{ .pat = as.pattern }),
                .record => |fields_span| {
                    const fields = pass.program.recordDestructSpan(fields_span);
                    for (0..fields.len) |index| try clone.op(.{ .pat = GuardedList.at(fields, index).pattern });
                },
                .tuple => |items| try clone.patSpanOps(items),
                .tag => |tag| try clone.patSpanOps(tag.payloads),
                .nominal => |backing| try clone.op(.{ .pat = backing }),
                .list, .str_pattern => return false,
            }
            try clone.op(.{ .finish_pat = .{ .id = pat_id, .base = base } });
            return true;
        }

        fn finishPat(clone: *FreshClone, pat_id: Ast.PatId, base: usize) Common.LowerError!void {
            const pass = clone.pass;
            const pat = pass.program.getPat(pat_id);
            const children = clone.childResults(base);
            const data: Ast.PatData = switch (pat.data) {
                .as => |as| blk: {
                    const fresh = try pass.program.addLocal(pass.symbols.fresh(), pat.ty);
                    try clone.renames.put(as.local, fresh);
                    break :blk .{ .as = .{ .pattern = @enumFromInt(children[0]), .local = fresh } };
                },
                .record => |fields_span| blk: {
                    const fields = try GuardedList.dupe(pass.allocator, Ast.RecordDestruct, pass.program.recordDestructSpan(fields_span));
                    defer pass.allocator.free(fields);
                    for (fields, children) |*field, raw| field.pattern = @enumFromInt(raw);
                    break :blk .{ .record = try pass.program.addRecordDestructSpan(fields) };
                },
                .tuple => .{ .tuple = try clone.patIdSpan(children) },
                .tag => |tag| .{ .tag = .{ .name = tag.name, .payloads = try clone.patIdSpan(children) } },
                .nominal => .{ .nominal = @enumFromInt(children[0]) },
                .bind,
                .wildcard,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .list,
                .str_pattern,
                => Common.invariant("fresh clone finished a pattern it does not rebuild"),
            };
            try clone.replaceResults(base, @intFromEnum(try pass.program.addPat(.{ .ty = pat.ty, .data = data })));
        }

        fn exprIdSpan(clone: *FreshClone, raws: []const u32) Allocator.Error!Ast.Span(Ast.ExprId) {
            const allocator = clone.pass.allocator;
            const ids = try allocator.alloc(Ast.ExprId, raws.len);
            defer allocator.free(ids);
            for (ids, raws) |*id, raw| id.* = @enumFromInt(raw);
            return clone.pass.program.addExprSpan(ids);
        }

        fn patIdSpan(clone: *FreshClone, raws: []const u32) Allocator.Error!Ast.Span(Ast.PatId) {
            const allocator = clone.pass.allocator;
            const ids = try allocator.alloc(Ast.PatId, raws.len);
            defer allocator.free(ids);
            for (ids, raws) |*id, raw| id.* = @enumFromInt(raw);
            return clone.pass.program.addPatSpan(ids);
        }

        fn fieldExprSpan(clone: *FreshClone, source: Ast.Span(Ast.FieldExpr), raws: []const u32) Allocator.Error!Ast.Span(Ast.FieldExpr) {
            const allocator = clone.pass.allocator;
            const fields = try GuardedList.dupe(allocator, Ast.FieldExpr, clone.pass.program.fieldExprSpan(source));
            defer allocator.free(fields);
            for (fields, raws) |*field, raw| field.value = @enumFromInt(raw);
            return clone.pass.program.addFieldExprSpan(fields);
        }

        fn captureOperandSpan(clone: *FreshClone, source: Ast.Span(Ast.CaptureOperand), raws: []const u32) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
            const allocator = clone.pass.allocator;
            const operands = try GuardedList.dupe(allocator, Ast.CaptureOperand, clone.pass.program.captureOperandSpan(source));
            defer allocator.free(operands);
            for (operands, raws) |*operand, raw| operand.value = @enumFromInt(raw);
            return clone.pass.program.addCaptureOperandSpan(operands);
        }

        /// Block data from statement results followed by the final result.
        fn blockData(clone: *FreshClone, raws: []const u32) Allocator.Error!@FieldType(Ast.ExprData, "block") {
            const allocator = clone.pass.allocator;
            const stmt_count = raws.len - 1;
            const stmts = try allocator.alloc(Ast.StmtId, stmt_count);
            defer allocator.free(stmts);
            for (stmts, raws[0..stmt_count]) |*stmt, raw| stmt.* = @enumFromInt(raw);
            return .{
                .statements = try clone.pass.program.addStmtSpan(stmts),
                .final_expr = @enumFromInt(raws[stmt_count]),
            };
        }

        /// If data from (condition, body) result pairs followed by the final
        /// else result.
        fn ifData(clone: *FreshClone, raws: []const u32) Allocator.Error!@FieldType(Ast.ExprData, "if_") {
            const allocator = clone.pass.allocator;
            const branch_count = (raws.len - 1) / 2;
            const branches = try allocator.alloc(Ast.IfBranch, branch_count);
            defer allocator.free(branches);
            for (branches, 0..) |*branch, index| branch.* = .{
                .cond = @enumFromInt(raws[2 * index]),
                .body = @enumFromInt(raws[2 * index + 1]),
            };
            return .{
                .branches = try clone.pass.program.addIfBranchSpan(branches),
                .final_else = @enumFromInt(raws[raws.len - 1]),
            };
        }

        /// Match data from the scrutinee result followed by (pattern, body)
        /// result pairs.
        fn matchData(clone: *FreshClone, match: @FieldType(Ast.ExprData, "match_"), raws: []const u32) Allocator.Error!@FieldType(Ast.ExprData, "match_") {
            const allocator = clone.pass.allocator;
            const branch_count = (raws.len - 1) / 2;
            const branches = try allocator.alloc(Ast.Branch, branch_count);
            defer allocator.free(branches);
            for (branches, 0..) |*branch, index| branch.* = .{
                .pat = @enumFromInt(raws[1 + 2 * index]),
                .guard = null,
                .body = @enumFromInt(raws[2 + 2 * index]),
            };
            return .{
                .scrutinee = @enumFromInt(raws[0]),
                .branches = try clone.pass.program.addBranchSpan(branches),
                .comptime_site = match.comptime_site,
            };
        }
    };

    fn runFreshClone(
        self: *Pass,
        root: FreshCloneOp,
        renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId),
        loop_parts: ?IteratorLoopParts,
    ) Common.LowerError!?u32 {
        var clone: FreshClone = .{ .pass = self, .renames = renames, .loop_parts = loop_parts };
        defer clone.deinit();
        return clone.run(root);
    }

    /// Normalize a whole function body once through the value-aware cloner.
    /// Structural consumers expose exact producer calls locally, call-pattern
    /// rewrites consume the resulting values, and loop fixed points scalarize
    /// known carried structure without a body-category routing decision.
    fn cloneFnBodyInPlace(self: *Pass, fn_id: Ast.FnId) Common.LowerError!void {
        const fn_index = @intFromEnum(fn_id);
        if (fn_index < self.whole_body_cloned.len and self.whole_body_cloned[fn_index]) return;

        const fn_ = self.program.getFn(fn_id);
        const body = switch (self.sourceBody(fn_id)) {
            .roc => |body| body,
            .hosted => return,
        };
        var cloner = Cloner.initForOriginalBodyRewrite(self);
        defer cloner.deinit();
        cloner.inline_calls = switch (self.clone_inlining) {
            .all_calls => .all,
            .iterator_fusion => .iterator_fusion,
        };
        cloner.inline_direct_requires_known_arg = true;
        const args = self.program.typedLocalSpan(fn_.args);
        for (0..args.len) |index| {
            const local = GuardedList.at(args, index).local;
            try cloner.putLocalAlias(local, local);
        }
        const captures = self.program.typedLocalSpan(fn_.captures);
        for (0..captures.len) |index| {
            const local = GuardedList.at(captures, index).local;
            try cloner.putLocalAlias(local, local);
        }
        const outer_shapes = self.program.beginFnShapes(fn_id);
        const cloned = try cloner.cloneExpr(body);
        // Unchanged subtrees of the source body are reused rather than
        // re-created, so the rewritten body keeps the source body's shapes.
        const shapes = self.program.finishFnShapes(outer_shapes).merged(fn_.shapes);
        self.program.setFn(fn_id, .{
            .symbol = fn_.symbol,
            .source = fn_.source,
            .spec_constr_pattern = fn_.spec_constr_pattern,
            .root_identity = fn_.root_identity,
            .signature = fn_.signature,
            .iterator_fusion_scope = fn_.iterator_fusion_scope,
            .args = fn_.args,
            .captures = fn_.captures,
            .body = .{ .roc = cloned },
            .ret = fn_.ret,
            .shapes = shapes,
        });
        if (fn_index < self.whole_body_cloned.len) self.whole_body_cloned[fn_index] = true;
    }

    fn cloneFnBodyForIteratorFusion(self: *Pass, fn_id: Ast.FnId) Common.LowerError!void {
        const fn_index = @intFromEnum(fn_id);
        const fn_ = self.program.getFn(fn_id);
        const body = switch (self.sourceBody(fn_id)) {
            .roc => |source| source,
            .hosted => return,
        };

        var cloner = Cloner.initForRewrite(self);
        defer cloner.deinit();
        cloner.inline_calls = .iterator_fusion;
        cloner.inline_direct_requires_known_arg = false;
        cloner.rewrite_call_patterns = false;
        cloner.emit_callable_workers = false;

        const args = self.program.typedLocalSpan(fn_.args);
        for (0..args.len) |index| {
            const local = GuardedList.at(args, index).local;
            try cloner.putLocalAlias(local, local);
        }
        const captures = self.program.typedLocalSpan(fn_.captures);
        for (0..captures.len) |index| {
            const local = GuardedList.at(captures, index).local;
            try cloner.putLocalAlias(local, local);
        }

        const outer_shapes = self.program.beginFnShapes(fn_id);
        const cloned = try cloner.cloneExpr(body);
        // Unchanged subtrees of the source body are reused rather than
        // re-created, so the rewritten body keeps the source body's shapes.
        const shapes = self.program.finishFnShapes(outer_shapes).merged(fn_.shapes);
        self.program.setFn(fn_id, .{
            .symbol = fn_.symbol,
            .source = fn_.source,
            .spec_constr_pattern = fn_.spec_constr_pattern,
            .root_identity = fn_.root_identity,
            .signature = fn_.signature,
            .iterator_fusion_scope = true,
            .args = fn_.args,
            .captures = fn_.captures,
            .body = .{ .roc = cloned },
            .ret = fn_.ret,
            .shapes = shapes,
        });
        if (!self.borrowed_worker) self.whole_body_cloned[fn_index] = true;
    }

    /// Once the specialization graph is complete, clone only functions whose
    /// loop result ABI contains fields their exact continuation cannot observe.
    /// Calls and loop-carried state stay unchanged in this final pass; its sole
    /// authority is the producer-visible result binding and continuation.
    fn projectUnusedLoopResults(self: *Pass) Common.LowerError!void {
        try self.runIndependentPhase(.unused_loop_results, self.program.fnCount());
    }

    fn projectUnusedLoopResultsInFn(self: *Pass, fn_id: Ast.FnId) Common.LowerError!bool {
        const fn_ = self.program.getFn(fn_id);
        const body = switch (fn_.body) {
            .roc => |body| body,
            .hosted => return false,
        };
        var demands = ExitDemand.Inventory.init(self.allocator, self.program);
        defer demands.deinit();
        try demands.collect(body);
        if (!demands.hasSelection()) return false;

        var cloner = Cloner.initForLoopExitSelection(self);
        defer cloner.deinit();
        cloner.exit_demands = &demands;
        const outer_shapes = self.program.beginFnShapes(fn_id);
        const cloned = try cloner.cloneExpr(body);
        // Unchanged subtrees of the source body are reused rather than
        // re-created, so the rewritten body keeps the source body's shapes.
        const shapes = self.program.finishFnShapes(outer_shapes).merged(fn_.shapes);
        self.program.setFn(fn_id, .{
            .symbol = fn_.symbol,
            .source = fn_.source,
            .spec_constr_pattern = fn_.spec_constr_pattern,
            .root_identity = fn_.root_identity,
            .signature = fn_.signature,
            .args = fn_.args,
            .captures = fn_.captures,
            .body = .{ .roc = cloned },
            .ret = fn_.ret,
            .shapes = shapes,
        });
        return true;
    }

    /// Redirect every direct call in `expr_id` to its selected
    /// specialization, children before the call that uses them. `done`
    /// marks expressions already visited, so a shared subtree is rewritten
    /// once.
    fn rewriteCallsInExpr(self: *Pass, expr_id: Ast.ExprId, done: []bool) Allocator.Error!void {
        const Visitor = struct {
            pass: *Pass,
            done: []bool,

            pub fn enterExpr(visitor: @This(), id: Ast.ExprId) Allocator.Error!Ast.ExprWalk {
                const index = @intFromEnum(id);
                if (visitor.done[index]) return .skip;
                visitor.done[index] = true;
                return switch (visitor.pass.program.getExprAt(index).data) {
                    // Its closed source initializer is shared and immutable.
                    .static_data_candidate => .skip,
                    // Its producer witness is closed and immutable.
                    .comptime_value => .skip,
                    .lambda,
                    .def_ref,
                    .fn_def,
                    => Common.invariant("pre-lift function expression reached call-pattern specialization"),
                    .call_proc => .descend_then_exit,
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
                };
            }

            pub fn exitExpr(visitor: @This(), id: Ast.ExprId) Allocator.Error!void {
                try visitor.pass.rewriteCallProc(id, visitor.pass.program.getExpr(id).data.call_proc);
            }
        };
        try Ast.walkExprs(self.allocator, self.program, .{ .expr = expr_id }, Visitor{ .pass = self, .done = done });
    }

    fn rewriteCallProc(self: *Pass, expr_id: Ast.ExprId, call: @import("../monotype/ast.zig").CallProc) Allocator.Error!void {
        const callee = Ast.localDirectCallee(call) orelse return;
        const raw = @intFromEnum(callee);
        if (raw >= self.plans.len) return;
        if (self.plans[raw].specs.items.len == 0) return;

        const args = try GuardedList.dupe(self.allocator, Ast.ExprId, self.program.exprSpan(call.args));
        defer self.allocator.free(args);
        for (self.plans[raw].specs.items) |spec| {
            var rewritten_args = std.ArrayList(Ast.ExprId).empty;
            defer rewritten_args.deinit(self.allocator);

            var cloner = Cloner.initForRewrite(self);
            defer cloner.deinit();
            var bindings: BindingChain = .{};

            if (try self.appendExistingCallArgs(&cloner, spec.pattern, args, &bindings, &rewritten_args)) {
                const new_call: Ast.ExprData = .{ .call_proc = .{
                    .callee = .{ .lifted = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before rewriting") },
                    .args = try self.program.addExprSpan(rewritten_args.items),
                    .iterator_procedure = call.iterator_procedure,
                    .captures = call.captures,
                    .is_cold = call.is_cold,
                } };
                if (bindings.isEmpty()) {
                    self.program.setExprData(expr_id, new_call);
                } else {
                    // Decomposing the argument created bindings its leaves
                    // reference; the rewritten call site becomes a let chain
                    // ending in the specialized call.
                    const call_ty = self.program.getExpr(expr_id).ty;
                    const call_expr = try cloner.addExpr(.{ .ty = call_ty, .data = new_call });
                    const wrapped = try cloner.wrapBindings(bindings, call_expr);
                    self.program.setExprData(expr_id, self.program.getExpr(wrapped).data);
                }
                return;
            }
        }
    }

    fn appendExistingCallArgs(
        self: *Pass,
        cloner: *Cloner,
        pattern: CallPattern,
        args: []const Ast.ExprId,
        bindings: *BindingChain,
        out: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!bool {
        const binding_mark = bindings.mark();
        var matched = false;
        defer if (!matched) bindings.rewind(binding_mark);

        if (pattern.args.len != args.len) Common.invariant("call-pattern arity differed from direct call arity");
        for (pattern.args, args) |shape, arg| {
            const cloned = try cloner.cloneExprValue(arg);
            bindings.appendChain(cloned.bindings);
            if (!try shapeMatchesValue(self.program, shape, cloned.value)) return false;
            try cloner.appendExprsFromValue(shape, cloned.value, out);
        }
        matched = true;
        return true;
    }

    /// The constructor shape of an expression, or null when it is not an
    /// explicit constructor. A component that is not one stands as `.any` of
    /// its type, except a nominal's backing, which decides the nominal.
    /// Constructors nest as deeply as the source writes them, so each waits
    /// on an explicit frame for its components.
    fn constructorShape(self: *Pass, root: Ast.ExprId) Allocator.Error!?Shape {
        const Frame = struct {
            expr_id: Ast.ExprId,
            /// Each component's expression, when it has one, and the type an
            /// absent or non-constructor component stands as.
            components: []const ConstructorShapeComponent,
            shapes: []?Shape,
            next: usize = 0,
        };
        var frames = std.ArrayList(Frame).empty;
        defer {
            for (frames.items) |frame| {
                self.allocator.free(frame.components);
                self.allocator.free(frame.shapes);
            }
            frames.deinit(self.allocator);
        }
        var delivered: ?Shape = null;
        switch (try self.enterConstructorShape(root)) {
            .leaf => |shape| return shape,
            .components => |components| {
                errdefer self.allocator.free(components);
                const shapes = try self.allocator.alloc(?Shape, components.len);
                errdefer self.allocator.free(shapes);
                try frames.append(self.allocator, .{ .expr_id = root, .components = components, .shapes = shapes });
            },
        }
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next > 0) frame.shapes[frame.next - 1] = delivered;
            delivered = null;
            if (frame.next < frame.components.len) {
                const component = frame.components[frame.next];
                frame.next += 1;
                const expr_id = component.expr orelse continue;
                switch (try self.enterConstructorShape(expr_id)) {
                    .leaf => |shape| delivered = shape,
                    .components => |components| {
                        errdefer self.allocator.free(components);
                        const shapes = try self.allocator.alloc(?Shape, components.len);
                        errdefer self.allocator.free(shapes);
                        try frames.append(self.allocator, .{ .expr_id = expr_id, .components = components, .shapes = shapes });
                    },
                }
                continue;
            }
            const finished = frames.pop().?;
            defer {
                self.allocator.free(finished.components);
                self.allocator.free(finished.shapes);
            }
            const shape = try self.finishConstructorShape(finished.expr_id, finished.components, finished.shapes);
            if (frames.items.len == 0) return shape;
            delivered = shape;
        }
    }

    const ConstructorShapeComponent = struct {
        expr: ?Ast.ExprId,
        ty: Type.TypeId,
    };

    const ConstructorShapeEntry = union(enum) {
        /// The expression's shape needs no component.
        leaf: ?Shape,
        /// The expression's components, in order. Owned.
        components: []const ConstructorShapeComponent,
    };

    fn enterConstructorShape(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!ConstructorShapeEntry {
        const expr = self.program.getExpr(expr_id);
        if (expr.data == .tag or expr.data == .record or expr.data == .tuple) assertStructuralConstructionType(self.program, expr.ty);
        var components = std.ArrayList(ConstructorShapeComponent).empty;
        errdefer components.deinit(self.allocator);
        switch (expr.data) {
            .tag => |tag| {
                const payloads = self.program.exprSpan(tag.payloads);
                for (0..payloads.len) |index| {
                    const payload = GuardedList.at(payloads, index);
                    try components.append(self.allocator, .{ .expr = payload, .ty = self.program.getExpr(payload).ty });
                }
            },
            .record => |fields_span| {
                const fields = self.program.fieldExprSpan(fields_span);
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    try components.append(self.allocator, .{ .expr = field.value, .ty = self.program.getExpr(field.value).ty });
                }
            },
            .record_update => |update| {
                const type_fields = self.program.types.fieldSpan(recordUpdateFieldSpan(self.program, expr.ty));
                const update_fields = self.program.fieldExprSpan(update.fields);
                for (0..type_fields.len) |index| {
                    const type_field = GuardedList.at(type_fields, index);
                    const updated = for (0..update_fields.len) |update_index| {
                        const field = GuardedList.at(update_fields, update_index);
                        if (self.program.names.recordFieldLabelTextEql(type_field.name, field.name)) break field.value;
                    } else null;
                    try components.append(self.allocator, .{ .expr = updated, .ty = type_field.ty });
                }
            },
            .tuple => |items_span| {
                const items = self.program.exprSpan(items_span);
                for (0..items.len) |index| {
                    const item = GuardedList.at(items, index);
                    try components.append(self.allocator, .{ .expr = item, .ty = self.program.getExpr(item).ty });
                }
            },
            .nominal => |backing| try components.append(self.allocator, .{ .expr = backing, .ty = self.program.getExpr(backing).ty }),
            .fn_ref => |fn_ref| {
                const capture_operands = self.program.captureOperandSpan(fn_ref.captures);
                for (0..capture_operands.len) |index| {
                    const operand = GuardedList.at(capture_operands, index);
                    try components.append(self.allocator, .{ .expr = operand.value, .ty = self.program.getExpr(operand.value).ty });
                }
            },
            .typed_boundary,
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .static_data_candidate,
            .comptime_value,
            .list,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .call_value,
            .call_proc,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => return .{ .leaf = null },
        }
        return .{ .components = try components.toOwnedSlice(self.allocator) };
    }

    fn finishConstructorShape(
        self: *Pass,
        expr_id: Ast.ExprId,
        components: []const ConstructorShapeComponent,
        component_shapes: []const ?Shape,
    ) Allocator.Error!?Shape {
        const arena = self.arena.allocator();
        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .tag => |tag| blk: {
                const shapes = try arena.alloc(Shape, components.len);
                for (shapes, components, component_shapes) |*shape, component, component_shape| {
                    shape.* = component_shape orelse .{ .any = component.ty };
                }
                break :blk Shape{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = shapes,
                } };
            },
            .record => |fields_span| blk: {
                const fields = self.program.fieldExprSpan(fields_span);
                const shapes = try arena.alloc(FieldShape, components.len);
                for (shapes, components, component_shapes, 0..) |*shape, component, component_shape, index| {
                    shape.* = .{
                        .name = GuardedList.at(fields, index).name,
                        .shape = component_shape orelse .{ .any = component.ty },
                    };
                }
                break :blk Shape{ .record = .{
                    .ty = expr.ty,
                    .fields = shapes,
                } };
            },
            .record_update => blk: {
                const record_ty = recordUpdateBackingType(self.program, expr.ty);
                const type_fields = self.program.types.fieldSpan(recordUpdateFieldSpan(self.program, expr.ty));
                const shapes = try arena.alloc(FieldShape, components.len);
                for (shapes, components, component_shapes, 0..) |*shape, component, component_shape, index| {
                    shape.* = .{
                        .name = GuardedList.at(type_fields, index).name,
                        .shape = component_shape orelse .{ .any = component.ty },
                    };
                }
                const record_shape = Shape{ .record = .{
                    .ty = record_ty,
                    .fields = shapes,
                } };
                if (nominalConstructionLayer(self.program, expr.ty) != null) {
                    const backing = try arena.create(Shape);
                    backing.* = record_shape;
                    break :blk Shape{ .nominal = .{
                        .ty = expr.ty,
                        .backing = backing,
                    } };
                }
                break :blk record_shape;
            },
            .tuple => blk: {
                const shapes = try arena.alloc(Shape, components.len);
                for (shapes, components, component_shapes) |*shape, component, component_shape| {
                    shape.* = component_shape orelse .{ .any = component.ty };
                }
                break :blk Shape{ .tuple = .{
                    .ty = expr.ty,
                    .items = shapes,
                } };
            },
            .nominal => blk: {
                const backing_shape = component_shapes[0] orelse break :blk null;
                const stored = try arena.create(Shape);
                stored.* = backing_shape;
                break :blk Shape{ .nominal = .{
                    .ty = expr.ty,
                    .backing = stored,
                } };
            },
            .fn_ref => |fn_ref| blk: {
                const capture_shapes = try arena.alloc(Shape, components.len);
                for (capture_shapes, components, component_shapes) |*shape, component, component_shape| {
                    shape.* = component_shape orelse .{ .any = component.ty };
                }
                break :blk Shape{ .callable = .{
                    .ty = expr.ty,
                    .fn_id = fn_ref.fn_id,
                    .captures = capture_shapes,
                } };
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .let_, .lambda, .def_ref, .fn_def, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
        };
    }

    /// Total work budget for deriving one shape. Values reachable here are
    /// not always small finite trees—a loop-carried value can reference
    /// itself through the fixpoint of a recursive construction, and deep
    /// chains share substructure—so the walk spends one shared budget per
    /// node visit and degrades to `.any` (no known shape) when it runs out.
    /// `.any` is this function's existing "don't specialize on this" answer,
    /// so exhaustion is a missed specialization, never a wrong shape. See
    /// design.md "Core Principles" on bounded post-check walks.
    const shape_work_budget: u32 = 4096;

    fn shapeFromValue(self: *Pass, root: Value) Allocator.Error!ShapeProof {
        var budget: u32 = shape_work_budget;
        // Composite values wait on explicit frames for their components,
        // entered in pre-order so the budget reaches the same nodes a direct
        // walk does.
        const Frame = struct {
            value: Value,
            components: []const Value,
            proofs: []ShapeProof,
            next: usize = 0,
        };
        var frames = std.ArrayList(Frame).empty;
        defer {
            for (frames.items) |frame| {
                self.allocator.free(frame.components);
                self.allocator.free(frame.proofs);
            }
            frames.deinit(self.allocator);
        }
        var delivered: ShapeProof = undefined;
        switch (try self.enterShapeFromValue(root, &budget)) {
            .proof => |proof| return proof,
            .components => |entry| try frames.append(self.allocator, .{ .value = entry.value, .components = entry.components, .proofs = entry.proofs }),
        }
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next > 0) frame.proofs[frame.next - 1] = delivered;
            if (frame.next < frame.components.len) {
                const component = frame.components[frame.next];
                frame.next += 1;
                switch (try self.enterShapeFromValue(component, &budget)) {
                    .proof => |proof| delivered = proof,
                    .components => |entry| {
                        errdefer {
                            self.allocator.free(entry.components);
                            self.allocator.free(entry.proofs);
                        }
                        try frames.append(self.allocator, .{ .value = entry.value, .components = entry.components, .proofs = entry.proofs });
                    },
                }
                continue;
            }
            const finished = frames.pop().?;
            defer {
                self.allocator.free(finished.components);
                self.allocator.free(finished.proofs);
            }
            const proof = try self.finishShapeFromValue(finished.value, finished.components, finished.proofs);
            if (frames.items.len == 0) return proof;
            delivered = proof;
        }
    }

    const ShapeFromValueEntry = union(enum) {
        proof: ShapeProof,
        /// A composite value (wrappers stripped) and its components, each
        /// with a proof slot. Owned.
        components: struct { value: Value, components: []const Value, proofs: []ShapeProof },
    };

    /// Spend budget on a value and its wrappers, answering a leaf at once.
    fn enterShapeFromValue(self: *Pass, start: Value, budget: *u32) Allocator.Error!ShapeFromValueEntry {
        var value = start;
        while (true) {
            if (budget.* == 0) return .{ .proof = .unknown_budget_exhausted };
            budget.* -= 1;
            switch (value) {
                .expr => |expr| return .{ .proof = if (try self.constructorShape(expr)) |shape| .{ .proven = shape } else .disproven },
                .runtime_anchor => |anchor| value = anchor.structure.*,
                .static_data_candidate => |candidate| value = candidate.structure.*,
                .tag, .record, .tuple, .nominal, .callable => break,
            }
        }
        var components = std.ArrayList(Value).empty;
        errdefer components.deinit(self.allocator);
        switch (value) {
            .tag => |tag| try components.appendSlice(self.allocator, tag.payloads),
            .record => |record| for (record.fields) |field| try components.append(self.allocator, field.value),
            .tuple => |tuple| try components.appendSlice(self.allocator, tuple.items),
            .nominal => |nominal| try components.append(self.allocator, nominal.backing.*),
            .callable => |callable| for (callable.captures) |capture| try components.append(self.allocator, capture.value),
            .expr, .runtime_anchor, .static_data_candidate => unreachable,
        }
        const owned = try components.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(owned);
        return .{ .components = .{ .value = value, .components = owned, .proofs = try self.allocator.alloc(ShapeProof, owned.len) } };
    }

    /// A component's shape, or `.any` of its type when it has none.
    fn shapeOrAny(self: *Pass, proof: ShapeProof, component: Value) Shape {
        return switch (proof) {
            .proven => |shape| shape,
            .disproven, .unknown_budget_exhausted => .{ .any = valueType(self.program, component) },
        };
    }

    fn finishShapeFromValue(self: *Pass, value: Value, components: []const Value, proofs: []const ShapeProof) Allocator.Error!ShapeProof {
        const arena = self.arena.allocator();
        switch (value) {
            .tag => |tag| {
                const payloads = try arena.alloc(Shape, components.len);
                for (payloads, proofs, components) |*payload, proof, component| payload.* = self.shapeOrAny(proof, component);
                return .{ .proven = .{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } } };
            },
            .record => |record| {
                const fields = try arena.alloc(FieldShape, components.len);
                for (fields, record.fields, proofs, components) |*field, source, proof, component| {
                    field.* = .{ .name = source.name, .shape = self.shapeOrAny(proof, component) };
                }
                return .{ .proven = .{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } } };
            },
            .tuple => |tuple| {
                const items = try arena.alloc(Shape, components.len);
                for (items, proofs, components) |*item, proof, component| item.* = self.shapeOrAny(proof, component);
                return .{ .proven = .{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } } };
            },
            .nominal => |nominal| {
                const backing_shape = switch (proofs[0]) {
                    .proven => |shape| shape,
                    .disproven => return .disproven,
                    .unknown_budget_exhausted => return .unknown_budget_exhausted,
                };
                const stored = try arena.create(Shape);
                stored.* = backing_shape;
                return .{ .proven = .{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } } };
            },
            .callable => |callable| {
                const captures = try arena.alloc(Shape, components.len);
                for (captures, proofs, components) |*capture, proof, component| capture.* = self.shapeOrAny(proof, component);
                return .{ .proven = .{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } } };
            },
            .expr, .runtime_anchor, .static_data_candidate => unreachable,
        }
    }
};

/// One clone's substitution environment. It resolves a source local to its
/// known value through two maps and records every write on an undo log so a
/// scope's writes can be unwound at its boundary.
///
/// The exact-local map is keyed by `LocalId`. The other two maps are keyed by
/// `BinderIdentity`—the checked pattern binder together with the digest of the
/// local's monomorphic type, so two locals that share a binder but were
/// monomorphized at different types stay distinct bindings. `binder_aliases`
/// resolves every binder-equivalent local while cloning, for opaque and
/// structural values alike. `binder_subst` exposes only known structure and
/// loop-carried values to specialization decisions. Keeping those indexes
/// separate makes lexical identity independent of value shape without turning
/// an opaque binding into constructor evidence.
const Subst = struct {
    exact: collections.DenseMap(Ast.LocalId, Value),
    binder_subst: std.AutoHashMap(BinderIdentity, Value),
    binder_aliases: std.AutoHashMap(BinderIdentity, Value),
    /// Binder identities carried by an enclosing loop being cloned, with a
    /// nesting refcount. A carried variable's value must survive every `let`
    /// scope inside the loop body: the state-merge lowering binds a merged
    /// copy in a nested `let` whose lexical remainder is only the merge's
    /// syntactic result, yet the loop back edge reads that copy through its
    /// binder. Cloning a `let` value floats any update to a carried binder past its
    /// own restore so a later reference resolves to the merged value rather
    /// than the loop-entry value pinned at loop setup.
    loop_carried_binders: std.AutoHashMap(BinderIdentity, u32),
    changes: std.ArrayList(BindingChange),
    allocator: Allocator,

    fn init(allocator: Allocator) Subst {
        return .{
            .exact = collections.DenseMap(Ast.LocalId, Value).init(allocator),
            .binder_subst = std.AutoHashMap(BinderIdentity, Value).init(allocator),
            .binder_aliases = std.AutoHashMap(BinderIdentity, Value).init(allocator),
            .loop_carried_binders = std.AutoHashMap(BinderIdentity, u32).init(allocator),
            .changes = .empty,
            .allocator = allocator,
        };
    }

    fn deinit(self: *Subst) void {
        self.changes.deinit(self.allocator);
        self.loop_carried_binders.deinit();
        self.binder_aliases.deinit();
        self.binder_subst.deinit();
        self.exact.deinit();
    }

    /// Identity a local's binder-scoped substitution is keyed by: the pattern
    /// binder together with the digest of the local's monomorphic type. Two
    /// locals that share a binder but were monomorphized at different types are
    /// distinct bindings and must not read one another's substitution. Every
    /// local reference resolves through here, so the digest must come from the
    /// store's memoized construction, which is why these helpers need the
    /// program mutable.
    fn binderIdentityOf(program: *Ast.Program, local: Ast.LocalId) ?BinderIdentity {
        const local_data = program.getLocal(local);
        const binder = local_data.binder orelse return null;
        return .{
            .binder = binder,
            .digest = program.types.equalityDigest(&program.names, local_data.ty),
        };
    }

    /// Resolve a local to its known value through the exact-local map, then the
    /// binder-wide map.
    fn get(self: *const Subst, program: *Ast.Program, local: Ast.LocalId) ?Value {
        if (self.exact.get(local)) |value| return value;
        if (binderIdentityOf(program, local)) |identity| {
            if (self.binder_subst.get(identity)) |value| return value;
        }
        return null;
    }

    /// Resolve a local for emitted code, including the active value of a
    /// binder-equivalent Monotype local id.
    fn getForClone(self: *const Subst, program: *Ast.Program, local: Ast.LocalId) ?Value {
        if (self.exact.get(local)) |value| return value;
        if (binderIdentityOf(program, local)) |identity| {
            if (self.binder_aliases.get(identity)) |value| return value;
        }
        return null;
    }

    /// Resolve a local through the exact-local map only. The shape probes use
    /// this deliberately: they ask whether *this* local was substituted with a
    /// known value here, not whether its binder holds one somewhere.
    fn getExact(self: *const Subst, local: Ast.LocalId) ?Value {
        return self.exact.get(local);
    }

    /// The change-log length; pass it to `restore` to unwind every write made
    /// after this point.
    fn watermark(self: *const Subst) usize {
        return self.changes.items.len;
    }

    fn put(self: *Subst, program: *Ast.Program, local: Ast.LocalId, value: Value) Allocator.Error!void {
        try self.putExact(local, value);

        const identity = binderIdentityOf(program, local) orelse return;
        try self.putAlias(identity, value);
        const subst_binder = self.isLoopCarried(identity) or switch (value) {
            .tag,
            .record,
            .tuple,
            .nominal,
            .runtime_anchor,
            => true,
            .expr,
            .static_data_candidate,
            .callable,
            => false,
        };
        if (!subst_binder) return;
        const previous_binder = self.binder_subst.get(identity);
        try self.changes.append(self.allocator, .{
            .key = .{ .binder = identity },
            .previous = previous_binder,
        });
        try self.binder_subst.put(identity, value);
    }

    /// Install a substitution for one exact local id without touching its
    /// binder's maps. A binder-wide entry claims that every version of the
    /// variable resolves to this value, which holds only where the value is in
    /// scope. Pinning a binding whose scope is narrower than the position being
    /// cloned—a loop param, read from the loop's own initial values—must stay
    /// exact, or a sibling version of the same variable resolves to a binding
    /// that does not exist at its use site.
    fn putExact(self: *Subst, local: Ast.LocalId, value: Value) Allocator.Error!void {
        const previous = self.exact.get(local);
        try self.changes.append(self.allocator, .{
            .key = .{ .local = local },
            .previous = previous,
        });
        try self.exact.put(local, value);
    }

    fn putAlias(self: *Subst, identity: BinderIdentity, value: Value) Allocator.Error!void {
        const previous = self.binder_aliases.get(identity);
        try self.changes.append(self.allocator, .{
            .key = .{ .alias = identity },
            .previous = previous,
        });
        try self.binder_aliases.put(identity, value);
    }

    fn putLocalAlias(self: *Subst, program: *Ast.Program, local: Ast.LocalId, value: Value) Allocator.Error!void {
        const identity = binderIdentityOf(program, local) orelse return;
        try self.putAlias(identity, value);
    }

    /// Install a binder-wide substitution for a loop-carried slot. Reassigned
    /// copies of a carried variable share its source binder but not its local
    /// id, so binder identity is the only path they resolve through. Unlike
    /// `put`, the entry is written for any value variant: an opaque scalar
    /// param must reach those copies too, or they resolve to the dropped
    /// pre-loop local and capture recomputation turns the vanished binding into
    /// a phantom root argument.
    fn putLoopCarried(self: *Subst, identity: BinderIdentity, value: Value) Allocator.Error!void {
        try self.putAlias(identity, value);
        const previous = self.binder_subst.get(identity);
        try self.changes.append(self.allocator, .{
            .key = .{ .binder = identity },
            .previous = previous,
        });
        try self.binder_subst.put(identity, value);
    }

    /// Remove the pre-loop `binder_subst` value for the variable carried by a
    /// loop slot whose initial value is that variable, and return the slot's
    /// binder identity so the loop clone can install its param value under it.
    /// The removal is recorded on the change log so it is restored when the
    /// loop clone finishes. Returns null when the initial is not a bare
    /// binder-carrying local; the identity is returned whether or not a
    /// pre-loop entry existed, because the slot's reassigned copies resolve
    /// through it either way.
    fn dropCarriedBinder(self: *Subst, program: *Ast.Program, initial: Ast.ExprId) Allocator.Error!?BinderIdentity {
        const local = localExpr(program, initial) orelse return null;
        const identity = binderIdentityOf(program, local) orelse return null;
        if (self.binder_subst.get(identity)) |previous| {
            try self.changes.append(self.allocator, .{
                .key = .{ .binder = identity },
                .previous = previous,
            });
            _ = self.binder_subst.remove(identity);
        }
        if (self.binder_aliases.get(identity)) |previous| {
            try self.changes.append(self.allocator, .{
                .key = .{ .alias = identity },
                .previous = previous,
            });
            _ = self.binder_aliases.remove(identity);
        }
        return identity;
    }

    /// Whether an enclosing loop currently carries this binder.
    fn isLoopCarried(self: *const Subst, identity: BinderIdentity) bool {
        return self.loop_carried_binders.contains(identity);
    }

    /// Register a binder as carried by a loop being cloned. Nested loops that
    /// carry the same binder are counted so the marker survives until the
    /// outermost such loop finishes.
    fn markLoopCarried(self: *Subst, identity: BinderIdentity) Allocator.Error!void {
        const entry = try self.loop_carried_binders.getOrPut(identity);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    /// Drop one registration of a carried binder, removing it at zero.
    fn unmarkLoopCarried(self: *Subst, identity: BinderIdentity) void {
        const entry = self.loop_carried_binders.getPtr(identity) orelse return;
        if (entry.* <= 1) {
            _ = self.loop_carried_binders.remove(identity);
        } else {
            entry.* -= 1;
        }
    }

    /// Restore the change log to `start`, but re-apply the value each carried
    /// binder holds now so it survives this scope's teardown. A loop-carried
    /// binder's value escapes the `let` that binds it—the loop back edge
    /// reads it through its binder after the binding's lexical remainder ends—
    /// so its update floats out to the enclosing scope, where an outer restore
    /// (an arm boundary or the loop clone itself) still unwinds it.
    fn restoreFloatingLoopCarries(self: *Subst, start: usize) Allocator.Error!void {
        if (self.loop_carried_binders.count() == 0) return self.restore(start);
        var floated = std.ArrayList(struct { identity: BinderIdentity, value: Value }).empty;
        defer floated.deinit(self.allocator);
        for (self.changes.items[start..]) |change| {
            const identity = switch (change.key) {
                .binder => |identity| identity,
                .local, .alias => continue,
            };
            if (!self.isLoopCarried(identity)) continue;
            const value = self.binder_subst.get(identity) orelse continue;
            var seen = false;
            for (floated.items) |entry| {
                if (std.meta.eql(entry.identity, identity)) {
                    seen = true;
                    break;
                }
            }
            if (!seen) try floated.append(self.allocator, .{ .identity = identity, .value = value });
        }
        self.restore(start);
        for (floated.items) |entry| try self.putLoopCarried(entry.identity, entry.value);
    }

    fn restore(self: *Subst, start: usize) void {
        var index = self.changes.items.len;
        while (index > start) {
            index -= 1;
            const change = self.changes.items[index];
            switch (change.key) {
                .local => |local| {
                    if (change.previous) |previous| {
                        self.exact.putAssumeCapacity(local, previous);
                    } else {
                        _ = self.exact.remove(local);
                    }
                },
                .binder => |identity| {
                    if (change.previous) |previous| {
                        self.binder_subst.putAssumeCapacity(identity, previous);
                    } else {
                        _ = self.binder_subst.remove(identity);
                    }
                },
                .alias => |identity| {
                    if (change.previous) |previous| {
                        self.binder_aliases.putAssumeCapacity(identity, previous);
                    } else {
                        _ = self.binder_aliases.remove(identity);
                    }
                },
            }
        }
        self.changes.shrinkRetainingCapacity(start);
    }
};

const Cloner = struct {
    pass: *Pass,
    purpose: ClonePurpose,
    /// Only whole-body replacement may retain immutable source nodes: every
    /// other clone needs a disjoint expression identity space.
    source_reuse: SourceReuse,
    /// Symbolic values, shapes, and strict-binding chains owned by this clone.
    /// Accepted call patterns are copied into the pass-wide arena before this
    /// short-lived scratch arena is released.
    arena: std.heap.ArenaAllocator,
    source_fn: Ast.FnId,
    pattern: CallPattern,
    subst: Subst,
    inline_stack: std.ArrayList(InlineFrame),
    loop_stack: std.ArrayList(LoopPattern),
    /// Exit-ABI selection for each loop body currently being cloned,
    /// innermost last. A null frame preserves that loop's source exit ABI and
    /// shadows any selection owned by an enclosing loop.
    loop_exit_stack: std.ArrayList(?LoopExitSelection),
    exit_demands: ?*const ExitDemand.Inventory = null,
    exit_tuple_items: collections.DenseMap(Ast.LocalId, []?Ast.ExprId),
    join_stack: std.ArrayList(ActiveJoinClone),
    /// Remaining arms the shape-preserving let-of-case rewrite may still
    /// process. Each arm receives its own clone of the small dispatch, so on
    /// recursively generated code (derived parsers) the dispatch copies
    /// compound; this budget bounds them. When it runs out the rewrite
    /// retains the plain shared join, which clones no dispatch.
    let_case_shape_growth: CodeGrowthBudget,
    /// Active let-of-case join rewrites, innermost last. Cloning a jump whose
    /// target belongs to one of these frames records the jump site's symbolic
    /// argument values for later parameter decomposition instead of cloning
    /// the argument expressions directly.
    let_case_builds: std.ArrayList(*LetCaseBuild),
    /// Expression ids below this count existed before this clone began and
    /// are the only ids the clone may read as source. Every id at or above it
    /// was emitted by this clone, and emitted output carries rewrite decisions
    /// the inline stack and the growth budgets already made: a retained call
    /// is one an active frame declined, a selected arm is one the value
    /// evidence resolved. Reading output back as source would repeat those
    /// decisions outside the frames that justified them.
    output_start: usize,
    /// Expression ranges this clone built from source parts in order to
    /// clone them (a let-of-case dispatch, a `let` over a block's remaining
    /// statements, the block after a planned loop exit): the one kind of
    /// emitted expression the clone reads as source.
    clone_templates: std.ArrayList(ExprIdRange),
    /// The symbolic result value of every arm of each `match` or `if` this
    /// clone emitted from cloned arms, keyed by the emitted expression.
    /// Case-of-case distribution consumes these recorded values, so it never
    /// re-derives an arm by cloning emitted output.
    arm_values: collections.DenseMap(Ast.ExprId, []const Value),
    /// The symbolic value of the tail of each block this clone emitted
    /// statement by statement, keyed by the emitted block. A rewrite that
    /// reads an emitted arm's structure looks through such a block to its
    /// tail value while keeping the block's statements as they stand.
    block_tail_values: collections.DenseMap(Ast.ExprId, Value),
    /// Keys recorded into `arm_values` and `block_tail_values`, in order, for
    /// rejected loop attempt rollback. The rewind reuses the attempt's
    /// expression ids, so values recorded during the attempt must not
    /// outlive it.
    recorded_value_keys: std.ArrayList(RecordedValueKey),
    /// Fresh output locals bound by the recursive let statement whose value is
    /// currently cloning. A callable worker created while filling such a value
    /// must capture the recursive slot itself, not a field projected from it,
    /// so construction does not read the still-zeroed recursive payload.
    active_recursive_value_locals: collections.DenseMap(Ast.LocalId, void),
    rebased_inline_scopes: std.AutoHashMap(InlineScopeRebasePair, Ast.InlineScopeId),
    inline_scope_origins: collections.DenseMap(Ast.InlineScopeId, Ast.InlineScopeId),
    /// Insertions into the two inline-scope maps, in order, for rejected loop
    /// attempt rollback. Accepted entries remain useful for the rest of the clone.
    rebased_inline_scope_changes: std.ArrayList(InlineScopeRebasePair),
    /// Depth of the wrapper-strip recursion in the static value matchers
    /// (`bindPatToValue`/`bindPatToMatchValue`/`bindPatToFlowValue`), counting
    /// each `runtime_anchor.structure`/`nominal.backing`/
    /// `static_data_candidate.structure` pointer edge followed. A loop-carried
    /// value can reference itself through those edges, so an unbounded strip
    /// would hang; reaching `value_wrapper_strip_cap` declines the static
    /// decision toward a residual runtime match.
    wrapper_strip_depth: usize,
    /// Depth of the wrapper-strip recursion in `materialize`, counting each
    /// `nominal.backing`/callable-capture edge followed. Static candidates
    /// materialize their closed source expression without following the view.
    /// `materialize` runs on values proven acyclic by construction—a cyclic
    /// value is rebound through a plain source clone before it can
    /// reach here—so reaching `value_wrapper_strip_cap` is a compiler bug.
    materialize_strip_depth: usize,
    inline_calls: InlineCallMode,
    iterator_inline_depth: usize,
    inline_direct_requires_known_arg: bool,
    rewrite_call_patterns: bool,
    /// Pattern discovery and detect-only walks do not own output functions.
    /// Production clones reserve callable workers through the pass-wide table.
    emit_callable_workers: bool,
    /// Work left for case-of-case distribution in this clone. Each produced
    /// branch body spends one unit before its arm value is distributed, so
    /// nested distribution cannot multiply the expression store or recurse
    /// without spending this total growth budget.
    case_of_case_growth: CodeGrowthBudget,
    /// Remaining source-body work that this clone may inline. The per-function
    /// body-size gate bounds one expansion, but a small acyclic wrapper graph
    /// can still duplicate each child at every level and grow exponentially.
    /// Charging every accepted inline by its exact source-body size bounds the
    /// complete transitive expansion while retaining the ordinary call once
    /// the budget is spent.
    inline_body_growth: CodeGrowthBudget,
    current_loc: SourceLoc,
    current_region: Region,
    current_inline_scope: Ast.InlineScopeId,

    // Sized so realistic hot procedures never exhaust it: a saturated budget
    // leaves the remaining match-of-match results materialized as real tag
    // unions mid-procedure, whose per-iteration refcount pairs and payload
    // copies then poison every loop below the cutoff (measured 10-25%
    // slowdowns on deflate decode shapes at 256). The budget still bounds
    // pathological distribution cascades; it is generated-code fuel, not a
    // legality condition.
    const case_of_case_work_budget: u32 = 65536;
    // Sized like the case-of-case budget above: cumulative inlining in a
    // realistic hot procedure (a decode loop inlining its refill and append
    // helpers throughout) runs well past a few thousand size units, and a
    // saturated budget strands the remaining helpers as out-of-line calls in
    // the hottest paths. The bound still stops pathological cascades.
    const inline_body_work_budget: u32 = 65536;

    const SourceReuse = enum {
        none,
        original_body,
    };

    fn init(pass: *Pass, source_fn: Ast.FnId, pattern: CallPattern) Cloner {
        return .{
            .pass = pass,
            .purpose = .specialization,
            .source_reuse = .none,
            .arena = std.heap.ArenaAllocator.init(pass.allocator),
            .source_fn = source_fn,
            .pattern = pattern,
            .subst = Subst.init(pass.allocator),
            .inline_stack = .empty,
            .loop_stack = .empty,
            .loop_exit_stack = .empty,
            .exit_tuple_items = .init(pass.allocator),
            .join_stack = .empty,
            .let_case_shape_growth = .init(let_case_shape_arm_budget),
            .let_case_builds = .empty,
            .output_start = pass.program.exprCount(),
            .clone_templates = .empty,
            .arm_values = collections.DenseMap(Ast.ExprId, []const Value).init(pass.allocator),
            .block_tail_values = collections.DenseMap(Ast.ExprId, Value).init(pass.allocator),
            .recorded_value_keys = .empty,
            .active_recursive_value_locals = collections.DenseMap(Ast.LocalId, void).init(pass.allocator),
            .rebased_inline_scopes = std.AutoHashMap(InlineScopeRebasePair, Ast.InlineScopeId).init(pass.allocator),
            .inline_scope_origins = collections.DenseMap(Ast.InlineScopeId, Ast.InlineScopeId).init(pass.allocator),
            .rebased_inline_scope_changes = .empty,
            .wrapper_strip_depth = 0,
            .materialize_strip_depth = 0,
            .inline_calls = .all,
            .iterator_inline_depth = 0,
            .inline_direct_requires_known_arg = true,
            .rewrite_call_patterns = true,
            .emit_callable_workers = true,
            .case_of_case_growth = .init(case_of_case_work_budget),
            .inline_body_growth = .init(inline_body_work_budget),
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
            .current_inline_scope = Ast.InlineScopeId.none,
        };
    }

    fn initForRewrite(pass: *Pass) Cloner {
        return .{
            .pass = pass,
            .purpose = .rewrite,
            .source_reuse = .none,
            .arena = std.heap.ArenaAllocator.init(pass.allocator),
            .source_fn = undefined, // initForRewrite never calls buildArgs, which is the only reader.
            .pattern = .{ .args = &.{} },
            .subst = Subst.init(pass.allocator),
            .inline_stack = .empty,
            .loop_stack = .empty,
            .loop_exit_stack = .empty,
            .exit_tuple_items = .init(pass.allocator),
            .join_stack = .empty,
            .let_case_shape_growth = .init(let_case_shape_arm_budget),
            .let_case_builds = .empty,
            .output_start = pass.program.exprCount(),
            .clone_templates = .empty,
            .arm_values = collections.DenseMap(Ast.ExprId, []const Value).init(pass.allocator),
            .block_tail_values = collections.DenseMap(Ast.ExprId, Value).init(pass.allocator),
            .recorded_value_keys = .empty,
            .active_recursive_value_locals = collections.DenseMap(Ast.LocalId, void).init(pass.allocator),
            .rebased_inline_scopes = std.AutoHashMap(InlineScopeRebasePair, Ast.InlineScopeId).init(pass.allocator),
            .inline_scope_origins = collections.DenseMap(Ast.InlineScopeId, Ast.InlineScopeId).init(pass.allocator),
            .rebased_inline_scope_changes = .empty,
            .wrapper_strip_depth = 0,
            .materialize_strip_depth = 0,
            .inline_calls = .all,
            .iterator_inline_depth = 0,
            .inline_direct_requires_known_arg = false,
            .rewrite_call_patterns = true,
            .emit_callable_workers = true,
            .case_of_case_growth = .init(case_of_case_work_budget),
            .inline_body_growth = .init(inline_body_work_budget),
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
            .current_inline_scope = Ast.InlineScopeId.none,
        };
    }

    fn initForOriginalBodyRewrite(pass: *Pass) Cloner {
        var cloner = initForRewrite(pass);
        cloner.source_reuse = .original_body;
        return cloner;
    }

    fn initForLoopExitSelection(pass: *Pass) Cloner {
        var cloner = initForRewrite(pass);
        cloner.purpose = .loop_exit_selection;
        cloner.inline_calls = .none;
        cloner.rewrite_call_patterns = false;
        cloner.emit_callable_workers = false;
        return cloner;
    }

    fn deinit(self: *Cloner) void {
        self.inline_stack.deinit(self.pass.allocator);
        self.loop_stack.deinit(self.pass.allocator);
        self.loop_exit_stack.deinit(self.pass.allocator);
        self.exit_tuple_items.deinit();
        self.join_stack.deinit(self.pass.allocator);
        self.let_case_builds.deinit(self.pass.allocator);
        self.clone_templates.deinit(self.pass.allocator);
        self.arm_values.deinit();
        self.block_tail_values.deinit();
        self.recorded_value_keys.deinit(self.pass.allocator);
        self.active_recursive_value_locals.deinit();
        self.rebased_inline_scopes.deinit();
        self.inline_scope_origins.deinit();
        self.rebased_inline_scope_changes.deinit(self.pass.allocator);
        self.subst.deinit();
        self.arena.deinit();
    }

    /// Debug validator for the clone-source invariant described on
    /// `output_start`: a clone reads only expressions that existed when it
    /// began, plus its own registered clone templates.
    fn assertSourceExpr(self: *const Cloner, expr_id: Ast.ExprId) void {
        if (!std.debug.runtime_safety) return;
        const raw = @intFromEnum(expr_id);
        if (raw < self.output_start) return;
        for (self.clone_templates.items) |range| {
            if (raw >= range.start and raw < range.end) return;
        }
        Common.invariant("SpecConstr clone read an expression it emitted as source");
    }

    /// Register the expressions added since `start` as a template this clone
    /// built from source parts in order to clone it, so `assertSourceExpr`
    /// admits reading them.
    fn registerCloneTemplate(self: *Cloner, start: usize) Allocator.Error!void {
        try self.clone_templates.append(self.pass.allocator, .{
            .start = start,
            .end = self.pass.program.exprCount(),
        });
    }

    /// Remember the result value of every arm of an emitted `match` or `if`
    /// so case-of-case distribution can consume it without re-deriving the
    /// arm from output.
    fn recordArmValues(self: *Cloner, emitted: Ast.ExprId, values: []const Value) Allocator.Error!void {
        if (@intFromEnum(emitted) < self.output_start) {
            Common.invariant("arm values were recorded for an expression this clone did not emit");
        }
        try self.arm_values.putNoClobber(emitted, values);
        try self.recorded_value_keys.append(self.pass.allocator, .{ .arm_values = emitted });
    }

    /// Remember the tail value of a block emitted statement by statement.
    fn recordBlockTailValue(self: *Cloner, emitted: Ast.ExprId, value: Value) Allocator.Error!void {
        if (@intFromEnum(emitted) < self.output_start) {
            Common.invariant("a block tail value was recorded for an expression this clone did not emit");
        }
        try self.block_tail_values.putNoClobber(emitted, value);
        try self.recorded_value_keys.append(self.pass.allocator, .{ .block_tail = emitted });
    }

    /// The structure an emitted arm contributes to a rewrite: its recorded
    /// value, read through an emitted block to the tail value recorded for
    /// that block.
    fn emittedArmStructure(self: *const Cloner, arm_body: Ast.ExprId, recorded: Value) Value {
        if (recorded != .expr or recorded.expr != arm_body) return recorded;
        return self.block_tail_values.get(arm_body) orelse recorded;
    }

    fn admitInlineBodyGrowth(self: *Cloner, body_size: BodySize) bool {
        const exact_size = body_size.exactValue() orelse return false;
        return self.inline_body_growth.admit(@max(exact_size, 1)) == .admitted;
    }

    // Cloning //
    //
    // Cloning an expression clones its subexpressions, and materializing a
    // value materializes its parts, from inside the construct's own clone.
    // Every cloning computation that clones a subexpression, materializes a
    // nested value, or binds a nested pattern suspends as a `CloneFrame` on
    // one heap-backed stack and resumes with the child's result, so
    // expression, value, and pattern nesting never become native call depth.
    // Frames make the same cloning calls, in the same order, as a direct
    // recursive clone would, and restore the cloner state they change when
    // they finish. An allocation failure abandons the whole clone, so no
    // frame restores state while unwinding.

    const CloneResult = union(enum) {
        none,
        value: Value,
        maybe_value: ?Value,
        expr: Ast.ExprId,
        maybe_expr: ?Ast.ExprId,
        data: Ast.ExprData,
        maybe_data: ?Ast.ExprData,
        parts: ClonedParts,
        arm: ClonedArm,
        maybe_arm: ?ClonedArm,
        cloned: ClonedValue,
        flag: bool,
        stmt: ClonedStmt,
        expr_span: Ast.Span(Ast.ExprId),
        stmt_span: Ast.Span(Ast.StmtId),
        capture_span: Ast.Span(Ast.CaptureOperand),
        field_span: Ast.Span(Ast.FieldExpr),
        branches: ClonedBranches,
        if_branches: ClonedIfBranches,
        supplied: SuppliedSlot,
        pieces: ?LetCaseJoinPieces,

        fn get(self: CloneResult, comptime tag: std.meta.Tag(CloneResult)) @FieldType(CloneResult, @tagName(tag)) {
            if (std.meta.activeTag(self) != tag) Common.invariant("SpecConstr clone frame received the wrong result kind");
            return @field(self, @tagName(tag));
        }
    };

    const CloneFrame = struct {
        cursor: u8 = 0,
        index: usize = 0,
        task: CloneTask,
    };

    const CloneStep = union(enum) {
        /// Suspend this frame until `task` returns.
        call: CloneTask,
        /// Replace this frame by `task`, whose result is this frame's result.
        tail: CloneTask,
        ret: CloneResult,
    };

    /// Run a cloning computation to completion on an explicit frame stack.
    fn runClone(self: *Cloner, root: CloneTask) Common.LowerError!CloneResult {
        var frames: std.ArrayList(CloneFrame) = .empty;
        defer frames.deinit(self.pass.allocator);
        errdefer for (frames.items) |*frame| self.releaseCloneFrame(frame);
        try frames.append(self.pass.allocator, .{ .task = root });
        var input: ?CloneResult = null;
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            switch (try self.stepClone(frame, input)) {
                .call => |task| {
                    try frames.append(self.pass.allocator, .{ .task = task });
                    input = null;
                },
                .tail => |task| {
                    frame.* = .{ .task = task };
                    input = null;
                },
                .ret => |result| {
                    _ = frames.pop();
                    if (frames.items.len == 0) return result;
                    input = result;
                },
            }
        }
    }

    /// Free the scratch lists a frame owns outside the clone arena.
    /// Idempotent, so a frame that already released a list on its normal
    /// path is unaffected.
    fn releaseCloneFrame(self: *Cloner, frame: *CloneFrame) void {
        const allocator = self.pass.allocator;
        switch (frame.task) {
            .block_value => |*task| if (task.statements_owned) {
                task.statements.deinit(allocator);
                task.statements_owned = false;
            },
            .exit_block_value => |*task| if (task.statements_owned) {
                task.statements.deinit(allocator);
                task.statements_owned = false;
            },
            .let_case_arm_body => |*task| {
                task.emitted_statements.deinit(allocator);
                task.emitted_statements = .empty;
            },
            .distribute_arm => |*task| if (task.statements_owned) {
                task.statements.deinit(allocator);
                task.statements_owned = false;
            },
            .continue_ => |*task| if (task.new_values_owned) {
                task.new_values.deinit(allocator);
                task.new_values_owned = false;
            },
            .loop_value => |*task| if (task.attempt_lists_owned) {
                task.new_params.deinit(allocator);
                task.new_initials.deinit(allocator);
                task.attempt_lists_owned = false;
            },
            .call_proc => |*task| if (task.out_owned) {
                task.out.deinit(allocator);
                task.out_owned = false;
            },
            .stmt_span => |*task| {
                task.values.deinit(allocator);
                task.values = .empty;
            },
            .collect => |*task| {
                task.items.deinit(allocator);
                task.items = .empty;
                task.scopes.deinit(allocator);
                task.scopes = .empty;
            },
            .expr, .keeping, .parts, .without_reuse, .expr_value_owned, .expr_value, .demanding, .callable_from_function_value, .wrapped_value, .plain, .join_point, .let_value, .let_with_value, .bind_let_value, .positioned_reusable, .loop_exit_values, .loop_body, .selected_loop_exit, .make_reusable, .let_of_case, .let_of_case_shared, .divergent_tail, .capture_let_case_jump, .inline_let_case_join, .finalize_let_case_join, .rebuild_let_case_join_value, .emit_block_with_tail, .append_exprs_from_value, .supply_loop_slot, .field_access_value, .tuple_access, .match, .select_known_match, .bind_pat_to_match_value, .local_value, .inline_value, .case_of_case, .distribute_value, .inline_callable, .inline_direct, .wrap_typed_boundary, .stmt, .expr_span, .capture_span, .field_span, .span, .branch_span, .if_branch_span, .materialize, .materialize_callable, .materialize_worker, .materialize_with_captures, .value_flow_bind => {},
        }
    }

    /// A fresh strict chain for a frame that collects its own bindings.
    fn newChain(self: *Cloner) Allocator.Error!*BindingChain {
        const chain = try self.arena.allocator().create(BindingChain);
        chain.* = .{};
        return chain;
    }

    fn retValue(value: Value) CloneStep {
        return .{ .ret = .{ .value = value } };
    }

    fn retExpr(expr: Ast.ExprId) CloneStep {
        return .{ .ret = .{ .expr = expr } };
    }

    /// Move the cloner's source location and inline scope to `expr_id`,
    /// returning what to restore.
    fn enterExprSource(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!SourceContext {
        const saved = SourceContext{
            .loc = self.current_loc,
            .region = self.current_region,
            .inline_scope = self.current_inline_scope,
        };
        try self.adoptExprInlineScope(expr_id);
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;
        return saved;
    }

    // Wrappers running one cloning computation for callers outside the
    // cloning frames.

    fn cloneExpr(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Ast.ExprId {
        return (try self.runClone(.{ .expr = expr_id })).get(.expr);
    }

    fn cloneExprValue(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!ClonedValue {
        return (try self.runClone(.{ .expr_value_owned = .{ .expr = expr_id, .demand_shape = false } })).get(.cloned);
    }

    fn cloneExprValueDemandingShape(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!ClonedValue {
        return (try self.runClone(.{ .expr_value_owned = .{ .expr = expr_id, .demand_shape = true } })).get(.cloned);
    }

    fn cloneExprPlain(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Ast.ExprId {
        return (try self.runClone(.{ .plain = .{ .expr = expr_id } })).get(.expr);
    }

    fn materialize(self: *Cloner, value: Value) Common.LowerError!Ast.ExprId {
        return (try self.runClone(.{ .materialize = .{ .value = value } })).get(.expr);
    }

    fn collectCallPatternsInExpr(self: *Cloner, owner: Ast.FnId, expr_id: Ast.ExprId) Common.LowerError!void {
        _ = (try self.runClone(.{ .collect = .{ .owner = owner, .expr = expr_id } })).get(.none);
    }

    fn appendExprsFromValue(self: *Cloner, shape: Shape, value: Value, out: *std.ArrayList(Ast.ExprId)) Common.LowerError!void {
        _ = (try self.runClone(.{ .append_exprs_from_value = .{ .shape = shape, .value = value, .out = out } })).get(.none);
    }

    fn bindPatToMatchValue(self: *Cloner, pat_id: Ast.PatId, value: Value, body: Ast.ExprId, bindings: *BindingChain) Common.LowerError!?Value {
        return (try self.runClone(.{ .bind_pat_to_match_value = .{ .pat = pat_id, .value = value, .body = body, .bindings = bindings } })).get(.maybe_value);
    }

    fn callableValueFromRef(
        self: *Cloner,
        ty: Type.TypeId,
        fn_ref: @import("../monotype/ast.zig").LiftedFunctionValue,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        return (try self.runClone(.{ .callable_from_function_value = .{ .ty = ty, .fn_ref = fn_ref, .bindings = bindings } })).get(.value);
    }

    fn simplifyKnownMatchValue(self: *Cloner, scrutinee: Value, branches: Ast.Span(Ast.Branch), bindings: *BindingChain) Common.LowerError!?Value {
        return (try self.runClone(.{ .select_known_match = .{ .scrutinee = scrutinee, .branches = branches, .bindings = bindings } })).get(.maybe_value);
    }

    fn cloneSelectedLoopExit(self: *Cloner, break_ty: Type.TypeId, value_expr: Ast.ExprId, selection: LoopExitSelection) Common.LowerError!Ast.ExprId {
        return (try self.runClone(.{ .selected_loop_exit = .{ .break_ty = break_ty, .value_expr = value_expr, .selection = selection } })).get(.expr);
    }

    fn cloneLoopWithSelectedExit(self: *Cloner, ty: Type.TypeId, loop: @FieldType(Ast.ExprData, "loop_"), selection: LoopExitSelection) Common.LowerError!Ast.ExprId {
        const chain = try self.newChain();
        return (try self.runClone(try self.wrappedValueTask(.{ .loop_value = .{ .ty = ty, .loop = loop, .bindings = chain, .exit_selection = selection } }, chain))).get(.expr);
    }

    const CloneTask = union(enum) {
        /// `cloneExpr`
        expr: Ast.ExprId,
        /// An expression cloned together with its value.
        keeping: KeepingTask,
        /// An expression's parts cloned into a rebuilt expression.
        parts: PartsTask,
        without_reuse: WithoutReuseTask,
        /// `cloneExprValue` and `cloneExprValueDemandingShape`
        expr_value_owned: ExprValueOwnedTask,
        /// An expression's value cloned into a caller's slot.
        expr_value: ExprValueTask,
        /// An expression's value cloned into a caller's slot, demanding its
        /// shape.
        demanding: DemandingTask,
        callable_from_function_value: CallableFromFunctionValueTask,
        wrapped_value: WrappedValueTask,
        /// `cloneExprPlain`
        plain: PlainTask,
        join_point: JoinPointTask,
        let_value: LetValueTask,
        let_with_value: LetWithValueTask,
        bind_let_value: BindLetValueTask,
        positioned_reusable: PositionedReusableTask,
        loop_exit_values: LoopExitValuesTask,
        loop_body: LoopBodyTask,
        selected_loop_exit: SelectedLoopExitTask,
        /// `makeReusableForMatch`
        make_reusable: MakeReusableTask,
        let_of_case: LetOfCaseTask,
        let_of_case_shared: LetOfCaseSharedTask,
        let_case_arm_body: LetCaseArmBodyTask,
        divergent_tail: DivergentTailTask,
        capture_let_case_jump: CaptureLetCaseJumpTask,
        inline_let_case_join: InlineLetCaseJoinTask,
        finalize_let_case_join: FinalizeLetCaseJoinTask,
        rebuild_let_case_join_value: RebuildLetCaseJoinValueTask,
        loop_value: LoopValueTask,
        block_value: BlockValueTask,
        exit_block_value: ExitBlockValueTask,
        emit_block_with_tail: EmitBlockWithTailTask,
        continue_: ContinueTask,
        call_proc: CallProcTask,
        append_exprs_from_value: AppendExprsTask,
        supply_loop_slot: SupplyLoopSlotTask,
        field_access_value: FieldAccessValueTask,
        tuple_access: TupleAccessTask,
        match: MatchTask,
        select_known_match: SelectKnownMatchTask,
        bind_pat_to_match_value: BindPatToMatchValueTask,
        /// The value of a local bound by a match or an inlined call.
        local_value: LocalValueTask,
        /// An inlined value cloned without following a cyclic definition.
        inline_value: InlineValueTask,
        case_of_case: CaseOfCaseTask,
        distribute_arm: DistributeArmTask,
        distribute_value: DistributeValueTask,
        inline_callable: InlineCallableTask,
        inline_direct: InlineDirectTask,
        wrap_typed_boundary: WrapTypedBoundaryTask,
        stmt: StmtTask,
        expr_span: struct { span: Ast.Span(Ast.ExprId) },
        capture_span: struct { span: Ast.Span(Ast.CaptureOperand) },
        field_span: struct { span: Ast.Span(Ast.FieldExpr) },
        span: SpanTask,
        stmt_span: StmtSpanTask,
        branch_span: BranchSpanTask,
        if_branch_span: IfBranchSpanTask,
        materialize: MaterializeTask,
        materialize_callable: struct { callable: CallableValue },
        materialize_worker: MaterializeWorkerTask,
        materialize_with_captures: MaterializeWithCapturesTask,
        value_flow_bind: ValueFlowBindTask,
        collect: CollectTask,
    };

    fn stepClone(self: *Cloner, frame: *CloneFrame, input: ?CloneResult) Common.LowerError!CloneStep {
        return switch (frame.task) {
            .expr => |expr_id| stepCloneExpr(frame, expr_id, input),
            .keeping => |*task| self.stepKeeping(frame, task, input),
            .parts => |*task| self.stepParts(frame, task, input),
            .without_reuse => |*task| self.stepWithoutReuse(frame, task, input),
            .expr_value_owned => |*task| self.stepExprValueOwned(frame, task, input),
            .expr_value => |*task| self.stepExprValue(frame, task, input),
            .demanding => |*task| self.stepDemanding(frame, task, input),
            .callable_from_function_value => |*task| self.stepCallableFromFunctionValue(frame, task, input),
            .wrapped_value => |*task| self.stepWrappedValue(frame, task, input),
            .plain => |*task| self.stepPlain(frame, task, input),
            .join_point => |*task| self.stepJoinPoint(frame, task, input),
            .let_value => |*task| self.stepLetValue(frame, task, input),
            .let_with_value => |*task| self.stepLetWithValue(frame, task, input),
            .bind_let_value => |*task| self.stepBindLetValue(task),
            .positioned_reusable => |*task| self.stepPositionedReusable(frame, task, input),
            .loop_exit_values => |*task| self.stepLoopExitValues(frame, task, input),
            .loop_body => |*task| self.stepLoopBody(frame, task, input),
            .selected_loop_exit => |*task| self.stepSelectedLoopExit(frame, task, input),
            .make_reusable => |*task| self.stepMakeReusable(frame, task, input),
            .let_of_case => |*task| self.stepLetOfCase(frame, task, input),
            .let_of_case_shared => |*task| self.stepLetOfCaseShared(frame, task, input),
            .let_case_arm_body => |*task| self.stepLetCaseArmBody(frame, task, input),
            .divergent_tail => |*task| self.stepDivergentTail(frame, task, input),
            .capture_let_case_jump => |*task| self.stepCaptureLetCaseJump(frame, task, input),
            .inline_let_case_join => |*task| self.stepInlineLetCaseJoin(frame, task, input),
            .finalize_let_case_join => |*task| self.stepFinalizeLetCaseJoin(frame, task, input),
            .rebuild_let_case_join_value => |*task| self.stepRebuildLetCaseJoinValue(frame, task, input),
            .loop_value => |*task| self.stepLoopValue(frame, task, input),
            .block_value => |*task| self.stepBlockValue(frame, task, input),
            .exit_block_value => |*task| self.stepExitBlockValue(frame, task, input),
            .emit_block_with_tail => |*task| self.stepEmitBlockWithTail(frame, task, input),
            .continue_ => |*task| self.stepContinue(frame, task, input),
            .call_proc => |*task| self.stepCallProc(frame, task, input),
            .append_exprs_from_value => |*task| self.stepAppendExprs(frame, task, input),
            .supply_loop_slot => |*task| self.stepSupplyLoopSlot(frame, task, input),
            .field_access_value => |*task| self.stepFieldAccessValue(frame, task, input),
            .tuple_access => |*task| self.stepTupleAccess(frame, task, input),
            .match => |*task| self.stepMatch(frame, task, input),
            .select_known_match => |*task| self.stepSelectKnownMatch(frame, task, input),
            .bind_pat_to_match_value => |*task| self.stepBindPatToMatchValue(frame, task, input),
            .local_value => |*task| self.stepLocalValue(frame, task, input),
            .inline_value => |*task| self.stepInlineValue(frame, task, input),
            .case_of_case => |*task| self.stepCaseOfCase(frame, task, input),
            .distribute_arm => |*task| self.stepDistributeArm(frame, task, input),
            .distribute_value => |*task| stepDistributeValue(frame, task, input),
            .inline_callable => |*task| self.stepInlineCallable(frame, task, input),
            .inline_direct => |*task| self.stepInlineDirect(frame, task, input),
            .wrap_typed_boundary => |*task| self.stepWrapTypedBoundary(frame, task, input),
            .stmt => |*task| self.stepStmt(frame, task, input),
            .expr_span => |task| .{ .tail = .{ .span = .{ .span = .{ .exprs = task.span } } } },
            .capture_span => |task| .{ .tail = .{ .span = .{ .span = .{ .captures = task.span } } } },
            .field_span => |task| .{ .tail = .{ .span = .{ .span = .{ .fields = task.span } } } },
            .span => |*task| self.stepSpan(frame, task, input),
            .stmt_span => |*task| self.stepStmtSpan(frame, task, input),
            .branch_span => |*task| self.stepBranchSpan(frame, task, input),
            .if_branch_span => |*task| self.stepIfBranchSpan(frame, task, input),
            .materialize => |*task| self.stepMaterialize(frame, task, input),
            .materialize_callable => |task| self.stepMaterializeCallable(task.callable),
            .materialize_worker => |*task| self.stepMaterializeWorker(frame, task, input),
            .materialize_with_captures => |*task| self.stepMaterializeWithCaptures(frame, task, input),
            .value_flow_bind => |*task| self.stepValueFlowBind(frame, task, input),
            .collect => |*task| self.stepCollect(frame, task, input),
        };
    }

    fn stepCloneExpr(frame: *CloneFrame, expr_id: Ast.ExprId, input: ?CloneResult) CloneStep {
        if (frame.cursor == 0) {
            frame.cursor = 1;
            return .{ .call = .{ .keeping = .{ .expr = expr_id } } };
        }
        return retExpr(input.?.get(.arm).body);
    }

    /// Clone an expression to output and also return the symbolic value of
    /// its result, for callers that emit the result as a branch arm and must
    /// record that value for case-of-case distribution.
    const KeepingTask = struct { expr: Ast.ExprId, parts: ClonedParts = undefined };

    fn stepKeeping(self: *Cloner, frame: *CloneFrame, task: *KeepingTask, input: ?CloneResult) Common.LowerError!CloneStep {
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = .{ .parts = .{ .expr = task.expr } } };
            },
            1 => {
                task.parts = input.?.get(.parts);
                if (task.parts.reused) |reused| return .{ .ret = .{ .arm = .{ .body = reused, .value = task.parts.value } } };
                frame.cursor = 2;
                return .{ .call = .{ .materialize = .{ .value = task.parts.value } } };
            },
            else => return .{ .ret = .{ .arm = .{
                .body = try self.wrapBindings(task.parts.bindings, input.?.get(.expr)),
                .value = task.parts.value,
            } } },
        }
    }

    /// Clone an expression and hand back its strict chain unplaced, for a
    /// caller that places the chain in its own statement list so the value's
    /// leaves stay in that list's scope.
    const PartsTask = struct { expr: Ast.ExprId, saved: SourceContext = undefined };

    fn stepParts(self: *Cloner, frame: *CloneFrame, task: *PartsTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            self.assertSourceExpr(task.expr);
            task.saved = try self.enterExprSource(task.expr);
            frame.cursor = 1;
            return .{ .call = .{ .expr_value_owned = .{ .expr = task.expr, .demand_shape = false } } };
        }
        const cloned = input.?.get(.cloned);
        const reused = cloned.bindings.isEmpty() and
            self.canReuseOriginalExpr(task.expr) and
            try self.valueMatchesSourceExpr(cloned.value, task.expr, 0);
        task.saved.restore(self);
        return .{ .ret = .{ .parts = .{
            .reused = if (reused) task.expr else null,
            .bindings = cloned.bindings,
            .value = cloned.value,
        } } };
    }

    /// A cloning computation run with source reuse disabled.
    const WithoutReuseTask = struct {
        child: *const CloneTask,
        saved: SourceReuse = undefined,
    };

    fn stepWithoutReuse(self: *Cloner, frame: *CloneFrame, task: *WithoutReuseTask, input: ?CloneResult) CloneStep {
        if (frame.cursor == 0) {
            task.saved = self.source_reuse;
            self.source_reuse = .none;
            frame.cursor = 1;
            return .{ .call = task.child.* };
        }
        self.source_reuse = task.saved;
        return .{ .ret = input.? };
    }

    fn withoutReuseTask(self: *Cloner, child: CloneTask) Allocator.Error!CloneTask {
        const stored = try self.arena.allocator().create(CloneTask);
        stored.* = child;
        return .{ .without_reuse = .{ .child = stored } };
    }

    /// `cloneExprValue` or `cloneExprValueDemandingShape`: the value together
    /// with the strict chain it produced.
    const ExprValueOwnedTask = struct { expr: Ast.ExprId, demand_shape: bool, chain: *BindingChain = undefined };

    fn stepExprValueOwned(self: *Cloner, frame: *CloneFrame, task: *ExprValueOwnedTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            task.chain = try self.newChain();
            frame.cursor = 1;
            return .{ .call = if (task.demand_shape)
                .{ .demanding = .{ .expr = task.expr, .bindings = task.chain } }
            else
                .{ .expr_value = .{ .expr = task.expr, .bindings = task.chain } } };
        }
        return .{ .ret = .{ .cloned = .{ .bindings = task.chain.*, .value = input.?.get(.value) } } };
    }

    const ExprValueTask = struct {
        expr: Ast.ExprId,
        bindings: *BindingChain,
        saved: SourceContext = undefined,
        /// Owned by the clone arena: the source children being cloned.
        sources: []const Ast.ExprId = &.{},
        source_fields: []const Ast.FieldExpr = &.{},
        values: []Value = &.{},
        fields: []FieldValue = &.{},
        result_type_fields: []const Type.Field = &.{},
        value: Value = undefined,
        other: Value = undefined,
        expr_out: Ast.ExprId = undefined,
        binding_mark: ?*BindingNode = null,
        enters_iterator: bool = false,
    };

    fn finishExprValue(self: *Cloner, task: *ExprValueTask, value: Value) CloneStep {
        task.saved.restore(self);
        return retValue(value);
    }

    fn stepExprValue(self: *Cloner, frame: *CloneFrame, task: *ExprValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const expr_id = task.expr;
        const bindings = task.bindings;
        if (frame.cursor == 0) {
            self.assertSourceExpr(expr_id);
            task.saved = try self.enterExprSource(expr_id);
        }
        const expr = self.pass.program.getExpr(expr_id);
        switch (expr.data) {
            .local => |local| {
                if (frame.cursor != 0) return self.finishExprValue(task, input.?.get(.value));
                if (self.subst.getForClone(self.pass.program, local)) |value| {
                    if (!sameType(self.pass.program, expr.ty, valueType(self.pass.program, value))) {
                        frame.cursor = 1;
                        return .{ .call = .{ .wrap_typed_boundary = .{ .target_ty = expr.ty, .structure = value } } };
                    }
                    const resolved_local = switch (value) {
                        .expr => |resolved| localExpr(self.pass.program, resolved),
                        .runtime_anchor => |anchor| localExpr(self.pass.program, anchor.runtime),
                        .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => null,
                    };
                    if (resolved_local != null and
                        resolved_local.? == local and
                        self.canReuseOriginalExpr(expr_id))
                    {
                        return self.finishExprValue(task, .{ .expr = expr_id });
                    }
                    return self.finishExprValue(task, value);
                }
                if (self.canReuseOriginalExpr(expr_id)) return self.finishExprValue(task, .{ .expr = expr_id });
                return self.finishExprValue(task, .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } }) });
            },
            .fn_ref => |fn_ref| {
                if (frame.cursor != 0) return self.finishExprValue(task, input.?.get(.value));
                frame.cursor = 1;
                return .{ .call = .{ .callable_from_function_value = .{ .ty = expr.ty, .fn_ref = fn_ref, .bindings = bindings } } };
            },
            .static_data_candidate => return self.finishExprValue(task, (try self.pass.staticDataStructure(expr_id)).*),
            .comptime_value => return self.finishExprValue(task, .{ .expr = expr_id }),
            .typed_boundary => |boundary| switch (frame.cursor) {
                0 => {
                    frame.cursor = 1;
                    return .{ .call = .{ .demanding = .{ .expr = boundary.value, .bindings = bindings } } };
                },
                1 => {
                    task.value = input.?.get(.value);
                    frame.cursor = 2;
                    return .{ .call = .{ .materialize = .{ .value = task.value } } };
                },
                else => {
                    const source = input.?.get(.expr);
                    const runtime = try self.addExpr(.{ .ty = expr.ty, .data = .{ .typed_boundary = .{ .value = source } } });
                    return self.finishExprValue(task, try self.typedBoundaryValue(task.value, runtime));
                },
            },
            .tag => |tag| {
                if (frame.cursor == 0) {
                    assertStructuralConstructionType(self.pass.program, expr.ty);
                    task.sources = try GuardedList.dupe(self.arena.allocator(), Ast.ExprId, self.pass.program.exprSpan(tag.payloads));
                    task.values = try self.arena.allocator().alloc(Value, task.sources.len);
                    frame.cursor = 1;
                } else {
                    task.values[frame.index] = input.?.get(.value);
                    frame.index += 1;
                }
                if (frame.index < task.sources.len) {
                    return .{ .call = .{ .demanding = .{ .expr = task.sources[frame.index], .bindings = bindings } } };
                }
                return self.finishExprValue(task, .{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = task.values,
                } });
            },
            .record => |fields_span| {
                if (frame.cursor == 0) {
                    assertStructuralConstructionType(self.pass.program, expr.ty);
                    task.source_fields = try GuardedList.dupe(self.arena.allocator(), Ast.FieldExpr, self.pass.program.fieldExprSpan(fields_span));
                    task.fields = try self.arena.allocator().alloc(FieldValue, task.source_fields.len);
                    frame.cursor = 1;
                } else {
                    task.fields[frame.index] = .{
                        .name = task.source_fields[frame.index].name,
                        .value = input.?.get(.value),
                    };
                    frame.index += 1;
                }
                if (frame.index < task.source_fields.len) {
                    return .{ .call = .{ .demanding = .{ .expr = task.source_fields[frame.index].value, .bindings = bindings } } };
                }
                return self.finishExprValue(task, .{ .record = .{
                    .ty = expr.ty,
                    .fields = task.fields,
                } });
            },
            .record_update => |update| return try self.stepRecordUpdateValue(frame, task, expr, update, input),
            .tuple => |items_span| {
                if (frame.cursor == 0) {
                    assertStructuralConstructionType(self.pass.program, expr.ty);
                    task.sources = try GuardedList.dupe(self.arena.allocator(), Ast.ExprId, self.pass.program.exprSpan(items_span));
                    task.values = try self.arena.allocator().alloc(Value, task.sources.len);
                    frame.cursor = 1;
                } else {
                    task.values[frame.index] = input.?.get(.value);
                    frame.index += 1;
                }
                if (frame.index < task.sources.len) {
                    return .{ .call = .{ .demanding = .{ .expr = task.sources[frame.index], .bindings = bindings } } };
                }
                return self.finishExprValue(task, .{ .tuple = .{
                    .ty = expr.ty,
                    .items = task.values,
                } });
            },
            .nominal => |backing| {
                if (frame.cursor == 0) {
                    frame.cursor = 1;
                    return .{ .call = .{ .demanding = .{ .expr = backing, .bindings = bindings } } };
                }
                return self.finishExprValue(task, .{ .nominal = .{
                    .ty = expr.ty,
                    .backing = try self.copyValue(input.?.get(.value)),
                } });
            },
            .let_ => |let_| {
                if (frame.cursor != 0) return self.finishExprValue(task, input.?.get(.value));
                frame.cursor = 1;
                return .{ .call = .{ .let_value = .{ .let_ = LetParts.fromExpr(let_), .bindings = bindings } } };
            },
            .loop_ => |loop| {
                if (frame.cursor != 0) return self.finishExprValue(task, input.?.get(.value));
                frame.cursor = 1;
                return .{ .call = .{ .loop_value = .{ .ty = expr.ty, .loop = loop, .bindings = bindings, .exit_selection = null } } };
            },
            .block => |block| {
                if (frame.cursor != 0) return self.finishExprValue(task, input.?.get(.value));
                frame.cursor = 1;
                return .{ .call = .{ .block_value = .{ .ty = expr.ty, .block = block, .bindings = bindings } } };
            },
            .field_access => |field| {
                if (frame.cursor != 0) return self.finishExprValue(task, input.?.get(.value));
                frame.cursor = 1;
                return .{ .call = .{ .field_access_value = .{ .original_expr = expr_id, .ty = expr.ty, .field = field, .bindings = bindings } } };
            },
            .tuple_access => |access| switch (frame.cursor) {
                0 => {
                    if (self.selectedTupleItem(access)) |item| return self.finishExprValue(task, .{ .expr = item });
                    task.binding_mark = bindings.mark();
                    frame.cursor = 1;
                    return .{ .call = .{ .demanding = .{ .expr = access.tuple, .bindings = bindings } } };
                },
                1 => {
                    const receiver = input.?.get(.value);
                    if (itemFromValue(receiver, access.elem_index)) |value| return self.finishExprValue(task, value);
                    if (bindings.mark() == task.binding_mark and
                        valueRetainsExpr(receiver, access.tuple) and
                        self.canReuseOriginalExpr(expr_id))
                    {
                        return self.finishExprValue(task, .{ .expr = expr_id });
                    }
                    frame.cursor = 2;
                    return .{ .call = .{ .materialize = .{ .value = receiver } } };
                },
                else => return self.finishExprValue(task, .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = input.?.get(.expr),
                    .elem_index = access.elem_index,
                } } }) }),
            },
            .match_ => |match| switch (frame.cursor) {
                0 => {
                    frame.cursor = 1;
                    return .{ .call = .{ .demanding = .{ .expr = match.scrutinee, .bindings = bindings } } };
                },
                1 => {
                    task.value = input.?.get(.value);
                    frame.cursor = 2;
                    return .{ .call = .{ .select_known_match = .{ .scrutinee = task.value, .branches = match.branches, .bindings = bindings } } };
                },
                2 => {
                    if (input.?.get(.maybe_value)) |value| return self.finishExprValue(task, value);
                    frame.cursor = 3;
                    return .{ .call = .{ .materialize = .{ .value = task.value } } };
                },
                3 => {
                    task.expr_out = input.?.get(.expr);
                    if (self.purpose != .loop_exit_selection) {
                        frame.cursor = 4;
                        return .{ .call = .{ .case_of_case = .{ .ty = expr.ty, .scrutinee_expr = task.expr_out, .outer_branches = match.branches } } };
                    }
                    frame.cursor = 5;
                    return .{ .call = .{ .branch_span = .{ .span = match.branches } } };
                },
                4 => {
                    if (input.?.get(.maybe_value)) |value| return self.finishExprValue(task, value);
                    frame.cursor = 5;
                    return .{ .call = .{ .branch_span = .{ .span = match.branches } } };
                },
                else => {
                    const branches = input.?.get(.branches);
                    const residual = try self.addExpr(.{ .ty = expr.ty, .data = .{ .match_ = .{
                        .scrutinee = task.expr_out,
                        .branches = branches.span,
                        .comptime_site = match.comptime_site,
                    } } });
                    try self.recordArmValues(residual, branches.values);
                    return self.finishExprValue(task, .{ .expr = residual });
                },
            },
            .call_value => |call| switch (frame.cursor) {
                0 => {
                    frame.cursor = 1;
                    return .{ .call = .{ .demanding = .{ .expr = call.callee, .bindings = bindings } } };
                },
                1 => {
                    task.value = input.?.get(.value);
                    const callee_structure = structuralValue(task.value);
                    if (callee_structure == .callable and self.inline_calls.admitsCallable(callee_structure.callable, self.iterator_inline_depth != 0)) {
                        task.enters_iterator = self.iterator_inline_depth != 0 or callee_structure.callable.iterator_step;
                        if (task.enters_iterator) self.iterator_inline_depth += 1;
                        frame.cursor = 2;
                        return .{ .call = .{ .inline_callable = .{
                            .ty = expr.ty,
                            .callable = callee_structure.callable,
                            .args_span = call.args,
                            .original_expr = expr_id,
                            .result_shape_demanded = false,
                            .bindings = bindings,
                        } } };
                    }
                    frame.cursor = 3;
                    return .{ .call = .{ .materialize = .{ .value = task.value } } };
                },
                2 => {
                    if (task.enters_iterator) self.iterator_inline_depth -= 1;
                    return self.finishExprValue(task, input.?.get(.value));
                },
                3 => {
                    task.expr_out = input.?.get(.expr);
                    frame.cursor = 4;
                    return .{ .call = .{ .expr_span = .{ .span = call.args } } };
                },
                else => return self.finishExprValue(task, .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .call_value = .{
                    .callee = task.expr_out,
                    .args = input.?.get(.expr_span),
                } } }) }),
            },
            .call_proc => |call| {
                switch (frame.cursor) {
                    0 => {},
                    // The call cloned plainly.
                    1 => return self.finishExprValue(task, .{ .expr = input.?.get(.expr) }),
                    // The call inlined.
                    else => {
                        if (task.enters_iterator) self.iterator_inline_depth -= 1;
                        return self.finishExprValue(task, input.?.get(.value));
                    },
                }
                frame.cursor = 1;
                if (call.is_cold) return .{ .call = .{ .plain = .{ .expr = expr_id } } };
                if (self.inline_calls == .iterator_fusion and isForcedDynamicIteratorType(self.pass.program, expr.ty)) {
                    return .{ .call = .{ .plain = .{ .expr = expr_id } } };
                }
                if (!self.inline_calls.admitsDirect(call.iterator_procedure, self.iterator_inline_depth != 0)) {
                    return .{ .call = .{ .plain = .{ .expr = expr_id } } };
                }
                const callee = Ast.localDirectCallee(call) orelse return .{ .call = .{ .plain = .{ .expr = expr_id } } };
                const has_known_shape_arg = try self.directCallHasKnownShapeArg(call.args);
                // A direct call carries its callee's captures by the callee's
                // own capture locals: the residual call imports those locals
                // into the enclosing function. In a context where a capture
                // operand has been substituted away from the callee's local,
                // that import would name a local the context does not have,
                // so the call cannot stay residual and must inline.
                const captures_foreign = self.callCapturesAreForeign(call.captures);
                if (self.inline_direct_requires_known_arg and
                    !has_known_shape_arg and
                    !isIteratorProducer(call.iterator_procedure) and
                    !captures_foreign)
                {
                    return .{ .call = .{ .plain = .{ .expr = expr_id } } };
                }
                task.enters_iterator = self.iterator_inline_depth != 0 or isIteratorProducer(call.iterator_procedure);
                if (task.enters_iterator) self.iterator_inline_depth += 1;
                frame.cursor = 2;
                return .{ .call = .{ .inline_direct = .{
                    .callee = callee,
                    .args_span = call.args,
                    .captures_span = call.captures,
                    .original_expr = expr_id,
                    .result_shape_demanded = false,
                    .bindings = bindings,
                } } };
            },
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .list,
            .lambda,
            .def_ref,
            .fn_def,
            .low_level,
            .structural_eq,
            .structural_hash,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => {
                if (frame.cursor != 0) return self.finishExprValue(task, .{ .expr = input.?.get(.expr) });
                frame.cursor = 1;
                return .{ .call = .{ .plain = .{ .expr = expr_id } } };
            },
        }
    }

    fn stepRecordUpdateValue(
        self: *Cloner,
        frame: *CloneFrame,
        task: *ExprValueTask,
        expr: Ast.Expr,
        update: Ast.RecordUpdate,
        input: ?CloneResult,
    ) Common.LowerError!CloneStep {
        const bindings = task.bindings;
        switch (frame.cursor) {
            0 => {
                task.source_fields = try GuardedList.dupe(
                    self.arena.allocator(),
                    Ast.FieldExpr,
                    self.pass.program.fieldExprSpan(update.fields),
                );
                task.result_type_fields = try GuardedList.dupe(
                    self.arena.allocator(),
                    Type.Field,
                    self.pass.program.types.fieldSpan(recordUpdateFieldSpan(self.pass.program, expr.ty)),
                );
                frame.cursor = 1;
                return .{ .call = .{ .expr = update.base } };
            },
            1 => {
                const base = input.?.get(.expr);
                const base_ty = self.pass.program.getExpr(base).ty;
                const base_type_fields = try GuardedList.dupe(
                    self.pass.allocator,
                    Type.Field,
                    self.pass.program.types.fieldSpan(recordUpdateFieldSpan(self.pass.program, base_ty)),
                );
                defer self.pass.allocator.free(base_type_fields);
                if (base_type_fields.len != task.result_type_fields.len) {
                    Common.invariant("record update base and result had different field counts in SpecConstr");
                }
                const base_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), base_ty);
                try bindings.appendBinding(self.arena.allocator(), .{
                    .local = base_local,
                    .ty = base_ty,
                    .value = base,
                });
                const base_ref = try self.addExpr(.{ .ty = base_ty, .data = .{ .local = base_local } });

                task.fields = try self.arena.allocator().alloc(FieldValue, task.result_type_fields.len);
                var snapshot_fields = std.ArrayList(Ast.RecordDestruct).empty;
                defer snapshot_fields.deinit(self.pass.allocator);
                for (task.result_type_fields, 0..) |result_type_field, index| {
                    const updated = for (task.source_fields) |field| {
                        if (self.pass.program.names.recordFieldLabelTextEql(result_type_field.name, field.name)) break field.value;
                    } else null;
                    if (updated != null) continue;

                    const base_type_field = base_type_fields[index];
                    if (!self.pass.program.names.recordFieldLabelTextEql(result_type_field.name, base_type_field.name)) {
                        Common.invariant("record update base and result had different ordered fields in SpecConstr");
                    }
                    if (!try self.pass.program.types.typeEql(
                        &self.pass.program.names,
                        base_type_field.ty,
                        result_type_field.ty,
                    )) {
                        Common.invariant("record update changed the type of an unmodified field in SpecConstr");
                    }

                    const read_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), base_type_field.ty);
                    const read_pat = try self.pass.program.addPat(.{ .ty = base_type_field.ty, .data = .{ .bind = read_local } });
                    try snapshot_fields.append(self.pass.allocator, .{ .name = base_type_field.name, .pattern = read_pat });
                    task.fields[index] = .{
                        .name = result_type_field.name,
                        .value = .{ .expr = try self.addExpr(.{ .ty = base_type_field.ty, .data = .{ .local = read_local } }) },
                    };
                }

                // Snapshot every unchanged field before cloning replacement
                // work. A later whole-base read would keep its collections
                // shared across mutations and prevent in-place reuse.
                if (snapshot_fields.items.len != 0) {
                    const snapshot_pat = try self.pass.program.addPat(.{ .ty = base_ty, .data = .{
                        .record = try self.pass.program.addRecordDestructSpan(snapshot_fields.items),
                    } });
                    const snapshot = try self.addStmt(.{ .let_ = .{ .pat = snapshot_pat, .value = base_ref } });
                    try bindings.appendStatement(self.arena.allocator(), snapshot);
                }
                frame.cursor = 2;
            },
            else => {
                task.fields[frame.index] = .{
                    .name = task.result_type_fields[frame.index].name,
                    .value = input.?.get(.value),
                };
                frame.index += 1;
            },
        }
        while (frame.index < task.result_type_fields.len) {
            const type_field = task.result_type_fields[frame.index];
            const updated = for (task.source_fields) |field| {
                if (self.pass.program.names.recordFieldLabelTextEql(type_field.name, field.name)) break field.value;
            } else {
                frame.index += 1;
                continue;
            };
            return .{ .call = .{ .demanding = .{ .expr = updated, .bindings = bindings } } };
        }
        const record_value = Value{ .record = .{
            .ty = recordUpdateBackingType(self.pass.program, expr.ty),
            .fields = task.fields,
        } };
        if (nominalConstructionLayer(self.pass.program, expr.ty) != null) {
            const backing = try self.arena.allocator().create(Value);
            backing.* = record_value;
            return self.finishExprValue(task, .{ .nominal = .{
                .ty = expr.ty,
                .backing = backing,
            } });
        }
        return self.finishExprValue(task, record_value);
    }

    const DemandingTask = struct {
        expr: Ast.ExprId,
        bindings: *BindingChain,
        value: Value = undefined,
        callee_expr: Ast.ExprId = undefined,
        enters_iterator: bool = false,
    };

    fn stepDemanding(self: *Cloner, frame: *CloneFrame, task: *DemandingTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const expr_id = task.expr;
        const bindings = task.bindings;
        if (self.purpose == .loop_exit_selection) {
            switch (frame.cursor) {
                0 => {
                    frame.cursor = 1;
                    return .{ .call = .{ .expr_value = .{ .expr = expr_id, .bindings = bindings } } };
                },
                else => {
                    const value = input.?.get(.value);
                    // Finish a constructor child's strict work before visiting the
                    // next child, whose block may contribute its own binding chain.
                    // A typed runtime anchor must also keep its boundary evaluation.
                    return .{ .tail = try self.makeReusableTask(if (value == .runtime_anchor)
                        .{ .expr = value.runtime_anchor.runtime }
                    else
                        value, bindings) };
                },
            }
        }
        const expr = self.pass.program.getExpr(expr_id);
        const value_task: CloneTask = .{ .expr_value = .{ .expr = expr_id, .bindings = bindings } };
        switch (expr.data) {
            .call_proc => |call| {
                if (frame.cursor != 0) {
                    if (task.enters_iterator) self.iterator_inline_depth -= 1;
                    return .{ .ret = input.? };
                }
                if (self.inline_calls == .iterator_fusion and isForcedDynamicIteratorType(self.pass.program, expr.ty)) {
                    return .{ .tail = value_task };
                }
                if (call.is_cold or !self.inline_calls.admitsDirect(call.iterator_procedure, self.iterator_inline_depth != 0)) {
                    return .{ .tail = value_task };
                }
                const callee = Ast.localDirectCallee(call) orelse return .{ .tail = value_task };
                task.enters_iterator = self.iterator_inline_depth != 0 or isIteratorProducer(call.iterator_procedure);
                if (task.enters_iterator) self.iterator_inline_depth += 1;
                frame.cursor = 1;
                return .{ .call = .{ .inline_direct = .{
                    .callee = callee,
                    .args_span = call.args,
                    .captures_span = call.captures,
                    .original_expr = expr_id,
                    .result_shape_demanded = true,
                    .bindings = bindings,
                } } };
            },
            .call_value => |call| switch (frame.cursor) {
                0 => {
                    frame.cursor = 1;
                    return .{ .call = .{ .demanding = .{ .expr = call.callee, .bindings = bindings } } };
                },
                1 => {
                    task.value = input.?.get(.value);
                    const callee_structure = structuralValue(task.value);
                    if (callee_structure == .callable and self.inline_calls.admitsCallable(callee_structure.callable, self.iterator_inline_depth != 0)) {
                        task.enters_iterator = self.iterator_inline_depth != 0 or callee_structure.callable.iterator_step;
                        if (task.enters_iterator) self.iterator_inline_depth += 1;
                        frame.cursor = 2;
                        return .{ .call = .{ .inline_callable = .{
                            .ty = expr.ty,
                            .callable = callee_structure.callable,
                            .args_span = call.args,
                            .original_expr = expr_id,
                            .result_shape_demanded = true,
                            .bindings = bindings,
                        } } };
                    }
                    frame.cursor = 3;
                    return .{ .call = .{ .materialize = .{ .value = task.value } } };
                },
                2 => {
                    if (task.enters_iterator) self.iterator_inline_depth -= 1;
                    return .{ .ret = input.? };
                },
                3 => {
                    task.callee_expr = input.?.get(.expr);
                    frame.cursor = 4;
                    return .{ .call = .{ .expr_span = .{ .span = call.args } } };
                },
                else => return retValue(.{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .call_value = .{
                    .callee = task.callee_expr,
                    .args = input.?.get(.expr_span),
                } } }) }),
            },
            .block => |block| return .{ .tail = if (self.pass.program.stmtSpan(block.statements).len == 0)
                .{ .demanding = .{ .expr = block.final_expr, .bindings = bindings } }
            else
                value_task },
            .comptime_branch_taken => |taken| return .{ .tail = .{ .demanding = .{ .expr = taken.body, .bindings = bindings } } },
            .typed_boundary => return .{ .tail = value_task },
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .static_data_candidate,
            .comptime_value,
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .nominal,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .fn_ref,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => return .{ .tail = value_task },
        }
    }

    const CallableFromFunctionValueTask = struct {
        ty: Type.TypeId,
        fn_ref: @import("../monotype/ast.zig").LiftedFunctionValue,
        bindings: *BindingChain,
        captures: []CaptureValue = &.{},
    };

    fn stepCallableFromFunctionValue(self: *Cloner, frame: *CloneFrame, task: *CallableFromFunctionValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const fn_ref = task.fn_ref;
        switch (frame.cursor) {
            0 => {
                if (try self.pass.inlineSourceBody(fn_ref.fn_id)) |source| {
                    if (!source.size.admits()) {
                        frame.cursor = 1;
                        return .{ .call = .{ .capture_span = .{ .span = fn_ref.captures } } };
                    }
                }
                const capture_count: usize = @intCast(fn_ref.captures.len);
                task.captures = try self.arena.allocator().alloc(CaptureValue, capture_count);
                frame.cursor = 2;
            },
            1 => return retValue(.{ .expr = try self.addExpr(.{ .ty = task.ty, .data = .{ .fn_ref = .{
                .fn_id = fn_ref.fn_id,
                .captures = input.?.get(.capture_span),
            } } }) }),
            else => {
                task.captures[frame.index] = .{
                    .id = self.pass.program.captureOperandAt(fn_ref.captures, frame.index).id,
                    .value = input.?.get(.value),
                };
                frame.index += 1;
            },
        }
        if (frame.index < task.captures.len) {
            const operand = self.pass.program.captureOperandAt(fn_ref.captures, frame.index);
            return .{ .call = .{ .expr_value = .{ .expr = operand.value, .bindings = task.bindings } } };
        }
        return retValue(.{ .callable = .{
            .ty = task.ty,
            .fn_id = fn_ref.fn_id,
            .captures = task.captures,
        } });
    }

    /// A value computation whose strict chain is placed around its
    /// materialized result.
    const WrappedValueTask = struct {
        child: *const CloneTask,
        chain: *BindingChain,
    };

    fn wrappedValueTask(self: *Cloner, child: CloneTask, chain: *BindingChain) Allocator.Error!CloneTask {
        const stored = try self.arena.allocator().create(CloneTask);
        stored.* = child;
        return .{ .wrapped_value = .{ .child = stored, .chain = chain } };
    }

    fn stepWrappedValue(self: *Cloner, frame: *CloneFrame, task: *WrappedValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = task.child.* };
            },
            1 => {
                frame.cursor = 2;
                return .{ .call = .{ .materialize = .{ .value = input.?.get(.value) } } };
            },
            else => return retExpr(try self.wrapBindings(task.chain.*, input.?.get(.expr))),
        }
    }

    const PlainTask = struct {
        expr: Ast.ExprId,
        saved: SourceContext = undefined,
        a: Ast.ExprId = undefined,
        b: Ast.ExprId = undefined,
        c: Ast.ExprId = undefined,
        expr_span: Ast.Span(Ast.ExprId) = undefined,
        if_branches: ClonedIfBranches = undefined,
        local: Ast.LocalId = undefined,
        other_local: Ast.LocalId = undefined,
        loop_params: Ast.Span(Ast.TypedLocal) = undefined,
        shadow_start: usize = undefined,
    };

    fn finishPlainExpr(self: *Cloner, task: *PlainTask, expr_id: Ast.ExprId) CloneStep {
        task.saved.restore(self);
        return retExpr(expr_id);
    }

    /// Emit the cloned data at the expression's own source location, then
    /// restore the enclosing one.
    fn finishPlainData(self: *Cloner, task: *PlainTask, expr: Ast.Expr, data: Ast.ExprData) Common.LowerError!CloneStep {
        const cloned = if (plainExprCanReuse(expr.data) and
            std.meta.eql(expr.data, data) and
            self.canReuseOriginalExpr(task.expr))
            task.expr
        else
            try self.addExpr(.{ .ty = expr.ty, .data = data });
        task.saved.restore(self);
        return retExpr(cloned);
    }

    fn stepPlain(self: *Cloner, frame: *CloneFrame, task: *PlainTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const expr_id = task.expr;
        const expr = self.pass.program.getExpr(expr_id);
        if (frame.cursor == 0) {
            self.assertSourceExpr(expr_id);
            task.saved = try self.enterExprSource(expr_id);
            if (self.source_reuse == .original_body) {
                switch (expr.data) {
                    .@"unreachable",
                    .unit,
                    .uninitialized,
                    .int_lit,
                    .frac_f32_lit,
                    .frac_f64_lit,
                    .dec_lit,
                    .str_lit,
                    .bytes_lit,
                    .crash,
                    .checked_error,
                    .comptime_exhaustiveness_failed,
                    => if (self.canReuseOriginalExpr(expr_id)) return self.finishPlainExpr(task, expr_id),
                    .uninitialized_payload => |payload| {
                        if (self.cloneLocalRef(payload.condition) == payload.condition and
                            self.canReuseOriginalExpr(expr_id))
                        {
                            return self.finishPlainExpr(task, expr_id);
                        }
                    },
                    .local,
                    .list,
                    .tuple,
                    .record,
                    .record_update,
                    .tag,
                    .static_data_candidate,
                    .comptime_value,
                    .typed_boundary,
                    .nominal,
                    .let_,
                    .lambda,
                    .def_ref,
                    .fn_def,
                    .fn_ref,
                    .call_value,
                    .call_proc,
                    .low_level,
                    .field_access,
                    .tuple_access,
                    .structural_eq,
                    .structural_hash,
                    .match_,
                    .if_,
                    .block,
                    .loop_,
                    .break_,
                    .continue_,
                    .join_point,
                    .jump,
                    .if_initialized_payload,
                    .try_sequence,
                    .try_record_sequence,
                    .return_,
                    .comptime_branch_taken,
                    .dbg,
                    .expect_err,
                    .literal_rejected,
                    .expect,
                    => {},
                }
            }
        }
        const cursor = frame.cursor;
        frame.cursor += 1;
        switch (expr.data) {
            .@"unreachable" => return self.finishPlainData(task, expr, .@"unreachable"),
            .local => |local| return self.finishPlainData(task, expr, .{ .local = local }),
            .unit => return self.finishPlainData(task, expr, .unit),
            .uninitialized => return self.finishPlainData(task, expr, .uninitialized),
            .uninitialized_payload => |payload| return self.finishPlainData(task, expr, .{ .uninitialized_payload = .{
                .condition = self.cloneLocalRef(payload.condition),
                .mask = payload.mask,
            } }),
            .int_lit => |value| return self.finishPlainData(task, expr, .{ .int_lit = value }),
            .frac_f32_lit => |value| return self.finishPlainData(task, expr, .{ .frac_f32_lit = value }),
            .frac_f64_lit => |value| return self.finishPlainData(task, expr, .{ .frac_f64_lit = value }),
            .dec_lit => |value| return self.finishPlainData(task, expr, .{ .dec_lit = value }),
            .str_lit => |value| return self.finishPlainData(task, expr, .{ .str_lit = value }),
            .bytes_lit => |value| return self.finishPlainData(task, expr, .{ .bytes_lit = value }),
            .list => |items| {
                if (cursor == 0) return .{ .call = .{ .expr_span = .{ .span = items } } };
                return self.finishPlainData(task, expr, .{ .list = input.?.get(.expr_span) });
            },
            .tuple => |items| {
                if (cursor == 0) return .{ .call = .{ .expr_span = .{ .span = items } } };
                return self.finishPlainData(task, expr, .{ .tuple = input.?.get(.expr_span) });
            },
            .record => |fields| {
                if (cursor == 0) return .{ .call = .{ .field_span = .{ .span = fields } } };
                return self.finishPlainData(task, expr, .{ .record = input.?.get(.field_span) });
            },
            .record_update => |update| switch (cursor) {
                0 => return .{ .call = .{ .expr = update.base } },
                1 => {
                    task.a = input.?.get(.expr);
                    return .{ .call = .{ .field_span = .{ .span = update.fields } } };
                },
                else => return self.finishPlainData(task, expr, .{ .record_update = .{
                    .base = task.a,
                    .fields = input.?.get(.field_span),
                } }),
            },
            .tag => |tag| {
                if (cursor == 0) return .{ .call = .{ .expr_span = .{ .span = tag.payloads } } };
                return self.finishPlainData(task, expr, .{ .tag = .{
                    .name = tag.name,
                    .payloads = input.?.get(.expr_span),
                } });
            },
            .static_data_candidate => return self.finishPlainExpr(task, expr_id),
            .comptime_value => return self.finishPlainExpr(task, expr_id),
            .typed_boundary => |boundary| {
                if (cursor == 0) return .{ .call = .{ .expr = boundary.value } };
                return self.finishPlainData(task, expr, .{ .typed_boundary = .{ .value = input.?.get(.expr) } });
            },
            .nominal => |backing| {
                if (cursor == 0) return .{ .call = .{ .expr = backing } };
                return self.finishPlainData(task, expr, .{ .nominal = input.?.get(.expr) });
            },
            .let_ => |let_| {
                if (cursor == 0) {
                    const chain = try self.newChain();
                    return .{ .call = try self.wrappedValueTask(.{ .let_value = .{ .let_ = LetParts.fromExpr(let_), .bindings = chain } }, chain) };
                }
                return self.finishPlainData(task, expr, self.pass.program.getExpr(input.?.get(.expr)).data);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .fn_ref => |fn_ref| {
                if (cursor == 0) return .{ .call = .{ .capture_span = .{ .span = fn_ref.captures } } };
                return self.finishPlainData(task, expr, .{ .fn_ref = .{
                    .fn_id = fn_ref.fn_id,
                    .captures = input.?.get(.capture_span),
                } });
            },
            .call_value => |call| switch (cursor) {
                0 => return .{ .call = .{ .expr = call.callee } },
                1 => {
                    task.a = input.?.get(.expr);
                    return .{ .call = .{ .expr_span = .{ .span = call.args } } };
                },
                else => return self.finishPlainData(task, expr, .{ .call_value = .{
                    .callee = task.a,
                    .args = input.?.get(.expr_span),
                } }),
            },
            .call_proc => |call| {
                if (cursor == 0) return .{ .call = .{ .call_proc = .{ .ty = expr.ty, .call = call } } };
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .low_level => |call| {
                if (cursor == 0) return .{ .call = .{ .expr_span = .{ .span = call.args } } };
                return self.finishPlainData(task, expr, .{ .low_level = .{
                    .op = call.op,
                    .args = input.?.get(.expr_span),
                } });
            },
            .field_access => |field| {
                if (cursor == 0) {
                    const chain = try self.newChain();
                    return .{ .call = try self.wrappedValueTask(.{ .field_access_value = .{
                        .original_expr = expr_id,
                        .ty = expr.ty,
                        .field = field,
                        .bindings = chain,
                    } }, chain) };
                }
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .tuple_access => |access| {
                if (cursor == 0) return .{ .call = .{ .tuple_access = .{ .original_expr = expr_id, .ty = expr.ty, .access = access } } };
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .structural_eq => |eq| switch (cursor) {
                0 => return .{ .call = .{ .expr = eq.lhs } },
                1 => {
                    task.a = input.?.get(.expr);
                    return .{ .call = .{ .expr = eq.rhs } };
                },
                else => return self.finishPlainData(task, expr, .{ .structural_eq = .{
                    .lhs = task.a,
                    .rhs = input.?.get(.expr),
                    .negated = eq.negated,
                } }),
            },
            .structural_hash => |h| switch (cursor) {
                0 => return .{ .call = .{ .expr = h.value } },
                1 => {
                    task.a = input.?.get(.expr);
                    return .{ .call = .{ .expr = h.hasher } };
                },
                else => return self.finishPlainData(task, expr, .{ .structural_hash = .{
                    .value = task.a,
                    .hasher = input.?.get(.expr),
                } }),
            },
            .match_ => |match| {
                if (cursor == 0) return .{ .call = .{ .match = .{ .ty = expr.ty, .match = match } } };
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .if_ => |if_| switch (cursor) {
                0 => return .{ .call = .{ .if_branch_span = .{ .span = if_.branches } } },
                1 => {
                    task.if_branches = input.?.get(.if_branches);
                    return .{ .call = .{ .keeping = .{ .expr = if_.final_else } } };
                },
                else => {
                    const final_else = input.?.get(.arm);
                    const branches = task.if_branches;
                    const if_data: Ast.ExprData = .{ .if_ = .{
                        .branches = branches.span,
                        .final_else = final_else.body,
                    } };
                    if (std.meta.eql(expr.data, if_data) and self.canReuseOriginalExpr(expr_id)) return self.finishPlainExpr(task, expr_id);
                    const cloned = try self.addExpr(.{ .ty = expr.ty, .data = if_data });
                    const values = try self.arena.allocator().alloc(Value, branches.values.len + 1);
                    @memcpy(values[0..branches.values.len], branches.values);
                    values[branches.values.len] = final_else.value;
                    try self.recordArmValues(cloned, values);
                    return self.finishPlainExpr(task, cloned);
                },
            },
            .block => |block| {
                if (cursor == 0) {
                    const chain = try self.newChain();
                    return .{ .call = try self.wrappedValueTask(.{ .block_value = .{ .ty = expr.ty, .block = block, .bindings = chain } }, chain) };
                }
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .loop_ => |loop| {
                if (cursor == 0) {
                    const chain = try self.newChain();
                    return .{ .call = try self.wrappedValueTask(.{ .loop_value = .{ .ty = expr.ty, .loop = loop, .bindings = chain, .exit_selection = null } }, chain) };
                }
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .break_ => |maybe| {
                if (cursor == 0) {
                    if (self.currentLoopExitSelection()) |selection| {
                        const value = maybe orelse Common.invariant("selected value-producing loop had a valueless break");
                        frame.cursor = 10;
                        return .{ .call = .{ .selected_loop_exit = .{ .break_ty = expr.ty, .value_expr = value, .selection = selection } } };
                    }
                    const value = maybe orelse return self.finishPlainData(task, expr, .{ .break_ = null });
                    return .{ .call = .{ .expr = value } };
                }
                if (cursor == 10) return self.finishPlainExpr(task, input.?.get(.expr));
                return self.finishPlainData(task, expr, .{ .break_ = input.?.get(.expr) });
            },
            .continue_ => |continue_| {
                if (cursor == 0) return .{ .call = .{ .continue_ = .{ .ty = expr.ty, .values = continue_.values } } };
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .join_point => |join_point| {
                if (cursor == 0) return .{ .call = .{ .join_point = .{ .ty = expr.ty, .join_point = join_point } } };
                return self.finishPlainExpr(task, input.?.get(.expr));
            },
            .jump => |jump| switch (cursor) {
                0 => {
                    if (self.letCaseJoinFor(jump.target)) |join| {
                        frame.cursor = 10;
                        return .{ .call = .{ .capture_let_case_jump = .{ .ty = expr.ty, .join = join, .jump = jump } } };
                    }
                    return .{ .call = .{ .expr_span = .{ .span = jump.args } } };
                },
                1 => {
                    task.expr_span = input.?.get(.expr_span);
                    task.loop_params = try self.cloneLoopUpdateParams(jump.loop_params);
                    return .{ .call = .{ .expr_span = .{ .span = jump.loop_values } } };
                },
                10 => return self.finishPlainExpr(task, input.?.get(.expr)),
                else => return self.finishPlainData(task, expr, .{ .jump = .{
                    .target = self.clonedJoinTarget(jump.target),
                    .args = task.expr_span,
                    .loop_params = task.loop_params,
                    .loop_values = input.?.get(.expr_span),
                } }),
            },
            .if_initialized_payload => |payload_switch| switch (cursor) {
                0 => return .{ .call = .{ .expr = payload_switch.cond } },
                1 => {
                    task.a = input.?.get(.expr);
                    task.local = self.cloneLocalRef(payload_switch.payload);
                    return .{ .call = .{ .expr = payload_switch.initialized } };
                },
                2 => {
                    task.b = input.?.get(.expr);
                    return .{ .call = .{ .expr = payload_switch.uninitialized } };
                },
                else => return self.finishPlainData(task, expr, .{ .if_initialized_payload = .{
                    .cond = task.a,
                    .cond_mask = payload_switch.cond_mask,
                    .payload = task.local,
                    .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
                    .initialized = task.b,
                    .uninitialized = input.?.get(.expr),
                } }),
            },
            .try_sequence => |sequence| switch (cursor) {
                0 => return .{ .call = .{ .expr = sequence.try_expr } },
                1 => {
                    task.a = input.?.get(.expr);
                    task.shadow_start = self.subst.watermark();
                    const ok_ty = self.pass.program.getLocal(sequence.ok_local).ty;
                    task.local = try self.cloneBinder(sequence.ok_local, ok_ty, .bind_runtime);
                    return .{ .call = .{ .expr = sequence.ok_body } };
                },
                else => {
                    const ok_body = input.?.get(.expr);
                    self.subst.restore(task.shadow_start);
                    return self.finishPlainData(task, expr, .{ .try_sequence = .{
                        .try_expr = task.a,
                        .ok_local = task.local,
                        .err_is_cold = sequence.err_is_cold,
                        .err_target = if (sequence.err_target) |target| self.clonedJoinTarget(target) else null,
                        .ok_body = ok_body,
                    } });
                },
            },
            .try_record_sequence => |sequence| switch (cursor) {
                0 => return .{ .call = .{ .expr = sequence.try_expr } },
                1 => {
                    task.a = input.?.get(.expr);
                    task.shadow_start = self.subst.watermark();
                    const value_ty = self.pass.program.getLocal(sequence.value_local).ty;
                    task.local = try self.cloneBinder(sequence.value_local, value_ty, .bind_runtime);
                    const rest_ty = self.pass.program.getLocal(sequence.rest_local).ty;
                    task.other_local = try self.cloneBinder(sequence.rest_local, rest_ty, .bind_runtime);
                    return .{ .call = .{ .expr = sequence.ok_body } };
                },
                else => {
                    const ok_body = input.?.get(.expr);
                    self.subst.restore(task.shadow_start);
                    return self.finishPlainData(task, expr, .{ .try_record_sequence = .{
                        .try_expr = task.a,
                        .value_local = task.local,
                        .value_field = sequence.value_field,
                        .rest_local = task.other_local,
                        .rest_field = sequence.rest_field,
                        .err_is_cold = sequence.err_is_cold,
                        .err_target = if (sequence.err_target) |target| self.clonedJoinTarget(target) else null,
                        .ok_body = ok_body,
                    } });
                },
            },
            .return_ => |ret| {
                if (cursor == 0) return .{ .call = .{ .expr = ret.value } };
                return self.finishPlainData(task, expr, .{ .return_ = .{
                    .value = input.?.get(.expr),
                    .target = ret.target,
                } });
            },
            .crash => |msg| return self.finishPlainData(task, expr, .{ .crash = msg }),
            .checked_error => |msg| return self.finishPlainData(task, expr, .{ .checked_error = msg }),
            .comptime_branch_taken => |taken| {
                if (cursor == 0) return .{ .call = .{ .expr = taken.body } };
                return self.finishPlainData(task, expr, .{ .comptime_branch_taken = .{
                    .site = taken.site,
                    .branch_index = taken.branch_index,
                    .body = input.?.get(.expr),
                } });
            },
            .comptime_exhaustiveness_failed => |site| return self.finishPlainData(task, expr, .{ .comptime_exhaustiveness_failed = site }),
            .dbg => |child| {
                if (cursor == 0) return .{ .call = .{ .expr = child } };
                return self.finishPlainData(task, expr, .{ .dbg = input.?.get(.expr) });
            },
            .expect_err => |expect_err| {
                if (cursor == 0) return .{ .call = .{ .expr = expect_err.msg } };
                return self.finishPlainData(task, expr, .{ .expect_err = .{
                    .msg = input.?.get(.expr),
                    .region = expect_err.region,
                } });
            },
            .expect => |child| {
                if (cursor == 0) return .{ .call = .{ .expr = child } };
                return self.finishPlainData(task, expr, .{ .expect = input.?.get(.expr) });
            },
            .literal_rejected => |rejected| {
                if (cursor == 0) return .{ .call = .{ .expr = rejected.msg } };
                return self.finishPlainData(task, expr, .{ .literal_rejected = .{
                    .msg = input.?.get(.expr),
                    .site = rejected.site,
                } });
            },
        }
    }

    /// The parts of a `let` a clone consumes: a source `let_` expression, or
    /// a binding statement followed by the rest of its block.
    const LetParts = struct {
        bind: Ast.PatId,
        value: Ast.ExprId,
        rest: Ast.ExprId,
        comptime_site: ?Ast.ComptimeSiteId,

        fn fromExpr(let_: @FieldType(Ast.ExprData, "let_")) LetParts {
            return .{ .bind = let_.bind, .value = let_.value, .rest = let_.rest, .comptime_site = let_.comptime_site };
        }
    };

    const JoinPointTask = struct {
        ty: Type.TypeId,
        join_point: Ast.JoinPointExpr,
        retained: Ast.Span(Ast.TypedLocal) = undefined,
        params: []Ast.TypedLocal = &.{},
        target: Ast.JoinPointId = undefined,
        change_start: usize = 0,
        body: Ast.ExprId = undefined,
    };

    fn stepJoinPoint(self: *Cloner, frame: *CloneFrame, task: *JoinPointTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const join_point = task.join_point;
        switch (frame.cursor) {
            0 => {
                task.retained = try self.cloneRetainedLocals(join_point.retained);
                const source_params = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(join_point.params));
                defer self.pass.allocator.free(source_params);
                task.params = try self.arena.allocator().alloc(Ast.TypedLocal, source_params.len);
                for (source_params, 0..) |source_param, index| {
                    const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), source_param.ty);
                    task.params[index] = .{ .local = local, .ty = source_param.ty };
                }

                task.target = self.pass.freshJoinPoint();
                try self.join_stack.append(self.pass.allocator, .{ .source = join_point.id, .target = task.target });

                task.change_start = self.subst.watermark();
                for (source_params, task.params) |source_param, param| {
                    const local_expr = try self.addExpr(.{ .ty = param.ty, .data = .{ .local = param.local } });
                    try self.subst.put(self.pass.program, source_param.local, .{ .expr = local_expr });
                }
                frame.cursor = 1;
                return .{ .call = .{ .expr = join_point.body } };
            },
            1 => {
                task.body = input.?.get(.expr);
                // The remainder's jumps may forward-reference the join's own params:
                // an `uninitialized_payload` argument names the flag param carrying
                // its initialized-ness. Keep the param substitutions active so those
                // references follow the freshened params.
                frame.cursor = 2;
                return .{ .call = .{ .expr = join_point.remainder } };
            },
            else => {
                const remainder = input.?.get(.expr);
                self.subst.restore(task.change_start);
                const cloned = try self.addExpr(.{ .ty = task.ty, .data = .{ .join_point = .{
                    .id = task.target,
                    .params = try self.pass.program.addTypedLocalSpan(task.params),
                    .retained = task.retained,
                    .body = task.body,
                    .remainder = remainder,
                } } });
                _ = self.join_stack.pop();
                return retExpr(cloned);
            },
        }
    }

    const LetValueTask = struct {
        let_: LetParts,
        bindings: *BindingChain,
        value_chain: *BindingChain = undefined,
    };

    fn stepLetValue(self: *Cloner, frame: *CloneFrame, task: *LetValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        switch (frame.cursor) {
            0 => {
                if (self.purpose == .loop_exit_selection) {
                    frame.cursor = 1;
                    return .{ .call = .{ .loop_exit_values = .{ .let_ = task.let_ } } };
                }
            },
            1 => if (input.?.get(.maybe_expr)) |selected| return retValue(.{ .expr = selected }),
            else => {
                const value = input.?.get(.value);
                task.bindings.appendChain(task.value_chain.*);
                return .{ .tail = .{ .let_with_value = .{ .let_ = task.let_, .value = value, .bindings = task.bindings } } };
            },
        }
        task.value_chain = try self.newChain();
        frame.cursor = 2;
        return .{ .call = .{ .expr_value = .{ .expr = task.let_.value, .bindings = task.value_chain } } };
    }

    /// Consume an already-cloned producer. Block traversal uses this only when
    /// the value needs a shared continuation; ordinary bindings stay in its
    /// iterative statement walk.
    const LetWithValueTask = struct {
        let_: LetParts,
        value: Value,
        bindings: *BindingChain,
        value_expr: Ast.ExprId = undefined,
        change_start: usize = 0,
        bind: Ast.PatId = undefined,
    };

    fn stepLetWithValue(self: *Cloner, frame: *CloneFrame, task: *LetWithValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const let_ = task.let_;
        const case_expr = if (self.purpose == .loop_exit_selection) null else self.caseExprFromValue(task.value);
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = .{ .materialize = .{ .value = task.value } } };
            },
            1 => {
                task.value_expr = input.?.get(.expr);
                if (case_expr) |case| {
                    frame.cursor = 2;
                    return .{ .call = .{ .let_of_case = .{ .let_ = let_, .value_expr = case } } };
                }
            },
            2 => if (input.?.get(.maybe_data)) |data| {
                const rest_ty = self.pass.program.getExpr(let_.rest).ty;
                return retValue(.{ .expr = try self.addExpr(.{ .ty = rest_ty, .data = data }) });
            },
            3 => {
                if (input.?.get(.flag)) {
                    frame.cursor = 4;
                    return .{ .call = .{ .expr_value = .{ .expr = let_.rest, .bindings = task.bindings } } };
                }
                // A branch-built value that cannot bind as one value transfers each
                // branch result to one shared continuation.
                if (case_expr) |case| {
                    frame.cursor = 5;
                    return .{ .call = .{ .let_of_case = .{ .let_ = let_, .value_expr = case } } };
                }
                return try self.letWithValueRuntimeBinding(frame, task);
            },
            4 => {
                const rest = input.?.get(.value);
                try self.subst.restoreFloatingLoopCarries(task.change_start);
                return retValue(rest);
            },
            5 => {
                if (input.?.get(.maybe_data)) |data| {
                    const rest_ty = self.pass.program.getExpr(let_.rest).ty;
                    return retValue(.{ .expr = try self.addExpr(.{ .ty = rest_ty, .data = data }) });
                }
                return try self.letWithValueRuntimeBinding(frame, task);
            },
            else => {
                const rest = input.?.get(.expr);
                try self.subst.restoreFloatingLoopCarries(task.change_start);
                return retValue(.{ .expr = try self.addExpr(.{ .ty = self.pass.program.getExpr(let_.rest).ty, .data = .{ .let_ = .{
                    .bind = task.bind,
                    .value = task.value_expr,
                    .rest = rest,
                    .comptime_site = let_.comptime_site,
                } } }) });
            },
        }
        task.change_start = self.subst.watermark();
        frame.cursor = 3;
        return .{ .call = .{ .bind_let_value = .{ .pat = let_.bind, .source_value = let_.value, .value = task.value, .bindings = task.bindings } } };
    }

    /// Name the value's opaque leaves and pin them at this position: the
    /// same computations in the same order, but the bound name keeps its
    /// structured value for the continuation.
    fn letWithValueRuntimeBinding(self: *Cloner, frame: *CloneFrame, task: *LetWithValueTask) Common.LowerError!CloneStep {
        task.bind = try self.clonePat(task.let_.bind, .bind_runtime);
        frame.cursor = 6;
        return .{ .call = .{ .expr = task.let_.rest } };
    }

    const BindLetValueTask = struct {
        pat: Ast.PatId,
        source_value: Ast.ExprId,
        value: Value,
        bindings: *BindingChain,
    };

    fn stepBindLetValue(self: *Cloner, task: *BindLetValueTask) Common.LowerError!CloneStep {
        const change_start = self.subst.watermark();
        if (try self.bindPatToReusableValue(task.pat, task.value) == .match) return .{ .ret = .{ .flag = true } };
        self.subst.restore(change_start);
        return .{ .tail = .{ .positioned_reusable = .{
            .pat = task.pat,
            .source_value = task.source_value,
            .recursive = false,
            .value = task.value,
            .bindings = task.bindings,
        } } };
    }

    /// Dissolve a binding while retaining every opaque leaf in the strict
    /// chain owned by the binding's original position. No work is discarded or
    /// commuted: naming the leaves once makes the structured value reusable
    /// without requiring purity, termination, or speculatability.
    const PositionedReusableTask = struct {
        pat: Ast.PatId,
        source_value: Ast.ExprId,
        recursive: bool,
        value: Value,
        bindings: *BindingChain,
        bindings_before: ?*BindingNode = null,
        change_before: usize = 0,
    };

    fn stepPositionedReusable(self: *Cloner, frame: *CloneFrame, task: *PositionedReusableTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            // A binding refers to itself exactly when its producer marked it
            // recursive.
            const self_referential = task.recursive;
            if (self_referential) return .{ .ret = .{ .flag = false } };

            task.bindings_before = task.bindings.mark();
            task.change_before = self.subst.watermark();
            frame.cursor = 1;
            return .{ .call = try self.makeReusableTask(task.value, task.bindings) };
        }
        const reusable = input.?.get(.value);
        if (try self.bindPatToReusableValue(task.pat, reusable) != .match) {
            self.subst.restore(task.change_before);
            task.bindings.rewind(task.bindings_before);
            return .{ .ret = .{ .flag = false } };
        }
        return .{ .ret = .{ .flag = true } };
    }

    /// Consume the immutable function demand plan. This returns completed
    /// output: neither the loop nor its continuation is cloned a second time.
    const LoopExitValuesTask = struct {
        let_: LetParts,
        change_start: usize = 0,
        plan: *const ExitDemand.Plan = undefined,
        params: []Ast.TypedLocal = &.{},
        tuple_items: ?[]?Ast.ExprId = null,
        rest_ty: Type.TypeId = undefined,
        result_ty: Type.TypeId = undefined,
        target: ?Ast.JoinPointId = null,
        loop: Ast.ExprId = undefined,
    };

    fn stepLoopExitValues(self: *Cloner, frame: *CloneFrame, task: *LoopExitValuesTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const let_ = task.let_;
        switch (frame.cursor) {
            0 => {
                const demands = self.exit_demands orelse return .{ .ret = .{ .maybe_expr = null } };
                task.plan = demands.get(let_.bind) orelse return .{ .ret = .{ .maybe_expr = null } };
                const plan = task.plan;
                const loop_expr = self.pass.program.getExpr(let_.value);
                if (loop_expr.data != .loop_) Common.invariant("planned exit binding lost its loop");
                task.change_start = self.subst.watermark();

                const count = plan.used_count;
                const kept = try self.arena.allocator().alloc(u32, count);
                const types = try self.arena.allocator().alloc(Type.TypeId, count);
                task.params = try self.arena.allocator().alloc(Ast.TypedLocal, count);
                task.tuple_items = if (plan.aggregate != null)
                    try self.arena.allocator().alloc(?Ast.ExprId, plan.items.len)
                else
                    null;
                if (task.tuple_items) |items| @memset(items, null);
                var index: usize = 0;
                for (plan.items, 0..) |item, source_index| {
                    if (!item.used) continue;
                    kept[index] = @intCast(source_index);
                    types[index] = item.ty;
                    task.params[index] = .{ .ty = item.ty, .local = try self.pass.program.addLocal(self.pass.symbols.fresh(), item.ty) };
                    index += 1;
                }
                task.rest_ty = self.pass.program.getExpr(let_.rest).ty;
                task.result_ty = if (count == 1) task.params[0].ty else task.rest_ty;
                task.target = if (count == 1) null else self.pass.freshJoinPoint();
                const selection = LoopExitSelection{
                    .source_ty = plan.source_ty,
                    .source_arity = plan.items.len,
                    .kept_types = types,
                    .kept_indices = kept,
                    .transfer = if (task.target) |join| .{ .jump = .{ .target = join } } else .break_value,
                };
                // Initial values are in the enclosing scope, before result bindings.
                const chain = try self.newChain();
                frame.cursor = 1;
                return .{ .call = try self.wrappedValueTask(.{ .loop_value = .{
                    .ty = task.result_ty,
                    .loop = loop_expr.data.loop_,
                    .bindings = chain,
                    .exit_selection = selection,
                } }, chain) };
            },
            1 => {
                task.loop = input.?.get(.expr);
                const plan = task.plan;
                var index: usize = 0;
                for (plan.items, 0..) |item, source_index| {
                    if (!item.used) continue;
                    const ref = Value{ .expr = try self.addExpr(.{ .ty = item.ty, .data = .{ .local = task.params[index].local } }) };
                    if (item.local) |local| try self.subst.put(self.pass.program, local, ref);
                    if (task.tuple_items) |items| items[source_index] = ref.expr;
                    index += 1;
                }
                if (plan.aggregate) |local| {
                    if (self.exit_tuple_items.contains(local)) Common.invariant("loop exit selection rebound an active aggregate");
                    try self.exit_tuple_items.put(local, task.tuple_items.?);
                }
                frame.cursor = 2;
                return .{ .call = .{ .expr = let_.rest } };
            },
            else => {
                const rest = input.?.get(.expr);
                const result = if (task.target) |join|
                    try self.addExpr(.{ .ty = task.rest_ty, .data = .{ .join_point = .{
                        .id = join,
                        .params = try self.pass.program.addTypedLocalSpan(task.params),
                        .body = rest,
                        .remainder = task.loop,
                    } } })
                else blk: {
                    const bind = try self.pass.program.addPat(.{ .ty = task.result_ty, .data = .{ .bind = task.params[0].local } });
                    break :blk try self.addExpr(.{ .ty = task.rest_ty, .data = .{ .let_ = .{
                        .bind = bind,
                        .value = task.loop,
                        .rest = rest,
                        .comptime_site = let_.comptime_site,
                    } } });
                };
                if (task.plan.aggregate) |local| _ = self.exit_tuple_items.remove(local);
                self.subst.restore(task.change_start);
                return .{ .ret = .{ .maybe_expr = result } };
            },
        }
    }

    const LoopBodyTask = struct { body: Ast.ExprId, selection: ?LoopExitSelection };

    fn stepLoopBody(self: *Cloner, frame: *CloneFrame, task: *LoopBodyTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            try self.loop_exit_stack.append(self.pass.allocator, task.selection);
            frame.cursor = 1;
            return .{ .call = .{ .expr = task.body } };
        }
        _ = self.loop_exit_stack.pop();
        return .{ .ret = input.? };
    }

    const SelectedLoopExitTask = struct {
        break_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        selection: LoopExitSelection,
        chain: *BindingChain = undefined,
        reusable: Value = undefined,
        args: []Ast.ExprId = &.{},
    };

    fn stepSelectedLoopExit(self: *Cloner, frame: *CloneFrame, task: *SelectedLoopExitTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const selection = task.selection;
        switch (frame.cursor) {
            0 => {
                task.chain = try self.newChain();
                // Exit selection discards tuple construction, never strict work. Name every
                // opaque leaf, including unselected components, in source order.
                frame.cursor = 1;
                return .{ .call = .{ .demanding = .{ .expr = task.value_expr, .bindings = task.chain } } };
            },
            1 => {
                task.reusable = input.?.get(.value);
                task.args = try self.arena.allocator().alloc(Ast.ExprId, selection.kept_indices.len);
                if (task.reusable == .tuple and task.reusable.tuple.ty == selection.source_ty) {
                    if (task.reusable.tuple.items.len != selection.source_arity) {
                        Common.invariant("selected loop exit tuple arity differed from its source ABI");
                    }
                    frame.cursor = 2;
                } else {
                    // Runtime tuples and typed boundaries have exact typed tuple reads
                    // too. Keep their runtime representation boundary and
                    // evaluate the producer once, without building symbolic dead items.
                    frame.cursor = 3;
                    return .{ .call = .{ .materialize = .{ .value = task.reusable } } };
                }
            },
            // A selected tuple item, materialized.
            2 => {
                task.args[frame.index] = input.?.get(.expr);
                frame.index += 1;
            },
            3 => {
                frame.cursor = 4;
                return .{ .call = try self.makeReusableTask(.{ .expr = input.?.get(.expr) }, task.chain) };
            },
            4 => {
                frame.cursor = 5;
                return .{ .call = .{ .materialize = .{ .value = input.?.get(.value) } } };
            },
            else => {
                const receiver = input.?.get(.expr);
                for (selection.kept_indices, selection.kept_types, task.args) |index, ty, *out| {
                    out.* = try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = index,
                    } } });
                }
                return retExpr(try self.selectedLoopExitTransfer(task));
            },
        }
        if (frame.index < selection.kept_indices.len) {
            return .{ .call = .{ .materialize = .{ .value = task.reusable.tuple.items[selection.kept_indices[frame.index]] } } };
        }
        return retExpr(try self.selectedLoopExitTransfer(task));
    }

    fn selectedLoopExitTransfer(self: *Cloner, task: *SelectedLoopExitTask) Common.LowerError!Ast.ExprId {
        const selection = task.selection;
        const projected = switch (selection.transfer) {
            .break_value => blk: {
                if (selection.kept_indices.len != 1) Common.invariant("direct loop exit selection did not contain one value");
                const projected_expr = try self.addExpr(.{
                    .ty = task.break_ty,
                    .data = .{ .break_ = task.args[0] },
                });
                break :blk projected_expr;
            },
            .jump => |jump_transfer| blk: {
                const jump = try self.addExpr(.{
                    .ty = task.break_ty,
                    .data = .{ .jump = .{
                        .target = jump_transfer.target,
                        .args = try self.pass.program.addExprSpan(task.args),
                    } },
                });
                break :blk jump;
            },
        };

        return try self.wrapBindings(task.chain.*, projected);
    }

    fn makeReusableTask(self: *Cloner, value: Value, bindings: *BindingChain) Allocator.Error!CloneTask {
        const budget = try self.arena.allocator().create(u32);
        budget.* = make_reusable_work_budget;
        return .{ .make_reusable = .{ .value = value, .budget = budget, .bindings = bindings } };
    }

    const MakeReusableTask = struct {
        value: Value,
        budget: *u32,
        bindings: *BindingChain,
        local: Ast.LocalId = undefined,
        ty: Type.TypeId = undefined,
        values: []Value = &.{},
        fields: []FieldValue = &.{},
        captures: []CaptureValue = &.{},
    };

    fn stepMakeReusable(self: *Cloner, frame: *CloneFrame, task: *MakeReusableTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const value = task.value;
        const bindings = task.bindings;
        switch (frame.cursor) {
            0 => {
                if (task.budget.* == 0) {
                    task.ty = valueType(self.pass.program, value);
                    task.local = try self.pass.program.addLocal(self.pass.symbols.fresh(), task.ty);
                    frame.cursor = 1;
                    return .{ .call = .{ .materialize = .{ .value = value } } };
                }
                task.budget.* -= 1;
                if (try self.valueCanSubstitute(value) == .proven) return retValue(value);
                switch (value) {
                    .expr => |expr| {
                        const ty = self.pass.program.getExpr(expr).ty;
                        const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                        try bindings.appendBinding(self.arena.allocator(), .{
                            .local = local,
                            .ty = ty,
                            .value = expr,
                        });
                        return retValue(.{ .expr = try self.addExpr(.{
                            .ty = ty,
                            .data = .{ .local = local },
                        }) });
                    },
                    .runtime_anchor => return retValue(value),
                    .static_data_candidate => |candidate| {
                        task.ty = candidate.ty;
                        task.local = try self.pass.program.addLocal(self.pass.symbols.fresh(), candidate.ty);
                        frame.cursor = 1;
                        return .{ .call = .{ .materialize = .{ .value = value } } };
                    },
                    .tag => |tag| task.values = try self.arena.allocator().alloc(Value, tag.payloads.len),
                    .record => |record| task.fields = try self.arena.allocator().alloc(FieldValue, record.fields.len),
                    .tuple => |tuple| task.values = try self.arena.allocator().alloc(Value, tuple.items.len),
                    .nominal => task.values = try self.arena.allocator().alloc(Value, 1),
                    .callable => |callable| task.captures = try self.arena.allocator().alloc(CaptureValue, callable.captures.len),
                }
                frame.cursor = 2;
            },
            // A materialized value named by one strict binding.
            1 => {
                try bindings.appendBinding(self.arena.allocator(), .{
                    .local = task.local,
                    .ty = task.ty,
                    .value = input.?.get(.expr),
                });
                return retValue(.{ .expr = try self.addExpr(.{ .ty = task.ty, .data = .{ .local = task.local } }) });
            },
            else => {
                const child = input.?.get(.value);
                switch (value) {
                    .tag, .tuple, .nominal => task.values[frame.index] = child,
                    .record => |record| task.fields[frame.index] = .{ .name = record.fields[frame.index].name, .value = child },
                    .callable => |callable| task.captures[frame.index] = .{ .id = callable.captures[frame.index].id, .value = child },
                    .expr, .runtime_anchor, .static_data_candidate => unreachable,
                }
                frame.index += 1;
            },
        }
        const next_child: ?Value = switch (value) {
            .tag => |tag| if (frame.index < tag.payloads.len) tag.payloads[frame.index] else null,
            .record => |record| if (frame.index < record.fields.len) record.fields[frame.index].value else null,
            .tuple => |tuple| if (frame.index < tuple.items.len) tuple.items[frame.index] else null,
            .nominal => |nominal| if (frame.index < 1) nominal.backing.* else null,
            .callable => |callable| if (frame.index < callable.captures.len) callable.captures[frame.index].value else null,
            .expr, .runtime_anchor, .static_data_candidate => unreachable,
        };
        if (next_child) |child| {
            return .{ .call = .{ .make_reusable = .{ .value = child, .budget = task.budget, .bindings = bindings } } };
        }
        return retValue(switch (value) {
            .tag => |tag| .{ .tag = .{
                .ty = tag.ty,
                .name = tag.name,
                .payloads = task.values,
            } },
            .record => |record| .{ .record = .{
                .ty = record.ty,
                .fields = task.fields,
            } },
            .tuple => |tuple| .{ .tuple = .{
                .ty = tuple.ty,
                .items = task.values,
            } },
            .nominal => |nominal| blk: {
                const backing = try self.arena.allocator().create(Value);
                backing.* = task.values[0];
                break :blk .{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| .{ .callable = .{
                .ty = callable.ty,
                .fn_id = callable.fn_id,
                .captures = task.captures,
                .iterator_step = callable.iterator_step,
            } },
            .expr, .runtime_anchor, .static_data_candidate => unreachable,
        });
    }

    /// Rewrite `let bind = <match/if> in rest` so every arm transfers its
    /// result to shared continuation code through a join point, without
    /// cloning that continuation into the arms and without losing the arms'
    /// statically known value structure:
    ///
    /// - Each arm's result value must be a known structure (constructor,
    ///   record, tuple, callable). An opaque arm result gains nothing from
    ///   the rewrite and would only push the continuation behind a join—
    ///   defeating downstream tail-call and loop-shape recognition—so the
    ///   rewrite declines and the let lowers as an ordinary binding, exactly
    ///   as arm sinking declined for the same reason.
    /// - When the continuation immediately matches on the bound value, each
    ///   continuation branch becomes its own join point and the arms clone
    ///   only the small dispatching match, which folds against an arm's
    ///   known constructor into a direct jump. Only the dispatch is ever
    ///   copied; continuation code is stored once.
    /// - A join's parameters are the decomposed leaves of the values its
    ///   jump sites supply, whenever those values agree on one structure
    ///   skeleton. The join body re-binds the structured value over the
    ///   parameter locals, so specialization inside the shared continuation
    ///   (loop-state scalarization, worker selection) still sees the shape.
    const LetOfCaseTask = struct {
        let_: LetParts,
        value_expr: Ast.ExprId,
        recorded: ?[]const Value = null,
        rest_ty: Type.TypeId = undefined,
        probe: Ast.LocalId = undefined,
        dispatch: Ast.ExprId = undefined,
        joins: []LetCaseJoin = &.{},
        frame_index: usize = 0,
        match_branches: []const Ast.Branch = &.{},
        match_rewritten: []Ast.Branch = &.{},
        if_branches: []const Ast.IfBranch = &.{},
        if_rewritten: []Ast.IfBranch = &.{},
        change_start: usize = 0,
        result: Ast.ExprData = undefined,
    };

    fn stepLetOfCase(self: *Cloner, frame: *CloneFrame, task: *LetOfCaseTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const value_data = self.pass.program.getExpr(task.value_expr).data;
        switch (frame.cursor) {
            0 => {
                if (value_data != .match_ and value_data != .if_) return .{ .ret = .{ .maybe_data = null } };

                // The arms are read through the values recorded when the case was
                // emitted. The only emitted cases without recorded values are join
                // dispatches, whose arms transfer control and produce no value to
                // bind. A source case reused unchanged by this clone is re-read as
                // source.
                task.recorded = self.arm_values.get(task.value_expr);
                if (task.recorded == null and @intFromEnum(task.value_expr) >= self.output_start) return .{ .ret = .{ .maybe_data = null } };

                const arm_count: usize = if (value_data == .match_)
                    self.pass.program.branchSpan(value_data.match_.branches).len
                else
                    self.pass.program.ifBranchSpan(value_data.if_.branches).len + 1;
                if (self.let_case_shape_growth.admit(arm_count) != .admitted) {
                    return .{ .tail = .{ .let_of_case_shared = .{ .let_ = task.let_, .value_expr = task.value_expr } } };
                }

                const arena = self.arena.allocator();
                const value_ty = self.pass.program.getExpr(task.value_expr).ty;
                task.rest_ty = self.pass.program.getExpr(task.let_.rest).ty;

                // The probe stands for "this arm's result value" while an arm clones
                // the dispatch: each arm substitutes it with its own known value. The
                // dispatch is a template this clone builds to clone repeatedly, so its
                // expressions are registered as clone sources.
                const template_start = self.pass.program.exprCount();
                task.probe = try self.pass.program.addLocal(self.pass.symbols.fresh(), value_ty);
                const probe_ref = try self.addExpr(.{ .ty = value_ty, .data = .{ .local = task.probe } });

                task.joins = try self.letCaseJoinPlan(task.let_, arena);
                task.dispatch = try self.letCaseDispatchExpr(task.let_, task.joins, probe_ref, task.rest_ty);
                try self.registerCloneTemplate(template_start);

                const build = try arena.create(LetCaseBuild);
                build.* = .{ .joins = task.joins };
                task.frame_index = self.let_case_builds.items.len;
                try self.let_case_builds.append(self.pass.allocator, build);

                switch (value_data) {
                    .match_ => |match| {
                        task.match_branches = try GuardedList.dupe(arena, Ast.Branch, self.pass.program.branchSpan(match.branches));
                        task.match_rewritten = try arena.alloc(Ast.Branch, task.match_branches.len);
                    },
                    .if_ => |if_| {
                        task.if_branches = try GuardedList.dupe(arena, Ast.IfBranch, self.pass.program.ifBranchSpan(if_.branches));
                        task.if_rewritten = try arena.alloc(Ast.IfBranch, task.if_branches.len);
                    },
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
                }
                frame.cursor = 1;
            },
            // A rewritten arm.
            1 => {
                const body = input.?.get(.maybe_expr) orelse {
                    if (value_data == .match_) self.subst.restore(task.change_start);
                    self.let_case_builds.shrinkRetainingCapacity(task.frame_index);
                    return .{ .ret = .{ .maybe_data = null } };
                };
                switch (value_data) {
                    .match_ => {
                        self.subst.restore(task.change_start);
                        const branch = task.match_branches[frame.index];
                        task.match_rewritten[frame.index] = .{
                            .pat = branch.pat,
                            .bindings = branch.bindings,
                            .guard = branch.guard,
                            .body = body,
                        };
                    },
                    .if_ => {
                        if (frame.index < task.if_branches.len) {
                            task.if_rewritten[frame.index] = .{
                                .cond = task.if_branches[frame.index].cond,
                                .body = body,
                            };
                        } else {
                            task.result = .{ .if_ = .{
                                .branches = try self.pass.program.addIfBranchSpan(task.if_rewritten),
                                .final_else = body,
                            } };
                        }
                    },
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
                }
                frame.index += 1;
            },
            // A finalized join.
            else => {
                if (input.?.get(.pieces)) |pieces| {
                    const join = &task.joins[frame.index];
                    const remainder = try self.addExpr(.{ .ty = task.rest_ty, .data = task.result });
                    task.result = .{ .join_point = .{
                        .id = join.id,
                        .params = pieces.params,
                        .body = pieces.body,
                        .remainder = remainder,
                    } };
                }
                return try self.nextLetCaseJoin(frame, task);
            },
        }
        switch (value_data) {
            .match_ => |match| {
                if (frame.index < task.match_branches.len) {
                    const branch = task.match_branches[frame.index];
                    task.change_start = self.subst.watermark();
                    const recorded_value: ?Value = if (task.recorded) |values| values[frame.index] else null;
                    if (recorded_value == null) {
                        try self.shadowPatLocals(branch.pat);
                        try self.shadowStmtSpanLocals(branch.bindings);
                    }
                    return .{ .call = .{ .let_case_arm_body = .{ .probe = task.probe, .dispatch = task.dispatch, .branch_body = branch.body, .recorded_value = recorded_value } } };
                }
                task.result = .{ .match_ = .{
                    .scrutinee = match.scrutinee,
                    .branches = try self.pass.program.addBranchSpan(task.match_rewritten),
                    .comptime_site = match.comptime_site,
                } };
            },
            .if_ => |if_| {
                if (frame.index < task.if_branches.len) {
                    return .{ .call = .{ .let_case_arm_body = .{
                        .probe = task.probe,
                        .dispatch = task.dispatch,
                        .branch_body = task.if_branches[frame.index].body,
                        .recorded_value = if (task.recorded) |values| values[frame.index] else null,
                    } } };
                }
                if (frame.index == task.if_branches.len) {
                    return .{ .call = .{ .let_case_arm_body = .{
                        .probe = task.probe,
                        .dispatch = task.dispatch,
                        .branch_body = if_.final_else,
                        .recorded_value = if (task.recorded) |values| values[task.if_branches.len] else null,
                    } } };
                }
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
        }

        // Different outer constructors cannot expose shared leaves to one continuation.
        if (task.joins.len == 1 and task.joins[0].binding == .pattern and task.joins[0].sites.items.len > 1) {
            const sites = task.joins[0].sites.items;
            const outer_values = try self.pass.allocator.alloc(Value, sites.len);
            defer self.pass.allocator.free(outer_values);
            for (sites, outer_values) |site, *value| {
                if (site.values.len != 1) Common.invariant("let-of-case pattern join site did not supply one value");
                value.* = site.values[0];
            }
            if (!self.valuesShareOuterSkeleton(outer_values)) {
                self.let_case_builds.shrinkRetainingCapacity(task.frame_index);
                return .{ .ret = .{ .maybe_data = null } };
            }
        }

        // Wrap the rewritten case in its live join points, innermost last so
        // every jump site in the case sits inside each join's remainder.
        frame.index = task.joins.len;
        return try self.nextLetCaseJoin(frame, task);
    }

    fn nextLetCaseJoin(self: *Cloner, frame: *CloneFrame, task: *LetOfCaseTask) Common.LowerError!CloneStep {
        while (frame.index > 0) {
            frame.index -= 1;
            const join = &task.joins[frame.index];
            if (join.sites.items.len == 0) continue;
            frame.cursor = 2;
            return .{ .call = .{ .finalize_let_case_join = .{ .join = join, .rest_ty = task.rest_ty } } };
        }
        self.let_case_builds.shrinkRetainingCapacity(task.frame_index);
        return .{ .ret = .{ .maybe_data = task.result } };
    }

    /// The budget-exhausted shape: one join point whose single parameter is
    /// the branch-built value, with every already-cloned arm body threaded to
    /// it as a jump argument. Stores no copy of arm bodies or continuation,
    /// so it is safe at any nesting depth; it keeps no static value shapes.
    const LetOfCaseSharedTask = struct {
        let_: LetParts,
        value_expr: Ast.ExprId,
        join_param: Ast.LocalId = undefined,
        change_start: usize = 0,
        bind: Ast.PatId = undefined,
    };

    fn stepLetOfCaseShared(self: *Cloner, frame: *CloneFrame, task: *LetOfCaseSharedTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const let_ = task.let_;
        const value_ty = self.pass.program.getExpr(task.value_expr).ty;
        if (frame.cursor == 0) {
            task.join_param = try self.pass.program.addLocal(self.pass.symbols.fresh(), value_ty);
            task.change_start = self.subst.watermark();
            task.bind = try self.clonePat(let_.bind, .bind_runtime);
            frame.cursor = 1;
            return .{ .call = .{ .expr = let_.rest } };
        }
        const rest = input.?.get(.expr);
        const value_data = self.pass.program.getExpr(task.value_expr).data;
        const rest_ty = self.pass.program.getExpr(let_.rest).ty;
        const params = [_]Ast.TypedLocal{.{ .local = task.join_param, .ty = value_ty }};
        self.subst.restore(task.change_start);
        return .{ .ret = .{ .maybe_data = try self.letOfCaseSharedJoin(let_, task.value_expr, value_data, rest_ty, task.join_param, &params, task.bind, rest) } };
    }

    fn letOfCaseSharedJoin(
        self: *Cloner,
        let_: LetParts,
        value_expr: Ast.ExprId,
        value_data: Ast.ExprData,
        rest_ty: Type.TypeId,
        join_param: Ast.LocalId,
        params: []const Ast.TypedLocal,
        bind: Ast.PatId,
        rest: Ast.ExprId,
    ) Common.LowerError!Ast.ExprData {
        const value_ty = self.pass.program.getExpr(value_expr).ty;
        const param_expr = try self.addExpr(.{ .ty = value_ty, .data = .{ .local = join_param } });
        const continuation = try self.addExpr(.{ .ty = rest_ty, .data = .{ .let_ = .{
            .bind = bind,
            .value = param_expr,
            .rest = rest,
            .comptime_site = let_.comptime_site,
        } } });

        const join_id = self.pass.freshJoinPoint();
        const remainder = switch (value_data) {
            .match_ => |match| blk: {
                const branches = try GuardedList.dupe(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(match.branches));
                defer self.pass.allocator.free(branches);
                const rewritten = try self.pass.allocator.alloc(Ast.Branch, branches.len);
                defer self.pass.allocator.free(rewritten);
                for (branches, 0..) |branch, index| {
                    const args = [_]Ast.ExprId{branch.body};
                    rewritten[index] = .{
                        .pat = branch.pat,
                        .bindings = branch.bindings,
                        .guard = branch.guard,
                        .body = try self.addExpr(.{ .ty = rest_ty, .data = .{ .jump = .{
                            .target = join_id,
                            .args = try self.pass.program.addExprSpan(&args),
                        } } }),
                    };
                }
                break :blk try self.addExpr(.{ .ty = rest_ty, .data = .{ .match_ = .{
                    .scrutinee = match.scrutinee,
                    .branches = try self.pass.program.addBranchSpan(rewritten),
                    .comptime_site = match.comptime_site,
                } } });
            },
            .if_ => |if_| blk: {
                const branches = try GuardedList.dupe(self.pass.allocator, Ast.IfBranch, self.pass.program.ifBranchSpan(if_.branches));
                defer self.pass.allocator.free(branches);
                const rewritten = try self.pass.allocator.alloc(Ast.IfBranch, branches.len);
                defer self.pass.allocator.free(rewritten);
                for (branches, 0..) |branch, index| {
                    const args = [_]Ast.ExprId{branch.body};
                    rewritten[index] = .{
                        .cond = branch.cond,
                        .body = try self.addExpr(.{ .ty = rest_ty, .data = .{ .jump = .{
                            .target = join_id,
                            .args = try self.pass.program.addExprSpan(&args),
                        } } }),
                    };
                }
                const else_args = [_]Ast.ExprId{if_.final_else};
                const final_else = try self.addExpr(.{ .ty = rest_ty, .data = .{ .jump = .{
                    .target = join_id,
                    .args = try self.pass.program.addExprSpan(&else_args),
                } } });
                break :blk try self.addExpr(.{ .ty = rest_ty, .data = .{ .if_ = .{
                    .branches = try self.pass.program.addIfBranchSpan(rewritten),
                    .final_else = final_else,
                } } });
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
        };

        return .{ .join_point = .{
            .id = join_id,
            .params = try self.pass.program.addTypedLocalSpan(params),
            .body = continuation,
            .remainder = remainder,
        } };
    }

    /// Clone the dispatch into one arm of the let's case. `recorded_value` is
    /// the arm's result value recorded when the arm was emitted; the emitted
    /// arm keeps its statements as they stand and only its tail changes, so
    /// no emitted expression is ever re-derived. Only a source arm reused
    /// unchanged by this clone has no recorded value, and re-reading it is
    /// reading source. Returns null when the arm's value is opaque,
    /// declining the whole rewrite.
    const LetCaseArmBodyTask = struct {
        probe: Ast.LocalId,
        dispatch: Ast.ExprId,
        branch_body: Ast.ExprId,
        recorded_value: ?Value,
        statements: ?Ast.Span(Ast.StmtId) = null,
        change_start: usize = 0,
        arm: ClonedValue = undefined,
        emitted_statements: std.ArrayList(Ast.StmtId) = .empty,
    };

    fn stepLetCaseArmBody(self: *Cloner, frame: *CloneFrame, task: *LetCaseArmBodyTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const dispatch_ty = self.pass.program.getExpr(task.dispatch).ty;
        const branch_expr = self.pass.program.getExpr(task.branch_body);
        if (task.recorded_value) |recorded| {
            switch (frame.cursor) {
                0 => {
                    const structure = self.emittedArmStructure(task.branch_body, recorded);
                    task.statements = if (branch_expr.data == .block) branch_expr.data.block.statements else null;
                    const tail_expr = if (branch_expr.data == .block) branch_expr.data.block.final_expr else task.branch_body;
                    if (structure == .expr) {
                        frame.cursor = 1;
                        return .{ .call = .{ .divergent_tail = .{ .expr = tail_expr, .ty = dispatch_ty, .origin = .emitted } } };
                    }
                    task.change_start = self.subst.watermark();
                    try self.subst.put(self.pass.program, task.probe, structure);
                    frame.cursor = 2;
                    return .{ .call = .{ .expr = task.dispatch } };
                },
                else => {
                    const final = if (frame.cursor == 1)
                        input.?.get(.maybe_expr) orelse return .{ .ret = .{ .maybe_expr = null } }
                    else blk: {
                        self.subst.restore(task.change_start);
                        break :blk input.?.get(.expr);
                    };
                    if (task.statements) |stmts| {
                        return .{ .ret = .{ .maybe_expr = try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                            .statements = stmts,
                            .final_expr = final,
                        } } }) } };
                    }
                    return .{ .ret = .{ .maybe_expr = final } };
                },
            }
        }
        switch (branch_expr.data) {
            .block => |block| switch (frame.cursor) {
                0 => {
                    // A branch-built or looping tail always derives an opaque
                    // value and is never divergent: decline without re-deriving,
                    // so the nested rewrites inside it do not rerun just to be
                    // thrown away.
                    switch (self.pass.program.getExpr(block.final_expr).data) {
                        .match_, .if_, .loop_ => return .{ .ret = .{ .maybe_expr = null } },
                        .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
                    }

                    task.change_start = self.subst.watermark();
                    const source = self.pass.program.stmtSpan(block.statements);
                    for (0..GuardedList.borrowLen(source)) |index| {
                        try task.emitted_statements.append(self.pass.allocator, GuardedList.at(source, index));
                    }
                    frame.cursor = 1;
                    return .{ .call = .{ .expr_value_owned = .{ .expr = block.final_expr, .demand_shape = false } } };
                },
                1 => {
                    task.arm = input.?.get(.cloned);
                    if (task.arm.value == .expr) {
                        frame.cursor = 2;
                        return .{ .call = .{ .divergent_tail = .{ .expr = block.final_expr, .ty = dispatch_ty, .origin = .source } } };
                    }
                    try self.subst.put(self.pass.program, task.probe, task.arm.value);
                    try self.appendBindingStmts(task.arm.bindings, &task.emitted_statements);
                    frame.cursor = 3;
                    return .{ .call = .{ .expr = task.dispatch } };
                },
                2 => {
                    self.subst.restore(task.change_start);
                    const divergent = input.?.get(.maybe_expr) orelse {
                        task.emitted_statements.deinit(self.pass.allocator);
                        return .{ .ret = .{ .maybe_expr = null } };
                    };
                    try self.appendBindingStmts(task.arm.bindings, &task.emitted_statements);
                    const emitted = try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                        .statements = try self.pass.program.addStmtSpan(task.emitted_statements.items),
                        .final_expr = divergent,
                    } } });
                    task.emitted_statements.deinit(self.pass.allocator);
                    return .{ .ret = .{ .maybe_expr = emitted } };
                },
                else => {
                    const rest = input.?.get(.expr);
                    self.subst.restore(task.change_start);
                    const emitted = try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                        .statements = try self.pass.program.addStmtSpan(task.emitted_statements.items),
                        .final_expr = rest,
                    } } });
                    task.emitted_statements.deinit(self.pass.allocator);
                    return .{ .ret = .{ .maybe_expr = emitted } };
                },
            },
            .match_, .if_, .loop_ => return .{ .ret = .{ .maybe_expr = null } },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => switch (frame.cursor) {
                0 => {
                    frame.cursor = 1;
                    return .{ .call = .{ .expr_value_owned = .{ .expr = task.branch_body, .demand_shape = false } } };
                },
                1 => {
                    task.arm = input.?.get(.cloned);
                    task.change_start = self.subst.watermark();
                    if (task.arm.value == .expr) {
                        frame.cursor = 2;
                        return .{ .call = .{ .divergent_tail = .{ .expr = task.branch_body, .ty = dispatch_ty, .origin = .source } } };
                    }
                    try self.subst.put(self.pass.program, task.probe, task.arm.value);
                    frame.cursor = 3;
                    return .{ .call = .{ .expr = task.dispatch } };
                },
                2 => {
                    self.subst.restore(task.change_start);
                    const divergent = input.?.get(.maybe_expr) orelse return .{ .ret = .{ .maybe_expr = null } };
                    return .{ .ret = .{ .maybe_expr = try self.wrapBindings(task.arm.bindings, divergent) } };
                },
                else => {
                    const rest = try self.wrapBindings(task.arm.bindings, input.?.get(.expr));
                    self.subst.restore(task.change_start);
                    return .{ .ret = .{ .maybe_expr = rest } };
                },
            },
        }
    }

    /// Where a divergent tail lives: a source tail's operands are cloned,
    /// while an emitted tail is dead once its arm is rebuilt, so its operands
    /// move into the retyped tail as they stand.
    const DivergentTailOrigin = enum { source, emitted };

    const DivergentTailTask = struct { expr: Ast.ExprId, ty: Type.TypeId, origin: DivergentTailOrigin };

    /// Emit a divergent tail at `ty` when `expr` is one, otherwise null.
    fn stepDivergentTail(self: *Cloner, frame: *CloneFrame, task: *DivergentTailTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const expr = self.pass.program.getExpr(task.expr);
        const ty = task.ty;
        return .{ .ret = .{ .maybe_expr = switch (expr.data) {
            .@"unreachable" => try self.addExpr(.{ .ty = ty, .data = .@"unreachable" }),
            .crash => |msg| try self.addExpr(.{ .ty = ty, .data = .{ .crash = msg } }),
            .checked_error => |msg| try self.addExpr(.{ .ty = ty, .data = .{ .checked_error = msg } }),
            .comptime_exhaustiveness_failed => |site| try self.addExpr(.{ .ty = ty, .data = .{ .comptime_exhaustiveness_failed = site } }),
            .return_ => |ret| blk: {
                const value = switch (task.origin) {
                    .source => if (frame.cursor == 0) {
                        frame.cursor = 1;
                        return .{ .call = .{ .expr = ret.value } };
                    } else input.?.get(.expr),
                    .emitted => ret.value,
                };
                break :blk try self.addExpr(.{ .ty = ty, .data = .{ .return_ = .{
                    .value = value,
                    .target = ret.target,
                } } });
            },
            .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .comptime_branch_taken, .dbg, .expect_err, .expect, .literal_rejected => null,
        } } };
    }

    /// Record a jump into an active let-of-case join: capture the symbolic
    /// value of every argument and emit a placeholder jump whose argument
    /// span is patched once the join's parameters are decided.
    const CaptureLetCaseJumpTask = struct {
        ty: Type.TypeId,
        join: *LetCaseJoin,
        jump: Ast.JumpExpr,
        args: []const Ast.ExprId = &.{},
        values: []Value = &.{},
        chain: *BindingChain = undefined,
    };

    fn stepCaptureLetCaseJump(self: *Cloner, frame: *CloneFrame, task: *CaptureLetCaseJumpTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        if (frame.cursor == 0) {
            task.args = try GuardedList.dupe(arena, Ast.ExprId, self.pass.program.exprSpan(task.jump.args));
            task.values = try arena.alloc(Value, task.args.len);
            task.chain = try self.newChain();
            frame.cursor = 1;
        } else {
            task.values[frame.index] = input.?.get(.value);
            frame.index += 1;
        }
        if (frame.index < task.args.len) {
            return .{ .call = .{ .demanding = .{ .expr = task.args[frame.index], .bindings = task.chain } } };
        }
        const placeholder = try self.addExpr(.{ .ty = task.ty, .data = .{ .jump = .{
            .target = task.join.id,
            .args = try self.pass.program.addExprSpan(&[_]Ast.ExprId{}),
        } } });
        try task.join.sites.append(arena, .{ .expr = placeholder, .bindings = task.chain.*, .values = task.values });
        return retExpr(placeholder);
    }

    /// Clone a join's continuation body directly at its only jump site,
    /// binding the continuation's binders to the site's symbolic values so
    /// the shared code keeps every statically known shape. The placeholder
    /// jump expression is overwritten with the cloned body.
    const InlineLetCaseJoinTask = struct {
        join: *LetCaseJoin,
        site: LetCaseJumpSite,
        rest_ty: Type.TypeId,
        change_start: usize = 0,
        value_expr: Ast.ExprId = undefined,
        bind: Ast.PatId = undefined,
    };

    fn stepInlineLetCaseJoin(self: *Cloner, frame: *CloneFrame, task: *InlineLetCaseJoinTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const join = task.join;
        const site = task.site;
        const body = switch (frame.cursor) {
            0 => {
                task.change_start = self.subst.watermark();
                switch (join.binding) {
                    .locals => |locals| {
                        if (site.values.len != locals.len) {
                            Common.invariant("let-of-case jump site argument count differed from join binder count");
                        }
                        for (locals, site.values) |local, value| try self.subst.put(self.pass.program, local, value);
                        frame.cursor = 1;
                        return .{ .call = .{ .expr = join.body } };
                    },
                    .pattern => |binding| {
                        if (try self.bindPatToFlowValue(binding.pat, site.values[0])) {
                            frame.cursor = 1;
                            return .{ .call = .{ .expr = join.body } };
                        }
                        // The pattern could not consume the value's structure; keep
                        // an ordinary let of the materialized value at the site.
                        self.subst.restore(task.change_start);
                        frame.cursor = 2;
                        return .{ .call = .{ .materialize = .{ .value = site.values[0] } } };
                    },
                }
            },
            1 => blk: {
                const body = input.?.get(.expr);
                self.subst.restore(task.change_start);
                break :blk body;
            },
            2 => {
                task.value_expr = input.?.get(.expr);
                task.change_start = self.subst.watermark();
                task.bind = try self.clonePat(join.binding.pattern.pat, .bind_runtime);
                frame.cursor = 3;
                return .{ .call = .{ .expr = join.body } };
            },
            else => blk: {
                const rest = input.?.get(.expr);
                self.subst.restore(task.change_start);
                break :blk try self.addExpr(.{ .ty = task.rest_ty, .data = .{ .let_ = .{
                    .bind = task.bind,
                    .value = task.value_expr,
                    .rest = rest,
                    .comptime_site = join.binding.pattern.comptime_site,
                } } });
            },
        };
        const wrapped = try self.wrapBindings(site.bindings, body);
        self.pass.program.setExprData(site.expr, self.pass.program.getExpr(wrapped).data);
        return .{ .ret = .none };
    }

    const LetCaseJoinPieces = struct {
        params: Ast.Span(Ast.TypedLocal),
        body: Ast.ExprId,
    };

    /// Decompose a join's incoming values into shared parameters, clone the
    /// join's continuation body once against the rebuilt values, and patch
    /// every jump site with its leaf arguments. A join with exactly one jump
    /// site stores no continuation copy either way, so its body is cloned
    /// directly at the site—against the site's full symbolic values—and
    /// no join point is emitted (null).
    const FinalizeLetCaseJoinTask = struct {
        join: *LetCaseJoin,
        rest_ty: Type.TypeId,
        slot_count: usize = 0,
        params: *std.ArrayList(Ast.TypedLocal) = undefined,
        site_args: []std.ArrayList(Ast.ExprId) = &.{},
        rebuilt: []Value = &.{},
        budget: *CodeGrowthBudget = undefined,
        change_start: usize = 0,
        param_local: Ast.LocalId = undefined,
        bind: Ast.PatId = undefined,
        body: Ast.ExprId = undefined,
    };

    fn stepFinalizeLetCaseJoin(self: *Cloner, frame: *CloneFrame, task: *FinalizeLetCaseJoinTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const join = task.join;
        const sites = join.sites.items;
        switch (frame.cursor) {
            0 => {
                if (sites.len == 1) {
                    frame.cursor = 1;
                    return .{ .call = .{ .inline_let_case_join = .{ .join = join, .site = sites[0], .rest_ty = task.rest_ty } } };
                }
                task.slot_count = switch (join.binding) {
                    .pattern => 1,
                    .locals => |locals| locals.len,
                };
                task.params = try arena.create(std.ArrayList(Ast.TypedLocal));
                task.params.* = .empty;
                task.site_args = try arena.alloc(std.ArrayList(Ast.ExprId), sites.len);
                for (task.site_args) |*list| list.* = .empty;
                task.rebuilt = try arena.alloc(Value, task.slot_count);
                task.budget = try arena.create(CodeGrowthBudget);
                task.budget.* = CodeGrowthBudget.init(let_case_join_leaf_budget);
                frame.cursor = 2;
            },
            1 => return .{ .ret = .{ .pieces = null } },
            // A rebuilt slot.
            2 => {
                task.rebuilt[frame.index] = input.?.get(.value);
                frame.index += 1;
            },
            // The continuation body cloned against the rebuilt values.
            3 => {
                task.body = input.?.get(.expr);
                self.subst.restore(task.change_start);
                return .{ .ret = .{ .pieces = try self.patchLetCaseJoinSites(task) } };
            },
            // One site's value, materialized for the opaque parameter.
            4 => {
                try task.site_args[frame.index].append(arena, input.?.get(.expr));
                frame.index += 1;
                return try self.nextOpaqueJoinArgument(frame, task);
            },
            // The continuation body behind the opaque parameter's let.
            else => {
                const rest = input.?.get(.expr);
                self.subst.restore(task.change_start);
                task.body = try self.addExpr(.{ .ty = task.rest_ty, .data = .{ .let_ = .{
                    .bind = task.bind,
                    .value = task.body,
                    .rest = rest,
                    .comptime_site = join.binding.pattern.comptime_site,
                } } });
                return .{ .ret = .{ .pieces = try self.patchLetCaseJoinSites(task) } };
            },
        }
        if (frame.index < task.slot_count) {
            const slot_values = try arena.alloc(Value, sites.len);
            for (sites, 0..) |site, site_index| {
                if (site.values.len != task.slot_count) {
                    Common.invariant("let-of-case jump site argument count differed from join binder count");
                }
                slot_values[site_index] = site.values[frame.index];
            }
            return .{ .call = .{ .rebuild_let_case_join_value = .{
                .values = slot_values,
                .params = task.params,
                .site_args = task.site_args,
                .budget = task.budget,
            } } };
        }

        task.change_start = self.subst.watermark();
        switch (join.binding) {
            .locals => |locals| {
                for (locals, task.rebuilt) |local, value| try self.subst.put(self.pass.program, local, value);
                frame.cursor = 3;
                return .{ .call = .{ .expr = join.body } };
            },
            .pattern => |binding| {
                if (try self.bindPatToFlowValue(binding.pat, task.rebuilt[0])) {
                    frame.cursor = 3;
                    return .{ .call = .{ .expr = join.body } };
                }
                // The pattern could not consume the rebuilt structure; fall
                // back to one opaque parameter bound by an ordinary let.
                self.subst.restore(task.change_start);
                task.params.clearRetainingCapacity();
                for (task.site_args) |*list| list.clearRetainingCapacity();
                const param_ty = valueType(self.pass.program, sites[0].values[0]);
                task.param_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), param_ty);
                try task.params.append(arena, .{ .local = task.param_local, .ty = param_ty });
                frame.index = 0;
                return try self.nextOpaqueJoinArgument(frame, task);
            },
        }
    }

    fn nextOpaqueJoinArgument(self: *Cloner, frame: *CloneFrame, task: *FinalizeLetCaseJoinTask) Common.LowerError!CloneStep {
        const sites = task.join.sites.items;
        if (frame.index < sites.len) {
            frame.cursor = 4;
            return .{ .call = .{ .materialize = .{ .value = sites[frame.index].values[0] } } };
        }
        const param_ty = valueType(self.pass.program, sites[0].values[0]);
        // `body` holds the parameter reference until the let is built.
        task.body = try self.addExpr(.{ .ty = param_ty, .data = .{ .local = task.param_local } });
        task.change_start = self.subst.watermark();
        task.bind = try self.clonePat(task.join.binding.pattern.pat, .bind_runtime);
        frame.cursor = 5;
        return .{ .call = .{ .expr = task.join.body } };
    }

    fn patchLetCaseJoinSites(self: *Cloner, task: *FinalizeLetCaseJoinTask) Common.LowerError!LetCaseJoinPieces {
        const join = task.join;
        for (join.sites.items, task.site_args) |site, list| {
            const jump = try self.addExpr(.{ .ty = task.rest_ty, .data = .{ .jump = .{
                .target = join.id,
                .args = try self.pass.program.addExprSpan(list.items),
            } } });
            const wrapped = try self.wrapBindings(site.bindings, jump);
            self.pass.program.setExprData(site.expr, self.pass.program.getExpr(wrapped).data);
        }

        return .{
            .params = try self.pass.program.addTypedLocalSpan(task.params.items),
            .body = task.body,
        };
    }

    /// Node budget and parameter cap for decomposing one join's incoming
    /// values. Values can be compact graphs reached by combinatorially many
    /// paths (see `make_reusable_work_budget`), so the walk spends one shared
    /// budget per node and keeps any remaining sub-value as one opaque
    /// parameter when it runs out.
    const let_case_join_leaf_budget: u32 = 1024;
    const let_case_join_param_cap: usize = 64;

    /// Whether every value has the same outermost constructor: the same tag,
    /// record fields, tuple arity, nominal type, or callable target and
    /// capture identities. Only such values decompose into shared leaves.
    fn valuesShareOuterSkeleton(self: *Cloner, values: []const Value) bool {
        const first = values[0];
        for (values[1..]) |other| {
            if (std.meta.activeTag(other) != std.meta.activeTag(first)) return false;
            switch (first) {
                .expr, .runtime_anchor, .static_data_candidate => return false,
                .tag => |first_tag| {
                    const other_tag = other.tag;
                    if (other_tag.ty != first_tag.ty) return false;
                    if (!self.pass.program.names.tagLabelTextEql(other_tag.name, first_tag.name)) return false;
                    if (other_tag.payloads.len != first_tag.payloads.len) return false;
                },
                .record => |first_record| {
                    const other_record = other.record;
                    if (other_record.ty != first_record.ty) return false;
                    if (other_record.fields.len != first_record.fields.len) return false;
                    for (other_record.fields, first_record.fields) |other_field, first_field| {
                        if (!self.pass.program.names.recordFieldLabelTextEql(other_field.name, first_field.name)) return false;
                    }
                },
                .tuple => |first_tuple| {
                    const other_tuple = other.tuple;
                    if (other_tuple.ty != first_tuple.ty) return false;
                    if (other_tuple.items.len != first_tuple.items.len) return false;
                },
                .nominal => |first_nominal| {
                    if (other.nominal.ty != first_nominal.ty) return false;
                },
                .callable => |first_callable| {
                    const other_callable = other.callable;
                    if (other_callable.ty != first_callable.ty) return false;
                    if (other_callable.fn_id != first_callable.fn_id) return false;
                    if (other_callable.captures.len != first_callable.captures.len) return false;
                    for (other_callable.captures, first_callable.captures) |other_capture, first_capture| {
                        if (other_capture.id != first_capture.id) return false;
                    }
                },
            }
        }
        return switch (first) {
            .expr, .runtime_anchor, .static_data_candidate => false,
            .tag, .record, .tuple, .nominal, .callable => true,
        };
    }

    /// Structure-decompose the values every site supplies for one binder
    /// slot. Where all sites agree on the same constructor skeleton, the
    /// skeleton is rebuilt over fresh parameter locals minted for its opaque
    /// leaves and each site's leaf expressions become its jump arguments; any
    /// disagreement (or an exhausted budget) makes that position one opaque
    /// parameter.
    const RebuildLetCaseJoinValueTask = struct {
        values: []const Value,
        params: *std.ArrayList(Ast.TypedLocal),
        site_args: []std.ArrayList(Ast.ExprId),
        budget: *CodeGrowthBudget,
        structured: bool = false,
        param_local: Ast.LocalId = undefined,
        children: []Value = &.{},
        fields: []FieldValue = &.{},
        captures: []CaptureValue = &.{},
        iterator_step: bool = false,
    };

    fn rebuildLetCaseJoinIsStructured(self: *Cloner, task: *RebuildLetCaseJoinValueTask) bool {
        const values = task.values;
        if (task.params.items.len >= let_case_join_param_cap) return false;
        if (task.budget.admit(1) != .admitted) return false;
        if (!self.valuesShareOuterSkeleton(values)) return false;
        if (values[0] == .callable) {
            task.iterator_step = values[0].callable.iterator_step;
            for (values[1..]) |other| task.iterator_step = task.iterator_step and other.callable.iterator_step;
        }
        return true;
    }

    fn stepRebuildLetCaseJoinValue(self: *Cloner, frame: *CloneFrame, task: *RebuildLetCaseJoinValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const values = task.values;
        switch (frame.cursor) {
            0 => {
                if (values.len == 0) Common.invariant("let-of-case join had no jump sites to decompose");
                task.structured = self.rebuildLetCaseJoinIsStructured(task);
                if (!task.structured) {
                    // Opaque leaf: one parameter; each site materializes its own value.
                    const leaf_ty = valueType(self.pass.program, values[0]);
                    task.param_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), leaf_ty);
                    try task.params.append(arena, .{ .local = task.param_local, .ty = leaf_ty });
                    frame.cursor = 2;
                    return .{ .call = .{ .materialize = .{ .value = values[0] } } };
                }
                switch (values[0]) {
                    .tag => |first| task.children = try arena.alloc(Value, first.payloads.len),
                    .record => |first| task.fields = try arena.alloc(FieldValue, first.fields.len),
                    .tuple => |first| task.children = try arena.alloc(Value, first.items.len),
                    .nominal => task.children = try arena.alloc(Value, 1),
                    .callable => |first| task.captures = try arena.alloc(CaptureValue, first.captures.len),
                    .expr, .runtime_anchor, .static_data_candidate => unreachable,
                }
                frame.cursor = 1;
            },
            // A rebuilt child position.
            1 => {
                const child = input.?.get(.value);
                switch (values[0]) {
                    .tag, .tuple, .nominal => task.children[frame.index] = child,
                    .record => |first| task.fields[frame.index] = .{ .name = first.fields[frame.index].name, .value = child },
                    .callable => |first| task.captures[frame.index] = .{ .id = first.captures[frame.index].id, .value = child },
                    .expr, .runtime_anchor, .static_data_candidate => unreachable,
                }
                frame.index += 1;
            },
            // One site's leaf, materialized as its jump argument.
            else => {
                try task.site_args[frame.index].append(arena, input.?.get(.expr));
                frame.index += 1;
                if (frame.index < values.len) {
                    return .{ .call = .{ .materialize = .{ .value = values[frame.index] } } };
                }
                const leaf_ty = valueType(self.pass.program, values[0]);
                return retValue(.{ .expr = try self.addExpr(.{ .ty = leaf_ty, .data = .{ .local = task.param_local } }) });
            },
        }
        const child_count: usize = switch (values[0]) {
            .tag => |first| first.payloads.len,
            .record => |first| first.fields.len,
            .tuple => |first| first.items.len,
            .nominal => 1,
            .callable => |first| first.captures.len,
            .expr, .runtime_anchor, .static_data_candidate => unreachable,
        };
        if (frame.index < child_count) {
            const children = try arena.alloc(Value, values.len);
            for (values, children) |value, *child| child.* = switch (value) {
                .tag => |tag| tag.payloads[frame.index],
                .record => |record| record.fields[frame.index].value,
                .tuple => |tuple| tuple.items[frame.index],
                .nominal => |nominal| nominal.backing.*,
                .callable => |callable| callable.captures[frame.index].value,
                .expr, .runtime_anchor, .static_data_candidate => unreachable,
            };
            return .{ .call = .{ .rebuild_let_case_join_value = .{
                .values = children,
                .params = task.params,
                .site_args = task.site_args,
                .budget = task.budget,
            } } };
        }
        return retValue(switch (values[0]) {
            .tag => |first| .{ .tag = .{ .ty = first.ty, .name = first.name, .payloads = task.children } },
            .record => |first| .{ .record = .{ .ty = first.ty, .fields = task.fields } },
            .tuple => |first| .{ .tuple = .{ .ty = first.ty, .items = task.children } },
            .nominal => |first| blk: {
                const backing = try arena.create(Value);
                backing.* = task.children[0];
                break :blk .{ .nominal = .{ .ty = first.ty, .backing = backing } };
            },
            .callable => |first| .{ .callable = .{
                .ty = first.ty,
                .fn_id = first.fn_id,
                .captures = task.captures,
                .iterator_step = task.iterator_step,
            } },
            .expr, .runtime_anchor, .static_data_candidate => unreachable,
        });
    }

    const LoopValueTask = struct {
        ty: Type.TypeId,
        loop: @FieldType(Ast.ExprData, "loop_"),
        bindings: *BindingChain,
        exit_selection: ?LoopExitSelection,
        params: []const Ast.TypedLocal = &.{},
        initial_values: []const Ast.ExprId = &.{},
        values: []Value = &.{},
        shapes: []Shape = &.{},
        has_constructor: bool = false,
        forward_start: usize = 0,
        change_start: usize = 0,
        carried_identities: []?BinderIdentity = &.{},
        attempt_mark: LoopAttemptMark = undefined,
        new_params: *std.ArrayList(Ast.TypedLocal) = undefined,
        new_initials: *std.ArrayList(Ast.ExprId) = undefined,
        attempt_lists_owned: bool = false,
        split_start: usize = 0,
        forward_sources: std.ArrayList(Ast.LocalId) = .empty,
        forward_finals: std.ArrayList(Ast.LocalId) = .empty,
        initial_exprs: []Ast.ExprId = &.{},
        whole_params: []Ast.TypedLocal = &.{},
    };

    /// Cursor states of a loop-value clone.
    const LoopValueCursor = struct {
        const start = 0;
        const initial_value = 1;
        const split_initials = 2;
        const split_body = 3;
        const whole_initial = 4;
        const whole_body = 5;
    };

    fn finishLoopValue(self: *Cloner, task: *LoopValueTask, value: Value) CloneStep {
        for (task.carried_identities) |identity| {
            if (identity) |carried| self.subst.unmarkLoopCarried(carried);
        }
        self.subst.restore(task.change_start);
        return retValue(value);
    }

    fn stepLoopValue(self: *Cloner, frame: *CloneFrame, task: *LoopValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        switch (frame.cursor) {
            LoopValueCursor.start => {
                task.params = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(task.loop.params));
                task.initial_values = try GuardedList.dupe(arena, Ast.ExprId, self.pass.program.exprSpan(task.loop.initial_values));
                if (task.params.len != task.initial_values.len) Common.invariant("loop parameter count differed from initial value count");

                task.values = try arena.alloc(Value, task.initial_values.len);
                task.shapes = try arena.alloc(Shape, task.initial_values.len);
                // An initial value may forward-reference the loop's own params: an
                // `uninitialized_payload` argument names the flag param that carries
                // its initialized-ness at the loop head. The emitted params do not
                // exist yet, so pin those references to the source param ids while
                // cloning; each emission below retargets them to its fresh params.
                //
                // The pin is exact, never binder-wide. A loop-carried `var` shares one
                // binder across its pre-loop version, its param, and its post-loop
                // version, and the slot's own initial value is that pre-loop version.
                // Claiming the param binder-wide here would resolve that initial value
                // to the param, so the loop would seed its slot from the binding it is
                // about to introduce. The param becomes the binder's value only inside
                // the loop, which is what `putLoopCarried` installs below.
                task.forward_start = self.subst.watermark();
                for (task.params) |param| try self.pinSourceLocal(param.local);
                frame.cursor = LoopValueCursor.initial_value;
            },
            LoopValueCursor.initial_value => {
                const index = frame.index;
                task.values[index] = input.?.get(.value);
                if (self.purpose == .loop_exit_selection) {
                    task.shapes[index] = .{ .any = task.params[index].ty };
                } else switch (try self.pass.shapeFromValue(task.values[index])) {
                    .proven => |shape| {
                        task.shapes[index] = shape;
                        task.has_constructor = task.has_constructor or
                            self.inline_calls != .iterator_fusion or
                            isExactGeneratedIteratorType(self.pass.program, valueType(self.pass.program, task.values[index]));
                    },
                    .disproven, .unknown_budget_exhausted => {
                        task.shapes[index] = .{ .any = valueType(self.pass.program, task.values[index]) };
                    },
                }
                frame.index += 1;
            },
            LoopValueCursor.split_initials => {
                frame.index += 1;
                return try self.nextLoopSplitInitial(frame, task);
            },
            LoopValueCursor.split_body => {
                const body = input.?.get(.expr);
                const loop_frame = self.loop_stack.pop() orelse Common.invariant("loop stack underflow after split attempt");
                if (!loop_frame.any_demoted) {
                    try self.retargetLoopForwardConditions(task.new_initials.items, task.forward_sources.items, task.forward_finals.items);
                    const loop = try self.addExpr(.{ .ty = task.ty, .data = .{ .loop_ = .{
                        .params = try self.pass.program.addTypedLocalSpan(task.new_params.items),
                        .initial_values = try self.pass.program.addExprSpan(task.new_initials.items),
                        .body = body,
                    } } });
                    task.new_params.deinit(self.pass.allocator);
                    task.new_initials.deinit(self.pass.allocator);
                    task.attempt_lists_owned = false;
                    return self.finishLoopValue(task, .{ .expr = loop });
                }

                task.new_params.deinit(self.pass.allocator);
                task.new_initials.deinit(self.pass.allocator);
                task.attempt_lists_owned = false;
                self.subst.restore(task.split_start);
                self.rewindLoopAttempt(task.attempt_mark);
                // Back edges demoted their unsupplied leaves in place. Any slot that
                // still carries constructor structure is worth another split attempt.
                task.has_constructor = false;
                for (task.shapes) |shape| {
                    if (shape != .any) task.has_constructor = true;
                }
                return try self.nextLoopAttempt(frame, task);
            },
            LoopValueCursor.whole_initial => {
                task.initial_exprs[frame.index] = input.?.get(.expr);
                frame.index += 1;
                return try self.nextLoopWholeInitial(frame, task);
            },
            LoopValueCursor.whole_body => {
                const body = input.?.get(.expr);
                if (self.loop_stack.pop() == null) Common.invariant("loop stack underflow after whole-state body clone");
                return self.finishLoopValue(task, .{ .expr = try self.addExpr(.{ .ty = task.ty, .data = .{ .loop_ = .{
                    .params = try self.pass.program.addTypedLocalSpan(task.whole_params),
                    .initial_values = try self.pass.program.addExprSpan(task.initial_exprs),
                    .body = body,
                } } }) });
            },
            LoopValueCursor.whole_body + 1...std.math.maxInt(u8) => unreachable,
        }
        if (frame.index < task.initial_values.len) {
            return .{ .call = .{ .demanding = .{ .expr = task.initial_values[frame.index], .bindings = task.bindings } } };
        }
        self.subst.restore(task.forward_start);

        task.change_start = self.subst.watermark();

        // A loop-carried variable that was bound to a known constructor before the
        // loop leaves that value in the binder-wide substitution, keyed on its
        // source binder.
        // Every back edge reassigns the variable, so its pre-loop value is not
        // what the slot carries inside the loop. Reads sharing that binder (the
        // reassigned copies feeding `continue`) must resolve to the value the slot
        // actually holds, so drop those pre-loop values before cloning the body
        // and keep each slot's identity: the emitted params are installed under
        // it below, which is the only resolution path a reassigned copy has.
        //
        // That identity comes from the slot's initial value, which is only
        // sound when that initial local is the pre-loop version of the slot's
        // own variable. A variable initialized as a bare alias of another
        // in-scope variable (`var $last_break = cluster_start`) carries the
        // initializer's binder on its initial local instead; installing the
        // slot's per-iteration value under that binder would make body reads
        // of the initializer variable resolve to the loop-carried slot, which
        // diverges from it after the first back edge. The body referencing the
        // initial's exact local is the signature of that alias shape—a
        // consumed pre-loop version is never read again—so claim (and drop)
        // the carried binder only when the body does not.
        const contents = try self.pass.loopBodyContents(task.loop.body, task.initial_values);
        task.carried_identities = try arena.alloc(?BinderIdentity, task.initial_values.len);
        for (task.initial_values, task.carried_identities) |initial, *identity| {
            identity.* = null;
            const initial_local = localExpr(self.pass.program, initial) orelse continue;
            if (contents.references(initial_local)) continue;
            identity.* = try self.subst.dropCarriedBinder(self.pass.program, initial);
        }

        // Mark each carried binder so a state-merged or reassigned copy bound in
        // a nested `let` while cloning the body floats its value past that let's
        // restore, letting the back edge resolve it through its binder.
        for (task.carried_identities) |identity| {
            if (identity) |carried| try self.subst.markLoopCarried(carried);
        }

        // Splitting a slot into its shape leaves is only sound when every back
        // edge can hand those leaves back. Whether a back edge can is knowable
        // only while cloning the body: an advanced successor becomes a known
        // constructor value through step inlining and known-tag collapse, which
        // the source expressions do not show. So the split is decided by
        // attempt: substitute each carried slot with its entry shape's leaves,
        // clone the body, and let every back edge either supply the leaves or
        // demote the specific leaves it cannot supply. A demoted leaf becomes a
        // runtime scalar over its finite value set (e.g. an entry-known tag a
        // back edge flips to a sibling tag) while its sibling leaves stay split.
        // The failed clone is discarded and the attempt repeats. Each retry
        // erases at least one constructor leaf, so attempts are bounded by the
        // leaf count.
        //
        // Shape splitting is proved only by the local `continue` edges below.
        // A `return` exits the enclosing function outside that fixed point, so
        // a loop containing one must retain its whole runtime slots.
        if (task.has_constructor and contents.contains_return) task.has_constructor = false;
        return try self.nextLoopAttempt(frame, task);
    }

    fn nextLoopAttempt(self: *Cloner, frame: *CloneFrame, task: *LoopValueTask) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        if (task.has_constructor) {
            task.attempt_mark = self.markLoopAttempt();
            task.new_params = try arena.create(std.ArrayList(Ast.TypedLocal));
            task.new_params.* = .empty;
            task.new_initials = try arena.create(std.ArrayList(Ast.ExprId));
            task.new_initials.* = .empty;
            task.attempt_lists_owned = true;
            task.split_start = self.subst.watermark();
            task.forward_sources = .empty;
            task.forward_finals = .empty;
            frame.index = 0;
            return try self.startLoopSplitSlot(frame, task);
        }

        const whole_shapes = try arena.alloc(Shape, task.params.len);
        for (task.params, 0..) |param, index| whole_shapes[index] = .{ .any = param.ty };
        task.shapes = whole_shapes;
        task.initial_exprs = try arena.alloc(Ast.ExprId, task.values.len);
        frame.index = 0;
        return try self.nextLoopWholeInitial(frame, task);
    }

    /// Substitute one carried slot with its entry shape's leaves, then supply
    /// its initial value's leaves as the new initial values.
    fn startLoopSplitSlot(self: *Cloner, frame: *CloneFrame, task: *LoopValueTask) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        if (frame.index >= task.params.len) {
            try self.loop_stack.append(self.pass.allocator, .{
                .values = task.shapes,
                .any_demoted = false,
            });
            frame.cursor = LoopValueCursor.split_body;
            return .{ .call = .{ .loop_body = .{ .body = task.loop.body, .selection = task.exit_selection } } };
        }
        const index = frame.index;
        const param = task.params[index];
        const shape = task.shapes[index];
        const leaf_start = task.new_params.items.len;
        const param_value = try self.valueFromShapeArgs(shape, task.new_params);
        try self.subst.put(self.pass.program, param.local, param_value);
        if (task.carried_identities[index]) |identity| try self.subst.putLoopCarried(identity, param_value);
        // An `.any` slot keeps its whole value in one param, so a
        // forward reference to the source param means this param.
        if (shape == .any) {
            try task.forward_sources.append(arena, param.local);
            try task.forward_finals.append(arena, task.new_params.items[leaf_start].local);
        }
        frame.cursor = LoopValueCursor.split_initials;
        return .{ .call = .{ .append_exprs_from_value = .{ .shape = shape, .value = task.values[index], .out = task.new_initials } } };
    }

    fn nextLoopSplitInitial(self: *Cloner, frame: *CloneFrame, task: *LoopValueTask) Common.LowerError!CloneStep {
        return try self.startLoopSplitSlot(frame, task);
    }

    fn nextLoopWholeInitial(self: *Cloner, frame: *CloneFrame, task: *LoopValueTask) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        if (frame.index < task.values.len) {
            frame.cursor = LoopValueCursor.whole_initial;
            return .{ .call = .{ .materialize = .{ .value = task.values[frame.index] } } };
        }
        task.whole_params = try arena.alloc(Ast.TypedLocal, task.params.len);
        const forward_sources = try arena.alloc(Ast.LocalId, task.params.len);
        const forward_finals = try arena.alloc(Ast.LocalId, task.params.len);
        for (task.params, task.whole_params, forward_sources, forward_finals, task.carried_identities) |param, *whole, *source, *final, carried_identity| {
            whole.* = .{
                .local = try self.cloneBinder(param.local, param.ty, .bind_runtime),
                .ty = param.ty,
            };
            if (carried_identity) |identity| {
                // The exact-local entry `cloneBinder` just installed for this
                // param, not a binder-wide entry a sibling might hold.
                const param_value = self.subst.getExact(param.local) orelse
                    Common.invariant("carried whole-state param had no substitution after binding");
                try self.subst.putLoopCarried(identity, param_value);
            }
            source.* = param.local;
            final.* = whole.local;
        }
        try self.retargetLoopForwardConditions(task.initial_exprs, forward_sources, forward_finals);
        try self.loop_stack.append(self.pass.allocator, .{
            .values = task.shapes,
            .any_demoted = false,
        });
        frame.cursor = LoopValueCursor.whole_body;
        return .{ .call = .{ .loop_body = .{ .body = task.loop.body, .selection = task.exit_selection } } };
    }

    /// Clone each source statement once. Strict bindings can travel with the
    /// result's structure; retained statements keep that structure inside its
    /// block. Encountering a retained statement never restarts prefix cloning.
    const BlockValueTask = struct {
        ty: Type.TypeId,
        block: @FieldType(Ast.ExprData, "block"),
        bindings: *BindingChain,
        change_start: usize = 0,
        block_bindings: *BindingChain = undefined,
        statements: *std.ArrayList(Ast.StmtId) = undefined,
        statements_owned: bool = false,
        terminated: bool = false,
        stmt_context: ?SourceContext = null,
        case_value: ?Value = null,
        value: Value = undefined,
        pattern: Ast.PatId = undefined,
    };

    /// Cursor states of a block-value clone.
    const BlockValueCursor = struct {
        const start = 0;
        const let_value = 1;
        const let_bound = 2;
        const let_residual = 3;
        const let_continuation = 4;
        const recursive_stmt = 5;
        const expr_value = 6;
        const expr_reusable = 7;
        const stmt = 8;
        const final = 9;
        const finish = 10;
    };

    fn finishBlockValueTask(self: *Cloner, task: *BlockValueTask, value: Value) CloneStep {
        task.statements.deinit(self.pass.allocator);
        task.statements_owned = false;
        self.subst.restore(task.change_start);
        return retValue(value);
    }

    /// Leave the current statement's source context and move to the next
    /// statement.
    fn nextBlockStatement(self: *Cloner, frame: *CloneFrame, task: *BlockValueTask) void {
        if (task.stmt_context) |context| context.restore(self);
        task.stmt_context = null;
        frame.index += 1;
    }

    fn stepBlockValue(self: *Cloner, frame: *CloneFrame, task: *BlockValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const block = task.block;
        switch (frame.cursor) {
            BlockValueCursor.start => {
                if (self.purpose == .loop_exit_selection) {
                    return .{ .tail = .{ .exit_block_value = .{ .ty = task.ty, .block = block, .bindings = task.bindings } } };
                }
                task.change_start = self.subst.watermark();
                task.block_bindings = try self.newChain();
                task.statements = try self.arena.allocator().create(std.ArrayList(Ast.StmtId));
                task.statements.* = .empty;
                task.statements_owned = true;
                task.terminated = self.pass.program.getExpr(block.final_expr).data == .@"unreachable";
            },
            BlockValueCursor.let_value => {
                const value = input.?.get(.value);
                const let_ = self.pass.program.getStmt(GuardedList.at(self.pass.program.stmtSpan(block.statements), frame.index)).let_;
                if (self.caseExprFromValue(value) == null) {
                    task.value = value;
                    frame.cursor = BlockValueCursor.let_bound;
                    return .{ .call = .{ .bind_let_value = .{ .pat = let_.pat, .source_value = let_.value, .value = value, .bindings = task.block_bindings } } };
                }
                task.case_value = value;
                return try self.blockLetContinuation(frame, task);
            },
            BlockValueCursor.let_bound => {
                if (input.?.get(.flag)) {
                    self.nextBlockStatement(frame, task);
                } else {
                    // Keep an ordinary runtime destructure at its
                    // source position without turning the remaining
                    // statement span into nested let expressions.
                    const let_ = self.pass.program.getStmt(GuardedList.at(self.pass.program.stmtSpan(block.statements), frame.index)).let_;
                    task.pattern = try self.clonePat(let_.pat, .bind_runtime);
                    frame.cursor = BlockValueCursor.let_residual;
                    return .{ .call = .{ .materialize = .{ .value = task.value } } };
                }
            },
            BlockValueCursor.let_residual => {
                const let_ = self.pass.program.getStmt(GuardedList.at(self.pass.program.stmtSpan(block.statements), frame.index)).let_;
                const residual = try self.addStmt(.{ .let_ = .{
                    .pat = task.pattern,
                    .value = input.?.get(.expr),
                    .comptime_site = let_.comptime_site,
                } });
                try self.appendBindingStmts(task.block_bindings.*, task.statements);
                task.block_bindings.* = .{};
                try task.statements.append(self.pass.allocator, residual);
                self.nextBlockStatement(frame, task);
            },
            BlockValueCursor.let_continuation => {
                const value = input.?.get(.value);
                if (value != .expr and task.statements.items.len == 0 and !task.terminated) {
                    task.bindings.appendChain(task.block_bindings.*);
                    if (task.stmt_context) |context| context.restore(self);
                    task.stmt_context = null;
                    return self.finishBlockValueTask(task, value);
                }
                // The continuation's value is branch-built. The block
                // keeps it as its recorded tail so a case over this
                // block reads the arms' structure instead of one
                // opaque value.
                frame.cursor = BlockValueCursor.finish;
                return .{ .call = .{ .emit_block_with_tail = .{ .ty = task.ty, .statements = task.statements, .tail = .{
                    .reused = null,
                    .bindings = task.block_bindings.*,
                    .value = input.?.get(.value),
                } } } };
            },
            BlockValueCursor.recursive_stmt => {
                const cloned = input.?.get(.stmt);
                task.block_bindings.appendChain(cloned.bindings);
                const anchor = cloned.stmt orelse
                    Common.invariant("recursive statement dissolved while cloning a transparent block");
                try task.block_bindings.appendStatement(self.arena.allocator(), anchor);
                self.nextBlockStatement(frame, task);
            },
            BlockValueCursor.expr_value => {
                frame.cursor = BlockValueCursor.expr_reusable;
                return .{ .call = try self.makeReusableTask(input.?.get(.value), task.block_bindings) };
            },
            BlockValueCursor.expr_reusable => self.nextBlockStatement(frame, task),
            BlockValueCursor.stmt => {
                const cloned = input.?.get(.stmt);
                task.block_bindings.appendChain(cloned.bindings);
                try self.appendBindingStmts(task.block_bindings.*, task.statements);
                task.block_bindings.* = .{};
                if (cloned.stmt) |out| try task.statements.append(self.pass.allocator, out);
                self.nextBlockStatement(frame, task);
            },
            BlockValueCursor.final => {
                const value = input.?.get(.value);
                // An unreachable final marker is valid only inside the block whose
                // earlier statement terminates. It must never escape as a value.
                if (task.statements.items.len == 0 and !task.terminated) {
                    task.bindings.appendChain(task.block_bindings.*);
                    return self.finishBlockValueTask(task, value);
                }
                frame.cursor = BlockValueCursor.finish;
                return .{ .call = .{ .emit_block_with_tail = .{ .ty = task.ty, .statements = task.statements, .tail = .{
                    .reused = null,
                    .bindings = task.block_bindings.*,
                    .value = value,
                } } } };
            },
            BlockValueCursor.finish => {
                // The block's own emitted expression.
                if (task.stmt_context) |context| context.restore(self);
                task.stmt_context = null;
                return self.finishBlockValueTask(task, .{ .expr = input.?.get(.expr) });
            },
            BlockValueCursor.finish + 1...std.math.maxInt(u8) => unreachable,
        }
        return try self.nextBlockStatementStep(frame, task);
    }

    fn nextBlockStatementStep(self: *Cloner, frame: *CloneFrame, task: *BlockValueTask) Common.LowerError!CloneStep {
        const block = task.block;
        if (frame.index >= block.statements.len) {
            frame.cursor = BlockValueCursor.final;
            return .{ .call = .{ .expr_value = .{ .expr = block.final_expr, .bindings = task.block_bindings } } };
        }
        const stmt_id = GuardedList.at(self.pass.program.stmtSpan(block.statements), frame.index);
        task.stmt_context = try self.enterStmtSource(stmt_id);
        task.case_value = null;
        const stmt = self.pass.program.getStmt(stmt_id);
        switch (stmt) {
            .let_ => |let_| {
                if (!let_.recursive and !task.terminated) {
                    frame.cursor = BlockValueCursor.let_value;
                    return .{ .call = .{ .expr_value = .{ .expr = let_.value, .bindings = task.block_bindings } } };
                }
                if (try self.blockLetTakesContinuation(task, let_, frame.index)) {
                    return try self.blockLetContinuation(frame, task);
                }
                if (let_.recursive and task.statements.items.len == 0 and !task.terminated) {
                    // Recursive bindings remain runtime anchors even
                    // when the surrounding block exposes its tail value.
                    frame.cursor = BlockValueCursor.recursive_stmt;
                    return .{ .call = .{ .stmt = .{ .stmt = stmt_id } } };
                }
            },
            .expr => |stmt_expr| if (task.statements.items.len == 0 and !task.terminated) {
                frame.cursor = BlockValueCursor.expr_value;
                return .{ .call = .{ .expr_value = .{ .expr = stmt_expr, .bindings = task.block_bindings } } };
            },
            .uninitialized, .expect, .dbg, .return_, .crash, .checked_error => {},
        }
        frame.cursor = BlockValueCursor.stmt;
        return .{ .call = .{ .stmt = .{ .stmt = stmt_id } } };
    }

    /// Whether a non-recursive binding statement hands its value and the
    /// rest of the block to one shared continuation.
    fn blockLetTakesContinuation(self: *Cloner, task: *BlockValueTask, let_: anytype, index: usize) Common.LowerError!bool {
        const block = task.block;
        return !let_.recursive and
            (!task.terminated or
                (self.pass.program.getExpr(let_.value).data == .loop_ and
                    (try self.pass.tuplePatternIsPartiallyUsedInBlockTail(
                        let_.pat,
                        self.pass.program.stmtSpan(block.statements),
                        index + 1,
                        block.final_expr,
                    ) or
                        try self.pass.aggregateLoopBindingIsPartiallyUsedInBlockTail(
                            let_.pat,
                            let_.value,
                            self.pass.program.stmtSpan(block.statements),
                            index + 1,
                            block.final_expr,
                        ))));
    }

    /// The untouched suffix is source, not cloned output. Give let-of-case
    /// one shared continuation without copying or speculatively walking that
    /// suffix.
    fn blockLetContinuation(self: *Cloner, frame: *CloneFrame, task: *BlockValueTask) Common.LowerError!CloneStep {
        const block = task.block;
        const index = frame.index;
        const let_ = self.pass.program.getStmt(GuardedList.at(self.pass.program.stmtSpan(block.statements), index)).let_;
        const template_start = self.pass.program.exprCount();
        const tail = try self.pass.program.addExpr(.{ .ty = task.ty, .data = .{ .block = .{
            .statements = .{
                .start = block.statements.start + @as(u32, @intCast(index)) + 1,
                .len = block.statements.len - @as(u32, @intCast(index)) - 1,
            },
            .final_expr = block.final_expr,
        } } });
        try self.registerCloneTemplate(template_start);
        const continuation = LetParts{
            .bind = let_.pat,
            .value = let_.value,
            .rest = tail,
            .comptime_site = let_.comptime_site,
        };
        frame.cursor = BlockValueCursor.let_continuation;
        if (task.case_value) |cloned| {
            return .{ .call = .{ .let_with_value = .{ .let_ = continuation, .value = cloned, .bindings = task.block_bindings } } };
        }
        return .{ .call = .{ .let_value = .{ .let_ = continuation, .bindings = task.block_bindings } } };
    }

    /// Exit selection consumes its planned source continuation directly, rather
    /// than running the general let-of-case transformations over that suffix.
    const ExitBlockValueTask = struct {
        ty: Type.TypeId,
        block: @FieldType(Ast.ExprData, "block"),
        bindings: *BindingChain,
        change_start: usize = 0,
        block_bindings: *BindingChain = undefined,
        statements: *std.ArrayList(Ast.StmtId) = undefined,
        statements_owned: bool = false,
        final: ?Ast.ExprId = null,
        value: Value = undefined,
    };

    fn stepExitBlockValue(self: *Cloner, frame: *CloneFrame, task: *ExitBlockValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const block = task.block;
        switch (frame.cursor) {
            0 => {
                task.change_start = self.subst.watermark();
                task.block_bindings = try self.newChain();
                task.statements = try self.arena.allocator().create(std.ArrayList(Ast.StmtId));
                task.statements.* = .empty;
                task.statements_owned = true;
                frame.cursor = 1;
            },
            // A statement expression's discarded value.
            1 => frame.index += 1,
            // A planned loop selection consumed the rest of the block.
            2 => {
                task.final = input.?.get(.maybe_expr) orelse Common.invariant("planned loop selection was not consumed");
                return try self.finishExitBlock(frame, task, .{ .expr = task.final.? });
            },
            // A cloned statement.
            3 => {
                const cloned = input.?.get(.stmt);
                task.block_bindings.appendChain(cloned.bindings);
                if (cloned.stmt) |out| {
                    if (self.pass.program.getStmt(out) == .let_) {
                        try task.block_bindings.appendStatement(self.arena.allocator(), out);
                    } else {
                        try self.appendBindingStmts(task.block_bindings.*, task.statements);
                        task.block_bindings.* = .{};
                        try task.statements.append(self.pass.allocator, out);
                    }
                }
                frame.index += 1;
            },
            // The block's final value.
            4 => return try self.finishExitBlock(frame, task, input.?.get(.value)),
            else => {
                const materialized = input.?.get(.expr);
                const result = if (frame.cursor == 5)
                    try self.addExpr(.{ .ty = task.ty, .data = .{ .block = .{
                        .statements = try self.pass.program.addStmtSpan(task.statements.items),
                        .final_expr = materialized,
                    } } })
                else
                    try self.wrapBindings(task.block_bindings.*, materialized);
                task.statements.deinit(self.pass.allocator);
                task.statements_owned = false;
                self.subst.restore(task.change_start);
                return retValue(.{ .expr = result });
            },
        }
        while (frame.index < block.statements.len) {
            const index = frame.index;
            const stmt_id = GuardedList.at(self.pass.program.stmtSpan(block.statements), index);
            const stmt = self.pass.program.getStmt(stmt_id);
            if (stmt == .expr) {
                frame.cursor = 1;
                return .{ .call = .{ .demanding = .{ .expr = stmt.expr, .bindings = task.block_bindings } } };
            }
            if (stmt == .let_ and !stmt.let_.recursive) {
                if (self.exit_demands) |demands| {
                    if (demands.get(stmt.let_.pat) != null) {
                        // The remaining block is a template built from
                        // source parts for this clone to read as source.
                        const template_start = self.pass.program.exprCount();
                        const tail = try self.addExpr(.{ .ty = task.ty, .data = .{ .block = .{
                            .statements = .{
                                .start = block.statements.start + @as(u32, @intCast(index)) + 1,
                                .len = block.statements.len - @as(u32, @intCast(index)) - 1,
                            },
                            .final_expr = block.final_expr,
                        } } });
                        try self.registerCloneTemplate(template_start);
                        frame.cursor = 2;
                        return .{ .call = .{ .loop_exit_values = .{ .let_ = .{
                            .bind = stmt.let_.pat,
                            .value = stmt.let_.value,
                            .rest = tail,
                            .comptime_site = stmt.let_.comptime_site,
                        } } } };
                    }
                }
            }
            frame.cursor = 3;
            return .{ .call = .{ .stmt = .{ .stmt = stmt_id } } };
        }
        frame.cursor = 4;
        return .{ .call = .{ .expr_value = .{ .expr = block.final_expr, .bindings = task.block_bindings } } };
    }

    fn finishExitBlock(self: *Cloner, frame: *CloneFrame, task: *ExitBlockValueTask, value: Value) Common.LowerError!CloneStep {
        task.value = value;
        if (task.statements.items.len != 0) {
            try self.appendBindingStmts(task.block_bindings.*, task.statements);
            frame.cursor = 5;
            return .{ .call = .{ .materialize = .{ .value = value } } };
        }
        // A terminating statement owns its unreachable final marker. Keep
        // that block intact instead of exposing the marker as an ordinary value.
        if (task.final == null and self.pass.program.getExpr(task.block.final_expr).data == .@"unreachable") {
            frame.cursor = 6;
            return .{ .call = .{ .materialize = .{ .value = value } } };
        }
        task.bindings.appendChain(task.block_bindings.*);
        task.statements.deinit(self.pass.allocator);
        task.statements_owned = false;
        self.subst.restore(task.change_start);
        return retValue(value);
    }

    /// Emit a block from cloned statements and its cloned tail. The tail's
    /// strict chain joins the statements rather than nesting in a block of
    /// its own, so the recorded tail value stays in scope of the statements
    /// a rewrite keeps when it replaces the tail.
    const EmitBlockWithTailTask = struct {
        ty: Type.TypeId,
        statements: *std.ArrayList(Ast.StmtId),
        tail: ClonedParts,
    };

    fn stepEmitBlockWithTail(self: *Cloner, frame: *CloneFrame, task: *EmitBlockWithTailTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const final_expr = if (frame.cursor == 0) task.tail.reused orelse {
            try self.appendBindingStmts(task.tail.bindings, task.statements);
            frame.cursor = 1;
            return .{ .call = .{ .materialize = .{ .value = task.tail.value } } };
        } else input.?.get(.expr);
        const emitted = try self.addExpr(.{ .ty = task.ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(task.statements.items),
            .final_expr = final_expr,
        } } });
        try self.recordBlockTailValue(emitted, task.tail.value);
        return retExpr(emitted);
    }

    const ContinueTask = struct {
        ty: Type.TypeId,
        values: Ast.Span(Ast.ExprId),
        frame_count: usize = 0,
        source_values: []const Ast.ExprId = &.{},
        new_values: *std.ArrayList(Ast.ExprId) = undefined,
        new_values_owned: bool = false,
        chain: *BindingChain = undefined,
    };

    fn stepContinue(self: *Cloner, frame: *CloneFrame, task: *ContinueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        switch (frame.cursor) {
            0 => {
                task.frame_count = self.loop_stack.items.len;
                if (task.frame_count == 0) {
                    frame.cursor = 10;
                    return .{ .call = .{ .expr_span = .{ .span = task.values } } };
                }
                const loop = self.loop_stack.items[task.frame_count - 1];
                task.source_values = try GuardedList.dupe(arena, Ast.ExprId, self.pass.program.exprSpan(task.values));
                if (task.source_values.len != loop.values.len) {
                    Common.invariantFmt("continue value count differed from specialized loop pattern: continue has {d} values, {d} frames with innermost expecting {d}", .{ task.source_values.len, task.frame_count, loop.values.len });
                }
                task.new_values = try arena.create(std.ArrayList(Ast.ExprId));
                task.new_values.* = .empty;
                task.new_values_owned = true;
                task.chain = try self.newChain();
                frame.cursor = 1;
            },
            // A slot's back-edge value.
            1 => {
                const shape = self.loop_stack.items[task.frame_count - 1].values[frame.index];
                frame.cursor = 2;
                return .{ .call = .{ .supply_loop_slot = .{ .shape = shape, .value = input.?.get(.value), .out = task.new_values } } };
            },
            // A slot's supplied leaves.
            2 => {
                const supplied = input.?.get(.supplied);
                if (supplied.demoted) {
                    // This back edge could not supply some of the slot's entry-shape
                    // leaves. Record the per-leaf demotion so the split attempt
                    // carries those leaves as runtime scalars while their siblings
                    // stay split; the values emitted here belong to a clone the
                    // attempt discards and retries.
                    self.loop_stack.items[task.frame_count - 1].values[frame.index] = supplied.shape;
                    self.loop_stack.items[task.frame_count - 1].any_demoted = true;
                }
                frame.index += 1;
                frame.cursor = 1;
            },
            else => return retExpr(try self.addExpr(.{ .ty = task.ty, .data = .{ .continue_ = .{
                .values = input.?.get(.expr_span),
            } } })),
        }
        if (frame.index < task.source_values.len) {
            return .{ .call = .{ .expr_value = .{ .expr = task.source_values[frame.index], .bindings = task.chain } } };
        }
        const continued = try self.addExpr(.{ .ty = task.ty, .data = .{ .continue_ = .{
            .values = try self.pass.program.addExprSpan(task.new_values.items),
        } } });
        task.new_values.deinit(self.pass.allocator);
        task.new_values_owned = false;
        return retExpr(try self.wrapBindings(task.chain.*, continued));
    }

    const CallProcTask = struct {
        ty: Type.TypeId,
        call: @import("../monotype/ast.zig").CallProc,
        args: []const Ast.ExprId = &.{},
        values: []Value = &.{},
        analyzed: []ClonedValue = &.{},
        spec_pattern: ?CallPattern = null,
        spec_fn: Ast.FnId = undefined,
        out: *std.ArrayList(Ast.ExprId) = undefined,
        out_owned: bool = false,
        chain: *BindingChain = undefined,
        expr_span: Ast.Span(Ast.ExprId) = undefined,
    };

    /// Cursor states of a direct call clone.
    const CallProcCursor = struct {
        const start = 0;
        const plain_args = 1;
        const plain_captures = 2;
        const analyzed_arg = 3;
        const specialized_arg = 4;
        const specialized_captures = 5;
        const residual_arg = 6;
        const residual_captures = 7;
    };

    fn stepCallProc(self: *Cloner, frame: *CloneFrame, task: *CallProcTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const call = task.call;
        const arena = self.arena.allocator();
        switch (frame.cursor) {
            CallProcCursor.start => {
                const rewrites = if (Ast.localDirectCallee(call)) |callee|
                    !call.is_cold and self.rewrite_call_patterns and @intFromEnum(callee) < self.pass.plans.len
                else
                    false;
                if (!rewrites) {
                    frame.cursor = CallProcCursor.plain_args;
                    return .{ .call = .{ .expr_span = .{ .span = call.args } } };
                }
                task.args = try GuardedList.dupe(arena, Ast.ExprId, self.pass.program.exprSpan(call.args));
                task.values = try arena.alloc(Value, task.args.len);
                task.analyzed = try arena.alloc(ClonedValue, task.args.len);
                frame.cursor = CallProcCursor.analyzed_arg;
            },
            CallProcCursor.plain_args => {
                task.expr_span = input.?.get(.expr_span);
                frame.cursor = CallProcCursor.plain_captures;
                return .{ .call = .{ .capture_span = .{ .span = call.captures } } };
            },
            CallProcCursor.plain_captures => return retExpr(try self.addExpr(.{ .ty = task.ty, .data = .{ .call_proc = .{
                .callee = call.callee,
                .args = task.expr_span,
                .iterator_procedure = call.iterator_procedure,
                .captures = input.?.get(.capture_span),
                .is_cold = call.is_cold,
            } } })),
            CallProcCursor.analyzed_arg => {
                task.analyzed[frame.index] = input.?.get(.cloned);
                task.values[frame.index] = task.analyzed[frame.index].value;
                frame.index += 1;
            },
            CallProcCursor.specialized_arg => {
                frame.index += 1;
                return try self.nextSpecializedCallArg(frame, task);
            },
            CallProcCursor.specialized_captures => {
                const specialized = try self.addExpr(.{ .ty = task.ty, .data = .{ .call_proc = .{
                    .callee = .{ .lifted = task.spec_fn },
                    .args = task.expr_span,
                    .iterator_procedure = call.iterator_procedure,
                    .captures = input.?.get(.capture_span),
                    .is_cold = call.is_cold,
                } } });
                return retExpr(try self.wrapBindings(task.chain.*, specialized));
            },
            CallProcCursor.residual_arg => {
                task.out.items[frame.index] = input.?.get(.expr);
                frame.index += 1;
                return try self.nextResidualCallArg(frame, task);
            },
            CallProcCursor.residual_captures => {
                const residual = try self.addExpr(.{ .ty = task.ty, .data = .{ .call_proc = .{
                    .callee = call.callee,
                    .args = task.expr_span,
                    .iterator_procedure = call.iterator_procedure,
                    .captures = input.?.get(.capture_span),
                    .is_cold = call.is_cold,
                } } });
                return retExpr(try self.wrapBindings(task.chain.*, residual));
            },
            CallProcCursor.residual_captures + 1...std.math.maxInt(u8) => unreachable,
        }
        const raw = @intFromEnum(Ast.localDirectCallee(call).?);
        if (frame.index < task.args.len) {
            const callee_uses = self.pass.plans[raw].used_args;
            return .{ .call = .{ .expr_value_owned = .{ .expr = task.args[frame.index], .demand_shape = callee_uses[frame.index] } } };
        }
        try self.pass.ensureCallPatternForValues(Ast.localDirectCallee(call).?, task.values);

        // Every outcome below reads the argument values produced above
        // rather than cloning the source arguments again: a second clone
        // re-descends every argument, so a nested call chain (e.g. a long
        // `+` sum, or a chain of builder-method calls) would clone each
        // level twice and expand exponentially with depth. The reuse is
        // also required for correctness when producing values with binding
        // chains: those chains must be placed exactly once before the call.
        task.chain = try self.newChain();
        task.out = try arena.create(std.ArrayList(Ast.ExprId));
        task.out.* = .empty;
        task.out_owned = true;
        for (self.pass.plans[raw].specs.items) |spec| {
            if (!try callPatternMatchesValues(self.pass.program, spec.pattern, task.values)) continue;
            task.spec_pattern = spec.pattern;
            task.spec_fn = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before cloning calls");
            frame.index = 0;
            return try self.nextSpecializedCallArg(frame, task);
        }

        // No specialization matched, so the call stays residual.
        try task.out.resize(self.pass.allocator, task.values.len);
        frame.index = 0;
        return try self.nextResidualCallArg(frame, task);
    }

    fn nextSpecializedCallArg(self: *Cloner, frame: *CloneFrame, task: *CallProcTask) Common.LowerError!CloneStep {
        const pattern = task.spec_pattern.?;
        if (frame.index < pattern.args.len) {
            task.chain.appendChain(task.analyzed[frame.index].bindings);
            frame.cursor = CallProcCursor.specialized_arg;
            return .{ .call = .{ .append_exprs_from_value = .{ .shape = pattern.args[frame.index], .value = task.values[frame.index], .out = task.out } } };
        }
        task.expr_span = try self.pass.program.addExprSpan(task.out.items);
        task.out.deinit(self.pass.allocator);
        task.out_owned = false;
        frame.cursor = CallProcCursor.specialized_captures;
        return .{ .call = .{ .capture_span = .{ .span = task.call.captures } } };
    }

    fn nextResidualCallArg(self: *Cloner, frame: *CloneFrame, task: *CallProcTask) Common.LowerError!CloneStep {
        if (frame.index < task.values.len) {
            task.chain.appendChain(task.analyzed[frame.index].bindings);
            frame.cursor = CallProcCursor.residual_arg;
            return .{ .call = .{ .materialize = .{ .value = task.values[frame.index] } } };
        }
        task.expr_span = try self.pass.program.addExprSpan(task.out.items);
        task.out.deinit(self.pass.allocator);
        task.out_owned = false;
        frame.cursor = CallProcCursor.residual_captures;
        return .{ .call = .{ .capture_span = .{ .span = task.call.captures } } };
    }

    const AppendExprsTask = struct {
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    };

    fn stepAppendExprs(self: *Cloner, frame: *CloneFrame, task: *AppendExprsTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const structural_value = structuralValue(task.value);
        if (frame.cursor == 0) {
            frame.cursor = 1;
            switch (task.shape) {
                .any => return .{ .call = .{ .materialize = .{ .value = task.value } } },
                .tag => if (structural_value != .tag) Common.invariant("tag call pattern matched a non-tag value"),
                .record => |record| {
                    if (structural_value != .record) Common.invariant("record call pattern matched a non-record value");
                    for (record.fields, structural_value.record.fields) |field_shape, field| {
                        if (!self.pass.program.names.recordFieldLabelTextEql(field_shape.name, field.name)) Common.invariant("record call-pattern field order changed after matching");
                    }
                },
                .tuple => if (structural_value != .tuple) Common.invariant("tuple call pattern matched a non-tuple value"),
                .nominal => if (structural_value != .nominal) Common.invariant("nominal call pattern matched a non-nominal value"),
                .callable => if (structural_value != .callable) Common.invariant("callable call pattern matched a non-callable value"),
            }
        } else switch (task.shape) {
            .any => {
                try task.out.append(self.pass.allocator, input.?.get(.expr));
                return .{ .ret = .none };
            },
            .tag, .record, .tuple, .nominal, .callable => frame.index += 1,
        }
        const index = frame.index;
        const child: ?AppendExprsTask = switch (task.shape) {
            .any => unreachable,
            .tag => |tag| if (index < tag.payloads.len) .{ .shape = tag.payloads[index], .value = structural_value.tag.payloads[index], .out = task.out } else null,
            .record => |record| if (index < record.fields.len) .{ .shape = record.fields[index].shape, .value = structural_value.record.fields[index].value, .out = task.out } else null,
            .tuple => |tuple| if (index < tuple.items.len) .{ .shape = tuple.items[index], .value = structural_value.tuple.items[index], .out = task.out } else null,
            .nominal => |nominal| if (index < 1) .{ .shape = nominal.backing.*, .value = structural_value.nominal.backing.*, .out = task.out } else null,
            .callable => |callable| if (index < callable.captures.len) .{ .shape = callable.captures[index], .value = structural_value.callable.captures[index].value, .out = task.out } else null,
        };
        if (child) |next| return .{ .call = .{ .append_exprs_from_value = next } };
        return .{ .ret = .none };
    }

    /// Supply a loop slot's entry-shape leaves from a back edge's value,
    /// appending one expr per leaf to `out` in the order `valueFromShapeArgs`
    /// created the leaf params. Where the value structurally matches the shape,
    /// the split leaves are emitted directly (or read from an opaque expr via
    /// field access). Where a sub-path of the value cannot supply the shape's
    /// leaves—a back edge flipping an entry-known tag to a sibling tag, or a
    /// value that is not the shape's constructor—that sub-path demotes to
    /// `.any` and its whole value materializes as one runtime scalar over its
    /// finite value set, while its sibling leaves stay split. The returned
    /// shape carries the demotions; `demoted` is set when any leaf demoted.
    const SupplyLoopSlotTask = struct {
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
        mode: enum { append, materialize_any, demote, structural } = .structural,
        /// The opaque receiver whose fields or items supply the leaves.
        receiver: ?Ast.ExprId = null,
        shapes: []Shape = &.{},
        fields: []FieldShape = &.{},
        demoted: bool = false,
    };

    fn stepSupplyLoopSlot(self: *Cloner, frame: *CloneFrame, task: *SupplyLoopSlotTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const shape = task.shape;
        const value = task.value;
        if (frame.cursor == 0) {
            frame.cursor = 1;
            if (try shapeMatchesValue(self.pass.program, shape, value)) {
                task.mode = .append;
                return .{ .call = .{ .append_exprs_from_value = .{ .shape = shape, .value = value, .out = task.out } } };
            }
            switch (shape) {
                .any => {
                    task.mode = .materialize_any;
                    return .{ .call = .{ .materialize = .{ .value = value } } };
                },
                .tag => |tag| {
                    if (value != .tag) return self.demoteLoopSlotLeaf(task);
                    const value_tag = value.tag;
                    if (!self.pass.program.names.tagLabelTextEql(value_tag.name, tag.name) or
                        !sameType(self.pass.program, tag.ty, value_tag.ty) or
                        value_tag.payloads.len != tag.payloads.len)
                    {
                        return self.demoteLoopSlotLeaf(task);
                    }
                    task.shapes = try arena.alloc(Shape, tag.payloads.len);
                },
                .record => |record| switch (value) {
                    .record => |value_record| {
                        if (!sameType(self.pass.program, record.ty, value_record.ty) or
                            value_record.fields.len != record.fields.len)
                        {
                            return self.demoteLoopSlotLeaf(task);
                        }
                        task.fields = try arena.alloc(FieldShape, record.fields.len);
                    },
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return self.demoteLoopSlotLeaf(task);
                        task.receiver = receiver;
                        task.fields = try arena.alloc(FieldShape, record.fields.len);
                    },
                    .runtime_anchor, .static_data_candidate, .tag, .tuple, .nominal, .callable => return self.demoteLoopSlotLeaf(task),
                },
                .tuple => |tuple| switch (value) {
                    .tuple => |value_tuple| {
                        if (!sameType(self.pass.program, tuple.ty, value_tuple.ty) or
                            value_tuple.items.len != tuple.items.len)
                        {
                            return self.demoteLoopSlotLeaf(task);
                        }
                        task.shapes = try arena.alloc(Shape, tuple.items.len);
                    },
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return self.demoteLoopSlotLeaf(task);
                        task.receiver = receiver;
                        task.shapes = try arena.alloc(Shape, tuple.items.len);
                    },
                    .runtime_anchor, .static_data_candidate, .tag, .record, .nominal, .callable => return self.demoteLoopSlotLeaf(task),
                },
                .nominal => |nominal| switch (value) {
                    .nominal => |value_nominal| {
                        if (!sameType(self.pass.program, nominal.ty, value_nominal.ty)) return self.demoteLoopSlotLeaf(task);
                        task.shapes = try arena.alloc(Shape, 1);
                    },
                    .expr, .runtime_anchor, .static_data_candidate, .tag, .record, .tuple, .callable => return self.demoteLoopSlotLeaf(task),
                },
                .callable => |callable| {
                    if (value != .callable) return self.demoteLoopSlotLeaf(task);
                    const value_callable = value.callable;
                    if (!sameType(self.pass.program, callable.ty, value_callable.ty) or
                        !callableTargetMatches(self.pass.program, callable.fn_id, value_callable.fn_id) or
                        value_callable.captures.len != callable.captures.len)
                    {
                        return self.demoteLoopSlotLeaf(task);
                    }
                    task.shapes = try arena.alloc(Shape, callable.captures.len);
                },
            }
        } else switch (task.mode) {
            .append => return .{ .ret = .{ .supplied = .{ .shape = shape, .demoted = false } } },
            .materialize_any => {
                try task.out.append(self.pass.allocator, input.?.get(.expr));
                return .{ .ret = .{ .supplied = .{ .shape = shape, .demoted = false } } };
            },
            .demote => {
                try task.out.append(self.pass.allocator, input.?.get(.expr));
                return .{ .ret = .{ .supplied = .{ .shape = .{ .any = shapeType(shape) }, .demoted = true } } };
            },
            .structural => {
                const supplied = input.?.get(.supplied);
                switch (shape) {
                    .record => |record| task.fields[frame.index] = .{ .name = record.fields[frame.index].name, .shape = supplied.shape },
                    .tag, .tuple, .nominal, .callable => task.shapes[frame.index] = supplied.shape,
                    .any => unreachable,
                }
                task.demoted = task.demoted or supplied.demoted;
                frame.index += 1;
            },
        }
        const index = frame.index;
        switch (shape) {
            .any => unreachable,
            .tag => |tag| if (index < tag.payloads.len) {
                return .{ .call = .{ .supply_loop_slot = .{ .shape = tag.payloads[index], .value = value.tag.payloads[index], .out = task.out } } };
            } else return .{ .ret = .{ .supplied = .{ .shape = .{ .tag = .{ .ty = tag.ty, .name = tag.name, .payloads = task.shapes } }, .demoted = task.demoted } } },
            .record => |record| if (index < record.fields.len) {
                const field_shape = record.fields[index];
                if (task.receiver) |receiver| {
                    const field_expr = try self.addFieldAccessExpr(
                        shapeType(field_shape.shape),
                        receiver,
                        field_shape.name,
                    );
                    return .{ .call = .{ .supply_loop_slot = .{ .shape = field_shape.shape, .value = .{ .expr = field_expr }, .out = task.out } } };
                }
                const field_value = value.record.fields[index];
                if (!self.pass.program.names.recordFieldLabelTextEql(field_shape.name, field_value.name)) return self.demoteLoopSlotLeaf(task);
                return .{ .call = .{ .supply_loop_slot = .{ .shape = field_shape.shape, .value = field_value.value, .out = task.out } } };
            } else return .{ .ret = .{ .supplied = .{ .shape = .{ .record = .{ .ty = record.ty, .fields = task.fields } }, .demoted = task.demoted } } },
            .tuple => |tuple| if (index < tuple.items.len) {
                const item_shape = tuple.items[index];
                if (task.receiver) |receiver| {
                    const item_expr = try self.addExpr(.{ .ty = shapeType(item_shape), .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    return .{ .call = .{ .supply_loop_slot = .{ .shape = item_shape, .value = .{ .expr = item_expr }, .out = task.out } } };
                }
                return .{ .call = .{ .supply_loop_slot = .{ .shape = item_shape, .value = value.tuple.items[index], .out = task.out } } };
            } else return .{ .ret = .{ .supplied = .{ .shape = .{ .tuple = .{ .ty = tuple.ty, .items = task.shapes } }, .demoted = task.demoted } } },
            .nominal => |nominal| if (index < 1) {
                return .{ .call = .{ .supply_loop_slot = .{ .shape = nominal.backing.*, .value = value.nominal.backing.*, .out = task.out } } };
            } else {
                const backing = try arena.create(Shape);
                backing.* = task.shapes[0];
                return .{ .ret = .{ .supplied = .{ .shape = .{ .nominal = .{ .ty = nominal.ty, .backing = backing } }, .demoted = task.demoted } } };
            },
            .callable => |callable| if (index < callable.captures.len) {
                return .{ .call = .{ .supply_loop_slot = .{ .shape = callable.captures[index], .value = value.callable.captures[index].value, .out = task.out } } };
            } else return .{ .ret = .{ .supplied = .{ .shape = .{ .callable = .{ .ty = callable.ty, .fn_id = callable.fn_id, .captures = task.shapes } }, .demoted = task.demoted } } },
        }
    }

    /// Carry the whole slot value as one runtime scalar.
    fn demoteLoopSlotLeaf(_: *Cloner, task: *SupplyLoopSlotTask) CloneStep {
        task.mode = .demote;
        return .{ .call = .{ .materialize = .{ .value = task.value } } };
    }

    const FieldAccessValueTask = struct {
        original_expr: Ast.ExprId,
        ty: Type.TypeId,
        field: @FieldType(Ast.ExprData, "field_access"),
        bindings: *BindingChain,
        binding_mark: ?*BindingNode = null,
        consumed: u32 = 0,
    };

    fn stepFieldAccessValue(self: *Cloner, frame: *CloneFrame, task: *FieldAccessValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const field = task.field;
        switch (frame.cursor) {
            0 => {
                task.binding_mark = task.bindings.mark();
                frame.cursor = 1;
                return .{ .call = .{ .demanding = .{ .expr = field.receiver, .bindings = task.bindings } } };
            },
            1 => {
                const receiver = input.?.get(.value);
                if (field.segments.len == 0) Common.invariant("field access path had no segments");

                var prefix = receiver;
                var consumed: u32 = 0;
                while (consumed < field.segments.len) : (consumed += 1) {
                    const segment = self.pass.program.fieldAccessSegmentAt(field.segments, consumed);
                    prefix = fieldFromValue(self.pass.program, prefix, segment.field) orelse break;
                }
                if (consumed == field.segments.len) return retValue(prefix);

                if (consumed == 0 and
                    task.bindings.mark() == task.binding_mark and
                    valueRetainsExpr(prefix, field.receiver) and
                    self.canReuseOriginalExpr(task.original_expr))
                {
                    return retValue(.{ .expr = task.original_expr });
                }
                task.consumed = consumed;
                frame.cursor = 2;
                return .{ .call = .{ .materialize = .{ .value = prefix } } };
            },
            else => {
                const residual_segments: Ast.Span(Ast.FieldAccessSegment) = .{
                    .start = field.segments.start + task.consumed,
                    .len = field.segments.len - task.consumed,
                };
                return retValue(.{ .expr = try self.addExpr(.{ .ty = task.ty, .data = .{ .field_access = .{
                    .receiver = input.?.get(.expr),
                    .segments = residual_segments,
                } } }) });
            },
        }
    }

    const TupleAccessTask = struct {
        original_expr: Ast.ExprId,
        ty: Type.TypeId,
        access: @FieldType(Ast.ExprData, "tuple_access"),
        receiver: ClonedValue = undefined,
        reuses_item: bool = false,
    };

    fn stepTupleAccess(self: *Cloner, frame: *CloneFrame, task: *TupleAccessTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const access = task.access;
        switch (frame.cursor) {
            0 => {
                if (self.selectedTupleItem(access)) |item| return retExpr(item);
                frame.cursor = 1;
                return .{ .call = .{ .expr_value_owned = .{ .expr = access.tuple, .demand_shape = true } } };
            },
            1 => {
                task.receiver = input.?.get(.cloned);
                if (itemFromValue(task.receiver.value, access.elem_index)) |value| {
                    task.reuses_item = true;
                    frame.cursor = 2;
                    return .{ .call = .{ .materialize = .{ .value = value } } };
                }
                if (task.receiver.bindings.isEmpty() and
                    valueRetainsExpr(task.receiver.value, access.tuple) and
                    self.canReuseOriginalExpr(task.original_expr))
                {
                    return retExpr(task.original_expr);
                }
                frame.cursor = 2;
                return .{ .call = .{ .materialize = .{ .value = task.receiver.value } } };
            },
            else => {
                const materialized = input.?.get(.expr);
                if (task.reuses_item) return retExpr(try self.wrapBindings(task.receiver.bindings, materialized));
                const item = try self.addExpr(.{ .ty = task.ty, .data = .{ .tuple_access = .{
                    .tuple = materialized,
                    .elem_index = access.elem_index,
                } } });
                return retExpr(try self.wrapBindings(task.receiver.bindings, item));
            },
        }
    }

    const MatchTask = struct {
        ty: Type.TypeId,
        match: @import("../monotype/ast.zig").MatchExpr,
        scrutinee: ClonedValue = undefined,
        scrutinee_expr: Ast.ExprId = undefined,
        chain: *BindingChain = undefined,
        plain: bool = false,
    };

    fn stepMatch(self: *Cloner, frame: *CloneFrame, task: *MatchTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const match = task.match;
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = .{ .expr_value_owned = .{ .expr = match.scrutinee, .demand_shape = true } } };
            },
            1 => {
                task.scrutinee = input.?.get(.cloned);
                task.chain = try self.newChain();
                task.chain.* = task.scrutinee.bindings;
                if ((try self.knownConstructorSize(task.scrutinee.value)).exactValue() == null) {
                    // The scrutinee's measured size saturated the work budget: it is
                    // cyclic or too deep to materialize. Skip the known-match collapse
                    // and emit the residual match over a plain clone of the source
                    // scrutinee, finite by construction, rather than materializing a
                    // possibly self-referential value. The discarded clone owns its
                    // bindings, so the plain re-clone is the only emitted evaluation.
                    task.plain = true;
                    frame.cursor = 3;
                    return .{ .call = .{ .plain = .{ .expr = match.scrutinee } } };
                }
                frame.cursor = 2;
                return .{ .call = .{ .select_known_match = .{ .scrutinee = task.scrutinee.value, .branches = match.branches, .bindings = task.chain } } };
            },
            2 => {
                if (input.?.get(.maybe_value)) |value| {
                    frame.cursor = 5;
                    return .{ .call = .{ .materialize = .{ .value = value } } };
                }
                frame.cursor = 3;
                return .{ .call = .{ .materialize = .{ .value = task.scrutinee.value } } };
            },
            3 => {
                task.scrutinee_expr = input.?.get(.expr);
                frame.cursor = 4;
                return .{ .call = .{ .branch_span = .{ .span = match.branches } } };
            },
            4 => {
                const branches = input.?.get(.branches);
                const residual = try self.addExpr(.{ .ty = task.ty, .data = .{ .match_ = .{
                    .scrutinee = task.scrutinee_expr,
                    .branches = branches.span,
                    .comptime_site = match.comptime_site,
                } } });
                try self.recordArmValues(residual, branches.values);
                if (task.plain) return retExpr(residual);
                return retExpr(try self.wrapBindings(task.chain.*, residual));
            },
            else => return retExpr(try self.wrapBindings(task.chain.*, input.?.get(.expr))),
        }
    }

    /// Collapse a match whose scrutinee is a known constructor to the selected
    /// branch's body. A value no branch matches leaves the match
    /// materialized: case-of-case distribution *offers* a value a branch may
    /// not structurally cover (an opaque tag payload the selection cannot
    /// verify), and a match checking could not prove exhaustive has no branch
    /// for some constructors, so the match's own failure path (its
    /// compile-time site's exhaustiveness failure, or a runtime error) is
    /// what that value reaches. An empty branch set is absurd elimination. A symbolic
    /// structural value does not prove reachability because an eager child may
    /// itself be an impossible expression, so that match must also remain.
    const SelectKnownMatchTask = struct {
        scrutinee: Value,
        branches: Ast.Span(Ast.Branch),
        bindings: *BindingChain,
        change_start: usize = 0,
    };

    fn stepSelectKnownMatch(self: *Cloner, frame: *CloneFrame, task: *SelectKnownMatchTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const branches_span = task.branches;
        switch (frame.cursor) {
            0 => {
                if (task.scrutinee == .expr) return .{ .ret = .{ .maybe_value = null } };
                if (branches_span.len == 0) return .{ .ret = .{ .maybe_value = null } };
                // Read each branch by stable index rather than holding a `branchSpan`
                // borrow: cloning a branch body can append to `branches` through a
                // nested match, which would invalidate a live borrow.
                while (frame.index < branches_span.len) : (frame.index += 1) {
                    const branch = self.pass.program.branchAt(branches_span, frame.index);
                    const match_change_start = self.subst.watermark();
                    const verdict = try self.bindPatToValue(branch.pat, task.scrutinee);
                    self.subst.restore(match_change_start);
                    switch (verdict) {
                        // This branch can be neither ruled in nor ruled out
                        // statically, so the whole fold aborts and the residual
                        // match decides at runtime.
                        .unknown, .unknown_budget_exhausted => return .{ .ret = .{ .maybe_value = null } },
                        .no_match => continue,
                        .match => {},
                    }
                    if (branch.guard != null or branch.bindings.len != 0) return .{ .ret = .{ .maybe_value = null } };

                    task.change_start = self.subst.watermark();
                    frame.cursor = 1;
                    return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = branch.pat, .value = task.scrutinee, .body = branch.body, .bindings = task.bindings } } };
                }
                return .{ .ret = .{ .maybe_value = null } };
            },
            1 => {
                if (input.?.get(.maybe_value) == null) {
                    Common.invariant("known constructor match changed after reusable payload binding");
                }
                const branch = self.pass.program.branchAt(branches_span, frame.index);
                frame.cursor = 2;
                return .{ .call = .{ .expr_value = .{ .expr = branch.body, .bindings = task.bindings } } };
            },
            else => {
                const body = input.?.get(.value);
                self.subst.restore(task.change_start);
                return .{ .ret = .{ .maybe_value = body } };
            },
        }
    }

    const BindPatToMatchValueTask = struct {
        pat: Ast.PatId,
        value: Value,
        body: Ast.ExprId,
        bindings: *BindingChain,
        /// How this frame finishes with its child's result.
        mode: enum {
            /// The child's value is bound to `pat`'s local.
            bind_local,
            /// The child's value is the result.
            forward,
            /// The child's reuse-safe value is the result.
            reusable,
            /// The child's value is `pat`'s `as` base structure; the anchor
            /// or base is bound to the `as` local.
            as_anchor,
            as_base,
            /// A runtime anchor's structure, re-anchored.
            anchored,
            /// A static data candidate's structure, rebuilt as a candidate.
            static_candidate,
            /// One field, item, or payload of a structured value.
            children,
            /// A field or item of an opaque readable receiver; the value
            /// itself is the result.
            receiver_children,
            /// A nominal backing, rebuilt under the nominal.
            nominal,
            /// A stripped nominal or static-data wrapper.
            stripped,
        } = .forward,
        base: Value = undefined,
        values: []Value = &.{},
        fields: []FieldValue = &.{},
    };

    fn stepBindPatToMatchValue(self: *Cloner, frame: *CloneFrame, task: *BindPatToMatchValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const pat = self.pass.program.getPat(task.pat);
        const value = task.value;
        const bindings = task.bindings;
        if (frame.cursor != 0) {
            const child = input.?;
            switch (task.mode) {
                .bind_local => {
                    const prepared = child.get(.value);
                    try self.subst.put(self.pass.program, pat.data.bind, prepared);
                    return .{ .ret = .{ .maybe_value = prepared } };
                },
                .forward => return .{ .ret = child },
                .reusable => return .{ .ret = .{ .maybe_value = child.get(.value) } },
                .as_anchor, .as_base => {
                    if (frame.cursor == 1) {
                        // The base value for the inner pattern.
                        task.base = child.get(.value);
                        frame.cursor = 2;
                        if (task.base == .runtime_anchor) {
                            task.mode = .as_anchor;
                            return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = pat.data.as.pattern, .value = task.base.runtime_anchor.structure.*, .body = task.body, .bindings = bindings } } };
                        }
                        task.mode = .as_base;
                        return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = pat.data.as.pattern, .value = task.base, .body = task.body, .bindings = bindings } } };
                    }
                    const inner = child.get(.maybe_value) orelse return .{ .ret = .{ .maybe_value = null } };
                    const prepared = if (task.mode == .as_anchor)
                        try self.runtimeAnchoredValue(inner, task.base.runtime_anchor.runtime)
                    else
                        inner;
                    try self.subst.put(self.pass.program, pat.data.as.local, prepared);
                    return .{ .ret = .{ .maybe_value = prepared } };
                },
                .anchored => {
                    const prepared = child.get(.maybe_value) orelse return .{ .ret = .{ .maybe_value = null } };
                    return .{ .ret = .{ .maybe_value = try self.runtimeAnchoredValue(prepared, value.runtime_anchor.runtime) } };
                },
                .static_candidate => {
                    self.wrapper_strip_depth -= 1;
                    const prepared_structure = child.get(.maybe_value) orelse return .{ .ret = .{ .maybe_value = null } };
                    const structure = try arena.create(Value);
                    structure.* = prepared_structure;
                    var prepared = value.static_data_candidate;
                    prepared.structure = structure;
                    return .{ .ret = .{ .maybe_value = Value{ .static_data_candidate = prepared } } };
                },
                .stripped => {
                    self.wrapper_strip_depth -= 1;
                    return .{ .ret = child };
                },
                .nominal => {
                    self.wrapper_strip_depth -= 1;
                    const inner = child.get(.maybe_value) orelse return .{ .ret = .{ .maybe_value = null } };
                    const backing = try arena.create(Value);
                    backing.* = inner;
                    return .{ .ret = .{ .maybe_value = Value{ .nominal = .{
                        .ty = structuralValue(value).nominal.ty,
                        .backing = backing,
                    } } } };
                },
                .children => {
                    const index = frame.index;
                    switch (pat.data) {
                        .record => {
                            const record = value.record;
                            const prepared = switch (child) {
                                .maybe_value => |maybe| maybe orelse return .{ .ret = .{ .maybe_value = null } },
                                .value => |reusable| reusable,
                                .none, .expr, .maybe_expr, .data, .maybe_data, .parts, .arm, .maybe_arm, .cloned, .flag, .stmt, .expr_span, .stmt_span, .capture_span, .field_span, .branches, .if_branches, .supplied, .pieces => Common.invariant("record match field received the wrong result kind"),
                            };
                            task.fields[index] = .{ .name = record.fields[index].name, .value = prepared };
                        },
                        .tuple, .tag => task.values[index] = child.get(.maybe_value) orelse return .{ .ret = .{ .maybe_value = null } },
                        .bind, .wildcard, .as, .list, .nominal, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => unreachable,
                    }
                    frame.index += 1;
                },
                .receiver_children => {
                    _ = child.get(.maybe_value) orelse return .{ .ret = .{ .maybe_value = null } };
                    frame.index += 1;
                },
            }
            return try self.nextMatchValueChild(frame, task);
        }
        frame.cursor = 1;
        switch (pat.data) {
            .bind => {
                task.mode = .bind_local;
                return .{ .call = .{ .local_value = .{ .value = value, .bindings = bindings } } };
            },
            .wildcard => {
                task.mode = .reusable;
                return .{ .call = try self.makeReusableTask(value, bindings) };
            },
            .as => {
                task.mode = .as_base;
                if (try self.valueCanSubstitute(value) == .proven) {
                    // The base is the value itself.
                    return self.stepBindPatToMatchValue(frame, task, .{ .value = value });
                }
                return .{ .call = try self.makeReusableTask(value, bindings) };
            },
            .record => switch (value) {
                .runtime_anchor => |anchor| {
                    task.mode = .anchored;
                    return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = task.pat, .value = anchor.structure.*, .body = task.body, .bindings = bindings } } };
                },
                .static_data_candidate => |candidate| return self.bindStaticCandidateToMatchValue(task, candidate),
                .record => |record| {
                    task.mode = .children;
                    task.fields = try arena.alloc(FieldValue, record.fields.len);
                    return try self.nextMatchValueChild(frame, task);
                },
                .nominal => |nominal| return self.bindStrippedToMatchValue(task, task.pat, nominal.backing.*),
                .expr => |receiver| {
                    if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .{ .ret = .{ .maybe_value = null } };
                    task.mode = .receiver_children;
                    return try self.nextMatchValueChild(frame, task);
                },
                .tag, .tuple, .callable => return .{ .ret = .{ .maybe_value = null } },
            },
            .tuple => |items_span| switch (value) {
                .runtime_anchor => |anchor| {
                    task.mode = .anchored;
                    return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = task.pat, .value = anchor.structure.*, .body = task.body, .bindings = bindings } } };
                },
                .static_data_candidate => |candidate| return self.bindStaticCandidateToMatchValue(task, candidate),
                .tuple => |tuple| {
                    if (self.pass.program.patSpan(items_span).len != tuple.items.len) return .{ .ret = .{ .maybe_value = null } };
                    task.mode = .children;
                    task.values = try arena.alloc(Value, tuple.items.len);
                    return try self.nextMatchValueChild(frame, task);
                },
                .nominal => |nominal| return self.bindStrippedToMatchValue(task, task.pat, nominal.backing.*),
                .expr => |receiver| {
                    if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .{ .ret = .{ .maybe_value = null } };
                    task.mode = .receiver_children;
                    return try self.nextMatchValueChild(frame, task);
                },
                .tag, .record, .callable => return .{ .ret = .{ .maybe_value = null } },
            },
            .tag => |tag_pat| {
                if (value == .static_data_candidate) return self.bindStaticCandidateToMatchValue(task, value.static_data_candidate);
                const tag = tagFromValue(value) orelse return .{ .ret = .{ .maybe_value = null } };
                if (!self.pass.program.names.tagLabelTextEql(tag.name, tag_pat.name)) return .{ .ret = .{ .maybe_value = null } };
                if (self.pass.program.patSpan(tag_pat.payloads).len != tag.payloads.len) return .{ .ret = .{ .maybe_value = null } };
                task.mode = .children;
                task.value = .{ .tag = tag };
                task.values = try arena.alloc(Value, tag.payloads.len);
                return try self.nextMatchValueChild(frame, task);
            },
            .nominal => |backing_pat| {
                if (value == .static_data_candidate) return self.bindStaticCandidateToMatchValue(task, value.static_data_candidate);
                const structure = structuralValue(value);
                if (structure != .nominal) return .{ .ret = .{ .maybe_value = null } };
                // Recurse into a nominal backing while binding a known match
                // value, counting the pointer-edge strip so a value that
                // references itself through those edges cannot loop forever.
                // The caller's static probe (`bindPatToValue` in the known
                // match selection) already declines the collapse for such a
                // value, so reaching the cap here is not expected; declining
                // the reuse binding is conservative.
                if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return .{ .ret = .{ .maybe_value = null } };
                self.wrapper_strip_depth += 1;
                task.mode = .nominal;
                return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = backing_pat, .value = structure.nominal.backing.*, .body = task.body, .bindings = bindings } } };
            },
            // List patterns are not statically destructured during
            // specialization; use the runtime match.
            .list,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .str_pattern,
            => return .{ .ret = .{ .maybe_value = null } },
        }
    }

    fn bindStaticCandidateToMatchValue(self: *Cloner, task: *BindPatToMatchValueTask, candidate: StaticDataCandidateValue) CloneStep {
        return self.bindStrippedToMatchValueAs(task, task.pat, candidate.structure.*, .static_candidate);
    }

    fn bindStrippedToMatchValue(self: *Cloner, task: *BindPatToMatchValueTask, pat: Ast.PatId, value: Value) CloneStep {
        return self.bindStrippedToMatchValueAs(task, pat, value, .stripped);
    }

    /// Bind through a stripped nominal or static-data wrapper, counting the
    /// strip; a value at the cap declines the reuse binding.
    fn bindStrippedToMatchValueAs(
        self: *Cloner,
        task: *BindPatToMatchValueTask,
        pat: Ast.PatId,
        value: Value,
        mode: @FieldType(BindPatToMatchValueTask, "mode"),
    ) CloneStep {
        if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return .{ .ret = .{ .maybe_value = null } };
        self.wrapper_strip_depth += 1;
        task.mode = mode;
        return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = pat, .value = value, .body = task.body, .bindings = task.bindings } } };
    }

    fn nextMatchValueChild(self: *Cloner, frame: *CloneFrame, task: *BindPatToMatchValueTask) Common.LowerError!CloneStep {
        const pat = self.pass.program.getPat(task.pat);
        const index = frame.index;
        const bindings = task.bindings;
        switch (pat.data) {
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                if (task.mode == .receiver_children) {
                    if (index >= fields.len) return .{ .ret = .{ .maybe_value = task.value } };
                    const field = GuardedList.at(fields, index);
                    const field_ty = self.pass.program.getPat(field.pattern).ty;
                    const field_expr = try self.addFieldAccessExpr(field_ty, task.value.expr, field.name);
                    return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = field.pattern, .value = .{ .expr = field_expr }, .body = task.body, .bindings = bindings } } };
                }
                const record = task.value.record;
                if (index >= record.fields.len) {
                    return .{ .ret = .{ .maybe_value = Value{ .record = .{
                        .ty = record.ty,
                        .fields = task.fields,
                    } } } };
                }
                const field = record.fields[index];
                if (recordPatField(self.pass.program, fields, field.name)) |field_pat| {
                    return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = field_pat, .value = field.value, .body = task.body, .bindings = bindings } } };
                }
                return .{ .call = try self.makeReusableTask(field.value, bindings) };
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                if (task.mode == .receiver_children) {
                    if (index >= pats.len) return .{ .ret = .{ .maybe_value = task.value } };
                    const child_pat = GuardedList.at(pats, index);
                    const item_ty = self.pass.program.getPat(child_pat).ty;
                    const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                        .tuple = task.value.expr,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = child_pat, .value = .{ .expr = item_expr }, .body = task.body, .bindings = bindings } } };
                }
                const tuple = task.value.tuple;
                if (index >= pats.len) {
                    return .{ .ret = .{ .maybe_value = Value{ .tuple = .{
                        .ty = tuple.ty,
                        .items = task.values,
                    } } } };
                }
                return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = GuardedList.at(pats, index), .value = tuple.items[index], .body = task.body, .bindings = bindings } } };
            },
            .tag => |tag_pat| {
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                const tag = task.value.tag;
                if (index >= pats.len) {
                    return .{ .ret = .{ .maybe_value = Value{ .tag = .{
                        .ty = tag.ty,
                        .name = tag.name,
                        .payloads = task.values,
                    } } } };
                }
                return .{ .call = .{ .bind_pat_to_match_value = .{ .pat = GuardedList.at(pats, index), .value = tag.payloads[index], .body = task.body, .bindings = bindings } } };
            },
            .bind, .wildcard, .as, .list, .nominal, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => unreachable,
        }
    }

    /// Explicit code-growth ceiling above which a known constructor value bound
    /// to an inlined or matched local is named once instead of expanded at each
    /// use. A
    /// statically constructed adapter chain is tens of nodes; a
    /// recursively-constructed chain wrapped a runtime number of times has no
    /// static depth, so its fixpoint known value instead fills the shape work
    /// budget and reaches thousands of nodes. Substituting that value shares it
    /// into every use, where each level of specialization re-walks and
    /// re-inlines the whole thing, and the total never settles. A value this
    /// large would let each use independently expand the same graph. Naming it
    /// once is the ordinary exact lowering and bounds generated code; it is not
    /// used as structural evidence. See design.md "Core Principles" on proof
    /// exhaustion versus code-growth admission.
    const known_value_expansion_limit: usize = 512;

    /// The value a matched or inlined local is bound to: the value itself
    /// when it can substitute, a reuse-safe rebinding otherwise, and a named
    /// materialization when it is too large to expand at each use.
    const LocalValueTask = struct {
        value: Value,
        bindings: *BindingChain,
    };

    fn stepLocalValue(self: *Cloner, frame: *CloneFrame, task: *LocalValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 1) {
            return .{ .tail = try self.makeReusableTask(.{ .expr = input.?.get(.expr) }, task.bindings) };
        }
        switch ((try self.knownConstructorSize(task.value)).admitExpansion(known_value_expansion_limit)) {
            .admitted => {},
            .denied_growth_limit, .denied_unknown_measure => {
                // Materialize the known value once and bind it reuse-safely,
                // so it is no longer tracked as a known constructor at its
                // use sites.
                frame.cursor = 1;
                return .{ .call = .{ .materialize = .{ .value = task.value } } };
            },
        }
        if (try self.valueCanSubstitute(task.value) == .proven) return retValue(task.value);
        return .{ .tail = try self.makeReusableTask(task.value, task.bindings) };
    }

    /// Clone a source expression to a known value for inlining, rebinding a
    /// value whose measured constructor size saturated the work budget through
    /// a plain clone of the source expression instead. A saturated size means
    /// the value is cyclic or too deep to measure; boxing it would
    /// deep-materialize a possibly self-referential value, whereas a plain
    /// clone of the source expression is finite by construction. The first
    /// clone owns its bindings, so declining it discards that entire chain and
    /// cannot duplicate any computation when the source is cloned plainly.
    const InlineValueTask = struct {
        expr: Ast.ExprId,
        demand_shape: bool,
        bindings: *BindingChain,
    };

    fn stepInlineValue(self: *Cloner, frame: *CloneFrame, task: *InlineValueTask, input: ?CloneResult) Common.LowerError!CloneStep {
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = .{ .expr_value_owned = .{ .expr = task.expr, .demand_shape = task.demand_shape } } };
            },
            1 => {
                const cloned = input.?.get(.cloned);
                if ((try self.knownConstructorSize(cloned.value)).exactValue() == null) {
                    frame.cursor = 2;
                    return .{ .call = .{ .plain = .{ .expr = task.expr } } };
                }
                task.bindings.appendChain(cloned.bindings);
                return retValue(cloned.value);
            },
            else => return .{ .tail = try self.makeReusableTask(.{ .expr = input.?.get(.expr) }, task.bindings) },
        }
    }

    /// Distribute an outer match over an emitted `match` or `if` scrutinee so
    /// the outer arms land where each inner arm's constructor is known. The
    /// inner arms are read through the values recorded when the scrutinee was
    /// emitted (`arm_values`); an emitted scrutinee without recorded values is
    /// opaque. A scrutinee that is a source expression reused unchanged by
    /// this clone has no recorded values and is re-read as source, which is
    /// the same read that produced it.
    const CaseOfCaseTask = struct {
        ty: Type.TypeId,
        scrutinee_expr: Ast.ExprId,
        outer_branches: Ast.Span(Ast.Branch),
        recorded: ?[]const Value = null,
        match_branches: []const Ast.Branch = &.{},
        match_rewritten: []Ast.Branch = &.{},
        if_branches: []const Ast.IfBranch = &.{},
        if_rewritten: []Ast.IfBranch = &.{},
        values: []Value = &.{},
    };

    fn stepCaseOfCase(self: *Cloner, frame: *CloneFrame, task: *CaseOfCaseTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const scrutinee_data = self.pass.program.getExpr(task.scrutinee_expr).data;
        if (frame.cursor == 0) {
            const outer_branches = self.pass.program.branchSpan(task.outer_branches);
            for (0..outer_branches.len) |branch_index| {
                const branch = GuardedList.at(outer_branches, branch_index);
                if (branch.guard != null or branch.bindings.len != 0) return .{ .ret = .{ .maybe_value = null } };
            }

            const branch_work = switch (scrutinee_data) {
                .match_ => |inner_match| self.pass.program.branchSpan(inner_match.branches).len,
                .if_ => |inner_if| self.pass.program.ifBranchSpan(inner_if.branches).len + 1,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return .{ .ret = .{ .maybe_value = null } },
            };
            task.recorded = self.arm_values.get(task.scrutinee_expr);
            if (task.recorded == null and @intFromEnum(task.scrutinee_expr) >= self.output_start) return .{ .ret = .{ .maybe_value = null } };
            if (self.case_of_case_growth.admit(@max(branch_work, 1)) != .admitted) return .{ .ret = .{ .maybe_value = null } };

            switch (scrutinee_data) {
                .match_ => |inner_match| {
                    task.match_branches = try GuardedList.dupe(arena, Ast.Branch, self.pass.program.branchSpan(inner_match.branches));
                    task.match_rewritten = try arena.alloc(Ast.Branch, task.match_branches.len);
                    task.values = try arena.alloc(Value, task.match_branches.len);
                },
                .if_ => |inner_if| {
                    task.if_branches = try GuardedList.dupe(arena, Ast.IfBranch, self.pass.program.ifBranchSpan(inner_if.branches));
                    task.if_rewritten = try arena.alloc(Ast.IfBranch, task.if_branches.len);
                    task.values = try arena.alloc(Value, task.if_branches.len + 1);
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
            }
            frame.cursor = 1;
        } else {
            const arm = input.?.get(.maybe_arm) orelse return .{ .ret = .{ .maybe_value = null } };
            const index = frame.index;
            switch (scrutinee_data) {
                .match_ => {
                    const inner_branch = task.match_branches[index];
                    task.match_rewritten[index] = .{
                        .pat = inner_branch.pat,
                        .bindings = inner_branch.bindings,
                        .guard = inner_branch.guard,
                        .body = arm.body,
                    };
                },
                .if_ => if (index < task.if_branches.len) {
                    task.if_rewritten[index] = .{
                        .cond = task.if_branches[index].cond,
                        .body = arm.body,
                    };
                } else {
                    task.values[index] = arm.value;
                    const distributed = try self.addExpr(.{ .ty = task.ty, .data = .{ .if_ = .{
                        .branches = try self.pass.program.addIfBranchSpan(task.if_rewritten),
                        .final_else = arm.body,
                    } } });
                    try self.recordArmValues(distributed, task.values);
                    return .{ .ret = .{ .maybe_value = .{ .expr = distributed } } };
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
            }
            task.values[index] = arm.value;
            frame.index += 1;
        }
        const index = frame.index;
        switch (scrutinee_data) {
            .match_ => |inner_match| {
                if (index < task.match_branches.len) {
                    const recorded_value: ?Value = if (task.recorded) |arm_values| arm_values[index] else null;
                    const inner_branch = task.match_branches[index];
                    return .{ .call = .{ .distribute_arm = .{
                        .ty = task.ty,
                        .arm_body = inner_branch.body,
                        .recorded_value = recorded_value,
                        .source_scope = .{ .pat = inner_branch.pat, .bindings = inner_branch.bindings },
                        .outer_branches = task.outer_branches,
                    } } };
                }
                const distributed = try self.addExpr(.{ .ty = task.ty, .data = .{ .match_ = .{
                    .scrutinee = inner_match.scrutinee,
                    .branches = try self.pass.program.addBranchSpan(task.match_rewritten),
                    .comptime_site = inner_match.comptime_site,
                } } });
                try self.recordArmValues(distributed, task.values);
                return .{ .ret = .{ .maybe_value = .{ .expr = distributed } } };
            },
            .if_ => |inner_if| return .{ .call = .{ .distribute_arm = .{
                .ty = task.ty,
                .arm_body = if (index < task.if_branches.len) task.if_branches[index].body else inner_if.final_else,
                .recorded_value = if (task.recorded) |arm_values| arm_values[index] else null,
                .source_scope = null,
                .outer_branches = task.outer_branches,
            } } },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
        }
    }

    /// The pattern scope of a source match arm re-read during distribution.
    const SourceArmScope = struct {
        pat: Ast.PatId,
        bindings: Ast.Span(Ast.StmtId),
    };

    /// Distribute the outer match over one inner arm. `recorded_value` is the
    /// arm's result value recorded when the arm was emitted; the emitted arm
    /// keeps its statements as they stand and only its tail changes, so no
    /// emitted expression is ever re-derived. Only a source arm reused
    /// unchanged by this clone has no recorded value, and re-reading it is
    /// reading source.
    const DistributeArmTask = struct {
        ty: Type.TypeId,
        arm_body: Ast.ExprId,
        recorded_value: ?Value,
        source_scope: ?SourceArmScope,
        outer_branches: Ast.Span(Ast.Branch),
        chain: *BindingChain = undefined,
        change_start: usize = 0,
        outer_value: Value = undefined,
        statements: *std.ArrayList(Ast.StmtId) = undefined,
        statements_owned: bool = false,
    };

    fn stepDistributeArm(self: *Cloner, frame: *CloneFrame, task: *DistributeArmTask, input: ?CloneResult) Common.LowerError!CloneStep {
        switch (frame.cursor) {
            0 => {
                task.chain = try self.newChain();
                task.change_start = self.subst.watermark();
                if (task.recorded_value) |recorded| {
                    frame.cursor = 2;
                    return .{ .call = .{ .distribute_value = .{
                        .ty = task.ty,
                        .inner_value = self.emittedArmStructure(task.arm_body, recorded),
                        .outer_branches = task.outer_branches,
                        .bindings = task.chain,
                    } } };
                }
                if (task.source_scope) |scope| {
                    try self.shadowPatLocals(scope.pat);
                    try self.shadowStmtSpanLocals(scope.bindings);
                }
                frame.cursor = 1;
                return .{ .call = .{ .expr_value = .{ .expr = task.arm_body, .bindings = task.chain } } };
            },
            1 => {
                frame.cursor = 2;
                return .{ .call = .{ .distribute_value = .{
                    .ty = task.ty,
                    .inner_value = input.?.get(.value),
                    .outer_branches = task.outer_branches,
                    .bindings = task.chain,
                } } };
            },
            2 => {
                task.outer_value = input.?.get(.maybe_value) orelse {
                    self.subst.restore(task.change_start);
                    return .{ .ret = .{ .maybe_arm = null } };
                };
                if (task.recorded_value != null) {
                    const arm_data = self.pass.program.getExpr(task.arm_body).data;
                    if (arm_data == .block) {
                        task.statements = try self.arena.allocator().create(std.ArrayList(Ast.StmtId));
                        task.statements.* = .empty;
                        task.statements_owned = true;
                        const existing = self.pass.program.stmtSpan(arm_data.block.statements);
                        for (0..existing.len) |index| {
                            try task.statements.append(self.pass.allocator, GuardedList.at(existing, index));
                        }
                        // A later distribution keeps these statements and replaces
                        // the tail again. Keep the new value's strict bindings here
                        // too, so its recorded leaves remain in that retained scope.
                        frame.cursor = 3;
                        return .{ .call = .{ .emit_block_with_tail = .{ .ty = task.ty, .statements = task.statements, .tail = .{
                            .reused = null,
                            .bindings = task.chain.*,
                            .value = task.outer_value,
                        } } } };
                    }
                }
                frame.cursor = 4;
                return .{ .call = .{ .materialize = .{ .value = task.outer_value } } };
            },
            3 => {
                const body = input.?.get(.expr);
                task.statements.deinit(self.pass.allocator);
                task.statements_owned = false;
                self.subst.restore(task.change_start);
                return .{ .ret = .{ .maybe_arm = .{ .body = body, .value = task.outer_value } } };
            },
            else => {
                const body = try self.wrapBindings(task.chain.*, input.?.get(.expr));
                self.subst.restore(task.change_start);
                return .{ .ret = .{ .maybe_arm = .{ .body = body, .value = task.outer_value } } };
            },
        }
    }

    /// Collapse an outer match against one inner-branch result: a known
    /// constructor selects its arm directly, and a branch-built result
    /// distributes recursively so the arms land where the constructors are
    /// known.
    const DistributeValueTask = struct {
        ty: Type.TypeId,
        inner_value: Value,
        outer_branches: Ast.Span(Ast.Branch),
        bindings: *BindingChain,
    };

    fn stepDistributeValue(frame: *CloneFrame, task: *DistributeValueTask, input: ?CloneResult) CloneStep {
        if (frame.cursor == 0) {
            frame.cursor = 1;
            return .{ .call = .{ .select_known_match = .{ .scrutinee = task.inner_value, .branches = task.outer_branches, .bindings = task.bindings } } };
        }
        if (input.?.get(.maybe_value)) |value| return .{ .ret = .{ .maybe_value = value } };
        return switch (task.inner_value) {
            .expr => |expr| .{ .tail = .{ .case_of_case = .{ .ty = task.ty, .scrutinee_expr = expr, .outer_branches = task.outer_branches } } },
            .runtime_anchor, .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => .{ .ret = .{ .maybe_value = null } },
        };
    }

    const InlineCallableTask = struct {
        ty: Type.TypeId,
        callable: CallableValue,
        args_span: Ast.Span(Ast.ExprId),
        original_expr: Ast.ExprId,
        result_shape_demanded: bool,
        bindings: *BindingChain,
        source_expr: Ast.ExprId = undefined,
        needs_typed_boundary: bool = false,
        exact_call_size: ?usize = null,
        source_args: []const Ast.TypedLocal = &.{},
        args: []const Ast.ExprId = &.{},
        source_captures: []const Ast.TypedLocal = &.{},
        change_start: usize = 0,
        arg_values: []Value = &.{},
        prepared_args: []Value = &.{},
        saved_inline_scope: Ast.InlineScopeId = undefined,
        callee_expr: Ast.ExprId = undefined,
        inlined: bool = false,
    };

    /// Cursor states of an inlined callable call.
    const InlineCallableCursor = struct {
        const start = 0;
        const residual_callee = 1;
        const residual_args = 2;
        const capture = 3;
        const arg = 4;
        const prepared_arg = 5;
        const body = 6;
        const boundary = 7;
    };

    /// Keep a callable call residual: its callee materialized and its
    /// arguments cloned.
    fn residualCallableCall(frame: *CloneFrame, task: *InlineCallableTask) CloneStep {
        frame.cursor = InlineCallableCursor.residual_callee;
        return .{ .call = .{ .materialize = .{ .value = .{ .callable = task.callable } } } };
    }

    fn finishInlinedCall(self: *Cloner, task: anytype, value: Value, callee: Ast.FnId) CloneStep {
        self.current_inline_scope = task.saved_inline_scope;
        const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
        if (popped.fn_id != callee) Common.invariant("call-pattern inline stack was corrupted");
        self.subst.restore(task.change_start);
        return retValue(value);
    }

    fn stepInlineCallable(self: *Cloner, frame: *CloneFrame, task: *InlineCallableTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const callable = task.callable;
        switch (frame.cursor) {
            InlineCallableCursor.start => {
                const source_fn = self.pass.program.getFn(callable.fn_id);
                const source = try self.pass.inlineSourceBody(callable.fn_id) orelse return residualCallableCall(frame, task);
                if (!source.size.admits()) return residualCallableCall(frame, task);
                if (try exprContainsReturn(self.pass.allocator, self.pass.program, source.expr) or
                    try exprContainsFreeLoopControl(self.pass.allocator, self.pass.program, source.expr))
                {
                    return residualCallableCall(frame, task);
                }
                // Independently specialized Monotype graphs can give the caller and
                // callee representation-distinct result types. Iterator specialization
                // may still expose the callee's structural result, but must retain an
                // explicit boundary for every path which materializes that value.
                task.needs_typed_boundary = !sameType(self.pass.program, task.ty, source_fn.ret);
                if (task.needs_typed_boundary and !callable.iterator_step) return residualCallableCall(frame, task);
                var callable_call_size = ConstructorSize{ .exact = 0 };
                for (callable.captures) |capture| callable_call_size = callable_call_size.plus(try self.knownConstructorSize(capture.value));
                callable_call_size = callable_call_size.plus(try self.argsKnownConstructorSize(task.args_span));
                task.exact_call_size = callable_call_size.exactValue();
                for (self.inline_stack.items) |active| {
                    if (active.fn_id != callable.fn_id) continue;
                    const current_size = task.exact_call_size orelse return residualCallableCall(frame, task);
                    const active_size = active.known_size orelse return residualCallableCall(frame, task);
                    if (current_size == 0 or current_size >= active_size) return residualCallableCall(frame, task);
                }
                if (!self.admitInlineBodyGrowth(source.size)) return residualCallableCall(frame, task);

                task.source_expr = source.expr;
                task.source_args = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
                task.args = try GuardedList.dupe(arena, Ast.ExprId, self.pass.program.exprSpan(task.args_span));
                if (task.source_args.len != task.args.len) Common.invariant("callable call arity differed from lifted function arity");

                task.source_captures = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
                if (task.source_captures.len != callable.captures.len) {
                    Common.invariant("callable value capture count differed from lifted function capture count");
                }

                task.change_start = self.subst.watermark();
                task.arg_values = try arena.alloc(Value, task.args.len);
                task.prepared_args = try arena.alloc(Value, task.args.len);
                frame.cursor = InlineCallableCursor.capture;
            },
            InlineCallableCursor.residual_callee => {
                task.callee_expr = input.?.get(.expr);
                frame.cursor = InlineCallableCursor.residual_args;
                return .{ .call = .{ .expr_span = .{ .span = task.args_span } } };
            },
            InlineCallableCursor.residual_args => return retValue(.{ .expr = try self.addExpr(.{ .ty = task.ty, .data = .{ .call_value = .{
                .callee = task.callee_expr,
                .args = input.?.get(.expr_span),
            } } }) }),
            InlineCallableCursor.capture => {
                const prepared = input.?.get(.value);
                try self.subst.put(self.pass.program, task.source_captures[frame.index].local, prepared);
                frame.index += 1;
            },
            InlineCallableCursor.arg => {
                task.arg_values[frame.index] = input.?.get(.value);
                frame.index += 1;
            },
            InlineCallableCursor.prepared_arg => {
                task.prepared_args[frame.index] = input.?.get(.value);
                frame.index += 1;
            },
            InlineCallableCursor.body => {
                const inlined = input.?.get(.value);
                if (task.needs_typed_boundary) {
                    frame.cursor = InlineCallableCursor.boundary;
                    return .{ .call = .{ .wrap_typed_boundary = .{ .target_ty = task.ty, .structure = inlined } } };
                }
                return self.finishInlinedCall(task, inlined, callable.fn_id);
            },
            InlineCallableCursor.boundary => return self.finishInlinedCall(task, input.?.get(.value), callable.fn_id),
            InlineCallableCursor.boundary + 1...std.math.maxInt(u8) => unreachable,
        }
        if (frame.cursor == InlineCallableCursor.capture) {
            if (frame.index < task.source_captures.len) {
                const id = self.pass.program.captureIdOfLocal(task.source_captures[frame.index].local);
                const capture_value = callableCaptureValueForId(callable.captures, id) orelse
                    Common.invariant("callable value had no value for a source capture slot");
                return .{ .call = try self.makeReusableTask(capture_value, task.bindings) };
            }
            frame.index = 0;
            frame.cursor = InlineCallableCursor.arg;
        }
        if (frame.cursor == InlineCallableCursor.arg) {
            if (frame.index < task.args.len) {
                const callee_raw = @intFromEnum(callable.fn_id);
                const demand_arg_shape = task.result_shape_demanded and
                    callee_raw < self.pass.plans.len and
                    self.pass.plans[callee_raw].used_args[frame.index];
                return .{ .call = .{ .inline_value = .{ .expr = task.args[frame.index], .demand_shape = demand_arg_shape, .bindings = task.bindings } } };
            }
            frame.index = 0;
            frame.cursor = InlineCallableCursor.prepared_arg;
        }
        if (frame.index < task.arg_values.len) {
            return .{ .call = .{ .local_value = .{ .value = task.arg_values[frame.index], .bindings = task.bindings } } };
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callable.fn_id, .known_size = task.exact_call_size });
        for (task.source_args, task.prepared_args) |source_arg, arg_value| {
            try self.subst.put(self.pass.program, source_arg.local, arg_value);
        }
        task.saved_inline_scope = self.current_inline_scope;
        try self.enterInlineScope(callable.fn_id, self.pass.program.exprLoc(task.original_expr));
        frame.cursor = InlineCallableCursor.body;
        return .{ .call = try self.withoutReuseTask(.{ .expr_value = .{ .expr = task.source_expr, .bindings = task.bindings } }) };
    }

    const InlineDirectTask = struct {
        callee: Ast.FnId,
        args_span: Ast.Span(Ast.ExprId),
        captures_span: Ast.Span(Ast.CaptureOperand),
        original_expr: Ast.ExprId,
        result_shape_demanded: bool,
        bindings: *BindingChain,
        result_ty: Type.TypeId = undefined,
        source_expr: Ast.ExprId = undefined,
        needs_typed_boundary: bool = false,
        exact_call_size: ?usize = null,
        source_args: []const Ast.TypedLocal = &.{},
        args: []const Ast.ExprId = &.{},
        captures: []const Ast.TypedLocal = &.{},
        operands: []const Ast.CaptureOperand = &.{},
        change_start: usize = 0,
        capture_values: []CaptureValue = &.{},
        arg_values: []Value = &.{},
        prepared_captures: []Value = &.{},
        prepared_args: []Value = &.{},
        saved_inline_scope: Ast.InlineScopeId = undefined,
    };

    /// Cursor states of an inlined direct call.
    const InlineDirectCursor = struct {
        const start = 0;
        const plain = 1;
        const capture_value = 2;
        const arg_value = 3;
        const prepared_capture = 4;
        const prepared_arg = 5;
        const body = 6;
        const boundary = 7;
    };

    fn plainDirectCall(frame: *CloneFrame, task: *InlineDirectTask) CloneStep {
        frame.cursor = InlineDirectCursor.plain;
        return .{ .call = .{ .plain = .{ .expr = task.original_expr } } };
    }

    fn stepInlineDirect(self: *Cloner, frame: *CloneFrame, task: *InlineDirectTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const callee = task.callee;
        switch (frame.cursor) {
            InlineDirectCursor.start => {
                const source_fn = self.pass.program.getFn(callee);
                task.result_ty = self.pass.program.getExpr(task.original_expr).ty;
                const source = try self.pass.inlineSourceBody(callee) orelse return plainDirectCall(frame, task);
                if (!source.size.admits()) return plainDirectCall(frame, task);
                if (try exprContainsReturn(self.pass.allocator, self.pass.program, source.expr) or
                    try exprContainsFreeLoopControl(self.pass.allocator, self.pass.program, source.expr))
                {
                    return plainDirectCall(frame, task);
                }
                // A checker-stamped iterator path may expose a callee result whose
                // independently specialized type differs from the call-site type, as
                // long as the representation conversion remains explicit.
                task.needs_typed_boundary = !sameType(self.pass.program, task.result_ty, source_fn.ret);
                if (task.needs_typed_boundary and self.iterator_inline_depth == 0) return plainDirectCall(frame, task);
                const direct_call_size = (try self.argsKnownConstructorSize(task.args_span)).plus(try self.captureOperandsKnownConstructorSize(task.captures_span));
                task.exact_call_size = direct_call_size.exactValue();
                for (self.inline_stack.items) |active| {
                    if (active.fn_id != callee) continue;
                    const current_size = task.exact_call_size orelse return plainDirectCall(frame, task);
                    const active_size = active.known_size orelse return plainDirectCall(frame, task);
                    if (current_size == 0 or current_size >= active_size) return plainDirectCall(frame, task);
                }
                if (!self.admitInlineBodyGrowth(source.size)) return plainDirectCall(frame, task);
                task.source_expr = source.expr;
                task.source_args = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
                task.args = try GuardedList.dupe(arena, Ast.ExprId, self.pass.program.exprSpan(task.args_span));
                if (task.source_args.len != task.args.len) Common.invariant("direct call arity differed from lifted function arity");

                task.change_start = self.subst.watermark();

                task.captures = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
                // The call's capture operands are keyed by CaptureId, not positional
                // with the callee's capture slots. Clone each operand's value keyed by
                // id, then resolve each slot's value by its own CaptureId below.
                task.operands = try GuardedList.dupe(arena, Ast.CaptureOperand, self.pass.program.captureOperandSpan(task.captures_span));
                if (task.captures.len != task.operands.len) {
                    Common.invariant("direct call capture count differed from lifted function capture count");
                }
                task.capture_values = try arena.alloc(CaptureValue, task.operands.len);
                task.arg_values = try arena.alloc(Value, task.args.len);
                task.prepared_captures = try arena.alloc(Value, task.captures.len);
                task.prepared_args = try arena.alloc(Value, task.args.len);
                frame.cursor = InlineDirectCursor.capture_value;
            },
            InlineDirectCursor.plain => return retValue(.{ .expr = input.?.get(.expr) }),
            InlineDirectCursor.capture_value => {
                task.capture_values[frame.index] = .{ .id = task.operands[frame.index].id, .value = input.?.get(.value) };
                frame.index += 1;
            },
            InlineDirectCursor.arg_value => {
                task.arg_values[frame.index] = input.?.get(.value);
                frame.index += 1;
            },
            InlineDirectCursor.prepared_capture => {
                task.prepared_captures[frame.index] = input.?.get(.value);
                frame.index += 1;
            },
            InlineDirectCursor.prepared_arg => {
                task.prepared_args[frame.index] = input.?.get(.value);
                frame.index += 1;
            },
            InlineDirectCursor.body => {
                const inlined = input.?.get(.value);
                if (task.needs_typed_boundary) {
                    frame.cursor = InlineDirectCursor.boundary;
                    return .{ .call = .{ .wrap_typed_boundary = .{ .target_ty = task.result_ty, .structure = inlined } } };
                }
                return self.finishInlinedCall(task, inlined, callee);
            },
            InlineDirectCursor.boundary => return self.finishInlinedCall(task, input.?.get(.value), callee),
            InlineDirectCursor.boundary + 1...std.math.maxInt(u8) => unreachable,
        }
        if (frame.cursor == InlineDirectCursor.capture_value) {
            if (frame.index < task.operands.len) {
                return .{ .call = .{ .inline_value = .{ .expr = task.operands[frame.index].value, .demand_shape = false, .bindings = task.bindings } } };
            }
            frame.index = 0;
            frame.cursor = InlineDirectCursor.arg_value;
        }
        if (frame.cursor == InlineDirectCursor.arg_value) {
            if (frame.index < task.args.len) {
                const demand_arg_shape = task.result_shape_demanded and
                    @intFromEnum(callee) < self.pass.plans.len and
                    self.pass.plans[@intFromEnum(callee)].used_args[frame.index];
                return .{ .call = .{ .inline_value = .{ .expr = task.args[frame.index], .demand_shape = demand_arg_shape, .bindings = task.bindings } } };
            }
            frame.index = 0;
            frame.cursor = InlineDirectCursor.prepared_capture;
        }
        if (frame.cursor == InlineDirectCursor.prepared_capture) {
            if (frame.index < task.captures.len) {
                const id = self.pass.program.captureIdOfLocal(task.captures[frame.index].local);
                const capture_value = callableCaptureValueForId(task.capture_values, id) orelse
                    Common.invariant("direct call had no value for a source capture slot");
                return .{ .call = .{ .local_value = .{ .value = capture_value, .bindings = task.bindings } } };
            }
            frame.index = 0;
            frame.cursor = InlineDirectCursor.prepared_arg;
        }
        if (frame.index < task.arg_values.len) {
            return .{ .call = .{ .local_value = .{ .value = task.arg_values[frame.index], .bindings = task.bindings } } };
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callee, .known_size = task.exact_call_size });
        for (task.captures, task.prepared_captures) |capture, capture_value| {
            try self.subst.put(self.pass.program, capture.local, capture_value);
        }
        for (task.source_args, task.prepared_args) |source_arg, arg_value| {
            try self.subst.put(self.pass.program, source_arg.local, arg_value);
        }
        task.saved_inline_scope = self.current_inline_scope;
        try self.enterInlineScope(callee, self.pass.program.exprLoc(task.original_expr));
        frame.cursor = InlineDirectCursor.body;
        return .{ .call = try self.withoutReuseTask(.{ .expr_value = .{ .expr = task.source_expr, .bindings = task.bindings } }) };
    }

    const WrapTypedBoundaryTask = struct { target_ty: Type.TypeId, structure: Value };

    fn stepWrapTypedBoundary(self: *Cloner, frame: *CloneFrame, task: *WrapTypedBoundaryTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            frame.cursor = 1;
            return .{ .call = .{ .materialize = .{ .value = task.structure } } };
        }
        const runtime = try self.addExpr(.{ .ty = task.target_ty, .data = .{ .typed_boundary = .{
            .value = input.?.get(.expr),
        } } });
        return retValue(try self.typedBoundaryValue(task.structure, runtime));
    }

    /// Clone one statement. A binding statement whose value's opaque leaves
    /// can all be named dissolves instead: the returned binding chain is
    /// placed by the caller at this statement's position—the same
    /// computations in the same order—and the bound name keeps its
    /// structured value for the rest of the block. `stmt` is null when the
    /// source binding dissolved completely.
    const StmtTask = struct {
        stmt: Ast.StmtId,
        saved: SourceContext = undefined,
        chain: *BindingChain = undefined,
        recursive_value_bindings: *BindingChain = undefined,
        recursive_pat: ?Ast.PatId = null,
        value: Value = undefined,
        value_expr: Ast.ExprId = undefined,
        materialized_value: Ast.ExprId = undefined,
        change_before: usize = 0,
        bindings_before: ?*BindingNode = null,
    };

    fn finishStmt(self: *Cloner, task: *StmtTask, cloned: ?Ast.Stmt) Common.LowerError!CloneStep {
        const result = ClonedStmt{
            .bindings = task.chain.*,
            .stmt = if (cloned) |actual| try self.addStmt(actual) else null,
        };
        task.saved.restore(self);
        return .{ .ret = .{ .stmt = result } };
    }

    /// Leave the active-recursive marks of a recursive binding statement.
    fn leaveRecursiveStmt(self: *Cloner, task: *StmtTask, let_: anytype) Allocator.Error!void {
        if (task.recursive_pat) |pat| try self.unmarkActiveRecursiveValuePat(pat);
        if (let_.recursive) try self.unmarkActiveRecursiveValuePat(let_.pat);
    }

    fn stepStmt(self: *Cloner, frame: *CloneFrame, task: *StmtTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            task.saved = try self.enterStmtSource(task.stmt);
            task.chain = try self.newChain();
        }
        const stmt = self.pass.program.getStmt(task.stmt);
        const cursor = frame.cursor;
        frame.cursor += 1;
        switch (stmt) {
            .uninitialized => |pat| return try self.finishStmt(task, .{ .uninitialized = try self.clonePat(pat, .bind_runtime) }),
            .let_ => |let_| switch (cursor) {
                0 => {
                    if (let_.recursive) try self.markActiveRecursiveValuePat(let_.pat);
                    task.recursive_pat = if (let_.recursive)
                        try self.clonePat(let_.pat, .bind_runtime)
                    else
                        null;
                    if (task.recursive_pat) |pat| try self.markActiveRecursiveValuePat(pat);
                    // Bindings produced while cloning a recursive initializer are
                    // in the recursive pattern's scope and may themselves use its
                    // fresh runtime local. Keep that complete chain inside the
                    // initializer; placing it before the recursive statement
                    // would turn those exact back-edges into free locals.
                    task.recursive_value_bindings = try self.newChain();
                    return .{ .call = .{ .expr_value = .{
                        .expr = let_.value,
                        .bindings = if (let_.recursive) task.recursive_value_bindings else task.chain,
                    } } };
                },
                1 => {
                    task.value = input.?.get(.value);
                    return .{ .call = .{ .materialize = .{ .value = task.value } } };
                },
                2 => {
                    task.materialized_value = input.?.get(.expr);
                    task.value_expr = if (let_.recursive)
                        try self.wrapBindings(task.recursive_value_bindings.*, task.materialized_value)
                    else
                        task.materialized_value;
                    const continuation_value = if (let_.recursive and
                        self.pass.program.getPat(let_.pat).data == .bind and
                        self.pass.program.getPat(task.recursive_pat.?).data == .bind)
                    continuation: {
                        const runtime_local = self.pass.program.getPat(task.recursive_pat.?).data.bind;
                        const runtime = try self.addExpr(.{
                            .ty = self.pass.program.getLocal(runtime_local).ty,
                            .data = .{ .local = runtime_local },
                        });
                        var budget: u32 = recursive_anchor_scope_work_budget;
                        break :continuation (try self.reanchorRecursiveValue(
                            task.value,
                            runtime,
                            task.recursive_value_bindings.*,
                            &budget,
                            true,
                        )) orelse Common.invariant("whole recursive binding had no runtime anchor");
                    } else task.value;
                    const value_escapes_recursive_scope = let_.recursive and
                        self.pass.program.getPat(let_.pat).data != .bind and
                        try task.recursive_value_bindings.referencedByExpr(self.pass.allocator, self.pass.program, task.materialized_value);
                    if (!value_escapes_recursive_scope and
                        try self.bindPatToReusableValue(let_.pat, continuation_value) == .match)
                    {
                        const cloned: Ast.Stmt = .{ .let_ = .{
                            .pat = task.recursive_pat orelse try self.clonePat(let_.pat, .output_only),
                            .value = task.value_expr,
                            .recursive = let_.recursive,
                            .comptime_site = let_.comptime_site,
                        } };
                        try self.leaveRecursiveStmt(task, let_);
                        return try self.finishStmt(task, cloned);
                    }
                    // A binding refers to itself exactly when its producer
                    // marked it recursive.
                    const self_referential = let_.recursive;
                    if (!self_referential) {
                        // The drained bindings sit exactly where the statement
                        // sat, so no evaluation moves and no gate is needed.
                        task.change_before = self.subst.watermark();
                        task.bindings_before = task.chain.mark();
                        return .{ .call = try self.makeReusableTask(task.value, task.chain) };
                    }
                    return try self.finishRuntimeLetStmt(task, let_);
                },
                else => {
                    const reusable = input.?.get(.value);
                    if (try self.bindPatToFlowValue(let_.pat, reusable)) {
                        try self.leaveRecursiveStmt(task, let_);
                        return try self.finishStmt(task, null);
                    }
                    self.subst.restore(task.change_before);
                    task.chain.rewind(task.bindings_before);
                    return try self.finishRuntimeLetStmt(task, let_);
                },
            },
            .expr => |expr| {
                if (cursor == 0) return .{ .call = .{ .expr = expr } };
                return try self.finishStmt(task, .{ .expr = input.?.get(.expr) });
            },
            .expect => |expr| {
                if (cursor == 0) return .{ .call = .{ .expr = expr } };
                return try self.finishStmt(task, .{ .expect = input.?.get(.expr) });
            },
            .dbg => |expr| {
                if (cursor == 0) return .{ .call = .{ .expr = expr } };
                return try self.finishStmt(task, .{ .dbg = input.?.get(.expr) });
            },
            .return_ => |ret| {
                if (cursor == 0) return .{ .call = .{ .expr = ret.value } };
                return try self.finishStmt(task, .{ .return_ = .{
                    .value = input.?.get(.expr),
                    .target = ret.target,
                } });
            },
            .crash => |msg| return try self.finishStmt(task, .{ .crash = msg }),
            .checked_error => |msg| return try self.finishStmt(task, .{ .checked_error = msg }),
        }
    }

    fn finishRuntimeLetStmt(self: *Cloner, task: *StmtTask, let_: anytype) Common.LowerError!CloneStep {
        const cloned: Ast.Stmt = .{ .let_ = .{
            .pat = task.recursive_pat orelse try self.clonePat(let_.pat, .bind_runtime),
            .value = task.value_expr,
            .recursive = let_.recursive,
            .comptime_site = let_.comptime_site,
        } };
        try self.leaveRecursiveStmt(task, let_);
        return try self.finishStmt(task, cloned);
    }

    /// A span of expressions, capture operands, or record fields, each
    /// cloned in order. An unchanged span is reused when the clone may reuse
    /// source.
    const SpanTask = struct {
        span: SpanSource,
        values: []Ast.ExprId = &.{},
        unchanged: bool = false,
        count: usize = 0,
    };

    const SpanSource = union(enum) {
        exprs: Ast.Span(Ast.ExprId),
        captures: Ast.Span(Ast.CaptureOperand),
        fields: Ast.Span(Ast.FieldExpr),
    };

    fn spanChild(self: *Cloner, span: SpanSource, index: usize) Ast.ExprId {
        return switch (span) {
            .exprs => |exprs| GuardedList.at(self.pass.program.exprSpan(exprs), index),
            .captures => |captures| self.pass.program.captureOperandAt(captures, index).value,
            .fields => |fields| GuardedList.at(self.pass.program.fieldExprSpan(fields), index).value,
        };
    }

    fn stepSpan(self: *Cloner, frame: *CloneFrame, task: *SpanTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            task.count = switch (task.span) {
                .exprs => |span| span.len,
                .captures => |span| span.len,
                .fields => |span| span.len,
            };
            task.values = try self.arena.allocator().alloc(Ast.ExprId, task.count);
            task.unchanged = self.source_reuse == .original_body;
            frame.cursor = 1;
        } else {
            task.values[frame.index] = input.?.get(.expr);
            task.unchanged = task.unchanged and task.values[frame.index] == self.spanChild(task.span, frame.index);
            frame.index += 1;
        }
        if (frame.index < task.count) return .{ .call = .{ .expr = self.spanChild(task.span, frame.index) } };
        switch (task.span) {
            .exprs => |span| {
                if (task.unchanged) return .{ .ret = .{ .expr_span = span } };
                return .{ .ret = .{ .expr_span = try self.pass.program.addExprSpan(task.values) } };
            },
            .captures => |span| {
                if (task.unchanged) return .{ .ret = .{ .capture_span = span } };
                const operands = try self.pass.allocator.alloc(Ast.CaptureOperand, task.count);
                defer self.pass.allocator.free(operands);
                for (operands, task.values, 0..) |*operand, value, index| {
                    operand.* = .{ .id = self.pass.program.captureOperandAt(span, index).id, .value = value };
                }
                return .{ .ret = .{ .capture_span = try self.pass.program.addCaptureOperandSpan(operands) } };
            },
            .fields => |span| {
                if (task.unchanged) return .{ .ret = .{ .field_span = span } };
                const fields = try self.pass.allocator.alloc(Ast.FieldExpr, task.count);
                defer self.pass.allocator.free(fields);
                for (fields, task.values, 0..) |*field, value, index| {
                    field.* = .{ .name = GuardedList.at(self.pass.program.fieldExprSpan(span), index).name, .value = value };
                }
                return .{ .ret = .{ .field_span = try self.pass.program.addFieldExprSpan(fields) } };
            },
        }
    }

    const StmtSpanTask = struct {
        span: Ast.Span(Ast.StmtId),
        source: []const Ast.StmtId = &.{},
        values: std.ArrayList(Ast.StmtId) = .empty,
    };

    fn stepStmtSpan(self: *Cloner, frame: *CloneFrame, task: *StmtSpanTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            task.source = try GuardedList.dupe(self.arena.allocator(), Ast.StmtId, self.pass.program.stmtSpan(task.span));
            frame.cursor = 1;
        } else {
            const cloned = input.?.get(.stmt);
            try self.appendBindingStmts(cloned.bindings, &task.values);
            if (cloned.stmt) |cloned_stmt| try task.values.append(self.pass.allocator, cloned_stmt);
            frame.index += 1;
        }
        if (frame.index < task.source.len) return .{ .call = .{ .stmt = .{ .stmt = task.source[frame.index] } } };
        const span = try self.pass.program.addStmtSpan(task.values.items);
        task.values.deinit(self.pass.allocator);
        return .{ .ret = .{ .stmt_span = span } };
    }

    const BranchSpanTask = struct {
        span: Ast.Span(Ast.Branch),
        source: []const Ast.Branch = &.{},
        branches: []Ast.Branch = &.{},
        values: []Value = &.{},
        change_start: usize = 0,
    };

    fn stepBranchSpan(self: *Cloner, frame: *CloneFrame, task: *BranchSpanTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        switch (frame.cursor) {
            0 => {
                task.source = try GuardedList.dupe(arena, Ast.Branch, self.pass.program.branchSpan(task.span));
                task.branches = try arena.alloc(Ast.Branch, task.source.len);
                task.values = try arena.alloc(Value, task.source.len);
            },
            // The branch's cloned binding statements.
            1 => {
                const branch = task.source[frame.index];
                task.branches[frame.index].bindings = input.?.get(.stmt_span);
                if (branch.guard) |guard| {
                    frame.cursor = 2;
                    return .{ .call = .{ .expr = guard } };
                }
                task.branches[frame.index].guard = null;
                frame.cursor = 3;
                return .{ .call = .{ .keeping = .{ .expr = branch.body } } };
            },
            // The branch's cloned guard.
            2 => {
                task.branches[frame.index].guard = input.?.get(.expr);
                frame.cursor = 3;
                return .{ .call = .{ .keeping = .{ .expr = task.source[frame.index].body } } };
            },
            // The branch's cloned body.
            else => {
                const body = input.?.get(.arm);
                task.branches[frame.index].body = body.body;
                task.values[frame.index] = body.value;
                self.subst.restore(task.change_start);
                frame.index += 1;
            },
        }
        if (frame.index < task.source.len) {
            const branch = task.source[frame.index];
            task.change_start = self.subst.watermark();
            task.branches[frame.index].pat = try self.clonePat(branch.pat, .bind_runtime);
            frame.cursor = 1;
            return .{ .call = .{ .stmt_span = .{ .span = branch.bindings } } };
        }
        return .{ .ret = .{ .branches = .{
            .span = try self.pass.program.addBranchSpan(task.branches),
            .values = task.values,
        } } };
    }

    const IfBranchSpanTask = struct {
        span: Ast.Span(Ast.IfBranch),
        source: []const Ast.IfBranch = &.{},
        branches: []Ast.IfBranch = &.{},
        values: []Value = &.{},
        unchanged: bool = false,
    };

    fn stepIfBranchSpan(self: *Cloner, frame: *CloneFrame, task: *IfBranchSpanTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        switch (frame.cursor) {
            0 => {
                task.source = try GuardedList.dupe(arena, Ast.IfBranch, self.pass.program.ifBranchSpan(task.span));
                task.branches = try arena.alloc(Ast.IfBranch, task.source.len);
                task.values = try arena.alloc(Value, task.source.len);
                task.unchanged = self.source_reuse == .original_body;
            },
            // The branch's cloned condition.
            1 => {
                task.branches[frame.index].cond = input.?.get(.expr);
                frame.cursor = 2;
                return .{ .call = .{ .keeping = .{ .expr = task.source[frame.index].body } } };
            },
            // The branch's cloned body.
            else => {
                const body = input.?.get(.arm);
                task.branches[frame.index].body = body.body;
                task.values[frame.index] = body.value;
                task.unchanged = task.unchanged and std.meta.eql(task.branches[frame.index], task.source[frame.index]);
                frame.index += 1;
            },
        }
        if (frame.index < task.source.len) {
            frame.cursor = 1;
            return .{ .call = .{ .expr = task.source[frame.index].cond } };
        }
        return .{ .ret = .{ .if_branches = .{
            .span = if (task.unchanged) task.span else try self.pass.program.addIfBranchSpan(task.branches),
            .values = task.values,
        } } };
    }

    const MaterializeTask = struct {
        value: Value,
        exprs: []Ast.ExprId = &.{},
    };

    fn stepMaterialize(self: *Cloner, frame: *CloneFrame, task: *MaterializeTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const value = task.value;
        if (frame.cursor == 0) {
            frame.cursor = 1;
            const child_count: usize = switch (value) {
                .expr => |expr| return retExpr(expr),
                .runtime_anchor => |anchor| return retExpr(anchor.runtime),
                .static_data_candidate => |candidate| return retExpr(candidate.expr),
                .tag => |tag| tag.payloads.len,
                .record => |record| record.fields.len,
                .tuple => |tuple| tuple.items.len,
                .nominal => blk: {
                    if (self.materialize_strip_depth >= value_wrapper_strip_cap) {
                        Common.invariant("materialize followed a nominal backing chain past the strip cap; a cyclic value reached materialization");
                    }
                    self.materialize_strip_depth += 1;
                    break :blk 1;
                },
                .callable => |callable| return .{ .tail = .{ .materialize_callable = .{ .callable = callable } } },
            };
            task.exprs = try self.arena.allocator().alloc(Ast.ExprId, child_count);
        } else {
            task.exprs[frame.index] = input.?.get(.expr);
            frame.index += 1;
        }
        if (frame.index < task.exprs.len) {
            const child = switch (value) {
                .tag => |tag| tag.payloads[frame.index],
                .record => |record| record.fields[frame.index].value,
                .tuple => |tuple| tuple.items[frame.index],
                .nominal => |nominal| nominal.backing.*,
                .expr, .runtime_anchor, .static_data_candidate, .callable => unreachable,
            };
            return .{ .call = .{ .materialize = .{ .value = child } } };
        }
        switch (value) {
            .tag => |tag| return retExpr(try self.addExpr(.{ .ty = tag.ty, .data = .{ .tag = .{
                .name = tag.name,
                .payloads = try self.pass.program.addExprSpan(task.exprs),
            } } })),
            .record => |record| {
                const fields = try self.pass.allocator.alloc(Ast.FieldExpr, record.fields.len);
                defer self.pass.allocator.free(fields);
                for (record.fields, task.exprs, fields) |field, expr, *out| out.* = .{ .name = field.name, .value = expr };
                return retExpr(try self.addExpr(.{ .ty = record.ty, .data = .{
                    .record = try self.pass.program.addFieldExprSpan(fields),
                } }));
            },
            .tuple => |tuple| return retExpr(try self.addExpr(.{ .ty = tuple.ty, .data = .{
                .tuple = try self.pass.program.addExprSpan(task.exprs),
            } })),
            .nominal => |nominal| {
                const materialized = try self.addExpr(.{ .ty = nominal.ty, .data = .{
                    .nominal = task.exprs[0],
                } });
                self.materialize_strip_depth -= 1;
                return retExpr(materialized);
            },
            .expr, .runtime_anchor, .static_data_candidate, .callable => unreachable,
        }
    }

    fn stepMaterializeCallable(self: *Cloner, callable: CallableValue) CloneStep {
        const fn_ = self.pass.program.getFn(callable.fn_id);
        const captures = self.pass.program.typedLocalSpan(fn_.captures);
        if (captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        var all_original = true;
        for (0..captures.len) |index| {
            const capture = GuardedList.at(captures, index);
            const value = callableCaptureValueForId(callable.captures, self.pass.program.captureIdOfLocal(capture.local)) orelse {
                all_original = false;
                break;
            };
            const expr = switch (value) {
                .expr => |expr| expr,
                .runtime_anchor => |anchor| anchor.runtime,
                .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => {
                    all_original = false;
                    break;
                },
            };
            const local = localExpr(self.pass.program, expr) orelse {
                all_original = false;
                break;
            };
            if (local != capture.local) {
                all_original = false;
                break;
            }
        }

        if (!all_original and self.emit_callable_workers) return .{ .tail = .{ .materialize_worker = .{ .callable = callable } } };

        return .{ .tail = .{ .materialize_with_captures = .{
            .ty = callable.ty,
            .fn_id = callable.fn_id,
            .captures_span = fn_.captures,
            .values = callable.captures,
        } } };
    }

    fn cloneFieldTupleReadReplacingRoot(
        self: *Cloner,
        source: Ast.ExprId,
        root: Ast.ExprId,
        replacement: Ast.ExprId,
    ) Common.LowerError!Ast.ExprId {
        // Collect the read chain from `source` down to `root`, then emit it
        // again bottom-up over `replacement`.
        var chain: std.ArrayList(Ast.ExprId) = .empty;
        defer chain.deinit(self.pass.allocator);
        var current = source;
        while (current != root) {
            try chain.append(self.pass.allocator, current);
            const data = self.pass.program.getExpr(current).data;
            current = switch (data) {
                .field_access => |field| field.receiver,
                .tuple_access => |access| access.tuple,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => Common.invariant("recursive field/tuple read replacement reached a non-access expression before its root"),
            };
        }
        var rebuilt = replacement;
        var index = chain.items.len;
        while (index > 0) {
            index -= 1;
            const expr = self.pass.program.getExpr(chain.items[index]);
            rebuilt = switch (expr.data) {
                .field_access => |field| try self.addExpr(.{
                    .ty = expr.ty,
                    .data = .{
                        .field_access = .{
                            .receiver = rebuilt,
                            // The clone stays within `pass.program`, so the source span
                            // remains valid for the replacement expression.
                            .segments = field.segments,
                        },
                    },
                }),
                .tuple_access => |access| try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = rebuilt,
                    .elem_index = access.elem_index,
                } } }),
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
            };
        }
        return rebuilt;
    }

    const MaterializeWorkerTask = struct {
        callable: CallableValue,
        worker_fn_id: Ast.FnId = undefined,
        symbol: Common.Symbol = undefined,
        source_fn: Ast.Fn = undefined,
        args_span: Ast.Span(Ast.TypedLocal) = undefined,
        captures_span: Ast.Span(Ast.TypedLocal) = undefined,
        worker_capture_values: []CaptureValue = &.{},
        change_start: usize = 0,
        saved_strip_depth: usize = 0,
        outer_shapes: Ast.Program.FnShapesScope = undefined,
    };

    fn stepMaterializeWorker(self: *Cloner, frame: *CloneFrame, task: *MaterializeWorkerTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const arena = self.arena.allocator();
        const callable = task.callable;
        switch (frame.cursor) {
            0 => {},
            1 => {
                const worker_body = input.?.get(.expr);
                const worker_shapes = self.pass.program.finishFnShapes(task.outer_shapes);
                self.materialize_strip_depth = task.saved_strip_depth;
                self.pass.program.setFn(task.worker_fn_id, .{
                    .symbol = task.symbol,
                    .source = task.source_fn.source,
                    .signature = null,
                    .args = task.args_span,
                    .captures = task.captures_span,
                    .body = .{ .roc = worker_body },
                    .ret = task.source_fn.ret,
                    .shapes = worker_shapes,
                });
                frame.cursor = 2;
                return .{ .call = .{ .materialize_with_captures = .{
                    .ty = callable.ty,
                    .fn_id = task.worker_fn_id,
                    .captures_span = task.captures_span,
                    .values = task.worker_capture_values,
                } } };
            },
            else => {
                self.subst.restore(task.change_start);
                return .{ .ret = input.? };
            },
        }
        if (self.pass.borrowed_worker) Common.invariant("SpecConstr body worker attempted callable-worker creation");
        const source_fn_id = self.pass.callable_sources.get(callable.fn_id) orelse callable.fn_id;
        const source_fn = self.pass.program.getFn(source_fn_id);
        task.source_fn = source_fn;
        const source_captures = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        const worker_key: CallableWorkerIdentity = .{
            .template = Mono.fnTemplateDigest(
                source_fn.source orelse Common.invariant("rewritten callable source had no Monotype template identity"),
                &self.pass.program.types,
                &self.pass.program.names,
            ),
            .callable_abi = self.pass.program.types.representationDigestCached(&self.pass.program.names, callable.ty, null),
            .capture_abi = self.callableCaptureAbiDigest(source_captures, callable.captures),
        };
        if (self.pass.callable_workers.get(worker_key)) |worker_fn_id| {
            const worker = self.pass.program.getFn(worker_fn_id);
            return .{ .tail = .{ .materialize_with_captures = .{
                .ty = callable.ty,
                .fn_id = worker_fn_id,
                .captures_span = worker.captures,
                .values = callable.captures,
            } } };
        }

        const source_body = switch (self.pass.sourceBody(source_fn_id)) {
            .roc => |body| body,
            .hosted => Common.invariant("hosted callable value needed a rewritten body"),
        };
        // Capture locals are the worker's dynamic inputs. Preserve each
        // source capture's complete identity, but give its slot the exact type
        // of the rewritten operand that this worker body consumes.
        const worker_captures = try arena.alloc(Ast.TypedLocal, source_captures.len);
        task.worker_capture_values = try arena.alloc(CaptureValue, source_captures.len);
        const worker_body_values = try arena.alloc(Value, source_captures.len);
        for (source_captures, 0..) |source_capture, index| {
            const id = self.pass.program.captureIdOfLocal(source_capture.local);
            const capture_value = callableCaptureValueForId(callable.captures, id) orelse
                Common.invariant("rewritten callable had no value for a source capture slot");
            const field_tuple_read_root = self.activeRecursiveFieldTupleReadRoot(capture_value);
            const capture_ty = if (field_tuple_read_root) |root|
                self.pass.program.getExpr(root).ty
            else
                valueType(self.pass.program, capture_value);
            const source_local = self.pass.program.getLocal(source_capture.local);
            const local = try self.pass.program.addLocalWithCaptureIdentity(
                self.pass.symbols.fresh(),
                capture_ty,
                source_local.binder,
                id,
                source_local.checked_capture_id,
            );
            worker_captures[index] = .{ .local = local, .ty = capture_ty };
            const local_expr = try self.addExpr(.{
                .ty = capture_ty,
                .data = .{ .local = local },
            });
            if (field_tuple_read_root) |root| {
                const source_expr = switch (capture_value) {
                    .expr => |expr| expr,
                    .runtime_anchor => |anchor| anchor.runtime,
                    .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => Common.invariant("recursive field/tuple capture had no exact runtime expression"),
                };
                task.worker_capture_values[index] = .{ .id = id, .value = .{ .expr = root } };
                worker_body_values[index] = .{ .expr = try self.cloneFieldTupleReadReplacingRoot(source_expr, root, local_expr) };
            } else {
                task.worker_capture_values[index] = .{ .id = id, .value = capture_value };
                worker_body_values[index] = .{ .expr = local_expr };
            }
        }
        task.captures_span = try self.pass.program.addTypedLocalSpan(worker_captures);

        const source_args = try GuardedList.dupe(arena, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        const args = try arena.alloc(Ast.TypedLocal, source_args.len);
        for (source_args, 0..) |source_arg, index| {
            const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), source_arg.ty);
            args[index] = .{ .local = local, .ty = source_arg.ty };
        }
        task.args_span = try self.pass.program.addTypedLocalSpan(args);

        // Reserve and index the worker before cloning. Recursive references
        // therefore reuse this exact function id, and cloning can never start
        // from a worker produced by an earlier materialization.
        task.symbol = self.pass.symbols.fresh();
        var worker_source = source_fn.source orelse Common.invariant("callable worker lacks frozen source authority");
        worker_source.frozen_worker = worker_key.template.bytes ++ worker_key.callable_abi.bytes ++ worker_key.capture_abi.bytes;
        task.worker_fn_id = try self.pass.program.addFn(.{
            .symbol = task.symbol,
            .source = worker_source,
            .signature = null,
            .args = task.args_span,
            .captures = task.captures_span,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        try self.pass.callable_workers.put(worker_key, task.worker_fn_id);
        try self.pass.callable_sources.put(task.worker_fn_id, source_fn_id);
        try self.pass.copyProcDebugName(source_fn.symbol, task.symbol);

        task.change_start = self.subst.watermark();

        for (source_captures, worker_body_values) |source_capture, capture_value| {
            try self.subst.put(self.pass.program, source_capture.local, capture_value);
            // Different Monotype specializations of one lexical capture can
            // leave distinct local ids with the same binder and monomorphic
            // type in a callable template. A shared callable worker has one
            // dynamic slot for that identity, so clone every equivalent use
            // through the selected source capture local.
        }
        for (source_args, args) |source_arg, arg| {
            const arg_expr = try self.addExpr(.{
                .ty = arg.ty,
                .data = .{ .local = arg.local },
            });
            try self.subst.put(self.pass.program, source_arg.local, .{ .expr = arg_expr });
        }

        // The worker body is a fresh value tree, not a continuation of the
        // capture chain that reached this worker, so its own materializations
        // start their strip depth from zero.
        task.saved_strip_depth = self.materialize_strip_depth;
        self.materialize_strip_depth = 0;
        task.outer_shapes = self.pass.program.beginFnShapes(task.worker_fn_id);
        frame.cursor = 1;
        return .{ .call = try self.withoutReuseTask(.{ .expr = source_body }) };
    }

    const MaterializeWithCapturesTask = struct {
        ty: Type.TypeId,
        fn_id: Ast.FnId,
        captures_span: Ast.Span(Ast.TypedLocal),
        values: []const CaptureValue,
        captures: []const Ast.TypedLocal = &.{},
        operands: []Ast.CaptureOperand = &.{},
    };

    fn stepMaterializeWithCaptures(self: *Cloner, frame: *CloneFrame, task: *MaterializeWithCapturesTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            task.captures = try GuardedList.dupe(self.arena.allocator(), Ast.TypedLocal, self.pass.program.typedLocalSpan(task.captures_span));
            if (task.captures.len != task.values.len) {
                Common.invariant("callable value capture count differed from specialized function capture count");
            }
            task.operands = try self.arena.allocator().alloc(Ast.CaptureOperand, task.captures.len);
            frame.cursor = 1;
        } else {
            const capture = task.captures[frame.index];
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const producer = input.?.get(.expr);
            const producer_ty = self.pass.program.getExpr(producer).ty;
            const value_expr = if (sameType(self.pass.program, capture.ty, producer_ty))
                producer
            else
                try self.addExpr(.{ .ty = capture.ty, .data = .{ .typed_boundary = .{ .value = producer } } });
            self.materialize_strip_depth -= 1;
            const value_local = localExpr(self.pass.program, value_expr);
            const operand_value = if (value_local != null and value_local.? == capture.local)
                try self.addExpr(.{ .ty = capture.ty, .data = .{ .local = capture.local } })
            else
                value_expr;
            task.operands[frame.index] = .{ .id = id, .value = operand_value };
            frame.index += 1;
        }
        if (frame.index < task.captures.len) {
            const capture = task.captures[frame.index];
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const value = callableCaptureValueForId(task.values, id) orelse
                Common.invariant("specialized callable had no value for a capture slot");
            if (self.materialize_strip_depth >= value_wrapper_strip_cap) {
                Common.invariant("materialize followed a callable capture chain past the strip cap; a cyclic value reached materialization");
            }
            self.materialize_strip_depth += 1;
            return .{ .call = .{ .materialize = .{ .value = value } } };
        }
        return retExpr(try self.addExpr(.{ .ty = task.ty, .data = .{ .fn_ref = .{
            .fn_id = task.fn_id,
            .captures = try self.pass.program.addCaptureOperandSpan(task.operands),
        } } }));
    }

    /// Bind a pattern for value flow: to the value itself when it can
    /// substitute, else to a reuse-safe rebinding unless the source value
    /// refers to its own binder.
    const ValueFlowBindTask = struct {
        pat: Ast.PatId,
        source_value: Ast.ExprId,
        recursive: bool,
        value: Value,
        bindings: *BindingChain,
        change_before: usize = 0,
        bindings_before: ?*BindingNode = null,
    };

    fn stepValueFlowBind(self: *Cloner, frame: *CloneFrame, task: *ValueFlowBindTask, input: ?CloneResult) Common.LowerError!CloneStep {
        if (frame.cursor == 0) {
            task.change_before = self.subst.watermark();
            task.bindings_before = task.bindings.mark();
            if (try self.bindPatToReusableValue(task.pat, task.value) == .match) return .{ .ret = .{ .flag = true } };
            self.subst.restore(task.change_before);
            task.bindings.rewind(task.bindings_before);

            // A binding refers to itself exactly when its producer marked it
            // recursive.
            const self_referential = task.recursive;
            if (self_referential) return .{ .ret = .{ .flag = false } };

            frame.cursor = 1;
            return .{ .call = try self.makeReusableTask(task.value, task.bindings) };
        }
        if (try self.bindPatToFlowValue(task.pat, input.?.get(.value))) return .{ .ret = .{ .flag = true } };
        self.subst.restore(task.change_before);
        task.bindings.rewind(task.bindings_before);
        return .{ .ret = .{ .flag = false } };
    }

    /// Discover the call patterns a body's direct calls would specialize to,
    /// flowing known values through its bindings. The walk keeps its own work
    /// list in source order; value clones it needs run as child frames.
    const CollectTask = struct {
        owner: Ast.FnId,
        expr: Ast.ExprId,
        items: std.ArrayList(CollectItem) = .empty,
        scopes: std.ArrayList(usize) = .empty,
        /// The item whose value clones are in flight.
        pending: ?CollectPending = null,
    };

    const CollectItem = union(enum) {
        expr: Ast.ExprId,
        stmt: Ast.StmtId,
        /// Open a substitution scope, restored by the matching `close_scope`.
        open_scope,
        close_scope,
        shadow_local: Ast.LocalId,
        shadow_pat: Ast.PatId,
        /// Flow a binding's value into its pattern: `let_` opens its own
        /// scope for the rest, a statement binds into the enclosing one.
        bind_value: struct { pat: Ast.PatId, value: Ast.ExprId, recursive: bool },
        /// Record the call pattern of a direct call whose operands were
        /// collected.
        call_pattern: Ast.ExprId,
    };

    const CollectPending = union(enum) {
        bind_value: struct { pat: Ast.PatId, value: Ast.ExprId, recursive: bool, cloned: ClonedValue = undefined, chain: *BindingChain = undefined },
        call_pattern: struct { callee: Ast.FnId, args: []const Ast.ExprId, values: []Value },
    };

    fn stepCollect(self: *Cloner, frame: *CloneFrame, task: *CollectTask, input: ?CloneResult) Common.LowerError!CloneStep {
        const allocator = self.pass.allocator;
        if (frame.cursor == 0) {
            try task.items.append(allocator, .{ .expr = task.expr });
            frame.cursor = 1;
        } else if (task.pending) |*pending| {
            switch (pending.*) {
                .bind_value => |*bind| {
                    if (frame.cursor == 1) {
                        bind.cloned = input.?.get(.cloned);
                        bind.chain = try self.newChain();
                        bind.chain.* = bind.cloned.bindings;
                        frame.cursor = 2;
                        return .{ .call = .{ .value_flow_bind = .{
                            .pat = bind.pat,
                            .source_value = bind.value,
                            .recursive = bind.recursive,
                            .value = bind.cloned.value,
                            .bindings = bind.chain,
                        } } };
                    }
                    if (!input.?.get(.flag)) try self.shadowPatLocals(bind.pat);
                    task.pending = null;
                    frame.cursor = 1;
                },
                .call_pattern => |*call| {
                    call.values[frame.index] = input.?.get(.cloned).value;
                    frame.index += 1;
                    if (frame.index < call.args.len) return self.collectCallArg(frame, call.callee, call.args);
                    try self.pass.recordCallPatternForValues(call.callee, call.values);
                    task.pending = null;
                },
            }
        }
        while (task.items.pop()) |item| {
            // An item appends the items it expands to in source order, then
            // they are reversed onto the list.
            const start = task.items.items.len;
            switch (item) {
                .open_scope => try task.scopes.append(allocator, self.subst.watermark()),
                .close_scope => self.subst.restore(task.scopes.pop() orelse Common.invariant("call-pattern collection closed a scope it never opened")),
                .shadow_local => |local| try self.shadowLocal(local),
                .shadow_pat => |pat| try self.shadowPatLocals(pat),
                .bind_value => |bind| {
                    task.pending = .{ .bind_value = .{ .pat = bind.pat, .value = bind.value, .recursive = bind.recursive } };
                    frame.cursor = 1;
                    return .{ .call = .{ .expr_value_owned = .{ .expr = bind.value, .demand_shape = false } } };
                },
                .call_pattern => |expr_id| {
                    const call = self.pass.program.getExpr(expr_id).data.call_proc;
                    const callee = Ast.localDirectCallee(call) orelse continue;
                    const callee_raw = @intFromEnum(callee);
                    if (callee_raw >= self.pass.plans.len) continue;
                    if (self.pass.newSpecAdmission(callee_raw) != .admitted) continue;

                    const args = try GuardedList.dupe(self.arena.allocator(), Ast.ExprId, self.pass.program.exprSpan(call.args));
                    const values = try self.arena.allocator().alloc(Value, args.len);
                    if (args.len == 0) {
                        try self.pass.recordCallPatternForValues(callee, values);
                        continue;
                    }
                    task.pending = .{ .call_pattern = .{ .callee = callee, .args = args, .values = values } };
                    frame.index = 0;
                    return self.collectCallArg(frame, callee, args);
                },
                .stmt => |stmt_id| switch (self.pass.program.getStmt(stmt_id)) {
                    .let_ => |let_| {
                        try task.items.append(allocator, .{ .expr = let_.value });
                        try task.items.append(allocator, .{ .bind_value = .{ .pat = let_.pat, .value = let_.value, .recursive = let_.recursive } });
                    },
                    .expr,
                    .expect,
                    .dbg,
                    => |expr| try task.items.append(allocator, .{ .expr = expr }),
                    .return_ => |ret| try task.items.append(allocator, .{ .expr = ret.value }),
                    .uninitialized => |pat| try task.items.append(allocator, .{ .shadow_pat = pat }),
                    .crash, .checked_error => {},
                },
                .expr => |expr_id| try self.expandCollectExpr(task, expr_id),
            }
            std.mem.reverse(CollectItem, task.items.items[start..]);
        }
        task.items.deinit(allocator);
        task.scopes.deinit(allocator);
        return .{ .ret = .none };
    }

    fn collectCallArg(self: *Cloner, frame: *CloneFrame, callee: Ast.FnId, args: []const Ast.ExprId) CloneStep {
        const callee_raw = @intFromEnum(callee);
        const index = frame.index;
        const demand_shape = callee_raw < self.pass.plans.len and
            index < self.pass.plans[callee_raw].used_args.len and
            self.pass.plans[callee_raw].used_args[index];
        return .{ .call = .{ .expr_value_owned = .{ .expr = args[index], .demand_shape = demand_shape } } };
    }

    fn collectExprSpan(self: *Cloner, task: *CollectTask, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        const exprs = self.pass.program.exprSpan(span);
        for (0..exprs.len) |index| try task.items.append(self.pass.allocator, .{ .expr = GuardedList.at(exprs, index) });
    }

    fn collectCaptureOperands(self: *Cloner, task: *CollectTask, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        const operands = self.pass.program.captureOperandSpan(span);
        for (0..operands.len) |index| try task.items.append(self.pass.allocator, .{ .expr = GuardedList.at(operands, index).value });
    }

    fn collectFieldExprs(self: *Cloner, task: *CollectTask, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        const fields = self.pass.program.fieldExprSpan(span);
        for (0..fields.len) |index| try task.items.append(self.pass.allocator, .{ .expr = GuardedList.at(fields, index).value });
    }

    fn collectStmts(self: *Cloner, task: *CollectTask, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        const stmts = self.pass.program.stmtSpan(span);
        for (0..stmts.len) |index| try task.items.append(self.pass.allocator, .{ .stmt = GuardedList.at(stmts, index) });
    }

    fn expandCollectExpr(self: *Cloner, task: *CollectTask, expr_id: Ast.ExprId) Allocator.Error!void {
        const allocator = self.pass.allocator;
        const items = &task.items;
        switch (self.pass.program.getExpr(expr_id).data) {
            .@"unreachable",
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            .comptime_value,
            => {},
            .fn_ref => |fn_ref| try self.collectCaptureOperands(task, fn_ref.captures),
            .list,
            .tuple,
            => |exprs| try self.collectExprSpan(task, exprs),
            .record => |fields| try self.collectFieldExprs(task, fields),
            .record_update => |update| {
                try items.append(allocator, .{ .expr = update.base });
                try self.collectFieldExprs(task, update.fields);
            },
            .tag => |tag| try self.collectExprSpan(task, tag.payloads),
            .static_data_candidate => |candidate| try items.append(allocator, .{ .expr = candidate.runtime_expr }),
            .typed_boundary => |boundary| try items.append(allocator, .{ .expr = boundary.value }),
            .nominal,
            .dbg,
            .expect,
            => |child| try items.append(allocator, .{ .expr = child }),
            .return_ => |ret| try items.append(allocator, .{ .expr = ret.value }),
            .expect_err => |expect_err| try items.append(allocator, .{ .expr = expect_err.msg }),
            .literal_rejected => |rejected| try items.append(allocator, .{ .expr = rejected.msg }),
            .comptime_branch_taken => |taken| try items.append(allocator, .{ .expr = taken.body }),
            .let_ => |let_| {
                try items.append(allocator, .{ .expr = let_.value });
                try items.append(allocator, .open_scope);
                try items.append(allocator, .{ .bind_value = .{ .pat = let_.bind, .value = let_.value, .recursive = false } });
                try items.append(allocator, .{ .expr = let_.rest });
                try items.append(allocator, .close_scope);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .call_value => |call| {
                try items.append(allocator, .{ .expr = call.callee });
                try self.collectExprSpan(task, call.args);
            },
            .call_proc => |call| {
                try self.collectExprSpan(task, call.args);
                try self.collectCaptureOperands(task, call.captures);
                try items.append(allocator, .{ .call_pattern = expr_id });
            },
            .low_level => |call| try self.collectExprSpan(task, call.args),
            .field_access => |field| try items.append(allocator, .{ .expr = field.receiver }),
            .tuple_access => |access| try items.append(allocator, .{ .expr = access.tuple }),
            .structural_eq => |eq| {
                try items.append(allocator, .{ .expr = eq.lhs });
                try items.append(allocator, .{ .expr = eq.rhs });
            },
            .structural_hash => |h| {
                try items.append(allocator, .{ .expr = h.value });
                try items.append(allocator, .{ .expr = h.hasher });
            },
            .match_ => |match| {
                try items.append(allocator, .{ .expr = match.scrutinee });
                const branches = self.pass.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    try items.append(allocator, .open_scope);
                    try items.append(allocator, .{ .shadow_pat = branch.pat });
                    try self.collectStmts(task, branch.bindings);
                    if (branch.guard) |guard| try items.append(allocator, .{ .expr = guard });
                    try items.append(allocator, .{ .expr = branch.body });
                    try items.append(allocator, .close_scope);
                }
            },
            .if_ => |if_| {
                const branches = self.pass.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    try items.append(allocator, .{ .expr = branch.cond });
                    try items.append(allocator, .{ .expr = branch.body });
                }
                try items.append(allocator, .{ .expr = if_.final_else });
            },
            .block => |block| {
                try items.append(allocator, .open_scope);
                try self.collectStmts(task, block.statements);
                try items.append(allocator, .{ .expr = block.final_expr });
                try items.append(allocator, .close_scope);
            },
            .loop_ => |loop| {
                try self.collectExprSpan(task, loop.initial_values);
                try items.append(allocator, .open_scope);
                const params = self.pass.program.typedLocalSpan(loop.params);
                for (0..params.len) |index| try items.append(allocator, .{ .shadow_local = GuardedList.at(params, index).local });
                try items.append(allocator, .{ .expr = loop.body });
                try items.append(allocator, .close_scope);
            },
            .break_ => |maybe| if (maybe) |value| try items.append(allocator, .{ .expr = value }),
            .continue_ => |continue_| try self.collectExprSpan(task, continue_.values),
            .join_point => |join_point| {
                try items.append(allocator, .open_scope);
                const params = self.pass.program.typedLocalSpan(join_point.params);
                for (0..params.len) |index| try items.append(allocator, .{ .shadow_local = GuardedList.at(params, index).local });
                try items.append(allocator, .{ .expr = join_point.body });
                try items.append(allocator, .close_scope);
                try items.append(allocator, .{ .expr = join_point.remainder });
            },
            .jump => |jump| {
                try self.collectExprSpan(task, jump.loop_values);
                try self.collectExprSpan(task, jump.args);
            },
            .if_initialized_payload => |payload_switch| {
                try items.append(allocator, .{ .expr = payload_switch.cond });
                try items.append(allocator, .{ .expr = payload_switch.initialized });
                try items.append(allocator, .{ .expr = payload_switch.uninitialized });
            },
            .try_sequence => |sequence| {
                try items.append(allocator, .{ .expr = sequence.try_expr });
                try items.append(allocator, .open_scope);
                try items.append(allocator, .{ .shadow_local = sequence.ok_local });
                try items.append(allocator, .{ .expr = sequence.ok_body });
                try items.append(allocator, .close_scope);
            },
            .try_record_sequence => |sequence| {
                try items.append(allocator, .{ .expr = sequence.try_expr });
                try items.append(allocator, .open_scope);
                try items.append(allocator, .{ .shadow_local = sequence.value_local });
                try items.append(allocator, .{ .shadow_local = sequence.rest_local });
                try items.append(allocator, .{ .expr = sequence.ok_body });
                try items.append(allocator, .close_scope);
            },
        }
    }

    fn buildArgs(self: *Cloner) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        const source_fn = self.pass.program.getFn(self.source_fn);
        const source_body = self.pass.sourceBody(self.source_fn);
        const source_args = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        if (source_args.len != self.pattern.args.len) Common.invariant("call-pattern argument count differed from source function arity");
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        self.current_loc = switch (source_body) {
            .roc => |body| self.pass.program.exprLoc(body),
            .hosted => SourceLoc.none,
        };
        self.current_region = switch (source_body) {
            .roc => |body| self.pass.program.exprRegion(body),
            .hosted => Region.zero(),
        };

        var args = std.ArrayList(Ast.TypedLocal).empty;
        defer args.deinit(self.pass.allocator);

        for (source_args, self.pattern.args) |source_arg, shape| {
            const value = try self.valueFromShapeArgs(shape, &args);
            try self.subst.put(self.pass.program, source_arg.local, value);
        }

        return try self.pass.program.addTypedLocalSpan(args.items);
    }

    /// The value a shape describes, with a fresh argument local for each
    /// `.any` position, appended to `args` in shape order. Shapes nest as
    /// deeply as their constructors, so positions are filled from a
    /// worklist, last-first so the first is filled next.
    fn valueFromShapeArgs(self: *Cloner, root_shape: Shape, args: *std.ArrayList(Ast.TypedLocal)) Allocator.Error!Value {
        const Pending = struct { shape: Shape, target: *Value };
        var result: Value = undefined;
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.pass.allocator);
        try pending.append(self.pass.allocator, .{ .shape = root_shape, .target = &result });
        const arena = self.arena.allocator();
        while (pending.pop()) |item| {
            const mark = pending.items.len;
            item.target.* = switch (item.shape) {
                .any => |ty| blk: {
                    const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                    try args.append(self.pass.allocator, .{ .local = local, .ty = ty });
                    break :blk .{ .expr = try self.addExpr(.{
                        .ty = ty,
                        .data = .{ .local = local },
                    }) };
                },
                .tag => |tag| blk: {
                    const payloads = try arena.alloc(Value, tag.payloads.len);
                    for (tag.payloads, payloads) |payload, *target| try pending.append(self.pass.allocator, .{ .shape = payload, .target = target });
                    break :blk .{ .tag = .{
                        .ty = tag.ty,
                        .name = tag.name,
                        .payloads = payloads,
                    } };
                },
                .record => |record| blk: {
                    const fields = try arena.alloc(FieldValue, record.fields.len);
                    for (record.fields, fields) |field, *target| {
                        target.name = field.name;
                        try pending.append(self.pass.allocator, .{ .shape = field.shape, .target = &target.value });
                    }
                    break :blk .{ .record = .{
                        .ty = record.ty,
                        .fields = fields,
                    } };
                },
                .tuple => |tuple| blk: {
                    const items = try arena.alloc(Value, tuple.items.len);
                    for (tuple.items, items) |item_shape, *target| try pending.append(self.pass.allocator, .{ .shape = item_shape, .target = target });
                    break :blk .{ .tuple = .{
                        .ty = tuple.ty,
                        .items = items,
                    } };
                },
                .nominal => |nominal| blk: {
                    const backing = try arena.create(Value);
                    try pending.append(self.pass.allocator, .{ .shape = nominal.backing.*, .target = backing });
                    break :blk .{ .nominal = .{
                        .ty = nominal.ty,
                        .backing = backing,
                    } };
                },
                .callable => |callable| blk: {
                    // A callable shape's captures are parallel, in ascending
                    // CaptureId order, to its function's sorted capture slots, so we
                    // read each capture's CaptureId from the matching slot.
                    const slots = self.pass.program.typedLocalSpan(self.pass.program.getFn(callable.fn_id).captures);
                    if (slots.len != callable.captures.len) {
                        Common.invariant("callable shape capture count differed from its function capture slots");
                    }
                    const captures = try arena.alloc(CaptureValue, callable.captures.len);
                    for (captures, callable.captures, 0..) |*target, capture, index| {
                        target.id = self.pass.program.captureIdOfLocal(GuardedList.at(slots, index).local);
                        try pending.append(self.pass.allocator, .{ .shape = capture, .target = &target.value });
                    }
                    break :blk .{ .callable = .{
                        .ty = callable.ty,
                        .fn_id = callable.fn_id,
                        .captures = captures,
                    } };
                },
            };
            std.mem.reverse(Pending, pending.items[mark..]);
        }
        return result;
    }

    fn directCallHasKnownShapeArg(self: *Cloner, args_span: Ast.Span(Ast.ExprId)) Allocator.Error!bool {
        const args = self.pass.program.exprSpan(args_span);
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (try self.exprHasKnownShape(arg)) return true;
        }
        return false;
    }

    /// Whether any capture operand of a direct call would clone to something
    /// other than the callee's own capture local—i.e. the call sits in a
    /// context where the captured bindings have been substituted.
    fn callCapturesAreForeign(self: *Cloner, captures_span: Ast.Span(Ast.CaptureOperand)) bool {
        const operands = self.pass.program.captureOperandSpan(captures_span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            const local = localExpr(self.pass.program, operand.value) orelse return true;
            const substituted = self.subst.get(self.pass.program, local) orelse continue;
            if (substituted != .expr) return true;
            const substituted_expr = substituted.expr;
            if (localExpr(self.pass.program, substituted_expr) != local) return true;
        }
        return false;
    }

    fn exprHasKnownShape(self: *Cloner, source_expr_id: Ast.ExprId) Allocator.Error!bool {
        var expr_id = source_expr_id;
        // Transparent wrappers are unwrapped iteratively.
        while (true) {
            switch (self.pass.program.getExpr(expr_id).data) {
                .static_data_candidate => |candidate| expr_id = candidate.runtime_expr,
                .typed_boundary => |boundary| expr_id = boundary.value,
                .comptime_branch_taken => |taken| expr_id = taken.body,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => break,
            }
        }
        const expr = self.pass.program.getExpr(expr_id);
        // These probes read the exact-local map only, not the binder-wide map:
        // they ask whether this specific local was directly substituted with a
        // known-shaped value here, so a binder-wide entry installed for a
        // sibling of the same binder must not answer for it.
        return switch (expr.data) {
            .local => |local| if (self.subst.getExact(local)) |value|
                shapeProofIsProven(try self.pass.shapeFromValue(value))
            else
                false,
            .tag,
            .record,
            .record_update,
            .tuple,
            .nominal,
            .fn_ref,
            => (try self.pass.constructorShape(expr_id)) != null,
            .list, .str_lit, .bytes_lit => false,
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk false;
                const receiver = self.subst.getExact(receiver_local) orelse break :blk false;
                const value = fieldPathFromValue(
                    self.pass.program,
                    receiver,
                    self.pass.program.fieldAccessSegmentSpan(field.segments),
                ) orelse break :blk false;
                break :blk shapeProofIsProven(try self.pass.shapeFromValue(value));
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk false;
                const tuple = self.subst.getExact(tuple_local) orelse break :blk false;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk false;
                break :blk shapeProofIsProven(try self.pass.shapeFromValue(value));
            },
            .static_data_candidate,
            .typed_boundary,
            .comptime_branch_taken,
            => Common.invariant("known-shape probe did not unwrap a transparent wrapper"),
            .comptime_value => false,
            .comptime_exhaustiveness_failed => false,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .call_value,
            .call_proc,
            .low_level,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => false,
        };
    }

    /// Total work budget for walking one substitution-candidate value.
    ///
    /// A known value is not always a small finite tree. A loop-carried value
    /// can reference itself through the fixpoint of a recursive construction
    /// (e.g. an iterator wrapped around itself a runtime number of times,
    /// where the step callable's capture reaches the nominal whose backing
    /// reaches the callable again), and a deep statically-built chain shares
    /// substructure between levels, so a per-level depth budget still permits
    /// combinatorially many paths through the shared nodes. The budget is
    /// therefore spent per NODE VISIT—one shared counter across the whole
    /// walk—which bounds total work absolutely for cycles and shared
    /// structure alike. See design.md "Core Principles" on bounded post-check
    /// walks.
    ///
    /// A work budget is the right bound here, rather than a visited set,
    /// because this predicate is allowed to answer "no" spuriously: declining
    /// a substitution keeps the construction materialized, which is a missed
    /// optimization and never a miscompile. A cyclic value exhausts the
    /// budget and gets "no"—the correct answer, since a self-referential
    /// value cannot be substituted anyway—and a value large enough to
    /// exhaust it honestly is one whose substitution would bloat the clone
    /// regardless. Value identity is also too murky for a reliable visited
    /// set: values are by-value unions holding slices, with only the nominal
    /// backing behind a stable pointer.
    const value_substitute_work_budget: u32 = 4096;

    fn valueCanSubstitute(self: *Cloner, value: Value) Allocator.Error!ProofStatus {
        var budget: u32 = value_substitute_work_budget;
        return self.valueCanSubstituteBudgeted(value, &budget);
    }

    /// Values are visited depth-first in order on a work stack; any
    /// disproven node disproves the whole.
    fn valueCanSubstituteBudgeted(self: *Cloner, root: Value, budget: *u32) Allocator.Error!ProofStatus {
        const allocator = self.pass.allocator;
        var proof = ProofStatus.proven;
        var stack: std.ArrayList(Value) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, root);
        while (stack.pop()) |value| {
            if (budget.* == 0) {
                proof = .unknown_budget_exhausted;
                continue;
            }
            budget.* -= 1;
            // Children are appended in order, then reversed.
            const start = stack.items.len;
            switch (value) {
                .expr => |expr| if (!try self.exprCanSubstitute(expr)) return .disproven,
                .runtime_anchor => |anchor| if (!try self.exprCanSubstitute(anchor.runtime)) return .disproven,
                .static_data_candidate => {},
                .tag => |tag| try stack.appendSlice(allocator, tag.payloads),
                .record => |record| for (record.fields) |field| try stack.append(allocator, field.value),
                .tuple => |tuple| try stack.appendSlice(allocator, tuple.items),
                .nominal => |nominal| try stack.append(allocator, nominal.backing.*),
                .callable => |callable| for (callable.captures) |capture| try stack.append(allocator, capture.value),
            }
            std.mem.reverse(Value, stack.items[start..]);
        }
        return proof;
    }

    /// Whether `expr_id` is a substitutable read: a leaf, or an access or
    /// callable whose operands all are. Operands are checked on a work stack.
    fn exprCanSubstitute(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!bool {
        const program = self.pass.program;
        var stack: std.ArrayList(Ast.ExprId) = .empty;
        defer stack.deinit(self.pass.allocator);
        try stack.append(self.pass.allocator, expr_id);
        while (stack.pop()) |id| switch (program.getExpr(id).data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .static_data_candidate,
            .comptime_value,
            => {},
            .fn_ref => |fn_ref| {
                const operand_count: usize = @intCast(fn_ref.captures.len);
                for (0..operand_count) |index| {
                    try stack.append(self.pass.allocator, program.captureOperandAt(fn_ref.captures, index).value);
                }
            },
            .field_access => |field| try stack.append(self.pass.allocator, field.receiver),
            .tuple_access => |access| try stack.append(self.pass.allocator, access.tuple),
            .typed_boundary,
            .@"unreachable",
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .nominal,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .call_value,
            .call_proc,
            .low_level,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => return false,
        };
        return true;
    }

    fn canReuseOriginalExpr(self: *const Cloner, expr_id: Ast.ExprId) bool {
        if (self.source_reuse != .original_body or self.inline_stack.items.len != 0) return false;

        // Reuse is valid only when cloning would preserve the source metadata.
        // Missing child metadata otherwise inherits the surrounding clone's
        // location, region, or inline scope.
        return std.meta.eql(self.current_loc, self.pass.program.exprLoc(expr_id)) and
            std.meta.eql(self.current_region, self.pass.program.exprRegion(expr_id)) and
            self.current_inline_scope == self.pass.program.exprInlineScope(expr_id);
    }

    fn valueRetainsExpr(value: Value, expr_id: Ast.ExprId) bool {
        return switch (value) {
            .expr => |expr| expr == expr_id,
            .runtime_anchor => |anchor| anchor.runtime == expr_id,
            .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => false,
        };
    }

    /// At a non-demanding materialization site, preserve a source constructor
    /// only when symbolic analysis left its complete runtime tree unchanged.
    /// Whether a value is exactly the source expression's construction:
    /// every component must be. Components are compared from a worklist; a
    /// component past the wrapper-strip depth declines the match.
    fn valueMatchesSourceExpr(
        self: *const Cloner,
        root_value: Value,
        root_expr: Ast.ExprId,
        root_depth: usize,
    ) Allocator.Error!bool {
        const Pair = struct { value: Value, expr_id: Ast.ExprId, depth: usize };
        var pending = std.ArrayList(Pair).empty;
        defer pending.deinit(self.pass.allocator);
        try pending.append(self.pass.allocator, .{ .value = root_value, .expr_id = root_expr, .depth = root_depth });
        while (pending.pop()) |pair| {
            const value = pair.value;
            const expr_id = pair.expr_id;
            const depth = pair.depth;
            if (depth >= value_wrapper_strip_cap) return false;
            if (value == .expr) {
                if (value.expr != expr_id) return false;
                continue;
            }

            const expr = self.pass.program.getExpr(expr_id);
            switch (value) {
                .runtime_anchor, .callable => return false,
                .static_data_candidate => |candidate| {
                    if (expr.data != .static_data_candidate) return false;
                    const source = expr.data.static_data_candidate;
                    if (!(candidate.ty == expr.ty and
                        candidate.static_data == source.static_data and
                        candidate.expr == expr_id)) return false;
                },
                .tag => |tag| {
                    if (expr.data != .tag) return false;
                    const source = expr.data.tag;
                    if (tag.ty != expr.ty or tag.name != source.name) return false;
                    const payloads = self.pass.program.exprSpan(source.payloads);
                    if (tag.payloads.len != payloads.len) return false;
                    for (0..payloads.len) |index| {
                        try pending.append(self.pass.allocator, .{ .value = tag.payloads[index], .expr_id = GuardedList.at(payloads, index), .depth = depth + 1 });
                    }
                },
                .record => |record| {
                    if (expr.data != .record) return false;
                    const fields = self.pass.program.fieldExprSpan(expr.data.record);
                    if (record.ty != expr.ty or record.fields.len != fields.len) return false;
                    for (0..fields.len) |index| {
                        const source = GuardedList.at(fields, index);
                        const field = record.fields[index];
                        if (field.name != source.name) return false;
                        try pending.append(self.pass.allocator, .{ .value = field.value, .expr_id = source.value, .depth = depth + 1 });
                    }
                },
                .tuple => |tuple| {
                    if (expr.data != .tuple) return false;
                    const items = self.pass.program.exprSpan(expr.data.tuple);
                    if (tuple.ty != expr.ty or tuple.items.len != items.len) return false;
                    for (0..items.len) |index| {
                        try pending.append(self.pass.allocator, .{ .value = tuple.items[index], .expr_id = GuardedList.at(items, index), .depth = depth + 1 });
                    }
                },
                .nominal => |nominal| {
                    if (expr.data != .nominal or nominal.ty != expr.ty) return false;
                    try pending.append(self.pass.allocator, .{ .value = nominal.backing.*, .expr_id = expr.data.nominal, .depth = depth + 1 });
                },
                .expr => unreachable,
            }
        }
        return true;
    }

    fn plainExprCanReuse(data: Ast.ExprData) bool {
        return switch (data) {
            .list,
            .record_update,
            .low_level,
            .structural_eq,
            .structural_hash,
            .if_,
            .return_,
            .comptime_branch_taken,
            .dbg,
            .expect_err,
            .literal_rejected,
            .expect,
            => true,
            .@"unreachable",
            .local,
            .unit,
            .uninitialized,
            .uninitialized_payload,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .tuple,
            .record,
            .tag,
            .static_data_candidate,
            .comptime_value,
            .typed_boundary,
            .nominal,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .fn_ref,
            .call_value,
            .call_proc,
            .field_access,
            .tuple_access,
            .match_,
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            => false,
        };
    }

    fn clonedJoinTarget(self: *Cloner, source: Ast.JoinPointId) Ast.JoinPointId {
        var index = self.join_stack.items.len;
        while (index > 0) {
            index -= 1;
            const join_point = self.join_stack.items[index];
            if (join_point.source == source) return join_point.target;
        }
        // Not being remapped: the join's definition encloses the region being
        // cloned rather than sitting inside it. A let-of-case dispatch
        // template jumps to joins that rewrite defines around every copy of
        // the template, and such a jump must keep aiming at the enclosing
        // definition. Join ids are minted from one pass-wide counter, so the
        // id cannot collide with a different join.
        return source;
    }

    const LoopAttemptMark = struct {
        analysis: Pass.AnalysisMark,
        callable_workers: usize,
        rebased_inline_scope_changes: usize,
        recorded_value_keys: usize,
        clone_templates: usize,
        let_case_depth: usize,
    };

    fn markLoopAttempt(self: *Cloner) LoopAttemptMark {
        std.debug.assert(self.purpose != .loop_exit_selection);
        return .{
            .analysis = self.pass.markAnalysis(),
            .callable_workers = self.pass.callable_workers.count(),
            .rebased_inline_scope_changes = self.rebased_inline_scope_changes.items.len,
            .recorded_value_keys = self.recorded_value_keys.items.len,
            .clone_templates = self.clone_templates.items.len,
            .let_case_depth = self.let_case_builds.items.len,
        };
    }

    /// Rewind a rejected loop attempt when all of its output is still private to
    /// that attempt. Callable workers and active let-of-case joins can retain
    /// or mutate references outside the append-only program suffix, so those
    /// attempts keep their unreachable output. Exit selection never retries.
    fn rewindLoopAttempt(
        self: *Cloner,
        mark: LoopAttemptMark,
    ) void {
        if (mark.let_case_depth != 0 or
            self.pass.callable_workers.count() != mark.callable_workers)
        {
            return;
        }

        while (self.rebased_inline_scope_changes.items.len > mark.rebased_inline_scope_changes) {
            const key = self.rebased_inline_scope_changes.pop() orelse
                Common.invariant("inline-scope rebase change log underflow");
            const removed = self.rebased_inline_scopes.fetchRemove(key) orelse
                Common.invariant("inline-scope rebase change had no cache entry");
            if (!self.inline_scope_origins.remove(removed.value)) {
                Common.invariant("inline-scope rebase change had no origin entry");
            }
        }

        // The rewind reuses the attempt's expression ids: every value recorded
        // for them, and every template range covering them, must go with it.
        while (self.recorded_value_keys.items.len > mark.recorded_value_keys) {
            const key = self.recorded_value_keys.pop() orelse
                Common.invariant("recorded value change log underflow");
            const removed = switch (key) {
                .arm_values => |emitted| self.arm_values.remove(emitted),
                .block_tail => |emitted| self.block_tail_values.remove(emitted),
            };
            if (!removed) Common.invariant("recorded value change had no map entry");
        }
        self.clone_templates.shrinkRetainingCapacity(mark.clone_templates);

        self.pass.rewindAnalysis(mark.analysis);
    }

    fn currentLoopExitSelection(self: *Cloner) ?LoopExitSelection {
        if (self.loop_exit_stack.items.len == 0) return null;
        return self.loop_exit_stack.items[self.loop_exit_stack.items.len - 1];
    }

    fn caseExprFromValue(self: *Cloner, value: Value) ?Ast.ExprId {
        const candidate = switch (value) {
            .expr => |expr| expr,
            .runtime_anchor => |anchor| anchor.runtime,
            .static_data_candidate => |static_candidate| switch (static_candidate.structure.*) {
                .expr => |runtime| runtime,
                .runtime_anchor => |anchor| anchor.runtime,
                .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => return null,
            },
            .tag, .record, .tuple, .nominal, .callable => return null,
        };
        const candidate_data = self.pass.program.getExpr(candidate).data;
        if (candidate_data != .if_ and candidate_data != .match_) return null;
        return candidate;
    }

    const let_case_shape_arm_budget: usize = 4096;

    /// Decide the join layout for a let-of-case rewrite: one join per branch
    /// of a continuation that immediately matches the bound value (so the
    /// dispatch can fold at each arm), otherwise one join owning the whole
    /// continuation.
    fn letCaseJoinPlan(self: *Cloner, let_: anytype, arena: Allocator) Common.LowerError![]LetCaseJoin {
        dispatch_split: {
            const bind_data = self.pass.program.getPat(let_.bind).data;
            if (bind_data != .bind) break :dispatch_split;
            const bind_local = bind_data.bind;
            const rest_data = self.pass.program.getExpr(let_.rest).data;
            if (rest_data != .match_) break :dispatch_split;
            const rest_match = rest_data.match_;
            const scrutinee_local = localExpr(self.pass.program, rest_match.scrutinee) orelse break :dispatch_split;
            if (scrutinee_local != bind_local) break :dispatch_split;
            if (try localUseCountInExpr(self.pass.allocator, self.pass.program, bind_local, let_.rest) != 1) break :dispatch_split;

            const branches = self.pass.program.branchSpan(rest_match.branches);
            const joins = try arena.alloc(LetCaseJoin, branches.len);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (branch.guard != null or branch.bindings.len != 0) break :dispatch_split;
                var binders: std.ArrayList(Ast.LocalId) = .empty;
                if (!try self.collectPatBinders(branch.pat, arena, &binders)) break :dispatch_split;
                joins[index] = .{
                    .id = self.pass.freshJoinPoint(),
                    .binding = .{ .locals = binders.items },
                    .body = branch.body,
                    .sites = .empty,
                };
            }
            return joins;
        }
        const joins = try arena.alloc(LetCaseJoin, 1);
        joins[0] = .{
            .id = self.pass.freshJoinPoint(),
            .binding = .{ .pattern = .{ .pat = let_.bind, .comptime_site = let_.comptime_site } },
            .body = let_.rest,
            .sites = .empty,
        };
        return joins;
    }

    /// The small expression each arm clones in place of the continuation:
    /// either a bare jump carrying the arm's value, or the continuation's
    /// dispatching match with every branch body replaced by a jump carrying
    /// that branch's pattern binders.
    fn letCaseDispatchExpr(
        self: *Cloner,
        let_: anytype,
        joins: []const LetCaseJoin,
        probe_ref: Ast.ExprId,
        rest_ty: Type.TypeId,
    ) Common.LowerError!Ast.ExprId {
        if (joins.len == 1 and joins[0].binding == .pattern) {
            const args = [_]Ast.ExprId{probe_ref};
            return try self.addExpr(.{ .ty = rest_ty, .data = .{ .jump = .{
                .target = joins[0].id,
                .args = try self.pass.program.addExprSpan(&args),
            } } });
        }
        const rest_match = self.pass.program.getExpr(let_.rest).data.match_;
        const branches = try GuardedList.dupe(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(rest_match.branches));
        defer self.pass.allocator.free(branches);
        const rewritten = try self.pass.allocator.alloc(Ast.Branch, branches.len);
        defer self.pass.allocator.free(rewritten);
        for (branches, joins, 0..) |branch, join, index| {
            const binders = join.binding.locals;
            const args = try self.pass.allocator.alloc(Ast.ExprId, binders.len);
            defer self.pass.allocator.free(args);
            for (binders, 0..) |binder, arg_index| {
                const binder_ty = self.pass.program.getLocal(binder).ty;
                args[arg_index] = try self.addExpr(.{ .ty = binder_ty, .data = .{ .local = binder } });
            }
            rewritten[index] = .{
                .pat = branch.pat,
                .bindings = branch.bindings,
                .guard = null,
                .body = try self.addExpr(.{ .ty = rest_ty, .data = .{ .jump = .{
                    .target = join.id,
                    .args = try self.pass.program.addExprSpan(args),
                } } }),
            };
        }
        return try self.addExpr(.{ .ty = rest_ty, .data = .{ .match_ = .{
            .scrutinee = probe_ref,
            .branches = try self.pass.program.addBranchSpan(rewritten),
            .comptime_site = rest_match.comptime_site,
        } } });
    }

    /// Append the binder locals of `pat_id` in traversal order. Returns false
    /// for pattern forms whose binders this rewrite does not thread through a
    /// join (list and string patterns), declining the dispatch split.
    /// One step of a walk over a pattern's binders in source order: every
    /// subpattern's binders come before the `as` local that binds it.
    const PatBinderEvent = union(enum) {
        binder: Ast.LocalId,
        /// A list or string pattern, reached before its subpatterns.
        sequence,
    };

    const PatBinderItem = union(enum) {
        pat: Ast.PatId,
        local: Ast.LocalId,
    };

    /// Walks a pattern's binders on an explicit stack.
    const PatBinderWalk = struct {
        program: *const Ast.Program,
        allocator: Allocator,
        stack: std.ArrayList(PatBinderItem) = .empty,

        fn init(program: *const Ast.Program, allocator: Allocator, root: Ast.PatId) Allocator.Error!PatBinderWalk {
            var walk: PatBinderWalk = .{ .program = program, .allocator = allocator };
            try walk.stack.append(allocator, .{ .pat = root });
            return walk;
        }

        fn deinit(walk: *PatBinderWalk) void {
            walk.stack.deinit(walk.allocator);
        }

        fn next(walk: *PatBinderWalk) Allocator.Error!?PatBinderEvent {
            while (walk.stack.pop()) |item| {
                const pat_id = switch (item) {
                    .local => |local| return .{ .binder = local },
                    .pat => |pat_id| pat_id,
                };
                // Components are pushed last-first so they are visited in order.
                const mark = walk.stack.items.len;
                switch (walk.program.getPat(pat_id).data) {
                    .bind => |local| return .{ .binder = local },
                    .wildcard,
                    .int_lit,
                    .dec_lit,
                    .frac_f32_lit,
                    .frac_f64_lit,
                    .str_lit,
                    => {},
                    .as => |as| {
                        try walk.stack.append(walk.allocator, .{ .local = as.local });
                        try walk.stack.append(walk.allocator, .{ .pat = as.pattern });
                    },
                    .record => |fields_span| {
                        const fields = walk.program.recordDestructSpan(fields_span);
                        for (0..fields.len) |index| try walk.stack.append(walk.allocator, .{ .pat = GuardedList.at(fields, index).pattern });
                        std.mem.reverse(PatBinderItem, walk.stack.items[mark..]);
                    },
                    .tuple => |items_span| {
                        const pats = walk.program.patSpan(items_span);
                        for (0..pats.len) |index| try walk.stack.append(walk.allocator, .{ .pat = GuardedList.at(pats, index) });
                        std.mem.reverse(PatBinderItem, walk.stack.items[mark..]);
                    },
                    .tag => |tag_pat| {
                        const pats = walk.program.patSpan(tag_pat.payloads);
                        for (0..pats.len) |index| try walk.stack.append(walk.allocator, .{ .pat = GuardedList.at(pats, index) });
                        std.mem.reverse(PatBinderItem, walk.stack.items[mark..]);
                    },
                    .nominal => |backing| try walk.stack.append(walk.allocator, .{ .pat = backing }),
                    .list => |list| {
                        const pats = walk.program.patSpan(list.patterns);
                        for (0..pats.len) |index| try walk.stack.append(walk.allocator, .{ .pat = GuardedList.at(pats, index) });
                        if (list.rest) |rest| {
                            if (rest.pattern) |rest_pattern| try walk.stack.append(walk.allocator, .{ .pat = rest_pattern });
                        }
                        std.mem.reverse(PatBinderItem, walk.stack.items[mark..]);
                        return .sequence;
                    },
                    .str_pattern => |str| {
                        const steps = walk.program.strPatternStepSpan(str.steps);
                        for (0..steps.len) |index| {
                            if (GuardedList.at(steps, index).capture) |capture| try walk.stack.append(walk.allocator, .{ .pat = capture });
                        }
                        std.mem.reverse(PatBinderItem, walk.stack.items[mark..]);
                        return .sequence;
                    },
                }
            }
            return null;
        }
    };

    fn collectPatBinders(self: *Cloner, pat_id: Ast.PatId, arena: Allocator, out: *std.ArrayList(Ast.LocalId)) Common.LowerError!bool {
        var walk = try PatBinderWalk.init(self.pass.program, self.pass.allocator, pat_id);
        defer walk.deinit();
        while (try walk.next()) |event| switch (event) {
            .binder => |local| try out.append(arena, local),
            .sequence => return false,
        };
        return true;
    }

    fn letCaseJoinFor(self: *Cloner, target: Ast.JoinPointId) ?*LetCaseJoin {
        var build_index = self.let_case_builds.items.len;
        while (build_index > 0) {
            build_index -= 1;
            const build = self.let_case_builds.items[build_index];
            for (build.joins) |*join| {
                if (join.id == target) return join;
            }
        }
        return null;
    }

    /// Rewrite loop initial values whose `uninitialized_payload` condition
    /// names a source loop param to name the emitted param instead. Initial
    /// values are cloned before the emitted params exist, so that forward
    /// reference is the one reference cloning cannot resolve by itself.
    fn retargetLoopForwardConditions(
        self: *Cloner,
        initials: []Ast.ExprId,
        source_locals: []const Ast.LocalId,
        final_locals: []const Ast.LocalId,
    ) Allocator.Error!void {
        for (initials) |*initial| {
            const expr = self.pass.program.getExpr(initial.*);
            if (expr.data != .uninitialized_payload) continue;
            const payload = expr.data.uninitialized_payload;
            for (source_locals, final_locals) |source, final| {
                if (payload.condition != source) continue;
                initial.* = try self.addExpr(.{ .ty = expr.ty, .data = .{ .uninitialized_payload = .{
                    .condition = final,
                    .mask = payload.mask,
                } } });
                break;
            }
        }
    }

    /// A narrowed aggregate binding has only these source-proven field uses.
    /// It has no runtime whole-tuple local and no fabricated dead components.
    fn selectedTupleItem(self: *Cloner, access: anytype) ?Ast.ExprId {
        if (self.purpose != .loop_exit_selection) return null;
        const source = self.pass.program.getExpr(access.tuple).data;
        if (source != .local) return null;
        const items = self.exit_tuple_items.get(source.local) orelse return null;
        if (access.elem_index >= items.len) Common.invariant("selected tuple access exceeded its source type");
        return items[access.elem_index] orelse Common.invariant("continuation read an unselected tuple item");
    }

    /// Total work budget for measuring one known value's constructor size.
    /// Substitution shares one value union across every use site, so a value
    /// built by a recursively-constructed chain is reached by combinatorially
    /// many paths; an unmemoized count re-descends the shared substructure and
    /// need not terminate in bounded time. The count spends one shared budget
    /// per node visit and reports the cap when it runs out. See design.md
    /// "Core Principles" on bounded post-check walks.
    const known_constructor_size_work_budget: u32 = 4096;

    /// Count the constructor nodes (tag, record, tuple, nominal, callable) in a
    /// known value, treating opaque `expr` leaves as zero. This is the measure
    /// the inline recursion guard shrinks: a call re-entering a function already
    /// on the inline stack is admitted only when its known-constructor arguments
    /// are strictly smaller, so inlining an adapter step's `Iter.next` on its
    /// inner iterator (one layer smaller) makes progress and terminates.
    fn knownConstructorSize(self: *Cloner, root: Value) Allocator.Error!ConstructorSize {
        var budget: u32 = known_constructor_size_work_budget;
        // Nodes are visited in pre-order from a worklist, components pushed
        // last-first, so the budget reaches the same nodes a direct walk does.
        var pending = std.ArrayList(Value).empty;
        defer pending.deinit(self.pass.allocator);
        try pending.append(self.pass.allocator, root);
        var total = ConstructorSize{ .exact = 0 };
        while (pending.pop()) |value| {
            if (budget == 0) {
                total = total.plus(.unknown_budget_exhausted);
                continue;
            }
            budget -= 1;
            const mark = pending.items.len;
            switch (value) {
                .expr => {},
                .runtime_anchor => |anchor| try pending.append(self.pass.allocator, anchor.structure.*),
                .static_data_candidate => |candidate| try pending.append(self.pass.allocator, candidate.structure.*),
                .tag => |tag| {
                    total = total.plus(.{ .exact = 1 });
                    try pending.appendSlice(self.pass.allocator, tag.payloads);
                },
                .record => |record| {
                    total = total.plus(.{ .exact = 1 });
                    for (record.fields) |field| try pending.append(self.pass.allocator, field.value);
                },
                .tuple => |tuple| {
                    total = total.plus(.{ .exact = 1 });
                    try pending.appendSlice(self.pass.allocator, tuple.items);
                },
                .nominal => |nominal| {
                    total = total.plus(.{ .exact = 1 });
                    try pending.append(self.pass.allocator, nominal.backing.*);
                },
                .callable => |callable| {
                    total = total.plus(.{ .exact = 1 });
                    for (callable.captures) |capture| try pending.append(self.pass.allocator, capture.value);
                },
            }
            std.mem.reverse(Value, pending.items[mark..]);
        }
        return total;
    }

    /// Resolve an expression to its known value through the current
    /// substitution environment without emitting anything. Used only to measure
    /// a call's known-constructor size for the inline recursion guard; returns
    /// null when the expression carries no known constructor here.
    /// The value an expression is statically known to be. An accessor chain
    /// (`x.a.0.b`) is followed down to its base first and then applied
    /// outward, so its length never becomes call depth.
    fn peekKnownValue(self: *Cloner, root: Ast.ExprId) Allocator.Error!?Value {
        var accessors = std.ArrayList(Ast.ExprId).empty;
        defer accessors.deinit(self.pass.allocator);
        var expr_id = root;
        var value: Value = base: while (true) {
            const expr = self.pass.program.getExpr(expr_id);
            switch (expr.data) {
                .local => |local| {
                    if (self.subst.get(self.pass.program, local)) |known| break :base known;
                    return null;
                },
                .field_access => |field| {
                    try accessors.append(self.pass.allocator, expr_id);
                    expr_id = field.receiver;
                },
                .tuple_access => |access| {
                    try accessors.append(self.pass.allocator, expr_id);
                    expr_id = access.tuple;
                },
                .static_data_candidate => |candidate| expr_id = candidate.runtime_expr,
                .comptime_value => return null,
                .typed_boundary => |boundary| expr_id = boundary.value,
                .unit,
                .@"unreachable",
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .list,
                .tuple,
                .record,
                .record_update,
                .tag,
                .nominal,
                .let_,
                .lambda,
                .def_ref,
                .fn_def,
                .fn_ref,
                .call_value,
                .call_proc,
                .low_level,
                .structural_eq,
                .structural_hash,
                .match_,
                .if_,
                .uninitialized,
                .uninitialized_payload,
                .if_initialized_payload,
                .try_sequence,
                .try_record_sequence,
                .block,
                .loop_,
                .break_,
                .continue_,
                .join_point,
                .jump,
                .return_,
                .crash,
                .checked_error,
                .comptime_branch_taken,
                .comptime_exhaustiveness_failed,
                .dbg,
                .expect_err,
                .literal_rejected,
                .expect,
                => return null,
            }
        };
        while (accessors.pop()) |accessor| {
            value = switch (self.pass.program.getExpr(accessor).data) {
                .field_access => |field| fieldPathFromValue(
                    self.pass.program,
                    value,
                    self.pass.program.fieldAccessSegmentSpan(field.segments),
                ),
                .tuple_access => |access| itemFromValue(value, access.elem_index),
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => unreachable,
            } orelse return null;
        }
        return value;
    }

    fn argsKnownConstructorSize(self: *Cloner, span: Ast.Span(Ast.ExprId)) Allocator.Error!ConstructorSize {
        var total = ConstructorSize{ .exact = 0 };
        const args = self.pass.program.exprSpan(span);
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (try self.peekKnownValue(arg)) |value| total = total.plus(try self.knownConstructorSize(value));
        }
        return total;
    }

    fn captureOperandsKnownConstructorSize(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!ConstructorSize {
        var total = ConstructorSize{ .exact = 0 };
        const operands = self.pass.program.captureOperandSpan(span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            if (try self.peekKnownValue(operand.value)) |value| total = total.plus(try self.knownConstructorSize(value));
        }
        return total;
    }

    /// Total work budget for making one value reuse-safe. A known value is not
    /// always a small finite tree: substitution shares one value union across
    /// every use site, so a value built by a recursively-constructed chain (an
    /// iterator wrapped around itself through many map layers) is a compact
    /// graph reached by combinatorially many distinct paths, and this walk
    /// probes each visited node with `valueCanSubstitute`—itself a full
    /// sub-walk—so its cost is the node count times that probe and grows far
    /// past any per-level depth. The walk spends one shared budget per node
    /// visit and, when it runs out, keeps the remaining sub-value materialized
    /// as-is instead of continuing to rewrite it. See design.md "Core
    /// Principles" on bounded post-check walks.
    ///
    /// When the budget is exhausted, the remaining sub-value is materialized
    /// and named as one strict binding. This bounds compiler work without
    /// weakening single evaluation or effect ordering.
    const make_reusable_work_budget: u32 = 4096;

    /// Place a strict chain in one flat block, in source evaluation order.
    fn wrapBindings(self: *Cloner, bindings: BindingChain, expr: Ast.ExprId) Common.LowerError!Ast.ExprId {
        if (bindings.isEmpty()) return expr;
        var statements = std.ArrayList(Ast.StmtId).empty;
        defer statements.deinit(self.pass.allocator);
        try self.appendBindingStmts(bindings, &statements);
        return try self.addExpr(.{ .ty = self.pass.program.getExpr(expr).ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(statements.items),
            .final_expr = expr,
        } } });
    }

    /// Place a strict chain into a statement list, oldest first.
    fn appendBindingStmts(self: *Cloner, bindings: BindingChain, out: *std.ArrayList(Ast.StmtId)) Common.LowerError!void {
        bindings.verify(self.pass.program);
        var current = bindings.first;
        while (current) |node| : (current = node.next) {
            switch (node.binding) {
                .strict => |binding| {
                    const pat = try self.pass.program.addPat(.{
                        .ty = binding.ty,
                        .data = .{ .bind = binding.local },
                    });
                    try out.append(self.pass.allocator, try self.addStmt(.{ .let_ = .{
                        .pat = pat,
                        .value = binding.value,
                    } }));
                },
                .statement => |stmt| try out.append(self.pass.allocator, stmt),
            }
        }
    }

    /// A pattern being bound to a value, waiting on its subpatterns.
    const PatValueFrame = struct {
        pat_id: Ast.PatId,
        value: Value,
        stage: enum { start, as_binding, from_receiver, from_value, nominal } = .start,
        index: usize = 0,
        verdict: MatchVerdict = .match,
        receiver: Ast.ExprId = undefined,
        record: RecordValue = undefined,
        tuple: TupleValue = undefined,
        tag: TagValue = undefined,
    };

    fn PatValueStep(comptime Answer: type) type {
        return union(enum) {
            /// Bind a subpattern; the frame resumes with its answer.
            child: struct { pat_id: Ast.PatId, value: Value },
            done: Answer,
        };
    }

    /// Drive a pattern-to-value binding on an explicit frame stack, so
    /// pattern nesting never nests native calls.
    fn runPatValue(
        self: *Cloner,
        comptime Answer: type,
        comptime step: fn (*Cloner, *PatValueFrame, ?Answer) Common.LowerError!PatValueStep(Answer),
        pat_id: Ast.PatId,
        value: Value,
    ) Common.LowerError!Answer {
        var frames = std.ArrayList(PatValueFrame).empty;
        defer {
            // A nominal frame holds one wrapper-strip level until it answers.
            for (frames.items) |frame| {
                if (frame.stage == .nominal) self.wrapper_strip_depth -= 1;
            }
            frames.deinit(self.pass.allocator);
        }
        try frames.append(self.pass.allocator, .{ .pat_id = pat_id, .value = value });
        var input: ?Answer = null;
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            const next = try step(self, frame, input);
            input = null;
            switch (next) {
                .child => |child| try frames.append(self.pass.allocator, .{ .pat_id = child.pat_id, .value = child.value }),
                .done => |answer| {
                    _ = frames.pop();
                    if (frames.items.len == 0) return answer;
                    input = answer;
                },
            }
        }
    }

    fn bindPatToValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!MatchVerdict {
        return try self.runPatValue(MatchVerdict, stepBindPatToValue, pat_id, value);
    }

    fn stepBindPatToValue(self: *Cloner, frame: *PatValueFrame, input: ?MatchVerdict) Common.LowerError!PatValueStep(MatchVerdict) {
        const pat = self.pass.program.getPat(frame.pat_id);
        const value = frame.value;
        // A component's verdict: a no-match decides the pattern, and an
        // unknown makes the pattern at most unknown.
        if (input) |child_verdict| switch (frame.stage) {
            .start => unreachable,
            .as_binding => {
                if (child_verdict != .match) return .{ .done = child_verdict };
                try self.subst.put(self.pass.program, pat.data.as.local, value);
                return .{ .done = .match };
            },
            .nominal => {
                self.wrapper_strip_depth -= 1;
                frame.stage = .start;
                return .{ .done = child_verdict };
            },
            .from_receiver, .from_value => switch (child_verdict) {
                .match => {},
                .no_match => return .{ .done = .no_match },
                .unknown, .unknown_budget_exhausted => frame.verdict = mergeMatchUnknown(frame.verdict, child_verdict),
            },
        };
        switch (pat.data) {
            .bind => |local| {
                try self.subst.put(self.pass.program, local, value);
                return .{ .done = .match };
            },
            .wildcard => return .{ .done = .match },
            .as => |as| {
                frame.stage = .as_binding;
                return .{ .child = .{ .pat_id = as.pattern, .value = value } };
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                if (frame.stage == .start) {
                    // An anchor's structure is matched in its place.
                    while (frame.value == .runtime_anchor) frame.value = frame.value.runtime_anchor.structure.*;
                    const record_value = frame.value;
                    switch (record_value) {
                        .runtime_anchor => unreachable,
                        .expr => |receiver| {
                            if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .{ .done = .unknown };
                            frame.stage = .from_receiver;
                            frame.receiver = receiver;
                        },
                        .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => {
                            frame.stage = .from_value;
                            if (isOpaqueBehindWrappers(record_value)) return .{ .done = .unknown };
                            frame.record = recordFromValue(record_value) orelse switch (record_value) {
                                .tag, .tuple, .callable => Common.invariant("record pattern matched a non-record value"),
                                .expr, .runtime_anchor, .static_data_candidate, .record, .nominal => Common.invariant("record value had no record backing"),
                            };
                        },
                    }
                }
                if (frame.index == fields.len) return .{ .done = frame.verdict };
                const field = GuardedList.at(fields, frame.index);
                frame.index += 1;
                if (frame.stage == .from_receiver) {
                    const field_ty = self.pass.program.getPat(field.pattern).ty;
                    const field_expr = try self.addFieldAccessExpr(field_ty, frame.receiver, field.name);
                    return .{ .child = .{ .pat_id = field.pattern, .value = .{ .expr = field_expr } } };
                }
                const field_value = fieldFromRecord(self.pass.program, frame.record, field.name) orelse
                    Common.invariant("record pattern field was absent from the record value");
                return .{ .child = .{ .pat_id = field.pattern, .value = field_value } };
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                if (frame.stage == .start) {
                    // An anchor's structure is matched in its place.
                    while (frame.value == .runtime_anchor) frame.value = frame.value.runtime_anchor.structure.*;
                    const tuple_value = frame.value;
                    switch (tuple_value) {
                        .runtime_anchor => unreachable,
                        .expr => |receiver| {
                            if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .{ .done = .unknown };
                            frame.stage = .from_receiver;
                            frame.receiver = receiver;
                        },
                        .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => {
                            frame.stage = .from_value;
                            if (isOpaqueBehindWrappers(tuple_value)) return .{ .done = .unknown };
                            frame.tuple = tupleFromValue(tuple_value) orelse switch (tuple_value) {
                                .tag, .record, .callable => Common.invariant("tuple pattern matched a non-tuple value"),
                                .expr, .runtime_anchor, .static_data_candidate, .tuple, .nominal => Common.invariant("tuple value had no tuple backing"),
                            };
                            if (pats.len != frame.tuple.items.len) Common.invariant("tuple pattern arity differed from the tuple value");
                        },
                    }
                }
                if (frame.index == pats.len) return .{ .done = frame.verdict };
                const index = frame.index;
                frame.index += 1;
                const child_pat = GuardedList.at(pats, index);
                if (frame.stage == .from_receiver) {
                    const item_ty = self.pass.program.getPat(child_pat).ty;
                    const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                        .tuple = frame.receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    return .{ .child = .{ .pat_id = child_pat, .value = .{ .expr = item_expr } } };
                }
                return .{ .child = .{ .pat_id = child_pat, .value = frame.tuple.items[index] } };
            },
            .tag => |tag_pat| {
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (frame.stage == .start) {
                    if (isOpaqueBehindWrappers(value)) return .{ .done = .unknown };
                    frame.tag = tagFromValue(value) orelse switch (value) {
                        .record, .tuple, .callable => Common.invariant("tag pattern matched a non-tag value"),
                        .expr, .runtime_anchor, .static_data_candidate, .tag, .nominal => Common.invariant("tag value had no tag backing"),
                    };
                    if (!self.pass.program.names.tagLabelTextEql(frame.tag.name, tag_pat.name)) return .{ .done = .no_match };
                    if (pats.len != frame.tag.payloads.len) Common.invariant("tag pattern payload arity differed from the tag value");
                    frame.stage = .from_value;
                }
                if (frame.index == pats.len) return .{ .done = frame.verdict };
                const index = frame.index;
                frame.index += 1;
                return .{ .child = .{ .pat_id = GuardedList.at(pats, index), .value = frame.tag.payloads[index] } };
            },
            .nominal => |backing_pat| {
                // Stripping a nominal or static-data wrapper follows a value
                // pointer edge that a recursive construction can loop through;
                // a cyclic value declines to a residual runtime match.
                if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return .{ .done = .unknown_budget_exhausted };
                const child: Ast.PatId, const child_value: Value = switch (value) {
                    .runtime_anchor => |anchor| .{ frame.pat_id, anchor.structure.* },
                    .static_data_candidate => |candidate| .{ frame.pat_id, candidate.structure.* },
                    .nominal => |nominal| .{ backing_pat, nominal.backing.* },
                    .expr => return .{ .done = .unknown },
                    .tag, .record, .tuple, .callable => Common.invariant("nominal pattern matched an unwrapped constructor value"),
                };
                self.wrapper_strip_depth += 1;
                frame.stage = .nominal;
                return .{ .child = .{ .pat_id = child, .value = child_value } };
            },
            // These pattern forms have no symbolic `Value` representation,
            // so their outcome is statically undecidable here.
            .list,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .str_pattern,
            => return .{ .done = .unknown },
        }
    }

    /// Whether a nominal pattern's backing pattern binds its value only
    /// through record-field or tuple-item reads, which apply to the nominal
    /// value itself.
    fn patternProjectsNominalBacking(self: *const Cloner, backing_pat: Ast.PatId) bool {
        return switch (self.pass.program.getPat(backing_pat).data) {
            .record, .tuple => true,
            .bind, .wildcard, .as, .tag, .nominal, .list, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => false,
        };
    }

    fn bindPatToReusableValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!MatchVerdict {
        return switch (try self.valueCanSubstitute(value)) {
            .proven => if (try self.bindPatToFlowValue(pat_id, value)) .match else .unknown,
            .disproven => .unknown,
            .unknown_budget_exhausted => .unknown_budget_exhausted,
        };
    }

    /// Bind a pattern for ordinary structured value flow. Unlike the
    /// three-way static matcher above, this never selects a match branch: it
    /// may project a value of a statically known record or tuple type and
    /// simply reports whether all required substitutions could be formed.
    fn bindPatToFlowValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!bool {
        return try self.runPatValue(bool, stepBindPatToFlowValue, pat_id, value);
    }

    fn stepBindPatToFlowValue(self: *Cloner, frame: *PatValueFrame, input: ?bool) Common.LowerError!PatValueStep(bool) {
        const pat = self.pass.program.getPat(frame.pat_id);
        const value = frame.value;
        if (input) |bound| switch (frame.stage) {
            .start => unreachable,
            .as_binding => {
                if (!bound) return .{ .done = false };
                try self.subst.put(self.pass.program, pat.data.as.local, value);
                return .{ .done = true };
            },
            .nominal => {
                self.wrapper_strip_depth -= 1;
                frame.stage = .start;
                return .{ .done = bound };
            },
            .from_receiver, .from_value => if (!bound) return .{ .done = false },
        };
        switch (pat.data) {
            .bind => |local| {
                try self.subst.put(self.pass.program, local, value);
                return .{ .done = true };
            },
            .wildcard => return .{ .done = true },
            .as => |as| {
                frame.stage = .as_binding;
                return .{ .child = .{ .pat_id = as.pattern, .value = value } };
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                if (frame.stage == .start) switch (value) {
                    .record, .nominal, .runtime_anchor, .static_data_candidate => {
                        frame.record = recordFromValue(value) orelse return .{ .done = false };
                        frame.stage = .from_value;
                    },
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .{ .done = false };
                        frame.receiver = receiver;
                        frame.stage = .from_receiver;
                    },
                    .tag, .tuple, .callable => return .{ .done = false },
                };
                if (frame.index == fields.len) return .{ .done = true };
                const field = GuardedList.at(fields, frame.index);
                frame.index += 1;
                if (frame.stage == .from_receiver) {
                    const field_ty = self.pass.program.getPat(field.pattern).ty;
                    const field_expr = try self.addFieldAccessExpr(field_ty, frame.receiver, field.name);
                    return .{ .child = .{ .pat_id = field.pattern, .value = .{ .expr = field_expr } } };
                }
                const field_value = fieldFromRecord(self.pass.program, frame.record, field.name) orelse return .{ .done = false };
                return .{ .child = .{ .pat_id = field.pattern, .value = field_value } };
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                if (frame.stage == .start) switch (value) {
                    .tuple, .nominal, .runtime_anchor, .static_data_candidate => {
                        frame.tuple = tupleFromValue(value) orelse return .{ .done = false };
                        if (pats.len != frame.tuple.items.len) return .{ .done = false };
                        frame.stage = .from_value;
                    },
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .{ .done = false };
                        frame.receiver = receiver;
                        frame.stage = .from_receiver;
                    },
                    .tag, .record, .callable => return .{ .done = false },
                };
                if (frame.index == pats.len) return .{ .done = true };
                const index = frame.index;
                frame.index += 1;
                const child_pat = GuardedList.at(pats, index);
                if (frame.stage == .from_receiver) {
                    const item_ty = self.pass.program.getPat(child_pat).ty;
                    const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                        .tuple = frame.receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    return .{ .child = .{ .pat_id = child_pat, .value = .{ .expr = item_expr } } };
                }
                return .{ .child = .{ .pat_id = child_pat, .value = frame.tuple.items[index] } };
            },
            .tag => |tag_pat| {
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (frame.stage == .start) {
                    frame.tag = tagFromValue(value) orelse return .{ .done = false };
                    if (!self.pass.program.names.tagLabelTextEql(frame.tag.name, tag_pat.name)) return .{ .done = false };
                    if (pats.len != frame.tag.payloads.len) return .{ .done = false };
                    frame.stage = .from_value;
                }
                if (frame.index == pats.len) return .{ .done = true };
                const index = frame.index;
                frame.index += 1;
                return .{ .child = .{ .pat_id = GuardedList.at(pats, index), .value = frame.tag.payloads[index] } };
            },
            .nominal => |backing_pat| {
                // Stripping a nominal or static-data wrapper follows a value
                // pointer edge that a recursive construction can loop through;
                // a cyclic value declines the flow binding.
                if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return .{ .done = false };
                const child: Ast.PatId, const child_value: Value = switch (value) {
                    .runtime_anchor => |anchor| .{ frame.pat_id, anchor.structure.* },
                    .static_data_candidate => |candidate| .{ frame.pat_id, candidate.structure.* },
                    .nominal => |nominal| .{ backing_pat, nominal.backing.* },
                    .expr => |receiver| if (canReadFieldsFromExpr(self.pass.program, receiver) and self.patternProjectsNominalBacking(backing_pat))
                        .{ backing_pat, value }
                    else
                        return .{ .done = false },
                    .tag, .record, .tuple, .callable => return .{ .done = false },
                };
                self.wrapper_strip_depth += 1;
                frame.stage = .nominal;
                return .{ .child = .{ .pat_id = child, .value = child_value } };
            },
            .list,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .str_pattern,
            => return .{ .done = false },
        }
    }

    /// Record an identity substitution for a local bound by an already-emitted
    /// pattern. This is used when a rewrite reuses that exact pattern node;
    /// source patterns cloned into new code go through `clonePat`, which gives
    /// every emitted binder a fresh local instead.
    fn shadowLocal(self: *Cloner, local: Ast.LocalId) Common.LowerError!void {
        const ty = self.pass.program.getLocal(local).ty;
        try self.subst.put(self.pass.program, local, .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = local } }) });
    }

    /// Record an identity substitution for one exact local id, leaving its
    /// binder's other versions to resolve however they already do. This is the
    /// pin for a binding whose scope does not cover the position being cloned,
    /// where `shadowLocal`'s binder-wide claim would be false.
    fn pinSourceLocal(self: *Cloner, local: Ast.LocalId) Common.LowerError!void {
        const ty = self.pass.program.getLocal(local).ty;
        try self.subst.putExact(local, .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = local } }) });
    }

    fn putLocalAlias(self: *Cloner, source: Ast.LocalId, target: Ast.LocalId) Common.LowerError!void {
        const ty = self.pass.program.getLocal(target).ty;
        const target_expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = target } });
        try self.subst.putLocalAlias(self.pass.program, source, .{ .expr = target_expr });
    }

    fn shadowPatLocals(self: *Cloner, pat_id: Ast.PatId) Common.LowerError!void {
        var walk = try PatBinderWalk.init(self.pass.program, self.pass.allocator, pat_id);
        defer walk.deinit();
        while (try walk.next()) |event| switch (event) {
            .binder => |local| try self.shadowLocal(local),
            .sequence => {},
        };
    }

    fn shadowStmtSpanLocals(self: *Cloner, span: Ast.Span(Ast.StmtId)) Common.LowerError!void {
        const statements = self.pass.program.stmtSpan(span);
        for (0..statements.len) |index| {
            switch (self.pass.program.getStmt(GuardedList.at(statements, index))) {
                .let_ => |let_| try self.shadowPatLocals(let_.pat),
                .uninitialized => |pat| try self.shadowPatLocals(pat),
                .expr, .expect, .dbg, .return_, .crash, .checked_error => {},
            }
        }
    }

    fn markActiveRecursiveValuePat(self: *Cloner, pat_id: Ast.PatId) Allocator.Error!void {
        var walk = try PatBinderWalk.init(self.pass.program, self.pass.allocator, pat_id);
        defer walk.deinit();
        while (try walk.next()) |event| switch (event) {
            .binder => |local| try self.active_recursive_value_locals.put(local, {}),
            .sequence => {},
        };
    }

    fn unmarkActiveRecursiveValuePat(self: *Cloner, pat_id: Ast.PatId) Allocator.Error!void {
        var walk = try PatBinderWalk.init(self.pass.program, self.pass.allocator, pat_id);
        defer walk.deinit();
        while (try walk.next()) |event| switch (event) {
            .binder => |local| _ = self.active_recursive_value_locals.remove(local),
            .sequence => {},
        };
    }

    const BinderCloneMode = enum {
        /// The surrounding clone has already replaced every use of this
        /// binding with a known value. The emitted pattern still needs its own
        /// fresh identity, but must not overwrite that value substitution.
        output_only,
        /// The cloned body retains references to the runtime binding. Map the
        /// source local to the fresh output local for the binding's scope.
        bind_runtime,
    };

    fn cloneBinder(self: *Cloner, source: Ast.LocalId, ty: Type.TypeId, mode: BinderCloneMode) Common.LowerError!Ast.LocalId {
        const fresh = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
        if (mode == .bind_runtime) {
            const local_expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = fresh } });
            try self.subst.put(self.pass.program, source, .{ .expr = local_expr });
        }
        return fresh;
    }

    /// Rewrite a local reference stored directly in an expression node rather
    /// than in a child `.local` expression. These fields require a runtime
    /// local, so a structured substitution is an invalid cloned IR state.
    fn cloneLocalRef(self: *Cloner, source: Ast.LocalId) Ast.LocalId {
        const value = self.subst.getForClone(self.pass.program, source) orelse return source;
        const expr = switch (value) {
            .expr => |expr| expr,
            .runtime_anchor => |anchor| anchor.runtime,
            .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => Common.invariant("SpecConstr local-id field referenced a non-local substituted value"),
        };
        return localExpr(self.pass.program, expr) orelse
            Common.invariant("SpecConstr local-id field referenced a non-local expression");
    }

    fn cloneLoopUpdateParams(self: *Cloner, span: Ast.Span(Ast.TypedLocal)) Common.LowerError!Ast.Span(Ast.TypedLocal) {
        const source = self.pass.program.typedLocalSpan(span);
        const params = try self.pass.allocator.alloc(Ast.TypedLocal, source.len);
        defer self.pass.allocator.free(params);
        for (0..source.len) |index| {
            const param = GuardedList.at(source, index);
            params[index] = .{ .local = self.cloneLocalRef(param.local), .ty = param.ty };
        }
        return try self.pass.program.addTypedLocalSpan(params);
    }

    /// Retained ownership follows SpecConstr's exact value substitution. A
    /// scalarized aggregate has no runtime aggregate local to retain, so keep
    /// its runtime leaf locals instead of rematerializing the aggregate.
    fn cloneRetainedLocals(self: *Cloner, span: Ast.Span(Ast.TypedLocal)) Common.LowerError!Ast.Span(Ast.TypedLocal) {
        if (span.len == 0) return .empty();
        const source = self.pass.program.typedLocalSpan(span);
        var retained: std.ArrayList(Ast.TypedLocal) = .empty;
        defer retained.deinit(self.pass.allocator);
        for (0..source.len) |index| {
            const typed = GuardedList.at(source, index);
            if (self.subst.getForClone(self.pass.program, typed.local)) |value| {
                try self.appendRetainedValueLocals(value, &retained);
            } else {
                try retained.append(self.pass.allocator, typed);
            }
        }
        try compactRetainedLocals(self.pass.allocator, &retained);
        return try self.pass.program.addTypedLocalSpan(retained.items);
    }

    /// Compact touched ordinals, not the raw ID range: retained leaves can mix
    /// old source locals with generated locals arbitrarily far away.
    fn compactRetainedLocals(
        allocator: Allocator,
        retained: *std.ArrayList(Ast.TypedLocal),
    ) Allocator.Error!void {
        if (retained.items.len < 2) return;
        const ordinals = try allocator.alloc(usize, retained.items.len);
        defer allocator.free(ordinals);
        const duplicates = try allocator.alloc(bool, retained.items.len);
        defer allocator.free(duplicates);
        @memset(duplicates, false);
        for (ordinals, 0..) |*ordinal, index| ordinal.* = index;
        std.sort.heap(usize, ordinals, retained.items, struct {
            fn lessThan(leaves: []const Ast.TypedLocal, lhs: usize, rhs: usize) bool {
                const lhs_id = @intFromEnum(leaves[lhs].local);
                const rhs_id = @intFromEnum(leaves[rhs].local);
                return if (lhs_id == rhs_id) lhs < rhs else lhs_id < rhs_id;
            }
        }.lessThan);
        for (ordinals[1..], ordinals[0 .. ordinals.len - 1]) |ordinal, previous| {
            if (retained.items[ordinal].local == retained.items[previous].local) {
                duplicates[ordinal] = true;
            }
        }
        var len: usize = 0;
        for (retained.items, duplicates) |typed, duplicate| {
            if (duplicate) continue;
            retained.items[len] = typed;
            len += 1;
        }
        retained.items.len = len;
    }

    fn appendRetainedExprLocal(
        self: *Cloner,
        expr: Ast.ExprId,
        retained: *std.ArrayList(Ast.TypedLocal),
    ) Common.LowerError!void {
        const value = self.pass.program.getExpr(expr);
        if (value.data == .local) {
            return try retained.append(self.pass.allocator, .{ .local = value.data.local, .ty = value.ty });
        }
        if (value.data == .unit or
            value.data == .@"unreachable" or
            value.data == .int_lit or
            value.data == .frac_f32_lit or
            value.data == .frac_f64_lit or
            value.data == .dec_lit or
            value.data == .str_lit or
            value.data == .bytes_lit or
            value.data == .uninitialized or
            value.data == .uninitialized_payload) return;
        Common.invariant("SpecConstr retained ownership leaf was neither a runtime local nor a static value");
    }

    /// Append the runtime locals `root` retains, in depth-first order. The
    /// walk keeps its own work stack; each entry carries the number of
    /// nominal layers above it, which a finite value keeps below
    /// `value_wrapper_strip_cap`.
    fn appendRetainedValueLocals(
        self: *Cloner,
        root: Value,
        retained: *std.ArrayList(Ast.TypedLocal),
    ) Common.LowerError!void {
        const Item = struct { value: Value, depth: usize };
        const allocator = self.pass.allocator;
        var stack: std.ArrayList(Item) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, .{ .value = root, .depth = 0 });
        while (stack.pop()) |item| {
            if (item.depth >= value_wrapper_strip_cap) {
                Common.invariant("SpecConstr retained ownership followed a cyclic value");
            }
            // Children are appended in order, then reversed.
            const start = stack.items.len;
            switch (item.value) {
                .expr => |expr| try self.appendRetainedExprLocal(expr, retained),
                .runtime_anchor => |anchor| try self.appendRetainedExprLocal(anchor.runtime, retained),
                .static_data_candidate => {}, // Closed static values retain no caller locals.
                .tag => |tag| for (tag.payloads) |payload| try stack.append(allocator, .{ .value = payload, .depth = item.depth }),
                .record => |record| for (record.fields) |field| try stack.append(allocator, .{ .value = field.value, .depth = item.depth }),
                .tuple => |tuple| for (tuple.items) |value| try stack.append(allocator, .{ .value = value, .depth = item.depth }),
                .nominal => |nominal| try stack.append(allocator, .{ .value = nominal.backing.*, .depth = item.depth + 1 }),
                .callable => |callable| for (callable.captures) |capture| try stack.append(allocator, .{ .value = capture.value, .depth = item.depth }),
            }
            std.mem.reverse(Item, stack.items[start..]);
        }
    }

    /// Clone a pattern tree. A pattern is added after its subpatterns, and
    /// each unfinished pattern waits in a frame on an explicit stack while a
    /// subpattern is cloned, so pattern nesting never becomes native call
    /// depth. Subpatterns, binders, and spans are cloned and added in the
    /// order a direct recursive clone visited them.
    fn clonePat(self: *Cloner, root: Ast.PatId, mode: BinderCloneMode) Common.LowerError!Ast.PatId {
        const allocator = self.pass.allocator;
        const Frame = struct {
            pat: Ast.PatId,
            /// The cloned subpatterns so far.
            children: std.ArrayList(Ast.PatId) = .empty,
            /// A list pattern's cloned element span, added before its rest.
            span: ?Ast.Span(Ast.PatId) = null,
        };
        var frames: std.ArrayList(Frame) = .empty;
        defer {
            for (frames.items) |*frame| frame.children.deinit(allocator);
            frames.deinit(allocator);
        }
        try frames.append(allocator, .{ .pat = root });
        var input: ?Ast.PatId = null;
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            if (input) |cloned| try frame.children.append(allocator, cloned);
            input = null;
            const pat = self.pass.program.getPat(frame.pat);
            const done = frame.children.items.len;
            const next_child: ?Ast.PatId = switch (pat.data) {
                .as => |as| if (done == 0) as.pattern else null,
                .nominal => |backing| if (done == 0) backing else null,
                .record => |fields| blk: {
                    const destructs = self.pass.program.recordDestructSpan(fields);
                    break :blk if (done < destructs.len) GuardedList.at(destructs, done).pattern else null;
                },
                .tuple => |items| blk: {
                    const pats = self.pass.program.patSpan(items);
                    break :blk if (done < pats.len) GuardedList.at(pats, done) else null;
                },
                .tag => |tag| blk: {
                    const pats = self.pass.program.patSpan(tag.payloads);
                    break :blk if (done < pats.len) GuardedList.at(pats, done) else null;
                },
                .list => |list| blk: {
                    const pats = self.pass.program.patSpan(list.patterns);
                    if (done < pats.len) break :blk GuardedList.at(pats, done);
                    if (frame.span == null) frame.span = try self.pass.program.addPatSpan(frame.children.items);
                    const rest = list.rest orelse break :blk null;
                    const rest_pattern = rest.pattern orelse break :blk null;
                    break :blk if (done == pats.len) rest_pattern else null;
                },
                .str_pattern => |str| blk: {
                    const steps = self.pass.program.strPatternStepSpan(str.steps);
                    var captured: usize = 0;
                    for (0..steps.len) |index| {
                        const capture = GuardedList.at(steps, index).capture orelse continue;
                        if (captured == done) break :blk capture;
                        captured += 1;
                    }
                    break :blk null;
                },
                .bind, .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => null,
            };
            if (next_child) |child| {
                try frames.append(allocator, .{ .pat = child });
                continue;
            }

            const children = frame.children.items;
            const data: Ast.PatData = switch (pat.data) {
                .bind => |local| .{ .bind = try self.cloneBinder(local, pat.ty, mode) },
                .wildcard => .wildcard,
                .as => |as| .{ .as = .{
                    .pattern = children[0],
                    .local = try self.cloneBinder(as.local, pat.ty, mode),
                } },
                .record => |fields| blk: {
                    const destructs = self.pass.program.recordDestructSpan(fields);
                    const values = try allocator.alloc(Ast.RecordDestruct, children.len);
                    defer allocator.free(values);
                    for (values, children, 0..) |*value, child, index| value.* = .{
                        .name = GuardedList.at(destructs, index).name,
                        .pattern = child,
                    };
                    break :blk .{ .record = try self.pass.program.addRecordDestructSpan(values) };
                },
                .tuple => .{ .tuple = try self.pass.program.addPatSpan(children) },
                .list => |list| .{ .list = .{
                    .patterns = frame.span.?,
                    .rest = if (list.rest) |rest| .{
                        .index = rest.index,
                        .pattern = if (rest.pattern != null) children[children.len - 1] else null,
                    } else null,
                } },
                .tag => |tag| .{ .tag = .{
                    .name = tag.name,
                    .payloads = try self.pass.program.addPatSpan(children),
                } },
                .nominal => .{ .nominal = children[0] },
                .int_lit => |value| .{ .int_lit = value },
                .dec_lit => |value| .{ .dec_lit = value },
                .frac_f32_lit => |value| .{ .frac_f32_lit = value },
                .frac_f64_lit => |value| .{ .frac_f64_lit = value },
                .str_lit => |value| .{ .str_lit = value },
                .str_pattern => |str| blk: {
                    const input_steps = self.pass.program.strPatternStepSpan(str.steps);
                    const output_steps = try allocator.alloc(Ast.StrPatternStep, input_steps.len);
                    defer allocator.free(output_steps);
                    var captured: usize = 0;
                    for (output_steps, 0..) |*output_step, index| {
                        const input_step = GuardedList.at(input_steps, index);
                        output_step.* = .{
                            .capture = if (input_step.capture != null) cap: {
                                captured += 1;
                                break :cap children[captured - 1];
                            } else null,
                            .delimiter = input_step.delimiter,
                        };
                    }
                    break :blk .{ .str_pattern = .{
                        .prefix = str.prefix,
                        .steps = try self.pass.program.addStrPatternStepSpan(output_steps),
                        .end = str.end,
                    } };
                },
            };
            const cloned = try self.pass.program.addPat(.{ .ty = pat.ty, .data = data });
            var finished = frames.pop().?;
            finished.children.deinit(allocator);
            if (frames.items.len == 0) return cloned;
            input = cloned;
        }
    }

    /// Total node-visit bound for proving which initializer leaves survive a
    /// recursive statement's scope. Symbolic values can be cyclic or heavily
    /// shared, so an unbounded structural walk is not total. Exhaustion never
    /// loses no information: the unresolved sub-value becomes its exact runtime
    /// expression, preserving correctness and single evaluation while declining
    /// only the unproved structure. See design.md "Core Principles" on bounded
    /// post-check walks.
    const recursive_anchor_scope_work_budget: u32 = 4096;

    /// Whether a structural sub-value cannot safely flow beyond an
    /// initializer chain. A non-reusable expression could duplicate runtime
    /// work through later structural use; an expression referencing a chain
    /// local would escape its lexical scope. Either case must use the retained
    /// recursive value's exact runtime position instead.
    fn valueContainsNonReusableOrInitializerLocalExpr(
        self: *Cloner,
        bindings: BindingChain,
        root: Value,
        budget: *u32,
    ) Common.LowerError!bool {
        const allocator = self.pass.allocator;
        const program = self.pass.program;
        var stack: std.ArrayList(Value) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, root);
        // Any hit, including an exhausted budget, answers true; the search
        // order only decides which nodes the budget reaches.
        while (stack.pop()) |value| {
            if (budget.* == 0) return true;
            budget.* -= 1;
            const start = stack.items.len;
            switch (value) {
                .expr => |expr| if (!try self.exprCanSubstitute(expr) or
                    try bindings.referencedByExpr(allocator, program, expr)) return true,
                .runtime_anchor => |anchor| {
                    if (!try self.exprCanSubstitute(anchor.runtime) or
                        try bindings.referencedByExpr(allocator, program, anchor.runtime)) return true;
                    try stack.append(allocator, anchor.structure.*);
                },
                .static_data_candidate => {},
                .tag => |tag| try stack.appendSlice(allocator, tag.payloads),
                .record => |record| for (record.fields) |field| try stack.append(allocator, field.value),
                .tuple => |tuple| try stack.appendSlice(allocator, tuple.items),
                .nominal => |nominal| try stack.append(allocator, nominal.backing.*),
                .callable => |callable| for (callable.captures) |capture| try stack.append(allocator, capture.value),
            }
            std.mem.reverse(Value, stack.items[start..]);
        }
        return false;
    }

    fn runtimeAnchoredValue(self: *Cloner, structure: Value, runtime: Ast.ExprId) Allocator.Error!Value {
        if (std.debug.runtime_safety and !try self.exprCanSubstitute(runtime)) {
            Common.invariant("runtime-anchored structure had a non-reusable runtime expression");
        }
        if (std.debug.runtime_safety and
            !try self.pass.program.types.typeEql(
                &self.pass.program.names,
                valueType(self.pass.program, structure),
                self.pass.program.getExpr(runtime).ty,
            ))
        {
            Common.invariant("runtime anchor and its symbolic structure had different types");
        }
        const stored_structure = try self.arena.allocator().create(Value);
        stored_structure.* = structure;
        return .{ .runtime_anchor = .{
            .runtime = runtime,
            .structure = stored_structure,
        } };
    }

    fn typedBoundaryValue(self: *Cloner, structure: Value, runtime: Ast.ExprId) Allocator.Error!Value {
        if (std.debug.runtime_safety) {
            const boundary = self.pass.program.getExpr(runtime).data;
            if (boundary != .typed_boundary) Common.invariant("typed-boundary structure had no typed-boundary runtime expression");
            // Compare the immediate producer view. `structure` can itself be
            // anchored by an earlier typed boundary, and stripping that anchor
            // would incorrectly skip a valid link in the conversion chain.
            if (!try self.pass.program.types.typeEql(
                &self.pass.program.names,
                valueType(self.pass.program, structure),
                self.pass.program.getExpr(boundary.typed_boundary.value).ty,
            )) {
                Common.invariant("typed-boundary structure differed from the boundary's producer type");
            }
        }
        // A boundary does not manufacture constructor evidence. When the
        // producer view is opaque all the way through any earlier anchors,
        // retain only the exact runtime expression so static pattern matching
        // correctly reports an unknown value.
        if (structuralValue(structure) == .expr) return .{ .expr = runtime };
        const stored_structure = try self.arena.allocator().create(Value);
        stored_structure.* = structure;
        return .{ .runtime_anchor = .{
            .runtime = runtime,
            .structure = stored_structure,
        } };
    }

    /// Rebase initializer structure onto the exact runtime value which owns
    /// it. Every structural node with an accessible runtime position keeps
    /// both views: consumers can specialize through `structure`, while an
    /// opaque use materializes `runtime` without rebuilding or copying it.
    /// A child whose runtime position cannot be expressed (tag payloads and
    /// callable captures) remains structural only when the scope proof shows
    /// all of its leaves already outlive the initializer.
    fn reanchorRecursiveValue(
        self: *Cloner,
        root_value: Value,
        root_runtime: Ast.ExprId,
        initializer_bindings: BindingChain,
        budget: *u32,
        root_has_value_type: bool,
    ) Common.LowerError!?Value {
        // Structures nest as deeply as their constructors, so each record,
        // tuple, or nominal waits on an explicit frame for its components,
        // entered in pre-order so the budget reaches the same nodes a direct
        // walk does.
        const Frame = struct {
            structure: Value,
            runtime: Ast.ExprId,
            runtime_has_value_type: bool,
            values: []Value = &.{},
            fields: []FieldValue = &.{},
            next: usize = 0,
        };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(self.pass.allocator);
        const arena = self.arena.allocator();
        var delivered: ?Value = null;
        var request: ?struct { value: Value, runtime: Ast.ExprId, has_value_type: bool } = .{
            .value = root_value,
            .runtime = root_runtime,
            .has_value_type = root_has_value_type,
        };
        while (true) {
            if (request) |pending| {
                request = null;
                switch (try self.enterReanchor(pending.value, pending.runtime, initializer_bindings, budget, pending.has_value_type)) {
                    .done => |value| delivered = value,
                    .structure => |structure| {
                        var frame: Frame = .{
                            .structure = structure,
                            .runtime = pending.runtime,
                            .runtime_has_value_type = pending.has_value_type,
                        };
                        switch (structure) {
                            .record => |record| frame.fields = try arena.alloc(FieldValue, record.fields.len),
                            .tuple => |tuple| frame.values = try arena.alloc(Value, tuple.items.len),
                            .nominal => {},
                            .expr, .runtime_anchor, .static_data_candidate, .tag, .callable => unreachable,
                        }
                        try frames.append(self.pass.allocator, frame);
                    },
                }
            }
            if (frames.items.len == 0) return delivered;
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next > 0) {
                const index = frame.next - 1;
                switch (frame.structure) {
                    .record => |record| frame.fields[index] = .{
                        .name = record.fields[index].name,
                        .value = delivered orelse Common.invariant("record field with a runtime position could not be recursively anchored"),
                    },
                    .tuple => frame.values[index] = delivered orelse
                        Common.invariant("tuple item with a runtime position could not be recursively anchored"),
                    .nominal => {},
                    .expr, .runtime_anchor, .static_data_candidate, .tag, .callable => unreachable,
                }
            }
            switch (frame.structure) {
                .record => |record| if (frame.next < record.fields.len) {
                    const field = record.fields[frame.next];
                    frame.next += 1;
                    const field_runtime = try self.addFieldAccessExpr(
                        valueType(self.pass.program, field.value),
                        frame.runtime,
                        field.name,
                    );
                    request = .{ .value = field.value, .runtime = field_runtime, .has_value_type = true };
                    continue;
                },
                .tuple => |tuple| if (frame.next < tuple.items.len) {
                    const index = frame.next;
                    frame.next += 1;
                    const item = tuple.items[index];
                    const item_runtime = try self.addExpr(.{ .ty = valueType(self.pass.program, item), .data = .{ .tuple_access = .{
                        .tuple = frame.runtime,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    request = .{ .value = item, .runtime = item_runtime, .has_value_type = true };
                    continue;
                },
                .nominal => |nominal| if (frame.next == 0) {
                    frame.next = 1;
                    request = .{ .value = nominal.backing.*, .runtime = frame.runtime, .has_value_type = false };
                    continue;
                },
                .expr, .runtime_anchor, .static_data_candidate, .tag, .callable => unreachable,
            }
            const finished = frames.pop().?;
            const structure: Value = switch (finished.structure) {
                .record => |record| .{ .record = .{ .ty = record.ty, .fields = finished.fields } },
                .tuple => |tuple| .{ .tuple = .{ .ty = tuple.ty, .items = finished.values } },
                .nominal => |nominal| blk: {
                    const inner = delivered orelse {
                        delivered = if (finished.runtime_has_value_type) Value{ .expr = finished.runtime } else null;
                        continue;
                    };
                    const backing = try arena.create(Value);
                    backing.* = inner;
                    break :blk .{ .nominal = .{ .ty = nominal.ty, .backing = backing } };
                },
                .expr, .runtime_anchor, .static_data_candidate, .tag, .callable => unreachable,
            };
            delivered = if (finished.runtime_has_value_type) try self.runtimeAnchoredValue(structure, finished.runtime) else structure;
        }
    }

    const ReanchorEntry = union(enum) {
        /// The value's rebased form, decided without components.
        done: ?Value,
        /// A record, tuple, or nominal whose components are rebased next.
        structure: Value,
    };

    /// Spend budget on a value and any anchors around it, deciding it at once
    /// unless it is a record, tuple, or nominal.
    fn enterReanchor(
        self: *Cloner,
        start: Value,
        runtime: Ast.ExprId,
        initializer_bindings: BindingChain,
        budget: *u32,
        runtime_has_value_type: bool,
    ) Common.LowerError!ReanchorEntry {
        var value = start;
        while (true) {
            if (budget.* == 0) return .{ .done = if (runtime_has_value_type) Value{ .expr = runtime } else null };
            budget.* -= 1;
            switch (value) {
                .runtime_anchor => |anchor| value = anchor.structure.*,
                .expr, .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => break,
            }
        }
        switch (value) {
            .expr => |expr| {
                if (try self.exprCanSubstitute(expr) and
                    !try initializer_bindings.referencedByExpr(self.pass.allocator, self.pass.program, expr)) return .{ .done = value };
                return .{ .done = if (runtime_has_value_type) Value{ .expr = runtime } else null };
            },
            .runtime_anchor => unreachable,
            .record, .tuple, .nominal => return .{ .structure = value },
            .static_data_candidate, .tag, .callable => {
                if (try self.valueContainsNonReusableOrInitializerLocalExpr(initializer_bindings, value, budget)) {
                    return .{ .done = if (runtime_has_value_type) Value{ .expr = runtime } else null };
                }
                return .{ .done = if (runtime_has_value_type) try self.runtimeAnchoredValue(value, runtime) else value };
            },
        }
    }

    const SourceContext = struct {
        loc: SourceLoc,
        region: Region,
        inline_scope: Ast.InlineScopeId,

        fn restore(self: SourceContext, cloner: *Cloner) void {
            cloner.current_loc = self.loc;
            cloner.current_region = self.region;
            cloner.current_inline_scope = self.inline_scope;
        }
    };

    fn enterStmtSource(self: *Cloner, stmt_id: Ast.StmtId) Allocator.Error!SourceContext {
        const saved = SourceContext{
            .loc = self.current_loc,
            .region = self.current_region,
            .inline_scope = self.current_inline_scope,
        };
        errdefer saved.restore(self);
        try self.adoptStmtInlineScope(stmt_id);
        const stmt_loc = self.pass.program.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) self.current_loc = stmt_loc;
        const stmt_region = self.pass.program.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) self.current_region = stmt_region;
        return saved;
    }

    fn activeRecursiveFieldTupleReadRoot(self: *Cloner, value: Value) ?Ast.ExprId {
        const expr_id = switch (value) {
            .expr => |expr| expr,
            .runtime_anchor => |anchor| anchor.runtime,
            .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => return null,
        };
        const data = self.pass.program.getExpr(expr_id).data;
        if (data != .field_access and data != .tuple_access) return null;
        return self.activeRecursiveFieldTupleReadBase(expr_id);
    }

    fn activeRecursiveFieldTupleReadBase(self: *Cloner, start: Ast.ExprId) ?Ast.ExprId {
        var expr_id = start;
        while (true) {
            const data = self.pass.program.getExpr(expr_id).data;
            if (data == .local) return if (self.active_recursive_value_locals.contains(data.local)) expr_id else null;
            if (data == .field_access) {
                expr_id = data.field_access.receiver;
            } else if (data == .tuple_access) {
                expr_id = data.tuple_access.tuple;
            } else return null;
        }
    }

    fn callableCaptureAbiDigest(
        self: *Cloner,
        source_captures: []const Ast.TypedLocal,
        values: []const CaptureValue,
    ) names.TypeDigest {
        var hasher = TypeDigestHasher.init();
        hasher.update("roc.spec_constr.callable_capture_abi.v2");
        var word: [4]u8 = undefined;
        std.mem.writeInt(u32, &word, @intCast(source_captures.len), .little);
        hasher.update(&word);
        for (source_captures) |capture| {
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const value = callableCaptureValueForId(values, id) orelse
                Common.invariant("rewritten callable had no value for a source capture slot");
            std.mem.writeInt(u32, &word, @intFromEnum(id), .little);
            hasher.update(&word);
            const digest = self.pass.program.types.representationDigestCached(&self.pass.program.names, valueType(self.pass.program, value), null);
            hasher.update(&digest.bytes);
        }
        return .{ .bytes = hasher.finalResult() };
    }

    fn callableCaptureValueForId(values: []const CaptureValue, id: check.CheckedModule.CaptureId) ?Value {
        for (values) |capture_value| {
            if (capture_value.id == id) return capture_value.value;
        }
        return null;
    }

    fn copyValue(self: *Cloner, value: Value) Allocator.Error!*const Value {
        const out = try self.arena.allocator().create(Value);
        out.* = value;
        return out;
    }

    fn adoptExprInlineScope(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!void {
        try self.adoptInlineScope(self.pass.program.exprInlineScope(expr_id));
    }

    fn adoptStmtInlineScope(self: *Cloner, stmt_id: Ast.StmtId) Allocator.Error!void {
        try self.adoptInlineScope(self.pass.program.stmtInlineScope(stmt_id));
    }

    fn adoptInlineScope(self: *Cloner, source: Ast.InlineScopeId) Allocator.Error!void {
        if (source == Ast.InlineScopeId.none) return;
        if (self.current_inline_scope == Ast.InlineScopeId.none) {
            self.current_inline_scope = source;
            return;
        }
        self.current_inline_scope = try self.rebaseInlineScope(source, self.current_inline_scope);
    }

    fn rebaseInlineScope(
        self: *Cloner,
        source: Ast.InlineScopeId,
        outer: Ast.InlineScopeId,
    ) Allocator.Error!Ast.InlineScopeId {
        if (source == Ast.InlineScopeId.none) return outer;
        if (self.inlineScopeCovers(outer, source)) return outer;
        const key = InlineScopeRebasePair{ .source = source, .outer = outer };
        if (self.rebased_inline_scopes.get(key)) |existing| return existing;

        // Collect the frames of `source` that `outer` does not already carry,
        // innermost first. Walking iteratively keeps a deep inline stack from
        // overflowing the compiler's own stack.
        var chain = std.ArrayList(Ast.InlineScopeId).empty;
        defer chain.deinit(self.pass.allocator);

        var base = outer;
        var cursor = source;
        while (cursor != Ast.InlineScopeId.none) {
            if (self.inlineScopeCovers(outer, cursor)) break;
            if (self.rebased_inline_scopes.get(.{ .source = cursor, .outer = outer })) |existing| {
                base = existing;
                break;
            }
            try chain.append(self.pass.allocator, cursor);
            cursor = self.pass.program.inlineScope(cursor).parent;
        }

        var i = chain.items.len;
        while (i > 0) {
            i -= 1;
            const src = chain.items[i];
            const original = self.pass.program.inlineScope(src);
            const rebased = try self.pass.program.addInlineScope(.{
                .source_symbol = original.source_symbol,
                .source_loc = original.source_loc,
                .call_site = original.call_site,
                .parent = base,
            });
            const rebase_key = InlineScopeRebasePair{ .source = src, .outer = outer };
            try self.rebased_inline_scope_changes.ensureUnusedCapacity(self.pass.allocator, 1);
            try self.inline_scope_origins.put(rebased, src);
            errdefer _ = self.inline_scope_origins.remove(rebased);
            try self.rebased_inline_scopes.putNoClobber(rebase_key, rebased);
            self.rebased_inline_scope_changes.appendAssumeCapacity(rebase_key);
            base = rebased;
        }
        return base;
    }

    /// Whether `outer` already stands for the frame `frame`, either because
    /// `frame` is `outer` itself, because `frame` is one of `outer`'s ancestors,
    /// or because `outer` is a re-based copy of `frame`.
    ///
    /// Re-basing a frame `outer` already carries would append a duplicate copy
    /// of it. SpecConstr re-clones an already-inlined body once per
    /// distribution step, so every duplicate is re-duplicated by the next step
    /// and the inline stack grows without bound.
    fn inlineScopeCovers(self: *Cloner, outer: Ast.InlineScopeId, frame: Ast.InlineScopeId) bool {
        if (self.inline_scope_origins.get(outer) == frame) return true;
        var cursor = outer;
        while (cursor != Ast.InlineScopeId.none) {
            if (cursor == frame) return true;
            cursor = self.pass.program.inlineScope(cursor).parent;
        }
        return false;
    }

    fn enterInlineScope(self: *Cloner, callee: Ast.FnId, call_site: SourceLoc) Allocator.Error!void {
        const source_fn = self.pass.program.getFn(callee);
        self.current_inline_scope = try self.pass.program.addInlineScope(.{
            .source_symbol = source_fn.symbol,
            .source_loc = switch (self.pass.sourceBody(callee)) {
                .roc => |body| self.pass.program.exprLoc(body),
                .hosted => SourceLoc.none,
            },
            .call_site = call_site,
            .parent = self.current_inline_scope,
        });
    }

    fn addFieldAccessExpr(
        self: *Cloner,
        ty: Type.TypeId,
        receiver: Ast.ExprId,
        field: names.RecordFieldNameId,
    ) Allocator.Error!Ast.ExprId {
        const segments = try self.pass.program.addFieldAccessSegmentSpan(&.{.{ .field = field }});
        return try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = receiver,
            .segments = segments,
        } } });
    }

    fn addExpr(self: *Cloner, expr: Ast.Expr) Allocator.Error!Ast.ExprId {
        const saved_loc = self.pass.program.current_loc;
        defer self.pass.program.current_loc = saved_loc;
        const saved_region = self.pass.program.current_region;
        defer self.pass.program.current_region = saved_region;
        const saved_inline_scope = self.pass.program.current_inline_scope;
        defer self.pass.program.current_inline_scope = saved_inline_scope;
        self.pass.program.current_loc = self.current_loc;
        self.pass.program.current_region = self.current_region;
        self.pass.program.current_inline_scope = self.current_inline_scope;
        return try self.pass.program.addExpr(expr);
    }

    fn addStmt(self: *Cloner, stmt: Ast.Stmt) Allocator.Error!Ast.StmtId {
        const saved_loc = self.pass.program.current_loc;
        defer self.pass.program.current_loc = saved_loc;
        const saved_region = self.pass.program.current_region;
        defer self.pass.program.current_region = saved_region;
        const saved_inline_scope = self.pass.program.current_inline_scope;
        defer self.pass.program.current_inline_scope = saved_inline_scope;
        self.pass.program.current_loc = self.current_loc;
        self.pass.program.current_region = self.current_region;
        self.pass.program.current_inline_scope = self.current_inline_scope;
        return try self.pass.program.addStmt(stmt);
    }
};

/// Debug-only lexical-scope walk of a rewritten function body. Every `.local`
/// reference must resolve to a binding still in scope; the initial scope is
/// seeded with the function's arguments and recomputed captures. Binders enter
/// scope as the walk descends into the region they govern and leave when it
/// ascends, mirroring `lift.zig`'s capture walk (`collectExpr`/
/// `bindPat`) so the same reference set is judged, but asserting membership
/// rather than recording free variables.
const BodyLocalScope = struct {
    program: *const Ast.Program,
    allocator: Allocator,
    fn_index: ?usize,
    bound: collections.DenseMap(Ast.LocalId, u32),
    joins: collections.DenseMap(Ast.JoinPointId, u32),

    fn checkUse(self: *BodyLocalScope, local: Ast.LocalId) void {
        if (self.bound.contains(local)) return;
        const fn_index = self.fn_index orelse Common.invariantFmt(
            "static initializer references local {d} (`{s}`) bound outside its own scope",
            .{ @intFromEnum(local), self.program.localName(local) },
        );
        const func = self.program.getFnAt(fn_index);
        Common.invariantFmt(
            "rewritten fn {d} (symbol {d}) references local {d} (`{s}`) bound by no enclosing scope, argument, or capture",
            .{ fn_index, @intFromEnum(func.symbol), @intFromEnum(local), self.program.localName(local) },
        );
    }

    fn bind(self: *BodyLocalScope, local: Ast.LocalId) Allocator.Error!void {
        const entry = try self.bound.getOrPut(local);
        entry.value_ptr.* = if (entry.found_existing) entry.value_ptr.* + 1 else 1;
    }

    fn unbind(self: *BodyLocalScope, local: Ast.LocalId) void {
        const entry = self.bound.getPtr(local) orelse return;
        if (entry.* <= 1) {
            _ = self.bound.remove(local);
        } else {
            entry.* -= 1;
        }
    }

    fn unbindAll(self: *BodyLocalScope, locals: []const Ast.LocalId) void {
        var index = locals.len;
        while (index > 0) {
            index -= 1;
            self.unbind(locals[index]);
        }
    }

    /// One step of the scope walk. Scopes are opened and closed explicitly;
    /// a binder records into the innermost open scope, and closing it unbinds
    /// what it recorded in reverse.
    const Work = union(enum) {
        expr: Ast.ExprId,
        stmt: Ast.StmtId,
        bind_pat: Ast.PatId,
        bind_local: Ast.LocalId,
        bind_typed_locals: Ast.Span(Ast.TypedLocal),
        open_scope,
        close_scope,
        close_join: Ast.JoinPointId,
    };

    const Walk = struct {
        scope: *BodyLocalScope,
        work: std.ArrayList(Work) = .empty,
        added: std.ArrayList(Ast.LocalId) = .empty,
        scope_starts: std.ArrayList(usize) = .empty,

        fn deinit(walk: *Walk) void {
            walk.work.deinit(walk.scope.allocator);
            walk.added.deinit(walk.scope.allocator);
            walk.scope_starts.deinit(walk.scope.allocator);
        }

        fn push(walk: *Walk, item: Work) Allocator.Error!void {
            try walk.work.append(walk.scope.allocator, item);
        }

        fn pushExprSpan(walk: *Walk, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
            const values = walk.scope.program.exprSpan(span);
            for (0..values.len) |index| try walk.push(.{ .expr = GuardedList.at(values, index) });
        }

        fn pushStmtSpan(walk: *Walk, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
            const values = walk.scope.program.stmtSpan(span);
            for (0..values.len) |index| try walk.push(.{ .stmt = GuardedList.at(values, index) });
        }

        fn pushCaptureOperands(walk: *Walk, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
            const operands = walk.scope.program.captureOperandSpan(span);
            for (0..operands.len) |index| try walk.push(.{ .expr = GuardedList.at(operands, index).value });
        }

        fn pushFieldExprs(walk: *Walk, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
            const field_exprs = walk.scope.program.fieldExprSpan(span);
            for (0..field_exprs.len) |index| try walk.push(.{ .expr = GuardedList.at(field_exprs, index).value });
        }

        fn record(walk: *Walk, local: Ast.LocalId) Allocator.Error!void {
            try walk.scope.bind(local);
            try walk.added.append(walk.scope.allocator, local);
        }

        const Binder = struct {
            walk: *Walk,

            pub fn bindLocal(binder: Binder, local: Ast.LocalId) Allocator.Error!void {
                try binder.walk.record(local);
            }
        };

        fn run(walk: *Walk, root: Ast.ExprId) Allocator.Error!void {
            try walk.push(.{ .expr = root });
            while (walk.work.pop()) |item| {
                // Each step appends its sub-steps in evaluation order; they
                // are reversed onto the stack so they run in that order.
                const start = walk.work.items.len;
                try walk.step(item);
                std.mem.reverse(Work, walk.work.items[start..]);
            }
        }

        fn step(walk: *Walk, item: Work) Allocator.Error!void {
            const self = walk.scope;
            switch (item) {
                .open_scope => try walk.scope_starts.append(self.allocator, walk.added.items.len),
                .close_scope => {
                    const start = walk.scope_starts.pop() orelse
                        Common.invariant("body-local validation closed a scope it never opened");
                    self.unbindAll(walk.added.items[start..]);
                    walk.added.shrinkRetainingCapacity(start);
                },
                .bind_pat => |pat| try Ast.forEachBoundLocal(self.allocator, self.program, pat, Binder{ .walk = walk }),
                .bind_local => |local| try walk.record(local),
                .bind_typed_locals => |span| {
                    const locals = self.program.typedLocalSpan(span);
                    for (0..locals.len) |index| try walk.record(GuardedList.at(locals, index).local);
                },
                .close_join => |id| if (!self.joins.remove(id)) {
                    Common.invariant("rewritten body lost an active join point during validation");
                },
                .stmt => |stmt_id| switch (self.program.getStmt(stmt_id)) {
                    .uninitialized => |pat| try walk.push(.{ .bind_pat = pat }),
                    .let_ => |let_| if (let_.recursive) {
                        try walk.push(.{ .bind_pat = let_.pat });
                        try walk.push(.{ .expr = let_.value });
                    } else {
                        try walk.push(.{ .expr = let_.value });
                        try walk.push(.{ .bind_pat = let_.pat });
                    },
                    .expr,
                    .expect,
                    .dbg,
                    => |expr| try walk.push(.{ .expr = expr }),
                    .return_ => |ret| try walk.push(.{ .expr = ret.value }),
                    .crash, .checked_error => {},
                },
                .expr => |expr_id| try walk.stepExpr(expr_id),
            }
        }

        fn stepExpr(walk: *Walk, expr_id: Ast.ExprId) Allocator.Error!void {
            const self = walk.scope;
            switch (self.program.getExpr(expr_id).data) {
                .local => |local| self.checkUse(local),
                .unit,
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .bytes_lit,
                .uninitialized,
                .uninitialized_payload,
                .crash,
                .checked_error,
                .comptime_exhaustiveness_failed,
                .@"unreachable",
                => {},
                .lambda,
                .def_ref,
                .fn_def,
                => Common.invariant("pre-lift function expression reached body-local validation"),
                .fn_ref => |fn_ref| try walk.pushCaptureOperands(fn_ref.captures),
                .list,
                .tuple,
                => |items| try walk.pushExprSpan(items),
                .record => |fields| try walk.pushFieldExprs(fields),
                .record_update => |update| {
                    try walk.push(.{ .expr = update.base });
                    try walk.pushFieldExprs(update.fields);
                },
                .tag => |tag| try walk.pushExprSpan(tag.payloads),
                .static_data_candidate => |candidate| try walk.push(.{ .expr = candidate.runtime_expr }),
                .comptime_value => |candidate| try walk.push(.{ .expr = candidate.initializer }),
                .typed_boundary => |boundary| try walk.push(.{ .expr = boundary.value }),
                .nominal,
                .dbg,
                .expect,
                => |child| try walk.push(.{ .expr = child }),
                .return_ => |ret| try walk.push(.{ .expr = ret.value }),
                .expect_err => |expect_err| try walk.push(.{ .expr = expect_err.msg }),
                .literal_rejected => |rejected| try walk.push(.{ .expr = rejected.msg }),
                .comptime_branch_taken => |taken| try walk.push(.{ .expr = taken.body }),
                .let_ => |let_| {
                    try walk.push(.{ .expr = let_.value });
                    try walk.push(.open_scope);
                    try walk.push(.{ .bind_pat = let_.bind });
                    try walk.push(.{ .expr = let_.rest });
                    try walk.push(.close_scope);
                },
                .call_value => |call| {
                    try walk.push(.{ .expr = call.callee });
                    try walk.pushExprSpan(call.args);
                },
                .call_proc => |call| {
                    try walk.pushExprSpan(call.args);
                    try walk.pushCaptureOperands(call.captures);
                },
                .low_level => |call| try walk.pushExprSpan(call.args),
                .field_access => |field| try walk.push(.{ .expr = field.receiver }),
                .tuple_access => |access| try walk.push(.{ .expr = access.tuple }),
                .structural_eq => |eq| {
                    try walk.push(.{ .expr = eq.lhs });
                    try walk.push(.{ .expr = eq.rhs });
                },
                .structural_hash => |hash| {
                    try walk.push(.{ .expr = hash.value });
                    try walk.push(.{ .expr = hash.hasher });
                },
                .match_ => |match| {
                    try walk.push(.{ .expr = match.scrutinee });
                    const branches = self.program.branchSpan(match.branches);
                    for (0..branches.len) |index| {
                        const branch = GuardedList.at(branches, index);
                        try walk.push(.open_scope);
                        try walk.push(.{ .bind_pat = branch.pat });
                        try walk.pushStmtSpan(branch.bindings);
                        if (branch.guard) |guard| try walk.push(.{ .expr = guard });
                        try walk.push(.{ .expr = branch.body });
                        try walk.push(.close_scope);
                    }
                },
                .if_ => |if_| {
                    const branches = self.program.ifBranchSpan(if_.branches);
                    for (0..branches.len) |index| {
                        const branch = GuardedList.at(branches, index);
                        try walk.push(.{ .expr = branch.cond });
                        try walk.push(.{ .expr = branch.body });
                    }
                    try walk.push(.{ .expr = if_.final_else });
                },
                .if_initialized_payload => |payload_switch| {
                    // The condition's walk leaves the bound set as it found
                    // it, so the payload can be checked before it runs.
                    self.checkUse(payload_switch.payload);
                    try walk.push(.{ .expr = payload_switch.cond });
                    try walk.push(.{ .expr = payload_switch.initialized });
                    try walk.push(.{ .expr = payload_switch.uninitialized });
                },
                .try_sequence => |sequence| {
                    if (sequence.err_target) |target| {
                        const arity = self.joins.get(target) orelse
                            Common.invariant("rewritten Try sequence targeted a join point outside lexical scope");
                        if (arity != 1) Common.invariant("rewritten Try sequence error target did not have one parameter");
                    }
                    try walk.push(.{ .expr = sequence.try_expr });
                    try walk.push(.open_scope);
                    try walk.push(.{ .bind_local = sequence.ok_local });
                    try walk.push(.{ .expr = sequence.ok_body });
                    try walk.push(.close_scope);
                },
                .try_record_sequence => |sequence| {
                    if (sequence.err_target) |target| {
                        const arity = self.joins.get(target) orelse
                            Common.invariant("rewritten Try record sequence targeted a join point outside lexical scope");
                        if (arity != 1) Common.invariant("rewritten Try record sequence error target did not have one parameter");
                    }
                    try walk.push(.{ .expr = sequence.try_expr });
                    try walk.push(.open_scope);
                    try walk.push(.{ .bind_local = sequence.value_local });
                    try walk.push(.{ .bind_local = sequence.rest_local });
                    try walk.push(.{ .expr = sequence.ok_body });
                    try walk.push(.close_scope);
                },
                .block => |block| {
                    try walk.push(.open_scope);
                    try walk.pushStmtSpan(block.statements);
                    try walk.push(.{ .expr = block.final_expr });
                    try walk.push(.close_scope);
                },
                .loop_ => |loop| {
                    try walk.pushExprSpan(loop.initial_values);
                    try walk.push(.open_scope);
                    try walk.push(.{ .bind_typed_locals = loop.params });
                    try walk.push(.{ .expr = loop.body });
                    try walk.push(.close_scope);
                },
                .break_ => |maybe| if (maybe) |value| try walk.push(.{ .expr = value }),
                .continue_ => |continue_| try walk.pushExprSpan(continue_.values),
                .join_point => |join_point| {
                    const join_entry = try self.joins.getOrPut(join_point.id);
                    if (join_entry.found_existing) {
                        Common.invariant("rewritten body redeclared an active join point id");
                    }
                    join_entry.value_ptr.* = join_point.params.len;
                    const retained = self.program.typedLocalSpan(join_point.retained);
                    for (0..retained.len) |index| self.checkUse(GuardedList.at(retained, index).local);
                    try walk.push(.open_scope);
                    try walk.push(.{ .bind_typed_locals = join_point.params });
                    try walk.push(.{ .expr = join_point.body });
                    try walk.push(.close_scope);
                    try walk.push(.{ .expr = join_point.remainder });
                    try walk.push(.{ .close_join = join_point.id });
                },
                .jump => |jump| {
                    const arity = self.joins.get(jump.target) orelse
                        Common.invariant("rewritten body jumped to a join point outside lexical scope");
                    if (arity != jump.args.len) {
                        Common.invariant("rewritten body jump arity differed from its join point parameters");
                    }
                    const update_params = self.program.typedLocalSpan(jump.loop_params);
                    if (update_params.len != jump.loop_values.len) {
                        Common.invariant("rewritten body jump loop-update arity differed");
                    }
                    for (0..update_params.len) |index| self.checkUse(GuardedList.at(update_params, index).local);
                    try walk.pushExprSpan(jump.loop_values);
                    try walk.pushExprSpan(jump.args);
                },
            }
        }
    };

    /// Walk `expr_id` on an explicit work stack, checking every use against
    /// the bindings in scope at that point.
    fn walkExpr(self: *BodyLocalScope, expr_id: Ast.ExprId) Allocator.Error!void {
        var walk: Walk = .{ .scope = self };
        defer walk.deinit();
        try walk.run(expr_id);
    }
};

fn localExpr(program: *const Ast.Program, expr_id: Ast.ExprId) ?Ast.LocalId {
    const data = program.getExpr(expr_id).data;
    return if (data == .local) data.local else null;
}

fn fnBodySizeWithin(allocator: Allocator, program: *const Ast.Program, body: Ast.FnBody, limit: usize) Allocator.Error!BodySize {
    return switch (body) {
        .roc => |expr| try exprBodySizeWithin(allocator, program, expr, limit),
        .hosted => .{ .exact = 0 },
    };
}

/// Count the expressions in `expr_id`, stopping once more than `limit` have
/// been seen. The count is order-independent, so the walk keeps its own
/// work stack.
fn exprBodySizeWithin(allocator: Allocator, program: *const Ast.Program, expr_id: Ast.ExprId, limit: usize) Allocator.Error!BodySize {
    var remaining = limit;
    var stack: std.ArrayList(Ast.ExprChild) = .empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, .{ .expr = expr_id });
    while (stack.pop()) |child| switch (child) {
        .stmt => |stmt_id| try Ast.appendStmtChildren(allocator, program, stmt_id, &stack),
        .expr => |id| {
            if (remaining == 0) return .over_limit;
            remaining -= 1;
            switch (program.getExpr(id).data) {
                .lambda,
                .def_ref,
                .fn_def,
                => Common.invariant("pre-lift function expression reached SpecConstr body-size counting"),
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => try Ast.appendChildren(allocator, program, id, &stack),
            }
        },
    };
    return .{ .exact = limit - remaining };
}

/// Program-wide procedure-use snapshot shared by SpecConstr graph rewrites.
/// A transformation that changes edges requires a fresh snapshot; consumers
/// never combine usage counts from different program generations.
const ProgramProcedureUsage = struct {
    tail_self_calls: []TailSelfCallSummary,
    fn_uses: []ProcedureUse,

    fn collect(allocator: Allocator, program: *const Ast.Program) Allocator.Error!ProgramProcedureUsage {
        const fn_count = program.fnCount();
        const tail_self_calls = try allocator.alloc(TailSelfCallSummary, fn_count);
        errdefer allocator.free(tail_self_calls);
        @memset(tail_self_calls, .{});
        const fn_uses = try allocator.alloc(ProcedureUse, fn_count);
        errdefer allocator.free(fn_uses);
        @memset(fn_uses, .{});

        for (0..fn_count) |owner_index| {
            const owner: Ast.FnId = @enumFromInt(@as(u32, @intCast(owner_index)));
            const body = switch (program.getFnAt(owner_index).body) {
                .roc => |body| body,
                .hosted => continue,
            };
            // Both walks answer questions the body's recorded shapes already
            // settle for a body without the shape: no return expression, and
            // no self call means an empty summary.
            const shapes = program.getFnAt(owner_index).shapes;
            if (builtin.mode == .Debug) {
                if (try exprContainsReturn(allocator, program, body) and !shapes.contains_return) {
                    std.debug.panic("function {d} contains a return its shapes {any} do not record", .{ owner_index, shapes });
                }
                if (!shapes.self_call) {
                    const summary = try tailSelfCallSummary(allocator, program, body, owner);
                    if (!summary.valid or summary.count != 0) {
                        std.debug.panic("function {d} calls itself although its shapes {any} do not record it", .{ owner_index, shapes });
                    }
                }
            }
            fn_uses[owner_index].contains_return = shapes.contains_return;
            tail_self_calls[owner_index] = if (shapes.self_call) try tailSelfCallSummary(allocator, program, body, owner) else .{};
            try collectAllFnUsesInExpr(allocator, program, body, owner, fn_uses);
        }
        for (program.rootsView()) |root| {
            fn_uses[@intFromEnum(root.fn_id)].value_refs += 1;
        }
        return .{
            .tail_self_calls = tail_self_calls,
            .fn_uses = fn_uses,
        };
    }

    fn takeFnUses(self: *ProgramProcedureUsage) []ProcedureUse {
        const fn_uses = self.fn_uses;
        self.fn_uses = &.{};
        return fn_uses;
    }

    fn deinit(self: *ProgramProcedureUsage, allocator: Allocator) void {
        if (self.fn_uses.len != 0) allocator.free(self.fn_uses);
        if (self.tail_self_calls.len != 0) allocator.free(self.tail_self_calls);
        self.* = undefined;
    }
};

/// Count every procedure use in `expr_id` into `uses`. Only a procedure's
/// counts and its single external call site are recorded, so visiting order
/// does not matter and the walk runs on its own work stack.
fn collectAllFnUsesInExpr(
    allocator: Allocator,
    program: *const Ast.Program,
    expr_id: Ast.ExprId,
    owner: Ast.FnId,
    uses: []ProcedureUse,
) Allocator.Error!void {
    var stack: std.ArrayList(Ast.ExprChild) = .empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, .{ .expr = expr_id });
    while (stack.pop()) |child| switch (child) {
        .stmt => |stmt_id| try Ast.appendStmtChildren(allocator, program, stmt_id, &stack),
        .expr => |id| {
            switch (program.getExpr(id).data) {
                .lambda,
                .def_ref,
                .fn_def,
                => Common.invariant("pre-lift function expression reached specialized-worker use analysis"),
                .fn_ref => |fn_ref| uses[@intFromEnum(fn_ref.fn_id)].value_refs += 1,
                .call_proc => |call| if (Ast.localDirectCallee(call)) |callee| {
                    if (callee != owner) {
                        const summary = &uses[@intFromEnum(callee)];
                        summary.external_calls += 1;
                        summary.external_call_expr = id;
                        summary.external_call_owner = owner;
                    }
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
            }
            try Ast.appendChildren(allocator, program, id, &stack);
        },
    };
}

const TailSelfCallSummary = struct {
    valid: bool = true,
    count: usize = 0,
};

/// Prove that every self call occurs in a result position from which the
/// worker returns directly. The accepted set is the exact lifted-IR
/// tail-position grammar; any self call in an operand or statement rejects
/// localization. Tail positions are visited on a work stack; the summary is
/// a count plus a validity flag, so their order does not matter.
fn tailSelfCallSummary(allocator: Allocator, program: *const Ast.Program, root: Ast.ExprId, target: Ast.FnId) Allocator.Error!TailSelfCallSummary {
    const invalid: TailSelfCallSummary = .{ .valid = false };
    var summary: TailSelfCallSummary = .{};
    var tails: std.ArrayList(Ast.ExprId) = .empty;
    defer tails.deinit(allocator);
    try tails.append(allocator, root);
    while (tails.pop()) |expr_id| switch (program.getExpr(expr_id).data) {
        .call_proc => |call| {
            if (try exprSpanCallsFn(allocator, program, call.args, target) or
                try captureOperandSpanCallsFn(allocator, program, call.captures, target))
            {
                return invalid;
            }
            if (Ast.localDirectCallee(call) == target) summary.count += 1;
        },
        .let_ => |let_| {
            if (try exprCallsFn(allocator, program, let_.value, target)) return invalid;
            try tails.append(allocator, let_.rest);
        },
        .match_ => |match| {
            if (try exprCallsFn(allocator, program, match.scrutinee, target)) return invalid;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (try stmtCallsFn(allocator, program, GuardedList.at(bindings, binding_index), target)) return invalid;
                }
                if (branch.guard) |guard| {
                    if (try exprCallsFn(allocator, program, guard, target)) return invalid;
                }
                try tails.append(allocator, branch.body);
            }
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (try exprCallsFn(allocator, program, branch.cond, target)) return invalid;
                try tails.append(allocator, branch.body);
            }
            try tails.append(allocator, if_.final_else);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                if (try stmtCallsFn(allocator, program, GuardedList.at(statements, index), target)) return invalid;
            }
            try tails.append(allocator, block.final_expr);
        },
        .join_point => |join_point| {
            try tails.append(allocator, join_point.body);
            try tails.append(allocator, join_point.remainder);
        },
        .if_initialized_payload => |payload_switch| {
            if (try exprCallsFn(allocator, program, payload_switch.cond, target)) return invalid;
            try tails.append(allocator, payload_switch.initialized);
            try tails.append(allocator, payload_switch.uninitialized);
        },
        .try_sequence => |sequence| {
            if (try exprCallsFn(allocator, program, sequence.try_expr, target)) return invalid;
            try tails.append(allocator, sequence.ok_body);
        },
        .try_record_sequence => |sequence| {
            if (try exprCallsFn(allocator, program, sequence.try_expr, target)) return invalid;
            try tails.append(allocator, sequence.ok_body);
        },
        .comptime_branch_taken => |taken| try tails.append(allocator, taken.body),
        .typed_boundary => |boundary| try tails.append(allocator, boundary.value),
        .local,
        .unit,
        .@"unreachable",
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .static_data_candidate,
        .comptime_value,
        .list,
        .tuple,
        .record,
        .record_update,
        .tag,
        .nominal,
        .lambda,
        .def_ref,
        .fn_def,
        .fn_ref,
        .call_value,
        .low_level,
        .field_access,
        .tuple_access,
        .structural_eq,
        .structural_hash,
        .uninitialized,
        .uninitialized_payload,
        .loop_,
        .break_,
        .continue_,
        .jump,
        .return_,
        .crash,
        .checked_error,
        .comptime_exhaustiveness_failed,
        .dbg,
        .expect_err,
        .literal_rejected,
        .expect,
        => if (try exprCallsFn(allocator, program, expr_id, target)) return invalid,
    };
    return summary;
}

/// Append every expression of `span`, in order, to a plain work stack.
fn pushExprSpanWork(allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.ExprId), stack: *std.ArrayList(Ast.ExprId)) Allocator.Error!void {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| try stack.append(allocator, GuardedList.at(exprs, index));
}

/// Push every expression of `span` onto an `Ast.anyExpr` search stack.
fn pushExprSpanSearch(allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.ExprId), stack: *std.ArrayList(Ast.ExprChild)) Allocator.Error!void {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| try stack.append(allocator, .{ .expr = GuardedList.at(exprs, index) });
}

/// Whether this body contains an exact checker-stamped iterator producer.
/// Debug builds use this scan to verify that a function excluded from the
/// iterator fusion phase by its recorded shapes indeed has no producer; the
/// phase itself selects bodies by the `iterator_producer` flag alone.
fn exprContainsIteratorProducer(allocator: Allocator, program: *const Ast.Program, expr_id: Ast.ExprId) Allocator.Error!bool {
    const Search = struct {
        allocator: Allocator,
        program: *const Ast.Program,

        pub fn visitExpr(search: @This(), id: Ast.ExprId, stack: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return switch (search.program.getExpr(id).data) {
                .comptime_value => .skip,
                .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached iterator-producer scan"),
                .call_proc => |call| if (isIteratorProducer(call.iterator_procedure)) .found else .descend,
                .jump => |jump| blk: {
                    try pushExprSpanSearch(search.allocator, search.program, jump.args, stack);
                    break :blk .children;
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
            };
        }

        pub fn visitStmt(_: @This(), _: Ast.StmtId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return .descend;
        }
    };
    return Ast.anyExpr(allocator, program, expr_id, Search{ .allocator = allocator, .program = program });
}

fn exprCallsFn(allocator: Allocator, program: *const Ast.Program, expr_id: Ast.ExprId, fn_id: Ast.FnId) Allocator.Error!bool {
    const Search = struct {
        program: *const Ast.Program,
        fn_id: Ast.FnId,

        pub fn visitExpr(search: @This(), id: Ast.ExprId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return switch (search.program.getExpr(id).data) {
                .comptime_value => .skip,
                .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached recursive-call scan"),
                .call_proc => |call| if (Ast.localDirectCallee(call) == search.fn_id) .found else .descend,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
            };
        }

        pub fn visitStmt(_: @This(), _: Ast.StmtId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return .descend;
        }
    };
    return Ast.anyExpr(allocator, program, expr_id, Search{ .program = program, .fn_id = fn_id });
}

fn exprSpanCallsFn(allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.ExprId), fn_id: Ast.FnId) Allocator.Error!bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        if (try exprCallsFn(allocator, program, GuardedList.at(exprs, index), fn_id)) return true;
    }
    return false;
}

fn captureOperandSpanCallsFn(allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand), fn_id: Ast.FnId) Allocator.Error!bool {
    const operands = program.captureOperandSpan(span);
    for (0..operands.len) |index| {
        if (try exprCallsFn(allocator, program, GuardedList.at(operands, index).value, fn_id)) return true;
    }
    return false;
}

fn stmtCallsFn(allocator: Allocator, program: *const Ast.Program, stmt_id: Ast.StmtId, fn_id: Ast.FnId) Allocator.Error!bool {
    return switch (program.getStmt(stmt_id)) {
        .let_ => |let_| exprCallsFn(allocator, program, let_.value, fn_id),
        .expr, .expect, .dbg => |expr| exprCallsFn(allocator, program, expr, fn_id),
        .return_ => |ret| exprCallsFn(allocator, program, ret.value, fn_id),
        .uninitialized, .crash, .checked_error => false,
    };
}

fn exprContainsReturn(allocator: Allocator, program: *const Ast.Program, expr_id: Ast.ExprId) Allocator.Error!bool {
    const Search = struct {
        program: *const Ast.Program,

        pub fn visitExpr(search: @This(), id: Ast.ExprId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return switch (search.program.getExpr(id).data) {
                .return_ => .found,
                .lambda, .def_ref, .fn_def, .comptime_value => .skip,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
            };
        }

        pub fn visitStmt(search: @This(), id: Ast.StmtId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return switch (search.program.getStmt(id)) {
                .return_ => .found,
                .let_, .expr, .expect, .dbg, .uninitialized, .crash, .checked_error => .descend,
            };
        }
    };
    return Ast.anyExpr(allocator, program, expr_id, Search{ .program = program });
}

/// Whether `expr_id` contains any reference to `local`, including through
/// nested loops, join points, and closure capture operands. Lambda and
/// function-definition leaves reach enclosing locals only through explicit
/// capture operands, which `fn_ref` and `call_proc` spans carry.
fn exprReferencesLocal(allocator: Allocator, program: *const Ast.Program, expr_id: Ast.ExprId, local: Ast.LocalId) Allocator.Error!bool {
    const Search = struct {
        program: *const Ast.Program,
        local: Ast.LocalId,

        pub fn visitExpr(search: @This(), id: Ast.ExprId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return switch (search.program.getExpr(id).data) {
                .local => |referenced| if (referenced == search.local) .found else .skip,
                .uninitialized_payload => |payload| if (payload.condition == search.local) .found else .skip,
                .join_point => |join_point| if (typedLocalSpanContains(search.program, join_point.retained, search.local)) .found else .descend,
                .lambda, .def_ref, .fn_def, .comptime_value => .skip,
                .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
            };
        }

        pub fn visitStmt(_: @This(), _: Ast.StmtId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return .descend;
        }
    };
    return Ast.anyExpr(allocator, program, expr_id, Search{ .program = program, .local = local });
}

/// What a loop body contains: whether it returns from the enclosing
/// function (`exprContainsReturn`), and which of the loop's initial locals it
/// references (`exprReferencesLocal`).
const LoopBodyContents = struct {
    contains_return: bool,
    initial_locals: []const InitialLocalUse,

    fn references(contents: LoopBodyContents, local: Ast.LocalId) bool {
        for (contents.initial_locals) |use| {
            if (use.local == local) return use.referenced;
        }
        Common.invariant("loop body contents queried a local that is not one of the loop's initial locals");
    }
};

const InitialLocalUse = struct { local: Ast.LocalId, referenced: bool };

fn initialLocalUses(arena: Allocator, program: *const Ast.Program, initial_values: anytype, len: usize) Allocator.Error![]InitialLocalUse {
    var uses: std.ArrayList(InitialLocalUse) = .empty;
    for (0..len) |index| {
        const local = localExpr(program, GuardedList.at(initial_values, index)) orelse continue;
        try uses.append(arena, .{ .local = local, .referenced = false });
    }
    return uses.items;
}

/// A loop whose body the contents walk is inside.
const OpenLoop = struct {
    body: Ast.ExprId,
    contains_return: bool = false,
    uses: []InitialLocalUse,
};

/// One open loop's initial local, awaiting a reference in its body.
const LocalRegistration = struct { open_index: usize, use_index: usize };

/// Walk one loop body on an explicit stack, recording the contents of it and
/// of every loop nested in it. A reference marks each open loop whose initial
/// locals include the referenced local, and a return marks every open loop,
/// each at most once, so the walk is linear in the body.
fn walkLoopBodyContents(pass: *Pass, root_body: Ast.ExprId, root_uses: []InitialLocalUse) Allocator.Error!LoopBodyContents {
    const allocator = pass.allocator;
    const program: *const Ast.Program = pass.program;
    const Item = union(enum) {
        enter: Ast.ExprChild,
        open: struct { body: Ast.ExprId, uses: []InitialLocalUse },
        close,
    };
    var stack: std.ArrayList(Item) = .empty;
    defer stack.deinit(allocator);
    var children: std.ArrayList(Ast.ExprChild) = .empty;
    defer children.deinit(allocator);
    var open: std.ArrayList(OpenLoop) = .empty;
    defer open.deinit(allocator);
    var registered: std.AutoHashMapUnmanaged(Ast.LocalId, std.ArrayList(LocalRegistration)) = .empty;
    defer {
        var lists = registered.valueIterator();
        while (lists.next()) |list| list.deinit(allocator);
        registered.deinit(allocator);
    }
    var root_contents: LoopBodyContents = undefined;

    try stack.append(allocator, .{ .open = .{ .body = root_body, .uses = root_uses } });
    while (stack.pop()) |item| {
        children.clearRetainingCapacity();
        switch (item) {
            .open => |loop| {
                const open_index = open.items.len;
                try open.append(allocator, .{ .body = loop.body, .uses = loop.uses });
                for (loop.uses, 0..) |use, use_index| {
                    const entry = try registered.getOrPut(allocator, use.local);
                    if (!entry.found_existing) entry.value_ptr.* = .empty;
                    try entry.value_ptr.append(allocator, .{ .open_index = open_index, .use_index = use_index });
                }
                try stack.append(allocator, .close);
                try stack.append(allocator, .{ .enter = .{ .expr = loop.body } });
                continue;
            },
            .close => {
                const loop = open.pop().?;
                const open_index = open.items.len;
                for (loop.uses) |use| {
                    const list = registered.getPtr(use.local).?;
                    while (list.items.len > 0 and list.items[list.items.len - 1].open_index == open_index) _ = list.pop();
                }
                const contents: LoopBodyContents = .{ .contains_return = loop.contains_return, .initial_locals = loop.uses };
                if (open_index == 0) {
                    root_contents = contents;
                } else if (program.isFrozenExpr(loop.body)) {
                    try pass.loop_body_contents.put(allocator, loop.body, contents);
                }
                continue;
            },
            .enter => |child| switch (child) {
                .stmt => |stmt_id| {
                    if (program.getStmt(stmt_id) == .return_) markOpenLoopsReturn(open.items);
                    try Ast.appendStmtChildren(allocator, program, stmt_id, &children);
                },
                .expr => |expr_id| switch (program.getExpr(expr_id).data) {
                    .lambda, .def_ref, .fn_def, .comptime_value => continue,
                    .local => |local| {
                        markLocalReferenced(open.items, &registered, local);
                        continue;
                    },
                    .uninitialized_payload => |payload| {
                        markLocalReferenced(open.items, &registered, payload.condition);
                        continue;
                    },
                    .join_point => |join_point| {
                        const retained = program.typedLocalSpan(join_point.retained);
                        for (0..retained.len) |index| {
                            markLocalReferenced(open.items, &registered, GuardedList.at(retained, index).local);
                        }
                        try Ast.appendChildren(allocator, program, expr_id, &children);
                    },
                    .return_ => {
                        markOpenLoopsReturn(open.items);
                        try Ast.appendChildren(allocator, program, expr_id, &children);
                    },
                    .loop_ => |nested| {
                        const initial_values = program.exprSpan(nested.initial_values);
                        try stack.append(allocator, .{ .open = .{
                            .body = nested.body,
                            .uses = try initialLocalUses(pass.arena.allocator(), program, initial_values, initial_values.len),
                        } });
                        for (0..initial_values.len) |index| {
                            try children.append(allocator, .{ .expr = GuardedList.at(initial_values, index) });
                        }
                    },
                    .unit, .@"unreachable", .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .break_, .continue_, .jump, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => try Ast.appendChildren(allocator, program, expr_id, &children),
                },
            },
        }
        var index = children.items.len;
        while (index > 0) {
            index -= 1;
            try stack.append(allocator, .{ .enter = children.items[index] });
        }
    }
    return root_contents;
}

fn markOpenLoopsReturn(open: []OpenLoop) void {
    var index = open.len;
    while (index > 0) {
        index -= 1;
        if (open[index].contains_return) return;
        open[index].contains_return = true;
    }
}

/// Mark every open loop that has `local` as an initial local. Each
/// registration is marked once, then dropped.
fn markLocalReferenced(
    open: []OpenLoop,
    registered: *std.AutoHashMapUnmanaged(Ast.LocalId, std.ArrayList(LocalRegistration)),
    local: Ast.LocalId,
) void {
    const list = registered.getPtr(local) orelse return;
    for (list.items) |registration| open[registration.open_index].uses[registration.use_index].referenced = true;
    list.clearRetainingCapacity();
}

fn typedLocalSpanContains(program: *const Ast.Program, span: Ast.Span(Ast.TypedLocal), local: Ast.LocalId) bool {
    const locals = program.typedLocalSpan(span);
    for (0..locals.len) |index| {
        if (GuardedList.at(locals, index).local == local) return true;
    }
    return false;
}

/// Reports whether moving `expr_id` beneath another loop would change the
/// target of a lexical `break` or `continue`. A loop owns control transfers in
/// its body, but not in its initial values. The search runs on its own work
/// stack, each entry carrying the number of loops enclosing it.
fn exprContainsFreeLoopControl(allocator: Allocator, program: *const Ast.Program, expr_id: Ast.ExprId) Allocator.Error!bool {
    const Item = struct { child: Ast.ExprChild, loop_depth: usize };
    var stack: std.ArrayList(Item) = .empty;
    defer stack.deinit(allocator);
    var children: std.ArrayList(Ast.ExprChild) = .empty;
    defer children.deinit(allocator);
    try stack.append(allocator, .{ .child = .{ .expr = expr_id }, .loop_depth = 0 });
    while (stack.pop()) |item| {
        children.clearRetainingCapacity();
        switch (item.child) {
            .stmt => |stmt_id| try Ast.appendStmtChildren(allocator, program, stmt_id, &children),
            .expr => |id| switch (program.getExpr(id).data) {
                .lambda, .def_ref, .fn_def, .comptime_value => {},
                .loop_ => |loop| {
                    try pushExprSpanSearch(allocator, program, loop.initial_values, &children);
                    try stack.append(allocator, .{ .child = .{ .expr = loop.body }, .loop_depth = item.loop_depth + 1 });
                },
                .break_, .continue_ => {
                    if (item.loop_depth == 0) return true;
                    try Ast.appendChildren(allocator, program, id, &children);
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .join_point, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => try Ast.appendChildren(allocator, program, id, &children),
            },
        }
        for (children.items) |child| try stack.append(allocator, .{ .child = child, .loop_depth = item.loop_depth });
    }
    return false;
}

/// Record the tuple fields demanded from one aggregate local, rejecting any
/// use that observes the aggregate as a whole or through a non-tuple
/// access. `LocalId` is program-global, so binders introduced below this
/// expression cannot shadow the queried identity. On rejection `used` holds
/// no meaningful answer, so the search visits positions in any order.
fn collectTupleLocalDemand(
    allocator: Allocator,
    program: *const Ast.Program,
    local: Ast.LocalId,
    root: Ast.ExprChild,
    used: []bool,
) Allocator.Error!bool {
    const Search = struct {
        allocator: Allocator,
        program: *const Ast.Program,
        local: Ast.LocalId,
        used: []bool,

        pub fn visitExpr(search: @This(), id: Ast.ExprId, stack: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            const program_ = search.program;
            return switch (program_.getExpr(id).data) {
                .local => |seen| if (seen == search.local) .found else .skip,
                .static_data_candidate,
                .comptime_value,
                .lambda,
                .def_ref,
                .fn_def,
                => .skip,
                .tuple_access => |access| blk: {
                    const receiver = program_.getExpr(access.tuple);
                    if (receiver.data == .local and receiver.data.local == search.local) {
                        if (access.elem_index >= search.used.len) break :blk .found;
                        search.used[access.elem_index] = true;
                        break :blk .skip;
                    }
                    break :blk .descend;
                },
                .join_point => |join_point| blk: {
                    if (typedLocalSpanContains(program_, join_point.retained, search.local)) @memset(search.used, true);
                    const params = program_.typedLocalSpan(join_point.params);
                    for (0..params.len) |index| {
                        if (GuardedList.at(params, index).local == search.local) {
                            try stack.append(search.allocator, .{ .expr = join_point.remainder });
                            break :blk .children;
                        }
                    }
                    break :blk .descend;
                },
                .if_initialized_payload => |payload| if (payload.payload == search.local) .found else .descend,
                .try_sequence => |sequence| blk: {
                    if (sequence.ok_local != search.local) break :blk .descend;
                    try stack.append(search.allocator, .{ .expr = sequence.try_expr });
                    break :blk .children;
                },
                .try_record_sequence => |sequence| blk: {
                    if (sequence.value_local != search.local and sequence.rest_local != search.local) break :blk .descend;
                    try stack.append(search.allocator, .{ .expr = sequence.try_expr });
                    break :blk .children;
                },
                .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .block, .loop_, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
            };
        }

        pub fn visitStmt(_: @This(), _: Ast.StmtId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return .descend;
        }
    };
    return !try Ast.anyChild(allocator, program, root, Search{
        .allocator = allocator,
        .program = program,
        .local = local,
        .used = used,
    });
}

/// Count the references to `local` in `root`. A join point's parameters and a
/// Try sequence's bound locals shadow `local` in the body they scope.
fn localUseCount(allocator: Allocator, program: *const Ast.Program, local: Ast.LocalId, root: Ast.ExprChild) Allocator.Error!usize {
    const Counter = struct {
        allocator: Allocator,
        program: *const Ast.Program,
        local: Ast.LocalId,
        count: *usize,

        pub fn visitExpr(counter: @This(), id: Ast.ExprId, stack: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            const program_ = counter.program;
            return switch (program_.getExpr(id).data) {
                .local => |seen| blk: {
                    if (seen == counter.local) counter.count.* += 1;
                    break :blk .skip;
                },
                .comptime_value,
                .lambda,
                .def_ref,
                .fn_def,
                => .skip,
                .join_point => |join_point| blk: {
                    if (typedLocalSpanContains(program_, join_point.retained, counter.local)) counter.count.* += 1;
                    const params = program_.typedLocalSpan(join_point.params);
                    for (0..params.len) |index| {
                        if (GuardedList.at(params, index).local == counter.local) {
                            try stack.append(counter.allocator, .{ .expr = join_point.remainder });
                            break :blk .children;
                        }
                    }
                    break :blk .descend;
                },
                .if_initialized_payload => |payload_switch| blk: {
                    if (payload_switch.payload == counter.local) counter.count.* += 1;
                    break :blk .descend;
                },
                .try_sequence => |sequence| blk: {
                    if (sequence.ok_local != counter.local) break :blk .descend;
                    try stack.append(counter.allocator, .{ .expr = sequence.try_expr });
                    break :blk .children;
                },
                .try_record_sequence => |sequence| blk: {
                    if (sequence.value_local != counter.local and sequence.rest_local != counter.local) break :blk .descend;
                    try stack.append(counter.allocator, .{ .expr = sequence.try_expr });
                    break :blk .children;
                },
                .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .block, .loop_, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => .descend,
            };
        }

        pub fn visitStmt(_: @This(), _: Ast.StmtId, _: *std.ArrayList(Ast.ExprChild)) Allocator.Error!Ast.ExprSearch {
            return .descend;
        }
    };
    var count: usize = 0;
    _ = try Ast.anyChild(allocator, program, root, Counter{
        .allocator = allocator,
        .program = program,
        .local = local,
        .count = &count,
    });
    return count;
}

fn localUseCountInExpr(allocator: Allocator, program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) Allocator.Error!usize {
    return localUseCount(allocator, program, local, .{ .expr = expr_id });
}

fn canReadFieldsFromExpr(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    const data = program.getExpr(expr_id).data;
    return data == .local or data == .field_access or data == .tuple_access;
}

fn shapeType(shape: Shape) Type.TypeId {
    return switch (shape) {
        .any => |ty| ty,
        .tag => |tag| tag.ty,
        .record => |record| record.ty,
        .tuple => |tuple| tuple.ty,
        .nominal => |nominal| nominal.ty,
        .callable => |callable| callable.ty,
    };
}

/// Debug enforcement of the nominal construction invariant: a structural
/// constructor expression (tag, record, tuple) must never be typed at a
/// nominal type—Monotype lowering wraps every such construction in
/// explicit `.nominal` nodes, and the static matcher relies on pattern and
/// value representations aligning exactly.
fn assertStructuralConstructionType(program: *const Ast.Program, ty: Type.TypeId) void {
    if (!std.debug.runtime_safety) return;
    var current = ty;
    while (true) {
        const content = program.types.get(current);
        if (content != .named) return;
        const named = content.named;
        const backing = named.backing orelse return;
        switch (named.kind) {
            .alias => current = backing.ty,
            .nominal, .@"opaque" => Common.invariant("structural constructor value was typed at a nominal type without its nominal wrapper"),
        }
    }
}

const NominalConstructionLayer = struct {
    named: Type.TypeId,
    backing: Type.TypeId,
};

fn nominalConstructionLayer(program: *const Ast.Program, ty: Type.TypeId) ?NominalConstructionLayer {
    var current = ty;
    while (true) {
        const content = program.types.get(current);
        if (content != .named) return null;
        const named = content.named;
        const backing = named.backing orelse return null;
        switch (named.kind) {
            .alias => current = backing.ty,
            .nominal, .@"opaque" => return .{ .named = current, .backing = backing.ty },
        }
    }
}

fn recordUpdateBackingType(program: *const Ast.Program, ty: Type.TypeId) Type.TypeId {
    var current = ty;
    while (true) {
        const content = program.types.get(current);
        if (content == .record) return current;
        if (content != .named) Common.invariant("record update had a non-record backing type");
        const backing = content.named.backing orelse
            Common.invariant("record update had a named type without an explicit backing");
        current = backing.ty;
    }
}

fn recordUpdateFieldSpan(program: *const Ast.Program, ty: Type.TypeId) Type.Span {
    const content = program.types.get(recordUpdateBackingType(program, ty));
    if (content != .record) unreachable;
    return content.record;
}

fn valueType(program: *const Ast.Program, value: Value) Type.TypeId {
    return switch (value) {
        .expr => |expr| program.getExpr(expr).ty,
        .runtime_anchor => |anchor| program.getExpr(anchor.runtime).ty,
        .static_data_candidate => |candidate| candidate.ty,
        .tag => |tag| tag.ty,
        .record => |record| record.ty,
        .tuple => |tuple| tuple.ty,
        .nominal => |nominal| nominal.ty,
        .callable => |callable| callable.ty,
    };
}

fn structuralValue(value: Value) Value {
    return structuralValueStripping(value, 0);
}

fn structuralValueStripping(start: Value, strip_depth: usize) Value {
    var value = start;
    var depth = strip_depth;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) {
            Common.invariant("structuralValue followed a value wrapper chain past the strip cap");
        }
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .expr, .tag, .record, .tuple, .nominal, .callable => return value,
        };
    }
}

/// Whether two Monotype ids denote the same type with the same
/// representation. The type store is not interned: each specialization
/// materializes its own ids, so structurally identical types reached from
/// different specializations (a call site and the callee's own body) carry
/// different ids and compare by digest. The representation digest ignores a
/// named type's checked re-entry reference, which depends on the route that
/// produced the id (an interface summary replay or an expansion), so that
/// route never changes a SpecConstr decision. It still observes backings, so
/// equal types with different representations stay distinct. Both sides
/// digest through the store's memoized construction, which is why this probe
/// (and everything that reaches it) takes the program mutable.
///
/// Aliases digest as opaque named nodes, so this deliberately answers false
/// for an alias-wrapped type against its backing: that can miss an
/// optimization but can never merge two representations invalidly.
fn sameType(program: *Ast.Program, lhs: Type.TypeId, rhs: Type.TypeId) bool {
    if (lhs == rhs) return true;
    const lhs_digest = program.types.representationDigestCached(&program.names, lhs, null);
    const rhs_digest = program.types.representationDigestCached(&program.names, rhs, null);
    return std.mem.eql(u8, &lhs_digest.bytes, &rhs_digest.bytes);
}

fn typeFieldByName(fields: anytype, name: names.RecordFieldNameId) ?Type.TypeId {
    for (0..GuardedList.borrowLen(fields)) |index| {
        const field = GuardedList.at(fields, index);
        if (field.name == name) return field.ty;
    }
    return null;
}

fn typeTagByName(
    program: *const Ast.Program,
    ty: Type.TypeId,
    name: names.TagNameId,
) ?Type.Tag {
    const content = program.types.get(ty);
    if (content != .tag_union) return null;
    const tags = program.types.tagSpan(content.tag_union);
    for (0..tags.len) |index| {
        const tag = GuardedList.at(tags, index);
        if (tag.name == name) return tag;
    }
    return null;
}

/// Content digest of a call pattern, rendered the way `patternEql` compares:
/// types by their Monotype digest, labels by text, callable targets by the
/// target function's checked source identity.
fn patternDigest(program: *Ast.Program, pattern: CallPattern) Allocator.Error!names.TypeDigest {
    var hasher = TypeDigestHasher.init();
    writePatternBytes(&hasher, "roc.spec-constr.call-pattern.v1");
    writePatternU32(&hasher, @intCast(pattern.args.len));
    for (pattern.args) |shape| try writeShapeDigest(program, &hasher, shape);
    return .{ .bytes = hasher.finalResult() };
}

/// One pending write of a shape digest: a shape, or the label written just
/// before a record field's shape.
const ShapeDigestItem = union(enum) {
    shape: Shape,
    field_label: names.RecordFieldNameId,
};

/// Write a shape's digest in pre-order, each node before its components.
/// Components are pushed last-first so the first is written next.
fn writeShapeDigest(program: *Ast.Program, hasher: *TypeDigestHasher, root: Shape) Allocator.Error!void {
    var pending = std.ArrayList(ShapeDigestItem).empty;
    defer pending.deinit(program.allocator);
    try pending.append(program.allocator, .{ .shape = root });
    while (pending.pop()) |item| {
        const shape = switch (item) {
            .field_label => |name| {
                writePatternBytes(hasher, program.names.recordFieldLabelText(name));
                continue;
            },
            .shape => |shape| shape,
        };
        writePatternBytes(hasher, @tagName(shape));
        switch (shape) {
            .any => |ty| writePatternType(program, hasher, ty),
            .tag => |tag| {
                writePatternType(program, hasher, tag.ty);
                writePatternBytes(hasher, program.names.tagLabelText(tag.name));
                writePatternU32(hasher, @intCast(tag.payloads.len));
                try pushShapeDigestItems(program, &pending, tag.payloads);
            },
            .record => |record| {
                writePatternType(program, hasher, record.ty);
                writePatternU32(hasher, @intCast(record.fields.len));
                var index = record.fields.len;
                while (index > 0) {
                    index -= 1;
                    try pending.append(program.allocator, .{ .shape = record.fields[index].shape });
                    try pending.append(program.allocator, .{ .field_label = record.fields[index].name });
                }
            },
            .tuple => |tuple| {
                writePatternType(program, hasher, tuple.ty);
                writePatternU32(hasher, @intCast(tuple.items.len));
                try pushShapeDigestItems(program, &pending, tuple.items);
            },
            .nominal => |nominal| {
                writePatternType(program, hasher, nominal.ty);
                try pending.append(program.allocator, .{ .shape = nominal.backing.* });
            },
            .callable => |callable| {
                writePatternType(program, hasher, callable.ty);
                const target = program.fnSourceDigest(callable.fn_id) orelse
                    Common.invariant("call-pattern callable target has no checked source identity");
                hasher.update(&target);
                writePatternU32(hasher, @intCast(callable.captures.len));
                try pushShapeDigestItems(program, &pending, callable.captures);
            },
        }
    }
}

fn pushShapeDigestItems(program: *Ast.Program, pending: *std.ArrayList(ShapeDigestItem), shapes: []const Shape) Allocator.Error!void {
    var index = shapes.len;
    while (index > 0) {
        index -= 1;
        try pending.append(program.allocator, .{ .shape = shapes[index] });
    }
}

fn writePatternType(program: *Ast.Program, hasher: *TypeDigestHasher, ty: Type.TypeId) void {
    const digest = program.types.representationDigestCached(&program.names, ty, null);
    hasher.update(&digest.bytes);
}

fn writePatternBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writePatternU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writePatternU32(hasher: *TypeDigestHasher, value: u32) void {
    var buffer: [4]u8 = undefined;
    buffer[0] = @truncate(value);
    buffer[1] = @truncate(value >> 8);
    buffer[2] = @truncate(value >> 16);
    buffer[3] = @truncate(value >> 24);
    hasher.update(&buffer);
}

fn patternEql(program: *Ast.Program, lhs: CallPattern, rhs: CallPattern) Allocator.Error!bool {
    if (lhs.args.len != rhs.args.len) return false;
    for (lhs.args, rhs.args) |lhs_arg, rhs_arg| {
        if (!try shapeEql(program, lhs_arg, rhs_arg)) return false;
    }
    return true;
}

/// Whether two shapes are equal: every pair of corresponding components
/// must be, so pairs are compared from a worklist.
fn shapeEql(program: *Ast.Program, lhs_root: Shape, rhs_root: Shape) Allocator.Error!bool {
    const Pair = struct { lhs: Shape, rhs: Shape };
    var pending = std.ArrayList(Pair).empty;
    defer pending.deinit(program.allocator);
    try pending.append(program.allocator, .{ .lhs = lhs_root, .rhs = rhs_root });
    while (pending.pop()) |pair| {
        const lhs = pair.lhs;
        const rhs = pair.rhs;
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
        switch (lhs) {
            .any => |lhs_ty| if (!sameType(program, lhs_ty, rhs.any)) return false,
            .tag => |lhs_tag| {
                const rhs_tag = rhs.tag;
                if (!sameType(program, lhs_tag.ty, rhs_tag.ty) or
                    !program.names.tagLabelTextEql(lhs_tag.name, rhs_tag.name) or
                    lhs_tag.payloads.len != rhs_tag.payloads.len)
                {
                    return false;
                }
                for (lhs_tag.payloads, rhs_tag.payloads) |lhs_payload, rhs_payload| {
                    try pending.append(program.allocator, .{ .lhs = lhs_payload, .rhs = rhs_payload });
                }
            },
            .record => |lhs_record| {
                const rhs_record = rhs.record;
                if (!sameType(program, lhs_record.ty, rhs_record.ty) or lhs_record.fields.len != rhs_record.fields.len) return false;
                for (lhs_record.fields, rhs_record.fields) |lhs_field, rhs_field| {
                    if (!program.names.recordFieldLabelTextEql(lhs_field.name, rhs_field.name)) return false;
                    try pending.append(program.allocator, .{ .lhs = lhs_field.shape, .rhs = rhs_field.shape });
                }
            },
            .tuple => |lhs_tuple| {
                const rhs_tuple = rhs.tuple;
                if (!sameType(program, lhs_tuple.ty, rhs_tuple.ty) or lhs_tuple.items.len != rhs_tuple.items.len) return false;
                for (lhs_tuple.items, rhs_tuple.items) |lhs_item, rhs_item| {
                    try pending.append(program.allocator, .{ .lhs = lhs_item, .rhs = rhs_item });
                }
            },
            .nominal => |lhs_nominal| {
                const rhs_nominal = rhs.nominal;
                if (!sameType(program, lhs_nominal.ty, rhs_nominal.ty)) return false;
                try pending.append(program.allocator, .{ .lhs = lhs_nominal.backing.*, .rhs = rhs_nominal.backing.* });
            },
            .callable => |lhs_callable| {
                const rhs_callable = rhs.callable;
                if (!sameType(program, lhs_callable.ty, rhs_callable.ty) or
                    !callableTargetMatches(program, lhs_callable.fn_id, rhs_callable.fn_id) or
                    lhs_callable.captures.len != rhs_callable.captures.len)
                {
                    return false;
                }
                for (lhs_callable.captures, rhs_callable.captures) |lhs_capture, rhs_capture| {
                    try pending.append(program.allocator, .{ .lhs = lhs_capture, .rhs = rhs_capture });
                }
            },
        }
    }
    return true;
}

/// Whether one specialization's call pattern accepts a call's argument values.
/// This reads the values the caller already cloned and takes no `Cloner`, so
/// deciding a specialization cannot clone a source argument a second time and a
/// rejected specialization costs nothing and leaves nothing behind.
fn callPatternMatchesValues(program: *Ast.Program, pattern: CallPattern, values: []const Value) Allocator.Error!bool {
    if (pattern.args.len != values.len) Common.invariant("call-pattern arity differed from direct call arity");
    for (pattern.args, values) |shape, value| {
        if (!try shapeMatchesValue(program, shape, value)) return false;
    }
    return true;
}

/// Whether a value has a shape: every component must, so pairs are checked
/// from a worklist.
fn shapeMatchesValue(program: *Ast.Program, root_shape: Shape, root_value: Value) Allocator.Error!bool {
    const Pair = struct { shape: Shape, value: Value };
    var pending = std.ArrayList(Pair).empty;
    defer pending.deinit(program.allocator);
    try pending.append(program.allocator, .{ .shape = root_shape, .value = root_value });
    while (pending.pop()) |pair| {
        const structural_value = structuralValue(pair.value);
        switch (pair.shape) {
            .any => {},
            .tag => |tag| {
                if (structural_value != .tag) return false;
                const value_tag = structural_value.tag;
                if (!sameType(program, tag.ty, value_tag.ty) or
                    !program.names.tagLabelTextEql(tag.name, value_tag.name) or
                    tag.payloads.len != value_tag.payloads.len)
                {
                    return false;
                }
                for (tag.payloads, value_tag.payloads) |payload_shape, payload_value| {
                    try pending.append(program.allocator, .{ .shape = payload_shape, .value = payload_value });
                }
            },
            .record => |record| {
                if (structural_value != .record) return false;
                const value_record = structural_value.record;
                if (!sameType(program, record.ty, value_record.ty) or record.fields.len != value_record.fields.len) return false;
                for (record.fields, value_record.fields) |field_shape, field_value| {
                    if (!program.names.recordFieldLabelTextEql(field_shape.name, field_value.name)) return false;
                    try pending.append(program.allocator, .{ .shape = field_shape.shape, .value = field_value.value });
                }
            },
            .tuple => |tuple| {
                if (structural_value != .tuple) return false;
                const value_tuple = structural_value.tuple;
                if (!sameType(program, tuple.ty, value_tuple.ty) or tuple.items.len != value_tuple.items.len) return false;
                for (tuple.items, value_tuple.items) |item_shape, item_value| {
                    try pending.append(program.allocator, .{ .shape = item_shape, .value = item_value });
                }
            },
            .nominal => |nominal| {
                if (structural_value != .nominal) return false;
                const value_nominal = structural_value.nominal;
                if (!sameType(program, nominal.ty, value_nominal.ty)) return false;
                try pending.append(program.allocator, .{ .shape = nominal.backing.*, .value = value_nominal.backing.* });
            },
            .callable => |callable| {
                if (structural_value != .callable) return false;
                const value_callable = structural_value.callable;
                if (!sameType(program, callable.ty, value_callable.ty) or
                    !callableTargetMatches(program, callable.fn_id, value_callable.fn_id) or
                    callable.captures.len != value_callable.captures.len)
                {
                    return false;
                }
                for (callable.captures, value_callable.captures) |capture_shape, capture_value| {
                    try pending.append(program.allocator, .{ .shape = capture_shape, .value = capture_value.value });
                }
            },
        }
    }
    return true;
}

fn callableTargetMatches(program: *const Ast.Program, expected: Ast.FnId, actual: Ast.FnId) bool {
    if (expected == actual) return true;
    const expected_source = program.getFn(expected).source orelse return false;
    const actual_source = program.getFn(actual).source orelse return false;
    return Mono.fnTemplateIdentityEql(expected_source, actual_source);
}

// The field, item, tag, record, and tuple readers below run only on values
// already proven to be a record, tuple, or tag under some wrapper chain, so
// following that chain to the read field, item, or tag terminates by
// construction. A value that references itself through the
// `runtime_anchor.structure`/`nominal.backing`/
// `static_data_candidate.structure` pointer edges would loop, so each reader
// counts the edges it follows and treats reaching `value_wrapper_strip_cap` as
// a compiler bug.
fn fieldFromValue(program: *const Ast.Program, value: Value, name: names.RecordFieldNameId) ?Value {
    const field = fieldFromValueStripping(program, value, name, 0) orelse return null;
    if (!isGeneratedIteratorStepField(program, valueType(program, structuralValue(value)), name)) return field;
    return switch (field) {
        .callable => |callable| blk: {
            var step = callable;
            step.iterator_step = true;
            break :blk .{ .callable = step };
        },
        .expr, .runtime_anchor, .static_data_candidate, .tag, .record, .tuple, .nominal => field,
    };
}

fn isGeneratedIteratorStepField(
    program: *const Ast.Program,
    receiver_ty: Type.TypeId,
    field: names.RecordFieldNameId,
) bool {
    const receiver_type = program.types.get(receiver_ty);
    if (receiver_type != .named) return false;
    const named = receiver_type.named;
    const topology = named.def.iterator_topology orelse return false;
    const backing = named.backing orelse return false;
    return backing.authority == .generated_private and
        field == topology.step_field;
}

fn fieldFromValueStripping(program: *const Ast.Program, start: Value, name: names.RecordFieldNameId, strip_depth: usize) ?Value {
    var value = start;
    var depth = strip_depth;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) Common.invariant("fieldFromValue followed a value wrapper chain past the strip cap");
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .record => |record| return fieldFromRecord(program, record, name),
            .nominal => |nominal| nominal.backing.*,
            .expr, .tag, .tuple, .callable => return null,
        };
    }
}

fn fieldPathFromValue(program: *const Ast.Program, receiver: Value, segments: anytype) ?Value {
    if (segments.len == 0) Common.invariant("field access path had no segments");
    var value = receiver;
    for (0..segments.len) |index| {
        const segment = GuardedList.at(segments, index);
        value = fieldFromValue(program, value, segment.field) orelse return null;
    }
    return value;
}

fn fieldFromRecord(program: *const Ast.Program, record: RecordValue, name: names.RecordFieldNameId) ?Value {
    for (record.fields) |field| {
        if (program.names.recordFieldLabelTextEql(field.name, name)) return field.value;
    }
    return null;
}

fn recordPatField(program: *const Ast.Program, fields: anytype, name: names.RecordFieldNameId) ?Ast.PatId {
    for (0..fields.len) |index| {
        const field = GuardedList.at(fields, index);
        if (program.names.recordFieldLabelTextEql(field.name, name)) return field.pattern;
    }
    return null;
}

fn itemFromValue(value: Value, index: u32) ?Value {
    return itemFromValueStripping(value, index, 0);
}

fn itemFromValueStripping(start: Value, index: u32, strip_depth: usize) ?Value {
    var value = start;
    var depth = strip_depth;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) Common.invariant("itemFromValue followed a value wrapper chain past the strip cap");
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .tuple => |tuple| return if (index < tuple.items.len) tuple.items[index] else null,
            .nominal => |nominal| nominal.backing.*,
            .expr, .tag, .record, .callable => return null,
        };
    }
}

/// Whether stripping every value wrapper, including nominal backings, leaves only
/// an opaque runtime expression. A nominal such as `Bool` can wrap a runtime
/// local, so its constructor is unknown even though the wrapper is structured.
fn isOpaqueBehindWrappers(start: Value) bool {
    var value = start;
    var depth: usize = 0;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) Common.invariant("isOpaqueBehindWrappers followed a value wrapper chain past the strip cap");
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .nominal => |nominal| nominal.backing.*,
            .expr => return true,
            .tag, .record, .tuple, .callable => return false,
        };
    }
}

fn tagFromValue(value: Value) ?TagValue {
    return tagFromValueStripping(value, 0);
}

fn tagFromValueStripping(start: Value, strip_depth: usize) ?TagValue {
    var value = start;
    var depth = strip_depth;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) Common.invariant("tagFromValue followed a value wrapper chain past the strip cap");
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .tag => |tag| return tag,
            .nominal => |nominal| nominal.backing.*,
            .expr, .record, .tuple, .callable => return null,
        };
    }
}

fn recordFromValue(value: Value) ?RecordValue {
    return recordFromValueStripping(value, 0);
}

fn recordFromValueStripping(start: Value, strip_depth: usize) ?RecordValue {
    var value = start;
    var depth = strip_depth;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) Common.invariant("recordFromValue followed a value wrapper chain past the strip cap");
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .record => |record| return record,
            .nominal => |nominal| nominal.backing.*,
            .expr, .tag, .tuple, .callable => return null,
        };
    }
}

fn tupleFromValue(value: Value) ?TupleValue {
    return tupleFromValueStripping(value, 0);
}

fn tupleFromValueStripping(start: Value, strip_depth: usize) ?TupleValue {
    var value = start;
    var depth = strip_depth;
    while (true) : (depth += 1) {
        if (depth >= value_wrapper_strip_cap) Common.invariant("tupleFromValue followed a value wrapper chain past the strip cap");
        value = switch (value) {
            .runtime_anchor => |anchor| anchor.structure.*,
            .static_data_candidate => |candidate| candidate.structure.*,
            .tuple => |tuple| return tuple,
            .nominal => |nominal| nominal.backing.*,
            .expr, .tag, .record, .callable => return null,
        };
    }
}

fn emptyLiftedProgramForTest(allocator: Allocator) Ast.Program {
    return Ast.Program.init(
        allocator,
        names.NameStore.init(allocator),
        Type.Store.init(allocator),
        .empty, // const_fn_evidence
        .empty, // const_fn_evidence_frames
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // field_access_segments
        .empty, // fn_def_captures
        .empty, // capture_operands
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        Mono.ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // inline_scopes
        .empty, // expr_inline_scopes
        .empty, // stmt_inline_scopes
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
}

test "SpecConstr analysis rewind discards field access segments" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const before = program.markSpecConstrAnalysis();
    const field = try program.names.internRecordFieldLabel("field");
    _ = try program.addFieldAccessSegmentSpan(&.{.{ .field = field }});
    program.rewindSpecConstrAnalysis(before);

    try std.testing.expectEqualDeep(before, program.markSpecConstrAnalysis());
}

fn addStaticDataIdentityForTest(program: *Ast.Program) Allocator.Error!Common.StaticDataId {
    const id: Common.StaticDataId = @enumFromInt(@as(u32, @intCast(program.static_data_values.len())));
    // SpecConstr transports and compares the allocated ID but never reads the
    // checked request. These tests stop before static-data lowering consumes it.
    try program.static_data_values.append(program.allocator, undefined);
    return id;
}

test "compile-time root reads remain opaque through specialization and cloning" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const item_ty = try program.types.add(.{ .primitive = .u8 });
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{item_ty}) });
    const item = try program.addExpr(.{ .ty = item_ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 } } });
    const initializer = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{item}) } });
    const root: Common.ComptimeValueRoot = .{
        .module = .{},
        .root = .{ .checked = @enumFromInt(1) },
        .const_locator = .{
            .artifact = .{},
            .owner = .{ .hoisted_expr = .{ .module_idx = 0, .expr = @enumFromInt(1) } },
            .template = @enumFromInt(1),
            .source_scheme = .{},
        },
    };
    const root_id = try program.addComptimeValueRoot(root);
    const read = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .comptime_value = .{
        .root = root_id,
        .initializer = initializer,
    } } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const value = try cloner.cloneExprValueDemandingShape(read);
    try std.testing.expect(value.bindings.isEmpty());
    try std.testing.expect(value.value == .expr);
    try std.testing.expectEqual(read, value.value.expr);
    try std.testing.expectEqual(read, try cloner.cloneExprPlain(read));
    try std.testing.expect(!try cloner.exprHasKnownShape(read));
    try std.testing.expect((try cloner.peekKnownValue(read)) == null);
    try std.testing.expect((try pass.staticDataStructure(read)).* == .expr);
    var renames = collections.DenseMap(Ast.LocalId, Ast.LocalId).init(allocator);
    defer renames.deinit();
    try std.testing.expectEqual(read, (try pass.cloneExprFresh(read, &renames)).?);
    try std.testing.expectEqual(initializer, program.getExpr(read).data.comptime_value.initializer);
    try std.testing.expectEqual(root_id, program.getExpr(read).data.comptime_value.root);
    try std.testing.expectEqualDeep(root, program.getComptimeValueRoot(root_id));
    try std.testing.expectEqual(@as(usize, 1), program.comptime_value_roots.len());
}

test "static candidate clones share closed initializers without emitting caller work" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const item_ty = try program.types.add(.{ .primitive = .u8 });
    const list_ty = try program.types.add(.{ .list = item_ty });
    const item = try program.addExpr(.{ .ty = item_ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 } } });
    const list = try program.addExpr(.{ .ty = list_ty, .data = .{ .list = try program.addExprSpan(&.{item}) } });
    const candidate = try program.addExpr(.{ .ty = list_ty, .data = .{ .static_data_candidate = .{
        .storage = .aggregate,
        .static_data = try addStaticDataIdentityForTest(&program),
        .runtime_expr = list,
    } } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    const before = program.markSpecConstrAnalysis();
    for (0..64) |_| {
        var rewrite = Cloner.initForRewrite(&pass);
        defer rewrite.deinit();
        var exit = Cloner.initForLoopExitSelection(&pass);
        defer exit.deinit();
        const cloned = try exit.cloneExprValueDemandingShape(candidate);
        try std.testing.expect(cloned.bindings.isEmpty());
        try std.testing.expectEqual(candidate, try exit.materialize(cloned.value));
        try std.testing.expectEqual(candidate, try rewrite.cloneExpr(candidate));
        try std.testing.expectEqual(candidate, try rewrite.cloneExprPlain(candidate));
        var renames = collections.DenseMap(Ast.LocalId, Ast.LocalId).init(allocator);
        defer renames.deinit();
        try std.testing.expectEqual(candidate, (try pass.cloneExprFresh(candidate, &renames)).?);
    }
    try std.testing.expectEqualDeep(before, program.markSpecConstrAnalysis());
    // The shape reader treats the list as opaque; its element needs no view.
    try std.testing.expectEqual(@as(usize, 2), pass.static_data_structure.count());
}

test "static candidate constructor views are linear in shared source graph size" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    var ty = try program.types.add(.{ .primitive = .u8 });
    var expr = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 } } });
    const depth = 24;
    for (0..depth) |_| {
        ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ ty, ty }) });
        expr = try program.addExpr(.{ .ty = ty, .data = .{ .tuple = try program.addExprSpan(&.{ expr, expr }) } });
    }
    const candidate = try program.addExpr(.{ .ty = ty, .data = .{ .static_data_candidate = .{
        .storage = .aggregate,
        .static_data = try addStaticDataIdentityForTest(&program),
        .runtime_expr = expr,
    } } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    const mark = pass.markAnalysis();
    const first = try pass.staticDataStructure(candidate);
    pass.rewindAnalysis(mark);
    try std.testing.expect(first == try pass.staticDataStructure(candidate));
    try std.testing.expectEqual(@as(usize, depth + 2), pass.static_data_structure.count());
    try std.testing.expectEqualDeep(mark.program, program.markSpecConstrAnalysis());
    // The shared graph denotes 2^24 leaves, but only its 26 distinct nodes
    // need storage. The view also survives output rewinds and cloner lifetimes.
}

test "static candidate match rebinding preserves the initializer and its private scope" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.{ .primitive = .u8 });
    const local = try program.addLocal(@enumFromInt(1), ty);
    const local_pat = try program.addPat(.{ .ty = ty, .data = .{ .bind = local } });
    const literal = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 } } });
    const read = try program.addExpr(.{ .ty = ty, .data = .{ .local = local } });
    const private = try program.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
        .bind = local_pat,
        .value = literal,
        .rest = read,
    } } });
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ty}) });
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{private}) } });
    const candidate = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .static_data_candidate = .{
        .storage = .aggregate,
        .static_data = try addStaticDataIdentityForTest(&program),
        .runtime_expr = tuple,
    } } });
    const field_local = try program.addLocal(@enumFromInt(2), ty);
    const field_pat = try program.addPat(.{ .ty = ty, .data = .{ .bind = field_local } });
    const pat = try program.addPat(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addPatSpan(&.{field_pat}) } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const cloned = try cloner.cloneExprValue(candidate);
    try std.testing.expect(cloned.bindings.isEmpty());
    try std.testing.expectEqual(private, itemFromValue(cloned.value, 0).?.expr);
    var bindings: BindingChain = .{};
    const prepared = (try cloner.bindPatToMatchValue(pat, cloned.value, literal, &bindings)).?;
    try std.testing.expect(!bindings.isEmpty());
    try std.testing.expectEqual(candidate, try cloner.materialize(prepared));
    try std.testing.expectEqual(private, itemFromValue(cloned.value, 0).?.expr);
    const selected = try cloner.materialize(itemFromValue(prepared, 0).?);
    try std.testing.expect(program.getExpr(selected).data.local != local);
    try std.testing.expectEqual(tuple, program.getExpr(candidate).data.static_data_candidate.runtime_expr);
}

test "loop exit selection clone is isolated from ordinary rewrites" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();

    var rewrite = Cloner.initForRewrite(&pass);
    defer rewrite.deinit();
    try std.testing.expectEqual(ClonePurpose.rewrite, rewrite.purpose);
    try std.testing.expectEqual(InlineCallMode.all, rewrite.inline_calls);
    try std.testing.expect(rewrite.rewrite_call_patterns);
    try std.testing.expect(rewrite.emit_callable_workers);

    var exit_selection = Cloner.initForLoopExitSelection(&pass);
    defer exit_selection.deinit();
    try std.testing.expectEqual(ClonePurpose.loop_exit_selection, exit_selection.purpose);
    try std.testing.expectEqual(InlineCallMode.none, exit_selection.inline_calls);
    try std.testing.expect(!exit_selection.rewrite_call_patterns);
    try std.testing.expect(!exit_selection.emit_callable_workers);
}

test "only original-body rewrites reuse unchanged source expressions" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const leaf = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();

    const before_reuse = program.exprCount();
    var original_body = Cloner.initForOriginalBodyRewrite(&pass);
    defer original_body.deinit();
    try std.testing.expectEqual(leaf, try original_body.cloneExprPlain(leaf));
    try std.testing.expectEqual(before_reuse, program.exprCount());

    const source_local = try program.addLocal(@enumFromInt(1), unit_ty);
    const source_ref = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = source_local } });
    const uninitialized = try program.addExpr(.{ .ty = unit_ty, .data = .{ .uninitialized_payload = .{
        .condition = source_local,
        .mask = 1,
    } } });
    const dbg = try program.addExpr(.{ .ty = unit_ty, .data = .{ .dbg = source_ref } });
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{unit_ty}) });
    const tuple = try program.addExpr(.{
        .ty = tuple_ty,
        .data = .{ .tuple = try program.addExprSpan(&.{source_ref}) },
    });
    const tuple_local = try program.addLocal(@enumFromInt(3), tuple_ty);
    const tuple_ref = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .local = tuple_local } });
    const tuple_access = try program.addExpr(.{
        .ty = unit_ty,
        .data = .{ .tuple_access = .{ .tuple = tuple_ref, .elem_index = 0 } },
    });
    // This source was built after the clone began, so mark it as source.
    original_body.output_start = program.exprCount();
    try std.testing.expectEqual(source_ref, try original_body.cloneExpr(source_ref));
    try std.testing.expectEqual(uninitialized, try original_body.cloneExpr(uninitialized));
    try std.testing.expectEqual(dbg, try original_body.cloneExpr(dbg));
    // Symbolic consumers still receive an unevaluated constructor, while a
    // non-demanding materialization can retain its complete source expression.
    const tuple_value = try original_body.cloneExprValue(tuple);
    try std.testing.expect(tuple_value.value == .tuple);
    try std.testing.expectEqual(tuple, try original_body.cloneExpr(tuple));
    try std.testing.expectEqual(tuple_access, try original_body.cloneExpr(tuple_access));

    const target_local = try program.addLocal(@enumFromInt(2), unit_ty);
    const target_ref = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = target_local } });
    try original_body.subst.putExact(source_local, .{ .expr = target_ref });
    try std.testing.expectEqual(target_ref, try original_body.cloneExpr(source_ref));
    const cloned_uninitialized = try original_body.cloneExpr(uninitialized);
    try std.testing.expect(cloned_uninitialized != uninitialized);
    const cloned_payload = program.getExpr(cloned_uninitialized).data.uninitialized_payload;
    try std.testing.expectEqual(target_local, cloned_payload.condition);
    const cloned_dbg = try original_body.cloneExpr(dbg);
    try std.testing.expect(cloned_dbg != dbg);
    try std.testing.expectEqual(target_ref, program.getExpr(cloned_dbg).data.dbg);
    try std.testing.expect(try original_body.cloneExpr(tuple) != tuple);

    var ordinary = Cloner.initForRewrite(&pass);
    defer ordinary.deinit();
    const before_clone = program.exprCount();
    const cloned = try ordinary.cloneExprPlain(leaf);
    try std.testing.expect(cloned != leaf);
    try std.testing.expectEqual(before_clone + 1, program.exprCount());

    var loop_exit = Cloner.initForLoopExitSelection(&pass);
    defer loop_exit.deinit();
    const before_loop_exit = program.exprCount();
    try std.testing.expect(try loop_exit.cloneExprPlain(leaf) != leaf);
    try std.testing.expectEqual(before_loop_exit + 1, program.exprCount());
}

test "rejected loop shape attempts do not retain emitted expressions" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const union_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });
    const initial_name = try program.names.internTagLabel("Initial");
    const next_name = try program.names.internTagLabel("Next");
    const param = try program.addLocal(@enumFromInt(1), union_ty);
    const initial = try program.addExpr(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = initial_name,
        .payloads = Ast.Span(Ast.ExprId).empty(),
    } } });
    const next = try program.addExpr(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = next_name,
        .payloads = Ast.Span(Ast.ExprId).empty(),
    } } });
    const continue_ = try program.addExpr(.{ .ty = union_ty, .data = .{ .continue_ = .{
        .values = try program.addExprSpan(&.{next}),
    } } });
    const loop = try program.addExpr(.{ .ty = union_ty, .data = .{ .loop_ = .{
        .params = try program.addTypedLocalSpan(&.{.{ .local = param, .ty = union_ty }}),
        .initial_values = try program.addExprSpan(&.{initial}),
        .body = continue_,
    } } });

    const exprs_before = program.exprCount();
    const locals_before = program.localsView().len;
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const cloned = try cloner.cloneExpr(loop);

    // The first split attempt emits a mismatched back edge, then demotes the
    // slot. Only initial-value anchoring and the whole-slot retry remain.
    try std.testing.expectEqual(@as(usize, 6), program.exprCount() - exprs_before);
    try std.testing.expectEqual(@as(usize, 1), program.localsView().len - locals_before);
    const cloned_loop = program.getExpr(cloned);
    if (cloned_loop.data != .loop_) return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 1), GuardedList.borrowLen(program.typedLocalSpan(cloned_loop.data.loop_.params)));
    try std.testing.expectEqual(@as(usize, 1), GuardedList.borrowLen(program.exprSpan(cloned_loop.data.loop_.initial_values)));
    const cloned_continue = program.getExpr(cloned_loop.data.loop_.body);
    if (cloned_continue.data != .continue_) return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 1), GuardedList.borrowLen(program.exprSpan(cloned_continue.data.continue_.values)));
}

test "SpecConstr preserves record update ordering while exposing its final shape" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const a = try program.names.internRecordFieldLabel("a");
    const b = try program.names.internRecordFieldLabel("b");
    const record_ty = try program.types.add(.{ .record = try program.types.addRecordFields(&program.names, &.{
        .{ .name = a, .ty = u8_ty, .default = null },
        .{ .name = b, .ty = u8_ty, .default = null },
    }) });
    const base_local = try program.addLocal(@enumFromInt(1), record_ty);
    const update_local = try program.addLocal(@enumFromInt(2), u8_ty);
    const base = try program.addExpr(.{ .ty = record_ty, .data = .{ .local = base_local } });
    const update_value = try program.addExpr(.{ .ty = u8_ty, .data = .{ .local = update_local } });
    const update = try program.addExpr(.{ .ty = record_ty, .data = .{ .record_update = .{
        .base = base,
        .fields = try program.addFieldExprSpan(&.{.{ .name = b, .value = update_value }}),
    } } });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    const shape = (try pass.constructorShape(update)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(shape == .record);
    try std.testing.expectEqual(@as(usize, 2), shape.record.fields.len);

    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const cloned = try cloner.cloneExprValue(update);
    if (cloned.value != .record) return error.TestUnexpectedResult;
    const record = cloned.value.record;
    try std.testing.expectEqual(@as(usize, 2), record.fields.len);

    const base_binding = cloned.bindings.first orelse return error.TestUnexpectedResult;
    const snapshot_node = base_binding.next orelse return error.TestUnexpectedResult;
    try std.testing.expect(snapshot_node.next == null);
    const snapshot = program.getStmt(snapshot_node.binding.statement).let_;
    const snapshot_fields = program.recordDestructSpan(program.getPat(snapshot.pat).data.record);
    try std.testing.expectEqual(@as(usize, 1), snapshot_fields.len);
    const snapshot_field = GuardedList.at(snapshot_fields, 0);
    try std.testing.expectEqual(a, snapshot_field.name);
    try std.testing.expectEqual(base_binding.binding.strict.local, program.getExpr(snapshot.value).data.local);
    try std.testing.expectEqual(program.getPat(snapshot_field.pattern).data.bind, program.getExpr(record.fields[0].value.expr).data.local);

    try std.testing.expectEqual(b, record.fields[1].name);
    try std.testing.expectEqual(update_local, program.getExpr(record.fields[1].value.expr).data.local);
}

test "SpecConstr record update permits an updated field representation to change" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const u16_ty = try program.types.add(.{ .primitive = .u16 });
    const a = try program.names.internRecordFieldLabel("a");
    const b = try program.names.internRecordFieldLabel("b");
    const base_record_ty = try program.types.add(.{ .record = try program.types.addRecordFields(&program.names, &.{
        .{ .name = a, .ty = u8_ty, .default = null },
        .{ .name = b, .ty = u8_ty, .default = null },
    }) });
    const result_record_ty = try program.types.add(.{ .record = try program.types.addRecordFields(&program.names, &.{
        .{ .name = a, .ty = u8_ty, .default = null },
        .{ .name = b, .ty = u16_ty, .default = null },
    }) });
    const base_local = try program.addLocal(@enumFromInt(1), base_record_ty);
    const update_local = try program.addLocal(@enumFromInt(2), u16_ty);
    const base = try program.addExpr(.{ .ty = base_record_ty, .data = .{ .local = base_local } });
    const update_value = try program.addExpr(.{ .ty = u16_ty, .data = .{ .local = update_local } });
    const update = try program.addExpr(.{ .ty = result_record_ty, .data = .{ .record_update = .{
        .base = base,
        .fields = try program.addFieldExprSpan(&.{.{ .name = b, .value = update_value }}),
    } } });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const cloned = try cloner.cloneExprValue(update);
    if (cloned.value != .record) return error.TestUnexpectedResult;
    const record = cloned.value.record;
    try std.testing.expectEqual(result_record_ty, record.ty);

    const base_binding = cloned.bindings.first orelse return error.TestUnexpectedResult;
    const snapshot_node = base_binding.next orelse return error.TestUnexpectedResult;
    try std.testing.expect(snapshot_node.next == null);
    const snapshot = program.getStmt(snapshot_node.binding.statement).let_;
    const snapshot_fields = program.recordDestructSpan(program.getPat(snapshot.pat).data.record);
    try std.testing.expectEqual(@as(usize, 1), snapshot_fields.len);
    try std.testing.expectEqual(a, GuardedList.at(snapshot_fields, 0).name);

    try std.testing.expectEqual(a, record.fields[0].name);
    try std.testing.expectEqual(u8_ty, valueType(&program, record.fields[0].value));
    try std.testing.expectEqual(b, record.fields[1].name);
    try std.testing.expectEqual(u16_ty, valueType(&program, record.fields[1].value));
}

test "SpecConstr keeps a transparent recursive anchor and its initializer bindings in scope" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const field = try program.names.internRecordFieldLabel("field");
    const record_ty = try program.types.add(.{ .record = try program.types.addRecordFields(&program.names, &.{
        .{ .name = field, .ty = u8_ty, .default = null },
    }) });
    const source_local = try program.addLocal(@enumFromInt(1), record_ty);
    const source_pat = try program.addPat(.{ .ty = record_ty, .data = .{ .bind = source_local } });
    const source_ref = try program.addExpr(.{ .ty = record_ty, .data = .{ .local = source_local } });
    const initializer = try program.addExpr(.{ .ty = record_ty, .data = .{ .record_update = .{
        .base = source_ref,
        .fields = try program.addFieldExprSpan(&.{}),
    } } });
    const source_stmt = try program.addStmt(.{ .let_ = .{
        .pat = source_pat,
        .value = initializer,
        .recursive = true,
    } });
    const source_block = try program.addExpr(.{ .ty = record_ty, .data = .{ .block = .{
        .statements = try program.addStmtSpan(&.{source_stmt}),
        .final_expr = source_ref,
    } } });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const cloned = try cloner.cloneExprValue(source_block);
    const anchor_node = cloned.bindings.first orelse return error.TestUnexpectedResult;
    try std.testing.expect(anchor_node.next == null);
    const anchor_id = switch (anchor_node.binding) {
        .statement => |stmt| stmt,
        .strict => return error.TestUnexpectedResult,
    };
    const anchor = program.getStmt(anchor_id).let_;
    try std.testing.expect(anchor.recursive);
    const anchor_local = program.getPat(anchor.pat).data.bind;
    try std.testing.expect(cloned.value == .runtime_anchor);
    const runtime_anchor = cloned.value.runtime_anchor;
    try std.testing.expectEqual(anchor_local, program.getExpr(runtime_anchor.runtime).data.local);
    try std.testing.expect(runtime_anchor.structure.* == .record);
    try std.testing.expectEqual(runtime_anchor.runtime, try cloner.materialize(cloned.value));
    switch (try pass.shapeFromValue(cloned.value)) {
        .proven => |shape| try std.testing.expect(shape == .record),
        .disproven, .unknown_budget_exhausted => return error.TestUnexpectedResult,
    }

    // Structure consumers still see the record, but its field is projected
    // from the retained runtime anchor instead of retaining the initializer's
    // private temporary or reconstructing the record.
    const anchored_field = runtime_anchor.structure.record.fields[0].value;
    if (anchored_field != .expr) return error.TestUnexpectedResult;
    const field_access = program.getExpr(anchored_field.expr).data.field_access;
    try std.testing.expectEqual(anchor_local, program.getExpr(field_access.receiver).data.local);

    // A structurally visible leaf must also be reusable, not merely in scope.
    // Otherwise inspecting the anchor's structure could duplicate strict work
    // from its initializer. Model such a leaf with an opaque block and verify
    // that reanchoring replaces it with a projection from the exact runtime
    // record instead.
    const zero = try program.addExpr(.{ .ty = u8_ty, .data = .{ .int_lit = .{
        .bytes = @bitCast(@as(i128, 0)),
        .kind = .i128,
    } } });
    const opaque_field = try program.addExpr(.{ .ty = u8_ty, .data = .{ .block = .{
        .statements = Ast.Span(Ast.StmtId).empty(),
        .final_expr = zero,
    } } });
    const synthetic_fields = [_]FieldValue{.{
        .name = field,
        .value = .{ .expr = opaque_field },
    }};
    const no_bindings: BindingChain = .{};
    var reanchor_budget: u32 = Cloner.recursive_anchor_scope_work_budget;
    const reanchored = (try cloner.reanchorRecursiveValue(
        .{ .record = .{ .ty = record_ty, .fields = &synthetic_fields } },
        runtime_anchor.runtime,
        no_bindings,
        &reanchor_budget,
        true,
    )) orelse return error.TestUnexpectedResult;
    if (reanchored != .runtime_anchor) return error.TestUnexpectedResult;
    const projected_field = reanchored.runtime_anchor.structure.record.fields[0].value;
    if (projected_field != .expr or projected_field.expr == opaque_field) return error.TestUnexpectedResult;
    const projected_access = program.getExpr(projected_field.expr).data.field_access;
    try std.testing.expectEqual(anchor_local, program.getExpr(projected_access.receiver).data.local);

    // The record-update base is strict work created from the recursive
    // reference. It must remain in the initializer, where the anchor local is
    // already in scope, rather than preceding the recursive statement.
    const initializer_block = program.getExpr(anchor.value).data.block;
    const initializer_let = program.getStmt(GuardedList.at(program.stmtSpan(initializer_block.statements), 0)).let_;
    try std.testing.expectEqual(anchor_local, program.getExpr(initializer_let.value).data.local);

    const wrapped = try cloner.wrapBindings(cloned.bindings, try cloner.materialize(cloned.value));
    const output_block = program.getExpr(wrapped).data.block;
    const output_stmts = program.stmtSpan(output_block.statements);
    try std.testing.expectEqual(@as(usize, 1), output_stmts.len);
    try std.testing.expectEqual(anchor_id, GuardedList.at(output_stmts, 0));
    try std.testing.expectEqual(runtime_anchor.runtime, output_block.final_expr);

    _ = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(2),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = wrapped },
        .ret = record_ty,
    });

    var scope: BodyLocalScope = .{
        .program = &program,
        .allocator = allocator,
        .fn_index = 0,
        .bound = collections.DenseMap(Ast.LocalId, u32).init(allocator),
        .joins = collections.DenseMap(Ast.JoinPointId, u32).init(allocator),
    };
    defer scope.bound.deinit();
    defer scope.joins.deinit();
    try scope.walkExpr(wrapped);
}

test "SpecConstr clones nested block prefixes once before retained statements" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const str_ty = try program.types.add(.{ .primitive = .str });
    const literal = try program.addStringLiteral("value");
    const leaf = try program.addExpr(.{ .ty = str_ty, .data = .{ .str_lit = literal } });
    const depth = 8;
    var body = leaf;
    for (0..depth) |index| {
        const local = try program.addLocal(@enumFromInt(index + 1), str_ty);
        const pattern = try program.addPat(.{ .ty = str_ty, .data = .{ .bind = local } });
        const bind = try program.addStmt(.{ .let_ = .{ .pat = pattern, .value = body } });
        const observe = try program.addStmt(.{ .dbg = leaf });
        const result = try program.addExpr(.{ .ty = str_ty, .data = .{ .local = local } });
        body = try program.addExpr(.{ .ty = str_ty, .data = .{ .block = .{
            .statements = try program.addStmtSpan(&.{ bind, observe }),
            .final_expr = result,
        } } });
    }
    program.next_symbol = depth + 1;

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const before = program.exprCount();
    _ = try cloner.cloneExpr(body);

    // A retained statement after each prefix used to reject a speculative
    // block clone and re-clone that prefix, multiplying work at every nesting
    // level. Bound total construction, not merely the small final body.
    try std.testing.expect(program.exprCount() - before <= 12 * depth);
}

test "SpecConstr keeps flat block bindings out of continuation templates" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const str_ty = try program.types.add(.{ .primitive = .str });
    const literal = try program.addStringLiteral("value");
    var value = try program.addExpr(.{ .ty = str_ty, .data = .{ .str_lit = literal } });
    var statements = std.ArrayList(Ast.StmtId).empty;
    defer statements.deinit(allocator);
    const binding_count = 1024;
    for (0..binding_count) |index| {
        const local = try program.addLocal(@enumFromInt(index + 1), str_ty);
        const pattern = try program.addPat(.{ .ty = str_ty, .data = .{ .bind = local } });
        try statements.append(allocator, try program.addStmt(.{ .let_ = .{ .pat = pattern, .value = value } }));
        value = try program.addExpr(.{ .ty = str_ty, .data = .{ .local = local } });
    }
    const body = try program.addExpr(.{ .ty = str_ty, .data = .{ .block = .{
        .statements = try program.addStmtSpan(statements.items),
        .final_expr = value,
    } } });
    program.next_symbol = binding_count + 1;

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const before = program.exprCount();
    _ = try cloner.cloneExpr(body);
    // Ordinary sequential bindings are processed iteratively. Only a
    // branch-built value needs a source template for its shared continuation.
    try std.testing.expectEqual(@as(usize, 0), cloner.clone_templates.items.len);
    try std.testing.expect(program.exprCount() - before <= 4 * binding_count);
}

test "SpecConstr residual block destructures preserve statement source context" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const str_ty = try program.types.add(.{ .primitive = .str });
    const present = try program.names.internTagLabel("Present");
    const missing = try program.names.internTagLabel("Missing");
    const tags = try program.types.addTagVariants(&program.names, &.{
        .{ .name = present, .checked_name = present, .payloads = try program.types.addSpan(&.{str_ty}) },
        .{ .name = missing, .checked_name = missing, .payloads = .empty() },
    });
    const tagged_ty = try program.types.add(.{ .tag_union = tags });
    const input = try program.addLocal(@enumFromInt(1), tagged_ty);
    const payload = try program.addLocal(@enumFromInt(2), str_ty);
    const input_expr = try program.addExpr(.{ .ty = tagged_ty, .data = .{ .local = input } });
    const result = try program.addExpr(.{ .ty = str_ty, .data = .{ .local = payload } });
    const payload_pat = try program.addPat(.{ .ty = str_ty, .data = .{ .bind = payload } });
    const pattern = try program.addPat(.{ .ty = tagged_ty, .data = .{ .tag = .{
        .name = present,
        .payloads = try program.addPatSpan(&.{payload_pat}),
    } } });
    const stmt_loc: SourceLoc = .{ .file = 1, .line = 3, .column = 5 };
    const stmt_region = Region.from_raw_offsets(20, 40);
    const site: Ast.ComptimeSiteId = @enumFromInt(program.comptime_sites.len());
    try program.comptime_sites.append(allocator, .{ .kind = .destructure, .owner = .first, .region = stmt_region });
    program.current_loc = stmt_loc;
    program.current_region = stmt_region;
    const binding = try program.addStmt(.{ .let_ = .{ .pat = pattern, .value = input_expr, .comptime_site = site } });
    program.current_loc = .{ .file = 1, .line = 1, .column = 1 };
    program.current_region = Region.from_raw_offsets(0, 80);
    const body = try program.addExpr(.{ .ty = str_ty, .data = .{ .block = .{
        .statements = try program.addStmtSpan(&.{binding}),
        .final_expr = result,
    } } });
    program.next_symbol = 3;

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const cloned = program.getExpr(try cloner.cloneExpr(body)).data.block;
    try std.testing.expectEqual(@as(u32, 1), cloned.statements.len);
    const cloned_stmt = GuardedList.at(program.stmtSpan(cloned.statements), 0);
    try std.testing.expectEqualDeep(stmt_loc, program.stmtLoc(cloned_stmt));
    try std.testing.expectEqualDeep(stmt_region, program.stmtRegion(cloned_stmt));
    const cloned_bind = program.getStmt(cloned_stmt).let_;
    try std.testing.expectEqual(site, cloned_bind.comptime_site.?);
    const cloned_tag = program.getPat(cloned_bind.pat).data.tag;
    const cloned_payload = program.getPat(GuardedList.at(program.patSpan(cloned_tag.payloads), 0)).data.bind;
    try std.testing.expect(cloned_payload != payload);
    try std.testing.expectEqual(cloned_payload, program.getExpr(cloned.final_expr).data.local);
}

test "SpecConstr accepts a transparent alias record update base" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const field = try program.names.internRecordFieldLabel("field");
    const record_ty = try program.types.add(.{ .record = try program.types.addRecordFields(&program.names, &.{
        .{ .name = field, .ty = u8_ty, .default = null },
    }) });
    const module_identity = try program.names.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try program.names.internTypeName("RecordAlias");
    const alias_ty = try program.types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = .{ .ty = record_ty, .use = .inspectable },
    } });
    const base_local = try program.addLocal(@enumFromInt(1), record_ty);
    const update_local = try program.addLocal(@enumFromInt(2), u8_ty);
    const base = try program.addExpr(.{ .ty = record_ty, .data = .{ .local = base_local } });
    const update_value = try program.addExpr(.{ .ty = u8_ty, .data = .{ .local = update_local } });
    const update = try program.addExpr(.{ .ty = alias_ty, .data = .{ .record_update = .{
        .base = base,
        .fields = try program.addFieldExprSpan(&.{.{ .name = field, .value = update_value }}),
    } } });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const cloned = try cloner.cloneExprValue(update);
    try std.testing.expect(cloned.value == .record);
    try std.testing.expectEqual(record_ty, cloned.value.record.ty);
}

test "SpecConstr compares representations, not checked provenance" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const u16_ty = try program.types.add(.{ .primitive = .u16 });
    const module_identity = try program.names.internModuleIdentity(&([_]u8{0xCD} ** 32));
    const type_name = try program.names.internTypeName("Wrapper");
    const Nominal = struct {
        fn add(p: *Ast.Program, module: names.ModuleIdentityId, name: names.TypeNameId, checked_ty: u32, backing: Type.TypeId) Allocator.Error!Type.TypeId {
            return p.types.add(.{ .named = .{
                .named_type = .{ .module = .{}, .ty = @enumFromInt(checked_ty) },
                .def = .{ .module = module, .type_name = name, .source_decl = 7 },
                .kind = .nominal,
                .args = Type.Span.empty(),
                .backing = .{ .ty = backing, .use = .inspectable },
            } });
        }
    };
    const first = try Nominal.add(&program, module_identity, type_name, 1, u8_ty);
    const other_occurrence = try Nominal.add(&program, module_identity, type_name, 2, u8_ty);
    const other_backing = try Nominal.add(&program, module_identity, type_name, 1, u16_ty);

    const first_full = program.types.typeDigestCached(&program.names, first, null);
    const other_occurrence_full = program.types.typeDigestCached(&program.names, other_occurrence, null);
    try std.testing.expect(!std.mem.eql(u8, &first_full.bytes, &other_occurrence_full.bytes));
    try std.testing.expect(sameType(&program, first, other_occurrence));
    try std.testing.expect(!sameType(&program, first, other_backing));
}

test "call-pattern scans direct call and function reference capture operands" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const local = try program.addLocal(@enumFromInt(1), unit_ty);
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    _ = try program.addExprSpan(&.{unit_expr});
    const local_expr = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = local } });

    const return_expr = try program.addExpr(.{ .ty = unit_ty, .data = .{ .return_ = .{
        .value = local_expr,
        .target = unit_ty,
    } } });
    const captures = try program.addCaptureOperandSpan(&.{.{
        .id = check.CheckedModule.CaptureId.generatedLift(0),
        .value = return_expr,
    }});
    const fn_ref = try program.addExpr(.{
        .ty = unit_ty,
        .data = .{
            .fn_ref = .{
                .fn_id = undefined, // not read by the call-pattern scanners under test
                .captures = captures,
            },
        },
    });
    const call_proc = try program.addExpr(.{
        .ty = unit_ty,
        .data = .{
            .call_proc = .{
                .callee = undefined, // not read by the call-pattern scanners under test
                .args = Ast.Span(Ast.ExprId).empty(),
                .captures = captures,
            },
        },
    });

    try std.testing.expect(try exprContainsReturn(std.testing.allocator, &program, fn_ref));
    try std.testing.expectEqual(@as(usize, 1), try localUseCountInExpr(std.testing.allocator, &program, local, fn_ref));
    try std.testing.expect(try exprContainsReturn(std.testing.allocator, &program, call_proc));
    try std.testing.expectEqual(@as(usize, 1), try localUseCountInExpr(std.testing.allocator, &program, local, call_proc));
}

/// Delays callbacks until receive and completes newest-first, including the
/// single-lane case. Scratch is destroyed at callback return, not at merge.
const ReverseSpecConstrExecutor = struct {
    queued: [Pass.wave_capacity]TaskExecutor.Task = undefined,
    len: usize = 0,
    worker_count: usize,
    fail_after: ?usize = null,
    submitted: usize = 0,

    fn executor(self: *@This()) TaskExecutor.Executor {
        return .{ .context = self, .worker_count = self.worker_count, .beginFn = begin, .submitFn = submit, .receiveFn = receive, .endFn = end };
    }
    fn begin(context: *anyopaque) void {
        const self: *@This() = @ptrCast(@alignCast(context));
        std.debug.assert(self.len == 0);
    }
    fn submit(context: *anyopaque, task: TaskExecutor.Task) Allocator.Error!void {
        const self: *@This() = @ptrCast(@alignCast(context));
        if (self.fail_after == self.submitted) return error.OutOfMemory;
        self.queued[self.len] = task;
        self.len += 1;
        self.submitted += 1;
    }
    fn receive(context: *anyopaque) TaskExecutor.Completion {
        const self: *@This() = @ptrCast(@alignCast(context));
        self.len -= 1;
        const task = self.queued[self.len];
        var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer scratch.deinit();
        var lane = TaskExecutor.LaneState.init(std.testing.allocator);
        defer lane.deinit();
        return .{ .id = task.id, .worker_id = 0, .value = task.run(task.context, .{
            .id = 0,
            .allocator = std.testing.allocator,
            .scratch = scratch.allocator(),
            .lane_state = &lane,
        }) };
    }
    fn end(context: *anyopaque) void {
        const self: *@This() = @ptrCast(@alignCast(context));
        std.debug.assert(self.len == 0);
    }
};

test "staged SpecConstr discovery admits source order with duplicates and bounded reverse waves" {
    const allocator = std.testing.allocator;
    for ([_]usize{ 0, 1, 2, 4 }) |worker_count| {
        var program = emptyLiftedProgramForTest(allocator);
        defer program.deinit();
        const unit_ty = try program.types.add(.zst);
        const tag_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });
        const unit = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
        const arg = try program.addLocal(@enumFromInt(1), tag_ty);
        const target = try program.addFn(.{
            .shapes = program.finishFnShapes(.{}),
            .symbol = @enumFromInt(2),
            .args = try program.addTypedLocalSpan(&.{.{ .local = arg, .ty = tag_ty }}),
            .captures = .empty(),
            .body = .{ .roc = unit },
            .ret = unit_ty,
        });
        const labels = [_]names.TagNameId{
            try program.names.internTagLabel("First"),
            try program.names.internTagLabel("Second"),
            try program.names.internTagLabel("Third"),
            try program.names.internTagLabel("Fourth"),
        };
        for (0..40) |index| {
            const label_index = if (index == 0) 0 else @min(index - 1, 3);
            const tag = try program.addExpr(.{ .ty = tag_ty, .data = .{ .tag = .{
                .name = labels[label_index],
                .payloads = .empty(),
            } } });
            const call = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
                .callee = .{ .lifted = target },
                .args = try program.addExprSpan(&.{tag}),
                .captures = .empty(),
            } } });
            _ = try program.addFn(.{
                .shapes = program.finishFnShapes(.{}),
                .symbol = @enumFromInt(@as(u32, @intCast(index + 3))),
                .args = .empty(),
                .captures = .empty(),
                .body = .{ .roc = call },
                .ret = unit_ty,
            });
        }
        var pass = try Pass.init(allocator, &program);
        defer pass.deinit();
        pass.plans[0].used_args[0] = true;
        var executor: ReverseSpecConstrExecutor = .{ .worker_count = worker_count };
        var metrics: ParallelMetrics = .{};
        pass.options = .{ .executor = if (worker_count == 0) null else executor.executor(), .metrics_out = &metrics };
        const before = program.markSpecConstrAnalysis();
        try pass.collectValueAwareCallPatterns(program.fnCount());
        try std.testing.expectEqualDeep(before, program.markSpecConstrAnalysis());
        try std.testing.expectEqual(@as(usize, 3), pass.plans[0].specs.items.len);
        for (pass.plans[0].specs.items, 0..) |spec, index| {
            try std.testing.expectEqual(labels[index], spec.pattern.args[0].tag.name);
        }
        try std.testing.expectEqual(@as(u64, 3), metrics.patterns_admitted);
        // All sources discover against phase-entry capacity, including the
        // second wave after coordinator admission has saturated the target.
        try std.testing.expectEqual(@as(u64, 40), metrics.patterns_recorded);
        try std.testing.expectEqual(@as(u64, Pass.wave_capacity), metrics.peak_retained_shards);
        try std.testing.expectEqual(@as(u64, if (worker_count == 0) 0 else if (builtin.mode == .Debug) 41 else 40), metrics.tasks_committed);
        try std.testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
    }
}

test "SpecConstr shard identity totals reject exhaustion before publication" {
    const max = std.math.maxInt(u32);
    try std.testing.expectEqual(max, try checkedIdentityTotal(max - 1, 1));
    try std.testing.expectEqual(max, try checkedIdentityTotal(max, 0));
    try std.testing.expectError(error.OutOfMemory, checkedIdentityTotal(max, 1));
    try std.testing.expectError(error.OutOfMemory, checkedIdentityTotal(max - 1, 2));
}

test "staged SpecConstr phase entry capacity fixes discovery budgets across waves" {
    const allocator = std.testing.allocator;
    for ([_]usize{ 0, 1, 2, 4 }) |worker_count| {
        var program = emptyLiftedProgramForTest(allocator);
        defer program.deinit();
        const ty = try program.types.add(.zst);
        const tag_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });
        const unit = try program.addExpr(.{ .ty = ty, .data = .unit });
        var consumers: [2]Ast.FnId = undefined;
        for (&consumers, 0..) |*consumer, i| {
            const arg = try program.addLocal(@enumFromInt(@as(u32, @intCast(i))), tag_ty);
            consumer.* = try program.addFn(.{
                .shapes = program.finishFnShapes(.{}),
                .symbol = @enumFromInt(@as(u32, @intCast(i + 2))),
                .args = try program.addTypedLocalSpan(&.{.{ .local = arg, .ty = tag_ty }}),
                .captures = .empty(),
                .body = .{ .roc = unit },
                .ret = ty,
            });
        }
        var tags: [3]Ast.ExprId = undefined;
        for (&tags, [_][]const u8{ "A", "B", "C" }) |*tag, label| {
            tag.* = try program.addExpr(.{ .ty = tag_ty, .data = .{ .tag = .{
                .name = try program.names.internTagLabel(label),
                .payloads = .empty(),
            } } });
        }
        // Every demand of this argument spends 192 units of the body-wide
        // inline budget, even when its consumer already has three requests.
        const statement = try program.addStmt(.{ .expr = unit });
        var expensive_statements: [190]Ast.StmtId = undefined;
        @memset(&expensive_statements, statement);
        const producer_body = try program.addExpr(.{ .ty = tag_ty, .data = .{ .block = .{
            .statements = try program.addStmtSpan(&expensive_statements),
            .final_expr = tags[0],
        } } });
        const producer = try program.addFn(.{
            .shapes = program.finishFnShapes(.{}),
            .symbol = @enumFromInt(4),
            .args = .empty(),
            .captures = .empty(),
            .body = .{ .roc = producer_body },
            .ret = tag_ty,
        });
        const expensive_arg = try program.addExpr(.{ .ty = tag_ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = producer },
            .args = .empty(),
            .iterator_procedure = .single,
        } } });
        const duplicate = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = consumers[0] },
            .args = try program.addExprSpan(&.{expensive_arg}),
        } } });
        const later = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = consumers[1] },
            .args = try program.addExprSpan(&.{expensive_arg}),
        } } });
        const repeated = try program.addStmt(.{ .expr = duplicate });
        var repeats: [400]Ast.StmtId = undefined;
        @memset(&repeats, repeated);
        const budget_body = try program.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try program.addStmtSpan(&repeats),
            .final_expr = later,
        } } });
        for (0..37) |i| {
            const body = if (i == 36) budget_body else try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
                .callee = .{ .lifted = consumers[0] },
                .args = try program.addExprSpan(&.{tags[i % tags.len]}),
            } } });
            // Every body here calls a consumer with a known tag; the last one
            // reuses a body whose expressions were created before this loop.
            _ = try program.addFn(.{
                .shapes = program.finishFnShapes(.{}).merged(.{ .direct_call = true, .constructs_value = true }),
                .symbol = @enumFromInt(@as(u32, @intCast(i + 5))),
                .args = .empty(),
                .captures = .empty(),
                .body = .{ .roc = body },
                .ret = ty,
            });
        }
        var pass = try Pass.init(allocator, &program);
        defer pass.deinit();
        pass.plans[0].used_args[0] = true;
        pass.plans[1].used_args[0] = true;
        var executor: ReverseSpecConstrExecutor = .{ .worker_count = worker_count };
        pass.options.executor = if (worker_count == 0) null else executor.executor();
        try pass.collectValueAwareCallPatterns(program.fnCount());
        try std.testing.expectEqual(@as(usize, 3), pass.plans[0].specs.items.len);
        // Admission in the first wave must not save inline budget for this
        // distinct callee in the second wave.
        try std.testing.expectEqual(@as(usize, 0), pass.plans[1].specs.items.len);
        // A new phase observes the now-preexisting cap and legitimately skips
        // the duplicate arguments, leaving budget for the later distinct call.
        try pass.collectValueAwareCallPatterns(program.fnCount());
        try std.testing.expectEqual(@as(usize, 1), pass.plans[1].specs.items.len);
    }
}

test "SpecConstr retained local compaction ignores raw ID gaps and keeps first typed occurrences" {
    var program = emptyLiftedProgramForTest(std.testing.allocator);
    defer program.deinit();
    const high: Ast.LocalId = @enumFromInt(0xffff_fffe);
    const low: Ast.LocalId = @enumFromInt(1);
    const middle: Ast.LocalId = @enumFromInt(100);
    const first_ty = try program.types.add(.zst);
    const later_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{first_ty}) });
    var leaves = [_]Ast.TypedLocal{
        .{ .local = high, .ty = first_ty },
        .{ .local = low, .ty = later_ty },
        .{ .local = high, .ty = later_ty },
        .{ .local = middle, .ty = first_ty },
        .{ .local = low, .ty = first_ty },
        .{ .local = high, .ty = later_ty },
    };
    // Only leaf-sized ordinal and duplicate columns fit here, regardless of
    // the source/generated ID gap. Compaction never consults the type store.
    var buffer: [128]u8 = undefined;
    var bounded = std.heap.FixedBufferAllocator.init(&buffer);
    var retained: std.ArrayList(Ast.TypedLocal) = .empty;
    try Cloner.compactRetainedLocals(bounded.allocator(), &retained);
    try std.testing.expectEqual(@as(usize, 0), bounded.end_index);
    retained = .{ .items = &leaves, .capacity = leaves.len };
    try Cloner.compactRetainedLocals(bounded.allocator(), &retained);
    try std.testing.expectEqualSlices(Ast.TypedLocal, &.{
        .{ .local = high, .ty = first_ty },
        .{ .local = low, .ty = later_ty },
        .{ .local = middle, .ty = first_ty },
    }, retained.items);
}

test "SpecConstr retained locals allocate by exact leaves not unrelated prefix" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.zst);
    for (0..100_000) |i| _ = try program.addLocal(@enumFromInt(@as(u32, @intCast(i))), ty);
    const local: Ast.LocalId = @enumFromInt(99_999);
    const span = try program.addTypedLocalSpan(&.{
        .{ .local = local, .ty = ty },
        .{ .local = local, .ty = ty },
    });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var buffer: [4096]u8 = undefined;
    var bounded = std.heap.FixedBufferAllocator.init(&buffer);
    pass.allocator = bounded.allocator();
    defer pass.allocator = allocator;
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const before_empty = bounded.end_index;
    try std.testing.expectEqualDeep(Ast.Span(Ast.TypedLocal).empty(), try cloner.cloneRetainedLocals(.empty()));
    try std.testing.expectEqual(before_empty, bounded.end_index);
    const result = try cloner.cloneRetainedLocals(span);
    try std.testing.expectEqual(@as(u32, 1), result.len);
    try std.testing.expectEqual(local, GuardedList.at(program.typedLocalSpan(result), 0).local);
}

test "staged SpecConstr submission failure drains accepted tasks" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const unit_ty = try program.types.add(.zst);
    const unit = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const target = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(1),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = unit },
        .ret = unit_ty,
    });
    const call = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = target },
        .args = .empty(),
        .captures = .empty(),
    } } });
    for (0..4) |index| {
        _ = try program.addFn(.{
            .shapes = program.finishFnShapes(.{}).merged(.{ .direct_call = true }),
            .symbol = @enumFromInt(@as(u32, @intCast(index + 2))),
            .args = .empty(),
            .captures = .empty(),
            .body = .{ .roc = call },
            .ret = unit_ty,
        });
    }
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var executor: ReverseSpecConstrExecutor = .{ .worker_count = 4, .fail_after = 2 };
    pass.options.executor = executor.executor();
    try std.testing.expectError(error.OutOfMemory, pass.collectValueAwareCallPatterns(program.fnCount()));
    try std.testing.expectEqual(@as(usize, 0), executor.len);
}

fn checkSpecConstrShardAllocationFailure(allocator: Allocator, source: *Pass, fn_id: Ast.FnId, phase: Phase) Common.LowerError!void {
    var work: Pass.Work = .{ .source = source, .fn_id = fn_id, .phase = phase };
    var lane = TaskExecutor.LaneState.init(allocator);
    defer lane.deinit();
    _ = Pass.Work.callback(&work, .{
        .id = 0,
        .allocator = allocator,
        .scratch = allocator,
        .lane_state = &lane,
    });
    if (work.failure) |err| {
        std.debug.assert(work.output == null);
        return err;
    }
    const output = work.output.?;
    defer output.deinit();
    if (phase != .discovery) std.debug.assert(output.changed);
}

fn specConstrSelectionProgramForTest(allocator: Allocator) Allocator.Error!struct { program: Ast.Program, fn_id: Ast.FnId } {
    var program = emptyLiftedProgramForTest(allocator);
    errdefer program.deinit();
    var symbols: Common.SymbolGen = .{};
    const unit_ty = try program.types.add(.zst);
    const unit = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ unit_ty, unit_ty }) });
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{ unit, unit }) } });
    const exit = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .break_ = tuple } });
    const loop = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .loop_ = .{
        .params = .empty(),
        .initial_values = .empty(),
        .body = exit,
    } } });
    const local = try program.addLocal(symbols.fresh(), unit_ty);
    const local_ref = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = local } });
    const first_pat = try program.addPat(.{ .ty = unit_ty, .data = .{ .bind = local } });
    const second_local = try program.addLocal(symbols.fresh(), unit_ty);
    const second_pat = try program.addPat(.{ .ty = unit_ty, .data = .{ .bind = second_local } });
    const body = try program.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{
        .bind = try program.addPat(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addPatSpan(&.{ first_pat, second_pat }) } }),
        .value = loop,
        .rest = local_ref,
    } } });
    const fn_id = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = symbols.fresh(),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = body },
        .ret = unit_ty,
    });
    program.next_symbol = symbols.next;
    try program.names.prepareForReadSharing();
    try program.types.prepareForReadSharing(&program.names);
    return .{ .program = program, .fn_id = fn_id };
}

test "staged SpecConstr shard allocation failures release output and scratch owners" {
    const allocator = std.testing.allocator;
    var fixture = try specConstrSelectionProgramForTest(allocator);
    const program = &fixture.program;
    defer program.deinit();
    var pass = try Pass.init(allocator, program);
    defer pass.deinit();
    for ([_]Phase{ .discovery, .iterator_fusion, .unused_loop_results }) |phase| {
        try std.testing.checkAllAllocationFailures(allocator, checkSpecConstrShardAllocationFailure, .{ &pass, fixture.fn_id, phase });
    }
}

fn checkSpecConstrCommitAllocationFailure(allocator: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    var fixture = try specConstrSelectionProgramForTest(allocator);
    const program = &fixture.program;
    defer program.deinit();
    var pass = try Pass.init(allocator, program);
    defer pass.deinit();
    // Callback allocations succeed, so the sweep reaches coordinator commit
    // with a live changed shard whose ownership must also be released on OOM.
    var executor: ReverseSpecConstrExecutor = .{ .worker_count = 2 };
    var metrics: ParallelMetrics = .{};
    pass.options = .{ .executor = executor.executor(), .metrics_out = &metrics };
    try pass.runIndependentPhase(.unused_loop_results, program.fnCount());
    try std.testing.expectEqual(@as(u64, 1), metrics.bodies_committed);
}

test "staged SpecConstr changed shard commit allocation failures release owners" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, checkSpecConstrCommitAllocationFailure, .{});
}

fn checkSpecConstrRequestAllocationFailure(allocator: Allocator) (Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    var symbols: Common.SymbolGen = .{};
    const ty = try program.types.add(.zst);
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ty}) });
    const unit = try program.addExpr(.{ .ty = ty, .data = .unit });
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{unit}) } });
    const arg = try program.addLocal(symbols.fresh(), tuple_ty);
    const target = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = symbols.fresh(),
        .args = try program.addTypedLocalSpan(&.{.{ .local = arg, .ty = tuple_ty }}),
        .captures = .empty(),
        .body = .{ .roc = unit },
        .ret = ty,
    });
    const call = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = target },
        .args = try program.addExprSpan(&.{tuple}),
    } } });
    _ = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = symbols.fresh(),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = call },
        .ret = ty,
    });
    program.next_symbol = symbols.next;
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    pass.plans[0].used_args[0] = true;
    try pass.collectValueAwareCallPatterns(program.fnCount());
    try std.testing.expectEqual(@as(usize, 1), pass.plans[0].specs.items.len);
}

test "staged SpecConstr request copying and admission allocation failures release owners" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, checkSpecConstrRequestAllocationFailure, .{});
}

test "issue 10313 value-aware call-pattern collection does not append lifted IR" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ unit_ty, unit_ty }) });
    const arg_local = try program.addLocal(@enumFromInt(1), unit_ty);
    const bound_local = try program.addLocal(@enumFromInt(2), tuple_ty);
    const arg_ref = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = arg_local } });
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const tuple_expr = try program.addExpr(.{
        .ty = tuple_ty,
        .data = .{ .tuple = try program.addExprSpan(&.{ arg_ref, unit_expr }) },
    });
    const bind_pat = try program.addPat(.{ .ty = tuple_ty, .data = .{ .bind = bound_local } });
    const body = try program.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{
        .bind = bind_pat,
        .value = tuple_expr,
        .rest = unit_expr,
    } } });
    _ = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(3),
        .args = try program.addTypedLocalSpan(&.{.{ .local = arg_local, .ty = unit_ty }}),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = body },
        .ret = unit_ty,
    });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();

    const before_collect = program.markSpecConstrAnalysis();
    const symbol_before_collect = pass.symbols.next;
    const join_before_collect = pass.next_join_point;
    try pass.collectValueAwareCallPatterns(1);
    try std.testing.expectEqualDeep(before_collect, program.markSpecConstrAnalysis());
    try std.testing.expectEqual(symbol_before_collect, pass.symbols.next);
    try std.testing.expectEqual(join_before_collect, pass.next_join_point);
    try std.testing.expectEqual(@as(usize, 0), pass.arena.queryCapacity());
}

test "SpecConstr admission uses body size and worker count before cloning" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const one_let_pat = try program.addPat(.{ .ty = unit_ty, .data = .wildcard });
    const one_let = try program.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{
        .bind = one_let_pat,
        .value = unit_expr,
        .rest = unit_expr,
    } } });

    switch (try exprBodySizeWithin(std.testing.allocator, &program, one_let, 3)) {
        .exact => |count| try std.testing.expectEqual(@as(usize, 3), count),
        .over_limit => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(BodySize.over_limit, try exprBodySizeWithin(std.testing.allocator, &program, one_let, 2));

    var large_body = unit_expr;
    for (0..(spec_constr_body_expr_threshold / 2 + 1)) |_| {
        const pat = try program.addPat(.{ .ty = unit_ty, .data = .wildcard });
        large_body = try program.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{
            .bind = pat,
            .value = unit_expr,
            .rest = large_body,
        } } });
    }

    _ = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });
    const large_fn_id = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(2),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = large_body },
        .ret = unit_ty,
    });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();

    try std.testing.expectEqual(SpecAdmission.admitted, pass.newSpecAdmission(0));
    for (0..spec_constr_specialization_count) |_| {
        try pass.plans[0].specs.append(allocator, .{ .pattern = .{ .args = &.{} } });
    }
    try std.testing.expectEqual(SpecAdmission.denied_spec_count, pass.newSpecAdmission(0));
    try std.testing.expectEqual(SpecAdmission.denied_body_size, pass.newSpecAdmission(1));
    const large_inline_source = try pass.inlineSourceBody(@enumFromInt(1)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!large_inline_source.size.admits());

    const fn_ty = try program.types.add(.{ .func = .{
        .args = Type.Span.empty(),
        .ret = unit_ty,
    } });
    const large_fn_ref = try program.addExpr(.{ .ty = fn_ty, .data = .{ .fn_ref = .{
        .fn_id = large_fn_id,
        .captures = Ast.Span(Ast.CaptureOperand).empty(),
    } } });
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    const cloned_large_fn_ref = try cloner.cloneExprValue(large_fn_ref);
    if (cloned_large_fn_ref.value != .expr) return error.TestUnexpectedResult;
    const residual_large_fn_ref = cloned_large_fn_ref.value.expr;
    try std.testing.expect(program.getExpr(residual_large_fn_ref).data == .fn_ref);

    const large_fn = program.getFnAt(1);
    program.setFnAt(1, .{
        .symbol = large_fn.symbol,
        .source = large_fn.source,
        .signature = large_fn.signature,
        .args = large_fn.args,
        .captures = large_fn.captures,
        .body = .{ .roc = unit_expr },
        .ret = large_fn.ret,
    });
    try pass.refreshPreCloneSource(1);
    try std.testing.expectEqual(SpecAdmission.admitted, pass.newSpecAdmission(1));
}

test "SpecConstr bounds cumulative inlining across small acyclic wrappers" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    var result_ty = unit_ty;
    var callee = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });

    // Every wrapper is individually tiny, but each calls the preceding
    // wrapper twice. Per-body admission alone therefore expands this graph
    // exponentially even though it contains no recursion; the depth keeps
    // the fully-inlined size far past the budget.
    for (0..18) |depth| {
        const first = try program.addExpr(.{ .ty = result_ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = callee },
            .args = Ast.Span(Ast.ExprId).empty(),
            .captures = Ast.Span(Ast.CaptureOperand).empty(),
        } } });
        const second = try program.addExpr(.{ .ty = result_ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = callee },
            .args = Ast.Span(Ast.ExprId).empty(),
            .captures = Ast.Span(Ast.CaptureOperand).empty(),
        } } });
        const wrapper_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ result_ty, result_ty }) });
        const body = try program.addExpr(.{
            .ty = wrapper_ty,
            .data = .{ .tuple = try program.addExprSpan(&.{ first, second }) },
        });
        callee = try program.addFn(.{
            .shapes = program.finishFnShapes(.{}),
            .symbol = @enumFromInt(@as(u32, @intCast(depth + 2))),
            .args = Ast.Span(Ast.TypedLocal).empty(),
            .captures = Ast.Span(Ast.TypedLocal).empty(),
            .body = .{ .roc = body },
            .ret = wrapper_ty,
        });
        result_ty = wrapper_ty;
    }

    const root_call = try program.addExpr(.{ .ty = result_ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = callee },
        .args = Ast.Span(Ast.ExprId).empty(),
        .captures = Ast.Span(Ast.CaptureOperand).empty(),
    } } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const before = program.exprCount();
    _ = try cloner.cloneExprValue(root_call);
    const growth = program.exprCount() - before;

    var retained_call = false;
    for (before..program.exprCount()) |index| {
        if (program.getExprAt(index).data == .call_proc) {
            retained_call = true;
            break;
        }
    }
    try std.testing.expect(retained_call);
    try std.testing.expect(growth < Cloner.inline_body_work_budget * 2);
}

test "issue 10760 SpecConstr bounds cloning of rewritten inline bodies" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const pair_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ unit_ty, unit_ty }) });
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const pair_expr = try program.addExpr(.{
        .ty = pair_ty,
        .data = .{ .tuple = try program.addExprSpan(&.{ unit_expr, unit_expr }) },
    });

    const leaf_arg = try program.addLocal(@enumFromInt(1), pair_ty);
    var result_ty = unit_ty;
    var callee = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(2),
        .args = try program.addTypedLocalSpan(&.{.{ .local = leaf_arg, .ty = pair_ty }}),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });
    // Each source wrapper is tiny. Before source and output bodies were kept
    // separate, rewriting in source order replaced both calls with the already
    // rewritten output of the preceding wrapper. Re-cloning the final output
    // while charging the final wrapper's original body size is the
    // non-converging path from issue 10760.
    for (0..9) |depth| {
        const first = try program.addExpr(.{ .ty = result_ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = callee },
            .args = try program.addExprSpan(&.{pair_expr}),
            .captures = Ast.Span(Ast.CaptureOperand).empty(),
        } } });
        const second = try program.addExpr(.{ .ty = result_ty, .data = .{ .call_proc = .{
            .callee = .{ .lifted = callee },
            .args = try program.addExprSpan(&.{pair_expr}),
            .captures = Ast.Span(Ast.CaptureOperand).empty(),
        } } });
        const wrapper_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ result_ty, result_ty }) });
        const body = try program.addExpr(.{
            .ty = wrapper_ty,
            .data = .{ .tuple = try program.addExprSpan(&.{ first, second }) },
        });
        const arg = try program.addLocal(
            @enumFromInt(@as(u32, @intCast(depth + 3))),
            pair_ty,
        );
        callee = try program.addFn(.{
            .shapes = program.finishFnShapes(.{}),
            .symbol = @enumFromInt(@as(u32, @intCast(depth + 12))),
            .args = try program.addTypedLocalSpan(&.{.{ .local = arg, .ty = pair_ty }}),
            .captures = Ast.Span(Ast.TypedLocal).empty(),
            .body = .{ .roc = body },
            .ret = wrapper_ty,
        });
        result_ty = wrapper_ty;
    }

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();

    for (0..pass.plans.len) |index| {
        try pass.cloneFnBodyInPlace(@enumFromInt(@as(u32, @intCast(index))));
    }

    const rewritten_body = switch (program.getFn(callee).body) {
        .roc => |body| body,
        .hosted => return error.TestUnexpectedResult,
    };
    const inline_source = try pass.inlineSourceBody(callee) orelse return error.TestUnexpectedResult;
    try std.testing.expect(inline_source.expr != rewritten_body);
    const source_work = inline_source.size.exactValue() orelse return error.TestUnexpectedResult;
    const rewritten_work = switch (try exprBodySizeWithin(std.testing.allocator, &program, rewritten_body, Cloner.inline_body_work_budget)) {
        .exact => |size| size,
        .over_limit => return error.TestUnexpectedResult,
    };
    try std.testing.expect(rewritten_work > source_work);

    const root_call = try program.addExpr(.{ .ty = result_ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = callee },
        .args = try program.addExprSpan(&.{pair_expr}),
        .captures = Ast.Span(Ast.CaptureOperand).empty(),
    } } });
    const root_source_size = switch (try exprBodySizeWithin(std.testing.allocator, &program, root_call, spec_constr_body_expr_threshold)) {
        .exact => |size| size,
        .over_limit => return error.TestUnexpectedResult,
    };

    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    cloner.inline_direct_requires_known_arg = true;
    const before_exprs = program.exprCount();
    const before_work = cloner.inline_body_growth.remaining;
    _ = try cloner.cloneExpr(root_call);
    const growth = program.exprCount() - before_exprs;
    const charged_work = before_work - cloner.inline_body_growth.remaining;

    // An admitted inline must charge enough source-body work to bound what it
    // emits. Declining the inline may emit only the original residual call.
    const growth_limit = charged_work + root_source_size;
    try std.testing.expectEqual(growth_limit, @max(growth_limit, growth));
}

test "value-aware call-pattern collection keeps generic producer calls opaque" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ unit_ty, unit_ty }) });
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const tuple_expr = try program.addExpr(.{
        .ty = tuple_ty,
        .data = .{ .tuple = try program.addExprSpan(&.{ unit_expr, unit_expr }) },
    });

    const producer = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = tuple_expr },
        .ret = tuple_ty,
    });

    const consumer_arg = try program.addLocal(@enumFromInt(2), tuple_ty);
    const consumer = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(3),
        .args = try program.addTypedLocalSpan(&.{.{ .local = consumer_arg, .ty = tuple_ty }}),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });

    const producer_call = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = producer },
        .args = Ast.Span(Ast.ExprId).empty(),
    } } });
    const inline_arg_consumer_call = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = consumer },
        .args = try program.addExprSpan(&.{producer_call}),
    } } });
    _ = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(4),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = inline_arg_consumer_call },
        .ret = unit_ty,
    });

    const bound_tuple_local = try program.addLocal(@enumFromInt(5), tuple_ty);
    const bound_tuple_ref = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .local = bound_tuple_local } });
    const let_arg_consumer_call = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = consumer },
        .args = try program.addExprSpan(&.{bound_tuple_ref}),
    } } });
    const bind_tuple = try program.addPat(.{ .ty = tuple_ty, .data = .{ .bind = bound_tuple_local } });
    const let_body = try program.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{
        .bind = bind_tuple,
        .value = tuple_expr,
        .rest = let_arg_consumer_call,
    } } });
    _ = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(6),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = let_body },
        .ret = unit_ty,
    });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    pass.plans[@intFromEnum(consumer)].used_args[0] = true;

    try pass.collectValueAwareCallPatterns(3);
    try std.testing.expectEqual(@as(usize, 0), pass.plans[@intFromEnum(consumer)].specs.items.len);

    try pass.collectValueAwareCallPatterns(4);
    try std.testing.expectEqual(@as(usize, 1), pass.plans[@intFromEnum(consumer)].specs.items.len);
}

test "generic value cloning preserves a call until its result shape is demanded" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const union_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });
    const tag_name = try program.names.internTagLabel("Result");
    const tag = try program.addExpr(.{
        .ty = union_ty,
        .data = .{ .tag = .{
            .name = tag_name,
            .payloads = Ast.Span(Ast.ExprId).empty(),
        } },
    });
    const callee = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = tag },
        .ret = union_ty,
    });
    const call = try program.addExpr(.{
        .ty = union_ty,
        .data = .{ .call_proc = .{
            .callee = .{ .lifted = callee },
            .args = Ast.Span(Ast.ExprId).empty(),
        } },
    });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    cloner.inline_direct_requires_known_arg = true;

    const generic = try cloner.cloneExprValue(call);
    if (generic.value != .expr) return error.TestUnexpectedResult;
    const residual = generic.value.expr;
    try std.testing.expect(program.getExpr(residual).data == .call_proc);

    const demanded = try cloner.cloneExprValueDemandingShape(call);
    try std.testing.expect(demanded.value == .tag);
}

test "issue 10168 SpecConstr clones every capture when nested cloning grows the capture store" {
    const allocator = std.testing.allocator;
    // Repro for https://github.com/roc-lang/roc/issues/10168. Cloning one
    // capture may append nested callable operands, but every sibling capture
    // must still be cloned with its original identity.
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const fn_ty = try program.types.add(.{ .func = .{
        .args = Type.Span.empty(),
        .ret = unit_ty,
    } });
    const fn_list_ty = try program.types.add(.{ .list = fn_ty });
    const unit_expr = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const nested_binder: check.CheckedModule.PatternBinderId = @enumFromInt(1);
    const nested_capture_local = try program.addLocalWithBinder(@enumFromInt(1), unit_ty, nested_binder);
    const nested_capture_slots = try program.addTypedLocalSpan(&.{.{
        .local = nested_capture_local,
        .ty = unit_ty,
    }});
    const nested_fn = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(2),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = nested_capture_slots,
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });
    const nested_capture_value = try program.addExpr(.{
        .ty = unit_ty,
        .data = .{ .local = nested_capture_local },
    });
    const nested_operands = try program.addCaptureOperandSpan(&.{.{
        .id = check.CheckedModule.CaptureId.fromBinder(nested_binder),
        .value = nested_capture_value,
    }});
    const nested_fn_ref = try program.addExpr(.{
        .ty = fn_ty,
        .data = .{ .fn_ref = .{
            .fn_id = nested_fn,
            .captures = nested_operands,
        } },
    });

    // The list forces the nested callable value to be materialized while
    // cloning the first outer capture, which appends its capture operands.
    const first_value = try program.addExpr(.{
        .ty = fn_list_ty,
        .data = .{ .list = try program.addExprSpan(&.{nested_fn_ref}) },
    });
    const second_value = unit_expr;
    const first_local = try program.addLocal(@enumFromInt(3), fn_list_ty);
    const second_local = try program.addLocal(@enumFromInt(4), unit_ty);
    const first_id = program.ensureLiftCaptureId(first_local);
    const second_id = program.ensureLiftCaptureId(second_local);
    const outer_capture_slots = try program.addTypedLocalSpan(&.{
        .{ .local = first_local, .ty = fn_list_ty },
        .{ .local = second_local, .ty = unit_ty },
    });
    const outer_fn = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(5),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = outer_capture_slots,
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });
    const outer_operands = try program.addCaptureOperandSpan(&.{
        .{ .id = first_id, .value = first_value },
        .{ .id = second_id, .value = second_value },
    });

    // Make the nested append move the backing allocation deterministically,
    // independent of ArrayList's current growth policy.
    while (program.capture_operands.len() < program.capture_operands.capacity()) {
        _ = try program.addCaptureOperandSpan(&.{.{
            .id = check.CheckedModule.CaptureId.generatedLift(3),
            .value = second_value,
        }});
    }

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    var bindings: BindingChain = .{};
    const value = try cloner.callableValueFromRef(fn_ty, .{
        .fn_id = outer_fn,
        .captures = outer_operands,
    }, &bindings);
    try std.testing.expect(bindings.isEmpty());
    if (value != .callable) return error.TestUnexpectedResult;
    const callable = value.callable;
    try std.testing.expectEqual(@as(usize, 2), callable.captures.len);
    try std.testing.expectEqual(first_id, callable.captures[0].id);
    try std.testing.expectEqual(second_id, callable.captures[1].id);
}

test "field access folding preserves shared residual suffix spans" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const outer_name = try program.names.internRecordFieldLabel("outer");
    const middle_name = try program.names.internRecordFieldLabel("middle");
    const leaf_name = try program.names.internRecordFieldLabel("leaf");

    const leaf_ty = try program.types.add(.{ .primitive = .u8 });
    const inner_ty = try program.types.add(.{ .record = try program.types.addFields(&.{
        .{ .name = leaf_name, .ty = leaf_ty, .default = null },
    }) });
    const middle_ty = try program.types.add(.{ .record = try program.types.addFields(&.{
        .{ .name = middle_name, .ty = inner_ty, .default = null },
    }) });
    const outer_ty = try program.types.add(.{ .record = try program.types.addFields(&.{
        .{ .name = outer_name, .ty = middle_ty, .default = null },
    }) });

    const leaf_local = try program.addLocal(@enumFromInt(1), leaf_ty);
    const leaf_expr = try program.addExpr(.{ .ty = leaf_ty, .data = .{ .local = leaf_local } });
    const inner_expr = try program.addExpr(.{ .ty = inner_ty, .data = .{
        .record = try program.addFieldExprSpan(&.{.{ .name = leaf_name, .value = leaf_expr }}),
    } });
    const middle_expr = try program.addExpr(.{ .ty = middle_ty, .data = .{
        .record = try program.addFieldExprSpan(&.{.{ .name = middle_name, .value = inner_expr }}),
    } });
    const full_receiver = try program.addExpr(.{ .ty = outer_ty, .data = .{
        .record = try program.addFieldExprSpan(&.{.{ .name = outer_name, .value = middle_expr }}),
    } });
    const full_segments = try program.addFieldAccessSegmentSpan(&.{
        .{ .field = outer_name },
        .{ .field = middle_name },
        .{ .field = leaf_name },
    });
    const full_access = try program.addExpr(.{ .ty = leaf_ty, .data = .{ .field_access = .{
        .receiver = full_receiver,
        .segments = full_segments,
    } } });

    const unknown_middle_local = try program.addLocal(@enumFromInt(2), middle_ty);
    const unknown_middle_expr = try program.addExpr(.{ .ty = middle_ty, .data = .{ .local = unknown_middle_local } });
    const observable_unknown_middle = try program.addExpr(.{ .ty = middle_ty, .data = .{ .dbg = unknown_middle_expr } });
    const partial_receiver = try program.addExpr(.{ .ty = outer_ty, .data = .{
        .record = try program.addFieldExprSpan(&.{.{ .name = outer_name, .value = observable_unknown_middle }}),
    } });
    const partial_access = try program.addExpr(.{ .ty = leaf_ty, .data = .{ .field_access = .{
        .receiver = partial_receiver,
        .segments = full_segments,
    } } });

    const original_segment_count = program.field_access_segments.len();
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const full_expr_start = program.exprCount();
    const folded_leaf = try cloner.cloneExpr(full_access);
    try std.testing.expectEqual(full_expr_start + 1, program.exprCount());
    try std.testing.expectEqual(leaf_local, program.getExpr(folded_leaf).data.local);
    try std.testing.expectEqual(original_segment_count, program.field_access_segments.len());

    const partial_expr_start = program.exprCount();
    const residual_expr = try cloner.cloneExpr(partial_access);
    const residual = blk_residual: {
        const scrutinee = program.getExpr(residual_expr).data;
        if (scrutinee != .field_access) return error.TestUnexpectedResult;
        break :blk_residual scrutinee.field_access;
    };
    const residual_receiver_child = blk_residual_receiver_child: {
        const scrutinee = program.getExpr(residual.receiver).data;
        if (scrutinee != .dbg) return error.TestUnexpectedResult;
        break :blk_residual_receiver_child scrutinee.dbg;
    };
    try std.testing.expectEqual(unknown_middle_local, program.getExpr(residual_receiver_child).data.local);
    try std.testing.expectEqual(full_segments.start + 1, residual.segments.start);
    try std.testing.expectEqual(full_segments.len - 1, residual.segments.len);
    try std.testing.expectEqual(middle_name, program.fieldAccessSegmentAt(residual.segments, 0).field);
    try std.testing.expectEqual(leaf_name, program.fieldAccessSegmentAt(residual.segments, 1).field);
    try std.testing.expectEqual(original_segment_count, program.field_access_segments.len());

    var dbg_count: usize = 0;
    var field_access_count: usize = 0;
    for (partial_expr_start..program.exprCount()) |raw_expr| {
        const counted_expr = program.getExpr(@enumFromInt(@as(u32, @intCast(raw_expr)))).data;
        if (counted_expr == .dbg) dbg_count += 1;
        if (counted_expr == .field_access) field_access_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), dbg_count);
    try std.testing.expectEqual(@as(usize, 1), field_access_count);
}

test "expression traversal visits both operands of structural_hash" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const value_local = try program.addLocal(@enumFromInt(1), unit_ty);
    const hasher_local = try program.addLocal(@enumFromInt(2), unit_ty);

    const value_expr = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = value_local } });
    const hasher_local_expr = try program.addExpr(.{ .ty = unit_ty, .data = .{ .local = hasher_local } });
    const hasher_expr = try program.addExpr(.{ .ty = unit_ty, .data = .{ .return_ = .{
        .value = hasher_local_expr,
        .target = unit_ty,
    } } });
    const hash_expr = try program.addExpr(.{ .ty = unit_ty, .data = .{ .structural_hash = .{
        .value = value_expr,
        .hasher = hasher_expr,
    } } });

    // The `hasher` operand is an unrestricted expression, so every traversal
    // must descend into it as well as into `value`. A `return_` reachable only
    // through `hasher` proves the hasher side is walked; counting each local
    // proves both sides are walked exactly once.
    try std.testing.expect(try exprContainsReturn(std.testing.allocator, &program, hash_expr));
    try std.testing.expectEqual(@as(usize, 1), try localUseCountInExpr(std.testing.allocator, &program, value_local, hash_expr));
    try std.testing.expectEqual(@as(usize, 1), try localUseCountInExpr(std.testing.allocator, &program, hasher_local, hash_expr));
}

test "static match verdicts separate definite no-match from statically undecidable" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const union_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });

    const foo = try program.names.internTagLabel("Foo");
    const bar = try program.names.internTagLabel("Bar");

    const opaque_expr = try program.addExpr(.{ .ty = u8_ty, .data = .{ .local = try program.addLocal(@enumFromInt(1), u8_ty) } });
    const opaque_value = Value{ .expr = opaque_expr };
    const foo_value = Value{ .tag = .{ .ty = union_ty, .name = foo, .payloads = &.{opaque_value} } };

    const wildcard_pat = try program.addPat(.{ .ty = u8_ty, .data = .wildcard });
    const foo_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = foo,
        .payloads = try program.addPatSpan(&.{wildcard_pat}),
    } } });
    const bar_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = bar,
        .payloads = try program.addPatSpan(&.{wildcard_pat}),
    } } });

    // Same tag name matches; a different tag name is a definite no-match.
    try std.testing.expectEqual(MatchVerdict.match, try cloner.bindPatToValue(foo_pat, foo_value));
    try std.testing.expectEqual(MatchVerdict.no_match, try cloner.bindPatToValue(bar_pat, foo_value));

    // A tag pattern probing an opaque expression component is undecidable.
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(foo_pat, opaque_value));

    // List, string, and numeric-literal patterns have no symbolic value
    // representation, so they are undecidable even against known components.
    const list_pat = try program.addPat(.{ .ty = u8_ty, .data = .{ .list = .{
        .patterns = Ast.Span(Ast.PatId).empty(),
        .rest = null,
    } } });
    const str_lit = try program.addStringLiteral("known");
    const str_pat = try program.addPat(.{ .ty = u8_ty, .data = .{ .str_lit = str_lit } });
    const int_pat = try program.addPat(.{ .ty = u8_ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 } } });
    const foo_list_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = foo,
        .payloads = try program.addPatSpan(&.{list_pat}),
    } } });
    const foo_str_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = foo,
        .payloads = try program.addPatSpan(&.{str_pat}),
    } } });
    const foo_int_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{
        .name = foo,
        .payloads = try program.addPatSpan(&.{int_pat}),
    } } });
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(foo_list_pat, foo_value));
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(foo_str_pat, foo_value));
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(foo_int_pat, foo_value));

    // Tuple patterns: a definite no-match on any element decides the whole
    // pattern even when another element is undecidable; otherwise an
    // undecidable element makes the whole pattern undecidable.
    const tuple_ty = try program.types.add(.{ .tuple = Type.Span.empty() });
    const tuple_value = Value{ .tuple = .{ .ty = tuple_ty, .items = &.{ foo_value, opaque_value } } };
    const both_undecidable = try program.addPat(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addPatSpan(&.{ foo_list_pat, list_pat }) } });
    const excluded_and_undecidable = try program.addPat(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addPatSpan(&.{ bar_pat, list_pat }) } });
    const matched_and_undecidable = try program.addPat(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addPatSpan(&.{ foo_pat, list_pat }) } });
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(both_undecidable, tuple_value));
    try std.testing.expectEqual(MatchVerdict.no_match, try cloner.bindPatToValue(excluded_and_undecidable, tuple_value));
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(matched_and_undecidable, tuple_value));

    // Nominal patterns delegate to the backing; probing an opaque value is
    // undecidable.
    const nominal_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .nominal = foo_pat } });
    const backing = Value{ .tag = .{ .ty = union_ty, .name = foo, .payloads = &.{opaque_value} } };
    const nominal_value = Value{ .nominal = .{ .ty = union_ty, .backing = &backing } };
    try std.testing.expectEqual(MatchVerdict.match, try cloner.bindPatToValue(nominal_pat, nominal_value));
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(nominal_pat, opaque_value));

    // A structured wrapper does not make its opaque backing statically known.
    const wrapped_opaque = Value{ .nominal = .{ .ty = union_ty, .backing = &opaque_value } };
    const record_pat = try program.addPat(.{ .ty = u8_ty, .data = .{ .record = Ast.Span(Ast.RecordDestruct).empty() } });
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(foo_pat, wrapped_opaque));
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(both_undecidable, wrapped_opaque));
    try std.testing.expectEqual(MatchVerdict.unknown, try cloner.bindPatToValue(record_pat, wrapped_opaque));
}

test "static value matchers bound wrapper strips over a cyclic value" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const union_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });

    // A static-data-candidate value whose symbolic edge points back at itself:
    // the fixpoint shape a recursively-constructed value takes when a `.local`
    // resolves through the substitution maps to an ancestor of its own
    // construction. Stripping the wrapper never reaches a constructor.
    var cyclic: Value = undefined;
    cyclic = .{
        .static_data_candidate = .{
            .ty = union_ty,
            // Never read: matching follows only the symbolic edge, and whole
            // static-value substitution does not inspect the initializer.
            .static_data = undefined,
            .expr = undefined,
            .structure = &cyclic,
        },
    };

    // A cyclic symbolic view does not prevent reuse of the closed initializer.
    try std.testing.expectEqual(ProofStatus.proven, try cloner.valueCanSubstitute(cyclic));

    // A nominal pattern strips the wrapper chain looking for its backing. The
    // static-data case keeps the same pattern, so the strip would loop forever
    // on the cycle; the strip cap declines it to a residual runtime match
    // (`.unknown`) and to a declined flow binding (`false`) rather than hanging.
    const wildcard_pat = try program.addPat(.{ .ty = u8_ty, .data = .wildcard });
    const nominal_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .nominal = wildcard_pat } });
    try std.testing.expectEqual(MatchVerdict.unknown_budget_exhausted, try cloner.bindPatToValue(nominal_pat, cyclic));
    try std.testing.expectEqual(false, try cloner.bindPatToFlowValue(nominal_pat, cyclic));
}

test "SpecConstr pattern clones bind fresh local identities" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const source_local = try program.addLocal(@enumFromInt(1), u8_ty);
    const source_pat = try program.addPat(.{ .ty = u8_ty, .data = .{ .bind = source_local } });
    const source_ref = try program.addExpr(.{ .ty = u8_ty, .data = .{ .local = source_local } });
    const source_payload_ref = try program.addExpr(.{ .ty = u8_ty, .data = .{ .uninitialized_payload = .{ .condition = source_local } } });
    // This source was built after the clone began, so mark it as source.
    cloner.output_start = program.exprCount();

    const first_change = cloner.subst.watermark();
    const first_pat = try cloner.clonePat(source_pat, .bind_runtime);
    const first_pat_data = program.getPat(first_pat).data;
    if (first_pat_data != .bind) return error.TestUnexpectedResult;
    const first_local = first_pat_data.bind;
    const first_ref = try cloner.cloneExpr(source_ref);
    try std.testing.expectEqual(first_local, program.getExpr(first_ref).data.local);
    const first_payload_ref = try cloner.cloneExpr(source_payload_ref);
    try std.testing.expectEqual(first_local, program.getExpr(first_payload_ref).data.uninitialized_payload.condition);
    cloner.subst.restore(first_change);

    const second_change = cloner.subst.watermark();
    const second_pat = try cloner.clonePat(source_pat, .bind_runtime);
    const second_pat_data = program.getPat(second_pat).data;
    if (second_pat_data != .bind) return error.TestUnexpectedResult;
    const second_local = second_pat_data.bind;
    const second_ref = try cloner.cloneExpr(source_ref);
    try std.testing.expectEqual(second_local, program.getExpr(second_ref).data.local);
    cloner.subst.restore(second_change);

    try std.testing.expect(source_local != first_local);
    try std.testing.expect(source_local != second_local);
    try std.testing.expect(first_local != second_local);

    const known_local = try program.addLocal(@enumFromInt(2), u8_ty);
    const known_ref = try program.addExpr(.{ .ty = u8_ty, .data = .{ .local = known_local } });
    const known_change = cloner.subst.watermark();
    try cloner.subst.put(cloner.pass.program, source_local, .{ .expr = known_ref });
    const output_pat = try cloner.clonePat(source_pat, .output_only);
    const output_pat_data = program.getPat(output_pat).data;
    if (output_pat_data != .bind) return error.TestUnexpectedResult;
    const output_local = output_pat_data.bind;
    const substituted_ref = try cloner.cloneExpr(source_ref);
    try std.testing.expectEqual(known_local, program.getExpr(substituted_ref).data.local);
    try std.testing.expect(output_local != source_local);
    try std.testing.expect(output_local != known_local);
    cloner.subst.restore(known_change);
}

test "whole-body normalization resolves binder-equivalent argument locals" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const ty = try program.types.add(.{ .primitive = .u8 });
    const binder: check.CheckedModule.PatternBinderId = @enumFromInt(1);
    const argument = try program.addLocalWithBinder(@enumFromInt(1), ty, binder);
    const equivalent = try program.addLocalWithBinder(@enumFromInt(2), ty, binder);
    const equivalent_ref = try program.addExpr(.{ .ty = ty, .data = .{ .local = equivalent } });
    const fn_id = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(3),
        .args = try program.addTypedLocalSpan(&.{.{ .local = argument, .ty = ty }}),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = equivalent_ref },
        .ret = ty,
    });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    try pass.cloneFnBodyInPlace(fn_id);

    const cloned_body = switch (program.getFn(fn_id).body) {
        .roc => |body| body,
        .hosted => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(argument, program.getExpr(cloned_body).data.local);
}

/// Substitutes a local of the first nominal for a same-binder local of the
/// second, and returns the substituted expression.
fn substituteNamedForTest(
    program: *Ast.Program,
    first_checked_ty: u32,
    first_backing: Type.TypeId,
    second_checked_ty: u32,
    second_backing: Type.TypeId,
) Common.LowerError!struct { cloned: Ast.ExprId, replacement: Ast.LocalId, second_ty: Type.TypeId } {
    const module_identity = try program.names.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try program.names.internTypeName("Nominal");
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const first_ty = try program.types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(first_checked_ty) },
        .def = def,
        .kind = .nominal,
        .args = Type.Span.empty(),
        .backing = .{ .ty = first_backing, .use = .inspectable },
    } });
    const second_ty = try program.types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(second_checked_ty) },
        .def = def,
        .kind = .nominal,
        .args = Type.Span.empty(),
        .backing = .{ .ty = second_backing, .use = .inspectable },
    } });
    const binder: check.CheckedModule.PatternBinderId = @enumFromInt(1);
    const first = try program.addLocalWithBinder(@enumFromInt(1), first_ty, binder);
    const second = try program.addLocalWithBinder(@enumFromInt(2), second_ty, binder);
    const replacement = try program.addLocal(@enumFromInt(3), first_ty);
    const replacement_expr = try program.addExpr(.{ .ty = first_ty, .data = .{ .local = replacement } });
    const second_expr = try program.addExpr(.{ .ty = second_ty, .data = .{ .local = second } });

    var pass = try Pass.init(program.allocator, program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    try cloner.subst.put(program, first, .{ .expr = replacement_expr });
    return .{ .cloned = try cloner.cloneExpr(second_expr), .replacement = replacement, .second_ty = second_ty };
}

test "substitution keeps a typed boundary between named types with distinct representations" {
    var program = emptyLiftedProgramForTest(std.testing.allocator);
    defer program.deinit();
    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const u16_ty = try program.types.add(.{ .primitive = .u16 });

    const result = try substituteNamedForTest(&program, 1, u8_ty, 1, u16_ty);
    const boundary = program.getExpr(result.cloned).data.typed_boundary;
    try std.testing.expectEqual(result.second_ty, program.getExpr(result.cloned).ty);
    try std.testing.expectEqual(result.replacement, program.getExpr(boundary.value).data.local);
}

test "substitution resolves named types that differ only in checked provenance" {
    var program = emptyLiftedProgramForTest(std.testing.allocator);
    defer program.deinit();
    const u8_ty = try program.types.add(.{ .primitive = .u8 });

    const result = try substituteNamedForTest(&program, 1, u8_ty, 2, u8_ty);
    try std.testing.expectEqual(result.replacement, program.getExpr(result.cloned).data.local);
}

test "known match fold aborts on undecidable branches and keeps the match when every branch is excluded" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();

    const u8_ty = try program.types.add(.{ .primitive = .u8 });
    const union_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });
    const foo = try program.names.internTagLabel("Foo");
    const bar = try program.names.internTagLabel("Bar");

    const foo_value = Value{ .tag = .{ .ty = union_ty, .name = foo, .payloads = &.{} } };
    const foo_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{ .name = foo, .payloads = Ast.Span(Ast.PatId).empty() } } });
    const bar_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .tag = .{ .name = bar, .payloads = Ast.Span(Ast.PatId).empty() } } });
    const list_pat = try program.addPat(.{ .ty = union_ty, .data = .{ .list = .{
        .patterns = Ast.Span(Ast.PatId).empty(),
        .rest = null,
    } } });
    const body = try program.addExpr(.{ .ty = u8_ty, .data = .unit });
    // This source was built after the clone began, so mark it as source.
    cloner.output_start = program.exprCount();

    // An undecidable branch before any definite match aborts the fold: the
    // residual match stays in the output.
    const undecidable_branches = try program.addBranchSpan(&.{
        .{ .pat = list_pat, .body = body },
        .{ .pat = foo_pat, .body = body },
    });
    var bindings: BindingChain = .{};
    try std.testing.expectEqual(@as(?Value, null), try cloner.simplifyKnownMatchValue(foo_value, undecidable_branches, &bindings));

    // A definite match after definite no-matches folds.
    const folding_branches = try program.addBranchSpan(&.{
        .{ .pat = bar_pat, .body = body },
        .{ .pat = foo_pat, .body = body },
    });
    try std.testing.expect((try cloner.simplifyKnownMatchValue(foo_value, folding_branches, &bindings)) != null);

    // A value no branch matches leaves the match in place: a match checking
    // could not prove exhaustive fails through its own failure path.
    const excluded_branches = try program.addBranchSpan(&.{
        .{ .pat = bar_pat, .body = body },
    });
    try std.testing.expectEqual(@as(?Value, null), try cloner.simplifyKnownMatchValue(foo_value, excluded_branches, &bindings));
}

test "known match fold preserves absurd elimination of a structural product" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();

    const empty_union_ty = try program.types.add(.{ .tag_union = Type.Span.empty() });
    const field_name = try program.names.internRecordFieldLabel("impossible");
    const record_ty = try program.types.add(.{ .record = try program.types.addRecordFields(&program.names, &.{
        .{ .name = field_name, .ty = empty_union_ty, .default = null },
    }) });
    const impossible_local = try program.addLocal(@enumFromInt(1), empty_union_ty);
    const impossible_expr = try program.addExpr(.{ .ty = empty_union_ty, .data = .{ .local = impossible_local } });
    const record_value = Value{ .record = .{
        .ty = record_ty,
        .fields = &.{.{ .name = field_name, .value = .{ .expr = impossible_expr } }},
    } };

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForRewrite(&pass);
    defer cloner.deinit();
    var bindings: BindingChain = .{};
    try std.testing.expectEqual(
        @as(?Value, null),
        try cloner.simplifyKnownMatchValue(record_value, Ast.Span(Ast.Branch).empty(), &bindings),
    );
}

test "call-pattern specialization declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "SpecConstr loop projection scan is stack safe on deep sequential expressions" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const unit_ty = try program.types.add(.zst);
    const unit = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    var body = unit;
    for (0..50_000) |_| {
        const local = try program.addLocal(@enumFromInt(@as(u32, @intCast(program.localsView().len))), unit_ty);
        const pat = try program.addPat(.{ .ty = unit_ty, .data = .{ .bind = local } });
        body = try program.addExpr(.{ .ty = unit_ty, .data = .{ .let_ = .{ .bind = pat, .value = unit, .rest = body } } });
    }
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var demand = ExitDemand.Inventory.init(allocator, &program);
    defer demand.deinit();
    try demand.collect(body);
    try std.testing.expect(!demand.hasSelection());
}

// These fixtures exercise the exit ABI independently of front-end inlining.
fn testExitProducer(program: *Ast.Program, ty: Type.TypeId, symbol: u32) std.mem.Allocator.Error!Ast.ExprId {
    const fn_id = try program.addFn(.{
        .shapes = program.finishFnShapes(.{}),
        .symbol = @enumFromInt(symbol),
        .args = .empty(),
        .captures = .empty(),
        .body = .hosted,
        .ret = ty,
    });
    return program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{ .callee = .{ .lifted = fn_id }, .args = .empty() } } });
}

test "loop exit projection evaluates an opaque producer once for multiple fields" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.{ .primitive = .u8 });
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ ty, ty, ty }) });
    const producer = try testExitProducer(&program, tuple_ty, 1);
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForLoopExitSelection(&pass);
    defer cloner.deinit();
    const exit_join = pass.freshJoinPoint();
    const result = try cloner.cloneSelectedLoopExit(ty, producer, .{
        .source_ty = tuple_ty,
        .source_arity = 3,
        .kept_indices = &.{ 2, 0 },
        .kept_types = &.{ ty, ty },
        .transfer = .{ .jump = .{
            .target = exit_join,
        } },
    });
    const block = program.getExpr(result).data.block;
    try std.testing.expectEqual(@as(u32, 1), block.statements.len);
    const binding = program.getStmt(GuardedList.at(program.stmtSpan(block.statements), 0)).let_;
    try std.testing.expectEqualDeep(program.getExpr(producer).data.call_proc.callee, program.getExpr(binding.value).data.call_proc.callee);
    try std.testing.expectEqual(@as(u32, 0), program.getExpr(binding.value).data.call_proc.args.len);
    const local = program.getPat(binding.pat).data.bind;
    const jump = program.getExpr(block.final_expr).data.jump;
    try std.testing.expectEqual(exit_join, jump.target);
    try std.testing.expectEqual(@as(u32, 2), jump.args.len);
    for ([_]u32{ 2, 0 }, 0..) |field, i| {
        const read = program.getExpr(GuardedList.at(program.exprSpan(jump.args), i));
        try std.testing.expectEqual(ty, read.ty);
        try std.testing.expectEqual(field, read.data.tuple_access.elem_index);
        try std.testing.expectEqual(local, program.getExpr(read.data.tuple_access.tuple).data.local);
    }
}

test "loop exit projection preserves discarded strict components in evaluation order" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.{ .primitive = .u8 });
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ ty, ty, ty }) });
    const producers = [_]Ast.ExprId{
        try testExitProducer(&program, ty, 1),
        try testExitProducer(&program, ty, 2),
        try testExitProducer(&program, ty, 3),
    };
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&producers) } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForLoopExitSelection(&pass);
    defer cloner.deinit();
    const result = try cloner.cloneSelectedLoopExit(ty, tuple, .{
        .source_ty = tuple_ty,
        .source_arity = 3,
        .kept_indices = &.{1},
        .kept_types = &.{ty},
        .transfer = .break_value,
    });
    const block = program.getExpr(result).data.block;
    try std.testing.expectEqual(@as(u32, 3), block.statements.len);
    for (producers, 0..) |producer, i| {
        const binding = program.getStmt(GuardedList.at(program.stmtSpan(block.statements), i)).let_;
        try std.testing.expectEqualDeep(program.getExpr(producer).data.call_proc.callee, program.getExpr(binding.value).data.call_proc.callee);
        try std.testing.expectEqual(@as(u32, 0), program.getExpr(binding.value).data.call_proc.args.len);
    }
    const selected = program.getStmt(GuardedList.at(program.stmtSpan(block.statements), 1)).let_;
    const value = program.getExpr(block.final_expr).data.break_.?;
    try std.testing.expectEqual(program.getPat(selected.pat).data.bind, program.getExpr(value).data.local);
}

test "loop exit demand is linear in tuple width and rejects whole tuple uses" {
    const allocator = std.testing.allocator;
    for ([_]usize{ 8, 128 }) |width| {
        var program = emptyLiftedProgramForTest(allocator);
        defer program.deinit();
        const ty = try program.types.add(.zst);
        const tys = try allocator.alloc(Type.TypeId, width);
        defer allocator.free(tys);
        @memset(tys, ty);
        const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(tys) });
        const input = try program.addLocal(@enumFromInt(1), tuple_ty);
        const ref = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .local = input } });
        const exit = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .break_ = ref } });
        const loop = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .loop_ = .{
            .params = .empty(),
            .initial_values = .empty(),
            .body = exit,
        } } });
        const pats = try allocator.alloc(Ast.PatId, width);
        defer allocator.free(pats);
        for (pats, 0..) |*pat, i| pat.* = try program.addPat(.{
            .ty = ty,
            .data = .{ .bind = try program.addLocal(@enumFromInt(@as(u32, @intCast(i)) + 2), ty) },
        });
        const pat = try program.addPat(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addPatSpan(pats) } });
        const uses = try allocator.alloc(Ast.ExprId, width);
        defer allocator.free(uses);
        for (uses) |*use| use.* = try program.addExpr(.{ .ty = ty, .data = .{ .local = program.getPat(pats[0]).data.bind } });
        const rest = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(uses) } });
        const body = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .let_ = .{ .bind = pat, .value = loop, .rest = rest } } });
        var demand = ExitDemand.Inventory.init(allocator, &program);
        defer demand.deinit();
        try demand.collect(body);
        try std.testing.expect(demand.get(pat) != null);
        // One body walk, independent of how many locals the pattern defines.
        try std.testing.expectEqual(width + 5, demand.expr_visits);

        const aggregate = try program.addLocal(@enumFromInt(@as(u32, @intCast(width)) + 2), tuple_ty);
        const aggregate_pat = try program.addPat(.{ .ty = tuple_ty, .data = .{ .bind = aggregate } });
        const aggregate_ref = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .local = aggregate } });
        const whole_use = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .let_ = .{
            .bind = aggregate_pat,
            .value = loop,
            .rest = aggregate_ref,
        } } });
        var whole = ExitDemand.Inventory.init(allocator, &program);
        defer whole.deinit();
        try whole.collect(whole_use);
        try std.testing.expect(!whole.hasSelection());
    }
}

test "loop exit projection retains typed boundaries around known tuples" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.zst);
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ ty, ty }) });
    const unit = try program.addExpr(.{ .ty = ty, .data = .unit });
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{ unit, unit }) } });
    const boundary = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .typed_boundary = .{ .value = tuple } } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForLoopExitSelection(&pass);
    defer cloner.deinit();
    const result = try cloner.cloneSelectedLoopExit(ty, boundary, .{
        .source_ty = tuple_ty,
        .source_arity = 2,
        .kept_indices = &.{0},
        .kept_types = &.{ty},
        .transfer = .break_value,
    });
    const block = program.getExpr(result).data.block;
    try std.testing.expectEqual(@as(u32, 1), block.statements.len);
    const binding = program.getStmt(GuardedList.at(program.stmtSpan(block.statements), 0)).let_;
    try std.testing.expect(program.getExpr(binding.value).data == .typed_boundary);
    const read = program.getExpr(program.getExpr(block.final_expr).data.break_.?).data.tuple_access;
    try std.testing.expectEqual(program.getPat(binding.pat).data.bind, program.getExpr(read.tuple).data.local);
}

test "loop exit projection preserves nested exits and established tuple parameters" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.zst);
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ ty, ty }) });
    const unit = try program.addExpr(.{ .ty = ty, .data = .unit });
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{ unit, unit }) } });
    const param = try program.addLocal(@enumFromInt(1), tuple_ty);
    const param_ref = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .local = param } });
    const nested_exit = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .break_ = tuple } });
    const nested = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .loop_ = .{
        .params = .empty(),
        .initial_values = .empty(),
        .body = nested_exit,
    } } });
    const statement = try program.addStmt(.{ .expr = nested });
    const exit = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .break_ = param_ref } });
    const body = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .block = .{
        .statements = try program.addStmtSpan(&.{statement}),
        .final_expr = exit,
    } } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForLoopExitSelection(&pass);
    defer cloner.deinit();
    const result = try cloner.cloneLoopWithSelectedExit(ty, Mono.LoopExpr{
        .params = try program.addTypedLocalSpan(&.{.{ .local = param, .ty = tuple_ty }}),
        .initial_values = try program.addExprSpan(&.{tuple}),
        .body = body,
    }, .{ .source_ty = tuple_ty, .source_arity = 2, .kept_indices = &.{0}, .kept_types = &.{ty}, .transfer = .break_value });
    const loop = program.getExpr(result).data.loop_;
    try std.testing.expectEqual(@as(u32, 1), loop.params.len);
    try std.testing.expectEqual(tuple_ty, GuardedList.at(program.typedLocalSpan(loop.params), 0).ty);
    const block = program.getExpr(loop.body).data.block;
    const nested_out = program.getExpr(program.getStmt(GuardedList.at(program.stmtSpan(block.statements), 0)).let_.value).data.loop_;
    try std.testing.expectEqual(tuple_ty, program.getExpr(program.getExpr(nested_out.body).data.break_.?).ty);
    const selected = program.getExpr(program.getExpr(block.final_expr).data.break_.?);
    try std.testing.expectEqual(ty, selected.ty);
    try std.testing.expectEqual(@as(u32, 0), selected.data.tuple_access.elem_index);
    try std.testing.expectEqual(GuardedList.at(program.typedLocalSpan(loop.params), 0).local, program.getExpr(selected.data.tuple_access.tuple).data.local);
}

test "loop exit projection orders an earlier opaque item before a later block chain" {
    const allocator = std.testing.allocator;
    var program = emptyLiftedProgramForTest(allocator);
    defer program.deinit();
    const ty = try program.types.add(.zst);
    const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ ty, ty }) });
    const calls = [_]Ast.ExprId{
        try testExitProducer(&program, ty, 1),
        try testExitProducer(&program, ty, 2),
        try testExitProducer(&program, ty, 3),
    };
    const stmt = try program.addStmt(.{ .expr = calls[1] });
    const block = try program.addExpr(.{ .ty = ty, .data = .{ .block = .{
        .statements = try program.addStmtSpan(&.{stmt}),
        .final_expr = calls[2],
    } } });
    const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{ calls[0], block }) } });
    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    var cloner = Cloner.initForLoopExitSelection(&pass);
    defer cloner.deinit();
    const result = try cloner.cloneSelectedLoopExit(ty, tuple, .{
        .source_ty = tuple_ty,
        .source_arity = 2,
        .kept_indices = &.{1},
        .kept_types = &.{ty},
        .transfer = .break_value,
    });
    const out = program.getExpr(result).data.block;
    try std.testing.expectEqual(@as(u32, 3), out.statements.len);
    for (calls, 0..) |call, i| {
        const binding = program.getStmt(GuardedList.at(program.stmtSpan(out.statements), i)).let_;
        try std.testing.expectEqualDeep(program.getExpr(call).data.call_proc.callee, program.getExpr(binding.value).data.call_proc.callee);
    }
}

test "SpecConstr operand sequencing preserves nested mutable versions" {
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |use_let| {
        var program = emptyLiftedProgramForTest(allocator);
        defer program.deinit();
        const ty = try program.types.add(.{ .primitive = .u8 });
        const tuple_ty = try program.types.add(.{ .tuple = try program.types.addSpan(&.{ty}) });
        const binder: check.CheckedModule.PatternBinderId = @enumFromInt(1);
        const initial = try program.addLocalWithBinder(@enumFromInt(1), ty, binder);
        const version = try program.addLocalWithBinder(@enumFromInt(2), ty, binder);
        const initial_pat = try program.addPat(.{ .ty = ty, .data = .{ .bind = initial } });
        const version_pat = try program.addPat(.{ .ty = ty, .data = .{ .bind = version } });
        const zero = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 0)), .kind = .u128 } } });
        const seven = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 } } });
        const declaration = try program.addStmt(.{ .let_ = .{ .pat = initial_pat, .value = zero } });
        const nested = if (use_let)
            try program.addExpr(.{ .ty = ty, .data = .{ .let_ = .{ .bind = version_pat, .value = seven, .rest = zero } } })
        else
            try program.addExpr(.{ .ty = ty, .data = .{ .block = .{
                .statements = try program.addStmtSpan(&.{try program.addStmt(.{ .let_ = .{ .pat = version_pat, .value = seven } })}),
                .final_expr = zero,
            } } });
        const tuple = try program.addExpr(.{ .ty = tuple_ty, .data = .{ .tuple = try program.addExprSpan(&.{nested}) } });
        const discarded = try program.addStmt(.{ .expr = tuple });
        const later_read = try program.addExpr(.{ .ty = ty, .data = .{ .local = version } });
        const body = try program.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try program.addStmtSpan(&.{ declaration, discarded }),
            .final_expr = later_read,
        } } });
        const fn_id = try program.addFn(.{
            .symbol = @enumFromInt(3),
            .args = .empty(),
            .captures = .empty(),
            .body = .{ .roc = body },
            .ret = ty,
        });
        program.next_symbol = 4;
        try @import("normalize.zig").run(&program);
        var pass = try Pass.init(allocator, &program);
        defer pass.deinit();
        var cloner = Cloner.initForRewrite(&pass);
        defer cloner.deinit();
        const result = try cloner.cloneExpr(program.getFn(fn_id).body.roc);
        try std.testing.expectEqualDeep(program.getExpr(seven).data, program.getExpr(result).data);
    }
}
