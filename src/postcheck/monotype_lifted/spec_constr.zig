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
//!     range_iter = 0.I64..=15
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

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// Whether a checker-stamped compiler procedure constructs an iterator value.
/// The stamp is exact producer data; result type, callee spelling, and call
/// shape are deliberately irrelevant here.
fn isIteratorProducer(procedure: ?check.StaticDispatchRegistry.IteratorProcedureId) bool {
    return if (procedure) |exact| exact.producesIteratorValue() else false;
}

/// The error set of a span-walk visitor: `visit`'s own error set merged with
/// the allocation failure raised while copying the span.
fn WalkSpanError(comptime visit: anytype) type {
    const ret = @typeInfo(@TypeOf(visit)).@"fn".return_type.?;
    return Allocator.Error || @typeInfo(ret).error_union.error_set;
}

/// Copy `slice` into scratch memory, then invoke `visit` for every element.
/// The copy is a mutation-during-iteration guard: element callbacks append to the
/// span stores they walk, so the traversal must iterate a snapshot taken before
/// any element is visited rather than the live span. `context` carries whatever
/// state the callback needs (visitor `self`, owner ids, done markers).
fn walkSpanCloned(
    allocator: Allocator,
    comptime T: type,
    slice: anytype,
    context: anytype,
    comptime visit: anytype,
) WalkSpanError(visit)!void {
    const source = try GuardedList.dupe(allocator, T, slice);
    defer allocator.free(source);
    for (source) |item| try visit(context, item);
}

/// Specialize recursive direct calls whose arguments are known constructor shapes.
pub fn run(allocator: Allocator, program: *Ast.Program) Common.LowerError!void {
    var pass = try Pass.init(allocator, program);
    defer pass.deinit();
    try pass.run();
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

/// Maximum number of `nominal.backing` / `static_data_candidate.runtime` /
/// callable-capture pointer edges any single value-tree strip may follow. A
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
    static_data_candidate: StaticDataCandidateValue,
    tag: TagValue,
    record: RecordValue,
    tuple: TupleValue,
    nominal: NominalValue,
    callable: CallableValue,
};

const StaticDataCandidateValue = struct {
    ty: Type.TypeId,
    static_data: Common.StaticDataId,
    runtime: *const Value,
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

const FnPlan = struct {
    used_args: []bool,
    body_size: BodySize,
    specs: std.ArrayList(Spec),

    fn deinit(self: *FnPlan, allocator: Allocator) void {
        allocator.free(self.used_args);
        self.specs.deinit(allocator);
    }
};

/// A pattern binder paired with the monomorphic type it was bound at. A single
/// source binder is reused across every monomorphization of its binding, so the
/// binder alone does not identify a value; the type digest completes the
/// identity, matching the `(binder, type)` identity Monotype lowering uses for
/// locals. See `Builder.sameLocalIdentity` in monotype/lower.zig.
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

const BindingNode = struct {
    binding: StrictBinding,
    previous: ?*BindingNode = null,
    next: ?*BindingNode = null,
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
        node.* = .{ .binding = binding, .previous = self.last };
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
            std.debug.assert(program.getLocal(node.binding.local).ty == node.binding.ty);
            std.debug.assert(program.getExpr(node.binding.value).ty == node.binding.ty);
            previous = node;
        }
        std.debug.assert(previous == self.last);
        std.debug.assert((self.first == null) == (self.last == null));
    }
};

/// Symbolic structure plus the strict computations that produce its opaque
/// leaves. The chain is placed exactly once before any use of `value`.
const ClonedValue = struct {
    bindings: BindingChain = .{},
    value: Value,
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

/// Exact live items passed from a loop's compiler-generated state result to the
/// continuation that consumes it. Back-edge state is deliberately unaffected:
/// a one-item exit breaks with that existing item type, while a multi-item exit
/// jumps to a typed shared continuation.
const LoopExitSelection = struct {
    source_arity: usize,
    kept_indices: []const u32,
    result_ty: Type.TypeId,
    transfer: union(enum) {
        break_value,
        jump: struct {
            target: Ast.JoinPointId,
            sites: *std.ArrayList(Ast.ExprId),
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
    known_size: usize,
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

        for (plans, 0..) |*plan, index| {
            const fn_ = program.getFnAt(index);
            const args = program.typedLocalSpan(fn_.args);
            const used_args = try allocator.alloc(bool, args.len);
            errdefer allocator.free(used_args);
            @memset(used_args, false);
            plan.* = .{
                .used_args = used_args,
                .body_size = fnBodySizeWithin(program, fn_.body, spec_constr_body_expr_threshold),
                .specs = .empty,
            };
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
            .next_join_point = 0,
        };
    }

    fn freshJoinPoint(self: *Pass) Ast.JoinPointId {
        const id: Ast.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
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

    fn finishAnalysis(self: *Pass, mark: AnalysisMark) void {
        self.program.finishSpecConstrAnalysis(mark.program);
        self.restoreAnalysisIds(mark);
    }

    fn deinit(self: *Pass) void {
        self.callable_sources.deinit();
        self.callable_workers.deinit();
        self.allocator.free(self.whole_body_cloned);
        for (self.plans) |*plan| plan.deinit(self.allocator);
        self.allocator.free(self.plans);
        self.arena.deinit();
    }

    fn run(self: *Pass) Common.LowerError!void {
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
        try self.localizeSingleUseTailRecursiveWorkers(original_fn_count);
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
    fn localizeSingleUseTailRecursiveWorkers(self: *Pass, original_fn_count: usize) Common.LowerError!void {
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
            if (program_usage.contains_return[worker_index]) continue;

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
        const localized_body = try self.rewriteTailSelfCallsAsJumps(cloned_body, worker_id, worker.captures, loop_join);

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
    /// different loop or join point.
    fn rewriteTailSelfCallsAsJumps(
        self: *Pass,
        expr_id: Ast.ExprId,
        worker_id: Ast.FnId,
        capture_slots: Ast.Span(Ast.TypedLocal),
        loop_join: Ast.JoinPointId,
    ) Common.LowerError!Ast.ExprId {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .call_proc => |call| {
                if (Ast.localDirectCallee(call) != worker_id) return expr_id;
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
            .let_ => |let_| {
                var rewritten = let_;
                rewritten.rest = try self.rewriteTailSelfCallsAsJumps(let_.rest, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .let_ = rewritten });
            },
            .match_ => |match| {
                const branches = try GuardedList.dupe(self.allocator, Ast.Branch, self.program.branchSpan(match.branches));
                defer self.allocator.free(branches);
                for (branches) |*branch| {
                    branch.body = try self.rewriteTailSelfCallsAsJumps(branch.body, worker_id, capture_slots, loop_join);
                }
                var rewritten = match;
                rewritten.branches = try self.program.addBranchSpan(branches);
                self.program.setExprData(expr_id, .{ .match_ = rewritten });
            },
            .if_ => |if_| {
                const branches = try GuardedList.dupe(self.allocator, Ast.IfBranch, self.program.ifBranchSpan(if_.branches));
                defer self.allocator.free(branches);
                for (branches) |*branch| {
                    branch.body = try self.rewriteTailSelfCallsAsJumps(branch.body, worker_id, capture_slots, loop_join);
                }
                self.program.setExprData(expr_id, .{ .if_ = .{
                    .branches = try self.program.addIfBranchSpan(branches),
                    .final_else = try self.rewriteTailSelfCallsAsJumps(if_.final_else, worker_id, capture_slots, loop_join),
                } });
            },
            .block => |block| {
                var rewritten = block;
                rewritten.final_expr = try self.rewriteTailSelfCallsAsJumps(block.final_expr, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .block = rewritten });
            },
            .join_point => |join_point| {
                var rewritten = join_point;
                rewritten.body = try self.rewriteTailSelfCallsAsJumps(join_point.body, worker_id, capture_slots, loop_join);
                rewritten.remainder = try self.rewriteTailSelfCallsAsJumps(join_point.remainder, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .join_point = rewritten });
            },
            .if_initialized_payload => |payload_switch| {
                var rewritten = payload_switch;
                rewritten.initialized = try self.rewriteTailSelfCallsAsJumps(payload_switch.initialized, worker_id, capture_slots, loop_join);
                rewritten.uninitialized = try self.rewriteTailSelfCallsAsJumps(payload_switch.uninitialized, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .if_initialized_payload = rewritten });
            },
            .try_sequence => |sequence| {
                var rewritten = sequence;
                rewritten.ok_body = try self.rewriteTailSelfCallsAsJumps(sequence.ok_body, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .try_sequence = rewritten });
            },
            .try_record_sequence => |sequence| {
                var rewritten = sequence;
                rewritten.ok_body = try self.rewriteTailSelfCallsAsJumps(sequence.ok_body, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .try_record_sequence = rewritten });
            },
            .comptime_branch_taken => |taken| {
                var rewritten = taken;
                rewritten.body = try self.rewriteTailSelfCallsAsJumps(taken.body, worker_id, capture_slots, loop_join);
                self.program.setExprData(expr_id, .{ .comptime_branch_taken = rewritten });
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
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => {},
        }
        return expr_id;
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
            self.refreshPreCloneBodySize(index);
        }
    }

    fn refreshPreCloneBodySize(self: *Pass, index: usize) void {
        if (index >= self.plans.len) Common.invariant("SpecConstr body-size refresh received a generated function");
        self.plans[index].body_size = fnBodySizeWithin(
            self.program,
            self.program.getFnAt(index).body,
            spec_constr_body_expr_threshold,
        );
    }

    fn copyProcDebugName(self: *Pass, source_symbol: Common.Symbol, target_symbol: Common.Symbol) Allocator.Error!void {
        if (self.program.procDebugName(source_symbol)) |name| {
            try self.program.setProcDebugName(target_symbol, name);
        }
    }

    fn collectArgUses(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            for (0..original_fn_count) |index| {
                const fn_ = self.program.getFnAt(index);
                const body = switch (fn_.body) {
                    .roc => |body| body,
                    .hosted => continue,
                };
                const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
                try self.markArgUsesInExpr(fn_id, body, &changed);
            }
        }
    }

    fn collectCallPatterns(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        var index: usize = 0;
        while (index < original_fn_count) : (index += 1) {
            const fn_ = self.program.getFnAt(index);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.collectCallPatternsInExpr(fn_id, body);
        }
    }

    /// The syntax-directed collector above cannot see that a direct-call
    /// argument is known when it is first named by a `let`. Walk with the
    /// cloner's substitution environment so those calls still reserve workers.
    fn collectValueAwareCallPatterns(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        const analysis_mark = self.markAnalysis();
        defer self.finishAnalysis(analysis_mark);

        var index: usize = 0;
        while (index < original_fn_count) : (index += 1) {
            const fn_ = self.program.getFnAt(index);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            var cloner = Cloner.initForRewrite(self);
            cloner.rewrite_call_patterns = false;
            cloner.emit_callable_workers = false;
            // Checker-stamped iterator producers enter an explicit fusion
            // context. Their helper calls and generated-private step callables
            // remain visible transitively so the complete iterator pipeline can
            // seed fusion workers. Generic calls outside that context remain
            // opaque until a production clone enforces emitted-code admission.
            cloner.inline_calls = .iterator_fusion;
            cloner.inline_direct_requires_known_arg = true;
            defer cloner.deinit();
            try cloner.collectCallPatternsInExpr(fn_id, body);
            self.rewindAnalysis(analysis_mark);
        }
    }

    fn reserveSpecIds(self: *Pass) Allocator.Error!void {
        for (self.plans, 0..) |*plan, source_index| {
            const source_fn = self.program.getFnAt(source_index);
            for (plan.specs.items) |*spec| {
                const symbol = self.symbols.fresh();
                const fn_id = try self.program.addFn(.{
                    .symbol = symbol,
                    .source = source_fn.source,
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

    fn markArgUsesInExpr(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId, changed: *bool) Allocator.Error!void {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
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
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            => {},
            .fn_ref => |fn_ref| {
                const operands = self.program.captureOperandSpan(fn_ref.captures);
                for (0..operands.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(operands, index).value, changed);
            },
            .list,
            .tuple,
            => |items| {
                const exprs = self.program.exprSpan(items);
                for (0..exprs.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(exprs, index), changed);
            },
            .record => |fields| {
                const field_exprs = self.program.fieldExprSpan(fields);
                for (0..field_exprs.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(field_exprs, index).value, changed);
            },
            .record_update => |update| {
                try self.markArgUsesInExpr(fn_id, update.base, changed);
                const field_exprs = self.program.fieldExprSpan(update.fields);
                for (0..field_exprs.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(field_exprs, index).value, changed);
            },
            .tag => |tag| {
                const payloads = self.program.exprSpan(tag.payloads);
                for (0..payloads.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(payloads, index), changed);
            },
            .static_data_candidate => |candidate| try self.markArgUsesInExpr(fn_id, candidate.runtime_expr, changed),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.markArgUsesInExpr(fn_id, child, changed),
            .return_ => |ret| try self.markArgUsesInExpr(fn_id, ret.value, changed),
            .expect_err => |expect_err| try self.markArgUsesInExpr(fn_id, expect_err.msg, changed),
            .comptime_branch_taken => |taken| try self.markArgUsesInExpr(fn_id, taken.body, changed),
            .let_ => |let_| {
                try self.markArgUsesInExpr(fn_id, let_.value, changed);
                try self.markArgUsesInExpr(fn_id, let_.rest, changed);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .call_value => |call| {
                try self.markArgUsesInExpr(fn_id, call.callee, changed);
                const args = self.program.exprSpan(call.args);
                for (0..args.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(args, index), changed);
            },
            .call_proc => |call| {
                const args = self.program.exprSpan(call.args);
                for (0..args.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(args, index), changed);
                const captures = self.program.captureOperandSpan(call.captures);
                for (0..captures.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(captures, index).value, changed);
                const callee = Ast.localDirectCallee(call) orelse return;
                const callee_raw = @intFromEnum(callee);
                if (callee_raw < self.plans.len) {
                    const callee_uses = self.plans[callee_raw].used_args;
                    if (args.len != callee_uses.len) Common.invariant("direct call arity differed from lifted function arity while propagating argument uses");
                    for (0..args.len) |index| {
                        const arg = GuardedList.at(args, index);
                        const callee_uses_arg = callee_uses[index];
                        if (callee_uses_arg) {
                            self.markArgUseIfLocal(fn_id, arg, changed);
                        }
                    }
                }
            },
            .low_level => |call| {
                const args = self.program.exprSpan(call.args);
                for (0..args.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(args, index), changed);
            },
            .field_access => |field| {
                self.markArgUseIfLocal(fn_id, field.receiver, changed);
                try self.markArgUsesInExpr(fn_id, field.receiver, changed);
            },
            .tuple_access => |access| {
                self.markArgUseIfLocal(fn_id, access.tuple, changed);
                try self.markArgUsesInExpr(fn_id, access.tuple, changed);
            },
            .structural_eq => |eq| {
                try self.markArgUsesInExpr(fn_id, eq.lhs, changed);
                try self.markArgUsesInExpr(fn_id, eq.rhs, changed);
            },
            .structural_hash => |h| {
                try self.markArgUsesInExpr(fn_id, h.value, changed);
                try self.markArgUsesInExpr(fn_id, h.hasher, changed);
            },
            .match_ => |match| {
                self.markArgUseIfLocal(fn_id, match.scrutinee, changed);
                try self.markArgUsesInExpr(fn_id, match.scrutinee, changed);
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    const bindings = self.program.stmtSpan(branch.bindings);
                    for (0..bindings.len) |binding_index| {
                        try self.markArgUsesInStmt(fn_id, GuardedList.at(bindings, binding_index), changed);
                    }
                    if (branch.guard) |guard| try self.markArgUsesInExpr(fn_id, guard, changed);
                    try self.markArgUsesInExpr(fn_id, branch.body, changed);
                }
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    try self.markArgUsesInExpr(fn_id, branch.cond, changed);
                    try self.markArgUsesInExpr(fn_id, branch.body, changed);
                }
                try self.markArgUsesInExpr(fn_id, if_.final_else, changed);
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| try self.markArgUsesInStmt(fn_id, GuardedList.at(statements, index), changed);
                try self.markArgUsesInExpr(fn_id, block.final_expr, changed);
            },
            .loop_ => |loop| {
                const initial_values = self.program.exprSpan(loop.initial_values);
                for (0..initial_values.len) |index| {
                    const initial = GuardedList.at(initial_values, index);
                    // A loop-carried argument is a shape-relevant use: the
                    // split scalarizes the slot only when the entry shape is
                    // known, so a caller must expose the construction it
                    // passes here.
                    self.markArgUseIfLocal(fn_id, initial, changed);
                    try self.markArgUsesInExpr(fn_id, initial, changed);
                }
                try self.markArgUsesInExpr(fn_id, loop.body, changed);
            },
            .break_ => |maybe| if (maybe) |value| try self.markArgUsesInExpr(fn_id, value, changed),
            .continue_ => |continue_| {
                const values = self.program.exprSpan(continue_.values);
                for (0..values.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(values, index), changed);
            },
            .join_point => |join_point| {
                try self.markArgUsesInExpr(fn_id, join_point.body, changed);
                try self.markArgUsesInExpr(fn_id, join_point.remainder, changed);
            },
            .jump => |jump| {
                const args = self.program.exprSpan(jump.args);
                for (0..args.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(args, index), changed);
            },
            .if_initialized_payload => |payload_switch| {
                try self.markArgUsesInExpr(fn_id, payload_switch.cond, changed);
                try self.markArgUsesInExpr(fn_id, payload_switch.initialized, changed);
                try self.markArgUsesInExpr(fn_id, payload_switch.uninitialized, changed);
            },
            .try_sequence => |sequence| {
                try self.markArgUsesInExpr(fn_id, sequence.try_expr, changed);
                try self.markArgUsesInExpr(fn_id, sequence.ok_body, changed);
            },
            .try_record_sequence => |sequence| {
                try self.markArgUsesInExpr(fn_id, sequence.try_expr, changed);
                try self.markArgUsesInExpr(fn_id, sequence.ok_body, changed);
            },
        }
    }

    fn markArgUsesInStmt(self: *Pass, fn_id: Ast.FnId, stmt_id: Ast.StmtId, changed: *bool) Allocator.Error!void {
        switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| try self.markArgUsesInExpr(fn_id, let_.value, changed),
            .expr,
            .expect,
            .dbg,
            => |expr| try self.markArgUsesInExpr(fn_id, expr, changed),
            .return_ => |ret| try self.markArgUsesInExpr(fn_id, ret.value, changed),
            .uninitialized, .crash => {},
        }
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

    fn collectCallPatternsInExpr(self: *Pass, owner: Ast.FnId, expr_id: Ast.ExprId) Allocator.Error!void {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
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
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            => {},
            .fn_ref => |fn_ref| try self.collectCallPatternsInCaptureOperandSpan(owner, fn_ref.captures),
            .list,
            .tuple,
            => |items| try self.collectCallPatternsInExprSpan(owner, items),
            .record => |fields| try self.collectCallPatternsInFieldExprSpan(owner, fields),
            .record_update => |update| {
                try self.collectCallPatternsInExpr(owner, update.base);
                try self.collectCallPatternsInFieldExprSpan(owner, update.fields);
            },
            .tag => |tag| try self.collectCallPatternsInExprSpan(owner, tag.payloads),
            .static_data_candidate => |candidate| try self.collectCallPatternsInExpr(owner, candidate.runtime_expr),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.collectCallPatternsInExpr(owner, child),
            .return_ => |ret| try self.collectCallPatternsInExpr(owner, ret.value),
            .expect_err => |expect_err| try self.collectCallPatternsInExpr(owner, expect_err.msg),
            .comptime_branch_taken => |taken| try self.collectCallPatternsInExpr(owner, taken.body),
            .let_ => |let_| {
                try self.collectCallPatternsInExpr(owner, let_.value);
                try self.collectCallPatternsInExpr(owner, let_.rest);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .call_value => |call| {
                try self.collectCallPatternsInExpr(owner, call.callee);
                try self.collectCallPatternsInExprSpan(owner, call.args);
            },
            .call_proc => |call| {
                try self.collectCallPatternsInExprSpan(owner, call.args);
                try self.collectCallPatternsInCaptureOperandSpan(owner, call.captures);
                const callee = Ast.localDirectCallee(call) orelse return;
                if (@intFromEnum(callee) < self.plans.len) try self.recordCallPattern(callee, call.args);
            },
            .low_level => |call| {
                try self.collectCallPatternsInExprSpan(owner, call.args);
            },
            .field_access => |field| try self.collectCallPatternsInExpr(owner, field.receiver),
            .tuple_access => |access| try self.collectCallPatternsInExpr(owner, access.tuple),
            .structural_eq => |eq| {
                try self.collectCallPatternsInExpr(owner, eq.lhs);
                try self.collectCallPatternsInExpr(owner, eq.rhs);
            },
            .structural_hash => |h| {
                try self.collectCallPatternsInExpr(owner, h.value);
                try self.collectCallPatternsInExpr(owner, h.hasher);
            },
            .match_ => |match| {
                try self.collectCallPatternsInExpr(owner, match.scrutinee);
                try self.collectCallPatternsInBranchSpan(owner, match.branches);
            },
            .if_ => |if_| {
                try self.collectCallPatternsInIfBranchSpan(owner, if_.branches);
                try self.collectCallPatternsInExpr(owner, if_.final_else);
            },
            .block => |block| {
                try self.collectCallPatternsInStmtSpan(owner, block.statements);
                try self.collectCallPatternsInExpr(owner, block.final_expr);
            },
            .loop_ => |loop| {
                try self.collectCallPatternsInExprSpan(owner, loop.initial_values);
                try self.collectCallPatternsInExpr(owner, loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectCallPatternsInExpr(owner, value),
            .continue_ => |continue_| try self.collectCallPatternsInExprSpan(owner, continue_.values),
            .join_point => |join_point| {
                try self.collectCallPatternsInExpr(owner, join_point.body);
                try self.collectCallPatternsInExpr(owner, join_point.remainder);
            },
            .jump => |jump| try self.collectCallPatternsInExprSpan(owner, jump.args),
            .if_initialized_payload => |payload_switch| {
                try self.collectCallPatternsInExpr(owner, payload_switch.cond);
                try self.collectCallPatternsInExpr(owner, payload_switch.initialized);
                try self.collectCallPatternsInExpr(owner, payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.collectCallPatternsInExpr(owner, sequence.try_expr);
                try self.collectCallPatternsInExpr(owner, sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.collectCallPatternsInExpr(owner, sequence.try_expr);
                try self.collectCallPatternsInExpr(owner, sequence.ok_body);
            },
        }
    }

    fn collectCallPatternsInExprSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.ExprId, self.program.exprSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, expr: Ast.ExprId) Allocator.Error!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, expr);
            }
        }.visit);
    }

    fn collectCallPatternsInCaptureOperandSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.CaptureOperand, self.program.captureOperandSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, operand: Ast.CaptureOperand) Allocator.Error!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, operand.value);
            }
        }.visit);
    }

    fn collectCallPatternsInFieldExprSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.FieldExpr, self.program.fieldExprSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, field: Ast.FieldExpr) Allocator.Error!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, field.value);
            }
        }.visit);
    }

    fn collectCallPatternsInBranchSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.Branch)) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.Branch, self.program.branchSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, branch: Ast.Branch) Allocator.Error!void {
                try ctx.self.collectCallPatternsInStmtSpan(ctx.owner, branch.bindings);
                if (branch.guard) |guard| try ctx.self.collectCallPatternsInExpr(ctx.owner, guard);
                try ctx.self.collectCallPatternsInExpr(ctx.owner, branch.body);
            }
        }.visit);
    }

    fn collectCallPatternsInIfBranchSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.IfBranch)) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.IfBranch, self.program.ifBranchSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, branch: Ast.IfBranch) Allocator.Error!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, branch.cond);
                try ctx.self.collectCallPatternsInExpr(ctx.owner, branch.body);
            }
        }.visit);
    }

    fn collectCallPatternsInStmtSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.StmtId, self.program.stmtSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, stmt: Ast.StmtId) Allocator.Error!void {
                try ctx.self.collectCallPatternsInStmt(ctx.owner, stmt);
            }
        }.visit);
    }

    fn collectCallPatternsInStmt(self: *Pass, owner: Ast.FnId, stmt_id: Ast.StmtId) Allocator.Error!void {
        switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| try self.collectCallPatternsInExpr(owner, let_.value),
            .expr,
            .expect,
            .dbg,
            => |expr| try self.collectCallPatternsInExpr(owner, expr),
            .return_ => |ret| try self.collectCallPatternsInExpr(owner, ret.value),
            .uninitialized, .crash => {},
        }
    }

    fn newSpecAdmission(self: *const Pass, raw: usize) SpecAdmission {
        if (!self.plans[raw].body_size.admits()) return .denied_body_size;
        if (self.plans[raw].specs.items.len >= spec_constr_specialization_count) return .denied_spec_count;
        return .admitted;
    }

    fn inlineBodySize(self: *const Pass, fn_id: Ast.FnId, body: Ast.ExprId) BodySize {
        const raw = @intFromEnum(fn_id);
        return if (raw < self.plans.len)
            self.plans[raw].body_size
        else
            exprBodySizeWithin(self.program, body, spec_constr_body_expr_threshold);
    }

    fn inlineBodyAdmission(self: *const Pass, fn_id: Ast.FnId, body: Ast.ExprId) SpecAdmission {
        return if (self.inlineBodySize(fn_id, body).admits()) .admitted else .denied_body_size;
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
            if (patternEql(self.program, spec.pattern, pattern)) return;
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
        for (self.plans[raw].specs.items) |spec| {
            if (patternEql(self.program, spec.pattern, pattern)) return;
        }

        try self.plans[raw].specs.append(self.allocator, .{
            .pattern = pattern,
        });
    }

    fn ensureCallPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return;
        if (!self.plans[raw].body_size.admits()) return;

        const pattern = (try self.callPatternForValues(fn_id, values)) orelse return;
        for (self.plans[raw].specs.items) |spec| {
            if (patternEql(self.program, spec.pattern, pattern)) return;
        }
        if (self.newSpecAdmission(raw) != .admitted) return;

        const source_fn = self.program.getFnAt(raw);
        const symbol = self.symbols.fresh();
        const fn_id_reserved = try self.program.addFn(.{
            .symbol = symbol,
            .source = source_fn.source,
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

        try cloner.inline_stack.append(self.allocator, .{ .fn_id = source_fn_id, .known_size = 0 });
        defer {
            const popped = cloner.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow while writing specialization");
            if (popped.fn_id != source_fn_id) Common.invariant("call-pattern inline stack was corrupted while writing specialization");
        }

        const args = try cloner.buildArgs();
        const body: Ast.FnBody = switch (source_fn.body) {
            .roc => |body_expr| .{ .roc = try cloner.cloneExpr(body_expr) },
            .hosted => Common.invariant("hosted function had a call-pattern specialization"),
        };

        self.program.setFn(spec_fn_id, .{
            .symbol = symbol,
            .source = source_fn.source,
            .signature = null,
            .args = args,
            .captures = source_fn.captures,
            .body = body,
            .ret = source_fn.ret,
        });
        try self.copyProcDebugName(source_fn.symbol, symbol);
    }

    fn rewriteExistingCalls(self: *Pass) Allocator.Error!void {
        const done = try self.allocator.alloc(bool, self.program.exprCount());
        defer self.allocator.free(done);
        @memset(done, false);

        const fn_count = self.program.fnCount();
        for (0..fn_count) |index| {
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
            const body = switch (self.program.getFnAt(index).body) {
                .roc => |body| body,
                .hosted => continue,
            };
            try self.cloneFnBodyInPlace(fn_id, body);
        }
    }

    /// Whether cloning only a loop would lose the enclosing tail's exact
    /// demand for its compiler-generated state result. Such a function is
    /// cloned as a whole so `loopWithSelectedExitValues` can omit unused
    /// exit fields while leaving the complete back-edge state intact.
    fn bodyHasProjectableLoopResult(self: *Pass, expr_id: Ast.ExprId) bool {
        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .let_ => |let_| blk: {
                break :blk (self.program.getExpr(let_.value).data == .loop_ and
                    self.tuplePatternIsPartiallyUsedInExpr(let_.bind, let_.rest)) or
                    self.bodyHasProjectableLoopResult(let_.value) or
                    self.bodyHasProjectableLoopResult(let_.rest);
            },
            .block => |block| blk: {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| {
                    const stmt_id = GuardedList.at(statements, index);
                    switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| {
                            if (self.program.getExpr(let_.value).data == .loop_ and
                                self.tuplePatternIsPartiallyUsedInBlockTail(
                                    let_.pat,
                                    statements,
                                    index + 1,
                                    block.final_expr,
                                )) break :blk true;
                            if (self.bodyHasProjectableLoopResult(let_.value)) break :blk true;
                        },
                        .expr, .expect, .dbg => |value| if (self.bodyHasProjectableLoopResult(value)) break :blk true,
                        .return_ => |ret| if (self.bodyHasProjectableLoopResult(ret.value)) break :blk true,
                        .uninitialized, .crash => {},
                    }
                }
                break :blk self.bodyHasProjectableLoopResult(block.final_expr);
            },
            .if_ => |if_| blk: {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    if (self.bodyHasProjectableLoopResult(branch.cond) or
                        self.bodyHasProjectableLoopResult(branch.body)) break :blk true;
                }
                break :blk self.bodyHasProjectableLoopResult(if_.final_else);
            },
            .match_ => |match| blk: {
                if (self.bodyHasProjectableLoopResult(match.scrutinee)) break :blk true;
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    const bindings = self.program.stmtSpan(branch.bindings);
                    for (0..bindings.len) |binding_index| {
                        if (self.stmtHasProjectableLoopResult(GuardedList.at(bindings, binding_index))) break :blk true;
                    }
                    if (branch.guard) |guard| if (self.bodyHasProjectableLoopResult(guard)) break :blk true;
                    if (self.bodyHasProjectableLoopResult(branch.body)) break :blk true;
                }
                break :blk false;
            },
            .loop_ => |loop| self.bodyHasProjectableLoopResult(loop.body),
            .nominal, .dbg, .expect => |child| self.bodyHasProjectableLoopResult(child),
            .comptime_branch_taken => |taken| self.bodyHasProjectableLoopResult(taken.body),
            .join_point => |join_point| self.bodyHasProjectableLoopResult(join_point.body) or
                self.bodyHasProjectableLoopResult(join_point.remainder),
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
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
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
            .break_,
            .continue_,
            .jump,
            .return_,
            .crash,
            .comptime_exhaustiveness_failed,
            .expect_err,
            => false,
        };
    }

    fn stmtHasProjectableLoopResult(self: *Pass, stmt_id: Ast.StmtId) bool {
        return switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| self.bodyHasProjectableLoopResult(let_.value),
            .expr, .expect, .dbg => |expr| self.bodyHasProjectableLoopResult(expr),
            .return_ => |ret| self.bodyHasProjectableLoopResult(ret.value),
            .uninitialized, .crash => false,
        };
    }

    fn bodyHasAggregateProjectableLoopResult(self: *Pass, expr_id: Ast.ExprId) Common.LowerError!bool {
        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .let_ => |let_| (self.program.getExpr(let_.value).data == .loop_ and
                try self.aggregateLoopBindingIsPartiallyUsedInExpr(let_.bind, let_.value, let_.rest)) or
                try self.bodyHasAggregateProjectableLoopResult(let_.value) or
                try self.bodyHasAggregateProjectableLoopResult(let_.rest),
            .block => |block| blk: {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| {
                    const stmt_id = GuardedList.at(statements, index);
                    switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| {
                            if (self.program.getExpr(let_.value).data == .loop_ and
                                try self.aggregateLoopBindingIsPartiallyUsedInBlockTail(
                                    let_.pat,
                                    let_.value,
                                    statements,
                                    index + 1,
                                    block.final_expr,
                                )) break :blk true;
                            if (try self.bodyHasAggregateProjectableLoopResult(let_.value)) break :blk true;
                        },
                        .expr, .expect, .dbg => |value| if (try self.bodyHasAggregateProjectableLoopResult(value)) break :blk true,
                        .return_ => |ret| if (try self.bodyHasAggregateProjectableLoopResult(ret.value)) break :blk true,
                        .uninitialized, .crash => {},
                    }
                }
                break :blk try self.bodyHasAggregateProjectableLoopResult(block.final_expr);
            },
            .if_ => |if_| blk: {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    if (try self.bodyHasAggregateProjectableLoopResult(branch.cond) or
                        try self.bodyHasAggregateProjectableLoopResult(branch.body)) break :blk true;
                }
                break :blk try self.bodyHasAggregateProjectableLoopResult(if_.final_else);
            },
            .match_ => |match| blk: {
                if (try self.bodyHasAggregateProjectableLoopResult(match.scrutinee)) break :blk true;
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    const bindings = self.program.stmtSpan(branch.bindings);
                    for (0..bindings.len) |binding_index| {
                        if (try self.stmtHasAggregateProjectableLoopResult(GuardedList.at(bindings, binding_index))) break :blk true;
                    }
                    if (branch.guard) |guard| {
                        if (try self.bodyHasAggregateProjectableLoopResult(guard)) break :blk true;
                    }
                    if (try self.bodyHasAggregateProjectableLoopResult(branch.body)) break :blk true;
                }
                break :blk false;
            },
            .loop_ => |loop| try self.bodyHasAggregateProjectableLoopResult(loop.body),
            .nominal, .dbg, .expect => |child| try self.bodyHasAggregateProjectableLoopResult(child),
            .comptime_branch_taken => |taken| try self.bodyHasAggregateProjectableLoopResult(taken.body),
            .join_point => |join_point| try self.bodyHasAggregateProjectableLoopResult(join_point.body) or
                try self.bodyHasAggregateProjectableLoopResult(join_point.remainder),
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
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
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
            .break_,
            .continue_,
            .jump,
            .return_,
            .crash,
            .comptime_exhaustiveness_failed,
            .expect_err,
            => false,
        };
    }

    fn stmtHasAggregateProjectableLoopResult(self: *Pass, stmt_id: Ast.StmtId) Common.LowerError!bool {
        return switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| try self.bodyHasAggregateProjectableLoopResult(let_.value),
            .expr, .expect, .dbg => |expr| try self.bodyHasAggregateProjectableLoopResult(expr),
            .return_ => |ret| try self.bodyHasAggregateProjectableLoopResult(ret.value),
            .uninitialized, .crash => false,
        };
    }

    fn aggregateLoopBindingIsPartiallyUsedInExpr(
        self: *Pass,
        pat_id: Ast.PatId,
        loop_id: Ast.ExprId,
        rest: Ast.ExprId,
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
        if (!collectTupleLocalDemandInExpr(self.program, local, rest, used)) return false;
        const used_count = std.mem.count(bool, used, &.{true});
        return used_count != 0 and used_count != items.len;
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
            if (!collectTupleLocalDemandInStmt(self.program, local, GuardedList.at(statements, index), used)) return false;
        }
        if (!collectTupleLocalDemandInExpr(self.program, local, final_expr, used)) return false;
        const used_count = std.mem.count(bool, used, &.{true});
        return used_count != 0 and used_count != items.len;
    }

    fn tuplePatternIsPartiallyUsedInExpr(self: *Pass, pat_id: Ast.PatId, rest: Ast.ExprId) bool {
        const pat_data = self.program.getPat(pat_id).data;
        if (pat_data != .tuple) return false;
        const items = self.program.patSpan(pat_data.tuple);
        if (items.len < 2) return false;
        var used: usize = 0;
        for (0..items.len) |index| {
            const item_data = self.program.getPat(GuardedList.at(items, index)).data;
            if (item_data != .bind) return false;
            const local = item_data.bind;
            if (localUseCountInExpr(self.program, local, rest) != 0) used += 1;
        }
        return used != 0 and used != items.len;
    }

    fn tuplePatternIsPartiallyUsedInBlockTail(
        self: *Pass,
        pat_id: Ast.PatId,
        statements: Ast.ProgramSpanBorrow(Ast.StmtId, "stmt_ids"),
        tail_start: usize,
        final_expr: Ast.ExprId,
    ) bool {
        const pat_data = self.program.getPat(pat_id).data;
        if (pat_data != .tuple) return false;
        const items = self.program.patSpan(pat_data.tuple);
        if (items.len < 2) return false;
        var used: usize = 0;
        for (0..items.len) |index| {
            const item_data = self.program.getPat(GuardedList.at(items, index)).data;
            if (item_data != .bind) return false;
            const local = item_data.bind;
            var count = localUseCountInExpr(self.program, local, final_expr);
            for (tail_start..statements.len) |stmt_index| {
                count += localUseCountInStmt(self.program, local, GuardedList.at(statements, stmt_index));
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
                .uninitialized, .expect, .dbg, .return_, .crash => continue,
            }
            if (loop_stmt_index != null) break;
        }
        const li = loop_stmt_index orelse return null;

        const loop_parts = (try self.matchIteratorLoopParts(loop_expr_id)) orelse return null;
        if (localUseCountInExpr(self.program, loop_parts.source_local, body) != 1) return null;
        // A fold's result feeds the block's final expression directly, so the
        // transformed fold value can take its place.
        if (loop_parts.carry_count == 1) {
            const rl = result_local orelse return null;
            if (localExpr(self.program, block.final_expr) != rl) return null;
            if (localUseCountInExpr(self.program, rl, body) != 1) return null;
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
        const expr = self.program.getExpr(expr_id);
        if (expr.data == .continue_) return self.program.exprSpan(expr.data.continue_.values);
        if (expr.data == .block) return self.tailContinueValues(expr.data.block.final_expr);
        return null;
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
        const stripped = self.stripArmBlock(arm);
        if (localExpr(self.program, stripped)) |local| {
            return .{ .base = local, .items = try self.allocator.alloc(Ast.ExprId, 0) };
        }
        const call = self.asDirectCall(stripped) orelse return null;
        if (call.args.len != 2 or !self.callIsSuffixAppend(call)) return null;
        const item = GuardedList.at(call.args, 1);
        const inner = (try self.reduceArmChain(GuardedList.at(call.args, 0))) orelse return null;
        defer self.allocator.free(inner.items);
        const items = try self.allocator.alloc(Ast.ExprId, inner.items.len + 1);
        @memcpy(items[0..inner.items.len], inner.items);
        items[inner.items.len] = item;
        return .{ .base = inner.base, .items = items };
    }

    fn branchAppendPlanIsWorkFree(self: *Pass, branch_expr_id: Ast.ExprId) Common.LowerError!ProofStatus {
        var budget: u32 = 4096;
        const data = self.program.getExpr(branch_expr_id).data;
        if (data == .if_) {
            const if_ = data.if_;
            const branches = self.program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const condition = self.exprIsStructurallyWorkFree(branch.cond, &budget);
                if (condition != .proven) return condition;
                const body = try self.appendArmItemsAreWorkFree(branch.body, &budget);
                if (body != .proven) return body;
            }
            return try self.appendArmItemsAreWorkFree(if_.final_else, &budget);
        } else if (data == .match_) {
            const match = data.match_;
            const scrutinee = self.exprIsStructurallyWorkFree(match.scrutinee, &budget);
            if (scrutinee != .proven) return scrutinee;
            const branches = self.program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const binding_proof = self.stmtSpanIsStructurallyWorkFree(branch.bindings, &budget);
                if (binding_proof != .proven) return binding_proof;
                if (branch.guard) |guard| {
                    const guard_proof = self.exprIsStructurallyWorkFree(guard, &budget);
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
            const proof = self.exprIsStructurallyWorkFree(item, budget);
            if (proof != .proven) return proof;
        }
        return .proven;
    }

    /// A source expression whose evaluation is only finite structural assembly,
    /// record-field reads, or tag-payload reads. In particular, this excludes
    /// every call, low-level op, loop, allocation-bearing collection literal,
    /// control transfer, and diagnostic operation. Exhaustion declines the rewrite.
    fn exprIsStructurallyWorkFree(self: *Pass, expr_id: Ast.ExprId, budget: *u32) ProofStatus {
        if (budget.* == 0) return .unknown_budget_exhausted;
        budget.* -= 1;
        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            => .proven,
            .tuple => |items| self.exprSpanIsStructurallyWorkFree(items, budget),
            .record => |fields| blk: {
                const values = self.program.fieldExprSpan(fields);
                var proof = ProofStatus.proven;
                for (0..values.len) |index| {
                    proof = proofAnd(proof, self.exprIsStructurallyWorkFree(GuardedList.at(values, index).value, budget));
                    if (proof == .disproven) break;
                }
                break :blk proof;
            },
            .tag => |tag| self.exprSpanIsStructurallyWorkFree(tag.payloads, budget),
            .nominal => |backing| self.exprIsStructurallyWorkFree(backing, budget),
            .field_access => |field| self.exprIsStructurallyWorkFree(field.receiver, budget),
            .tuple_access => |access| self.exprIsStructurallyWorkFree(access.tuple, budget),
            .static_data_candidate => |candidate| self.exprIsStructurallyWorkFree(candidate.runtime_expr, budget),
            .block => |block| if (self.program.stmtSpan(block.statements).len == 0)
                self.exprIsStructurallyWorkFree(block.final_expr, budget)
            else
                .disproven,
            .comptime_branch_taken => |taken| self.exprIsStructurallyWorkFree(taken.body, budget),
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
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => .disproven,
        };
    }

    fn exprSpanIsStructurallyWorkFree(self: *Pass, span: Ast.Span(Ast.ExprId), budget: *u32) ProofStatus {
        const values = self.program.exprSpan(span);
        var proof = ProofStatus.proven;
        for (0..values.len) |index| {
            proof = proofAnd(proof, self.exprIsStructurallyWorkFree(GuardedList.at(values, index), budget));
            if (proof == .disproven) return .disproven;
        }
        return proof;
    }

    fn stmtSpanIsStructurallyWorkFree(self: *Pass, span: Ast.Span(Ast.StmtId), budget: *u32) ProofStatus {
        const statements = self.program.stmtSpan(span);
        var proof = ProofStatus.proven;
        for (0..statements.len) |index| {
            const stmt_proof = switch (self.program.getStmt(GuardedList.at(statements, index))) {
                .let_ => |let_| if (let_.recursive) .disproven else self.exprIsStructurallyWorkFree(let_.value, budget),
                .uninitialized => .proven,
                .expr, .expect, .dbg, .return_, .crash => .disproven,
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
        if (call.iterator_procedure != .iter_append) return false;
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
        if (localUseCountInExpr(self.program, loop_parts.rest_local, loop_parts.one_body) != 1) return null;

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
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .continue_ => |cont| {
                const values = self.program.exprSpan(cont.values);
                if (values.len != loop_parts.carry_count + 1) return null;
                if (loop_parts.carry_count == 0) {
                    return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .unit });
                }
                return try self.cloneExprFresh(GuardedList.at(values, 1), renames);
            },
            .block => |block| {
                const source = try GuardedList.dupe(self.allocator, Ast.StmtId, self.program.stmtSpan(block.statements));
                defer self.allocator.free(source);
                var stmts = std.ArrayList(Ast.StmtId).empty;
                defer stmts.deinit(self.allocator);
                for (source) |stmt_id| {
                    const cloned = (try self.cloneStmtFresh(stmt_id, renames)) orelse return null;
                    try stmts.append(self.allocator, cloned);
                }
                const final = (try self.cloneNewCarry(block.final_expr, renames, loop_parts)) orelse return null;
                return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .block = .{
                    .statements = try self.program.addStmtSpan(stmts.items),
                    .final_expr = final,
                } } });
            },
            .if_ => |if_| {
                const branches = try GuardedList.dupe(self.allocator, Ast.IfBranch, self.program.ifBranchSpan(if_.branches));
                defer self.allocator.free(branches);
                var rewritten = try self.allocator.alloc(Ast.IfBranch, branches.len);
                defer self.allocator.free(rewritten);
                for (branches, 0..) |br, index| {
                    const cond = (try self.cloneExprFresh(br.cond, renames)) orelse return null;
                    const arm = (try self.cloneNewCarry(br.body, renames, loop_parts)) orelse return null;
                    rewritten[index] = .{ .cond = cond, .body = arm };
                }
                const final_else = (try self.cloneNewCarry(if_.final_else, renames, loop_parts)) orelse return null;
                return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .if_ = .{
                    .branches = try self.program.addIfBranchSpan(rewritten),
                    .final_else = final_else,
                } } });
            },
            .match_ => |match| {
                const scrutinee = (try self.cloneExprFresh(match.scrutinee, renames)) orelse return null;
                const branches = try GuardedList.dupe(self.allocator, Ast.Branch, self.program.branchSpan(match.branches));
                defer self.allocator.free(branches);
                var rewritten = try self.allocator.alloc(Ast.Branch, branches.len);
                defer self.allocator.free(rewritten);
                for (branches, 0..) |br, index| {
                    if (br.guard != null or br.bindings.len != 0) return null;
                    const pat = (try self.clonePatFresh(br.pat, renames)) orelse return null;
                    const arm = (try self.cloneNewCarry(br.body, renames, loop_parts)) orelse return null;
                    rewritten[index] = .{ .pat = pat, .guard = null, .body = arm };
                }
                return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .match_ = .{
                    .scrutinee = scrutinee,
                    .branches = try self.program.addBranchSpan(rewritten),
                    .comptime_site = match.comptime_site,
                } } });
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
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => return try self.cloneExprFresh(expr_id, renames),
        }
    }

    fn cloneStmtFresh(self: *Pass, stmt_id: Ast.StmtId, renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.StmtId {
        switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| {
                const value = (try self.cloneExprFresh(let_.value, renames)) orelse return null;
                const pat = (try self.clonePatFresh(let_.pat, renames)) orelse return null;
                return try self.program.addStmt(.{ .let_ = .{
                    .pat = pat,
                    .value = value,
                    .recursive = let_.recursive,
                    .comptime_site = let_.comptime_site,
                } });
            },
            .expr => |e| {
                const cloned = (try self.cloneExprFresh(e, renames)) orelse return null;
                return try self.program.addStmt(.{ .expr = cloned });
            },
            .uninitialized, .expect, .dbg, .return_, .crash => return null,
        }
    }

    /// Deep-clone a pure-computation expression, applying local renames and
    /// allocating fresh locals at binding sites. Returns null for constructs
    /// outside the foldable set.
    fn cloneExprFresh(self: *Pass, expr_id: Ast.ExprId, renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.ExprId {
        const expr = self.program.getExpr(expr_id);
        const data: Ast.ExprData = switch (expr.data) {
            .local => |local| .{ .local = renames.get(local) orelse local },
            .unit => .unit,
            .int_lit => |v| .{ .int_lit = v },
            .frac_f32_lit => |v| .{ .frac_f32_lit = v },
            .frac_f64_lit => |v| .{ .frac_f64_lit = v },
            .dec_lit => |v| .{ .dec_lit = v },
            .str_lit => |v| .{ .str_lit = v },
            .bytes_lit => |v| .{ .bytes_lit = v },
            .crash => |v| .{ .crash = v },
            .list => |items| .{ .list = (try self.cloneExprSpanFresh(items, renames)) orelse return null },
            .tuple => |items| .{ .tuple = (try self.cloneExprSpanFresh(items, renames)) orelse return null },
            .record => |fields| .{ .record = (try self.cloneFieldSpanFresh(fields, renames)) orelse return null },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = (try self.cloneExprSpanFresh(tag.payloads, renames)) orelse return null,
            } },
            .static_data_candidate => |candidate| .{ .static_data_candidate = .{
                .static_data = candidate.static_data,
                .runtime_expr = (try self.cloneExprFresh(candidate.runtime_expr, renames)) orelse return null,
            } },
            .nominal => |backing| .{ .nominal = (try self.cloneExprFresh(backing, renames)) orelse return null },
            .fn_ref => |fn_ref| .{ .fn_ref = .{
                .fn_id = fn_ref.fn_id,
                .captures = (try self.cloneCaptureOperandSpanFresh(fn_ref.captures, renames)) orelse return null,
            } },
            .field_access => |field| .{ .field_access = .{
                .receiver = (try self.cloneExprFresh(field.receiver, renames)) orelse return null,
                .segments = field.segments,
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = (try self.cloneExprFresh(access.tuple, renames)) orelse return null,
                .elem_index = access.elem_index,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = (try self.cloneExprFresh(eq.lhs, renames)) orelse return null,
                .rhs = (try self.cloneExprFresh(eq.rhs, renames)) orelse return null,
                .negated = eq.negated,
            } },
            .structural_hash => |h| .{ .structural_hash = .{
                .value = (try self.cloneExprFresh(h.value, renames)) orelse return null,
                .hasher = (try self.cloneExprFresh(h.hasher, renames)) orelse return null,
            } },
            .low_level => |call| .{ .low_level = .{
                .op = call.op,
                .args = (try self.cloneExprSpanFresh(call.args, renames)) orelse return null,
            } },
            .call_proc => |call| .{ .call_proc = .{
                .callee = call.callee,
                .args = (try self.cloneExprSpanFresh(call.args, renames)) orelse return null,
                .iterator_procedure = call.iterator_procedure,
                .captures = (try self.cloneCaptureOperandSpanFresh(call.captures, renames)) orelse return null,
                .is_cold = call.is_cold,
            } },
            .call_value => |call| .{ .call_value = .{
                .callee = (try self.cloneExprFresh(call.callee, renames)) orelse return null,
                .args = (try self.cloneExprSpanFresh(call.args, renames)) orelse return null,
            } },
            .let_ => |let_| blk: {
                const value = (try self.cloneExprFresh(let_.value, renames)) orelse return null;
                const pat = (try self.clonePatFresh(let_.bind, renames)) orelse return null;
                const rest = (try self.cloneExprFresh(let_.rest, renames)) orelse return null;
                break :blk .{ .let_ = .{
                    .bind = pat,
                    .value = value,
                    .rest = rest,
                    .comptime_site = let_.comptime_site,
                } };
            },
            .block => |block| blk: {
                const source = try GuardedList.dupe(self.allocator, Ast.StmtId, self.program.stmtSpan(block.statements));
                defer self.allocator.free(source);
                var stmts = std.ArrayList(Ast.StmtId).empty;
                defer stmts.deinit(self.allocator);
                for (source) |stmt_id| {
                    const cloned = (try self.cloneStmtFresh(stmt_id, renames)) orelse return null;
                    try stmts.append(self.allocator, cloned);
                }
                const final = (try self.cloneExprFresh(block.final_expr, renames)) orelse return null;
                break :blk .{ .block = .{
                    .statements = try self.program.addStmtSpan(stmts.items),
                    .final_expr = final,
                } };
            },
            .if_ => |if_| blk: {
                const branches = try GuardedList.dupe(self.allocator, Ast.IfBranch, self.program.ifBranchSpan(if_.branches));
                defer self.allocator.free(branches);
                var rewritten = try self.allocator.alloc(Ast.IfBranch, branches.len);
                defer self.allocator.free(rewritten);
                for (branches, 0..) |br, index| {
                    const cond = (try self.cloneExprFresh(br.cond, renames)) orelse return null;
                    const arm = (try self.cloneExprFresh(br.body, renames)) orelse return null;
                    rewritten[index] = .{ .cond = cond, .body = arm };
                }
                const final_else = (try self.cloneExprFresh(if_.final_else, renames)) orelse return null;
                break :blk .{ .if_ = .{
                    .branches = try self.program.addIfBranchSpan(rewritten),
                    .final_else = final_else,
                } };
            },
            .match_ => |match| blk: {
                const scrutinee = (try self.cloneExprFresh(match.scrutinee, renames)) orelse return null;
                const branches = try GuardedList.dupe(self.allocator, Ast.Branch, self.program.branchSpan(match.branches));
                defer self.allocator.free(branches);
                var rewritten = try self.allocator.alloc(Ast.Branch, branches.len);
                defer self.allocator.free(rewritten);
                for (branches, 0..) |br, index| {
                    if (br.guard != null or br.bindings.len != 0) return null;
                    const pat = (try self.clonePatFresh(br.pat, renames)) orelse return null;
                    const arm = (try self.cloneExprFresh(br.body, renames)) orelse return null;
                    rewritten[index] = .{ .pat = pat, .guard = null, .body = arm };
                }
                break :blk .{ .match_ = .{
                    .scrutinee = scrutinee,
                    .branches = try self.program.addBranchSpan(rewritten),
                    .comptime_site = match.comptime_site,
                } };
            },
            // An early return exits the enclosing function; it is preserved
            // verbatim in the peeled tail, where it fires only after the base
            // iteration completes without returning—the same order the
            // unfused loop would return in.
            .return_ => |ret| .{ .return_ = .{
                .value = (try self.cloneExprFresh(ret.value, renames)) orelse return null,
                .target = ret.target,
            } },
            .continue_ => |continue_| .{ .continue_ = .{
                .values = (try self.cloneExprSpanFresh(continue_.values, renames)) orelse return null,
            } },
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
            .expect,
            => return null,
        };
        const ty = if (expr.data == .local)
            if (renames.get(expr.data.local)) |renamed|
                self.program.getLocal(renamed).ty
            else
                expr.ty
        else
            expr.ty;
        return try self.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn cloneExprSpanFresh(self: *Pass, span: Ast.Span(Ast.ExprId), renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.ExprId) {
        const source = try GuardedList.dupe(self.allocator, Ast.ExprId, self.program.exprSpan(span));
        defer self.allocator.free(source);
        var out = try self.allocator.alloc(Ast.ExprId, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |item, index| {
            out[index] = (try self.cloneExprFresh(item, renames)) orelse return null;
        }
        return try self.program.addExprSpan(out);
    }

    fn cloneCaptureOperandSpanFresh(self: *Pass, span: Ast.Span(Ast.CaptureOperand), renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.CaptureOperand) {
        const source = try GuardedList.dupe(self.allocator, Ast.CaptureOperand, self.program.captureOperandSpan(span));
        defer self.allocator.free(source);
        var out = try self.allocator.alloc(Ast.CaptureOperand, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |operand, index| {
            out[index] = .{
                .id = operand.id,
                .value = (try self.cloneExprFresh(operand.value, renames)) orelse return null,
            };
        }
        return try self.program.addCaptureOperandSpan(out);
    }

    fn cloneFieldSpanFresh(self: *Pass, span: Ast.Span(Ast.FieldExpr), renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.FieldExpr) {
        const source = try GuardedList.dupe(self.allocator, Ast.FieldExpr, self.program.fieldExprSpan(span));
        defer self.allocator.free(source);
        var out = try self.allocator.alloc(Ast.FieldExpr, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |field, index| {
            out[index] = .{
                .name = field.name,
                .value = (try self.cloneExprFresh(field.value, renames)) orelse return null,
            };
        }
        return try self.program.addFieldExprSpan(out);
    }

    /// Clone a pattern, allocating a fresh local for every binding site and
    /// recording the rename. Returns null for list/string patterns, which the
    /// fold does not replay.
    fn clonePatFresh(self: *Pass, pat_id: Ast.PatId, renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.PatId {
        const pat = self.program.getPat(pat_id);
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| blk: {
                const fresh = try self.program.addLocal(self.symbols.fresh(), pat.ty);
                try renames.put(local, fresh);
                break :blk .{ .bind = fresh };
            },
            .wildcard => .wildcard,
            .int_lit => |v| .{ .int_lit = v },
            .dec_lit => |v| .{ .dec_lit = v },
            .frac_f32_lit => |v| .{ .frac_f32_lit = v },
            .frac_f64_lit => |v| .{ .frac_f64_lit = v },
            .str_lit => |v| .{ .str_lit = v },
            .as => |as| blk: {
                const inner = (try self.clonePatFresh(as.pattern, renames)) orelse return null;
                const fresh = try self.program.addLocal(self.symbols.fresh(), pat.ty);
                try renames.put(as.local, fresh);
                break :blk .{ .as = .{ .pattern = inner, .local = fresh } };
            },
            .record => |fields_span| blk: {
                const fields = try GuardedList.dupe(self.allocator, Ast.RecordDestruct, self.program.recordDestructSpan(fields_span));
                defer self.allocator.free(fields);
                var out = try self.allocator.alloc(Ast.RecordDestruct, fields.len);
                defer self.allocator.free(out);
                for (fields, 0..) |field, index| {
                    out[index] = .{
                        .name = field.name,
                        .pattern = (try self.clonePatFresh(field.pattern, renames)) orelse return null,
                    };
                }
                break :blk .{ .record = try self.program.addRecordDestructSpan(out) };
            },
            .tuple => |items_span| blk: {
                const cloned = (try self.clonePatSpanFresh(items_span, renames)) orelse return null;
                break :blk .{ .tuple = cloned };
            },
            .tag => |tag| blk: {
                const cloned = (try self.clonePatSpanFresh(tag.payloads, renames)) orelse return null;
                break :blk .{ .tag = .{ .name = tag.name, .payloads = cloned } };
            },
            .nominal => |backing| .{ .nominal = (try self.clonePatFresh(backing, renames)) orelse return null },
            .list, .str_pattern => return null,
        };
        return try self.program.addPat(.{ .ty = pat.ty, .data = data });
    }

    fn clonePatSpanFresh(self: *Pass, span: Ast.Span(Ast.PatId), renames: *collections.DenseMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.PatId) {
        const source = try GuardedList.dupe(self.allocator, Ast.PatId, self.program.patSpan(span));
        defer self.allocator.free(source);
        var out = try self.allocator.alloc(Ast.PatId, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |child, index| {
            out[index] = (try self.clonePatFresh(child, renames)) orelse return null;
        }
        return try self.program.addPatSpan(out);
    }

    /// Normalize a whole function body once through the value-aware cloner.
    /// Structural consumers expose exact producer calls locally, call-pattern
    /// rewrites consume the resulting values, and loop fixed points scalarize
    /// known carried structure without a body-category routing decision.
    fn cloneFnBodyInPlace(self: *Pass, fn_id: Ast.FnId, body: Ast.ExprId) Common.LowerError!void {
        const fn_index = @intFromEnum(fn_id);
        if (fn_index < self.whole_body_cloned.len and self.whole_body_cloned[fn_index]) return;

        const fn_ = self.program.getFn(fn_id);
        var cloner = Cloner.initForRewrite(self);
        defer cloner.deinit();
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
        const cloned = try cloner.cloneExpr(body);
        self.program.setFn(fn_id, .{
            .symbol = fn_.symbol,
            .source = fn_.source,
            .signature = fn_.signature,
            .args = fn_.args,
            .captures = fn_.captures,
            .body = .{ .roc = cloned },
            .ret = fn_.ret,
        });
        if (fn_index < self.whole_body_cloned.len) self.whole_body_cloned[fn_index] = true;
    }

    /// Once the specialization graph is complete, clone only functions whose
    /// loop result ABI contains fields their exact continuation cannot observe.
    /// Calls and loop-carried state stay unchanged in this final pass; its sole
    /// authority is the producer-visible result binding and continuation.
    fn projectUnusedLoopResults(self: *Pass) Common.LowerError!void {
        const fn_count = self.program.fnCount();
        for (0..fn_count) |index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            const fn_ = self.program.getFn(fn_id);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const tuple_projectable = self.bodyHasProjectableLoopResult(body);
            const aggregate_projectable = try self.bodyHasAggregateProjectableLoopResult(body);
            if (!tuple_projectable and !aggregate_projectable) continue;

            var cloner = Cloner.initForRewrite(self);
            defer cloner.deinit();
            cloner.inline_calls = .none;
            cloner.rewrite_call_patterns = false;
            cloner.emit_callable_workers = false;
            const cloned = try cloner.cloneExpr(body);
            self.program.setFn(fn_id, .{
                .symbol = fn_.symbol,
                .source = fn_.source,
                .signature = fn_.signature,
                .args = fn_.args,
                .captures = fn_.captures,
                .body = .{ .roc = cloned },
                .ret = fn_.ret,
            });
        }
    }

    /// Collect outermost loops with an explicitly known constructor in their
    /// initial carried state. A nested loop is left to the clone of its
    /// enclosing loop, and a plain scalar counting loop does not qualify.
    fn rewriteCallsInExpr(self: *Pass, expr_id: Ast.ExprId, done: []bool) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (done[index]) return;
        done[index] = true;

        const expr = self.program.getExprAt(index);
        switch (expr.data) {
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
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            => {},
            .fn_ref => |fn_ref| try self.rewriteCallsInCaptureOperandSpan(fn_ref.captures, done),
            .list,
            .tuple,
            => |items| try self.rewriteCallsInExprSpan(items, done),
            .record => |fields| try self.rewriteCallsInFieldExprSpan(fields, done),
            .record_update => |update| {
                try self.rewriteCallsInExpr(update.base, done);
                try self.rewriteCallsInFieldExprSpan(update.fields, done);
            },
            .tag => |tag| try self.rewriteCallsInExprSpan(tag.payloads, done),
            .static_data_candidate => |candidate| try self.rewriteCallsInExpr(candidate.runtime_expr, done),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.rewriteCallsInExpr(child, done),
            .return_ => |ret| try self.rewriteCallsInExpr(ret.value, done),
            .expect_err => |expect_err| try self.rewriteCallsInExpr(expect_err.msg, done),
            .comptime_branch_taken => |taken| try self.rewriteCallsInExpr(taken.body, done),
            .let_ => |let_| {
                try self.rewriteCallsInExpr(let_.value, done);
                try self.rewriteCallsInExpr(let_.rest, done);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .call_value => |call| {
                try self.rewriteCallsInExpr(call.callee, done);
                try self.rewriteCallsInExprSpan(call.args, done);
            },
            .call_proc => |call| {
                try self.rewriteCallsInExprSpan(call.args, done);
                try self.rewriteCallsInCaptureOperandSpan(call.captures, done);
                try self.rewriteCallProc(expr_id, call);
            },
            .low_level => |call| try self.rewriteCallsInExprSpan(call.args, done),
            .field_access => |field| try self.rewriteCallsInExpr(field.receiver, done),
            .tuple_access => |access| try self.rewriteCallsInExpr(access.tuple, done),
            .structural_eq => |eq| {
                try self.rewriteCallsInExpr(eq.lhs, done);
                try self.rewriteCallsInExpr(eq.rhs, done);
            },
            .structural_hash => |h| {
                try self.rewriteCallsInExpr(h.value, done);
                try self.rewriteCallsInExpr(h.hasher, done);
            },
            .match_ => |match| {
                try self.rewriteCallsInExpr(match.scrutinee, done);
                try self.rewriteCallsInBranchSpan(match.branches, done);
            },
            .if_ => |if_| {
                try self.rewriteCallsInIfBranchSpan(if_.branches, done);
                try self.rewriteCallsInExpr(if_.final_else, done);
            },
            .block => |block| {
                try self.rewriteCallsInStmtSpan(block.statements, done);
                try self.rewriteCallsInExpr(block.final_expr, done);
            },
            .loop_ => |loop| {
                try self.rewriteCallsInExprSpan(loop.initial_values, done);
                try self.rewriteCallsInExpr(loop.body, done);
            },
            .break_ => |maybe| if (maybe) |value| try self.rewriteCallsInExpr(value, done),
            .continue_ => |continue_| try self.rewriteCallsInExprSpan(continue_.values, done),
            .join_point => |join_point| {
                try self.rewriteCallsInExpr(join_point.body, done);
                try self.rewriteCallsInExpr(join_point.remainder, done);
            },
            .jump => |jump| try self.rewriteCallsInExprSpan(jump.args, done),
            .if_initialized_payload => |payload_switch| {
                try self.rewriteCallsInExpr(payload_switch.cond, done);
                try self.rewriteCallsInExpr(payload_switch.initialized, done);
                try self.rewriteCallsInExpr(payload_switch.uninitialized, done);
            },
            .try_sequence => |sequence| {
                try self.rewriteCallsInExpr(sequence.try_expr, done);
                try self.rewriteCallsInExpr(sequence.ok_body, done);
            },
            .try_record_sequence => |sequence| {
                try self.rewriteCallsInExpr(sequence.try_expr, done);
                try self.rewriteCallsInExpr(sequence.ok_body, done);
            },
        }
    }

    fn rewriteCallsInExprSpan(self: *Pass, span: Ast.Span(Ast.ExprId), done: []bool) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.ExprId, self.program.exprSpan(span), .{ .self = self, .done = done }, struct {
            fn visit(ctx: anytype, expr: Ast.ExprId) Allocator.Error!void {
                try ctx.self.rewriteCallsInExpr(expr, ctx.done);
            }
        }.visit);
    }

    fn rewriteCallsInCaptureOperandSpan(self: *Pass, span: Ast.Span(Ast.CaptureOperand), done: []bool) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.CaptureOperand, self.program.captureOperandSpan(span), .{ .self = self, .done = done }, struct {
            fn visit(ctx: anytype, operand: Ast.CaptureOperand) Allocator.Error!void {
                try ctx.self.rewriteCallsInExpr(operand.value, ctx.done);
            }
        }.visit);
    }

    fn rewriteCallsInFieldExprSpan(self: *Pass, span: Ast.Span(Ast.FieldExpr), done: []bool) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.FieldExpr, self.program.fieldExprSpan(span), .{ .self = self, .done = done }, struct {
            fn visit(ctx: anytype, field: Ast.FieldExpr) Allocator.Error!void {
                try ctx.self.rewriteCallsInExpr(field.value, ctx.done);
            }
        }.visit);
    }

    fn rewriteCallsInBranchSpan(self: *Pass, span: Ast.Span(Ast.Branch), done: []bool) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.Branch, self.program.branchSpan(span), .{ .self = self, .done = done }, struct {
            fn visit(ctx: anytype, branch: Ast.Branch) Allocator.Error!void {
                try ctx.self.rewriteCallsInStmtSpan(branch.bindings, ctx.done);
                if (branch.guard) |guard| try ctx.self.rewriteCallsInExpr(guard, ctx.done);
                try ctx.self.rewriteCallsInExpr(branch.body, ctx.done);
            }
        }.visit);
    }

    fn rewriteCallsInIfBranchSpan(self: *Pass, span: Ast.Span(Ast.IfBranch), done: []bool) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.IfBranch, self.program.ifBranchSpan(span), .{ .self = self, .done = done }, struct {
            fn visit(ctx: anytype, branch: Ast.IfBranch) Allocator.Error!void {
                try ctx.self.rewriteCallsInExpr(branch.cond, ctx.done);
                try ctx.self.rewriteCallsInExpr(branch.body, ctx.done);
            }
        }.visit);
    }

    fn rewriteCallsInStmtSpan(self: *Pass, span: Ast.Span(Ast.StmtId), done: []bool) Allocator.Error!void {
        try walkSpanCloned(self.allocator, Ast.StmtId, self.program.stmtSpan(span), .{ .self = self, .done = done }, struct {
            fn visit(ctx: anytype, stmt: Ast.StmtId) Allocator.Error!void {
                try ctx.self.rewriteCallsInStmt(stmt, ctx.done);
            }
        }.visit);
    }

    fn rewriteCallsInStmt(self: *Pass, stmt_id: Ast.StmtId, done: []bool) Allocator.Error!void {
        switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| try self.rewriteCallsInExpr(let_.value, done),
            .expr,
            .expect,
            .dbg,
            => |expr| try self.rewriteCallsInExpr(expr, done),
            .return_ => |ret| try self.rewriteCallsInExpr(ret.value, done),
            .uninitialized, .crash => {},
        }
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
            if (!shapeMatchesValue(self.program, shape, cloned.value)) return false;
            try cloner.appendExprsFromValue(shape, cloned.value, out);
        }
        matched = true;
        return true;
    }

    fn appendExistingExprsForShape(
        self: *Pass,
        shape: Shape,
        expr_id: Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!bool {
        switch (shape) {
            .any => {
                try out.append(self.allocator, expr_id);
                return true;
            },
            .tag => |tag| {
                const expr = self.program.getExpr(expr_id);
                const expr_tag = switch (expr.data) {
                    .tag => |expr_tag| expr_tag,
                    _ => return false,
                };
                if (!sameType(self.program, expr.ty, tag.ty) or !self.program.names.tagLabelTextEql(expr_tag.name, tag.name)) return false;
                const payloads = self.program.exprSpan(expr_tag.payloads);
                if (payloads.len != tag.payloads.len) Common.invariant("tag call pattern arity differed from tag expression arity");
                for (tag.payloads, payloads) |payload_shape, payload| {
                    if (!try self.appendExistingExprsForShape(payload_shape, payload, out)) return false;
                }
                return true;
            },
            .record => |record| {
                const expr = self.program.getExpr(expr_id);
                const fields = switch (expr.data) {
                    .record => |fields| self.program.fieldExprSpan(fields),
                    _ => return false,
                };
                if (!sameType(self.program, expr.ty, record.ty) or fields.len != record.fields.len) return false;
                for (record.fields, fields) |field_shape, field| {
                    if (!self.program.names.recordFieldLabelTextEql(field_shape.name, field.name)) return false;
                    if (!try self.appendExistingExprsForShape(field_shape.shape, field.value, out)) return false;
                }
                return true;
            },
            .tuple => |tuple| {
                const expr = self.program.getExpr(expr_id);
                const items = switch (expr.data) {
                    .tuple => |items| self.program.exprSpan(items),
                    _ => return false,
                };
                if (!sameType(self.program, expr.ty, tuple.ty) or items.len != tuple.items.len) return false;
                for (tuple.items, items) |item_shape, item| {
                    if (!try self.appendExistingExprsForShape(item_shape, item, out)) return false;
                }
                return true;
            },
            .nominal => |nominal| {
                const expr = self.program.getExpr(expr_id);
                const backing = switch (expr.data) {
                    .nominal => |backing| backing,
                    _ => return false,
                };
                if (!sameType(self.program, expr.ty, nominal.ty)) return false;
                return try self.appendExistingExprsForShape(nominal.backing.*, backing, out);
            },
            .callable => return false,
        }
    }

    fn constructorShape(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!?Shape {
        const expr = self.program.getExpr(expr_id);
        if (expr.data == .tag or expr.data == .record or expr.data == .tuple) assertStructuralConstructionType(self.program, expr.ty);
        return switch (expr.data) {
            .tag => |tag| blk: {
                const payloads = self.program.exprSpan(tag.payloads);
                const shapes = try self.arena.allocator().alloc(Shape, payloads.len);
                for (0..payloads.len) |index| {
                    const payload = GuardedList.at(payloads, index);
                    shapes[index] = (try self.constructorShape(payload)) orelse
                        .{ .any = self.program.getExpr(payload).ty };
                }
                break :blk Shape{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = shapes,
                } };
            },
            .record => |fields_span| blk: {
                const fields = self.program.fieldExprSpan(fields_span);
                const shapes = try self.arena.allocator().alloc(FieldShape, fields.len);
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    shapes[index] = .{
                        .name = field.name,
                        .shape = (try self.constructorShape(field.value)) orelse
                            .{ .any = self.program.getExpr(field.value).ty },
                    };
                }
                break :blk Shape{ .record = .{
                    .ty = expr.ty,
                    .fields = shapes,
                } };
            },
            .record_update => |update| blk: {
                const record_ty = recordUpdateBackingType(self.program, expr.ty);
                const type_fields = self.program.types.fieldSpan(recordUpdateFieldSpan(self.program, expr.ty));
                const update_fields = self.program.fieldExprSpan(update.fields);
                const shapes = try self.arena.allocator().alloc(FieldShape, type_fields.len);
                for (0..type_fields.len) |index| {
                    const type_field = GuardedList.at(type_fields, index);
                    const updated = for (0..update_fields.len) |update_index| {
                        const field = GuardedList.at(update_fields, update_index);
                        if (self.program.names.recordFieldLabelTextEql(type_field.name, field.name)) break field.value;
                    } else null;
                    shapes[index] = .{
                        .name = type_field.name,
                        .shape = if (updated) |value|
                            (try self.constructorShape(value)) orelse .{ .any = type_field.ty }
                        else
                            .{ .any = type_field.ty },
                    };
                }
                const record_shape = Shape{ .record = .{
                    .ty = record_ty,
                    .fields = shapes,
                } };
                if (nominalConstructionLayer(self.program, expr.ty) != null) {
                    const backing = try self.arena.allocator().create(Shape);
                    backing.* = record_shape;
                    break :blk Shape{ .nominal = .{
                        .ty = expr.ty,
                        .backing = backing,
                    } };
                }
                break :blk record_shape;
            },
            .tuple => |items_span| blk: {
                const items = self.program.exprSpan(items_span);
                const shapes = try self.arena.allocator().alloc(Shape, items.len);
                for (0..items.len) |index| {
                    const item = GuardedList.at(items, index);
                    shapes[index] = (try self.constructorShape(item)) orelse
                        .{ .any = self.program.getExpr(item).ty };
                }
                break :blk Shape{ .tuple = .{
                    .ty = expr.ty,
                    .items = shapes,
                } };
            },
            .nominal => |backing| blk: {
                const backing_shape = (try self.constructorShape(backing)) orelse break :blk null;
                const stored = try self.arena.allocator().create(Shape);
                stored.* = backing_shape;
                break :blk Shape{ .nominal = .{
                    .ty = expr.ty,
                    .backing = stored,
                } };
            },
            .fn_ref => |fn_ref| blk: {
                const capture_operands = self.program.captureOperandSpan(fn_ref.captures);
                const capture_shapes = try self.arena.allocator().alloc(Shape, capture_operands.len);
                for (0..capture_operands.len) |index| {
                    const operand = GuardedList.at(capture_operands, index);
                    capture_shapes[index] = (try self.constructorShape(operand.value)) orelse
                        .{ .any = self.program.getExpr(operand.value).ty };
                }
                break :blk Shape{ .callable = .{
                    .ty = expr.ty,
                    .fn_id = fn_ref.fn_id,
                    .captures = capture_shapes,
                } };
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
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => null,
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

    fn shapeFromValue(self: *Pass, value: Value) Allocator.Error!ShapeProof {
        var budget: u32 = shape_work_budget;
        return try self.shapeFromValueBudgeted(value, &budget);
    }

    fn shapeFromValueBudgeted(self: *Pass, value: Value, budget: *u32) Allocator.Error!ShapeProof {
        if (budget.* == 0) return .unknown_budget_exhausted;
        budget.* -= 1;
        return switch (value) {
            .expr => |expr| if (try self.constructorShape(expr)) |shape| .{ .proven = shape } else .disproven,
            .static_data_candidate => |candidate| try self.shapeFromValueBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                const payloads = try self.arena.allocator().alloc(Shape, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = switch (try self.shapeFromValueBudgeted(payload, budget)) {
                        .proven => |shape| shape,
                        .disproven, .unknown_budget_exhausted => .{ .any = valueType(self.program, payload) },
                    };
                }
                break :blk ShapeProof{ .proven = .{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } } };
            },
            .record => |record| blk: {
                const fields = try self.arena.allocator().alloc(FieldShape, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .shape = switch (try self.shapeFromValueBudgeted(field.value, budget)) {
                            .proven => |shape| shape,
                            .disproven, .unknown_budget_exhausted => .{ .any = valueType(self.program, field.value) },
                        },
                    };
                }
                break :blk ShapeProof{ .proven = .{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } } };
            },
            .tuple => |tuple| blk: {
                const items = try self.arena.allocator().alloc(Shape, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = switch (try self.shapeFromValueBudgeted(item, budget)) {
                        .proven => |shape| shape,
                        .disproven, .unknown_budget_exhausted => .{ .any = valueType(self.program, item) },
                    };
                }
                break :blk ShapeProof{ .proven = .{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } } };
            },
            .nominal => |nominal| blk: {
                const backing_shape = switch (try self.shapeFromValueBudgeted(nominal.backing.*, budget)) {
                    .proven => |shape| shape,
                    .disproven => break :blk .disproven,
                    .unknown_budget_exhausted => break :blk .unknown_budget_exhausted,
                };
                const stored = try self.arena.allocator().create(Shape);
                stored.* = backing_shape;
                break :blk ShapeProof{ .proven = .{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } } };
            },
            .callable => |callable| blk: {
                const captures = try self.arena.allocator().alloc(Shape, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = switch (try self.shapeFromValueBudgeted(capture.value, budget)) {
                        .proven => |shape| shape,
                        .disproven, .unknown_budget_exhausted => .{ .any = valueType(self.program, capture.value) },
                    };
                }
                break :blk ShapeProof{ .proven = .{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } } };
            },
        };
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
    /// binder. `cloneLetValue` floats any update to a carried binder past its
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
    /// distinct bindings and must not read one another's substitution.
    fn binderIdentityOf(program: *const Ast.Program, local: Ast.LocalId) ?BinderIdentity {
        const local_data = program.getLocal(local);
        const binder = local_data.binder orelse return null;
        return .{
            .binder = binder,
            .digest = program.types.typeDigest(&program.names, local_data.ty),
        };
    }

    /// Resolve a local to its known value through the exact-local map, then the
    /// binder-wide map.
    fn get(self: *const Subst, program: *const Ast.Program, local: Ast.LocalId) ?Value {
        if (self.exact.get(local)) |value| return value;
        if (binderIdentityOf(program, local)) |identity| {
            if (self.binder_subst.get(identity)) |value| return value;
        }
        return null;
    }

    /// Resolve a local for emitted code, including the active value of a
    /// binder-equivalent Monotype local id.
    fn getForClone(self: *const Subst, program: *const Ast.Program, local: Ast.LocalId) ?Value {
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

    fn put(self: *Subst, program: *const Ast.Program, local: Ast.LocalId, value: Value) Allocator.Error!void {
        const previous = self.exact.get(local);
        try self.changes.append(self.allocator, .{
            .key = .{ .local = local },
            .previous = previous,
        });
        try self.exact.put(local, value);

        const identity = binderIdentityOf(program, local) orelse return;
        try self.putAlias(identity, value);
        const subst_binder = self.isLoopCarried(identity) or switch (value) {
            .tag,
            .record,
            .tuple,
            .nominal,
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

    fn putAlias(self: *Subst, identity: BinderIdentity, value: Value) Allocator.Error!void {
        const previous = self.binder_aliases.get(identity);
        try self.changes.append(self.allocator, .{
            .key = .{ .alias = identity },
            .previous = previous,
        });
        try self.binder_aliases.put(identity, value);
    }

    fn putLocalAlias(self: *Subst, program: *const Ast.Program, local: Ast.LocalId, value: Value) Allocator.Error!void {
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
    fn dropCarriedBinder(self: *Subst, program: *const Ast.Program, initial: Ast.ExprId) Allocator.Error!?BinderIdentity {
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
    /// Exact provenance for break nodes already rewritten to a selected loop
    /// result. Normalization can re-clone output nodes while the owning loop
    /// selection remains active; propagating this stamp makes that clone
    /// idempotent without inferring provenance from expression shape.
    selected_loop_exit_tys: collections.DenseMap(Ast.ExprId, Type.TypeId),
    join_stack: std.ArrayList(ActiveJoinClone),
    /// Remaining arms the shape-preserving let-of-case rewrite may still
    /// process. That rewrite re-clones each arm's body against the small
    /// dispatch, and a re-cloned arm can contain further let-of-case values,
    /// so unbounded application compounds on recursively generated code
    /// (derived parsers) until the compiler overflows its stack. When the
    /// budget runs out the rewrite retains the plain shared join, which never
    /// re-clones arm bodies.
    let_case_shape_growth: CodeGrowthBudget,
    /// Active let-of-case join rewrites, innermost last. Cloning a jump whose
    /// target belongs to one of these frames records the jump site's symbolic
    /// argument values for later parameter decomposition instead of cloning
    /// the argument expressions directly.
    let_case_builds: std.ArrayList(*LetCaseBuild),
    /// Fresh output locals bound by the recursive let statement whose value is
    /// currently cloning. A callable worker created while filling such a value
    /// must capture the recursive slot itself, not a field projected from it,
    /// so construction does not read the still-zeroed recursive payload.
    active_recursive_value_locals: collections.DenseMap(Ast.LocalId, void),
    rebased_inline_scopes: std.AutoHashMap(InlineScopeRebasePair, Ast.InlineScopeId),
    inline_scope_origins: collections.DenseMap(Ast.InlineScopeId, Ast.InlineScopeId),
    /// Depth of the wrapper-strip recursion in the static value matchers
    /// (`bindPatToValue`/`bindPatToMatchValue`/`bindPatToFlowValue`), counting
    /// each `nominal.backing`/`static_data_candidate.runtime` pointer edge
    /// followed. A loop-carried value can reference itself through those edges,
    /// so an unbounded strip would hang; reaching `value_wrapper_strip_cap`
    /// declines the static decision toward a residual runtime match.
    wrapper_strip_depth: usize,
    /// Depth of the wrapper-strip recursion in `materialize`, counting each
    /// `nominal.backing`/`static_data_candidate.runtime`/callable-capture edge
    /// followed. `materialize` runs on values proven acyclic by construction—
    /// a cyclic value is rebound through a plain source clone before it can
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
    /// branch body spends one unit before it is cloned, so nested distribution
    /// cannot multiply the expression store or recurse without spending this
    /// total growth budget.
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

    fn init(pass: *Pass, source_fn: Ast.FnId, pattern: CallPattern) Cloner {
        return .{
            .pass = pass,
            .arena = std.heap.ArenaAllocator.init(pass.allocator),
            .source_fn = source_fn,
            .pattern = pattern,
            .subst = Subst.init(pass.allocator),
            .inline_stack = .empty,
            .loop_stack = .empty,
            .loop_exit_stack = .empty,
            .selected_loop_exit_tys = collections.DenseMap(Ast.ExprId, Type.TypeId).init(pass.allocator),
            .join_stack = .empty,
            .let_case_shape_growth = .init(let_case_shape_arm_budget),
            .let_case_builds = .empty,
            .active_recursive_value_locals = collections.DenseMap(Ast.LocalId, void).init(pass.allocator),
            .rebased_inline_scopes = std.AutoHashMap(InlineScopeRebasePair, Ast.InlineScopeId).init(pass.allocator),
            .inline_scope_origins = collections.DenseMap(Ast.InlineScopeId, Ast.InlineScopeId).init(pass.allocator),
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
            .arena = std.heap.ArenaAllocator.init(pass.allocator),
            .source_fn = undefined, // initForRewrite never calls buildArgs, which is the only reader.
            .pattern = .{ .args = &.{} },
            .subst = Subst.init(pass.allocator),
            .inline_stack = .empty,
            .loop_stack = .empty,
            .loop_exit_stack = .empty,
            .selected_loop_exit_tys = collections.DenseMap(Ast.ExprId, Type.TypeId).init(pass.allocator),
            .join_stack = .empty,
            .let_case_shape_growth = .init(let_case_shape_arm_budget),
            .let_case_builds = .empty,
            .active_recursive_value_locals = collections.DenseMap(Ast.LocalId, void).init(pass.allocator),
            .rebased_inline_scopes = std.AutoHashMap(InlineScopeRebasePair, Ast.InlineScopeId).init(pass.allocator),
            .inline_scope_origins = collections.DenseMap(Ast.InlineScopeId, Ast.InlineScopeId).init(pass.allocator),
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

    fn deinit(self: *Cloner) void {
        self.inline_stack.deinit(self.pass.allocator);
        self.loop_stack.deinit(self.pass.allocator);
        self.loop_exit_stack.deinit(self.pass.allocator);
        self.selected_loop_exit_tys.deinit();
        self.join_stack.deinit(self.pass.allocator);
        self.let_case_builds.deinit(self.pass.allocator);
        self.active_recursive_value_locals.deinit();
        self.rebased_inline_scopes.deinit();
        self.inline_scope_origins.deinit();
        self.subst.deinit();
        self.arena.deinit();
    }

    fn admitInlineBodyGrowth(self: *Cloner, body_size: BodySize) bool {
        const exact_size = body_size.exactValue() orelse return false;
        return self.inline_body_growth.admit(@max(exact_size, 1)) == .admitted;
    }

    fn collectCallPatternsInExpr(self: *Cloner, owner: Ast.FnId, expr_id: Ast.ExprId) Common.LowerError!void {
        const expr = self.pass.program.getExpr(expr_id);
        switch (expr.data) {
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
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            => {},
            .fn_ref => |fn_ref| try self.collectCallPatternsInCaptureOperandSpan(owner, fn_ref.captures),
            .list,
            .tuple,
            => |items| try self.collectCallPatternsInExprSpan(owner, items),
            .record => |fields| try self.collectCallPatternsInFieldExprSpan(owner, fields),
            .record_update => |update| {
                try self.collectCallPatternsInExpr(owner, update.base);
                try self.collectCallPatternsInFieldExprSpan(owner, update.fields);
            },
            .tag => |tag| try self.collectCallPatternsInExprSpan(owner, tag.payloads),
            .static_data_candidate => |candidate| try self.collectCallPatternsInExpr(owner, candidate.runtime_expr),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.collectCallPatternsInExpr(owner, child),
            .return_ => |ret| try self.collectCallPatternsInExpr(owner, ret.value),
            .expect_err => |expect_err| try self.collectCallPatternsInExpr(owner, expect_err.msg),
            .comptime_branch_taken => |taken| try self.collectCallPatternsInExpr(owner, taken.body),
            .let_ => |let_| try self.collectCallPatternsInLet(owner, let_.bind, let_.value, let_.rest, false),
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .call_value => |call| {
                try self.collectCallPatternsInExpr(owner, call.callee);
                try self.collectCallPatternsInExprSpan(owner, call.args);
            },
            .call_proc => |call| {
                try self.collectCallPatternsInExprSpan(owner, call.args);
                try self.collectCallPatternsInCaptureOperandSpan(owner, call.captures);

                const callee = Ast.localDirectCallee(call) orelse return;
                const callee_raw = @intFromEnum(callee);
                if (callee_raw >= self.pass.plans.len) return;
                if (self.pass.newSpecAdmission(callee_raw) != .admitted) return;

                const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(call.args));
                defer self.pass.allocator.free(args);

                const values = try self.pass.allocator.alloc(Value, args.len);
                defer self.pass.allocator.free(values);
                for (args, 0..) |arg, index| {
                    const demand_shape = callee_raw < self.pass.plans.len and
                        index < self.pass.plans[callee_raw].used_args.len and
                        self.pass.plans[callee_raw].used_args[index];
                    values[index] = if (demand_shape)
                        (try self.cloneExprValueDemandingShape(arg)).value
                    else
                        (try self.cloneExprValue(arg)).value;
                }
                try self.pass.recordCallPatternForValues(callee, values);
            },
            .low_level => |call| try self.collectCallPatternsInExprSpan(owner, call.args),
            .field_access => |field| try self.collectCallPatternsInExpr(owner, field.receiver),
            .tuple_access => |access| try self.collectCallPatternsInExpr(owner, access.tuple),
            .structural_eq => |eq| {
                try self.collectCallPatternsInExpr(owner, eq.lhs);
                try self.collectCallPatternsInExpr(owner, eq.rhs);
            },
            .structural_hash => |h| {
                try self.collectCallPatternsInExpr(owner, h.value);
                try self.collectCallPatternsInExpr(owner, h.hasher);
            },
            .match_ => |match| {
                try self.collectCallPatternsInExpr(owner, match.scrutinee);
                try self.collectCallPatternsInBranchSpan(owner, match.branches);
            },
            .if_ => |if_| {
                try self.collectCallPatternsInIfBranchSpan(owner, if_.branches);
                try self.collectCallPatternsInExpr(owner, if_.final_else);
            },
            .block => |block| {
                const change_start = self.subst.watermark();
                defer self.subst.restore(change_start);
                try self.collectCallPatternsInStmtSpan(owner, block.statements);
                try self.collectCallPatternsInExpr(owner, block.final_expr);
            },
            .loop_ => |loop| {
                try self.collectCallPatternsInExprSpan(owner, loop.initial_values);
                const change_start = self.subst.watermark();
                defer self.subst.restore(change_start);
                const params = self.pass.program.typedLocalSpan(loop.params);
                for (0..params.len) |index| {
                    try self.shadowLocal(GuardedList.at(params, index).local);
                }
                try self.collectCallPatternsInExpr(owner, loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectCallPatternsInExpr(owner, value),
            .continue_ => |continue_| try self.collectCallPatternsInExprSpan(owner, continue_.values),
            .join_point => |join_point| {
                const change_start = self.subst.watermark();
                const params = self.pass.program.typedLocalSpan(join_point.params);
                for (0..params.len) |index| try self.shadowLocal(GuardedList.at(params, index).local);
                try self.collectCallPatternsInExpr(owner, join_point.body);
                self.subst.restore(change_start);
                try self.collectCallPatternsInExpr(owner, join_point.remainder);
            },
            .jump => |jump| try self.collectCallPatternsInExprSpan(owner, jump.args),
            .if_initialized_payload => |payload_switch| {
                try self.collectCallPatternsInExpr(owner, payload_switch.cond);
                try self.collectCallPatternsInExpr(owner, payload_switch.initialized);
                try self.collectCallPatternsInExpr(owner, payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.collectCallPatternsInExpr(owner, sequence.try_expr);
                const change_start = self.subst.watermark();
                defer self.subst.restore(change_start);
                try self.shadowLocal(sequence.ok_local);
                try self.collectCallPatternsInExpr(owner, sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.collectCallPatternsInExpr(owner, sequence.try_expr);
                const change_start = self.subst.watermark();
                defer self.subst.restore(change_start);
                try self.shadowLocal(sequence.value_local);
                try self.shadowLocal(sequence.rest_local);
                try self.collectCallPatternsInExpr(owner, sequence.ok_body);
            },
        }
    }

    fn collectCallPatternsInLet(
        self: *Cloner,
        owner: Ast.FnId,
        pat_id: Ast.PatId,
        value_expr: Ast.ExprId,
        rest_expr: Ast.ExprId,
        recursive: bool,
    ) Common.LowerError!void {
        try self.collectCallPatternsInExpr(owner, value_expr);

        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);

        var cloned = try self.cloneExprValue(value_expr);
        if (!try self.bindPatternForValueFlow(pat_id, value_expr, recursive, cloned.value, &cloned.bindings)) {
            try self.shadowPatLocals(pat_id);
        }
        try self.collectCallPatternsInExpr(owner, rest_expr);
    }

    fn collectCallPatternsInExprSpan(self: *Cloner, owner: Ast.FnId, span: Ast.Span(Ast.ExprId)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, expr: Ast.ExprId) Common.LowerError!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, expr);
            }
        }.visit);
    }

    fn collectCallPatternsInCaptureOperandSpan(self: *Cloner, owner: Ast.FnId, span: Ast.Span(Ast.CaptureOperand)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.CaptureOperand, self.pass.program.captureOperandSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, operand: Ast.CaptureOperand) Common.LowerError!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, operand.value);
            }
        }.visit);
    }

    fn collectCallPatternsInFieldExprSpan(self: *Cloner, owner: Ast.FnId, span: Ast.Span(Ast.FieldExpr)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.FieldExpr, self.pass.program.fieldExprSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, field: Ast.FieldExpr) Common.LowerError!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, field.value);
            }
        }.visit);
    }

    fn collectCallPatternsInBranchSpan(self: *Cloner, owner: Ast.FnId, span: Ast.Span(Ast.Branch)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, branch: Ast.Branch) Common.LowerError!void {
                const change_start = ctx.self.subst.watermark();
                defer ctx.self.subst.restore(change_start);
                try ctx.self.shadowPatLocals(branch.pat);
                try ctx.self.collectCallPatternsInStmtSpan(ctx.owner, branch.bindings);
                if (branch.guard) |guard| try ctx.self.collectCallPatternsInExpr(ctx.owner, guard);
                try ctx.self.collectCallPatternsInExpr(ctx.owner, branch.body);
            }
        }.visit);
    }

    fn collectCallPatternsInIfBranchSpan(self: *Cloner, owner: Ast.FnId, span: Ast.Span(Ast.IfBranch)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.IfBranch, self.pass.program.ifBranchSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, branch: Ast.IfBranch) Common.LowerError!void {
                try ctx.self.collectCallPatternsInExpr(ctx.owner, branch.cond);
                try ctx.self.collectCallPatternsInExpr(ctx.owner, branch.body);
            }
        }.visit);
    }

    fn collectCallPatternsInStmtSpan(self: *Cloner, owner: Ast.FnId, span: Ast.Span(Ast.StmtId)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(span), .{ .self = self, .owner = owner }, struct {
            fn visit(ctx: anytype, stmt: Ast.StmtId) Common.LowerError!void {
                try ctx.self.collectCallPatternsInStmt(ctx.owner, stmt);
            }
        }.visit);
    }

    fn collectCallPatternsInStmt(self: *Cloner, owner: Ast.FnId, stmt_id: Ast.StmtId) Common.LowerError!void {
        switch (self.pass.program.getStmt(stmt_id)) {
            .let_ => |let_| {
                try self.collectCallPatternsInExpr(owner, let_.value);
                var cloned = try self.cloneExprValue(let_.value);
                if (!try self.bindPatternForValueFlow(let_.pat, let_.value, let_.recursive, cloned.value, &cloned.bindings)) {
                    try self.shadowPatLocals(let_.pat);
                }
            },
            .expr,
            .expect,
            .dbg,
            => |expr| try self.collectCallPatternsInExpr(owner, expr),
            .return_ => |ret| try self.collectCallPatternsInExpr(owner, ret.value),
            .uninitialized => |pat| try self.shadowPatLocals(pat),
            .crash => {},
        }
    }

    fn bindPatternForValueFlow(
        self: *Cloner,
        pat_id: Ast.PatId,
        source_value: Ast.ExprId,
        recursive: bool,
        value: Value,
        bindings: *BindingChain,
    ) Common.LowerError!bool {
        const change_before = self.subst.watermark();
        const bindings_before = bindings.mark();
        if (try self.bindPatToReusableValue(pat_id, value) == .match) return true;
        self.subst.restore(change_before);
        bindings.rewind(bindings_before);

        const pat = self.pass.program.getPat(pat_id);
        const self_referential = if (pat.data == .bind)
            localUseCountInExpr(self.pass.program, pat.data.bind, source_value) != 0
        else
            recursive;
        if (self_referential) return false;

        const reusable = try self.makeReusableForMatch(value, bindings);
        if (try self.bindPatToFlowValue(pat_id, reusable)) return true;
        self.subst.restore(change_before);
        bindings.rewind(bindings_before);
        return false;
    }

    fn buildArgs(self: *Cloner) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        const source_fn = self.pass.program.getFn(self.source_fn);
        const source_args = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        if (source_args.len != self.pattern.args.len) Common.invariant("call-pattern argument count differed from source function arity");
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        self.current_loc = switch (source_fn.body) {
            .roc => |body| self.pass.program.exprLoc(body),
            .hosted => SourceLoc.none,
        };
        self.current_region = switch (source_fn.body) {
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

    fn valueFromShapeArgs(self: *Cloner, shape: Shape, args: *std.ArrayList(Ast.TypedLocal)) Allocator.Error!Value {
        switch (shape) {
            .any => |ty| {
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try args.append(self.pass.allocator, .{ .local = local, .ty = ty });
                return .{ .expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                }) };
            },
            .tag => |tag| {
                const payloads = try self.arena.allocator().alloc(Value, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = try self.valueFromShapeArgs(payload, args);
                }
                return .{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| {
                const fields = try self.arena.allocator().alloc(FieldValue, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.valueFromShapeArgs(field.shape, args),
                    };
                }
                return .{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| {
                const items = try self.arena.allocator().alloc(Value, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = try self.valueFromShapeArgs(item, args);
                }
                return .{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| {
                const backing = try self.arena.allocator().create(Value);
                backing.* = try self.valueFromShapeArgs(nominal.backing.*, args);
                return .{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| {
                // A callable shape's captures are parallel, in ascending
                // CaptureId order, to its function's sorted capture slots, so we
                // read each capture's CaptureId from the matching slot.
                const slots = self.pass.program.typedLocalSpan(self.pass.program.getFn(callable.fn_id).captures);
                if (slots.len != callable.captures.len) {
                    Common.invariant("callable shape capture count differed from its function capture slots");
                }
                const captures = try self.arena.allocator().alloc(CaptureValue, callable.captures.len);
                for (0..callable.captures.len) |index| {
                    const capture = callable.captures[index];
                    const slot = GuardedList.at(slots, index);
                    captures[index] = .{
                        .id = self.pass.program.captureIdOfLocal(slot.local),
                        .value = try self.valueFromShapeArgs(capture, args),
                    };
                }
                return .{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
        }
    }

    fn cloneExpr(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Ast.ExprId {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const saved_inline_scope = self.current_inline_scope;
        defer self.current_inline_scope = saved_inline_scope;
        try self.adoptExprInlineScope(expr_id);
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;

        const cloned = try self.cloneExprValue(expr_id);
        return try self.wrapBindings(cloned.bindings, try self.materialize(cloned.value));
    }

    fn cloneExprValue(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!ClonedValue {
        var bindings: BindingChain = .{};
        const value = try self.cloneExprValueInto(expr_id, &bindings);
        return .{ .bindings = bindings, .value = value };
    }

    fn cloneExprValueInto(self: *Cloner, expr_id: Ast.ExprId, bindings: *BindingChain) Common.LowerError!Value {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const saved_inline_scope = self.current_inline_scope;
        defer self.current_inline_scope = saved_inline_scope;
        try self.adoptExprInlineScope(expr_id);
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;

        const expr = self.pass.program.getExpr(expr_id);
        switch (expr.data) {
            .local => |local| {
                if (self.subst.getForClone(self.pass.program, local)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } }) };
            },
            .fn_ref => |fn_ref| return try self.callableValueFromRef(expr.ty, fn_ref, bindings),
            .static_data_candidate => |candidate| {
                const runtime = try self.arena.allocator().create(Value);
                runtime.* = try self.cloneExprValueDemandingShapeInto(candidate.runtime_expr, bindings);
                return .{ .static_data_candidate = .{
                    .ty = expr.ty,
                    .static_data = candidate.static_data,
                    .runtime = runtime,
                } };
            },
            .tag => |tag| {
                assertStructuralConstructionType(self.pass.program, expr.ty);
                const payload_exprs = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(tag.payloads));
                defer self.pass.allocator.free(payload_exprs);
                const payloads = try self.arena.allocator().alloc(Value, payload_exprs.len);
                for (payload_exprs, 0..) |payload, index| {
                    payloads[index] = try self.cloneExprValueDemandingShapeInto(payload, bindings);
                }
                return .{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |fields_span| {
                assertStructuralConstructionType(self.pass.program, expr.ty);
                const source_fields = try GuardedList.dupe(self.pass.allocator, Ast.FieldExpr, self.pass.program.fieldExprSpan(fields_span));
                defer self.pass.allocator.free(source_fields);
                const fields = try self.arena.allocator().alloc(FieldValue, source_fields.len);
                for (source_fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.cloneExprValueDemandingShapeInto(field.value, bindings),
                    };
                }
                return .{ .record = .{
                    .ty = expr.ty,
                    .fields = fields,
                } };
            },
            .record_update => |update| {
                const source_fields = try GuardedList.dupe(
                    self.pass.allocator,
                    Ast.FieldExpr,
                    self.pass.program.fieldExprSpan(update.fields),
                );
                defer self.pass.allocator.free(source_fields);
                const record_ty = recordUpdateBackingType(self.pass.program, expr.ty);
                const type_fields = try GuardedList.dupe(
                    self.pass.allocator,
                    Type.Field,
                    self.pass.program.types.fieldSpan(recordUpdateFieldSpan(self.pass.program, expr.ty)),
                );
                defer self.pass.allocator.free(type_fields);

                const base = try self.cloneExpr(update.base);
                const base_ty = self.pass.program.getExpr(base).ty;
                if (!sameType(self.pass.program, base_ty, expr.ty)) {
                    Common.invariant("record update base type differed from its result type in SpecConstr");
                }
                const base_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), base_ty);
                try bindings.appendBinding(self.arena.allocator(), .{
                    .local = base_local,
                    .ty = base_ty,
                    .value = base,
                });
                const base_ref = try self.addExpr(.{ .ty = base_ty, .data = .{ .local = base_local } });

                const fields = try self.arena.allocator().alloc(FieldValue, type_fields.len);
                for (type_fields, 0..) |type_field, index| {
                    const updated = for (source_fields) |field| {
                        if (self.pass.program.names.recordFieldLabelTextEql(type_field.name, field.name)) break field.value;
                    } else null;
                    if (updated != null) continue;

                    const read = try self.addExpr(.{ .ty = type_field.ty, .data = .{ .field_access = .{
                        .receiver = base_ref,
                        .segments = try self.pass.program.addFieldAccessSegmentSpan(&.{.{ .field = type_field.name }}),
                    } } });
                    const read_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), type_field.ty);
                    try bindings.appendBinding(self.arena.allocator(), .{
                        .local = read_local,
                        .ty = type_field.ty,
                        .value = read,
                    });
                    fields[index] = .{
                        .name = type_field.name,
                        .value = .{ .expr = try self.addExpr(.{ .ty = type_field.ty, .data = .{ .local = read_local } }) },
                    };
                }

                for (type_fields, 0..) |type_field, index| {
                    const updated = for (source_fields) |field| {
                        if (self.pass.program.names.recordFieldLabelTextEql(type_field.name, field.name)) break field.value;
                    } else continue;
                    fields[index] = .{
                        .name = type_field.name,
                        .value = try self.cloneExprValueDemandingShapeInto(updated, bindings),
                    };
                }
                const record_value = Value{ .record = .{
                    .ty = record_ty,
                    .fields = fields,
                } };
                if (nominalConstructionLayer(self.pass.program, expr.ty) != null) {
                    const backing = try self.arena.allocator().create(Value);
                    backing.* = record_value;
                    return .{ .nominal = .{
                        .ty = expr.ty,
                        .backing = backing,
                    } };
                }
                return record_value;
            },
            .tuple => |items_span| {
                assertStructuralConstructionType(self.pass.program, expr.ty);
                const source_items = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(items_span));
                defer self.pass.allocator.free(source_items);
                const items = try self.arena.allocator().alloc(Value, source_items.len);
                for (source_items, 0..) |item, index| {
                    items[index] = try self.cloneExprValueDemandingShapeInto(item, bindings);
                }
                return .{ .tuple = .{
                    .ty = expr.ty,
                    .items = items,
                } };
            },
            .nominal => |backing| {
                const backing_value = try self.cloneExprValueDemandingShapeInto(backing, bindings);
                return .{ .nominal = .{
                    .ty = expr.ty,
                    .backing = try self.copyValue(backing_value),
                } };
            },
            .let_ => |let_| return try self.cloneLetValue(let_, bindings),
            .loop_ => |loop| return try self.cloneLoopValue(expr.ty, loop, bindings, null),
            .block => |block| {
                if (try self.cloneBlockValue(block, bindings)) |value| return value;
                return .{ .expr = try self.cloneExprPlain(expr_id) };
            },
            .field_access => |field| return try self.cloneFieldAccessValue(expr.ty, field, bindings),
            .tuple_access => |access| {
                const receiver = try self.cloneExprValueDemandingShapeInto(access.tuple, bindings);
                if (itemFromValue(receiver, access.elem_index)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = try self.materialize(receiver),
                    .elem_index = access.elem_index,
                } } }) };
            },
            .match_ => |match| {
                const scrutinee = try self.cloneExprValueDemandingShapeInto(match.scrutinee, bindings);
                if (try self.simplifyKnownMatchValue(scrutinee, match.branches, bindings)) |value| return value;
                const scrutinee_expr = try self.materialize(scrutinee);
                if (try self.cloneCaseOfCaseValue(expr.ty, scrutinee_expr, match.branches)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .match_ = .{
                    .scrutinee = scrutinee_expr,
                    .branches = try self.cloneBranchSpan(match.branches),
                    .comptime_site = match.comptime_site,
                } } }) };
            },
            .call_value => |call| {
                const callee = try self.cloneExprValueDemandingShapeInto(call.callee, bindings);
                if (callee == .callable and self.inline_calls.admitsCallable(callee.callable, self.iterator_inline_depth != 0)) {
                    const enters_iterator = self.inline_calls == .iterator_fusion;
                    if (enters_iterator) self.iterator_inline_depth += 1;
                    defer {
                        if (enters_iterator) self.iterator_inline_depth -= 1;
                    }
                    return try self.inlineCallableCallValue(expr.ty, callee.callable, call.args, expr_id, false, bindings);
                }
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(callee),
                    .args = try self.cloneExprSpan(call.args),
                } } }) };
            },
            .call_proc => |call| {
                if (call.is_cold) return .{ .expr = try self.cloneExprPlain(expr_id) };
                if (!self.inline_calls.admitsDirect(call.iterator_procedure, self.iterator_inline_depth != 0)) {
                    return .{ .expr = try self.cloneExprPlain(expr_id) };
                }
                const callee = Ast.localDirectCallee(call) orelse return .{ .expr = try self.cloneExprPlain(expr_id) };
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
                    return .{ .expr = try self.cloneExprPlain(expr_id) };
                }
                const enters_iterator = self.inline_calls == .iterator_fusion;
                if (enters_iterator) self.iterator_inline_depth += 1;
                defer {
                    if (enters_iterator) self.iterator_inline_depth -= 1;
                }
                return try self.inlineDirectCallValue(
                    callee,
                    call.args,
                    call.captures,
                    expr_id,
                    false,
                    bindings,
                );
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
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => return .{ .expr = try self.cloneExprPlain(expr_id) },
        }
    }

    fn cloneExprValueDemandingShape(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!ClonedValue {
        var bindings: BindingChain = .{};
        const value = try self.cloneExprValueDemandingShapeInto(expr_id, &bindings);
        return .{ .bindings = bindings, .value = value };
    }

    fn cloneExprValueDemandingShapeInto(self: *Cloner, expr_id: Ast.ExprId, bindings: *BindingChain) Common.LowerError!Value {
        const expr = self.pass.program.getExpr(expr_id);
        return switch (expr.data) {
            .call_proc => |call| blk: {
                if (call.is_cold or !self.inline_calls.admitsDirect(call.iterator_procedure, self.iterator_inline_depth != 0)) {
                    break :blk try self.cloneExprValueInto(expr_id, bindings);
                }
                const callee = Ast.localDirectCallee(call) orelse break :blk try self.cloneExprValueInto(expr_id, bindings);
                const enters_iterator = self.inline_calls == .iterator_fusion;
                if (enters_iterator) self.iterator_inline_depth += 1;
                defer {
                    if (enters_iterator) self.iterator_inline_depth -= 1;
                }
                break :blk try self.inlineDirectCallValue(callee, call.args, call.captures, expr_id, true, bindings);
            },
            .call_value => |call| blk: {
                const callee = try self.cloneExprValueDemandingShapeInto(call.callee, bindings);
                if (callee == .callable and self.inline_calls.admitsCallable(callee.callable, self.iterator_inline_depth != 0)) {
                    const enters_iterator = self.inline_calls == .iterator_fusion;
                    if (enters_iterator) self.iterator_inline_depth += 1;
                    defer {
                        if (enters_iterator) self.iterator_inline_depth -= 1;
                    }
                    break :blk try self.inlineCallableCallValue(expr.ty, callee.callable, call.args, expr_id, true, bindings);
                }
                break :blk .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(callee),
                    .args = try self.cloneExprSpan(call.args),
                } } }) };
            },
            .block => |block| if (self.pass.program.stmtSpan(block.statements).len == 0)
                try self.cloneExprValueDemandingShapeInto(block.final_expr, bindings)
            else
                try self.cloneExprValueInto(expr_id, bindings),
            .comptime_branch_taken => |taken| try self.cloneExprValueDemandingShapeInto(taken.body, bindings),
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
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => try self.cloneExprValueInto(expr_id, bindings),
        };
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

    fn exprHasKnownShape(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!bool {
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
            .static_data_candidate => |candidate| try self.exprHasKnownShape(candidate.runtime_expr),
            .comptime_branch_taken => |taken| try self.exprHasKnownShape(taken.body),
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
            .dbg,
            .expect_err,
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

    fn valueCanSubstitute(self: *Cloner, value: Value) ProofStatus {
        var budget: u32 = value_substitute_work_budget;
        return self.valueCanSubstituteBudgeted(value, &budget);
    }

    fn valueCanSubstituteBudgeted(self: *Cloner, value: Value, budget: *u32) ProofStatus {
        if (budget.* == 0) return .unknown_budget_exhausted;
        budget.* -= 1;
        return switch (value) {
            .expr => |expr| if (self.exprCanSubstitute(expr)) .proven else .disproven,
            .static_data_candidate => |candidate| self.valueCanSubstituteBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                var proof = ProofStatus.proven;
                for (tag.payloads) |payload| {
                    proof = proofAnd(proof, self.valueCanSubstituteBudgeted(payload, budget));
                    if (proof == .disproven) break;
                }
                break :blk proof;
            },
            .record => |record| blk: {
                var proof = ProofStatus.proven;
                for (record.fields) |field| {
                    proof = proofAnd(proof, self.valueCanSubstituteBudgeted(field.value, budget));
                    if (proof == .disproven) break;
                }
                break :blk proof;
            },
            .tuple => |tuple| blk: {
                var proof = ProofStatus.proven;
                for (tuple.items) |item| {
                    proof = proofAnd(proof, self.valueCanSubstituteBudgeted(item, budget));
                    if (proof == .disproven) break;
                }
                break :blk proof;
            },
            .nominal => |nominal| self.valueCanSubstituteBudgeted(nominal.backing.*, budget),
            .callable => |callable| blk: {
                var proof = ProofStatus.proven;
                for (callable.captures) |capture| {
                    proof = proofAnd(proof, self.valueCanSubstituteBudgeted(capture.value, budget));
                    if (proof == .disproven) break;
                }
                break :blk proof;
            },
        };
    }

    fn exprCanSubstitute(self: *Cloner, expr_id: Ast.ExprId) bool {
        return switch (self.pass.program.getExpr(expr_id).data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            => true,
            .fn_ref => |fn_ref| self.captureOperandSpanCanSubstitute(fn_ref.captures),
            .static_data_candidate => |candidate| self.exprCanSubstitute(candidate.runtime_expr),
            .field_access => |field| self.exprCanSubstitute(field.receiver),
            .tuple_access => |access| self.exprCanSubstitute(access.tuple),
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
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => false,
        };
    }

    fn captureOperandSpanCanSubstitute(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) bool {
        const operand_count: usize = @intCast(span.len);
        for (0..operand_count) |index| {
            const operand = self.pass.program.captureOperandAt(span, index);
            if (!self.exprCanSubstitute(operand.value)) return false;
        }
        return true;
    }

    fn callableValueFromRef(
        self: *Cloner,
        ty: Type.TypeId,
        fn_ref: @import("../monotype/ast.zig").LiftedFunctionValue,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        const source_fn = self.pass.program.getFn(fn_ref.fn_id);
        if (source_fn.body == .roc and
            self.pass.inlineBodyAdmission(fn_ref.fn_id, source_fn.body.roc) != .admitted)
        {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .fn_ref = .{
                .fn_id = fn_ref.fn_id,
                .captures = try self.cloneCaptureOperandSpan(fn_ref.captures),
            } } }) };
        }

        const capture_count: usize = @intCast(fn_ref.captures.len);
        const captures = try self.arena.allocator().alloc(CaptureValue, capture_count);
        for (0..capture_count) |index| {
            const operand = self.pass.program.captureOperandAt(fn_ref.captures, index);
            captures[index] = .{
                .id = operand.id,
                .value = try self.cloneExprValueInto(operand.value, bindings),
            };
        }
        return .{ .callable = .{
            .ty = ty,
            .fn_id = fn_ref.fn_id,
            .captures = captures,
        } };
    }

    fn cloneExprPlain(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Ast.ExprId {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const saved_inline_scope = self.current_inline_scope;
        defer self.current_inline_scope = saved_inline_scope;
        try self.adoptExprInlineScope(expr_id);
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;

        const expr = self.pass.program.getExpr(expr_id);
        const data: Ast.ExprData = switch (expr.data) {
            .@"unreachable" => .@"unreachable",
            .local => |local| .{ .local = local },
            .unit => .unit,
            .uninitialized => .uninitialized,
            .uninitialized_payload => |payload| .{ .uninitialized_payload = .{
                .condition = self.cloneLocalRef(payload.condition),
                .mask = payload.mask,
            } },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .bytes_lit => |value| .{ .bytes_lit = value },
            .list => |items| .{ .list = try self.cloneExprSpan(items) },
            .tuple => |items| .{ .tuple = try self.cloneExprSpan(items) },
            .record => |fields| .{ .record = try self.cloneFieldExprSpan(fields) },
            .record_update => |update| .{ .record_update = .{
                .base = try self.cloneExpr(update.base),
                .fields = try self.cloneFieldExprSpan(update.fields),
            } },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.cloneExprSpan(tag.payloads),
            } },
            .static_data_candidate => |candidate| .{ .static_data_candidate = .{
                .static_data = candidate.static_data,
                .runtime_expr = try self.cloneExpr(candidate.runtime_expr),
            } },
            .nominal => |backing| .{ .nominal = try self.cloneExpr(backing) },
            .let_ => |let_| try self.cloneLet(let_),
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .fn_ref => |fn_ref| .{ .fn_ref = .{
                .fn_id = fn_ref.fn_id,
                .captures = try self.cloneCaptureOperandSpan(fn_ref.captures),
            } },
            .call_value => |call| .{ .call_value = .{
                .callee = try self.cloneExpr(call.callee),
                .args = try self.cloneExprSpan(call.args),
            } },
            .call_proc => |call| return try self.cloneCallProc(expr.ty, call),
            .low_level => |call| .{ .low_level = .{
                .op = call.op,
                .args = try self.cloneExprSpan(call.args),
            } },
            .field_access => |field| return try self.cloneFieldAccess(expr.ty, field),
            .tuple_access => |access| return try self.cloneTupleAccess(expr.ty, access),
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.cloneExpr(eq.lhs),
                .rhs = try self.cloneExpr(eq.rhs),
                .negated = eq.negated,
            } },
            .structural_hash => |h| .{ .structural_hash = .{
                .value = try self.cloneExpr(h.value),
                .hasher = try self.cloneExpr(h.hasher),
            } },
            .match_ => |match| return try self.cloneMatch(expr.ty, match),
            .if_ => |if_| .{ .if_ = .{
                .branches = try self.cloneIfBranchSpan(if_.branches),
                .final_else = try self.cloneExpr(if_.final_else),
            } },
            .block => |block| return try self.cloneBlock(expr.ty, block),
            .loop_ => |loop| {
                var bindings: BindingChain = .{};
                const value = try self.cloneLoopValue(expr.ty, loop, &bindings, null);
                return try self.wrapBindings(bindings, try self.materialize(value));
            },
            .break_ => |maybe| blk: {
                if (self.currentLoopExitSelection()) |selection| {
                    if (self.selected_loop_exit_tys.get(expr_id)) |selected_ty| {
                        if (selected_ty != selection.result_ty) {
                            Common.invariant("selected break was re-cloned under a different loop exit ABI");
                        }
                        const projected = try self.addExpr(.{
                            .ty = expr.ty,
                            .data = .{ .break_ = if (maybe) |value| try self.cloneExpr(value) else null },
                        });
                        try self.selected_loop_exit_tys.put(projected, selected_ty);
                        return projected;
                    }
                    const value = maybe orelse Common.invariant("selected value-producing loop had a valueless break");
                    return try self.cloneSelectedLoopExit(expr.ty, value, selection);
                }
                break :blk .{ .break_ = if (maybe) |value| try self.cloneExpr(value) else null };
            },
            .continue_ => |continue_| return try self.cloneContinue(expr.ty, continue_),
            .join_point => |join_point| return try self.cloneJoinPoint(expr.ty, join_point),
            .jump => |jump| blk: {
                if (self.letCaseJoinFor(jump.target)) |join| {
                    return try self.captureLetCaseJump(expr.ty, join, jump);
                }
                if (self.selectedExitJumpSites(jump.target)) |sites| {
                    // A loop-exit transfer site minted by an active selection is
                    // being duplicated (an enclosing arm rewrite is re-cloning
                    // the region). The target is already in this clone's id
                    // space, and the selection must see every surviving copy of
                    // its exit, so keep the target and register the duplicate.
                    const duplicated = try self.addExpr(.{ .ty = expr.ty, .data = .{ .jump = .{
                        .target = jump.target,
                        .args = try self.cloneExprSpan(jump.args),
                    } } });
                    try sites.append(self.pass.allocator, duplicated);
                    return duplicated;
                }
                break :blk .{ .jump = .{
                    .target = self.clonedJoinTarget(jump.target),
                    .args = try self.cloneExprSpan(jump.args),
                } };
            },
            .if_initialized_payload => |payload_switch| .{ .if_initialized_payload = .{
                .cond = try self.cloneExpr(payload_switch.cond),
                .cond_mask = payload_switch.cond_mask,
                .payload = self.cloneLocalRef(payload_switch.payload),
                .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
                .initialized = try self.cloneExpr(payload_switch.initialized),
                .uninitialized = try self.cloneExpr(payload_switch.uninitialized),
            } },
            .try_sequence => |sequence| blk: {
                const try_expr = try self.cloneExpr(sequence.try_expr);
                const shadow_start = self.subst.watermark();
                const ok_ty = self.pass.program.getLocal(sequence.ok_local).ty;
                const ok_local = try self.cloneBinder(sequence.ok_local, ok_ty, .bind_runtime);
                const ok_body = try self.cloneExpr(sequence.ok_body);
                self.subst.restore(shadow_start);
                break :blk .{ .try_sequence = .{
                    .try_expr = try_expr,
                    .ok_local = ok_local,
                    .err_is_cold = sequence.err_is_cold,
                    .ok_body = ok_body,
                } };
            },
            .try_record_sequence => |sequence| blk: {
                const try_expr = try self.cloneExpr(sequence.try_expr);
                const shadow_start = self.subst.watermark();
                const value_ty = self.pass.program.getLocal(sequence.value_local).ty;
                const value_local = try self.cloneBinder(sequence.value_local, value_ty, .bind_runtime);
                const rest_ty = self.pass.program.getLocal(sequence.rest_local).ty;
                const rest_local = try self.cloneBinder(sequence.rest_local, rest_ty, .bind_runtime);
                const ok_body = try self.cloneExpr(sequence.ok_body);
                self.subst.restore(shadow_start);
                break :blk .{ .try_record_sequence = .{
                    .try_expr = try_expr,
                    .value_local = value_local,
                    .value_field = sequence.value_field,
                    .rest_local = rest_local,
                    .rest_field = sequence.rest_field,
                    .err_is_cold = sequence.err_is_cold,
                    .ok_body = ok_body,
                } };
            },
            .return_ => |ret| .{ .return_ = .{
                .value = try self.cloneExpr(ret.value),
                .target = ret.target,
            } },
            .crash => |msg| .{ .crash = msg },
            .comptime_branch_taken => |taken| .{ .comptime_branch_taken = .{
                .site = taken.site,
                .branch_index = taken.branch_index,
                .body = try self.cloneExpr(taken.body),
            } },
            .comptime_exhaustiveness_failed => |site| .{ .comptime_exhaustiveness_failed = site },
            .dbg => |child| .{ .dbg = try self.cloneExpr(child) },
            .expect_err => |expect_err| .{ .expect_err = .{
                .msg = try self.cloneExpr(expect_err.msg),
                .region = expect_err.region,
            } },
            .expect => |child| .{ .expect = try self.cloneExpr(child) },
        };
        return try self.addExpr(.{ .ty = expr.ty, .data = data });
    }

    fn cloneJoinPoint(self: *Cloner, ty: Type.TypeId, join_point: Ast.JoinPointExpr) Common.LowerError!Ast.ExprId {
        const source_params = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(join_point.params));
        defer self.pass.allocator.free(source_params);
        const params = try self.pass.allocator.alloc(Ast.TypedLocal, source_params.len);
        defer self.pass.allocator.free(params);
        for (source_params, 0..) |source_param, index| {
            const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), source_param.ty);
            params[index] = .{ .local = local, .ty = source_param.ty };
        }

        const target = self.pass.freshJoinPoint();
        try self.join_stack.append(self.pass.allocator, .{ .source = join_point.id, .target = target });
        defer _ = self.join_stack.pop();

        const change_start = self.subst.watermark();
        for (source_params, params) |source_param, param| {
            const local_expr = try self.addExpr(.{ .ty = param.ty, .data = .{ .local = param.local } });
            try self.subst.put(self.pass.program, source_param.local, .{ .expr = local_expr });
        }
        const body = try self.cloneExpr(join_point.body);
        // The remainder's jumps may forward-reference the join's own params:
        // an `uninitialized_payload` argument names the flag param carrying
        // its initialized-ness. Keep the param substitutions active so those
        // references follow the freshened params.
        const remainder = try self.cloneExpr(join_point.remainder);
        self.subst.restore(change_start);

        return try self.addExpr(.{ .ty = ty, .data = .{ .join_point = .{
            .id = target,
            .params = try self.pass.program.addTypedLocalSpan(params),
            .body = body,
            .remainder = remainder,
        } } });
    }

    fn clonedJoinTarget(self: *Cloner, source: Ast.JoinPointId) Ast.JoinPointId {
        var index = self.join_stack.items.len;
        while (index > 0) {
            index -= 1;
            const join_point = self.join_stack.items[index];
            if (join_point.source == source) return join_point.target;
        }
        // Not being remapped: the join's definition encloses the region being
        // cloned rather than sitting inside it. Rewrites re-clone regions of
        // already-emitted output in place (arm transfers, loop exit
        // selection), and a jump out of such a region must keep aiming at the
        // enclosing definition. Join ids are minted from one pass-wide
        // counter, so the id cannot collide with a different join.
        return source;
    }

    fn cloneLetValue(self: *Cloner, let_: anytype, bindings: *BindingChain) Common.LowerError!Value {
        if (try self.loopWithSelectedExitValues(let_)) |selected| return try self.cloneExprValueInto(selected, bindings);

        var value_bindings: BindingChain = .{};
        const value = try self.cloneExprValueInto(let_.value, &value_bindings);
        bindings.appendChain(value_bindings);
        const value_expr = try self.materialize(value);
        if (self.caseExprFromValue(value)) |case_expr| {
            if (try self.cloneLetOfCase(let_, case_expr)) |data| {
                const rest_ty = self.pass.program.getExpr(let_.rest).ty;
                return .{ .expr = try self.addExpr(.{ .ty = rest_ty, .data = data }) };
            }
        }
        const change_start = self.subst.watermark();
        const bound = try self.bindPatToReusableValue(let_.bind, value);
        if (bound == .match) {
            const rest = try self.cloneExprValueInto(let_.rest, bindings);
            try self.subst.restoreFloatingLoopCarries(change_start);
            return rest;
        }
        self.subst.restore(change_start);
        if (try self.bindPatToPositionedReusableValue(let_.bind, let_.value, false, value, bindings)) {
            const rest = try self.cloneExprValueInto(let_.rest, bindings);
            try self.subst.restoreFloatingLoopCarries(change_start);
            return rest;
        }
        // A branch-built value that cannot bind as one value transfers each
        // branch result to one shared continuation.
        if (self.caseExprFromValue(value)) |case_expr| {
            if (try self.cloneLetOfCase(let_, case_expr)) |data| {
                const rest_ty = self.pass.program.getExpr(let_.rest).ty;
                return .{ .expr = try self.addExpr(.{ .ty = rest_ty, .data = data }) };
            }
        }
        // Name the value's opaque leaves and pin them at this position: the
        // same computations in the same order, but the bound name keeps its
        // structured value for the continuation.
        const bind = try self.clonePat(let_.bind, .bind_runtime);
        const rest = try self.cloneExpr(let_.rest);
        try self.subst.restoreFloatingLoopCarries(change_start);
        return .{ .expr = try self.addExpr(.{ .ty = self.pass.program.getExpr(let_.rest).ty, .data = .{ .let_ = .{
            .bind = bind,
            .value = value_expr,
            .rest = rest,
            .comptime_site = let_.comptime_site,
        } } }) };
    }

    /// Remove dead values from a loop's exit ABI using the exact binding pattern
    /// and continuation that consume it. The loop still carries its complete
    /// state through every `continue`; exits transfer only the live values to the
    /// unchanged continuation. Returns null when the binding is not a
    /// compiler-generated tuple state or when every value remains live.
    fn loopWithSelectedExitValues(self: *Cloner, let_: anytype) Common.LowerError!?Ast.ExprId {
        const loop_expr = self.pass.program.getExpr(let_.value);
        if (loop_expr.data != .loop_) return null;
        const loop = loop_expr.data.loop_;

        var kept_indices = std.ArrayList(u32).empty;
        defer kept_indices.deinit(self.pass.allocator);
        var kept_params = std.ArrayList(Ast.TypedLocal).empty;
        defer kept_params.deinit(self.pass.allocator);
        var source_arity: usize = 0;
        var aggregate_local: ?Ast.LocalId = null;
        var aggregate_tys: ?[]Type.TypeId = null;
        defer if (aggregate_tys) |tys| self.pass.allocator.free(tys);
        var selected_locals: ?[]?Ast.LocalId = null;
        defer if (selected_locals) |locals| self.pass.allocator.free(locals);

        switch (self.pass.program.getPat(let_.bind).data) {
            .tuple => |items_span| {
                const source_items = try GuardedList.dupe(self.pass.allocator, Ast.PatId, self.pass.program.patSpan(items_span));
                defer self.pass.allocator.free(source_items);
                if (source_items.len < 2) return null;
                source_arity = source_items.len;
                for (source_items, 0..) |pat_id, index| {
                    const pat = self.pass.program.getPat(pat_id);
                    if (pat.data != .bind) return null;
                    const local = pat.data.bind;
                    if (localUseCountInExpr(self.pass.program, local, let_.rest) == 0) continue;
                    try kept_indices.append(self.pass.allocator, @intCast(index));
                    try kept_params.append(self.pass.allocator, .{ .local = local, .ty = pat.ty });
                }
            },
            .bind => |local| {
                const loop_type = self.pass.program.types.get(loop_expr.ty);
                if (loop_type != .tuple) return null;
                const type_span = loop_type.tuple;
                const source_tys = try GuardedList.dupe(self.pass.allocator, Type.TypeId, self.pass.program.types.span(type_span));
                aggregate_tys = source_tys;
                if (source_tys.len < 2) return null;
                source_arity = source_tys.len;

                const used = try self.pass.allocator.alloc(bool, source_arity);
                defer self.pass.allocator.free(used);
                @memset(used, false);
                if (!collectTupleLocalDemandInExpr(self.pass.program, local, let_.rest, used)) return null;

                const locals = try self.pass.allocator.alloc(?Ast.LocalId, source_arity);
                selected_locals = locals;
                @memset(locals, null);
                aggregate_local = local;
                for (source_tys, used, 0..) |ty, is_used, index| {
                    if (!is_used) continue;
                    const selected_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                    locals[index] = selected_local;
                    try kept_indices.append(self.pass.allocator, @intCast(index));
                    try kept_params.append(self.pass.allocator, .{ .local = selected_local, .ty = ty });
                }
            },
            .wildcard,
            .as,
            .record,
            .list,
            .tag,
            .nominal,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .str_pattern,
            => return null,
        }
        if (kept_indices.items.len == 0 or kept_indices.items.len == source_arity) return null;
        var selected_rest = let_.rest;
        if (aggregate_local) |local| {
            const source_tys = aggregate_tys orelse Common.invariant("loop exit selection had no source tuple types");
            const locals = selected_locals orelse Common.invariant("loop exit selection had no selected locals");
            const items = try self.arena.allocator().alloc(Value, source_arity);
            for (source_tys, locals, 0..) |ty, maybe_local, index| {
                const item_expr = if (maybe_local) |selected_local|
                    try self.addExpr(.{ .ty = ty, .data = .{ .local = selected_local } })
                else
                    try self.addExpr(.{ .ty = ty, .data = .@"unreachable" });
                items[index] = .{ .expr = item_expr };
            }

            const change_start = self.subst.watermark();
            errdefer self.subst.restore(change_start);
            try self.subst.put(self.pass.program, local, .{ .tuple = .{
                .ty = loop_expr.ty,
                .items = items,
            } });
            selected_rest = try self.cloneExpr(let_.rest);
            self.subst.restore(change_start);
        }

        const kept = try self.arena.allocator().dupe(u32, kept_indices.items);
        if (kept_params.items.len == 1) {
            const selected = kept_params.items[0];
            const selection = LoopExitSelection{
                .source_arity = source_arity,
                .kept_indices = kept,
                .result_ty = selected.ty,
                .transfer = .break_value,
            };
            const selected_loop = try self.cloneLoopWithSelectedExit(selected.ty, loop, selection);
            const bind = try self.pass.program.addPat(.{ .ty = selected.ty, .data = .{ .bind = selected.local } });
            return try self.addExpr(.{ .ty = self.pass.program.getExpr(selected_rest).ty, .data = .{ .let_ = .{
                .bind = bind,
                .value = selected_loop,
                .rest = selected_rest,
                .comptime_site = let_.comptime_site,
            } } });
        }

        const result_ty = self.pass.program.getExpr(selected_rest).ty;
        const target = self.pass.freshJoinPoint();
        var exit_sites = std.ArrayList(Ast.ExprId).empty;
        defer exit_sites.deinit(self.pass.allocator);
        const selection = LoopExitSelection{
            .source_arity = source_arity,
            .kept_indices = kept,
            .result_ty = result_ty,
            .transfer = .{ .jump = .{
                .target = target,
                .sites = &exit_sites,
            } },
        };
        const remainder = try self.cloneLoopWithSelectedExit(result_ty, loop, selection);

        if (exit_sites.items.len == 0) return null;
        if (exit_sites.items.len == 1 and !exprContainsFreeLoopControl(self.pass.program, selected_rest, 0)) {
            try self.inlineLoopExitAtSite(exit_sites.items[0], kept_params.items, selected_rest, target);
            return remainder;
        }

        return try self.addExpr(.{ .ty = result_ty, .data = .{ .join_point = .{
            .id = target,
            .params = try self.pass.program.addTypedLocalSpan(kept_params.items),
            .body = selected_rest,
            .remainder = remainder,
        } } });
    }

    fn cloneLoopWithSelectedExit(
        self: *Cloner,
        ty: Type.TypeId,
        loop: anytype,
        selection: LoopExitSelection,
    ) Common.LowerError!Ast.ExprId {
        var bindings: BindingChain = .{};
        const value = try self.cloneLoopValue(ty, loop, &bindings, selection);
        return try self.wrapBindings(bindings, try self.materialize(value));
    }

    fn cloneLoopBody(
        self: *Cloner,
        body: Ast.ExprId,
        selection: ?LoopExitSelection,
    ) Common.LowerError!Ast.ExprId {
        try self.loop_exit_stack.append(self.pass.allocator, selection);
        defer _ = self.loop_exit_stack.pop();
        return try self.cloneExpr(body);
    }

    fn currentLoopExitSelection(self: *Cloner) ?LoopExitSelection {
        if (self.loop_exit_stack.items.len == 0) return null;
        return self.loop_exit_stack.items[self.loop_exit_stack.items.len - 1];
    }

    /// The live site list of the active selection that owns this jump target,
    /// if any. Exit-transfer jumps are minted in the clone's own id space, so a
    /// jump to a selection's target can only be one of that selection's sites
    /// being cloned again.
    fn selectedExitJumpSites(self: *Cloner, target: Ast.JoinPointId) ?*std.ArrayList(Ast.ExprId) {
        var index = self.loop_exit_stack.items.len;
        while (index > 0) {
            index -= 1;
            const selection = self.loop_exit_stack.items[index] orelse continue;
            switch (selection.transfer) {
                .break_value => {},
                .jump => |jump_transfer| if (jump_transfer.target == target) return jump_transfer.sites,
            }
        }
        return null;
    }

    fn cloneSelectedLoopExit(
        self: *Cloner,
        break_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        selection: LoopExitSelection,
    ) Common.LowerError!Ast.ExprId {
        var bindings: BindingChain = .{};
        const exit_value = try self.cloneExprValueDemandingShapeInto(value_expr, &bindings);
        const tuple = tupleFromValue(exit_value) orelse Common.invariant("selected loop exit did not carry compiler-generated tuple state");
        if (tuple.items.len != selection.source_arity) {
            Common.invariant("selected loop exit tuple arity differed from its source ABI");
        }

        const projected = switch (selection.transfer) {
            .break_value => blk: {
                if (selection.kept_indices.len != 1) Common.invariant("direct loop exit selection did not contain one value");
                const projected_expr = try self.addExpr(.{
                    .ty = break_ty,
                    .data = .{ .break_ = try self.materialize(tuple.items[selection.kept_indices[0]]) },
                });
                try self.selected_loop_exit_tys.put(projected_expr, selection.result_ty);
                break :blk projected_expr;
            },
            .jump => |jump_transfer| blk: {
                const args = try self.pass.allocator.alloc(Ast.ExprId, selection.kept_indices.len);
                defer self.pass.allocator.free(args);
                for (selection.kept_indices, args) |index, *out| out.* = try self.materialize(tuple.items[index]);
                const jump = try self.addExpr(.{
                    .ty = break_ty,
                    .data = .{ .jump = .{
                        .target = jump_transfer.target,
                        .args = try self.pass.program.addExprSpan(args),
                    } },
                });
                try jump_transfer.sites.append(self.pass.allocator, jump);
                break :blk jump;
            },
        };

        return try self.wrapBindings(bindings, projected);
    }

    fn inlineLoopExitAtSite(
        self: *Cloner,
        site: Ast.ExprId,
        params: []const Ast.TypedLocal,
        continuation: Ast.ExprId,
        target: Ast.JoinPointId,
    ) Common.LowerError!void {
        const site_data = self.pass.program.getExpr(site).data;
        if (site_data != .jump) Common.invariant("loop exit site was not a jump");
        const jump = site_data.jump;
        if (jump.target != target) Common.invariant("loop exit site targeted a different continuation");
        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(jump.args));
        defer self.pass.allocator.free(args);
        if (args.len != params.len) Common.invariant("loop exit argument count differed from continuation parameter count");

        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);
        for (params, args) |param, arg| try self.subst.put(self.pass.program, param.local, .{ .expr = arg });
        const body = try self.cloneExpr(continuation);
        // The site is a diverging loop exit: lexically-following loop code is
        // only dead while it stays one. Inlining the continuation bare would
        // fall through into that code and discard the result, so the site
        // becomes a break carrying the continuation's value out of the loop.
        self.pass.program.setExprData(site, .{ .break_ = body });
    }

    /// Dissolve a binding while retaining every opaque leaf in the strict
    /// chain owned by the binding's original position. No work is discarded or
    /// commuted: naming the leaves once makes the structured value reusable
    /// without requiring purity, termination, or speculatability.
    fn bindPatToPositionedReusableValue(
        self: *Cloner,
        pat_id: Ast.PatId,
        source_value: Ast.ExprId,
        recursive: bool,
        value: Value,
        bindings: *BindingChain,
    ) Common.LowerError!bool {
        const pat = self.pass.program.getPat(pat_id);
        const self_referential = if (pat.data == .bind)
            localUseCountInExpr(self.pass.program, pat.data.bind, source_value) != 0
        else
            recursive;
        if (self_referential) return false;

        const bindings_before = bindings.mark();
        const change_before = self.subst.watermark();
        const reusable = try self.makeReusableForMatch(value, bindings);
        if (try self.bindPatToReusableValue(pat_id, reusable) != .match) {
            self.subst.restore(change_before);
            bindings.rewind(bindings_before);
            return false;
        }
        return true;
    }

    fn cloneLet(self: *Cloner, let_: anytype) Common.LowerError!Ast.ExprData {
        var bindings: BindingChain = .{};
        const value = try self.cloneLetValue(let_, &bindings);
        const expr = try self.wrapBindings(bindings, try self.materialize(value));
        return self.pass.program.getExpr(expr).data;
    }

    fn caseExprFromValue(self: *Cloner, value: Value) ?Ast.ExprId {
        const candidate = switch (value) {
            .expr => |expr| expr,
            .static_data_candidate => |static_candidate| switch (static_candidate.runtime.*) {
                .expr => |runtime| runtime,
                .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => return null,
            },
            .tag, .record, .tuple, .nominal, .callable => return null,
        };
        const candidate_data = self.pass.program.getExpr(candidate).data;
        if (candidate_data != .if_ and candidate_data != .match_) return null;
        return candidate;
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
    fn cloneLetOfCase(self: *Cloner, let_: anytype, value_expr: Ast.ExprId) Common.LowerError!?Ast.ExprData {
        const value_data = self.pass.program.getExpr(value_expr).data;
        if (value_data != .match_ and value_data != .if_) return null;

        const arm_count: usize = if (value_data == .match_)
            self.pass.program.branchSpan(value_data.match_.branches).len
        else
            self.pass.program.ifBranchSpan(value_data.if_.branches).len + 1;
        if (self.let_case_shape_growth.admit(arm_count) != .admitted) {
            return try self.cloneLetOfCaseShared(let_, value_expr);
        }

        const arena = self.arena.allocator();
        const value_ty = self.pass.program.getExpr(value_expr).ty;
        const rest_ty = self.pass.program.getExpr(let_.rest).ty;

        // The probe stands for "this arm's result value" while an arm clones
        // the dispatch: each arm substitutes it with its own known value.
        const probe = try self.pass.program.addLocal(self.pass.symbols.fresh(), value_ty);
        const probe_ref = try self.addExpr(.{ .ty = value_ty, .data = .{ .local = probe } });

        const joins = try self.letCaseJoinPlan(let_, arena);
        const dispatch = try self.letCaseDispatchExpr(let_, joins, probe_ref, rest_ty);

        var build = LetCaseBuild{ .joins = joins };
        const frame_index = self.let_case_builds.items.len;
        try self.let_case_builds.append(self.pass.allocator, &build);
        defer self.let_case_builds.shrinkRetainingCapacity(frame_index);

        const case_data: Ast.ExprData = switch (value_data) {
            .match_ => |match| blk: {
                const branches = try GuardedList.dupe(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(match.branches));
                defer self.pass.allocator.free(branches);
                const rewritten = try self.pass.allocator.alloc(Ast.Branch, branches.len);
                defer self.pass.allocator.free(rewritten);
                for (branches, 0..) |branch, index| {
                    const change_start = self.subst.watermark();
                    try self.shadowPatLocals(branch.pat);
                    try self.shadowStmtSpanLocals(branch.bindings);
                    const body = (try self.cloneLetOfCaseArmBody(probe, dispatch, branch.body)) orelse {
                        self.subst.restore(change_start);
                        return null;
                    };
                    self.subst.restore(change_start);
                    rewritten[index] = .{
                        .pat = branch.pat,
                        .bindings = branch.bindings,
                        .guard = branch.guard,
                        .body = body,
                    };
                }
                break :blk .{ .match_ = .{
                    .scrutinee = match.scrutinee,
                    .branches = try self.pass.program.addBranchSpan(rewritten),
                    .comptime_site = match.comptime_site,
                } };
            },
            .if_ => |if_| blk: {
                const branches = try GuardedList.dupe(self.pass.allocator, Ast.IfBranch, self.pass.program.ifBranchSpan(if_.branches));
                defer self.pass.allocator.free(branches);
                const rewritten = try self.pass.allocator.alloc(Ast.IfBranch, branches.len);
                defer self.pass.allocator.free(rewritten);
                for (branches, 0..) |branch, index| {
                    rewritten[index] = .{
                        .cond = branch.cond,
                        .body = (try self.cloneLetOfCaseArmBody(probe, dispatch, branch.body)) orelse return null,
                    };
                }
                const final_else = (try self.cloneLetOfCaseArmBody(probe, dispatch, if_.final_else)) orelse return null;
                break :blk .{ .if_ = .{
                    .branches = try self.pass.program.addIfBranchSpan(rewritten),
                    .final_else = final_else,
                } };
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
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => unreachable,
        };

        // Wrap the rewritten case in its live join points, innermost last so
        // every jump site in the case sits inside each join's remainder.
        var result = case_data;
        var join_index = joins.len;
        while (join_index > 0) {
            join_index -= 1;
            const join = &joins[join_index];
            if (join.sites.items.len == 0) continue;
            const pieces = (try self.finalizeLetCaseJoin(join, rest_ty)) orelse continue;
            const remainder = try self.addExpr(.{ .ty = rest_ty, .data = result });
            result = .{ .join_point = .{
                .id = join.id,
                .params = pieces.params,
                .body = pieces.body,
                .remainder = remainder,
            } };
        }
        return result;
    }

    const let_case_shape_arm_budget: usize = 4096;

    /// The budget-exhausted shape: one join point whose single parameter is
    /// the branch-built value, with every already-cloned arm body threaded to
    /// it as a jump argument. Stores no copy of arm bodies or continuation,
    /// so it is safe at any recursion depth; it keeps no static value shapes.
    fn cloneLetOfCaseShared(self: *Cloner, let_: anytype, value_expr: Ast.ExprId) Common.LowerError!?Ast.ExprData {
        const value_data = self.pass.program.getExpr(value_expr).data;
        const value_ty = self.pass.program.getExpr(value_expr).ty;
        const rest_ty = self.pass.program.getExpr(let_.rest).ty;
        const join_param = try self.pass.program.addLocal(self.pass.symbols.fresh(), value_ty);
        const params = [_]Ast.TypedLocal{.{ .local = join_param, .ty = value_ty }};
        const param_expr = try self.addExpr(.{ .ty = value_ty, .data = .{ .local = join_param } });

        const change_start = self.subst.watermark();
        const bind = try self.clonePat(let_.bind, .bind_runtime);
        const rest = try self.cloneExpr(let_.rest);
        self.subst.restore(change_start);
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
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => unreachable,
        };

        return .{ .join_point = .{
            .id = join_id,
            .params = try self.pass.program.addTypedLocalSpan(&params),
            .body = continuation,
            .remainder = remainder,
        } };
    }

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
            if (localUseCountInExpr(self.pass.program, bind_local, let_.rest) != 1) break :dispatch_split;

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
    fn collectPatBinders(self: *Cloner, pat_id: Ast.PatId, arena: Allocator, out: *std.ArrayList(Ast.LocalId)) Common.LowerError!bool {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| try out.append(arena, local),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .as => |as| {
                if (!try self.collectPatBinders(as.pattern, arena, out)) return false;
                try out.append(arena, as.local);
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                for (0..fields.len) |index| {
                    if (!try self.collectPatBinders(GuardedList.at(fields, index).pattern, arena, out)) return false;
                }
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                for (0..pats.len) |index| {
                    if (!try self.collectPatBinders(GuardedList.at(pats, index), arena, out)) return false;
                }
            },
            .tag => |tag_pat| {
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                for (0..pats.len) |index| {
                    if (!try self.collectPatBinders(GuardedList.at(pats, index), arena, out)) return false;
                }
            },
            .nominal => |backing| {
                if (!try self.collectPatBinders(backing, arena, out)) return false;
            },
            .list, .str_pattern => return false,
        }
        return true;
    }

    /// Rewrite one arm of the case. The arm keeps its own statements and
    /// effects; its result value must be a known structure, which the cloned
    /// dispatch consumes. Returns null when the arm's value is opaque,
    /// declining the whole rewrite.
    ///
    /// `branch_body` is already-cloned output of the value clone, with fresh
    /// ids referenced nowhere else, so a block arm's statements are reused
    /// as they stand and only the tail expression is re-derived for its
    /// symbolic value. Re-cloning whole arm bodies here re-ran every nested
    /// rewrite inside them a second time, which compounded across nesting
    /// levels and drained the pass-wide growth budgets on copies that were
    /// then discarded.
    fn cloneLetOfCaseArmBody(self: *Cloner, probe: Ast.LocalId, dispatch: Ast.ExprId, branch_body: Ast.ExprId) Common.LowerError!?Ast.ExprId {
        const dispatch_ty = self.pass.program.getExpr(dispatch).ty;
        const branch_expr = self.pass.program.getExpr(branch_body);
        switch (branch_expr.data) {
            .block => |block| {
                // A branch-built or looping tail always derives an opaque
                // value and is never divergent: decline without re-deriving,
                // so the nested rewrites inside it do not rerun just to be
                // thrown away.
                switch (self.pass.program.getExpr(block.final_expr).data) {
                    .match_, .if_, .loop_ => return null,
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
                    .list,
                    .tuple,
                    .record,
                    .record_update,
                    .tag,
                    .nominal,
                    .block,
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
                    .break_,
                    .continue_,
                    .join_point,
                    .jump,
                    .return_,
                    .crash,
                    .comptime_branch_taken,
                    .comptime_exhaustiveness_failed,
                    .dbg,
                    .expect_err,
                    .expect,
                    => {},
                }

                const change_start = self.subst.watermark();

                var statements = std.ArrayList(Ast.StmtId).empty;
                defer statements.deinit(self.pass.allocator);
                const source = self.pass.program.stmtSpan(block.statements);
                for (0..GuardedList.borrowLen(source)) |index| {
                    try statements.append(self.pass.allocator, GuardedList.at(source, index));
                }

                const final = try self.cloneExprValue(block.final_expr);
                if (final.value == .expr) {
                    if (try self.cloneDivergentAtType(block.final_expr, dispatch_ty)) |divergent| {
                        self.subst.restore(change_start);
                        try self.appendBindingStmts(final.bindings, &statements);
                        return try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                            .statements = try self.pass.program.addStmtSpan(statements.items),
                            .final_expr = divergent,
                        } } });
                    }
                    self.subst.restore(change_start);
                    return null;
                }

                try self.subst.put(self.pass.program, probe, final.value);
                try self.appendBindingStmts(final.bindings, &statements);
                const rest = try self.cloneExpr(dispatch);
                self.subst.restore(change_start);

                return try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                    .statements = try self.pass.program.addStmtSpan(statements.items),
                    .final_expr = rest,
                } } });
            },
            .match_, .if_, .loop_ => return null,
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
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => {
                const branch = try self.cloneExprValue(branch_body);
                const change_start = self.subst.watermark();
                if (branch.value == .expr) {
                    if (try self.cloneDivergentAtType(branch_body, dispatch_ty)) |divergent| {
                        self.subst.restore(change_start);
                        return try self.wrapBindings(branch.bindings, divergent);
                    }
                    self.subst.restore(change_start);
                    return null;
                }
                try self.subst.put(self.pass.program, probe, branch.value);
                const rest = try self.wrapBindings(branch.bindings, try self.cloneExpr(dispatch));
                self.subst.restore(change_start);
                return rest;
            },
        }
    }

    fn cloneDivergentAtType(self: *Cloner, expr_id: Ast.ExprId, ty: Type.TypeId) Common.LowerError!?Ast.ExprId {
        const expr = self.pass.program.getExpr(expr_id);
        return switch (expr.data) {
            .@"unreachable" => try self.addExpr(.{ .ty = ty, .data = .@"unreachable" }),
            .crash => |msg| try self.addExpr(.{ .ty = ty, .data = .{ .crash = msg } }),
            .comptime_exhaustiveness_failed => |site| try self.addExpr(.{ .ty = ty, .data = .{ .comptime_exhaustiveness_failed = site } }),
            .return_ => |ret| try self.addExpr(.{ .ty = ty, .data = .{ .return_ = .{
                .value = try self.cloneExpr(ret.value),
                .target = ret.target,
            } } }),
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .static_data_candidate,
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
            .comptime_branch_taken,
            .dbg,
            .expect_err,
            .expect,
            => null,
        };
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

    /// Record a jump into an active let-of-case join: capture the symbolic
    /// value of every argument and emit a placeholder jump whose argument
    /// span is patched once the join's parameters are decided.
    fn captureLetCaseJump(self: *Cloner, ty: Type.TypeId, join: *LetCaseJoin, jump: Ast.JumpExpr) Common.LowerError!Ast.ExprId {
        const arena = self.arena.allocator();
        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(jump.args));
        defer self.pass.allocator.free(args);
        const values = try arena.alloc(Value, args.len);
        var bindings: BindingChain = .{};
        for (args, 0..) |arg, index| {
            values[index] = try self.cloneExprValueDemandingShapeInto(arg, &bindings);
        }
        const placeholder = try self.addExpr(.{ .ty = ty, .data = .{ .jump = .{
            .target = join.id,
            .args = try self.pass.program.addExprSpan(&[_]Ast.ExprId{}),
        } } });
        try join.sites.append(arena, .{ .expr = placeholder, .bindings = bindings, .values = values });
        return placeholder;
    }

    const LetCaseJoinPieces = struct {
        params: Ast.Span(Ast.TypedLocal),
        body: Ast.ExprId,
    };

    /// Clone a join's continuation body directly at its only jump site,
    /// binding the continuation's binders to the site's symbolic values so
    /// the shared code keeps every statically known shape. The placeholder
    /// jump expression is overwritten with the cloned body.
    fn inlineLetCaseJoinAtSite(self: *Cloner, join: *LetCaseJoin, site: LetCaseJumpSite, rest_ty: Type.TypeId) Common.LowerError!void {
        const change_start = self.subst.watermark();
        const body = body: switch (join.binding) {
            .locals => |locals| {
                if (site.values.len != locals.len) {
                    Common.invariant("let-of-case jump site argument count differed from join binder count");
                }
                for (locals, site.values) |local, value| try self.subst.put(self.pass.program, local, value);
                const body = try self.cloneExpr(join.body);
                self.subst.restore(change_start);
                break :body body;
            },
            .pattern => |binding| {
                if (try self.bindPatToFlowValue(binding.pat, site.values[0])) {
                    const body = try self.cloneExpr(join.body);
                    self.subst.restore(change_start);
                    break :body body;
                }
                // The pattern could not consume the value's structure; keep
                // an ordinary let of the materialized value at the site.
                self.subst.restore(change_start);
                const value_expr = try self.materialize(site.values[0]);
                const pat_change_start = self.subst.watermark();
                const bind = try self.clonePat(binding.pat, .bind_runtime);
                const rest = try self.cloneExpr(join.body);
                self.subst.restore(pat_change_start);
                break :body try self.addExpr(.{ .ty = rest_ty, .data = .{ .let_ = .{
                    .bind = bind,
                    .value = value_expr,
                    .rest = rest,
                    .comptime_site = binding.comptime_site,
                } } });
            },
        };
        const wrapped = try self.wrapBindings(site.bindings, body);
        self.pass.program.setExprData(site.expr, self.pass.program.getExpr(wrapped).data);
    }

    /// Decompose a join's incoming values into shared parameters, clone the
    /// join's continuation body once against the rebuilt values, and patch
    /// every jump site with its leaf arguments. A join with exactly one jump
    /// site stores no continuation copy either way, so its body is cloned
    /// directly at the site—against the site's full symbolic values—and
    /// no join point is emitted (null).
    fn finalizeLetCaseJoin(self: *Cloner, join: *LetCaseJoin, rest_ty: Type.TypeId) Common.LowerError!?LetCaseJoinPieces {
        const arena = self.arena.allocator();
        const sites = join.sites.items;
        if (sites.len == 1) {
            try self.inlineLetCaseJoinAtSite(join, sites[0], rest_ty);
            return null;
        }
        const slot_count: usize = switch (join.binding) {
            .pattern => 1,
            .locals => |locals| locals.len,
        };

        var params: std.ArrayList(Ast.TypedLocal) = .empty;
        const site_args = try arena.alloc(std.ArrayList(Ast.ExprId), sites.len);
        for (site_args) |*list| list.* = .empty;

        const rebuilt = try arena.alloc(Value, slot_count);
        var budget = CodeGrowthBudget.init(let_case_join_leaf_budget);
        const slot_values = try self.pass.allocator.alloc(Value, sites.len);
        defer self.pass.allocator.free(slot_values);
        for (0..slot_count) |slot| {
            for (sites, 0..) |site, site_index| {
                if (site.values.len != slot_count) {
                    Common.invariant("let-of-case jump site argument count differed from join binder count");
                }
                slot_values[site_index] = site.values[slot];
            }
            rebuilt[slot] = try self.rebuildLetCaseJoinValue(slot_values, arena, &params, site_args, &budget);
        }

        const change_start = self.subst.watermark();
        const body = body: switch (join.binding) {
            .locals => |locals| {
                for (locals, rebuilt) |local, value| try self.subst.put(self.pass.program, local, value);
                const body = try self.cloneExpr(join.body);
                self.subst.restore(change_start);
                break :body body;
            },
            .pattern => |binding| {
                if (try self.bindPatToFlowValue(binding.pat, rebuilt[0])) {
                    const body = try self.cloneExpr(join.body);
                    self.subst.restore(change_start);
                    break :body body;
                }
                // The pattern could not consume the rebuilt structure; fall
                // back to one opaque parameter bound by an ordinary let.
                self.subst.restore(change_start);
                params.clearRetainingCapacity();
                for (site_args) |*list| list.clearRetainingCapacity();
                const param_ty = valueType(self.pass.program, sites[0].values[0]);
                const param_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), param_ty);
                try params.append(arena, .{ .local = param_local, .ty = param_ty });
                for (sites, site_args) |site, *list| {
                    try list.append(arena, try self.materialize(site.values[0]));
                }
                const param_expr = try self.addExpr(.{ .ty = param_ty, .data = .{ .local = param_local } });
                const pat_change_start = self.subst.watermark();
                const bind = try self.clonePat(binding.pat, .bind_runtime);
                const rest = try self.cloneExpr(join.body);
                self.subst.restore(pat_change_start);
                break :body try self.addExpr(.{ .ty = rest_ty, .data = .{ .let_ = .{
                    .bind = bind,
                    .value = param_expr,
                    .rest = rest,
                    .comptime_site = binding.comptime_site,
                } } });
            },
        };

        for (sites, site_args) |site, list| {
            const jump = try self.addExpr(.{ .ty = rest_ty, .data = .{ .jump = .{
                .target = join.id,
                .args = try self.pass.program.addExprSpan(list.items),
            } } });
            const wrapped = try self.wrapBindings(site.bindings, jump);
            self.pass.program.setExprData(site.expr, self.pass.program.getExpr(wrapped).data);
        }

        return .{
            .params = try self.pass.program.addTypedLocalSpan(params.items),
            .body = body,
        };
    }

    /// Node budget and parameter cap for decomposing one join's incoming
    /// values. Values can be compact graphs reached by combinatorially many
    /// paths (see `make_reusable_work_budget`), so the walk spends one shared
    /// budget per node and keeps any remaining sub-value as one opaque
    /// parameter when it runs out.
    const let_case_join_leaf_budget: u32 = 1024;
    const let_case_join_param_cap: usize = 64;

    /// Structure-decompose the values every site supplies for one binder
    /// slot. Where all sites agree on the same constructor skeleton, the
    /// skeleton is rebuilt over fresh parameter locals minted for its opaque
    /// leaves and each site's leaf expressions become its jump arguments; any
    /// disagreement (or an exhausted budget) makes that position one opaque
    /// parameter.
    fn rebuildLetCaseJoinValue(
        self: *Cloner,
        values: []const Value,
        arena: Allocator,
        params: *std.ArrayList(Ast.TypedLocal),
        site_args: []std.ArrayList(Ast.ExprId),
        budget: *CodeGrowthBudget,
    ) Common.LowerError!Value {
        if (values.len == 0) Common.invariant("let-of-case join had no jump sites to decompose");
        structured: {
            if (params.items.len >= let_case_join_param_cap) break :structured;
            if (budget.admit(1) != .admitted) break :structured;
            switch (values[0]) {
                .expr, .static_data_candidate => break :structured,
                .tag => |first| {
                    for (values[1..]) |other| {
                        if (other != .tag) break :structured;
                        const other_tag = other.tag;
                        if (other_tag.ty != first.ty) break :structured;
                        if (!self.pass.program.names.tagLabelTextEql(other_tag.name, first.name)) break :structured;
                        if (other_tag.payloads.len != first.payloads.len) break :structured;
                    }
                    const payloads = try arena.alloc(Value, first.payloads.len);
                    const children = try self.pass.allocator.alloc(Value, values.len);
                    defer self.pass.allocator.free(children);
                    for (0..first.payloads.len) |index| {
                        for (values, children) |value, *child| child.* = value.tag.payloads[index];
                        payloads[index] = try self.rebuildLetCaseJoinValue(children, arena, params, site_args, budget);
                    }
                    return .{ .tag = .{ .ty = first.ty, .name = first.name, .payloads = payloads } };
                },
                .record => |first| {
                    for (values[1..]) |other| {
                        if (other != .record) break :structured;
                        const other_record = other.record;
                        if (other_record.ty != first.ty) break :structured;
                        if (other_record.fields.len != first.fields.len) break :structured;
                        for (other_record.fields, first.fields) |other_field, first_field| {
                            if (!self.pass.program.names.recordFieldLabelTextEql(other_field.name, first_field.name)) break :structured;
                        }
                    }
                    const fields = try arena.alloc(FieldValue, first.fields.len);
                    const children = try self.pass.allocator.alloc(Value, values.len);
                    defer self.pass.allocator.free(children);
                    for (0..first.fields.len) |index| {
                        for (values, children) |value, *child| child.* = value.record.fields[index].value;
                        fields[index] = .{
                            .name = first.fields[index].name,
                            .value = try self.rebuildLetCaseJoinValue(children, arena, params, site_args, budget),
                        };
                    }
                    return .{ .record = .{ .ty = first.ty, .fields = fields } };
                },
                .tuple => |first| {
                    for (values[1..]) |other| {
                        if (other != .tuple) break :structured;
                        const other_tuple = other.tuple;
                        if (other_tuple.ty != first.ty) break :structured;
                        if (other_tuple.items.len != first.items.len) break :structured;
                    }
                    const items = try arena.alloc(Value, first.items.len);
                    const children = try self.pass.allocator.alloc(Value, values.len);
                    defer self.pass.allocator.free(children);
                    for (0..first.items.len) |index| {
                        for (values, children) |value, *child| child.* = value.tuple.items[index];
                        items[index] = try self.rebuildLetCaseJoinValue(children, arena, params, site_args, budget);
                    }
                    return .{ .tuple = .{ .ty = first.ty, .items = items } };
                },
                .nominal => |first| {
                    for (values[1..]) |other| {
                        if (other != .nominal) break :structured;
                        const other_nominal = other.nominal;
                        if (other_nominal.ty != first.ty) break :structured;
                    }
                    const children = try self.pass.allocator.alloc(Value, values.len);
                    defer self.pass.allocator.free(children);
                    for (values, children) |value, *child| child.* = value.nominal.backing.*;
                    const backing = try arena.create(Value);
                    backing.* = try self.rebuildLetCaseJoinValue(children, arena, params, site_args, budget);
                    return .{ .nominal = .{ .ty = first.ty, .backing = backing } };
                },
                .callable => |first| {
                    var iterator_step = first.iterator_step;
                    for (values[1..]) |other| {
                        if (other != .callable) break :structured;
                        const other_callable = other.callable;
                        if (other_callable.ty != first.ty) break :structured;
                        if (other_callable.fn_id != first.fn_id) break :structured;
                        if (other_callable.captures.len != first.captures.len) break :structured;
                        iterator_step = iterator_step and other_callable.iterator_step;
                        for (other_callable.captures, first.captures) |other_capture, first_capture| {
                            if (other_capture.id != first_capture.id) break :structured;
                        }
                    }
                    const captures = try arena.alloc(CaptureValue, first.captures.len);
                    const children = try self.pass.allocator.alloc(Value, values.len);
                    defer self.pass.allocator.free(children);
                    for (0..first.captures.len) |index| {
                        for (values, children) |value, *child| child.* = value.callable.captures[index].value;
                        captures[index] = .{
                            .id = first.captures[index].id,
                            .value = try self.rebuildLetCaseJoinValue(children, arena, params, site_args, budget),
                        };
                    }
                    return .{ .callable = .{
                        .ty = first.ty,
                        .fn_id = first.fn_id,
                        .captures = captures,
                        .iterator_step = iterator_step,
                    } };
                },
            }
        }
        // Opaque leaf: one parameter; each site materializes its own value.
        const leaf_ty = valueType(self.pass.program, values[0]);
        const param_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), leaf_ty);
        try params.append(arena, .{ .local = param_local, .ty = leaf_ty });
        for (values, site_args) |value, *list| {
            try list.append(arena, try self.materialize(value));
        }
        return .{ .expr = try self.addExpr(.{ .ty = leaf_ty, .data = .{ .local = param_local } }) };
    }

    fn cloneLoopValue(
        self: *Cloner,
        ty: Type.TypeId,
        loop: anytype,
        bindings: *BindingChain,
        exit_selection: ?LoopExitSelection,
    ) Common.LowerError!Value {
        const params = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(loop.params));
        defer self.pass.allocator.free(params);
        const initial_values = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(loop.initial_values));
        defer self.pass.allocator.free(initial_values);
        if (params.len != initial_values.len) Common.invariant("loop parameter count differed from initial value count");

        const values = try self.pass.allocator.alloc(Value, initial_values.len);
        defer self.pass.allocator.free(values);
        const shapes = try self.arena.allocator().alloc(Shape, initial_values.len);
        var has_constructor = false;
        // An initial value may forward-reference the loop's own params: an
        // `uninitialized_payload` argument names the flag param that carries
        // its initialized-ness at the loop head. The emitted params do not
        // exist yet, so pin those references to the source param ids while
        // cloning; each emission below retargets them to its fresh params.
        const forward_start = self.subst.watermark();
        for (params) |param| try self.shadowLocal(param.local);
        for (initial_values, 0..) |initial, index| {
            values[index] = try self.cloneExprValueDemandingShapeInto(initial, bindings);
            switch (try self.pass.shapeFromValue(values[index])) {
                .proven => |shape| {
                    shapes[index] = shape;
                    has_constructor = true;
                },
                .disproven, .unknown_budget_exhausted => {
                    shapes[index] = .{ .any = valueType(self.pass.program, values[index]) };
                },
            }
        }
        self.subst.restore(forward_start);

        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);

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
        const carried_identities = try self.pass.allocator.alloc(?BinderIdentity, initial_values.len);
        defer self.pass.allocator.free(carried_identities);
        for (initial_values, carried_identities) |initial, *identity| {
            identity.* = null;
            const initial_local = localExpr(self.pass.program, initial) orelse continue;
            if (exprReferencesLocal(self.pass.program, loop.body, initial_local)) continue;
            identity.* = try self.subst.dropCarriedBinder(self.pass.program, initial);
        }

        // Mark each carried binder so a state-merged or reassigned copy bound in
        // a nested `let` while cloning the body floats its value past that let's
        // restore, letting the back edge resolve it through its binder.
        for (carried_identities) |identity| {
            if (identity) |carried| try self.subst.markLoopCarried(carried);
        }
        defer for (carried_identities) |identity| {
            if (identity) |carried| self.subst.unmarkLoopCarried(carried);
        };

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
        if (exprContainsReturn(self.pass.program, loop.body)) has_constructor = false;
        while (has_constructor) {
            var new_params = std.ArrayList(Ast.TypedLocal).empty;
            defer new_params.deinit(self.pass.allocator);

            var new_initials = std.ArrayList(Ast.ExprId).empty;
            defer new_initials.deinit(self.pass.allocator);

            const split_start = self.subst.watermark();
            var forward_sources = std.ArrayList(Ast.LocalId).empty;
            defer forward_sources.deinit(self.pass.allocator);
            var forward_finals = std.ArrayList(Ast.LocalId).empty;
            defer forward_finals.deinit(self.pass.allocator);
            for (params, shapes, values, carried_identities) |param, shape, value, carried_identity| {
                const leaf_start = new_params.items.len;
                const param_value = try self.valueFromShapeArgs(shape, &new_params);
                try self.subst.put(self.pass.program, param.local, param_value);
                if (carried_identity) |identity| try self.subst.putLoopCarried(identity, param_value);
                try self.appendExprsFromValue(shape, value, &new_initials);
                // An `.any` slot keeps its whole value in one param, so a
                // forward reference to the source param means this param.
                if (shape == .any) {
                    try forward_sources.append(self.pass.allocator, param.local);
                    try forward_finals.append(self.pass.allocator, new_params.items[leaf_start].local);
                }
            }

            try self.loop_stack.append(self.pass.allocator, .{
                .values = shapes,
                .any_demoted = false,
            });
            const body = try self.cloneLoopBody(loop.body, exit_selection);
            const frame = self.loop_stack.pop() orelse Common.invariant("loop stack underflow after split attempt");

            if (!frame.any_demoted) {
                try self.retargetLoopForwardConditions(new_initials.items, forward_sources.items, forward_finals.items);
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
                    .params = try self.pass.program.addTypedLocalSpan(new_params.items),
                    .initial_values = try self.pass.program.addExprSpan(new_initials.items),
                    .body = body,
                } } }) };
            }

            self.subst.restore(split_start);
            // Back edges demoted their unsupplied leaves in place. Any slot that
            // still carries constructor structure is worth another split attempt.
            has_constructor = false;
            for (shapes) |shape| {
                if (shape != .any) has_constructor = true;
            }
        }

        const whole_shapes = try self.arena.allocator().alloc(Shape, params.len);
        for (params, 0..) |param, index| whole_shapes[index] = .{ .any = param.ty };

        const initial_exprs = try self.pass.allocator.alloc(Ast.ExprId, values.len);
        defer self.pass.allocator.free(initial_exprs);
        for (values, initial_exprs) |value, *expr| expr.* = try self.materialize(value);

        const whole_params = try self.pass.allocator.alloc(Ast.TypedLocal, params.len);
        defer self.pass.allocator.free(whole_params);
        const forward_sources = try self.pass.allocator.alloc(Ast.LocalId, params.len);
        defer self.pass.allocator.free(forward_sources);
        const forward_finals = try self.pass.allocator.alloc(Ast.LocalId, params.len);
        defer self.pass.allocator.free(forward_finals);
        for (params, whole_params, forward_sources, forward_finals, carried_identities) |param, *whole, *source, *final, carried_identity| {
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
        try self.retargetLoopForwardConditions(initial_exprs, forward_sources, forward_finals);
        try self.loop_stack.append(self.pass.allocator, .{
            .values = whole_shapes,
            .any_demoted = false,
        });
        const body = try self.cloneLoopBody(loop.body, exit_selection);
        if (self.loop_stack.pop() == null) Common.invariant("loop stack underflow after whole-state body clone");
        return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
            .params = try self.pass.program.addTypedLocalSpan(whole_params),
            .initial_values = try self.pass.program.addExprSpan(initial_exprs),
            .body = body,
        } } }) };
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

    /// A block whose statements all dissolve—each binding retains its strict
    /// work in the block's source-ordered binding chain, and each discarded
    /// expression is speculatable—is transparent to value flow: its result keeps the final
    /// expression's structure. A statement that must stay a statement (an
    /// effect, a runtime destructure, control flow) pins the block, which
    /// then materializes as written. Returns null on a pinned block with all
    /// speculative work undone.
    fn cloneBlockValue(
        self: *Cloner,
        block: anytype,
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        // The block-final position is the capability that makes an
        // `unreachable` marker valid. Treating this block as transparent would
        // let the marker escape into an ordinary expression position (for
        // example, the rest of a synthesized `let`).
        if (self.pass.program.getExpr(block.final_expr).data == .@"unreachable") return null;

        const change_start = self.subst.watermark();
        var block_bindings: BindingChain = .{};

        const source = try GuardedList.dupe(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        for (source) |stmt_id| {
            const stmt = self.pass.program.getStmt(stmt_id);
            const let_ = switch (stmt) {
                .let_ => |let_| let_,
                // Preserve a discarded expression's opaque work as strict
                // bindings at this exact statement position. Only its
                // structurally work-free result is discarded.
                .expr => |stmt_expr| {
                    const discarded = try self.cloneExprValueInto(stmt_expr, &block_bindings);
                    _ = try self.makeReusableForMatch(discarded, &block_bindings);
                    continue;
                },
                .uninitialized, .expect, .dbg, .return_, .crash => {
                    self.subst.restore(change_start);
                    return null;
                },
            };
            const value = try self.cloneExprValueInto(let_.value, &block_bindings);
            if (self.caseExprFromValue(value) != null) {
                self.subst.restore(change_start);
                return null;
            }
            if (try self.bindPatToReusableValue(let_.pat, value) == .match) continue;
            if (!try self.bindPatToPositionedReusableValue(let_.pat, let_.value, let_.recursive, value, &block_bindings)) {
                self.subst.restore(change_start);
                return null;
            }
        }

        const final = try self.cloneExprValueInto(block.final_expr, &block_bindings);
        self.subst.restore(change_start);
        bindings.appendChain(block_bindings);
        return final;
    }

    fn cloneBlock(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Ast.ExprId {
        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);

        const terminated = self.pass.program.getExpr(block.final_expr).data == .@"unreachable";

        const source = try GuardedList.dupe(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        var statements = std.ArrayList(Ast.StmtId).empty;
        defer statements.deinit(self.pass.allocator);
        for (source, 0..) |stmt, index| {
            // A binding statement is a let expression over the block's tail.
            // Cloning it as one lets a branch-built value sink the tail into
            // the branches, where each branch's constructor is known.
            switch (self.pass.program.getStmt(stmt)) {
                .let_ => |let_| if (!let_.recursive and
                    (!terminated or
                        (self.pass.program.getExpr(let_.value).data == .loop_ and
                            (self.pass.tuplePatternIsPartiallyUsedInBlockTail(
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
                                )))))
                {
                    const tail = try self.pass.program.addExpr(.{ .ty = ty, .data = .{ .block = .{
                        .statements = try self.pass.program.addStmtSpan(source[index + 1 ..]),
                        .final_expr = block.final_expr,
                    } } });
                    const synthetic = try self.pass.program.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                        .bind = let_.pat,
                        .value = let_.value,
                        .rest = tail,
                        .comptime_site = let_.comptime_site,
                    } } });
                    return try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
                        .statements = try self.pass.program.addStmtSpan(statements.items),
                        .final_expr = try self.cloneExpr(synthetic),
                    } } });
                },
                .uninitialized, .expr, .expect, .dbg, .return_, .crash => {},
            }
            const cloned = try self.cloneStmt(stmt);
            try self.appendBindingStmts(cloned.bindings, &statements);
            if (cloned.stmt) |cloned_stmt| try statements.append(self.pass.allocator, cloned_stmt);
        }

        return try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(statements.items),
            .final_expr = try self.cloneExpr(block.final_expr),
        } } });
    }

    fn cloneContinue(self: *Cloner, ty: Type.TypeId, continue_: anytype) Common.LowerError!Ast.ExprId {
        const frame_count = self.loop_stack.items.len;
        if (frame_count == 0) return try self.addExpr(.{ .ty = ty, .data = .{ .continue_ = .{
            .values = try self.cloneExprSpan(continue_.values),
        } } });
        const loop = self.loop_stack.items[frame_count - 1];
        const values = self.pass.program.exprSpan(continue_.values);
        const source_values = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, values);
        defer self.pass.allocator.free(source_values);
        if (source_values.len != loop.values.len) {
            Common.invariantFmt("continue value count differed from specialized loop pattern: continue has {d} values, {d} frames with innermost expecting {d}", .{ source_values.len, frame_count, loop.values.len });
        }

        var new_values = std.ArrayList(Ast.ExprId).empty;
        defer new_values.deinit(self.pass.allocator);
        var bindings: BindingChain = .{};

        for (loop.values, source_values, 0..) |shape, value_expr, slot_index| {
            const value = try self.cloneExprValueInto(value_expr, &bindings);
            const supplied = try self.supplyLoopSlotLeaves(shape, value, &new_values);
            if (supplied.demoted) {
                // This back edge could not supply some of the slot's entry-shape
                // leaves. Record the per-leaf demotion so the split attempt
                // carries those leaves as runtime scalars while their siblings
                // stay split; the values emitted here belong to a clone the
                // attempt discards and retries.
                self.loop_stack.items[frame_count - 1].values[slot_index] = supplied.shape;
                self.loop_stack.items[frame_count - 1].any_demoted = true;
            }
        }

        const continued = try self.addExpr(.{ .ty = ty, .data = .{ .continue_ = .{
            .values = try self.pass.program.addExprSpan(new_values.items),
        } } });
        return try self.wrapBindings(bindings, continued);
    }

    fn cloneCallProc(
        self: *Cloner,
        ty: Type.TypeId,
        call: @import("../monotype/ast.zig").CallProc,
    ) Common.LowerError!Ast.ExprId {
        if (call.is_cold) {
            return try self.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
                .callee = call.callee,
                .args = try self.cloneExprSpan(call.args),
                .iterator_procedure = call.iterator_procedure,
                .captures = try self.cloneCaptureOperandSpan(call.captures),
                .is_cold = true,
            } } });
        }

        const callee = Ast.localDirectCallee(call) orelse return try self.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
            .callee = call.callee,
            .args = try self.cloneExprSpan(call.args),
            .iterator_procedure = call.iterator_procedure,
            .captures = try self.cloneCaptureOperandSpan(call.captures),
            .is_cold = call.is_cold,
        } } });
        const raw = @intFromEnum(callee);
        if (self.rewrite_call_patterns and raw < self.pass.plans.len) {
            const source_args = self.pass.program.exprSpan(call.args);
            const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, source_args);
            defer self.pass.allocator.free(args);

            const values = try self.pass.allocator.alloc(Value, args.len);
            defer self.pass.allocator.free(values);
            const analyzed = try self.pass.allocator.alloc(ClonedValue, args.len);
            defer self.pass.allocator.free(analyzed);
            const callee_uses = self.pass.plans[raw].used_args;
            for (args, 0..) |arg, index| {
                analyzed[index] = if (callee_uses[index])
                    try self.cloneExprValueDemandingShape(arg)
                else
                    try self.cloneExprValue(arg);
                values[index] = analyzed[index].value;
            }
            try self.pass.ensureCallPatternForValues(callee, values);

            // Every outcome below reads the argument values produced above
            // rather than cloning the source arguments again: a second clone
            // re-descends every argument, so a nested call chain (e.g. a long
            // `+` sum, or a chain of builder-method calls) would clone each
            // level twice and expand exponentially with depth. The reuse is
            // also required for correctness when producing values with binding
            // chains: those chains must be placed exactly once before the call.
            for (self.pass.plans[raw].specs.items) |spec| {
                if (!callPatternMatchesValues(self.pass.program, spec.pattern, values)) continue;

                var rewritten_args = std.ArrayList(Ast.ExprId).empty;
                defer rewritten_args.deinit(self.pass.allocator);
                var bindings: BindingChain = .{};
                for (spec.pattern.args, values, analyzed) |shape, value, cloned| {
                    bindings.appendChain(cloned.bindings);
                    try self.appendExprsFromValue(shape, value, &rewritten_args);
                }
                const specialized = try self.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
                    .callee = .{ .lifted = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before cloning calls") },
                    .args = try self.pass.program.addExprSpan(rewritten_args.items),
                    .iterator_procedure = call.iterator_procedure,
                    .captures = try self.cloneCaptureOperandSpan(call.captures),
                    .is_cold = call.is_cold,
                } } });
                return try self.wrapBindings(bindings, specialized);
            }

            // No specialization matched, so the call stays residual.
            const residual_args = try self.pass.allocator.alloc(Ast.ExprId, values.len);
            defer self.pass.allocator.free(residual_args);
            var bindings: BindingChain = .{};
            for (values, analyzed, 0..) |value, cloned, index| {
                bindings.appendChain(cloned.bindings);
                residual_args[index] = try self.materialize(value);
            }
            const residual = try self.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
                .callee = call.callee,
                .args = try self.pass.program.addExprSpan(residual_args),
                .iterator_procedure = call.iterator_procedure,
                .captures = try self.cloneCaptureOperandSpan(call.captures),
                .is_cold = call.is_cold,
            } } });
            return try self.wrapBindings(bindings, residual);
        }
        return try self.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
            .callee = call.callee,
            .args = try self.cloneExprSpan(call.args),
            .iterator_procedure = call.iterator_procedure,
            .captures = try self.cloneCaptureOperandSpan(call.captures),
            .is_cold = call.is_cold,
        } } });
    }

    fn appendExprsFromValue(
        self: *Cloner,
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!void {
        const structural_value = if (value == .static_data_candidate) value.static_data_candidate.runtime.* else value;
        switch (shape) {
            .any => {
                try out.append(self.pass.allocator, try self.materialize(value));
            },
            .tag => |tag| {
                if (structural_value != .tag) Common.invariant("tag call pattern matched a non-tag value");
                const tag_value = structural_value.tag;
                for (tag.payloads, tag_value.payloads) |payload_shape, payload| {
                    try self.appendExprsFromValue(payload_shape, payload, out);
                }
            },
            .record => |record| {
                if (structural_value != .record) Common.invariant("record call pattern matched a non-record value");
                const record_value = structural_value.record;
                for (record.fields, record_value.fields) |field_shape, field| {
                    if (!self.pass.program.names.recordFieldLabelTextEql(field_shape.name, field.name)) Common.invariant("record call-pattern field order changed after matching");
                    try self.appendExprsFromValue(field_shape.shape, field.value, out);
                }
            },
            .tuple => |tuple| {
                if (structural_value != .tuple) Common.invariant("tuple call pattern matched a non-tuple value");
                const tuple_value = structural_value.tuple;
                for (tuple.items, tuple_value.items) |item_shape, item| {
                    try self.appendExprsFromValue(item_shape, item, out);
                }
            },
            .nominal => |nominal| {
                if (structural_value != .nominal) Common.invariant("nominal call pattern matched a non-nominal value");
                const nominal_value = structural_value.nominal;
                try self.appendExprsFromValue(nominal.backing.*, nominal_value.backing.*, out);
            },
            .callable => |callable| {
                if (structural_value != .callable) Common.invariant("callable call pattern matched a non-callable value");
                const callable_value = structural_value.callable;
                for (callable.captures, callable_value.captures) |capture_shape, capture_value| {
                    try self.appendExprsFromValue(capture_shape, capture_value.value, out);
                }
            },
        }
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
    fn supplyLoopSlotLeaves(
        self: *Cloner,
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!SuppliedSlot {
        if (shapeMatchesValue(self.pass.program, shape, value)) {
            try self.appendExprsFromValue(shape, value, out);
            return .{ .shape = shape, .demoted = false };
        }

        switch (shape) {
            .any => {
                try out.append(self.pass.allocator, try self.materialize(value));
                return .{ .shape = shape, .demoted = false };
            },
            .tag => |tag| {
                if (value != .tag) return try self.demoteLoopSlotLeaf(tag.ty, value, out);
                const value_tag = value.tag;
                if (!self.pass.program.names.tagLabelTextEql(value_tag.name, tag.name) or
                    !sameType(self.pass.program, tag.ty, value_tag.ty) or
                    value_tag.payloads.len != tag.payloads.len)
                {
                    return try self.demoteLoopSlotLeaf(tag.ty, value, out);
                }
                const payloads = try self.arena.allocator().alloc(Shape, tag.payloads.len);
                var demoted = false;
                for (tag.payloads, value_tag.payloads, 0..) |payload_shape, payload_value, index| {
                    const supplied = try self.supplyLoopSlotLeaves(payload_shape, payload_value, out);
                    payloads[index] = supplied.shape;
                    demoted = demoted or supplied.demoted;
                }
                return .{ .shape = .{ .tag = .{ .ty = tag.ty, .name = tag.name, .payloads = payloads } }, .demoted = demoted };
            },
            .record => |record| {
                switch (value) {
                    .record => |value_record| {
                        if (sameType(self.pass.program, record.ty, value_record.ty) and
                            value_record.fields.len == record.fields.len)
                        {
                            const fields = try self.arena.allocator().alloc(FieldShape, record.fields.len);
                            var demoted = false;
                            for (record.fields, value_record.fields, 0..) |field_shape, field_value, index| {
                                if (!self.pass.program.names.recordFieldLabelTextEql(field_shape.name, field_value.name)) return try self.demoteLoopSlotLeaf(record.ty, value, out);
                                const supplied = try self.supplyLoopSlotLeaves(field_shape.shape, field_value.value, out);
                                fields[index] = .{ .name = field_shape.name, .shape = supplied.shape };
                                demoted = demoted or supplied.demoted;
                            }
                            return .{ .shape = .{ .record = .{ .ty = record.ty, .fields = fields } }, .demoted = demoted };
                        }
                    },
                    .expr => |receiver| {
                        if (canReadFieldsFromExpr(self.pass.program, receiver)) {
                            const fields = try self.arena.allocator().alloc(FieldShape, record.fields.len);
                            var demoted = false;
                            for (record.fields, 0..) |field_shape, index| {
                                const field_expr = try self.addFieldAccessExpr(
                                    shapeType(field_shape.shape),
                                    receiver,
                                    field_shape.name,
                                );
                                const supplied = try self.supplyLoopSlotLeaves(field_shape.shape, .{ .expr = field_expr }, out);
                                fields[index] = .{ .name = field_shape.name, .shape = supplied.shape };
                                demoted = demoted or supplied.demoted;
                            }
                            return .{ .shape = .{ .record = .{ .ty = record.ty, .fields = fields } }, .demoted = demoted };
                        }
                    },
                    .static_data_candidate, .tag, .tuple, .nominal, .callable => {},
                }
                return try self.demoteLoopSlotLeaf(record.ty, value, out);
            },
            .tuple => |tuple| {
                switch (value) {
                    .tuple => |value_tuple| {
                        if (sameType(self.pass.program, tuple.ty, value_tuple.ty) and
                            value_tuple.items.len == tuple.items.len)
                        {
                            const items = try self.arena.allocator().alloc(Shape, tuple.items.len);
                            var demoted = false;
                            for (tuple.items, value_tuple.items, 0..) |item_shape, item_value, index| {
                                const supplied = try self.supplyLoopSlotLeaves(item_shape, item_value, out);
                                items[index] = supplied.shape;
                                demoted = demoted or supplied.demoted;
                            }
                            return .{ .shape = .{ .tuple = .{ .ty = tuple.ty, .items = items } }, .demoted = demoted };
                        }
                    },
                    .expr => |receiver| {
                        if (canReadFieldsFromExpr(self.pass.program, receiver)) {
                            const items = try self.arena.allocator().alloc(Shape, tuple.items.len);
                            var demoted = false;
                            for (tuple.items, 0..) |item_shape, index| {
                                const item_expr = try self.addExpr(.{ .ty = shapeType(item_shape), .data = .{ .tuple_access = .{
                                    .tuple = receiver,
                                    .elem_index = @as(u32, @intCast(index)),
                                } } });
                                const supplied = try self.supplyLoopSlotLeaves(item_shape, .{ .expr = item_expr }, out);
                                items[index] = supplied.shape;
                                demoted = demoted or supplied.demoted;
                            }
                            return .{ .shape = .{ .tuple = .{ .ty = tuple.ty, .items = items } }, .demoted = demoted };
                        }
                    },
                    .static_data_candidate, .tag, .record, .nominal, .callable => {},
                }
                return try self.demoteLoopSlotLeaf(tuple.ty, value, out);
            },
            .nominal => |nominal| {
                switch (value) {
                    .nominal => |value_nominal| {
                        if (sameType(self.pass.program, nominal.ty, value_nominal.ty)) {
                            const supplied = try self.supplyLoopSlotLeaves(nominal.backing.*, value_nominal.backing.*, out);
                            const backing = try self.arena.allocator().create(Shape);
                            backing.* = supplied.shape;
                            return .{ .shape = .{ .nominal = .{ .ty = nominal.ty, .backing = backing } }, .demoted = supplied.demoted };
                        }
                    },
                    .expr, .static_data_candidate, .tag, .record, .tuple, .callable => {},
                }
                return try self.demoteLoopSlotLeaf(nominal.ty, value, out);
            },
            .callable => |callable| {
                if (value != .callable) return try self.demoteLoopSlotLeaf(callable.ty, value, out);
                const value_callable = value.callable;
                if (!sameType(self.pass.program, callable.ty, value_callable.ty) or
                    !callableTargetMatches(self.pass.program, callable.fn_id, value_callable.fn_id) or
                    value_callable.captures.len != callable.captures.len)
                {
                    return try self.demoteLoopSlotLeaf(callable.ty, value, out);
                }
                const captures = try self.arena.allocator().alloc(Shape, callable.captures.len);
                var demoted = false;
                for (callable.captures, value_callable.captures, 0..) |capture_shape, capture_value, index| {
                    const supplied = try self.supplyLoopSlotLeaves(capture_shape, capture_value.value, out);
                    captures[index] = supplied.shape;
                    demoted = demoted or supplied.demoted;
                }
                return .{ .shape = .{ .callable = .{ .ty = callable.ty, .fn_id = callable.fn_id, .captures = captures } }, .demoted = demoted };
            },
        }
    }

    fn demoteLoopSlotLeaf(
        self: *Cloner,
        ty: Type.TypeId,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!SuppliedSlot {
        try out.append(self.pass.allocator, try self.materialize(value));
        return .{ .shape = .{ .any = ty }, .demoted = true };
    }

    fn cloneFieldAccessValue(self: *Cloner, ty: Type.TypeId, field: anytype, bindings: *BindingChain) Common.LowerError!Value {
        const receiver = try self.cloneExprValueDemandingShapeInto(field.receiver, bindings);
        if (field.segments.len == 0) Common.invariant("field access path had no segments");

        var prefix = receiver;
        var consumed: u32 = 0;
        while (consumed < field.segments.len) : (consumed += 1) {
            const segment = self.pass.program.fieldAccessSegmentAt(field.segments, consumed);
            prefix = fieldFromValue(self.pass.program, prefix, segment.field) orelse break;
        }
        if (consumed == field.segments.len) return prefix;

        const residual_segments: Ast.Span(Ast.FieldAccessSegment) = .{
            .start = field.segments.start + consumed,
            .len = field.segments.len - consumed,
        };
        return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = try self.materialize(prefix),
            .segments = residual_segments,
        } } }) };
    }

    fn cloneFieldAccess(self: *Cloner, ty: Type.TypeId, field: anytype) Common.LowerError!Ast.ExprId {
        var bindings: BindingChain = .{};
        const value = try self.cloneFieldAccessValue(ty, field, &bindings);
        return try self.wrapBindings(bindings, try self.materialize(value));
    }

    fn cloneTupleAccess(self: *Cloner, ty: Type.TypeId, access: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingShape(access.tuple);
        if (itemFromValue(receiver.value, access.elem_index)) |value| {
            return try self.wrapBindings(receiver.bindings, try self.materialize(value));
        }
        const item = try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
            .tuple = try self.materialize(receiver.value),
            .elem_index = access.elem_index,
        } } });
        return try self.wrapBindings(receiver.bindings, item);
    }

    fn cloneMatch(self: *Cloner, ty: Type.TypeId, match: @import("../monotype/ast.zig").MatchExpr) Common.LowerError!Ast.ExprId {
        var scrutinee = try self.cloneExprValueDemandingShape(match.scrutinee);
        if (self.knownConstructorSize(scrutinee.value).exactValue() == null) {
            // The scrutinee's measured size saturated the work budget: it is
            // cyclic or too deep to materialize. Skip the known-match collapse
            // and emit the residual match over a plain clone of the source
            // scrutinee, finite by construction, rather than materializing a
            // possibly self-referential value. The discarded clone owns its
            // bindings, so the plain re-clone is the only emitted evaluation.
            return try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
                .scrutinee = try self.cloneExprPlain(match.scrutinee),
                .branches = try self.cloneBranchSpan(match.branches),
                .comptime_site = match.comptime_site,
            } } });
        }
        if (try self.simplifyKnownMatch(scrutinee.value, match.branches, &scrutinee.bindings)) |body| {
            return try self.wrapBindings(scrutinee.bindings, body);
        }

        const scrutinee_expr = try self.materialize(scrutinee.value);
        const residual = try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = scrutinee_expr,
            .branches = try self.cloneBranchSpan(match.branches),
            .comptime_site = match.comptime_site,
        } } });
        return try self.wrapBindings(scrutinee.bindings, residual);
    }

    fn simplifyKnownMatch(
        self: *Cloner,
        scrutinee: Value,
        branches_span: Ast.Span(Ast.Branch),
        bindings: *BindingChain,
    ) Common.LowerError!?Ast.ExprId {
        if (try self.simplifyKnownMatchValue(scrutinee, branches_span, bindings)) |value| {
            return try self.materialize(value);
        }
        return null;
    }

    fn simplifyKnownMatchValue(
        self: *Cloner,
        scrutinee: Value,
        branches_span: Ast.Span(Ast.Branch),
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        return self.selectKnownMatchValue(scrutinee, branches_span, false, bindings);
    }

    /// Collapse a match whose scrutinee is a known constructor to the selected
    /// branch's body. `decline_on_no_match` distinguishes the two callers: the
    /// direct known-match collapse proves exhaustiveness (a known constructor
    /// always selects a branch), so a miss is an invariant; case-of-case
    /// distribution instead *offers* a value that a branch may not structurally
    /// cover (an opaque tag payload the selection cannot verify), so it declines
    /// and leaves the match materialized.
    fn selectKnownMatchValue(
        self: *Cloner,
        scrutinee: Value,
        branches_span: Ast.Span(Ast.Branch),
        decline_on_no_match: bool,
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        if (scrutinee == .expr) return null;
        // Read each branch by stable index rather than holding a `branchSpan`
        // borrow: `cloneExprValue(branch.body)` below can append to `branches`
        // through a nested match, which would invalidate a live borrow.
        for (0..branches_span.len) |branch_index| {
            const branch = self.pass.program.branchAt(branches_span, branch_index);
            const match_change_start = self.subst.watermark();
            const verdict = try self.bindPatToValue(branch.pat, scrutinee);
            self.subst.restore(match_change_start);
            switch (verdict) {
                // This branch can be neither ruled in nor ruled out
                // statically, so the whole fold aborts and the residual
                // match decides at runtime.
                .unknown, .unknown_budget_exhausted => return null,
                .no_match => continue,
                .match => {},
            }
            if (branch.guard != null or branch.bindings.len != 0) return null;

            const change_start = self.subst.watermark();
            if (try self.bindPatToMatchValue(branch.pat, scrutinee, branch.body, bindings) == null) {
                Common.invariant("known constructor match changed after reusable payload binding");
            }
            const body = try self.cloneExprValueInto(branch.body, bindings);
            self.subst.restore(change_start);
            return body;
        }
        if (decline_on_no_match) return null;
        Common.invariant("known constructor match had no matching branch");
    }

    fn bindPatToMatchValue(
        self: *Cloner,
        pat_id: Ast.PatId,
        value: Value,
        body: Ast.ExprId,
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| {
                const prepared = try self.valueForMatchLocal(value, bindings);
                try self.subst.put(self.pass.program, local, prepared);
                return prepared;
            },
            .wildcard => return try self.makeReusableForMatch(value, bindings),
            .as => |as| {
                const base = if (self.valueCanSubstitute(value) == .proven)
                    value
                else
                    try self.makeReusableForMatch(value, bindings);
                const prepared = (try self.bindPatToMatchValue(as.pattern, base, body, bindings)) orelse return null;
                try self.subst.put(self.pass.program, as.local, prepared);
                return prepared;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                switch (value) {
                    .static_data_candidate => |candidate| return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        candidate,
                        body,
                        bindings,
                    ),
                    .record => |record| {
                        const prepared_fields = try self.arena.allocator().alloc(FieldValue, record.fields.len);
                        for (record.fields, 0..) |field, index| {
                            if (recordPatField(self.pass.program, fields, field.name)) |field_pat| {
                                const prepared = (try self.bindPatToMatchValue(field_pat, field.value, body, bindings)) orelse return null;
                                prepared_fields[index] = .{
                                    .name = field.name,
                                    .value = prepared,
                                };
                            } else {
                                prepared_fields[index] = .{
                                    .name = field.name,
                                    .value = try self.makeReusableForMatch(field.value, bindings),
                                };
                            }
                        }
                        return Value{ .record = .{
                            .ty = record.ty,
                            .fields = prepared_fields,
                        } };
                    },
                    .nominal => |nominal| return try self.bindPatToMatchValueStripped(pat_id, nominal.backing.*, body, bindings),
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return null;
                        for (0..fields.len) |index| {
                            const field = GuardedList.at(fields, index);
                            const field_ty = self.pass.program.getPat(field.pattern).ty;
                            const field_expr = try self.addFieldAccessExpr(field_ty, receiver, field.name);
                            _ = (try self.bindPatToMatchValue(field.pattern, .{ .expr = field_expr }, body, bindings)) orelse return null;
                        }
                        return value;
                    },
                    .tag, .tuple, .callable => return null,
                }
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                switch (value) {
                    .static_data_candidate => |candidate| return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        candidate,
                        body,
                        bindings,
                    ),
                    .tuple => |tuple| {
                        if (pats.len != tuple.items.len) return null;
                        const items = try self.arena.allocator().alloc(Value, tuple.items.len);
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            const child_value = tuple.items[index];
                            items[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, bindings)) orelse return null;
                        }
                        return Value{ .tuple = .{
                            .ty = tuple.ty,
                            .items = items,
                        } };
                    },
                    .nominal => |nominal| return try self.bindPatToMatchValueStripped(pat_id, nominal.backing.*, body, bindings),
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return null;
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            const item_ty = self.pass.program.getPat(child_pat).ty;
                            const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                                .tuple = receiver,
                                .elem_index = @as(u32, @intCast(index)),
                            } } });
                            _ = (try self.bindPatToMatchValue(child_pat, .{ .expr = item_expr }, body, bindings)) orelse return null;
                        }
                        return value;
                    },
                    .tag, .record, .callable => return null,
                }
            },
            .tag => |tag_pat| {
                if (value == .static_data_candidate) {
                    return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        value.static_data_candidate,
                        body,
                        bindings,
                    );
                }
                const tag = tagFromValue(value) orelse return null;
                if (!self.pass.program.names.tagLabelTextEql(tag.name, tag_pat.name)) return null;
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) return null;
                const payloads = try self.arena.allocator().alloc(Value, tag.payloads.len);
                for (0..pats.len) |index| {
                    const child_pat = GuardedList.at(pats, index);
                    const child_value = tag.payloads[index];
                    payloads[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, bindings)) orelse return null;
                }
                return Value{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .nominal => |backing_pat| {
                if (value == .static_data_candidate) {
                    return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        value.static_data_candidate,
                        body,
                        bindings,
                    );
                }
                if (value != .nominal) return null;
                const nominal = value.nominal;
                const backing = try self.arena.allocator().create(Value);
                backing.* = (try self.bindPatToMatchValueStripped(backing_pat, nominal.backing.*, body, bindings)) orelse return null;
                return Value{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
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
            => return null,
        }
    }

    fn bindStaticDataCandidateToMatchValue(
        self: *Cloner,
        pat_id: Ast.PatId,
        candidate: StaticDataCandidateValue,
        body: Ast.ExprId,
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        const runtime = try self.arena.allocator().create(Value);
        runtime.* = (try self.bindPatToMatchValueStripped(pat_id, candidate.runtime.*, body, bindings)) orelse return null;
        return Value{ .static_data_candidate = .{
            .ty = candidate.ty,
            .static_data = candidate.static_data,
            .runtime = runtime,
        } };
    }

    /// Recurse into a nominal backing or static-data runtime while binding a
    /// known match value, counting the pointer-edge strip so a value that
    /// references itself through those edges cannot loop forever. The caller's
    /// static probe (`bindPatToValue` in `selectKnownMatchValue`) already
    /// declines the collapse for such a value, so reaching the cap here is not
    /// expected; returning null declines the reuse binding conservatively.
    fn bindPatToMatchValueStripped(
        self: *Cloner,
        pat_id: Ast.PatId,
        value: Value,
        body: Ast.ExprId,
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return null;
        self.wrapper_strip_depth += 1;
        defer self.wrapper_strip_depth -= 1;
        return try self.bindPatToMatchValue(pat_id, value, body, bindings);
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

    /// Materialize a known value once and bind it reuse-safely, so it is no
    /// longer tracked as a known constructor at its use sites.
    fn nameUnexpandedKnownValue(self: *Cloner, value: Value, bindings: *BindingChain) Common.LowerError!Value {
        return try self.makeReusableForMatch(.{ .expr = try self.materialize(value) }, bindings);
    }

    fn valueForMatchLocal(
        self: *Cloner,
        value: Value,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        switch (self.knownConstructorSize(value).admitExpansion(known_value_expansion_limit)) {
            .admitted => {},
            .denied_growth_limit, .denied_unknown_measure => return try self.nameUnexpandedKnownValue(value, bindings),
        }
        if (self.valueCanSubstitute(value) == .proven) return value;
        return try self.makeReusableForMatch(value, bindings);
    }

    fn valueForInlineLocal(
        self: *Cloner,
        value: Value,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        switch (self.knownConstructorSize(value).admitExpansion(known_value_expansion_limit)) {
            .admitted => {},
            .denied_growth_limit, .denied_unknown_measure => return try self.nameUnexpandedKnownValue(value, bindings),
        }
        if (self.valueCanSubstitute(value) == .proven) return value;
        return try self.makeReusableForMatch(value, bindings);
    }

    /// Clone a source expression to a known value for inlining, rebinding a
    /// value whose measured constructor size saturated the work budget through
    /// a plain clone of the source expression instead. A saturated size means
    /// the value is cyclic or too deep to measure; boxing it would
    /// deep-materialize a possibly self-referential value, whereas a plain
    /// clone of the source expression is finite by construction. The first
    /// clone owns its bindings, so declining it discards that entire chain and
    /// cannot duplicate any computation when the source is cloned plainly.
    fn cloneInlineValueBoundingCycles(
        self: *Cloner,
        expr_id: Ast.ExprId,
        demand_shape: bool,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        const cloned = if (demand_shape)
            try self.cloneExprValueDemandingShape(expr_id)
        else
            try self.cloneExprValue(expr_id);
        if (self.knownConstructorSize(cloned.value).exactValue() == null) {
            return try self.makeReusableForMatch(.{ .expr = try self.cloneExprPlain(expr_id) }, bindings);
        }
        bindings.appendChain(cloned.bindings);
        return cloned.value;
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
    fn knownConstructorSize(self: *Cloner, value: Value) ConstructorSize {
        var budget: u32 = known_constructor_size_work_budget;
        return self.knownConstructorSizeBudgeted(value, &budget);
    }

    fn knownConstructorSizeBudgeted(self: *Cloner, value: Value, budget: *u32) ConstructorSize {
        if (budget.* == 0) return .unknown_budget_exhausted;
        budget.* -= 1;
        return switch (value) {
            .expr => .{ .exact = 0 },
            .static_data_candidate => |candidate| self.knownConstructorSizeBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                var count = ConstructorSize{ .exact = 1 };
                for (tag.payloads) |payload| count = count.plus(self.knownConstructorSizeBudgeted(payload, budget));
                break :blk count;
            },
            .record => |record| blk: {
                var count = ConstructorSize{ .exact = 1 };
                for (record.fields) |field| count = count.plus(self.knownConstructorSizeBudgeted(field.value, budget));
                break :blk count;
            },
            .tuple => |tuple| blk: {
                var count = ConstructorSize{ .exact = 1 };
                for (tuple.items) |item| count = count.plus(self.knownConstructorSizeBudgeted(item, budget));
                break :blk count;
            },
            .nominal => |nominal| (ConstructorSize{ .exact = 1 }).plus(self.knownConstructorSizeBudgeted(nominal.backing.*, budget)),
            .callable => |callable| blk: {
                var count = ConstructorSize{ .exact = 1 };
                for (callable.captures) |capture| count = count.plus(self.knownConstructorSizeBudgeted(capture.value, budget));
                break :blk count;
            },
        };
    }

    /// Resolve an expression to its known value through the current
    /// substitution environment without emitting anything. Used only to measure
    /// a call's known-constructor size for the inline recursion guard; returns
    /// null when the expression carries no known constructor here.
    fn peekKnownValue(self: *Cloner, expr_id: Ast.ExprId) ?Value {
        const expr = self.pass.program.getExpr(expr_id);
        return switch (expr.data) {
            .local => |local| blk: {
                if (self.subst.get(self.pass.program, local)) |value| break :blk value;
                break :blk null;
            },
            .field_access => |field| blk: {
                const receiver = self.peekKnownValue(field.receiver) orelse break :blk null;
                break :blk fieldPathFromValue(
                    self.pass.program,
                    receiver,
                    self.pass.program.fieldAccessSegmentSpan(field.segments),
                );
            },
            .tuple_access => |access| blk: {
                const receiver = self.peekKnownValue(access.tuple) orelse break :blk null;
                break :blk itemFromValue(receiver, access.elem_index);
            },
            .static_data_candidate => |candidate| self.peekKnownValue(candidate.runtime_expr),
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
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => null,
        };
    }

    fn argsKnownConstructorSize(self: *Cloner, span: Ast.Span(Ast.ExprId)) ConstructorSize {
        var total = ConstructorSize{ .exact = 0 };
        const args = self.pass.program.exprSpan(span);
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (self.peekKnownValue(arg)) |value| total = total.plus(self.knownConstructorSize(value));
        }
        return total;
    }

    fn captureOperandsKnownConstructorSize(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) ConstructorSize {
        var total = ConstructorSize{ .exact = 0 };
        const operands = self.pass.program.captureOperandSpan(span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            if (self.peekKnownValue(operand.value)) |value| total = total.plus(self.knownConstructorSize(value));
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

    fn makeReusableForMatch(self: *Cloner, value: Value, bindings: *BindingChain) Common.LowerError!Value {
        var budget: u32 = make_reusable_work_budget;
        return try self.makeReusableForMatchBudgeted(value, &budget, bindings);
    }

    fn makeReusableForMatchBudgeted(
        self: *Cloner,
        value: Value,
        budget: *u32,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        if (budget.* == 0) {
            const ty = valueType(self.pass.program, value);
            const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
            try bindings.appendBinding(self.arena.allocator(), .{
                .local = local,
                .ty = ty,
                .value = try self.materialize(value),
            });
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = local } }) };
        }
        budget.* -= 1;
        if (self.valueCanSubstitute(value) == .proven) return value;
        return switch (value) {
            .expr => |expr| blk: {
                const ty = self.pass.program.getExpr(expr).ty;
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try bindings.appendBinding(self.arena.allocator(), .{
                    .local = local,
                    .ty = ty,
                    .value = expr,
                });
                break :blk Value{ .expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                }) };
            },
            .static_data_candidate => |candidate| blk: {
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), candidate.ty);
                try bindings.appendBinding(self.arena.allocator(), .{
                    .local = local,
                    .ty = candidate.ty,
                    .value = try self.materialize(value),
                });
                break :blk Value{ .expr = try self.addExpr(.{
                    .ty = candidate.ty,
                    .data = .{ .local = local },
                }) };
            },
            .tag => |tag| blk: {
                const payloads = try self.arena.allocator().alloc(Value, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = try self.makeReusableForMatchBudgeted(payload, budget, bindings);
                }
                break :blk Value{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| blk: {
                const fields = try self.arena.allocator().alloc(FieldValue, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.makeReusableForMatchBudgeted(field.value, budget, bindings),
                    };
                }
                break :blk Value{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.arena.allocator().alloc(Value, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = try self.makeReusableForMatchBudgeted(item, budget, bindings);
                }
                break :blk Value{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing = try self.arena.allocator().create(Value);
                backing.* = try self.makeReusableForMatchBudgeted(nominal.backing.*, budget, bindings);
                break :blk Value{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.arena.allocator().alloc(CaptureValue, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = .{
                        .id = capture.id,
                        .value = try self.makeReusableForMatchBudgeted(capture.value, budget, bindings),
                    };
                }
                break :blk Value{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                    .iterator_step = callable.iterator_step,
                } };
            },
        };
    }

    /// Place a strict chain around `expr`, oldest binding outermost.
    fn wrapBindings(self: *Cloner, bindings: BindingChain, expr: Ast.ExprId) Common.LowerError!Ast.ExprId {
        bindings.verify(self.pass.program);
        if (bindings.isEmpty()) return expr;
        const ty = self.pass.program.getExpr(expr).ty;
        var result = expr;
        var current = bindings.last;
        while (current) |node| : (current = node.previous) {
            const binding = node.binding;
            const pat = try self.pass.program.addPat(.{
                .ty = binding.ty,
                .data = .{ .bind = binding.local },
            });
            result = try self.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = binding.value,
                .rest = result,
            } } });
        }
        return result;
    }

    /// Place a strict chain into a statement list, oldest first.
    fn appendBindingStmts(self: *Cloner, bindings: BindingChain, out: *std.ArrayList(Ast.StmtId)) Common.LowerError!void {
        bindings.verify(self.pass.program);
        var current = bindings.first;
        while (current) |node| : (current = node.next) {
            const binding = node.binding;
            const pat = try self.pass.program.addPat(.{
                .ty = binding.ty,
                .data = .{ .bind = binding.local },
            });
            try out.append(self.pass.allocator, try self.addStmt(.{ .let_ = .{
                .pat = pat,
                .value = binding.value,
            } }));
        }
    }

    fn cloneCaseOfCaseValue(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee_expr: Ast.ExprId,
        outer_branches_span: Ast.Span(Ast.Branch),
    ) Common.LowerError!?Value {
        const scrutinee_data = self.pass.program.getExpr(scrutinee_expr).data;

        const outer_branches = self.pass.program.branchSpan(outer_branches_span);
        for (0..outer_branches.len) |branch_index| {
            const branch = GuardedList.at(outer_branches, branch_index);
            if (branch.guard != null or branch.bindings.len != 0) return null;
        }

        const branch_work = switch (scrutinee_data) {
            .match_ => |inner_match| self.pass.program.branchSpan(inner_match.branches).len,
            .if_ => |inner_if| self.pass.program.ifBranchSpan(inner_if.branches).len + 1,
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
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => return null,
        };
        if (self.case_of_case_growth.admit(@max(branch_work, 1)) != .admitted) return null;

        switch (scrutinee_data) {
            .match_ => |inner_match| {
                const inner_branches = try GuardedList.dupe(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(inner_match.branches));
                defer self.pass.allocator.free(inner_branches);

                var rewritten = try self.pass.allocator.alloc(Ast.Branch, inner_branches.len);
                defer self.pass.allocator.free(rewritten);

                for (inner_branches, 0..) |inner_branch, index| {
                    const change_start = self.subst.watermark();
                    try self.shadowPatLocals(inner_branch.pat);
                    try self.shadowStmtSpanLocals(inner_branch.bindings);
                    const body = (try self.distributeMatchOverArmBody(ty, inner_branch.body, outer_branches_span)) orelse {
                        self.subst.restore(change_start);
                        return null;
                    };
                    rewritten[index] = .{
                        .pat = inner_branch.pat,
                        .bindings = inner_branch.bindings,
                        .guard = inner_branch.guard,
                        .body = body,
                    };
                    self.subst.restore(change_start);
                }

                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
                    .scrutinee = inner_match.scrutinee,
                    .branches = try self.pass.program.addBranchSpan(rewritten),
                    .comptime_site = inner_match.comptime_site,
                } } }) };
            },
            .if_ => |inner_if| {
                const inner_branches = try GuardedList.dupe(self.pass.allocator, Ast.IfBranch, self.pass.program.ifBranchSpan(inner_if.branches));
                defer self.pass.allocator.free(inner_branches);

                var rewritten = try self.pass.allocator.alloc(Ast.IfBranch, inner_branches.len);
                defer self.pass.allocator.free(rewritten);

                for (inner_branches, 0..) |inner_branch, index| {
                    const body = (try self.distributeMatchOverArmBody(ty, inner_branch.body, outer_branches_span)) orelse return null;
                    rewritten[index] = .{
                        .cond = inner_branch.cond,
                        .body = body,
                    };
                }

                const final_else = (try self.distributeMatchOverArmBody(ty, inner_if.final_else, outer_branches_span)) orelse return null;

                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .if_ = .{
                    .branches = try self.pass.program.addIfBranchSpan(rewritten),
                    .final_else = final_else,
                } } }) };
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
            .block,
            .loop_,
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => unreachable,
        }
    }

    /// Distribute the outer match over one inner arm. The arm is
    /// already-cloned output with fresh ids referenced nowhere else, so a
    /// block arm keeps its statements as they stand and only the tail
    /// expression is re-derived for its symbolic value; a tail that is
    /// itself branch-built recurses structurally without any re-clone.
    /// Re-cloning whole arm bodies here compounded across nested
    /// case-of-case levels.
    fn distributeMatchOverArmBody(
        self: *Cloner,
        ty: Type.TypeId,
        arm_body: Ast.ExprId,
        outer_branches_span: Ast.Span(Ast.Branch),
    ) Common.LowerError!?Ast.ExprId {
        const arm_expr = self.pass.program.getExpr(arm_body);
        switch (arm_expr.data) {
            .block => |block| {
                const tail = block.final_expr;
                var branch_bindings: BindingChain = .{};
                const inner_value: Value = switch (self.pass.program.getExpr(tail).data) {
                    // Branch-built and looping tails recurse (or decline)
                    // through the distribution itself, without re-deriving
                    // the expression.
                    .match_, .if_, .loop_ => .{ .expr = tail },
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
                    .list,
                    .tuple,
                    .record,
                    .record_update,
                    .tag,
                    .nominal,
                    .block,
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
                    .break_,
                    .continue_,
                    .join_point,
                    .jump,
                    .return_,
                    .crash,
                    .comptime_branch_taken,
                    .comptime_exhaustiveness_failed,
                    .dbg,
                    .expect_err,
                    .expect,
                    => try self.cloneExprValueInto(tail, &branch_bindings),
                };
                const outer_value = (try self.distributeMatchOverValue(ty, inner_value, outer_branches_span, &branch_bindings)) orelse return null;

                var statements = std.ArrayList(Ast.StmtId).empty;
                defer statements.deinit(self.pass.allocator);
                const source = self.pass.program.stmtSpan(block.statements);
                for (0..GuardedList.borrowLen(source)) |index| {
                    try statements.append(self.pass.allocator, GuardedList.at(source, index));
                }
                return try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
                    .statements = try self.pass.program.addStmtSpan(statements.items),
                    .final_expr = try self.wrapBindings(branch_bindings, try self.materialize(outer_value)),
                } } });
            },
            .match_, .if_, .loop_ => {
                var branch_bindings: BindingChain = .{};
                const outer_value = (try self.distributeMatchOverValue(ty, .{ .expr = arm_body }, outer_branches_span, &branch_bindings)) orelse return null;
                return try self.wrapBindings(branch_bindings, try self.materialize(outer_value));
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
            .break_,
            .continue_,
            .join_point,
            .jump,
            .return_,
            .crash,
            .comptime_branch_taken,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => {
                var branch_bindings: BindingChain = .{};
                const inner_value = try self.cloneExprValueInto(arm_body, &branch_bindings);
                const outer_value = (try self.distributeMatchOverValue(ty, inner_value, outer_branches_span, &branch_bindings)) orelse return null;
                return try self.wrapBindings(branch_bindings, try self.materialize(outer_value));
            },
        }
    }

    /// Collapse an outer match against one inner-branch result: a known
    /// constructor selects its arm directly, and a branch-built result
    /// distributes recursively so the arms land where the constructors are
    /// known.
    fn distributeMatchOverValue(
        self: *Cloner,
        ty: Type.TypeId,
        inner_value: Value,
        outer_branches_span: Ast.Span(Ast.Branch),
        bindings: *BindingChain,
    ) Common.LowerError!?Value {
        if (try self.selectKnownMatchValue(inner_value, outer_branches_span, true, bindings)) |value| return value;
        return switch (inner_value) {
            .expr => |expr| try self.cloneCaseOfCaseValue(ty, expr, outer_branches_span),
            .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => null,
        };
    }

    fn inlineCallableCallValue(
        self: *Cloner,
        ty: Type.TypeId,
        callable: CallableValue,
        args_span: Ast.Span(Ast.ExprId),
        original_expr: Ast.ExprId,
        result_shape_demanded: bool,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        const source_fn = self.pass.program.getFn(callable.fn_id);
        // Replacing the call with its body transfers the body's result value
        // directly into the caller. Independently specialized Monotype graphs
        // can make those types related but representation-distinct; only exact
        // closed-type identity proves that no representation boundary would be
        // erased by the inline.
        if (!sameType(self.pass.program, ty, source_fn.ret)) {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) };
        }
        const body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) },
        };
        const body_size = self.pass.inlineBodySize(callable.fn_id, body);
        if (!body_size.admits()) {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) };
        }
        if (exprContainsReturn(self.pass.program, body)) {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) };
        }
        var callable_call_size = ConstructorSize{ .exact = 0 };
        for (callable.captures) |capture| callable_call_size = callable_call_size.plus(self.knownConstructorSize(capture.value));
        callable_call_size = callable_call_size.plus(self.argsKnownConstructorSize(args_span));
        const exact_call_size = callable_call_size.exactValue() orelse {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) };
        };
        for (self.inline_stack.items) |active| {
            if (active.fn_id != callable.fn_id) continue;
            if (exact_call_size == 0 or exact_call_size >= active.known_size) {
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(.{ .callable = callable }),
                    .args = try self.cloneExprSpan(args_span),
                } } }) };
            }
        }
        if (!self.admitInlineBodyGrowth(body_size)) {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) };
        }

        const source_args = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(args_span));
        defer self.pass.allocator.free(args);
        if (source_args.len != args.len) Common.invariant("callable call arity differed from lifted function arity");

        const source_captures = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(source_captures);
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);

        const prepared_captures = try self.pass.allocator.alloc(Value, callable.captures.len);
        defer self.pass.allocator.free(prepared_captures);
        for (source_captures, 0..) |source_capture, index| {
            const id = self.pass.program.captureIdOfLocal(source_capture.local);
            const capture_value = callableCaptureValueForId(callable.captures, id) orelse
                Common.invariant("callable value had no value for a source capture slot");
            prepared_captures[index] = try self.makeReusableForMatch(capture_value, bindings);
            try self.subst.put(self.pass.program, source_capture.local, prepared_captures[index]);
        }

        const arg_values = try self.pass.allocator.alloc(Value, args.len);
        defer self.pass.allocator.free(arg_values);
        const callee_raw = @intFromEnum(callable.fn_id);
        for (args, 0..) |arg_expr, index| {
            const demand_arg_shape = result_shape_demanded and
                callee_raw < self.pass.plans.len and
                self.pass.plans[callee_raw].used_args[index];
            arg_values[index] = try self.cloneInlineValueBoundingCycles(arg_expr, demand_arg_shape, bindings);
        }

        const prepared_args = try self.pass.allocator.alloc(Value, arg_values.len);
        defer self.pass.allocator.free(prepared_args);
        for (arg_values, 0..) |arg_value, index| {
            prepared_args[index] = try self.valueForInlineLocal(arg_value, bindings);
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callable.fn_id, .known_size = exact_call_size });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callable.fn_id) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.subst.put(self.pass.program, source_arg.local, arg_value);
        }

        const saved_inline_scope = self.current_inline_scope;
        defer self.current_inline_scope = saved_inline_scope;
        try self.enterInlineScope(callable.fn_id, self.pass.program.exprLoc(original_expr));
        return try self.cloneExprValueInto(body, bindings);
    }

    fn inlineDirectCallValue(
        self: *Cloner,
        callee: Ast.FnId,
        args_span: Ast.Span(Ast.ExprId),
        captures_span: Ast.Span(Ast.CaptureOperand),
        original_expr: Ast.ExprId,
        result_shape_demanded: bool,
        bindings: *BindingChain,
    ) Common.LowerError!Value {
        const source_fn = self.pass.program.getFn(callee);
        const result_ty = self.pass.program.getExpr(original_expr).ty;
        // A call and its independently specialized callee can be related
        // without having the same runtime representation. Keep that explicit
        // call boundary unless their closed Monotype digests are identical.
        if (!sameType(self.pass.program, result_ty, source_fn.ret)) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }
        const body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => return .{ .expr = try self.cloneExprPlain(original_expr) },
        };
        const body_size = self.pass.inlineBodySize(callee, body);
        if (!body_size.admits()) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }
        if (exprContainsReturn(self.pass.program, body)) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }
        const direct_call_size = self.argsKnownConstructorSize(args_span).plus(self.captureOperandsKnownConstructorSize(captures_span));
        const exact_call_size = direct_call_size.exactValue() orelse return .{ .expr = try self.cloneExprPlain(original_expr) };
        for (self.inline_stack.items) |active| {
            if (active.fn_id != callee) continue;
            if (exact_call_size == 0 or exact_call_size >= active.known_size) {
                return .{ .expr = try self.cloneExprPlain(original_expr) };
            }
        }
        if (!self.admitInlineBodyGrowth(body_size)) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }
        const source_args = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(args_span));
        defer self.pass.allocator.free(args);
        if (source_args.len != args.len) Common.invariant("direct call arity differed from lifted function arity");

        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);

        const captures = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(captures);
        // The call's capture operands are keyed by CaptureId, not positional
        // with the callee's capture slots. Clone each operand's value keyed by
        // id, then resolve each slot's value by its own CaptureId below.
        const operands = try GuardedList.dupe(self.pass.allocator, Ast.CaptureOperand, self.pass.program.captureOperandSpan(captures_span));
        defer self.pass.allocator.free(operands);
        if (captures.len != operands.len) {
            Common.invariant("direct call capture count differed from lifted function capture count");
        }

        const capture_values = try self.pass.allocator.alloc(CaptureValue, operands.len);
        defer self.pass.allocator.free(capture_values);
        for (operands, 0..) |operand, index| {
            capture_values[index] = .{
                .id = operand.id,
                .value = try self.cloneInlineValueBoundingCycles(operand.value, false, bindings),
            };
        }

        const arg_values = try self.pass.allocator.alloc(Value, args.len);
        defer self.pass.allocator.free(arg_values);
        for (args, 0..) |arg_expr, index| {
            const demand_arg_shape = result_shape_demanded and
                @intFromEnum(callee) < self.pass.plans.len and
                self.pass.plans[@intFromEnum(callee)].used_args[index];
            arg_values[index] = try self.cloneInlineValueBoundingCycles(arg_expr, demand_arg_shape, bindings);
        }

        const prepared_captures = try self.pass.allocator.alloc(Value, captures.len);
        defer self.pass.allocator.free(prepared_captures);
        for (captures, 0..) |capture, index| {
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const capture_value = callableCaptureValueForId(capture_values, id) orelse
                Common.invariant("direct call had no value for a source capture slot");
            prepared_captures[index] = try self.valueForInlineLocal(capture_value, bindings);
        }

        const prepared_args = try self.pass.allocator.alloc(Value, arg_values.len);
        defer self.pass.allocator.free(prepared_args);
        for (arg_values, 0..) |arg_value, index| {
            prepared_args[index] = try self.valueForInlineLocal(arg_value, bindings);
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callee, .known_size = exact_call_size });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callee) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (captures, prepared_captures) |capture, capture_value| {
            try self.subst.put(self.pass.program, capture.local, capture_value);
        }
        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.subst.put(self.pass.program, source_arg.local, arg_value);
        }

        const saved_inline_scope = self.current_inline_scope;
        defer self.current_inline_scope = saved_inline_scope;
        try self.enterInlineScope(callee, self.pass.program.exprLoc(original_expr));
        return try self.cloneExprValueInto(body, bindings);
    }

    fn bindPatToValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!MatchVerdict {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| {
                try self.subst.put(self.pass.program, local, value);
                return .match;
            },
            .wildcard => return .match,
            .as => |as| {
                const verdict = try self.bindPatToValue(as.pattern, value);
                if (verdict != .match) return verdict;
                try self.subst.put(self.pass.program, as.local, value);
                return .match;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                switch (value) {
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .unknown;
                        var verdict: MatchVerdict = .match;
                        for (0..fields.len) |index| {
                            const field = GuardedList.at(fields, index);
                            const field_ty = self.pass.program.getPat(field.pattern).ty;
                            const field_expr = try self.addFieldAccessExpr(field_ty, receiver, field.name);
                            const child_verdict = try self.bindPatToValue(field.pattern, .{ .expr = field_expr });
                            switch (child_verdict) {
                                .match => {},
                                .no_match => return .no_match,
                                .unknown, .unknown_budget_exhausted => verdict = mergeMatchUnknown(verdict, child_verdict),
                            }
                        }
                        return verdict;
                    },
                    .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => {},
                }
                const record = recordFromValue(value) orelse switch (value) {
                    .tag, .tuple, .callable => Common.invariant("record pattern matched a non-record value"),
                    .expr, .static_data_candidate, .record, .nominal => Common.invariant("record value had no record backing"),
                };
                var verdict: MatchVerdict = .match;
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    const field_value = fieldFromRecord(self.pass.program, record, field.name) orelse
                        Common.invariant("record pattern field was absent from the record value");
                    const child_verdict = try self.bindPatToValue(field.pattern, field_value);
                    switch (child_verdict) {
                        .match => {},
                        .no_match => return .no_match,
                        .unknown, .unknown_budget_exhausted => verdict = mergeMatchUnknown(verdict, child_verdict),
                    }
                }
                return verdict;
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                switch (value) {
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return .unknown;
                        var verdict: MatchVerdict = .match;
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            const item_ty = self.pass.program.getPat(child_pat).ty;
                            const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                                .tuple = receiver,
                                .elem_index = @as(u32, @intCast(index)),
                            } } });
                            const child_verdict = try self.bindPatToValue(child_pat, .{ .expr = item_expr });
                            switch (child_verdict) {
                                .match => {},
                                .no_match => return .no_match,
                                .unknown, .unknown_budget_exhausted => verdict = mergeMatchUnknown(verdict, child_verdict),
                            }
                        }
                        return verdict;
                    },
                    .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => {},
                }
                const tuple = tupleFromValue(value) orelse switch (value) {
                    .tag, .record, .callable => Common.invariant("tuple pattern matched a non-tuple value"),
                    .expr, .static_data_candidate, .tuple, .nominal => Common.invariant("tuple value had no tuple backing"),
                };
                if (pats.len != tuple.items.len) Common.invariant("tuple pattern arity differed from the tuple value");
                var verdict: MatchVerdict = .match;
                for (0..pats.len) |index| {
                    const child_pat = GuardedList.at(pats, index);
                    const child_value = tuple.items[index];
                    const child_verdict = try self.bindPatToValue(child_pat, child_value);
                    switch (child_verdict) {
                        .match => {},
                        .no_match => return .no_match,
                        .unknown, .unknown_budget_exhausted => verdict = mergeMatchUnknown(verdict, child_verdict),
                    }
                }
                return verdict;
            },
            .tag => |tag_pat| {
                if (value == .expr) return .unknown;
                const tag = tagFromValue(value) orelse switch (value) {
                    .record, .tuple, .callable => Common.invariant("tag pattern matched a non-tag value"),
                    .expr, .static_data_candidate, .tag, .nominal => Common.invariant("tag value had no tag backing"),
                };
                if (!self.pass.program.names.tagLabelTextEql(tag.name, tag_pat.name)) return .no_match;
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) Common.invariant("tag pattern payload arity differed from the tag value");
                var verdict: MatchVerdict = .match;
                for (0..pats.len) |index| {
                    const child_pat = GuardedList.at(pats, index);
                    const child_value = tag.payloads[index];
                    const child_verdict = try self.bindPatToValue(child_pat, child_value);
                    switch (child_verdict) {
                        .match => {},
                        .no_match => return .no_match,
                        .unknown, .unknown_budget_exhausted => verdict = mergeMatchUnknown(verdict, child_verdict),
                    }
                }
                return verdict;
            },
            .nominal => |backing_pat| {
                // Stripping a nominal or static-data wrapper follows a value
                // pointer edge that a recursive construction can loop through;
                // a cyclic value declines to a residual runtime match.
                if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return .unknown_budget_exhausted;
                self.wrapper_strip_depth += 1;
                defer self.wrapper_strip_depth -= 1;
                return switch (value) {
                    .static_data_candidate => |candidate| try self.bindPatToValue(pat_id, candidate.runtime.*),
                    .nominal => |nominal| try self.bindPatToValue(backing_pat, nominal.backing.*),
                    .expr => .unknown,
                    .tag, .record, .tuple, .callable => Common.invariant("nominal pattern matched an unwrapped constructor value"),
                };
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
            => return .unknown,
        }
    }

    fn bindPatToReusableValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!MatchVerdict {
        return switch (self.valueCanSubstitute(value)) {
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
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| {
                try self.subst.put(self.pass.program, local, value);
                return true;
            },
            .wildcard => return true,
            .as => |as| {
                if (!try self.bindPatToFlowValue(as.pattern, value)) return false;
                try self.subst.put(self.pass.program, as.local, value);
                return true;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                switch (value) {
                    .record, .nominal, .static_data_candidate => {
                        const record = recordFromValue(value) orelse return false;
                        for (0..fields.len) |index| {
                            const field = GuardedList.at(fields, index);
                            const field_value = fieldFromRecord(self.pass.program, record, field.name) orelse return false;
                            if (!try self.bindPatToFlowValue(field.pattern, field_value)) return false;
                        }
                    },
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                        for (0..fields.len) |index| {
                            const field = GuardedList.at(fields, index);
                            const field_ty = self.pass.program.getPat(field.pattern).ty;
                            const field_expr = try self.addFieldAccessExpr(field_ty, receiver, field.name);
                            if (!try self.bindPatToFlowValue(field.pattern, .{ .expr = field_expr })) return false;
                        }
                    },
                    .tag, .tuple, .callable => return false,
                }
                return true;
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                switch (value) {
                    .tuple, .nominal, .static_data_candidate => {
                        const tuple = tupleFromValue(value) orelse return false;
                        if (pats.len != tuple.items.len) return false;
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            if (!try self.bindPatToFlowValue(child_pat, tuple.items[index])) return false;
                        }
                    },
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            const item_ty = self.pass.program.getPat(child_pat).ty;
                            const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                                .tuple = receiver,
                                .elem_index = @as(u32, @intCast(index)),
                            } } });
                            if (!try self.bindPatToFlowValue(child_pat, .{ .expr = item_expr })) return false;
                        }
                    },
                    .tag, .record, .callable => return false,
                }
                return true;
            },
            .tag => |tag_pat| {
                const tag = tagFromValue(value) orelse return false;
                if (!self.pass.program.names.tagLabelTextEql(tag.name, tag_pat.name)) return false;
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) return false;
                for (0..pats.len) |index| {
                    if (!try self.bindPatToFlowValue(GuardedList.at(pats, index), tag.payloads[index])) return false;
                }
                return true;
            },
            .nominal => |backing_pat| {
                // Stripping a nominal or static-data wrapper follows a value
                // pointer edge that a recursive construction can loop through;
                // a cyclic value declines the flow binding.
                if (self.wrapper_strip_depth >= value_wrapper_strip_cap) return false;
                self.wrapper_strip_depth += 1;
                defer self.wrapper_strip_depth -= 1;
                return switch (value) {
                    .static_data_candidate => |candidate| try self.bindPatToFlowValue(pat_id, candidate.runtime.*),
                    .nominal => |nominal| try self.bindPatToFlowValue(backing_pat, nominal.backing.*),
                    .expr, .tag, .record, .tuple, .callable => false,
                };
            },
            .list,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .str_pattern,
            => return false,
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

    fn putLocalAlias(self: *Cloner, source: Ast.LocalId, target: Ast.LocalId) Common.LowerError!void {
        const ty = self.pass.program.getLocal(target).ty;
        const target_expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = target } });
        try self.subst.putLocalAlias(self.pass.program, source, .{ .expr = target_expr });
    }

    fn shadowPatLocals(self: *Cloner, pat_id: Ast.PatId) Common.LowerError!void {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| try self.shadowLocal(local),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .as => |as| {
                try self.shadowPatLocals(as.pattern);
                try self.shadowLocal(as.local);
            },
            .record => |fields| {
                const record_fields = self.pass.program.recordDestructSpan(fields);
                for (0..record_fields.len) |index| {
                    try self.shadowPatLocals(GuardedList.at(record_fields, index).pattern);
                }
            },
            .tuple => |items| {
                const children = self.pass.program.patSpan(items);
                for (0..children.len) |index| try self.shadowPatLocals(GuardedList.at(children, index));
            },
            .tag => |tag| {
                const children = self.pass.program.patSpan(tag.payloads);
                for (0..children.len) |index| try self.shadowPatLocals(GuardedList.at(children, index));
            },
            .nominal => |backing| try self.shadowPatLocals(backing),
            .list => |list| {
                const children = self.pass.program.patSpan(list.patterns);
                for (0..children.len) |index| try self.shadowPatLocals(GuardedList.at(children, index));
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| try self.shadowPatLocals(rest_pattern);
                }
            },
            .str_pattern => |str| {
                const steps = self.pass.program.strPatternStepSpan(str.steps);
                for (0..steps.len) |index| {
                    if (GuardedList.at(steps, index).capture) |capture| try self.shadowPatLocals(capture);
                }
            },
        }
    }

    fn shadowStmtSpanLocals(self: *Cloner, span: Ast.Span(Ast.StmtId)) Common.LowerError!void {
        const statements = self.pass.program.stmtSpan(span);
        for (0..statements.len) |index| {
            switch (self.pass.program.getStmt(GuardedList.at(statements, index))) {
                .let_ => |let_| try self.shadowPatLocals(let_.pat),
                .uninitialized => |pat| try self.shadowPatLocals(pat),
                .expr, .expect, .dbg, .return_, .crash => {},
            }
        }
    }

    fn markActiveRecursiveValuePat(self: *Cloner, pat_id: Ast.PatId) Allocator.Error!void {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| try self.active_recursive_value_locals.put(local, {}),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .as => |as| {
                try self.markActiveRecursiveValuePat(as.pattern);
                try self.active_recursive_value_locals.put(as.local, {});
            },
            .record => |fields| {
                const record_fields = self.pass.program.recordDestructSpan(fields);
                for (0..record_fields.len) |index| {
                    try self.markActiveRecursiveValuePat(GuardedList.at(record_fields, index).pattern);
                }
            },
            .tuple => |items| {
                const children = self.pass.program.patSpan(items);
                for (0..children.len) |index| try self.markActiveRecursiveValuePat(GuardedList.at(children, index));
            },
            .tag => |tag| {
                const children = self.pass.program.patSpan(tag.payloads);
                for (0..children.len) |index| try self.markActiveRecursiveValuePat(GuardedList.at(children, index));
            },
            .nominal => |backing| try self.markActiveRecursiveValuePat(backing),
            .list => |list| {
                const children = self.pass.program.patSpan(list.patterns);
                for (0..children.len) |index| try self.markActiveRecursiveValuePat(GuardedList.at(children, index));
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| try self.markActiveRecursiveValuePat(rest_pattern);
                }
            },
            .str_pattern => |str| {
                const steps = self.pass.program.strPatternStepSpan(str.steps);
                for (0..steps.len) |index| {
                    if (GuardedList.at(steps, index).capture) |capture| try self.markActiveRecursiveValuePat(capture);
                }
            },
        }
    }

    fn unmarkActiveRecursiveValuePat(self: *Cloner, pat_id: Ast.PatId) void {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| _ = self.active_recursive_value_locals.remove(local),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .as => |as| {
                self.unmarkActiveRecursiveValuePat(as.pattern);
                _ = self.active_recursive_value_locals.remove(as.local);
            },
            .record => |fields| {
                const record_fields = self.pass.program.recordDestructSpan(fields);
                for (0..record_fields.len) |index| {
                    self.unmarkActiveRecursiveValuePat(GuardedList.at(record_fields, index).pattern);
                }
            },
            .tuple => |items| {
                const children = self.pass.program.patSpan(items);
                for (0..children.len) |index| self.unmarkActiveRecursiveValuePat(GuardedList.at(children, index));
            },
            .tag => |tag| {
                const children = self.pass.program.patSpan(tag.payloads);
                for (0..children.len) |index| self.unmarkActiveRecursiveValuePat(GuardedList.at(children, index));
            },
            .nominal => |backing| self.unmarkActiveRecursiveValuePat(backing),
            .list => |list| {
                const children = self.pass.program.patSpan(list.patterns);
                for (0..children.len) |index| self.unmarkActiveRecursiveValuePat(GuardedList.at(children, index));
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| self.unmarkActiveRecursiveValuePat(rest_pattern);
                }
            },
            .str_pattern => |str| {
                const steps = self.pass.program.strPatternStepSpan(str.steps);
                for (0..steps.len) |index| {
                    if (GuardedList.at(steps, index).capture) |capture| self.unmarkActiveRecursiveValuePat(capture);
                }
            },
        }
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
            .static_data_candidate, .tag, .record, .tuple, .nominal, .callable => Common.invariant("SpecConstr local-id field referenced a non-local substituted value"),
        };
        return localExpr(self.pass.program, expr) orelse
            Common.invariant("SpecConstr local-id field referenced a non-local expression");
    }

    fn clonePat(self: *Cloner, pat_id: Ast.PatId, mode: BinderCloneMode) Common.LowerError!Ast.PatId {
        const pat = self.pass.program.getPat(pat_id);
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| .{ .bind = try self.cloneBinder(local, pat.ty, mode) },
            .wildcard => .wildcard,
            .as => |as| .{ .as = .{
                .pattern = try self.clonePat(as.pattern, mode),
                .local = try self.cloneBinder(as.local, pat.ty, mode),
            } },
            .record => |fields| .{ .record = try self.cloneRecordDestructSpan(fields, mode) },
            .tuple => |items| .{ .tuple = try self.clonePatSpan(items, mode) },
            .list => |list| .{ .list = .{
                .patterns = try self.clonePatSpan(list.patterns, mode),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |rest_pattern| try self.clonePat(rest_pattern, mode) else null,
                } else null,
            } },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.clonePatSpan(tag.payloads, mode),
            } },
            .nominal => |backing| .{ .nominal = try self.clonePat(backing, mode) },
            .int_lit => |value| .{ .int_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .str_pattern => |str| .{ .str_pattern = try self.cloneStrPattern(str, mode) },
        };
        return try self.pass.program.addPat(.{ .ty = pat.ty, .data = data });
    }

    fn cloneStrPattern(self: *Cloner, str: Ast.StrPattern, mode: BinderCloneMode) Common.LowerError!Ast.StrPattern {
        const input_steps = self.pass.program.strPatternStepSpan(str.steps);
        const output_steps = try self.pass.allocator.alloc(Ast.StrPatternStep, input_steps.len);
        defer self.pass.allocator.free(output_steps);

        for (0..input_steps.len) |index| {
            const input_step = GuardedList.at(input_steps, index);
            const output_step = &output_steps[index];
            output_step.* = .{
                .capture = if (input_step.capture) |capture| try self.clonePat(capture, mode) else null,
                .delimiter = input_step.delimiter,
            };
        }

        return .{
            .prefix = str.prefix,
            .steps = try self.pass.program.addStrPatternStepSpan(output_steps),
            .end = str.end,
        };
    }

    /// Clone one statement. A binding statement whose value's opaque leaves
    /// can all be named dissolves instead: the returned binding chain is
    /// placed by the caller at this statement's position—the same
    /// computations in the same order—and the bound name keeps its
    /// structured value for the rest of the block. `stmt` is null when the
    /// source binding dissolved completely.
    fn cloneStmt(self: *Cloner, stmt_id: Ast.StmtId) Common.LowerError!ClonedStmt {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const saved_inline_scope = self.current_inline_scope;
        defer self.current_inline_scope = saved_inline_scope;
        try self.adoptStmtInlineScope(stmt_id);
        const stmt_loc = self.pass.program.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) self.current_loc = stmt_loc;
        const stmt_region = self.pass.program.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) self.current_region = stmt_region;

        const stmt = self.pass.program.getStmt(stmt_id);
        var bindings: BindingChain = .{};
        const cloned: ?Ast.Stmt = switch (stmt) {
            .uninitialized => |pat| blk: {
                break :blk .{ .uninitialized = try self.clonePat(pat, .bind_runtime) };
            },
            .let_ => |let_| blk: {
                if (let_.recursive) try self.markActiveRecursiveValuePat(let_.pat);
                defer if (let_.recursive) self.unmarkActiveRecursiveValuePat(let_.pat);
                const recursive_pat = if (let_.recursive)
                    try self.clonePat(let_.pat, .bind_runtime)
                else
                    null;
                if (recursive_pat) |pat| try self.markActiveRecursiveValuePat(pat);
                defer if (recursive_pat) |pat| self.unmarkActiveRecursiveValuePat(pat);
                const value = try self.cloneExprValueInto(let_.value, &bindings);
                const value_expr = try self.materialize(value);
                if (try self.bindPatToReusableValue(let_.pat, value) == .match) {
                    break :blk .{ .let_ = .{
                        .pat = recursive_pat orelse try self.clonePat(let_.pat, .output_only),
                        .value = value_expr,
                        .recursive = let_.recursive,
                        .comptime_site = let_.comptime_site,
                    } };
                }
                const pat = self.pass.program.getPat(let_.pat);
                const self_referential = if (pat.data == .bind)
                    localUseCountInExpr(self.pass.program, pat.data.bind, let_.value) != 0
                else
                    let_.recursive;
                if (!self_referential) {
                    // The drained bindings sit exactly where the statement
                    // sat, so no evaluation moves and no gate is needed.
                    const change_before = self.subst.watermark();
                    const bindings_before = bindings.mark();
                    const reusable = try self.makeReusableForMatch(value, &bindings);
                    if (try self.bindPatToFlowValue(let_.pat, reusable)) break :blk null;
                    self.subst.restore(change_before);
                    bindings.rewind(bindings_before);
                }
                break :blk .{ .let_ = .{
                    .pat = recursive_pat orelse try self.clonePat(let_.pat, .bind_runtime),
                    .value = value_expr,
                    .recursive = let_.recursive,
                    .comptime_site = let_.comptime_site,
                } };
            },
            .expr => |expr| .{ .expr = try self.cloneExpr(expr) },
            .expect => |expr| .{ .expect = try self.cloneExpr(expr) },
            .dbg => |expr| .{ .dbg = try self.cloneExpr(expr) },
            .return_ => |ret| .{ .return_ = .{
                .value = try self.cloneExpr(ret.value),
                .target = ret.target,
            } },
            .crash => |msg| .{ .crash = msg },
        };
        return .{
            .bindings = bindings,
            .stmt = if (cloned) |actual| try self.addStmt(actual) else null,
        };
    }

    fn cloneExprSpan(self: *Cloner, span: Ast.Span(Ast.ExprId)) Common.LowerError!Ast.Span(Ast.ExprId) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.ExprId, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |expr, index| values[index] = try self.cloneExpr(expr);
        return try self.pass.program.addExprSpan(values);
    }

    fn cloneStmtSpan(self: *Cloner, span: Ast.Span(Ast.StmtId)) Common.LowerError!Ast.Span(Ast.StmtId) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(span));
        defer self.pass.allocator.free(source);

        var values: std.ArrayList(Ast.StmtId) = .empty;
        defer values.deinit(self.pass.allocator);
        for (source) |stmt| {
            const cloned = try self.cloneStmt(stmt);
            try self.appendBindingStmts(cloned.bindings, &values);
            if (cloned.stmt) |cloned_stmt| try values.append(self.pass.allocator, cloned_stmt);
        }
        return try self.pass.program.addStmtSpan(values.items);
    }

    fn cloneCaptureOperandSpan(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) Common.LowerError!Ast.Span(Ast.CaptureOperand) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.CaptureOperand, self.pass.program.captureOperandSpan(span));
        defer self.pass.allocator.free(source);

        const operands = try self.pass.allocator.alloc(Ast.CaptureOperand, source.len);
        defer self.pass.allocator.free(operands);
        for (source, 0..) |operand, index| {
            operands[index] = .{
                .id = operand.id,
                .value = try self.cloneExpr(operand.value),
            };
        }
        return try self.pass.program.addCaptureOperandSpan(operands);
    }

    fn clonePatSpan(self: *Cloner, span: Ast.Span(Ast.PatId), mode: BinderCloneMode) Common.LowerError!Ast.Span(Ast.PatId) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.PatId, self.pass.program.patSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.PatId, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |pat, index| values[index] = try self.clonePat(pat, mode);
        return try self.pass.program.addPatSpan(values);
    }

    fn cloneFieldExprSpan(self: *Cloner, span: Ast.Span(Ast.FieldExpr)) Common.LowerError!Ast.Span(Ast.FieldExpr) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.FieldExpr, self.pass.program.fieldExprSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.FieldExpr, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |field, index| {
            values[index] = .{
                .name = field.name,
                .value = try self.cloneExpr(field.value),
            };
        }
        return try self.pass.program.addFieldExprSpan(values);
    }

    fn cloneRecordDestructSpan(self: *Cloner, span: Ast.Span(Ast.RecordDestruct), mode: BinderCloneMode) Common.LowerError!Ast.Span(Ast.RecordDestruct) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.RecordDestruct, self.pass.program.recordDestructSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.RecordDestruct, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |field, index| {
            values[index] = .{
                .name = field.name,
                .pattern = try self.clonePat(field.pattern, mode),
            };
        }
        return try self.pass.program.addRecordDestructSpan(values);
    }

    fn cloneBranchSpan(self: *Cloner, span: Ast.Span(Ast.Branch)) Common.LowerError!Ast.Span(Ast.Branch) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.Branch, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |branch, index| {
            const change_start = self.subst.watermark();
            const pat = try self.clonePat(branch.pat, .bind_runtime);
            values[index] = .{
                .pat = pat,
                .bindings = try self.cloneStmtSpan(branch.bindings),
                .guard = if (branch.guard) |guard| try self.cloneExpr(guard) else null,
                .body = try self.cloneExpr(branch.body),
            };
            self.subst.restore(change_start);
        }
        return try self.pass.program.addBranchSpan(values);
    }

    fn cloneIfBranchSpan(self: *Cloner, span: Ast.Span(Ast.IfBranch)) Common.LowerError!Ast.Span(Ast.IfBranch) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.IfBranch, self.pass.program.ifBranchSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.IfBranch, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |branch, index| {
            values[index] = .{
                .cond = try self.cloneExpr(branch.cond),
                .body = try self.cloneExpr(branch.body),
            };
        }
        return try self.pass.program.addIfBranchSpan(values);
    }

    fn materialize(self: *Cloner, value: Value) Common.LowerError!Ast.ExprId {
        switch (value) {
            .expr => |expr| return expr,
            .static_data_candidate => |candidate| {
                if (self.materialize_strip_depth >= value_wrapper_strip_cap) {
                    Common.invariant("materialize followed a static-data runtime chain past the strip cap; a cyclic value reached materialization");
                }
                self.materialize_strip_depth += 1;
                defer self.materialize_strip_depth -= 1;
                return try self.addExpr(.{ .ty = candidate.ty, .data = .{ .static_data_candidate = .{
                    .static_data = candidate.static_data,
                    .runtime_expr = try self.materialize(candidate.runtime.*),
                } } });
            },
            .tag => |tag| {
                const payloads = try self.pass.allocator.alloc(Ast.ExprId, tag.payloads.len);
                defer self.pass.allocator.free(payloads);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = try self.materialize(payload);
                }
                return try self.addExpr(.{ .ty = tag.ty, .data = .{ .tag = .{
                    .name = tag.name,
                    .payloads = try self.pass.program.addExprSpan(payloads),
                } } });
            },
            .record => |record| {
                const fields = try self.pass.allocator.alloc(Ast.FieldExpr, record.fields.len);
                defer self.pass.allocator.free(fields);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.materialize(field.value),
                    };
                }
                return try self.addExpr(.{ .ty = record.ty, .data = .{
                    .record = try self.pass.program.addFieldExprSpan(fields),
                } });
            },
            .tuple => |tuple| {
                const items = try self.pass.allocator.alloc(Ast.ExprId, tuple.items.len);
                defer self.pass.allocator.free(items);
                for (tuple.items, 0..) |item, index| {
                    items[index] = try self.materialize(item);
                }
                return try self.addExpr(.{ .ty = tuple.ty, .data = .{
                    .tuple = try self.pass.program.addExprSpan(items),
                } });
            },
            .nominal => |nominal| {
                if (self.materialize_strip_depth >= value_wrapper_strip_cap) {
                    Common.invariant("materialize followed a nominal backing chain past the strip cap; a cyclic value reached materialization");
                }
                self.materialize_strip_depth += 1;
                defer self.materialize_strip_depth -= 1;
                return try self.addExpr(.{ .ty = nominal.ty, .data = .{
                    .nominal = try self.materialize(nominal.backing.*),
                } });
            },
            .callable => |callable| return try self.materializeCallable(callable),
        }
    }

    fn materializeCallable(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
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
            if (value != .expr) {
                all_original = false;
                break;
            }
            const expr = value.expr;
            const local = localExpr(self.pass.program, expr) orelse {
                all_original = false;
                break;
            };
            if (local != capture.local) {
                all_original = false;
                break;
            }
        }

        if (!all_original and self.emit_callable_workers) return try self.materializeCallableWorker(callable);

        return try self.materializeCallableWithCaptures(callable.ty, callable.fn_id, fn_.captures, callable.captures);
    }

    fn activeRecursiveFieldTupleReadRoot(self: *Cloner, value: Value) ?Ast.ExprId {
        if (value != .expr) return null;
        const expr_id = value.expr;
        const data = self.pass.program.getExpr(expr_id).data;
        if (data != .field_access and data != .tuple_access) return null;
        return self.activeRecursiveFieldTupleReadBase(expr_id);
    }

    fn activeRecursiveFieldTupleReadBase(self: *Cloner, expr_id: Ast.ExprId) ?Ast.ExprId {
        const data = self.pass.program.getExpr(expr_id).data;
        if (data == .local) return if (self.active_recursive_value_locals.contains(data.local)) expr_id else null;
        if (data == .field_access) return self.activeRecursiveFieldTupleReadBase(data.field_access.receiver);
        if (data == .tuple_access) return self.activeRecursiveFieldTupleReadBase(data.tuple_access.tuple);
        return null;
    }

    fn cloneFieldTupleReadReplacingRoot(
        self: *Cloner,
        source: Ast.ExprId,
        root: Ast.ExprId,
        replacement: Ast.ExprId,
    ) Common.LowerError!Ast.ExprId {
        if (source == root) return replacement;
        const expr = self.pass.program.getExpr(source);
        if (expr.data == .field_access) {
            const field = expr.data.field_access;
            return try self.addExpr(.{
                .ty = expr.ty,
                .data = .{
                    .field_access = .{
                        .receiver = try self.cloneFieldTupleReadReplacingRoot(field.receiver, root, replacement),
                        // The clone stays within `pass.program`, so the source span
                        // remains valid for the replacement expression.
                        .segments = field.segments,
                    },
                },
            });
        }
        if (expr.data == .tuple_access) {
            const access = expr.data.tuple_access;
            return try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                .tuple = try self.cloneFieldTupleReadReplacingRoot(access.tuple, root, replacement),
                .elem_index = access.elem_index,
            } } });
        }
        Common.invariant("recursive field/tuple read replacement reached a non-access expression before its root");
    }

    fn materializeCallableWorker(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
        const source_fn_id = self.pass.callable_sources.get(callable.fn_id) orelse callable.fn_id;
        const source_fn = self.pass.program.getFn(source_fn_id);
        const source_captures = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(source_captures);
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        const worker_key: CallableWorkerIdentity = .{
            .template = Mono.fnTemplateDigest(
                source_fn.source orelse Common.invariant("rewritten callable source had no Monotype template identity"),
                &self.pass.program.types,
                &self.pass.program.names,
            ),
            .callable_abi = self.pass.program.types.typeDigest(&self.pass.program.names, callable.ty),
            .capture_abi = self.callableCaptureAbiDigest(source_captures, callable.captures),
        };
        if (self.pass.callable_workers.get(worker_key)) |worker_fn_id| {
            const worker = self.pass.program.getFn(worker_fn_id);
            return try self.materializeCallableWithCaptures(callable.ty, worker_fn_id, worker.captures, callable.captures);
        }

        const source_body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => Common.invariant("hosted callable value needed a rewritten body"),
        };
        // Capture locals are the worker's dynamic inputs. Preserve each
        // source capture's complete identity, but give its slot the exact type
        // of the rewritten operand that this worker body consumes.
        const worker_captures = try self.pass.allocator.alloc(Ast.TypedLocal, source_captures.len);
        defer self.pass.allocator.free(worker_captures);
        const worker_capture_values = try self.pass.allocator.alloc(CaptureValue, source_captures.len);
        defer self.pass.allocator.free(worker_capture_values);
        const worker_body_values = try self.pass.allocator.alloc(Value, source_captures.len);
        defer self.pass.allocator.free(worker_body_values);
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
                if (capture_value != .expr) unreachable;
                const source_expr = capture_value.expr;
                worker_capture_values[index] = .{ .id = id, .value = .{ .expr = root } };
                worker_body_values[index] = .{ .expr = try self.cloneFieldTupleReadReplacingRoot(source_expr, root, local_expr) };
            } else {
                worker_capture_values[index] = .{ .id = id, .value = capture_value };
                worker_body_values[index] = .{ .expr = local_expr };
            }
        }
        const captures_span = try self.pass.program.addTypedLocalSpan(worker_captures);

        const source_args = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try self.pass.allocator.alloc(Ast.TypedLocal, source_args.len);
        defer self.pass.allocator.free(args);
        for (source_args, 0..) |source_arg, index| {
            const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), source_arg.ty);
            args[index] = .{ .local = local, .ty = source_arg.ty };
        }
        const args_span = try self.pass.program.addTypedLocalSpan(args);

        // Reserve and index the worker before cloning. Recursive references
        // therefore reuse this exact function id, and cloning can never start
        // from a worker produced by an earlier materialization.
        const symbol = self.pass.symbols.fresh();
        const worker_fn_id = try self.pass.program.addFn(.{
            .symbol = symbol,
            .source = source_fn.source,
            .signature = null,
            .args = args_span,
            .captures = captures_span,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        try self.pass.callable_workers.put(worker_key, worker_fn_id);
        try self.pass.callable_sources.put(worker_fn_id, source_fn_id);
        try self.pass.copyProcDebugName(source_fn.symbol, symbol);

        const change_start = self.subst.watermark();
        defer self.subst.restore(change_start);

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
        const saved_strip_depth = self.materialize_strip_depth;
        self.materialize_strip_depth = 0;
        const worker_body = try self.cloneExpr(source_body);
        self.materialize_strip_depth = saved_strip_depth;
        self.pass.program.setFn(worker_fn_id, .{
            .symbol = symbol,
            .source = source_fn.source,
            .signature = null,
            .args = args_span,
            .captures = captures_span,
            .body = .{ .roc = worker_body },
            .ret = source_fn.ret,
        });

        return try self.materializeCallableWithCaptures(
            callable.ty,
            worker_fn_id,
            captures_span,
            worker_capture_values,
        );
    }

    fn callableCaptureAbiDigest(
        self: *Cloner,
        source_captures: []const Ast.TypedLocal,
        values: []const CaptureValue,
    ) names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update("roc.spec_constr.callable_capture_abi.v1");
        var word: [4]u8 = undefined;
        std.mem.writeInt(u32, &word, @intCast(source_captures.len), .little);
        hasher.update(&word);
        for (source_captures) |capture| {
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const value = callableCaptureValueForId(values, id) orelse
                Common.invariant("rewritten callable had no value for a source capture slot");
            std.mem.writeInt(u32, &word, @intFromEnum(id), .little);
            hasher.update(&word);
            const digest = self.pass.program.types.typeDigest(&self.pass.program.names, valueType(self.pass.program, value));
            hasher.update(&digest.bytes);
        }
        return .{ .bytes = hasher.finalResult() };
    }

    fn materializeCallableWithCaptures(
        self: *Cloner,
        ty: Type.TypeId,
        fn_id: Ast.FnId,
        captures_span: Ast.Span(Ast.TypedLocal),
        values: []const CaptureValue,
    ) Common.LowerError!Ast.ExprId {
        const captures = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(captures_span));
        defer self.pass.allocator.free(captures);
        if (captures.len != values.len) {
            Common.invariant("callable value capture count differed from specialized function capture count");
        }

        const operands = try self.pass.allocator.alloc(Ast.CaptureOperand, captures.len);
        defer self.pass.allocator.free(operands);
        for (captures, 0..) |capture, index| {
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const value = callableCaptureValueForId(values, id) orelse
                Common.invariant("specialized callable had no value for a capture slot");
            const value_expr = blk: {
                if (self.materialize_strip_depth >= value_wrapper_strip_cap) {
                    Common.invariant("materialize followed a callable capture chain past the strip cap; a cyclic value reached materialization");
                }
                self.materialize_strip_depth += 1;
                defer self.materialize_strip_depth -= 1;
                break :blk try self.materialize(value);
            };
            const value_local = localExpr(self.pass.program, value_expr);
            const operand_value = if (value_local != null and value_local.? == capture.local)
                try self.addExpr(.{ .ty = capture.ty, .data = .{ .local = capture.local } })
            else
                value_expr;
            operands[index] = .{ .id = id, .value = operand_value };
        }

        return try self.addExpr(.{ .ty = ty, .data = .{ .fn_ref = .{
            .fn_id = fn_id,
            .captures = try self.pass.program.addCaptureOperandSpan(operands),
        } } });
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
            try self.rebased_inline_scopes.put(.{ .source = src, .outer = outer }, rebased);
            try self.inline_scope_origins.put(rebased, src);
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
            .source_loc = switch (source_fn.body) {
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
/// ascends, mirroring `lift.zig`'s capture walk (`collectExpr`/`collectStmt`/
/// `bindPat`) so the same reference set is judged, but asserting membership
/// rather than recording free variables.
const BodyLocalScope = struct {
    program: *const Ast.Program,
    allocator: Allocator,
    fn_index: usize,
    bound: collections.DenseMap(Ast.LocalId, u32),
    joins: collections.DenseMap(Ast.JoinPointId, u32),

    fn checkUse(self: *BodyLocalScope, local: Ast.LocalId) void {
        if (self.bound.contains(local)) return;
        const func = self.program.getFnAt(self.fn_index);
        Common.invariantFmt(
            "rewritten fn {d} (symbol {d}) references local {d} (`{s}`) bound by no enclosing scope, argument, or capture",
            .{ self.fn_index, @intFromEnum(func.symbol), @intFromEnum(local), self.program.localName(local) },
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

    fn bindTypedLocals(self: *BodyLocalScope, span: Ast.Span(Ast.TypedLocal), added: *std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        const locals = self.program.typedLocalSpan(span);
        for (0..locals.len) |index| {
            const local = GuardedList.at(locals, index).local;
            try self.bind(local);
            try added.append(self.allocator, local);
        }
    }

    fn bindPat(self: *BodyLocalScope, pat_id: Ast.PatId, added: *std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        switch (self.program.getPat(pat_id).data) {
            .bind => |local| {
                try self.bind(local);
                try added.append(self.allocator, local);
            },
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .str_pattern => |str| {
                const steps = self.program.strPatternStepSpan(str.steps);
                for (0..steps.len) |index| {
                    if (GuardedList.at(steps, index).capture) |capture| try self.bindPat(capture, added);
                }
            },
            .as => |as| {
                try self.bindPat(as.pattern, added);
                try self.bind(as.local);
                try added.append(self.allocator, as.local);
            },
            .record => |fields| {
                const destructs = self.program.recordDestructSpan(fields);
                for (0..destructs.len) |index| try self.bindPat(GuardedList.at(destructs, index).pattern, added);
            },
            .tuple => |items| {
                const children = self.program.patSpan(items);
                for (0..children.len) |index| try self.bindPat(GuardedList.at(children, index), added);
            },
            .list => |list| {
                const children = self.program.patSpan(list.patterns);
                for (0..children.len) |index| try self.bindPat(GuardedList.at(children, index), added);
                if (list.rest) |rest| if (rest.pattern) |rest_pattern| try self.bindPat(rest_pattern, added);
            },
            .tag => |tag| {
                const payloads = self.program.patSpan(tag.payloads);
                for (0..payloads.len) |index| try self.bindPat(GuardedList.at(payloads, index), added);
            },
            .nominal => |backing| try self.bindPat(backing, added),
        }
    }

    fn walkExprSpan(self: *BodyLocalScope, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        const values = self.program.exprSpan(span);
        for (0..values.len) |index| try self.walkExpr(GuardedList.at(values, index));
    }

    fn walkStmt(self: *BodyLocalScope, stmt_id: Ast.StmtId, added: *std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        switch (self.program.getStmt(stmt_id)) {
            .uninitialized => |pat| try self.bindPat(pat, added),
            .let_ => |let_| {
                if (let_.recursive) {
                    try self.bindPat(let_.pat, added);
                    try self.walkExpr(let_.value);
                } else {
                    try self.walkExpr(let_.value);
                    try self.bindPat(let_.pat, added);
                }
            },
            .expr,
            .expect,
            .dbg,
            => |expr| try self.walkExpr(expr),
            .return_ => |ret| try self.walkExpr(ret.value),
            .crash => {},
        }
    }

    fn walkExpr(self: *BodyLocalScope, expr_id: Ast.ExprId) Allocator.Error!void {
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
            .comptime_exhaustiveness_failed,
            .@"unreachable",
            => {},
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached body-local validation"),
            .fn_ref => |fn_ref| {
                const operands = self.program.captureOperandSpan(fn_ref.captures);
                for (0..operands.len) |index| try self.walkExpr(GuardedList.at(operands, index).value);
            },
            .list,
            .tuple,
            => |items| try self.walkExprSpan(items),
            .record => |fields| {
                const field_exprs = self.program.fieldExprSpan(fields);
                for (0..field_exprs.len) |index| try self.walkExpr(GuardedList.at(field_exprs, index).value);
            },
            .record_update => |update| {
                try self.walkExpr(update.base);
                const field_exprs = self.program.fieldExprSpan(update.fields);
                for (0..field_exprs.len) |index| try self.walkExpr(GuardedList.at(field_exprs, index).value);
            },
            .tag => |tag| try self.walkExprSpan(tag.payloads),
            .static_data_candidate => |candidate| try self.walkExpr(candidate.runtime_expr),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.walkExpr(child),
            .return_ => |ret| try self.walkExpr(ret.value),
            .expect_err => |expect_err| try self.walkExpr(expect_err.msg),
            .comptime_branch_taken => |taken| try self.walkExpr(taken.body),
            .let_ => |let_| {
                try self.walkExpr(let_.value);
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.allocator);
                try self.bindPat(let_.bind, &added);
                try self.walkExpr(let_.rest);
                self.unbindAll(added.items);
            },
            .call_value => |call| {
                try self.walkExpr(call.callee);
                try self.walkExprSpan(call.args);
            },
            .call_proc => |call| {
                try self.walkExprSpan(call.args);
                const operands = self.program.captureOperandSpan(call.captures);
                for (0..operands.len) |index| try self.walkExpr(GuardedList.at(operands, index).value);
            },
            .low_level => |call| try self.walkExprSpan(call.args),
            .field_access => |field| try self.walkExpr(field.receiver),
            .tuple_access => |access| try self.walkExpr(access.tuple),
            .structural_eq => |eq| {
                try self.walkExpr(eq.lhs);
                try self.walkExpr(eq.rhs);
            },
            .structural_hash => |hash| {
                try self.walkExpr(hash.value);
                try self.walkExpr(hash.hasher);
            },
            .match_ => |match| {
                try self.walkExpr(match.scrutinee);
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    var added: std.ArrayList(Ast.LocalId) = .empty;
                    defer added.deinit(self.allocator);
                    try self.bindPat(branch.pat, &added);
                    const bindings = self.program.stmtSpan(branch.bindings);
                    for (0..bindings.len) |binding_index| {
                        try self.walkStmt(GuardedList.at(bindings, binding_index), &added);
                    }
                    if (branch.guard) |guard| try self.walkExpr(guard);
                    try self.walkExpr(branch.body);
                    self.unbindAll(added.items);
                }
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    try self.walkExpr(branch.cond);
                    try self.walkExpr(branch.body);
                }
                try self.walkExpr(if_.final_else);
            },
            .if_initialized_payload => |payload_switch| {
                try self.walkExpr(payload_switch.cond);
                self.checkUse(payload_switch.payload);
                try self.walkExpr(payload_switch.initialized);
                try self.walkExpr(payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.walkExpr(sequence.try_expr);
                try self.bind(sequence.ok_local);
                try self.walkExpr(sequence.ok_body);
                self.unbind(sequence.ok_local);
            },
            .try_record_sequence => |sequence| {
                try self.walkExpr(sequence.try_expr);
                try self.bind(sequence.value_local);
                try self.bind(sequence.rest_local);
                try self.walkExpr(sequence.ok_body);
                self.unbind(sequence.rest_local);
                self.unbind(sequence.value_local);
            },
            .block => |block| {
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.allocator);
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| try self.walkStmt(GuardedList.at(statements, index), &added);
                try self.walkExpr(block.final_expr);
                self.unbindAll(added.items);
            },
            .loop_ => |loop| {
                try self.walkExprSpan(loop.initial_values);
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.allocator);
                try self.bindTypedLocals(loop.params, &added);
                try self.walkExpr(loop.body);
                self.unbindAll(added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.walkExpr(value),
            .continue_ => |continue_| try self.walkExprSpan(continue_.values),
            .join_point => |join_point| {
                const join_entry = try self.joins.getOrPut(join_point.id);
                if (join_entry.found_existing) {
                    Common.invariant("rewritten body redeclared an active join point id");
                }
                join_entry.value_ptr.* = join_point.params.len;
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.allocator);
                try self.bindTypedLocals(join_point.params, &added);
                try self.walkExpr(join_point.body);
                self.unbindAll(added.items);
                try self.walkExpr(join_point.remainder);
                if (!self.joins.remove(join_point.id)) {
                    Common.invariant("rewritten body lost an active join point during validation");
                }
            },
            .jump => |jump| {
                const arity = self.joins.get(jump.target) orelse
                    Common.invariant("rewritten body jumped to a join point outside lexical scope");
                if (arity != jump.args.len) {
                    Common.invariant("rewritten body jump arity differed from its join point parameters");
                }
                try self.walkExprSpan(jump.args);
            },
        }
    }
};

fn localExpr(program: *const Ast.Program, expr_id: Ast.ExprId) ?Ast.LocalId {
    const data = program.getExpr(expr_id).data;
    return if (data == .local) data.local else null;
}

fn fnBodySizeWithin(program: *const Ast.Program, body: Ast.FnBody, limit: usize) BodySize {
    return switch (body) {
        .roc => |expr| exprBodySizeWithin(program, expr, limit),
        .hosted => .{ .exact = 0 },
    };
}

fn exprBodySizeWithin(program: *const Ast.Program, expr_id: Ast.ExprId, limit: usize) BodySize {
    var counter = BodySizeCounter{
        .program = program,
        .remaining = limit,
    };
    counter.countExpr(expr_id);
    return if (counter.over_limit) .over_limit else .{ .exact = limit - counter.remaining };
}

const BodySizeCounter = struct {
    program: *const Ast.Program,
    remaining: usize,
    over_limit: bool = false,

    fn spend(self: *BodySizeCounter) bool {
        if (self.over_limit) return false;
        if (self.remaining == 0) {
            self.over_limit = true;
            return false;
        }
        self.remaining -= 1;
        return true;
    }

    fn countExprSpan(self: *BodySizeCounter, span: Ast.Span(Ast.ExprId)) void {
        const exprs = self.program.exprSpan(span);
        for (0..exprs.len) |index| self.countExpr(GuardedList.at(exprs, index));
    }

    fn countCaptureOperandSpan(self: *BodySizeCounter, span: Ast.Span(Ast.CaptureOperand)) void {
        const operands = self.program.captureOperandSpan(span);
        for (0..operands.len) |index| self.countExpr(GuardedList.at(operands, index).value);
    }

    fn countStmt(self: *BodySizeCounter, stmt_id: Ast.StmtId) void {
        switch (self.program.getStmt(stmt_id)) {
            .let_ => |let_| self.countExpr(let_.value),
            .expr,
            .expect,
            .dbg,
            => |expr| self.countExpr(expr),
            .return_ => |ret| self.countExpr(ret.value),
            .uninitialized, .crash => {},
        }
    }

    fn countExpr(self: *BodySizeCounter, expr_id: Ast.ExprId) void {
        if (!self.spend()) return;
        switch (self.program.getExpr(expr_id).data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .crash,
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            => {},
            .fn_ref => |fn_ref| self.countCaptureOperandSpan(fn_ref.captures),
            .list,
            .tuple,
            => |items| self.countExprSpan(items),
            .record => |fields| {
                const field_exprs = self.program.fieldExprSpan(fields);
                for (0..field_exprs.len) |index| self.countExpr(GuardedList.at(field_exprs, index).value);
            },
            .record_update => |update| {
                self.countExpr(update.base);
                const field_exprs = self.program.fieldExprSpan(update.fields);
                for (0..field_exprs.len) |index| self.countExpr(GuardedList.at(field_exprs, index).value);
            },
            .tag => |tag| self.countExprSpan(tag.payloads),
            .static_data_candidate => |candidate| self.countExpr(candidate.runtime_expr),
            .nominal,
            .dbg,
            .expect,
            => |child| self.countExpr(child),
            .return_ => |ret| self.countExpr(ret.value),
            .expect_err => |expect_err| self.countExpr(expect_err.msg),
            .comptime_branch_taken => |taken| self.countExpr(taken.body),
            .let_ => |let_| {
                self.countExpr(let_.value);
                self.countExpr(let_.rest);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached SpecConstr body-size counting"),
            .call_value => |call| {
                self.countExpr(call.callee);
                self.countExprSpan(call.args);
            },
            .call_proc => |call| {
                self.countExprSpan(call.args);
                self.countCaptureOperandSpan(call.captures);
            },
            .low_level => |call| self.countExprSpan(call.args),
            .field_access => |field| self.countExpr(field.receiver),
            .tuple_access => |access| self.countExpr(access.tuple),
            .structural_eq => |eq| {
                self.countExpr(eq.lhs);
                self.countExpr(eq.rhs);
            },
            .structural_hash => |hash| {
                self.countExpr(hash.value);
                self.countExpr(hash.hasher);
            },
            .match_ => |match| {
                self.countExpr(match.scrutinee);
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    const bindings = self.program.stmtSpan(branch.bindings);
                    for (0..bindings.len) |binding_index| self.countStmt(GuardedList.at(bindings, binding_index));
                    if (branch.guard) |guard| self.countExpr(guard);
                    self.countExpr(branch.body);
                }
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    self.countExpr(branch.cond);
                    self.countExpr(branch.body);
                }
                self.countExpr(if_.final_else);
            },
            .if_initialized_payload => |payload_switch| {
                self.countExpr(payload_switch.cond);
                self.countExpr(payload_switch.initialized);
                self.countExpr(payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                self.countExpr(sequence.try_expr);
                self.countExpr(sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                self.countExpr(sequence.try_expr);
                self.countExpr(sequence.ok_body);
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| self.countStmt(GuardedList.at(statements, index));
                self.countExpr(block.final_expr);
            },
            .loop_ => |loop| {
                self.countExprSpan(loop.initial_values);
                self.countExpr(loop.body);
            },
            .break_ => |maybe| if (maybe) |value| self.countExpr(value),
            .continue_ => |continue_| self.countExprSpan(continue_.values),
            .join_point => |join_point| {
                self.countExpr(join_point.body);
                self.countExpr(join_point.remainder);
            },
            .jump => |jump| self.countExprSpan(jump.args),
        }
    }
};

const FnUseSummary = struct {
    external_calls: usize = 0,
    external_call_expr: ?Ast.ExprId = null,
    value_refs: usize = 0,
};

/// Program-wide procedure-use snapshot shared by SpecConstr graph rewrites.
/// A transformation that changes edges requires a fresh snapshot; consumers
/// never combine usage counts from different program generations.
const ProgramProcedureUsage = struct {
    contains_return: []bool,
    tail_self_calls: []TailSelfCallSummary,
    fn_uses: []FnUseSummary,

    fn collect(allocator: Allocator, program: *const Ast.Program) Allocator.Error!ProgramProcedureUsage {
        const fn_count = program.fnCount();
        const contains_return = try allocator.alloc(bool, fn_count);
        errdefer allocator.free(contains_return);
        @memset(contains_return, false);
        const tail_self_calls = try allocator.alloc(TailSelfCallSummary, fn_count);
        errdefer allocator.free(tail_self_calls);
        @memset(tail_self_calls, .{});
        const fn_uses = try allocator.alloc(FnUseSummary, fn_count);
        errdefer allocator.free(fn_uses);
        @memset(fn_uses, .{});

        for (0..fn_count) |owner_index| {
            const owner: Ast.FnId = @enumFromInt(@as(u32, @intCast(owner_index)));
            const body = switch (program.getFnAt(owner_index).body) {
                .roc => |body| body,
                .hosted => continue,
            };
            contains_return[owner_index] = exprContainsReturn(program, body);
            tail_self_calls[owner_index] = tailSelfCallSummary(program, body, owner);
            collectAllFnUsesInExpr(program, body, owner, fn_uses);
        }
        for (program.rootsView()) |root| {
            fn_uses[@intFromEnum(root.fn_id)].value_refs += 1;
        }
        return .{
            .contains_return = contains_return,
            .tail_self_calls = tail_self_calls,
            .fn_uses = fn_uses,
        };
    }

    fn deinit(self: *ProgramProcedureUsage, allocator: Allocator) void {
        allocator.free(self.fn_uses);
        allocator.free(self.tail_self_calls);
        allocator.free(self.contains_return);
        self.* = undefined;
    }
};

fn collectAllFnUsesInExpr(
    program: *const Ast.Program,
    expr_id: Ast.ExprId,
    owner: Ast.FnId,
    uses: []FnUseSummary,
) void {
    switch (program.getExpr(expr_id).data) {
        .local,
        .unit,
        .@"unreachable",
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        => {},
        .fn_ref => |fn_ref| {
            uses[@intFromEnum(fn_ref.fn_id)].value_refs += 1;
            collectAllFnUsesInCaptureOperands(program, fn_ref.captures, owner, uses);
        },
        .list,
        .tuple,
        => |items| collectAllFnUsesInExprSpan(program, items, owner, uses),
        .record => |fields| {
            const values = program.fieldExprSpan(fields);
            for (0..values.len) |index| {
                collectAllFnUsesInExpr(program, GuardedList.at(values, index).value, owner, uses);
            }
        },
        .record_update => |update| {
            collectAllFnUsesInExpr(program, update.base, owner, uses);
            const values = program.fieldExprSpan(update.fields);
            for (0..values.len) |index| {
                collectAllFnUsesInExpr(program, GuardedList.at(values, index).value, owner, uses);
            }
        },
        .tag => |tag| collectAllFnUsesInExprSpan(program, tag.payloads, owner, uses),
        .static_data_candidate => |candidate| collectAllFnUsesInExpr(program, candidate.runtime_expr, owner, uses),
        .nominal,
        .dbg,
        .expect,
        => |child| collectAllFnUsesInExpr(program, child, owner, uses),
        .return_ => |ret| collectAllFnUsesInExpr(program, ret.value, owner, uses),
        .expect_err => |expect_err| collectAllFnUsesInExpr(program, expect_err.msg, owner, uses),
        .comptime_branch_taken => |taken| collectAllFnUsesInExpr(program, taken.body, owner, uses),
        .let_ => |let_| {
            collectAllFnUsesInExpr(program, let_.value, owner, uses);
            collectAllFnUsesInExpr(program, let_.rest, owner, uses);
        },
        .lambda,
        .def_ref,
        .fn_def,
        => Common.invariant("pre-lift function expression reached specialized-worker use analysis"),
        .call_value => |call| {
            collectAllFnUsesInExpr(program, call.callee, owner, uses);
            collectAllFnUsesInExprSpan(program, call.args, owner, uses);
        },
        .call_proc => |call| {
            if (Ast.localDirectCallee(call)) |callee| {
                if (callee != owner) {
                    const summary = &uses[@intFromEnum(callee)];
                    summary.external_calls += 1;
                    summary.external_call_expr = expr_id;
                }
            }
            collectAllFnUsesInExprSpan(program, call.args, owner, uses);
            collectAllFnUsesInCaptureOperands(program, call.captures, owner, uses);
        },
        .low_level => |call| collectAllFnUsesInExprSpan(program, call.args, owner, uses),
        .field_access => |field| collectAllFnUsesInExpr(program, field.receiver, owner, uses),
        .tuple_access => |access| collectAllFnUsesInExpr(program, access.tuple, owner, uses),
        .structural_eq => |eq| {
            collectAllFnUsesInExpr(program, eq.lhs, owner, uses);
            collectAllFnUsesInExpr(program, eq.rhs, owner, uses);
        },
        .structural_hash => |hash| {
            collectAllFnUsesInExpr(program, hash.value, owner, uses);
            collectAllFnUsesInExpr(program, hash.hasher, owner, uses);
        },
        .match_ => |match| {
            collectAllFnUsesInExpr(program, match.scrutinee, owner, uses);
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    collectAllFnUsesInStmt(program, GuardedList.at(bindings, binding_index), owner, uses);
                }
                if (branch.guard) |guard| collectAllFnUsesInExpr(program, guard, owner, uses);
                collectAllFnUsesInExpr(program, branch.body, owner, uses);
            }
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                collectAllFnUsesInExpr(program, branch.cond, owner, uses);
                collectAllFnUsesInExpr(program, branch.body, owner, uses);
            }
            collectAllFnUsesInExpr(program, if_.final_else, owner, uses);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                collectAllFnUsesInStmt(program, GuardedList.at(statements, index), owner, uses);
            }
            collectAllFnUsesInExpr(program, block.final_expr, owner, uses);
        },
        .loop_ => |loop| {
            collectAllFnUsesInExprSpan(program, loop.initial_values, owner, uses);
            collectAllFnUsesInExpr(program, loop.body, owner, uses);
        },
        .break_ => |maybe| if (maybe) |value| collectAllFnUsesInExpr(program, value, owner, uses),
        .continue_ => |continue_| collectAllFnUsesInExprSpan(program, continue_.values, owner, uses),
        .join_point => |join_point| {
            collectAllFnUsesInExpr(program, join_point.body, owner, uses);
            collectAllFnUsesInExpr(program, join_point.remainder, owner, uses);
        },
        .jump => |jump| collectAllFnUsesInExprSpan(program, jump.args, owner, uses),
        .if_initialized_payload => |payload_switch| {
            collectAllFnUsesInExpr(program, payload_switch.cond, owner, uses);
            collectAllFnUsesInExpr(program, payload_switch.initialized, owner, uses);
            collectAllFnUsesInExpr(program, payload_switch.uninitialized, owner, uses);
        },
        .try_sequence => |sequence| {
            collectAllFnUsesInExpr(program, sequence.try_expr, owner, uses);
            collectAllFnUsesInExpr(program, sequence.ok_body, owner, uses);
        },
        .try_record_sequence => |sequence| {
            collectAllFnUsesInExpr(program, sequence.try_expr, owner, uses);
            collectAllFnUsesInExpr(program, sequence.ok_body, owner, uses);
        },
    }
}

fn collectAllFnUsesInExprSpan(program: *const Ast.Program, span: Ast.Span(Ast.ExprId), owner: Ast.FnId, uses: []FnUseSummary) void {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| collectAllFnUsesInExpr(program, GuardedList.at(exprs, index), owner, uses);
}

fn collectAllFnUsesInCaptureOperands(program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand), owner: Ast.FnId, uses: []FnUseSummary) void {
    const operands = program.captureOperandSpan(span);
    for (0..operands.len) |index| collectAllFnUsesInExpr(program, GuardedList.at(operands, index).value, owner, uses);
}

fn collectAllFnUsesInStmt(program: *const Ast.Program, stmt_id: Ast.StmtId, owner: Ast.FnId, uses: []FnUseSummary) void {
    switch (program.getStmt(stmt_id)) {
        .let_ => |let_| collectAllFnUsesInExpr(program, let_.value, owner, uses),
        .expr, .expect, .dbg => |expr| collectAllFnUsesInExpr(program, expr, owner, uses),
        .return_ => |ret| collectAllFnUsesInExpr(program, ret.value, owner, uses),
        .uninitialized, .crash => {},
    }
}

const TailSelfCallSummary = struct {
    valid: bool = true,
    count: usize = 0,

    fn merge(self: *TailSelfCallSummary, other: TailSelfCallSummary) void {
        self.valid = self.valid and other.valid;
        self.count += other.count;
    }
};

/// Prove that every self call occurs in a result position from which the
/// worker returns directly. The accepted set is the exact lifted-IR
/// tail-position grammar; any self call in an operand or statement rejects
/// localization.
fn tailSelfCallSummary(program: *const Ast.Program, expr_id: Ast.ExprId, target: Ast.FnId) TailSelfCallSummary {
    const expr = program.getExpr(expr_id);
    return switch (expr.data) {
        .call_proc => |call| blk: {
            if (exprSpanCallsFn(program, call.args, target) or
                captureOperandSpanCallsFn(program, call.captures, target))
            {
                break :blk .{ .valid = false };
            }
            break :blk if (Ast.localDirectCallee(call) == target)
                .{ .count = 1 }
            else
                .{};
        },
        .let_ => |let_| if (exprCallsFn(program, let_.value, target))
            .{ .valid = false }
        else
            tailSelfCallSummary(program, let_.rest, target),
        .match_ => |match| blk: {
            if (exprCallsFn(program, match.scrutinee, target)) break :blk .{ .valid = false };
            var summary: TailSelfCallSummary = .{};
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (stmtCallsFn(program, GuardedList.at(bindings, binding_index), target)) break :blk .{ .valid = false };
                }
                if (branch.guard) |guard| {
                    if (exprCallsFn(program, guard, target)) break :blk .{ .valid = false };
                }
                summary.merge(tailSelfCallSummary(program, branch.body, target));
            }
            break :blk summary;
        },
        .if_ => |if_| blk: {
            var summary: TailSelfCallSummary = .{};
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (exprCallsFn(program, branch.cond, target)) break :blk .{ .valid = false };
                summary.merge(tailSelfCallSummary(program, branch.body, target));
            }
            summary.merge(tailSelfCallSummary(program, if_.final_else, target));
            break :blk summary;
        },
        .block => |block| blk: {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                if (stmtCallsFn(program, GuardedList.at(statements, index), target)) {
                    break :blk .{ .valid = false };
                }
            }
            break :blk tailSelfCallSummary(program, block.final_expr, target);
        },
        .join_point => |join_point| blk: {
            var summary = tailSelfCallSummary(program, join_point.body, target);
            summary.merge(tailSelfCallSummary(program, join_point.remainder, target));
            break :blk summary;
        },
        .if_initialized_payload => |payload_switch| blk: {
            if (exprCallsFn(program, payload_switch.cond, target)) break :blk .{ .valid = false };
            var summary = tailSelfCallSummary(program, payload_switch.initialized, target);
            summary.merge(tailSelfCallSummary(program, payload_switch.uninitialized, target));
            break :blk summary;
        },
        .try_sequence => |sequence| if (exprCallsFn(program, sequence.try_expr, target))
            .{ .valid = false }
        else
            tailSelfCallSummary(program, sequence.ok_body, target),
        .try_record_sequence => |sequence| if (exprCallsFn(program, sequence.try_expr, target))
            .{ .valid = false }
        else
            tailSelfCallSummary(program, sequence.ok_body, target),
        .comptime_branch_taken => |taken| tailSelfCallSummary(program, taken.body, target),
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
        .comptime_exhaustiveness_failed,
        .dbg,
        .expect_err,
        .expect,
        => if (exprCallsFn(program, expr_id, target)) .{ .valid = false } else .{},
    };
}

fn exprCallsFn(program: *const Ast.Program, expr_id: Ast.ExprId, fn_id: Ast.FnId) bool {
    return switch (program.getExpr(expr_id).data) {
        .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .crash, .comptime_exhaustiveness_failed, .uninitialized, .uninitialized_payload => false,
        .fn_ref => |fn_ref| captureOperandSpanCallsFn(program, fn_ref.captures, fn_id),
        .list, .tuple => |items| exprSpanCallsFn(program, items, fn_id),
        .record => |fields| blk: {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                if (exprCallsFn(program, GuardedList.at(field_exprs, index).value, fn_id)) break :blk true;
            }
            break :blk false;
        },
        .record_update => |update| blk: {
            if (exprCallsFn(program, update.base, fn_id)) break :blk true;
            const field_exprs = program.fieldExprSpan(update.fields);
            for (0..field_exprs.len) |index| {
                if (exprCallsFn(program, GuardedList.at(field_exprs, index).value, fn_id)) break :blk true;
            }
            break :blk false;
        },
        .tag => |tag| exprSpanCallsFn(program, tag.payloads, fn_id),
        .static_data_candidate => |candidate| exprCallsFn(program, candidate.runtime_expr, fn_id),
        .nominal, .dbg, .expect => |child| exprCallsFn(program, child, fn_id),
        .return_ => |ret| exprCallsFn(program, ret.value, fn_id),
        .expect_err => |expect_err| exprCallsFn(program, expect_err.msg, fn_id),
        .comptime_branch_taken => |taken| exprCallsFn(program, taken.body, fn_id),
        .let_ => |let_| exprCallsFn(program, let_.value, fn_id) or exprCallsFn(program, let_.rest, fn_id),
        .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached recursive-call scan"),
        .call_value => |call| exprCallsFn(program, call.callee, fn_id) or exprSpanCallsFn(program, call.args, fn_id),
        .call_proc => |call| blk: {
            if (Ast.localDirectCallee(call)) |callee| {
                if (callee == fn_id) break :blk true;
            }
            break :blk exprSpanCallsFn(program, call.args, fn_id) or
                captureOperandSpanCallsFn(program, call.captures, fn_id);
        },
        .low_level => |call| exprSpanCallsFn(program, call.args, fn_id),
        .field_access => |field| exprCallsFn(program, field.receiver, fn_id),
        .tuple_access => |access| exprCallsFn(program, access.tuple, fn_id),
        .structural_eq => |eq| exprCallsFn(program, eq.lhs, fn_id) or exprCallsFn(program, eq.rhs, fn_id),
        .structural_hash => |h| exprCallsFn(program, h.value, fn_id) or exprCallsFn(program, h.hasher, fn_id),
        .match_ => |match| blk: {
            if (exprCallsFn(program, match.scrutinee, fn_id)) break :blk true;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (stmtCallsFn(program, GuardedList.at(bindings, binding_index), fn_id)) break :blk true;
                }
                if (branch.guard) |guard| if (exprCallsFn(program, guard, fn_id)) break :blk true;
                if (exprCallsFn(program, branch.body, fn_id)) break :blk true;
            }
            break :blk false;
        },
        .if_ => |if_| blk: {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (exprCallsFn(program, branch.cond, fn_id) or exprCallsFn(program, branch.body, fn_id)) break :blk true;
            }
            break :blk exprCallsFn(program, if_.final_else, fn_id);
        },
        .block => |block| blk: {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                if (stmtCallsFn(program, GuardedList.at(statements, index), fn_id)) break :blk true;
            }
            break :blk exprCallsFn(program, block.final_expr, fn_id);
        },
        .loop_ => |loop| exprSpanCallsFn(program, loop.initial_values, fn_id) or exprCallsFn(program, loop.body, fn_id),
        .break_ => |maybe| if (maybe) |value| exprCallsFn(program, value, fn_id) else false,
        .continue_ => |continue_| exprSpanCallsFn(program, continue_.values, fn_id),
        .join_point => |join_point| exprCallsFn(program, join_point.body, fn_id) or exprCallsFn(program, join_point.remainder, fn_id),
        .jump => |jump| exprSpanCallsFn(program, jump.args, fn_id),
        .if_initialized_payload => |payload_switch| exprCallsFn(program, payload_switch.cond, fn_id) or
            exprCallsFn(program, payload_switch.initialized, fn_id) or
            exprCallsFn(program, payload_switch.uninitialized, fn_id),
        .try_sequence => |sequence| exprCallsFn(program, sequence.try_expr, fn_id) or exprCallsFn(program, sequence.ok_body, fn_id),
        .try_record_sequence => |sequence| exprCallsFn(program, sequence.try_expr, fn_id) or exprCallsFn(program, sequence.ok_body, fn_id),
    };
}

fn exprSpanCallsFn(program: *const Ast.Program, span: Ast.Span(Ast.ExprId), fn_id: Ast.FnId) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        if (exprCallsFn(program, GuardedList.at(exprs, index), fn_id)) return true;
    }
    return false;
}

fn captureOperandSpanCallsFn(program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand), fn_id: Ast.FnId) bool {
    const operands = program.captureOperandSpan(span);
    for (0..operands.len) |index| {
        if (exprCallsFn(program, GuardedList.at(operands, index).value, fn_id)) return true;
    }
    return false;
}

fn stmtCallsFn(program: *const Ast.Program, stmt_id: Ast.StmtId, fn_id: Ast.FnId) bool {
    return switch (program.getStmt(stmt_id)) {
        .let_ => |let_| exprCallsFn(program, let_.value, fn_id),
        .expr, .expect, .dbg => |expr| exprCallsFn(program, expr, fn_id),
        .return_ => |ret| exprCallsFn(program, ret.value, fn_id),
        .uninitialized, .crash => false,
    };
}

fn exprContainsReturn(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.getExpr(expr_id).data) {
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
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        .lambda,
        .def_ref,
        .fn_def,
        => false,
        .fn_ref => |fn_ref| captureOperandSpanContainsReturn(program, fn_ref.captures),
        .return_ => true,
        .list,
        .tuple,
        => |items| exprSpanContainsReturn(program, items),
        .record => |fields| {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (exprContainsReturn(program, field.value)) return true;
            }
            return false;
        },
        .record_update => |update| {
            if (exprContainsReturn(program, update.base)) return true;
            const field_exprs = program.fieldExprSpan(update.fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (exprContainsReturn(program, field.value)) return true;
            }
            return false;
        },
        .tag => |tag| exprSpanContainsReturn(program, tag.payloads),
        .static_data_candidate => |candidate| exprContainsReturn(program, candidate.runtime_expr),
        .nominal,
        .dbg,
        .expect,
        => |child| exprContainsReturn(program, child),
        .expect_err => |expect_err| exprContainsReturn(program, expect_err.msg),
        .comptime_branch_taken => |taken| exprContainsReturn(program, taken.body),
        .let_ => |let_| exprContainsReturn(program, let_.value) or exprContainsReturn(program, let_.rest),
        .call_value => |call| exprContainsReturn(program, call.callee) or exprSpanContainsReturn(program, call.args),
        .call_proc => |call| exprSpanContainsReturn(program, call.args) or captureOperandSpanContainsReturn(program, call.captures),
        .low_level => |call| exprSpanContainsReturn(program, call.args),
        .field_access => |field| exprContainsReturn(program, field.receiver),
        .tuple_access => |access| exprContainsReturn(program, access.tuple),
        .structural_eq => |eq| exprContainsReturn(program, eq.lhs) or exprContainsReturn(program, eq.rhs),
        .structural_hash => |h| exprContainsReturn(program, h.value) or exprContainsReturn(program, h.hasher),
        .match_ => |match| {
            if (exprContainsReturn(program, match.scrutinee)) return true;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (stmtContainsReturn(program, GuardedList.at(bindings, binding_index))) return true;
                }
                if (branch.guard) |guard| {
                    if (exprContainsReturn(program, guard)) return true;
                }
                if (exprContainsReturn(program, branch.body)) return true;
            }
            return false;
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (exprContainsReturn(program, branch.cond)) return true;
                if (exprContainsReturn(program, branch.body)) return true;
            }
            return exprContainsReturn(program, if_.final_else);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                const stmt = GuardedList.at(statements, index);
                if (stmtContainsReturn(program, stmt)) return true;
            }
            return exprContainsReturn(program, block.final_expr);
        },
        .loop_ => |loop| exprSpanContainsReturn(program, loop.initial_values) or exprContainsReturn(program, loop.body),
        .break_ => |maybe| if (maybe) |value| exprContainsReturn(program, value) else false,
        .continue_ => |continue_| exprSpanContainsReturn(program, continue_.values),
        .join_point => |join_point| exprContainsReturn(program, join_point.body) or exprContainsReturn(program, join_point.remainder),
        .jump => |jump| exprSpanContainsReturn(program, jump.args),
        .if_initialized_payload => |payload_switch| exprContainsReturn(program, payload_switch.cond) or
            exprContainsReturn(program, payload_switch.initialized) or
            exprContainsReturn(program, payload_switch.uninitialized),
        .try_sequence => |sequence| exprContainsReturn(program, sequence.try_expr) or exprContainsReturn(program, sequence.ok_body),
        .try_record_sequence => |sequence| exprContainsReturn(program, sequence.try_expr) or exprContainsReturn(program, sequence.ok_body),
    };
}

fn exprSpanContainsReturn(program: *const Ast.Program, span: Ast.Span(Ast.ExprId)) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        const expr = GuardedList.at(exprs, index);
        if (exprContainsReturn(program, expr)) return true;
    }
    return false;
}

fn captureOperandSpanContainsReturn(program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand)) bool {
    const operands = program.captureOperandSpan(span);
    for (0..GuardedList.borrowLen(operands)) |index| {
        const operand = GuardedList.at(operands, index);
        if (exprContainsReturn(program, operand.value)) return true;
    }
    return false;
}

fn stmtContainsReturn(program: *const Ast.Program, stmt_id: Ast.StmtId) bool {
    return switch (program.getStmt(stmt_id)) {
        .return_ => true,
        .let_ => |let_| exprContainsReturn(program, let_.value),
        .expr,
        .expect,
        .dbg,
        => |expr| exprContainsReturn(program, expr),
        .uninitialized,
        .crash,
        => false,
    };
}

/// Whether `expr_id` contains any reference to `local`, including through
/// nested loops, join points, and closure capture operands. Lambda and
/// function-definition leaves reach enclosing locals only through explicit
/// capture operands, which `fn_ref` and `call_proc` spans carry.
fn exprReferencesLocal(program: *const Ast.Program, expr_id: Ast.ExprId, local: Ast.LocalId) bool {
    return switch (program.getExpr(expr_id).data) {
        .local => |referenced| referenced == local,
        .@"unreachable",
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .lambda,
        .def_ref,
        .fn_def,
        => false,
        .uninitialized_payload => |payload| payload.condition == local,
        .fn_ref => |fn_ref| captureOperandSpanReferencesLocal(program, fn_ref.captures, local),
        .return_ => |ret| exprReferencesLocal(program, ret.value, local),
        .list,
        .tuple,
        => |items| exprSpanReferencesLocal(program, items, local),
        .record => |fields| {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (exprReferencesLocal(program, field.value, local)) return true;
            }
            return false;
        },
        .record_update => |update| {
            if (exprReferencesLocal(program, update.base, local)) return true;
            const field_exprs = program.fieldExprSpan(update.fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (exprReferencesLocal(program, field.value, local)) return true;
            }
            return false;
        },
        .tag => |tag| exprSpanReferencesLocal(program, tag.payloads, local),
        .static_data_candidate => |candidate| exprReferencesLocal(program, candidate.runtime_expr, local),
        .nominal,
        .dbg,
        .expect,
        => |child| exprReferencesLocal(program, child, local),
        .expect_err => |expect_err| exprReferencesLocal(program, expect_err.msg, local),
        .comptime_branch_taken => |taken| exprReferencesLocal(program, taken.body, local),
        .let_ => |let_| exprReferencesLocal(program, let_.value, local) or exprReferencesLocal(program, let_.rest, local),
        .call_value => |call| exprReferencesLocal(program, call.callee, local) or exprSpanReferencesLocal(program, call.args, local),
        .call_proc => |call| exprSpanReferencesLocal(program, call.args, local) or captureOperandSpanReferencesLocal(program, call.captures, local),
        .low_level => |call| exprSpanReferencesLocal(program, call.args, local),
        .field_access => |field| exprReferencesLocal(program, field.receiver, local),
        .tuple_access => |access| exprReferencesLocal(program, access.tuple, local),
        .structural_eq => |eq| exprReferencesLocal(program, eq.lhs, local) or exprReferencesLocal(program, eq.rhs, local),
        .structural_hash => |h| exprReferencesLocal(program, h.value, local) or exprReferencesLocal(program, h.hasher, local),
        .match_ => |match| {
            if (exprReferencesLocal(program, match.scrutinee, local)) return true;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (stmtReferencesLocal(program, GuardedList.at(bindings, binding_index), local)) return true;
                }
                if (branch.guard) |guard| {
                    if (exprReferencesLocal(program, guard, local)) return true;
                }
                if (exprReferencesLocal(program, branch.body, local)) return true;
            }
            return false;
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (exprReferencesLocal(program, branch.cond, local)) return true;
                if (exprReferencesLocal(program, branch.body, local)) return true;
            }
            return exprReferencesLocal(program, if_.final_else, local);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                const stmt = GuardedList.at(statements, index);
                if (stmtReferencesLocal(program, stmt, local)) return true;
            }
            return exprReferencesLocal(program, block.final_expr, local);
        },
        .loop_ => |loop| exprSpanReferencesLocal(program, loop.initial_values, local) or exprReferencesLocal(program, loop.body, local),
        .break_ => |maybe| if (maybe) |value| exprReferencesLocal(program, value, local) else false,
        .continue_ => |continue_| exprSpanReferencesLocal(program, continue_.values, local),
        .join_point => |join_point| exprReferencesLocal(program, join_point.body, local) or exprReferencesLocal(program, join_point.remainder, local),
        .jump => |jump| exprSpanReferencesLocal(program, jump.args, local),
        .if_initialized_payload => |payload_switch| exprReferencesLocal(program, payload_switch.cond, local) or
            exprReferencesLocal(program, payload_switch.initialized, local) or
            exprReferencesLocal(program, payload_switch.uninitialized, local),
        .try_sequence => |sequence| exprReferencesLocal(program, sequence.try_expr, local) or exprReferencesLocal(program, sequence.ok_body, local),
        .try_record_sequence => |sequence| exprReferencesLocal(program, sequence.try_expr, local) or exprReferencesLocal(program, sequence.ok_body, local),
    };
}

fn exprSpanReferencesLocal(program: *const Ast.Program, span: Ast.Span(Ast.ExprId), local: Ast.LocalId) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        const expr = GuardedList.at(exprs, index);
        if (exprReferencesLocal(program, expr, local)) return true;
    }
    return false;
}

fn captureOperandSpanReferencesLocal(program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand), local: Ast.LocalId) bool {
    const operands = program.captureOperandSpan(span);
    for (0..GuardedList.borrowLen(operands)) |index| {
        const operand = GuardedList.at(operands, index);
        if (exprReferencesLocal(program, operand.value, local)) return true;
    }
    return false;
}

fn stmtReferencesLocal(program: *const Ast.Program, stmt_id: Ast.StmtId, local: Ast.LocalId) bool {
    return switch (program.getStmt(stmt_id)) {
        .return_ => |ret| exprReferencesLocal(program, ret.value, local),
        .let_ => |let_| exprReferencesLocal(program, let_.value, local),
        .expr,
        .expect,
        .dbg,
        => |expr| exprReferencesLocal(program, expr, local),
        .uninitialized,
        .crash,
        => false,
    };
}

/// Reports whether moving `expr_id` beneath another loop would change the
/// target of a lexical `break` or `continue`. Monotype expression ownership is
/// acyclic, so this structural recursion always terminates. A loop owns control
/// transfers in its body, but not in its initial values.
fn exprContainsFreeLoopControl(program: *const Ast.Program, expr_id: Ast.ExprId, loop_depth: usize) bool {
    return switch (program.getExpr(expr_id).data) {
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
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        .lambda,
        .def_ref,
        .fn_def,
        => false,
        .fn_ref => |fn_ref| captureOperandSpanContainsFreeLoopControl(program, fn_ref.captures, loop_depth),
        .return_ => |ret| exprContainsFreeLoopControl(program, ret.value, loop_depth),
        .list,
        .tuple,
        => |items| exprSpanContainsFreeLoopControl(program, items, loop_depth),
        .record => |fields| {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (exprContainsFreeLoopControl(program, field.value, loop_depth)) return true;
            }
            return false;
        },
        .record_update => |update| {
            if (exprContainsFreeLoopControl(program, update.base, loop_depth)) return true;
            const field_exprs = program.fieldExprSpan(update.fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (exprContainsFreeLoopControl(program, field.value, loop_depth)) return true;
            }
            return false;
        },
        .tag => |tag| exprSpanContainsFreeLoopControl(program, tag.payloads, loop_depth),
        .static_data_candidate => |candidate| exprContainsFreeLoopControl(program, candidate.runtime_expr, loop_depth),
        .nominal,
        .dbg,
        .expect,
        => |child| exprContainsFreeLoopControl(program, child, loop_depth),
        .expect_err => |expect_err| exprContainsFreeLoopControl(program, expect_err.msg, loop_depth),
        .comptime_branch_taken => |taken| exprContainsFreeLoopControl(program, taken.body, loop_depth),
        .let_ => |let_| exprContainsFreeLoopControl(program, let_.value, loop_depth) or exprContainsFreeLoopControl(program, let_.rest, loop_depth),
        .call_value => |call| exprContainsFreeLoopControl(program, call.callee, loop_depth) or exprSpanContainsFreeLoopControl(program, call.args, loop_depth),
        .call_proc => |call| exprSpanContainsFreeLoopControl(program, call.args, loop_depth) or captureOperandSpanContainsFreeLoopControl(program, call.captures, loop_depth),
        .low_level => |call| exprSpanContainsFreeLoopControl(program, call.args, loop_depth),
        .field_access => |field| exprContainsFreeLoopControl(program, field.receiver, loop_depth),
        .tuple_access => |access| exprContainsFreeLoopControl(program, access.tuple, loop_depth),
        .structural_eq => |eq| exprContainsFreeLoopControl(program, eq.lhs, loop_depth) or exprContainsFreeLoopControl(program, eq.rhs, loop_depth),
        .structural_hash => |h| exprContainsFreeLoopControl(program, h.value, loop_depth) or exprContainsFreeLoopControl(program, h.hasher, loop_depth),
        .match_ => |match| {
            if (exprContainsFreeLoopControl(program, match.scrutinee, loop_depth)) return true;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (stmtContainsFreeLoopControl(program, GuardedList.at(bindings, binding_index), loop_depth)) return true;
                }
                if (branch.guard) |guard| {
                    if (exprContainsFreeLoopControl(program, guard, loop_depth)) return true;
                }
                if (exprContainsFreeLoopControl(program, branch.body, loop_depth)) return true;
            }
            return false;
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (exprContainsFreeLoopControl(program, branch.cond, loop_depth)) return true;
                if (exprContainsFreeLoopControl(program, branch.body, loop_depth)) return true;
            }
            return exprContainsFreeLoopControl(program, if_.final_else, loop_depth);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                const stmt = GuardedList.at(statements, index);
                if (stmtContainsFreeLoopControl(program, stmt, loop_depth)) return true;
            }
            return exprContainsFreeLoopControl(program, block.final_expr, loop_depth);
        },
        .loop_ => |loop| exprSpanContainsFreeLoopControl(program, loop.initial_values, loop_depth) or
            exprContainsFreeLoopControl(program, loop.body, loop_depth + 1),
        .break_ => |maybe| loop_depth == 0 or
            (if (maybe) |value| exprContainsFreeLoopControl(program, value, loop_depth) else false),
        .continue_ => |continue_| loop_depth == 0 or exprSpanContainsFreeLoopControl(program, continue_.values, loop_depth),
        .join_point => |join_point| exprContainsFreeLoopControl(program, join_point.body, loop_depth) or
            exprContainsFreeLoopControl(program, join_point.remainder, loop_depth),
        .jump => |jump| exprSpanContainsFreeLoopControl(program, jump.args, loop_depth),
        .if_initialized_payload => |payload_switch| exprContainsFreeLoopControl(program, payload_switch.cond, loop_depth) or
            exprContainsFreeLoopControl(program, payload_switch.initialized, loop_depth) or
            exprContainsFreeLoopControl(program, payload_switch.uninitialized, loop_depth),
        .try_sequence => |sequence| exprContainsFreeLoopControl(program, sequence.try_expr, loop_depth) or
            exprContainsFreeLoopControl(program, sequence.ok_body, loop_depth),
        .try_record_sequence => |sequence| exprContainsFreeLoopControl(program, sequence.try_expr, loop_depth) or
            exprContainsFreeLoopControl(program, sequence.ok_body, loop_depth),
    };
}

fn exprSpanContainsFreeLoopControl(program: *const Ast.Program, span: Ast.Span(Ast.ExprId), loop_depth: usize) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        if (exprContainsFreeLoopControl(program, GuardedList.at(exprs, index), loop_depth)) return true;
    }
    return false;
}

fn captureOperandSpanContainsFreeLoopControl(program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand), loop_depth: usize) bool {
    const operands = program.captureOperandSpan(span);
    for (0..GuardedList.borrowLen(operands)) |index| {
        if (exprContainsFreeLoopControl(program, GuardedList.at(operands, index).value, loop_depth)) return true;
    }
    return false;
}

fn stmtContainsFreeLoopControl(program: *const Ast.Program, stmt_id: Ast.StmtId, loop_depth: usize) bool {
    return switch (program.getStmt(stmt_id)) {
        .return_ => |ret| exprContainsFreeLoopControl(program, ret.value, loop_depth),
        .let_ => |let_| exprContainsFreeLoopControl(program, let_.value, loop_depth),
        .expr,
        .expect,
        .dbg,
        => |expr| exprContainsFreeLoopControl(program, expr, loop_depth),
        .uninitialized,
        .crash,
        => false,
    };
}

/// Record the tuple fields demanded from one aggregate local, rejecting any
/// use that observes the aggregate as a whole or through a non-tuple
/// access. `LocalId` is program-global, so binders introduced below this
/// expression cannot shadow the queried identity.
fn collectTupleLocalDemandInExpr(
    program: *const Ast.Program,
    local: Ast.LocalId,
    expr_id: Ast.ExprId,
    used: []bool,
) bool {
    return switch (program.getExpr(expr_id).data) {
        .local => |seen| seen != local,
        .@"unreachable",
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        .static_data_candidate,
        .lambda,
        .def_ref,
        .fn_def,
        => true,
        .fn_ref => |fn_ref| collectTupleLocalDemandInCaptureOperands(program, local, fn_ref.captures, used),
        .list,
        .tuple,
        => |items| collectTupleLocalDemandInExprSpan(program, local, items, used),
        .record => |fields| blk: {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                if (!collectTupleLocalDemandInExpr(program, local, GuardedList.at(field_exprs, index).value, used)) break :blk false;
            }
            break :blk true;
        },
        .record_update => |update| blk: {
            if (!collectTupleLocalDemandInExpr(program, local, update.base, used)) break :blk false;
            const field_exprs = program.fieldExprSpan(update.fields);
            for (0..field_exprs.len) |index| {
                if (!collectTupleLocalDemandInExpr(program, local, GuardedList.at(field_exprs, index).value, used)) break :blk false;
            }
            break :blk true;
        },
        .tag => |tag| collectTupleLocalDemandInExprSpan(program, local, tag.payloads, used),
        .nominal,
        .dbg,
        .expect,
        => |child| collectTupleLocalDemandInExpr(program, local, child, used),
        .return_ => |ret| collectTupleLocalDemandInExpr(program, local, ret.value, used),
        .expect_err => |expect_err| collectTupleLocalDemandInExpr(program, local, expect_err.msg, used),
        .comptime_branch_taken => |taken| collectTupleLocalDemandInExpr(program, local, taken.body, used),
        .let_ => |let_| collectTupleLocalDemandInExpr(program, local, let_.value, used) and
            collectTupleLocalDemandInExpr(program, local, let_.rest, used),
        .call_value => |call| collectTupleLocalDemandInExpr(program, local, call.callee, used) and
            collectTupleLocalDemandInExprSpan(program, local, call.args, used),
        .call_proc => |call| collectTupleLocalDemandInExprSpan(program, local, call.args, used) and
            collectTupleLocalDemandInCaptureOperands(program, local, call.captures, used),
        .low_level => |call| collectTupleLocalDemandInExprSpan(program, local, call.args, used),
        .field_access => |field| collectTupleLocalDemandInExpr(program, local, field.receiver, used),
        .tuple_access => |access| blk: {
            const receiver = program.getExpr(access.tuple);
            if (receiver.data == .local and receiver.data.local == local) {
                if (access.elem_index >= used.len) break :blk false;
                used[access.elem_index] = true;
                break :blk true;
            }
            break :blk collectTupleLocalDemandInExpr(program, local, access.tuple, used);
        },
        .structural_eq => |eq| collectTupleLocalDemandInExpr(program, local, eq.lhs, used) and
            collectTupleLocalDemandInExpr(program, local, eq.rhs, used),
        .structural_hash => |hash| collectTupleLocalDemandInExpr(program, local, hash.value, used) and
            collectTupleLocalDemandInExpr(program, local, hash.hasher, used),
        .match_ => |match| blk: {
            if (!collectTupleLocalDemandInExpr(program, local, match.scrutinee, used)) break :blk false;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    if (!collectTupleLocalDemandInStmt(program, local, GuardedList.at(bindings, binding_index), used)) break :blk false;
                }
                if (branch.guard) |guard| {
                    if (!collectTupleLocalDemandInExpr(program, local, guard, used)) break :blk false;
                }
                if (!collectTupleLocalDemandInExpr(program, local, branch.body, used)) break :blk false;
            }
            break :blk true;
        },
        .if_ => |if_| blk: {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (!collectTupleLocalDemandInExpr(program, local, branch.cond, used)) break :blk false;
                if (!collectTupleLocalDemandInExpr(program, local, branch.body, used)) break :blk false;
            }
            break :blk collectTupleLocalDemandInExpr(program, local, if_.final_else, used);
        },
        .block => |block| blk: {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                if (!collectTupleLocalDemandInStmt(program, local, GuardedList.at(statements, index), used)) break :blk false;
            }
            break :blk collectTupleLocalDemandInExpr(program, local, block.final_expr, used);
        },
        .loop_ => |loop| collectTupleLocalDemandInExprSpan(program, local, loop.initial_values, used) and
            collectTupleLocalDemandInExpr(program, local, loop.body, used),
        .break_ => |maybe| if (maybe) |value| collectTupleLocalDemandInExpr(program, local, value, used) else true,
        .continue_ => |continue_| collectTupleLocalDemandInExprSpan(program, local, continue_.values, used),
        .join_point => |join_point| blk: {
            var shadows = false;
            const params = program.typedLocalSpan(join_point.params);
            for (0..params.len) |index| {
                if (GuardedList.at(params, index).local == local) {
                    shadows = true;
                    break;
                }
            }
            if (!shadows and !collectTupleLocalDemandInExpr(program, local, join_point.body, used)) break :blk false;
            break :blk collectTupleLocalDemandInExpr(program, local, join_point.remainder, used);
        },
        .jump => |jump| collectTupleLocalDemandInExprSpan(program, local, jump.args, used),
        .if_initialized_payload => |payload| blk: {
            if (payload.payload == local) break :blk false;
            break :blk collectTupleLocalDemandInExpr(program, local, payload.cond, used) and
                collectTupleLocalDemandInExpr(program, local, payload.initialized, used) and
                collectTupleLocalDemandInExpr(program, local, payload.uninitialized, used);
        },
        .try_sequence => |sequence| collectTupleLocalDemandInExpr(program, local, sequence.try_expr, used) and
            (sequence.ok_local == local or collectTupleLocalDemandInExpr(program, local, sequence.ok_body, used)),
        .try_record_sequence => |sequence| collectTupleLocalDemandInExpr(program, local, sequence.try_expr, used) and
            (sequence.value_local == local or sequence.rest_local == local or
                collectTupleLocalDemandInExpr(program, local, sequence.ok_body, used)),
    };
}

fn collectTupleLocalDemandInExprSpan(
    program: *const Ast.Program,
    local: Ast.LocalId,
    span: Ast.Span(Ast.ExprId),
    used: []bool,
) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        if (!collectTupleLocalDemandInExpr(program, local, GuardedList.at(exprs, index), used)) return false;
    }
    return true;
}

fn collectTupleLocalDemandInCaptureOperands(
    program: *const Ast.Program,
    local: Ast.LocalId,
    span: Ast.Span(Ast.CaptureOperand),
    used: []bool,
) bool {
    const operands = program.captureOperandSpan(span);
    for (0..GuardedList.borrowLen(operands)) |index| {
        if (!collectTupleLocalDemandInExpr(program, local, GuardedList.at(operands, index).value, used)) return false;
    }
    return true;
}

fn collectTupleLocalDemandInStmt(
    program: *const Ast.Program,
    local: Ast.LocalId,
    stmt_id: Ast.StmtId,
    used: []bool,
) bool {
    return switch (program.getStmt(stmt_id)) {
        .let_ => |let_| collectTupleLocalDemandInExpr(program, local, let_.value, used),
        .expr,
        .expect,
        .dbg,
        => |expr| collectTupleLocalDemandInExpr(program, local, expr, used),
        .return_ => |ret| collectTupleLocalDemandInExpr(program, local, ret.value, used),
        .uninitialized,
        .crash,
        => true,
    };
}

fn localUseCountInExpr(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) usize {
    return switch (program.getExpr(expr_id).data) {
        .@"unreachable" => 0,
        .local => |seen| if (seen == local) 1 else 0,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        => 0,
        .fn_ref => |fn_ref| localUseCountInCaptureOperandSpan(program, local, fn_ref.captures),
        .list,
        .tuple,
        => |items| localUseCountInExprSpan(program, local, items),
        .record => |fields| blk: {
            var count: usize = 0;
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                count += localUseCountInExpr(program, local, field.value);
            }
            break :blk count;
        },
        .record_update => |update| blk: {
            var count = localUseCountInExpr(program, local, update.base);
            const field_exprs = program.fieldExprSpan(update.fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                count += localUseCountInExpr(program, local, field.value);
            }
            break :blk count;
        },
        .tag => |tag| localUseCountInExprSpan(program, local, tag.payloads),
        .static_data_candidate => |candidate| localUseCountInExpr(program, local, candidate.runtime_expr),
        .nominal,
        .dbg,
        .expect,
        => |child| localUseCountInExpr(program, local, child),
        .return_ => |ret| localUseCountInExpr(program, local, ret.value),
        .expect_err => |expect_err| localUseCountInExpr(program, local, expect_err.msg),
        .comptime_branch_taken => |taken| localUseCountInExpr(program, local, taken.body),
        .let_ => |let_| localUseCountInExpr(program, local, let_.value) + localUseCountInExpr(program, local, let_.rest),
        .lambda,
        .def_ref,
        .fn_def,
        => 0,
        .call_value => |call| localUseCountInExpr(program, local, call.callee) + localUseCountInExprSpan(program, local, call.args),
        .call_proc => |call| localUseCountInExprSpan(program, local, call.args) + localUseCountInCaptureOperandSpan(program, local, call.captures),
        .low_level => |call| localUseCountInExprSpan(program, local, call.args),
        .field_access => |field| localUseCountInExpr(program, local, field.receiver),
        .tuple_access => |access| localUseCountInExpr(program, local, access.tuple),
        .structural_eq => |eq| localUseCountInExpr(program, local, eq.lhs) + localUseCountInExpr(program, local, eq.rhs),
        .structural_hash => |h| localUseCountInExpr(program, local, h.value) + localUseCountInExpr(program, local, h.hasher),
        .match_ => |match| blk: {
            var count = localUseCountInExpr(program, local, match.scrutinee);
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                const bindings = program.stmtSpan(branch.bindings);
                for (0..bindings.len) |binding_index| {
                    count += localUseCountInStmt(program, local, GuardedList.at(bindings, binding_index));
                }
                if (branch.guard) |guard| count += localUseCountInExpr(program, local, guard);
                count += localUseCountInExpr(program, local, branch.body);
            }
            break :blk count;
        },
        .if_ => |if_| blk: {
            var count: usize = 0;
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                count += localUseCountInExpr(program, local, branch.cond);
                count += localUseCountInExpr(program, local, branch.body);
            }
            count += localUseCountInExpr(program, local, if_.final_else);
            break :blk count;
        },
        .block => |block| blk: {
            var count: usize = 0;
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                const stmt = GuardedList.at(statements, index);
                count += localUseCountInStmt(program, local, stmt);
            }
            count += localUseCountInExpr(program, local, block.final_expr);
            break :blk count;
        },
        .loop_ => |loop| localUseCountInExprSpan(program, local, loop.initial_values) + localUseCountInExpr(program, local, loop.body),
        .break_ => |maybe| if (maybe) |value| localUseCountInExpr(program, local, value) else 0,
        .continue_ => |continue_| localUseCountInExprSpan(program, local, continue_.values),
        .join_point => |join_point| blk: {
            var body_count = localUseCountInExpr(program, local, join_point.body);
            const params = program.typedLocalSpan(join_point.params);
            for (0..params.len) |index| {
                if (GuardedList.at(params, index).local == local) {
                    body_count = 0;
                    break;
                }
            }
            break :blk body_count + localUseCountInExpr(program, local, join_point.remainder);
        },
        .jump => |jump| localUseCountInExprSpan(program, local, jump.args),
        .if_initialized_payload => |payload_switch| localUseCountInExpr(program, local, payload_switch.cond) +
            (if (payload_switch.payload == local) @as(usize, 1) else 0) +
            localUseCountInExpr(program, local, payload_switch.initialized) +
            localUseCountInExpr(program, local, payload_switch.uninitialized),
        .try_sequence => |sequence| localUseCountInExpr(program, local, sequence.try_expr) +
            if (sequence.ok_local == local) 0 else localUseCountInExpr(program, local, sequence.ok_body),
        .try_record_sequence => |sequence| localUseCountInExpr(program, local, sequence.try_expr) +
            if (sequence.value_local == local or sequence.rest_local == local) 0 else localUseCountInExpr(program, local, sequence.ok_body),
    };
}

fn localUseCountInExprSpan(program: *const Ast.Program, local: Ast.LocalId, span: Ast.Span(Ast.ExprId)) usize {
    var count: usize = 0;
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        const expr = GuardedList.at(exprs, index);
        count += localUseCountInExpr(program, local, expr);
    }
    return count;
}

fn localUseCountInCaptureOperandSpan(program: *const Ast.Program, local: Ast.LocalId, span: Ast.Span(Ast.CaptureOperand)) usize {
    var count: usize = 0;
    const operands = program.captureOperandSpan(span);
    for (0..GuardedList.borrowLen(operands)) |index| {
        const operand = GuardedList.at(operands, index);
        count += localUseCountInExpr(program, local, operand.value);
    }
    return count;
}

fn localUseCountInStmt(program: *const Ast.Program, local: Ast.LocalId, stmt_id: Ast.StmtId) usize {
    return switch (program.getStmt(stmt_id)) {
        .uninitialized => 0,
        .let_ => |let_| localUseCountInExpr(program, local, let_.value),
        .expr,
        .expect,
        .dbg,
        => |expr| localUseCountInExpr(program, local, expr),
        .return_ => |ret| localUseCountInExpr(program, local, ret.value),
        .crash => 0,
    };
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
        .static_data_candidate => |candidate| candidate.ty,
        .tag => |tag| tag.ty,
        .record => |record| record.ty,
        .tuple => |tuple| tuple.ty,
        .nominal => |nominal| nominal.ty,
        .callable => |callable| callable.ty,
    };
}

/// Whether two Monotype ids denote the same type. The type store is not
/// interned: each specialization materializes its own ids, so structurally
/// identical types reached from different specializations (a call site and
/// the callee's own body) carry different ids and compare by digest.
fn sameType(program: *const Ast.Program, lhs: Type.TypeId, rhs: Type.TypeId) bool {
    if (lhs == rhs) return true;
    const lhs_digest = program.types.typeDigest(&program.names, lhs);
    const rhs_digest = program.types.typeDigest(&program.names, rhs);
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

fn patternEql(program: *const Ast.Program, lhs: CallPattern, rhs: CallPattern) bool {
    if (lhs.args.len != rhs.args.len) return false;
    for (lhs.args, rhs.args) |lhs_arg, rhs_arg| {
        if (!shapeEql(program, lhs_arg, rhs_arg)) return false;
    }
    return true;
}

fn shapeEql(program: *const Ast.Program, lhs: Shape, rhs: Shape) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .any => |lhs_ty| sameType(program, lhs_ty, rhs.any),
        .tag => |lhs_tag| blk: {
            const rhs_tag = rhs.tag;
            if (!sameType(program, lhs_tag.ty, rhs_tag.ty) or
                !program.names.tagLabelTextEql(lhs_tag.name, rhs_tag.name) or
                lhs_tag.payloads.len != rhs_tag.payloads.len)
            {
                break :blk false;
            }
            for (lhs_tag.payloads, rhs_tag.payloads) |lhs_payload, rhs_payload| {
                if (!shapeEql(program, lhs_payload, rhs_payload)) break :blk false;
            }
            break :blk true;
        },
        .record => |lhs_record| blk: {
            const rhs_record = rhs.record;
            if (!sameType(program, lhs_record.ty, rhs_record.ty) or lhs_record.fields.len != rhs_record.fields.len) break :blk false;
            for (lhs_record.fields, rhs_record.fields) |lhs_field, rhs_field| {
                if (!program.names.recordFieldLabelTextEql(lhs_field.name, rhs_field.name) or
                    !shapeEql(program, lhs_field.shape, rhs_field.shape))
                {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .tuple => |lhs_tuple| blk: {
            const rhs_tuple = rhs.tuple;
            if (!sameType(program, lhs_tuple.ty, rhs_tuple.ty) or lhs_tuple.items.len != rhs_tuple.items.len) break :blk false;
            for (lhs_tuple.items, rhs_tuple.items) |lhs_item, rhs_item| {
                if (!shapeEql(program, lhs_item, rhs_item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |lhs_nominal| {
            const rhs_nominal = rhs.nominal;
            return sameType(program, lhs_nominal.ty, rhs_nominal.ty) and shapeEql(program, lhs_nominal.backing.*, rhs_nominal.backing.*);
        },
        .callable => |lhs_callable| blk: {
            const rhs_callable = rhs.callable;
            if (!sameType(program, lhs_callable.ty, rhs_callable.ty) or
                !callableTargetMatches(program, lhs_callable.fn_id, rhs_callable.fn_id) or
                lhs_callable.captures.len != rhs_callable.captures.len)
            {
                break :blk false;
            }
            for (lhs_callable.captures, rhs_callable.captures) |lhs_capture, rhs_capture| {
                if (!shapeEql(program, lhs_capture, rhs_capture)) break :blk false;
            }
            break :blk true;
        },
    };
}

/// Whether one specialization's call pattern accepts a call's argument values.
/// This reads the values the caller already cloned and takes no `Cloner`, so
/// deciding a specialization cannot clone a source argument a second time and a
/// rejected specialization costs nothing and leaves nothing behind.
fn callPatternMatchesValues(program: *const Ast.Program, pattern: CallPattern, values: []const Value) bool {
    if (pattern.args.len != values.len) Common.invariant("call-pattern arity differed from direct call arity");
    for (pattern.args, values) |shape, value| {
        if (!shapeMatchesValue(program, shape, value)) return false;
    }
    return true;
}

fn shapeMatchesValue(program: *const Ast.Program, shape: Shape, value: Value) bool {
    const structural_value = if (value == .static_data_candidate) value.static_data_candidate.runtime.* else value;
    return switch (shape) {
        .any => true,
        .tag => |tag| blk: {
            if (structural_value != .tag) break :blk false;
            const value_tag = structural_value.tag;
            if (!sameType(program, tag.ty, value_tag.ty) or
                !program.names.tagLabelTextEql(tag.name, value_tag.name) or
                tag.payloads.len != value_tag.payloads.len)
            {
                break :blk false;
            }
            for (tag.payloads, value_tag.payloads) |payload_shape, payload_value| {
                if (!shapeMatchesValue(program, payload_shape, payload_value)) break :blk false;
            }
            break :blk true;
        },
        .record => |record| blk: {
            if (structural_value != .record) break :blk false;
            const value_record = structural_value.record;
            if (!sameType(program, record.ty, value_record.ty) or record.fields.len != value_record.fields.len) break :blk false;
            for (record.fields, value_record.fields) |field_shape, field_value| {
                if (!program.names.recordFieldLabelTextEql(field_shape.name, field_value.name) or
                    !shapeMatchesValue(program, field_shape.shape, field_value.value))
                {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .tuple => |tuple| blk: {
            if (structural_value != .tuple) break :blk false;
            const value_tuple = structural_value.tuple;
            if (!sameType(program, tuple.ty, value_tuple.ty) or tuple.items.len != value_tuple.items.len) break :blk false;
            for (tuple.items, value_tuple.items) |item_shape, item_value| {
                if (!shapeMatchesValue(program, item_shape, item_value)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |nominal| blk: {
            if (structural_value != .nominal) break :blk false;
            const value_nominal = structural_value.nominal;
            break :blk sameType(program, nominal.ty, value_nominal.ty) and shapeMatchesValue(program, nominal.backing.*, value_nominal.backing.*);
        },
        .callable => |callable| blk: {
            if (structural_value != .callable) break :blk false;
            const value_callable = structural_value.callable;
            if (!sameType(program, callable.ty, value_callable.ty) or
                !callableTargetMatches(program, callable.fn_id, value_callable.fn_id) or
                callable.captures.len != value_callable.captures.len)
            {
                break :blk false;
            }
            for (callable.captures, value_callable.captures) |capture_shape, capture_value| {
                if (!shapeMatchesValue(program, capture_shape, capture_value.value)) break :blk false;
            }
            break :blk true;
        },
    };
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
// `nominal.backing`/`static_data_candidate.runtime` pointer edges would loop,
// so each reader counts the edges it follows and treats reaching
// `value_wrapper_strip_cap` as a compiler bug.
fn fieldFromValue(program: *const Ast.Program, value: Value, name: names.RecordFieldNameId) ?Value {
    const field = fieldFromValueStripping(program, value, name, 0) orelse return null;
    if (!isGeneratedIteratorStepField(program, valueType(program, value), name)) return field;
    return switch (field) {
        .callable => |callable| blk: {
            var step = callable;
            step.iterator_step = true;
            break :blk .{ .callable = step };
        },
        .expr, .static_data_candidate, .tag, .record, .tuple, .nominal => field,
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

fn fieldFromValueStripping(program: *const Ast.Program, value: Value, name: names.RecordFieldNameId, strip_depth: usize) ?Value {
    if (strip_depth >= value_wrapper_strip_cap) Common.invariant("fieldFromValue followed a value wrapper chain past the strip cap");
    return switch (value) {
        .static_data_candidate => |candidate| fieldFromValueStripping(program, candidate.runtime.*, name, strip_depth + 1),
        .record => |record| fieldFromRecord(program, record, name),
        .nominal => |nominal| fieldFromValueStripping(program, nominal.backing.*, name, strip_depth + 1),
        .expr, .tag, .tuple, .callable => null,
    };
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

fn itemFromValueStripping(value: Value, index: u32, strip_depth: usize) ?Value {
    if (strip_depth >= value_wrapper_strip_cap) Common.invariant("itemFromValue followed a value wrapper chain past the strip cap");
    return switch (value) {
        .static_data_candidate => |candidate| itemFromValueStripping(candidate.runtime.*, index, strip_depth + 1),
        .tuple => |tuple| if (index < tuple.items.len) tuple.items[index] else null,
        .nominal => |nominal| itemFromValueStripping(nominal.backing.*, index, strip_depth + 1),
        .expr, .tag, .record, .callable => null,
    };
}

fn tagFromValue(value: Value) ?TagValue {
    return tagFromValueStripping(value, 0);
}

fn tagFromValueStripping(value: Value, strip_depth: usize) ?TagValue {
    if (strip_depth >= value_wrapper_strip_cap) Common.invariant("tagFromValue followed a value wrapper chain past the strip cap");
    return switch (value) {
        .static_data_candidate => |candidate| tagFromValueStripping(candidate.runtime.*, strip_depth + 1),
        .tag => |tag| tag,
        .nominal => |nominal| tagFromValueStripping(nominal.backing.*, strip_depth + 1),
        .expr, .record, .tuple, .callable => null,
    };
}

fn recordFromValue(value: Value) ?RecordValue {
    return recordFromValueStripping(value, 0);
}

fn recordFromValueStripping(value: Value, strip_depth: usize) ?RecordValue {
    if (strip_depth >= value_wrapper_strip_cap) Common.invariant("recordFromValue followed a value wrapper chain past the strip cap");
    return switch (value) {
        .static_data_candidate => |candidate| recordFromValueStripping(candidate.runtime.*, strip_depth + 1),
        .record => |record| record,
        .nominal => |nominal| recordFromValueStripping(nominal.backing.*, strip_depth + 1),
        .expr, .tag, .tuple, .callable => null,
    };
}

fn tupleFromValue(value: Value) ?TupleValue {
    return tupleFromValueStripping(value, 0);
}

fn tupleFromValueStripping(value: Value, strip_depth: usize) ?TupleValue {
    if (strip_depth >= value_wrapper_strip_cap) Common.invariant("tupleFromValue followed a value wrapper chain past the strip cap");
    return switch (value) {
        .static_data_candidate => |candidate| tupleFromValueStripping(candidate.runtime.*, strip_depth + 1),
        .tuple => |tuple| tuple,
        .nominal => |nominal| tupleFromValueStripping(nominal.backing.*, strip_depth + 1),
        .expr, .tag, .record, .callable => null,
    };
}

fn emptyLiftedProgramForTest(allocator: Allocator) Ast.Program {
    return Ast.Program.init(
        allocator,
        names.NameStore.init(allocator),
        Type.Store.init(allocator),
        .empty, // imported_fns
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
    const read_binding = base_binding.next orelse return error.TestUnexpectedResult;
    try std.testing.expect(read_binding.next == null);
    const read = blk_read: {
        const scrutinee = program.getExpr(read_binding.binding.value).data;
        if (scrutinee != .field_access) return error.TestUnexpectedResult;
        break :blk_read scrutinee.field_access;
    };
    const read_segments = program.fieldAccessSegmentSpan(read.segments);
    try std.testing.expectEqual(@as(usize, 1), GuardedList.borrowLen(read_segments));
    try std.testing.expectEqual(a, GuardedList.at(read_segments, 0).field);
    try std.testing.expectEqual(base_binding.binding.local, program.getExpr(read.receiver).data.local);

    try std.testing.expectEqual(a, record.fields[0].name);
    try std.testing.expectEqual(read_binding.binding.local, program.getExpr(record.fields[0].value.expr).data.local);
    try std.testing.expectEqual(b, record.fields[1].name);
    try std.testing.expectEqual(update_local, program.getExpr(record.fields[1].value.expr).data.local);
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

    try std.testing.expect(exprContainsReturn(&program, fn_ref));
    try std.testing.expectEqual(@as(usize, 1), localUseCountInExpr(&program, local, fn_ref));
    try std.testing.expect(exprContainsReturn(&program, call_proc));
    try std.testing.expectEqual(@as(usize, 1), localUseCountInExpr(&program, local, call_proc));
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

    switch (exprBodySizeWithin(&program, one_let, 3)) {
        .exact => |count| try std.testing.expectEqual(@as(usize, 3), count),
        .over_limit => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(BodySize.over_limit, exprBodySizeWithin(&program, one_let, 2));

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
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = unit_expr },
        .ret = unit_ty,
    });
    const large_fn_id = try program.addFn(.{
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
    try std.testing.expectEqual(SpecAdmission.denied_body_size, pass.inlineBodyAdmission(@enumFromInt(1), large_body));

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
    pass.refreshPreCloneBodySize(1);
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
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = tuple_expr },
        .ret = tuple_ty,
    });

    const consumer_arg = try program.addLocal(@enumFromInt(2), tuple_ty);
    const consumer = try program.addFn(.{
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
    try std.testing.expect(exprContainsReturn(&program, hash_expr));
    try std.testing.expectEqual(@as(usize, 1), localUseCountInExpr(&program, value_local, hash_expr));
    try std.testing.expectEqual(@as(usize, 1), localUseCountInExpr(&program, hasher_local, hash_expr));
}

test "call-pattern specialization preserves imported direct calls" {
    const allocator = std.testing.allocator;
    var mono = Mono.Program.init(allocator);
    errdefer mono.deinit();

    const unit_ty = try mono.types.add(.zst);
    const imported = try mono.addImportedFn(.{
        .shard = @enumFromInt(1),
        .fn_id = @enumFromInt(1),
    });
    const body = try mono.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = Mono.importedProcCallee(imported),
        .args = Mono.Span(Mono.ExprId).empty(),
    } } });
    try mono.defs.append(allocator, .{
        .symbol = @enumFromInt(1),
        .args = Mono.Span(Mono.TypedLocal).empty(),
        .body = .{ .roc = body },
        .ret = unit_ty,
    });

    var lifted = try @import("lift.zig").run(allocator, mono);
    defer lifted.deinit();

    try run(allocator, &lifted);

    const body_data = lifted.getExpr(body).data;
    if (body_data != .call_proc) return error.TestUnexpectedResult;
    const call = body_data.call_proc;
    switch (call.callee) {
        .func => |slot| switch (slot) {
            .imported => |actual| try std.testing.expectEqual(imported, actual),
            .local => return error.TestUnexpectedResult,
        },
        .lifted => return error.TestUnexpectedResult,
    }
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

    // A static-data-candidate value whose runtime edge points back at itself:
    // the fixpoint shape a recursively-constructed value takes when a `.local`
    // resolves through the substitution maps to an ancestor of its own
    // construction. Stripping the wrapper never reaches a constructor.
    var cyclic: Value = undefined;
    cyclic = .{
        .static_data_candidate = .{
            .ty = union_ty,
            // Never read: every walk this test exercises follows the runtime edge
            // and declines before any materialization would consume the id.
            .static_data = undefined,
            .runtime = &cyclic,
        },
    };

    // The substitution check answers "cannot substitute" on exhaustion—the
    // conservative direction, and correct, since a self-referential value
    // cannot be substituted.
    try std.testing.expectEqual(ProofStatus.unknown_budget_exhausted, cloner.valueCanSubstitute(cyclic));

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
        .symbol = @enumFromInt(3),
        .args = try program.addTypedLocalSpan(&.{.{ .local = argument, .ty = ty }}),
        .captures = Ast.Span(Ast.TypedLocal).empty(),
        .body = .{ .roc = equivalent_ref },
        .ret = ty,
    });

    var pass = try Pass.init(allocator, &program);
    defer pass.deinit();
    try pass.cloneFnBodyInPlace(fn_id, equivalent_ref);

    const cloned_body = switch (program.getFn(fn_id).body) {
        .roc => |body| body,
        .hosted => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(argument, program.getExpr(cloned_body).data.local);
}

test "known match fold aborts on undecidable branches and trips the invariant when every branch is excluded" {
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

    // Every branch a definite no-match violates checker exhaustiveness. In
    // Debug, the invariant panics; probe that abort from a fork on POSIX.
    if (comptime @import("builtin").mode == .Debug and @import("builtin").os.tag != .windows) {
        const excluded_branches = try program.addBranchSpan(&.{
            .{ .pat = bar_pat, .body = body },
        });
        const pid = std.c.fork();
        try std.testing.expect(pid >= 0);
        if (pid == 0) {
            const dev_null = std.c.open("/dev/null", .{ .ACCMODE = .WRONLY });
            if (dev_null >= 0) {
                _ = std.c.dup2(dev_null, 2);
                _ = std.c.close(dev_null);
            }
            _ = cloner.simplifyKnownMatchValue(foo_value, excluded_branches, &bindings) catch std.c._exit(2);
            // Reaching this line means the invariant did not fire.
            std.c._exit(0);
        }
        var status: c_int = 0;
        _ = std.c.waitpid(pid, &status, 0);
        const raw_status: u32 = @bitCast(status);
        const failed = std.posix.W.IFSIGNALED(raw_status) or
            (std.posix.W.IFEXITED(raw_status) and std.posix.W.EXITSTATUS(raw_status) != 0);
        try std.testing.expect(failed);
    }
}

test "call-pattern specialization declarations are referenced" {
    std.testing.refAllDecls(@This());
}
