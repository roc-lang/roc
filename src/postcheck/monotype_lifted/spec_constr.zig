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
/// branch verdict must abort a match fold — the residual match stays in the
/// output and decides at runtime — whereas `no_match` proves the branch can
/// be skipped.
const MatchVerdict = enum { match, no_match, unknown };

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
};

const CallPattern = struct {
    args: []const Shape,
};

const Spec = struct {
    pattern: CallPattern,
    fn_id: ?Ast.FnId = null,
    written: bool = false,
};

const FnPlan = struct {
    used_args: []bool,
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
};

const BindingChange = struct {
    key: BindingTarget,
    previous: ?Value,
};

const PendingLet = struct {
    local: Ast.LocalId,
    ty: Type.TypeId,
    value: Ast.ExprId,
    /// The cloner's effect-mark count when this binding was created. A
    /// binding created after an effect was emitted in its region must not
    /// move to the region's start, because it would cross that effect.
    marks: usize,
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

const ActiveJoinClone = struct {
    source: Ast.JoinPointId,
    target: Ast.JoinPointId,
};

/// One jump into a let-of-case join: the placeholder jump expression emitted
/// at the site (its argument span is patched once the join's parameters are
/// decided) and the symbolic value the site supplies for each binder slot.
const LetCaseJumpSite = struct {
    expr: Ast.ExprId,
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

const Pass = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    program: *Ast.Program,
    plans: []FnPlan,
    symbols: Common.SymbolGen,
    /// Per source function: whether its body performs no observable effect.
    /// Calls to such functions are pure computations; only calls to the
    /// rest carry effects. Functions created during specialization are past
    /// the end of this table and count as effectful.
    fn_effect_free: []bool,
    /// Per source function: whether an explicit Roc crash is reachable through
    /// its body or a local direct call. Such calls retain their procedure
    /// boundary so optimized debug backtraces preserve source frames.
    fn_may_crash: []bool,
    /// Per source function: whether the branch-append peel rewrote its body
    /// before specialization. A peeled body iterates the shared base directly,
    /// so it no longer reads as branch-chosen, but its base loop still needs the
    /// whole-body scalarizing clone.
    peeled: []bool,
    /// Per source function: whether the whole-body value clone has already
    /// satisfied value-aware call rewriting, shape demand, and known-loop
    /// scalarization. Those analyses can all request the same clone, but the
    /// clone is one normalization pass and must run at most once per body.
    whole_body_cloned: []bool,
    /// Functions containing a field read, tuple-item read, match scrutinee, or
    /// inspected argument that demands the structural result of a local call.
    shape_demand_fns: []bool,
    /// Functions that directly call themselves. Let-substitution-aware call
    /// patterns are always relevant at these recursive worker boundaries.
    self_recursive_fns: []bool,
    /// One rewritten callable body per source lifted function. Capture values
    /// are explicit operands and therefore do not create new body identities.
    callable_workers: std.AutoHashMap(Ast.FnId, Ast.FnId),
    /// Reverse index from each rewritten callable body to its source function.
    /// This keeps later materialization rooted at the source instead of cloning
    /// an already-rewritten worker.
    callable_sources: std.AutoHashMap(Ast.FnId, Ast.FnId),
    next_join_point: u32,
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
                .specs = .empty,
            };
        }

        const fn_effect_free = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(fn_effect_free);
        for (0..program.fnCount()) |index| {
            const fn_ = program.getFnAt(index);
            fn_effect_free[index] = fn_.body == .roc;
        }
        var changed = true;
        while (changed) {
            changed = false;
            for (0..program.fnCount()) |index| {
                const fn_ = program.getFnAt(index);
                if (!fn_effect_free[index]) continue;
                const body = switch (fn_.body) {
                    .roc => |body| body,
                    .hosted => continue,
                };
                if (!exprHasNoObservableEffect(program, fn_effect_free, body, true)) {
                    fn_effect_free[index] = false;
                    changed = true;
                }
            }
        }

        const fn_may_crash = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(fn_may_crash);
        @memset(fn_may_crash, false);
        changed = true;
        while (changed) {
            changed = false;
            for (0..program.fnCount()) |index| {
                if (fn_may_crash[index]) continue;
                const body = switch (program.getFnAt(index).body) {
                    .roc => |body| body,
                    .hosted => continue,
                };
                if (exprMayCrash(program, fn_may_crash, body)) {
                    fn_may_crash[index] = true;
                    changed = true;
                }
            }
        }

        const peeled = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(peeled);
        @memset(peeled, false);

        const whole_body_cloned = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(whole_body_cloned);
        @memset(whole_body_cloned, false);

        const shape_demand_fns = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(shape_demand_fns);
        @memset(shape_demand_fns, false);

        const self_recursive_fns = try allocator.alloc(bool, program.fnCount());
        errdefer allocator.free(self_recursive_fns);
        for (0..program.fnCount()) |index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            self_recursive_fns[index] = switch (program.getFnAt(index).body) {
                .roc => |body| exprCallsFn(program, body, fn_id),
                .hosted => false,
            };
        }

        return .{
            .allocator = allocator,
            .arena = arena,
            .program = program,
            .plans = plans,
            .symbols = .{ .next = program.next_symbol },
            .fn_effect_free = fn_effect_free,
            .fn_may_crash = fn_may_crash,
            .peeled = peeled,
            .whole_body_cloned = whole_body_cloned,
            .shape_demand_fns = shape_demand_fns,
            .self_recursive_fns = self_recursive_fns,
            .callable_workers = std.AutoHashMap(Ast.FnId, Ast.FnId).init(allocator),
            .callable_sources = std.AutoHashMap(Ast.FnId, Ast.FnId).init(allocator),
            .next_join_point = 0,
        };
    }

    fn freshJoinPoint(self: *Pass) Ast.JoinPointId {
        const id: Ast.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn deinit(self: *Pass) void {
        self.callable_sources.deinit();
        self.callable_workers.deinit();
        self.allocator.free(self.self_recursive_fns);
        self.allocator.free(self.shape_demand_fns);
        self.allocator.free(self.fn_may_crash);
        self.allocator.free(self.fn_effect_free);
        self.allocator.free(self.peeled);
        self.allocator.free(self.whole_body_cloned);
        for (self.plans) |*plan| plan.deinit(self.allocator);
        self.allocator.free(self.plans);
        self.arena.deinit();
    }

    fn run(self: *Pass) Common.LowerError!void {
        const original_fn_count = self.plans.len;

        // The append peel runs first, while an append chain's arms are still
        // separate `append` calls over their shared base. Specialization would
        // otherwise collapse a multi-append arm into one specialized adapter
        // worker the peel could not unwrap.
        try self.peelBranchAppendLoops(original_fn_count);
        try self.collectArgUses(original_fn_count);
        try self.collectCallPatterns(original_fn_count);
        try self.collectValueAwareCallPatterns(original_fn_count);
        try self.reserveSpecIds();
        try self.createSpecializations(original_fn_count);
        try self.rewriteExistingCalls();
        try self.rewriteValueAwareCalls();
        try self.rewriteShapeDemandBodies(original_fn_count);
        try self.scalarizeKnownLoops(original_fn_count);
        try self.createSpecializations(original_fn_count);
        try Lift.recomputeCaptures(self.allocator, self.program);

        self.program.next_symbol = self.symbols.next;
    }

    /// Rewrite each branch-chosen `append`-loop function into a base loop plus a
    /// branch-dispatched tail, before specialization can collapse its arms.
    /// Records which functions were rewritten so their base loops still get the
    /// whole-body scalarizing clone later.
    fn peelBranchAppendLoops(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        for (0..original_fn_count) |index| {
            const fn_ = self.program.getFnAt(index);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            if (!try self.bodyHasBranchChosenIterLoop(body)) continue;
            if (try self.peelBranchAppendBody(body)) |peeled| {
                self.program.setFnAt(index, .{
                    .symbol = fn_.symbol,
                    .source = fn_.source,
                    .args = fn_.args,
                    .captures = fn_.captures,
                    .body = .{ .roc = peeled },
                    .ret = fn_.ret,
                });
                self.peeled[index] = true;
            }
        }
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
            cloner.allow_nonrecursive_value_patterns = self.shape_demand_fns[index];
            defer cloner.deinit();
            try cloner.collectCallPatternsInExpr(fn_id, body);
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
                            self.markShapeDemandIfDirect(fn_id, arg);
                        }
                    }
                }
            },
            .low_level => |call| {
                const args = self.program.exprSpan(call.args);
                for (0..args.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(args, index), changed);
            },
            .field_access => |field| {
                self.markShapeDemandIfDirect(fn_id, field.receiver);
                self.markArgUseIfLocal(fn_id, field.receiver, changed);
                try self.markArgUsesInExpr(fn_id, field.receiver, changed);
            },
            .tuple_access => |access| {
                self.markShapeDemandIfDirect(fn_id, access.tuple);
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
                self.markShapeDemandIfDirect(fn_id, match.scrutinee);
                self.markArgUseIfLocal(fn_id, match.scrutinee, changed);
                try self.markArgUsesInExpr(fn_id, match.scrutinee, changed);
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
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
                for (0..initial_values.len) |index| try self.markArgUsesInExpr(fn_id, GuardedList.at(initial_values, index), changed);
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

    fn markShapeDemandIfDirect(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId) void {
        if (self.transparentLocalDirectCall(expr_id) != null) {
            self.shape_demand_fns[@intFromEnum(fn_id)] = true;
        }
    }

    fn transparentLocalDirectCall(self: *Pass, expr_id: Ast.ExprId) ?Ast.FnId {
        return switch (self.program.getExpr(expr_id).data) {
            .call_proc => |call| Ast.localDirectCallee(call),
            .block => |block| if (self.program.stmtSpan(block.statements).len == 0)
                self.transparentLocalDirectCall(block.final_expr)
            else
                null,
            .comptime_branch_taken => |taken| self.transparentLocalDirectCall(taken.body),
            else => null,
        };
    }

    fn collectCallPatternsInExpr(self: *Pass, owner: Ast.FnId, expr_id: Ast.ExprId) Allocator.Error!void {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
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

    fn recordCallPattern(self: *Pass, fn_id: Ast.FnId, args_span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
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

        const fn_args = self.program.typedLocalSpan(self.program.getFnAt(raw).args);
        if (values.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const shapes = try self.arena.allocator().alloc(Shape, values.len);
        var has_constructor = false;

        for (values, 0..) |value, index| {
            if (self.plans[raw].used_args[index]) {
                if (try self.shapeFromValue(value)) |shape| {
                    shapes[index] = shape;
                    has_constructor = true;
                    continue;
                }
            }
            shapes[index] = .{ .any = valueType(self.program, value) };
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

    fn ensureCallPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return;

        const fn_args = self.program.typedLocalSpan(self.program.getFnAt(raw).args);
        if (values.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const shapes = try self.arena.allocator().alloc(Shape, values.len);
        var has_constructor = false;
        for (values, 0..) |value, index| {
            if (self.plans[raw].used_args[index]) {
                if (try self.shapeFromValue(value)) |shape| {
                    shapes[index] = shape;
                    has_constructor = true;
                    continue;
                }
            }
            shapes[index] = .{ .any = valueType(self.program, value) };
        }
        if (!has_constructor) return;

        const pattern: CallPattern = .{ .args = shapes };
        for (self.plans[raw].specs.items) |spec| {
            if (patternEql(self.program, spec.pattern, pattern)) return;
        }

        const source_fn = self.program.getFnAt(raw);
        const symbol = self.symbols.fresh();
        const fn_id_reserved = try self.program.addFn(.{
            .symbol = symbol,
            .source = source_fn.source,
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

    /// Detect call sites that only match a worker after `let` substitutions are
    /// visible, then clone the whole body through the value pass. Cloning the
    /// body dissolves the now-split construction bindings instead of leaving
    /// their strict runtime construction behind.
    fn rewriteValueAwareCalls(self: *Pass) Common.LowerError!void {
        const fn_count = self.program.fnCount();
        for (0..fn_count) |index| {
            const fn_ = self.program.getFnAt(index);
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            var cloner = Cloner.initForRewrite(self);
            cloner.value_aware_detect_only = true;
            cloner.emit_callable_workers = false;
            cloner.allow_nonrecursive_value_patterns = index < self.shape_demand_fns.len and self.shape_demand_fns[index];
            defer cloner.deinit();
            try cloner.rewriteCallsWithValuesInExpr(body);
            if (cloner.value_aware_rewrite_changed) {
                try self.cloneFnBodyInPlace(fn_id, body);
            }
        }
    }

    fn rewriteShapeDemandBodies(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        for (0..original_fn_count) |index| {
            if (!self.shape_demand_fns[index]) continue;
            const body = switch (self.program.getFnAt(index).body) {
                .roc => |body| body,
                .hosted => continue,
            };
            try self.cloneFnBodyInPlace(@enumFromInt(@as(u32, @intCast(index))), body);
        }
    }

    /// A `for` over a statically known source lowers to a loop carried directly
    /// in its enclosing function; it never becomes a call-pattern worker, so
    /// nothing else scalarizes it. Two source shapes are handled here.
    ///
    /// A loop over an owned construction (`for x in [1, 2, 3].iter()`) has its
    /// loop expression cloned through the value pass in place, so its
    /// loop-carried iterator construction inlines and its state splits into
    /// scalars — the same transformation a specialized collect worker receives.
    ///
    /// A loop over an iterator named by an enclosing `if`/`match` binding (`for x
    /// in collision_points`, where `collision_points` chose between `base` and
    /// `base.append(..)`) has the whole enclosing body cloned instead, so the
    /// value pass sinks the loop into each branch — where that branch's iterator
    /// constructor is known — and scalarizes each sunk loop over its own source.
    ///
    /// Only original function bodies are scanned. A specialized worker's loop
    /// already passed through loop-state cloning while the worker was written.
    fn scalarizeKnownLoops(self: *Pass, original_fn_count: usize) Common.LowerError!void {
        for (0..original_fn_count) |index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            const body = switch (self.program.getFnAt(index).body) {
                .roc => |body| body,
                .hosted => continue,
            };

            // A `for` over a branch-chosen or `append`-style iterator reads its
            // source through a local the enclosing scope bound to an `if`/`match`.
            // Cloning only the loop leaves that construction a residual value; the
            // whole body must clone so the value pass sinks the loop into each
            // branch (where the branch's constructor is known) and scalarizes each
            // sunk loop over its own source. Cloning the whole body also carries
            // any owned-construction loop it holds, so those are not collected
            // separately for it.
            // A peeled body iterates the shared base directly; scalarize its base
            // loop with the same whole-body clone the branch-chosen shape uses.
            if (self.peeled[index] or
                try self.bodyHasBranchChosenIterLoop(body) or
                try self.bodyHasLocalConstructionLoop(body))
            {
                try self.cloneFnBodyInPlace(fn_id, body);
                continue;
            }

            var loops = std.ArrayList(Ast.ExprId).empty;
            defer loops.deinit(self.allocator);
            try self.collectKnownLoops(body, &loops);
            for (loops.items) |loop_id| try self.cloneLoopInPlace(loop_id);
        }
    }

    /// Whether a function body holds a `for` loop over an iterator named by an
    /// enclosing `if`/`match` binding — the branch-chosen (tier-two) shape. The
    /// loop's first carried value is an identity-style construction over a single
    /// local, and that local is bound in scope to a branch expression whose arms
    /// are the differently-shaped iterators the loop must specialize over.
    fn bodyHasBranchChosenIterLoop(self: *Pass, body: Ast.ExprId) Allocator.Error!bool {
        var branch_bound = std.AutoHashMap(Ast.LocalId, void).init(self.allocator);
        defer branch_bound.deinit();
        try self.collectBranchBoundLocals(body, &branch_bound);
        if (branch_bound.count() == 0) return false;
        return self.loopConsumesBranchBoundLocal(body, &branch_bound);
    }

    /// Whether a function body holds a loop over a local whose value is bound
    /// in the same body to a structural value or direct construction call.
    fn bodyHasLocalConstructionLoop(self: *Pass, body: Ast.ExprId) Allocator.Error!bool {
        var construction_bound = std.AutoHashMap(Ast.LocalId, usize).init(self.allocator);
        defer construction_bound.deinit();
        try self.collectConstructionBoundLocals(body, &construction_bound);
        if (construction_bound.count() == 0) return false;
        return self.loopConsumesConstructionBoundLocal(body, &construction_bound);
    }

    /// Record every local bound (in a block statement or a `let` expression) to
    /// an `if`/`match` whose branches build iterator values — the sources a
    /// branch-chosen `for` loop consumes.
    fn collectBranchBoundLocals(
        self: *Pass,
        expr_id: Ast.ExprId,
        out: *std.AutoHashMap(Ast.LocalId, void),
    ) Allocator.Error!void {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .let_ => |let_| {
                try self.noteBranchBoundBinding(let_.bind, let_.value, out);
                try self.collectBranchBoundLocals(let_.value, out);
                try self.collectBranchBoundLocals(let_.rest, out);
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| {
                    const stmt_id = GuardedList.at(statements, index);
                    switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| {
                            try self.noteBranchBoundBinding(let_.pat, let_.value, out);
                            try self.collectBranchBoundLocals(let_.value, out);
                        },
                        .expr, .expect, .dbg => |value| try self.collectBranchBoundLocals(value, out),
                        .return_ => |ret| try self.collectBranchBoundLocals(ret.value, out),
                        else => {},
                    }
                }
                try self.collectBranchBoundLocals(block.final_expr, out);
            },
            .loop_ => |loop| {
                const initial_values = self.program.exprSpan(loop.initial_values);
                for (0..initial_values.len) |index| try self.collectBranchBoundLocals(GuardedList.at(initial_values, index), out);
                try self.collectBranchBoundLocals(loop.body, out);
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| try self.collectBranchBoundLocals(GuardedList.at(branches, index).body, out);
                try self.collectBranchBoundLocals(if_.final_else, out);
            },
            .match_ => |match| {
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| try self.collectBranchBoundLocals(GuardedList.at(branches, index).body, out);
            },
            .nominal, .dbg, .expect => |child| try self.collectBranchBoundLocals(child, out),
            .static_data_candidate => |candidate| try self.collectBranchBoundLocals(candidate.runtime_expr, out),
            .return_ => |ret| try self.collectBranchBoundLocals(ret.value, out),
            .comptime_branch_taken => |taken| try self.collectBranchBoundLocals(taken.body, out),
            else => {},
        }
    }

    fn noteBranchBoundBinding(
        self: *Pass,
        pat_id: Ast.PatId,
        value_id: Ast.ExprId,
        out: *std.AutoHashMap(Ast.LocalId, void),
    ) Allocator.Error!void {
        const local = switch (self.program.getPat(pat_id).data) {
            .bind => |local| local,
            else => return,
        };
        switch (self.program.getExpr(value_id).data) {
            .if_, .match_ => try out.put(local, {}),
            .local => |source| if (out.contains(source)) try out.put(local, {}),
            else => {},
        }
    }

    /// Record every local bound to a direct construction call.
    fn collectConstructionBoundLocals(
        self: *Pass,
        expr_id: Ast.ExprId,
        out: *std.AutoHashMap(Ast.LocalId, usize),
    ) Allocator.Error!void {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .let_ => |let_| {
                try self.noteConstructionBoundBinding(let_.bind, let_.value, out);
                try self.collectConstructionBoundLocals(let_.value, out);
                try self.collectConstructionBoundLocals(let_.rest, out);
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| {
                    const stmt_id = GuardedList.at(statements, index);
                    switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| {
                            try self.noteConstructionBoundBinding(let_.pat, let_.value, out);
                            try self.collectConstructionBoundLocals(let_.value, out);
                        },
                        .expr, .expect, .dbg => |value| try self.collectConstructionBoundLocals(value, out),
                        .return_ => |ret| try self.collectConstructionBoundLocals(ret.value, out),
                        else => {},
                    }
                }
                try self.collectConstructionBoundLocals(block.final_expr, out);
            },
            .loop_ => |loop| {
                const initial_values = self.program.exprSpan(loop.initial_values);
                for (0..initial_values.len) |index| try self.collectConstructionBoundLocals(GuardedList.at(initial_values, index), out);
                try self.collectConstructionBoundLocals(loop.body, out);
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| try self.collectConstructionBoundLocals(GuardedList.at(branches, index).body, out);
                try self.collectConstructionBoundLocals(if_.final_else, out);
            },
            .match_ => |match| {
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| try self.collectConstructionBoundLocals(GuardedList.at(branches, index).body, out);
            },
            .nominal, .dbg, .expect => |child| try self.collectConstructionBoundLocals(child, out),
            .static_data_candidate => |candidate| try self.collectConstructionBoundLocals(candidate.runtime_expr, out),
            .return_ => |ret| try self.collectConstructionBoundLocals(ret.value, out),
            .comptime_branch_taken => |taken| try self.collectConstructionBoundLocals(taken.body, out),
            else => {},
        }
    }

    fn noteConstructionBoundBinding(
        self: *Pass,
        pat_id: Ast.PatId,
        value_id: Ast.ExprId,
        out: *std.AutoHashMap(Ast.LocalId, usize),
    ) Allocator.Error!void {
        const local = switch (self.program.getPat(pat_id).data) {
            .bind => |local| local,
            else => return,
        };
        if ((try self.constructorShape(value_id)) != null) {
            try out.put(local, 1);
            return;
        }
        if (self.program.getExpr(value_id).data == .local) {
            if (out.get(self.program.getExpr(value_id).data.local)) |depth| {
                try out.put(local, depth);
                return;
            }
        }
        if (!self.localHasIteratorNamedType(local)) return;
        var budget: usize = 64;
        const depth = self.iteratorConstructionDepth(value_id, out, &budget);
        if (depth == 0) return;
        try out.put(local, depth);
    }

    /// Whether a loop consumes one of the locals bound to a known construction
    /// in the same body.
    fn loopConsumesConstructionBoundLocal(
        self: *Pass,
        expr_id: Ast.ExprId,
        set: *std.AutoHashMap(Ast.LocalId, usize),
    ) Common.LowerError!bool {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .loop_ => |loop| {
                const initials = self.program.exprSpan(loop.initial_values);
                for (0..initials.len) |index| {
                    const initial = self.program.getExpr(GuardedList.at(initials, index));
                    if (initial.data == .local and set.contains(initial.data.local)) return true;
                }
                if (try self.matchIteratorLoopParts(expr_id)) |parts| {
                    if (set.get(parts.source_local)) |depth| {
                        if (depth >= 2 and self.localHasIteratorNamedType(parts.source_local)) return true;
                    }
                }
                return self.loopConsumesConstructionBoundLocal(loop.body, set);
            },
            .let_ => |let_| {
                return (try self.loopConsumesConstructionBoundLocal(let_.value, set)) or
                    (try self.loopConsumesConstructionBoundLocal(let_.rest, set));
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| {
                    const stmt_id = GuardedList.at(statements, index);
                    const found = switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| try self.loopConsumesConstructionBoundLocal(let_.value, set),
                        .expr, .expect, .dbg => |value| try self.loopConsumesConstructionBoundLocal(value, set),
                        .return_ => |ret| try self.loopConsumesConstructionBoundLocal(ret.value, set),
                        else => false,
                    };
                    if (found) return true;
                }
                return self.loopConsumesConstructionBoundLocal(block.final_expr, set);
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    if (try self.loopConsumesConstructionBoundLocal(GuardedList.at(branches, index).body, set)) return true;
                }
                return self.loopConsumesConstructionBoundLocal(if_.final_else, set);
            },
            .match_ => |match| {
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    if (try self.loopConsumesConstructionBoundLocal(GuardedList.at(branches, index).body, set)) return true;
                }
                return false;
            },
            .nominal, .dbg, .expect => |child| return self.loopConsumesConstructionBoundLocal(child, set),
            .static_data_candidate => |candidate| return self.loopConsumesConstructionBoundLocal(candidate.runtime_expr, set),
            .return_ => |ret| return self.loopConsumesConstructionBoundLocal(ret.value, set),
            .comptime_branch_taken => |taken| return self.loopConsumesConstructionBoundLocal(taken.body, set),
            else => return false,
        }
    }

    fn localHasIteratorNamedType(self: *Pass, local: Ast.LocalId) bool {
        const ty = self.program.getLocal(local).ty;
        return self.typeIsIteratorNamed(ty);
    }

    fn typeIsIteratorNamed(self: *Pass, ty: Type.TypeId) bool {
        return switch (self.program.types.get(ty)) {
            .named => |named| blk: {
                const type_name = self.program.names.typeNameText(named.def.type_name);
                break :blk named.def.iterator_representation != .none or std.mem.eql(u8, type_name, "Builtin.Iter");
            },
            else => false,
        };
    }

    fn iteratorConstructionDepth(
        self: *Pass,
        expr_id: Ast.ExprId,
        known_depths: *std.AutoHashMap(Ast.LocalId, usize),
        budget: *usize,
    ) usize {
        if (budget.* == 0) return 0;
        budget.* -= 1;

        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .local => |local| known_depths.get(local) orelse 0,
            .call_proc => |call| blk: {
                if (Ast.localDirectCallee(call) == null) break :blk 0;
                if (!self.typeIsIteratorNamed(expr.ty)) break :blk 0;

                var inner_depth: usize = 0;
                const args = self.program.exprSpan(call.args);
                for (0..args.len) |index| {
                    inner_depth = @max(inner_depth, self.iteratorConstructionDepth(GuardedList.at(args, index), known_depths, budget));
                }
                break :blk inner_depth + 1;
            },
            .nominal => |backing| self.iteratorConstructionDepth(backing, known_depths, budget),
            .block => |block| blk: {
                if (self.program.stmtSpan(block.statements).len != 0) break :blk 0;
                break :blk self.iteratorConstructionDepth(block.final_expr, known_depths, budget);
            },
            .static_data_candidate => |candidate| self.iteratorConstructionDepth(candidate.runtime_expr, known_depths, budget),
            .comptime_branch_taken => |taken| self.iteratorConstructionDepth(taken.body, known_depths, budget),
            else => 0,
        };
    }

    /// Whether some loop's first carried value is an identity-style construction
    /// over one of the branch-bound locals.
    fn loopConsumesBranchBoundLocal(
        self: *Pass,
        expr_id: Ast.ExprId,
        set: *std.AutoHashMap(Ast.LocalId, void),
    ) Allocator.Error!bool {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .loop_ => |loop| {
                const initials = self.program.exprSpan(loop.initial_values);
                if (initials.len != 0 and self.loopInitialConsumesLocal(GuardedList.at(initials, 0), set)) return true;
                return self.loopConsumesBranchBoundLocal(loop.body, set);
            },
            .let_ => |let_| {
                return (try self.loopConsumesBranchBoundLocal(let_.value, set)) or
                    (try self.loopConsumesBranchBoundLocal(let_.rest, set));
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |index| {
                    const stmt_id = GuardedList.at(statements, index);
                    const found = switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| try self.loopConsumesBranchBoundLocal(let_.value, set),
                        .expr, .expect, .dbg => |value| try self.loopConsumesBranchBoundLocal(value, set),
                        .return_ => |ret| try self.loopConsumesBranchBoundLocal(ret.value, set),
                        else => false,
                    };
                    if (found) return true;
                }
                return self.loopConsumesBranchBoundLocal(block.final_expr, set);
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    if (try self.loopConsumesBranchBoundLocal(GuardedList.at(branches, index).body, set)) return true;
                }
                return self.loopConsumesBranchBoundLocal(if_.final_else, set);
            },
            .match_ => |match| {
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    if (try self.loopConsumesBranchBoundLocal(GuardedList.at(branches, index).body, set)) return true;
                }
                return false;
            },
            .nominal, .dbg, .expect => |child| return self.loopConsumesBranchBoundLocal(child, set),
            .static_data_candidate => |candidate| return self.loopConsumesBranchBoundLocal(candidate.runtime_expr, set),
            .return_ => |ret| return self.loopConsumesBranchBoundLocal(ret.value, set),
            .comptime_branch_taken => |taken| return self.loopConsumesBranchBoundLocal(taken.body, set),
            else => return false,
        }
    }

    /// Whether a loop's first initial is a single-local direct-call construction
    /// (`Iter.iter(named)`) over a branch-bound local.
    fn loopInitialConsumesLocal(
        self: *Pass,
        expr_id: Ast.ExprId,
        set: *std.AutoHashMap(Ast.LocalId, void),
    ) bool {
        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .local => |local| set.contains(local),
            .call_proc => |call| blk: {
                if (Ast.localDirectCallee(call) == null) break :blk false;
                const args = self.program.exprSpan(call.args);
                if (args.len != 1) break :blk false;
                const arg = self.program.getExpr(GuardedList.at(args, 0));
                break :blk switch (arg.data) {
                    .local => |local| set.contains(local),
                    else => false,
                };
            },
            else => false,
        };
    }

    /// The lowered desugared `for`-loop over an iterator: an iterator slot
    /// plus zero or one carried accumulator, whose body pulls the next item and
    /// dispatches on the pull result. Recognized structurally so the peel can
    /// factor the shared base iteration out of a branch-chosen source. A
    /// zero-carry loop is a side-effecting drive (optionally with an early
    /// `return`, e.g. a short-circuit search); a one-carry loop is a fold whose
    /// per-element result is the accumulator value the `One` arm continues with.
    const IteratorLoopParts = struct {
        /// The local fed to the iterator constructor in the iterator slot's
        /// initial value — the branch-bound source the loop consumes.
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
        /// The `One(...)` payload's item pattern — bound to each pulled element.
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
        const block = switch (body_expr.data) {
            .block => |b| b,
            else => {
                return null;
            },
        };
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
                    result_local = switch (self.program.getPat(let_.pat).data) {
                        .bind => |local| local,
                        else => continue,
                    };
                    loop_stmt_index = index;
                    loop_expr_id = let_.value;
                },
                .expr => |e| {
                    if (self.program.getExpr(e).data != .loop_) continue;
                    result_local = null;
                    loop_stmt_index = index;
                    loop_expr_id = e;
                },
                else => continue,
            }
            if (loop_stmt_index != null) break;
        }
        const li = loop_stmt_index orelse {
            return null;
        };

        const loop_parts = (try self.matchIteratorLoopParts(loop_expr_id)) orelse {
            return null;
        };
        if (localUseCountInExpr(self.program, loop_parts.source_local, body) != 1) {
            return null;
        }
        // A fold's result feeds the block's final expression directly, so the
        // transformed fold value can take its place.
        if (loop_parts.carry_count == 1) {
            const rl = result_local orelse {
                return null;
            };
            if (localExpr(self.program, block.final_expr) != rl) {
                return null;
            }
            if (localUseCountInExpr(self.program, rl, body) != 1) {
                return null;
            }
        } else if (result_local != null) {
            return null;
        }

        // Find the branch that binds the source, and confirm its arms share one
        // base source reached by unwrapping append adapter state.
        var collision_stmt_index: ?usize = null;
        var branch_expr_id: Ast.ExprId = undefined;
        for (stmts, 0..) |stmt_id, index| {
            const let_ = switch (self.program.getStmt(stmt_id)) {
                .let_ => |l| l,
                else => continue,
            };
            const bound = switch (self.program.getPat(let_.pat).data) {
                .bind => |local| local,
                else => continue,
            };
            if (bound != loop_parts.source_local) continue;
            switch (self.program.getExpr(let_.value).data) {
                .if_, .match_ => {},
                else => {
                    return null;
                },
            }
            collision_stmt_index = index;
            branch_expr_id = let_.value;
            break;
        }
        const ci = collision_stmt_index orelse {
            return null;
        };

        const base_local = (try self.sharedArmBase(branch_expr_id)) orelse {
            return null;
        };

        // Build the loop so its iterator slot iterates the shared base.
        const new_loop = (try self.buildLoopOverBase(loop_expr_id, base_local, loop_parts)) orelse {
            return null;
        };

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
        const tail = (try self.buildTailDispatch(branch_expr_id, base_local, carry_start, loop_parts)) orelse {
            return null;
        };

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
            switch (expr.data) {
                .block => |block| {
                    if (self.program.stmtSpan(block.statements).len != 0) return current;
                    current = block.final_expr;
                },
                else => return current,
            }
        }
    }

    const DirectCall = struct { fn_id: Ast.FnId, args: Ast.ProgramSpanBorrow(Ast.ExprId, "expr_ids") };

    fn asDirectCall(self: *Pass, expr_id: Ast.ExprId) ?DirectCall {
        const expr = self.program.getExpr(expr_id);
        if (expr.data != .call_proc) return null;
        const call = expr.data.call_proc;
        const fn_id = Ast.localDirectCallee(call) orelse return null;
        return .{ .fn_id = fn_id, .args = self.program.exprSpan(call.args) };
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

        // The iterator slot's initial constructs the iterator from one source
        // local — the branch-bound value.
        const iter_call = self.asDirectCall(GuardedList.at(initials, 0)) orelse return null;
        if (iter_call.args.len != 1) return null;
        const source_local = localExpr(self.program, GuardedList.at(iter_call.args, 0)) orelse return null;

        const match_expr = self.program.getExpr(self.stripArmBlock(loop.body));
        if (match_expr.data != .match_) return null;
        const match = match_expr.data.match_;

        // The scrutinee pulls the next item from the iterator slot.
        const next_call = self.asDirectCall(match.scrutinee) orelse return null;
        if (next_call.args.len != 1) return null;
        if (localExpr(self.program, GuardedList.at(next_call.args, 0)) != iter_param) return null;

        var item_pat: ?Ast.PatId = null;
        var one_body: Ast.ExprId = undefined;
        var rest_local: Ast.LocalId = undefined;
        const branches = self.program.branchSpan(match.branches);
        for (0..branches.len) |branch_index| {
            const branch = GuardedList.at(branches, branch_index);
            if (branch.guard != null) return null;
            const pat = self.program.getPat(branch.pat);
            const tag = switch (pat.data) {
                .tag => |t| t,
                else => return null,
            };
            const payloads = self.program.patSpan(tag.payloads);
            if (payloads.len == 0) {
                // Exhausted arm: breaks, carrying the accumulator unchanged.
                const broke = self.stripArmBlock(branch.body);
                const break_val = switch (self.program.getExpr(broke).data) {
                    .break_ => |maybe| maybe,
                    else => return null,
                };
                if (carry_count == 0) {
                    if (break_val != null) return null;
                } else {
                    const bv = break_val orelse return null;
                    if (localExpr(self.program, bv) != carry_param) return null;
                }
                continue;
            }
            if (payloads.len != 1) return null;
            const record_fields = switch (self.program.getPat(GuardedList.at(payloads, 0)).data) {
                .record => |fields| self.program.recordDestructSpan(fields),
                else => return null,
            };
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
        // the loop's own (unit) result type. Reuse an existing type id — the
        // Monotype type store is frozen during this pass.
        const value_ty = if (carry_count == 1)
            carry_ty
        else
            self.program.getExpr(loop_expr_id).ty;
        return .{
            .source_local = source_local,
            .iter_init = GuardedList.at(initials, 0),
            .carry_count = carry_count,
            .carry_param = carry_param,
            .carry_ty = carry_ty,
            .value_ty = value_ty,
            .item_pat = ip,
            .one_body = one_body,
            .rest_local = rest_local,
        };
    }

    fn bindLocalOf(self: *Pass, pat_id: Ast.PatId) ?Ast.LocalId {
        return switch (self.program.getPat(pat_id).data) {
            .bind => |local| local,
            else => null,
        };
    }

    /// The values of the `continue` at the tail position of a loop-body arm,
    /// or null when the arm's tail is not a plain `continue`.
    fn tailContinueValues(self: *Pass, expr_id: Ast.ExprId) ?Ast.ProgramSpanBorrow(Ast.ExprId, "expr_ids") {
        const expr = self.program.getExpr(expr_id);
        return switch (expr.data) {
            .continue_ => |cont| self.program.exprSpan(cont.values),
            .block => |block| self.tailContinueValues(block.final_expr),
            else => null,
        };
    }

    /// The shared base local every arm of the source branch reduces to, or null
    /// when the arms do not share one base under append unwrapping.
    fn sharedArmBase(self: *Pass, branch_expr_id: Ast.ExprId) Common.LowerError!?Ast.LocalId {
        const expr = self.program.getExpr(branch_expr_id);
        var base: ?Ast.LocalId = null;
        switch (expr.data) {
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |branch_index| {
                    const br = GuardedList.at(branches, branch_index);
                    if (!try self.armBaseMatches(br.body, &base)) return null;
                }
                if (!try self.armBaseMatches(if_.final_else, &base)) return null;
            },
            .match_ => |match| {
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |branch_index| {
                    const br = GuardedList.at(branches, branch_index);
                    if (br.guard != null) return null;
                    if (!try self.armBaseMatches(br.body, &base)) return null;
                }
            },
            else => return null,
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
        if (call.args.len != 2) return null;
        if (!self.fnIsSuffixAppend(call.fn_id)) return null;
        const item = GuardedList.at(call.args, 1);
        const inner = (try self.reduceArmChain(GuardedList.at(call.args, 0))) orelse return null;
        defer self.allocator.free(inner.items);
        const items = try self.allocator.alloc(Ast.ExprId, inner.items.len + 1);
        @memcpy(items[0..inner.items.len], inner.items);
        items[inner.items.len] = item;
        return .{ .base = inner.base, .items = items };
    }

    /// Whether a two-argument function returns the internal representation
    /// Monotype minted for `Iter.append`. The producer records the adapter kind
    /// on the nominal; specialization consumes that evidence directly instead
    /// of reverse-engineering a generated step function's body shape.
    fn fnIsSuffixAppend(self: *Pass, fn_id: Ast.FnId) bool {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.program.fnCount()) return false;
        const fn_ = self.program.getFnAt(raw);
        if (self.program.typedLocalSpan(fn_.args).len != 2) return false;
        return switch (self.program.types.get(fn_.ret)) {
            .named => |named| named.def.iterator_representation == .minted and
                named.def.iterator_kind == .append,
            else => false,
        };
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
        const iter_call = iter_call_expr.data.call_proc;

        const base_ty = self.program.getLocal(base_local).ty;
        const base_ref = try self.program.addExpr(.{ .ty = base_ty, .data = .{ .local = base_local } });
        const new_iter_init = try self.program.addExpr(.{ .ty = iter_call_expr.ty, .data = .{ .call_proc = .{
            .callee = iter_call.callee,
            .args = try self.program.addExprSpan(&.{base_ref}),
            .captures = iter_call.captures,
            .is_cold = iter_call.is_cold,
        } } });

        // Keep every accumulator slot's initial value; only the iterator slot
        // changes to iterate the shared base.
        const initials = try GuardedList.dupe(self.allocator, Ast.ExprId, self.program.exprSpan(loop.initial_values));
        defer self.allocator.free(initials);
        initials[0] = new_iter_init;
        return try self.program.addExpr(.{ .ty = loop_expr.ty, .data = .{ .loop_ = .{
            .params = loop.params,
            .initial_values = try self.program.addExprSpan(initials),
            .body = loop.body,
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
        switch (expr.data) {
            .if_ => |if_| {
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
            },
            .match_ => |match| {
                const branches = try GuardedList.dupe(self.allocator, Ast.Branch, self.program.branchSpan(match.branches));
                defer self.allocator.free(branches);
                var rewritten = try self.allocator.alloc(Ast.Branch, branches.len);
                defer self.allocator.free(rewritten);
                for (branches, 0..) |br, index| {
                    const arm = (try self.buildArmTail(br.body, base_local, carry_start, loop_parts)) orelse return null;
                    rewritten[index] = .{ .pat = br.pat, .guard = br.guard, .body = arm };
                }
                return try self.program.addExpr(.{ .ty = loop_parts.value_ty, .data = .{ .match_ = .{
                    .scrutinee = match.scrutinee,
                    .branches = try self.program.addBranchSpan(rewritten),
                    .comptime_site = match.comptime_site,
                } } });
            },
            else => return null,
        }
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
        var renames = std.AutoHashMap(Ast.LocalId, Ast.LocalId).init(self.allocator);
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
        renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId),
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
                    if (br.guard != null) return null;
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
            else => return try self.cloneExprFresh(expr_id, renames),
        }
    }

    fn cloneStmtFresh(self: *Pass, stmt_id: Ast.StmtId, renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.StmtId {
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
            else => return null,
        }
    }

    /// Deep-clone a pure-computation expression, applying local renames and
    /// allocating fresh locals at binding sites. Returns null for constructs
    /// outside the foldable set.
    fn cloneExprFresh(self: *Pass, expr_id: Ast.ExprId, renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.ExprId {
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
                .field = field.field,
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
                    if (br.guard != null) return null;
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
            // iteration completes without returning — the same order the
            // unfused loop would return in.
            .return_ => |ret| .{ .return_ = .{
                .value = (try self.cloneExprFresh(ret.value, renames)) orelse return null,
                .target = ret.target,
            } },
            else => return null,
        };
        return try self.program.addExpr(.{ .ty = expr.ty, .data = data });
    }

    fn cloneExprSpanFresh(self: *Pass, span: Ast.Span(Ast.ExprId), renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.ExprId) {
        const source = try GuardedList.dupe(self.allocator, Ast.ExprId, self.program.exprSpan(span));
        defer self.allocator.free(source);
        var out = try self.allocator.alloc(Ast.ExprId, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |item, index| {
            out[index] = (try self.cloneExprFresh(item, renames)) orelse return null;
        }
        return try self.program.addExprSpan(out);
    }

    fn cloneCaptureOperandSpanFresh(self: *Pass, span: Ast.Span(Ast.CaptureOperand), renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.CaptureOperand) {
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

    fn cloneFieldSpanFresh(self: *Pass, span: Ast.Span(Ast.FieldExpr), renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.FieldExpr) {
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
    fn clonePatFresh(self: *Pass, pat_id: Ast.PatId, renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.PatId {
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
            else => return null,
        };
        return try self.program.addPat(.{ .ty = pat.ty, .data = data });
    }

    fn clonePatSpanFresh(self: *Pass, span: Ast.Span(Ast.PatId), renames: *std.AutoHashMap(Ast.LocalId, Ast.LocalId)) Common.LowerError!?Ast.Span(Ast.PatId) {
        const source = try GuardedList.dupe(self.allocator, Ast.PatId, self.program.patSpan(span));
        defer self.allocator.free(source);
        var out = try self.allocator.alloc(Ast.PatId, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |child, index| {
            out[index] = (try self.clonePatFresh(child, renames)) orelse return null;
        }
        return try self.program.addPatSpan(out);
    }

    /// Clone a whole function body through the value pass and replace it. The
    /// value pass inlines the branch-chosen iterator's construction into each
    /// branch, sinks the consuming loop into those branches, and scalarizes each
    /// sunk loop's carried state.
    fn cloneFnBodyInPlace(self: *Pass, fn_id: Ast.FnId, body: Ast.ExprId) Common.LowerError!void {
        const fn_index = @intFromEnum(fn_id);
        if (fn_index < self.whole_body_cloned.len and self.whole_body_cloned[fn_index]) return;

        var cloner = Cloner.initForRewrite(self);
        defer cloner.deinit();
        // The branch-chosen iterator's construction chain (its source `List.iter`,
        // each `append`/`map` adapter) spans separate `let` bindings. Its leaf
        // source is a list value the known-shape gate does not count, so the
        // source construction would stay residual and the branch would never
        // become a known value the loop can sink into. Counting a list source as
        // a known-shape argument exposes that construction — enough for the branch
        // to sink and each sunk loop to scalarize — without force-inlining the
        // arbitrary user calls whose over-inlining breaks known-match collapse.
        cloner.inline_direct_requires_known_arg = true;
        cloner.inline_list_source_construction = true;
        cloner.force_loop_initial_inline = true;
        const cloned = try cloner.cloneExpr(body);
        const fn_ = self.program.getFn(fn_id);
        self.program.setFn(fn_id, .{
            .symbol = fn_.symbol,
            .source = fn_.source,
            .args = fn_.args,
            .captures = fn_.captures,
            .body = .{ .roc = cloned },
            .ret = fn_.ret,
        });
        if (fn_index < self.whole_body_cloned.len) self.whole_body_cloned[fn_index] = true;
    }

    /// Collect outermost loops with an explicitly known constructor in their
    /// initial carried state. A nested loop is left to the clone of its
    /// enclosing loop, and a plain scalar counting loop does not qualify.
    fn collectKnownLoops(self: *Pass, expr_id: Ast.ExprId, out: *std.ArrayList(Ast.ExprId)) Allocator.Error!void {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .loop_ => |loop| {
                const initials = self.program.exprSpan(loop.initial_values);
                for (0..initials.len) |index| {
                    const initial = GuardedList.at(initials, index);
                    const constructor = try self.constructorShape(initial);
                    if (constructor != null or self.loopInitialIsOwnedConstruction(initial)) {
                        try out.append(self.allocator, expr_id);
                        return;
                    }
                }
                try self.collectKnownLoops(loop.body, out);
            },
            .let_ => |let_| {
                try self.collectKnownLoops(let_.value, out);
                try self.collectKnownLoops(let_.rest, out);
            },
            .block => |block| {
                const statements = self.program.stmtSpan(block.statements);
                for (0..statements.len) |stmt_index| {
                    const stmt_id = GuardedList.at(statements, stmt_index);
                    switch (self.program.getStmt(stmt_id)) {
                        .let_ => |let_| try self.collectKnownLoops(let_.value, out),
                        .expr, .expect, .dbg => |value| try self.collectKnownLoops(value, out),
                        .return_ => |ret| try self.collectKnownLoops(ret.value, out),
                        else => {},
                    }
                }
                try self.collectKnownLoops(block.final_expr, out);
            },
            .match_ => |match| {
                try self.collectKnownLoops(match.scrutinee, out);
                const branches = self.program.branchSpan(match.branches);
                for (0..branches.len) |branch_index| {
                    try self.collectKnownLoops(GuardedList.at(branches, branch_index).body, out);
                }
            },
            .if_ => |if_| {
                const branches = self.program.ifBranchSpan(if_.branches);
                for (0..branches.len) |branch_index| {
                    try self.collectKnownLoops(GuardedList.at(branches, branch_index).body, out);
                }
                try self.collectKnownLoops(if_.final_else, out);
            },
            .nominal, .dbg, .expect => |child| try self.collectKnownLoops(child, out),
            .static_data_candidate => |candidate| try self.collectKnownLoops(candidate.runtime_expr, out),
            .return_ => |ret| try self.collectKnownLoops(ret.value, out),
            .comptime_branch_taken => |taken| try self.collectKnownLoops(taken.body, out),
            .join_point => |join_point| {
                try self.collectKnownLoops(join_point.body, out);
                try self.collectKnownLoops(join_point.remainder, out);
            },
            .jump => |jump| {
                const args = self.program.exprSpan(jump.args);
                for (0..args.len) |index| try self.collectKnownLoops(GuardedList.at(args, index), out);
            },
            else => {},
        }
    }

    /// Whether a loop's first carried value is an iterator built directly over a
    /// source the loop owns — a construction call (`List.iter`, a range) whose
    /// arguments are all built inline rather than named locals. A `for` over a
    /// pipeline named beforehand (`for x in some_iter`) reads its source through
    /// a local, so it does not qualify here; a branch-chosen source instead
    /// routes through the whole-body clone, which sinks the loop into each
    /// branch where the source construction is inline.
    fn loopInitialIsOwnedConstruction(self: *Pass, expr_id: Ast.ExprId) bool {
        const expr = self.program.getExpr(expr_id);
        if (expr.data != .call_proc) return false;
        const call = expr.data.call_proc;
        if (Ast.localDirectCallee(call) == null) return false;
        const args = self.program.exprSpan(call.args);
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (self.program.getExpr(arg).data == .local) return false;
        }
        return true;
    }

    /// Clone one loop through the value pass and overwrite the original in
    /// place. Free locals the loop reads (its enclosing bindings) resolve to
    /// themselves, so only the loop-carried iterator construction inlines.
    fn cloneLoopInPlace(self: *Pass, loop_id: Ast.ExprId) Common.LowerError!void {
        var cloner = Cloner.initForRewrite(self);
        defer cloner.deinit();
        cloner.inline_direct_requires_known_arg = true;
        cloner.force_loop_initial_inline = true;
        const cloned = try cloner.cloneExpr(loop_id);
        self.program.setExprData(loop_id, self.program.getExpr(cloned).data);
    }

    fn rewriteCallsInExpr(self: *Pass, expr_id: Ast.ExprId, done: []bool) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (done[index]) return;
        done[index] = true;

        const expr = self.program.getExprAt(index);
        switch (expr.data) {
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

            if (try self.appendExistingCallArgs(&cloner, spec.pattern, args, &rewritten_args)) {
                const new_call: Ast.ExprData = .{ .call_proc = .{
                    .callee = .{ .lifted = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before rewriting") },
                    .args = try self.program.addExprSpan(rewritten_args.items),
                    .captures = call.captures,
                    .is_cold = call.is_cold,
                } };
                if (cloner.pending.items.len == 0) {
                    self.program.setExprData(expr_id, new_call);
                } else {
                    // Decomposing the argument created bindings its leaves
                    // reference; the rewritten call site becomes a let chain
                    // ending in the specialized call.
                    const call_ty = self.program.getExpr(expr_id).ty;
                    const call_expr = try cloner.addExpr(.{ .ty = call_ty, .data = new_call });
                    const wrapped = try cloner.flushPendingSince(0, call_expr);
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
        out: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!bool {
        if (pattern.args.len != args.len) Common.invariant("call-pattern arity differed from direct call arity");
        for (pattern.args, args) |shape, arg| {
            const value = try cloner.cloneExprValue(arg);
            if (!shapeMatchesValue(self.program, shape, value)) return false;
            try cloner.appendExprsFromValue(shape, value, out);
        }
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
                    else => return false,
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
                    else => return false,
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
                    else => return false,
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
                    else => return false,
                };
                if (!sameType(self.program, expr.ty, nominal.ty)) return false;
                return try self.appendExistingExprsForShape(nominal.backing.*, backing, out);
            },
            .callable => return false,
        }
    }

    fn constructorShape(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!?Shape {
        const expr = self.program.getExpr(expr_id);
        switch (expr.data) {
            .tag, .record, .tuple => assertStructuralConstructionType(self.program, expr.ty),
            else => {},
        }
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
            else => null,
        };
    }

    /// Total work budget for deriving one shape. Values reachable here are
    /// not always small finite trees — a loop-carried value can reference
    /// itself through the fixpoint of a recursive construction, and deep
    /// chains share substructure — so the walk spends one shared budget per
    /// node visit and degrades to `.any` (no known shape) when it runs out.
    /// `.any` is this function's existing "don't specialize on this" answer,
    /// so exhaustion is a missed specialization, never a wrong shape. See
    /// design.md "Core Principles" on bounded post-check walks.
    const shape_work_budget: u32 = 4096;

    fn shapeFromValue(self: *Pass, value: Value) Allocator.Error!?Shape {
        var budget: u32 = shape_work_budget;
        return try self.shapeFromValueBudgeted(value, &budget);
    }

    fn shapeFromValueBudgeted(self: *Pass, value: Value, budget: *u32) Allocator.Error!?Shape {
        if (budget.* == 0) return null;
        budget.* -= 1;
        return switch (value) {
            .expr => |expr| try self.constructorShape(expr),
            .static_data_candidate => |candidate| try self.shapeFromValueBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                const payloads = try self.arena.allocator().alloc(Shape, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = (try self.shapeFromValueBudgeted(payload, budget)) orelse
                        .{ .any = valueType(self.program, payload) };
                }
                break :blk Shape{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| blk: {
                const fields = try self.arena.allocator().alloc(FieldShape, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .shape = (try self.shapeFromValueBudgeted(field.value, budget)) orelse
                            .{ .any = valueType(self.program, field.value) },
                    };
                }
                break :blk Shape{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.arena.allocator().alloc(Shape, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = (try self.shapeFromValueBudgeted(item, budget)) orelse
                        .{ .any = valueType(self.program, item) };
                }
                break :blk Shape{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing_shape = (try self.shapeFromValueBudgeted(nominal.backing.*, budget)) orelse break :blk null;
                const stored = try self.arena.allocator().create(Shape);
                stored.* = backing_shape;
                break :blk Shape{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.arena.allocator().alloc(Shape, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = (try self.shapeFromValueBudgeted(capture.value, budget)) orelse
                        .{ .any = valueType(self.program, capture.value) };
                }
                break :blk Shape{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
        };
    }
};

const Cloner = struct {
    pass: *Pass,
    source_fn: Ast.FnId,
    pattern: CallPattern,
    subst: std.AutoHashMap(Ast.LocalId, Value),
    binder_subst: std.AutoHashMap(BinderIdentity, Value),
    changes: std.ArrayList(BindingChange),
    inline_stack: std.ArrayList(InlineFrame),
    loop_stack: std.ArrayList(LoopPattern),
    join_stack: std.ArrayList(ActiveJoinClone),
    /// Remaining arms the shape-preserving let-of-case rewrite may still
    /// process. That rewrite re-clones each arm's body against the small
    /// dispatch, and a re-cloned arm can contain further let-of-case values,
    /// so unbounded application compounds on recursively generated code
    /// (derived parsers) until the compiler overflows its stack. When the
    /// budget runs out the rewrite falls back to the plain shared join,
    /// which never re-clones arm bodies.
    let_case_shape_arms_remaining: usize,
    /// Active let-of-case join rewrites, innermost last. Cloning a jump whose
    /// target belongs to one of these frames records the jump site's symbolic
    /// argument values for later parameter decomposition instead of cloning
    /// the argument expressions directly.
    let_case_builds: std.ArrayList(*LetCaseBuild),
    /// Bindings created while producing a structured value, not yet emitted.
    /// Each holds a fresh local the value's leaves reference. They are
    /// emitted — oldest outermost, preserving evaluation order — at the
    /// nearest enclosing region boundary (`cloneExpr`), or earlier by any
    /// construct that pins its value with `resolvePending`.
    pending: std.ArrayList(PendingLet),
    /// Count of effect-bearing expressions emitted so far. Compared against
    /// `region_entry_marks` to decide whether a pending binding may move to
    /// its region's start without crossing an effect.
    effect_marks: usize,
    region_entry_marks: usize,
    inline_direct_calls: bool,
    inline_direct_requires_known_arg: bool,
    rewrite_call_patterns: bool,
    /// Pattern discovery and detect-only walks do not own output functions.
    /// Production clones reserve callable workers through the pass-wide table.
    emit_callable_workers: bool,
    value_aware_rewrite_changed: bool,
    value_aware_detect_only: bool,
    /// Shape-demanding owners may specialize nonrecursive callees; all other
    /// owners restrict let-substituted call patterns to recursive workers.
    allow_nonrecursive_value_patterns: bool,
    /// When set, a loop's initial values inline their construction call even
    /// without a known-shape argument, exposing an iterator constructor whose
    /// arguments (a source list, a range bound) are opaque scalars. Only set for
    /// an in-place loop clone, where the surrounding bindings are absent, so a
    /// named upstream pipeline stays a residual value rather than being expanded
    /// here (which the branch-join/`append` value tracking is not yet ready for).
    force_loop_initial_inline: bool = false,
    /// When set, a `list` (or `str`) source expression counts as a known-shape
    /// argument, so a direct construction over it (`List.iter(list)`) inlines
    /// even under the known-shape gate. Set only for a branch-chosen loop's
    /// whole-body clone, where the iterator source must inline for the branch to
    /// become a known value the loop can sink into.
    inline_list_source_construction: bool = false,
    /// Work left for case-of-case distribution in this clone. Each produced
    /// branch body spends one unit before it is cloned, so nested distribution
    /// cannot multiply the expression store without bound. Depth is bounded
    /// separately so a narrow rewrite cannot exhaust the compiler stack before
    /// spending this total work budget.
    case_of_case_work_remaining: u32,
    case_of_case_depth: u8,
    current_loc: SourceLoc,
    current_region: Region,

    const case_of_case_work_budget: u32 = 256;
    const case_of_case_depth_limit: u8 = 64;

    fn init(pass: *Pass, source_fn: Ast.FnId, pattern: CallPattern) Cloner {
        return .{
            .pass = pass,
            .source_fn = source_fn,
            .pattern = pattern,
            .subst = std.AutoHashMap(Ast.LocalId, Value).init(pass.allocator),
            .binder_subst = std.AutoHashMap(BinderIdentity, Value).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .loop_stack = .empty,
            .join_stack = .empty,
            .let_case_shape_arms_remaining = let_case_shape_arm_budget,
            .let_case_builds = .empty,
            .pending = .empty,
            .effect_marks = 0,
            .region_entry_marks = 0,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = true,
            .rewrite_call_patterns = true,
            .emit_callable_workers = true,
            .value_aware_rewrite_changed = false,
            .value_aware_detect_only = false,
            .allow_nonrecursive_value_patterns = false,
            .case_of_case_work_remaining = case_of_case_work_budget,
            .case_of_case_depth = 0,
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
        };
    }

    fn initForRewrite(pass: *Pass) Cloner {
        return .{
            .pass = pass,
            .source_fn = undefined, // initForRewrite never calls buildArgs, which is the only reader.
            .pattern = .{ .args = &.{} },
            .subst = std.AutoHashMap(Ast.LocalId, Value).init(pass.allocator),
            .binder_subst = std.AutoHashMap(BinderIdentity, Value).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .loop_stack = .empty,
            .join_stack = .empty,
            .let_case_shape_arms_remaining = let_case_shape_arm_budget,
            .let_case_builds = .empty,
            .pending = .empty,
            .effect_marks = 0,
            .region_entry_marks = 0,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = false,
            .rewrite_call_patterns = true,
            .emit_callable_workers = true,
            .value_aware_rewrite_changed = false,
            .value_aware_detect_only = false,
            .allow_nonrecursive_value_patterns = false,
            .case_of_case_work_remaining = case_of_case_work_budget,
            .case_of_case_depth = 0,
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
        };
    }

    fn deinit(self: *Cloner) void {
        self.pending.deinit(self.pass.allocator);
        self.inline_stack.deinit(self.pass.allocator);
        self.loop_stack.deinit(self.pass.allocator);
        self.join_stack.deinit(self.pass.allocator);
        self.let_case_builds.deinit(self.pass.allocator);
        self.changes.deinit(self.pass.allocator);
        self.binder_subst.deinit();
        self.subst.deinit();
    }

    fn collectCallPatternsInExpr(self: *Cloner, owner: Ast.FnId, expr_id: Ast.ExprId) Common.LowerError!void {
        const expr = self.pass.program.getExpr(expr_id);
        switch (expr.data) {
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
                if (!self.allow_nonrecursive_value_patterns and
                    (callee_raw >= self.pass.self_recursive_fns.len or !self.pass.self_recursive_fns[callee_raw])) return;
                const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(call.args));
                defer self.pass.allocator.free(args);

                const pending_start = self.pending.items.len;
                defer self.pending.shrinkRetainingCapacity(pending_start);

                const values = try self.pass.allocator.alloc(Value, args.len);
                defer self.pass.allocator.free(values);
                for (args, 0..) |arg, index| {
                    values[index] = try self.cloneExprValue(arg);
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
                const change_start = self.changes.items.len;
                const pending_start = self.pending.items.len;
                defer {
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_start);
                }
                try self.collectCallPatternsInStmtSpan(owner, block.statements);
                try self.collectCallPatternsInExpr(owner, block.final_expr);
            },
            .loop_ => |loop| {
                try self.collectCallPatternsInExprSpan(owner, loop.initial_values);
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
                const params = self.pass.program.typedLocalSpan(loop.params);
                for (0..params.len) |index| {
                    try self.shadowLocal(GuardedList.at(params, index).local);
                }
                try self.collectCallPatternsInExpr(owner, loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectCallPatternsInExpr(owner, value),
            .continue_ => |continue_| try self.collectCallPatternsInExprSpan(owner, continue_.values),
            .join_point => |join_point| {
                const change_start = self.changes.items.len;
                const params = self.pass.program.typedLocalSpan(join_point.params);
                for (0..params.len) |index| try self.shadowLocal(GuardedList.at(params, index).local);
                try self.collectCallPatternsInExpr(owner, join_point.body);
                self.restore(change_start);
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
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
                try self.shadowLocal(sequence.ok_local);
                try self.collectCallPatternsInExpr(owner, sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.collectCallPatternsInExpr(owner, sequence.try_expr);
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
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

        const change_start = self.changes.items.len;
        const pending_start = self.pending.items.len;
        defer {
            self.restore(change_start);
            self.pending.shrinkRetainingCapacity(pending_start);
        }

        const value = try self.cloneExprValue(value_expr);
        if (!try self.bindPatternForValueFlow(pat_id, value_expr, recursive, value)) {
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
                const change_start = ctx.self.changes.items.len;
                defer ctx.self.restore(change_start);
                try ctx.self.shadowPatLocals(branch.pat);
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

                const pending_start = self.pending.items.len;
                defer self.pending.shrinkRetainingCapacity(pending_start);

                const value = try self.cloneExprValue(let_.value);
                if (!try self.bindPatternForValueFlow(let_.pat, let_.value, let_.recursive, value)) {
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
    ) Common.LowerError!bool {
        const change_before = self.changes.items.len;
        const pending_before = self.pending.items.len;
        if (try self.bindPatToReusableValue(pat_id, value) == .match) return true;
        self.restore(change_before);
        self.pending.shrinkRetainingCapacity(pending_before);

        const pat = self.pass.program.getPat(pat_id);
        const self_referential = switch (pat.data) {
            .bind => |local| localUseCountInExpr(self.pass.program, local, source_value) != 0,
            else => recursive,
        };
        if (self_referential) return false;

        const reusable = try self.makeReusableForMatch(value);
        if (try self.bindPatToFlowValue(pat_id, reusable)) return true;
        self.restore(change_before);
        self.pending.shrinkRetainingCapacity(pending_before);
        return false;
    }

    fn rewriteCallsWithValuesInExpr(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!void {
        const expr = self.pass.program.getExpr(expr_id);
        switch (expr.data) {
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
            .fn_ref => |fn_ref| try self.rewriteCallsWithValuesInCaptureOperandSpan(fn_ref.captures),
            .list,
            .tuple,
            => |items| try self.rewriteCallsWithValuesInExprSpan(items),
            .record => |fields| try self.rewriteCallsWithValuesInFieldExprSpan(fields),
            .tag => |tag| try self.rewriteCallsWithValuesInExprSpan(tag.payloads),
            .static_data_candidate => |candidate| try self.rewriteCallsWithValuesInExpr(candidate.runtime_expr),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.rewriteCallsWithValuesInExpr(child),
            .return_ => |ret| try self.rewriteCallsWithValuesInExpr(ret.value),
            .expect_err => |expect_err| try self.rewriteCallsWithValuesInExpr(expect_err.msg),
            .comptime_branch_taken => |taken| try self.rewriteCallsWithValuesInExpr(taken.body),
            .let_ => |let_| {
                try self.rewriteCallsWithValuesInExpr(let_.value);
                const change_start = self.changes.items.len;
                const pending_start = self.pending.items.len;
                defer {
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_start);
                }
                const value = try self.cloneExprValue(let_.value);
                if (!try self.bindPatternForValueFlow(let_.bind, let_.value, false, value)) {
                    try self.shadowPatLocals(let_.bind);
                }
                try self.rewriteCallsWithValuesInExpr(let_.rest);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .call_value => |call| {
                try self.rewriteCallsWithValuesInExpr(call.callee);
                try self.rewriteCallsWithValuesInExprSpan(call.args);
            },
            .call_proc => |call| {
                try self.rewriteCallsWithValuesInExprSpan(call.args);
                try self.rewriteCallsWithValuesInCaptureOperandSpan(call.captures);
                try self.rewriteCallProcWithValues(expr_id, call);
            },
            .low_level => |call| try self.rewriteCallsWithValuesInExprSpan(call.args),
            .field_access => |field| try self.rewriteCallsWithValuesInExpr(field.receiver),
            .tuple_access => |access| try self.rewriteCallsWithValuesInExpr(access.tuple),
            .structural_eq => |eq| {
                try self.rewriteCallsWithValuesInExpr(eq.lhs);
                try self.rewriteCallsWithValuesInExpr(eq.rhs);
            },
            .structural_hash => |h| {
                try self.rewriteCallsWithValuesInExpr(h.value);
                try self.rewriteCallsWithValuesInExpr(h.hasher);
            },
            .match_ => |match| {
                try self.rewriteCallsWithValuesInExpr(match.scrutinee);
                try self.rewriteCallsWithValuesInBranchSpan(match.branches);
            },
            .if_ => |if_| {
                try self.rewriteCallsWithValuesInIfBranchSpan(if_.branches);
                try self.rewriteCallsWithValuesInExpr(if_.final_else);
            },
            .block => |block| {
                const change_start = self.changes.items.len;
                const pending_start = self.pending.items.len;
                defer {
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_start);
                }
                try self.rewriteCallsWithValuesInStmtSpan(block.statements);
                try self.rewriteCallsWithValuesInExpr(block.final_expr);
            },
            .loop_ => |loop| {
                try self.rewriteCallsWithValuesInExprSpan(loop.initial_values);
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
                const params = self.pass.program.typedLocalSpan(loop.params);
                for (0..params.len) |index| {
                    try self.shadowLocal(GuardedList.at(params, index).local);
                }
                try self.rewriteCallsWithValuesInExpr(loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.rewriteCallsWithValuesInExpr(value),
            .continue_ => |continue_| try self.rewriteCallsWithValuesInExprSpan(continue_.values),
            .join_point => |join_point| {
                const change_start = self.changes.items.len;
                const params = self.pass.program.typedLocalSpan(join_point.params);
                for (0..params.len) |index| try self.shadowLocal(GuardedList.at(params, index).local);
                try self.rewriteCallsWithValuesInExpr(join_point.body);
                self.restore(change_start);
                try self.rewriteCallsWithValuesInExpr(join_point.remainder);
            },
            .jump => |jump| try self.rewriteCallsWithValuesInExprSpan(jump.args),
            .if_initialized_payload => |payload_switch| {
                try self.rewriteCallsWithValuesInExpr(payload_switch.cond);
                try self.rewriteCallsWithValuesInExpr(payload_switch.initialized);
                try self.rewriteCallsWithValuesInExpr(payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.rewriteCallsWithValuesInExpr(sequence.try_expr);
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
                try self.shadowLocal(sequence.ok_local);
                try self.rewriteCallsWithValuesInExpr(sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.rewriteCallsWithValuesInExpr(sequence.try_expr);
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
                try self.shadowLocal(sequence.value_local);
                try self.shadowLocal(sequence.rest_local);
                try self.rewriteCallsWithValuesInExpr(sequence.ok_body);
            },
        }
    }

    fn rewriteCallProcWithValues(self: *Cloner, expr_id: Ast.ExprId, call: @import("../monotype/ast.zig").CallProc) Common.LowerError!void {
        if (call.is_cold) return;
        const callee = Ast.localDirectCallee(call) orelse return;
        const raw = @intFromEnum(callee);
        if (raw >= self.pass.plans.len or self.pass.plans[raw].specs.items.len == 0) return;
        if (!self.allow_nonrecursive_value_patterns and
            (raw >= self.pass.self_recursive_fns.len or !self.pass.self_recursive_fns[raw])) return;

        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(call.args));
        defer self.pass.allocator.free(args);

        const pending_start = self.pending.items.len;
        defer self.pending.shrinkRetainingCapacity(pending_start);

        const values = try self.pass.allocator.alloc(Value, args.len);
        defer self.pass.allocator.free(values);
        for (args, 0..) |arg, index| {
            values[index] = try self.cloneExprValue(arg);
        }

        for (self.pass.plans[raw].specs.items) |spec| {
            if (spec.pattern.args.len != values.len) Common.invariant("call-pattern arity differed from direct call arity");
            var matches = true;
            for (spec.pattern.args, values) |shape, value| {
                if (!shapeMatchesValue(self.pass.program, shape, value)) {
                    matches = false;
                    break;
                }
            }
            if (!matches) continue;

            self.value_aware_rewrite_changed = true;
            if (self.value_aware_detect_only) return;

            var rewritten_args = std.ArrayList(Ast.ExprId).empty;
            defer rewritten_args.deinit(self.pass.allocator);
            for (spec.pattern.args, values) |shape, value| {
                try self.appendExprsFromValue(shape, value, &rewritten_args);
            }

            const new_call: Ast.ExprData = .{ .call_proc = .{
                .callee = .{ .lifted = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before value-aware rewriting") },
                .args = try self.pass.program.addExprSpan(rewritten_args.items),
                .captures = call.captures,
                .is_cold = call.is_cold,
            } };
            if (self.pending.items.len == pending_start) {
                self.pass.program.setExprData(expr_id, new_call);
            } else {
                const call_ty = self.pass.program.getExpr(expr_id).ty;
                const call_expr = try self.addExpr(.{ .ty = call_ty, .data = new_call });
                const wrapped = try self.flushPendingSince(pending_start, call_expr);
                self.pass.program.setExprData(expr_id, self.pass.program.getExpr(wrapped).data);
            }
            return;
        }
    }

    fn rewriteCallsWithValuesInExprSpan(self: *Cloner, span: Ast.Span(Ast.ExprId)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(span), self, struct {
            fn visit(cloner: *Cloner, expr: Ast.ExprId) Common.LowerError!void {
                try cloner.rewriteCallsWithValuesInExpr(expr);
            }
        }.visit);
    }

    fn rewriteCallsWithValuesInCaptureOperandSpan(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.CaptureOperand, self.pass.program.captureOperandSpan(span), self, struct {
            fn visit(cloner: *Cloner, operand: Ast.CaptureOperand) Common.LowerError!void {
                try cloner.rewriteCallsWithValuesInExpr(operand.value);
            }
        }.visit);
    }

    fn rewriteCallsWithValuesInFieldExprSpan(self: *Cloner, span: Ast.Span(Ast.FieldExpr)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.FieldExpr, self.pass.program.fieldExprSpan(span), self, struct {
            fn visit(cloner: *Cloner, field: Ast.FieldExpr) Common.LowerError!void {
                try cloner.rewriteCallsWithValuesInExpr(field.value);
            }
        }.visit);
    }

    fn rewriteCallsWithValuesInBranchSpan(self: *Cloner, span: Ast.Span(Ast.Branch)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(span), self, struct {
            fn visit(cloner: *Cloner, branch: Ast.Branch) Common.LowerError!void {
                const change_start = cloner.changes.items.len;
                defer cloner.restore(change_start);
                try cloner.shadowPatLocals(branch.pat);
                if (branch.guard) |guard| try cloner.rewriteCallsWithValuesInExpr(guard);
                try cloner.rewriteCallsWithValuesInExpr(branch.body);
            }
        }.visit);
    }

    fn rewriteCallsWithValuesInIfBranchSpan(self: *Cloner, span: Ast.Span(Ast.IfBranch)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.IfBranch, self.pass.program.ifBranchSpan(span), self, struct {
            fn visit(cloner: *Cloner, branch: Ast.IfBranch) Common.LowerError!void {
                try cloner.rewriteCallsWithValuesInExpr(branch.cond);
                try cloner.rewriteCallsWithValuesInExpr(branch.body);
            }
        }.visit);
    }

    fn rewriteCallsWithValuesInStmtSpan(self: *Cloner, span: Ast.Span(Ast.StmtId)) Common.LowerError!void {
        try walkSpanCloned(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(span), self, struct {
            fn visit(cloner: *Cloner, stmt: Ast.StmtId) Common.LowerError!void {
                try cloner.rewriteCallsWithValuesInStmt(stmt);
            }
        }.visit);
    }

    fn rewriteCallsWithValuesInStmt(self: *Cloner, stmt_id: Ast.StmtId) Common.LowerError!void {
        switch (self.pass.program.getStmt(stmt_id)) {
            .let_ => |let_| {
                try self.rewriteCallsWithValuesInExpr(let_.value);

                const pending_start = self.pending.items.len;
                defer self.pending.shrinkRetainingCapacity(pending_start);

                const value = try self.cloneExprValue(let_.value);
                if (!try self.bindPatternForValueFlow(let_.pat, let_.value, let_.recursive, value)) {
                    try self.shadowPatLocals(let_.pat);
                }
            },
            .expr,
            .expect,
            .dbg,
            => |expr| try self.rewriteCallsWithValuesInExpr(expr),
            .return_ => |ret| try self.rewriteCallsWithValuesInExpr(ret.value),
            .uninitialized => |pat| try self.shadowPatLocals(pat),
            .crash => {},
        }
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
            try self.putSubst(source_arg.local, value);
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
                const payloads = try self.pass.arena.allocator().alloc(Value, tag.payloads.len);
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
                const fields = try self.pass.arena.allocator().alloc(FieldValue, record.fields.len);
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
                const items = try self.pass.arena.allocator().alloc(Value, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = try self.valueFromShapeArgs(item, args);
                }
                return .{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| {
                const backing = try self.pass.arena.allocator().create(Value);
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
                const captures = try self.pass.arena.allocator().alloc(CaptureValue, callable.captures.len);
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
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;

        // Region boundary: pending bindings created below this expression are
        // emitted here, where they dominate every leaf reference inside it.
        const pending_start = self.pending.items.len;
        const saved_entry_marks = self.region_entry_marks;
        self.region_entry_marks = self.effect_marks;
        defer self.region_entry_marks = saved_entry_marks;
        const result = try self.materialize(try self.cloneExprValue(expr_id));
        return try self.flushPendingSince(pending_start, result);
    }

    fn cloneExprValue(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;

        const expr = self.pass.program.getExpr(expr_id);
        switch (expr.data) {
            .local => |local| {
                if (self.subst.get(local)) |value| return value;
                if (self.binderIdentityOf(local)) |identity| {
                    if (self.binder_subst.get(identity)) |value| return value;
                }
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } }) };
            },
            .fn_ref => |fn_ref| return try self.callableValueFromRef(expr.ty, fn_ref),
            .static_data_candidate => |candidate| {
                const runtime = try self.pass.arena.allocator().create(Value);
                runtime.* = try self.cloneExprValueDemandingShape(candidate.runtime_expr);
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
                const payloads = try self.pass.arena.allocator().alloc(Value, payload_exprs.len);
                for (payload_exprs, 0..) |payload, index| {
                    payloads[index] = try self.cloneExprValueDemandingShape(payload);
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
                const fields = try self.pass.arena.allocator().alloc(FieldValue, source_fields.len);
                for (source_fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.cloneExprValueDemandingShape(field.value),
                    };
                }
                return .{ .record = .{
                    .ty = expr.ty,
                    .fields = fields,
                } };
            },
            .tuple => |items_span| {
                assertStructuralConstructionType(self.pass.program, expr.ty);
                const source_items = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(items_span));
                defer self.pass.allocator.free(source_items);
                const items = try self.pass.arena.allocator().alloc(Value, source_items.len);
                for (source_items, 0..) |item, index| {
                    items[index] = try self.cloneExprValueDemandingShape(item);
                }
                return .{ .tuple = .{
                    .ty = expr.ty,
                    .items = items,
                } };
            },
            .nominal => |backing| {
                const backing_value = try self.cloneExprValueDemandingShape(backing);
                return .{ .nominal = .{
                    .ty = expr.ty,
                    .backing = try self.copyValue(backing_value),
                } };
            },
            .let_ => |let_| return try self.cloneLetValue(let_),
            .loop_ => |loop| return try self.cloneLoopValue(expr.ty, loop),
            .block => |block| {
                if (try self.cloneBlockValue(block)) |value| return value;
                return .{ .expr = try self.cloneExprPlain(expr_id) };
            },
            .field_access => |field| {
                const receiver = try self.cloneExprValueDemandingShape(field.receiver);
                if (fieldFromValue(self.pass.program, receiver, field.field)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .field_access = .{
                    .receiver = try self.materialize(receiver),
                    .field = field.field,
                } } }) };
            },
            .tuple_access => |access| {
                const receiver = try self.cloneExprValueDemandingShape(access.tuple);
                if (itemFromValue(receiver, access.elem_index)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = try self.materialize(receiver),
                    .elem_index = access.elem_index,
                } } }) };
            },
            .match_ => |match| {
                const scrutinee = try self.cloneExprValueDemandingShape(match.scrutinee);
                if (try self.simplifyKnownMatchValue(scrutinee, match.branches)) |value| return value;
                const scrutinee_expr = try self.materialize(scrutinee);
                if (try self.cloneCaseOfCaseValue(expr.ty, scrutinee_expr, match.branches)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .match_ = .{
                    .scrutinee = scrutinee_expr,
                    .branches = try self.cloneBranchSpan(match.branches),
                    .comptime_site = match.comptime_site,
                } } }) };
            },
            .call_value => |call| {
                const callee = try self.cloneExprValueDemandingShape(call.callee);
                if (callee == .callable) {
                    return try self.inlineCallableCallValue(expr.ty, callee.callable, call.args);
                }
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(callee),
                    .args = try self.cloneExprSpan(call.args),
                } } }) };
            },
            .call_proc => |call| {
                if (call.is_cold) return .{ .expr = try self.cloneExprPlain(expr_id) };
                if (!self.inline_direct_calls) return .{ .expr = try self.cloneExprPlain(expr_id) };
                const callee = Ast.localDirectCallee(call) orelse return .{ .expr = try self.cloneExprPlain(expr_id) };
                const callee_raw = @intFromEnum(callee);
                if (callee_raw < self.pass.fn_may_crash.len and self.pass.fn_may_crash[callee_raw]) {
                    return .{ .expr = try self.cloneExprPlain(expr_id) };
                }
                const has_known_shape_arg = try self.directCallHasKnownShapeArg(call.args);
                // A direct call carries its callee's captures by the callee's
                // own capture locals: the residual call imports those locals
                // into the enclosing function. In a context where a capture
                // operand has been substituted away from the callee's local,
                // that import would name a local the context does not have,
                // so the call cannot stay residual and must inline.
                const captures_foreign = self.callCapturesAreForeign(call.captures);
                if (self.inline_direct_requires_known_arg and !has_known_shape_arg and !captures_foreign) {
                    return .{ .expr = try self.cloneExprPlain(expr_id) };
                }
                return try self.inlineDirectCallValue(
                    callee,
                    call.args,
                    call.captures,
                    expr_id,
                );
            },
            else => return .{ .expr = try self.cloneExprPlain(expr_id) },
        }
    }

    fn cloneExprValueDemandingShape(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        const expr = self.pass.program.getExpr(expr_id);
        return switch (expr.data) {
            .call_proc => |call| blk: {
                if (call.is_cold or !self.inline_direct_calls) break :blk try self.cloneExprValue(expr_id);
                const callee = Ast.localDirectCallee(call) orelse break :blk try self.cloneExprValue(expr_id);
                const raw = @intFromEnum(callee);
                if (raw < self.pass.fn_may_crash.len and self.pass.fn_may_crash[raw]) {
                    break :blk try self.cloneExprValue(expr_id);
                }
                break :blk try self.inlineDirectCallValue(callee, call.args, call.captures, expr_id);
            },
            .block => |block| if (self.pass.program.stmtSpan(block.statements).len == 0)
                try self.cloneExprValueDemandingShape(block.final_expr)
            else
                try self.cloneExprValue(expr_id),
            .comptime_branch_taken => |taken| try self.cloneExprValueDemandingShape(taken.body),
            else => try self.cloneExprValue(expr_id),
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
    /// other than the callee's own capture local — i.e. the call sits in a
    /// context where the captured bindings have been substituted.
    fn callCapturesAreForeign(self: *Cloner, captures_span: Ast.Span(Ast.CaptureOperand)) bool {
        const operands = self.pass.program.captureOperandSpan(captures_span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            const local = localExpr(self.pass.program, operand.value) orelse return true;
            if (self.subst.contains(local)) return true;
            if (self.binderIdentityOf(local)) |identity| {
                if (self.binder_subst.contains(identity)) return true;
            }
        }
        return false;
    }

    fn exprHasKnownShape(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!bool {
        const expr = self.pass.program.getExpr(expr_id);
        return switch (expr.data) {
            .local => |local| if (self.subst.get(local)) |value|
                (try self.pass.shapeFromValue(value)) != null
            else
                false,
            .tag,
            .record,
            .tuple,
            .nominal,
            .fn_ref,
            => (try self.pass.constructorShape(expr_id)) != null,
            .list, .str_lit, .bytes_lit => self.inline_list_source_construction,
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk false;
                const receiver = self.subst.get(receiver_local) orelse break :blk false;
                const value = fieldFromValue(self.pass.program, receiver, field.field) orelse break :blk false;
                break :blk (try self.pass.shapeFromValue(value)) != null;
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk false;
                const tuple = self.subst.get(tuple_local) orelse break :blk false;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk false;
                break :blk (try self.pass.shapeFromValue(value)) != null;
            },
            .static_data_candidate => |candidate| try self.exprHasKnownShape(candidate.runtime_expr),
            .comptime_branch_taken => |taken| try self.exprHasKnownShape(taken.body),
            .comptime_exhaustiveness_failed => false,
            else => false,
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
    /// therefore spent per NODE VISIT — one shared counter across the whole
    /// walk — which bounds total work absolutely for cycles and shared
    /// structure alike. See design.md "Core Principles" on bounded post-check
    /// walks.
    ///
    /// A work budget is the right bound here, rather than a visited set,
    /// because this predicate is allowed to answer "no" spuriously: declining
    /// a substitution keeps the construction materialized, which is a missed
    /// optimization and never a miscompile. A cyclic value exhausts the
    /// budget and gets "no" — the correct answer, since a self-referential
    /// value cannot be substituted anyway — and a value large enough to
    /// exhaust it honestly is one whose substitution would bloat the clone
    /// regardless. Value identity is also too murky for a reliable visited
    /// set: values are by-value unions holding slices, with only the nominal
    /// backing behind a stable pointer.
    const value_substitute_work_budget: u32 = 4096;

    fn valueCanSubstitute(self: *Cloner, value: Value) bool {
        var budget: u32 = value_substitute_work_budget;
        return self.valueCanSubstituteBudgeted(value, &budget);
    }

    fn valueCanSubstituteBudgeted(self: *Cloner, value: Value, budget: *u32) bool {
        if (budget.* == 0) return false;
        budget.* -= 1;
        return switch (value) {
            .expr => |expr| self.exprCanSubstitute(expr),
            .static_data_candidate => |candidate| self.valueCanSubstituteBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                for (tag.payloads) |payload| {
                    if (!self.valueCanSubstituteBudgeted(payload, budget)) break :blk false;
                }
                break :blk true;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (!self.valueCanSubstituteBudgeted(field.value, budget)) break :blk false;
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (tuple.items) |item| {
                    if (!self.valueCanSubstituteBudgeted(item, budget)) break :blk false;
                }
                break :blk true;
            },
            .nominal => |nominal| self.valueCanSubstituteBudgeted(nominal.backing.*, budget),
            .callable => |callable| blk: {
                for (callable.captures) |capture| {
                    if (!self.valueCanSubstituteBudgeted(capture.value, budget)) break :blk false;
                }
                break :blk true;
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
            else => false,
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

    fn callableValueFromRef(self: *Cloner, ty: Type.TypeId, fn_ref: @import("../monotype/ast.zig").LiftedFunctionValue) Common.LowerError!Value {
        const capture_count: usize = @intCast(fn_ref.captures.len);
        const captures = try self.pass.arena.allocator().alloc(CaptureValue, capture_count);
        for (0..capture_count) |index| {
            const operand = self.pass.program.captureOperandAt(fn_ref.captures, index);
            captures[index] = .{ .id = operand.id, .value = try self.cloneExprValue(operand.value) };
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
        const expr_loc = self.pass.program.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.current_loc = expr_loc;
        const expr_region = self.pass.program.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.current_region = expr_region;

        const expr = self.pass.program.getExpr(expr_id);
        const data: Ast.ExprData = switch (expr.data) {
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
            .call_proc => |call| try self.cloneCallProc(call),
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
            .loop_ => |loop| return try self.materialize(try self.cloneLoopValue(expr.ty, loop)),
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.cloneExpr(value) else null },
            .continue_ => |continue_| try self.cloneContinue(continue_),
            .join_point => |join_point| return try self.cloneJoinPoint(expr.ty, join_point),
            .jump => |jump| blk: {
                if (self.letCaseJoinFor(jump.target)) |join| {
                    return try self.captureLetCaseJump(expr.ty, join, jump);
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
                const shadow_start = self.changes.items.len;
                const ok_ty = self.pass.program.getLocal(sequence.ok_local).ty;
                const ok_local = try self.cloneBinder(sequence.ok_local, ok_ty, .bind_runtime);
                const ok_body = try self.cloneExpr(sequence.ok_body);
                self.restore(shadow_start);
                break :blk .{ .try_sequence = .{
                    .try_expr = try_expr,
                    .ok_local = ok_local,
                    .err_is_cold = sequence.err_is_cold,
                    .ok_body = ok_body,
                } };
            },
            .try_record_sequence => |sequence| blk: {
                const try_expr = try self.cloneExpr(sequence.try_expr);
                const shadow_start = self.changes.items.len;
                const value_ty = self.pass.program.getLocal(sequence.value_local).ty;
                const value_local = try self.cloneBinder(sequence.value_local, value_ty, .bind_runtime);
                const rest_ty = self.pass.program.getLocal(sequence.rest_local).ty;
                const rest_local = try self.cloneBinder(sequence.rest_local, rest_ty, .bind_runtime);
                const ok_body = try self.cloneExpr(sequence.ok_body);
                self.restore(shadow_start);
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

        const change_start = self.changes.items.len;
        for (source_params, params) |source_param, param| {
            const local_expr = try self.addExpr(.{ .ty = param.ty, .data = .{ .local = param.local } });
            try self.putSubst(source_param.local, .{ .expr = local_expr });
        }
        const body = try self.cloneExpr(join_point.body);
        // The remainder's jumps may forward-reference the join's own params:
        // an `uninitialized_payload` argument names the flag param carrying
        // its initialized-ness. Keep the param substitutions active so those
        // references follow the freshened params.
        const remainder = try self.cloneExpr(join_point.remainder);
        self.restore(change_start);

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
        Common.invariant("SpecConstr jump referenced a join point outside its lexical scope");
    }

    fn cloneLetValue(self: *Cloner, let_: anytype) Common.LowerError!Value {
        const value = try self.cloneExprValue(let_.value);
        const value_expr = try self.materialize(value);
        if (self.caseExprFromValue(value)) |case_expr| {
            if (try self.cloneLetOfCase(let_, case_expr)) |data| {
                const rest_ty = self.pass.program.getExpr(let_.rest).ty;
                return .{ .expr = try self.addExpr(.{ .ty = rest_ty, .data = data }) };
            }
        }
        const change_start = self.changes.items.len;
        const bound = try self.bindPatToReusableValue(let_.bind, value);
        if (bound == .match) {
            const rest = try self.cloneExprValue(let_.rest);
            self.restore(change_start);
            return rest;
        }
        self.restore(change_start);
        if (try self.bindPatToSingleUseRestValue(let_.bind, value, let_.rest)) {
            const rest = try self.cloneExprValue(let_.rest);
            self.restore(change_start);
            return rest;
        }
        if (try self.bindPatToPendingReusableValue(let_.bind, let_.value, false, value)) {
            const rest = try self.cloneExprValue(let_.rest);
            self.restore(change_start);
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
        {
            const pat = self.pass.program.getPat(let_.bind);
            const self_referential = switch (pat.data) {
                .bind => |local| localUseCountInExpr(self.pass.program, local, let_.value) != 0,
                else => false,
            };
            if (!self_referential) {
                const pending_before = self.pending.items.len;
                const reusable = try self.makeReusableForMatch(value);
                if (try self.bindPatToFlowValue(let_.bind, reusable)) {
                    const rest = try self.materialize(try self.cloneExprValue(let_.rest));
                    self.restore(change_start);
                    return .{ .expr = try self.flushPendingSince(pending_before, rest) };
                }
                self.restore(change_start);
                self.pending.shrinkRetainingCapacity(pending_before);
            }
        }
        const bind = try self.clonePat(let_.bind, .bind_runtime);
        const rest = try self.cloneExpr(let_.rest);
        self.restore(change_start);
        return .{ .expr = try self.addExpr(.{ .ty = self.pass.program.getExpr(let_.rest).ty, .data = .{ .let_ = .{
            .bind = bind,
            .value = value_expr,
            .rest = rest,
            .comptime_site = let_.comptime_site,
        } } }) };
    }

    /// Dissolve a binding by naming its value's opaque leaves as pending
    /// bindings: the value keeps its structure, uses substitute leaf
    /// references, and the pending bindings are emitted where the stack next
    /// flushes, still dominating every use. Sound only when every named leaf
    /// is an effect-free computation created before any effect in its region,
    /// and the value does not reference its own binder. Returns false with
    /// all speculative work undone.
    fn bindPatToPendingReusableValue(
        self: *Cloner,
        pat_id: Ast.PatId,
        source_value: Ast.ExprId,
        recursive: bool,
        value: Value,
    ) Common.LowerError!bool {
        const pat = self.pass.program.getPat(pat_id);
        const self_referential = switch (pat.data) {
            .bind => |local| localUseCountInExpr(self.pass.program, local, source_value) != 0,
            else => recursive,
        };
        if (self_referential) return false;
        if (self.effect_marks != self.region_entry_marks) return false;

        const pending_before = self.pending.items.len;
        const change_before = self.changes.items.len;
        const reusable = try self.makeReusableForMatch(value);
        for (self.pending.items[pending_before..]) |pend| {
            if (!exprHasNoObservableEffect(self.pass.program, self.pass.fn_effect_free, pend.value, false)) {
                self.restore(change_before);
                self.pending.shrinkRetainingCapacity(pending_before);
                return false;
            }
        }
        if (try self.bindPatToReusableValue(pat_id, reusable) != .match) {
            self.restore(change_before);
            self.pending.shrinkRetainingCapacity(pending_before);
            return false;
        }
        return true;
    }

    fn cloneLet(self: *Cloner, let_: anytype) Common.LowerError!Ast.ExprData {
        const value = try self.cloneExprValue(let_.value);
        const value_expr = try self.materialize(value);
        const change_start = self.changes.items.len;
        const bound = try self.bindPatToReusableValue(let_.bind, value);
        var bind: Ast.PatId = undefined;
        const rest = if (bound == .match) blk: {
            const cloned = try self.cloneExpr(let_.rest);
            self.restore(change_start);
            bind = try self.clonePat(let_.bind, .output_only);
            break :blk cloned;
        } else if (try self.bindPatToSingleUseRestValue(let_.bind, value, let_.rest)) blk: {
            const cloned = try self.cloneExpr(let_.rest);
            self.restore(change_start);
            bind = try self.clonePat(let_.bind, .output_only);
            break :blk cloned;
        } else blk: {
            self.restore(change_start);
            if (self.caseExprFromValue(value)) |case_expr| {
                if (try self.cloneLetOfCase(let_, case_expr)) |data| return data;
            }
            bind = try self.clonePat(let_.bind, .bind_runtime);
            const rest = try self.cloneExpr(let_.rest);
            self.restore(change_start);
            break :blk rest;
        };
        return .{ .let_ = .{
            .bind = bind,
            .value = value_expr,
            .rest = rest,
            .comptime_site = let_.comptime_site,
        } };
    }

    fn bindPatToSingleUseRestValue(self: *Cloner, pat_id: Ast.PatId, value: Value, rest: Ast.ExprId) Common.LowerError!bool {
        const local = switch (self.pass.program.getPat(pat_id).data) {
            .bind => |local| local,
            else => return false,
        };
        const unsafe_count = self.unsafeLeafCount(value);
        const uses = localUseCountInExpr(self.pass.program, local, rest);
        const before_effect = localUseBeforeEffect(self.pass.program, local, rest);
        if (unsafe_count != 1 or uses != 1 or !before_effect) {
            return false;
        }
        try self.putSubst(local, value);
        return true;
    }

    fn caseExprFromValue(self: *Cloner, value: Value) ?Ast.ExprId {
        const candidate = switch (value) {
            .expr => |expr| expr,
            .static_data_candidate => |static_candidate| switch (static_candidate.runtime.*) {
                .expr => |runtime| runtime,
                else => return null,
            },
            else => return null,
        };
        return switch (self.pass.program.getExpr(candidate).data) {
            .if_, .match_ => candidate,
            else => null,
        };
    }

    /// Rewrite `let bind = <match/if> in rest` so every arm transfers its
    /// result to shared continuation code through a join point, without
    /// cloning that continuation into the arms and without losing the arms'
    /// statically known value structure:
    ///
    /// - Each arm's result value must be a known structure (constructor,
    ///   record, tuple, callable). An opaque arm result gains nothing from
    ///   the rewrite and would only push the continuation behind a join —
    ///   defeating downstream tail-call and loop-shape recognition — so the
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
        switch (value_data) {
            .match_, .if_ => {},
            else => return null,
        }

        const arm_count: usize = switch (value_data) {
            .match_ => |match| self.pass.program.branchSpan(match.branches).len,
            .if_ => |if_| self.pass.program.ifBranchSpan(if_.branches).len + 1,
            else => unreachable,
        };
        if (self.let_case_shape_arms_remaining < arm_count) {
            return try self.cloneLetOfCaseShared(let_, value_expr);
        }
        self.let_case_shape_arms_remaining -= arm_count;

        const arena = self.pass.arena.allocator();
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
                    const change_start = self.changes.items.len;
                    try self.shadowPatLocals(branch.pat);
                    const body = (try self.cloneLetOfCaseArmBody(probe, dispatch, branch.body)) orelse {
                        self.restore(change_start);
                        return null;
                    };
                    self.restore(change_start);
                    rewritten[index] = .{
                        .pat = branch.pat,
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
            else => unreachable,
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

    const let_case_shape_arm_budget: usize = 256;

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

        const change_start = self.changes.items.len;
        const pending_start = self.pending.items.len;
        const bind = try self.clonePat(let_.bind, .bind_runtime);
        const rest = try self.flushPendingSince(pending_start, try self.cloneExpr(let_.rest));
        self.restore(change_start);
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
            else => unreachable,
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
            const bind_local = switch (self.pass.program.getPat(let_.bind).data) {
                .bind => |local| local,
                else => break :dispatch_split,
            };
            const rest_match = switch (self.pass.program.getExpr(let_.rest).data) {
                .match_ => |match| match,
                else => break :dispatch_split,
            };
            const scrutinee_local = localExpr(self.pass.program, rest_match.scrutinee) orelse break :dispatch_split;
            if (scrutinee_local != bind_local) break :dispatch_split;
            if (localUseCountInExpr(self.pass.program, bind_local, let_.rest) != 1) break :dispatch_split;

            const branches = self.pass.program.branchSpan(rest_match.branches);
            const joins = try arena.alloc(LetCaseJoin, branches.len);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (branch.guard != null) break :dispatch_split;
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

    /// Clone one arm of the rewritten case. The arm keeps its own statements
    /// and effects; its result value must be a known structure, which the
    /// cloned dispatch consumes. Returns null when the arm's value is opaque,
    /// declining the whole rewrite.
    fn cloneLetOfCaseArmBody(self: *Cloner, probe: Ast.LocalId, dispatch: Ast.ExprId, branch_body: Ast.ExprId) Common.LowerError!?Ast.ExprId {
        // The rewritten arm flushes every pending binding it creates, so it
        // is its own region.
        const saved_entry_marks = self.region_entry_marks;
        self.region_entry_marks = self.effect_marks;
        defer self.region_entry_marks = saved_entry_marks;

        const dispatch_ty = self.pass.program.getExpr(dispatch).ty;
        const branch_expr = self.pass.program.getExpr(branch_body);
        switch (branch_expr.data) {
            .block => |block| {
                const change_start = self.changes.items.len;
                const pending_entry = self.pending.items.len;

                const source = try GuardedList.dupe(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(block.statements));
                defer self.pass.allocator.free(source);

                var statements = std.ArrayList(Ast.StmtId).empty;
                defer statements.deinit(self.pass.allocator);
                for (source) |stmt| {
                    const pending_start = self.pending.items.len;
                    const cloned = try self.cloneStmt(stmt);
                    try self.appendPendingStmtsSince(pending_start, &statements);
                    if (cloned) |cloned_stmt| try statements.append(self.pass.allocator, cloned_stmt);
                }

                const pending_final = self.pending.items.len;
                const final_value = try self.cloneExprValue(block.final_expr);
                if (final_value == .expr) {
                    if (try self.cloneDivergentAtType(block.final_expr, dispatch_ty)) |divergent| {
                        self.restore(change_start);
                        try self.appendPendingStmtsSince(pending_final, &statements);
                        return try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                            .statements = try self.pass.program.addStmtSpan(statements.items),
                            .final_expr = divergent,
                        } } });
                    }
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_entry);
                    return null;
                }

                try self.putSubst(probe, final_value);
                try self.appendPendingStmtsSince(pending_final, &statements);
                const rest = try self.cloneExpr(dispatch);
                self.restore(change_start);

                return try self.addExpr(.{ .ty = dispatch_ty, .data = .{ .block = .{
                    .statements = try self.pass.program.addStmtSpan(statements.items),
                    .final_expr = rest,
                } } });
            },
            else => {
                const pending_entry = self.pending.items.len;
                const branch_value = try self.cloneExprValue(branch_body);
                const change_start = self.changes.items.len;
                if (branch_value == .expr) {
                    if (try self.cloneDivergentAtType(branch_body, dispatch_ty)) |divergent| {
                        self.restore(change_start);
                        return try self.flushPendingSince(pending_entry, divergent);
                    }
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_entry);
                    return null;
                }
                try self.putSubst(probe, branch_value);
                const rest = try self.flushPendingSince(pending_entry, try self.cloneExpr(dispatch));
                self.restore(change_start);
                return rest;
            },
        }
    }

    fn cloneDivergentAtType(self: *Cloner, expr_id: Ast.ExprId, ty: Type.TypeId) Common.LowerError!?Ast.ExprId {
        const expr = self.pass.program.getExpr(expr_id);
        return switch (expr.data) {
            .crash => |msg| try self.addExpr(.{ .ty = ty, .data = .{ .crash = msg } }),
            .comptime_exhaustiveness_failed => |site| try self.addExpr(.{ .ty = ty, .data = .{ .comptime_exhaustiveness_failed = site } }),
            .return_ => |ret| try self.addExpr(.{ .ty = ty, .data = .{ .return_ = .{
                .value = try self.cloneExpr(ret.value),
                .target = ret.target,
            } } }),
            else => null,
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
        const arena = self.pass.arena.allocator();
        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(jump.args));
        defer self.pass.allocator.free(args);
        const values = try arena.alloc(Value, args.len);
        for (args, 0..) |arg, index| {
            values[index] = try self.cloneExprValueDemandingShape(arg);
        }
        const placeholder = try self.addExpr(.{ .ty = ty, .data = .{ .jump = .{
            .target = join.id,
            .args = try self.pass.program.addExprSpan(&[_]Ast.ExprId{}),
        } } });
        try join.sites.append(arena, .{ .expr = placeholder, .values = values });
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
        const saved_entry_marks = self.region_entry_marks;
        self.region_entry_marks = self.effect_marks;
        defer self.region_entry_marks = saved_entry_marks;

        const change_start = self.changes.items.len;
        const body = body: switch (join.binding) {
            .locals => |locals| {
                if (site.values.len != locals.len) {
                    Common.invariant("let-of-case jump site argument count differed from join binder count");
                }
                for (locals, site.values) |local, value| try self.putSubst(local, value);
                const body = try self.cloneExpr(join.body);
                self.restore(change_start);
                break :body body;
            },
            .pattern => |binding| {
                if (try self.bindPatToFlowValue(binding.pat, site.values[0])) {
                    const body = try self.cloneExpr(join.body);
                    self.restore(change_start);
                    break :body body;
                }
                // The pattern could not consume the value's structure; keep
                // an ordinary let of the materialized value at the site.
                self.restore(change_start);
                const value_expr = try self.materialize(site.values[0]);
                const pat_change_start = self.changes.items.len;
                const bind = try self.clonePat(binding.pat, .bind_runtime);
                const rest = try self.cloneExpr(join.body);
                self.restore(pat_change_start);
                break :body try self.addExpr(.{ .ty = rest_ty, .data = .{ .let_ = .{
                    .bind = bind,
                    .value = value_expr,
                    .rest = rest,
                    .comptime_site = binding.comptime_site,
                } } });
            },
        };
        self.pass.program.setExprData(site.expr, self.pass.program.getExpr(body).data);
    }

    /// Decompose a join's incoming values into shared parameters, clone the
    /// join's continuation body once against the rebuilt values, and patch
    /// every jump site with its leaf arguments. A join with exactly one jump
    /// site stores no continuation copy either way, so its body is cloned
    /// directly at the site — against the site's full symbolic values — and
    /// no join point is emitted (null).
    fn finalizeLetCaseJoin(self: *Cloner, join: *LetCaseJoin, rest_ty: Type.TypeId) Common.LowerError!?LetCaseJoinPieces {
        const arena = self.pass.arena.allocator();
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
        var budget: u32 = let_case_join_leaf_budget;
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

        const change_start = self.changes.items.len;
        const saved_entry_marks = self.region_entry_marks;
        self.region_entry_marks = self.effect_marks;
        defer self.region_entry_marks = saved_entry_marks;
        const body = body: switch (join.binding) {
            .locals => |locals| {
                for (locals, rebuilt) |local, value| try self.putSubst(local, value);
                const body = try self.cloneExpr(join.body);
                self.restore(change_start);
                break :body body;
            },
            .pattern => |binding| {
                if (try self.bindPatToFlowValue(binding.pat, rebuilt[0])) {
                    const body = try self.cloneExpr(join.body);
                    self.restore(change_start);
                    break :body body;
                }
                // The pattern could not consume the rebuilt structure; fall
                // back to one opaque parameter bound by an ordinary let.
                self.restore(change_start);
                params.clearRetainingCapacity();
                for (site_args) |*list| list.clearRetainingCapacity();
                const param_ty = valueType(self.pass.program, sites[0].values[0]);
                const param_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), param_ty);
                try params.append(arena, .{ .local = param_local, .ty = param_ty });
                for (sites, site_args) |site, *list| {
                    try list.append(arena, try self.materialize(site.values[0]));
                }
                const param_expr = try self.addExpr(.{ .ty = param_ty, .data = .{ .local = param_local } });
                const pat_change_start = self.changes.items.len;
                const bind = try self.clonePat(binding.pat, .bind_runtime);
                const rest = try self.cloneExpr(join.body);
                self.restore(pat_change_start);
                break :body try self.addExpr(.{ .ty = rest_ty, .data = .{ .let_ = .{
                    .bind = bind,
                    .value = param_expr,
                    .rest = rest,
                    .comptime_site = binding.comptime_site,
                } } });
            },
        };

        for (sites, site_args) |site, list| {
            self.pass.program.setExprData(site.expr, .{ .jump = .{
                .target = join.id,
                .args = try self.pass.program.addExprSpan(list.items),
            } });
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
        budget: *u32,
    ) Common.LowerError!Value {
        if (values.len == 0) Common.invariant("let-of-case join had no jump sites to decompose");
        structured: {
            if (budget.* == 0 or params.items.len >= let_case_join_param_cap) break :structured;
            budget.* -= 1;
            switch (values[0]) {
                .expr, .static_data_candidate => break :structured,
                .tag => |first| {
                    for (values[1..]) |other| {
                        const other_tag = switch (other) {
                            .tag => |tag| tag,
                            else => break :structured,
                        };
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
                        const other_record = switch (other) {
                            .record => |record| record,
                            else => break :structured,
                        };
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
                        const other_tuple = switch (other) {
                            .tuple => |tuple| tuple,
                            else => break :structured,
                        };
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
                        const other_nominal = switch (other) {
                            .nominal => |nominal| nominal,
                            else => break :structured,
                        };
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
                    for (values[1..]) |other| {
                        const other_callable = switch (other) {
                            .callable => |callable| callable,
                            else => break :structured,
                        };
                        if (other_callable.ty != first.ty) break :structured;
                        if (other_callable.fn_id != first.fn_id) break :structured;
                        if (other_callable.captures.len != first.captures.len) break :structured;
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
                    return .{ .callable = .{ .ty = first.ty, .fn_id = first.fn_id, .captures = captures } };
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

    fn cloneLoopValue(self: *Cloner, ty: Type.TypeId, loop: anytype) Common.LowerError!Value {
        const params = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(loop.params));
        defer self.pass.allocator.free(params);
        const initial_values = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(loop.initial_values));
        defer self.pass.allocator.free(initial_values);
        if (params.len != initial_values.len) Common.invariant("loop parameter count differed from initial value count");

        const values = try self.pass.allocator.alloc(Value, initial_values.len);
        defer self.pass.allocator.free(values);
        const shapes = try self.pass.arena.allocator().alloc(Shape, initial_values.len);
        var has_constructor = false;
        // A loop-carried value that begins as an iterator construction only
        // reveals its constructor shape after that construction inlines. An
        // adapter constructor (e.g. `List.iter(list)`, `Iter.map(inner, f)`)
        // returns a record whose leaves the split threads as scalars, but its
        // arguments (the source list, the inner iterator) need not themselves
        // be known shapes. So expose the initial value's constructor by
        // inlining its construction call regardless of argument shape; the
        // per-argument known-shape gate governs only residual body calls.
        const saved_requires_known_arg = self.inline_direct_requires_known_arg;
        if (self.force_loop_initial_inline) self.inline_direct_requires_known_arg = false;
        // An initial value may forward-reference the loop's own params: an
        // `uninitialized_payload` argument names the flag param that carries
        // its initialized-ness at the loop head. The emitted params do not
        // exist yet, so pin those references to the source param ids while
        // cloning; each emission below retargets them to its fresh params.
        const forward_start = self.changes.items.len;
        for (params) |param| try self.shadowLocal(param.local);
        for (initial_values, 0..) |initial, index| {
            values[index] = try self.cloneExprValueDemandingShape(initial);
            if (try self.pass.shapeFromValue(values[index])) |shape| {
                shapes[index] = shape;
                has_constructor = true;
            } else {
                shapes[index] = .{ .any = valueType(self.pass.program, values[index]) };
            }
        }
        self.restore(forward_start);
        self.inline_direct_requires_known_arg = saved_requires_known_arg;

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        // A loop-carried variable that was bound to a known constructor before the
        // loop leaves that value in `binder_subst`, keyed on its source binder.
        // Every back edge reassigns the variable, so its pre-loop value is not
        // what the slot carries inside the loop. Reads sharing that binder (the
        // reassigned copies feeding `continue`) must resolve to the value the slot
        // actually holds, so drop those pre-loop values before cloning the body.
        for (initial_values) |initial| try self.dropCarriedBinderValue(initial);

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
        while (has_constructor) {
            var new_params = std.ArrayList(Ast.TypedLocal).empty;
            defer new_params.deinit(self.pass.allocator);

            var new_initials = std.ArrayList(Ast.ExprId).empty;
            defer new_initials.deinit(self.pass.allocator);

            const split_start = self.changes.items.len;
            var forward_sources = std.ArrayList(Ast.LocalId).empty;
            defer forward_sources.deinit(self.pass.allocator);
            var forward_finals = std.ArrayList(Ast.LocalId).empty;
            defer forward_finals.deinit(self.pass.allocator);
            for (params, shapes, values) |param, shape, value| {
                const leaf_start = new_params.items.len;
                const param_value = try self.valueFromShapeArgs(shape, &new_params);
                try self.putSubst(param.local, param_value);
                try self.appendExprsFromValue(shape, value, &new_initials);
                switch (shape) {
                    // An `.any` slot keeps its whole value in one param, so a
                    // forward reference to the source param means this param.
                    .any => {
                        try forward_sources.append(self.pass.allocator, param.local);
                        try forward_finals.append(self.pass.allocator, new_params.items[leaf_start].local);
                    },
                    else => {},
                }
            }

            try self.loop_stack.append(self.pass.allocator, .{ .values = shapes, .any_demoted = false });
            const body = try self.cloneExpr(loop.body);
            const frame = self.loop_stack.pop() orelse Common.invariant("loop stack underflow after split attempt");

            if (!frame.any_demoted) {
                try self.retargetLoopForwardConditions(new_initials.items, forward_sources.items, forward_finals.items);
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
                    .params = try self.pass.program.addTypedLocalSpan(new_params.items),
                    .initial_values = try self.pass.program.addExprSpan(new_initials.items),
                    .body = body,
                } } }) };
            }

            self.restore(split_start);
            // Back edges demoted their unsupplied leaves in place. Any slot that
            // still carries constructor structure is worth another split attempt.
            has_constructor = false;
            for (shapes) |shape| switch (shape) {
                .any => {},
                else => has_constructor = true,
            };
        }

        const whole_shapes = try self.pass.arena.allocator().alloc(Shape, params.len);
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
        for (params, whole_params, forward_sources, forward_finals) |param, *whole, *source, *final| {
            whole.* = .{
                .local = try self.cloneBinder(param.local, param.ty, .bind_runtime),
                .ty = param.ty,
            };
            source.* = param.local;
            final.* = whole.local;
        }
        try self.retargetLoopForwardConditions(initial_exprs, forward_sources, forward_finals);
        try self.loop_stack.append(self.pass.allocator, .{ .values = whole_shapes, .any_demoted = false });
        const body = try self.cloneExpr(loop.body);
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
            const payload = switch (expr.data) {
                .uninitialized_payload => |payload| payload,
                else => continue,
            };
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

    /// Remove the pre-loop `binder_subst` value for the variable carried by a
    /// loop slot whose initial value is that variable. The removal is recorded on
    /// the change log so it is restored when the loop clone finishes.
    fn dropCarriedBinderValue(self: *Cloner, initial: Ast.ExprId) Allocator.Error!void {
        const local = localExpr(self.pass.program, initial) orelse return;
        const identity = self.binderIdentityOf(local) orelse return;
        const previous = self.binder_subst.get(identity) orelse return;
        try self.changes.append(self.pass.allocator, .{
            .key = .{ .binder = identity },
            .previous = previous,
        });
        _ = self.binder_subst.remove(identity);
    }

    /// A block whose statements all dissolve — each binds a substitutable
    /// value, or names an effect-free computation that becomes a pending
    /// binding — is transparent to value flow: its result keeps the final
    /// expression's structure. A statement that must stay a statement (an
    /// effect, a runtime destructure, control flow) pins the block, which
    /// then materializes as written. Returns null on a pinned block with all
    /// speculative work undone.
    fn cloneBlockValue(self: *Cloner, block: anytype) Common.LowerError!?Value {
        const change_start = self.changes.items.len;
        const pending_entry = self.pending.items.len;

        const source = try GuardedList.dupe(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        for (source) |stmt_id| {
            const stmt = self.pass.program.getStmt(stmt_id);
            const let_ = switch (stmt) {
                .let_ => |let_| let_,
                // A discarded effect-free expression performs no observable
                // work, so the statement dissolves with the block.
                .expr => |stmt_expr| {
                    if (exprHasNoObservableEffect(self.pass.program, self.pass.fn_effect_free, stmt_expr, false)) continue;
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_entry);
                    return null;
                },
                else => {
                    self.restore(change_start);
                    self.pending.shrinkRetainingCapacity(pending_entry);
                    return null;
                },
            };
            const value = try self.cloneExprValue(let_.value);
            if (self.caseExprFromValue(value) != null) {
                self.restore(change_start);
                self.pending.shrinkRetainingCapacity(pending_entry);
                return null;
            }
            if (try self.bindPatToReusableValue(let_.pat, value) == .match) continue;
            if (!try self.bindPatToPendingReusableValue(let_.pat, let_.value, let_.recursive, value)) {
                self.restore(change_start);
                self.pending.shrinkRetainingCapacity(pending_entry);
                return null;
            }
        }

        const final = try self.cloneExprValue(block.final_expr);
        self.restore(change_start);
        return final;
    }

    fn cloneBlock(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Ast.ExprId {
        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const source = try GuardedList.dupe(self.pass.allocator, Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        var statements = std.ArrayList(Ast.StmtId).empty;
        defer statements.deinit(self.pass.allocator);
        for (source, 0..) |stmt, index| {
            // A binding statement is a let expression over the block's tail.
            // Cloning it as one lets a branch-built value sink the tail into
            // the branches, where each branch's constructor is known.
            switch (self.pass.program.getStmt(stmt)) {
                .let_ => |let_| if (!let_.recursive) {
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
                else => {},
            }
            const pending_start = self.pending.items.len;
            const cloned = try self.cloneStmt(stmt);
            try self.appendPendingStmtsSince(pending_start, &statements);
            if (cloned) |cloned_stmt| try statements.append(self.pass.allocator, cloned_stmt);
        }

        return try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(statements.items),
            .final_expr = try self.cloneExpr(block.final_expr),
        } } });
    }

    fn cloneContinue(self: *Cloner, continue_: anytype) Common.LowerError!Ast.ExprData {
        const frame_count = self.loop_stack.items.len;
        if (frame_count == 0) return .{ .continue_ = .{
            .values = try self.cloneExprSpan(continue_.values),
        } };
        const loop = self.loop_stack.items[frame_count - 1];
        const values = self.pass.program.exprSpan(continue_.values);
        const source_values = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, values);
        defer self.pass.allocator.free(source_values);
        if (source_values.len != loop.values.len) Common.invariant("continue value count differed from specialized loop pattern");

        var new_values = std.ArrayList(Ast.ExprId).empty;
        defer new_values.deinit(self.pass.allocator);

        for (loop.values, source_values, 0..) |shape, value_expr, slot_index| {
            const value = try self.cloneExprValue(value_expr);
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

        return .{ .continue_ = .{
            .values = try self.pass.program.addExprSpan(new_values.items),
        } };
    }

    fn cloneCallProc(self: *Cloner, call: @import("../monotype/ast.zig").CallProc) Common.LowerError!Ast.ExprData {
        if (call.is_cold) {
            return .{ .call_proc = .{
                .callee = call.callee,
                .args = try self.cloneExprSpan(call.args),
                .captures = try self.cloneCaptureOperandSpan(call.captures),
                .is_cold = true,
            } };
        }

        const callee = Ast.localDirectCallee(call) orelse return .{ .call_proc = .{
            .callee = call.callee,
            .args = try self.cloneExprSpan(call.args),
            .captures = try self.cloneCaptureOperandSpan(call.captures),
            .is_cold = call.is_cold,
        } };
        const raw = @intFromEnum(callee);
        if (self.rewrite_call_patterns and raw < self.pass.plans.len) {
            const source_args = self.pass.program.exprSpan(call.args);
            const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, source_args);
            defer self.pass.allocator.free(args);

            const values = try self.pass.allocator.alloc(Value, args.len);
            defer self.pass.allocator.free(values);
            const callee_uses = self.pass.plans[raw].used_args;
            const pending_before = self.pending.items.len;
            for (args, 0..) |arg, index| {
                values[index] = if (callee_uses[index])
                    try self.cloneExprValueDemandingShape(arg)
                else
                    try self.cloneExprValue(arg);
            }
            try self.pass.ensureCallPatternForValues(callee, values);

            for (self.pass.plans[raw].specs.items) |spec| {
                var rewritten_args = std.ArrayList(Ast.ExprId).empty;
                defer rewritten_args.deinit(self.pass.allocator);

                if (try self.appendClonedCallArgs(spec.pattern, args, &rewritten_args)) {
                    return .{ .call_proc = .{
                        .callee = .{ .lifted = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before cloning calls") },
                        .args = try self.pass.program.addExprSpan(rewritten_args.items),
                        .captures = try self.cloneCaptureOperandSpan(call.captures),
                        .is_cold = call.is_cold,
                    } };
                }
            }

            // No specialization matched, so the call stays residual. Reuse the
            // argument values already produced above instead of re-cloning the
            // source arguments: a second clone re-descends every argument, so a
            // nested call chain (e.g. a long `+` sum) would clone each level
            // twice and expand exponentially with depth. The reuse is exact when
            // producing the values created no pending bindings, since a plain
            // re-clone would then also create none and yield the same result.
            if (self.pending.items.len == pending_before) {
                const residual_args = try self.pass.allocator.alloc(Ast.ExprId, values.len);
                defer self.pass.allocator.free(residual_args);
                for (values, 0..) |value, index| {
                    residual_args[index] = try self.materialize(value);
                }
                return .{ .call_proc = .{
                    .callee = call.callee,
                    .args = try self.pass.program.addExprSpan(residual_args),
                    .captures = try self.cloneCaptureOperandSpan(call.captures),
                    .is_cold = call.is_cold,
                } };
            }
        }
        return .{ .call_proc = .{
            .callee = call.callee,
            .args = try self.cloneExprSpan(call.args),
            .captures = try self.cloneCaptureOperandSpan(call.captures),
            .is_cold = call.is_cold,
        } };
    }

    fn appendClonedCallArgs(
        self: *Cloner,
        pattern: CallPattern,
        args: []const Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        if (pattern.args.len != args.len) Common.invariant("call-pattern arity differed from direct call arity");
        for (pattern.args, args) |shape, arg| {
            if (!try self.appendClonedExprsForShape(shape, arg, out)) return false;
        }
        return true;
    }

    fn appendClonedExprsForShape(
        self: *Cloner,
        shape: Shape,
        expr_id: Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        switch (shape) {
            .any => {
                try out.append(self.pass.allocator, try self.cloneExpr(expr_id));
                return true;
            },
            else => {
                const value = try self.valueForCallArg(expr_id);
                if (!shapeMatchesValue(self.pass.program, shape, value)) return false;
                try self.appendExprsFromValue(shape, value, out);
                return true;
            },
        }
    }

    fn valueForCallArg(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        return try self.cloneExprValueDemandingShape(expr_id);
    }

    fn appendExprsFromValue(
        self: *Cloner,
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!void {
        const structural_value = switch (value) {
            .static_data_candidate => |candidate| candidate.runtime.*,
            else => value,
        };
        switch (shape) {
            .any => {
                try out.append(self.pass.allocator, try self.materialize(value));
            },
            .tag => |tag| {
                const tag_value = switch (structural_value) {
                    .tag => |tag_value| tag_value,
                    else => Common.invariant("tag call pattern matched a non-tag value"),
                };
                for (tag.payloads, tag_value.payloads) |payload_shape, payload| {
                    try self.appendExprsFromValue(payload_shape, payload, out);
                }
            },
            .record => |record| {
                const record_value = switch (structural_value) {
                    .record => |record_value| record_value,
                    else => Common.invariant("record call pattern matched a non-record value"),
                };
                for (record.fields, record_value.fields) |field_shape, field| {
                    if (!self.pass.program.names.recordFieldLabelTextEql(field_shape.name, field.name)) Common.invariant("record call-pattern field order changed after matching");
                    try self.appendExprsFromValue(field_shape.shape, field.value, out);
                }
            },
            .tuple => |tuple| {
                const tuple_value = switch (structural_value) {
                    .tuple => |tuple_value| tuple_value,
                    else => Common.invariant("tuple call pattern matched a non-tuple value"),
                };
                for (tuple.items, tuple_value.items) |item_shape, item| {
                    try self.appendExprsFromValue(item_shape, item, out);
                }
            },
            .nominal => |nominal| {
                const nominal_value = switch (structural_value) {
                    .nominal => |nominal_value| nominal_value,
                    else => Common.invariant("nominal call pattern matched a non-nominal value"),
                };
                try self.appendExprsFromValue(nominal.backing.*, nominal_value.backing.*, out);
            },
            .callable => |callable| {
                const callable_value = switch (structural_value) {
                    .callable => |callable_value| callable_value,
                    else => Common.invariant("callable call pattern matched a non-callable value"),
                };
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
    /// leaves — a back edge flipping an entry-known tag to a sibling tag, or a
    /// value that is not the shape's constructor — that sub-path demotes to
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
                const value_tag = switch (value) {
                    .tag => |value_tag| value_tag,
                    else => return try self.demoteLoopSlotLeaf(tag.ty, value, out),
                };
                if (!self.pass.program.names.tagLabelTextEql(value_tag.name, tag.name) or
                    !sameType(self.pass.program, tag.ty, value_tag.ty) or
                    value_tag.payloads.len != tag.payloads.len)
                {
                    return try self.demoteLoopSlotLeaf(tag.ty, value, out);
                }
                const payloads = try self.pass.arena.allocator().alloc(Shape, tag.payloads.len);
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
                            const fields = try self.pass.arena.allocator().alloc(FieldShape, record.fields.len);
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
                            const fields = try self.pass.arena.allocator().alloc(FieldShape, record.fields.len);
                            var demoted = false;
                            for (record.fields, 0..) |field_shape, index| {
                                const field_expr = try self.addExpr(.{ .ty = shapeType(field_shape.shape), .data = .{ .field_access = .{
                                    .receiver = receiver,
                                    .field = field_shape.name,
                                } } });
                                const supplied = try self.supplyLoopSlotLeaves(field_shape.shape, .{ .expr = field_expr }, out);
                                fields[index] = .{ .name = field_shape.name, .shape = supplied.shape };
                                demoted = demoted or supplied.demoted;
                            }
                            return .{ .shape = .{ .record = .{ .ty = record.ty, .fields = fields } }, .demoted = demoted };
                        }
                    },
                    else => {},
                }
                return try self.demoteLoopSlotLeaf(record.ty, value, out);
            },
            .tuple => |tuple| {
                switch (value) {
                    .tuple => |value_tuple| {
                        if (sameType(self.pass.program, tuple.ty, value_tuple.ty) and
                            value_tuple.items.len == tuple.items.len)
                        {
                            const items = try self.pass.arena.allocator().alloc(Shape, tuple.items.len);
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
                            const items = try self.pass.arena.allocator().alloc(Shape, tuple.items.len);
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
                    else => {},
                }
                return try self.demoteLoopSlotLeaf(tuple.ty, value, out);
            },
            .nominal => |nominal| {
                switch (value) {
                    .nominal => |value_nominal| {
                        if (sameType(self.pass.program, nominal.ty, value_nominal.ty)) {
                            const supplied = try self.supplyLoopSlotLeaves(nominal.backing.*, value_nominal.backing.*, out);
                            const backing = try self.pass.arena.allocator().create(Shape);
                            backing.* = supplied.shape;
                            return .{ .shape = .{ .nominal = .{ .ty = nominal.ty, .backing = backing } }, .demoted = supplied.demoted };
                        }
                    },
                    else => {},
                }
                return try self.demoteLoopSlotLeaf(nominal.ty, value, out);
            },
            .callable => |callable| {
                const value_callable = switch (value) {
                    .callable => |value_callable| value_callable,
                    else => return try self.demoteLoopSlotLeaf(callable.ty, value, out),
                };
                if (!sameType(self.pass.program, callable.ty, value_callable.ty) or
                    !callableTargetMatches(self.pass.program, callable.fn_id, value_callable.fn_id) or
                    value_callable.captures.len != callable.captures.len)
                {
                    return try self.demoteLoopSlotLeaf(callable.ty, value, out);
                }
                const captures = try self.pass.arena.allocator().alloc(Shape, callable.captures.len);
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

    fn cloneFieldAccess(self: *Cloner, ty: Type.TypeId, field: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingShape(field.receiver);
        if (fieldFromValue(self.pass.program, receiver, field.field)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = try self.materialize(receiver),
            .field = field.field,
        } } });
    }

    fn cloneTupleAccess(self: *Cloner, ty: Type.TypeId, access: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingShape(access.tuple);
        if (itemFromValue(receiver, access.elem_index)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
            .tuple = try self.materialize(receiver),
            .elem_index = access.elem_index,
        } } });
    }

    fn cloneMatch(self: *Cloner, ty: Type.TypeId, match: @import("../monotype/ast.zig").MatchExpr) Common.LowerError!Ast.ExprId {
        const scrutinee = try self.cloneExprValueDemandingShape(match.scrutinee);
        if (try self.simplifyKnownMatch(scrutinee, match.branches)) |body| return body;

        const scrutinee_expr = try self.materialize(scrutinee);
        return try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = scrutinee_expr,
            .branches = try self.cloneBranchSpan(match.branches),
            .comptime_site = match.comptime_site,
        } } });
    }

    fn simplifyKnownMatch(self: *Cloner, scrutinee: Value, branches_span: Ast.Span(Ast.Branch)) Common.LowerError!?Ast.ExprId {
        if (try self.simplifyKnownMatchValue(scrutinee, branches_span)) |value| {
            return try self.materialize(value);
        }
        return null;
    }

    fn simplifyKnownMatchValue(self: *Cloner, scrutinee: Value, branches_span: Ast.Span(Ast.Branch)) Common.LowerError!?Value {
        return self.selectKnownMatchValue(scrutinee, branches_span, false);
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
    ) Common.LowerError!?Value {
        if (scrutinee == .expr) return null;
        const branches = self.pass.program.branchSpan(branches_span);
        for (0..branches.len) |branch_index| {
            const branch = GuardedList.at(branches, branch_index);
            const match_change_start = self.changes.items.len;
            const verdict = try self.bindPatToValue(branch.pat, scrutinee);
            self.restore(match_change_start);
            switch (verdict) {
                // This branch can be neither ruled in nor ruled out
                // statically, so the whole fold aborts and the residual
                // match decides at runtime.
                .unknown => return null,
                .no_match => continue,
                .match => {},
            }
            if (branch.guard != null) return null;

            const pending_start = self.pending.items.len;
            const change_start = self.changes.items.len;
            const unsafe_count = self.unsafeLeafCount(scrutinee);
            if (try self.bindPatToMatchValue(branch.pat, scrutinee, branch.body, unsafe_count) == null) {
                Common.invariant("known constructor match changed after reusable payload binding");
            }
            const body = try self.cloneExprValue(branch.body);
            self.restore(change_start);
            return try self.resolvePending(pending_start, body);
        }
        if (decline_on_no_match) return null;
        Common.invariant("known constructor match had no matching branch");
    }

    fn bindPatToMatchValue(
        self: *Cloner,
        pat_id: Ast.PatId,
        value: Value,
        body: Ast.ExprId,
        unsafe_count: usize,
    ) Common.LowerError!?Value {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| {
                const prepared = try self.valueForMatchLocal(local, value, body, unsafe_count);
                try self.putSubst(local, prepared);
                return prepared;
            },
            .wildcard => return try self.makeReusableForMatch(value),
            .as => |as| {
                const as_uses = localUseCountInExpr(self.pass.program, as.local, body);
                const base = if (self.valueCanSubstitute(value) or
                    (unsafe_count == 1 and as_uses == 1 and localUseBeforeEffect(self.pass.program, as.local, body)))
                    value
                else
                    try self.makeReusableForMatch(value);
                const prepared = (try self.bindPatToMatchValue(as.pattern, base, body, unsafe_count)) orelse return null;
                try self.putSubst(as.local, prepared);
                return prepared;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                switch (value) {
                    .static_data_candidate => |candidate| return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        candidate,
                        body,
                        unsafe_count,
                    ),
                    .record => |record| {
                        const prepared_fields = try self.pass.arena.allocator().alloc(FieldValue, record.fields.len);
                        for (record.fields, 0..) |field, index| {
                            if (recordPatField(self.pass.program, fields, field.name)) |field_pat| {
                                const prepared = (try self.bindPatToMatchValue(field_pat, field.value, body, unsafe_count)) orelse return null;
                                prepared_fields[index] = .{
                                    .name = field.name,
                                    .value = prepared,
                                };
                            } else {
                                prepared_fields[index] = .{
                                    .name = field.name,
                                    .value = try self.makeReusableForMatch(field.value),
                                };
                            }
                        }
                        return Value{ .record = .{
                            .ty = record.ty,
                            .fields = prepared_fields,
                        } };
                    },
                    .nominal => |nominal| return try self.bindPatToMatchValue(pat_id, nominal.backing.*, body, unsafe_count),
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return null;
                        for (0..fields.len) |index| {
                            const field = GuardedList.at(fields, index);
                            const field_ty = self.pass.program.getPat(field.pattern).ty;
                            const field_expr = try self.addExpr(.{ .ty = field_ty, .data = .{ .field_access = .{
                                .receiver = receiver,
                                .field = field.name,
                            } } });
                            _ = (try self.bindPatToMatchValue(field.pattern, .{ .expr = field_expr }, body, unsafe_count)) orelse return null;
                        }
                        return value;
                    },
                    else => return null,
                }
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                switch (value) {
                    .static_data_candidate => |candidate| return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        candidate,
                        body,
                        unsafe_count,
                    ),
                    .tuple => |tuple| {
                        if (pats.len != tuple.items.len) return null;
                        const items = try self.pass.arena.allocator().alloc(Value, tuple.items.len);
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            const child_value = tuple.items[index];
                            items[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, unsafe_count)) orelse return null;
                        }
                        return Value{ .tuple = .{
                            .ty = tuple.ty,
                            .items = items,
                        } };
                    },
                    .nominal => |nominal| return try self.bindPatToMatchValue(pat_id, nominal.backing.*, body, unsafe_count),
                    .expr => |receiver| {
                        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return null;
                        for (0..pats.len) |index| {
                            const child_pat = GuardedList.at(pats, index);
                            const item_ty = self.pass.program.getPat(child_pat).ty;
                            const item_expr = try self.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                                .tuple = receiver,
                                .elem_index = @as(u32, @intCast(index)),
                            } } });
                            _ = (try self.bindPatToMatchValue(child_pat, .{ .expr = item_expr }, body, unsafe_count)) orelse return null;
                        }
                        return value;
                    },
                    else => return null,
                }
            },
            .tag => |tag_pat| {
                if (value == .static_data_candidate) {
                    return try self.bindStaticDataCandidateToMatchValue(
                        pat_id,
                        value.static_data_candidate,
                        body,
                        unsafe_count,
                    );
                }
                const tag = tagFromValue(value) orelse return null;
                if (!self.pass.program.names.tagLabelTextEql(tag.name, tag_pat.name)) return null;
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) return null;
                const payloads = try self.pass.arena.allocator().alloc(Value, tag.payloads.len);
                for (0..pats.len) |index| {
                    const child_pat = GuardedList.at(pats, index);
                    const child_value = tag.payloads[index];
                    payloads[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, unsafe_count)) orelse return null;
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
                        unsafe_count,
                    );
                }
                const nominal = switch (value) {
                    .nominal => |nominal| nominal,
                    else => return null,
                };
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = (try self.bindPatToMatchValue(backing_pat, nominal.backing.*, body, unsafe_count)) orelse return null;
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
        unsafe_count: usize,
    ) Common.LowerError!?Value {
        const runtime = try self.pass.arena.allocator().create(Value);
        runtime.* = (try self.bindPatToMatchValue(pat_id, candidate.runtime.*, body, unsafe_count)) orelse return null;
        return Value{ .static_data_candidate = .{
            .ty = candidate.ty,
            .static_data = candidate.static_data,
            .runtime = runtime,
        } };
    }

    /// Node-count threshold above which a known constructor value bound to an
    /// inlined or matched local is boxed instead of substituted or reused. A
    /// statically constructed adapter chain is tens of nodes; a
    /// recursively-constructed chain wrapped a runtime number of times has no
    /// static depth, so its fixpoint known value instead fills the shape work
    /// budget and reaches thousands of nodes. Substituting that value shares it
    /// into every use, where each level of specialization re-walks and
    /// re-inlines the whole thing, and the total never settles. A value this
    /// large is past the point where per-use specialization pays for itself, so
    /// binding it once behind a local (the sanctioned dynamic boundary) both
    /// bounds the work and is the right code: real chains stay an order of
    /// magnitude under the threshold and keep their per-use specialization.
    /// Declining to track a value is a missed optimization, never a wrong
    /// lowering. See design.md "Core Principles" on bounded post-check walks.
    const known_value_track_cap: usize = 512;

    /// Materialize a known value once and bind it reuse-safely, so it is no
    /// longer tracked as a known constructor at its use sites.
    fn boxDeepKnownValue(self: *Cloner, value: Value) Common.LowerError!Value {
        return try self.makeReusableForMatch(.{ .expr = try self.materialize(value) });
    }

    fn valueForMatchLocal(
        self: *Cloner,
        local: Ast.LocalId,
        value: Value,
        body: Ast.ExprId,
        unsafe_count: usize,
    ) Common.LowerError!Value {
        if (self.knownConstructorSize(value) >= known_value_track_cap) {
            return try self.boxDeepKnownValue(value);
        }
        const uses = localUseCountInExpr(self.pass.program, local, body);
        if (self.valueCanSubstitute(value) or
            (unsafe_count == 1 and uses == 1 and localUseBeforeEffect(self.pass.program, local, body)))
        {
            return value;
        }
        return try self.makeReusableForMatch(value);
    }

    fn valueForInlineLocal(
        self: *Cloner,
        local: Ast.LocalId,
        value: Value,
        body: Ast.ExprId,
        unsafe_count: usize,
    ) Common.LowerError!Value {
        if (self.knownConstructorSize(value) >= known_value_track_cap) {
            return try self.boxDeepKnownValue(value);
        }
        const uses = localUseCountInExpr(self.pass.program, local, body);
        if (self.valueCanSubstitute(value) or
            (unsafe_count == 1 and uses == 1 and localUseBeforeEffect(self.pass.program, local, body)))
        {
            return value;
        }
        return try self.makeReusableForMatch(value);
    }

    /// Reported size for a known value that exhausts the size work budget: a
    /// value too large to measure counts as effectively unbounded. Reporting a
    /// value this large (rather than a truncated count) errs the inline
    /// recursion guard toward declining — a call whose size reads as the cap is
    /// never strictly smaller than an active frame, so it takes the residual
    /// (boxed) call — which is the safe direction for a depth/size measure.
    const known_constructor_size_cap: usize = std.math.maxInt(usize);

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
    fn knownConstructorSize(self: *Cloner, value: Value) usize {
        var budget: u32 = known_constructor_size_work_budget;
        return self.knownConstructorSizeBudgeted(value, &budget);
    }

    fn knownConstructorSizeBudgeted(self: *Cloner, value: Value, budget: *u32) usize {
        if (budget.* == 0) return known_constructor_size_cap;
        budget.* -= 1;
        return switch (value) {
            .expr => 0,
            .static_data_candidate => |candidate| self.knownConstructorSizeBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                var count: usize = 1;
                for (tag.payloads) |payload| count += self.knownConstructorSizeBudgeted(payload, budget);
                break :blk count;
            },
            .record => |record| blk: {
                var count: usize = 1;
                for (record.fields) |field| count += self.knownConstructorSizeBudgeted(field.value, budget);
                break :blk count;
            },
            .tuple => |tuple| blk: {
                var count: usize = 1;
                for (tuple.items) |item| count += self.knownConstructorSizeBudgeted(item, budget);
                break :blk count;
            },
            .nominal => |nominal| 1 + self.knownConstructorSizeBudgeted(nominal.backing.*, budget),
            .callable => |callable| blk: {
                var count: usize = 1;
                for (callable.captures) |capture| count += self.knownConstructorSizeBudgeted(capture.value, budget);
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
                if (self.subst.get(local)) |value| break :blk value;
                if (self.binderIdentityOf(local)) |identity| {
                    if (self.binder_subst.get(identity)) |value| break :blk value;
                }
                break :blk null;
            },
            .field_access => |field| blk: {
                const receiver = self.peekKnownValue(field.receiver) orelse break :blk null;
                break :blk fieldFromValue(self.pass.program, receiver, field.field);
            },
            .tuple_access => |access| blk: {
                const receiver = self.peekKnownValue(access.tuple) orelse break :blk null;
                break :blk itemFromValue(receiver, access.elem_index);
            },
            .static_data_candidate => |candidate| self.peekKnownValue(candidate.runtime_expr),
            else => null,
        };
    }

    fn argsKnownConstructorSize(self: *Cloner, span: Ast.Span(Ast.ExprId)) usize {
        var total: usize = 0;
        const args = self.pass.program.exprSpan(span);
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (self.peekKnownValue(arg)) |value| total += self.knownConstructorSize(value);
        }
        return total;
    }

    fn captureOperandsKnownConstructorSize(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) usize {
        var total: usize = 0;
        const operands = self.pass.program.captureOperandSpan(span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            if (self.peekKnownValue(operand.value)) |value| total += self.knownConstructorSize(value);
        }
        return total;
    }

    /// Reported unsafe-leaf count for a known value that exhausts the work
    /// budget: a value too large to scan counts as having many unsafe leaves.
    /// Reporting the cap (rather than a truncated count) errs every consumer
    /// toward reuse — a count above one fails the `unsafe_count == 1`
    /// single-substitution conditions, so the value is bound to a local and
    /// evaluated once instead of duplicated — which is the safe direction: it
    /// can never drop or reorder an effect a truncated count would have missed.
    const unsafe_leaf_count_cap: usize = std.math.maxInt(usize);

    /// Total work budget for scanning one known value's unsafe leaves. Shared
    /// substructure makes an unmemoized scan re-descend combinatorially many
    /// paths, so the scan spends one shared budget per node visit and reports
    /// the cap when it runs out. See design.md "Core Principles" on bounded
    /// post-check walks.
    const unsafe_leaf_count_work_budget: u32 = 4096;

    fn unsafeLeafCount(self: *Cloner, value: Value) usize {
        var budget: u32 = unsafe_leaf_count_work_budget;
        return self.unsafeLeafCountBudgeted(value, &budget);
    }

    fn unsafeLeafCountBudgeted(self: *Cloner, value: Value, budget: *u32) usize {
        if (budget.* == 0) return unsafe_leaf_count_cap;
        budget.* -= 1;
        return switch (value) {
            .expr => |expr| if (self.exprCanSubstitute(expr)) 0 else 1,
            .static_data_candidate => |candidate| self.unsafeLeafCountBudgeted(candidate.runtime.*, budget),
            .tag => |tag| blk: {
                var count: usize = 0;
                for (tag.payloads) |payload| count += self.unsafeLeafCountBudgeted(payload, budget);
                break :blk count;
            },
            .record => |record| blk: {
                var count: usize = 0;
                for (record.fields) |field| count += self.unsafeLeafCountBudgeted(field.value, budget);
                break :blk count;
            },
            .tuple => |tuple| blk: {
                var count: usize = 0;
                for (tuple.items) |item| count += self.unsafeLeafCountBudgeted(item, budget);
                break :blk count;
            },
            .nominal => |nominal| self.unsafeLeafCountBudgeted(nominal.backing.*, budget),
            .callable => |callable| blk: {
                var count: usize = 0;
                for (callable.captures) |capture| count += self.unsafeLeafCountBudgeted(capture.value, budget);
                break :blk count;
            },
        };
    }

    /// Total work budget for making one value reuse-safe. A known value is not
    /// always a small finite tree: substitution shares one value union across
    /// every use site, so a value built by a recursively-constructed chain (an
    /// iterator wrapped around itself through many map layers) is a compact
    /// graph reached by combinatorially many distinct paths, and this walk
    /// probes each visited node with `valueCanSubstitute` — itself a full
    /// sub-walk — so its cost is the node count times that probe and grows far
    /// past any per-level depth. The walk spends one shared budget per node
    /// visit and, when it runs out, keeps the remaining sub-value materialized
    /// as-is instead of continuing to rewrite it. See design.md "Core
    /// Principles" on bounded post-check walks.
    ///
    /// Keeping a sub-value as-is declines the single-evaluation rewrite for it,
    /// the same conservative direction the substitution check takes on its own
    /// exhaustion: the values large enough to exhaust this budget are the deep
    /// constructor chains of recursive iterator construction, whose leaves are
    /// pure structural components, so leaving them un-rewritten at worst
    /// recomputes a pure leaf and never drops or reorders an effect.
    const make_reusable_work_budget: u32 = 4096;

    fn makeReusableForMatch(self: *Cloner, value: Value) Common.LowerError!Value {
        var budget: u32 = make_reusable_work_budget;
        return try self.makeReusableForMatchBudgeted(value, &budget);
    }

    fn makeReusableForMatchBudgeted(self: *Cloner, value: Value, budget: *u32) Common.LowerError!Value {
        if (budget.* == 0) return value;
        budget.* -= 1;
        if (self.valueCanSubstitute(value)) return value;
        return switch (value) {
            .expr => |expr| blk: {
                const ty = self.pass.program.getExpr(expr).ty;
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try self.pending.append(self.pass.allocator, .{
                    .local = local,
                    .ty = ty,
                    .value = expr,
                    .marks = self.effect_marks,
                });
                break :blk Value{ .expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                }) };
            },
            .static_data_candidate => |candidate| blk: {
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), candidate.ty);
                try self.pending.append(self.pass.allocator, .{
                    .local = local,
                    .ty = candidate.ty,
                    .value = try self.materialize(value),
                    .marks = self.effect_marks,
                });
                break :blk Value{ .expr = try self.addExpr(.{
                    .ty = candidate.ty,
                    .data = .{ .local = local },
                }) };
            },
            .tag => |tag| blk: {
                const payloads = try self.pass.arena.allocator().alloc(Value, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = try self.makeReusableForMatchBudgeted(payload, budget);
                }
                break :blk Value{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| blk: {
                const fields = try self.pass.arena.allocator().alloc(FieldValue, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.makeReusableForMatchBudgeted(field.value, budget),
                    };
                }
                break :blk Value{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.pass.arena.allocator().alloc(Value, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = try self.makeReusableForMatchBudgeted(item, budget);
                }
                break :blk Value{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = try self.makeReusableForMatchBudgeted(nominal.backing.*, budget);
                break :blk Value{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.pass.arena.allocator().alloc(CaptureValue, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = .{
                        .id = capture.id,
                        .value = try self.makeReusableForMatchBudgeted(capture.value, budget),
                    };
                }
                break :blk Value{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
        };
    }

    /// Emit the pending bindings created at or after `start` as a let chain
    /// around `expr`, oldest outermost so evaluation order is preserved, and
    /// drop them from the stack.
    fn flushPendingSince(self: *Cloner, start: usize, expr: Ast.ExprId) Common.LowerError!Ast.ExprId {
        if (self.pending.items.len <= start) return expr;
        const ty = self.pass.program.getExpr(expr).ty;
        var result = expr;
        var index = self.pending.items.len;
        while (index > start) {
            index -= 1;
            const pending = self.pending.items[index];
            const pat = try self.pass.program.addPat(.{
                .ty = pending.ty,
                .data = .{ .bind = pending.local },
            });
            result = try self.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = pending.value,
                .rest = result,
            } } });
        }
        self.pending.shrinkRetainingCapacity(start);
        return result;
    }

    /// Emit the pending bindings created at or after `start` as let
    /// statements, oldest first, and drop them from the stack. Used where a
    /// statement list is being built, so the bindings dominate the statement
    /// whose cloning created them and everything after it.
    fn appendPendingStmtsSince(self: *Cloner, start: usize, out: *std.ArrayList(Ast.StmtId)) Common.LowerError!void {
        for (self.pending.items[start..]) |pending| {
            const pat = try self.pass.program.addPat(.{
                .ty = pending.ty,
                .data = .{ .bind = pending.local },
            });
            try out.append(self.pass.allocator, try self.addStmt(.{ .let_ = .{
                .pat = pat,
                .value = pending.value,
            } }));
        }
        self.pending.shrinkRetainingCapacity(start);
    }

    /// Resolve the pending bindings a construct created while producing
    /// `body`. A structured value whose bindings are all effect-free
    /// computations, created in a region that has emitted no effect, keeps
    /// its structure: the bindings stay pending and the region boundary
    /// emits them, where they still dominate every leaf reference and cross
    /// only effect-free evaluation. Anything else pins the value here — it
    /// is materialized and wrapped so evaluation order and count stay
    /// exactly as written.
    fn resolvePending(self: *Cloner, start: usize, body: Value) Common.LowerError!Value {
        if (self.pending.items.len <= start) return body;
        if (body != .expr) {
            var delegatable = true;
            for (self.pending.items[start..]) |pending| {
                if (pending.marks != self.region_entry_marks or
                    !exprHasNoObservableEffect(self.pass.program, self.pass.fn_effect_free, pending.value, false))
                {
                    delegatable = false;
                    break;
                }
            }
            if (delegatable) return body;
        }
        return .{ .expr = try self.flushPendingSince(start, try self.materialize(body)) };
    }

    fn cloneCaseOfCaseValue(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee_expr: Ast.ExprId,
        outer_branches_span: Ast.Span(Ast.Branch),
    ) Common.LowerError!?Value {
        const pending_entry = self.pending.items.len;
        const scrutinee_data = self.pass.program.getExpr(scrutinee_expr).data;

        const outer_branches = self.pass.program.branchSpan(outer_branches_span);
        for (0..outer_branches.len) |branch_index| {
            const branch = GuardedList.at(outer_branches, branch_index);
            if (branch.guard != null) return null;
        }

        const branch_work = switch (scrutinee_data) {
            .match_ => |inner_match| self.pass.program.branchSpan(inner_match.branches).len,
            .if_ => |inner_if| self.pass.program.ifBranchSpan(inner_if.branches).len + 1,
            else => return null,
        };
        if (self.case_of_case_depth == case_of_case_depth_limit) return null;
        if (!self.spendCaseOfCaseWork(branch_work)) return null;
        self.case_of_case_depth += 1;
        defer self.case_of_case_depth -= 1;

        switch (scrutinee_data) {
            .match_ => |inner_match| {
                const inner_branches = try GuardedList.dupe(self.pass.allocator, Ast.Branch, self.pass.program.branchSpan(inner_match.branches));
                defer self.pass.allocator.free(inner_branches);

                var rewritten = try self.pass.allocator.alloc(Ast.Branch, inner_branches.len);
                defer self.pass.allocator.free(rewritten);

                for (inner_branches, 0..) |inner_branch, index| {
                    // Each rewritten branch flushes every pending binding it
                    // creates, so it is its own region.
                    const pending_start = self.pending.items.len;
                    const saved_entry_marks = self.region_entry_marks;
                    self.region_entry_marks = self.effect_marks;
                    defer self.region_entry_marks = saved_entry_marks;
                    const change_start = self.changes.items.len;
                    try self.shadowPatLocals(inner_branch.pat);
                    const inner_value = try self.cloneExprValue(inner_branch.body);
                    const outer_value = (try self.distributeMatchOverValue(ty, inner_value, outer_branches_span)) orelse {
                        self.restore(change_start);
                        self.pending.shrinkRetainingCapacity(pending_entry);
                        return null;
                    };
                    rewritten[index] = .{
                        .pat = inner_branch.pat,
                        .guard = inner_branch.guard,
                        .body = try self.flushPendingSince(pending_start, try self.materialize(outer_value)),
                    };
                    self.restore(change_start);
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
                    // Each rewritten branch flushes every pending binding it
                    // creates, so it is its own region.
                    const pending_start = self.pending.items.len;
                    const saved_entry_marks = self.region_entry_marks;
                    self.region_entry_marks = self.effect_marks;
                    defer self.region_entry_marks = saved_entry_marks;
                    const inner_value = try self.cloneExprValue(inner_branch.body);
                    const outer_value = (try self.distributeMatchOverValue(ty, inner_value, outer_branches_span)) orelse {
                        self.pending.shrinkRetainingCapacity(pending_entry);
                        return null;
                    };
                    rewritten[index] = .{
                        .cond = inner_branch.cond,
                        .body = try self.flushPendingSince(pending_start, try self.materialize(outer_value)),
                    };
                }

                const pending_start = self.pending.items.len;
                const saved_entry_marks = self.region_entry_marks;
                self.region_entry_marks = self.effect_marks;
                defer self.region_entry_marks = saved_entry_marks;
                const else_value = try self.cloneExprValue(inner_if.final_else);
                const outer_else = (try self.distributeMatchOverValue(ty, else_value, outer_branches_span)) orelse {
                    self.pending.shrinkRetainingCapacity(pending_entry);
                    return null;
                };
                const final_else = try self.flushPendingSince(pending_start, try self.materialize(outer_else));

                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .if_ = .{
                    .branches = try self.pass.program.addIfBranchSpan(rewritten),
                    .final_else = final_else,
                } } }) };
            },
            else => unreachable,
        }
    }

    fn spendCaseOfCaseWork(self: *Cloner, amount: usize) bool {
        if (amount > @as(usize, self.case_of_case_work_remaining)) return false;
        self.case_of_case_work_remaining -= @intCast(amount);
        return true;
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
    ) Common.LowerError!?Value {
        if (try self.selectKnownMatchValue(inner_value, outer_branches_span, true)) |value| return value;
        return switch (inner_value) {
            .expr => |expr| try self.cloneCaseOfCaseValue(ty, expr, outer_branches_span),
            else => null,
        };
    }

    fn inlineCallableCallValue(
        self: *Cloner,
        ty: Type.TypeId,
        callable: CallableValue,
        args_span: Ast.Span(Ast.ExprId),
    ) Common.LowerError!Value {
        var callable_call_size: usize = 0;
        for (callable.captures) |capture| callable_call_size += self.knownConstructorSize(capture.value);
        callable_call_size += self.argsKnownConstructorSize(args_span);
        for (self.inline_stack.items) |active| {
            if (active.fn_id != callable.fn_id) continue;
            if (callable_call_size == 0 or callable_call_size >= active.known_size) {
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(.{ .callable = callable }),
                    .args = try self.cloneExprSpan(args_span),
                } } }) };
            }
        }

        const source_fn = self.pass.program.getFn(callable.fn_id);
        const body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) },
        };
        if (exprContainsReturn(self.pass.program, body)) {
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

        const pending_start = self.pending.items.len;
        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const prepared_captures = try self.pass.allocator.alloc(Value, callable.captures.len);
        defer self.pass.allocator.free(prepared_captures);
        for (source_captures, 0..) |source_capture, index| {
            const id = self.pass.program.captureIdOfLocal(source_capture.local);
            const capture_value = callableCaptureValueForId(callable.captures, id) orelse
                Common.invariant("callable value had no value for a source capture slot");
            prepared_captures[index] = try self.makeReusableForMatch(capture_value);
            try self.putSubst(source_capture.local, prepared_captures[index]);
        }

        const arg_values = try self.pass.allocator.alloc(Value, args.len);
        defer self.pass.allocator.free(arg_values);
        for (args, 0..) |arg_expr, index| {
            arg_values[index] = try self.cloneExprValue(arg_expr);
        }

        var unsafe_count: usize = 0;
        for (prepared_captures) |capture_value| unsafe_count += self.unsafeLeafCount(capture_value);
        for (arg_values) |arg_value| unsafe_count += self.unsafeLeafCount(arg_value);

        const prepared_args = try self.pass.allocator.alloc(Value, arg_values.len);
        defer self.pass.allocator.free(prepared_args);
        for (source_args, arg_values, 0..) |source_arg, arg_value, index| {
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, unsafe_count);
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callable.fn_id, .known_size = callable_call_size });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callable.fn_id) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        return try self.resolvePending(pending_start, try self.cloneExprValue(body));
    }

    fn inlineDirectCallValue(
        self: *Cloner,
        callee: Ast.FnId,
        args_span: Ast.Span(Ast.ExprId),
        captures_span: Ast.Span(Ast.CaptureOperand),
        original_expr: Ast.ExprId,
    ) Common.LowerError!Value {
        const direct_call_size = self.argsKnownConstructorSize(args_span) + self.captureOperandsKnownConstructorSize(captures_span);
        for (self.inline_stack.items) |active| {
            if (active.fn_id != callee) continue;
            if (direct_call_size == 0 or direct_call_size >= active.known_size) {
                return .{ .expr = try self.cloneExprPlain(original_expr) };
            }
        }

        const source_fn = self.pass.program.getFn(callee);
        const body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => return .{ .expr = try self.cloneExprPlain(original_expr) },
        };
        if (exprContainsReturn(self.pass.program, body)) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }
        const source_args = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(args_span));
        defer self.pass.allocator.free(args);
        if (source_args.len != args.len) Common.invariant("direct call arity differed from lifted function arity");

        const pending_start = self.pending.items.len;
        const change_start = self.changes.items.len;
        defer self.restore(change_start);

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
                .value = try self.cloneExprValue(operand.value),
            };
        }

        const arg_values = try self.pass.allocator.alloc(Value, args.len);
        defer self.pass.allocator.free(arg_values);
        for (args, 0..) |arg_expr, index| {
            arg_values[index] = try self.cloneExprValue(arg_expr);
        }

        var unsafe_count: usize = 0;
        for (capture_values) |capture_value| unsafe_count += self.unsafeLeafCount(capture_value.value);
        for (arg_values) |arg_value| unsafe_count += self.unsafeLeafCount(arg_value);

        const prepared_captures = try self.pass.allocator.alloc(Value, captures.len);
        defer self.pass.allocator.free(prepared_captures);
        for (captures, 0..) |capture, index| {
            const id = self.pass.program.captureIdOfLocal(capture.local);
            const capture_value = callableCaptureValueForId(capture_values, id) orelse
                Common.invariant("direct call had no value for a source capture slot");
            prepared_captures[index] = try self.valueForInlineLocal(capture.local, capture_value, body, unsafe_count);
        }

        const prepared_args = try self.pass.allocator.alloc(Value, arg_values.len);
        defer self.pass.allocator.free(prepared_args);
        for (source_args, arg_values, 0..) |source_arg, arg_value, index| {
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, unsafe_count);
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callee, .known_size = direct_call_size });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callee) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (captures, prepared_captures) |capture, capture_value| {
            try self.putSubst(capture.local, capture_value);
        }
        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        return try self.resolvePending(pending_start, try self.cloneExprValue(body));
    }

    fn bindPatToValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!MatchVerdict {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| {
                try self.putSubst(local, value);
                return .match;
            },
            .wildcard => return .match,
            .as => |as| {
                const verdict = try self.bindPatToValue(as.pattern, value);
                if (verdict != .match) return verdict;
                try self.putSubst(as.local, value);
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
                            const field_expr = try self.addExpr(.{ .ty = field_ty, .data = .{ .field_access = .{
                                .receiver = receiver,
                                .field = field.name,
                            } } });
                            switch (try self.bindPatToValue(field.pattern, .{ .expr = field_expr })) {
                                .match => {},
                                .no_match => return .no_match,
                                .unknown => verdict = .unknown,
                            }
                        }
                        return verdict;
                    },
                    else => {},
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
                    switch (try self.bindPatToValue(field.pattern, field_value)) {
                        .match => {},
                        .no_match => return .no_match,
                        .unknown => verdict = .unknown,
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
                            switch (try self.bindPatToValue(child_pat, .{ .expr = item_expr })) {
                                .match => {},
                                .no_match => return .no_match,
                                .unknown => verdict = .unknown,
                            }
                        }
                        return verdict;
                    },
                    else => {},
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
                    switch (try self.bindPatToValue(child_pat, child_value)) {
                        .match => {},
                        .no_match => return .no_match,
                        .unknown => verdict = .unknown,
                    }
                }
                return verdict;
            },
            .tag => |tag_pat| {
                switch (value) {
                    .expr => return .unknown,
                    else => {},
                }
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
                    switch (try self.bindPatToValue(child_pat, child_value)) {
                        .match => {},
                        .no_match => return .no_match,
                        .unknown => verdict = .unknown,
                    }
                }
                return verdict;
            },
            .nominal => |backing_pat| {
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
        if (!self.valueCanSubstitute(value)) return .unknown;
        return if (try self.bindPatToFlowValue(pat_id, value)) .match else .unknown;
    }

    /// Bind a pattern for ordinary structured value flow. Unlike the
    /// three-way static matcher above, this never selects a match branch: it
    /// may project a value of a statically known record or tuple type and
    /// simply reports whether all required substitutions could be formed.
    fn bindPatToFlowValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!bool {
        const pat = self.pass.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| {
                try self.putSubst(local, value);
                return true;
            },
            .wildcard => return true,
            .as => |as| {
                if (!try self.bindPatToFlowValue(as.pattern, value)) return false;
                try self.putSubst(as.local, value);
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
                            const field_expr = try self.addExpr(.{ .ty = field_ty, .data = .{ .field_access = .{
                                .receiver = receiver,
                                .field = field.name,
                            } } });
                            if (!try self.bindPatToFlowValue(field.pattern, .{ .expr = field_expr })) return false;
                        }
                    },
                    else => return false,
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
                    else => return false,
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
            .nominal => |backing_pat| return switch (value) {
                .static_data_candidate => |candidate| try self.bindPatToFlowValue(pat_id, candidate.runtime.*),
                .nominal => |nominal| try self.bindPatToFlowValue(backing_pat, nominal.backing.*),
                else => false,
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
        try self.putSubst(local, .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .local = local } }) });
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
            try self.putSubst(source, .{ .expr = local_expr });
        }
        return fresh;
    }

    /// Rewrite a local reference stored directly in an expression node rather
    /// than in a child `.local` expression. These fields require a runtime
    /// local, so a structured substitution is an invalid cloned IR state.
    fn cloneLocalRef(self: *Cloner, source: Ast.LocalId) Ast.LocalId {
        const value = self.subst.get(source) orelse blk: {
            const identity = self.binderIdentityOf(source) orelse return source;
            break :blk self.binder_subst.get(identity) orelse return source;
        };
        const expr = switch (value) {
            .expr => |expr| expr,
            else => Common.invariant("SpecConstr local-id field referenced a non-local substituted value"),
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
    /// can all be named dissolves instead: the caller drains the pending
    /// bindings at this statement's position — the same computations in the
    /// same order — and the bound name keeps its structured value for the
    /// rest of the block. Returns null for a dissolved statement.
    fn cloneStmt(self: *Cloner, stmt_id: Ast.StmtId) Common.LowerError!?Ast.StmtId {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const stmt_loc = self.pass.program.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) self.current_loc = stmt_loc;
        const stmt_region = self.pass.program.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) self.current_region = stmt_region;

        const stmt = self.pass.program.getStmt(stmt_id);
        return try self.addStmt(switch (stmt) {
            .uninitialized => |pat| blk: {
                break :blk .{ .uninitialized = try self.clonePat(pat, .bind_runtime) };
            },
            .let_ => |let_| blk: {
                const recursive_pat = if (let_.recursive)
                    try self.clonePat(let_.pat, .bind_runtime)
                else
                    null;
                const value = try self.cloneExprValue(let_.value);
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
                const self_referential = switch (pat.data) {
                    .bind => |local| localUseCountInExpr(self.pass.program, local, let_.value) != 0,
                    else => let_.recursive,
                };
                if (!self_referential) {
                    // The drained bindings sit exactly where the statement
                    // sat, so no evaluation moves and no gate is needed.
                    const change_before = self.changes.items.len;
                    const pending_before = self.pending.items.len;
                    const reusable = try self.makeReusableForMatch(value);
                    if (try self.bindPatToFlowValue(let_.pat, reusable)) return null;
                    self.restore(change_before);
                    self.pending.shrinkRetainingCapacity(pending_before);
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
        });
    }

    fn cloneExprSpan(self: *Cloner, span: Ast.Span(Ast.ExprId)) Common.LowerError!Ast.Span(Ast.ExprId) {
        const source = try GuardedList.dupe(self.pass.allocator, Ast.ExprId, self.pass.program.exprSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.ExprId, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |expr, index| values[index] = try self.cloneExpr(expr);
        return try self.pass.program.addExprSpan(values);
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
            const change_start = self.changes.items.len;
            const pat = try self.clonePat(branch.pat, .bind_runtime);
            values[index] = .{
                .pat = pat,
                .guard = if (branch.guard) |guard| try self.cloneExpr(guard) else null,
                .body = try self.cloneExpr(branch.body),
            };
            self.restore(change_start);
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
            .static_data_candidate => |candidate| return try self.addExpr(.{ .ty = candidate.ty, .data = .{ .static_data_candidate = .{
                .static_data = candidate.static_data,
                .runtime_expr = try self.materialize(candidate.runtime.*),
            } } }),
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
            .nominal => |nominal| return try self.addExpr(.{ .ty = nominal.ty, .data = .{
                .nominal = try self.materialize(nominal.backing.*),
            } }),
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
            const expr = switch (value) {
                .expr => |expr| expr,
                else => {
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

        if (!all_original and self.emit_callable_workers) return try self.materializeCallableWorker(callable);

        return try self.materializeCallableWithCaptures(callable.ty, callable.fn_id, fn_.captures, callable.captures);
    }

    fn materializeCallableWorker(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
        const source_fn_id = self.pass.callable_sources.get(callable.fn_id) orelse callable.fn_id;
        if (self.pass.callable_workers.get(source_fn_id)) |worker_fn_id| {
            const worker = self.pass.program.getFn(worker_fn_id);
            return try self.materializeCallableWithCaptures(callable.ty, worker_fn_id, worker.captures, callable.captures);
        }

        const source_fn = self.pass.program.getFn(source_fn_id);
        const source_body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => Common.invariant("hosted callable value needed a rewritten body"),
        };
        const source_captures = try GuardedList.dupe(self.pass.allocator, Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(source_captures);
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        // Capture locals are the worker's dynamic inputs. Their values belong
        // on each function reference, not in the worker identity or body.
        const captures_span = source_fn.captures;

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
            .args = args_span,
            .captures = captures_span,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        try self.pass.callable_workers.put(source_fn_id, worker_fn_id);
        try self.pass.callable_sources.put(worker_fn_id, source_fn_id);
        try self.pass.copyProcDebugName(source_fn.symbol, symbol);

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        for (source_captures) |source_capture| {
            const local_expr = try self.addExpr(.{
                .ty = source_capture.ty,
                .data = .{ .local = source_capture.local },
            });
            try self.putSubst(source_capture.local, .{ .expr = local_expr });
        }
        for (source_args, args) |source_arg, arg| {
            const arg_expr = try self.addExpr(.{
                .ty = arg.ty,
                .data = .{ .local = arg.local },
            });
            try self.putSubst(source_arg.local, .{ .expr = arg_expr });
        }

        self.pass.program.setFn(worker_fn_id, .{
            .symbol = symbol,
            .source = source_fn.source,
            .args = args_span,
            .captures = captures_span,
            .body = .{ .roc = try self.cloneExpr(source_body) },
            .ret = source_fn.ret,
        });

        return try self.materializeCallableWithCaptures(
            callable.ty,
            worker_fn_id,
            captures_span,
            callable.captures,
        );
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
            const value_expr = try self.materialize(value);
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
        const out = try self.pass.arena.allocator().create(Value);
        out.* = value;
        return out;
    }

    fn putSubst(self: *Cloner, local: Ast.LocalId, value: Value) Allocator.Error!void {
        const previous = self.subst.get(local);
        try self.changes.append(self.pass.allocator, .{
            .key = .{ .local = local },
            .previous = previous,
        });
        try self.subst.put(local, value);

        const subst_binder = switch (value) {
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
        if (subst_binder) if (self.binderIdentityOf(local)) |identity| {
            const previous_binder = self.binder_subst.get(identity);
            try self.changes.append(self.pass.allocator, .{
                .key = .{ .binder = identity },
                .previous = previous_binder,
            });
            try self.binder_subst.put(identity, value);
        };
    }

    /// Identity a local's binder-scoped substitution is keyed by: the pattern
    /// binder together with the digest of the local's monomorphic type. Two
    /// locals that share a binder but were monomorphized at different types are
    /// distinct bindings and must not read one another's substitution.
    fn binderIdentityOf(self: *Cloner, local: Ast.LocalId) ?BinderIdentity {
        const local_data = self.pass.program.getLocal(local);
        const binder = local_data.binder orelse return null;
        return .{
            .binder = binder,
            .digest = self.pass.program.types.typeDigest(&self.pass.program.names, local_data.ty),
        };
    }

    fn restore(self: *Cloner, start: usize) void {
        var index = self.changes.items.len;
        while (index > start) {
            index -= 1;
            const change = self.changes.items[index];
            switch (change.key) {
                .local => |local| {
                    if (change.previous) |previous| {
                        self.subst.putAssumeCapacity(local, previous);
                    } else {
                        _ = self.subst.remove(local);
                    }
                },
                .binder => |identity| {
                    if (change.previous) |previous| {
                        self.binder_subst.putAssumeCapacity(identity, previous);
                    } else {
                        _ = self.binder_subst.remove(identity);
                    }
                },
            }
        }
        self.changes.shrinkRetainingCapacity(start);
    }

    fn addExpr(self: *Cloner, expr: Ast.Expr) Allocator.Error!Ast.ExprId {
        // Track emissions that carry an observable effect (host effects enter
        // through calls; low-level ops are data operations apart from the
        // crash op and the process-seed read). Pending bindings created after
        // such an emission must not move ahead of it.
        switch (expr.data) {
            .call_proc => |call| {
                const effect_free = switch (call.callee) {
                    .lifted => |fn_id| blk: {
                        const raw = @intFromEnum(fn_id);
                        break :blk raw < self.pass.fn_effect_free.len and self.pass.fn_effect_free[raw];
                    },
                    .func => false,
                };
                if (!effect_free) self.effect_marks += 1;
            },
            .call_value,
            .crash,
            .dbg,
            .expect,
            .expect_err,
            .comptime_exhaustiveness_failed,
            => self.effect_marks += 1,
            .low_level => |call| switch (call.op) {
                .crash, .dict_pseudo_seed => self.effect_marks += 1,
                else => {},
            },
            else => {},
        }
        const saved_loc = self.pass.program.current_loc;
        defer self.pass.program.current_loc = saved_loc;
        const saved_region = self.pass.program.current_region;
        defer self.pass.program.current_region = saved_region;
        self.pass.program.current_loc = self.current_loc;
        self.pass.program.current_region = self.current_region;
        return try self.pass.program.addExpr(expr);
    }

    fn addStmt(self: *Cloner, stmt: Ast.Stmt) Allocator.Error!Ast.StmtId {
        const saved_loc = self.pass.program.current_loc;
        defer self.pass.program.current_loc = saved_loc;
        const saved_region = self.pass.program.current_region;
        defer self.pass.program.current_region = saved_region;
        self.pass.program.current_loc = self.current_loc;
        self.pass.program.current_region = self.current_region;
        return try self.pass.program.addStmt(stmt);
    }
};

fn localExpr(program: *const Ast.Program, expr_id: Ast.ExprId) ?Ast.LocalId {
    return switch (program.getExpr(expr_id).data) {
        .local => |local| local,
        else => null,
    };
}

fn exprCallsFn(program: *const Ast.Program, expr_id: Ast.ExprId, fn_id: Ast.FnId) bool {
    return switch (program.getExpr(expr_id).data) {
        .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .crash, .comptime_exhaustiveness_failed, .uninitialized, .uninitialized_payload => false,
        .fn_ref => |fn_ref| captureOperandSpanCallsFn(program, fn_ref.captures, fn_id),
        .list, .tuple => |items| exprSpanCallsFn(program, items, fn_id),
        .record => |fields| blk: {
            const field_exprs = program.fieldExprSpan(fields);
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

fn exprMayCrash(program: *const Ast.Program, fn_may_crash: []const bool, expr_id: Ast.ExprId) bool {
    return switch (program.getExpr(expr_id).data) {
        .crash, .comptime_exhaustiveness_failed => true,
        .local,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .uninitialized,
        .uninitialized_payload,
        .def_ref,
        => false,
        .fn_ref => |fn_ref| captureOperandSpanMayCrash(program, fn_may_crash, fn_ref.captures),
        .list,
        .tuple,
        => |items| exprSpanMayCrash(program, fn_may_crash, items),
        .record => |fields| blk: {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                if (exprMayCrash(program, fn_may_crash, GuardedList.at(field_exprs, index).value)) break :blk true;
            }
            break :blk false;
        },
        .tag => |tag| exprSpanMayCrash(program, fn_may_crash, tag.payloads),
        .static_data_candidate => |candidate| exprMayCrash(program, fn_may_crash, candidate.runtime_expr),
        .nominal,
        .dbg,
        .expect,
        => |child| exprMayCrash(program, fn_may_crash, child),
        .return_ => |ret| exprMayCrash(program, fn_may_crash, ret.value),
        .expect_err => |expect_err| exprMayCrash(program, fn_may_crash, expect_err.msg),
        .comptime_branch_taken => |taken| exprMayCrash(program, fn_may_crash, taken.body),
        .let_ => |let_| exprMayCrash(program, fn_may_crash, let_.value) or
            exprMayCrash(program, fn_may_crash, let_.rest),
        .lambda,
        .fn_def,
        => false,
        .call_value => |call| exprMayCrash(program, fn_may_crash, call.callee) or
            exprSpanMayCrash(program, fn_may_crash, call.args),
        .call_proc => |call| blk: {
            if (Ast.localDirectCallee(call)) |callee| {
                const raw = @intFromEnum(callee);
                if (raw < fn_may_crash.len and fn_may_crash[raw]) break :blk true;
            }
            break :blk exprSpanMayCrash(program, fn_may_crash, call.args) or
                captureOperandSpanMayCrash(program, fn_may_crash, call.captures);
        },
        .low_level => |call| call.op == .crash or exprSpanMayCrash(program, fn_may_crash, call.args),
        .field_access => |field| exprMayCrash(program, fn_may_crash, field.receiver),
        .tuple_access => |access| exprMayCrash(program, fn_may_crash, access.tuple),
        .structural_eq => |eq| exprMayCrash(program, fn_may_crash, eq.lhs) or
            exprMayCrash(program, fn_may_crash, eq.rhs),
        .structural_hash => |h| exprMayCrash(program, fn_may_crash, h.value) or
            exprMayCrash(program, fn_may_crash, h.hasher),
        .match_ => |match| blk: {
            if (exprMayCrash(program, fn_may_crash, match.scrutinee)) break :blk true;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (branch.guard) |guard| {
                    if (exprMayCrash(program, fn_may_crash, guard)) break :blk true;
                }
                if (exprMayCrash(program, fn_may_crash, branch.body)) break :blk true;
            }
            break :blk false;
        },
        .if_ => |if_| blk: {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (exprMayCrash(program, fn_may_crash, branch.cond) or
                    exprMayCrash(program, fn_may_crash, branch.body)) break :blk true;
            }
            break :blk exprMayCrash(program, fn_may_crash, if_.final_else);
        },
        .block => |block| blk: {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                if (stmtMayCrash(program, fn_may_crash, GuardedList.at(statements, index))) break :blk true;
            }
            break :blk exprMayCrash(program, fn_may_crash, block.final_expr);
        },
        .loop_ => |loop| exprSpanMayCrash(program, fn_may_crash, loop.initial_values) or
            exprMayCrash(program, fn_may_crash, loop.body),
        .break_ => |maybe| if (maybe) |value| exprMayCrash(program, fn_may_crash, value) else false,
        .continue_ => |continue_| exprSpanMayCrash(program, fn_may_crash, continue_.values),
        .join_point => |join_point| exprMayCrash(program, fn_may_crash, join_point.body) or
            exprMayCrash(program, fn_may_crash, join_point.remainder),
        .jump => |jump| exprSpanMayCrash(program, fn_may_crash, jump.args),
        .if_initialized_payload => |payload_switch| exprMayCrash(program, fn_may_crash, payload_switch.cond) or
            exprMayCrash(program, fn_may_crash, payload_switch.initialized) or
            exprMayCrash(program, fn_may_crash, payload_switch.uninitialized),
        .try_sequence => |sequence| exprMayCrash(program, fn_may_crash, sequence.try_expr) or
            exprMayCrash(program, fn_may_crash, sequence.ok_body),
        .try_record_sequence => |sequence| exprMayCrash(program, fn_may_crash, sequence.try_expr) or
            exprMayCrash(program, fn_may_crash, sequence.ok_body),
    };
}

fn exprSpanMayCrash(program: *const Ast.Program, fn_may_crash: []const bool, span: Ast.Span(Ast.ExprId)) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        if (exprMayCrash(program, fn_may_crash, GuardedList.at(exprs, index))) return true;
    }
    return false;
}

fn captureOperandSpanMayCrash(program: *const Ast.Program, fn_may_crash: []const bool, span: Ast.Span(Ast.CaptureOperand)) bool {
    const operands = program.captureOperandSpan(span);
    for (0..operands.len) |index| {
        if (exprMayCrash(program, fn_may_crash, GuardedList.at(operands, index).value)) return true;
    }
    return false;
}

fn stmtMayCrash(program: *const Ast.Program, fn_may_crash: []const bool, stmt_id: Ast.StmtId) bool {
    return switch (program.getStmt(stmt_id)) {
        .crash => true,
        .let_ => |let_| exprMayCrash(program, fn_may_crash, let_.value),
        .expr,
        .expect,
        .dbg,
        => |expr| exprMayCrash(program, fn_may_crash, expr),
        .return_ => |ret| exprMayCrash(program, fn_may_crash, ret.value),
        .uninitialized => false,
    };
}

fn exprContainsReturn(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.getExpr(expr_id).data) {
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

fn localUseCountInExpr(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) usize {
    return switch (program.getExpr(expr_id).data) {
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

const LocalUseScan = struct {
    seen_effect: bool = false,
    found_before_effect: bool = false,
    found_after_effect: bool = false,
};

/// Whether evaluating an expression can produce an observable effect. Host
/// effects enter through procedure calls, so a call carries an effect unless
/// its target's whole body is effect-free; low-level ops are data operations
/// apart from the crash op and the process-seed read. Divergence through
/// checked arithmetic is not an effect: within one straight-line region a
/// crash commutes with pure evaluation.
/// `allow_control` distinguishes the two users: classifying a whole function
/// body tolerates control transfers (they stay inside the function), while a
/// value being discarded or moved must not carry one.
fn exprHasNoObservableEffect(program: *const Ast.Program, fn_effect_free: []const bool, expr_id: Ast.ExprId, allow_control: bool) bool {
    const expr = program.getExpr(expr_id);
    return switch (expr.data) {
        .local,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .uninitialized,
        .uninitialized_payload,
        .def_ref,
        .fn_def,
        => true,
        .fn_ref => |fn_ref| captureOperandSpanHasNoObservableEffect(program, fn_effect_free, fn_ref.captures, allow_control),
        .list,
        .tuple,
        => |items| exprSpanHasNoObservableEffect(program, fn_effect_free, items, allow_control),
        .record => |fields| blk: {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                if (!exprHasNoObservableEffect(program, fn_effect_free, field.value, allow_control)) break :blk false;
            }
            break :blk true;
        },
        .tag => |tag| exprSpanHasNoObservableEffect(program, fn_effect_free, tag.payloads, allow_control),
        .static_data_candidate => true,
        .nominal => |child| exprHasNoObservableEffect(program, fn_effect_free, child, allow_control),
        .field_access => |field| exprHasNoObservableEffect(program, fn_effect_free, field.receiver, allow_control),
        .tuple_access => |access| exprHasNoObservableEffect(program, fn_effect_free, access.tuple, allow_control),
        .structural_eq => |eq| exprHasNoObservableEffect(program, fn_effect_free, eq.lhs, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, eq.rhs, allow_control),
        .structural_hash => |h| exprHasNoObservableEffect(program, fn_effect_free, h.value, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, h.hasher, allow_control),
        .low_level => |call| switch (call.op) {
            .crash, .dict_pseudo_seed => false,
            else => exprSpanHasNoObservableEffect(program, fn_effect_free, call.args, allow_control),
        },
        .call_proc => |call| blk: {
            const callee = switch (call.callee) {
                .lifted => |fn_id| fn_id,
                .func => break :blk false,
            };
            const raw = @intFromEnum(callee);
            if (raw >= fn_effect_free.len or !fn_effect_free[raw]) break :blk false;
            break :blk exprSpanHasNoObservableEffect(program, fn_effect_free, call.args, allow_control) and
                captureOperandSpanHasNoObservableEffect(program, fn_effect_free, call.captures, allow_control);
        },
        .let_ => |let_| exprHasNoObservableEffect(program, fn_effect_free, let_.value, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, let_.rest, allow_control),
        .if_ => |if_| blk: {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (!exprHasNoObservableEffect(program, fn_effect_free, branch.cond, allow_control)) break :blk false;
                if (!exprHasNoObservableEffect(program, fn_effect_free, branch.body, allow_control)) break :blk false;
            }
            break :blk exprHasNoObservableEffect(program, fn_effect_free, if_.final_else, allow_control);
        },
        .match_ => |match| blk: {
            if (!exprHasNoObservableEffect(program, fn_effect_free, match.scrutinee, allow_control)) break :blk false;
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (branch.guard) |guard| {
                    if (!exprHasNoObservableEffect(program, fn_effect_free, guard, allow_control)) break :blk false;
                }
                if (!exprHasNoObservableEffect(program, fn_effect_free, branch.body, allow_control)) break :blk false;
            }
            break :blk true;
        },
        .block => |block| blk: {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                const stmt_id = GuardedList.at(statements, index);
                const no_effect = switch (program.getStmt(stmt_id)) {
                    .let_ => |let_| exprHasNoObservableEffect(program, fn_effect_free, let_.value, allow_control),
                    .expr => |stmt_expr| exprHasNoObservableEffect(program, fn_effect_free, stmt_expr, allow_control),
                    .uninitialized => true,
                    .return_ => |ret| allow_control and exprHasNoObservableEffect(program, fn_effect_free, ret.value, allow_control),
                    .expect, .dbg, .crash => false,
                };
                if (!no_effect) break :blk false;
            }
            break :blk exprHasNoObservableEffect(program, fn_effect_free, block.final_expr, allow_control);
        },
        // A loop contains its own back edges, so its body may transfer
        // control regardless of the caller's tolerance.
        .loop_ => |loop| exprSpanHasNoObservableEffect(program, fn_effect_free, loop.initial_values, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, loop.body, true),
        .break_ => |maybe| allow_control and
            (if (maybe) |value| exprHasNoObservableEffect(program, fn_effect_free, value, allow_control) else true),
        .continue_ => |continue_| allow_control and exprSpanHasNoObservableEffect(program, fn_effect_free, continue_.values, allow_control),
        .join_point => |join_point| exprHasNoObservableEffect(program, fn_effect_free, join_point.body, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, join_point.remainder, allow_control),
        .jump => |jump| exprSpanHasNoObservableEffect(program, fn_effect_free, jump.args, allow_control),
        .if_initialized_payload => |payload_switch| exprHasNoObservableEffect(program, fn_effect_free, payload_switch.cond, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, payload_switch.initialized, allow_control) and
            exprHasNoObservableEffect(program, fn_effect_free, payload_switch.uninitialized, allow_control),
        .comptime_branch_taken => |taken| exprHasNoObservableEffect(program, fn_effect_free, taken.body, allow_control),
        .return_ => |ret| allow_control and exprHasNoObservableEffect(program, fn_effect_free, ret.value, allow_control),
        .lambda,
        .call_value,
        .crash,
        .dbg,
        .expect,
        .expect_err,
        .comptime_exhaustiveness_failed,
        .try_sequence,
        .try_record_sequence,
        => false,
    };
}

fn exprSpanHasNoObservableEffect(program: *const Ast.Program, fn_effect_free: []const bool, span: Ast.Span(Ast.ExprId), allow_control: bool) bool {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        const expr = GuardedList.at(exprs, index);
        if (!exprHasNoObservableEffect(program, fn_effect_free, expr, allow_control)) return false;
    }
    return true;
}

fn captureOperandSpanHasNoObservableEffect(program: *const Ast.Program, fn_effect_free: []const bool, span: Ast.Span(Ast.CaptureOperand), allow_control: bool) bool {
    const operands = program.captureOperandSpan(span);
    for (0..operands.len) |index| {
        const operand = GuardedList.at(operands, index);
        if (!exprHasNoObservableEffect(program, fn_effect_free, operand.value, allow_control)) return false;
    }
    return true;
}

fn localUseBeforeEffect(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) bool {
    var scan: LocalUseScan = .{};
    scanLocalUseInExpr(program, local, expr_id, &scan);
    return scan.found_before_effect and !scan.found_after_effect;
}

fn scanLocalUseInExpr(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId, scan: *LocalUseScan) void {
    const expr = program.getExpr(expr_id);
    switch (expr.data) {
        .local => |seen| {
            if (seen == local) {
                if (scan.seen_effect) {
                    scan.found_after_effect = true;
                } else {
                    scan.found_before_effect = true;
                }
            }
        },
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .uninitialized,
        .uninitialized_payload,
        => {},
        .fn_ref => |fn_ref| scanLocalUseInCaptureOperandSpan(program, local, fn_ref.captures, scan),
        .crash, .comptime_exhaustiveness_failed => scan.seen_effect = true,
        .list,
        .tuple,
        => |items| scanLocalUseInExprSpan(program, local, items, scan),
        .record => |fields| {
            const field_exprs = program.fieldExprSpan(fields);
            for (0..field_exprs.len) |index| {
                const field = GuardedList.at(field_exprs, index);
                scanLocalUseInExpr(program, local, field.value, scan);
            }
        },
        .tag => |tag| scanLocalUseInExprSpan(program, local, tag.payloads, scan),
        .static_data_candidate => |candidate| scanLocalUseInExpr(program, local, candidate.runtime_expr, scan),
        .nominal => |child| scanLocalUseInExpr(program, local, child, scan),
        .return_ => |ret| {
            scanLocalUseInExpr(program, local, ret.value, scan);
            scan.seen_effect = true;
        },
        .dbg,
        .expect,
        => |child| {
            scanLocalUseInExpr(program, local, child, scan);
            scan.seen_effect = true;
        },
        .expect_err => |expect_err| {
            scanLocalUseInExpr(program, local, expect_err.msg, scan);
            scan.seen_effect = true;
        },
        .comptime_branch_taken => |taken| scanLocalUseInExpr(program, local, taken.body, scan),
        .let_ => |let_| {
            scanLocalUseInExpr(program, local, let_.value, scan);
            scanLocalUseInExpr(program, local, let_.rest, scan);
        },
        .lambda,
        .def_ref,
        .fn_def,
        => {},
        .call_value => |call| {
            scanLocalUseInExpr(program, local, call.callee, scan);
            scanLocalUseInExprSpan(program, local, call.args, scan);
            scan.seen_effect = true;
        },
        .call_proc => |call| {
            scanLocalUseInExprSpan(program, local, call.args, scan);
            scanLocalUseInCaptureOperandSpan(program, local, call.captures, scan);
            scan.seen_effect = true;
        },
        .low_level => |call| {
            scanLocalUseInExprSpan(program, local, call.args, scan);
            scan.seen_effect = true;
        },
        .field_access => |field| scanLocalUseInExpr(program, local, field.receiver, scan),
        .tuple_access => |access| scanLocalUseInExpr(program, local, access.tuple, scan),
        .structural_eq => |eq| {
            scanLocalUseInExpr(program, local, eq.lhs, scan);
            scanLocalUseInExpr(program, local, eq.rhs, scan);
            scan.seen_effect = true;
        },
        .structural_hash => |h| {
            scanLocalUseInExpr(program, local, h.value, scan);
            scanLocalUseInExpr(program, local, h.hasher, scan);
            scan.seen_effect = true;
        },
        .match_ => |match| {
            scanLocalUseInExpr(program, local, match.scrutinee, scan);
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                var branch_scan = scan.*;
                if (branch.guard) |guard| scanLocalUseInExpr(program, local, guard, &branch_scan);
                scanLocalUseInExpr(program, local, branch.body, &branch_scan);
                scan.found_before_effect = scan.found_before_effect or branch_scan.found_before_effect;
                scan.found_after_effect = scan.found_after_effect or branch_scan.found_after_effect;
                scan.seen_effect = scan.seen_effect or branch_scan.seen_effect;
            }
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                scanLocalUseInExpr(program, local, branch.cond, scan);
                var branch_scan = scan.*;
                scanLocalUseInExpr(program, local, branch.body, &branch_scan);
                scan.found_before_effect = scan.found_before_effect or branch_scan.found_before_effect;
                scan.found_after_effect = scan.found_after_effect or branch_scan.found_after_effect;
                scan.seen_effect = scan.seen_effect or branch_scan.seen_effect;
            }
            scanLocalUseInExpr(program, local, if_.final_else, scan);
        },
        .block => |block| {
            const statements = program.stmtSpan(block.statements);
            for (0..statements.len) |index| {
                const stmt = GuardedList.at(statements, index);
                scanLocalUseInStmt(program, local, stmt, scan);
            }
            scanLocalUseInExpr(program, local, block.final_expr, scan);
        },
        .loop_ => |loop| {
            scanLocalUseInExprSpan(program, local, loop.initial_values, scan);
            scanLocalUseInExpr(program, local, loop.body, scan);
        },
        .break_ => |maybe| {
            if (maybe) |value| scanLocalUseInExpr(program, local, value, scan);
            scan.seen_effect = true;
        },
        .continue_ => |continue_| {
            scanLocalUseInExprSpan(program, local, continue_.values, scan);
            scan.seen_effect = true;
        },
        .join_point => |join_point| {
            var body_scan = scan.*;
            scanLocalUseInExpr(program, local, join_point.body, &body_scan);
            var remainder_scan = scan.*;
            scanLocalUseInExpr(program, local, join_point.remainder, &remainder_scan);
            scan.found_before_effect = scan.found_before_effect or body_scan.found_before_effect or remainder_scan.found_before_effect;
            scan.found_after_effect = scan.found_after_effect or body_scan.found_after_effect or remainder_scan.found_after_effect;
            scan.seen_effect = scan.seen_effect or body_scan.seen_effect or remainder_scan.seen_effect;
        },
        .jump => |jump| {
            scanLocalUseInExprSpan(program, local, jump.args, scan);
            scan.seen_effect = true;
        },
        .if_initialized_payload => |payload_switch| {
            scanLocalUseInExpr(program, local, payload_switch.cond, scan);

            var initialized_scan = scan.*;
            if (payload_switch.payload == local) {
                if (initialized_scan.seen_effect) {
                    initialized_scan.found_after_effect = true;
                } else {
                    initialized_scan.found_before_effect = true;
                }
            }
            scanLocalUseInExpr(program, local, payload_switch.initialized, &initialized_scan);

            var uninitialized_scan = scan.*;
            scanLocalUseInExpr(program, local, payload_switch.uninitialized, &uninitialized_scan);

            scan.found_before_effect = scan.found_before_effect or initialized_scan.found_before_effect or uninitialized_scan.found_before_effect;
            scan.found_after_effect = scan.found_after_effect or initialized_scan.found_after_effect or uninitialized_scan.found_after_effect;
            scan.seen_effect = scan.seen_effect or initialized_scan.seen_effect or uninitialized_scan.seen_effect;
        },
        .try_sequence => |sequence| {
            scanLocalUseInExpr(program, local, sequence.try_expr, scan);
            if (sequence.ok_local != local) {
                scanLocalUseInExpr(program, local, sequence.ok_body, scan);
            }
            scan.seen_effect = true;
        },
        .try_record_sequence => |sequence| {
            scanLocalUseInExpr(program, local, sequence.try_expr, scan);
            if (sequence.value_local != local and sequence.rest_local != local) {
                scanLocalUseInExpr(program, local, sequence.ok_body, scan);
            }
            scan.seen_effect = true;
        },
    }
}

fn scanLocalUseInExprSpan(
    program: *const Ast.Program,
    local: Ast.LocalId,
    span: Ast.Span(Ast.ExprId),
    scan: *LocalUseScan,
) void {
    const exprs = program.exprSpan(span);
    for (0..exprs.len) |index| {
        const expr = GuardedList.at(exprs, index);
        scanLocalUseInExpr(program, local, expr, scan);
    }
}

fn scanLocalUseInCaptureOperandSpan(
    program: *const Ast.Program,
    local: Ast.LocalId,
    span: Ast.Span(Ast.CaptureOperand),
    scan: *LocalUseScan,
) void {
    const operands = program.captureOperandSpan(span);
    for (0..GuardedList.borrowLen(operands)) |index| {
        const operand = GuardedList.at(operands, index);
        scanLocalUseInExpr(program, local, operand.value, scan);
    }
}

fn scanLocalUseInStmt(program: *const Ast.Program, local: Ast.LocalId, stmt_id: Ast.StmtId, scan: *LocalUseScan) void {
    switch (program.getStmt(stmt_id)) {
        .uninitialized => {},
        .let_ => |let_| scanLocalUseInExpr(program, local, let_.value, scan),
        .expr => |expr| scanLocalUseInExpr(program, local, expr, scan),
        .expect,
        .dbg,
        => |expr| {
            scanLocalUseInExpr(program, local, expr, scan);
            scan.seen_effect = true;
        },
        .return_ => |ret| {
            scanLocalUseInExpr(program, local, ret.value, scan);
            scan.seen_effect = true;
        },
        .crash => scan.seen_effect = true,
    }
}

fn canReadFieldsFromExpr(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.getExpr(expr_id).data) {
        .local,
        .field_access,
        .tuple_access,
        => true,
        else => false,
    };
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
/// nominal type — Monotype lowering wraps every such construction in
/// explicit `.nominal` nodes, and the static matcher relies on pattern and
/// value representations aligning exactly.
fn assertStructuralConstructionType(program: *const Ast.Program, ty: Type.TypeId) void {
    if (!std.debug.runtime_safety) return;
    var current = ty;
    while (true) {
        switch (program.types.get(current)) {
            .named => |named| {
                const backing = named.backing orelse return;
                switch (named.kind) {
                    .alias => current = backing.ty,
                    .nominal, .@"opaque" => Common.invariant("structural constructor value was typed at a nominal type without its nominal wrapper"),
                }
            },
            else => return,
        }
    }
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

fn shapeMatchesValue(program: *const Ast.Program, shape: Shape, value: Value) bool {
    const structural_value = switch (value) {
        .static_data_candidate => |candidate| candidate.runtime.*,
        else => value,
    };
    return switch (shape) {
        .any => true,
        .tag => |tag| blk: {
            const value_tag = switch (structural_value) {
                .tag => |value_tag| value_tag,
                else => break :blk false,
            };
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
            const value_record = switch (structural_value) {
                .record => |value_record| value_record,
                else => break :blk false,
            };
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
            const value_tuple = switch (structural_value) {
                .tuple => |value_tuple| value_tuple,
                else => break :blk false,
            };
            if (!sameType(program, tuple.ty, value_tuple.ty) or tuple.items.len != value_tuple.items.len) break :blk false;
            for (tuple.items, value_tuple.items) |item_shape, item_value| {
                if (!shapeMatchesValue(program, item_shape, item_value)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |nominal| blk: {
            const value_nominal = switch (structural_value) {
                .nominal => |value_nominal| value_nominal,
                else => break :blk false,
            };
            break :blk sameType(program, nominal.ty, value_nominal.ty) and shapeMatchesValue(program, nominal.backing.*, value_nominal.backing.*);
        },
        .callable => |callable| blk: {
            const value_callable = switch (structural_value) {
                .callable => |value_callable| value_callable,
                else => break :blk false,
            };
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

fn fieldFromValue(program: *const Ast.Program, value: Value, name: names.RecordFieldNameId) ?Value {
    return switch (value) {
        .static_data_candidate => |candidate| fieldFromValue(program, candidate.runtime.*, name),
        .record => |record| fieldFromRecord(program, record, name),
        .nominal => |nominal| fieldFromValue(program, nominal.backing.*, name),
        else => null,
    };
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
    return switch (value) {
        .static_data_candidate => |candidate| itemFromValue(candidate.runtime.*, index),
        .tuple => |tuple| if (index < tuple.items.len) tuple.items[index] else null,
        .nominal => |nominal| itemFromValue(nominal.backing.*, index),
        else => null,
    };
}

fn tagFromValue(value: Value) ?TagValue {
    return switch (value) {
        .static_data_candidate => |candidate| tagFromValue(candidate.runtime.*),
        .tag => |tag| tag,
        .nominal => |nominal| tagFromValue(nominal.backing.*),
        else => null,
    };
}

fn recordFromValue(value: Value) ?RecordValue {
    return switch (value) {
        .static_data_candidate => |candidate| recordFromValue(candidate.runtime.*),
        .record => |record| record,
        .nominal => |nominal| recordFromValue(nominal.backing.*),
        else => null,
    };
}

fn tupleFromValue(value: Value) ?TupleValue {
    return switch (value) {
        .static_data_candidate => |candidate| tupleFromValue(candidate.runtime.*),
        .tuple => |tuple| tuple,
        .nominal => |nominal| tupleFromValue(nominal.backing.*),
        else => null,
    };
}

fn emptyLiftedProgramForTest(allocator: Allocator) Ast.Program {
    return Ast.Program.init(
        allocator,
        names.NameStore.init(allocator),
        Type.Store.init(allocator),
        .empty, // imported_fns
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // fn_def_captures
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
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
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
    try std.testing.expect(localUseBeforeEffect(&program, local, fn_ref));
    try std.testing.expect(exprContainsReturn(&program, call_proc));
    try std.testing.expectEqual(@as(usize, 1), localUseCountInExpr(&program, local, call_proc));
    try std.testing.expect(localUseBeforeEffect(&program, local, call_proc));
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

    const value = try cloner.callableValueFromRef(fn_ty, .{
        .fn_id = outer_fn,
        .captures = outer_operands,
    });
    const callable = switch (value) {
        .callable => |callable| callable,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(@as(usize, 2), callable.captures.len);
    try std.testing.expectEqual(first_id, callable.captures[0].id);
    try std.testing.expectEqual(second_id, callable.captures[1].id);
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

    const call = switch (lifted.getExpr(body).data) {
        .call_proc => |call| call,
        else => return error.TestUnexpectedResult,
    };
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

    const first_change = cloner.changes.items.len;
    const first_pat = try cloner.clonePat(source_pat, .bind_runtime);
    const first_local = switch (program.getPat(first_pat).data) {
        .bind => |local| local,
        else => return error.TestUnexpectedResult,
    };
    const first_ref = try cloner.cloneExpr(source_ref);
    try std.testing.expectEqual(first_local, program.getExpr(first_ref).data.local);
    const first_payload_ref = try cloner.cloneExpr(source_payload_ref);
    try std.testing.expectEqual(first_local, program.getExpr(first_payload_ref).data.uninitialized_payload.condition);
    cloner.restore(first_change);

    const second_change = cloner.changes.items.len;
    const second_pat = try cloner.clonePat(source_pat, .bind_runtime);
    const second_local = switch (program.getPat(second_pat).data) {
        .bind => |local| local,
        else => return error.TestUnexpectedResult,
    };
    const second_ref = try cloner.cloneExpr(source_ref);
    try std.testing.expectEqual(second_local, program.getExpr(second_ref).data.local);
    cloner.restore(second_change);

    try std.testing.expect(source_local != first_local);
    try std.testing.expect(source_local != second_local);
    try std.testing.expect(first_local != second_local);

    const known_local = try program.addLocal(@enumFromInt(2), u8_ty);
    const known_ref = try program.addExpr(.{ .ty = u8_ty, .data = .{ .local = known_local } });
    const known_change = cloner.changes.items.len;
    try cloner.putSubst(source_local, .{ .expr = known_ref });
    const output_pat = try cloner.clonePat(source_pat, .output_only);
    const output_local = switch (program.getPat(output_pat).data) {
        .bind => |local| local,
        else => return error.TestUnexpectedResult,
    };
    const substituted_ref = try cloner.cloneExpr(source_ref);
    try std.testing.expectEqual(known_local, program.getExpr(substituted_ref).data.local);
    try std.testing.expect(output_local != source_local);
    try std.testing.expect(output_local != known_local);
    cloner.restore(known_change);
}

test "known match fold aborts on undecidable branches and trips the invariant when every branch is excluded" {
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

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
    try std.testing.expectEqual(@as(?Value, null), try cloner.simplifyKnownMatchValue(foo_value, undecidable_branches));

    // A definite match after definite no-matches folds.
    const folding_branches = try program.addBranchSpan(&.{
        .{ .pat = bar_pat, .body = body },
        .{ .pat = foo_pat, .body = body },
    });
    try std.testing.expect((try cloner.simplifyKnownMatchValue(foo_value, folding_branches)) != null);

    // Every branch a definite no-match violates checker exhaustiveness: the
    // invariant must fire. The panic aborts, so probe it from a fork.
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
        _ = cloner.simplifyKnownMatchValue(foo_value, excluded_branches) catch std.c._exit(2);
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

test "call-pattern specialization declarations are referenced" {
    std.testing.refAllDecls(@This());
}
