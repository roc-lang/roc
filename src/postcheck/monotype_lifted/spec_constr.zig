//! Make calls cheaper when they pass values with known values to code that
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
//! the same known_value as this Roc code. The range is wrapped in a stream record; map
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
//! In that inlined form, the loop state `$rest` has a known constructor known_value:
//! it is a `Stream` record whose `step!` field is the lifted function created by
//! `Stream.map`, with captures for the source step thunk and the mapping
//! function. Each `One` or `Skip` branch constructs the same mapped stream known_value
//! for the next iteration. Without this pass, the compiler lowers that as a loop
//! over a single stream value, repacking stream fields and rebuilding the step
//! closure before immediately reading them again.
//!
//! This pass specializes the collect worker for the known stream known_value. Written
//! in pure Roc terms, the optimized known_value is:
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
//! The implementation has four parts:
//!
//! 1. Scan original lifted functions and mark argument positions read by
//!    `match`, field access, or tuple access. Direct calls propagate those marks
//!    to the caller's corresponding arguments.
//! 2. Rewrite each original Roc body with the same value-environment clone used
//!    for workers, while preserving that original function's ABI. This base-body
//!    rewrite can specialize local loop state even when the function was called
//!    with only primitive arguments.
//! 3. While cloning a base body or worker, record direct-call patterns as soon as
//!    known-value arguments reach a callee that reads them. Recording a pattern
//!    immediately reserves a worker id and pushes a worker job.
//! 4. Drain the worker worklist by cloning each source body into the reserved
//!    worker. Known constructor arguments are split into leaves; ordinary
//!    arguments stay as normal worker arguments. Calls matching a recorded
//!    pattern are redirected during the containing clone, so there is no later
//!    cleanup walk over already-written bodies.
//!
//! Cloning with a value environment is where the simplifications happen: known
//! records simplify field reads, known tuples simplify tuple reads, known tags
//! simplify matches, known callable values inline direct calls, and loop state is
//! split when every `continue` value can provide the same leaves.
//!
//! Callable identity is part of a call pattern. A lifted callable matches only
//! the same function id, or a specialized clone whose stored source function
//! template is the same. That keeps dispatch static while allowing this pass's
//! own callable workers to match the patterns that created them.

const std = @import("std");

const SourceLoc = @import("base").SourceLoc;
const Region = @import("base").Region;
const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const can = @import("can");
const Mono = @import("../monotype/ast.zig");
const Type = @import("../monotype/type.zig");
const Solved = @import("../lambda_solved/ast.zig");
const SolvedType = @import("../lambda_solved/type.zig");
const check = @import("check");
const names = check.CheckedNames;

const Allocator = std.mem.Allocator;

/// Specialize recursive direct calls whose arguments are known constructor known_values.
pub fn run(allocator: Allocator, program: *Ast.Program) Common.LowerError!void {
    var pass = try Pass.init(allocator, program, null);
    defer pass.deinit();
    try pass.run();
}

/// Specialize with Lambda Solved type data available for checked-call known_values.
pub fn runWithSolved(allocator: Allocator, solved: *Solved.Program) Common.LowerError!void {
    var pass = try Pass.init(allocator, &solved.lifted, solved);
    defer pass.deinit();
    try pass.run();
}

const KnownValue = union(enum) {
    any: Type.TypeId,
    leaf: Type.TypeId,
    tag: KnownTag,
    record: KnownRecord,
    tuple: KnownTuple,
    nominal: KnownNominal,
    callable: KnownCallable,
    finite_tags: KnownTags,
    finite_callables: KnownCallables,
};

const KnownTag = struct {
    ty: Type.TypeId,
    name: names.TagNameId,
    payloads: []const KnownValue,
};

const KnownField = struct {
    name: names.RecordFieldNameId,
    known_value: KnownValue,
};

const KnownRecord = struct {
    ty: Type.TypeId,
    fields: []const KnownField,
};

const KnownTuple = struct {
    ty: Type.TypeId,
    items: []const KnownValue,
};

const KnownNominal = struct {
    ty: Type.TypeId,
    backing: *const KnownValue,
};

const KnownCallable = struct {
    ty: Type.TypeId,
    fn_id: Ast.FnId,
    captures: []const KnownValue,
};

const KnownTags = struct {
    ty: Type.TypeId,
    alternatives: []const KnownTag,
};

const KnownCallables = struct {
    ty: Type.TypeId,
    alternatives: []const KnownCallable,
};

const DemandedKnownValue = union(enum) {
    any: Type.TypeId,
    leaf: Type.TypeId,
    tag: DemandedKnownTag,
    record: DemandedKnownRecord,
    tuple: DemandedKnownTuple,
    nominal: DemandedKnownNominal,
    callable: DemandedKnownCallable,
    finite_tags: DemandedKnownTags,
    finite_callables: DemandedKnownCallables,
};

const DemandedKnownTag = struct {
    ty: Type.TypeId,
    name: names.TagNameId,
    payloads: []const DemandedKnownIndexedValue,
};

const DemandedKnownField = struct {
    name: names.RecordFieldNameId,
    known_value: DemandedKnownValue,
};

const DemandedKnownRecord = struct {
    ty: Type.TypeId,
    fields: []const DemandedKnownField,
};

const DemandedKnownTuple = struct {
    ty: Type.TypeId,
    items: []const DemandedKnownIndexedValue,
};

const DemandedKnownNominal = struct {
    ty: Type.TypeId,
    backing: ?*const DemandedKnownValue,
};

const DemandedKnownCallable = struct {
    ty: Type.TypeId,
    fn_id: Ast.FnId,
    captures: []const DemandedKnownIndexedValue,
};

const DemandedKnownTags = struct {
    ty: Type.TypeId,
    alternatives: []const DemandedKnownTag,
};

const DemandedKnownCallables = struct {
    ty: Type.TypeId,
    alternatives: []const DemandedKnownCallable,
};

const DemandedKnownIndexedValue = struct {
    index: u32,
    known_value: DemandedKnownValue,
};

const KnownMatchMode = enum {
    strict,
    speculative,
};

const Value = union(enum) {
    expr: Ast.ExprId,
    expr_with_known_value: ExprWithKnownValue,
    let_: LetValue,
    if_: IfValue,
    tag: TagValue,
    record: RecordValue,
    tuple: TupleValue,
    nominal: NominalValue,
    callable: CallableValue,
    finite_tags: FiniteTagsValue,
    finite_callables: FiniteCallablesValue,
};

const ExprWithKnownValue = struct {
    expr: Ast.ExprId,
    known_value: KnownValue,
};

const LetValue = struct {
    lets: []const PendingLet,
    body: *const Value,
};

const IfValueBranch = struct {
    cond: Ast.ExprId,
    body: Value,
};

const IfValue = struct {
    ty: Type.TypeId,
    branches: []const IfValueBranch,
    final_else: *const Value,
};

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

const CallableValue = struct {
    ty: Type.TypeId,
    fn_id: Ast.FnId,
    captures: []const Value,
};

const FiniteTagsValue = struct {
    ty: Type.TypeId,
    selector: Ast.ExprId,
    alternatives: []const TagValue,
};

const FiniteCallablesValue = struct {
    ty: Type.TypeId,
    selector: Ast.ExprId,
    alternatives: []const CallableValue,
};

const CallPattern = struct {
    args: []const KnownValue,
};

const ValueDemand = union(enum) {
    none,
    materialize,
    record: []const FieldDemand,
    tuple: []const ItemDemand,
    nominal: *const ValueDemand,
    callable: CallableDemand,
};

const FieldDemand = struct {
    name: names.RecordFieldNameId,
    demand: *const ValueDemand,
};

const ItemDemand = struct {
    index: u32,
    demand: *const ValueDemand,
};

const CallableDemand = struct {
    captures: []const ValueDemand,
};

const Spec = struct {
    pattern: CallPattern,
    fn_id: ?Ast.FnId = null,
    written: bool = false,
};

const FnPlan = struct {
    used_args: []bool,
    arg_demands: []ValueDemand,
    specs: std.ArrayList(Spec),

    fn deinit(self: *FnPlan, allocator: Allocator) void {
        allocator.free(self.arg_demands);
        allocator.free(self.used_args);
        self.specs.deinit(allocator);
    }
};

const WorkerJob = struct {
    source_fn: Ast.FnId,
    spec_index: usize,
};

const CallableSpecialization = struct {
    source_fn: Ast.FnId,
    captures: []const KnownValue,
    fn_id: Ast.FnId,
};

const BindingTarget = union(enum) {
    local: Ast.LocalId,
};

const BindingChange = struct {
    key: BindingTarget,
    previous: ?Value,
};

const PendingLet = struct {
    local: Ast.LocalId,
    ty: Type.TypeId,
    value: Ast.ExprId,
    known_value: ?KnownValue = null,
};

const BlockTail = struct {
    statements: []const Ast.StmtId,
    final_expr: Ast.ExprId,
};

const LoopPattern = struct {
    values: []const KnownValue,
    refinements: []?KnownValue,
};

const StateLoopPattern = struct {
    states: []const StateLoopKnownState,
};

const StateLoopKnownState = struct {
    id: Ast.StateLoopStateId,
    values: []const KnownValue,
};

const ActiveInline = struct {
    fn_id: Ast.FnId,
    args: ?[]const KnownValue = null,
};

const Pass = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    program: *Ast.Program,
    solved: ?*const Solved.Program,
    plans: []FnPlan,
    original_bodies: []const ?Ast.ExprId,
    worker_worklist: std.ArrayList(WorkerJob),
    callable_specializations: std.ArrayList(CallableSpecialization),
    symbols: Common.SymbolGen,

    fn init(allocator: Allocator, program: *Ast.Program, solved: ?*const Solved.Program) Allocator.Error!Pass {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        const plans = try allocator.alloc(FnPlan, program.fns.items.len);
        errdefer allocator.free(plans);

        for (plans, 0..) |*plan, index| {
            const fn_ = program.fns.items[index];
            const args = program.typedLocalSpan(fn_.args);
            const used_args = try allocator.alloc(bool, args.len);
            errdefer allocator.free(used_args);
            @memset(used_args, false);
            const arg_demands = try allocator.alloc(ValueDemand, args.len);
            errdefer allocator.free(arg_demands);
            @memset(arg_demands, .none);
            plan.* = .{
                .used_args = used_args,
                .arg_demands = arg_demands,
                .specs = .empty,
            };
        }

        return .{
            .allocator = allocator,
            .arena = arena,
            .program = program,
            .solved = solved,
            .plans = plans,
            .original_bodies = &.{},
            .worker_worklist = .empty,
            .callable_specializations = .empty,
            .symbols = .{ .next = program.next_symbol },
        };
    }

    fn deinit(self: *Pass) void {
        self.callable_specializations.deinit(self.allocator);
        self.worker_worklist.deinit(self.allocator);
        for (self.plans) |*plan| plan.deinit(self.allocator);
        self.allocator.free(self.plans);
        self.arena.deinit();
    }

    fn solvedSingleCallableMember(self: *const Pass, expr_id: Ast.ExprId) ?SolvedType.FnMember {
        const solved = self.solved orelse return null;
        const raw = @intFromEnum(expr_id);
        if (raw >= solved.expr_tys.items.len) return null;
        return self.solvedSingleCallableMemberFromType(solved.expr_tys.items[raw]);
    }

    fn solvedSingleCallableMemberFromType(self: *const Pass, ty: SolvedType.TypeVarId) ?SolvedType.FnMember {
        const solved = self.solved orelse return null;
        const callable_ty = switch (solved.types.rootContent(ty)) {
            .func => |func| func.callable,
            .lambda_set => ty,
            else => return null,
        };
        const members = switch (solved.types.rootContent(callable_ty)) {
            .lambda_set => |members| members,
            else => return null,
        };
        const member_items = solved.types.memberSpan(members);
        if (member_items.len == 0) return null;
        if (member_items.len != 1 and !self.solvedCallableMembersAreEquivalent(member_items)) return null;
        return member_items[0];
    }

    fn solvedCallableMembersAreEquivalent(self: *const Pass, members: []const SolvedType.FnMember) bool {
        if (members.len <= 1) return true;

        const first_fn_id = self.fnWithSymbol(members[0].lambda) orelse return false;
        const solved = self.solved orelse return false;
        const first_captures = solved.types.captureSpan(members[0].captures);

        for (members[1..]) |member| {
            const fn_id = self.fnWithSymbol(member.lambda) orelse return false;
            if (!callableTargetMatches(self.program, first_fn_id, fn_id)) return false;

            const captures = solved.types.captureSpan(member.captures);
            if (captures.len != first_captures.len) return false;
            for (first_captures, captures) |first_capture, capture| {
                if (first_capture.local != capture.local) return false;
            }
        }

        return true;
    }

    fn fnWithSymbol(self: *const Pass, symbol: Common.Symbol) ?Ast.FnId {
        for (self.program.fns.items, 0..) |fn_, index| {
            if (fn_.symbol == symbol) return @enumFromInt(@as(u32, @intCast(index)));
        }
        return null;
    }

    fn primitiveType(self: *Pass, primitive: Type.Primitive) Allocator.Error!Type.TypeId {
        return try self.program.types.add(.{ .primitive = primitive });
    }

    fn run(self: *Pass) Common.LowerError!void {
        const original_fn_count = self.plans.len;
        const original_bodies = try self.captureOriginalBodies(original_fn_count);
        defer self.allocator.free(original_bodies);
        self.original_bodies = original_bodies;

        try self.collectArgUses(original_fn_count);
        try self.rewriteBaseBodies(original_bodies);
        try self.createSpecializations(original_bodies);

        self.original_bodies = &.{};
        self.program.next_symbol = self.symbols.next;
    }

    fn originalBody(self: *const Pass, fn_id: Ast.FnId) ?Ast.ExprId {
        const index = @intFromEnum(fn_id);
        if (index >= self.original_bodies.len) return null;
        return self.original_bodies[index];
    }

    fn copyProcDebugName(self: *Pass, source_symbol: Common.Symbol, target_symbol: Common.Symbol) Allocator.Error!void {
        if (self.program.procDebugName(source_symbol)) |name| {
            try self.program.setProcDebugName(target_symbol, name);
        }
    }

    fn captureOriginalBodies(self: *Pass, original_fn_count: usize) Allocator.Error![]?Ast.ExprId {
        const original_bodies = try self.allocator.alloc(?Ast.ExprId, original_fn_count);
        for (self.program.fns.items[0..original_fn_count], original_bodies) |fn_, *body_slot| {
            body_slot.* = switch (fn_.body) {
                .roc => |body| body,
                .hosted => null,
            };
        }
        return original_bodies;
    }

    fn collectArgUses(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.program.fns.items[0..original_fn_count], 0..) |fn_, index| {
                const body = switch (fn_.body) {
                    .roc => |body| body,
                    .hosted => continue,
                };
                const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
                try self.markArgUsesInExpr(fn_id, body, &changed);
            }
        }
    }

    fn rewriteBaseBodies(self: *Pass, original_bodies: []const ?Ast.ExprId) Common.LowerError!void {
        for (original_bodies, 0..) |maybe_body, index| {
            const body_expr = maybe_body orelse continue;
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));

            var cloner = Cloner.initForBaseBody(self, fn_id);
            defer cloner.deinit();

            try cloner.inline_stack.append(self.allocator, .{ .fn_id = fn_id });
            defer {
                const popped = cloner.inline_stack.pop() orelse Common.invariant("base body inline stack underflow");
                if (popped.fn_id != fn_id) Common.invariant("base body inline stack was corrupted");
            }

            const cloned_body = try cloner.cloneExpr(body_expr);
            self.program.fns.items[index].body = .{ .roc = cloned_body };
        }
    }

    fn createSpecializations(self: *Pass, original_bodies: []const ?Ast.ExprId) Common.LowerError!void {
        while (self.worker_worklist.pop()) |job| {
            const source_index = @intFromEnum(job.source_fn);
            const source_body = original_bodies[source_index] orelse
                Common.invariant("hosted function had a call-pattern specialization");
            if (self.plans[source_index].specs.items[job.spec_index].written) continue;

            self.plans[source_index].specs.items[job.spec_index].written = true;
            try self.writeSpecialization(job.source_fn, job.spec_index, source_body);
        }
    }

    fn markArgUsesInExpr(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId, changed: *bool) Allocator.Error!void {
        const expr = self.program.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .fn_ref,
            .crash,
            .comptime_exhaustiveness_failed,
            .uninitialized,
            .uninitialized_payload,
            => {},
            .static_data_candidate => |candidate| try self.markArgUsesInExpr(fn_id, candidate.fallback, changed),
            .list,
            .tuple,
            => |items| for (self.program.exprSpan(items)) |child| try self.markArgUsesInExpr(fn_id, child, changed),
            .record => |fields| for (self.program.fieldExprSpan(fields)) |field| try self.markArgUsesInExpr(fn_id, field.value, changed),
            .tag => |tag| for (self.program.exprSpan(tag.payloads)) |payload| try self.markArgUsesInExpr(fn_id, payload, changed),
            .nominal,
            .return_,
            .dbg,
            .expect,
            => |child| try self.markArgUsesInExpr(fn_id, child, changed),
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
                for (self.program.exprSpan(call.args)) |arg| try self.markArgUsesInExpr(fn_id, arg, changed);
            },
            .call_proc => |call| {
                const args = self.program.exprSpan(call.args);
                for (args) |arg| try self.markArgUsesInExpr(fn_id, arg, changed);
                const callee = Ast.callProcCallee(call);
                const callee_raw = @intFromEnum(callee);
                if (callee_raw < self.plans.len) {
                    const callee_uses = self.plans[callee_raw].used_args;
                    if (args.len != callee_uses.len) Common.invariant("direct call arity differed from lifted function arity while propagating argument uses");
                    const callee_demands = self.plans[callee_raw].arg_demands;
                    for (args, callee_uses, callee_demands) |arg, callee_uses_arg, callee_demand| {
                        if (callee_uses_arg) try self.markArgDemandIfLocal(fn_id, arg, callee_demand, changed);
                    }
                }
            },
            .low_level => |call| {
                for (self.program.exprSpan(call.args)) |arg| try self.markArgUsesInExpr(fn_id, arg, changed);
            },
            .field_access => |field| {
                try self.markArgDemandIfLocal(
                    fn_id,
                    field.receiver,
                    try self.demandRecordField(field.field, .materialize),
                    changed,
                );
                try self.markArgUsesInExpr(fn_id, field.receiver, changed);
            },
            .tuple_access => |access| {
                try self.markArgDemandIfLocal(
                    fn_id,
                    access.tuple,
                    try self.demandTupleItem(access.elem_index, .materialize),
                    changed,
                );
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
                try self.markArgUseIfLocal(fn_id, match.scrutinee, changed);
                try self.markArgUsesInExpr(fn_id, match.scrutinee, changed);
                for (self.program.branchSpan(match.branches)) |branch| {
                    if (branch.guard) |guard| try self.markArgUsesInExpr(fn_id, guard, changed);
                    try self.markArgUsesInExpr(fn_id, branch.body, changed);
                }
            },
            .if_ => |if_| {
                for (self.program.ifBranchSpan(if_.branches)) |branch| {
                    try self.markArgUsesInExpr(fn_id, branch.cond, changed);
                    try self.markArgUsesInExpr(fn_id, branch.body, changed);
                }
                try self.markArgUsesInExpr(fn_id, if_.final_else, changed);
            },
            .block => |block| {
                for (self.program.stmtSpan(block.statements)) |stmt| try self.markArgUsesInStmt(fn_id, stmt, changed);
                try self.markArgUsesInExpr(fn_id, block.final_expr, changed);
            },
            .loop_ => |loop| {
                for (self.program.exprSpan(loop.initial_values)) |initial| try self.markArgUsesInExpr(fn_id, initial, changed);
                try self.markArgUsesInExpr(fn_id, loop.body, changed);
            },
            .state_loop => |state_loop| {
                for (self.program.exprSpan(state_loop.entry_values)) |initial| try self.markArgUsesInExpr(fn_id, initial, changed);
                for (self.program.stateLoopStateSpan(state_loop.states)) |state| {
                    try self.markArgUsesInExpr(fn_id, state.body, changed);
                }
            },
            .break_ => |maybe| if (maybe) |value| try self.markArgUsesInExpr(fn_id, value, changed),
            .continue_ => |continue_| for (self.program.exprSpan(continue_.values)) |value| try self.markArgUsesInExpr(fn_id, value, changed),
            .state_continue => |continue_| for (self.program.exprSpan(continue_.values)) |value| try self.markArgUsesInExpr(fn_id, value, changed),
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
        switch (self.program.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| try self.markArgUsesInExpr(fn_id, let_.value, changed),
            .expr,
            .expect,
            .dbg,
            .return_,
            => |expr| try self.markArgUsesInExpr(fn_id, expr, changed),
            .uninitialized, .crash => {},
        }
    }

    fn markArgUseIfLocal(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId, changed: *bool) Allocator.Error!void {
        try self.markArgDemandIfLocal(fn_id, expr_id, .materialize, changed);
    }

    fn markArgDemandIfLocal(
        self: *Pass,
        fn_id: Ast.FnId,
        expr_id: Ast.ExprId,
        demand: ValueDemand,
        changed: *bool,
    ) Allocator.Error!void {
        if (demand == .none) return;
        const local = localExpr(self.program, expr_id) orelse return;
        const args = self.program.typedLocalSpan(self.program.fns.items[@intFromEnum(fn_id)].args);
        for (args, 0..) |arg, index| {
            if (arg.local == local) {
                const used = &self.plans[@intFromEnum(fn_id)].used_args[index];
                if (!used.*) {
                    used.* = true;
                    changed.* = true;
                }
                const merged = try self.mergeValueDemand(self.plans[@intFromEnum(fn_id)].arg_demands[index], demand);
                if (!valueDemandEql(self.plans[@intFromEnum(fn_id)].arg_demands[index], merged)) {
                    self.plans[@intFromEnum(fn_id)].arg_demands[index] = merged;
                    changed.* = true;
                }
                return;
            }
        }
    }

    fn storedDemand(self: *Pass, demand: ValueDemand) Allocator.Error!*const ValueDemand {
        const stored = try self.arena.allocator().create(ValueDemand);
        stored.* = demand;
        return stored;
    }

    fn demandRecordField(self: *Pass, field: names.RecordFieldNameId, demand: ValueDemand) Allocator.Error!ValueDemand {
        const fields = try self.arena.allocator().alloc(FieldDemand, 1);
        fields[0] = .{
            .name = field,
            .demand = try self.storedDemand(demand),
        };
        return .{ .record = fields };
    }

    fn demandTupleItem(self: *Pass, index: u32, demand: ValueDemand) Allocator.Error!ValueDemand {
        const items = try self.arena.allocator().alloc(ItemDemand, 1);
        items[0] = .{
            .index = index,
            .demand = try self.storedDemand(demand),
        };
        return .{ .tuple = items };
    }

    fn mergeValueDemand(self: *Pass, existing: ValueDemand, incoming: ValueDemand) Allocator.Error!ValueDemand {
        if (existing == .materialize or incoming == .materialize) return .materialize;
        if (existing == .none) return incoming;
        if (incoming == .none) return existing;
        if (std.meta.activeTag(existing) != std.meta.activeTag(incoming)) return .materialize;

        return switch (existing) {
            .none, .materialize => unreachable,
            .record => try self.mergeRecordDemand(existing.record, incoming.record),
            .tuple => try self.mergeTupleDemand(existing.tuple, incoming.tuple),
            .nominal => blk: {
                const merged = try self.mergeValueDemand(existing.nominal.*, incoming.nominal.*);
                break :blk ValueDemand{ .nominal = try self.storedDemand(merged) };
            },
            .callable => |existing_callable| blk: {
                const incoming_callable = incoming.callable;
                if (existing_callable.captures.len != incoming_callable.captures.len) break :blk .materialize;
                const captures = try self.arena.allocator().alloc(ValueDemand, existing_callable.captures.len);
                for (existing_callable.captures, incoming_callable.captures, captures) |existing_capture, incoming_capture, *out| {
                    out.* = try self.mergeValueDemand(existing_capture, incoming_capture);
                }
                break :blk ValueDemand{ .callable = .{ .captures = captures } };
            },
        };
    }

    fn mergeRecordDemand(
        self: *Pass,
        existing: []const FieldDemand,
        incoming: []const FieldDemand,
    ) Allocator.Error!ValueDemand {
        var fields = std.ArrayList(FieldDemand).empty;
        defer fields.deinit(self.allocator);
        try fields.appendSlice(self.allocator, existing);

        for (incoming) |incoming_field| {
            for (fields.items) |*field| {
                if (field.name != incoming_field.name) continue;
                const merged = try self.mergeValueDemand(field.demand.*, incoming_field.demand.*);
                field.demand = try self.storedDemand(merged);
                break;
            } else {
                try fields.append(self.allocator, incoming_field);
            }
        }

        return .{ .record = try self.arena.allocator().dupe(FieldDemand, fields.items) };
    }

    fn mergeTupleDemand(
        self: *Pass,
        existing: []const ItemDemand,
        incoming: []const ItemDemand,
    ) Allocator.Error!ValueDemand {
        var items = std.ArrayList(ItemDemand).empty;
        defer items.deinit(self.allocator);
        try items.appendSlice(self.allocator, existing);

        for (incoming) |incoming_item| {
            for (items.items) |*item| {
                if (item.index != incoming_item.index) continue;
                const merged = try self.mergeValueDemand(item.demand.*, incoming_item.demand.*);
                item.demand = try self.storedDemand(merged);
                break;
            } else {
                try items.append(self.allocator, incoming_item);
            }
        }

        return .{ .tuple = try self.arena.allocator().dupe(ItemDemand, items.items) };
    }

    fn ensureCallPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return;

        const fn_args = self.program.typedLocalSpan(self.program.fns.items[raw].args);
        if (values.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const known_values = try self.arena.allocator().alloc(KnownValue, values.len);
        var has_constructor = false;
        for (values, 0..) |value, index| {
            if (self.plans[raw].used_args[index]) {
                if (try self.knownValueFromValue(value)) |known_value| {
                    known_values[index] = known_value;
                    has_constructor = true;
                    continue;
                }
            }
            known_values[index] = .{ .any = valueType(self.program, value) };
        }
        if (!has_constructor) return;

        const pattern: CallPattern = .{ .args = known_values };
        for (self.plans[raw].specs.items) |spec| {
            if (patternEql(self.program, spec.pattern, pattern)) return;
        }

        const spec_index = self.plans[raw].specs.items.len;
        try self.plans[raw].specs.append(self.allocator, .{ .pattern = pattern });
        try self.reserveWorker(@enumFromInt(@as(u32, @intCast(raw))), spec_index);
    }

    fn reserveWorker(self: *Pass, source_fn_id: Ast.FnId, spec_index: usize) Allocator.Error!void {
        const source_index = @intFromEnum(source_fn_id);
        const spec = &self.plans[source_index].specs.items[spec_index];
        if (spec.fn_id != null) Common.invariant("call-pattern specialization id was assigned twice");

        try self.program.fns.ensureUnusedCapacity(self.allocator, 1);
        try self.worker_worklist.ensureUnusedCapacity(self.allocator, 1);

        const source_fn = self.program.fns.items[source_index];
        const fn_id_reserved: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.program.fns.items.len)));
        const symbol = self.symbols.fresh();
        spec.fn_id = fn_id_reserved;
        self.program.fns.appendAssumeCapacity(.{
            .symbol = symbol,
            .source = source_fn.source,
            .args = .empty(),
            .captures = source_fn.captures,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        self.worker_worklist.appendAssumeCapacity(.{
            .source_fn = source_fn_id,
            .spec_index = spec_index,
        });
        try self.copyProcDebugName(source_fn.symbol, symbol);
    }

    fn writeSpecialization(self: *Pass, source_fn_id: Ast.FnId, spec_index: usize, source_body: Ast.ExprId) Common.LowerError!void {
        const source_fn = self.program.fns.items[@intFromEnum(source_fn_id)];
        const spec = &self.plans[@intFromEnum(source_fn_id)].specs.items[spec_index];

        const spec_fn_id = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before cloning");
        const symbol = self.program.fns.items[@intFromEnum(spec_fn_id)].symbol;

        var cloner = Cloner.init(self, source_fn_id, spec.pattern);
        defer cloner.deinit();

        try cloner.inline_stack.append(self.allocator, .{ .fn_id = source_fn_id });
        defer {
            const popped = cloner.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow while writing specialization");
            if (popped.fn_id != source_fn_id) Common.invariant("call-pattern inline stack was corrupted while writing specialization");
        }

        const args = try cloner.buildArgs();
        const body: Ast.FnBody = .{ .roc = try cloner.cloneExpr(source_body) };

        self.program.fns.items[@intFromEnum(spec_fn_id)] = .{
            .symbol = symbol,
            .source = source_fn.source,
            .args = args,
            .captures = source_fn.captures,
            .body = body,
            .ret = source_fn.ret,
        };
        try self.copyProcDebugName(source_fn.symbol, symbol);
    }

    fn constructorKnownValue(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!?KnownValue {
        const expr = self.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .list,
            => KnownValue{ .leaf = expr.ty },
            .tag => |tag| blk: {
                const payloads = self.program.exprSpan(tag.payloads);
                const known_values = try self.arena.allocator().alloc(KnownValue, payloads.len);
                for (payloads, 0..) |payload, index| {
                    known_values[index] = (try self.constructorKnownValue(payload)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(payload)].ty };
                }
                break :blk KnownValue{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = known_values,
                } };
            },
            .record => |fields_span| blk: {
                const fields = self.program.fieldExprSpan(fields_span);
                const known_values = try self.arena.allocator().alloc(KnownField, fields.len);
                for (fields, 0..) |field, index| {
                    known_values[index] = .{
                        .name = field.name,
                        .known_value = (try self.constructorKnownValue(field.value)) orelse
                            .{ .any = self.program.exprs.items[@intFromEnum(field.value)].ty },
                    };
                }
                break :blk KnownValue{ .record = .{
                    .ty = expr.ty,
                    .fields = known_values,
                } };
            },
            .tuple => |items_span| blk: {
                const items = self.program.exprSpan(items_span);
                const known_values = try self.arena.allocator().alloc(KnownValue, items.len);
                for (items, 0..) |item, index| {
                    known_values[index] = (try self.constructorKnownValue(item)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(item)].ty };
                }
                break :blk KnownValue{ .tuple = .{
                    .ty = expr.ty,
                    .items = known_values,
                } };
            },
            .nominal => |backing| blk: {
                const backing_known_value = (try self.constructorKnownValue(backing)) orelse break :blk null;
                const stored = try self.arena.allocator().create(KnownValue);
                stored.* = backing_known_value;
                break :blk KnownValue{ .nominal = .{
                    .ty = expr.ty,
                    .backing = stored,
                } };
            },
            .fn_ref => |fn_id| blk: {
                const fn_ = self.program.fns.items[@intFromEnum(fn_id)];
                const captures = self.program.typedLocalSpan(fn_.captures);
                const capture_known_values = try self.arena.allocator().alloc(KnownValue, captures.len);
                for (captures, 0..) |capture, index| {
                    capture_known_values[index] = .{ .any = capture.ty };
                }
                break :blk KnownValue{ .callable = .{
                    .ty = expr.ty,
                    .fn_id = fn_id,
                    .captures = capture_known_values,
                } };
            },
            else => null,
        };
    }

    fn knownValueFromValue(self: *Pass, value: Value) Allocator.Error!?KnownValue {
        return switch (value) {
            .expr => |expr| try self.constructorKnownValue(expr),
            .expr_with_known_value => |known_value_expr| known_value_expr.known_value,
            .let_ => |let_value| try self.knownValueFromValue(let_value.body.*),
            .if_ => |if_value| blk: {
                var joined: ?KnownValue = null;
                for (if_value.branches) |branch| {
                    const branch_known_value = (try self.knownValueFromValue(branch.body)) orelse break :blk null;
                    joined = if (joined) |existing|
                        (try joinKnownValuesInArena(self.program, self.arena.allocator(), existing, branch_known_value)) orelse break :blk null
                    else
                        branch_known_value;
                }
                const final_known_value = (try self.knownValueFromValue(if_value.final_else.*)) orelse break :blk null;
                break :blk if (joined) |existing|
                    (try joinKnownValuesInArena(self.program, self.arena.allocator(), existing, final_known_value)) orelse null
                else
                    final_known_value;
            },
            .tag => |tag| blk: {
                const payloads = try self.arena.allocator().alloc(KnownValue, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = (try self.knownValueFromValue(payload)) orelse
                        .{ .any = valueType(self.program, payload) };
                }
                break :blk KnownValue{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| blk: {
                const fields = try self.arena.allocator().alloc(KnownField, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .known_value = (try self.knownValueFromValue(field.value)) orelse
                            .{ .any = valueType(self.program, field.value) },
                    };
                }
                break :blk KnownValue{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.arena.allocator().alloc(KnownValue, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = (try self.knownValueFromValue(item)) orelse
                        .{ .any = valueType(self.program, item) };
                }
                break :blk KnownValue{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing_known_value = (try self.knownValueFromValue(nominal.backing.*)) orelse break :blk null;
                const stored = try self.arena.allocator().create(KnownValue);
                stored.* = backing_known_value;
                break :blk KnownValue{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.arena.allocator().alloc(KnownValue, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = (try self.knownValueFromValue(capture)) orelse
                        (try leafKnownValueFromValue(self.program, capture)) orelse
                        .{ .any = valueType(self.program, capture) };
                }
                break :blk KnownValue{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
            .finite_tags => |finite_tags| blk: {
                const alternatives = try self.arena.allocator().alloc(KnownTag, finite_tags.alternatives.len);
                for (finite_tags.alternatives, alternatives) |alternative, *out| {
                    const payloads = try self.arena.allocator().alloc(KnownValue, alternative.payloads.len);
                    for (alternative.payloads, payloads) |payload, *payload_out| {
                        payload_out.* = (try self.knownValueFromValue(payload)) orelse
                            .{ .any = valueType(self.program, payload) };
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .name = alternative.name,
                        .payloads = payloads,
                    };
                }
                break :blk KnownValue{ .finite_tags = .{
                    .ty = finite_tags.ty,
                    .alternatives = alternatives,
                } };
            },
            .finite_callables => |finite_callables| blk: {
                const alternatives = try self.arena.allocator().alloc(KnownCallable, finite_callables.alternatives.len);
                for (finite_callables.alternatives, alternatives) |alternative, *out| {
                    const captures = try self.arena.allocator().alloc(KnownValue, alternative.captures.len);
                    for (alternative.captures, captures) |capture, *capture_out| {
                        capture_out.* = (try self.knownValueFromValue(capture)) orelse
                            (try leafKnownValueFromValue(self.program, capture)) orelse
                            .{ .any = valueType(self.program, capture) };
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .fn_id = alternative.fn_id,
                        .captures = captures,
                    };
                }
                break :blk KnownValue{ .finite_callables = .{
                    .ty = finite_callables.ty,
                    .alternatives = alternatives,
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
    state_loop_state_map: std.AutoHashMap(Ast.StateLoopStateId, Ast.StateLoopStateId),
    changes: std.ArrayList(BindingChange),
    inline_stack: std.ArrayList(ActiveInline),
    loop_stack: std.ArrayList(LoopPattern),
    state_loop_stack: std.ArrayList(StateLoopPattern),
    inline_direct_calls: bool,
    inline_direct_requires_known_arg: bool,
    record_call_patterns: bool,
    source_arg_locals_in_scope: bool,
    current_loc: SourceLoc,
    current_region: Region,

    fn init(pass: *Pass, source_fn: Ast.FnId, pattern: CallPattern) Cloner {
        return .{
            .pass = pass,
            .source_fn = source_fn,
            .pattern = pattern,
            .subst = std.AutoHashMap(Ast.LocalId, Value).init(pass.allocator),
            .state_loop_state_map = std.AutoHashMap(Ast.StateLoopStateId, Ast.StateLoopStateId).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .loop_stack = .empty,
            .state_loop_stack = .empty,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = true,
            .record_call_patterns = true,
            .source_arg_locals_in_scope = false,
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
        };
    }

    fn initForBaseClone(pass: *Pass) Cloner {
        return .{
            .pass = pass,
            .source_fn = undefined, // Base-body cloning never calls buildArgs, which is the only reader.
            .pattern = .{ .args = &.{} },
            .subst = std.AutoHashMap(Ast.LocalId, Value).init(pass.allocator),
            .state_loop_state_map = std.AutoHashMap(Ast.StateLoopStateId, Ast.StateLoopStateId).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .loop_stack = .empty,
            .state_loop_stack = .empty,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = false,
            .record_call_patterns = true,
            .source_arg_locals_in_scope = false,
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
        };
    }

    fn initForBaseBody(pass: *Pass, source_fn: Ast.FnId) Cloner {
        var cloner = Cloner.initForBaseClone(pass);
        cloner.source_fn = source_fn;
        cloner.inline_direct_requires_known_arg = true;
        cloner.source_arg_locals_in_scope = true;
        return cloner;
    }

    fn deinit(self: *Cloner) void {
        self.state_loop_stack.deinit(self.pass.allocator);
        self.inline_stack.deinit(self.pass.allocator);
        self.loop_stack.deinit(self.pass.allocator);
        self.changes.deinit(self.pass.allocator);
        self.state_loop_state_map.deinit();
        self.subst.deinit();
    }

    fn buildArgs(self: *Cloner) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        const source_fn = self.pass.program.fns.items[@intFromEnum(self.source_fn)];
        const source_args = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
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

        for (source_args, self.pattern.args) |source_arg, known_value| {
            const value = try self.valueFromKnownValueArgs(known_value, &args);
            try self.putSubst(source_arg.local, value);
        }

        return try self.pass.program.addTypedLocalSpan(args.items);
    }

    fn valueFromKnownValueArgs(self: *Cloner, known_value: KnownValue, args: *std.ArrayList(Ast.TypedLocal)) Allocator.Error!Value {
        switch (known_value) {
            .any => |ty| {
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try args.append(self.pass.allocator, .{ .local = local, .ty = ty });
                return .{ .expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                }) };
            },
            .leaf => |ty| {
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
                    payloads[index] = try self.valueFromKnownValueArgs(payload, args);
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
                        .value = try self.valueFromKnownValueArgs(field.known_value, args),
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
                    items[index] = try self.valueFromKnownValueArgs(item, args);
                }
                return .{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| {
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = try self.valueFromKnownValueArgs(nominal.backing.*, args);
                return .{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| {
                const captures = try self.pass.arena.allocator().alloc(Value, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = try self.valueFromKnownValueArgs(capture, args);
                }
                return .{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
            .finite_tags => |finite_tags| {
                const selector_ty = try self.pass.primitiveType(.u64);
                const selector_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), selector_ty);
                try args.append(self.pass.allocator, .{ .local = selector_local, .ty = selector_ty });
                const selector = try self.addExpr(.{
                    .ty = selector_ty,
                    .data = .{ .local = selector_local },
                });

                const alternatives = try self.pass.arena.allocator().alloc(TagValue, finite_tags.alternatives.len);
                for (finite_tags.alternatives, alternatives) |alternative, *out| {
                    const payloads = try self.pass.arena.allocator().alloc(Value, alternative.payloads.len);
                    for (alternative.payloads, payloads) |payload_known_value, *payload_out| {
                        payload_out.* = try self.valueFromKnownValueArgs(payload_known_value, args);
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .name = alternative.name,
                        .payloads = payloads,
                    };
                }

                return .{ .finite_tags = .{
                    .ty = finite_tags.ty,
                    .selector = selector,
                    .alternatives = alternatives,
                } };
            },
            .finite_callables => |finite_callables| {
                const selector_ty = try self.pass.primitiveType(.u64);
                const selector_local = try self.pass.program.addLocal(self.pass.symbols.fresh(), selector_ty);
                try args.append(self.pass.allocator, .{ .local = selector_local, .ty = selector_ty });
                const selector = try self.addExpr(.{
                    .ty = selector_ty,
                    .data = .{ .local = selector_local },
                });

                const alternatives = try self.pass.arena.allocator().alloc(CallableValue, finite_callables.alternatives.len);
                for (finite_callables.alternatives, alternatives) |alternative, *out| {
                    const captures = try self.pass.arena.allocator().alloc(Value, alternative.captures.len);
                    for (alternative.captures, captures) |capture_known_value, *capture_out| {
                        capture_out.* = try self.valueFromKnownValueArgs(capture_known_value, args);
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .fn_id = alternative.fn_id,
                        .captures = captures,
                    };
                }

                return .{ .finite_callables = .{
                    .ty = finite_callables.ty,
                    .selector = selector,
                    .alternatives = alternatives,
                } };
            },
        }
    }

    fn cloneExpr(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Ast.ExprId {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        self.current_loc = self.pass.program.exprLoc(expr_id);
        self.current_region = self.pass.program.exprRegion(expr_id);
        return try self.materialize(try self.cloneExprValue(expr_id));
    }

    fn cloneExprValue(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        self.current_loc = self.pass.program.exprLoc(expr_id);
        self.current_region = self.pass.program.exprRegion(expr_id);

        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local => |local| {
                if (self.subst.get(local)) |value| return value;
                if (try self.solvedSingleCallable(expr_id)) |callable| return callable;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } }) };
            },
            .fn_ref => |fn_id| return try self.callableValue(expr.ty, fn_id),
            .tag => |tag| {
                const payload_exprs = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(tag.payloads));
                defer self.pass.allocator.free(payload_exprs);
                const payloads = try self.pass.arena.allocator().alloc(Value, payload_exprs.len);
                for (payload_exprs, 0..) |payload, index| {
                    payloads[index] = try self.cloneExprValueDemandingKnownValue(payload);
                }
                return .{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |fields_span| {
                const source_fields = try self.pass.allocator.dupe(Ast.FieldExpr, self.pass.program.fieldExprSpan(fields_span));
                defer self.pass.allocator.free(source_fields);
                const fields = try self.pass.arena.allocator().alloc(FieldValue, source_fields.len);
                for (source_fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .value = try self.cloneExprValueDemandingKnownValue(field.value),
                    };
                }
                return .{ .record = .{
                    .ty = expr.ty,
                    .fields = fields,
                } };
            },
            .tuple => |items_span| {
                const source_items = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(items_span));
                defer self.pass.allocator.free(source_items);
                const items = try self.pass.arena.allocator().alloc(Value, source_items.len);
                for (source_items, 0..) |item, index| {
                    items[index] = try self.cloneExprValueDemandingKnownValue(item);
                }
                return .{ .tuple = .{
                    .ty = expr.ty,
                    .items = items,
                } };
            },
            .nominal => |backing| {
                const backing_value = try self.cloneExprValueDemandingKnownValue(backing);
                return .{ .nominal = .{
                    .ty = expr.ty,
                    .backing = try self.copyValue(backing_value),
                } };
            },
            .let_ => |let_| return try self.cloneLetValue(let_),
            .field_access => |field| {
                const receiver = try self.cloneExprValueDemandingKnownValue(field.receiver);
                if (try self.fieldFromKnownValue(receiver, field.field)) |value| return value;
                if (try self.solvedSingleCallable(expr_id)) |callable| return callable;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .field_access = .{
                    .receiver = try self.materialize(receiver),
                    .field = field.field,
                } } }) };
            },
            .tuple_access => |access| {
                const receiver = try self.cloneExprValueDemandingKnownValue(access.tuple);
                if (try self.itemFromKnownValue(receiver, access.elem_index)) |value| return value;
                if (try self.solvedSingleCallable(expr_id)) |callable| return callable;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = try self.materialize(receiver),
                    .elem_index = access.elem_index,
                } } }) };
            },
            .match_ => |match| {
                const scrutinee = try self.cloneExprValueDemandingKnownValue(match.scrutinee);
                if (try self.simplifyKnownMatchValue(expr.ty, scrutinee, match.branches)) |value| return value;
                const scrutinee_expr = try self.materialize(scrutinee);
                if (try self.cloneCaseOfCaseValue(expr.ty, scrutinee_expr, match.branches)) |value| return value;
                const scrutinee_known_value = try self.pass.knownValueFromValue(scrutinee);
                return try self.cloneMatchJoinedValue(expr.ty, scrutinee_expr, match, scrutinee_known_value);
            },
            .if_ => |if_| return try self.cloneIfValue(expr.ty, if_),
            .block => |block| return try self.cloneBlockValue(expr.ty, block),
            .call_value => |call| {
                const callee = try self.cloneExprValueDemandingKnownValue(call.callee);
                return try self.callKnownValue(expr.ty, callee, call.args, false);
            },
            .call_proc => |call| {
                if (call.is_cold) return .{ .expr = try self.cloneExprPlain(expr_id) };
                if (!self.inline_direct_calls) return .{ .expr = try self.cloneExprPlain(expr_id) };
                const has_known_value_arg = try self.directCallHasKnownValueArg(call.args);
                if (self.inline_direct_requires_known_arg and !has_known_value_arg) {
                    return .{ .expr = try self.cloneExprPlain(expr_id) };
                }
                return try self.inlineDirectCallValue(
                    Ast.callProcCallee(call),
                    call.args,
                    expr_id,
                    false,
                );
            },
            else => return .{ .expr = try self.cloneExprPlain(expr_id) },
        }
    }

    fn cloneExprValueDemandingKnownValue(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        const value = blk: switch (expr.data) {
            .call_value => |call| {
                const callee = try self.cloneExprValueDemandingKnownValue(call.callee);
                break :blk try self.callKnownValue(expr.ty, callee, call.args, true);
            },
            .call_proc => |call| {
                if (call.is_cold) break :blk try self.cloneExprValue(expr_id);
                if (!self.inline_direct_calls) break :blk try self.cloneExprValue(expr_id);
                break :blk try self.inlineDirectCallValue(
                    Ast.callProcCallee(call),
                    call.args,
                    expr_id,
                    true,
                );
            },
            .block => |block| break :blk try self.cloneBlockValueDemandingKnownValue(expr.ty, block),
            .comptime_branch_taken => |taken| break :blk try self.cloneExprValueDemandingKnownValue(taken.body),
            else => break :blk try self.cloneExprValue(expr_id),
        };
        return try self.ensureDemandedKnownValue(value);
    }

    fn ensureDemandedKnownValue(self: *Cloner, value: Value) Common.LowerError!Value {
        if ((try self.pass.knownValueFromValue(value)) != null) return value;
        return switch (value) {
            .expr => |expr| blk: {
                const ty = self.pass.program.exprs.items[@intFromEnum(expr)].ty;
                if (try typeMayContainRefcounted(self.pass.program, ty)) break :blk value;
                break :blk Value{ .expr_with_known_value = .{
                    .expr = expr,
                    .known_value = .{ .leaf = ty },
                } };
            },
            .let_ => |let_value| blk: {
                const body = try self.ensureDemandedKnownValue(let_value.body.*);
                break :blk Value{ .let_ = .{
                    .lets = let_value.lets,
                    .body = try self.copyValue(body),
                } };
            },
            else => value,
        };
    }

    fn directCallHasKnownValueArg(self: *Cloner, args_span: Ast.Span(Ast.ExprId)) Allocator.Error!bool {
        for (self.pass.program.exprSpan(args_span)) |arg| {
            if (try self.exprHasKnownValue(arg)) return true;
        }
        return false;
    }

    fn directCallActiveArgKnownValues(self: *Cloner, args_span: Ast.Span(Ast.ExprId)) Allocator.Error![]const KnownValue {
        const args = self.pass.program.exprSpan(args_span);
        const known_values = try self.pass.arena.allocator().alloc(KnownValue, args.len);
        for (args, 0..) |arg, index| {
            known_values[index] = (try self.exprKnownValueNoInline(arg)) orelse .{
                .any = self.pass.program.exprs.items[@intFromEnum(arg)].ty,
            };
        }
        return known_values;
    }

    fn exprKnownValueNoInline(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!?KnownValue {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .local => |local| if (self.subst.get(local)) |value|
                try self.pass.knownValueFromValue(value)
            else
                null,
            .fn_ref => |fn_id| try self.knownCallable(expr.ty, fn_id),
            .tag,
            .record,
            .tuple,
            .nominal,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .list,
            => try self.pass.constructorKnownValue(expr_id),
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk null;
                const receiver = self.subst.get(receiver_local) orelse break :blk null;
                const value = fieldFromValue(receiver, field.field) orelse break :blk null;
                break :blk try self.pass.knownValueFromValue(value);
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk null;
                const tuple = self.subst.get(tuple_local) orelse break :blk null;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk null;
                break :blk try self.pass.knownValueFromValue(value);
            },
            .comptime_branch_taken => |taken| try self.exprKnownValueNoInline(taken.body),
            else => null,
        };
    }

    fn exprHasKnownValue(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!bool {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .local => |local| if (self.subst.get(local)) |value|
                (try self.pass.knownValueFromValue(value)) != null
            else
                false,
            .fn_ref => true,
            .tag,
            .record,
            .tuple,
            .nominal,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .list,
            => (try self.pass.constructorKnownValue(expr_id)) != null,
            .static_data_candidate => true,
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk false;
                const receiver = self.subst.get(receiver_local) orelse break :blk false;
                const value = fieldFromValue(receiver, field.field) orelse break :blk false;
                break :blk (try self.pass.knownValueFromValue(value)) != null;
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk false;
                const tuple = self.subst.get(tuple_local) orelse break :blk false;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk false;
                break :blk (try self.pass.knownValueFromValue(value)) != null;
            },
            .comptime_branch_taken => |taken| try self.exprHasKnownValue(taken.body),
            .comptime_exhaustiveness_failed => false,
            else => false,
        };
    }

    fn valueCanSubstitute(self: *Cloner, value: Value) bool {
        return switch (value) {
            .expr => |expr| self.exprCanSubstitute(expr),
            .let_ => false,
            .if_ => |if_value| blk: {
                for (if_value.branches) |branch| {
                    if (!self.exprCanSubstitute(branch.cond) or !self.valueCanSubstitute(branch.body)) break :blk false;
                }
                break :blk self.valueCanSubstitute(if_value.final_else.*);
            },
            .tag => |tag| blk: {
                for (tag.payloads) |payload| {
                    if (!self.valueCanSubstitute(payload)) break :blk false;
                }
                break :blk true;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (!self.valueCanSubstitute(field.value)) break :blk false;
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (tuple.items) |item| {
                    if (!self.valueCanSubstitute(item)) break :blk false;
                }
                break :blk true;
            },
            .nominal => |nominal| self.valueCanSubstitute(nominal.backing.*),
            .callable => |callable| blk: {
                for (callable.captures) |capture| {
                    if (!self.valueCanSubstitute(capture)) break :blk false;
                }
                break :blk true;
            },
            .finite_tags => |finite_tags| blk: {
                if (!self.exprCanSubstitute(finite_tags.selector)) break :blk false;
                for (finite_tags.alternatives) |alternative| {
                    for (alternative.payloads) |payload| {
                        if (!self.valueCanSubstitute(payload)) break :blk false;
                    }
                }
                break :blk true;
            },
            .finite_callables => |finite_callables| blk: {
                if (!self.exprCanSubstitute(finite_callables.selector)) break :blk false;
                for (finite_callables.alternatives) |alternative| {
                    for (alternative.captures) |capture| {
                        if (!self.valueCanSubstitute(capture)) break :blk false;
                    }
                }
                break :blk true;
            },
            .expr_with_known_value => |known_value_expr| self.exprCanSubstitute(known_value_expr.expr),
        };
    }

    fn exprCanSubstitute(self: *Cloner, expr_id: Ast.ExprId) bool {
        return switch (self.pass.program.exprs.items[@intFromEnum(expr_id)].data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .fn_ref,
            => true,
            .field_access => |field| self.exprCanSubstitute(field.receiver),
            .tuple_access => |access| self.exprCanSubstitute(access.tuple),
            else => false,
        };
    }

    fn callableValue(self: *Cloner, ty: Type.TypeId, fn_id: Ast.FnId) Common.LowerError!Value {
        const fn_ = self.pass.program.fns.items[@intFromEnum(fn_id)];
        const source_captures = self.pass.program.typedLocalSpan(fn_.captures);
        const captures = try self.pass.arena.allocator().alloc(Value, source_captures.len);
        for (source_captures, 0..) |capture, index| {
            if (self.subst.get(capture.local)) |value| {
                captures[index] = value;
            } else if (try self.scopedLocalValue(capture)) |value| {
                captures[index] = value;
            } else {
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .fn_ref = fn_id } }) };
            }
        }
        return .{ .callable = .{
            .ty = ty,
            .fn_id = fn_id,
            .captures = captures,
        } };
    }

    fn knownCallable(self: *Cloner, ty: Type.TypeId, fn_id: Ast.FnId) Allocator.Error!KnownValue {
        const fn_ = self.pass.program.fns.items[@intFromEnum(fn_id)];
        const source_captures = self.pass.program.typedLocalSpan(fn_.captures);
        const captures = try self.pass.arena.allocator().alloc(KnownValue, source_captures.len);
        for (source_captures, 0..) |capture, index| {
            captures[index] = if (self.subst.get(capture.local)) |value|
                (try self.pass.knownValueFromValue(value)) orelse .{ .any = valueType(self.pass.program, value) }
            else
                .{ .any = capture.ty };
        }
        return .{ .callable = .{
            .ty = ty,
            .fn_id = fn_id,
            .captures = captures,
        } };
    }

    fn solvedSingleCallable(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!?Value {
        const solved = self.pass.solved orelse return null;
        const member = self.pass.solvedSingleCallableMember(expr_id) orelse return null;
        const fn_id = self.pass.fnWithSymbol(member.lambda) orelse return null;
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        const solved_captures = solved.types.captureSpan(member.captures);
        const source_fn = self.pass.program.fns.items[@intFromEnum(fn_id)];
        const source_captures = self.pass.program.typedLocalSpan(source_fn.captures);
        if (solved_captures.len != source_captures.len) {
            Common.invariant("Lambda Solved callable member capture count differed from lifted function captures");
        }

        const captures = try self.pass.arena.allocator().alloc(Value, solved_captures.len);
        for (solved_captures, 0..) |capture, index| {
            if (capture.local != source_captures[index].local) {
                Common.invariant("Lambda Solved callable member captures differed from lifted function capture order");
            }
            captures[index] = if (self.subst.get(capture.local)) |value|
                value
            else if (try self.scopedLocalValue(source_captures[index])) |value|
                value
            else
                return null;
        }
        return .{ .callable = .{
            .ty = expr.ty,
            .fn_id = fn_id,
            .captures = captures,
        } };
    }

    fn cloneExprPlain(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Ast.ExprId {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        self.current_loc = self.pass.program.exprLoc(expr_id);
        self.current_region = self.pass.program.exprRegion(expr_id);

        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        const data: Ast.ExprData = switch (expr.data) {
            .local => |local| .{ .local = local },
            .unit => .unit,
            .uninitialized => .uninitialized,
            .uninitialized_payload => |payload| .{ .uninitialized_payload = payload },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .static_data => |value| .{ .static_data = value },
            .static_data_candidate => |candidate| .{ .static_data_candidate = .{
                .static_data = candidate.static_data,
                .fallback = try self.cloneExpr(candidate.fallback),
            } },
            .list => |items| .{ .list = try self.cloneExprSpan(items) },
            .tuple => |items| .{ .tuple = try self.cloneExprSpan(items) },
            .record => |fields| .{ .record = try self.cloneFieldExprSpan(fields) },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.cloneExprSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.cloneExpr(backing) },
            .let_ => |let_| try self.cloneLet(let_),
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached call-pattern specialization"),
            .fn_ref => |target| .{ .fn_ref = target },
            .call_value => |call| .{ .call_value = .{
                .callee = try self.cloneExpr(call.callee),
                .args = try self.cloneExprSpan(call.args),
            } },
            .call_proc => |call| return try self.cloneCallProcExpr(expr.ty, call),
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
            .loop_ => |loop| return try self.cloneLoop(expr.ty, loop),
            .state_loop => |state_loop| blk: {
                const states = try self.cloneStateLoopStateSpan(state_loop.states);
                break :blk .{ .state_loop = .{
                    .entry_state = self.cloneStateLoopStateId(state_loop.entry_state),
                    .entry_values = try self.cloneExprSpan(state_loop.entry_values),
                    .states = states,
                } };
            },
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.cloneExpr(value) else null },
            .continue_ => |continue_| try self.cloneContinue(expr.ty, continue_),
            .state_continue => |continue_| .{ .state_continue = .{
                .target_state = self.cloneStateLoopStateId(continue_.target_state),
                .values = try self.cloneExprSpan(continue_.values),
            } },
            .if_initialized_payload => |payload_switch| .{ .if_initialized_payload = .{
                .cond = try self.cloneExpr(payload_switch.cond),
                .cond_mask = payload_switch.cond_mask,
                .payload = payload_switch.payload,
                .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
                .initialized = try self.cloneExpr(payload_switch.initialized),
                .uninitialized = try self.cloneExpr(payload_switch.uninitialized),
            } },
            .try_sequence => |sequence| .{ .try_sequence = .{
                .try_expr = try self.cloneExpr(sequence.try_expr),
                .ok_local = sequence.ok_local,
                .err_is_cold = sequence.err_is_cold,
                .ok_body = try self.cloneExpr(sequence.ok_body),
            } },
            .try_record_sequence => |sequence| .{ .try_record_sequence = .{
                .try_expr = try self.cloneExpr(sequence.try_expr),
                .value_local = sequence.value_local,
                .value_field = sequence.value_field,
                .rest_local = sequence.rest_local,
                .rest_field = sequence.rest_field,
                .err_is_cold = sequence.err_is_cold,
                .ok_body = try self.cloneExpr(sequence.ok_body),
            } },
            .return_ => |value| .{ .return_ = try self.cloneExpr(value) },
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

    fn cloneLetValue(self: *Cloner, let_: anytype) Common.LowerError!Value {
        const raw_value = try self.cloneExprValueDemandingKnownValue(let_.value);
        var pending_lets = std.ArrayList(PendingLet).empty;
        defer pending_lets.deinit(self.pass.allocator);

        const pending_change_start = self.changes.items.len;
        var value = raw_value;
        while (value == .let_) {
            try pending_lets.appendSlice(self.pass.allocator, value.let_.lets);
            try self.bindPendingLetKnownValues(value.let_.lets);
            value = value.let_.body.*;
        }

        const reusable = try self.makeReusableForMatch(value, &pending_lets);
        const bind_change_start = self.changes.items.len;
        if (try self.bindPatToReusableValue(let_.bind, reusable)) {
            const rest = try self.cloneExprValue(let_.rest);
            self.restore(pending_change_start);
            return try self.wrapPendingLets(rest, pending_lets.items, true);
        }
        self.restore(bind_change_start);

        if (try self.bindPatToSingleUseRestValue(let_.bind, value, let_.rest)) {
            const rest = try self.cloneExprValue(let_.rest);
            self.restore(pending_change_start);
            return try self.wrapPendingLets(rest, pending_lets.items, true);
        }
        self.restore(pending_change_start);

        const value_expr = try self.materialize(raw_value);
        const change_start = self.changes.items.len;
        if (try self.bindPatToMaterializedKnownValue(let_.bind, raw_value)) {
            const rest_value = try self.cloneExprValue(let_.rest);
            const rest = try self.materialize(rest_value);
            self.restore(change_start);
            return .{ .expr = try self.addExpr(.{ .ty = self.pass.program.exprs.items[@intFromEnum(let_.rest)].ty, .data = .{ .let_ = .{
                .bind = try self.clonePat(let_.bind),
                .value = value_expr,
                .rest = rest,
                .comptime_site = let_.comptime_site,
            } } }) };
        }
        self.restore(change_start);
        return .{ .expr = try self.addExpr(.{ .ty = self.pass.program.exprs.items[@intFromEnum(let_.rest)].ty, .data = .{ .let_ = .{
            .bind = try self.clonePat(let_.bind),
            .value = value_expr,
            .rest = try self.cloneExpr(let_.rest),
            .comptime_site = let_.comptime_site,
        } } }) };
    }

    fn cloneLet(self: *Cloner, let_: anytype) Common.LowerError!Ast.ExprData {
        const value = try self.cloneExprValue(let_.value);
        const value_expr = try self.materialize(value);
        const change_start = self.changes.items.len;
        const bound = try self.bindPatToReusableValue(let_.bind, value);
        const rest = if (bound) blk: {
            const cloned = try self.cloneExpr(let_.rest);
            self.restore(change_start);
            break :blk cloned;
        } else if (try self.bindPatToSingleUseRestValue(let_.bind, value, let_.rest)) blk: {
            const cloned = try self.cloneExpr(let_.rest);
            self.restore(change_start);
            break :blk cloned;
        } else blk: {
            if (try self.bindPatToMaterializedKnownValue(let_.bind, value)) {
                const cloned = try self.cloneExpr(let_.rest);
                self.restore(change_start);
                break :blk cloned;
            }
            self.restore(change_start);
            if (try self.cloneLetOfCase(let_, value_expr)) |data| return data;
            break :blk try self.cloneExpr(let_.rest);
        };
        return .{ .let_ = .{
            .bind = try self.clonePat(let_.bind),
            .value = value_expr,
            .rest = rest,
            .comptime_site = let_.comptime_site,
        } };
    }

    fn cloneLetOfCase(self: *Cloner, let_: anytype, value_expr: Ast.ExprId) Common.LowerError!?Ast.ExprData {
        const value_data = self.pass.program.exprs.items[@intFromEnum(value_expr)].data;
        const match = switch (value_data) {
            .match_ => |match| match,
            else => return null,
        };

        const branches = try self.pass.allocator.dupe(Ast.Branch, self.pass.program.branchSpan(match.branches));
        defer self.pass.allocator.free(branches);

        var rewritten = try self.pass.allocator.alloc(Ast.Branch, branches.len);
        defer self.pass.allocator.free(rewritten);

        for (branches, 0..) |branch, index| {
            const body = (try self.cloneLetCaseBranchBody(let_, branch.body)) orelse return null;
            rewritten[index] = .{
                .pat = branch.pat,
                .guard = branch.guard,
                .body = body,
            };
        }

        return .{ .match_ = .{
            .scrutinee = match.scrutinee,
            .branches = try self.pass.program.addBranchSpan(rewritten),
            .comptime_site = match.comptime_site,
        } };
    }

    fn cloneLetCaseBranchBody(self: *Cloner, let_: anytype, branch_body: Ast.ExprId) Common.LowerError!?Ast.ExprId {
        const branch_expr = self.pass.program.exprs.items[@intFromEnum(branch_body)];
        switch (branch_expr.data) {
            .block => |block| {
                const change_start = self.changes.items.len;

                const source = try self.pass.allocator.dupe(Ast.StmtId, self.pass.program.stmtSpan(block.statements));
                defer self.pass.allocator.free(source);

                var statements = std.ArrayList(Ast.StmtId).empty;
                defer statements.deinit(self.pass.allocator);
                for (source, 0..) |stmt, stmt_index| {
                    try self.cloneStmtInto(stmt, &statements, .{
                        .statements = source[stmt_index + 1 ..],
                        .final_expr = block.final_expr,
                    });
                }

                const final_value = try self.cloneExprValue(block.final_expr);
                const rest_ty = self.pass.program.exprs.items[@intFromEnum(let_.rest)].ty;
                if (!try self.bindPatToReusableValue(let_.bind, final_value)) {
                    if (try self.cloneDivergentAtType(block.final_expr, rest_ty)) |divergent| {
                        self.restore(change_start);
                        return try self.addExpr(.{ .ty = rest_ty, .data = .{ .block = .{
                            .statements = try self.pass.program.addStmtSpan(statements.items),
                            .final_expr = divergent,
                        } } });
                    }
                    self.restore(change_start);
                    return null;
                }

                const rest = try self.cloneExpr(let_.rest);
                self.restore(change_start);

                return try self.addExpr(.{ .ty = rest_ty, .data = .{ .block = .{
                    .statements = try self.pass.program.addStmtSpan(statements.items),
                    .final_expr = rest,
                } } });
            },
            else => {
                const branch_value = try self.cloneExprValue(branch_body);
                const change_start = self.changes.items.len;
                if (!try self.bindPatToReusableValue(let_.bind, branch_value)) {
                    self.restore(change_start);
                    return null;
                }
                const rest = try self.cloneExpr(let_.rest);
                self.restore(change_start);
                return rest;
            },
        }
    }

    fn cloneDivergentAtType(self: *Cloner, expr_id: Ast.ExprId, ty: Type.TypeId) Common.LowerError!?Ast.ExprId {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .crash => |msg| try self.addExpr(.{ .ty = ty, .data = .{ .crash = msg } }),
            .comptime_exhaustiveness_failed => |site| try self.addExpr(.{ .ty = ty, .data = .{ .comptime_exhaustiveness_failed = site } }),
            .return_ => |value| try self.addExpr(.{ .ty = ty, .data = .{ .return_ = try self.cloneExpr(value) } }),
            else => null,
        };
    }

    fn scopedLocalValue(self: *Cloner, local: Ast.TypedLocal) Common.LowerError!?Value {
        if (!self.localCanBeReferencedDirectly(local.local)) return null;
        return .{ .expr = try self.addExpr(.{
            .ty = local.ty,
            .data = .{ .local = local.local },
        }) };
    }

    fn localCanBeReferencedDirectly(self: *Cloner, local: Ast.LocalId) bool {
        const current_fn = self.pass.program.fns.items[@intFromEnum(self.source_fn)];
        if (localInTypedLocalSpan(self.pass.program.typedLocalSpan(current_fn.captures), local)) return true;
        if (localInTypedLocalSpan(self.pass.program.typedLocalSpan(current_fn.args), local)) {
            return self.source_arg_locals_in_scope;
        }

        for (self.inline_stack.items) |active| {
            if (active.fn_id == self.source_fn) continue;
            const active_fn = self.pass.program.fns.items[@intFromEnum(active.fn_id)];
            if (localInTypedLocalSpan(self.pass.program.typedLocalSpan(active_fn.args), local)) return false;
            if (localInTypedLocalSpan(self.pass.program.typedLocalSpan(active_fn.captures), local)) return false;
        }

        return false;
    }

    fn cloneLoop(self: *Cloner, ty: Type.TypeId, loop: anytype) Common.LowerError!Ast.ExprId {
        const params = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(loop.params));
        defer self.pass.allocator.free(params);
        const initial_values = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(loop.initial_values));
        defer self.pass.allocator.free(initial_values);
        if (params.len != initial_values.len) Common.invariant("loop parameter count differed from initial value count");

        const values = try self.pass.allocator.alloc(Value, initial_values.len);
        defer self.pass.allocator.free(values);
        for (initial_values, 0..) |initial, index| {
            values[index] = try self.cloneExprValueDemandingKnownValue(initial);
        }
        return try self.cloneLoopFromInitialValues(ty, loop, params, values);
    }

    fn cloneLoopFromInitialValues(
        self: *Cloner,
        ty: Type.TypeId,
        loop: anytype,
        params: []const Ast.TypedLocal,
        values: []const Value,
    ) Common.LowerError!Ast.ExprId {
        if (try self.cloneLoopUnwrappedLet(ty, loop, params, values)) |unwrapped| return unwrapped;
        if (try self.cloneLoopDistributedIf(ty, loop, params, values)) |distributed| return distributed;

        const known_values = try self.pass.arena.allocator().alloc(KnownValue, values.len);
        var has_constructor = false;
        for (values, 0..) |value, index| {
            const initial_value_ty = valueType(self.pass.program, value);
            if (try self.pass.knownValueFromValue(value)) |known_value| {
                if (try self.projectableLoopKnownValueForValue(known_value, value)) |loop_known_value| {
                    known_values[index] = loop_known_value;
                    has_constructor = true;
                } else {
                    known_values[index] = .{ .any = initial_value_ty };
                }
            } else {
                known_values[index] = .{ .any = initial_value_ty };
            }
        }
        if (!has_constructor) {
            const initial_span = try self.valuesToExprSpan(values);
            return try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
                .params = loop.params,
                .initial_values = initial_span,
                .body = try self.cloneExpr(loop.body),
            } } });
        }

        while (true) {
            const change_start = self.changes.items.len;
            defer self.restore(change_start);

            var new_params = std.ArrayList(Ast.TypedLocal).empty;
            defer new_params.deinit(self.pass.allocator);

            var new_initials = std.ArrayList(Ast.ExprId).empty;
            defer new_initials.deinit(self.pass.allocator);

            var initial_split_failed = false;
            for (params, known_values, values, 0..) |param, *known_value, value, index| {
                const param_value = try self.valueFromKnownValueArgs(known_value.*, &new_params);
                try self.putSubst(param.local, param_value);
                if (!try self.appendFieldReadExprsFromValue(known_value.*, value, &new_initials)) {
                    const downgrade_ty = known_valueType(known_value.*);
                    known_values[index] = .{ .any = downgrade_ty };
                    initial_split_failed = true;
                    break;
                }
            }
            if (initial_split_failed) continue;
            const refinements = try self.pass.allocator.alloc(?KnownValue, known_values.len);
            defer self.pass.allocator.free(refinements);
            @memset(refinements, null);

            try self.loop_stack.append(self.pass.allocator, .{
                .values = known_values,
                .refinements = refinements,
            });
            const body = try self.cloneExpr(loop.body);
            _ = self.loop_stack.pop();

            var refined = false;
            for (known_values, refinements, 0..) |*known_value, maybe_refinement, index| {
                const refinement = maybe_refinement orelse continue;
                if (known_valueEql(self.pass.program, known_value.*, refinement)) continue;
                known_values[index] = refinement;
                refined = true;
            }
            if (refined) continue;

            if (knownValuesContainFiniteState(known_values)) {
                return try self.cloneStateLoopFromKnownValues(ty, loop, params, values, known_values);
            }

            return try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
                .params = try self.pass.program.addTypedLocalSpan(new_params.items),
                .initial_values = try self.pass.program.addExprSpan(new_initials.items),
                .body = body,
            } } });
        }
    }

    fn cloneStateLoopFromKnownValues(
        self: *Cloner,
        ty: Type.TypeId,
        loop: anytype,
        params: []const Ast.TypedLocal,
        values: []const Value,
        known_values: []const KnownValue,
    ) Common.LowerError!Ast.ExprId {
        const state_keys = try self.expandStateKnownValues(known_values);
        if (state_keys.len < 2) Common.invariant("state_loop requested without multiple finite states");

        const state_start: u32 = @intCast(self.pass.program.state_loop_states.items.len);
        try self.pass.program.state_loop_states.ensureUnusedCapacity(self.pass.program.allocator, state_keys.len);

        const states = try self.pass.allocator.alloc(StateLoopKnownState, state_keys.len);
        defer self.pass.allocator.free(states);

        for (state_keys, 0..) |state_values, index| {
            if (state_values.len != params.len) Common.invariant("state_loop key arity differed from loop params");
            const state_id: Ast.StateLoopStateId = @enumFromInt(state_start + @as(u32, @intCast(index)));
            states[index] = .{
                .id = state_id,
                .values = state_values,
            };
            self.pass.program.state_loop_states.appendAssumeCapacity(undefined);
        }

        const entry_state = self.stateForValues(states, values) orelse
            Common.invariant("state_loop initial values did not match any state");

        var entry_values = std.ArrayList(Ast.ExprId).empty;
        defer entry_values.deinit(self.pass.allocator);
        for (entry_state.values, values) |known_value, value| {
            if (!try self.appendFieldReadExprsFromValue(known_value, value, &entry_values)) {
                Common.invariant("state_loop initial value could not be split into entry state params");
            }
        }

        try self.state_loop_stack.append(self.pass.allocator, .{ .states = states });
        defer _ = self.state_loop_stack.pop();

        for (states) |state| {
            const change_start = self.changes.items.len;
            defer self.restore(change_start);

            var state_params = std.ArrayList(Ast.TypedLocal).empty;
            defer state_params.deinit(self.pass.allocator);

            for (params, state.values) |param, known_value| {
                const param_value = try self.valueFromKnownValueArgs(known_value, &state_params);
                try self.putSubst(param.local, param_value);
            }

            self.pass.program.state_loop_states.items[@intFromEnum(state.id)] = .{
                .params = try self.pass.program.addTypedLocalSpan(state_params.items),
                .body = try self.cloneExpr(loop.body),
            };
        }

        const state_span: Ast.Span(Ast.StateLoopState) = .{
            .start = state_start,
            .len = @intCast(states.len),
        };
        return try self.addExpr(.{ .ty = ty, .data = .{ .state_loop = .{
            .entry_state = entry_state.id,
            .entry_values = try self.pass.program.addExprSpan(entry_values.items),
            .states = state_span,
        } } });
    }

    fn expandStateKnownValues(self: *Cloner, known_values: []const KnownValue) Allocator.Error![]const []const KnownValue {
        return try self.knownValueProducts(known_values);
    }

    fn knownValueProducts(self: *Cloner, known_values: []const KnownValue) Allocator.Error![]const []const KnownValue {
        const options = try self.pass.allocator.alloc([]const KnownValue, known_values.len);
        defer self.pass.allocator.free(options);
        for (known_values, 0..) |known_value, index| {
            options[index] = try self.expandKnownValue(known_value);
        }

        var products = std.ArrayList([]const KnownValue).empty;
        defer products.deinit(self.pass.allocator);
        const current = try self.pass.allocator.alloc(KnownValue, known_values.len);
        defer self.pass.allocator.free(current);

        try self.appendKnownValueProducts(options, 0, current, &products);
        return try self.pass.arena.allocator().dupe([]const KnownValue, products.items);
    }

    fn appendKnownValueProducts(
        self: *Cloner,
        options: []const []const KnownValue,
        index: usize,
        current: []KnownValue,
        products: *std.ArrayList([]const KnownValue),
    ) Allocator.Error!void {
        if (index == options.len) {
            try products.append(self.pass.allocator, try self.pass.arena.allocator().dupe(KnownValue, current));
            return;
        }

        for (options[index]) |option| {
            current[index] = option;
            try self.appendKnownValueProducts(options, index + 1, current, products);
        }
    }

    fn expandKnownValue(self: *Cloner, known_value: KnownValue) Allocator.Error![]const KnownValue {
        return switch (known_value) {
            .any,
            .leaf,
            => try self.singleKnownValue(known_value),
            .tag => |tag| try self.expandKnownTag(tag),
            .record => |record| try self.expandKnownRecord(record),
            .tuple => |tuple| try self.expandKnownTuple(tuple),
            .nominal => |nominal| try self.expandKnownNominal(nominal),
            .callable => |callable| try self.expandKnownCallable(callable),
            .finite_tags => |finite_tags| try self.expandKnownTags(finite_tags),
            .finite_callables => |finite_callables| try self.expandKnownCallables(finite_callables),
        };
    }

    fn singleKnownValue(self: *Cloner, known_value: KnownValue) Allocator.Error![]const KnownValue {
        const values = try self.pass.arena.allocator().alloc(KnownValue, 1);
        values[0] = known_value;
        return values;
    }

    fn expandKnownRecord(self: *Cloner, record: KnownRecord) Allocator.Error![]const KnownValue {
        const child_values = try self.pass.allocator.alloc(KnownValue, record.fields.len);
        defer self.pass.allocator.free(child_values);
        for (record.fields, 0..) |field, index| {
            child_values[index] = field.known_value;
        }

        const products = try self.knownValueProducts(child_values);
        const alternatives = try self.pass.arena.allocator().alloc(KnownValue, products.len);
        for (products, alternatives) |product, *out| {
            const fields = try self.pass.arena.allocator().alloc(KnownField, record.fields.len);
            for (record.fields, product, fields) |field, field_known_value, *field_out| {
                field_out.* = .{
                    .name = field.name,
                    .known_value = field_known_value,
                };
            }
            out.* = .{ .record = .{
                .ty = record.ty,
                .fields = fields,
            } };
        }
        return alternatives;
    }

    fn expandKnownTuple(self: *Cloner, tuple: KnownTuple) Allocator.Error![]const KnownValue {
        const products = try self.knownValueProducts(tuple.items);
        const alternatives = try self.pass.arena.allocator().alloc(KnownValue, products.len);
        for (products, alternatives) |product, *out| {
            out.* = .{ .tuple = .{
                .ty = tuple.ty,
                .items = product,
            } };
        }
        return alternatives;
    }

    fn expandKnownNominal(self: *Cloner, nominal: KnownNominal) Allocator.Error![]const KnownValue {
        const backing_alternatives = try self.expandKnownValue(nominal.backing.*);
        const alternatives = try self.pass.arena.allocator().alloc(KnownValue, backing_alternatives.len);
        for (backing_alternatives, alternatives) |backing, *out| {
            const stored = try self.pass.arena.allocator().create(KnownValue);
            stored.* = backing;
            out.* = .{ .nominal = .{
                .ty = nominal.ty,
                .backing = stored,
            } };
        }
        return alternatives;
    }

    fn expandKnownTag(self: *Cloner, tag: KnownTag) Allocator.Error![]const KnownValue {
        const products = try self.knownValueProducts(tag.payloads);
        const alternatives = try self.pass.arena.allocator().alloc(KnownValue, products.len);
        for (products, alternatives) |product, *out| {
            out.* = .{ .tag = .{
                .ty = tag.ty,
                .name = tag.name,
                .payloads = product,
            } };
        }
        return alternatives;
    }

    fn expandKnownTags(self: *Cloner, finite_tags: KnownTags) Allocator.Error![]const KnownValue {
        var alternatives = std.ArrayList(KnownValue).empty;
        defer alternatives.deinit(self.pass.allocator);
        for (finite_tags.alternatives) |tag| {
            const expanded = try self.expandKnownTag(tag);
            try alternatives.appendSlice(self.pass.allocator, expanded);
        }
        return try self.pass.arena.allocator().dupe(KnownValue, alternatives.items);
    }

    fn expandKnownCallable(self: *Cloner, callable: KnownCallable) Allocator.Error![]const KnownValue {
        const products = try self.knownValueProducts(callable.captures);
        const alternatives = try self.pass.arena.allocator().alloc(KnownValue, products.len);
        for (products, alternatives) |product, *out| {
            out.* = .{ .callable = .{
                .ty = callable.ty,
                .fn_id = callable.fn_id,
                .captures = product,
            } };
        }
        return alternatives;
    }

    fn expandKnownCallables(self: *Cloner, finite_callables: KnownCallables) Allocator.Error![]const KnownValue {
        var alternatives = std.ArrayList(KnownValue).empty;
        defer alternatives.deinit(self.pass.allocator);
        for (finite_callables.alternatives) |callable| {
            const expanded = try self.expandKnownCallable(callable);
            try alternatives.appendSlice(self.pass.allocator, expanded);
        }
        return try self.pass.arena.allocator().dupe(KnownValue, alternatives.items);
    }

    fn stateForValues(self: *Cloner, states: []const StateLoopKnownState, values: []const Value) ?StateLoopKnownState {
        var found: ?StateLoopKnownState = null;
        for (states) |state| {
            if (!knownValuesMatchValues(self.pass.program, state.values, values)) continue;
            if (found != null) Common.invariant("state_loop edge matched multiple states");
            found = state;
        }
        return found;
    }

    fn cloneLoopUnwrappedLet(
        self: *Cloner,
        ty: Type.TypeId,
        loop: anytype,
        params: []const Ast.TypedLocal,
        values: []const Value,
    ) Common.LowerError!?Ast.ExprId {
        for (values, 0..) |value, value_index| {
            const let_value = switch (value) {
                .let_ => |let_value| let_value,
                else => continue,
            };

            var unwrapped_values = try self.pass.allocator.dupe(Value, values);
            defer self.pass.allocator.free(unwrapped_values);
            unwrapped_values[value_index] = let_value.body.*;

            const change_start = self.changes.items.len;
            defer self.restore(change_start);
            try self.bindPendingLetKnownValues(let_value.lets);

            const body = try self.cloneLoopFromInitialValues(ty, loop, params, unwrapped_values);
            return try self.wrapPendingLetsAroundExpr(ty, body, let_value.lets);
        }

        return null;
    }

    fn cloneLoopDistributedIf(
        self: *Cloner,
        ty: Type.TypeId,
        loop: anytype,
        params: []const Ast.TypedLocal,
        values: []const Value,
    ) Common.LowerError!?Ast.ExprId {
        for (values, 0..) |value, value_index| {
            const if_value = switch (value) {
                .if_ => |if_value| if_value,
                else => continue,
            };
            const branches = try self.pass.allocator.alloc(Ast.IfBranch, if_value.branches.len);
            defer self.pass.allocator.free(branches);
            var branch_values = try self.pass.allocator.dupe(Value, values);
            defer self.pass.allocator.free(branch_values);

            for (if_value.branches, 0..) |branch, branch_index| {
                branch_values[value_index] = branch.body;
                branches[branch_index] = .{
                    .cond = branch.cond,
                    .body = try self.cloneLoopFromInitialValues(ty, loop, params, branch_values),
                };
            }

            branch_values[value_index] = if_value.final_else.*;
            const final_else = try self.cloneLoopFromInitialValues(ty, loop, params, branch_values);

            return try self.addExpr(.{ .ty = ty, .data = .{ .if_ = .{
                .branches = try self.pass.program.addIfBranchSpan(branches),
                .final_else = final_else,
            } } });
        }

        return null;
    }

    fn projectableLoopKnownValueForValue(self: *Cloner, known_value: KnownValue, value: Value) Allocator.Error!?KnownValue {
        return switch (value) {
            .record => |record_value| blk: {
                const record_known_value = switch (known_value) {
                    .record => |record| record,
                    else => break :blk null,
                };
                if (record_known_value.fields.len != record_value.fields.len) Common.invariant("record loop state changed field count before specialization");
                const fields = try self.pass.arena.allocator().alloc(KnownField, record_known_value.fields.len);
                for (record_known_value.fields, record_value.fields, 0..) |field_known_value, field_value, index| {
                    if (field_known_value.name != field_value.name) Common.invariant("record loop state changed field order before specialization");
                    const projected = try self.projectableLoopKnownValueForValue(field_known_value.known_value, field_value.value);
                    fields[index] = .{
                        .name = field_known_value.name,
                        .known_value = projected orelse .{ .any = known_valueType(field_known_value.known_value) },
                    };
                }
                break :blk KnownValue{ .record = .{
                    .ty = record_known_value.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple_value| blk: {
                const tuple_known_value = switch (known_value) {
                    .tuple => |tuple| tuple,
                    else => break :blk null,
                };
                if (tuple_known_value.items.len != tuple_value.items.len) Common.invariant("tuple loop state changed item count before specialization");
                const items = try self.pass.arena.allocator().alloc(KnownValue, tuple_known_value.items.len);
                for (tuple_known_value.items, tuple_value.items, 0..) |item_known_value, item_value, index| {
                    items[index] = (try self.projectableLoopKnownValueForValue(item_known_value, item_value)) orelse
                        .{ .any = known_valueType(item_known_value) };
                }
                break :blk KnownValue{ .tuple = .{
                    .ty = tuple_known_value.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal_value| blk: {
                const nominal_known_value = switch (known_value) {
                    .nominal => |nominal| nominal,
                    else => break :blk null,
                };
                const backing = (try self.projectableLoopKnownValueForValue(nominal_known_value.backing.*, nominal_value.backing.*)) orelse break :blk null;
                const stored = try self.pass.arena.allocator().create(KnownValue);
                stored.* = backing;
                break :blk KnownValue{ .nominal = .{
                    .ty = nominal_known_value.ty,
                    .backing = stored,
                } };
            },
            .callable => |callable_value| blk: {
                if (knownValueMatchesValue(self.pass.program, known_value, value)) break :blk known_value;
                if (known_value == .finite_callables) {
                    const finite_callables = known_value.finite_callables;
                    if (finiteCallableAlternativeIndex(self.pass.program, finite_callables.alternatives, callable_value) != null) {
                        break :blk known_value;
                    }
                }
                const callable_known_value = switch (known_value) {
                    .callable => |callable| callable,
                    else => break :blk null,
                };
                if (!callableTargetMatches(self.pass.program, callable_known_value.fn_id, callable_value.fn_id) or
                    callable_known_value.captures.len != callable_value.captures.len)
                {
                    break :blk null;
                }
                const captures = try self.pass.arena.allocator().alloc(KnownValue, callable_known_value.captures.len);
                for (callable_known_value.captures, callable_value.captures, 0..) |capture_known_value, capture_value, index| {
                    const projected = try self.projectableLoopKnownValueForValue(capture_known_value, capture_value);
                    captures[index] = projected orelse .{ .any = known_valueType(capture_known_value) };
                }
                break :blk KnownValue{ .callable = .{
                    .ty = callable_known_value.ty,
                    .fn_id = callable_known_value.fn_id,
                    .captures = captures,
                } };
            },
            .finite_tags => |finite_value| blk: {
                const finite_known_value = switch (known_value) {
                    .finite_tags => |finite_tags| finite_tags,
                    else => break :blk null,
                };
                if (!knownTagsMatchesValue(self.pass.program, finite_known_value, finite_value)) break :blk null;
                break :blk known_value;
            },
            .finite_callables => |finite_value| blk: {
                const finite_known_value = switch (known_value) {
                    .finite_callables => |finite_callables| finite_callables,
                    else => break :blk null,
                };
                if (!knownCallablesMatchesValue(self.pass.program, finite_known_value, finite_value)) break :blk null;
                break :blk known_value;
            },
            .let_ => |let_value| try self.projectableLoopKnownValueForValue(known_value, let_value.body.*),
            .if_ => null,
            .expr_with_known_value => |known| if (canReadFieldsFromExpr(self.pass.program, known.expr))
                try self.projectableLoopKnownValueFromExpr(known.known_value)
            else
                null,
            .tag => if (knownValueMatchesValue(self.pass.program, known_value, value)) known_value else switch (known_value) {
                .finite_tags => |finite_tags| if (finiteTagAlternativeIndex(self.pass.program, finite_tags.alternatives, value.tag) != null) known_value else null,
                else => null,
            },
            .expr,
            => if (known_valueCanProjectFromExpr(known_value)) known_value else null,
        };
    }

    fn projectableLoopKnownValueFromExpr(self: *Cloner, known_value: KnownValue) Allocator.Error!?KnownValue {
        if (known_valueCanProjectFromExpr(known_value)) return known_value;

        return switch (known_value) {
            .any => known_value,
            .leaf => known_value,
            .record => |record| blk: {
                const fields = try self.pass.arena.allocator().alloc(KnownField, record.fields.len);
                for (record.fields, fields) |field, *out| {
                    out.* = .{
                        .name = field.name,
                        .known_value = (try self.projectableLoopKnownValueFromExpr(field.known_value)) orelse
                            .{ .any = known_valueType(field.known_value) },
                    };
                }
                break :blk KnownValue{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.pass.arena.allocator().alloc(KnownValue, tuple.items.len);
                for (tuple.items, items) |item, *out| {
                    out.* = (try self.projectableLoopKnownValueFromExpr(item)) orelse
                        .{ .any = known_valueType(item) };
                }
                break :blk KnownValue{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing = (try self.projectableLoopKnownValueFromExpr(nominal.backing.*)) orelse break :blk null;
                const stored = try self.pass.arena.allocator().create(KnownValue);
                stored.* = backing;
                break :blk KnownValue{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } };
            },
            .tag,
            .callable,
            .finite_tags,
            .finite_callables,
            => null,
        };
    }

    fn cloneBlock(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Ast.ExprId {
        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const source = try self.pass.allocator.dupe(Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        var statements = std.ArrayList(Ast.StmtId).empty;
        defer statements.deinit(self.pass.allocator);
        for (source, 0..) |stmt, index| {
            try self.cloneStmtInto(stmt, &statements, .{
                .statements = source[index + 1 ..],
                .final_expr = block.final_expr,
            });
        }

        return try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(statements.items),
            .final_expr = try self.cloneExpr(block.final_expr),
        } } });
    }

    fn cloneBlockValue(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Value {
        return try self.cloneBlockValueWithFinalDemand(ty, block, false);
    }

    fn cloneBlockValueDemandingKnownValue(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Value {
        return try self.cloneBlockValueWithFinalDemand(ty, block, true);
    }

    fn cloneBlockValueWithFinalDemand(
        self: *Cloner,
        ty: Type.TypeId,
        block: anytype,
        demand_final_known_value: bool,
    ) Common.LowerError!Value {
        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const source = try self.pass.allocator.dupe(Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        var statements = std.ArrayList(Ast.StmtId).empty;
        defer statements.deinit(self.pass.allocator);
        for (source, 0..) |stmt, index| {
            try self.cloneStmtInto(stmt, &statements, .{
                .statements = source[index + 1 ..],
                .final_expr = block.final_expr,
            });
        }

        const final_value = if (demand_final_known_value)
            try self.cloneExprValueDemandingKnownValue(block.final_expr)
        else
            try self.cloneExprValue(block.final_expr);
        if (demand_final_known_value) {
            if (statements.items.len == 0) return final_value;

            var pending_lets = std.ArrayList(PendingLet).empty;
            defer pending_lets.deinit(self.pass.allocator);
            if (try self.appendPendingLetsFromStatements(statements.items, &pending_lets)) {
                return try self.wrapPendingLets(final_value, pending_lets.items, true);
            }
        }

        const final_expr = try self.materialize(final_value);
        const block_expr = try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(statements.items),
            .final_expr = final_expr,
        } } });

        const known_value = (try self.pass.knownValueFromValue(final_value)) orelse return .{ .expr = block_expr };
        return .{ .expr_with_known_value = .{
            .expr = block_expr,
            .known_value = known_value,
        } };
    }

    fn cloneContinue(self: *Cloner, ty: Type.TypeId, continue_: anytype) Common.LowerError!Ast.ExprData {
        const loop = self.loop_stack.getLastOrNull() orelse {
            const state_loop = self.state_loop_stack.getLastOrNull() orelse return .{ .continue_ = .{
                .values = try self.cloneExprSpan(continue_.values),
            } };
            return try self.cloneStateContinue(ty, state_loop, continue_);
        };
        const values = self.pass.program.exprSpan(continue_.values);
        const source_values = try self.pass.allocator.dupe(Ast.ExprId, values);
        defer self.pass.allocator.free(source_values);
        if (source_values.len != loop.values.len) Common.invariant("continue value count differed from specialized loop pattern");

        var pending_statements = std.ArrayList(Ast.StmtId).empty;
        defer pending_statements.deinit(self.pass.allocator);

        const pending_change_start = self.changes.items.len;
        defer self.restore(pending_change_start);

        const continue_values = try self.pass.allocator.alloc(Value, source_values.len);
        defer self.pass.allocator.free(continue_values);

        for (source_values, 0..) |value_expr, index| {
            var value = try self.cloneExprValueDemandingKnownValue(value_expr);
            while (value == .let_) {
                try self.appendPendingLetStmts(value.let_.lets, &pending_statements);
                try self.bindPendingLetKnownValues(value.let_.lets);
                value = value.let_.body.*;
            }
            value = try self.hoistNestedLetsFromValue(value, &pending_statements);
            continue_values[index] = value;
        }

        const continue_data = try self.cloneContinueDataFromValues(ty, loop, continue_values);
        if (pending_statements.items.len == 0) return continue_data;

        const continue_expr = try self.addExpr(.{ .ty = ty, .data = continue_data });
        return .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(pending_statements.items),
            .final_expr = continue_expr,
        } };
    }

    fn cloneStateContinue(self: *Cloner, ty: Type.TypeId, state_loop: StateLoopPattern, continue_: anytype) Common.LowerError!Ast.ExprData {
        const values = self.pass.program.exprSpan(continue_.values);
        const source_values = try self.pass.allocator.dupe(Ast.ExprId, values);
        defer self.pass.allocator.free(source_values);
        if (state_loop.states.len == 0) Common.invariant("state_continue had no possible target states");

        const arity = state_loop.states[0].values.len;
        if (source_values.len != arity) Common.invariant("state_continue value count differed from state_loop arity");

        var pending_statements = std.ArrayList(Ast.StmtId).empty;
        defer pending_statements.deinit(self.pass.allocator);

        const pending_change_start = self.changes.items.len;
        defer self.restore(pending_change_start);

        const continue_values = try self.pass.allocator.alloc(Value, source_values.len);
        defer self.pass.allocator.free(continue_values);

        for (source_values, 0..) |value_expr, index| {
            var value = try self.cloneExprValueDemandingKnownValue(value_expr);
            while (value == .let_) {
                try self.appendPendingLetStmts(value.let_.lets, &pending_statements);
                try self.bindPendingLetKnownValues(value.let_.lets);
                value = value.let_.body.*;
            }
            value = try self.hoistNestedLetsFromValue(value, &pending_statements);
            continue_values[index] = value;
        }

        const continue_data = try self.cloneStateContinueDataFromValues(ty, state_loop, continue_values);
        if (pending_statements.items.len == 0) return continue_data;

        const continue_expr = try self.addExpr(.{ .ty = ty, .data = continue_data });
        return .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(pending_statements.items),
            .final_expr = continue_expr,
        } };
    }

    fn cloneStateContinueDataFromValues(
        self: *Cloner,
        ty: Type.TypeId,
        state_loop: StateLoopPattern,
        values: []const Value,
    ) Common.LowerError!Ast.ExprData {
        for (values, 0..) |value, value_index| {
            const let_value = switch (value) {
                .let_ => |let_value| let_value,
                else => continue,
            };

            var unwrapped_values = try self.pass.allocator.dupe(Value, values);
            defer self.pass.allocator.free(unwrapped_values);
            unwrapped_values[value_index] = let_value.body.*;

            const change_start = self.changes.items.len;
            defer self.restore(change_start);
            try self.bindPendingLetKnownValues(let_value.lets);

            const continue_expr = try self.addExpr(.{
                .ty = ty,
                .data = try self.cloneStateContinueDataFromValues(ty, state_loop, unwrapped_values),
            });
            const wrapped = try self.wrapPendingLetsAroundExpr(ty, continue_expr, let_value.lets);
            return .{ .block = .{
                .statements = try self.pass.program.addStmtSpan(&.{}),
                .final_expr = wrapped,
            } };
        }

        for (values, 0..) |value, value_index| {
            const if_value = switch (value) {
                .if_ => |if_value| if_value,
                else => continue,
            };

            const branches = try self.pass.allocator.alloc(Ast.IfBranch, if_value.branches.len);
            defer self.pass.allocator.free(branches);
            var branch_values = try self.pass.allocator.dupe(Value, values);
            defer self.pass.allocator.free(branch_values);

            for (if_value.branches, 0..) |branch, branch_index| {
                branch_values[value_index] = branch.body;
                branches[branch_index] = .{
                    .cond = branch.cond,
                    .body = try self.addExpr(.{
                        .ty = ty,
                        .data = try self.cloneStateContinueDataFromValues(ty, state_loop, branch_values),
                    }),
                };
            }

            branch_values[value_index] = if_value.final_else.*;
            const final_else = try self.addExpr(.{
                .ty = ty,
                .data = try self.cloneStateContinueDataFromValues(ty, state_loop, branch_values),
            });

            return .{ .if_ = .{
                .branches = try self.pass.program.addIfBranchSpan(branches),
                .final_else = final_else,
            } };
        }

        const target_state = self.stateForValues(state_loop.states, values) orelse
            Common.invariant("state_continue values did not match any state_loop state");

        var new_values = std.ArrayList(Ast.ExprId).empty;
        defer new_values.deinit(self.pass.allocator);

        for (target_state.values, values) |known_value, value| {
            if (knownValueMatchesValue(self.pass.program, known_value, value)) {
                try self.appendExprsFromValue(known_value, value, &new_values);
            } else if (!try self.appendFieldReadExprsFromValue(known_value, value, &new_values)) {
                Common.invariant("state_continue value could not be split into target state params");
            }
        }

        return .{ .state_continue = .{
            .target_state = target_state.id,
            .values = try self.pass.program.addExprSpan(new_values.items),
        } };
    }

    fn hoistNestedLetsFromValue(
        self: *Cloner,
        value: Value,
        pending_statements: *std.ArrayList(Ast.StmtId),
    ) Common.LowerError!Value {
        return switch (value) {
            .let_ => |let_value| blk: {
                try self.appendPendingLetStmts(let_value.lets, pending_statements);
                try self.bindPendingLetKnownValues(let_value.lets);
                break :blk try self.hoistNestedLetsFromValue(let_value.body.*, pending_statements);
            },
            .tag => |tag| blk: {
                const payloads = try self.pass.arena.allocator().alloc(Value, tag.payloads.len);
                for (tag.payloads, payloads) |payload, *out| {
                    out.* = try self.hoistNestedLetsFromValue(payload, pending_statements);
                }
                break :blk Value{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| blk: {
                const fields = try self.pass.arena.allocator().alloc(FieldValue, record.fields.len);
                for (record.fields, fields) |field, *out| {
                    out.* = .{
                        .name = field.name,
                        .value = try self.hoistNestedLetsFromValue(field.value, pending_statements),
                    };
                }
                break :blk Value{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.pass.arena.allocator().alloc(Value, tuple.items.len);
                for (tuple.items, items) |item, *out| {
                    out.* = try self.hoistNestedLetsFromValue(item, pending_statements);
                }
                break :blk Value{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = try self.hoistNestedLetsFromValue(nominal.backing.*, pending_statements);
                break :blk Value{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.pass.arena.allocator().alloc(Value, callable.captures.len);
                for (callable.captures, captures) |capture, *out| {
                    out.* = try self.hoistNestedLetsFromValue(capture, pending_statements);
                }
                break :blk Value{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
            .finite_tags => |finite_tags| blk: {
                const alternatives = try self.pass.arena.allocator().alloc(TagValue, finite_tags.alternatives.len);
                for (finite_tags.alternatives, alternatives) |alternative, *out| {
                    const payloads = try self.pass.arena.allocator().alloc(Value, alternative.payloads.len);
                    for (alternative.payloads, payloads) |payload, *payload_out| {
                        payload_out.* = try self.hoistNestedLetsFromValue(payload, pending_statements);
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .name = alternative.name,
                        .payloads = payloads,
                    };
                }
                break :blk Value{ .finite_tags = .{
                    .ty = finite_tags.ty,
                    .selector = finite_tags.selector,
                    .alternatives = alternatives,
                } };
            },
            .finite_callables => |finite_callables| blk: {
                const alternatives = try self.pass.arena.allocator().alloc(CallableValue, finite_callables.alternatives.len);
                for (finite_callables.alternatives, alternatives) |alternative, *out| {
                    const captures = try self.pass.arena.allocator().alloc(Value, alternative.captures.len);
                    for (alternative.captures, captures) |capture, *capture_out| {
                        capture_out.* = try self.hoistNestedLetsFromValue(capture, pending_statements);
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .fn_id = alternative.fn_id,
                        .captures = captures,
                    };
                }
                break :blk Value{ .finite_callables = .{
                    .ty = finite_callables.ty,
                    .selector = finite_callables.selector,
                    .alternatives = alternatives,
                } };
            },
            .if_,
            .expr,
            .expr_with_known_value,
            => value,
        };
    }

    fn cloneContinueDataFromValues(
        self: *Cloner,
        ty: Type.TypeId,
        loop: LoopPattern,
        values: []const Value,
    ) Common.LowerError!Ast.ExprData {
        for (values, 0..) |value, value_index| {
            const let_value = switch (value) {
                .let_ => |let_value| let_value,
                else => continue,
            };

            var unwrapped_values = try self.pass.allocator.dupe(Value, values);
            defer self.pass.allocator.free(unwrapped_values);
            unwrapped_values[value_index] = let_value.body.*;

            const change_start = self.changes.items.len;
            defer self.restore(change_start);
            try self.bindPendingLetKnownValues(let_value.lets);

            const continue_expr = try self.addExpr(.{
                .ty = ty,
                .data = try self.cloneContinueDataFromValues(ty, loop, unwrapped_values),
            });
            const wrapped = try self.wrapPendingLetsAroundExpr(ty, continue_expr, let_value.lets);
            return .{ .block = .{
                .statements = try self.pass.program.addStmtSpan(&.{}),
                .final_expr = wrapped,
            } };
        }

        for (values, 0..) |value, value_index| {
            const if_value = switch (value) {
                .if_ => |if_value| if_value,
                else => continue,
            };

            const branches = try self.pass.allocator.alloc(Ast.IfBranch, if_value.branches.len);
            defer self.pass.allocator.free(branches);
            var branch_values = try self.pass.allocator.dupe(Value, values);
            defer self.pass.allocator.free(branch_values);

            for (if_value.branches, 0..) |branch, branch_index| {
                branch_values[value_index] = branch.body;
                branches[branch_index] = .{
                    .cond = branch.cond,
                    .body = try self.addExpr(.{
                        .ty = ty,
                        .data = try self.cloneContinueDataFromValues(ty, loop, branch_values),
                    }),
                };
            }

            branch_values[value_index] = if_value.final_else.*;
            const final_else = try self.addExpr(.{
                .ty = ty,
                .data = try self.cloneContinueDataFromValues(ty, loop, branch_values),
            });

            return .{ .if_ = .{
                .branches = try self.pass.program.addIfBranchSpan(branches),
                .final_else = final_else,
            } };
        }

        var new_values = std.ArrayList(Ast.ExprId).empty;
        defer new_values.deinit(self.pass.allocator);

        for (loop.values, values, 0..) |known_value, value, index| {
            if (!knownValueMatchesValue(self.pass.program, known_value, value)) {
                if (!try self.appendFieldReadExprsFromValue(known_value, value, &new_values)) {
                    const refined = try self.refineLoopKnownValueForValue(known_value, value);
                    try self.noteLoopRefinement(loop, index, refined);
                    if (!try self.appendFieldReadExprsFromValue(refined, value, &new_values)) {
                        Common.invariant("refined continue value did not match specialized loop state");
                    }
                }
                continue;
            }
            try self.appendExprsFromValue(known_value, value, &new_values);
        }

        return .{ .continue_ = .{
            .values = try self.pass.program.addExprSpan(new_values.items),
        } };
    }

    fn noteLoopRefinement(self: *Cloner, loop: LoopPattern, index: usize, refinement: KnownValue) Allocator.Error!void {
        if (index >= loop.refinements.len) Common.invariant("loop refinement index exceeded active loop state");
        loop.refinements[index] = if (loop.refinements[index]) |existing|
            try self.commonLoopKnownValue(existing, refinement)
        else
            refinement;
    }

    fn refineLoopKnownValueForValue(self: *Cloner, known_value: KnownValue, value: Value) Common.LowerError!KnownValue {
        if (knownValueMatchesValue(self.pass.program, known_value, value)) return known_value;

        return switch (known_value) {
            .any => known_value,
            .leaf => known_value,
            .record => |record| blk: {
                const fields = try self.pass.arena.allocator().alloc(KnownField, record.fields.len);
                if (recordFromValue(value)) |record_value| {
                    if (record.fields.len != record_value.fields.len) Common.invariant("record loop state changed field count");
                    for (record.fields, record_value.fields, 0..) |field, field_value, index| {
                        if (field.name != field_value.name) Common.invariant("record loop state changed field order");
                        fields[index] = .{
                            .name = field.name,
                            .known_value = try self.refineLoopKnownValueForValue(field.known_value, field_value.value),
                        };
                    }
                    break :blk KnownValue{ .record = .{ .ty = record.ty, .fields = fields } };
                }

                const receiver = projectableExprFromValue(value) orelse break :blk KnownValue{ .any = record.ty };
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) break :blk KnownValue{ .any = record.ty };
                const actual_known_value = switch (value) {
                    .expr_with_known_value => |known_value_expr| known_value_expr.known_value,
                    else => null,
                };
                for (record.fields, 0..) |field, index| {
                    const actual_field = if (actual_known_value) |actual|
                        fieldKnownValueFromKnownValue(actual, field.name)
                    else
                        null;
                    const field_expr = try self.addExpr(.{ .ty = known_valueType(field.known_value), .data = .{ .field_access = .{
                        .receiver = receiver,
                        .field = field.name,
                    } } });
                    const field_value = if (actual_field) |actual|
                        valueFromProjectedExpr(field_expr, actual)
                    else
                        Value{ .expr = field_expr };
                    fields[index] = .{
                        .name = field.name,
                        .known_value = try self.refineLoopKnownValueForValue(field.known_value, field_value),
                    };
                }
                break :blk KnownValue{ .record = .{ .ty = record.ty, .fields = fields } };
            },
            .tuple => |tuple| blk: {
                const items = try self.pass.arena.allocator().alloc(KnownValue, tuple.items.len);
                if (tupleFromValue(value)) |tuple_value| {
                    if (tuple.items.len != tuple_value.items.len) Common.invariant("tuple loop state changed item count");
                    for (tuple.items, tuple_value.items, 0..) |item, item_value, index| {
                        items[index] = try self.refineLoopKnownValueForValue(item, item_value);
                    }
                    break :blk KnownValue{ .tuple = .{ .ty = tuple.ty, .items = items } };
                }

                const receiver = projectableExprFromValue(value) orelse break :blk KnownValue{ .any = tuple.ty };
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) break :blk KnownValue{ .any = tuple.ty };
                const actual_known_value = switch (value) {
                    .expr_with_known_value => |known_value_expr| known_value_expr.known_value,
                    else => null,
                };
                for (tuple.items, 0..) |item, index| {
                    const actual_item = if (actual_known_value) |actual|
                        itemKnownValueFromKnownValue(actual, @as(u32, @intCast(index)))
                    else
                        null;
                    const item_expr = try self.addExpr(.{ .ty = known_valueType(item), .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    const item_value = if (actual_item) |actual|
                        valueFromProjectedExpr(item_expr, actual)
                    else
                        Value{ .expr = item_expr };
                    items[index] = try self.refineLoopKnownValueForValue(item, item_value);
                }
                break :blk KnownValue{ .tuple = .{ .ty = tuple.ty, .items = items } };
            },
            .nominal => |nominal| blk: {
                const value_nominal = switch (value) {
                    .nominal => |nominal_value| nominal_value,
                    else => break :blk KnownValue{ .any = nominal.ty },
                };
                const backing = try self.pass.arena.allocator().create(KnownValue);
                backing.* = try self.refineLoopKnownValueForValue(nominal.backing.*, value_nominal.backing.*);
                break :blk KnownValue{ .nominal = .{ .ty = nominal.ty, .backing = backing } };
            },
            .tag => |tag| blk: {
                const value_tag = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => break :blk KnownValue{ .any = tag.ty },
                };
                const value_known_value = (try self.pass.knownValueFromValue(.{ .tag = value_tag })) orelse break :blk KnownValue{ .any = tag.ty };
                break :blk try self.commonLoopKnownValue(known_value, value_known_value);
            },
            .callable => |callable| blk: {
                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => break :blk KnownValue{ .any = callable.ty },
                };
                const value_known_value = (try self.pass.knownValueFromValue(.{ .callable = callable_value })) orelse break :blk KnownValue{ .any = callable.ty };
                break :blk try self.commonLoopKnownValue(known_value, value_known_value);
            },
            .finite_tags => |finite_tags| blk: {
                switch (value) {
                    .tag => |tag_value| {
                        const value_known_value = (try self.pass.knownValueFromValue(.{ .tag = tag_value })) orelse break :blk KnownValue{ .any = finite_tags.ty };
                        break :blk try self.commonLoopKnownValue(known_value, value_known_value);
                    },
                    .finite_tags => |finite_value| {
                        const value_known_value = (try self.pass.knownValueFromValue(.{ .finite_tags = finite_value })) orelse break :blk KnownValue{ .any = finite_tags.ty };
                        break :blk try self.commonLoopKnownValue(known_value, value_known_value);
                    },
                    else => break :blk KnownValue{ .any = finite_tags.ty },
                }
            },
            .finite_callables => |finite_callables| blk: {
                switch (value) {
                    .callable => |callable_value| {
                        const value_known_value = (try self.pass.knownValueFromValue(.{ .callable = callable_value })) orelse break :blk KnownValue{ .any = finite_callables.ty };
                        break :blk try self.commonLoopKnownValue(known_value, value_known_value);
                    },
                    .finite_callables => |finite_value| {
                        const value_known_value = (try self.pass.knownValueFromValue(.{ .finite_callables = finite_value })) orelse break :blk KnownValue{ .any = finite_callables.ty };
                        break :blk try self.commonLoopKnownValue(known_value, value_known_value);
                    },
                    else => break :blk KnownValue{ .any = finite_callables.ty },
                }
            },
        };
    }

    fn commonLoopKnownValue(self: *Cloner, lhs: KnownValue, rhs: KnownValue) Allocator.Error!KnownValue {
        if (known_valueEql(self.pass.program, lhs, rhs)) return lhs;
        const ty = known_valueType(lhs);
        if (!sameType(self.pass.program, ty, known_valueType(rhs))) Common.invariant("loop state refinement changed type");
        if (try commonKnownTags(self.pass.program, self.pass.arena.allocator(), lhs, rhs)) |finite_tags| {
            return finite_tags;
        }
        if (try commonKnownCallables(self.pass.program, self.pass.arena.allocator(), lhs, rhs)) |finite_callables| {
            return finite_callables;
        }
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return .{ .any = ty };

        return switch (lhs) {
            .any => .{ .any = ty },
            .leaf => .{ .leaf = ty },
            .record => |lhs_record| blk: {
                const rhs_record = rhs.record;
                if (lhs_record.fields.len != rhs_record.fields.len) break :blk KnownValue{ .any = ty };
                const fields = try self.pass.arena.allocator().alloc(KnownField, lhs_record.fields.len);
                for (lhs_record.fields, rhs_record.fields, 0..) |lhs_field, rhs_field, index| {
                    if (lhs_field.name != rhs_field.name) break :blk KnownValue{ .any = ty };
                    fields[index] = .{
                        .name = lhs_field.name,
                        .known_value = try self.commonLoopKnownValue(lhs_field.known_value, rhs_field.known_value),
                    };
                }
                break :blk KnownValue{ .record = .{ .ty = lhs_record.ty, .fields = fields } };
            },
            .tuple => |lhs_tuple| blk: {
                const rhs_tuple = rhs.tuple;
                if (lhs_tuple.items.len != rhs_tuple.items.len) break :blk KnownValue{ .any = ty };
                const items = try self.pass.arena.allocator().alloc(KnownValue, lhs_tuple.items.len);
                for (lhs_tuple.items, rhs_tuple.items, 0..) |lhs_item, rhs_item, index| {
                    items[index] = try self.commonLoopKnownValue(lhs_item, rhs_item);
                }
                break :blk KnownValue{ .tuple = .{ .ty = lhs_tuple.ty, .items = items } };
            },
            .nominal => |lhs_nominal| blk: {
                const rhs_nominal = rhs.nominal;
                const backing = try self.pass.arena.allocator().create(KnownValue);
                backing.* = try self.commonLoopKnownValue(lhs_nominal.backing.*, rhs_nominal.backing.*);
                break :blk KnownValue{ .nominal = .{ .ty = lhs_nominal.ty, .backing = backing } };
            },
            .callable => |lhs_callable| blk: {
                const rhs_callable = rhs.callable;
                if (!callableTargetMatches(self.pass.program, lhs_callable.fn_id, rhs_callable.fn_id) or
                    lhs_callable.captures.len != rhs_callable.captures.len)
                {
                    break :blk KnownValue{ .any = ty };
                }
                const captures = try self.pass.arena.allocator().alloc(KnownValue, lhs_callable.captures.len);
                for (lhs_callable.captures, rhs_callable.captures, 0..) |lhs_capture, rhs_capture, index| {
                    captures[index] = try self.commonLoopKnownValue(lhs_capture, rhs_capture);
                }
                break :blk KnownValue{ .callable = .{
                    .ty = lhs_callable.ty,
                    .fn_id = lhs_callable.fn_id,
                    .captures = captures,
                } };
            },
            .tag => .{ .any = ty },
            .finite_tags => .{ .any = ty },
            .finite_callables => .{ .any = ty },
        };
    }

    fn valuesToExprSpan(self: *Cloner, values: []const Value) Common.LowerError!Ast.Span(Ast.ExprId) {
        const exprs = try self.pass.allocator.alloc(Ast.ExprId, values.len);
        defer self.pass.allocator.free(exprs);
        for (values, 0..) |value, index| {
            exprs[index] = try self.materialize(value);
        }
        return try self.pass.program.addExprSpan(exprs);
    }

    fn cloneCallProcExpr(self: *Cloner, ty: Type.TypeId, call: @import("../monotype/ast.zig").CallProc) Common.LowerError!Ast.ExprId {
        const data = try self.cloneCallProcData(call);
        const cloned_call = switch (data) {
            .call_proc => |cloned| cloned,
            else => Common.invariant("direct call cloning produced a non-call expression"),
        };
        const call_expr = try self.addExpr(.{ .ty = ty, .data = data });
        return try self.wrapDirectCallCaptureLets(ty, Ast.callProcCallee(cloned_call), call_expr);
    }

    fn cloneCallProcData(self: *Cloner, call: @import("../monotype/ast.zig").CallProc) Common.LowerError!Ast.ExprData {
        if (call.is_cold) {
            return .{ .call_proc = .{
                .callee = call.callee,
                .args = try self.cloneExprSpan(call.args),
                .is_cold = true,
            } };
        }

        const callee = Ast.callProcCallee(call);
        const raw = @intFromEnum(callee);
        if (raw < self.pass.plans.len) {
            const source_args = self.pass.program.exprSpan(call.args);
            const args = try self.pass.allocator.dupe(Ast.ExprId, source_args);
            defer self.pass.allocator.free(args);

            const values = try self.pass.allocator.alloc(Value, args.len);
            defer self.pass.allocator.free(values);
            for (args, 0..) |arg, index| {
                values[index] = try self.cloneExprValue(arg);
            }
            if (self.record_call_patterns) {
                try self.pass.ensureCallPatternForValues(callee, values);
            }

            for (self.pass.plans[raw].specs.items) |spec| {
                const spec_fn_id = spec.fn_id orelse
                    Common.invariant("call-pattern specialization id was not assigned before cloning calls");
                var rewritten_args = std.ArrayList(Ast.ExprId).empty;
                defer rewritten_args.deinit(self.pass.allocator);

                if (try self.appendClonedCallArgs(spec.pattern, args, &rewritten_args)) {
                    return .{ .call_proc = .{
                        .callee = .{ .lifted = spec_fn_id },
                        .args = try self.pass.program.addExprSpan(rewritten_args.items),
                        .is_cold = call.is_cold,
                    } };
                }
            }
        }
        return .{ .call_proc = .{
            .callee = call.callee,
            .args = try self.cloneExprSpan(call.args),
            .is_cold = call.is_cold,
        } };
    }

    fn wrapDirectCallCaptureLets(self: *Cloner, ty: Type.TypeId, callee: Ast.FnId, call_expr: Ast.ExprId) Common.LowerError!Ast.ExprId {
        const callee_fn = self.pass.program.fns.items[@intFromEnum(callee)];
        const captures = self.pass.program.typedLocalSpan(callee_fn.captures);
        if (captures.len == 0) return call_expr;

        const values = try self.pass.allocator.alloc(?Ast.ExprId, captures.len);
        defer self.pass.allocator.free(values);
        for (captures, 0..) |capture, index| {
            const value = self.subst.get(capture.local) orelse {
                values[index] = null;
                continue;
            };
            const value_expr = try self.materialize(value);
            const value_local = localExpr(self.pass.program, value_expr);
            values[index] = if (value_local != null and value_local.? == capture.local) null else value_expr;
        }

        var result = call_expr;
        var index = values.len;
        while (index > 0) {
            index -= 1;
            const value_expr = values[index] orelse continue;
            const pat = try self.pass.program.addPat(.{
                .ty = captures[index].ty,
                .data = .{ .bind = captures[index].local },
            });
            result = try self.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = value_expr,
                .rest = result,
            } } });
        }
        return result;
    }

    fn appendClonedCallArgs(
        self: *Cloner,
        pattern: CallPattern,
        args: []const Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        if (pattern.args.len != args.len) Common.invariant("call-pattern arity differed from direct call arity");
        for (pattern.args, args) |known_value, arg| {
            if (!try self.appendClonedExprsForKnownValue(known_value, arg, out)) return false;
        }
        return true;
    }

    fn appendClonedExprsForKnownValue(
        self: *Cloner,
        known_value: KnownValue,
        expr_id: Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        switch (known_value) {
            .any => {
                try out.append(self.pass.allocator, try self.cloneExpr(expr_id));
                return true;
            },
            else => {
                const value = try self.valueForCallArg(expr_id);
                if (!knownValueMatchesValue(self.pass.program, known_value, value)) return false;
                try self.appendExprsFromValue(known_value, value, out);
                return true;
            },
        }
    }

    fn valueForCallArg(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        return try self.cloneExprValueDemandingKnownValue(expr_id);
    }

    fn appendExprsFromValue(
        self: *Cloner,
        known_value: KnownValue,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!void {
        if (value == .expr_with_known_value) switch (known_value) {
            .any => {},
            else => {
                if (!try self.appendFieldReadExprsFromValue(known_value, value, out)) {
                    Common.invariant("known-value expression could not be split into requested known_value");
                }
                return;
            },
        };

        switch (known_value) {
            .any,
            .leaf,
            => try out.append(self.pass.allocator, try self.materializePublic(value)),
            .tag => |tag| {
                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => Common.invariant("tag call pattern matched a non-tag value"),
                };
                for (tag.payloads, tag_value.payloads) |payload_known_value, payload| {
                    try self.appendExprsFromValue(payload_known_value, payload, out);
                }
            },
            .record => |record| {
                const record_value = switch (value) {
                    .record => |record_value| record_value,
                    else => Common.invariant("record call pattern matched a non-record value"),
                };
                for (record.fields) |field_known_value| {
                    const field_value = fieldFromRecord(record_value, field_known_value.name) orelse
                        Common.invariant("record call-pattern field was not present after matching");
                    try self.appendExprsFromValue(field_known_value.known_value, field_value, out);
                }
            },
            .tuple => |tuple| {
                const tuple_value = switch (value) {
                    .tuple => |tuple_value| tuple_value,
                    else => Common.invariant("tuple call pattern matched a non-tuple value"),
                };
                for (tuple.items, tuple_value.items) |item_known_value, item| {
                    try self.appendExprsFromValue(item_known_value, item, out);
                }
            },
            .nominal => |nominal| {
                const nominal_value = switch (value) {
                    .nominal => |nominal_value| nominal_value,
                    else => Common.invariant("nominal call pattern matched a non-nominal value"),
                };
                try self.appendExprsFromValue(nominal.backing.*, nominal_value.backing.*, out);
            },
            .callable => |callable| {
                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => Common.invariant("callable call pattern matched a non-callable value"),
                };
                for (callable.captures, callable_value.captures) |capture_known_value, capture_value| {
                    try self.appendExprsFromValue(capture_known_value, capture_value, out);
                }
            },
            .finite_callables => |finite_callables| {
                if (value == .finite_callables) {
                    const finite_value = value.finite_callables;
                    if (!knownCallablesMatchesValue(self.pass.program, finite_callables, finite_value)) {
                        Common.invariant("finite callable known_value matched a different finite callable value");
                    }
                    try out.append(self.pass.allocator, finite_value.selector);
                    for (finite_callables.alternatives, finite_value.alternatives) |alternative_known_value, alternative_value| {
                        if (!callableTargetMatches(self.pass.program, alternative_known_value.fn_id, alternative_value.fn_id) or
                            alternative_known_value.captures.len != alternative_value.captures.len)
                        {
                            Common.invariant("finite callable value alternatives changed after matching");
                        }
                        for (alternative_known_value.captures, alternative_value.captures) |capture_known_value, capture_value| {
                            try self.appendExprsFromValue(capture_known_value, capture_value, out);
                        }
                    }
                    return;
                }

                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => Common.invariant("finite callable call pattern matched a non-callable value"),
                };
                const active_index = finiteCallableAlternativeIndex(self.pass.program, finite_callables.alternatives, callable_value) orelse
                    Common.invariant("finite callable known_value did not contain the continued callable value");
                try out.append(self.pass.allocator, try self.selectorLiteral(@intCast(active_index)));
                for (finite_callables.alternatives, 0..) |alternative_known_value, alternative_index| {
                    if (alternative_index == active_index) {
                        for (alternative_known_value.captures, callable_value.captures) |capture_known_value, capture_value| {
                            try self.appendExprsFromValue(capture_known_value, capture_value, out);
                        }
                    } else {
                        for (alternative_known_value.captures) |capture_known_value| {
                            try self.appendUninitializedExprsForKnownValue(capture_known_value, out);
                        }
                    }
                }
            },
            .finite_tags => |finite_tags| {
                if (value == .finite_tags) {
                    const finite_value = value.finite_tags;
                    if (!knownTagsMatchesValue(self.pass.program, finite_tags, finite_value)) {
                        Common.invariant("finite tag known_value matched a different finite tag value");
                    }
                    try out.append(self.pass.allocator, finite_value.selector);
                    for (finite_tags.alternatives, finite_value.alternatives) |alternative_known_value, alternative_value| {
                        if (alternative_known_value.name != alternative_value.name or alternative_known_value.payloads.len != alternative_value.payloads.len) {
                            Common.invariant("finite tag value alternatives changed after matching");
                        }
                        for (alternative_known_value.payloads, alternative_value.payloads) |payload_known_value, payload_value| {
                            try self.appendExprsFromValue(payload_known_value, payload_value, out);
                        }
                    }
                    return;
                }

                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => Common.invariant("finite tag call pattern matched a non-tag value"),
                };
                const active_index = finiteTagAlternativeIndex(self.pass.program, finite_tags.alternatives, tag_value) orelse
                    Common.invariant("finite tag known_value did not contain the continued tag value");
                try out.append(self.pass.allocator, try self.selectorLiteral(@intCast(active_index)));
                for (finite_tags.alternatives, 0..) |alternative_known_value, alternative_index| {
                    if (alternative_index == active_index) {
                        for (alternative_known_value.payloads, tag_value.payloads) |payload_known_value, payload_value| {
                            try self.appendExprsFromValue(payload_known_value, payload_value, out);
                        }
                    } else {
                        for (alternative_known_value.payloads) |payload_known_value| {
                            try self.appendUninitializedExprsForKnownValue(payload_known_value, out);
                        }
                    }
                }
            },
        }
    }

    fn appendUninitializedExprsForKnownValue(
        self: *Cloner,
        known_value: KnownValue,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!void {
        switch (known_value) {
            .any,
            .leaf,
            => |ty| try out.append(self.pass.allocator, try self.addExpr(.{ .ty = ty, .data = .uninitialized })),
            .tag => |tag| {
                for (tag.payloads) |payload| try self.appendUninitializedExprsForKnownValue(payload, out);
            },
            .record => |record| {
                for (record.fields) |field| try self.appendUninitializedExprsForKnownValue(field.known_value, out);
            },
            .tuple => |tuple| {
                for (tuple.items) |item| try self.appendUninitializedExprsForKnownValue(item, out);
            },
            .nominal => |nominal| try self.appendUninitializedExprsForKnownValue(nominal.backing.*, out),
            .callable => |callable| {
                for (callable.captures) |capture| try self.appendUninitializedExprsForKnownValue(capture, out);
            },
            .finite_callables => |finite_callables| {
                const selector_ty = try self.pass.primitiveType(.u64);
                try out.append(self.pass.allocator, try self.addExpr(.{ .ty = selector_ty, .data = .uninitialized }));
                for (finite_callables.alternatives) |alternative| {
                    for (alternative.captures) |capture| try self.appendUninitializedExprsForKnownValue(capture, out);
                }
            },
            .finite_tags => |finite_tags| {
                const selector_ty = try self.pass.primitiveType(.u64);
                try out.append(self.pass.allocator, try self.addExpr(.{ .ty = selector_ty, .data = .uninitialized }));
                for (finite_tags.alternatives) |alternative| {
                    for (alternative.payloads) |payload| try self.appendUninitializedExprsForKnownValue(payload, out);
                }
            },
        }
    }

    fn appendFieldReadExprsFromValue(
        self: *Cloner,
        known_value: KnownValue,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        if (value != .expr_with_known_value and knownValueMatchesValue(self.pass.program, known_value, value)) {
            try self.appendExprsFromValue(known_value, value, out);
            return true;
        }

        switch (known_value) {
            .any,
            .leaf,
            => {
                try out.append(self.pass.allocator, try self.materializePublic(value));
                return true;
            },
            .record => |record| {
                if (recordFromValue(value)) |record_value| {
                    for (record.fields) |field_known_value| {
                        const field_value = fieldFromRecord(record_value, field_known_value.name) orelse return false;
                        if (!try self.appendFieldReadExprsFromValue(field_known_value.known_value, field_value, out)) return false;
                    }
                    return true;
                }

                const receiver = projectableExprFromValue(value) orelse return false;
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                const actual_known_value = switch (value) {
                    .expr_with_known_value => |known_value_expr| known_value_expr.known_value,
                    else => null,
                };
                for (record.fields) |field| {
                    const actual_field = if (actual_known_value) |actual|
                        fieldKnownValueFromKnownValue(actual, field.name)
                    else
                        null;
                    const field_expr = try self.addExpr(.{ .ty = known_valueType(field.known_value), .data = .{ .field_access = .{
                        .receiver = receiver,
                        .field = field.name,
                    } } });
                    const field_value = if (actual_field) |actual|
                        valueFromProjectedExpr(field_expr, actual)
                    else
                        Value{ .expr = field_expr };
                    if (!try self.appendFieldReadExprsFromValue(field.known_value, field_value, out)) return false;
                }
                return true;
            },
            .tuple => |tuple| {
                if (tupleFromValue(value)) |tuple_value| {
                    if (tuple.items.len != tuple_value.items.len) return false;
                    for (tuple.items, tuple_value.items) |item_known_value, item_value| {
                        if (!try self.appendFieldReadExprsFromValue(item_known_value, item_value, out)) return false;
                    }
                    return true;
                }

                const receiver = projectableExprFromValue(value) orelse return false;
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                const actual_known_value = switch (value) {
                    .expr_with_known_value => |known_value_expr| known_value_expr.known_value,
                    else => null,
                };
                for (tuple.items, 0..) |item, index| {
                    const actual_item = if (actual_known_value) |actual|
                        itemKnownValueFromKnownValue(actual, @as(u32, @intCast(index)))
                    else
                        null;
                    const item_expr = try self.addExpr(.{ .ty = known_valueType(item), .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    const item_value = if (actual_item) |actual|
                        valueFromProjectedExpr(item_expr, actual)
                    else
                        Value{ .expr = item_expr };
                    if (!try self.appendFieldReadExprsFromValue(item, item_value, out)) return false;
                }
                return true;
            },
            .nominal => |nominal| {
                const backing_value = switch (value) {
                    .nominal => |nominal_value| nominal_value.backing.*,
                    else => value,
                };
                return try self.appendFieldReadExprsFromValue(nominal.backing.*, backing_value, out);
            },
            .callable => |callable| {
                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => return false,
                };
                if (!callableTargetMatches(self.pass.program, callable.fn_id, callable_value.fn_id) or
                    callable.captures.len != callable_value.captures.len)
                {
                    return false;
                }
                for (callable.captures, callable_value.captures) |capture_known_value, capture_value| {
                    if (!try self.appendFieldReadExprsFromValue(capture_known_value, capture_value, out)) return false;
                }
                return true;
            },
            .finite_callables => |finite_callables| {
                if (value == .finite_callables) {
                    const finite_value = value.finite_callables;
                    if (!knownCallablesMatchesValue(self.pass.program, finite_callables, finite_value)) return false;
                    try out.append(self.pass.allocator, finite_value.selector);
                    for (finite_callables.alternatives, finite_value.alternatives) |alternative_known_value, alternative_value| {
                        for (alternative_known_value.captures, alternative_value.captures) |capture_known_value, capture_value| {
                            if (!try self.appendFieldReadExprsFromValue(capture_known_value, capture_value, out)) return false;
                        }
                    }
                    return true;
                }
                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => return false,
                };
                const active_index = finiteCallableAlternativeIndex(self.pass.program, finite_callables.alternatives, callable_value) orelse return false;
                try out.append(self.pass.allocator, try self.selectorLiteral(@intCast(active_index)));
                for (finite_callables.alternatives, 0..) |alternative_known_value, alternative_index| {
                    if (alternative_index == active_index) {
                        for (alternative_known_value.captures, callable_value.captures) |capture_known_value, capture_value| {
                            if (!try self.appendFieldReadExprsFromValue(capture_known_value, capture_value, out)) return false;
                        }
                    } else {
                        for (alternative_known_value.captures) |capture_known_value| {
                            try self.appendUninitializedExprsForKnownValue(capture_known_value, out);
                        }
                    }
                }
                return true;
            },
            .finite_tags => |finite_tags| {
                if (value == .finite_tags) {
                    const finite_value = value.finite_tags;
                    if (!knownTagsMatchesValue(self.pass.program, finite_tags, finite_value)) return false;
                    try out.append(self.pass.allocator, finite_value.selector);
                    for (finite_tags.alternatives, finite_value.alternatives) |alternative_known_value, alternative_value| {
                        for (alternative_known_value.payloads, alternative_value.payloads) |payload_known_value, payload_value| {
                            if (!try self.appendFieldReadExprsFromValue(payload_known_value, payload_value, out)) return false;
                        }
                    }
                    return true;
                }
                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => return false,
                };
                const active_index = finiteTagAlternativeIndex(self.pass.program, finite_tags.alternatives, tag_value) orelse return false;
                try out.append(self.pass.allocator, try self.selectorLiteral(@intCast(active_index)));
                for (finite_tags.alternatives, 0..) |alternative_known_value, alternative_index| {
                    if (alternative_index == active_index) {
                        for (alternative_known_value.payloads, tag_value.payloads) |payload_known_value, payload_value| {
                            if (!try self.appendFieldReadExprsFromValue(payload_known_value, payload_value, out)) return false;
                        }
                    } else {
                        for (alternative_known_value.payloads) |payload_known_value| {
                            try self.appendUninitializedExprsForKnownValue(payload_known_value, out);
                        }
                    }
                }
                return true;
            },
            .tag,
            => return false,
        }
    }

    fn selectorLiteral(self: *Cloner, value: u64) Common.LowerError!Ast.ExprId {
        const selector_ty = try self.pass.primitiveType(.u64);
        return try self.addExpr(.{
            .ty = selector_ty,
            .data = .{ .int_lit = unsignedIntLiteral(value) },
        });
    }

    fn selectorEquals(self: *Cloner, selector: Ast.ExprId, value: u64) Common.LowerError!Ast.ExprId {
        const bool_ty = try self.pass.primitiveType(.bool);
        const literal = try self.selectorLiteral(value);
        const args = try self.pass.program.addExprSpan(&.{ selector, literal });
        return try self.addExpr(.{
            .ty = bool_ty,
            .data = .{ .low_level = .{
                .op = .num_is_eq,
                .args = args,
            } },
        });
    }

    fn cloneFieldAccess(self: *Cloner, ty: Type.TypeId, field: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingKnownValue(field.receiver);
        if (try self.fieldFromKnownValue(receiver, field.field)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = try self.materialize(receiver),
            .field = field.field,
        } } });
    }

    fn cloneTupleAccess(self: *Cloner, ty: Type.TypeId, access: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingKnownValue(access.tuple);
        if (try self.itemFromKnownValue(receiver, access.elem_index)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
            .tuple = try self.materialize(receiver),
            .elem_index = access.elem_index,
        } } });
    }

    fn fieldFromKnownValue(self: *Cloner, receiver: Value, field: names.RecordFieldNameId) Common.LowerError!?Value {
        if (fieldFromValue(receiver, field)) |value| return value;

        const known_value_expr = switch (receiver) {
            .expr_with_known_value => |known_value_expr| known_value_expr,
            else => return null,
        };
        if (!canReadFieldsFromExpr(self.pass.program, known_value_expr.expr)) return null;

        const field_known_value = fieldKnownValueFromKnownValue(known_value_expr.known_value, field) orelse return null;
        const field_expr = try self.addExpr(.{ .ty = known_valueType(field_known_value), .data = .{ .field_access = .{
            .receiver = known_value_expr.expr,
            .field = field,
        } } });
        return valueFromProjectedExpr(field_expr, field_known_value);
    }

    fn itemFromKnownValue(self: *Cloner, receiver: Value, index: u32) Common.LowerError!?Value {
        if (itemFromValue(receiver, index)) |value| return value;

        const known_value_expr = switch (receiver) {
            .expr_with_known_value => |known_value_expr| known_value_expr,
            else => return null,
        };
        if (!canReadFieldsFromExpr(self.pass.program, known_value_expr.expr)) return null;

        const item_known_value = itemKnownValueFromKnownValue(known_value_expr.known_value, index) orelse return null;
        const item_expr = try self.addExpr(.{ .ty = known_valueType(item_known_value), .data = .{ .tuple_access = .{
            .tuple = known_value_expr.expr,
            .elem_index = index,
        } } });
        return valueFromProjectedExpr(item_expr, item_known_value);
    }

    fn cloneIfValue(self: *Cloner, ty: Type.TypeId, if_: anytype) Common.LowerError!Value {
        const source_branches = try self.pass.allocator.dupe(Ast.IfBranch, self.pass.program.ifBranchSpan(if_.branches));
        defer self.pass.allocator.free(source_branches);

        return try self.cloneIfValueFromBranches(ty, source_branches, 0, if_.final_else);
    }

    fn cloneIfValueFromBranches(
        self: *Cloner,
        ty: Type.TypeId,
        source_branches: []const Ast.IfBranch,
        index: usize,
        final_else: Ast.ExprId,
    ) Common.LowerError!Value {
        if (index == source_branches.len) {
            return try self.cloneExprValueDemandingKnownValue(final_else);
        }

        const branch = source_branches[index];
        const cond_value = try self.cloneExprValueDemandingKnownValue(branch.cond);
        if (knownIfConditionBoolTag(self.pass.program, cond_value)) |cond| {
            if (cond) return try self.cloneScopedExprValueDemandingKnownValue(branch.body);
            return try self.cloneIfValueFromBranches(ty, source_branches, index + 1, final_else);
        }
        if (finiteBoolTagsValue(self.pass.program, cond_value)) |finite_bool| {
            const true_value = try self.cloneScopedExprValueDemandingKnownValue(branch.body);
            const false_value = try self.cloneIfValueFromBranches(ty, source_branches, index + 1, final_else);
            return try self.selectFiniteBoolValue(ty, finite_bool, true_value, false_value);
        }

        const if_branches = try self.pass.arena.allocator().alloc(IfValueBranch, 1);
        if_branches[0] = .{
            .cond = try self.materialize(cond_value),
            .body = try self.cloneScopedExprValueDemandingKnownValue(branch.body),
        };
        const else_value = try self.pass.arena.allocator().create(Value);
        else_value.* = try self.cloneIfValueFromBranches(ty, source_branches, index + 1, final_else);
        return .{ .if_ = .{
            .ty = ty,
            .branches = if_branches,
            .final_else = else_value,
        } };
    }

    fn cloneScopedExprValueDemandingKnownValue(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        const change_start = self.changes.items.len;
        const value = try self.cloneExprValueDemandingKnownValue(expr_id);
        self.restore(change_start);
        return value;
    }

    fn selectFiniteBoolValue(
        self: *Cloner,
        ty: Type.TypeId,
        finite_bool: FiniteTagsValue,
        true_value: Value,
        false_value: Value,
    ) Common.LowerError!Value {
        if (finite_bool.alternatives.len == 0) {
            Common.invariant("finite Bool value had no alternatives");
        }
        if (finite_bool.alternatives.len == 1) {
            const cond = boolTagValue(self.pass.program, finite_bool.alternatives[0]) orelse
                Common.invariant("finite Bool alternative was not Bool");
            return if (cond) true_value else false_value;
        }

        const branch_count = finite_bool.alternatives.len - 1;
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, branch_count);
        for (finite_bool.alternatives[0..branch_count], branches, 0..) |alternative, *out, alternative_index| {
            const cond = boolTagValue(self.pass.program, alternative) orelse
                Common.invariant("finite Bool alternative was not Bool");
            out.* = .{
                .cond = try self.selectorEquals(finite_bool.selector, @intCast(alternative_index)),
                .body = if (cond) true_value else false_value,
            };
        }

        const final_cond = boolTagValue(self.pass.program, finite_bool.alternatives[branch_count]) orelse
            Common.invariant("finite Bool final alternative was not Bool");
        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = if (final_cond) true_value else false_value;
        return .{ .if_ = .{
            .ty = ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn cloneMatchJoinedValue(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee_expr: Ast.ExprId,
        match: @import("../monotype/ast.zig").MatchExpr,
        scrutinee_known_value: ?KnownValue,
    ) Common.LowerError!Value {
        const source_branches = try self.pass.allocator.dupe(Ast.Branch, self.pass.program.branchSpan(match.branches));
        defer self.pass.allocator.free(source_branches);

        var branches = std.ArrayList(Ast.Branch).empty;
        defer branches.deinit(self.pass.allocator);
        var body_values = std.ArrayList(Value).empty;
        defer body_values.deinit(self.pass.allocator);

        for (source_branches) |branch| {
            if (scrutinee_known_value) |known_value| {
                if (patternDefinitelyExcludedByKnownValue(self.pass.program, branch.pat, known_value)) continue;
            }
            const change_start = self.changes.items.len;
            if (scrutinee_known_value) |known_value| {
                _ = try self.bindPatToExprWithKnownValue(branch.pat, known_value);
            }
            const cloned_branch = Ast.Branch{
                .pat = try self.clonePat(branch.pat),
                .guard = if (branch.guard) |guard| try self.cloneExpr(guard) else null,
                .body = undefined,
            };
            const body_value = try self.cloneExprValueDemandingKnownValue(branch.body);
            try branches.append(self.pass.allocator, .{
                .pat = cloned_branch.pat,
                .guard = cloned_branch.guard,
                .body = try self.materialize(body_value),
            });
            try body_values.append(self.pass.allocator, body_value);
            self.restore(change_start);
        }

        const known_value = try self.joinKnownValuesFromValues(body_values.items);
        const match_expr = try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = scrutinee_expr,
            .branches = try self.pass.program.addBranchSpan(branches.items),
            .comptime_site = match.comptime_site,
        } } });

        if (known_value == null) return .{ .expr = match_expr };

        return .{ .expr_with_known_value = .{
            .expr = match_expr,
            .known_value = known_value.?,
        } };
    }

    fn joinKnownValuesFromValues(self: *Cloner, values: []const Value) Allocator.Error!?KnownValue {
        if (values.len == 0) return null;
        var joined = (try self.pass.knownValueFromValue(values[0])) orelse return null;
        for (values[1..]) |value| {
            const next = (try self.pass.knownValueFromValue(value)) orelse return null;
            joined = (try self.joinKnownValuePair(joined, next)) orelse return null;
        }
        return joined;
    }

    fn joinKnownValuePair(self: *Cloner, lhs: KnownValue, rhs: KnownValue) Allocator.Error!?KnownValue {
        return try joinKnownValuesInArena(self.pass.program, self.pass.arena.allocator(), lhs, rhs);
    }

    fn cloneMatch(self: *Cloner, ty: Type.TypeId, match: @import("../monotype/ast.zig").MatchExpr) Common.LowerError!Ast.ExprId {
        const scrutinee = try self.cloneExprValueDemandingKnownValue(match.scrutinee);
        if (try self.simplifyKnownMatch(ty, scrutinee, match.branches)) |body| return body;

        const scrutinee_expr = try self.materialize(scrutinee);
        const scrutinee_known_value = try self.pass.knownValueFromValue(scrutinee);
        return try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = scrutinee_expr,
            .branches = try self.cloneBranchSpanWithScrutineeKnownValue(match.branches, scrutinee_known_value),
            .comptime_site = match.comptime_site,
        } } });
    }

    fn simplifyKnownMatch(self: *Cloner, ty: Type.TypeId, scrutinee: Value, branches_span: Ast.Span(Ast.Branch)) Common.LowerError!?Ast.ExprId {
        if (try self.simplifyKnownMatchValueWithKnownValuePreservation(ty, scrutinee, branches_span, false)) |value| {
            return try self.materialize(value);
        }
        return null;
    }

    fn simplifyKnownMatchValue(self: *Cloner, ty: Type.TypeId, scrutinee: Value, branches_span: Ast.Span(Ast.Branch)) Common.LowerError!?Value {
        return try self.simplifyKnownMatchValueWithKnownValuePreservation(ty, scrutinee, branches_span, true);
    }

    fn simplifyKnownMatchValueWithKnownValuePreservation(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee: Value,
        branches_span: Ast.Span(Ast.Branch),
        preserve_branch_known_value: bool,
    ) Common.LowerError!?Value {
        return try self.simplifyKnownMatchValueMode(ty, scrutinee, branches_span, .strict, preserve_branch_known_value);
    }

    fn simplifyKnownMatchValueMode(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee: Value,
        branches_span: Ast.Span(Ast.Branch),
        mode: KnownMatchMode,
        preserve_branch_known_value: bool,
    ) Common.LowerError!?Value {
        switch (scrutinee) {
            .expr,
            .expr_with_known_value,
            => return null,
            .let_ => |let_value| {
                const change_start = self.changes.items.len;
                defer self.restore(change_start);
                try self.bindPendingLetKnownValues(let_value.lets);
                const body = (try self.simplifyKnownMatchValueMode(ty, let_value.body.*, branches_span, mode, preserve_branch_known_value)) orelse return null;
                return try self.wrapPendingLets(body, let_value.lets, true);
            },
            .if_ => |if_value| return try self.simplifyKnownMatchIfValue(ty, if_value, branches_span, preserve_branch_known_value),
            .finite_tags => |finite_tags| return try self.simplifyKnownMatchFiniteTagsValue(ty, finite_tags, branches_span, preserve_branch_known_value),
            else => {},
        }
        for (self.pass.program.branchSpan(branches_span)) |branch| {
            const match_change_start = self.changes.items.len;
            const matches = try self.bindPatToValue(branch.pat, scrutinee);
            self.restore(match_change_start);
            if (!matches) continue;
            if (branch.guard != null) return null;

            var pending_lets = std.ArrayList(PendingLet).empty;
            defer pending_lets.deinit(self.pass.allocator);

            const change_start = self.changes.items.len;
            const unsafe_count = self.unsafeLeafCount(scrutinee);
            if (try self.bindPatToMatchValue(branch.pat, scrutinee, branch.body, unsafe_count, &pending_lets) == null) {
                Common.invariant("known constructor match changed after reusable payload binding");
            }
            const body = try self.cloneExprValue(branch.body);
            self.restore(change_start);
            return try self.wrapPendingLets(body, pending_lets.items, preserve_branch_known_value);
        }
        switch (mode) {
            .strict => Common.invariant("known constructor match had no matching branch"),
            .speculative => return null,
        }
    }

    fn simplifyKnownMatchIfValue(
        self: *Cloner,
        ty: Type.TypeId,
        if_value: IfValue,
        branches_span: Ast.Span(Ast.Branch),
        preserve_branch_known_value: bool,
    ) Common.LowerError!?Value {
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, if_value.branches.len);
        for (if_value.branches, 0..) |branch, index| {
            const simplified = try self.simplifyKnownMatchValueMode(ty, branch.body, branches_span, .speculative, preserve_branch_known_value);
            branches[index] = .{
                .cond = branch.cond,
                .body = simplified orelse return null,
            };
        }

        const final_else = try self.pass.arena.allocator().create(Value);
        const simplified_final_else = try self.simplifyKnownMatchValueMode(ty, if_value.final_else.*, branches_span, .speculative, preserve_branch_known_value);
        final_else.* = simplified_final_else orelse return null;

        return .{ .if_ = .{
            .ty = ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn simplifyKnownMatchFiniteTagsValue(
        self: *Cloner,
        ty: Type.TypeId,
        finite_tags: FiniteTagsValue,
        branches_span: Ast.Span(Ast.Branch),
        preserve_branch_known_value: bool,
    ) Common.LowerError!?Value {
        if (finite_tags.alternatives.len == 0) {
            Common.invariant("finite tag match had no alternatives");
        }
        if (finite_tags.alternatives.len == 1) {
            return try self.simplifyKnownMatchValueMode(ty, .{ .tag = finite_tags.alternatives[0] }, branches_span, .speculative, preserve_branch_known_value);
        }

        const branch_count = finite_tags.alternatives.len - 1;
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, branch_count);
        for (finite_tags.alternatives[0..branch_count], branches, 0..) |alternative, *branch, index| {
            branch.* = .{
                .cond = try self.selectorEquals(finite_tags.selector, @intCast(index)),
                .body = (try self.simplifyKnownMatchValueMode(ty, .{ .tag = alternative }, branches_span, .speculative, preserve_branch_known_value)) orelse
                    return null,
            };
        }

        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = (try self.simplifyKnownMatchValueMode(ty, .{ .tag = finite_tags.alternatives[branch_count] }, branches_span, .speculative, preserve_branch_known_value)) orelse
            return null;

        return .{ .if_ = .{
            .ty = ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn bindPatToMatchValue(
        self: *Cloner,
        pat_id: Ast.PatId,
        value: Value,
        body: Ast.ExprId,
        unsafe_count: usize,
        pending_lets: *std.ArrayList(PendingLet),
    ) Common.LowerError!?Value {
        const pat = self.pass.program.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bind => |local| {
                const prepared = try self.valueForMatchLocal(local, value, body, unsafe_count, pending_lets);
                try self.putSubst(local, prepared);
                return prepared;
            },
            .wildcard => return try self.makeReusableForMatch(value, pending_lets),
            .as => |as| {
                const as_uses = localUseCountInExpr(self.pass.program, as.local, body);
                const base = if (self.valueCanSubstitute(value) or
                    (unsafe_count == 1 and as_uses == 1 and localUseBeforeEffect(self.pass.program, as.local, body)))
                    value
                else
                    try self.makeReusableForMatch(value, pending_lets);
                const prepared = (try self.bindPatToMatchValue(as.pattern, base, body, unsafe_count, pending_lets)) orelse return null;
                try self.putSubst(as.local, prepared);
                return prepared;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                if (recordFromValue(value)) |record| {
                    const prepared_fields = try self.pass.arena.allocator().alloc(FieldValue, record.fields.len);
                    for (record.fields, 0..) |field, index| {
                        if (recordPatField(fields, field.name)) |field_pat| {
                            const prepared = (try self.bindPatToMatchValue(field_pat, field.value, body, unsafe_count, pending_lets)) orelse return null;
                            prepared_fields[index] = .{
                                .name = field.name,
                                .value = prepared,
                            };
                        } else {
                            prepared_fields[index] = .{
                                .name = field.name,
                                .value = try self.makeReusableForMatch(field.value, pending_lets),
                            };
                        }
                    }
                    return Value{ .record = .{
                        .ty = record.ty,
                        .fields = prepared_fields,
                    } };
                }
                for (fields) |field| {
                    const field_ty = self.pass.program.pats.items[@intFromEnum(field.pattern)].ty;
                    const field_value = (try self.fieldFromPatternValue(value, field.name, field_ty)) orelse return null;
                    _ = (try self.bindPatToMatchValue(field.pattern, field_value, body, unsafe_count, pending_lets)) orelse return null;
                }
                return try self.makeReusableForMatch(value, pending_lets);
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                if (tupleFromValue(value)) |tuple| {
                    if (pats.len != tuple.items.len) return null;
                    const items = try self.pass.arena.allocator().alloc(Value, tuple.items.len);
                    for (pats, tuple.items, 0..) |child_pat, child_value, index| {
                        items[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, unsafe_count, pending_lets)) orelse return null;
                    }
                    return Value{ .tuple = .{
                        .ty = tuple.ty,
                        .items = items,
                    } };
                }
                for (pats, 0..) |child_pat, index| {
                    const item_ty = self.pass.program.pats.items[@intFromEnum(child_pat)].ty;
                    const item_value = (try self.itemFromPatternValue(value, @intCast(index), item_ty)) orelse return null;
                    _ = (try self.bindPatToMatchValue(child_pat, item_value, body, unsafe_count, pending_lets)) orelse return null;
                }
                return try self.makeReusableForMatch(value, pending_lets);
            },
            .tag => |tag_pat| {
                const tag = tagFromValue(value) orelse return null;
                if (tag.name != tag_pat.name) return null;
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) return null;
                const payloads = try self.pass.arena.allocator().alloc(Value, tag.payloads.len);
                for (pats, tag.payloads, 0..) |child_pat, child_value, index| {
                    payloads[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, unsafe_count, pending_lets)) orelse return null;
                }
                return Value{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .nominal => |backing_pat| {
                const nominal = switch (value) {
                    .nominal => |nominal| nominal,
                    else => return null,
                };
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = (try self.bindPatToMatchValue(backing_pat, nominal.backing.*, body, unsafe_count, pending_lets)) orelse return null;
                return Value{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            // List patterns are not statically destructured during
            // specialization; fall back to the runtime match.
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

    fn valueForMatchLocal(
        self: *Cloner,
        local: Ast.LocalId,
        value: Value,
        body: Ast.ExprId,
        unsafe_count: usize,
        pending_lets: *std.ArrayList(PendingLet),
    ) Common.LowerError!Value {
        const uses = localUseCountInExpr(self.pass.program, local, body);
        if (self.valueCanSubstitute(value) or
            (unsafe_count == 1 and uses == 1 and localUseBeforeEffect(self.pass.program, local, body)))
        {
            return value;
        }
        return try self.makeReusableForMatch(value, pending_lets);
    }

    fn valueForInlineLocal(
        self: *Cloner,
        local: Ast.LocalId,
        value: Value,
        body: Ast.ExprId,
        pending_lets: *std.ArrayList(PendingLet),
    ) Common.LowerError!Value {
        const uses = localMaxUseCountPerPathInExpr(self.pass.program, local, body);
        if (self.valueCanSubstitute(value) or
            (uses == 1 and localUseBeforeEffect(self.pass.program, local, body)))
        {
            return value;
        }
        return try self.makeReusableForMatch(value, pending_lets);
    }

    fn unsafeLeafCount(self: *Cloner, value: Value) usize {
        return switch (value) {
            .expr => |expr| if (self.exprCanSubstitute(expr)) 0 else 1,
            .expr_with_known_value => |known_value_expr| if (self.exprCanSubstitute(known_value_expr.expr)) 0 else 1,
            .let_ => |let_value| blk: {
                var count: usize = let_value.lets.len;
                count += self.unsafeLeafCount(let_value.body.*);
                break :blk count;
            },
            .if_ => |if_value| blk: {
                var count: usize = 0;
                for (if_value.branches) |branch| {
                    if (!self.exprCanSubstitute(branch.cond)) count += 1;
                    count += self.unsafeLeafCount(branch.body);
                }
                count += self.unsafeLeafCount(if_value.final_else.*);
                break :blk count;
            },
            .tag => |tag| blk: {
                var count: usize = 0;
                for (tag.payloads) |payload| count += self.unsafeLeafCount(payload);
                break :blk count;
            },
            .record => |record| blk: {
                var count: usize = 0;
                for (record.fields) |field| count += self.unsafeLeafCount(field.value);
                break :blk count;
            },
            .tuple => |tuple| blk: {
                var count: usize = 0;
                for (tuple.items) |item| count += self.unsafeLeafCount(item);
                break :blk count;
            },
            .nominal => |nominal| self.unsafeLeafCount(nominal.backing.*),
            .callable => |callable| blk: {
                var count: usize = 0;
                for (callable.captures) |capture| count += self.unsafeLeafCount(capture);
                break :blk count;
            },
            .finite_tags => |finite_tags| blk: {
                var count: usize = if (self.exprCanSubstitute(finite_tags.selector)) 0 else 1;
                for (finite_tags.alternatives) |alternative| {
                    for (alternative.payloads) |payload| count += self.unsafeLeafCount(payload);
                }
                break :blk count;
            },
            .finite_callables => |finite_callables| blk: {
                var count: usize = if (self.exprCanSubstitute(finite_callables.selector)) 0 else 1;
                for (finite_callables.alternatives) |alternative| {
                    for (alternative.captures) |capture| count += self.unsafeLeafCount(capture);
                }
                break :blk count;
            },
        };
    }

    fn makeReusableForMatch(self: *Cloner, value: Value, pending_lets: *std.ArrayList(PendingLet)) Common.LowerError!Value {
        if (self.valueCanSubstitute(value)) return value;
        return switch (value) {
            .expr => |expr| blk: {
                const ty = self.pass.program.exprs.items[@intFromEnum(expr)].ty;
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try pending_lets.append(self.pass.allocator, .{
                    .local = local,
                    .ty = ty,
                    .value = expr,
                });
                break :blk Value{ .expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                }) };
            },
            .expr_with_known_value => |known_value_expr| blk: {
                const ty = self.pass.program.exprs.items[@intFromEnum(known_value_expr.expr)].ty;
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try pending_lets.append(self.pass.allocator, .{
                    .local = local,
                    .ty = ty,
                    .value = known_value_expr.expr,
                    .known_value = known_value_expr.known_value,
                });
                const local_expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                });
                break :blk Value{ .expr_with_known_value = .{
                    .expr = local_expr,
                    .known_value = known_value_expr.known_value,
                } };
            },
            .let_ => |let_value| blk: {
                const body = try self.makeReusableForMatch(let_value.body.*, pending_lets);
                const lets = try self.pass.arena.allocator().dupe(PendingLet, let_value.lets);
                break :blk Value{ .let_ = .{
                    .lets = lets,
                    .body = try self.copyValue(body),
                } };
            },
            .if_ => |if_value| blk: {
                const branches = try self.pass.arena.allocator().alloc(IfValueBranch, if_value.branches.len);
                for (if_value.branches, 0..) |branch, index| {
                    var branch_pending_lets = std.ArrayList(PendingLet).empty;
                    defer branch_pending_lets.deinit(self.pass.allocator);
                    const branch_body = try self.makeReusableForMatch(branch.body, &branch_pending_lets);
                    branches[index] = .{
                        .cond = try self.makeExprReusableForMatch(branch.cond, pending_lets),
                        .body = try self.wrapPendingLets(branch_body, branch_pending_lets.items, true),
                    };
                }
                const final_else = try self.pass.arena.allocator().create(Value);
                var else_pending_lets = std.ArrayList(PendingLet).empty;
                defer else_pending_lets.deinit(self.pass.allocator);
                const else_body = try self.makeReusableForMatch(if_value.final_else.*, &else_pending_lets);
                final_else.* = try self.wrapPendingLets(else_body, else_pending_lets.items, true);
                break :blk Value{ .if_ = .{
                    .ty = if_value.ty,
                    .branches = branches,
                    .final_else = final_else,
                } };
            },
            .tag => |tag| blk: {
                const payloads = try self.pass.arena.allocator().alloc(Value, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = try self.makeReusableForMatch(payload, pending_lets);
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
                        .value = try self.makeReusableForMatch(field.value, pending_lets),
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
                    items[index] = try self.makeReusableForMatch(item, pending_lets);
                }
                break :blk Value{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = try self.makeReusableForMatch(nominal.backing.*, pending_lets);
                break :blk Value{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.pass.arena.allocator().alloc(Value, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = try self.makeReusableForMatch(capture, pending_lets);
                }
                break :blk Value{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
            .finite_tags => |finite_tags| blk: {
                const alternatives = try self.pass.arena.allocator().alloc(TagValue, finite_tags.alternatives.len);
                for (finite_tags.alternatives, alternatives) |alternative, *out| {
                    const payloads = try self.pass.arena.allocator().alloc(Value, alternative.payloads.len);
                    for (alternative.payloads, payloads) |payload, *payload_out| {
                        payload_out.* = try self.makeReusableForMatch(payload, pending_lets);
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .name = alternative.name,
                        .payloads = payloads,
                    };
                }
                break :blk Value{ .finite_tags = .{
                    .ty = finite_tags.ty,
                    .selector = try self.makeExprReusableForMatch(finite_tags.selector, pending_lets),
                    .alternatives = alternatives,
                } };
            },
            .finite_callables => |finite_callables| blk: {
                const alternatives = try self.pass.arena.allocator().alloc(CallableValue, finite_callables.alternatives.len);
                for (finite_callables.alternatives, alternatives) |alternative, *out| {
                    const captures = try self.pass.arena.allocator().alloc(Value, alternative.captures.len);
                    for (alternative.captures, captures) |capture, *capture_out| {
                        capture_out.* = try self.makeReusableForMatch(capture, pending_lets);
                    }
                    out.* = .{
                        .ty = alternative.ty,
                        .fn_id = alternative.fn_id,
                        .captures = captures,
                    };
                }
                break :blk Value{ .finite_callables = .{
                    .ty = finite_callables.ty,
                    .selector = try self.makeExprReusableForMatch(finite_callables.selector, pending_lets),
                    .alternatives = alternatives,
                } };
            },
        };
    }

    fn makeExprReusableForMatch(
        self: *Cloner,
        expr: Ast.ExprId,
        pending_lets: *std.ArrayList(PendingLet),
    ) Common.LowerError!Ast.ExprId {
        if (self.exprCanSubstitute(expr)) return expr;

        const ty = self.pass.program.exprs.items[@intFromEnum(expr)].ty;
        const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
        try pending_lets.append(self.pass.allocator, .{
            .local = local,
            .ty = ty,
            .value = expr,
            .known_value = try self.pass.constructorKnownValue(expr),
        });
        return try self.addExpr(.{
            .ty = ty,
            .data = .{ .local = local },
        });
    }

    fn wrapPendingLets(self: *Cloner, body: Value, pending_lets: []const PendingLet, preserve_known_value: bool) Common.LowerError!Value {
        if (pending_lets.len == 0) return body;

        const known_value = if (preserve_known_value) try self.pass.knownValueFromValue(body) else null;
        if (known_value != null) {
            const lets = try self.pass.arena.allocator().dupe(PendingLet, pending_lets);
            return .{ .let_ = .{
                .lets = lets,
                .body = try self.copyValue(body),
            } };
        }

        const ty = valueType(self.pass.program, body);
        var result = try self.materialize(body);
        result = try self.wrapPendingLetsAroundExpr(ty, result, pending_lets);
        return .{ .expr = result };
    }

    fn wrapPendingLetsAroundExpr(
        self: *Cloner,
        ty: Type.TypeId,
        body_expr: Ast.ExprId,
        pending_lets: []const PendingLet,
    ) Common.LowerError!Ast.ExprId {
        var result = body_expr;
        var index = pending_lets.len;
        while (index > 0) {
            index -= 1;
            const pending = pending_lets[index];
            const pat = try self.pass.program.addPat(.{
                .ty = pending.ty,
                .data = .{ .bind = pending.local },
            });
            result = try self.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = try self.cloneExpr(pending.value),
                .rest = result,
            } } });
        }
        return result;
    }

    fn cloneCaseOfCaseValue(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee_expr: Ast.ExprId,
        outer_branches_span: Ast.Span(Ast.Branch),
    ) Common.LowerError!?Value {
        const scrutinee_data = self.pass.program.exprs.items[@intFromEnum(scrutinee_expr)].data;
        const inner_match = switch (scrutinee_data) {
            .match_ => |match| match,
            else => return null,
        };

        const outer_branches = self.pass.program.branchSpan(outer_branches_span);
        for (outer_branches) |branch| {
            if (branch.guard != null) return null;
        }

        const inner_branches = try self.pass.allocator.dupe(Ast.Branch, self.pass.program.branchSpan(inner_match.branches));
        defer self.pass.allocator.free(inner_branches);

        var rewritten = try self.pass.allocator.alloc(Ast.Branch, inner_branches.len);
        defer self.pass.allocator.free(rewritten);

        for (inner_branches, 0..) |inner_branch, index| {
            const inner_value = try self.cloneExprValue(inner_branch.body);
            const outer_value = (try self.simplifyKnownMatchValueMode(ty, inner_value, outer_branches_span, .speculative, true)) orelse return null;
            rewritten[index] = .{
                .pat = inner_branch.pat,
                .guard = inner_branch.guard,
                .body = try self.materialize(outer_value),
            };
        }

        return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = inner_match.scrutinee,
            .branches = try self.pass.program.addBranchSpan(rewritten),
            .comptime_site = inner_match.comptime_site,
        } } }) };
    }

    fn inlineCallableCallValue(
        self: *Cloner,
        ty: Type.TypeId,
        callable: CallableValue,
        args_span: Ast.Span(Ast.ExprId),
        demand_result_known_value: bool,
    ) Common.LowerError!Value {
        for (self.inline_stack.items) |active| {
            if (active.fn_id == callable.fn_id) {
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(.{ .callable = callable }),
                    .args = try self.cloneExprSpan(args_span),
                } } }) };
            }
        }

        const source_fn = self.pass.program.fns.items[@intFromEnum(callable.fn_id)];
        const body = self.pass.originalBody(callable.fn_id) orelse switch (source_fn.body) {
            .roc => |body| body,
            .hosted => {
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(.{ .callable = callable }),
                    .args = try self.cloneExprSpan(args_span),
                } } }) };
            },
        };
        if (exprContainsReturn(self.pass.program, body)) {
            return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(.{ .callable = callable }),
                .args = try self.cloneExprSpan(args_span),
            } } }) };
        }

        const source_args = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(args_span));
        defer self.pass.allocator.free(args);
        if (source_args.len != args.len) Common.invariant("callable call arity differed from lifted function arity");

        const source_captures = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(source_captures);
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        var pending_lets = std.ArrayList(PendingLet).empty;
        defer pending_lets.deinit(self.pass.allocator);

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const prepared_captures = try self.pass.allocator.alloc(Value, callable.captures.len);
        defer self.pass.allocator.free(prepared_captures);
        for (source_captures, callable.captures, 0..) |source_capture, capture_value, index| {
            prepared_captures[index] = try self.valueForInlineLocal(source_capture.local, capture_value, body, &pending_lets);
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
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, &pending_lets);
        }

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callable.fn_id });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callable.fn_id) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        const body_value = if (demand_result_known_value)
            try self.cloneExprValueDemandingKnownValue(body)
        else
            try self.cloneExprValue(body);
        return try self.wrapPendingLets(body_value, pending_lets.items, demand_result_known_value);
    }

    fn callKnownValue(
        self: *Cloner,
        ty: Type.TypeId,
        callee: Value,
        args_span: Ast.Span(Ast.ExprId),
        demand_result_known_value: bool,
    ) Common.LowerError!Value {
        return switch (callee) {
            .callable => |callable| try self.inlineCallableCallValue(ty, callable, args_span, demand_result_known_value),
            .finite_callables => |finite_callables| try self.callFiniteCallablesValue(ty, finite_callables, args_span, demand_result_known_value),
            .if_ => |if_value| try self.callIfValue(ty, if_value, args_span, demand_result_known_value),
            else => .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                .callee = try self.materialize(callee),
                .args = try self.cloneExprSpan(args_span),
            } } }) },
        };
    }

    fn callFiniteCallablesValue(
        self: *Cloner,
        ty: Type.TypeId,
        finite_callables: FiniteCallablesValue,
        args_span: Ast.Span(Ast.ExprId),
        demand_result_known_value: bool,
    ) Common.LowerError!Value {
        if (finite_callables.alternatives.len == 0) {
            Common.invariant("finite callable value had no alternatives");
        }
        if (finite_callables.alternatives.len == 1) {
            return try self.inlineCallableCallValue(ty, finite_callables.alternatives[0], args_span, demand_result_known_value);
        }

        const branch_count = finite_callables.alternatives.len - 1;
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, branch_count);
        for (finite_callables.alternatives[0..branch_count], branches, 0..) |alternative, *branch, index| {
            branch.* = .{
                .cond = try self.selectorEquals(finite_callables.selector, @intCast(index)),
                .body = try self.inlineCallableCallValue(ty, alternative, args_span, demand_result_known_value),
            };
        }

        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = try self.inlineCallableCallValue(ty, finite_callables.alternatives[branch_count], args_span, demand_result_known_value);
        return .{ .if_ = .{
            .ty = ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn callIfValue(
        self: *Cloner,
        ty: Type.TypeId,
        if_value: IfValue,
        args_span: Ast.Span(Ast.ExprId),
        demand_result_known_value: bool,
    ) Common.LowerError!Value {
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, if_value.branches.len);
        for (if_value.branches, 0..) |branch, index| {
            branches[index] = .{
                .cond = branch.cond,
                .body = try self.callKnownValue(ty, branch.body, args_span, demand_result_known_value),
            };
        }
        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = try self.callKnownValue(ty, if_value.final_else.*, args_span, demand_result_known_value);
        return .{ .if_ = .{
            .ty = ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn inlineDirectCallValue(
        self: *Cloner,
        callee: Ast.FnId,
        args_span: Ast.Span(Ast.ExprId),
        original_expr: Ast.ExprId,
        demand_result_known_value: bool,
    ) Common.LowerError!Value {
        const active_arg_known_values = try self.directCallActiveArgKnownValues(args_span);
        for (self.inline_stack.items) |active| {
            if (active.fn_id != callee) continue;
            const active_args = active.args orelse return .{ .expr = try self.cloneExprPlain(original_expr) };
            if (!known_valuesStrictlyDescend(self.pass.program, active_args, active_arg_known_values)) {
                return .{ .expr = try self.cloneExprPlain(original_expr) };
            }
        }

        const source_fn = self.pass.program.fns.items[@intFromEnum(callee)];
        const body = self.pass.originalBody(callee) orelse switch (source_fn.body) {
            .roc => |body| body,
            .hosted => {
                return .{ .expr = try self.cloneExprPlain(original_expr) };
            },
        };
        if (exprContainsReturn(self.pass.program, body)) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }
        if (!self.directInlineCapturesAvailable(source_fn)) {
            return .{ .expr = try self.cloneExprPlain(original_expr) };
        }

        const source_args = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(args_span));
        defer self.pass.allocator.free(args);
        if (source_args.len != args.len) Common.invariant("direct call arity differed from lifted function arity");

        var pending_lets = std.ArrayList(PendingLet).empty;
        defer pending_lets.deinit(self.pass.allocator);

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const captures = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(captures);
        for (captures) |capture| {
            if (self.subst.get(capture.local)) |value| {
                try self.putSubst(capture.local, try self.makeReusableForMatch(value, &pending_lets));
            }
        }

        const arg_values = try self.pass.allocator.alloc(Value, args.len);
        defer self.pass.allocator.free(arg_values);
        const callee_uses = if (@intFromEnum(callee) < self.pass.plans.len)
            self.pass.plans[@intFromEnum(callee)].used_args
        else
            &.{};
        for (args, 0..) |arg_expr, index| {
            arg_values[index] = if (index < callee_uses.len and callee_uses[index])
                try self.cloneExprValueDemandingKnownValue(arg_expr)
            else
                try self.cloneExprValue(arg_expr);
        }

        var unsafe_count: usize = 0;
        for (arg_values) |arg_value| unsafe_count += self.unsafeLeafCount(arg_value);
        for (captures) |capture| {
            if (self.subst.get(capture.local)) |value| unsafe_count += self.unsafeLeafCount(value);
        }

        const prepared_args = try self.pass.allocator.alloc(Value, arg_values.len);
        defer self.pass.allocator.free(prepared_args);
        for (source_args, arg_values, 0..) |source_arg, arg_value, index| {
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, &pending_lets);
        }

        try self.inline_stack.append(self.pass.allocator, .{
            .fn_id = callee,
            .args = active_arg_known_values,
        });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callee) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        const body_value = if (demand_result_known_value)
            try self.cloneExprValueDemandingKnownValue(body)
        else
            try self.cloneExprValue(body);
        return try self.wrapPendingLets(body_value, pending_lets.items, demand_result_known_value);
    }

    fn directInlineCapturesAvailable(self: *Cloner, source_fn: Ast.Fn) bool {
        for (self.pass.program.typedLocalSpan(source_fn.captures)) |capture| {
            if (!self.subst.contains(capture.local)) return false;
        }
        return true;
    }

    fn fieldFromPatternValue(
        self: *Cloner,
        value: Value,
        field: names.RecordFieldNameId,
        ty: Type.TypeId,
    ) Common.LowerError!?Value {
        if (fieldFromValue(value, field)) |field_value| return field_value;

        const projected_from = try self.projectablePatternValue(value);
        const known_value = switch (projected_from) {
            .expr_with_known_value => |known| fieldKnownValueFromKnownValue(known.known_value, field),
            else => null,
        };
        const receiver = projectableExprFromValue(projected_from) orelse return null;
        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return null;
        const field_expr = try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = receiver,
            .field = field,
        } } });
        return if (known_value) |known|
            valueFromProjectedExpr(field_expr, known)
        else
            Value{ .expr = field_expr };
    }

    fn itemFromPatternValue(
        self: *Cloner,
        value: Value,
        index: u32,
        ty: Type.TypeId,
    ) Common.LowerError!?Value {
        if (itemFromValue(value, index)) |item_value| return item_value;

        const projected_from = try self.projectablePatternValue(value);
        const known_value = switch (projected_from) {
            .expr_with_known_value => |known| itemKnownValueFromKnownValue(known.known_value, index),
            else => null,
        };
        const receiver = projectableExprFromValue(projected_from) orelse return null;
        if (!canReadFieldsFromExpr(self.pass.program, receiver)) return null;
        const item_expr = try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
            .tuple = receiver,
            .elem_index = index,
        } } });
        return if (known_value) |known|
            valueFromProjectedExpr(item_expr, known)
        else
            Value{ .expr = item_expr };
    }

    fn projectablePatternValue(
        self: *Cloner,
        value: Value,
    ) Common.LowerError!Value {
        if (projectableExprFromValue(value)) |expr| {
            if (canReadFieldsFromExpr(self.pass.program, expr)) return value;
        }
        return value;
    }

    fn bindPatToValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!bool {
        const pat = self.pass.program.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bind => |local| {
                try self.putSubst(local, value);
                return true;
            },
            .wildcard => return true,
            .as => |as| {
                if (!try self.bindPatToValue(as.pattern, value)) return false;
                try self.putSubst(as.local, value);
                return true;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                if (recordFromValue(value)) |record| {
                    for (fields) |field| {
                        const field_value = fieldFromRecord(record, field.name) orelse return false;
                        if (!try self.bindPatToValue(field.pattern, field_value)) return false;
                    }
                    return true;
                }
                for (fields) |field| {
                    const field_ty = self.pass.program.pats.items[@intFromEnum(field.pattern)].ty;
                    const field_value = (try self.fieldFromPatternValue(value, field.name, field_ty)) orelse return false;
                    if (!try self.bindPatToValue(field.pattern, field_value)) return false;
                }
                return true;
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                if (tupleFromValue(value)) |tuple| {
                    if (pats.len != tuple.items.len) return false;
                    for (pats, tuple.items) |child_pat, child_value| {
                        if (!try self.bindPatToValue(child_pat, child_value)) return false;
                    }
                    return true;
                }
                for (pats, 0..) |child_pat, index| {
                    const item_ty = self.pass.program.pats.items[@intFromEnum(child_pat)].ty;
                    const item_value = (try self.itemFromPatternValue(value, @intCast(index), item_ty)) orelse return false;
                    if (!try self.bindPatToValue(child_pat, item_value)) return false;
                }
                return true;
            },
            .tag => |tag_pat| {
                const tag = tagFromValue(value) orelse return false;
                if (tag.name != tag_pat.name) return false;
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) return false;
                for (pats, tag.payloads) |child_pat, child_value| {
                    if (!try self.bindPatToValue(child_pat, child_value)) return false;
                }
                return true;
            },
            .nominal => |backing_pat| {
                const nominal = switch (value) {
                    .nominal => |nominal| nominal,
                    else => return false,
                };
                return try self.bindPatToValue(backing_pat, nominal.backing.*);
            },
            // List patterns are not statically bound during specialization.
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

    fn bindPatToReusableValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!bool {
        if (!self.valueCanSubstitute(value)) return false;
        return try self.bindPatToValue(pat_id, value);
    }

    fn bindPatToSingleUseTailValue(self: *Cloner, pat_id: Ast.PatId, value: Value, tail: BlockTail) Common.LowerError!bool {
        const pat = self.pass.program.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bind => |local| {
                const uses = localMaxUseCountPerPathInBlockTail(self.pass.program, local, tail);
                const before_effect = localUseBeforeEffectInBlockTail(self.pass.program, local, tail);
                if (uses != 1) return false;
                if (!before_effect) return false;
                try self.putSubst(local, value);
                return true;
            },
            .wildcard,
            .as,
            .record,
            .tuple,
            .list,
            .tag,
            .nominal,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .str_pattern,
            => return false,
        }
    }

    fn bindPatToSingleUseRestValue(self: *Cloner, pat_id: Ast.PatId, value: Value, rest: Ast.ExprId) Common.LowerError!bool {
        return try self.bindPatToSingleUseTailValue(pat_id, value, .{
            .statements = &.{},
            .final_expr = rest,
        });
    }

    fn bindPatToMaterializedKnownValue(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!bool {
        const known_value = (try self.pass.knownValueFromValue(value)) orelse return false;
        return try self.bindPatToExprWithKnownValue(pat_id, known_value);
    }

    fn bindPatToExprWithKnownValue(self: *Cloner, pat_id: Ast.PatId, known_value: KnownValue) Common.LowerError!bool {
        const pat = self.pass.program.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bind => |local| {
                const local_ty = self.pass.program.locals.items[@intFromEnum(local)].ty;
                const local_expr = try self.addExpr(.{
                    .ty = local_ty,
                    .data = .{ .local = local },
                });
                try self.putSubst(local, .{ .expr_with_known_value = .{
                    .expr = local_expr,
                    .known_value = known_value,
                } });
                return true;
            },
            .wildcard => return true,
            .as => |as| {
                if (!try self.bindPatToExprWithKnownValue(as.pattern, known_value)) return false;
                const local_ty = self.pass.program.locals.items[@intFromEnum(as.local)].ty;
                const local_expr = try self.addExpr(.{
                    .ty = local_ty,
                    .data = .{ .local = as.local },
                });
                try self.putSubst(as.local, .{ .expr_with_known_value = .{
                    .expr = local_expr,
                    .known_value = known_value,
                } });
                return true;
            },
            .record => |fields_span| {
                const fields = self.pass.program.recordDestructSpan(fields_span);
                for (fields) |field| {
                    const field_known_value = fieldKnownValueFromKnownValue(known_value, field.name) orelse
                        KnownValue{ .any = self.pass.program.pats.items[@intFromEnum(field.pattern)].ty };
                    if (!try self.bindPatToExprWithKnownValue(field.pattern, field_known_value)) return false;
                }
                return true;
            },
            .tuple => |items_span| {
                const pats = self.pass.program.patSpan(items_span);
                for (pats, 0..) |child_pat, index| {
                    const item_known_value = itemKnownValueFromKnownValue(known_value, @as(u32, @intCast(index))) orelse
                        KnownValue{ .any = self.pass.program.pats.items[@intFromEnum(child_pat)].ty };
                    if (!try self.bindPatToExprWithKnownValue(child_pat, item_known_value)) return false;
                }
                return true;
            },
            .tag => |tag_pat| {
                const pats = self.pass.program.patSpan(tag_pat.payloads);
                if (knownTagForPattern(known_value, tag_pat.name)) |tag_known_value| {
                    if (pats.len != tag_known_value.payloads.len) return false;
                    for (pats, tag_known_value.payloads) |child_pat, payload_known_value| {
                        if (!try self.bindPatToExprWithKnownValue(child_pat, payload_known_value)) return false;
                    }
                } else {
                    for (pats) |child_pat| {
                        const payload_known_value = KnownValue{ .any = self.pass.program.pats.items[@intFromEnum(child_pat)].ty };
                        if (!try self.bindPatToExprWithKnownValue(child_pat, payload_known_value)) return false;
                    }
                }
                return true;
            },
            .nominal => |backing_pat| {
                const backing_known_value = switch (known_value) {
                    .nominal => |nominal| nominal.backing.*,
                    else => KnownValue{ .any = self.pass.program.pats.items[@intFromEnum(backing_pat)].ty },
                };
                return try self.bindPatToExprWithKnownValue(backing_pat, backing_known_value);
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

    fn clonePat(self: *Cloner, pat_id: Ast.PatId) Allocator.Error!Ast.PatId {
        const pat = self.pass.program.pats.items[@intFromEnum(pat_id)];
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| .{ .bind = local },
            .wildcard => .wildcard,
            .as => |as| .{ .as = .{
                .pattern = try self.clonePat(as.pattern),
                .local = as.local,
            } },
            .record => |fields| .{ .record = try self.cloneRecordDestructSpan(fields) },
            .tuple => |items| .{ .tuple = try self.clonePatSpan(items) },
            .list => |list| .{ .list = .{
                .patterns = try self.clonePatSpan(list.patterns),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |rest_pattern| try self.clonePat(rest_pattern) else null,
                } else null,
            } },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.clonePatSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.clonePat(backing) },
            .int_lit => |value| .{ .int_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .str_pattern => |str| .{ .str_pattern = try self.cloneStrPattern(str) },
        };
        return try self.pass.program.addPat(.{ .ty = pat.ty, .data = data });
    }

    fn cloneStrPattern(self: *Cloner, str: Ast.StrPattern) Allocator.Error!Ast.StrPattern {
        const input_steps = self.pass.program.strPatternStepSpan(str.steps);
        const output_steps = try self.pass.allocator.alloc(Ast.StrPatternStep, input_steps.len);
        defer self.pass.allocator.free(output_steps);

        for (input_steps, output_steps) |input_step, *output_step| {
            output_step.* = .{
                .capture = if (input_step.capture) |capture| try self.clonePat(capture) else null,
                .delimiter = input_step.delimiter,
            };
        }

        return .{
            .prefix = str.prefix,
            .steps = try self.pass.program.addStrPatternStepSpan(output_steps),
            .end = str.end,
        };
    }

    fn cloneStmtInto(
        self: *Cloner,
        stmt_id: Ast.StmtId,
        out: *std.ArrayList(Ast.StmtId),
        tail: BlockTail,
    ) Common.LowerError!void {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        self.current_loc = self.pass.program.stmtLoc(stmt_id);
        self.current_region = self.pass.program.stmtRegion(stmt_id);

        const stmt = self.pass.program.stmts.items[@intFromEnum(stmt_id)];
        const cloned: Ast.Stmt = switch (stmt) {
            .uninitialized => |pat| .{ .uninitialized = try self.clonePat(pat) },
            .let_ => |let_| blk: {
                var value = if (let_.recursive)
                    try self.cloneExprValue(let_.value)
                else
                    try self.cloneExprValueDemandingKnownValue(let_.value);
                if (!let_.recursive) {
                    while (value == .let_) {
                        try self.appendPendingLetStmts(value.let_.lets, out);
                        value = value.let_.body.*;
                    }

                    var pending_lets = std.ArrayList(PendingLet).empty;
                    defer pending_lets.deinit(self.pass.allocator);

                    const reusable = try self.makeReusableForMatch(value, &pending_lets);
                    const bind_change_start = self.changes.items.len;
                    if (try self.bindPatToReusableValue(let_.pat, reusable)) {
                        try self.appendPendingLetStmts(pending_lets.items, out);
                        return;
                    }
                    self.restore(bind_change_start);

                    if (try self.bindPatToSingleUseTailValue(let_.pat, value, tail)) {
                        return;
                    }
                    self.restore(bind_change_start);
                }
                const value_expr = try self.materialize(value);
                if (!let_.recursive and try self.bindPatToReusableValue(let_.pat, value)) {
                    return;
                }
                _ = try self.bindPatToMaterializedKnownValue(let_.pat, value);
                break :blk .{ .let_ = .{
                    .pat = try self.clonePat(let_.pat),
                    .value = value_expr,
                    .recursive = let_.recursive,
                    .comptime_site = let_.comptime_site,
                } };
            },
            .expr => |expr| .{ .expr = try self.cloneExpr(expr) },
            .expect => |expr| .{ .expect = try self.cloneExpr(expr) },
            .dbg => |expr| .{ .dbg = try self.cloneExpr(expr) },
            .return_ => |expr| .{ .return_ = try self.cloneExpr(expr) },
            .crash => |msg| .{ .crash = msg },
        };
        try out.append(self.pass.allocator, try self.addStmt(cloned));
    }

    fn appendPendingLetStmts(
        self: *Cloner,
        pending_lets: []const PendingLet,
        out: *std.ArrayList(Ast.StmtId),
    ) Common.LowerError!void {
        for (pending_lets) |pending| {
            const pat = try self.pass.program.addPat(.{
                .ty = pending.ty,
                .data = .{ .bind = pending.local },
            });
            try out.append(self.pass.allocator, try self.addStmt(.{ .let_ = .{
                .pat = pat,
                .value = try self.cloneExpr(pending.value),
                .recursive = false,
                .comptime_site = null,
            } }));
        }
    }

    fn bindPendingLetKnownValues(self: *Cloner, pending_lets: []const PendingLet) Common.LowerError!void {
        for (pending_lets) |pending| {
            const known_value = pending.known_value orelse continue;
            const local_expr = try self.addExpr(.{
                .ty = pending.ty,
                .data = .{ .local = pending.local },
            });
            try self.putSubst(pending.local, .{ .expr_with_known_value = .{
                .expr = local_expr,
                .known_value = known_value,
            } });
        }
    }

    fn appendPendingLetsFromStatements(
        self: *Cloner,
        statements: []const Ast.StmtId,
        out: *std.ArrayList(PendingLet),
    ) Allocator.Error!bool {
        for (statements) |stmt_id| {
            const stmt = self.pass.program.stmts.items[@intFromEnum(stmt_id)];
            const let_ = switch (stmt) {
                .let_ => |let_| let_,
                .expr => |expr| {
                    if (discardedExprIsEffectFree(self.pass.program, expr)) continue;
                    return false;
                },
                else => return false,
            };
            if (let_.recursive) return false;
            const pat = self.pass.program.pats.items[@intFromEnum(let_.pat)];
            const local = switch (pat.data) {
                .bind => |local| local,
                else => return false,
            };
            try out.append(self.pass.allocator, .{
                .local = local,
                .ty = pat.ty,
                .value = let_.value,
                .known_value = try self.pass.constructorKnownValue(let_.value),
            });
        }
        return true;
    }

    fn cloneExprSpan(self: *Cloner, span: Ast.Span(Ast.ExprId)) Common.LowerError!Ast.Span(Ast.ExprId) {
        const source = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.ExprId, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |expr, index| values[index] = try self.cloneExpr(expr);
        return try self.pass.program.addExprSpan(values);
    }

    fn clonePatSpan(self: *Cloner, span: Ast.Span(Ast.PatId)) Allocator.Error!Ast.Span(Ast.PatId) {
        const source = try self.pass.allocator.dupe(Ast.PatId, self.pass.program.patSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.PatId, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |pat, index| values[index] = try self.clonePat(pat);
        return try self.pass.program.addPatSpan(values);
    }

    fn cloneFieldExprSpan(self: *Cloner, span: Ast.Span(Ast.FieldExpr)) Common.LowerError!Ast.Span(Ast.FieldExpr) {
        const source = try self.pass.allocator.dupe(Ast.FieldExpr, self.pass.program.fieldExprSpan(span));
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

    fn cloneRecordDestructSpan(self: *Cloner, span: Ast.Span(Ast.RecordDestruct)) Allocator.Error!Ast.Span(Ast.RecordDestruct) {
        const source = try self.pass.allocator.dupe(Ast.RecordDestruct, self.pass.program.recordDestructSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.RecordDestruct, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |field, index| {
            values[index] = .{
                .name = field.name,
                .pattern = try self.clonePat(field.pattern),
            };
        }
        return try self.pass.program.addRecordDestructSpan(values);
    }

    fn cloneBranchSpanWithScrutineeKnownValue(
        self: *Cloner,
        span: Ast.Span(Ast.Branch),
        scrutinee_known_value: ?KnownValue,
    ) Common.LowerError!Ast.Span(Ast.Branch) {
        const source = try self.pass.allocator.dupe(Ast.Branch, self.pass.program.branchSpan(span));
        defer self.pass.allocator.free(source);

        var values = std.ArrayList(Ast.Branch).empty;
        defer values.deinit(self.pass.allocator);
        for (source) |branch| {
            if (scrutinee_known_value) |known_value| {
                if (patternDefinitelyExcludedByKnownValue(self.pass.program, branch.pat, known_value)) continue;
            }
            const change_start = self.changes.items.len;
            if (scrutinee_known_value) |known_value| {
                _ = try self.bindPatToExprWithKnownValue(branch.pat, known_value);
            }
            try values.append(self.pass.allocator, .{
                .pat = try self.clonePat(branch.pat),
                .guard = if (branch.guard) |guard| try self.cloneExpr(guard) else null,
                .body = try self.cloneExpr(branch.body),
            });
            self.restore(change_start);
        }
        return try self.pass.program.addBranchSpan(values.items);
    }

    fn cloneIfBranchSpan(self: *Cloner, span: Ast.Span(Ast.IfBranch)) Common.LowerError!Ast.Span(Ast.IfBranch) {
        const source = try self.pass.allocator.dupe(Ast.IfBranch, self.pass.program.ifBranchSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.IfBranch, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |branch, index| {
            const change_start = self.changes.items.len;
            values[index] = .{
                .cond = try self.cloneExpr(branch.cond),
                .body = try self.cloneExpr(branch.body),
            };
            self.restore(change_start);
        }
        return try self.pass.program.addIfBranchSpan(values);
    }

    fn cloneStateLoopStateSpan(self: *Cloner, span: Ast.Span(Ast.StateLoopState)) Common.LowerError!Ast.Span(Ast.StateLoopState) {
        const source = try self.pass.allocator.dupe(Ast.StateLoopState, self.pass.program.stateLoopStateSpan(span));
        defer self.pass.allocator.free(source);

        const start: u32 = @intCast(self.pass.program.state_loop_states.items.len);
        try self.pass.program.state_loop_states.ensureUnusedCapacity(self.pass.program.allocator, source.len);
        for (source, 0..) |_, index| {
            const old_id: Ast.StateLoopStateId = @enumFromInt(span.start + @as(u32, @intCast(index)));
            const new_id: Ast.StateLoopStateId = @enumFromInt(start + @as(u32, @intCast(index)));
            try self.state_loop_state_map.put(old_id, new_id);
            self.pass.program.state_loop_states.appendAssumeCapacity(undefined);
        }

        for (source, 0..) |state, index| {
            self.pass.program.state_loop_states.items[start + index] = .{
                .params = state.params,
                .body = try self.cloneExpr(state.body),
            };
        }

        return .{ .start = start, .len = @intCast(source.len) };
    }

    fn cloneStateLoopStateId(self: *Cloner, id: Ast.StateLoopStateId) Ast.StateLoopStateId {
        return self.state_loop_state_map.get(id) orelse
            Common.invariant("state_continue reached SpecConstr clone before its state_loop reserved the target state");
    }

    fn materialize(self: *Cloner, value: Value) Common.LowerError!Ast.ExprId {
        switch (value) {
            .expr => |expr| return expr,
            .expr_with_known_value => |known_value_expr| return known_value_expr.expr,
            .let_ => |let_value| {
                const body = try self.materialize(let_value.body.*);
                return try self.wrapPendingLetsAroundExpr(valueType(self.pass.program, let_value.body.*), body, let_value.lets);
            },
            .if_ => |if_value| {
                const branches = try self.pass.allocator.alloc(Ast.IfBranch, if_value.branches.len);
                defer self.pass.allocator.free(branches);
                for (if_value.branches, 0..) |branch, index| {
                    branches[index] = .{
                        .cond = branch.cond,
                        .body = try self.materialize(branch.body),
                    };
                }
                return try self.addExpr(.{ .ty = if_value.ty, .data = .{ .if_ = .{
                    .branches = try self.pass.program.addIfBranchSpan(branches),
                    .final_else = try self.materialize(if_value.final_else.*),
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
            .nominal => |nominal| return try self.addExpr(.{ .ty = nominal.ty, .data = .{
                .nominal = try self.materialize(nominal.backing.*),
            } }),
            .callable => |callable| return try self.materializeCallable(callable),
            .finite_tags => |finite_tags| return try self.materialize(try self.finiteTagsAsIfValue(finite_tags)),
            .finite_callables => |finite_callables| return try self.materialize(try self.finiteCallablesAsIfValue(finite_callables)),
        }
    }

    fn materializePublic(self: *Cloner, value: Value) Common.LowerError!Ast.ExprId {
        switch (value) {
            .expr => |expr| return expr,
            .expr_with_known_value => |known_value_expr| return known_value_expr.expr,
            .let_ => |let_value| {
                const body = try self.materializePublic(let_value.body.*);
                return try self.wrapPendingLetsAroundExpr(valueType(self.pass.program, let_value.body.*), body, let_value.lets);
            },
            .if_ => |if_value| {
                const branches = try self.pass.allocator.alloc(Ast.IfBranch, if_value.branches.len);
                defer self.pass.allocator.free(branches);
                for (if_value.branches, 0..) |branch, index| {
                    branches[index] = .{
                        .cond = branch.cond,
                        .body = try self.materializePublic(branch.body),
                    };
                }
                return try self.addExpr(.{ .ty = if_value.ty, .data = .{ .if_ = .{
                    .branches = try self.pass.program.addIfBranchSpan(branches),
                    .final_else = try self.materializePublic(if_value.final_else.*),
                } } });
            },
            .tag => |tag| {
                const payloads = try self.pass.allocator.alloc(Ast.ExprId, tag.payloads.len);
                defer self.pass.allocator.free(payloads);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = try self.materializePublic(payload);
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
                        .value = try self.materializePublic(field.value),
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
                    items[index] = try self.materializePublic(item);
                }
                return try self.addExpr(.{ .ty = tuple.ty, .data = .{
                    .tuple = try self.pass.program.addExprSpan(items),
                } });
            },
            .nominal => |nominal| return try self.addExpr(.{ .ty = nominal.ty, .data = .{
                .nominal = try self.materializePublic(nominal.backing.*),
            } }),
            .callable => |callable| return try self.materializePublicCallable(callable),
            .finite_tags => |finite_tags| return try self.materializePublic(try self.finiteTagsAsIfValue(finite_tags)),
            .finite_callables => |finite_callables| return try self.materializePublic(try self.finiteCallablesAsIfValue(finite_callables)),
        }
    }

    fn materializePublicCallable(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
        const source_fn = self.pass.program.fns.items[@intFromEnum(callable.fn_id)];
        return try self.materializeCallableWithCaptures(
            callable.ty,
            callable.fn_id,
            source_fn.captures,
            callable.captures,
        );
    }

    fn finiteTagsAsIfValue(self: *Cloner, finite_tags: FiniteTagsValue) Common.LowerError!Value {
        if (finite_tags.alternatives.len == 0) {
            Common.invariant("finite tag value had no alternatives");
        }
        if (finite_tags.alternatives.len == 1) {
            return .{ .tag = finite_tags.alternatives[0] };
        }

        const branch_count = finite_tags.alternatives.len - 1;
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, branch_count);
        for (finite_tags.alternatives[0..branch_count], branches, 0..) |alternative, *branch, index| {
            branch.* = .{
                .cond = try self.selectorEquals(finite_tags.selector, @intCast(index)),
                .body = .{ .tag = alternative },
            };
        }

        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = .{ .tag = finite_tags.alternatives[branch_count] };
        return .{ .if_ = .{
            .ty = finite_tags.ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn finiteCallablesAsIfValue(self: *Cloner, finite_callables: FiniteCallablesValue) Common.LowerError!Value {
        if (finite_callables.alternatives.len == 0) {
            Common.invariant("finite callable value had no alternatives");
        }
        if (finite_callables.alternatives.len == 1) {
            return .{ .callable = finite_callables.alternatives[0] };
        }

        const branch_count = finite_callables.alternatives.len - 1;
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, branch_count);
        for (finite_callables.alternatives[0..branch_count], branches, 0..) |alternative, *branch, index| {
            branch.* = .{
                .cond = try self.selectorEquals(finite_callables.selector, @intCast(index)),
                .body = .{ .callable = alternative },
            };
        }

        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = .{ .callable = finite_callables.alternatives[branch_count] };
        return .{ .if_ = .{
            .ty = finite_callables.ty,
            .branches = branches,
            .final_else = final_else,
        } };
    }

    fn materializeCallable(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
        const fn_ = self.pass.program.fns.items[@intFromEnum(callable.fn_id)];
        const captures = self.pass.program.typedLocalSpan(fn_.captures);
        if (captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        var all_original = true;
        for (captures, callable.captures) |capture, value| {
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

        if (!all_original) {
            return try self.specializedCallableRef(callable);
        }

        return try self.addExpr(.{ .ty = callable.ty, .data = .{ .fn_ref = callable.fn_id } });
    }

    fn specializedCallableRef(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
        const capture_known_values = try self.callableCaptureKnownValues(callable);
        if (self.existingCallableSpecialization(callable.fn_id, capture_known_values)) |existing| {
            const existing_fn = self.pass.program.fns.items[@intFromEnum(existing)];
            return try self.materializeCallableWithCaptureKnownValues(
                callable.ty,
                existing,
                existing_fn.captures,
                capture_known_values,
                callable.captures,
            );
        }

        const source_fn = self.pass.program.fns.items[@intFromEnum(callable.fn_id)];
        const source_body = self.pass.originalBody(callable.fn_id) orelse switch (source_fn.body) {
            .roc => |body| body,
            .hosted => Common.invariant("hosted callable value needed capture substitution"),
        };

        const source_captures = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(source_captures);
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        var captures = std.ArrayList(Ast.TypedLocal).empty;
        defer captures.deinit(self.pass.allocator);

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        for (source_captures, capture_known_values) |source_capture, capture_known_value| {
            const capture_value = try self.valueFromKnownValueArgs(capture_known_value, &captures);
            try self.putSubst(source_capture.local, capture_value);
        }

        const captures_span = try self.pass.program.addTypedLocalSpan(captures.items);

        const source_args = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.args));
        defer self.pass.allocator.free(source_args);
        const args = try self.pass.allocator.alloc(Ast.TypedLocal, source_args.len);
        defer self.pass.allocator.free(args);
        for (source_args, 0..) |source_arg, index| {
            const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), source_arg.ty);
            args[index] = .{ .local = local, .ty = source_arg.ty };
        }
        const args_span = try self.pass.program.addTypedLocalSpan(args);

        const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.pass.program.fns.items.len)));
        const symbol = self.pass.symbols.fresh();
        try self.pass.program.fns.append(self.pass.allocator, .{
            .symbol = symbol,
            .source = source_fn.source,
            .args = args_span,
            .captures = captures_span,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        try self.pass.callable_specializations.append(self.pass.allocator, .{
            .source_fn = callable.fn_id,
            .captures = capture_known_values,
            .fn_id = fn_id,
        });
        try self.pass.copyProcDebugName(source_fn.symbol, symbol);

        const result = try self.materializeCallableWithCaptureKnownValues(
            callable.ty,
            fn_id,
            captures_span,
            capture_known_values,
            callable.captures,
        );

        for (source_args, args) |source_arg, arg| {
            const arg_expr = try self.addExpr(.{
                .ty = arg.ty,
                .data = .{ .local = arg.local },
            });
            try self.putSubst(source_arg.local, .{ .expr = arg_expr });
        }

        const cloned_body = try self.cloneExpr(source_body);
        self.pass.program.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = symbol,
            .source = source_fn.source,
            .args = args_span,
            .captures = captures_span,
            .body = .{ .roc = cloned_body },
            .ret = source_fn.ret,
        };

        return result;
    }

    fn callableCaptureKnownValues(self: *Cloner, callable: CallableValue) Allocator.Error![]const KnownValue {
        const captures = try self.pass.arena.allocator().alloc(KnownValue, callable.captures.len);
        for (callable.captures, captures) |capture, *out| {
            const known_value = (try self.pass.knownValueFromValue(capture)) orelse {
                out.* = .{ .any = valueType(self.pass.program, capture) };
                continue;
            };
            out.* = (try self.projectableLoopKnownValueForValue(known_value, capture)) orelse
                .{ .any = valueType(self.pass.program, capture) };
        }
        return captures;
    }

    fn existingCallableSpecialization(
        self: *Cloner,
        source_fn: Ast.FnId,
        capture_known_values: []const KnownValue,
    ) ?Ast.FnId {
        for (self.pass.callable_specializations.items) |specialization| {
            if (specialization.source_fn != source_fn) continue;
            if (specialization.captures.len != capture_known_values.len) continue;
            var matches = true;
            for (specialization.captures, capture_known_values) |existing, requested| {
                if (!known_valueEql(self.pass.program, existing, requested)) {
                    matches = false;
                    break;
                }
            }
            if (matches) return specialization.fn_id;
        }
        return null;
    }

    fn materializeCallableWithCaptureKnownValues(
        self: *Cloner,
        ty: Type.TypeId,
        fn_id: Ast.FnId,
        captures_span: Ast.Span(Ast.TypedLocal),
        capture_known_values: []const KnownValue,
        values: []const Value,
    ) Common.LowerError!Ast.ExprId {
        var flattened = std.ArrayList(Ast.ExprId).empty;
        defer flattened.deinit(self.pass.allocator);
        var pending_lets = std.ArrayList(PendingLet).empty;
        defer pending_lets.deinit(self.pass.allocator);

        if (capture_known_values.len != values.len) {
            Common.invariant("callable capture known_value count differed from capture value count");
        }
        for (capture_known_values, values) |known_value, value| {
            if (!try self.appendCaptureExprsFromValue(known_value, value, &flattened, &pending_lets)) {
                Common.invariant("callable capture value could not be split into requested known_value");
            }
        }

        const captures = self.pass.program.typedLocalSpan(captures_span);
        if (captures.len != flattened.items.len) {
            Common.invariant("split callable capture count differed between specialization and materialization");
        }

        const value_exprs = try self.pass.allocator.alloc(?Ast.ExprId, flattened.items.len);
        defer self.pass.allocator.free(value_exprs);
        for (captures, flattened.items, 0..) |capture, value_expr, index| {
            const value_local = localExpr(self.pass.program, value_expr);
            value_exprs[index] = if (value_local != null and value_local.? == capture.local) null else value_expr;
        }

        var result = try self.addExpr(.{ .ty = ty, .data = .{ .fn_ref = fn_id } });
        var index = value_exprs.len;
        while (index > 0) {
            index -= 1;
            const value_expr = value_exprs[index] orelse continue;
            const pat = try self.pass.program.addPat(.{
                .ty = captures[index].ty,
                .data = .{ .bind = captures[index].local },
            });
            result = try self.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = value_expr,
                .rest = result,
            } } });
        }
        return try self.wrapPendingLetsAroundExpr(ty, result, pending_lets.items);
    }

    fn appendCaptureExprsFromValue(
        self: *Cloner,
        known_value: KnownValue,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
        pending_lets: *std.ArrayList(PendingLet),
    ) Common.LowerError!bool {
        if (value == .let_) {
            const let_value = value.let_;
            try pending_lets.appendSlice(self.pass.allocator, let_value.lets);
            return try self.appendCaptureExprsFromValue(known_value, let_value.body.*, out, pending_lets);
        }

        switch (known_value) {
            .any,
            .leaf,
            => {
                try out.append(self.pass.allocator, try self.materializePublic(value));
                return true;
            },
            .tag => |tag| {
                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => return false,
                };
                if (!sameType(self.pass.program, tag.ty, tag_value.ty) or
                    tag.name != tag_value.name or
                    tag.payloads.len != tag_value.payloads.len)
                {
                    return false;
                }
                for (tag.payloads, tag_value.payloads) |payload_known_value, payload| {
                    if (!try self.appendCaptureExprsFromValue(payload_known_value, payload, out, pending_lets)) return false;
                }
                return true;
            },
            .record => |record| {
                if (recordFromValue(value)) |record_value| {
                    for (record.fields) |field_known_value| {
                        const field_value = fieldFromRecord(record_value, field_known_value.name) orelse return false;
                        if (!try self.appendCaptureExprsFromValue(field_known_value.known_value, field_value, out, pending_lets)) return false;
                    }
                    return true;
                }
                return try self.appendFieldReadExprsFromValue(known_value, value, out);
            },
            .tuple => |tuple| {
                if (tupleFromValue(value)) |tuple_value| {
                    if (tuple.items.len != tuple_value.items.len) return false;
                    for (tuple.items, tuple_value.items) |item_known_value, item| {
                        if (!try self.appendCaptureExprsFromValue(item_known_value, item, out, pending_lets)) return false;
                    }
                    return true;
                }
                return try self.appendFieldReadExprsFromValue(known_value, value, out);
            },
            .nominal => |nominal| {
                const backing_value = switch (value) {
                    .nominal => |nominal_value| nominal_value.backing.*,
                    else => return try self.appendFieldReadExprsFromValue(known_value, value, out),
                };
                return try self.appendCaptureExprsFromValue(nominal.backing.*, backing_value, out, pending_lets);
            },
            .callable => |callable| {
                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => return try self.appendFieldReadExprsFromValue(known_value, value, out),
                };
                if (!callableTargetMatches(self.pass.program, callable.fn_id, callable_value.fn_id) or
                    callable.captures.len != callable_value.captures.len)
                {
                    return false;
                }
                for (callable.captures, callable_value.captures) |capture_known_value, capture_value| {
                    if (!try self.appendCaptureExprsFromValue(capture_known_value, capture_value, out, pending_lets)) return false;
                }
                return true;
            },
            .finite_callables => |finite_callables| {
                if (value == .finite_callables) {
                    const finite_value = value.finite_callables;
                    if (!knownCallablesMatchesValue(self.pass.program, finite_callables, finite_value)) return false;
                    try out.append(self.pass.allocator, finite_value.selector);
                    for (finite_callables.alternatives, finite_value.alternatives) |alternative_known_value, alternative_value| {
                        if (!callableTargetMatches(self.pass.program, alternative_known_value.fn_id, alternative_value.fn_id) or
                            alternative_known_value.captures.len != alternative_value.captures.len)
                        {
                            return false;
                        }
                        for (alternative_known_value.captures, alternative_value.captures) |capture_known_value, capture_value| {
                            if (!try self.appendCaptureExprsFromValue(capture_known_value, capture_value, out, pending_lets)) return false;
                        }
                    }
                    return true;
                }

                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => return try self.appendFieldReadExprsFromValue(known_value, value, out),
                };
                const active_index = finiteCallableAlternativeIndex(self.pass.program, finite_callables.alternatives, callable_value) orelse return false;
                try out.append(self.pass.allocator, try self.selectorLiteral(@intCast(active_index)));
                for (finite_callables.alternatives, 0..) |alternative_known_value, alternative_index| {
                    if (alternative_index == active_index) {
                        for (alternative_known_value.captures, callable_value.captures) |capture_known_value, capture_value| {
                            if (!try self.appendCaptureExprsFromValue(capture_known_value, capture_value, out, pending_lets)) return false;
                        }
                    } else {
                        for (alternative_known_value.captures) |capture_known_value| {
                            try self.appendUninitializedExprsForKnownValue(capture_known_value, out);
                        }
                    }
                }
                return true;
            },
            .finite_tags => |finite_tags| {
                if (value == .finite_tags) {
                    const finite_value = value.finite_tags;
                    if (!knownTagsMatchesValue(self.pass.program, finite_tags, finite_value)) return false;
                    try out.append(self.pass.allocator, finite_value.selector);
                    for (finite_tags.alternatives, finite_value.alternatives) |alternative_known_value, alternative_value| {
                        if (alternative_known_value.name != alternative_value.name or
                            alternative_known_value.payloads.len != alternative_value.payloads.len)
                        {
                            return false;
                        }
                        for (alternative_known_value.payloads, alternative_value.payloads) |payload_known_value, payload_value| {
                            if (!try self.appendCaptureExprsFromValue(payload_known_value, payload_value, out, pending_lets)) return false;
                        }
                    }
                    return true;
                }

                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => return try self.appendFieldReadExprsFromValue(known_value, value, out),
                };
                const active_index = finiteTagAlternativeIndex(self.pass.program, finite_tags.alternatives, tag_value) orelse return false;
                try out.append(self.pass.allocator, try self.selectorLiteral(@intCast(active_index)));
                for (finite_tags.alternatives, 0..) |alternative_known_value, alternative_index| {
                    if (alternative_index == active_index) {
                        for (alternative_known_value.payloads, tag_value.payloads) |payload_known_value, payload_value| {
                            if (!try self.appendCaptureExprsFromValue(payload_known_value, payload_value, out, pending_lets)) return false;
                        }
                    } else {
                        for (alternative_known_value.payloads) |payload_known_value| {
                            try self.appendUninitializedExprsForKnownValue(payload_known_value, out);
                        }
                    }
                }
                return true;
            },
        }
    }

    fn materializeCallableWithCaptures(
        self: *Cloner,
        ty: Type.TypeId,
        fn_id: Ast.FnId,
        captures_span: Ast.Span(Ast.TypedLocal),
        values: []const Value,
    ) Common.LowerError!Ast.ExprId {
        const captures = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(captures_span));
        defer self.pass.allocator.free(captures);
        if (captures.len != values.len) {
            Common.invariant("callable value capture count differed from specialized function capture count");
        }

        const value_exprs = try self.pass.allocator.alloc(?Ast.ExprId, values.len);
        defer self.pass.allocator.free(value_exprs);
        for (captures, values, 0..) |capture, value, index| {
            const value_expr = try self.materializePublic(value);
            const value_local = localExpr(self.pass.program, value_expr);
            value_exprs[index] = if (value_local != null and value_local.? == capture.local) null else value_expr;
        }

        var result = try self.addExpr(.{ .ty = ty, .data = .{ .fn_ref = fn_id } });
        var index = value_exprs.len;
        while (index > 0) {
            index -= 1;
            const value_expr = value_exprs[index] orelse continue;
            const pat = try self.pass.program.addPat(.{
                .ty = captures[index].ty,
                .data = .{ .bind = captures[index].local },
            });
            result = try self.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = value_expr,
                .rest = result,
            } } });
        }
        return result;
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

        if (self.pass.program.locals.items[@intFromEnum(local)].binder) |binder| {
            for (self.pass.program.locals.items, 0..) |candidate, index| {
                if (candidate.id == local or candidate.binder == null or candidate.binder.? != binder) continue;
                const candidate_local: Ast.LocalId = @enumFromInt(@as(u32, @intCast(index)));
                if (!self.localBelongsToCurrentClone(candidate_local)) continue;
                const candidate_previous = self.subst.get(candidate_local);
                try self.changes.append(self.pass.allocator, .{
                    .key = .{ .local = candidate_local },
                    .previous = candidate_previous,
                });
                try self.subst.put(candidate_local, value);
            }
        }
    }

    fn localBelongsToCurrentClone(self: *Cloner, local: Ast.LocalId) bool {
        if (self.localBelongsToFn(self.source_fn, local)) return true;
        for (self.inline_stack.items) |active| {
            if (self.localBelongsToFn(active.fn_id, local)) return true;
        }
        return false;
    }

    fn localBelongsToFn(self: *Cloner, fn_id: Ast.FnId, local: Ast.LocalId) bool {
        const fn_ = self.pass.program.fns.items[@intFromEnum(fn_id)];
        if (localInTypedLocalSpan(self.pass.program.typedLocalSpan(fn_.args), local)) return true;
        if (localInTypedLocalSpan(self.pass.program.typedLocalSpan(fn_.captures), local)) return true;
        const body = self.pass.originalBody(fn_id) orelse switch (fn_.body) {
            .roc => |body| body,
            .hosted => return false,
        };
        return localUseCountInExpr(self.pass.program, local, body) != 0;
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
            }
        }
        self.changes.shrinkRetainingCapacity(start);
    }

    fn addExpr(self: *Cloner, expr: Ast.Expr) Allocator.Error!Ast.ExprId {
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
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
        .local => |local| local,
        else => null,
    };
}

fn localInTypedLocalSpan(locals: []const Ast.TypedLocal, local: Ast.LocalId) bool {
    for (locals) |candidate| {
        if (candidate.local == local) return true;
    }
    return false;
}

/// A body with an early return can be cloned into a worker with the same return
/// target, but it cannot be directly inlined into a different caller body.
fn exprContainsReturn(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
        .return_ => true,
        .local,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .static_data,
        .uninitialized,
        .uninitialized_payload,
        .fn_ref,
        .crash,
        .comptime_exhaustiveness_failed,
        => false,
        .static_data_candidate => |candidate| exprContainsReturn(program, candidate.fallback),
        .list,
        .tuple,
        => |items| exprSpanContainsReturn(program, items),
        .record => |fields| blk: {
            for (program.fieldExprSpan(fields)) |field| {
                if (exprContainsReturn(program, field.value)) break :blk true;
            }
            break :blk false;
        },
        .tag => |tag| exprSpanContainsReturn(program, tag.payloads),
        .nominal,
        .dbg,
        .expect,
        => |child| exprContainsReturn(program, child),
        .expect_err => |expect_err| exprContainsReturn(program, expect_err.msg),
        .comptime_branch_taken => |taken| exprContainsReturn(program, taken.body),
        .let_ => |let_| exprContainsReturn(program, let_.value) or exprContainsReturn(program, let_.rest),
        .lambda,
        .def_ref,
        .fn_def,
        => Common.invariant("pre-lift function expression reached call-pattern return scan"),
        .call_value => |call| exprContainsReturn(program, call.callee) or exprSpanContainsReturn(program, call.args),
        .call_proc => |call| exprSpanContainsReturn(program, call.args),
        .low_level => |call| exprSpanContainsReturn(program, call.args),
        .field_access => |field| exprContainsReturn(program, field.receiver),
        .tuple_access => |access| exprContainsReturn(program, access.tuple),
        .structural_eq => |eq| exprContainsReturn(program, eq.lhs) or exprContainsReturn(program, eq.rhs),
        .structural_hash => |h| exprContainsReturn(program, h.value) or exprContainsReturn(program, h.hasher),
        .match_ => |match| blk: {
            if (exprContainsReturn(program, match.scrutinee)) break :blk true;
            for (program.branchSpan(match.branches)) |branch| {
                if (branch.guard) |guard| {
                    if (exprContainsReturn(program, guard)) break :blk true;
                }
                if (exprContainsReturn(program, branch.body)) break :blk true;
            }
            break :blk false;
        },
        .if_ => |if_| blk: {
            for (program.ifBranchSpan(if_.branches)) |branch| {
                if (exprContainsReturn(program, branch.cond) or exprContainsReturn(program, branch.body)) {
                    break :blk true;
                }
            }
            break :blk exprContainsReturn(program, if_.final_else);
        },
        .if_initialized_payload => |payload_switch| exprContainsReturn(program, payload_switch.cond) or
            exprContainsReturn(program, payload_switch.initialized) or
            exprContainsReturn(program, payload_switch.uninitialized),
        .try_sequence => |sequence| exprContainsReturn(program, sequence.try_expr) or
            exprContainsReturn(program, sequence.ok_body),
        .try_record_sequence => |sequence| exprContainsReturn(program, sequence.try_expr) or
            exprContainsReturn(program, sequence.ok_body),
        .block => |block| stmtSpanContainsReturn(program, block.statements) or exprContainsReturn(program, block.final_expr),
        .loop_ => |loop| exprSpanContainsReturn(program, loop.initial_values) or exprContainsReturn(program, loop.body),
        .state_loop => |state_loop| blk: {
            if (exprSpanContainsReturn(program, state_loop.entry_values)) break :blk true;
            for (program.stateLoopStateSpan(state_loop.states)) |state| {
                if (exprContainsReturn(program, state.body)) break :blk true;
            }
            break :blk false;
        },
        .break_ => |maybe| if (maybe) |value| exprContainsReturn(program, value) else false,
        .continue_ => |continue_| exprSpanContainsReturn(program, continue_.values),
        .state_continue => |continue_| exprSpanContainsReturn(program, continue_.values),
    };
}

fn exprSpanContainsReturn(program: *const Ast.Program, span: Ast.Span(Ast.ExprId)) bool {
    for (program.exprSpan(span)) |expr_id| {
        if (exprContainsReturn(program, expr_id)) return true;
    }
    return false;
}

fn stmtContainsReturn(program: *const Ast.Program, stmt_id: Ast.StmtId) bool {
    return switch (program.stmts.items[@intFromEnum(stmt_id)]) {
        .return_ => true,
        .let_ => |let_| exprContainsReturn(program, let_.value),
        .expr,
        .expect,
        .dbg,
        => |expr_id| exprContainsReturn(program, expr_id),
        .uninitialized,
        .crash,
        => false,
    };
}

fn stmtSpanContainsReturn(program: *const Ast.Program, span: Ast.Span(Ast.StmtId)) bool {
    for (program.stmtSpan(span)) |stmt_id| {
        if (stmtContainsReturn(program, stmt_id)) return true;
    }
    return false;
}

fn localUseCountInExpr(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) usize {
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
        .local => |seen| if (seen == local) 1 else 0,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .static_data,
        .fn_ref,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        => 0,
        .static_data_candidate => |candidate| localUseCountInExpr(program, local, candidate.fallback),
        .list,
        .tuple,
        => |items| localUseCountInExprSpan(program, local, items),
        .record => |fields| blk: {
            var count: usize = 0;
            for (program.fieldExprSpan(fields)) |field| count += localUseCountInExpr(program, local, field.value);
            break :blk count;
        },
        .tag => |tag| localUseCountInExprSpan(program, local, tag.payloads),
        .nominal,
        .return_,
        .dbg,
        .expect,
        => |child| localUseCountInExpr(program, local, child),
        .expect_err => |expect_err| localUseCountInExpr(program, local, expect_err.msg),
        .comptime_branch_taken => |taken| localUseCountInExpr(program, local, taken.body),
        .let_ => |let_| localUseCountInExpr(program, local, let_.value) + localUseCountInExpr(program, local, let_.rest),
        .lambda,
        .def_ref,
        .fn_def,
        => 0,
        .call_value => |call| localUseCountInExpr(program, local, call.callee) + localUseCountInExprSpan(program, local, call.args),
        .call_proc => |call| localUseCountInExprSpan(program, local, call.args),
        .low_level => |call| localUseCountInExprSpan(program, local, call.args),
        .field_access => |field| localUseCountInExpr(program, local, field.receiver),
        .tuple_access => |access| localUseCountInExpr(program, local, access.tuple),
        .structural_eq => |eq| localUseCountInExpr(program, local, eq.lhs) + localUseCountInExpr(program, local, eq.rhs),
        .structural_hash => |h| localUseCountInExpr(program, local, h.value) + localUseCountInExpr(program, local, h.hasher),
        .match_ => |match| blk: {
            var count = localUseCountInExpr(program, local, match.scrutinee);
            for (program.branchSpan(match.branches)) |branch| {
                if (branch.guard) |guard| count += localUseCountInExpr(program, local, guard);
                count += localUseCountInExpr(program, local, branch.body);
            }
            break :blk count;
        },
        .if_ => |if_| blk: {
            var count: usize = 0;
            for (program.ifBranchSpan(if_.branches)) |branch| {
                count += localUseCountInExpr(program, local, branch.cond);
                count += localUseCountInExpr(program, local, branch.body);
            }
            count += localUseCountInExpr(program, local, if_.final_else);
            break :blk count;
        },
        .block => |block| blk: {
            var count: usize = 0;
            for (program.stmtSpan(block.statements)) |stmt| count += localUseCountInStmt(program, local, stmt);
            count += localUseCountInExpr(program, local, block.final_expr);
            break :blk count;
        },
        .loop_ => |loop| localUseCountInExprSpan(program, local, loop.initial_values) + localUseCountInExpr(program, local, loop.body),
        .state_loop => |state_loop| blk: {
            var count = localUseCountInExprSpan(program, local, state_loop.entry_values);
            for (program.stateLoopStateSpan(state_loop.states)) |state| {
                count += localUseCountInExpr(program, local, state.body);
            }
            break :blk count;
        },
        .break_ => |maybe| if (maybe) |value| localUseCountInExpr(program, local, value) else 0,
        .continue_ => |continue_| localUseCountInExprSpan(program, local, continue_.values),
        .state_continue => |continue_| localUseCountInExprSpan(program, local, continue_.values),
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
    for (program.exprSpan(span)) |expr| count += localUseCountInExpr(program, local, expr);
    return count;
}

fn localMaxUseCountPerPathInExpr(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) usize {
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
        .local => |seen| if (seen == local) 1 else 0,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .static_data,
        .fn_ref,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        .lambda,
        .def_ref,
        .fn_def,
        => 0,
        .static_data_candidate => |candidate| localMaxUseCountPerPathInExpr(program, local, candidate.fallback),
        .list,
        .tuple,
        => |items| localMaxUseCountPerPathInExprSpan(program, local, items),
        .record => |fields| blk: {
            var count: usize = 0;
            for (program.fieldExprSpan(fields)) |field| count += localMaxUseCountPerPathInExpr(program, local, field.value);
            break :blk count;
        },
        .tag => |tag| localMaxUseCountPerPathInExprSpan(program, local, tag.payloads),
        .nominal,
        .return_,
        .dbg,
        .expect,
        => |child| localMaxUseCountPerPathInExpr(program, local, child),
        .expect_err => |expect_err| localMaxUseCountPerPathInExpr(program, local, expect_err.msg),
        .comptime_branch_taken => |taken| localMaxUseCountPerPathInExpr(program, local, taken.body),
        .let_ => |let_| localMaxUseCountPerPathInExpr(program, local, let_.value) +
            localMaxUseCountPerPathInExpr(program, local, let_.rest),
        .call_value => |call| localMaxUseCountPerPathInExpr(program, local, call.callee) +
            localMaxUseCountPerPathInExprSpan(program, local, call.args),
        .call_proc => |call| localMaxUseCountPerPathInExprSpan(program, local, call.args),
        .low_level => |call| localMaxUseCountPerPathInExprSpan(program, local, call.args),
        .field_access => |field| localMaxUseCountPerPathInExpr(program, local, field.receiver),
        .tuple_access => |access| localMaxUseCountPerPathInExpr(program, local, access.tuple),
        .structural_eq => |eq| localMaxUseCountPerPathInExpr(program, local, eq.lhs) +
            localMaxUseCountPerPathInExpr(program, local, eq.rhs),
        .structural_hash => |h| localMaxUseCountPerPathInExpr(program, local, h.value) +
            localMaxUseCountPerPathInExpr(program, local, h.hasher),
        .match_ => |match| blk: {
            const scrutinee_count = localMaxUseCountPerPathInExpr(program, local, match.scrutinee);
            var max_branch_count: usize = 0;
            for (program.branchSpan(match.branches)) |branch| {
                var branch_count: usize = if (branch.guard) |guard|
                    localMaxUseCountPerPathInExpr(program, local, guard)
                else
                    0;
                branch_count += localMaxUseCountPerPathInExpr(program, local, branch.body);
                max_branch_count = @max(max_branch_count, branch_count);
            }
            break :blk scrutinee_count + max_branch_count;
        },
        .if_ => |if_| blk: {
            var count: usize = 0;
            var max_branch_count: usize = 0;
            for (program.ifBranchSpan(if_.branches)) |branch| {
                count += localMaxUseCountPerPathInExpr(program, local, branch.cond);
                max_branch_count = @max(max_branch_count, localMaxUseCountPerPathInExpr(program, local, branch.body));
            }
            max_branch_count = @max(max_branch_count, localMaxUseCountPerPathInExpr(program, local, if_.final_else));
            break :blk count + max_branch_count;
        },
        .block => |block| blk: {
            var count: usize = 0;
            for (program.stmtSpan(block.statements)) |stmt| count += localMaxUseCountPerPathInStmt(program, local, stmt);
            count += localMaxUseCountPerPathInExpr(program, local, block.final_expr);
            break :blk count;
        },
        .loop_ => |loop| blk: {
            const initial_count = localMaxUseCountPerPathInExprSpan(program, local, loop.initial_values);
            const body_count = localMaxUseCountPerPathInExpr(program, local, loop.body);
            break :blk initial_count + if (body_count == 0) @as(usize, 0) else @max(body_count, 2);
        },
        .state_loop => |state_loop| blk: {
            const initial_count = localMaxUseCountPerPathInExprSpan(program, local, state_loop.entry_values);
            var max_state_count: usize = 0;
            for (program.stateLoopStateSpan(state_loop.states)) |state| {
                max_state_count = @max(max_state_count, localMaxUseCountPerPathInExpr(program, local, state.body));
            }
            break :blk initial_count + if (max_state_count == 0) @as(usize, 0) else @max(max_state_count, 2);
        },
        .break_ => |maybe| if (maybe) |value| localMaxUseCountPerPathInExpr(program, local, value) else 0,
        .continue_ => |continue_| localMaxUseCountPerPathInExprSpan(program, local, continue_.values),
        .state_continue => |continue_| localMaxUseCountPerPathInExprSpan(program, local, continue_.values),
        .if_initialized_payload => |payload_switch| localMaxUseCountPerPathInExpr(program, local, payload_switch.cond) +
            @max(
                (if (payload_switch.payload == local) @as(usize, 1) else 0) +
                    localMaxUseCountPerPathInExpr(program, local, payload_switch.initialized),
                localMaxUseCountPerPathInExpr(program, local, payload_switch.uninitialized),
            ),
        .try_sequence => |sequence| localMaxUseCountPerPathInExpr(program, local, sequence.try_expr) +
            if (sequence.ok_local == local) 0 else localMaxUseCountPerPathInExpr(program, local, sequence.ok_body),
        .try_record_sequence => |sequence| localMaxUseCountPerPathInExpr(program, local, sequence.try_expr) +
            if (sequence.value_local == local or sequence.rest_local == local) 0 else localMaxUseCountPerPathInExpr(program, local, sequence.ok_body),
    };
}

fn localMaxUseCountPerPathInExprSpan(program: *const Ast.Program, local: Ast.LocalId, span: Ast.Span(Ast.ExprId)) usize {
    var count: usize = 0;
    for (program.exprSpan(span)) |expr| count += localMaxUseCountPerPathInExpr(program, local, expr);
    return count;
}

fn discardedExprIsEffectFree(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
        .local,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .static_data,
        .fn_ref,
        .uninitialized,
        .uninitialized_payload,
        => true,
        .static_data_candidate => |candidate| discardedExprIsEffectFree(program, candidate.fallback),
        .list,
        .tuple,
        => |items| discardedExprSpanIsEffectFree(program, items),
        .record => |fields| blk: {
            for (program.fieldExprSpan(fields)) |field| {
                if (!discardedExprIsEffectFree(program, field.value)) break :blk false;
            }
            break :blk true;
        },
        .tag => |tag| discardedExprSpanIsEffectFree(program, tag.payloads),
        .nominal => |backing| discardedExprIsEffectFree(program, backing),
        .let_ => |let_| discardedExprIsEffectFree(program, let_.value) and discardedExprIsEffectFree(program, let_.rest),
        .field_access => |field| discardedExprIsEffectFree(program, field.receiver),
        .tuple_access => |access| discardedExprIsEffectFree(program, access.tuple),
        .comptime_branch_taken => |taken| discardedExprIsEffectFree(program, taken.body),
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
        .block,
        .loop_,
        .state_loop,
        .break_,
        .continue_,
        .state_continue,
        .return_,
        .dbg,
        .expect,
        .expect_err,
        .crash,
        .comptime_exhaustiveness_failed,
        .if_initialized_payload,
        .try_sequence,
        .try_record_sequence,
        => false,
    };
}

fn discardedExprSpanIsEffectFree(program: *const Ast.Program, span: Ast.Span(Ast.ExprId)) bool {
    for (program.exprSpan(span)) |expr| {
        if (!discardedExprIsEffectFree(program, expr)) return false;
    }
    return true;
}

fn localUseCountInStmt(program: *const Ast.Program, local: Ast.LocalId, stmt_id: Ast.StmtId) usize {
    return switch (program.stmts.items[@intFromEnum(stmt_id)]) {
        .uninitialized => 0,
        .let_ => |let_| localUseCountInExpr(program, local, let_.value),
        .expr,
        .expect,
        .dbg,
        .return_,
        => |expr| localUseCountInExpr(program, local, expr),
        .crash => 0,
    };
}

fn localMaxUseCountPerPathInStmt(program: *const Ast.Program, local: Ast.LocalId, stmt_id: Ast.StmtId) usize {
    return switch (program.stmts.items[@intFromEnum(stmt_id)]) {
        .uninitialized => 0,
        .let_ => |let_| localMaxUseCountPerPathInExpr(program, local, let_.value),
        .expr,
        .expect,
        .dbg,
        .return_,
        => |expr| localMaxUseCountPerPathInExpr(program, local, expr),
        .crash => 0,
    };
}

fn localMaxUseCountPerPathInBlockTail(program: *const Ast.Program, local: Ast.LocalId, tail: BlockTail) usize {
    var count: usize = 0;
    for (tail.statements) |stmt| count += localMaxUseCountPerPathInStmt(program, local, stmt);
    count += localMaxUseCountPerPathInExpr(program, local, tail.final_expr);
    return count;
}

const LocalUseScan = struct {
    seen_effect: bool = false,
    found_before_effect: bool = false,
    found_after_effect: bool = false,
};

fn localUseBeforeEffectInBlockTail(program: *const Ast.Program, local: Ast.LocalId, tail: BlockTail) bool {
    var scan: LocalUseScan = .{};
    for (tail.statements) |stmt| scanLocalUseInStmt(program, local, stmt, &scan);
    scanLocalUseInExpr(program, local, tail.final_expr, &scan);
    return scan.found_before_effect and !scan.found_after_effect;
}

fn localUseBeforeEffect(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId) bool {
    var scan: LocalUseScan = .{};
    scanLocalUseInExpr(program, local, expr_id, &scan);
    return scan.found_before_effect and !scan.found_after_effect;
}

fn scanLocalUseInExpr(program: *const Ast.Program, local: Ast.LocalId, expr_id: Ast.ExprId, scan: *LocalUseScan) void {
    const expr = program.exprs.items[@intFromEnum(expr_id)];
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
        .static_data,
        .fn_ref,
        .uninitialized,
        .uninitialized_payload,
        => {},
        .static_data_candidate => |candidate| scanLocalUseInExpr(program, local, candidate.fallback, scan),
        .crash, .comptime_exhaustiveness_failed => scan.seen_effect = true,
        .list,
        .tuple,
        => |items| scanLocalUseInExprSpan(program, local, items, scan),
        .record => |fields| {
            for (program.fieldExprSpan(fields)) |field| scanLocalUseInExpr(program, local, field.value, scan);
        },
        .tag => |tag| scanLocalUseInExprSpan(program, local, tag.payloads, scan),
        .nominal => |child| scanLocalUseInExpr(program, local, child, scan),
        .return_ => |child| {
            scanLocalUseInExpr(program, local, child, scan);
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
            const after_scrutinee = scan.*;
            var merged = after_scrutinee;
            for (program.branchSpan(match.branches)) |branch| {
                var branch_scan = after_scrutinee;
                if (branch.guard) |guard| scanLocalUseInExpr(program, local, guard, &branch_scan);
                scanLocalUseInExpr(program, local, branch.body, &branch_scan);
                merged.found_before_effect = merged.found_before_effect or branch_scan.found_before_effect;
                merged.found_after_effect = merged.found_after_effect or branch_scan.found_after_effect;
                merged.seen_effect = merged.seen_effect or branch_scan.seen_effect;
            }
            scan.* = merged;
        },
        .if_ => |if_| {
            var condition_scan = scan.*;
            var merged = condition_scan;
            for (program.ifBranchSpan(if_.branches)) |branch| {
                scanLocalUseInExpr(program, local, branch.cond, &condition_scan);
                var branch_scan = condition_scan;
                scanLocalUseInExpr(program, local, branch.body, &branch_scan);
                merged.found_before_effect = merged.found_before_effect or branch_scan.found_before_effect;
                merged.found_after_effect = merged.found_after_effect or branch_scan.found_after_effect;
                merged.seen_effect = merged.seen_effect or branch_scan.seen_effect;
            }
            var else_scan = condition_scan;
            scanLocalUseInExpr(program, local, if_.final_else, &else_scan);
            merged.found_before_effect = merged.found_before_effect or else_scan.found_before_effect;
            merged.found_after_effect = merged.found_after_effect or else_scan.found_after_effect;
            merged.seen_effect = merged.seen_effect or else_scan.seen_effect;
            scan.* = merged;
        },
        .block => |block| {
            for (program.stmtSpan(block.statements)) |stmt| scanLocalUseInStmt(program, local, stmt, scan);
            scanLocalUseInExpr(program, local, block.final_expr, scan);
        },
        .loop_ => |loop| {
            scanLocalUseInExprSpan(program, local, loop.initial_values, scan);
            scanLocalUseInExpr(program, local, loop.body, scan);
        },
        .state_loop => |state_loop| {
            scanLocalUseInExprSpan(program, local, state_loop.entry_values, scan);
            for (program.stateLoopStateSpan(state_loop.states)) |state| {
                scanLocalUseInExpr(program, local, state.body, scan);
            }
        },
        .break_ => |maybe| {
            if (maybe) |value| scanLocalUseInExpr(program, local, value, scan);
            scan.seen_effect = true;
        },
        .continue_ => |continue_| {
            scanLocalUseInExprSpan(program, local, continue_.values, scan);
            scan.seen_effect = true;
        },
        .state_continue => |continue_| {
            scanLocalUseInExprSpan(program, local, continue_.values, scan);
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
    for (program.exprSpan(span)) |expr| scanLocalUseInExpr(program, local, expr, scan);
}

fn scanLocalUseInStmt(program: *const Ast.Program, local: Ast.LocalId, stmt_id: Ast.StmtId, scan: *LocalUseScan) void {
    switch (program.stmts.items[@intFromEnum(stmt_id)]) {
        .uninitialized => {},
        .let_ => |let_| scanLocalUseInExpr(program, local, let_.value, scan),
        .expr => |expr| scanLocalUseInExpr(program, local, expr, scan),
        .expect,
        .dbg,
        => |expr| {
            scanLocalUseInExpr(program, local, expr, scan);
            scan.seen_effect = true;
        },
        .return_ => |expr| {
            scanLocalUseInExpr(program, local, expr, scan);
            scan.seen_effect = true;
        },
        .crash => scan.seen_effect = true,
    }
}

fn canReadFieldsFromExpr(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
        .local,
        .field_access,
        .tuple_access,
        => true,
        else => false,
    };
}

fn projectableExprFromValue(value: Value) ?Ast.ExprId {
    return switch (value) {
        .expr => |expr| expr,
        .expr_with_known_value => |known_value_expr| known_value_expr.expr,
        else => null,
    };
}

fn valueFromProjectedExpr(expr: Ast.ExprId, known_value: KnownValue) Value {
    return switch (known_value) {
        .any => .{ .expr = expr },
        else => .{ .expr_with_known_value = .{
            .expr = expr,
            .known_value = known_value,
        } },
    };
}

fn fieldKnownValueFromKnownValue(known_value: KnownValue, name: names.RecordFieldNameId) ?KnownValue {
    return switch (known_value) {
        .record => |record| blk: {
            for (record.fields) |field| {
                if (field.name == name) break :blk field.known_value;
            }
            break :blk null;
        },
        .nominal => |nominal| fieldKnownValueFromKnownValue(nominal.backing.*, name),
        else => null,
    };
}

fn itemKnownValueFromKnownValue(known_value: KnownValue, index: u32) ?KnownValue {
    return switch (known_value) {
        .tuple => |tuple| if (index < tuple.items.len) tuple.items[index] else null,
        .nominal => |nominal| itemKnownValueFromKnownValue(nominal.backing.*, index),
        else => null,
    };
}

fn knownTagForPattern(known_value: KnownValue, name: names.TagNameId) ?KnownTag {
    return switch (known_value) {
        .tag => |tag| if (tag.name == name) tag else null,
        .finite_tags => |finite_tags| blk: {
            for (finite_tags.alternatives) |alternative| {
                if (alternative.name == name) break :blk alternative;
            }
            break :blk null;
        },
        .nominal => |nominal| knownTagForPattern(nominal.backing.*, name),
        else => null,
    };
}

fn patternDefinitelyExcludedByKnownValue(program: *const Ast.Program, pat_id: Ast.PatId, known_value: KnownValue) bool {
    const pat = program.pats.items[@intFromEnum(pat_id)];
    return switch (pat.data) {
        .as => |as| patternDefinitelyExcludedByKnownValue(program, as.pattern, known_value),
        .nominal => |backing| switch (known_value) {
            .nominal => |nominal| patternDefinitelyExcludedByKnownValue(program, backing, nominal.backing.*),
            else => false,
        },
        .tag => |tag_pat| switch (known_value) {
            .tag,
            .finite_tags,
            => knownTagForPattern(known_value, tag_pat.name) == null,
            .nominal => |nominal| patternDefinitelyExcludedByKnownValue(program, pat_id, nominal.backing.*),
            else => false,
        },
        else => false,
    };
}

fn known_valueType(known_value: KnownValue) Type.TypeId {
    return switch (known_value) {
        .any => |ty| ty,
        .leaf => |ty| ty,
        .tag => |tag| tag.ty,
        .record => |record| record.ty,
        .tuple => |tuple| tuple.ty,
        .nominal => |nominal| nominal.ty,
        .callable => |callable| callable.ty,
        .finite_tags => |finite_tags| finite_tags.ty,
        .finite_callables => |finite_callables| finite_callables.ty,
    };
}

fn valueType(program: *const Ast.Program, value: Value) Type.TypeId {
    return switch (value) {
        .expr => |expr| program.exprs.items[@intFromEnum(expr)].ty,
        .expr_with_known_value => |known_value_expr| program.exprs.items[@intFromEnum(known_value_expr.expr)].ty,
        .let_ => |let_value| valueType(program, let_value.body.*),
        .if_ => |if_value| if_value.ty,
        .tag => |tag| tag.ty,
        .record => |record| record.ty,
        .tuple => |tuple| tuple.ty,
        .nominal => |nominal| nominal.ty,
        .callable => |callable| callable.ty,
        .finite_tags => |finite_tags| finite_tags.ty,
        .finite_callables => |finite_callables| finite_callables.ty,
    };
}

fn leafKnownValueFromValue(program: *const Ast.Program, value: Value) Allocator.Error!?KnownValue {
    return switch (value) {
        .expr => |expr| switch (program.exprs.items[@intFromEnum(expr)].data) {
            .local => blk: {
                const ty = program.exprs.items[@intFromEnum(expr)].ty;
                if (try typeMayContainRefcounted(program, ty)) break :blk null;
                break :blk KnownValue{ .leaf = ty };
            },
            else => null,
        },
        else => null,
    };
}

fn typeMayContainRefcounted(program: *const Ast.Program, ty: Type.TypeId) Allocator.Error!bool {
    var stack = std.ArrayList(Type.TypeId).empty;
    defer stack.deinit(program.allocator);
    return try typeMayContainRefcountedInner(program, ty, &stack);
}

fn typeMayContainRefcountedInner(
    program: *const Ast.Program,
    ty: Type.TypeId,
    stack: *std.ArrayList(Type.TypeId),
) Allocator.Error!bool {
    for (stack.items) |active| {
        if (active == ty) return true;
    }

    try stack.append(program.allocator, ty);
    defer _ = stack.pop();

    return switch (program.types.get(ty)) {
        .primitive => |primitive| primitive == .str,
        .named => |named| if (named.backing) |backing|
            try typeMayContainRefcountedInner(program, backing.ty, stack)
        else
            true,
        .record => |fields_span| blk: {
            for (program.types.fieldSpan(fields_span)) |field| {
                if (try typeMayContainRefcountedInner(program, field.ty, stack)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |items_span| blk: {
            for (program.types.span(items_span)) |item| {
                if (try typeMayContainRefcountedInner(program, item, stack)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tags_span| blk: {
            for (program.types.tagSpan(tags_span)) |tag| {
                for (program.types.span(tag.payloads)) |payload| {
                    if (try typeMayContainRefcountedInner(program, payload, stack)) break :blk true;
                }
            }
            break :blk false;
        },
        .zst => false,
        .list,
        .box,
        .func,
        .erased,
        => true,
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
        if (!known_valueEql(program, lhs_arg, rhs_arg)) return false;
    }
    return true;
}

fn valueDemandEql(lhs: ValueDemand, rhs: ValueDemand) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .none,
        .materialize,
        => true,
        .record => |lhs_fields| blk: {
            const rhs_fields = rhs.record;
            if (lhs_fields.len != rhs_fields.len) break :blk false;
            for (lhs_fields) |lhs_field| {
                const rhs_field = fieldDemandByName(rhs_fields, lhs_field.name) orelse break :blk false;
                if (!valueDemandEql(lhs_field.demand.*, rhs_field.demand.*)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |lhs_items| blk: {
            const rhs_items = rhs.tuple;
            if (lhs_items.len != rhs_items.len) break :blk false;
            for (lhs_items) |lhs_item| {
                const rhs_item = itemDemandByIndex(rhs_items, lhs_item.index) orelse break :blk false;
                if (!valueDemandEql(lhs_item.demand.*, rhs_item.demand.*)) break :blk false;
            }
            break :blk true;
        },
        .nominal => valueDemandEql(lhs.nominal.*, rhs.nominal.*),
        .callable => |lhs_callable| blk: {
            const rhs_callable = rhs.callable;
            if (lhs_callable.captures.len != rhs_callable.captures.len) break :blk false;
            for (lhs_callable.captures, rhs_callable.captures) |lhs_capture, rhs_capture| {
                if (!valueDemandEql(lhs_capture, rhs_capture)) break :blk false;
            }
            break :blk true;
        },
    };
}

fn fieldDemandByName(fields: []const FieldDemand, name: names.RecordFieldNameId) ?FieldDemand {
    for (fields) |field| {
        if (field.name == name) return field;
    }
    return null;
}

fn itemDemandByIndex(items: []const ItemDemand, index: u32) ?ItemDemand {
    for (items) |item| {
        if (item.index == index) return item;
    }
    return null;
}

fn joinKnownValuesInArena(
    program: *const Ast.Program,
    arena: Allocator,
    lhs: KnownValue,
    rhs: KnownValue,
) Allocator.Error!?KnownValue {
    if (known_valueEql(program, lhs, rhs)) return lhs;
    if (!sameType(program, known_valueType(lhs), known_valueType(rhs))) return null;
    if (try commonKnownTags(program, arena, lhs, rhs)) |finite_tags| return finite_tags;
    if (try commonKnownCallables(program, arena, lhs, rhs)) |finite_callables| return finite_callables;

    return switch (lhs) {
        .any => |ty| KnownValue{ .any = ty },
        .leaf => |ty| blk: {
            const rhs_ty = switch (rhs) {
                .leaf => |rhs_ty| rhs_ty,
                else => break :blk null,
            };
            break :blk if (sameType(program, ty, rhs_ty)) KnownValue{ .leaf = ty } else null;
        },
        .tag => |lhs_tag| blk: {
            const rhs_tag = switch (rhs) {
                .tag => |tag| tag,
                else => break :blk null,
            };
            if (lhs_tag.name != rhs_tag.name or lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk null;
            const payloads = try arena.alloc(KnownValue, lhs_tag.payloads.len);
            for (lhs_tag.payloads, rhs_tag.payloads, 0..) |lhs_payload, rhs_payload, index| {
                payloads[index] = (try joinKnownValuesInArena(program, arena, lhs_payload, rhs_payload)) orelse
                    (joinUnknownChild(program, lhs_payload, rhs_payload) orelse break :blk null);
            }
            break :blk KnownValue{ .tag = .{
                .ty = lhs_tag.ty,
                .name = lhs_tag.name,
                .payloads = payloads,
            } };
        },
        .record => |lhs_record| blk: {
            const rhs_record = switch (rhs) {
                .record => |record| record,
                else => break :blk null,
            };
            if (lhs_record.fields.len != rhs_record.fields.len) break :blk null;
            const fields = try arena.alloc(KnownField, lhs_record.fields.len);
            for (lhs_record.fields, rhs_record.fields, 0..) |lhs_field, rhs_field, index| {
                if (lhs_field.name != rhs_field.name) break :blk null;
                fields[index] = .{
                    .name = lhs_field.name,
                    .known_value = (try joinKnownValuesInArena(program, arena, lhs_field.known_value, rhs_field.known_value)) orelse
                        (joinUnknownChild(program, lhs_field.known_value, rhs_field.known_value) orelse break :blk null),
                };
            }
            break :blk KnownValue{ .record = .{
                .ty = lhs_record.ty,
                .fields = fields,
            } };
        },
        .tuple => |lhs_tuple| blk: {
            const rhs_tuple = switch (rhs) {
                .tuple => |tuple| tuple,
                else => break :blk null,
            };
            if (lhs_tuple.items.len != rhs_tuple.items.len) break :blk null;
            const items = try arena.alloc(KnownValue, lhs_tuple.items.len);
            for (lhs_tuple.items, rhs_tuple.items, 0..) |lhs_item, rhs_item, index| {
                items[index] = (try joinKnownValuesInArena(program, arena, lhs_item, rhs_item)) orelse
                    (joinUnknownChild(program, lhs_item, rhs_item) orelse break :blk null);
            }
            break :blk KnownValue{ .tuple = .{
                .ty = lhs_tuple.ty,
                .items = items,
            } };
        },
        .nominal => |lhs_nominal| blk: {
            const rhs_nominal = switch (rhs) {
                .nominal => |nominal| nominal,
                else => break :blk null,
            };
            const backing = (try joinKnownValuesInArena(program, arena, lhs_nominal.backing.*, rhs_nominal.backing.*)) orelse break :blk null;
            const stored = try arena.create(KnownValue);
            stored.* = backing;
            break :blk KnownValue{ .nominal = .{
                .ty = lhs_nominal.ty,
                .backing = stored,
            } };
        },
        .callable => |lhs_callable| blk: {
            const rhs_callable = switch (rhs) {
                .callable => |callable| callable,
                else => break :blk null,
            };
            if (!callableTargetMatches(program, lhs_callable.fn_id, rhs_callable.fn_id) or
                lhs_callable.captures.len != rhs_callable.captures.len)
            {
                break :blk null;
            }
            const captures = try arena.alloc(KnownValue, lhs_callable.captures.len);
            for (lhs_callable.captures, rhs_callable.captures, 0..) |lhs_capture, rhs_capture, index| {
                captures[index] = (try joinKnownValuesInArena(program, arena, lhs_capture, rhs_capture)) orelse
                    (joinUnknownChild(program, lhs_capture, rhs_capture) orelse break :blk null);
            }
            break :blk KnownValue{ .callable = .{
                .ty = lhs_callable.ty,
                .fn_id = lhs_callable.fn_id,
                .captures = captures,
            } };
        },
        .finite_tags => null,
        .finite_callables => null,
    };
}

fn joinUnknownChild(program: *const Ast.Program, lhs: KnownValue, rhs: KnownValue) ?KnownValue {
    const lhs_ty = known_valueType(lhs);
    return if (sameType(program, lhs_ty, known_valueType(rhs)))
        KnownValue{ .any = lhs_ty }
    else
        null;
}

fn known_valuesStrictlyDescend(program: *const Ast.Program, active: []const KnownValue, next: []const KnownValue) bool {
    if (active.len != next.len) return false;
    var descended = false;
    for (active, next) |active_known_value, next_known_value| {
        if (known_valueEql(program, active_known_value, next_known_value)) continue;
        if (!known_valueContainsStrictSubknown_value(program, active_known_value, next_known_value)) return false;
        descended = true;
    }
    return descended;
}

fn known_valueContainsStrictSubknown_value(program: *const Ast.Program, container: KnownValue, needle: KnownValue) bool {
    return switch (container) {
        .any => false,
        .leaf => false,
        .tag => |tag| {
            for (tag.payloads) |payload| {
                if (known_valueEql(program, payload, needle) or known_valueContainsStrictSubknown_value(program, payload, needle)) return true;
            }
            return false;
        },
        .record => |record| {
            for (record.fields) |field| {
                if (known_valueEql(program, field.known_value, needle) or known_valueContainsStrictSubknown_value(program, field.known_value, needle)) return true;
            }
            return false;
        },
        .tuple => |tuple| {
            for (tuple.items) |item| {
                if (known_valueEql(program, item, needle) or known_valueContainsStrictSubknown_value(program, item, needle)) return true;
            }
            return false;
        },
        .nominal => |nominal| {
            return known_valueEql(program, nominal.backing.*, needle) or known_valueContainsStrictSubknown_value(program, nominal.backing.*, needle);
        },
        .callable => |callable| {
            for (callable.captures) |capture| {
                if (known_valueEql(program, capture, needle) or known_valueContainsStrictSubknown_value(program, capture, needle)) return true;
            }
            return false;
        },
        .finite_tags => |finite_tags| {
            for (finite_tags.alternatives) |alternative| {
                for (alternative.payloads) |payload| {
                    if (known_valueEql(program, payload, needle) or known_valueContainsStrictSubknown_value(program, payload, needle)) return true;
                }
            }
            return false;
        },
        .finite_callables => |finite_callables| {
            for (finite_callables.alternatives) |alternative| {
                for (alternative.captures) |capture| {
                    if (known_valueEql(program, capture, needle) or known_valueContainsStrictSubknown_value(program, capture, needle)) return true;
                }
            }
            return false;
        },
    };
}

fn knownValuesContainFiniteState(known_values: []const KnownValue) bool {
    for (known_values) |known_value| {
        if (knownValueContainsFiniteState(known_value)) return true;
    }
    return false;
}

fn knownValueContainsFiniteState(known_value: KnownValue) bool {
    return switch (known_value) {
        .any,
        .leaf,
        => false,
        .tag => |tag| blk: {
            for (tag.payloads) |payload| {
                if (knownValueContainsFiniteState(payload)) break :blk true;
            }
            break :blk false;
        },
        .record => |record| blk: {
            for (record.fields) |field| {
                if (knownValueContainsFiniteState(field.known_value)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |tuple| blk: {
            for (tuple.items) |item| {
                if (knownValueContainsFiniteState(item)) break :blk true;
            }
            break :blk false;
        },
        .nominal => |nominal| knownValueContainsFiniteState(nominal.backing.*),
        .callable => |callable| blk: {
            for (callable.captures) |capture| {
                if (knownValueContainsFiniteState(capture)) break :blk true;
            }
            break :blk false;
        },
        .finite_tags,
        .finite_callables,
        => true,
    };
}

fn demandedKnownValueFromDemand(
    arena: Allocator,
    known_value: KnownValue,
    demand: ValueDemand,
) Allocator.Error!?DemandedKnownValue {
    return switch (demand) {
        .none => null,
        .materialize => try materializedDemandedKnownValue(arena, known_value),
        .record => |field_demands| blk: {
            if (known_value == .nominal) {
                const demanded_backing = (try demandedKnownValueFromDemand(arena, known_value.nominal.backing.*, demand)) orelse break :blk null;
                const backing = try arena.create(DemandedKnownValue);
                backing.* = demanded_backing;
                break :blk DemandedKnownValue{ .nominal = .{
                    .ty = known_value.nominal.ty,
                    .backing = backing,
                } };
            }
            const record = switch (known_value) {
                .record => |record| record,
                else => break :blk null,
            };

            var fields = std.ArrayList(DemandedKnownField).empty;
            defer fields.deinit(arena);
            for (record.fields) |field| {
                const field_demand = fieldDemandByName(field_demands, field.name) orelse continue;
                const demanded_field = (try demandedKnownValueFromDemand(arena, field.known_value, field_demand.demand.*)) orelse continue;
                try fields.append(arena, .{
                    .name = field.name,
                    .known_value = demanded_field,
                });
            }
            if (fields.items.len == 0) break :blk null;
            break :blk DemandedKnownValue{ .record = .{
                .ty = record.ty,
                .fields = try arena.dupe(DemandedKnownField, fields.items),
            } };
        },
        .tuple => |item_demands| blk: {
            if (known_value == .nominal) {
                const demanded_backing = (try demandedKnownValueFromDemand(arena, known_value.nominal.backing.*, demand)) orelse break :blk null;
                const backing = try arena.create(DemandedKnownValue);
                backing.* = demanded_backing;
                break :blk DemandedKnownValue{ .nominal = .{
                    .ty = known_value.nominal.ty,
                    .backing = backing,
                } };
            }
            const tuple = switch (known_value) {
                .tuple => |tuple| tuple,
                else => break :blk null,
            };

            var items = std.ArrayList(DemandedKnownIndexedValue).empty;
            defer items.deinit(arena);
            for (tuple.items, 0..) |item, index| {
                const item_demand = itemDemandByIndex(item_demands, @intCast(index)) orelse continue;
                const demanded_item = (try demandedKnownValueFromDemand(arena, item, item_demand.demand.*)) orelse continue;
                try items.append(arena, .{
                    .index = @intCast(index),
                    .known_value = demanded_item,
                });
            }
            if (items.items.len == 0) break :blk null;
            break :blk DemandedKnownValue{ .tuple = .{
                .ty = tuple.ty,
                .items = try arena.dupe(DemandedKnownIndexedValue, items.items),
            } };
        },
        .nominal => |backing_demand| blk: {
            const nominal = switch (known_value) {
                .nominal => |nominal| nominal,
                else => break :blk null,
            };
            const demanded_backing = (try demandedKnownValueFromDemand(arena, nominal.backing.*, backing_demand.*)) orelse break :blk null;
            const backing = try arena.create(DemandedKnownValue);
            backing.* = demanded_backing;
            break :blk DemandedKnownValue{ .nominal = .{
                .ty = nominal.ty,
                .backing = backing,
            } };
        },
        .callable => |callable_demand| blk: {
            if (known_value == .nominal) {
                const demanded_backing = (try demandedKnownValueFromDemand(arena, known_value.nominal.backing.*, demand)) orelse break :blk null;
                const backing = try arena.create(DemandedKnownValue);
                backing.* = demanded_backing;
                break :blk DemandedKnownValue{ .nominal = .{
                    .ty = known_value.nominal.ty,
                    .backing = backing,
                } };
            }

            switch (known_value) {
                .callable => |callable| {
                    const captures = try demandedKnownCapturesFromDemand(arena, callable.captures, callable_demand);
                    break :blk DemandedKnownValue{ .callable = .{
                        .ty = callable.ty,
                        .fn_id = callable.fn_id,
                        .captures = captures,
                    } };
                },
                .finite_callables => |finite_callables| {
                    const alternatives = try arena.alloc(DemandedKnownCallable, finite_callables.alternatives.len);
                    for (finite_callables.alternatives, alternatives) |alternative, *out| {
                        const captures = try demandedKnownCapturesFromDemand(arena, alternative.captures, callable_demand);
                        out.* = .{
                            .ty = alternative.ty,
                            .fn_id = alternative.fn_id,
                            .captures = captures,
                        };
                    }
                    break :blk DemandedKnownValue{ .finite_callables = .{
                        .ty = finite_callables.ty,
                        .alternatives = alternatives,
                    } };
                },
                else => break :blk null,
            }
        },
    };
}

fn materializedDemandedKnownValue(arena: Allocator, known_value: KnownValue) Allocator.Error!DemandedKnownValue {
    return switch (known_value) {
        .any => |ty| .{ .any = ty },
        .leaf => |ty| .{ .leaf = ty },
        .tag => |tag| .{ .tag = .{
            .ty = tag.ty,
            .name = tag.name,
            .payloads = try materializedDemandedKnownIndexedValues(arena, tag.payloads),
        } },
        .record => |record| blk: {
            const fields = try arena.alloc(DemandedKnownField, record.fields.len);
            for (record.fields, fields) |field, *out| {
                out.* = .{
                    .name = field.name,
                    .known_value = try materializedDemandedKnownValue(arena, field.known_value),
                };
            }
            break :blk DemandedKnownValue{ .record = .{
                .ty = record.ty,
                .fields = fields,
            } };
        },
        .tuple => |tuple| .{ .tuple = .{
            .ty = tuple.ty,
            .items = try materializedDemandedKnownIndexedValues(arena, tuple.items),
        } },
        .nominal => |nominal| blk: {
            const backing = try arena.create(DemandedKnownValue);
            backing.* = try materializedDemandedKnownValue(arena, nominal.backing.*);
            break :blk DemandedKnownValue{ .nominal = .{
                .ty = nominal.ty,
                .backing = backing,
            } };
        },
        .callable => |callable| .{ .callable = .{
            .ty = callable.ty,
            .fn_id = callable.fn_id,
            .captures = try materializedDemandedKnownIndexedValues(arena, callable.captures),
        } },
        .finite_tags => |finite_tags| blk: {
            const alternatives = try arena.alloc(DemandedKnownTag, finite_tags.alternatives.len);
            for (finite_tags.alternatives, alternatives) |alternative, *out| {
                out.* = .{
                    .ty = alternative.ty,
                    .name = alternative.name,
                    .payloads = try materializedDemandedKnownIndexedValues(arena, alternative.payloads),
                };
            }
            break :blk DemandedKnownValue{ .finite_tags = .{
                .ty = finite_tags.ty,
                .alternatives = alternatives,
            } };
        },
        .finite_callables => |finite_callables| blk: {
            const alternatives = try arena.alloc(DemandedKnownCallable, finite_callables.alternatives.len);
            for (finite_callables.alternatives, alternatives) |alternative, *out| {
                out.* = .{
                    .ty = alternative.ty,
                    .fn_id = alternative.fn_id,
                    .captures = try materializedDemandedKnownIndexedValues(arena, alternative.captures),
                };
            }
            break :blk DemandedKnownValue{ .finite_callables = .{
                .ty = finite_callables.ty,
                .alternatives = alternatives,
            } };
        },
    };
}

fn materializedDemandedKnownIndexedValues(
    arena: Allocator,
    values: []const KnownValue,
) Allocator.Error![]const DemandedKnownIndexedValue {
    const indexed = try arena.alloc(DemandedKnownIndexedValue, values.len);
    for (values, indexed, 0..) |known_value, *out, index| {
        out.* = .{
            .index = @intCast(index),
            .known_value = try materializedDemandedKnownValue(arena, known_value),
        };
    }
    return indexed;
}

fn demandedKnownCapturesFromDemand(
    arena: Allocator,
    captures: []const KnownValue,
    demand: CallableDemand,
) Allocator.Error![]const DemandedKnownIndexedValue {
    var demanded = std.ArrayList(DemandedKnownIndexedValue).empty;
    defer demanded.deinit(arena);
    for (captures, 0..) |capture, index| {
        if (index >= demand.captures.len) break;
        const capture_demand = demand.captures[index];
        const demanded_capture = (try demandedKnownValueFromDemand(arena, capture, capture_demand)) orelse continue;
        try demanded.append(arena, .{
            .index = @intCast(index),
            .known_value = demanded_capture,
        });
    }
    return try arena.dupe(DemandedKnownIndexedValue, demanded.items);
}

fn known_valueEql(program: *const Ast.Program, lhs: KnownValue, rhs: KnownValue) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .any => |lhs_ty| sameType(program, lhs_ty, rhs.any),
        .leaf => |lhs_ty| sameType(program, lhs_ty, rhs.leaf),
        .tag => |lhs_tag| blk: {
            const rhs_tag = rhs.tag;
            if (!sameType(program, lhs_tag.ty, rhs_tag.ty) or lhs_tag.name != rhs_tag.name or lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk false;
            for (lhs_tag.payloads, rhs_tag.payloads) |lhs_payload, rhs_payload| {
                if (!known_valueEql(program, lhs_payload, rhs_payload)) break :blk false;
            }
            break :blk true;
        },
        .record => |lhs_record| blk: {
            const rhs_record = rhs.record;
            if (!sameType(program, lhs_record.ty, rhs_record.ty) or lhs_record.fields.len != rhs_record.fields.len) break :blk false;
            for (lhs_record.fields, rhs_record.fields) |lhs_field, rhs_field| {
                if (lhs_field.name != rhs_field.name or !known_valueEql(program, lhs_field.known_value, rhs_field.known_value)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |lhs_tuple| blk: {
            const rhs_tuple = rhs.tuple;
            if (!sameType(program, lhs_tuple.ty, rhs_tuple.ty) or lhs_tuple.items.len != rhs_tuple.items.len) break :blk false;
            for (lhs_tuple.items, rhs_tuple.items) |lhs_item, rhs_item| {
                if (!known_valueEql(program, lhs_item, rhs_item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |lhs_nominal| {
            const rhs_nominal = rhs.nominal;
            return sameType(program, lhs_nominal.ty, rhs_nominal.ty) and known_valueEql(program, lhs_nominal.backing.*, rhs_nominal.backing.*);
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
                if (!known_valueEql(program, lhs_capture, rhs_capture)) break :blk false;
            }
            break :blk true;
        },
        .finite_tags => |lhs_finite| knownTagsEql(program, lhs_finite, rhs.finite_tags),
        .finite_callables => |lhs_finite| knownCallablesEql(program, lhs_finite, rhs.finite_callables),
    };
}

fn knownValuesMatchValues(program: *const Ast.Program, known_values: []const KnownValue, values: []const Value) bool {
    if (known_values.len != values.len) return false;
    for (known_values, values) |known_value, value| {
        if (!knownValueMatchesValue(program, known_value, value)) return false;
    }
    return true;
}

fn knownValueMatchesValue(program: *const Ast.Program, known_value: KnownValue, value: Value) bool {
    if (value == .expr_with_known_value) {
        if (known_value == .any) return sameType(program, known_value.any, valueType(program, value));
        if (known_value == .leaf) return sameType(program, known_value.leaf, valueType(program, value));
        if (!canReadFieldsFromExpr(program, value.expr_with_known_value.expr)) return false;
        return known_valueCanProjectFromExpr(known_value) and knownValueMatchesKnownValue(program, known_value, value.expr_with_known_value.known_value);
    }

    return switch (known_value) {
        .any => |ty| sameType(program, ty, valueType(program, value)),
        .leaf => |ty| sameType(program, ty, valueType(program, value)),
        .tag => |tag| blk: {
            const value_tag = switch (value) {
                .tag => |value_tag| value_tag,
                else => break :blk false,
            };
            if (!sameType(program, tag.ty, value_tag.ty) or tag.name != value_tag.name or tag.payloads.len != value_tag.payloads.len) break :blk false;
            for (tag.payloads, value_tag.payloads) |payload_known_value, payload_value| {
                if (!knownValueMatchesValue(program, payload_known_value, payload_value)) break :blk false;
            }
            break :blk true;
        },
        .record => |record| blk: {
            const value_record = switch (value) {
                .record => |value_record| value_record,
                else => break :blk false,
            };
            if (!sameType(program, record.ty, value_record.ty)) break :blk false;
            for (record.fields) |field_known_value| {
                const field_value = fieldFromRecord(value_record, field_known_value.name) orelse break :blk false;
                if (!knownValueMatchesValue(program, field_known_value.known_value, field_value)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |tuple| blk: {
            const value_tuple = switch (value) {
                .tuple => |value_tuple| value_tuple,
                else => break :blk false,
            };
            if (!sameType(program, tuple.ty, value_tuple.ty) or tuple.items.len != value_tuple.items.len) break :blk false;
            for (tuple.items, value_tuple.items) |item_known_value, item_value| {
                if (!knownValueMatchesValue(program, item_known_value, item_value)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |nominal| blk: {
            const value_nominal = switch (value) {
                .nominal => |value_nominal| value_nominal,
                else => break :blk false,
            };
            break :blk sameType(program, nominal.ty, value_nominal.ty) and knownValueMatchesValue(program, nominal.backing.*, value_nominal.backing.*);
        },
        .callable => |callable| blk: {
            const value_callable = switch (value) {
                .callable => |value_callable| value_callable,
                else => break :blk false,
            };
            if (!sameType(program, callable.ty, value_callable.ty) or
                !callableTargetMatches(program, callable.fn_id, value_callable.fn_id) or
                callable.captures.len != value_callable.captures.len)
            {
                break :blk false;
            }
            for (callable.captures, value_callable.captures) |capture_known_value, capture_value| {
                if (!knownValueMatchesValue(program, capture_known_value, capture_value)) break :blk false;
            }
            break :blk true;
        },
        .finite_tags => |finite_tags| blk: {
            switch (value) {
                .tag => |tag| break :blk finiteTagAlternativeIndex(program, finite_tags.alternatives, tag) != null,
                .finite_tags => |finite_value| break :blk knownTagsMatchesValue(program, finite_tags, finite_value),
                else => break :blk false,
            }
        },
        .finite_callables => |finite_callables| blk: {
            switch (value) {
                .callable => |callable| break :blk finiteCallableAlternativeIndex(program, finite_callables.alternatives, callable) != null,
                .finite_callables => |finite_value| break :blk knownCallablesMatchesValue(program, finite_callables, finite_value),
                else => break :blk false,
            }
        },
    };
}

fn known_valueCanProjectFromExpr(known_value: KnownValue) bool {
    return switch (known_value) {
        .any => true,
        .leaf => true,
        .record => |record| blk: {
            for (record.fields) |field| {
                if (!known_valueCanProjectFromExpr(field.known_value)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |tuple| blk: {
            for (tuple.items) |item| {
                if (!known_valueCanProjectFromExpr(item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |nominal| known_valueCanProjectFromExpr(nominal.backing.*),
        .tag,
        .callable,
        .finite_tags,
        .finite_callables,
        => false,
    };
}

fn knownValueMatchesKnownValue(program: *const Ast.Program, pattern: KnownValue, actual: KnownValue) bool {
    return switch (pattern) {
        .any => |ty| sameType(program, ty, known_valueType(actual)),
        .leaf => |ty| sameType(program, ty, known_valueType(actual)),
        .tag => |pattern_tag| blk: {
            const actual_tag = switch (actual) {
                .tag => |tag| tag,
                else => break :blk false,
            };
            if (!sameType(program, pattern_tag.ty, actual_tag.ty) or
                pattern_tag.name != actual_tag.name or
                pattern_tag.payloads.len != actual_tag.payloads.len)
            {
                break :blk false;
            }
            for (pattern_tag.payloads, actual_tag.payloads) |pattern_payload, actual_payload| {
                if (!knownValueMatchesKnownValue(program, pattern_payload, actual_payload)) break :blk false;
            }
            break :blk true;
        },
        .record => |pattern_record| blk: {
            const actual_record = switch (actual) {
                .record => |record| record,
                else => break :blk false,
            };
            if (!sameType(program, pattern_record.ty, actual_record.ty)) break :blk false;
            for (pattern_record.fields) |pattern_field| {
                const actual_field = fieldKnownValueFromKnownValue(actual, pattern_field.name) orelse break :blk false;
                if (!knownValueMatchesKnownValue(program, pattern_field.known_value, actual_field)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |pattern_tuple| blk: {
            const actual_tuple = switch (actual) {
                .tuple => |tuple| tuple,
                else => break :blk false,
            };
            if (!sameType(program, pattern_tuple.ty, actual_tuple.ty) or pattern_tuple.items.len != actual_tuple.items.len) break :blk false;
            for (pattern_tuple.items, actual_tuple.items) |pattern_item, actual_item| {
                if (!knownValueMatchesKnownValue(program, pattern_item, actual_item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |pattern_nominal| blk: {
            const actual_nominal = switch (actual) {
                .nominal => |nominal| nominal,
                else => break :blk false,
            };
            break :blk sameType(program, pattern_nominal.ty, actual_nominal.ty) and
                knownValueMatchesKnownValue(program, pattern_nominal.backing.*, actual_nominal.backing.*);
        },
        .callable => |pattern_callable| blk: {
            const actual_callable = switch (actual) {
                .callable => |callable| callable,
                else => break :blk false,
            };
            if (!sameType(program, pattern_callable.ty, actual_callable.ty) or
                !callableTargetMatches(program, pattern_callable.fn_id, actual_callable.fn_id) or
                pattern_callable.captures.len != actual_callable.captures.len)
            {
                break :blk false;
            }
            for (pattern_callable.captures, actual_callable.captures) |pattern_capture, actual_capture| {
                if (!knownValueMatchesKnownValue(program, pattern_capture, actual_capture)) break :blk false;
            }
            break :blk true;
        },
        .finite_tags => |pattern_finite| blk: {
            const actual_finite = switch (actual) {
                .finite_tags => |finite_tags| finite_tags,
                .tag => |tag| {
                    break :blk finiteKnownTagContainsKnownTag(program, pattern_finite, tag);
                },
                else => break :blk false,
            };
            break :blk knownTagsContainsKnownValue(program, pattern_finite, actual_finite);
        },
        .finite_callables => |pattern_finite| blk: {
            const actual_finite = switch (actual) {
                .finite_callables => |finite_callables| finite_callables,
                .callable => |callable| {
                    break :blk finiteKnownCallableContainsKnownCallable(program, pattern_finite, callable);
                },
                else => break :blk false,
            };
            break :blk knownCallablesContainsKnownValue(program, pattern_finite, actual_finite);
        },
    };
}

fn commonKnownTags(
    program: *const Ast.Program,
    arena: Allocator,
    lhs: KnownValue,
    rhs: KnownValue,
) Allocator.Error!?KnownValue {
    const ty = known_valueType(lhs);
    var alternatives = std.ArrayList(KnownTag).empty;
    defer alternatives.deinit(arena);

    if (!try appendKnownTagAlternatives(program, arena, &alternatives, lhs)) return null;
    if (!try appendKnownTagAlternatives(program, arena, &alternatives, rhs)) return null;
    if (alternatives.items.len == 0) return null;
    if (alternatives.items.len == 1) return KnownValue{ .tag = alternatives.items[0] };

    const stored = try arena.dupe(KnownTag, alternatives.items);
    return KnownValue{ .finite_tags = .{
        .ty = ty,
        .alternatives = stored,
    } };
}

fn appendKnownTagAlternatives(
    program: *const Ast.Program,
    arena: Allocator,
    out: *std.ArrayList(KnownTag),
    known_value: KnownValue,
) Allocator.Error!bool {
    switch (known_value) {
        .tag => |tag| return try appendTagAlternative(program, arena, out, tag),
        .finite_tags => |finite_tags| {
            for (finite_tags.alternatives) |alternative| {
                if (!try appendTagAlternative(program, arena, out, alternative)) return false;
            }
            return true;
        },
        else => return false,
    }
}

fn appendTagAlternative(
    program: *const Ast.Program,
    arena: Allocator,
    out: *std.ArrayList(KnownTag),
    candidate: KnownTag,
) Allocator.Error!bool {
    for (out.items, 0..) |existing, index| {
        if (existing.name != candidate.name) continue;
        if (!sameType(program, existing.ty, candidate.ty)) return false;
        if (existing.payloads.len != candidate.payloads.len) return false;

        const payloads = try arena.alloc(KnownValue, existing.payloads.len);
        for (existing.payloads, candidate.payloads, payloads) |lhs_payload, rhs_payload, *payload_out| {
            payload_out.* = (try joinKnownValuesInArena(program, arena, lhs_payload, rhs_payload)) orelse
                .{ .any = known_valueType(lhs_payload) };
        }
        out.items[index] = .{
            .ty = existing.ty,
            .name = existing.name,
            .payloads = payloads,
        };
        return true;
    }

    try out.append(arena, candidate);
    return true;
}

fn finiteTagAlternativeIndex(
    program: *const Ast.Program,
    alternatives: []const KnownTag,
    value: TagValue,
) ?usize {
    for (alternatives, 0..) |alternative, index| {
        if (!sameType(program, alternative.ty, value.ty)) continue;
        if (alternative.name != value.name or alternative.payloads.len != value.payloads.len) continue;
        for (alternative.payloads, value.payloads) |payload_known_value, payload_value| {
            if (!knownValueMatchesValue(program, payload_known_value, payload_value)) break;
        } else {
            return index;
        }
    }
    return null;
}

fn knownTagsMatchesValue(
    program: *const Ast.Program,
    known_value: KnownTags,
    value: FiniteTagsValue,
) bool {
    if (!sameType(program, known_value.ty, value.ty)) return false;
    if (known_value.alternatives.len != value.alternatives.len) return false;
    for (known_value.alternatives, value.alternatives) |known_value_alternative, value_alternative| {
        if (!sameType(program, known_value_alternative.ty, value_alternative.ty)) return false;
        if (known_value_alternative.name != value_alternative.name or known_value_alternative.payloads.len != value_alternative.payloads.len) return false;
        for (known_value_alternative.payloads, value_alternative.payloads) |payload_known_value, payload_value| {
            if (!knownValueMatchesValue(program, payload_known_value, payload_value)) return false;
        }
    }
    return true;
}

fn knownTagsEql(program: *const Ast.Program, lhs: KnownTags, rhs: KnownTags) bool {
    if (!sameType(program, lhs.ty, rhs.ty) or lhs.alternatives.len != rhs.alternatives.len) return false;
    for (lhs.alternatives) |lhs_alternative| {
        for (rhs.alternatives) |rhs_alternative| {
            if (knownTagEql(program, lhs_alternative, rhs_alternative)) break;
        } else {
            return false;
        }
    }
    return true;
}

fn knownTagEql(program: *const Ast.Program, lhs: KnownTag, rhs: KnownTag) bool {
    if (!sameType(program, lhs.ty, rhs.ty) or
        lhs.name != rhs.name or lhs.payloads.len != rhs.payloads.len)
    {
        return false;
    }
    for (lhs.payloads, rhs.payloads) |lhs_payload, rhs_payload| {
        if (!known_valueEql(program, lhs_payload, rhs_payload)) return false;
    }
    return true;
}

fn finiteKnownTagContainsKnownTag(program: *const Ast.Program, finite: KnownTags, tag: KnownTag) bool {
    for (finite.alternatives) |alternative| {
        if (!sameType(program, alternative.ty, tag.ty) or
            alternative.name != tag.name or
            alternative.payloads.len != tag.payloads.len)
        {
            continue;
        }
        for (alternative.payloads, tag.payloads) |pattern_payload, actual_payload| {
            if (!knownValueMatchesKnownValue(program, pattern_payload, actual_payload)) break;
        } else {
            return true;
        }
    }
    return false;
}

fn knownTagsContainsKnownValue(program: *const Ast.Program, pattern: KnownTags, actual: KnownTags) bool {
    if (!sameType(program, pattern.ty, actual.ty)) return false;
    for (actual.alternatives) |alternative| {
        if (!finiteKnownTagContainsKnownTag(program, pattern, alternative)) return false;
    }
    return true;
}

fn commonKnownCallables(
    program: *const Ast.Program,
    arena: Allocator,
    lhs: KnownValue,
    rhs: KnownValue,
) Allocator.Error!?KnownValue {
    const ty = known_valueType(lhs);
    var alternatives = std.ArrayList(KnownCallable).empty;
    defer alternatives.deinit(arena);

    if (!try appendKnownCallableAlternatives(program, arena, &alternatives, lhs)) return null;
    if (!try appendKnownCallableAlternatives(program, arena, &alternatives, rhs)) return null;
    if (alternatives.items.len == 0) return null;
    if (alternatives.items.len == 1) return KnownValue{ .callable = alternatives.items[0] };

    const stored = try arena.dupe(KnownCallable, alternatives.items);
    return KnownValue{ .finite_callables = .{
        .ty = ty,
        .alternatives = stored,
    } };
}

fn appendKnownCallableAlternatives(
    program: *const Ast.Program,
    arena: Allocator,
    out: *std.ArrayList(KnownCallable),
    known_value: KnownValue,
) Allocator.Error!bool {
    switch (known_value) {
        .callable => |callable| return try appendCallableAlternative(program, arena, out, callable),
        .finite_callables => |finite_callables| {
            for (finite_callables.alternatives) |alternative| {
                if (!try appendCallableAlternative(program, arena, out, alternative)) return false;
            }
            return true;
        },
        else => return false,
    }
}

fn appendCallableAlternative(
    program: *const Ast.Program,
    arena: Allocator,
    out: *std.ArrayList(KnownCallable),
    candidate: KnownCallable,
) Allocator.Error!bool {
    for (out.items, 0..) |existing, index| {
        if (!callableTargetMatches(program, existing.fn_id, candidate.fn_id)) continue;
        if (!sameType(program, existing.ty, candidate.ty)) return false;
        if (existing.captures.len != candidate.captures.len) return false;

        const captures = try arena.alloc(KnownValue, existing.captures.len);
        for (existing.captures, candidate.captures, captures) |lhs_capture, rhs_capture, *capture_out| {
            capture_out.* = (try joinKnownValuesInArena(program, arena, lhs_capture, rhs_capture)) orelse
                .{ .any = known_valueType(lhs_capture) };
        }
        out.items[index] = .{
            .ty = existing.ty,
            .fn_id = existing.fn_id,
            .captures = captures,
        };
        return true;
    }

    try out.append(arena, candidate);
    return true;
}

fn finiteCallableAlternativeIndex(
    program: *const Ast.Program,
    alternatives: []const KnownCallable,
    value: CallableValue,
) ?usize {
    for (alternatives, 0..) |alternative, index| {
        if (!sameType(program, alternative.ty, value.ty)) continue;
        if (!callableTargetMatches(program, alternative.fn_id, value.fn_id) or alternative.captures.len != value.captures.len) continue;
        for (alternative.captures, value.captures) |capture_known_value, capture_value| {
            if (!knownValueMatchesValue(program, capture_known_value, capture_value)) break;
        } else {
            return index;
        }
    }
    return null;
}

fn knownCallablesMatchesValue(
    program: *const Ast.Program,
    known_value: KnownCallables,
    value: FiniteCallablesValue,
) bool {
    if (!sameType(program, known_value.ty, value.ty)) return false;
    if (known_value.alternatives.len != value.alternatives.len) return false;
    for (known_value.alternatives, value.alternatives) |known_value_alternative, value_alternative| {
        if (!sameType(program, known_value_alternative.ty, value_alternative.ty)) return false;
        if (!callableTargetMatches(program, known_value_alternative.fn_id, value_alternative.fn_id) or
            known_value_alternative.captures.len != value_alternative.captures.len)
        {
            return false;
        }
        for (known_value_alternative.captures, value_alternative.captures) |capture_known_value, capture_value| {
            if (!knownValueMatchesValue(program, capture_known_value, capture_value)) return false;
        }
    }
    return true;
}

fn knownCallablesEql(program: *const Ast.Program, lhs: KnownCallables, rhs: KnownCallables) bool {
    if (!sameType(program, lhs.ty, rhs.ty) or lhs.alternatives.len != rhs.alternatives.len) return false;
    for (lhs.alternatives) |lhs_alternative| {
        for (rhs.alternatives) |rhs_alternative| {
            if (knownCallableEql(program, lhs_alternative, rhs_alternative)) break;
        } else {
            return false;
        }
    }
    return true;
}

fn knownCallableEql(program: *const Ast.Program, lhs: KnownCallable, rhs: KnownCallable) bool {
    if (!sameType(program, lhs.ty, rhs.ty) or
        !callableTargetMatches(program, lhs.fn_id, rhs.fn_id) or
        lhs.captures.len != rhs.captures.len)
    {
        return false;
    }
    for (lhs.captures, rhs.captures) |lhs_capture, rhs_capture| {
        if (!known_valueEql(program, lhs_capture, rhs_capture)) return false;
    }
    return true;
}

fn finiteKnownCallableContainsKnownCallable(program: *const Ast.Program, finite: KnownCallables, callable: KnownCallable) bool {
    for (finite.alternatives) |alternative| {
        if (!sameType(program, alternative.ty, callable.ty) or
            !callableTargetMatches(program, alternative.fn_id, callable.fn_id) or
            alternative.captures.len != callable.captures.len)
        {
            continue;
        }
        for (alternative.captures, callable.captures) |pattern_capture, actual_capture| {
            if (!knownValueMatchesKnownValue(program, pattern_capture, actual_capture)) break;
        } else {
            return true;
        }
    }
    return false;
}

fn knownCallablesContainsKnownValue(program: *const Ast.Program, pattern: KnownCallables, actual: KnownCallables) bool {
    if (!sameType(program, pattern.ty, actual.ty)) return false;
    for (actual.alternatives) |alternative| {
        if (!finiteKnownCallableContainsKnownCallable(program, pattern, alternative)) return false;
    }
    return true;
}

fn callableTargetMatches(program: *const Ast.Program, expected: Ast.FnId, actual: Ast.FnId) bool {
    if (expected == actual) return true;
    const expected_source = program.fns.items[@intFromEnum(expected)].source orelse return false;
    const actual_source = program.fns.items[@intFromEnum(actual)].source orelse return false;
    return Mono.fnTemplateIdentityEql(expected_source, actual_source);
}

fn fieldFromValue(value: Value, name: names.RecordFieldNameId) ?Value {
    const record = recordFromValue(value) orelse return null;
    return fieldFromRecord(record, name);
}

fn fieldFromRecord(record: RecordValue, name: names.RecordFieldNameId) ?Value {
    for (record.fields) |field| {
        if (field.name == name) return field.value;
    }
    return null;
}

fn recordPatField(fields: []const Ast.RecordDestruct, name: names.RecordFieldNameId) ?Ast.PatId {
    for (fields) |field| {
        if (field.name == name) return field.pattern;
    }
    return null;
}

fn itemFromValue(value: Value, index: u32) ?Value {
    const tuple = tupleFromValue(value) orelse return null;
    if (index >= tuple.items.len) return null;
    return tuple.items[index];
}

fn tagFromValue(value: Value) ?TagValue {
    return switch (value) {
        .tag => |tag| tag,
        .nominal => |nominal| tagFromValue(nominal.backing.*),
        else => null,
    };
}

fn knownIfConditionBoolTag(program: *const Ast.Program, value: Value) ?bool {
    const tag = tagFromValue(value) orelse return null;
    return boolTagValue(program, tag) orelse
        Common.invariant("known if condition Bool tag used a non-Bool tag label");
}

fn finiteBoolTagsValue(program: *const Ast.Program, value: Value) ?FiniteTagsValue {
    const finite_tags = switch (value) {
        .finite_tags => |finite_tags| finite_tags,
        else => return null,
    };
    for (finite_tags.alternatives) |alternative| {
        if (boolTagValue(program, alternative) == null) return null;
    }
    return finite_tags;
}

fn boolTagValue(program: *const Ast.Program, tag: TagValue) ?bool {
    if (tag.payloads.len != 0) Common.invariant("Bool tag had payloads");
    const tag_text = program.names.tagLabelText(tag.name);
    if (std.mem.eql(u8, tag_text, "True")) return true;
    if (std.mem.eql(u8, tag_text, "False")) return false;
    return null;
}

fn unsignedIntLiteral(value: anytype) can.CIR.IntValue {
    const widened: u128 = @intCast(value);
    return .{ .bytes = @bitCast(widened), .kind = .u128 };
}

fn recordFromValue(value: Value) ?RecordValue {
    return switch (value) {
        .record => |record| record,
        .nominal => |nominal| recordFromValue(nominal.backing.*),
        else => null,
    };
}

fn tupleFromValue(value: Value) ?TupleValue {
    return switch (value) {
        .tuple => |tuple| tuple,
        .nominal => |nominal| tupleFromValue(nominal.backing.*),
        else => null,
    };
}

test "value demand equality ignores projection order" {
    const materialize: ValueDemand = .materialize;
    const none: ValueDemand = .none;
    const field_a: names.RecordFieldNameId = @enumFromInt(1);
    const field_b: names.RecordFieldNameId = @enumFromInt(2);

    const record_lhs_fields = [_]FieldDemand{
        .{ .name = field_a, .demand = &materialize },
        .{ .name = field_b, .demand = &none },
    };
    const record_rhs_fields = [_]FieldDemand{
        .{ .name = field_b, .demand = &none },
        .{ .name = field_a, .demand = &materialize },
    };
    try std.testing.expect(valueDemandEql(
        .{ .record = &record_lhs_fields },
        .{ .record = &record_rhs_fields },
    ));

    const tuple_lhs_items = [_]ItemDemand{
        .{ .index = 0, .demand = &materialize },
        .{ .index = 3, .demand = &none },
    };
    const tuple_rhs_items = [_]ItemDemand{
        .{ .index = 3, .demand = &none },
        .{ .index = 0, .demand = &materialize },
    };
    try std.testing.expect(valueDemandEql(
        .{ .tuple = &tuple_lhs_items },
        .{ .tuple = &tuple_rhs_items },
    ));

    const record_missing_field = [_]FieldDemand{
        .{ .name = field_a, .demand = &materialize },
    };
    try std.testing.expect(!valueDemandEql(
        .{ .record = &record_lhs_fields },
        .{ .record = &record_missing_field },
    ));
}

test "demanded known value materialization preserves indexed tuple children" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const tuple_ty: Type.TypeId = @enumFromInt(10);
    const first_ty: Type.TypeId = @enumFromInt(11);
    const second_ty: Type.TypeId = @enumFromInt(12);
    const third_ty: Type.TypeId = @enumFromInt(13);
    const dense_items = [_]KnownValue{
        .{ .leaf = first_ty },
        .{ .any = second_ty },
        .{ .leaf = third_ty },
    };
    const known = KnownValue{ .tuple = .{
        .ty = tuple_ty,
        .items = &dense_items,
    } };

    const demanded = (try demandedKnownValueFromDemand(arena.allocator(), known, .materialize)) orelse
        return error.TestUnexpectedResult;

    try std.testing.expectEqual(tuple_ty, demanded.tuple.ty);
    try std.testing.expectEqual(@as(usize, 3), demanded.tuple.items.len);
    try std.testing.expectEqual(@as(u32, 0), demanded.tuple.items[0].index);
    try std.testing.expectEqual(@as(u32, 1), demanded.tuple.items[1].index);
    try std.testing.expectEqual(@as(u32, 2), demanded.tuple.items[2].index);
    try std.testing.expectEqual(second_ty, demanded.tuple.items[1].known_value.any);
}

test "demanded known value omits tuple siblings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const tuple_ty: Type.TypeId = @enumFromInt(20);
    const first_ty: Type.TypeId = @enumFromInt(21);
    const second_ty: Type.TypeId = @enumFromInt(22);
    const third_ty: Type.TypeId = @enumFromInt(23);
    const dense_items = [_]KnownValue{
        .{ .leaf = first_ty },
        .{ .any = second_ty },
        .{ .leaf = third_ty },
    };
    const known = KnownValue{ .tuple = .{
        .ty = tuple_ty,
        .items = &dense_items,
    } };
    const materialize: ValueDemand = .materialize;
    const item_demands = [_]ItemDemand{
        .{ .index = 1, .demand = &materialize },
    };

    const demanded = (try demandedKnownValueFromDemand(arena.allocator(), known, .{ .tuple = &item_demands })) orelse
        return error.TestUnexpectedResult;

    try std.testing.expectEqual(tuple_ty, demanded.tuple.ty);
    try std.testing.expectEqual(@as(usize, 1), demanded.tuple.items.len);
    try std.testing.expectEqual(@as(u32, 1), demanded.tuple.items[0].index);
    try std.testing.expectEqual(second_ty, demanded.tuple.items[0].known_value.any);
}

test "demanded known value distinguishes omitted capture from unknown carried capture" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const callable_ty: Type.TypeId = @enumFromInt(30);
    const first_ty: Type.TypeId = @enumFromInt(31);
    const second_ty: Type.TypeId = @enumFromInt(32);
    const third_ty: Type.TypeId = @enumFromInt(33);
    const captures = [_]KnownValue{
        .{ .leaf = first_ty },
        .{ .any = second_ty },
        .{ .leaf = third_ty },
    };
    const known = KnownValue{ .callable = .{
        .ty = callable_ty,
        .fn_id = @enumFromInt(0),
        .captures = &captures,
    } };
    const capture_demands = [_]ValueDemand{
        .none,
        .materialize,
        .none,
    };

    const demanded = (try demandedKnownValueFromDemand(arena.allocator(), known, .{
        .callable = .{ .captures = &capture_demands },
    })) orelse return error.TestUnexpectedResult;

    try std.testing.expectEqual(callable_ty, demanded.callable.ty);
    try std.testing.expectEqual(@as(usize, 1), demanded.callable.captures.len);
    try std.testing.expectEqual(@as(u32, 1), demanded.callable.captures[0].index);
    try std.testing.expectEqual(second_ty, demanded.callable.captures[0].known_value.any);
}

test "demanded known value preserves callable target with no demanded captures" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const callable_ty: Type.TypeId = @enumFromInt(50);
    const first_ty: Type.TypeId = @enumFromInt(51);
    const second_ty: Type.TypeId = @enumFromInt(52);
    const captures = [_]KnownValue{
        .{ .leaf = first_ty },
        .{ .any = second_ty },
    };
    const fn_id: Ast.FnId = @enumFromInt(3);
    const known = KnownValue{ .callable = .{
        .ty = callable_ty,
        .fn_id = fn_id,
        .captures = &captures,
    } };
    const capture_demands = [_]ValueDemand{
        .none,
        .none,
    };

    const demanded = (try demandedKnownValueFromDemand(arena.allocator(), known, .{
        .callable = .{ .captures = &capture_demands },
    })) orelse return error.TestUnexpectedResult;

    try std.testing.expectEqual(callable_ty, demanded.callable.ty);
    try std.testing.expectEqual(fn_id, demanded.callable.fn_id);
    try std.testing.expectEqual(@as(usize, 0), demanded.callable.captures.len);
}

test "demanded known value preserves finite callable alternatives with no demanded captures" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const callable_ty: Type.TypeId = @enumFromInt(60);
    const capture_ty: Type.TypeId = @enumFromInt(61);
    const first_fn: Ast.FnId = @enumFromInt(5);
    const second_fn: Ast.FnId = @enumFromInt(6);
    const first_captures = [_]KnownValue{
        .{ .leaf = capture_ty },
    };
    const second_captures = [_]KnownValue{
        .{ .any = capture_ty },
    };
    const alternatives = [_]KnownCallable{
        .{
            .ty = callable_ty,
            .fn_id = first_fn,
            .captures = &first_captures,
        },
        .{
            .ty = callable_ty,
            .fn_id = second_fn,
            .captures = &second_captures,
        },
    };
    const known = KnownValue{ .finite_callables = .{
        .ty = callable_ty,
        .alternatives = &alternatives,
    } };
    const capture_demands = [_]ValueDemand{
        .none,
    };

    const demanded = (try demandedKnownValueFromDemand(arena.allocator(), known, .{
        .callable = .{ .captures = &capture_demands },
    })) orelse return error.TestUnexpectedResult;

    try std.testing.expectEqual(callable_ty, demanded.finite_callables.ty);
    try std.testing.expectEqual(@as(usize, 2), demanded.finite_callables.alternatives.len);
    try std.testing.expectEqual(first_fn, demanded.finite_callables.alternatives[0].fn_id);
    try std.testing.expectEqual(second_fn, demanded.finite_callables.alternatives[1].fn_id);
    try std.testing.expectEqual(@as(usize, 0), demanded.finite_callables.alternatives[0].captures.len);
    try std.testing.expectEqual(@as(usize, 0), demanded.finite_callables.alternatives[1].captures.len);
}

test "demanded known value omits unused record fields" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const record_ty: Type.TypeId = @enumFromInt(40);
    const kept_ty: Type.TypeId = @enumFromInt(41);
    const omitted_ty: Type.TypeId = @enumFromInt(42);
    const kept_field: names.RecordFieldNameId = @enumFromInt(1);
    const omitted_field: names.RecordFieldNameId = @enumFromInt(2);
    const fields = [_]KnownField{
        .{ .name = kept_field, .known_value = .{ .leaf = kept_ty } },
        .{ .name = omitted_field, .known_value = .{ .leaf = omitted_ty } },
    };
    const known = KnownValue{ .record = .{
        .ty = record_ty,
        .fields = &fields,
    } };
    const materialize: ValueDemand = .materialize;
    const field_demands = [_]FieldDemand{
        .{ .name = kept_field, .demand = &materialize },
    };

    const demanded = (try demandedKnownValueFromDemand(arena.allocator(), known, .{ .record = &field_demands })) orelse
        return error.TestUnexpectedResult;

    try std.testing.expectEqual(record_ty, demanded.record.ty);
    try std.testing.expectEqual(@as(usize, 1), demanded.record.fields.len);
    try std.testing.expectEqual(kept_field, demanded.record.fields[0].name);
    try std.testing.expectEqual(kept_ty, demanded.record.fields[0].known_value.leaf);
}

test "call-pattern specialization declarations are referenced" {
    std.testing.refAllDecls(@This());
}
