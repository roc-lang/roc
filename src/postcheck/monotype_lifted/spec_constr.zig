//! Make calls cheaper when they pass values with known facts to code that
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
//! the same fact as this Roc code. The range is wrapped in a stream record; map
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
//! In that inlined form, the loop state `$rest` has a known constructor fact:
//! it is a `Stream` record whose `step!` field is the lifted function created by
//! `Stream.map`, with captures for the source step thunk and the mapping
//! function. Each `One` or `Skip` branch constructs the same mapped stream fact
//! for the next iteration. Without this pass, the compiler lowers that as a loop
//! over a single stream value, repacking stream fields and rebuilding the step
//! closure before immediately reading them again.
//!
//! This pass specializes the collect worker for the known stream fact. Written
//! in pure Roc terms, the optimized fact is:
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
//!    known-fact arguments reach a callee that reads them. Recording a pattern
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
const Mono = @import("../monotype/ast.zig");
const Type = @import("../monotype/type.zig");
const Solved = @import("../lambda_solved/ast.zig");
const SolvedType = @import("../lambda_solved/type.zig");
const check = @import("check");
const names = @import("check").CheckedNames;

const Allocator = std.mem.Allocator;

/// Specialize recursive direct calls whose arguments are known constructor facts.
pub fn run(allocator: Allocator, program: *Ast.Program) Common.LowerError!void {
    var pass = try Pass.init(allocator, program, null);
    defer pass.deinit();
    try pass.run();
}

pub fn runWithSolved(allocator: Allocator, solved: *Solved.Program) Common.LowerError!void {
    var pass = try Pass.init(allocator, &solved.lifted, solved);
    defer pass.deinit();
    try pass.run();
}

const ValueFact = union(enum) {
    any: Type.TypeId,
    leaf: Type.TypeId,
    tag: TagFact,
    record: RecordFact,
    tuple: TupleFact,
    nominal: NominalFact,
    callable: CallableFact,
};

const TagFact = struct {
    ty: Type.TypeId,
    name: names.TagNameId,
    payloads: []const ValueFact,
};

const FieldFact = struct {
    name: names.RecordFieldNameId,
    fact: ValueFact,
};

const RecordFact = struct {
    ty: Type.TypeId,
    fields: []const FieldFact,
};

const TupleFact = struct {
    ty: Type.TypeId,
    items: []const ValueFact,
};

const NominalFact = struct {
    ty: Type.TypeId,
    backing: *const ValueFact,
};

const CallableFact = struct {
    ty: Type.TypeId,
    fn_id: Ast.FnId,
    captures: []const ValueFact,
};

const KnownMatchMode = enum {
    strict,
    speculative,
};

const Value = union(enum) {
    expr: Ast.ExprId,
    expr_with_known_fact: ExprWithKnownFactValue,
    let_: LetValue,
    if_: IfValue,
    tag: TagValue,
    record: RecordValue,
    tuple: TupleValue,
    nominal: NominalValue,
    callable: CallableValue,
};

const ExprWithKnownFactValue = struct {
    expr: Ast.ExprId,
    fact: ValueFact,
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

const CallPattern = struct {
    args: []const ValueFact,
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

const WorkerJob = struct {
    source_fn: Ast.FnId,
    spec_index: usize,
};

const BindingTarget = union(enum) {
    local: Ast.LocalId,
    binder: check.CheckedModule.PatternBinderId,
};

const BindingChange = struct {
    key: BindingTarget,
    previous: ?Value,
};

const PendingLet = struct {
    local: Ast.LocalId,
    ty: Type.TypeId,
    value: Ast.ExprId,
    fact: ?ValueFact = null,
};

const BlockTail = struct {
    statements: []const Ast.StmtId,
    final_expr: Ast.ExprId,
};

const LoopPattern = struct {
    values: []const ValueFact,
    refinements: []?ValueFact,
};

const ActiveCallable = struct {
    source: Ast.FnId,
    specialized: Ast.FnId,
};

const ActiveInline = struct {
    fn_id: Ast.FnId,
    args: ?[]const ValueFact = null,
};

const Pass = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    program: *Ast.Program,
    solved: ?*const Solved.Program,
    plans: []FnPlan,
    worker_worklist: std.ArrayList(WorkerJob),
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
            plan.* = .{
                .used_args = used_args,
                .specs = .empty,
            };
        }

        return .{
            .allocator = allocator,
            .arena = arena,
            .program = program,
            .solved = solved,
            .plans = plans,
            .worker_worklist = .empty,
            .symbols = .{ .next = program.next_symbol },
        };
    }

    fn deinit(self: *Pass) void {
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
        if (member_items.len != 1) return null;
        return member_items[0];
    }

    fn fnWithSymbol(self: *const Pass, symbol: Common.Symbol) ?Ast.FnId {
        for (self.program.fns.items, 0..) |fn_, index| {
            if (fn_.symbol == symbol) return @enumFromInt(@as(u32, @intCast(index)));
        }
        return null;
    }

    fn run(self: *Pass) Common.LowerError!void {
        const original_fn_count = self.plans.len;
        const original_bodies = try self.captureOriginalBodies(original_fn_count);
        defer self.allocator.free(original_bodies);

        try self.collectArgUses(original_fn_count);
        try self.rewriteBaseBodies(original_bodies);
        try self.createSpecializations(original_bodies);

        self.program.next_symbol = self.symbols.next;
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
                    for (args, callee_uses) |arg, callee_uses_arg| {
                        if (callee_uses_arg) self.markArgUseIfLocal(fn_id, arg, changed);
                    }
                }
            },
            .low_level => |call| {
                for (self.program.exprSpan(call.args)) |arg| try self.markArgUsesInExpr(fn_id, arg, changed);
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
            .break_ => |maybe| if (maybe) |value| try self.markArgUsesInExpr(fn_id, value, changed),
            .continue_ => |continue_| for (self.program.exprSpan(continue_.values)) |value| try self.markArgUsesInExpr(fn_id, value, changed),
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

    fn markArgUseIfLocal(self: *Pass, fn_id: Ast.FnId, expr_id: Ast.ExprId, changed: *bool) void {
        const local = localExpr(self.program, expr_id) orelse return;
        const args = self.program.typedLocalSpan(self.program.fns.items[@intFromEnum(fn_id)].args);
        for (args, 0..) |arg, index| {
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

    fn ensureCallPatternForValues(self: *Pass, fn_id: Ast.FnId, values: []const Value) Common.LowerError!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.plans.len) return;

        const fn_args = self.program.typedLocalSpan(self.program.fns.items[raw].args);
        if (values.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const facts = try self.arena.allocator().alloc(ValueFact, values.len);
        var has_constructor = false;
        for (values, 0..) |value, index| {
            if (self.plans[raw].used_args[index]) {
                if (try self.factFromValue(value)) |fact| {
                    facts[index] = fact;
                    has_constructor = true;
                    continue;
                }
            }
            facts[index] = .{ .any = valueType(self.program, value) };
        }
        if (!has_constructor) return;

        const pattern: CallPattern = .{ .args = facts };
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

    fn constructorFact(self: *Pass, expr_id: Ast.ExprId) Allocator.Error!?ValueFact {
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
            => ValueFact{ .leaf = expr.ty },
            .tag => |tag| blk: {
                const payloads = self.program.exprSpan(tag.payloads);
                const facts = try self.arena.allocator().alloc(ValueFact, payloads.len);
                for (payloads, 0..) |payload, index| {
                    facts[index] = (try self.constructorFact(payload)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(payload)].ty };
                }
                break :blk ValueFact{ .tag = .{
                    .ty = expr.ty,
                    .name = tag.name,
                    .payloads = facts,
                } };
            },
            .record => |fields_span| blk: {
                const fields = self.program.fieldExprSpan(fields_span);
                const facts = try self.arena.allocator().alloc(FieldFact, fields.len);
                for (fields, 0..) |field, index| {
                    facts[index] = .{
                        .name = field.name,
                        .fact = (try self.constructorFact(field.value)) orelse
                            .{ .any = self.program.exprs.items[@intFromEnum(field.value)].ty },
                    };
                }
                break :blk ValueFact{ .record = .{
                    .ty = expr.ty,
                    .fields = facts,
                } };
            },
            .tuple => |items_span| blk: {
                const items = self.program.exprSpan(items_span);
                const facts = try self.arena.allocator().alloc(ValueFact, items.len);
                for (items, 0..) |item, index| {
                    facts[index] = (try self.constructorFact(item)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(item)].ty };
                }
                break :blk ValueFact{ .tuple = .{
                    .ty = expr.ty,
                    .items = facts,
                } };
            },
            .nominal => |backing| blk: {
                const backing_fact = (try self.constructorFact(backing)) orelse break :blk null;
                const stored = try self.arena.allocator().create(ValueFact);
                stored.* = backing_fact;
                break :blk ValueFact{ .nominal = .{
                    .ty = expr.ty,
                    .backing = stored,
                } };
            },
            .fn_ref => |fn_id| blk: {
                const fn_ = self.program.fns.items[@intFromEnum(fn_id)];
                const captures = self.program.typedLocalSpan(fn_.captures);
                const capture_facts = try self.arena.allocator().alloc(ValueFact, captures.len);
                for (captures, 0..) |capture, index| {
                    capture_facts[index] = .{ .any = capture.ty };
                }
                break :blk ValueFact{ .callable = .{
                    .ty = expr.ty,
                    .fn_id = fn_id,
                    .captures = capture_facts,
                } };
            },
            else => null,
        };
    }

    fn factFromValue(self: *Pass, value: Value) Allocator.Error!?ValueFact {
        return switch (value) {
            .expr => |expr| try self.constructorFact(expr),
            .expr_with_known_fact => |known_fact_expr| known_fact_expr.fact,
            .let_ => |let_value| try self.factFromValue(let_value.body.*),
            .if_ => |if_value| blk: {
                var joined: ?ValueFact = null;
                for (if_value.branches) |branch| {
                    const branch_fact = (try self.factFromValue(branch.body)) orelse break :blk null;
                    joined = if (joined) |existing|
                        (try joinFactsInArena(self.program, self.arena.allocator(), existing, branch_fact)) orelse break :blk null
                    else
                        branch_fact;
                }
                const final_fact = (try self.factFromValue(if_value.final_else.*)) orelse break :blk null;
                break :blk if (joined) |existing|
                    (try joinFactsInArena(self.program, self.arena.allocator(), existing, final_fact)) orelse null
                else
                    final_fact;
            },
            .tag => |tag| blk: {
                const payloads = try self.arena.allocator().alloc(ValueFact, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = (try self.factFromValue(payload)) orelse
                        .{ .any = valueType(self.program, payload) };
                }
                break :blk ValueFact{ .tag = .{
                    .ty = tag.ty,
                    .name = tag.name,
                    .payloads = payloads,
                } };
            },
            .record => |record| blk: {
                const fields = try self.arena.allocator().alloc(FieldFact, record.fields.len);
                for (record.fields, 0..) |field, index| {
                    fields[index] = .{
                        .name = field.name,
                        .fact = (try self.factFromValue(field.value)) orelse
                            .{ .any = valueType(self.program, field.value) },
                    };
                }
                break :blk ValueFact{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.arena.allocator().alloc(ValueFact, tuple.items.len);
                for (tuple.items, 0..) |item, index| {
                    items[index] = (try self.factFromValue(item)) orelse
                        .{ .any = valueType(self.program, item) };
                }
                break :blk ValueFact{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing_fact = (try self.factFromValue(nominal.backing.*)) orelse break :blk null;
                const stored = try self.arena.allocator().create(ValueFact);
                stored.* = backing_fact;
                break :blk ValueFact{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } };
            },
            .callable => |callable| blk: {
                const captures = try self.arena.allocator().alloc(ValueFact, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = (try self.factFromValue(capture)) orelse
                        .{ .any = valueType(self.program, capture) };
                }
                break :blk ValueFact{ .callable = .{
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
    /// Same-binder substitutions are valid only inside the function body that
    /// established them. Inline boundaries clear this map before binding callee
    /// locals because checked binder ids are not global across lifted modules.
    binder_subst: std.AutoHashMap(check.CheckedModule.PatternBinderId, Value),
    changes: std.ArrayList(BindingChange),
    inline_stack: std.ArrayList(ActiveInline),
    callable_stack: std.ArrayList(ActiveCallable),
    loop_stack: std.ArrayList(LoopPattern),
    inline_direct_calls: bool,
    inline_direct_requires_known_arg: bool,
    record_call_patterns: bool,
    current_loc: SourceLoc,
    current_region: Region,

    fn init(pass: *Pass, source_fn: Ast.FnId, pattern: CallPattern) Cloner {
        return .{
            .pass = pass,
            .source_fn = source_fn,
            .pattern = pattern,
            .subst = std.AutoHashMap(Ast.LocalId, Value).init(pass.allocator),
            .binder_subst = std.AutoHashMap(check.CheckedModule.PatternBinderId, Value).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .callable_stack = .empty,
            .loop_stack = .empty,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = true,
            .record_call_patterns = true,
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
            .binder_subst = std.AutoHashMap(check.CheckedModule.PatternBinderId, Value).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .callable_stack = .empty,
            .loop_stack = .empty,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = false,
            .record_call_patterns = true,
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
        };
    }

    fn initForBaseBody(pass: *Pass, source_fn: Ast.FnId) Cloner {
        var cloner = Cloner.initForBaseClone(pass);
        cloner.source_fn = source_fn;
        cloner.inline_direct_requires_known_arg = true;
        return cloner;
    }

    fn deinit(self: *Cloner) void {
        self.inline_stack.deinit(self.pass.allocator);
        self.callable_stack.deinit(self.pass.allocator);
        self.loop_stack.deinit(self.pass.allocator);
        self.changes.deinit(self.pass.allocator);
        self.binder_subst.deinit();
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

        for (source_args, self.pattern.args) |source_arg, fact| {
            const value = try self.valueFromFactArgs(fact, &args);
            try self.putSubst(source_arg.local, value);
        }

        return try self.pass.program.addTypedLocalSpan(args.items);
    }

    fn valueFromFactArgs(self: *Cloner, fact: ValueFact, args: *std.ArrayList(Ast.TypedLocal)) Allocator.Error!Value {
        switch (fact) {
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
                    payloads[index] = try self.valueFromFactArgs(payload, args);
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
                        .value = try self.valueFromFactArgs(field.fact, args),
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
                    items[index] = try self.valueFromFactArgs(item, args);
                }
                return .{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| {
                const backing = try self.pass.arena.allocator().create(Value);
                backing.* = try self.valueFromFactArgs(nominal.backing.*, args);
                return .{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = backing,
                } };
            },
            .callable => |callable| {
                const captures = try self.pass.arena.allocator().alloc(Value, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = try self.valueFromFactArgs(capture, args);
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
                if (self.pass.program.locals.items[@intFromEnum(local)].binder) |binder| {
                    if (self.binder_subst.get(binder)) |value| return value;
                }
                if (try self.solvedSingleCallable(expr_id)) |callable| return callable;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } }) };
            },
            .fn_ref => |fn_id| return try self.callableValue(expr.ty, fn_id),
            .tag => |tag| {
                const payload_exprs = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(tag.payloads));
                defer self.pass.allocator.free(payload_exprs);
                const payloads = try self.pass.arena.allocator().alloc(Value, payload_exprs.len);
                for (payload_exprs, 0..) |payload, index| {
                    payloads[index] = try self.cloneExprValueDemandingFact(payload);
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
                        .value = try self.cloneExprValueDemandingFact(field.value),
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
                    items[index] = try self.cloneExprValueDemandingFact(item);
                }
                return .{ .tuple = .{
                    .ty = expr.ty,
                    .items = items,
                } };
            },
            .nominal => |backing| {
                const backing_value = try self.cloneExprValueDemandingFact(backing);
                return .{ .nominal = .{
                    .ty = expr.ty,
                    .backing = try self.copyValue(backing_value),
                } };
            },
            .let_ => |let_| return try self.cloneLetValue(let_),
            .field_access => |field| {
                const receiver = try self.cloneExprValueDemandingFact(field.receiver);
                if (try self.fieldFromKnownValue(receiver, field.field)) |value| return value;
                if (try self.solvedSingleCallable(expr_id)) |callable| return callable;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .field_access = .{
                    .receiver = try self.materialize(receiver),
                    .field = field.field,
                } } }) };
            },
            .tuple_access => |access| {
                const receiver = try self.cloneExprValueDemandingFact(access.tuple);
                if (try self.itemFromKnownValue(receiver, access.elem_index)) |value| return value;
                if (try self.solvedSingleCallable(expr_id)) |callable| return callable;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = try self.materialize(receiver),
                    .elem_index = access.elem_index,
                } } }) };
            },
            .match_ => |match| {
                const scrutinee = try self.cloneExprValueDemandingFact(match.scrutinee);
                if (try self.simplifyKnownMatchValue(expr.ty, scrutinee, match.branches)) |value| return value;
                const scrutinee_expr = try self.materialize(scrutinee);
                if (try self.cloneCaseOfCaseValue(expr.ty, scrutinee_expr, match.branches)) |value| return value;
                return try self.cloneMatchJoinedValue(expr.ty, scrutinee_expr, match);
            },
            .if_ => |if_| return try self.cloneIfValue(expr.ty, if_),
            .block => |block| return try self.cloneBlockValue(expr.ty, block),
            .call_value => |call| {
                const callee = try self.cloneExprValueDemandingFact(call.callee);
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
                const has_known_fact_arg = try self.directCallHasKnownFactArg(call.args);
                if (self.inline_direct_requires_known_arg and !has_known_fact_arg) {
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

    fn cloneExprValueDemandingFact(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .call_proc => |call| {
                if (call.is_cold) return try self.cloneExprValue(expr_id);
                if (!self.inline_direct_calls) return try self.cloneExprValue(expr_id);
                return try self.inlineDirectCallValue(
                    Ast.callProcCallee(call),
                    call.args,
                    expr_id,
                    true,
                );
            },
            .block => |block| return try self.cloneBlockValueDemandingFact(expr.ty, block),
            .comptime_branch_taken => |taken| return try self.cloneExprValueDemandingFact(taken.body),
            else => return try self.cloneExprValue(expr_id),
        }
    }

    fn directCallHasKnownFactArg(self: *Cloner, args_span: Ast.Span(Ast.ExprId)) Allocator.Error!bool {
        for (self.pass.program.exprSpan(args_span)) |arg| {
            if (try self.exprHasKnownFact(arg)) return true;
        }
        return false;
    }

    fn directCallActiveArgFacts(self: *Cloner, args_span: Ast.Span(Ast.ExprId)) Allocator.Error![]const ValueFact {
        const args = self.pass.program.exprSpan(args_span);
        const facts = try self.pass.arena.allocator().alloc(ValueFact, args.len);
        for (args, 0..) |arg, index| {
            facts[index] = (try self.exprKnownFactNoInline(arg)) orelse .{
                .any = self.pass.program.exprs.items[@intFromEnum(arg)].ty,
            };
        }
        return facts;
    }

    fn exprKnownFactNoInline(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!?ValueFact {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .local => |local| if (self.subst.get(local)) |value|
                try self.pass.factFromValue(value)
            else
                null,
            .fn_ref => |fn_id| try self.callableFact(expr.ty, fn_id),
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
            => try self.pass.constructorFact(expr_id),
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk null;
                const receiver = self.subst.get(receiver_local) orelse break :blk null;
                const value = fieldFromValue(receiver, field.field) orelse break :blk null;
                break :blk try self.pass.factFromValue(value);
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk null;
                const tuple = self.subst.get(tuple_local) orelse break :blk null;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk null;
                break :blk try self.pass.factFromValue(value);
            },
            .comptime_branch_taken => |taken| try self.exprKnownFactNoInline(taken.body),
            else => null,
        };
    }

    fn exprHasKnownFact(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!bool {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .local => |local| if (self.subst.get(local)) |value|
                (try self.pass.factFromValue(value)) != null
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
            => (try self.pass.constructorFact(expr_id)) != null,
            .static_data_candidate => true,
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk false;
                const receiver = self.subst.get(receiver_local) orelse break :blk false;
                const value = fieldFromValue(receiver, field.field) orelse break :blk false;
                break :blk (try self.pass.factFromValue(value)) != null;
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk false;
                const tuple = self.subst.get(tuple_local) orelse break :blk false;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk false;
                break :blk (try self.pass.factFromValue(value)) != null;
            },
            .comptime_branch_taken => |taken| try self.exprHasKnownFact(taken.body),
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
            .expr_with_known_fact => |known_fact_expr| self.exprCanSubstitute(known_fact_expr.expr),
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
            } else {
                captures[index] = .{ .expr = try self.addExpr(.{
                    .ty = capture.ty,
                    .data = .{ .local = capture.local },
                }) };
            }
        }
        return .{ .callable = .{
            .ty = ty,
            .fn_id = fn_id,
            .captures = captures,
        } };
    }

    fn callableFact(self: *Cloner, ty: Type.TypeId, fn_id: Ast.FnId) Allocator.Error!ValueFact {
        const fn_ = self.pass.program.fns.items[@intFromEnum(fn_id)];
        const source_captures = self.pass.program.typedLocalSpan(fn_.captures);
        const captures = try self.pass.arena.allocator().alloc(ValueFact, source_captures.len);
        for (source_captures, 0..) |capture, index| {
            captures[index] = if (self.subst.get(capture.local)) |value|
                (try self.pass.factFromValue(value)) orelse .{ .any = valueType(self.pass.program, value) }
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
        if (solved_captures.len != 0) return null;
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
            else
                .{ .expr = try self.addExpr(.{
                    .ty = source_captures[index].ty,
                    .data = .{ .local = capture.local },
                }) };
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
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.cloneExpr(value) else null },
            .continue_ => |continue_| try self.cloneContinue(expr.ty, continue_),
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
        const raw_value = try self.cloneExprValueDemandingFact(let_.value);
        var pending_lets = std.ArrayList(PendingLet).empty;
        defer pending_lets.deinit(self.pass.allocator);

        const pending_change_start = self.changes.items.len;
        var value = raw_value;
        while (value == .let_) {
            try pending_lets.appendSlice(self.pass.allocator, value.let_.lets);
            try self.bindPendingLetFacts(value.let_.lets);
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
        if (try self.bindPatToMaterializedFact(let_.bind, raw_value)) {
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
            if (try self.bindPatToMaterializedFact(let_.bind, value)) {
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

    fn cloneLoop(self: *Cloner, ty: Type.TypeId, loop: anytype) Common.LowerError!Ast.ExprId {
        const params = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(loop.params));
        defer self.pass.allocator.free(params);
        const initial_values = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(loop.initial_values));
        defer self.pass.allocator.free(initial_values);
        if (params.len != initial_values.len) Common.invariant("loop parameter count differed from initial value count");

        const values = try self.pass.allocator.alloc(Value, initial_values.len);
        defer self.pass.allocator.free(values);
        for (initial_values, 0..) |initial, index| {
            values[index] = try self.cloneExprValueDemandingFact(initial);
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

        const facts = try self.pass.arena.allocator().alloc(ValueFact, values.len);
        var has_constructor = false;
        for (values, 0..) |value, index| {
            if (try self.pass.factFromValue(value)) |fact| {
                if (try self.projectableLoopFactForValue(fact, value)) |loop_fact| {
                    facts[index] = loop_fact;
                    has_constructor = true;
                } else {
                    facts[index] = .{ .any = valueType(self.pass.program, value) };
                }
            } else {
                facts[index] = .{ .any = valueType(self.pass.program, value) };
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

            for (params, facts, values) |param, fact, value| {
                const param_value = try self.valueFromFactArgs(fact, &new_params);
                try self.putSubst(param.local, param_value);
                try self.appendExprsFromValue(fact, value, &new_initials);
            }

            const refinements = try self.pass.allocator.alloc(?ValueFact, facts.len);
            defer self.pass.allocator.free(refinements);
            @memset(refinements, null);

            try self.loop_stack.append(self.pass.allocator, .{
                .values = facts,
                .refinements = refinements,
            });
            const body = try self.cloneExpr(loop.body);
            _ = self.loop_stack.pop();

            var refined = false;
            for (facts, refinements, 0..) |*fact, maybe_refinement, index| {
                const refinement = maybe_refinement orelse continue;
                if (factEql(self.pass.program, fact.*, refinement)) continue;
                facts[index] = refinement;
                refined = true;
            }
            if (refined) continue;

            return try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
                .params = try self.pass.program.addTypedLocalSpan(new_params.items),
                .initial_values = try self.pass.program.addExprSpan(new_initials.items),
                .body = body,
            } } });
        }
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
            try self.bindPendingLetFacts(let_value.lets);

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

    fn projectableLoopFactForValue(self: *Cloner, fact: ValueFact, value: Value) Allocator.Error!?ValueFact {
        return switch (value) {
            .record => |record_value| blk: {
                const record_fact = switch (fact) {
                    .record => |record| record,
                    else => break :blk null,
                };
                if (record_fact.fields.len != record_value.fields.len) Common.invariant("record loop state changed field count before specialization");
                const fields = try self.pass.arena.allocator().alloc(FieldFact, record_fact.fields.len);
                for (record_fact.fields, record_value.fields, 0..) |field_fact, field_value, index| {
                    if (field_fact.name != field_value.name) Common.invariant("record loop state changed field order before specialization");
                    fields[index] = .{
                        .name = field_fact.name,
                        .fact = (try self.projectableLoopFactForValue(field_fact.fact, field_value.value)) orelse
                            .{ .any = factType(field_fact.fact) },
                    };
                }
                break :blk ValueFact{ .record = .{
                    .ty = record_fact.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple_value| blk: {
                const tuple_fact = switch (fact) {
                    .tuple => |tuple| tuple,
                    else => break :blk null,
                };
                if (tuple_fact.items.len != tuple_value.items.len) Common.invariant("tuple loop state changed item count before specialization");
                const items = try self.pass.arena.allocator().alloc(ValueFact, tuple_fact.items.len);
                for (tuple_fact.items, tuple_value.items, 0..) |item_fact, item_value, index| {
                    items[index] = (try self.projectableLoopFactForValue(item_fact, item_value)) orelse
                        .{ .any = factType(item_fact) };
                }
                break :blk ValueFact{ .tuple = .{
                    .ty = tuple_fact.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal_value| blk: {
                const nominal_fact = switch (fact) {
                    .nominal => |nominal| nominal,
                    else => break :blk null,
                };
                const backing = (try self.projectableLoopFactForValue(nominal_fact.backing.*, nominal_value.backing.*)) orelse break :blk null;
                const stored = try self.pass.arena.allocator().create(ValueFact);
                stored.* = backing;
                break :blk ValueFact{ .nominal = .{
                    .ty = nominal_fact.ty,
                    .backing = stored,
                } };
            },
            .callable => |callable_value| blk: {
                const callable_fact = switch (fact) {
                    .callable => |callable| callable,
                    else => break :blk null,
                };
                if (!callableTargetMatches(self.pass.program, callable_fact.fn_id, callable_value.fn_id) or
                    callable_fact.captures.len != callable_value.captures.len)
                {
                    break :blk null;
                }
                const captures = try self.pass.arena.allocator().alloc(ValueFact, callable_fact.captures.len);
                for (callable_fact.captures, callable_value.captures, 0..) |capture_fact, capture_value, index| {
                    captures[index] = (try self.projectableLoopFactForValue(capture_fact, capture_value)) orelse
                        .{ .any = factType(capture_fact) };
                }
                break :blk ValueFact{ .callable = .{
                    .ty = callable_fact.ty,
                    .fn_id = callable_fact.fn_id,
                    .captures = captures,
                } };
            },
            .let_ => null,
            .if_ => null,
            .expr_with_known_fact => |known| if (canReadFieldsFromExpr(self.pass.program, known.expr))
                try self.projectableLoopFactFromExpr(known.fact)
            else
                null,
            .expr,
            .tag,
            => if (factCanProjectFromExpr(fact)) fact else null,
        };
    }

    fn projectableLoopFactFromExpr(self: *Cloner, fact: ValueFact) Allocator.Error!?ValueFact {
        if (factCanProjectFromExpr(fact)) return fact;

        return switch (fact) {
            .any => fact,
            .leaf => fact,
            .record => |record| blk: {
                const fields = try self.pass.arena.allocator().alloc(FieldFact, record.fields.len);
                for (record.fields, fields) |field, *out| {
                    out.* = .{
                        .name = field.name,
                        .fact = (try self.projectableLoopFactFromExpr(field.fact)) orelse
                            .{ .any = factType(field.fact) },
                    };
                }
                break :blk ValueFact{ .record = .{
                    .ty = record.ty,
                    .fields = fields,
                } };
            },
            .tuple => |tuple| blk: {
                const items = try self.pass.arena.allocator().alloc(ValueFact, tuple.items.len);
                for (tuple.items, items) |item, *out| {
                    out.* = (try self.projectableLoopFactFromExpr(item)) orelse
                        .{ .any = factType(item) };
                }
                break :blk ValueFact{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing = (try self.projectableLoopFactFromExpr(nominal.backing.*)) orelse break :blk null;
                const stored = try self.pass.arena.allocator().create(ValueFact);
                stored.* = backing;
                break :blk ValueFact{ .nominal = .{
                    .ty = nominal.ty,
                    .backing = stored,
                } };
            },
            .tag,
            .callable,
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

    fn cloneBlockValueDemandingFact(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Value {
        return try self.cloneBlockValueWithFinalDemand(ty, block, true);
    }

    fn cloneBlockValueWithFinalDemand(
        self: *Cloner,
        ty: Type.TypeId,
        block: anytype,
        demand_final_fact: bool,
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

        const final_value = if (demand_final_fact)
            try self.cloneExprValueDemandingFact(block.final_expr)
        else
            try self.cloneExprValue(block.final_expr);
        if (demand_final_fact) {
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

        const fact = (try self.pass.factFromValue(final_value)) orelse return .{ .expr = block_expr };
        return .{ .expr_with_known_fact = .{
            .expr = block_expr,
            .fact = fact,
        } };
    }

    fn cloneContinue(self: *Cloner, ty: Type.TypeId, continue_: anytype) Common.LowerError!Ast.ExprData {
        const loop = self.loop_stack.getLastOrNull() orelse return .{ .continue_ = .{
            .values = try self.cloneExprSpan(continue_.values),
        } };
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
            var value = try self.cloneExprValueDemandingFact(value_expr);
            while (value == .let_) {
                try self.appendPendingLetStmts(value.let_.lets, &pending_statements);
                try self.bindPendingLetFacts(value.let_.lets);
                value = value.let_.body.*;
            }
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

    fn cloneContinueDataFromValues(
        self: *Cloner,
        ty: Type.TypeId,
        loop: LoopPattern,
        values: []const Value,
    ) Common.LowerError!Ast.ExprData {
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

        for (loop.values, values, 0..) |fact, value, index| {
            if (!factMatchesValue(self.pass.program, fact, value)) {
                if (!try self.appendFieldReadExprsFromValue(fact, value, &new_values)) {
                    const refined = try self.refineLoopFactForValue(fact, value);
                    try self.noteLoopRefinement(loop, index, refined);
                    if (!try self.appendFieldReadExprsFromValue(refined, value, &new_values)) {
                        Common.invariant("refined continue value did not match specialized loop state");
                    }
                }
                continue;
            }
            try self.appendExprsFromValue(fact, value, &new_values);
        }

        return .{ .continue_ = .{
            .values = try self.pass.program.addExprSpan(new_values.items),
        } };
    }

    fn noteLoopRefinement(self: *Cloner, loop: LoopPattern, index: usize, refinement: ValueFact) Allocator.Error!void {
        if (index >= loop.refinements.len) Common.invariant("loop refinement index exceeded active loop state");
        loop.refinements[index] = if (loop.refinements[index]) |existing|
            try self.commonLoopFact(existing, refinement)
        else
            refinement;
    }

    fn refineLoopFactForValue(self: *Cloner, fact: ValueFact, value: Value) Common.LowerError!ValueFact {
        if (factMatchesValue(self.pass.program, fact, value)) return fact;

        return switch (fact) {
            .any => fact,
            .leaf => fact,
            .record => |record| blk: {
                const fields = try self.pass.arena.allocator().alloc(FieldFact, record.fields.len);
                if (recordFromValue(value)) |record_value| {
                    if (record.fields.len != record_value.fields.len) Common.invariant("record loop state changed field count");
                    for (record.fields, record_value.fields, 0..) |field, field_value, index| {
                        if (field.name != field_value.name) Common.invariant("record loop state changed field order");
                        fields[index] = .{
                            .name = field.name,
                            .fact = try self.refineLoopFactForValue(field.fact, field_value.value),
                        };
                    }
                    break :blk ValueFact{ .record = .{ .ty = record.ty, .fields = fields } };
                }

                const receiver = projectableExprFromValue(value) orelse break :blk ValueFact{ .any = record.ty };
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) break :blk ValueFact{ .any = record.ty };
                const actual_fact = switch (value) {
                    .expr_with_known_fact => |known_fact_expr| known_fact_expr.fact,
                    else => null,
                };
                for (record.fields, 0..) |field, index| {
                    const actual_field = if (actual_fact) |actual|
                        fieldFactFromFact(actual, field.name)
                    else
                        null;
                    const field_expr = try self.addExpr(.{ .ty = factType(field.fact), .data = .{ .field_access = .{
                        .receiver = receiver,
                        .field = field.name,
                    } } });
                    const field_value = if (actual_field) |actual|
                        valueFromProjectedExpr(field_expr, actual)
                    else
                        Value{ .expr = field_expr };
                    fields[index] = .{
                        .name = field.name,
                        .fact = try self.refineLoopFactForValue(field.fact, field_value),
                    };
                }
                break :blk ValueFact{ .record = .{ .ty = record.ty, .fields = fields } };
            },
            .tuple => |tuple| blk: {
                const items = try self.pass.arena.allocator().alloc(ValueFact, tuple.items.len);
                if (tupleFromValue(value)) |tuple_value| {
                    if (tuple.items.len != tuple_value.items.len) Common.invariant("tuple loop state changed item count");
                    for (tuple.items, tuple_value.items, 0..) |item, item_value, index| {
                        items[index] = try self.refineLoopFactForValue(item, item_value);
                    }
                    break :blk ValueFact{ .tuple = .{ .ty = tuple.ty, .items = items } };
                }

                const receiver = projectableExprFromValue(value) orelse break :blk ValueFact{ .any = tuple.ty };
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) break :blk ValueFact{ .any = tuple.ty };
                const actual_fact = switch (value) {
                    .expr_with_known_fact => |known_fact_expr| known_fact_expr.fact,
                    else => null,
                };
                for (tuple.items, 0..) |item, index| {
                    const actual_item = if (actual_fact) |actual|
                        itemFactFromFact(actual, @as(u32, @intCast(index)))
                    else
                        null;
                    const item_expr = try self.addExpr(.{ .ty = factType(item), .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    const item_value = if (actual_item) |actual|
                        valueFromProjectedExpr(item_expr, actual)
                    else
                        Value{ .expr = item_expr };
                    items[index] = try self.refineLoopFactForValue(item, item_value);
                }
                break :blk ValueFact{ .tuple = .{ .ty = tuple.ty, .items = items } };
            },
            .nominal => |nominal| blk: {
                const value_nominal = switch (value) {
                    .nominal => |nominal_value| nominal_value,
                    else => break :blk ValueFact{ .any = nominal.ty },
                };
                const backing = try self.pass.arena.allocator().create(ValueFact);
                backing.* = try self.refineLoopFactForValue(nominal.backing.*, value_nominal.backing.*);
                break :blk ValueFact{ .nominal = .{ .ty = nominal.ty, .backing = backing } };
            },
            .tag => |tag| if (tagFromValue(value) != null) fact else ValueFact{ .any = tag.ty },
            .callable => |callable| blk: {
                const callable_value = switch (value) {
                    .callable => |callable_value| callable_value,
                    else => break :blk ValueFact{ .any = callable.ty },
                };
                if (!sameType(self.pass.program, callable.ty, callable_value.ty) or
                    !callableTargetMatches(self.pass.program, callable.fn_id, callable_value.fn_id) or
                    callable.captures.len != callable_value.captures.len)
                {
                    break :blk ValueFact{ .any = callable.ty };
                }
                const captures = try self.pass.arena.allocator().alloc(ValueFact, callable.captures.len);
                for (callable.captures, callable_value.captures, 0..) |capture, capture_value, index| {
                    captures[index] = try self.refineLoopFactForValue(capture, capture_value);
                }
                break :blk ValueFact{ .callable = .{
                    .ty = callable.ty,
                    .fn_id = callable.fn_id,
                    .captures = captures,
                } };
            },
        };
    }

    fn commonLoopFact(self: *Cloner, lhs: ValueFact, rhs: ValueFact) Allocator.Error!ValueFact {
        if (factEql(self.pass.program, lhs, rhs)) return lhs;
        const ty = factType(lhs);
        if (!sameType(self.pass.program, ty, factType(rhs))) Common.invariant("loop state refinement changed type");
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return .{ .any = ty };

        return switch (lhs) {
            .any => .{ .any = ty },
            .leaf => .{ .leaf = ty },
            .record => |lhs_record| blk: {
                const rhs_record = rhs.record;
                if (lhs_record.fields.len != rhs_record.fields.len) break :blk ValueFact{ .any = ty };
                const fields = try self.pass.arena.allocator().alloc(FieldFact, lhs_record.fields.len);
                for (lhs_record.fields, rhs_record.fields, 0..) |lhs_field, rhs_field, index| {
                    if (lhs_field.name != rhs_field.name) break :blk ValueFact{ .any = ty };
                    fields[index] = .{
                        .name = lhs_field.name,
                        .fact = try self.commonLoopFact(lhs_field.fact, rhs_field.fact),
                    };
                }
                break :blk ValueFact{ .record = .{ .ty = lhs_record.ty, .fields = fields } };
            },
            .tuple => |lhs_tuple| blk: {
                const rhs_tuple = rhs.tuple;
                if (lhs_tuple.items.len != rhs_tuple.items.len) break :blk ValueFact{ .any = ty };
                const items = try self.pass.arena.allocator().alloc(ValueFact, lhs_tuple.items.len);
                for (lhs_tuple.items, rhs_tuple.items, 0..) |lhs_item, rhs_item, index| {
                    items[index] = try self.commonLoopFact(lhs_item, rhs_item);
                }
                break :blk ValueFact{ .tuple = .{ .ty = lhs_tuple.ty, .items = items } };
            },
            .nominal => |lhs_nominal| blk: {
                const rhs_nominal = rhs.nominal;
                const backing = try self.pass.arena.allocator().create(ValueFact);
                backing.* = try self.commonLoopFact(lhs_nominal.backing.*, rhs_nominal.backing.*);
                break :blk ValueFact{ .nominal = .{ .ty = lhs_nominal.ty, .backing = backing } };
            },
            .callable => |lhs_callable| blk: {
                const rhs_callable = rhs.callable;
                if (!callableTargetMatches(self.pass.program, lhs_callable.fn_id, rhs_callable.fn_id) or
                    lhs_callable.captures.len != rhs_callable.captures.len)
                {
                    break :blk ValueFact{ .any = ty };
                }
                const captures = try self.pass.arena.allocator().alloc(ValueFact, lhs_callable.captures.len);
                for (lhs_callable.captures, rhs_callable.captures, 0..) |lhs_capture, rhs_capture, index| {
                    captures[index] = try self.commonLoopFact(lhs_capture, rhs_capture);
                }
                break :blk ValueFact{ .callable = .{
                    .ty = lhs_callable.ty,
                    .fn_id = lhs_callable.fn_id,
                    .captures = captures,
                } };
            },
            .tag => .{ .any = ty },
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
        for (pattern.args, args) |fact, arg| {
            if (!try self.appendClonedExprsForFact(fact, arg, out)) return false;
        }
        return true;
    }

    fn appendClonedExprsForFact(
        self: *Cloner,
        fact: ValueFact,
        expr_id: Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        switch (fact) {
            .any => {
                try out.append(self.pass.allocator, try self.cloneExpr(expr_id));
                return true;
            },
            else => {
                const value = try self.valueForCallArg(expr_id);
                if (!factMatchesValue(self.pass.program, fact, value)) return false;
                try self.appendExprsFromValue(fact, value, out);
                return true;
            },
        }
    }

    fn valueForCallArg(self: *Cloner, expr_id: Ast.ExprId) Common.LowerError!Value {
        return try self.cloneExprValueDemandingFact(expr_id);
    }

    fn appendExprsFromValue(
        self: *Cloner,
        fact: ValueFact,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!void {
        if (value == .expr_with_known_fact) switch (fact) {
            .any => {},
            else => {
                if (!try self.appendFieldReadExprsFromValue(fact, value, out)) {
                    Common.invariant("known-fact expression could not be split into requested fact");
                }
                return;
            },
        };

        switch (fact) {
            .any,
            .leaf,
            => try out.append(self.pass.allocator, try self.materialize(value)),
            .tag => |tag| {
                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => Common.invariant("tag call pattern matched a non-tag value"),
                };
                for (tag.payloads, tag_value.payloads) |payload_fact, payload| {
                    try self.appendExprsFromValue(payload_fact, payload, out);
                }
            },
            .record => |record| {
                const record_value = switch (value) {
                    .record => |record_value| record_value,
                    else => Common.invariant("record call pattern matched a non-record value"),
                };
                for (record.fields, record_value.fields) |field_fact, field| {
                    if (field_fact.name != field.name) Common.invariant("record call-pattern field order changed after matching");
                    try self.appendExprsFromValue(field_fact.fact, field.value, out);
                }
            },
            .tuple => |tuple| {
                const tuple_value = switch (value) {
                    .tuple => |tuple_value| tuple_value,
                    else => Common.invariant("tuple call pattern matched a non-tuple value"),
                };
                for (tuple.items, tuple_value.items) |item_fact, item| {
                    try self.appendExprsFromValue(item_fact, item, out);
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
                for (callable.captures, callable_value.captures) |capture_fact, capture_value| {
                    try self.appendExprsFromValue(capture_fact, capture_value, out);
                }
            },
        }
    }

    fn appendFieldReadExprsFromValue(
        self: *Cloner,
        fact: ValueFact,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        if (value != .expr_with_known_fact and factMatchesValue(self.pass.program, fact, value)) {
            try self.appendExprsFromValue(fact, value, out);
            return true;
        }

        switch (fact) {
            .any,
            .leaf,
            => {
                try out.append(self.pass.allocator, try self.materialize(value));
                return true;
            },
            .record => |record| {
                if (recordFromValue(value)) |record_value| {
                    if (record.fields.len != record_value.fields.len) return false;
                    for (record.fields, record_value.fields) |field_fact, field_value| {
                        if (field_fact.name != field_value.name) return false;
                        if (!try self.appendFieldReadExprsFromValue(field_fact.fact, field_value.value, out)) return false;
                    }
                    return true;
                }

                const receiver = projectableExprFromValue(value) orelse return false;
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                for (record.fields) |field| {
                    const field_expr = try self.addExpr(.{ .ty = factType(field.fact), .data = .{ .field_access = .{
                        .receiver = receiver,
                        .field = field.name,
                    } } });
                    if (!try self.appendFieldReadExprsFromValue(field.fact, .{ .expr = field_expr }, out)) return false;
                }
                return true;
            },
            .tuple => |tuple| {
                if (tupleFromValue(value)) |tuple_value| {
                    if (tuple.items.len != tuple_value.items.len) return false;
                    for (tuple.items, tuple_value.items) |item_fact, item_value| {
                        if (!try self.appendFieldReadExprsFromValue(item_fact, item_value, out)) return false;
                    }
                    return true;
                }

                const receiver = projectableExprFromValue(value) orelse return false;
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                for (tuple.items, 0..) |item, index| {
                    const item_expr = try self.addExpr(.{ .ty = factType(item), .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    if (!try self.appendFieldReadExprsFromValue(item, .{ .expr = item_expr }, out)) return false;
                }
                return true;
            },
            .nominal => |nominal| return try self.appendFieldReadExprsFromValue(nominal.backing.*, value, out),
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
                for (callable.captures, callable_value.captures) |capture_fact, capture_value| {
                    if (!try self.appendFieldReadExprsFromValue(capture_fact, capture_value, out)) return false;
                }
                return true;
            },
            .tag,
            => return false,
        }
    }

    fn cloneFieldAccess(self: *Cloner, ty: Type.TypeId, field: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingFact(field.receiver);
        if (try self.fieldFromKnownValue(receiver, field.field)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = try self.materialize(receiver),
            .field = field.field,
        } } });
    }

    fn cloneTupleAccess(self: *Cloner, ty: Type.TypeId, access: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValueDemandingFact(access.tuple);
        if (try self.itemFromKnownValue(receiver, access.elem_index)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
            .tuple = try self.materialize(receiver),
            .elem_index = access.elem_index,
        } } });
    }

    fn fieldFromKnownValue(self: *Cloner, receiver: Value, field: names.RecordFieldNameId) Common.LowerError!?Value {
        if (fieldFromValue(receiver, field)) |value| return value;

        const known_fact_expr = switch (receiver) {
            .expr_with_known_fact => |known_fact_expr| known_fact_expr,
            else => return null,
        };
        if (!canReadFieldsFromExpr(self.pass.program, known_fact_expr.expr)) return null;

        const field_fact = fieldFactFromFact(known_fact_expr.fact, field) orelse return null;
        const field_expr = try self.addExpr(.{ .ty = factType(field_fact), .data = .{ .field_access = .{
            .receiver = known_fact_expr.expr,
            .field = field,
        } } });
        return valueFromProjectedExpr(field_expr, field_fact);
    }

    fn itemFromKnownValue(self: *Cloner, receiver: Value, index: u32) Common.LowerError!?Value {
        if (itemFromValue(receiver, index)) |value| return value;

        const known_fact_expr = switch (receiver) {
            .expr_with_known_fact => |known_fact_expr| known_fact_expr,
            else => return null,
        };
        if (!canReadFieldsFromExpr(self.pass.program, known_fact_expr.expr)) return null;

        const item_fact = itemFactFromFact(known_fact_expr.fact, index) orelse return null;
        const item_expr = try self.addExpr(.{ .ty = factType(item_fact), .data = .{ .tuple_access = .{
            .tuple = known_fact_expr.expr,
            .elem_index = index,
        } } });
        return valueFromProjectedExpr(item_expr, item_fact);
    }

    fn cloneIfValue(self: *Cloner, ty: Type.TypeId, if_: anytype) Common.LowerError!Value {
        const source_branches = try self.pass.allocator.dupe(Ast.IfBranch, self.pass.program.ifBranchSpan(if_.branches));
        defer self.pass.allocator.free(source_branches);

        const branches = try self.pass.allocator.alloc(Ast.IfBranch, source_branches.len);
        defer self.pass.allocator.free(branches);
        const body_values = try self.pass.allocator.alloc(Value, source_branches.len + 1);
        defer self.pass.allocator.free(body_values);

        for (source_branches, 0..) |branch, index| {
            branches[index] = .{
                .cond = try self.materialize(try self.cloneExprValueDemandingFact(branch.cond)),
                .body = undefined,
            };
            body_values[index] = try self.cloneExprValueDemandingFact(branch.body);
        }

        body_values[source_branches.len] = try self.cloneExprValueDemandingFact(if_.final_else);

        const if_branches = try self.pass.arena.allocator().alloc(IfValueBranch, source_branches.len);
        for (branches, body_values[0..source_branches.len], 0..) |branch, body, index| {
            if_branches[index] = .{
                .cond = branch.cond,
                .body = body,
            };
        }
        return .{ .if_ = .{
            .ty = ty,
            .branches = if_branches,
            .final_else = try self.copyValue(body_values[source_branches.len]),
        } };
    }

    fn cloneMatchJoinedValue(self: *Cloner, ty: Type.TypeId, scrutinee_expr: Ast.ExprId, match: @import("../monotype/ast.zig").MatchExpr) Common.LowerError!Value {
        const source_branches = try self.pass.allocator.dupe(Ast.Branch, self.pass.program.branchSpan(match.branches));
        defer self.pass.allocator.free(source_branches);

        const branches = try self.pass.allocator.alloc(Ast.Branch, source_branches.len);
        defer self.pass.allocator.free(branches);
        const body_values = try self.pass.allocator.alloc(Value, source_branches.len);
        defer self.pass.allocator.free(body_values);

        for (source_branches, 0..) |branch, index| {
            branches[index] = .{
                .pat = try self.clonePat(branch.pat),
                .guard = if (branch.guard) |guard| try self.cloneExpr(guard) else null,
                .body = undefined,
            };
            body_values[index] = try self.cloneExprValueDemandingFact(branch.body);
            branches[index].body = try self.materialize(body_values[index]);
        }

        const fact = try self.joinValueFacts(body_values);
        const match_expr = try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = scrutinee_expr,
            .branches = try self.pass.program.addBranchSpan(branches),
            .comptime_site = match.comptime_site,
        } } });

        if (fact == null) return .{ .expr = match_expr };

        return .{ .expr_with_known_fact = .{
            .expr = match_expr,
            .fact = fact.?,
        } };
    }

    fn joinValueFacts(self: *Cloner, values: []const Value) Allocator.Error!?ValueFact {
        if (values.len == 0) return null;
        var joined = (try self.pass.factFromValue(values[0])) orelse return null;
        for (values[1..]) |value| {
            const next = (try self.pass.factFromValue(value)) orelse return null;
            joined = (try self.joinFacts(joined, next)) orelse return null;
        }
        return joined;
    }

    fn joinFacts(self: *Cloner, lhs: ValueFact, rhs: ValueFact) Allocator.Error!?ValueFact {
        return try joinFactsInArena(self.pass.program, self.pass.arena.allocator(), lhs, rhs);
    }

    fn cloneMatch(self: *Cloner, ty: Type.TypeId, match: @import("../monotype/ast.zig").MatchExpr) Common.LowerError!Ast.ExprId {
        const scrutinee = try self.cloneExprValue(match.scrutinee);
        if (try self.simplifyKnownMatch(ty, scrutinee, match.branches)) |body| return body;

        const scrutinee_expr = try self.materialize(scrutinee);
        return try self.addExpr(.{ .ty = ty, .data = .{ .match_ = .{
            .scrutinee = scrutinee_expr,
            .branches = try self.cloneBranchSpan(match.branches),
            .comptime_site = match.comptime_site,
        } } });
    }

    fn simplifyKnownMatch(self: *Cloner, ty: Type.TypeId, scrutinee: Value, branches_span: Ast.Span(Ast.Branch)) Common.LowerError!?Ast.ExprId {
        if (try self.simplifyKnownMatchValue(ty, scrutinee, branches_span)) |value| {
            return try self.materialize(value);
        }
        return null;
    }

    fn simplifyKnownMatchValue(self: *Cloner, ty: Type.TypeId, scrutinee: Value, branches_span: Ast.Span(Ast.Branch)) Common.LowerError!?Value {
        return try self.simplifyKnownMatchValueMode(ty, scrutinee, branches_span, .strict);
    }

    fn simplifyKnownMatchValueMode(
        self: *Cloner,
        ty: Type.TypeId,
        scrutinee: Value,
        branches_span: Ast.Span(Ast.Branch),
        mode: KnownMatchMode,
    ) Common.LowerError!?Value {
        switch (scrutinee) {
            .expr,
            .expr_with_known_fact,
            .let_,
            => return null,
            .if_ => |if_value| return try self.simplifyKnownMatchIfValue(ty, if_value, branches_span),
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
            return try self.wrapPendingLets(body, pending_lets.items, false);
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
    ) Common.LowerError!?Value {
        const branches = try self.pass.arena.allocator().alloc(IfValueBranch, if_value.branches.len);
        for (if_value.branches, 0..) |branch, index| {
            branches[index] = .{
                .cond = branch.cond,
                .body = (try self.simplifyKnownMatchValueMode(ty, branch.body, branches_span, .speculative)) orelse
                    return null,
            };
        }

        const final_else = try self.pass.arena.allocator().create(Value);
        final_else.* = (try self.simplifyKnownMatchValueMode(ty, if_value.final_else.*, branches_span, .speculative)) orelse
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
                const record = recordFromValue(value) orelse return null;
                const fields = self.pass.program.recordDestructSpan(fields_span);
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
            },
            .tuple => |items_span| {
                const tuple = tupleFromValue(value) orelse return null;
                const pats = self.pass.program.patSpan(items_span);
                if (pats.len != tuple.items.len) return null;
                const items = try self.pass.arena.allocator().alloc(Value, tuple.items.len);
                for (pats, tuple.items, 0..) |child_pat, child_value, index| {
                    items[index] = (try self.bindPatToMatchValue(child_pat, child_value, body, unsafe_count, pending_lets)) orelse return null;
                }
                return Value{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
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
        unsafe_count: usize,
        pending_lets: *std.ArrayList(PendingLet),
    ) Common.LowerError!Value {
        _ = unsafe_count;
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
            .expr_with_known_fact => |known_fact_expr| if (self.exprCanSubstitute(known_fact_expr.expr)) 0 else 1,
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
            .expr_with_known_fact => |known_fact_expr| blk: {
                const ty = self.pass.program.exprs.items[@intFromEnum(known_fact_expr.expr)].ty;
                const local = try self.pass.program.addLocal(self.pass.symbols.fresh(), ty);
                try pending_lets.append(self.pass.allocator, .{
                    .local = local,
                    .ty = ty,
                    .value = known_fact_expr.expr,
                    .fact = known_fact_expr.fact,
                });
                const local_expr = try self.addExpr(.{
                    .ty = ty,
                    .data = .{ .local = local },
                });
                break :blk Value{ .expr_with_known_fact = .{
                    .expr = local_expr,
                    .fact = known_fact_expr.fact,
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
                    branches[index] = .{
                        .cond = branch.cond,
                        .body = try self.makeReusableForMatch(branch.body, pending_lets),
                    };
                }
                const final_else = try self.pass.arena.allocator().create(Value);
                final_else.* = try self.makeReusableForMatch(if_value.final_else.*, pending_lets);
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
        };
    }

    fn wrapPendingLets(self: *Cloner, body: Value, pending_lets: []const PendingLet, preserve_fact: bool) Common.LowerError!Value {
        if (pending_lets.len == 0) return body;

        const fact = if (preserve_fact) try self.pass.factFromValue(body) else null;
        if (fact != null) {
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
            const outer_value = (try self.simplifyKnownMatchValueMode(ty, inner_value, outer_branches_span, .speculative)) orelse return null;
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
        const body = switch (source_fn.body) {
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
            prepared_captures[index] = try self.valueForInlineLocal(source_capture.local, capture_value, body, 0, &pending_lets);
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
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, unsafe_count, &pending_lets);
        }

        try self.clearBinderSubstitutionsForInline();

        try self.inline_stack.append(self.pass.allocator, .{ .fn_id = callable.fn_id });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callable.fn_id) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        return try self.wrapPendingLets(try self.cloneExprValue(body), pending_lets.items, false);
    }

    fn inlineDirectCallValue(
        self: *Cloner,
        callee: Ast.FnId,
        args_span: Ast.Span(Ast.ExprId),
        original_expr: Ast.ExprId,
        demand_result_fact: bool,
    ) Common.LowerError!Value {
        const active_arg_facts = try self.directCallActiveArgFacts(args_span);
        for (self.inline_stack.items) |active| {
            if (active.fn_id != callee) continue;
            const active_args = active.args orelse return .{ .expr = try self.cloneExprPlain(original_expr) };
            if (!factsStrictlyDescend(self.pass.program, active_args, active_arg_facts)) {
                return .{ .expr = try self.cloneExprPlain(original_expr) };
            }
        }

        const source_fn = self.pass.program.fns.items[@intFromEnum(callee)];
        const body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => {
                return .{ .expr = try self.cloneExprPlain(original_expr) };
            },
        };
        if (exprContainsReturn(self.pass.program, body)) {
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
                try self.cloneExprValueDemandingFact(arg_expr)
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
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, unsafe_count, &pending_lets);
        }

        try self.clearBinderSubstitutionsForInline();

        try self.inline_stack.append(self.pass.allocator, .{
            .fn_id = callee,
            .args = active_arg_facts,
        });
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped.fn_id != callee) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        const body_value = if (demand_result_fact)
            try self.cloneExprValueDemandingFact(body)
        else
            try self.cloneExprValue(body);
        return try self.wrapPendingLets(body_value, pending_lets.items, demand_result_fact);
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
                const record = recordFromValue(value) orelse return false;
                const fields = self.pass.program.recordDestructSpan(fields_span);
                for (fields) |field| {
                    const field_value = fieldFromRecord(record, field.name) orelse return false;
                    if (!try self.bindPatToValue(field.pattern, field_value)) return false;
                }
                return true;
            },
            .tuple => |items_span| {
                const tuple = tupleFromValue(value) orelse return false;
                const pats = self.pass.program.patSpan(items_span);
                if (pats.len != tuple.items.len) return false;
                for (pats, tuple.items) |child_pat, child_value| {
                    if (!try self.bindPatToValue(child_pat, child_value)) return false;
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

    fn bindPatToMaterializedFact(self: *Cloner, pat_id: Ast.PatId, value: Value) Common.LowerError!bool {
        const fact = (try self.pass.factFromValue(value)) orelse return false;
        return try self.bindPatToExprWithKnownFact(pat_id, fact);
    }

    fn bindPatToExprWithKnownFact(self: *Cloner, pat_id: Ast.PatId, fact: ValueFact) Common.LowerError!bool {
        const pat = self.pass.program.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bind => |local| {
                const local_ty = self.pass.program.locals.items[@intFromEnum(local)].ty;
                const local_expr = try self.addExpr(.{
                    .ty = local_ty,
                    .data = .{ .local = local },
                });
                try self.putSubst(local, .{ .expr_with_known_fact = .{
                    .expr = local_expr,
                    .fact = fact,
                } });
                return true;
            },
            .wildcard => return true,
            .as => |as| {
                if (!try self.bindPatToExprWithKnownFact(as.pattern, fact)) return false;
                const local_ty = self.pass.program.locals.items[@intFromEnum(as.local)].ty;
                const local_expr = try self.addExpr(.{
                    .ty = local_ty,
                    .data = .{ .local = as.local },
                });
                try self.putSubst(as.local, .{ .expr_with_known_fact = .{
                    .expr = local_expr,
                    .fact = fact,
                } });
                return true;
            },
            .nominal => |backing_pat| {
                const backing_fact = switch (fact) {
                    .nominal => |nominal| nominal.backing.*,
                    else => return false,
                };
                return try self.bindPatToExprWithKnownFact(backing_pat, backing_fact);
            },
            .record,
            .tuple,
            .tag,
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
                    try self.cloneExprValueDemandingFact(let_.value);
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
                _ = try self.bindPatToMaterializedFact(let_.pat, value);
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

    fn bindPendingLetFacts(self: *Cloner, pending_lets: []const PendingLet) Common.LowerError!void {
        for (pending_lets) |pending| {
            const fact = pending.fact orelse continue;
            const local_expr = try self.addExpr(.{
                .ty = pending.ty,
                .data = .{ .local = pending.local },
            });
            try self.putSubst(pending.local, .{ .expr_with_known_fact = .{
                .expr = local_expr,
                .fact = fact,
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
                .fact = try self.pass.constructorFact(let_.value),
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

    fn cloneBranchSpan(self: *Cloner, span: Ast.Span(Ast.Branch)) Common.LowerError!Ast.Span(Ast.Branch) {
        const source = try self.pass.allocator.dupe(Ast.Branch, self.pass.program.branchSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.Branch, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |branch, index| {
            values[index] = .{
                .pat = try self.clonePat(branch.pat),
                .guard = if (branch.guard) |guard| try self.cloneExpr(guard) else null,
                .body = try self.cloneExpr(branch.body),
            };
        }
        return try self.pass.program.addBranchSpan(values);
    }

    fn cloneIfBranchSpan(self: *Cloner, span: Ast.Span(Ast.IfBranch)) Common.LowerError!Ast.Span(Ast.IfBranch) {
        const source = try self.pass.allocator.dupe(Ast.IfBranch, self.pass.program.ifBranchSpan(span));
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
            .expr_with_known_fact => |known_fact_expr| return known_fact_expr.expr,
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
        }
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
            var active_index = self.callable_stack.items.len;
            while (active_index > 0) {
                active_index -= 1;
                const active = self.callable_stack.items[active_index];
                if (active.source == callable.fn_id) {
                    const active_fn = self.pass.program.fns.items[@intFromEnum(active.specialized)];
                    return try self.materializeCallableWithCaptures(
                        callable.ty,
                        active.specialized,
                        active_fn.captures,
                        callable.captures,
                    );
                }
            }
            return try self.specializedCallableRef(callable);
        }

        return try self.addExpr(.{ .ty = callable.ty, .data = .{ .fn_ref = callable.fn_id } });
    }

    fn specializedCallableRef(self: *Cloner, callable: CallableValue) Common.LowerError!Ast.ExprId {
        const source_fn = self.pass.program.fns.items[@intFromEnum(callable.fn_id)];
        const source_body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => Common.invariant("hosted callable value needed capture substitution"),
        };

        const source_captures = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(source_fn.captures));
        defer self.pass.allocator.free(source_captures);
        if (source_captures.len != callable.captures.len) {
            Common.invariant("callable value capture count differed from lifted function capture count");
        }

        // Reuse the source function's capture local ids rather than allocating
        // fresh ones. Captures are carried implicitly by the lambda type, not
        // passed as call arguments, so a leftover direct call to the
        // un-specialized recursive callee still references the SOURCE capture
        // locals. If the specialized function bound fresh capture locals, that
        // implicit reference would point at a local never defined in the
        // specialized body, surfacing as an unbound local in the lowered LIR.
        // Args still get fresh locals below: they are always explicit and fully
        // remapped through the subst map, so they carry no implicit references.
        const captures = try self.pass.allocator.dupe(Ast.TypedLocal, source_captures);
        defer self.pass.allocator.free(captures);
        const captures_span = try self.pass.program.addTypedLocalSpan(captures);

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
        try self.pass.copyProcDebugName(source_fn.symbol, symbol);

        try self.callable_stack.append(self.pass.allocator, .{
            .source = callable.fn_id,
            .specialized = fn_id,
        });
        defer {
            const popped = self.callable_stack.pop() orelse Common.invariant("callable specialization stack underflow");
            if (popped.source != callable.fn_id or popped.specialized != fn_id) {
                Common.invariant("callable specialization stack was corrupted");
            }
        }

        const result = try self.materializeCallableWithCaptures(
            callable.ty,
            fn_id,
            captures_span,
            callable.captures,
        );

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        try self.clearBinderSubstitutionsForInline();

        for (source_captures, captures) |source_capture, capture| {
            const local_expr = try self.addExpr(.{
                .ty = capture.ty,
                .data = .{ .local = capture.local },
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

        // Build the body before writing the final function slot. The clone can
        // re-enter callable materialization for this active specialization.
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
            const value_expr = try self.materialize(value);
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

        const subst_binder = switch (value) {
            .tag,
            .record,
            .tuple,
            .nominal,
            => true,
            .expr,
            .expr_with_known_fact,
            .let_,
            .if_,
            .callable,
            => false,
        };
        if (subst_binder) if (self.pass.program.locals.items[@intFromEnum(local)].binder) |binder| {
            const previous_binder = self.binder_subst.get(binder);
            try self.changes.append(self.pass.allocator, .{
                .key = .{ .binder = binder },
                .previous = previous_binder,
            });
            try self.binder_subst.put(binder, value);
        };
    }

    fn clearBinderSubstitutionsForInline(self: *Cloner) Allocator.Error!void {
        var iter = self.binder_subst.iterator();
        while (iter.next()) |entry| {
            try self.changes.append(self.pass.allocator, .{
                .key = .{ .binder = entry.key_ptr.* },
                .previous = entry.value_ptr.*,
            });
        }
        self.binder_subst.clearRetainingCapacity();
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
                .binder => |binder| {
                    if (change.previous) |previous| {
                        self.binder_subst.putAssumeCapacity(binder, previous);
                    } else {
                        _ = self.binder_subst.remove(binder);
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
        .break_ => |maybe| if (maybe) |value| exprContainsReturn(program, value) else false,
        .continue_ => |continue_| exprSpanContainsReturn(program, continue_.values),
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
        .break_ => |maybe| if (maybe) |value| localUseCountInExpr(program, local, value) else 0,
        .continue_ => |continue_| localUseCountInExprSpan(program, local, continue_.values),
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
        .break_ => |maybe| if (maybe) |value| localMaxUseCountPerPathInExpr(program, local, value) else 0,
        .continue_ => |continue_| localMaxUseCountPerPathInExprSpan(program, local, continue_.values),
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
        .break_,
        .continue_,
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

fn localUseCountInBlockTail(program: *const Ast.Program, local: Ast.LocalId, tail: BlockTail) usize {
    var count: usize = 0;
    for (tail.statements) |stmt| count += localUseCountInStmt(program, local, stmt);
    count += localUseCountInExpr(program, local, tail.final_expr);
    return count;
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
        .break_ => |maybe| {
            if (maybe) |value| scanLocalUseInExpr(program, local, value, scan);
            scan.seen_effect = true;
        },
        .continue_ => |continue_| {
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
        .expr_with_known_fact => |known_fact_expr| known_fact_expr.expr,
        else => null,
    };
}

fn valueFromProjectedExpr(expr: Ast.ExprId, fact: ValueFact) Value {
    return switch (fact) {
        .any => .{ .expr = expr },
        else => .{ .expr_with_known_fact = .{
            .expr = expr,
            .fact = fact,
        } },
    };
}

fn fieldFactFromFact(fact: ValueFact, name: names.RecordFieldNameId) ?ValueFact {
    return switch (fact) {
        .record => |record| blk: {
            for (record.fields) |field| {
                if (field.name == name) break :blk field.fact;
            }
            break :blk null;
        },
        .nominal => |nominal| fieldFactFromFact(nominal.backing.*, name),
        else => null,
    };
}

fn itemFactFromFact(fact: ValueFact, index: u32) ?ValueFact {
    return switch (fact) {
        .tuple => |tuple| if (index < tuple.items.len) tuple.items[index] else null,
        .nominal => |nominal| itemFactFromFact(nominal.backing.*, index),
        else => null,
    };
}

fn factType(fact: ValueFact) Type.TypeId {
    return switch (fact) {
        .any => |ty| ty,
        .leaf => |ty| ty,
        .tag => |tag| tag.ty,
        .record => |record| record.ty,
        .tuple => |tuple| tuple.ty,
        .nominal => |nominal| nominal.ty,
        .callable => |callable| callable.ty,
    };
}

fn valueType(program: *const Ast.Program, value: Value) Type.TypeId {
    return switch (value) {
        .expr => |expr| program.exprs.items[@intFromEnum(expr)].ty,
        .expr_with_known_fact => |known_fact_expr| program.exprs.items[@intFromEnum(known_fact_expr.expr)].ty,
        .let_ => |let_value| valueType(program, let_value.body.*),
        .if_ => |if_value| if_value.ty,
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
        if (!factEql(program, lhs_arg, rhs_arg)) return false;
    }
    return true;
}

fn joinFactsInArena(
    program: *const Ast.Program,
    arena: Allocator,
    lhs: ValueFact,
    rhs: ValueFact,
) Allocator.Error!?ValueFact {
    if (factEql(program, lhs, rhs)) return lhs;
    if (!sameType(program, factType(lhs), factType(rhs))) return null;

    return switch (lhs) {
        .any => |ty| ValueFact{ .any = ty },
        .leaf => |ty| blk: {
            const rhs_ty = switch (rhs) {
                .leaf => |rhs_ty| rhs_ty,
                else => break :blk null,
            };
            break :blk if (sameType(program, ty, rhs_ty)) ValueFact{ .leaf = ty } else null;
        },
        .tag => |lhs_tag| blk: {
            const rhs_tag = switch (rhs) {
                .tag => |tag| tag,
                else => break :blk null,
            };
            if (lhs_tag.name != rhs_tag.name or lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk null;
            const payloads = try arena.alloc(ValueFact, lhs_tag.payloads.len);
            for (lhs_tag.payloads, rhs_tag.payloads, 0..) |lhs_payload, rhs_payload, index| {
                payloads[index] = (try joinFactsInArena(program, arena, lhs_payload, rhs_payload)) orelse
                    .{ .any = factType(lhs_payload) };
            }
            break :blk ValueFact{ .tag = .{
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
            const fields = try arena.alloc(FieldFact, lhs_record.fields.len);
            for (lhs_record.fields, rhs_record.fields, 0..) |lhs_field, rhs_field, index| {
                if (lhs_field.name != rhs_field.name) break :blk null;
                fields[index] = .{
                    .name = lhs_field.name,
                    .fact = (try joinFactsInArena(program, arena, lhs_field.fact, rhs_field.fact)) orelse
                        .{ .any = factType(lhs_field.fact) },
                };
            }
            break :blk ValueFact{ .record = .{
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
            const items = try arena.alloc(ValueFact, lhs_tuple.items.len);
            for (lhs_tuple.items, rhs_tuple.items, 0..) |lhs_item, rhs_item, index| {
                items[index] = (try joinFactsInArena(program, arena, lhs_item, rhs_item)) orelse
                    .{ .any = factType(lhs_item) };
            }
            break :blk ValueFact{ .tuple = .{
                .ty = lhs_tuple.ty,
                .items = items,
            } };
        },
        .nominal => |lhs_nominal| blk: {
            const rhs_nominal = switch (rhs) {
                .nominal => |nominal| nominal,
                else => break :blk null,
            };
            const backing = (try joinFactsInArena(program, arena, lhs_nominal.backing.*, rhs_nominal.backing.*)) orelse break :blk null;
            const stored = try arena.create(ValueFact);
            stored.* = backing;
            break :blk ValueFact{ .nominal = .{
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
            const captures = try arena.alloc(ValueFact, lhs_callable.captures.len);
            for (lhs_callable.captures, rhs_callable.captures, 0..) |lhs_capture, rhs_capture, index| {
                captures[index] = (try joinFactsInArena(program, arena, lhs_capture, rhs_capture)) orelse
                    .{ .any = factType(lhs_capture) };
            }
            break :blk ValueFact{ .callable = .{
                .ty = lhs_callable.ty,
                .fn_id = lhs_callable.fn_id,
                .captures = captures,
            } };
        },
    };
}

fn factsStrictlyDescend(program: *const Ast.Program, active: []const ValueFact, next: []const ValueFact) bool {
    if (active.len != next.len) return false;
    var descended = false;
    for (active, next) |active_fact, next_fact| {
        if (factEql(program, active_fact, next_fact)) continue;
        if (!factContainsStrictSubfact(program, active_fact, next_fact)) return false;
        descended = true;
    }
    return descended;
}

fn factContainsStrictSubfact(program: *const Ast.Program, container: ValueFact, needle: ValueFact) bool {
    return switch (container) {
        .any => false,
        .leaf => false,
        .tag => |tag| {
            for (tag.payloads) |payload| {
                if (factEql(program, payload, needle) or factContainsStrictSubfact(program, payload, needle)) return true;
            }
            return false;
        },
        .record => |record| {
            for (record.fields) |field| {
                if (factEql(program, field.fact, needle) or factContainsStrictSubfact(program, field.fact, needle)) return true;
            }
            return false;
        },
        .tuple => |tuple| {
            for (tuple.items) |item| {
                if (factEql(program, item, needle) or factContainsStrictSubfact(program, item, needle)) return true;
            }
            return false;
        },
        .nominal => |nominal| {
            return factEql(program, nominal.backing.*, needle) or factContainsStrictSubfact(program, nominal.backing.*, needle);
        },
        .callable => |callable| {
            for (callable.captures) |capture| {
                if (factEql(program, capture, needle) or factContainsStrictSubfact(program, capture, needle)) return true;
            }
            return false;
        },
    };
}

fn factEql(program: *const Ast.Program, lhs: ValueFact, rhs: ValueFact) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .any => |lhs_ty| sameType(program, lhs_ty, rhs.any),
        .leaf => |lhs_ty| sameType(program, lhs_ty, rhs.leaf),
        .tag => |lhs_tag| blk: {
            const rhs_tag = rhs.tag;
            if (!sameType(program, lhs_tag.ty, rhs_tag.ty) or lhs_tag.name != rhs_tag.name or lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk false;
            for (lhs_tag.payloads, rhs_tag.payloads) |lhs_payload, rhs_payload| {
                if (!factEql(program, lhs_payload, rhs_payload)) break :blk false;
            }
            break :blk true;
        },
        .record => |lhs_record| blk: {
            const rhs_record = rhs.record;
            if (!sameType(program, lhs_record.ty, rhs_record.ty) or lhs_record.fields.len != rhs_record.fields.len) break :blk false;
            for (lhs_record.fields, rhs_record.fields) |lhs_field, rhs_field| {
                if (lhs_field.name != rhs_field.name or !factEql(program, lhs_field.fact, rhs_field.fact)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |lhs_tuple| blk: {
            const rhs_tuple = rhs.tuple;
            if (!sameType(program, lhs_tuple.ty, rhs_tuple.ty) or lhs_tuple.items.len != rhs_tuple.items.len) break :blk false;
            for (lhs_tuple.items, rhs_tuple.items) |lhs_item, rhs_item| {
                if (!factEql(program, lhs_item, rhs_item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |lhs_nominal| {
            const rhs_nominal = rhs.nominal;
            return sameType(program, lhs_nominal.ty, rhs_nominal.ty) and factEql(program, lhs_nominal.backing.*, rhs_nominal.backing.*);
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
                if (!factEql(program, lhs_capture, rhs_capture)) break :blk false;
            }
            break :blk true;
        },
    };
}

fn factMatchesValue(program: *const Ast.Program, fact: ValueFact, value: Value) bool {
    if (value == .expr_with_known_fact) {
        if (fact == .any) return true;
        if (fact == .leaf) return sameType(program, fact.leaf, valueType(program, value));
        if (!canReadFieldsFromExpr(program, value.expr_with_known_fact.expr)) return false;
        return factCanProjectFromExpr(fact) and factMatchesFact(program, fact, value.expr_with_known_fact.fact);
    }

    return switch (fact) {
        .any => true,
        .leaf => |ty| sameType(program, ty, valueType(program, value)),
        .tag => |tag| blk: {
            const value_tag = switch (value) {
                .tag => |value_tag| value_tag,
                else => break :blk false,
            };
            if (!sameType(program, tag.ty, value_tag.ty) or tag.name != value_tag.name or tag.payloads.len != value_tag.payloads.len) break :blk false;
            for (tag.payloads, value_tag.payloads) |payload_fact, payload_value| {
                if (!factMatchesValue(program, payload_fact, payload_value)) break :blk false;
            }
            break :blk true;
        },
        .record => |record| blk: {
            const value_record = switch (value) {
                .record => |value_record| value_record,
                else => break :blk false,
            };
            if (!sameType(program, record.ty, value_record.ty) or record.fields.len != value_record.fields.len) break :blk false;
            for (record.fields, value_record.fields) |field_fact, field_value| {
                if (field_fact.name != field_value.name or !factMatchesValue(program, field_fact.fact, field_value.value)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |tuple| blk: {
            const value_tuple = switch (value) {
                .tuple => |value_tuple| value_tuple,
                else => break :blk false,
            };
            if (!sameType(program, tuple.ty, value_tuple.ty) or tuple.items.len != value_tuple.items.len) break :blk false;
            for (tuple.items, value_tuple.items) |item_fact, item_value| {
                if (!factMatchesValue(program, item_fact, item_value)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |nominal| blk: {
            const value_nominal = switch (value) {
                .nominal => |value_nominal| value_nominal,
                else => break :blk false,
            };
            break :blk sameType(program, nominal.ty, value_nominal.ty) and factMatchesValue(program, nominal.backing.*, value_nominal.backing.*);
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
            for (callable.captures, value_callable.captures) |capture_fact, capture_value| {
                if (!factMatchesValue(program, capture_fact, capture_value)) break :blk false;
            }
            break :blk true;
        },
    };
}

fn factCanProjectFromExpr(fact: ValueFact) bool {
    return switch (fact) {
        .any => true,
        .leaf => true,
        .record => |record| blk: {
            for (record.fields) |field| {
                if (!factCanProjectFromExpr(field.fact)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |tuple| blk: {
            for (tuple.items) |item| {
                if (!factCanProjectFromExpr(item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |nominal| factCanProjectFromExpr(nominal.backing.*),
        .tag,
        .callable,
        => false,
    };
}

fn factMatchesFact(program: *const Ast.Program, pattern: ValueFact, actual: ValueFact) bool {
    return switch (pattern) {
        .any => true,
        .leaf => |ty| sameType(program, ty, factType(actual)),
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
                if (!factMatchesFact(program, pattern_payload, actual_payload)) break :blk false;
            }
            break :blk true;
        },
        .record => |pattern_record| blk: {
            const actual_record = switch (actual) {
                .record => |record| record,
                else => break :blk false,
            };
            if (!sameType(program, pattern_record.ty, actual_record.ty) or pattern_record.fields.len != actual_record.fields.len) break :blk false;
            for (pattern_record.fields, actual_record.fields) |pattern_field, actual_field| {
                if (pattern_field.name != actual_field.name or !factMatchesFact(program, pattern_field.fact, actual_field.fact)) break :blk false;
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
                if (!factMatchesFact(program, pattern_item, actual_item)) break :blk false;
            }
            break :blk true;
        },
        .nominal => |pattern_nominal| blk: {
            const actual_nominal = switch (actual) {
                .nominal => |nominal| nominal,
                else => break :blk false,
            };
            break :blk sameType(program, pattern_nominal.ty, actual_nominal.ty) and
                factMatchesFact(program, pattern_nominal.backing.*, actual_nominal.backing.*);
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
                if (!factMatchesFact(program, pattern_capture, actual_capture)) break :blk false;
            }
            break :blk true;
        },
    };
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

test "call-pattern specialization declarations are referenced" {
    std.testing.refAllDecls(@This());
}
