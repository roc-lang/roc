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
//! over a single stream value, repacking stream fields and rebuilding the step
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
    tag: TagValue,
    record: RecordValue,
    tuple: TupleValue,
    nominal: NominalValue,
    callable: CallableValue,
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

/// A cloned capture operand carried on a callable value: its exact
/// CaptureId plus the cloned supplying value. Keeping the id lets materialization
/// pair operands to the (possibly specialized) target's sorted capture slots by
/// id rather than by position.
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
};

const LoopPattern = struct {
    values: []const Shape,
};

const ActiveCallable = struct {
    source: Ast.FnId,
    specialized: Ast.FnId,
};

const Pass = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    program: *Ast.Program,
    plans: []FnPlan,
    symbols: Common.SymbolGen,

    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Pass {
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
            .plans = plans,
            .symbols = .{ .next = program.next_symbol },
        };
    }

    fn deinit(self: *Pass) void {
        for (self.plans) |*plan| plan.deinit(self.allocator);
        self.allocator.free(self.plans);
        self.arena.deinit();
    }

    fn run(self: *Pass) Common.LowerError!void {
        const original_fn_count = self.plans.len;

        try self.collectArgUses(original_fn_count);
        try self.collectCallPatterns(original_fn_count);
        try self.reserveSpecIds();
        try self.createSpecializations(original_fn_count);
        try self.rewriteExistingCalls();
        try Lift.recomputeCaptures(self.allocator, self.program);

        self.program.next_symbol = self.symbols.next;
    }

    fn copyProcDebugName(self: *Pass, source_symbol: Common.Symbol, target_symbol: Common.Symbol) Allocator.Error!void {
        if (self.program.procDebugName(source_symbol)) |name| {
            try self.program.setProcDebugName(target_symbol, name);
        }
    }

    fn collectArgUses(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        // This loop only reads functions; it must not append to `program.fns`,
        // whose reallocation would dangle the slice it iterates. Assert that in
        // debug builds; the check compiles out of release builds.
        const fns_base = self.program.fns.items.ptr;
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
        if (@import("builtin").mode == .Debug) {
            std.debug.assert(self.program.fns.items.ptr == fns_base);
        }
    }

    fn collectCallPatterns(self: *Pass, original_fn_count: usize) Allocator.Error!void {
        // Collecting a pattern materializes specialized callables, which appends
        // to `program.fns` and can reallocate it. Re-read `items` by index each
        // step rather than iterate a slice captured before the loop.
        var index: usize = 0;
        while (index < original_fn_count) : (index += 1) {
            const fn_ = self.program.fns.items[index];
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.collectCallPatternsInExpr(fn_id, body);
        }
    }

    fn reserveSpecIds(self: *Pass) Allocator.Error!void {
        for (self.plans, 0..) |*plan, source_index| {
            const source_fn = self.program.fns.items[source_index];
            for (plan.specs.items) |*spec| {
                const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.program.fns.items.len)));
                const symbol = self.symbols.fresh();
                spec.fn_id = fn_id;
                try self.program.fns.append(self.allocator, .{
                    .symbol = symbol,
                    .source = source_fn.source,
                    .args = .empty(),
                    .captures = source_fn.captures,
                    .body = .hosted,
                    .ret = source_fn.ret,
                });
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
        const expr = self.program.exprs.items[@intFromEnum(expr_id)];
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
                for (self.program.captureOperandSpan(fn_ref.captures)) |operand| try self.markArgUsesInExpr(fn_id, operand.value, changed);
            },
            .list,
            .tuple,
            => |items| for (self.program.exprSpan(items)) |child| try self.markArgUsesInExpr(fn_id, child, changed),
            .record => |fields| for (self.program.fieldExprSpan(fields)) |field| try self.markArgUsesInExpr(fn_id, field.value, changed),
            .tag => |tag| for (self.program.exprSpan(tag.payloads)) |payload| try self.markArgUsesInExpr(fn_id, payload, changed),
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
                for (self.program.exprSpan(call.args)) |arg| try self.markArgUsesInExpr(fn_id, arg, changed);
            },
            .call_proc => |call| {
                const args = self.program.exprSpan(call.args);
                for (args) |arg| try self.markArgUsesInExpr(fn_id, arg, changed);
                for (self.program.captureOperandSpan(call.captures)) |operand| try self.markArgUsesInExpr(fn_id, operand.value, changed);
                const callee = Ast.localDirectCallee(call) orelse return;
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
            => |expr| try self.markArgUsesInExpr(fn_id, expr, changed),
            .return_ => |ret| try self.markArgUsesInExpr(fn_id, ret.value, changed),
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

    fn collectCallPatternsInExpr(self: *Pass, owner: Ast.FnId, expr_id: Ast.ExprId) Allocator.Error!void {
        const expr = self.program.exprs.items[@intFromEnum(expr_id)];
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
        const source = try self.allocator.dupe(Ast.ExprId, self.program.exprSpan(span));
        defer self.allocator.free(source);
        for (source) |expr| try self.collectCallPatternsInExpr(owner, expr);
    }

    fn collectCallPatternsInCaptureOperandSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.CaptureOperand, self.program.captureOperandSpan(span));
        defer self.allocator.free(source);
        for (source) |operand| try self.collectCallPatternsInExpr(owner, operand.value);
    }

    fn collectCallPatternsInFieldExprSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.FieldExpr, self.program.fieldExprSpan(span));
        defer self.allocator.free(source);
        for (source) |field| try self.collectCallPatternsInExpr(owner, field.value);
    }

    fn collectCallPatternsInBranchSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.Branch)) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.Branch, self.program.branchSpan(span));
        defer self.allocator.free(source);
        for (source) |branch| {
            if (branch.guard) |guard| try self.collectCallPatternsInExpr(owner, guard);
            try self.collectCallPatternsInExpr(owner, branch.body);
        }
    }

    fn collectCallPatternsInIfBranchSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.IfBranch)) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.IfBranch, self.program.ifBranchSpan(span));
        defer self.allocator.free(source);
        for (source) |branch| {
            try self.collectCallPatternsInExpr(owner, branch.cond);
            try self.collectCallPatternsInExpr(owner, branch.body);
        }
    }

    fn collectCallPatternsInStmtSpan(self: *Pass, owner: Ast.FnId, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.StmtId, self.program.stmtSpan(span));
        defer self.allocator.free(source);
        for (source) |stmt| try self.collectCallPatternsInStmt(owner, stmt);
    }

    fn collectCallPatternsInStmt(self: *Pass, owner: Ast.FnId, stmt_id: Ast.StmtId) Allocator.Error!void {
        switch (self.program.stmts.items[@intFromEnum(stmt_id)]) {
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
        const args = try self.allocator.dupe(Ast.ExprId, self.program.exprSpan(args_span));
        defer self.allocator.free(args);
        const fn_args = self.program.typedLocalSpan(self.program.fns.items[raw].args);
        if (args.len != fn_args.len) Common.invariant("direct call arity differed from lifted function arity");

        const shapes = try self.arena.allocator().alloc(Shape, args.len);
        var has_constructor = false;

        for (args, 0..) |arg, index| {
            if (self.plans[raw].used_args[index]) {
                var cloner = Cloner.initForRewrite(self);
                defer cloner.deinit();
                const value = try cloner.cloneExprValue(arg);
                if (try self.shapeFromValue(value)) |shape| {
                    shapes[index] = shape;
                    has_constructor = true;
                    continue;
                }
            }
            shapes[index] = .{ .any = self.program.exprs.items[@intFromEnum(arg)].ty };
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

        const fn_args = self.program.typedLocalSpan(self.program.fns.items[raw].args);
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

        const source_fn = self.program.fns.items[raw];
        const fn_id_reserved: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.program.fns.items.len)));
        const symbol = self.symbols.fresh();
        try self.plans[raw].specs.append(self.allocator, .{
            .pattern = pattern,
            .fn_id = fn_id_reserved,
        });
        try self.program.fns.append(self.allocator, .{
            .symbol = symbol,
            .source = source_fn.source,
            .args = .empty(),
            .captures = source_fn.captures,
            .body = .hosted,
            .ret = source_fn.ret,
        });
        try self.copyProcDebugName(source_fn.symbol, symbol);
    }

    fn writeSpecialization(self: *Pass, source_fn_id: Ast.FnId, spec_index: usize) Common.LowerError!void {
        const source_fn = self.program.fns.items[@intFromEnum(source_fn_id)];
        const spec = &self.plans[@intFromEnum(source_fn_id)].specs.items[spec_index];

        const spec_fn_id = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before cloning");
        const symbol = self.program.fns.items[@intFromEnum(spec_fn_id)].symbol;

        var cloner = Cloner.init(self, source_fn_id, spec.pattern);
        defer cloner.deinit();

        try cloner.inline_stack.append(self.allocator, source_fn_id);
        defer {
            const popped = cloner.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow while writing specialization");
            if (popped != source_fn_id) Common.invariant("call-pattern inline stack was corrupted while writing specialization");
        }

        const args = try cloner.buildArgs();
        const body: Ast.FnBody = switch (source_fn.body) {
            .roc => |body_expr| .{ .roc = try cloner.cloneExpr(body_expr) },
            .hosted => Common.invariant("hosted function had a call-pattern specialization"),
        };

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

    fn rewriteExistingCalls(self: *Pass) Allocator.Error!void {
        const done = try self.allocator.alloc(bool, self.program.exprs.items.len);
        defer self.allocator.free(done);
        @memset(done, false);

        // This loop only reads functions; it must not append to `program.fns`,
        // whose reallocation would dangle the slice it iterates. Assert that in
        // debug builds; the check compiles out of release builds.
        const fns_base = self.program.fns.items.ptr;
        const fn_count = self.program.fns.items.len;
        for (self.program.fns.items[0..fn_count]) |fn_| {
            const body = switch (fn_.body) {
                .roc => |body| body,
                .hosted => continue,
            };
            try self.rewriteCallsInExpr(body, done);
        }
        if (@import("builtin").mode == .Debug) {
            std.debug.assert(self.program.fns.items.ptr == fns_base);
        }
    }

    fn rewriteCallsInExpr(self: *Pass, expr_id: Ast.ExprId, done: []bool) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (done[index]) return;
        done[index] = true;

        const expr = self.program.exprs.items[index];
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
        const source = try self.allocator.dupe(Ast.ExprId, self.program.exprSpan(span));
        defer self.allocator.free(source);
        for (source) |expr| try self.rewriteCallsInExpr(expr, done);
    }

    fn rewriteCallsInCaptureOperandSpan(self: *Pass, span: Ast.Span(Ast.CaptureOperand), done: []bool) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.CaptureOperand, self.program.captureOperandSpan(span));
        defer self.allocator.free(source);
        for (source) |operand| try self.rewriteCallsInExpr(operand.value, done);
    }

    fn rewriteCallsInFieldExprSpan(self: *Pass, span: Ast.Span(Ast.FieldExpr), done: []bool) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.FieldExpr, self.program.fieldExprSpan(span));
        defer self.allocator.free(source);
        for (source) |field| try self.rewriteCallsInExpr(field.value, done);
    }

    fn rewriteCallsInBranchSpan(self: *Pass, span: Ast.Span(Ast.Branch), done: []bool) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.Branch, self.program.branchSpan(span));
        defer self.allocator.free(source);
        for (source) |branch| {
            if (branch.guard) |guard| try self.rewriteCallsInExpr(guard, done);
            try self.rewriteCallsInExpr(branch.body, done);
        }
    }

    fn rewriteCallsInIfBranchSpan(self: *Pass, span: Ast.Span(Ast.IfBranch), done: []bool) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.IfBranch, self.program.ifBranchSpan(span));
        defer self.allocator.free(source);
        for (source) |branch| {
            try self.rewriteCallsInExpr(branch.cond, done);
            try self.rewriteCallsInExpr(branch.body, done);
        }
    }

    fn rewriteCallsInStmtSpan(self: *Pass, span: Ast.Span(Ast.StmtId), done: []bool) Allocator.Error!void {
        const source = try self.allocator.dupe(Ast.StmtId, self.program.stmtSpan(span));
        defer self.allocator.free(source);
        for (source) |stmt| try self.rewriteCallsInStmt(stmt, done);
    }

    fn rewriteCallsInStmt(self: *Pass, stmt_id: Ast.StmtId, done: []bool) Allocator.Error!void {
        switch (self.program.stmts.items[@intFromEnum(stmt_id)]) {
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

        const args = try self.allocator.dupe(Ast.ExprId, self.program.exprSpan(call.args));
        defer self.allocator.free(args);
        for (self.plans[raw].specs.items) |spec| {
            var rewritten_args = std.ArrayList(Ast.ExprId).empty;
            defer rewritten_args.deinit(self.allocator);

            if (try self.appendExistingCallArgs(spec.pattern, args, &rewritten_args)) {
                self.program.exprs.items[@intFromEnum(expr_id)].data = .{ .call_proc = .{
                    .callee = .{ .lifted = spec.fn_id orelse Common.invariant("call-pattern specialization id was not assigned before rewriting") },
                    .args = try self.program.addExprSpan(rewritten_args.items),
                    .captures = call.captures,
                    .is_cold = call.is_cold,
                } };
                return;
            }
        }
    }

    fn appendExistingCallArgs(
        self: *Pass,
        pattern: CallPattern,
        args: []const Ast.ExprId,
        out: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!bool {
        if (pattern.args.len != args.len) Common.invariant("call-pattern arity differed from direct call arity");
        var cloner = Cloner.initForRewrite(self);
        defer cloner.deinit();

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
                const expr = self.program.exprs.items[@intFromEnum(expr_id)];
                const expr_tag = switch (expr.data) {
                    .tag => |expr_tag| expr_tag,
                    else => return false,
                };
                if (!sameType(self.program, expr.ty, tag.ty) or expr_tag.name != tag.name) return false;
                const payloads = self.program.exprSpan(expr_tag.payloads);
                if (payloads.len != tag.payloads.len) Common.invariant("tag call pattern arity differed from tag expression arity");
                for (tag.payloads, payloads) |payload_shape, payload| {
                    if (!try self.appendExistingExprsForShape(payload_shape, payload, out)) return false;
                }
                return true;
            },
            .record => |record| {
                const expr = self.program.exprs.items[@intFromEnum(expr_id)];
                const fields = switch (expr.data) {
                    .record => |fields| self.program.fieldExprSpan(fields),
                    else => return false,
                };
                if (!sameType(self.program, expr.ty, record.ty) or fields.len != record.fields.len) return false;
                for (record.fields, fields) |field_shape, field| {
                    if (field_shape.name != field.name) return false;
                    if (!try self.appendExistingExprsForShape(field_shape.shape, field.value, out)) return false;
                }
                return true;
            },
            .tuple => |tuple| {
                const expr = self.program.exprs.items[@intFromEnum(expr_id)];
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
                const expr = self.program.exprs.items[@intFromEnum(expr_id)];
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
        const expr = self.program.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .tag => |tag| blk: {
                const payloads = self.program.exprSpan(tag.payloads);
                const shapes = try self.arena.allocator().alloc(Shape, payloads.len);
                for (payloads, 0..) |payload, index| {
                    shapes[index] = (try self.constructorShape(payload)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(payload)].ty };
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
                for (fields, 0..) |field, index| {
                    shapes[index] = .{
                        .name = field.name,
                        .shape = (try self.constructorShape(field.value)) orelse
                            .{ .any = self.program.exprs.items[@intFromEnum(field.value)].ty },
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
                for (items, 0..) |item, index| {
                    shapes[index] = (try self.constructorShape(item)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(item)].ty };
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
                for (capture_operands, 0..) |operand, index| {
                    capture_shapes[index] = (try self.constructorShape(operand.value)) orelse
                        .{ .any = self.program.exprs.items[@intFromEnum(operand.value)].ty };
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

    fn shapeFromValue(self: *Pass, value: Value) Allocator.Error!?Shape {
        return switch (value) {
            .expr => |expr| try self.constructorShape(expr),
            .tag => |tag| blk: {
                const payloads = try self.arena.allocator().alloc(Shape, tag.payloads.len);
                for (tag.payloads, 0..) |payload, index| {
                    payloads[index] = (try self.shapeFromValue(payload)) orelse
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
                        .shape = (try self.shapeFromValue(field.value)) orelse
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
                    items[index] = (try self.shapeFromValue(item)) orelse
                        .{ .any = valueType(self.program, item) };
                }
                break :blk Shape{ .tuple = .{
                    .ty = tuple.ty,
                    .items = items,
                } };
            },
            .nominal => |nominal| blk: {
                const backing_shape = (try self.shapeFromValue(nominal.backing.*)) orelse break :blk null;
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
                    captures[index] = (try self.shapeFromValue(capture.value)) orelse
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
    binder_subst: std.AutoHashMap(check.CheckedModule.PatternBinderId, Value),
    changes: std.ArrayList(BindingChange),
    inline_stack: std.ArrayList(Ast.FnId),
    callable_stack: std.ArrayList(ActiveCallable),
    loop_stack: std.ArrayList(LoopPattern),
    inline_direct_calls: bool,
    inline_direct_requires_known_arg: bool,
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
            .binder_subst = std.AutoHashMap(check.CheckedModule.PatternBinderId, Value).init(pass.allocator),
            .changes = .empty,
            .inline_stack = .empty,
            .callable_stack = .empty,
            .loop_stack = .empty,
            .inline_direct_calls = true,
            .inline_direct_requires_known_arg = false,
            .current_loc = SourceLoc.none,
            .current_region = Region.zero(),
        };
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
                const slots = self.pass.program.typedLocalSpan(self.pass.program.fns.items[@intFromEnum(callable.fn_id)].captures);
                if (slots.len != callable.captures.len) {
                    Common.invariant("callable shape capture count differed from its function capture slots");
                }
                const captures = try self.pass.arena.allocator().alloc(CaptureValue, callable.captures.len);
                for (callable.captures, slots, 0..) |capture, slot, index| {
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
        return try self.materialize(try self.cloneExprValue(expr_id));
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

        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local => |local| {
                if (self.subst.get(local)) |value| return value;
                if (self.pass.program.locals.items[@intFromEnum(local)].binder) |binder| {
                    if (self.binder_subst.get(binder)) |value| return value;
                }
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } }) };
            },
            .fn_ref => |fn_ref| return try self.callableValueFromRef(expr.ty, fn_ref),
            .tag => |tag| {
                const payload_exprs = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(tag.payloads));
                defer self.pass.allocator.free(payload_exprs);
                const payloads = try self.pass.arena.allocator().alloc(Value, payload_exprs.len);
                for (payload_exprs, 0..) |payload, index| {
                    payloads[index] = try self.cloneExprValue(payload);
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
                        .value = try self.cloneExprValue(field.value),
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
                    items[index] = try self.cloneExprValue(item);
                }
                return .{ .tuple = .{
                    .ty = expr.ty,
                    .items = items,
                } };
            },
            .nominal => |backing| {
                const backing_value = try self.cloneExprValue(backing);
                return .{ .nominal = .{
                    .ty = expr.ty,
                    .backing = try self.copyValue(backing_value),
                } };
            },
            .let_ => |let_| return try self.cloneLetValue(let_),
            .field_access => |field| {
                const receiver = try self.cloneExprValue(field.receiver);
                if (fieldFromValue(receiver, field.field)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .field_access = .{
                    .receiver = try self.materialize(receiver),
                    .field = field.field,
                } } }) };
            },
            .tuple_access => |access| {
                const receiver = try self.cloneExprValue(access.tuple);
                if (itemFromValue(receiver, access.elem_index)) |value| return value;
                return .{ .expr = try self.addExpr(.{ .ty = expr.ty, .data = .{ .tuple_access = .{
                    .tuple = try self.materialize(receiver),
                    .elem_index = access.elem_index,
                } } }) };
            },
            .match_ => |match| {
                const scrutinee = try self.cloneExprValue(match.scrutinee);
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
                const callee = try self.cloneExprValue(call.callee);
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
                const has_known_shape_arg = try self.directCallHasKnownShapeArg(call.args);
                if (self.inline_direct_requires_known_arg and !has_known_shape_arg) {
                    return .{ .expr = try self.cloneExprPlain(expr_id) };
                }
                const callee = Ast.localDirectCallee(call) orelse return .{ .expr = try self.cloneExprPlain(expr_id) };
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

    fn directCallHasKnownShapeArg(self: *Cloner, args_span: Ast.Span(Ast.ExprId)) Allocator.Error!bool {
        for (self.pass.program.exprSpan(args_span)) |arg| {
            if (try self.exprHasKnownShape(arg)) return true;
        }
        return false;
    }

    fn exprHasKnownShape(self: *Cloner, expr_id: Ast.ExprId) Allocator.Error!bool {
        const expr = self.pass.program.exprs.items[@intFromEnum(expr_id)];
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
            .field_access => |field| blk: {
                const receiver_local = localExpr(self.pass.program, field.receiver) orelse break :blk false;
                const receiver = self.subst.get(receiver_local) orelse break :blk false;
                const value = fieldFromValue(receiver, field.field) orelse break :blk false;
                break :blk (try self.pass.shapeFromValue(value)) != null;
            },
            .tuple_access => |access| blk: {
                const tuple_local = localExpr(self.pass.program, access.tuple) orelse break :blk false;
                const tuple = self.subst.get(tuple_local) orelse break :blk false;
                const value = itemFromValue(tuple, access.elem_index) orelse break :blk false;
                break :blk (try self.pass.shapeFromValue(value)) != null;
            },
            .comptime_branch_taken => |taken| try self.exprHasKnownShape(taken.body),
            .comptime_exhaustiveness_failed => false,
            else => false,
        };
    }

    fn valueCanSubstitute(self: *Cloner, value: Value) bool {
        return switch (value) {
            .expr => |expr| self.exprCanSubstitute(expr),
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
                    if (!self.valueCanSubstitute(capture.value)) break :blk false;
                }
                break :blk true;
            },
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
            .bytes_lit,
            => true,
            .fn_ref => |fn_ref| self.captureOperandSpanCanSubstitute(fn_ref.captures),
            .field_access => |field| self.exprCanSubstitute(field.receiver),
            .tuple_access => |access| self.exprCanSubstitute(access.tuple),
            else => false,
        };
    }

    fn captureOperandSpanCanSubstitute(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) bool {
        for (self.pass.program.captureOperandSpan(span)) |operand| {
            if (!self.exprCanSubstitute(operand.value)) return false;
        }
        return true;
    }

    fn callableValueFromRef(self: *Cloner, ty: Type.TypeId, fn_ref: @import("../monotype/ast.zig").LiftedFunctionValue) Common.LowerError!Value {
        const source_operands = self.pass.program.captureOperandSpan(fn_ref.captures);
        const captures = try self.pass.arena.allocator().alloc(CaptureValue, source_operands.len);
        for (source_operands, 0..) |operand, index| {
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
            .bytes_lit => |value| .{ .bytes_lit = value },
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
            .loop_ => |loop| return try self.cloneLoop(expr.ty, loop),
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.cloneExpr(value) else null },
            .continue_ => |continue_| try self.cloneContinue(continue_),
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

    fn cloneLetValue(self: *Cloner, let_: anytype) Common.LowerError!Value {
        const value = try self.cloneExprValue(let_.value);
        const value_expr = try self.materialize(value);
        const change_start = self.changes.items.len;
        const bound = try self.bindPatToReusableValue(let_.bind, value);
        if (bound) {
            const rest = try self.cloneExprValue(let_.rest);
            self.restore(change_start);
            return rest;
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
        } else blk: {
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

                const statements = try self.pass.allocator.alloc(Ast.StmtId, source.len);
                defer self.pass.allocator.free(statements);
                for (source, 0..) |stmt, index| {
                    statements[index] = try self.cloneStmt(stmt);
                }

                const final_value = try self.cloneExprValue(block.final_expr);
                const rest_ty = self.pass.program.exprs.items[@intFromEnum(let_.rest)].ty;
                if (!try self.bindPatToReusableValue(let_.bind, final_value)) {
                    if (try self.cloneDivergentAtType(block.final_expr, rest_ty)) |divergent| {
                        self.restore(change_start);
                        return try self.addExpr(.{ .ty = rest_ty, .data = .{ .block = .{
                            .statements = try self.pass.program.addStmtSpan(statements),
                            .final_expr = divergent,
                        } } });
                    }
                    self.restore(change_start);
                    return null;
                }

                const rest = try self.cloneExpr(let_.rest);
                self.restore(change_start);

                return try self.addExpr(.{ .ty = rest_ty, .data = .{ .block = .{
                    .statements = try self.pass.program.addStmtSpan(statements),
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
            .return_ => |ret| try self.addExpr(.{ .ty = ty, .data = .{ .return_ = .{
                .value = try self.cloneExpr(ret.value),
                .target = ret.target,
            } } }),
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
        const shapes = try self.pass.arena.allocator().alloc(Shape, initial_values.len);
        var has_constructor = false;
        for (initial_values, 0..) |initial, index| {
            values[index] = try self.cloneExprValue(initial);
            if (try self.pass.shapeFromValue(values[index])) |shape| {
                shapes[index] = shape;
                has_constructor = true;
            } else {
                shapes[index] = .{ .any = valueType(self.pass.program, values[index]) };
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

        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        var new_params = std.ArrayList(Ast.TypedLocal).empty;
        defer new_params.deinit(self.pass.allocator);

        var new_initials = std.ArrayList(Ast.ExprId).empty;
        defer new_initials.deinit(self.pass.allocator);

        for (params, shapes, values) |param, shape, value| {
            const param_value = try self.valueFromShapeArgs(shape, &new_params);
            try self.putSubst(param.local, param_value);
            try self.appendExprsFromValue(shape, value, &new_initials);
        }

        try self.loop_stack.append(self.pass.allocator, .{ .values = shapes });
        defer _ = self.loop_stack.pop();

        return try self.addExpr(.{ .ty = ty, .data = .{ .loop_ = .{
            .params = try self.pass.program.addTypedLocalSpan(new_params.items),
            .initial_values = try self.pass.program.addExprSpan(new_initials.items),
            .body = try self.cloneExpr(loop.body),
        } } });
    }

    fn cloneBlock(self: *Cloner, ty: Type.TypeId, block: anytype) Common.LowerError!Ast.ExprId {
        const change_start = self.changes.items.len;
        defer self.restore(change_start);

        const source = try self.pass.allocator.dupe(Ast.StmtId, self.pass.program.stmtSpan(block.statements));
        defer self.pass.allocator.free(source);

        const statements = try self.pass.allocator.alloc(Ast.StmtId, source.len);
        defer self.pass.allocator.free(statements);
        for (source, 0..) |stmt, index| {
            statements[index] = try self.cloneStmt(stmt);
        }

        return try self.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try self.pass.program.addStmtSpan(statements),
            .final_expr = try self.cloneExpr(block.final_expr),
        } } });
    }

    fn cloneContinue(self: *Cloner, continue_: anytype) Common.LowerError!Ast.ExprData {
        const loop = self.loop_stack.getLastOrNull() orelse return .{ .continue_ = .{
            .values = try self.cloneExprSpan(continue_.values),
        } };
        const values = self.pass.program.exprSpan(continue_.values);
        const source_values = try self.pass.allocator.dupe(Ast.ExprId, values);
        defer self.pass.allocator.free(source_values);
        if (source_values.len != loop.values.len) Common.invariant("continue value count differed from specialized loop pattern");

        var new_values = std.ArrayList(Ast.ExprId).empty;
        defer new_values.deinit(self.pass.allocator);

        for (loop.values, source_values) |shape, value_expr| {
            const value = try self.cloneExprValue(value_expr);
            if (!shapeMatchesValue(self.pass.program, shape, value)) {
                if (!try self.appendFieldReadExprsFromValue(shape, value, &new_values)) {
                    Common.invariant("continue value did not match specialized loop state");
                }
                continue;
            }
            try self.appendExprsFromValue(shape, value, &new_values);
        }

        return .{ .continue_ = .{
            .values = try self.pass.program.addExprSpan(new_values.items),
        } };
    }

    fn valuesToExprSpan(self: *Cloner, values: []const Value) Common.LowerError!Ast.Span(Ast.ExprId) {
        const exprs = try self.pass.allocator.alloc(Ast.ExprId, values.len);
        defer self.pass.allocator.free(exprs);
        for (values, 0..) |value, index| {
            exprs[index] = try self.materialize(value);
        }
        return try self.pass.program.addExprSpan(exprs);
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
            .is_cold = call.is_cold,
        } };
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
        return try self.cloneExprValue(expr_id);
    }

    fn appendExprsFromValue(
        self: *Cloner,
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!void {
        switch (shape) {
            .any => try out.append(self.pass.allocator, try self.materialize(value)),
            .tag => |tag| {
                const tag_value = switch (value) {
                    .tag => |tag_value| tag_value,
                    else => Common.invariant("tag call pattern matched a non-tag value"),
                };
                for (tag.payloads, tag_value.payloads) |payload_shape, payload| {
                    try self.appendExprsFromValue(payload_shape, payload, out);
                }
            },
            .record => |record| {
                const record_value = switch (value) {
                    .record => |record_value| record_value,
                    else => Common.invariant("record call pattern matched a non-record value"),
                };
                for (record.fields, record_value.fields) |field_shape, field| {
                    if (field_shape.name != field.name) Common.invariant("record call-pattern field order changed after matching");
                    try self.appendExprsFromValue(field_shape.shape, field.value, out);
                }
            },
            .tuple => |tuple| {
                const tuple_value = switch (value) {
                    .tuple => |tuple_value| tuple_value,
                    else => Common.invariant("tuple call pattern matched a non-tuple value"),
                };
                for (tuple.items, tuple_value.items) |item_shape, item| {
                    try self.appendExprsFromValue(item_shape, item, out);
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
                for (callable.captures, callable_value.captures) |capture_shape, capture_value| {
                    try self.appendExprsFromValue(capture_shape, capture_value.value, out);
                }
            },
        }
    }

    fn appendFieldReadExprsFromValue(
        self: *Cloner,
        shape: Shape,
        value: Value,
        out: *std.ArrayList(Ast.ExprId),
    ) Common.LowerError!bool {
        if (shapeMatchesValue(self.pass.program, shape, value)) {
            try self.appendExprsFromValue(shape, value, out);
            return true;
        }

        switch (shape) {
            .any => {
                try out.append(self.pass.allocator, try self.materialize(value));
                return true;
            },
            .record => |record| {
                const receiver = switch (value) {
                    .expr => |expr| expr,
                    else => return false,
                };
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                for (record.fields) |field| {
                    const field_expr = try self.addExpr(.{ .ty = shapeType(field.shape), .data = .{ .field_access = .{
                        .receiver = receiver,
                        .field = field.name,
                    } } });
                    if (!try self.appendFieldReadExprsFromValue(field.shape, .{ .expr = field_expr }, out)) return false;
                }
                return true;
            },
            .tuple => |tuple| {
                const receiver = switch (value) {
                    .expr => |expr| expr,
                    else => return false,
                };
                if (!canReadFieldsFromExpr(self.pass.program, receiver)) return false;
                for (tuple.items, 0..) |item, index| {
                    const item_expr = try self.addExpr(.{ .ty = shapeType(item), .data = .{ .tuple_access = .{
                        .tuple = receiver,
                        .elem_index = @as(u32, @intCast(index)),
                    } } });
                    if (!try self.appendFieldReadExprsFromValue(item, .{ .expr = item_expr }, out)) return false;
                }
                return true;
            },
            .tag,
            .nominal,
            .callable,
            => return false,
        }
    }

    fn cloneFieldAccess(self: *Cloner, ty: Type.TypeId, field: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValue(field.receiver);
        if (fieldFromValue(receiver, field.field)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .field_access = .{
            .receiver = try self.materialize(receiver),
            .field = field.field,
        } } });
    }

    fn cloneTupleAccess(self: *Cloner, ty: Type.TypeId, access: anytype) Common.LowerError!Ast.ExprId {
        const receiver = try self.cloneExprValue(access.tuple);
        if (itemFromValue(receiver, access.elem_index)) |value| return try self.materialize(value);
        return try self.addExpr(.{ .ty = ty, .data = .{ .tuple_access = .{
            .tuple = try self.materialize(receiver),
            .elem_index = access.elem_index,
        } } });
    }

    fn cloneMatch(self: *Cloner, ty: Type.TypeId, match: @import("../monotype/ast.zig").MatchExpr) Common.LowerError!Ast.ExprId {
        const scrutinee = try self.cloneExprValue(match.scrutinee);
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
        if (scrutinee == .expr) return null;
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
            return try self.wrapPendingLets(body, pending_lets.items);
        }
        Common.invariant("known constructor match had no matching branch");
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
        const uses = localUseCountInExpr(self.pass.program, local, body);
        if (self.valueCanSubstitute(value) or
            (unsafe_count == 1 and uses == 1 and localUseBeforeEffect(self.pass.program, local, body)))
        {
            return value;
        }
        return try self.makeReusableForMatch(value, pending_lets);
    }

    fn unsafeLeafCount(self: *Cloner, value: Value) usize {
        return switch (value) {
            .expr => |expr| if (self.exprCanSubstitute(expr)) 0 else 1,
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
                for (callable.captures) |capture| count += self.unsafeLeafCount(capture.value);
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
                const captures = try self.pass.arena.allocator().alloc(CaptureValue, callable.captures.len);
                for (callable.captures, 0..) |capture, index| {
                    captures[index] = .{
                        .id = capture.id,
                        .value = try self.makeReusableForMatch(capture.value, pending_lets),
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

    fn wrapPendingLets(self: *Cloner, body: Value, pending_lets: []const PendingLet) Common.LowerError!Value {
        if (pending_lets.len == 0) return body;

        const ty = valueType(self.pass.program, body);
        var result = try self.materialize(body);
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
                .value = pending.value,
                .rest = result,
            } } });
        }
        return .{ .expr = result };
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
            const outer_value = (try self.simplifyKnownMatchValue(inner_value, outer_branches_span)) orelse return null;
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
            if (active == callable.fn_id) {
                return .{ .expr = try self.addExpr(.{ .ty = ty, .data = .{ .call_value = .{
                    .callee = try self.materialize(.{ .callable = callable }),
                    .args = try self.cloneExprSpan(args_span),
                } } }) };
            }
        }

        const source_fn = self.pass.program.fns.items[@intFromEnum(callable.fn_id)];
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

        const prepared_captures = try self.pass.allocator.alloc(Value, source_captures.len);
        defer self.pass.allocator.free(prepared_captures);
        for (source_captures, 0..) |source_capture, index| {
            const id = self.pass.program.captureIdOfLocal(source_capture.local);
            const capture_value = callableCaptureValueForId(callable.captures, id) orelse
                Common.invariant("inlined callable had no value for a capture slot");
            prepared_captures[index] = try self.makeReusableForMatch(capture_value, &pending_lets);
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

        try self.inline_stack.append(self.pass.allocator, callable.fn_id);
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped != callable.fn_id) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        return try self.wrapPendingLets(try self.cloneExprValue(body), pending_lets.items);
    }

    fn inlineDirectCallValue(
        self: *Cloner,
        callee: Ast.FnId,
        args_span: Ast.Span(Ast.ExprId),
        captures_span: Ast.Span(Ast.CaptureOperand),
        original_expr: Ast.ExprId,
    ) Common.LowerError!Value {
        for (self.inline_stack.items) |active| {
            if (active == callee) return .{ .expr = try self.cloneExprPlain(original_expr) };
        }

        const source_fn = self.pass.program.fns.items[@intFromEnum(callee)];
        const body = switch (source_fn.body) {
            .roc => |body| body,
            .hosted => return .{ .expr = try self.cloneExprPlain(original_expr) },
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
        // The call's capture operands are keyed by CaptureId, not positional
        // with the callee's capture slots. Clone each operand's value keyed by
        // id, then resolve each slot's value by its own CaptureId below.
        const operands = try self.pass.allocator.dupe(Ast.CaptureOperand, self.pass.program.captureOperandSpan(captures_span));
        defer self.pass.allocator.free(operands);
        if (captures.len != operands.len) {
            Common.invariant("direct call capture count differed from lifted function capture count");
        }

        const capture_values = try self.pass.allocator.alloc(CaptureValue, operands.len);
        defer self.pass.allocator.free(capture_values);
        for (operands, 0..) |operand, index| {
            capture_values[index] = .{ .id = operand.id, .value = try self.cloneExprValue(operand.value) };
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
                Common.invariant("inlined direct call had no value for a capture slot");
            prepared_captures[index] = try self.valueForInlineLocal(capture.local, capture_value, body, unsafe_count, &pending_lets);
        }

        const prepared_args = try self.pass.allocator.alloc(Value, arg_values.len);
        defer self.pass.allocator.free(prepared_args);
        for (source_args, arg_values, 0..) |source_arg, arg_value, index| {
            prepared_args[index] = try self.valueForInlineLocal(source_arg.local, arg_value, body, unsafe_count, &pending_lets);
        }

        try self.inline_stack.append(self.pass.allocator, callee);
        defer {
            const popped = self.inline_stack.pop() orelse Common.invariant("call-pattern inline stack underflow");
            if (popped != callee) Common.invariant("call-pattern inline stack was corrupted");
        }

        for (captures, prepared_captures) |capture, capture_value| {
            try self.putSubst(capture.local, capture_value);
        }
        for (source_args, prepared_args) |source_arg, arg_value| {
            try self.putSubst(source_arg.local, arg_value);
        }

        return try self.wrapPendingLets(try self.cloneExprValue(body), pending_lets.items);
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

    fn cloneStmt(self: *Cloner, stmt_id: Ast.StmtId) Common.LowerError!Ast.StmtId {
        const saved_loc = self.current_loc;
        defer self.current_loc = saved_loc;
        const saved_region = self.current_region;
        defer self.current_region = saved_region;
        const stmt_loc = self.pass.program.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) self.current_loc = stmt_loc;
        const stmt_region = self.pass.program.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) self.current_region = stmt_region;

        const stmt = self.pass.program.stmts.items[@intFromEnum(stmt_id)];
        return try self.addStmt(switch (stmt) {
            .uninitialized => |pat| .{ .uninitialized = try self.clonePat(pat) },
            .let_ => |let_| blk: {
                const value = try self.cloneExprValue(let_.value);
                const value_expr = try self.materialize(value);
                _ = try self.bindPatToReusableValue(let_.pat, value);
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
            .return_ => |ret| .{ .return_ = .{
                .value = try self.cloneExpr(ret.value),
                .target = ret.target,
            } },
            .crash => |msg| .{ .crash = msg },
        });
    }

    fn cloneExprSpan(self: *Cloner, span: Ast.Span(Ast.ExprId)) Common.LowerError!Ast.Span(Ast.ExprId) {
        const source = try self.pass.allocator.dupe(Ast.ExprId, self.pass.program.exprSpan(span));
        defer self.pass.allocator.free(source);

        const values = try self.pass.allocator.alloc(Ast.ExprId, source.len);
        defer self.pass.allocator.free(values);
        for (source, 0..) |expr, index| values[index] = try self.cloneExpr(expr);
        return try self.pass.program.addExprSpan(values);
    }

    /// Clone a keyed capture operand span, preserving each operand's CaptureId
    /// and cloning its supplying expression.
    fn cloneCaptureOperandSpan(self: *Cloner, span: Ast.Span(Ast.CaptureOperand)) Common.LowerError!Ast.Span(Ast.CaptureOperand) {
        const source = try self.pass.allocator.dupe(Ast.CaptureOperand, self.pass.program.captureOperandSpan(span));
        defer self.pass.allocator.free(source);

        const operands = try self.pass.allocator.alloc(Ast.CaptureOperand, source.len);
        defer self.pass.allocator.free(operands);
        for (source, 0..) |operand, index| operands[index] = .{ .id = operand.id, .value = try self.cloneExpr(operand.value) };
        return try self.pass.program.addCaptureOperandSpan(operands);
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
        for (captures) |capture| {
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

        return try self.materializeCallableWithCaptures(callable.ty, callable.fn_id, fn_.captures, callable.captures);
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
        values: []const CaptureValue,
    ) Common.LowerError!Ast.ExprId {
        const captures = try self.pass.allocator.dupe(Ast.TypedLocal, self.pass.program.typedLocalSpan(captures_span));
        defer self.pass.allocator.free(captures);
        if (captures.len != values.len) {
            Common.invariant("callable value capture count differed from specialized function capture count");
        }

        // Build one operand per capture slot, keyed by the slot's CaptureId. The
        // supplying value is looked up by id (not position), so a specialized
        // callee whose slots differ in order from the source still gets each
        // capture's correct value.
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

    /// Look up the cloned value supplying capture `id` in a callable value's
    /// keyed captures.
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

fn exprContainsReturn(program: *const Ast.Program, expr_id: Ast.ExprId) bool {
    return switch (program.exprs.items[@intFromEnum(expr_id)].data) {
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
        .fn_ref => |fn_ref| exprSpanContainsReturn(program, fn_ref.captures),
        .return_ => true,
        .list,
        .tuple,
        => |items| exprSpanContainsReturn(program, items),
        .record => |fields| {
            for (program.fieldExprSpan(fields)) |field| {
                if (exprContainsReturn(program, field.value)) return true;
            }
            return false;
        },
        .tag => |tag| exprSpanContainsReturn(program, tag.payloads),
        .nominal,
        .dbg,
        .expect,
        => |child| exprContainsReturn(program, child),
        .expect_err => |expect_err| exprContainsReturn(program, expect_err.msg),
        .comptime_branch_taken => |taken| exprContainsReturn(program, taken.body),
        .let_ => |let_| exprContainsReturn(program, let_.value) or exprContainsReturn(program, let_.rest),
        .call_value => |call| exprContainsReturn(program, call.callee) or exprSpanContainsReturn(program, call.args),
        .call_proc => |call| exprSpanContainsReturn(program, call.args) or exprSpanContainsReturn(program, call.captures),
        .low_level => |call| exprSpanContainsReturn(program, call.args),
        .field_access => |field| exprContainsReturn(program, field.receiver),
        .tuple_access => |access| exprContainsReturn(program, access.tuple),
        .structural_eq => |eq| exprContainsReturn(program, eq.lhs) or exprContainsReturn(program, eq.rhs),
        .structural_hash => |h| exprContainsReturn(program, h.value) or exprContainsReturn(program, h.hasher),
        .match_ => |match| {
            if (exprContainsReturn(program, match.scrutinee)) return true;
            for (program.branchSpan(match.branches)) |branch| {
                if (branch.guard) |guard| {
                    if (exprContainsReturn(program, guard)) return true;
                }
                if (exprContainsReturn(program, branch.body)) return true;
            }
            return false;
        },
        .if_ => |if_| {
            for (program.ifBranchSpan(if_.branches)) |branch| {
                if (exprContainsReturn(program, branch.cond)) return true;
                if (exprContainsReturn(program, branch.body)) return true;
            }
            return exprContainsReturn(program, if_.final_else);
        },
        .block => |block| {
            for (program.stmtSpan(block.statements)) |stmt| {
                if (stmtContainsReturn(program, stmt)) return true;
            }
            return exprContainsReturn(program, block.final_expr);
        },
        .loop_ => |loop| exprSpanContainsReturn(program, loop.initial_values) or exprContainsReturn(program, loop.body),
        .break_ => |maybe| if (maybe) |value| exprContainsReturn(program, value) else false,
        .continue_ => |continue_| exprSpanContainsReturn(program, continue_.values),
        .if_initialized_payload => |payload_switch| exprContainsReturn(program, payload_switch.cond) or
            exprContainsReturn(program, payload_switch.initialized) or
            exprContainsReturn(program, payload_switch.uninitialized),
        .try_sequence => |sequence| exprContainsReturn(program, sequence.try_expr) or exprContainsReturn(program, sequence.ok_body),
        .try_record_sequence => |sequence| exprContainsReturn(program, sequence.try_expr) or exprContainsReturn(program, sequence.ok_body),
    };
}

fn exprSpanContainsReturn(program: *const Ast.Program, span: Ast.Span(Ast.ExprId)) bool {
    for (program.exprSpan(span)) |expr| {
        if (exprContainsReturn(program, expr)) return true;
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
        => |expr| exprContainsReturn(program, expr),
        .uninitialized,
        .crash,
        => false,
    };
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
        .bytes_lit,
        .crash,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        => 0,
        .fn_ref => |fn_ref| localUseCountInExprSpan(program, local, fn_ref.captures),
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
        .call_proc => |call| localUseCountInExprSpan(program, local, call.args) + localUseCountInExprSpan(program, local, call.captures),
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

fn localUseCountInStmt(program: *const Ast.Program, local: Ast.LocalId, stmt_id: Ast.StmtId) usize {
    return switch (program.stmts.items[@intFromEnum(stmt_id)]) {
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
        .bytes_lit,
        .uninitialized,
        .uninitialized_payload,
        => {},
        .fn_ref => |fn_ref| scanLocalUseInExprSpan(program, local, fn_ref.captures, scan),
        .crash, .comptime_exhaustiveness_failed => scan.seen_effect = true,
        .list,
        .tuple,
        => |items| scanLocalUseInExprSpan(program, local, items, scan),
        .record => |fields| {
            for (program.fieldExprSpan(fields)) |field| scanLocalUseInExpr(program, local, field.value, scan);
        },
        .tag => |tag| scanLocalUseInExprSpan(program, local, tag.payloads, scan),
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
            scanLocalUseInExprSpan(program, local, call.captures, scan);
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
            for (program.branchSpan(match.branches)) |branch| {
                var branch_scan = scan.*;
                if (branch.guard) |guard| scanLocalUseInExpr(program, local, guard, &branch_scan);
                scanLocalUseInExpr(program, local, branch.body, &branch_scan);
                scan.found_before_effect = scan.found_before_effect or branch_scan.found_before_effect;
                scan.found_after_effect = scan.found_after_effect or branch_scan.found_after_effect;
                scan.seen_effect = scan.seen_effect or branch_scan.seen_effect;
            }
        },
        .if_ => |if_| {
            for (program.ifBranchSpan(if_.branches)) |branch| {
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
        .return_ => |ret| {
            scanLocalUseInExpr(program, local, ret.value, scan);
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

fn valueType(program: *const Ast.Program, value: Value) Type.TypeId {
    return switch (value) {
        .expr => |expr| program.exprs.items[@intFromEnum(expr)].ty,
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
            if (!sameType(program, lhs_tag.ty, rhs_tag.ty) or lhs_tag.name != rhs_tag.name or lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk false;
            for (lhs_tag.payloads, rhs_tag.payloads) |lhs_payload, rhs_payload| {
                if (!shapeEql(program, lhs_payload, rhs_payload)) break :blk false;
            }
            break :blk true;
        },
        .record => |lhs_record| blk: {
            const rhs_record = rhs.record;
            if (!sameType(program, lhs_record.ty, rhs_record.ty) or lhs_record.fields.len != rhs_record.fields.len) break :blk false;
            for (lhs_record.fields, rhs_record.fields) |lhs_field, rhs_field| {
                if (lhs_field.name != rhs_field.name or !shapeEql(program, lhs_field.shape, rhs_field.shape)) break :blk false;
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
    return switch (shape) {
        .any => true,
        .tag => |tag| blk: {
            const value_tag = switch (value) {
                .tag => |value_tag| value_tag,
                else => break :blk false,
            };
            if (!sameType(program, tag.ty, value_tag.ty) or tag.name != value_tag.name or tag.payloads.len != value_tag.payloads.len) break :blk false;
            for (tag.payloads, value_tag.payloads) |payload_shape, payload_value| {
                if (!shapeMatchesValue(program, payload_shape, payload_value)) break :blk false;
            }
            break :blk true;
        },
        .record => |record| blk: {
            const value_record = switch (value) {
                .record => |value_record| value_record,
                else => break :blk false,
            };
            if (!sameType(program, record.ty, value_record.ty) or record.fields.len != value_record.fields.len) break :blk false;
            for (record.fields, value_record.fields) |field_shape, field_value| {
                if (field_shape.name != field_value.name or !shapeMatchesValue(program, field_shape.shape, field_value.value)) break :blk false;
            }
            break :blk true;
        },
        .tuple => |tuple| blk: {
            const value_tuple = switch (value) {
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
            const value_nominal = switch (value) {
                .nominal => |value_nominal| value_nominal,
                else => break :blk false,
            };
            break :blk sameType(program, nominal.ty, value_nominal.ty) and shapeMatchesValue(program, nominal.backing.*, value_nominal.backing.*);
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
            for (callable.captures, value_callable.captures) |capture_shape, capture_value| {
                if (!shapeMatchesValue(program, capture_shape, capture_value.value)) break :blk false;
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

    const call = switch (lifted.exprs.items[@intFromEnum(body)].data) {
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

test "call-pattern specialization declarations are referenced" {
    std.testing.refAllDecls(@This());
}
