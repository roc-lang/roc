//! Explicit inline eligibility analysis over Lambda Solved IR.

const std = @import("std");
const collections = @import("collections");

const Common = @import("common.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const SpecConstr = @import("monotype_lifted/spec_constr.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");
const GuardedList = collections.GuardedList;

/// Post-check inline analysis mode.
pub const Mode = enum {
    none,
    wrappers,
};

/// Immutable inline eligibility table consumed by later lowering stages.
pub const Plan = struct {
    inline_bodies: []const ?Lifted.ExprId = &.{},

    pub fn bodyForFn(self: Plan, fn_id: Lifted.FnId) ?Lifted.ExprId {
        if (self.inline_bodies.len == 0) return null;

        const index = @intFromEnum(fn_id);
        if (index >= self.inline_bodies.len) {
            Common.invariant("inline plan did not contain a lifted function");
        }
        return self.inline_bodies[index];
    }
};

/// Allocator-owned storage for a post-check inline plan.
pub const OwnedPlan = struct {
    allocator: std.mem.Allocator,
    inline_bodies: []?Lifted.ExprId,

    pub fn empty(allocator: std.mem.Allocator) OwnedPlan {
        return .{ .allocator = allocator, .inline_bodies = &.{} };
    }

    pub fn deinit(self: *OwnedPlan) void {
        if (self.inline_bodies.len != 0) self.allocator.free(self.inline_bodies);
        self.* = empty(self.allocator);
    }

    pub fn view(self: *const OwnedPlan) Plan {
        return .{ .inline_bodies = self.inline_bodies };
    }
};

/// Analyze a Lambda Solved program and produce explicit inline decisions.
/// With `keep_keyed_specializations`, a function that is a keyed template
/// specialization is never inlined away: a pack program offers those
/// procedures from its manifest, so each must survive as a procedure even
/// when its only caller is the export wrapper.
pub fn analyze(
    allocator: std.mem.Allocator,
    mode: Mode,
    procedure_usage: SpecConstr.ProcedureUsage,
    solved: *const Solved.Program,
    keep_keyed_specializations: bool,
) std.mem.Allocator.Error!OwnedPlan {
    return switch (mode) {
        .none => OwnedPlan.empty(allocator),
        .wrappers => try InlineAnalyzer.run(allocator, procedure_usage, solved, keep_keyed_specializations),
    };
}

const Decision = union(enum) {
    unknown,
    visiting,
    never,
    inline_body: Candidate,
};

const Candidate = struct {
    body: Lifted.ExprId,
    kind: enum { wrapper, single_use },
};

const MaterializationState = enum {
    unknown,
    visiting,
    once,
    multiple,
};

const InlineAnalyzer = struct {
    allocator: std.mem.Allocator,
    procedure_usage: SpecConstr.ProcedureUsage,
    solved: *const Solved.Program,
    solved_types: SolvedType.Store.View,
    decisions: []Decision,
    stack: std.ArrayList(Lifted.FnId),
    keep_keyed_specializations: bool,

    fn run(
        allocator: std.mem.Allocator,
        procedure_usage: SpecConstr.ProcedureUsage,
        solved: *const Solved.Program,
        keep_keyed_specializations: bool,
    ) std.mem.Allocator.Error!OwnedPlan {
        if (procedure_usage.items.len != solved.lifted.fnCount()) {
            Common.invariant("optimized inline analysis requires exact use information for every lifted function");
        }
        const decisions = try allocator.alloc(Decision, solved.lifted.fnCount());
        errdefer allocator.free(decisions);
        @memset(decisions, .unknown);

        var analyzer = InlineAnalyzer{
            .allocator = allocator,
            .procedure_usage = procedure_usage,
            .solved = solved,
            .solved_types = solved.types.view(),
            .decisions = decisions,
            .stack = .empty,
            .keep_keyed_specializations = keep_keyed_specializations,
        };
        defer analyzer.stack.deinit(allocator);

        for (0..solved.lifted.fnCount()) |index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            _ = try analyzer.inlineBody(fn_id);
        }

        const materialization_states = try allocator.alloc(MaterializationState, decisions.len);
        defer allocator.free(materialization_states);
        @memset(materialization_states, .unknown);
        for (0..solved.lifted.fnCount()) |index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try analyzer.resolveSingleUseMaterialization(fn_id, materialization_states);
        }

        const inline_bodies = try allocator.alloc(?Lifted.ExprId, decisions.len);
        errdefer allocator.free(inline_bodies);
        for (decisions, 0..) |decision, index| {
            inline_bodies[index] = switch (decision) {
                .inline_body => |candidate| candidate.body,
                .unknown,
                .visiting,
                .never,
                => null,
            };
        }

        allocator.free(decisions);
        analyzer.decisions = &.{};

        return .{
            .allocator = allocator,
            .inline_bodies = inline_bodies,
        };
    }

    fn inlineCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Candidate {
        if (self.keep_keyed_specializations) {
            if (self.solved.lifted.getFn(fn_id).source) |template| {
                if (template.spec_key != null) return null;
            }
        }
        if (try self.wrapperCandidate(fn_id)) |body| return .{ .body = body, .kind = .wrapper };
        if (self.singleUseCandidate(fn_id)) |body| return .{ .body = body, .kind = .single_use };
        return null;
    }

    fn singleUseCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) ?Lifted.ExprId {
        const use = self.procedure_usage.get(fn_id);
        if (use.external_calls != 1 or use.value_refs != 0) return null;
        if (use.contains_return) return null;

        const call_expr_id = use.external_call_expr orelse
            Common.invariant("single-use function had no external call expression");
        const call_expr = self.solved.lifted.getExpr(call_expr_id);
        if (call_expr.data != .call_proc) {
            Common.invariant("single-use function external call was not a direct call expression");
        }
        const call = call_expr.data.call_proc;
        if (Lifted.localDirectCallee(call) != fn_id) {
            Common.invariant("single-use function external call changed callee");
        }
        if (call.is_cold) return null;

        const source_fn = self.solved.lifted.getFn(fn_id);
        if (self.solved.lifted.typedLocalSpan(source_fn.captures).len != 0) return null;
        if (self.solvedCaptureCount(fn_id) != 0) return null;
        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };
        return body;
    }

    fn solvedCaptureCount(self: *const InlineAnalyzer, fn_id: Lifted.FnId) usize {
        const captures = self.solvedCapturesForFn(fn_id);
        return self.solved_types.captureSpan(captures).len;
    }

    fn solvedCapturesForFn(self: *const InlineAnalyzer, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.getFn(fn_id).symbol;
        const fn_content = self.solved.types.rootContent(self.solved.fn_tys.items[@intFromEnum(fn_id)]);
        if (fn_content != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        const callable_content = self.solved.types.rootContent(fn_content.func.callable);
        const callable = if (callable_content == .lambda_set)
            callable_content.lambda_set
        else if (callable_content == .erased)
            callable_content.erased.members
        else
            Common.invariant("callable value did not have a resolved callable slot");
        for (self.solved_types.memberSpan(callable)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        return .empty();
    }

    /// One function whose inline eligibility is being decided. Its candidate
    /// body is walked on its own work list, which suspends at each direct
    /// call until that callee's decision is made, so neither call chains nor
    /// expression nesting become native call depth.
    const Visit = struct {
        fn_id: Lifted.FnId,
        candidate: Candidate,
        walk: std.ArrayList(WalkItem),
    };

    const WalkItem = struct { child: Lifted.ExprChild, loop_depth: usize };

    const Entered = union(enum) {
        /// The function's decision was already made.
        decided: ?Lifted.ExprId,
        /// A visit of the function's candidate body began.
        started,
    };

    fn inlineBody(self: *InlineAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Lifted.ExprId {
        var visits: std.ArrayList(Visit) = .empty;
        defer {
            for (visits.items) |*visit| visit.walk.deinit(self.allocator);
            visits.deinit(self.allocator);
        }
        switch (try self.enterInlineBody(fn_id, &visits)) {
            .decided => |body| return body,
            .started => {},
        }
        while (true) {
            const visit = &visits.items[visits.items.len - 1];
            const outcome = try self.walkVisit(visit, &visits);
            if (outcome == .suspended) continue;
            const finished = visits.pop().?;
            var walk = finished.walk;
            walk.deinit(self.allocator);
            const body = self.finishVisit(finished, outcome == .closed);
            if (visits.items.len == 0) return body;
        }
    }

    fn enterInlineBody(self: *InlineAnalyzer, fn_id: Lifted.FnId, visits: *std.ArrayList(Visit)) std.mem.Allocator.Error!Entered {
        const index = @intFromEnum(fn_id);
        switch (self.decisions[index]) {
            .unknown => {},
            .visiting => {
                self.markCycle(fn_id);
                return .{ .decided = null };
            },
            .never => return .{ .decided = null },
            .inline_body => |candidate| return .{ .decided = candidate.body },
        }

        self.decisions[index] = .visiting;
        try self.stack.append(self.allocator, fn_id);

        const candidate = try self.inlineCandidate(fn_id) orelse {
            self.decisions[index] = .never;
            self.leaveInlineBody(fn_id);
            return .{ .decided = null };
        };

        // Visit every proc called anywhere in the candidate body, including calls
        // nested inside low-level operands or other call arguments. A self-call
        // re-enters this function while it is `.visiting`, so `markCycle` marks
        // the whole cycle `.never` and keeps it out of the inline plan instead
        // of inlining it without bound.
        var walk: std.ArrayList(WalkItem) = .empty;
        errdefer walk.deinit(self.allocator);
        try walk.append(self.allocator, .{ .child = .{ .expr = candidate.body }, .loop_depth = 0 });
        try visits.append(self.allocator, .{ .fn_id = fn_id, .candidate = candidate, .walk = walk });
        return .started;
    }

    fn leaveInlineBody(self: *InlineAnalyzer, fn_id: Lifted.FnId) void {
        const popped = self.stack.pop() orelse Common.invariant("inline analysis stack underflow");
        if (popped != fn_id) Common.invariant("inline analysis stack was corrupted");
    }

    fn finishVisit(self: *InlineAnalyzer, visit: Visit, closed: bool) ?Lifted.ExprId {
        const index = @intFromEnum(visit.fn_id);
        defer self.leaveInlineBody(visit.fn_id);
        if (!closed) {
            self.decisions[index] = .never;
            return null;
        }
        switch (self.decisions[index]) {
            .never => return null,
            .visiting => {},
            .unknown,
            .inline_body,
            => Common.invariant("inline analysis decision changed unexpectedly while visiting a candidate"),
        }
        self.decisions[index] = .{ .inline_body = visit.candidate };
        return visit.candidate.body;
    }

    const WalkOutcome = enum {
        /// A callee's visit began; this walk resumes after it.
        suspended,
        /// The body is closed: no escaping return, break, or continue.
        closed,
        /// The body can transfer control out of itself.
        open,
    };

    /// Walk a candidate body, proving that every break and continue is owned
    /// by a loop inside the body while visiting every proc it calls, so
    /// cycles consisting entirely of selected bodies are rejected before
    /// lowering; combining the checks avoids a second traversal. Positions
    /// are visited in source order.
    fn walkVisit(self: *InlineAnalyzer, visit: *Visit, visits: *std.ArrayList(Visit)) std.mem.Allocator.Error!WalkOutcome {
        const walk = &visit.walk;
        var children: std.ArrayList(Lifted.ExprChild) = .empty;
        defer children.deinit(self.allocator);
        while (walk.pop()) |item| {
            const depth = item.loop_depth;
            children.clearRetainingCapacity();
            var callee: ?Lifted.FnId = null;
            switch (item.child) {
                .stmt => |stmt_id| switch (self.solved.lifted.getStmt(stmt_id)) {
                    .return_ => return .open,
                    .let_, .expr, .expect, .dbg, .uninitialized, .crash => try Lifted.appendStmtChildren(self.allocator, &self.solved.lifted, stmt_id, &children),
                },
                .expr => |expr_id| switch (self.solved.lifted.getExpr(expr_id).data) {
                    .return_ => return .open,
                    .break_ => if (depth == 0) return .open else try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children),
                    .continue_ => if (depth == 0) return .open else try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children),
                    .loop_ => |loop| {
                        const initial_values = self.solved.lifted.exprSpan(loop.initial_values);
                        for (0..initial_values.len) |index| try children.append(self.allocator, .{ .expr = GuardedList.at(initial_values, index) });
                        try walk.append(self.allocator, .{ .child = .{ .expr = loop.body }, .loop_depth = depth + 1 });
                    },
                    .lambda,
                    .fn_def,
                    .uninitialized,
                    .uninitialized_payload,
                    .crash,
                    .comptime_exhaustiveness_failed,
                    .inline_expects_enabled,
                    => {},
                    .call_proc => |call| {
                        callee = Lifted.localDirectCallee(call);
                        try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children);
                    },
                    .@"unreachable",
                    .local,
                    .unit,
                    .int_lit,
                    .frac_f32_lit,
                    .frac_f64_lit,
                    .dec_lit,
                    .str_lit,
                    .bytes_lit,
                    .def_ref,
                    .fn_ref,
                    .list,
                    .tuple,
                    .record,
                    .record_update,
                    .tag,
                    .typed_boundary,
                    .static_data_candidate,
                    .comptime_value,
                    .nominal,
                    .dbg,
                    .expect,
                    .expect_err,
                    .literal_rejected,
                    .comptime_branch_taken,
                    .let_,
                    .call_value,
                    .low_level,
                    .field_access,
                    .tuple_access,
                    .structural_eq,
                    .structural_hash,
                    .match_,
                    .if_,
                    .block,
                    .join_point,
                    .jump,
                    .if_initialized_payload,
                    .try_sequence,
                    .try_record_sequence,
                    => try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children),
                },
            }
            var index = children.items.len;
            while (index > 0) {
                index -= 1;
                try walk.append(self.allocator, .{ .child = children.items[index], .loop_depth = depth });
            }
            // The callee is decided before the call's operands are visited.
            if (callee) |target| switch (try self.enterInlineBody(target, visits)) {
                .decided => {},
                .started => return .suspended,
            };
        }
        return .closed;
    }

    /// Resolve outer single-use candidates before their descendants. Demoting
    /// an outer body creates a procedure boundary, which can make a nested
    /// single-use body safe to inline exactly once.
    fn resolveSingleUseMaterialization(
        self: *InlineAnalyzer,
        fn_id: Lifted.FnId,
        states: []MaterializationState,
    ) std.mem.Allocator.Error!void {
        _ = try self.runMaterialization(.{ .resolve = fn_id }, states);
    }

    /// One step of the materialization proof. `resolve` settles a single-use
    /// candidate; `body_once` asks whether a body is lowered in exactly one
    /// place. Both follow unique call owners outward, so the chain of owners
    /// is held on an explicit stack.
    const MaterializationFrame = union(enum) {
        resolve: Lifted.FnId,
        body_once: Lifted.FnId,
    };

    fn runMaterialization(self: *InlineAnalyzer, root: MaterializationFrame, states: []MaterializationState) std.mem.Allocator.Error!bool {
        var frames: std.ArrayList(MaterializationFrame) = .empty;
        defer frames.deinit(self.allocator);
        try frames.append(self.allocator, root);
        // The result of the frame that just finished, for its parent.
        var result: ?bool = null;
        while (frames.items.len != 0) {
            const frame = frames.items[frames.items.len - 1];
            switch (frame) {
                .resolve => |fn_id| {
                    const index = @intFromEnum(fn_id);
                    if (result) |once| {
                        if (!once) self.decisions[index] = .never;
                        states[index] = .once;
                        _ = frames.pop();
                        result = true;
                        continue;
                    }
                    const candidate = switch (self.decisions[index]) {
                        .inline_body => |candidate| candidate,
                        .never => {
                            states[index] = .once;
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .unknown,
                        .visiting,
                        => Common.invariant("inline materialization analysis saw an unfinished decision"),
                    };
                    if (candidate.kind != .single_use) {
                        _ = frames.pop();
                        result = true;
                        continue;
                    }

                    switch (states[index]) {
                        .unknown => states[index] = .visiting,
                        .visiting => Common.invariant("single-use call-owner graph contained a selected cycle"),
                        .once => {
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .multiple => Common.invariant("resolved single-use body still had multiple materializations"),
                    }

                    const use = self.procedure_usage.get(fn_id);
                    const owner = use.external_call_owner orelse
                        Common.invariant("single-use function had no external call owner");
                    try frames.append(self.allocator, .{ .body_once = owner });
                },
                .body_once => |fn_id| {
                    const index = @intFromEnum(fn_id);
                    if (result) |once| {
                        states[index] = if (once) .once else .multiple;
                        _ = frames.pop();
                        result = once;
                        continue;
                    }
                    switch (self.decisions[index]) {
                        .never => {
                            states[index] = .once;
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .inline_body => |candidate| if (candidate.kind == .single_use) {
                            // The resolved single-use body answers for this one.
                            frames.items[frames.items.len - 1] = .{ .resolve = fn_id };
                            continue;
                        },
                        .unknown,
                        .visiting,
                        => Common.invariant("inline materialization analysis saw an unfinished owner decision"),
                    }

                    switch (states[index]) {
                        .unknown => states[index] = .visiting,
                        .visiting => Common.invariant("selected wrapper call-owner graph contained a cycle"),
                        .once => {
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .multiple => {
                            _ = frames.pop();
                            result = false;
                            continue;
                        },
                    }

                    const use = self.procedure_usage.get(fn_id);
                    const settled: ?bool = if (use.external_calls == 0)
                        true
                    else if (use.external_calls != 1)
                        false
                    else blk: {
                        const call_expr_id = use.external_call_expr orelse
                            Common.invariant("single-call inline owner had no external call expression");
                        const call_expr = self.solved.lifted.getExpr(call_expr_id);
                        if (call_expr.data != .call_proc) {
                            Common.invariant("single-call inline owner use was not a direct call expression");
                        }
                        if (call_expr.data.call_proc.is_cold) break :blk true;
                        if (use.value_refs != 0) break :blk false;
                        break :blk null;
                    };
                    if (settled) |once| {
                        states[index] = if (once) .once else .multiple;
                        _ = frames.pop();
                        result = once;
                        continue;
                    }
                    const owner = use.external_call_owner orelse
                        Common.invariant("single-call inline owner had no external call owner");
                    try frames.append(self.allocator, .{ .body_once = owner });
                },
            }
            result = null;
        }
        return result.?;
    }

    fn wrapperCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Lifted.ExprId {
        const source_fn = self.solved.lifted.getFn(fn_id);
        if (self.solved.lifted.typedLocalSpan(source_fn.captures).len != 0) return null;
        if (self.solvedCaptureCount(fn_id) != 0) return null;

        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };

        if (!try self.isInlineableWrapperBody(body)) return null;
        if (!try self.bodyReadsOnlyArgs(fn_id, body)) return null;
        return body;
    }

    fn bodyReadsOnlyArgs(self: *const InlineAnalyzer, fn_id: Lifted.FnId, body: Lifted.ExprId) std.mem.Allocator.Error!bool {
        const source_fn = self.solved.lifted.getFn(fn_id);
        return self.exprReadsOnlyArgs(body, self.solved.lifted.typedLocalSpan(source_fn.args));
    }

    /// Whether `root` reads no local other than `args`. Every subexpression
    /// must, so they are checked in any order on a work stack.
    fn exprReadsOnlyArgs(self: *const InlineAnalyzer, root: Lifted.ExprId, args: anytype) std.mem.Allocator.Error!bool {
        const lifted = &self.solved.lifted;
        var stack: std.ArrayList(Lifted.ExprChild) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .expr = root });
        while (stack.pop()) |child| {
            const expr_id = child.expr;
            switch (lifted.getExpr(expr_id).data) {
                .local => |local| if (!localIsArg(local, args)) return false,
                .@"unreachable",
                .unit,
                .int_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .dec_lit,
                .str_lit,
                .crash,
                .bytes_lit,
                .def_ref,
                .inline_expects_enabled,
                => {},
                .fn_ref,
                .list,
                .tuple,
                .record,
                .record_update,
                .tag,
                .static_data_candidate,
                .comptime_value,
                .typed_boundary,
                .nominal,
                .dbg,
                .expect,
                .return_,
                .expect_err,
                .literal_rejected,
                .comptime_branch_taken,
                .call_value,
                .low_level,
                .field_access,
                .tuple_access,
                .structural_eq,
                .structural_hash,
                => try Lifted.appendChildren(self.allocator, lifted, expr_id, &stack),
                .call_proc => |call| {
                    if (call.is_cold) return false;
                    try Lifted.appendChildren(self.allocator, lifted, expr_id, &stack);
                },
                .block => |block| if (!self.isLiteralCrash(expr_id)) {
                    if (lifted.stmtSpan(block.statements).len != 0) return false;
                    try stack.append(self.allocator, .{ .expr = block.final_expr });
                },
                .if_ => {
                    // This admits guarded wrappers, not arbitrary conditional
                    // arguments inside otherwise call-through wrappers.
                    if (!try self.isInlineableWrapperBody(expr_id)) return false;
                    try Lifted.appendChildren(self.allocator, lifted, expr_id, &stack);
                },
                .lambda,
                .fn_def,
                .let_,
                .match_,
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
                .comptime_exhaustiveness_failed,
                => return false,
            }
        }
        return true;
    }

    fn localIsArg(local: Lifted.LocalId, args: anytype) bool {
        for (0..args.len) |index| {
            const arg = GuardedList.at(args, index);
            if (arg.local == local) return true;
        }
        return false;
    }

    /// What a wrapper position must be: a whole wrapper body, a guard arm
    /// (a literal crash or a wrapper body), or an operand a wrapper builds
    /// its result from (an argument, a literal, or another wrapper body).
    const WrapperRole = enum { body, arm, operand };

    /// Whether a body is a checked wrapper: one operation, a constant, or a
    /// single guard whose arms are each of those or a crash. Such a body does
    /// less work than its call costs, and substituting it at every call site
    /// exposes its guard and its constant arguments to LIR range analysis
    /// before backend instruction selection. `List.get` and the byte reads
    /// have this shape with a `Try` on each arm. Every position must qualify
    /// in its role, so positions are checked in any order on a work stack.
    fn isInlineableWrapperBody(self: *const InlineAnalyzer, root: Lifted.ExprId) std.mem.Allocator.Error!bool {
        const lifted = &self.solved.lifted;
        const Item = struct { role: WrapperRole, expr: Lifted.ExprId };
        var stack: std.ArrayList(Item) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .role = .body, .expr = root });
        while (stack.pop()) |item| {
            const expr = lifted.getExpr(item.expr);
            const role: WrapperRole = switch (item.role) {
                .arm => if (self.isLiteralCrash(item.expr)) continue else .body,
                .operand => switch (expr.data) {
                    .local,
                    .unit,
                    .int_lit,
                    .frac_f32_lit,
                    .frac_f64_lit,
                    .dec_lit,
                    .str_lit,
                    .bytes_lit,
                    => continue,
                    .call_proc,
                    .low_level,
                    .tag,
                    .nominal,
                    .if_,
                    .block,
                    => .body,
                    .@"unreachable",
                    .crash,
                    .def_ref,
                    .fn_ref,
                    .list,
                    .tuple,
                    .record,
                    .record_update,
                    .static_data_candidate,
                    .inline_expects_enabled,
                    .comptime_value,
                    .typed_boundary,
                    .dbg,
                    .expect,
                    .return_,
                    .expect_err,
                    .literal_rejected,
                    .comptime_branch_taken,
                    .call_value,
                    .field_access,
                    .tuple_access,
                    .structural_eq,
                    .structural_hash,
                    .lambda,
                    .fn_def,
                    .let_,
                    .match_,
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
                    .comptime_exhaustiveness_failed,
                    => return false,
                },
                .body => .body,
            };
            std.debug.assert(role == .body);
            switch (expr.data) {
                .call_proc, .low_level => {},
                .tag => |tag| {
                    const payloads = lifted.exprSpan(tag.payloads);
                    for (0..payloads.len) |index| try stack.append(self.allocator, .{ .role = .operand, .expr = GuardedList.at(payloads, index) });
                },
                .nominal => |backing| try stack.append(self.allocator, .{ .role = .body, .expr = backing }),
                .if_ => |if_| {
                    const branches = lifted.ifBranchSpan(if_.branches);
                    if (branches.len != 1) return false;
                    try stack.append(self.allocator, .{ .role = .arm, .expr = GuardedList.at(branches, 0).body });
                    try stack.append(self.allocator, .{ .role = .arm, .expr = if_.final_else });
                },
                .block => |block| {
                    if (lifted.stmtSpan(block.statements).len != 0) return false;
                    try stack.append(self.allocator, .{ .role = .body, .expr = block.final_expr });
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .inline_expects_enabled, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return false,
            }
        }
        return true;
    }

    /// Whether an expression is a crash with a literal message, possibly
    /// under blocks that hold nothing else.
    fn isLiteralCrash(self: *const InlineAnalyzer, root: Lifted.ExprId) bool {
        const lifted = &self.solved.lifted;
        var expr_id = root;
        while (true) {
            switch (lifted.getExpr(expr_id).data) {
                .crash => return true,
                .low_level => |call| {
                    if (call.op != .crash) return false;
                    const args = lifted.exprSpan(call.args);
                    return args.len == 1 and
                        self.isStringLiteral(GuardedList.at(args, 0));
                },
                .block => |block| {
                    const stmts = lifted.stmtSpan(block.statements);
                    if (stmts.len == 0) {
                        expr_id = block.final_expr;
                        continue;
                    }
                    // SpecConstr normalizes a terminal crash to a statement
                    // followed by unreachable. No preceding work is admitted.
                    if (stmts.len != 1 or lifted.getExpr(block.final_expr).data != .@"unreachable") return false;
                    switch (lifted.getStmt(GuardedList.at(stmts, 0))) {
                        .crash => return true,
                        .expr => |child| expr_id = child,
                        .uninitialized, .let_, .expect, .dbg, .return_ => return false,
                    }
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .inline_expects_enabled, .comptime_value, .typed_boundary, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .continue_, .join_point, .jump, .return_, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return false,
            }
        }
    }

    fn isStringLiteral(self: *const InlineAnalyzer, root: Lifted.ExprId) bool {
        var expr_id = root;
        while (true) {
            switch (self.solved.lifted.getExpr(expr_id).data) {
                .str_lit => return true,
                .nominal => |backing| expr_id = backing,
                .typed_boundary => |boundary| expr_id = boundary.value,
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .bytes_lit, .static_data_candidate, .inline_expects_enabled, .comptime_value, .list, .tuple, .record, .record_update, .tag, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => return false,
            }
        }
    }

    fn markCycle(self: *InlineAnalyzer, repeated: Lifted.FnId) void {
        var cycle_start: ?usize = null;
        for (self.stack.items, 0..) |fn_id, index| {
            if (fn_id == repeated) {
                cycle_start = index;
                break;
            }
        }
        const start = cycle_start orelse Common.invariant("inline cycle did not refer to a visiting function");
        for (self.stack.items[start..]) |fn_id| {
            self.decisions[@intFromEnum(fn_id)] = .never;
        }
    }
};
