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
pub fn analyze(
    allocator: std.mem.Allocator,
    mode: Mode,
    procedure_usage: SpecConstr.ProcedureUsage,
    solved: *const Solved.Program,
) std.mem.Allocator.Error!OwnedPlan {
    return switch (mode) {
        .none => OwnedPlan.empty(allocator),
        .wrappers => try InlineAnalyzer.run(allocator, procedure_usage, solved),
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

    fn run(
        allocator: std.mem.Allocator,
        procedure_usage: SpecConstr.ProcedureUsage,
        solved: *const Solved.Program,
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
            analyzer.resolveSingleUseMaterialization(fn_id, materialization_states);
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

    fn inlineBody(self: *InlineAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Lifted.ExprId {
        const index = @intFromEnum(fn_id);
        switch (self.decisions[index]) {
            .unknown => {},
            .visiting => {
                self.markCycle(fn_id);
                return null;
            },
            .never => return null,
            .inline_body => |candidate| return candidate.body,
        }

        self.decisions[index] = .visiting;
        try self.stack.append(self.allocator, fn_id);
        defer {
            const popped = self.stack.pop() orelse Common.invariant("inline analysis stack underflow");
            if (popped != fn_id) Common.invariant("inline analysis stack was corrupted");
        }

        const candidate = self.inlineCandidate(fn_id) orelse {
            self.decisions[index] = .never;
            return null;
        };

        // Visit every proc called anywhere in the candidate body, including calls
        // nested inside low-level operands or other call arguments. A self-call
        // re-enters this function while it is `.visiting`, so `markCycle` marks
        // the whole cycle `.never` and keeps it out of the inline plan instead
        // of inlining it without bound.
        if (!try self.visitBodyCallees(candidate.body, 0)) {
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

        self.decisions[index] = .{ .inline_body = candidate };
        return candidate.body;
    }

    fn inlineCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) ?Candidate {
        if (self.wrapperCandidate(fn_id)) |body| return .{ .body = body, .kind = .wrapper };
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

    /// Resolve outer single-use candidates before their descendants. Demoting
    /// an outer body creates a procedure boundary, which can make a nested
    /// single-use body safe to inline exactly once.
    fn resolveSingleUseMaterialization(
        self: *InlineAnalyzer,
        fn_id: Lifted.FnId,
        states: []MaterializationState,
    ) void {
        const index = @intFromEnum(fn_id);
        const candidate = switch (self.decisions[index]) {
            .inline_body => |candidate| candidate,
            .never => {
                states[index] = .once;
                return;
            },
            .unknown,
            .visiting,
            => Common.invariant("inline materialization analysis saw an unfinished decision"),
        };
        if (candidate.kind != .single_use) return;

        switch (states[index]) {
            .unknown => states[index] = .visiting,
            .visiting => Common.invariant("single-use call-owner graph contained a selected cycle"),
            .once => return,
            .multiple => Common.invariant("resolved single-use body still had multiple materializations"),
        }

        const use = self.procedure_usage.get(fn_id);
        const owner = use.external_call_owner orelse
            Common.invariant("single-use function had no external call owner");
        if (!self.bodyMaterializedOnce(owner, states)) {
            self.decisions[index] = .never;
        }
        states[index] = .once;
    }

    /// Whether the selected inline plan lowers this source body in exactly one
    /// place. A procedure boundary owns one body materialization. A selected
    /// body instead inherits the materialization count of its unique caller;
    /// multiple direct inline sites or a simultaneous value/procedure use stop
    /// the proof.
    fn bodyMaterializedOnce(
        self: *InlineAnalyzer,
        fn_id: Lifted.FnId,
        states: []MaterializationState,
    ) bool {
        const index = @intFromEnum(fn_id);
        switch (self.decisions[index]) {
            .never => {
                states[index] = .once;
                return true;
            },
            .inline_body => |candidate| if (candidate.kind == .single_use) {
                self.resolveSingleUseMaterialization(fn_id, states);
                return true;
            },
            .unknown,
            .visiting,
            => Common.invariant("inline materialization analysis saw an unfinished owner decision"),
        }

        switch (states[index]) {
            .unknown => states[index] = .visiting,
            .visiting => Common.invariant("selected wrapper call-owner graph contained a cycle"),
            .once => return true,
            .multiple => return false,
        }

        const use = self.procedure_usage.get(fn_id);
        const once = if (use.external_calls == 0)
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

            const owner = use.external_call_owner orelse
                Common.invariant("single-call inline owner had no external call owner");
            break :blk self.bodyMaterializedOnce(owner, states);
        };
        states[index] = if (once) .once else .multiple;
        return once;
    }

    fn wrapperCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) ?Lifted.ExprId {
        const source_fn = self.solved.lifted.getFn(fn_id);
        if (self.solved.lifted.typedLocalSpan(source_fn.captures).len != 0) return null;
        if (self.solvedCaptureCount(fn_id) != 0) return null;

        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };

        if (!self.bodyReadsOnlyArgs(fn_id, body)) return null;
        if (!self.isInlineableWrapperBody(body)) return null;
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

    fn bodyReadsOnlyArgs(self: *const InlineAnalyzer, fn_id: Lifted.FnId, body: Lifted.ExprId) bool {
        const source_fn = self.solved.lifted.getFn(fn_id);
        return self.exprReadsOnlyArgs(body, self.solved.lifted.typedLocalSpan(source_fn.args));
    }

    fn exprReadsOnlyArgs(self: *const InlineAnalyzer, expr_id: Lifted.ExprId, args: anytype) bool {
        const expr = self.solved.lifted.getExpr(expr_id);
        return switch (expr.data) {
            .local => |local| localIsArg(local, args),
            .@"unreachable",
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .def_ref,
            => true,
            .fn_ref => |fn_ref| self.captureOperandSpanReadsOnlyArgs(fn_ref.captures, args),
            .list,
            .tuple,
            => |items| self.exprSpanReadsOnlyArgs(items, args),
            .record => |fields| {
                const field_exprs = self.solved.lifted.fieldExprSpan(fields);
                for (0..field_exprs.len) |index| {
                    const field = GuardedList.at(field_exprs, index);
                    if (!self.exprReadsOnlyArgs(field.value, args)) return false;
                }
                return true;
            },
            .record_update => |update| {
                if (!self.exprReadsOnlyArgs(update.base, args)) return false;
                const field_exprs = self.solved.lifted.fieldExprSpan(update.fields);
                for (0..field_exprs.len) |index| {
                    const field = GuardedList.at(field_exprs, index);
                    if (!self.exprReadsOnlyArgs(field.value, args)) return false;
                }
                return true;
            },
            .tag => |tag| self.exprSpanReadsOnlyArgs(tag.payloads, args),
            .static_data_candidate => |candidate| self.exprReadsOnlyArgs(candidate.runtime_expr, args),
            .nominal,
            .dbg,
            .expect,
            => |child| self.exprReadsOnlyArgs(child, args),
            .return_ => |ret| self.exprReadsOnlyArgs(ret.value, args),
            .expect_err => |expect_err| self.exprReadsOnlyArgs(expect_err.msg, args),
            .comptime_branch_taken => |taken| self.exprReadsOnlyArgs(taken.body, args),
            .call_value => |call| self.exprReadsOnlyArgs(call.callee, args) and self.exprSpanReadsOnlyArgs(call.args, args),
            .call_proc => |call| !call.is_cold and
                self.exprSpanReadsOnlyArgs(call.args, args) and
                self.captureOperandSpanReadsOnlyArgs(call.captures, args),
            .low_level => |call| self.exprSpanReadsOnlyArgs(call.args, args),
            .field_access => |field| self.exprReadsOnlyArgs(field.receiver, args),
            .tuple_access => |access| self.exprReadsOnlyArgs(access.tuple, args),
            .structural_eq => |eq| self.exprReadsOnlyArgs(eq.lhs, args) and self.exprReadsOnlyArgs(eq.rhs, args),
            .structural_hash => |h| self.exprReadsOnlyArgs(h.value, args) and self.exprReadsOnlyArgs(h.hasher, args),
            .block => |block| self.solved.lifted.stmtSpan(block.statements).len == 0 and self.exprReadsOnlyArgs(block.final_expr, args),
            .lambda,
            .fn_def,
            .let_,
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
            .crash,
            .comptime_exhaustiveness_failed,
            => false,
        };
    }

    fn exprSpanReadsOnlyArgs(self: *const InlineAnalyzer, span: Lifted.Span(Lifted.ExprId), args: anytype) bool {
        const exprs = self.solved.lifted.exprSpan(span);
        for (0..exprs.len) |index| {
            const expr = GuardedList.at(exprs, index);
            if (!self.exprReadsOnlyArgs(expr, args)) return false;
        }
        return true;
    }

    fn captureOperandSpanReadsOnlyArgs(self: *const InlineAnalyzer, span: Lifted.Span(Lifted.CaptureOperand), args: anytype) bool {
        const operands = self.solved.lifted.captureOperandSpan(span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            if (!self.exprReadsOnlyArgs(operand.value, args)) return false;
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

    fn isInlineableWrapperBody(self: *const InlineAnalyzer, expr_id: Lifted.ExprId) bool {
        const expr = self.solved.lifted.getExpr(expr_id);
        if (expr.data == .call_proc or expr.data == .low_level) return true;
        if (expr.data != .block) return false;
        return self.solved.lifted.stmtSpan(expr.data.block.statements).len == 0 and
            self.isInlineableWrapperBody(expr.data.block.final_expr);
    }

    /// Visit every proc called within an inline candidate so cycles consisting
    /// entirely of selected bodies are rejected before lowering. At the same
    /// time, prove that every break and continue is owned by a loop inside the
    /// body; combining the checks avoids a second candidate-body traversal.
    fn visitBodyCallees(self: *InlineAnalyzer, expr_id: Lifted.ExprId, loop_depth: usize) std.mem.Allocator.Error!bool {
        const expr = self.solved.lifted.getExpr(expr_id);
        return switch (expr.data) {
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
            => true,
            .fn_ref => |fn_ref| try self.visitCaptureOperandSpanCallees(fn_ref.captures, loop_depth),
            .list,
            .tuple,
            => |items| try self.visitSpanCallees(items, loop_depth),
            .record => |fields| {
                const field_exprs = self.solved.lifted.fieldExprSpan(fields);
                for (0..field_exprs.len) |index| {
                    const field = GuardedList.at(field_exprs, index);
                    if (!try self.visitBodyCallees(field.value, loop_depth)) return false;
                }
                return true;
            },
            .record_update => |update| {
                if (!try self.visitBodyCallees(update.base, loop_depth)) return false;
                const field_exprs = self.solved.lifted.fieldExprSpan(update.fields);
                for (0..field_exprs.len) |index| {
                    const field = GuardedList.at(field_exprs, index);
                    if (!try self.visitBodyCallees(field.value, loop_depth)) return false;
                }
                return true;
            },
            .tag => |tag| try self.visitSpanCallees(tag.payloads, loop_depth),
            .static_data_candidate => |candidate| try self.visitBodyCallees(candidate.runtime_expr, loop_depth),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.visitBodyCallees(child, loop_depth),
            .return_ => false,
            .expect_err => |expect_err| try self.visitBodyCallees(expect_err.msg, loop_depth),
            .comptime_branch_taken => |taken| try self.visitBodyCallees(taken.body, loop_depth),
            .let_ => |let_| {
                if (!try self.visitBodyCallees(let_.value, loop_depth)) return false;
                return try self.visitBodyCallees(let_.rest, loop_depth);
            },
            .call_value => |call| {
                if (!try self.visitBodyCallees(call.callee, loop_depth)) return false;
                return try self.visitSpanCallees(call.args, loop_depth);
            },
            .call_proc => |call| {
                if (Lifted.localDirectCallee(call)) |callee| {
                    _ = try self.inlineBody(callee);
                }
                if (!try self.visitSpanCallees(call.args, loop_depth)) return false;
                return try self.visitCaptureOperandSpanCallees(call.captures, loop_depth);
            },
            .low_level => |call| try self.visitSpanCallees(call.args, loop_depth),
            .field_access => |field| try self.visitBodyCallees(field.receiver, loop_depth),
            .tuple_access => |access| try self.visitBodyCallees(access.tuple, loop_depth),
            .structural_eq => |eq| {
                if (!try self.visitBodyCallees(eq.lhs, loop_depth)) return false;
                return try self.visitBodyCallees(eq.rhs, loop_depth);
            },
            .structural_hash => |h| {
                if (!try self.visitBodyCallees(h.value, loop_depth)) return false;
                return try self.visitBodyCallees(h.hasher, loop_depth);
            },
            .match_ => |match_| {
                if (!try self.visitBodyCallees(match_.scrutinee, loop_depth)) return false;
                const branches = self.solved.lifted.branchSpan(match_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    if (!try self.visitStmtSpanCallees(branch.bindings, loop_depth)) return false;
                    if (branch.guard) |guard| {
                        if (!try self.visitBodyCallees(guard, loop_depth)) return false;
                    }
                    if (!try self.visitBodyCallees(branch.body, loop_depth)) return false;
                }
                return true;
            },
            .if_ => |if_| {
                const branches = self.solved.lifted.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    if (!try self.visitBodyCallees(branch.cond, loop_depth)) return false;
                    if (!try self.visitBodyCallees(branch.body, loop_depth)) return false;
                }
                return try self.visitBodyCallees(if_.final_else, loop_depth);
            },
            .block => |block| {
                if (!try self.visitStmtSpanCallees(block.statements, loop_depth)) return false;
                return try self.visitBodyCallees(block.final_expr, loop_depth);
            },
            .loop_ => |loop| {
                if (!try self.visitSpanCallees(loop.initial_values, loop_depth)) return false;
                return try self.visitBodyCallees(loop.body, loop_depth + 1);
            },
            .break_ => |maybe| if (loop_depth == 0)
                false
            else if (maybe) |value|
                try self.visitBodyCallees(value, loop_depth)
            else
                true,
            .continue_ => |continue_| loop_depth != 0 and try self.visitSpanCallees(continue_.values, loop_depth),
            .join_point => |join_point| {
                if (!try self.visitBodyCallees(join_point.body, loop_depth)) return false;
                return try self.visitBodyCallees(join_point.remainder, loop_depth);
            },
            .jump => |jump| try self.visitSpanCallees(jump.args, loop_depth),
            .if_initialized_payload => |payload_switch| {
                if (!try self.visitBodyCallees(payload_switch.cond, loop_depth)) return false;
                if (!try self.visitBodyCallees(payload_switch.initialized, loop_depth)) return false;
                return try self.visitBodyCallees(payload_switch.uninitialized, loop_depth);
            },
            .try_sequence => |sequence| {
                if (!try self.visitBodyCallees(sequence.try_expr, loop_depth)) return false;
                return try self.visitBodyCallees(sequence.ok_body, loop_depth);
            },
            .try_record_sequence => |sequence| {
                if (!try self.visitBodyCallees(sequence.try_expr, loop_depth)) return false;
                return try self.visitBodyCallees(sequence.ok_body, loop_depth);
            },
            .lambda,
            .fn_def,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .comptime_exhaustiveness_failed,
            => true,
        };
    }

    fn visitSpanCallees(self: *InlineAnalyzer, span: Lifted.Span(Lifted.ExprId), loop_depth: usize) std.mem.Allocator.Error!bool {
        const exprs = self.solved.lifted.exprSpan(span);
        for (0..exprs.len) |index| {
            const child = GuardedList.at(exprs, index);
            if (!try self.visitBodyCallees(child, loop_depth)) return false;
        }
        return true;
    }

    fn visitStmtSpanCallees(self: *InlineAnalyzer, span: Lifted.Span(Lifted.StmtId), loop_depth: usize) std.mem.Allocator.Error!bool {
        const stmts = self.solved.lifted.stmtSpan(span);
        for (0..stmts.len) |index| {
            const closed = switch (self.solved.lifted.getStmt(GuardedList.at(stmts, index))) {
                .let_ => |let_| try self.visitBodyCallees(let_.value, loop_depth),
                .expr,
                .expect,
                .dbg,
                => |child| try self.visitBodyCallees(child, loop_depth),
                .return_ => false,
                .uninitialized,
                .crash,
                => true,
            };
            if (!closed) return false;
        }
        return true;
    }

    fn visitCaptureOperandSpanCallees(self: *InlineAnalyzer, span: Lifted.Span(Lifted.CaptureOperand), loop_depth: usize) std.mem.Allocator.Error!bool {
        const operands = self.solved.lifted.captureOperandSpan(span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            if (!try self.visitBodyCallees(operand.value, loop_depth)) return false;
        }
        return true;
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
