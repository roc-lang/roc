//! Explicit inline eligibility analysis over Lambda Solved IR.

const std = @import("std");

const Common = @import("common.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");

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
    solved: *const Solved.Program,
) std.mem.Allocator.Error!OwnedPlan {
    return switch (mode) {
        .none => OwnedPlan.empty(allocator),
        .wrappers => try WrapperAnalyzer.run(allocator, solved),
    };
}

const Decision = union(enum) {
    unknown,
    visiting,
    never,
    inline_body: Lifted.ExprId,
};

const WrapperAnalyzer = struct {
    allocator: std.mem.Allocator,
    solved: *const Solved.Program,
    decisions: []Decision,
    stack: std.ArrayList(Lifted.FnId),

    fn run(
        allocator: std.mem.Allocator,
        solved: *const Solved.Program,
    ) std.mem.Allocator.Error!OwnedPlan {
        const decisions = try allocator.alloc(Decision, solved.lifted.fns.items.len);
        errdefer allocator.free(decisions);
        @memset(decisions, .unknown);

        var analyzer = WrapperAnalyzer{
            .allocator = allocator,
            .solved = solved,
            .decisions = decisions,
            .stack = .empty,
        };
        defer analyzer.stack.deinit(allocator);

        for (solved.lifted.fns.items, 0..) |_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            _ = try analyzer.inlineBody(fn_id);
        }

        const inline_bodies = try allocator.alloc(?Lifted.ExprId, decisions.len);
        errdefer allocator.free(inline_bodies);
        for (decisions, 0..) |decision, index| {
            inline_bodies[index] = switch (decision) {
                .inline_body => |body| body,
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

    fn inlineBody(self: *WrapperAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Lifted.ExprId {
        const index = @intFromEnum(fn_id);
        switch (self.decisions[index]) {
            .unknown => {},
            .visiting => {
                self.markCycle(fn_id);
                return null;
            },
            .never => return null,
            .inline_body => |body| return body,
        }

        self.decisions[index] = .visiting;
        try self.stack.append(self.allocator, fn_id);
        defer {
            const popped = self.stack.pop() orelse Common.invariant("inline analysis stack underflow");
            if (popped != fn_id) Common.invariant("inline analysis stack was corrupted");
        }

        const wrapper_body = self.wrapperCandidate(fn_id) orelse {
            self.decisions[index] = .never;
            return null;
        };

        // Visit every proc called anywhere in the wrapper body, including calls
        // nested inside low-level operands or other call arguments. A self-call
        // re-enters this function while it is `.visiting`, so `markCycle` marks
        // the whole cycle `.never` and keeps it out of the inline plan instead
        // of inlining it without bound.
        try self.visitBodyCallees(wrapper_body);

        switch (self.decisions[index]) {
            .never => return null,
            .visiting => {},
            .unknown,
            .inline_body,
            => Common.invariant("inline analysis decision changed unexpectedly while visiting a candidate"),
        }

        self.decisions[index] = .{ .inline_body = wrapper_body };
        return wrapper_body;
    }

    fn wrapperCandidate(self: *const WrapperAnalyzer, fn_id: Lifted.FnId) ?Lifted.ExprId {
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(fn_id)];

        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };

        if (exprContainsReturn(&self.solved.lifted, body)) return null;
        if (!self.bodyReadsOnlyInlineInputs(fn_id, body)) return null;
        if (!self.isInlineableWrapperBody(body)) return null;
        return body;
    }

    fn solvedCapturesForFn(self: *const WrapperAnalyzer, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        const func = switch (self.solved.types.rootContent(self.solved.fn_tys.items[@intFromEnum(fn_id)])) {
            .func => |func| func,
            else => Common.invariant("direct Lambda Mono function table contains a non-function type"),
        };
        const callable = switch (self.solved.types.rootContent(func.callable)) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("callable value did not have a resolved callable slot"),
        };
        for (self.solved.types.memberSpan(callable)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        return .empty();
    }

    fn bodyReadsOnlyInlineInputs(self: *const WrapperAnalyzer, fn_id: Lifted.FnId, body: Lifted.ExprId) bool {
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(fn_id)];
        return self.exprReadsOnlyInlineInputs(
            body,
            self.solved.lifted.typedLocalSpan(source_fn.args),
            self.solved.lifted.typedLocalSpan(source_fn.captures),
            self.solved.types.captureSpan(self.solvedCapturesForFn(fn_id)),
        );
    }

    fn exprReadsOnlyInlineInputs(
        self: *const WrapperAnalyzer,
        expr_id: Lifted.ExprId,
        args: []const Lifted.TypedLocal,
        source_captures: []const Lifted.TypedLocal,
        solved_captures: []const SolvedType.Capture,
    ) bool {
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .local => |local| localIsInlineInput(local, args, source_captures, solved_captures),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .def_ref,
            => true,
            .static_data_candidate => |candidate| self.exprReadsOnlyInlineInputs(candidate.fallback, args, source_captures, solved_captures),
            .list,
            .tuple,
            => |items| self.exprSpanReadsOnlyInlineInputs(items, args, source_captures, solved_captures),
            .record => |fields| {
                for (self.solved.lifted.fieldExprSpan(fields)) |field| {
                    if (!self.exprReadsOnlyInlineInputs(field.value, args, source_captures, solved_captures)) return false;
                }
                return true;
            },
            .tag => |tag| self.exprSpanReadsOnlyInlineInputs(tag.payloads, args, source_captures, solved_captures),
            .nominal,
            .dbg,
            .expect,
            .return_,
            => |child| self.exprReadsOnlyInlineInputs(child, args, source_captures, solved_captures),
            .expect_err => |expect_err| self.exprReadsOnlyInlineInputs(expect_err.msg, args, source_captures, solved_captures),
            .comptime_branch_taken => |taken| self.exprReadsOnlyInlineInputs(taken.body, args, source_captures, solved_captures),
            .call_value => |call| self.exprReadsOnlyInlineInputs(call.callee, args, source_captures, solved_captures) and
                self.exprSpanReadsOnlyInlineInputs(call.args, args, source_captures, solved_captures),
            .call_proc => |call| !call.is_cold and self.exprSpanReadsOnlyInlineInputs(call.args, args, source_captures, solved_captures),
            .low_level => |call| self.exprSpanReadsOnlyInlineInputs(call.args, args, source_captures, solved_captures),
            .field_access => |field| self.exprReadsOnlyInlineInputs(field.receiver, args, source_captures, solved_captures),
            .tuple_access => |access| self.exprReadsOnlyInlineInputs(access.tuple, args, source_captures, solved_captures),
            .structural_eq => |eq| self.exprReadsOnlyInlineInputs(eq.lhs, args, source_captures, solved_captures) and
                self.exprReadsOnlyInlineInputs(eq.rhs, args, source_captures, solved_captures),
            .structural_hash => |h| self.exprReadsOnlyInlineInputs(h.value, args, source_captures, solved_captures) and
                self.exprReadsOnlyInlineInputs(h.hasher, args, source_captures, solved_captures),
            .block => |block| self.solved.lifted.stmtSpan(block.statements).len == 0 and
                self.exprReadsOnlyInlineInputs(block.final_expr, args, source_captures, solved_captures),
            .lambda,
            // A function reference can carry a nested body whose captures must
            // be rebound for each inline site. This inliner only remaps the
            // immediate callee body, so closure-producing wrappers are not
            // transparent here.
            .fn_ref,
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
            .state_loop,
            .break_,
            .continue_,
            .state_continue,
            .crash,
            .comptime_exhaustiveness_failed,
            => false,
        };
    }

    fn exprSpanReadsOnlyInlineInputs(
        self: *const WrapperAnalyzer,
        span: Lifted.Span(Lifted.ExprId),
        args: []const Lifted.TypedLocal,
        source_captures: []const Lifted.TypedLocal,
        solved_captures: []const SolvedType.Capture,
    ) bool {
        for (self.solved.lifted.exprSpan(span)) |expr| {
            if (!self.exprReadsOnlyInlineInputs(expr, args, source_captures, solved_captures)) return false;
        }
        return true;
    }

    fn localIsInlineInput(
        local: Lifted.LocalId,
        args: []const Lifted.TypedLocal,
        source_captures: []const Lifted.TypedLocal,
        solved_captures: []const SolvedType.Capture,
    ) bool {
        for (args) |arg| {
            if (arg.local == local) return true;
        }
        for (source_captures) |capture| {
            if (capture.local != local) continue;
            for (solved_captures) |solved_capture| {
                if (solved_capture.local == local) return true;
            }
            return false;
        }
        return false;
    }

    fn isInlineableWrapperBody(self: *const WrapperAnalyzer, expr_id: Lifted.ExprId) bool {
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
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
            .call_proc, .low_level => true,
            .field_access => |field| self.isInlineableWrapperBody(field.receiver),
            .tuple_access => |access| self.isInlineableWrapperBody(access.tuple),
            .tuple,
            => |items| self.exprSpanIsInlineableWrapperBody(items),
            .record => |fields| {
                for (self.solved.lifted.fieldExprSpan(fields)) |field| {
                    if (!self.isInlineableWrapperBody(field.value)) return false;
                }
                return true;
            },
            .tag => |tag| self.exprSpanIsInlineableWrapperBody(tag.payloads),
            .nominal => |backing| self.isInlineableWrapperBody(backing),
            .block => |block| self.solved.lifted.stmtSpan(block.statements).len == 0 and
                self.isInlineableWrapperBody(block.final_expr),
            else => false,
        };
    }

    fn exprSpanIsInlineableWrapperBody(self: *const WrapperAnalyzer, span: Lifted.Span(Lifted.ExprId)) bool {
        for (self.solved.lifted.exprSpan(span)) |expr| {
            if (!self.isInlineableWrapperBody(expr)) return false;
        }
        return true;
    }

    /// Visit every proc called within a wrapper body so inline cycles are
    /// detected even when the recursive call is nested inside low-level
    /// operands or other call arguments, mirroring the shapes accepted by
    /// `exprReadsOnlyArgs`.
    fn visitBodyCallees(self: *WrapperAnalyzer, expr_id: Lifted.ExprId) std.mem.Allocator.Error!void {
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .static_data,
            .def_ref,
            .fn_ref,
            => {},
            .static_data_candidate => |candidate| try self.visitBodyCallees(candidate.fallback),
            .list,
            .tuple,
            => |items| try self.visitSpanCallees(items),
            .record => |fields| {
                for (self.solved.lifted.fieldExprSpan(fields)) |field| {
                    try self.visitBodyCallees(field.value);
                }
            },
            .tag => |tag| try self.visitSpanCallees(tag.payloads),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.visitBodyCallees(child),
            .expect_err => |expect_err| try self.visitBodyCallees(expect_err.msg),
            .comptime_branch_taken => |taken| try self.visitBodyCallees(taken.body),
            .call_value => |call| {
                try self.visitBodyCallees(call.callee);
                try self.visitSpanCallees(call.args);
            },
            .call_proc => |call| {
                _ = try self.inlineBody(Lifted.callProcCallee(call));
                try self.visitSpanCallees(call.args);
            },
            .low_level => |call| try self.visitSpanCallees(call.args),
            .field_access => |field| try self.visitBodyCallees(field.receiver),
            .tuple_access => |access| try self.visitBodyCallees(access.tuple),
            .structural_eq => |eq| {
                try self.visitBodyCallees(eq.lhs);
                try self.visitBodyCallees(eq.rhs);
            },
            .structural_hash => |h| {
                try self.visitBodyCallees(h.value);
                try self.visitBodyCallees(h.hasher);
            },
            .block => |block| {
                try self.visitStmtSpanCallees(block.statements);
                try self.visitBodyCallees(block.final_expr);
            },
            .let_ => |let_| {
                try self.visitBodyCallees(let_.value);
                try self.visitBodyCallees(let_.rest);
            },
            .match_ => |match| {
                try self.visitBodyCallees(match.scrutinee);
                for (self.solved.lifted.branchSpan(match.branches)) |branch| {
                    if (branch.guard) |guard| try self.visitBodyCallees(guard);
                    try self.visitBodyCallees(branch.body);
                }
            },
            .if_ => |if_| {
                for (self.solved.lifted.ifBranchSpan(if_.branches)) |branch| {
                    try self.visitBodyCallees(branch.cond);
                    try self.visitBodyCallees(branch.body);
                }
                try self.visitBodyCallees(if_.final_else);
            },
            .if_initialized_payload => |payload_switch| {
                try self.visitBodyCallees(payload_switch.cond);
                try self.visitBodyCallees(payload_switch.initialized);
                try self.visitBodyCallees(payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.visitBodyCallees(sequence.try_expr);
                try self.visitBodyCallees(sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.visitBodyCallees(sequence.try_expr);
                try self.visitBodyCallees(sequence.ok_body);
            },
            .loop_ => |loop| {
                try self.visitSpanCallees(loop.initial_values);
                try self.visitBodyCallees(loop.body);
            },
            .state_loop => |state_loop| {
                try self.visitSpanCallees(state_loop.entry_values);
                for (self.solved.lifted.stateLoopStateSpan(state_loop.states)) |state| {
                    try self.visitBodyCallees(state.body);
                }
            },
            .break_ => |maybe| if (maybe) |value| try self.visitBodyCallees(value),
            .continue_ => |continue_| try self.visitSpanCallees(continue_.values),
            .state_continue => |continue_| try self.visitSpanCallees(continue_.values),
            .return_ => Common.invariant("return-containing inline candidate reached callee visitor"),
            .lambda,
            .fn_def,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .comptime_exhaustiveness_failed,
            => {},
        }
    }

    fn visitSpanCallees(self: *WrapperAnalyzer, span: Lifted.Span(Lifted.ExprId)) std.mem.Allocator.Error!void {
        for (self.solved.lifted.exprSpan(span)) |child| {
            try self.visitBodyCallees(child);
        }
    }

    fn visitStmtSpanCallees(self: *WrapperAnalyzer, span: Lifted.Span(Lifted.StmtId)) std.mem.Allocator.Error!void {
        for (self.solved.lifted.stmtSpan(span)) |stmt_id| {
            try self.visitStmtCallees(stmt_id);
        }
    }

    fn visitStmtCallees(self: *WrapperAnalyzer, stmt_id: Lifted.StmtId) std.mem.Allocator.Error!void {
        switch (self.solved.lifted.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| try self.visitBodyCallees(let_.value),
            .expr,
            .expect,
            .dbg,
            .return_,
            => |expr| try self.visitBodyCallees(expr),
            .uninitialized,
            .crash,
            => {},
        }
    }

    fn markCycle(self: *WrapperAnalyzer, repeated: Lifted.FnId) void {
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

fn exprContainsReturn(program: *const Lifted.Program, expr_id: Lifted.ExprId) bool {
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
        => Common.invariant("pre-lift function expression reached inline return scan"),
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

fn exprSpanContainsReturn(program: *const Lifted.Program, span: Lifted.Span(Lifted.ExprId)) bool {
    for (program.exprSpan(span)) |expr_id| {
        if (exprContainsReturn(program, expr_id)) return true;
    }
    return false;
}

fn stmtContainsReturn(program: *const Lifted.Program, stmt_id: Lifted.StmtId) bool {
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

fn stmtSpanContainsReturn(program: *const Lifted.Program, span: Lifted.Span(Lifted.StmtId)) bool {
    for (program.stmtSpan(span)) |stmt_id| {
        if (stmtContainsReturn(program, stmt_id)) return true;
    }
    return false;
}
