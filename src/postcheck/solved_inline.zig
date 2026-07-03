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

    fn solvedCaptureCount(self: *const WrapperAnalyzer, fn_id: Lifted.FnId) usize {
        const captures = self.solvedCapturesForFn(fn_id);
        return self.solved.types.captureSpan(captures).len;
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

    fn bodyReadsOnlyArgs(self: *const WrapperAnalyzer, fn_id: Lifted.FnId, body: Lifted.ExprId) bool {
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(fn_id)];
        return self.exprReadsOnlyArgs(body, self.solved.lifted.typedLocalSpan(source_fn.args));
    }

    fn exprReadsOnlyArgs(self: *const WrapperAnalyzer, expr_id: Lifted.ExprId, args: []const Lifted.TypedLocal) bool {
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .local => |local| localIsArg(local, args),
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
                for (self.solved.lifted.fieldExprSpan(fields)) |field| {
                    if (!self.exprReadsOnlyArgs(field.value, args)) return false;
                }
                return true;
            },
            .tag => |tag| self.exprSpanReadsOnlyArgs(tag.payloads, args),
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
            .crash,
            .comptime_exhaustiveness_failed,
            => false,
        };
    }

    fn exprSpanReadsOnlyArgs(self: *const WrapperAnalyzer, span: Lifted.Span(Lifted.ExprId), args: []const Lifted.TypedLocal) bool {
        for (self.solved.lifted.exprSpan(span)) |expr| {
            if (!self.exprReadsOnlyArgs(expr, args)) return false;
        }
        return true;
    }

    fn captureOperandSpanReadsOnlyArgs(self: *const WrapperAnalyzer, span: Lifted.Span(Lifted.CaptureOperand), args: []const Lifted.TypedLocal) bool {
        for (self.solved.lifted.captureOperandSpan(span)) |operand| {
            if (!self.exprReadsOnlyArgs(operand.value, args)) return false;
        }
        return true;
    }

    fn localIsArg(local: Lifted.LocalId, args: []const Lifted.TypedLocal) bool {
        for (args) |arg| {
            if (arg.local == local) return true;
        }
        return false;
    }

    fn isInlineableWrapperBody(self: *const WrapperAnalyzer, expr_id: Lifted.ExprId) bool {
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .call_proc, .low_level => true,
            .block => |block| self.solved.lifted.stmtSpan(block.statements).len == 0 and
                self.isInlineableWrapperBody(block.final_expr),
            else => false,
        };
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
            .bytes_lit,
            .def_ref,
            => {},
            .fn_ref => |fn_ref| try self.visitCaptureOperandSpanCallees(fn_ref.captures),
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
            .return_ => |ret| try self.visitBodyCallees(ret.value),
            .expect_err => |expect_err| try self.visitBodyCallees(expect_err.msg),
            .comptime_branch_taken => |taken| try self.visitBodyCallees(taken.body),
            .call_value => |call| {
                try self.visitBodyCallees(call.callee);
                try self.visitSpanCallees(call.args);
            },
            .call_proc => |call| {
                if (Lifted.localDirectCallee(call)) |callee| {
                    _ = try self.inlineBody(callee);
                }
                try self.visitSpanCallees(call.args);
                try self.visitCaptureOperandSpanCallees(call.captures);
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
            .block => |block| try self.visitBodyCallees(block.final_expr),
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

    fn visitCaptureOperandSpanCallees(self: *WrapperAnalyzer, span: Lifted.Span(Lifted.CaptureOperand)) std.mem.Allocator.Error!void {
        for (self.solved.lifted.captureOperandSpan(span)) |operand| {
            try self.visitBodyCallees(operand.value);
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
