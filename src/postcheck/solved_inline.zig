//! Explicit inline eligibility analysis over Lambda Solved IR.

const std = @import("std");

const Common = @import("common.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");

/// Post-check inline analysis mode.
pub const Mode = enum {
    none,
    direct_call_wrappers,
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
        .direct_call_wrappers => try DirectCallWrapperAnalyzer.run(allocator, solved),
    };
}

const Decision = union(enum) {
    unknown,
    visiting,
    never,
    inline_direct_call: Lifted.ExprId,
};

const Candidate = struct {
    body: Lifted.ExprId,
    callee: Lifted.FnId,
};

const DirectCallWrapperAnalyzer = struct {
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

        var analyzer = DirectCallWrapperAnalyzer{
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
                .inline_direct_call => |body| body,
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

    fn inlineBody(self: *DirectCallWrapperAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Lifted.ExprId {
        const index = @intFromEnum(fn_id);
        switch (self.decisions[index]) {
            .unknown => {},
            .visiting => {
                self.markCycle(fn_id);
                return null;
            },
            .never => return null,
            .inline_direct_call => |body| return body,
        }

        self.decisions[index] = .visiting;
        try self.stack.append(self.allocator, fn_id);
        defer {
            const popped = self.stack.pop() orelse Common.invariant("inline analysis stack underflow");
            if (popped != fn_id) Common.invariant("inline analysis stack was corrupted");
        }

        const wrapper = self.wrapperCandidate(fn_id) orelse {
            self.decisions[index] = .never;
            return null;
        };

        _ = try self.inlineBody(wrapper.callee);

        switch (self.decisions[index]) {
            .never => return null,
            .visiting => {},
            .unknown,
            .inline_direct_call,
            => Common.invariant("inline analysis decision changed unexpectedly while visiting a candidate"),
        }

        self.decisions[index] = .{ .inline_direct_call = wrapper.body };
        return wrapper.body;
    }

    fn wrapperCandidate(self: *const DirectCallWrapperAnalyzer, fn_id: Lifted.FnId) ?Candidate {
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(fn_id)];
        if (self.solved.lifted.typedLocalSpan(source_fn.captures).len != 0) return null;
        if (self.solvedCaptureCount(fn_id) != 0) return null;

        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };

        const callee = self.directCallCalleeFromBody(body) orelse return null;
        return .{ .body = body, .callee = callee };
    }

    fn solvedCaptureCount(self: *const DirectCallWrapperAnalyzer, fn_id: Lifted.FnId) usize {
        const captures = self.solvedCapturesForFn(fn_id);
        return self.solved.types.captureSpan(captures).len;
    }

    fn solvedCapturesForFn(self: *const DirectCallWrapperAnalyzer, fn_id: Lifted.FnId) SolvedType.Span {
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

    fn directCallCalleeFromBody(self: *const DirectCallWrapperAnalyzer, expr_id: Lifted.ExprId) ?Lifted.FnId {
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .call_proc => |call| if (call.is_cold) null else Lifted.callProcCallee(call),
            .block => |block| blk: {
                if (self.solved.lifted.stmtSpan(block.statements).len != 0) return null;
                const final_expr = self.solved.lifted.exprs.items[@intFromEnum(block.final_expr)];
                break :blk switch (final_expr.data) {
                    .call_proc => |call| if (call.is_cold) null else Lifted.callProcCallee(call),
                    else => null,
                };
            },
            else => null,
        };
    }

    fn markCycle(self: *DirectCallWrapperAnalyzer, repeated: Lifted.FnId) void {
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
