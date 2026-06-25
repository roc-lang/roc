//! Normalize compiler-internal iterator plan values before Lambda solving.
//!
//! Iterator plans are a post-check optimization representation. This is the
//! narrow boundary that prevents raw plan values from reaching ordinary
//! Lambda-to-LIR lowering. It is not a general cleanup optimizer: optimized
//! consumers may eventually consume plans here, and any remaining plan
//! expression must be replaced by its already-lowered public `Iter`
//! materialization.

const std = @import("std");

const Common = @import("common.zig");
const Lifted = @import("monotype_lifted/ast.zig");

const Allocator = std.mem.Allocator;

pub fn run(allocator: Allocator, program: *Lifted.Program) Common.LowerError!void {
    var rewrite = try Rewrite.init(allocator, program);
    defer rewrite.deinit();
    try rewrite.run();
}

const Rewrite = struct {
    allocator: Allocator,
    program: *Lifted.Program,
    expr_done: []bool,
    stmt_done: []bool,

    fn init(allocator: Allocator, program: *Lifted.Program) Allocator.Error!Rewrite {
        const expr_done = try allocator.alloc(bool, program.exprs.items.len);
        errdefer allocator.free(expr_done);
        @memset(expr_done, false);

        const stmt_done = try allocator.alloc(bool, program.stmts.items.len);
        errdefer allocator.free(stmt_done);
        @memset(stmt_done, false);

        return .{
            .allocator = allocator,
            .program = program,
            .expr_done = expr_done,
            .stmt_done = stmt_done,
        };
    }

    fn deinit(self: *Rewrite) void {
        self.allocator.free(self.stmt_done);
        self.allocator.free(self.expr_done);
    }

    fn run(self: *Rewrite) Common.LowerError!void {
        for (self.program.fns.items) |fn_| {
            switch (fn_.body) {
                .roc => |body| try self.rewriteExpr(body),
                .hosted => {},
            }
        }
    }

    fn rewriteStmt(self: *Rewrite, stmt_id: Lifted.StmtId) Common.LowerError!void {
        const index = @intFromEnum(stmt_id);
        if (self.stmt_done[index]) return;
        self.stmt_done[index] = true;

        switch (self.program.stmts.items[index]) {
            .uninitialized => {},
            .let_ => |let_| try self.rewriteExpr(let_.value),
            .expr,
            .expect,
            .dbg,
            .return_,
            => |expr| try self.rewriteExpr(expr),
            .crash => {},
        }
    }

    fn rewriteExpr(self: *Rewrite, expr_id: Lifted.ExprId) Common.LowerError!void {
        const index = @intFromEnum(expr_id);
        if (self.expr_done[index]) return;
        self.expr_done[index] = true;

        const data = self.program.exprs.items[index].data;
        switch (data) {
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
            .crash,
            .comptime_exhaustiveness_failed,
            .fn_ref,
            => {},
            .iter_plan => |plan_id| try self.materializeIterPlanExpr(expr_id, plan_id),
            .static_data_candidate => |candidate| try self.rewriteExpr(candidate.fallback),
            .list,
            .tuple,
            => |items| for (self.program.exprSpan(items)) |child| try self.rewriteExpr(child),
            .record => |fields| for (self.program.fieldExprSpan(fields)) |field| try self.rewriteExpr(field.value),
            .tag => |tag| for (self.program.exprSpan(tag.payloads)) |child| try self.rewriteExpr(child),
            .nominal,
            .return_,
            .dbg,
            .expect,
            => |child| try self.rewriteExpr(child),
            .expect_err => |expect_err| try self.rewriteExpr(expect_err.msg),
            .comptime_branch_taken => |taken| try self.rewriteExpr(taken.body),
            .let_ => |let_| {
                try self.rewriteExpr(let_.value);
                try self.rewriteExpr(let_.rest);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached iterator-plan elimination"),
            .call_value => |call| {
                try self.rewriteExpr(call.callee);
                for (self.program.exprSpan(call.args)) |arg| try self.rewriteExpr(arg);
            },
            .call_proc => |call| for (self.program.exprSpan(call.args)) |arg| try self.rewriteExpr(arg),
            .low_level => |call| for (self.program.exprSpan(call.args)) |arg| try self.rewriteExpr(arg),
            .field_access => |field| try self.rewriteExpr(field.receiver),
            .tuple_access => |access| try self.rewriteExpr(access.tuple),
            .structural_eq => |eq| {
                try self.rewriteExpr(eq.lhs);
                try self.rewriteExpr(eq.rhs);
            },
            .structural_hash => |h| {
                try self.rewriteExpr(h.value);
                try self.rewriteExpr(h.hasher);
            },
            .match_ => |match| {
                try self.rewriteExpr(match.scrutinee);
                for (self.program.branchSpan(match.branches)) |branch| {
                    if (branch.guard) |guard| try self.rewriteExpr(guard);
                    try self.rewriteExpr(branch.body);
                }
            },
            .if_ => |if_| {
                for (self.program.ifBranchSpan(if_.branches)) |branch| {
                    try self.rewriteExpr(branch.cond);
                    try self.rewriteExpr(branch.body);
                }
                try self.rewriteExpr(if_.final_else);
            },
            .if_initialized_payload => |payload_switch| {
                try self.rewriteExpr(payload_switch.cond);
                try self.rewriteExpr(payload_switch.initialized);
                try self.rewriteExpr(payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.rewriteExpr(sequence.try_expr);
                try self.rewriteExpr(sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.rewriteExpr(sequence.try_expr);
                try self.rewriteExpr(sequence.ok_body);
            },
            .block => |block| {
                for (self.program.stmtSpan(block.statements)) |stmt| try self.rewriteStmt(stmt);
                try self.rewriteExpr(block.final_expr);
            },
            .loop_ => |loop| {
                for (self.program.exprSpan(loop.initial_values)) |initial| try self.rewriteExpr(initial);
                try self.rewriteExpr(loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.rewriteExpr(value),
            .continue_ => |continue_| for (self.program.exprSpan(continue_.values)) |value| try self.rewriteExpr(value),
        }
    }

    fn materializeIterPlanExpr(
        self: *Rewrite,
        expr_id: Lifted.ExprId,
        plan_id: Lifted.IterPlanId,
    ) Common.LowerError!void {
        const plan_raw = @intFromEnum(plan_id);
        if (plan_raw >= self.program.iter_plans.items.len) {
            Common.invariant("iterator plan expression referenced a missing plan during elimination");
        }
        const materialized = self.program.iter_plans.items[plan_raw].materialized orelse
            Common.invariant("iterator plan reached elimination without a public materialization");

        try self.rewriteExpr(materialized);

        const expr = &self.program.exprs.items[@intFromEnum(expr_id)];
        const materialized_expr = self.program.exprs.items[@intFromEnum(materialized)];
        if (expr.ty != materialized_expr.ty) {
            Common.invariant("iterator plan materialization had a different type than the plan expression");
        }
        expr.data = materialized_expr.data;
    }
};
