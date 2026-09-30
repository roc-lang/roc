//! Sequence strict operands before their consumers. Monotype mutable versions
//! can be bound inside a block/let operand and used by the enclosing continuation.
//! The source expression graph is acyclic; function recursion travels through
//! explicit function references, which this walk does not follow.
//! Lift those bindings into that continuation, keeping every operand's evaluation
//! in source order. Branch and loop bodies retain their own control-flow scopes.
const std = @import("std");
const collections = @import("collections");
const Ast = @import("ast.zig");
const Common = @import("../common.zig");
const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;
const Statements = struct {
    list: std.ArrayList(Ast.StmtId) = .empty,
    terminated: bool = false,

    fn append(self: *Statements, allocator: Allocator, stmt: Ast.StmtId) Allocator.Error!void {
        try self.list.append(allocator, stmt);
    }

    fn deinit(self: *Statements, allocator: Allocator) void {
        self.list.deinit(allocator);
    }
};

/// Sequence lifted operands while retaining lexical variable scope.
pub fn run(program: *Ast.Program) Allocator.Error!void {
    var normalizer = Normalizer{ .program = program, .symbols = .{ .next = program.next_symbol } };
    const old_loc = program.current_loc;
    const old_region = program.current_region;
    const old_inline = program.current_inline_scope;
    defer {
        program.current_loc = old_loc;
        program.current_region = old_region;
        program.current_inline_scope = old_inline;
    }
    for (0..program.fnCount()) |index| {
        const id: Ast.FnId = @enumFromInt(index);
        var function = program.getFn(id);
        if (function.body != .roc) continue;
        const outer_shapes = program.beginFnShapes(id);
        function.body = .{ .roc = try normalizer.scope(function.body.roc) };
        function.shapes = program.finishFnShapes(outer_shapes);
        program.setFn(id, function);
    }
    program.next_symbol = normalizer.symbols.next;
}

const Normalizer = struct {
    program: *Ast.Program,
    symbols: Common.SymbolGen,

    fn scope(self: *Normalizer, source: Ast.ExprId) Allocator.Error!Ast.ExprId {
        var statements: Statements = .{};
        defer statements.deinit(self.program.allocator);
        const result = try self.expression(source, &statements);
        if (statements.list.items.len == 0) return result;
        return try self.program.addExpr(.{ .ty = self.program.getExpr(result).ty, .data = .{ .block = .{
            .statements = try self.program.addStmtSpan(statements.list.items),
            .final_expr = result,
        } } });
    }

    fn statement(self: *Normalizer, source: Ast.StmtId, statements: *Statements) Allocator.Error!void {
        if (statements.terminated) return;
        const old_loc = self.program.current_loc;
        const old_region = self.program.current_region;
        const old_inline = self.program.current_inline_scope;
        self.program.current_loc = self.program.stmtLoc(source);
        self.program.current_region = self.program.stmtRegion(source);
        self.program.current_inline_scope = self.program.stmtInlineScope(source);
        defer {
            self.program.current_loc = old_loc;
            self.program.current_region = old_region;
            self.program.current_inline_scope = old_inline;
        }
        var stmt = self.program.getStmt(source);
        switch (stmt) {
            .let_ => |*binding| binding.value = if (binding.recursive) try self.scope(binding.value) else try self.expression(binding.value, statements),
            .expr => |*value| value.* = try self.expression(value.*, statements),
            .dbg => |*value| value.* = try self.expression(value.*, statements),
            // Expect conditions retain their run/omit execution context.
            .expect => |*value| value.* = try self.scope(value.*),
            .return_ => |*ret| ret.value = try self.expression(ret.value, statements),
            .uninitialized, .crash => {},
        }
        if (statements.terminated) return;
        if (stmt == .let_ and terminal(self.program.getExpr(stmt.let_.value).data)) stmt = .{ .expr = stmt.let_.value };
        try statements.append(self.program.allocator, try self.program.addStmt(stmt));
        statements.terminated = switch (stmt) {
            .return_, .crash => true,
            .expr => |value| terminal(self.program.getExpr(value).data),
            .uninitialized, .let_, .expect, .dbg => false,
        };
    }

    fn statementSpan(self: *Normalizer, span: Ast.Span(Ast.StmtId), statements: *Statements) Allocator.Error!void {
        for (0..span.len) |index| {
            const source = GuardedList.at(self.program.stmtSpan(span), index);
            try self.statement(source, statements);
        }
    }

    /// Name an operand before processing the next operand. This is sequencing,
    /// not code motion: even a pure opaque call must execute at this position.
    fn operand(self: *Normalizer, source: Ast.ExprId, statements: *Statements) Allocator.Error!Ast.ExprId {
        const result = try self.expression(source, statements);
        const expr = self.program.getExpr(result);
        if (terminal(expr.data)) {
            if (expr.data != .@"unreachable") try statements.append(self.program.allocator, try self.program.addStmt(.{ .expr = result }));
            statements.terminated = true;
            return try self.program.addExpr(.{ .ty = expr.ty, .data = .@"unreachable" });
        }
        switch (expr.data) {
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
            .inline_expects_enabled,
            => return result,
            .static_data_candidate,
            .comptime_value,
            .typed_boundary,
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .nominal,
            .let_,
            .fn_ref,
            .call_value,
            .call_proc,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .join_point,
            .comptime_branch_taken,
            .dbg,
            .expect,
            => {},
            .@"unreachable",
            .break_,
            .continue_,
            .jump,
            .return_,
            .crash,
            .comptime_exhaustiveness_failed,
            .expect_err,
            .literal_rejected,
            => unreachable,
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("unlifted function reached operand sequencing"),
        }
        const local = try self.program.addLocal(self.symbols.fresh(), expr.ty);
        const pattern = try self.program.addPat(.{ .ty = expr.ty, .data = .{ .bind = local } });
        try statements.append(self.program.allocator, try self.program.addStmt(.{ .let_ = .{
            .pat = pattern,
            .value = result,
        } }));
        return try self.program.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } });
    }

    fn terminal(data: Ast.ExprData) bool {
        return switch (data) {
            .@"unreachable", .return_, .break_, .continue_, .jump, .crash, .expect_err, .literal_rejected, .comptime_exhaustiveness_failed => true,
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .static_data_candidate,
            .inline_expects_enabled,
            .comptime_value,
            .typed_boundary,
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .nominal,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .fn_ref,
            .call_value,
            .call_proc,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .join_point,
            .comptime_branch_taken,
            .dbg,
            .expect,
            => false,
        };
    }

    fn expressionSpan(self: *Normalizer, span: Ast.Span(Ast.ExprId), statements: *Statements) Allocator.Error!Ast.Span(Ast.ExprId) {
        const items = try self.program.allocator.alloc(Ast.ExprId, span.len);
        defer self.program.allocator.free(items);
        for (items, 0..) |*item, index| item.* = try self.operand(GuardedList.at(self.program.exprSpan(span), index), statements);
        return try self.program.addExprSpan(items);
    }

    fn fieldSpan(self: *Normalizer, span: Ast.Span(Ast.FieldExpr), statements: *Statements) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const items = try self.program.allocator.alloc(Ast.FieldExpr, span.len);
        defer self.program.allocator.free(items);
        for (items, 0..) |*item, index| {
            item.* = GuardedList.at(self.program.fieldExprSpan(span), index);
            item.value = try self.operand(item.value, statements);
        }
        return try self.program.addFieldExprSpan(items);
    }

    fn captureSpan(self: *Normalizer, span: Ast.Span(Ast.CaptureOperand), statements: *Statements) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        const items = try self.program.allocator.alloc(Ast.CaptureOperand, span.len);
        defer self.program.allocator.free(items);
        for (items, 0..) |*item, index| {
            item.* = GuardedList.at(self.program.captureOperandSpan(span), index);
            item.value = try self.operand(item.value, statements);
        }
        return try self.program.addCaptureOperandSpan(items);
    }

    fn expression(self: *Normalizer, source: Ast.ExprId, statements: *Statements) Allocator.Error!Ast.ExprId {
        const old_loc = self.program.current_loc;
        const old_region = self.program.current_region;
        const old_inline = self.program.current_inline_scope;
        self.program.current_loc = self.program.exprLoc(source);
        self.program.current_region = self.program.exprRegion(source);
        self.program.current_inline_scope = self.program.exprInlineScope(source);
        defer {
            self.program.current_loc = old_loc;
            self.program.current_region = old_region;
            self.program.current_inline_scope = old_inline;
        }
        const expr = self.program.getExpr(source);
        if (statements.terminated) return try self.program.addExpr(.{ .ty = expr.ty, .data = .@"unreachable" });
        var data = expr.data;
        self.program.noteExprShapes(expr);
        switch (data) {
            .block => |block| {
                try self.statementSpan(block.statements, statements);
                return try self.expression(block.final_expr, statements);
            },
            .let_ => |binding| {
                const value = try self.expression(binding.value, statements);
                if (!statements.terminated and terminal(self.program.getExpr(value).data)) {
                    if (self.program.getExpr(value).data != .@"unreachable") try statements.append(self.program.allocator, try self.program.addStmt(.{ .expr = value }));
                    statements.terminated = true;
                }
                if (statements.terminated) return try self.program.addExpr(.{ .ty = expr.ty, .data = .@"unreachable" });
                try statements.append(self.program.allocator, try self.program.addStmt(.{ .let_ = .{
                    .pat = binding.bind,
                    .value = value,
                    .comptime_site = binding.comptime_site,
                } }));
                return try self.expression(binding.rest, statements);
            },
            .match_ => |*match| {
                match.scrutinee = try self.operand(match.scrutinee, statements);
                const branches = try self.program.allocator.alloc(Ast.Branch, match.branches.len);
                defer self.program.allocator.free(branches);
                for (branches, 0..) |*branch, index| {
                    branch.* = GuardedList.at(self.program.branchSpan(match.branches), index);
                    var bindings: Statements = .{};
                    defer bindings.deinit(self.program.allocator);
                    try self.statementSpan(branch.bindings, &bindings);
                    if (branch.guard) |guard| branch.guard = try self.expression(guard, &bindings);
                    branch.bindings = try self.program.addStmtSpan(bindings.list.items);
                    branch.body = try self.scope(branch.body);
                }
                match.branches = try self.program.addBranchSpan(branches);
            },
            .if_ => |conditional| return try self.sequenceIf(expr.ty, conditional, 0, statements),
            .if_initialized_payload => |*conditional| {
                conditional.cond = try self.operand(conditional.cond, statements);
                conditional.initialized = try self.scope(conditional.initialized);
                conditional.uninitialized = try self.scope(conditional.uninitialized);
            },
            .loop_ => |*loop| {
                loop.initial_values = try self.expressionSpan(loop.initial_values, statements);
                loop.body = try self.scope(loop.body);
            },
            .join_point => |*join| {
                join.body = try self.scope(join.body);
                join.remainder = try self.scope(join.remainder);
            },
            .try_sequence => |*sequence| {
                sequence.try_expr = try self.operand(sequence.try_expr, statements);
                sequence.ok_body = try self.scope(sequence.ok_body);
            },
            .try_record_sequence => |*sequence| {
                sequence.try_expr = try self.operand(sequence.try_expr, statements);
                sequence.ok_body = try self.scope(sequence.ok_body);
            },
            // These children provide evidence or conditional execution, rather
            // than ordinary strict operands at the enclosing position.
            .static_data_candidate => |*candidate| candidate.runtime_expr = try self.scope(candidate.runtime_expr),
            .comptime_value => |*candidate| candidate.initializer = try self.scope(candidate.initializer),
            .dbg => |*child| child.* = try self.expression(child.*, statements),
            .expect => |*child| child.* = try self.scope(child.*),
            .comptime_branch_taken => |*taken| taken.body = try self.scope(taken.body),
            .lambda, .def_ref, .fn_def => Common.invariant("unlifted function reached operand sequencing"),
            .list, .tuple => |*items| items.* = try self.expressionSpan(items.*, statements),
            .record => |*fields| fields.* = try self.fieldSpan(fields.*, statements),
            .record_update => |*update| {
                update.base = try self.operand(update.base, statements);
                update.fields = try self.fieldSpan(update.fields, statements);
            },
            .tag => |*tag| tag.payloads = try self.expressionSpan(tag.payloads, statements),
            .fn_ref => |*reference| reference.captures = try self.captureSpan(reference.captures, statements),
            .typed_boundary => |*boundary| boundary.value = try self.expression(boundary.value, statements),
            .nominal => |*child| child.* = try self.operand(child.*, statements),
            .call_value => |*call| {
                call.callee = try self.operand(call.callee, statements);
                call.args = try self.expressionSpan(call.args, statements);
            },
            .call_proc => |*call| {
                call.args = try self.expressionSpan(call.args, statements);
                call.captures = try self.captureSpan(call.captures, statements);
            },
            .low_level => |*call| call.args = try self.expressionSpan(call.args, statements),
            .field_access => |*field| field.receiver = try self.operand(field.receiver, statements),
            .tuple_access => |*access| access.tuple = try self.operand(access.tuple, statements),
            .structural_eq => |*equal| {
                equal.lhs = try self.operand(equal.lhs, statements);
                equal.rhs = try self.operand(equal.rhs, statements);
            },
            .structural_hash => |*hash| {
                hash.value = try self.operand(hash.value, statements);
                hash.hasher = try self.operand(hash.hasher, statements);
            },
            .return_ => |*ret| ret.value = try self.expression(ret.value, statements),
            .expect_err => |*failure| failure.msg = try self.operand(failure.msg, statements),
            .literal_rejected => |*failure| failure.msg = try self.operand(failure.msg, statements),
            .break_ => |*value| if (value.*) |child| {
                value.* = try self.operand(child, statements);
            },
            .continue_ => |*transfer| transfer.values = try self.expressionSpan(transfer.values, statements),
            .jump => |*transfer| {
                transfer.loop_values = try self.expressionSpan(transfer.loop_values, statements);
                transfer.args = try self.expressionSpan(transfer.args, statements);
            },
            // The source marker carries the producer's explicit termination
            // relation, including divergent calls whose syntax is ordinary.
            .@"unreachable" => {
                statements.terminated = true;
                return source;
            },
            .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .inline_expects_enabled, .crash, .comptime_exhaustiveness_failed => return source,
        }
        if (statements.terminated) return try self.program.addExpr(.{ .ty = expr.ty, .data = .@"unreachable" });
        return try self.program.addExpr(.{ .ty = expr.ty, .data = data });
    }

    /// Later conditions execute only when all earlier conditions are false.
    /// Keep their prefixes in that else continuation, never before the first if.
    fn sequenceIf(self: *Normalizer, ty: @import("../monotype/type.zig").TypeId, source: @import("../monotype/ast.zig").IfExpr, index: usize, statements: *Statements) Allocator.Error!Ast.ExprId {
        if (index == source.branches.len) return try self.expression(source.final_else, statements);
        const branch = GuardedList.at(self.program.ifBranchSpan(source.branches), index);
        const cond = try self.operand(branch.cond, statements);
        if (statements.terminated) return try self.program.addExpr(.{ .ty = ty, .data = .@"unreachable" });
        const body = try self.scope(branch.body);
        var else_statements: Statements = .{};
        defer else_statements.deinit(self.program.allocator);
        var final_else = try self.sequenceIf(ty, source, index + 1, &else_statements);
        if (else_statements.list.items.len != 0) final_else = try self.program.addExpr(.{ .ty = ty, .data = .{ .block = .{
            .statements = try self.program.addStmtSpan(else_statements.list.items),
            .final_expr = final_else,
        } } });
        if (statements.terminated) return try self.program.addExpr(.{ .ty = ty, .data = .@"unreachable" });
        return try self.program.addExpr(.{ .ty = ty, .data = .{ .if_ = .{
            .branches = try self.program.addIfBranchSpan(&.{.{ .cond = cond, .body = body }}),
            .final_else = final_else,
        } } });
    }
};
