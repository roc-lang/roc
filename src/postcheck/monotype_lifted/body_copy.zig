//! Structural copies of lifted expressions.
//!
//! Monotype restores each compile-time constant once and names that one
//! expression wherever the constant is read, so two parents can hold the same
//! expression. An expression with expressions inside it has exactly one
//! parent in lifted output (design.md "Operand Sequencing"), so lifting gives
//! every parent after the first a copy of its own: each statement and each
//! expression with expressions inside it is added again, and locals, literals
//! and markers stay the ones the source names.
//! Copying runs from an explicit stack, so the depth of an expression never
//! becomes native call depth.
const std = @import("std");
const collections = @import("collections");
const Ast = @import("ast.zig");
const Common = @import("../common.zig");
const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

const Work = union(enum) {
    /// Copy this expression and deliver the copy.
    copy: Ast.ExprId,
    /// Every expression inside this one has been delivered: add it again
    /// around those copies.
    assemble: Ast.ExprId,
};

/// Copy the expression rooted at `root` and return the copy's root.
/// Expression shapes are recorded for whichever function's shapes the program
/// is collecting.
pub fn copy(program: *Ast.Program, root: Ast.ExprId) Allocator.Error!Ast.ExprId {
    var copier: Copier = .{ .program = program };
    defer copier.deinit();
    const position = copier.savePosition();
    defer copier.restorePosition(position);
    return try copier.copyOne(root);
}

/// Give `parent` a copy of its own of each expression in `shared`, which are
/// expressions directly inside it that another parent also holds. An
/// expression listed twice is copied at two of the places `parent` names it.
/// `parent` keeps its id.
pub fn copyInside(program: *Ast.Program, parent: Ast.ExprId, shared: []const Ast.ExprId) Allocator.Error!void {
    var copier: Copier = .{ .program = program };
    defer copier.deinit();
    const position = copier.savePosition();
    defer copier.restorePosition(position);

    const data = program.getExpr(parent).data;
    copier.children.clearRetainingCapacity();
    try copier.collectChildren(data);
    const inside = try program.allocator.dupe(Ast.ExprId, copier.children.items);
    defer program.allocator.free(inside);
    const copied = try program.allocator.alloc(bool, inside.len);
    defer program.allocator.free(copied);
    @memset(copied, false);
    var any = false;
    for (shared) |expr| {
        for (inside, copied) |*child, *done| {
            if (done.* or child.* != expr) continue;
            child.* = try copier.copyOne(expr);
            done.* = true;
            any = true;
            break;
        }
    }
    if (!any) return;
    program.setExprData(parent, try copier.assembledData(data, inside));
}

const Copier = struct {
    program: *Ast.Program,
    work: std.ArrayList(Work) = .empty,
    /// Copies delivered and not yet taken by the expression that holds them,
    /// in evaluation order.
    delivered: std.ArrayList(Ast.ExprId) = .empty,
    /// The expressions directly inside the expression being started.
    children: std.ArrayList(Ast.ExprId) = .empty,
    stmt_scratch: std.ArrayList(Ast.StmtId) = .empty,
    expr_scratch: std.ArrayList(Ast.ExprId) = .empty,
    field_scratch: std.ArrayList(Ast.FieldExpr) = .empty,
    capture_scratch: std.ArrayList(Ast.CaptureOperand) = .empty,
    branch_scratch: std.ArrayList(Ast.Branch) = .empty,

    fn deinit(self: *Copier) void {
        const allocator = self.program.allocator;
        self.branch_scratch.deinit(allocator);
        self.capture_scratch.deinit(allocator);
        self.field_scratch.deinit(allocator);
        self.expr_scratch.deinit(allocator);
        self.stmt_scratch.deinit(allocator);
        self.children.deinit(allocator);
        self.delivered.deinit(allocator);
        self.work.deinit(allocator);
    }

    const Position = struct {
        loc: @FieldType(Ast.Program, "current_loc"),
        region: @FieldType(Ast.Program, "current_region"),
        inline_scope: @FieldType(Ast.Program, "current_inline_scope"),
    };

    fn savePosition(self: *Copier) Position {
        return .{
            .loc = self.program.current_loc,
            .region = self.program.current_region,
            .inline_scope = self.program.current_inline_scope,
        };
    }

    fn restorePosition(self: *Copier, position: Position) void {
        self.program.current_loc = position.loc;
        self.program.current_region = position.region;
        self.program.current_inline_scope = position.inline_scope;
    }

    fn copyOne(self: *Copier, root: Ast.ExprId) Allocator.Error!Ast.ExprId {
        const base = self.work.items.len;
        try self.work.append(self.program.allocator, .{ .copy = root });
        while (self.work.items.len > base) switch (self.work.pop().?) {
            .copy => |expr| try self.begin(expr),
            .assemble => |expr| try self.assemble(expr),
        };
        return self.delivered.pop().?;
    }

    fn child(self: *Copier, expr: Ast.ExprId) Allocator.Error!void {
        try self.children.append(self.program.allocator, expr);
    }

    fn childExprs(self: *Copier, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        for (0..span.len) |index| try self.child(GuardedList.at(self.program.exprSpan(span), index));
    }

    fn childFields(self: *Copier, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        for (0..span.len) |index| try self.child(GuardedList.at(self.program.fieldExprSpan(span), index).value);
    }

    fn childCaptures(self: *Copier, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        for (0..span.len) |index| try self.child(GuardedList.at(self.program.captureOperandSpan(span), index).value);
    }

    fn childStatements(self: *Copier, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        for (0..span.len) |index| {
            switch (self.program.getStmt(GuardedList.at(self.program.stmtSpan(span), index))) {
                .let_ => |binding| try self.child(binding.value),
                .expr, .expect, .dbg => |value| try self.child(value),
                .return_ => |ret| try self.child(ret.value),
                .uninitialized, .crash, .checked_error => {},
            }
        }
    }

    /// Deliver a leaf as it is, or queue the expressions inside `expr` ahead
    /// of assembling its copy around theirs.
    fn begin(self: *Copier, expr: Ast.ExprId) Allocator.Error!void {
        const allocator = self.program.allocator;
        const data = self.program.getExpr(expr).data;
        if (data.isLeaf()) {
            try self.delivered.append(allocator, expr);
            return;
        }
        self.children.clearRetainingCapacity();
        try self.collectChildren(data);
        try self.work.append(allocator, .{ .assemble = expr });
        // The stack hands the children back first to last.
        var index = self.children.items.len;
        while (index > 0) {
            index -= 1;
            try self.work.append(allocator, .{ .copy = self.children.items[index] });
        }
    }

    /// Append every expression directly inside `data` to `children`, in
    /// evaluation order.
    fn collectChildren(self: *Copier, data: Ast.ExprData) Allocator.Error!void {
        switch (data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            => {},
            .let_ => Common.invariant("a `let` expression reached a lifted body copy"),
            .lambda, .def_ref, .fn_def => Common.invariant("an unlifted function reached a lifted body copy"),
            .list, .tuple => |items| try self.childExprs(items),
            .tag => |tag| try self.childExprs(tag.payloads),
            .low_level => |call| try self.childExprs(call.args),
            .continue_ => |transfer| try self.childExprs(transfer.values),
            .jump => |transfer| {
                try self.childExprs(transfer.loop_values);
                try self.childExprs(transfer.args);
            },
            .record => |fields| try self.childFields(fields),
            .record_update => |update| {
                try self.child(update.base);
                try self.childFields(update.fields);
            },
            .fn_ref => |reference| try self.childCaptures(reference.captures),
            .call_value => |call| {
                try self.child(call.callee);
                try self.childExprs(call.args);
            },
            .call_proc => |call| {
                try self.childExprs(call.args);
                try self.childCaptures(call.captures);
            },
            .structural_eq => |equal| {
                try self.child(equal.lhs);
                try self.child(equal.rhs);
            },
            .structural_hash => |hash| {
                try self.child(hash.value);
                try self.child(hash.hasher);
            },
            .loop_ => |loop| {
                try self.childExprs(loop.initial_values);
                try self.child(loop.body);
            },
            .nominal, .dbg, .expect => |value| try self.child(value),
            .typed_boundary => |boundary| try self.child(boundary.value),
            .row_widen => |widen| try self.child(widen.value),
            .return_ => |ret| try self.child(ret.value),
            .field_access => |access| try self.child(access.receiver),
            .tuple_access => |access| try self.child(access.tuple),
            inline .expect_err, .literal_rejected => |failure| try self.child(failure.msg),
            .break_ => |maybe_value| if (maybe_value) |value| try self.child(value),
            .static_data_candidate => |candidate| if (candidate.value.subexpression()) |value| try self.child(value),
            .comptime_value => |candidate| try self.child(candidate.initializer),
            .join_point => |join| {
                try self.child(join.body);
                try self.child(join.remainder);
            },
            .if_initialized_payload => |conditional| {
                try self.child(conditional.cond);
                try self.child(conditional.initialized);
                try self.child(conditional.uninitialized);
            },
            inline .try_sequence, .try_record_sequence => |sequence| {
                try self.child(sequence.try_expr);
                try self.child(sequence.ok_body);
            },
            .match_ => |match| {
                try self.child(match.scrutinee);
                for (0..match.branches.len) |index| {
                    const branch = GuardedList.at(self.program.branchSpan(match.branches), index);
                    try self.childStatements(branch.bindings);
                    if (branch.guard.get()) |guard| try self.child(guard);
                    try self.child(branch.body);
                }
            },
            .if_ => |conditional| {
                for (0..conditional.branches.len) |index| {
                    const branch = GuardedList.at(self.program.ifBranchSpan(conditional.branches), index);
                    try self.child(branch.cond);
                    try self.child(branch.body);
                }
                try self.child(conditional.final_else);
            },
            .block => |block| {
                try self.childStatements(block.statements);
                try self.child(block.final_expr);
            },
        }
    }

    /// The copies an expression takes, first to last.
    const Taken = struct {
        copies: []const Ast.ExprId,
        next: usize = 0,

        fn one(taken: *Taken) Ast.ExprId {
            defer taken.next += 1;
            return taken.copies[taken.next];
        }
    };

    fn takeExprs(self: *Copier, taken: *Taken, span: Ast.Span(Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        self.expr_scratch.clearRetainingCapacity();
        for (0..span.len) |_| try self.expr_scratch.append(self.program.allocator, taken.one());
        return try self.program.addExprSpan(self.expr_scratch.items);
    }

    fn takeFields(self: *Copier, taken: *Taken, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        self.field_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            var field = GuardedList.at(self.program.fieldExprSpan(span), index);
            field.value = taken.one();
            try self.field_scratch.append(self.program.allocator, field);
        }
        return try self.program.addFieldExprSpan(self.field_scratch.items);
    }

    fn takeCaptures(self: *Copier, taken: *Taken, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        self.capture_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            var operand = GuardedList.at(self.program.captureOperandSpan(span), index);
            operand.value = taken.one();
            try self.capture_scratch.append(self.program.allocator, operand);
        }
        return try self.program.addCaptureOperandSpan(self.capture_scratch.items);
    }

    /// Add each statement of `span` again, at its own source position,
    /// around the copy of its value.
    fn takeStatements(self: *Copier, taken: *Taken, span: Ast.Span(Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const program = self.program;
        const loc = program.current_loc;
        const region = program.current_region;
        const inline_scope = program.current_inline_scope;
        defer {
            program.current_loc = loc;
            program.current_region = region;
            program.current_inline_scope = inline_scope;
        }
        self.stmt_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            const source = GuardedList.at(program.stmtSpan(span), index);
            var stmt = program.getStmt(source);
            switch (stmt) {
                .let_ => |*binding| binding.value = taken.one(),
                .expr, .expect, .dbg => |*value| value.* = taken.one(),
                .return_ => |*ret| ret.value = taken.one(),
                .uninitialized, .crash, .checked_error => {},
            }
            program.current_loc = program.stmtLoc(source);
            program.current_region = program.stmtRegion(source);
            program.current_inline_scope = program.stmtInlineScope(source);
            try self.stmt_scratch.append(program.allocator, try program.addStmt(stmt));
        }
        return try program.addStmtSpan(self.stmt_scratch.items);
    }

    /// Add `expr` again around the copies of the expressions inside it, at
    /// its own source position, and deliver the copy.
    fn assemble(self: *Copier, expr: Ast.ExprId) Allocator.Error!void {
        const program = self.program;
        const source = program.getExpr(expr);
        const count = self.childCount(source.data);
        const first = self.delivered.items.len - count;
        // The copies are read from storage of their own: copying statements
        // below never moves them.
        const copies = try program.allocator.dupe(Ast.ExprId, self.delivered.items[first..]);
        defer program.allocator.free(copies);
        self.delivered.shrinkRetainingCapacity(first);
        const data = try self.assembledData(source.data, copies);

        program.current_loc = program.exprLoc(expr);
        program.current_region = program.exprRegion(expr);
        program.current_inline_scope = program.exprInlineScope(expr);
        try self.delivered.append(program.allocator, try program.addExpr(.{ .ty = source.ty, .data = data }));
    }

    /// `source` around `inside`, which holds one expression for each
    /// expression directly inside it, in evaluation order. Its spans and
    /// statements are added again.
    fn assembledData(self: *Copier, source: Ast.ExprData, inside: []const Ast.ExprId) Allocator.Error!Ast.ExprData {
        const program = self.program;
        var data = source;
        var taken: Taken = .{ .copies = inside };

        switch (data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            => {},
            .let_ => Common.invariant("a `let` expression reached a lifted body copy"),
            .lambda, .def_ref, .fn_def => Common.invariant("an unlifted function reached a lifted body copy"),
            .list, .tuple => |*items| items.* = try self.takeExprs(&taken, items.*),
            .tag => |*tag| tag.payloads = try self.takeExprs(&taken, tag.payloads),
            .low_level => |*call| call.args = try self.takeExprs(&taken, call.args),
            .continue_ => |*transfer| transfer.values = try self.takeExprs(&taken, transfer.values),
            .jump => |*transfer| {
                transfer.loop_values = try self.takeExprs(&taken, transfer.loop_values);
                transfer.args = try self.takeExprs(&taken, transfer.args);
            },
            .record => |*fields| fields.* = try self.takeFields(&taken, fields.*),
            .record_update => |*update| {
                update.base = taken.one();
                update.fields = try self.takeFields(&taken, update.fields);
            },
            .fn_ref => |*reference| reference.captures = try self.takeCaptures(&taken, reference.captures),
            .call_value => |*call| {
                call.callee = taken.one();
                call.args = try self.takeExprs(&taken, call.args);
            },
            .call_proc => |*call| {
                call.args = try self.takeExprs(&taken, call.args);
                call.captures = try self.takeCaptures(&taken, call.captures);
            },
            .structural_eq => |*equal| {
                equal.lhs = taken.one();
                equal.rhs = taken.one();
            },
            .structural_hash => |*hash| {
                hash.value = taken.one();
                hash.hasher = taken.one();
            },
            .loop_ => |*loop| {
                loop.initial_values = try self.takeExprs(&taken, loop.initial_values);
                loop.body = taken.one();
            },
            .nominal, .dbg, .expect => |*value| value.* = taken.one(),
            .typed_boundary => |*boundary| boundary.value = taken.one(),
            .row_widen => |*widen| widen.value = taken.one(),
            .return_ => |*ret| ret.value = taken.one(),
            .field_access => |*access| access.receiver = taken.one(),
            .tuple_access => |*access| access.tuple = taken.one(),
            inline .expect_err, .literal_rejected => |*failure| failure.msg = taken.one(),
            .break_ => |*maybe_value| if (maybe_value.* != null) {
                maybe_value.* = taken.one();
            },
            .static_data_candidate => |*candidate| if (candidate.value.subexpressionPtr()) |value| {
                value.* = taken.one();
            },
            .comptime_value => |*candidate| candidate.initializer = taken.one(),
            .join_point => |*join| {
                join.body = taken.one();
                join.remainder = taken.one();
            },
            .if_initialized_payload => |*conditional| {
                conditional.cond = taken.one();
                conditional.initialized = taken.one();
                conditional.uninitialized = taken.one();
            },
            inline .try_sequence, .try_record_sequence => |*sequence| {
                sequence.try_expr = taken.one();
                sequence.ok_body = taken.one();
            },
            .match_ => |*match| {
                match.scrutinee = taken.one();
                const branches = try program.allocator.alloc(Ast.Branch, match.branches.len);
                defer program.allocator.free(branches);
                for (branches, 0..) |*branch, index| {
                    branch.* = GuardedList.at(program.branchSpan(match.branches), index);
                    branch.bindings = try self.takeStatements(&taken, branch.bindings);
                    if (branch.guard != .none) branch.guard = .some(taken.one());
                    branch.body = taken.one();
                }
                match.branches = try program.addBranchSpan(branches);
            },
            .if_ => |*conditional| {
                const branches = try program.allocator.alloc(Ast.IfBranch, conditional.branches.len);
                defer program.allocator.free(branches);
                for (branches, 0..) |*branch, index| {
                    branch.* = GuardedList.at(program.ifBranchSpan(conditional.branches), index);
                    branch.cond = taken.one();
                    branch.body = taken.one();
                }
                conditional.branches = try program.addIfBranchSpan(branches);
                conditional.final_else = taken.one();
            },
            .block => |*block| {
                block.statements = try self.takeStatements(&taken, block.statements);
                block.final_expr = taken.one();
            },
        }
        std.debug.assert(taken.next == inside.len);
        return data;
    }

    /// How many expressions stand directly inside `data`.
    fn childCount(self: *Copier, data: Ast.ExprData) usize {
        const program = self.program;
        return switch (data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            => unreachable, // `begin` queues an assembly only for the forms below
            .list, .tuple => |items| items.len,
            .tag => |tag| tag.payloads.len,
            .low_level => |call| call.args.len,
            .continue_ => |transfer| transfer.values.len,
            .jump => |transfer| transfer.loop_values.len + transfer.args.len,
            .record => |fields| fields.len,
            .record_update => |update| 1 + update.fields.len,
            .fn_ref => |reference| reference.captures.len,
            .call_value => |call| 1 + call.args.len,
            .call_proc => |call| call.args.len + call.captures.len,
            .structural_eq, .structural_hash, .join_point => 2,
            .loop_ => |loop| loop.initial_values.len + 1,
            .nominal, .dbg, .expect, .typed_boundary, .row_widen, .return_, .field_access, .tuple_access, .expect_err, .literal_rejected, .comptime_value => 1,
            .break_ => |maybe_value| if (maybe_value == null) 0 else 1,
            .static_data_candidate => |candidate| if (candidate.value.subexpression() == null) 0 else 1,
            .if_initialized_payload => 3,
            .try_sequence, .try_record_sequence => 2,
            .match_ => |match| count: {
                var total: usize = 1;
                for (0..match.branches.len) |index| {
                    const branch = GuardedList.at(program.branchSpan(match.branches), index);
                    total += self.statementValueCount(branch.bindings) + 1;
                    if (branch.guard != .none) total += 1;
                }
                break :count total;
            },
            .if_ => |conditional| conditional.branches.len * 2 + 1,
            .block => |block| self.statementValueCount(block.statements) + 1,
        };
    }

    fn statementValueCount(self: *Copier, span: Ast.Span(Ast.StmtId)) usize {
        var total: usize = 0;
        for (0..span.len) |index| {
            switch (self.program.getStmt(GuardedList.at(self.program.stmtSpan(span), index))) {
                .let_, .expr, .expect, .dbg, .return_ => total += 1,
                .uninitialized, .crash, .checked_error => {},
            }
        }
        return total;
    }
};

const sequenced = @import("sequenced.zig");

test "a copied body names no statement or compound expression its source names" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    // One restored constant, read by two functions.
    const constant = try t.tuple(&.{ try t.tuple(&.{seven}), seven });
    const binding = try t.bind(constant);
    const body = try t.block(&.{binding}, seven);
    const first = try t.function(body);
    const second = try t.function(body);
    try std.testing.expectEqualStrings(
        "an expression with expressions inside it is used in two places",
        (try sequenced.check(&t.program)).?.message,
    );

    var function = t.program.getFn(second);
    function.body = .{ .roc = try copy(&t.program, body) };
    t.program.setFn(second, function);
    try std.testing.expectEqual(@as(?sequenced.Violation, null), try sequenced.check(&t.program));

    try std.testing.expectEqual(body, t.program.getFn(first).body.roc);
    const copied = t.program.getExpr(t.program.getFn(second).body.roc).data.block;
    try std.testing.expect(t.program.getFn(second).body.roc != body);
    // Leaves stay the ones the source names.
    try std.testing.expectEqual(seven, copied.final_expr);
    const copied_binding = GuardedList.at(t.program.stmtSpan(copied.statements), 0);
    try std.testing.expect(copied_binding != binding);
    const copied_constant = t.program.getStmt(copied_binding).let_.value;
    try std.testing.expect(copied_constant != constant);
    const items = t.program.getExpr(copied_constant).data.tuple;
    try std.testing.expect(GuardedList.at(t.program.exprSpan(items), 0) != GuardedList.at(t.program.exprSpan(t.program.getExpr(constant).data.tuple), 0));
    try std.testing.expectEqual(seven, GuardedList.at(t.program.exprSpan(items), 1));
}

test "a parent given its own copy of a shared expression keeps its id and everything else it names" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    // One restored constant, read once by one function and twice by another.
    const constant = try t.tuple(&.{ try t.tuple(&.{seven}), seven });
    const own = try t.tuple(&.{seven});
    _ = try t.function(try t.tuple(&.{constant}));
    const holder = try t.tuple(&.{ own, constant, constant });
    _ = try t.function(holder);
    try std.testing.expectEqualStrings(
        "an expression with expressions inside it is used in two places",
        (try sequenced.check(&t.program)).?.message,
    );

    try copyInside(&t.program, holder, &.{ constant, constant });
    try std.testing.expectEqual(@as(?sequenced.Violation, null), try sequenced.check(&t.program));

    const items = t.program.getExpr(holder).data.tuple;
    const second = GuardedList.at(t.program.exprSpan(items), 1);
    const third = GuardedList.at(t.program.exprSpan(items), 2);
    try std.testing.expectEqual(own, GuardedList.at(t.program.exprSpan(items), 0));
    try std.testing.expect(second != constant);
    try std.testing.expect(third != constant);
    try std.testing.expect(second != third);
    try std.testing.expectEqualDeep(t.program.getExpr(seven).data, t.program.getExpr(GuardedList.at(t.program.exprSpan(t.program.getExpr(second).data.tuple), 1)).data);
}
