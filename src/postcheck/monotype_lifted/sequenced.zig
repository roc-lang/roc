//! Debug verification that every lifted function body is operand-sequenced
//! (design.md "Operand Sequencing"). Monotype lowering emits bodies in this
//! form and lifting keeps it, so the stages after lifting read statement
//! lists whose order is evaluation order and whose bindings are in lexical
//! scope for everything that follows them.
//! The walk runs from an explicit stack, so the depth of a body never becomes
//! native call depth.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const Ast = @import("ast.zig");
const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// The first expression found out of sequenced form, and the rule it breaks.
pub const Violation = struct {
    message: []const u8,
    expr: Ast.ExprId,
};

/// Where an expression stands in whatever holds it.
const Position = enum {
    /// The whole of a scope of its own: a function body, a branch body, a
    /// loop body, a join body or remainder, a false continuation, a
    /// recursive binding's value, an expect condition, or the evidence a
    /// compile-time or static-data candidate keeps.
    scope,
    /// A strict operand, evaluated to a value before its consumer.
    operand,
    /// A value whose statements join its holder's sequence without it being
    /// a strict operand: it may transfer control itself.
    joined,
    /// The final expression of a block whose statements all complete.
    final,
    /// The final expression of a block whose statements transfer control.
    final_after_transfer,
};

const Visit = struct {
    expr: Ast.ExprId,
    position: Position,
};

/// Panic in Debug builds when a lifted body is not operand-sequenced.
pub fn verify(program: *const Ast.Program) Allocator.Error!void {
    if (@import("builtin").mode != .debug) return;
    const violation = (try check(program)) orelse return;
    base.invariant("postcheck invariant violated: {s} (expression {d}, a {s})", .{
        violation.message,
        @backingInt(violation.expr),
        @tagName(program.getExpr(violation.expr).data),
    });
}

/// The check itself, apart from the panicking wrapper so tests can call it.
pub fn check(program: *const Ast.Program) Allocator.Error!?Violation {
    var checker: Checker = .{ .program = program };
    defer checker.deinit();
    checker.seen = try std.DynamicBitSetUnmanaged.initEmpty(program.allocator, program.exprCount());
    for (0..program.fnCount()) |index| {
        const function = program.getFnAt(index);
        switch (function.body) {
            .roc => |body| if (try checker.run(body)) |violation| return violation,
            .hosted => {},
        }
    }
    return null;
}

const Checker = struct {
    program: *const Ast.Program,
    stack: std.ArrayList(Visit) = .empty,
    /// The expressions with expressions inside them that a body has reached.
    seen: std.DynamicBitSetUnmanaged = .{},

    fn deinit(self: *Checker) void {
        self.stack.deinit(self.program.allocator);
        self.seen.deinit(self.program.allocator);
    }

    fn push(self: *Checker, expr: Ast.ExprId, position: Position) Allocator.Error!void {
        try self.stack.append(self.program.allocator, .{ .expr = expr, .position = position });
    }

    fn pushExprs(self: *Checker, span: Ast.Span(Ast.ExprId), position: Position) Allocator.Error!void {
        const exprs = self.program.exprSpan(span);
        for (0..span.len) |index| try self.push(GuardedList.at(exprs, index), position);
    }

    fn pushFields(self: *Checker, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        const fields = self.program.fieldExprSpan(span);
        for (0..span.len) |index| try self.push(GuardedList.at(fields, index).value, .operand);
    }

    fn pushCaptures(self: *Checker, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        const captures = self.program.captureOperandSpan(span);
        for (0..span.len) |index| try self.push(GuardedList.at(captures, index).value, .operand);
    }

    fn run(self: *Checker, body: Ast.ExprId) Allocator.Error!?Violation {
        self.stack.clearRetainingCapacity();
        try self.push(body, .scope);
        while (self.stack.pop()) |visit| {
            if (try self.visitExpr(visit)) |message| return .{ .message = message, .expr = visit.expr };
        }
        return null;
    }

    /// Check a statement list and queue its values. Returns whether the list
    /// transfers control, or the rule one of its statements breaks.
    fn visitStatements(self: *Checker, span: Ast.Span(Ast.StmtId), transfers: *bool) Allocator.Error!?[]const u8 {
        const statements = self.program.stmtSpan(span);
        transfers.* = false;
        for (0..span.len) |index| {
            if (transfers.*) return "a statement follows a transfer of control in its list";
            switch (self.program.getStmt(GuardedList.at(statements, index))) {
                .let_ => |binding| try self.push(binding.value, if (binding.recursive) .scope else .operand),
                .expr => |value| {
                    const data = self.program.getExpr(value).data;
                    if (data == .@"unreachable") return "an expression statement is an unreachable marker";
                    try self.push(value, .joined);
                    transfers.* = data.transfersControl();
                },
                .dbg => |value| try self.push(value, .joined),
                .expect => |condition| try self.push(condition, .scope),
                .return_ => |ret| {
                    try self.push(ret.value, .joined);
                    transfers.* = true;
                },
                .uninitialized => {},
                .crash, .checked_error => transfers.* = true,
            }
        }
        return null;
    }

    fn visitExpr(self: *Checker, visit: Visit) Allocator.Error!?[]const u8 {
        const data = self.program.getExpr(visit.expr).data;
        if (!data.isLeaf()) {
            const index = @backingInt(visit.expr);
            if (self.seen.isSet(index)) return "an expression with expressions inside it is used in two places";
            self.seen.set(index);
        }
        switch (visit.position) {
            .scope => {},
            .operand => {
                if (data == .block) return "a block is a strict operand";
                if (data.transfersControl()) return "a strict operand transfers control";
            },
            .joined => {
                if (data == .block) return "a block is a value inside another expression's sequence";
                if (data == .@"unreachable") return "an unreachable marker is a value inside another expression's sequence";
            },
            .final => if (data == .block) return "a block is another block's final expression",
            .final_after_transfer => if (data != .@"unreachable") return "a block whose statements transfer control does not end in an unreachable marker",
        }
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
            .let_ => return "a `let` expression remains",
            .lambda, .def_ref, .fn_def => return "an unlifted function remains",
            .block => |block| {
                if (block.statements.len == 0) return "a block has no statements";
                var transfers = false;
                if (try self.visitStatements(block.statements, &transfers)) |message| return message;
                try self.push(block.final_expr, if (transfers) .final_after_transfer else .final);
            },
            .list, .tuple => |items| try self.pushExprs(items, .operand),
            .tag => |tag| try self.pushExprs(tag.payloads, .operand),
            .low_level => |call| try self.pushExprs(call.args, .operand),
            .continue_ => |transfer| try self.pushExprs(transfer.values, .operand),
            .jump => |transfer| {
                try self.pushExprs(transfer.loop_values, .operand);
                try self.pushExprs(transfer.args, .operand);
            },
            .record => |fields| try self.pushFields(fields),
            .record_update => |update| {
                try self.push(update.base, .operand);
                try self.pushFields(update.fields);
            },
            .fn_ref => |reference| try self.pushCaptures(reference.captures),
            .call_value => |call| {
                try self.push(call.callee, .operand);
                try self.pushExprs(call.args, .operand);
            },
            .call_proc => |call| {
                try self.pushExprs(call.args, .operand);
                try self.pushCaptures(call.captures);
            },
            .structural_eq => |equal| {
                try self.push(equal.lhs, .operand);
                try self.push(equal.rhs, .operand);
            },
            .structural_hash => |hash| {
                try self.push(hash.value, .operand);
                try self.push(hash.hasher, .operand);
            },
            .loop_ => |loop| {
                try self.pushExprs(loop.initial_values, .operand);
                try self.push(loop.body, .scope);
            },
            .nominal => |value| try self.push(value, .operand),
            .field_access => |access| try self.push(access.receiver, .operand),
            .tuple_access => |access| try self.push(access.tuple, .operand),
            inline .expect_err, .literal_rejected => |failure| try self.push(failure.msg, .operand),
            .break_ => |maybe_value| if (maybe_value) |value| try self.push(value, .operand),
            .dbg => |value| try self.push(value, .joined),
            .typed_boundary => |boundary| try self.push(boundary.value, .joined),
            .row_widen => |widen| try self.push(widen.value, .joined),
            .return_ => |ret| try self.push(ret.value, .joined),
            .static_data_candidate => |candidate| if (candidate.value.subexpression()) |value| try self.push(value, .scope),
            .comptime_value => |candidate| try self.push(candidate.initializer, .scope),
            .expect => |condition| try self.push(condition, .scope),
            .join_point => |join| {
                try self.push(join.body, .scope);
                try self.push(join.remainder, .scope);
            },
            .if_initialized_payload => |conditional| {
                try self.push(conditional.cond, .operand);
                try self.push(conditional.initialized, .scope);
                try self.push(conditional.uninitialized, .scope);
            },
            inline .try_sequence, .try_record_sequence => |sequence| {
                try self.push(sequence.try_expr, .operand);
                try self.push(sequence.ok_body, .scope);
            },
            .match_ => |match| {
                try self.push(match.scrutinee, .operand);
                const branches = self.program.branchSpan(match.branches);
                for (0..match.branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    var transfers = false;
                    if (try self.visitStatements(branch.bindings, &transfers)) |message| return message;
                    if (branch.guard.get()) |guard| {
                        if (transfers) return "a match guard follows bindings that transfer control";
                        try self.push(guard, .joined);
                    }
                    try self.push(branch.body, .scope);
                }
            },
            .if_ => |conditional| {
                if (conditional.branches.len != 1) return "an `if` does not have exactly one condition";
                const branch = GuardedList.at(self.program.ifBranchSpan(conditional.branches), 0);
                try self.push(branch.cond, .operand);
                try self.push(branch.body, .scope);
                try self.push(conditional.final_else, .scope);
            },
        }
        return null;
    }
};

const check_module = @import("check");
const Type = @import("../monotype/type.zig");

/// A test program whose every expression has one type, so each test states
/// only the shape it is about.
pub const TestProgram = struct {
    program: Ast.Program,
    ty: Type.TypeId,
    next_symbol: u32 = 1,

    pub fn init(allocator: Allocator) Allocator.Error!TestProgram {
        var program = Ast.Program.init(
            allocator,
            check_module.CheckedNames.NameStore.init(allocator),
            Type.Store.init(allocator),
            .empty, // const_fn_evidence
            .empty, // const_fn_evidence_targets
            .empty, // const_fn_evidence_structurals
            .empty, // const_fn_evidence_frames
            .empty, // exprs
            .empty, // pats
            .empty, // stmts
            .empty, // locals
            .empty, // expr_ids
            .empty, // pat_ids
            .empty, // typed_locals
            .empty, // stmt_ids
            .empty, // field_exprs
            .empty, // field_access_segments
            .empty, // fn_def_captures
            .empty, // capture_operands
            .empty, // record_destructs
            .empty, // str_pattern_steps
            .empty, // branches
            .empty, // if_branches
            .empty, // string_literals
            Ast.ProcDebugNameMap.init(allocator),
            .empty, // source_files
            .empty, // expr_locs
            .empty, // expr_regions
            .empty, // stmt_locs
            .empty, // stmt_regions
            .empty, // inline_scopes
            .empty, // expr_inline_scopes
            .empty, // stmt_inline_scopes
            .empty, // local_names
            .empty, // static_data_values
            .empty, // comptime_sites
            100, // next_symbol
        );
        errdefer program.deinit();
        const ty = try program.types.add(.{ .primitive = .u8 });
        return .{ .program = program, .ty = ty };
    }

    pub fn deinit(self: *TestProgram) void {
        self.program.deinit();
    }

    pub fn expr(self: *TestProgram, data: Ast.ExprData) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{ .ty = self.ty, .data = data });
    }

    pub fn int(self: *TestProgram, value: u8) Allocator.Error!Ast.ExprId {
        return try self.expr(.{ .int_lit = .{ .bytes = @bitCast(@as(u128, value)), .kind = .u128 } });
    }

    pub fn tuple(self: *TestProgram, items: []const Ast.ExprId) Allocator.Error!Ast.ExprId {
        return try self.expr(.{ .tuple = try self.program.addExprSpan(items) });
    }

    /// A statement binding `value` to a new local.
    pub fn bind(self: *TestProgram, value: Ast.ExprId) Allocator.Error!Ast.StmtId {
        const local = try self.program.addLocal(@fromBackingInt(@intCast(self.next_symbol)), self.ty);
        self.next_symbol += 1;
        return try self.program.addStmt(.{ .let_ = .{
            .pat = try self.program.addPat(.{ .ty = self.ty, .data = .{ .bind = local } }),
            .value = value,
        } });
    }

    pub fn block(self: *TestProgram, statements: []const Ast.StmtId, final_expr: Ast.ExprId) Allocator.Error!Ast.ExprId {
        return try self.expr(.{ .block = .{
            .statements = try self.program.addStmtSpan(statements),
            .final_expr = final_expr,
        } });
    }

    /// Add a function whose body is `body`.
    pub fn function(self: *TestProgram, body: Ast.ExprId) Allocator.Error!Ast.FnId {
        defer self.next_symbol += 1;
        return try self.program.addFn(.{
            .symbol = @fromBackingInt(@intCast(self.next_symbol)),
            .args = .empty(),
            .captures = .empty(),
            .body = .{ .roc = body },
            .ret = self.ty,
        });
    }

    /// The rule the program breaks once `body` is a function's body too.
    pub fn broken(self: *TestProgram, body: Ast.ExprId) Allocator.Error!?[]const u8 {
        _ = try self.function(body);
        const violation = (try check(&self.program)) orelse return null;
        return violation.message;
    }
};

test "a body whose operands are values and whose statements are in scope order is sequenced" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    const pair = try t.tuple(&.{ seven, seven });
    const transfer = try t.program.addStmt(.{ .return_ = .{ .value = seven, .target = t.ty } });
    const taken = try t.block(&.{ try t.bind(pair), transfer }, try t.expr(.@"unreachable"));
    const body = try t.expr(.{ .if_ = .{
        .branches = try t.program.addIfBranchSpan(&.{.{ .cond = seven, .body = taken }}),
        .final_else = try t.block(&.{try t.bind(try t.tuple(&.{seven}))}, seven),
    } });
    try std.testing.expectEqual(@as(?[]const u8, null), try t.broken(body));
}

test "a block or a `let` left in an operand is not sequenced" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    const operand = try t.block(&.{try t.bind(seven)}, seven);
    try std.testing.expectEqualStrings("a block is a strict operand", (try t.broken(try t.tuple(&.{operand}))).?);

    var with_let = try TestProgram.init(std.testing.allocator);
    defer with_let.deinit();
    const eight = try with_let.int(8);
    const local = try with_let.program.addLocal(@fromBackingInt(@intCast(1)), with_let.ty);
    const binding = try with_let.expr(.{ .let_ = .{
        .bind = try with_let.program.addPat(.{ .ty = with_let.ty, .data = .{ .bind = local } }),
        .value = eight,
        .rest = eight,
    } });
    try std.testing.expectEqualStrings("a `let` expression remains", (try with_let.broken(binding)).?);
}

test "an operand that transfers control is not sequenced" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const transfer = try t.expr(.{ .return_ = .{ .value = try t.int(7), .target = t.ty } });
    try std.testing.expectEqualStrings("a strict operand transfers control", (try t.broken(try t.tuple(&.{transfer}))).?);
}

test "a statement after a transfer of control is not sequenced" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    const transfer = try t.program.addStmt(.{ .return_ = .{ .value = seven, .target = t.ty } });
    const after = try t.block(&.{ transfer, try t.bind(seven) }, try t.expr(.@"unreachable"));
    try std.testing.expectEqualStrings("a statement follows a transfer of control in its list", (try t.broken(after)).?);

    var open = try TestProgram.init(std.testing.allocator);
    defer open.deinit();
    const eight = try open.int(8);
    const leaves = try open.program.addStmt(.{ .return_ = .{ .value = eight, .target = open.ty } });
    try std.testing.expectEqualStrings(
        "a block whose statements transfer control does not end in an unreachable marker",
        (try open.broken(try open.block(&.{leaves}, eight))).?,
    );
}

test "an `if` with a second condition is not sequenced" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    const chain = try t.expr(.{ .if_ = .{
        .branches = try t.program.addIfBranchSpan(&.{ .{ .cond = seven, .body = seven }, .{ .cond = seven, .body = seven } }),
        .final_else = seven,
    } });
    try std.testing.expectEqualStrings("an `if` does not have exactly one condition", (try t.broken(chain)).?);
}

test "an expression with operands used in two places is not sequenced" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const shared = try t.tuple(&.{try t.int(7)});
    try std.testing.expectEqualStrings(
        "an expression with expressions inside it is used in two places",
        (try t.broken(try t.tuple(&.{ shared, shared }))).?,
    );
}
