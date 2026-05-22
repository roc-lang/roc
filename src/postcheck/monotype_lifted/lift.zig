//! Closure lifting over Monotype IR.

const std = @import("std");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const Ast = @import("ast.zig");

const Allocator = std.mem.Allocator;

pub fn run(
    allocator: Allocator,
    mono: Mono.Program,
) Common.LowerError!Ast.Program {
    var owned = mono;
    errdefer owned.deinit();

    var types = owned.types;
    owned.types = @import("../monotype/type.zig").Store.init(allocator);
    var string_literals = owned.string_literals;
    owned.string_literals = .empty;
    var name_store = owned.names;
    owned.names = @import("check").CheckedNames.NameStore.init(allocator);
    var program = Ast.Program.init(allocator, name_store, types, string_literals, owned.next_symbol);
    name_store = undefined;
    types = undefined;
    string_literals = undefined;
    errdefer program.deinit();

    try program.locals.appendSlice(allocator, owned.locals.items);

    var lifter = Lifter.init(allocator, &owned, &program);
    defer lifter.deinit();

    try lifter.lowerDefsAndRoots();
    program.next_symbol = lifter.symbols.next;

    owned.deinit();
    return program;
}

const DefMap = []?Ast.FnId;

const Lifter = struct {
    allocator: Allocator,
    input: *const Mono.Program,
    output: *Ast.Program,
    expr_map: std.AutoHashMap(Mono.ExprId, Ast.ExprId),
    pat_map: std.AutoHashMap(Mono.PatId, Ast.PatId),
    stmt_map: std.AutoHashMap(Mono.StmtId, Ast.StmtId),
    def_map: DefMap,
    symbols: Common.SymbolGen,

    fn init(allocator: Allocator, input: *const Mono.Program, output: *Ast.Program) Lifter {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .expr_map = std.AutoHashMap(Mono.ExprId, Ast.ExprId).init(allocator),
            .pat_map = std.AutoHashMap(Mono.PatId, Ast.PatId).init(allocator),
            .stmt_map = std.AutoHashMap(Mono.StmtId, Ast.StmtId).init(allocator),
            .def_map = &.{},
            .symbols = .{ .next = input.next_symbol },
        };
    }

    fn deinit(self: *Lifter) void {
        if (self.def_map.len > 0) self.allocator.free(self.def_map);
        self.stmt_map.deinit();
        self.pat_map.deinit();
        self.expr_map.deinit();
    }

    fn lowerDefsAndRoots(self: *Lifter) Allocator.Error!void {
        self.def_map = try self.allocator.alloc(?Ast.FnId, self.input.defs.items.len);
        @memset(self.def_map, null);

        for (self.input.defs.items, 0..) |def, index| {
            const fn_id = try self.lowerTopLevelDef(def);
            self.def_map[index] = fn_id;
        }

        for (self.input.roots.items) |root| {
            const raw = @intFromEnum(root.def);
            if (raw >= self.def_map.len) Common.invariant("Monotype root references a missing definition");
            const fn_id = self.def_map[raw] orelse
                Common.invariant("Monotype root definition was not lifted");
            try self.output.roots.append(self.allocator, .{
                .fn_id = fn_id,
                .request = root.request,
            });
        }
    }

    fn lowerTopLevelDef(self: *Lifter, def: Mono.Def) Allocator.Error!Ast.FnId {
        var captures = CaptureSet.init(self.allocator);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(&bound, self.input.typedLocalSpan(def.args));
        try captures.collectExpr(self.input, def.body, &bound);

        if (captures.items.items.len != 0) {
            Common.invariant("top-level Monotype definition has free locals after checked closure collection");
        }

        const body = try self.lowerExpr(def.body);
        const args = try self.copyTypedLocalSpan(def.args);
        return try self.output.addFn(.{
            .symbol = def.symbol,
            .source = def.fn_def,
            .args = args,
            .captures = .empty(),
            .body = body,
            .ret = def.ret,
        });
    }

    fn lowerExpr(self: *Lifter, expr_id: Mono.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |cached| return cached;

        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        const data: Ast.ExprData = switch (expr.data) {
            .local => |local| .{ .local = local },
            .unit => .unit,
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .record => |fields| .{ .record = try self.lowerFieldExprSpan(fields) },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.lowerExprSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.lowerExpr(backing) },
            .let_ => |let_| .{ .let_ = .{
                .bind = try self.lowerPat(let_.bind),
                .value = try self.lowerExpr(let_.value),
                .rest = try self.lowerExpr(let_.rest),
            } },
            .lambda => |lambda| try self.liftLambda(expr.ty, lambda),
            .fn_def => |fn_def| .{ .fn_def = fn_def },
            .call_value => |call| .{ .call_value = .{
                .callee = try self.lowerExpr(call.callee),
                .args = try self.lowerExprSpan(call.args),
            } },
            .call_proc => |call| .{ .call_proc = .{
                .callee = call.callee,
                .args = try self.lowerExprSpan(call.args),
            } },
            .low_level => |call| .{ .low_level = .{
                .op = call.op,
                .args = try self.lowerExprSpan(call.args),
            } },
            .field_access => |field| .{ .field_access = .{
                .receiver = try self.lowerExpr(field.receiver),
                .field = field.field,
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
                .negated = eq.negated,
            } },
            .match_ => |match| .{ .match_ = .{
                .scrutinee = try self.lowerExpr(match.scrutinee),
                .branches = try self.lowerBranchSpan(match.branches),
            } },
            .if_ => |if_| .{ .if_ = .{
                .branches = try self.lowerIfBranchSpan(if_.branches),
                .final_else = try self.lowerExpr(if_.final_else),
            } },
            .block => |block| .{ .block = .{
                .statements = try self.lowerStmtSpan(block.statements),
                .final_expr = try self.lowerExpr(block.final_expr),
            } },
            .loop_ => |loop| .{ .loop_ = .{
                .params = try self.copyTypedLocalSpan(loop.params),
                .initial_values = try self.lowerExprSpan(loop.initial_values),
                .body = try self.lowerExpr(loop.body),
            } },
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.lowerExpr(value) else null },
            .continue_ => |continue_| .{ .continue_ = .{ .values = try self.lowerExprSpan(continue_.values) } },
            .return_ => |value| .{ .return_ = try self.lowerExpr(value) },
            .crash => |msg| .{ .crash = msg },
            .dbg => |child| .{ .dbg = try self.lowerExpr(child) },
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
        };

        const lowered = try self.output.addExpr(.{ .ty = expr.ty, .data = data });
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    fn liftLambda(self: *Lifter, ty: @import("../monotype/type.zig").TypeId, lambda: Mono.LambdaExpr) Allocator.Error!Ast.ExprData {
        var captures = CaptureSet.init(self.allocator);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(&bound, self.input.typedLocalSpan(lambda.args));
        try captures.collectExpr(self.input, lambda.body, &bound);

        const body = try self.lowerExpr(lambda.body);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        const fn_id = try self.output.addFn(.{
            .symbol = self.symbols.fresh(),
            .source = lambda.source,
            .args = try self.copyTypedLocalSpan(lambda.args),
            .captures = capture_span,
            .body = body,
            .ret = functionRet(&self.output.types, ty),
        });
        return .{ .fn_ref = fn_id };
    }

    fn lowerPat(self: *Lifter, pat_id: Mono.PatId) Allocator.Error!Ast.PatId {
        if (self.pat_map.get(pat_id)) |cached| return cached;
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| .{ .bind = local },
            .wildcard => .wildcard,
            .as => |as| .{ .as = .{
                .pattern = try self.lowerPat(as.pattern),
                .local = as.local,
            } },
            .record => |fields| .{ .record = try self.lowerRecordDestructSpan(fields) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpan(items) },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.lowerPatSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.lowerPat(backing) },
            .int_lit => |value| .{ .int_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .str_lit => |value| .{ .str_lit = value },
        };
        const lowered = try self.output.addPat(.{ .ty = pat.ty, .data = data });
        try self.pat_map.put(pat_id, lowered);
        return lowered;
    }

    fn lowerStmt(self: *Lifter, stmt_id: Mono.StmtId) Allocator.Error!Ast.StmtId {
        if (self.stmt_map.get(stmt_id)) |cached| return cached;
        const stmt = self.input.stmts.items[@intFromEnum(stmt_id)];
        const lowered_stmt: Ast.Stmt = switch (stmt) {
            .let_ => |let_| .{ .let_ = .{
                .pat = try self.lowerPat(let_.pat),
                .value = try self.lowerExpr(let_.value),
            } },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .dbg => |expr| .{ .dbg = try self.lowerExpr(expr) },
            .return_ => |expr| .{ .return_ = try self.lowerExpr(expr) },
            .crash => |msg| .{ .crash = msg },
        };
        const lowered = try self.output.addStmt(lowered_stmt);
        try self.stmt_map.put(stmt_id, lowered);
        return lowered;
    }

    fn lowerExprSpan(self: *Lifter, span: Mono.Span(Mono.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const input_items = self.input.exprSpan(span);
        const lowered = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerExpr(item);
        return try self.output.addExprSpan(lowered);
    }

    fn lowerPatSpan(self: *Lifter, span: Mono.Span(Mono.PatId)) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.input.patSpan(span);
        const lowered = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerPat(item);
        return try self.output.addPatSpan(lowered);
    }

    fn lowerStmtSpan(self: *Lifter, span: Mono.Span(Mono.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.stmtSpan(span);
        const lowered = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerStmt(item);
        return try self.output.addStmtSpan(lowered);
    }

    fn copyTypedLocalSpan(self: *Lifter, span: Mono.Span(Mono.TypedLocal)) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        return try self.output.addTypedLocalSpan(self.input.typedLocalSpan(span));
    }

    fn lowerFieldExprSpan(self: *Lifter, span: Mono.Span(Mono.FieldExpr)) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const input_items = self.input.fieldExprSpan(span);
        const lowered = try self.allocator.alloc(Ast.FieldExpr, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addFieldExprSpan(lowered);
    }

    fn lowerRecordDestructSpan(self: *Lifter, span: Mono.Span(Mono.RecordDestruct)) Allocator.Error!Ast.Span(Ast.RecordDestruct) {
        const input_items = self.input.recordDestructSpan(span);
        const lowered = try self.allocator.alloc(Ast.RecordDestruct, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .pattern = try self.lowerPat(field.pattern),
            };
        }
        return try self.output.addRecordDestructSpan(lowered);
    }

    fn lowerBranchSpan(self: *Lifter, span: Mono.Span(Mono.Branch)) Allocator.Error!Ast.Span(Ast.Branch) {
        const input_items = self.input.branchSpan(span);
        const lowered = try self.allocator.alloc(Ast.Branch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .pat = try self.lowerPat(branch.pat),
                .guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null,
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.output.addBranchSpan(lowered);
    }

    fn lowerIfBranchSpan(self: *Lifter, span: Mono.Span(Mono.IfBranch)) Allocator.Error!Ast.Span(Ast.IfBranch) {
        const input_items = self.input.ifBranchSpan(span);
        const lowered = try self.allocator.alloc(Ast.IfBranch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .cond = try self.lowerExpr(branch.cond),
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.output.addIfBranchSpan(lowered);
    }
};

const BoundSet = std.AutoHashMap(Mono.LocalId, void);

const CaptureSet = struct {
    allocator: Allocator,
    items: std.ArrayList(Ast.TypedLocal),
    seen: std.AutoHashMap(Mono.LocalId, void),

    fn init(allocator: Allocator) CaptureSet {
        return .{
            .allocator = allocator,
            .items = .empty,
            .seen = std.AutoHashMap(Mono.LocalId, void).init(allocator),
        };
    }

    fn deinit(self: *CaptureSet) void {
        self.seen.deinit();
        self.items.deinit(self.allocator);
    }

    fn addIfFree(self: *CaptureSet, input: *const Mono.Program, local: Mono.LocalId, bound: *const BoundSet) Allocator.Error!void {
        if (bound.contains(local) or self.seen.contains(local)) return;
        try self.seen.put(local, {});
        const local_data = input.locals.items[@intFromEnum(local)];
        try self.items.append(self.allocator, .{
            .local = local,
            .ty = local_data.ty,
        });
    }

    fn collectExpr(self: *CaptureSet, input: *const Mono.Program, expr_id: Mono.ExprId, bound: *BoundSet) Allocator.Error!void {
        const expr = input.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local => |local| try self.addIfFree(input, local, bound),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .fn_def,
            .crash,
            => {},
            .list,
            .tuple,
            => |items| for (input.exprSpan(items)) |child| try self.collectExpr(input, child, bound),
            .record => |fields| for (input.fieldExprSpan(fields)) |field| try self.collectExpr(input, field.value, bound),
            .tag => |tag| for (input.exprSpan(tag.payloads)) |child| try self.collectExpr(input, child, bound),
            .nominal,
            .return_,
            .dbg,
            .expect,
            => |child| try self.collectExpr(input, child, bound),
            .let_ => |let_| {
                try self.collectExpr(input, let_.value, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindPat(self.allocator, input, let_.bind, bound, &added);
                try self.collectExpr(input, let_.rest, bound);
                removeBound(bound, added.items);
            },
            .lambda => |lambda| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, bound, input.typedLocalSpan(lambda.args), &added);
                try self.collectExpr(input, lambda.body, bound);
                removeBound(bound, added.items);
            },
            .call_value => |call| {
                try self.collectExpr(input, call.callee, bound);
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(input, arg, bound);
            },
            .call_proc => |call| for (input.exprSpan(call.args)) |arg| try self.collectExpr(input, arg, bound),
            .low_level => |call| for (input.exprSpan(call.args)) |arg| try self.collectExpr(input, arg, bound),
            .field_access => |field| try self.collectExpr(input, field.receiver, bound),
            .tuple_access => |access| try self.collectExpr(input, access.tuple, bound),
            .structural_eq => |eq| {
                try self.collectExpr(input, eq.lhs, bound);
                try self.collectExpr(input, eq.rhs, bound);
            },
            .match_ => |match| {
                try self.collectExpr(input, match.scrutinee, bound);
                for (input.branchSpan(match.branches)) |branch| {
                    var added = std.ArrayList(Mono.LocalId).empty;
                    defer added.deinit(self.allocator);
                    try bindPat(self.allocator, input, branch.pat, bound, &added);
                    if (branch.guard) |guard| try self.collectExpr(input, guard, bound);
                    try self.collectExpr(input, branch.body, bound);
                    removeBound(bound, added.items);
                }
            },
            .if_ => |if_| {
                for (input.ifBranchSpan(if_.branches)) |branch| {
                    try self.collectExpr(input, branch.cond, bound);
                    try self.collectExpr(input, branch.body, bound);
                }
                try self.collectExpr(input, if_.final_else, bound);
            },
            .block => |block| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                for (input.stmtSpan(block.statements)) |stmt| try self.collectStmt(input, stmt, bound, &added);
                try self.collectExpr(input, block.final_expr, bound);
                removeBound(bound, added.items);
            },
            .loop_ => |loop| {
                for (input.exprSpan(loop.initial_values)) |initial| try self.collectExpr(input, initial, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, bound, input.typedLocalSpan(loop.params), &added);
                try self.collectExpr(input, loop.body, bound);
                removeBound(bound, added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectExpr(input, value, bound),
            .continue_ => |continue_| for (input.exprSpan(continue_.values)) |value| try self.collectExpr(input, value, bound),
        }
    }

    fn collectStmt(self: *CaptureSet, input: *const Mono.Program, stmt_id: Mono.StmtId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
        switch (input.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| {
                try self.collectExpr(input, let_.value, bound);
                try bindPat(self.allocator, input, let_.pat, bound, added);
            },
            .expr,
            .expect,
            .dbg,
            .return_,
            => |expr| try self.collectExpr(input, expr, bound),
            .crash => {},
        }
    }
};

fn bindTypedLocals(bound: *BoundSet, locals: []const Mono.TypedLocal) Allocator.Error!void {
    for (locals) |local| try bound.put(local.local, {});
}

fn bindTypedLocalsTracked(allocator: Allocator, bound: *BoundSet, locals: []const Mono.TypedLocal, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    for (locals) |local| {
        try bound.put(local.local, {});
        try added.append(allocator, local.local);
    }
}

fn bindPat(allocator: Allocator, input: *const Mono.Program, pat_id: Mono.PatId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    switch (input.pats.items[@intFromEnum(pat_id)].data) {
        .bind => |local| {
            try bound.put(local, {});
            try added.append(allocator, local);
        },
        .wildcard,
        .int_lit,
        .dec_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .str_lit,
        => {},
        .as => |as| {
            try bindPat(allocator, input, as.pattern, bound, added);
            try bound.put(as.local, {});
            try added.append(allocator, as.local);
        },
        .record => |fields| for (input.recordDestructSpan(fields)) |field| try bindPat(allocator, input, field.pattern, bound, added),
        .tuple => |items| for (input.patSpan(items)) |child| try bindPat(allocator, input, child, bound, added),
        .tag => |tag| for (input.patSpan(tag.payloads)) |child| try bindPat(allocator, input, child, bound, added),
        .nominal => |backing| try bindPat(allocator, input, backing, bound, added),
    }
}

fn removeBound(bound: *BoundSet, locals: []const Mono.LocalId) void {
    var index = locals.len;
    while (index > 0) {
        index -= 1;
        _ = bound.remove(locals[index]);
    }
}

fn functionRet(types: *const @import("../monotype/type.zig").Store, ty: @import("../monotype/type.zig").TypeId) @import("../monotype/type.zig").TypeId {
    return switch (types.get(ty)) {
        .func => |fn_ty| fn_ty.ret,
        else => Common.invariant("lifted lambda expression did not have a function type"),
    };
}

test "monotype lifted lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
