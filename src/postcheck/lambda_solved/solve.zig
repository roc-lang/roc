//! Lambda solving over lifted Monotype IR.

const std = @import("std");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;

pub fn run(
    allocator: Allocator,
    lifted: Lifted.Program,
) Common.LowerError!Ast.Program {
    var owned = lifted;
    errdefer owned.deinit();

    var program = Ast.Program.init(allocator, owned);
    owned = undefined;
    errdefer program.deinit();

    var solver = try Solver.init(allocator, &program);
    defer solver.deinit();
    try solver.solve();

    return program;
}

const Solver = struct {
    allocator: Allocator,
    program: *Ast.Program,
    local_tys: []?Type.TypeVarId,
    expr_tys: []?Type.TypeVarId,
    pat_tys: []?Type.TypeVarId,
    expr_done: []bool,
    loop_results: std.ArrayList(Type.TypeVarId),
    loop_params: std.ArrayList(Type.Span),
    return_tys: std.ArrayList(Type.TypeVarId),

    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Solver {
        const local_tys = try allocator.alloc(?Type.TypeVarId, program.lifted.locals.items.len);
        errdefer allocator.free(local_tys);
        @memset(local_tys, null);

        const expr_tys = try allocator.alloc(?Type.TypeVarId, program.lifted.exprs.items.len);
        errdefer allocator.free(expr_tys);
        @memset(expr_tys, null);

        const expr_done = try allocator.alloc(bool, program.lifted.exprs.items.len);
        errdefer allocator.free(expr_done);
        @memset(expr_done, false);

        const pat_tys = try allocator.alloc(?Type.TypeVarId, program.lifted.pats.items.len);
        errdefer allocator.free(pat_tys);
        @memset(pat_tys, null);

        return .{
            .allocator = allocator,
            .program = program,
            .local_tys = local_tys,
            .expr_tys = expr_tys,
            .pat_tys = pat_tys,
            .expr_done = expr_done,
            .loop_results = .empty,
            .loop_params = .empty,
            .return_tys = .empty,
        };
    }

    fn deinit(self: *Solver) void {
        self.return_tys.deinit(self.allocator);
        self.loop_params.deinit(self.allocator);
        self.loop_results.deinit(self.allocator);
        self.allocator.free(self.expr_done);
        self.allocator.free(self.pat_tys);
        self.allocator.free(self.expr_tys);
        self.allocator.free(self.local_tys);
    }

    fn solve(self: *Solver) Allocator.Error!void {
        for (self.program.lifted.locals.items, 0..) |local, index| {
            self.local_tys[index] = try self.lowerTypeFresh(local.ty);
        }

        try self.program.fn_tys.ensureTotalCapacity(self.allocator, self.program.lifted.fns.items.len);
        try self.program.defs.ensureTotalCapacity(self.allocator, self.program.lifted.fns.items.len);

        for (self.program.lifted.fns.items, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            const fn_ty = try self.functionType(fn_id, fn_);
            try self.program.fn_tys.append(self.allocator, fn_ty);
            try self.program.defs.append(self.allocator, .{
                .symbol = fn_.symbol,
                .ty = fn_ty,
                .body = fn_.body,
            });
        }

        for (self.program.lifted.fns.items, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.solveFn(fn_id, fn_);
        }

        try self.program.expr_tys.ensureTotalCapacity(self.allocator, self.expr_tys.len);
        for (self.expr_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.lowerTypeFresh(self.program.lifted.exprs.items[index].ty);
            try self.program.expr_tys.append(self.allocator, self.program.types.root(ty));
        }

        try self.program.pat_tys.ensureTotalCapacity(self.allocator, self.pat_tys.len);
        for (self.pat_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.lowerTypeFresh(self.program.lifted.pats.items[index].ty);
            try self.program.pat_tys.append(self.allocator, self.program.types.root(ty));
        }

        try self.program.local_tys.ensureTotalCapacity(self.allocator, self.local_tys.len);
        for (self.local_tys) |maybe_ty| {
            const ty = maybe_ty orelse Common.invariant("Lambda Solved local type slot was not initialized");
            try self.program.local_tys.append(self.allocator, self.program.types.root(ty));
        }
    }

    fn functionType(self: *Solver, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!Type.TypeVarId {
        _ = fn_id;

        const arg_locals = self.program.lifted.typedLocalSpan(fn_.args);
        const args = try self.allocator.alloc(Type.TypeVarId, arg_locals.len);
        defer self.allocator.free(args);
        for (arg_locals, 0..) |arg, i| {
            args[i] = try self.lowerTypeFresh(arg.ty);
            try self.unify(args[i], self.localTy(arg.local));
        }

        const capture_locals = self.program.lifted.typedLocalSpan(fn_.captures);
        const captures = try self.allocator.alloc(Type.Capture, capture_locals.len);
        defer self.allocator.free(captures);
        for (capture_locals, 0..) |capture, i| {
            const local = self.program.lifted.locals.items[@intFromEnum(capture.local)];
            captures[i] = .{
                .local = capture.local,
                .symbol = local.symbol,
                .binder = local.binder,
                .ty = self.localTy(capture.local),
            };
        }

        const capture_span = try self.program.types.addCaptures(captures);
        const members = [_]Type.FnMember{.{
            .lambda = fn_.symbol,
            .captures = capture_span,
        }};
        const callable = try self.program.types.add(.{ .lambda_set = try self.program.types.addMembers(&members) });

        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(args),
            .callable = callable,
            .ret = try self.lowerTypeFresh(fn_.ret),
        } });
    }

    fn solveFn(self: *Solver, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!void {
        const fn_ty = self.program.fn_tys.items[@intFromEnum(fn_id)];
        const fn_content = self.program.types.rootContent(fn_ty);
        const func = switch (fn_content) {
            .func => |func| func,
            else => Common.invariant("Lambda Solved function table contains a non-function type"),
        };

        const arg_tys = self.program.types.span(func.args);
        const arg_locals = self.program.lifted.typedLocalSpan(fn_.args);
        if (arg_tys.len != arg_locals.len) Common.invariant("Lambda Solved function arity changed after registration");
        for (arg_locals, 0..) |arg, i| {
            try self.unify(arg_tys[i], self.localTy(arg.local));
        }

        try self.return_tys.append(self.allocator, func.ret);
        defer _ = self.return_tys.pop();

        const body_ty = try self.inferExpr(fn_.body);
        try self.unify(body_ty, func.ret);
    }

    fn inferExpr(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        const expected = try self.exprSlot(expr_id);
        if (self.expr_done[index]) return expected;
        self.expr_done[index] = true;

        const expr = self.program.lifted.exprs.items[index];
        switch (expr.data) {
            .local => |local| try self.unify(expected, self.localTy(local)),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .crash,
            => {},
            .list => |items| {
                const elem_ty = try self.listElem(expected);
                for (self.program.lifted.exprSpan(items)) |child| {
                    try self.unify(try self.inferExpr(child), elem_ty);
                }
            },
            .tuple => |items| {
                const item_tys = try self.tupleItems(expected);
                const children = self.program.lifted.exprSpan(items);
                if (item_tys.len != children.len) Common.invariant("tuple expression arity differs from its checked type");
                for (children, 0..) |child, i| {
                    try self.unify(try self.inferExpr(child), item_tys[i]);
                }
            },
            .record => |fields| {
                for (self.program.lifted.fieldExprSpan(fields)) |field| {
                    try self.unify(try self.inferExpr(field.value), try self.recordField(expected, field.name));
                }
            },
            .tag => |tag| {
                const payload_tys = try self.tagPayloads(expected, tag.name);
                const payloads = self.program.lifted.exprSpan(tag.payloads);
                if (payload_tys.len != payloads.len) Common.invariant("tag expression payload arity differs from its checked type");
                for (payloads, 0..) |payload, i| {
                    try self.unify(try self.inferExpr(payload), payload_tys[i]);
                }
            },
            .nominal => |backing| {
                if (try self.namedBacking(expected)) |backing_ty| {
                    try self.unify(try self.inferExpr(backing), backing_ty);
                } else {
                    _ = try self.inferExpr(backing);
                }
            },
            .let_ => |let_| {
                const value_ty = try self.inferExpr(let_.value);
                try self.bindPattern(let_.bind, value_ty);
                try self.unify(expected, try self.inferExpr(let_.rest));
            },
            .fn_ref => |fn_id| try self.unify(expected, self.program.fn_tys.items[@intFromEnum(fn_id)]),
            .fn_def => |fn_def| try self.unify(expected, self.program.fn_tys.items[@intFromEnum(self.fnIdForSource(fn_def))]),
            .call_value => |call| {
                const args = try self.inferExprSpan(call.args);
                defer self.allocator.free(args);
                const callable = try self.program.types.add(.unbound);
                const wanted = try self.program.types.add(.{ .func = .{
                    .args = try self.program.types.addSpan(args),
                    .callable = callable,
                    .ret = expected,
                } });
                try self.unify(try self.inferExpr(call.callee), wanted);
            },
            .call_proc => |call| {
                const args = try self.inferExprSpan(call.args);
                defer self.allocator.free(args);
                const callable = try self.program.types.add(.unbound);
                const wanted = try self.program.types.add(.{ .func = .{
                    .args = try self.program.types.addSpan(args),
                    .callable = callable,
                    .ret = expected,
                } });
                try self.unify(self.program.fn_tys.items[@intFromEnum(self.fnIdForSource(call.callee))], wanted);
            },
            .low_level => |call| {
                for (self.program.lifted.exprSpan(call.args)) |arg| {
                    _ = try self.inferExpr(arg);
                }
            },
            .field_access => |field| {
                const receiver_ty = try self.inferExpr(field.receiver);
                try self.unify(expected, try self.recordField(receiver_ty, field.field));
            },
            .tuple_access => |access| {
                const receiver_ty = try self.inferExpr(access.tuple);
                const items = try self.tupleItems(receiver_ty);
                if (access.elem_index >= items.len) Common.invariant("tuple access index exceeds tuple arity");
                try self.unify(expected, items[access.elem_index]);
            },
            .structural_eq => |eq| {
                const lhs = try self.inferExpr(eq.lhs);
                const rhs = try self.inferExpr(eq.rhs);
                try self.unify(lhs, rhs);
            },
            .match_ => |match| {
                const scrutinee_ty = try self.inferExpr(match.scrutinee);
                for (self.program.lifted.branchSpan(match.branches)) |branch| {
                    try self.bindPattern(branch.pat, scrutinee_ty);
                    if (branch.guard) |guard| _ = try self.inferExpr(guard);
                    try self.unify(expected, try self.inferExpr(branch.body));
                }
            },
            .if_ => |if_| {
                for (self.program.lifted.ifBranchSpan(if_.branches)) |branch| {
                    _ = try self.inferExpr(branch.cond);
                    try self.unify(expected, try self.inferExpr(branch.body));
                }
                try self.unify(expected, try self.inferExpr(if_.final_else));
            },
            .block => |block| {
                for (self.program.lifted.stmtSpan(block.statements)) |stmt| try self.inferStmt(stmt);
                try self.unify(expected, try self.inferExpr(block.final_expr));
            },
            .loop_ => |loop| {
                const params = self.program.lifted.typedLocalSpan(loop.params);
                const initials = self.program.lifted.exprSpan(loop.initial_values);
                if (params.len != initials.len) Common.invariant("loop parameter count differs from initial value count");
                const param_tys = try self.allocator.alloc(Type.TypeVarId, params.len);
                defer self.allocator.free(param_tys);
                for (params, 0..) |param, i| {
                    param_tys[i] = self.localTy(param.local);
                    try self.unify(param_tys[i], try self.inferExpr(initials[i]));
                }
                try self.loop_results.append(self.allocator, expected);
                try self.loop_params.append(self.allocator, try self.program.types.addSpan(param_tys));
                defer _ = self.loop_params.pop();
                defer _ = self.loop_results.pop();
                try self.unify(expected, try self.inferExpr(loop.body));
            },
            .break_ => |maybe| {
                if (maybe) |value| {
                    try self.unify(self.currentLoopResult(), try self.inferExpr(value));
                }
            },
            .continue_ => |continue_| {
                const params = self.program.types.span(self.currentLoopParams());
                const values = self.program.lifted.exprSpan(continue_.values);
                if (params.len != values.len) Common.invariant("continue value count differs from loop parameter count");
                for (values, 0..) |value, i| {
                    try self.unify(params[i], try self.inferExpr(value));
                }
            },
            .return_ => |value| try self.unify(self.currentReturnTy(), try self.inferExpr(value)),
            .dbg,
            .expect,
            => |child| _ = try self.inferExpr(child),
        }
        return self.program.types.root(expected);
    }

    fn inferStmt(self: *Solver, stmt_id: Lifted.StmtId) Allocator.Error!void {
        switch (self.program.lifted.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| {
                const value_ty = try self.inferExpr(let_.value);
                try self.bindPattern(let_.pat, value_ty);
            },
            .expr,
            .expect,
            .dbg,
            => |expr| _ = try self.inferExpr(expr),
            .return_ => |expr| try self.unify(self.currentReturnTy(), try self.inferExpr(expr)),
            .crash => {},
        }
    }

    fn bindPattern(self: *Solver, pat_id: Lifted.PatId, value_ty: Type.TypeVarId) Allocator.Error!void {
        const pat = self.program.lifted.pats.items[@intFromEnum(pat_id)];
        const pat_ty = try self.patSlot(pat_id);
        try self.unify(value_ty, pat_ty);
        switch (pat.data) {
            .bind => |local| try self.unify(self.localTy(local), pat_ty),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .as => |as| {
                try self.unify(self.localTy(as.local), pat_ty);
                try self.bindPattern(as.pattern, pat_ty);
            },
            .record => |fields| {
                for (self.program.lifted.recordDestructSpan(fields)) |field| {
                    try self.bindPattern(field.pattern, try self.recordField(pat_ty, field.name));
                }
            },
            .tuple => |items| {
                const item_tys = try self.tupleItems(pat_ty);
                const pats = self.program.lifted.patSpan(items);
                if (item_tys.len != pats.len) Common.invariant("tuple pattern arity differs from its checked type");
                for (pats, 0..) |child, i| try self.bindPattern(child, item_tys[i]);
            },
            .tag => |tag| {
                const payload_tys = try self.tagPayloads(pat_ty, tag.name);
                const payloads = self.program.lifted.patSpan(tag.payloads);
                if (payload_tys.len != payloads.len) Common.invariant("tag pattern payload arity differs from its checked type");
                for (payloads, 0..) |child, i| try self.bindPattern(child, payload_tys[i]);
            },
            .nominal => |backing| {
                if (try self.namedBacking(pat_ty)) |backing_ty| {
                    try self.bindPattern(backing, backing_ty);
                } else {
                    try self.bindPattern(backing, pat_ty);
                }
            },
        }
    }

    fn exprSlot(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| return ty;

        const expr = self.program.lifted.exprs.items[index];
        const ty = switch (expr.data) {
            .fn_ref => |fn_id| self.program.fn_tys.items[@intFromEnum(fn_id)],
            .fn_def => |fn_def| self.program.fn_tys.items[@intFromEnum(self.fnIdForSource(fn_def))],
            else => try self.lowerTypeFresh(expr.ty),
        };
        self.expr_tys[index] = ty;
        return ty;
    }

    fn patSlot(self: *Solver, pat_id: Lifted.PatId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(pat_id);
        if (self.pat_tys[index]) |ty| return ty;
        const ty = try self.lowerTypeFresh(self.program.lifted.pats.items[index].ty);
        self.pat_tys[index] = ty;
        return ty;
    }

    fn inferExprSpan(self: *Solver, span: Lifted.Span(Lifted.ExprId)) Allocator.Error![]Type.TypeVarId {
        const exprs = self.program.lifted.exprSpan(span);
        const tys = try self.allocator.alloc(Type.TypeVarId, exprs.len);
        errdefer self.allocator.free(tys);
        for (exprs, 0..) |expr, i| tys[i] = try self.inferExpr(expr);
        return tys;
    }

    fn localTy(self: *Solver, local: Lifted.LocalId) Type.TypeVarId {
        return self.local_tys[@intFromEnum(local)] orelse Common.invariant("Lambda Solved local reached solver without a type slot");
    }

    fn currentReturnTy(self: *Solver) Type.TypeVarId {
        if (self.return_tys.items.len == 0) Common.invariant("return expression reached Lambda Solved outside a function");
        return self.return_tys.items[self.return_tys.items.len - 1];
    }

    fn currentLoopResult(self: *Solver) Type.TypeVarId {
        if (self.loop_results.items.len == 0) Common.invariant("break expression reached Lambda Solved outside a loop");
        return self.loop_results.items[self.loop_results.items.len - 1];
    }

    fn currentLoopParams(self: *Solver) Type.Span {
        if (self.loop_params.items.len == 0) Common.invariant("continue expression reached Lambda Solved outside a loop");
        return self.loop_params.items[self.loop_params.items.len - 1];
    }

    fn fnIdForSource(self: *Solver, fn_def: @import("../monotype/ast.zig").FnTemplate) Lifted.FnId {
        for (self.program.lifted.fns.items, 0..) |fn_, index| {
            if (fn_.source) |source| {
                if (std.meta.eql(source, fn_def)) return @enumFromInt(@as(u32, @intCast(index)));
            }
        }
        Common.invariant("function value referenced a function definition that was not lifted");
    }

    fn lowerTypeFresh(self: *Solver, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        var cloner = TypeCloner.init(self);
        defer cloner.deinit();
        return try cloner.lower(ty);
    }

    fn listElem(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        return switch (try self.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("list expression had a non-list checked type"),
        };
    }

    fn tupleItems(self: *Solver, ty: Type.TypeVarId) Allocator.Error![]const Type.TypeVarId {
        return switch (try self.shapeContent(ty)) {
            .tuple => |items| self.program.types.span(items),
            else => Common.invariant("tuple expression had a non-tuple checked type"),
        };
    }

    fn recordField(self: *Solver, ty: Type.TypeVarId, name: Type.names.RecordFieldNameId) Allocator.Error!Type.TypeVarId {
        return switch (try self.shapeContent(ty)) {
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    if (field.name == name) return field.ty;
                }
                Common.invariant("record field was absent from checked record type");
            },
            else => Common.invariant("record field operation had a non-record checked type"),
        };
    }

    fn tagPayloads(self: *Solver, ty: Type.TypeVarId, name: Type.names.TagNameId) Allocator.Error![]const Type.TypeVarId {
        return switch (try self.shapeContent(ty)) {
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    if (tag.name == name) return self.program.types.span(tag.payloads);
                }
                Common.invariant("tag was absent from checked tag-union type");
            },
            else => Common.invariant("tag operation had a non-tag-union checked type"),
        };
    }

    fn namedBacking(self: *Solver, ty: Type.TypeVarId) Allocator.Error!?Type.TypeVarId {
        return switch (self.program.types.rootContent(ty)) {
            .named => |named| if (named.backing) |backing| backing.ty else null,
            else => null,
        };
    }

    fn shapeContent(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Content {
        var current = self.program.types.root(ty);
        while (true) {
            switch (self.program.types.get(current)) {
                .named => |named| if (named.backing) |backing| {
                    current = self.program.types.root(backing.ty);
                    continue;
                } else return self.program.types.get(current),
                else => return self.program.types.get(current),
            }
        }
    }

    fn unify(self: *Solver, lhs: Type.TypeVarId, rhs: Type.TypeVarId) Allocator.Error!void {
        const a = self.program.types.root(lhs);
        const b = self.program.types.root(rhs);
        if (a == b) return;

        const left = self.program.types.get(a);
        const right = self.program.types.get(b);

        switch (left) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound => {
                self.program.types.set(a, .{ .link = b });
                return;
            },
            .forall => Common.invariant("generalized Lambda Solved type reached local unification without instantiation"),
            else => {},
        }
        switch (right) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound => {
                self.program.types.set(b, .{ .link = a });
                return;
            },
            .forall => Common.invariant("generalized Lambda Solved type reached local unification without instantiation"),
            else => {},
        }

        switch (left) {
            .primitive => |left_primitive| switch (right) {
                .primitive => |right_primitive| {
                    if (left_primitive != right_primitive) Common.invariant("primitive types failed Lambda Solved unification");
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("primitive type failed Lambda Solved unification"),
            },
            .zst => switch (right) {
                .zst => self.program.types.set(b, .{ .link = a }),
                else => Common.invariant("zero-sized type failed Lambda Solved unification"),
            },
            .erased => |left_erased| switch (right) {
                .erased => |right_erased| {
                    if (!std.mem.eql(u8, left_erased.source_fn_ty.bytes[0..], right_erased.source_fn_ty.bytes[0..])) {
                        Common.invariant("erased callable source function types failed Lambda Solved unification");
                    }
                    self.program.types.set(a, left);
                    self.program.types.set(b, .{ .link = a });
                },
                .lambda_set => {
                    self.program.types.set(a, left);
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("erased callable type failed Lambda Solved unification"),
            },
            .lambda_set => |left_members| switch (right) {
                .erased => {
                    self.program.types.set(a, right);
                    self.program.types.set(b, .{ .link = a });
                },
                .lambda_set => |right_members| {
                    const merged = try self.mergeLambdaSets(left_members, right_members);
                    self.program.types.set(a, .{ .lambda_set = merged });
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("lambda set failed Lambda Solved unification"),
            },
            .func => |left_fn| switch (right) {
                .func => |right_fn| {
                    try self.unifySpans(left_fn.args, right_fn.args, "function argument lists failed Lambda Solved unification");
                    try self.unify(left_fn.callable, right_fn.callable);
                    try self.unify(left_fn.ret, right_fn.ret);
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("function type failed Lambda Solved unification"),
            },
            .list => |left_elem| switch (right) {
                .list => |right_elem| {
                    try self.unify(left_elem, right_elem);
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("list type failed Lambda Solved unification"),
            },
            .box => |left_elem| switch (right) {
                .box => |right_elem| {
                    try self.unify(left_elem, right_elem);
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("box type failed Lambda Solved unification"),
            },
            .tuple => |left_items| switch (right) {
                .tuple => |right_items| {
                    try self.unifySpans(left_items, right_items, "tuple item lists failed Lambda Solved unification");
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("tuple type failed Lambda Solved unification"),
            },
            .record => |left_fields| switch (right) {
                .record => |right_fields| {
                    try self.unifyFields(left_fields, right_fields);
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("record type failed Lambda Solved unification"),
            },
            .tag_union => |left_tags| switch (right) {
                .tag_union => |right_tags| {
                    try self.unifyTags(left_tags, right_tags);
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("tag-union type failed Lambda Solved unification"),
            },
            .named => |left_named| switch (right) {
                .named => |right_named| {
                    if (!std.meta.eql(left_named.named_type, right_named.named_type) or
                        !std.meta.eql(left_named.def, right_named.def) or
                        left_named.kind != right_named.kind)
                    {
                        Common.invariant("named type identity failed Lambda Solved unification");
                    }
                    try self.unifySpans(left_named.args, right_named.args, "named type arguments failed Lambda Solved unification");
                    if (left_named.backing) |left_backing| {
                        const right_backing = right_named.backing orelse Common.invariant("named type backing differed during Lambda Solved unification");
                        if (left_backing.use != right_backing.use) Common.invariant("named type backing use differed during Lambda Solved unification");
                        try self.unify(left_backing.ty, right_backing.ty);
                    } else if (right_named.backing != null) {
                        Common.invariant("named type backing differed during Lambda Solved unification");
                    }
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("named type failed Lambda Solved unification"),
            },
            .link, .unbound, .forall => unreachable,
        }
    }

    fn unifySpans(self: *Solver, lhs: Type.Span, rhs: Type.Span, comptime message: []const u8) Allocator.Error!void {
        const left = self.program.types.span(lhs);
        const right = self.program.types.span(rhs);
        if (left.len != right.len) Common.invariant(message);
        for (left, 0..) |left_ty, i| try self.unify(left_ty, right[i]);
    }

    fn unifyFields(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        const left = self.program.types.fieldSpan(lhs);
        const right = self.program.types.fieldSpan(rhs);
        if (left.len != right.len) Common.invariant("record field count failed Lambda Solved unification");
        for (left, 0..) |left_field, i| {
            const right_field = right[i];
            if (left_field.name != right_field.name) Common.invariant("record field order failed Lambda Solved unification");
            try self.unify(left_field.ty, right_field.ty);
        }
    }

    fn unifyTags(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        const left = self.program.types.tagSpan(lhs);
        const right = self.program.types.tagSpan(rhs);
        if (left.len != right.len) Common.invariant("tag count failed Lambda Solved unification");
        for (left, 0..) |left_tag, i| {
            const right_tag = right[i];
            if (left_tag.name != right_tag.name) Common.invariant("tag order failed Lambda Solved unification");
            try self.unifySpans(left_tag.payloads, right_tag.payloads, "tag payload count failed Lambda Solved unification");
        }
    }

    fn mergeLambdaSets(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!Type.Span {
        var members = std.ArrayList(Type.FnMember).empty;
        defer members.deinit(self.allocator);

        for (self.program.types.memberSpan(lhs)) |member| try members.append(self.allocator, member);

        for (self.program.types.memberSpan(rhs)) |right_member| {
            var found = false;
            for (members.items) |left_member| {
                if (left_member.lambda != right_member.lambda) continue;
                found = true;
                try self.unifyCaptures(left_member.captures, right_member.captures);
                break;
            }
            if (!found) try members.append(self.allocator, right_member);
        }

        return try self.program.types.addMembers(members.items);
    }

    fn unifyCaptures(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        const left = self.program.types.captureSpan(lhs);
        const right = self.program.types.captureSpan(rhs);
        if (left.len != right.len) Common.invariant("capture count failed Lambda Solved unification");
        for (left, 0..) |left_capture, i| {
            const right_capture = right[i];
            if (left_capture.local != right_capture.local or
                left_capture.symbol != right_capture.symbol or
                left_capture.binder != right_capture.binder)
            {
                Common.invariant("capture identity failed Lambda Solved unification");
            }
            try self.unify(left_capture.ty, right_capture.ty);
        }
    }
};

const TypeCloner = struct {
    solver: *Solver,
    map: std.AutoHashMap(MonoType.TypeId, Type.TypeVarId),

    fn init(solver: *Solver) TypeCloner {
        return .{
            .solver = solver,
            .map = std.AutoHashMap(MonoType.TypeId, Type.TypeVarId).init(solver.allocator),
        };
    }

    fn deinit(self: *TypeCloner) void {
        self.map.deinit();
    }

    fn lower(self: *TypeCloner, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        if (self.map.get(ty)) |cached| return cached;
        const reserved = try self.solver.program.types.add(.unbound);
        try self.map.put(ty, reserved);
        self.solver.program.types.set(reserved, try self.lowerContent(self.solver.program.lifted.types.get(ty)));
        return reserved;
    }

    fn lowerContent(self: *TypeCloner, content: MonoType.Content) Allocator.Error!Type.Content {
        return switch (content) {
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |source_fn_ty| .{ .erased = .{ .source_fn_ty = source_fn_ty } },
            .list => |elem| .{ .list = try self.lower(elem) },
            .box => |elem| .{ .box = try self.lower(elem) },
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSpan(self.solver.program.lifted.types.span(items));
                defer self.solver.allocator.free(lowered);
                break :blk .{ .tuple = try self.solver.program.types.addSpan(lowered) };
            },
            .record => |fields| blk: {
                const lowered = try self.solver.allocator.alloc(Type.Field, fields.len);
                defer self.solver.allocator.free(lowered);
                for (self.solver.program.lifted.types.fieldSpan(fields), 0..) |field, i| {
                    lowered[i] = .{
                        .name = field.name,
                        .ty = try self.lower(field.ty),
                    };
                }
                break :blk .{ .record = try self.solver.program.types.addFields(lowered) };
            },
            .tag_union => |tags| blk: {
                const lowered = try self.solver.allocator.alloc(Type.Tag, tags.len);
                defer self.solver.allocator.free(lowered);
                for (self.solver.program.lifted.types.tagSpan(tags), 0..) |tag, i| {
                    const payloads = try self.lowerTypeSpan(self.solver.program.lifted.types.span(tag.payloads));
                    defer self.solver.allocator.free(payloads);
                    lowered[i] = .{
                        .name = tag.name,
                        .payloads = try self.solver.program.types.addSpan(payloads),
                    };
                }
                break :blk .{ .tag_union = try self.solver.program.types.addTags(lowered) };
            },
            .named => |named| blk: {
                const args = try self.lowerTypeSpan(self.solver.program.lifted.types.span(named.args));
                defer self.solver.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .args = try self.solver.program.types.addSpan(args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.lower(backing.ty),
                        .use = backing.use,
                    } else null,
                } };
            },
            .fn => |fn_ty| blk: {
                const args = try self.lowerTypeSpan(self.solver.program.lifted.types.span(fn_ty.args));
                defer self.solver.allocator.free(args);
                break :blk .{ .func = .{
                    .args = try self.solver.program.types.addSpan(args),
                    .callable = try self.solver.program.types.add(.unbound),
                    .ret = try self.lower(fn_ty.ret),
                } };
            },
        };
    }

    fn lowerTypeSpan(self: *TypeCloner, items: []const MonoType.TypeId) Allocator.Error![]Type.TypeVarId {
        const lowered = try self.solver.allocator.alloc(Type.TypeVarId, items.len);
        errdefer self.solver.allocator.free(lowered);
        for (items, 0..) |item, i| lowered[i] = try self.lower(item);
        return lowered;
    }
};

test "lambda solved solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
