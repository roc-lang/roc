//! Lambda solving over lifted Monotype IR.

const std = @import("std");
const can = @import("can");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const static_dispatch = check.StaticDispatchRegistry;

const UnifyPair = struct {
    first: Type.TypeVarId,
    second: Type.TypeVarId,

    fn init(lhs: Type.TypeVarId, rhs: Type.TypeVarId) UnifyPair {
        return if (lhs.is_gt(rhs))
            .{ .first = rhs, .second = lhs }
        else
            .{ .first = lhs, .second = rhs };
    }
};

/// Solve lambda-set relationships in a lifted Monotype program.
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
    lifted: Lifted.ProgramView,
    local_tys: []?Type.TypeVarId,
    expr_tys: []?Type.TypeVarId,
    pat_tys: []?Type.TypeVarId,
    expr_done: []bool,
    generated_backing_pats: []bool,
    loop_results: std.ArrayList(Type.TypeVarId),
    loop_params: std.ArrayList(Type.Span),
    return_contexts: std.ArrayList(ReturnContext),
    active_unifications: std.AutoHashMap(UnifyPair, void),

    const FunctionShape = struct {
        args: Type.Span,
        callable: Type.TypeVarId,
        ret: Type.TypeVarId,
    };

    const ReturnContext = struct {
        mono_ret: MonoType.TypeId,
        solved_ret: Type.TypeVarId,
    };

    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Solver {
        const lifted = program.lifted.view();

        const local_tys = try allocator.alloc(?Type.TypeVarId, lifted.locals.len);
        errdefer allocator.free(local_tys);
        @memset(local_tys, null);

        const expr_tys = try allocator.alloc(?Type.TypeVarId, lifted.exprs.len);
        errdefer allocator.free(expr_tys);
        @memset(expr_tys, null);

        const expr_done = try allocator.alloc(bool, lifted.exprs.len);
        errdefer allocator.free(expr_done);
        @memset(expr_done, false);

        const pat_tys = try allocator.alloc(?Type.TypeVarId, lifted.pats.len);
        errdefer allocator.free(pat_tys);
        @memset(pat_tys, null);

        const generated_backing_pats = try allocator.alloc(bool, lifted.pats.len);
        errdefer allocator.free(generated_backing_pats);
        @memset(generated_backing_pats, false);

        return .{
            .allocator = allocator,
            .program = program,
            .lifted = lifted,
            .local_tys = local_tys,
            .expr_tys = expr_tys,
            .pat_tys = pat_tys,
            .expr_done = expr_done,
            .generated_backing_pats = generated_backing_pats,
            .loop_results = .empty,
            .loop_params = .empty,
            .return_contexts = .empty,
            .active_unifications = std.AutoHashMap(UnifyPair, void).init(allocator),
        };
    }

    fn deinit(self: *Solver) void {
        self.active_unifications.deinit();
        self.return_contexts.deinit(self.allocator);
        self.loop_params.deinit(self.allocator);
        self.loop_results.deinit(self.allocator);
        self.allocator.free(self.generated_backing_pats);
        self.allocator.free(self.expr_done);
        self.allocator.free(self.pat_tys);
        self.allocator.free(self.expr_tys);
        self.allocator.free(self.local_tys);
    }

    fn solve(self: *Solver) Allocator.Error!void {
        for (self.lifted.locals, 0..) |local, index| {
            self.local_tys[index] = try self.lowerTypeFresh(local.ty);
        }

        try self.program.fn_tys.ensureTotalCapacity(self.allocator, self.lifted.fns.len);
        try self.program.defs.ensureTotalCapacity(self.allocator, self.lifted.fns.len);

        for (self.lifted.fns) |fn_| {
            const fn_ty = try self.functionType(fn_);
            try self.program.fn_tys.append(self.allocator, fn_ty);
            try self.program.defs.append(self.allocator, .{
                .symbol = fn_.symbol,
                .ty = fn_ty,
                .body = switch (fn_.body) {
                    .roc => |body| .{ .roc = body },
                    .hosted => .hosted,
                },
            });
        }

        for (self.lifted.fns, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.solveFn(fn_id, fn_);
        }

        try self.program.layout_requests.ensureTotalCapacity(self.allocator, self.lifted.layout_requests.len);
        for (self.lifted.layout_requests) |request| {
            const ty = if (request.fn_id) |fn_id|
                self.fnRetType(fn_id)
            else
                try self.lowerTypeFresh(request.ty);
            try self.markErasedCallablesReachedByType(ty);
            try self.program.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = ty,
                .fn_id = request.fn_id,
            });
        }

        try self.program.runtime_schema_requests.ensureTotalCapacity(self.allocator, self.lifted.runtime_schema_requests.len);
        for (self.lifted.runtime_schema_requests) |request| {
            const ty = try self.lowerTypeFresh(request.ty);
            try self.markErasedCallablesReachedByType(ty);
            try self.program.runtime_schema_requests.append(self.allocator, .{
                .def = request.def,
                .ty = ty,
            });
        }

        try self.closeUnfilledCallableSlots();

        try self.program.expr_tys.ensureTotalCapacity(self.allocator, self.expr_tys.len);
        for (self.expr_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.lowerTypeFresh(self.lifted.exprs[index].ty);
            try self.program.expr_tys.append(self.allocator, self.program.types.root(ty));
        }

        try self.program.pat_tys.ensureTotalCapacity(self.allocator, self.pat_tys.len);
        for (self.pat_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.lowerTypeFresh(self.lifted.pats[index].ty);
            try self.program.pat_tys.append(self.allocator, self.program.types.root(ty));
        }

        try self.program.local_tys.ensureTotalCapacity(self.allocator, self.local_tys.len);
        for (self.local_tys) |maybe_ty| {
            const ty = maybe_ty orelse Common.invariant("Lambda Solved local type slot was not initialized");
            try self.program.local_tys.append(self.allocator, self.program.types.root(ty));
        }

        for (self.program.layout_requests.items) |*request| {
            request.ty = self.program.types.root(request.ty);
        }
        for (self.program.runtime_schema_requests.items) |*request| {
            request.ty = self.program.types.root(request.ty);
        }
    }

    fn functionType(self: *Solver, fn_: Lifted.Fn) Allocator.Error!Type.TypeVarId {
        const arg_locals = self.lifted.typedLocalSpan(fn_.args);
        const args = try self.allocator.alloc(Type.TypeVarId, arg_locals.len);
        defer self.allocator.free(args);
        for (arg_locals, 0..) |arg, i| {
            const local = self.lifted.locals[@intFromEnum(arg.local)];
            if (@import("builtin").mode == .Debug and local.ty != arg.ty) {
                Common.invariant("Lambda Solved function argument type differed from its local type");
            }
            args[i] = self.localTy(arg.local);
        }

        const capture_locals = self.lifted.typedLocalSpan(fn_.captures);
        const captures = try self.allocator.alloc(Type.Capture, capture_locals.len);
        defer self.allocator.free(captures);
        for (capture_locals, 0..) |capture, i| {
            const local = self.lifted.locals[@intFromEnum(capture.local)];
            captures[i] = .{
                .local = capture.local,
                .symbol = local.symbol,
                .binder = local.binder,
                .capture_id = local.capture_id,
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

    fn fnRetType(self: *Solver, fn_id: Lifted.FnId) Type.TypeVarId {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.program.fn_tys.items.len) Common.invariant("Lambda Solved layout request referenced a missing function");
        const fn_ty = self.program.types.rootContent(self.program.fn_tys.items[raw]);
        return switch (fn_ty) {
            .func => |func| func.ret,
            else => Common.invariant("Lambda Solved layout request referenced a non-function"),
        };
    }

    fn solveFn(self: *Solver, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!void {
        const fn_ty = self.program.fn_tys.items[@intFromEnum(fn_id)];
        const fn_content = self.program.types.rootContent(fn_ty);
        const func = switch (fn_content) {
            .func => |func| func,
            else => Common.invariant("Lambda Solved function table contains a non-function type"),
        };

        const arg_locals = self.lifted.typedLocalSpan(fn_.args);
        if (func.args.count() != arg_locals.len) Common.invariant("Lambda Solved function arity changed after registration");
        for (arg_locals, 0..) |arg, i| {
            try self.unify(self.program.types.spanItem(func.args, i), self.localTy(arg.local));
        }

        try self.return_contexts.append(self.allocator, .{
            .mono_ret = fn_.ret,
            .solved_ret = func.ret,
        });
        defer _ = self.return_contexts.pop();

        switch (fn_.body) {
            .roc => |body| {
                _ = try self.expectExpr(body, func.ret);
            },
            .hosted => {},
        }
    }

    fn closeUnfilledCallableSlots(self: *Solver) Allocator.Error!void {
        const count = self.program.types.vars.items.len;
        const done = try self.allocator.alloc(bool, count);
        defer self.allocator.free(done);
        @memset(done, false);

        const active = try self.allocator.alloc(bool, count);
        defer self.allocator.free(active);
        @memset(active, false);

        for (0..count) |index| {
            try self.closeCallableSlotsInType(@enumFromInt(@as(u32, @intCast(index))), done, active);
        }
    }

    fn closeCallableSlotsInType(
        self: *Solver,
        ty: Type.TypeVarId,
        done: []bool,
        active: []bool,
    ) Allocator.Error!void {
        const root = self.program.types.root(ty);
        const index = @intFromEnum(root);
        if (done[index] or active[index]) return;

        active[index] = true;
        defer {
            active[index] = false;
            done[index] = true;
        }

        switch (self.program.types.get(root)) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound,
            .forall,
            .primitive,
            .zst,
            => {},
            .func => |func| {
                try self.closeCallableSlot(func.callable, done, active);
                for (self.program.types.span(func.args)) |arg| {
                    try self.closeCallableSlotsInType(arg, done, active);
                }
                try self.closeCallableSlotsInType(func.ret, done, active);
            },
            .list => |elem| try self.closeCallableSlotsInType(elem, done, active),
            .box => |elem| try self.closeCallableSlotsInType(elem, done, active),
            .tuple => |items| {
                for (self.program.types.span(items)) |item| {
                    try self.closeCallableSlotsInType(item, done, active);
                }
            },
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    try self.closeCallableSlotsInType(field.ty, done, active);
                }
            },
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    for (self.program.types.span(tag.payloads)) |payload| {
                        try self.closeCallableSlotsInType(payload, done, active);
                    }
                }
            },
            .named => |named| {
                for (self.program.types.span(named.args)) |arg| {
                    try self.closeCallableSlotsInType(arg, done, active);
                }
                if (named.backing) |backing| {
                    try self.closeCallableSlotsInType(backing.ty, done, active);
                }
            },
            .lambda_set => |members| try self.closeCallableSlotsInMembers(members, done, active),
            .erased => |erased| try self.closeCallableSlotsInMembers(erased.members, done, active),
        }
    }

    fn closeCallableSlot(
        self: *Solver,
        callable: Type.TypeVarId,
        done: []bool,
        active: []bool,
    ) Allocator.Error!void {
        const root = self.program.types.root(callable);
        switch (self.program.types.get(root)) {
            .unbound => self.program.types.set(root, .{ .lambda_set = .empty() }),
            .lambda_set,
            .erased,
            => try self.closeCallableSlotsInType(root, done, active),
            else => Common.invariant("function callable slot resolved to a non-callable type"),
        }
    }

    fn closeCallableSlotsInMembers(
        self: *Solver,
        members: Type.Span,
        done: []bool,
        active: []bool,
    ) Allocator.Error!void {
        for (self.program.types.memberSpan(members)) |member| {
            for (self.program.types.captureSpan(member.captures)) |capture| {
                try self.closeCallableSlotsInType(capture.ty, done, active);
            }
        }
    }

    fn inferExpr(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        const expected = try self.exprSlot(expr_id);
        if (self.expr_done[index]) return expected;
        self.expr_done[index] = true;

        const expr = self.lifted.exprs[index];
        switch (expr.data) {
            .local => |local| try self.unify(expected, self.localTy(local)),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .comptime_exhaustiveness_failed,
            => {},
            .list => |items| {
                const elem_ty = try self.listElem(expected);
                for (self.lifted.exprSpan(items)) |child| {
                    _ = try self.expectExpr(child, elem_ty);
                }
            },
            .tuple => |items| {
                const item_tys = try self.tupleItemsSpan(expected);
                const children = self.lifted.exprSpan(items);
                if (item_tys.count() != children.len) Common.invariant("tuple expression arity differs from its checked type");
                for (children, 0..) |child, i| {
                    const item_ty = self.program.types.spanItem(item_tys, i);
                    _ = try self.expectExpr(child, item_ty);
                }
            },
            .record => |fields| {
                for (self.lifted.fieldExprSpan(fields)) |field| {
                    _ = try self.expectExpr(field.value, try self.recordField(expected, field.name));
                }
            },
            .tag => |tag| {
                const payload_tys = try self.tagPayloadsSpan(expected, tag.name);
                const payloads = self.lifted.exprSpan(tag.payloads);
                if (payload_tys.count() != payloads.len) Common.invariant("tag expression payload arity differs from its checked type");
                for (payloads, 0..) |payload, i| {
                    const expected_payload_ty = self.program.types.spanItem(payload_tys, i);
                    _ = try self.expectExpr(payload, expected_payload_ty);
                }
            },
            .nominal => |backing| {
                if (try self.namedBacking(expected)) |backing_ty| {
                    if (self.hasBuiltinOwner(expected, .fields) or self.hasBuiltinOwner(expected, .field)) {
                        try self.inferGeneratedOpaqueBacking(backing);
                    } else {
                        _ = try self.expectExpr(backing, backing_ty);
                    }
                } else {
                    _ = try self.inferExpr(backing);
                }
            },
            .let_ => |let_| {
                const value_ty = try self.inferExpr(let_.value);
                try self.bindPattern(let_.bind, value_ty);
                _ = try self.expectExpr(let_.rest, expected);
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached Lambda Solved"),
            .fn_ref => |fn_ref| {
                try self.unify(expected, self.program.fn_tys.items[@intFromEnum(fn_ref.fn_id)]);
                const captures = self.liftedCapturesForFn(fn_ref.fn_id);
                const capture_operands = self.lifted.captureOperandSpan(fn_ref.captures);
                if (captures.len != capture_operands.len) {
                    Common.invariant("function reference capture count differs from its target");
                }
                for (captures, capture_operands) |capture, operand| {
                    if (operand.id != self.lifted.captureIdOfLocal(capture.local)) {
                        Common.invariant("function reference capture operand CaptureId did not match its slot");
                    }
                    _ = try self.expectExpr(operand.value, self.localTy(capture.local));
                }
            },
            .call_value => |call| {
                const func = try self.functionShape(try self.inferExpr(call.callee));
                const args = self.lifted.exprSpan(call.args);
                if (func.args.count() != args.len) Common.invariant("value call arity differs from its checked type");
                try self.unify(expected, func.ret);
                for (args, 0..) |arg, i| {
                    _ = try self.expectExpr(arg, self.program.types.spanItem(func.args, i));
                }
            },
            .call_proc => |call| {
                const args = self.lifted.exprSpan(call.args);
                switch (Lifted.directCallee(call)) {
                    .local => |callee| {
                        const func = try self.functionShape(self.program.fn_tys.items[@intFromEnum(callee)]);
                        if (func.args.count() != args.len) Common.invariant("procedure call arity differs from its checked type");
                        try self.unify(expected, func.ret);
                        for (args, 0..) |arg, i| {
                            _ = try self.expectExpr(arg, self.program.types.spanItem(func.args, i));
                        }
                        const captures = self.liftedCapturesForFn(callee);
                        const capture_operands = self.lifted.captureOperandSpan(call.captures);
                        if (captures.len != capture_operands.len) Common.invariant("procedure call capture count differs from its callee");
                        for (captures, capture_operands) |capture, operand| {
                            if (operand.id != self.lifted.captureIdOfLocal(capture.local)) {
                                Common.invariant("procedure call capture operand CaptureId did not match its slot");
                            }
                            _ = try self.expectExpr(operand.value, self.localTy(capture.local));
                        }
                    },
                    .imported => {
                        for (args) |arg| {
                            _ = try self.inferExpr(arg);
                        }
                        if (call.captures.len != 0) Common.invariant("imported direct call carried local capture operands");
                    },
                }
            },
            .low_level => |call| {
                const args = self.lifted.exprSpan(call.args);
                const arg_tys = try self.allocator.alloc(Type.TypeVarId, args.len);
                defer self.allocator.free(arg_tys);
                for (args, 0..) |arg, i| {
                    arg_tys[i] = try self.inferExpr(arg);
                }
                try self.bindLowLevelTypes(call.op, expected, arg_tys);
            },
            .field_access => |field| {
                const receiver_ty = try self.inferExpr(field.receiver);
                try self.unify(expected, try self.recordField(receiver_ty, field.field));
            },
            .tuple_access => |access| {
                const receiver_ty = try self.inferExpr(access.tuple);
                const items = try self.tupleItemsSpan(receiver_ty);
                if (access.elem_index >= items.count()) Common.invariant("tuple access index exceeds tuple arity");
                try self.unify(expected, self.program.types.spanItem(items, access.elem_index));
            },
            .structural_eq => |eq| {
                const lhs = try self.inferExpr(eq.lhs);
                const rhs = try self.inferExpr(eq.rhs);
                try self.unify(lhs, rhs);
            },
            .structural_hash => |h| {
                _ = try self.inferExpr(h.value);
                const hasher = try self.inferExpr(h.hasher);
                // `to_hash` threads the Hasher through, so the result type equals
                // the Hasher argument's type.
                try self.unify(expected, hasher);
            },
            .match_ => |match| {
                const scrutinee_ty = try self.inferExpr(match.scrutinee);
                for (self.lifted.branchSpan(match.branches)) |branch| {
                    try self.bindPattern(branch.pat, scrutinee_ty);
                    if (branch.guard) |guard| _ = try self.inferExpr(guard);
                    _ = try self.expectExpr(branch.body, expected);
                }
            },
            .if_ => |if_| {
                for (self.lifted.ifBranchSpan(if_.branches)) |branch| {
                    _ = try self.inferExpr(branch.cond);
                    _ = try self.expectExpr(branch.body, expected);
                }
                _ = try self.expectExpr(if_.final_else, expected);
            },
            .if_initialized_payload => |payload_switch| {
                _ = try self.inferExpr(payload_switch.cond);
                _ = self.localTy(payload_switch.payload);
                _ = try self.expectExpr(payload_switch.initialized, expected);
                _ = try self.expectExpr(payload_switch.uninitialized, expected);
            },
            .try_sequence => |sequence| {
                const try_ty = try self.inferExpr(sequence.try_expr);
                const tags = switch (try self.shapeContent(try_ty)) {
                    .tag_union => |span| self.program.types.tagSpan(span),
                    else => Common.invariant("try_sequence input was not a Try tag union"),
                };
                var ok_ty: ?Type.TypeVarId = null;
                for (tags) |tag| {
                    if (!std.mem.eql(u8, self.lifted.names.tagLabelText(tag.name), "Ok")) continue;
                    const payloads = self.program.types.span(tag.payloads);
                    if (payloads.len != 1) Common.invariant("try_sequence Ok tag had unexpected payload arity");
                    ok_ty = payloads[0];
                    break;
                }
                try self.unify(self.localTy(sequence.ok_local), ok_ty orelse Common.invariant("try_sequence input had no Ok tag"));
                _ = try self.expectExpr(sequence.ok_body, expected);
            },
            .try_record_sequence => |sequence| {
                const try_ty = try self.inferExpr(sequence.try_expr);
                const tags = switch (try self.shapeContent(try_ty)) {
                    .tag_union => |span| self.program.types.tagSpan(span),
                    else => Common.invariant("try_record_sequence input was not a Try tag union"),
                };
                var ok_ty: ?Type.TypeVarId = null;
                for (tags) |tag| {
                    if (!std.mem.eql(u8, self.lifted.names.tagLabelText(tag.name), "Ok")) continue;
                    const payloads = self.program.types.span(tag.payloads);
                    if (payloads.len != 1) Common.invariant("try_record_sequence Ok tag had unexpected payload arity");
                    ok_ty = payloads[0];
                    break;
                }
                const ok_record_ty = ok_ty orelse Common.invariant("try_record_sequence input had no Ok tag");
                try self.unify(self.localTy(sequence.value_local), try self.recordField(ok_record_ty, sequence.value_field));
                try self.unify(self.localTy(sequence.rest_local), try self.recordField(ok_record_ty, sequence.rest_field));
                _ = try self.expectExpr(sequence.ok_body, expected);
            },
            .block => |block| {
                for (self.lifted.stmtSpan(block.statements)) |stmt| try self.inferStmt(stmt);
                _ = try self.expectExpr(block.final_expr, expected);
            },
            .loop_ => |loop| {
                const params = self.lifted.typedLocalSpan(loop.params);
                const initials = self.lifted.exprSpan(loop.initial_values);
                if (params.len != initials.len) Common.invariant("loop parameter count differs from initial value count");
                const param_tys = try self.allocator.alloc(Type.TypeVarId, params.len);
                defer self.allocator.free(param_tys);
                for (params, 0..) |param, i| {
                    param_tys[i] = self.localTy(param.local);
                    _ = try self.expectExpr(initials[i], param_tys[i]);
                }
                try self.loop_results.append(self.allocator, expected);
                try self.loop_params.append(self.allocator, try self.program.types.addSpan(param_tys));
                defer _ = self.loop_params.pop();
                defer _ = self.loop_results.pop();
                _ = try self.expectExpr(loop.body, expected);
            },
            .break_ => |maybe| {
                if (maybe) |value| {
                    _ = try self.expectExpr(value, self.currentLoopResult());
                }
            },
            .continue_ => |continue_| {
                const params = self.currentLoopParams();
                const values = self.lifted.exprSpan(continue_.values);
                if (params.count() != values.len) Common.invariant("continue value count differs from loop parameter count");
                for (values, 0..) |value, i| {
                    const param_ty = self.program.types.spanItem(params, i);
                    _ = try self.expectExpr(value, param_ty);
                }
            },
            .return_ => |ret| _ = try self.expectExpr(ret.value, try self.returnTargetTy(ret.target)),
            .dbg,
            .expect,
            => |child| _ = try self.inferExpr(child),
            .expect_err => |expect_err| _ = try self.inferExpr(expect_err.msg),
            .comptime_branch_taken => |taken| _ = try self.expectExpr(taken.body, expected),
        }
        return self.program.types.root(expected);
    }

    fn inferStmt(self: *Solver, stmt_id: Lifted.StmtId) Allocator.Error!void {
        switch (self.lifted.stmts[@intFromEnum(stmt_id)]) {
            .uninitialized => |pat| {
                const pat_ty = try self.lowerTypeFresh(self.lifted.pats[@intFromEnum(pat)].ty);
                try self.bindPattern(pat, pat_ty);
            },
            .let_ => |let_| {
                const value_ty = try self.inferExpr(let_.value);
                try self.bindPattern(let_.pat, value_ty);
            },
            .expr,
            .expect,
            .dbg,
            => |expr| _ = try self.inferExpr(expr),
            .return_ => |ret| _ = try self.expectExpr(ret.value, try self.returnTargetTy(ret.target)),
            .crash => {},
        }
    }

    fn inferGeneratedOpaqueBacking(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (self.expr_done[index]) return;

        const expr = self.lifted.exprs[index];
        switch (expr.data) {
            .record => |fields| {
                _ = try self.exprSlot(expr_id);
                self.expr_done[index] = true;
                for (self.lifted.fieldExprSpan(fields)) |field| {
                    _ = try self.inferExpr(field.value);
                }
            },
            .tuple => |items| {
                _ = try self.exprSlot(expr_id);
                self.expr_done[index] = true;
                for (self.lifted.exprSpan(items)) |item| {
                    _ = try self.inferExpr(item);
                }
            },
            .tag => |tag| {
                _ = try self.exprSlot(expr_id);
                self.expr_done[index] = true;
                for (self.lifted.exprSpan(tag.payloads)) |payload| {
                    _ = try self.inferExpr(payload);
                }
            },
            .nominal => |backing| {
                _ = try self.exprSlot(expr_id);
                self.expr_done[index] = true;
                try self.inferGeneratedOpaqueBacking(backing);
            },
            .let_ => |let_| {
                _ = try self.exprSlot(expr_id);
                self.expr_done[index] = true;
                const value_ty = try self.inferExpr(let_.value);
                try self.bindPattern(let_.bind, value_ty);
                try self.inferGeneratedOpaqueBacking(let_.rest);
            },
            else => _ = try self.inferExpr(expr_id),
        }
    }

    fn bindPattern(self: *Solver, pat_id: Lifted.PatId, value_ty: Type.TypeVarId) Allocator.Error!void {
        const index = @intFromEnum(pat_id);
        if (self.generated_backing_pats[index]) {
            const pat_ty = self.pat_tys[index] orelse Common.invariant("generated backing pattern was marked before its type was assigned");
            try self.unifyGeneratedOpaqueBacking(pat_ty, value_ty);
            return;
        }
        const pat_ty = try self.expectPat(pat_id, value_ty);
        try self.bindPatternAtType(pat_id, pat_ty);
    }

    fn bindPatternAtType(self: *Solver, pat_id: Lifted.PatId, pat_ty: Type.TypeVarId) Allocator.Error!void {
        const pat = self.lifted.pats[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bind => |local| try self.unify(self.localTy(local), pat_ty),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .str_pattern => |str| {
                for (self.lifted.strPatternStepSpan(str.steps)) |step| {
                    if (step.capture) |capture| {
                        try self.bindPattern(capture, pat_ty);
                    }
                }
            },
            .as => |as| {
                try self.unify(self.localTy(as.local), pat_ty);
                try self.bindPattern(as.pattern, pat_ty);
            },
            .record => |fields| {
                for (self.lifted.recordDestructSpan(fields)) |field| {
                    try self.bindPattern(field.pattern, try self.recordField(pat_ty, field.name));
                }
            },
            .tuple => |items| {
                const item_tys = try self.tupleItemsSpan(pat_ty);
                const pats = self.lifted.patSpan(items);
                if (item_tys.count() != pats.len) Common.invariant("tuple pattern arity differs from its checked type");
                for (pats, 0..) |child, i| {
                    const item_ty = self.program.types.spanItem(item_tys, i);
                    try self.bindPattern(child, item_ty);
                }
            },
            .list => |list| {
                const elem_ty = try self.listElem(pat_ty);
                for (self.lifted.patSpan(list.patterns)) |child| {
                    try self.bindPattern(child, elem_ty);
                }
                // A captured rest is itself a list with the same element type.
                if (list.rest) |rest| if (rest.pattern) |rest_pattern| try self.bindPattern(rest_pattern, pat_ty);
            },
            .tag => |tag| {
                const payload_tys = try self.tagPayloadsSpan(pat_ty, tag.name);
                const payloads = self.lifted.patSpan(tag.payloads);
                if (payload_tys.count() != payloads.len) Common.invariant("tag pattern payload arity differs from its checked type");
                for (payloads, 0..) |child, i| {
                    const payload_ty = self.program.types.spanItem(payload_tys, i);
                    try self.bindPattern(child, payload_ty);
                }
            },
            .nominal => |backing| {
                if (self.hasGeneratedOpaquePatOwner(pat_id) or self.hasBuiltinOwner(pat_ty, .fields) or self.hasBuiltinOwner(pat_ty, .field)) {
                    try self.bindGeneratedOpaqueBackingPattern(backing);
                } else {
                    if (try self.namedBacking(pat_ty)) |backing_ty| {
                        try self.bindPattern(backing, backing_ty);
                    } else {
                        try self.bindPattern(backing, pat_ty);
                    }
                }
            },
        }
    }

    fn hasGeneratedOpaquePatOwner(self: *Solver, pat_id: Lifted.PatId) bool {
        return switch (self.lifted.types.get(self.lifted.pats[@intFromEnum(pat_id)].ty)) {
            .named => |named| isGeneratedOpaqueEvidenceOwner(named.builtin_owner),
            else => false,
        };
    }

    fn bindGeneratedOpaqueBackingPattern(self: *Solver, pat_id: Lifted.PatId) Allocator.Error!void {
        const index = @intFromEnum(pat_id);
        if (self.generated_backing_pats[index]) return;
        self.generated_backing_pats[index] = true;
        const pat_ty = try self.lowerTypeFresh(self.lifted.pats[@intFromEnum(pat_id)].ty);
        self.pat_tys[index] = pat_ty;
        try self.bindPatternAtType(pat_id, pat_ty);
    }

    fn unifyGeneratedOpaqueBacking(self: *Solver, generated_ty: Type.TypeVarId, expected_ty: Type.TypeVarId) Allocator.Error!void {
        const generated = self.program.types.root(generated_ty);
        const expected = self.program.types.root(expected_ty);
        if (generated == expected) return;

        const generated_score = generatedBackingScore(self.program.types.get(generated)) orelse {
            try self.unify(generated, expected);
            return;
        };
        const expected_score = generatedBackingScore(self.program.types.get(expected)) orelse {
            try self.unify(generated, expected);
            return;
        };

        if (generated_score > expected_score) {
            self.program.types.set(expected, .{ .link = generated });
        } else if (expected_score > generated_score) {
            self.program.types.set(generated, .{ .link = expected });
        } else {
            try self.unify(generated, expected);
        }
    }

    fn expectExpr(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const slot = try self.expectExprSlot(expr_id, expected);
        const inferred = try self.inferExpr(expr_id);
        try self.unify(slot, inferred);
        return self.program.types.root(slot);
    }

    fn exprSlot(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| return ty;

        const expr = self.lifted.exprs[index];
        const ty = switch (expr.data) {
            .local => |local| self.localTy(local),
            .fn_ref => |fn_ref| self.program.fn_tys.items[@intFromEnum(fn_ref.fn_id)],
            .call_proc => |call| switch (Lifted.directCallee(call)) {
                .local => |callee| (try self.functionShape(self.program.fn_tys.items[@intFromEnum(callee)])).ret,
                .imported => try self.lowerTypeFresh(expr.ty),
            },
            else => try self.lowerTypeFresh(expr.ty),
        };
        self.expr_tys[index] = ty;
        return ty;
    }

    fn expectExprSlot(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| {
            try self.unify(ty, expected);
            return self.program.types.root(ty);
        }

        const expr = self.lifted.exprs[index];
        const ty = switch (expr.data) {
            .local => |local| self.localTy(local),
            .fn_ref => |fn_ref| self.program.fn_tys.items[@intFromEnum(fn_ref.fn_id)],
            else => expected,
        };
        try self.unify(ty, expected);
        self.expr_tys[index] = ty;
        return self.program.types.root(ty);
    }

    fn expectPat(self: *Solver, pat_id: Lifted.PatId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(pat_id);
        if (self.pat_tys[index]) |ty| {
            try self.unify(ty, expected);
            return self.program.types.root(ty);
        }

        const pat = self.lifted.pats[index];
        const ty = switch (pat.data) {
            .bind => |local| self.localTy(local),
            .as => |as| self.localTy(as.local),
            else => expected,
        };
        try self.unify(ty, expected);
        self.pat_tys[index] = ty;
        return self.program.types.root(ty);
    }

    fn functionShape(self: *Solver, ty: Type.TypeVarId) Allocator.Error!FunctionShape {
        return switch (try self.shapeContent(ty)) {
            .func => |func| .{ .args = func.args, .callable = func.callable, .ret = func.ret },
            else => Common.invariant("call expression had a non-function checked type"),
        };
    }

    fn liftedCapturesForFn(self: *Solver, fn_id: Lifted.FnId) []const Lifted.TypedLocal {
        return self.lifted.typedLocalSpan(self.lifted.fns[@intFromEnum(fn_id)].captures);
    }

    fn localTy(self: *Solver, local: Lifted.LocalId) Type.TypeVarId {
        return self.local_tys[@intFromEnum(local)] orelse Common.invariant("Lambda Solved local reached solver without a type slot");
    }

    fn returnTargetTy(self: *Solver, target: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        if (self.return_contexts.items.len == 0) Common.invariant("return expression reached Lambda Solved outside a function");
        const context = self.return_contexts.items[self.return_contexts.items.len - 1];
        if (!try self.sameMonoType(target, context.mono_ret)) {
            Common.invariant("return target type differed from enclosing function return type");
        }
        return context.solved_ret;
    }

    fn sameMonoType(self: *Solver, a: MonoType.TypeId, b: MonoType.TypeId) Allocator.Error!bool {
        if (a == b) return true;
        return try self.lifted.types.typeEql(self.allocator, self.lifted.names, a, b);
    }

    fn currentLoopResult(self: *Solver) Type.TypeVarId {
        if (self.loop_results.items.len == 0) Common.invariant("break expression reached Lambda Solved outside a loop");
        return self.loop_results.items[self.loop_results.items.len - 1];
    }

    fn currentLoopParams(self: *Solver) Type.Span {
        if (self.loop_params.items.len == 0) Common.invariant("continue expression reached Lambda Solved outside a loop");
        return self.loop_params.items[self.loop_params.items.len - 1];
    }

    fn markErasedCallablesReachedByType(self: *Solver, ty: Type.TypeVarId) Allocator.Error!void {
        var active = std.AutoHashMap(Type.TypeVarId, void).init(self.allocator);
        defer active.deinit();
        try self.markErasedCallablesReachedByTypeInner(ty, &active);
    }

    fn markErasedCallablesReachedByTypeInner(
        self: *Solver,
        ty: Type.TypeVarId,
        active: *std.AutoHashMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        const root = self.program.types.root(ty);
        if (active.contains(root)) return;
        try active.put(root, {});
        defer _ = active.remove(root);

        switch (self.program.types.get(root)) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound, .forall, .primitive, .zst, .erased => {},
            .func => |func| {
                const erased = try self.program.types.add(.{ .erased = .{
                    .source_fn_ty = try self.solvedTypeDigest(root),
                    .members = .empty(),
                } });
                try self.unify(func.callable, erased);
                for (self.program.types.span(func.args)) |arg| {
                    try self.markErasedCallablesReachedByTypeInner(arg, active);
                }
                try self.markErasedCallablesReachedByTypeInner(func.ret, active);
            },
            .list => |elem| try self.markErasedCallablesReachedByTypeInner(elem, active),
            .box => |elem| try self.markErasedCallablesReachedByTypeInner(elem, active),
            .tuple => |items| {
                for (self.program.types.span(items)) |item| {
                    try self.markErasedCallablesReachedByTypeInner(item, active);
                }
            },
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    try self.markErasedCallablesReachedByTypeInner(field.ty, active);
                }
            },
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    for (self.program.types.span(tag.payloads)) |payload| {
                        try self.markErasedCallablesReachedByTypeInner(payload, active);
                    }
                }
            },
            .named => |named| {
                for (self.program.types.span(named.args)) |arg| {
                    try self.markErasedCallablesReachedByTypeInner(arg, active);
                }
                if (named.backing) |backing| {
                    try self.markErasedCallablesReachedByTypeInner(backing.ty, active);
                }
            },
            .lambda_set => |members| {
                for (self.program.types.memberSpan(members)) |member| {
                    for (self.program.types.captureSpan(member.captures)) |capture| {
                        try self.markErasedCallablesReachedByTypeInner(capture.ty, active);
                    }
                }
            },
        }
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

    fn tupleItemsSpan(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Span {
        return switch (try self.shapeContent(ty)) {
            .tuple => |items| items,
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

    fn recordFieldByLabel(self: *Solver, ty: Type.TypeVarId, label: []const u8) Allocator.Error!Type.TypeVarId {
        return switch (try self.shapeContent(ty)) {
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    if (std.mem.eql(u8, self.lifted.names.recordFieldLabelText(field.name), label)) return field.ty;
                }
                Common.invariant("low-level record result was missing a required field");
            },
            else => Common.invariant("low-level record result had a non-record checked type"),
        };
    }

    fn tagPayloadsSpan(self: *Solver, ty: Type.TypeVarId, name: Type.names.TagNameId) Allocator.Error!Type.Span {
        return switch (try self.shapeContent(ty)) {
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    if (tag.name == name) {
                        return tag.payloads;
                    }
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

    fn hasBuiltinOwner(self: *Solver, ty: Type.TypeVarId, owner: static_dispatch.BuiltinOwner) bool {
        return switch (self.program.types.rootContent(ty)) {
            .named => |named| if (named.builtin_owner) |builtin_owner| builtin_owner == owner else false,
            else => false,
        };
    }

    fn generatedOpaqueEvidenceScore(self: *Solver, named: anytype) u8 {
        if (!isGeneratedOpaqueEvidenceOwner(named.builtin_owner)) return 0;

        const backing = named.backing orelse return 0;
        return generatedBackingScore(self.program.types.rootContent(backing.ty)) orelse 2;
    }

    fn bindLowLevelTypes(
        self: *Solver,
        op: can.CIR.Expr.LowLevel,
        expected: Type.TypeVarId,
        args: []const Type.TypeVarId,
    ) Allocator.Error!void {
        switch (op) {
            .box_box => {
                expectLowLevelArity(op, args, 1);
                try self.unify(args[0], try self.boxElem(expected));
                try self.markErasedCallablesReachedByType(args[0]);
            },
            .box_unbox => {
                expectLowLevelArity(op, args, 1);
                try self.unify(expected, try self.boxElem(args[0]));
                try self.markErasedCallablesReachedByType(expected);
            },
            .list_get_unsafe => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, try self.listElem(args[0]));
            },
            .list_append_unsafe => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
                try self.unify(args[1], try self.listElem(expected));
            },
            .list_concat => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
                try self.unify(expected, args[1]);
            },
            .list_reserve,
            .list_drop_at,
            .list_sublist,
            .list_take_first,
            .list_take_last,
            .list_drop_first,
            .list_drop_last,
            => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
            },
            .list_release_excess_capacity,
            .list_reverse,
            => {
                expectLowLevelArity(op, args, 1);
                try self.unify(expected, args[0]);
            },
            .list_set => {
                expectLowLevelArity(op, args, 3);
                try self.unify(expected, args[0]);
                try self.unify(args[2], try self.listElem(expected));
            },
            .list_replace_unsafe => {
                expectLowLevelArity(op, args, 3);
                const elem = try self.listElem(args[0]);
                try self.unify(args[2], elem);
                try self.unify(try self.recordFieldByLabel(expected, "list"), args[0]);
                try self.unify(try self.recordFieldByLabel(expected, "prev"), elem);
            },
            .list_swap => {
                expectLowLevelArity(op, args, 3);
                try self.unify(expected, args[0]);
            },
            .list_prepend => {
                expectLowLevelArity(op, args, 2);
                try self.unify(expected, args[0]);
                try self.unify(args[1], try self.listElem(expected));
            },
            .dict_pseudo_seed => expectLowLevelArity(op, args, 0),
            .hasher_finish => expectLowLevelArity(op, args, 1),
            .crypto_sha256_hash_bytes,
            .crypto_sha256_hasher_finish,
            .crypto_blake3_hash_bytes,
            .crypto_blake3_hasher_finish,
            => expectLowLevelArity(op, args, 1),
            .crypto_sha256_hasher_empty,
            .crypto_blake3_hasher_empty,
            => expectLowLevelArity(op, args, 0),
            .crypto_sha256_hasher_write,
            .crypto_blake3_hasher_write,
            => expectLowLevelArity(op, args, 2),
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_u128,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            .hasher_write_i128,
            .hasher_write_f32,
            .hasher_write_f64,
            .hasher_write_dec,
            .hasher_write_bytes,
            .hasher_write_str,
            => expectLowLevelArity(op, args, 2),
            else => {},
        }
    }

    fn expectLowLevelArity(
        op: can.CIR.Expr.LowLevel,
        args: []const Type.TypeVarId,
        expected: usize,
    ) void {
        if (args.len == expected) return;

        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "postcheck invariant violated: low-level op {s} had {d} args, expected {d}",
                .{ @tagName(op), args.len, expected },
            );
        }
        unreachable;
    }

    fn boxElem(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        return switch (try self.shapeContent(ty)) {
            .box => |elem| elem,
            else => Common.invariant("box low-level operation had a non-box checked type"),
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

        const pair = UnifyPair.init(a, b);
        const active_entry = try self.active_unifications.getOrPut(pair);
        if (active_entry.found_existing) return;
        defer _ = self.active_unifications.remove(pair);

        if (transparentAliasBacking(left)) |backing| {
            try self.unify(backing, b);
            self.program.types.set(a, .{ .link = self.program.types.root(backing) });
            return;
        }
        if (transparentAliasBacking(right)) |backing| {
            try self.unify(a, backing);
            self.program.types.set(b, .{ .link = self.program.types.root(backing) });
            return;
        }

        switch (left) {
            .primitive => |left_primitive| switch (right) {
                .primitive => |right_primitive| {
                    if (left_primitive != right_primitive) {
                        Common.invariant("primitive types failed Lambda Solved unification");
                    }
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
                    self.program.types.set(a, .{ .erased = .{
                        .source_fn_ty = left_erased.source_fn_ty,
                        .members = try self.mergeLambdaSets(left_erased.members, right_erased.members),
                    } });
                    self.program.types.set(b, .{ .link = a });
                },
                .lambda_set => |right_members| {
                    self.program.types.set(a, .{ .erased = .{
                        .source_fn_ty = left_erased.source_fn_ty,
                        .members = try self.mergeLambdaSets(left_erased.members, right_members),
                    } });
                    self.program.types.set(b, .{ .link = a });
                },
                else => Common.invariant("erased callable type failed Lambda Solved unification"),
            },
            .lambda_set => |left_members| switch (right) {
                .erased => |right_erased| {
                    self.program.types.set(a, .{ .erased = .{
                        .source_fn_ty = right_erased.source_fn_ty,
                        .members = try self.mergeLambdaSets(left_members, right_erased.members),
                    } });
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
                    if (!std.meta.eql(left_named.def, right_named.def) or
                        left_named.kind != right_named.kind or
                        left_named.builtin_owner != right_named.builtin_owner)
                    {
                        Common.invariant("named type identity failed Lambda Solved unification");
                    }
                    try self.unifySpans(left_named.args, right_named.args, "named type arguments failed Lambda Solved unification");
                    // Aliases have already been unwrapped above. Generated
                    // opaque evidence uses its backing only to carry generated
                    // compile-time evidence rows, so two values with the same nominal
                    // identity may intentionally have different backing rows.
                    if (isGeneratedOpaqueEvidenceOwner(left_named.builtin_owner) or
                        isGeneratedOpaqueEvidenceOwner(right_named.builtin_owner))
                    {
                        if (self.generatedOpaqueEvidenceScore(right_named) > self.generatedOpaqueEvidenceScore(left_named)) {
                            self.program.types.set(a, .{ .link = b });
                        } else {
                            self.program.types.set(b, .{ .link = a });
                        }
                    } else {
                        if (left_named.backing) |left_backing| {
                            const right_backing = right_named.backing orelse Common.invariant("named type backing differed during Lambda Solved unification");
                            if (left_backing.use != right_backing.use) Common.invariant("named type backing use differed during Lambda Solved unification");
                            try self.unify(left_backing.ty, right_backing.ty);
                        } else if (right_named.backing != null) {
                            Common.invariant("named type backing differed during Lambda Solved unification");
                        }
                        self.program.types.set(b, .{ .link = a });
                    }
                },
                else => Common.invariant("named type failed Lambda Solved unification"),
            },
            .link, .unbound, .forall => unreachable,
        }
    }

    fn transparentAliasBacking(content: Type.Content) ?Type.TypeVarId {
        return switch (content) {
            .named => |named| if (named.kind == .alias)
                (named.backing orelse Common.invariant("transparent alias reached Lambda Solved without a backing type")).ty
            else
                null,
            else => null,
        };
    }

    fn unifySpans(self: *Solver, lhs: Type.Span, rhs: Type.Span, comptime message: []const u8) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant(message);
        for (0..lhs.count()) |i| {
            const left_ty = self.program.types.spanItem(lhs, i);
            const right_ty = self.program.types.spanItem(rhs, i);
            try self.unify(left_ty, right_ty);
        }
    }

    fn unifyFields(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant("record field count failed Lambda Solved unification");
        for (0..lhs.count()) |i| {
            const left_field = self.program.types.fieldItem(lhs, i);
            const right_field = self.program.types.fieldItem(rhs, i);
            if (left_field.name != right_field.name) Common.invariant("record field order failed Lambda Solved unification");
            try self.unify(left_field.ty, right_field.ty);
        }
    }

    fn unifyTags(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant("tag count failed Lambda Solved unification");
        for (0..lhs.count()) |i| {
            const left_tag = self.program.types.tagItem(lhs, i);
            const right_tag = self.program.types.tagItem(rhs, i);
            if (left_tag.name != right_tag.name) Common.invariant("tag order failed Lambda Solved unification");
            try self.unifySpans(left_tag.payloads, right_tag.payloads, "tag payload count failed Lambda Solved unification");
        }
    }

    fn mergeLambdaSets(self: *Solver, lhs: Type.Span, rhs: Type.Span) Allocator.Error!Type.Span {
        var members = std.ArrayList(Type.FnMember).empty;
        defer members.deinit(self.allocator);

        for (0..lhs.count()) |i| try members.append(self.allocator, self.program.types.memberItem(lhs, i));

        for (0..rhs.count()) |i| {
            const right_member = self.program.types.memberItem(rhs, i);
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
        if (lhs.count() != rhs.count()) Common.invariant("capture count failed Lambda Solved unification");
        for (0..lhs.count()) |i| {
            const left_capture = self.program.types.captureItem(lhs, i);
            const right_capture = self.program.types.captureItem(rhs, i);
            if (left_capture.capture_id != right_capture.capture_id) {
                Common.invariant("capture identity failed Lambda Solved unification");
            }
            try self.unify(left_capture.ty, right_capture.ty);
        }
    }

    fn solvedTypeDigest(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        var active = std.AutoHashMap(Type.TypeVarId, void).init(self.allocator);
        defer active.deinit();
        try self.writeSolvedTypeDigest(&hasher, ty, &active);
        return .{ .bytes = hasher.finalResult() };
    }

    fn writeSolvedTypeDigest(
        self: *Solver,
        hasher: *std.crypto.hash.sha2.Sha256,
        ty: Type.TypeVarId,
        active: *std.AutoHashMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        const root = self.program.types.root(ty);
        if (active.contains(root)) {
            writeBytes(hasher, "cycle");
            writeU32(hasher, @intFromEnum(root));
            return;
        }
        try active.put(root, {});
        defer _ = active.remove(root);

        switch (self.program.types.get(root)) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound, .forall => Common.invariant("unresolved Lambda Solved type reached erased callable digest"),
            .primitive => |primitive| {
                writeBytes(hasher, "primitive");
                writeBytes(hasher, @tagName(primitive));
            },
            .zst => writeBytes(hasher, "zst"),
            .erased => |erased| {
                writeBytes(hasher, "erased");
                hasher.update(&erased.source_fn_ty.bytes);
            },
            .func => |func| {
                writeBytes(hasher, "func");
                try self.writeSolvedTypeSpanDigest(hasher, func.args, active);
                try self.writeSolvedTypeDigest(hasher, func.ret, active);
            },
            .list => |elem| {
                writeBytes(hasher, "list");
                try self.writeSolvedTypeDigest(hasher, elem, active);
            },
            .box => |elem| {
                writeBytes(hasher, "box");
                try self.writeSolvedTypeDigest(hasher, elem, active);
            },
            .tuple => |items| {
                writeBytes(hasher, "tuple");
                try self.writeSolvedTypeSpanDigest(hasher, items, active);
            },
            .record => |fields| {
                writeBytes(hasher, "record");
                const field_slice = self.program.types.fieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (field_slice) |field| {
                    writeBytes(hasher, self.lifted.names.recordFieldLabelText(field.name));
                    try self.writeSolvedTypeDigest(hasher, field.ty, active);
                }
            },
            .tag_union => |tags| {
                writeBytes(hasher, "tag_union");
                const tag_slice = self.program.types.tagSpan(tags);
                writeU32(hasher, @intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    writeBytes(hasher, self.lifted.names.tagLabelText(tag.name));
                    try self.writeSolvedTypeSpanDigest(hasher, tag.payloads, active);
                }
            },
            .named => |named| {
                writeBytes(hasher, "named");
                hasher.update(&named.named_type.module.bytes);
                writeBytes(hasher, self.lifted.names.moduleNameText(named.def.module_name));
                writeBytes(hasher, self.lifted.names.typeNameText(named.def.type_name));
                writeBytes(hasher, @tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    writeBytes(hasher, "builtin");
                    writeBytes(hasher, @tagName(owner));
                } else {
                    writeBytes(hasher, "not-builtin");
                }
                try self.writeSolvedTypeSpanDigest(hasher, named.args, active);
            },
            .lambda_set => |members| {
                writeBytes(hasher, "lambda_set");
                const member_slice = self.program.types.memberSpan(members);
                writeU32(hasher, @intCast(member_slice.len));
                for (member_slice) |member| {
                    writeU32(hasher, @intFromEnum(member.lambda));
                    const captures = self.program.types.captureSpan(member.captures);
                    writeU32(hasher, @intCast(captures.len));
                    for (captures) |capture| {
                        writeU32(hasher, @intFromEnum(capture.symbol));
                        try self.writeSolvedTypeDigest(hasher, capture.ty, active);
                    }
                }
            },
        }
    }

    fn writeSolvedTypeSpanDigest(
        self: *Solver,
        hasher: *std.crypto.hash.sha2.Sha256,
        span: Type.Span,
        active: *std.AutoHashMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        const values = self.program.types.span(span);
        writeU32(hasher, @intCast(values.len));
        for (values) |child| {
            try self.writeSolvedTypeDigest(hasher, child, active);
        }
    }
};

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

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
        self.solver.program.types.set(reserved, try self.lowerContent(self.solver.lifted.types.get(ty)));
        return reserved;
    }

    /// Re-materializes a nominal record's declared field order from the monotype
    /// declared-field store into the Lambda Solved store. Named entries copy the
    /// shared field-name id; padding entries re-lower their reserved type.
    fn lowerDeclaredOrder(self: *TypeCloner, span: MonoType.Span) Allocator.Error!Type.Span {
        const source = self.solver.lifted.types.declaredFieldSpan(span);
        if (source.len == 0) return Type.Span.empty();
        const lowered = try self.solver.allocator.alloc(Type.DeclaredField, source.len);
        defer self.solver.allocator.free(lowered);
        for (source, 0..) |entry, i| {
            lowered[i] = switch (entry) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.lower(ty) },
            };
        }
        return try self.solver.program.types.addDeclaredFields(lowered);
    }

    fn lowerContent(self: *TypeCloner, content: MonoType.Content) Allocator.Error!Type.Content {
        return switch (content) {
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |source_fn_ty| .{ .erased = .{ .source_fn_ty = source_fn_ty, .members = .empty() } },
            .list => |elem| .{ .list = try self.lower(elem) },
            .box => |elem| .{ .box = try self.lower(elem) },
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSpan(self.solver.lifted.types.span(items));
                defer self.solver.allocator.free(lowered);
                break :blk .{ .tuple = try self.solver.program.types.addSpan(lowered) };
            },
            .record => |fields| blk: {
                const lowered = try self.solver.allocator.alloc(Type.Field, fields.len);
                defer self.solver.allocator.free(lowered);
                for (self.solver.lifted.types.fieldSpan(fields), 0..) |field, i| {
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
                for (self.solver.lifted.types.tagSpan(tags), 0..) |tag, i| {
                    const payloads = try self.lowerTypeSpan(self.solver.lifted.types.span(tag.payloads));
                    defer self.solver.allocator.free(payloads);
                    lowered[i] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.solver.program.types.addSpan(payloads),
                    };
                }
                break :blk .{ .tag_union = try self.solver.program.types.addTags(lowered) };
            },
            .named => |named| blk: {
                const args = try self.lowerTypeSpan(self.solver.lifted.types.span(named.args));
                defer self.solver.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = try self.solver.program.types.addSpan(args),
                    .backing = if (named.backing) |raw_backing| blk_backing: {
                        const backing_ty = if (isGeneratedOpaqueEvidenceOwner(named.builtin_owner))
                            raw_backing.ty
                        else
                            try self.structuralBackingForNamed(named.def, raw_backing.ty);
                        break :blk_backing .{
                            .ty = try self.lower(backing_ty),
                            .use = raw_backing.use,
                        };
                    } else null,
                    .declared_order = try self.lowerDeclaredOrder(named.declared_order),
                } };
            },
            .func => |fn_ty| blk: {
                const args = try self.lowerTypeSpan(self.solver.lifted.types.span(fn_ty.args));
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

    fn structuralBackingForNamed(
        self: *TypeCloner,
        owner_def: MonoType.TypeDef,
        backing: MonoType.TypeId,
    ) Allocator.Error!MonoType.TypeId {
        var seen = std.AutoHashMap(MonoType.TypeId, void).init(self.solver.allocator);
        defer seen.deinit();
        var current = backing;
        while (true) {
            if (seen.contains(current)) return current;
            try seen.put(current, {});
            switch (self.solver.lifted.types.get(current)) {
                .named => |named| {
                    if (named.kind != .alias and !sameMonoTypeDef(named.def, owner_def)) return current;
                    const next = named.backing orelse return current;
                    current = next.ty;
                },
                else => return current,
            }
        }
    }
};

fn generatedBackingScore(content: Type.Content) ?u8 {
    return switch (content) {
        .record => |fields| if (fields.count() == 0) 1 else 2,
        .zst => 1,
        else => null,
    };
}

fn isGeneratedOpaqueEvidenceOwner(owner: ?static_dispatch.BuiltinOwner) bool {
    const actual = owner orelse return false;
    return switch (actual) {
        .fields,
        .field,
        .parse_tag_union_spec,
        => true,
        else => false,
    };
}

fn sameMonoTypeDef(left: MonoType.TypeDef, right: MonoType.TypeDef) bool {
    return left.module_name == right.module_name and
        left.type_name == right.type_name and
        left.source_decl == right.source_decl;
}

test "lambda solved solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
