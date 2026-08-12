//! Lambda solving over lifted Monotype IR.

const std = @import("std");
const collections = @import("collections");
const can = @import("can");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const static_dispatch = check.StaticDispatchRegistry;
const names = check.CheckedNames;

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

/// The store writes a unification defers until every type it pushed onto the
/// unify stack has been processed.
const UnifyFinishAction = union(enum) {
    none,
    link_rhs_to_lhs: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
    },
    link_var_to_root: struct {
        var_: Type.TypeVarId,
        target: Type.TypeVarId,
    },
    set_left_erased_link_right: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        source_fn_ty: Type.names.TypeDigest,
        members: Type.Span,
    },
    set_left_lambda_set_link_right: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        members: Type.Span,
    },
    set_left_tag_union_link_right: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
        tags: Type.Span,
    },
};

const UnifyFrame = union(enum) {
    process: struct {
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
    },
    finish: struct {
        pair: UnifyPair,
        action: UnifyFinishAction,
    },
};

/// A pair of spans whose element-wise unification is deferred until after the
/// enclosing merge has computed the span it writes into the store.
const DeferredSpanPair = struct {
    lhs: Type.Span,
    rhs: Type.Span,
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
    join_points: std.ArrayList(ActiveJoinPoint),
    return_contexts: std.ArrayList(ReturnContext),
    active_unifications: std.AutoHashMap(UnifyPair, void),
    unify_stack: std.ArrayList(UnifyFrame),
    active_private_evidence_relations: std.AutoHashMap(UnifyPair, void),
    /// Per lifted Monotype: whether any `func` or `erased` node is reachable
    /// from it. Clones of callable-free types carry no unbound slots and no
    /// mutable lambda-set state, so one shared clone serves every use, and
    /// lazy-leaf walks skip callable-free leaves without materializing them.
    contains_callable: []bool,
    /// Per lifted Monotype: whether a forced-dynamic iterator named type is
    /// reachable from it. The forced-dynamic scan materializes exactly these
    /// leaves so the named nodes it must mark exist in the solved store.
    contains_forced_dynamic: []bool,
    shared_clones: collections.DenseMap(MonoType.TypeId, Type.TypeVarId),
    /// One memo map per lazily materialized tree, tying recursive
    /// back-references to their existing vars exactly as an eager clone's
    /// per-call memo did. Allocated on a leaf's first expansion.
    leaf_contexts: std.ArrayList(collections.DenseMap(MonoType.TypeId, Type.TypeVarId)),

    const FunctionShape = struct {
        args: Type.Span,
        callable: Type.TypeVarId,
        ret: Type.TypeVarId,
    };

    const BoundLowLevel = enum {
        box_box,
        box_unbox,
        list_get_unsafe,
        list_append_unsafe,
        list_concat,
        list_reserve,
        list_drop_at,
        list_sublist,
        list_take_first,
        list_take_last,
        list_drop_first,
        list_drop_last,
        list_release_excess_capacity,
        list_reverse,
        list_set,
        list_replace_unsafe,
        list_swap,
        list_prepend,
        dict_pseudo_seed,
        hasher_finish,
        crypto_sha256_hash_bytes,
        crypto_sha256_hasher_finish,
        crypto_blake3_hash_bytes,
        crypto_blake3_hasher_finish,
        crypto_sha256_hasher_empty,
        crypto_blake3_hasher_empty,
        crypto_sha256_hasher_write,
        crypto_blake3_hasher_write,
        hasher_write_bool,
        hasher_write_u8,
        hasher_write_u16,
        hasher_write_u32,
        hasher_write_u64,
        hasher_write_u128,
        hasher_write_i8,
        hasher_write_i16,
        hasher_write_i32,
        hasher_write_i64,
        hasher_write_i128,
        hasher_write_f32,
        hasher_write_f64,
        hasher_write_dec,
        hasher_write_bytes,
        hasher_write_str,
    };

    const ReturnContext = struct {
        mono_ret: MonoType.TypeId,
        solved_ret: Type.TypeVarId,
    };

    const ActiveJoinPoint = struct {
        id: Lifted.JoinPointId,
        params: Type.Span,
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

        const masks = try computeReachabilityMasks(allocator, lifted.types);
        errdefer allocator.free(masks.contains_callable);
        errdefer allocator.free(masks.contains_forced_dynamic);

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
            .join_points = .empty,
            .return_contexts = .empty,
            .active_unifications = std.AutoHashMap(UnifyPair, void).init(allocator),
            .unify_stack = .empty,
            .active_private_evidence_relations = std.AutoHashMap(UnifyPair, void).init(allocator),
            .contains_callable = masks.contains_callable,
            .contains_forced_dynamic = masks.contains_forced_dynamic,
            .shared_clones = collections.DenseMap(MonoType.TypeId, Type.TypeVarId).init(allocator),
            .leaf_contexts = .empty,
        };
    }

    fn deinit(self: *Solver) void {
        for (self.leaf_contexts.items) |*ctx| ctx.deinit();
        self.leaf_contexts.deinit(self.allocator);
        self.shared_clones.deinit();
        self.allocator.free(self.contains_forced_dynamic);
        self.allocator.free(self.contains_callable);
        self.active_private_evidence_relations.deinit();
        self.unify_stack.deinit(self.allocator);
        self.active_unifications.deinit();
        self.return_contexts.deinit(self.allocator);
        self.join_points.deinit(self.allocator);
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

        try self.markAbiBoundaryCallables();

        try self.program.layout_requests.ensureTotalCapacity(self.allocator, self.lifted.layout_requests.len);
        for (self.lifted.layout_requests) |request| {
            const ty = if (request.fn_id) |fn_id|
                self.fnRetType(fn_id)
            else
                try self.monoLeaf(request.ty);
            try self.markErasedCallablesReachedByType(ty);
            try self.program.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = ty,
                .fn_id = request.fn_id,
                .const_locator = request.const_locator,
            });
        }

        try self.program.runtime_schema_requests.ensureTotalCapacity(self.allocator, self.lifted.runtime_schema_requests.len);
        for (self.lifted.runtime_schema_requests) |request| {
            const ty = try self.monoLeaf(request.ty);
            try self.markErasedCallablesReachedByType(ty);
            try self.program.runtime_schema_requests.append(self.allocator, .{
                .def = request.def,
                .ty = ty,
            });
        }

        try self.markForcedDynamicIteratorCallables();

        try self.program.expr_tys.ensureTotalCapacity(self.allocator, self.expr_tys.len);
        for (self.expr_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.monoLeaf(self.lifted.exprs[index].ty);
            try self.program.expr_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        try self.program.pat_tys.ensureTotalCapacity(self.allocator, self.pat_tys.len);
        for (self.pat_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.monoLeaf(self.lifted.pats[index].ty);
            try self.program.pat_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        try self.program.local_tys.ensureTotalCapacity(self.allocator, self.local_tys.len);
        for (self.local_tys) |maybe_ty| {
            const ty = maybe_ty orelse Common.invariant("Lambda Solved local type slot was not initialized");
            try self.program.local_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        for (self.program.layout_requests.items) |*request| {
            request.ty = self.program.types.rootCompressed(request.ty);
        }
        for (self.program.runtime_schema_requests.items) |*request| {
            request.ty = self.program.types.rootCompressed(request.ty);
        }

        try self.finalizeMonoLeaves();
        // After finalization so the materialized clones' callable slots close
        // exactly as their eager counterparts always did.
        try self.closeUnfilledCallableSlots();
    }

    fn functionType(self: *Solver, fn_: Lifted.Fn) Allocator.Error!Type.TypeVarId {
        const arg_locals = self.lifted.typedLocalSpan(fn_.args);
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
                .checked_capture_id = local.checked_capture_id,
                .ty = self.localTy(capture.local),
            };
        }

        const capture_span = try self.program.types.addCaptures(captures);
        const members = [_]Type.FnMember{.{
            .lambda = fn_.symbol,
            .captures = capture_span,
        }};
        const callable = try self.program.types.add(.{ .lambda_set = try self.program.types.addMembers(&members) });

        if (fn_.signature) |signature| {
            const fn_ty = try self.monoLeaf(signature);
            const content = try self.resolvedContent(fn_ty);
            if (std.meta.activeTag(content) != .func) Common.invariant("producer-authored lifted function signature was not a function");
            const func = content.func;
            if (func.args.count() != arg_locals.len) {
                Common.invariant("producer-authored lifted function signature arity changed before Lambda Solved");
            }
            for (arg_locals, 0..) |arg, i| {
                const local = self.lifted.locals[@intFromEnum(arg.local)];
                if (@import("builtin").mode == .Debug and
                    !try self.sameMonoType(local.ty, arg.ty))
                {
                    Common.invariant("Lambda Solved function argument type differed from its local type");
                }
                try self.unify(self.localTy(arg.local), self.program.types.spanItem(func.args, i));
            }
            try self.unify(func.callable, callable);
            return self.program.types.rootCompressed(fn_ty);
        }

        const args = try self.allocator.alloc(Type.TypeVarId, arg_locals.len);
        defer self.allocator.free(args);
        for (arg_locals, 0..) |arg, i| {
            const local = self.lifted.locals[@intFromEnum(arg.local)];
            if (@import("builtin").mode == .Debug and
                !try self.sameMonoType(local.ty, arg.ty))
            {
                Common.invariant("Lambda Solved function argument type differed from its local type");
            }
            args[i] = self.localTy(arg.local);
        }

        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(args),
            .callable = callable,
            .ret = try self.lowerTypeFresh(fn_.ret),
        } });
    }

    fn fnRetType(self: *Solver, fn_id: Lifted.FnId) Type.TypeVarId {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.program.fn_tys.items.len) Common.invariant("Lambda Solved layout request referenced a missing function");
        const fn_ty = self.program.types.rootContentCompressed(self.program.fn_tys.items[raw]);
        if (std.meta.activeTag(fn_ty) != .func) Common.invariant("Lambda Solved layout request referenced a non-function");
        return fn_ty.func.ret;
    }

    fn solveFn(self: *Solver, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!void {
        const fn_ty = self.program.fn_tys.items[@intFromEnum(fn_id)];
        const fn_content = self.program.types.rootContentCompressed(fn_ty);
        if (std.meta.activeTag(fn_content) != .func) Common.invariant("Lambda Solved function table contains a non-function type");
        const func = fn_content.func;

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

    /// Host-facing function schemas use the erased callable representation for
    /// every callable value reachable from an argument or result. Seed that
    /// explicit boundary requirement after ordinary body constraints have
    /// unified, but before unresolved callable slots are closed as finite.
    fn markAbiBoundaryCallables(self: *Solver) Allocator.Error!void {
        for (self.lifted.fns, 0..) |fn_, index| {
            if (fn_.body != .hosted) continue;
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.markErasedCallablesAtFunctionBoundary(self.program.fn_tys.items[@intFromEnum(fn_id)]);
        }

        for (self.lifted.roots) |root| {
            switch (root.request.abi) {
                .platform, .hosted => {
                    const index = @intFromEnum(root.fn_id);
                    if (index >= self.program.fn_tys.items.len) {
                        Common.invariant("Lambda Solved ABI root referenced a missing function");
                    }
                    try self.markErasedCallablesAtFunctionBoundary(self.program.fn_tys.items[index]);
                },
                .roc, .test_expect, .compile_time => {},
            }
        }
    }

    fn markErasedCallablesAtFunctionBoundary(self: *Solver, fn_ty: Type.TypeVarId) Allocator.Error!void {
        const content = try self.resolvedContent(fn_ty);
        if (std.meta.activeTag(content) != .func) Common.invariant("Lambda Solved ABI boundary referenced a non-function");
        const func = content.func;
        for (0..func.args.count()) |index| {
            const arg = self.program.types.spanItem(func.args, index);
            try self.markErasedCallablesReachedByType(arg);
        }
        try self.markErasedCallablesReachedByType(func.ret);
    }

    fn closeUnfilledCallableSlots(self: *Solver) Allocator.Error!void {
        self.program.types.compressAllRoots();

        const count = self.program.types.vars.items.len;
        const done = try self.allocator.alloc(bool, count);
        defer self.allocator.free(done);
        @memset(done, false);

        const active = try self.allocator.alloc(bool, count);
        defer self.allocator.free(active);
        @memset(active, false);

        for (0..count) |index| {
            const ty: Type.TypeVarId = @enumFromInt(@as(u32, @intCast(index)));
            if (std.meta.activeTag(self.program.types.get(ty)) != .link) try self.closeCallableSlotsInType(ty, done, active);
        }
    }

    fn closeCallableSlotsInType(
        self: *Solver,
        ty: Type.TypeVarId,
        done: []bool,
        active: []bool,
    ) Allocator.Error!void {
        const root = self.program.types.rootCompressed(ty);
        const root_index = @intFromEnum(root);
        if (done[root_index] or active[root_index]) return;

        active[root_index] = true;
        defer {
            active[root_index] = false;
            done[root_index] = true;
        }

        switch (self.program.types.get(root)) {
            // A leaf never materialized its callable slots; finalization's
            // clones create them after this pass, unbound, exactly as the
            // post-solve eager clones always did.
            .mono => {},
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound,
            .forall,
            .primitive,
            .zst,
            => {},
            .func => |func| {
                try self.closeCallableSlot(func.callable, done, active);
                for (0..func.args.count()) |arg_index| {
                    const arg = self.program.types.spanItem(func.args, arg_index);
                    try self.closeCallableSlotsInType(arg, done, active);
                }
                try self.closeCallableSlotsInType(func.ret, done, active);
            },
            .list => |elem| try self.closeCallableSlotsInType(elem, done, active),
            .box => |elem| try self.closeCallableSlotsInType(elem, done, active),
            .tuple => |items| {
                for (0..items.count()) |index| {
                    const item = self.program.types.spanItem(items, index);
                    try self.closeCallableSlotsInType(item, done, active);
                }
            },
            .record => |fields| {
                for (0..fields.count()) |index| {
                    const field = self.program.types.fieldItem(fields, index);
                    try self.closeCallableSlotsInType(field.ty, done, active);
                    if (field.value_ty) |value_ty| try self.closeCallableSlotsInType(value_ty, done, active);
                }
            },
            .tag_union => |tags| {
                for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    for (0..tag.payloads.count()) |payload_index| {
                        const payload = self.program.types.spanItem(tag.payloads, payload_index);
                        try self.closeCallableSlotsInType(payload, done, active);
                    }
                }
            },
            .named => |named| {
                for (0..named.args.count()) |index| {
                    const arg = self.program.types.spanItem(named.args, index);
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
        const root = self.program.types.rootCompressed(callable);
        switch (self.program.types.get(root)) {
            .unbound => self.program.types.set(root, .{ .lambda_set = .empty() }),
            .lambda_set,
            .erased,
            => try self.closeCallableSlotsInType(root, done, active),
            .link,
            .forall,
            .primitive,
            .named,
            .record,
            .tuple,
            .tag_union,
            .list,
            .box,
            .func,
            .zst,
            .mono,
            => Common.invariant("function callable slot resolved to a non-callable type"),
        }
    }

    fn closeCallableSlotsInMembers(
        self: *Solver,
        members: Type.Span,
        done: []bool,
        active: []bool,
    ) Allocator.Error!void {
        for (0..members.count()) |member_index| {
            const member = self.program.types.memberItem(members, member_index);
            for (0..member.captures.count()) |capture_index| {
                const capture = self.program.types.captureItem(member.captures, capture_index);
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
            .static_data_candidate => |candidate| _ = try self.expectExpr(candidate.runtime_expr, expected),
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
            .record_update => |update| {
                _ = try self.expectExpr(update.base, expected);
                for (self.lifted.fieldExprSpan(update.fields)) |field| {
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
                    if (try self.hasBuiltinOwner(expected, .fields) or try self.hasBuiltinOwner(expected, .field)) {
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
                var prefix_ty = try self.inferExpr(field.receiver);
                const segments = self.lifted.fieldAccessSegmentSpan(field.segments);
                if (segments.len == 0) Common.invariant("field access path had no segments");
                for (segments) |segment| {
                    prefix_ty = try self.recordField(prefix_ty, segment.field);
                }
                try self.unify(expected, prefix_ty);
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
                    for (self.lifted.stmtSpan(branch.bindings)) |binding| try self.inferStmt(binding);
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
                const content = try self.shapeContent(try_ty);
                if (std.meta.activeTag(content) != .tag_union) Common.invariant("try_sequence input was not a Try tag union");
                const tags = content.tag_union;
                var ok_ty: ?Type.TypeVarId = null;
                for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    if (!std.mem.eql(u8, self.lifted.names.tagLabelText(tag.name), "Ok")) continue;
                    if (tag.payloads.count() != 1) Common.invariant("try_sequence Ok tag had unexpected payload arity");
                    ok_ty = self.program.types.spanItem(tag.payloads, 0);
                    break;
                }
                try self.unify(self.localTy(sequence.ok_local), ok_ty orelse Common.invariant("try_sequence input had no Ok tag"));
                _ = try self.expectExpr(sequence.ok_body, expected);
            },
            .try_record_sequence => |sequence| {
                const try_ty = try self.inferExpr(sequence.try_expr);
                const content = try self.shapeContent(try_ty);
                if (std.meta.activeTag(content) != .tag_union) Common.invariant("try_record_sequence input was not a Try tag union");
                const tags = content.tag_union;
                var ok_ty: ?Type.TypeVarId = null;
                for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    if (!std.mem.eql(u8, self.lifted.names.tagLabelText(tag.name), "Ok")) continue;
                    if (tag.payloads.count() != 1) Common.invariant("try_record_sequence Ok tag had unexpected payload arity");
                    ok_ty = self.program.types.spanItem(tag.payloads, 0);
                    break;
                }
                const ok_record_ty = ok_ty orelse Common.invariant("try_record_sequence input had no Ok tag");
                try self.unify(self.localTy(sequence.value_local), try self.recordField(ok_record_ty, sequence.value_field));
                try self.unify(self.localTy(sequence.rest_local), try self.recordField(ok_record_ty, sequence.rest_field));
                _ = try self.expectExpr(sequence.ok_body, expected);
            },
            .@"unreachable" => {},
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
            .join_point => |join_point| {
                const params = self.lifted.typedLocalSpan(join_point.params);
                const param_tys = try self.allocator.alloc(Type.TypeVarId, params.len);
                defer self.allocator.free(param_tys);
                for (params, 0..) |param, param_index| {
                    param_tys[param_index] = self.localTy(param.local);
                }
                try self.join_points.append(self.allocator, .{
                    .id = join_point.id,
                    .params = try self.program.types.addSpan(param_tys),
                });
                defer _ = self.join_points.pop();
                _ = try self.expectExpr(join_point.body, expected);
                _ = try self.expectExpr(join_point.remainder, expected);
            },
            .jump => |jump| {
                const params = self.activeJoinPoint(jump.target).params;
                const args = self.lifted.exprSpan(jump.args);
                if (params.count() != args.len) Common.invariant("jump argument count differs from join-point parameter count");
                for (args, 0..) |arg, arg_index| {
                    _ = try self.expectExpr(arg, self.program.types.spanItem(params, arg_index));
                }
            },
            .return_ => |ret| _ = try self.expectExpr(ret.value, try self.returnTargetTy(ret.target)),
            .dbg,
            .expect,
            => |child| _ = try self.inferExpr(child),
            .expect_err => |expect_err| _ = try self.inferExpr(expect_err.msg),
            .comptime_branch_taken => |taken| _ = try self.expectExpr(taken.body, expected),
        }
        return self.program.types.rootCompressed(expected);
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
        const tag = std.meta.activeTag(expr.data);
        if (tag == .record) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            for (self.lifted.fieldExprSpan(expr.data.record)) |field| {
                _ = try self.inferExpr(field.value);
            }
            return;
        }
        if (tag == .record_update) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            const update = expr.data.record_update;
            _ = try self.inferExpr(update.base);
            for (self.lifted.fieldExprSpan(update.fields)) |field| {
                _ = try self.inferExpr(field.value);
            }
            return;
        }
        if (tag == .tuple) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            for (self.lifted.exprSpan(expr.data.tuple)) |item| {
                _ = try self.inferExpr(item);
            }
            return;
        }
        if (tag == .tag) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            for (self.lifted.exprSpan(expr.data.tag.payloads)) |payload| {
                _ = try self.inferExpr(payload);
            }
            return;
        }
        if (tag == .static_data_candidate) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            try self.inferGeneratedOpaqueBacking(expr.data.static_data_candidate.runtime_expr);
            return;
        }
        if (tag == .nominal) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            try self.inferGeneratedOpaqueBacking(expr.data.nominal);
            return;
        }
        if (tag == .let_) {
            _ = try self.exprSlot(expr_id);
            self.expr_done[index] = true;
            const let_ = expr.data.let_;
            const value_ty = try self.inferExpr(let_.value);
            try self.bindPattern(let_.bind, value_ty);
            try self.inferGeneratedOpaqueBacking(let_.rest);
            return;
        }
        _ = try self.inferExpr(expr_id);
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
                if (self.hasGeneratedOpaquePatOwner(pat_id) or try self.hasBuiltinOwner(pat_ty, .fields) or try self.hasBuiltinOwner(pat_ty, .field)) {
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
        const content = self.lifted.types.get(self.lifted.pats[@intFromEnum(pat_id)].ty);
        if (std.meta.activeTag(content) != .named) return false;
        const backing = content.named.backing orelse return false;
        return backing.authority == .generated_private;
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
        const generated = self.program.types.rootCompressed(generated_ty);
        const expected = self.program.types.rootCompressed(expected_ty);
        if (generated == expected) return;
        // The caller reached this path only through a pattern whose named
        // backing carries generated-private authority. Preserve that explicit
        // producer-owned backing deterministically; structural size is not an
        // authority signal.
        self.program.types.set(expected, .{ .link = generated });
    }

    fn expectExpr(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const slot = try self.expectExprSlot(expr_id, expected);
        const inferred = try self.inferExpr(expr_id);
        try self.unify(slot, inferred);
        return self.program.types.rootCompressed(slot);
    }

    fn exprSlot(self: *Solver, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| return ty;

        const expr = self.lifted.exprs[index];
        const tag = std.meta.activeTag(expr.data);
        const ty = if (tag == .local)
            self.localTy(expr.data.local)
        else if (tag == .fn_ref)
            self.program.fn_tys.items[@intFromEnum(expr.data.fn_ref.fn_id)]
        else if (tag == .call_proc)
            switch (Lifted.directCallee(expr.data.call_proc)) {
                .local => |callee| (try self.functionShape(self.program.fn_tys.items[@intFromEnum(callee)])).ret,
                .imported => try self.lowerTypeFresh(expr.ty),
            }
        else
            try self.lowerTypeFresh(expr.ty);
        self.expr_tys[index] = ty;
        return ty;
    }

    fn expectExprSlot(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(expr_id);
        if (self.expr_tys[index]) |ty| {
            try self.unify(ty, expected);
            return self.program.types.rootCompressed(ty);
        }

        const expr = self.lifted.exprs[index];
        const tag = std.meta.activeTag(expr.data);
        const ty = if (tag == .local)
            self.localTy(expr.data.local)
        else if (tag == .fn_ref)
            self.program.fn_tys.items[@intFromEnum(expr.data.fn_ref.fn_id)]
        else
            expected;
        try self.unify(ty, expected);
        self.expr_tys[index] = ty;
        return self.program.types.rootCompressed(ty);
    }

    fn expectPat(self: *Solver, pat_id: Lifted.PatId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const index = @intFromEnum(pat_id);
        if (self.pat_tys[index]) |ty| {
            try self.unify(ty, expected);
            return self.program.types.rootCompressed(ty);
        }

        const pat = self.lifted.pats[index];
        const tag = std.meta.activeTag(pat.data);
        const ty = if (tag == .bind)
            self.localTy(pat.data.bind)
        else if (tag == .as)
            self.localTy(pat.data.as.local)
        else
            expected;
        try self.unify(ty, expected);
        self.pat_tys[index] = ty;
        return self.program.types.rootCompressed(ty);
    }

    fn functionShape(self: *Solver, ty: Type.TypeVarId) Allocator.Error!FunctionShape {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .func) Common.invariant("call expression had a non-function checked type");
        return .{ .args = content.func.args, .callable = content.func.callable, .ret = content.func.ret };
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

    fn markForcedDynamicIteratorCallables(self: *Solver) Allocator.Error!void {
        self.program.types.compressAllRoots();
        // Expanding a leaf appends fresh child vars, so the bound is re-read
        // every iteration and an expanded var is revisited in place.
        var index: usize = 0;
        while (index < self.program.types.vars.items.len) : (index += 1) {
            const ty: Type.TypeVarId = @enumFromInt(@as(u32, @intCast(index)));
            if (self.program.types.rootCompressed(ty) != ty) continue;
            const content = self.program.types.get(ty);
            const tag = std.meta.activeTag(content);
            if (tag == .named) {
                if (content.named.def.iterator_representation == .forced_dynamic) {
                    try self.markErasedCallablesReachedByType(ty);
                }
            } else if (tag == .mono) {
                if (self.contains_forced_dynamic[@intFromEnum(content.mono.id)]) {
                    _ = try self.expandMonoRoot(ty, content.mono);
                    index -= 1;
                }
            }
        }
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

    fn activeJoinPoint(self: *Solver, id: Lifted.JoinPointId) ActiveJoinPoint {
        var index = self.join_points.items.len;
        while (index > 0) {
            index -= 1;
            const join_point = self.join_points.items[index];
            if (join_point.id == id) return join_point;
        }
        Common.invariant("jump expression referenced a join point outside its lexical scope");
    }

    fn markErasedCallablesReachedByType(self: *Solver, ty: Type.TypeVarId) Allocator.Error!void {
        var active = collections.DenseMap(Type.TypeVarId, void).init(self.allocator);
        defer active.deinit();
        try self.markErasedCallablesReachedByTypeInner(ty, &active);
    }

    fn markErasedCallablesReachedByTypeInner(
        self: *Solver,
        ty: Type.TypeVarId,
        active: *collections.DenseMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        const root = self.program.types.rootCompressed(ty);
        if (active.contains(root)) return;
        try active.put(root, {});
        defer _ = active.remove(root);

        const content = self.program.types.get(root);
        const resolved = if (std.meta.activeTag(content) == .mono)
            // Callable-free leaves contain nothing this walk could mark.
            if (self.contains_callable[@intFromEnum(content.mono.id)])
                try self.expandMonoRoot(root, content.mono)
            else
                return
        else
            content;
        switch (resolved) {
            .mono => Common.invariant("lazy Monotype leaf reached erased-callable marking unexpanded"),
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound, .forall, .primitive, .zst => {},
            .erased => |erased| try self.markErasedCallablesReachedByMembers(erased.members, active),
            .func => |func| {
                const erased = try self.program.types.add(.{ .erased = .{
                    .source_fn_ty = try self.solvedTypeDigest(root),
                    .members = .empty(),
                } });
                try self.unify(func.callable, erased);
                for (0..func.args.count()) |index| {
                    const arg = self.program.types.spanItem(func.args, index);
                    try self.markErasedCallablesReachedByTypeInner(arg, active);
                }
                try self.markErasedCallablesReachedByTypeInner(func.ret, active);
            },
            .list => |elem| try self.markErasedCallablesReachedByTypeInner(elem, active),
            .box => |elem| try self.markErasedCallablesReachedByTypeInner(elem, active),
            .tuple => |items| {
                for (0..items.count()) |index| {
                    const item = self.program.types.spanItem(items, index);
                    try self.markErasedCallablesReachedByTypeInner(item, active);
                }
            },
            .record => |fields| {
                for (0..fields.count()) |index| {
                    const field = self.program.types.fieldItem(fields, index);
                    try self.markErasedCallablesReachedByTypeInner(field.ty, active);
                    if (field.value_ty) |value_ty| try self.markErasedCallablesReachedByTypeInner(value_ty, active);
                }
            },
            .tag_union => |tags| {
                for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    for (0..tag.payloads.count()) |payload_index| {
                        const payload = self.program.types.spanItem(tag.payloads, payload_index);
                        try self.markErasedCallablesReachedByTypeInner(payload, active);
                    }
                }
            },
            .named => |named| {
                for (0..named.args.count()) |index| {
                    const arg = self.program.types.spanItem(named.args, index);
                    try self.markErasedCallablesReachedByTypeInner(arg, active);
                }
                if (named.backing) |backing| {
                    try self.markErasedCallablesReachedByTypeInner(backing.ty, active);
                }
            },
            .lambda_set => |members| try self.markErasedCallablesReachedByMembers(members, active),
        }
    }

    fn markErasedCallablesReachedByMembers(
        self: *Solver,
        members: Type.Span,
        active: *collections.DenseMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        for (0..members.count()) |member_index| {
            const member = self.program.types.memberItem(members, member_index);
            for (0..member.captures.count()) |capture_index| {
                const capture = self.program.types.captureItem(member.captures, capture_index);
                try self.markErasedCallablesReachedByTypeInner(capture.ty, active);
            }
        }
    }

    /// New lazy leaf for a lifted Monotype. Each use owns its var; the leaf
    /// materializes one level at a time as unification or shape reads touch
    /// it, and `finalizeMonoLeaves` replaces whatever survives solving.
    fn monoLeaf(self: *Solver, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        return try self.program.types.add(.{ .mono = .{ .id = ty } });
    }

    fn lowerTypeFresh(self: *Solver, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        return try self.monoLeaf(ty);
    }

    const MonoLeaf = std.meta.fieldInfo(Type.Content, .mono).type;

    /// Materialize a lazy leaf's root one level in place: children become new
    /// leaves and function callable slots start unbound, exactly as an eager
    /// clone's would. The leaf's clone context ties recursive back-references
    /// to their existing vars, so a recursive Monotype materializes as the
    /// same cyclic graph an eager clone produced.
    fn expandMonoRoot(self: *Solver, root: Type.TypeVarId, leaf: MonoLeaf) Allocator.Error!Type.Content {
        const ctx: u32 = if (leaf.ctx != Type.no_leaf_context) leaf.ctx else blk: {
            const index: u32 = @intCast(self.leaf_contexts.items.len);
            try self.leaf_contexts.append(self.allocator, collections.DenseMap(MonoType.TypeId, Type.TypeVarId).init(self.allocator));
            break :blk index;
        };
        if (self.leaf_contexts.items[ctx].get(leaf.id)) |existing| {
            const existing_root = self.program.types.rootCompressed(existing);
            if (existing_root != root) {
                self.program.types.set(root, .{ .link = existing_root });
                return try self.resolvedContentAt(existing_root);
            }
        } else {
            try self.leaf_contexts.items[ctx].put(leaf.id, root);
        }
        var cloner = TypeCloner.init(self);
        cloner.lazy_ctx = ctx;
        defer cloner.deinit();
        const content = try cloner.lowerContent(self.lifted.types.get(leaf.id));
        self.program.types.set(root, content);
        return content;
    }

    fn resolvedContentAt(self: *Solver, root: Type.TypeVarId) Allocator.Error!Type.Content {
        const content = self.program.types.get(root);
        if (std.meta.activeTag(content) == .mono) return try self.expandMonoRoot(root, content.mono);
        return content;
    }

    fn resolvedContent(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Content {
        return try self.resolvedContentAt(self.program.types.rootCompressed(ty));
    }

    /// Replace every output-reachable lazy leaf with a link to a materialized
    /// clone so program views never observe one. Untouched leaves of one
    /// callable-free Monotype share one clone; callable-bearing leaves get a
    /// private clone whose callable-free subgraphs still share.
    fn finalizeMonoLeaves(self: *Solver) Allocator.Error!void {
        var visited = collections.DenseMap(Type.TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        var work = std.ArrayList(Type.TypeVarId).empty;
        defer work.deinit(self.allocator);

        for (self.program.defs.items) |def| try work.append(self.allocator, def.ty);
        try work.appendSlice(self.allocator, self.program.fn_tys.items);
        try work.appendSlice(self.allocator, self.program.local_tys.items);
        try work.appendSlice(self.allocator, self.program.expr_tys.items);
        try work.appendSlice(self.allocator, self.program.pat_tys.items);
        for (self.program.layout_requests.items) |request| try work.append(self.allocator, request.ty);
        for (self.program.runtime_schema_requests.items) |request| try work.append(self.allocator, request.ty);

        while (work.pop()) |ty| {
            const root = self.program.types.rootCompressed(ty);
            const gop = try visited.getOrPut(root);
            if (gop.found_existing) continue;
            switch (self.program.types.get(root)) {
                .link => Common.invariant("Lambda Solved root returned a link"),
                .mono => |leaf| {
                    const clone = try self.finalMonoClone(leaf.id);
                    self.program.types.set(root, .{ .link = self.program.types.rootCompressed(clone) });
                },
                .unbound, .forall, .primitive, .zst => {},
                .list, .box => |elem| try work.append(self.allocator, elem),
                .tuple => |items| for (0..items.count()) |index| {
                    try work.append(self.allocator, self.program.types.spanItem(items, index));
                },
                .record => |fields| for (0..fields.count()) |index| {
                    try work.append(self.allocator, self.program.types.fieldItem(fields, index).ty);
                },
                .tag_union => |tags| for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    for (0..tag.payloads.count()) |payload_index| {
                        try work.append(self.allocator, self.program.types.spanItem(tag.payloads, payload_index));
                    }
                },
                .func => |func| {
                    for (0..func.args.count()) |index| {
                        try work.append(self.allocator, self.program.types.spanItem(func.args, index));
                    }
                    try work.append(self.allocator, func.callable);
                    try work.append(self.allocator, func.ret);
                },
                .named => |named| {
                    for (0..named.args.count()) |index| {
                        try work.append(self.allocator, self.program.types.spanItem(named.args, index));
                    }
                    if (named.backing) |backing| try work.append(self.allocator, backing.ty);
                    for (0..named.declared_order.count()) |index| switch (self.program.types.declaredFieldItem(named.declared_order, index)) {
                        .named => {},
                        .padding => |padding_ty| try work.append(self.allocator, padding_ty),
                    };
                },
                .lambda_set => |members| for (0..members.count()) |member_index| {
                    const member = self.program.types.memberItem(members, member_index);
                    for (0..member.captures.count()) |capture_index| {
                        try work.append(self.allocator, self.program.types.captureItem(member.captures, capture_index).ty);
                    }
                },
                .erased => |erased| for (0..erased.members.count()) |member_index| {
                    const member = self.program.types.memberItem(erased.members, member_index);
                    for (0..member.captures.count()) |capture_index| {
                        try work.append(self.allocator, self.program.types.captureItem(member.captures, capture_index).ty);
                    }
                },
            }
        }
    }

    /// Materialized clone for a leaf that survived solving, matching what the
    /// post-solve eager clone produced: shared for callable-free Monotypes,
    /// self-marking for forced-dynamic iterator content.
    fn finalMonoClone(self: *Solver, id: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        var cloner = TypeCloner.init(self);
        cloner.share = true;
        defer cloner.deinit();
        const lowered = try cloner.lower(id);
        try cloner.markForcedDynamicCallables();
        return lowered;
    }

    fn listElem(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .list) Common.invariant("list expression had a non-list checked type");
        return content.list;
    }

    fn tupleItemsSpan(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Span {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .tuple) Common.invariant("tuple expression had a non-tuple checked type");
        return content.tuple;
    }

    fn recordField(self: *Solver, ty: Type.TypeVarId, name: Type.names.RecordFieldNameId) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .record) Common.invariant("record field operation had a non-record checked type");
        for (0..content.record.count()) |index| {
            const field = self.program.types.fieldItem(content.record, index);
            if (field.name == name) return field.ty;
        }
        Common.invariant("record field was absent from checked record type");
    }

    fn recordFieldByLabel(self: *Solver, ty: Type.TypeVarId, label: []const u8) Allocator.Error!Type.TypeVarId {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .record) Common.invariant("low-level record result had a non-record checked type");
        for (0..content.record.count()) |index| {
            const field = self.program.types.fieldItem(content.record, index);
            if (std.mem.eql(u8, self.lifted.names.recordFieldLabelText(field.name), label)) return field.ty;
        }
        Common.invariant("low-level record result was missing a required field");
    }

    fn tagPayloadsSpan(self: *Solver, ty: Type.TypeVarId, name: Type.names.TagNameId) Allocator.Error!Type.Span {
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .tag_union) Common.invariant("tag operation had a non-tag-union checked type");
        for (0..content.tag_union.count()) |index| {
            const tag = self.program.types.tagItem(content.tag_union, index);
            if (tag.name == name) return tag.payloads;
        }
        Common.invariant("tag was absent from checked tag-union type");
    }

    fn namedBacking(self: *Solver, ty: Type.TypeVarId) Allocator.Error!?Type.TypeVarId {
        const content = try self.resolvedContent(ty);
        if (std.meta.activeTag(content) != .named) return null;
        return if (content.named.backing) |backing| backing.ty else null;
    }

    fn hasBuiltinOwner(self: *Solver, ty: Type.TypeVarId, owner: static_dispatch.BuiltinOwner) Allocator.Error!bool {
        const content = try self.resolvedContent(ty);
        if (std.meta.activeTag(content) != .named) return false;
        return if (content.named.builtin_owner) |builtin_owner| builtin_owner == owner else false;
    }

    fn bindLowLevelTypes(
        self: *Solver,
        op: can.CIR.Expr.LowLevel,
        expected: Type.TypeVarId,
        args: []const Type.TypeVarId,
    ) Allocator.Error!void {
        const bound_op = std.meta.stringToEnum(BoundLowLevel, @tagName(op)) orelse return;
        switch (bound_op) {
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
        const content = try self.shapeContent(ty);
        if (std.meta.activeTag(content) != .box) Common.invariant("box low-level operation had a non-box checked type");
        return content.box;
    }

    fn shapeContent(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.Content {
        var current = self.program.types.rootCompressed(ty);
        while (true) {
            const content = try self.resolvedContentAt(current);
            if (std.meta.activeTag(content) != .named) return content;
            const backing = content.named.backing orelse return content;
            current = self.program.types.rootCompressed(backing.ty);
        }
    }

    /// Drive unification from an explicit stack so structural nesting costs
    /// heap frames instead of call frames. The loop only owns the frames it
    /// pushed above `base`, so the helpers below may call back into `unify`
    /// while an outer unification still has pending frames underneath.
    fn unify(self: *Solver, lhs: Type.TypeVarId, rhs: Type.TypeVarId) Allocator.Error!void {
        const base = self.unify_stack.items.len;
        try self.pushUnifyPair(&self.unify_stack, lhs, rhs);

        while (self.unify_stack.items.len > base) {
            const frame = self.unify_stack.pop().?;
            switch (frame) {
                .process => |process| try self.processUnifyPair(&self.unify_stack, process.lhs, process.rhs),
                .finish => |finish| {
                    self.applyUnifyFinish(finish.action);
                    _ = self.active_unifications.remove(finish.pair);
                },
            }
        }
    }

    fn pushUnifyPair(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
    ) Allocator.Error!void {
        try stack.append(self.allocator, .{ .process = .{ .lhs = lhs, .rhs = rhs } });
    }

    fn processUnifyPair(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        lhs: Type.TypeVarId,
        rhs: Type.TypeVarId,
    ) Allocator.Error!void {
        const a = self.program.types.rootCompressed(lhs);
        const b = self.program.types.rootCompressed(rhs);
        if (a == b) return;

        const raw_left = self.program.types.get(a);
        const raw_right = self.program.types.get(b);
        if (raw_left == .mono and raw_right == .mono and raw_left.mono.id == raw_right.mono.id) {
            self.program.types.set(b, .{ .link = a });
            return;
        }
        const left = if (std.meta.activeTag(raw_left) == .mono) try self.expandMonoRoot(a, raw_left.mono) else raw_left;
        const right = if (std.meta.activeTag(raw_right) == .mono) try self.expandMonoRoot(b, raw_right.mono) else raw_right;

        const left_tag = std.meta.activeTag(left);
        if (left_tag == .link) Common.invariant("Lambda Solved root returned a link");
        if (left_tag == .unbound) {
            self.program.types.set(a, .{ .link = b });
            return;
        }
        if (left_tag == .forall) Common.invariant("generalized Lambda Solved type reached local unification without instantiation");

        const right_tag = std.meta.activeTag(right);
        if (right_tag == .link) Common.invariant("Lambda Solved root returned a link");
        if (right_tag == .unbound) {
            self.program.types.set(b, .{ .link = a });
            return;
        }
        if (right_tag == .forall) Common.invariant("generalized Lambda Solved type reached local unification without instantiation");

        const pair = UnifyPair.init(a, b);
        const active_entry = try self.active_unifications.getOrPut(pair);
        if (active_entry.found_existing) return;
        errdefer _ = self.active_unifications.remove(pair);

        // Reserve the finish frame before pushing any children so it pops last
        // and retires `pair` once every type it scheduled has been unified.
        const finish_index = stack.items.len;
        try stack.append(self.allocator, .{ .finish = .{ .pair = pair, .action = .none } });
        try self.unifyRoots(stack, finish_index, a, b, left, right);
    }

    fn unifyRoots(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        finish_index: usize,
        a: Type.TypeVarId,
        b: Type.TypeVarId,
        left: Type.Content,
        right: Type.Content,
    ) Allocator.Error!void {
        if (transparentAliasBacking(left)) |backing| {
            stack.items[finish_index].finish.action = .{ .link_var_to_root = .{ .var_ = a, .target = backing } };
            try self.pushUnifyPair(stack, backing, b);
            return;
        }
        if (transparentAliasBacking(right)) |backing| {
            stack.items[finish_index].finish.action = .{ .link_var_to_root = .{ .var_ = b, .target = backing } };
            try self.pushUnifyPair(stack, a, backing);
            return;
        }
        if (try self.typeIsProvenUninhabited(a)) {
            self.program.types.set(a, .{ .link = b });
            return;
        }
        if (try self.typeIsProvenUninhabited(b)) {
            self.program.types.set(b, .{ .link = a });
            return;
        }
        if (try self.unifyInspectableNamedBacking(a, b, left, right)) return;
        if (try self.unifyInspectableNamedBacking(b, a, right, left)) return;
        if (try self.unifyPublicNamedBacking(a, b, right)) return;
        if (try self.unifyPublicNamedBacking(b, a, left)) return;

        switch (left) {
            .mono => Common.invariant("lazy Monotype leaf reached unification unexpanded"),
            .primitive => |left_primitive| {
                if (right != .primitive) Common.invariant("primitive type failed Lambda Solved unification");
                if (left_primitive != right.primitive) {
                    Common.invariant("primitive types failed Lambda Solved unification");
                }
                self.program.types.set(b, .{ .link = a });
            },
            .zst => {
                if (right != .zst) Common.invariant("zero-sized type failed Lambda Solved unification");
                self.program.types.set(b, .{ .link = a });
            },
            .erased => |left_erased| {
                if (right == .erased) {
                    const right_erased = right.erased;
                    if (!std.mem.eql(u8, left_erased.source_fn_ty.bytes[0..], right_erased.source_fn_ty.bytes[0..])) {
                        Common.invariant("erased callable source function types failed Lambda Solved unification");
                    }
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_erased.members, right_erased.members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_erased_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .source_fn_ty = left_erased.source_fn_ty,
                        .members = merged,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else if (right == .lambda_set) {
                    const right_members = right.lambda_set;
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_erased.members, right_members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_erased_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .source_fn_ty = left_erased.source_fn_ty,
                        .members = merged,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else {
                    Common.invariant("erased callable type failed Lambda Solved unification");
                }
            },
            .lambda_set => |left_members| {
                if (right == .erased) {
                    const right_erased = right.erased;
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_members, right_erased.members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_erased_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .source_fn_ty = right_erased.source_fn_ty,
                        .members = merged,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else if (right == .lambda_set) {
                    const right_members = right.lambda_set;
                    var capture_pairs = std.ArrayList(DeferredSpanPair).empty;
                    defer capture_pairs.deinit(self.allocator);
                    const merged = try self.mergeLambdaSets(left_members, right_members, &capture_pairs);
                    stack.items[finish_index].finish.action = .{ .set_left_lambda_set_link_right = .{
                        .lhs = a,
                        .rhs = b,
                        .members = merged,
                    } };
                    try self.pushCaptureSpanPairs(stack, capture_pairs.items);
                } else {
                    Common.invariant("lambda set failed Lambda Solved unification");
                }
            },
            .func => |left_fn| {
                if (right != .func) Common.invariant("function type failed Lambda Solved unification");
                const right_fn = right.func;
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushUnifyPair(stack, left_fn.ret, right_fn.ret);
                try self.pushUnifyPair(stack, left_fn.callable, right_fn.callable);
                try self.pushSpanPairs(stack, left_fn.args, right_fn.args, "function argument lists failed Lambda Solved unification");
            },
            .list => |left_elem| {
                if (right != .list) Common.invariant("list type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushUnifyPair(stack, left_elem, right.list);
            },
            .box => |left_elem| {
                if (right != .box) Common.invariant("box type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushUnifyPair(stack, left_elem, right.box);
            },
            .tuple => |left_items| {
                if (right != .tuple) Common.invariant("tuple type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushSpanPairs(stack, left_items, right.tuple, "tuple item lists failed Lambda Solved unification");
            },
            .record => |left_fields| {
                if (right != .record) Common.invariant("record type failed Lambda Solved unification");
                stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                try self.pushFieldPairs(stack, left_fields, right.record);
            },
            .tag_union => |left_tags| {
                if (right != .tag_union) Common.invariant("tag-union type failed Lambda Solved unification");
                const right_tags = right.tag_union;
                if (left_tags.count() == 0) {
                    self.program.types.set(a, .{ .link = b });
                    return;
                }
                if (right_tags.count() == 0) {
                    self.program.types.set(b, .{ .link = a });
                    return;
                }
                var payload_pairs = std.ArrayList(DeferredSpanPair).empty;
                defer payload_pairs.deinit(self.allocator);
                const merged = try self.mergeTags(left_tags, right_tags, &payload_pairs);
                stack.items[finish_index].finish.action = .{ .set_left_tag_union_link_right = .{
                    .lhs = a,
                    .rhs = b,
                    .tags = merged,
                } };
                try self.pushPayloadSpanPairs(stack, payload_pairs.items);
            },
            .named => |left_named| {
                if (right != .named) Common.invariant("named type failed Lambda Solved unification");
                const right_named = right.named;
                if (!std.meta.eql(left_named.def, right_named.def) or
                    left_named.kind != right_named.kind or
                    left_named.builtin_owner != right_named.builtin_owner)
                {
                    if (try self.unifyForcedDynamicIterator(a, b, left_named, right_named)) return;
                    if (try self.unifyIteratorOwnerStampedPublic(a, b, left_named, right_named)) return;
                    if (try self.unifyGeneratedIteratorJoin(a, b, left_named, right_named)) return;
                    if (try self.unifyPublicGeneratedIterator(a, b, left_named, right_named)) return;
                    if (try self.unifyNominalOpaqueViews(a, b, left_named, right_named)) return;
                    Common.invariant("named type identity failed Lambda Solved unification");
                }
                if (left_named.backing) |left_backing| {
                    const right_backing = right_named.backing orelse Common.invariant("named type backing differed during Lambda Solved unification");
                    if (left_backing.use != right_backing.use) Common.invariant("named type backing use differed during Lambda Solved unification");
                    if (left_backing.authority == right_backing.authority) {
                        stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                        try self.pushUnifyPair(stack, left_backing.ty, right_backing.ty);
                    } else if (left_backing.authority == .generated_private) {
                        try self.relateGeneratedPrivateEvidence(right_backing.ty, left_backing.ty);
                        self.program.types.set(b, .{ .link = a });
                    } else if (right_backing.authority == .generated_private) {
                        try self.relateGeneratedPrivateEvidence(left_backing.ty, right_backing.ty);
                        self.program.types.set(a, .{ .link = b });
                    } else {
                        Common.invariant("named type backing authorities were incompatible during Lambda Solved unification");
                    }
                } else if (right_named.backing != null) {
                    Common.invariant("named type backing differed during Lambda Solved unification");
                } else {
                    stack.items[finish_index].finish.action = .{ .link_rhs_to_lhs = .{ .lhs = a, .rhs = b } };
                    try self.pushSpanPairs(stack, left_named.args, right_named.args, "named type arguments failed Lambda Solved unification");
                }
            },
            .link, .unbound, .forall => unreachable,
        }
    }

    fn applyUnifyFinish(self: *Solver, action: UnifyFinishAction) void {
        switch (action) {
            .none => {},
            .link_rhs_to_lhs => |link| self.program.types.set(link.rhs, .{ .link = link.lhs }),
            .link_var_to_root => |link| self.program.types.set(link.var_, .{ .link = self.program.types.rootCompressed(link.target) }),
            .set_left_erased_link_right => |set| {
                self.program.types.set(set.lhs, .{ .erased = .{
                    .source_fn_ty = set.source_fn_ty,
                    .members = set.members,
                } });
                self.program.types.set(set.rhs, .{ .link = set.lhs });
            },
            .set_left_lambda_set_link_right => |set| {
                self.program.types.set(set.lhs, .{ .lambda_set = set.members });
                self.program.types.set(set.rhs, .{ .link = set.lhs });
            },
            .set_left_tag_union_link_right => |set| {
                self.program.types.set(set.lhs, .{ .tag_union = set.tags });
                self.program.types.set(set.rhs, .{ .link = set.lhs });
            },
        }
    }

    /// Relate the definition-private nominal and opaque interface views of one
    /// checked definition without widening the opaque side's inspectability.
    /// Checking and Monotype have already established the exact TypeDef
    /// identity; Lambda Solved consumes that relation solely to propagate
    /// callable flow through the shared runtime representation.
    fn unifyNominalOpaqueViews(
        self: *Solver,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (!sameMonoTypeDef(left.def, right.def) or
            left.builtin_owner != right.builtin_owner)
        {
            return false;
        }
        const left_is_nominal = left.kind == .nominal;
        const right_is_nominal = right.kind == .nominal;
        const left_is_opaque = left.kind == .@"opaque";
        const right_is_opaque = right.kind == .@"opaque";
        if (!((left_is_nominal and right_is_opaque) or
            (left_is_opaque and right_is_nominal)))
        {
            return false;
        }

        try self.unifySpans(
            left.args,
            right.args,
            "nominal/opaque type arguments failed Lambda Solved unification",
        );
        const left_backing = left.backing orelse
            Common.invariant("nominal/opaque visibility relation lacked a checked runtime backing");
        const right_backing = right.backing orelse
            Common.invariant("nominal/opaque visibility relation lacked a checked runtime backing");
        if (left_backing.authority != .checked_public or
            right_backing.authority != .checked_public)
        {
            Common.invariant("nominal/opaque visibility relation lacked checked-public backing authority");
        }
        if (left_is_nominal and left_backing.use != .inspectable) {
            Common.invariant("definition-private nominal view lacked inspectable backing authority");
        }
        if (right_is_nominal and right_backing.use != .inspectable) {
            Common.invariant("definition-private nominal view lacked inspectable backing authority");
        }
        if (left_is_opaque and left_backing.use != .runtime_layout_only) {
            Common.invariant("opaque interface view carried inspectable backing authority");
        }
        if (right_is_opaque and right_backing.use != .runtime_layout_only) {
            Common.invariant("opaque interface view carried inspectable backing authority");
        }

        try self.unify(left_backing.ty, right_backing.ty);
        if (left_is_opaque) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
    }

    fn unifyPublicNamedBacking(
        self: *Solver,
        backing_ty: Type.TypeVarId,
        named_ty: Type.TypeVarId,
        named_content: Type.Content,
    ) Allocator.Error!bool {
        if (std.meta.activeTag(named_content) != .named) return false;
        const named = named_content.named;
        switch (named.kind) {
            .nominal, .@"opaque" => {},
            .alias => return false,
        }
        const backing = named.backing orelse return false;
        if (backing.authority != .checked_public or backing.use != .inspectable) return false;
        if (!try self.typeIsProvenUninhabited(backing_ty)) return false;
        const backing_root = self.program.types.rootCompressed(backing_ty);
        const named_root = self.program.types.rootCompressed(named_ty);
        if (backing_root != named_root) {
            self.program.types.set(backing_root, .{ .link = named_root });
        }
        return true;
    }

    fn unifyInspectableNamedBacking(
        self: *Solver,
        structural_ty: Type.TypeVarId,
        named_ty: Type.TypeVarId,
        structural_content: Type.Content,
        named_content: Type.Content,
    ) Allocator.Error!bool {
        const structural_tag = std.meta.activeTag(structural_content);
        if (structural_tag == .named or structural_tag == .link or structural_tag == .unbound or structural_tag == .forall) return false;
        if (std.meta.activeTag(named_content) != .named) return false;
        const named = named_content.named;
        if (named.kind == .alias) return false;
        const backing = named.backing orelse return false;
        if (backing.use != .inspectable) return false;

        const moved_structural = try self.program.types.add(structural_content);
        try self.unify(moved_structural, backing.ty);
        const structural_root = self.program.types.rootCompressed(structural_ty);
        const named_root = self.program.types.rootCompressed(named_ty);
        if (structural_root != named_root) {
            self.program.types.set(structural_root, .{ .link = named_root });
        }
        return true;
    }

    fn typeIsProvenUninhabited(self: *Solver, ty: Type.TypeVarId) Allocator.Error!bool {
        var visiting = collections.DenseMap(Type.TypeVarId, void).init(self.allocator);
        defer visiting.deinit();
        return self.typeIsProvenUninhabitedInner(ty, &visiting);
    }

    fn typeIsProvenUninhabitedInner(
        self: *Solver,
        ty: Type.TypeVarId,
        visiting: *collections.DenseMap(Type.TypeVarId, void),
    ) Allocator.Error!bool {
        const root = self.program.types.rootCompressed(ty);
        const entry = try visiting.getOrPut(root);
        if (entry.found_existing) return false;
        defer _ = visiting.remove(root);

        return switch (self.program.types.get(root)) {
            // Probe leaves against the lifted store instead of materializing:
            // uninhabitedness is a pure function of the Monotype.
            .mono => |leaf| blk: {
                var mono_visiting = collections.DenseMap(MonoType.TypeId, void).init(self.allocator);
                defer mono_visiting.deinit();
                break :blk try self.monoProvenUninhabited(leaf.id, &mono_visiting);
            },
            .named => |named| if (named.backing) |backing|
                if (backing.use == .inspectable)
                    self.typeIsProvenUninhabitedInner(backing.ty, visiting)
                else
                    false
            else
                false,
            .tag_union => |tags| blk: {
                if (tags.count() == 0) break :blk true;
                for (0..tags.count()) |tag_index| {
                    const tag = self.program.types.tagItem(tags, tag_index);
                    var tag_inhabited = true;
                    for (0..tag.payloads.count()) |payload_index| {
                        if (try self.typeIsProvenUninhabitedInner(self.program.types.spanItem(tag.payloads, payload_index), visiting)) {
                            tag_inhabited = false;
                            break;
                        }
                    }
                    if (tag_inhabited) break :blk false;
                }
                break :blk true;
            },
            .tuple => |items| blk: {
                for (0..items.count()) |index| {
                    if (try self.typeIsProvenUninhabitedInner(self.program.types.spanItem(items, index), visiting)) break :blk true;
                }
                break :blk false;
            },
            .record => |fields| blk: {
                for (0..fields.count()) |index| {
                    if (try self.typeIsProvenUninhabitedInner(self.program.types.fieldItem(fields, index).ty, visiting)) break :blk true;
                }
                break :blk false;
            },
            .box => |payload| self.typeIsProvenUninhabitedInner(payload, visiting),
            .list, .func, .primitive, .lambda_set, .erased, .zst, .link, .unbound, .forall => false,
        };
    }

    /// `typeIsProvenUninhabitedInner` over the lifted Monotype store, for
    /// lazy leaves that have not materialized.
    fn monoProvenUninhabited(
        self: *Solver,
        id: MonoType.TypeId,
        visiting: *collections.DenseMap(MonoType.TypeId, void),
    ) Allocator.Error!bool {
        const entry = try visiting.getOrPut(id);
        if (entry.found_existing) return false;
        defer _ = visiting.remove(id);

        return switch (self.lifted.types.get(id)) {
            .named => |named| if (named.backing) |backing|
                if (backing.use == .inspectable)
                    self.monoProvenUninhabited(backing.ty, visiting)
                else
                    false
            else
                false,
            .tag_union => |tags| blk: {
                const tag_span = self.lifted.types.tagSpan(tags);
                if (tag_span.len == 0) break :blk true;
                for (tag_span) |tag| {
                    var tag_inhabited = true;
                    for (self.lifted.types.span(tag.payloads)) |payload| {
                        if (try self.monoProvenUninhabited(payload, visiting)) {
                            tag_inhabited = false;
                            break;
                        }
                    }
                    if (tag_inhabited) break :blk false;
                }
                break :blk true;
            },
            .tuple => |items| blk: {
                for (self.lifted.types.span(items)) |item| {
                    if (try self.monoProvenUninhabited(item, visiting)) break :blk true;
                }
                break :blk false;
            },
            .record => |fields| blk: {
                for (self.lifted.types.fieldSpan(fields)) |field| {
                    if (try self.monoProvenUninhabited(field.ty, visiting)) break :blk true;
                }
                break :blk false;
            },
            .box => |payload| self.monoProvenUninhabited(payload, visiting),
            .list, .func, .primitive, .erased, .zst => false,
        };
    }

    /// Transfer Lambda Solved callable evidence from a checked-public value
    /// shape into its producer-authored generated-private representation.
    /// Monotype has already sealed both representations, so this relation
    /// deliberately preserves every composite and named root. Only callable
    /// slots (and still-open Lambda Solved slots) are unified.
    fn relateGeneratedPrivateEvidence(
        self: *Solver,
        public_ty: Type.TypeVarId,
        private_ty: Type.TypeVarId,
    ) Allocator.Error!void {
        const public_root = self.program.types.rootCompressed(public_ty);
        const private_root = self.program.types.rootCompressed(private_ty);
        if (public_root == private_root) return;

        const pair = UnifyPair.init(public_root, private_root);
        const active = try self.active_private_evidence_relations.getOrPut(pair);
        if (active.found_existing) return;
        defer _ = self.active_private_evidence_relations.remove(pair);

        const public = try self.resolvedContentAt(public_root);
        const private = try self.resolvedContentAt(private_root);
        const private_content_tag = std.meta.activeTag(private);
        if (public == .unbound or private == .unbound or
            public == .lambda_set or private == .lambda_set or
            public == .erased or private == .erased)
        {
            try self.unify(public_root, private_root);
            return;
        }

        switch (public) {
            .link, .unbound, .lambda_set, .erased => unreachable,
            .mono => Common.invariant("lazy Monotype leaf reached the generated-private evidence relation unexpanded"),
            .forall => Common.invariant("generated-private evidence relation received a generalized public type"),
            .primitive => |public_primitive| {
                if (private_content_tag != .primitive) Common.invariant("generated-private evidence relation received different type structure");
                if (public_primitive != private.primitive) Common.invariant("generated-private evidence relation received different primitive types");
            },
            .zst => if (private != .zst) Common.invariant("generated-private evidence relation received different type structure"),
            .list => |public_elem| {
                if (private_content_tag != .list) Common.invariant("generated-private evidence relation received different type structure");
                try self.relateGeneratedPrivateEvidence(public_elem, private.list);
            },
            .box => |public_elem| {
                if (private_content_tag != .box) Common.invariant("generated-private evidence relation received different type structure");
                try self.relateGeneratedPrivateEvidence(public_elem, private.box);
            },
            .tuple => |public_items| {
                if (private_content_tag != .tuple) Common.invariant("generated-private evidence relation received different type structure");
                const private_items = private.tuple;
                if (public_items.count() != private_items.count()) {
                    Common.invariant("generated-private evidence relation received tuples of different arity");
                }
                for (0..public_items.count()) |index| {
                    try self.relateGeneratedPrivateEvidence(
                        self.program.types.spanItem(public_items, index),
                        self.program.types.spanItem(private_items, index),
                    );
                }
            },
            .record => |public_fields| {
                if (private_content_tag != .record) Common.invariant("generated-private evidence relation received different type structure");
                const private_fields = private.record;
                if (public_fields.count() != private_fields.count()) {
                    Common.invariant("generated-private evidence relation received records with different fields");
                }
                for (0..public_fields.count()) |index| {
                    const public_field = self.program.types.fieldItem(public_fields, index);
                    const private_field = self.program.types.fieldItem(private_fields, index);
                    if (public_field.name != private_field.name) {
                        Common.invariant("generated-private evidence relation received records with different fields");
                    }
                    try self.relateGeneratedPrivateEvidence(public_field.ty, private_field.ty);
                    if ((public_field.value_ty == null) != (private_field.value_ty == null)) {
                        Common.invariant("generated-private evidence relation received different record field kinds");
                    }
                    if (public_field.value_ty) |public_value_ty| {
                        try self.relateGeneratedPrivateEvidence(public_value_ty, private_field.value_ty.?);
                    }
                }
            },
            .tag_union => |public_tags| {
                if (private_content_tag != .tag_union) Common.invariant("generated-private evidence relation received different type structure");
                const private_tags = private.tag_union;
                if (public_tags.count() != private_tags.count()) {
                    Common.invariant("generated-private evidence relation received tag unions with different tags");
                }
                for (0..public_tags.count()) |tag_index| {
                    const public_tag = self.program.types.tagItem(public_tags, tag_index);
                    const private_tag = self.program.types.tagItem(private_tags, tag_index);
                    if (public_tag.name != private_tag.name or public_tag.checked_name != private_tag.checked_name or
                        public_tag.payloads.count() != private_tag.payloads.count())
                    {
                        Common.invariant("generated-private evidence relation received tag unions with different tags");
                    }
                    for (0..public_tag.payloads.count()) |payload_index| {
                        try self.relateGeneratedPrivateEvidence(
                            self.program.types.spanItem(public_tag.payloads, payload_index),
                            self.program.types.spanItem(private_tag.payloads, payload_index),
                        );
                    }
                }
            },
            .func => |public_fn| {
                if (private_content_tag != .func) Common.invariant("generated-private evidence relation received different type structure");
                const private_fn = private.func;
                if (public_fn.args.count() != private_fn.args.count()) {
                    Common.invariant("generated-private evidence relation received functions of different arity");
                }
                for (0..public_fn.args.count()) |index| {
                    try self.relateGeneratedPrivateEvidence(
                        self.program.types.spanItem(public_fn.args, index),
                        self.program.types.spanItem(private_fn.args, index),
                    );
                }
                try self.unify(public_fn.callable, private_fn.callable);
                try self.relateGeneratedPrivateEvidence(public_fn.ret, private_fn.ret);
            },
            .named => |public_named| {
                if (private_content_tag != .named) Common.invariant("generated-private evidence relation received different type structure");
                const private_named = private.named;
                const same_identity = public_named.kind == private_named.kind and
                    std.meta.eql(public_named.def, private_named.def) and
                    public_named.builtin_owner == private_named.builtin_owner;
                if (!same_identity and MonoType.iteratorRelation(public_named, private_named) == .ordinary) {
                    Common.invariant("generated-private evidence relation received different named types");
                }
                if (same_identity) {
                    if (public_named.args.count() != private_named.args.count()) {
                        Common.invariant("generated-private evidence relation received named types with different arity");
                    }
                    for (0..public_named.args.count()) |index| {
                        try self.relateGeneratedPrivateEvidence(
                            self.program.types.spanItem(public_named.args, index),
                            self.program.types.spanItem(private_named.args, index),
                        );
                    }
                    if (public_named.backing) |public_backing| {
                        const private_backing = private_named.backing orelse
                            Common.invariant("generated-private evidence relation received different named backing presence");
                        if (public_backing.use != private_backing.use) {
                            Common.invariant("generated-private evidence relation received different named backing uses");
                        }
                        try self.relateGeneratedPrivateEvidence(public_backing.ty, private_backing.ty);
                    } else if (private_named.backing != null) {
                        Common.invariant("generated-private evidence relation received different named backing presence");
                    }
                } else {
                    if (public_named.args.count() == 0 or private_named.args.count() == 0) {
                        Common.invariant("generated-private iterator evidence lacked a public item argument");
                    }
                    try self.relateGeneratedPrivateEvidence(
                        self.program.types.spanItem(public_named.args, 0),
                        self.program.types.spanItem(private_named.args, 0),
                    );
                }
            },
        }
    }

    fn unifyIteratorOwnerStampedPublic(
        self: *Solver,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (left.kind != right.kind) return false;
        if (!sameMonoTypeDef(left.def, right.def)) return false;
        _ = iteratorLikeOwnerFromPair(left.builtin_owner, right.builtin_owner) orelse return false;
        if (left.builtin_owner == right.builtin_owner) return false;

        try self.unifySpans(left.args, right.args, "iterator owner-stamp argument lists failed Lambda Solved unification");
        if (isIteratorLikeOwner(left.builtin_owner)) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
    }

    fn unifyForcedDynamicIterator(
        self: *Solver,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (MonoType.iteratorRelation(left, right) != .forced_dynamic) return false;

        const left_dynamic = left.def.iterator_representation == .forced_dynamic;
        if (left.args.count() == 0 or right.args.count() == 0) {
            Common.invariant("forced-dynamic iterator reached Lambda Solved without a public item argument");
        }

        try self.unify(self.program.types.spanItem(left.args, 0), self.program.types.spanItem(right.args, 0));
        const dynamic = if (left_dynamic) left else right;
        const other = if (left_dynamic) right else left;
        switch (other.def.iterator_representation) {
            .none => try self.relateForcedDynamicPublicEvidence(dynamic, other),
            .minted => try self.unifyGeneratedIteratorBackings(left, right),
            .forced_dynamic => Common.invariant("forced-dynamic iterator relation received two dynamic representations"),
        }
        if (left_dynamic) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
    }

    fn relateForcedDynamicPublicEvidence(self: *Solver, dynamic: anytype, public: anytype) Allocator.Error!void {
        const public_backing = public.backing orelse return;
        const dynamic_backing = dynamic.backing orelse
            Common.invariant("forced-dynamic iterator relation found dynamic backing on only one side");
        if (public_backing.use != dynamic_backing.use) {
            Common.invariant("forced-dynamic iterator relation found different backing uses");
        }
        if (public_backing.authority != .checked_public or dynamic_backing.authority != .generated_private) {
            Common.invariant("forced-dynamic iterator evidence relation received incorrect backing authority");
        }
        try self.relateGeneratedPrivateEvidence(public_backing.ty, dynamic_backing.ty);
    }

    fn unifyGeneratedIteratorJoin(
        self: *Solver,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (MonoType.iteratorRelation(left, right) != .minted_join) return false;

        if (left.args.count() == 0 or right.args.count() == 0) {
            Common.invariant("generated iterator join reached Lambda Solved without a public item argument");
        }
        try self.unify(self.program.types.spanItem(left.args, 0), self.program.types.spanItem(right.args, 0));

        if (left.backing) |left_backing| {
            const right_backing = right.backing orelse
                Common.invariant("generated iterator join found backing on only one side");
            if (left_backing.use != right_backing.use) {
                Common.invariant("generated iterator join found different backing uses");
            }
            if (left_backing.authority != right_backing.authority) {
                Common.invariant("generated iterator join found different backing authorities");
            }
            try self.unify(left_backing.ty, right_backing.ty);
        } else if (right.backing != null) {
            Common.invariant("generated iterator join found backing on only one side");
        }

        if (isIteratorLikeOwner(left.builtin_owner)) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
    }

    fn unifyPublicGeneratedIterator(
        self: *Solver,
        left_ty: Type.TypeVarId,
        right_ty: Type.TypeVarId,
        left: anytype,
        right: anytype,
    ) Allocator.Error!bool {
        if (MonoType.iteratorRelation(left, right) != .public_minted) return false;

        if (left.args.count() == 0 or right.args.count() == 0) {
            Common.invariant("generated iterator evidence reached Lambda Solved without a public item argument");
        }
        try self.unify(self.program.types.spanItem(left.args, 0), self.program.types.spanItem(right.args, 0));

        const left_minted = left.def.iterator_representation == .minted;
        const generated = if (left_minted) left else right;
        const public = if (left_minted) right else left;
        if (public.backing) |public_backing| {
            const generated_backing = generated.backing orelse
                Common.invariant("generated iterator evidence had no private backing");
            if (generated_backing.authority != .generated_private) {
                Common.invariant("generated iterator evidence backing lacked private authority");
            }
            try self.relateGeneratedPrivateEvidence(public_backing.ty, generated_backing.ty);
        }

        if (left_minted) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
    }

    fn unifyGeneratedIteratorBackings(self: *Solver, left: anytype, right: anytype) Allocator.Error!void {
        const left_backing = left.backing orelse
            Common.invariant("generated iterator relation found backing on only one side");
        const right_backing = right.backing orelse
            Common.invariant("generated iterator relation found backing on only one side");
        if (left_backing.use != right_backing.use) {
            Common.invariant("generated iterator relation found different backing uses");
        }
        if (left_backing.authority != .generated_private or right_backing.authority != .generated_private) {
            Common.invariant("private iterator relation received a checked-public backing");
        }
        try self.unify(left_backing.ty, right_backing.ty);
    }

    fn transparentAliasBacking(content: Type.Content) ?Type.TypeVarId {
        if (std.meta.activeTag(content) != .named or content.named.kind != .alias) return null;
        return (content.named.backing orelse Common.invariant("transparent alias reached Lambda Solved without a backing type")).ty;
    }

    fn unifySpans(self: *Solver, lhs: Type.Span, rhs: Type.Span, comptime message: []const u8) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant(message);
        for (0..lhs.count()) |i| {
            const left_ty = self.program.types.spanItem(lhs, i);
            const right_ty = self.program.types.spanItem(rhs, i);
            try self.unify(left_ty, right_ty);
        }
    }

    /// Push one `process` frame per span element, in reverse so the stack
    /// pops them in span order.
    fn pushSpanPairs(
        self: *Solver,
        stack: *std.ArrayList(UnifyFrame),
        lhs: Type.Span,
        rhs: Type.Span,
        comptime message: []const u8,
    ) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant(message);
        var i = lhs.count();
        while (i > 0) {
            i -= 1;
            const left_ty = self.program.types.spanItem(lhs, i);
            const right_ty = self.program.types.spanItem(rhs, i);
            try self.pushUnifyPair(stack, left_ty, right_ty);
        }
    }

    fn pushFieldPairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant("record field count failed Lambda Solved unification");
        var i = lhs.count();
        while (i > 0) {
            i -= 1;
            const left_field = self.program.types.fieldItem(lhs, i);
            const right_field = self.program.types.fieldItem(rhs, i);
            if (left_field.name != right_field.name) Common.invariant("record field order failed Lambda Solved unification");
            try self.pushUnifyPair(stack, left_field.ty, right_field.ty);
            if ((left_field.value_ty == null) != (right_field.value_ty == null)) {
                Common.invariant("record field kind failed Lambda Solved unification");
            }
            if (left_field.value_ty) |left_value_ty| {
                try self.pushUnifyPair(stack, left_value_ty, right_field.value_ty.?);
            }
        }
    }

    fn pushPayloadSpanPairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), pairs: []const DeferredSpanPair) Allocator.Error!void {
        var i = pairs.len;
        while (i > 0) {
            i -= 1;
            try self.pushSpanPairs(stack, pairs[i].lhs, pairs[i].rhs, "tag payload count failed Lambda Solved unification");
        }
    }

    fn pushCaptureSpanPairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), pairs: []const DeferredSpanPair) Allocator.Error!void {
        var i = pairs.len;
        while (i > 0) {
            i -= 1;
            try self.pushCapturePairs(stack, pairs[i].lhs, pairs[i].rhs);
        }
    }

    fn pushCapturePairs(self: *Solver, stack: *std.ArrayList(UnifyFrame), lhs: Type.Span, rhs: Type.Span) Allocator.Error!void {
        if (lhs.count() != rhs.count()) Common.invariant("capture count failed Lambda Solved unification");
        var i = lhs.count();
        while (i > 0) {
            i -= 1;
            const left_capture = self.program.types.captureItem(lhs, i);
            const right_capture = self.program.types.captureItem(rhs, i);
            if (left_capture.capture_id != right_capture.capture_id) {
                Common.invariant("capture identity failed Lambda Solved unification");
            }
            try self.pushUnifyPair(stack, left_capture.ty, right_capture.ty);
        }
    }

    /// Merge two tag unions, collecting the shared tags' payload spans for the
    /// caller to unify once the merged span has been recorded.
    fn mergeTags(
        self: *Solver,
        lhs: Type.Span,
        rhs: Type.Span,
        payload_pairs: *std.ArrayList(DeferredSpanPair),
    ) Allocator.Error!Type.Span {
        var merged = std.ArrayList(Type.Tag).empty;
        defer merged.deinit(self.allocator);
        var shared_count: usize = 0;

        for (0..lhs.count()) |left_index| {
            const left_tag = self.program.types.tagItem(lhs, left_index);
            try merged.append(self.allocator, left_tag);
            for (0..rhs.count()) |right_index| {
                const right_tag = self.program.types.tagItem(rhs, right_index);
                if (left_tag.name != right_tag.name) continue;
                try payload_pairs.append(self.allocator, .{
                    .lhs = left_tag.payloads,
                    .rhs = right_tag.payloads,
                });
                shared_count += 1;
                break;
            }
        }

        if (shared_count == 0) Common.invariant("disjoint tag unions failed Lambda Solved unification");

        for (0..rhs.count()) |right_index| {
            const right_tag = self.program.types.tagItem(rhs, right_index);
            for (0..lhs.count()) |left_index| {
                if (self.program.types.tagItem(lhs, left_index).name == right_tag.name) break;
            } else {
                try merged.append(self.allocator, right_tag);
            }
        }

        return try self.program.types.addTags(merged.items);
    }

    /// Merge two lambda sets, collecting the shared members' capture spans for
    /// the caller to unify once the merged span has been recorded.
    fn mergeLambdaSets(
        self: *Solver,
        lhs: Type.Span,
        rhs: Type.Span,
        capture_pairs: *std.ArrayList(DeferredSpanPair),
    ) Allocator.Error!Type.Span {
        var members = std.ArrayList(Type.FnMember).empty;
        defer members.deinit(self.allocator);

        for (0..lhs.count()) |i| try members.append(self.allocator, self.program.types.memberItem(lhs, i));

        for (0..rhs.count()) |i| {
            const right_member = self.program.types.memberItem(rhs, i);
            var found = false;
            for (members.items) |left_member| {
                if (left_member.lambda != right_member.lambda) continue;
                found = true;
                try capture_pairs.append(self.allocator, .{
                    .lhs = left_member.captures,
                    .rhs = right_member.captures,
                });
                break;
            }
            if (!found) try members.append(self.allocator, right_member);
        }

        return try self.program.types.addMembers(members.items);
    }

    fn solvedTypeDigest(self: *Solver, ty: Type.TypeVarId) Allocator.Error!Type.names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        var active = collections.DenseMap(Type.TypeVarId, void).init(self.allocator);
        defer active.deinit();
        try self.writeSolvedTypeDigest(&hasher, ty, &active);
        return .{ .bytes = hasher.finalResult() };
    }

    fn writeSolvedTypeDigest(
        self: *Solver,
        hasher: *std.crypto.hash.sha2.Sha256,
        ty: Type.TypeVarId,
        active: *collections.DenseMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        const root = self.program.types.rootCompressed(ty);
        if (active.contains(root)) {
            writeBytes(hasher, "cycle");
            writeU32(hasher, @intFromEnum(root));
            return;
        }
        try active.put(root, {});
        defer _ = active.remove(root);

        switch (try self.resolvedContentAt(root)) {
            .mono => Common.invariant("lazy Monotype leaf reached digest hashing unexpanded"),
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
                writeU32(hasher, @intCast(fields.count()));
                for (0..fields.count()) |index| {
                    const field = self.program.types.fieldItem(fields, index);
                    writeBytes(hasher, self.lifted.names.recordFieldLabelText(field.name));
                    MonoType.writeFieldDefaultDigest(self.lifted.names, hasher, field.default);
                    if (field.value_ty) |value_ty| {
                        writeBytes(hasher, "field-optional-value");
                        try self.writeSolvedTypeDigest(hasher, value_ty, active);
                    } else {
                        writeBytes(hasher, "field-inline-value");
                    }
                    try self.writeSolvedTypeDigest(hasher, field.ty, active);
                }
            },
            .tag_union => |tags| {
                writeBytes(hasher, "tag_union");
                writeU32(hasher, @intCast(tags.count()));
                for (0..tags.count()) |index| {
                    const tag = self.program.types.tagItem(tags, index);
                    writeBytes(hasher, self.lifted.names.tagLabelText(tag.name));
                    try self.writeSolvedTypeSpanDigest(hasher, tag.payloads, active);
                }
            },
            .named => |named| {
                writeBytes(hasher, "named");
                hasher.update(&named.named_type.module.bytes);
                writeBytes(hasher, self.lifted.names.moduleIdentityBytes(named.def.module));
                writeOptionalU32(hasher, named.def.source_decl);
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
                writeU32(hasher, @intCast(members.count()));
                for (0..members.count()) |member_index| {
                    const member = self.program.types.memberItem(members, member_index);
                    writeU32(hasher, @intFromEnum(member.lambda));
                    writeU32(hasher, @intCast(member.captures.count()));
                    for (0..member.captures.count()) |capture_index| {
                        const capture = self.program.types.captureItem(member.captures, capture_index);
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
        active: *collections.DenseMap(Type.TypeVarId, void),
    ) Allocator.Error!void {
        writeU32(hasher, @intCast(span.count()));
        for (0..span.count()) |index| {
            const child = self.program.types.spanItem(span, index);
            try self.writeSolvedTypeDigest(hasher, child, active);
        }
    }
};

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeOptionalU32(hasher: *std.crypto.hash.sha2.Sha256, value: ?u32) void {
    if (value) |v| {
        hasher.update(&[_]u8{1});
        writeU32(hasher, v);
    } else {
        hasher.update(&[_]u8{0});
    }
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

const ReachabilityMasks = struct {
    contains_callable: []bool,
    contains_forced_dynamic: []bool,
};

/// Reverse-reachability over the lifted Monotype store: from `func` and
/// `erased` nodes (types whose clones need fresh callable slots) and from
/// forced-dynamic iterator named nodes (leaves the forced-dynamic scan must
/// materialize).
fn computeReachabilityMasks(allocator: Allocator, types: anytype) Allocator.Error!ReachabilityMasks {
    const count = types.types.len;
    const flags = try allocator.alloc(bool, count);
    errdefer allocator.free(flags);
    @memset(flags, false);
    const forced = try allocator.alloc(bool, count);
    errdefer allocator.free(forced);
    @memset(forced, false);

    const edge_counts = try allocator.alloc(u32, count);
    defer allocator.free(edge_counts);
    @memset(edge_counts, 0);

    const Walk = struct {
        fn children(store: @TypeOf(types), content: MonoType.Content, callback: anytype) void {
            switch (content) {
                .primitive, .zst, .erased => {},
                .list, .box => |elem| callback.child(elem),
                .tuple => |items| for (store.span(items)) |item| callback.child(item),
                .record => |fields| for (store.fieldSpan(fields)) |field| {
                    callback.child(field.ty);
                    if (field.value_ty) |value_ty| callback.child(value_ty);
                },
                .tag_union => |tags| for (store.tagSpan(tags)) |tag| {
                    for (store.span(tag.payloads)) |payload| callback.child(payload);
                },
                .named => |named| {
                    for (store.span(named.args)) |arg| callback.child(arg);
                    if (named.backing) |backing| callback.child(backing.ty);
                    for (store.declaredFieldSpan(named.declared_order)) |declared| switch (declared) {
                        .named => {},
                        .padding => |padding_ty| callback.child(padding_ty),
                    };
                },
                .func => |func| {
                    for (store.span(func.args)) |arg| callback.child(arg);
                    callback.child(func.ret);
                },
            }
        }
    };

    for (types.types) |content| {
        const Counter = struct {
            counts: []u32,
            fn child(self: @This(), ty: MonoType.TypeId) void {
                self.counts[@intFromEnum(ty)] += 1;
            }
        };
        Walk.children(types, content, Counter{ .counts = edge_counts });
    }

    var parent_starts = try allocator.alloc(u32, count + 1);
    defer allocator.free(parent_starts);
    parent_starts[0] = 0;
    for (edge_counts, 0..) |edge_count, index| {
        parent_starts[index + 1] = parent_starts[index] + edge_count;
    }
    const parents = try allocator.alloc(u32, parent_starts[count]);
    defer allocator.free(parents);
    const parent_writes = try allocator.dupe(u32, parent_starts[0..count]);
    defer allocator.free(parent_writes);
    for (types.types, 0..) |content, parent_index| {
        const Filler = struct {
            parents: []u32,
            writes: []u32,
            parent: u32,
            fn child(self: @This(), ty: MonoType.TypeId) void {
                const child_index = @intFromEnum(ty);
                self.parents[self.writes[child_index]] = self.parent;
                self.writes[child_index] += 1;
            }
        };
        Walk.children(types, content, Filler{ .parents = parents, .writes = parent_writes, .parent = @intCast(parent_index) });
    }

    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    for (types.types, 0..) |content, index| {
        const tag = std.meta.activeTag(content);
        if (tag == .func or tag == .erased) {
            flags[index] = true;
            try work.append(allocator, @intCast(index));
        }
    }
    while (work.pop()) |index| {
        for (parents[parent_starts[index]..parent_starts[index + 1]]) |parent| {
            if (flags[parent]) continue;
            flags[parent] = true;
            try work.append(allocator, parent);
        }
    }
    for (types.types, 0..) |content, index| {
        if (std.meta.activeTag(content) == .named) {
            if (content.named.def.iterator_representation == .forced_dynamic) {
                forced[index] = true;
                try work.append(allocator, @intCast(index));
            }
        }
    }
    while (work.pop()) |index| {
        for (parents[parent_starts[index]..parent_starts[index + 1]]) |parent| {
            if (forced[parent]) continue;
            forced[parent] = true;
            try work.append(allocator, parent);
        }
    }
    return .{ .contains_callable = flags, .contains_forced_dynamic = forced };
}

const TypeCloner = struct {
    solver: *Solver,
    map: collections.DenseMap(MonoType.TypeId, Type.TypeVarId),
    /// Unification rewrites var contents in place (alias backings, named
    /// absorption, uninhabited links), so clones that can still reach `unify`
    /// must stay per-use. After solving no var is unified again, and clones of
    /// callable-free types carry no unbound slots, so those may share one var
    /// per Monotype during finalization.
    share: bool = false,
    /// One-level mode: children lower to lazy leaves in this clone context
    /// instead of eager clones, reusing the context's existing var when the
    /// Monotype already occurs in the tree. Used by `expandMonoRoot`.
    lazy_ctx: ?u32 = null,

    fn init(solver: *Solver) TypeCloner {
        return .{
            .solver = solver,
            .map = collections.DenseMap(MonoType.TypeId, Type.TypeVarId).init(solver.allocator),
        };
    }

    fn deinit(self: *TypeCloner) void {
        self.map.deinit();
    }

    fn lower(self: *TypeCloner, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        if (self.lazy_ctx) |ctx| {
            const map = &self.solver.leaf_contexts.items[ctx];
            if (map.get(ty)) |existing| return existing;
            const created = try self.solver.program.types.add(.{ .mono = .{ .id = ty, .ctx = ctx } });
            try map.put(ty, created);
            return created;
        }
        if (self.map.get(ty)) |cached| return cached;
        const shareable = self.share and !self.solver.contains_callable[@intFromEnum(ty)];
        if (shareable) {
            if (self.solver.shared_clones.get(ty)) |shared| {
                try self.map.put(ty, shared);
                return shared;
            }
        }
        const reserved = try self.solver.program.types.add(.unbound);
        try self.map.put(ty, reserved);
        self.solver.program.types.set(reserved, try self.lowerContent(self.solver.lifted.types.get(ty)));
        if (shareable) try self.solver.shared_clones.put(ty, reserved);
        return reserved;
    }

    /// Apply the explicit dynamic boundary only after the entire requested
    /// Monotype clone is complete. A forced iterator can be reached while an
    /// enclosing function or payload clone still holds reservations, so doing
    /// this per-node would let callable identity observe an unfinished graph.
    fn markForcedDynamicCallables(self: *TypeCloner) Allocator.Error!void {
        var entries = self.map.iterator();
        while (entries.next()) |entry| {
            const content = self.solver.lifted.types.get(entry.key_ptr.*);
            if (std.meta.activeTag(content) == .named) {
                if (content.named.def.iterator_representation == .forced_dynamic) {
                    try self.solver.markErasedCallablesReachedByType(entry.value_ptr.*);
                }
            }
        }
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
                        .value_ty = if (field.value_ty) |value_ty| try self.lower(value_ty) else null,
                        .default = field.default,
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
                        const backing_ty = if (raw_backing.authority == .generated_private)
                            raw_backing.ty
                        else
                            try self.structuralBackingForNamed(named.def, raw_backing.ty);
                        break :blk_backing .{
                            .ty = try self.lower(backing_ty),
                            .use = raw_backing.use,
                            .authority = raw_backing.authority,
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
        var seen = collections.DenseMap(MonoType.TypeId, void).init(self.solver.allocator);
        defer seen.deinit();
        var current = backing;
        while (true) {
            if (seen.contains(current)) return current;
            try seen.put(current, {});
            const content = self.solver.lifted.types.get(current);
            if (std.meta.activeTag(content) != .named) return current;
            if (content.named.kind != .alias and !sameMonoTypeDef(content.named.def, owner_def)) return current;
            const next = content.named.backing orelse return current;
            current = next.ty;
        }
    }
};

fn sameMonoTypeDef(left: MonoType.TypeDef, right: MonoType.TypeDef) bool {
    return left.module == right.module and
        left.type_name == right.type_name and
        left.source_decl == right.source_decl and
        optionalDigestEql(left.generated, right.generated) and
        left.iterator_representation == right.iterator_representation and
        left.iterator_kind == right.iterator_kind and
        left.iterator_depth == right.iterator_depth and
        std.meta.eql(left.iterator_topology, right.iterator_topology);
}

fn iteratorLikeOwnerFromPair(
    left: ?static_dispatch.BuiltinOwner,
    right: ?static_dispatch.BuiltinOwner,
) ?static_dispatch.BuiltinOwner {
    if (left) |left_owner| {
        if (!isIteratorLikeOwner(left_owner)) return null;
        if (right) |right_owner| {
            if (left_owner != right_owner) return null;
        }
        return left_owner;
    }
    if (right) |right_owner| {
        if (!isIteratorLikeOwner(right_owner)) return null;
        return right_owner;
    }
    return null;
}

fn isIteratorLikeOwner(owner: ?static_dispatch.BuiltinOwner) bool {
    return static_dispatch.isIteratorOwner(owner orelse return false);
}

fn optionalDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
    if (left == null and right == null) return true;
    if (left == null or right == null) return false;
    return std.mem.eql(u8, left.?.bytes[0..], right.?.bytes[0..]);
}

test "lambda solved erased callable digest includes record field default identity" {
    const gpa = std.testing.allocator;

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const field_name = try name_store.internRecordFieldLabel("retries");
    const module = try name_store.internModuleIdentity(&([_]u8{0xD5} ** 32));

    var program: Ast.Program = undefined;
    program.types = Type.Store.init(gpa);
    defer program.types.deinit();

    const value_ty = try program.types.add(.{ .primitive = .u8 });
    const plain_ty = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = value_ty,
        .default = null,
    }}) });
    const first_default_ty = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = value_ty,
        .default = .{ .module = module, .expr_node = 3 },
    }}) });
    const second_default_ty = try program.types.add(.{ .record = try program.types.addFields(&.{.{
        .name = field_name,
        .ty = value_ty,
        .default = .{ .module = module, .expr_node = 4 },
    }}) });

    var lifted: Lifted.ProgramView = undefined;
    lifted.names = &name_store;
    var solver: Solver = undefined;
    solver.allocator = gpa;
    solver.program = &program;
    solver.lifted = lifted;

    const plain_digest = try solver.solvedTypeDigest(plain_ty);
    const first_default_digest = try solver.solvedTypeDigest(first_default_ty);
    const second_default_digest = try solver.solvedTypeDigest(second_default_ty);
    try std.testing.expect(!std.mem.eql(u8, plain_digest.bytes[0..], first_default_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, first_default_digest.bytes[0..], second_default_digest.bytes[0..]));
}

test "lambda solved solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
