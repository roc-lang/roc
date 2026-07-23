//! Lambda solving over lifted Monotype IR.
//!
//! This stage computes callable flow: the finite set of concrete lambdas that
//! can reach each function-typed position. That flow appears nowhere in the
//! checked module, so unlike the Monotype stage this solver derives its domain
//! for the first time rather than re-deriving a checked one. It is the sole
//! general unifier kept after checking, exempt by design (reunify.md section
//! 12.1), and its architecture — a close port of the cor `lss` lambdasolved
//! experiment — does not change. The load-bearing invariants (reunify.md
//! section 12):
//!
//! - Sets live inside types. Every `func` node carries a third slot, its
//!   `callable`, alongside args and ret; a lambda set buried in a record field,
//!   tag payload, or list element propagates by ordinary structural traversal.
//!   The solver builds enriched types, it does not read a side table.
//! - Set agreement is equality closure, never directed subset flow. A set
//!   determines the tag-union layout of its closures, so the producer and
//!   consumer of one runtime value must share the same set; one-way propagation
//!   would permit two layouts for one value and would need re-tagging coercions
//!   on every edge to stay sound. Merging slots by equality is union-find, i.e.
//!   unification (`unify`, `mergeLambdaSets` keyed by lambda symbol,
//!   `unifyCaptures` pointwise with a hard capture-identity invariant).
//! - Erasure infects both directions. A consumer that boxes a callable erases
//!   the producer's construction site too, so `markErasedCallablesReachedByType`
//!   unifies a minted erased node into every callable slot reached as data, and
//!   erased absorbs a lambda set from either side. Still-unbound slots — never
//!   called, never stored — seal to the empty set (`closeCallableSlot`).
//! - Downstream identity depends on merged roots. `FnSpec` deduplicates
//!   procedures on the rooted solved function type var, so the merged
//!   equivalence class is the specialization identity.
//! - The solver never generalizes. `Content.forall` is never constructed; it
//!   exists only as an invariant trap. Each lifted function gets exactly one
//!   solved type, and every use unifies against that same var. Where cor mints
//!   two specializations of a polymorphic `id`, roc pools both closures into one
//!   merged set and one procedure: coarser but self-consistent, because every
//!   connected position shares one equivalence class and one layout. This is a
//!   deliberate divergence from cor's per-use instantiation, not a defect;
//!   lambda-set polymorphism is a separate design effort if ever wanted, so do
//!   not "fix" it in either direction here.
//! - Monotype structural identity never implies callable-flow identity. Types
//!   enter through `lowerTypeFresh`, whose cloner keeps an active-path map (not
//!   a completed-graph memo): a reservation is reused only by a genuine
//!   recursive back-edge, and a later non-recursive occurrence of one monotype
//!   id clones fresh with its own callable slot. Two structurally identical
//!   function types therefore get distinct slots; slots become equal only
//!   through recursion or an explicit value-flow unification. Callable-free
//!   subgraphs may still share a completed clone, behind a
//!   `containsCallableOccurrence` proof. This lands before the Monotype store is
//!   interned, so interning cannot silently coarsen sets (reunify.md section
//!   12.5).
//!
//! Beyond the callable slots, the structural walk makes a fixed, inventoried
//! set of non-callable decisions — the empty-tag-union tie-break, backed-alias
//! unwrapping (`transparentAliasBacking`), score-selected generated-evidence
//! backings, four iterator nominal-identity joins, named backing authority, and
//! the erasure pass's iterator-backing exemption. That inventory is the census
//! in reunify.md section 12.4 item 5. The Debug-only `seamResidualShapesAgree`
//! checkpoint asserts that, past every census relation, both seam sides share a
//! content constructor, so Monotype drift during its migration is caught at the
//! seam; the census identifiers are also pinned by line count in
//! `ci/check_reunify_manifest.pl`, so a new special relation must be classified
//! there and in the census before it can land. The field pair deliberately
//! outside the seam contract is a named type's layout-only `declared_order` and
//! `named_type`, which the close and erase passes also skip.

const std = @import("std");
const can = @import("can");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");
const census = @import("../monotype/census.zig");
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
    /// Memoized `containsCallableOccurrence` results, keyed by monotype id.
    /// The predicate is a pure function of the immutable lifted monotype
    /// graph, so it is cached once on the solver and reused by every cloning
    /// call rather than recomputed per position.
    callable_occurrence_memo: std.AutoHashMap(MonoType.TypeId, bool),

    /// Result of one `containsCallableOccurrence` sub-walk. `touched_active`
    /// records whether the walk leaned on an id still open higher on the DFS
    /// stack; such a result may be incomplete, so only fully-resolved results
    /// (`touched_active == false`) are cached.
    const CallablePresence = struct {
        present: bool,
        touched_active: bool,
    };

    const FunctionShape = struct {
        args: Type.Span,
        callable: Type.TypeVarId,
        ret: Type.TypeVarId,
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
            .callable_occurrence_memo = std.AutoHashMap(MonoType.TypeId, bool).init(allocator),
        };
    }

    fn deinit(self: *Solver) void {
        self.callable_occurrence_memo.deinit();
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
                .const_locator = request.const_locator,
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
            try self.program.expr_tys.append(self.allocator, self.program.types.rootCompressed(ty));
        }

        try self.program.pat_tys.ensureTotalCapacity(self.allocator, self.pat_tys.len);
        for (self.pat_tys, 0..) |maybe_ty, index| {
            const ty = maybe_ty orelse try self.lowerTypeFresh(self.lifted.pats[index].ty);
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
        const fn_ty = self.program.types.rootContentCompressed(self.program.fn_tys.items[raw]);
        return switch (fn_ty) {
            .func => |func| func.ret,
            else => Common.invariant("Lambda Solved layout request referenced a non-function"),
        };
    }

    fn solveFn(self: *Solver, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!void {
        const fn_ty = self.program.fn_tys.items[@intFromEnum(fn_id)];
        const fn_content = self.program.types.rootContentCompressed(fn_ty);
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
            switch (self.program.types.get(ty)) {
                .link => {},
                else => try self.closeCallableSlotsInType(ty, done, active),
            }
        }
    }

    fn closeCallableSlotsInType(
        self: *Solver,
        ty: Type.TypeVarId,
        done: []bool,
        active: []bool,
    ) Allocator.Error!void {
        const root = self.program.types.rootCompressed(ty);
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
        const root = self.program.types.rootCompressed(callable);
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
            .static_data_candidate => |candidate| _ = try self.expectExpr(candidate.runtime_expr, expected),
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
                const field_ty = try self.recordField(receiver_ty, field.field);
                try self.unify(expected, field_ty);
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
            .static_data_candidate => |candidate| {
                _ = try self.exprSlot(expr_id);
                self.expr_done[index] = true;
                try self.inferGeneratedOpaqueBacking(candidate.runtime_expr);
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
        const generated = self.program.types.rootCompressed(generated_ty);
        const expected = self.program.types.rootCompressed(expected_ty);
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
            census.bump("lambda_generated_backing_equal_score");
            try self.unify(generated, expected);
        }
    }

    fn expectExpr(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const slot = try self.expectExprSlot(expr_id, expected);
        const inferred = try self.inferExpr(expr_id);
        try self.unify(slot, inferred);
        try self.expectGeneratedIteratorBackingExpr(expr_id, expected, slot, inferred);
        return self.program.types.rootCompressed(slot);
    }

    fn expectGeneratedIteratorBackingExpr(
        self: *Solver,
        expr_id: Lifted.ExprId,
        expected: Type.TypeVarId,
        slot: Type.TypeVarId,
        inferred: Type.TypeVarId,
    ) Allocator.Error!void {
        const backing_ty = self.generatedIteratorBacking(expected) orelse
            self.generatedIteratorBacking(slot) orelse
            self.generatedIteratorBacking(inferred) orelse
            return;
        switch (self.lifted.exprs[@intFromEnum(expr_id)].data) {
            .record,
            .tuple,
            .tag,
            .nominal,
            .let_,
            .static_data_candidate,
            => {},
            else => return,
        }
        try self.expectExprAtTypeEvenIfDone(expr_id, backing_ty);
    }

    fn expectExprAtTypeEvenIfDone(self: *Solver, expr_id: Lifted.ExprId, expected: Type.TypeVarId) Allocator.Error!void {
        const expr = self.lifted.exprs[@intFromEnum(expr_id)];
        switch (expr.data) {
            .record => |fields| {
                for (self.lifted.fieldExprSpan(fields)) |field| {
                    const field_ty = try self.recordField(expected, field.name);
                    try self.expectExprAtTypeEvenIfDone(field.value, field_ty);
                }
            },
            .tuple => |items| {
                const item_tys = try self.tupleItemsSpan(expected);
                const children = self.lifted.exprSpan(items);
                if (item_tys.count() != children.len) Common.invariant("tuple expression arity differs from generated backing type");
                for (children, 0..) |child, i| {
                    try self.expectExprAtTypeEvenIfDone(child, self.program.types.spanItem(item_tys, i));
                }
            },
            .tag => |tag| {
                const payload_tys = try self.tagPayloadsSpan(expected, tag.name);
                const payloads = self.lifted.exprSpan(tag.payloads);
                if (payload_tys.count() != payloads.len) Common.invariant("tag expression payload arity differs from generated backing type");
                for (payloads, 0..) |payload, i| {
                    try self.expectExprAtTypeEvenIfDone(payload, self.program.types.spanItem(payload_tys, i));
                }
            },
            .static_data_candidate => |candidate| try self.expectExprAtTypeEvenIfDone(candidate.runtime_expr, expected),
            .nominal => |backing| {
                const backing_ty = try self.namedBacking(expected) orelse expected;
                try self.expectExprAtTypeEvenIfDone(backing, backing_ty);
            },
            .let_ => |let_| {
                const value_ty = try self.inferExpr(let_.value);
                try self.bindPattern(let_.bind, value_ty);
                try self.expectExprAtTypeEvenIfDone(let_.rest, expected);
            },
            .match_ => |match| {
                // The match slot was solved at the minted nominal type during
                // the first walk. Its branch bodies still need the backing
                // type, without unifying the match node with that backing.
                const scrutinee_ty = try self.inferExpr(match.scrutinee);
                for (self.lifted.branchSpan(match.branches)) |branch| {
                    try self.bindPattern(branch.pat, scrutinee_ty);
                    if (branch.guard) |guard| _ = try self.inferExpr(guard);
                    try self.expectExprAtTypeEvenIfDone(branch.body, expected);
                }
            },
            .if_ => |if_| {
                for (self.lifted.ifBranchSpan(if_.branches)) |branch| {
                    _ = try self.inferExpr(branch.cond);
                    try self.expectExprAtTypeEvenIfDone(branch.body, expected);
                }
                try self.expectExprAtTypeEvenIfDone(if_.final_else, expected);
            },
            .block => |block| try self.expectExprAtTypeEvenIfDone(block.final_expr, expected),
            .comptime_branch_taken => |taken| try self.expectExprAtTypeEvenIfDone(taken.body, expected),
            .if_initialized_payload => |payload_switch| {
                _ = try self.inferExpr(payload_switch.cond);
                _ = self.localTy(payload_switch.payload);
                try self.expectExprAtTypeEvenIfDone(payload_switch.initialized, expected);
                try self.expectExprAtTypeEvenIfDone(payload_switch.uninitialized, expected);
            },
            .try_sequence => |sequence| try self.expectExprAtTypeEvenIfDone(sequence.ok_body, expected),
            .try_record_sequence => |sequence| try self.expectExprAtTypeEvenIfDone(sequence.ok_body, expected),
            else => {
                const inferred = try self.inferExpr(expr_id);
                // An opaque leaf that already carries the generated nominal
                // has no backing expression to revisit. Its construction site
                // was traversed separately; keep this use at the nominal type.
                if (self.generatedIteratorBacking(inferred) != null and
                    self.generatedIteratorBacking(expected) == null)
                {
                    return;
                }
                const slot = try self.expectExprSlot(expr_id, expected);
                try self.unify(slot, inferred);
            },
        }
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
            return self.program.types.rootCompressed(ty);
        }

        const expr = self.lifted.exprs[index];
        const ty = switch (expr.data) {
            .local => |local| self.localTy(local),
            .fn_ref => |fn_ref| self.program.fn_tys.items[@intFromEnum(fn_ref.fn_id)],
            else => expected,
        };
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
        const ty = switch (pat.data) {
            .bind => |local| self.localTy(local),
            .as => |as| self.localTy(as.local),
            else => expected,
        };
        try self.unify(ty, expected);
        self.pat_tys[index] = ty;
        return self.program.types.rootCompressed(ty);
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
        var active = std.AutoHashMap(Type.TypeVarId, void).init(self.allocator);
        defer active.deinit();
        try self.markErasedCallablesReachedByTypeInner(ty, &active, false);
    }

    fn markErasedCallablesReachedByTypeInner(
        self: *Solver,
        ty: Type.TypeVarId,
        active: *std.AutoHashMap(Type.TypeVarId, void),
        // True while walking the backing of a bounded `Iter`/`Stream` nominal.
        // Its step closure stays a lambda set (inline captures) rather than
        // erasing to a boxed callable; every other function still erases.
        in_iter_backing: bool,
    ) Allocator.Error!void {
        const root = self.program.types.rootCompressed(ty);
        if (active.contains(root)) return;
        try active.put(root, {});
        defer _ = active.remove(root);

        switch (self.program.types.get(root)) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound, .forall, .primitive, .zst, .erased => {},
            .func => |func| {
                if (in_iter_backing) {
                    try self.markErasedCallablesReachedByTypeInner(func.callable, active, false);
                } else {
                    const erased = try self.program.types.add(.{ .erased = .{
                        .source_fn_ty = try self.solvedTypeDigest(root),
                        .members = .empty(),
                    } });
                    try self.unify(func.callable, erased);
                }
                for (self.program.types.span(func.args)) |arg| {
                    try self.markErasedCallablesReachedByTypeInner(arg, active, false);
                }
                try self.markErasedCallablesReachedByTypeInner(func.ret, active, false);
            },
            .list => |elem| try self.markErasedCallablesReachedByTypeInner(elem, active, in_iter_backing),
            .box => |elem| try self.markErasedCallablesReachedByTypeInner(elem, active, in_iter_backing),
            .tuple => |items| {
                for (self.program.types.span(items)) |item| {
                    try self.markErasedCallablesReachedByTypeInner(item, active, in_iter_backing);
                }
            },
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    try self.markErasedCallablesReachedByTypeInner(field.ty, active, in_iter_backing);
                }
            },
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    for (self.program.types.span(tag.payloads)) |payload| {
                        try self.markErasedCallablesReachedByTypeInner(payload, active, in_iter_backing);
                    }
                }
            },
            .named => |named| {
                for (self.program.types.span(named.args)) |arg| {
                    try self.markErasedCallablesReachedByTypeInner(arg, active, false);
                }
                if (named.backing) |backing| {
                    const backing_is_iter = named.def.iterator_representation == .minted and
                        if (named.builtin_owner) |owner|
                            static_dispatch.isIteratorOwner(owner)
                        else
                            false;
                    try self.markErasedCallablesReachedByTypeInner(backing.ty, active, backing_is_iter);
                }
            },
            .lambda_set => |members| {
                for (self.program.types.memberSpan(members)) |member| {
                    for (self.program.types.captureSpan(member.captures)) |capture| {
                        try self.markErasedCallablesReachedByTypeInner(capture.ty, active, in_iter_backing);
                    }
                }
            },
        }
    }

    /// Whether the lifted monotype subgraph rooted at `ty` transitively holds
    /// any function-typed occurrence: a `.func` node, which the cloner lowers
    /// to a fresh callable slot, or an `.erased` node, whose lowered form
    /// accumulates erased members. A subgraph with neither has no callable
    /// flow, so its completed clone may be shared across occurrences without
    /// coupling a lambda set or an erased-member accumulation. A subgraph with
    /// either must clone fresh at each non-recursive occurrence.
    fn containsCallableOccurrence(self: *Solver, ty: MonoType.TypeId) Allocator.Error!bool {
        var visiting = std.AutoHashMap(MonoType.TypeId, void).init(self.allocator);
        defer visiting.deinit();
        return (try self.containsCallableOccurrenceInner(ty, &visiting)).present;
    }

    fn containsCallableOccurrenceInner(
        self: *Solver,
        ty: MonoType.TypeId,
        visiting: *std.AutoHashMap(MonoType.TypeId, void),
    ) Allocator.Error!CallablePresence {
        if (self.callable_occurrence_memo.get(ty)) |known| {
            return .{ .present = known, .touched_active = false };
        }
        if (visiting.contains(ty)) {
            // A back-edge to an id whose walk is still open. The edge itself
            // adds no callable node; whether the cycle reaches one is decided
            // on the branch that first opened the id.
            return .{ .present = false, .touched_active = true };
        }
        try visiting.put(ty, {});

        var present = false;
        var touched_active = false;
        const fold = struct {
            fn go(
                inner: *Solver,
                child: MonoType.TypeId,
                open: *std.AutoHashMap(MonoType.TypeId, void),
                acc_present: *bool,
                acc_touched: *bool,
            ) Allocator.Error!void {
                const result = try inner.containsCallableOccurrenceInner(child, open);
                acc_present.* = acc_present.* or result.present;
                acc_touched.* = acc_touched.* or result.touched_active;
            }
        }.go;

        switch (self.lifted.types.get(ty)) {
            .primitive, .zst => {},
            // A function type gets a fresh callable slot; an erased function
            // accumulates members. Either couples callable flow when shared.
            .func, .erased => present = true,
            .list, .box => |elem| try fold(self, elem, visiting, &present, &touched_active),
            .tuple => |items| {
                for (self.lifted.types.span(items)) |item| {
                    try fold(self, item, visiting, &present, &touched_active);
                }
            },
            .record => |fields| {
                for (self.lifted.types.fieldSpan(fields)) |field| {
                    try fold(self, field.ty, visiting, &present, &touched_active);
                }
            },
            .tag_union => |tags| {
                for (self.lifted.types.tagSpan(tags)) |tag| {
                    for (self.lifted.types.span(tag.payloads)) |payload| {
                        try fold(self, payload, visiting, &present, &touched_active);
                    }
                }
            },
            .named => |named| {
                for (self.lifted.types.span(named.args)) |arg| {
                    try fold(self, arg, visiting, &present, &touched_active);
                }
                if (named.backing) |backing| {
                    try fold(self, backing.ty, visiting, &present, &touched_active);
                }
                for (self.lifted.types.declaredFieldSpan(named.declared_order)) |entry| {
                    switch (entry) {
                        .named => {},
                        .padding => |pad| try fold(self, pad, visiting, &present, &touched_active),
                    }
                }
            },
        }

        _ = visiting.remove(ty);
        // Cache only a result whose walk never leaned on an id still open
        // higher on the stack. Such a result is the complete answer for this
        // subgraph; a result reached through an open ancestor may be partial,
        // so it is left uncached for the cycle head to settle.
        if (!touched_active) {
            try self.callable_occurrence_memo.put(ty, present);
        }
        return .{ .present = present, .touched_active = touched_active };
    }

    fn lowerTypeFresh(self: *Solver, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        var cloner = TypeCloner.init(self);
        defer cloner.deinit();
        const lowered = try cloner.lower(ty);
        for (cloner.forced_dynamic_backings.items) |backing| {
            try self.markErasedCallablesReachedByType(backing);
        }
        return lowered;
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
        return switch (self.program.types.rootContentCompressed(ty)) {
            .named => |named| if (named.backing) |backing| backing.ty else null,
            else => null,
        };
    }

    fn generatedIteratorBacking(self: *Solver, ty: Type.TypeVarId) ?Type.TypeVarId {
        return switch (self.program.types.rootContent(ty)) {
            .named => |named| blk: {
                if (named.def.iterator_representation == .none) break :blk null;
                const owner = named.builtin_owner orelse break :blk null;
                if (!static_dispatch.isIteratorOwner(owner)) break :blk null;
                break :blk if (named.backing) |backing| backing.ty else null;
            },
            else => null,
        };
    }

    fn hasBuiltinOwner(self: *Solver, ty: Type.TypeVarId, owner: static_dispatch.BuiltinOwner) bool {
        return switch (self.program.types.rootContentCompressed(ty)) {
            .named => |named| if (named.builtin_owner) |builtin_owner| builtin_owner == owner else false,
            else => false,
        };
    }

    fn generatedOpaqueEvidenceScore(self: *Solver, named: anytype) u8 {
        if (!isGeneratedOpaqueEvidenceOwner(named.builtin_owner)) return 0;

        const backing = named.backing orelse return 0;
        return generatedBackingScore(self.program.types.rootContentCompressed(backing.ty)) orelse 2;
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
        var current = self.program.types.rootCompressed(ty);
        while (true) {
            switch (self.program.types.get(current)) {
                .named => |named| if (named.backing) |backing| {
                    current = self.program.types.rootCompressed(backing.ty);
                    continue;
                } else return self.program.types.get(current),
                else => return self.program.types.get(current),
            }
        }
    }

    fn unify(self: *Solver, lhs: Type.TypeVarId, rhs: Type.TypeVarId) Allocator.Error!void {
        const a = self.program.types.rootCompressed(lhs);
        const b = self.program.types.rootCompressed(rhs);
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
            self.program.types.set(a, .{ .link = self.program.types.rootCompressed(backing) });
            return;
        }
        if (transparentAliasBacking(right)) |backing| {
            try self.unify(a, backing);
            self.program.types.set(b, .{ .link = self.program.types.rootCompressed(backing) });
            return;
        }

        // An empty tag union is the seal for a slot that no value reached: a
        // variable that was defaulted at Monotype materialization rather than
        // constrained by evidence (see Monotype import, which re-enters it as an
        // unresolved node instead of a closed row). Two representations of the
        // same runtime type can therefore disagree here — most visibly in the
        // phantom argument types of a function value that is inspected but never
        // called (`|x, y| x + y` rendered as `<function>`), where the callee's
        // own body solves an argument to a concrete number while the referencing
        // site left the shared variable to seal as `[]`. Such a slot fixes no
        // layout, so it yields to a concrete peer instead of tripping the
        // exact-match invariant, matching how the Monotype layer lets local
        // evidence supersede an empty tag union.
        if (isEmptyTagUnion(left) and !isEmptyTagUnion(right)) {
            self.program.types.set(a, .{ .link = b });
            return;
        }
        if (isEmptyTagUnion(right) and !isEmptyTagUnion(left)) {
            self.program.types.set(b, .{ .link = a });
            return;
        }

        if (@import("builtin").mode == .Debug and !seamResidualShapesAgree(left, right)) {
            Common.invariant("Lambda Solved seam saw divergent structural shapes past the reunify section 12.4 census");
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
                        if (try self.unifyForcedDynamicIterator(a, b, left_named, right_named)) return;
                        if (try self.unifyIteratorOwnerStampedPublic(a, b, left_named, right_named)) return;
                        if (try self.unifyGeneratedIteratorJoin(a, b, left_named, right_named)) return;
                        if (try self.unifyPublicGeneratedIterator(a, b, left_named, right_named)) return;
                        Common.invariant("named type identity failed Lambda Solved unification");
                    }
                    try self.unifySpans(left_named.args, right_named.args, "named type arguments failed Lambda Solved unification");
                    // Aliases have already been unwrapped above. Generated
                    // compile-time evidence owners (`FieldNames`/`FieldName`/
                    // `ParseTagUnionSpec`) use their backing only to carry
                    // evidence rows, so two values with the same nominal identity
                    // may intentionally have different backing rows; the
                    // higher-scored backing is selected without unifying them.
                    // Iterators are excluded: a minted `Iter`/`Stream` keeps its
                    // step callable in its backing, so two same-identity minted
                    // iterators must unify their backings for the step callable
                    // members to merge. Selecting one backing by score instead
                    // would drop a step closure, leaving a zero-sized callable a
                    // later construction then mismatches at the box boundary.
                    if (isScoreSelectedEvidenceOwner(left_named.builtin_owner) or
                        isScoreSelectedEvidenceOwner(right_named.builtin_owner))
                    {
                        const evidence_scores = [2]u8{ self.generatedOpaqueEvidenceScore(right_named), self.generatedOpaqueEvidenceScore(left_named) };
                        if (census.enabled and evidence_scores[0] == evidence_scores[1]) {
                            census.bump("lambda_generated_backing_equal_score");
                        }
                        if (evidence_scores[0] > evidence_scores[1]) {
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
        try self.unifyIteratorBackings(left, right);
        if (left_dynamic) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
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

        if (left.def.iterator_representation == .minted) {
            self.program.types.set(right_ty, .{ .link = left_ty });
        } else {
            self.program.types.set(left_ty, .{ .link = right_ty });
        }
        return true;
    }

    fn unifyIteratorBackings(self: *Solver, left: anytype, right: anytype) Allocator.Error!void {
        if (left.backing) |left_backing| {
            const right_backing = right.backing orelse
                Common.invariant("iterator unification found backing on only one side");
            if (left_backing.use != right_backing.use) {
                Common.invariant("iterator unification found different backing uses");
            }
            try self.unify(left_backing.ty, right_backing.ty);
        } else if (right.backing != null) {
            Common.invariant("iterator unification found backing on only one side");
        }
    }

    fn transparentAliasBacking(content: Type.Content) ?Type.TypeVarId {
        return switch (content) {
            .named => |named| if (named.kind == .alias) blk: {
                if (census.enabled and named.builtin_owner != null) {
                    census.bump("lambda_alias_unwrap_builtin_owned");
                }
                break :blk (named.backing orelse Common.invariant("transparent alias reached Lambda Solved without a backing type")).ty;
            } else null,
            else => null,
        };
    }

    /// A tag union with no tags is the materialized seal for an unconstrained
    /// slot; it is uninhabited and fixes no layout.
    fn isEmptyTagUnion(content: Type.Content) bool {
        return switch (content) {
            .tag_union => |tags| tags.count() == 0,
            else => false,
        };
    }

    /// Debug-only seam assertion for reunify.md section 12.6. Both operands of
    /// a structural unification descend from the same ground lifted monotypes,
    /// so once the section 12.4 census relations above have each had their
    /// chance to fire, the residual pair must share a content constructor
    /// before the structural walk recurses. The only census relation that
    /// legitimately crosses constructors at this point is erased-callable
    /// dominance, which absorbs a lambda set from either direction. This is a
    /// single seam checkpoint, not a re-check of the walk: constructor-internal
    /// disagreement stays the walk's job (the count and label checks in
    /// unifySpans/unifyFields/unifyTags and the per-constructor else arms), the
    /// layout-only declared_order and named_type fields are never compared (the
    /// named arm discards the loser's copies), and a func's callable slot is
    /// the solver's own unknown rather than shared structure. A firing here
    /// means the Monotype stage handed the seam two divergent shapes: report it
    /// as a census gap, never widen this exemption to silence it.
    fn seamResidualShapesAgree(left: Type.Content, right: Type.Content) bool {
        if (std.meta.activeTag(left) == std.meta.activeTag(right)) return true;
        return switch (left) {
            .erased => right == .lambda_set,
            .lambda_set => right == .erased,
            else => false,
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
        const root = self.program.types.rootCompressed(ty);
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

const TypeCloner = struct {
    solver: *Solver,
    /// Monotype ids whose clone is in progress along the current path, mapped
    /// to the var reserved on entry. A back-edge to an id still on this path
    /// reuses its reservation, tying a recursive knot; the entry is removed
    /// once the clone completes, so a later non-recursive occurrence of the
    /// same id clones fresh with its own callable slot.
    active_path: std.AutoHashMap(MonoType.TypeId, Type.TypeVarId),
    /// Completed clones of callable-free subgraphs, keyed by monotype id. Only
    /// ids for which `containsCallableOccurrence` is false are recorded; their
    /// clones hold no callable slot, so sharing them across occurrences
    /// couples nothing and keeps large scalar-only types from re-cloning.
    shared_callable_free: std.AutoHashMap(MonoType.TypeId, Type.TypeVarId),
    forced_dynamic_backings: std.ArrayList(Type.TypeVarId),

    fn init(solver: *Solver) TypeCloner {
        return .{
            .solver = solver,
            .active_path = std.AutoHashMap(MonoType.TypeId, Type.TypeVarId).init(solver.allocator),
            .shared_callable_free = std.AutoHashMap(MonoType.TypeId, Type.TypeVarId).init(solver.allocator),
            .forced_dynamic_backings = .empty,
        };
    }

    fn deinit(self: *TypeCloner) void {
        self.forced_dynamic_backings.deinit(self.solver.allocator);
        self.shared_callable_free.deinit();
        self.active_path.deinit();
    }

    fn lower(self: *TypeCloner, ty: MonoType.TypeId) Allocator.Error!Type.TypeVarId {
        // A back-edge to an id still being cloned on the current path reuses
        // the var reserved on entry, tying the recursive knot.
        if (self.active_path.get(ty)) |reserved| return reserved;

        // A callable-free subgraph carries no callable slot, so its completed
        // clone is safe to share across occurrences.
        const callable_free = !(try self.solver.containsCallableOccurrence(ty));
        if (callable_free) {
            if (self.shared_callable_free.get(ty)) |cached| return cached;
        }

        const reserved = try self.solver.program.types.add(.unbound);
        try self.active_path.put(ty, reserved);
        const content = try self.lowerContent(self.solver.lifted.types.get(ty));
        self.solver.program.types.set(reserved, content);
        if (content == .named and content.named.def.iterator_representation == .forced_dynamic) {
            const backing = content.named.backing orelse
                Common.invariant("forced-dynamic iterator reached Lambda Solved without a backing type");
            try self.forced_dynamic_backings.append(self.solver.allocator, backing.ty);
        }
        _ = self.active_path.remove(ty);
        if (callable_free) {
            try self.shared_callable_free.put(ty, reserved);
        }
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
    return MonoType.generatedEvidenceOwnerUsesBacking(owner orelse return false);
}

/// A generated-backing owner whose same-identity instances carry independent
/// evidence backing rows that unification selects by score rather than unifies
/// (`FieldNames`/`FieldName`/`ParseTagUnionSpec`). Iterators also carry a
/// generated backing but are excluded here: their same-identity instances share
/// one backing structure whose step callable members must merge, so they take
/// ordinary backing unification instead of score selection.
fn isScoreSelectedEvidenceOwner(owner: ?static_dispatch.BuiltinOwner) bool {
    const resolved = owner orelse return false;
    return isGeneratedOpaqueEvidenceOwner(resolved) and !static_dispatch.isIteratorOwner(resolved);
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
    const actual = owner orelse return false;
    return switch (actual) {
        .iter,
        .stream,
        => true,
        else => false,
    };
}

fn sameMonoTypeDef(left: MonoType.TypeDef, right: MonoType.TypeDef) bool {
    return left.module == right.module and
        left.type_name == right.type_name and
        left.source_decl == right.source_decl and
        optionalDigestEql(left.generated, right.generated) and
        left.iterator_representation == right.iterator_representation and
        left.iterator_kind == right.iterator_kind and
        left.iterator_depth == right.iterator_depth;
}

fn optionalDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
    if (left == null and right == null) return true;
    if (left == null or right == null) return false;
    return std.mem.eql(u8, left.?.bytes[0..], right.?.bytes[0..]);
}

// --- Direct lambda-set invariant tests (reunify.md section 12.6) ---
//
// The Lambda Mono differential harness consumes the same solved program on
// both sides, so a mutated set corrupts both identically and the harness
// cannot see it; set-coarsening is usually behavior-preserving so output tests
// miss it too. These tests drive the solver's own store directly, each one
// pinning one section 12 invariant. They build a bare `Solver` over an
// `Ast.Program` whose lifted half is left unset (the store-only tests never
// read it, and the cloning tests point `lifted.types` at a hand-built monotype
// store), so no lifted program scaffolding is needed.

fn testProgram(gpa: Allocator) Ast.Program {
    return .{
        .allocator = gpa,
        .lifted = undefined,
        .types = Type.Store.init(gpa),
        .defs = .empty,
        .local_tys = .empty,
        .expr_tys = .empty,
        .pat_tys = .empty,
        .fn_tys = .empty,
        .layout_requests = .empty,
        .runtime_schema_requests = .empty,
    };
}

fn testSolver(program: *Ast.Program) Solver {
    return .{
        .allocator = program.allocator,
        .program = program,
        .lifted = undefined,
        .local_tys = &.{},
        .expr_tys = &.{},
        .pat_tys = &.{},
        .expr_done = &.{},
        .generated_backing_pats = &.{},
        .loop_results = .empty,
        .loop_params = .empty,
        .join_points = .empty,
        .return_contexts = .empty,
        .active_unifications = std.AutoHashMap(UnifyPair, void).init(program.allocator),
        .callable_occurrence_memo = std.AutoHashMap(MonoType.TypeId, bool).init(program.allocator),
    };
}

fn deinitTestSolver(solver: *Solver) void {
    solver.callable_occurrence_memo.deinit();
    solver.active_unifications.deinit();
}

fn testSym(n: u32) Common.Symbol {
    return @enumFromInt(n);
}

fn testFieldName(n: u32) names.RecordFieldNameId {
    return @enumFromInt(n);
}

fn testLocal(n: u32) Lifted.LocalId {
    return @enumFromInt(n);
}

fn testModuleIdentity(n: u32) names.ModuleIdentityId {
    return @enumFromInt(n);
}

fn testTypeName(n: u32) names.TypeNameId {
    return @enumFromInt(n);
}

fn addSingletonSet(store: *Type.Store, lambda: Common.Symbol) Allocator.Error!Type.TypeVarId {
    const members = try store.addMembers(&.{.{ .lambda = lambda, .captures = Type.Span.empty() }});
    return store.add(.{ .lambda_set = members });
}

fn solvedRoot(store: *Type.Store, ty: Type.TypeVarId) Type.Content {
    return store.get(store.rootCompressed(ty));
}

test "occurrence cloning gives structurally identical function-typed fields distinct callable slots" {
    const gpa = std.testing.allocator;

    // Two record fields whose declared type is the *same* monotype function id,
    // the shape interning would coarsen. The cloner must still hand each
    // occurrence its own callable slot; sharing would merge their lambda sets
    // with no value-flow edge.
    var mono = MonoType.Store.init(gpa);
    defer mono.deinit();

    const elem = try mono.add(.{ .primitive = .u64 });
    const arg_span = try mono.addSpan(&.{elem});
    const fn_ty = try mono.add(.{ .func = .{ .args = arg_span, .ret = elem } });
    const fields = try mono.addFields(&.{
        .{ .name = testFieldName(0), .ty = fn_ty },
        .{ .name = testFieldName(1), .ty = fn_ty },
    });
    const record = try mono.add(.{ .record = fields });

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    solver.lifted.types = mono.view();

    const lowered = try solver.lowerTypeFresh(record);

    const record_fields = program.types.fieldSpan(solvedRoot(&program.types, lowered).record);
    const first = record_fields[0].ty;
    const second = record_fields[1].ty;
    try std.testing.expect(program.types.rootCompressed(first) != program.types.rootCompressed(second));

    const first_fn = solvedRoot(&program.types, first).func;
    const second_fn = solvedRoot(&program.types, second).func;
    try std.testing.expect(first_fn.callable != second_fn.callable);
    try std.testing.expect(program.types.get(first_fn.callable) == .unbound);
    try std.testing.expect(program.types.get(second_fn.callable) == .unbound);
}

test "explicit value flow merges the callable slots of two function positions" {
    const gpa = std.testing.allocator;

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    const store = &program.types;

    const set_a = try addSingletonSet(store, testSym(1));
    const set_b = try addSingletonSet(store, testSym(2));
    const ret_a = try store.add(.zst);
    const ret_b = try store.add(.zst);
    const fn_a = try store.add(.{ .func = .{ .args = Type.Span.empty(), .callable = set_a, .ret = ret_a } });
    const fn_b = try store.add(.{ .func = .{ .args = Type.Span.empty(), .callable = set_b, .ret = ret_b } });

    try solver.unify(fn_a, fn_b);

    try std.testing.expect(store.rootCompressed(fn_a) == store.rootCompressed(fn_b));
    const merged_callable = solvedRoot(store, solvedRoot(store, fn_a).func.callable);
    const members = store.memberSpan(merged_callable.lambda_set);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(testSym(1), members[0].lambda);
    try std.testing.expectEqual(testSym(2), members[1].lambda);
}

test "occurrence cloning ties a genuine recursive back-reference and nothing else" {
    const gpa = std.testing.allocator;

    // `rec = (rec) -> u64` is self-referential through its argument. A record
    // with two `rec` fields gives two independent occurrences of that type.
    var mono = MonoType.Store.init(gpa);
    defer mono.deinit();

    const ret = try mono.add(.{ .primitive = .u64 });
    const Ctx = struct { store: *MonoType.Store, ret: MonoType.TypeId };
    const rec = try mono.addRecursive(Ctx{ .store = &mono, .ret = ret }, struct {
        fn fill(ctx: Ctx, self_id: MonoType.TypeId) Allocator.Error!MonoType.Content {
            const args = try ctx.store.addSpan(&.{self_id});
            return .{ .func = .{ .args = args, .ret = ctx.ret } };
        }
    }.fill);
    const fields = try mono.addFields(&.{
        .{ .name = testFieldName(0), .ty = rec },
        .{ .name = @enumFromInt(1), .ty = rec },
    });
    const record = try mono.add(.{ .record = fields });

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    solver.lifted.types = mono.view();

    const lowered = try solver.lowerTypeFresh(record);
    const record_fields = program.types.fieldSpan(solvedRoot(&program.types, lowered).record);
    const first = program.types.rootCompressed(record_fields[0].ty);
    const second = program.types.rootCompressed(record_fields[1].ty);

    // Separate occurrences do not share.
    try std.testing.expect(first != second);
    const first_fn = program.types.get(first).func;
    const second_fn = program.types.get(second).func;
    try std.testing.expect(first_fn.callable != second_fn.callable);

    // The recursive self-reference inside one clone ties the genuine back-edge:
    // the argument var is the cloned function itself.
    const first_arg = program.types.rootCompressed(program.types.spanItem(first_fn.args, 0));
    try std.testing.expectEqual(first, first_arg);
    const second_arg = program.types.rootCompressed(program.types.spanItem(second_fn.args, 0));
    try std.testing.expectEqual(second, second_arg);
}

test "mergeLambdaSets unions members by lambda symbol and unifies shared captures pointwise" {
    const gpa = std.testing.allocator;

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    const store = &program.types;

    const cap_a = try store.add(.unbound);
    const cap_b = try store.add(.unbound);
    const capture_id: check.CheckedModule.CaptureId = @enumFromInt(9);
    const captures_left = try store.addCaptures(&.{
        .{ .local = testLocal(0), .symbol = testSym(0), .binder = null, .capture_id = capture_id, .ty = cap_a },
    });
    const captures_right = try store.addCaptures(&.{
        .{ .local = testLocal(0), .symbol = testSym(0), .binder = null, .capture_id = capture_id, .ty = cap_b },
    });
    const left = try store.addMembers(&.{.{ .lambda = testSym(1), .captures = captures_left }});
    const right = try store.addMembers(&.{
        .{ .lambda = testSym(1), .captures = captures_right },
        .{ .lambda = testSym(2), .captures = Type.Span.empty() },
    });

    const merged = try solver.mergeLambdaSets(left, right);

    const members = store.memberSpan(merged);
    try std.testing.expectEqual(@as(usize, 2), members.len);
    try std.testing.expectEqual(testSym(1), members[0].lambda);
    try std.testing.expectEqual(testSym(2), members[1].lambda);
    // The shared lambda merged once, and its capture types unified pointwise.
    try std.testing.expectEqual(store.rootCompressed(cap_a), store.rootCompressed(cap_b));
}

test "unifyCaptures unifies capture types under matching identity" {
    const gpa = std.testing.allocator;

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    const store = &program.types;

    const left_ty = try store.add(.unbound);
    const right_ty = try store.add(.zst);
    const capture_id: check.CheckedModule.CaptureId = @enumFromInt(4);
    const left = try store.addCaptures(&.{
        .{ .local = testLocal(0), .symbol = testSym(0), .binder = null, .capture_id = capture_id, .ty = left_ty },
    });
    const right = try store.addCaptures(&.{
        .{ .local = @enumFromInt(1), .symbol = testSym(3), .binder = null, .capture_id = capture_id, .ty = right_ty },
    });

    try solver.unifyCaptures(left, right);
    try std.testing.expectEqual(store.rootCompressed(left_ty), store.rootCompressed(right_ty));
    // A capture-count or capture-identity mismatch is a `Common.invariant`
    // hard failure, which aborts the process rather than returning an error, so
    // that path is exercised by the seeded mutation check, not in-process here.
}

test "erased callable absorbs a lambda set from either direction and accumulates members" {
    const gpa = std.testing.allocator;

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    const store = &program.types;

    const digest = Type.names.TypeDigest{ .bytes = [_]u8{0} ** 32 };

    // lambda_set on the left, erased on the right.
    const set_a = try addSingletonSet(store, testSym(1));
    const erased_a = try store.add(.{ .erased = .{
        .source_fn_ty = digest,
        .members = try store.addMembers(&.{.{ .lambda = testSym(2), .captures = Type.Span.empty() }}),
    } });
    try solver.unify(set_a, erased_a);
    const left_root = solvedRoot(store, set_a);
    try std.testing.expect(left_root == .erased);
    try std.testing.expectEqual(@as(usize, 2), store.memberSpan(left_root.erased.members).len);

    // erased on the left, lambda_set on the right.
    const erased_b = try store.add(.{ .erased = .{
        .source_fn_ty = digest,
        .members = try store.addMembers(&.{.{ .lambda = testSym(3), .captures = Type.Span.empty() }}),
    } });
    const set_b = try addSingletonSet(store, testSym(4));
    try solver.unify(erased_b, set_b);
    const right_root = solvedRoot(store, erased_b);
    try std.testing.expect(right_root == .erased);
    try std.testing.expectEqual(@as(usize, 2), store.memberSpan(right_root.erased.members).len);
}

test "an unbound callable slot seals to the empty set" {
    const gpa = std.testing.allocator;

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    const store = &program.types;

    const callable = try store.add(.unbound);
    const ret = try store.add(.zst);
    _ = try store.add(.{ .func = .{ .args = Type.Span.empty(), .callable = callable, .ret = ret } });

    try solver.closeUnfilledCallableSlots();

    const sealed = solvedRoot(store, callable);
    try std.testing.expect(sealed == .lambda_set);
    try std.testing.expectEqual(@as(usize, 0), store.memberSpan(sealed.lambda_set).len);
}

test "the erasure pass keeps a minted iterator step closure a lambda set while erasing plain callables" {
    const gpa = std.testing.allocator;

    var program = testProgram(gpa);
    defer program.types.deinit();
    var solver = testSolver(&program);
    defer deinitTestSolver(&solver);
    const store = &program.types;

    // A minted `Iter` nominal holds its step closure by value in its backing;
    // the erasure pass must leave that closure a lambda set.
    const step_set = try addSingletonSet(store, testSym(1));
    const step_ret = try store.add(.zst);
    const step_fn = try store.add(.{ .func = .{ .args = Type.Span.empty(), .callable = step_set, .ret = step_ret } });
    const iter_def = MonoType.TypeDef{
        .module = testModuleIdentity(0),
        .type_name = testTypeName(0),
        .iterator_representation = .minted,
        .iterator_kind = .map,
        .iterator_depth = 1,
    };
    const iterator = try store.add(.{ .named = .{
        .named_type = std.mem.zeroes(MonoType.NamedType),
        .def = iter_def,
        .kind = .nominal,
        .builtin_owner = .iter,
        .args = Type.Span.empty(),
        .backing = .{ .ty = step_fn, .use = .runtime_layout_only },
        .declared_order = Type.Span.empty(),
    } });

    // A plain callable reached as data, by contrast, must erase.
    const plain_set = try addSingletonSet(store, testSym(2));
    const plain_ret = try store.add(.zst);
    const plain_fn = try store.add(.{ .func = .{ .args = Type.Span.empty(), .callable = plain_set, .ret = plain_ret } });

    try solver.markErasedCallablesReachedByType(iterator);
    try solver.markErasedCallablesReachedByType(plain_fn);

    try std.testing.expect(solvedRoot(store, step_set) == .lambda_set);
    try std.testing.expect(solvedRoot(store, plain_set) == .erased);
}

test "lambda solved solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
