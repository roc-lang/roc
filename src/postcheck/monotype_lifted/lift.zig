//! Closure lifting over Monotype IR.

const std = @import("std");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const MonoType = @import("../monotype/type.zig");
const Ast = @import("ast.zig");
const checked = @import("check").CheckedModule;

const Allocator = std.mem.Allocator;

/// Lift nested Monotype functions into explicit function bodies.
pub fn run(
    allocator: Allocator,
    mono: Mono.Program,
) Common.LowerError!Ast.Program {
    var owned = mono;
    errdefer owned.deinit();

    var name_store = owned.names;
    owned.names = @import("check").CheckedNames.NameStore.init(allocator);
    var types = owned.types;
    owned.types = @import("../monotype/type.zig").Store.init(allocator);
    var exprs = owned.exprs;
    owned.exprs = .empty;
    var pats = owned.pats;
    owned.pats = .empty;
    var stmts = owned.stmts;
    owned.stmts = .empty;
    var locals = owned.locals;
    owned.locals = .empty;
    var expr_ids = owned.expr_ids;
    owned.expr_ids = .empty;
    var pat_ids = owned.pat_ids;
    owned.pat_ids = .empty;
    var typed_locals = owned.typed_locals;
    owned.typed_locals = .empty;
    var stmt_ids = owned.stmt_ids;
    owned.stmt_ids = .empty;
    var field_exprs = owned.field_exprs;
    owned.field_exprs = .empty;
    var record_destructs = owned.record_destructs;
    owned.record_destructs = .empty;
    const str_pattern_steps = owned.str_pattern_steps;
    owned.str_pattern_steps = .empty;
    var branches = owned.branches;
    owned.branches = .empty;
    var if_branches = owned.if_branches;
    owned.if_branches = .empty;
    var string_literals = owned.string_literals;
    owned.string_literals = .empty;
    var proc_debug_names = owned.proc_debug_names;
    owned.proc_debug_names = Mono.ProcDebugNameMap.init(allocator);
    var runtime_schema_requests = owned.runtime_schema_requests;
    owned.runtime_schema_requests = .empty;
    var comptime_sites = owned.comptime_sites;
    owned.comptime_sites = .empty;
    var source_files = owned.source_files;
    owned.source_files = .empty;
    var expr_locs = owned.expr_locs;
    owned.expr_locs = .empty;
    var stmt_locs = owned.stmt_locs;
    owned.stmt_locs = .empty;
    var local_names = owned.local_names;
    owned.local_names = .empty;

    var program = Ast.Program.init(
        allocator,
        name_store,
        types,
        exprs,
        pats,
        stmts,
        locals,
        expr_ids,
        pat_ids,
        typed_locals,
        stmt_ids,
        field_exprs,
        record_destructs,
        str_pattern_steps,
        branches,
        if_branches,
        string_literals,
        proc_debug_names,
        source_files,
        expr_locs,
        stmt_locs,
        local_names,
        comptime_sites,
        owned.next_symbol,
    );
    name_store = undefined;
    types = undefined;
    exprs = undefined;
    pats = undefined;
    stmts = undefined;
    locals = undefined;
    expr_ids = undefined;
    pat_ids = undefined;
    typed_locals = undefined;
    stmt_ids = undefined;
    field_exprs = undefined;
    record_destructs = undefined;
    branches = undefined;
    if_branches = undefined;
    string_literals = undefined;
    proc_debug_names = undefined;
    source_files = undefined;
    expr_locs = undefined;
    stmt_locs = undefined;
    local_names = undefined;
    comptime_sites = undefined;
    program.runtime_schema_requests = runtime_schema_requests;
    runtime_schema_requests = undefined;
    errdefer program.deinit();

    var lifter = try Lifter.init(allocator, &owned, &program);
    defer lifter.deinit();

    try lifter.lowerDefsAndRoots();
    program.next_symbol = lifter.symbols.next;

    owned.deinit();
    return program;
}

const FnActiveSet = std.AutoHashMap(Ast.FnId, void);

const DefMap = []?Ast.FnId;
const NestedDefMap = []?Ast.FnId;
const FnMap = []?Ast.FnId;

const MonoFnBody = struct {
    args: Mono.Span(Mono.TypedLocal),
    body: Mono.FnBody,
};

const Lifter = struct {
    allocator: Allocator,
    source: *const Mono.Program,
    output: *Ast.Program,
    expr_done: []bool,
    stmt_done: []bool,
    def_map: DefMap,
    nested_def_map: NestedDefMap,
    fn_map: FnMap,
    fn_bodies: std.ArrayList(?MonoFnBody),
    nested_fn_ids: std.AutoHashMap(Ast.FnId, void),
    initialized_fns: std.AutoHashMap(Ast.FnId, void),
    symbols: Common.SymbolGen,

    fn init(allocator: Allocator, source: *const Mono.Program, output: *Ast.Program) Allocator.Error!Lifter {
        const expr_done = try allocator.alloc(bool, output.exprCount());
        errdefer allocator.free(expr_done);
        @memset(expr_done, false);

        const stmt_done = try allocator.alloc(bool, output.stmtCount());
        errdefer allocator.free(stmt_done);
        @memset(stmt_done, false);

        return .{
            .allocator = allocator,
            .source = source,
            .output = output,
            .expr_done = expr_done,
            .stmt_done = stmt_done,
            .def_map = &.{},
            .nested_def_map = &.{},
            .fn_map = &.{},
            .fn_bodies = .empty,
            .nested_fn_ids = std.AutoHashMap(Ast.FnId, void).init(allocator),
            .initialized_fns = std.AutoHashMap(Ast.FnId, void).init(allocator),
            .symbols = .{ .next = source.next_symbol },
        };
    }

    fn deinit(self: *Lifter) void {
        self.initialized_fns.deinit();
        self.nested_fn_ids.deinit();
        self.fn_bodies.deinit(self.allocator);
        if (self.fn_map.len > 0) self.allocator.free(self.fn_map);
        if (self.nested_def_map.len > 0) self.allocator.free(self.nested_def_map);
        if (self.def_map.len > 0) self.allocator.free(self.def_map);
        self.allocator.free(self.stmt_done);
        self.allocator.free(self.expr_done);
    }

    fn lowerDefsAndRoots(self: *Lifter) Allocator.Error!void {
        self.fn_map = try self.allocator.alloc(?Ast.FnId, self.source.fns.items.len);
        @memset(self.fn_map, null);

        self.def_map = try self.allocator.alloc(?Ast.FnId, self.source.defs.items.len);
        @memset(self.def_map, null);

        for (self.source.defs.items, 0..) |def, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
            try self.output.fns.append(self.allocator, undefined);
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = def.body });
            self.def_map[index] = fn_id;
            if (def.fn_id) |source_fn_id| self.registerFn(source_fn_id, fn_id);
        }

        self.nested_def_map = try self.allocator.alloc(?Ast.FnId, self.source.nested_defs.items.len);
        @memset(self.nested_def_map, null);
        for (self.source.nested_defs.items, 0..) |def, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
            try self.output.fns.append(self.allocator, undefined);
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = .{ .roc = def.body } });
            self.nested_def_map[index] = fn_id;
            try self.nested_fn_ids.put(fn_id, {});
            self.registerFn(def.fn_id, fn_id);
        }

        for (self.source.defs.items, 0..) |def, index| {
            try self.lowerTopLevelDef(self.def_map[index] orelse
                Common.invariant("Monotype definition was not reserved before lifting"), def);
        }

        for (self.source.nested_defs.items, 0..) |def, index| {
            try self.lowerNestedDef(self.nested_def_map[index] orelse
                Common.invariant("Monotype nested definition was not reserved before lifting"), def);
        }

        for (self.output.fns.items, 0..) |_, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            if (self.initialized_fns.contains(fn_id)) continue;
            Common.invariant("Monotype Lifted function was reserved but not initialized");
        }

        for (self.source.roots.items) |root| {
            const raw = @intFromEnum(root.def);
            if (raw >= self.def_map.len) Common.invariant("Monotype root references a missing definition");
            const fn_id = self.def_map[raw] orelse
                Common.invariant("Monotype root definition was not lifted");
            try self.output.roots.append(self.allocator, .{
                .fn_id = fn_id,
                .request = root.request,
            });
        }

        for (self.source.layout_requests.items) |request| {
            const fn_id = if (request.def) |def| blk: {
                const raw = @intFromEnum(def);
                if (raw >= self.def_map.len) Common.invariant("Monotype static data layout request references a missing definition");
                break :blk self.def_map[raw] orelse
                    Common.invariant("Monotype static data layout request definition was not lifted");
            } else null;
            try self.output.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = request.ty,
                .fn_id = fn_id,
            });
        }
    }

    fn lowerTopLevelDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.Def) Allocator.Error!void {
        var active = self.initFnActiveSet();
        defer active.deinit();
        var captures = CaptureSet.init(self, &active);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.output, &bound, self.output.typedLocalSpan(def.args));
        switch (def.body) {
            .roc => |body| try captures.collectExpr(body, &bound),
            .hosted => {},
        }

        if (captures.items.items.len != 0) {
            Common.invariant("top-level Monotype definition has free locals after checked closure collection");
        }

        const body: Ast.FnBody = switch (def.body) {
            .roc => |body| blk: {
                try self.rewriteExpr(body);
                break :blk .{ .roc = body };
            },
            .hosted => .hosted,
        };
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = def.symbol,
            .source = if (def.fn_id) |source_fn_id| self.defSource(source_fn_id, def.fn_def) else null,
            .args = def.args,
            .captures = .empty(),
            .body = body,
            .ret = def.ret,
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn lowerNestedDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.NestedDef) Allocator.Error!void {
        var active = self.initFnActiveSet();
        defer active.deinit();
        var captures = CaptureSet.init(self, &active);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.output, &bound, self.output.typedLocalSpan(def.args));
        try captures.collectExpr(def.body, &bound);

        try self.rewriteExpr(def.body);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = def.symbol,
            .source = self.nestedSource(def.fn_id, def.fn_def),
            .args = def.args,
            .captures = capture_span,
            .body = .{ .roc = def.body },
            .ret = def.ret,
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn rewriteStmt(self: *Lifter, stmt_id: Mono.StmtId) Allocator.Error!void {
        const index = @intFromEnum(stmt_id);
        if (self.stmt_done[index]) return;
        self.stmt_done[index] = true;

        switch (self.output.stmts.items[index]) {
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

    fn rewriteExpr(self: *Lifter, expr_id: Mono.ExprId) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (self.expr_done[index]) return;
        self.expr_done[index] = true;

        const expr = &self.output.exprs.items[index];
        switch (expr.data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .crash,
            .comptime_exhaustiveness_failed,
            .fn_ref,
            => {},
            .list,
            .tuple,
            => |items| for (self.output.exprSpan(items)) |child| try self.rewriteExpr(child),
            .record => |fields| for (self.output.fieldExprSpan(fields)) |field| try self.rewriteExpr(field.value),
            .tag => |tag| for (self.output.exprSpan(tag.payloads)) |child| try self.rewriteExpr(child),
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
            .lambda => |lambda| try self.liftLambda(expr_id, expr.ty, lambda),
            .def_ref => |def_id| {
                const raw = @intFromEnum(def_id);
                if (raw >= self.def_map.len) Common.invariant("Monotype definition reference was outside the definition table");
                expr.data = .{ .fn_ref = self.def_map[raw] orelse
                    Common.invariant("Monotype definition reference reached lifting before its function was registered") };
            },
            .fn_def => |fn_id| expr.data = .{ .fn_ref = self.liftedFn(fn_id) },
            .call_value => |call| {
                try self.rewriteExpr(call.callee);
                for (self.output.exprSpan(call.args)) |arg| try self.rewriteExpr(arg);
            },
            .call_proc => |call| {
                const fn_id = switch (call.callee) {
                    .func => |mono_fn_id| self.liftedFn(mono_fn_id),
                    .lifted => |fn_id| fn_id,
                };
                for (self.output.exprSpan(call.args)) |arg| try self.rewriteExpr(arg);
                expr.data = .{ .call_proc = .{
                    .callee = .{ .lifted = fn_id },
                    .args = call.args,
                } };
            },
            .low_level => |call| for (self.output.exprSpan(call.args)) |arg| try self.rewriteExpr(arg),
            .field_access => |field| try self.rewriteExpr(field.receiver),
            .tuple_access => |access| try self.rewriteExpr(access.tuple),
            .structural_eq => |eq| {
                try self.rewriteExpr(eq.lhs);
                try self.rewriteExpr(eq.rhs);
            },
            .match_ => |match| {
                try self.rewriteExpr(match.scrutinee);
                for (self.output.branchSpan(match.branches)) |branch| {
                    if (branch.guard) |guard| try self.rewriteExpr(guard);
                    try self.rewriteExpr(branch.body);
                }
            },
            .if_ => |if_| {
                for (self.output.ifBranchSpan(if_.branches)) |branch| {
                    try self.rewriteExpr(branch.cond);
                    try self.rewriteExpr(branch.body);
                }
                try self.rewriteExpr(if_.final_else);
            },
            .block => |block| {
                for (self.output.stmtSpan(block.statements)) |stmt| try self.rewriteStmt(stmt);
                try self.rewriteExpr(block.final_expr);
            },
            .loop_ => |loop| {
                for (self.output.exprSpan(loop.initial_values)) |initial| try self.rewriteExpr(initial);
                try self.rewriteExpr(loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.rewriteExpr(value),
            .continue_ => |continue_| for (self.output.exprSpan(continue_.values)) |value| try self.rewriteExpr(value),
        }
    }

    fn liftLambda(self: *Lifter, expr_id: Mono.ExprId, ty: @import("../monotype/type.zig").TypeId, lambda: Mono.LambdaExpr) Allocator.Error!void {
        const fn_id = try self.reserveFn(lambda.fn_id);
        self.output.exprs.items[@intFromEnum(expr_id)].data = .{ .fn_ref = fn_id };
        if (self.nested_fn_ids.contains(fn_id)) return;
        if (self.initialized_fns.contains(fn_id)) return;

        try self.setFnBody(fn_id, .{ .args = lambda.args, .body = .{ .roc = lambda.body } });
        var active = self.initFnActiveSet();
        defer active.deinit();
        var captures = CaptureSet.init(self, &active);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.output, &bound, self.output.typedLocalSpan(lambda.args));
        try captures.collectExpr(lambda.body, &bound);

        try self.rewriteExpr(lambda.body);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = self.symbols.fresh(),
            .source = self.source.fnSource(lambda.fn_id),
            .args = lambda.args,
            .captures = capture_span,
            .body = .{ .roc = lambda.body },
            .ret = functionRet(&self.output.types, ty),
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn reserveFn(self: *Lifter, mono_fn_id: Mono.FnId) Allocator.Error!Ast.FnId {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype lambda referenced a missing function specialization");
        if (self.fn_map[raw]) |existing| return existing;

        const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
        try self.output.fns.append(self.allocator, undefined);
        try self.fn_bodies.append(self.allocator, null);
        self.fn_map[raw] = fn_id;
        return fn_id;
    }

    fn setFnBody(self: *Lifter, fn_id: Ast.FnId, body: MonoFnBody) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.fn_bodies.items.len) Common.invariant("lifted function body id was outside body table");
        self.fn_bodies.items[raw] = body;
    }

    fn initFnActiveSet(self: *Lifter) FnActiveSet {
        return FnActiveSet.init(self.allocator);
    }

    fn registerFn(self: *Lifter, mono_fn_id: Mono.FnId, fn_id: Ast.FnId) void {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype definition referenced a missing function specialization");
        if (self.fn_map[raw]) |existing| {
            if (existing != fn_id) Common.invariant("Monotype function specialization was assigned two lifted function ids");
            return;
        }
        self.fn_map[raw] = fn_id;
    }

    fn liftedFn(self: *Lifter, mono_fn_id: Mono.FnId) Ast.FnId {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype expression referenced a missing function specialization");
        return self.fn_map[raw] orelse
            Common.invariant("Monotype expression referenced a function specialization before lifting registered it");
    }

    fn defSource(self: *Lifter, mono_fn_id: Mono.FnId, expected: ?Mono.FnTemplate) ?Mono.FnTemplate {
        const source = self.source.fnSource(mono_fn_id);
        if (expected) |template| {
            if (!std.meta.eql(source, template)) {
                Common.invariant("Monotype definition source disagreed with its function specialization source");
            }
        }
        return source;
    }

    fn nestedSource(self: *Lifter, mono_fn_id: Mono.FnId, expected: Mono.FnTemplate) Mono.FnTemplate {
        const source = self.source.fnSource(mono_fn_id);
        if (!std.meta.eql(source, expected)) {
            Common.invariant("Monotype nested definition source disagreed with its function specialization source");
        }
        return source;
    }
};

const BoundSet = struct {
    locals: std.AutoHashMap(Mono.LocalId, void),
    binders: std.AutoHashMap(checked.PatternBinderId, u32),

    fn init(allocator: Allocator) BoundSet {
        return .{
            .locals = std.AutoHashMap(Mono.LocalId, void).init(allocator),
            .binders = std.AutoHashMap(checked.PatternBinderId, u32).init(allocator),
        };
    }

    fn deinit(self: *BoundSet) void {
        self.binders.deinit();
        self.locals.deinit();
    }

    fn contains(self: *const BoundSet, input: *const Ast.Program, local: Mono.LocalId) bool {
        if (self.locals.contains(local)) return true;
        const binder = input.locals.items[@intFromEnum(local)].binder orelse return false;
        return self.binders.contains(binder);
    }

    fn put(self: *BoundSet, input: *const Ast.Program, local: Mono.LocalId) Allocator.Error!void {
        try self.locals.put(local, {});
        if (input.locals.items[@intFromEnum(local)].binder) |binder| try self.putBinder(binder);
    }

    fn remove(self: *BoundSet, input: *const Ast.Program, local: Mono.LocalId) void {
        _ = self.locals.remove(local);
        if (input.locals.items[@intFromEnum(local)].binder) |binder| self.removeBinder(binder);
    }

    fn putBinder(self: *BoundSet, binder: checked.PatternBinderId) Allocator.Error!void {
        const entry = try self.binders.getOrPut(binder);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    fn removeBinder(self: *BoundSet, binder: checked.PatternBinderId) void {
        const count = self.binders.getPtr(binder) orelse
            Common.invariant("capture collection removed an unbound checked binder");
        if (count.* > 1) {
            count.* -= 1;
        } else {
            _ = self.binders.remove(binder);
        }
    }
};

const CaptureSet = struct {
    allocator: Allocator,
    lifter: *Lifter,
    items: std.ArrayList(Ast.TypedLocal),
    seen: std.AutoHashMap(Mono.LocalId, void),
    active: *FnActiveSet,

    fn init(lifter: *Lifter, active: *FnActiveSet) CaptureSet {
        return .{
            .allocator = lifter.allocator,
            .lifter = lifter,
            .items = .empty,
            .seen = std.AutoHashMap(Mono.LocalId, void).init(lifter.allocator),
            .active = active,
        };
    }

    fn deinit(self: *CaptureSet) void {
        self.seen.deinit();
        self.items.deinit(self.allocator);
    }

    fn addIfFree(self: *CaptureSet, local: Mono.LocalId, bound: *const BoundSet) Allocator.Error!void {
        if (bound.contains(self.lifter.output, local) or self.seen.contains(local)) return;
        const local_data = self.lifter.output.locals.items[@intFromEnum(local)];
        try self.seen.put(local, {});
        try self.items.append(self.allocator, .{
            .local = local,
            .ty = local_data.ty,
        });
    }

    fn collectExpr(self: *CaptureSet, expr_id: Mono.ExprId, bound: *BoundSet) Allocator.Error!void {
        const input = self.lifter.output;
        const expr = input.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local => |local| try self.addIfFree(local, bound),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .def_ref,
            .crash,
            .comptime_exhaustiveness_failed,
            => {},
            .fn_ref => |fn_id| try self.collectFnCaptures(fn_id, bound),
            .fn_def => |fn_id| try self.collectFnCaptures(self.lifter.liftedFn(fn_id), bound),
            .list,
            .tuple,
            => |items| for (input.exprSpan(items)) |child| try self.collectExpr(child, bound),
            .record => |fields| for (input.fieldExprSpan(fields)) |field| try self.collectExpr(field.value, bound),
            .tag => |tag| for (input.exprSpan(tag.payloads)) |child| try self.collectExpr(child, bound),
            .nominal,
            .return_,
            .dbg,
            .expect,
            => |child| try self.collectExpr(child, bound),
            .expect_err => |expect_err| try self.collectExpr(expect_err.msg, bound),
            .comptime_branch_taken => |taken| try self.collectExpr(taken.body, bound),
            .let_ => |let_| {
                try self.collectExpr(let_.value, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindPat(self.allocator, input, let_.bind, bound, &added);
                try self.collectExpr(let_.rest, bound);
                removeBound(input, bound, added.items);
            },
            .lambda => |lambda| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(lambda.args), &added);
                try self.collectExpr(lambda.body, bound);
                removeBound(input, bound, added.items);
            },
            .call_value => |call| {
                try self.collectExpr(call.callee, bound);
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound);
            },
            .call_proc => |call| {
                switch (call.callee) {
                    .func => |mono_fn_id| try self.collectFnCaptures(self.lifter.liftedFn(mono_fn_id), bound),
                    .lifted => |fn_id| try self.collectFnCaptures(fn_id, bound),
                }
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound);
            },
            .low_level => |call| for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound),
            .field_access => |field| try self.collectExpr(field.receiver, bound),
            .tuple_access => |access| try self.collectExpr(access.tuple, bound),
            .structural_eq => |eq| {
                try self.collectExpr(eq.lhs, bound);
                try self.collectExpr(eq.rhs, bound);
            },
            .match_ => |match| {
                try self.collectExpr(match.scrutinee, bound);
                for (input.branchSpan(match.branches)) |branch| {
                    var added = std.ArrayList(Mono.LocalId).empty;
                    defer added.deinit(self.allocator);
                    try bindPat(self.allocator, input, branch.pat, bound, &added);
                    if (branch.guard) |guard| try self.collectExpr(guard, bound);
                    try self.collectExpr(branch.body, bound);
                    removeBound(input, bound, added.items);
                }
            },
            .if_ => |if_| {
                for (input.ifBranchSpan(if_.branches)) |branch| {
                    try self.collectExpr(branch.cond, bound);
                    try self.collectExpr(branch.body, bound);
                }
                try self.collectExpr(if_.final_else, bound);
            },
            .block => |block| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                for (input.stmtSpan(block.statements)) |stmt| try self.collectStmt(input, stmt, bound, &added);
                try self.collectExpr(block.final_expr, bound);
                removeBound(input, bound, added.items);
            },
            .loop_ => |loop| {
                for (input.exprSpan(loop.initial_values)) |initial| try self.collectExpr(initial, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(loop.params), &added);
                try self.collectExpr(loop.body, bound);
                removeBound(input, bound, added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectExpr(value, bound),
            .continue_ => |continue_| for (input.exprSpan(continue_.values)) |value| try self.collectExpr(value, bound),
        }
    }

    fn collectFnCaptures(self: *CaptureSet, fn_id: Ast.FnId, caller_bound: *BoundSet) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.lifter.fn_bodies.items.len) Common.invariant("capture collection referenced a missing lifted function");
        const body = self.lifter.fn_bodies.items[raw] orelse return;
        const entry = try self.active.getOrPut(fn_id);
        if (entry.found_existing) return;
        defer _ = self.active.remove(fn_id);

        var local_captures = CaptureSet.init(self.lifter, self.active);
        defer local_captures.deinit();

        var body_bound = BoundSet.init(self.allocator);
        defer body_bound.deinit();
        try bindTypedLocals(self.lifter.output, &body_bound, self.lifter.output.typedLocalSpan(body.args));
        switch (body.body) {
            .roc => |expr| try local_captures.collectExpr(expr, &body_bound),
            .hosted => {},
        }

        for (local_captures.items.items) |capture| {
            try self.addIfFree(capture.local, caller_bound);
        }
    }

    fn collectStmt(self: *CaptureSet, input: *const Ast.Program, stmt_id: Mono.StmtId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
        switch (input.stmts.items[@intFromEnum(stmt_id)]) {
            .uninitialized => |pat| try bindPat(self.allocator, input, pat, bound, added),
            .let_ => |let_| {
                if (let_.recursive) {
                    try bindPat(self.allocator, input, let_.pat, bound, added);
                    try self.collectExpr(let_.value, bound);
                } else {
                    try self.collectExpr(let_.value, bound);
                    try bindPat(self.allocator, input, let_.pat, bound, added);
                }
            },
            .expr,
            .expect,
            .dbg,
            .return_,
            => |expr| try self.collectExpr(expr, bound),
            .crash => {},
        }
    }
};

fn bindTypedLocals(input: *const Ast.Program, bound: *BoundSet, locals: []const Mono.TypedLocal) Allocator.Error!void {
    for (locals) |local| try bound.put(input, local.local);
}

fn bindTypedLocalsTracked(allocator: Allocator, input: *const Ast.Program, bound: *BoundSet, locals: []const Mono.TypedLocal, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    for (locals) |local| {
        try bound.put(input, local.local);
        try added.append(allocator, local.local);
    }
}

fn bindPat(allocator: Allocator, input: *const Ast.Program, pat_id: Mono.PatId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    switch (input.pats.items[@intFromEnum(pat_id)].data) {
        .bind => |local| {
            try bound.put(input, local);
            try added.append(allocator, local);
        },
        .wildcard,
        .int_lit,
        .dec_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .str_lit,
        => {},
        .str_pattern => |str| {
            for (input.strPatternStepSpan(str.steps)) |step| {
                if (step.capture) |capture| {
                    try bindPat(allocator, input, capture, bound, added);
                }
            }
        },
        .as => |as| {
            try bindPat(allocator, input, as.pattern, bound, added);
            try bound.put(input, as.local);
            try added.append(allocator, as.local);
        },
        .record => |fields| for (input.recordDestructSpan(fields)) |field| try bindPat(allocator, input, field.pattern, bound, added),
        .tuple => |items| for (input.patSpan(items)) |child| try bindPat(allocator, input, child, bound, added),
        .tag => |tag| for (input.patSpan(tag.payloads)) |child| try bindPat(allocator, input, child, bound, added),
        .nominal => |backing| try bindPat(allocator, input, backing, bound, added),
    }
}

fn removeBound(input: *const Ast.Program, bound: *BoundSet, locals: []const Mono.LocalId) void {
    var index = locals.len;
    while (index > 0) {
        index -= 1;
        bound.remove(input, locals[index]);
    }
}

fn functionRet(types: *const MonoType.Store, ty: MonoType.TypeId) MonoType.TypeId {
    return switch (shapeContent(types, ty)) {
        .func => |fn_ty| fn_ty.ret,
        else => Common.invariant("lifted lambda expression did not have a function type"),
    };
}

fn shapeContent(types: *const MonoType.Store, ty: MonoType.TypeId) MonoType.Content {
    var current = ty;
    while (true) {
        switch (types.get(current)) {
            .named => |named| if (named.backing) |backing| {
                current = backing.ty;
                continue;
            } else {
                return types.get(current);
            },
            else => |content| return content,
        }
    }
}

test "monotype lifted lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
