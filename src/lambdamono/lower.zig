//! Executable lowering from lambdasolved to lambdamono.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const solved = @import("lambdasolved");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");
const lower_type = @import("lower_type.zig");
const specializations = @import("specializations.zig");
const layouts_mod = @import("layouts.zig");

const Symbol = symbol_mod.Symbol;
const TypeVarId = solved.Type.TypeVarId;

/// Final executable lowering result, including layouts and generated wrappers.
pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    layouts: layouts_mod.Layouts,
    strings: base.StringLiteral.Store,
    entrypoint_wrappers: []Symbol,

    /// Release all memory owned by the executable lowering result.
    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.layouts.deinit(self.store.allocator);
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
        if (self.entrypoint_wrappers.len > 0) {
            self.store.allocator.free(self.entrypoint_wrappers);
        }
    }

    pub fn take(self: *Result, allocator: std.mem.Allocator) std.mem.Allocator.Error!Result {
        const result = self.*;
        var empty_store = ast.Store.init(allocator);
        errdefer empty_store.deinit();
        const empty_layouts = try layouts_mod.Layouts.initEmpty(allocator, &empty_store);
        self.* = .{
            .store = empty_store,
            .root_defs = .empty,
            .symbols = symbol_mod.Store.init(allocator),
            .types = type_mod.Store.init(allocator),
            .layouts = empty_layouts,
            .strings = .{},
            .entrypoint_wrappers = &.{},
        };
        return result;
    }
};

/// Lower a solved program into executable lambdamono form.
pub fn run(allocator: std.mem.Allocator, input: *solved.Lower.Result) std.mem.Allocator.Error!Result {
    return runWithEntrypoints(allocator, input, &.{});
}

/// Lower a solved program while generating wrappers for the requested entrypoints.
pub fn runWithEntrypoints(
    allocator: std.mem.Allocator,
    input: *solved.Lower.Result,
    entrypoints: []const Symbol,
) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, try input.take(allocator), entrypoints);
    defer lowerer.deinit();
    if (entrypoints.len > 0) {
        lowerer.entrypoint_wrappers = try allocator.alloc(Symbol, entrypoints.len);
        for (lowerer.entrypoint_wrappers) |*slot| {
            slot.* = Symbol.none;
        }
    }
    try lowerer.lowerProgram();
    var explicit_layouts = try layouts_mod.Layouts.initEmpty(allocator, &lowerer.output);
    errdefer {
        explicit_layouts.deinit(allocator);
    }
    try lowerer.finalizeLayouts(&explicit_layouts);
    var result = try lowerer.finish();
    result.layouts = explicit_layouts;
    return result;
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: solved.Lower.Result,
    entrypoints: []const Symbol,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    types: type_mod.Store,
    queue: specializations.Queue,
    fenv: []specializations.FEnvEntry,
    pending_values: std.ArrayList(ast.Def),
    inspect_defs: std.ArrayList(ast.Def),
    inspect_helpers: std.AutoHashMap(InspectKey, Symbol),
    top_level_values: std.AutoHashMap(Symbol, TopLevelValueSource),
    top_level_value_types: std.AutoHashMap(Symbol, type_mod.TypeId),
    hosted_call_sigs: std.AutoHashMap(Symbol, HostedCallSignature),
    entrypoint_wrappers: []Symbol,
    current_specializing_symbol: ?Symbol,

    const InspectKey = struct {
        value_ty: type_mod.TypeId,
        result_ty: type_mod.TypeId,
    };

    const EnvEntry = struct {
        symbol: Symbol,
        ty: TypeVarId,
        exec_ty: type_mod.TypeId,
        exact_fn_symbol: ?Symbol = null,
    };

    const CallSpecialization = struct {
        args_tys: []type_mod.TypeId,
        ret_ty: type_mod.TypeId,
        capture_ty: ?type_mod.TypeId,

        fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
            if (self.args_tys.len != 0) allocator.free(self.args_tys);
        }
    };

    const HostedCallSignature = struct {
        args_tys: []const type_mod.TypeId,
        ret_ty: type_mod.TypeId,

        fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
            if (self.args_tys.len != 0) allocator.free(self.args_tys);
        }
    };

    const HostedCallSignatureView = struct {
        args_tys: []const type_mod.TypeId,
        ret_ty: type_mod.TypeId,
    };

    const TopLevelValueSource = union(enum) {
        fn_: solved.Ast.FnDef,
        hosted_fn: solved.Ast.HostedFnDef,
        val: solved.Ast.ExprId,
        run: solved.Ast.RunDef,
    };

    const InstScope = struct {
        allocator: std.mem.Allocator,
        mapping: std.AutoHashMap(TypeVarId, TypeVarId),
        types: *solved.Type.Store,
        owned_types: ?*solved.Type.Store,

        fn init(allocator: std.mem.Allocator) InstScope {
            const owned_types = allocator.create(solved.Type.Store) catch @panic("OOM");
            owned_types.* = solved.Type.Store.init(allocator);
            return .{
                .allocator = allocator,
                .mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(allocator),
                .types = owned_types,
                .owned_types = owned_types,
            };
        }

        fn borrow(allocator: std.mem.Allocator, types: *solved.Type.Store) InstScope {
            return .{
                .allocator = allocator,
                .mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(allocator),
                .types = types,
                .owned_types = null,
            };
        }

        fn deinit(self: *InstScope) void {
            if (self.owned_types) |owned_types| {
                owned_types.deinit();
                self.allocator.destroy(owned_types);
            }
            self.mapping.deinit();
        }
    };

    fn init(allocator: std.mem.Allocator, input: solved.Lower.Result, entrypoints: []const Symbol) Lowerer {
        var lowerer = Lowerer{
            .allocator = allocator,
            .input = input,
            .entrypoints = entrypoints,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .types = undefined,
            .queue = specializations.Queue.init(allocator),
            .fenv = &.{},
            .pending_values = .empty,
            .inspect_defs = .empty,
            .inspect_helpers = std.AutoHashMap(InspectKey, Symbol).init(allocator),
            .top_level_values = std.AutoHashMap(Symbol, TopLevelValueSource).init(allocator),
            .top_level_value_types = std.AutoHashMap(Symbol, type_mod.TypeId).init(allocator),
            .hosted_call_sigs = std.AutoHashMap(Symbol, HostedCallSignature).init(allocator),
            .entrypoint_wrappers = &.{},
            .current_specializing_symbol = null,
        };
        lowerer.types = type_mod.Store.init(allocator);
        return lowerer;
    }

    fn deinit(self: *Lowerer) void {
        self.top_level_values.deinit();
        self.top_level_value_types.deinit();
        var hosted_sigs = self.hosted_call_sigs.valueIterator();
        while (hosted_sigs.next()) |sig| {
            sig.deinit(self.allocator);
        }
        self.hosted_call_sigs.deinit();
        self.inspect_helpers.deinit();
        self.inspect_defs.deinit(self.allocator);
        self.pending_values.deinit(self.allocator);
        self.queue.deinit();
        if (self.entrypoint_wrappers.len > 0) {
            self.allocator.free(self.entrypoint_wrappers);
        }
        if (self.fenv.len != 0) self.allocator.free(self.fenv);
        self.types.deinit();
        self.root_defs.deinit(self.allocator);
        self.output.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) std.mem.Allocator.Error!Result {
        const result = Result{
            .store = self.output,
            .root_defs = self.root_defs,
            .symbols = self.input.symbols,
            .types = self.types,
            .layouts = undefined,
            .strings = self.input.strings,
            .entrypoint_wrappers = self.entrypoint_wrappers,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.strings = .{};
        self.input.idents.deinit(self.allocator);
        self.input.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.types = type_mod.Store.init(self.allocator);
        self.entrypoint_wrappers = &.{};
        return result;
    }

    fn finalizeTypedSymbol(self: *Lowerer, bind: *ast.TypedSymbol) std.mem.Allocator.Error!void {
        bind.ty = try self.internExecutableType(bind.ty);
    }

    fn finalizeExpr(self: *Lowerer, expr: *ast.Expr, _: usize) std.mem.Allocator.Error!void {
        expr.ty = try self.internExecutableType(expr.ty);
        switch (expr.data) {
            .let_ => |*let_expr| try self.finalizeTypedSymbol(&let_expr.bind),
            else => {},
        }
    }

    fn finalizeStmt(self: *Lowerer, stmt: *ast.Stmt) std.mem.Allocator.Error!void {
        switch (stmt.*) {
            .decl => |*decl| try self.finalizeTypedSymbol(&decl.bind),
            .var_decl => |*decl| try self.finalizeTypedSymbol(&decl.bind),
            else => {},
        }
    }

    fn finalizeDef(self: *Lowerer, def: *ast.Def) std.mem.Allocator.Error!void {
        if (def.result_ty) |ty| {
            def.result_ty = try self.internExecutableType(ty);
        }
    }

    fn finalizeLayouts(self: *Lowerer, layouts: *layouts_mod.Layouts) std.mem.Allocator.Error!void {
        for (self.output.typed_symbols.items, 0..) |*bind, i| {
            try self.finalizeTypedSymbol(bind);
            try layouts.recordTypedSymbol(self.allocator, &self.types, &self.input.idents, i, bind.*);
        }
        for (self.output.pats.items, 0..) |*pat, i| {
            pat.ty = try self.internExecutableType(pat.ty);
            try layouts.recordPat(self.allocator, &self.types, &self.input.idents, &self.output, @enumFromInt(@as(u32, @intCast(i))), pat.*);
        }
        for (self.output.exprs.items, 0..) |*expr, i| {
            try self.finalizeExpr(expr, i);
            try layouts.recordExpr(self.allocator, &self.types, &self.input.idents, &self.output, @enumFromInt(@as(u32, @intCast(i))), expr.*);
        }
        for (self.output.stmts.items) |*stmt| {
            try self.finalizeStmt(stmt);
        }
        for (self.output.defs.items, 0..) |*def, i| {
            try self.finalizeDef(def);
            const def_id: ast.DefId = @enumFromInt(@as(u32, @intCast(i)));
            const ret_ty = switch (def.value) {
                .fn_ => |fn_def| def.result_ty orelse self.output.getExpr(fn_def.body).ty,
                .hosted_fn => def.result_ty orelse
                    debugPanic("lambdamono.lower.finalizeLayouts hosted def missing explicit result type"),
                .val => |expr_id| def.result_ty orelse self.output.getExpr(expr_id).ty,
                .run => |run_def| def.result_ty orelse self.output.getExpr(run_def.body).ty,
            };
            try layouts.recordDefRet(self.allocator, &self.types, &self.input.idents, def_id, ret_ty);
        }

        const u32_ty = try self.types.internResolved(.{ .primitive = .u32 });
        const erased_ty = try self.types.internResolved(.{ .primitive = .erased });
        const box_erased_ty = try self.types.internResolved(.{ .box = erased_ty });
        try layouts.recordType(self.allocator, &self.types, &self.input.idents, u32_ty);
        try layouts.recordType(self.allocator, &self.types, &self.input.idents, box_erased_ty);
    }

    fn lowerProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        self.fenv = try specializations.buildFEnv(self.allocator, &self.input);
        try self.indexTopLevelValues();
        try self.indexHostedFunctions();
        try self.seedEntrypointSpecializations();

        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .fn_, .hosted_fn => {},
                .val, .run => _ = try self.ensureTopLevelValueLowered(def.bind.symbol),
            }
        }

        try self.buildEntrypointWrappers();

        while (self.queue.nextPending()) |pending| {
            // Snapshot the pending request before specialization recurses and
            // potentially grows the queue. Specialization must consume stable
            // explicit facts, not keep reading through a queue slot that may
            // be relocated by later appends.
            const specialized_symbol = pending.specialized_symbol;
            try self.promotePendingSummary(pending);
            pending.status = .specializing;
            const specialized = try self.specializeFn(specialized_symbol);
            _ = try self.finishPendingSpecialization(specialized_symbol, specialized);
        }

        const fn_defs = try self.queue.solvedDefs(self.allocator);
        defer self.allocator.free(fn_defs);

        for (fn_defs) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .hosted_fn => |hosted_fn| {
                    _ = try self.output.addDef(try self.lowerHostedDef(def.bind, hosted_fn));
                },
                else => {},
            }
        }
        for (self.inspect_defs.items) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
        for (self.pending_values.items) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }

        try self.moveEntrypointRootsToEnd();
    }

    fn lookupPendingBySpecializedSymbol(self: *Lowerer, symbol: Symbol) ?*specializations.Pending {
        for (self.queue.items.items) |*item| {
            if (item.specialized_symbol == symbol) return item;
        }
        return null;
    }

    fn executableTypeIsAbstract(self: *Lowerer, ty: type_mod.TypeId) bool {
        return self.types.containsAbstractLeaf(ty);
    }

    fn sourceTypeIsAbstract(self: *Lowerer, solved_types: *const solved.Type.Store, ty: TypeVarId) bool {
        var visited = std.AutoHashMap(TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        return self.sourceTypeIsAbstractVisited(solved_types, ty, &visited) catch true;
    }

    fn sourceTypeIsAbstractVisited(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        ty: TypeVarId,
        visited: *std.AutoHashMap(TypeVarId, void),
    ) std.mem.Allocator.Error!bool {
        const id = solved_types.unlinkPreservingNominalConst(ty);
        if (visited.contains(id)) return false;
        try visited.put(id, {});
        return switch (solved_types.getNode(id)) {
            .for_a, .flex_for_a, .unbd => true,
            .link => unreachable,
            .nominal => |nominal| blk: {
                for (solved_types.sliceTypeVarSpan(nominal.args)) |arg| {
                    if (try self.sourceTypeIsAbstractVisited(solved_types, arg, visited)) break :blk true;
                }
                break :blk try self.sourceTypeIsAbstractVisited(solved_types, nominal.backing, visited);
            },
            .content => |content| switch (content) {
                .primitive, .lambda_set => false,
                .func => |func| blk: {
                    for (solved_types.sliceTypeVarSpan(func.args)) |arg| {
                        if (try self.sourceTypeIsAbstractVisited(solved_types, arg, visited)) break :blk true;
                    }
                    if (try self.sourceTypeIsAbstractVisited(solved_types, func.ret, visited)) break :blk true;
                    break :blk try self.sourceTypeIsAbstractVisited(solved_types, func.lset, visited);
                },
                .list => |elem| try self.sourceTypeIsAbstractVisited(solved_types, elem, visited),
                .box => |elem| try self.sourceTypeIsAbstractVisited(solved_types, elem, visited),
                .tuple => |elems| blk: {
                    for (solved_types.sliceTypeVarSpan(elems)) |elem| {
                        if (try self.sourceTypeIsAbstractVisited(solved_types, elem, visited)) break :blk true;
                    }
                    break :blk false;
                },
                .record => |record| blk: {
                    for (solved_types.sliceFields(record.fields)) |field| {
                        if (try self.sourceTypeIsAbstractVisited(solved_types, field.ty, visited)) break :blk true;
                    }
                    break :blk false;
                },
                .tag_union => |tag_union| blk: {
                    for (solved_types.sliceTags(tag_union.tags)) |tag| {
                        for (solved_types.sliceTypeVarSpan(tag.args)) |arg| {
                            if (try self.sourceTypeIsAbstractVisited(solved_types, arg, visited)) break :blk true;
                        }
                    }
                    break :blk false;
                },
            },
        };
    }

    fn refineBindingBodyIfNeeded(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        body_expr_id: solved.Ast.ExprId,
        refined_bind_ty: TypeVarId,
        initial_body: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const initial_exec_ty = self.output.getExpr(initial_body).ty;
        if (!self.executableTypeIsAbstract(initial_exec_ty)) return initial_body;
        if (self.sourceTypeIsAbstract(inst.types, refined_bind_ty)) return initial_body;

        const refined_body = try self.specializeExprAtSourceTy(inst, mono_cache, venv, body_expr_id, refined_bind_ty);
        if (self.executableTypeIsAbstract(self.output.getExpr(refined_body).ty)) {
            debugPanic("lambdamono.lower.refineBindingBodyIfNeeded concrete source type still lowered to abstract executable type");
        }
        return refined_body;
    }

    fn normalizeBindingBodyToSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        refined_bind_ty: TypeVarId,
        body_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (self.sourceTypeIsAbstract(inst.types, refined_bind_ty)) return body_expr;

        const expected_exec_ty = try self.lowerExecutableTypeFromSolvedIn(
            inst.types,
            mono_cache,
            refined_bind_ty,
        );
        if (self.types.equalIds(self.output.getExpr(body_expr).ty, expected_exec_ty)) {
            return body_expr;
        }

        return try self.bridgeExprAtSolvedTypeToExpectedExecutableType(
            inst,
            mono_cache,
            refined_bind_ty,
            body_expr,
            expected_exec_ty,
        );
    }

    fn queuedRefinedSourceTypeForLoweredExpr(
        self: *Lowerer,
        inst: *InstScope,
        lowered_expr: ast.ExprId,
    ) std.mem.Allocator.Error!?TypeVarId {
        var current = lowered_expr;
        while (true) {
            switch (self.output.getExpr(current).data) {
                .bridge => |inner| {
                    current = inner;
                    continue;
                },
                .call => |call| {
                    const pending = self.lookupPendingBySpecializedSymbol(call.proc) orelse return null;
                    const summary_types = pending.summary_types orelse
                        debugPanic("lambdamono.lower.queuedRefinedSourceTypeForLoweredExpr missing specialization summary store");
                    const summary_fn_ty = pending.summary_fn_ty orelse
                        debugPanic("lambdamono.lower.queuedRefinedSourceTypeForLoweredExpr missing specialization summary type");
                    return try self.cloneTypeIntoInstFromStore(
                        inst,
                        summary_types,
                        summary_types.fnShape(summary_fn_ty).ret,
                    );
                },
                else => return null,
            }
        }
    }

    fn refinedSourceTypeForLoweredExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        source_expr_id: solved.Ast.ExprId,
        current_source_ty: TypeVarId,
        lowered_expr: ast.ExprId,
    ) std.mem.Allocator.Error!TypeVarId {
        if (try self.queuedRefinedSourceTypeForLoweredExpr(inst, lowered_expr)) |queued_ty| {
            return queued_ty;
        }
        return try self.refinedSourceTypeForExpr(inst, venv, source_expr_id, current_source_ty);
    }

    const SpecializedCallableSummary = struct {
        symbol: Symbol,
        summary_types: *solved.Type.Store,
        summary_fn_ty: TypeVarId,
        exec_capture_ty: ?type_mod.TypeId,
        exec_args_tys: []const type_mod.TypeId,
        exec_ret_ty: type_mod.TypeId,
    };

    const PendingExecutableSignatureView = struct {
        capture_ty: ?type_mod.TypeId,
        args_tys: []const type_mod.TypeId,
        ret_ty: type_mod.TypeId,
    };

    fn mergeExecutableSignatureType(
        self: *Lowerer,
        existing: type_mod.TypeId,
        incoming: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.types.equalIds(existing, incoming)) return existing;
        if (self.executableTypeIsAbstract(existing) and !self.executableTypeIsAbstract(incoming)) return incoming;
        if (!self.executableTypeIsAbstract(existing) and self.executableTypeIsAbstract(incoming)) return existing;
        if (self.types.getTypePreservingNominal(existing) == .nominal and
            self.types.getTypePreservingNominal(incoming) != .nominal)
        {
            const existing_nominal = self.types.getTypePreservingNominal(existing).nominal;
            const merged_backing = try self.mergeExecutableSignatureType(existing_nominal.backing, incoming);
            return try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = existing_nominal.module_idx,
                    .ident = existing_nominal.ident,
                    .is_opaque = existing_nominal.is_opaque,
                    .args = try self.types.dupeTypeIds(existing_nominal.args),
                    .backing = merged_backing,
                },
            }));
        }
        if (self.types.getTypePreservingNominal(existing) != .nominal and
            self.types.getTypePreservingNominal(incoming) == .nominal)
        {
            const incoming_nominal = self.types.getTypePreservingNominal(incoming).nominal;
            const merged_backing = try self.mergeExecutableSignatureType(existing, incoming_nominal.backing);
            return try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = incoming_nominal.module_idx,
                    .ident = incoming_nominal.ident,
                    .is_opaque = incoming_nominal.is_opaque,
                    .args = try self.types.dupeTypeIds(incoming_nominal.args),
                    .backing = merged_backing,
                },
            }));
        }
        return switch (self.types.getTypePreservingNominal(existing)) {
            .primitive => |existing_prim| switch (self.types.getTypePreservingNominal(incoming)) {
                .primitive => |incoming_prim| if (existing_prim == incoming_prim)
                    existing
                else if (existing_prim == .dec and isNumericPrimitive(incoming_prim))
                    incoming
                else if (incoming_prim == .dec and isNumericPrimitive(existing_prim))
                    existing
                else
                    debugPanicFmt(
                        "lambdamono.lower.mergeExecutableSignatureType conflicting primitive executable signature existing={s} incoming={s}",
                        .{ @tagName(existing_prim), @tagName(incoming_prim) },
                    ),
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType primitive shape mismatch"),
            },
            .erased_fn => |existing_capture_ty| switch (self.types.getTypePreservingNominal(incoming)) {
                .erased_fn => |incoming_capture_ty| try self.makeErasedFnType(if (existing_capture_ty) |existing_capture|
                    if (incoming_capture_ty) |incoming_capture|
                        try self.mergeExecutableSignatureType(existing_capture, incoming_capture)
                    else
                        existing_capture
                else
                    incoming_capture_ty),
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType erased fn shape mismatch"),
            },
            .nominal => |existing_nominal| switch (self.types.getTypePreservingNominal(incoming)) {
                .nominal => |incoming_nominal| blk: {
                    if (existing_nominal.module_idx != incoming_nominal.module_idx or
                        existing_nominal.ident != incoming_nominal.ident or
                        existing_nominal.is_opaque != incoming_nominal.is_opaque or
                        existing_nominal.args.len != incoming_nominal.args.len)
                    {
                        const existing_text = self.debugExecutableTypeSummary(existing);
                        defer existing_text.deinit(self.allocator);
                        const incoming_text = self.debugExecutableTypeSummary(incoming);
                        defer incoming_text.deinit(self.allocator);
                        debugPanicFmt(
                            "lambdamono.lower.mergeExecutableSignatureType nominal shape mismatch existing={s} incoming={s}",
                            .{ existing_text.text, incoming_text.text },
                        );
                    }
                    const merged_args = try self.types.allocator.alloc(type_mod.TypeId, existing_nominal.args.len);
                    for (existing_nominal.args, incoming_nominal.args, 0..) |existing_arg, incoming_arg, i| {
                        merged_args[i] = try self.mergeExecutableSignatureType(existing_arg, incoming_arg);
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{
                        .nominal = .{
                            .module_idx = existing_nominal.module_idx,
                            .ident = existing_nominal.ident,
                            .is_opaque = existing_nominal.is_opaque,
                            .args = try self.types.dupeTypeIds(merged_args),
                            .backing = try self.mergeExecutableSignatureType(existing_nominal.backing, incoming_nominal.backing),
                        },
                    }));
                },
                else => {
                    const existing_text = self.debugExecutableTypeSummary(existing);
                    defer existing_text.deinit(self.allocator);
                    const incoming_text = self.debugExecutableTypeSummary(incoming);
                    defer incoming_text.deinit(self.allocator);
                    debugPanicFmt(
                        "lambdamono.lower.mergeExecutableSignatureType nominal shape mismatch existing={s} incoming={s}",
                        .{ existing_text.text, incoming_text.text },
                    );
                },
            },
            .list => |existing_elem| switch (self.types.getTypePreservingNominal(incoming)) {
                .list => |incoming_elem| try self.internExecutableType(try self.types.internResolved(.{
                    .list = try self.mergeExecutableSignatureType(existing_elem, incoming_elem),
                })),
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType list shape mismatch"),
            },
            .box => |existing_elem| switch (self.types.getTypePreservingNominal(incoming)) {
                .box => |incoming_elem| try self.internExecutableType(try self.types.internResolved(.{
                    .box = try self.mergeExecutableSignatureType(existing_elem, incoming_elem),
                })),
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType box shape mismatch"),
            },
            .tuple => |existing_elems| switch (self.types.getTypePreservingNominal(incoming)) {
                .tuple => |incoming_elems| blk: {
                    if (existing_elems.len != incoming_elems.len) {
                        debugPanic("lambdamono.lower.mergeExecutableSignatureType tuple shape mismatch");
                    }
                    const merged = try self.types.allocator.alloc(type_mod.TypeId, existing_elems.len);
                    for (existing_elems, incoming_elems, 0..) |existing_elem, incoming_elem, i| {
                        merged[i] = try self.mergeExecutableSignatureType(existing_elem, incoming_elem);
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{ .tuple = try self.types.dupeTypeIds(merged) }));
                },
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType tuple shape mismatch"),
            },
            .record => |existing_record| switch (self.types.getTypePreservingNominal(incoming)) {
                .record => |incoming_record| blk: {
                    if (existing_record.fields.len != incoming_record.fields.len) {
                        debugPanic("lambdamono.lower.mergeExecutableSignatureType record shape mismatch");
                    }
                    const merged = try self.types.allocator.alloc(type_mod.Field, existing_record.fields.len);
                    for (existing_record.fields, incoming_record.fields, 0..) |existing_field, incoming_field, i| {
                        if (existing_field.name != incoming_field.name) {
                            debugPanic("lambdamono.lower.mergeExecutableSignatureType record field mismatch");
                        }
                        merged[i] = .{
                            .name = existing_field.name,
                            .ty = try self.mergeExecutableSignatureType(existing_field.ty, incoming_field.ty),
                        };
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{
                        .record = .{ .fields = try self.types.dupeFields(merged) },
                    }));
                },
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType record shape mismatch"),
            },
            .tag_union => |existing_tag_union| switch (self.types.getTypePreservingNominal(incoming)) {
                .tag_union => |incoming_tag_union| blk: {
                    if (existing_tag_union.tags.len != incoming_tag_union.tags.len) {
                        debugPanic("lambdamono.lower.mergeExecutableSignatureType tag union shape mismatch");
                    }
                    const merged_tags = try self.types.allocator.alloc(type_mod.Tag, existing_tag_union.tags.len);
                    for (existing_tag_union.tags, incoming_tag_union.tags, 0..) |existing_tag, incoming_tag, i| {
                        if (!std.meta.eql(existing_tag.name, incoming_tag.name) or existing_tag.args.len != incoming_tag.args.len) {
                            debugPanic("lambdamono.lower.mergeExecutableSignatureType tag mismatch");
                        }
                        const merged_args = try self.types.allocator.alloc(type_mod.TypeId, existing_tag.args.len);
                        for (existing_tag.args, incoming_tag.args, 0..) |existing_arg, incoming_arg, arg_i| {
                            merged_args[arg_i] = try self.mergeExecutableSignatureType(existing_arg, incoming_arg);
                        }
                        merged_tags[i] = .{
                            .name = existing_tag.name,
                            .args = try self.types.dupeTypeIds(merged_args),
                        };
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{
                        .tag_union = .{ .tags = try self.types.dupeTags(merged_tags) },
                    }));
                },
                else => debugPanic("lambdamono.lower.mergeExecutableSignatureType tag union shape mismatch"),
            },
            .placeholder, .unbd, .link => debugPanic("lambdamono.lower.mergeExecutableSignatureType unresolved executable type"),
        };
    }

    fn mergePendingExecutableSignature(
        self: *Lowerer,
        pending: *specializations.Pending,
        arg_exec_tys: []const type_mod.TypeId,
        ret_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        if (pending.exec_args_tys) |existing_args| {
            if (existing_args.len != arg_exec_tys.len) {
                debugPanic("lambdamono.lower.mergePendingExecutableSignature arg arity mismatch");
            }
            var changed = false;
            const merged_args: []type_mod.TypeId = if (arg_exec_tys.len == 0)
                &.{}
            else
                try self.allocator.alloc(type_mod.TypeId, arg_exec_tys.len);
            errdefer if (merged_args.len != 0) self.allocator.free(merged_args);
            for (existing_args, arg_exec_tys, 0..) |existing, incoming, i| {
                merged_args[i] = try self.mergeExecutableSignatureType(existing, incoming);
                if (!self.types.equalIds(merged_args[i], existing)) changed = true;
            }
            if (changed) {
                if (existing_args.len != 0) self.allocator.free(existing_args);
                pending.exec_args_tys = merged_args;
            } else if (merged_args.len != 0) {
                self.allocator.free(merged_args);
            }
        } else {
            pending.exec_args_tys = if (arg_exec_tys.len == 0)
                &.{}
            else
                try self.allocator.dupe(type_mod.TypeId, arg_exec_tys);
        }

        pending.exec_ret_ty = if (pending.exec_ret_ty) |existing|
            try self.mergeExecutableSignatureType(existing, ret_exec_ty)
        else
            ret_exec_ty;
    }

    fn mergePendingExecutableArgSignature(
        self: *Lowerer,
        pending: *specializations.Pending,
        arg_exec_tys: []const type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        if (pending.exec_args_tys) |existing_args| {
            if (existing_args.len != arg_exec_tys.len) {
                debugPanic("lambdamono.lower.mergePendingExecutableArgSignature arg arity mismatch");
            }
            var changed = false;
            const merged_args: []type_mod.TypeId = if (arg_exec_tys.len == 0)
                &.{}
            else
                try self.allocator.alloc(type_mod.TypeId, arg_exec_tys.len);
            errdefer if (merged_args.len != 0) self.allocator.free(merged_args);
            for (existing_args, arg_exec_tys, 0..) |existing, incoming, i| {
                merged_args[i] = try self.mergeExecutableSignatureType(existing, incoming);
                if (!self.types.equalIds(merged_args[i], existing)) changed = true;
            }
            if (changed) {
                if (existing_args.len != 0) self.allocator.free(existing_args);
                pending.exec_args_tys = merged_args;
            } else if (merged_args.len != 0) {
                self.allocator.free(merged_args);
            }
        } else {
            pending.exec_args_tys = if (arg_exec_tys.len == 0)
                &.{}
            else
                try self.allocator.dupe(type_mod.TypeId, arg_exec_tys);
        }
    }

    fn mergePendingExecutableCaptureSignature(
        self: *Lowerer,
        pending: *specializations.Pending,
        capture_exec_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        if (capture_exec_ty == null) return;
        pending.exec_capture_ty = if (pending.exec_capture_ty) |existing|
            try self.mergeExecutableSignatureType(existing, capture_exec_ty.?)
        else
            capture_exec_ty.?;
    }

    fn mergePendingExecutableReturnSignature(
        self: *Lowerer,
        pending: *specializations.Pending,
        ret_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        pending.exec_ret_ty = if (pending.exec_ret_ty) |existing|
            try self.mergeExecutableSignatureType(existing, ret_exec_ty)
        else
            ret_exec_ty;
    }

    fn promotePendingSummary(self: *Lowerer, pending: *specializations.Pending) std.mem.Allocator.Error!void {
        if (pending.summary_types != null) return;
        const requested_types = pending.requested_types orelse
            debugPanic("lambdamono.lower.promotePendingSummary missing requested specialization store");
        const requested_ty = pending.requested_ty orelse
            debugPanic("lambdamono.lower.promotePendingSummary missing requested specialization type");
        const summary_types = try self.allocator.create(solved.Type.Store);
        summary_types.* = requested_types;
        pending.requested_types = null;
        pending.requested_ty = null;
        pending.summary_types = summary_types;
        pending.summary_fn_ty = requested_ty;
    }

    fn requirePendingExecutableSignature(
        self: *const Lowerer,
        pending: *const specializations.Pending,
    ) PendingExecutableSignatureView {
        _ = self;
        return .{
            .capture_ty = pending.exec_capture_ty,
            .args_tys = pending.exec_args_tys orelse
                debugPanic("lambdamono.lower.requirePendingExecutableSignature missing executable arg signature"),
            .ret_ty = pending.exec_ret_ty orelse
                debugPanic("lambdamono.lower.requirePendingExecutableSignature missing executable return signature"),
        };
    }

    fn refreshPendingExecutableSignatureFromSummary(
        self: *Lowerer,
        pending: *specializations.Pending,
    ) std.mem.Allocator.Error!void {
        const summary_types = pending.summary_types orelse
            debugPanic("lambdamono.lower.refreshPendingExecutableSignatureFromSummary missing summary store");
        const summary_fn_ty = pending.summary_fn_ty orelse
            debugPanic("lambdamono.lower.refreshPendingExecutableSignatureFromSummary missing summary fn type");

        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        const fn_shape = summary_types.fnShape(summary_fn_ty);
        const source_arg_tys = summary_types.sliceTypeVarSpan(fn_shape.args);
        const derived_args: []type_mod.TypeId = if (source_arg_tys.len == 0)
            &.{}
        else
            try self.allocator.alloc(type_mod.TypeId, source_arg_tys.len);

        for (source_arg_tys, 0..) |arg_ty, i| {
            derived_args[i] = switch (pending.repr_mode) {
                .natural => try self.lowerExecutableTypeFromSolvedIn(summary_types, &mono_cache, arg_ty),
                .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(summary_types, &mono_cache, arg_ty),
            };
        }

        const derived_ret = switch (pending.repr_mode) {
            .natural => try self.lowerExecutableTypeFromSolvedIn(summary_types, &mono_cache, fn_shape.ret),
            .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(summary_types, &mono_cache, fn_shape.ret),
        };

        if (pending.exec_args_tys) |existing_args| {
            const merged_args = try self.allocator.alloc(type_mod.TypeId, existing_args.len);
            errdefer self.allocator.free(merged_args);
            if (existing_args.len != derived_args.len) {
                debugPanic("lambdamono.lower.refreshPendingExecutableSignatureFromSummary arg arity mismatch");
            }
            for (existing_args, derived_args, 0..) |existing, incoming, i| {
                merged_args[i] = try self.mergeExecutableSignatureType(existing, incoming);
            }
            if (existing_args.len != 0) self.allocator.free(existing_args);
            if (derived_args.len != 0) self.allocator.free(derived_args);
            pending.exec_args_tys = merged_args;
        } else {
            pending.exec_args_tys = derived_args;
        }

        pending.exec_ret_ty = if (pending.exec_ret_ty) |existing|
            try self.mergeExecutableSignatureType(existing, derived_ret)
        else
            derived_ret;
    }

    fn ensureQueuedCallableSpecializedForCall(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        requested_name: Symbol,
        repr_mode: specializations.Pending.ReprMode,
        requested_ty: TypeVarId,
        capture_exec_ty: ?type_mod.TypeId,
        arg_exprs: []const ast.ExprId,
        expected_ret_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedCallableSummary {
        _ = expected_ret_exec_ty;
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();
        const fn_shape = solved_types.fnShape(requested_ty);
        const arg_source_tys = solved_types.sliceTypeVarSpan(fn_shape.args);
        if (arg_source_tys.len != arg_exprs.len) {
            debugPanic("lambdamono.lower.ensureQueuedCallableSpecializedForCall call arg arity mismatch");
        }
        const arg_exec_tys = try self.allocator.alloc(type_mod.TypeId, arg_source_tys.len);
        defer self.allocator.free(arg_exec_tys);
        for (arg_exprs, 0..) |arg_expr, i| {
            arg_exec_tys[i] = self.output.getExpr(arg_expr).ty;
        }
        const ret_exec_ty = switch (repr_mode) {
            .natural => try self.lowerExecutableTypeFromSolvedIn(solved_types, &mono_cache, fn_shape.ret),
            .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, &mono_cache, fn_shape.ret),
        };
        const pending_symbol = try specializations.specializeFnWithExecArgs(
            &self.queue,
            self.fenv,
            solved_types,
            &self.input.symbols,
            requested_name,
            repr_mode,
            requested_ty,
            &self.types,
            capture_exec_ty,
            arg_exec_tys,
            ret_exec_ty,
        );
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.ensureQueuedCallableSpecializedForCall missing queued specialization");
        if (pending.summary_types == null) {
            try self.promotePendingSummary(pending);
        }
        try self.mergePendingExecutableCaptureSignature(pending, capture_exec_ty);
        try self.mergePendingExecutableArgSignature(pending, arg_exec_tys);
        try self.mergePendingExecutableReturnSignature(pending, ret_exec_ty);
        return try self.ensureQueuedCallableSpecialized(pending_symbol);
    }

    fn ensureQueuedCallableSpecialized(
        self: *Lowerer,
        pending_symbol: Symbol,
    ) std.mem.Allocator.Error!SpecializedCallableSummary {
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized missing queued specialization");
        switch (pending.status) {
            .done, .specializing => return .{
                .symbol = pending_symbol,
                .summary_types = pending.summary_types orelse
                    debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized specialization missing summary store"),
                .summary_fn_ty = pending.summary_fn_ty orelse
                    debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized specialization missing summary type"),
                .exec_capture_ty = pending.exec_capture_ty,
                .exec_args_tys = pending.exec_args_tys orelse
                    debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized specialization missing executable arg signature"),
                .exec_ret_ty = pending.exec_ret_ty orelse
                    debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized specialization missing executable return signature"),
            },
            .pending => {
                if (pending.summary_types == null) {
                    try self.promotePendingSummary(pending);
                }
                pending.status = .specializing;
                const specialized = try self.specializeFn(pending_symbol);
                const updated = try self.finishPendingSpecialization(pending_symbol, specialized);
                return .{
                    .symbol = pending_symbol,
                    .summary_types = updated.summary_types orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized completed specialization missing summary store"),
                    .summary_fn_ty = updated.summary_fn_ty orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized completed specialization missing summary type"),
                    .exec_capture_ty = updated.exec_capture_ty,
                    .exec_args_tys = updated.exec_args_tys orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized completed specialization missing executable arg signature"),
                    .exec_ret_ty = updated.exec_ret_ty orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized completed specialization missing executable return signature"),
                };
            },
        }
    }

    fn finishPendingSpecialization(
        self: *Lowerer,
        pending_symbol: Symbol,
        specialized: SpecializedFn,
    ) std.mem.Allocator.Error!*specializations.Pending {
        const updated = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.finishPendingSpecialization lost queued specialization after growth");
        try self.refreshPendingExecutableSignatureFromSummary(updated);
        _ = self.requirePendingExecutableSignature(updated);
        updated.specialized = specialized.fn_def;
        updated.status = .done;
        return updated;
    }

    fn seedEntrypointSpecializations(self: *Lowerer) std.mem.Allocator.Error!void {
        if (self.entrypoints.len == 0) return;

        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        for (self.entrypoints) |entry_symbol| {
            const entry = specializations.lookupFnExact(self.fenv, entry_symbol) orelse continue;
            switch (self.input.types.lambdaRepr(entry.fn_ty)) {
                .erased => debugPanic("lambdamono.lower entrypoint specialization expected lambda set"),
                .lset => {
                    _ = try self.queueCallableSpecializationWithExplicitSignature(
                        &self.input.types,
                        &mono_cache,
                        entry_symbol,
                        .natural,
                        entry.fn_ty,
                        null,
                    );
                },
            }
        }
    }

    fn collectFunctionSignature(
        self: *Lowerer,
        fn_ty: TypeVarId,
        args: *std.ArrayList(TypeVarId),
    ) std.mem.Allocator.Error!TypeVarId {
        const fn_shape = self.input.types.fnShape(fn_ty);
        try args.appendSlice(self.allocator, self.input.types.sliceTypeVarSpan(fn_shape.args));
        return fn_shape.ret;
    }

    fn buildEntrypointWrappers(self: *Lowerer) std.mem.Allocator.Error!void {
        if (self.entrypoints.len == 0) return;

        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        var arg_vars = std.ArrayList(TypeVarId).empty;
        defer arg_vars.deinit(self.allocator);
        for (self.entrypoints, 0..) |entry_symbol, entry_idx| {
            self.entrypoint_wrappers[entry_idx] = entry_symbol;

            const entry_fn_ty = if (specializations.lookupFnExact(self.fenv, entry_symbol)) |entry|
                entry.fn_ty
            else if (self.top_level_values.get(entry_symbol)) |source|
                switch (source) {
                    .fn_ => debugPanic("lambdamono.lower.lowerProgram expected entrypoint function symbol to be specialized"),
                    .hosted_fn => debugPanic("lambdamono.lower.lowerProgram expected entrypoint hosted function symbol to be specialized"),
                    .val => |expr_id| self.input.store.getExpr(expr_id).ty,
                    .run => |run_def| self.input.store.getExpr(run_def.body).ty,
                }
            else
                continue;

            arg_vars.clearRetainingCapacity();
            const entry_is_callable = self.input.types.maybeLambdaRepr(entry_fn_ty) != null;
            const final_ret = if (entry_is_callable)
                try self.collectFunctionSignature(entry_fn_ty, &arg_vars)
            else
                entry_fn_ty;

            const needs_wrapper = if (specializations.lookupFnExact(self.fenv, entry_symbol)) |_|
                entry_is_callable and arg_vars.items.len > 1
            else
                true;

            if (!needs_wrapper) {
                if (specializations.lookupFnExact(self.fenv, entry_symbol)) |entry| {
                    self.entrypoint_wrappers[entry_idx] = try self.queueCallableSpecializationWithExplicitSignature(
                        &self.input.types,
                        &mono_cache,
                        entry_symbol,
                        .natural,
                        entry.fn_ty,
                        null,
                    );
                }
                continue;
            }

            const arg_count = arg_vars.items.len;
            const arg_exec_tys = try self.allocator.alloc(type_mod.TypeId, arg_count);
            defer self.allocator.free(arg_exec_tys);

            for (arg_vars.items, 0..) |arg_ty, i| {
                arg_exec_tys[i] = try self.lowerExecutableTypeFromSolvedIn(&self.input.types, &mono_cache, arg_ty);
            }
            const final_ret_exec_ty = try self.lowerExecutableTypeFromSolvedIn(&self.input.types, &mono_cache, final_ret);

            const entry_name = self.input.idents.getText(self.input.symbols.get(entry_symbol).name);
            const wrapper_name = try std.fmt.allocPrint(self.allocator, "{s}__entrypoint", .{entry_name});
            defer self.allocator.free(wrapper_name);
            const wrapper_ident = try self.input.idents.insert(self.allocator, base.Ident.for_text(wrapper_name));
            const wrapper_symbol = try self.input.symbols.add(wrapper_ident, .synthetic);
            self.entrypoint_wrappers[entry_idx] = wrapper_symbol;

            const wrapper_args = try self.allocator.alloc(ast.TypedSymbol, arg_count);
            defer self.allocator.free(wrapper_args);
            const arg_exprs = try self.allocator.alloc(ast.ExprId, arg_count);
            defer self.allocator.free(arg_exprs);

            for (arg_exec_tys, 0..) |arg_ty, i| {
                const arg_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                wrapper_args[i] = .{ .ty = arg_ty, .symbol = arg_symbol };
                arg_exprs[i] = try self.output.addExpr(.{
                    .ty = arg_ty,
                    .data = .{ .var_ = arg_symbol },
                });
            }

            const args_span = try self.output.addTypedSymbolSpan(wrapper_args);
            var current_expr: ast.ExprId = undefined;
            if (specializations.lookupFnExact(self.fenv, entry_symbol)) |_| {
                const specialized = try self.ensureQueuedCallableSpecializedForCall(
                    &self.input.types,
                    entry_symbol,
                    .natural,
                    entry_fn_ty,
                    null,
                    arg_exprs,
                    final_ret_exec_ty,
                );
                var inst = InstScope.borrow(self.allocator, &self.input.types);
                defer inst.deinit();
                current_expr = try self.applyExactTopLevelFunctionCall(
                    &inst,
                    &mono_cache,
                    specialized.symbol,
                    arg_vars.items,
                    arg_exprs,
                    specialized.exec_args_tys,
                    specialized.exec_ret_ty,
                    final_ret_exec_ty,
                );
            } else {
                const func_exec_ty = try self.ensureTopLevelValueLowered(entry_symbol);
                const func_expr = try self.output.addExpr(.{
                    .ty = func_exec_ty,
                    .data = .{ .var_ = entry_symbol },
                });

                if (!entry_is_callable or arg_count == 0) {
                    current_expr = func_expr;
                } else {
                    var inst = InstScope.init(self.allocator);
                    defer inst.deinit();
                    var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
                    defer mapping.deinit();
                    const requested_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
                        &inst,
                        &self.input.types,
                        &mapping,
                        entry_fn_ty,
                    );
                    const requested_args = inst.types.sliceTypeVarSpan(inst.types.fnShape(requested_fn_ty).args);
                    current_expr = try self.applyCallableValueCall(
                        &inst,
                        &mono_cache,
                        func_expr,
                        requested_fn_ty,
                        requested_fn_ty,
                        func_exec_ty,
                        requested_args,
                        arg_exprs,
                        final_ret_exec_ty,
                        self.exactCallableSymbolForBinding(entry_symbol),
                    );
                }
            }

            const def_id = try self.output.addDef(.{
                .bind = wrapper_symbol,
                .value = .{ .fn_ = .{
                    .args = args_span,
                    .body = current_expr,
                } },
            });
            try self.root_defs.append(self.allocator, def_id);
        }
    }

    fn moveEntrypointRootsToEnd(self: *Lowerer) std.mem.Allocator.Error!void {
        if (self.entrypoint_wrappers.len == 0 or self.root_defs.items.len == 0) return;

        const original = try self.allocator.dupe(ast.DefId, self.root_defs.items);
        defer self.allocator.free(original);

        self.root_defs.clearRetainingCapacity();

        for (original) |def_id| {
            const bind = self.output.getDef(def_id).bind;
            if (!self.isEntrypointRootSymbol(bind)) {
                try self.root_defs.append(self.allocator, def_id);
            }
        }

        for (original) |def_id| {
            const bind = self.output.getDef(def_id).bind;
            if (self.isEntrypointRootSymbol(bind)) {
                try self.root_defs.append(self.allocator, def_id);
            }
        }
    }

    fn isEntrypointRootSymbol(self: *const Lowerer, symbol: Symbol) bool {
        for (self.entrypoint_wrappers) |entry_symbol| {
            if (entry_symbol == symbol) return true;
        }
        return false;
    }

    fn applyCallableValueCall(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        func_expr: ast.ExprId,
        current_fn_ty: TypeVarId,
        requested_fn_ty: TypeVarId,
        func_exec_ty: type_mod.TypeId,
        arg_source_tys: []const TypeVarId,
        arg_exprs: []const ast.ExprId,
        result_exec_ty: type_mod.TypeId,
        direct_func_symbol: ?Symbol,
    ) std.mem.Allocator.Error!ast.ExprId {
        const solved_types = inst.types;
        self.assertCallableExprMatchesSolvedType(solved_types, func_expr, current_fn_ty);
        const fn_shape = solved_types.fnShape(requested_fn_ty);
        const param_source_tys = solved_types.sliceTypeVarSpan(fn_shape.args);
        if (param_source_tys.len != arg_exprs.len or arg_source_tys.len != arg_exprs.len) {
            debugPanic("lambdamono.lower.applyCallableValueCall call arg arity mismatch");
        }
        return switch (self.types.getType(func_exec_ty)) {
            .erased_fn => blk: {
                var common_arg_tys: ?[]const type_mod.TypeId = null;
                defer if (common_arg_tys) |args| if (args.len != 0) self.allocator.free(args);
                var common_ret_ty: ?type_mod.TypeId = null;

                switch (solved_types.lambdaRepr(requested_fn_ty)) {
                    .lset => |lambda_members| {
                        for (lambda_members) |lambda_member| {
                            const specialized = try self.ensureQueuedCallableSpecializedForCall(
                                solved_types,
                                lambda_member.symbol,
                                .erased_boundary,
                                requested_fn_ty,
                                self.erasedFnCaptureType(func_exec_ty),
                                arg_exprs,
                                result_exec_ty,
                            );
                            if (common_arg_tys == null) {
                                common_arg_tys = try self.allocator.dupe(type_mod.TypeId, specialized.exec_args_tys);
                            } else {
                                if (common_arg_tys.?.len != specialized.exec_args_tys.len) {
                                    debugPanic("lambdamono.lower.applyCallableValueCall erased fn arg arity mismatch across queued specializations");
                                }
                                for (common_arg_tys.?, specialized.exec_args_tys) |expected_arg_ty, actual_arg_ty| {
                                    if (!self.types.equalIds(expected_arg_ty, actual_arg_ty)) {
                                        debugPanic("lambdamono.lower.applyCallableValueCall erased fn arg executable type mismatch across queued specializations");
                                    }
                                }
                            }
                            if (common_ret_ty) |existing_ret_ty| {
                                if (!self.types.equalIds(existing_ret_ty, specialized.exec_ret_ty)) {
                                    debugPanic("lambdamono.lower.applyCallableValueCall erased fn return executable type mismatch across queued specializations");
                                }
                            } else {
                                common_ret_ty = specialized.exec_ret_ty;
                            }
                        }
                    },
                    .erased => {
                        const erased_arg_tys = try self.allocator.alloc(type_mod.TypeId, param_source_tys.len);
                        defer self.allocator.free(erased_arg_tys);
                        for (param_source_tys, 0..) |param_source_ty, i| {
                            erased_arg_tys[i] = try self.lowerErasedBoundaryExecutableTypeIn(
                                solved_types,
                                mono_cache,
                                param_source_ty,
                            );
                        }
                        common_arg_tys = try self.allocator.dupe(type_mod.TypeId, erased_arg_tys);
                        common_ret_ty = try self.lowerErasedBoundaryExecutableTypeIn(
                            solved_types,
                            mono_cache,
                            fn_shape.ret,
                        );
                    },
                }

                const lowered_args = try self.bridgeCallArgsToExpectedExecutableTypes(
                    inst,
                    mono_cache,
                    arg_source_tys,
                    arg_exprs,
                    common_arg_tys orelse
                        debugPanic("lambdamono.lower.applyCallableValueCall erased fn missing executable arg signature"),
                );
                defer if (lowered_args.len != 0) self.allocator.free(lowered_args);

                const call_ret_ty = try self.mergeExecutableSignatureType(
                    result_exec_ty,
                    common_ret_ty orelse
                        debugPanic("lambdamono.lower.applyCallableValueCall erased fn missing executable return signature"),
                );

                break :blk self.output.addExpr(.{
                    .ty = call_ret_ty,
                    .data = .{ .call_erased = .{
                        .func = func_expr,
                        .args = try self.output.addExprSpan(lowered_args),
                        .capture_ty = self.erasedFnCaptureType(func_exec_ty),
                    } },
                });
            },
            .tag_union => |tag_union| blk: {
                if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    debugPanic("lambdamono.lower.applyCallableValueCall expected callable executable type");
                }

                const lambda_members = try self.collectExecutableLambdaMembers(solved_types, requested_fn_ty, func_exec_ty);
                defer self.allocator.free(lambda_members);
                const branches = try self.allocator.alloc(ast.Branch, lambda_members.len);
                defer self.allocator.free(branches);
                var result_ty: ?type_mod.TypeId = null;
                var branch_call_args: ?[]ast.ExprId = null;
                defer if (branch_call_args) |args| if (args.len != 0) self.allocator.free(args);
                var common_arg_tys: ?[]const type_mod.TypeId = null;
                defer if (common_arg_tys) |args| if (args.len != 0) self.allocator.free(args);

                for (lambda_members, 0..) |lambda_member, i| {
                    const specialization_symbol = if (lambda_members.len == 1)
                        direct_func_symbol orelse lambda_member.symbol
                    else
                        lambda_member.symbol;
                    if (self.isHostedFunctionSymbol(specialization_symbol)) {
                        const sig = self.lookupHostedCallSig(specialization_symbol);
                        if (sig.args_tys.len != arg_exprs.len) {
                            debugPanic("lambdamono.lower.applyCallableValueCall branch arg arity mismatch");
                        }
                        if (common_arg_tys == null) {
                            branch_call_args = try self.bridgeCallArgsToExpectedExecutableTypes(
                                inst,
                                mono_cache,
                                arg_source_tys,
                                arg_exprs,
                                sig.args_tys,
                            );
                            common_arg_tys = try self.allocator.dupe(type_mod.TypeId, sig.args_tys);
                        } else {
                            for (common_arg_tys.?, sig.args_tys) |expected_arg_ty, actual_arg_ty| {
                                if (!self.types.equalIds(expected_arg_ty, actual_arg_ty)) {
                                    debugPanic("lambdamono.lower.applyCallableValueCall inconsistent branch arg executable types");
                                }
                            }
                        }
                        if (lambda_member.capture_ty != null) {
                            debugPanic("lambdamono.lower.applyCallableValueCall hosted function had captures");
                        }
                        const final_branch_ret_ty = if (self.executableTypeIsAbstract(result_exec_ty) and
                            !self.executableTypeIsAbstract(sig.ret_ty))
                            sig.ret_ty
                        else
                            result_exec_ty;
                        var body_expr = try self.output.addExpr(.{
                            .ty = sig.ret_ty,
                            .data = .{ .call = .{
                                .proc = lambda_member.symbol,
                                .args = try self.output.addExprSpan(branch_call_args.?),
                            } },
                        });
                        if (!self.types.equalIds(sig.ret_ty, final_branch_ret_ty)) {
                            body_expr = try self.emitExplicitBridgeExpr(body_expr, final_branch_ret_ty);
                        }
                        const body_ty = self.output.getExpr(body_expr).ty;
                        if (result_ty) |existing| {
                            if (existing != body_ty) {
                                debugPanic("lambdamono.lower.applyFuncValue inconsistent branch return executable types");
                            }
                        } else {
                            result_ty = body_ty;
                        }
                        branches[i] = .{
                            .pat = try self.output.addPat(.{
                                .ty = self.output.getExpr(func_expr).ty,
                                .data = .{ .tag = .{
                                    .name = lower_type.lambdaTagKey(lambda_member.symbol),
                                    .discriminant = lambda_member.discriminant,
                                    .args = ast.Span(ast.PatId).empty(),
                                } },
                            }),
                            .body = body_expr,
                        };
                        continue;
                    }

                    const current_member = solved_types.requireLambdaMember(requested_fn_ty, specialization_symbol);
                    if (lambda_member.capture_ty == null and current_member.captures.len != 0) {
                        debugPanic("lambdamono.lower.applyCallableValueCall missing executable capture payload for lambda member");
                    }
                    const specialized = try self.ensureQueuedCallableSpecializedForCall(
                        solved_types,
                        specialization_symbol,
                        .natural,
                        requested_fn_ty,
                        lambda_member.capture_ty,
                        arg_exprs,
                        result_exec_ty,
                    );
                    if (common_arg_tys == null) {
                        branch_call_args = try self.bridgeCallArgsToExpectedExecutableTypes(
                            inst,
                            mono_cache,
                            arg_source_tys,
                            arg_exprs,
                            specialized.exec_args_tys,
                        );
                        common_arg_tys = try self.allocator.dupe(type_mod.TypeId, specialized.exec_args_tys);
                    } else {
                        for (common_arg_tys.?, specialized.exec_args_tys) |expected_arg_ty, actual_arg_ty| {
                            if (!self.types.equalIds(expected_arg_ty, actual_arg_ty)) {
                                debugPanic("lambdamono.lower.applyCallableValueCall inconsistent branch arg executable types");
                            }
                        }
                    }
                    if (lambda_member.capture_ty == null) {
                        const final_branch_ret_ty = if (self.executableTypeIsAbstract(result_exec_ty) and
                            !self.executableTypeIsAbstract(specialized.exec_ret_ty))
                            specialized.exec_ret_ty
                        else
                            result_exec_ty;
                        var body_expr = try self.output.addExpr(.{
                            .ty = specialized.exec_ret_ty,
                            .data = .{ .call = .{
                                .proc = specialized.symbol,
                                .args = try self.output.addExprSpan(branch_call_args.?),
                            } },
                        });
                        if (!self.types.equalIds(specialized.exec_ret_ty, final_branch_ret_ty)) {
                            body_expr = try self.emitExplicitBridgeExpr(body_expr, final_branch_ret_ty);
                        }
                        const body_ty = self.output.getExpr(body_expr).ty;
                        if (result_ty) |existing| {
                            if (existing != body_ty) {
                                debugPanic("lambdamono.lower.applyFuncValue inconsistent branch return executable types");
                            }
                        } else {
                            result_ty = body_ty;
                        }
                        branches[i] = .{
                            .pat = try self.output.addPat(.{
                                .ty = self.output.getExpr(func_expr).ty,
                                .data = .{ .tag = .{
                                    .name = lower_type.lambdaTagKey(lambda_member.symbol),
                                    .discriminant = lambda_member.discriminant,
                                    .args = ast.Span(ast.PatId).empty(),
                                } },
                            }),
                            .body = body_expr,
                        };
                    } else {
                        const capture_ty = lambda_member.capture_ty.?;
                        const capture_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                        const capture_pat = try self.output.addPat(.{
                            .ty = capture_ty,
                            .data = .{ .var_ = capture_symbol },
                        });
                        const pat = try self.output.addPat(.{
                            .ty = self.output.getExpr(func_expr).ty,
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(lambda_member.symbol),
                                .discriminant = lambda_member.discriminant,
                                .args = try self.output.addPatSpan(&.{capture_pat}),
                            } },
                        });
                        const call_args_with_capture = try self.allocator.alloc(ast.ExprId, branch_call_args.?.len + 1);
                        defer self.allocator.free(call_args_with_capture);
                        @memcpy(call_args_with_capture[0..branch_call_args.?.len], branch_call_args.?);
                        call_args_with_capture[branch_call_args.?.len] = try self.output.addExpr(.{
                            .ty = capture_ty,
                            .data = .{ .var_ = capture_symbol },
                        });
                        var body_expr = try self.output.addExpr(.{
                            .ty = specialized.exec_ret_ty,
                            .data = .{ .call = .{
                                .proc = specialized.symbol,
                                .args = try self.output.addExprSpan(call_args_with_capture),
                            } },
                        });
                        const final_branch_ret_ty = if (self.executableTypeIsAbstract(result_exec_ty) and
                            !self.executableTypeIsAbstract(specialized.exec_ret_ty))
                            specialized.exec_ret_ty
                        else
                            result_exec_ty;
                        if (!self.types.equalIds(specialized.exec_ret_ty, final_branch_ret_ty)) {
                            body_expr = try self.emitExplicitBridgeExpr(body_expr, final_branch_ret_ty);
                        }
                        const body_ty = self.output.getExpr(body_expr).ty;
                        if (result_ty) |existing| {
                            if (existing != body_ty) {
                                debugPanic("lambdamono.lower.applyFuncValue inconsistent branch return executable types");
                            }
                        } else {
                            result_ty = body_ty;
                        }
                        branches[i] = .{
                            .pat = pat,
                            .body = body_expr,
                        };
                    }
                }

                break :blk try self.output.addExpr(.{
                    .ty = result_ty orelse result_exec_ty,
                    .data = .{ .when = .{
                        .cond = func_expr,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                });
            },
            else => debugPanic("lambdamono.lower.applyCallableValueCall expected callable executable type"),
        };
    }

    fn indexTopLevelValues(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .fn_ => |fn_def| try self.top_level_values.put(def.bind.symbol, .{ .fn_ = fn_def }),
                .hosted_fn => |hosted_fn| try self.top_level_values.put(def.bind.symbol, .{ .hosted_fn = hosted_fn }),
                .val => |expr_id| try self.top_level_values.put(def.bind.symbol, .{ .val = expr_id }),
                .run => |run_def| try self.top_level_values.put(def.bind.symbol, .{ .run = run_def }),
            }
        }
    }

    fn indexHostedFunctions(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .hosted_fn => {
                    const sig = try self.buildHostedCallSig(def.bind.ty);
                    errdefer sig.deinit(self.allocator);
                    try self.hosted_call_sigs.put(def.bind.symbol, sig);
                },
                else => {},
            }
        }
    }

    fn lowerHostedDef(
        self: *Lowerer,
        bind: solved.Ast.TypedSymbol,
        hosted_fn: solved.Ast.HostedFnDef,
    ) std.mem.Allocator.Error!ast.Def {
        const sig = self.lookupHostedCallSig(bind.symbol);
        const hosted_args = self.input.store.sliceTypedSymbolSpan(hosted_fn.args);
        if (sig.args_tys.len != hosted_args.len) {
            debugPanic("lambdamono.lower.lowerHostedDef hosted arg arity mismatch");
        }
        const lowered_args = try self.allocator.alloc(ast.TypedSymbol, hosted_args.len);
        defer self.allocator.free(lowered_args);

        for (hosted_args, sig.args_tys, 0..) |arg, arg_exec_ty, i| {
            lowered_args[i] = .{
                .ty = arg_exec_ty,
                .symbol = arg.symbol,
            };
        }

        return .{
            .bind = bind.symbol,
            .result_ty = sig.ret_ty,
            .value = .{ .hosted_fn = .{
                .bind = bind.symbol,
                .args = try self.output.addTypedSymbolSpan(lowered_args),
                .hosted = hosted_fn.hosted,
            } },
        };
    }

    fn buildHostedCallSig(
        self: *Lowerer,
        source_fn_ty: TypeVarId,
    ) std.mem.Allocator.Error!HostedCallSignature {
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        const source_fn = self.input.types.fnShape(source_fn_ty);
        const source_args = self.input.types.sliceTypeVarSpan(source_fn.args);
        const args_tys = try self.allocator.alloc(type_mod.TypeId, source_args.len);
        errdefer self.allocator.free(args_tys);
        for (source_args, 0..) |arg_ty, i| {
            args_tys[i] = try self.lowerErasedBoundaryExecutableTypeIn(&self.input.types, &mono_cache, arg_ty);
        }
        const ret_ty = try self.lowerErasedBoundaryExecutableTypeIn(&self.input.types, &mono_cache, source_fn.ret);
        return .{
            .args_tys = args_tys,
            .ret_ty = ret_ty,
        };
    }

    fn lowerErasedBoundaryNominalArgs(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        args_span: solved.Type.Span(TypeVarId),
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        const args = solved_types.sliceTypeVarSpan(args_span);
        const lowered_args = try self.allocator.alloc(type_mod.TypeId, args.len);
        defer self.allocator.free(lowered_args);
        for (args, 0..) |arg, i| {
            lowered_args[i] = try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, arg);
        }
        return try self.types.dupeTypeIds(lowered_args);
    }

    fn lowerErasedBoundaryExecutableTypeIn(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.internExecutableType(
            try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, ty),
        );
    }

    fn lowerErasedBoundaryExecutableTypeRec(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = solved_types.unlinkPreservingNominal(ty);
        if (mono_cache.active.get(id)) |active| return active;
        if (mono_cache.resolved.get(id)) |cached| return cached;
        if (mono_cache.provisional.get(id)) |provisional| {
            if (self.types.isFullyResolved(provisional)) {
                const canonical = try self.types.internTypeId(provisional);
                const removed = mono_cache.provisional.remove(id);
                if (comptime builtin.mode == .Debug) {
                    std.debug.assert(removed);
                } else if (!removed) {
                    unreachable;
                }
                try mono_cache.resolved.put(id, canonical);
                return canonical;
            }
            return provisional;
        }

        const placeholder = try self.types.addType(.placeholder);
        try mono_cache.active.put(id, placeholder);

        const lowered: type_mod.Content = switch (solved_types.getNode(id)) {
            .link => unreachable,
            .nominal => |nominal| .{ .nominal = .{
                .module_idx = nominal.module_idx,
                .ident = nominal.ident,
                .is_opaque = nominal.is_opaque,
                .args = try self.lowerErasedBoundaryNominalArgs(solved_types, mono_cache, nominal.args),
                .backing = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, nominal.backing),
            } },
            .for_a, .flex_for_a, .unbd => .{ .primitive = .erased },
            .content => |content| switch (content) {
                .func => switch (solved_types.lambdaRepr(id)) {
                    .lset => |lambdas| .{
                        .erased_fn = try self.commonErasedBoundaryCaptureTypeFromLambdas(
                            solved_types,
                            mono_cache,
                            lambdas,
                        ),
                    },
                    .erased => .{ .erased_fn = null },
                },
                .lambda_set => |span| .{
                    .erased_fn = try self.commonErasedBoundaryCaptureTypeFromLambdas(
                        solved_types,
                        mono_cache,
                        solved_types.sliceLambdas(span),
                    ),
                },
                .primitive => |prim| .{ .primitive = prim },
                .list => |elem| .{
                    .list = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, elem),
                },
                .box => |elem| .{
                    .box = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, elem),
                },
                .tuple => |tuple| blk: {
                    const elems = solved_types.sliceTypeVarSpan(tuple);
                    const lowered_elems = try self.types.allocator.alloc(type_mod.TypeId, elems.len);
                    defer self.types.allocator.free(lowered_elems);
                    for (elems, 0..) |elem, i| {
                        lowered_elems[i] = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, elem);
                    }
                    break :blk .{ .tuple = try self.types.dupeTypeIds(lowered_elems) };
                },
                .record => |record| blk: {
                    const fields = solved_types.sliceFields(record.fields);
                    const out = try self.types.allocator.alloc(type_mod.Field, fields.len);
                    defer self.types.allocator.free(out);
                    for (fields, 0..) |field, i| {
                        out[i] = .{
                            .name = field.name,
                            .ty = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, field.ty),
                        };
                    }
                    std.mem.sort(type_mod.Field, out, &self.input.idents, struct {
                        fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                            return std.mem.lessThan(
                                u8,
                                idents.getText(a.name),
                                idents.getText(b.name),
                            );
                        }
                    }.lessThan);
                    assertSortedFields(&self.input.idents, out);
                    break :blk .{ .record = .{
                        .fields = try self.types.dupeFields(out),
                    } };
                },
                .tag_union => |tag_union| blk: {
                    const tags = solved_types.sliceTags(tag_union.tags);
                    const out = try self.types.allocator.alloc(type_mod.Tag, tags.len);
                    defer self.types.allocator.free(out);
                    defer for (out[0..tags.len]) |tag| {
                        if (tag.args.len > 0) self.types.allocator.free(tag.args);
                    };
                    for (tags, 0..) |tag, i| {
                        const args = solved_types.sliceTypeVarSpan(tag.args);
                        const lowered_args = try self.types.allocator.alloc(type_mod.TypeId, args.len);
                        defer self.types.allocator.free(lowered_args);
                        for (args, 0..) |arg, arg_i| {
                            lowered_args[arg_i] = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, arg);
                        }
                        out[i] = .{
                            .name = .{ .ctor = tag.name },
                            .args = try self.types.dupeTypeIds(lowered_args),
                        };
                    }
                    break :blk .{ .tag_union = .{
                        .tags = try self.types.dupeTags(out),
                    } };
                },
            },
        };

        self.types.setType(placeholder, lowered);
        const removed = mono_cache.active.remove(id);
        if (comptime builtin.mode == .Debug) {
            std.debug.assert(removed);
        } else if (!removed) {
            unreachable;
        }
        if (self.types.isFullyResolved(placeholder)) {
            const canonical = try self.types.internTypeId(placeholder);
            try mono_cache.resolved.put(id, canonical);
            return canonical;
        }

        try mono_cache.provisional.put(id, placeholder);
        return placeholder;
    }

    fn commonErasedBoundaryCaptureTypeFromLambdas(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        lambdas: []const solved.Type.Lambda,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        var common: ?type_mod.TypeId = null;
        for (lambdas) |lambda| {
            const captures = solved_types.sliceCaptures(lambda.captures);
            const next: ?type_mod.TypeId = if (captures.len == 0)
                null
            else
                try self.lowerHostedBoundaryCaptures(solved_types, mono_cache, captures);
            if (common == null) {
                common = next;
                continue;
            }
            if (common == null and next == null) continue;
            if (common == null or next == null or common.? != next.?) {
                debugPanic("lambdamono.lower.commonErasedBoundaryCaptureTypeFromLambdas erased-boundary boxed callable variants require a common capture type");
            }
        }
        return common;
    }

    fn lowerHostedBoundaryCaptures(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        captures: []const solved.Type.Capture,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const capture_bindings = try self.types.allocator.alloc(lower_type.CaptureBinding, captures.len);
        defer self.types.allocator.free(capture_bindings);

        for (captures, 0..) |capture, i| {
            capture_bindings[i] = .{
                .symbol = capture.symbol,
                .lowered_ty = try self.lowerErasedBoundaryExecutableTypeRec(solved_types, mono_cache, capture.ty),
            };
        }

        return try lower_type.lowerCaptureBindings(
            solved_types,
            &self.types,
            mono_cache,
            capture_bindings,
            &self.input.symbols,
        );
    }

    fn lowerExecutableCaptureTypeForLambdaMember(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        current_fn_ty: TypeVarId,
        symbol: Symbol,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const captures = solved_types.requireLambdaCaptures(current_fn_ty, symbol);
        if (captures.len == 0) return null;
        return try self.internExecutableType(
            try self.lowerCaptureRecordTypeFromSolved(solved_types, mono_cache, captures),
        );
    }

    fn boxBoundaryBuiltinOp(self: *const Lowerer, requested_name: Symbol) ?base.LowLevel {
        const entry = specializations.lookupFnExact(self.fenv, requested_name) orelse return null;
        if (!self.input.types.hasCapturelessLambda(entry.fn_ty, requested_name)) return null;
        const body = self.input.store.getExpr(entry.fn_def.body);
        return switch (body.data) {
            .low_level => |ll| blk: {
                const args = self.input.store.sliceExprSpan(ll.args);
                if (args.len != 1) break :blk null;
                const arg_expr = self.input.store.getExpr(args[0]);
                const fn_args = self.input.store.sliceTypedSymbolSpan(entry.fn_def.args);
                if (fn_args.len != 1) break :blk null;
                if (arg_expr.data != .var_ or arg_expr.data.var_ != fn_args[0].symbol) break :blk null;
                break :blk switch (ll.op) {
                    .box_box, .box_unbox => ll.op,
                    else => null,
                };
            },
            else => null,
        };
    }

    fn boxBoundaryResultTypeFromArg(
        self: *Lowerer,
        op: base.LowLevel,
        arg_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (op) {
            .box_box => try self.internExecutableType(try self.types.internResolved(.{ .box = arg_ty })),
            .box_unbox => switch (self.types.getTypePreservingNominal(arg_ty)) {
                .box => |elem| elem,
                else => debugPanic("lambdamono.lower.boxBoundaryResultTypeFromArg box_unbox expected boxed executable arg"),
            },
            else => unreachable,
        };
    }

    fn lowLevelResultTypeFromArgs(
        self: *Lowerer,
        op: base.LowLevel,
        args: []const ast.ExprId,
        default_ty: type_mod.TypeId,
    ) type_mod.TypeId {
        return switch (op) {
            .list_append_unsafe,
            .list_concat,
            .list_drop_at,
            .list_sublist,
            .list_set,
            .list_prepend,
            .list_drop_first,
            .list_drop_last,
            .list_take_first,
            .list_take_last,
            .list_reverse,
            .list_reserve,
            .list_release_excess_capacity,
            => if (args.len == 0)
                debugPanic("lambdamono.lower.lowLevelResultTypeFromArgs list op expected args")
            else
                self.output.getExpr(args[0]).ty,
            .list_get_unsafe => if (args.len == 0)
                debugPanic("lambdamono.lower.lowLevelResultTypeFromArgs list_get_unsafe expected args")
            else
                self.listElemType(self.output.getExpr(args[0]).ty) orelse
                    debugPanic("lambdamono.lower.lowLevelResultTypeFromArgs list_get_unsafe expected concrete list arg"),
            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_plus,
            .num_minus,
            .num_times,
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            .num_pow,
            .num_sqrt,
            .num_log,
            .num_round,
            .num_floor,
            .num_ceiling,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            => default_ty,
            else => default_ty,
        };
    }

    fn lowLevelOperandSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        op: base.LowLevel,
        args: solved.Ast.Span(solved.Ast.ExprId),
        expr_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!?TypeVarId {
        const source_args = self.input.store.sliceExprSpan(args);
        const candidate = switch (op) {
            .bool_not,
            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_plus,
            .num_minus,
            .num_times,
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            .num_pow,
            .num_sqrt,
            .num_log,
            .num_round,
            .num_floor,
            .num_ceiling,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            => expr_source_ty,

            .num_is_eq,
            .num_is_gt,
            .num_is_gte,
            .num_is_lt,
            .num_is_lte,
            .num_to_str,
            => if (source_args.len == 0)
                debugPanic("lambdamono.lower.lowLevelOperandSourceTy numeric op expected args")
            else
                try self.instantiatedSourceTypeForExpr(inst, venv, source_args[0]),

            else => null,
        };

        if (candidate == null) return null;
        const operand_source_ty = candidate.?;
        const operand_terminal = inst.types.unlinkPreservingNominal(operand_source_ty);
        switch (inst.types.getNode(operand_terminal)) {
            .unbd, .for_a, .flex_for_a => return null,
            else => {},
        }

        const operand_exec_ty = try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, operand_source_ty);
        return switch (self.types.getType(operand_exec_ty)) {
            .primitive => operand_source_ty,
            else => null,
        };
    }

    fn freshPrimitiveSourceTy(
        _: *Lowerer,
        inst: *InstScope,
        prim: solved.Type.Prim,
    ) std.mem.Allocator.Error!TypeVarId {
        return try inst.types.freshContent(.{ .primitive = prim });
    }

    fn firstListArgSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        args: []const solved.Ast.ExprId,
    ) std.mem.Allocator.Error!TypeVarId {
        var fallback: ?TypeVarId = null;
        for (args) |arg_expr_id| {
            const arg_ty = try self.refinedSourceTypeForExpr(
                inst,
                venv,
                arg_expr_id,
                try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id),
            );
            if (self.solvedListElemType(inst.types, arg_ty) == null) continue;
            if (fallback == null) fallback = arg_ty;
            if (!self.sourceTypeIsAbstract(inst.types, arg_ty)) return arg_ty;
        }
        return fallback orelse
            debugPanic("lambdamono.lower.firstListArgSourceTy expected list-typed low-level arg");
    }

    fn primitiveOperandSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        op: base.LowLevel,
        args: solved.Ast.Span(solved.Ast.ExprId),
        expr_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!?TypeVarId {
        const source_args = self.input.store.sliceExprSpan(args);
        const candidate = switch (op) {
            .bool_not,
            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_plus,
            .num_minus,
            .num_times,
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            .num_pow,
            .num_sqrt,
            .num_log,
            .num_round,
            .num_floor,
            .num_ceiling,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            => expr_source_ty,

            .num_is_eq,
            .num_is_gt,
            .num_is_gte,
            .num_is_lt,
            .num_is_lte,
            .num_to_str,
            => if (source_args.len == 0)
                debugPanic("lambdamono.lower.primitiveOperandSourceTy numeric op expected args")
            else
                try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    source_args[0],
                    try self.instantiatedSourceTypeForExpr(inst, venv, source_args[0]),
                ),

            else => null,
        };

        if (candidate == null) return null;
        const operand_source_ty = candidate.?;
        const operand_terminal = inst.types.unlinkPreservingNominal(operand_source_ty);
        return switch (inst.types.getNode(operand_terminal)) {
            .content => |content| switch (content) {
                .primitive => operand_source_ty,
                else => null,
            },
            else => null,
        };
    }

    fn lowLevelResultSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        op: base.LowLevel,
        args: solved.Ast.Span(solved.Ast.ExprId),
        current_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!?TypeVarId {
        const source_args = self.input.store.sliceExprSpan(args);
        return switch (op) {
            .box_unbox => blk: {
                if (source_args.len != 1) {
                    debugPanic("lambdamono.lower.lowLevelResultSourceTy box_unbox expected one arg");
                }
                const arg_source_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    source_args[0],
                    try self.instantiatedSourceTypeForExpr(inst, venv, source_args[0]),
                );
                break :blk self.solvedBoxElemType(inst.types, arg_source_ty) orelse
                    debugPanic("lambdamono.lower.lowLevelResultSourceTy box_unbox missing solved payload type");
            },
            .box_box => blk: {
                if (source_args.len != 1) {
                    debugPanic("lambdamono.lower.lowLevelResultSourceTy box_box expected one arg");
                }
                const arg_source_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    source_args[0],
                    try self.instantiatedSourceTypeForExpr(inst, venv, source_args[0]),
                );
                const boxed_payload_ty = self.solvedBoxElemType(inst.types, current_source_ty) orelse
                    debugPanic("lambdamono.lower.lowLevelResultSourceTy box_box missing solved payload type");
                try self.unifyIn(inst.types, boxed_payload_ty, arg_source_ty);
                break :blk current_source_ty;
            },

            .str_concat,
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_repeat,
            .str_drop_prefix,
            .str_drop_suffix,
            .str_from_utf8_lossy,
            .str_join_with,
            .num_to_str,
            .u8_to_str,
            .i8_to_str,
            .u16_to_str,
            .i16_to_str,
            .u32_to_str,
            .i32_to_str,
            .u64_to_str,
            .i64_to_str,
            .u128_to_str,
            .i128_to_str,
            .dec_to_str,
            .f32_to_str,
            .f64_to_str,
            => try self.freshPrimitiveSourceTy(inst, .str),

            .bool_not,
            .str_contains,
            .str_caseless_ascii_equals,
            .str_starts_with,
            .str_ends_with,
            .num_is_eq,
            .num_is_gt,
            .num_is_gte,
            .num_is_lt,
            .num_is_lte,
            => try self.freshPrimitiveSourceTy(inst, .bool),

            .str_count_utf8_bytes,
            .list_len,
            => try self.freshPrimitiveSourceTy(inst, .u64),

            .str_to_utf8 => blk: {
                const u8_ty = try self.freshPrimitiveSourceTy(inst, .u8);
                break :blk try inst.types.freshContent(.{ .list = u8_ty });
            },

            .list_append_unsafe,
            .list_concat,
            .list_drop_at,
            .list_sublist,
            .list_set,
            .list_prepend,
            .list_drop_first,
            .list_drop_last,
            .list_take_first,
            .list_take_last,
            .list_reverse,
            .list_reserve,
            .list_release_excess_capacity,
            => try self.firstListArgSourceTy(inst, venv, source_args),

            .list_get_unsafe => blk: {
                const list_source_ty = try self.firstListArgSourceTy(inst, venv, source_args);
                break :blk self.solvedListElemType(inst.types, list_source_ty) orelse
                    debugPanic("lambdamono.lower.lowLevelResultSourceTy list_get_unsafe expected solved list arg");
            },

            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_plus,
            .num_minus,
            .num_times,
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            .num_pow,
            .num_sqrt,
            .num_log,
            .num_round,
            .num_floor,
            .num_ceiling,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            => try self.primitiveOperandSourceTy(inst, venv, op, args, current_source_ty),

            else => null,
        };
    }

    fn lowerSolvedNominalArgs(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        args_span: solved.Type.Span(TypeVarId),
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        const args = solved_types.sliceTypeVarSpan(args_span);
        const lowered_args = try self.allocator.alloc(type_mod.TypeId, args.len);
        defer self.allocator.free(lowered_args);
        for (args, 0..) |arg, i| {
            lowered_args[i] = try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, arg);
        }
        return try self.types.dupeTypeIds(lowered_args);
    }

    fn lowerBoxBoundaryCallableTypeIn(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = solved_types.unlinkPreservingNominal(ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerBoxBoundaryCallableTypeIn(solved_types, mono_cache, nominal.backing);
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(solved_types, mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .func => switch (solved_types.lambdaRepr(id)) {
                    .lset => |lambdas| try self.makeErasedFnType(
                        try self.commonErasedCaptureType(solved_types, mono_cache, id, lambdas),
                    ),
                    .erased => try self.makeErasedFnType(null),
                },
                .lambda_set => |lambdas| try self.makeErasedFnType(
                    try self.commonErasedCaptureType(solved_types, mono_cache, id, solved_types.sliceLambdas(lambdas)),
                ),
                else => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, id),
            },
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => debugPanic("lambdamono.lower.lowerBoxBoundaryCallableType unbound type survived instantiation"),
        };
    }

    fn lowerBoxedBoundaryCallableTypeIn(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = solved_types.unlinkPreservingNominal(ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerBoxedBoundaryCallableTypeIn(solved_types, mono_cache, nominal.backing);
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(solved_types, mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .box => |elem| blk: {
                    const lowered_elem = try self.lowerBoxBoundaryCallableTypeIn(solved_types, mono_cache, elem);
                    break :blk try self.internExecutableType(try self.types.internResolved(.{ .box = lowered_elem }));
                },
                else => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, id),
            },
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => debugPanic("lambdamono.lower.lowerBoxedBoundaryCallableType unbound type survived instantiation"),
        };
    }

    fn eraseBoundaryExecutableType(
        self: *Lowerer,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |nominal| try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = nominal.args,
                    .backing = try self.eraseBoundaryExecutableType(nominal.backing),
                },
            })),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                try self.makeErasedFnType(self.commonErasedCaptureTypeFromExecutable(tag_union.tags))
            else
                ty,
            .erased_fn => ty,
            else => ty,
        };
    }

    fn eraseBoundaryBoxedExecutableType(
        self: *Lowerer,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |nominal| try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = nominal.args,
                    .backing = try self.eraseBoundaryBoxedExecutableType(nominal.backing),
                },
            })),
            .box => |elem| try self.internExecutableType(try self.types.internResolved(.{
                .box = try self.eraseBoundaryExecutableType(elem),
            })),
            else => ty,
        };
    }

    fn unlinkExecutableTypeVar(self: *Lowerer, solved_types: *solved.Type.Store, ty: TypeVarId) TypeVarId {
        const id = solved_types.unlinkPreservingNominal(ty);
        return switch (solved_types.getNode(id)) {
            .nominal => id,
            .content => |content| switch (content) {
                .func => |func| if (self.lambdaSetIsErased(solved_types, func.lset)) id else self.unlinkExecutableTypeVar(solved_types, func.lset),
                else => id,
            },
            else => id,
        };
    }

    fn lambdaSetIsErased(_: *Lowerer, solved_types: *solved.Type.Store, lset: TypeVarId) bool {
        const id = solved_types.unlink(lset);
        return switch (solved_types.getNode(id)) {
            .content => |content| switch (content) {
                .primitive => |prim| prim == .erased,
                else => false,
            },
            else => false,
        };
    }

    fn lowerExecutableTypeNode(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.internExecutableType(try lower_type.lowerType(
            solved_types,
            &self.types,
            mono_cache,
            ty,
            &self.input.symbols,
        ));
    }

    fn lowerErasedFnExecutableType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        solved_ty: TypeVarId,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const id = solved_types.unlinkPreservingNominal(solved_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerErasedFnExecutableType(solved_types, mono_cache, nominal.backing) orelse break :blk null;
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(solved_types, mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .func => switch (solved_types.lambdaRepr(solved_ty)) {
                    .erased => try self.makeErasedFnType(null),
                    .lset => null,
                },
                else => null,
            },
            else => null,
        };
    }

    fn lowerExecutableTypeFromSolvedIn(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        solved_ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const lowered = if (self.boxPayloadRequiresErasure(solved_types, solved_ty))
            try self.lowerBoxedBoundaryCallableTypeIn(solved_types, mono_cache, solved_ty)
        else if (try self.lowerErasedFnExecutableType(solved_types, mono_cache, solved_ty)) |erased|
            erased
        else
            try self.internExecutableType(try self.lowerExecutableTypeNode(solved_types, mono_cache, solved_ty));
        return try self.internExecutableType(lowered);
    }

    fn internExecutableType(self: *Lowerer, ty: type_mod.TypeId) std.mem.Allocator.Error!type_mod.TypeId {
        if (!self.types.isFullyResolved(ty)) {
            debugPanic("lambdamono output invariant violated: unresolved executable type escaped stage boundary");
        }
        return try self.types.internTypeId(ty);
    }

    fn lowerPatternTypeAtSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        solved_ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, solved_ty);
    }

    fn solvedTagArgTypes(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        tag_ty: TypeVarId,
        discriminant: u16,
    ) ?[]const TypeVarId {
        const id = solved_types.unlinkConst(tag_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedTagArgTypes(solved_types, nominal.backing, discriminant),
            .content => |content| switch (content) {
                .tag_union => |tag_union| solved_types.sliceTypeVarSpan(solved_types.sliceTags(tag_union.tags)[discriminant].args),
                else => null,
            },
            else => null,
        };
    }

    fn boxedPayloadType(self: *Lowerer, solved_types: *solved.Type.Store, ty: TypeVarId) ?TypeVarId {
        const id = solved_types.unlinkPreservingNominal(ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.boxedPayloadType(solved_types, nominal.backing),
            .content => |content| switch (content) {
                .box => |elem| elem,
                else => null,
            },
            else => null,
        };
    }

    fn boxPayloadRequiresErasure(self: *Lowerer, solved_types: *solved.Type.Store, ty: TypeVarId) bool {
        const payload = self.boxedPayloadType(solved_types, ty) orelse return false;
        const payload_id = solved_types.unlink(payload);
        return switch (solved_types.getNode(payload_id)) {
            .content => |content| switch (content) {
                .func => switch (solved_types.lambdaRepr(payload)) {
                    .lset => true,
                    .erased => false,
                },
                .lambda_set => true,
                else => false,
            },
            else => false,
        };
    }

    fn recordTypeMatchesFields(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        fields_span: ast.Span(ast.FieldExpr),
    ) bool {
        return switch (self.types.getTypePreservingNominal(record_ty)) {
            .nominal => |nominal| self.recordTypeMatchesFields(nominal.backing, fields_span),
            .record => |record| blk: {
                const record_fields = record.fields;
                const fields = self.output.sliceFieldExprSpan(fields_span);
                if (record_fields.len != fields.len) break :blk false;
                for (record_fields, fields) |record_field, field| {
                    if (record_field.name != field.name or record_field.ty != self.output.getExpr(field.value).ty) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            else => false,
        };
    }

    fn recordTypeFromFields(
        self: *Lowerer,
        fields_span: ast.Span(ast.FieldExpr),
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const fields = self.output.sliceFieldExprSpan(fields_span);
        const lowered_fields = try self.allocator.alloc(type_mod.Field, fields.len);
        defer self.allocator.free(lowered_fields);
        for (fields, 0..) |field, i| {
            lowered_fields[i] = .{
                .name = field.name,
                .ty = self.output.getExpr(field.value).ty,
            };
        }
        std.mem.sort(type_mod.Field, lowered_fields, &self.input.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                return std.mem.lessThan(
                    u8,
                    idents.getText(a.name),
                    idents.getText(b.name),
                );
            }
        }.lessThan);
        return try self.internExecutableType(try self.types.internResolved(.{ .record = .{
            .fields = try self.types.dupeFields(lowered_fields),
        } }));
    }

    fn orderedRecordFields(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        fields_span: ast.Span(ast.FieldExpr),
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const fields = self.output.sliceFieldExprSpan(fields_span);
        if (fields.len <= 1) return fields_span;

        const OrderedField = struct {
            index: u16,
            field: ast.FieldExpr,
        };
        const ordered = try self.allocator.alloc(OrderedField, fields.len);
        defer self.allocator.free(ordered);
        for (fields, 0..) |field, i| {
            const field_info = self.recordFieldByName(record_ty, field.name) orelse
                debugPanic("lambdamono.lower.record missing field in record type");
            ordered[i] = .{
                .index = field_info.index,
                .field = field,
            };
        }
        std.mem.sort(OrderedField, ordered, {}, struct {
            fn lessThan(_: void, a: OrderedField, b: OrderedField) bool {
                return a.index < b.index;
            }
        }.lessThan);

        const sorted = try self.allocator.alloc(ast.FieldExpr, ordered.len);
        defer self.allocator.free(sorted);
        for (ordered, 0..) |entry, i| {
            sorted[i] = entry.field;
        }
        return try self.output.addFieldExprSpan(sorted);
    }

    fn recordFieldType(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        field_index: u16,
    ) ?type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(record_ty)) {
            .nominal => |nominal| self.recordFieldType(nominal.backing, field_index),
            .record => |record| blk: {
                const fields = record.fields;
                const target_index: usize = @intCast(field_index);
                if (target_index >= fields.len) break :blk null;
                break :blk fields[target_index].ty;
            },
            else => null,
        };
    }

    fn recordFieldByName(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        field_name: base.Ident.Idx,
    ) ?struct { index: u16, ty: type_mod.TypeId } {
        return switch (self.types.getTypePreservingNominal(record_ty)) {
            .nominal => |nominal| self.recordFieldByName(nominal.backing, field_name),
            .record => |record| blk: {
                const fields = record.fields;
                for (fields, 0..) |field, i| {
                    if (field.name == field_name) {
                        break :blk .{ .index = @intCast(i), .ty = field.ty };
                    }
                }
                break :blk null;
            },
            else => null,
        };
    }

    fn recordFieldIndexByNameAndType(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        field_name: base.Ident.Idx,
        field_ty: type_mod.TypeId,
    ) ?u16 {
        return switch (self.types.getTypePreservingNominal(record_ty)) {
            .nominal => |nominal| self.recordFieldIndexByNameAndType(nominal.backing, field_name, field_ty),
            .record => |record| blk: {
                const fields = record.fields;
                for (fields, 0..) |field, i| {
                    if (field.name == field_name and field.ty == field_ty) {
                        break :blk @intCast(i);
                    }
                }
                break :blk null;
            },
            else => null,
        };
    }

    fn tupleTypeMatchesElems(
        self: *Lowerer,
        tuple_ty: type_mod.TypeId,
        elems_span: ast.Span(ast.ExprId),
    ) bool {
        return switch (self.types.getTypePreservingNominal(tuple_ty)) {
            .nominal => |nominal| self.tupleTypeMatchesElems(nominal.backing, elems_span),
            .tuple => |tuple| blk: {
                const tuple_elems = tuple;
                const elems = self.output.sliceExprSpan(elems_span);
                if (tuple_elems.len != elems.len) break :blk false;
                for (tuple_elems, elems) |tuple_elem, elem| {
                    if (tuple_elem != self.output.getExpr(elem).ty) break :blk false;
                }
                break :blk true;
            },
            else => false,
        };
    }

    fn tupleElemTypes(
        self: *Lowerer,
        tuple_ty: type_mod.TypeId,
    ) ?[]const type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(tuple_ty)) {
            .nominal => |nominal| self.tupleElemTypes(nominal.backing),
            .tuple => |tuple| tuple,
            else => null,
        };
    }

    fn solvedTupleElemTypes(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        tuple_ty: TypeVarId,
    ) ?[]const TypeVarId {
        const id = solved_types.unlinkConst(tuple_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedTupleElemTypes(solved_types, nominal.backing),
            .content => |content| switch (content) {
                .tuple => |tuple| solved_types.sliceTypeVarSpan(tuple),
                else => null,
            },
            else => null,
        };
    }

    fn solvedTupleElemType(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        tuple_ty: TypeVarId,
        elem_index: u32,
    ) ?TypeVarId {
        const elems = self.solvedTupleElemTypes(solved_types, tuple_ty) orelse return null;
        const target_index: usize = @intCast(elem_index);
        if (target_index >= elems.len) return null;
        return elems[target_index];
    }

    fn solvedRecordFieldByName(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        record_ty: TypeVarId,
        field_name: base.Ident.Idx,
    ) ?TypeVarId {
        const id = solved_types.unlinkConst(record_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedRecordFieldByName(solved_types, nominal.backing, field_name),
            .content => |content| switch (content) {
                .record => |record| blk: {
                    const fields = solved_types.sliceFields(record.fields);
                    for (fields) |field| {
                        if (field.name == field_name) break :blk field.ty;
                    }
                    break :blk null;
                },
                else => null,
            },
            else => null,
        };
    }

    fn listElemType(
        self: *Lowerer,
        list_ty: type_mod.TypeId,
    ) ?type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(list_ty)) {
            .nominal => |nominal| self.listElemType(nominal.backing),
            .list => |elem_ty| elem_ty,
            else => null,
        };
    }

    fn solvedListElemType(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        list_ty: TypeVarId,
    ) ?TypeVarId {
        const id = solved_types.unlinkConst(list_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedListElemType(solved_types, nominal.backing),
            .content => |content| switch (content) {
                .list => |elem_ty| elem_ty,
                else => null,
            },
            else => null,
        };
    }

    fn solvedBoxElemType(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        box_ty: TypeVarId,
    ) ?TypeVarId {
        const id = solved_types.unlinkConst(box_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedBoxElemType(solved_types, nominal.backing),
            .content => |content| switch (content) {
                .box => |elem| elem,
                else => null,
            },
            else => null,
        };
    }

    const SolvedTagInfo = struct {
        discriminant: u16,
        args: []const TypeVarId,
    };

    fn solvedTagInfoByName(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        tag_ty: TypeVarId,
        tag_name: base.Ident.Idx,
    ) ?SolvedTagInfo {
        const id = solved_types.unlinkConst(tag_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedTagInfoByName(solved_types, nominal.backing, tag_name),
            .content => |content| switch (content) {
                .tag_union => |tag_union| blk: {
                    const tags = solved_types.sliceTags(tag_union.tags);
                    for (tags, 0..) |tag, i| {
                        if (tag.name == tag_name) {
                            break :blk .{
                                .discriminant = @intCast(i),
                                .args = solved_types.sliceTypeVarSpan(tag.args),
                            };
                        }
                    }
                    break :blk null;
                },
                else => null,
            },
            else => null,
        };
    }

    fn solvedTagPayloadType(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        union_ty: TypeVarId,
        tag_discriminant: u16,
        payload_index: u16,
    ) ?TypeVarId {
        const id = solved_types.unlinkConst(union_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedTagPayloadType(
                solved_types,
                nominal.backing,
                tag_discriminant,
                payload_index,
            ),
            .content => |content| switch (content) {
                .tag_union => |tag_union| blk: {
                    const tags = solved_types.sliceTags(tag_union.tags);
                    if (tag_discriminant >= tags.len) break :blk null;
                    const args = solved_types.sliceTypeVarSpan(tags[tag_discriminant].args);
                    if (payload_index >= args.len) break :blk null;
                    break :blk args[payload_index];
                },
                else => null,
            },
            else => null,
        };
    }

    const ExecutableTagInfo = struct {
        discriminant: u16,
        args: []const type_mod.TypeId,
    };

    fn executableTagInfoByName(
        self: *Lowerer,
        tag_ty: type_mod.TypeId,
        tag_name: base.Ident.Idx,
    ) ?ExecutableTagInfo {
        return switch (self.types.getTypePreservingNominal(tag_ty)) {
            .nominal => |nominal| self.executableTagInfoByName(nominal.backing, tag_name),
            .tag_union => |tag_union| blk: {
                for (tag_union.tags, 0..) |tag, i| {
                    switch (tag.name) {
                        .ctor => |name| if (name == tag_name) {
                            break :blk .{
                                .discriminant = @intCast(i),
                                .args = tag.args,
                            };
                        },
                        .lambda => {},
                    }
                }
                break :blk null;
            },
            else => null,
        };
    }

    fn executableTagInfoByTagName(
        self: *Lowerer,
        tag_ty: type_mod.TypeId,
        tag_name: type_mod.TagName,
    ) ?ExecutableTagInfo {
        return switch (self.types.getTypePreservingNominal(tag_ty)) {
            .nominal => |nominal| self.executableTagInfoByTagName(nominal.backing, tag_name),
            .tag_union => |tag_union| blk: {
                for (tag_union.tags, 0..) |tag, i| {
                    if (std.meta.eql(tag.name, tag_name)) {
                        break :blk .{
                            .discriminant = @intCast(i),
                            .args = tag.args,
                        };
                    }
                }
                break :blk null;
            },
            else => null,
        };
    }

    fn strengthenTagExecutableType(
        self: *Lowerer,
        tag_ty: type_mod.TypeId,
        tag_name: base.Ident.Idx,
        actual_arg_tys: []const type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(tag_ty)) {
            .nominal => |nominal| try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.concretizeNominalExecutableArgs(nominal.args),
                    .backing = try self.strengthenTagExecutableType(nominal.backing, tag_name, actual_arg_tys),
                },
            })),
            .tag_union => |tag_union| blk: {
                const merged_tags = try self.types.allocator.alloc(type_mod.Tag, tag_union.tags.len);
                for (tag_union.tags, 0..) |tag, i| {
                    switch (tag.name) {
                        .ctor => |name| if (name == tag_name) {
                            if (tag.args.len != actual_arg_tys.len) {
                                debugPanic("lambdamono.lower.strengthenTagExecutableType arg arity mismatch");
                            }
                            const merged_args = try self.types.allocator.alloc(type_mod.TypeId, tag.args.len);
                            for (tag.args, actual_arg_tys, 0..) |existing_arg, actual_arg, arg_i| {
                                merged_args[arg_i] = try self.mergeExecutableSignatureType(existing_arg, actual_arg);
                            }
                            merged_tags[i] = .{
                                .name = tag.name,
                                .args = try self.types.dupeTypeIds(merged_args),
                            };
                            continue;
                        },
                        .lambda => {},
                    }
                    const concretized_args = try self.types.allocator.alloc(type_mod.TypeId, tag.args.len);
                    for (tag.args, 0..) |existing_arg, arg_i| {
                        concretized_args[arg_i] = try self.concretizeAbstractExecutableType(existing_arg);
                    }
                    merged_tags[i] = .{
                        .name = tag.name,
                        .args = concretized_args,
                    };
                }
                if (builtin.mode == .Debug) {
                    for (merged_tags) |merged_tag| {
                        for (merged_tag.args) |arg| {
                            if (@intFromEnum(arg) == 0xAAAAAAAA) {
                                debugPanic("lambdamono.lower.strengthenTagExecutableType produced uninitialized arg type");
                            }
                        }
                    }
                }
                break :blk try self.internExecutableType(try self.types.internResolved(.{
                    .tag_union = .{ .tags = try self.types.dupeTags(merged_tags) },
                }));
            },
            else => tag_ty,
        };
    }

    fn strengthenRecordExecutableType(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        actual_fields: []const ast.FieldExpr,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(record_ty)) {
            .nominal => |nominal| try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.concretizeNominalExecutableArgs(nominal.args),
                    .backing = try self.strengthenRecordExecutableType(nominal.backing, actual_fields),
                },
            })),
            .record => |record| blk: {
                if (record.fields.len != actual_fields.len) {
                    debugPanic("lambdamono.lower.strengthenRecordExecutableType field arity mismatch");
                }
                const merged_fields = try self.types.allocator.alloc(type_mod.Field, record.fields.len);
                for (record.fields, 0..) |existing_field, i| {
                    const actual_field = blk2: {
                        for (actual_fields, 0..) |field, actual_i| {
                            if (field.name == existing_field.name) break :blk2 actual_i;
                        }
                        debugPanic("lambdamono.lower.strengthenRecordExecutableType missing field");
                    };
                    merged_fields[i] = .{
                        .name = existing_field.name,
                        .ty = try self.mergeExecutableSignatureType(
                            existing_field.ty,
                            self.output.getExpr(actual_fields[actual_field].value).ty,
                        ),
                    };
                }
                break :blk try self.internExecutableType(try self.types.internResolved(.{
                    .record = .{ .fields = try self.types.dupeFields(merged_fields) },
                }));
            },
            else => debugPanic("lambdamono.lower.strengthenRecordExecutableType expected record executable type"),
        };
    }

    fn strengthenTupleExecutableType(
        self: *Lowerer,
        tuple_ty: type_mod.TypeId,
        actual_elems: []const ast.ExprId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(tuple_ty)) {
            .nominal => |nominal| try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.concretizeNominalExecutableArgs(nominal.args),
                    .backing = try self.strengthenTupleExecutableType(nominal.backing, actual_elems),
                },
            })),
            .tuple => |tuple| blk: {
                if (tuple.len != actual_elems.len) {
                    debugPanic("lambdamono.lower.strengthenTupleExecutableType elem arity mismatch");
                }
                const merged_elems = try self.types.allocator.alloc(type_mod.TypeId, tuple.len);
                for (tuple, actual_elems, 0..) |existing_elem, actual_elem, i| {
                    merged_elems[i] = try self.mergeExecutableSignatureType(
                        existing_elem,
                        self.output.getExpr(actual_elem).ty,
                    );
                }
                break :blk try self.internExecutableType(try self.types.internResolved(.{
                    .tuple = try self.types.dupeTypeIds(merged_elems),
                }));
            },
            else => debugPanic("lambdamono.lower.strengthenTupleExecutableType expected tuple executable type"),
        };
    }

    fn strengthenListExecutableType(
        self: *Lowerer,
        list_ty: type_mod.TypeId,
        actual_elem_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.types.containsAbstractLeaf(list_ty)) {
            return try self.materializeListLiteralExecutableType(list_ty, actual_elem_ty);
        }
        const concrete_elem_ty = actual_elem_ty orelse return list_ty;
        return switch (self.types.getTypePreservingNominal(list_ty)) {
            .nominal => |nominal| try self.internExecutableType(try self.types.internResolved(.{
                .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.concretizeNominalExecutableArgs(nominal.args),
                    .backing = try self.strengthenListExecutableType(nominal.backing, concrete_elem_ty),
                },
            })),
            .list => |existing_elem_ty| try self.internExecutableType(try self.types.internResolved(.{
                .list = try self.mergeExecutableSignatureType(existing_elem_ty, concrete_elem_ty),
            })),
            else => debugPanic("lambdamono.lower.strengthenListExecutableType expected list executable type"),
        };
    }

    fn tagArgTypes(
        self: *Lowerer,
        tag_ty: type_mod.TypeId,
        discriminant: u16,
    ) ?[]const type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(tag_ty)) {
            .nominal => |nominal| self.tagArgTypes(nominal.backing, discriminant),
            .tag_union => |tag_union| tag_union.tags[discriminant].args,
            else => null,
        };
    }

    fn tupleTypeFromElems(
        self: *Lowerer,
        elems_span: ast.Span(ast.ExprId),
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const elems = self.output.sliceExprSpan(elems_span);
        const lowered_elems = try self.allocator.alloc(type_mod.TypeId, elems.len);
        defer self.allocator.free(lowered_elems);
        for (elems, 0..) |elem, i| {
            lowered_elems[i] = self.output.getExpr(elem).ty;
        }
        return try self.internExecutableType(try self.types.internResolved(.{ .tuple = try self.types.dupeTypeIds(lowered_elems) }));
    }

    fn containsErasedFn(self: *Lowerer, ty: type_mod.TypeId) bool {
        var visited = std.AutoHashMap(type_mod.TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.containsErasedFnRec(ty, &visited);
    }

    fn containsErasedFnRec(
        self: *Lowerer,
        ty: type_mod.TypeId,
        visited: *std.AutoHashMap(type_mod.TypeId, void),
    ) bool {
        var canonical = ty;
        while (true) {
            switch (self.types.types.items[@intFromEnum(canonical)]) {
                .link => |next| canonical = next,
                else => break,
            }
        }
        if (visited.contains(canonical)) return false;
        visited.put(canonical, {}) catch @panic("OOM");
        return switch (self.types.getTypePreservingNominal(canonical)) {
            .erased_fn => true,
            .nominal => |nominal| self.containsErasedFnRec(nominal.backing, visited),
            .box => |elem| self.containsErasedFnRec(elem, visited),
            .list => |elem| self.containsErasedFnRec(elem, visited),
            .tuple => |elems| blk: {
                for (elems) |elem| {
                    if (self.containsErasedFnRec(elem, visited)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (self.containsErasedFnRec(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (self.containsErasedFnRec(arg, visited)) break :blk true;
                    }
                }
                break :blk false;
            },
            else => false,
        };
    }

    fn erasedFnCaptureType(self: *Lowerer, ty: type_mod.TypeId) ?type_mod.TypeId {
        var visited = std.AutoHashMap(type_mod.TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.erasedFnCaptureTypeRec(ty, &visited);
    }

    fn erasedFnCaptureTypeRec(
        self: *Lowerer,
        ty: type_mod.TypeId,
        visited: *std.AutoHashMap(type_mod.TypeId, void),
    ) ?type_mod.TypeId {
        var canonical = ty;
        while (true) {
            switch (self.types.types.items[@intFromEnum(canonical)]) {
                .link => |next| canonical = next,
                else => break,
            }
        }
        if (visited.contains(canonical)) return null;
        visited.put(canonical, {}) catch @panic("OOM");
        return switch (self.types.getTypePreservingNominal(canonical)) {
            .nominal => |nominal| self.erasedFnCaptureTypeRec(nominal.backing, visited),
            .erased_fn => |capture_ty| capture_ty,
            else => null,
        };
    }

    fn ensureTopLevelValueLowered(self: *Lowerer, symbol: Symbol) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.top_level_value_types.get(symbol)) |existing| return existing;

        const source = self.lookupTopLevelValueSource(symbol) orelse
            debugPanic("lambdamono.lower.ensureTopLevelValueLowered missing top-level value");

        const expr_id: solved.Ast.ExprId = switch (source) {
            .fn_ => debugPanic("lambdamono.lower.ensureTopLevelValueLowered expected value source, found function"),
            .hosted_fn => debugPanic("lambdamono.lower.ensureTopLevelValueLowered expected value source, found hosted function"),
            .val => |value_expr| value_expr,
            .run => |run_def| run_def.body,
        };

        const specialized = try self.specializeStandaloneValue(.{ .symbol = symbol, .ty = self.input.store.getExpr(expr_id).ty }, expr_id);
        const result_ty = self.output.getExpr(specialized.expr).ty;
        try self.top_level_value_types.put(symbol, result_ty);

        switch (source) {
            .fn_ => debugPanic("lambdamono.lower.ensureTopLevelValueLowered expected value source, found function"),
            .hosted_fn => debugPanic("lambdamono.lower.ensureTopLevelValueLowered expected value source, found hosted function"),
            .val => {
                try self.pending_values.append(self.allocator, .{
                    .bind = specialized.symbol,
                    .result_ty = result_ty,
                    .value = .{ .val = specialized.expr },
                });
            },
            .run => |run_def| {
                try self.pending_values.append(self.allocator, .{
                    .bind = specialized.symbol,
                    .result_ty = result_ty,
                    .value = .{ .run = .{
                        .body = specialized.expr,
                        .entry_ty = run_def.entry_ty,
                    } },
                });
            },
        }

        return result_ty;
    }

    fn lookupTopLevelValueSource(self: *const Lowerer, symbol: Symbol) ?TopLevelValueSource {
        if (self.top_level_values.get(symbol)) |source| return source;

        const target_origin = self.input.symbols.get(symbol).origin;
        for (self.input.root_defs.items) |def_id| {
            const def = self.input.store.getDef(def_id);
            if (def.bind.symbol != symbol) {
                const def_origin = self.input.symbols.get(def.bind.symbol).origin;
                const matches_origin = switch (target_origin) {
                    .top_level_def => |target| switch (def_origin) {
                        .top_level_def => |candidate| candidate.module_idx == target.module_idx and candidate.def_idx == target.def_idx,
                        else => false,
                    },
                    else => false,
                };
                if (!matches_origin) continue;
            }
            return switch (def.value) {
                .fn_ => |fn_def| .{ .fn_ = fn_def },
                .hosted_fn => |hosted_fn| .{ .hosted_fn = hosted_fn },
                .val => |expr_id| .{ .val = expr_id },
                .run => |run_def| .{ .run = run_def },
            };
        }

        return switch (self.input.symbols.get(symbol).origin) {
            .specialized_top_level_def => |data| self.lookupTopLevelValueSource(@enumFromInt(data.source_symbol)),
            .specialized_local_fn => |data| self.lookupTopLevelValueSource(@enumFromInt(data.source_symbol)),
            .lifted_local_fn => |data| self.lookupTopLevelValueSource(@enumFromInt(data.source_symbol)),
            .lifted_local_fn_alias => |data| self.lookupTopLevelValueSource(@enumFromInt(data.source_symbol)),
            .top_level_def => {
                for (self.input.symbols.entries.items, 0..) |entry, i| {
                    switch (entry.origin) {
                        .specialized_top_level_def => |data| {
                            if (data.source_symbol == symbol.raw()) {
                                if (self.lookupTopLevelValueSource(@enumFromInt(@as(u32, @intCast(i))))) |source| {
                                    return source;
                                }
                            }
                        },
                        else => {},
                    }
                }
                return null;
            },
            else => null,
        };
    }

    const SpecializedStandaloneValue = struct {
        symbol: Symbol,
        expr: ast.ExprId,
    };

    const SpecializedFn = struct {
        fn_def: ast.FnDef,
    };

    const FrozenFnWorld = struct {
        inst: InstScope,
        mono_cache: lower_type.MonoCache,
        fn_ty: TypeVarId,
        fn_shape: solved.Type.FnShape,
        capture_span: solved.Type.Span(solved.Type.Capture),

        fn deinit(self: *FrozenFnWorld) void {
            self.mono_cache.deinit();
            self.inst.deinit();
        }
    };

    const FrozenCallWorld = struct {
        inst: InstScope,
        mono_cache: lower_type.MonoCache,
        env: []EnvEntry,
        fn_ty: TypeVarId,

        fn deinit(self: *FrozenCallWorld, allocator: std.mem.Allocator) void {
            allocator.free(self.env);
            self.mono_cache.deinit();
            self.inst.deinit();
        }
    };

    fn specializeStandaloneValue(
        self: *Lowerer,
        bind: solved.Ast.TypedSymbol,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!SpecializedStandaloneValue {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();
        const bind_ty = try self.cloneTypeIntoInstFromStore(&inst, &self.input.types, bind.ty);
        const initial_expr = try self.specializeExprAtSourceTy(&inst, &mono_cache, &.{}, expr_id, bind_ty);
        const refined_bind_ty = try self.refinedSourceTypeForLoweredExpr(
            &inst,
            &.{},
            expr_id,
            bind_ty,
            initial_expr,
        );
        const effective_bind_ty = if (self.sourceTypeIsAbstract(inst.types, bind_ty))
            refined_bind_ty
        else
            bind_ty;
        const refined_expr = try self.refineBindingBodyIfNeeded(
            &inst,
            &mono_cache,
            &.{},
            expr_id,
            effective_bind_ty,
            initial_expr,
        );
        return .{
            .symbol = bind.symbol,
            .expr = try self.normalizeBindingBodyToSourceTy(
                &inst,
                &mono_cache,
                effective_bind_ty,
                refined_expr,
            ),
        };
    }

    fn freezeFnWorld(self: *Lowerer, pending: specializations.Pending) std.mem.Allocator.Error!FrozenFnWorld {
        const summary_types = pending.summary_types orelse
            debugPanic("lambdamono.lower.freezeFnWorld missing specialization summary store");
        const summary_fn_ty = pending.summary_fn_ty orelse
            debugPanic("lambdamono.lower.freezeFnWorld missing specialization summary type");
        var inst = InstScope.borrow(self.allocator, summary_types);
        errdefer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        errdefer mono_cache.deinit();

        const fn_ty = try self.cloneInstType(&inst, pending.fn_ty);
        try self.unifyIn(inst.types, fn_ty, summary_fn_ty);
        const member = inst.types.requireLambdaMember(fn_ty, pending.name);

        return .{
            .inst = inst,
            .mono_cache = mono_cache,
            .fn_ty = fn_ty,
            .fn_shape = inst.types.fnShape(fn_ty),
            .capture_span = member.lambda.captures,
        };
    }

    fn freezeSpecializedCallWorld(
        self: *Lowerer,
        current_types: *const solved.Type.Store,
        venv: []const EnvEntry,
        summary_types: *const solved.Type.Store,
        summary_fn_ty: TypeVarId,
        arg_tys: []const TypeVarId,
        result_ty: TypeVarId,
    ) std.mem.Allocator.Error!FrozenCallWorld {
        var inst = InstScope.init(self.allocator);
        errdefer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        errdefer mono_cache.deinit();

        var summary_mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer summary_mapping.deinit();
        const cloned_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            &inst,
            summary_types,
            &summary_mapping,
            summary_fn_ty,
        );
        const fn_shape = inst.types.fnShape(cloned_fn_ty);
        const fn_arg_tys = inst.types.sliceTypeVarSpan(fn_shape.args);
        if (fn_arg_tys.len != arg_tys.len) {
            debugPanic("lambdamono.lower.freezeSpecializedCallWorld call arg arity mismatch");
        }

        var current_mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer current_mapping.deinit();
        for (arg_tys, 0..) |arg_ty, i| {
            const cloned_arg_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
                &inst,
                current_types,
                &current_mapping,
                arg_ty,
            );
            try self.unifyIn(inst.types, fn_arg_tys[i], cloned_arg_ty);
        }
        const cloned_result_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            &inst,
            current_types,
            &current_mapping,
            result_ty,
        );
        try self.unifyIn(inst.types, fn_shape.ret, cloned_result_ty);

        const cloned_env = try self.cloneEnvIntoInstFromStoreWithMapping(
            &inst,
            &mono_cache,
            current_types,
            &current_mapping,
            venv,
        );

        return .{
            .inst = inst,
            .mono_cache = mono_cache,
            .env = cloned_env,
            .fn_ty = cloned_fn_ty,
        };
    }

    fn buildExactRequestedFnType(
        _: *Lowerer,
        solved_types: *solved.Type.Store,
        target_symbol: Symbol,
        arg_tys: []const TypeVarId,
        result_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        const empty_captures = try solved_types.addCaptures(&.{});
        const lambda_span = try solved_types.addLambdas(&.{.{
            .symbol = target_symbol,
            .captures = empty_captures,
        }});
        const requested_lset = try solved_types.freshContent(.{ .lambda_set = lambda_span });
        return try solved_types.freshContent(.{ .func = .{
            .args = try solved_types.addTypeVarSpan(arg_tys),
            .lset = requested_lset,
            .ret = result_ty,
        } });
    }

    fn buildExactRequestedFnTypeFromConstraint(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        target_symbol: Symbol,
        constraint_fn_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        const fn_shape = solved_types.fnShape(constraint_fn_ty);
        return try self.buildExactRequestedFnType(
            solved_types,
            target_symbol,
            solved_types.sliceTypeVarSpan(fn_shape.args),
            fn_shape.ret,
        );
    }

    fn refineCallArgSourceTypesIn(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        arg_expr_ids: []const solved.Ast.ExprId,
        expected_arg_tys: []const TypeVarId,
    ) std.mem.Allocator.Error!void {
        if (arg_expr_ids.len != expected_arg_tys.len) {
            debugPanic("lambdamono.lower.refineCallArgSourceTypesIn arg arity mismatch");
        }
        var changed = true;
        while (changed) {
            changed = false;
            for (arg_expr_ids, expected_arg_tys) |arg_expr_id, expected_arg_ty| {
                const expected_id = inst.types.unlinkPreservingNominalConst(expected_arg_ty);
                switch (inst.types.getNode(expected_id)) {
                    .content => |content| switch (content) {
                        .func => {},
                        else => continue,
                    },
                    else => continue,
                }
                const before = try inst.types.structuralKeyOwned(expected_arg_ty);
                defer inst.types.allocator.free(before);
                const refined_arg_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    arg_expr_id,
                    expected_arg_ty,
                );
                try self.unifyIn(inst.types, expected_arg_ty, refined_arg_ty);
                const after = try inst.types.structuralKeyOwned(expected_arg_ty);
                defer inst.types.allocator.free(after);
                if (!std.mem.eql(u8, before, after)) {
                    changed = true;
                }
            }
        }
    }

    fn cloneTypeIntoInstFromStore(
        self: *Lowerer,
        inst: *InstScope,
        source_types: *const solved.Type.Store,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        if (source_types == &self.input.types) {
            return try self.cloneInstType(inst, ty);
        }
        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        return try self.cloneTypeIntoInstFromStoreWithMapping(inst, source_types, &mapping, ty);
    }

    fn cloneTypeIntoInstFromStoreWithMapping(
        self: *Lowerer,
        inst: *InstScope,
        source_types: *const solved.Type.Store,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
        ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        return try self.cloneTypeFromStoreRec(inst.types, source_types, mapping, ty);
    }

    fn cloneEnvIntoInstFromStoreWithMapping(
        self: *Lowerer,
        inst: *InstScope,
        _: *lower_type.MonoCache,
        source_types: *const solved.Type.Store,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
        venv: []const EnvEntry,
    ) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, venv.len);
        errdefer self.allocator.free(out);
        for (venv, 0..) |entry, i| {
            const cloned_ty = try self.cloneTypeIntoInstFromStoreWithMapping(inst, source_types, mapping, entry.ty);
            out[i] = .{
                .symbol = entry.symbol,
                .ty = cloned_ty,
                .exec_ty = entry.exec_ty,
                .exact_fn_symbol = entry.exact_fn_symbol,
            };
        }
        return out;
    }

    fn cloneTypeFromStoreRec(
        self: *Lowerer,
        target_types: *solved.Type.Store,
        source_types: *const solved.Type.Store,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
        ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        const id = source_types.unlinkPreservingNominalConst(ty);
        if (mapping.get(id)) |cached| return cached;

        const cloned = switch (source_types.getNode(id)) {
            .link => unreachable,
            .for_a, .flex_for_a => try target_types.freshFlexForA(),
            .unbd => try target_types.freshUnbd(),
            .nominal => |nominal| blk: {
                const placeholder = try target_types.freshUnbd();
                try mapping.put(id, placeholder);
                const args = source_types.sliceTypeVarSpan(nominal.args);
                const cloned_args = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(cloned_args);
                for (args, 0..) |arg, i| {
                    cloned_args[i] = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, arg);
                }
                target_types.setNode(placeholder, .{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try target_types.addTypeVarSpan(cloned_args),
                    .backing = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, nominal.backing),
                } });
                break :blk placeholder;
            },
            .content => |content| blk: {
                const placeholder = try target_types.freshUnbd();
                try mapping.put(id, placeholder);
                const node = switch (content) {
                    .primitive => solved.Type.Node{ .content = .{ .primitive = content.primitive } },
                    .func => |func| blk2: {
                        const args = source_types.sliceTypeVarSpan(func.args);
                        const out_args = try self.allocator.alloc(TypeVarId, args.len);
                        defer self.allocator.free(out_args);
                        for (args, 0..) |arg, i| {
                            out_args[i] = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, arg);
                        }
                        break :blk2 solved.Type.Node{ .content = .{ .func = .{
                            .args = try target_types.addTypeVarSpan(out_args),
                            .lset = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, func.lset),
                            .ret = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, func.ret),
                        } } };
                    },
                    .list => |elem| solved.Type.Node{ .content = .{
                        .list = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, elem),
                    } },
                    .box => |elem| solved.Type.Node{ .content = .{
                        .box = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, elem),
                    } },
                    .tuple => |tuple| blk2: {
                        const elems = source_types.sliceTypeVarSpan(tuple);
                        const out = try self.allocator.alloc(TypeVarId, elems.len);
                        defer self.allocator.free(out);
                        for (elems, 0..) |elem, i| {
                            out[i] = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, elem);
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .tuple = try target_types.addTypeVarSpan(out),
                        } };
                    },
                    .record => |record| blk2: {
                        const fields = source_types.sliceFields(record.fields);
                        const out = try self.allocator.alloc(solved.Type.Field, fields.len);
                        defer self.allocator.free(out);
                        for (fields, 0..) |field, i| {
                            out[i] = .{
                                .name = field.name,
                                .ty = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, field.ty),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .record = .{ .fields = try target_types.addFields(out) },
                        } };
                    },
                    .tag_union => |tag_union| blk2: {
                        const tags = source_types.sliceTags(tag_union.tags);
                        const out = try self.allocator.alloc(solved.Type.Tag, tags.len);
                        defer self.allocator.free(out);
                        for (tags, 0..) |tag, i| {
                            const args = source_types.sliceTypeVarSpan(tag.args);
                            const out_args = try self.allocator.alloc(TypeVarId, args.len);
                            defer self.allocator.free(out_args);
                            for (args, 0..) |arg, arg_i| {
                                out_args[arg_i] = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, arg);
                            }
                            out[i] = .{
                                .name = tag.name,
                                .args = try target_types.addTypeVarSpan(out_args),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .tag_union = .{ .tags = try target_types.addTags(out) },
                        } };
                    },
                    .lambda_set => |lambda_set| blk2: {
                        const lambdas = source_types.sliceLambdas(lambda_set);
                        const out = try self.allocator.alloc(solved.Type.Lambda, lambdas.len);
                        defer self.allocator.free(out);
                        for (lambdas, 0..) |lambda, i| {
                            const captures = source_types.sliceCaptures(lambda.captures);
                            const out_captures = try self.allocator.alloc(solved.Type.Capture, captures.len);
                            defer self.allocator.free(out_captures);
                            for (captures, 0..) |capture, capture_i| {
                                out_captures[capture_i] = .{
                                    .symbol = capture.symbol,
                                    .ty = try self.cloneTypeFromStoreRec(target_types, source_types, mapping, capture.ty),
                                };
                            }
                            out[i] = .{
                                .symbol = lambda.symbol,
                                .captures = try target_types.addCaptures(out_captures),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .lambda_set = try target_types.addLambdas(out),
                        } };
                    },
                };
                target_types.setNode(placeholder, node);
                break :blk placeholder;
            },
        };

        try mapping.put(id, cloned);
        return cloned;
    }

    fn freezeExactCallWorld(
        self: *Lowerer,
        current_types: *const solved.Type.Store,
        venv: []const EnvEntry,
        target_symbol: Symbol,
        actual_arg_tys: []const TypeVarId,
        result_ty: TypeVarId,
    ) std.mem.Allocator.Error!FrozenCallWorld {
        var inst = InstScope.init(self.allocator);
        errdefer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        errdefer mono_cache.deinit();

        const source_fn_ty = self.lookupTopLevelBindType(target_symbol) orelse
            debugPanic("lambdamono.lower.freezeExactCallWorld missing solved source type for exact target");
        const cloned_source_fn_ty = try self.cloneInstType(&inst, source_fn_ty);

        var current_mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer current_mapping.deinit();
        const fn_arg_tys = try self.allocator.alloc(TypeVarId, actual_arg_tys.len);
        defer self.allocator.free(fn_arg_tys);
        for (actual_arg_tys, 0..) |arg_ty, i| {
            fn_arg_tys[i] = try self.cloneTypeIntoInstFromStoreWithMapping(
                &inst,
                current_types,
                &current_mapping,
                arg_ty,
            );
        }
        const cloned_result_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            &inst,
            current_types,
            &current_mapping,
            result_ty,
        );
        const empty_captures = try inst.types.addCaptures(&.{});
        const lambda_span = try inst.types.addLambdas(&.{.{
            .symbol = target_symbol,
            .captures = empty_captures,
        }});
        const requested_lset = try inst.types.freshContent(.{ .lambda_set = lambda_span });
        const requested_fn_ty = try inst.types.freshContent(.{ .func = .{
            .args = try inst.types.addTypeVarSpan(fn_arg_tys),
            .lset = requested_lset,
            .ret = cloned_result_ty,
        } });
        try self.unifyIn(inst.types, cloned_source_fn_ty, requested_fn_ty);

        const cloned_env = try self.cloneEnvIntoInstFromStoreWithMapping(
            &inst,
            &mono_cache,
            current_types,
            &current_mapping,
            venv,
        );

        return .{
            .inst = inst,
            .mono_cache = mono_cache,
            .env = cloned_env,
            .fn_ty = cloned_source_fn_ty,
        };
    }

    fn freezeExactRequestedCallWorld(
        self: *Lowerer,
        current_types: *const solved.Type.Store,
        venv: []const EnvEntry,
        target_symbol: Symbol,
        requested_fn_ty: TypeVarId,
    ) std.mem.Allocator.Error!FrozenCallWorld {
        var inst = InstScope.init(self.allocator);
        errdefer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        errdefer mono_cache.deinit();

        const source_fn_ty = self.lookupTopLevelBindType(target_symbol) orelse
            debugPanic("lambdamono.lower.freezeExactRequestedCallWorld missing solved source type for exact target");
        const cloned_source_fn_ty = try self.cloneInstType(&inst, source_fn_ty);

        var current_mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer current_mapping.deinit();
        const cloned_requested_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            &inst,
            current_types,
            &current_mapping,
            requested_fn_ty,
        );
        try self.unifyIn(inst.types, cloned_source_fn_ty, cloned_requested_fn_ty);

        const cloned_env = try self.cloneEnvIntoInstFromStoreWithMapping(
            &inst,
            &mono_cache,
            current_types,
            &current_mapping,
            venv,
        );
        return .{
            .inst = inst,
            .mono_cache = mono_cache,
            .env = cloned_env,
            .fn_ty = cloned_source_fn_ty,
        };
    }

    fn freezeCallWorld(
        self: *Lowerer,
        source_types: *const solved.Type.Store,
        venv: []const EnvEntry,
        func_ty: TypeVarId,
        arg_tys: []const TypeVarId,
        result_ty: TypeVarId,
    ) std.mem.Allocator.Error!FrozenCallWorld {
        var inst = InstScope.init(self.allocator);
        errdefer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        errdefer mono_cache.deinit();

        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();

        const cloned_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(&inst, source_types, &mapping, func_ty);
        const fn_shape = inst.types.fnShape(cloned_fn_ty);
        const fn_arg_tys = inst.types.sliceTypeVarSpan(fn_shape.args);
        if (fn_arg_tys.len != arg_tys.len) {
            debugPanic("lambdamono.lower.freezeCallWorld call arg arity mismatch");
        }
        for (arg_tys, 0..) |arg_ty, i| {
            const cloned_arg_ty = try self.cloneTypeIntoInstFromStoreWithMapping(&inst, source_types, &mapping, arg_ty);
            try self.unifyIn(inst.types, fn_arg_tys[i], cloned_arg_ty);
        }
        const cloned_result_ty = try self.cloneTypeIntoInstFromStoreWithMapping(&inst, source_types, &mapping, result_ty);
        try self.unifyIn(inst.types, fn_shape.ret, cloned_result_ty);

        const cloned_env = try self.cloneEnvIntoInstFromStoreWithMapping(&inst, &mono_cache, source_types, &mapping, venv);

        return .{
            .inst = inst,
            .mono_cache = mono_cache,
            .env = cloned_env,
            .fn_ty = cloned_fn_ty,
        };
    }

    fn exactConcreteResultSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        target_symbol: Symbol,
    ) std.mem.Allocator.Error!?TypeVarId {
        const source_fn_ty = self.lookupTopLevelBindType(target_symbol) orelse return null;
        const cloned_source_fn_ty = try self.cloneTypeIntoInstFromStore(inst, &self.input.types, source_fn_ty);
        const source_shape = inst.types.fnShape(cloned_source_fn_ty);
        const source_ret_ty = source_shape.ret;
        if (self.sourceTypeIsAbstract(inst.types, source_ret_ty)) return null;
        return source_ret_ty;
    }

    const AttachedMethodOwner = struct {
        module_idx: u32,
        type_ident: base.Ident.Idx,
    };

    fn maybeAttachedMethodOwnerForType(
        self: *const Lowerer,
        solved_types: *const solved.Type.Store,
        dispatcher_ty: TypeVarId,
    ) ?AttachedMethodOwner {
        const dispatcher_id = solved_types.unlinkPreservingNominalConst(dispatcher_ty);
        return switch (solved_types.getNode(dispatcher_id)) {
            .nominal => |nominal| .{
                .module_idx = nominal.module_idx,
                .type_ident = nominal.ident,
            },
            .content => |content| switch (content) {
                .primitive => |prim| .{
                    .module_idx = self.input.builtin_module_idx,
                    .type_ident = switch (prim) {
                        .bool => self.input.builtin_primitive_owner_idents.bool,
                        .str => self.input.builtin_primitive_owner_idents.str,
                        .u8 => self.input.builtin_primitive_owner_idents.u8,
                        .i8 => self.input.builtin_primitive_owner_idents.i8,
                        .u16 => self.input.builtin_primitive_owner_idents.u16,
                        .i16 => self.input.builtin_primitive_owner_idents.i16,
                        .u32 => self.input.builtin_primitive_owner_idents.u32,
                        .i32 => self.input.builtin_primitive_owner_idents.i32,
                        .u64 => self.input.builtin_primitive_owner_idents.u64,
                        .i64 => self.input.builtin_primitive_owner_idents.i64,
                        .u128 => self.input.builtin_primitive_owner_idents.u128,
                        .i128 => self.input.builtin_primitive_owner_idents.i128,
                        .f32 => self.input.builtin_primitive_owner_idents.f32,
                        .f64 => self.input.builtin_primitive_owner_idents.f64,
                        .dec => self.input.builtin_primitive_owner_idents.dec,
                        .erased => debugPanic(
                            "lambdamono.lower.maybeAttachedMethodOwnerForType reached erased primitive dispatcher",
                        ),
                    },
                },
                .list => .{
                    .module_idx = self.input.builtin_module_idx,
                    .type_ident = self.input.builtin_list_ident,
                },
                .box => .{
                    .module_idx = self.input.builtin_module_idx,
                    .type_ident = self.input.builtin_box_ident,
                },
                else => null,
            },
            else => null,
        };
    }

    fn attachedMethodOwnerForType(
        self: *const Lowerer,
        solved_types: *const solved.Type.Store,
        dispatcher_ty: TypeVarId,
        method_name: base.Ident.Idx,
    ) AttachedMethodOwner {
        return self.maybeAttachedMethodOwnerForType(solved_types, dispatcher_ty) orelse {
            const dispatcher_id = solved_types.unlinkPreservingNominalConst(dispatcher_ty);
            switch (solved_types.getNode(dispatcher_id)) {
                .content => |content| {
                    const key = solved_types.structuralKeyOwned(dispatcher_ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, key, "<oom>")) solved_types.allocator.free(key);
                    debugPanicFmt(
                        "lambdamono.lower.attachedMethodOwnerForType expected monomorphic nominal/list/box dispatcher for method {s}: node={s} key={s}",
                        .{ self.input.idents.getText(method_name), @tagName(content), key },
                    );
                },
                else => {
                    const key = solved_types.structuralKeyOwned(dispatcher_ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, key, "<oom>")) solved_types.allocator.free(key);
                    debugPanicFmt(
                        "lambdamono.lower.attachedMethodOwnerForType expected monomorphic dispatcher type for method {s}: node={s} key={s}",
                        .{ self.input.idents.getText(method_name), @tagName(solved_types.getNode(dispatcher_id)), key },
                    );
                },
            }
        };
    }

    fn attachedMethodOwnerForExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!AttachedMethodOwner {
        const expr_ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
        if (self.maybeAttachedMethodOwnerForType(inst.types, expr_ty)) |owner| {
            return owner;
        }

        const expr = self.input.store.getExpr(expr_id);
        return switch (expr.data) {
            .call => |call| blk: {
                const func_ty = try self.instantiatedSourceTypeForExpr(inst, venv, call.func);
                const arg_expr_ids = self.input.store.sliceExprSpan(call.args);
                const arg_source_tys = try self.allocator.alloc(TypeVarId, arg_expr_ids.len);
                defer self.allocator.free(arg_source_tys);
                for (arg_expr_ids, 0..) |arg_expr_id, i| {
                    arg_source_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
                }

                const base_func_source = self.input.store.getExpr(call.func);
                const direct_func_symbol = self.directCallableSymbolFromExpr(venv, base_func_source);
                if (direct_func_symbol) |symbol| {
                    if (inst.types.hasCapturelessLambda(func_ty, symbol)) {
                        var frozen = try self.freezeExactCallWorld(
                            inst.types,
                            venv,
                            symbol,
                            arg_source_tys,
                            expr_ty,
                        );
                        defer frozen.deinit(self.allocator);
                        break :blk self.attachedMethodOwnerForType(
                            frozen.inst.types,
                            frozen.inst.types.fnShape(frozen.fn_ty).ret,
                            method_name,
                        );
                    }
                }

                var frozen = try self.freezeCallWorld(inst.types, venv, func_ty, arg_source_tys, expr_ty);
                defer frozen.deinit(self.allocator);
                break :blk self.attachedMethodOwnerForType(
                    frozen.inst.types,
                    frozen.inst.types.fnShape(frozen.fn_ty).ret,
                    method_name,
                );
            },
            .dispatch_call => |method_call| blk: {
                const receiver_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, method_call.receiver);
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const target_symbol = try self.resolveAttachedMethodTargetFromExpr(
                    inst,
                    venv,
                    method_call.receiver,
                    method_call.method_name,
                );
                const requested_arg_tys = try self.allocator.alloc(TypeVarId, method_args.len + 1);
                defer self.allocator.free(requested_arg_tys);
                requested_arg_tys[0] = receiver_source_ty;
                for (method_args, 0..) |arg_expr_id, i| {
                    requested_arg_tys[i + 1] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
                }
                const exact_requested_ty = try self.buildExactRequestedFnType(
                    inst.types,
                    target_symbol,
                    requested_arg_tys,
                    expr_ty,
                );
                _ = exact_requested_ty;
                var frozen = try self.freezeExactCallWorld(
                    inst.types,
                    venv,
                    target_symbol,
                    requested_arg_tys,
                    expr_ty,
                );
                defer frozen.deinit(self.allocator);
                const frozen_shape = frozen.inst.types.fnShape(frozen.fn_ty);
                const frozen_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen_shape.args);
                const refined_receiver_ty = try self.refinedSourceTypeForExpr(
                    &frozen.inst,
                    frozen.env,
                    method_call.receiver,
                    frozen_arg_tys[0],
                );
                try self.unifyIn(frozen.inst.types, frozen_arg_tys[0], refined_receiver_ty);
                try self.refineCallArgSourceTypesIn(
                    &frozen.inst,
                    frozen.env,
                    method_args,
                    frozen_arg_tys[1..],
                );
                break :blk self.attachedMethodOwnerForType(
                    frozen.inst.types,
                    frozen_shape.ret,
                    method_name,
                );
            },
            .type_dispatch_call => |method_call| blk: {
                var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
                defer mapping.deinit();
                const dispatcher_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
                    inst,
                    &self.input.types,
                    &mapping,
                    method_call.dispatcher_ty,
                );
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const target_symbol = self.resolveAttachedMethodTarget(
                    inst.types,
                    dispatcher_ty,
                    method_call.method_name,
                );
                const requested_arg_tys = try self.allocator.alloc(TypeVarId, method_args.len);
                defer self.allocator.free(requested_arg_tys);
                for (method_args, 0..) |arg_expr_id, i| {
                    requested_arg_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
                }
                const exact_requested_ty = try self.buildExactRequestedFnType(
                    inst.types,
                    target_symbol,
                    requested_arg_tys,
                    expr_ty,
                );
                _ = exact_requested_ty;
                var frozen = try self.freezeExactCallWorld(
                    inst.types,
                    venv,
                    target_symbol,
                    requested_arg_tys,
                    expr_ty,
                );
                defer frozen.deinit(self.allocator);
                const frozen_shape = frozen.inst.types.fnShape(frozen.fn_ty);
                const frozen_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen_shape.args);
                try self.refineCallArgSourceTypesIn(
                    &frozen.inst,
                    frozen.env,
                    method_args,
                    frozen_arg_tys,
                );
                break :blk self.attachedMethodOwnerForType(
                    frozen.inst.types,
                    frozen_shape.ret,
                    method_name,
                );
            },
            else => self.attachedMethodOwnerForType(inst.types, expr_ty, method_name),
        };
    }

    fn resolveAttachedMethodTargetFromExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        method_name: base.Ident.Idx,
    ) std.mem.Allocator.Error!Symbol {
        const owner = try self.attachedMethodOwnerForExpr(inst, venv, expr_id, method_name);
        const raw_symbol = self.input.attached_method_index.get(.{
            .module_idx = owner.module_idx,
            .type_ident = owner.type_ident,
            .method_ident = method_name,
        }) orelse debugPanic(
            "lambdamono.lower.resolveAttachedMethodTargetFromExpr missing attached method for monomorphic dispatcher",
        );
        return self.exactCallableSymbolForBinding(raw_symbol) orelse debugPanicFmt(
            "lambdamono.lower.resolveAttachedMethodTargetFromExpr attached method {s} resolved to binding {d} but not an exact callable",
            .{ self.input.idents.getText(method_name), raw_symbol.raw() },
        );
    }

    fn resolveAttachedMethodTarget(
        self: *const Lowerer,
        solved_types: *const solved.Type.Store,
        dispatcher_ty: TypeVarId,
        method_name: base.Ident.Idx,
    ) Symbol {
        const owner = self.attachedMethodOwnerForType(solved_types, dispatcher_ty, method_name);
        const raw_symbol = self.input.attached_method_index.get(.{
            .module_idx = owner.module_idx,
            .type_ident = owner.type_ident,
            .method_ident = method_name,
        }) orelse debugPanic(
            "lambdamono.lower.resolveAttachedMethodTarget missing attached method for monomorphic dispatcher",
        );
        return self.exactCallableSymbolForBinding(raw_symbol) orelse debugPanicFmt(
            "lambdamono.lower.resolveAttachedMethodTarget attached method {s} resolved to binding {d} but not an exact callable",
            .{ self.input.idents.getText(method_name), raw_symbol.raw() },
        );
    }

    fn specializeFn(self: *Lowerer, pending_symbol: Symbol) std.mem.Allocator.Error!SpecializedFn {
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.specializeFn missing queued specialization");
        const previous_specializing = self.current_specializing_symbol;
        self.current_specializing_symbol = pending_symbol;
        defer self.current_specializing_symbol = previous_specializing;
        var frozen = try self.freezeFnWorld(pending.*);
        defer frozen.deinit();
        const fn_body = pending.fn_def.body;
        const frozen_captures = frozen.inst.types.sliceCaptures(frozen.capture_span);
        const fn_args = self.input.store.sliceTypedSymbolSpan(pending.fn_def.args);
        const requested_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen.fn_shape.args);
        if (fn_args.len != requested_arg_tys.len) {
            debugPanic("lambdamono.lower.specializeFn function arg arity mismatch");
        }
        const initial_arg_exec_tys = try self.allocator.alloc(type_mod.TypeId, requested_arg_tys.len);
        defer self.allocator.free(initial_arg_exec_tys);
        for (requested_arg_tys, 0..) |arg_ty, i| {
            initial_arg_exec_tys[i] = switch (pending.repr_mode) {
                .natural => try self.lowerExecutableTypeFromSolvedIn(
                    frozen.inst.types,
                    &frozen.mono_cache,
                    arg_ty,
                ),
                .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(
                    frozen.inst.types,
                    &frozen.mono_cache,
                    arg_ty,
                ),
            };
        }
        switch (frozen.inst.types.lambdaRepr(frozen.fn_ty)) {
            .lset => {},
            .erased => debugPanic("lambdamono.lower.specializeFn expected concrete solved lambda-set type"),
        }
        const initial_ret_exec_ty = switch (pending.repr_mode) {
            .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(
                frozen.inst.types,
                &frozen.mono_cache,
                frozen.fn_shape.ret,
            ),
            .natural => blk: {
                const lowered_ret_ty = try self.lowerExecutableTypeFromSolvedIn(
                    frozen.inst.types,
                    &frozen.mono_cache,
                    frozen.fn_shape.ret,
                );
                break :blk lowered_ret_ty;
            },
        };
        try self.mergePendingExecutableSignature(pending, initial_arg_exec_tys, initial_ret_exec_ty);
        const proc_sig = self.requirePendingExecutableSignature(pending);
        var final_source_ret_ty = frozen.fn_shape.ret;
        const result: ast.FnDef = switch (pending.repr_mode) {
            .natural => if (frozen_captures.len == 0) .{
                .args = blk: {
                    const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len);
                    defer self.allocator.free(lowered_args);
                    for (fn_args, proc_sig.args_tys, 0..) |arg, exec_ty, i| {
                        lowered_args[i] = .{
                            .ty = exec_ty,
                            .symbol = arg.symbol,
                        };
                    }
                    break :blk try self.output.addTypedSymbolSpan(lowered_args);
                },
                .body = blk: {
                    const body_env = try self.buildFnBodyEnv(
                        frozen.inst.types,
                        requested_arg_tys,
                        proc_sig.args_tys,
                        fn_args,
                        &[_]EnvEntry{},
                    );
                    defer self.allocator.free(body_env);
                    const body_expr = self.input.store.getExpr(fn_body);
                    const body_default_ty = if (self.executableTypeIsAbstract(proc_sig.ret_ty))
                        try self.lowerExecutableTypeFromSolvedIn(
                            frozen.inst.types,
                            &frozen.mono_cache,
                            frozen.fn_shape.ret,
                        )
                    else
                        proc_sig.ret_ty;
                    const body = try self.specializeExprWithDefaultTy(
                        &frozen.inst,
                        &frozen.mono_cache,
                        body_env,
                        fn_body,
                        body_expr,
                        frozen.fn_shape.ret,
                        body_default_ty,
                    );
                    final_source_ret_ty = try self.refinedSourceTypeForLoweredExpr(
                        &frozen.inst,
                        body_env,
                        fn_body,
                        frozen.fn_shape.ret,
                        body,
                    );
                    const body_ty = self.output.getExpr(body).ty;
                    const fn_ret_exec_ty = try self.mergeExecutableSignatureType(proc_sig.ret_ty, body_ty);
                    const bridged = if (!self.types.equalIds(body_ty, fn_ret_exec_ty))
                        try self.retargetOrEmitExplicitBridgeExpr(body, fn_ret_exec_ty)
                    else
                        body;
                    break :blk bridged;
                },
            } else blk: {
                const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                const capture_exec_ty = proc_sig.capture_ty orelse
                    debugPanic("lambdamono.lower.specializeFn missing queued executable capture signature");
                const capture_env = try self.captureBindingsFromCaptures(
                    frozen.inst.types,
                    frozen_captures,
                    capture_exec_ty,
                );
                defer self.allocator.free(capture_env);
                const body_env = try self.buildFnBodyEnv(
                    frozen.inst.types,
                    requested_arg_tys,
                    proc_sig.args_tys,
                    fn_args,
                    capture_env,
                );
                defer self.allocator.free(body_env);

                const body_expr = self.input.store.getExpr(fn_body);
                const body_default_ty = if (self.executableTypeIsAbstract(proc_sig.ret_ty))
                    try self.lowerExecutableTypeFromSolvedIn(
                        frozen.inst.types,
                        &frozen.mono_cache,
                        frozen.fn_shape.ret,
                    )
                else
                    proc_sig.ret_ty;
                var body = try self.specializeExprWithDefaultTy(
                    &frozen.inst,
                    &frozen.mono_cache,
                    body_env,
                    fn_body,
                    body_expr,
                    frozen.fn_shape.ret,
                    body_default_ty,
                );
                final_source_ret_ty = try self.refinedSourceTypeForLoweredExpr(
                    &frozen.inst,
                    body_env,
                    fn_body,
                    frozen.fn_shape.ret,
                    body,
                );
                const body_ty = self.output.getExpr(body).ty;
                const fn_ret_exec_ty = try self.mergeExecutableSignatureType(proc_sig.ret_ty, body_ty);
                if (!self.types.equalIds(body_ty, fn_ret_exec_ty)) {
                    body = try self.retargetOrEmitExplicitBridgeExpr(body, fn_ret_exec_ty);
                }
                body = try self.bindCaptureLets(
                    captures_symbol,
                    capture_exec_ty,
                    capture_env,
                    body,
                );
                const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len + 1);
                defer self.allocator.free(lowered_args);
                for (fn_args, proc_sig.args_tys, 0..) |arg, exec_ty, i| {
                    lowered_args[i] = .{
                        .ty = exec_ty,
                        .symbol = arg.symbol,
                    };
                }
                lowered_args[fn_args.len] = .{
                    .ty = capture_exec_ty,
                    .symbol = captures_symbol,
                };
                const args_span = try self.output.addTypedSymbolSpan(lowered_args);
                break :blk .{
                    .args = args_span,
                    .body = body,
                };
            },
            .erased_boundary => blk: {
                var captures_ty: ?type_mod.TypeId = null;
                var capture_env: []EnvEntry = &[_]EnvEntry{};
                defer if (capture_env.len != 0) self.allocator.free(capture_env);
                if (frozen_captures.len != 0) {
                    const lowered_capture_ty = proc_sig.capture_ty orelse
                        debugPanic("lambdamono.lower.specializeFn missing queued erased executable capture signature");
                    captures_ty = lowered_capture_ty;
                    capture_env = try self.captureBindingsFromCaptures(
                        frozen.inst.types,
                        frozen_captures,
                        lowered_capture_ty,
                    );
                }
                const body_env = try self.buildFnBodyEnv(
                    frozen.inst.types,
                    requested_arg_tys,
                    proc_sig.args_tys,
                    fn_args,
                    capture_env,
                );
                defer self.allocator.free(body_env);

                const body_expr = self.input.store.getExpr(fn_body);
                const body_default_ty = if (self.executableTypeIsAbstract(proc_sig.ret_ty))
                    try self.lowerExecutableTypeFromSolvedIn(
                        frozen.inst.types,
                        &frozen.mono_cache,
                        frozen.fn_shape.ret,
                    )
                else
                    proc_sig.ret_ty;
                var body = try self.specializeExprWithDefaultTy(
                    &frozen.inst,
                    &frozen.mono_cache,
                    body_env,
                    fn_body,
                    body_expr,
                    frozen.fn_shape.ret,
                    body_default_ty,
                );
                final_source_ret_ty = try self.refinedSourceTypeForLoweredExpr(
                    &frozen.inst,
                    body_env,
                    fn_body,
                    frozen.fn_shape.ret,
                    body,
                );
                if (self.types.getTypePreservingNominal(proc_sig.ret_ty) == .erased_fn and
                    frozen.inst.types.maybeLambdaRepr(frozen.fn_shape.ret) != null)
                {
                    body = try self.lowerBoxBoundaryExpr(
                        &frozen.inst,
                        &frozen.mono_cache,
                        body_env,
                        frozen.fn_shape.ret,
                        body,
                    );
                }
                const body_ty = self.output.getExpr(body).ty;
                const fn_ret_exec_ty = try self.mergeExecutableSignatureType(proc_sig.ret_ty, body_ty);
                if (!self.types.equalIds(body_ty, fn_ret_exec_ty)) {
                    body = try self.retargetOrEmitExplicitBridgeExpr(body, fn_ret_exec_ty);
                }
                if (capture_env.len != 0) {
                    const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                    body = try self.bindCaptureLets(captures_symbol, captures_ty.?, capture_env, body);
                    break :blk .{
                        .args = blk_args: {
                            const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len + 1);
                            defer self.allocator.free(lowered_args);
                            for (fn_args, proc_sig.args_tys, 0..) |arg, exec_ty, i| {
                                lowered_args[i] = .{
                                    .ty = exec_ty,
                                    .symbol = arg.symbol,
                                };
                            }
                            lowered_args[fn_args.len] = .{
                                .ty = captures_ty.?,
                                .symbol = captures_symbol,
                            };
                            break :blk_args try self.output.addTypedSymbolSpan(lowered_args);
                        },
                        .body = body,
                    };
                }
                break :blk .{
                    .args = blk_args: {
                        const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len);
                        defer self.allocator.free(lowered_args);
                        for (fn_args, proc_sig.args_tys, 0..) |arg, exec_ty, i| {
                            lowered_args[i] = .{
                                .ty = exec_ty,
                                .symbol = arg.symbol,
                            };
                        }
                        break :blk_args try self.output.addTypedSymbolSpan(lowered_args);
                    },
                    .body = body,
                };
            },
        };
        try self.unifyIn(frozen.inst.types, frozen.fn_shape.ret, final_source_ret_ty);
        const updated_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.specializeFn lost queued specialization before final signature merge");
        try self.mergePendingExecutableReturnSignature(updated_pending, self.output.getExpr(result.body).ty);
        return .{
            .fn_def = result,
        };
    }

    fn buildFnBodyEnv(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        arg_tys: []const TypeVarId,
        arg_exec_tys: []const type_mod.TypeId,
        args: []const solved.Ast.TypedSymbol,
        capture_env: []const EnvEntry,
    ) std.mem.Allocator.Error![]EnvEntry {
        if (arg_tys.len != arg_exec_tys.len or arg_tys.len != args.len) {
            debugPanic("lambdamono.lower.buildFnBodyEnv arg arity mismatch");
        }
        const out = try self.allocator.alloc(EnvEntry, capture_env.len + args.len);
        for (args, arg_tys, arg_exec_tys, 0..) |arg, arg_ty, arg_exec_ty, i| {
            const exact_fn_symbol = self.singletonExactCallableSymbolForExecutableType(arg_exec_ty) orelse
                self.singletonExactCallableSymbolForSourceType(solved_types, arg_ty);
            out[i] = .{
                .symbol = arg.symbol,
                .ty = arg_ty,
                .exec_ty = arg_exec_ty,
                .exact_fn_symbol = exact_fn_symbol,
            };
        }
        for (capture_env, 0..) |capture, i| {
            out[i + args.len] = capture;
        }
        return out;
    }

    fn lowerCaptureRecordTypeFromSolved(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        captures: []const solved.Type.Capture,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.internExecutableType(try lower_type.lowerCaptures(
            solved_types,
            &self.types,
            mono_cache,
            captures,
            &self.input.symbols,
        ));
    }

    const CurrentCapturePayload = struct {
        env: []EnvEntry,
        ty: ?type_mod.TypeId,

        fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
            if (self.env.len != 0) allocator.free(self.env);
        }
    };

    fn currentCapturePayloadFromEnv(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        captures: []const solved.Type.Capture,
        venv: []const EnvEntry,
    ) std.mem.Allocator.Error!CurrentCapturePayload {
        if (captures.len == 0) {
            return .{ .env = &.{}, .ty = null };
        }

        const capture_env = try self.allocator.alloc(EnvEntry, captures.len);
        errdefer self.allocator.free(capture_env);
        const capture_bindings = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        defer self.allocator.free(capture_bindings);

        for (captures, 0..) |capture, i| {
            const entry = self.lookupEnvEntry(venv, capture.symbol) orelse
                debugPanic("lambdamono.lower.currentCapturePayloadFromEnv missing current capture binding");
            capture_env[i] = entry;
            capture_bindings[i] = .{
                .symbol = capture.symbol,
                .lowered_ty = entry.exec_ty,
            };
        }

        return .{
            .env = capture_env,
            .ty = try self.internExecutableType(try lower_type.lowerCaptureBindings(
                &self.input.types,
                &self.types,
                mono_cache,
                capture_bindings,
                &self.input.symbols,
            )),
        };
    }

    fn queueCallableSpecializationWithExplicitSignature(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        symbol: Symbol,
        repr_mode: specializations.Pending.ReprMode,
        requested_ty: TypeVarId,
        capture_exec_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!Symbol {
        const fn_shape = solved_types.fnShape(requested_ty);
        const arg_source_tys = solved_types.sliceTypeVarSpan(fn_shape.args);
        const exec_arg_tys = try self.allocator.alloc(type_mod.TypeId, arg_source_tys.len);
        defer self.allocator.free(exec_arg_tys);
        for (arg_source_tys, 0..) |arg_ty, i| {
            exec_arg_tys[i] = switch (repr_mode) {
                .natural => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, arg_ty),
                .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, arg_ty),
            };
        }
        const exec_ret_ty = switch (repr_mode) {
            .natural => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, fn_shape.ret),
            .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, fn_shape.ret),
        };
        return try specializations.specializeFnWithExecArgs(
            &self.queue,
            self.fenv,
            solved_types,
            &self.input.symbols,
            symbol,
            repr_mode,
            requested_ty,
            &self.types,
            capture_exec_ty,
            exec_arg_tys,
            exec_ret_ty,
        );
    }

    fn bindCaptureLets(
        self: *Lowerer,
        captures_symbol: Symbol,
        capture_record_ty: type_mod.TypeId,
        captures: []const EnvEntry,
        body: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var result = body;
        var idx = captures.len;
        while (idx > 0) {
            idx -= 1;
            const capture = captures[idx];
            const capture_name = self.input.symbols.get(capture.symbol).name;
            const field_info = self.recordFieldByName(capture_record_ty, capture_name) orelse
                debugPanic("lambdamono.lower.bindCaptureLets missing capture field");
            const captures_var = try self.output.addExpr(.{
                .ty = capture_record_ty,
                .data = .{ .var_ = captures_symbol },
            });
            const access = try self.output.addExpr(.{
                .ty = field_info.ty,
                .data = .{ .access = .{
                    .record = captures_var,
                    .field = capture_name,
                    .field_index = field_info.index,
                } },
            });
            result = try self.output.addExpr(.{
                .ty = self.output.getExpr(result).ty,
                .data = .{ .let_ = .{
                    .bind = .{
                        .ty = field_info.ty,
                        .symbol = capture.symbol,
                    },
                    .body = access,
                    .rest = result,
                } },
            });
        }
        return result;
    }

    fn specializeExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
        return try self.specializeExprAtSourceTy(inst, mono_cache, venv, expr_id, ty);
    }

    fn specializeExprAtSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.input.store.getExpr(expr_id);
        const default_ty = try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, ty);
        return try self.specializeExprWithDefaultTy(inst, mono_cache, venv, expr_id, expr, ty, default_ty);
    }

    fn specializeExprWithDefaultTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        expr: solved.Ast.Expr,
        ty: TypeVarId,
        default_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (expr.data == .inspect) {
            return try self.specializeInspectExpr(inst, mono_cache, venv, expr.data.inspect, default_ty);
        }
        const specialized: SpecializedExprLowering = switch (expr.data) {
            .var_ => |symbol| blk: {
                break :blk try self.specializeVarExpr(inst, mono_cache, venv, symbol, ty, default_ty);
            },
            .int_lit => |value| .{ .ty = default_ty, .data = try self.specializeIntLiteral(default_ty, value) },
            .frac_f32_lit => |value| .{ .ty = default_ty, .data = try self.specializeF32Literal(default_ty, value) },
            .frac_f64_lit => |value| .{ .ty = default_ty, .data = try self.specializeF64Literal(default_ty, value) },
            .dec_lit => |value| .{ .ty = default_ty, .data = try self.specializeDecLiteral(default_ty, value) },
            .str_lit => |value| .{ .ty = default_ty, .data = .{ .str_lit = value } },
            .bool_lit => |value| .{ .ty = default_ty, .data = .{ .bool_lit = value } },
            .unit => .{ .ty = default_ty, .data = .unit },
            .tag => |tag| blk: {
                const source_args = self.input.store.sliceExprSpan(tag.args);
                const lowered_args = try self.allocator.alloc(ast.ExprId, source_args.len);
                defer self.allocator.free(lowered_args);

                const solved_tag_info = self.solvedTagInfoByName(inst.types, ty, tag.name) orelse
                    debugPanic("lambdamono.lower.tag missing solved tag info");
                const tag_info = self.executableTagInfoByName(default_ty, tag.name) orelse
                    debugPanic("lambdamono.lower.tag missing executable tag info");
                if (tag_info.args.len != source_args.len) {
                    debugPanic("lambdamono.lower.tag expected tag arg count mismatch");
                }
                if (solved_tag_info.args.len != source_args.len) {
                    debugPanic("lambdamono.lower.tag expected solved tag arg count mismatch");
                }

                for (source_args, 0..) |arg_expr_id, i| {
                    const lowered = try self.specializeExprAtSourceTy(
                        inst,
                        mono_cache,
                        venv,
                        arg_expr_id,
                        solved_tag_info.args[i],
                    );
                    lowered_args[i] = lowered;
                }

                const tag_ty = if (self.executableTypeIsAbstract(default_ty)) blk_ty: {
                    const actual_arg_tys = try self.allocator.alloc(type_mod.TypeId, lowered_args.len);
                    defer if (actual_arg_tys.len != 0) self.allocator.free(actual_arg_tys);
                    for (lowered_args, 0..) |lowered_arg, i| {
                        actual_arg_tys[i] = self.output.getExpr(lowered_arg).ty;
                    }
                    break :blk_ty try self.strengthenTagExecutableType(
                        default_ty,
                        tag.name,
                        actual_arg_tys,
                    );
                } else default_ty;
                const tag_exec_info = self.executableTagInfoByName(tag_ty, tag.name) orelse
                    debugPanic("lambdamono.lower.tag missing executable tag info");
                for (lowered_args, tag_exec_info.args, 0..) |lowered_arg, strengthened_arg_ty, i| {
                    if (!self.types.equalIds(self.output.getExpr(lowered_arg).ty, strengthened_arg_ty)) {
                        lowered_args[i] = try self.retargetOrEmitExplicitBridgeExpr(lowered_arg, strengthened_arg_ty);
                    }
                }

                break :blk .{ .ty = tag_ty, .data = .{ .tag = .{
                    .name = .{ .ctor = tag.name },
                    .discriminant = tag_exec_info.discriminant,
                    .args = try self.output.addExprSpan(lowered_args),
                } } };
            },
            .record => |fields| blk: {
                const source_fields = self.input.store.sliceFieldExprSpan(fields);
                const lowered = try self.allocator.alloc(ast.FieldExpr, source_fields.len);
                defer self.allocator.free(lowered);

                for (source_fields, 0..) |field, i| {
                    const source_field_ty = self.solvedRecordFieldByName(inst.types, ty, field.name) orelse
                        debugPanic("lambdamono.lower.record missing solved record field");
                    const value = try self.specializeExprAtSourceTy(inst, mono_cache, venv, field.value, source_field_ty);
                    lowered[i] = .{
                        .name = field.name,
                        .value = value,
                    };
                }
                const record_ty = if (self.executableTypeIsAbstract(default_ty)) switch (self.types.getTypePreservingNominal(default_ty)) {
                    .nominal, .record => try self.strengthenRecordExecutableType(default_ty, lowered),
                    else => blk2: {
                        const lowered_fields = try self.output.addFieldExprSpan(lowered);
                        break :blk2 try self.recordTypeFromFields(lowered_fields);
                    },
                } else default_ty;
                for (lowered) |*field| {
                    const expected_field = self.recordFieldByName(record_ty, field.name) orelse
                        debugPanic("lambdamono.lower.record missing executable record field");
                    if (!self.types.equalIds(self.output.getExpr(field.value).ty, expected_field.ty)) {
                        field.value = try self.retargetOrEmitExplicitBridgeExpr(field.value, expected_field.ty);
                    }
                }
                const lowered_fields = try self.output.addFieldExprSpan(lowered);
                const ordered_fields = try self.orderedRecordFields(record_ty, lowered_fields);
                break :blk .{ .ty = record_ty, .data = .{ .record = ordered_fields } };
            },
            .access => |access| blk: {
                const record = try self.specializeExpr(inst, mono_cache, venv, access.record);
                const record_ty = self.output.getExpr(record).ty;
                const field_info = self.recordFieldByName(record_ty, access.field) orelse
                    debugPanic("lambdamono.lower.access missing record field");
                const field_ty = field_info.ty;
                const result_ty = if (self.types.containsAbstractLeaf(default_ty))
                    field_ty
                else
                    try self.mergeExecutableSignatureType(default_ty, field_ty);
                const access_expr = try self.output.addExpr(.{
                    .ty = field_ty,
                    .data = .{ .access = .{
                        .record = record,
                        .field = access.field,
                        .field_index = field_info.index,
                    } },
                });
                if (self.types.equalIds(field_ty, result_ty)) {
                    break :blk .{ .ty = field_ty, .data = self.output.getExpr(access_expr).data };
                }
                const bridged = try self.emitExplicitBridgeExpr(access_expr, result_ty);
                break :blk .{ .ty = result_ty, .data = self.output.getExpr(bridged).data };
            },
            .let_ => |let_expr| blk: {
                const bind_ty = try self.cloneTypeIntoInstFromStore(inst, &self.input.types, let_expr.bind.ty);
                const initial_body = try self.specializeExprAtSourceTy(inst, mono_cache, venv, let_expr.body, bind_ty);
                const refined_bind_ty = try self.refinedSourceTypeForLoweredExpr(
                    inst,
                    venv,
                    let_expr.body,
                    bind_ty,
                    initial_body,
                );
                const effective_bind_ty = if (self.sourceTypeIsAbstract(inst.types, bind_ty))
                    refined_bind_ty
                else
                    bind_ty;
                const body = try self.refineBindingBodyIfNeeded(
                    inst,
                    mono_cache,
                    venv,
                    let_expr.body,
                    effective_bind_ty,
                    initial_body,
                );
                const normalized_body = try self.normalizeBindingBodyToSourceTy(
                    inst,
                    mono_cache,
                    effective_bind_ty,
                    body,
                );
                const bind_exec_ty = self.output.getExpr(normalized_body).ty;
                const source_exact_fn_symbol = self.directCallableSymbol(self.input.store.getExpr(let_expr.body));
                const rest_env = try self.extendEnv(venv, .{
                    .symbol = let_expr.bind.symbol,
                    .ty = effective_bind_ty,
                    .exec_ty = bind_exec_ty,
                    .exact_fn_symbol = source_exact_fn_symbol,
                });
                defer self.allocator.free(rest_env);
                const rest = try self.specializeExpr(inst, mono_cache, rest_env, let_expr.rest);
                break :blk .{
                    .ty = self.output.getExpr(rest).ty,
                    .data = .{ .let_ = .{
                        .bind = .{
                            .ty = bind_exec_ty,
                            .symbol = let_expr.bind.symbol,
                        },
                        .body = normalized_body,
                        .rest = rest,
                    } },
                };
            },
            .call => |call| try self.specializeCallExpr(inst, mono_cache, venv, expr_id, call, ty, default_ty),
            .structural_eq => |eq| try self.specializeStructuralEqExpr(
                inst,
                mono_cache,
                venv,
                eq,
                default_ty,
            ),
            .method_eq => |eq| try self.specializeMethodEqExpr(
                inst,
                mono_cache,
                venv,
                eq,
                default_ty,
            ),
            .dispatch_call => |method_call| try self.specializeDispatchCallExpr(
                inst,
                mono_cache,
                venv,
                method_call,
                ty,
                default_ty,
            ),
            .type_dispatch_call => |method_call| try self.specializeTypeDispatchCallExpr(
                inst,
                mono_cache,
                venv,
                method_call,
                ty,
                default_ty,
            ),
            .inspect => unreachable,
            .low_level => |ll| blk: {
                const lowered_args = switch (ll.op) {
                    .box_box, .box_unbox => blk_args: {
                        const source_args = self.input.store.sliceExprSpan(ll.args);
                        const out = try self.allocator.alloc(ast.ExprId, source_args.len);
                        defer self.allocator.free(out);
                        for (source_args, 0..) |arg_id, i| {
                            out[i] = try self.specializeBoxBoundaryArgExpr(inst, mono_cache, venv, ll.op, arg_id);
                        }
                        break :blk_args try self.output.addExprSpan(out);
                    },
                    else => blk_args: {
                        if (try self.lowLevelOperandSourceTy(inst, mono_cache, venv, ll.op, ll.args, ty)) |operand_source_ty| {
                            const source_args = self.input.store.sliceExprSpan(ll.args);
                            const out = try self.allocator.alloc(ast.ExprId, source_args.len);
                            defer self.allocator.free(out);
                            for (source_args, 0..) |arg_id, i| {
                                out[i] = try self.specializeExprAtSourceTy(inst, mono_cache, venv, arg_id, operand_source_ty);
                            }
                            break :blk_args try self.output.addExprSpan(out);
                        }
                        break :blk_args try self.specializeExprSpan(inst, mono_cache, venv, ll.args);
                    },
                };
                const args = self.output.sliceExprSpan(lowered_args);
                const result_ty = switch (ll.op) {
                    .box_box => blk_ty: {
                        if (args.len != 1) {
                            debugPanic("lambdamono.lower.specializeExpr box_box expected one arg");
                        }
                        break :blk_ty try self.boxBoundaryResultTypeFromArg(.box_box, self.output.getExpr(args[0]).ty);
                    },
                    .box_unbox => blk_ty: {
                        if (args.len != 1) {
                            debugPanic("lambdamono.lower.specializeExpr box_unbox expected one arg");
                        }
                        break :blk_ty try self.boxBoundaryResultTypeFromArg(.box_unbox, self.output.getExpr(args[0]).ty);
                    },
                    else => self.lowLevelResultTypeFromArgs(ll.op, args, default_ty),
                };
                switch (ll.op) {
                    .box_box, .box_unbox => {
                        if (args.len != 1) debugPanic("lambdamono.lower.specializeExpr box op expected one arg");
                        if (ll.op == .box_unbox) {
                            const arg_ty = self.output.getExpr(args[0]).ty;
                            if (self.types.getType(arg_ty) != .box) {
                                debugPanic("lambdamono.lower.specializeExpr box_unbox expected boxed executable arg");
                            }
                        }
                    },
                    else => {},
                }
                break :blk .{
                    .ty = result_ty,
                    .data = .{ .low_level = .{
                        .op = ll.op,
                        .args = lowered_args,
                    } },
                };
            },
            .when => |when_expr| blk: {
                const cond_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, when_expr.cond);
                const input_branch_ids = self.input.store.sliceBranchSpan(when_expr.branches);
                const cond = try self.specializeExprAtSourceTy(
                    inst,
                    mono_cache,
                    venv,
                    when_expr.cond,
                    cond_source_ty,
                );
                const lowered_branches = try self.allocator.alloc(ast.Branch, input_branch_ids.len);
                defer self.allocator.free(lowered_branches);

                for (input_branch_ids, 0..) |branch_id, i| {
                    const branch = self.input.store.getBranch(branch_id);
                    const branch_pat = self.input.store.getPat(branch.pat);
                    const branch_pat_source_ty = try self.cloneTypeIntoInstFromStore(
                        inst,
                        inst.types,
                        cond_source_ty,
                    );
                    const pat_result = try self.specializePatAtSourceAndExecutableTy(
                        inst,
                        mono_cache,
                        branch_pat,
                        branch_pat_source_ty,
                        self.output.getExpr(cond).ty,
                    );
                    defer self.allocator.free(pat_result.additions);
                    const branch_env = try self.concatEnv(venv, pat_result.additions);
                    defer self.allocator.free(branch_env);
                    const branch_body_expr = self.input.store.getExpr(branch.body);
                    const branch_body_source_ty = try self.cloneTypeIntoInstFromStore(
                        inst,
                        &self.input.types,
                        branch_body_expr.ty,
                    );
                    const body = if (self.executableTypeIsAbstract(default_ty))
                        try self.specializeExpr(inst, mono_cache, branch_env, branch.body)
                    else
                        try self.specializeExprWithDefaultTy(
                            inst,
                            mono_cache,
                            branch_env,
                            branch.body,
                            branch_body_expr,
                            branch_body_source_ty,
                            default_ty,
                        );
                    lowered_branches[i] = .{
                        .pat = pat_result.pat,
                        .body = body,
                    };
                }

                const result_ty = if (self.executableTypeIsAbstract(default_ty)) blk_result: {
                    var merged = self.output.getExpr(lowered_branches[0].body).ty;
                    for (lowered_branches[1..]) |branch| {
                        const branch_body_ty = self.output.getExpr(branch.body).ty;
                        merged = try self.mergeExecutableSignatureType(merged, branch_body_ty);
                    }
                    break :blk_result merged;
                } else default_ty;
                for (lowered_branches) |*branch| {
                    const branch_body_ty = self.output.getExpr(branch.body).ty;
                    if (!self.types.equalIds(branch_body_ty, result_ty)) {
                        if (!try self.retargetExprToExpectedExecutableType(branch.body, result_ty)) {
                            branch.body = try self.emitExplicitBridgeExpr(branch.body, result_ty);
                        }
                    }
                }
                const branches = try self.output.addBranchSpan(lowered_branches);
                break :blk .{ .ty = result_ty, .data = .{ .when = .{
                    .cond = cond,
                    .branches = branches,
                } } };
            },
            .if_ => |if_expr| blk: {
                const cond = try self.specializeExpr(inst, mono_cache, venv, if_expr.cond);
                const then_expr = self.input.store.getExpr(if_expr.then_body);
                const else_expr = self.input.store.getExpr(if_expr.else_body);
                const then_source_ty = try self.cloneTypeIntoInstFromStore(
                    inst,
                    &self.input.types,
                    then_expr.ty,
                );
                const else_source_ty = try self.cloneTypeIntoInstFromStore(
                    inst,
                    &self.input.types,
                    else_expr.ty,
                );
                var then_body = if (self.executableTypeIsAbstract(default_ty))
                    try self.specializeExpr(inst, mono_cache, venv, if_expr.then_body)
                else
                    try self.specializeExprWithDefaultTy(
                        inst,
                        mono_cache,
                        venv,
                        if_expr.then_body,
                        then_expr,
                        then_source_ty,
                        default_ty,
                    );
                var else_body = if (self.executableTypeIsAbstract(default_ty))
                    try self.specializeExpr(inst, mono_cache, venv, if_expr.else_body)
                else
                    try self.specializeExprWithDefaultTy(
                        inst,
                        mono_cache,
                        venv,
                        if_expr.else_body,
                        else_expr,
                        else_source_ty,
                        default_ty,
                    );
                const then_ty = self.output.getExpr(then_body).ty;
                const else_ty = self.output.getExpr(else_body).ty;
                const result_ty = if (self.executableTypeIsAbstract(default_ty))
                    try self.mergeExecutableSignatureType(then_ty, else_ty)
                else
                    default_ty;
                if (!self.types.equalIds(then_ty, result_ty)) {
                    if (!try self.retargetExprToExpectedExecutableType(then_body, result_ty)) {
                        then_body = try self.emitExplicitBridgeExpr(then_body, result_ty);
                    }
                }
                if (!self.types.equalIds(else_ty, result_ty)) {
                    if (!try self.retargetExprToExpectedExecutableType(else_body, result_ty)) {
                        else_body = try self.emitExplicitBridgeExpr(else_body, result_ty);
                    }
                }
                break :blk .{ .ty = result_ty, .data = .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                } } };
            },
            .block => |block| blk: {
                var lowered = try self.specializeBlockExpr(inst, mono_cache, venv, block);
                const initial_result_ty = self.output.getExpr(lowered.final_expr).ty;
                const result_ty = if (self.executableTypeIsAbstract(default_ty))
                    initial_result_ty
                else
                    default_ty;
                if (!self.types.equalIds(initial_result_ty, result_ty)) {
                    if (!try self.retargetExprToExpectedExecutableType(lowered.final_expr, result_ty)) {
                        lowered.final_expr = try self.emitExplicitBridgeExpr(lowered.final_expr, result_ty);
                    }
                }
                break :blk .{ .ty = result_ty, .data = .{ .block = lowered } };
            },
            .tuple => |elems| blk: {
                const source_elems = self.input.store.sliceExprSpan(elems);
                const lowered = try self.allocator.alloc(ast.ExprId, source_elems.len);
                defer self.allocator.free(lowered);

                const source_elem_tys = self.solvedTupleElemTypes(inst.types, ty);
                const expected_elem_tys = self.tupleElemTypes(default_ty);
                if (source_elem_tys) |elem_tys| {
                    if (elem_tys.len != source_elems.len) {
                        debugPanic("lambdamono.lower.tuple expected solved elem count mismatch");
                    }
                }
                if (expected_elem_tys) |elem_tys| {
                    if (elem_tys.len != source_elems.len) {
                        debugPanic("lambdamono.lower.tuple expected elem count mismatch");
                    }
                }

                for (source_elems, 0..) |elem_expr_id, i| {
                    const lowered_elem = if (source_elem_tys) |elem_tys|
                        try self.specializeExprAtSourceTy(inst, mono_cache, venv, elem_expr_id, elem_tys[i])
                    else
                        try self.specializeExpr(inst, mono_cache, venv, elem_expr_id);
                    lowered[i] = lowered_elem;
                }

                const tuple_ty = if (!self.executableTypeIsAbstract(default_ty))
                    default_ty
                else if (expected_elem_tys != null)
                    try self.strengthenTupleExecutableType(default_ty, lowered)
                else blk2: {
                    const lowered_elems = try self.output.addExprSpan(lowered);
                    break :blk2 try self.tupleTypeFromElems(lowered_elems);
                };
                const strengthened_elem_tys = self.tupleElemTypes(tuple_ty) orelse
                    debugPanic("lambdamono.lower.tuple missing strengthened elem types");
                for (lowered, strengthened_elem_tys, 0..) |lowered_elem, expected_elem_ty, i| {
                    if (!self.types.equalIds(self.output.getExpr(lowered_elem).ty, expected_elem_ty)) {
                        lowered[i] = try self.retargetOrEmitExplicitBridgeExpr(lowered_elem, expected_elem_ty);
                    }
                }
                const lowered_elems = try self.output.addExprSpan(lowered);
                break :blk .{ .ty = tuple_ty, .data = .{ .tuple = lowered_elems } };
            },
            .tag_payload => |tag_payload| blk: {
                const tag_union = try self.specializeExpr(inst, mono_cache, venv, tag_payload.tag_union);
                const tag_union_ty = self.output.getExpr(tag_union).ty;
                const payload_tys = self.tagArgTypes(tag_union_ty, tag_payload.tag_discriminant) orelse
                    debugPanic("lambdamono.lower.tag_payload missing executable tag payload types");
                if (tag_payload.payload_index >= payload_tys.len) {
                    debugPanic("lambdamono.lower.tag_payload missing payload");
                }
                const payload_ty = payload_tys[tag_payload.payload_index];
                const result_ty = if (self.types.containsAbstractLeaf(default_ty))
                    payload_ty
                else
                    try self.mergeExecutableSignatureType(default_ty, payload_ty);
                const payload_expr = try self.output.addExpr(.{
                    .ty = payload_ty,
                    .data = .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .tag_discriminant = tag_payload.tag_discriminant,
                        .payload_index = tag_payload.payload_index,
                    } },
                });
                if (self.types.equalIds(payload_ty, result_ty)) {
                    break :blk .{ .ty = payload_ty, .data = self.output.getExpr(payload_expr).data };
                }
                const bridged = try self.emitExplicitBridgeExpr(payload_expr, result_ty);
                break :blk .{ .ty = result_ty, .data = self.output.getExpr(bridged).data };
            },
            .tuple_access => |access| blk: {
                const tuple = try self.specializeExpr(inst, mono_cache, venv, access.tuple);
                const tuple_ty = self.output.getExpr(tuple).ty;
                const tuple_elems = self.tupleElemTypes(tuple_ty) orelse
                    debugPanic("lambdamono.lower.tuple_access expected tuple type");
                if (access.elem_index >= tuple_elems.len) {
                    debugPanic("lambdamono.lower.tuple_access missing tuple element");
                }
                const field_ty = tuple_elems[access.elem_index];
                const result_ty = if (self.types.containsAbstractLeaf(default_ty))
                    field_ty
                else
                    try self.mergeExecutableSignatureType(default_ty, field_ty);
                if (self.types.equalIds(field_ty, result_ty)) {
                    break :blk .{ .ty = field_ty, .data = .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                    } } };
                }
                const access_expr = try self.output.addExpr(.{
                    .ty = field_ty,
                    .data = .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                    } },
                });
                const bridged = try self.emitExplicitBridgeExpr(access_expr, result_ty);
                break :blk .{ .ty = result_ty, .data = self.output.getExpr(bridged).data };
            },
            .list => |items| blk: {
                const source_items = self.input.store.sliceExprSpan(items);
                const lowered = try self.allocator.alloc(ast.ExprId, source_items.len);
                defer self.allocator.free(lowered);

                const source_elem_ty = self.solvedListElemType(inst.types, ty);

                if (source_items.len == 0) {
                    if (!self.executableTypeIsAbstract(default_ty)) {
                        break :blk .{ .ty = default_ty, .data = .{ .list = try self.output.addExprSpan(lowered) } };
                    }
                    const lowered_source_elem_ty = if (source_elem_ty) |elem_ty|
                        try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, elem_ty)
                    else
                        null;
                    const list_ty = try self.strengthenListExecutableType(default_ty, lowered_source_elem_ty);
                    break :blk .{ .ty = list_ty, .data = .{ .list = try self.output.addExprSpan(lowered) } };
                }

                for (source_items, 0..) |item_expr_id, i| {
                    const lowered_item = if (source_elem_ty) |elem_ty|
                        try self.specializeExprAtSourceTy(inst, mono_cache, venv, item_expr_id, elem_ty)
                    else
                        try self.specializeExpr(inst, mono_cache, venv, item_expr_id);
                    lowered[i] = lowered_item;
                }
                const list_ty = if (!self.executableTypeIsAbstract(default_ty)) blk_ty: {
                    break :blk_ty default_ty;
                } else blk_ty: {
                    var actual_elem_ty = self.output.getExpr(lowered[0]).ty;
                    for (lowered[1..]) |lowered_item| {
                        actual_elem_ty = try self.mergeExecutableSignatureType(actual_elem_ty, self.output.getExpr(lowered_item).ty);
                    }
                    break :blk_ty try self.strengthenListExecutableType(default_ty, actual_elem_ty);
                };
                const expected_elem_ty = self.listElemType(list_ty) orelse
                    debugPanic("lambdamono.lower.list missing strengthened elem type");
                for (lowered, 0..) |lowered_item, i| {
                    if (!self.types.equalIds(self.output.getExpr(lowered_item).ty, expected_elem_ty)) {
                        lowered[i] = try self.retargetOrEmitExplicitBridgeExpr(lowered_item, expected_elem_ty);
                    }
                }
                break :blk .{ .ty = list_ty, .data = .{ .list = try self.output.addExprSpan(lowered) } };
            },
            .return_ => |ret_expr| .{ .ty = default_ty, .data = .{ .return_ = try self.specializeExpr(inst, mono_cache, venv, ret_expr) } },
            .runtime_error => |msg| .{ .ty = default_ty, .data = .{ .runtime_error = msg } },
            .for_ => |for_expr| .{ .ty = default_ty, .data = .{ .for_ = try self.specializeForExpr(inst, mono_cache, venv, for_expr) } },
        };
        return try self.output.addExpr(.{ .ty = specialized.ty, .data = specialized.data });
    }

    const dec_scale_i128: i128 = 1_000_000_000_000_000_000;

    fn specializeIntLiteral(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        value: i128,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.types.getType(target_ty)) {
            .primitive => |prim| switch (prim) {
                .dec => .{ .dec_lit = try self.decFromWholeInt(value) },
                .f32 => .{ .frac_f32_lit = @as(f32, @floatFromInt(value)) },
                .f64 => .{ .frac_f64_lit = @as(f64, @floatFromInt(value)) },
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => .{ .int_lit = value },
                .bool, .str, .erased => debugPanicFmt(
                    "lambdamono numeric literal invariant violated: int literal lowered to non-numeric primitive {s}",
                    .{@tagName(prim)},
                ),
            },
            else => debugPanicFmt(
                "lambdamono numeric literal invariant violated: int literal lowered to unsupported target type {d} ({s})",
                .{ @intFromEnum(target_ty), @tagName(self.types.getTypePreservingNominal(target_ty)) },
            ),
        };
    }

    fn specializeDecLiteral(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        scaled_value: i128,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.types.getType(target_ty)) {
            .primitive => |prim| switch (prim) {
                .dec => .{ .dec_lit = scaled_value },
                .f32 => .{ .frac_f32_lit = @floatCast(self.decToF64(scaled_value)) },
                .f64 => .{ .frac_f64_lit = self.decToF64(scaled_value) },
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => blk: {
                    if (@rem(scaled_value, dec_scale_i128) != 0) {
                        debugPanic(
                            "lambdamono numeric literal invariant violated: decimal literal lowered to integer target with fractional component",
                        );
                    }
                    break :blk .{ .int_lit = @divExact(scaled_value, dec_scale_i128) };
                },
                .bool, .str, .erased => debugPanicFmt(
                    "lambdamono numeric literal invariant violated: decimal literal lowered to non-numeric primitive {s}",
                    .{@tagName(prim)},
                ),
            },
            else => debugPanicFmt(
                "lambdamono numeric literal invariant violated: decimal literal lowered to unsupported target type {d} ({s})",
                .{ @intFromEnum(target_ty), @tagName(self.types.getTypePreservingNominal(target_ty)) },
            ),
        };
    }

    fn specializeF32Literal(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        value: f32,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.types.getType(target_ty)) {
            .primitive => |prim| switch (prim) {
                .f32 => .{ .frac_f32_lit = value },
                else => debugPanicFmt(
                    "lambdamono numeric literal invariant violated: f32 literal lowered to non-f32 target {s}",
                    .{@tagName(prim)},
                ),
            },
            else => debugPanicFmt(
                "lambdamono numeric literal invariant violated: f32 literal lowered to unsupported target type {d} ({s})",
                .{ @intFromEnum(target_ty), @tagName(self.types.getTypePreservingNominal(target_ty)) },
            ),
        };
    }

    fn specializeF64Literal(
        self: *Lowerer,
        target_ty: type_mod.TypeId,
        value: f64,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        return switch (self.types.getType(target_ty)) {
            .primitive => |prim| switch (prim) {
                .f64 => .{ .frac_f64_lit = value },
                else => debugPanicFmt(
                    "lambdamono numeric literal invariant violated: f64 literal lowered to non-f64 target {s}",
                    .{@tagName(prim)},
                ),
            },
            else => debugPanicFmt(
                "lambdamono numeric literal invariant violated: f64 literal lowered to unsupported target type {d} ({s})",
                .{ @intFromEnum(target_ty), @tagName(self.types.getTypePreservingNominal(target_ty)) },
            ),
        };
    }

    fn decFromWholeInt(_: *Lowerer, value: i128) std.mem.Allocator.Error!i128 {
        const result = @mulWithOverflow(value, dec_scale_i128);
        if (result[1] != 0) {
            debugPanic("lambdamono numeric literal invariant violated: Dec whole-int literal overflowed i128 representation");
        }
        return result[0];
    }

    fn decToF64(_: *Lowerer, value: i128) f64 {
        return @as(f64, @floatFromInt(value)) / @as(f64, @floatFromInt(dec_scale_i128));
    }

    fn specializeInspectExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        value_expr_id: solved.Ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const value_expr = try self.specializeExpr(inst, mono_cache, venv, value_expr_id);
        return try self.specializeInspectValueExpr(mono_cache, value_expr, result_ty);
    }

    fn ensureInspectHelper(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        value_ty: type_mod.TypeId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!Symbol {
        if (self.types.containsAbstractLeaf(value_ty)) {
            const value_summary = self.debugExecutableTypeSummary(value_ty);
            defer value_summary.deinit(self.allocator);
            const result_summary = self.debugExecutableTypeSummary(result_ty);
            defer result_summary.deinit(self.allocator);
            debugPanicFmt(
                "lambdamono.inspect helper received abstract executable value type value={s} result={s}",
                .{ value_summary.text, result_summary.text },
            );
        }
        const key: InspectKey = .{
            .value_ty = value_ty,
            .result_ty = result_ty,
        };
        if (self.inspect_helpers.get(key)) |symbol| return symbol;

        const symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
        try self.inspect_helpers.put(key, symbol);

        const arg_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
        const arg_expr = try self.makeVarExpr(value_ty, arg_symbol);
        const body = try self.buildInlineInspectValueExpr(mono_cache, arg_expr, result_ty);

        try self.inspect_defs.append(self.allocator, .{
            .bind = symbol,
            .result_ty = null,
            .value = .{ .fn_ = .{
                .args = try self.output.addTypedSymbolSpan(&.{.{
                    .ty = value_ty,
                    .symbol = arg_symbol,
                }}),
                .body = body,
            } },
        });

        return symbol;
    }

    fn lookupTopLevelBindType(self: *const Lowerer, symbol: Symbol) ?TypeVarId {
        for (self.input.store.defsSlice()) |def| {
            if (def.bind.symbol == symbol) return def.bind.ty;
        }

        const target_origin = self.input.symbols.get(symbol).origin;
        for (self.input.store.defsSlice()) |def| {
            const def_origin = self.input.symbols.get(def.bind.symbol).origin;
            const matches_origin = switch (target_origin) {
                .top_level_def => |target| switch (def_origin) {
                    .top_level_def => |candidate| candidate.module_idx == target.module_idx and candidate.def_idx == target.def_idx,
                    else => false,
                },
                else => false,
            };
            if (matches_origin) return def.bind.ty;
        }

        return switch (target_origin) {
            .specialized_top_level_def => |data| self.lookupTopLevelBindType(@enumFromInt(data.source_symbol)),
            .specialized_local_fn => |data| self.lookupTopLevelBindType(@enumFromInt(data.source_symbol)),
            .lifted_local_fn => |data| self.lookupTopLevelBindType(@enumFromInt(data.source_symbol)),
            .lifted_local_fn_alias => |data| self.lookupTopLevelBindType(@enumFromInt(data.source_symbol)),
            .top_level_def => {
                for (self.input.symbols.entries.items, 0..) |entry, i| {
                    switch (entry.origin) {
                        .specialized_top_level_def => |data| {
                            if (data.source_symbol == symbol.raw()) {
                                if (self.lookupTopLevelBindType(@enumFromInt(@as(u32, @intCast(i))))) |ty| {
                                    return ty;
                                }
                            }
                        },
                        else => {},
                    }
                }
                return null;
            },
            else => null,
        };
    }

    fn makeInspectHelperCall(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        value_expr: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const value_ty = self.output.getExpr(value_expr).ty;
        const helper = try self.ensureInspectHelper(mono_cache, value_ty, result_ty);
        return try self.output.addExpr(.{
            .ty = result_ty,
            .data = .{ .call = .{
                .proc = helper,
                .args = try self.output.addExprSpan(&.{value_expr}),
            } },
        });
    }

    fn buildInlineInspectValueExpr(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        value_expr: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const value_ty = self.output.getExpr(value_expr).ty;
        return switch (self.types.getTypePreservingNominal(value_ty)) {
            .placeholder => debugPanic("lambdamono.lower.buildInlineInspectValueExpr unresolved executable type"),
            .unbd => debugPanic("lambdamono.lower.buildInlineInspectValueExpr abstract executable type leaked to inspect"),
            .link => unreachable,
            .primitive => |prim| switch (prim) {
                .str => self.makeLowLevelExpr(result_ty, .str_inspect, &.{value_expr}),
                .bool => self.makeBoolInspectExpr(value_expr, result_ty),
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                .f32,
                .f64,
                .dec,
                => self.makeLowLevelExpr(result_ty, .num_to_str, &.{value_expr}),
                .erased => self.makeStringLiteralExpr(result_ty, "<opaque>"),
            },
            .erased_fn => self.makeStringLiteralExpr(result_ty, "<fn>"),
            .nominal => |nominal| blk: {
                if (nominal.is_opaque) {
                    break :blk self.makeStringLiteralExpr(result_ty, "<opaque>");
                }
                const backing_expr = try self.emitExplicitBridgeExpr(value_expr, nominal.backing);
                break :blk try self.buildInlineInspectValueExpr(mono_cache, backing_expr, result_ty);
            },
            .list => |elem_ty| self.makeListInspectExpr(mono_cache, elem_ty, value_expr, result_ty),
            .box => |elem_ty| blk: {
                switch (self.types.getTypePreservingNominal(elem_ty)) {
                    .erased_fn => break :blk self.makeStringLiteralExpr(result_ty, "<fn>"),
                    .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                        break :blk self.makeStringLiteralExpr(result_ty, "<fn>");
                    },
                    else => {},
                }
                const unboxed_expr = try self.makeLowLevelExpr(elem_ty, .box_unbox, &.{value_expr});
                break :blk try self.specializeInspectValueExpr(mono_cache, unboxed_expr, result_ty);
            },
            .tuple => |elems| self.makeTupleInspectExpr(mono_cache, value_expr, elems, result_ty),
            .record => |record| self.makeRecordInspectExpr(mono_cache, value_expr, record.fields, result_ty),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                self.makeStringLiteralExpr(result_ty, "<fn>")
            else
                self.makeTagUnionInspectExpr(mono_cache, value_expr, tag_union.tags, result_ty),
        };
    }

    fn retypeDivergingExpr(
        self: *Lowerer,
        expr_id: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!?ast.ExprId {
        return switch (self.output.getExpr(expr_id).data) {
            .runtime_error => |msg| try self.output.addExpr(.{
                .ty = result_ty,
                .data = .{ .runtime_error = msg },
            }),
            .block => |block| if (try self.retypeDivergingExpr(block.final_expr, result_ty)) |final_expr| try self.output.addExpr(.{
                .ty = result_ty,
                .data = .{ .block = .{
                    .stmts = block.stmts,
                    .final_expr = final_expr,
                } },
            }) else null,
            else => null,
        };
    }

    fn makeBoolInspectExpr(
        self: *Lowerer,
        value_expr: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.output.addExpr(.{
            .ty = result_ty,
            .data = .{ .if_ = .{
                .cond = value_expr,
                .then_body = try self.makeStringLiteralExpr(result_ty, "True"),
                .else_body = try self.makeStringLiteralExpr(result_ty, "False"),
            } },
        });
    }

    fn makeListInspectExpr(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        elem_ty: type_mod.TypeId,
        list_expr: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const prefix_expr = try self.makeStringLiteralExpr(result_ty, "[");
        const suffix_expr = try self.makeStringLiteralExpr(result_ty, "]");
        const separator_expr = try self.makeStringLiteralExpr(result_ty, ", ");
        const unit_ty = try self.makeUnitType();
        const unit_expr = try self.output.addExpr(.{
            .ty = unit_ty,
            .data = .unit,
        });
        const bool_ty = try self.makePrimitiveType(.bool);

        const out_bind: ast.TypedSymbol = .{
            .ty = result_ty,
            .symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic),
        };
        const out_decl = try self.output.addStmt(.{ .var_decl = .{
            .bind = out_bind,
            .body = prefix_expr,
        } });

        const item_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
        const item_pat = try self.output.addPat(.{
            .ty = elem_ty,
            .data = .{ .var_ = item_symbol },
        });
        const item_expr = try self.makeVarExpr(elem_ty, item_symbol);

        const out_before_cmp = try self.makeVarExpr(result_ty, out_bind.symbol);
        const is_first_expr = try self.makeLowLevelExpr(bool_ty, .str_is_eq, &.{ out_before_cmp, prefix_expr });
        const inspected_item = try self.specializeInspectValueExpr(mono_cache, item_expr, result_ty);

        const out_then_expr = try self.makeVarExpr(result_ty, out_bind.symbol);
        const appended_first = try self.makeStringConcatExpr(result_ty, out_then_expr, inspected_item);
        const then_stmt = try self.output.addStmt(.{ .reassign = .{
            .target = out_bind.symbol,
            .body = appended_first,
        } });
        const then_body = try self.output.addExpr(.{
            .ty = unit_ty,
            .data = .{ .block = .{
                .stmts = try self.output.addStmtSpan(&.{then_stmt}),
                .final_expr = unit_expr,
            } },
        });

        const out_else_prefix = try self.makeVarExpr(result_ty, out_bind.symbol);
        const with_separator = try self.makeStringConcatExpr(result_ty, out_else_prefix, separator_expr);
        const else_sep_stmt = try self.output.addStmt(.{ .reassign = .{
            .target = out_bind.symbol,
            .body = with_separator,
        } });
        const out_else_value = try self.makeVarExpr(result_ty, out_bind.symbol);
        const appended_else = try self.makeStringConcatExpr(result_ty, out_else_value, inspected_item);
        const else_item_stmt = try self.output.addStmt(.{ .reassign = .{
            .target = out_bind.symbol,
            .body = appended_else,
        } });
        const else_body = try self.output.addExpr(.{
            .ty = unit_ty,
            .data = .{ .block = .{
                .stmts = try self.output.addStmtSpan(&.{ else_sep_stmt, else_item_stmt }),
                .final_expr = unit_expr,
            } },
        });

        const loop_body = try self.output.addExpr(.{
            .ty = unit_ty,
            .data = .{ .if_ = .{
                .cond = is_first_expr,
                .then_body = then_body,
                .else_body = else_body,
            } },
        });

        const loop_expr = try self.output.addExpr(.{
            .ty = unit_ty,
            .data = .{ .for_ = .{
                .patt = item_pat,
                .iterable = list_expr,
                .body = loop_body,
            } },
        });
        const loop_stmt = try self.output.addStmt(.{ .expr = loop_expr });

        const out_before_close = try self.makeVarExpr(result_ty, out_bind.symbol);
        const closed_out = try self.makeStringConcatExpr(result_ty, out_before_close, suffix_expr);
        const close_stmt = try self.output.addStmt(.{ .reassign = .{
            .target = out_bind.symbol,
            .body = closed_out,
        } });

        const final_out = try self.makeVarExpr(result_ty, out_bind.symbol);
        return try self.output.addExpr(.{
            .ty = result_ty,
            .data = .{ .block = .{
                .stmts = try self.output.addStmtSpan(&.{ out_decl, loop_stmt, close_stmt }),
                .final_expr = final_out,
            } },
        });
    }

    fn makeRecordInspectExpr(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        record_expr: ast.ExprId,
        fields_span: []const type_mod.Field,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const fields = fields_span;
        if (fields.len == 0) {
            return self.makeStringLiteralExpr(result_ty, "{}");
        }

        const record_ty = self.output.getExpr(record_expr).ty;
        const subject_bind: ast.TypedSymbol = .{
            .ty = record_ty,
            .symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic),
        };
        const subject_decl = try self.output.addStmt(.{ .decl = .{
            .bind = subject_bind,
            .body = record_expr,
        } });
        const subject_expr = try self.makeVarExpr(record_ty, subject_bind.symbol);

        var current = try self.makeStringLiteralExpr(result_ty, "{ ");
        for (fields, 0..) |field, i| {
            if (i != 0) {
                current = try self.makeStringConcatExpr(
                    result_ty,
                    current,
                    try self.makeStringLiteralExpr(result_ty, ", "),
                );
            }

            current = try self.makeStringConcatExpr(
                result_ty,
                current,
                try self.makeStringLiteralExpr(result_ty, self.lookupIdentNameBytes(field.name)),
            );
            current = try self.makeStringConcatExpr(
                result_ty,
                current,
                try self.makeStringLiteralExpr(result_ty, ": "),
            );

            const field_expr = try self.output.addExpr(.{
                .ty = field.ty,
                .data = .{ .access = .{
                    .record = subject_expr,
                    .field = field.name,
                    .field_index = @intCast(i),
                } },
            });
            current = try self.makeStringConcatExpr(
                result_ty,
                current,
                try self.specializeInspectValueExpr(mono_cache, field_expr, result_ty),
            );
        }

        const final_expr = try self.makeStringConcatExpr(
            result_ty,
            current,
            try self.makeStringLiteralExpr(result_ty, " }"),
        );
        return try self.output.addExpr(.{
            .ty = result_ty,
            .data = .{ .block = .{
                .stmts = try self.output.addStmtSpan(&.{subject_decl}),
                .final_expr = final_expr,
            } },
        });
    }

    fn makeTupleInspectExpr(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        tuple_expr: ast.ExprId,
        elems_span: []const type_mod.TypeId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const elems = elems_span;
        if (elems.len == 0) {
            return self.makeStringLiteralExpr(result_ty, "()");
        }

        const tuple_ty = self.output.getExpr(tuple_expr).ty;
        const subject_bind: ast.TypedSymbol = .{
            .ty = tuple_ty,
            .symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic),
        };
        const subject_decl = try self.output.addStmt(.{ .decl = .{
            .bind = subject_bind,
            .body = tuple_expr,
        } });
        const subject_expr = try self.makeVarExpr(tuple_ty, subject_bind.symbol);

        var current = try self.makeStringLiteralExpr(result_ty, "(");
        for (elems, 0..) |elem_ty, i| {
            if (i != 0) {
                current = try self.makeStringConcatExpr(
                    result_ty,
                    current,
                    try self.makeStringLiteralExpr(result_ty, ", "),
                );
            }

            const elem_expr = try self.output.addExpr(.{
                .ty = elem_ty,
                .data = .{ .tuple_access = .{
                    .tuple = subject_expr,
                    .elem_index = @intCast(i),
                } },
            });
            current = try self.makeStringConcatExpr(
                result_ty,
                current,
                try self.specializeInspectValueExpr(mono_cache, elem_expr, result_ty),
            );
        }

        const final_expr = try self.makeStringConcatExpr(
            result_ty,
            current,
            try self.makeStringLiteralExpr(result_ty, ")"),
        );
        return try self.output.addExpr(.{
            .ty = result_ty,
            .data = .{ .block = .{
                .stmts = try self.output.addStmtSpan(&.{subject_decl}),
                .final_expr = final_expr,
            } },
        });
    }

    fn makeTagUnionInspectExpr(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        value_expr: ast.ExprId,
        tags_span: []const type_mod.Tag,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const union_ty = self.output.getExpr(value_expr).ty;
        const tags = tags_span;
        const branches = try self.allocator.alloc(ast.Branch, tags.len);
        defer self.allocator.free(branches);

        for (tags, 0..) |tag, i| {
            const tag_args = tag.args;
            const pat_args = try self.allocator.alloc(ast.PatId, tag_args.len);
            defer self.allocator.free(pat_args);

            var current = try self.makeStringLiteralExpr(result_ty, self.lookupTagNameBytes(tag.name));

            for (tag_args, 0..) |arg_ty, arg_i| {
                const arg_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                pat_args[arg_i] = try self.output.addPat(.{
                    .ty = arg_ty,
                    .data = .{ .var_ = arg_symbol },
                });

                const arg_expr = try self.makeVarExpr(arg_ty, arg_symbol);
                if (arg_i == 0) {
                    current = try self.makeStringConcatExpr(
                        result_ty,
                        current,
                        try self.makeStringLiteralExpr(result_ty, "("),
                    );
                } else {
                    current = try self.makeStringConcatExpr(
                        result_ty,
                        current,
                        try self.makeStringLiteralExpr(result_ty, ", "),
                    );
                }
                current = try self.makeStringConcatExpr(
                    result_ty,
                    current,
                    try self.specializeInspectValueExpr(mono_cache, arg_expr, result_ty),
                );
            }

            if (tag_args.len > 0) {
                current = try self.makeStringConcatExpr(
                    result_ty,
                    current,
                    try self.makeStringLiteralExpr(result_ty, ")"),
                );
            }

            branches[i] = .{
                .pat = try self.output.addPat(.{
                    .ty = union_ty,
                    .data = .{ .tag = .{
                        .name = tag.name,
                        .discriminant = @intCast(i),
                        .args = try self.output.addPatSpan(pat_args),
                    } },
                }),
                .body = current,
            };
        }

        return try self.output.addExpr(.{
            .ty = result_ty,
            .data = .{ .when = .{
                .cond = value_expr,
                .branches = try self.output.addBranchSpan(branches),
            } },
        });
    }

    fn tagUnionIsInternalLambdaSet(
        _: *const Lowerer,
        tags_span: []const type_mod.Tag,
    ) bool {
        const tags = tags_span;
        if (tags.len == 0) return false;

        for (tags) |tag| switch (tag.name) {
            .lambda => {},
            .ctor => return false,
        };

        return true;
    }

    fn specializeInspectValueExpr(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        value_expr: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (try self.retypeDivergingExpr(value_expr, result_ty)) |diverging| {
            return diverging;
        }

        const value_ty = self.output.getExpr(value_expr).ty;
        return switch (self.types.getType(value_ty)) {
            .placeholder => debugPanic("lambdamono.lower.specializeInspectValueExpr unresolved executable type"),
            .unbd => debugPanic("lambdamono.lower.specializeInspectValueExpr abstract executable type leaked to inspect"),
            .link => unreachable,
            .primitive => |prim| switch (prim) {
                .str => self.makeLowLevelExpr(result_ty, .str_inspect, &.{value_expr}),
                .bool => self.makeBoolInspectExpr(value_expr, result_ty),
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                .f32,
                .f64,
                .dec,
                => self.makeLowLevelExpr(result_ty, .num_to_str, &.{value_expr}),
                .erased => self.makeStringLiteralExpr(result_ty, "<opaque>"),
            },
            .erased_fn => self.makeStringLiteralExpr(result_ty, "<fn>"),
            .nominal => self.makeInspectHelperCall(mono_cache, value_expr, result_ty),
            .list,
            .box,
            .tuple,
            .record,
            => self.makeInspectHelperCall(mono_cache, value_expr, result_ty),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                self.makeStringLiteralExpr(result_ty, "<fn>")
            else
                self.makeInspectHelperCall(mono_cache, value_expr, result_ty),
        };
    }

    fn makePrimitiveType(self: *Lowerer, prim: type_mod.Prim) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.internResolved(.{ .primitive = prim });
    }

    fn makeUnitType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.internResolved(.{ .record = .{ .fields = &.{} } });
    }

    fn concretizeAbstractExecutableType(
        self: *Lowerer,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (!self.types.containsAbstractLeaf(ty)) return ty;

        return switch (self.types.getTypePreservingNominal(ty)) {
            .placeholder => debugPanic("lambdamono.lower.concretizeAbstractExecutableType encountered placeholder"),
            .unbd => try self.makeUnitType(),
            .link => unreachable,
            .primitive, .erased_fn => ty,
            .nominal => |nominal| blk: {
                const out_args = try self.types.allocator.alloc(type_mod.TypeId, nominal.args.len);
                for (nominal.args, 0..) |arg, i| {
                    out_args[i] = try self.concretizeAbstractExecutableType(arg);
                }
                break :blk try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = out_args,
                    .backing = try self.concretizeAbstractExecutableType(nominal.backing),
                } });
            },
            .list => |elem| try self.types.internResolved(.{
                .list = try self.concretizeAbstractExecutableType(elem),
            }),
            .box => |elem| try self.types.internResolved(.{
                .box = try self.concretizeAbstractExecutableType(elem),
            }),
            .tuple => |elems| blk: {
                const out = try self.types.allocator.alloc(type_mod.TypeId, elems.len);
                for (elems, 0..) |elem, i| {
                    out[i] = try self.concretizeAbstractExecutableType(elem);
                }
                break :blk try self.types.internResolved(.{ .tuple = out });
            },
            .record => |record| blk: {
                const out = try self.types.allocator.alloc(type_mod.Field, record.fields.len);
                for (record.fields, 0..) |field, i| {
                    out[i] = .{
                        .name = field.name,
                        .ty = try self.concretizeAbstractExecutableType(field.ty),
                    };
                }
                break :blk try self.types.internResolved(.{ .record = .{ .fields = out } });
            },
            .tag_union => |tag_union| blk: {
                const out = try self.types.allocator.alloc(type_mod.Tag, tag_union.tags.len);
                for (tag_union.tags, 0..) |tag, i| {
                    const out_args = try self.types.allocator.alloc(type_mod.TypeId, tag.args.len);
                    for (tag.args, 0..) |arg, arg_i| {
                        out_args[arg_i] = try self.concretizeAbstractExecutableType(arg);
                    }
                    out[i] = .{
                        .name = tag.name,
                        .args = out_args,
                    };
                }
                break :blk try self.types.internResolved(.{ .tag_union = .{ .tags = out } });
            },
        };
    }

    fn concretizeNominalExecutableArgs(
        self: *Lowerer,
        args: []const type_mod.TypeId,
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        const out_args = try self.types.allocator.alloc(type_mod.TypeId, args.len);
        defer self.types.allocator.free(out_args);
        for (args, 0..) |arg, i| {
            out_args[i] = try self.concretizeAbstractExecutableType(arg);
        }
        return try self.types.dupeTypeIds(out_args);
    }

    fn materializeListLiteralExecutableType(
        self: *Lowerer,
        default_ty: type_mod.TypeId,
        elem_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (!self.types.containsAbstractLeaf(default_ty)) return default_ty;
        const concrete_elem_ty = if (elem_ty) |candidate|
            try self.concretizeAbstractExecutableType(candidate)
        else
            try self.makeUnitType();
        return try self.types.internResolved(.{ .list = concrete_elem_ty });
    }

    fn isEmptyRecordType(self: *Lowerer, ty: type_mod.TypeId) bool {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |nominal| self.isEmptyRecordType(nominal.backing),
            .record => |record| record.fields.len == 0,
            else => false,
        };
    }

    fn makeErasedFnType(
        self: *Lowerer,
        capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.internResolved(.{ .erased_fn = capture_ty });
    }

    fn specializeBoxBoundaryArgExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        op: base.LowLevel,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const lowered_expr = try self.specializeExpr(inst, mono_cache, venv, expr_id);
        const source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
        const lowered = try self.lowerBoxBoundaryExpr(inst, mono_cache, venv, source_ty, lowered_expr);
        const expected_exec_ty = switch (op) {
            .box_box => try self.lowerBoxBoundaryCallableTypeIn(inst.types, mono_cache, source_ty),
            .box_unbox => try self.lowerBoxedBoundaryCallableTypeIn(inst.types, mono_cache, source_ty),
            else => unreachable,
        };
        const strengthened_exec_ty = try self.mergeExecutableSignatureType(
            expected_exec_ty,
            self.output.getExpr(lowered).ty,
        );
        if (self.types.equalIds(self.output.getExpr(lowered).ty, strengthened_exec_ty)) {
            return lowered;
        }
        return try self.emitExplicitBridgeExpr(lowered, strengthened_exec_ty);
    }

    fn lowerBoxBoundaryExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        _: []const EnvEntry,
        source_ty: TypeVarId,
        lowered_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const lowered = self.output.getExpr(lowered_expr);
        const source_repr = inst.types.maybeLambdaRepr(source_ty) orelse return lowered_expr;
        switch (source_repr) {
            .lset => {
                switch (self.types.getType(lowered.ty)) {
                    .erased_fn => if (self.erasedFnCaptureType(lowered.ty) != null) return lowered_expr,
                    else => {},
                }
                return try self.lowerConcreteCallableAsErased(
                    inst,
                    mono_cache,
                    source_ty,
                    lowered_expr,
                    null,
                );
            },
            .erased => {
                return switch (self.types.getTypePreservingNominal(lowered.ty)) {
                    .erased_fn => lowered_expr,
                    else => debugPanic("lambdamono.lower.lowerBoxBoundaryExpr solved erased callable lowered as non-erased executable"),
                };
            },
        }
    }

    fn lowerConcreteCallableAsErased(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        callable_id: ast.ExprId,
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const callable = self.output.getExpr(callable_id);
        const lambda_members = try self.collectExecutableLambdaMembers(inst.types, source_ty, callable.ty);
        defer self.allocator.free(lambda_members);

        return switch (callable.data) {
            .tag => |tag| blk: {
                if (tag.discriminant >= lambda_members.len) {
                    debugPanic("lambdamono.lower.lowerConcreteCallableAsErased tag discriminant out of bounds");
                }
                const capture_args = self.output.sliceExprSpan(tag.args);
                const capture_expr = switch (capture_args.len) {
                    0 => null,
                    1 => capture_args[0],
                    else => debugPanic("lambdamono.lower.lowerConcreteCallableAsErased expected at most one capture payload"),
                };
                break :blk try self.makeErasedPackedFnExpr(inst, mono_cache, source_ty, lambda_members[tag.discriminant].symbol, capture_expr, forced_capture_ty);
            },
            .packed_fn => callable_id,
            else => blk: {
                const erased_capture_ty = forced_capture_ty orelse self.commonErasedCaptureTypeFromMembers(lambda_members);
                if (erased_capture_ty == null) {
                    for (lambda_members) |lambda_member| {
                        if (lambda_member.capture_ty != null) {
                            if (builtin.mode == .Debug) {
                                debugPanic("lambdamono.lower.lowerConcreteCallableAsErased missing common capture type");
                            } else {
                                unreachable;
                            }
                        }
                    }
                }
                const branches = try self.allocator.alloc(ast.Branch, lambda_members.len);
                defer self.allocator.free(branches);
                for (lambda_members, 0..) |lambda_member, i| {
                    branches[i] = try self.makeErasedCallableBranch(
                        inst,
                        mono_cache,
                        source_ty,
                        callable.ty,
                        lambda_member,
                        erased_capture_ty,
                    );
                }
                break :blk try self.output.addExpr(.{
                    .ty = try self.makeErasedFnType(erased_capture_ty),
                    .data = .{ .when = .{
                        .cond = callable_id,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                });
            },
        };
    }

    fn requireExecutableType(
        self: *Lowerer,
        actual_ty: type_mod.TypeId,
        expected_ty: type_mod.TypeId,
        comptime context: []const u8,
    ) void {
        if (self.types.equalIds(actual_ty, expected_ty)) return;
        debugPanicFmt(
            "lambdamono.lower.{s} executable type mismatch actual={s}#{d} expected={s}#{d}",
            .{
                context,
                @tagName(self.types.getTypePreservingNominal(actual_ty)),
                @intFromEnum(actual_ty),
                @tagName(self.types.getTypePreservingNominal(expected_ty)),
                @intFromEnum(expected_ty),
            },
        );
    }

    fn emitExplicitBridgeExpr(
        self: *Lowerer,
        expr_id: ast.ExprId,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (self.types.containsAbstractLeaf(expected_ty)) {
            debugPanicFmt(
                "lambdamono.lower.emitExplicitBridgeExpr abstract expected executable type expr={d} actual_ty={d} expected_ty={d} actual_tag={s} expected_tag={s}",
                .{
                    @intFromEnum(expr_id),
                    @intFromEnum(self.output.getExpr(expr_id).ty),
                    @intFromEnum(expected_ty),
                    @tagName(self.types.getTypePreservingNominal(self.output.getExpr(expr_id).ty)),
                    @tagName(self.types.getTypePreservingNominal(expected_ty)),
                },
            );
        }
        const expr = self.output.getExpr(expr_id);
        const actual_ty = expr.ty;
        const actual_tag = self.types.getTypePreservingNominal(actual_ty);
        const expected_tag = self.types.getTypePreservingNominal(expected_ty);
        if ((actual_tag == .record and expected_tag == .tag_union) or
            (actual_tag == .tag_union and expected_tag == .tag_union) or
            (actual_tag == .list and expected_tag == .list))
        {
            const actual_elem_id: u32 = switch (actual_tag) {
                .list => |elem| @intFromEnum(elem),
                else => 0,
            };
            const expected_elem_id: u32 = switch (expected_tag) {
                .list => |elem| @intFromEnum(elem),
                else => 0,
            };
            const actual_elem_tag = switch (actual_tag) {
                .list => |elem| @tagName(self.types.getTypePreservingNominal(elem)),
                else => "-",
            };
            const expected_elem_tag = switch (expected_tag) {
                .list => |elem| @tagName(self.types.getTypePreservingNominal(elem)),
                else => "-",
            };
            const actual_elem_prim = switch (actual_tag) {
                .list => |elem| switch (self.types.getTypePreservingNominal(elem)) {
                    .primitive => |prim| @tagName(prim),
                    else => "-",
                },
                else => "-",
            };
            const expected_elem_prim = switch (expected_tag) {
                .list => |elem| switch (self.types.getTypePreservingNominal(elem)) {
                    .primitive => |prim| @tagName(prim),
                    else => "-",
                },
                else => "-",
            };
            const var_symbol_name = switch (expr.data) {
                .var_ => |symbol| blk: {
                    const entry = self.input.symbols.get(symbol);
                    if (entry.name.isNone()) break :blk "<none>";
                    break :blk self.input.idents.getText(entry.name);
                },
                else => "-",
            };
            debugPanicFmt(
                "lambdamono.lower.emitExplicitBridgeExpr debug trap expr={d} expr_tag={s} var={s} actual_ty={d} actual_tag={s} expected_ty={d} expected_tag={s} actual_elem={d}:{s}:{s} expected_elem={d}:{s}:{s}",
                .{
                    @intFromEnum(expr_id),
                    @tagName(expr.data),
                    var_symbol_name,
                    @intFromEnum(actual_ty),
                    @tagName(actual_tag),
                    @intFromEnum(expected_ty),
                    @tagName(expected_tag),
                    actual_elem_id,
                    actual_elem_tag,
                    actual_elem_prim,
                    expected_elem_id,
                    expected_elem_tag,
                    expected_elem_prim,
                },
            );
        }
        if (actual_ty == expected_ty) return expr_id;
        if (self.types.getTypePreservingNominal(expected_ty) == .primitive and
            self.types.getTypePreservingNominal(expected_ty).primitive == .erased)
        {
            const boxed_expr = switch (self.types.getTypePreservingNominal(actual_ty)) {
                .box => expr_id,
                else => blk: {
                    const boxed_ty = try self.types.internResolved(.{ .box = actual_ty });
                    break :blk try self.makeLowLevelExpr(boxed_ty, .box_box, &.{expr_id});
                },
            };
            if (self.output.getExpr(boxed_expr).ty == expected_ty) return boxed_expr;
            return try self.output.addExpr(.{
                .ty = expected_ty,
                .data = .{ .bridge = boxed_expr },
            });
        }
        return try self.output.addExpr(.{
            .ty = expected_ty,
            .data = .{ .bridge = expr_id },
        });
    }

    fn retargetOrEmitExplicitBridgeExpr(
        self: *Lowerer,
        expr_id: ast.ExprId,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (self.types.equalIds(self.output.getExpr(expr_id).ty, expected_ty)) {
            return expr_id;
        }
        if (try self.retargetExprToExpectedExecutableType(expr_id, expected_ty)) {
            return expr_id;
        }
        return try self.emitExplicitBridgeExpr(expr_id, expected_ty);
    }

    fn retargetExprToExpectedExecutableType(
        self: *Lowerer,
        expr_id: ast.ExprId,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!bool {
        const expr_idx = @intFromEnum(expr_id);
        var expr = self.output.exprs.items[expr_idx];
        switch (expr.data) {
            .tag => |tag| {
                const tag_exec_info = self.executableTagInfoByTagName(expected_ty, tag.name) orelse return false;
                const tag_args = self.output.sliceExprSpan(tag.args);
                const updated_args = try self.allocator.alloc(ast.ExprId, tag_args.len);
                defer self.allocator.free(updated_args);
                for (tag_args, tag_exec_info.args, 0..) |arg_expr, expected_arg_ty, i| {
                    updated_args[i] = if (self.types.equalIds(self.output.getExpr(arg_expr).ty, expected_arg_ty))
                        arg_expr
                    else if (try self.retargetExprToExpectedExecutableType(arg_expr, expected_arg_ty))
                        arg_expr
                    else
                        try self.emitExplicitBridgeExpr(arg_expr, expected_arg_ty);
                }
                expr.ty = expected_ty;
                expr.data = .{ .tag = .{
                    .name = tag.name,
                    .discriminant = tag_exec_info.discriminant,
                    .args = try self.output.addExprSpan(updated_args),
                } };
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .record => |fields_span| {
                const fields = self.output.sliceFieldExprSpan(fields_span);
                const updated_fields = try self.allocator.alloc(ast.FieldExpr, fields.len);
                defer self.allocator.free(updated_fields);
                for (fields, 0..) |field, i| {
                    const expected_field = self.recordFieldByName(expected_ty, field.name) orelse
                        return false;
                    updated_fields[i] = .{
                        .name = field.name,
                        .value = if (self.types.equalIds(self.output.getExpr(field.value).ty, expected_field.ty))
                            field.value
                        else if (try self.retargetExprToExpectedExecutableType(field.value, expected_field.ty))
                            field.value
                        else
                            try self.emitExplicitBridgeExpr(field.value, expected_field.ty),
                    };
                }
                const updated_span = try self.output.addFieldExprSpan(updated_fields);
                expr.ty = expected_ty;
                expr.data = .{ .record = try self.orderedRecordFields(expected_ty, updated_span) };
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .int_lit => |value| {
                expr.ty = expected_ty;
                expr.data = try self.specializeIntLiteral(expected_ty, value);
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .frac_f32_lit => |value| {
                expr.ty = expected_ty;
                expr.data = try self.specializeF32Literal(expected_ty, value);
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .frac_f64_lit => |value| {
                expr.ty = expected_ty;
                expr.data = try self.specializeF64Literal(expected_ty, value);
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .dec_lit => |value| {
                expr.ty = expected_ty;
                expr.data = try self.specializeDecLiteral(expected_ty, value);
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .tuple => |elems_span| {
                const expected_elem_tys = self.tupleElemTypes(expected_ty) orelse return false;
                const elems = self.output.sliceExprSpan(elems_span);
                if (expected_elem_tys.len != elems.len) return false;
                const updated_elems = try self.allocator.alloc(ast.ExprId, elems.len);
                defer self.allocator.free(updated_elems);
                for (elems, expected_elem_tys, 0..) |elem_expr, expected_elem_ty, i| {
                    updated_elems[i] = if (self.types.equalIds(self.output.getExpr(elem_expr).ty, expected_elem_ty))
                        elem_expr
                    else if (try self.retargetExprToExpectedExecutableType(elem_expr, expected_elem_ty))
                        elem_expr
                    else
                        try self.emitExplicitBridgeExpr(elem_expr, expected_elem_ty);
                }
                expr.ty = expected_ty;
                expr.data = .{ .tuple = try self.output.addExprSpan(updated_elems) };
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .list => |items_span| {
                const expected_elem_ty = self.listElemType(expected_ty) orelse return false;
                const items = self.output.sliceExprSpan(items_span);
                const updated_items = try self.allocator.alloc(ast.ExprId, items.len);
                defer self.allocator.free(updated_items);
                for (items, 0..) |item_expr, i| {
                    updated_items[i] = if (self.types.equalIds(self.output.getExpr(item_expr).ty, expected_elem_ty))
                        item_expr
                    else if (try self.retargetExprToExpectedExecutableType(item_expr, expected_elem_ty))
                        item_expr
                    else
                        try self.emitExplicitBridgeExpr(item_expr, expected_elem_ty);
                }
                expr.ty = expected_ty;
                expr.data = .{ .list = try self.output.addExprSpan(updated_items) };
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .let_ => |let_expr| {
                if (!try self.retargetExprToExpectedExecutableType(let_expr.rest, expected_ty)) return false;
                expr.ty = expected_ty;
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .block => |block| {
                if (!try self.retargetExprToExpectedExecutableType(block.final_expr, expected_ty)) return false;
                expr.ty = expected_ty;
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            else => return false,
        }
    }

    fn makeErasedPackedFnExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
        symbol: Symbol,
        capture_expr: ?ast.ExprId,
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var capture_expr_opt = capture_expr;
        const current_member = inst.types.requireLambdaMember(requested_ty, symbol);
        if (forced_capture_ty != null and current_member.captures.len == 0) {
            debugPanic("lambdamono.lower.makeErasedPackedFnExpr forced capture type on captureless lambda");
        }
        var capture_ty: ?type_mod.TypeId = forced_capture_ty;
        if (capture_expr_opt) |capture| {
            if (capture_ty == null) {
                capture_ty = self.output.getExpr(capture).ty;
            } else if (!self.types.equalIds(self.output.getExpr(capture).ty, capture_ty.?)) {
                capture_expr_opt = try self.emitExplicitBridgeExpr(capture, capture_ty.?);
            }
        }
        if (capture_expr_opt == null) {
            if (capture_ty) |expected_ty| {
                if (self.isEmptyRecordType(expected_ty)) {
                    capture_ty = null;
                } else if (current_member.captures.len != 0) {
                    debugPanic("lambdamono.lower.makeErasedPackedFnExpr missing capture payload for non-empty capture type");
                }
            }
        } else if (capture_ty) |expected_ty| {
            if (self.isEmptyRecordType(expected_ty)) {
                capture_expr_opt = null;
                capture_ty = null;
            }
        }
        if (capture_expr_opt) |capture| {
            const capture_box_ty = try self.types.internResolved(.{ .box = capture_ty.? });
            capture_expr_opt = try self.makeLowLevelExpr(capture_box_ty, .box_box, &.{capture});
        }
        if (capture_ty == null and current_member.captures.len != 0) {
            debugPanic("lambdamono.lower.makeErasedPackedFnExpr missing capture type for captured lambda");
        }
        const specialized_symbol = try self.queueCallableSpecializationWithExplicitSignature(
            inst.types,
            mono_cache,
            symbol,
            .erased_boundary,
            requested_ty,
            capture_ty,
        );
        return try self.output.addExpr(.{
            .ty = try self.makeErasedFnType(capture_ty),
            .data = .{ .packed_fn = .{
                .lambda = specialized_symbol,
                .captures = capture_expr_opt,
                .capture_ty = capture_ty,
            } },
        });
    }

    fn makeErasedPackedFnExprForExactSymbol(
        self: *Lowerer,
        symbol: Symbol,
        exact_fn_ty: TypeVarId,
        capture_expr: ?ast.ExprId,
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();
        const current_member = self.input.types.requireLambdaMember(exact_fn_ty, symbol);
        var capture_expr_opt = capture_expr;
        var capture_ty = forced_capture_ty;
        if (capture_expr_opt) |capture| {
            if (capture_ty == null) {
                capture_ty = self.output.getExpr(capture).ty;
            } else if (self.output.getExpr(capture).ty != capture_ty.?) {
                capture_expr_opt = try self.emitExplicitBridgeExpr(capture, capture_ty.?);
            }
        }
        if (capture_ty) |expected_ty| {
            if (self.isEmptyRecordType(expected_ty)) {
                capture_expr_opt = null;
                capture_ty = null;
            }
        }
        if (capture_expr_opt) |capture| {
            const capture_box_ty = try self.types.internResolved(.{ .box = capture_ty.? });
            capture_expr_opt = try self.makeLowLevelExpr(capture_box_ty, .box_box, &.{capture});
        }
        if (capture_ty == null and current_member.captures.len != 0) {
            debugPanic("lambdamono.lower.makeErasedPackedFnExprForExactSymbol missing capture type for captured lambda");
        }
        const specialized_symbol = try self.queueCallableSpecializationWithExplicitSignature(
            &self.input.types,
            &mono_cache,
            symbol,
            .erased_boundary,
            exact_fn_ty,
            capture_ty,
        );
        return try self.output.addExpr(.{
            .ty = try self.makeErasedFnType(capture_ty),
            .data = .{ .packed_fn = .{
                .lambda = specialized_symbol,
                .captures = capture_expr_opt,
                .capture_ty = capture_ty,
            } },
        });
    }

    fn lowerExactExecutableCallableAsErased(
        self: *Lowerer,
        callable_id: ast.ExprId,
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const callable = self.output.getExpr(callable_id);
        const callable_ty = callable.ty;
        const tag_union = switch (self.types.getTypePreservingNominal(callable_ty)) {
            .tag_union => |tag_union| tag_union,
            else => debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected executable lambda-set type"),
        };
        if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
            debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected internal lambda-set tags");
        }

        return switch (callable.data) {
            .tag => |tag| blk: {
                const lambda_symbol = switch (tag.name) {
                    .lambda => |lambda_symbol| lambda_symbol,
                    .ctor => debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected lambda tag"),
                };
                const exact = specializations.lookupFnExact(self.fenv, lambda_symbol) orelse
                    debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased missing exact function");
                const capture_args = self.output.sliceExprSpan(tag.args);
                const capture_expr = switch (capture_args.len) {
                    0 => null,
                    1 => capture_args[0],
                    else => debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected at most one capture payload"),
                };
                break :blk try self.makeErasedPackedFnExprForExactSymbol(
                    lambda_symbol,
                    exact.fn_ty,
                    capture_expr,
                    forced_capture_ty,
                );
            },
            else => blk: {
                const erased_capture_ty = forced_capture_ty orelse self.commonErasedCaptureTypeFromExecutable(tag_union.tags);
                const branches = try self.allocator.alloc(ast.Branch, tag_union.tags.len);
                defer self.allocator.free(branches);
                for (tag_union.tags, 0..) |tag_info, i| {
                    const lambda_symbol = switch (tag_info.name) {
                        .lambda => |lambda_symbol| lambda_symbol,
                        .ctor => debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected lambda tag"),
                    };
                    const exact = specializations.lookupFnExact(self.fenv, lambda_symbol) orelse
                        debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased missing exact function");
                    if (tag_info.args.len == 0) {
                        branches[i] = .{
                            .pat = try self.output.addPat(.{
                                .ty = callable_ty,
                                .data = .{ .tag = .{
                                    .name = tag_info.name,
                                    .discriminant = @intCast(i),
                                    .args = ast.Span(ast.PatId).empty(),
                                } },
                            }),
                            .body = try self.makeErasedPackedFnExprForExactSymbol(
                                lambda_symbol,
                                exact.fn_ty,
                                null,
                                erased_capture_ty,
                            ),
                        };
                    } else {
                        if (tag_info.args.len != 1) {
                            debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected at most one capture payload");
                        }
                        const capture_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                        const capture_pat = try self.output.addPat(.{
                            .ty = tag_info.args[0],
                            .data = .{ .var_ = capture_symbol },
                        });
                        const capture_expr = try self.output.addExpr(.{
                            .ty = tag_info.args[0],
                            .data = .{ .var_ = capture_symbol },
                        });
                        branches[i] = .{
                            .pat = try self.output.addPat(.{
                                .ty = callable_ty,
                                .data = .{ .tag = .{
                                    .name = tag_info.name,
                                    .discriminant = @intCast(i),
                                    .args = try self.output.addPatSpan(&.{capture_pat}),
                                } },
                            }),
                            .body = try self.makeErasedPackedFnExprForExactSymbol(
                                lambda_symbol,
                                exact.fn_ty,
                                capture_expr,
                                erased_capture_ty,
                            ),
                        };
                    }
                }
                break :blk try self.output.addExpr(.{
                    .ty = try self.makeErasedFnType(erased_capture_ty),
                    .data = .{ .when = .{
                        .cond = callable_id,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                });
            },
        };
    }

    fn commonErasedCaptureType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
        lambdas: []const solved.Type.Lambda,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        var common: ?type_mod.TypeId = null;
        for (lambdas) |lambda| {
            const next = try self.lowerExecutableCaptureTypeForLambdaMember(
                solved_types,
                mono_cache,
                requested_ty,
                lambda.symbol,
            );
            if (common == null) {
                common = next;
                continue;
            }
            if (common == null and next == null) continue;
            if (common == null or next == null or common.? != next.?) {
                debugPanic("lambdamono.lower.commonErasedCaptureType boxed callable variants require a common capture type");
            }
        }
        return common;
    }

    // Executable target-shape helper used only when an explicit executable
    // boundary type must be rewritten to its erased callable form.
    fn commonErasedCaptureTypeFromExecutable(
        _: *Lowerer,
        tags_span: []const type_mod.Tag,
    ) ?type_mod.TypeId {
        const tags = tags_span;
        var common: ?type_mod.TypeId = null;
        for (tags) |tag| {
            const args = tag.args;
            const next: ?type_mod.TypeId = switch (args.len) {
                0 => null,
                1 => args[0],
                else => debugPanic("lambdamono.lower.commonErasedCaptureTypeFromExecutable expected at most one capture payload"),
            };
            if (common == null) {
                common = next;
                continue;
            }
            if (common == null and next == null) continue;
            if (common == null or next == null or common.? != next.?) {
                debugPanic("lambdamono.lower.commonErasedCaptureTypeFromExecutable boxed callable variants require a common capture type");
            }
        }
        return common;
    }

    fn makeErasedCallableBranch(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
        callable_ty: type_mod.TypeId,
        lambda_member: LambdaMemberInfo,
        erased_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.Branch {
        return if (lambda_member.capture_ty == null) .{
            .pat = try self.output.addPat(.{
                .ty = callable_ty,
                .data = .{ .tag = .{
                    .name = lower_type.lambdaTagKey(lambda_member.symbol),
                    .discriminant = lambda_member.discriminant,
                    .args = ast.Span(ast.PatId).empty(),
                } },
            }),
            .body = try self.makeErasedPackedFnExpr(inst, mono_cache, requested_ty, lambda_member.symbol, null, erased_capture_ty),
        } else blk: {
            const capture_ty = erased_capture_ty orelse lambda_member.capture_ty.?;
            const capture_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
            const capture_pat = try self.output.addPat(.{
                .ty = capture_ty,
                .data = .{ .var_ = capture_symbol },
            });
            const capture_expr = try self.output.addExpr(.{
                .ty = capture_ty,
                .data = .{ .var_ = capture_symbol },
            });
            break :blk .{
                .pat = try self.output.addPat(.{
                    .ty = callable_ty,
                    .data = .{ .tag = .{
                        .name = lower_type.lambdaTagKey(lambda_member.symbol),
                        .discriminant = lambda_member.discriminant,
                        .args = try self.output.addPatSpan(&.{capture_pat}),
                    } },
                }),
                .body = try self.makeErasedPackedFnExpr(inst, mono_cache, requested_ty, lambda_member.symbol, capture_expr, capture_ty),
            };
        };
    }

    fn makeVarExpr(
        self: *Lowerer,
        ty: type_mod.TypeId,
        symbol: Symbol,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.output.addExpr(.{
            .ty = ty,
            .data = .{ .var_ = symbol },
        });
    }

    fn internStringLiteral(
        self: *Lowerer,
        bytes: []const u8,
    ) std.mem.Allocator.Error!base.StringLiteral.Idx {
        return self.input.strings.insert(self.allocator, bytes);
    }

    fn makeStringLiteralExpr(
        self: *Lowerer,
        str_ty: type_mod.TypeId,
        bytes: []const u8,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.output.addExpr(.{
            .ty = str_ty,
            .data = .{ .str_lit = try self.internStringLiteral(bytes) },
        });
    }

    fn makeStringConcatExpr(
        self: *Lowerer,
        str_ty: type_mod.TypeId,
        left: ast.ExprId,
        right: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return self.makeLowLevelExpr(str_ty, .str_concat, &.{ left, right });
    }

    fn makeLowLevelExpr(
        self: *Lowerer,
        ret_ty: type_mod.TypeId,
        op: base.LowLevel,
        args: []const ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr_id = try self.output.addExpr(.{
            .ty = ret_ty,
            .data = .{ .low_level = .{
                .op = op,
                .args = try self.output.addExprSpan(args),
            } },
        });
        return expr_id;
    }

    fn lookupTagNameBytes(self: *Lowerer, tag_name: type_mod.TagName) []const u8 {
        return switch (tag_name) {
            .ctor => |ident| blk: {
                break :blk self.lookupIdentNameBytes(ident);
            },
            .lambda => debugPanic("lambdamono.inspect invariant violated: internal lambda-set tag reached string rendering"),
        };
    }

    fn lookupIdentNameBytes(self: *Lowerer, ident: base.Ident.Idx) []const u8 {
        return self.input.idents.getText(ident);
    }

    fn specializeVarExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        symbol: Symbol,
        instantiated_ty: TypeVarId,
        _: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const solved_types = inst.types;
        if (self.isHostedFunctionSymbol(symbol)) {
            return switch (solved_types.lambdaRepr(instantiated_ty)) {
                .lset => blk: {
                    const member = solved_types.requireLambdaMember(instantiated_ty, symbol);
                    if (member.captures.len != 0) {
                        debugPanic("lambdamono.lower.specializeVarExpr hosted function had captures");
                    }
                    const precise_ty = try self.makeSingletonExecutableLambdaType(symbol, null);
                    break :blk .{
                        .ty = precise_ty,
                        .data = .{ .tag = .{
                            .name = lower_type.lambdaTagKey(symbol),
                            .discriminant = 0,
                            .args = ast.Span(ast.ExprId).empty(),
                        } },
                    };
                },
                .erased => debugPanic("lambdamono.lower.specializeVarExpr hosted function used as erased callable"),
            };
        }

        if (specializations.lookupFnExact(self.fenv, symbol)) |entry| {
            switch (solved_types.lambdaRepr(instantiated_ty)) {
                .lset => {
                    const member = solved_types.requireLambdaMember(instantiated_ty, symbol);
                    const current_capture = try self.currentCapturePayloadFromEnv(mono_cache, member.captures, venv);
                    defer current_capture.deinit(self.allocator);
                    if (member.captures.len == 0) {
                        return .{
                            .ty = try self.makeSingletonExecutableLambdaType(symbol, null),
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(symbol),
                                .discriminant = 0,
                                .args = ast.Span(ast.ExprId).empty(),
                            } },
                        };
                    }

                    const capture_exec_ty = current_capture.ty orelse
                        debugPanic("lambdamono.lower.specializeVarExpr missing current executable capture payload");
                    const precise_ty = try self.makeSingletonExecutableLambdaType(symbol, capture_exec_ty);
                    const capture_record = try self.specializeCaptureRecord(current_capture.env, capture_exec_ty);
                    const args = try self.allocator.alloc(ast.ExprId, 1);
                    defer self.allocator.free(args);
                    args[0] = capture_record;
                    return .{
                        .ty = precise_ty,
                        .data = .{ .tag = .{
                            .name = lower_type.lambdaTagKey(symbol),
                            .discriminant = 0,
                            .args = try self.output.addExprSpan(args),
                        } },
                    };
                },
                .erased => {},
            }

            var exact_inst = InstScope.init(self.allocator);
            defer exact_inst.deinit();
            var exact_mono_cache = lower_type.MonoCache.init(self.allocator);
            defer exact_mono_cache.deinit();

            const exact_source_ty = try self.cloneInstType(&exact_inst, entry.fn_ty);
            const exact_requested_ty = try self.cloneTypeIntoInstFromStore(&exact_inst, solved_types, instantiated_ty);
            try self.unifyIn(exact_inst.types, exact_source_ty, exact_requested_ty);
            const current_member = exact_inst.types.requireLambdaMember(exact_source_ty, symbol);
            const current_captures = current_member.captures;
            return switch (exact_inst.types.lambdaRepr(exact_requested_ty)) {
                .lset => blk: {
                    const current_capture = try self.currentCapturePayloadFromEnv(&exact_mono_cache, current_captures, venv);
                    defer current_capture.deinit(self.allocator);
                    if (current_captures.len == 0) {
                        break :blk .{
                            .ty = try self.makeSingletonExecutableLambdaType(symbol, null),
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(symbol),
                                .discriminant = 0,
                                .args = ast.Span(ast.ExprId).empty(),
                            } },
                        };
                    }
                    const capture_exec_ty = current_capture.ty orelse
                        debugPanic("lambdamono.lower.specializeVarExpr missing exact current executable capture payload");
                    const precise_ty = try self.makeSingletonExecutableLambdaType(symbol, capture_exec_ty);
                    const capture_record = try self.specializeCaptureRecord(current_capture.env, capture_exec_ty);
                    const args = try self.allocator.alloc(ast.ExprId, 1);
                    defer self.allocator.free(args);
                    args[0] = capture_record;
                    break :blk .{
                        .ty = precise_ty,
                        .data = .{ .tag = .{
                            .name = lower_type.lambdaTagKey(symbol),
                            .discriminant = 0,
                            .args = try self.output.addExprSpan(args),
                        } },
                        };
                },
                .erased => blk: {
                    const current_capture = try self.currentCapturePayloadFromEnv(&exact_mono_cache, current_captures, venv);
                    defer current_capture.deinit(self.allocator);
                    const authoritative_capture_ty = current_capture.ty;
                    const specialized_symbol = try self.queueCallableSpecializationWithExplicitSignature(
                        exact_inst.types,
                        &exact_mono_cache,
                        symbol,
                        .erased_boundary,
                        exact_requested_ty,
                        authoritative_capture_ty,
                    );
                    var capture_expr: ?ast.ExprId = if (current_capture.ty) |capture_ty|
                        try self.specializeCaptureRecord(current_capture.env, capture_ty)
                    else
                        null;
                    if (capture_expr) |captures_value| {
                        const capture_box_ty = try self.types.internResolved(.{ .box = authoritative_capture_ty.? });
                        capture_expr = try self.makeLowLevelExpr(capture_box_ty, .box_box, &.{captures_value});
                    }
                    break :blk .{
                        .ty = try self.makeErasedFnType(authoritative_capture_ty),
                        .data = .{ .packed_fn = .{
                            .lambda = specialized_symbol,
                            .captures = capture_expr,
                            .capture_ty = authoritative_capture_ty,
                        } },
                    };
                },
            };
        }

        if (self.lookupEnvEntry(venv, symbol)) |entry| {
            if (!self.sourceTypeIsAbstract(solved_types, entry.ty) and
                self.executableTypeIsAbstract(entry.exec_ty))
            {
                const symbol_name = if (self.input.symbols.get(symbol).name.isNone())
                    "<none>"
                else
                    self.input.idents.getText(self.input.symbols.get(symbol).name);
                const source_key = solved_types.structuralKeyOwned(entry.ty) catch "<oom>";
                defer if (!std.mem.eql(u8, source_key, "<oom>")) solved_types.allocator.free(source_key);
                const exec_key = self.types.structuralKeyOwned(entry.exec_ty) catch "<oom>";
                defer if (!std.mem.eql(u8, exec_key, "<oom>")) self.types.allocator.free(exec_key);
                std.debug.panic(
                    "lambdamono.lower.specializeVarExpr env lookup returned abstract exec for concrete source symbol={d} name={s} source={s} exec={s}",
                    .{ symbol.raw(), symbol_name, source_key, exec_key },
                );
            }
            return .{
                .ty = entry.exec_ty,
                .data = .{ .var_ = symbol },
            };
        }
        if (self.lookupTopLevelValueType(symbol)) |top_level_ty| {
            if (!self.sourceTypeIsAbstract(solved_types, instantiated_ty) and
                self.executableTypeIsAbstract(top_level_ty))
            {
                const symbol_name = if (self.input.symbols.get(symbol).name.isNone())
                    "<none>"
                else
                    self.input.idents.getText(self.input.symbols.get(symbol).name);
                const source_key = solved_types.structuralKeyOwned(instantiated_ty) catch "<oom>";
                defer if (!std.mem.eql(u8, source_key, "<oom>")) solved_types.allocator.free(source_key);
                const exec_key = self.types.structuralKeyOwned(top_level_ty) catch "<oom>";
                defer if (!std.mem.eql(u8, exec_key, "<oom>")) self.types.allocator.free(exec_key);
                std.debug.panic(
                    "lambdamono.lower.specializeVarExpr top-level lookup returned abstract exec for concrete source symbol={d} name={s} source={s} exec={s}",
                    .{ symbol.raw(), symbol_name, source_key, exec_key },
                );
            }
            return .{
                .ty = top_level_ty,
                .data = .{ .var_ = symbol },
            };
        }
        const entry = self.input.symbols.get(symbol);
        const symbol_name = if (entry.name.isNone())
            "<none>"
        else
            self.input.idents.getText(entry.name);
        debugPanicFmt(
            "lambdamono.lower.specializeVarExpr unbound variable {d} ({s}) origin {s}",
            .{ symbol.raw(), symbol_name, @tagName(entry.origin) },
        );
    }

    fn specializeCaptureRecord(
        self: *Lowerer,
        captures: []const EnvEntry,
        capture_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const capture_fields = switch (self.types.getTypePreservingNominal(capture_ty)) {
            .nominal => |nominal| switch (self.types.getTypePreservingNominal(nominal.backing)) {
                .record => |record| record.fields,
                else => debugPanic("lambdamono.lower.specializeCaptureRecord expected capture record"),
            },
            .record => |record| record.fields,
            else => debugPanic("lambdamono.lower.specializeCaptureRecord expected capture record"),
        };
        if (builtin.mode == .Debug and capture_fields.len != captures.len) {
            debugPanic("lambdamono.lower.specializeCaptureRecord capture field count mismatch");
        }

        const fields = try self.allocator.alloc(ast.FieldExpr, capture_fields.len);
        defer self.allocator.free(fields);

        for (capture_fields, 0..) |field, i| {
            const capture = self.lookupCaptureEnvEntry(captures, field.name) orelse
                debugPanic("lambdamono.lower.specializeCaptureRecord missing capture field");
            const capture_expr = try self.output.addExpr(.{
                .ty = field.ty,
                .data = .{ .var_ = capture.symbol },
            });
            fields[i] = .{
                .name = field.name,
                .value = capture_expr,
            };
        }

        return try self.output.addExpr(.{
            .ty = capture_ty,
            .data = .{ .record = try self.output.addFieldExprSpan(fields) },
        });
    }

    const SpecializedExprLowering = struct {
        ty: type_mod.TypeId,
        data: ast.Expr.Data,
    };

    const LambdaMemberInfo = struct {
        symbol: Symbol,
        discriminant: u16,
        capture_ty: ?type_mod.TypeId,
    };

    fn executableCaptureTypeForLambdaMember(
        self: *const Lowerer,
        callable_exec_ty: type_mod.TypeId,
        symbol: Symbol,
    ) ?type_mod.TypeId {
        const tag_union = switch (self.types.getType(callable_exec_ty)) {
            .tag_union => |tag_union| tag_union,
            else => debugPanic("lambdamono.lower.executableCaptureTypeForLambdaMember expected executable lambda-set type"),
        };
        for (tag_union.tags) |tag| {
            switch (tag.name) {
                .lambda => |tag_symbol| if (tag_symbol == symbol) {
                    return switch (tag.args.len) {
                        0 => null,
                        1 => tag.args[0],
                        else => debugPanic("lambdamono.lower.executableCaptureTypeForLambdaMember expected at most one capture payload"),
                    };
                },
                .ctor => {},
            }
        }
        debugPanic("lambdamono.lower.executableCaptureTypeForLambdaMember missing lambda tag in executable callable type");
    }

    fn collectSolvedLambdaMembers(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        requested_ty: TypeVarId,
    ) std.mem.Allocator.Error![]LambdaMemberInfo {
        const lambdas = switch (solved_types.lambdaRepr(requested_ty)) {
            .erased => debugPanic("lambdamono.lower.collectLsetLambdaMembers expected concrete lambda-set"),
            .lset => |lset| lset,
        };
        const out = try self.allocator.alloc(LambdaMemberInfo, lambdas.len);
        for (lambdas, 0..) |lambda, i| {
            out[i] = .{
                .symbol = lambda.symbol,
                .discriminant = @intCast(i),
                .capture_ty = null,
            };
        }
        return out;
    }

    fn collectExecutableLambdaMembers(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        requested_ty: TypeVarId,
        callable_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error![]LambdaMemberInfo {
        const lambdas = switch (solved_types.lambdaRepr(requested_ty)) {
            .erased => debugPanic("lambdamono.lower.collectExecutableLambdaMembers expected concrete lambda-set"),
            .lset => |lset| lset,
        };
        const out = try self.allocator.alloc(LambdaMemberInfo, lambdas.len);
        for (lambdas, 0..) |lambda, i| {
            out[i] = .{
                .symbol = lambda.symbol,
                .discriminant = @intCast(i),
                .capture_ty = self.executableCaptureTypeForLambdaMember(callable_exec_ty, lambda.symbol),
            };
        }
        return out;
    }

    fn makeSingletonExecutableLambdaType(
        self: *Lowerer,
        symbol: Symbol,
        capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const args = if (capture_ty) |ty|
            try self.types.dupeTypeIds(&.{ty})
        else
            &.{};
        const tags = try self.types.dupeTags(&.{.{
            .name = lower_type.lambdaTagKey(symbol),
            .args = args,
        }});
        return try self.types.internTypeId(try self.types.internResolved(.{
            .tag_union = .{ .tags = tags },
        }));
    }

    fn commonErasedCaptureTypeFromMembers(
        _: *const Lowerer,
        members: []const LambdaMemberInfo,
    ) ?type_mod.TypeId {
        var common: ?type_mod.TypeId = null;
        for (members) |member| {
            const next = member.capture_ty;
            if (common == null) {
                common = next;
                continue;
            }
            if (common == null and next == null) continue;
            if (common == null or next == null or common.? != next.?) {
                debugPanic("lambdamono.lower.commonErasedCaptureTypeFromMembers boxed callable variants require a common capture type");
            }
        }
        return common;
    }

    fn captureBindingsFromCaptures(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        captures: []const solved.Type.Capture,
        capture_record_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, captures.len);
        for (captures, 0..) |capture, i| {
            const capture_name = self.input.symbols.get(capture.symbol).name;
            const field_info = self.recordFieldByName(capture_record_ty, capture_name) orelse
                debugPanic("lambdamono.lower.captureBindingsFromCaptures missing capture field");
            const exact_fn_symbol = self.singletonExactCallableSymbolForExecutableType(field_info.ty) orelse
                self.singletonExactCallableSymbolForSourceType(solved_types, capture.ty);
            out[i] = .{
                .symbol = capture.symbol,
                .ty = capture.ty,
                .exec_ty = field_info.ty,
                .exact_fn_symbol = exact_fn_symbol,
            };
        }
        return out;
    }

    fn singletonExactCallableSymbolForSourceType(
        _: *const Lowerer,
        solved_types: *solved.Type.Store,
        ty: TypeVarId,
    ) ?Symbol {
        const repr = solved_types.maybeLambdaRepr(ty) orelse return null;
        return switch (repr) {
            .erased => null,
            .lset => |members| switch (members.len) {
                0 => debugPanic("lambdamono.lower.singletonExactCallableSymbolForSourceType empty lambda set"),
                1 => members[0].symbol,
                else => null,
            },
        };
    }

    fn singletonExactCallableSymbolForExecutableType(
        self: *const Lowerer,
        ty: type_mod.TypeId,
    ) ?Symbol {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .tag_union => |tag_union| blk: {
                if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) break :blk null;
                break :blk switch (tag_union.tags.len) {
                    0 => debugPanic("lambdamono.lower.singletonExactCallableSymbolForExecutableType empty lambda set"),
                    1 => switch (tag_union.tags[0].name) {
                        .lambda => |symbol| symbol,
                        .ctor => debugPanic("lambdamono.lower.singletonExactCallableSymbolForExecutableType lambda-set tag had constructor name"),
                    },
                    else => null,
                };
            },
            else => null,
        };
    }

    fn exactCallableSymbolForBinding(self: *const Lowerer, symbol: Symbol) ?Symbol {
        if (self.input.exact_callable_aliases.get(symbol)) |exact| return exact;
        if (specializations.lookupFnExact(self.fenv, symbol) != null) return symbol;
        const source = self.lookupTopLevelValueSource(symbol) orelse return null;
        return switch (source) {
            .fn_, .hosted_fn => symbol,
            .val, .run => null,
        };
    }

    fn lookupCaptureEnvEntry(
        self: *Lowerer,
        captures: []const EnvEntry,
        field_name: base.Ident.Idx,
    ) ?EnvEntry {
        for (captures) |capture| {
            if (self.input.symbols.get(capture.symbol).name == field_name) {
                return capture;
            }
        }
        return null;
    }

    fn directCallableSymbol(self: *const Lowerer, expr: solved.Ast.Expr) ?Symbol {
        return switch (expr.data) {
            .var_ => |symbol| if (symbol.isNone())
                null
            else
                self.exactCallableSymbolForBinding(symbol),
            .call => |call| blk: {
                const func_expr = self.input.store.getExpr(call.func);
                const func_symbol = self.directCallableSymbol(func_expr) orelse break :blk null;
                const op = self.boxBoundaryBuiltinOp(func_symbol) orelse break :blk null;
                if (op != .box_box and op != .box_unbox) break :blk null;
                const args = self.input.store.sliceExprSpan(call.args);
                if (args.len != 1) break :blk null;
                break :blk self.directCallableSymbol(self.input.store.getExpr(args[0]));
            },
            .low_level => |ll| blk: {
                if (ll.op != .box_box and ll.op != .box_unbox) break :blk null;
                const args = self.input.store.sliceExprSpan(ll.args);
                if (args.len != 1) break :blk null;
                break :blk self.directCallableSymbol(self.input.store.getExpr(args[0]));
            },
            else => null,
        };
    }

    fn directCallableSymbolFromExpr(
        self: *const Lowerer,
        venv: []const EnvEntry,
        expr: solved.Ast.Expr,
    ) ?Symbol {
        if (self.directCallableSymbol(expr)) |symbol| return symbol;
        if (expr.data != .var_) return null;
        return if (self.lookupEnvEntry(venv, expr.data.var_)) |entry|
            entry.exact_fn_symbol
        else
            null;
    }

    fn boxBoundaryBuiltinOpForExpr(
        self: *const Lowerer,
        venv: []const EnvEntry,
        expr: solved.Ast.Expr,
    ) ?base.LowLevel {
        if (self.directCallableSymbolFromExpr(venv, expr)) |symbol| {
            if (self.boxBoundaryBuiltinOp(symbol)) |op| return op;
        }
        return if (expr.data == .var_) self.boxBoundaryBuiltinOp(expr.data.var_) else null;
    }

    fn instantiatedSourceTypeForExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!TypeVarId {
        const expr = self.input.store.getExpr(expr_id);
        return switch (expr.data) {
            .var_ => |symbol| if (self.lookupEnvEntry(venv, symbol)) |entry| blk: {
                if (entry.exact_fn_symbol) |exact_symbol| {
                    const entry_id = inst.types.unlinkPreservingNominalConst(entry.ty);
                    switch (inst.types.getNode(entry_id)) {
                        .content => |content| switch (content) {
                            .func => {},
                            else => break :blk entry.ty,
                        },
                        else => break :blk entry.ty,
                    }
                    if (self.isHostedFunctionSymbol(exact_symbol)) {
                        const source_fn_ty = self.lookupTopLevelBindType(exact_symbol) orelse
                            debugPanic("lambdamono.lower.instantiatedSourceTypeForExpr missing hosted exact source type");
                        break :blk try self.cloneTypeIntoInstFromStore(inst, &self.input.types, source_fn_ty);
                    }
                    const exact = specializations.lookupFnExact(self.fenv, exact_symbol) orelse
                        debugPanic("lambdamono.lower.instantiatedSourceTypeForExpr missing exact function source type");
                    break :blk try self.cloneTypeIntoInstFromStore(inst, &self.input.types, exact.fn_ty);
                }
                break :blk entry.ty;
            } else
                try self.cloneInstType(inst, expr.ty),
            .access => |access| {
                const record_ty = try self.instantiatedSourceTypeForExpr(inst, venv, access.record);
                return self.solvedRecordFieldByName(inst.types, record_ty, access.field) orelse
                    debugPanic("lambdamono.lower.instantiatedSourceTypeForExpr missing solved record field");
            },
            .tuple_access => |access| {
                const tuple_ty = try self.instantiatedSourceTypeForExpr(inst, venv, access.tuple);
                return self.solvedTupleElemType(inst.types, tuple_ty, access.elem_index) orelse
                    debugPanic("lambdamono.lower.instantiatedSourceTypeForExpr missing solved tuple element");
            },
            .tag_payload => |tag_payload| {
                const union_ty = try self.instantiatedSourceTypeForExpr(inst, venv, tag_payload.tag_union);
                return self.solvedTagPayloadType(
                    inst.types,
                    union_ty,
                    tag_payload.tag_discriminant,
                    tag_payload.payload_index,
                ) orelse debugPanic("lambdamono.lower.instantiatedSourceTypeForExpr missing solved tag payload");
            },
            .call, .dispatch_call, .type_dispatch_call, .low_level => try self.refinedSourceTypeForExpr(
                inst,
                venv,
                expr_id,
                try self.cloneInstType(inst, expr.ty),
            ),
            .record => try self.cloneInstType(inst, expr.ty),
            else => try self.cloneInstType(inst, expr.ty),
        };
    }

    fn refinedSourceTypeForExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        current_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        const expr = self.input.store.getExpr(expr_id);
        return switch (expr.data) {
            .var_ => |symbol| blk: {
                const binding_has_authoritative_source_fn =
                    specializations.lookupFnExact(self.fenv, symbol) != null or
                    switch (self.lookupTopLevelValueSource(symbol) orelse break :blk current_source_ty) {
                        .fn_, .hosted_fn => true,
                        .val, .run => false,
                    };
                if (!binding_has_authoritative_source_fn) break :blk current_source_ty;
                const exact_symbol = self.exactCallableSymbolForBinding(symbol) orelse break :blk current_source_ty;
                const current_source_id = inst.types.unlinkPreservingNominalConst(current_source_ty);
                switch (inst.types.getNode(current_source_id)) {
                    .content => |content| switch (content) {
                        .func => {},
                        else => break :blk current_source_ty,
                    },
                    else => break :blk current_source_ty,
                }

                const source_fn_ty = self.lookupTopLevelBindType(exact_symbol) orelse
                    debugPanic("lambdamono.lower.refinedSourceTypeForExpr missing exact source type");
                var fresh_mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
                defer fresh_mapping.deinit();
                const cloned_source_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
                    inst,
                    &self.input.types,
                    &fresh_mapping,
                    source_fn_ty,
                );
                try self.unifyIn(inst.types, cloned_source_fn_ty, current_source_ty);
                break :blk cloned_source_fn_ty;
            },
            .call => |call| blk: {
                const func_ty = try self.instantiatedSourceTypeForExpr(inst, venv, call.func);
                const arg_expr_ids = self.input.store.sliceExprSpan(call.args);
                const arg_source_tys = try self.allocator.alloc(TypeVarId, arg_expr_ids.len);
                defer self.allocator.free(arg_source_tys);
                for (arg_expr_ids, 0..) |arg_expr_id, i| {
                    arg_source_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
                }

                const base_func_source = self.input.store.getExpr(call.func);
                const direct_func_symbol = self.directCallableSymbolFromExpr(venv, base_func_source);
                const box_boundary = self.boxBoundaryBuiltinOpForExpr(venv, base_func_source);
                if (box_boundary) |op| {
                    if (arg_source_tys.len != 1) {
                        debugPanic("lambdamono.lower.refinedSourceTypeForExpr box boundary call must have arity 1");
                    }
                    switch (op) {
                        .box_unbox => break :blk self.solvedBoxElemType(inst.types, arg_source_tys[0]) orelse
                            debugPanic("lambdamono.lower.refinedSourceTypeForExpr box_unbox missing solved payload type"),
                        .box_box => {
                            const refined_arg_ty = try self.refinedSourceTypeForExpr(inst, venv, arg_expr_ids[0], arg_source_tys[0]);
                            const boxed_payload_ty = self.solvedBoxElemType(inst.types, current_source_ty) orelse
                                debugPanic("lambdamono.lower.refinedSourceTypeForExpr box_box missing solved payload type");
                            try self.unifyIn(inst.types, boxed_payload_ty, refined_arg_ty);
                            break :blk current_source_ty;
                        },
                        else => unreachable,
                    }
                }
                if (direct_func_symbol) |symbol| {
                    if (inst.types.hasCapturelessLambda(func_ty, symbol)) {
                        if (try self.exactConcreteResultSourceTy(inst, symbol)) |authoritative_ret_ty| {
                            break :blk authoritative_ret_ty;
                        }
                        const exact_requested_ty = try self.buildExactRequestedFnType(
                            inst.types,
                            symbol,
                            arg_source_tys,
                            current_source_ty,
                        );
                        var frozen = if (self.isHostedFunctionSymbol(symbol))
                            try self.freezeExactCallWorld(
                                inst.types,
                                venv,
                                symbol,
                                arg_source_tys,
                                current_source_ty,
                            )
                        else
                            try self.freezeExactRequestedCallWorld(
                                inst.types,
                                venv,
                                symbol,
                                exact_requested_ty,
                            );
                        defer frozen.deinit(self.allocator);
                        const frozen_shape = frozen.inst.types.fnShape(frozen.fn_ty);
                        const frozen_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen_shape.args);
                        try self.refineCallArgSourceTypesIn(
                            &frozen.inst,
                            frozen.env,
                            arg_expr_ids,
                            frozen_arg_tys,
                        );
                        break :blk try self.cloneTypeIntoInstFromStore(
                            inst,
                            frozen.inst.types,
                            frozen_shape.ret,
                        );
                    }
                }

                var frozen = try self.freezeCallWorld(inst.types, venv, func_ty, arg_source_tys, current_source_ty);
                defer frozen.deinit(self.allocator);
                const frozen_shape = frozen.inst.types.fnShape(frozen.fn_ty);
                const frozen_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen_shape.args);
                try self.refineCallArgSourceTypesIn(
                    &frozen.inst,
                    frozen.env,
                    arg_expr_ids,
                    frozen_arg_tys,
                );
                break :blk try self.cloneTypeIntoInstFromStore(
                    inst,
                    frozen.inst.types,
                    frozen_shape.ret,
                );
            },
            .low_level => |ll| blk: {
                break :blk if (try self.lowLevelResultSourceTy(
                    inst,
                    venv,
                    ll.op,
                    ll.args,
                    current_source_ty,
                )) |refined|
                    refined
                else
                    current_source_ty;
            },
            .dispatch_call => |method_call| blk: {
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const receiver_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, method_call.receiver);
                const target_symbol = try self.resolveAttachedMethodTargetFromExpr(
                    inst,
                    venv,
                    method_call.receiver,
                    method_call.method_name,
                );
                const requested_arg_tys = try self.allocator.alloc(TypeVarId, method_args.len + 1);
                defer self.allocator.free(requested_arg_tys);
                requested_arg_tys[0] = receiver_source_ty;
                for (method_args, 0..) |arg_expr_id, i| {
                    requested_arg_tys[i + 1] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
                }
                const exact_requested_ty = try self.buildExactRequestedFnType(
                    inst.types,
                    target_symbol,
                    requested_arg_tys,
                    current_source_ty,
                );
                var frozen = if (self.isHostedFunctionSymbol(target_symbol))
                    try self.freezeExactCallWorld(
                        inst.types,
                        venv,
                        target_symbol,
                        requested_arg_tys,
                        current_source_ty,
                    )
                else
                    try self.freezeExactRequestedCallWorld(
                        inst.types,
                        venv,
                        target_symbol,
                        exact_requested_ty,
                    );
                defer frozen.deinit(self.allocator);
                const frozen_shape = frozen.inst.types.fnShape(frozen.fn_ty);
                const frozen_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen_shape.args);
                const refined_receiver_ty = try self.refinedSourceTypeForExpr(
                    &frozen.inst,
                    frozen.env,
                    method_call.receiver,
                    frozen_arg_tys[0],
                );
                try self.unifyIn(frozen.inst.types, frozen_arg_tys[0], refined_receiver_ty);
                try self.refineCallArgSourceTypesIn(
                    &frozen.inst,
                    frozen.env,
                    method_args,
                    frozen_arg_tys[1..],
                );
                break :blk try self.cloneTypeIntoInstFromStore(
                    inst,
                    frozen.inst.types,
                    frozen_shape.ret,
                );
            },
            .type_dispatch_call => |method_call| blk: {
                var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
                defer mapping.deinit();
                const dispatcher_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
                    inst,
                    &self.input.types,
                    &mapping,
                    method_call.dispatcher_ty,
                );
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const target_symbol = self.resolveAttachedMethodTarget(
                    inst.types,
                    dispatcher_ty,
                    method_call.method_name,
                );
                const requested_arg_tys = try self.allocator.alloc(TypeVarId, method_args.len);
                defer self.allocator.free(requested_arg_tys);
                for (method_args, 0..) |arg_expr_id, i| {
                    requested_arg_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
                }
                const exact_requested_ty = try self.buildExactRequestedFnType(
                    inst.types,
                    target_symbol,
                    requested_arg_tys,
                    current_source_ty,
                );
                var frozen = if (self.isHostedFunctionSymbol(target_symbol))
                    try self.freezeExactCallWorld(
                        inst.types,
                        venv,
                        target_symbol,
                        requested_arg_tys,
                        current_source_ty,
                    )
                else
                    try self.freezeExactRequestedCallWorld(
                        inst.types,
                        venv,
                        target_symbol,
                        exact_requested_ty,
                    );
                defer frozen.deinit(self.allocator);
                const frozen_shape = frozen.inst.types.fnShape(frozen.fn_ty);
                const frozen_arg_tys = frozen.inst.types.sliceTypeVarSpan(frozen_shape.args);
                try self.refineCallArgSourceTypesIn(
                    &frozen.inst,
                    frozen.env,
                    method_args,
                    frozen_arg_tys,
                );
                break :blk try self.cloneTypeIntoInstFromStore(
                    inst,
                    frozen.inst.types,
                    frozen_shape.ret,
                );
            },
            else => current_source_ty,
        };
    }

    const CallTarget = union(enum) {
        expr: ast.ExprId,
        exact_top_level: Symbol,
    };

    fn lowerAppliedResultExecutableType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        call_target: CallTarget,
        _: TypeVarId,
        result_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (call_target) {
            .exact_top_level => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, result_source_ty),
            .expr => |func_expr| switch (self.types.getTypePreservingNominal(self.output.getExpr(func_expr).ty)) {
                .erased_fn => if (solved_types.maybeLambdaRepr(result_source_ty) != null)
                    try self.lowerBoxBoundaryCallableTypeIn(solved_types, mono_cache, result_source_ty)
                else
                    try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, result_source_ty),
                else => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, result_source_ty),
            },
        };
    }

    fn bridgeExprAtSolvedTypeToExpectedExecutableType(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        expr_id: ast.ExprId,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const actual_ty = self.output.getExpr(expr_id).ty;
        if (self.types.equalIds(actual_ty, expected_ty)) {
            return expr_id;
        }

        if (self.types.getTypePreservingNominal(expected_ty) == .primitive and
            self.types.getTypePreservingNominal(expected_ty).primitive == .erased)
        {
            const boxed_ty = try self.types.internResolved(.{ .box = actual_ty });
            const boxed_expr = try self.makeLowLevelExpr(boxed_ty, .box_box, &.{expr_id});
            if (self.types.equalIds(self.output.getExpr(boxed_expr).ty, expected_ty)) {
                return boxed_expr;
            }
            return try self.emitExplicitBridgeExpr(boxed_expr, expected_ty);
        }

        if (self.types.getTypePreservingNominal(expected_ty) == .erased_fn) {
            switch (inst.types.lambdaRepr(source_ty)) {
                .lset => {
                    const lowered = try self.lowerConcreteCallableAsErased(
                        inst,
                        mono_cache,
                        source_ty,
                        expr_id,
                        self.erasedFnCaptureType(expected_ty),
                    );
                    if (self.types.equalIds(self.output.getExpr(lowered).ty, expected_ty)) {
                        return lowered;
                    }
                    return try self.emitExplicitBridgeExpr(lowered, expected_ty);
                },
                .erased => {},
            }
            switch (self.types.getTypePreservingNominal(actual_ty)) {
                .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    const lowered = try self.lowerExactExecutableCallableAsErased(
                        expr_id,
                        self.erasedFnCaptureType(expected_ty),
                    );
                    if (self.types.equalIds(self.output.getExpr(lowered).ty, expected_ty)) {
                        return lowered;
                    }
                    return try self.emitExplicitBridgeExpr(lowered, expected_ty);
                },
                else => {},
            }
        }

        return try self.retargetOrEmitExplicitBridgeExpr(expr_id, expected_ty);
    }

    fn bridgeCallArgsToExpectedExecutableTypes(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_arg_tys: []const TypeVarId,
        arg_exprs: []const ast.ExprId,
        expected_arg_tys: []const type_mod.TypeId,
    ) std.mem.Allocator.Error![]ast.ExprId {
        if (arg_exprs.len != source_arg_tys.len or arg_exprs.len != expected_arg_tys.len) {
            debugPanic("lambdamono.lower.bridgeCallArgsToExpectedExecutableTypes arg arity mismatch");
        }
        const out = try self.allocator.alloc(ast.ExprId, arg_exprs.len);
        for (arg_exprs, source_arg_tys, expected_arg_tys, 0..) |arg_expr, source_arg_ty, expected_arg_ty, i| {
            out[i] = try self.bridgeExprAtSolvedTypeToExpectedExecutableType(
                inst,
                mono_cache,
                source_arg_ty,
                arg_expr,
                expected_arg_ty,
            );
        }
        return out;
    }

    fn applyExactTopLevelFunctionCall(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        proc_symbol: Symbol,
        arg_source_tys: []const TypeVarId,
        arg_exprs: []const ast.ExprId,
        proc_arg_exec_tys: []const type_mod.TypeId,
        proc_ret_exec_ty: type_mod.TypeId,
        expected_result_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (proc_arg_exec_tys.len != arg_exprs.len or arg_source_tys.len != arg_exprs.len) {
            debugPanic("lambdamono.lower.applyExactTopLevelFunctionCall call arg arity mismatch");
        }
        const call_args = try self.bridgeCallArgsToExpectedExecutableTypes(
            inst,
            mono_cache,
            arg_source_tys,
            arg_exprs,
            proc_arg_exec_tys,
        );
        defer if (call_args.len != 0) self.allocator.free(call_args);

        const final_result_exec_ty = if (self.executableTypeIsAbstract(expected_result_exec_ty))
            proc_ret_exec_ty
        else
            try self.mergeExecutableSignatureType(proc_ret_exec_ty, expected_result_exec_ty);
        var call_expr = try self.output.addExpr(.{
            .ty = proc_ret_exec_ty,
            .data = .{ .call = .{
                .proc = proc_symbol,
                .args = try self.output.addExprSpan(call_args),
            } },
        });
        if (!self.types.equalIds(proc_ret_exec_ty, final_result_exec_ty)) {
            call_expr = try self.emitExplicitBridgeExpr(call_expr, final_result_exec_ty);
        }
        return call_expr;
    }

    fn assertCallableExprMatchesSolvedType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        func_expr: ast.ExprId,
        current_fn_ty: TypeVarId,
    ) void {
        if (builtin.mode != .Debug) return;

        const expr_ty = self.output.getExpr(func_expr).ty;
        if (self.types.getTypePreservingNominal(expr_ty) == .erased_fn) {
            return;
        }
        switch (solved_types.lambdaRepr(current_fn_ty)) {
            .erased => switch (self.types.getTypePreservingNominal(expr_ty)) {
                .erased_fn => {},
                else => {
                    const expr = self.output.getExpr(func_expr);
                    switch (expr.data) {
                        .var_ => |symbol| debugPanicFmt(
                            "lambdamono.lower.assertCallableExprMatchesSolvedType expected erased callable expr, found {s} for expr {d} data {s} symbol {d}",
                            .{
                                @tagName(self.types.getTypePreservingNominal(expr_ty)),
                                @intFromEnum(func_expr),
                                @tagName(expr.data),
                                symbol.raw(),
                            },
                        ),
                        else => debugPanicFmt(
                            "lambdamono.lower.assertCallableExprMatchesSolvedType expected erased callable expr, found {s} for expr {d} data {s}",
                            .{
                                @tagName(self.types.getTypePreservingNominal(expr_ty)),
                                @intFromEnum(func_expr),
                                @tagName(expr.data),
                            },
                        ),
                    }
                },
            },
            .lset => |lambdas| switch (self.types.getType(expr_ty)) {
                .tag_union => |tag_union| {
                    if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                        debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType expected internal lambda-set expr");
                    }
                    if (tag_union.tags.len != lambdas.len) {
                        debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType lambda count mismatch");
                    }
                    for (tag_union.tags, lambdas) |tag, lambda| {
                        const expected_capture_count = solved_types.sliceCaptures(lambda.captures).len;
                        switch (tag.name) {
                            .lambda => |lambda_symbol| if (lambda_symbol != lambda.symbol) {
                                debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType lambda symbol mismatch");
                            },
                            .ctor => debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType expected lambda tag"),
                        }
                        if (expected_capture_count == 0) {
                            if (tag.args.len != 0) {
                                debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType unexpected capture payload");
                            }
                        } else if (tag.args.len != 1) {
                            debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType missing capture payload");
                        }
                    }
                },
                else => debugPanic("lambdamono.lower.assertCallableExprMatchesSolvedType expected lambda-set expr"),
            },
        }
    }

    fn specializeStructuralEqExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        eq: @FieldType(solved.Ast.Expr.Data, "structural_eq"),
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const receiver_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, eq.lhs);
        const arg_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, eq.rhs);

        var lowered_receiver = try self.specializeExprAtSourceTy(
            inst,
            mono_cache,
            venv,
            eq.lhs,
            receiver_source_ty,
        );
        if (try self.retypeDivergingExpr(lowered_receiver, expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }

        var lowered_arg = try self.specializeExprAtSourceTy(
            inst,
            mono_cache,
            venv,
            eq.rhs,
            arg_source_ty,
        );
        if (try self.retypeDivergingExpr(lowered_arg, expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }

        const operand_exec_ty = try self.mergeExecutableSignatureType(
            self.output.getExpr(lowered_receiver).ty,
            self.output.getExpr(lowered_arg).ty,
        );
        if (self.executableTypeIsAbstract(operand_exec_ty)) {
            debugPanic("lambdamono.lower.specializeStructuralEqExpr abstract operand executable type");
        }
        if (!self.types.equalIds(self.output.getExpr(lowered_receiver).ty, operand_exec_ty)) {
            lowered_receiver = try self.emitExplicitBridgeExpr(lowered_receiver, operand_exec_ty);
        }
        if (!self.types.equalIds(self.output.getExpr(lowered_arg).ty, operand_exec_ty)) {
            lowered_arg = try self.emitExplicitBridgeExpr(lowered_arg, operand_exec_ty);
        }

        const bool_ty = try self.makePrimitiveType(.bool);
        var eq_expr = try self.makeLowLevelExpr(bool_ty, .num_is_eq, &.{ lowered_receiver, lowered_arg });
        if (!self.types.equalIds(self.output.getExpr(eq_expr).ty, expected_exec_ty)) {
            eq_expr = try self.emitExplicitBridgeExpr(eq_expr, expected_exec_ty);
        }
        const final_expr = self.output.getExpr(eq_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
        };
    }

    fn methodEqIdent(self: *const Lowerer) base.Ident.Idx {
        return self.input.idents.lookup(base.Ident.for_text("is_eq")) orelse
            debugPanic("lambdamono.lower.methodEqIdent missing is_eq ident");
    }

    fn specializeMethodEqExpr(
        self: *Lowerer,
        inst: *InstScope,
        _: *lower_type.MonoCache,
        venv: []const EnvEntry,
        eq: @FieldType(solved.Ast.Expr.Data, "method_eq"),
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const receiver_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, eq.lhs);
        const arg_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, eq.rhs);
        const target_symbol = try self.resolveAttachedMethodTargetFromExpr(
            inst,
            venv,
            eq.lhs,
            self.methodEqIdent(),
        );
        const bool_source_ty = try inst.types.freshContent(.{ .primitive = .bool });
        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        const dispatch_constraint_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            inst,
            &self.input.types,
            &mapping,
            eq.dispatch_constraint_ty,
        );
        const dispatch_shape = inst.types.fnShape(dispatch_constraint_ty);
        const dispatch_arg_tys = inst.types.sliceTypeVarSpan(dispatch_shape.args);
        if (dispatch_arg_tys.len != 2) {
            debugPanic("lambdamono.lower.specializeMethodEqExpr method_eq arg arity mismatch");
        }
        try self.unifyIn(inst.types, dispatch_arg_tys[0], receiver_source_ty);
        try self.unifyIn(inst.types, dispatch_arg_tys[1], arg_source_ty);
        try self.unifyIn(inst.types, dispatch_shape.ret, bool_source_ty);
        const requested_call_ty = try self.buildExactRequestedFnTypeFromConstraint(
            inst.types,
            target_symbol,
            dispatch_constraint_ty,
        );
        var frozen = if (self.isHostedFunctionSymbol(target_symbol))
            try self.freezeExactRequestedCallWorld(
                inst.types,
                venv,
                target_symbol,
                requested_call_ty,
            )
        else
            try self.freezeExactRequestedCallWorld(
                inst.types,
                venv,
                target_symbol,
                requested_call_ty,
            );
        defer frozen.deinit(self.allocator);
        const frozen_call_ty = frozen.fn_ty;
        const fn_shape = frozen.inst.types.fnShape(frozen_call_ty);
        const fn_arg_tys = frozen.inst.types.sliceTypeVarSpan(fn_shape.args);
        if (fn_arg_tys.len != 2) {
            debugPanic("lambdamono.lower.specializeMethodEqExpr method_eq arg arity mismatch");
        }

        const lowered_args = try self.allocator.alloc(ast.ExprId, fn_arg_tys.len);
        defer self.allocator.free(lowered_args);
        lowered_args[0] = try self.specializeExprAtSourceTy(
            &frozen.inst,
            &frozen.mono_cache,
            frozen.env,
            eq.lhs,
            fn_arg_tys[0],
        );
        if (try self.retypeDivergingExpr(lowered_args[0], expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }
        lowered_args[1] = try self.specializeExprAtSourceTy(
            &frozen.inst,
            &frozen.mono_cache,
            frozen.env,
            eq.rhs,
            fn_arg_tys[1],
        );

        const hosted_sig = if (self.isHostedFunctionSymbol(target_symbol))
            self.lookupHostedCallSig(target_symbol)
        else
            null;
        const specialized = if (!self.isHostedFunctionSymbol(target_symbol))
            try self.ensureQueuedCallableSpecializedForCall(
                frozen.inst.types,
                target_symbol,
                .natural,
                frozen_call_ty,
                null,
                lowered_args,
                expected_exec_ty,
            )
        else
            null;
        var result_expr = try self.applyExactTopLevelFunctionCall(
            &frozen.inst,
            &frozen.mono_cache,
            if (specialized) |resolved| resolved.symbol else target_symbol,
            fn_arg_tys,
            lowered_args,
            if (hosted_sig) |sig| sig.args_tys else specialized.?.exec_args_tys,
            if (hosted_sig) |sig| sig.ret_ty else specialized.?.exec_ret_ty,
            expected_exec_ty,
        );
        if (eq.negated) {
            result_expr = try self.makeLowLevelExpr(self.output.getExpr(result_expr).ty, .bool_not, &.{result_expr});
        }
        const final_result_exec_ty = if (self.executableTypeIsAbstract(expected_exec_ty))
            self.output.getExpr(result_expr).ty
        else
            try self.mergeExecutableSignatureType(self.output.getExpr(result_expr).ty, expected_exec_ty);
        if (!self.types.equalIds(self.output.getExpr(result_expr).ty, final_result_exec_ty)) {
            result_expr = try self.emitExplicitBridgeExpr(result_expr, final_result_exec_ty);
        }
        const final_expr = self.output.getExpr(result_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
        };
    }

    fn specializeDispatchCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        _: *lower_type.MonoCache,
        venv: []const EnvEntry,
        method_call: @FieldType(solved.Ast.Expr.Data, "dispatch_call"),
        expected_result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const method_args = self.input.store.sliceExprSpan(method_call.args);
        const receiver_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, method_call.receiver);
        const target_symbol = try self.resolveAttachedMethodTargetFromExpr(
            inst,
            venv,
            method_call.receiver,
            method_call.method_name,
        );
        const requested_arg_tys = try self.allocator.alloc(TypeVarId, method_args.len + 1);
        defer self.allocator.free(requested_arg_tys);
        requested_arg_tys[0] = receiver_source_ty;
        for (method_args, 0..) |arg_expr_id, i| {
            requested_arg_tys[i + 1] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
        }
        const exact_requested_ty = try self.buildExactRequestedFnType(
            inst.types,
            target_symbol,
            requested_arg_tys,
            expected_result_source_ty,
        );
        var frozen = if (self.isHostedFunctionSymbol(target_symbol))
            try self.freezeExactCallWorld(
                inst.types,
                venv,
                target_symbol,
                requested_arg_tys,
                expected_result_source_ty,
            )
        else
            try self.freezeExactRequestedCallWorld(
                inst.types,
                venv,
                target_symbol,
                exact_requested_ty,
            );
        defer frozen.deinit(self.allocator);
        const requested_call_ty = frozen.fn_ty;
        const fn_shape = frozen.inst.types.fnShape(requested_call_ty);
        const fn_arg_tys = frozen.inst.types.sliceTypeVarSpan(fn_shape.args);
        if (fn_arg_tys.len != method_args.len + 1) {
            debugPanic("lambdamono.lower.specializeDispatchCallExpr method arg arity mismatch");
        }
        const refined_receiver_ty = try self.refinedSourceTypeForExpr(
            &frozen.inst,
            frozen.env,
            method_call.receiver,
            fn_arg_tys[0],
        );
        try self.unifyIn(frozen.inst.types, fn_arg_tys[0], refined_receiver_ty);
        try self.refineCallArgSourceTypesIn(
            &frozen.inst,
            frozen.env,
            method_args,
            fn_arg_tys[1..],
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, fn_arg_tys.len);
        defer self.allocator.free(lowered_args);
        lowered_args[0] = try self.specializeExprAtSourceTy(
            &frozen.inst,
            &frozen.mono_cache,
            frozen.env,
            method_call.receiver,
            fn_arg_tys[0],
        );
        if (try self.retypeDivergingExpr(lowered_args[0], expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }

        for (method_args, 0..) |arg_expr_id, i| {
            lowered_args[i + 1] = try self.specializeExprAtSourceTy(
                &frozen.inst,
                &frozen.mono_cache,
                frozen.env,
                arg_expr_id,
                fn_arg_tys[i + 1],
            );
        }

        const hosted_sig = if (self.isHostedFunctionSymbol(target_symbol))
            self.lookupHostedCallSig(target_symbol)
        else
            null;
        const specialized = if (!self.isHostedFunctionSymbol(target_symbol))
            try self.ensureQueuedCallableSpecializedForCall(
                frozen.inst.types,
                target_symbol,
                .natural,
                requested_call_ty,
                null,
                lowered_args,
                expected_exec_ty,
            )
        else
            null;
        var result_expr = try self.applyExactTopLevelFunctionCall(
            &frozen.inst,
            &frozen.mono_cache,
            if (specialized) |resolved| resolved.symbol else target_symbol,
            fn_arg_tys,
            lowered_args,
            if (hosted_sig) |sig| sig.args_tys else specialized.?.exec_args_tys,
            if (hosted_sig) |sig| sig.ret_ty else specialized.?.exec_ret_ty,
            expected_exec_ty,
        );
        const final_result_exec_ty = if (self.executableTypeIsAbstract(expected_exec_ty))
            self.output.getExpr(result_expr).ty
        else
            try self.mergeExecutableSignatureType(self.output.getExpr(result_expr).ty, expected_exec_ty);
        if (!self.types.equalIds(self.output.getExpr(result_expr).ty, final_result_exec_ty)) {
            result_expr = try self.emitExplicitBridgeExpr(result_expr, final_result_exec_ty);
        }
        const final_expr = self.output.getExpr(result_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
        };
    }

    fn specializeTypeDispatchCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        _: *lower_type.MonoCache,
        venv: []const EnvEntry,
        method_call: @FieldType(solved.Ast.Expr.Data, "type_dispatch_call"),
        expected_result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const method_args = self.input.store.sliceExprSpan(method_call.args);
        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        const dispatcher_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            inst,
            &self.input.types,
            &mapping,
            method_call.dispatcher_ty,
        );
        const target_symbol = self.resolveAttachedMethodTarget(inst.types, dispatcher_ty, method_call.method_name);
        const requested_arg_tys = try self.allocator.alloc(TypeVarId, method_args.len);
        defer self.allocator.free(requested_arg_tys);
        for (method_args, 0..) |arg_expr_id, i| {
            requested_arg_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
        }
        const exact_requested_ty = try self.buildExactRequestedFnType(
            inst.types,
            target_symbol,
            requested_arg_tys,
            expected_result_source_ty,
        );
        var frozen = if (self.isHostedFunctionSymbol(target_symbol))
            try self.freezeExactCallWorld(
                inst.types,
                venv,
                target_symbol,
                requested_arg_tys,
                expected_result_source_ty,
            )
        else
            try self.freezeExactRequestedCallWorld(
                inst.types,
                venv,
                target_symbol,
                exact_requested_ty,
            );
        defer frozen.deinit(self.allocator);
        const requested_call_ty = frozen.fn_ty;
        const fn_shape = frozen.inst.types.fnShape(requested_call_ty);
        const fn_arg_tys = frozen.inst.types.sliceTypeVarSpan(fn_shape.args);
        if (fn_arg_tys.len != method_args.len) {
            debugPanic("lambdamono.lower.specializeTypeDispatchCallExpr method arg arity mismatch");
        }
        try self.refineCallArgSourceTypesIn(
            &frozen.inst,
            frozen.env,
            method_args,
            fn_arg_tys,
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, fn_arg_tys.len);
        defer self.allocator.free(lowered_args);
        for (method_args, 0..) |arg_expr_id, i| {
            lowered_args[i] = try self.specializeExprAtSourceTy(
                &frozen.inst,
                &frozen.mono_cache,
                frozen.env,
                arg_expr_id,
                fn_arg_tys[i],
            );
        }

        const hosted_sig = if (self.isHostedFunctionSymbol(target_symbol))
            self.lookupHostedCallSig(target_symbol)
        else
            null;
        const specialized = if (!self.isHostedFunctionSymbol(target_symbol))
            try self.ensureQueuedCallableSpecializedForCall(
                frozen.inst.types,
                target_symbol,
                .natural,
                requested_call_ty,
                null,
                lowered_args,
                expected_exec_ty,
            )
        else
            null;
        var result_expr = try self.applyExactTopLevelFunctionCall(
            &frozen.inst,
            &frozen.mono_cache,
            if (specialized) |resolved| resolved.symbol else target_symbol,
            fn_arg_tys,
            lowered_args,
            if (hosted_sig) |sig| sig.args_tys else specialized.?.exec_args_tys,
            if (hosted_sig) |sig| sig.ret_ty else specialized.?.exec_ret_ty,
            expected_exec_ty,
        );
        const final_result_exec_ty = if (self.executableTypeIsAbstract(expected_exec_ty))
            self.output.getExpr(result_expr).ty
        else
            try self.mergeExecutableSignatureType(self.output.getExpr(result_expr).ty, expected_exec_ty);
        if (!self.types.equalIds(self.output.getExpr(result_expr).ty, final_result_exec_ty)) {
            result_expr = try self.emitExplicitBridgeExpr(result_expr, final_result_exec_ty);
        }
        const final_expr = self.output.getExpr(result_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
        };
    }

    fn specializeCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        _: *lower_type.MonoCache,
        venv: []const EnvEntry,
        _: solved.Ast.ExprId,
        call: @FieldType(solved.Ast.Expr.Data, "call"),
        expected_result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const base_func_source = self.input.store.getExpr(call.func);
        const func_ty = try self.instantiatedSourceTypeForExpr(inst, venv, call.func);
        const arg_expr_ids = self.input.store.sliceExprSpan(call.args);
        const arg_source_tys = try self.allocator.alloc(TypeVarId, arg_expr_ids.len);
        defer self.allocator.free(arg_source_tys);

        for (arg_expr_ids, 0..) |arg_expr_id, i| {
            arg_source_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
        }

        var box_boundary = self.boxBoundaryBuiltinOpForExpr(venv, base_func_source);
        if (box_boundary) |op| {
            var frozen_boundary = try self.freezeCallWorld(
                inst.types,
                venv,
                func_ty,
                arg_source_tys,
                expected_result_source_ty,
            );
            defer frozen_boundary.deinit(self.allocator);
            if (arg_expr_ids.len != 1) {
                debugPanic("lambdamono.lower.specializeCallExpr box boundary call must have arity 1");
            }
            const lowered_arg = try self.specializeBoxBoundaryArgExpr(&frozen_boundary.inst, &frozen_boundary.mono_cache, frozen_boundary.env, op, arg_expr_ids[0]);
            const result_expr = try self.output.addExpr(.{
                .ty = try self.boxBoundaryResultTypeFromArg(op, self.output.getExpr(lowered_arg).ty),
                .data = .{ .low_level = .{
                    .op = op,
                    .args = try self.output.addExprSpan(&.{lowered_arg}),
                } },
            });
            const lowered = self.output.getExpr(result_expr);
            return .{
                .ty = lowered.ty,
                .data = lowered.data,
            };
        }

        const direct_func_symbol = self.directCallableSymbolFromExpr(venv, base_func_source);
        const exact_top_level_symbol = if (direct_func_symbol) |symbol|
            if (inst.types.hasCapturelessLambda(func_ty, symbol)) symbol else null
        else
            null;
        const result_expr = if (exact_top_level_symbol) |target_symbol| blk: {
            const requested_result_source_ty = if (try self.exactConcreteResultSourceTy(inst, target_symbol)) |authoritative_ret_ty|
                authoritative_ret_ty
            else
                expected_result_source_ty;
            const exact_requested_ty = try self.buildExactRequestedFnType(
                inst.types,
                target_symbol,
                arg_source_tys,
                requested_result_source_ty,
            );
            var frozen_exact = if (self.isHostedFunctionSymbol(target_symbol))
                try self.freezeCallWorld(
                    inst.types,
                    venv,
                    func_ty,
                    arg_source_tys,
                    requested_result_source_ty,
                )
            else
                try self.freezeExactRequestedCallWorld(
                    inst.types,
                    venv,
                    target_symbol,
                    exact_requested_ty,
                );
            defer frozen_exact.deinit(self.allocator);

            const requested_call_ty = frozen_exact.fn_ty;
            const fn_shape = frozen_exact.inst.types.fnShape(requested_call_ty);
            const fn_arg_tys = frozen_exact.inst.types.sliceTypeVarSpan(fn_shape.args);
            try self.refineCallArgSourceTypesIn(
                &frozen_exact.inst,
                frozen_exact.env,
                arg_expr_ids,
                fn_arg_tys,
            );
            const lowered_args = try self.allocator.alloc(ast.ExprId, arg_expr_ids.len);
            defer self.allocator.free(lowered_args);
            for (arg_expr_ids, 0..) |arg_expr_id, i| {
                lowered_args[i] = try self.specializeExprAtSourceTy(
                    &frozen_exact.inst,
                    &frozen_exact.mono_cache,
                    frozen_exact.env,
                    arg_expr_id,
                    fn_arg_tys[i],
                );
            }
            const hosted_sig = if (self.isHostedFunctionSymbol(target_symbol))
                self.lookupHostedCallSig(target_symbol)
            else
                null;
            const specialized = if (!self.isHostedFunctionSymbol(target_symbol))
                try self.ensureQueuedCallableSpecializedForCall(
                    frozen_exact.inst.types,
                    target_symbol,
                    .natural,
                    requested_call_ty,
                    null,
                    lowered_args,
                    expected_exec_ty,
                )
            else
                null;
            break :blk try self.applyExactTopLevelFunctionCall(
                &frozen_exact.inst,
                &frozen_exact.mono_cache,
                if (specialized) |resolved| resolved.symbol else target_symbol,
                fn_arg_tys,
                lowered_args,
                if (hosted_sig) |sig| sig.args_tys else specialized.?.exec_args_tys,
                if (hosted_sig) |sig| sig.ret_ty else specialized.?.exec_ret_ty,
                expected_exec_ty,
            );
        } else blk: {
            var frozen = try self.freezeCallWorld(
                inst.types,
                venv,
                func_ty,
                arg_source_tys,
                expected_result_source_ty,
            );
            defer frozen.deinit(self.allocator);

            const requested_call_ty = frozen.fn_ty;
            const fn_shape = frozen.inst.types.fnShape(requested_call_ty);
            const fn_arg_tys = frozen.inst.types.sliceTypeVarSpan(fn_shape.args);
            var lowered_func = try self.specializeExprAtSourceTy(
                &frozen.inst,
                &frozen.mono_cache,
                frozen.env,
                call.func,
                requested_call_ty,
            );
            const expected_func_exec_ty = try self.lowerExecutableTypeFromSolvedIn(
                frozen.inst.types,
                &frozen.mono_cache,
                requested_call_ty,
            );
            if (!self.types.equalIds(self.output.getExpr(lowered_func).ty, expected_func_exec_ty) and
                !self.executableTypeIsAbstract(expected_func_exec_ty))
            {
                lowered_func = try self.bridgeExprAtSolvedTypeToExpectedExecutableType(
                    &frozen.inst,
                    &frozen.mono_cache,
                    requested_call_ty,
                    lowered_func,
                    expected_func_exec_ty,
                );
            }
            const lowered_func_exec_ty = self.output.getExpr(lowered_func).ty;
            if (self.executableTypeIsAbstract(lowered_func_exec_ty)) {
                debugPanic("lambdamono.lower.specializeCallExpr callable value lowered to abstract executable type");
            }
            if (box_boundary == null) {
                const lowered_func_expr = self.output.getExpr(lowered_func);
                if (lowered_func_expr.data == .var_) {
                    box_boundary = self.boxBoundaryBuiltinOp(lowered_func_expr.data.var_);
                }
            }

            const lowered_args = try self.allocator.alloc(ast.ExprId, arg_expr_ids.len);
            defer self.allocator.free(lowered_args);
            for (arg_expr_ids, 0..) |arg_expr_id, i| {
                lowered_args[i] = try self.specializeExprAtSourceTy(
                    &frozen.inst,
                    &frozen.mono_cache,
                    frozen.env,
                    arg_expr_id,
                    fn_arg_tys[i],
                );
            }
            const result_exec_ty = try self.lowerAppliedResultExecutableType(
                frozen.inst.types,
                &frozen.mono_cache,
                .{ .expr = lowered_func },
                requested_call_ty,
                fn_shape.ret,
            );
            break :blk try self.applyCallableValueCall(
                &frozen.inst,
                &frozen.mono_cache,
                lowered_func,
                requested_call_ty,
                requested_call_ty,
                lowered_func_exec_ty,
                fn_arg_tys,
                lowered_args,
                result_exec_ty,
                direct_func_symbol,
            );
        };

        const lowered = self.output.getExpr(result_expr);
        return .{
            .ty = lowered.ty,
            .data = lowered.data,
        };
    }

    fn specializeBlockExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        incoming_env: []const EnvEntry,
        block: @FieldType(solved.Ast.Expr.Data, "block"),
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var env = try self.cloneEnv(incoming_env);
        defer self.allocator.free(env);

        const stmts = self.input.store.sliceStmtSpan(block.stmts);
        const out = try self.allocator.alloc(ast.StmtId, stmts.len);
        defer self.allocator.free(out);

        for (stmts, 0..) |stmt_id, i| {
            const result = try self.specializeStmt(inst, mono_cache, env, stmt_id);
            out[i] = result.stmt;
            self.allocator.free(env);
            env = result.env;
        }

        return .{
            .stmts = try self.output.addStmtSpan(out),
            .final_expr = try self.specializeExpr(inst, mono_cache, env, block.final_expr),
        };
    }

    fn specializeForExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        for_expr: @FieldType(solved.Ast.Expr.Data, "for_"),
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "for_") {
        const iterable = try self.specializeExpr(inst, mono_cache, venv, for_expr.iterable);
        const pat_result = try self.specializePat(inst, mono_cache, for_expr.patt);
        defer self.allocator.free(pat_result.additions);
        const body_env = try self.concatEnv(venv, pat_result.additions);
        defer self.allocator.free(body_env);
        return .{
            .patt = pat_result.pat,
            .iterable = iterable,
            .body = try self.specializeExpr(inst, mono_cache, body_env, for_expr.body),
        };
    }

    const PatResult = struct {
        pat: ast.PatId,
        additions: []EnvEntry,
    };

    fn specializePat(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat_id: solved.Ast.PatId,
    ) std.mem.Allocator.Error!PatResult {
        const pat = self.input.store.getPat(pat_id);
        const ty = try self.cloneInstType(inst, pat.ty);
        return try self.specializePatAtSourceTy(inst, mono_cache, pat, ty);
    }

    fn specializePatAtSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat: solved.Ast.Pat,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!PatResult {
        const lowered_ty = try self.lowerPatternTypeAtSourceTy(inst, mono_cache, ty);
        return try self.specializePatAtSourceAndExecutableTy(inst, mono_cache, pat, ty, lowered_ty);
    }

    fn specializePatAtSourceAndExecutableTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat: solved.Ast.Pat,
        ty: TypeVarId,
        lowered_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!PatResult {
        return switch (pat.data) {
            .var_ => |symbol| .{
                .pat = try self.output.addPat(.{
                    .ty = lowered_ty,
                    .data = .{ .var_ = symbol },
                }),
                .additions = if (symbol.isNone())
                    try self.allocator.dupe(EnvEntry, &.{})
                else
                    try self.allocator.dupe(EnvEntry, &.{.{ .symbol = symbol, .ty = ty, .exec_ty = lowered_ty, .exact_fn_symbol = null }}),
            },
            .bool_lit => |value| .{
                .pat = try self.output.addPat(.{
                    .ty = lowered_ty,
                    .data = .{ .bool_lit = value },
                }),
                .additions = try self.allocator.dupe(EnvEntry, &.{}),
            },
            .tag => |tag| blk: {
                const source_args = self.input.store.slicePatSpan(tag.args);
                const lowered_args = try self.allocator.alloc(ast.PatId, source_args.len);
                defer self.allocator.free(lowered_args);

                var additions = std.ArrayList(EnvEntry).empty;
                defer additions.deinit(self.allocator);
                const tag_info = self.solvedTagInfoByName(inst.types, ty, tag.name) orelse
                    debugPanic("lambdamono.lower.specializePat missing solved tag info");
                const arg_tys = tag_info.args;
                const exec_tag_info = self.executableTagInfoByName(lowered_ty, tag.name) orelse
                    debugPanic("lambdamono.lower.specializePat missing executable tag info");
                const exec_arg_tys = exec_tag_info.args;
                if (arg_tys.len != source_args.len) {
                    debugPanic("lambdamono.lower.specializePat expected tag arg count mismatch");
                }
                if (exec_arg_tys.len != source_args.len) {
                    debugPanic("lambdamono.lower.specializePat expected executable tag arg count mismatch");
                }
                for (source_args, 0..) |arg_pat, i| {
                    const arg_pat_data = self.input.store.getPat(arg_pat);
                    const arg_ty = arg_tys[i];
                    const lowered = try self.specializePatAtSourceAndExecutableTy(
                        inst,
                        mono_cache,
                        arg_pat_data,
                        arg_ty,
                        exec_arg_tys[i],
                    );
                    lowered_args[i] = lowered.pat;
                    try additions.appendSlice(self.allocator, lowered.additions);
                    self.allocator.free(lowered.additions);
                }

                break :blk .{
                    .pat = try self.output.addPat(.{
                        .ty = lowered_ty,
                        .data = .{ .tag = .{
                            .name = .{ .ctor = tag.name },
                            .discriminant = tag_info.discriminant,
                            .args = try self.output.addPatSpan(lowered_args),
                        } },
                    }),
                    .additions = try additions.toOwnedSlice(self.allocator),
                };
            },
        };
    }

    const StmtResult = struct {
        stmt: ast.StmtId,
        env: []EnvEntry,
    };

    fn specializeStmt(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        stmt_id: solved.Ast.StmtId,
    ) std.mem.Allocator.Error!StmtResult {
        const stmt = self.input.store.getStmt(stmt_id);
        return switch (stmt) {
            .decl => |decl| blk: {
                const bind_source_id = self.input.types.unlinkPreservingNominalConst(decl.bind.ty);
                const bind_source_preexisting = inst.mapping.get(bind_source_id);
                var bind_source_cache = lower_type.MonoCache.init(self.allocator);
                defer bind_source_cache.deinit();
                const bind_source_exec_ty = try self.lowerExecutableTypeFromSolvedIn(
                    &self.input.types,
                    &bind_source_cache,
                    decl.bind.ty,
                );
                const bind_ty = try self.cloneTypeIntoInstFromStore(inst, &self.input.types, decl.bind.ty);
                if (!self.sourceTypeIsAbstract(&self.input.types, decl.bind.ty) and
                    self.sourceTypeIsAbstract(inst.types, bind_ty))
                {
                    const bind_name = self.input.idents.getText(self.input.symbols.get(decl.bind.symbol).name);
                    const source_key = self.input.types.structuralKeyOwned(decl.bind.ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, source_key, "<oom>")) self.input.types.allocator.free(source_key);
                    const cloned_key = inst.types.structuralKeyOwned(bind_ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, cloned_key, "<oom>")) inst.types.allocator.free(cloned_key);
                    std.debug.panic(
                        "lambdamono.lower.specializeStmt concrete source bind cloned abstract bind={s} source={s} cloned={s}",
                        .{ bind_name, source_key, cloned_key },
                    );
                }
                const bind_pre_exec_ty = if (!self.sourceTypeIsAbstract(inst.types, bind_ty))
                    try self.lowerExecutableTypeFromSolvedIn(
                        inst.types,
                        mono_cache,
                        bind_ty,
                    )
                else
                    null;
                const initial_body = try self.specializeExprAtSourceTy(inst, mono_cache, venv, decl.body, bind_ty);
                const source_exact_fn_symbol = self.directCallableSymbol(self.input.store.getExpr(decl.body));
                const refined_bind_ty = try self.refinedSourceTypeForLoweredExpr(
                    inst,
                    venv,
                    decl.body,
                    bind_ty,
                    initial_body,
                );
                const effective_bind_ty = if (self.sourceTypeIsAbstract(inst.types, bind_ty))
                    refined_bind_ty
                else
                    bind_ty;
                if (!self.sourceTypeIsAbstract(&self.input.types, decl.bind.ty) and
                    self.sourceTypeIsAbstract(inst.types, effective_bind_ty))
                {
                    const bind_name = self.input.idents.getText(self.input.symbols.get(decl.bind.symbol).name);
                    const source_key = self.input.types.structuralKeyOwned(decl.bind.ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, source_key, "<oom>")) self.input.types.allocator.free(source_key);
                    const effective_key = inst.types.structuralKeyOwned(effective_bind_ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, effective_key, "<oom>")) inst.types.allocator.free(effective_key);
                    std.debug.panic(
                        "lambdamono.lower.specializeStmt concrete source bind refined abstract bind={s} source={s} effective={s}",
                        .{ bind_name, source_key, effective_key },
                    );
                }
                const body = try self.refineBindingBodyIfNeeded(
                    inst,
                    mono_cache,
                    venv,
                    decl.body,
                    effective_bind_ty,
                    initial_body,
                );
                if (!self.sourceTypeIsAbstract(inst.types, effective_bind_ty)) {
                    const expected_exec_ty = try self.lowerExecutableTypeFromSolvedIn(
                        inst.types,
                        mono_cache,
                        effective_bind_ty,
                    );
                    const actual_exec_ty = self.output.getExpr(body).ty;
                    if (!self.types.equalIds(actual_exec_ty, expected_exec_ty)) {
                        const bind_name = self.input.idents.getText(self.input.symbols.get(decl.bind.symbol).name);
                        const current_fn_name = if (self.current_specializing_symbol) |symbol|
                            self.input.idents.getText(self.input.symbols.get(symbol).name)
                        else
                            "<none>";
                        const source_key = inst.types.structuralKeyOwned(effective_bind_ty) catch "<oom>";
                        defer if (!std.mem.eql(u8, source_key, "<oom>")) inst.types.allocator.free(source_key);
                        const source_exec_summary = self.debugExecutableTypeSummary(bind_source_exec_ty);
                        defer source_exec_summary.deinit(self.allocator);
                        const mapped_summary = if (bind_source_preexisting) |mapped_ty|
                            self.debugExecutableTypeSummary(try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, mapped_ty))
                        else
                            DebugTypeText{ .text = "<none>", .owned = false };
                        defer mapped_summary.deinit(self.allocator);
                        const pre_summary = if (bind_pre_exec_ty) |pre_ty|
                            self.debugExecutableTypeSummary(pre_ty)
                        else
                            DebugTypeText{ .text = "<abstract>", .owned = false };
                        defer pre_summary.deinit(self.allocator);
                        const actual_summary = self.debugExecutableTypeSummary(actual_exec_ty);
                        defer actual_summary.deinit(self.allocator);
                        const expected_summary = self.debugExecutableTypeSummary(expected_exec_ty);
                        defer expected_summary.deinit(self.allocator);
                        std.debug.panic(
                            "lambdamono.lower.specializeStmt decl normalization mismatch fn={s} bind={s} source_input_exec={s} preexisting_map_exec={s} pre_exec={s} source={s} actual_exec={s} expected_exec={s}",
                            .{
                                current_fn_name,
                                bind_name,
                                source_exec_summary.text,
                                mapped_summary.text,
                                pre_summary.text,
                                source_key,
                                actual_summary.text,
                                expected_summary.text,
                            },
                        );
                    }
                }
                const normalized_body = try self.normalizeBindingBodyToSourceTy(
                    inst,
                    mono_cache,
                    effective_bind_ty,
                    body,
                );
                const bind_exec_ty = self.output.getExpr(normalized_body).ty;
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .decl = .{
                            .bind = .{
                                .ty = bind_exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = normalized_body,
                        },
                    }),
                    .env = blk_env: {
                        break :blk_env try self.extendEnv(venv, .{
                            .symbol = decl.bind.symbol,
                            .ty = effective_bind_ty,
                            .exec_ty = bind_exec_ty,
                            .exact_fn_symbol = source_exact_fn_symbol,
                        });
                    },
                };
            },
            .var_decl => |decl| blk: {
                const bind_ty = try self.cloneTypeIntoInstFromStore(inst, &self.input.types, decl.bind.ty);
                if (!self.sourceTypeIsAbstract(&self.input.types, decl.bind.ty) and
                    self.sourceTypeIsAbstract(inst.types, bind_ty))
                {
                    const bind_name = self.input.idents.getText(self.input.symbols.get(decl.bind.symbol).name);
                    const source_key = self.input.types.structuralKeyOwned(decl.bind.ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, source_key, "<oom>")) self.input.types.allocator.free(source_key);
                    const cloned_key = inst.types.structuralKeyOwned(bind_ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, cloned_key, "<oom>")) inst.types.allocator.free(cloned_key);
                    std.debug.panic(
                        "lambdamono.lower.specializeStmt concrete source var_decl cloned abstract bind={s} source={s} cloned={s}",
                        .{ bind_name, source_key, cloned_key },
                    );
                }
                const initial_body = try self.specializeExprAtSourceTy(inst, mono_cache, venv, decl.body, bind_ty);
                const source_exact_fn_symbol = self.directCallableSymbol(self.input.store.getExpr(decl.body));
                const refined_bind_ty = try self.refinedSourceTypeForLoweredExpr(
                    inst,
                    venv,
                    decl.body,
                    bind_ty,
                    initial_body,
                );
                const effective_bind_ty = if (self.sourceTypeIsAbstract(inst.types, bind_ty))
                    refined_bind_ty
                else
                    bind_ty;
                if (!self.sourceTypeIsAbstract(&self.input.types, decl.bind.ty) and
                    self.sourceTypeIsAbstract(inst.types, effective_bind_ty))
                {
                    const bind_name = self.input.idents.getText(self.input.symbols.get(decl.bind.symbol).name);
                    const source_key = self.input.types.structuralKeyOwned(decl.bind.ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, source_key, "<oom>")) self.input.types.allocator.free(source_key);
                    const effective_key = inst.types.structuralKeyOwned(effective_bind_ty) catch "<oom>";
                    defer if (!std.mem.eql(u8, effective_key, "<oom>")) inst.types.allocator.free(effective_key);
                    std.debug.panic(
                        "lambdamono.lower.specializeStmt concrete source var_decl refined abstract bind={s} source={s} effective={s}",
                        .{ bind_name, source_key, effective_key },
                    );
                }
                const body = try self.refineBindingBodyIfNeeded(
                    inst,
                    mono_cache,
                    venv,
                    decl.body,
                    effective_bind_ty,
                    initial_body,
                );
                if (!self.sourceTypeIsAbstract(inst.types, effective_bind_ty)) {
                    const expected_exec_ty = try self.lowerExecutableTypeFromSolvedIn(
                        inst.types,
                        mono_cache,
                        effective_bind_ty,
                    );
                    const actual_exec_ty = self.output.getExpr(body).ty;
                    if (!self.types.equalIds(actual_exec_ty, expected_exec_ty)) {
                        const bind_name = self.input.idents.getText(self.input.symbols.get(decl.bind.symbol).name);
                        const source_key = inst.types.structuralKeyOwned(effective_bind_ty) catch "<oom>";
                        defer if (!std.mem.eql(u8, source_key, "<oom>")) inst.types.allocator.free(source_key);
                        const actual_summary = self.debugExecutableTypeSummary(actual_exec_ty);
                        defer actual_summary.deinit(self.allocator);
                        const expected_summary = self.debugExecutableTypeSummary(expected_exec_ty);
                        defer expected_summary.deinit(self.allocator);
                        std.debug.panic(
                            "lambdamono.lower.specializeStmt var_decl normalization mismatch bind={s} source={s} actual_exec={s} expected_exec={s}",
                            .{ bind_name, source_key, actual_summary.text, expected_summary.text },
                        );
                    }
                }
                const normalized_body = try self.normalizeBindingBodyToSourceTy(
                    inst,
                    mono_cache,
                    effective_bind_ty,
                    body,
                );
                const bind_exec_ty = self.output.getExpr(normalized_body).ty;
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .var_decl = .{
                            .bind = .{
                                .ty = bind_exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = normalized_body,
                        },
                    }),
                    .env = blk_env: {
                        break :blk_env try self.extendEnv(venv, .{
                            .symbol = decl.bind.symbol,
                            .ty = effective_bind_ty,
                            .exec_ty = bind_exec_ty,
                            .exact_fn_symbol = source_exact_fn_symbol,
                        });
                    },
                };
            },
            .reassign => |reassign| .{
                .stmt = try self.output.addStmt(.{
                    .reassign = .{
                        .target = reassign.target,
                        .body = try self.specializeExpr(inst, mono_cache, venv, reassign.body),
                    },
                }),
                .env = try self.cloneEnv(venv),
            },
            .expr => |expr| .{
                .stmt = try self.output.addStmt(.{ .expr = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .debug => |expr| .{
                .stmt = try self.output.addStmt(.{ .debug = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .expect => |expr| .{
                .stmt = try self.output.addStmt(.{ .expect = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .crash => |msg| .{
                .stmt = try self.output.addStmt(.{ .crash = msg }),
                .env = try self.cloneEnv(venv),
            },
            .return_ => |expr| .{
                .stmt = try self.output.addStmt(.{ .return_ = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .break_ => .{
                .stmt = try self.output.addStmt(.break_),
                .env = try self.cloneEnv(venv),
            },
            .for_ => |for_stmt| blk: {
                const pat_result = try self.specializePat(inst, mono_cache, for_stmt.patt);
                defer self.allocator.free(pat_result.additions);
                const body_env = try self.concatEnv(venv, pat_result.additions);
                defer self.allocator.free(body_env);
                break :blk .{
                    .stmt = try self.output.addStmt(.{ .for_ = .{
                        .patt = pat_result.pat,
                        .iterable = try self.specializeExpr(inst, mono_cache, venv, for_stmt.iterable),
                        .body = try self.specializeExpr(inst, mono_cache, body_env, for_stmt.body),
                    } }),
                    .env = try self.cloneEnv(venv),
                };
            },
            .while_ => |while_stmt| .{
                .stmt = try self.output.addStmt(.{ .while_ = .{
                    .cond = try self.specializeExpr(inst, mono_cache, venv, while_stmt.cond),
                    .body = try self.specializeExpr(inst, mono_cache, venv, while_stmt.body),
                } }),
                .env = try self.cloneEnv(venv),
            },
        };
    }

    fn specializeExprSpan(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        span: solved.Ast.Span(solved.Ast.ExprId),
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const source = self.input.store.sliceExprSpan(span);
        const out = try self.allocator.alloc(ast.ExprId, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |expr_id, i| {
            out[i] = try self.specializeExpr(inst, mono_cache, venv, expr_id);
        }
        return try self.output.addExprSpan(out);
    }

    fn cloneInstType(self: *Lowerer, inst: *InstScope, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        if (inst.mapping.get(id)) |cached| return cached;

        const cloned = switch (self.input.types.getNode(id)) {
            .link => unreachable,
            .for_a => try inst.types.freshFlexForA(),
            .flex_for_a => try inst.types.freshFlexForA(),
            .unbd => try inst.types.freshUnbd(),
            .nominal => |nominal| blk: {
                const placeholder = try inst.types.freshUnbd();
                try inst.mapping.put(id, placeholder);
                const args = self.input.types.sliceTypeVarSpan(nominal.args);
                const cloned_args = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(cloned_args);
                for (args, 0..) |arg, i| {
                    cloned_args[i] = try self.cloneInstType(inst, arg);
                }
                const node = solved.Type.Node{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try inst.types.addTypeVarSpan(cloned_args),
                    .backing = try self.cloneInstType(inst, nominal.backing),
                } };
                inst.types.setNode(placeholder, node);
                break :blk placeholder;
            },
            .content => |content| blk: {
                const placeholder = try inst.types.freshUnbd();
                try inst.mapping.put(id, placeholder);
                const node = switch (content) {
                    .primitive => solved.Type.Node{ .content = .{ .primitive = content.primitive } },
                    .func => |func| blk2: {
                        const args = self.input.types.sliceTypeVarSpan(func.args);
                        const out_args = try self.allocator.alloc(TypeVarId, args.len);
                        defer self.allocator.free(out_args);
                        for (args, 0..) |arg, i| {
                            out_args[i] = try self.cloneInstType(inst, arg);
                        }
                        break :blk2 solved.Type.Node{ .content = .{ .func = .{
                            .args = try inst.types.addTypeVarSpan(out_args),
                            .lset = try self.cloneInstType(inst, func.lset),
                            .ret = try self.cloneInstType(inst, func.ret),
                        } } };
                    },
                    .list => |elem| solved.Type.Node{ .content = .{
                        .list = try self.cloneInstType(inst, elem),
                    } },
                    .box => |elem| solved.Type.Node{ .content = .{
                        .box = try self.cloneInstType(inst, elem),
                    } },
                    .tuple => |tuple| blk2: {
                        const elems = self.input.types.sliceTypeVarSpan(tuple);
                        const elems_copy = try self.allocator.dupe(TypeVarId, elems);
                        defer self.allocator.free(elems_copy);
                        const out = try self.allocator.alloc(TypeVarId, elems_copy.len);
                        defer self.allocator.free(out);
                        for (elems_copy, 0..) |elem, i| {
                            out[i] = try self.cloneInstType(inst, elem);
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .tuple = try inst.types.addTypeVarSpan(out),
                        } };
                    },
                    .record => |record| blk2: {
                        const fields = self.input.types.sliceFields(record.fields);
                        const fields_copy = try self.allocator.dupe(solved.Type.Field, fields);
                        defer self.allocator.free(fields_copy);
                        const out = try self.allocator.alloc(solved.Type.Field, fields_copy.len);
                        defer self.allocator.free(out);
                        for (fields_copy, 0..) |field, i| {
                            out[i] = .{
                                .name = field.name,
                                .ty = try self.cloneInstType(inst, field.ty),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .record = .{ .fields = try inst.types.addFields(out) },
                        } };
                    },
                    .tag_union => |tag_union| blk2: {
                        const tags = self.input.types.sliceTags(tag_union.tags);
                        const tags_copy = try self.allocator.dupe(solved.Type.Tag, tags);
                        defer self.allocator.free(tags_copy);
                        const out = try self.allocator.alloc(solved.Type.Tag, tags_copy.len);
                        defer self.allocator.free(out);
                        for (tags_copy, 0..) |tag, i| {
                            const args = self.input.types.sliceTypeVarSpan(tag.args);
                            const args_copy = try self.allocator.dupe(TypeVarId, args);
                            defer self.allocator.free(args_copy);
                            const out_args = try self.allocator.alloc(TypeVarId, args_copy.len);
                            defer self.allocator.free(out_args);
                            for (args_copy, 0..) |arg, arg_i| {
                                out_args[arg_i] = try self.cloneInstType(inst, arg);
                            }
                            out[i] = .{
                                .name = tag.name,
                                .args = try inst.types.addTypeVarSpan(out_args),
                            };
                        }
                        const tags_span = try inst.types.addTags(out);
                        break :blk2 solved.Type.Node{ .content = .{
                            .tag_union = .{ .tags = tags_span },
                        } };
                    },
                    .lambda_set => |lambda_set| blk2: {
                        const lambdas = self.input.types.sliceLambdas(lambda_set);
                        const lambdas_copy = try self.allocator.dupe(solved.Type.Lambda, lambdas);
                        defer self.allocator.free(lambdas_copy);
                        const out = try self.allocator.alloc(solved.Type.Lambda, lambdas_copy.len);
                        defer self.allocator.free(out);
                        for (lambdas_copy, 0..) |lambda, i| {
                            const captures = self.input.types.sliceCaptures(lambda.captures);
                            const captures_copy = try self.allocator.dupe(solved.Type.Capture, captures);
                            defer self.allocator.free(captures_copy);
                            const out_captures = try self.allocator.alloc(solved.Type.Capture, captures_copy.len);
                            defer self.allocator.free(out_captures);
                            for (captures_copy, 0..) |capture, capture_i| {
                                out_captures[capture_i] = .{
                                    .symbol = capture.symbol,
                                    .ty = try self.cloneInstType(inst, capture.ty),
                                };
                            }
                            out[i] = .{
                                .symbol = lambda.symbol,
                                .captures = try inst.types.addCaptures(out_captures),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .lambda_set = try inst.types.addLambdas(out),
                        } };
                    },
                };
                inst.types.setNode(placeholder, node);
                break :blk placeholder;
            },
        };

        try inst.mapping.put(id, cloned);
        return cloned;
    }

    fn unifyIn(self: *Lowerer, types: *solved.Type.Store, left: TypeVarId, right: TypeVarId) std.mem.Allocator.Error!void {
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        try self.unifyRec(types, left, right, &visited);
    }

    fn unifyRec(
        self: *Lowerer,
        types: *solved.Type.Store,
        left: TypeVarId,
        right: TypeVarId,
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!void {
        const l = types.unlink(left);
        const r = types.unlink(right);
        if (l == r) return;

        const key = (@as(u64, @intFromEnum(l)) << 32) | @as(u64, @intFromEnum(r));
        if (visited.contains(key)) return;
        try visited.put(key, {});

        switch (types.getNode(l)) {
            .unbd => {
                types.setNode(l, .{ .link = r });
                return;
            },
            .flex_for_a => {
                types.setNode(l, .{ .link = r });
                return;
            },
            .for_a => debugPanic("lambdamono.lower.unify generalized type without instantiation"),
            else => {},
        }
        switch (types.getNode(r)) {
            .unbd => {
                types.setNode(r, .{ .link = l });
                return;
            },
            .flex_for_a => {
                types.setNode(r, .{ .link = l });
                return;
            },
            .for_a => debugPanic("lambdamono.lower.unify generalized type without instantiation"),
            else => {},
        }

        const left_node = types.getNode(l);
        const right_node = types.getNode(r);
        const merged = switch (left_node) {
            .nominal => |left_nominal| switch (right_node) {
                .nominal => |right_nominal| {
                    if (left_nominal.module_idx != right_nominal.module_idx or
                        left_nominal.ident != right_nominal.ident or
                        left_nominal.is_opaque != right_nominal.is_opaque)
                    {
                        debugPanic("lambdamono.lower.unify incompatible nominal types");
                    }

                    const left_args = types.sliceTypeVarSpan(left_nominal.args);
                    const right_args = types.sliceTypeVarSpan(right_nominal.args);
                    if (left_args.len != right_args.len) {
                        debugPanic("lambdamono.lower.unify nominal arity mismatch");
                    }

                    for (left_args, 0..) |left_arg, i| {
                        try self.unifyRec(types, left_arg, right_args[i], visited);
                    }
                    try self.unifyRec(types, left_nominal.backing, right_nominal.backing, visited);
                    types.setNode(l, .{ .link = r });
                    return;
                },
                .content => {
                    try self.unifyRec(types, left_nominal.backing, r, visited);
                    return;
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible nodes: left={s} right={s}", .{ @tagName(left_node), @tagName(right_node) }),
            },
            .content => |left_content| switch (right_node) {
                .nominal => |right_nominal| {
                    try self.unifyRec(types, l, right_nominal.backing, visited);
                    return;
                },
                .content => |right_content| try self.unifyContent(types, left_content, right_content, visited),
                else => debugPanicFmt("lambdamono.lower.unify incompatible nodes: left={s} right={s}", .{ @tagName(left_node), @tagName(right_node) }),
            },
            else => debugPanic("lambdamono.lower.unify incompatible types"),
        };

        types.setNode(l, .{ .content = merged });
        types.setNode(r, .{ .link = l });
    }

    fn unifyContent(
        self: *Lowerer,
        types: *solved.Type.Store,
        left: solved.Type.Content,
        right: solved.Type.Content,
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Content {
        switch (left) {
            .primitive => |prim| switch (right) {
                .primitive => |other| {
                    if (prim != other) {
                        if (prim == .dec and isNumericPrimitive(other)) return .{ .primitive = other };
                        if (other == .dec and isNumericPrimitive(prim)) return .{ .primitive = prim };
                        debugPanic("lambdamono.lower.unify incompatible primitives");
                    }
                    return .{ .primitive = prim };
                },
                .lambda_set => {
                    if (prim == .erased) return .{ .primitive = .erased };
                    debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) });
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .func => |func| switch (right) {
                .func => |other| {
                    const func_args = types.sliceTypeVarSpan(func.args);
                    const other_args = types.sliceTypeVarSpan(other.args);
                    if (func_args.len != other_args.len) {
                        debugPanicFmt("lambdamono.lower.unify incompatible function arity: left={d} right={d}", .{ func_args.len, other_args.len });
                    }
                    for (func_args, other_args) |left_arg, right_arg| {
                        try self.unifyRec(types, left_arg, right_arg, visited);
                    }
                    try self.unifyRec(types, func.lset, other.lset, visited);
                    try self.unifyRec(types, func.ret, other.ret, visited);
                    return .{ .func = .{
                        .args = func.args,
                        .lset = func.lset,
                        .ret = func.ret,
                    } };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .list => |elem| switch (right) {
                .list => |other| {
                    try self.unifyRec(types, elem, other, visited);
                    return .{ .list = elem };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .box => |elem| switch (right) {
                .box => |other| {
                    try self.unifyRec(types, elem, other, visited);
                    return .{ .box = elem };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .tuple => |tuple| switch (right) {
                .tuple => |other| {
                    const left_elems = types.sliceTypeVarSpan(tuple);
                    const right_elems = types.sliceTypeVarSpan(other);
                    if (left_elems.len != right_elems.len) debugPanic("lambdamono.lower.unify tuple arity mismatch");
                    for (left_elems, right_elems) |left_elem, right_elem| {
                        try self.unifyRec(types, left_elem, right_elem, visited);
                    }
                    return .{ .tuple = tuple };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .record => |record| switch (right) {
                .record => |other| {
                    return .{ .record = .{
                        .fields = try self.unifyRecordFields(types, record.fields, other.fields, visited),
                    } };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .tag_union => |tag_union| switch (right) {
                .tag_union => |other| {
                    return .{ .tag_union = .{
                        .tags = try self.unifyTags(types, tag_union.tags, other.tags, visited),
                    } };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .lambda_set => |lambda_set| switch (right) {
                .lambda_set => |other| {
                    return .{ .lambda_set = try self.unifyLambdaSet(types, lambda_set, other, visited) };
                },
                .primitive => |other| {
                    if (other == .erased) return .{ .primitive = .erased };
                    debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) });
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
        }
    }

    fn isNumericPrimitive(prim: solved.Type.Prim) bool {
        return switch (prim) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => true,
            .bool, .str, .erased => false,
        };
    }

    fn unifyRecordFields(
        self: *Lowerer,
        types: *solved.Type.Store,
        left_span: solved.Type.Span(solved.Type.Field),
        right_span: solved.Type.Span(solved.Type.Field),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Field) {
        const left_fields = try self.allocator.dupe(solved.Type.Field, types.sliceFields(left_span));
        defer self.allocator.free(left_fields);
        const right_fields = try self.allocator.dupe(solved.Type.Field, types.sliceFields(right_span));
        defer self.allocator.free(right_fields);
        var out = std.ArrayList(solved.Type.Field).empty;
        defer out.deinit(self.allocator);

        var i: usize = 0;
        var j: usize = 0;
        while (i < left_fields.len or j < right_fields.len) {
            if (i == left_fields.len) {
                try out.append(self.allocator, right_fields[j]);
                j += 1;
                continue;
            }
            if (j == right_fields.len) {
                try out.append(self.allocator, left_fields[i]);
                i += 1;
                continue;
            }

            const order = std.mem.order(
                u8,
                self.input.idents.getText(left_fields[i].name),
                self.input.idents.getText(right_fields[j].name),
            );
            switch (order) {
                .lt => {
                    try out.append(self.allocator, left_fields[i]);
                    i += 1;
                },
                .gt => {
                    try out.append(self.allocator, right_fields[j]);
                    j += 1;
                },
                .eq => {
                    try self.unifyRec(types, left_fields[i].ty, right_fields[j].ty, visited);
                    try out.append(self.allocator, .{
                        .name = left_fields[i].name,
                        .ty = left_fields[i].ty,
                    });
                    i += 1;
                    j += 1;
                },
            }
        }

        return try types.addFields(out.items);
    }

    fn unifyTags(
        self: *Lowerer,
        types: *solved.Type.Store,
        left_span: solved.Type.Span(solved.Type.Tag),
        right_span: solved.Type.Span(solved.Type.Tag),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Tag) {
        const left_tags = try self.allocator.dupe(solved.Type.Tag, types.sliceTags(left_span));
        defer self.allocator.free(left_tags);
        const right_tags = try self.allocator.dupe(solved.Type.Tag, types.sliceTags(right_span));
        defer self.allocator.free(right_tags);
        var out = std.ArrayList(solved.Type.Tag).empty;
        defer out.deinit(self.allocator);

        var i: usize = 0;
        var j: usize = 0;
        while (i < left_tags.len or j < right_tags.len) {
            if (i == left_tags.len) {
                try out.append(self.allocator, right_tags[j]);
                j += 1;
                continue;
            }
            if (j == right_tags.len) {
                try out.append(self.allocator, left_tags[i]);
                i += 1;
                continue;
            }

            const order = std.mem.order(
                u8,
                self.input.idents.getText(left_tags[i].name),
                self.input.idents.getText(right_tags[j].name),
            );
            switch (order) {
                .lt => {
                    try out.append(self.allocator, left_tags[i]);
                    i += 1;
                },
                .gt => {
                    try out.append(self.allocator, right_tags[j]);
                    j += 1;
                },
                .eq => {
                    const left_args = types.sliceTypeVarSpan(left_tags[i].args);
                    const right_args = types.sliceTypeVarSpan(right_tags[j].args);
                    if (left_args.len != right_args.len) {
                        debugPanic("lambdamono.lower.unify tag arity mismatch");
                    }
                    for (left_args, right_args) |left_arg, right_arg| {
                        try self.unifyRec(types, left_arg, right_arg, visited);
                    }
                    try out.append(self.allocator, .{
                        .name = left_tags[i].name,
                        .args = left_tags[i].args,
                    });
                    i += 1;
                    j += 1;
                },
            }
        }

        return try types.addTags(out.items);
    }

    fn unifyLambdaSet(
        self: *Lowerer,
        types: *solved.Type.Store,
        left_span: solved.Type.Span(solved.Type.Lambda),
        right_span: solved.Type.Span(solved.Type.Lambda),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Lambda) {
        const left_lambdas = try self.allocator.dupe(solved.Type.Lambda, types.sliceLambdas(left_span));
        defer self.allocator.free(left_lambdas);
        const right_lambdas = try self.allocator.dupe(solved.Type.Lambda, types.sliceLambdas(right_span));
        defer self.allocator.free(right_lambdas);
        var out = std.ArrayList(solved.Type.Lambda).empty;
        defer out.deinit(self.allocator);

        var i: usize = 0;
        var j: usize = 0;
        while (i < left_lambdas.len or j < right_lambdas.len) {
            if (i == left_lambdas.len) {
                try out.append(self.allocator, right_lambdas[j]);
                j += 1;
                continue;
            }
            if (j == right_lambdas.len) {
                try out.append(self.allocator, left_lambdas[i]);
                i += 1;
                continue;
            }

            const left_symbol = left_lambdas[i].symbol.raw();
            const right_symbol = right_lambdas[j].symbol.raw();
            if (left_symbol < right_symbol) {
                try out.append(self.allocator, left_lambdas[i]);
                i += 1;
                continue;
            }
            if (left_symbol > right_symbol) {
                try out.append(self.allocator, right_lambdas[j]);
                j += 1;
                continue;
            }

            try out.append(self.allocator, .{
                .symbol = left_lambdas[i].symbol,
                .captures = try self.unifyCaptures(types, left_lambdas[i].captures, right_lambdas[j].captures, visited),
            });
            i += 1;
            j += 1;
        }

        return try types.addLambdas(out.items);
    }

    fn unifyCaptures(
        self: *Lowerer,
        types: *solved.Type.Store,
        left_span: solved.Type.Span(solved.Type.Capture),
        right_span: solved.Type.Span(solved.Type.Capture),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Capture) {
        const left_caps = try self.allocator.dupe(solved.Type.Capture, types.sliceCaptures(left_span));
        defer self.allocator.free(left_caps);
        const right_caps = try self.allocator.dupe(solved.Type.Capture, types.sliceCaptures(right_span));
        defer self.allocator.free(right_caps);
        var out = std.ArrayList(solved.Type.Capture).empty;
        defer out.deinit(self.allocator);

        var i: usize = 0;
        var j: usize = 0;
        while (i < left_caps.len or j < right_caps.len) {
            if (i == left_caps.len) {
                try out.append(self.allocator, right_caps[j]);
                j += 1;
                continue;
            }
            if (j == right_caps.len) {
                try out.append(self.allocator, left_caps[i]);
                i += 1;
                continue;
            }

            const left_symbol = left_caps[i].symbol.raw();
            const right_symbol = right_caps[j].symbol.raw();
            if (left_symbol < right_symbol) {
                try out.append(self.allocator, left_caps[i]);
                i += 1;
                continue;
            }
            if (left_symbol > right_symbol) {
                try out.append(self.allocator, right_caps[j]);
                j += 1;
                continue;
            }

            try self.unifyRec(types, left_caps[i].ty, right_caps[j].ty, visited);
            try out.append(self.allocator, .{
                .symbol = left_caps[i].symbol,
                .ty = left_caps[i].ty,
            });
            i += 1;
            j += 1;
        }

        return try types.addCaptures(out.items);
    }

    fn lookupEnvEntry(_: *const Lowerer, venv: []const EnvEntry, symbol: Symbol) ?EnvEntry {
        var i = venv.len;
        while (i > 0) {
            i -= 1;
            const entry = venv[i];
            if (entry.symbol == symbol) return entry;
        }
        return null;
    }

    fn assertSortedFields(idents: *const base.Ident.Store, fields: []const type_mod.Field) void {
        if (fields.len <= 1) return;

        var prev = fields[0];
        for (fields[1..]) |field| {
            switch (std.mem.order(
                u8,
                idents.getText(prev.name),
                idents.getText(field.name),
            )) {
                .lt => prev = field,
                .eq => debugPanicFmt("lambdamono lowered duplicate record field {s}", .{
                    idents.getText(field.name),
                }),
                .gt => debugPanicFmt("lambdamono lowered record fields were not pre-sorted: {s} then {s}", .{
                    idents.getText(prev.name),
                    idents.getText(field.name),
                }),
            }
        }
    }

    fn lookupTopLevelValueType(self: *const Lowerer, symbol: Symbol) ?type_mod.TypeId {
        return self.top_level_value_types.get(symbol);
    }

    fn lookupHostedCallSig(self: *const Lowerer, symbol: Symbol) HostedCallSignatureView {
        const sig = self.hosted_call_sigs.get(symbol) orelse
            debugPanic("lambdamono.lower.lookupHostedCallSig missing hosted executable signature");
        return .{
            .args_tys = sig.args_tys,
            .ret_ty = sig.ret_ty,
        };
    }

    fn isHostedFunctionSymbol(self: *const Lowerer, symbol: Symbol) bool {
        return self.hosted_call_sigs.contains(symbol);
    }

    fn cloneEnv(self: *Lowerer, env: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        return try self.allocator.dupe(EnvEntry, env);
    }

    fn extendEnv(self: *Lowerer, env: []const EnvEntry, extra: EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, env.len + 1);
        std.mem.copyForwards(EnvEntry, out[0..env.len], env);
        out[env.len] = extra;
        return out;
    }

    fn concatEnv(self: *Lowerer, left: []const EnvEntry, right: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, left.len + right.len);
        std.mem.copyForwards(EnvEntry, out[0..left.len], left);
        std.mem.copyForwards(EnvEntry, out[left.len..], right);
        return out;
    }

    const DebugTypeText = struct {
        text: []const u8,
        owned: bool,

        fn deinit(self: DebugTypeText, allocator: std.mem.Allocator) void {
            if (self.owned) allocator.free(self.text);
        }
    };

    fn debugExecutableTypeSummary(self: *Lowerer, ty: type_mod.TypeId) DebugTypeText {
        var out = std.ArrayList(u8).empty;
        self.debugWriteExecutableTypeSummary(&out, ty, 3) catch return .{ .text = "<oom>", .owned = false };
        return .{ .text = out.toOwnedSlice(self.allocator) catch return .{ .text = "<oom>", .owned = false }, .owned = true };
    }

    fn debugWriteExecutableTypeSummary(
        self: *Lowerer,
        out: *std.ArrayList(u8),
        ty: type_mod.TypeId,
        depth: u8,
    ) std.mem.Allocator.Error!void {
        if (depth == 0) {
            try out.appendSlice(self.allocator, "...");
            return;
        }

        switch (self.types.getTypePreservingNominal(ty)) {
            .placeholder => try out.appendSlice(self.allocator, "placeholder"),
            .unbd => try out.appendSlice(self.allocator, "unbd"),
            .link => try out.appendSlice(self.allocator, "link"),
            .primitive => |prim| try out.writer(self.allocator).print("prim({s})", .{@tagName(prim)}),
            .list => |elem| {
                try out.appendSlice(self.allocator, "list(");
                try self.debugWriteExecutableTypeSummary(out, elem, depth - 1);
                try out.append(self.allocator, ')');
            },
            .box => |elem| {
                try out.appendSlice(self.allocator, "box(");
                try self.debugWriteExecutableTypeSummary(out, elem, depth - 1);
                try out.append(self.allocator, ')');
            },
            .erased_fn => |capture_ty| {
                try out.appendSlice(self.allocator, "erased_fn");
                if (capture_ty) |capture| {
                    try out.append(self.allocator, '(');
                    try self.debugWriteExecutableTypeSummary(out, capture, depth - 1);
                    try out.append(self.allocator, ')');
                }
            },
            .tuple => |elems| {
                try out.appendSlice(self.allocator, "tuple(");
                for (elems, 0..) |elem, i| {
                    if (i != 0) try out.appendSlice(self.allocator, ", ");
                    try self.debugWriteExecutableTypeSummary(out, elem, depth - 1);
                }
                try out.append(self.allocator, ')');
            },
            .record => |record| {
                try out.appendSlice(self.allocator, "record{");
                for (record.fields, 0..) |field, i| {
                    if (i != 0) try out.appendSlice(self.allocator, ", ");
                    try out.writer(self.allocator).print("#{d}: ", .{@as(u32, field.name.idx)});
                    try self.debugWriteExecutableTypeSummary(out, field.ty, depth - 1);
                }
                try out.append(self.allocator, '}');
            },
            .tag_union => |tag_union| {
                try out.appendSlice(self.allocator, "tag[");
                for (tag_union.tags, 0..) |tag, i| {
                    if (i != 0) try out.appendSlice(self.allocator, " | ");
                    switch (tag.name) {
                        .ctor => |name| try out.writer(self.allocator).print("#{d}", .{@as(u32, name.idx)}),
                        .lambda => |symbol| try out.writer(self.allocator).print("lambda({d})", .{@intFromEnum(symbol)}),
                    }
                    if (tag.args.len != 0) {
                        try out.append(self.allocator, '(');
                        for (tag.args, 0..) |arg, arg_i| {
                            if (arg_i != 0) try out.appendSlice(self.allocator, ", ");
                            try self.debugWriteExecutableTypeSummary(out, arg, depth - 1);
                        }
                        try out.append(self.allocator, ')');
                    }
                }
                try out.append(self.allocator, ']');
            },
            .nominal => |nominal| {
                try out.writer(self.allocator).print(
                    "nominal(m={d}, i={d}, opaque={any}",
                    .{ nominal.module_idx, @as(u32, nominal.ident.idx), nominal.is_opaque },
                );
                if (nominal.args.len != 0) {
                    try out.appendSlice(self.allocator, ", args=[");
                    for (nominal.args, 0..) |arg, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try self.debugWriteExecutableTypeSummary(out, arg, depth - 1);
                    }
                    try out.append(self.allocator, ']');
                }
                try out.appendSlice(self.allocator, ", backing=");
                try self.debugWriteExecutableTypeSummary(out, nominal.backing, depth - 1);
                try out.append(self.allocator, ')');
            },
        }
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

fn debugPanicFmt(comptime msg: []const u8, args: anytype) noreturn {
    @branchHint(.cold);
    std.debug.panic(msg, args);
}

test "lambdamono lower tests" {
    std.testing.refAllDecls(@This());
}
