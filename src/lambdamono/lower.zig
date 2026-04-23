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
    entrypoint_wrappers: []Symbol,
    current_specializing_symbol: ?Symbol,
    current_return_exec_ty: ?type_mod.TypeId,

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
            .entrypoint_wrappers = &.{},
            .current_specializing_symbol = null,
            .current_return_exec_ty = null,
        };
        lowerer.types = type_mod.Store.init(allocator);
        return lowerer;
    }

    fn deinit(self: *Lowerer) void {
        self.top_level_values.deinit();
        self.top_level_value_types.deinit();
        self.inspect_helpers.deinit();
        self.inspect_defs.deinit(self.allocator);
        self.pending_values.deinit(self.allocator);
        self.queue.deinit();
        if (self.entrypoint_wrappers.len > 0) {
            self.allocator.free(self.entrypoint_wrappers);
        }
        specializations.deinitFEnv(self.allocator, self.fenv);
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
            switch (expr.data) {
                .let_ => |let_expr| try layouts.recordType(
                    self.allocator,
                    &self.types,
                    &self.input.idents,
                    let_expr.bind.ty,
                ),
                else => {},
            }
            try layouts.recordExpr(self.allocator, &self.types, &self.input.idents, &self.output, @enumFromInt(@as(u32, @intCast(i))), expr.*);
        }
        for (self.output.stmts.items) |*stmt| {
            try self.finalizeStmt(stmt);
            switch (stmt.*) {
                .decl => |decl| try layouts.recordType(
                    self.allocator,
                    &self.types,
                    &self.input.idents,
                    decl.bind.ty,
                ),
                .var_decl => |decl| try layouts.recordType(
                    self.allocator,
                    &self.types,
                    &self.input.idents,
                    decl.bind.ty,
                ),
                else => {},
            }
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
        try self.seedEntrypointSpecializations();

        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .fn_, .hosted_fn => {},
                .val => |value_expr| {
                    if (self.input.types.maybeLambdaRepr(self.input.store.getExpr(value_expr).ty) == null) {
                        _ = try self.ensureTopLevelValueLowered(def.bind.symbol);
                    }
                },
                .run => _ = try self.ensureTopLevelValueLowered(def.bind.symbol),
            }
        }

        try self.buildEntrypointWrappers();

        while (self.queue.nextPending()) |pending| {
            // Snapshot the pending request before specialization recurses and
            // potentially grows the queue. Specialization must consume stable
            // explicit facts, not keep reading through a queue slot that may
            // be relocated by later appends.
            const specialized_symbol = pending.specialized_symbol;
            try self.preparePendingSummary(pending);
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

    fn executableTypeIsAbstract(self: *const Lowerer, ty: type_mod.TypeId) bool {
        return self.types.containsAbstractLeaf(ty);
    }

    fn executableTypeHasLayoutAbstractLeaf(self: *const Lowerer, ty: type_mod.TypeId) bool {
        var visited = std.AutoHashMap(type_mod.TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.executableTypeHasLayoutAbstractLeafVisited(ty, &visited) catch true;
    }

    fn lowerRequestedExecutableReturnTypeFromSource(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        repr_mode: specializations.Pending.ReprMode,
        comptime context: []const u8,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const lowered = switch (repr_mode) {
            .natural => try self.lowerExecutableTypeFromSolvedIn(
                solved_types,
                mono_cache,
                source_ty,
            ),
            .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(
                solved_types,
                mono_cache,
                source_ty,
            ),
        };
        return self.requireConcreteExecutableType(lowered, context);
    }

    const SourceExecBinding = struct {
        source: TypeVarId,
        exec: type_mod.TypeId,
    };

    const SourceExecPair = struct {
        source: TypeVarId,
        exec: type_mod.TypeId,
    };

    fn lowerRequestedExecutableReturnTypeFromCallRelation(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        source_arg_tys: []const TypeVarId,
        exec_arg_tys: []const type_mod.TypeId,
        source_ret_ty: TypeVarId,
        repr_mode: specializations.Pending.ReprMode,
        comptime context: []const u8,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (source_arg_tys.len != exec_arg_tys.len) {
            debugPanic("lambdamono.lower.lowerRequestedExecutableReturnTypeFromCallRelation arg arity mismatch");
        }
        var bindings = std.ArrayList(SourceExecBinding).empty;
        defer bindings.deinit(self.allocator);
        var active = std.ArrayList(SourceExecPair).empty;
        defer active.deinit(self.allocator);
        for (source_arg_tys, exec_arg_tys) |source_arg_ty, exec_arg_ty| {
            try self.collectSourceExecBindings(
                solved_types,
                source_arg_ty,
                exec_arg_ty,
                &bindings,
                &active,
            );
        }
        const lowered = try self.lowerExecutableTypeFromSolvedWithBindings(
            solved_types,
            mono_cache,
            source_ret_ty,
            bindings.items,
            repr_mode,
            .normal,
        );
        return self.requireConcreteExecutableType(lowered, context);
    }

    fn appendSourceExecBinding(
        self: *Lowerer,
        bindings: *std.ArrayList(SourceExecBinding),
        source_ty: TypeVarId,
        exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        for (bindings.items) |binding| {
            if (binding.source == source_ty) {
                if (!self.types.equalIds(binding.exec, exec_ty)) {
                    debugPanic("lambdamono.lower.appendSourceExecBinding executable type disagreement");
                }
                return;
            }
        }
        try bindings.append(self.allocator, .{ .source = source_ty, .exec = exec_ty });
    }

    fn collectSourceExecBindings(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        source_ty: TypeVarId,
        exec_ty: type_mod.TypeId,
        bindings: *std.ArrayList(SourceExecBinding),
        active: *std.ArrayList(SourceExecPair),
    ) std.mem.Allocator.Error!void {
        const source_root = solved_types.unlinkPreservingNominal(source_ty);
        const exec_root = try self.types.keyId(exec_ty);
        try self.appendSourceExecBinding(bindings, source_root, exec_root);
        for (active.items) |pair| {
            if (pair.source == source_root and pair.exec == exec_root) return;
        }
        try active.append(self.allocator, .{ .source = source_root, .exec = exec_root });
        defer _ = active.pop();

        switch (solved_types.getNode(source_root)) {
            .nominal => |source_nominal| switch (self.types.getTypePreservingNominal(exec_root)) {
                .nominal => |exec_nominal| try self.collectSourceExecBindings(
                    solved_types,
                    source_nominal.backing,
                    exec_nominal.backing,
                    bindings,
                    active,
                ),
                else => {},
            },
            .content => |source_content| switch (source_content) {
                .list => |source_elem| switch (self.types.getTypePreservingNominal(exec_root)) {
                    .list => |exec_elem| try self.collectSourceExecBindings(solved_types, source_elem, exec_elem, bindings, active),
                    else => {},
                },
                .box => |source_elem| switch (self.types.getTypePreservingNominal(exec_root)) {
                    .box => |exec_elem| try self.collectSourceExecBindings(solved_types, source_elem, exec_elem, bindings, active),
                    else => {},
                },
                .tuple => |source_span| switch (self.types.getTypePreservingNominal(exec_root)) {
                    .tuple => |exec_elems| {
                        const source_elems = solved_types.sliceTypeVarSpan(source_span);
                        if (source_elems.len != exec_elems.len) return;
                        for (source_elems, exec_elems) |source_elem, exec_elem| {
                            try self.collectSourceExecBindings(solved_types, source_elem, exec_elem, bindings, active);
                        }
                    },
                    else => {},
                },
                .record => |source_record| switch (self.types.getTypePreservingNominal(exec_root)) {
                    .record => |exec_record| {
                        const source_fields = solved_types.sliceFields(source_record.fields);
                        if (source_fields.len != exec_record.fields.len) return;
                        for (source_fields, exec_record.fields) |source_field, exec_field| {
                            if (source_field.name != exec_field.name) return;
                            try self.collectSourceExecBindings(solved_types, source_field.ty, exec_field.ty, bindings, active);
                        }
                    },
                    else => {},
                },
                .tag_union => |source_union| switch (self.types.getTypePreservingNominal(exec_root)) {
                    .tag_union => |exec_union| {
                        const source_tags = solved_types.sliceTags(source_union.tags);
                        if (source_tags.len != exec_union.tags.len) return;
                        for (source_tags, exec_union.tags) |source_tag, exec_tag| {
                            if (exec_tag.name != .ctor or exec_tag.name.ctor != source_tag.name) return;
                            const source_args = solved_types.sliceTypeVarSpan(source_tag.args);
                            if (source_args.len != exec_tag.args.len) return;
                            for (source_args, exec_tag.args) |source_arg, exec_arg| {
                                try self.collectSourceExecBindings(solved_types, source_arg, exec_arg, bindings, active);
                            }
                        }
                    },
                    else => {},
                },
                .func, .lambda_set, .primitive => {},
            },
            else => {},
        }
    }

    const SourceExecPosition = enum {
        normal,
        box_payload,
    };

    fn lookupSourceExecBinding(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        source_ty: TypeVarId,
        bindings: []const SourceExecBinding,
    ) ?type_mod.TypeId {
        _ = self;
        const source_root = solved_types.unlinkPreservingNominal(source_ty);
        for (bindings) |binding| {
            if (binding.source == source_root) return binding.exec;
        }
        return null;
    }

    fn lowerExecutableTypeFromSolvedWithBindings(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        bindings: []const SourceExecBinding,
        repr_mode: specializations.Pending.ReprMode,
        position: SourceExecPosition,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.lookupSourceExecBinding(solved_types, source_ty, bindings)) |exec_ty| {
            return exec_ty;
        }
        const source_root = solved_types.unlinkPreservingNominal(source_ty);
        return switch (solved_types.getNode(source_root)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerExecutableTypeFromSolvedWithBindings(
                    solved_types,
                    mono_cache,
                    nominal.backing,
                    bindings,
                    repr_mode,
                    position,
                );
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(solved_types, mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .list => |elem| try self.internExecutableType(try self.types.internResolved(.{
                    .list = try self.lowerExecutableTypeFromSolvedWithBindings(
                        solved_types,
                        mono_cache,
                        elem,
                        bindings,
                        repr_mode,
                        .normal,
                    ),
                })),
                .box => |elem| try self.internExecutableType(try self.types.internResolved(.{
                    .box = try self.lowerExecutableTypeFromSolvedWithBindings(
                        solved_types,
                        mono_cache,
                        elem,
                        bindings,
                        repr_mode,
                        .box_payload,
                    ),
                })),
                .tuple => |span| blk: {
                    const elems = solved_types.sliceTypeVarSpan(span);
                    const lowered = try self.allocator.alloc(type_mod.TypeId, elems.len);
                    defer self.allocator.free(lowered);
                    for (elems, 0..) |elem, i| {
                        lowered[i] = try self.lowerExecutableTypeFromSolvedWithBindings(
                            solved_types,
                            mono_cache,
                            elem,
                            bindings,
                            repr_mode,
                            .normal,
                        );
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{
                        .tuple = try self.types.dupeTypeIds(lowered),
                    }));
                },
                .record => |record| blk: {
                    const fields = solved_types.sliceFields(record.fields);
                    const lowered = try self.allocator.alloc(type_mod.Field, fields.len);
                    defer self.allocator.free(lowered);
                    for (fields, 0..) |field, i| {
                        lowered[i] = .{
                            .name = field.name,
                            .ty = try self.lowerExecutableTypeFromSolvedWithBindings(
                                solved_types,
                                mono_cache,
                                field.ty,
                                bindings,
                                repr_mode,
                                .normal,
                            ),
                        };
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{
                        .record = .{ .fields = try self.types.dupeFields(lowered) },
                    }));
                },
                .tag_union => |tag_union| blk: {
                    const source_tags = solved_types.sliceTags(tag_union.tags);
                    const lowered = try self.allocator.alloc(type_mod.Tag, source_tags.len);
                    defer self.allocator.free(lowered);
                    for (source_tags, 0..) |tag, i| {
                        const args = solved_types.sliceTypeVarSpan(tag.args);
                        const lowered_args = try self.allocator.alloc(type_mod.TypeId, args.len);
                        defer self.allocator.free(lowered_args);
                        for (args, 0..) |arg, j| {
                            lowered_args[j] = try self.lowerExecutableTypeFromSolvedWithBindings(
                                solved_types,
                                mono_cache,
                                arg,
                                bindings,
                                repr_mode,
                                .normal,
                            );
                        }
                        lowered[i] = .{
                            .name = .{ .ctor = tag.name },
                            .args = try self.types.dupeTypeIds(lowered_args),
                        };
                    }
                    break :blk try self.internExecutableType(try self.types.internResolved(.{
                        .tag_union = .{ .tags = try self.types.dupeTags(lowered) },
                    }));
                },
                .func => switch (repr_mode) {
                    .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, source_root),
                    .natural => switch (position) {
                        .normal => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, source_root),
                        .box_payload => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, source_root),
                    },
                },
                .primitive, .lambda_set => try self.lowerExecutableTypeFromSolvedIn(solved_types, mono_cache, source_root),
            },
            else => try self.lowerRequestedExecutableReturnTypeFromSource(
                solved_types,
                mono_cache,
                source_root,
                repr_mode,
                "lowerExecutableTypeFromSolvedWithBindings",
            ),
        };
    }

    fn stableConcreteExpectedExecutableType(
        self: *const Lowerer,
        ty: type_mod.TypeId,
        comptime context: []const u8,
    ) ?type_mod.TypeId {
        if (self.executableTypeIsAbstract(ty)) return null;
        const concrete = self.requireConcreteExecutableType(ty, context);
        if (self.isExecutableCallableType(concrete)) return null;
        return concrete;
    }

    fn executableTypeHasLayoutAbstractLeafVisited(
        self: *const Lowerer,
        ty: type_mod.TypeId,
        visited: *std.AutoHashMap(type_mod.TypeId, void),
    ) std.mem.Allocator.Error!bool {
        var root = ty;
        while (true) switch (self.types.types.items[@intFromEnum(root)]) {
            .link => |next| root = next,
            else => break,
        };
        if (visited.contains(root)) return false;
        try visited.put(root, {});

        return switch (self.types.types.items[@intFromEnum(root)]) {
            .placeholder, .unbd => true,
            .link => unreachable,
            .primitive => false,
            .nominal => |nominal| try self.executableTypeHasLayoutAbstractLeafVisited(nominal.backing, visited),
            .list => |elem| try self.executableTypeHasLayoutAbstractLeafVisited(elem, visited),
            .box => |elem| try self.executableTypeHasLayoutAbstractLeafVisited(elem, visited),
            .erased_fn => |erased_fn| blk: {
                if (erased_fn.capture) |capture| {
                    if (try self.executableTypeHasLayoutAbstractLeafVisited(capture, visited)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |tuple| blk: {
                for (tuple) |elem| {
                    if (try self.executableTypeHasLayoutAbstractLeafVisited(elem, visited)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.executableTypeHasLayoutAbstractLeafVisited(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (try self.executableTypeHasLayoutAbstractLeafVisited(arg, visited)) break :blk true;
                    }
                }
                break :blk false;
            },
        };
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

    fn requireConcreteExecutableTypeForBinding(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        bind_ty: TypeVarId,
        comptime context: []const u8,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.sourceTypeIsAbstract(inst.types, bind_ty)) {
            const source_key = inst.types.structuralKeyOwned(bind_ty) catch "<oom>";
            defer if (!std.mem.eql(u8, source_key, "<oom>")) inst.types.allocator.free(source_key);
            debugPanicFmt(
                "lambdamono.lower.{s} expected concrete binding source type before executable lowering: {s}",
                .{ context, source_key },
            );
        }

        return try self.lowerExecutableTypeFromSolvedIn(
            inst.types,
            mono_cache,
            bind_ty,
        );
    }

    fn requireConcreteExecutableType(
        self: *const Lowerer,
        ty: type_mod.TypeId,
        comptime context: []const u8,
    ) type_mod.TypeId {
        if (!self.executableTypeIsAbstract(ty)) return ty;

        const ty_text = self.debugExecutableTypeSummary(ty);
        defer ty_text.deinit(self.allocator);
        debugPanicFmt(
            "lambdamono.lower.{s} expected concrete executable type, found {s}",
            .{ context, ty_text.text },
        );
    }

    fn freshAbstractExecutableType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.addType(.unbd);
    }

    fn currentEnvEntryExecutableType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        entry: EnvEntry,
        comptime context: []const u8,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        _ = self;
        _ = mono_cache;
        _ = solved_types;
        _ = context;
        return entry.exec_ty;
    }

    fn lowerRequestedExecutableArgTypeFromExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        source_ty: TypeVarId,
        repr_mode: specializations.Pending.ReprMode,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const expr = self.input.store.getExpr(expr_id);
        if (expr.data == .var_) {
            if (self.lookupEnvEntry(venv, expr.data.var_)) |entry| {
                const entry_exec_ty = try self.currentEnvEntryExecutableType(
                    inst.types,
                    mono_cache,
                    entry,
                    "lowerRequestedExecutableArgTypeFromExpr(env)",
                );
                if (!self.executableTypeIsAbstract(entry_exec_ty)) {
                    return self.requireConcreteExecutableType(
                        entry_exec_ty,
                        "lowerRequestedExecutableArgTypeFromExpr(env concrete)",
                    );
                }
            }
        }

        const lowered = switch (repr_mode) {
            .natural => try self.lowerExecutableTypeFromSolvedIn(
                inst.types,
                mono_cache,
                source_ty,
            ),
            .erased_boundary => try self.lowerErasedBoundaryExecutableTypeIn(
                inst.types,
                mono_cache,
                source_ty,
            ),
        };
        if (!self.executableTypeIsAbstract(lowered)) return lowered;
        if (self.isExecutableCallableType(lowered)) return lowered;
        debugPanic("lambdamono.lower.lowerRequestedExecutableArgTypeFromExpr abstract executable arg type");
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
            .ret_ty = pending.exec_ret_ty,
        };
    }

    fn validatePendingExecutableSignature(self: *const Lowerer, pending: *const specializations.Pending) void {
        const signature = self.requirePendingExecutableSignature(pending);
        if (signature.capture_ty) |capture_ty| {
            _ = self.requireConcreteExecutableType(
                capture_ty,
                "validatePendingExecutableSignature(capture)",
            );
        }
        for (signature.args_tys) |arg_ty| {
            if (!self.executableTypeIsAbstract(arg_ty)) {
                _ = self.requireConcreteExecutableType(
                    arg_ty,
                    "validatePendingExecutableSignature(arg)",
                );
            }
        }
        if (!self.executableTypeIsAbstract(signature.ret_ty)) {
            _ = self.requireConcreteExecutableType(
                signature.ret_ty,
                "validatePendingExecutableSignature(ret)",
            );
        }
    }

    fn ensurePendingExecutableArgTypes(
        self: *Lowerer,
        pending: *specializations.Pending,
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        const exec_args = pending.exec_args_tys orelse
            debugPanic("lambdamono.lower.ensurePendingExecutableArgTypes missing executable arg signature");
        for (exec_args) |arg_ty| {
            if (self.executableTypeIsAbstract(arg_ty)) {
                debugPanic("lambdamono.lower.ensurePendingExecutableArgTypes abstract executable arg signature");
            }
        }
        return exec_args;
    }

    fn ensurePendingExecutableReturnType(
        self: *Lowerer,
        pending: *specializations.Pending,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return self.requireConcreteExecutableType(
            pending.exec_ret_ty,
            "ensurePendingExecutableReturnType",
        );
    }

    fn executableCallableSigsEqual(
        self: *Lowerer,
        left: type_mod.CallableSig,
        right: type_mod.CallableSig,
    ) bool {
        if (left.args.len != right.args.len) return false;
        for (left.args, right.args) |left_arg, right_arg| {
            if (!self.types.equalIds(left_arg, right_arg)) return false;
        }
        return self.types.equalIds(left.ret, right.ret);
    }

    fn absorbExplicitBodyReturnExecutableType(
        self: *Lowerer,
        pending_symbol: Symbol,
        body_ty: type_mod.TypeId,
    ) void {
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.absorbExplicitBodyReturnExecutableType missing pending specialization");
        const current_ret_ty = pending.exec_ret_ty;
        if (self.types.equalIds(current_ret_ty, body_ty)) return;
        const current = self.types.getTypePreservingNominal(current_ret_ty);
        const body = self.types.getTypePreservingNominal(body_ty);
        if (current == .erased_fn and body == .erased_fn and
            current.erased_fn.capture == null and body.erased_fn.capture != null and
            self.executableCallableSigsEqual(current.erased_fn.call, body.erased_fn.call))
        {
            pending.exec_ret_ty = body_ty;
            return;
        }
    }

    fn preparePendingSummary(
        self: *Lowerer,
        pending: *specializations.Pending,
    ) std.mem.Allocator.Error!void {
        if (pending.summary_types == null) {
            try self.promotePendingSummary(pending);
        }
        if (pending.summary_seeded) return;

        switch (pending.source_def) {
            .hosted_fn => {},
            .fn_ => {
                const summary_types = pending.summary_types orelse
                    debugPanic("lambdamono.lower.preparePendingSummary missing specialization summary store");
                const summary_fn_ty = pending.summary_fn_ty orelse
                    debugPanic("lambdamono.lower.preparePendingSummary missing specialization summary type");
                var inst = InstScope.borrow(self.allocator, summary_types);
                defer inst.deinit();
                var exact_mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
                defer exact_mapping.deinit();
                pending.summary_exact_fn_ty = try self.cloneTypeFromStoreRec(
                    summary_types,
                    &self.input.types,
                    &exact_mapping,
                    pending.fn_ty,
                );
                if (pending.summary_exact_captures) |captures| {
                    if (captures.len != 0) self.allocator.free(captures);
                }
                if (pending.source_captures.len == 0) {
                    pending.summary_exact_captures = &.{};
                } else {
                    const exact_captures = try self.allocator.alloc(solved.Type.Capture, pending.source_captures.len);
                    for (pending.source_captures, 0..) |capture, i| {
                        exact_captures[i] = .{
                            .symbol = capture.symbol,
                            .ty = try self.cloneTypeFromStoreRec(
                                summary_types,
                                &self.input.types,
                                &exact_mapping,
                                capture.ty,
                            ),
                        };
                    }
                    pending.summary_exact_captures = exact_captures;
                }
                const source_summary_fn_ty = pending.summary_exact_fn_ty orelse
                    debugPanic("lambdamono.lower.preparePendingSummary missing cloned source specialization type");
                try self.unifyIn(inst.types, source_summary_fn_ty, summary_fn_ty);
            },
        }

        switch (pending.source_def) {
            .hosted_fn => {},
            .fn_ => try self.refreshPendingExecutableCaptureTypeFromSummary(pending),
        }

        self.validatePendingExecutableSignature(pending);
        pending.summary_seeded = true;
    }

    fn refreshPendingExecutableCaptureTypeFromSummary(
        self: *Lowerer,
        pending: *specializations.Pending,
    ) std.mem.Allocator.Error!void {
        const current_capture_ty = pending.exec_capture_ty orelse return;
        if (!self.executableTypeIsAbstract(current_capture_ty)) return;

        const summary_types = pending.summary_types orelse
            debugPanic("lambdamono.lower.refreshPendingExecutableCaptureTypeFromSummary missing specialization summary store");
        const captures = pending.summary_exact_captures orelse
            debugPanic("lambdamono.lower.refreshPendingExecutableCaptureTypeFromSummary missing exact specialization captures");
        if (captures.len == 0) {
            pending.exec_capture_ty = null;
            return;
        }

        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();
        pending.exec_capture_ty = self.requireConcreteExecutableType(
            try self.lowerCaptureRecordTypeFromSolved(
                summary_types,
                &mono_cache,
                captures,
            ),
            "refreshPendingExecutableCaptureTypeFromSummary",
        );
    }

    fn ensureQueuedCallableSpecializedForRequestedType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        requested_name: Symbol,
        repr_mode: specializations.Pending.ReprMode,
        requested_ty: TypeVarId,
        capture_exec_ty: ?type_mod.TypeId,
        capture_exact_symbols: ?[]const Symbol,
    ) std.mem.Allocator.Error!SpecializedCallableSummary {
        const pending_symbol = try self.queueCallableSpecializationWithExplicitSignature(
            solved_types,
            mono_cache,
            requested_name,
            repr_mode,
            requested_ty,
            capture_exec_ty,
            capture_exact_symbols,
        );
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.ensureQueuedCallableSpecializedForRequestedType missing queued specialization");
        try self.preparePendingSummary(pending);
        return try self.ensureQueuedCallableSpecialized(pending_symbol);
    }

    fn ensureQueuedCallableSpecializedWithExecSignature(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        requested_name: Symbol,
        repr_mode: specializations.Pending.ReprMode,
        requested_ty: TypeVarId,
        capture_exec_ty: ?type_mod.TypeId,
        capture_exact_symbols: ?[]const Symbol,
        exec_arg_tys: []const type_mod.TypeId,
        exec_ret_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedCallableSummary {
        const pending_symbol = try self.queueCallableSpecializationWithResolvedExecSignature(
            solved_types,
            requested_name,
            repr_mode,
            requested_ty,
            capture_exec_ty,
            capture_exact_symbols,
            exec_arg_tys,
            exec_ret_ty,
        );
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.ensureQueuedCallableSpecializedWithExecSignature missing queued specialization");
        try self.preparePendingSummary(pending);
        return try self.ensureQueuedCallableSpecialized(pending_symbol);
    }

    fn ensureExactCallableSpecializedForRequestedType(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        requested_name: Symbol,
        requested_ty: TypeVarId,
        capture_exec_ty: ?type_mod.TypeId,
        capture_exact_symbols: ?[]const Symbol,
    ) std.mem.Allocator.Error!SpecializedCallableSummary {
        return try self.ensureQueuedCallableSpecializedForRequestedType(
            solved_types,
            mono_cache,
            requested_name,
            self.exactCallableReprMode(requested_name),
            requested_ty,
            capture_exec_ty,
            capture_exact_symbols,
        );
    }

    fn ensureQueuedCallableSpecialized(
        self: *Lowerer,
        pending_symbol: Symbol,
    ) std.mem.Allocator.Error!SpecializedCallableSummary {
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized missing queued specialization");
        try self.preparePendingSummary(pending);
        switch (pending.status) {
            .done, .specializing => {
                const exec_args_tys = try self.ensurePendingExecutableArgTypes(pending);
                const exec_ret_ty = try self.ensurePendingExecutableReturnType(pending);
                self.validatePendingExecutableSignature(pending);
                return .{
                    .symbol = pending_symbol,
                    .summary_types = pending.summary_types orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized specialization missing summary store"),
                    .summary_fn_ty = pending.summary_fn_ty orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized specialization missing summary type"),
                    .exec_capture_ty = pending.exec_capture_ty,
                    .exec_args_tys = exec_args_tys,
                    .exec_ret_ty = exec_ret_ty,
                };
            },
            .pending => {
                pending.status = .specializing;
                const specialized = try self.specializeFn(pending_symbol);
                const updated = try self.finishPendingSpecialization(pending_symbol, specialized);
                const exec_args_tys = try self.ensurePendingExecutableArgTypes(updated);
                const exec_ret_ty = try self.ensurePendingExecutableReturnType(updated);
                return .{
                    .symbol = pending_symbol,
                    .summary_types = updated.summary_types orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized completed specialization missing summary store"),
                    .summary_fn_ty = updated.summary_fn_ty orelse
                        debugPanic("lambdamono.lower.ensureQueuedCallableSpecialized completed specialization missing summary type"),
                    .exec_capture_ty = updated.exec_capture_ty,
                    .exec_args_tys = exec_args_tys,
                    .exec_ret_ty = exec_ret_ty,
                };
            },
        }
    }

    fn finishPendingSpecialization(
        self: *Lowerer,
        pending_symbol: Symbol,
        specialized: SpecializedDef,
    ) std.mem.Allocator.Error!*specializations.Pending {
        const updated = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.finishPendingSpecialization lost queued specialization after growth");
        _ = try self.ensurePendingExecutableArgTypes(updated);
        _ = try self.ensurePendingExecutableReturnType(updated);
        self.validatePendingExecutableSignature(updated);
        updated.specialized = specialized.value;
        updated.status = .done;
        return updated;
    }

    fn overwriteSolvedTypeWithExact(
        _: *Lowerer,
        types: *solved.Type.Store,
        target_ty: TypeVarId,
        exact_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        const target_id = types.unlinkPreservingNominal(target_ty);
        const exact_id = types.unlinkPreservingNominal(exact_ty);
        if (target_id == exact_id) return;

        switch (types.getNode(target_id)) {
            .nominal => |nominal| switch (types.getNode(exact_id)) {
                .nominal => types.setNode(target_id, .{ .link = exact_id }),
                else => {
                    const wrapped_exact = try types.fresh(.{ .nominal = .{
                        .module_idx = nominal.module_idx,
                        .ident = nominal.ident,
                        .is_opaque = nominal.is_opaque,
                        .args = nominal.args,
                        .backing = exact_id,
                    } });
                    types.setNode(target_id, .{ .link = wrapped_exact });
                },
            },
            else => types.setNode(target_id, .{ .link = exact_id }),
        }
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
                        null,
                    );
                },
            }
        }
    }

    fn exprContainsReturn(self: *const Lowerer, expr_id: solved.Ast.ExprId) bool {
        const expr = self.input.store.getExpr(expr_id);
        return switch (expr.data) {
            .var_,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .runtime_error,
            => false,
            .tag => |tag| {
                for (self.input.store.sliceExprSpan(tag.args)) |arg_expr| {
                    if (self.exprContainsReturn(arg_expr)) return true;
                }
                return false;
            },
            .record => |fields| {
                for (self.input.store.sliceFieldExprSpan(fields)) |field| {
                    if (self.exprContainsReturn(field.value)) return true;
                }
                return false;
            },
            .access => |access| self.exprContainsReturn(access.record),
            .structural_eq => |eq| self.exprContainsReturn(eq.lhs) or self.exprContainsReturn(eq.rhs),
            .method_eq => |eq| self.exprContainsReturn(eq.lhs) or self.exprContainsReturn(eq.rhs),
            .dispatch_call => |call| {
                if (self.exprContainsReturn(call.receiver)) return true;
                for (self.input.store.sliceExprSpan(call.args)) |arg_expr| {
                    if (self.exprContainsReturn(arg_expr)) return true;
                }
                return false;
            },
            .type_dispatch_call => |call| {
                for (self.input.store.sliceExprSpan(call.args)) |arg_expr| {
                    if (self.exprContainsReturn(arg_expr)) return true;
                }
                return false;
            },
            .let_ => |let_expr| self.exprContainsReturn(let_expr.body) or self.exprContainsReturn(let_expr.rest),
            .call => |call| {
                if (self.exprContainsReturn(call.func)) return true;
                for (self.input.store.sliceExprSpan(call.args)) |arg_expr| {
                    if (self.exprContainsReturn(arg_expr)) return true;
                }
                return false;
            },
            .inspect => |inner| self.exprContainsReturn(inner),
            .low_level => |ll| {
                for (self.input.store.sliceExprSpan(ll.args)) |arg_expr| {
                    if (self.exprContainsReturn(arg_expr)) return true;
                }
                return false;
            },
            .when => |when_expr| {
                if (when_expr.is_try_suffix) return true;
                if (self.exprContainsReturn(when_expr.cond)) return true;
                for (self.input.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.input.store.getBranch(branch_id);
                    if (self.exprContainsReturn(branch.body)) return true;
                }
                return false;
            },
            .if_ => |if_expr| self.exprContainsReturn(if_expr.cond) or
                self.exprContainsReturn(if_expr.then_body) or
                self.exprContainsReturn(if_expr.else_body),
            .block => |block| {
                for (self.input.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    if (self.stmtContainsReturn(self.input.store.getStmt(stmt_id))) return true;
                }
                return self.exprContainsReturn(block.final_expr);
            },
            .tuple => |items| {
                for (self.input.store.sliceExprSpan(items)) |item| {
                    if (self.exprContainsReturn(item)) return true;
                }
                return false;
            },
            .tag_payload => |payload| self.exprContainsReturn(payload.tag_union),
            .tuple_access => |access| self.exprContainsReturn(access.tuple),
            .list => |items| {
                for (self.input.store.sliceExprSpan(items)) |item| {
                    if (self.exprContainsReturn(item)) return true;
                }
                return false;
            },
            .return_ => true,
            .for_ => |for_expr| self.exprContainsReturn(for_expr.iterable) or self.exprContainsReturn(for_expr.body),
        };
    }

    fn stmtContainsReturn(self: *const Lowerer, stmt: solved.Ast.Stmt) bool {
        return switch (stmt) {
            .decl => |decl| self.exprContainsReturn(decl.body),
            .var_decl => |decl| self.exprContainsReturn(decl.body),
            .reassign => |reassign| self.exprContainsReturn(reassign.body),
            .expr => |expr_id| self.exprContainsReturn(expr_id),
            .debug => |expr_id| self.exprContainsReturn(expr_id),
            .expect => |expr_id| self.exprContainsReturn(expr_id),
            .crash => false,
            .return_ => true,
            .break_ => false,
            .for_ => |for_stmt| self.exprContainsReturn(for_stmt.iterable) or self.exprContainsReturn(for_stmt.body),
            .while_ => |while_stmt| self.exprContainsReturn(while_stmt.cond) or self.exprContainsReturn(while_stmt.body),
        };
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

    fn explicitExprSourceType(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!TypeVarId {
        const expr = self.input.store.getExpr(expr_id);
        return switch (expr.data) {
            .var_ => |symbol| if (self.lookupEnvEntry(venv, symbol)) |entry|
                entry.ty
            else
                try self.cloneInstType(inst, expr.ty),
            .access => |access| {
                const record_ty = try self.explicitExprSourceType(inst, venv, access.record);
                return self.solvedRecordFieldByName(inst.types, record_ty, access.field) orelse
                    debugPanic("lambdamono.lower.explicitExprSourceType missing solved record field");
            },
            .tuple_access => |access| {
                const tuple_ty = try self.explicitExprSourceType(inst, venv, access.tuple);
                return self.solvedTupleElemType(inst.types, tuple_ty, access.elem_index) orelse
                    debugPanic("lambdamono.lower.explicitExprSourceType missing solved tuple element");
            },
            .tag_payload => |tag_payload| {
                const union_ty = try self.explicitExprSourceType(inst, venv, tag_payload.tag_union);
                return self.solvedTagPayloadTypeByName(
                    inst.types,
                    union_ty,
                    tag_payload.tag_name,
                    tag_payload.payload_index,
                ) orelse self.solvedTagPayloadType(
                    inst.types,
                    union_ty,
                    tag_payload.tag_discriminant,
                    tag_payload.payload_index,
                ) orelse debugPanic("lambdamono.lower.explicitExprSourceType missing solved tag payload");
            },
            .int_lit, .dec_lit => try self.cloneInstType(inst, expr.ty),
            .list, .record, .tag => {
                const current_ty = try self.cloneInstType(inst, expr.ty);
                return try self.refinedSourceTypeForExpr(inst, venv, expr_id, current_ty);
            },
            else => try self.cloneInstType(inst, expr.ty),
        };
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
                const specialized = try self.ensureQueuedCallableSpecializedForRequestedType(
                    &self.input.types,
                    &mono_cache,
                    entry_symbol,
                    .natural,
                    entry_fn_ty,
                    null,
                    null,
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
                    specialized.exec_capture_ty,
                    null,
                    specialized.exec_ret_ty,
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
        func_source_ty: TypeVarId,
        call_relation_ty: TypeVarId,
        func_exec_ty: type_mod.TypeId,
        arg_source_tys: []const TypeVarId,
        arg_exprs: []const ast.ExprId,
        expected_result_exec_ty: type_mod.TypeId,
        direct_func_symbol: ?Symbol,
    ) std.mem.Allocator.Error!ast.ExprId {
        const solved_types = inst.types;
        self.assertCallableExprMatchesSolvedType(solved_types, func_expr, func_source_ty);
        const fn_shape = solved_types.fnShape(call_relation_ty);
        const param_source_tys = solved_types.sliceTypeVarSpan(fn_shape.args);
        if (param_source_tys.len != arg_exprs.len or arg_source_tys.len != arg_exprs.len) {
            debugPanic("lambdamono.lower.applyCallableValueCall call arg arity mismatch");
        }
        const exec_call_sig = self.requireExecutableCallableSig(
            func_exec_ty,
            "applyCallableValueCall",
        );
        const concrete_result_exec_ty = if (!self.executableTypeIsAbstract(expected_result_exec_ty))
            self.requireConcreteExecutableType(
                expected_result_exec_ty,
                "applyCallableValueCall",
            )
        else
            exec_call_sig.ret;
        if (exec_call_sig.args.len != arg_exprs.len) {
            debugPanic("lambdamono.lower.applyCallableValueCall callable executable arg arity mismatch");
        }
        return switch (self.types.getType(func_exec_ty)) {
            .erased_fn => blk: {
                const func_capture_ty = self.erasedFnCaptureType(func_exec_ty);
                switch (solved_types.lambdaRepr(func_source_ty)) {
                    .lset => |lambda_members| {
                        for (lambda_members) |lambda_member| {
                            const specialized = try self.ensureQueuedCallableSpecializedForRequestedType(
                                solved_types,
                                mono_cache,
                                lambda_member.symbol,
                                .erased_boundary,
                                call_relation_ty,
                                func_capture_ty,
                                null,
                            );
                            if (specialized.exec_capture_ty != func_capture_ty) {
                                debugPanicFmt(
                                    "lambdamono.lower.applyCallableValueCall erased capture mismatch symbol={d} func_capture_ty={?d} specialized_capture_ty={?d}",
                                    .{
                                        lambda_member.symbol.raw(),
                                        if (func_capture_ty) |ty| @intFromEnum(ty) else null,
                                        if (specialized.exec_capture_ty) |ty| @intFromEnum(ty) else null,
                                    },
                                );
                            }
                            if (specialized.exec_args_tys.len != exec_call_sig.args.len) {
                                debugPanic("lambdamono.lower.applyCallableValueCall erased fn arg arity mismatch against executable callable signature");
                            }
                            for (specialized.exec_args_tys, exec_call_sig.args) |actual_arg_ty, expected_arg_ty| {
                                if (!self.types.equalIds(actual_arg_ty, expected_arg_ty)) {
                                    debugPanic("lambdamono.lower.applyCallableValueCall erased fn arg executable type mismatch against callable executable signature");
                                }
                            }
                            if (!self.types.equalIds(specialized.exec_ret_ty, exec_call_sig.ret)) {
                                debugPanic("lambdamono.lower.applyCallableValueCall erased fn return executable type mismatch against callable executable signature");
                            }
                        }
                    },
                    .erased => {},
                }

                const lowered_args = try self.bridgeCallArgsToExpectedExecutableTypes(
                    inst,
                    mono_cache,
                    arg_source_tys,
                    arg_exprs,
                    exec_call_sig.args,
                );
                defer if (lowered_args.len != 0) self.allocator.free(lowered_args);
                const call_ret_ty = exec_call_sig.ret;

                var call_expr = try self.output.addExpr(.{
                    .ty = call_ret_ty,
                    .data = .{ .call_erased = .{
                        .func = func_expr,
                        .args = try self.output.addExprSpan(lowered_args),
                        .capture_ty = self.erasedFnCaptureType(func_exec_ty),
                    } },
                });
                if (!self.types.equalIds(call_ret_ty, concrete_result_exec_ty)) {
                    call_expr = try self.emitExplicitBridgeExpr(call_expr, concrete_result_exec_ty);
                }
                break :blk call_expr;
            },
            .tag_union => |tag_union| blk: {
                if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    debugPanic("lambdamono.lower.applyCallableValueCall expected callable executable type");
                }

                const lambda_members = try self.collectExecutableLambdaMembers(solved_types, func_source_ty, func_exec_ty);
                defer self.allocator.free(lambda_members);
                const branches = try self.allocator.alloc(ast.Branch, lambda_members.len);
                defer self.allocator.free(branches);
                const branch_call_args = try self.bridgeCallArgsToExpectedExecutableTypes(
                    inst,
                    mono_cache,
                    arg_source_tys,
                    arg_exprs,
                    exec_call_sig.args,
                );
                defer if (branch_call_args.len != 0) self.allocator.free(branch_call_args);

                for (lambda_members, 0..) |lambda_member, i| {
                    const specialization_symbol = if (lambda_members.len == 1)
                        direct_func_symbol orelse lambda_member.symbol
                    else
                        lambda_member.symbol;
                    const current_member = solved_types.requireLambdaMember(func_source_ty, specialization_symbol);
                    if (lambda_member.capture_ty == null and current_member.captures.len != 0) {
                        debugPanic("lambdamono.lower.applyCallableValueCall missing executable capture payload for lambda member");
                    }
                    const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
                        solved_types,
                        specialization_symbol,
                        .natural,
                        call_relation_ty,
                        lambda_member.capture_ty,
                        null,
                        exec_call_sig.args,
                        exec_call_sig.ret,
                    );
                    if (specialized.exec_args_tys.len != exec_call_sig.args.len) {
                        debugPanic("lambdamono.lower.applyCallableValueCall branch arg arity mismatch against callable executable signature");
                    }
                    for (specialized.exec_args_tys, exec_call_sig.args) |actual_arg_ty, expected_arg_ty| {
                        if (!self.types.equalIds(actual_arg_ty, expected_arg_ty)) {
                            debugPanic("lambdamono.lower.applyCallableValueCall branch arg executable type mismatch against callable executable signature");
                        }
                    }
                    if (!self.types.equalIds(specialized.exec_ret_ty, exec_call_sig.ret)) {
                        debugPanic("lambdamono.lower.applyCallableValueCall branch return executable type mismatch against callable executable signature");
                    }
                    if (lambda_member.capture_ty == null) {
                        var body_expr = try self.output.addExpr(.{
                            .ty = specialized.exec_ret_ty,
                            .data = .{ .call = .{
                                .proc = specialized.symbol,
                                .args = try self.output.addExprSpan(branch_call_args),
                            } },
                        });
                        if (!self.types.equalIds(specialized.exec_ret_ty, concrete_result_exec_ty)) {
                            body_expr = try self.emitExplicitBridgeExpr(body_expr, concrete_result_exec_ty);
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
                        const call_args_with_capture = try self.allocator.alloc(ast.ExprId, branch_call_args.len + 1);
                        defer self.allocator.free(call_args_with_capture);
                        @memcpy(call_args_with_capture[0..branch_call_args.len], branch_call_args);
                        call_args_with_capture[branch_call_args.len] = try self.output.addExpr(.{
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
                        if (!self.types.equalIds(specialized.exec_ret_ty, concrete_result_exec_ty)) {
                            body_expr = try self.emitExplicitBridgeExpr(body_expr, concrete_result_exec_ty);
                        }
                        branches[i] = .{
                            .pat = pat,
                            .body = body_expr,
                        };
                    }
                }

                break :blk try self.output.addExpr(.{
                    .ty = concrete_result_exec_ty,
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
        return try self.internExecutableType(try lower_type.lowerErasedBoundaryType(
            solved_types,
            &self.types,
            mono_cache,
            ty,
            &self.input.symbols,
        ));
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

    fn instantiateConstraintFnTypeForExprs(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        arg_expr_ids: []const solved.Ast.ExprId,
        source_constraint_ty: TypeVarId,
        expected_result_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        const constraint_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            inst,
            &self.input.types,
            &mapping,
            source_constraint_ty,
        );
        const constraint_shape = inst.types.fnShape(constraint_fn_ty);
        const constraint_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(constraint_shape.args),
        );
        defer if (constraint_arg_tys.len != 0) self.allocator.free(constraint_arg_tys);
        if (constraint_arg_tys.len != arg_expr_ids.len) {
            debugPanic("lambdamono.lower.instantiateConstraintFnTypeForExprs arg arity mismatch");
        }
        try self.unifyIn(inst.types, constraint_shape.ret, expected_result_source_ty);
        for (arg_expr_ids, constraint_arg_tys) |arg_expr_id, constraint_arg_ty| {
            const arg_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
            try self.unifyIn(inst.types, constraint_arg_ty, arg_source_ty);
            const refined_arg_ty = try self.refinedSourceTypeForExpr(
                inst,
                venv,
                arg_expr_id,
                constraint_arg_ty,
            );
            try self.unifyIn(inst.types, constraint_arg_ty, refined_arg_ty);
        }
        try self.refineCallArgSourceTypesIn(inst, venv, arg_expr_ids, constraint_arg_tys);
        return constraint_fn_ty;
    }

    fn instantiateExplicitConstraintFnTypeForExprs(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        arg_expr_ids: []const solved.Ast.ExprId,
        source_constraint_ty: TypeVarId,
        expected_result_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        const constraint_fn_ty = try self.cloneTypeIntoInstFromStoreWithMapping(
            inst,
            &self.input.types,
            &mapping,
            source_constraint_ty,
        );
        const constraint_shape = inst.types.fnShape(constraint_fn_ty);
        const constraint_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(constraint_shape.args),
        );
        defer if (constraint_arg_tys.len != 0) self.allocator.free(constraint_arg_tys);
        if (constraint_arg_tys.len != arg_expr_ids.len) {
            debugPanic("lambdamono.lower.instantiateExplicitConstraintFnTypeForExprs arg arity mismatch");
        }
        try self.unifyIn(inst.types, constraint_shape.ret, expected_result_source_ty);
        for (arg_expr_ids, constraint_arg_tys) |arg_expr_id, constraint_arg_ty| {
            const arg_source_ty = try self.explicitExprSourceType(inst, venv, arg_expr_id);
            try self.unifyIn(inst.types, constraint_arg_ty, arg_source_ty);
        }
        return constraint_fn_ty;
    }

    fn lowerSolvedNominalArgs(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        args_span: solved.Type.Span(TypeVarId),
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        _ = self;
        _ = solved_types;
        _ = mono_cache;
        _ = args_span;
        return &.{};
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
                .func => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, id),
                .lambda_set => debugPanic("lambdamono.lower.lowerBoxBoundaryCallableType bare lambda_set survived to callable boundary lowering"),
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
                debugPanic("lambdamono.lower.eraseBoundaryExecutableType missing explicit erased callable type")
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
                    .erased => try self.lowerErasedBoundaryExecutableTypeIn(solved_types, mono_cache, solved_ty),
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

    fn recordTypeFromFieldValues(
        self: *Lowerer,
        fields: []const ast.FieldExpr,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const record_fields = try self.allocator.alloc(type_mod.Field, fields.len);
        defer self.allocator.free(record_fields);
        for (fields, 0..) |field, i| {
            record_fields[i] = .{
                .name = field.name,
                .ty = self.output.getExpr(field.value).ty,
            };
        }
        return try self.internExecutableType(try self.types.internResolved(.{
            .record = .{ .fields = try self.types.dupeFields(record_fields) },
        }));
    }

    fn executableCallableSigEqual(
        self: *Lowerer,
        left: type_mod.CallableSig,
        right: type_mod.CallableSig,
    ) bool {
        if (left.args.len != right.args.len) return false;
        for (left.args, right.args) |left_arg, right_arg| {
            if (!self.types.equalIds(left_arg, right_arg)) return false;
        }
        return self.types.equalIds(left.ret, right.ret);
    }

    fn executableTagArgsEqual(
        self: *Lowerer,
        left: []const type_mod.TypeId,
        right: []const type_mod.TypeId,
    ) bool {
        if (left.len != right.len) return false;
        for (left, right) |left_arg, right_arg| {
            if (!self.types.equalIds(left_arg, right_arg)) return false;
        }
        return true;
    }

    fn naturalExecutableCallableTypesMatchIgnoringCallSig(
        self: *Lowerer,
        left_ty: type_mod.TypeId,
        right_ty: type_mod.TypeId,
    ) bool {
        const left_union = switch (self.types.getTypePreservingNominal(left_ty)) {
            .nominal => |nominal| return self.naturalExecutableCallableTypesMatchIgnoringCallSig(nominal.backing, right_ty),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                tag_union
            else
                return false,
            else => return false,
        };
        const right_union = switch (self.types.getTypePreservingNominal(right_ty)) {
            .nominal => |nominal| return self.naturalExecutableCallableTypesMatchIgnoringCallSig(left_ty, nominal.backing),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                tag_union
            else
                return false,
            else => return false,
        };
        if (left_union.tags.len != right_union.tags.len) return false;
        for (left_union.tags, right_union.tags) |left_tag, right_tag| {
            if (!std.meta.eql(left_tag.name, right_tag.name)) return false;
            if (!self.executableTagArgsEqual(left_tag.args, right_tag.args)) return false;
        }
        return true;
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

    fn solvedTagPayloadTypeByName(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        union_ty: TypeVarId,
        tag_name: base.Ident.Idx,
        payload_index: u16,
    ) ?TypeVarId {
        const tag_info = self.solvedTagInfoByName(solved_types, union_ty, tag_name) orelse
            return null;
        if (payload_index >= tag_info.args.len) return null;
        return tag_info.args[payload_index];
    }

    fn solvedTagNameByDiscriminant(
        self: *Lowerer,
        solved_types: *const solved.Type.Store,
        union_ty: TypeVarId,
        tag_discriminant: u16,
    ) ?base.Ident.Idx {
        const id = solved_types.unlinkConst(union_ty);
        return switch (solved_types.getNode(id)) {
            .nominal => |nominal| self.solvedTagNameByDiscriminant(
                solved_types,
                nominal.backing,
                tag_discriminant,
            ),
            .content => |content| switch (content) {
                .tag_union => |tag_union| blk: {
                    const tags = solved_types.sliceTags(tag_union.tags);
                    if (tag_discriminant >= tags.len) break :blk null;
                    break :blk tags[tag_discriminant].name;
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

    fn executableTagInfoByDiscriminant(
        self: *Lowerer,
        tag_ty: type_mod.TypeId,
        tag_discriminant: u16,
    ) ?ExecutableTagInfo {
        return switch (self.types.getTypePreservingNominal(tag_ty)) {
            .nominal => |nominal| self.executableTagInfoByDiscriminant(
                nominal.backing,
                tag_discriminant,
            ),
            .tag_union => |tag_union| blk: {
                if (tag_discriminant >= tag_union.tags.len) break :blk null;
                break :blk .{
                    .discriminant = tag_discriminant,
                    .args = tag_union.tags[tag_discriminant].args,
                };
            },
            else => null,
        };
    }

    fn singletonExecutableCtorTagInfo(
        self: *Lowerer,
        tag_ty: type_mod.TypeId,
    ) ?ExecutableTagInfo {
        return switch (self.types.getTypePreservingNominal(tag_ty)) {
            .nominal => |nominal| self.singletonExecutableCtorTagInfo(nominal.backing),
            .tag_union => |tag_union| blk: {
                if (tag_union.tags.len != 1) break :blk null;
                switch (tag_union.tags[0].name) {
                    .ctor => break :blk .{
                        .discriminant = 0,
                        .args = tag_union.tags[0].args,
                    },
                    .lambda => break :blk null,
                }
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

    fn executableCallableSig(self: *const Lowerer, ty: type_mod.TypeId) ?type_mod.CallableSig {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |nominal| self.executableCallableSig(nominal.backing),
            .erased_fn => |erased_fn| erased_fn.call,
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                tag_union.call
            else
                null,
            else => null,
        };
    }

    fn requireExecutableCallableSig(
        self: *const Lowerer,
        ty: type_mod.TypeId,
        comptime context: []const u8,
    ) type_mod.CallableSig {
        return self.executableCallableSig(ty) orelse
            debugPanicFmt(
                "lambdamono.lower.{s} expected callable executable signature on type {d}",
                .{ context, @intFromEnum(ty) },
            );
    }

    fn isNaturalExecutableCallableType(self: *const Lowerer, ty: type_mod.TypeId) bool {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |nominal| self.isNaturalExecutableCallableType(nominal.backing),
            .tag_union => |tag_union| self.tagUnionIsInternalLambdaSet(tag_union.tags),
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
            .erased_fn => |erased_fn| erased_fn.capture,
            else => null,
        };
    }

    fn ensureTopLevelValueLowered(self: *Lowerer, symbol: Symbol) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.top_level_value_types.get(symbol)) |existing| return existing;

        const source_symbol = self.rootSourceSymbol(symbol);
        const source = self.top_level_values.get(source_symbol) orelse
            debugPanic("lambdamono.lower.ensureTopLevelValueLowered missing top-level value");

        const expr_id: solved.Ast.ExprId = switch (source) {
            .fn_ => debugPanic("lambdamono.lower.ensureTopLevelValueLowered expected value source, found function"),
            .hosted_fn => debugPanic("lambdamono.lower.ensureTopLevelValueLowered expected value source, found hosted function"),
            .val => |value_expr| value_expr,
            .run => |run_def| run_def.body,
        };

        const specialized = try self.specializeStandaloneValue(.{ .symbol = source_symbol, .ty = self.input.store.getExpr(expr_id).ty }, expr_id);
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

    fn rootSourceSymbol(self: *const Lowerer, symbol: Symbol) Symbol {
        var current = symbol;
        while (true) {
            current = switch (self.input.symbols.get(current).origin) {
                .specialized_top_level_def => |data| @enumFromInt(data.source_symbol),
                .specialized_local_fn => |data| @enumFromInt(data.source_symbol),
                .lifted_local_fn => |data| @enumFromInt(data.source_symbol),
                .lifted_local_fn_alias => |data| @enumFromInt(data.source_symbol),
                .top_level_def,
                .local_pattern,
                .lifted_lambda,
                .synthetic,
                => return current,
            };
        }
    }

    const SpecializedStandaloneValue = struct {
        symbol: Symbol,
        expr: ast.ExprId,
    };

    const SpecializedDef = struct {
        value: ast.DefVal,
    };

    const LoweredBinding = struct {
        source_ty: TypeVarId,
        exec_ty: type_mod.TypeId,
        body: ast.ExprId,
    };

    const ImportedCallableSummary = struct {
        fn_ty: TypeVarId,
        fn_shape: solved.Type.FnShape,
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
        try self.preRefineExprSourceTypes(&inst, &mono_cache, &.{}, expr_id);
        const lowered = try self.lowerBindingBody(
            &inst,
            &mono_cache,
            &.{},
            bind,
            expr_id,
            null,
            "specializeStandaloneValue",
        );
        return .{
            .symbol = bind.symbol,
            .expr = lowered.body,
        };
    }

    fn lowerBindingBody(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        bind: solved.Ast.TypedSymbol,
        body_expr_id: solved.Ast.ExprId,
        bind_ty_override: ?TypeVarId,
        comptime context: []const u8,
    ) std.mem.Allocator.Error!LoweredBinding {
        const bind_ty = if (bind_ty_override) |ty|
            ty
        else blk: {
            const cloned = try self.freshCloneTypeIntoInstFromStore(inst, &self.input.types, bind.ty);
            if (!self.sourceTypeIsAbstract(&self.input.types, bind.ty) and
                self.sourceTypeIsAbstract(inst.types, cloned))
            {
                const bind_name = self.input.idents.getText(self.input.symbols.get(bind.symbol).name);
                const source_key = self.input.types.structuralKeyOwned(bind.ty) catch "<oom>";
                defer if (!std.mem.eql(u8, source_key, "<oom>")) self.input.types.allocator.free(source_key);
                const cloned_key = inst.types.structuralKeyOwned(cloned) catch "<oom>";
                defer if (!std.mem.eql(u8, cloned_key, "<oom>")) inst.types.allocator.free(cloned_key);
                std.debug.panic(
                    "lambdamono.lower.{s} concrete source bind cloned abstract bind={s} source={s} cloned={s}",
                    .{ context, bind_name, source_key, cloned_key },
                );
            }
            break :blk cloned;
        };

        const body_expr = self.input.store.getExpr(body_expr_id);
        if (!self.sourceTypeIsAbstract(inst.types, bind_ty)) {
            const bind_exec_ty = try self.requireConcreteExecutableTypeForBinding(
                inst,
                mono_cache,
                bind_ty,
                context,
            );
            const lowered_body = try self.specializeExprWithDefaultTyAndSourceTy(
                inst,
                mono_cache,
                venv,
                body_expr_id,
                body_expr,
                bind_ty,
                bind_exec_ty,
            );
            const lowered_exec_ty = self.requireConcreteExecutableType(
                self.output.getExpr(lowered_body.expr).ty,
                context,
            );
            return .{
                .source_ty = bind_ty,
                .exec_ty = lowered_exec_ty,
                .body = lowered_body.expr,
            };
        }

        const bind_exec_ty = try self.lowerExecutableTypeFromSolvedIn(
            inst.types,
            mono_cache,
            bind_ty,
        );
        const lowered_body = try self.specializeExprWithDefaultTyAndSourceTy(
            inst,
            mono_cache,
            venv,
            body_expr_id,
            body_expr,
            bind_ty,
            bind_exec_ty,
        );
        const lowered_exec_ty = self.output.getExpr(lowered_body.expr).ty;
        if (self.executableTypeHasLayoutAbstractLeaf(lowered_exec_ty)) {
            const bind_entry = self.input.symbols.get(bind.symbol);
            const debug_bind_name = if (bind_entry.name == base.Ident.Idx.NONE)
                "<none>"
            else
                self.input.idents.getText(bind_entry.name);
            const bind_source_summary = self.debugSolvedTypeSummary(inst.types, bind_ty);
            defer bind_source_summary.deinit(self.allocator);
            const body_source_summary = self.debugSolvedTypeSummary(inst.types, lowered_body.source_ty);
            defer body_source_summary.deinit(self.allocator);
            const body_exec_summary = self.debugExecutableTypeSummary(lowered_exec_ty);
            defer body_exec_summary.deinit(self.allocator);
            const body_expr_debug = self.input.store.getExpr(body_expr_id);
            const body_op = if (body_expr_debug.data == .low_level)
                @tagName(body_expr_debug.data.low_level.op)
            else
                "<none>";
            const body_arg0_summary = if (body_expr_debug.data == .low_level and
                self.input.store.sliceExprSpan(body_expr_debug.data.low_level.args).len != 0)
                blk_arg0: {
                    const arg0_ty = self.instantiatedSourceTypeForExpr(
                        inst,
                        venv,
                        self.input.store.sliceExprSpan(body_expr_debug.data.low_level.args)[0],
                    ) catch break :blk_arg0 null;
                    break :blk_arg0 self.debugSolvedTypeSummary(inst.types, arg0_ty);
                }
            else
                null;
            defer if (body_arg0_summary) |summary| summary.deinit(self.allocator);
            debugPanicFmt(
                "lambdamono.lower.{s} abstract executable binding bind={s} body_tag={s} body_op={s} body_arg0={s} source={s} body_source={s} body_exec={s}",
                .{
                    context,
                    debug_bind_name,
                    @tagName(body_expr_debug.data),
                    body_op,
                    if (body_arg0_summary) |summary| summary.text else "<none>",
                    bind_source_summary.text,
                    body_source_summary.text,
                    body_exec_summary.text,
                },
            );
        }
        return .{
            .source_ty = bind_ty,
            .exec_ty = lowered_exec_ty,
            .body = lowered_body.expr,
        };
    }

    fn importCallableSummaryIntoInst(
        self: *Lowerer,
        inst: *InstScope,
        current_relation_ty: TypeVarId,
        summary_types: *const solved.Type.Store,
        summary_fn_ty: TypeVarId,
    ) std.mem.Allocator.Error!ImportedCallableSummary {
        const cloned_fn_ty = try self.cloneTypeIntoInstFromStore(
            inst,
            summary_types,
            summary_fn_ty,
        );
        try self.unifyIn(inst.types, cloned_fn_ty, current_relation_ty);
        return .{
            .fn_ty = cloned_fn_ty,
            .fn_shape = inst.types.fnShape(cloned_fn_ty),
        };
    }

    fn preRefineBindingInEnv(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        bind: solved.Ast.TypedSymbol,
        body_expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!EnvEntry {
        try self.preRefineExprSourceTypes(inst, mono_cache, venv, body_expr_id);
        const bind_ty = try self.freshCloneTypeIntoInstFromStore(inst, &self.input.types, bind.ty);
        const body_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, body_expr_id);
        try self.unifyIn(inst.types, bind_ty, body_source_ty);
        const refined_body_ty = try self.refinedSourceTypeForExpr(
            inst,
            venv,
            body_expr_id,
            bind_ty,
        );
        try self.unifyIn(inst.types, bind_ty, refined_body_ty);
        return .{
            .symbol = bind.symbol,
            .ty = bind_ty,
            .exec_ty = try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, bind_ty),
            .exact_fn_symbol = self.exactCallableSymbolForBoundExpr(bind.symbol, body_expr_id),
        };
    }

    fn preRefineStmtSourceTypes(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        stmt_id: solved.Ast.StmtId,
    ) std.mem.Allocator.Error![]EnvEntry {
        const stmt = self.input.store.getStmt(stmt_id);
        return switch (stmt) {
            .decl => |decl| {
                const entry = try self.preRefineBindingInEnv(inst, mono_cache, venv, decl.bind, decl.body);
                return try self.extendEnv(venv, entry);
            },
            .var_decl => |decl| {
                const entry = try self.preRefineBindingInEnv(inst, mono_cache, venv, decl.bind, decl.body);
                return try self.extendEnv(venv, entry);
            },
            .reassign => |reassign| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, reassign.body);
                if (self.lookupEnvEntry(venv, reassign.target)) |target_entry| {
                    const body_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, reassign.body);
                    try self.unifyIn(inst.types, target_entry.ty, body_source_ty);
                }
                return try self.cloneEnv(venv);
            },
            .expr => |expr_id| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, expr_id);
                return try self.cloneEnv(venv);
            },
            .debug => |expr_id| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, expr_id);
                return try self.cloneEnv(venv);
            },
            .expect => |expr_id| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, expr_id);
                return try self.cloneEnv(venv);
            },
            .return_ => |expr_id| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, expr_id);
                return try self.cloneEnv(venv);
            },
            .for_ => |for_stmt| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, for_stmt.iterable);
                const source_pat = self.input.store.getPat(for_stmt.patt);
                const pat_ty = try self.cloneInstType(inst, source_pat.ty);
                const iterable_ty = try self.instantiatedSourceTypeForExpr(inst, venv, for_stmt.iterable);
                const elem_ty = self.solvedListElemType(inst.types, iterable_ty) orelse
                    debugPanic("lambdamono.lower.preRefineStmtSourceTypes expected list iterable source type");
                try self.unifyIn(inst.types, pat_ty, elem_ty);
                const additions = try self.collectPatBindingsAtSourceTy(inst, mono_cache, source_pat, pat_ty);
                defer self.allocator.free(additions);
                const body_env = try self.concatEnv(venv, additions);
                defer self.allocator.free(body_env);
                try self.preRefineExprSourceTypes(inst, mono_cache, body_env, for_stmt.body);
                return try self.cloneEnv(venv);
            },
            .while_ => |while_stmt| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, while_stmt.cond);
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, while_stmt.body);
                return try self.cloneEnv(venv);
            },
            .crash, .break_ => try self.cloneEnv(venv),
        };
    }

    fn preRefineBlockSourceTypes(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        incoming_env: []const EnvEntry,
        block: @FieldType(solved.Ast.Expr.Data, "block"),
    ) std.mem.Allocator.Error!void {
        var env = try self.cloneEnv(incoming_env);
        defer self.allocator.free(env);

        const stmts = self.input.store.sliceStmtSpan(block.stmts);
        for (stmts) |stmt_id| {
            const next_env = try self.preRefineStmtSourceTypes(inst, mono_cache, env, stmt_id);
            self.allocator.free(env);
            env = next_env;
        }

        try self.preRefineExprSourceTypes(inst, mono_cache, env, block.final_expr);
    }

    fn preRefineExprSourceTypes(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        const expr = self.input.store.getExpr(expr_id);
        switch (expr.data) {
            .var_, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bool_lit, .unit => {},
            .inspect => |value_expr_id| try self.preRefineExprSourceTypes(inst, mono_cache, venv, value_expr_id),
            .tag => |tag| {
                for (self.input.store.sliceExprSpan(tag.args)) |arg_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, arg_expr_id);
                }
            },
            .record => |fields| {
                for (self.input.store.sliceFieldExprSpan(fields)) |field| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, field.value);
                }
            },
            .access => |access| try self.preRefineExprSourceTypes(inst, mono_cache, venv, access.record),
            .tuple_access => |access| try self.preRefineExprSourceTypes(inst, mono_cache, venv, access.tuple),
            .tag_payload => |tag_payload| try self.preRefineExprSourceTypes(inst, mono_cache, venv, tag_payload.tag_union),
            .tuple => |elems| {
                for (self.input.store.sliceExprSpan(elems)) |elem_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, elem_expr_id);
                }
            },
            .list => |items| {
                for (self.input.store.sliceExprSpan(items)) |item_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, item_expr_id);
                }
            },
            .return_ => |ret_expr_id| try self.preRefineExprSourceTypes(inst, mono_cache, venv, ret_expr_id),
            .runtime_error => {},
            .for_ => |for_expr| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, for_expr.iterable);
                const source_pat = self.input.store.getPat(for_expr.patt);
                const pat_ty = try self.cloneInstType(inst, source_pat.ty);
                const iterable_ty = try self.instantiatedSourceTypeForExpr(inst, venv, for_expr.iterable);
                const elem_ty = self.solvedListElemType(inst.types, iterable_ty) orelse
                    debugPanic("lambdamono.lower.preRefineExprSourceTypes expected list iterable source type");
                try self.unifyIn(inst.types, pat_ty, elem_ty);
                const additions = try self.collectPatBindingsAtSourceTy(inst, mono_cache, source_pat, pat_ty);
                defer self.allocator.free(additions);
                const body_env = try self.concatEnv(venv, additions);
                defer self.allocator.free(body_env);
                try self.preRefineExprSourceTypes(inst, mono_cache, body_env, for_expr.body);
            },
            .let_ => |let_expr| {
                const entry = try self.preRefineBindingInEnv(inst, mono_cache, venv, let_expr.bind, let_expr.body);
                const rest_env = try self.extendEnv(venv, entry);
                defer self.allocator.free(rest_env);
                try self.preRefineExprSourceTypes(inst, mono_cache, rest_env, let_expr.rest);
            },
            .call => |call| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, call.func);
                const arg_expr_ids = self.input.store.sliceExprSpan(call.args);
                for (arg_expr_ids) |arg_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, arg_expr_id);
                }
                const current_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                const call_relation_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    arg_expr_ids,
                    call.call_constraint_ty,
                    current_result_ty,
                );
                const current_func_ty = try self.instantiatedSourceTypeForExpr(inst, venv, call.func);
                const refined_func_ty = try self.refinedSourceTypeForExpr(inst, venv, call.func, current_func_ty);
                try self.unifyIn(inst.types, current_func_ty, refined_func_ty);
                try self.unifyIn(inst.types, refined_func_ty, call_relation_ty);
            },
            .structural_eq => |eq| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, eq.lhs);
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, eq.rhs);
            },
            .method_eq => |eq| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, eq.lhs);
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, eq.rhs);
                const current_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                const dispatch_arg_exprs = [_]solved.Ast.ExprId{ eq.lhs, eq.rhs };
                _ = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    &dispatch_arg_exprs,
                    eq.dispatch_constraint_ty,
                    current_result_ty,
                );
            },
            .dispatch_call => |method_call| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, method_call.receiver);
                for (self.input.store.sliceExprSpan(method_call.args)) |arg_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, arg_expr_id);
                }
            },
            .type_dispatch_call => |method_call| {
                for (self.input.store.sliceExprSpan(method_call.args)) |arg_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, arg_expr_id);
                }
            },
            .low_level => |ll| {
                for (self.input.store.sliceExprSpan(ll.args)) |arg_expr_id| {
                    try self.preRefineExprSourceTypes(inst, mono_cache, venv, arg_expr_id);
                }
            },
            .when => |when_expr| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, when_expr.cond);
                const cond_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, when_expr.cond);
                const result_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
                for (self.input.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.input.store.getBranch(branch_id);
                    const branch_pat = self.input.store.getPat(branch.pat);
                    const additions = try self.collectPatBindingsAtSourceTy(inst, mono_cache, branch_pat, cond_source_ty);
                    defer self.allocator.free(additions);
                    const branch_env = try self.concatEnv(venv, additions);
                    defer self.allocator.free(branch_env);
                    try self.preRefineExprSourceTypes(inst, mono_cache, branch_env, branch.body);
                    const refined_branch_ty = try self.refinedSourceTypeForExpr(
                        inst,
                        branch_env,
                        branch.body,
                        result_source_ty,
                    );
                    try self.unifyIn(inst.types, result_source_ty, refined_branch_ty);
                }
            },
            .if_ => |if_expr| {
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, if_expr.cond);
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, if_expr.then_body);
                try self.preRefineExprSourceTypes(inst, mono_cache, venv, if_expr.else_body);
                const result_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
                const refined_then_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    if_expr.then_body,
                    result_source_ty,
                );
                try self.unifyIn(inst.types, result_source_ty, refined_then_ty);
                const refined_else_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    if_expr.else_body,
                    result_source_ty,
                );
                try self.unifyIn(inst.types, result_source_ty, refined_else_ty);
            },
            .block => |block| try self.preRefineBlockSourceTypes(inst, mono_cache, venv, block),
        }

        const current_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
        const refined_source_ty = try self.refinedSourceTypeForExpr(inst, venv, expr_id, current_source_ty);
        try self.unifyIn(inst.types, current_source_ty, refined_source_ty);
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
        const exact_arg_tys = try self.allocator.alloc(?TypeVarId, arg_expr_ids.len);
        defer self.allocator.free(exact_arg_tys);
        for (arg_expr_ids, 0..) |arg_expr_id, i| {
            const expr = self.input.store.getExpr(arg_expr_id);
            exact_arg_tys[i] = if (self.directCallableSymbolFromExpr(venv, expr)) |exact_symbol| blk: {
                const exact_entry = specializations.lookupFnExact(self.fenv, exact_symbol) orelse break :blk null;
                break :blk try self.freshCloneTypeIntoInstFromStore(
                    inst,
                    &self.input.types,
                    exact_entry.fn_ty,
                );
            } else null;
        }
        var changed = true;
        while (changed) {
            changed = false;
            for (arg_expr_ids, expected_arg_tys, 0..) |arg_expr_id, expected_arg_ty, i| {
                const before = try inst.types.structuralKeyOwned(expected_arg_ty);
                defer inst.types.allocator.free(before);
                const refined_arg_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    arg_expr_id,
                    expected_arg_ty,
                );
                try self.unifyIn(inst.types, expected_arg_ty, refined_arg_ty);
                if (exact_arg_tys[i]) |exact_source_ty| {
                    try self.unifyIn(inst.types, expected_arg_ty, exact_source_ty);
                }
                const after = try inst.types.structuralKeyOwned(expected_arg_ty);
                defer inst.types.allocator.free(after);
                if (!std.mem.eql(u8, before, after)) {
                    changed = true;
                }
            }
        }
    }

    fn dupeTypeVarIds(
        self: *Lowerer,
        ids: []const TypeVarId,
    ) std.mem.Allocator.Error![]TypeVarId {
        return if (ids.len == 0)
            &.{}
        else
            try self.allocator.dupe(TypeVarId, ids);
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

    fn freshCloneTypeIntoInstFromStore(
        self: *Lowerer,
        inst: *InstScope,
        source_types: *const solved.Type.Store,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
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
        mono_cache: *lower_type.MonoCache,
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
                .exec_ty = try self.currentEnvEntryExecutableType(
                    inst.types,
                    mono_cache,
                    .{
                        .symbol = entry.symbol,
                        .ty = cloned_ty,
                        .exec_ty = entry.exec_ty,
                        .exact_fn_symbol = entry.exact_fn_symbol,
                    },
                    "cloneEnvIntoInstFromStoreWithMapping",
                ),
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
            .for_a, .flex_for_a => blk: {
                const fresh = try target_types.freshFlexForA();
                try mapping.put(id, fresh);
                break :blk fresh;
            },
            .unbd => blk: {
                const fresh = try target_types.freshUnbd();
                try mapping.put(id, fresh);
                break :blk fresh;
            },
            .nominal => |nominal| blk: {
                const placeholder = try target_types.freshUnbd();
                try mapping.put(id, placeholder);
                const source_args = source_types.sliceTypeVarSpan(nominal.args);
                const args = try self.allocator.dupe(TypeVarId, source_args);
                defer self.allocator.free(args);
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
                        const source_args = source_types.sliceTypeVarSpan(func.args);
                        const args = try self.allocator.dupe(TypeVarId, source_args);
                        defer self.allocator.free(args);
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
                        const source_elems = source_types.sliceTypeVarSpan(tuple);
                        const elems = try self.allocator.dupe(TypeVarId, source_elems);
                        defer self.allocator.free(elems);
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
                        const source_fields = source_types.sliceFields(record.fields);
                        const fields = try self.allocator.dupe(solved.Type.Field, source_fields);
                        defer self.allocator.free(fields);
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
                        const source_tags = source_types.sliceTags(tag_union.tags);
                        const tags = try self.allocator.dupe(solved.Type.Tag, source_tags);
                        defer self.allocator.free(tags);
                        const out = try self.allocator.alloc(solved.Type.Tag, tags.len);
                        defer self.allocator.free(out);
                        for (tags, 0..) |tag, i| {
                            const source_args = source_types.sliceTypeVarSpan(tag.args);
                            const args = try self.allocator.dupe(TypeVarId, source_args);
                            defer self.allocator.free(args);
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
                        const source_lambdas = source_types.sliceLambdas(lambda_set);
                        const lambdas = try self.allocator.dupe(solved.Type.Lambda, source_lambdas);
                        defer self.allocator.free(lambdas);
                        const out = try self.allocator.alloc(solved.Type.Lambda, lambdas.len);
                        defer self.allocator.free(out);
                        for (lambdas, 0..) |lambda, i| {
                            const source_captures = source_types.sliceCaptures(lambda.captures);
                            const captures = try self.allocator.dupe(solved.Type.Capture, source_captures);
                            defer self.allocator.free(captures);
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
                const arg_expr_ids = self.input.store.sliceExprSpan(call.args);
                const call_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                const call_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    arg_expr_ids,
                    call.call_constraint_ty,
                    call_result_ty,
                );
                break :blk self.attachedMethodOwnerForType(
                    inst.types,
                    inst.types.fnShape(call_constraint_ty).ret,
                    method_name,
                );
            },
            .dispatch_call => |method_call| blk: {
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const dispatch_arg_exprs = try self.allocator.alloc(solved.Ast.ExprId, method_args.len + 1);
                defer self.allocator.free(dispatch_arg_exprs);
                dispatch_arg_exprs[0] = method_call.receiver;
                @memcpy(dispatch_arg_exprs[1..], method_args);
                const call_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                const dispatch_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    dispatch_arg_exprs,
                    method_call.dispatch_constraint_ty,
                    call_result_ty,
                );
                const target_symbol = try self.resolveAttachedMethodTargetFromExpr(
                    inst,
                    venv,
                    method_call.receiver,
                    method_call.method_name,
                );
                var mono_cache = lower_type.MonoCache.init(self.allocator);
                defer mono_cache.deinit();
                const specialized = try self.ensureExactCallableSpecializedForRequestedType(
                    inst.types,
                    &mono_cache,
                    target_symbol,
                    dispatch_constraint_ty,
                    null,
                    null,
                );
                const imported = try self.importCallableSummaryIntoInst(
                    inst,
                    dispatch_constraint_ty,
                    specialized.summary_types,
                    specialized.summary_fn_ty,
                );
                const frozen_shape = imported.fn_shape;
                const frozen_arg_tys = inst.types.sliceTypeVarSpan(frozen_shape.args);
                const refined_receiver_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    method_call.receiver,
                    frozen_arg_tys[0],
                );
                try self.unifyIn(inst.types, frozen_arg_tys[0], refined_receiver_ty);
                try self.refineCallArgSourceTypesIn(
                    inst,
                    venv,
                    method_args,
                    frozen_arg_tys[1..],
                );
                break :blk self.attachedMethodOwnerForType(
                    inst.types,
                    frozen_shape.ret,
                    method_name,
                );
            },
            .type_dispatch_call => |method_call| blk: {
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const call_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                const dispatch_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    method_args,
                    method_call.dispatch_constraint_ty,
                    call_result_ty,
                );
                const dispatcher_ty = try self.cloneTypeIntoInstFromStore(
                    inst,
                    &self.input.types,
                    method_call.dispatcher_ty,
                );
                const target_symbol = self.resolveAttachedMethodTarget(
                    inst.types,
                    dispatcher_ty,
                    method_call.method_name,
                );
                var mono_cache = lower_type.MonoCache.init(self.allocator);
                defer mono_cache.deinit();
                const specialized = try self.ensureExactCallableSpecializedForRequestedType(
                    inst.types,
                    &mono_cache,
                    target_symbol,
                    dispatch_constraint_ty,
                    null,
                    null,
                );
                const imported = try self.importCallableSummaryIntoInst(
                    inst,
                    dispatch_constraint_ty,
                    specialized.summary_types,
                    specialized.summary_fn_ty,
                );
                const frozen_shape = imported.fn_shape;
                const frozen_arg_tys = inst.types.sliceTypeVarSpan(frozen_shape.args);
                try self.refineCallArgSourceTypesIn(
                    inst,
                    venv,
                    method_args,
                    frozen_arg_tys,
                );
                break :blk self.attachedMethodOwnerForType(
                    inst.types,
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

    fn specializeFn(self: *Lowerer, pending_symbol: Symbol) std.mem.Allocator.Error!SpecializedDef {
        const pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
            debugPanic("lambdamono.lower.specializeFn missing queued specialization");
        const source_def = pending.source_def;
        const repr_mode = pending.repr_mode;
        const capture_exact_symbols = pending.capture_exact_symbols;
        const previous_specializing = self.current_specializing_symbol;
        self.current_specializing_symbol = pending_symbol;
        defer self.current_specializing_symbol = previous_specializing;
        switch (source_def) {
            .hosted_fn => |hosted_fn| {
                if (repr_mode != .erased_boundary) {
                    debugPanic("lambdamono.lower.specializeFn hosted specialization must use erased boundary representation");
                }
                const proc_arg_exec_tys = try self.ensurePendingExecutableArgTypes(pending);
                const hosted_args = self.input.store.sliceTypedSymbolSpan(hosted_fn.args);
                if (hosted_args.len != proc_arg_exec_tys.len) {
                    debugPanic("lambdamono.lower.specializeFn hosted arg arity mismatch");
                }
                const lowered_args = try self.allocator.alloc(ast.TypedSymbol, hosted_args.len);
                defer self.allocator.free(lowered_args);
                for (hosted_args, proc_arg_exec_tys, 0..) |arg, exec_ty, i| {
                    lowered_args[i] = .{
                        .ty = exec_ty,
                        .symbol = arg.symbol,
                    };
                }
                return .{
                    .value = .{ .hosted_fn = .{
                        .bind = pending_symbol,
                        .args = try self.output.addTypedSymbolSpan(lowered_args),
                        .hosted = hosted_fn.hosted,
                    } },
                };
            },
            .fn_ => |fn_def| {
                const summary_types = pending.summary_types orelse
                    debugPanic("lambdamono.lower.specializeFn missing specialization summary store");
                const summary_fn_ty = pending.summary_fn_ty orelse
                    debugPanic("lambdamono.lower.specializeFn missing specialization summary type");
                const summary_exact_captures = pending.summary_exact_captures orelse
                    debugPanic("lambdamono.lower.specializeFn missing exact specialization captures");
                var inst = InstScope.borrow(self.allocator, summary_types);
                defer inst.deinit();
                var mono_cache = lower_type.MonoCache.init(self.allocator);
                defer mono_cache.deinit();
                const fn_body = fn_def.body;
                const frozen_captures = summary_exact_captures;
                const fn_args = self.input.store.sliceTypedSymbolSpan(fn_def.args);
                const fn_shape = inst.types.fnShape(summary_fn_ty);
                const requested_arg_tys = try self.dupeTypeVarIds(
                    inst.types.sliceTypeVarSpan(fn_shape.args),
                );
                defer if (requested_arg_tys.len != 0) self.allocator.free(requested_arg_tys);
                const initial_proc_arg_exec_tys = try self.ensurePendingExecutableArgTypes(pending);
                const initial_proc_capture_ty = pending.exec_capture_ty;
                const preserved_summary_ret_ty = try self.freshCloneTypeIntoInstFromStore(
                    &inst,
                    inst.types,
                    fn_shape.ret,
                );
                if (fn_args.len != requested_arg_tys.len) {
                    debugPanic("lambdamono.lower.specializeFn function arg arity mismatch");
                }
                var final_source_ret_ty = fn_shape.ret;
                const result: ast.FnDef = switch (repr_mode) {
                    .natural => if (frozen_captures.len == 0) blk_fn: {
                        const provisional_body_env = try self.buildFnBodyEnv(
                            inst.types,
                            requested_arg_tys,
                            initial_proc_arg_exec_tys,
                            fn_args,
                            &[_]EnvEntry{},
                        );
                        defer self.allocator.free(provisional_body_env);
                        try self.preRefineExprSourceTypes(&inst, &mono_cache, provisional_body_env, fn_body);
                        const body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                            debugPanic("lambdamono.lower.specializeFn lost pending specialization before lowering natural body");
                        const proc_arg_exec_tys = try self.ensurePendingExecutableArgTypes(body_pending);
                        const body_env = try self.buildFnBodyEnv(
                            inst.types,
                            requested_arg_tys,
                            proc_arg_exec_tys,
                            fn_args,
                            &[_]EnvEntry{},
                        );
                        defer self.allocator.free(body_env);
                        const body = blk_body: {
                            const pre_body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                                debugPanic("lambdamono.lower.specializeFn lost pending specialization before reading natural return signature");
                            const pre_lower_ret_ty = try self.ensurePendingExecutableReturnType(pre_body_pending);
                            const previous_return_exec_ty = self.current_return_exec_ty;
                            self.current_return_exec_ty = pre_lower_ret_ty;
                            defer self.current_return_exec_ty = previous_return_exec_ty;
                            const body_expr = self.input.store.getExpr(fn_body);
                            const lowered_body = try self.specializeExprWithDefaultTyAndSourceTy(
                                &inst,
                                &mono_cache,
                                body_env,
                                fn_body,
                                body_expr,
                                fn_shape.ret,
                                pre_lower_ret_ty,
                            );
                            final_source_ret_ty = lowered_body.source_ty;
                            const body = lowered_body.expr;
                            const authoritative_source_ret_ty = if (!self.sourceTypeIsAbstract(
                                inst.types,
                                preserved_summary_ret_ty,
                            ))
                                preserved_summary_ret_ty
                            else
                                final_source_ret_ty;
                            try self.overwriteSolvedTypeWithExact(
                                inst.types,
                                fn_shape.ret,
                                authoritative_source_ret_ty,
                            );
                            const post_body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                                debugPanic("lambdamono.lower.specializeFn lost pending specialization after lowering natural body");
                            const body_ty = self.output.getExpr(body).ty;
                            self.absorbExplicitBodyReturnExecutableType(pending_symbol, body_ty);
                            const authoritative_ret_ty = try self.ensurePendingExecutableReturnType(post_body_pending);
                            const bridged = if (!self.types.equalIds(body_ty, authoritative_ret_ty))
                                try self.retargetOrEmitExplicitBridgeExpr(body, authoritative_ret_ty)
                            else
                                body;
                            break :blk_body bridged;
                        };
                        const args_span = blk_args: {
                            const updated_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                                debugPanic("lambdamono.lower.specializeFn lost pending specialization before finalizing natural args");
                            const finalized_proc_arg_exec_tys = try self.ensurePendingExecutableArgTypes(updated_pending);
                            const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len);
                            defer self.allocator.free(lowered_args);
                            for (fn_args, finalized_proc_arg_exec_tys, 0..) |arg, exec_ty, i| {
                                lowered_args[i] = .{
                                    .ty = exec_ty,
                                    .symbol = arg.symbol,
                                };
                            }
                            break :blk_args try self.output.addTypedSymbolSpan(lowered_args);
                        };
                        break :blk_fn .{
                            .args = args_span,
                            .body = body,
                        };
                    } else blk: {
                        const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                        const capture_exec_ty = initial_proc_capture_ty orelse
                            debugPanic("lambdamono.lower.specializeFn missing queued executable capture signature");
                        const capture_env = try self.captureBindingsFromCaptures(
                            inst.types,
                            frozen_captures,
                            capture_exec_ty,
                            capture_exact_symbols,
                        );
                        defer self.allocator.free(capture_env);
                        const provisional_body_env = try self.buildFnBodyEnv(
                            inst.types,
                            requested_arg_tys,
                            initial_proc_arg_exec_tys,
                            fn_args,
                            capture_env,
                        );
                        defer self.allocator.free(provisional_body_env);
                        try self.preRefineExprSourceTypes(&inst, &mono_cache, provisional_body_env, fn_body);
                        const pre_body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                            debugPanic("lambdamono.lower.specializeFn lost pending specialization before reading captured natural return signature");
                        const pre_lower_ret_ty = try self.ensurePendingExecutableReturnType(pre_body_pending);
                        const previous_return_exec_ty = self.current_return_exec_ty;
                        self.current_return_exec_ty = pre_lower_ret_ty;
                        defer self.current_return_exec_ty = previous_return_exec_ty;

                        const body_expr = self.input.store.getExpr(fn_body);
                        const lowered_body = try self.specializeExprWithDefaultTyAndSourceTy(
                            &inst,
                            &mono_cache,
                            provisional_body_env,
                            fn_body,
                            body_expr,
                            fn_shape.ret,
                            pre_lower_ret_ty,
                        );
                        final_source_ret_ty = lowered_body.source_ty;
                        var body = lowered_body.expr;
                        const authoritative_source_ret_ty = if (!self.sourceTypeIsAbstract(
                            inst.types,
                            preserved_summary_ret_ty,
                        ))
                            preserved_summary_ret_ty
                        else
                            final_source_ret_ty;
                        try self.overwriteSolvedTypeWithExact(
                            inst.types,
                            fn_shape.ret,
                            authoritative_source_ret_ty,
                        );
                        const post_body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                            debugPanic("lambdamono.lower.specializeFn lost pending specialization after lowering captured natural body");
                        const body_ty = self.output.getExpr(body).ty;
                        self.absorbExplicitBodyReturnExecutableType(pending_symbol, body_ty);
                        const authoritative_ret_ty = try self.ensurePendingExecutableReturnType(post_body_pending);
                        if (!self.types.equalIds(body_ty, authoritative_ret_ty)) {
                            body = try self.retargetOrEmitExplicitBridgeExpr(body, authoritative_ret_ty);
                        }
                        body = try self.bindCaptureLets(
                            captures_symbol,
                            capture_exec_ty,
                            capture_env,
                            body,
                        );
                        const updated_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                            debugPanic("lambdamono.lower.specializeFn lost pending specialization before finalizing captured natural args");
                        const finalized_proc_arg_exec_tys = try self.ensurePendingExecutableArgTypes(updated_pending);
                        const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len + 1);
                        defer self.allocator.free(lowered_args);
                        for (fn_args, finalized_proc_arg_exec_tys, 0..) |arg, exec_ty, i| {
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
                            const lowered_capture_ty = initial_proc_capture_ty orelse
                                debugPanic("lambdamono.lower.specializeFn missing queued erased executable capture signature");
                            captures_ty = lowered_capture_ty;
                            capture_env = try self.captureBindingsFromCaptures(
                                inst.types,
                                frozen_captures,
                                lowered_capture_ty,
                                capture_exact_symbols,
                            );
                        }
                        const provisional_body_env = try self.buildFnBodyEnv(
                            inst.types,
                            requested_arg_tys,
                            initial_proc_arg_exec_tys,
                            fn_args,
                            capture_env,
                        );
                        defer self.allocator.free(provisional_body_env);
                        try self.preRefineExprSourceTypes(&inst, &mono_cache, provisional_body_env, fn_body);
                        const pre_body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                            debugPanic("lambdamono.lower.specializeFn lost pending specialization before reading erased signature");
                        const proc_arg_exec_tys = try self.ensurePendingExecutableArgTypes(pre_body_pending);
                        const body_env = try self.buildFnBodyEnv(
                            inst.types,
                            requested_arg_tys,
                            proc_arg_exec_tys,
                            fn_args,
                            capture_env,
                        );
                        defer self.allocator.free(body_env);
                        const pre_lower_ret_ty = try self.ensurePendingExecutableReturnType(pre_body_pending);
                        const previous_return_exec_ty = self.current_return_exec_ty;
                        self.current_return_exec_ty = pre_lower_ret_ty;
                        defer self.current_return_exec_ty = previous_return_exec_ty;

                        const body_expr = self.input.store.getExpr(fn_body);
                        const lowered_body = try self.specializeExprWithDefaultTyAndSourceTy(
                            &inst,
                            &mono_cache,
                            body_env,
                            fn_body,
                            body_expr,
                            fn_shape.ret,
                            pre_lower_ret_ty,
                        );
                        final_source_ret_ty = lowered_body.source_ty;
                        var body = lowered_body.expr;
                        const authoritative_source_ret_ty = if (!self.sourceTypeIsAbstract(
                            inst.types,
                            preserved_summary_ret_ty,
                        ))
                            preserved_summary_ret_ty
                        else
                            final_source_ret_ty;
                        try self.overwriteSolvedTypeWithExact(
                            inst.types,
                            fn_shape.ret,
                            authoritative_source_ret_ty,
                        );
                        const post_body_pending = self.lookupPendingBySpecializedSymbol(pending_symbol) orelse
                            debugPanic("lambdamono.lower.specializeFn lost pending specialization after lowering erased body");
                        const final_ret_ty = try self.ensurePendingExecutableReturnType(post_body_pending);
                        if (self.types.getTypePreservingNominal(final_ret_ty) == .erased_fn and
                            inst.types.maybeLambdaRepr(fn_shape.ret) != null)
                        {
                            body = try self.lowerBoxBoundaryExpr(
                                &inst,
                                &mono_cache,
                                body_env,
                                fn_shape.ret,
                                body,
                            );
                        }
                        const body_ty = self.output.getExpr(body).ty;
                        self.absorbExplicitBodyReturnExecutableType(pending_symbol, body_ty);
                        const authoritative_ret_ty = try self.ensurePendingExecutableReturnType(post_body_pending);
                        if (!self.types.equalIds(body_ty, authoritative_ret_ty)) {
                            body = try self.retargetOrEmitExplicitBridgeExpr(body, authoritative_ret_ty);
                        }
                        if (capture_env.len != 0) {
                            const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                            body = try self.bindCaptureLets(captures_symbol, captures_ty.?, capture_env, body);
                            break :blk .{
                                .args = blk_args: {
                                const lowered_args = try self.allocator.alloc(ast.TypedSymbol, fn_args.len + 1);
                                defer self.allocator.free(lowered_args);
                                for (fn_args, proc_arg_exec_tys, 0..) |arg, exec_ty, i| {
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
                            for (fn_args, proc_arg_exec_tys, 0..) |arg, exec_ty, i| {
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
                try self.overwriteSolvedTypeWithExact(inst.types, fn_shape.ret, final_source_ret_ty);
                return .{
                    .value = .{ .fn_ = result },
                };
            },
        }
    }

    fn buildFnBodyEnv(
        self: *Lowerer,
        _: *solved.Type.Store,
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
            out[i] = .{
                .symbol = arg.symbol,
                .ty = arg_ty,
                .exec_ty = arg_exec_ty,
                .exact_fn_symbol = null,
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

    fn currentCapturePayloadFromSymbols(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        capture_symbols: []const Symbol,
        venv: []const EnvEntry,
    ) std.mem.Allocator.Error!CurrentCapturePayload {
        if (capture_symbols.len == 0) {
            return .{ .env = &.{}, .ty = null };
        }

        const capture_env = try self.allocator.alloc(EnvEntry, capture_symbols.len);
        errdefer self.allocator.free(capture_env);
        const capture_bindings = try self.allocator.alloc(lower_type.CaptureBinding, capture_symbols.len);
        defer self.allocator.free(capture_bindings);

        for (capture_symbols, 0..) |capture_symbol, i| {
            const entry = self.lookupEnvEntry(venv, capture_symbol) orelse
                debugPanic("lambdamono.lower.currentCapturePayloadFromEnv missing current capture binding");
            const current_exec_ty = try self.currentEnvEntryExecutableType(
                solved_types,
                mono_cache,
                entry,
                "currentCapturePayloadFromSymbols",
            );
            capture_env[i] = entry;
            capture_bindings[i] = .{
                .symbol = capture_symbol,
                .lowered_ty = current_exec_ty,
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
        capture_exact_symbols: ?[]const Symbol,
    ) std.mem.Allocator.Error!Symbol {
        var normalized_capture_exec_ty = capture_exec_ty;
        if (repr_mode == .erased_boundary) {
            if (normalized_capture_exec_ty) |ty| {
                if (self.isEmptyRecordType(ty)) {
                    normalized_capture_exec_ty = null;
                }
            }
        }
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
        return try self.queueCallableSpecializationWithResolvedExecSignature(
            solved_types,
            symbol,
            repr_mode,
            requested_ty,
            capture_exec_ty,
            capture_exact_symbols,
            exec_arg_tys,
            exec_ret_ty,
        );
    }

    fn queueCallableSpecializationWithResolvedExecSignature(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        symbol: Symbol,
        repr_mode: specializations.Pending.ReprMode,
        requested_ty: TypeVarId,
        capture_exec_ty: ?type_mod.TypeId,
        capture_exact_symbols: ?[]const Symbol,
        exec_arg_tys: []const type_mod.TypeId,
        exec_ret_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!Symbol {
        var normalized_capture_exec_ty = capture_exec_ty;
        if (repr_mode == .erased_boundary) {
            if (normalized_capture_exec_ty) |ty| {
                if (self.isEmptyRecordType(ty)) {
                    normalized_capture_exec_ty = null;
                }
            }
        }
        if (specializations.lookupFnExact(self.fenv, symbol) != null) {
            return try specializations.specializeFnWithExecArgs(
                &self.queue,
                self.fenv,
                solved_types,
                &self.input.symbols,
                symbol,
                repr_mode,
                requested_ty,
                &self.types,
                normalized_capture_exec_ty,
                capture_exact_symbols,
                exec_arg_tys,
                exec_ret_ty,
            );
        }
        const source = self.top_level_values.get(self.rootSourceSymbol(symbol)) orelse
            debugPanic("lambdamono.lower.queueCallableSpecializationWithExplicitSignature missing top-level callable source");
        return switch (source) {
            .hosted_fn => |hosted_fn| try specializations.specializeHostedWithExecArgs(
                &self.queue,
                solved_types,
                &self.input.symbols,
                symbol,
                repr_mode,
                requested_ty,
                hosted_fn,
                &self.types,
                normalized_capture_exec_ty,
                capture_exact_symbols,
                exec_arg_tys,
                exec_ret_ty,
            ),
            .fn_ => debugPanic("lambdamono.lower.queueCallableSpecializationWithExplicitSignature missing exact function env entry"),
            .val, .run => debugPanic("lambdamono.lower.queueCallableSpecializationWithExplicitSignature expected exact callable symbol"),
        };
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

    const LoweredExprWithSourceTy = struct {
        expr: ast.ExprId,
        source_ty: TypeVarId,
    };

    fn specializeExprAtSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.input.store.getExpr(expr_id);
        const default_ty = try self.explicitValueExecutableTypeForExpr(
            inst.types,
            mono_cache,
            venv,
            expr_id,
            ty,
        );
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
        return (try self.specializeExprWithDefaultTyAndSourceTy(
            inst,
            mono_cache,
            venv,
            expr_id,
            expr,
            ty,
            default_ty,
        )).expr;
    }

    fn specializeExprWithDefaultTyAndSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        _: solved.Ast.ExprId,
        expr: solved.Ast.Expr,
        ty: TypeVarId,
        default_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!LoweredExprWithSourceTy {
        if (expr.data == .inspect) {
            return .{
                .expr = try self.specializeInspectExpr(inst, mono_cache, venv, expr.data.inspect, default_ty),
                .source_ty = ty,
            };
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

                const solved_tag_arg_tys = try self.tagArgSourceTypesForExpr(
                    inst,
                    venv,
                    ty,
                    tag.name,
                    source_args,
                );
                defer if (solved_tag_arg_tys.len != 0) self.allocator.free(solved_tag_arg_tys);
                const requested_tag_ty = if (!self.sourceTypeIsAbstract(inst.types, ty)) blk_tag_ty: {
                    const lowered_source_tag_ty = try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, ty);
                    if (!self.executableTypeIsAbstract(lowered_source_tag_ty)) {
                        break :blk_tag_ty self.requireConcreteExecutableType(
                            lowered_source_tag_ty,
                            "specializeExpr(tag source)",
                        );
                    }
                    break :blk_tag_ty self.requireConcreteExecutableType(
                        try self.lowerErasedBoundaryExecutableTypeIn(inst.types, mono_cache, ty),
                        "specializeExpr(tag source erased boundary)",
                    );
                } else if (!self.executableTypeIsAbstract(default_ty))
                    self.requireConcreteExecutableType(default_ty, "specializeExpr(tag)")
                else
                    null;
                const stable_tag_ty = if (requested_tag_ty) |tag_ty|
                    self.stableConcreteExpectedExecutableType(tag_ty, "specializeExpr(tag stable)")
                else
                    null;
                const tag_exec_info = if (requested_tag_ty) |tag_ty|
                    self.executableTagInfoByName(tag_ty, tag.name) orelse {
                        const tag_ty_summary = self.debugExecutableTypeSummary(tag_ty);
                        defer tag_ty_summary.deinit(self.allocator);
                        debugPanicFmt(
                            "lambdamono.lower.tag missing executable tag info tag={s} exec_ty={s}",
                            .{ self.input.idents.getText(tag.name), tag_ty_summary.text },
                        );
                    }
                else
                    null;
                if (tag_exec_info) |info| {
                    if (info.args.len != source_args.len) {
                        debugPanic("lambdamono.lower.tag expected tag arg count mismatch");
                    }
                }

                const actual_arg_tys = try self.allocator.alloc(type_mod.TypeId, source_args.len);
                defer self.allocator.free(actual_arg_tys);
                for (source_args, 0..) |arg_expr_id, i| {
                    const arg_expr = self.input.store.getExpr(arg_expr_id);
                    const requested_arg_ty = if (stable_tag_ty != null and tag_exec_info != null)
                        tag_exec_info.?.args[i]
                    else
                        try self.lowerExecutableTypeFromSolvedIn(
                            inst.types,
                            mono_cache,
                            solved_tag_arg_tys[i],
                        );
                    const explicit_arg_exec_ty = try self.explicitValueExecutableTypeForExpr(
                        inst.types,
                        mono_cache,
                        venv,
                        arg_expr_id,
                        solved_tag_arg_tys[i],
                    );
                    const expected_arg_ty = if (self.executableTypesHaveErasedCallableShapeMismatch(
                        explicit_arg_exec_ty,
                        requested_arg_ty,
                    ))
                        explicit_arg_exec_ty
                    else
                        requested_arg_ty;
                    const lowered = try self.specializeExprWithDefaultTy(
                        inst,
                        mono_cache,
                        venv,
                        arg_expr_id,
                        arg_expr,
                        solved_tag_arg_tys[i],
                        expected_arg_ty,
                    );
                    lowered_args[i] = lowered;
                    actual_arg_tys[i] = self.output.getExpr(lowered).ty;
                }
                const result_ty = if (source_args.len != 0)
                    try self.lowerRequestedExecutableReturnTypeFromCallRelation(
                        inst.types,
                        mono_cache,
                        solved_tag_arg_tys,
                        actual_arg_tys,
                        ty,
                        .natural,
                        "specializeExpr(tag result)",
                    )
                else if (stable_tag_ty) |tag_ty|
                    tag_ty
                else if (requested_tag_ty) |tag_ty|
                    self.requireConcreteExecutableType(tag_ty, "specializeExpr(tag)")
                else
                    debugPanic("lambdamono.lower.tag missing explicit executable tag result type");
                const final_tag_info = self.executableTagInfoByName(result_ty, tag.name) orelse {
                    const tag_ty_summary = self.debugExecutableTypeSummary(result_ty);
                    defer tag_ty_summary.deinit(self.allocator);
                    debugPanicFmt(
                        "lambdamono.lower.tag missing final executable tag info tag={s} exec_ty={s}",
                        .{ self.input.idents.getText(tag.name), tag_ty_summary.text },
                    );
                };
                if (final_tag_info.args.len != source_args.len) {
                    debugPanic("lambdamono.lower.tag final executable tag arg count mismatch");
                }
                for (lowered_args, final_tag_info.args, 0..) |lowered_arg, expected_arg_ty, i| {
                    if (!self.types.equalIds(self.output.getExpr(lowered_arg).ty, expected_arg_ty)) {
                        lowered_args[i] = try self.retargetOrEmitExplicitBridgeExpr(lowered_arg, expected_arg_ty);
                    }
                }

                break :blk .{
                    .ty = result_ty,
                    .data = .{ .tag = .{
                        .name = .{ .ctor = tag.name },
                        .discriminant = final_tag_info.discriminant,
                        .args = try self.output.addExprSpan(lowered_args),
                    } },
                    .source_ty = ty,
                };
            },
            .record => |fields| blk: {
                const source_fields = self.input.store.sliceFieldExprSpan(fields);
                const lowered = try self.allocator.alloc(ast.FieldExpr, source_fields.len);
                defer self.allocator.free(lowered);
                const concrete_record_ty = self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(record)",
                );

                for (source_fields, 0..) |field, i| {
                    const source_field_ty = self.solvedRecordFieldByName(inst.types, ty, field.name) orelse
                        debugPanic("lambdamono.lower.record missing solved record field");
                    const field_expr = self.input.store.getExpr(field.value);
                    const expected_field_ty = if (concrete_record_ty) |record_ty|
                        (self.recordFieldByName(record_ty, field.name) orelse
                            debugPanic("lambdamono.lower.record missing executable record field")).ty
                    else
                        try self.lowerExecutableTypeFromSolvedIn(
                            inst.types,
                            mono_cache,
                            source_field_ty,
                        );
                    const value = try self.specializeExprWithDefaultTy(
                        inst,
                        mono_cache,
                        venv,
                        field.value,
                        field_expr,
                        source_field_ty,
                        expected_field_ty,
                    );
                    lowered[i] = .{
                        .name = field.name,
                        .value = value,
                    };
                }
                var result_uses_field_value_tys = concrete_record_ty == null;
                if (concrete_record_ty) |record_ty| {
                    for (lowered) |*field| {
                        const expected_field = self.recordFieldByName(record_ty, field.name) orelse
                            debugPanic("lambdamono.lower.record missing executable record field");
                        if (!self.types.equalIds(self.output.getExpr(field.value).ty, expected_field.ty)) {
                            if (self.executableTypesHaveErasedCallableShapeMismatch(
                                self.output.getExpr(field.value).ty,
                                expected_field.ty,
                            )) {
                                result_uses_field_value_tys = true;
                            } else {
                                field.value = try self.retargetOrEmitExplicitBridgeExpr(field.value, expected_field.ty);
                            }
                        }
                    }
                }
                const lowered_fields = try self.output.addFieldExprSpan(lowered);
                const result_ty = if (result_uses_field_value_tys)
                    try self.recordTypeFromFieldValues(lowered)
                else if (concrete_record_ty) |record_ty|
                    record_ty
                else
                    unreachable;
                const ordered_fields = try self.orderedRecordFields(result_ty, lowered_fields);
                break :blk .{ .ty = result_ty, .data = .{ .record = ordered_fields } };
            },
            .access => |access| blk: {
                const record = try self.specializeExpr(inst, mono_cache, venv, access.record);
                const record_ty = self.output.getExpr(record).ty;
                const field_info = self.recordFieldByName(record_ty, access.field) orelse
                    debugPanic("lambdamono.lower.access missing record field");
                const field_ty = field_info.ty;
                const expected_access_ty = self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(access)",
                );
                const result_ty = if (expected_access_ty) |expected_ty|
                    if (self.executableTypesHaveErasedCallableShapeMismatch(field_ty, expected_ty))
                        field_ty
                    else
                        expected_ty
                else
                    field_ty;
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
                const refined_entry = try self.preRefineBindingInEnv(
                    inst,
                    mono_cache,
                    venv,
                    let_expr.bind,
                    let_expr.body,
                );
                const refined_rest_env = try self.extendEnv(venv, refined_entry);
                defer self.allocator.free(refined_rest_env);
                try self.preRefineExprSourceTypes(inst, mono_cache, refined_rest_env, let_expr.rest);

                var lowered_binding = try self.lowerBindingBody(
                    inst,
                    mono_cache,
                    venv,
                    let_expr.bind,
                    let_expr.body,
                    refined_entry.ty,
                    "specializeExpr(let_)",
                );
                if (!self.sourceTypeIsAbstract(inst.types, refined_entry.ty)) {
                    lowered_binding.source_ty = refined_entry.ty;
                    lowered_binding.exec_ty = self.requireConcreteExecutableType(
                        lowered_binding.exec_ty,
                        "specializeExpr(let_ refined)",
                    );
                }
                const source_exact_fn_symbol = self.exactCallableSymbolForBoundExpr(let_expr.bind.symbol, let_expr.body);
                const rest_env = try self.extendEnv(venv, .{
                    .symbol = let_expr.bind.symbol,
                    .ty = lowered_binding.source_ty,
                    .exec_ty = lowered_binding.exec_ty,
                    .exact_fn_symbol = source_exact_fn_symbol,
                });
                defer self.allocator.free(rest_env);
                const rest_expr = self.input.store.getExpr(let_expr.rest);
                const rest = try self.specializeExprWithDefaultTyAndSourceTy(
                    inst,
                    mono_cache,
                    rest_env,
                    let_expr.rest,
                    rest_expr,
                    ty,
                    default_ty,
                );
                break :blk .{
                    .ty = self.output.getExpr(rest.expr).ty,
                    .data = .{ .let_ = .{
                        .bind = .{
                            .ty = lowered_binding.exec_ty,
                            .symbol = let_expr.bind.symbol,
                        },
                        .body = lowered_binding.body,
                        .rest = rest.expr,
                    } },
                    .source_ty = rest.source_ty,
                };
            },
            .call => |call| try self.specializeCallExpr(inst, mono_cache, venv, call, ty, default_ty),
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
                const source_args = self.input.store.sliceExprSpan(ll.args);
                const constraint_fn_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    source_args,
                    ll.source_constraint_ty,
                    ty,
                );
                const constraint_shape = inst.types.fnShape(constraint_fn_ty);
                const constraint_arg_tys = try self.dupeTypeVarIds(
                    inst.types.sliceTypeVarSpan(constraint_shape.args),
                );
                defer if (constraint_arg_tys.len != 0) self.allocator.free(constraint_arg_tys);
                try self.refineCallArgSourceTypesIn(
                    inst,
                    venv,
                    source_args,
                    constraint_arg_tys,
                );
                const lowered_args = switch (ll.op) {
                    .box_box, .box_unbox => blk_args: {
                        const out = try self.allocator.alloc(ast.ExprId, source_args.len);
                        defer self.allocator.free(out);
                        for (source_args, 0..) |arg_id, i| {
                            out[i] = try self.specializeBoxBoundaryArgExpr(inst, mono_cache, venv, ll.op, arg_id);
                        }
                        break :blk_args try self.output.addExprSpan(out);
                    },
                    else => blk_args: {
                        const out = try self.allocator.alloc(ast.ExprId, source_args.len);
                        defer self.allocator.free(out);
                        for (source_args, 0..) |arg_id, i| {
                            const arg_expr = self.input.store.getExpr(arg_id);
                            out[i] = try self.specializeExprWithDefaultTy(
                                inst,
                                mono_cache,
                                venv,
                                arg_id,
                                arg_expr,
                                constraint_arg_tys[i],
                                try self.lowerExecutableTypeFromSolvedIn(
                                    inst.types,
                                    mono_cache,
                                    constraint_arg_tys[i],
                                ),
                            );
                            _ = self.requireConcreteExecutableType(
                                self.output.getExpr(out[i]).ty,
                                "specializeExpr(low_level arg)",
                            );
                        }
                        break :blk_args try self.output.addExprSpan(out);
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
                    else => try self.lowerExecutableTypeFromSolvedIn(
                        inst.types,
                        mono_cache,
                        constraint_shape.ret,
                    ),
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
                const source_result_ty = switch (ll.op) {
                    .box_box => blk_ty: {
                        if (source_args.len != 1) {
                            debugPanic("lambdamono.lower.specializeExpr box_box expected one source arg");
                        }
                        const arg_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, source_args[0]);
                        const refined_arg_ty = try self.refinedSourceTypeForExpr(inst, venv, source_args[0], arg_source_ty);
                        const boxed_payload_ty = self.solvedBoxElemType(inst.types, ty) orelse
                            debugPanic("lambdamono.lower.specializeExpr box_box missing solved payload type");
                        try self.unifyIn(inst.types, boxed_payload_ty, refined_arg_ty);
                        break :blk_ty ty;
                    },
                    .box_unbox => blk_ty: {
                        if (source_args.len != 1) {
                            debugPanic("lambdamono.lower.specializeExpr box_unbox expected one source arg");
                        }
                        const boxed_arg_ty = try self.instantiatedSourceTypeForExpr(inst, venv, source_args[0]);
                        break :blk_ty self.solvedBoxElemType(inst.types, boxed_arg_ty) orelse
                            debugPanic("lambdamono.lower.specializeExpr box_unbox missing solved payload type");
                    },
                    else => constraint_shape.ret,
                };
                break :blk .{
                    .ty = result_ty,
                    .data = .{ .low_level = .{
                        .op = ll.op,
                        .args = lowered_args,
                    } },
                    .source_ty = source_result_ty,
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
                var lowered_branches = std.ArrayList(ast.Branch).empty;
                defer lowered_branches.deinit(self.allocator);
                try lowered_branches.ensureTotalCapacity(self.allocator, input_branch_ids.len);
                var all_remaining_covered = false;
                const explicit_result_ty = try self.lowerExecutableTypeFromSolvedIn(
                    inst.types,
                    mono_cache,
                    ty,
                );
                const concrete_result_ty = self.stableConcreteExpectedExecutableType(
                    explicit_result_ty,
                    "specializeExpr(when result)",
                ) orelse self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(when)",
                );
                const branch_default_ty = concrete_result_ty orelse explicit_result_ty;

                for (input_branch_ids) |branch_id| {
                    if (all_remaining_covered) break;
                    const branch = self.input.store.getBranch(branch_id);
                    const branch_pat = self.input.store.getPat(branch.pat);
                    if (!self.patPossibleAtExecutableTy(branch_pat, self.output.getExpr(cond).ty)) {
                        continue;
                    }
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
                    const branch_body_source_ty = try self.freshCloneTypeIntoInstFromStore(
                        inst,
                        inst.types,
                        ty,
                    );
                    const lowered_body = try self.specializeExprWithDefaultTyAndSourceTy(
                        inst,
                        mono_cache,
                        branch_env,
                        branch.body,
                        branch_body_expr,
                        branch_body_source_ty,
                        branch_default_ty,
                    );
                    try lowered_branches.append(self.allocator, .{
                        .pat = pat_result.pat,
                        .body = lowered_body.expr,
                    });
                    if (self.patExhaustsExecutableTy(branch_pat, self.output.getExpr(cond).ty)) {
                        all_remaining_covered = true;
                    }
                }
                if (lowered_branches.items.len == 0) {
                    debugPanic("lambdamono.lower.specializeExpr(when) removed all branches");
                }

                const first_branch_ty = self.output.getExpr(lowered_branches.items[0].body).ty;
                const result_ty = if (concrete_result_ty) |concrete|
                    blk_result: {
                        var use_branch_ty = self.executableTypesHaveErasedCallableShapeMismatch(first_branch_ty, concrete);
                        for (lowered_branches.items[1..]) |branch| {
                            const branch_ty = self.output.getExpr(branch.body).ty;
                            if (self.executableTypesHaveErasedCallableShapeMismatch(branch_ty, concrete)) {
                                use_branch_ty = true;
                            }
                            if (use_branch_ty and !self.types.equalIds(first_branch_ty, branch_ty)) {
                                debugPanic("lambdamono.lower.specializeExpr(when) branch erased callable executable types disagree");
                            }
                        }
                        break :blk_result if (use_branch_ty) first_branch_ty else concrete;
                    }
                else blk_result: {
                    for (lowered_branches.items[1..]) |branch| {
                        const branch_ty = self.output.getExpr(branch.body).ty;
                        if (!self.types.equalIds(first_branch_ty, branch_ty)) {
                            debugPanic("lambdamono.lower.specializeExpr(when) branch executable types disagree without an explicit result type");
                        }
                    }
                    break :blk_result first_branch_ty;
                };
                if (!self.executableTypeIsAbstract(result_ty)) {
                    for (lowered_branches.items) |*branch| {
                        const branch_body_ty = self.output.getExpr(branch.body).ty;
                        if (!self.types.equalIds(branch_body_ty, result_ty)) {
                            if (!try self.retargetExprToExpectedExecutableType(branch.body, result_ty)) {
                                branch.body = try self.emitExplicitBridgeExpr(branch.body, result_ty);
                            }
                        }
                    }
                }
                const branches = try self.output.addBranchSpan(lowered_branches.items);
                break :blk .{ .ty = result_ty, .data = .{ .when = .{
                    .cond = cond,
                    .branches = branches,
                } }, .source_ty = ty };
            },
            .if_ => |if_expr| blk: {
                const cond = try self.specializeExpr(inst, mono_cache, venv, if_expr.cond);
                const then_expr = self.input.store.getExpr(if_expr.then_body);
                const else_expr = self.input.store.getExpr(if_expr.else_body);
                const then_source_ty = try self.freshCloneTypeIntoInstFromStore(
                    inst,
                    inst.types,
                    ty,
                );
                const else_source_ty = try self.freshCloneTypeIntoInstFromStore(
                    inst,
                    inst.types,
                    ty,
                );
                const explicit_result_ty = try self.lowerExecutableTypeFromSolvedIn(
                    inst.types,
                    mono_cache,
                    ty,
                );
                const concrete_result_ty = self.stableConcreteExpectedExecutableType(
                    explicit_result_ty,
                    "specializeExpr(if_ result)",
                ) orelse self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(if_)",
                );
                const branch_default_ty = concrete_result_ty orelse explicit_result_ty;
                const then_lowered = try self.specializeExprWithDefaultTyAndSourceTy(
                    inst,
                    mono_cache,
                    venv,
                    if_expr.then_body,
                    then_expr,
                    then_source_ty,
                    branch_default_ty,
                );
                const else_lowered = try self.specializeExprWithDefaultTyAndSourceTy(
                    inst,
                    mono_cache,
                    venv,
                    if_expr.else_body,
                    else_expr,
                    else_source_ty,
                    branch_default_ty,
                );
                var then_body = then_lowered.expr;
                var else_body = else_lowered.expr;
                const then_ty = self.output.getExpr(then_body).ty;
                const else_ty = self.output.getExpr(else_body).ty;
                const result_ty = if (concrete_result_ty) |concrete|
                    blk_result: {
                        if (self.executableTypesHaveErasedCallableShapeMismatch(then_ty, concrete) or
                            self.executableTypesHaveErasedCallableShapeMismatch(else_ty, concrete))
                        {
                            if (!self.types.equalIds(then_ty, else_ty)) {
                                debugPanic("lambdamono.lower.specializeExpr(if_) branch erased callable executable types disagree");
                            }
                            break :blk_result then_ty;
                        }
                        break :blk_result concrete;
                    }
                else blk_result: {
                    if (!self.types.equalIds(then_ty, else_ty)) {
                        debugPanic("lambdamono.lower.specializeExpr(if_) branch executable types disagree without an explicit result type");
                    }
                    break :blk_result then_ty;
                };
                if (!self.executableTypeIsAbstract(result_ty)) {
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
                }
                break :blk .{ .ty = result_ty, .data = .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                } }, .source_ty = ty };
            },
            .block => |block| blk: {
                const lowered = try self.specializeBlockExpr(
                    inst,
                    mono_cache,
                    venv,
                    block,
                    ty,
                    default_ty,
                );
                break :blk .{
                    .ty = self.output.getExpr(lowered.data.final_expr).ty,
                    .data = .{ .block = lowered.data },
                    .source_ty = lowered.source_ty,
                };
            },
            .tuple => |elems| blk: {
                const source_elems = self.input.store.sliceExprSpan(elems);
                const lowered = try self.allocator.alloc(ast.ExprId, source_elems.len);
                defer self.allocator.free(lowered);

                const source_elem_tys = self.solvedTupleElemTypes(inst.types, ty);
                const concrete_tuple_ty = if (!self.executableTypeIsAbstract(default_ty))
                    self.requireConcreteExecutableType(default_ty, "specializeExpr(tuple)")
                else
                    null;
                const expected_elem_tys = if (concrete_tuple_ty) |tuple_ty|
                    self.tupleElemTypes(tuple_ty)
                else
                    null;
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
                    const elem_expr = self.input.store.getExpr(elem_expr_id);
                    const elem_source_ty = if (source_elem_tys) |elem_tys|
                        elem_tys[i]
                    else
                        try self.instantiatedSourceTypeForExpr(inst, venv, elem_expr_id);
                    const expected_elem_ty = if (expected_elem_tys) |elem_tys|
                        elem_tys[i]
                    else
                        try self.lowerExecutableTypeFromSolvedIn(
                            inst.types,
                            mono_cache,
                            elem_source_ty,
                        );
                    const lowered_elem = try self.specializeExprWithDefaultTy(
                        inst,
                        mono_cache,
                        venv,
                        elem_expr_id,
                        elem_expr,
                        elem_source_ty,
                        expected_elem_ty,
                    );
                    lowered[i] = lowered_elem;
                }
                if (concrete_tuple_ty) |tuple_ty| {
                    const bridge_elem_tys = self.tupleElemTypes(tuple_ty) orelse
                        debugPanic("lambdamono.lower.tuple missing explicit elem types");
                    for (lowered, bridge_elem_tys, 0..) |lowered_elem, expected_elem_ty, i| {
                        if (!self.types.equalIds(self.output.getExpr(lowered_elem).ty, expected_elem_ty)) {
                            lowered[i] = try self.retargetOrEmitExplicitBridgeExpr(lowered_elem, expected_elem_ty);
                        }
                    }
                }
                const lowered_elems = try self.output.addExprSpan(lowered);
                const result_ty = if (concrete_tuple_ty) |tuple_ty|
                    tuple_ty
                else
                    debugPanic("lambdamono.lower.tuple missing explicit executable result type");
                break :blk .{ .ty = result_ty, .data = .{ .tuple = lowered_elems } };
            },
            .tag_payload => |tag_payload| blk: {
                const tag_union = try self.specializeExpr(inst, mono_cache, venv, tag_payload.tag_union);
                const tag_union_ty = self.output.getExpr(tag_union).ty;
                const tag_union_source_ty = try self.instantiatedSourceTypeForExpr(
                    inst,
                    venv,
                    tag_payload.tag_union,
                );
                const payload_source_ty = self.solvedTagPayloadTypeByName(
                    inst.types,
                    tag_union_source_ty,
                    tag_payload.tag_name,
                    tag_payload.payload_index,
                ) orelse self.solvedTagPayloadType(
                    inst.types,
                    tag_union_source_ty,
                    tag_payload.tag_discriminant,
                    tag_payload.payload_index,
                ) orelse ty;
                const exec_tag_info = self.executableTagInfoByDiscriminant(
                    tag_union_ty,
                    tag_payload.tag_discriminant,
                ) orelse
                    self.executableTagInfoByName(tag_union_ty, tag_payload.tag_name) orelse
                    self.singletonExecutableCtorTagInfo(tag_union_ty) orelse
                    {
                        const union_summary = self.debugExecutableTypeSummary(tag_union_ty);
                        defer union_summary.deinit(self.allocator);
                        debugPanicFmt(
                            "lambdamono.lower.tag_payload missing executable tag payload types tag={s} tag_id={d} union={s}",
                            .{
                                self.input.idents.getText(tag_payload.tag_name),
                                @as(u32, tag_payload.tag_name.idx),
                                union_summary.text,
                            },
                        );
                    };
                if (tag_payload.payload_index >= exec_tag_info.args.len) {
                    debugPanic("lambdamono.lower.tag_payload missing payload");
                }
                const payload_ty = exec_tag_info.args[tag_payload.payload_index];
                const expected_payload_ty = self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(tag_payload)",
                );
                const result_ty = if (expected_payload_ty) |expected_ty|
                    if (self.executableTypesHaveErasedCallableShapeMismatch(payload_ty, expected_ty))
                        payload_ty
                    else
                        expected_ty
                else
                    payload_ty;
                const payload_expr = try self.output.addExpr(.{
                    .ty = payload_ty,
                    .data = .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .tag_name = tag_payload.tag_name,
                        .tag_discriminant = exec_tag_info.discriminant,
                        .payload_index = tag_payload.payload_index,
                    } },
                });
                if (self.types.equalIds(payload_ty, result_ty)) {
                    break :blk .{
                        .ty = payload_ty,
                        .data = self.output.getExpr(payload_expr).data,
                        .source_ty = payload_source_ty,
                    };
                }
                const bridged = try self.emitExplicitBridgeExpr(payload_expr, result_ty);
                break :blk .{
                    .ty = result_ty,
                    .data = self.output.getExpr(bridged).data,
                    .source_ty = payload_source_ty,
                };
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
                const result_ty = self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(tuple_access)",
                ) orelse field_ty;
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
                const explicit_result_ty = try self.lowerExecutableTypeFromSolvedIn(
                    inst.types,
                    mono_cache,
                    ty,
                );
                const concrete_list_ty = self.stableConcreteExpectedExecutableType(
                    default_ty,
                    "specializeExpr(list)",
                );
                const expected_elem_ty = if (concrete_list_ty) |list_ty|
                    self.listElemType(list_ty) orelse
                        debugPanic("lambdamono.lower.list missing explicit elem type")
                else if (source_items.len == 0)
                    null
                else if (source_elem_ty) |elem_ty|
                    try self.lowerExecutableTypeFromSolvedIn(inst.types, mono_cache, elem_ty)
                else
                    null;
                const concrete_expected_elem_ty = if (expected_elem_ty) |elem_ty|
                    if (!self.executableTypeIsAbstract(elem_ty))
                        self.requireConcreteExecutableType(elem_ty, "specializeExpr(list elem)")
                    else
                        null
                else
                    null;

                if (source_items.len == 0) {
                    const empty_list_ty = if (concrete_list_ty) |list_ty|
                        list_ty
                    else if (!self.executableTypeIsAbstract(explicit_result_ty) and self.listElemType(explicit_result_ty) != null)
                        self.requireConcreteExecutableType(explicit_result_ty, "specializeExpr(empty list)")
                    else
                        debugPanic("lambdamono.lower.empty list missing explicit executable list type");
                    break :blk .{
                        .ty = empty_list_ty,
                        .data = .{ .list = try self.output.addExprSpan(lowered) },
                    };
                }

                for (source_items, 0..) |item_expr_id, i| {
                    const item_expr = self.input.store.getExpr(item_expr_id);
                    const item_source_ty = if (source_elem_ty) |elem_ty|
                        elem_ty
                    else
                        try self.instantiatedSourceTypeForExpr(inst, venv, item_expr_id);
                    const lowered_item = try self.specializeExprWithDefaultTy(
                        inst,
                        mono_cache,
                        venv,
                        item_expr_id,
                        item_expr,
                        item_source_ty,
                        concrete_expected_elem_ty orelse try self.lowerExecutableTypeFromSolvedIn(
                            inst.types,
                            mono_cache,
                            item_source_ty,
                        ),
                    );
                    lowered[i] = lowered_item;
                }
                if (concrete_expected_elem_ty) |elem_ty| {
                    for (lowered, 0..) |lowered_item, i| {
                        if (!self.types.equalIds(self.output.getExpr(lowered_item).ty, elem_ty)) {
                            lowered[i] = try self.retargetOrEmitExplicitBridgeExpr(lowered_item, elem_ty);
                        }
                    }
                }
                const result_ty = if (concrete_list_ty) |list_ty|
                    list_ty
                else
                    debugPanic("lambdamono.lower.list missing explicit executable result type");
                _ = self.listElemType(result_ty) orelse
                    debugPanic("lambdamono.lower.list missing explicit elem type");
                break :blk .{ .ty = result_ty, .data = .{ .list = try self.output.addExprSpan(lowered) } };
            },
            .return_ => |ret_expr| blk: {
                var lowered_ret = try self.specializeExpr(inst, mono_cache, venv, ret_expr);
                const expected_ret_ty = self.current_return_exec_ty orelse
                    debugPanic("lambdamono.lower.specializeExpr(return_) missing current executable return type");
                if (!self.types.equalIds(self.output.getExpr(lowered_ret).ty, expected_ret_ty)) {
                    lowered_ret = try self.retargetOrEmitExplicitBridgeExpr(lowered_ret, expected_ret_ty);
                }
                break :blk .{
                    .ty = default_ty,
                    .data = .{ .return_ = lowered_ret },
                };
            },
            .runtime_error => |msg| .{ .ty = default_ty, .data = .{ .runtime_error = msg } },
            .for_ => |for_expr| .{ .ty = default_ty, .data = .{ .for_ = try self.specializeForExpr(inst, mono_cache, venv, for_expr) } },
        };
        return .{
            .expr = try self.output.addExpr(.{ .ty = specialized.ty, .data = specialized.data }),
            .source_ty = specialized.source_ty orelse ty,
        };
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
        call_sig: type_mod.CallableSig,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.internResolved(.{ .erased_fn = .{
            .capture = capture_ty,
            .call = try self.types.dupeCallableSig(call_sig),
        } });
    }

    fn specializeBoxBoundaryArgExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        op: base.LowLevel,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, expr_id);
        const expr = self.input.store.getExpr(expr_id);
        const default_ty = if (op == .box_box and inst.types.maybeLambdaRepr(source_ty) != null)
            try self.freshAbstractExecutableType()
        else
            try self.explicitValueExecutableTypeForExpr(
                inst.types,
                mono_cache,
                venv,
                expr_id,
                source_ty,
            );
        const lowered_expr = (try self.specializeExprWithDefaultTyAndSourceTy(
            inst,
            mono_cache,
            venv,
            expr_id,
            expr,
            source_ty,
            default_ty,
        )).expr;
        const lowered = try self.lowerBoxBoundaryExpr(inst, mono_cache, venv, source_ty, lowered_expr);
        switch (op) {
            .box_box => {
                _ = self.requireConcreteExecutableType(
                    self.output.getExpr(lowered).ty,
                    "specializeBoxBoundaryArgExpr(box_box)",
                );
                return lowered;
            },
            .box_unbox => {
                const arg_ty = self.requireConcreteExecutableType(
                    self.output.getExpr(lowered).ty,
                    "specializeBoxBoundaryArgExpr(box_unbox)",
                );
                if (self.types.getTypePreservingNominal(arg_ty) != .box) {
                    debugPanic("lambdamono.lower.specializeBoxBoundaryArgExpr box_unbox expected boxed executable arg");
                }
                return lowered;
            },
            else => unreachable,
        }
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
                const erased_capture_ty = forced_capture_ty orelse
                    debugPanic("lambdamono.lower.lowerConcreteCallableAsErased missing explicit erased capture type");
                const erased_call_sig = self.requireExecutableCallableSig(
                    callable.ty,
                    "lowerConcreteCallableAsErased",
                );
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
                    .ty = try self.makeErasedFnType(erased_capture_ty, erased_call_sig),
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

    fn executableTypesHaveErasedCallableShapeMismatch(
        self: *Lowerer,
        actual_ty: type_mod.TypeId,
        expected_ty: type_mod.TypeId,
    ) bool {
        if (self.types.equalIds(actual_ty, expected_ty)) return false;
        const actual = self.types.getTypePreservingNominal(actual_ty);
        const expected = self.types.getTypePreservingNominal(expected_ty);
        if (actual == .erased_fn and expected == .erased_fn) return true;
        if (actual == .box and expected == .box) {
            return self.executableTypesHaveErasedCallableShapeMismatch(actual.box, expected.box);
        }
        if (actual == .list and expected == .list) {
            return self.executableTypesHaveErasedCallableShapeMismatch(actual.list, expected.list);
        }
        if (actual == .tuple and expected == .tuple) {
            if (actual.tuple.len != expected.tuple.len) return false;
            for (actual.tuple, expected.tuple) |actual_elem, expected_elem| {
                if (self.executableTypesHaveErasedCallableShapeMismatch(actual_elem, expected_elem)) return true;
            }
            return false;
        }
        if (actual == .record and expected == .record) {
            if (actual.record.fields.len != expected.record.fields.len) return false;
            for (actual.record.fields, expected.record.fields) |actual_field, expected_field| {
                if (actual_field.name != expected_field.name) return false;
                if (self.executableTypesHaveErasedCallableShapeMismatch(actual_field.ty, expected_field.ty)) return true;
            }
            return false;
        }
        if (actual == .tag_union and expected == .tag_union) {
            if (actual.tag_union.tags.len != expected.tag_union.tags.len) return false;
            for (actual.tag_union.tags, expected.tag_union.tags) |actual_tag, expected_tag| {
                if (!std.meta.eql(actual_tag.name, expected_tag.name)) return false;
                if (actual_tag.args.len != expected_tag.args.len) return false;
                for (actual_tag.args, expected_tag.args) |actual_arg, expected_arg| {
                    if (self.executableTypesHaveErasedCallableShapeMismatch(actual_arg, expected_arg)) return true;
                }
            }
            return false;
        }
        return false;
    }

    fn emitExplicitBridgeExpr(
        self: *Lowerer,
        expr_id: ast.ExprId,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.output.getExpr(expr_id);
        const actual_ty = expr.ty;
        if (self.types.containsAbstractLeaf(expected_ty)) {
            debugPanicFmt(
                "lambdamono.lower.emitExplicitBridgeExpr abstract expected executable type expr={d} actual_ty={d} expected_ty={d} actual_tag={s} expected_tag={s}",
                .{
                    @intFromEnum(expr_id),
                    @intFromEnum(actual_ty),
                    @intFromEnum(expected_ty),
                    @tagName(self.types.getTypePreservingNominal(actual_ty)),
                    @tagName(self.types.getTypePreservingNominal(expected_ty)),
                },
            );
        }
        if (actual_ty == expected_ty) return expr_id;
        if (self.executableTypesHaveErasedCallableShapeMismatch(actual_ty, expected_ty)) {
            const actual_summary = self.debugExecutableTypeSummary(actual_ty);
            defer actual_summary.deinit(self.allocator);
            const expected_summary = self.debugExecutableTypeSummary(expected_ty);
            defer expected_summary.deinit(self.allocator);
            debugPanicFmt(
                "lambdamono.lower.emitExplicitBridgeExpr erased callable shape mismatch expr={d} actual={s} expected={s}",
                .{ @intFromEnum(expr_id), actual_summary.text, expected_summary.text },
            );
        }
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

    const AuthoritativeCallableValue = struct {
        expr: ast.ExprId,
        ty: type_mod.TypeId,
    };

    fn isExecutableCallableType(self: *const Lowerer, ty: type_mod.TypeId) bool {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .erased_fn => true,
            .tag_union => |tag_union| self.tagUnionIsInternalLambdaSet(tag_union.tags),
            else => false,
        };
    }

    fn authoritativeCallableValue(
        self: *Lowerer,
        expr_id: ast.ExprId,
    ) AuthoritativeCallableValue {
        var current = expr_id;
        while (true) {
            const expr = self.output.getExpr(current);
            switch (expr.data) {
                .bridge => |inner| {
                    if (self.isExecutableCallableType(expr.ty) and !self.executableTypeIsAbstract(expr.ty)) {
                        return .{ .expr = current, .ty = expr.ty };
                    }
                    const inner_ty = self.output.getExpr(inner).ty;
                    if (self.isExecutableCallableType(inner_ty)) {
                        current = inner;
                        continue;
                    }
                },
                else => {},
            }
            return .{ .expr = current, .ty = expr.ty };
        }
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
            .when => |when_expr| {
                const branch_ids = self.output.sliceBranchSpan(when_expr.branches);
                const updated_branches = try self.allocator.alloc(ast.Branch, branch_ids.len);
                defer self.allocator.free(updated_branches);
                for (branch_ids, 0..) |branch_id, i| {
                    const branch = self.output.getBranch(branch_id);
                    const updated_body = if (self.types.equalIds(self.output.getExpr(branch.body).ty, expected_ty))
                        branch.body
                    else if (try self.retargetExprToExpectedExecutableType(branch.body, expected_ty))
                        branch.body
                    else
                        return false;
                    updated_branches[i] = .{
                        .pat = branch.pat,
                        .body = updated_body,
                    };
                }
                expr.ty = expected_ty;
                expr.data = .{ .when = .{
                    .cond = when_expr.cond,
                    .branches = try self.output.addBranchSpan(updated_branches),
                } };
                self.output.exprs.items[expr_idx] = expr;
                return true;
            },
            .if_ => |if_expr| {
                const updated_then = if (self.types.equalIds(self.output.getExpr(if_expr.then_body).ty, expected_ty))
                    if_expr.then_body
                else if (try self.retargetExprToExpectedExecutableType(if_expr.then_body, expected_ty))
                    if_expr.then_body
                else
                    return false;
                const updated_else = if (self.types.equalIds(self.output.getExpr(if_expr.else_body).ty, expected_ty))
                    if_expr.else_body
                else if (try self.retargetExprToExpectedExecutableType(if_expr.else_body, expected_ty))
                    if_expr.else_body
                else
                    return false;
                expr.ty = expected_ty;
                expr.data = .{ .if_ = .{
                    .cond = if_expr.cond,
                    .then_body = updated_then,
                    .else_body = updated_else,
                } };
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
        const capture_count = self.exactCallableCaptureCount(inst.types, requested_ty, symbol);
        if (forced_capture_ty != null and capture_count == 0) {
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
                } else if (capture_count != 0) {
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
        if (capture_ty == null and capture_count != 0) {
            debugPanic("lambdamono.lower.makeErasedPackedFnExpr missing capture type for captured lambda");
        }
        const erased_call_sig = self.requireExecutableCallableSig(
            try self.lowerErasedBoundaryExecutableTypeIn(inst.types, mono_cache, requested_ty),
            "makeErasedPackedFnExpr",
        );
        const specialized_symbol = try self.queueCallableSpecializationWithExplicitSignature(
            inst.types,
            mono_cache,
            symbol,
            .erased_boundary,
            requested_ty,
            capture_ty,
            null,
        );
        return try self.output.addExpr(.{
            .ty = try self.makeErasedFnType(capture_ty, erased_call_sig),
            .data = .{ .packed_fn = .{
                .lambda = specialized_symbol,
                .captures = capture_expr_opt,
                .capture_ty = capture_ty,
            } },
        });
    }

    fn lowerExactExecutableCallableAsErased(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
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
                const capture_args = self.output.sliceExprSpan(tag.args);
                const capture_expr = switch (capture_args.len) {
                    0 => null,
                    1 => capture_args[0],
                    else => debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected at most one capture payload"),
                };
                break :blk try self.makeErasedPackedFnExpr(
                    inst,
                    mono_cache,
                    source_ty,
                    lambda_symbol,
                    capture_expr,
                    forced_capture_ty,
                );
            },
            else => blk: {
                const erased_capture_ty = forced_capture_ty orelse
                    debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased missing explicit erased capture type");
                const erased_call_sig = self.requireExecutableCallableSig(
                    try self.lowerErasedBoundaryExecutableTypeIn(inst.types, mono_cache, source_ty),
                    "lowerExactExecutableCallableAsErased",
                );
                const branches = try self.allocator.alloc(ast.Branch, tag_union.tags.len);
                defer self.allocator.free(branches);
                for (tag_union.tags, 0..) |tag_info, i| {
                    const lambda_symbol = switch (tag_info.name) {
                        .lambda => |lambda_symbol| lambda_symbol,
                        .ctor => debugPanic("lambdamono.lower.lowerExactExecutableCallableAsErased expected lambda tag"),
                    };
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
                            .body = try self.makeErasedPackedFnExpr(
                                inst,
                                mono_cache,
                                source_ty,
                                lambda_symbol,
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
                            .body = try self.makeErasedPackedFnExpr(
                                inst,
                                mono_cache,
                                source_ty,
                                lambda_symbol,
                                capture_expr,
                                erased_capture_ty,
                            ),
                        };
                    }
                }
                break :blk try self.output.addExpr(.{
                    .ty = try self.makeErasedFnType(erased_capture_ty, erased_call_sig),
                    .data = .{ .when = .{
                        .cond = callable_id,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                });
            },
        };
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

    fn lowerVarExprToExpectedExecutableType(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        symbol: Symbol,
        source_ty: TypeVarId,
        current_exec_ty: type_mod.TypeId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const var_expr = try self.makeVarExpr(current_exec_ty, symbol);
        const lowered_expr = if (self.executableTypeIsAbstract(expected_exec_ty) or
            self.types.equalIds(current_exec_ty, expected_exec_ty) or
            self.executableTypesHaveErasedCallableShapeMismatch(current_exec_ty, expected_exec_ty))
            var_expr
        else
            try self.bridgeExprAtSolvedTypeToExpectedExecutableType(
                inst,
                mono_cache,
                source_ty,
                var_expr,
                expected_exec_ty,
            );
        const lowered = self.output.getExpr(lowered_expr);
        return .{
            .ty = lowered.ty,
            .data = lowered.data,
            .source_ty = source_ty,
        };
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

    fn unifyExactCallableSourceType(
        self: *Lowerer,
        inst: *InstScope,
        exact_fn_ty: TypeVarId,
        current_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        const source_fn_ty = try self.freshCloneTypeIntoInstFromStore(
            inst,
            &self.input.types,
            exact_fn_ty,
        );
        try self.unifyIn(inst.types, source_fn_ty, current_ty);
        return source_fn_ty;
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
        default_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const solved_types = inst.types;
        var refined_source_ty = instantiated_ty;
        if (self.isHostedTopLevelSymbol(symbol)) {
            const hosted_exec_ty = blk: {
                if (!self.executableTypeIsAbstract(default_ty)) {
                    const concrete = self.requireConcreteExecutableType(
                        default_ty,
                        "specializeVarExpr(hosted)",
                    );
                    if (self.types.getTypePreservingNominal(concrete) == .erased_fn) {
                        break :blk concrete;
                    }
                }
                break :blk self.requireConcreteExecutableType(
                    try self.lowerErasedBoundaryExecutableTypeIn(
                        solved_types,
                        mono_cache,
                        refined_source_ty,
                    ),
                    "specializeVarExpr(hosted derived)",
                );
            };
            if (self.types.getTypePreservingNominal(hosted_exec_ty) != .erased_fn) {
                debugPanic("lambdamono.lower.specializeVarExpr hosted symbol expected erased callable executable type");
            }
            const hosted_call_sig = self.requireExecutableCallableSig(
                hosted_exec_ty,
                "specializeVarExpr(hosted)",
            );
            const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
                solved_types,
                symbol,
                .erased_boundary,
                instantiated_ty,
                null,
                null,
                hosted_call_sig.args,
                hosted_call_sig.ret,
            );
            if (specialized.exec_capture_ty != null) {
                debugPanic("lambdamono.lower.specializeVarExpr hosted function had captures");
            }
            return .{
                .ty = hosted_exec_ty,
                .data = .{ .packed_fn = .{
                    .lambda = specialized.symbol,
                    .captures = null,
                    .capture_ty = null,
                } },
                .source_ty = refined_source_ty,
            };
        }

        if (specializations.lookupFnExact(self.fenv, symbol)) |entry| {
            refined_source_ty = try self.unifyExactCallableSourceType(inst, entry.fn_ty, refined_source_ty);
            return try self.lowerExactCallableVarExpr(
                inst,
                mono_cache,
                venv,
                symbol,
                entry.capture_symbols,
                refined_source_ty,
                default_ty,
            );
        }

        if (self.lookupEnvEntry(venv, symbol)) |entry| {
            if (entry.exact_fn_symbol) |exact_symbol| {
                const exact_entry = specializations.lookupFnExact(self.fenv, exact_symbol) orelse
                    debugPanic("lambdamono.lower.specializeVarExpr env exact callable missing source definition");
                refined_source_ty = try self.unifyExactCallableSourceType(inst, exact_entry.fn_ty, refined_source_ty);
                if (self.envHasAllCaptureSymbols(venv, exact_entry.capture_symbols)) {
                    return try self.lowerExactCallableVarExpr(
                        inst,
                        mono_cache,
                        venv,
                        exact_symbol,
                        exact_entry.capture_symbols,
                        refined_source_ty,
                        default_ty,
                    );
                }
                const current_exec_ty = try self.currentEnvEntryExecutableType(
                    solved_types,
                    mono_cache,
                    entry,
                    "specializeVarExpr(env_exact)",
                );
                return try self.lowerVarExprToExpectedExecutableType(
                    inst,
                    mono_cache,
                    symbol,
                    refined_source_ty,
                    current_exec_ty,
                    default_ty,
                );
            }
            const current_exec_ty = try self.currentEnvEntryExecutableType(
                solved_types,
                mono_cache,
                entry,
                "specializeVarExpr(env)",
            );
            return try self.lowerVarExprToExpectedExecutableType(
                inst,
                mono_cache,
                symbol,
                refined_source_ty,
                current_exec_ty,
                default_ty,
            );
        }

        if (self.exactCallableSymbolForBinding(symbol)) |exact_symbol| {
            if (exact_symbol != symbol) {
                const exact_entry = specializations.lookupFnExact(self.fenv, exact_symbol) orelse
                    debugPanic("lambdamono.lower.specializeVarExpr top-level exact callable alias missing source definition");
                refined_source_ty = try self.unifyExactCallableSourceType(inst, exact_entry.fn_ty, refined_source_ty);
                if (self.envHasAllCaptureSymbols(venv, exact_entry.capture_symbols)) {
                    return try self.lowerExactCallableVarExpr(
                        inst,
                        mono_cache,
                        venv,
                        exact_symbol,
                        exact_entry.capture_symbols,
                        refined_source_ty,
                        default_ty,
                    );
                }
            }
        }

        if (self.lookupTopLevelValueType(symbol)) |top_level_ty| {
            return try self.lowerVarExprToExpectedExecutableType(
                inst,
                mono_cache,
                symbol,
                refined_source_ty,
                top_level_ty,
                default_ty,
            );
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

    fn lowerExactCallableVarExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        exact_symbol: Symbol,
        capture_symbols: []const Symbol,
        refined_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const solved_types = inst.types;
        const current_capture = try self.currentCapturePayloadFromSymbols(
            solved_types,
            mono_cache,
            capture_symbols,
            venv,
        );
        defer current_capture.deinit(self.allocator);
        if (self.executableTypeIsAbstract(expected_exec_ty)) {
            const source_repr = solved_types.lambdaRepr(refined_source_ty);
            const fn_shape = solved_types.fnShape(refined_source_ty);
            const source_arg_tys = solved_types.sliceTypeVarSpan(fn_shape.args);
            const lowered_arg_tys = try self.allocator.alloc(type_mod.TypeId, source_arg_tys.len);
            defer self.allocator.free(lowered_arg_tys);
            for (source_arg_tys, 0..) |arg_ty, i| {
                lowered_arg_tys[i] = switch (source_repr) {
                    .lset => try self.lowerExecutableTypeFromSolvedIn(
                        solved_types,
                        mono_cache,
                        arg_ty,
                    ),
                    .erased => try self.lowerErasedBoundaryExecutableTypeIn(
                        solved_types,
                        mono_cache,
                        arg_ty,
                    ),
                };
            }
            const abstract_call_sig: type_mod.CallableSig = .{
                .args = try self.types.dupeTypeIds(lowered_arg_tys),
                .ret = switch (source_repr) {
                    .lset => try self.lowerExecutableTypeFromSolvedIn(
                        solved_types,
                        mono_cache,
                        fn_shape.ret,
                    ),
                    .erased => try self.lowerErasedBoundaryExecutableTypeIn(
                        solved_types,
                        mono_cache,
                        fn_shape.ret,
                    ),
                },
            };
            if (source_repr == .erased) {
                const capture_exact_symbols = try self.captureExactSymbolsFromEnv(capture_symbols, current_capture.env);
                defer if (capture_exact_symbols) |symbols| self.allocator.free(symbols);
                var authoritative_capture_ty = if (capture_symbols.len == 0)
                    null
                else
                    current_capture.ty orelse
                        debugPanic("lambdamono.lower.lowerExactCallableVarExpr missing current erased capture payload type for abstract exact callable");
                if (authoritative_capture_ty) |capture_ty| {
                    if (self.isEmptyRecordType(capture_ty)) {
                        authoritative_capture_ty = null;
                    }
                }
                if (capture_symbols.len != 0 and authoritative_capture_ty == null) {
                    debugPanic("lambdamono.lower.lowerExactCallableVarExpr abstract erased callable dropped non-empty exact capture payload");
                }
                const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
                    solved_types,
                    exact_symbol,
                    .erased_boundary,
                    refined_source_ty,
                    authoritative_capture_ty,
                    capture_exact_symbols,
                    abstract_call_sig.args,
                    abstract_call_sig.ret,
                );
                const authoritative_call_sig: type_mod.CallableSig = .{
                    .args = try self.types.dupeTypeIds(specialized.exec_args_tys),
                    .ret = specialized.exec_ret_ty,
                };
                defer if (authoritative_call_sig.args.len != 0) self.allocator.free(authoritative_call_sig.args);
                var capture_expr: ?ast.ExprId = if (authoritative_capture_ty) |capture_ty|
                    try self.specializeCaptureRecord(current_capture.env, capture_ty)
                else
                    null;
                if (capture_expr) |captures_value| {
                    const capture_box_ty = try self.types.internResolved(.{ .box = authoritative_capture_ty.? });
                    capture_expr = try self.makeLowLevelExpr(capture_box_ty, .box_box, &.{captures_value});
                }
                if (specialized.exec_capture_ty != authoritative_capture_ty) {
                    debugPanic("lambdamono.lower.lowerExactCallableVarExpr abstract erased specialization capture type mismatch");
                }
                const authoritative_exec_ty = try self.makeErasedFnType(
                    authoritative_capture_ty,
                    authoritative_call_sig,
                );
                return .{
                    .ty = authoritative_exec_ty,
                    .data = .{ .packed_fn = .{
                        .lambda = specialized.symbol,
                        .captures = capture_expr,
                        .capture_ty = authoritative_capture_ty,
                    } },
                    .source_ty = refined_source_ty,
                };
            }
            if (capture_symbols.len == 0) {
                return .{
                    .ty = try self.makeSingletonExecutableLambdaType(exact_symbol, null, abstract_call_sig),
                    .data = .{ .tag = .{
                        .name = lower_type.lambdaTagKey(exact_symbol),
                        .discriminant = 0,
                        .args = ast.Span(ast.ExprId).empty(),
                    } },
                    .source_ty = refined_source_ty,
                };
            }
            const capture_ty = current_capture.ty orelse
                debugPanic("lambdamono.lower.lowerExactCallableVarExpr missing capture payload for captured exact callable");
            const capture_record = try self.specializeCaptureRecord(current_capture.env, capture_ty);
            const args = try self.allocator.alloc(ast.ExprId, 1);
            defer self.allocator.free(args);
            args[0] = capture_record;
            return .{
                .ty = try self.makeSingletonExecutableLambdaType(exact_symbol, capture_ty, abstract_call_sig),
                .data = .{ .tag = .{
                    .name = lower_type.lambdaTagKey(exact_symbol),
                    .discriminant = 0,
                    .args = try self.output.addExprSpan(args),
                } },
                .source_ty = refined_source_ty,
            };
        }
        const concrete_expected_exec_ty = self.requireConcreteExecutableType(
            expected_exec_ty,
            "lowerExactCallableVarExpr",
        );
        const expected_call_sig = self.requireExecutableCallableSig(
            concrete_expected_exec_ty,
            "lowerExactCallableVarExpr",
        );
        const capture_exact_symbols = try self.captureExactSymbolsFromEnv(capture_symbols, current_capture.env);
        defer if (capture_exact_symbols) |symbols| self.allocator.free(symbols);
        switch (self.types.getTypePreservingNominal(concrete_expected_exec_ty)) {
            .tag_union => |tag_union| {
                if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    debugPanic("lambdamono.lower.lowerExactCallableVarExpr expected executable callable type");
                }
                switch (solved_types.lambdaRepr(refined_source_ty)) {
                    .lset => {},
                    .erased => debugPanic("lambdamono.lower.lowerExactCallableVarExpr erased callable lowered to natural executable type"),
                }
                const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
                    solved_types,
                    exact_symbol,
                    .natural,
                    refined_source_ty,
                    current_capture.ty,
                    capture_exact_symbols,
                    expected_call_sig.args,
                    expected_call_sig.ret,
                );
                const authoritative_call_sig: type_mod.CallableSig = .{
                    .args = try self.types.dupeTypeIds(specialized.exec_args_tys),
                    .ret = specialized.exec_ret_ty,
                };
                defer if (authoritative_call_sig.args.len != 0) self.allocator.free(authoritative_call_sig.args);
                if (capture_symbols.len == 0) {
                    if (specialized.exec_capture_ty != null) {
                        debugPanic("lambdamono.lower.lowerExactCallableVarExpr unexpected natural capture payload for captureless exact callable");
                    }
                    return .{
                        .ty = try self.makeSingletonExecutableLambdaType(exact_symbol, null, authoritative_call_sig),
                        .data = .{ .tag = .{
                            .name = lower_type.lambdaTagKey(exact_symbol),
                            .discriminant = 0,
                            .args = ast.Span(ast.ExprId).empty(),
                        } },
                        .source_ty = refined_source_ty,
                    };
                }
                const precise_capture_ty = specialized.exec_capture_ty orelse
                    debugPanic("lambdamono.lower.lowerExactCallableVarExpr missing natural capture payload type");
                const precise_ty = try self.makeSingletonExecutableLambdaType(
                    exact_symbol,
                    precise_capture_ty,
                    authoritative_call_sig,
                );
                const capture_record = try self.specializeCaptureRecord(current_capture.env, precise_capture_ty);
                const args = try self.allocator.alloc(ast.ExprId, 1);
                defer self.allocator.free(args);
                args[0] = capture_record;
                return .{
                    .ty = precise_ty,
                    .data = .{ .tag = .{
                        .name = lower_type.lambdaTagKey(exact_symbol),
                        .discriminant = 0,
                        .args = try self.output.addExprSpan(args),
                    } },
                    .source_ty = refined_source_ty,
                };
            },
            .erased_fn => {
                var authoritative_capture_ty = self.erasedFnCaptureType(concrete_expected_exec_ty);
                if (capture_symbols.len != 0) {
                    authoritative_capture_ty = current_capture.ty orelse
                        debugPanic("lambdamono.lower.lowerExactCallableVarExpr missing current erased capture payload type for captured exact callable");
                }
                if (authoritative_capture_ty) |capture_ty| {
                    if (self.isEmptyRecordType(capture_ty)) {
                        authoritative_capture_ty = null;
                    }
                }
                if (capture_symbols.len != 0 and authoritative_capture_ty == null) {
                    debugPanic("lambdamono.lower.lowerExactCallableVarExpr erased callable dropped non-empty exact capture payload");
                }
                const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
                    solved_types,
                    exact_symbol,
                    .erased_boundary,
                    refined_source_ty,
                    authoritative_capture_ty,
                    capture_exact_symbols,
                    expected_call_sig.args,
                    expected_call_sig.ret,
                );
                const authoritative_call_sig: type_mod.CallableSig = .{
                    .args = try self.types.dupeTypeIds(specialized.exec_args_tys),
                    .ret = specialized.exec_ret_ty,
                };
                defer if (authoritative_call_sig.args.len != 0) self.allocator.free(authoritative_call_sig.args);
                var capture_expr: ?ast.ExprId = if (authoritative_capture_ty) |capture_ty|
                    try self.specializeCaptureRecord(current_capture.env, capture_ty)
                else
                    null;
                if (capture_expr) |captures_value| {
                    const capture_box_ty = try self.types.internResolved(.{ .box = authoritative_capture_ty.? });
                    capture_expr = try self.makeLowLevelExpr(capture_box_ty, .box_box, &.{captures_value});
                }
                if (specialized.exec_capture_ty != authoritative_capture_ty) {
                    debugPanic("lambdamono.lower.lowerExactCallableVarExpr erased specialization capture type mismatch");
                }
                const authoritative_exec_ty = try self.makeErasedFnType(
                    authoritative_capture_ty,
                    authoritative_call_sig,
                );
                return .{
                    .ty = authoritative_exec_ty,
                    .data = .{ .packed_fn = .{
                        .lambda = specialized.symbol,
                        .captures = capture_expr,
                        .capture_ty = authoritative_capture_ty,
                    } },
                    .source_ty = refined_source_ty,
                };
            },
            else => debugPanic("lambdamono.lower.lowerExactCallableVarExpr expected callable executable type"),
        }
    }

    fn explicitValueExecutableTypeForExpr(
        self: *Lowerer,
        solved_types: *solved.Type.Store,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        source_ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const expr = self.input.store.getExpr(expr_id);
        return switch (expr.data) {
            .var_ => |symbol| {
                if (self.lookupEnvEntry(venv, symbol)) |entry| {
                    return try self.currentEnvEntryExecutableType(
                        solved_types,
                        mono_cache,
                        entry,
                        "explicitValueExecutableTypeForExpr",
                    );
                }
                if (self.lookupTopLevelValueType(symbol)) |top_level_ty| {
                    return top_level_ty;
                }
                return try self.lowerExecutableTypeFromSolvedIn(
                    solved_types,
                    mono_cache,
                    source_ty,
                );
            },
            else => try self.lowerExecutableTypeFromSolvedIn(
                solved_types,
                mono_cache,
                source_ty,
            ),
        };
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
        source_ty: ?TypeVarId = null,
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
        call_sig: type_mod.CallableSig,
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
            .tag_union = .{
                .tags = tags,
                .call = try self.types.dupeCallableSig(call_sig),
            },
        }));
    }

    fn captureBindingsFromCaptures(
        self: *Lowerer,
        _: *solved.Type.Store,
        captures: []const solved.Type.Capture,
        capture_record_ty: type_mod.TypeId,
        capture_exact_symbols: ?[]const Symbol,
    ) std.mem.Allocator.Error![]EnvEntry {
        if (capture_exact_symbols) |symbols| {
            if (symbols.len != captures.len) {
                debugPanic("lambdamono.lower.captureBindingsFromCaptures queued capture exact-symbol arity mismatch");
            }
        }
        const out = try self.allocator.alloc(EnvEntry, captures.len);
        for (captures, 0..) |capture, i| {
            const capture_name = self.input.symbols.get(capture.symbol).name;
            const field_info = self.recordFieldByName(capture_record_ty, capture_name) orelse
                debugPanic("lambdamono.lower.captureBindingsFromCaptures missing capture field");
            const pending_exact = if (capture_exact_symbols) |symbols|
                if (symbols[i].isNone()) null else symbols[i]
            else
                null;
            const exact_fn_symbol = pending_exact orelse
                self.exactCallableSymbolForBinding(capture.symbol);
            out[i] = .{
                .symbol = capture.symbol,
                .ty = capture.ty,
                .exec_ty = field_info.ty,
                .exact_fn_symbol = exact_fn_symbol,
            };
        }
        return out;
    }

    fn captureExactSymbolsFromEnv(
        self: *Lowerer,
        capture_symbols: []const Symbol,
        capture_env: []const EnvEntry,
    ) std.mem.Allocator.Error!?[]const Symbol {
        if (capture_symbols.len == 0) return null;

        const exact_symbols = try self.allocator.alloc(Symbol, capture_symbols.len);
        errdefer self.allocator.free(exact_symbols);

        var any_exact = false;
        for (capture_symbols, 0..) |capture_symbol, i| {
            const entry = self.lookupEnvEntry(capture_env, capture_symbol) orelse
                debugPanic("lambdamono.lower.captureExactSymbolsFromEnv missing capture binding");
            const exact_symbol = entry.exact_fn_symbol orelse Symbol.none;
            exact_symbols[i] = exact_symbol;
            if (!exact_symbol.isNone()) {
                any_exact = true;
            }
        }

        if (!any_exact) {
            self.allocator.free(exact_symbols);
            return null;
        }

        return exact_symbols;
    }

    fn exactCallableSymbolForBinding(self: *const Lowerer, symbol: Symbol) ?Symbol {
        if (specializations.lookupFnExact(self.fenv, symbol) != null) return symbol;
        if (self.input.exact_callable_aliases.get(symbol)) |exact| return exact;
        const root_symbol = self.rootSourceSymbol(symbol);
        if (specializations.lookupFnExact(self.fenv, root_symbol) != null) return root_symbol;
        if (self.input.exact_callable_aliases.get(root_symbol)) |exact| return exact;
        const source = self.top_level_values.get(root_symbol) orelse return null;
        return switch (source) {
            .fn_, .hosted_fn => root_symbol,
            .val => |expr_id| self.directCallableSymbol(self.input.store.getExpr(expr_id)),
            .run => null,
        };
    }

    fn exactCallableCaptureCount(
        self: *const Lowerer,
        solved_types: *solved.Type.Store,
        requested_ty: TypeVarId,
        symbol: Symbol,
    ) usize {
        if (solved_types.maybeLambdaRepr(requested_ty)) |repr| switch (repr) {
            .lset => |members| {
                for (members) |member| {
                    if (member.symbol == symbol) {
                        return solved_types.sliceCaptures(member.captures).len;
                    }
                }
            },
            .erased => {},
        };
        if (specializations.lookupFnExact(self.fenv, symbol)) |entry| {
            return entry.capture_symbols.len;
        }
        debugPanic("lambdamono.lower.exactCallableCaptureCount missing exact callable capture metadata");
    }

    fn isHostedTopLevelSymbol(self: *const Lowerer, symbol: Symbol) bool {
        return switch (self.top_level_values.get(self.rootSourceSymbol(symbol)) orelse return false) {
            .hosted_fn => true,
            .fn_, .val, .run => false,
        };
    }

    fn exactCallableReprMode(self: *const Lowerer, symbol: Symbol) specializations.Pending.ReprMode {
        return if (self.isHostedTopLevelSymbol(symbol)) .erased_boundary else .natural;
    }

    fn exactCallableSymbolForBoundExpr(
        self: *const Lowerer,
        bind_symbol: Symbol,
        body_expr_id: solved.Ast.ExprId,
    ) ?Symbol {
        if (self.input.exact_callable_aliases.get(bind_symbol)) |exact| return exact;
        if (self.exactCallableSymbolForBinding(bind_symbol)) |exact| return exact;
        return self.directCallableSymbol(self.input.store.getExpr(body_expr_id));
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

    fn envHasAllCaptureSymbols(
        self: *const Lowerer,
        venv: []const EnvEntry,
        capture_symbols: []const Symbol,
    ) bool {
        for (capture_symbols) |capture_symbol| {
            if (self.lookupEnvEntry(venv, capture_symbol) == null) return false;
        }
        return true;
    }

    fn directCallableSymbol(self: *const Lowerer, expr: solved.Ast.Expr) ?Symbol {
        return switch (expr.data) {
            .var_ => |symbol| if (symbol.isNone())
                null
            else
                self.exactCallableSymbolForBinding(symbol),
            .call => null,
            .low_level => null,
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
                if (entry.exact_fn_symbol != null) {
                    const entry_id = inst.types.unlinkPreservingNominalConst(entry.ty);
                    switch (inst.types.getNode(entry_id)) {
                        .content => |content| switch (content) {
                            .func => {},
                            else => break :blk entry.ty,
                        },
                        else => break :blk entry.ty,
                    }
                    break :blk entry.ty;
                }
                break :blk entry.ty;
            } else try self.cloneInstType(inst, expr.ty),
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
                return self.solvedTagPayloadTypeByName(
                    inst.types,
                    union_ty,
                    tag_payload.tag_name,
                    tag_payload.payload_index,
                ) orelse self.solvedTagPayloadType(
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

    fn explicitTagArgSourceTypesFromExprs(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        arg_expr_ids: []const solved.Ast.ExprId,
    ) std.mem.Allocator.Error![]TypeVarId {
        const arg_tys = try self.allocator.alloc(TypeVarId, arg_expr_ids.len);
        for (arg_expr_ids, 0..) |arg_expr_id, i| {
            arg_tys[i] = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_id);
        }
        return arg_tys;
    }

    fn explicitTagArgSourceTypesFromPats(
        self: *Lowerer,
        inst: *InstScope,
        arg_pat_ids: []const solved.Ast.PatId,
    ) std.mem.Allocator.Error![]TypeVarId {
        const arg_tys = try self.allocator.alloc(TypeVarId, arg_pat_ids.len);
        for (arg_pat_ids, 0..) |arg_pat_id, i| {
            arg_tys[i] = try self.cloneInstType(inst, self.input.store.getPat(arg_pat_id).ty);
        }
        return arg_tys;
    }

    fn tagArgSourceTypesForExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        union_source_ty: TypeVarId,
        tag_name: base.Ident.Idx,
        arg_expr_ids: []const solved.Ast.ExprId,
    ) std.mem.Allocator.Error![]TypeVarId {
        if (self.solvedTagInfoByName(inst.types, union_source_ty, tag_name)) |tag_info| {
            if (tag_info.args.len != arg_expr_ids.len) {
                debugPanic("lambdamono.lower.tagArgSourceTypesForExpr tag arg count mismatch");
            }
            return self.dupeTypeVarIds(tag_info.args);
        }
        return self.explicitTagArgSourceTypesFromExprs(inst, venv, arg_expr_ids);
    }

    fn tagArgSourceTypesForPat(
        self: *Lowerer,
        inst: *InstScope,
        union_source_ty: TypeVarId,
        tag_name: base.Ident.Idx,
        arg_pat_ids: []const solved.Ast.PatId,
    ) std.mem.Allocator.Error![]TypeVarId {
        if (self.solvedTagInfoByName(inst.types, union_source_ty, tag_name)) |tag_info| {
            if (tag_info.args.len != arg_pat_ids.len) {
                debugPanic("lambdamono.lower.tagArgSourceTypesForPat tag arg count mismatch");
            }
            return self.dupeTypeVarIds(tag_info.args);
        }
        return self.explicitTagArgSourceTypesFromPats(inst, arg_pat_ids);
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
            .var_ => |symbol| if (self.lookupEnvEntry(venv, symbol)) |entry|
                entry.ty
            else
                current_source_ty,
            .int_lit,
            .dec_lit,
            => current_source_ty,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .runtime_error,
            => try self.cloneInstType(inst, expr.ty),
            .list => |items_span| blk: {
                const items = self.input.store.sliceExprSpan(items_span);
                if (items.len == 0) break :blk current_source_ty;
                const elem_source_ty = self.solvedListElemType(inst.types, current_source_ty) orelse
                    debugPanic("lambdamono.lower.refinedSourceTypeForExpr expected solved list source type");
                for (items) |item_expr_id| {
                    const item_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, item_expr_id);
                    try self.unifyIn(inst.types, elem_source_ty, item_source_ty);
                    const refined_item_ty = try self.refinedSourceTypeForExpr(
                        inst,
                        venv,
                        item_expr_id,
                        item_source_ty,
                    );
                    try self.unifyIn(inst.types, elem_source_ty, refined_item_ty);
                }
                break :blk current_source_ty;
            },
            .record => |fields_span| blk: {
                const fields = self.input.store.sliceFieldExprSpan(fields_span);
                for (fields) |field| {
                    const field_source_ty = self.solvedRecordFieldByName(
                        inst.types,
                        current_source_ty,
                        field.name,
                    ) orelse debugPanic("lambdamono.lower.refinedSourceTypeForExpr missing solved record field");
                    const refined_field_ty = try self.refinedSourceTypeForExpr(
                        inst,
                        venv,
                        field.value,
                        field_source_ty,
                    );
                    try self.unifyIn(inst.types, field_source_ty, refined_field_ty);
                }
                break :blk current_source_ty;
            },
            .tag_payload => |tag_payload| blk: {
                const union_ty = try self.instantiatedSourceTypeForExpr(inst, venv, tag_payload.tag_union);
                break :blk self.solvedTagPayloadTypeByName(
                    inst.types,
                    union_ty,
                    tag_payload.tag_name,
                    tag_payload.payload_index,
                ) orelse self.solvedTagPayloadType(
                    inst.types,
                    union_ty,
                    tag_payload.tag_discriminant,
                    tag_payload.payload_index,
                ) orelse current_source_ty;
            },
            .tag => |tag| blk: {
                const source_args = self.input.store.sliceExprSpan(tag.args);
                const tag_arg_tys = try self.tagArgSourceTypesForExpr(
                    inst,
                    venv,
                    current_source_ty,
                    tag.name,
                    source_args,
                );
                defer if (tag_arg_tys.len != 0) self.allocator.free(tag_arg_tys);
                for (source_args, 0..) |arg_expr_id, i| {
                    const refined_arg_ty = try self.refinedSourceTypeForExpr(
                        inst,
                        venv,
                        arg_expr_id,
                        tag_arg_tys[i],
                    );
                    try self.unifyIn(inst.types, tag_arg_tys[i], refined_arg_ty);
                }
                break :blk current_source_ty;
            },
            .call => |call| blk: {
                const arg_expr_ids = self.input.store.sliceExprSpan(call.args);
                const base_func_source = self.input.store.getExpr(call.func);
                const box_boundary = self.boxBoundaryBuiltinOpForExpr(venv, base_func_source);
                const explicit_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                try self.unifyIn(inst.types, current_source_ty, explicit_result_ty);
                if (box_boundary) |op| {
                    if (arg_expr_ids.len != 1) {
                        debugPanic("lambdamono.lower.refinedSourceTypeForExpr box boundary call must have arity 1");
                    }
                    switch (op) {
                        .box_unbox => {
                            const boxed_arg_ty = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_ids[0]);
                            break :blk self.solvedBoxElemType(inst.types, boxed_arg_ty) orelse
                                debugPanic("lambdamono.lower.refinedSourceTypeForExpr box_unbox missing solved payload type");
                        },
                        .box_box => {
                            const arg_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_ids[0]);
                            const refined_arg_ty = try self.refinedSourceTypeForExpr(inst, venv, arg_expr_ids[0], arg_source_ty);
                            const boxed_payload_ty = self.solvedBoxElemType(inst.types, current_source_ty) orelse
                                debugPanic("lambdamono.lower.refinedSourceTypeForExpr box_box missing solved payload type");
                            try self.unifyIn(inst.types, boxed_payload_ty, refined_arg_ty);
                            break :blk current_source_ty;
                        },
                        else => unreachable,
                    }
                }
                const call_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    arg_expr_ids,
                    call.call_constraint_ty,
                    current_source_ty,
                );
                const current_func_ty = try self.instantiatedSourceTypeForExpr(inst, venv, call.func);
                const refined_func_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    call.func,
                    current_func_ty,
                );
                try self.unifyIn(inst.types, current_func_ty, refined_func_ty);
                try self.unifyIn(inst.types, refined_func_ty, call_constraint_ty);
                break :blk inst.types.fnShape(call_constraint_ty).ret;
            },
            .low_level => |ll| blk: {
                const arg_expr_ids = self.input.store.sliceExprSpan(ll.args);
                const explicit_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                try self.unifyIn(inst.types, current_source_ty, explicit_result_ty);
                const constraint_fn_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    arg_expr_ids,
                    ll.source_constraint_ty,
                    current_source_ty,
                );
                break :blk inst.types.fnShape(constraint_fn_ty).ret;
            },
            .dispatch_call => |method_call| blk: {
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const dispatch_arg_exprs = try self.allocator.alloc(solved.Ast.ExprId, method_args.len + 1);
                defer self.allocator.free(dispatch_arg_exprs);
                dispatch_arg_exprs[0] = method_call.receiver;
                @memcpy(dispatch_arg_exprs[1..], method_args);
                const explicit_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                try self.unifyIn(inst.types, current_source_ty, explicit_result_ty);
                const dispatch_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    dispatch_arg_exprs,
                    method_call.dispatch_constraint_ty,
                    current_source_ty,
                );
                const dispatch_shape = inst.types.fnShape(dispatch_constraint_ty);
                const frozen_arg_tys = try self.dupeTypeVarIds(
                    inst.types.sliceTypeVarSpan(dispatch_shape.args),
                );
                defer if (frozen_arg_tys.len != 0) self.allocator.free(frozen_arg_tys);
                if (frozen_arg_tys.len != method_args.len + 1) {
                    debugPanic("lambdamono.lower.refinedSourceTypeForExpr dispatch arg arity mismatch");
                }
                const refined_receiver_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    method_call.receiver,
                    frozen_arg_tys[0],
                );
                try self.unifyIn(inst.types, frozen_arg_tys[0], refined_receiver_ty);
                try self.refineCallArgSourceTypesIn(
                    inst,
                    venv,
                    method_args,
                    frozen_arg_tys[1..],
                );
                break :blk dispatch_shape.ret;
            },
            .type_dispatch_call => |method_call| blk: {
                const method_args = self.input.store.sliceExprSpan(method_call.args);
                const explicit_result_ty = try self.explicitExprSourceType(inst, venv, expr_id);
                try self.unifyIn(inst.types, current_source_ty, explicit_result_ty);
                const dispatch_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
                    inst,
                    venv,
                    method_args,
                    method_call.dispatch_constraint_ty,
                    current_source_ty,
                );
                const dispatch_shape = inst.types.fnShape(dispatch_constraint_ty);
                const frozen_arg_tys = try self.dupeTypeVarIds(
                    inst.types.sliceTypeVarSpan(dispatch_shape.args),
                );
                defer if (frozen_arg_tys.len != 0) self.allocator.free(frozen_arg_tys);
                if (frozen_arg_tys.len != method_args.len) {
                    debugPanic("lambdamono.lower.refinedSourceTypeForExpr type dispatch arg arity mismatch");
                }
                try self.refineCallArgSourceTypesIn(
                    inst,
                    venv,
                    method_args,
                    frozen_arg_tys,
                );
                break :blk dispatch_shape.ret;
            },
            .let_ => |let_expr| blk: {
                var local_mono_cache = lower_type.MonoCache.init(self.allocator);
                defer local_mono_cache.deinit();

                const bind_ty = try self.freshCloneTypeIntoInstFromStore(
                    inst,
                    &self.input.types,
                    let_expr.bind.ty,
                );
                const body_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, let_expr.body);
                try self.unifyIn(inst.types, bind_ty, body_source_ty);
                const refined_body_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    venv,
                    let_expr.body,
                    bind_ty,
                );
                try self.unifyIn(inst.types, bind_ty, refined_body_ty);
                const rest_env = try self.extendEnv(venv, .{
                    .symbol = let_expr.bind.symbol,
                    .ty = bind_ty,
                    .exec_ty = try self.lowerExecutableTypeFromSolvedIn(inst.types, &local_mono_cache, bind_ty),
                    .exact_fn_symbol = self.exactCallableSymbolForBoundExpr(let_expr.bind.symbol, let_expr.body),
                });
                defer self.allocator.free(rest_env);
                const refined_rest_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    rest_env,
                    let_expr.rest,
                    current_source_ty,
                );
                try self.unifyIn(inst.types, current_source_ty, refined_rest_ty);
                break :blk current_source_ty;
            },
            .block => |block| blk: {
                var local_mono_cache = lower_type.MonoCache.init(self.allocator);
                defer local_mono_cache.deinit();

                var env = try self.cloneEnv(venv);
                defer self.allocator.free(env);

                for (self.input.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    const next_env = try self.preRefineStmtSourceTypes(
                        inst,
                        &local_mono_cache,
                        env,
                        stmt_id,
                    );
                    self.allocator.free(env);
                    env = next_env;
                }
                const refined_final_ty = try self.refinedSourceTypeForExpr(
                    inst,
                    env,
                    block.final_expr,
                    current_source_ty,
                );
                try self.unifyIn(inst.types, current_source_ty, refined_final_ty);
                break :blk current_source_ty;
            },
            else => current_source_ty,
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
        proc_capture_exec_ty: ?type_mod.TypeId,
        capture_expr: ?ast.ExprId,
        proc_ret_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        if (proc_arg_exec_tys.len != arg_exprs.len or arg_source_tys.len != arg_exprs.len) {
            debugPanic("lambdamono.lower.applyExactTopLevelFunctionCall call arg arity mismatch");
        }
        const bridged_args = try self.bridgeCallArgsToExpectedExecutableTypes(
            inst,
            mono_cache,
            arg_source_tys,
            arg_exprs,
            proc_arg_exec_tys,
        );
        defer if (bridged_args.len != 0) self.allocator.free(bridged_args);

        const call_args = if (proc_capture_exec_ty) |capture_exec_ty| blk: {
            const capture = capture_expr orelse
                debugPanic("lambdamono.lower.applyExactTopLevelFunctionCall missing capture arg for captured specialization");
            const final_capture = if (!self.types.equalIds(self.output.getExpr(capture).ty, capture_exec_ty))
                try self.emitExplicitBridgeExpr(capture, capture_exec_ty)
            else
                capture;
            const out = try self.allocator.alloc(ast.ExprId, bridged_args.len + 1);
            @memcpy(out[0..bridged_args.len], bridged_args);
            out[bridged_args.len] = final_capture;
            break :blk out;
        } else blk: {
            if (capture_expr != null) {
                debugPanic("lambdamono.lower.applyExactTopLevelFunctionCall unexpected capture arg for captureless specialization");
            }
            break :blk try self.allocator.dupe(ast.ExprId, bridged_args);
        };
        defer if (call_args.len != 0) self.allocator.free(call_args);

        return try self.output.addExpr(.{
            .ty = proc_ret_exec_ty,
            .data = .{ .call = .{
                .proc = proc_symbol,
                .args = try self.output.addExprSpan(call_args),
            } },
        });
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
        const receiver_source_ty = try self.freshCloneTypeIntoInstFromStore(
            inst,
            &self.input.types,
            self.input.store.getExpr(eq.lhs).ty,
        );
        const arg_source_ty = try self.freshCloneTypeIntoInstFromStore(
            inst,
            &self.input.types,
            self.input.store.getExpr(eq.rhs).ty,
        );
        const refined_receiver_ty = try self.refinedSourceTypeForExpr(
            inst,
            venv,
            eq.lhs,
            receiver_source_ty,
        );
        try self.unifyIn(inst.types, receiver_source_ty, refined_receiver_ty);
        const refined_arg_ty = try self.refinedSourceTypeForExpr(
            inst,
            venv,
            eq.rhs,
            arg_source_ty,
        );
        try self.unifyIn(inst.types, arg_source_ty, refined_arg_ty);
        const operand_source_ty = try self.freshCloneTypeIntoInstFromStore(
            inst,
            inst.types,
            receiver_source_ty,
        );
        try self.unifyIn(inst.types, operand_source_ty, arg_source_ty);
        const operand_default_ty = try self.lowerExecutableTypeFromSolvedIn(
            inst.types,
            mono_cache,
            operand_source_ty,
        );

        const lhs_expr = self.input.store.getExpr(eq.lhs);
        var lowered_receiver = try self.specializeExprWithDefaultTy(
            inst,
            mono_cache,
            venv,
            eq.lhs,
            lhs_expr,
            receiver_source_ty,
            operand_default_ty,
        );
        if (try self.retypeDivergingExpr(lowered_receiver, expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }

        const rhs_expr = self.input.store.getExpr(eq.rhs);
        var lowered_arg = try self.specializeExprWithDefaultTy(
            inst,
            mono_cache,
            venv,
            eq.rhs,
            rhs_expr,
            arg_source_ty,
            operand_default_ty,
        );
        if (try self.retypeDivergingExpr(lowered_arg, expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }
        const receiver_exec_ty = self.output.getExpr(lowered_receiver).ty;
        const arg_exec_ty = self.output.getExpr(lowered_arg).ty;
        const operand_exec_ty = if (!self.executableTypeIsAbstract(operand_default_ty))
            self.requireConcreteExecutableType(
                operand_default_ty,
                "specializeStructuralEqExpr",
            )
        else if (self.types.equalIds(receiver_exec_ty, arg_exec_ty))
            receiver_exec_ty
        else
            debugPanic("lambdamono.lower.specializeStructuralEqExpr abstract operand executable types disagree");
        if (!self.types.equalIds(self.output.getExpr(lowered_receiver).ty, operand_exec_ty)) {
            lowered_receiver = try self.retargetOrEmitExplicitBridgeExpr(lowered_receiver, operand_exec_ty);
        }
        if (!self.types.equalIds(self.output.getExpr(lowered_arg).ty, operand_exec_ty)) {
            lowered_arg = try self.retargetOrEmitExplicitBridgeExpr(lowered_arg, operand_exec_ty);
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
        mono_cache: *lower_type.MonoCache,
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
        const dispatch_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(dispatch_shape.args),
        );
        defer if (dispatch_arg_tys.len != 0) self.allocator.free(dispatch_arg_tys);
        if (dispatch_arg_tys.len != 2) {
            debugPanic("lambdamono.lower.specializeMethodEqExpr method_eq arg arity mismatch");
        }
        try self.unifyIn(inst.types, dispatch_arg_tys[0], receiver_source_ty);
        try self.unifyIn(inst.types, dispatch_arg_tys[1], arg_source_ty);
        try self.unifyIn(inst.types, dispatch_shape.ret, bool_source_ty);
        const specialized = try self.ensureExactCallableSpecializedForRequestedType(
            inst.types,
            mono_cache,
            target_symbol,
            dispatch_constraint_ty,
            null,
            null,
        );
        const imported = try self.importCallableSummaryIntoInst(
            inst,
            dispatch_constraint_ty,
            specialized.summary_types,
            specialized.summary_fn_ty,
        );
        const fn_shape = imported.fn_shape;
        const fn_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(fn_shape.args),
        );
        defer if (fn_arg_tys.len != 0) self.allocator.free(fn_arg_tys);
        if (fn_arg_tys.len != 2) {
            debugPanic("lambdamono.lower.specializeMethodEqExpr method_eq arg arity mismatch");
        }

        const lowered_args = try self.allocator.alloc(ast.ExprId, fn_arg_tys.len);
        defer self.allocator.free(lowered_args);
        const lhs_expr = self.input.store.getExpr(eq.lhs);
        lowered_args[0] = try self.specializeExprWithDefaultTy(
            inst,
            mono_cache,
            venv,
            eq.lhs,
            lhs_expr,
            fn_arg_tys[0],
            specialized.exec_args_tys[0],
        );
        if (try self.retypeDivergingExpr(lowered_args[0], expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }
        const rhs_expr = self.input.store.getExpr(eq.rhs);
        lowered_args[1] = try self.specializeExprWithDefaultTy(
            inst,
            mono_cache,
            venv,
            eq.rhs,
            rhs_expr,
            fn_arg_tys[1],
            specialized.exec_args_tys[1],
        );
        var result_expr = try self.applyExactTopLevelFunctionCall(
            inst,
            mono_cache,
            specialized.symbol,
            fn_arg_tys,
            lowered_args,
            specialized.exec_args_tys,
            specialized.exec_capture_ty,
            null,
            specialized.exec_ret_ty,
        );
        if (eq.negated) {
            result_expr = try self.makeLowLevelExpr(self.output.getExpr(result_expr).ty, .bool_not, &.{result_expr});
        }
        const final_result_exec_ty = self.requireConcreteExecutableType(
            expected_exec_ty,
            "specializeMethodEqExpr",
        );
        if (!self.types.equalIds(self.output.getExpr(result_expr).ty, final_result_exec_ty)) {
            result_expr = try self.emitExplicitBridgeExpr(result_expr, final_result_exec_ty);
        }
        const final_expr = self.output.getExpr(result_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
            .source_ty = fn_shape.ret,
        };
    }

    fn specializeDispatchCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        method_call: @FieldType(solved.Ast.Expr.Data, "dispatch_call"),
        current_result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const method_args = self.input.store.sliceExprSpan(method_call.args);
        const dispatch_arg_exprs = try self.allocator.alloc(solved.Ast.ExprId, method_args.len + 1);
        defer self.allocator.free(dispatch_arg_exprs);
        dispatch_arg_exprs[0] = method_call.receiver;
        @memcpy(dispatch_arg_exprs[1..], method_args);
        const dispatch_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
            inst,
            venv,
            dispatch_arg_exprs,
            method_call.dispatch_constraint_ty,
            current_result_source_ty,
        );
        const initial_dispatch_shape = inst.types.fnShape(dispatch_constraint_ty);
        const initial_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(initial_dispatch_shape.args),
        );
        defer if (initial_arg_tys.len != 0) self.allocator.free(initial_arg_tys);
        if (initial_arg_tys.len != method_args.len + 1) {
            debugPanic("lambdamono.lower.specializeDispatchCallExpr initial method arg arity mismatch");
        }
        const refined_receiver_ty = try self.refinedSourceTypeForExpr(
            inst,
            venv,
            method_call.receiver,
            initial_arg_tys[0],
        );
        try self.unifyIn(inst.types, initial_arg_tys[0], refined_receiver_ty);
        try self.refineCallArgSourceTypesIn(
            inst,
            venv,
            method_args,
            initial_arg_tys[1..],
        );
        const target_symbol = try self.resolveAttachedMethodTargetFromExpr(
            inst,
            venv,
            method_call.receiver,
            method_call.method_name,
        );
        const exact_entry = specializations.lookupFnExact(self.fenv, target_symbol) orelse
            debugPanic("lambdamono.lower.specializeDispatchCallExpr resolved attached method target missing exact callable definition");
        const exact_requested_ty = try self.unifyExactCallableSourceType(
            inst,
            exact_entry.fn_ty,
            dispatch_constraint_ty,
        );
        const exact_requested_shape = inst.types.fnShape(exact_requested_ty);
        const exact_requested_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(exact_requested_shape.args),
        );
        defer if (exact_requested_arg_tys.len != 0) self.allocator.free(exact_requested_arg_tys);
        if (exact_requested_arg_tys.len != method_args.len + 1) {
            debugPanic("lambdamono.lower.specializeDispatchCallExpr exact requested method arg arity mismatch");
        }
        try self.unifyIn(inst.types, exact_requested_arg_tys[0], initial_arg_tys[0]);
        try self.refineCallArgSourceTypesIn(
            inst,
            venv,
            method_args,
            exact_requested_arg_tys[1..],
        );
        const repr_mode = self.exactCallableReprMode(target_symbol);
        const requested_exec_arg_tys = try self.allocator.alloc(type_mod.TypeId, exact_requested_arg_tys.len);
        defer if (requested_exec_arg_tys.len != 0) self.allocator.free(requested_exec_arg_tys);
        requested_exec_arg_tys[0] = try self.lowerRequestedExecutableArgTypeFromExpr(
            inst,
            mono_cache,
            venv,
            method_call.receiver,
            exact_requested_arg_tys[0],
            repr_mode,
        );
        for (method_args, 0..) |arg_expr_id, i| {
            requested_exec_arg_tys[i + 1] = try self.lowerRequestedExecutableArgTypeFromExpr(
                inst,
                mono_cache,
                venv,
                arg_expr_id,
                exact_requested_arg_tys[i + 1],
                repr_mode,
            );
        }
        const requested_exec_ret_ty = try self.lowerRequestedExecutableReturnTypeFromCallRelation(
            inst.types,
            mono_cache,
            exact_requested_arg_tys,
            requested_exec_arg_tys,
            exact_requested_shape.ret,
            repr_mode,
            "specializeDispatchCallExpr(requested ret)",
        );
        const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
            inst.types,
            target_symbol,
            repr_mode,
            exact_requested_ty,
            null,
            null,
            requested_exec_arg_tys,
            requested_exec_ret_ty,
        );
        const imported = try self.importCallableSummaryIntoInst(
            inst,
            dispatch_constraint_ty,
            specialized.summary_types,
            specialized.summary_fn_ty,
        );
        const fn_shape = imported.fn_shape;
        const fn_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(fn_shape.args),
        );
        defer if (fn_arg_tys.len != 0) self.allocator.free(fn_arg_tys);
        if (fn_arg_tys.len != method_args.len + 1) {
            debugPanic("lambdamono.lower.specializeDispatchCallExpr method arg arity mismatch");
        }
        try self.unifyIn(inst.types, fn_arg_tys[0], initial_arg_tys[0]);
        try self.refineCallArgSourceTypesIn(
            inst,
            venv,
            method_args,
            fn_arg_tys[1..],
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, fn_arg_tys.len);
        defer self.allocator.free(lowered_args);
        const receiver_expr = self.input.store.getExpr(method_call.receiver);
        lowered_args[0] = try self.specializeExprWithDefaultTy(
            inst,
            mono_cache,
            venv,
            method_call.receiver,
            receiver_expr,
            fn_arg_tys[0],
            specialized.exec_args_tys[0],
        );
        if (try self.retypeDivergingExpr(lowered_args[0], expected_exec_ty)) |diverging| {
            return .{
                .ty = self.output.getExpr(diverging).ty,
                .data = self.output.getExpr(diverging).data,
            };
        }

        for (method_args, 0..) |arg_expr_id, i| {
            const arg_expr = self.input.store.getExpr(arg_expr_id);
            lowered_args[i + 1] = try self.specializeExprWithDefaultTy(
                inst,
                mono_cache,
                venv,
                arg_expr_id,
                arg_expr,
                fn_arg_tys[i + 1],
                specialized.exec_args_tys[i + 1],
            );
        }
        const result_expr = try self.applyExactTopLevelFunctionCall(
            inst,
            mono_cache,
            specialized.symbol,
            fn_arg_tys,
            lowered_args,
            specialized.exec_args_tys,
            specialized.exec_capture_ty,
            null,
            specialized.exec_ret_ty,
        );
        const final_expr = self.output.getExpr(result_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
            .source_ty = fn_shape.ret,
        };
    }

    fn specializeTypeDispatchCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        method_call: @FieldType(solved.Ast.Expr.Data, "type_dispatch_call"),
        current_result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        _ = expected_exec_ty;
        const method_args = self.input.store.sliceExprSpan(method_call.args);
        const dispatch_constraint_ty = try self.instantiateConstraintFnTypeForExprs(
            inst,
            venv,
            method_args,
            method_call.dispatch_constraint_ty,
            current_result_source_ty,
        );
        const initial_dispatch_shape = inst.types.fnShape(dispatch_constraint_ty);
        const initial_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(initial_dispatch_shape.args),
        );
        defer if (initial_arg_tys.len != 0) self.allocator.free(initial_arg_tys);
        if (initial_arg_tys.len != method_args.len) {
            debugPanic("lambdamono.lower.specializeTypeDispatchCallExpr initial method arg arity mismatch");
        }
        try self.refineCallArgSourceTypesIn(
            inst,
            venv,
            method_args,
            initial_arg_tys,
        );
        const dispatcher_ty = try self.cloneTypeIntoInstFromStore(
            inst,
            &self.input.types,
            method_call.dispatcher_ty,
        );
        const target_symbol = self.resolveAttachedMethodTarget(inst.types, dispatcher_ty, method_call.method_name);
        const repr_mode = self.exactCallableReprMode(target_symbol);
        const requested_exec_arg_tys = try self.allocator.alloc(type_mod.TypeId, initial_arg_tys.len);
        defer if (requested_exec_arg_tys.len != 0) self.allocator.free(requested_exec_arg_tys);
        for (method_args, 0..) |arg_expr_id, i| {
            requested_exec_arg_tys[i] = try self.lowerRequestedExecutableArgTypeFromExpr(
                inst,
                mono_cache,
                venv,
                arg_expr_id,
                initial_arg_tys[i],
                repr_mode,
            );
        }
        const requested_exec_ret_ty = try self.lowerRequestedExecutableReturnTypeFromCallRelation(
            inst.types,
            mono_cache,
            initial_arg_tys,
            requested_exec_arg_tys,
            initial_dispatch_shape.ret,
            repr_mode,
            "specializeTypeDispatchCallExpr(requested ret)",
        );
        const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
            inst.types,
            target_symbol,
            repr_mode,
            dispatch_constraint_ty,
            null,
            null,
            requested_exec_arg_tys,
            requested_exec_ret_ty,
        );
        const imported = try self.importCallableSummaryIntoInst(
            inst,
            dispatch_constraint_ty,
            specialized.summary_types,
            specialized.summary_fn_ty,
        );
        const fn_shape = imported.fn_shape;
        const fn_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(fn_shape.args),
        );
        defer if (fn_arg_tys.len != 0) self.allocator.free(fn_arg_tys);
        if (fn_arg_tys.len != method_args.len) {
            debugPanic("lambdamono.lower.specializeTypeDispatchCallExpr method arg arity mismatch");
        }
        for (fn_arg_tys, initial_arg_tys) |fn_arg_ty, initial_arg_ty| {
            try self.unifyIn(inst.types, fn_arg_ty, initial_arg_ty);
        }
        try self.refineCallArgSourceTypesIn(
            inst,
            venv,
            method_args,
            fn_arg_tys,
        );

        const lowered_args = try self.allocator.alloc(ast.ExprId, fn_arg_tys.len);
        defer self.allocator.free(lowered_args);
        for (method_args, 0..) |arg_expr_id, i| {
            const arg_expr = self.input.store.getExpr(arg_expr_id);
            lowered_args[i] = try self.specializeExprWithDefaultTy(
                inst,
                mono_cache,
                venv,
                arg_expr_id,
                arg_expr,
                fn_arg_tys[i],
                specialized.exec_args_tys[i],
            );
        }
        const result_expr = try self.applyExactTopLevelFunctionCall(
            inst,
            mono_cache,
            specialized.symbol,
            fn_arg_tys,
            lowered_args,
            specialized.exec_args_tys,
            specialized.exec_capture_ty,
            null,
            specialized.exec_ret_ty,
        );
        const final_expr = self.output.getExpr(result_expr);
        return .{
            .ty = final_expr.ty,
            .data = final_expr.data,
            .source_ty = fn_shape.ret,
        };
    }

    fn specializeCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        call: @FieldType(solved.Ast.Expr.Data, "call"),
        current_result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const base_func_source = self.input.store.getExpr(call.func);
        const arg_expr_ids = self.input.store.sliceExprSpan(call.args);

        if (self.boxBoundaryBuiltinOpForExpr(venv, base_func_source)) |op| {
            if (arg_expr_ids.len != 1) {
                debugPanic("lambdamono.lower.specializeCallExpr box boundary call must have arity 1");
            }
            const lowered_arg = try self.specializeBoxBoundaryArgExpr(inst, mono_cache, venv, op, arg_expr_ids[0]);
            const result_expr = try self.output.addExpr(.{
                .ty = try self.boxBoundaryResultTypeFromArg(op, self.output.getExpr(lowered_arg).ty),
                .data = .{ .low_level = .{
                    .op = op,
                    .args = try self.output.addExprSpan(&.{lowered_arg}),
                } },
            });
            const lowered = self.output.getExpr(result_expr);
            const source_result_ty = switch (op) {
                .box_unbox => blk: {
                    const boxed_arg_ty = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_ids[0]);
                    break :blk self.solvedBoxElemType(inst.types, boxed_arg_ty) orelse
                        debugPanic("lambdamono.lower.specializeCallExpr box_unbox missing solved payload type");
                },
                .box_box => blk: {
                    const arg_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, arg_expr_ids[0]);
                    const refined_arg_ty = try self.refinedSourceTypeForExpr(inst, venv, arg_expr_ids[0], arg_source_ty);
                    const boxed_payload_ty = self.solvedBoxElemType(inst.types, current_result_source_ty) orelse
                        debugPanic("lambdamono.lower.specializeCallExpr box_box missing solved payload type");
                    try self.unifyIn(inst.types, boxed_payload_ty, refined_arg_ty);
                    break :blk current_result_source_ty;
                },
                else => unreachable,
            };
            return .{
                .ty = lowered.ty,
                .data = lowered.data,
                .source_ty = source_result_ty,
            };
        }

        const call_relation_ty = try self.instantiateConstraintFnTypeForExprs(
            inst,
            venv,
            arg_expr_ids,
            call.call_constraint_ty,
            current_result_source_ty,
        );
        const call_relation_shape = inst.types.fnShape(call_relation_ty);
        const call_arg_tys = try self.dupeTypeVarIds(
            inst.types.sliceTypeVarSpan(call_relation_shape.args),
        );
        defer if (call_arg_tys.len != 0) self.allocator.free(call_arg_tys);
        try self.refineCallArgSourceTypesIn(
            inst,
            venv,
            arg_expr_ids,
            call_arg_tys,
        );
        const direct_func_symbol = self.directCallableSymbolFromExpr(venv, base_func_source);
        const initial_func_source_ty = try self.instantiatedSourceTypeForExpr(
            inst,
            venv,
            call.func,
        );
        const func_source_ty = try self.refinedSourceTypeForExpr(
            inst,
            venv,
            call.func,
            initial_func_source_ty,
        );
        try self.unifyIn(inst.types, func_source_ty, call_relation_ty);
        if (direct_func_symbol) |exact_symbol| {
            const exact_source_entry = specializations.lookupFnExact(self.fenv, exact_symbol);
            const exact_capture_symbols: []const Symbol = if (exact_source_entry) |exact_entry|
                exact_entry.capture_symbols
            else if (self.isHostedTopLevelSymbol(exact_symbol))
                &.{}
            else
                &.{};
            if (exact_source_entry != null or self.isHostedTopLevelSymbol(exact_symbol)) {
                if (self.envHasAllCaptureSymbols(venv, exact_capture_symbols)) {
                    const explicit_call_relation_ty = try self.instantiateExplicitConstraintFnTypeForExprs(
                        inst,
                        venv,
                        arg_expr_ids,
                        call.call_constraint_ty,
                        current_result_source_ty,
                    );
                    const requested_call_ty = if (exact_source_entry) |entry|
                        try self.unifyExactCallableSourceType(inst, entry.fn_ty, explicit_call_relation_ty)
                    else
                        explicit_call_relation_ty;
                    const requested_call_shape = inst.types.fnShape(requested_call_ty);
                    const requested_call_arg_tys = try self.dupeTypeVarIds(
                        inst.types.sliceTypeVarSpan(requested_call_shape.args),
                    );
                    defer if (requested_call_arg_tys.len != 0) self.allocator.free(requested_call_arg_tys);
                    const current_capture = try self.currentCapturePayloadFromSymbols(
                        inst.types,
                        mono_cache,
                        exact_capture_symbols,
                        venv,
                    );
                    defer current_capture.deinit(self.allocator);
                    const capture_exact_symbols = try self.captureExactSymbolsFromEnv(
                        exact_capture_symbols,
                        current_capture.env,
                    );
                    defer if (capture_exact_symbols) |symbols| self.allocator.free(symbols);
                    const direct_exec_arg_tys = try self.allocator.alloc(type_mod.TypeId, arg_expr_ids.len);
                    defer if (direct_exec_arg_tys.len != 0) self.allocator.free(direct_exec_arg_tys);
                    const direct_arg_exprs = try self.allocator.alloc(ast.ExprId, arg_expr_ids.len);
                    defer self.allocator.free(direct_arg_exprs);
                    const repr_mode = self.exactCallableReprMode(exact_symbol);
                    if (requested_call_arg_tys.len != arg_expr_ids.len) {
                        debugPanic("lambdamono.lower.specializeCallExpr explicit direct arg arity mismatch");
                    }
                    for (arg_expr_ids, requested_call_arg_tys, 0..) |arg_expr_id, requested_arg_ty, i| {
                        const provisional_exec_ty = try self.lowerRequestedExecutableArgTypeFromExpr(
                            inst,
                            mono_cache,
                            venv,
                            arg_expr_id,
                            requested_arg_ty,
                            repr_mode,
                        );
                        const arg_expr = self.input.store.getExpr(arg_expr_id);
                        const lowered_arg = try self.specializeExprWithDefaultTy(
                            inst,
                            mono_cache,
                            venv,
                            arg_expr_id,
                            arg_expr,
                            requested_arg_ty,
                            provisional_exec_ty,
                        );
                        const authoritative_arg = self.authoritativeCallableValue(lowered_arg);
                        direct_arg_exprs[i] = authoritative_arg.expr;
                        direct_exec_arg_tys[i] = self.output.getExpr(direct_arg_exprs[i]).ty;
                    }
                    const direct_exec_ret_ty = try self.lowerRequestedExecutableReturnTypeFromCallRelation(
                        inst.types,
                        mono_cache,
                        requested_call_arg_tys,
                        direct_exec_arg_tys,
                        requested_call_shape.ret,
                        repr_mode,
                        "specializeCallExpr(direct exact ret)",
                    );
                    const specialized = try self.ensureQueuedCallableSpecializedWithExecSignature(
                        inst.types,
                        exact_symbol,
                        repr_mode,
                        requested_call_ty,
                        current_capture.ty,
                        capture_exact_symbols,
                        direct_exec_arg_tys,
                        direct_exec_ret_ty,
                    );
                    const imported = try self.importCallableSummaryIntoInst(
                        inst,
                        call_relation_ty,
                        specialized.summary_types,
                        specialized.summary_fn_ty,
                    );
                    const exact_shape = imported.fn_shape;
                    const exact_arg_tys = try self.dupeTypeVarIds(
                        inst.types.sliceTypeVarSpan(exact_shape.args),
                    );
                    defer if (exact_arg_tys.len != 0) self.allocator.free(exact_arg_tys);
                    const lowered_args = try self.allocator.alloc(ast.ExprId, arg_expr_ids.len);
                    defer self.allocator.free(lowered_args);
                    if (specialized.exec_args_tys.len != exact_arg_tys.len) {
                        debugPanic("lambdamono.lower.specializeCallExpr direct exact arg arity mismatch against specialization summary");
                    }
                    for (exact_arg_tys, call_arg_tys, arg_expr_ids, 0..) |exact_arg_ty, call_arg_ty, arg_expr_id, i| {
                        try self.unifyIn(inst.types, exact_arg_ty, call_arg_ty);
                        _ = arg_expr_id;
                        lowered_args[i] = direct_arg_exprs[i];
                        if (!self.types.equalIds(self.output.getExpr(lowered_args[i]).ty, specialized.exec_args_tys[i])) {
                            lowered_args[i] = try self.retargetOrEmitExplicitBridgeExpr(
                                lowered_args[i],
                                specialized.exec_args_tys[i],
                            );
                        }
                    }
                    const capture_expr = if (current_capture.ty) |capture_ty|
                        try self.specializeCaptureRecord(current_capture.env, capture_ty)
                    else
                        null;
                    const result_expr = try self.applyExactTopLevelFunctionCall(
                        inst,
                        mono_cache,
                        specialized.symbol,
                        exact_arg_tys,
                        lowered_args,
                        specialized.exec_args_tys,
                        specialized.exec_capture_ty,
                        capture_expr,
                        specialized.exec_ret_ty,
                    );
                    const lowered = self.output.getExpr(result_expr);
                    return .{
                        .ty = lowered.ty,
                        .data = lowered.data,
                        .source_ty = exact_shape.ret,
                    };
                }
            }
        }
        const func_default_ty = try self.explicitValueExecutableTypeForExpr(
            inst.types,
            mono_cache,
            venv,
            call.func,
            func_source_ty,
        );
        const func_expr = self.input.store.getExpr(call.func);
        var lowered_func = try self.specializeExprWithDefaultTy(
            inst,
            mono_cache,
            venv,
            call.func,
            func_expr,
            func_source_ty,
            func_default_ty,
        );
        const authoritative_callable = self.authoritativeCallableValue(lowered_func);
        lowered_func = authoritative_callable.expr;
        const lowered_func_exec_ty = authoritative_callable.ty;
        if (self.executableTypeIsAbstract(lowered_func_exec_ty)) {
            const default_summary = self.debugExecutableTypeSummary(func_default_ty);
            defer default_summary.deinit(self.allocator);
            const lowered_summary = self.debugExecutableTypeSummary(lowered_func_exec_ty);
            defer lowered_summary.deinit(self.allocator);
            debugPanicFmt(
                "lambdamono.lower.specializeCallExpr callable value missing explicit executable signature direct_symbol={?d} func_expr={d} func_var={?d} default_exec={s} lowered_exec={s}",
                .{
                    if (direct_func_symbol) |symbol| symbol.raw() else null,
                    @intFromEnum(call.func),
                    switch (base_func_source.data) {
                        .var_ => |symbol| symbol.raw(),
                        else => null,
                    },
                    default_summary.text,
                    lowered_summary.text,
                },
            );
        }
        const lowered_args = try self.allocator.alloc(ast.ExprId, arg_expr_ids.len);
        defer self.allocator.free(lowered_args);
        const exec_call_sig = self.requireExecutableCallableSig(
            lowered_func_exec_ty,
            "specializeCallExpr",
        );
        for (arg_expr_ids, 0..) |arg_expr_id, i| {
            const arg_expr = self.input.store.getExpr(arg_expr_id);
            lowered_args[i] = try self.specializeExprWithDefaultTy(
                inst,
                mono_cache,
                venv,
                arg_expr_id,
                arg_expr,
                call_arg_tys[i],
                exec_call_sig.args[i],
            );
        }
        const result_expr = try self.applyCallableValueCall(
            inst,
            mono_cache,
            lowered_func,
            func_source_ty,
            call_relation_ty,
            lowered_func_exec_ty,
            call_arg_tys,
            lowered_args,
            expected_exec_ty,
            direct_func_symbol,
        );

        const lowered = self.output.getExpr(result_expr);
        return .{
            .ty = lowered.ty,
            .data = lowered.data,
            .source_ty = call_relation_shape.ret,
        };
    }

    const SpecializedBlockExpr = struct {
        data: @FieldType(ast.Expr.Data, "block"),
        source_ty: TypeVarId,
    };

    fn specializeBlockExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        incoming_env: []const EnvEntry,
        block: @FieldType(solved.Ast.Expr.Data, "block"),
        final_source_ty: TypeVarId,
        final_default_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedBlockExpr {
        var block_refined_env = try self.cloneEnv(incoming_env);
        defer self.allocator.free(block_refined_env);

        const stmts = self.input.store.sliceStmtSpan(block.stmts);
        for (stmts) |stmt_id| {
            const next_env = try self.preRefineStmtSourceTypes(
                inst,
                mono_cache,
                block_refined_env,
                stmt_id,
            );
            self.allocator.free(block_refined_env);
            block_refined_env = next_env;
        }

        var env = try self.cloneEnv(incoming_env);
        defer self.allocator.free(env);

        const out = try self.allocator.alloc(ast.StmtId, stmts.len);
        defer self.allocator.free(out);

        for (stmts, 0..) |stmt_id, i| {
            const result = try self.specializeStmt(
                inst,
                mono_cache,
                env,
                block_refined_env,
                stmt_id,
            );
            out[i] = result.stmt;
            self.allocator.free(env);
            env = result.env;
        }

        const final_expr = self.input.store.getExpr(block.final_expr);
        const lowered_final = try self.specializeExprWithDefaultTyAndSourceTy(
            inst,
            mono_cache,
            env,
            block.final_expr,
            final_expr,
            final_source_ty,
            final_default_ty,
        );

        return .{
            .data = .{
                .stmts = try self.output.addStmtSpan(out),
                .final_expr = lowered_final.expr,
            },
            .source_ty = lowered_final.source_ty,
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
        const pat_exec_ty = self.listElemType(self.output.getExpr(iterable).ty) orelse
            debugPanic("lambdamono.lower.specializeForExpr expected list iterable executable type");
        const source_pat = self.input.store.getPat(for_expr.patt);
        const pat_ty = try self.cloneInstType(inst, source_pat.ty);
        const finalized_pat = try self.specializePatAtSourceAndExecutableTy(
            inst,
            mono_cache,
            source_pat,
            pat_ty,
            pat_exec_ty,
        );
        defer self.allocator.free(finalized_pat.additions);
        const body_env = try self.concatEnv(venv, finalized_pat.additions);
        defer self.allocator.free(body_env);
        const body = try self.specializeExpr(inst, mono_cache, body_env, for_expr.body);
        return .{
            .patt = finalized_pat.pat,
            .iterable = iterable,
            .body = body,
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

    fn collectPatBindingsAtSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat: solved.Ast.Pat,
        ty: TypeVarId,
    ) std.mem.Allocator.Error![]EnvEntry {
        return switch (pat.data) {
            .var_ => |symbol| if (symbol.isNone())
                try self.allocator.dupe(EnvEntry, &.{})
            else blk: {
                const lowered_ty = try self.lowerPatternTypeAtSourceTy(inst, mono_cache, ty);
                break :blk try self.allocator.dupe(EnvEntry, &.{.{
                    .symbol = symbol,
                    .ty = ty,
                    .exec_ty = lowered_ty,
                    .exact_fn_symbol = null,
                }});
            },
            .bool_lit => try self.allocator.dupe(EnvEntry, &.{}),
            .tag => |tag| blk: {
                const source_args = self.input.store.slicePatSpan(tag.args);
                var additions = std.ArrayList(EnvEntry).empty;
                defer additions.deinit(self.allocator);
                const arg_tys = try self.tagArgSourceTypesForPat(
                    inst,
                    ty,
                    tag.name,
                    source_args,
                );
                defer if (arg_tys.len != 0) self.allocator.free(arg_tys);
                for (source_args, 0..) |arg_pat, i| {
                    const arg_pat_data = self.input.store.getPat(arg_pat);
                    const lowered = try self.collectPatBindingsAtSourceTy(
                        inst,
                        mono_cache,
                        arg_pat_data,
                        arg_tys[i],
                    );
                    try additions.appendSlice(self.allocator, lowered);
                    self.allocator.free(lowered);
                }
                break :blk try additions.toOwnedSlice(self.allocator);
            },
        };
    }

    fn patPossibleAtExecutableTy(
        self: *Lowerer,
        pat: solved.Ast.Pat,
        lowered_ty: type_mod.TypeId,
    ) bool {
        return switch (pat.data) {
            .var_, .bool_lit => true,
            .tag => |tag| blk: {
                const exec_tag_info = self.executableTagInfoByName(lowered_ty, tag.name) orelse
                    break :blk false;
                const source_args = self.input.store.slicePatSpan(tag.args);
                if (source_args.len != exec_tag_info.args.len) {
                    debugPanic("lambdamono.lower.patPossibleAtExecutableTy executable tag arg count mismatch");
                }
                for (source_args, 0..) |arg_pat_id, i| {
                    if (!self.patPossibleAtExecutableTy(
                        self.input.store.getPat(arg_pat_id),
                        exec_tag_info.args[i],
                    )) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
        };
    }

    fn patExhaustsExecutableTy(
        self: *Lowerer,
        pat: solved.Ast.Pat,
        lowered_ty: type_mod.TypeId,
    ) bool {
        return switch (pat.data) {
            .var_ => true,
            .bool_lit => false,
            .tag => |tag| blk: {
                const exec_tag_info = self.executableTagInfoByName(lowered_ty, tag.name) orelse
                    break :blk false;
                const source_args = self.input.store.slicePatSpan(tag.args);
                if (source_args.len != exec_tag_info.args.len) {
                    debugPanic("lambdamono.lower.patExhaustsExecutableTy executable tag arg count mismatch");
                }
                const tag_ty_is_singleton = switch (self.types.getTypePreservingNominal(lowered_ty)) {
                    .nominal => |nominal| switch (self.types.getTypePreservingNominal(nominal.backing)) {
                        .tag_union => |tag_union| tag_union.tags.len == 1,
                        else => false,
                    },
                    .tag_union => |tag_union| tag_union.tags.len == 1,
                    else => false,
                };
                if (!tag_ty_is_singleton) break :blk false;
                for (source_args, 0..) |arg_pat_id, i| {
                    if (!self.patExhaustsExecutableTy(
                        self.input.store.getPat(arg_pat_id),
                        exec_tag_info.args[i],
                    )) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
        };
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
                const arg_tys = try self.tagArgSourceTypesForPat(
                    inst,
                    ty,
                    tag.name,
                    source_args,
                );
                defer if (arg_tys.len != 0) self.allocator.free(arg_tys);
                const exec_tag_info = self.executableTagInfoByName(lowered_ty, tag.name) orelse
                    debugPanic("lambdamono.lower.specializePat missing executable tag info");
                const exec_arg_tys = exec_tag_info.args;
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
                            .discriminant = exec_tag_info.discriminant,
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
        block_refined_env: []const EnvEntry,
        stmt_id: solved.Ast.StmtId,
    ) std.mem.Allocator.Error!StmtResult {
        const stmt = self.input.store.getStmt(stmt_id);
        return switch (stmt) {
            .decl => |decl| blk: {
                const refined_entry = self.lookupEnvEntry(block_refined_env, decl.bind.symbol) orelse
                    try self.preRefineBindingInEnv(
                        inst,
                        mono_cache,
                        venv,
                        decl.bind,
                        decl.body,
                    );
                var lowered_binding = try self.lowerBindingBody(
                    inst,
                    mono_cache,
                    venv,
                    decl.bind,
                    decl.body,
                    refined_entry.ty,
                    "specializeStmt(decl)",
                );
                if (!self.sourceTypeIsAbstract(inst.types, refined_entry.ty)) {
                    lowered_binding.source_ty = refined_entry.ty;
                    lowered_binding.exec_ty = self.requireConcreteExecutableType(
                        lowered_binding.exec_ty,
                        "specializeStmt(decl refined)",
                    );
                }
                const source_exact_fn_symbol = self.exactCallableSymbolForBoundExpr(decl.bind.symbol, decl.body);
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .decl = .{
                            .bind = .{
                                .ty = lowered_binding.exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = lowered_binding.body,
                        },
                    }),
                    .env = blk_env: {
                        break :blk_env try self.extendEnv(venv, .{
                            .symbol = decl.bind.symbol,
                            .ty = lowered_binding.source_ty,
                            .exec_ty = lowered_binding.exec_ty,
                            .exact_fn_symbol = source_exact_fn_symbol,
                        });
                    },
                };
            },
            .var_decl => |decl| blk: {
                const refined_entry = self.lookupEnvEntry(block_refined_env, decl.bind.symbol) orelse
                    try self.preRefineBindingInEnv(
                        inst,
                        mono_cache,
                        venv,
                        decl.bind,
                        decl.body,
                    );
                var lowered_binding = try self.lowerBindingBody(
                    inst,
                    mono_cache,
                    venv,
                    decl.bind,
                    decl.body,
                    refined_entry.ty,
                    "specializeStmt(var_decl)",
                );
                if (!self.sourceTypeIsAbstract(inst.types, refined_entry.ty)) {
                    lowered_binding.source_ty = refined_entry.ty;
                    lowered_binding.exec_ty = self.requireConcreteExecutableType(
                        lowered_binding.exec_ty,
                        "specializeStmt(var_decl refined)",
                    );
                }
                const source_exact_fn_symbol = self.exactCallableSymbolForBoundExpr(decl.bind.symbol, decl.body);
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .var_decl = .{
                            .bind = .{
                                .ty = lowered_binding.exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = lowered_binding.body,
                        },
                    }),
                    .env = blk_env: {
                        break :blk_env try self.extendEnv(venv, .{
                            .symbol = decl.bind.symbol,
                            .ty = lowered_binding.source_ty,
                            .exec_ty = lowered_binding.exec_ty,
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
            .return_ => |expr| blk: {
                var lowered = try self.specializeExpr(inst, mono_cache, venv, expr);
                const expected_ret_ty = self.current_return_exec_ty orelse
                    debugPanic("lambdamono.lower.specializeStmt(return_) missing current executable return type");
                if (!self.types.equalIds(self.output.getExpr(lowered).ty, expected_ret_ty)) {
                    lowered = try self.retargetOrEmitExplicitBridgeExpr(lowered, expected_ret_ty);
                }
                break :blk .{
                    .stmt = try self.output.addStmt(.{ .return_ = lowered }),
                    .env = try self.cloneEnv(venv),
                };
            },
            .break_ => .{
                .stmt = try self.output.addStmt(.break_),
                .env = try self.cloneEnv(venv),
            },
            .for_ => |for_stmt| blk: {
                const iterable = try self.specializeExpr(inst, mono_cache, venv, for_stmt.iterable);
                const pat_exec_ty = self.listElemType(self.output.getExpr(iterable).ty) orelse
                    debugPanic("lambdamono.lower.specializeStmt(for_) expected list iterable executable type");
                const source_pat = self.input.store.getPat(for_stmt.patt);
                const pat_ty = try self.cloneInstType(inst, source_pat.ty);
                const finalized_pat = try self.specializePatAtSourceAndExecutableTy(
                    inst,
                    mono_cache,
                    source_pat,
                    pat_ty,
                    pat_exec_ty,
                );
                defer self.allocator.free(finalized_pat.additions);
                const body_env = try self.concatEnv(venv, finalized_pat.additions);
                defer self.allocator.free(body_env);
                const body = try self.specializeExpr(inst, mono_cache, body_env, for_stmt.body);
                break :blk .{
                    .stmt = try self.output.addStmt(.{ .for_ = .{
                        .patt = finalized_pat.pat,
                        .iterable = iterable,
                        .body = body,
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

            const left_name: u32 = @bitCast(left_tags[i].name);
            const right_name: u32 = @bitCast(right_tags[j].name);
            const order = std.math.order(left_name, right_name);
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

    fn debugExecutableTypeSummary(self: *const Lowerer, ty: type_mod.TypeId) DebugTypeText {
        var out = std.ArrayList(u8).empty;
        self.debugWriteExecutableTypeSummary(&out, ty, 3) catch return .{ .text = "<oom>", .owned = false };
        return .{ .text = out.toOwnedSlice(self.allocator) catch return .{ .text = "<oom>", .owned = false }, .owned = true };
    }

    fn debugSolvedTypeSummary(self: *const Lowerer, solved_types: *const solved.Type.Store, ty: TypeVarId) DebugTypeText {
        var out = std.ArrayList(u8).empty;
        self.debugWriteSolvedTypeSummary(&out, solved_types, ty, 3) catch return .{ .text = "<oom>", .owned = false };
        return .{ .text = out.toOwnedSlice(self.allocator) catch return .{ .text = "<oom>", .owned = false }, .owned = true };
    }

    fn debugWriteExecutableTypeSummary(
        self: *const Lowerer,
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
            .erased_fn => |erased_fn| {
                try out.appendSlice(self.allocator, "erased_fn");
                if (erased_fn.capture) |capture| {
                    try out.append(self.allocator, '(');
                    try self.debugWriteExecutableTypeSummary(out, capture, depth - 1);
                    try out.append(self.allocator, ')');
                }
                try out.appendSlice(self.allocator, " -> ");
                try self.debugWriteExecutableTypeSummary(out, erased_fn.call.ret, depth - 1);
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
                if (tag_union.call) |call_sig| {
                    try out.appendSlice(self.allocator, " -> ");
                    try self.debugWriteExecutableTypeSummary(out, call_sig.ret, depth - 1);
                }
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

    fn debugWriteSolvedTypeSummary(
        self: *const Lowerer,
        out: *std.ArrayList(u8),
        solved_types: *const solved.Type.Store,
        ty: TypeVarId,
        depth: u8,
    ) std.mem.Allocator.Error!void {
        if (depth == 0) {
            try out.appendSlice(self.allocator, "...");
            return;
        }

        const id = solved_types.unlinkPreservingNominalConst(ty);
        switch (solved_types.getNode(id)) {
            .link => unreachable,
            .unbd => try out.appendSlice(self.allocator, "unbd"),
            .for_a => try out.appendSlice(self.allocator, "for_a"),
            .flex_for_a => try out.appendSlice(self.allocator, "flex_for_a"),
            .nominal => |nominal| {
                try out.writer(self.allocator).print(
                    "nominal(m={d}, i={d}, opaque={any}",
                    .{ nominal.module_idx, @as(u32, nominal.ident.idx), nominal.is_opaque },
                );
                const args = solved_types.sliceTypeVarSpan(nominal.args);
                if (args.len != 0) {
                    try out.appendSlice(self.allocator, ", args=[");
                    for (args, 0..) |arg, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try self.debugWriteSolvedTypeSummary(out, solved_types, arg, depth - 1);
                    }
                    try out.append(self.allocator, ']');
                }
                try out.appendSlice(self.allocator, ", backing=");
                try self.debugWriteSolvedTypeSummary(out, solved_types, nominal.backing, depth - 1);
                try out.append(self.allocator, ')');
            },
            .content => |content| switch (content) {
                .primitive => |prim| try out.writer(self.allocator).print("prim({s})", .{@tagName(prim)}),
                .tag_union => |tag_union| {
                    try out.appendSlice(self.allocator, "tag[");
                    const tags = solved_types.sliceTags(tag_union.tags);
                    for (tags, 0..) |tag, i| {
                        if (i != 0) try out.appendSlice(self.allocator, " | ");
                        try out.writer(self.allocator).print("#{d}", .{@as(u32, tag.name.idx)});
                        const args = solved_types.sliceTypeVarSpan(tag.args);
                        if (args.len != 0) {
                            try out.append(self.allocator, '(');
                            for (args, 0..) |arg, arg_i| {
                                if (arg_i != 0) try out.appendSlice(self.allocator, ", ");
                                try self.debugWriteSolvedTypeSummary(out, solved_types, arg, depth - 1);
                            }
                            try out.append(self.allocator, ')');
                        }
                    }
                    try out.append(self.allocator, ']');
                },
                .func => |func| {
                    try out.appendSlice(self.allocator, "fn(");
                    const args = solved_types.sliceTypeVarSpan(func.args);
                    for (args, 0..) |arg, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try self.debugWriteSolvedTypeSummary(out, solved_types, arg, depth - 1);
                    }
                    try out.appendSlice(self.allocator, ") -> ");
                    try self.debugWriteSolvedTypeSummary(out, solved_types, func.ret, depth - 1);
                },
                .list => |elem| {
                    try out.appendSlice(self.allocator, "list(");
                    try self.debugWriteSolvedTypeSummary(out, solved_types, elem, depth - 1);
                    try out.append(self.allocator, ')');
                },
                .box => |elem| {
                    try out.appendSlice(self.allocator, "box(");
                    try self.debugWriteSolvedTypeSummary(out, solved_types, elem, depth - 1);
                    try out.append(self.allocator, ')');
                },
                .tuple => |span| {
                    try out.appendSlice(self.allocator, "tuple(");
                    const elems = solved_types.sliceTypeVarSpan(span);
                    for (elems, 0..) |elem, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try self.debugWriteSolvedTypeSummary(out, solved_types, elem, depth - 1);
                    }
                    try out.append(self.allocator, ')');
                },
                .record => |record| {
                    try out.appendSlice(self.allocator, "record{");
                    const fields = solved_types.sliceFields(record.fields);
                    for (fields, 0..) |field, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try out.writer(self.allocator).print("#{d}: ", .{@as(u32, field.name.idx)});
                        try self.debugWriteSolvedTypeSummary(out, solved_types, field.ty, depth - 1);
                    }
                    try out.append(self.allocator, '}');
                },
                .lambda_set => |_| try out.appendSlice(self.allocator, "lambda_set"),
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
