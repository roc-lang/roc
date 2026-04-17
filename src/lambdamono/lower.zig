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

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    layouts: layouts_mod.Layouts,
    strings: base.StringLiteral.Store,
    entrypoint_wrappers: []Symbol,

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
};

pub fn run(allocator: std.mem.Allocator, input: solved.Lower.Result) std.mem.Allocator.Error!Result {
    return runWithEntrypoints(allocator, input, &.{});
}

pub fn runWithEntrypoints(
    allocator: std.mem.Allocator,
    input: solved.Lower.Result,
    entrypoints: []const Symbol,
) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, input, entrypoints);
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
    hosted_fn_symbols: std.AutoHashMap(Symbol, void),
    entrypoint_wrappers: []Symbol,

    const InspectKey = struct {
        value_ty: type_mod.TypeId,
        result_ty: type_mod.TypeId,
    };

    const EnvEntry = struct {
        symbol: Symbol,
        ty: TypeVarId,
        exec_ty: type_mod.TypeId,
        callable_expr: ?ast.ExprId = null,
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

        fn init(allocator: std.mem.Allocator) InstScope {
            return .{
                .allocator = allocator,
                .mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(allocator),
            };
        }

        fn deinit(self: *InstScope) void {
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
            .hosted_fn_symbols = std.AutoHashMap(Symbol, void).init(allocator),
            .entrypoint_wrappers = &.{},
        };
        lowerer.types = type_mod.Store.init(allocator);
        return lowerer;
    }

    fn deinit(self: *Lowerer) void {
        self.top_level_values.deinit();
        self.top_level_value_types.deinit();
        self.hosted_fn_symbols.deinit();
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
                .fn_ => |fn_def| self.output.getExpr(fn_def.body).ty,
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
            pending.specialized = try self.specializeFn(pending.*);
        }

        const fn_defs = try self.queue.solvedDefs(
            self.allocator,
            &self.input.types,
            &self.types,
            &self.input.symbols,
        );
        defer self.allocator.free(fn_defs);

        for (fn_defs) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .hosted_fn => |hosted_fn| {
                    const def_id = try self.output.addDef(try self.lowerHostedDef(def.bind, hosted_fn));
                    try self.root_defs.append(self.allocator, def_id);
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
    }

    fn seedEntrypointSpecializations(self: *Lowerer) std.mem.Allocator.Error!void {
        if (self.entrypoints.len == 0) return;

        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        for (self.entrypoints) |entry_symbol| {
            const entry = specializations.lookupFnExact(self.fenv, entry_symbol) orelse continue;
            switch (lower_type.lambdaRepr(&self.input.types, entry.fn_ty)) {
                .erased => debugPanic("lambdamono.lower entrypoint specialization expected lambda set"),
                .lset => {
                    const capture_ty = switch (try lower_type.extractLsetFn(
                        &self.input.types,
                        &self.types,
                        &mono_cache,
                        entry.fn_ty,
                        entry_symbol,
                        &self.input.symbols,
                    )) {
                        .toplevel => null,
                        .lset => |capture_info| capture_info.ty,
                    };
                    const sig = try self.buildLsetSpecializationSig(
                        &mono_cache,
                        capture_ty,
                        entry_symbol,
                        entry.fn_ty,
                        null,
                        null,
                    );
                    if (!self.queue.by_key.contains(sig)) {
                        try self.queue.items.append(self.allocator, .{
                            .name = entry.name,
                            .fn_ty = entry.fn_ty,
                            .fn_def = entry.fn_def,
                            .requested_ty = entry.fn_ty,
                            .sig = sig,
                            .specialized_symbol = entry_symbol,
                        });
                        try self.queue.by_key.put(sig, self.queue.items.items.len - 1);
                    }
                },
            }
        }
    }

    fn collectCurriedSignature(
        self: *Lowerer,
        fn_ty: TypeVarId,
        args: *std.ArrayList(TypeVarId),
        rets: *std.ArrayList(TypeVarId),
    ) std.mem.Allocator.Error!TypeVarId {
        var visited = std.AutoHashMap(TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        var current = fn_ty;
        while (true) {
            if (visited.contains(current)) {
                debugPanic("lambdamono.lower entrypoint wrapper detected recursive function type");
            }
            try visited.put(current, {});
            const id = self.input.types.unlink(current);
            switch (self.input.types.getNode(id)) {
                .nominal => |nominal| {
                    current = nominal.backing;
                },
                .content => |content| switch (content) {
                    .func => |func| {
                        try args.append(self.allocator, func.arg);
                        try rets.append(self.allocator, func.ret);
                        current = func.ret;
                    },
                    else => return current,
                },
                else => return current,
            }
        }
    }

    fn buildEntrypointWrappers(self: *Lowerer) std.mem.Allocator.Error!void {
        if (self.entrypoints.len == 0) return;

        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        var arg_vars = std.ArrayList(TypeVarId).empty;
        defer arg_vars.deinit(self.allocator);
        var ret_vars = std.ArrayList(TypeVarId).empty;
        defer ret_vars.deinit(self.allocator);

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
            ret_vars.clearRetainingCapacity();
            const final_ret = try self.collectCurriedSignature(entry_fn_ty, &arg_vars, &ret_vars);
            if (arg_vars.items.len == 0) {
                if (comptime builtin.mode == .Debug) {
                    std.debug.assert(final_ret == entry_fn_ty);
                } else if (final_ret != entry_fn_ty) {
                    unreachable;
                }
            } else {
                const last_ret = ret_vars.items[ret_vars.items.len - 1];
                if (comptime builtin.mode == .Debug) {
                    std.debug.assert(final_ret == last_ret);
                } else if (final_ret != last_ret) {
                    unreachable;
                }
            }
            if (arg_vars.items.len != ret_vars.items.len) {
                debugPanic("lambdamono.lower entrypoint wrapper arity mismatch");
            }

            const needs_wrapper = if (specializations.lookupFnExact(self.fenv, entry_symbol)) |_|
                arg_vars.items.len > 1
            else
                true;

            if (!needs_wrapper) continue;

            const arg_count = arg_vars.items.len;
            const arg_exec_tys = try self.allocator.alloc(type_mod.TypeId, arg_count);
            defer self.allocator.free(arg_exec_tys);
            const ret_exec_tys = try self.allocator.alloc(type_mod.TypeId, arg_count);
            defer self.allocator.free(ret_exec_tys);

            for (arg_vars.items, 0..) |arg_ty, i| {
                arg_exec_tys[i] = try self.lowerExecutableTypeWithBoxErasure(&mono_cache, arg_ty);
            }
            for (ret_vars.items, 0..) |ret_ty, i| {
                ret_exec_tys[i] = try self.lowerExecutableTypeWithBoxErasure(&mono_cache, ret_ty);
            }

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
            var inst = InstScope.init(self.allocator);
            defer inst.deinit();

            if (specializations.lookupFnExact(self.fenv, entry_symbol)) |_| {
                var first_arg_buf = [_]ast.ExprId{arg_exprs[0]};
                current_expr = try self.output.addExpr(.{
                    .ty = ret_exec_tys[0],
                    .data = .{ .call = .{
                        .proc = entry_symbol,
                        .args = try self.output.addExprSpan(&first_arg_buf),
                    } },
                });
            } else {
                const func_exec_ty = try self.ensureTopLevelValueLowered(entry_symbol);
                const func_expr = try self.output.addExpr(.{
                    .ty = func_exec_ty,
                    .data = .{ .var_ = entry_symbol },
                });

                if (arg_count == 0) {
                    current_expr = func_expr;
                } else {
                    current_expr = try self.applyFuncValue(
                        &inst,
                        &mono_cache,
                        func_expr,
                        entry_fn_ty,
                        func_exec_ty,
                        arg_exprs[0],
                        arg_vars.items[0],
                        ret_vars.items[0],
                        ret_exec_tys[0],
                        null,
                    );
                }
            }

            var i: usize = 1;
            while (i < arg_count) : (i += 1) {
                current_expr = try self.applyFuncValue(
                    &inst,
                    &mono_cache,
                    current_expr,
                    ret_vars.items[i - 1],
                    ret_exec_tys[i - 1],
                    arg_exprs[i],
                    arg_vars.items[i],
                    ret_vars.items[i],
                    ret_exec_tys[i],
                    null,
                );
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

    fn applyFuncValue(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        func_expr: ast.ExprId,
        func_ty: TypeVarId,
        func_exec_ty: type_mod.TypeId,
        arg_expr: ast.ExprId,
        arg_source_ty: TypeVarId,
        result_source_ty: TypeVarId,
        result_exec_ty: type_mod.TypeId,
        direct_func_symbol: ?Symbol,
    ) std.mem.Allocator.Error!ast.ExprId {
        return switch (self.types.getType(func_exec_ty)) {
            .erased_fn => {
                var arg_buf = [_]ast.ExprId{arg_expr};
                return self.output.addExpr(.{
                    .ty = result_exec_ty,
                    .data = .{ .call_indirect = .{
                        .func = func_expr,
                        .args = try self.output.addExprSpan(&arg_buf),
                        .capture_ty = self.erasedFnCaptureType(func_exec_ty),
                    } },
                });
            },
            .tag_union => |tag_union| blk: {
                if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    debugPanic("lambdamono.lower.applyFuncValue expected callable executable type");
                }

                const lambda_members = switch (lower_type.lambdaRepr(&self.input.types, func_ty)) {
                    .lset => try self.collectLsetLambdaMembers(mono_cache, func_ty),
                    .erased => try self.collectLsetLambdaMembersFromExecutable(tag_union),
                };
                defer self.allocator.free(lambda_members);
                const branches = try self.allocator.alloc(ast.Branch, lambda_members.len);
                defer self.allocator.free(branches);
                var result_ty: ?type_mod.TypeId = null;
                const ret_override: ?type_mod.TypeId = if (self.containsErasedFn(result_exec_ty) and
                    self.erasedFnCaptureType(result_exec_ty) != null)
                    result_exec_ty
                else
                    null;
                var call_arg = arg_expr;
                var call_arg_ty = self.output.getExpr(call_arg).ty;
                var expected_arg_ty: ?type_mod.TypeId = null;

                for (lambda_members, 0..) |lambda_member, i| {
                    const specialization_symbol = if (lambda_members.len == 1)
                        direct_func_symbol orelse lambda_member.symbol
                    else
                        lambda_member.symbol;
                    const sig = try self.buildLsetSpecializationSig(
                        mono_cache,
                        lambda_member.capture_ty,
                        specialization_symbol,
                        func_ty,
                        call_arg_ty,
                        ret_override,
                    );
                    if (expected_arg_ty == null) {
                        expected_arg_ty = sig.arg_ty;
                        if (!self.types.equalIds(call_arg_ty, sig.arg_ty) and self.containsErasedFn(sig.arg_ty)) {
                            call_arg = try self.bridgeExprToExpectedType(
                                inst,
                                mono_cache,
                                arg_source_ty,
                                call_arg,
                                sig.arg_ty,
                            );
                            call_arg_ty = self.output.getExpr(call_arg).ty;
                        }
                    } else if (!self.types.equalIds(expected_arg_ty.?, sig.arg_ty)) {
                        debugPanic("lambdamono.lower.applyFuncValue inconsistent branch arg executable types");
                    }
                    var expected_ret_ty: type_mod.TypeId = undefined;
                    if (self.types.equalIds(sig.ret_ty, result_exec_ty)) {
                        expected_ret_ty = sig.ret_ty;
                    } else if (self.preferErasedExecType(result_exec_ty, sig.ret_ty)) {
                        expected_ret_ty = sig.ret_ty;
                    } else if (self.containsErasedFn(result_exec_ty)) {
                        expected_ret_ty = result_exec_ty;
                    } else {
                        debugPanic("lambdamono.lower.applyFuncValue incompatible branch return executable types");
                    }
                    if (result_ty) |existing| {
                        if (existing != expected_ret_ty) {
                            debugPanic("lambdamono.lower.applyFuncValue inconsistent branch return executable types");
                        }
                    } else {
                        result_ty = expected_ret_ty;
                    }
                    if (self.isHostedFunctionSymbol(lambda_member.symbol)) {
                        if (lambda_member.capture_ty != null) {
                            debugPanic("lambdamono.lower.applyFuncValue hosted function had captures");
                        }
                        var body_expr = try self.output.addExpr(.{
                            .ty = sig.ret_ty,
                            .data = .{ .call = .{
                                .proc = lambda_member.symbol,
                                .args = try self.output.addExprSpan(&.{call_arg}),
                            } },
                        });
                        if (!self.types.equalIds(sig.ret_ty, expected_ret_ty)) {
                            body_expr = try self.bridgeExprToExpectedType(
                                inst,
                                mono_cache,
                                result_source_ty,
                                body_expr,
                                expected_ret_ty,
                            );
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

                    const specialized = try specializations.specializeFnLset(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        specialization_symbol,
                        func_ty,
                        sig,
                    );

                    if (lambda_member.capture_ty == null) {
                        var body_expr = try self.output.addExpr(.{
                            .ty = sig.ret_ty,
                            .data = .{ .call = .{
                                .proc = specialized,
                                .args = try self.output.addExprSpan(&.{call_arg}),
                            } },
                        });
                        if (!self.types.equalIds(sig.ret_ty, expected_ret_ty)) {
                            body_expr = try self.bridgeExprToExpectedType(
                                inst,
                                mono_cache,
                                result_source_ty,
                                body_expr,
                                expected_ret_ty,
                            );
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
                        var body_expr = try self.output.addExpr(.{
                            .ty = sig.ret_ty,
                            .data = .{ .call = .{
                                .proc = specialized,
                                .args = try self.output.addExprSpan(&.{
                                    call_arg,
                                    try self.output.addExpr(.{
                                        .ty = capture_ty,
                                        .data = .{ .var_ = capture_symbol },
                                    }),
                                }),
                            } },
                        });
                        if (!self.types.equalIds(sig.ret_ty, expected_ret_ty)) {
                            body_expr = try self.bridgeExprToExpectedType(
                                inst,
                                mono_cache,
                                result_source_ty,
                                body_expr,
                                expected_ret_ty,
                            );
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
            else => debugPanic("lambdamono.lower.applyFuncValue expected callable executable type"),
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
                .hosted_fn => try self.hosted_fn_symbols.put(def.bind.symbol, {}),
                else => {},
            }
        }
    }

    fn lowerHostedDef(
        self: *Lowerer,
        bind: solved.Ast.TypedSymbol,
        hosted_fn: solved.Ast.HostedFnDef,
    ) std.mem.Allocator.Error!ast.Def {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        const solved_fn_ty = try self.cloneInstType(&inst, bind.ty);
        const hosted_args = self.input.store.sliceTypedSymbolSpan(hosted_fn.args);
        const lowered_args = try self.allocator.alloc(ast.TypedSymbol, hosted_args.len);
        defer self.allocator.free(lowered_args);

        var current_fn_ty = solved_fn_ty;
        for (hosted_args, 0..) |arg, i| {
            const fn_parts = lower_type.extractFn(&self.input.types, current_fn_ty);
            lowered_args[i] = .{
                .ty = try self.lowerExecutableTypeFromSolved(&mono_cache, fn_parts.arg),
                .symbol = arg.symbol,
            };
            current_fn_ty = fn_parts.ret;
        }

        return .{
            .bind = bind.symbol,
            .result_ty = try self.lowerExecutableTypeFromSolved(&mono_cache, current_fn_ty),
            .value = .{ .hosted_fn = .{
                .bind = bind.symbol,
                .args = try self.output.addTypedSymbolSpan(lowered_args),
                .hosted = hosted_fn.hosted,
            } },
        };
    }

    fn requireLambdaDiscriminant(
        _: *const Lowerer,
        lambdas: []const solved.Type.Lambda,
        symbol: Symbol,
    ) u16 {
        for (lambdas, 0..) |lambda, i| {
            if (lambda.symbol == symbol) return @intCast(i);
        }
        debugPanic("lambdamono.lower.requireLambdaDiscriminant missing lambda");
    }

    fn buildLsetSpecializationSig(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_capture_ty: ?type_mod.TypeId,
        requested_name: Symbol,
        requested_ty: TypeVarId,
        arg_ty_override: ?type_mod.TypeId,
        ret_ty_override: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!specializations.SigKey {
        const requested_fn = lower_type.extractFn(&self.input.types, requested_ty);
        const builtin_boundary = self.boxBoundaryBuiltinOp(requested_name);
        const arg_ty = if (arg_ty_override) |override|
            if (builtin_boundary) |op|
                switch (op) {
                    .box_box => try self.eraseBoundaryExecutableType(try self.internExecutableType(override)),
                    .box_unbox => try self.eraseBoundaryBoxedExecutableType(try self.internExecutableType(override)),
                    else => unreachable,
                }
            else
                try self.internExecutableType(override)
        else if (builtin_boundary) |op|
            switch (op) {
                .box_box => try self.lowerBoxBoundaryCallableType(mono_cache, requested_fn.arg),
                .box_unbox => try self.lowerBoxedBoundaryCallableType(mono_cache, requested_fn.arg),
                else => unreachable,
            }
        else
            try self.lowerExecutableTypeFromSolved(mono_cache, requested_fn.arg);
        const ret_ty = if (builtin_boundary) |op|
            try self.boxBoundaryResultTypeFromArg(op, arg_ty)
        else if (ret_ty_override) |override|
            try self.internExecutableType(override)
        else
            try self.lowerExecutableTypeFromSolved(mono_cache, requested_fn.ret);
        const captures: specializations.CaptureSpec = if (requested_capture_ty) |capture_ty|
            .{ .lset = capture_ty }
        else
            .toplevel;
        return specializations.makeSigKey(requested_name, arg_ty, ret_ty, captures);
    }

    fn cloneFreshType(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        return self.cloneInstType(&inst, ty);
    }

    fn buildErasedSpecializationSig(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_name: Symbol,
        requested_ty: TypeVarId,
        capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!specializations.SigKey {
        const requested_fn = lower_type.extractFn(&self.input.types, requested_ty);
        const builtin_boundary = self.boxBoundaryBuiltinOp(requested_name);
        const arg_ty = if (builtin_boundary) |op|
            switch (op) {
                .box_box => try self.lowerBoxBoundaryCallableType(mono_cache, requested_fn.arg),
                .box_unbox => try self.lowerBoxedBoundaryCallableType(mono_cache, requested_fn.arg),
                else => unreachable,
            }
        else
            try self.lowerExecutableTypeFromSolved(mono_cache, requested_fn.arg);
        const ret_ty = if (builtin_boundary) |op|
            try self.boxBoundaryResultTypeFromArg(op, arg_ty)
        else
            try self.lowerExecutableTypeFromSolved(mono_cache, requested_fn.ret);
        const capture_ty_or_erased = capture_ty orelse try self.makePrimitiveType(.erased);
        const captures: specializations.CaptureSpec = .{ .erased = capture_ty_or_erased };
        return specializations.makeSigKey(requested_name, arg_ty, ret_ty, captures);
    }

    fn boxBoundaryBuiltinOp(self: *const Lowerer, requested_name: Symbol) ?base.LowLevel {
        const entry = specializations.lookupFnExact(self.fenv, requested_name) orelse return null;
        if (self.input.store.sliceTypedSymbolSpan(entry.fn_def.captures).len != 0) return null;
        const body = self.input.store.getExpr(entry.fn_def.body);
        return switch (body.data) {
            .low_level => |ll| blk: {
                const args = self.input.store.sliceExprSpan(ll.args);
                if (args.len != 1) break :blk null;
                const arg_expr = self.input.store.getExpr(args[0]);
                if (arg_expr.data != .var_ or arg_expr.data.var_ != entry.fn_def.arg.symbol) break :blk null;
                break :blk switch (ll.op) {
                    .box_box, .box_unbox => ll.op,
                    else => null,
                };
            },
            else => null,
        };
    }

    fn sourceSymbolFromOrigin(origin: symbol_mod.BindingOrigin) ?Symbol {
        return switch (origin) {
            .specialized_top_level_def => |info| Symbol.fromRaw(info.source_symbol),
            .specialized_local_fn => |info| Symbol.fromRaw(info.source_symbol),
            .lifted_local_fn => |info| Symbol.fromRaw(info.source_symbol),
            .lifted_local_fn_alias => |info| Symbol.fromRaw(info.source_symbol),
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

    fn lowerSolvedNominalArgs(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        args_span: solved.Type.Span(TypeVarId),
    ) std.mem.Allocator.Error![]const type_mod.TypeId {
        const args = self.input.types.sliceTypeVarSpan(args_span);
        const lowered_args = try self.allocator.alloc(type_mod.TypeId, args.len);
        defer self.allocator.free(lowered_args);
        for (args, 0..) |arg, i| {
            lowered_args[i] = try self.lowerExecutableTypeFromSolved(mono_cache, arg);
        }
        return try self.types.dupeTypeIds(lowered_args);
    }

    fn lowerBoxBoundaryCallableType(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerBoxBoundaryCallableType(mono_cache, nominal.backing);
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .func => switch (lower_type.lambdaRepr(&self.input.types, id)) {
                    .lset => |lambdas| try self.makeErasedFnType(
                        try self.commonErasedCaptureType(mono_cache, id, lambdas),
                    ),
                    .erased => try self.makeErasedFnType(null),
                },
                .lambda_set => switch (lower_type.lambdaRepr(&self.input.types, id)) {
                    .lset => |lambdas| try self.makeErasedFnType(
                        try self.commonErasedCaptureType(mono_cache, id, lambdas),
                    ),
                    .erased => try self.makeErasedFnType(null),
                },
                else => try self.lowerExecutableTypeFromSolved(mono_cache, id),
            },
            .link => unreachable,
            .unbd,
            .for_a,
            => debugPanic("lambdamono.lower.lowerBoxBoundaryCallableType unbound type survived instantiation"),
        };
    }

    fn lowerBoxedBoundaryCallableType(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerBoxedBoundaryCallableType(mono_cache, nominal.backing);
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .box => |elem| blk: {
                    const lowered_elem = try self.lowerBoxBoundaryCallableType(mono_cache, elem);
                    break :blk try self.internExecutableType(try self.types.internResolved(.{ .box = lowered_elem }));
                },
                else => try self.lowerExecutableTypeFromSolved(mono_cache, id),
            },
            .link => unreachable,
            .unbd,
            .for_a,
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
                try self.makeErasedFnType(try self.commonErasedCaptureTypeFromExecutable(tag_union.tags))
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

    fn lowerExecutableTypeWithBoxErasure(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, ty);
    }

    fn lowerExecutableTypeWithBoxErasureRec(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = self.unlinkExecutableTypeVar(ty);
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

        const lowered: type_mod.Content = switch (self.input.types.getNode(id)) {
            .link => unreachable,
            .nominal => |nominal| .{ .nominal = .{
                .module_idx = nominal.module_idx,
                .ident = nominal.ident,
                .is_opaque = nominal.is_opaque,
                .args = try self.lowerSolvedNominalArgs(mono_cache, nominal.args),
                .backing = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, nominal.backing),
            } },
            .for_a, .unbd => .{ .record = .{ .fields = &.{} } },
            .content => |content| switch (content) {
                .func => blk: {
                    if (self.maybeLambdaRepr(id)) |repr| switch (repr) {
                        .lset => |lambdas| {
                            const common_capture = try self.commonErasedCaptureType(mono_cache, id, lambdas);
                            break :blk .{ .erased_fn = common_capture };
                        },
                        .erased => {},
                    };
                    break :blk .{ .erased_fn = null };
                },
                .primitive => |prim| .{ .primitive = prim },
                .list => |elem| .{
                    .list = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, elem),
                },
                .box => |elem| blk: {
                    if (self.maybeLambdaRepr(elem)) |repr| switch (repr) {
                        .lset => |lambdas| {
                            const common_capture = try self.commonErasedCaptureType(mono_cache, elem, lambdas);
                            const erased_ty = try self.makeErasedFnType(common_capture);
                            break :blk .{ .box = erased_ty };
                        },
                        .erased => {},
                    };
                    break :blk .{ .box = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, elem) };
                },
                .tuple => |tuple| blk: {
                    const elems = self.input.types.sliceTypeVarSpan(tuple);
                    const lowered_elems = try self.types.allocator.alloc(type_mod.TypeId, elems.len);
                    defer self.types.allocator.free(lowered_elems);
                    for (elems, 0..) |elem, i| {
                        lowered_elems[i] = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, elem);
                    }
                    break :blk .{ .tuple = try self.types.dupeTypeIds(lowered_elems) };
                },
                .record => |record| blk: {
                    const fields = self.input.types.sliceFields(record.fields);
                    const out = try self.types.allocator.alloc(type_mod.Field, fields.len);
                    defer self.types.allocator.free(out);
                    for (fields, 0..) |field, i| {
                        out[i] = .{
                            .name = field.name,
                            .ty = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, field.ty),
                        };
                    }
                    assertSortedFields(&self.input.idents, out);
                    break :blk .{ .record = .{
                        .fields = try self.types.dupeFields(out),
                    } };
                },
                .tag_union => |tag_union| blk: {
                    const tags = self.input.types.sliceTags(tag_union.tags);
                    const out = try self.types.allocator.alloc(type_mod.Tag, tags.len);
                    defer self.types.allocator.free(out);
                    defer for (out[0..tags.len]) |tag| {
                        if (tag.args.len > 0) self.types.allocator.free(tag.args);
                    };
                    for (tags, 0..) |tag, i| {
                        const args = self.input.types.sliceTypeVarSpan(tag.args);
                        const lowered_args = try self.types.allocator.alloc(type_mod.TypeId, args.len);
                        defer self.types.allocator.free(lowered_args);
                        for (args, 0..) |arg, arg_i| {
                            lowered_args[arg_i] = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, arg);
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
                .lambda_set => |span| try self.lowerLambdaSetWithBoxErasure(mono_cache, self.input.types.sliceLambdas(span)),
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

    fn lowerLambdaSetWithBoxErasure(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        lambdas: []const solved.Type.Lambda,
    ) std.mem.Allocator.Error!type_mod.Content {
        const copied_lambdas = try self.types.allocator.dupe(solved.Type.Lambda, lambdas);
        defer self.types.allocator.free(copied_lambdas);

        const out = try self.types.allocator.alloc(type_mod.Tag, copied_lambdas.len);
        defer self.types.allocator.free(out);
        defer for (out[0..copied_lambdas.len]) |tag| {
            if (tag.args.len > 0) self.types.allocator.free(tag.args);
        };

        for (copied_lambdas, 0..) |lambda, i| {
            const captures = self.input.types.sliceCaptures(lambda.captures);
            if (captures.len == 0) {
                out[i] = .{
                    .name = .{ .lambda = lambda.symbol },
                    .args = &.{},
                };
            } else {
                const captures_ty = try self.lowerCapturesWithBoxErasure(mono_cache, captures);
                const args = try self.types.allocator.alloc(type_mod.TypeId, 1);
                defer self.types.allocator.free(args);
                args[0] = captures_ty;
                out[i] = .{
                    .name = .{ .lambda = lambda.symbol },
                    .args = try self.types.dupeTypeIds(args),
                };
            }
        }

        return .{ .tag_union = .{
            .tags = try self.types.dupeTags(out),
        } };
    }

    fn lowerCapturesWithBoxErasure(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        captures: []const solved.Type.Capture,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const capture_bindings = try self.types.allocator.alloc(lower_type.CaptureBinding, captures.len);
        defer self.types.allocator.free(capture_bindings);

        for (captures, 0..) |capture, i| {
            capture_bindings[i] = .{
                .symbol = capture.symbol,
                .solved_ty = capture.ty,
                .lowered_ty = try self.lowerExecutableTypeWithBoxErasureRec(mono_cache, capture.ty),
            };
        }

        return try lower_type.lowerCaptureBindings(
            &self.input.types,
            &self.types,
            mono_cache,
            capture_bindings,
            &self.input.symbols,
        );
    }

    fn unlinkExecutableTypeVar(self: *Lowerer, ty: TypeVarId) TypeVarId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => id,
            .content => |content| switch (content) {
                .func => |func| if (self.lambdaSetIsErased(func.lset)) id else self.unlinkExecutableTypeVar(func.lset),
                else => id,
            },
            else => id,
        };
    }

    fn functionResultType(self: *Lowerer, ty: TypeVarId) ?TypeVarId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |nominal| self.functionResultType(nominal.backing),
            .content => |content| switch (content) {
                .func => |func| func.ret,
                else => null,
            },
            else => null,
        };
    }

    fn lambdaSetIsErased(self: *Lowerer, lset: TypeVarId) bool {
        const id = self.input.types.unlink(lset);
        return switch (self.input.types.getNode(id)) {
            .content => |content| switch (content) {
                .primitive => |prim| prim == .erased,
                else => false,
            },
            else => false,
        };
    }

    fn lowerExecutableTypeNode(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.lowerExecutableTypeWithBoxErasure(mono_cache, ty);
    }

    fn lowerErasedFnExecutableType(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        solved_ty: TypeVarId,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const id = self.input.types.unlinkPreservingNominal(solved_ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |nominal| blk: {
                const lowered_backing = try self.lowerErasedFnExecutableType(mono_cache, nominal.backing) orelse break :blk null;
                break :blk try self.internExecutableType(try self.types.internResolved(.{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.lowerSolvedNominalArgs(mono_cache, nominal.args),
                    .backing = lowered_backing,
                } }));
            },
            .content => |content| switch (content) {
                .func => switch (lower_type.lambdaRepr(&self.input.types, solved_ty)) {
                    .erased => try self.makeErasedFnType(null),
                    .lset => null,
                },
                else => null,
            },
            else => null,
        };
    }

    fn lowerExecutableTypeFromSolved(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        solved_ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const lowered = if (self.boxPayloadRequiresErasure(solved_ty))
            try self.lowerBoxedBoundaryCallableType(mono_cache, solved_ty)
        else if (try self.lowerErasedFnExecutableType(mono_cache, solved_ty)) |erased|
            erased
        else
            try self.internExecutableType(try self.lowerExecutableTypeNode(mono_cache, solved_ty));
        return try self.internExecutableType(lowered);
    }

    fn internExecutableType(self: *Lowerer, ty: type_mod.TypeId) std.mem.Allocator.Error!type_mod.TypeId {
        if (!self.types.isFullyResolved(ty)) {
            debugPanic("lambdamono output invariant violated: unresolved executable type escaped stage boundary");
        }
        return try self.types.internTypeId(ty);
    }

    fn lowerExprType(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const expr = self.input.store.getExpr(expr_id);
        const solved_ty = try self.cloneInstType(inst, expr.ty);
        return try self.lowerExecutableTypeFromSolved(mono_cache, solved_ty);
    }

    fn lowerPatternType(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat_id: solved.Ast.PatId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const pat = self.input.store.getPat(pat_id);
        const solved_ty = try self.cloneInstType(inst, pat.ty);
        return try self.lowerExecutableTypeFromSolved(mono_cache, solved_ty);
    }

    fn boxedPayloadType(self: *Lowerer, ty: TypeVarId) ?TypeVarId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |nominal| self.boxedPayloadType(nominal.backing),
            .content => |content| switch (content) {
                .box => |elem| elem,
                else => null,
            },
            else => null,
        };
    }

    fn boxPayloadRequiresErasure(self: *Lowerer, ty: TypeVarId) bool {
        const payload = self.boxedPayloadType(ty) orelse return false;
        const payload_id = self.input.types.unlink(payload);
        return switch (self.input.types.getNode(payload_id)) {
            .content => |content| switch (content) {
                .func => switch (lower_type.lambdaRepr(&self.input.types, payload)) {
                    .lset => true,
                    .erased => false,
                },
                .lambda_set => switch (lower_type.lambdaRepr(&self.input.types, payload)) {
                    .lset => true,
                    .erased => false,
                },
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

    fn orderedRecordFieldsByName(
        self: *Lowerer,
        fields_span: ast.Span(ast.FieldExpr),
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const fields = self.output.sliceFieldExprSpan(fields_span);
        if (fields.len <= 1) return fields_span;

        const OrderedField = struct {
            name: base.Ident.Idx,
            field: ast.FieldExpr,
        };
        const ordered = try self.allocator.alloc(OrderedField, fields.len);
        defer self.allocator.free(ordered);
        for (fields, 0..) |field, i| {
            ordered[i] = .{
                .name = field.name,
                .field = field,
            };
        }
        std.mem.sort(OrderedField, ordered, &self.input.idents, struct {
            fn lessThan(idents: *const base.Ident.Store, a: OrderedField, b: OrderedField) bool {
                return std.mem.lessThan(
                    u8,
                    idents.getText(a.name),
                    idents.getText(b.name),
                );
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
        return switch (self.types.getTypePreservingNominal(ty)) {
            .erased_fn => true,
            .nominal => |nominal| self.containsErasedFn(nominal.backing),
            .box => |elem| self.containsErasedFn(elem),
            else => false,
        };
    }

    fn preferErasedExecType(
        self: *Lowerer,
        expected_ty: type_mod.TypeId,
        actual_ty: type_mod.TypeId,
    ) bool {
        const expected_content = self.types.getTypePreservingNominal(expected_ty);
        const actual_content = self.types.getTypePreservingNominal(actual_ty);
        return switch (expected_content) {
            .tag_union => |tag_union| self.tagUnionIsInternalLambdaSet(tag_union.tags) and actual_content == .erased_fn,
            .erased_fn => self.erasedFnCaptureType(expected_ty) == null and self.erasedFnCaptureType(actual_ty) != null,
            .nominal => |nominal| if (self.types.getTypePreservingNominal(nominal.backing) == .erased_fn)
                self.erasedFnCaptureType(expected_ty) == null and self.erasedFnCaptureType(actual_ty) != null
            else
                false,
            else => false,
        };
    }

    fn unwrapNominalTypeId(self: *Lowerer, ty: type_mod.TypeId) type_mod.TypeId {
        var current = ty;
        while (true) {
            switch (self.types.getTypePreservingNominal(current)) {
                .nominal => |nominal| current = nominal.backing,
                .link => unreachable,
                else => return current,
            }
        }
    }

    fn erasedFnCaptureType(self: *Lowerer, ty: type_mod.TypeId) ?type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |nominal| self.erasedFnCaptureType(nominal.backing),
            .erased_fn => |capture_ty| capture_ty,
            else => null,
        };
    }

    fn requireCaptureFieldExecutableType(
        self: *const Lowerer,
        capture_record_ty: type_mod.TypeId,
        symbol: Symbol,
    ) type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(capture_record_ty)) {
            .nominal => |nominal| self.requireCaptureFieldExecutableType(nominal.backing, symbol),
            .record => |record| blk: {
                const capture_name = self.input.symbols.get(symbol).name;
                for (record.fields) |field| {
                    if (field.name == capture_name) break :blk field.ty;
                }
                debugPanic("lambdamono.lower.requireCaptureFieldExecutableType missing capture field");
            },
            else => debugPanic("lambdamono.lower.requireCaptureFieldExecutableType expected capture record"),
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

    fn lookupTopLevelValueSource(self: *Lowerer, symbol: Symbol) ?TopLevelValueSource {
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

    fn specializeStandaloneExpr(self: *Lowerer, expr_id: solved.Ast.ExprId) std.mem.Allocator.Error!ast.ExprId {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();
        return try self.specializeExpr(&inst, &mono_cache, &.{}, expr_id);
    }

    const SpecializedStandaloneValue = struct {
        symbol: Symbol,
        expr: ast.ExprId,
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
        return .{
            .symbol = bind.symbol,
            .expr = try self.specializeExpr(&inst, &mono_cache, &.{}, expr_id),
        };
    }

    fn specializeFn(self: *Lowerer, pending: specializations.Pending) std.mem.Allocator.Error!ast.FnDef {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        const bridge_erased_ret = self.containsErasedFn(pending.sig.ret_ty);
        const t = try self.cloneInstType(&inst, pending.fn_ty);
        const requested_ty = try self.cloneInstType(&inst, pending.requested_ty);
        try self.unify(t, requested_ty);
        const specialized_fn = lower_type.extractFn(&self.input.types, t);

        const result: ast.FnDef = switch (lower_type.lambdaRepr(&self.input.types, t)) {
            .lset => switch (try lower_type.extractLsetFn(&self.input.types, &self.types, &mono_cache, t, pending.name, &self.input.symbols)) {
                .toplevel => .{
                    .args = blk: {
                        break :blk try self.output.addTypedSymbolSpan(&.{.{
                            .ty = pending.sig.arg_ty,
                            .symbol = pending.fn_def.arg.symbol,
                        }});
                    },
                    .body = blk: {
                        const body = try self.specializeExprWithExpectedExecTy(
                            &inst,
                            &mono_cache,
                            &.{.{
                                .symbol = pending.fn_def.arg.symbol,
                                .ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty),
                                .exec_ty = pending.sig.arg_ty,
                            }},
                            pending.fn_def.body,
                            pending.sig.ret_ty,
                        );
                        break :blk if (bridge_erased_ret and !self.types.equalIds(self.output.getExpr(body).ty, pending.sig.ret_ty))
                            try self.bridgeExprToExpectedType(&inst, &mono_cache, specialized_fn.ret, body, pending.sig.ret_ty)
                        else
                            body;
                    },
                },
                .lset => |capture_info| blk: {
                    const arg_ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty);
                    const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                    const capture_bindings = try self.captureBindingsFromTypedSymbolsAtType(
                        &inst,
                        pending.fn_def.captures,
                        capture_info.ty,
                    );
                    defer self.allocator.free(capture_bindings);
                    const arg_exec_ty = pending.sig.arg_ty;
                    const body_env = try self.buildFnBodyEnv(arg_ty, arg_exec_ty, pending.fn_def.arg.symbol, capture_bindings);
                    defer self.allocator.free(body_env);

                    var body = try self.specializeExprWithExpectedExecTy(
                        &inst,
                        &mono_cache,
                        body_env,
                        pending.fn_def.body,
                        pending.sig.ret_ty,
                    );
                    if (bridge_erased_ret and !self.types.equalIds(self.output.getExpr(body).ty, pending.sig.ret_ty)) {
                        body = try self.bridgeExprToExpectedType(
                            &inst,
                            &mono_cache,
                            specialized_fn.ret,
                            body,
                            pending.sig.ret_ty,
                        );
                    }
                    body = try self.bindCaptureLets(
                        captures_symbol,
                        try self.internExecutableType(capture_info.ty),
                        capture_bindings,
                        body,
                    );

                    break :blk .{
                        .args = try self.output.addTypedSymbolSpan(&.{
                            .{
                                .ty = arg_exec_ty,
                                .symbol = pending.fn_def.arg.symbol,
                            },
                            .{
                                .ty = try self.internExecutableType(capture_info.ty),
                                .symbol = captures_symbol,
                            },
                        }),
                        .body = body,
                    };
                },
            },
            .erased => blk: {
                const arg_ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty);
                const arg_exec_ty = pending.sig.arg_ty;
                const capture_span = self.input.store.sliceTypedSymbolSpan(pending.fn_def.captures);
                const capture_ty = pending.sig.capture_ty orelse
                    debugPanic("lambdamono.lower.specializeFn erased missing capture type");
                var captures: []lower_type.CaptureBinding = &.{};
                var captures_owned = false;
                if (capture_span.len != 0) {
                    switch (self.types.getTypePreservingNominal(capture_ty)) {
                        .primitive => |prim| if (prim == .erased) {
                            const entry = self.input.symbols.get(pending.name);
                            const name = if (entry.name.isNone())
                                "<none>"
                            else
                                self.input.idents.getText(entry.name);
                            debugPanicFmt(
                                "lambdamono.lower.specializeFn erased capture type is erased for {s}",
                                .{name},
                            );
                        },
                        else => {},
                    }
                    captures = try self.captureBindingsFromTypedSymbolsAtType(&inst, pending.fn_def.captures, capture_ty);
                    captures_owned = true;
                }
                defer if (captures_owned) self.allocator.free(captures);
                const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                const captures_ty = capture_ty;
                const body_env = try self.buildFnBodyEnv(arg_ty, arg_exec_ty, pending.fn_def.arg.symbol, captures);
                defer self.allocator.free(body_env);

                var body = try self.specializeExprWithExpectedExecTy(
                    &inst,
                    &mono_cache,
                    body_env,
                    pending.fn_def.body,
                    pending.sig.ret_ty,
                );
                if (bridge_erased_ret and !self.types.equalIds(self.output.getExpr(body).ty, pending.sig.ret_ty)) {
                    body = try self.bridgeExprToExpectedType(
                        &inst,
                        &mono_cache,
                        specialized_fn.ret,
                        body,
                        pending.sig.ret_ty,
                    );
                }
                if (captures.len != 0) {
                    body = try self.bindCaptureLets(captures_symbol, captures_ty, captures, body);
                }

                break :blk .{
                    .args = try self.output.addTypedSymbolSpan(&.{
                        .{
                            .ty = arg_exec_ty,
                            .symbol = pending.fn_def.arg.symbol,
                        },
                        .{
                            .ty = captures_ty,
                            .symbol = captures_symbol,
                        },
                    }),
                    .body = body,
                };
            },
        };
        return result;
    }

    fn buildFnBodyEnv(
        self: *Lowerer,
        arg_ty: TypeVarId,
        arg_exec_ty: type_mod.TypeId,
        arg_symbol: Symbol,
        captures: []const lower_type.CaptureBinding,
    ) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, captures.len + 1);
        out[0] = .{ .symbol = arg_symbol, .ty = arg_ty, .exec_ty = arg_exec_ty };
        for (captures, 0..) |capture, i| {
            out[i + 1] = .{
                .symbol = capture.symbol,
                .ty = capture.solved_ty,
                .exec_ty = capture.lowered_ty,
            };
        }
        return out;
    }

    fn bindCaptureLets(
        self: *Lowerer,
        captures_symbol: Symbol,
        capture_record_ty: type_mod.TypeId,
        captures: []const lower_type.CaptureBinding,
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
                .ty = capture.lowered_ty,
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
                        .ty = capture.lowered_ty,
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
        const expr = self.input.store.getExpr(expr_id);
        const ty = try self.cloneInstType(inst, expr.ty);
        const default_ty = try self.lowerExprType(inst, mono_cache, expr_id);
        return try self.specializeExprWithDefaultTy(inst, mono_cache, venv, expr_id, expr, ty, default_ty);
    }

    fn specializeExprWithExpectedExecTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.input.store.getExpr(expr_id);
        const ty = try self.cloneInstType(inst, expr.ty);
        return try self.specializeExprWithDefaultTy(inst, mono_cache, venv, expr_id, expr, ty, expected_exec_ty);
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
        if (self.exprNeedsRuntimeError(default_ty, expr.data)) {
            return try self.output.addExpr(.{
                .ty = default_ty,
                .data = .{ .runtime_error = try self.runtimeErrorLiteral() },
            });
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
            .tag => |tag| .{ .ty = default_ty, .data = .{ .tag = .{
                .name = .{ .ctor = tag.name },
                .discriminant = tag.discriminant,
                .args = try self.specializeExprSpan(inst, mono_cache, venv, tag.args),
            } } },
            .record => |fields| blk: {
                const lowered_fields = try self.specializeFieldSpan(inst, mono_cache, venv, fields);
                const record_ty = try self.recordTypeFromFields(lowered_fields);
                const ordered_fields = try self.orderedRecordFields(record_ty, lowered_fields);
                break :blk .{ .ty = record_ty, .data = .{ .record = ordered_fields } };
            },
            .access => |access| blk: {
                const record = try self.specializeExpr(inst, mono_cache, venv, access.record);
                const record_ty = self.output.getExpr(record).ty;
                const field_info = self.recordFieldByName(record_ty, access.field) orelse
                    debugPanic("lambdamono.lower.access missing record field");
                const field_ty = field_info.ty;
                break :blk .{ .ty = field_ty, .data = .{ .access = .{
                    .record = record,
                    .field = access.field,
                    .field_index = field_info.index,
                } } };
            },
            .let_ => |let_expr| blk: {
                const body = try self.specializeExpr(inst, mono_cache, venv, let_expr.body);
                const expected_exec_ty = try self.lowerExecutableTypeFromSolved(
                    mono_cache,
                    try self.cloneInstType(inst, let_expr.bind.ty),
                );
                var final_body = body;
                if (!self.types.equalIds(self.output.getExpr(body).ty, expected_exec_ty)) {
                    if (!self.preferErasedExecType(expected_exec_ty, self.output.getExpr(body).ty)) {
                        const source_ty = try self.cloneInstType(inst, self.input.store.getExpr(let_expr.body).ty);
                        final_body = try self.bridgeExprToExpectedType(inst, mono_cache, source_ty, body, expected_exec_ty);
                    }
                }
                const bind_exec_ty = self.output.getExpr(final_body).ty;
                const bind_ty = try self.cloneInstType(inst, let_expr.bind.ty);
                const rest_env = try self.extendEnv(venv, .{
                    .symbol = let_expr.bind.symbol,
                    .ty = bind_ty,
                    .exec_ty = bind_exec_ty,
                    .callable_expr = self.maybeCallableEnvExpr(venv, final_body),
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
                        .body = final_body,
                        .rest = rest,
                    } },
                };
            },
            .call => |call| try self.specializeCallExpr(inst, mono_cache, venv, expr_id, call),
            .method_call => |method_call| try self.specializeMethodCallExpr(
                inst,
                mono_cache,
                venv,
                method_call,
                ty,
                default_ty,
            ),
            .type_method_call => |method_call| try self.specializeTypeMethodCallExpr(
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
                    else => try self.specializeExprSpan(inst, mono_cache, venv, ll.args),
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
                    else => default_ty,
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
                const cond = try self.specializeExpr(inst, mono_cache, venv, when_expr.cond);
                const input_branch_ids = self.input.store.sliceBranchSpan(when_expr.branches);
                const lowered_branches = try self.allocator.alloc(ast.Branch, input_branch_ids.len);
                defer self.allocator.free(lowered_branches);

                for (input_branch_ids, 0..) |branch_id, i| {
                    const branch = self.input.store.getBranch(branch_id);
                    const pat_result = try self.specializePat(inst, mono_cache, branch.pat);
                    defer self.allocator.free(pat_result.additions);
                    const branch_env = try self.concatEnv(venv, pat_result.additions);
                    defer self.allocator.free(branch_env);
                    const source_branch_ty = try self.cloneInstType(inst, self.input.store.getExpr(branch.body).ty);
                    var body = try self.specializeExpr(inst, mono_cache, branch_env, branch.body);
                    if (!self.types.equalIds(self.output.getExpr(body).ty, default_ty)) {
                        body = try self.bridgeExprToExpectedType(
                            inst,
                            mono_cache,
                            source_branch_ty,
                            body,
                            default_ty,
                        );
                    }
                    lowered_branches[i] = .{
                        .pat = pat_result.pat,
                        .body = body,
                    };
                }

                const branches = try self.output.addBranchSpan(lowered_branches);
                var result_ty: ?type_mod.TypeId = null;
                for (self.output.sliceBranchSpan(branches)) |branch_id| {
                    const branch_body_ty = self.output.getExpr(self.output.getBranch(branch_id).body).ty;
                    if (result_ty) |existing| {
                        if (!self.types.equalIds(existing, branch_body_ty)) {
                            debugPanic("lambdamono.lower.specializeExpr when branch return executable types diverged");
                        }
                    } else {
                        result_ty = branch_body_ty;
                    }
                }
                break :blk .{ .ty = result_ty orelse default_ty, .data = .{ .when = .{
                    .cond = cond,
                    .branches = branches,
                } } };
            },
            .if_ => |if_expr| blk: {
                const cond = try self.specializeExpr(inst, mono_cache, venv, if_expr.cond);
                const then_source_ty = try self.cloneInstType(inst, self.input.store.getExpr(if_expr.then_body).ty);
                const else_source_ty = try self.cloneInstType(inst, self.input.store.getExpr(if_expr.else_body).ty);

                var then_body = try self.specializeExpr(inst, mono_cache, venv, if_expr.then_body);
                if (!self.types.equalIds(self.output.getExpr(then_body).ty, default_ty)) {
                    then_body = try self.bridgeExprToExpectedType(
                        inst,
                        mono_cache,
                        then_source_ty,
                        then_body,
                        default_ty,
                    );
                }

                var else_body = try self.specializeExpr(inst, mono_cache, venv, if_expr.else_body);
                if (!self.types.equalIds(self.output.getExpr(else_body).ty, default_ty)) {
                    else_body = try self.bridgeExprToExpectedType(
                        inst,
                        mono_cache,
                        else_source_ty,
                        else_body,
                        default_ty,
                    );
                }

                const then_ty = self.output.getExpr(then_body).ty;
                const else_ty = self.output.getExpr(else_body).ty;
                if (!self.types.equalIds(then_ty, else_ty)) {
                    debugPanic("lambdamono.lower.specializeExpr if branch return executable types diverged");
                }
                break :blk .{ .ty = then_ty, .data = .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                } } };
            },
            .block => |block| blk: {
                const lowered = try self.specializeBlockExpr(inst, mono_cache, venv, block);
                const result_ty = self.output.getExpr(lowered.final_expr).ty;
                break :blk .{ .ty = result_ty, .data = .{ .block = lowered } };
            },
            .tuple => |elems| blk: {
                const lowered_elems = try self.specializeExprSpan(inst, mono_cache, venv, elems);
                const tuple_ty = if (self.tupleTypeMatchesElems(default_ty, lowered_elems))
                    default_ty
                else
                    try self.tupleTypeFromElems(lowered_elems);
                break :blk .{ .ty = tuple_ty, .data = .{ .tuple = lowered_elems } };
            },
            .tag_payload => |tag_payload| .{ .ty = default_ty, .data = .{ .tag_payload = .{
                .tag_union = try self.specializeExpr(inst, mono_cache, venv, tag_payload.tag_union),
                .tag_discriminant = tag_payload.tag_discriminant,
                .payload_index = tag_payload.payload_index,
            } } },
            .tuple_access => |access| .{ .ty = default_ty, .data = .{ .tuple_access = .{
                .tuple = try self.specializeExpr(inst, mono_cache, venv, access.tuple),
                .elem_index = access.elem_index,
            } } },
            .list => |items| .{ .ty = default_ty, .data = .{ .list = try self.specializeExprSpan(inst, mono_cache, venv, items) } },
            .return_ => |ret_expr| .{ .ty = default_ty, .data = .{ .return_ = try self.specializeExpr(inst, mono_cache, venv, ret_expr) } },
            .runtime_error => |msg| .{ .ty = default_ty, .data = .{ .runtime_error = msg } },
            .for_ => |for_expr| .{ .ty = default_ty, .data = .{ .for_ = try self.specializeForExpr(inst, mono_cache, venv, for_expr) } },
        };
        return try self.output.addExpr(.{ .ty = specialized.ty, .data = specialized.data });
    }

    fn runtimeErrorLiteral(self: *Lowerer) std.mem.Allocator.Error!base.StringLiteral.Idx {
        return self.internStringLiteral("runtime error");
    }

    fn exprNeedsRuntimeError(self: *Lowerer, exec_ty: type_mod.TypeId, data: solved.Ast.Expr.Data) bool {
        const prim = switch (self.types.getType(exec_ty)) {
            .primitive => |prim| prim,
            else => return false,
        };

        return switch (data) {
            .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit => switch (prim) {
                .dec, .f32, .f64, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => false,
                .bool, .str, .erased => true,
            },
            .str_lit => prim != .str,
            .bool_lit => prim != .bool,
            else => false,
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
                const backing_expr = try self.retypeExpr(value_expr, nominal.backing);
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
        _: *Lowerer,
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

    fn ensureErasedCaptureType(self: *Lowerer, capture_ty: ?type_mod.TypeId) std.mem.Allocator.Error!type_mod.TypeId {
        return capture_ty orelse try self.makeUnitType();
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
        const source_ty = try self.cloneInstType(inst, self.input.store.getExpr(expr_id).ty);
        const lowered = try self.lowerBoxBoundaryExpr(inst, mono_cache, venv, source_ty, lowered_expr);
        var expected_exec_ty = switch (op) {
            .box_box => try self.lowerBoxBoundaryCallableType(mono_cache, source_ty),
            .box_unbox => try self.lowerBoxedBoundaryCallableType(mono_cache, source_ty),
            else => unreachable,
        };
        if (self.preferErasedExecType(expected_exec_ty, self.output.getExpr(lowered).ty)) {
            expected_exec_ty = self.output.getExpr(lowered).ty;
        }
        if (self.types.equalIds(self.output.getExpr(lowered).ty, expected_exec_ty)) {
            return lowered;
        }
        return try self.bridgeExprToExpectedType(inst, mono_cache, source_ty, lowered, expected_exec_ty);
    }

    fn lowerBoxBoundaryExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        source_ty: TypeVarId,
        lowered_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const lowered = self.output.getExpr(lowered_expr);
        if (lowered.data == .var_) {
            if (self.lookupEnvEntry(venv, lowered.data.var_)) |entry| {
                if (entry.callable_expr) |callable_expr| {
                    return try self.lowerBoxBoundaryExpr(inst, mono_cache, venv, source_ty, callable_expr);
                }
            }
        }

        const source_repr = self.maybeLambdaRepr(source_ty) orelse return lowered_expr;
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
                switch (self.types.getType(lowered.ty)) {
                    .erased_fn => return lowered_expr,
                    .tag_union => |tag_union| {
                        if (self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                            const forced_capture_ty = try self.commonErasedCaptureTypeFromExecutable(tag_union.tags);
                            return try self.lowerConcreteCallableAsErasedFromExecutable(
                                inst,
                                mono_cache,
                                source_ty,
                                lowered_expr,
                                tag_union,
                                forced_capture_ty,
                            );
                        }
                    },
                    else => {},
                }
                return lowered_expr;
            },
        }
    }

    fn maybeLambdaRepr(self: *Lowerer, ty: TypeVarId) ?lower_type.LambdaRepr {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |nominal| self.maybeLambdaRepr(nominal.backing),
            .content => |content| switch (content) {
                .func => lower_type.lambdaRepr(&self.input.types, id),
                .lambda_set => lower_type.lambdaRepr(&self.input.types, id),
                .primitive => |prim| if (prim == .erased) .{ .erased = {} } else null,
                else => null,
            },
            else => null,
        };
    }

    fn lowerConcreteCallableAsErased(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        callable_expr: ast.ExprId,
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const callable = self.output.getExpr(callable_expr);
        const lambda_members = try self.collectLsetLambdaMembers(mono_cache, source_ty);
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
            .packed_fn => callable_expr,
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
                        .cond = callable_expr,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                });
            },
        };
    }

    fn lowerConcreteCallableAsErasedFromExecutable(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        callable_expr: ast.ExprId,
        tag_union: @FieldType(type_mod.Content, "tag_union"),
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const callable = self.output.getExpr(callable_expr);
        const lambda_members = try self.collectLsetLambdaMembersFromExecutable(tag_union);
        defer self.allocator.free(lambda_members);

        return switch (callable.data) {
            .tag => |tag| blk: {
                if (tag.discriminant >= lambda_members.len) {
                    debugPanic("lambdamono.lower.lowerConcreteCallableAsErasedFromExecutable tag discriminant out of bounds");
                }
                const capture_args = self.output.sliceExprSpan(tag.args);
                const capture_expr = switch (capture_args.len) {
                    0 => null,
                    1 => capture_args[0],
                    else => debugPanic("lambdamono.lower.lowerConcreteCallableAsErasedFromExecutable expected at most one capture payload"),
                };
                break :blk try self.makeErasedPackedFnExpr(
                    inst,
                    mono_cache,
                    source_ty,
                    lambda_members[tag.discriminant].symbol,
                    capture_expr,
                    forced_capture_ty,
                );
            },
            .packed_fn => callable_expr,
            else => blk: {
                const erased_capture_ty = forced_capture_ty orelse try self.commonErasedCaptureTypeFromExecutable(tag_union.tags);
                if (erased_capture_ty == null) {
                    for (lambda_members) |lambda_member| {
                        if (lambda_member.capture_ty != null) {
                            if (builtin.mode == .Debug) {
                                debugPanic("lambdamono.lower.lowerConcreteCallableAsErasedFromExecutable missing common capture type");
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
                        .cond = callable_expr,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                });
            },
        };
    }

    fn bridgeExprToExpectedType(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        expr_id: ast.ExprId,
        expected_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.output.getExpr(expr_id);
        if (expr.ty == expected_ty) return expr_id;

        const expected_content = self.types.getTypePreservingNominal(expected_ty);
        switch (expected_content) {
            .tag_union => |expected_tag_union| {
                switch (self.types.getTypePreservingNominal(expr.ty)) {
                    .nominal => |actual_nominal| {
                        if (self.types.equalIds(actual_nominal.backing, expected_ty)) {
                            return try self.retypeExpr(expr_id, expected_ty);
                        }
                    },
                    else => {},
                }
                if (self.tagUnionIsInternalLambdaSet(expected_tag_union.tags)) {
                    switch (expr.data) {
                        .tag => |tag| switch (tag.name) {
                            .lambda => |lambda_symbol| {
                                const expected_members = try self.collectLsetLambdaMembersFromExecutable(expected_tag_union);
                                defer self.allocator.free(expected_members);

                                for (expected_members) |member| {
                                    if (member.symbol != lambda_symbol) continue;
                                    return try self.output.addExpr(.{
                                        .ty = expected_ty,
                                        .data = .{ .tag = .{
                                            .name = lower_type.lambdaTagKey(lambda_symbol),
                                            .discriminant = member.discriminant,
                                            .args = tag.args,
                                        } },
                                    });
                                }

                                debugPanic("lambdamono.lower.bridgeExprToExpectedType missing expected lambda member");
                            },
                            .ctor => {},
                        },
                        else => {},
                    }
                }
            },
            .box => |expected_elem| switch (self.types.getTypePreservingNominal(expected_elem)) {
                .erased_fn => {
                    const expr_box = self.types.getTypePreservingNominal(expr.ty);
                    const actual_elem = switch (expr_box) {
                        .box => |elem| elem,
                        else => debugPanic("lambdamono.lower.bridgeExprToExpectedType expected boxed executable source"),
                    };
                    const source_elem = self.boxedPayloadType(source_ty) orelse
                        debugPanic("lambdamono.lower.bridgeExprToExpectedType expected boxed solved source");
                    const unboxed = try self.makeLowLevelExpr(actual_elem, .box_unbox, &.{expr_id});
                    const bridged = try self.bridgeExprToExpectedType(
                        inst,
                        mono_cache,
                        source_elem,
                        unboxed,
                        expected_elem,
                    );
                    return try self.makeLowLevelExpr(expected_ty, .box_box, &.{bridged});
                },
                .nominal => |nominal| switch (self.types.getTypePreservingNominal(nominal.backing)) {
                    .erased_fn => {
                        const expr_box = self.types.getTypePreservingNominal(expr.ty);
                        const actual_elem = switch (expr_box) {
                            .box => |elem| elem,
                            else => debugPanic("lambdamono.lower.bridgeExprToExpectedType expected boxed executable source"),
                        };
                        const source_elem = self.boxedPayloadType(source_ty) orelse
                            debugPanic("lambdamono.lower.bridgeExprToExpectedType expected boxed solved source");
                        const unboxed = try self.makeLowLevelExpr(actual_elem, .box_unbox, &.{expr_id});
                        const bridged = try self.bridgeExprToExpectedType(
                            inst,
                            mono_cache,
                            source_elem,
                            unboxed,
                            expected_elem,
                        );
                        return try self.makeLowLevelExpr(expected_ty, .box_box, &.{bridged});
                    },
                    else => {},
                },
                else => {},
            },
            .list => |expected_elem| switch (self.types.getTypePreservingNominal(expr.ty)) {
                .list => |actual_elem| {
                    const expected_base = self.unwrapNominalTypeId(expected_elem);
                    const actual_base = self.unwrapNominalTypeId(actual_elem);
                    if (self.types.equalIds(expected_base, actual_base)) {
                        return try self.retypeExpr(expr_id, expected_ty);
                    }
                },
                else => {},
            },
            .tuple => |expected_tuple| switch (self.types.getTypePreservingNominal(expr.ty)) {
                .tuple => |actual_tuple| blk: {
                    const expected_elems = expected_tuple;
                    const actual_elems = actual_tuple;
                    if (expected_elems.len != actual_elems.len) break :blk;

                    const elem_exprs = try self.allocator.alloc(ast.ExprId, expected_elems.len);
                    defer self.allocator.free(elem_exprs);

                    for (expected_elems, actual_elems, 0..) |expected_elem, actual_elem, i| {
                        const elem_expr = try self.output.addExpr(.{
                            .ty = actual_elem,
                            .data = .{ .tuple_access = .{
                                .tuple = expr_id,
                                .elem_index = @intCast(i),
                            } },
                        });
                        elem_exprs[i] = if (self.types.equalIds(actual_elem, expected_elem))
                            elem_expr
                        else
                            try self.bridgeExprToExpectedType(
                                inst,
                                mono_cache,
                                source_ty,
                                elem_expr,
                                expected_elem,
                            );
                    }

                    return try self.output.addExpr(.{
                        .ty = expected_ty,
                        .data = .{ .tuple = try self.output.addExprSpan(elem_exprs) },
                    });
                },
                else => {},
            },
            .record => |expected_record| switch (self.types.getTypePreservingNominal(expr.ty)) {
                .record => |actual_record| blk: {
                    const expected_fields = expected_record.fields;
                    const actual_fields = actual_record.fields;
                    if (expected_fields.len != actual_fields.len) break :blk;

                    const field_exprs = try self.allocator.alloc(ast.FieldExpr, expected_fields.len);
                    defer self.allocator.free(field_exprs);

                    for (expected_fields, 0..) |expected_field, i| {
                        const actual_index = self.recordFieldIndexByNameAndType(
                            expr.ty,
                            expected_field.name,
                            expected_field.ty,
                        ) orelse break :blk;
                        const access_expr = try self.output.addExpr(.{
                            .ty = expected_field.ty,
                            .data = .{ .access = .{
                                .record = expr_id,
                                .field = expected_field.name,
                                .field_index = actual_index,
                            } },
                        });
                        field_exprs[i] = .{ .name = expected_field.name, .value = access_expr };
                    }

                    return try self.output.addExpr(.{
                        .ty = expected_ty,
                        .data = .{ .record = try self.output.addFieldExprSpan(field_exprs) },
                    });
                },
                else => {},
            },
            .erased_fn => {
                const source_repr = self.maybeLambdaRepr(source_ty) orelse
                    debugPanic("lambdamono.lower.bridgeExprToExpectedType erased_fn missing function source type");
                return switch (source_repr) {
                    .lset => try self.lowerConcreteCallableAsErased(
                        inst,
                        mono_cache,
                        source_ty,
                        expr_id,
                        null,
                    ),
                    .erased => switch (self.types.getType(expr.ty)) {
                        .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                            try self.lowerConcreteCallableAsErasedFromExecutable(
                                inst,
                                mono_cache,
                                source_ty,
                                expr_id,
                                tag_union,
                                null,
                            )
                        else
                            expr_id,
                        else => expr_id,
                    },
                };
            },
            .nominal => |nominal| {
                if (self.types.getTypePreservingNominal(nominal.backing) == .erased_fn) {
                    const bridged = blk: {
                        const source_repr = self.maybeLambdaRepr(source_ty) orelse
                            debugPanic("lambdamono.lower.bridgeExprToExpectedType erased_fn nominal missing function source type");
                        break :blk switch (source_repr) {
                            .lset => try self.lowerConcreteCallableAsErased(
                                inst,
                                mono_cache,
                                source_ty,
                                expr_id,
                                null,
                            ),
                            .erased => switch (self.types.getType(expr.ty)) {
                                .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                                    try self.lowerConcreteCallableAsErasedFromExecutable(
                                        inst,
                                        mono_cache,
                                        source_ty,
                                        expr_id,
                                        tag_union,
                                        null,
                                    )
                                else
                                    expr_id,
                                else => expr_id,
                            },
                        };
                    };
                    return try self.retypeExpr(bridged, expected_ty);
                }
                const bridged = if (self.types.equalIds(expr.ty, nominal.backing))
                    expr_id
                else
                    try self.bridgeExprToExpectedType(inst, mono_cache, source_ty, expr_id, nominal.backing);
                return try self.retypeExpr(bridged, expected_ty);
            },
            else => {},
        }

        const expected_tag = std.meta.activeTag(expected_content);
        const actual_content = self.types.getTypePreservingNominal(expr.ty);
        const actual_tag = std.meta.activeTag(actual_content);
        debugPanicFmt(
            "lambdamono.lower.bridgeExprToExpectedType missing explicit executable bridge (expected {s}, actual {s})",
            .{ @tagName(expected_tag), @tagName(actual_tag) },
        );
    }

    fn makeErasedPackedFnExpr(
        self: *Lowerer,
        _: *InstScope,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
        symbol: Symbol,
        capture_expr: ?ast.ExprId,
        forced_capture_ty: ?type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var capture_expr_opt = capture_expr;
        var capture_ty: ?type_mod.TypeId = forced_capture_ty orelse
            if (capture_expr) |capture| self.output.getExpr(capture).ty else null;
        if (forced_capture_ty) |forced_ty| {
            if (capture_expr_opt) |capture| {
                if (!self.types.equalIds(self.output.getExpr(capture).ty, forced_ty)) {
                    capture_expr_opt = try self.retypeExpr(capture, forced_ty);
                }
            }
        }
        if (capture_expr_opt == null) {
            if (capture_ty) |expected_ty| {
                if (self.isEmptyRecordType(expected_ty)) {
                    capture_ty = null;
                } else {
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
        const sig = try self.buildErasedSpecializationSig(
            mono_cache,
            symbol,
            requested_ty,
            capture_ty,
        );
        const specialized_symbol = try specializations.specializeFnErased(
            &self.queue,
            self.fenv,
            &self.input.symbols,
            symbol,
            requested_ty,
            sig,
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

    fn commonErasedCaptureType(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
        lambdas: []const solved.Type.Lambda,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        var common: ?type_mod.TypeId = null;
        for (lambdas) |lambda| {
            const next: ?type_mod.TypeId = switch (try lower_type.extractLsetFn(&self.input.types, &self.types, mono_cache, requested_ty, lambda.symbol, &self.input.symbols)) {
                .toplevel => null,
                .lset => |capture_info| try self.internExecutableType(capture_info.ty),
            };
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

    fn commonErasedCaptureTypeFromExecutable(
        _: *Lowerer,
        tags_span: []const type_mod.Tag,
    ) std.mem.Allocator.Error!?type_mod.TypeId {
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

    fn retypeExpr(
        self: *Lowerer,
        expr_id: ast.ExprId,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.output.getExpr(expr_id);
        return try self.output.addExpr(.{
            .ty = ty,
            .data = expr.data,
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
        if (self.isHostedFunctionSymbol(symbol)) {
            return switch (lower_type.lambdaRepr(&self.input.types, instantiated_ty)) {
                .lset => |_| blk: {
                    const precise_ty = try self.makeSingletonExecutableLambdaType(symbol, null);

                    switch (try lower_type.extractLsetFn(
                        &self.input.types,
                        &self.types,
                        mono_cache,
                        instantiated_ty,
                        symbol,
                        &self.input.symbols,
                    )) {
                        .toplevel => break :blk .{
                            .ty = precise_ty,
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(symbol),
                                .discriminant = 0,
                                .args = ast.Span(ast.ExprId).empty(),
                            } },
                        },
                        .lset => debugPanic("lambdamono.lower.specializeVarExpr hosted function had captures"),
                    }
                },
                .erased => debugPanic("lambdamono.lower.specializeVarExpr hosted function used as erased callable"),
            };
        }

        if (specializations.lookupFnExact(self.fenv, symbol)) |fn_entry| {
            return switch (lower_type.lambdaRepr(&self.input.types, instantiated_ty)) {
                .lset => |_| blk: {
                    const precise_toplevel_ty = try self.makeSingletonExecutableLambdaType(symbol, null);

                    switch (try lower_type.extractLsetFn(&self.input.types, &self.types, mono_cache, instantiated_ty, symbol, &self.input.symbols)) {
                        .toplevel => break :blk .{
                            .ty = precise_toplevel_ty,
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(symbol),
                                .discriminant = 0,
                                .args = ast.Span(ast.ExprId).empty(),
                            } },
                        },
                        .lset => |capture_info| {
                            const precise_ty = try self.makeSingletonExecutableLambdaType(symbol, try self.internExecutableType(capture_info.ty));
                            const capture_span = self.input.store.sliceTypedSymbolSpan(fn_entry.fn_def.captures);
                            const capture_bindings = if (capture_span.len != 0)
                                try self.captureBindingsFromTypedSymbolsAtType(
                                    inst,
                                    fn_entry.fn_def.captures,
                                    capture_info.ty,
                                )
                            else
                                try self.captureBindingsFromCaptures(venv, capture_info.captures);
                            defer self.allocator.free(capture_bindings);
                            const capture_record = try self.specializeCaptureRecord(capture_bindings, capture_info.ty);
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
                    }
                },
                .erased => blk: {
                    const captures = try self.captureBindingsForErasedVar(inst, venv, fn_entry.fn_def.captures);
                    defer self.allocator.free(captures);
                    const capture_ty: type_mod.TypeId = if (captures.len == 0)
                        try self.makeUnitType()
                    else
                        try self.internExecutableType(try lower_type.lowerCaptureBindings(
                            &self.input.types,
                            &self.types,
                            mono_cache,
                            captures,
                            &self.input.symbols,
                        ));
                    const sig = try self.buildErasedSpecializationSig(
                        mono_cache,
                        symbol,
                        instantiated_ty,
                        capture_ty,
                    );
                    const specialized_symbol = try specializations.specializeFnErased(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        symbol,
                        instantiated_ty,
                        sig,
                    );
                    var capture_expr: ?ast.ExprId = if (captures.len == 0)
                        null
                    else
                        try self.specializeCaptureRecord(captures, capture_ty);
                    if (capture_expr) |captures_value| {
                        const capture_box_ty = try self.types.internResolved(.{ .box = capture_ty });
                        capture_expr = try self.makeLowLevelExpr(capture_box_ty, .box_box, &.{captures_value});
                    }
                    break :blk .{
                        .ty = try self.makeErasedFnType(capture_ty),
                        .data = .{ .packed_fn = .{
                            .lambda = specialized_symbol,
                            .captures = capture_expr,
                            .capture_ty = capture_ty,
                        } },
                    };
                },
            };
        }

        if (self.lookupEnvEntry(venv, symbol)) |entry| {
            return .{
                .ty = entry.exec_ty,
                .data = .{ .var_ = symbol },
            };
        }
        if (self.lookupTopLevelValueType(symbol)) |top_level_ty| {
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
        captures: []const lower_type.CaptureBinding,
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
            const symbol = self.symbolForCaptureBinding(captures, field.name) orelse
                debugPanic("lambdamono.lower.specializeCaptureRecord missing capture field");
            const capture = self.lookupCaptureBinding(captures, symbol) orelse
                debugPanic("lambdamono.lower.specializeCaptureRecord missing capture binding");
            const capture_expr = try self.output.addExpr(.{
                .ty = capture.lowered_ty,
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

    fn lookupTopLevelFnType(self: *Lowerer, symbol: Symbol) ?TypeVarId {
        if (sourceSymbolFromOrigin(self.input.symbols.get(symbol).origin)) |source_symbol| {
            return self.lookupTopLevelFnType(source_symbol);
        }
        for (self.input.store.defsSlice()) |def| {
            if (def.bind.symbol != symbol) continue;
            return switch (def.value) {
                .fn_, .hosted_fn => def.bind.ty,
                else => null,
            };
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
            if (!matches_origin) continue;
            return switch (def.value) {
                .fn_, .hosted_fn => def.bind.ty,
                else => null,
            };
        }

        return null;
    }

    fn collectLsetLambdaMembersFromExecutable(
        self: *Lowerer,
        tag_union: @FieldType(type_mod.Content, "tag_union"),
    ) std.mem.Allocator.Error![]LambdaMemberInfo {
        const tags = tag_union.tags;
        const out = try self.allocator.alloc(LambdaMemberInfo, tags.len);
        for (tags, 0..) |tag, i| {
            const symbol = switch (tag.name) {
                .lambda => |lambda_symbol| lambda_symbol,
                .ctor => debugPanic("lambdamono.lower.collectLsetLambdaMembersFromExecutable expected lambda tag"),
            };
            const args = tag.args;
            const capture_ty = switch (args.len) {
                0 => null,
                1 => args[0],
                else => debugPanic("lambdamono.lower.collectLsetLambdaMembersFromExecutable expected at most one capture payload"),
            };
            out[i] = .{
                .symbol = symbol,
                .discriminant = @intCast(i),
                .capture_ty = capture_ty,
            };
        }
        return out;
    }

    fn collectLsetLambdaMembers(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
    ) std.mem.Allocator.Error![]LambdaMemberInfo {
        const lambdas = switch (lower_type.lambdaRepr(&self.input.types, requested_ty)) {
            .erased => debugPanic("lambdamono.lower.collectLsetLambdaMembers expected concrete lambda-set"),
            .lset => |lset| lset,
        };
        const out = try self.allocator.alloc(LambdaMemberInfo, lambdas.len);
        for (lambdas, 0..) |lambda, i| {
            const capture_ty: ?type_mod.TypeId = switch (try lower_type.extractLsetFn(&self.input.types, &self.types, mono_cache, requested_ty, lambda.symbol, &self.input.symbols)) {
                .toplevel => null,
                .lset => |capture_info| try self.internExecutableType(capture_info.ty),
            };
            out[i] = .{
                .symbol = lambda.symbol,
                .discriminant = @intCast(i),
                .capture_ty = capture_ty,
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

    fn captureBindingsFromTypedSymbols(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            const solved_ty = try self.cloneInstType(inst, capture.ty);
            const lowered_ty = try self.lowerExecutableTypeFromSolved(mono_cache, solved_ty);
            out[i] = .{
                .symbol = capture.symbol,
                .solved_ty = solved_ty,
                .lowered_ty = lowered_ty,
            };
        }
        return out;
    }

    fn captureBindingsFromCaptures(
        self: *Lowerer,
        venv: []const EnvEntry,
        captures: []const solved.Type.Capture,
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            const env_entry = self.lookupEnvEntry(venv, capture.symbol) orelse
                debugPanic("lambdamono.lower.captureBindingsFromCaptures missing capture");
            out[i] = .{
                .symbol = capture.symbol,
                .solved_ty = capture.ty,
                .lowered_ty = env_entry.exec_ty,
            };
        }
        return out;
    }

    fn captureBindingsForErasedVar(
        self: *Lowerer,
        _: *InstScope,
        venv: []const EnvEntry,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            const env_entry = self.lookupEnvEntry(venv, capture.symbol) orelse
                debugPanic("lambdamono.lower.captureBindingsForErasedVar missing capture");
            out[i] = .{
                .symbol = capture.symbol,
                .solved_ty = env_entry.ty,
                .lowered_ty = env_entry.exec_ty,
            };
        }
        return out;
    }

    fn captureBindingsFromTypedSymbolsAtType(
        self: *Lowerer,
        inst: *InstScope,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
        capture_record_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const fields = switch (self.types.getTypePreservingNominal(capture_record_ty)) {
            .nominal => |nominal| switch (self.types.getTypePreservingNominal(nominal.backing)) {
                .record => |record| record.fields,
                else => debugPanic("lambdamono.lower.captureBindingsFromTypedSymbolsAtType expected capture record"),
            },
            .record => |record| record.fields,
            else => debugPanic("lambdamono.lower.captureBindingsFromTypedSymbolsAtType expected capture record"),
        };
        const out = try self.allocator.alloc(lower_type.CaptureBinding, fields.len);
        for (fields, 0..) |field, i| {
            const symbol = self.symbolForCaptureField(captures, field.name) orelse
                debugPanic("lambdamono.lower.captureBindingsFromTypedSymbolsAtType missing capture field");
            const capture = self.lookupTypedCapture(captures, symbol) orelse
                debugPanic("lambdamono.lower.captureBindingsFromTypedSymbolsAtType missing capture symbol");
            out[i] = .{
                .symbol = symbol,
                .solved_ty = try self.cloneInstType(inst, capture.ty),
                .lowered_ty = field.ty,
            };
        }
        return out;
    }

    fn symbolForCaptureField(
        self: *Lowerer,
        captures: []const solved.Ast.TypedSymbol,
        field_name: base.Ident.Idx,
    ) ?Symbol {
        for (captures) |capture| {
            if (self.input.symbols.get(capture.symbol).name == field_name) {
                return capture.symbol;
            }
        }
        return null;
    }

    fn symbolForCaptureBinding(
        self: *Lowerer,
        captures: []const lower_type.CaptureBinding,
        field_name: base.Ident.Idx,
    ) ?Symbol {
        for (captures) |capture| {
            if (self.input.symbols.get(capture.symbol).name == field_name) {
                return capture.symbol;
            }
        }
        return null;
    }

    fn lookupCaptureBinding(
        _: *Lowerer,
        captures: []const lower_type.CaptureBinding,
        symbol: Symbol,
    ) ?lower_type.CaptureBinding {
        for (captures) |capture| {
            if (capture.symbol == symbol) return capture;
        }
        return null;
    }

    fn lookupTypedCapture(
        _: *Lowerer,
        captures: []const solved.Ast.TypedSymbol,
        symbol: Symbol,
    ) ?solved.Ast.TypedSymbol {
        for (captures) |capture| {
            if (capture.symbol == symbol) return capture;
        }
        return null;
    }

    fn directCallableSymbol(self: *const Lowerer, expr: solved.Ast.Expr) ?Symbol {
        if (expr.data != .var_) return null;
        const symbol = expr.data.var_;
        if (symbol.isNone()) return null;
        return if (specializations.lookupFnExact(self.fenv, symbol) != null) symbol else null;
    }

    fn instantiatedSourceTypeForExpr(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!TypeVarId {
        const expr = self.input.store.getExpr(expr_id);
        if (expr.data == .var_) {
            if (self.lookupEnvEntry(venv, expr.data.var_)) |entry| {
                return entry.ty;
            }
        }
        return try self.cloneInstType(inst, expr.ty);
    }

    fn primitiveMethodTypeMatches(self: *const Lowerer, prim: type_mod.Prim, type_ident: base.Ident.Idx) bool {
        const text = self.input.idents.getText(type_ident);
        return switch (prim) {
            .bool => std.mem.eql(u8, text, "Bool") or std.mem.eql(u8, text, "Builtin.Bool"),
            .str => std.mem.eql(u8, text, "Str") or std.mem.eql(u8, text, "Builtin.Str"),
            .u8 => std.mem.eql(u8, text, "U8") or std.mem.eql(u8, text, "Num.U8") or std.mem.eql(u8, text, "Builtin.Num.U8"),
            .i8 => std.mem.eql(u8, text, "I8") or std.mem.eql(u8, text, "Num.I8") or std.mem.eql(u8, text, "Builtin.Num.I8"),
            .u16 => std.mem.eql(u8, text, "U16") or std.mem.eql(u8, text, "Num.U16") or std.mem.eql(u8, text, "Builtin.Num.U16"),
            .i16 => std.mem.eql(u8, text, "I16") or std.mem.eql(u8, text, "Num.I16") or std.mem.eql(u8, text, "Builtin.Num.I16"),
            .u32 => std.mem.eql(u8, text, "U32") or std.mem.eql(u8, text, "Num.U32") or std.mem.eql(u8, text, "Builtin.Num.U32"),
            .i32 => std.mem.eql(u8, text, "I32") or std.mem.eql(u8, text, "Num.I32") or std.mem.eql(u8, text, "Builtin.Num.I32"),
            .u64 => std.mem.eql(u8, text, "U64") or std.mem.eql(u8, text, "Num.U64") or std.mem.eql(u8, text, "Builtin.Num.U64"),
            .i64 => std.mem.eql(u8, text, "I64") or std.mem.eql(u8, text, "Num.I64") or std.mem.eql(u8, text, "Builtin.Num.I64"),
            .u128 => std.mem.eql(u8, text, "U128") or std.mem.eql(u8, text, "Num.U128") or std.mem.eql(u8, text, "Builtin.Num.U128"),
            .i128 => std.mem.eql(u8, text, "I128") or std.mem.eql(u8, text, "Num.I128") or std.mem.eql(u8, text, "Builtin.Num.I128"),
            .f32 => std.mem.eql(u8, text, "F32") or std.mem.eql(u8, text, "Num.F32") or std.mem.eql(u8, text, "Builtin.Num.F32"),
            .f64 => std.mem.eql(u8, text, "F64") or std.mem.eql(u8, text, "Num.F64") or std.mem.eql(u8, text, "Builtin.Num.F64"),
            .dec => std.mem.eql(u8, text, "Dec") or std.mem.eql(u8, text, "Num.Dec") or std.mem.eql(u8, text, "Builtin.Num.Dec"),
            .erased => false,
        };
    }

    fn findPrimitiveAttachedMethodTarget(self: *Lowerer, prim: type_mod.Prim, method_name: base.Ident.Idx) Symbol {
        var iter = self.input.attached_method_index.iterator();
        var matched: ?Symbol = null;
        while (iter.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.method_ident != method_name) continue;
            if (!self.primitiveMethodTypeMatches(prim, key.type_ident)) continue;
            if (matched) |existing| {
                if (existing != entry.value_ptr.*) {
                    const type_text = self.input.idents.getText(key.type_ident);
                    const method_text = if (method_name.isNone()) "<none>" else self.input.idents.getText(method_name);
                    debugPanicFmt(
                        "lambdamono.lower.findPrimitiveAttachedMethodTarget found multiple targets for {s}.{s}",
                        .{ type_text, method_text },
                    );
                }
            } else {
                matched = entry.value_ptr.*;
            }
        }

        return matched orelse {
            const method_text = if (method_name.isNone()) "<none>" else self.input.idents.getText(method_name);
            debugPanicFmt(
                "lambdamono.lower.findPrimitiveAttachedMethodTarget missing attached method target for primitive {s}.{s}",
                .{ @tagName(prim), method_text },
            );
        };
    }

    fn findBuiltinAttachedMethodTarget(
        self: *Lowerer,
        owner: symbol_mod.BuiltinAttachedMethodOwner,
        method_name: base.Ident.Idx,
    ) Symbol {
        return self.input.builtin_attached_method_index.get(.{
            .owner = owner,
            .method_ident = method_name,
        }) orelse {
            const method_text = if (method_name.isNone()) "<none>" else self.input.idents.getText(method_name);
            debugPanicFmt(
                "lambdamono.lower.findBuiltinAttachedMethodTarget missing attached method target for builtin {s}.{s}",
                .{ @tagName(owner), method_text },
            );
        };
    }

    fn findAttachedMethodTargetForInstantiatedSource(self: *Lowerer, receiver_ty: TypeVarId, method_name: base.Ident.Idx) Symbol {
        const root = self.input.types.unlink(receiver_ty);
        const node = self.input.types.getNode(root);
        const nominal = switch (node) {
            .nominal => |nominal| nominal,
            .content => |content| switch (content) {
                .primitive => |prim| return self.findPrimitiveAttachedMethodTarget(prim, method_name),
                .list => return self.findBuiltinAttachedMethodTarget(.list, method_name),
                .box => return self.findBuiltinAttachedMethodTarget(.box, method_name),
                else => {
                    const method_text = if (method_name.isNone()) "<none>" else self.input.idents.getText(method_name);
                    debugPanicFmt(
                        "lambdamono.lower.findAttachedMethodTarget expected nominal or primitive receiver for method {s}",
                        .{method_text},
                    );
                },
            },
            else => {
                const method_text = if (method_name.isNone()) "<none>" else self.input.idents.getText(method_name);
                debugPanicFmt(
                    "lambdamono.lower.findAttachedMethodTarget expected resolved receiver for method {s}",
                    .{method_text},
                );
            },
        };
        const key = symbol_mod.AttachedMethodKey{
            .module_idx = nominal.module_idx,
            .type_ident = nominal.ident,
            .method_ident = method_name,
        };
        return self.input.attached_method_index.get(key) orelse {
            const type_text = if (nominal.ident.isNone()) "<none>" else self.input.idents.getText(nominal.ident);
            const method_text = if (method_name.isNone()) "<none>" else self.input.idents.getText(method_name);
            debugPanicFmt(
                "lambdamono.lower.findAttachedMethodTarget missing attached method target for {s}.{s}",
                .{ type_text, method_text },
            );
        };
    }

    const CallTarget = union(enum) {
        expr: ast.ExprId,
        exact_top_level: Symbol,
    };

    const AppliedCallArg = struct {
        expr: ast.ExprId,
        next_fn_ty: TypeVarId,
    };

    fn applyExactTopLevelFunctionArg(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        target_symbol: Symbol,
        current_fn_ty: TypeVarId,
        arg_expr: ast.ExprId,
        arg_source_ty: TypeVarId,
        result_source_ty: TypeVarId,
        result_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const arg_exec_ty = self.output.getExpr(arg_expr).ty;
        const ret_override: ?type_mod.TypeId = if (self.containsErasedFn(result_exec_ty) and
            self.erasedFnCaptureType(result_exec_ty) != null)
            result_exec_ty
        else
            null;

        return switch (lower_type.lambdaRepr(&self.input.types, current_fn_ty)) {
            .lset => blk: {
                const sig = try self.buildLsetSpecializationSig(
                    mono_cache,
                    null,
                    target_symbol,
                    current_fn_ty,
                    arg_exec_ty,
                    ret_override,
                );
                var final_arg = arg_expr;
                if (!self.types.equalIds(arg_exec_ty, sig.arg_ty) and self.containsErasedFn(sig.arg_ty)) {
                    final_arg = try self.bridgeExprToExpectedType(
                        inst,
                        mono_cache,
                        arg_source_ty,
                        arg_expr,
                        sig.arg_ty,
                    );
                }

                var call_expr = try self.output.addExpr(.{
                    .ty = sig.ret_ty,
                    .data = .{ .call = .{
                        .proc = if (self.isHostedFunctionSymbol(target_symbol))
                            target_symbol
                        else
                            try specializations.specializeFnLset(
                                &self.queue,
                                self.fenv,
                                &self.input.symbols,
                                target_symbol,
                                current_fn_ty,
                                sig,
                            ),
                        .args = try self.output.addExprSpan(&.{final_arg}),
                    } },
                });
                if (!self.types.equalIds(sig.ret_ty, result_exec_ty)) {
                    call_expr = try self.bridgeExprToExpectedType(
                        inst,
                        mono_cache,
                        result_source_ty,
                        call_expr,
                        result_exec_ty,
                    );
                }
                break :blk call_expr;
            },
            .erased => debugPanic("lambdamono.lower.applyExactTopLevelFunctionArg unexpected erased exact method target"),
        };
    }

    fn applyCallArg(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        call_target: CallTarget,
        direct_func_symbol: ?Symbol,
        current_fn_ty: TypeVarId,
        arg_expr: ast.ExprId,
        arg_source_ty: TypeVarId,
        result_source_ty: TypeVarId,
    ) std.mem.Allocator.Error!AppliedCallArg {
        const fn_parts = lower_type.extractFn(&self.input.types, current_fn_ty);
        try self.unify(fn_parts.arg, arg_source_ty);
        try self.unify(fn_parts.ret, result_source_ty);
        const result_exec_ty = try self.lowerExecutableTypeFromSolved(mono_cache, result_source_ty);
        const expr = switch (call_target) {
            .exact_top_level => |target_symbol| try self.applyExactTopLevelFunctionArg(
                inst,
                mono_cache,
                target_symbol,
                current_fn_ty,
                arg_expr,
                arg_source_ty,
                result_source_ty,
                result_exec_ty,
            ),
            .expr => |func_expr| try self.applyFuncValue(
                inst,
                mono_cache,
                func_expr,
                current_fn_ty,
                self.output.getExpr(func_expr).ty,
                arg_expr,
                arg_source_ty,
                result_source_ty,
                result_exec_ty,
                direct_func_symbol,
            ),
        };
        return .{
            .expr = expr,
            .next_fn_ty = fn_parts.ret,
        };
    }

    fn specializeMethodCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        method_call: @FieldType(solved.Ast.Expr.Data, "method_call"),
        result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const receiver_source_ty = try self.instantiatedSourceTypeForExpr(inst, venv, method_call.receiver);
        const target_symbol = self.findAttachedMethodTargetForInstantiatedSource(receiver_source_ty, method_call.method_name);
        const lowered_receiver = try self.specializeExpr(inst, mono_cache, venv, method_call.receiver);
        var current_fn_ty = try self.cloneFreshType(
            self.lookupTopLevelFnType(target_symbol) orelse
                debugPanic("lambdamono.lower.specializeMethodCallExpr missing target fn type"),
        );
        const first_result = try self.applyCallArg(
            inst,
            mono_cache,
            .{ .exact_top_level = target_symbol },
            null,
            current_fn_ty,
            lowered_receiver,
            receiver_source_ty,
            lower_type.extractFn(&self.input.types, current_fn_ty).ret,
        );
        var current_expr = first_result.expr;
        current_fn_ty = first_result.next_fn_ty;

        const method_args = self.input.store.sliceExprSpan(method_call.args);
        for (method_args, 0..) |arg_expr_id, i| {
            const lowered_arg = try self.specializeExpr(inst, mono_cache, venv, arg_expr_id);
            const arg_source_ty = try self.cloneInstType(inst, self.input.store.getExpr(arg_expr_id).ty);
            const applied = try self.applyCallArg(
                inst,
                mono_cache,
                .{ .expr = current_expr },
                null,
                current_fn_ty,
                lowered_arg,
                arg_source_ty,
                if (i + 1 == method_args.len)
                    result_source_ty
                else
                    lower_type.extractFn(&self.input.types, current_fn_ty).ret,
            );
            current_expr = applied.expr;
            current_fn_ty = applied.next_fn_ty;
        }

        var final_expr_id = current_expr;
        const final_expr = self.output.getExpr(current_expr);
        if (!self.types.equalIds(final_expr.ty, expected_exec_ty)) {
            if (!self.preferErasedExecType(expected_exec_ty, final_expr.ty)) {
                final_expr_id = try self.bridgeExprToExpectedType(
                    inst,
                    mono_cache,
                    result_source_ty,
                    current_expr,
                    expected_exec_ty,
                );
            }
        }
        const bridged_final_expr = self.output.getExpr(final_expr_id);
        return .{
            .ty = bridged_final_expr.ty,
            .data = bridged_final_expr.data,
        };
    }

    fn specializeTypeMethodCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        method_call: @FieldType(solved.Ast.Expr.Data, "type_method_call"),
        result_source_ty: TypeVarId,
        expected_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const dispatcher_source_ty = try self.cloneInstType(inst, method_call.dispatcher_ty);
        const target_symbol = self.findAttachedMethodTargetForInstantiatedSource(dispatcher_source_ty, method_call.method_name);
        var current_fn_ty = try self.cloneFreshType(
            self.lookupTopLevelFnType(target_symbol) orelse
                debugPanic("lambdamono.lower.specializeTypeMethodCallExpr missing target fn type"),
        );

        if (self.input.store.sliceExprSpan(method_call.args).len == 0) {
            const first_fn = lower_type.extractFn(&self.input.types, current_fn_ty);
            const unit_ty = try self.lowerExecutableTypeFromSolved(mono_cache, first_fn.arg);
            const unit_expr = try self.output.addExpr(.{
                .ty = unit_ty,
                .data = .unit,
            });
            const applied = try self.applyCallArg(
                inst,
                mono_cache,
                .{ .exact_top_level = target_symbol },
                null,
                current_fn_ty,
                unit_expr,
                first_fn.arg,
                first_fn.ret,
            );
            var final_expr_id = applied.expr;
            const final_expr = self.output.getExpr(applied.expr);
            if (!self.types.equalIds(final_expr.ty, expected_exec_ty)) {
                if (!self.preferErasedExecType(expected_exec_ty, final_expr.ty)) {
                    final_expr_id = try self.bridgeExprToExpectedType(
                        inst,
                        mono_cache,
                        result_source_ty,
                        applied.expr,
                        expected_exec_ty,
                    );
                }
            }
            const bridged_final_expr = self.output.getExpr(final_expr_id);
            return .{
                .ty = bridged_final_expr.ty,
                .data = bridged_final_expr.data,
            };
        }

        const method_args = self.input.store.sliceExprSpan(method_call.args);
        var current_expr: ?ast.ExprId = null;
        for (method_args, 0..) |arg_expr_id, i| {
            const lowered_arg = try self.specializeExpr(inst, mono_cache, venv, arg_expr_id);
            const arg_source_ty = try self.cloneInstType(inst, self.input.store.getExpr(arg_expr_id).ty);
            const applied = try self.applyCallArg(
                inst,
                mono_cache,
                if (i == 0)
                    .{ .exact_top_level = target_symbol }
                else
                    .{ .expr = current_expr.? },
                null,
                current_fn_ty,
                lowered_arg,
                arg_source_ty,
                if (i + 1 == method_args.len)
                    result_source_ty
                else
                    lower_type.extractFn(&self.input.types, current_fn_ty).ret,
            );
            current_expr = applied.expr;
            current_fn_ty = applied.next_fn_ty;
        }

        var final_expr_id = current_expr orelse
            debugPanic("lambdamono.lower.specializeTypeMethodCallExpr expected at least one rewritten arg");
        const final_expr = self.output.getExpr(final_expr_id);
        if (!self.types.equalIds(final_expr.ty, expected_exec_ty)) {
            if (!self.preferErasedExecType(expected_exec_ty, final_expr.ty)) {
                final_expr_id = try self.bridgeExprToExpectedType(
                    inst,
                    mono_cache,
                    result_source_ty,
                    final_expr_id,
                    expected_exec_ty,
                );
            }
        }
        const bridged_final_expr = self.output.getExpr(final_expr_id);
        return .{
            .ty = bridged_final_expr.ty,
            .data = bridged_final_expr.data,
        };
    }

    fn specializeCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        call_expr_id: solved.Ast.ExprId,
        call: @FieldType(solved.Ast.Expr.Data, "call"),
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const func_source = self.input.store.getExpr(call.func);
        const func_ty = try self.cloneInstType(inst, func_source.ty);
        const call_arg_source_ty = try self.cloneInstType(inst, self.input.store.getExpr(call.arg).ty);
        const call_ret_source_ty = try self.cloneInstType(inst, self.input.store.getExpr(call_expr_id).ty);

        const lowered_func = try self.specializeExpr(inst, mono_cache, venv, call.func);
        var box_boundary = if (func_source.data == .var_)
            self.boxBoundaryBuiltinOp(func_source.data.var_)
        else
            null;
        if (box_boundary == null) {
            const lowered_func_expr = self.output.getExpr(lowered_func);
            if (lowered_func_expr.data == .var_) {
                box_boundary = self.boxBoundaryBuiltinOp(lowered_func_expr.data.var_);
            }
        }
        const lowered_arg = if (box_boundary) |op|
            try self.specializeBoxBoundaryArgExpr(inst, mono_cache, venv, op, call.arg)
        else
            try self.specializeExpr(inst, mono_cache, venv, call.arg);
        const direct_func_symbol = self.directCallableSymbol(func_source);
        if (box_boundary) |op| {
            const func_parts = lower_type.extractFn(&self.input.types, func_ty);
            try self.unify(func_parts.arg, call_arg_source_ty);
            try self.unify(func_parts.ret, call_ret_source_ty);
            return .{
                .ty = try self.boxBoundaryResultTypeFromArg(op, self.output.getExpr(lowered_arg).ty),
                .data = .{ .low_level = .{
                    .op = op,
                    .args = try self.output.addExprSpan(&.{lowered_arg}),
                } },
            };
        }

        const applied = try self.applyCallArg(
            inst,
            mono_cache,
            .{ .expr = lowered_func },
            direct_func_symbol,
            func_ty,
            lowered_arg,
            call_arg_source_ty,
            call_ret_source_ty,
        );
        const lowered = self.output.getExpr(applied.expr);
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
        const lowered_ty = try self.lowerPatternType(inst, mono_cache, pat_id);

        return switch (pat.data) {
            .var_ => |symbol| .{
                .pat = try self.output.addPat(.{
                    .ty = lowered_ty,
                    .data = .{ .var_ = symbol },
                }),
                .additions = if (symbol.isNone())
                    try self.allocator.dupe(EnvEntry, &.{})
                else
                    try self.allocator.dupe(EnvEntry, &.{.{ .symbol = symbol, .ty = ty, .exec_ty = lowered_ty }}),
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
                for (source_args, 0..) |arg_pat, i| {
                    const lowered = try self.specializePat(inst, mono_cache, arg_pat);
                    lowered_args[i] = lowered.pat;
                    try additions.appendSlice(self.allocator, lowered.additions);
                    self.allocator.free(lowered.additions);
                }

                break :blk .{
                    .pat = try self.output.addPat(.{
                        .ty = lowered_ty,
                        .data = .{ .tag = .{
                            .name = .{ .ctor = tag.name },
                            .discriminant = tag.discriminant,
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
                const body = try self.specializeExpr(inst, mono_cache, venv, decl.body);
                const expected_exec_ty = try self.lowerExecutableTypeFromSolved(
                    mono_cache,
                    try self.cloneInstType(inst, decl.bind.ty),
                );
                var final_body = body;
                if (!self.types.equalIds(self.output.getExpr(body).ty, expected_exec_ty)) {
                    if (!self.preferErasedExecType(expected_exec_ty, self.output.getExpr(body).ty)) {
                        const source_ty = try self.cloneInstType(inst, self.input.store.getExpr(decl.body).ty);
                        final_body = try self.bridgeExprToExpectedType(inst, mono_cache, source_ty, body, expected_exec_ty);
                    }
                }
                const bind_exec_ty = self.output.getExpr(final_body).ty;
                const bind_ty = try self.cloneInstType(inst, decl.bind.ty);
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .decl = .{
                            .bind = .{
                                .ty = bind_exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = final_body,
                        },
                    }),
                    .env = try self.extendEnv(venv, .{
                        .symbol = decl.bind.symbol,
                        .ty = bind_ty,
                        .exec_ty = bind_exec_ty,
                        .callable_expr = self.maybeCallableEnvExpr(venv, final_body),
                    }),
                };
            },
            .var_decl => |decl| blk: {
                const body = try self.specializeExpr(inst, mono_cache, venv, decl.body);
                const expected_exec_ty = try self.lowerExecutableTypeFromSolved(
                    mono_cache,
                    try self.cloneInstType(inst, decl.bind.ty),
                );
                var final_body = body;
                if (!self.types.equalIds(self.output.getExpr(body).ty, expected_exec_ty)) {
                    if (!self.preferErasedExecType(expected_exec_ty, self.output.getExpr(body).ty)) {
                        const source_ty = try self.cloneInstType(inst, self.input.store.getExpr(decl.body).ty);
                        final_body = try self.bridgeExprToExpectedType(inst, mono_cache, source_ty, body, expected_exec_ty);
                    }
                }
                const bind_exec_ty = self.output.getExpr(final_body).ty;
                const bind_ty = try self.cloneInstType(inst, decl.bind.ty);
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .var_decl = .{
                            .bind = .{
                                .ty = bind_exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = final_body,
                        },
                    }),
                    .env = try self.extendEnv(venv, .{
                        .symbol = decl.bind.symbol,
                        .ty = bind_ty,
                        .exec_ty = bind_exec_ty,
                        .callable_expr = self.maybeCallableEnvExpr(venv, final_body),
                    }),
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

    fn specializeBranchSpan(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        span: solved.Ast.Span(solved.Ast.BranchId),
    ) std.mem.Allocator.Error!ast.Span(ast.BranchId) {
        const source = self.input.store.sliceBranchSpan(span);
        const out = try self.allocator.alloc(ast.Branch, source.len);
        defer self.allocator.free(out);

        for (source, 0..) |branch_id, i| {
            const branch = self.input.store.getBranch(branch_id);
            const pat_result = try self.specializePat(inst, mono_cache, branch.pat);
            defer self.allocator.free(pat_result.additions);
            const branch_env = try self.concatEnv(venv, pat_result.additions);
            defer self.allocator.free(branch_env);
            out[i] = .{
                .pat = pat_result.pat,
                .body = try self.specializeExpr(inst, mono_cache, branch_env, branch.body),
            };
        }

        return try self.output.addBranchSpan(out);
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

    fn specializeFieldSpan(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        span: solved.Ast.Span(solved.Ast.FieldExpr),
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const source = self.input.store.sliceFieldExprSpan(span);
        const out = try self.allocator.alloc(ast.FieldExpr, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |field, i| {
            out[i] = .{
                .name = field.name,
                .value = try self.specializeExpr(inst, mono_cache, venv, field.value),
            };
        }
        return try self.output.addFieldExprSpan(out);
    }

    fn cloneInstType(self: *Lowerer, inst: *InstScope, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        if (inst.mapping.get(id)) |cached| return cached;

        const cloned = switch (self.input.types.getNode(id)) {
            .link => unreachable,
            .for_a => try self.input.types.freshUnbd(),
            .unbd => try self.input.types.freshUnbd(),
            .nominal => |nominal| blk: {
                const placeholder = try self.input.types.freshUnbd();
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
                    .args = try self.input.types.addTypeVarSpan(cloned_args),
                    .backing = try self.cloneInstType(inst, nominal.backing),
                } };
                self.input.types.setNode(placeholder, node);
                break :blk placeholder;
            },
            .content => |content| blk: {
                const placeholder = try self.input.types.freshUnbd();
                try inst.mapping.put(id, placeholder);
                const node = switch (content) {
                    .primitive => solved.Type.Node{ .content = .{ .primitive = content.primitive } },
                    .func => solved.Type.Node{ .content = .{ .func = .{
                        .arg = try self.cloneInstType(inst, content.func.arg),
                        .lset = try self.cloneInstType(inst, content.func.lset),
                        .ret = try self.cloneInstType(inst, content.func.ret),
                    } } },
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
                            .tuple = try self.input.types.addTypeVarSpan(out),
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
                            .record = .{ .fields = try self.input.types.addFields(out) },
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
                                .args = try self.input.types.addTypeVarSpan(out_args),
                            };
                        }
                        const tags_span = try self.input.types.addTags(out);
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
                                .captures = try self.input.types.addCaptures(out_captures),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .lambda_set = try self.input.types.addLambdas(out),
                        } };
                    },
                };
                self.input.types.setNode(placeholder, node);
                break :blk placeholder;
            },
        };

        try inst.mapping.put(id, cloned);
        return cloned;
    }

    fn unify(self: *Lowerer, left: TypeVarId, right: TypeVarId) std.mem.Allocator.Error!void {
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        try self.unifyRec(left, right, &visited);
    }

    fn unifyRec(
        self: *Lowerer,
        left: TypeVarId,
        right: TypeVarId,
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!void {
        const l = self.input.types.unlink(left);
        const r = self.input.types.unlink(right);
        if (l == r) return;

        const key = (@as(u64, @intFromEnum(l)) << 32) | @as(u64, @intFromEnum(r));
        if (visited.contains(key)) return;
        try visited.put(key, {});

        switch (self.input.types.getNode(l)) {
            .unbd => {
                self.input.types.setNode(l, .{ .link = r });
                return;
            },
            .for_a => debugPanic("lambdamono.lower.unify generalized type without instantiation"),
            else => {},
        }
        switch (self.input.types.getNode(r)) {
            .unbd => {
                self.input.types.setNode(r, .{ .link = l });
                return;
            },
            .for_a => debugPanic("lambdamono.lower.unify generalized type without instantiation"),
            else => {},
        }

        const left_node = self.input.types.getNode(l);
        const right_node = self.input.types.getNode(r);
        const merged = switch (left_node) {
            .nominal => |left_nominal| switch (right_node) {
                .nominal => |right_nominal| {
                    if (left_nominal.module_idx != right_nominal.module_idx or
                        left_nominal.ident != right_nominal.ident or
                        left_nominal.is_opaque != right_nominal.is_opaque)
                    {
                        debugPanic("lambdamono.lower.unify incompatible nominal types");
                    }

                    const left_args = self.input.types.sliceTypeVarSpan(left_nominal.args);
                    const right_args = self.input.types.sliceTypeVarSpan(right_nominal.args);
                    if (left_args.len != right_args.len) {
                        debugPanic("lambdamono.lower.unify nominal arity mismatch");
                    }

                    for (left_args, 0..) |left_arg, i| {
                        try self.unifyRec(left_arg, right_args[i], visited);
                    }
                    try self.unifyRec(left_nominal.backing, right_nominal.backing, visited);
                    self.input.types.setNode(l, .{ .link = r });
                    return;
                },
                .content => {
                    try self.unifyRec(left_nominal.backing, r, visited);
                    return;
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible nodes: left={s} right={s}", .{ @tagName(left_node), @tagName(right_node) }),
            },
            .content => |left_content| switch (right_node) {
                .nominal => |right_nominal| {
                    try self.unifyRec(l, right_nominal.backing, visited);
                    return;
                },
                .content => |right_content| try self.unifyContent(left_content, right_content, visited),
                else => debugPanicFmt("lambdamono.lower.unify incompatible nodes: left={s} right={s}", .{ @tagName(left_node), @tagName(right_node) }),
            },
            else => debugPanic("lambdamono.lower.unify incompatible types"),
        };

        self.input.types.setNode(l, .{ .content = merged });
        self.input.types.setNode(r, .{ .link = l });
    }

    fn unifyContent(
        self: *Lowerer,
        left: solved.Type.Content,
        right: solved.Type.Content,
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Content {
        switch (left) {
            .primitive => |prim| switch (right) {
                .primitive => |other| {
                    if (prim != other) {
                        if (prim == .dec) return .{ .primitive = other };
                        if (other == .dec) return .{ .primitive = prim };
                        debugPanic("lambdamono.lower.unify incompatible primitives");
                    }
                    return .{ .primitive = prim };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .func => |func| switch (right) {
                .func => |other| {
                    try self.unifyRec(func.arg, other.arg, visited);
                    try self.unifyRec(func.lset, other.lset, visited);
                    try self.unifyRec(func.ret, other.ret, visited);
                    return .{ .func = .{
                        .arg = func.arg,
                        .lset = func.lset,
                        .ret = func.ret,
                    } };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .list => |elem| switch (right) {
                .list => |other| {
                    try self.unifyRec(elem, other, visited);
                    return .{ .list = elem };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .box => |elem| switch (right) {
                .box => |other| {
                    try self.unifyRec(elem, other, visited);
                    return .{ .box = elem };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .tuple => |tuple| switch (right) {
                .tuple => |other| {
                    const left_elems = self.input.types.sliceTypeVarSpan(tuple);
                    const right_elems = self.input.types.sliceTypeVarSpan(other);
                    if (left_elems.len != right_elems.len) debugPanic("lambdamono.lower.unify tuple arity mismatch");
                    for (left_elems, right_elems) |left_elem, right_elem| {
                        try self.unifyRec(left_elem, right_elem, visited);
                    }
                    return .{ .tuple = tuple };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .record => |record| switch (right) {
                .record => |other| {
                    return .{ .record = .{
                        .fields = try self.unifyRecordFields(record.fields, other.fields, visited),
                    } };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .tag_union => |tag_union| switch (right) {
                .tag_union => |other| {
                    return .{ .tag_union = .{
                        .tags = try self.unifyTags(tag_union.tags, other.tags, visited),
                    } };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
            .lambda_set => |lambda_set| switch (right) {
                .lambda_set => |other| {
                    return .{ .lambda_set = try self.unifyLambdaSet(lambda_set, other, visited) };
                },
                else => debugPanicFmt("lambdamono.lower.unify incompatible types: left={s} right={s}", .{ @tagName(left), @tagName(right) }),
            },
        }
    }

    fn unifyRecordFields(
        self: *Lowerer,
        left_span: solved.Type.Span(solved.Type.Field),
        right_span: solved.Type.Span(solved.Type.Field),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Field) {
        const left_fields = try self.allocator.dupe(solved.Type.Field, self.input.types.sliceFields(left_span));
        defer self.allocator.free(left_fields);
        const right_fields = try self.allocator.dupe(solved.Type.Field, self.input.types.sliceFields(right_span));
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
                    try self.unifyRec(left_fields[i].ty, right_fields[j].ty, visited);
                    try out.append(self.allocator, .{
                        .name = left_fields[i].name,
                        .ty = left_fields[i].ty,
                    });
                    i += 1;
                    j += 1;
                },
            }
        }

        return try self.input.types.addFields(out.items);
    }

    fn unifyTags(
        self: *Lowerer,
        left_span: solved.Type.Span(solved.Type.Tag),
        right_span: solved.Type.Span(solved.Type.Tag),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Tag) {
        const left_tags = try self.allocator.dupe(solved.Type.Tag, self.input.types.sliceTags(left_span));
        defer self.allocator.free(left_tags);
        const right_tags = try self.allocator.dupe(solved.Type.Tag, self.input.types.sliceTags(right_span));
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
                    const left_args = self.input.types.sliceTypeVarSpan(left_tags[i].args);
                    const right_args = self.input.types.sliceTypeVarSpan(right_tags[j].args);
                    if (left_args.len != right_args.len) {
                        debugPanic("lambdamono.lower.unify tag arity mismatch");
                    }
                    for (left_args, right_args) |left_arg, right_arg| {
                        try self.unifyRec(left_arg, right_arg, visited);
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

        return try self.input.types.addTags(out.items);
    }

    fn unifyLambdaSet(
        self: *Lowerer,
        left_span: solved.Type.Span(solved.Type.Lambda),
        right_span: solved.Type.Span(solved.Type.Lambda),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Lambda) {
        const left_lambdas = try self.allocator.dupe(solved.Type.Lambda, self.input.types.sliceLambdas(left_span));
        defer self.allocator.free(left_lambdas);
        const right_lambdas = try self.allocator.dupe(solved.Type.Lambda, self.input.types.sliceLambdas(right_span));
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
                .captures = try self.unifyCaptures(left_lambdas[i].captures, right_lambdas[j].captures, visited),
            });
            i += 1;
            j += 1;
        }

        return try self.input.types.addLambdas(out.items);
    }

    fn unifyCaptures(
        self: *Lowerer,
        left_span: solved.Type.Span(solved.Type.Capture),
        right_span: solved.Type.Span(solved.Type.Capture),
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!solved.Type.Span(solved.Type.Capture) {
        const left_caps = try self.allocator.dupe(solved.Type.Capture, self.input.types.sliceCaptures(left_span));
        defer self.allocator.free(left_caps);
        const right_caps = try self.allocator.dupe(solved.Type.Capture, self.input.types.sliceCaptures(right_span));
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

            try self.unifyRec(left_caps[i].ty, right_caps[j].ty, visited);
            try out.append(self.allocator, .{
                .symbol = left_caps[i].symbol,
                .ty = left_caps[i].ty,
            });
            i += 1;
            j += 1;
        }

        return try self.input.types.addCaptures(out.items);
    }

    fn lookupEnvEntry(_: *const Lowerer, venv: []const EnvEntry, symbol: Symbol) ?EnvEntry {
        for (venv) |entry| {
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
                .eq => debugPanic("lambdamono lowered duplicate record field"),
                .gt => debugPanic("lambdamono lowered record fields were not pre-sorted"),
            }
        }
    }

    fn maybeCallableEnvExpr(self: *Lowerer, venv: []const EnvEntry, expr_id: ast.ExprId) ?ast.ExprId {
        const expr = self.output.getExpr(expr_id);
        switch (self.types.getType(expr.ty)) {
            .erased_fn => return expr_id,
            .tag_union => |tag_union| {
                if (self.tagUnionIsInternalLambdaSet(tag_union.tags)) return expr_id;
            },
            else => {},
        }

        if (expr.data == .var_) {
            if (self.lookupEnvEntry(venv, expr.data.var_)) |entry| {
                return entry.callable_expr;
            }
        }

        return null;
    }

    fn lookupTopLevelValueType(self: *const Lowerer, symbol: Symbol) ?type_mod.TypeId {
        return self.top_level_value_types.get(symbol);
    }

    fn isHostedFunctionSymbol(self: *const Lowerer, symbol: Symbol) bool {
        return self.hosted_fn_symbols.contains(symbol);
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
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

fn debugPanicFmt(comptime msg: []const u8, args: anytype) noreturn {
    @branchHint(.cold);
    std.debug.panic(msg, args);
}

fn debugTodoLowLevel(op: base.LowLevel) noreturn {
    @branchHint(.cold);
    std.debug.panic("TODO lambdamono low-level op {s}", .{@tagName(op)});
}

test "lambdamono lower tests" {
    std.testing.refAllDecls(@This());
}
