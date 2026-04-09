//! Executable lowering from lambdasolved to lambdamono.

const std = @import("std");
const base = @import("base");
const solved = @import("lambdasolved");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");
const lower_type = @import("lower_type.zig");
const specializations = @import("specializations.zig");
const layout_facts = @import("layout_facts.zig");

const Symbol = symbol_mod.Symbol;
const TypeVarId = solved.Type.TypeVarId;

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    layout_facts: layout_facts.Facts,
    strings: base.StringLiteral.Store,

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.layout_facts.deinit(self.store.allocator);
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
    }
};

pub fn run(allocator: std.mem.Allocator, input: solved.Lower.Result) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, input);
    defer lowerer.deinit();
    try lowerer.lowerProgram();
    var explicit_layout_facts = try layout_facts.Facts.initEmpty(allocator, &lowerer.output);
    errdefer {
        explicit_layout_facts.deinit(allocator);
    }
    try lowerer.finalizePublishedTypes(&explicit_layout_facts);
    var result = try lowerer.finish();
    result.layout_facts = explicit_layout_facts;
    return result;
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: solved.Lower.Result,
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
        val: solved.Ast.ExprId,
        run: solved.Ast.RunDef,
    };

    const InstScope = struct {
        allocator: std.mem.Allocator,
        mapping: std.AutoHashMap(TypeVarId, TypeVarId),
        expr_type_facts: std.AutoHashMap(solved.Ast.ExprId, type_mod.TypeId),
        pat_type_facts: std.AutoHashMap(solved.Ast.PatId, type_mod.TypeId),
        binding_type_facts: std.AutoHashMap(TypeVarId, type_mod.TypeId),

        fn init(allocator: std.mem.Allocator) InstScope {
            return .{
                .allocator = allocator,
                .mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(allocator),
                .expr_type_facts = std.AutoHashMap(solved.Ast.ExprId, type_mod.TypeId).init(allocator),
                .pat_type_facts = std.AutoHashMap(solved.Ast.PatId, type_mod.TypeId).init(allocator),
                .binding_type_facts = std.AutoHashMap(TypeVarId, type_mod.TypeId).init(allocator),
            };
        }

        fn deinit(self: *InstScope) void {
            self.mapping.deinit();
            self.expr_type_facts.deinit();
            self.pat_type_facts.deinit();
            self.binding_type_facts.deinit();
        }
    };

    fn init(allocator: std.mem.Allocator, input: solved.Lower.Result) Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .types = type_mod.Store.init(allocator),
            .queue = specializations.Queue.init(allocator),
            .fenv = &.{},
            .pending_values = .empty,
            .inspect_defs = .empty,
            .inspect_helpers = std.AutoHashMap(InspectKey, Symbol).init(allocator),
            .top_level_values = std.AutoHashMap(Symbol, TopLevelValueSource).init(allocator),
            .top_level_value_types = std.AutoHashMap(Symbol, type_mod.TypeId).init(allocator),
        };
    }

    fn deinit(self: *Lowerer) void {
        self.top_level_values.deinit();
        self.top_level_value_types.deinit();
        self.inspect_helpers.deinit();
        self.inspect_defs.deinit(self.allocator);
        self.pending_values.deinit(self.allocator);
        self.queue.deinit();
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
            .layout_facts = undefined,
            .strings = self.input.strings,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.strings = .{};
        self.input.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.types = type_mod.Store.init(self.allocator);
        return result;
    }

    fn finalizeTypedSymbol(self: *Lowerer, bind: *ast.TypedSymbol) std.mem.Allocator.Error!void {
        bind.ty = try self.publishExecutableType(bind.ty);
    }

    fn finalizeExpr(self: *Lowerer, expr: *ast.Expr, expr_idx: usize) std.mem.Allocator.Error!void {
        _ = expr_idx;
        expr.ty = try self.publishExecutableType(expr.ty);
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
            def.result_ty = try self.publishExecutableType(ty);
        }
    }

    fn finalizePublishedTypes(self: *Lowerer, facts: *layout_facts.Facts) std.mem.Allocator.Error!void {
        for (self.output.typed_symbols.items, 0..) |*bind, i| {
            try self.finalizeTypedSymbol(bind);
            try facts.recordTypedSymbol(self.allocator, &self.types, i, bind.*);
        }
        for (self.output.pats.items, 0..) |*pat, i| {
            pat.ty = try self.publishExecutableType(pat.ty);
            try facts.recordPat(self.allocator, &self.types, &self.output, @enumFromInt(@as(u32, @intCast(i))), pat.*);
        }
        for (self.output.exprs.items, 0..) |*expr, i| {
            try self.finalizeExpr(expr, i);
            try facts.recordExpr(self.allocator, &self.types, &self.output, @enumFromInt(@as(u32, @intCast(i))), expr.*);
        }
        for (self.output.stmts.items) |*stmt| {
            try self.finalizeStmt(stmt);
        }
        for (self.output.defs.items, 0..) |*def, i| {
            try self.finalizeDef(def);
            const def_id: ast.DefId = @enumFromInt(@as(u32, @intCast(i)));
            const ret_ty = switch (def.value) {
                .fn_ => |fn_def| self.output.getExpr(fn_def.body).ty,
                .val => |expr_id| def.result_ty orelse self.output.getExpr(expr_id).ty,
                .run => |run_def| def.result_ty orelse self.output.getExpr(run_def.body).ty,
            };
            try facts.recordDefRet(self.allocator, &self.types, def_id, ret_ty);
        }
        try facts.finalizeRuntimeReprClasses(self.allocator);
    }

    fn lowerProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        self.fenv = try specializations.buildFEnv(self.allocator, &self.input);
        try self.indexTopLevelValues();

        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .fn_ => {},
                .val, .run => _ = try self.ensureTopLevelValueLowered(def.bind.symbol),
            }
        }

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
        for (self.inspect_defs.items) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
        for (self.pending_values.items) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
    }

    fn indexTopLevelValues(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .val => |expr_id| try self.top_level_values.put(def.bind.symbol, .{ .val = expr_id }),
                .run => |run_def| try self.top_level_values.put(def.bind.symbol, .{ .run = run_def }),
                .fn_ => {},
            }
        }
    }

    fn requireLambdaDiscriminant(
        self: *const Lowerer,
        lambdas: []const solved.Type.Lambda,
        symbol: Symbol,
    ) u16 {
        _ = self;
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
                    .box_box => try self.eraseBoundaryExecutableType(try self.publishExecutableType(override)),
                    .box_unbox => try self.eraseBoundaryBoxedExecutableType(try self.publishExecutableType(override)),
                    else => unreachable,
                }
            else
                try self.publishExecutableType(override)
        else if (builtin_boundary) |op|
            switch (op) {
                .box_box => try self.publishBoxBoundaryCallableTypeFact(mono_cache, requested_fn.arg),
                .box_unbox => try self.publishBoxedBoundaryCallableTypeFact(mono_cache, requested_fn.arg),
                else => unreachable,
            }
        else
            try self.publishExecutableTypeFact(mono_cache, requested_fn.arg);
        const ret_ty = if (builtin_boundary) |op|
            try self.boxBoundaryResultTypeFromArg(op, arg_ty)
        else if (ret_ty_override) |override|
            try self.publishExecutableType(override)
        else if (arg_ty_override) |_|
            if (specializations.lookupFn(self.fenv, requested_name) != null)
                try self.inferLsetSpecializedReturnType(
                    mono_cache,
                    requested_name,
                    requested_ty,
                    arg_ty,
                )
            else
                try self.publishExecutableTypeFact(mono_cache, requested_fn.ret)
        else
            try self.publishExecutableTypeFact(mono_cache, requested_fn.ret);
        const captures: specializations.CaptureSpec = if (requested_capture_ty) |capture_ty|
            .{ .lset = capture_ty }
        else
            .toplevel;
        return specializations.makeSigKey(requested_name, arg_ty, ret_ty, captures);
    }

    fn buildErasedSpecializationSig(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_name: Symbol,
        requested_ty: TypeVarId,
        captures_new: []const lower_type.CaptureBinding,
    ) std.mem.Allocator.Error!specializations.SigKey {
        const requested_fn = lower_type.extractFn(&self.input.types, requested_ty);
        const builtin_boundary = self.boxBoundaryBuiltinOp(requested_name);
        const arg_ty = if (builtin_boundary) |op|
            switch (op) {
                .box_box => try self.publishBoxBoundaryCallableTypeFact(mono_cache, requested_fn.arg),
                .box_unbox => try self.publishBoxedBoundaryCallableTypeFact(mono_cache, requested_fn.arg),
                else => unreachable,
            }
        else
            try self.publishExecutableTypeFact(mono_cache, requested_fn.arg);
        const ret_ty = if (builtin_boundary) |op|
            try self.boxBoundaryResultTypeFromArg(op, arg_ty)
        else
            try self.publishExecutableTypeFact(mono_cache, requested_fn.ret);
        const capture_ty = if (captures_new.len == 0)
            try self.makePrimitiveType(.erased)
        else
            try self.publishExecutableType(try lower_type.lowerCaptureBindings(
                &self.input.types,
                &self.types,
                mono_cache,
                captures_new,
                &self.input.symbols,
            ));
        const captures: specializations.CaptureSpec = .{ .erased = capture_ty };
        return specializations.makeSigKey(requested_name, arg_ty, ret_ty, captures);
    }

    fn boxBoundaryBuiltinOp(self: *const Lowerer, requested_name: Symbol) ?base.LowLevel {
        const entry = specializations.lookupFn(self.fenv, requested_name) orelse return null;
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

    fn boxBoundaryResultTypeFromArg(
        self: *Lowerer,
        op: base.LowLevel,
        arg_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (op) {
            .box_box => try self.publishExecutableType(try self.types.internResolved(.{ .box = arg_ty })),
            .box_unbox => switch (self.types.getTypePreservingNominal(arg_ty)) {
                .box => |elem| elem,
                else => debugPanic("lambdamono.lower.boxBoundaryResultTypeFromArg box_unbox expected boxed executable arg"),
            },
            else => unreachable,
        };
    }

    fn publishBoxBoundaryCallableTypeFact(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |backing| blk: {
                const lowered_backing = try self.publishBoxBoundaryCallableTypeFact(mono_cache, backing);
                break :blk try self.publishExecutableType(try self.types.internResolved(.{ .nominal = lowered_backing }));
            },
            .content => |content| switch (content) {
                .func => switch (lower_type.lambdaRepr(&self.input.types, id)) {
                    .lset => |lambdas| try self.makeErasedFnType(try self.commonErasedCaptureType(mono_cache, id, lambdas)),
                    .erased => debugPanic("lambdamono.lower.publishBoxBoundaryCallableTypeFact expected concrete lambda-set at box boundary"),
                },
                else => try self.publishExecutableTypeFact(mono_cache, id),
            },
            .link => unreachable,
            .unbd,
            .for_a,
            => try self.publishExecutableTypeFact(mono_cache, id),
        };
    }

    fn publishBoxedBoundaryCallableTypeFact(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const id = self.input.types.unlinkPreservingNominal(ty);
        return switch (self.input.types.getNode(id)) {
            .nominal => |backing| blk: {
                const lowered_backing = try self.publishBoxedBoundaryCallableTypeFact(mono_cache, backing);
                break :blk try self.publishExecutableType(try self.types.internResolved(.{ .nominal = lowered_backing }));
            },
            .content => |content| switch (content) {
                .box => |elem| blk: {
                    const lowered_elem = try self.publishBoxBoundaryCallableTypeFact(mono_cache, elem);
                    break :blk try self.publishExecutableType(try self.types.internResolved(.{ .box = lowered_elem }));
                },
                else => try self.publishExecutableTypeFact(mono_cache, id),
            },
            .link => unreachable,
            .unbd,
            .for_a,
            => try self.publishExecutableTypeFact(mono_cache, id),
        };
    }

    fn eraseBoundaryExecutableType(
        self: *Lowerer,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(ty)) {
            .nominal => |backing| try self.publishExecutableType(try self.types.internResolved(.{
                .nominal = try self.eraseBoundaryExecutableType(backing),
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
            .nominal => |backing| try self.publishExecutableType(try self.types.internResolved(.{
                .nominal = try self.eraseBoundaryBoxedExecutableType(backing),
            })),
            .box => |elem| try self.publishExecutableType(try self.types.internResolved(.{
                .box = try self.eraseBoundaryExecutableType(elem),
            })),
            else => ty,
        };
    }

    fn lowerExecutableTypeFact(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try lower_type.lowerType(&self.input.types, &self.types, mono_cache, ty, &self.input.symbols);
    }

    fn publishExecutableType(self: *Lowerer, ty: type_mod.TypeId) std.mem.Allocator.Error!type_mod.TypeId {
        if (!self.types.isFullyResolved(ty)) {
            debugPanic("lambdamono publication invariant violated: unresolved executable type escaped stage boundary");
        }
        return try self.types.canonicalizeResolved(ty);
    }

    fn publishExecutableTypeFact(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.publishExecutableType(try self.lowerExecutableTypeFact(mono_cache, ty));
    }

    fn recordExprTypeFact(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        if (inst.expr_type_facts.contains(expr_id)) return;
        const expr = self.input.store.getExpr(expr_id);
        const lowered_ty = switch (expr.data) {
            .var_ => blk: {
                const solved_ty = try self.cloneInstType(inst, expr.ty);
                if (inst.binding_type_facts.get(solved_ty)) |existing| break :blk existing;
                break :blk try self.publishExecutableTypeFact(mono_cache, solved_ty);
            },
            else => blk: {
                const solved_ty = try self.cloneInstType(inst, expr.ty);
                break :blk try self.publishExecutableTypeFact(mono_cache, solved_ty);
            },
        };
        try inst.expr_type_facts.put(expr_id, lowered_ty);
    }

    fn recordPatTypeFact(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat_id: solved.Ast.PatId,
    ) std.mem.Allocator.Error!void {
        if (inst.pat_type_facts.contains(pat_id)) return;
        const pat = self.input.store.getPat(pat_id);
        const solved_ty = try self.cloneInstType(inst, pat.ty);
        const lowered_ty = try self.publishExecutableTypeFact(mono_cache, solved_ty);
        try inst.pat_type_facts.put(pat_id, lowered_ty);
    }

    fn recordBindingTypeFact(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        bind: solved.Ast.TypedSymbol,
    ) std.mem.Allocator.Error!void {
        try self.recordBindingTypeFactFromSourceTy(inst, mono_cache, bind.ty);
    }

    fn recordBindingTypeFactFromSourceTy(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        const solved_ty = try self.cloneInstType(inst, source_ty);
        try self.recordSolvedBindingTypeFact(inst, mono_cache, solved_ty);
    }

    fn recordSolvedBindingTypeFact(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        solved_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        const lowered_ty = try self.publishExecutableTypeFact(mono_cache, solved_ty);
        if (inst.binding_type_facts.get(solved_ty)) |existing| {
            if (existing != lowered_ty) {
                debugPanic("lambdamono.lower.recordBindingTypeFact conflicting executable binding type");
            }
            return;
        }
        try inst.binding_type_facts.put(solved_ty, lowered_ty);
    }

    fn recordExplicitSolvedBindingTypeFact(
        self: *const Lowerer,
        inst: *InstScope,
        solved_ty: TypeVarId,
        lowered_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        _ = self;
        if (inst.binding_type_facts.get(solved_ty)) |existing| {
            if (existing != lowered_ty) {
                debugPanic("lambdamono.lower.recordExplicitSolvedBindingTypeFact conflicting executable binding type");
            }
            return;
        }
        try inst.binding_type_facts.put(solved_ty, lowered_ty);
    }

    fn recordBindingTypeFactFromExpr(
        self: *Lowerer,
        inst: *InstScope,
        source_ty: TypeVarId,
        body_expr: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        const solved_ty = try self.cloneInstType(inst, source_ty);
        const lowered_ty = self.requireExprTypeFact(inst, body_expr);
        if (inst.binding_type_facts.get(solved_ty)) |existing| {
            if (existing != lowered_ty) {
                debugPanic("lambdamono.lower.recordBindingTypeFactFromExpr conflicting executable binding type");
            }
            return;
        }
        try inst.binding_type_facts.put(solved_ty, lowered_ty);
    }

    fn requireExprTypeFact(
        self: *const Lowerer,
        inst: *const InstScope,
        expr_id: solved.Ast.ExprId,
    ) type_mod.TypeId {
        _ = self;
        return inst.expr_type_facts.get(expr_id) orelse
            debugPanic("lambdamono.lower.requireExprTypeFact missing explicit expr executable type");
    }

    fn requirePatTypeFact(
        self: *const Lowerer,
        inst: *const InstScope,
        pat_id: solved.Ast.PatId,
    ) type_mod.TypeId {
        _ = self;
        return inst.pat_type_facts.get(pat_id) orelse
            debugPanic("lambdamono.lower.requirePatTypeFact missing explicit pat executable type");
    }

    fn requireBindingTypeFact(
        self: *Lowerer,
        inst: *InstScope,
        source_ty: TypeVarId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const solved_ty = try self.cloneInstType(inst, source_ty);
        return self.requireSolvedTypeFact(inst, solved_ty);
    }

    fn requireSolvedTypeFact(
        self: *const Lowerer,
        inst: *const InstScope,
        solved_ty: TypeVarId,
    ) type_mod.TypeId {
        _ = self;
        return inst.binding_type_facts.get(solved_ty) orelse
            debugPanic("lambdamono.lower.requireSolvedTypeFact missing explicit executable type");
    }

    fn deriveRecordExprTypeFact(
        self: *Lowerer,
        inst: *const InstScope,
        fields_span: solved.Ast.Span(solved.Ast.FieldExpr),
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const fields = self.input.store.sliceFieldExprSpan(fields_span);
        const lowered_fields = try self.allocator.alloc(type_mod.Field, fields.len);
        defer self.allocator.free(lowered_fields);

        for (fields, 0..) |field, i| {
            lowered_fields[i] = .{
                .name = field.name,
                .ty = self.requireExprTypeFact(inst, field.value),
            };
        }

        return try self.publishExecutableType(try self.types.internResolved(.{
            .record = .{ .fields = try self.types.addFields(lowered_fields) },
        }));
    }

    fn deriveTupleExprTypeFact(
        self: *Lowerer,
        inst: *const InstScope,
        elems_span: solved.Ast.Span(solved.Ast.ExprId),
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const elems = self.input.store.sliceExprSpan(elems_span);
        const lowered_elems = try self.allocator.alloc(type_mod.TypeId, elems.len);
        defer self.allocator.free(lowered_elems);

        for (elems, 0..) |elem, i| {
            lowered_elems[i] = self.requireExprTypeFact(inst, elem);
        }

        return try self.publishExecutableType(try self.types.internResolved(.{
            .tuple = try self.types.addTypeSpan(lowered_elems),
        }));
    }

    fn requireRecordFieldExecutableType(
        self: *const Lowerer,
        record_ty: type_mod.TypeId,
        field_index: u16,
    ) type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(record_ty)) {
            .nominal => |backing| self.requireRecordFieldExecutableType(backing, field_index),
            .record => |record| self.types.sliceFields(record.fields)[field_index].ty,
            else => debugPanic("lambdamono.lower.requireRecordFieldExecutableType expected record executable type"),
        };
    }

    fn requireTupleElemExecutableType(
        self: *const Lowerer,
        tuple_ty: type_mod.TypeId,
        elem_index: u32,
    ) type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(tuple_ty)) {
            .nominal => |backing| self.requireTupleElemExecutableType(backing, elem_index),
            .tuple => |tuple| self.types.sliceTypeSpan(tuple)[elem_index],
            else => debugPanic("lambdamono.lower.requireTupleElemExecutableType expected tuple executable type"),
        };
    }

    fn requireCaptureFieldExecutableType(
        self: *const Lowerer,
        capture_record_ty: type_mod.TypeId,
        symbol: Symbol,
    ) type_mod.TypeId {
        return switch (self.types.getTypePreservingNominal(capture_record_ty)) {
            .nominal => |backing| self.requireCaptureFieldExecutableType(backing, symbol),
            .record => |record| blk: {
                const capture_name = self.input.symbols.get(symbol).name;
                for (self.types.sliceFields(record.fields)) |field| {
                    if (field.name == capture_name) break :blk field.ty;
                }
                debugPanic("lambdamono.lower.requireCaptureFieldExecutableType missing capture field");
            },
            else => debugPanic("lambdamono.lower.requireCaptureFieldExecutableType expected capture record"),
        };
    }

    fn requireExplicitCaptureBindingBySymbol(
        self: *const Lowerer,
        captures: []const lower_type.CaptureBinding,
        symbol: Symbol,
    ) lower_type.CaptureBinding {
        _ = self;
        for (captures) |capture| {
            if (capture.symbol == symbol) return capture;
        }
        debugPanic("lambdamono.lower.requireExplicitCaptureBindingBySymbol missing capture binding");
    }

    fn collectSpecializedFnExecutableFacts(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pending: specializations.Pending,
        specialized_fn_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        const arg_solved_ty = try self.cloneInstType(inst, pending.fn_def.arg.ty);
        try self.recordExplicitSolvedBindingTypeFact(inst, arg_solved_ty, pending.sig.arg_ty);

        switch (pending.sig.capture_kind) {
            .toplevel => {},
            .lset => {
                switch (try lower_type.extractLsetFn(
                    &self.input.types,
                    &self.types,
                    mono_cache,
                    specialized_fn_ty,
                    pending.name,
                    &self.input.symbols,
                )) {
                    .toplevel => {},
                    .lset => |capture_info| {
                        for (self.input.store.sliceTypedSymbolSpan(pending.fn_def.captures)) |capture| {
                            const capture_solved_ty = try self.cloneInstType(inst, capture.ty);
                            const capture_exec_ty = self.requireCaptureFieldExecutableType(capture_info.ty, capture.symbol);
                            try self.recordExplicitSolvedBindingTypeFact(inst, capture_solved_ty, capture_exec_ty);
                        }
                    },
                }
            },
            .erased => {
                const captures = pending.captures_new orelse &.{};
                for (self.input.store.sliceTypedSymbolSpan(pending.fn_def.captures)) |capture| {
                    const capture_solved_ty = try self.cloneInstType(inst, capture.ty);
                    const explicit_capture = self.requireExplicitCaptureBindingBySymbol(captures, capture.symbol);
                    try self.recordExplicitSolvedBindingTypeFact(inst, capture_solved_ty, explicit_capture.lowered_ty);
                }
            },
        }

        try self.collectExprExecutableFacts(inst, mono_cache, pending.fn_def.body);
    }

    fn inferLsetSpecializedReturnType(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_name: Symbol,
        requested_ty: TypeVarId,
        arg_exec_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!type_mod.TypeId {
        const fn_entry = specializations.lookupFn(self.fenv, requested_name) orelse
            debugPanic("lambdamono.lower.inferLsetSpecializedReturnType missing function");

        var sig_inst = InstScope.init(self.allocator);
        defer sig_inst.deinit();

        const fn_ty = try self.cloneInstType(&sig_inst, fn_entry.fn_ty);
        const requested_ty_clone = try self.cloneInstType(&sig_inst, requested_ty);
        try self.unify(fn_ty, requested_ty_clone);

        try self.collectSpecializedFnExecutableFacts(&sig_inst, mono_cache, .{
            .name = fn_entry.name,
            .fn_ty = fn_entry.fn_ty,
            .fn_def = fn_entry.fn_def,
            .requested_ty = requested_ty,
            .sig = .{
                .name = requested_name,
                .arg_ty = arg_exec_ty,
                .ret_ty = arg_exec_ty,
                .capture_kind = .lset,
                .capture_ty = null,
            },
            .specialized_symbol = requested_name,
            .captures_new = null,
            .specialized = null,
        }, fn_ty);
        return try self.publishExecutableType(self.requireExprTypeFact(&sig_inst, fn_entry.fn_def.body));
    }

    fn collectExprExecutableFacts(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        if (inst.expr_type_facts.contains(expr_id)) return;
        try self.recordExprTypeFact(inst, mono_cache, expr_id);

        const expr = self.input.store.getExpr(expr_id);
        switch (expr.data) {
            .var_,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .runtime_error,
            => {},
            .tag => |tag| {
                for (self.input.store.sliceExprSpan(tag.args)) |arg| {
                    try self.collectExprExecutableFacts(inst, mono_cache, arg);
                }
            },
            .record => |fields| {
                for (self.input.store.sliceFieldExprSpan(fields)) |field| {
                    try self.collectExprExecutableFacts(inst, mono_cache, field.value);
                }
                try inst.expr_type_facts.put(expr_id, try self.deriveRecordExprTypeFact(inst, fields));
            },
            .access => |access| {
                try self.collectExprExecutableFacts(inst, mono_cache, access.record);
                const record_ty = self.requireExprTypeFact(inst, access.record);
                try inst.expr_type_facts.put(expr_id, self.requireRecordFieldExecutableType(record_ty, access.field_index));
            },
            .let_ => |let_expr| {
                try self.collectExprExecutableFacts(inst, mono_cache, let_expr.body);
                try self.recordBindingTypeFactFromExpr(inst, let_expr.bind.ty, let_expr.body);
                try self.collectExprExecutableFacts(inst, mono_cache, let_expr.rest);
            },
            .call => |call| {
                try self.collectExprExecutableFacts(inst, mono_cache, call.func);
                try self.collectExprExecutableFacts(inst, mono_cache, call.arg);
                const func_expr = self.input.store.getExpr(call.func);
                if (func_expr.data == .var_) {
                    if (self.boxBoundaryBuiltinOp(func_expr.data.var_)) |op| {
                        const arg_ty = self.requireExprTypeFact(inst, call.arg);
                        const explicit_ty = switch (op) {
                            .box_box => try self.publishExecutableType(try self.types.internResolved(.{
                                .box = try self.eraseBoundaryExecutableType(arg_ty),
                            })),
                            .box_unbox => switch (self.types.getTypePreservingNominal(arg_ty)) {
                                .box => |elem| try self.eraseBoundaryExecutableType(elem),
                                else => debugPanic("lambdamono.lower.collectExprExecutableFacts box_unbox expected boxed arg executable fact"),
                            },
                            else => unreachable,
                        };
                        try inst.expr_type_facts.put(expr_id, explicit_ty);
                    }
                }
            },
            .inspect => |value| try self.collectExprExecutableFacts(inst, mono_cache, value),
            .low_level => |ll| {
                for (self.input.store.sliceExprSpan(ll.args)) |arg| {
                    try self.collectExprExecutableFacts(inst, mono_cache, arg);
                }
                const args = self.input.store.sliceExprSpan(ll.args);
                switch (ll.op) {
                    .box_box => {
                        if (args.len != 1) debugPanic("lambdamono.lower.collectExprExecutableFacts box_box expected one arg");
                        const arg_ty = self.requireExprTypeFact(inst, args[0]);
                        try inst.expr_type_facts.put(expr_id, try self.publishExecutableType(try self.types.internResolved(.{
                            .box = try self.eraseBoundaryExecutableType(arg_ty),
                        })));
                    },
                    .box_unbox => {
                        if (args.len != 1) debugPanic("lambdamono.lower.collectExprExecutableFacts box_unbox expected one arg");
                        const arg_ty = self.requireExprTypeFact(inst, args[0]);
                        const out_ty = switch (self.types.getTypePreservingNominal(arg_ty)) {
                            .box => |elem| try self.eraseBoundaryExecutableType(elem),
                            else => debugPanic("lambdamono.lower.collectExprExecutableFacts box_unbox expected boxed arg executable fact"),
                        };
                        try inst.expr_type_facts.put(expr_id, out_ty);
                    },
                    else => {},
                }
            },
            .when => |when_expr| {
                try self.collectExprExecutableFacts(inst, mono_cache, when_expr.cond);
                for (self.input.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.input.store.getBranch(branch_id);
                    try self.collectPatExecutableFacts(inst, mono_cache, branch.pat);
                    try self.collectExprExecutableFacts(inst, mono_cache, branch.body);
                }
            },
            .if_ => |if_expr| {
                try self.collectExprExecutableFacts(inst, mono_cache, if_expr.cond);
                try self.collectExprExecutableFacts(inst, mono_cache, if_expr.then_body);
                try self.collectExprExecutableFacts(inst, mono_cache, if_expr.else_body);
            },
            .block => |block| {
                for (self.input.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    try self.collectStmtExecutableFacts(inst, mono_cache, stmt_id);
                }
                try self.collectExprExecutableFacts(inst, mono_cache, block.final_expr);
            },
            .tuple => |tuple| {
                for (self.input.store.sliceExprSpan(tuple)) |elem| {
                    try self.collectExprExecutableFacts(inst, mono_cache, elem);
                }
                try inst.expr_type_facts.put(expr_id, try self.deriveTupleExprTypeFact(inst, tuple));
            },
            .tuple_access => |tuple_access| {
                try self.collectExprExecutableFacts(inst, mono_cache, tuple_access.tuple);
                const tuple_ty = self.requireExprTypeFact(inst, tuple_access.tuple);
                try inst.expr_type_facts.put(expr_id, self.requireTupleElemExecutableType(tuple_ty, tuple_access.elem_index));
            },
            .list => |items| {
                for (self.input.store.sliceExprSpan(items)) |item| {
                    try self.collectExprExecutableFacts(inst, mono_cache, item);
                }
            },
            .return_ => |ret| try self.collectExprExecutableFacts(inst, mono_cache, ret),
            .for_ => |for_expr| {
                try self.collectPatExecutableFacts(inst, mono_cache, for_expr.patt);
                try self.collectExprExecutableFacts(inst, mono_cache, for_expr.iterable);
                try self.collectExprExecutableFacts(inst, mono_cache, for_expr.body);
            },
        }
    }

    fn collectPatExecutableFacts(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat_id: solved.Ast.PatId,
    ) std.mem.Allocator.Error!void {
        if (inst.pat_type_facts.contains(pat_id)) return;
        try self.recordPatTypeFact(inst, mono_cache, pat_id);

        const pat = self.input.store.getPat(pat_id);
        switch (pat.data) {
            .var_ => {},
            .bool_lit => {},
            .tag => |tag| {
                for (self.input.store.slicePatSpan(tag.args)) |arg| {
                    try self.collectPatExecutableFacts(inst, mono_cache, arg);
                }
            },
        }
    }

    fn collectStmtExecutableFacts(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        stmt_id: solved.Ast.StmtId,
    ) std.mem.Allocator.Error!void {
        const stmt = self.input.store.getStmt(stmt_id);
        switch (stmt) {
            .decl => |decl| {
                try self.collectExprExecutableFacts(inst, mono_cache, decl.body);
                try self.recordBindingTypeFactFromExpr(inst, decl.bind.ty, decl.body);
            },
            .var_decl => |decl| {
                try self.collectExprExecutableFacts(inst, mono_cache, decl.body);
                try self.recordBindingTypeFactFromExpr(inst, decl.bind.ty, decl.body);
            },
            .reassign => |reassign| try self.collectExprExecutableFacts(inst, mono_cache, reassign.body),
            .expr => |expr_id| try self.collectExprExecutableFacts(inst, mono_cache, expr_id),
            .debug => |expr_id| try self.collectExprExecutableFacts(inst, mono_cache, expr_id),
            .expect => |expr_id| try self.collectExprExecutableFacts(inst, mono_cache, expr_id),
            .crash => {},
            .return_ => |expr_id| try self.collectExprExecutableFacts(inst, mono_cache, expr_id),
            .break_ => {},
            .for_ => |for_stmt| {
                try self.collectPatExecutableFacts(inst, mono_cache, for_stmt.patt);
                try self.collectExprExecutableFacts(inst, mono_cache, for_stmt.iterable);
                try self.collectExprExecutableFacts(inst, mono_cache, for_stmt.body);
            },
            .while_ => |while_stmt| {
                try self.collectExprExecutableFacts(inst, mono_cache, while_stmt.cond);
                try self.collectExprExecutableFacts(inst, mono_cache, while_stmt.body);
            },
        }
    }

    fn collectFnExecutableFacts(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        fn_def: solved.Ast.FnDef,
    ) std.mem.Allocator.Error!void {
        try self.recordBindingTypeFact(inst, mono_cache, fn_def.arg);
        for (self.input.store.sliceTypedSymbolSpan(fn_def.captures)) |capture| {
            try self.recordBindingTypeFact(inst, mono_cache, capture);
        }
        try self.collectExprExecutableFacts(inst, mono_cache, fn_def.body);
    }

    fn ensureTopLevelValueLowered(self: *Lowerer, symbol: Symbol) std.mem.Allocator.Error!type_mod.TypeId {
        if (self.top_level_value_types.get(symbol)) |existing| return existing;

        const source = self.top_level_values.get(symbol) orelse
            debugPanic("lambdamono.lower.ensureTopLevelValueLowered missing top-level value");

        const expr_id: solved.Ast.ExprId = switch (source) {
            .val => |value_expr| value_expr,
            .run => |run_def| run_def.body,
        };

        const specialized = try self.specializeStandaloneValue(.{ .symbol = symbol, .ty = self.input.store.getExpr(expr_id).ty }, expr_id);
        const result_ty = self.output.getExpr(specialized.expr).ty;
        try self.top_level_value_types.put(symbol, result_ty);

        switch (source) {
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
        try self.recordBindingTypeFact(&inst, &mono_cache, bind);
        try self.collectExprExecutableFacts(&inst, &mono_cache, expr_id);
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

        const bridge_erased_ret = @intFromEnum(pending.sig.ret_ty) < self.types.types.items.len and
            self.types.types.items[@intFromEnum(pending.sig.ret_ty)] == .erased_fn;
        const t = try self.cloneInstType(&inst, pending.fn_ty);
        const requested_ty = try self.cloneInstType(&inst, pending.requested_ty);
        try self.unify(t, requested_ty);
        const specialized_fn = lower_type.extractFn(&self.input.types, t);

        const result: ast.FnDef = switch (lower_type.lambdaRepr(&self.input.types, t)) {
            .lset => switch (try lower_type.extractLsetFn(
                &self.input.types,
                &self.types,
                &mono_cache,
                t,
                pending.name,
                &self.input.symbols,
            )) {
                .toplevel => .{
                    .args = blk: {
                        try self.collectSpecializedFnExecutableFacts(&inst, &mono_cache, pending, t);
                        break :blk try self.output.addTypedSymbolSpan(&.{.{
                            .ty = pending.sig.arg_ty,
                            .symbol = pending.fn_def.arg.symbol,
                        }});
                    },
                    .body = blk: {
                        const body = try self.specializeExpr(
                            &inst,
                            &mono_cache,
                            &.{.{
                                .symbol = pending.fn_def.arg.symbol,
                                .ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty),
                                .exec_ty = pending.sig.arg_ty,
                            }},
                            pending.fn_def.body,
                        );
                        break :blk if (bridge_erased_ret)
                            try self.bridgeExprToExpectedType(
                                &inst,
                                &mono_cache,
                                specialized_fn.ret,
                                body,
                                pending.sig.ret_ty,
                            )
                        else
                            body;
                    },
                },
                .lset => |capture_info| blk: {
                    try self.collectSpecializedFnExecutableFacts(&inst, &mono_cache, pending, t);
                    const arg_ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty);
                    const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                    const capture_bindings = try self.captureBindingsFromTypedSymbols(&inst, pending.fn_def.captures);
                    defer self.allocator.free(capture_bindings);
                    self.sortCaptureBindings(capture_bindings);
                    const arg_exec_ty = pending.sig.arg_ty;
                    const body_env = try self.buildFnBodyEnv(arg_ty, arg_exec_ty, pending.fn_def.arg.symbol, capture_bindings);
                    defer self.allocator.free(body_env);

                    var body = try self.specializeExpr(&inst, &mono_cache, body_env, pending.fn_def.body);
                    if (bridge_erased_ret) {
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
                        try self.publishExecutableType(capture_info.ty),
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
                                .ty = try self.publishExecutableType(capture_info.ty),
                                .symbol = captures_symbol,
                            },
                        }),
                        .body = body,
                    };
                },
            },
            .erased => blk: {
                const captures_new = pending.captures_new orelse &.{};
                const captures_solved = try self.captureSolvedBindingsFromTypedSymbols(&inst, pending.fn_def.captures);
                defer self.allocator.free(captures_solved);

                if (captures_solved.len != captures_new.len) {
                    debugPanic("lambdamono.lower.specializeFn erased capture arity mismatch");
                }
                for (captures_solved, captures_new) |capture, capture_new| {
                    if (capture.symbol != capture_new.symbol) {
                        debugPanic("lambdamono.lower.specializeFn erased capture symbol mismatch");
                    }
                    try self.unify(capture.solved_ty, capture_new.solved_ty);
                }

                try self.collectSpecializedFnExecutableFacts(&inst, &mono_cache, pending, t);
                const arg_ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty);
                const arg_exec_ty = pending.sig.arg_ty;
                const captures = try self.allocator.dupe(lower_type.CaptureBinding, captures_new);
                defer self.allocator.free(captures);
                self.sortCaptureBindings(captures);
                const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                const captures_ty = if (captures.len == 0)
                    try self.makePrimitiveType(.erased)
                else
                    try self.publishExecutableType(try lower_type.lowerCaptureBindings(
                        &self.input.types,
                        &self.types,
                        &mono_cache,
                        captures,
                        &self.input.symbols,
                    ));
                const body_env = try self.buildFnBodyEnv(arg_ty, arg_exec_ty, pending.fn_def.arg.symbol, captures);
                defer self.allocator.free(body_env);

                var body = try self.specializeExpr(&inst, &mono_cache, body_env, pending.fn_def.body);
                if (bridge_erased_ret) {
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
            const captures_var = try self.output.addExpr(.{
                .ty = capture_record_ty,
                .data = .{ .var_ = captures_symbol },
            });
            const access = try self.output.addExpr(.{
                .ty = capture.lowered_ty,
                .data = .{ .access = .{
                    .record = captures_var,
                    .field = self.input.symbols.get(capture.symbol).name,
                    .field_index = @intCast(idx),
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
        const default_ty = self.requireExprTypeFact(inst, expr_id);

        if (expr.data == .inspect) {
            return try self.specializeInspectExpr(inst, mono_cache, venv, expr.data.inspect, default_ty);
        }

        const specialized: SpecializedExprLowering = switch (expr.data) {
            .var_ => |symbol| blk: {
                break :blk try self.specializeVarExpr(inst, mono_cache, venv, symbol, ty, default_ty);
            },
            .int_lit => |value| .{ .ty = default_ty, .data = .{ .int_lit = value } },
            .frac_f32_lit => |value| .{ .ty = default_ty, .data = .{ .frac_f32_lit = value } },
            .frac_f64_lit => |value| .{ .ty = default_ty, .data = .{ .frac_f64_lit = value } },
            .dec_lit => |value| .{ .ty = default_ty, .data = .{ .dec_lit = value } },
            .str_lit => |value| .{ .ty = default_ty, .data = .{ .str_lit = value } },
            .bool_lit => |value| .{ .ty = default_ty, .data = .{ .bool_lit = value } },
            .unit => .{ .ty = default_ty, .data = .unit },
            .tag => |tag| .{ .ty = default_ty, .data = .{ .tag = .{
                .name = .{ .ctor = tag.name },
                .discriminant = tag.discriminant,
                .args = try self.specializeExprSpan(inst, mono_cache, venv, tag.args),
            } } },
            .record => |fields| .{ .ty = default_ty, .data = .{ .record = try self.specializeFieldSpan(inst, mono_cache, venv, fields) } },
            .access => |access| blk: {
                const record = try self.specializeExpr(inst, mono_cache, venv, access.record);
                break :blk .{ .ty = default_ty, .data = .{ .access = .{
                    .record = record,
                    .field = access.field,
                    .field_index = access.field_index,
                } } };
            },
            .let_ => |let_expr| blk: {
                const body = try self.specializeExpr(inst, mono_cache, venv, let_expr.body);
                const bind_exec_ty = self.output.getExpr(body).ty;
                const bind_ty = try self.cloneInstType(inst, let_expr.bind.ty);
                const rest_env = try self.extendEnv(venv, .{
                    .symbol = let_expr.bind.symbol,
                    .ty = bind_ty,
                    .exec_ty = bind_exec_ty,
                    .callable_expr = self.maybeCallableEnvExpr(venv, body),
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
                        .body = body,
                        .rest = rest,
                    } },
                };
            },
            .call => |call| try self.specializeCallExpr(inst, mono_cache, venv, call, default_ty),
            .inspect => unreachable,
            .low_level => |ll| blk: {
                const lowered_args = switch (ll.op) {
                    .box_box => blk_args: {
                        const source_args = self.input.store.sliceExprSpan(ll.args);
                        const out = try self.allocator.alloc(ast.ExprId, source_args.len);
                        defer self.allocator.free(out);
                        for (source_args, 0..) |arg_id, i| {
                            out[i] = try self.specializeBoxBoundaryArgExpr(inst, mono_cache, venv, arg_id);
                        }
                        break :blk_args try self.output.addExprSpan(out);
                    },
                    else => try self.specializeExprSpan(inst, mono_cache, venv, ll.args),
                };
                const result_ty = switch (ll.op) {
                    .box_box => blk_ty: {
                        const args = self.output.sliceExprSpan(lowered_args);
                        if (args.len != 1) debugPanic("lambdamono.lower.specializeExpr box_box expected one arg");
                        const arg_exec_ty = self.output.getExpr(args[0]).ty;
                        break :blk_ty try self.publishExecutableType(try self.types.internResolved(.{ .box = arg_exec_ty }));
                    },
                    .box_unbox => blk_ty: {
                        const args = self.output.sliceExprSpan(lowered_args);
                        if (args.len != 1) debugPanic("lambdamono.lower.specializeExpr box_unbox expected one arg");
                        const arg_ty = self.output.getExpr(args[0]).ty;
                        break :blk_ty switch (self.types.getType(arg_ty)) {
                            .box => |elem| elem,
                            else => debugPanic("lambdamono.lower.specializeExpr box_unbox expected boxed executable arg"),
                        };
                    },
                    else => default_ty,
                };
                break :blk .{
                    .ty = result_ty,
                    .data = .{ .low_level = .{
                        .op = ll.op,
                        .args = lowered_args,
                    } },
                };
            },
            .when => |when_expr| .{ .ty = default_ty, .data = .{ .when = .{
                .cond = try self.specializeExpr(inst, mono_cache, venv, when_expr.cond),
                .branches = try self.specializeBranchSpan(inst, mono_cache, venv, when_expr.branches),
            } } },
            .if_ => |if_expr| .{ .ty = default_ty, .data = .{ .if_ = .{
                .cond = try self.specializeExpr(inst, mono_cache, venv, if_expr.cond),
                .then_body = try self.specializeExpr(inst, mono_cache, venv, if_expr.then_body),
                .else_body = try self.specializeExpr(inst, mono_cache, venv, if_expr.else_body),
            } } },
            .block => |block| .{ .ty = default_ty, .data = .{ .block = try self.specializeBlockExpr(inst, mono_cache, venv, block) } },
            .tuple => |elems| .{ .ty = default_ty, .data = .{ .tuple = try self.specializeExprSpan(inst, mono_cache, venv, elems) } },
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

    fn specializeInspectExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        value_expr_id: solved.Ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const value_expr = try self.specializeExpr(inst, mono_cache, venv, value_expr_id);
        return try self.specializeInspectValueExpr(value_expr, result_ty);
    }

    fn ensureInspectHelper(
        self: *Lowerer,
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
        const body = try self.buildInlineInspectValueExpr(arg_expr, result_ty);

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
        value_expr: ast.ExprId,
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const value_ty = self.output.getExpr(value_expr).ty;
        const helper = try self.ensureInspectHelper(value_ty, result_ty);
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
            .nominal => |backing| blk: {
                const backing_expr = try self.retypeExpr(value_expr, backing);
                break :blk try self.buildInlineInspectValueExpr(backing_expr, result_ty);
            },
            .list => |elem_ty| self.makeListInspectExpr(elem_ty, value_expr, result_ty),
            .box => |elem_ty| blk: {
                switch (self.types.getTypePreservingNominal(elem_ty)) {
                    .erased_fn => break :blk self.makeStringLiteralExpr(result_ty, "<fn>"),
                    .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                        break :blk self.makeStringLiteralExpr(result_ty, "<fn>");
                    },
                    else => {},
                }
                const unboxed_expr = try self.makeLowLevelExpr(elem_ty, .box_unbox, &.{value_expr});
                break :blk try self.specializeInspectValueExpr(unboxed_expr, result_ty);
            },
            .tuple => |elems| self.makeTupleInspectExpr(value_expr, elems, result_ty),
            .record => |record| self.makeRecordInspectExpr(value_expr, record.fields, result_ty),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                self.makeStringLiteralExpr(result_ty, "<fn>")
            else
                self.makeTagUnionInspectExpr(value_expr, tag_union.tags, result_ty),
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
        const inspected_item = try self.specializeInspectValueExpr(item_expr, result_ty);

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
        record_expr: ast.ExprId,
        fields_span: type_mod.Span(type_mod.Field),
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const fields = self.types.sliceFields(fields_span);
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
                try self.specializeInspectValueExpr(field_expr, result_ty),
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
        tuple_expr: ast.ExprId,
        elems_span: type_mod.Span(type_mod.TypeId),
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const elems = self.types.sliceTypeSpan(elems_span);
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
                try self.specializeInspectValueExpr(elem_expr, result_ty),
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
        value_expr: ast.ExprId,
        tags_span: type_mod.Span(type_mod.Tag),
        result_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const union_ty = self.output.getExpr(value_expr).ty;
        const tags = self.types.sliceTags(tags_span);
        const branches = try self.allocator.alloc(ast.Branch, tags.len);
        defer self.allocator.free(branches);

        for (tags, 0..) |tag, i| {
            const tag_args = self.types.sliceTypeSpan(tag.args);
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
                    try self.specializeInspectValueExpr(arg_expr, result_ty),
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
        self: *Lowerer,
        tags_span: type_mod.Span(type_mod.Tag),
    ) bool {
        const tags = self.types.sliceTags(tags_span);
        if (tags.len == 0) return false;

        for (tags) |tag| switch (tag.name) {
            .lambda => {},
            .ctor => return false,
        };

        return true;
    }

    fn specializeInspectValueExpr(
        self: *Lowerer,
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
            .nominal => self.makeInspectHelperCall(value_expr, result_ty),
            .list,
            .box,
            .tuple,
            .record,
            => self.makeInspectHelperCall(value_expr, result_ty),
            .tag_union => |tag_union| if (self.tagUnionIsInternalLambdaSet(tag_union.tags))
                self.makeStringLiteralExpr(result_ty, "<fn>")
            else
                self.makeInspectHelperCall(value_expr, result_ty),
        };
    }

    fn makePrimitiveType(self: *Lowerer, prim: type_mod.Prim) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.internResolved(.{ .primitive = prim });
    }

    fn makeUnitType(self: *Lowerer) std.mem.Allocator.Error!type_mod.TypeId {
        return try self.types.internResolved(.{ .record = .{ .fields = type_mod.Span(type_mod.Field).empty() } });
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
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const lowered_expr = try self.specializeExpr(inst, mono_cache, venv, expr_id);
        const source_ty = try self.cloneInstType(inst, self.input.store.getExpr(expr_id).ty);
        return try self.lowerBoxBoundaryExpr(inst, mono_cache, venv, source_ty, lowered_expr);
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

        switch (self.types.getType(lowered.ty)) {
            .erased_fn => return lowered_expr,
            .tag_union => |tag_union| {
                if (self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    return try self.lowerConcreteCallableAsErased(inst, mono_cache, source_ty, lowered_expr);
                }
            },
            else => {},
        }

        return lowered_expr;
    }

    fn lowerConcreteCallableAsErased(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        source_ty: TypeVarId,
        callable_expr: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const callable = self.output.getExpr(callable_expr);
        const lambda_members = try self.freezeLsetLambdaMembers(mono_cache, source_ty);
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
                break :blk try self.makeErasedPackedFnExpr(inst, mono_cache, source_ty, lambda_members[tag.discriminant].symbol, capture_expr, null);
            },
            .packed_fn => callable_expr,
            else => blk: {
                const erased_capture_ty = self.commonErasedCaptureTypeFromFrozen(lambda_members);
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

        if (@intFromEnum(expected_ty) < self.types.types.items.len and self.types.types.items[@intFromEnum(expected_ty)] == .erased_fn) {
            return try self.lowerConcreteCallableAsErased(inst, mono_cache, source_ty, expr_id);
        }

        debugPanic("lambdamono.lower.bridgeExprToExpectedType missing explicit executable bridge");
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
        const fn_entry = specializations.lookupFn(self.fenv, symbol) orelse
            debugPanic("lambdamono.lower.makeErasedPackedFnExpr missing function");
        try self.collectFnExecutableFacts(inst, mono_cache, fn_entry.fn_def);
        const captures = try self.captureBindingsFromTypedSymbols(inst, fn_entry.fn_def.captures);
        defer self.allocator.free(captures);
        self.sortCaptureBindings(captures);
        const sig = try self.buildErasedSpecializationSig(
            mono_cache,
            symbol,
            requested_ty,
            captures,
        );
        const specialized_symbol = try specializations.specializeFnErased(
            &self.queue,
            self.fenv,
            &self.input.symbols,
            symbol,
            requested_ty,
            sig,
            captures,
        );
        const capture_ty = forced_capture_ty orelse if (capture_expr) |capture| self.output.getExpr(capture).ty else null;
        return try self.output.addExpr(.{
            .ty = try self.makeErasedFnType(capture_ty),
            .data = .{ .packed_fn = .{
                .lambda = specialized_symbol,
                .captures = capture_expr,
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
            const next: ?type_mod.TypeId = switch (try lower_type.extractLsetFn(
                &self.input.types,
                &self.types,
                mono_cache,
                requested_ty,
                lambda.symbol,
                &self.input.symbols,
            )) {
                .toplevel => null,
                .lset => |capture_info| try self.publishExecutableType(capture_info.ty),
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
        self: *Lowerer,
        tags_span: type_mod.Span(type_mod.Tag),
    ) std.mem.Allocator.Error!?type_mod.TypeId {
        const tags = self.types.sliceTags(tags_span);
        var common: ?type_mod.TypeId = null;
        for (tags) |tag| {
            const args = self.types.sliceTypeSpan(tag.args);
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
        lambda_member: FrozenLambdaMemberFact,
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
        default_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        if (specializations.lookupFn(self.fenv, symbol)) |fn_entry| {
            return switch (lower_type.lambdaRepr(&self.input.types, instantiated_ty)) {
                .lset => |lambdas| blk: {
                    const lambda_discriminant = self.requireLambdaDiscriminant(lambdas, symbol);

                    switch (try lower_type.extractLsetFn(
                        &self.input.types,
                        &self.types,
                        mono_cache,
                        instantiated_ty,
                        symbol,
                        &self.input.symbols,
                    )) {
                        .toplevel => break :blk .{
                            .ty = default_ty,
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(symbol),
                                .discriminant = lambda_discriminant,
                                .args = ast.Span(ast.ExprId).empty(),
                            } },
                        },
                        .lset => |capture_info| {
                            try self.collectFnExecutableFacts(inst, mono_cache, fn_entry.fn_def);
                            const capture_bindings = try self.captureBindingsFromTypedSymbols(inst, fn_entry.fn_def.captures);
                            defer self.allocator.free(capture_bindings);
                            self.sortCaptureBindings(capture_bindings);
                            const capture_record = try self.specializeCaptureRecord(capture_bindings, capture_info.ty);
                            const args = try self.allocator.alloc(ast.ExprId, 1);
                            defer self.allocator.free(args);
                            args[0] = capture_record;
                            break :blk .{
                                .ty = default_ty,
                                .data = .{ .tag = .{
                                    .name = lower_type.lambdaTagKey(symbol),
                                    .discriminant = lambda_discriminant,
                                    .args = try self.output.addExprSpan(args),
                                } },
                            };
                        },
                    }
                },
                .erased => blk: {
                    try self.collectFnExecutableFacts(inst, mono_cache, fn_entry.fn_def);
                    const captures = try self.captureBindingsForErasedVar(inst, venv, fn_entry.fn_def.captures);
                    defer self.allocator.free(captures);
                    const sig = try self.buildErasedSpecializationSig(
                        mono_cache,
                        symbol,
                        instantiated_ty,
                        captures,
                    );
                    const specialized_symbol = try specializations.specializeFnErased(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        symbol,
                        instantiated_ty,
                        sig,
                        captures,
                    );

                    const capture_ty: ?type_mod.TypeId = if (captures.len == 0)
                        null
                    else
                        try self.publishExecutableType(try lower_type.lowerCaptureBindings(
                            &self.input.types,
                            &self.types,
                            mono_cache,
                            captures,
                            &self.input.symbols,
                        ));
                    const capture_expr = if (capture_ty) |actual_capture_ty|
                        try self.specializeCaptureRecord(captures, actual_capture_ty)
                    else
                        null;
                    break :blk .{
                        .ty = try self.makeErasedFnType(capture_ty),
                        .data = .{ .packed_fn = .{
                            .lambda = specialized_symbol,
                            .captures = capture_expr,
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
        debugPanic("lambdamono.lower.specializeVarExpr unbound variable");
    }

    fn specializeCaptureRecord(
        self: *Lowerer,
        captures: []const lower_type.CaptureBinding,
        capture_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const fields = try self.allocator.alloc(ast.FieldExpr, captures.len);
        defer self.allocator.free(fields);

        for (captures, 0..) |capture, i| {
            const capture_expr = try self.output.addExpr(.{
                .ty = capture.lowered_ty,
                .data = .{ .var_ = capture.symbol },
            });
            fields[i] = .{
                .name = self.input.symbols.get(capture.symbol).name,
                .value = capture_expr,
            };
        }

        return try self.output.addExpr(.{
            .ty = capture_ty,
            .data = .{ .record = try self.output.addFieldExprSpan(fields) },
        });
    }

    const CaptureSolvedBinding = struct {
        symbol: Symbol,
        solved_ty: TypeVarId,
    };

    const SpecializedExprLowering = struct {
        ty: type_mod.TypeId,
        data: ast.Expr.Data,
    };

    const FrozenLambdaMemberFact = struct {
        symbol: Symbol,
        discriminant: u16,
        capture_ty: ?type_mod.TypeId,
    };

    fn freezeLsetLambdaMembers(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        requested_ty: TypeVarId,
    ) std.mem.Allocator.Error![]FrozenLambdaMemberFact {
        const lambdas = switch (lower_type.lambdaRepr(&self.input.types, requested_ty)) {
            .erased => debugPanic("lambdamono.lower.freezeLsetLambdaMembers expected concrete lambda-set"),
            .lset => |lset| lset,
        };
        const out = try self.allocator.alloc(FrozenLambdaMemberFact, lambdas.len);
        for (lambdas, 0..) |lambda, i| {
            const capture_ty: ?type_mod.TypeId = switch (try lower_type.extractLsetFn(
                &self.input.types,
                &self.types,
                mono_cache,
                requested_ty,
                lambda.symbol,
                &self.input.symbols,
            )) {
                .toplevel => null,
                .lset => |capture_info| try self.publishExecutableType(capture_info.ty),
            };
            out[i] = .{
                .symbol = lambda.symbol,
                .discriminant = @intCast(i),
                .capture_ty = capture_ty,
            };
        }
        return out;
    }

    fn commonErasedCaptureTypeFromFrozen(
        self: *const Lowerer,
        members: []const FrozenLambdaMemberFact,
    ) ?type_mod.TypeId {
        _ = self;
        var common: ?type_mod.TypeId = null;
        for (members) |member| {
            const next = member.capture_ty;
            if (common == null) {
                common = next;
                continue;
            }
            if (common == null and next == null) continue;
            if (common == null or next == null or common.? != next.?) {
                debugPanic("lambdamono.lower.commonErasedCaptureTypeFromFrozen boxed callable variants require a common capture type");
            }
        }
        return common;
    }

    fn captureSolvedBindingsFromTypedSymbols(
        self: *Lowerer,
        inst: *InstScope,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
    ) std.mem.Allocator.Error![]CaptureSolvedBinding {
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const out = try self.allocator.alloc(CaptureSolvedBinding, captures.len);
        for (captures, 0..) |capture, i| {
            out[i] = .{
                .symbol = capture.symbol,
                .solved_ty = try self.cloneInstType(inst, capture.ty),
            };
        }
        self.sortCaptureSolvedBindings(out);
        return out;
    }

    fn captureBindingsFromTypedSymbols(
        self: *Lowerer,
        inst: *InstScope,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            const solved_ty = try self.cloneInstType(inst, capture.ty);
            out[i] = .{
                .symbol = capture.symbol,
                .solved_ty = solved_ty,
                .lowered_ty = self.requireSolvedTypeFact(inst, solved_ty),
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
        self.sortCaptureBindings(out);
        return out;
    }

    fn sortCaptureSolvedBindings(self: *Lowerer, captures: []CaptureSolvedBinding) void {
        _ = self;
        std.mem.sortUnstable(CaptureSolvedBinding, captures, {}, struct {
            fn lessThan(_: void, left: CaptureSolvedBinding, right: CaptureSolvedBinding) bool {
                return left.symbol.raw() < right.symbol.raw();
            }
        }.lessThan);
    }

    fn sortCaptureBindings(self: *Lowerer, captures: []lower_type.CaptureBinding) void {
        _ = self;
        std.mem.sortUnstable(lower_type.CaptureBinding, captures, {}, struct {
            fn lessThan(_: void, left: lower_type.CaptureBinding, right: lower_type.CaptureBinding) bool {
                return left.symbol.raw() < right.symbol.raw();
            }
        }.lessThan);
    }

    fn specializeCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        call: @FieldType(solved.Ast.Expr.Data, "call"),
        lowered_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!SpecializedExprLowering {
        const func_source = self.input.store.getExpr(call.func);
        const func_ty = try self.cloneInstType(inst, func_source.ty);
        const lowered_func = try self.specializeExpr(inst, mono_cache, venv, call.func);
        const lowered_arg = if (func_source.data == .var_)
            if (self.boxBoundaryBuiltinOp(func_source.data.var_)) |_|
                try self.specializeBoxBoundaryArgExpr(inst, mono_cache, venv, call.arg)
            else
                try self.specializeExpr(inst, mono_cache, venv, call.arg)
        else
            try self.specializeExpr(inst, mono_cache, venv, call.arg);
        const lowered_func_ty = self.output.getExpr(lowered_func).ty;

        return switch (self.types.getType(lowered_func_ty)) {
            .erased_fn => .{
                .ty = lowered_ty,
                .data = .{ .call_indirect = .{
                    .func = lowered_func,
                    .args = try self.output.addExprSpan(&.{lowered_arg}),
                } },
            },
            .tag_union => |tag_union| blk: {
                if (!self.tagUnionIsInternalLambdaSet(tag_union.tags)) {
                    debugPanic("lambdamono.lower.specializeCallExpr expected callable executable type");
                }

                const lambda_members = try self.freezeLsetLambdaMembers(mono_cache, func_ty);
                defer self.allocator.free(lambda_members);
                const branches = try self.allocator.alloc(ast.Branch, lambda_members.len);
                defer self.allocator.free(branches);
                var result_ty: ?type_mod.TypeId = null;

                for (lambda_members, 0..) |lambda_member, i| {
                    const sig = try self.buildLsetSpecializationSig(
                        mono_cache,
                        lambda_member.capture_ty,
                        lambda_member.symbol,
                        func_ty,
                        self.output.getExpr(lowered_arg).ty,
                        null,
                    );
                    if (result_ty) |existing| {
                        if (existing != sig.ret_ty) {
                            debugPanic("lambdamono.lower.specializeCallExpr inconsistent branch return executable types");
                        }
                    } else {
                        result_ty = sig.ret_ty;
                    }
                    const specialized = try specializations.specializeFnLset(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        lambda_member.symbol,
                        func_ty,
                        sig,
                    );

                    branches[i] = if (lambda_member.capture_ty == null) .{
                        .pat = try self.output.addPat(.{
                            .ty = self.output.getExpr(lowered_func).ty,
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(lambda_member.symbol),
                                .discriminant = lambda_member.discriminant,
                                .args = ast.Span(ast.PatId).empty(),
                            } },
                        }),
                        .body = try self.output.addExpr(.{
                            .ty = sig.ret_ty,
                            .data = .{ .call = .{
                                .proc = specialized,
                                .args = try self.output.addExprSpan(&.{lowered_arg}),
                            } },
                        }),
                    } else blk2: {
                        const capture_ty = lambda_member.capture_ty.?;
                        const capture_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                        const capture_pat = try self.output.addPat(.{
                            .ty = capture_ty,
                            .data = .{ .var_ = capture_symbol },
                        });
                        const pat = try self.output.addPat(.{
                            .ty = self.output.getExpr(lowered_func).ty,
                            .data = .{ .tag = .{
                                .name = lower_type.lambdaTagKey(lambda_member.symbol),
                                .discriminant = lambda_member.discriminant,
                                .args = try self.output.addPatSpan(&.{capture_pat}),
                            } },
                        });
                        break :blk2 .{
                            .pat = pat,
                            .body = try self.output.addExpr(.{
                                .ty = sig.ret_ty,
                                .data = .{ .call = .{
                                    .proc = specialized,
                                    .args = try self.output.addExprSpan(&.{
                                        lowered_arg,
                                        try self.output.addExpr(.{
                                            .ty = capture_ty,
                                            .data = .{ .var_ = capture_symbol },
                                        }),
                                    }),
                                } },
                            }),
                        };
                    };
                }

                break :blk .{
                    .ty = result_ty orelse lowered_ty,
                    .data = .{ .when = .{
                        .cond = lowered_func,
                        .branches = try self.output.addBranchSpan(branches),
                    } },
                };
            },
            else => debugPanic("lambdamono.lower.specializeCallExpr expected callable executable type"),
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
        const lowered_ty = self.requirePatTypeFact(inst, pat_id);

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
                const bind_ty = try self.cloneInstType(inst, decl.bind.ty);
                const bind_exec_ty = self.output.getExpr(body).ty;
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .decl = .{
                            .bind = .{
                                .ty = bind_exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = body,
                        },
                    }),
                    .env = try self.extendEnv(venv, .{
                        .symbol = decl.bind.symbol,
                        .ty = bind_ty,
                        .exec_ty = bind_exec_ty,
                        .callable_expr = self.maybeCallableEnvExpr(venv, body),
                    }),
                };
            },
            .var_decl => |decl| blk: {
                const body = try self.specializeExpr(inst, mono_cache, venv, decl.body);
                const bind_ty = try self.cloneInstType(inst, decl.bind.ty);
                const bind_exec_ty = self.output.getExpr(body).ty;
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .var_decl = .{
                            .bind = .{
                                .ty = bind_exec_ty,
                                .symbol = decl.bind.symbol,
                            },
                            .body = body,
                        },
                    }),
                    .env = try self.extendEnv(venv, .{
                        .symbol = decl.bind.symbol,
                        .ty = bind_ty,
                        .exec_ty = bind_exec_ty,
                        .callable_expr = self.maybeCallableEnvExpr(venv, body),
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
            .nominal => |backing| blk: {
                const placeholder = try self.input.types.freshUnbd();
                try inst.mapping.put(id, placeholder);
                const node = solved.Type.Node{ .nominal = try self.cloneInstType(inst, backing) };
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
            .content => |left_content| switch (right_node) {
                .content => |right_content| try self.unifyContent(left_content, right_content, visited),
                else => debugPanic("lambdamono.lower.unify incompatible types"),
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
                    if (prim != other) debugPanic("lambdamono.lower.unify incompatible primitives");
                    return .{ .primitive = prim };
                },
                .lambda_set => if (prim == .erased)
                    return .{ .primitive = .erased }
                else
                    debugPanic("lambdamono.lower.unify incompatible types"),
                else => debugPanic("lambdamono.lower.unify incompatible types"),
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
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .list => |elem| switch (right) {
                .list => |other| {
                    try self.unifyRec(elem, other, visited);
                    return .{ .list = elem };
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .box => |elem| switch (right) {
                .box => |other| {
                    try self.unifyRec(elem, other, visited);
                    return .{ .box = elem };
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
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
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .record => |record| switch (right) {
                .record => |other| {
                    return .{ .record = .{
                        .fields = try self.unifyRecordFields(record.fields, other.fields, visited),
                    } };
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .tag_union => |tag_union| switch (right) {
                .tag_union => |other| {
                    return .{ .tag_union = .{
                        .tags = try self.unifyTags(tag_union.tags, other.tags, visited),
                    } };
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .lambda_set => |lambda_set| switch (right) {
                .lambda_set => |other| {
                    return .{ .lambda_set = try self.unifyLambdaSet(lambda_set, other, visited) };
                },
                .primitive => |prim| if (prim == .erased)
                    return .{ .primitive = .erased }
                else
                    debugPanic("lambdamono.lower.unify incompatible types"),
                else => debugPanic("lambdamono.lower.unify incompatible types"),
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

    fn lookupEnvEntry(self: *const Lowerer, venv: []const EnvEntry, symbol: Symbol) ?EnvEntry {
        _ = self;
        for (venv) |entry| {
            if (entry.symbol == symbol) return entry;
        }
        return null;
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

fn debugTodoLowLevel(op: base.LowLevel) noreturn {
    @branchHint(.cold);
    std.debug.panic("TODO lambdamono low-level op {s}", .{@tagName(op)});
}

test "lambdamono lower tests" {
    std.testing.refAllDecls(@This());
}
