//! Lower monotype_lifted into lambdasolved using cor's inst -> infer ->
//! propagate-erasure -> SCC ordering flow.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const lifted = @import("monotype_lifted");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");

const LiftedResult = lifted.Lower.Result;
const LiftedAst = lifted.Ast;
const LiftedType = lifted.Type;
const Symbol = symbol_mod.Symbol;
const TypeVarId = type_mod.TypeVarId;

/// Public struct `Result`.
pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    strings: base.StringLiteral.Store,
    idents: base.Ident.Store,
    attached_method_index: symbol_mod.AttachedMethodIndex,
    builtin_module_idx: u32,
    builtin_list_ident: base.Ident.Idx,
    builtin_box_ident: base.Ident.Idx,
    builtin_primitive_owner_idents: symbol_mod.PrimitiveMethodOwnerIdents,
    runtime_inspect_symbols: std.AutoHashMap(Symbol, Symbol),
    exact_callable_aliases: std.AutoHashMap(Symbol, Symbol),

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
        self.idents.deinit(self.store.allocator);
        self.attached_method_index.deinit();
        self.runtime_inspect_symbols.deinit();
        self.exact_callable_aliases.deinit();
    }

    pub fn take(self: *Result, allocator: std.mem.Allocator) std.mem.Allocator.Error!Result {
        const result = self.*;
        self.* = .{
            .store = ast.Store.init(allocator),
            .root_defs = .empty,
            .symbols = symbol_mod.Store.init(allocator),
            .types = type_mod.Store.init(allocator),
            .strings = .{},
            .idents = try base.Ident.Store.initCapacity(allocator, 1),
            .attached_method_index = symbol_mod.AttachedMethodIndex.init(allocator),
            .builtin_module_idx = 0,
            .builtin_list_ident = base.Ident.Idx.NONE,
            .builtin_box_ident = base.Ident.Idx.NONE,
            .builtin_primitive_owner_idents = symbol_mod.PrimitiveMethodOwnerIdents.none(),
            .runtime_inspect_symbols = std.AutoHashMap(Symbol, Symbol).init(allocator),
            .exact_callable_aliases = std.AutoHashMap(Symbol, Symbol).init(allocator),
        };
        return result;
    }
};

/// Run this compilation stage.
pub fn run(allocator: std.mem.Allocator, input: *LiftedResult) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, try input.take(allocator));
    defer lowerer.deinit();
    try lowerer.instantiateProgram();
    try lowerer.inferProgram();
    try lowerer.propagateErasure();
    try lowerer.reorderDefsByScc();
    return try lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: LiftedResult,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    types: type_mod.Store,
    def_id_by_symbol: std.AutoHashMap(Symbol, ast.DefId),
    lifted_fn_capture_spans: std.AutoHashMap(Symbol, LiftedAst.Span(LiftedAst.TypedSymbol)),
    exact_callable_aliases: std.AutoHashMap(Symbol, Symbol),
    fn_infer_states: std.AutoHashMap(Symbol, FnInferState),
    current_instantiate_cache: ?*std.AutoHashMap(LiftedType.TypeId, TypeVarId),
    current_expr_instantiate_cache: ?*std.AutoHashMap(LiftedAst.ExprId, ast.ExprId),
    current_return_ty: ?TypeVarId,
    current_def_symbol: ?Symbol,
    current_opaque_unify: ?DebugNominalContext,
    current_expr_id: ?ast.ExprId,
    current_call_context: ?DebugCallContext,
    runtime_error_msg: ?base.StringLiteral.Idx,

    const EnvEntry = struct {
        symbol: Symbol,
        ty: TypeVarId,
    };

    const TypePair = struct {
        left: TypeVarId,
        right: TypeVarId,
    };

    const DebugNominalContext = struct {
        left: []const u8,
        right: []const u8,
    };

    const DebugCallContext = struct {
        func_expr: []const u8,
        arg_expr: []const u8,
        func_ty: []const u8,
        arg_ty: []const u8,
        target_ty: []const u8,
    };

    const SccGroup = struct {
        def_ids: []ast.DefId,
    };

    const FnInferState = enum {
        pending,
        inferring,
        done,
    };

    fn init(allocator: std.mem.Allocator, input: LiftedResult) Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .types = type_mod.Store.init(allocator),
            .def_id_by_symbol = std.AutoHashMap(Symbol, ast.DefId).init(allocator),
            .lifted_fn_capture_spans = std.AutoHashMap(Symbol, LiftedAst.Span(LiftedAst.TypedSymbol)).init(allocator),
            .exact_callable_aliases = std.AutoHashMap(Symbol, Symbol).init(allocator),
            .fn_infer_states = std.AutoHashMap(Symbol, FnInferState).init(allocator),
            .current_instantiate_cache = null,
            .current_expr_instantiate_cache = null,
            .current_return_ty = null,
            .current_def_symbol = null,
            .current_opaque_unify = null,
            .current_expr_id = null,
            .current_call_context = null,
            .runtime_error_msg = null,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.def_id_by_symbol.deinit();
        self.lifted_fn_capture_spans.deinit();
        self.exact_callable_aliases.deinit();
        self.fn_infer_states.deinit();
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
            .strings = self.input.strings,
            .idents = self.input.idents,
            .attached_method_index = self.input.attached_method_index,
            .builtin_module_idx = self.input.builtin_module_idx,
            .builtin_list_ident = self.input.builtin_list_ident,
            .builtin_box_ident = self.input.builtin_box_ident,
            .builtin_primitive_owner_idents = self.input.builtin_primitive_owner_idents,
            .runtime_inspect_symbols = self.input.runtime_inspect_symbols,
            .exact_callable_aliases = self.exact_callable_aliases,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.types = type_mod.Store.init(self.allocator);
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.strings = .{};
        self.input.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.input.attached_method_index = symbol_mod.AttachedMethodIndex.init(self.allocator);
        self.input.builtin_module_idx = 0;
        self.input.builtin_list_ident = base.Ident.Idx.NONE;
        self.input.builtin_box_ident = base.Ident.Idx.NONE;
        self.input.builtin_primitive_owner_idents = symbol_mod.PrimitiveMethodOwnerIdents.none();
        self.input.runtime_inspect_symbols = std.AutoHashMap(Symbol, Symbol).init(self.allocator);
        self.exact_callable_aliases = std.AutoHashMap(Symbol, Symbol).init(self.allocator);
        return result;
    }

    fn instantiateProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            const def_id = try self.instantiateDef(def);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(self.root_defs.items[self.root_defs.items.len - 1] == def_id);
            } else if (self.root_defs.items[self.root_defs.items.len - 1] != def_id) {
                unreachable;
            }
        }
    }

    fn emitDef(self: *Lowerer, def: ast.Def) std.mem.Allocator.Error!ast.DefId {
        const def_id = try self.output.addDef(def);
        try self.root_defs.append(self.allocator, def_id);
        try self.def_id_by_symbol.put(def.bind.symbol, def_id);
        try self.fn_infer_states.put(def.bind.symbol, .pending);
        return def_id;
    }

    fn instantiateDef(self: *Lowerer, def: LiftedAst.Def) std.mem.Allocator.Error!ast.DefId {
        return switch (def.value) {
            .fn_ => |lifted_fn_def| blk: {
                var cache = std.AutoHashMap(LiftedType.TypeId, TypeVarId).init(self.allocator);
                defer cache.deinit();
                var expr_cache = std.AutoHashMap(LiftedAst.ExprId, ast.ExprId).init(self.allocator);
                defer expr_cache.deinit();
                const previous_cache = self.current_instantiate_cache;
                const previous_expr_cache = self.current_expr_instantiate_cache;
                self.current_instantiate_cache = &cache;
                self.current_expr_instantiate_cache = &expr_cache;
                defer self.current_instantiate_cache = previous_cache;
                defer self.current_expr_instantiate_cache = previous_expr_cache;

                const bind = try self.instantiateTypedSymbol(def.bind);
                try self.lifted_fn_capture_spans.put(bind.symbol, lifted_fn_def.captures);
                break :blk self.emitDef(.{
                    .bind = bind,
                    .value = .{ .fn_ = .{
                        .args = try self.instantiateTypedSymbolSpan(lifted_fn_def.args),
                        .body = try self.instantiateExpr(lifted_fn_def.body),
                    } },
                });
            },
            .hosted_fn => |hosted_fn| self.emitDef(.{
                .bind = try self.instantiateTypedSymbol(def.bind),
                .value = .{ .hosted_fn = .{
                    .bind = try self.instantiateTypedSymbol(hosted_fn.bind),
                    .args = try self.instantiateTypedSymbolSpan(hosted_fn.args),
                    .hosted = hosted_fn.hosted,
                } },
            }),
            .val => |expr_id| blk: {
                var expr_cache = std.AutoHashMap(LiftedAst.ExprId, ast.ExprId).init(self.allocator);
                defer expr_cache.deinit();
                const previous_expr_cache = self.current_expr_instantiate_cache;
                self.current_expr_instantiate_cache = &expr_cache;
                defer self.current_expr_instantiate_cache = previous_expr_cache;

                const bind = try self.instantiateTypedSymbol(def.bind);
                const body = try self.instantiateExpr(expr_id);
                try self.recordExactCallableAlias(bind.symbol, body);
                break :blk self.emitDef(.{
                    .bind = bind,
                    .value = .{ .val = body },
                });
            },
            .run => |run_def| blk: {
                var expr_cache = std.AutoHashMap(LiftedAst.ExprId, ast.ExprId).init(self.allocator);
                defer expr_cache.deinit();
                const previous_expr_cache = self.current_expr_instantiate_cache;
                self.current_expr_instantiate_cache = &expr_cache;
                defer self.current_expr_instantiate_cache = previous_expr_cache;

                break :blk self.emitDef(.{
                    .bind = try self.instantiateTypedSymbol(def.bind),
                    .value = .{ .run = .{
                        .body = try self.instantiateExpr(run_def.body),
                        .entry_ty = run_def.entry_ty,
                    } },
                });
            },
        };
    }

    fn debugWriteMonotypeType(self: *Lowerer, out: *std.ArrayList(u8), ty: LiftedType.TypeId) std.mem.Allocator.Error!void {
        switch (self.input.types.getTypePreservingNominal(ty)) {
            .placeholder => try out.appendSlice(self.allocator, "<placeholder>"),
            .unbd => try out.appendSlice(self.allocator, "?"),
            .link => unreachable,
            .primitive => |prim| try out.appendSlice(self.allocator, @tagName(prim)),
            .func => |func| {
                try out.appendSlice(self.allocator, "Fn(");
                for (func.args, 0..) |arg, i| {
                    if (i != 0) try out.appendSlice(self.allocator, ", ");
                    try self.debugWriteMonotypeType(out, arg);
                }
                if (func.lambdas.len != 0) {
                    try out.appendSlice(self.allocator, " {");
                    for (func.lambdas, 0..) |lambda, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try out.writer(self.allocator).print("{d}", .{@intFromEnum(lambda)});
                    }
                    try out.appendSlice(self.allocator, "}");
                }
                try out.appendSlice(self.allocator, " -> ");
                try self.debugWriteMonotypeType(out, func.ret);
                try out.appendSlice(self.allocator, ")");
            },
            .nominal => |nominal| {
                try out.writer(self.allocator).print("Nom({d}:{d} ", .{ nominal.module_idx, @as(u32, @bitCast(nominal.ident)) });
                try self.debugWriteMonotypeType(out, nominal.backing);
                try out.append(self.allocator, ')');
            },
            .list => |elem| {
                try out.appendSlice(self.allocator, "List(");
                try self.debugWriteMonotypeType(out, elem);
                try out.append(self.allocator, ')');
            },
            .box => |elem| {
                try out.appendSlice(self.allocator, "Box(");
                try self.debugWriteMonotypeType(out, elem);
                try out.append(self.allocator, ')');
            },
            .tuple => |elems| {
                try out.appendSlice(self.allocator, "Tuple(");
                for (elems, 0..) |elem, i| {
                    if (i != 0) try out.appendSlice(self.allocator, ", ");
                    try self.debugWriteMonotypeType(out, elem);
                }
                try out.append(self.allocator, ')');
            },
            .tag_union => |tag_union| {
                try out.writer(self.allocator).print("TagUnion[{d}]", .{tag_union.tags.len});
            },
            .record => |record| {
                try out.writer(self.allocator).print("Record[{d}]", .{record.fields.len});
            },
        }
    }

    fn instantiateTypedSymbol(self: *Lowerer, value: LiftedAst.TypedSymbol) std.mem.Allocator.Error!ast.TypedSymbol {
        return .{
            .ty = try self.instantiateType(value.ty),
            .symbol = value.symbol,
        };
    }

    fn instantiateTypedSymbolSpan(self: *Lowerer, span: LiftedAst.Span(LiftedAst.TypedSymbol)) std.mem.Allocator.Error!ast.Span(ast.TypedSymbol) {
        const values = self.input.store.sliceTypedSymbolSpan(span);
        const out = try self.allocator.alloc(ast.TypedSymbol, values.len);
        defer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = try self.instantiateTypedSymbol(value);
        }
        return try self.output.addTypedSymbolSpan(out);
    }

    fn instantiateLiftedTypedSymbolSlice(self: *Lowerer, span: LiftedAst.Span(LiftedAst.TypedSymbol)) std.mem.Allocator.Error![]ast.TypedSymbol {
        const values = self.input.store.sliceTypedSymbolSpan(span);
        const out = try self.allocator.alloc(ast.TypedSymbol, values.len);
        errdefer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = try self.instantiateTypedSymbol(value);
        }
        return out;
    }

    fn maybeExactCallableSymbolForExpr(self: *Lowerer, expr_id: ast.ExprId) ?Symbol {
        const expr = self.output.getExpr(expr_id);
        return switch (expr.data) {
            .var_ => |symbol| blk: {
                if (symbol.isNone()) break :blk null;
                if (self.exact_callable_aliases.get(symbol)) |exact| break :blk exact;
                if (self.def_id_by_symbol.get(symbol)) |def_id| {
                    const def = self.output.getDef(def_id);
                    break :blk switch (def.value) {
                        .fn_, .hosted_fn => symbol,
                        else => null,
                    };
                }
                break :blk null;
            },
            .call => null,
            .low_level => null,
            else => null,
        };
    }

    fn recordExactCallableAlias(self: *Lowerer, bind_symbol: Symbol, body_expr: ast.ExprId) std.mem.Allocator.Error!void {
        const exact_symbol = self.maybeExactCallableSymbolForExpr(body_expr) orelse return;
        try self.exact_callable_aliases.put(bind_symbol, exact_symbol);
    }

    fn instantiateExpr(self: *Lowerer, expr_id: LiftedAst.ExprId) std.mem.Allocator.Error!ast.ExprId {
        if (self.current_expr_instantiate_cache) |cache| {
            if (cache.get(expr_id)) |existing| return existing;
        }

        const expr = self.input.store.getExpr(expr_id);
        const ty = switch (expr.data) {
            .var_ => try self.instantiateOccurrenceType(try self.instantiateType(expr.ty)),
            else => try self.instantiateType(expr.ty),
        };
        const data: ast.Expr.Data = switch (expr.data) {
            .var_ => |symbol| .{ .var_ = symbol },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .bool_lit => |value| .{ .bool_lit = value },
            .unit => .unit,
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .discriminant = tag.discriminant,
                .args = try self.instantiateExprSpan(tag.args),
            } },
            .record => |fields| .{ .record = try self.instantiateFieldSpan(fields) },
            .access => |access| .{ .access = .{
                .record = try self.instantiateExpr(access.record),
                .field = access.field,
                .field_index = access.field_index,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.instantiateExpr(eq.lhs),
                .rhs = try self.instantiateExpr(eq.rhs),
            } },
            .method_eq => |eq| .{ .method_eq = .{
                .lhs = try self.instantiateExpr(eq.lhs),
                .rhs = try self.instantiateExpr(eq.rhs),
                .negated = eq.negated,
                .dispatch_constraint_ty = try self.instantiateType(eq.dispatch_constraint_ty),
            } },
            .dispatch_call => |method_call| .{ .dispatch_call = .{
                .receiver = try self.instantiateExpr(method_call.receiver),
                .method_name = method_call.method_name,
                .args = try self.instantiateExprSpan(method_call.args),
                .dispatch_constraint_ty = try self.instantiateType(method_call.dispatch_constraint_ty),
            } },
            .type_dispatch_call => |method_call| .{ .type_dispatch_call = .{
                .dispatcher_ty = try self.instantiateType(method_call.dispatcher_ty),
                .method_name = method_call.method_name,
                .args = try self.instantiateExprSpan(method_call.args),
                .dispatch_constraint_ty = try self.instantiateType(method_call.dispatch_constraint_ty),
            } },
            .let_ => |let_expr| .{ .let_ = .{
                .bind = try self.instantiateTypedSymbol(let_expr.bind),
                .body = try self.instantiateExpr(let_expr.body),
                .rest = try self.instantiateExpr(let_expr.rest),
            } },
            .call => |call| .{ .call = .{
                .func = try self.instantiateExpr(call.func),
                .args = try self.instantiateExprSpan(call.args),
                .call_constraint_ty = try self.instantiateType(call.call_constraint_ty),
            } },
            .inspect => |value| .{ .inspect = try self.instantiateExpr(value) },
            .low_level => |ll| .{ .low_level = .{
                .op = ll.op,
                .args = try self.instantiateExprSpan(ll.args),
                .source_constraint_ty = try self.instantiateType(ll.source_constraint_ty),
            } },
            .when => |when_expr| .{ .when = .{
                .cond = try self.instantiateExpr(when_expr.cond),
                .branches = try self.instantiateBranchSpan(when_expr.branches),
                .is_try_suffix = when_expr.is_try_suffix,
            } },
            .if_ => |if_expr| .{ .if_ = .{
                .cond = try self.instantiateExpr(if_expr.cond),
                .then_body = try self.instantiateExpr(if_expr.then_body),
                .else_body = try self.instantiateExpr(if_expr.else_body),
            } },
            .block => |block| .{ .block = .{
                .stmts = try self.instantiateStmtSpan(block.stmts),
                .final_expr = try self.instantiateExpr(block.final_expr),
            } },
            .tuple => |tuple| .{ .tuple = try self.instantiateExprSpan(tuple) },
            .tag_payload => |tag_payload| .{ .tag_payload = .{
                .tag_union = try self.instantiateExpr(tag_payload.tag_union),
                .tag_name = tag_payload.tag_name,
                .tag_discriminant = tag_payload.tag_discriminant,
                .payload_index = tag_payload.payload_index,
            } },
            .tuple_access => |tuple_access| .{ .tuple_access = .{
                .tuple = try self.instantiateExpr(tuple_access.tuple),
                .elem_index = tuple_access.elem_index,
            } },
            .list => |list| .{ .list = try self.instantiateExprSpan(list) },
            .return_ => |ret| .{ .return_ = try self.instantiateExpr(ret) },
            .runtime_error => |msg| .{ .runtime_error = msg },
            .for_ => |for_expr| .{ .for_ = .{
                .patt = try self.instantiatePat(for_expr.patt),
                .iterable = try self.instantiateExpr(for_expr.iterable),
                .body = try self.instantiateExpr(for_expr.body),
            } },
        };
        const lowered = try self.output.addExpr(.{ .ty = ty, .data = data });
        if (self.current_expr_instantiate_cache) |cache| {
            try cache.put(expr_id, lowered);
        }
        if (self.output.getExpr(lowered).data == .let_) {
            const let_expr = self.output.getExpr(lowered).data.let_;
            try self.recordExactCallableAlias(let_expr.bind.symbol, let_expr.body);
        }
        return lowered;
    }

    fn instantiateExprSpan(self: *Lowerer, span: LiftedAst.Span(LiftedAst.ExprId)) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const values = self.input.store.sliceExprSpan(span);
        const out = try self.allocator.alloc(ast.ExprId, values.len);
        defer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = try self.instantiateExpr(value);
        }
        return try self.output.addExprSpan(out);
    }

    fn instantiateFieldSpan(self: *Lowerer, span: LiftedAst.Span(LiftedAst.FieldExpr)) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const values = self.input.store.sliceFieldExprSpan(span);
        const out = try self.allocator.alloc(ast.FieldExpr, values.len);
        defer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = .{
                .name = value.name,
                .value = try self.instantiateExpr(value.value),
            };
        }
        return try self.output.addFieldExprSpan(out);
    }

    fn instantiatePat(self: *Lowerer, pat_id: LiftedAst.PatId) std.mem.Allocator.Error!ast.PatId {
        const pat = self.input.store.getPat(pat_id);
        const ty = try self.instantiateType(pat.ty);
        const data: ast.Pat.Data = switch (pat.data) {
            .var_ => |symbol| .{ .var_ = symbol },
            .bool_lit => |value| .{ .bool_lit = value },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .discriminant = tag.discriminant,
                .args = try self.instantiatePatSpan(tag.args),
            } },
        };
        return try self.output.addPat(.{ .ty = ty, .data = data });
    }

    fn instantiatePatSpan(self: *Lowerer, span: LiftedAst.Span(LiftedAst.PatId)) std.mem.Allocator.Error!ast.Span(ast.PatId) {
        const values = self.input.store.slicePatSpan(span);
        const out = try self.allocator.alloc(ast.PatId, values.len);
        defer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = try self.instantiatePat(value);
        }
        return try self.output.addPatSpan(out);
    }

    fn instantiateBranchSpan(self: *Lowerer, span: LiftedAst.Span(LiftedAst.BranchId)) std.mem.Allocator.Error!ast.Span(ast.BranchId) {
        const values = self.input.store.sliceBranchSpan(span);
        const out = try self.allocator.alloc(ast.Branch, values.len);
        defer self.allocator.free(out);
        for (values, 0..) |value, i| {
            const branch = self.input.store.getBranch(value);
            out[i] = .{
                .pat = try self.instantiatePat(branch.pat),
                .body = try self.instantiateExpr(branch.body),
            };
        }
        return try self.output.addBranchSpan(out);
    }

    fn instantiateStmt(self: *Lowerer, stmt_id: LiftedAst.StmtId) std.mem.Allocator.Error!ast.StmtId {
        const stmt = self.input.store.getStmt(stmt_id);
        const lowered: ast.Stmt = switch (stmt) {
            .decl => |decl| .{ .decl = .{
                .bind = try self.instantiateTypedSymbol(decl.bind),
                .body = try self.instantiateExpr(decl.body),
            } },
            .var_decl => |decl| .{ .var_decl = .{
                .bind = try self.instantiateTypedSymbol(decl.bind),
                .body = try self.instantiateExpr(decl.body),
            } },
            .reassign => |reassign| .{ .reassign = .{
                .target = reassign.target,
                .body = try self.instantiateExpr(reassign.body),
            } },
            .expr => |expr_id| .{ .expr = try self.instantiateExpr(expr_id) },
            .debug => |expr_id| .{ .debug = try self.instantiateExpr(expr_id) },
            .expect => |expr_id| .{ .expect = try self.instantiateExpr(expr_id) },
            .crash => |msg| .{ .crash = msg },
            .return_ => |expr_id| .{ .return_ = try self.instantiateExpr(expr_id) },
            .break_ => .break_,
            .for_ => |for_stmt| .{ .for_ = .{
                .patt = try self.instantiatePat(for_stmt.patt),
                .iterable = try self.instantiateExpr(for_stmt.iterable),
                .body = try self.instantiateExpr(for_stmt.body),
            } },
            .while_ => |while_stmt| .{ .while_ = .{
                .cond = try self.instantiateExpr(while_stmt.cond),
                .body = try self.instantiateExpr(while_stmt.body),
            } },
        };
        const stmt_out = try self.output.addStmt(lowered);
        switch (self.output.getStmt(stmt_out)) {
            .decl => |decl| try self.recordExactCallableAlias(decl.bind.symbol, decl.body),
            .var_decl => |decl| try self.recordExactCallableAlias(decl.bind.symbol, decl.body),
            else => {},
        }
        return stmt_out;
    }

    fn instantiateStmtSpan(self: *Lowerer, span: LiftedAst.Span(LiftedAst.StmtId)) std.mem.Allocator.Error!ast.Span(ast.StmtId) {
        const values = self.input.store.sliceStmtSpan(span);
        const out = try self.allocator.alloc(ast.StmtId, values.len);
        defer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = try self.instantiateStmt(value);
        }
        return try self.output.addStmtSpan(out);
    }

    fn instantiateType(self: *Lowerer, ty: LiftedType.TypeId) std.mem.Allocator.Error!TypeVarId {
        if (self.current_instantiate_cache) |cache| {
            return try self.instantiateTypeRec(ty, cache);
        }
        var cache = std.AutoHashMap(LiftedType.TypeId, TypeVarId).init(self.allocator);
        defer cache.deinit();
        return try self.instantiateTypeRec(ty, &cache);
    }

    fn instantiateTypeRec(
        self: *Lowerer,
        ty: LiftedType.TypeId,
        cache: *std.AutoHashMap(LiftedType.TypeId, TypeVarId),
    ) std.mem.Allocator.Error!TypeVarId {
        if (cache.get(ty)) |cached| return cached;

        const placeholder = try self.types.freshUnbd();
        try cache.put(ty, placeholder);

        const mono_ty = self.input.types.getTypePreservingNominal(ty);
        if (mono_ty == .unbd) {
            return placeholder;
        }

        const lowered = switch (mono_ty) {
            .placeholder => debugPanic("lambdasolved.instantiateTypeRec leaked monotype builder placeholder", .{}),
            .unbd => type_mod.Node.unbd,
            .link => unreachable,
            .nominal => |nominal| blk: {
                const args = nominal.args;
                const lowered_args = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(lowered_args);
                for (args, 0..) |arg, i| {
                    lowered_args[i] = try self.instantiateTypeRec(arg, cache);
                }
                const lowered_backing = try self.instantiateTypeRec(nominal.backing, cache);
                break :blk type_mod.Node{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.types.addTypeVarSpan(lowered_args),
                    .backing = lowered_backing,
                } };
            },
            .func => |func| blk: {
                const lowered_args = try self.allocator.alloc(TypeVarId, func.args.len);
                defer self.allocator.free(lowered_args);
                for (func.args, 0..) |arg, i| {
                    lowered_args[i] = try self.instantiateTypeRec(arg, cache);
                }
                break :blk type_mod.Node{ .content = .{ .func = .{
                    .args = try self.types.addTypeVarSpan(lowered_args),
                    .lset = if (func.lambdas.len == 0)
                        try self.types.freshUnbd()
                    else
                        try self.instantiateExactCallableLset(func.lambdas),
                    .ret = try self.instantiateTypeRec(func.ret, cache),
                } } };
            },
            .list => |elem| type_mod.Node{ .content = .{ .list = try self.instantiateTypeRec(elem, cache) } },
            .box => |elem| type_mod.Node{ .content = .{ .box = try self.instantiateTypeRec(elem, cache) } },
            .tuple => |tuple| blk: {
                const elems = tuple;
                const out = try self.allocator.alloc(TypeVarId, elems.len);
                defer self.allocator.free(out);
                for (elems, 0..) |elem, i| {
                    out[i] = try self.instantiateTypeRec(elem, cache);
                }
                break :blk type_mod.Node{ .content = .{ .tuple = try self.types.addTypeVarSpan(out) } };
            },
            .tag_union => |tag_union| blk: {
                const tags = tag_union.tags;
                const out = try self.allocator.alloc(type_mod.Tag, tags.len);
                defer self.allocator.free(out);
                for (tags, 0..) |tag, i| {
                    const arg_ids = tag.args;
                    const lowered_args = try self.allocator.alloc(TypeVarId, arg_ids.len);
                    defer self.allocator.free(lowered_args);
                    for (arg_ids, 0..) |arg_id, arg_i| {
                        lowered_args[arg_i] = try self.instantiateTypeRec(arg_id, cache);
                    }
                    out[i] = .{
                        .name = tag.name,
                        .args = try self.types.addTypeVarSpan(lowered_args),
                    };
                }
                break :blk type_mod.Node{ .content = .{ .tag_union = .{
                    .tags = try self.types.addTags(out),
                } } };
            },
            .record => |record| blk: {
                const fields = record.fields;
                const out = try self.allocator.alloc(type_mod.Field, fields.len);
                defer self.allocator.free(out);
                for (fields, 0..) |field, i| {
                    out[i] = .{
                        .name = field.name,
                        .ty = try self.instantiateTypeRec(field.ty, cache),
                    };
                }
                assertSortedFields(&self.input.idents, out);
                break :blk type_mod.Node{ .content = .{ .record = .{
                    .fields = try self.types.addFields(out),
                } } };
            },
            .primitive => |prim| type_mod.Node{ .content = .{ .primitive = prim } },
        };

        self.types.setNode(placeholder, lowered);
        return placeholder;
    }

    fn instantiateExactCallableLset(self: *Lowerer, symbols: []const Symbol) std.mem.Allocator.Error!TypeVarId {
        const lambdas = try self.allocator.alloc(type_mod.Lambda, symbols.len);
        defer self.allocator.free(lambdas);

        for (symbols, 0..) |symbol, i| {
            if (self.lifted_fn_capture_spans.get(symbol)) |captures| {
                if (captures.len != 0) {
                    debugPanic(
                        "lambdasolved invariant violated: captureful exact callable leaked through monotype callable transport",
                        .{},
                    );
                }
            }
            lambdas[i] = .{
                .symbol = symbol,
                .captures = type_mod.Span(type_mod.Capture).empty(),
            };
        }

        const lambda_span = try self.types.addLambdas(lambdas);
        return try self.types.freshContent(.{ .lambda_set = lambda_span });
    }

    fn instantiateOccurrenceType(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        return try self.instantiateOccurrenceTypeRec(ty, &mapping);
    }

    fn instantiateOccurrenceTypedSymbolsWithMapping(
        self: *Lowerer,
        values: []const ast.TypedSymbol,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ) std.mem.Allocator.Error![]ast.TypedSymbol {
        const out = try self.allocator.alloc(ast.TypedSymbol, values.len);
        errdefer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = .{
                .symbol = value.symbol,
                .ty = try self.instantiateOccurrenceTypeRec(value.ty, mapping),
            };
        }
        return out;
    }

    fn instantiateOccurrenceTypeRec(
        self: *Lowerer,
        ty: TypeVarId,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ) std.mem.Allocator.Error!TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        if (mapping.get(id)) |cached| return cached;

        switch (self.types.getNode(id)) {
            .unbd, .for_a, .flex_for_a => {
                const fresh = try self.types.freshUnbd();
                try mapping.put(id, fresh);
                return fresh;
            },
            else => {},
        }

        const placeholder = try self.types.freshUnbd();
        try mapping.put(id, placeholder);

        const copied = switch (self.types.getNode(id)) {
            .unbd, .for_a, .flex_for_a => unreachable,
            .link => unreachable,
            .nominal => |nominal| blk: {
                const args = try self.allocator.dupe(TypeVarId, self.types.sliceTypeVarSpan(nominal.args));
                defer self.allocator.free(args);
                const out_args = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(out_args);
                for (args, 0..) |arg, i| {
                    out_args[i] = try self.instantiateOccurrenceTypeRec(arg, mapping);
                }
                break :blk type_mod.Node{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.types.addTypeVarSpan(out_args),
                    .backing = try self.instantiateOccurrenceTypeRec(nominal.backing, mapping),
                } };
            },
            .content => |content| switch (content) {
                .primitive => type_mod.Node{ .content = .{ .primitive = content.primitive } },
                .func => type_mod.Node{ .content = .{ .func = .{
                    .args = try self.instantiateOccurrenceTypeVarSpanWithMapping(self.types.sliceTypeVarSpan(content.func.args), mapping),
                    .lset = try self.instantiateOccurrenceTypeRec(content.func.lset, mapping),
                    .ret = try self.instantiateOccurrenceTypeRec(content.func.ret, mapping),
                } } },
                .list => |elem| type_mod.Node{ .content = .{
                    .list = try self.instantiateOccurrenceTypeRec(elem, mapping),
                } },
                .box => |elem| type_mod.Node{ .content = .{
                    .box = try self.instantiateOccurrenceTypeRec(elem, mapping),
                } },
                .tuple => |tuple| blk: {
                    const elems = try self.allocator.dupe(TypeVarId, self.types.sliceTypeVarSpan(tuple));
                    defer self.allocator.free(elems);
                    const out_elems = try self.allocator.alloc(TypeVarId, elems.len);
                    defer self.allocator.free(out_elems);
                    for (elems, 0..) |elem, i| {
                        out_elems[i] = try self.instantiateOccurrenceTypeRec(elem, mapping);
                    }
                    break :blk type_mod.Node{ .content = .{
                        .tuple = try self.types.addTypeVarSpan(out_elems),
                    } };
                },
                .tag_union => |tag_union| blk: {
                    const tags = try self.allocator.dupe(type_mod.Tag, self.types.sliceTags(tag_union.tags));
                    defer self.allocator.free(tags);
                    const out_tags = try self.allocator.alloc(type_mod.Tag, tags.len);
                    defer self.allocator.free(out_tags);
                    for (tags, 0..) |tag, i| {
                        const args = try self.allocator.dupe(TypeVarId, self.types.sliceTypeVarSpan(tag.args));
                        defer self.allocator.free(args);
                        const out_args = try self.allocator.alloc(TypeVarId, args.len);
                        defer self.allocator.free(out_args);
                        for (args, 0..) |arg, arg_i| {
                            out_args[arg_i] = try self.instantiateOccurrenceTypeRec(arg, mapping);
                        }
                        out_tags[i] = .{
                            .name = tag.name,
                            .args = try self.types.addTypeVarSpan(out_args),
                        };
                    }
                    break :blk type_mod.Node{ .content = .{
                        .tag_union = .{ .tags = try self.types.addTags(out_tags) },
                    } };
                },
                .record => |record| blk: {
                    const fields = try self.allocator.dupe(type_mod.Field, self.types.sliceFields(record.fields));
                    defer self.allocator.free(fields);
                    const out_fields = try self.allocator.alloc(type_mod.Field, fields.len);
                    defer self.allocator.free(out_fields);
                    for (fields, 0..) |field, i| {
                        out_fields[i] = .{
                            .name = field.name,
                            .ty = try self.instantiateOccurrenceTypeRec(field.ty, mapping),
                        };
                    }
                    sortFieldsByName(&self.input.idents, out_fields);
                    break :blk type_mod.Node{ .content = .{
                        .record = .{ .fields = try self.types.addFields(out_fields) },
                    } };
                },
                .lambda_set => |lambda_set| blk: {
                    const lambdas = try self.allocator.dupe(type_mod.Lambda, self.types.sliceLambdas(lambda_set));
                    defer self.allocator.free(lambdas);
                    const out_lambdas = try self.allocator.alloc(type_mod.Lambda, lambdas.len);
                    defer self.allocator.free(out_lambdas);
                    for (lambdas, 0..) |lambda, i| {
                        const captures = try self.allocator.dupe(type_mod.Capture, self.types.sliceCaptures(lambda.captures));
                        defer self.allocator.free(captures);
                        const out_captures = try self.allocator.alloc(type_mod.Capture, captures.len);
                        defer self.allocator.free(out_captures);
                        for (captures, 0..) |capture, capture_i| {
                            out_captures[capture_i] = .{
                                .symbol = capture.symbol,
                                .ty = try self.instantiateOccurrenceTypeRec(capture.ty, mapping),
                            };
                        }
                        out_lambdas[i] = .{
                            .symbol = lambda.symbol,
                            .captures = try self.types.addCaptures(out_captures),
                        };
                    }
                    break :blk type_mod.Node{ .content = .{
                        .lambda_set = try self.types.addLambdas(out_lambdas),
                    } };
                },
            },
        };

        self.types.setNode(placeholder, copied);
        return placeholder;
    }

    fn instantiateOccurrenceTypeVarSpanWithMapping(
        self: *Lowerer,
        tys: []const TypeVarId,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ) std.mem.Allocator.Error!type_mod.Span(TypeVarId) {
        if (tys.len == 0) return type_mod.Span(TypeVarId).empty();

        const tys_copy = try self.allocator.dupe(TypeVarId, tys);
        defer self.allocator.free(tys_copy);
        const out = try self.allocator.alloc(TypeVarId, tys_copy.len);
        defer self.allocator.free(out);
        for (tys_copy, 0..) |inner_ty, i| {
            out[i] = try self.instantiateOccurrenceTypeRec(inner_ty, mapping);
        }
        return try self.types.addTypeVarSpan(out);
    }

    fn snapshotTypeVarSpanWithMapping(
        self: *Lowerer,
        tys: []const TypeVarId,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ) std.mem.Allocator.Error!type_mod.Span(TypeVarId) {
        if (tys.len == 0) return type_mod.Span(TypeVarId).empty();

        const out = try self.allocator.alloc(TypeVarId, tys.len);
        defer self.allocator.free(out);
        for (tys, 0..) |ty, i| {
            out[i] = try self.snapshotTypeRec(ty, mapping);
        }
        return try self.types.addTypeVarSpan(out);
    }

    fn snapshotTypeRec(
        self: *Lowerer,
        ty: TypeVarId,
        mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ) std.mem.Allocator.Error!TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        if (mapping.get(id)) |cached| return cached;

        switch (self.types.getNode(id)) {
            .unbd => {
                const fresh = try self.types.freshUnbd();
                try mapping.put(id, fresh);
                return fresh;
            },
            .for_a => {
                const fresh = try self.types.freshForA();
                try mapping.put(id, fresh);
                return fresh;
            },
            .flex_for_a => {
                const fresh = try self.types.freshFlexForA();
                try mapping.put(id, fresh);
                return fresh;
            },
            else => {},
        }

        const placeholder = try self.types.freshUnbd();
        try mapping.put(id, placeholder);

        const frozen = switch (self.types.getNode(id)) {
            .unbd, .for_a, .flex_for_a => unreachable,
            .link => unreachable,
            .nominal => |nominal| blk: {
                const args = try self.allocator.dupe(TypeVarId, self.types.sliceTypeVarSpan(nominal.args));
                defer self.allocator.free(args);
                const out_args = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(out_args);
                for (args, 0..) |arg, i| {
                    out_args[i] = try self.snapshotTypeRec(arg, mapping);
                }
                break :blk type_mod.Node{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.types.addTypeVarSpan(out_args),
                    .backing = try self.snapshotTypeRec(nominal.backing, mapping),
                } };
            },
            .content => |content| switch (content) {
                .primitive => type_mod.Node{ .content = .{ .primitive = content.primitive } },
                .func => blk: {
                    const src_args = self.types.sliceTypeVarSpan(content.func.args);
                    const out_args = try self.allocator.alloc(TypeVarId, src_args.len);
                    defer self.allocator.free(out_args);
                    for (src_args, 0..) |arg, i| {
                        out_args[i] = try self.snapshotTypeRec(arg, mapping);
                    }
                    break :blk type_mod.Node{ .content = .{ .func = .{
                        .args = try self.types.addTypeVarSpan(out_args),
                        .lset = try self.snapshotTypeRec(content.func.lset, mapping),
                        .ret = try self.snapshotTypeRec(content.func.ret, mapping),
                    } } };
                },
                .list => |elem| type_mod.Node{ .content = .{
                    .list = try self.snapshotTypeRec(elem, mapping),
                } },
                .box => |elem| type_mod.Node{ .content = .{
                    .box = try self.snapshotTypeRec(elem, mapping),
                } },
                .tuple => |tuple| blk: {
                    const elems = try self.allocator.dupe(TypeVarId, self.types.sliceTypeVarSpan(tuple));
                    defer self.allocator.free(elems);
                    const out_elems = try self.allocator.alloc(TypeVarId, elems.len);
                    defer self.allocator.free(out_elems);
                    for (elems, 0..) |elem, i| {
                        out_elems[i] = try self.snapshotTypeRec(elem, mapping);
                    }
                    break :blk type_mod.Node{ .content = .{
                        .tuple = try self.types.addTypeVarSpan(out_elems),
                    } };
                },
                .tag_union => |tag_union| blk: {
                    const tags = try self.allocator.dupe(type_mod.Tag, self.types.sliceTags(tag_union.tags));
                    defer self.allocator.free(tags);
                    const out_tags = try self.allocator.alloc(type_mod.Tag, tags.len);
                    defer self.allocator.free(out_tags);
                    for (tags, 0..) |tag, i| {
                        const args = try self.allocator.dupe(TypeVarId, self.types.sliceTypeVarSpan(tag.args));
                        defer self.allocator.free(args);
                        const out_args = try self.allocator.alloc(TypeVarId, args.len);
                        defer self.allocator.free(out_args);
                        for (args, 0..) |arg, arg_i| {
                            out_args[arg_i] = try self.snapshotTypeRec(arg, mapping);
                        }
                        out_tags[i] = .{
                            .name = tag.name,
                            .args = try self.types.addTypeVarSpan(out_args),
                        };
                    }
                    break :blk type_mod.Node{ .content = .{
                        .tag_union = .{ .tags = try self.types.addTags(out_tags) },
                    } };
                },
                .record => |record| blk: {
                    const fields = try self.allocator.dupe(type_mod.Field, self.types.sliceFields(record.fields));
                    defer self.allocator.free(fields);
                    const out_fields = try self.allocator.alloc(type_mod.Field, fields.len);
                    defer self.allocator.free(out_fields);
                    for (fields, 0..) |field, i| {
                        out_fields[i] = .{
                            .name = field.name,
                            .ty = try self.snapshotTypeRec(field.ty, mapping),
                        };
                    }
                    sortFieldsByName(&self.input.idents, out_fields);
                    break :blk type_mod.Node{ .content = .{
                        .record = .{ .fields = try self.types.addFields(out_fields) },
                    } };
                },
                .lambda_set => |lambda_set| blk: {
                    const lambdas = try self.allocator.dupe(type_mod.Lambda, self.types.sliceLambdas(lambda_set));
                    defer self.allocator.free(lambdas);
                    const out_lambdas = try self.allocator.alloc(type_mod.Lambda, lambdas.len);
                    defer self.allocator.free(out_lambdas);
                    for (lambdas, 0..) |lambda, i| {
                        const captures = try self.allocator.dupe(type_mod.Capture, self.types.sliceCaptures(lambda.captures));
                        defer self.allocator.free(captures);
                        const out_captures = try self.allocator.alloc(type_mod.Capture, captures.len);
                        defer self.allocator.free(out_captures);
                        for (captures, 0..) |capture, capture_i| {
                            out_captures[capture_i] = .{
                                .symbol = capture.symbol,
                                .ty = try self.snapshotTypeRec(capture.ty, mapping),
                            };
                        }
                        out_lambdas[i] = .{
                            .symbol = lambda.symbol,
                            .captures = try self.types.addCaptures(out_captures),
                        };
                    }
                    break :blk type_mod.Node{ .content = .{
                        .lambda_set = try self.types.addLambdas(out_lambdas),
                    } };
                },
            },
        };

        self.types.setNode(placeholder, frozen);
        return placeholder;
    }

    fn inferCallArgTypes(
        self: *Lowerer,
        venv: []const EnvEntry,
        arg_exprs: []const ast.ExprId,
    ) std.mem.Allocator.Error![]TypeVarId {
        const arg_tys = try self.allocator.alloc(TypeVarId, arg_exprs.len);
        errdefer self.allocator.free(arg_tys);
        for (arg_exprs, 0..) |arg, i| {
            const arg_ty = try self.inferExpr(venv, arg);
            self.output.exprs.items[@intFromEnum(arg)].ty = arg_ty;
            arg_tys[i] = arg_ty;
        }
        return arg_tys;
    }

    fn inferCallArgExprs(
        self: *Lowerer,
        venv: []const EnvEntry,
        arg_exprs: []const ast.ExprId,
    ) std.mem.Allocator.Error!void {
        for (arg_exprs) |arg| {
            const arg_ty = try self.inferExpr(venv, arg);
            self.output.exprs.items[@intFromEnum(arg)].ty = arg_ty;
        }
    }

    fn buildSolvedFunctionType(
        self: *Lowerer,
        arg_tys: []const TypeVarId,
        lset_ty: TypeVarId,
        ret_ty: TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        return try self.types.freshContent(.{ .func = .{
            .args = try self.types.addTypeVarSpan(arg_tys),
            .lset = lset_ty,
            .ret = ret_ty,
        } });
    }

    fn freeDebugCallContext(self: *Lowerer, ctx: DebugCallContext) void {
        if (std.mem.eql(u8, ctx.func_expr, "<none>") or std.mem.eql(u8, ctx.func_expr, "<pending>")) {} else self.allocator.free(ctx.func_expr);
        if (std.mem.eql(u8, ctx.arg_expr, "<none>") or std.mem.eql(u8, ctx.arg_expr, "<pending>")) {} else self.allocator.free(ctx.arg_expr);
        if (std.mem.eql(u8, ctx.func_ty, "<none>") or std.mem.eql(u8, ctx.func_ty, "<pending>")) {} else self.allocator.free(ctx.func_ty);
        if (std.mem.eql(u8, ctx.arg_ty, "<none>") or std.mem.eql(u8, ctx.arg_ty, "<pending>")) {} else self.allocator.free(ctx.arg_ty);
        if (std.mem.eql(u8, ctx.target_ty, "<none>") or std.mem.eql(u8, ctx.target_ty, "<pending>")) {} else self.allocator.free(ctx.target_ty);
    }

    fn restoreCallDebugContext(self: *Lowerer, previous: ?DebugCallContext) void {
        if (self.current_call_context) |ctx| self.freeDebugCallContext(ctx);
        self.current_call_context = previous;
    }

    fn withCallDebugContext(
        self: *Lowerer,
        func_expr_id: ast.ExprId,
        arg_exprs: []const ast.ExprId,
        func_ty: TypeVarId,
        arg_tys: []const TypeVarId,
        target_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        if (builtin.mode != .Debug) return;

        const func_expr_summary = self.debugTypeSummary(self.output.getExpr(func_expr_id).ty);
        defer if (func_expr_summary.owned) self.allocator.free(func_expr_summary.text);
        const func_ty_summary = self.debugTypeSummary(func_ty);
        defer if (func_ty_summary.owned) self.allocator.free(func_ty_summary.text);
        const target_ty_summary = self.debugTypeSummary(target_ty);
        defer if (target_ty_summary.owned) self.allocator.free(target_ty_summary.text);

        const arg_expr_summary = if (arg_exprs.len != 0) blk: {
            const summary = self.debugTypeSummary(self.output.getExpr(arg_exprs[0]).ty);
            break :blk summary;
        } else DebugTypeText{ .text = "<none>", .owned = false };
        defer if (arg_expr_summary.owned) self.allocator.free(arg_expr_summary.text);

        const arg_ty_summary = if (arg_tys.len != 0) blk: {
            const summary = self.debugTypeSummary(arg_tys[0]);
            break :blk summary;
        } else DebugTypeText{ .text = "<none>", .owned = false };
        defer if (arg_ty_summary.owned) self.allocator.free(arg_ty_summary.text);

        if (self.current_call_context) |ctx| self.freeDebugCallContext(ctx);
        self.current_call_context = .{
            .func_expr = try self.allocator.dupe(u8, func_expr_summary.text),
            .arg_expr = try self.allocator.dupe(u8, arg_expr_summary.text),
            .func_ty = try self.allocator.dupe(u8, func_ty_summary.text),
            .arg_ty = try self.allocator.dupe(u8, arg_ty_summary.text),
            .target_ty = try self.allocator.dupe(u8, target_ty_summary.text),
        };
    }

    fn assertCallBoundaryNotGeneralized(
        self: *Lowerer,
        comptime tag: []const u8,
        func_ty: TypeVarId,
        arg_exprs: []const ast.ExprId,
        arg_tys: []const TypeVarId,
        target_ty: TypeVarId,
        wanted_func_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        if (builtin.mode != .Debug) return;

        const func_is_generalized = self.isGeneralized(func_ty);
        const target_is_generalized = self.isGeneralized(target_ty);
        const wanted_is_generalized = self.isGeneralized(wanted_func_ty);
        var first_generalized_arg: ?usize = null;
        for (arg_tys, 0..) |arg_ty, i| {
            if (self.isGeneralized(arg_ty)) {
                first_generalized_arg = i;
                break;
            }
        }
        if (!func_is_generalized and !target_is_generalized and !wanted_is_generalized and first_generalized_arg == null) return;

        const func_text = self.debugTypeSummary(func_ty);
        defer if (func_text.owned) self.allocator.free(func_text.text);
        const target_text = self.debugTypeSummary(target_ty);
        defer if (target_text.owned) self.allocator.free(target_text.text);
        const wanted_text = self.debugTypeSummary(wanted_func_ty);
        defer if (wanted_text.owned) self.allocator.free(wanted_text.text);
        const arg_text = if (first_generalized_arg) |idx| blk: {
            const summary = self.debugTypeSummary(arg_tys[idx]);
            break :blk summary;
        } else DebugTypeText{ .text = "<none>", .owned = false };
        defer if (arg_text.owned) self.allocator.free(arg_text.text);

        std.debug.panic(
            "lambdasolved.{s} generalized call boundary\nfunc_generalized={any}\nfirst_generalized_arg={any}\narg_expr_count={d}\ntarget_generalized={any}\nwanted_generalized={any}\nfunc_ty={s}\narg_ty={s}\ntarget_ty={s}\nwanted_ty={s}",
            .{
                tag,
                func_is_generalized,
                first_generalized_arg,
                arg_exprs.len,
                target_is_generalized,
                wanted_is_generalized,
                func_text.text,
                arg_text.text,
                target_text.text,
                wanted_text.text,
            },
        );
    }

    fn assertCallResultNotGeneralized(
        self: *Lowerer,
        comptime tag: []const u8,
        result_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        if (builtin.mode != .Debug or !self.isGeneralized(result_ty)) return;

        const result_text = self.debugTypeSummary(result_ty);
        defer if (result_text.owned) self.allocator.free(result_text.text);
        std.debug.panic(
            "lambdasolved.{s} generalized call result\nresult_ty={s}",
            .{ tag, result_text.text },
        );
    }

    fn inferProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        const sccs = try self.computeSccs();
        defer self.freeSccs(sccs);

        var venv: []EnvEntry = &.{};
        for (sccs) |group| {
            if (group.def_ids.len == 1) {
                const def = self.output.getDef(group.def_ids[0]);
                const previous_def = self.current_def_symbol;
                self.current_def_symbol = def.bind.symbol;
                defer self.current_def_symbol = previous_def;
                switch (def.value) {
                    .fn_ => |fn_def| {
                        if (self.needsLexicalFnInference(def.bind.symbol) and
                            !(try self.canInferFnInEnv(venv, def.bind.symbol)))
                        {
                            const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = def.bind.ty });
                            if (venv.len != 0) self.allocator.free(venv);
                            venv = next;
                            continue;
                        }

                        const t_fn = try self.types.freshUnbd();
                        const inferred = try self.inferFn(venv, .{ .symbol = def.bind.symbol, .ty = t_fn }, fn_def);
                        const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                        try self.unify(declared_ty, inferred);
                        try self.generalize(venv, inferred);
                        try self.fn_infer_states.put(def.bind.symbol, .done);
                        const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = inferred });
                        if (venv.len != 0) self.allocator.free(venv);
                        venv = next;
                    },
                    .hosted_fn => |hosted_fn| {
                        const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                        const inferred = try self.inferHostedFn(.{ .symbol = def.bind.symbol, .ty = declared_ty }, hosted_fn);
                        try self.generalize(venv, inferred);
                        try self.fn_infer_states.put(def.bind.symbol, .done);
                        const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = inferred });
                        if (venv.len != 0) self.allocator.free(venv);
                        venv = next;
                    },
                    .val => |expr_id| {
                        const inferred = try self.inferExpr(venv, expr_id);
                        const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                        try self.unify(declared_ty, inferred);
                        const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = def.bind.ty });
                        if (venv.len != 0) self.allocator.free(venv);
                        venv = next;
                    },
                    .run => |run_def| {
                        const inferred = try self.inferExpr(venv, run_def.body);
                        const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                        try self.unify(declared_ty, inferred);
                        const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = def.bind.ty });
                        if (venv.len != 0) self.allocator.free(venv);
                        venv = next;
                    },
                }
                continue;
            }

            var rec_entries = try self.allocator.alloc(EnvEntry, group.def_ids.len);
            defer self.allocator.free(rec_entries);
            for (group.def_ids, 0..) |def_id, i| {
                const def = self.output.getDef(def_id);
                if (def.value != .fn_) return debugPanic("lambdasolved.inferProgram non-function recursive SCC", .{});
                rec_entries[i] = .{
                    .symbol = def.bind.symbol,
                    .ty = try self.types.freshUnbd(),
                };
            }

            const rec_env = try self.extendEnvMany(venv, rec_entries);
            defer self.allocator.free(rec_env);

            var generalized = try self.allocator.alloc(EnvEntry, group.def_ids.len);
            defer self.allocator.free(generalized);

            for (group.def_ids, 0..) |def_id, i| {
                const def = self.output.getDef(def_id);
                const fn_def = def.value.fn_;
                const previous_def = self.current_def_symbol;
                self.current_def_symbol = def.bind.symbol;
                defer self.current_def_symbol = previous_def;
                generalized[i] = .{
                    .symbol = def.bind.symbol,
                    .ty = try self.inferFn(rec_env, rec_entries[i], fn_def),
                };
            }

            for (group.def_ids, 0..) |def_id, i| {
                const def = self.output.getDef(def_id);
                const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                try self.unify(declared_ty, generalized[i].ty);
                try self.generalize(venv, generalized[i].ty);
                try self.fn_infer_states.put(def.bind.symbol, .done);
            }

            const next = try self.extendEnvMany(venv, generalized);
            if (venv.len != 0) self.allocator.free(venv);
            venv = next;
        }

        if (venv.len != 0) self.allocator.free(venv);
    }

    fn canInferFnInEnv(self: *Lowerer, venv: []const EnvEntry, symbol: Symbol) std.mem.Allocator.Error!bool {
        const def_id = self.def_id_by_symbol.get(symbol) orelse return false;
        const def = self.output.getDef(def_id);
        return switch (def.value) {
            .fn_ => {
                const capture_span = self.lifted_fn_capture_spans.get(symbol) orelse
                    debugPanic("lambdasolved.canInferFnInEnv missing lifted capture metadata", .{});
                for (self.input.store.sliceTypedSymbolSpan(capture_span)) |capture| {
                    if (self.lookupEnv(venv, capture.symbol) == null) return false;
                }
                return true;
            },
            .hosted_fn => true,
            else => false,
        };
    }

    fn ensureFnInferred(self: *Lowerer, venv: []const EnvEntry, symbol: Symbol) std.mem.Allocator.Error!void {
        const state = self.fn_infer_states.get(symbol) orelse return;
        switch (state) {
            .done, .inferring => return,
            .pending => {},
        }

        const def_id = self.def_id_by_symbol.get(symbol) orelse return;
        const def = self.output.getDef(def_id);
        if (!try self.canInferFnInEnv(venv, symbol)) return;

        try self.fn_infer_states.put(symbol, .inferring);
        errdefer self.fn_infer_states.put(symbol, .pending) catch unreachable;

        const previous_def = self.current_def_symbol;
        self.current_def_symbol = symbol;
        defer self.current_def_symbol = previous_def;

        switch (def.value) {
            .fn_ => |fn_def| {
                const t_fn = try self.types.freshUnbd();
                const inferred = try self.inferFn(venv, .{ .symbol = symbol, .ty = t_fn }, fn_def);
                const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                try self.unify(declared_ty, inferred);
                try self.generalize(venv, inferred);
            },
            .hosted_fn => |hosted_fn| {
                const declared_ty = try self.instantiateGeneralized(def.bind.ty);
                const inferred = try self.inferHostedFn(.{ .symbol = symbol, .ty = declared_ty }, hosted_fn);
                try self.generalize(venv, inferred);
            },
            else => unreachable,
        }

        try self.fn_infer_states.put(symbol, .done);
    }

    fn requireCallableTypeForSymbol(
        self: *Lowerer,
        venv: []const EnvEntry,
        symbol: Symbol,
    ) std.mem.Allocator.Error!TypeVarId {
        if (symbol.isNone()) {
            debugPanic("lambdasolved.requireCallableTypeForSymbol missing symbol", .{});
        }
        if (builtin.mode == .Debug) {
            if (self.current_def_symbol) |current_def_symbol| {
                if (symbol == current_def_symbol) {
                    const entry = self.input.symbols.get(symbol);
                    std.debug.panic(
                        "lambdasolved.requireCallableTypeForSymbol unexpectedly resolved current def symbol {s} ({d}) as an exact method target",
                        .{
                            if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name),
                            symbol.raw(),
                        },
                    );
                }
            }
        }
        if (self.needsLexicalFnInference(symbol)) {
            try self.ensureFnInferred(venv, symbol);
        }
        if (self.lookupEnv(venv, symbol)) |env_ty| return try self.instantiateGeneralized(env_ty);
        const def_id = self.def_id_by_symbol.get(symbol) orelse debugPanic(
            "lambdasolved.requireCallableTypeForSymbol missing def for explicit callable symbol",
            .{},
        );
        return try self.instantiateGeneralized(self.output.getDef(def_id).bind.ty);
    }

    fn inferFn(self: *Lowerer, venv: []const EnvEntry, fn_entry: EnvEntry, fn_def: ast.FnDef) std.mem.Allocator.Error!TypeVarId {
        const lifted_capture_span = self.lifted_fn_capture_spans.get(fn_entry.symbol) orelse
            debugPanic("lambdasolved.inferFn missing lifted capture metadata", .{});
        const captures = try self.instantiateLiftedTypedSymbolSlice(lifted_capture_span);
        defer self.allocator.free(captures);
        const fn_args = self.output.sliceTypedSymbolSpan(fn_def.args);
        const captures_env = try self.allocator.alloc(EnvEntry, captures.len + fn_args.len + 1);
        defer self.allocator.free(captures_env);
        const lset_captures = try self.allocator.alloc(type_mod.Capture, captures.len);
        defer self.allocator.free(lset_captures);
        captures_env[0] = fn_entry;
        for (fn_args, 0..) |arg, i| {
            captures_env[i + 1] = .{ .symbol = arg.symbol, .ty = arg.ty };
        }
        for (captures, 0..) |capture, i| {
            const capture_ty = if (self.lookupEnv(venv, capture.symbol)) |env_ty|
                env_ty
            else
                capture.ty;
            captures_env[i + 1 + fn_args.len] = .{ .symbol = capture.symbol, .ty = capture_ty };
            lset_captures[i] = .{ .symbol = capture.symbol, .ty = capture_ty };
        }

        const body_env = try self.extendEnvMany(venv, captures_env);
        defer self.allocator.free(body_env);

        const fn_ret_ty = try self.types.freshUnbd();
        const previous_return_ty = self.current_return_ty;
        self.current_return_ty = fn_ret_ty;
        defer self.current_return_ty = previous_return_ty;

        const captures_span = try self.types.addCaptures(lset_captures);
        const lambda_span = try self.types.addLambdas(&.{.{ .symbol = fn_entry.symbol, .captures = captures_span }});
        const lset_ty = try self.types.freshContent(.{ .lambda_set = lambda_span });
        const arg_tys = try self.allocator.alloc(TypeVarId, fn_args.len);
        defer self.allocator.free(arg_tys);
        for (fn_args, 0..) |arg, i| arg_tys[i] = arg.ty;
        const fn_ty = try self.buildSolvedFunctionType(arg_tys, lset_ty, fn_ret_ty);
        try self.unify(fn_entry.ty, fn_ty);

        const body_ty = try self.inferExpr(body_env, fn_def.body);
        try self.unify(fn_ret_ty, body_ty);
        return fn_entry.ty;
    }

    fn inferHostedFn(self: *Lowerer, fn_entry: EnvEntry, _: ast.HostedFnDef) std.mem.Allocator.Error!TypeVarId {
        const captures_span = try self.types.addCaptures(&.{});
        const lambda_span = try self.types.addLambdas(&.{.{ .symbol = fn_entry.symbol, .captures = captures_span }});
        const lset_ty = try self.types.freshContent(.{ .lambda_set = lambda_span });
        const fn_ty = try self.rewriteFunctionLsetType(fn_entry.ty, lset_ty);
        try self.unify(fn_entry.ty, fn_ty);
        return fn_entry.ty;
    }

    fn needsLexicalFnInference(self: *Lowerer, symbol: Symbol) bool {
        const origin = self.input.symbols.get(symbol).origin;
        return switch (origin) {
            .lifted_local_fn, .lifted_local_fn_alias, .lifted_lambda => true,
            else => false,
        };
    }

    fn inferExpr(self: *Lowerer, venv: []const EnvEntry, expr_id: ast.ExprId) std.mem.Allocator.Error!TypeVarId {
        const previous_expr = self.current_expr_id;
        self.current_expr_id = expr_id;
        defer self.current_expr_id = previous_expr;

        const expr = self.output.getExpr(expr_id);
        const target_ty = expr.ty;

        const inferred = switch (expr.data) {
            .var_ => |symbol| blk: {
                if (symbol.raw() >= self.input.symbols.len()) {
                    return debugPanic(
                        "lambdasolved.inferExpr invalid symbol {d} (store len {d})",
                        .{ symbol.raw(), self.input.symbols.len() },
                    );
                }
                if (self.needsLexicalFnInference(symbol)) {
                    try self.ensureFnInferred(venv, symbol);
                }
                const symbol_entry = self.input.symbols.get(symbol);
                const env_ty = self.lookupEnv(venv, symbol) orelse {
                    if (symbol_entry.origin == .top_level_def and !self.def_id_by_symbol.contains(symbol)) {
                        break :blk target_ty;
                    }
                    const origin_tag = @tagName(symbol_entry.origin);
                    const symbol_name = if (symbol_entry.name.isNone())
                        "<none>"
                    else
                        self.input.idents.getText(symbol_entry.name);
                    const def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                    const def_name = def_name_blk: {
                        if (self.current_def_symbol) |def_symbol| {
                            const def_entry = self.input.symbols.get(def_symbol);
                            break :def_name_blk if (def_entry.name.isNone()) "<none>" else self.input.idents.getText(def_entry.name);
                        }
                        break :def_name_blk "<none>";
                    };
                    const def_id_raw = if (self.def_id_by_symbol.get(symbol)) |def_id| @intFromEnum(def_id) else std.math.maxInt(u32);
                    var use_count: usize = 0;
                    var bind_count: usize = 0;
                    var pat_bind_count: usize = 0;
                    var capture_count: usize = 0;
                    var reassign_target_count: usize = 0;
                    var first_use_expr: ?ast.ExprId = null;
                    if (self.current_def_symbol) |def_symbol| {
                        if (self.def_id_by_symbol.get(def_symbol)) |def_id| {
                            self.debugSymbolCounts(
                                def_id,
                                symbol,
                                &use_count,
                                &bind_count,
                                &pat_bind_count,
                                &capture_count,
                                &reassign_target_count,
                                &first_use_expr,
                            );
                        }
                    }
                    const first_use_raw: u32 = if (first_use_expr) |expr_id_local|
                        @intFromEnum(expr_id_local)
                    else
                        std.math.maxInt(u32);
                    return debugPanic(
                        "lambdasolved.inferExpr unbound variable {d} ({s}) origin {s} in def {d} ({s}) uses {d} binds {d} pat_binds {d} captures {d} reassign_targets {d} first_use_expr {d} def_id {d}",
                        .{
                            symbol.raw(),
                            symbol_name,
                            origin_tag,
                            def_raw,
                            def_name,
                            use_count,
                            bind_count,
                            pat_bind_count,
                            capture_count,
                            reassign_target_count,
                            first_use_raw,
                            def_id_raw,
                        },
                    );
                };
                const inst_ty = try self.instantiateGeneralized(env_ty);
                try self.fixCaptures(venv, inst_ty, symbol);
                break :blk inst_ty;
            },
            .int_lit => blk: {
                break :blk target_ty;
            },
            .frac_f32_lit => target_ty,
            .frac_f64_lit => target_ty,
            .dec_lit => target_ty,
            .str_lit => target_ty,
            .bool_lit => target_ty,
            .unit => target_ty,
            .tag => |tag| blk: {
                const args = self.output.sliceExprSpan(tag.args);
                if (try self.isPrimitiveBoolType(target_ty)) {
                    if (args.len != 0) {
                        return debugPanic("lambdasolved.bool tag invariant violated: bool tags cannot carry arguments", .{});
                    }
                    break :blk target_ty;
                }
                const arg_tys = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(arg_tys);
                for (args, 0..) |arg, i| {
                    arg_tys[i] = try self.inferExpr(venv, arg);
                }
                const wanted = try self.types.freshContent(.{ .tag_union = .{
                    .tags = try self.types.addTags(&.{.{
                        .name = tag.name,
                        .args = try self.types.addTypeVarSpan(arg_tys),
                    }}),
                } });
                try self.unifyTargetWithWantedPreservingNominal(target_ty, wanted);
                break :blk target_ty;
            },
            .record => |fields| blk: {
                const field_values = self.output.sliceFieldExprSpan(fields);
                const field_tys = try self.allocator.alloc(type_mod.Field, field_values.len);
                defer self.allocator.free(field_tys);
                for (field_values, 0..) |field, i| {
                    field_tys[i] = .{
                        .name = field.name,
                        .ty = try self.inferExpr(venv, field.value),
                    };
                }
                std.mem.sort(type_mod.Field, field_tys, &self.input.idents, struct {
                    fn lessThan(idents: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
                        return std.mem.lessThan(
                            u8,
                            idents.getText(a.name),
                            idents.getText(b.name),
                        );
                    }
                }.lessThan);
                const wanted = try self.types.freshContent(.{ .record = .{
                    .fields = try self.types.addFields(field_tys),
                } });
                try self.unifyTargetWithWantedPreservingNominal(target_ty, wanted);
                break :blk target_ty;
            },
            .access => |access| blk: {
                const record_ty = try self.inferExpr(venv, access.record);
                const subject_ty = self.output.getExpr(access.record).ty;
                try self.unify(record_ty, subject_ty);
                const subject_root = self.types.unlinkErasingNominal(subject_ty);
                const subject_node = self.types.getNode(subject_root);
                const fields = switch (subject_node) {
                    .content => |content| switch (content) {
                        .record => |record| self.types.sliceFields(record.fields),
                        else => return debugPanic("lambdasolved.access invariant violated: subject is not a record", .{}),
                    },
                    else => return debugPanic("lambdasolved.access invariant violated: subject record type is unresolved", .{}),
                };
                const field_ty = for (fields) |field| {
                    if (field.name.eql(access.field)) break field.ty;
                } else return debugPanic("lambdasolved.access invariant violated: missing record field", .{});
                try self.unify(target_ty, field_ty);
                break :blk target_ty;
            },
            .structural_eq => |eq| blk: {
                const lhs_ty = try self.inferExpr(venv, eq.lhs);
                const rhs_ty = try self.inferExpr(venv, eq.rhs);
                try self.unify(lhs_ty, self.output.getExpr(eq.lhs).ty);
                try self.unify(rhs_ty, self.output.getExpr(eq.rhs).ty);
                try self.unify(lhs_ty, rhs_ty);
                try self.unify(target_ty, try self.freshPrimitiveType(.bool));
                break :blk target_ty;
            },
            .method_eq => |eq| blk: {
                const lhs_ty = try self.inferExpr(venv, eq.lhs);
                const rhs_ty = try self.inferExpr(venv, eq.rhs);
                try self.unify(lhs_ty, self.output.getExpr(eq.lhs).ty);
                try self.unify(rhs_ty, self.output.getExpr(eq.rhs).ty);
                const constraint_shape = self.types.fnShape(eq.dispatch_constraint_ty);
                const constraint_args = self.types.sliceTypeVarSpan(constraint_shape.args);
                if (constraint_args.len != 2) {
                    debugPanic("lambdasolved.method_eq invariant violated: dispatch arg arity mismatch", .{});
                }
                try self.unify(constraint_args[0], lhs_ty);
                try self.unify(constraint_args[1], rhs_ty);
                try self.unify(target_ty, constraint_shape.ret);
                break :blk target_ty;
            },
            .dispatch_call => |method_call| blk: {
                const receiver = method_call.receiver;
                const receiver_ty = try self.inferExpr(venv, receiver);
                try self.unify(receiver_ty, self.output.getExpr(receiver).ty);
                const arg_tys = try self.inferCallArgTypes(venv, self.output.sliceExprSpan(method_call.args));
                defer self.allocator.free(arg_tys);
                const constraint_shape = self.types.fnShape(method_call.dispatch_constraint_ty);
                const constraint_args = self.types.sliceTypeVarSpan(constraint_shape.args);
                if (constraint_args.len != arg_tys.len + 1) {
                    debugPanic("lambdasolved.dispatch_call invariant violated: dispatch arg arity mismatch", .{});
                }
                try self.unify(constraint_args[0], receiver_ty);
                for (arg_tys, 0..) |arg_ty, i| {
                    try self.unify(constraint_args[i + 1], arg_ty);
                }
                try self.unify(target_ty, constraint_shape.ret);
                break :blk target_ty;
            },
            .type_dispatch_call => |method_call| blk: {
                const arg_tys = try self.inferCallArgTypes(venv, self.output.sliceExprSpan(method_call.args));
                defer self.allocator.free(arg_tys);
                const constraint_shape = self.types.fnShape(method_call.dispatch_constraint_ty);
                const constraint_args = self.types.sliceTypeVarSpan(constraint_shape.args);
                if (constraint_args.len != arg_tys.len) {
                    debugPanic("lambdasolved.type_dispatch_call invariant violated: dispatch arg arity mismatch", .{});
                }
                for (arg_tys, 0..) |arg_ty, i| {
                    try self.unify(constraint_args[i], arg_ty);
                }
                try self.unify(target_ty, constraint_shape.ret);
                break :blk target_ty;
            },
            .let_ => |let_expr| blk: {
                const body_ty = try self.inferExpr(venv, let_expr.body);
                try self.unify(body_ty, let_expr.bind.ty);
                try self.generalize(venv, let_expr.bind.ty);
                const rest_env = try self.extendEnvOne(venv, .{ .symbol = let_expr.bind.symbol, .ty = let_expr.bind.ty });
                defer self.allocator.free(rest_env);
                break :blk try self.inferExpr(rest_env, let_expr.rest);
            },
            .call => |call| blk: {
                const call_args = try self.allocator.dupe(ast.ExprId, self.output.sliceExprSpan(call.args));
                defer self.allocator.free(call_args);
                const previous_call_context = self.current_call_context;
                defer self.restoreCallDebugContext(previous_call_context);

                const func_ty = try self.inferExpr(venv, call.func);
                const arg_tys = try self.inferCallArgTypes(venv, call_args);
                defer self.allocator.free(arg_tys);
                const constraint_shape = self.types.fnShape(call.call_constraint_ty);
                const constraint_args = self.types.sliceTypeVarSpan(constraint_shape.args);
                if (constraint_args.len != arg_tys.len) {
                    debugPanic("lambdasolved.call invariant violated: explicit call constraint arity mismatch", .{});
                }
                try self.withCallDebugContext(call.func, call_args, func_ty, arg_tys, target_ty);
                try self.assertCallBoundaryNotGeneralized("call", func_ty, call_args, arg_tys, target_ty, call.call_constraint_ty);
                try self.unify(func_ty, call.call_constraint_ty);
                for (constraint_args, arg_tys) |constraint_arg_ty, arg_ty| {
                    try self.unify(constraint_arg_ty, arg_ty);
                }
                try self.unify(target_ty, constraint_shape.ret);
                try self.assertCallResultNotGeneralized("call", target_ty);
                break :blk target_ty;
            },
            .inspect => |value| blk: {
                const value_ty = try self.inferExpr(venv, value);
                try self.unify(value_ty, value_ty);
                try self.unify(target_ty, try self.freshPrimitiveType(.str));
                break :blk target_ty;
            },
            .low_level => |ll| blk: {
                const args = self.output.sliceExprSpan(ll.args);
                const arg_tys = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(arg_tys);
                for (args, 0..) |arg, i| {
                    const arg_ty = try self.inferExpr(venv, arg);
                    arg_tys[i] = arg_ty;
                    try self.unify(arg_ty, self.output.getExpr(arg).ty);
                }
                const constraint_shape = self.types.fnShape(ll.source_constraint_ty);
                const constraint_args = self.types.sliceTypeVarSpan(constraint_shape.args);
                if (constraint_args.len != arg_tys.len) {
                    return debugPanic("lambdasolved.low_level invariant violated: source arg arity mismatch", .{});
                }
                for (constraint_args, arg_tys) |constraint_arg_ty, arg_ty| {
                    try self.unify(constraint_arg_ty, arg_ty);
                }
                try self.unify(target_ty, constraint_shape.ret);
                break :blk target_ty;
            },
            .when => |when_expr| blk: {
                const cond_ty = try self.inferExpr(venv, when_expr.cond);
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    if (!self.patPossibleAtTy(self.output.getPat(branch.pat), cond_ty)) continue;
                    const pat_result = try self.inferPat(venv, branch.pat);
                    defer self.allocator.free(pat_result.additions);
                    try self.unify(cond_ty, pat_result.ty);
                    const body_env = try self.extendEnvMany(venv, pat_result.additions);
                    defer self.allocator.free(body_env);
                    const body_ty = try self.inferExpr(body_env, branch.body);
                    if (self.typesCompatible(target_ty, body_ty)) {
                        try self.unify(target_ty, body_ty);
                    } else {
                        try self.markExprRuntimeError(branch.body, target_ty);
                    }
                }
                break :blk target_ty;
            },
            .if_ => |if_expr| blk: {
                const cond_ty = try self.inferExpr(venv, if_expr.cond);
                try self.unify(cond_ty, try self.freshPrimitiveType(.bool));
                const then_ty = try self.inferExpr(venv, if_expr.then_body);
                const else_ty = try self.inferExpr(venv, if_expr.else_body);
                if (self.typesCompatible(target_ty, then_ty)) {
                    try self.unify(target_ty, then_ty);
                } else {
                    try self.markExprRuntimeError(if_expr.then_body, target_ty);
                }
                if (self.typesCompatible(target_ty, else_ty)) {
                    try self.unify(target_ty, else_ty);
                } else {
                    try self.markExprRuntimeError(if_expr.else_body, target_ty);
                }
                break :blk target_ty;
            },
            .block => |block| blk: {
                var block_env = try self.cloneEnv(venv);
                for (self.output.sliceStmtSpan(block.stmts)) |stmt_id| {
                    const next_env = try self.inferStmt(block_env, stmt_id);
                    if (block_env.len != 0) self.allocator.free(block_env);
                    block_env = next_env;
                }
                defer if (block_env.len != 0) self.allocator.free(block_env);
                break :blk try self.inferExpr(block_env, block.final_expr);
            },
            .tuple => |tuple| blk: {
                const items = self.output.sliceExprSpan(tuple);
                const elem_tys = try self.allocator.alloc(TypeVarId, items.len);
                defer self.allocator.free(elem_tys);
                for (items, 0..) |item, i| {
                    elem_tys[i] = try self.inferExpr(venv, item);
                }
                const wanted = try self.types.freshContent(.{ .tuple = try self.types.addTypeVarSpan(elem_tys) });
                try self.unifyTargetWithWantedPreservingNominal(target_ty, wanted);
                break :blk target_ty;
            },
            .tag_payload => |tag_payload| blk: {
                const union_ty = try self.inferExpr(venv, tag_payload.tag_union);
                const subject_ty = self.output.getExpr(tag_payload.tag_union).ty;
                try self.unify(union_ty, subject_ty);
                break :blk target_ty;
            },
            .tuple_access => |tuple_access| blk: {
                const tuple_ty = try self.inferExpr(venv, tuple_access.tuple);
                const subject_ty = self.output.getExpr(tuple_access.tuple).ty;
                try self.unify(tuple_ty, subject_ty);
                const subject_root = self.types.unlinkErasingNominal(subject_ty);
                const subject_node = self.types.getNode(subject_root);
                const elems = switch (subject_node) {
                    .content => |content| switch (content) {
                        .tuple => |span| self.types.sliceTypeVarSpan(span),
                        else => return debugPanic("lambdasolved.tuple_access invariant violated: subject is not a tuple", .{}),
                    },
                    else => return debugPanic("lambdasolved.tuple_access invariant violated: subject tuple type is unresolved", .{}),
                };
                if (tuple_access.elem_index >= elems.len) {
                    return debugPanic("lambdasolved.tuple_access invariant violated: tuple index out of bounds", .{});
                }
                try self.unify(target_ty, elems[tuple_access.elem_index]);
                break :blk target_ty;
            },
            .list => |list| blk: {
                const elem_ty = try self.types.freshUnbd();
                const wanted = try self.types.freshContent(.{ .list = elem_ty });
                try self.unifyTargetWithWantedPreservingNominal(target_ty, wanted);
                for (self.output.sliceExprSpan(list)) |item| {
                    const item_ty = try self.inferExpr(venv, item);
                    try self.unify(elem_ty, item_ty);
                }
                break :blk target_ty;
            },
            .return_ => |ret| blk: {
                const return_ty = self.current_return_ty orelse return debugPanic("lambdasolved.return invariant violated: return outside function", .{});
                const value_ty = try self.inferExpr(venv, ret);
                try self.unify(return_ty, value_ty);
                break :blk try self.types.freshUnbd();
            },
            .runtime_error => target_ty,
            .for_ => |for_expr| blk: {
                const pat_result = try self.inferPat(venv, for_expr.patt);
                defer self.allocator.free(pat_result.additions);
                const iterable_ty = try self.inferExpr(venv, for_expr.iterable);
                const wanted_iterable = try self.types.freshContent(.{ .list = pat_result.ty });
                try self.unify(iterable_ty, wanted_iterable);
                const body_env = try self.extendEnvMany(venv, pat_result.additions);
                defer self.allocator.free(body_env);
                try self.inferExprDiscard(body_env, for_expr.body);
                break :blk target_ty;
            },
        };

        try self.unify(target_ty, inferred);
        return target_ty;
    }

    fn runtimeErrorMessage(self: *Lowerer) std.mem.Allocator.Error!base.StringLiteral.Idx {
        if (self.runtime_error_msg) |msg| return msg;
        const msg = try self.input.strings.insert(self.allocator, "type mismatch in branch");
        self.runtime_error_msg = msg;
        return msg;
    }

    fn markExprRuntimeError(self: *Lowerer, expr_id: ast.ExprId, expected_ty: TypeVarId) std.mem.Allocator.Error!void {
        const msg = try self.runtimeErrorMessage();
        // runtime_error is emitted by design when type checking found an error
        const idx = @intFromEnum(expr_id);
        var expr = self.output.exprs.items[idx];
        expr.ty = expected_ty;
        expr.data = .{ .runtime_error = msg };
        self.output.exprs.items[idx] = expr;
    }

    fn patPossibleAtTy(self: *Lowerer, pat: ast.Pat, ty: TypeVarId) bool {
        return switch (pat.data) {
            .var_, .bool_lit => true,
            .tag => |tag| blk: {
                const arg_tys = self.tagArgTypesByName(ty, tag.name) orelse
                    break :blk false;
                const arg_pats = self.output.slicePatSpan(tag.args);
                if (arg_tys.len != arg_pats.len) {
                    debugPanic("lambdasolved.patPossibleAtTy tag arg count mismatch", .{});
                }
                for (arg_pats, arg_tys) |arg_pat_id, arg_ty| {
                    if (!self.patPossibleAtTy(self.output.getPat(arg_pat_id), arg_ty)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
        };
    }

    fn typesCompatible(self: *Lowerer, left: TypeVarId, right: TypeVarId) bool {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.typesCompatibleRec(left, right, &visited);
    }

    fn typesCompatibleRec(self: *Lowerer, left: TypeVarId, right: TypeVarId, visited: *std.ArrayList(TypePair)) bool {
        const l = self.types.unlink(left);
        const r = self.types.unlink(right);
        if (l == r) return true;
        for (visited.items) |pair| {
            if (pair.left == l and pair.right == r) return true;
        }
        visited.append(self.allocator, .{ .left = l, .right = r }) catch return true;

        const left_node = self.types.getNode(l);
        const right_node = self.types.getNode(r);

        switch (left_node) {
            .unbd => return true,
            .for_a, .flex_for_a => return debugPanic("lambdasolved.compatibility generalized type without instantiation", .{}),
            .link => unreachable,
            .nominal => |left_nominal| switch (right_node) {
                .unbd => return true,
                .for_a, .flex_for_a => return debugPanic("lambdasolved.compatibility generalized type without instantiation", .{}),
                .link => unreachable,
                .nominal => |right_nominal| {
                    if (left_nominal.module_idx != right_nominal.module_idx) return false;
                    if (left_nominal.ident != right_nominal.ident) return false;
                    if (left_nominal.is_opaque != right_nominal.is_opaque) return false;

                    const left_args = self.types.sliceTypeVarSpan(left_nominal.args);
                    const right_args = self.types.sliceTypeVarSpan(right_nominal.args);
                    if (left_args.len != right_args.len) return false;
                    for (left_args, 0..) |left_arg, i| {
                        if (!self.typesCompatibleRec(left_arg, right_args[i], visited)) return false;
                    }

                    return self.typesCompatibleRec(left_nominal.backing, right_nominal.backing, visited);
                },
                .content => |right_content| return self.nominalBackingCompatible(left_nominal.backing, right_content, visited),
            },
            .content => |left_content| switch (right_node) {
                .unbd => return true,
                .for_a, .flex_for_a => return debugPanic("lambdasolved.compatibility generalized type without instantiation", .{}),
                .link => unreachable,
                .nominal => |right_nominal| return self.nominalBackingCompatible(right_nominal.backing, left_content, visited),
                .content => |right_content| return self.contentsCompatible(left_content, right_content, visited),
            },
        }
    }

    fn nominalBackingCompatible(
        self: *Lowerer,
        backing_ty: TypeVarId,
        content: type_mod.Content,
        visited: *std.ArrayList(TypePair),
    ) bool {
        const backing_root = self.types.unlinkErasingNominal(backing_ty);
        return switch (self.types.getNode(backing_root)) {
            .content => |backing_content| self.contentsCompatible(backing_content, content, visited),
            .unbd => true,
            .for_a, .flex_for_a => debugPanic("lambdasolved.compatibility generalized type without instantiation", .{}),
            .link, .nominal => unreachable,
        };
    }

    fn contentsCompatible(self: *Lowerer, left: type_mod.Content, right: type_mod.Content, visited: *std.ArrayList(TypePair)) bool {
        if (@as(std.meta.Tag(type_mod.Content), left) != @as(std.meta.Tag(type_mod.Content), right)) {
            return false;
        }

        return switch (left) {
            .primitive => |prim| prim == right.primitive or prim == .dec or right.primitive == .dec,
            .func => |func| {
                const left_args = self.types.sliceTypeVarSpan(func.args);
                const right_args = self.types.sliceTypeVarSpan(right.func.args);
                if (left_args.len != right_args.len) return false;
                for (left_args, right_args) |left_arg, right_arg| {
                    if (!self.typesCompatibleRec(left_arg, right_arg, visited)) return false;
                }
                return self.typesCompatibleRec(func.ret, right.func.ret, visited) and
                    self.typesCompatibleRec(func.lset, right.func.lset, visited);
            },
            .list => |elem| self.typesCompatibleRec(elem, right.list, visited),
            .box => |elem| self.typesCompatibleRec(elem, right.box, visited),
            .tuple => |elems| {
                const left_elems = self.types.sliceTypeVarSpan(elems);
                const right_elems = self.types.sliceTypeVarSpan(right.tuple);
                if (left_elems.len != right_elems.len) return false;
                for (left_elems, 0..) |left_elem, i| {
                    if (!self.typesCompatibleRec(left_elem, right_elems[i], visited)) return false;
                }
                return true;
            },
            .record => |record| {
                const left_fields = self.types.sliceFields(record.fields);
                const right_fields = self.types.sliceFields(right.record.fields);
                for (left_fields) |left_field| {
                    if (findFieldByName(right_fields, left_field.name)) |right_field| {
                        if (!self.typesCompatibleRec(left_field.ty, right_field.ty, visited)) return false;
                    }
                }
                for (right_fields) |right_field| {
                    if (findFieldByName(left_fields, right_field.name)) |left_field| {
                        if (!self.typesCompatibleRec(right_field.ty, left_field.ty, visited)) return false;
                    }
                }
                return true;
            },
            .tag_union => |tag_union| {
                const left_tags = self.types.sliceTags(tag_union.tags);
                const right_tags = self.types.sliceTags(right.tag_union.tags);
                for (left_tags) |left_tag| {
                    const right_tag = findTagByName(right_tags, left_tag.name) orelse return false;
                    const left_args = self.types.sliceTypeVarSpan(left_tag.args);
                    const right_args = self.types.sliceTypeVarSpan(right_tag.args);
                    if (left_args.len != right_args.len) return false;
                    for (left_args, 0..) |left_arg, i| {
                        if (!self.typesCompatibleRec(left_arg, right_args[i], visited)) return false;
                    }
                }
                for (right_tags) |right_tag| {
                    const left_tag = findTagByName(left_tags, right_tag.name) orelse return false;
                    const left_args = self.types.sliceTypeVarSpan(left_tag.args);
                    const right_args = self.types.sliceTypeVarSpan(right_tag.args);
                    if (left_args.len != right_args.len) return false;
                    for (left_args, 0..) |left_arg, i| {
                        if (!self.typesCompatibleRec(left_arg, right_args[i], visited)) return false;
                    }
                }
                return true;
            },
            .lambda_set => |lambda_set| {
                const left_lambdas = self.types.sliceLambdas(lambda_set);
                const right_lambdas = self.types.sliceLambdas(right.lambda_set);
                for (left_lambdas) |left_lambda| {
                    if (findLambdaBySymbol(right_lambdas, left_lambda.symbol)) |right_lambda| {
                        if (!self.capturesCompatible(left_lambda.captures, right_lambda.captures, visited)) return false;
                    }
                }
                for (right_lambdas) |right_lambda| {
                    if (findLambdaBySymbol(left_lambdas, right_lambda.symbol)) |left_lambda| {
                        if (!self.capturesCompatible(left_lambda.captures, right_lambda.captures, visited)) return false;
                    }
                }
                return true;
            },
        };
    }

    fn capturesCompatible(self: *Lowerer, left_span: type_mod.Span(type_mod.Capture), right_span: type_mod.Span(type_mod.Capture), visited: *std.ArrayList(TypePair)) bool {
        const left = self.types.sliceCaptures(left_span);
        const right = self.types.sliceCaptures(right_span);
        if (left.len != right.len) return false;
        for (left) |left_capture| {
            const right_capture = findCaptureBySymbol(right, left_capture.symbol) orelse return false;
            if (!self.typesCompatibleRec(left_capture.ty, right_capture.ty, visited)) return false;
        }
        return true;
    }

    fn unifyTargetWithWantedPreservingNominal(
        self: *Lowerer,
        target_ty: TypeVarId,
        wanted_ty: TypeVarId,
    ) std.mem.Allocator.Error!void {
        const target_root = self.types.unlinkPreservingNominal(target_ty);
        return switch (self.types.getNode(target_root)) {
            .nominal => |nominal| try self.unify(nominal.backing, wanted_ty),
            else => try self.unify(target_ty, wanted_ty),
        };
    }

    fn inferStmt(self: *Lowerer, venv: []const EnvEntry, stmt_id: ast.StmtId) std.mem.Allocator.Error![]EnvEntry {
        const stmt = self.output.getStmt(stmt_id);
        return switch (stmt) {
            .decl => |decl| blk: {
                const body_ty = try self.inferExpr(venv, decl.body);
                try self.unify(body_ty, decl.bind.ty);
                try self.generalize(venv, decl.bind.ty);
                break :blk try self.extendEnvOne(venv, .{ .symbol = decl.bind.symbol, .ty = decl.bind.ty });
            },
            .var_decl => |decl| blk: {
                const body_ty = try self.inferExpr(venv, decl.body);
                try self.unify(body_ty, decl.bind.ty);
                break :blk try self.extendEnvOne(venv, .{ .symbol = decl.bind.symbol, .ty = decl.bind.ty });
            },
            .reassign => |reassign| blk: {
                const target_ty = self.lookupEnv(venv, reassign.target) orelse return debugPanic("lambdasolved.inferStmt missing reassign target", .{});
                const body_ty = try self.inferExpr(venv, reassign.body);
                try self.unify(target_ty, body_ty);
                break :blk try self.cloneEnv(venv);
            },
            .expr => |expr_id| blk: {
                try self.inferExprDiscard(venv, expr_id);
                break :blk try self.cloneEnv(venv);
            },
            .debug => |expr_id| blk: {
                try self.inferExprDiscard(venv, expr_id);
                break :blk try self.cloneEnv(venv);
            },
            .expect => |expr_id| blk: {
                try self.inferExprDiscard(venv, expr_id);
                break :blk try self.cloneEnv(venv);
            },
            .crash => try self.cloneEnv(venv),
            .return_ => |expr_id| blk: {
                const return_ty = self.current_return_ty orelse return debugPanic("lambdasolved.return_stmt invariant violated: return outside function", .{});
                const value_ty = try self.inferExpr(venv, expr_id);
                try self.unify(return_ty, value_ty);
                break :blk try self.cloneEnv(venv);
            },
            .break_ => try self.cloneEnv(venv),
            .for_ => |for_stmt| blk: {
                const pat_result = try self.inferPat(venv, for_stmt.patt);
                defer self.allocator.free(pat_result.additions);
                const iterable_ty = try self.inferExpr(venv, for_stmt.iterable);
                const wanted_iterable = try self.types.freshContent(.{ .list = pat_result.ty });
                try self.unify(iterable_ty, wanted_iterable);
                const body_env = try self.extendEnvMany(venv, pat_result.additions);
                defer self.allocator.free(body_env);
                try self.inferExprDiscard(body_env, for_stmt.body);
                break :blk try self.cloneEnv(venv);
            },
            .while_ => |while_stmt| blk: {
                const cond_ty = try self.inferExpr(venv, while_stmt.cond);
                const bool_ty = try self.types.freshContent(.{ .primitive = .bool });
                try self.unify(cond_ty, bool_ty);
                try self.inferExprDiscard(venv, while_stmt.body);
                break :blk try self.cloneEnv(venv);
            },
        };
    }

    fn inferExprDiscard(self: *Lowerer, venv: []const EnvEntry, expr_id: ast.ExprId) std.mem.Allocator.Error!void {
        _ = try self.inferExpr(venv, expr_id);
    }

    const InferPatResult = struct {
        additions: []EnvEntry,
        ty: TypeVarId,
    };

    fn inferPat(self: *Lowerer, _: []const EnvEntry, pat_id: ast.PatId) std.mem.Allocator.Error!InferPatResult {
        const pat = self.output.getPat(pat_id);
        return switch (pat.data) {
            .var_ => |symbol| blk: {
                const adds = try self.allocator.alloc(EnvEntry, if (symbol.isNone()) 0 else 1);
                if (!symbol.isNone()) {
                    adds[0] = .{ .symbol = symbol, .ty = pat.ty };
                }
                break :blk .{ .additions = adds, .ty = pat.ty };
            },
            .bool_lit => .{
                .additions = try self.allocator.alloc(EnvEntry, 0),
                .ty = pat.ty,
            },
            .tag => |tag| blk: {
                const arg_pats = self.output.slicePatSpan(tag.args);
                if (try self.isPrimitiveBoolType(pat.ty)) {
                    if (arg_pats.len != 0) {
                        return debugPanic("lambdasolved.bool pattern invariant violated: bool tags cannot carry arguments", .{});
                    }
                    break :blk .{
                        .additions = try self.allocator.alloc(EnvEntry, 0),
                        .ty = pat.ty,
                    };
                }
                var additions_acc = std.ArrayList(EnvEntry).empty;
                errdefer additions_acc.deinit(self.allocator);
                const arg_tys = try self.allocator.alloc(TypeVarId, arg_pats.len);
                defer self.allocator.free(arg_tys);
                for (arg_pats, 0..) |arg_pat_id, i| {
                    const inferred = try self.inferPat(&.{}, arg_pat_id);
                    defer self.allocator.free(inferred.additions);
                    arg_tys[i] = inferred.ty;
                    try additions_acc.appendSlice(self.allocator, inferred.additions);
                }
                const tag_ty = try self.types.freshContent(.{ .tag_union = .{
                    .tags = try self.types.addTags(&.{.{ .name = tag.name, .args = try self.types.addTypeVarSpan(arg_tys) }}),
                } });
                try self.unify(pat.ty, tag_ty);
                const additions = try additions_acc.toOwnedSlice(self.allocator);
                break :blk .{ .additions = additions, .ty = pat.ty };
            },
        };
    }

    fn debugSymbolCounts(
        self: *Lowerer,
        def_id: ast.DefId,
        symbol: Symbol,
        use_count: *usize,
        bind_count: *usize,
        pat_bind_count: *usize,
        capture_count: *usize,
        reassign_target_count: *usize,
        first_use_expr: *?ast.ExprId,
    ) void {
        const def = self.output.getDef(def_id);
        if (def.bind.symbol == symbol) bind_count.* += 1;
        switch (def.value) {
            .fn_ => |fn_def| {
                for (self.output.sliceTypedSymbolSpan(fn_def.args)) |arg| {
                    if (arg.symbol == symbol) bind_count.* += 1;
                }
                const capture_member = self.types.maybeLambdaMember(def.bind.ty, def.bind.symbol);
                if (capture_member) |member| {
                    for (member.captures) |capture| {
                        if (capture.symbol == symbol) capture_count.* += 1;
                    }
                }
                self.debugCountExprSymbol(
                    fn_def.body,
                    symbol,
                    use_count,
                    bind_count,
                    pat_bind_count,
                    reassign_target_count,
                    first_use_expr,
                );
            },
            .hosted_fn => |hosted_fn| {
                if (hosted_fn.bind.symbol == symbol) bind_count.* += 1;
                const args = self.output.sliceTypedSymbolSpan(hosted_fn.args);
                for (args) |arg| {
                    if (arg.symbol == symbol) bind_count.* += 1;
                }
            },
            .val => |expr_id| self.debugCountExprSymbol(
                expr_id,
                symbol,
                use_count,
                bind_count,
                pat_bind_count,
                reassign_target_count,
                first_use_expr,
            ),
            .run => |run_def| self.debugCountExprSymbol(
                run_def.body,
                symbol,
                use_count,
                bind_count,
                pat_bind_count,
                reassign_target_count,
                first_use_expr,
            ),
        }
    }

    fn debugCountExprSymbol(
        self: *Lowerer,
        expr_id: ast.ExprId,
        symbol: Symbol,
        use_count: *usize,
        bind_count: *usize,
        pat_bind_count: *usize,
        reassign_target_count: *usize,
        first_use_expr: *?ast.ExprId,
    ) void {
        const expr = self.output.getExpr(expr_id);
        switch (expr.data) {
            .var_ => |expr_symbol| {
                if (expr_symbol == symbol) use_count.* += 1;
                if (expr_symbol == symbol and first_use_expr.* == null) first_use_expr.* = expr_id;
            },
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .runtime_error,
            => {},
            .tag => |tag| for (self.output.sliceExprSpan(tag.args)) |arg| self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .record => |fields| {
                for (self.output.sliceFieldExprSpan(fields)) |field| {
                    self.debugCountExprSymbol(field.value, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                }
            },
            .access => |access| self.debugCountExprSymbol(access.record, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .structural_eq => |eq| {
                self.debugCountExprSymbol(eq.lhs, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(eq.rhs, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .method_eq => |eq| {
                self.debugCountExprSymbol(eq.lhs, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(eq.rhs, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .dispatch_call => |method_call| {
                self.debugCountExprSymbol(method_call.receiver, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                for (self.output.sliceExprSpan(method_call.args)) |arg| {
                    self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                }
            },
            .type_dispatch_call => |method_call| {
                for (self.output.sliceExprSpan(method_call.args)) |arg| {
                    self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                }
            },
            .let_ => |let_expr| {
                if (let_expr.bind.symbol == symbol) bind_count.* += 1;
                self.debugCountExprSymbol(let_expr.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(let_expr.rest, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .call => |call| {
                self.debugCountExprSymbol(call.func, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                for (self.output.sliceExprSpan(call.args)) |arg| {
                    self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                }
            },
            .inspect => |value| self.debugCountExprSymbol(value, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .low_level => |ll| for (self.output.sliceExprSpan(ll.args)) |arg| self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .when => |when_expr| {
                self.debugCountExprSymbol(when_expr.cond, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    self.debugCountPatSymbol(branch.pat, symbol, pat_bind_count);
                    self.debugCountExprSymbol(branch.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                }
            },
            .if_ => |if_expr| {
                self.debugCountExprSymbol(if_expr.cond, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(if_expr.then_body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(if_expr.else_body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .block => |block| {
                for (self.output.sliceStmtSpan(block.stmts)) |stmt_id| {
                    self.debugCountStmtSymbol(stmt_id, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                }
                self.debugCountExprSymbol(block.final_expr, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .tuple => |tuple| for (self.output.sliceExprSpan(tuple)) |arg| self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .tag_payload => |tag_payload| self.debugCountExprSymbol(tag_payload.tag_union, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .tuple_access => |tuple_access| self.debugCountExprSymbol(tuple_access.tuple, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .list => |list| for (self.output.sliceExprSpan(list)) |arg| self.debugCountExprSymbol(arg, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .return_ => |ret| self.debugCountExprSymbol(ret, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .for_ => |for_expr| {
                self.debugCountPatSymbol(for_expr.patt, symbol, pat_bind_count);
                self.debugCountExprSymbol(for_expr.iterable, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(for_expr.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
        }
    }

    fn debugCountStmtSymbol(
        self: *Lowerer,
        stmt_id: ast.StmtId,
        symbol: Symbol,
        use_count: *usize,
        bind_count: *usize,
        pat_bind_count: *usize,
        reassign_target_count: *usize,
        first_use_expr: *?ast.ExprId,
    ) void {
        const stmt = self.output.getStmt(stmt_id);
        switch (stmt) {
            .decl => |decl| {
                if (decl.bind.symbol == symbol) bind_count.* += 1;
                self.debugCountExprSymbol(decl.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .var_decl => |decl| {
                if (decl.bind.symbol == symbol) bind_count.* += 1;
                self.debugCountExprSymbol(decl.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .reassign => |reassign| {
                if (reassign.target == symbol) reassign_target_count.* += 1;
                self.debugCountExprSymbol(reassign.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .expr => |expr_id| self.debugCountExprSymbol(expr_id, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .debug => |expr_id| self.debugCountExprSymbol(expr_id, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .expect => |expr_id| self.debugCountExprSymbol(expr_id, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .crash => {},
            .return_ => |expr_id| self.debugCountExprSymbol(expr_id, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr),
            .break_ => {},
            .for_ => |for_stmt| {
                self.debugCountPatSymbol(for_stmt.patt, symbol, pat_bind_count);
                self.debugCountExprSymbol(for_stmt.iterable, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(for_stmt.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
            .while_ => |while_stmt| {
                self.debugCountExprSymbol(while_stmt.cond, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
                self.debugCountExprSymbol(while_stmt.body, symbol, use_count, bind_count, pat_bind_count, reassign_target_count, first_use_expr);
            },
        }
    }

    fn debugCountPatSymbol(
        self: *Lowerer,
        pat_id: ast.PatId,
        symbol: Symbol,
        pat_bind_count: *usize,
    ) void {
        const pat = self.output.getPat(pat_id);
        switch (pat.data) {
            .var_ => |pat_symbol| {
                if (pat_symbol == symbol) pat_bind_count.* += 1;
            },
            .bool_lit => {},
            .tag => |tag| for (self.output.slicePatSpan(tag.args)) |arg| self.debugCountPatSymbol(arg, symbol, pat_bind_count),
        }
    }

    fn freshPrimitiveType(self: *Lowerer, prim: type_mod.Prim) std.mem.Allocator.Error!TypeVarId {
        return try self.types.freshContent(.{ .primitive = prim });
    }

    fn absDiffResultType(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const root = self.types.unlinkPreservingNominal(ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.absDiffResultType(nominal.backing),
            .content => |content| switch (content) {
                .primitive => |prim| switch (prim) {
                    .i8 => try self.freshPrimitiveType(.u8),
                    .i16 => try self.freshPrimitiveType(.u16),
                    .i32 => try self.freshPrimitiveType(.u32),
                    .i64 => try self.freshPrimitiveType(.u64),
                    .i128 => try self.freshPrimitiveType(.u128),
                    else => ty,
                },
                else => ty,
            },
            .unbd, .for_a, .flex_for_a => ty,
            .link => unreachable,
        };
    }

    fn isPrimitiveBoolType(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!bool {
        const root = self.types.unlink(ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.isPrimitiveBoolType(nominal.backing),
            .content => |content| switch (content) {
                .primitive => |prim| prim == .bool,
                else => false,
            },
            .unbd,
            .for_a,
            .flex_for_a,
            => false,
            .link => unreachable,
        };
    }

    fn exprIsFlexibleNumericLiteral(self: *Lowerer, expr_id: ast.ExprId) bool {
        return switch (self.output.getExpr(expr_id).data) {
            .int_lit, .dec_lit => true,
            else => false,
        };
    }

    fn propagateErasure(self: *Lowerer) std.mem.Allocator.Error!void {
        var top_env: []EnvEntry = &.{};
        defer if (top_env.len != 0) self.allocator.free(top_env);

        for (self.root_defs.items) |def_id| {
            const def = &self.output.defs.items[@intFromEnum(def_id)];
            switch (def.value) {
                .fn_ => |fn_def| {
                    var fn_env = top_env;
                    const fn_args = self.output.sliceTypedSymbolSpan(fn_def.args);
                    for (fn_args) |arg| {
                        const next_env = try self.extendEnvOne(fn_env, .{
                            .symbol = arg.symbol,
                            .ty = arg.ty,
                        });
                        if (fn_env.len != 0 and fn_env.ptr != top_env.ptr) self.allocator.free(fn_env);
                        fn_env = next_env;
                    }
                    defer if (fn_env.len != 0 and fn_env.ptr != top_env.ptr) self.allocator.free(fn_env);
                    const member = self.types.requireLambdaMember(def.bind.ty, def.bind.symbol);
                    for (member.captures) |capture| {
                        const next_env = try self.extendEnvOne(fn_env, .{
                            .symbol = capture.symbol,
                            .ty = capture.ty,
                        });
                        if (fn_env.len != 0 and fn_env.ptr != top_env.ptr) self.allocator.free(fn_env);
                        fn_env = next_env;
                    }
                    try self.propagateExprErasure(fn_def.body, fn_env);
                    def.bind.ty = try self.rewriteFunctionRetType(def.bind.ty, self.output.getExpr(fn_def.body).ty);
                },
                .hosted_fn => {},
                .val => |expr_id| {
                    try self.propagateExprErasure(expr_id, top_env);
                    def.bind.ty = self.output.getExpr(expr_id).ty;
                },
                .run => |run_def| {
                    try self.propagateExprErasure(run_def.body, top_env);
                    def.bind.ty = self.output.getExpr(run_def.body).ty;
                },
            }

            const next_top = try self.extendEnvOne(top_env, .{
                .symbol = def.bind.symbol,
                .ty = def.bind.ty,
            });
            if (top_env.len != 0) self.allocator.free(top_env);
            top_env = next_top;
        }
    }

    fn propagateExprErasure(
        self: *Lowerer,
        expr_id: ast.ExprId,
        venv: []const EnvEntry,
    ) std.mem.Allocator.Error!void {
        const expr = &self.output.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .var_ => |symbol| {
                if (self.lookupEnv(venv, symbol)) |env_ty| {
                    expr.ty = try self.overlayErasureTemplate(env_ty, expr.ty);
                }
            },
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
                const arg_exprs = self.output.sliceExprSpan(tag.args);
                for (arg_exprs) |arg| {
                    try self.propagateExprErasure(arg, venv);
                }
                const arg_tys = try self.allocator.alloc(TypeVarId, arg_exprs.len);
                defer self.allocator.free(arg_tys);
                for (arg_exprs, 0..) |arg, i| {
                    arg_tys[i] = self.output.getExpr(arg).ty;
                }
                expr.ty = try self.rewriteTagExprType(expr.ty, tag.name, arg_tys);
            },
            .record => |fields| blk: {
                const field_values = self.output.sliceFieldExprSpan(fields);
                for (field_values) |field| {
                    try self.propagateExprErasure(field.value, venv);
                }

                const record_id = self.types.unlinkPreservingNominal(expr.ty);
                const record_node = self.types.getNode(record_id);
                const record = switch (record_node) {
                    .content => |content| switch (content) {
                        .record => |record| record,
                        else => break :blk,
                    },
                    else => break :blk,
                };

                const record_fields = self.types.sliceFields(record.fields);
                if (record_fields.len != field_values.len) {
                    return debugPanic("lambdasolved.propagateExprErasure record field arity mismatch", .{});
                }
                for (record_fields, field_values) |record_field, field_value| {
                    if (record_field.name != field_value.name or record_field.ty != self.output.getExpr(field_value.value).ty) {
                        return debugPanic("lambdasolved.propagateExprErasure record field type mismatch", .{});
                    }
                }
            },
            .access => |access| try self.propagateExprErasure(access.record, venv),
            .structural_eq => |eq| {
                try self.propagateExprErasure(eq.lhs, venv);
                try self.propagateExprErasure(eq.rhs, venv);
            },
            .method_eq => |eq| {
                try self.propagateExprErasure(eq.lhs, venv);
                try self.propagateExprErasure(eq.rhs, venv);
            },
            .dispatch_call => |method_call| {
                try self.propagateExprErasure(method_call.receiver, venv);
                for (self.output.sliceExprSpan(method_call.args)) |arg| {
                    try self.propagateExprErasure(arg, venv);
                }
                expr.ty = self.types.fnShape(method_call.dispatch_constraint_ty).ret;
            },
            .type_dispatch_call => |method_call| {
                for (self.output.sliceExprSpan(method_call.args)) |arg| {
                    try self.propagateExprErasure(arg, venv);
                }
                expr.ty = self.types.fnShape(method_call.dispatch_constraint_ty).ret;
            },
            .let_ => |*let_expr| {
                try self.propagateExprErasure(let_expr.body, venv);
                const rest_env = try self.extendEnvOne(venv, .{
                    .symbol = let_expr.bind.symbol,
                    .ty = let_expr.bind.ty,
                });
                defer if (rest_env.len != 0) self.allocator.free(rest_env);
                try self.propagateExprErasure(let_expr.rest, rest_env);
            },
            .call => |call| {
                try self.propagateExprErasure(call.func, venv);
                for (self.output.sliceExprSpan(call.args)) |arg| {
                    try self.propagateExprErasure(arg, venv);
                }
                const func_expr = self.output.getExpr(call.func);
                expr.ty = self.types.fnShape(call.call_constraint_ty).ret;
                const boundary_symbol = self.maybeExactCallableSymbolForExpr(call.func) orelse blk_boundary: {
                    if (func_expr.data != .var_) break :blk_boundary null;
                    break :blk_boundary func_expr.data.var_;
                };
                if (boundary_symbol) |symbol| {
                    if (self.boxBoundaryBuiltinOpForSymbol(symbol)) |op| {
                        const call_args = self.output.sliceExprSpan(call.args);
                        if (call_args.len != 1) {
                            debugPanic("lambdasolved.propagateExprErasure box boundary builtin expected exactly one arg", .{});
                        }
                        const arg_ty = self.output.getExpr(call_args[0]).ty;
                        switch (op) {
                            .box_box => {
                                const boxed_arg_ty = try self.eraseBoundaryCallableType(arg_ty);
                                expr.ty = try self.types.freshContent(.{
                                    .box = boxed_arg_ty,
                                });
                            },
                            .box_unbox => {
                                const boxed_elem_ty = self.boxedPayloadType(arg_ty) orelse
                                    return debugPanic("lambdasolved.propagateExprErasure box_unbox call expected boxed arg", .{});
                                expr.ty = try self.eraseBoundaryCallableType(boxed_elem_ty);
                            },
                            else => unreachable,
                        }
                    }
                }
            },
            .inspect => |value| try self.propagateExprErasure(value, venv),
            .low_level => |ll| {
                const args = self.output.sliceExprSpan(ll.args);
                for (args) |arg| {
                    try self.propagateExprErasure(arg, venv);
                }
                switch (ll.op) {
                    .box_box => {
                        if (args.len != 1) {
                            return debugPanic("lambdasolved.propagateExprErasure box_box expected one arg", .{});
                        }

                        const arg_ty = self.output.getExpr(args[0]).ty;
                        const boxed_arg_ty = try self.eraseBoundaryCallableType(arg_ty);
                        expr.ty = try self.types.freshContent(.{
                            .box = boxed_arg_ty,
                        });
                    },
                    .box_unbox => {
                        if (args.len != 1) {
                            return debugPanic("lambdasolved.propagateExprErasure box_unbox expected one arg", .{});
                        }

                        const arg_ty = self.output.getExpr(args[0]).ty;
                        const boxed_elem_ty = self.boxedPayloadType(arg_ty) orelse
                            return debugPanic("lambdasolved.propagateExprErasure box_unbox expected boxed arg", .{});
                        expr.ty = try self.eraseBoundaryCallableType(boxed_elem_ty);
                    },
                    else => {},
                }
            },
            .when => |when_expr| {
                try self.propagateExprErasure(when_expr.cond, venv);
                const cond_expr = &self.output.exprs.items[@intFromEnum(when_expr.cond)];
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    try self.propagatePatErasure(branch.pat);
                    const branch_pat = self.output.getPat(branch.pat);
                    switch (branch_pat.data) {
                        .var_ => |symbol| if (symbol.isNone()) continue,
                        else => {},
                    }
                    cond_expr.ty = try self.overlayErasureTemplate(branch_pat.ty, cond_expr.ty);
                }
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    const branch_pat = &self.output.pats.items[@intFromEnum(branch.pat)];
                    branch_pat.ty = try self.overlayErasureTemplate(cond_expr.ty, branch_pat.ty);
                    try self.propagatePatternTagPayloadTypes(branch.pat);
                    const additions = try self.patternEnvEntries(branch.pat);
                    defer self.allocator.free(additions);
                    const body_env = try self.extendEnvMany(venv, additions);
                    defer if (body_env.len != 0) self.allocator.free(body_env);
                    try self.propagateExprErasure(branch.body, body_env);
                }
            },
            .if_ => |if_expr| {
                try self.propagateExprErasure(if_expr.cond, venv);
                try self.propagateExprErasure(if_expr.then_body, venv);
                try self.propagateExprErasure(if_expr.else_body, venv);
            },
            .block => |block| {
                var block_env = try self.cloneEnv(venv);
                defer if (block_env.len != 0) self.allocator.free(block_env);
                for (self.output.sliceStmtSpan(block.stmts)) |stmt_id| {
                    const next_env = try self.propagateStmtErasure(block_env, stmt_id);
                    if (block_env.len != 0) self.allocator.free(block_env);
                    block_env = next_env;
                }
                try self.propagateExprErasure(block.final_expr, block_env);
            },
            .tuple => |tuple| blk: {
                const elem_values = self.output.sliceExprSpan(tuple);
                for (elem_values) |item| {
                    try self.propagateExprErasure(item, venv);
                }

                const tuple_id = self.types.unlinkPreservingNominal(expr.ty);
                const tuple_node = self.types.getNode(tuple_id);
                const tuple_span = switch (tuple_node) {
                    .content => |content| switch (content) {
                        .tuple => |tuple_span| tuple_span,
                        else => break :blk,
                    },
                    else => break :blk,
                };
                const tuple_elems = self.types.sliceTypeVarSpan(tuple_span);
                if (tuple_elems.len != elem_values.len) {
                    return debugPanic("lambdasolved.propagateExprErasure tuple arity mismatch", .{});
                }
                for (tuple_elems, elem_values) |tuple_elem, elem_value| {
                    if (tuple_elem != self.output.getExpr(elem_value).ty) {
                        return debugPanic("lambdasolved.propagateExprErasure tuple element type mismatch", .{});
                    }
                }
            },
            .tag_payload => |tag_payload| try self.propagateExprErasure(tag_payload.tag_union, venv),
            .tuple_access => |tuple_access| try self.propagateExprErasure(tuple_access.tuple, venv),
            .list => |list| for (self.output.sliceExprSpan(list)) |item| try self.propagateExprErasure(item, venv),
            .return_ => |ret| try self.propagateExprErasure(ret, venv),
            .for_ => |for_expr| {
                try self.propagateExprErasure(for_expr.iterable, venv);
                const additions = try self.patternEnvEntries(for_expr.patt);
                defer self.allocator.free(additions);
                const body_env = try self.extendEnvMany(venv, additions);
                defer if (body_env.len != 0) self.allocator.free(body_env);
                try self.propagateExprErasure(for_expr.body, body_env);
            },
        }
    }

    fn propagatePatErasure(self: *Lowerer, pat_id: ast.PatId) std.mem.Allocator.Error!void {
        const pat = &self.output.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bool_lit => {},
            .var_ => |symbol| {
                if (symbol.isNone()) {
                    pat.ty = try self.freshPrimitiveType(.erased);
                }
            },
            .tag => |tag| {
                const arg_pat_ids = self.output.slicePatSpan(tag.args);
                for (arg_pat_ids) |arg_pat_id| {
                    try self.propagatePatErasure(arg_pat_id);
                }
                const arg_tys = try self.allocator.alloc(TypeVarId, arg_pat_ids.len);
                defer self.allocator.free(arg_tys);
                for (arg_pat_ids, 0..) |arg_pat_id, i| {
                    arg_tys[i] = self.output.getPat(arg_pat_id).ty;
                }
                pat.ty = try self.rewriteTagExprType(pat.ty, tag.name, arg_tys);
            },
        }
    }

    fn propagatePatternTagPayloadTypes(self: *Lowerer, pat_id: ast.PatId) std.mem.Allocator.Error!void {
        const pat = &self.output.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .bool_lit, .var_ => {},
            .tag => |tag| {
                if (try self.isPrimitiveBoolType(pat.ty)) return;
                const arg_tys = self.tagArgTypesByName(pat.ty, tag.name) orelse
                    return debugPanic("lambdasolved.propagatePatternTagPayloadTypes missing tag payload types", .{});
                const arg_pats = self.output.slicePatSpan(tag.args);
                if (arg_tys.len != arg_pats.len) {
                    return debugPanic("lambdasolved.propagatePatternTagPayloadTypes tag arg count mismatch", .{});
                }
                for (arg_pats, arg_tys) |arg_pat_id, arg_ty| {
                    self.output.pats.items[@intFromEnum(arg_pat_id)].ty = arg_ty;
                    try self.propagatePatternTagPayloadTypes(arg_pat_id);
                }
            },
        }
    }

    fn tagArgTypesByName(self: *Lowerer, ty: TypeVarId, tag_name: base.Ident.Idx) ?[]const TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| self.tagArgTypesByName(nominal.backing, tag_name),
            .content => |content| switch (content) {
                .tag_union => |tag_union| blk: {
                    for (self.types.sliceTags(tag_union.tags)) |tag| {
                        if (tag.name == tag_name) {
                            break :blk self.types.sliceTypeVarSpan(tag.args);
                        }
                    }
                    break :blk null;
                },
                else => null,
            },
            else => null,
        };
    }

    fn rewriteFunctionRetType(self: *Lowerer, ty: TypeVarId, new_ret: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| blk: {
                const rewritten = try self.rewriteFunctionRetType(nominal.backing, new_ret);
                if (rewritten == nominal.backing) break :blk id;
                var lowered = nominal;
                lowered.backing = rewritten;
                break :blk try self.types.fresh(.{ .nominal = lowered });
            },
            .content => |content| switch (content) {
                .func => |func| blk: {
                    if (func.ret == new_ret) break :blk id;
                    break :blk try self.types.freshContent(.{ .func = .{
                        .args = func.args,
                        .lset = func.lset,
                        .ret = new_ret,
                    } });
                },
                else => debugPanic("lambdasolved.rewriteFunctionRetType expected function", .{}),
            },
            else => debugPanic("lambdasolved.rewriteFunctionRetType expected function", .{}),
        };
    }

    fn rewriteFunctionLsetType(self: *Lowerer, ty: TypeVarId, new_lset: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| blk: {
                const rewritten = try self.rewriteFunctionLsetType(nominal.backing, new_lset);
                if (rewritten == nominal.backing) break :blk id;
                var lowered = nominal;
                lowered.backing = rewritten;
                break :blk try self.types.fresh(.{ .nominal = lowered });
            },
            .content => |content| switch (content) {
                .func => |func| blk: {
                    if (func.lset == new_lset) break :blk id;
                    break :blk try self.types.freshContent(.{ .func = .{
                        .args = func.args,
                        .lset = new_lset,
                        .ret = func.ret,
                    } });
                },
                else => debugPanic("lambdasolved.rewriteFunctionLsetType expected function", .{}),
            },
            else => debugPanic("lambdasolved.rewriteFunctionLsetType expected function", .{}),
        };
    }

    fn eraseBoundaryCallableType(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        return switch (self.types.maybeLambdaRepr(ty) orelse return ty) {
            .erased => ty,
            .lset => try self.rewriteFunctionLsetType(ty, try self.freshPrimitiveType(.erased)),
        };
    }

    fn rewriteTagExprType(
        self: *Lowerer,
        ty: TypeVarId,
        tag_name: base.Ident.Idx,
        arg_tys: []const TypeVarId,
    ) std.mem.Allocator.Error!TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| blk: {
                const rewritten_backing = try self.rewriteTagExprType(nominal.backing, tag_name, arg_tys);
                if (rewritten_backing == nominal.backing) break :blk id;
                var lowered = nominal;
                lowered.backing = rewritten_backing;
                break :blk try self.types.fresh(.{ .nominal = lowered });
            },
            .content => |content| switch (content) {
                .tag_union => |tag_union| blk: {
                    const tags = self.types.sliceTags(tag_union.tags);
                    const out_tags = try self.allocator.alloc(type_mod.Tag, tags.len);
                    defer self.allocator.free(out_tags);
                    var changed = false;
                    for (tags, 0..) |tag, i| {
                        if (tag.name == tag_name) {
                            const current_args = self.types.sliceTypeVarSpan(tag.args);
                            if (current_args.len != arg_tys.len) {
                                debugPanic("lambdasolved.rewriteTagExprType tag arg count mismatch", .{});
                            }
                            for (current_args, arg_tys) |current_arg, new_arg| {
                                if (current_arg != new_arg) changed = true;
                            }
                            out_tags[i] = .{
                                .name = tag.name,
                                .args = try self.types.addTypeVarSpan(arg_tys),
                            };
                        } else {
                            out_tags[i] = tag;
                        }
                    }
                    if (!changed) break :blk id;
                    break :blk try self.types.freshContent(.{ .tag_union = .{
                        .tags = try self.types.addTags(out_tags),
                    } });
                },
                else => id,
            },
            else => id,
        };
    }

    fn overlayErasureTemplate(self: *Lowerer, template_ty: TypeVarId, actual_ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.overlayErasureTemplateRec(template_ty, actual_ty, &visited);
    }

    fn overlayErasureTemplateRec(
        self: *Lowerer,
        template_ty: TypeVarId,
        actual_ty: TypeVarId,
        visited: *std.ArrayList(TypePair),
    ) std.mem.Allocator.Error!TypeVarId {
        const template_id = self.types.unlinkPreservingNominal(template_ty);
        const actual_id = self.types.unlinkPreservingNominal(actual_ty);

        if (template_id == actual_id) return actual_ty;
        const pair = TypePair{ .left = template_id, .right = actual_id };
        for (visited.items) |seen| {
            if (seen.left == pair.left and seen.right == pair.right) return actual_ty;
        }
        try visited.append(self.allocator, pair);

        return switch (self.types.getNode(template_id)) {
            .for_a, .flex_for_a => actual_ty,
            .nominal => |template_nominal| switch (self.types.getNode(actual_id)) {
                .nominal => |actual_nominal| blk: {
                    const lowered_backing = try self.overlayErasureTemplateRec(template_nominal.backing, actual_nominal.backing, visited);
                    if (lowered_backing == actual_nominal.backing) break :blk actual_ty;
                    var lowered = actual_nominal;
                    lowered.backing = lowered_backing;
                    break :blk try self.types.fresh(.{ .nominal = lowered });
                },
                else => actual_ty,
            },
            .content => |template_content| switch (template_content) {
                .primitive => |template_prim| switch (template_prim) {
                    .erased => template_ty,
                    else => actual_ty,
                },
                .func => |template_func| switch (self.types.getNode(actual_id)) {
                    .content => |actual_content| switch (actual_content) {
                        .func => |actual_func| blk: {
                            const template_args = self.types.sliceTypeVarSpan(template_func.args);
                            const actual_args = self.types.sliceTypeVarSpan(actual_func.args);
                            if (template_args.len != actual_args.len) break :blk actual_ty;
                            const arg_out = try self.allocator.alloc(TypeVarId, template_args.len);
                            defer self.allocator.free(arg_out);
                            var args_changed = false;
                            for (template_args, actual_args, 0..) |template_arg, actual_arg, i| {
                                arg_out[i] = try self.overlayErasureTemplateRec(template_arg, actual_arg, visited);
                                if (arg_out[i] != actual_arg) args_changed = true;
                            }
                            const ret = try self.overlayErasureTemplateRec(template_func.ret, actual_func.ret, visited);
                            const lset = actual_func.lset;
                            if (!args_changed and lset == actual_func.lset and ret == actual_func.ret) {
                                break :blk actual_ty;
                            }
                            break :blk try self.types.freshContent(.{ .func = .{
                                .args = try self.types.addTypeVarSpan(arg_out),
                                .lset = lset,
                                .ret = ret,
                            } });
                        },
                        else => actual_ty,
                    },
                    else => actual_ty,
                },
                .box => |template_elem| switch (self.types.getNode(actual_id)) {
                    .content => |actual_content| switch (actual_content) {
                        .box => |actual_elem| blk: {
                            const elem = try self.overlayErasureTemplateRec(template_elem, actual_elem, visited);
                            if (elem == actual_elem) break :blk actual_ty;
                            break :blk try self.types.freshContent(.{ .box = elem });
                        },
                        else => actual_ty,
                    },
                    else => actual_ty,
                },
                .list => |template_elem| switch (self.types.getNode(actual_id)) {
                    .content => |actual_content| switch (actual_content) {
                        .list => |actual_elem| blk: {
                            const elem = try self.overlayErasureTemplateRec(template_elem, actual_elem, visited);
                            if (elem == actual_elem) break :blk actual_ty;
                            break :blk try self.types.freshContent(.{ .list = elem });
                        },
                        else => actual_ty,
                    },
                    else => actual_ty,
                },
                .tuple => |template_tuple| switch (self.types.getNode(actual_id)) {
                    .content => |actual_content| switch (actual_content) {
                        .tuple => |actual_tuple| blk: {
                            const template_elems = self.types.sliceTypeVarSpan(template_tuple);
                            const actual_elems = self.types.sliceTypeVarSpan(actual_tuple);
                            if (template_elems.len != actual_elems.len) break :blk actual_ty;
                            const out = try self.allocator.alloc(TypeVarId, template_elems.len);
                            defer self.allocator.free(out);
                            var changed = false;
                            for (template_elems, actual_elems, 0..) |template_elem, actual_elem, i| {
                                out[i] = try self.overlayErasureTemplateRec(template_elem, actual_elem, visited);
                                if (out[i] != actual_elem) changed = true;
                            }
                            if (!changed) break :blk actual_ty;
                            break :blk try self.types.freshContent(.{ .tuple = try self.types.addTypeVarSpan(out) });
                        },
                        else => actual_ty,
                    },
                    else => actual_ty,
                },
                .record => |template_record| switch (self.types.getNode(actual_id)) {
                    .content => |actual_content| switch (actual_content) {
                        .record => |actual_record| blk: {
                            const template_fields = self.types.sliceFields(template_record.fields);
                            const actual_fields = self.types.sliceFields(actual_record.fields);
                            if (template_fields.len != actual_fields.len) break :blk actual_ty;
                            const out = try self.allocator.alloc(type_mod.Field, template_fields.len);
                            defer self.allocator.free(out);
                            var changed = false;
                            for (template_fields, actual_fields, 0..) |template_field, actual_field, i| {
                                if (template_field.name != actual_field.name) break :blk actual_ty;
                                out[i] = .{
                                    .name = actual_field.name,
                                    .ty = try self.overlayErasureTemplateRec(template_field.ty, actual_field.ty, visited),
                                };
                                if (out[i].ty != actual_field.ty) changed = true;
                            }
                            if (!changed) break :blk actual_ty;
                            break :blk try self.types.freshContent(.{ .record = .{
                                .fields = try self.types.addFields(out),
                            } });
                        },
                        else => actual_ty,
                    },
                    else => actual_ty,
                },
                .tag_union => |template_union| switch (self.types.getNode(actual_id)) {
                    .content => |actual_content| switch (actual_content) {
                        .tag_union => |actual_union| blk: {
                            const template_tags = self.types.sliceTags(template_union.tags);
                            const actual_tags = self.types.sliceTags(actual_union.tags);
                            if (template_tags.len != actual_tags.len) break :blk actual_ty;
                            const out = try self.allocator.alloc(type_mod.Tag, template_tags.len);
                            defer self.allocator.free(out);
                            var changed = false;
                            for (template_tags, actual_tags, 0..) |template_tag, actual_tag, i| {
                                if (template_tag.name != actual_tag.name) break :blk actual_ty;
                                const template_args = self.types.sliceTypeVarSpan(template_tag.args);
                                const actual_args = self.types.sliceTypeVarSpan(actual_tag.args);
                                if (template_args.len != actual_args.len) break :blk actual_ty;
                                const out_args = try self.allocator.alloc(TypeVarId, template_args.len);
                                defer self.allocator.free(out_args);
                                for (template_args, actual_args, 0..) |template_arg, actual_arg, arg_i| {
                                    out_args[arg_i] = try self.overlayErasureTemplateRec(template_arg, actual_arg, visited);
                                    if (out_args[arg_i] != actual_arg) changed = true;
                                }
                                out[i] = .{
                                    .name = actual_tag.name,
                                    .args = try self.types.addTypeVarSpan(out_args),
                                };
                            }
                            if (!changed) break :blk actual_ty;
                            break :blk try self.types.freshContent(.{ .tag_union = .{
                                .tags = try self.types.addTags(out),
                            } });
                        },
                        else => actual_ty,
                    },
                    else => actual_ty,
                },
                else => actual_ty,
            },
            else => actual_ty,
        };
    }

    fn propagateStmtErasure(
        self: *Lowerer,
        venv: []const EnvEntry,
        stmt_id: ast.StmtId,
    ) std.mem.Allocator.Error![]EnvEntry {
        const stmt = &self.output.stmts.items[@intFromEnum(stmt_id)];
        switch (stmt.*) {
            .decl => |*decl| {
                try self.propagateExprErasure(decl.body, venv);
                decl.bind.ty = self.output.getExpr(decl.body).ty;
                return try self.extendEnvOne(venv, .{
                    .symbol = decl.bind.symbol,
                    .ty = decl.bind.ty,
                });
            },
            .var_decl => |*decl| {
                try self.propagateExprErasure(decl.body, venv);
                decl.bind.ty = self.output.getExpr(decl.body).ty;
                return try self.extendEnvOne(venv, .{
                    .symbol = decl.bind.symbol,
                    .ty = decl.bind.ty,
                });
            },
            .reassign => |reassign| {
                try self.propagateExprErasure(reassign.body, venv);
                return try self.cloneEnv(venv);
            },
            .expr => |nested| {
                try self.propagateExprErasure(nested, venv);
                return try self.cloneEnv(venv);
            },
            .debug => |nested| {
                try self.propagateExprErasure(nested, venv);
                return try self.cloneEnv(venv);
            },
            .expect => |nested| {
                try self.propagateExprErasure(nested, venv);
                return try self.cloneEnv(venv);
            },
            .crash, .break_ => return try self.cloneEnv(venv),
            .return_ => |nested| {
                try self.propagateExprErasure(nested, venv);
                return try self.cloneEnv(venv);
            },
            .for_ => |for_stmt| {
                try self.propagateExprErasure(for_stmt.iterable, venv);
                const additions = try self.patternEnvEntries(for_stmt.patt);
                defer self.allocator.free(additions);
                const body_env = try self.extendEnvMany(venv, additions);
                defer if (body_env.len != 0) self.allocator.free(body_env);
                try self.propagateExprErasure(for_stmt.body, body_env);
                return try self.cloneEnv(venv);
            },
            .while_ => |while_stmt| {
                try self.propagateExprErasure(while_stmt.cond, venv);
                try self.propagateExprErasure(while_stmt.body, venv);
                return try self.cloneEnv(venv);
            },
        }
    }

    fn patternEnvEntries(self: *Lowerer, pat_id: ast.PatId) std.mem.Allocator.Error![]EnvEntry {
        const pat = self.output.getPat(pat_id);
        return switch (pat.data) {
            .bool_lit => try self.allocator.alloc(EnvEntry, 0),
            .var_ => |symbol| blk: {
                const out = try self.allocator.alloc(EnvEntry, 1);
                out[0] = .{ .symbol = symbol, .ty = pat.ty };
                break :blk out;
            },
            .tag => |tag| blk: {
                var out = std.ArrayList(EnvEntry).empty;
                defer out.deinit(self.allocator);
                for (self.output.slicePatSpan(tag.args)) |arg| {
                    const nested = try self.patternEnvEntries(arg);
                    defer self.allocator.free(nested);
                    try out.appendSlice(self.allocator, nested);
                }
                break :blk try out.toOwnedSlice(self.allocator);
            },
        };
    }

    fn lambdaSetIsErased(self: *Lowerer, lset_ty: TypeVarId) bool {
        const id = self.types.unlink(lset_ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| self.lambdaSetIsErased(nominal.backing),
            .content => |content| switch (content) {
                .primitive => |prim| prim == .erased,
                else => false,
            },
            .unbd, .for_a, .flex_for_a => false,
            .link => unreachable,
        };
    }

    fn boxedPayloadType(self: *Lowerer, ty: TypeVarId) ?TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| self.boxedPayloadType(nominal.backing),
            .content => |content| switch (content) {
                .box => |elem| elem,
                else => null,
            },
            else => null,
        };
    }

    fn boxBoundaryBuiltinOpForSymbol(self: *Lowerer, symbol: Symbol) ?base.LowLevel {
        if (self.exact_callable_aliases.get(symbol)) |exact| {
            if (exact != symbol) return self.boxBoundaryBuiltinOpForSymbol(exact);
        }
        const def_id = self.def_id_by_symbol.get(symbol) orelse return null;
        const def = self.output.getDef(def_id);
        const fn_def = switch (def.value) {
            .fn_ => |fn_def| fn_def,
            .val => |expr_id| {
                const exact = self.maybeExactCallableSymbolForExpr(expr_id) orelse return null;
                if (exact == symbol) return null;
                return self.boxBoundaryBuiltinOpForSymbol(exact);
            },
            else => return null,
        };
        if (!self.types.hasCapturelessLambda(def.bind.ty, def.bind.symbol)) return null;
        const body = self.output.getExpr(fn_def.body);
        return switch (body.data) {
            .low_level => |ll| blk: {
                const args = self.output.sliceExprSpan(ll.args);
                if (args.len != 1) break :blk null;
                const arg_expr = self.output.getExpr(args[0]);
                const fn_args = self.output.sliceTypedSymbolSpan(fn_def.args);
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

    fn reorderDefsByScc(self: *Lowerer) std.mem.Allocator.Error!void {
        const sccs = try self.computeSccs();
        defer self.freeSccs(sccs);

        self.root_defs.clearRetainingCapacity();
        for (sccs) |group| {
            try self.root_defs.appendSlice(self.allocator, group.def_ids);
        }
    }

    fn computeSccs(self: *Lowerer) std.mem.Allocator.Error![]SccGroup {
        const count = self.output.defsSlice().len;
        const ids = try self.allocator.alloc(ast.DefId, count);
        defer self.allocator.free(ids);
        for (self.output.defsSlice(), 0..) |_, i| {
            ids[i] = @enumFromInt(@as(u32, @intCast(i)));
        }

        const Tarjan = struct {
            lowerer: *Lowerer,
            ids: []const ast.DefId,
            index: i32,
            indices: []i32,
            lowlinks: []i32,
            on_stack: []bool,
            stack: std.ArrayList(usize),
            groups: std.ArrayList(SccGroup),

            fn run(tarjan: *@This()) std.mem.Allocator.Error![]SccGroup {
                for (tarjan.ids, 0..) |_, i| {
                    if (tarjan.indices[i] == -1) {
                        try tarjan.strongConnect(i);
                    }
                }
                return try tarjan.groups.toOwnedSlice(tarjan.lowerer.allocator);
            }

            fn strongConnect(tarjan: *@This(), idx: usize) std.mem.Allocator.Error!void {
                tarjan.indices[idx] = tarjan.index;
                tarjan.lowlinks[idx] = tarjan.index;
                tarjan.index += 1;
                try tarjan.stack.append(tarjan.lowerer.allocator, idx);
                tarjan.on_stack[idx] = true;

                const edges = try tarjan.lowerer.collectDefEdges(tarjan.ids[idx]);
                defer tarjan.lowerer.allocator.free(edges);
                for (edges) |target_symbol| {
                    const target_idx = tarjan.lowerer.lookupDenseIndex(tarjan.ids, target_symbol) orelse continue;
                    if (tarjan.indices[target_idx] == -1) {
                        try tarjan.strongConnect(target_idx);
                        tarjan.lowlinks[idx] = @min(tarjan.lowlinks[idx], tarjan.lowlinks[target_idx]);
                    } else if (tarjan.on_stack[target_idx]) {
                        tarjan.lowlinks[idx] = @min(tarjan.lowlinks[idx], tarjan.indices[target_idx]);
                    }
                }

                if (tarjan.lowlinks[idx] == tarjan.indices[idx]) {
                    var members = std.ArrayList(ast.DefId).empty;
                    while (true) {
                        const top_idx = tarjan.stack.pop().?;
                        tarjan.on_stack[top_idx] = false;
                        try members.append(tarjan.lowerer.allocator, tarjan.ids[top_idx]);
                        if (top_idx == idx) break;
                    }
                    try tarjan.groups.append(tarjan.lowerer.allocator, .{
                        .def_ids = try members.toOwnedSlice(tarjan.lowerer.allocator),
                    });
                }
            }
        };

        const indices = try self.allocator.alloc(i32, count);
        defer self.allocator.free(indices);
        const lowlinks = try self.allocator.alloc(i32, count);
        defer self.allocator.free(lowlinks);
        const on_stack = try self.allocator.alloc(bool, count);
        defer self.allocator.free(on_stack);
        @memset(indices, -1);
        @memset(lowlinks, 0);
        @memset(on_stack, false);

        var tarjan = Tarjan{
            .lowerer = self,
            .ids = ids,
            .index = 0,
            .indices = indices,
            .lowlinks = lowlinks,
            .on_stack = on_stack,
            .stack = .empty,
            .groups = .empty,
        };
        defer tarjan.stack.deinit(self.allocator);
        defer tarjan.groups.deinit(self.allocator);
        return try tarjan.run();
    }

    fn freeSccs(self: *Lowerer, sccs: []SccGroup) void {
        for (sccs) |group| {
            self.allocator.free(group.def_ids);
        }
        self.allocator.free(sccs);
    }

    fn collectDefEdges(self: *Lowerer, def_id: ast.DefId) std.mem.Allocator.Error![]Symbol {
        var edges = std.ArrayList(Symbol).empty;
        defer edges.deinit(self.allocator);

        const def = self.output.getDef(def_id);
        switch (def.value) {
            .fn_ => |fn_def| try self.collectExprEdges(fn_def.body, &edges),
            .hosted_fn => {},
            .val => |expr_id| try self.collectExprEdges(expr_id, &edges),
            .run => |run_def| try self.collectExprEdges(run_def.body, &edges),
        }

        return try edges.toOwnedSlice(self.allocator);
    }

    fn collectExprEdges(self: *Lowerer, expr_id: ast.ExprId, edges: *std.ArrayList(Symbol)) std.mem.Allocator.Error!void {
        const expr = self.output.getExpr(expr_id);
        switch (expr.data) {
            .var_ => |symbol| {
                if (self.def_id_by_symbol.contains(symbol) and !containsSymbol(edges.items, symbol)) {
                    try edges.append(self.allocator, symbol);
                }
            },
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .runtime_error,
            => {},
            .tag => |tag| for (self.output.sliceExprSpan(tag.args)) |arg| try self.collectExprEdges(arg, edges),
            .record => |fields| for (self.output.sliceFieldExprSpan(fields)) |field| try self.collectExprEdges(field.value, edges),
            .access => |access| try self.collectExprEdges(access.record, edges),
            .structural_eq => |eq| {
                try self.collectExprEdges(eq.lhs, edges);
                try self.collectExprEdges(eq.rhs, edges);
            },
            .method_eq => |eq| {
                try self.collectExprEdges(eq.lhs, edges);
                try self.collectExprEdges(eq.rhs, edges);
            },
            .dispatch_call => |method_call| {
                try self.collectExprEdges(method_call.receiver, edges);
                for (self.output.sliceExprSpan(method_call.args)) |arg| {
                    try self.collectExprEdges(arg, edges);
                }
            },
            .type_dispatch_call => |method_call| {
                for (self.output.sliceExprSpan(method_call.args)) |arg| {
                    try self.collectExprEdges(arg, edges);
                }
            },
            .let_ => |let_expr| {
                try self.collectExprEdges(let_expr.body, edges);
                try self.collectExprEdges(let_expr.rest, edges);
            },
            .call => |call| {
                try self.collectExprEdges(call.func, edges);
                for (self.output.sliceExprSpan(call.args)) |arg| {
                    try self.collectExprEdges(arg, edges);
                }
            },
            .inspect => |value| try self.collectExprEdges(value, edges),
            .low_level => |ll| for (self.output.sliceExprSpan(ll.args)) |arg| try self.collectExprEdges(arg, edges),
            .when => |when_expr| {
                try self.collectExprEdges(when_expr.cond, edges);
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    try self.collectExprEdges(branch.body, edges);
                }
            },
            .if_ => |if_expr| {
                try self.collectExprEdges(if_expr.cond, edges);
                try self.collectExprEdges(if_expr.then_body, edges);
                try self.collectExprEdges(if_expr.else_body, edges);
            },
            .block => |block| {
                for (self.output.sliceStmtSpan(block.stmts)) |stmt_id| {
                    const stmt = self.output.getStmt(stmt_id);
                    switch (stmt) {
                        .decl => |decl| try self.collectExprEdges(decl.body, edges),
                        .var_decl => |decl| try self.collectExprEdges(decl.body, edges),
                        .reassign => |reassign| try self.collectExprEdges(reassign.body, edges),
                        .expr => |nested| try self.collectExprEdges(nested, edges),
                        .debug => |nested| try self.collectExprEdges(nested, edges),
                        .expect => |nested| try self.collectExprEdges(nested, edges),
                        .crash => {},
                        .return_ => |nested| try self.collectExprEdges(nested, edges),
                        .break_ => {},
                        .for_ => |for_stmt| {
                            try self.collectExprEdges(for_stmt.iterable, edges);
                            try self.collectExprEdges(for_stmt.body, edges);
                        },
                        .while_ => |while_stmt| {
                            try self.collectExprEdges(while_stmt.cond, edges);
                            try self.collectExprEdges(while_stmt.body, edges);
                        },
                    }
                }
                try self.collectExprEdges(block.final_expr, edges);
            },
            .tuple => |tuple| for (self.output.sliceExprSpan(tuple)) |item| try self.collectExprEdges(item, edges),
            .tag_payload => |tag_payload| try self.collectExprEdges(tag_payload.tag_union, edges),
            .tuple_access => |tuple_access| try self.collectExprEdges(tuple_access.tuple, edges),
            .list => |list| for (self.output.sliceExprSpan(list)) |item| try self.collectExprEdges(item, edges),
            .return_ => |ret| try self.collectExprEdges(ret, edges),
            .for_ => |for_expr| {
                try self.collectExprEdges(for_expr.iterable, edges);
                try self.collectExprEdges(for_expr.body, edges);
            },
        }
    }


    fn lookupDenseIndex(self: *Lowerer, ids: []const ast.DefId, symbol: Symbol) ?usize {
        for (ids, 0..) |def_id, i| {
            if (self.output.getDef(def_id).bind.symbol == symbol) return i;
        }
        return null;
    }

    fn lookupEnv(_: *Lowerer, venv: []const EnvEntry, symbol: Symbol) ?TypeVarId {
        var i = venv.len;
        while (i > 0) {
            i -= 1;
            if (venv[i].symbol == symbol) return venv[i].ty;
        }
        return null;
    }

    fn cloneEnv(self: *Lowerer, venv: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        return try self.extendEnvMany(&.{}, venv);
    }

    fn extendEnvOne(self: *Lowerer, venv: []const EnvEntry, entry: EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        return try self.extendEnvMany(venv, &.{entry});
    }

    fn extendEnvMany(self: *Lowerer, venv: []const EnvEntry, extra: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, venv.len + extra.len);
        std.mem.copyForwards(EnvEntry, out[0..venv.len], venv);
        std.mem.copyForwards(EnvEntry, out[venv.len..], extra);
        return out;
    }

    fn isGeneralized(self: *Lowerer, ty: TypeVarId) bool {
        var visited = std.AutoHashMap(TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        return self.isGeneralizedRec(ty, &visited) catch false;
    }

    fn isGeneralizedRec(self: *Lowerer, ty: TypeVarId, visited: *std.AutoHashMap(TypeVarId, void)) std.mem.Allocator.Error!bool {
        const id = self.types.unlink(ty);
        if (visited.contains(id)) return false;
        try visited.put(id, {});

        return switch (self.types.getNode(id)) {
            .unbd => false,
            .for_a, .flex_for_a => true,
            .link => unreachable,
            .nominal => |nominal| blk: {
                for (self.types.sliceTypeVarSpan(nominal.args)) |arg| {
                    if (try self.isGeneralizedRec(arg, visited)) break :blk true;
                }
                break :blk try self.isGeneralizedRec(nominal.backing, visited);
            },
            .content => |content| switch (content) {
                .primitive => false,
                .func => |func| blk: {
                    for (self.types.sliceTypeVarSpan(func.args)) |arg| {
                        if (try self.isGeneralizedRec(arg, visited)) break :blk true;
                    }
                    break :blk try self.isGeneralizedRec(func.lset, visited) or
                        try self.isGeneralizedRec(func.ret, visited);
                },
                .list => |elem| try self.isGeneralizedRec(elem, visited),
                .box => |elem| try self.isGeneralizedRec(elem, visited),
                .tuple => |elems| blk: {
                    for (self.types.sliceTypeVarSpan(elems)) |elem| {
                        if (try self.isGeneralizedRec(elem, visited)) break :blk true;
                    }
                    break :blk false;
                },
                .tag_union => |tag_union| blk: {
                    for (self.types.sliceTags(tag_union.tags)) |tag| {
                        for (self.types.sliceTypeVarSpan(tag.args)) |arg| {
                            if (try self.isGeneralizedRec(arg, visited)) break :blk true;
                        }
                    }
                    break :blk false;
                },
                .record => |record| blk: {
                    for (self.types.sliceFields(record.fields)) |field| {
                        if (try self.isGeneralizedRec(field.ty, visited)) break :blk true;
                    }
                    break :blk false;
                },
                .lambda_set => |lambda_set| blk: {
                    for (self.types.sliceLambdas(lambda_set)) |lambda| {
                        for (self.types.sliceCaptures(lambda.captures)) |capture| {
                            if (try self.isGeneralizedRec(capture.ty, visited)) break :blk true;
                        }
                    }
                    break :blk false;
                },
            },
        };
    }

    fn instantiateGeneralized(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        if (!self.isGeneralized(ty)) return ty;

        var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(self.allocator);
        defer mapping.deinit();
        return try self.instantiateGeneralizedRec(ty, &mapping);
    }

    fn instantiateGeneralizedRec(self: *Lowerer, ty: TypeVarId, mapping: *std.AutoHashMap(TypeVarId, TypeVarId)) std.mem.Allocator.Error!TypeVarId {
        const id = self.types.unlinkPreservingNominal(ty);
        if (mapping.get(id)) |cached| return cached;

        switch (self.types.getNode(id)) {
            .unbd => return id,
            .for_a, .flex_for_a => {
                const fresh = try self.types.freshUnbd();
                try mapping.put(id, fresh);
                return fresh;
            },
            else => {},
        }

        const placeholder = try self.types.freshUnbd();
        try mapping.put(id, placeholder);

        const lowered = switch (self.types.getNode(id)) {
            .unbd, .for_a, .flex_for_a => unreachable,
            .link => unreachable,
            .nominal => |nominal| blk: {
                const args = self.types.sliceTypeVarSpan(nominal.args);
                const lowered_args = try self.allocator.alloc(TypeVarId, args.len);
                defer self.allocator.free(lowered_args);
                for (args, 0..) |arg, i| {
                    lowered_args[i] = try self.instantiateGeneralizedRec(arg, mapping);
                }
                break :blk type_mod.Node{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.types.addTypeVarSpan(lowered_args),
                    .backing = try self.instantiateGeneralizedRec(nominal.backing, mapping),
                } };
            },
            .content => |content| switch (content) {
                .primitive => type_mod.Node{ .content = .{ .primitive = content.primitive } },
                .func => blk: {
                    const src_args = self.types.sliceTypeVarSpan(content.func.args);
                    const out_args = try self.allocator.alloc(TypeVarId, src_args.len);
                    defer self.allocator.free(out_args);
                    for (src_args, 0..) |arg, i| {
                        out_args[i] = try self.instantiateGeneralizedRec(arg, mapping);
                    }
                    break :blk type_mod.Node{ .content = .{ .func = .{
                        .args = try self.types.addTypeVarSpan(out_args),
                        .lset = try self.instantiateGeneralizedRec(content.func.lset, mapping),
                        .ret = try self.instantiateGeneralizedRec(content.func.ret, mapping),
                    } } };
                },
                .list => |elem| type_mod.Node{ .content = .{
                    .list = try self.instantiateGeneralizedRec(elem, mapping),
                } },
                .box => |elem| type_mod.Node{ .content = .{
                    .box = try self.instantiateGeneralizedRec(elem, mapping),
                } },
                .tuple => |tuple| blk: {
                    const elems = self.types.sliceTypeVarSpan(tuple);
                    const elems_copy = try self.allocator.dupe(TypeVarId, elems);
                    defer self.allocator.free(elems_copy);
                    const lowered_elems = try self.allocator.alloc(TypeVarId, elems_copy.len);
                    defer self.allocator.free(lowered_elems);
                    for (elems_copy, 0..) |elem, i| {
                        lowered_elems[i] = try self.instantiateGeneralizedRec(elem, mapping);
                    }
                    break :blk type_mod.Node{ .content = .{
                        .tuple = try self.types.addTypeVarSpan(lowered_elems),
                    } };
                },
                .tag_union => |tag_union| blk: {
                    const tags = self.types.sliceTags(tag_union.tags);
                    const tags_copy = try self.allocator.dupe(type_mod.Tag, tags);
                    defer self.allocator.free(tags_copy);
                    const out = try self.allocator.alloc(type_mod.Tag, tags_copy.len);
                    defer self.allocator.free(out);
                    for (tags_copy, 0..) |tag, i| {
                        const args = self.types.sliceTypeVarSpan(tag.args);
                        const args_copy = try self.allocator.dupe(TypeVarId, args);
                        defer self.allocator.free(args_copy);
                        const lowered_args = try self.allocator.alloc(TypeVarId, args_copy.len);
                        defer self.allocator.free(lowered_args);
                        for (args_copy, 0..) |arg, arg_i| {
                            lowered_args[arg_i] = try self.instantiateGeneralizedRec(arg, mapping);
                        }
                        out[i] = .{
                            .name = tag.name,
                            .args = try self.types.addTypeVarSpan(lowered_args),
                        };
                    }
                    break :blk type_mod.Node{ .content = .{
                        .tag_union = .{ .tags = try self.types.addTags(out) },
                    } };
                },
                .record => |record| blk: {
                    const fields = self.types.sliceFields(record.fields);
                    const fields_copy = try self.allocator.dupe(type_mod.Field, fields);
                    defer self.allocator.free(fields_copy);
                    const out = try self.allocator.alloc(type_mod.Field, fields_copy.len);
                    defer self.allocator.free(out);
                    for (fields_copy, 0..) |field, i| {
                        out[i] = .{
                            .name = field.name,
                            .ty = try self.instantiateGeneralizedRec(field.ty, mapping),
                        };
                    }
                    sortFieldsByName(&self.input.idents, out);
                    break :blk type_mod.Node{ .content = .{
                        .record = .{ .fields = try self.types.addFields(out) },
                    } };
                },
                .lambda_set => |lambda_set| blk: {
                    const lambdas = self.types.sliceLambdas(lambda_set);
                    const lambdas_copy = try self.allocator.dupe(type_mod.Lambda, lambdas);
                    defer self.allocator.free(lambdas_copy);
                    const out_lambdas = try self.allocator.alloc(type_mod.Lambda, lambdas_copy.len);
                    defer self.allocator.free(out_lambdas);
                    for (lambdas_copy, 0..) |lambda, i| {
                        const captures = self.types.sliceCaptures(lambda.captures);
                        const captures_copy = try self.allocator.dupe(type_mod.Capture, captures);
                        defer self.allocator.free(captures_copy);
                        const out_captures = try self.allocator.alloc(type_mod.Capture, captures_copy.len);
                        defer self.allocator.free(out_captures);
                        for (captures_copy, 0..) |capture, capture_i| {
                            out_captures[capture_i] = .{
                                .symbol = capture.symbol,
                                .ty = try self.instantiateGeneralizedRec(capture.ty, mapping),
                            };
                        }
                        out_lambdas[i] = .{
                            .symbol = lambda.symbol,
                            .captures = try self.types.addCaptures(out_captures),
                        };
                    }
                    break :blk type_mod.Node{ .content = .{
                        .lambda_set = try self.types.addLambdas(out_lambdas),
                    } };
                },
            },
        };

        self.types.setNode(placeholder, lowered);
        return placeholder;
    }

    fn occurs(self: *Lowerer, needle: TypeVarId, ty: TypeVarId) bool {
        var visited = std.AutoHashMap(TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        return self.occursRec(self.types.unlink(needle), ty, &visited) catch false;
    }

    fn occursRec(self: *Lowerer, needle: TypeVarId, ty: TypeVarId, visited: *std.AutoHashMap(TypeVarId, void)) std.mem.Allocator.Error!bool {
        const id = self.types.unlink(ty);
        if (visited.contains(id)) return false;
        try visited.put(id, {});
        if (id == needle) return true;

        return switch (self.types.getNode(id)) {
            .unbd, .for_a, .flex_for_a => false,
            .link => unreachable,
            .nominal => |nominal| blk: {
                for (self.types.sliceTypeVarSpan(nominal.args)) |arg| {
                    if (try self.occursRec(needle, arg, visited)) break :blk true;
                }
                break :blk try self.occursRec(needle, nominal.backing, visited);
            },
            .content => |content| switch (content) {
                .primitive => false,
                .func => |func| blk: {
                    for (self.types.sliceTypeVarSpan(func.args)) |arg| {
                        if (try self.occursRec(needle, arg, visited)) break :blk true;
                    }
                    break :blk try self.occursRec(needle, func.lset, visited) or
                        try self.occursRec(needle, func.ret, visited);
                },
                .list => |elem| try self.occursRec(needle, elem, visited),
                .box => |elem| try self.occursRec(needle, elem, visited),
                .tuple => |elems| blk: {
                    for (self.types.sliceTypeVarSpan(elems)) |elem| {
                        if (try self.occursRec(needle, elem, visited)) break :blk true;
                    }
                    break :blk false;
                },
                .tag_union => |tag_union| blk: {
                    for (self.types.sliceTags(tag_union.tags)) |tag| {
                        for (self.types.sliceTypeVarSpan(tag.args)) |arg| {
                            if (try self.occursRec(needle, arg, visited)) break :blk true;
                        }
                    }
                    break :blk false;
                },
                .record => |record| blk: {
                    for (self.types.sliceFields(record.fields)) |field| {
                        if (try self.occursRec(needle, field.ty, visited)) break :blk true;
                    }
                    break :blk false;
                },
                .lambda_set => |lambda_set| blk: {
                    for (self.types.sliceLambdas(lambda_set)) |lambda| {
                        for (self.types.sliceCaptures(lambda.captures)) |capture| {
                            if (try self.occursRec(needle, capture.ty, visited)) break :blk true;
                        }
                    }
                    break :blk false;
                },
            },
        };
    }

    fn generalize(self: *Lowerer, venv: []const EnvEntry, ty: TypeVarId) std.mem.Allocator.Error!void {
        var visited = std.AutoHashMap(TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        try self.generalizeRec(venv, ty, &visited);
    }

    fn generalizeRec(self: *Lowerer, venv: []const EnvEntry, ty: TypeVarId, visited: *std.AutoHashMap(TypeVarId, void)) std.mem.Allocator.Error!void {
        const id = self.types.unlink(ty);
        if (visited.contains(id)) return;
        try visited.put(id, {});

        switch (self.types.getNode(id)) {
            .unbd => {
                for (venv) |entry| {
                    if (self.occurs(id, entry.ty)) {
                        return;
                    }
                }
                self.types.setNode(id, .for_a);
            },
            .for_a, .flex_for_a => {},
            .link => unreachable,
            .nominal => |nominal| {
                for (self.types.sliceTypeVarSpan(nominal.args)) |arg| {
                    try self.generalizeRec(venv, arg, visited);
                }
                try self.generalizeRec(venv, nominal.backing, visited);
            },
            .content => |content| switch (content) {
                .primitive => {},
                .func => |func| {
                    for (self.types.sliceTypeVarSpan(func.args)) |arg| {
                        try self.generalizeRec(venv, arg, visited);
                    }
                    try self.generalizeRec(venv, func.lset, visited);
                    try self.generalizeRec(venv, func.ret, visited);
                },
                .list => |elem| {
                    try self.generalizeRec(venv, elem, visited);
                },
                .box => |elem| {
                    try self.generalizeRec(venv, elem, visited);
                },
                .tuple => |elems| {
                    for (self.types.sliceTypeVarSpan(elems)) |elem| {
                        try self.generalizeRec(venv, elem, visited);
                    }
                },
                .tag_union => |tag_union| {
                    for (self.types.sliceTags(tag_union.tags)) |tag| {
                        for (self.types.sliceTypeVarSpan(tag.args)) |arg| {
                            try self.generalizeRec(venv, arg, visited);
                        }
                    }
                },
                .record => |record| {
                    for (self.types.sliceFields(record.fields)) |field| {
                        try self.generalizeRec(venv, field.ty, visited);
                    }
                },
                .lambda_set => |lambda_set| {
                    for (self.types.sliceLambdas(lambda_set)) |lambda| {
                        for (self.types.sliceCaptures(lambda.captures)) |capture| {
                            try self.generalizeRec(venv, capture.ty, visited);
                        }
                    }
                },
            },
        }
    }

    fn unify(self: *Lowerer, left: TypeVarId, right: TypeVarId) std.mem.Allocator.Error!void {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        try self.unifyRec(left, right, &visited);
    }

    fn unifyRec(self: *Lowerer, left: TypeVarId, right: TypeVarId, visited: *std.ArrayList(TypePair)) std.mem.Allocator.Error!void {
        const l = self.types.unlink(left);
        const r = self.types.unlink(right);
        if (l == r) return;
        for (visited.items) |pair| {
            if (pair.left == l and pair.right == r) return;
        }
        try visited.append(self.allocator, .{ .left = l, .right = r });

        switch (self.types.getNode(l)) {
            .unbd => {
                if (self.occurs(l, r)) {
                    if (builtin.mode == .Debug) {
                        const left_text = self.debugTypeSummary(l);
                        defer if (left_text.owned) self.allocator.free(left_text.text);
                        const right_text = self.debugTypeSummary(r);
                        defer if (right_text.owned) self.allocator.free(right_text.text);
                        const current_def_name = if (self.current_def_symbol) |def_symbol| blk: {
                            const entry = self.input.symbols.get(def_symbol);
                            break :blk if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name);
                        } else "<none>";
                        const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                        std.debug.panic(
                            "lambdasolved.unify occurs check failed\ndef={s}\ndef_raw={d}\nleft={s}\nright={s}",
                            .{ current_def_name, current_def_raw, left_text.text, right_text.text },
                        );
                    }
                    return debugPanic("lambdasolved.unify occurs check failed", .{});
                }
                self.types.setNode(l, .{ .link = r });
                return;
            },
            .for_a, .flex_for_a => {
                if (builtin.mode == .Debug) {
                    const left_text = self.debugTypeSummary(l);
                    defer if (left_text.owned) self.allocator.free(left_text.text);
                    const right_text = self.debugTypeSummary(r);
                    defer if (right_text.owned) self.allocator.free(right_text.text);
                    const current_def_name = if (self.current_def_symbol) |def_symbol| blk: {
                        const entry = self.input.symbols.get(def_symbol);
                        break :blk if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name);
                    } else "<none>";
                    const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                    std.debug.panic(
                        "lambdasolved.unify generalized type without instantiation\ndef={s}\ndef_raw={d}\nleft={s}\nright={s}",
                        .{ current_def_name, current_def_raw, left_text.text, right_text.text },
                    );
                }
                return debugPanic("lambdasolved.unify generalized type without instantiation", .{});
            },
            .link => unreachable,
            .nominal => |left_nominal| switch (self.types.getNode(r)) {
                .unbd => {
                    if (self.occurs(r, l)) {
                        if (builtin.mode == .Debug) {
                            const left_text = self.debugTypeSummary(l);
                            defer if (left_text.owned) self.allocator.free(left_text.text);
                            const right_text = self.debugTypeSummary(r);
                            defer if (right_text.owned) self.allocator.free(right_text.text);
                            const current_def_name = if (self.current_def_symbol) |def_symbol| blk: {
                                const entry = self.input.symbols.get(def_symbol);
                                break :blk if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name);
                            } else "<none>";
                            const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                            std.debug.panic(
                                "lambdasolved.unify occurs check failed\ndef={s}\ndef_raw={d}\nleft={s}\nright={s}",
                                .{ current_def_name, current_def_raw, left_text.text, right_text.text },
                            );
                        }
                        return debugPanic("lambdasolved.unify occurs check failed", .{});
                    }
                    self.types.setNode(r, .{ .link = l });
                    return;
                },
                .for_a, .flex_for_a => {
                    if (builtin.mode == .Debug) {
                        const left_text = self.debugTypeSummary(l);
                        defer if (left_text.owned) self.allocator.free(left_text.text);
                        const right_text = self.debugTypeSummary(r);
                        defer if (right_text.owned) self.allocator.free(right_text.text);
                        const current_def_name = if (self.current_def_symbol) |def_symbol| blk: {
                            const entry = self.input.symbols.get(def_symbol);
                            break :blk if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name);
                        } else "<none>";
                        const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                        std.debug.panic(
                            "lambdasolved.unify generalized type without instantiation\ndef={s}\ndef_raw={d}\nleft={s}\nright={s}",
                            .{ current_def_name, current_def_raw, left_text.text, right_text.text },
                        );
                    }
                    return debugPanic("lambdasolved.unify generalized type without instantiation", .{});
                },
                .link => unreachable,
                .nominal => |right_nominal| {
                    if (left_nominal.module_idx != right_nominal.module_idx or
                        left_nominal.ident != right_nominal.ident or
                        left_nominal.is_opaque != right_nominal.is_opaque)
                    {
                        return debugPanic("lambdasolved.unify incompatible nominal types", .{});
                    }

                    const left_args = self.types.sliceTypeVarSpan(left_nominal.args);
                    const right_args = self.types.sliceTypeVarSpan(right_nominal.args);
                    if (left_args.len != right_args.len) {
                        return debugPanic("lambdasolved.unify nominal arity mismatch", .{});
                    }

                    for (left_args, 0..) |left_arg, i| {
                        try self.unifyRec(left_arg, right_args[i], visited);
                    }
                    try self.unifyRec(left_nominal.backing, right_nominal.backing, visited);
                    self.types.setNode(l, .{ .link = r });
                    return;
                },
                .content => {
                    try self.unifyRec(left_nominal.backing, r, visited);
                    return;
                },
            },
            .content => |left_content| switch (self.types.getNode(r)) {
                .unbd => {
                    if (self.occurs(r, l)) {
                        if (builtin.mode == .Debug) {
                            const left_text = self.debugTypeSummary(l);
                            defer if (left_text.owned) self.allocator.free(left_text.text);
                            const right_text = self.debugTypeSummary(r);
                            defer if (right_text.owned) self.allocator.free(right_text.text);
                            const current_def_name = if (self.current_def_symbol) |def_symbol| blk: {
                                const entry = self.input.symbols.get(def_symbol);
                                break :blk if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name);
                            } else "<none>";
                            const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                            std.debug.panic(
                                "lambdasolved.unify occurs check failed\ndef={s}\ndef_raw={d}\nleft={s}\nright={s}",
                                .{ current_def_name, current_def_raw, left_text.text, right_text.text },
                            );
                        }
                        return debugPanic("lambdasolved.unify occurs check failed", .{});
                    }
                    self.types.setNode(r, .{ .link = l });
                    return;
                },
                .for_a, .flex_for_a => {
                    return debugPanic("lambdasolved.unify generalized type without instantiation", .{});
                },
                .link => unreachable,
                .nominal => |right_nominal| {
                    try self.unifyRec(l, right_nominal.backing, visited);
                    return;
                },
                .content => |right_content| {
                    const next_node = try self.unifyContent(left, left_content, right, right_content, visited);
                    const merged = try self.types.fresh(next_node);
                    self.types.setNode(l, .{ .link = merged });
                    self.types.setNode(r, .{ .link = merged });
                    return;
                },
            },
        }
    }

    const DebugTypeText = struct {
        text: []const u8,
        owned: bool,
    };

    fn debugTypeSummary(self: *Lowerer, ty: TypeVarId) DebugTypeText {
        if (builtin.mode != .Debug) unreachable;

        var out = std.ArrayList(u8).empty;
        self.debugWriteTypeSummary(&out, ty, 2) catch return .{ .text = "<oom>", .owned = false };
        const text = out.toOwnedSlice(self.allocator) catch return .{ .text = "<oom>", .owned = false };
        return .{ .text = text, .owned = true };
    }

    fn debugWriteTypeSummary(self: *Lowerer, out: *std.ArrayList(u8), ty: TypeVarId, depth: u8) std.mem.Allocator.Error!void {
        const id = self.types.unlinkPreservingNominal(ty);
        switch (self.types.getNode(id)) {
            .link => unreachable,
            .unbd => try out.writer(self.allocator).print("?{}", .{@intFromEnum(id)}),
            .for_a => try out.writer(self.allocator).print("forall{}", .{@intFromEnum(id)}),
            .flex_for_a => try out.writer(self.allocator).print("flexforall{}", .{@intFromEnum(id)}),
            .nominal => |nominal| {
                try out.appendSlice(self.allocator, self.input.idents.getText(nominal.ident));
                if (depth == 0) return;
                const args = self.types.sliceTypeVarSpan(nominal.args);
                if (args.len != 0) {
                    try out.append(self.allocator, '(');
                    for (args, 0..) |arg, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try self.debugWriteTypeSummary(out, arg, depth - 1);
                    }
                    try out.append(self.allocator, ')');
                }
            },
            .content => |content| switch (content) {
                .primitive => |prim| try out.appendSlice(self.allocator, @tagName(prim)),
                .list => |elem| {
                    try out.appendSlice(self.allocator, "List(");
                    if (depth == 0) {
                        try out.appendSlice(self.allocator, "...");
                    } else {
                        try self.debugWriteTypeSummary(out, elem, depth - 1);
                    }
                    try out.append(self.allocator, ')');
                },
                .box => |elem| {
                    try out.appendSlice(self.allocator, "Box(");
                    if (depth == 0) {
                        try out.appendSlice(self.allocator, "...");
                    } else {
                        try self.debugWriteTypeSummary(out, elem, depth - 1);
                    }
                    try out.append(self.allocator, ')');
                },
                .tuple => |_| try out.appendSlice(self.allocator, "Tuple"),
                .func => |func| {
                    try out.appendSlice(self.allocator, "Fn(");
                    if (depth == 0) {
                        try out.appendSlice(self.allocator, "...");
                    } else {
                        for (self.types.sliceTypeVarSpan(func.args), 0..) |arg, i| {
                            if (i != 0) try out.appendSlice(self.allocator, ", ");
                            try self.debugWriteTypeSummary(out, arg, depth - 1);
                        }
                        try out.appendSlice(self.allocator, " -> ");
                        try self.debugWriteTypeSummary(out, func.ret, depth - 1);
                    }
                    try out.append(self.allocator, ')');
                },
                .tag_union => |tag_union| {
                    const tags = self.types.sliceTags(tag_union.tags);
                    try out.append(self.allocator, '[');
                    for (tags, 0..) |tag, i| {
                        if (i != 0) try out.appendSlice(self.allocator, ", ");
                        try out.appendSlice(self.allocator, self.input.idents.getText(tag.name));
                    }
                    try out.append(self.allocator, ']');
                },
                .record => |_| try out.appendSlice(self.allocator, "Record"),
                .lambda_set => |_| try out.appendSlice(self.allocator, "LambdaSet"),
            },
        }
    }

    fn unifyContent(
        self: *Lowerer,
        left_ty: TypeVarId,
        left: type_mod.Content,
        right_ty: TypeVarId,
        right: type_mod.Content,
        visited: *std.ArrayList(TypePair),
    ) std.mem.Allocator.Error!type_mod.Node {
        if (@as(std.meta.Tag(type_mod.Content), left) != @as(std.meta.Tag(type_mod.Content), right)) {
            if (builtin.mode == .Debug) {
                const left_text = self.debugTypeSummary(left_ty);
                defer if (left_text.owned) self.allocator.free(left_text.text);
                const right_text = self.debugTypeSummary(right_ty);
                defer if (right_text.owned) self.allocator.free(right_text.text);
                const current_def_name = current_def_blk: {
                    if (self.current_def_symbol) |def_symbol| {
                        const def_entry = self.input.symbols.get(def_symbol);
                        break :current_def_blk if (def_entry.name.isNone()) "<none>" else self.input.idents.getText(def_entry.name);
                    }
                    break :current_def_blk "<none>";
                };
                const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                const current_def_origin = current_def_origin_blk: {
                    if (self.current_def_symbol) |def_symbol| {
                        break :current_def_origin_blk @tagName(self.input.symbols.get(def_symbol).origin);
                    }
                    break :current_def_origin_blk "<none>";
                };
                const current_expr_tag = if (self.current_expr_id) |expr_id|
                    @tagName(self.output.getExpr(expr_id).data)
                else
                    "<none>";
                const opaque_left = if (self.current_opaque_unify) |ctx| ctx.left else "<none>";
                const opaque_right = if (self.current_opaque_unify) |ctx| ctx.right else "<none>";
                const call_func_expr = if (self.current_call_context) |ctx| ctx.func_expr else "<none>";
                const call_arg_expr = if (self.current_call_context) |ctx| ctx.arg_expr else "<none>";
                const call_func_ty = if (self.current_call_context) |ctx| ctx.func_ty else "<none>";
                const call_arg_ty = if (self.current_call_context) |ctx| ctx.arg_ty else "<none>";
                const call_target_ty = if (self.current_call_context) |ctx| ctx.target_ty else "<none>";
                std.debug.panic(
                    "lambdasolved.unify incompatible types {s} vs {s}\ndef={s}\ndef_raw={d}\ndef_origin={s}\nexpr={s}\nopaque_left={s}\nopaque_right={s}\ncall_func_expr={s}\ncall_arg_expr={s}\ncall_func_ty={s}\ncall_arg_ty={s}\ncall_target_ty={s}\nleft={s}\nright={s}",
                    .{
                        @tagName(left),
                        @tagName(right),
                        current_def_name,
                        current_def_raw,
                        current_def_origin,
                        current_expr_tag,
                        opaque_left,
                        opaque_right,
                        call_func_expr,
                        call_arg_expr,
                        call_func_ty,
                        call_arg_ty,
                        call_target_ty,
                        left_text.text,
                        right_text.text,
                    },
                );
            } else unreachable;
        }

        return switch (left) {
            .primitive => |prim| blk: {
                if (prim != right.primitive) {
                    if (prim == .dec) break :blk .{ .content = .{ .primitive = right.primitive } };
                    if (right.primitive == .dec) break :blk .{ .content = .{ .primitive = prim } };
                    if (builtin.mode == .Debug) {
                        const left_text = self.debugTypeSummary(left_ty);
                        defer if (left_text.owned) self.allocator.free(left_text.text);
                        const right_text = self.debugTypeSummary(right_ty);
                        defer if (right_text.owned) self.allocator.free(right_text.text);
                        const current_def_name = current_def_blk: {
                            if (self.current_def_symbol) |def_symbol| {
                                const def_entry = self.input.symbols.get(def_symbol);
                                break :current_def_blk if (def_entry.name.isNone()) "<none>" else self.input.idents.getText(def_entry.name);
                            }
                            break :current_def_blk "<none>";
                        };
                        const current_def_raw = if (self.current_def_symbol) |def_symbol| def_symbol.raw() else std.math.maxInt(u32);
                        const current_def_origin = current_def_origin_blk: {
                            if (self.current_def_symbol) |def_symbol| {
                                break :current_def_origin_blk @tagName(self.input.symbols.get(def_symbol).origin);
                            }
                            break :current_def_origin_blk "<none>";
                        };
                        const current_expr_tag = if (self.current_expr_id) |expr_id|
                            @tagName(self.output.getExpr(expr_id).data)
                        else
                            "<none>";
                        const current_expr_raw = if (self.current_expr_id) |expr_id|
                            @intFromEnum(expr_id)
                        else
                            std.math.maxInt(u32);
                        const current_ll_op = if (self.current_expr_id) |expr_id| switch (self.output.getExpr(expr_id).data) {
                            .low_level => |ll| @tagName(ll.op),
                            else => "<none>",
                        } else "<none>";
                        const current_expr_symbol = current_expr_symbol_blk: {
                            if (self.current_expr_id) |expr_id| switch (self.output.getExpr(expr_id).data) {
                                .var_ => |symbol| {
                                    const symbol_entry = self.input.symbols.get(symbol);
                                    break :current_expr_symbol_blk if (symbol_entry.name.isNone())
                                        "<none>"
                                    else
                                        self.input.idents.getText(symbol_entry.name);
                                },
                                else => {},
                            };
                            break :current_expr_symbol_blk "<none>";
                        };
                        const call_func_expr = if (self.current_call_context) |ctx| ctx.func_expr else "<none>";
                        const call_arg_expr = if (self.current_call_context) |ctx| ctx.arg_expr else "<none>";
                        const call_func_ty = if (self.current_call_context) |ctx| ctx.func_ty else "<none>";
                        const call_arg_ty = if (self.current_call_context) |ctx| ctx.arg_ty else "<none>";
                        const call_target_ty = if (self.current_call_context) |ctx| ctx.target_ty else "<none>";
                        std.debug.panic(
                            "lambdasolved.unify incompatible primitives {s} vs {s}\ndef={s}\ndef_raw={d}\ndef_origin={s}\nexpr={s}\nexpr_raw={d}\nexpr_symbol={s}\nll_op={s}\ncall_func_expr={s}\ncall_arg_expr={s}\ncall_func_ty={s}\ncall_arg_ty={s}\ncall_target_ty={s}\nleft={s}\nright={s}",
                            .{
                                @tagName(prim),
                                @tagName(right.primitive),
                                current_def_name,
                                current_def_raw,
                                current_def_origin,
                                current_expr_tag,
                                current_expr_raw,
                                current_expr_symbol,
                                current_ll_op,
                                call_func_expr,
                                call_arg_expr,
                                call_func_ty,
                                call_arg_ty,
                                call_target_ty,
                                left_text.text,
                                right_text.text,
                            },
                        );
                    }
                    unreachable;
                }
                break :blk .{ .content = .{ .primitive = prim } };
            },
            .func => |func| blk: {
                const left_args = self.types.sliceTypeVarSpan(func.args);
                const right_args = self.types.sliceTypeVarSpan(right.func.args);
                if (left_args.len != right_args.len) {
                    return debugPanic("lambdasolved.unify incompatible function arity", .{});
                }
                for (left_args, right_args) |left_arg, right_arg| {
                    try self.unifyRec(left_arg, right_arg, visited);
                }
                try self.unifyRec(func.lset, right.func.lset, visited);
                try self.unifyRec(func.ret, right.func.ret, visited);
                break :blk .{ .content = .{ .func = func } };
            },
            .list => |elem| blk: {
                try self.unifyRec(elem, right.list, visited);
                break :blk .{ .content = .{ .list = elem } };
            },
            .box => |elem| blk: {
                try self.unifyRec(elem, right.box, visited);
                break :blk .{ .content = .{ .box = elem } };
            },
            .tuple => |tuple| blk: {
                const left_elems = self.types.sliceTypeVarSpan(tuple);
                const right_elems = self.types.sliceTypeVarSpan(right.tuple);
                if (left_elems.len != right_elems.len) {
                    return debugPanic("lambdasolved.unify tuple arity mismatch", .{});
                }
                for (left_elems, right_elems) |left_elem, right_elem| {
                    try self.unifyRec(left_elem, right_elem, visited);
                }
                break :blk .{ .content = .{ .tuple = tuple } };
            },
            .tag_union => |tag_union| blk: {
                break :blk .{ .content = .{ .tag_union = .{
                    .tags = try self.unifyTags(tag_union.tags, right.tag_union.tags, visited),
                } } };
            },
            .record => |record| blk: {
                break :blk .{ .content = .{ .record = .{
                    .fields = try self.unifyFields(record.fields, right.record.fields, visited),
                } } };
            },
            .lambda_set => |lambda_set| blk: {
                break :blk .{ .content = .{
                    .lambda_set = try self.unifyLambdaSets(lambda_set, right.lambda_set, visited),
                } };
            },
        };
    }

    fn unifyTags(self: *Lowerer, left_span: type_mod.Span(type_mod.Tag), right_span: type_mod.Span(type_mod.Tag), visited: *std.ArrayList(TypePair)) std.mem.Allocator.Error!type_mod.Span(type_mod.Tag) {
        const left = try self.allocator.dupe(type_mod.Tag, self.types.sliceTags(left_span));
        defer self.allocator.free(left);
        const right = try self.allocator.dupe(type_mod.Tag, self.types.sliceTags(right_span));
        defer self.allocator.free(right);
        var merged = std.ArrayList(type_mod.Tag).empty;
        defer merged.deinit(self.allocator);

        for (left) |left_tag| {
            var matched = false;
            for (right) |right_tag| {
                if (left_tag.name != right_tag.name) continue;
                matched = true;
                const left_args = self.types.sliceTypeVarSpan(left_tag.args);
                const right_args = self.types.sliceTypeVarSpan(right_tag.args);
                if (left_args.len != right_args.len) return debugPanic("lambdasolved.unify tag arity mismatch", .{});
                for (left_args, right_args) |left_arg, right_arg| {
                    try self.unifyRec(left_arg, right_arg, visited);
                }
                try merged.append(self.allocator, left_tag);
                break;
            }
            if (!matched) try merged.append(self.allocator, left_tag);
        }

        for (right) |right_tag| {
            if (!containsTagByName(merged.items, right_tag.name)) {
                try merged.append(self.allocator, right_tag);
            }
        }

        return try self.types.addTags(merged.items);
    }

    fn unifyFields(self: *Lowerer, left_span: type_mod.Span(type_mod.Field), right_span: type_mod.Span(type_mod.Field), visited: *std.ArrayList(TypePair)) std.mem.Allocator.Error!type_mod.Span(type_mod.Field) {
        const left = try self.allocator.dupe(type_mod.Field, self.types.sliceFields(left_span));
        defer self.allocator.free(left);
        const right = try self.allocator.dupe(type_mod.Field, self.types.sliceFields(right_span));
        defer self.allocator.free(right);
        var merged = std.ArrayList(type_mod.Field).empty;
        defer merged.deinit(self.allocator);

        for (left) |left_field| {
            var matched = false;
            for (right) |right_field| {
                if (left_field.name != right_field.name) continue;
                matched = true;
                try self.unifyRec(left_field.ty, right_field.ty, visited);
                try merged.append(self.allocator, left_field);
                break;
            }
            if (!matched) try merged.append(self.allocator, left_field);
        }

        for (right) |right_field| {
            if (!containsFieldByName(merged.items, right_field.name)) {
                try merged.append(self.allocator, right_field);
            }
        }

        sortFieldsByName(&self.input.idents, merged.items);
        return try self.types.addFields(merged.items);
    }

    fn unifyLambdaSets(self: *Lowerer, left_span: type_mod.Span(type_mod.Lambda), right_span: type_mod.Span(type_mod.Lambda), visited: *std.ArrayList(TypePair)) std.mem.Allocator.Error!type_mod.Span(type_mod.Lambda) {
        const left = try self.allocator.dupe(type_mod.Lambda, self.types.sliceLambdas(left_span));
        defer self.allocator.free(left);
        const right = try self.allocator.dupe(type_mod.Lambda, self.types.sliceLambdas(right_span));
        defer self.allocator.free(right);
        var merged = std.ArrayList(type_mod.Lambda).empty;
        defer merged.deinit(self.allocator);

        for (left) |left_lambda| {
            var matched = false;
            for (right) |right_lambda| {
                if (left_lambda.symbol != right_lambda.symbol) continue;
                matched = true;
                const captures = try self.unifyCaptures(left_lambda.captures, right_lambda.captures, visited);
                try merged.append(self.allocator, .{
                    .symbol = left_lambda.symbol,
                    .captures = captures,
                });
                break;
            }
            if (!matched) try merged.append(self.allocator, left_lambda);
        }

        for (right) |right_lambda| {
            if (!containsLambdaBySymbol(merged.items, right_lambda.symbol)) {
                try merged.append(self.allocator, right_lambda);
            }
        }

        return try self.types.addLambdas(merged.items);
    }

    fn unifyCaptures(self: *Lowerer, left_span: type_mod.Span(type_mod.Capture), right_span: type_mod.Span(type_mod.Capture), visited: *std.ArrayList(TypePair)) std.mem.Allocator.Error!type_mod.Span(type_mod.Capture) {
        const left = try self.allocator.dupe(type_mod.Capture, self.types.sliceCaptures(left_span));
        defer self.allocator.free(left);
        const right = try self.allocator.dupe(type_mod.Capture, self.types.sliceCaptures(right_span));
        defer self.allocator.free(right);
        if (left.len != right.len) return debugPanic("lambdasolved.unify incompatible captures", .{});

        var merged = std.ArrayList(type_mod.Capture).empty;
        defer merged.deinit(self.allocator);

        for (left) |left_capture| {
            const right_capture = findCaptureBySymbol(right, left_capture.symbol) orelse return debugPanic("lambdasolved.unify incompatible captures", .{});
            try self.unifyRec(left_capture.ty, right_capture.ty, visited);
            try merged.append(self.allocator, left_capture);
        }

        return try self.types.addCaptures(merged.items);
    }

    fn unlinkToLambdaSet(self: *Lowerer, ty: TypeVarId) ?TypeVarId {
        const id = self.types.unlink(ty);
        return switch (self.types.getNode(id)) {
            .nominal => |nominal| self.unlinkToLambdaSet(nominal.backing),
            .content => |content| switch (content) {
                .func => |func| self.unlinkToLambdaSet(func.lset),
                .lambda_set => id,
                else => null,
            },
            .unbd, .for_a, .flex_for_a => null,
            .link => unreachable,
        };
    }

    fn fixCaptures(self: *Lowerer, venv: []const EnvEntry, ty: TypeVarId, lambda_symbol: Symbol) std.mem.Allocator.Error!void {
        const lset_id = self.unlinkToLambdaSet(ty) orelse return;
        const node = self.types.getNode(self.types.unlink(lset_id));
        const lambda_set = node.content.lambda_set;
        for (self.types.sliceLambdas(lambda_set)) |lambda| {
            if (lambda.symbol != lambda_symbol) continue;
            for (self.types.sliceCaptures(lambda.captures)) |capture| {
                if (self.lookupEnv(venv, capture.symbol)) |env_ty| {
                    try self.unify(capture.ty, env_ty);
                }
            }
            return;
        }
    }
};

fn containsSymbol(symbols: []const Symbol, symbol: Symbol) bool {
    for (symbols) |existing| {
        if (existing == symbol) return true;
    }
    return false;
}

fn containsTagByName(tags: []const type_mod.Tag, name: base.Ident.Idx) bool {
    for (tags) |tag| {
        if (tag.name == name) return true;
    }
    return false;
}

fn findTagByName(tags: []const type_mod.Tag, name: base.Ident.Idx) ?type_mod.Tag {
    for (tags) |tag| {
        if (tag.name == name) return tag;
    }
    return null;
}

fn containsFieldByName(fields: []const type_mod.Field, name: base.Ident.Idx) bool {
    for (fields) |field| {
        if (field.name == name) return true;
    }
    return false;
}

fn findFieldByName(fields: []const type_mod.Field, name: base.Ident.Idx) ?type_mod.Field {
    for (fields) |field| {
        if (field.name == name) return field;
    }
    return null;
}

fn containsLambdaBySymbol(lambdas: []const type_mod.Lambda, symbol: Symbol) bool {
    for (lambdas) |lambda| {
        if (lambda.symbol == symbol) return true;
    }
    return false;
}

fn findLambdaBySymbol(lambdas: []const type_mod.Lambda, symbol: Symbol) ?type_mod.Lambda {
    for (lambdas) |lambda| {
        if (lambda.symbol == symbol) return lambda;
    }
    return null;
}

fn findCaptureBySymbol(captures: []const type_mod.Capture, symbol: Symbol) ?type_mod.Capture {
    for (captures) |capture| {
        if (capture.symbol == symbol) return capture;
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
            .eq => debugPanic("lambdasolved lowered duplicate record field", .{}),
            .gt => debugPanic("lambdasolved lowered record fields were not pre-sorted", .{}),
        }
    }
}

fn sortFieldsByName(idents: *const base.Ident.Store, fields: []type_mod.Field) void {
    std.mem.sort(type_mod.Field, fields, idents, struct {
        fn lessThan(store: *const base.Ident.Store, a: type_mod.Field, b: type_mod.Field) bool {
            return std.mem.lessThan(
                u8,
                store.getText(a.name),
                store.getText(b.name),
            );
        }
    }.lessThan);
}

fn debugPanic(comptime msg: []const u8, args: anytype) noreturn {
    @branchHint(.cold);
    std.debug.panic(msg, args);
}

test "lambdasolved lower tests" {
    std.testing.refAllDecls(@This());
}
