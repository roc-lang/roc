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

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    strings: base.StringLiteral.Store,

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
    }
};

pub fn run(allocator: std.mem.Allocator, input: LiftedResult) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, input);
    defer lowerer.deinit();
    try lowerer.instantiateProgram();
    try lowerer.inferProgram();
    try lowerer.propagateErasure();
    try lowerer.reorderDefsByScc();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: LiftedResult,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    types: type_mod.Store,
    def_id_by_symbol: std.AutoHashMap(Symbol, ast.DefId),
    current_return_ty: ?TypeVarId,

    const EnvEntry = struct {
        symbol: Symbol,
        ty: TypeVarId,
    };

    const TypePair = struct {
        left: TypeVarId,
        right: TypeVarId,
    };

    const SccGroup = struct {
        def_ids: []ast.DefId,
    };

    fn init(allocator: std.mem.Allocator, input: LiftedResult) Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .types = type_mod.Store.init(allocator),
            .def_id_by_symbol = std.AutoHashMap(Symbol, ast.DefId).init(allocator),
            .current_return_ty = null,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.def_id_by_symbol.deinit();
        self.types.deinit();
        self.root_defs.deinit(self.allocator);
        self.output.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .store = self.output,
            .root_defs = self.root_defs,
            .symbols = self.input.symbols,
            .types = self.types,
            .strings = self.input.strings,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.types = type_mod.Store.init(self.allocator);
        self.input.store = LiftedAst.Store.init(self.allocator);
        self.input.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.types = LiftedType.Store.init(self.allocator);
        self.input.strings = .{};
        return result;
    }

    fn instantiateProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            _ = try self.instantiateDef(def);
        }
    }

    fn emitDef(self: *Lowerer, def: ast.Def) std.mem.Allocator.Error!ast.DefId {
        const def_id = try self.output.addDef(def);
        try self.root_defs.append(self.allocator, def_id);
        try self.def_id_by_symbol.put(def.bind.symbol, def_id);
        return def_id;
    }

    fn instantiateDef(self: *Lowerer, def: LiftedAst.Def) std.mem.Allocator.Error!ast.DefId {
        const bind = try self.instantiateTypedSymbol(def.bind);
        return switch (def.value) {
            .fn_ => |fn_def| self.emitDef(.{
                .bind = bind,
                .value = .{ .fn_ = .{
                    .arg = try self.instantiateTypedSymbol(fn_def.arg),
                    .captures = try self.instantiateTypedSymbolSpan(fn_def.captures),
                    .body = try self.instantiateExpr(fn_def.body),
                } },
            }),
            .val => |expr_id| self.emitDef(.{
                .bind = bind,
                .value = .{ .val = try self.instantiateExpr(expr_id) },
            }),
            .run => |run_def| self.emitDef(.{
                .bind = bind,
                .value = .{ .run = .{
                    .body = try self.instantiateExpr(run_def.body),
                    .entry_ty = run_def.entry_ty,
                } },
            }),
        };
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

    fn instantiateExpr(self: *Lowerer, expr_id: LiftedAst.ExprId) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.input.store.getExpr(expr_id);
        const ty = try self.instantiateType(expr.ty);
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
                .args = try self.instantiateExprSpan(tag.args),
            } },
            .record => |fields| .{ .record = try self.instantiateFieldSpan(fields) },
            .access => |access| .{ .access = .{
                .record = try self.instantiateExpr(access.record),
                .field = access.field,
            } },
            .let_ => |let_expr| .{ .let_ = .{
                .bind = try self.instantiateTypedSymbol(let_expr.bind),
                .body = try self.instantiateExpr(let_expr.body),
                .rest = try self.instantiateExpr(let_expr.rest),
            } },
            .call => |call| .{ .call = .{
                .func = try self.instantiateExpr(call.func),
                .arg = try self.instantiateExpr(call.arg),
            } },
            .low_level => |ll| .{ .low_level = .{
                .op = ll.op,
                .args = try self.instantiateExprSpan(ll.args),
            } },
            .when => |when_expr| .{ .when = .{
                .cond = try self.instantiateExpr(when_expr.cond),
                .branches = try self.instantiateBranchSpan(when_expr.branches),
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
        return try self.output.addExpr(.{ .ty = ty, .data = data });
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
            .for_ => |for_stmt| .{ .for_ = .{
                .patt = try self.instantiatePat(for_stmt.patt),
                .iterable = try self.instantiateExpr(for_stmt.iterable),
                .body = try self.instantiateExpr(for_stmt.body),
            } },
        };
        return try self.output.addStmt(lowered);
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

        const mono_ty = self.input.types.getType(ty);
        if (mono_ty == .placeholder) {
            return placeholder;
        }
        if (mono_ty == .tag_union and self.input.types.sliceTags(mono_ty.tag_union.tags).len == 0) {
            return placeholder;
        }

        const content = switch (mono_ty) {
            .placeholder => unreachable,
            .func => |func| blk: {
                break :blk type_mod.Content{ .func = .{
                    .arg = try self.instantiateTypeRec(func.arg, cache),
                    .lset = try self.types.freshUnbd(),
                    .ret = try self.instantiateTypeRec(func.ret, cache),
                } };
            },
                .list => |elem| type_mod.Content{ .list = try self.instantiateTypeRec(elem, cache) },
                .tuple => |tuple| blk: {
                    const elems = self.input.types.sliceTypeSpan(tuple);
                const out = try self.allocator.alloc(TypeVarId, elems.len);
                defer self.allocator.free(out);
                for (elems, 0..) |elem, i| {
                    out[i] = try self.instantiateTypeRec(elem, cache);
                }
                break :blk type_mod.Content{ .tuple = try self.types.addTypeVarSpan(out) };
            },
            .tag_union => |tag_union| blk: {
                const tags = self.input.types.sliceTags(tag_union.tags);
                const out = try self.allocator.alloc(type_mod.Tag, tags.len);
                defer self.allocator.free(out);
                for (tags, 0..) |tag, i| {
                    const arg_ids = self.input.types.sliceTypeSpan(tag.args);
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
                break :blk type_mod.Content{ .tag_union = .{
                    .tags = try self.types.addTags(out),
                } };
            },
            .record => |record| blk: {
                const fields = self.input.types.sliceFields(record.fields);
                const out = try self.allocator.alloc(type_mod.Field, fields.len);
                defer self.allocator.free(out);
                for (fields, 0..) |field, i| {
                    out[i] = .{
                        .name = field.name,
                        .ty = try self.instantiateTypeRec(field.ty, cache),
                    };
                }
                break :blk type_mod.Content{ .record = .{
                    .fields = try self.types.addFields(out),
                } };
            },
            .primitive => |prim| type_mod.Content{ .primitive = prim },
        };

        self.types.setNode(placeholder, .{ .content = content });
        return placeholder;
    }

    fn inferProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        const sccs = try self.computeSccs();
        defer self.freeSccs(sccs);

        var venv: []EnvEntry = &.{};
        for (sccs) |group| {
            if (group.def_ids.len == 1) {
                const def = self.output.getDef(group.def_ids[0]);
                switch (def.value) {
                    .fn_ => |fn_def| {
                        const t_fn = try self.types.freshUnbd();
                        const inferred = try self.inferFn(venv, .{ .symbol = def.bind.symbol, .ty = t_fn }, fn_def);
                        try self.unify(def.bind.ty, inferred);
                        try self.generalize(venv, inferred);
                        const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = inferred });
                        if (venv.len != 0) self.allocator.free(venv);
                        venv = next;
                    },
                    .val => |expr_id| {
                        const inferred = try self.inferExpr(venv, expr_id);
                        try self.unify(def.bind.ty, inferred);
                        const next = try self.extendEnvOne(venv, .{ .symbol = def.bind.symbol, .ty = def.bind.ty });
                        if (venv.len != 0) self.allocator.free(venv);
                        venv = next;
                    },
                    .run => |run_def| {
                        const inferred = try self.inferExpr(venv, run_def.body);
                        try self.unify(def.bind.ty, inferred);
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
                if (def.value != .fn_) return debugPanic("lambdasolved.inferProgram non-function recursive SCC");
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
                generalized[i] = .{
                    .symbol = def.bind.symbol,
                    .ty = try self.inferFn(rec_env, rec_entries[i], fn_def),
                };
            }

            for (group.def_ids, 0..) |def_id, i| {
                const def = self.output.getDef(def_id);
                try self.unify(def.bind.ty, generalized[i].ty);
                try self.generalize(venv, generalized[i].ty);
                generalized[i].ty = def.bind.ty;
            }

            const next = try self.extendEnvMany(venv, generalized);
            if (venv.len != 0) self.allocator.free(venv);
            venv = next;
        }

        if (venv.len != 0) self.allocator.free(venv);
    }

    fn inferFn(self: *Lowerer, venv: []const EnvEntry, fn_entry: EnvEntry, fn_def: ast.FnDef) std.mem.Allocator.Error!TypeVarId {
        const captures = self.output.sliceTypedSymbolSpan(fn_def.captures);
        const captures_env = try self.allocator.alloc(EnvEntry, captures.len + 2);
        defer self.allocator.free(captures_env);
        captures_env[0] = fn_entry;
        captures_env[1] = .{ .symbol = fn_def.arg.symbol, .ty = fn_def.arg.ty };
        for (captures, 0..) |capture, i| {
            captures_env[i + 2] = .{ .symbol = capture.symbol, .ty = capture.ty };
        }

        const body_env = try self.extendEnvMany(venv, captures_env);
        defer self.allocator.free(body_env);

        const fn_ret_ty = try self.types.freshUnbd();
        const previous_return_ty = self.current_return_ty;
        self.current_return_ty = fn_ret_ty;
        defer self.current_return_ty = previous_return_ty;

        const body_ty = try self.inferExpr(body_env, fn_def.body);
        try self.unify(fn_ret_ty, body_ty);

        const lset_captures = try self.allocator.alloc(type_mod.Capture, captures.len);
        defer self.allocator.free(lset_captures);
        for (captures, 0..) |capture, i| {
            lset_captures[i] = .{ .symbol = capture.symbol, .ty = capture.ty };
        }
        const captures_span = try self.types.addCaptures(lset_captures);
        const lambda_span = try self.types.addLambdas(&.{.{ .symbol = fn_entry.symbol, .captures = captures_span }});
        const lset_ty = try self.types.freshContent(.{ .lambda_set = lambda_span });
        const fn_ty = try self.types.freshContent(.{ .func = .{
            .arg = fn_def.arg.ty,
            .lset = lset_ty,
            .ret = fn_ret_ty,
        } });
        try self.unify(fn_entry.ty, fn_ty);
        return fn_entry.ty;
    }

    fn inferExpr(self: *Lowerer, venv: []const EnvEntry, expr_id: ast.ExprId) std.mem.Allocator.Error!TypeVarId {
        const expr = self.output.getExpr(expr_id);
        const target_ty = expr.ty;

        const inferred = switch (expr.data) {
            .var_ => |symbol| blk: {
                const env_ty = self.lookupEnv(venv, symbol) orelse {
                    const entry = self.input.symbols.get(symbol);
                    std.debug.print(
                        "LSET_UNBOUND symbol={d} name={any} origin={any}\n",
                        .{ symbol.raw(), entry.name, entry.origin },
                    );
                    return debugPanic("lambdasolved.inferExpr unbound variable");
                };
                const inst_ty = try self.instantiateGeneralized(env_ty);
                try self.fixCaptures(venv, inst_ty, symbol);
                break :blk inst_ty;
            },
            .int_lit => target_ty,
            .frac_f32_lit => target_ty,
            .frac_f64_lit => target_ty,
            .dec_lit => target_ty,
            .str_lit => target_ty,
            .bool_lit => target_ty,
            .unit => target_ty,
            .tag => |tag| blk: {
                const args = self.output.sliceExprSpan(tag.args);
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
                try self.unify(target_ty, wanted);
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
                const wanted = try self.types.freshContent(.{ .record = .{
                    .fields = try self.types.addFields(field_tys),
                } });
                try self.unify(target_ty, wanted);
                break :blk target_ty;
            },
            .access => |access| blk: {
                const record_ty = try self.inferExpr(venv, access.record);
                const subject_ty = self.output.getExpr(access.record).ty;
                try self.unify(record_ty, subject_ty);
                const subject_root = self.types.unlink(subject_ty);
                const subject_node = self.types.getNode(subject_root);
                const fields = switch (subject_node) {
                    .content => |content| switch (content) {
                        .record => |record| self.types.sliceFields(record.fields),
                        else => return debugPanic("lambdasolved.access invariant violated: subject is not a record"),
                    },
                    else => return debugPanic("lambdasolved.access invariant violated: subject record type is unresolved"),
                };
                const field_ty = for (fields) |field| {
                    if (field.name.eql(access.field)) break field.ty;
                } else return debugPanic("lambdasolved.access invariant violated: missing record field");
                try self.unify(target_ty, field_ty);
                break :blk target_ty;
            },
            .let_ => |let_expr| blk: {
                const body_ty = try self.inferExpr(venv, let_expr.body);
                try self.unify(body_ty, let_expr.bind.ty);
                const rest_env = try self.extendEnvOne(venv, .{ .symbol = let_expr.bind.symbol, .ty = let_expr.bind.ty });
                defer self.allocator.free(rest_env);
                break :blk try self.inferExpr(rest_env, let_expr.rest);
            },
            .call => |call| blk: {
                const func_ty = try self.inferExpr(venv, call.func);
                const arg_ty = try self.inferExpr(venv, call.arg);
                const lset_ty = try self.types.freshUnbd();
                const wanted = try self.types.freshContent(.{ .func = .{
                    .arg = arg_ty,
                    .lset = lset_ty,
                    .ret = target_ty,
                } });
                try self.unify(func_ty, wanted);
                break :blk target_ty;
            },
            .low_level => |ll| blk: {
                for (self.output.sliceExprSpan(ll.args)) |arg| {
                    _ = try self.inferExpr(venv, arg);
                }
                break :blk target_ty;
            },
            .when => |when_expr| blk: {
                const cond_ty = try self.inferExpr(venv, when_expr.cond);
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    const pat_result = try self.inferPat(venv, branch.pat);
                    defer self.allocator.free(pat_result.additions);
                    try self.unify(cond_ty, pat_result.ty);
                    const body_env = try self.extendEnvMany(venv, pat_result.additions);
                    defer self.allocator.free(body_env);
                    const body_ty = try self.inferExpr(body_env, branch.body);
                    try self.unify(target_ty, body_ty);
                }
                break :blk target_ty;
            },
            .if_ => |if_expr| blk: {
                const cond_ty = try self.inferExpr(venv, if_expr.cond);
                try self.unify(cond_ty, try self.freshPrimitiveType(.bool));
                const then_ty = try self.inferExpr(venv, if_expr.then_body);
                const else_ty = try self.inferExpr(venv, if_expr.else_body);
                try self.unify(target_ty, then_ty);
                try self.unify(target_ty, else_ty);
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
                try self.unify(target_ty, wanted);
                break :blk target_ty;
            },
            .tuple_access => |tuple_access| blk: {
                const tuple_ty = try self.inferExpr(venv, tuple_access.tuple);
                const subject_ty = self.output.getExpr(tuple_access.tuple).ty;
                try self.unify(tuple_ty, subject_ty);
                const subject_root = self.types.unlink(subject_ty);
                const subject_node = self.types.getNode(subject_root);
                const elems = switch (subject_node) {
                    .content => |content| switch (content) {
                        .tuple => |span| self.types.sliceTypeVarSpan(span),
                        else => return debugPanic("lambdasolved.tuple_access invariant violated: subject is not a tuple"),
                    },
                    else => return debugPanic("lambdasolved.tuple_access invariant violated: subject tuple type is unresolved"),
                };
                if (tuple_access.elem_index >= elems.len) {
                    return debugPanic("lambdasolved.tuple_access invariant violated: tuple index out of bounds");
                }
                try self.unify(target_ty, elems[tuple_access.elem_index]);
                break :blk target_ty;
            },
            .list => |list| blk: {
                const elem_ty = try self.types.freshUnbd();
                const wanted = try self.types.freshContent(.{ .list = elem_ty });
                try self.unify(target_ty, wanted);
                for (self.output.sliceExprSpan(list)) |item| {
                    const item_ty = try self.inferExpr(venv, item);
                    try self.unify(elem_ty, item_ty);
                }
                break :blk target_ty;
            },
            .return_ => |ret| blk: {
                const return_ty = self.current_return_ty orelse return debugPanic("lambdasolved.return invariant violated: return outside function");
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
                _ = try self.inferExpr(body_env, for_expr.body);
                break :blk target_ty;
            },
        };

        try self.unify(target_ty, inferred);
        return target_ty;
    }

    fn inferStmt(self: *Lowerer, venv: []const EnvEntry, stmt_id: ast.StmtId) std.mem.Allocator.Error![]EnvEntry {
        const stmt = self.output.getStmt(stmt_id);
        return switch (stmt) {
            .decl => |decl| blk: {
                const body_ty = try self.inferExpr(venv, decl.body);
                try self.unify(body_ty, decl.bind.ty);
                break :blk try self.extendEnvOne(venv, .{ .symbol = decl.bind.symbol, .ty = decl.bind.ty });
            },
            .var_decl => |decl| blk: {
                const body_ty = try self.inferExpr(venv, decl.body);
                try self.unify(body_ty, decl.bind.ty);
                break :blk try self.extendEnvOne(venv, .{ .symbol = decl.bind.symbol, .ty = decl.bind.ty });
            },
            .reassign => |reassign| blk: {
                const target_ty = self.lookupEnv(venv, reassign.target) orelse return debugPanic("lambdasolved.inferStmt missing reassign target");
                const body_ty = try self.inferExpr(venv, reassign.body);
                try self.unify(target_ty, body_ty);
                break :blk try self.cloneEnv(venv);
            },
            .expr => |expr_id| blk: {
                _ = try self.inferExpr(venv, expr_id);
                break :blk try self.cloneEnv(venv);
            },
            .debug => |expr_id| blk: {
                _ = try self.inferExpr(venv, expr_id);
                break :blk try self.cloneEnv(venv);
            },
            .expect => |expr_id| blk: {
                _ = try self.inferExpr(venv, expr_id);
                break :blk try self.cloneEnv(venv);
            },
            .crash => try self.cloneEnv(venv),
            .return_ => |expr_id| blk: {
                const return_ty = self.current_return_ty orelse return debugPanic("lambdasolved.return_stmt invariant violated: return outside function");
                const value_ty = try self.inferExpr(venv, expr_id);
                try self.unify(return_ty, value_ty);
                break :blk try self.cloneEnv(venv);
            },
            .for_ => |for_stmt| blk: {
                const pat_result = try self.inferPat(venv, for_stmt.patt);
                defer self.allocator.free(pat_result.additions);
                const iterable_ty = try self.inferExpr(venv, for_stmt.iterable);
                const wanted_iterable = try self.types.freshContent(.{ .list = pat_result.ty });
                try self.unify(iterable_ty, wanted_iterable);
                const body_env = try self.extendEnvMany(venv, pat_result.additions);
                defer self.allocator.free(body_env);
                _ = try self.inferExpr(body_env, for_stmt.body);
                break :blk try self.cloneEnv(venv);
            },
        };
    }

    const InferPatResult = struct {
        additions: []EnvEntry,
        ty: TypeVarId,
    };

    fn inferPat(self: *Lowerer, venv: []const EnvEntry, pat_id: ast.PatId) std.mem.Allocator.Error!InferPatResult {
        _ = venv;
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
                        return debugPanic("lambdasolved.bool pattern invariant violated: bool tags cannot carry arguments");
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

    fn freshPrimitiveType(self: *Lowerer, prim: type_mod.Prim) std.mem.Allocator.Error!TypeVarId {
        return try self.types.freshContent(.{ .primitive = prim });
    }

    fn isPrimitiveBoolType(self: *Lowerer, ty: TypeVarId) std.mem.Allocator.Error!bool {
        const root = self.types.unlink(ty);
        return switch (self.types.getNode(root)) {
            .content => |content| switch (content) {
                .primitive => |prim| prim == .bool,
                else => false,
            },
            .unbd,
            .for_a,
            => false,
            .link => unreachable,
        };
    }

    fn propagateErasure(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.root_defs.items) |def_id| {
            const def = self.output.getDef(def_id);
            switch (def.value) {
                .fn_ => |fn_def| try self.propagateExprErasure(fn_def.body),
                .val => |expr_id| try self.propagateExprErasure(expr_id),
                .run => |run_def| try self.propagateExprErasure(run_def.body),
            }
        }
    }

    fn propagateExprErasure(self: *Lowerer, expr_id: ast.ExprId) std.mem.Allocator.Error!void {
        const expr = self.output.getExpr(expr_id);
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
            .tag => |tag| for (self.output.sliceExprSpan(tag.args)) |arg| try self.propagateExprErasure(arg),
            .record => |fields| for (self.output.sliceFieldExprSpan(fields)) |field| try self.propagateExprErasure(field.value),
            .access => |access| try self.propagateExprErasure(access.record),
            .let_ => |let_expr| {
                try self.propagateExprErasure(let_expr.body);
                try self.propagateExprErasure(let_expr.rest);
            },
            .call => |call| {
                try self.propagateExprErasure(call.func);
                try self.propagateExprErasure(call.arg);
            },
            .low_level => |ll| for (self.output.sliceExprSpan(ll.args)) |arg| try self.propagateExprErasure(arg),
            .when => |when_expr| {
                try self.propagateExprErasure(when_expr.cond);
                for (self.output.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.output.getBranch(branch_id);
                    try self.propagateExprErasure(branch.body);
                }
            },
            .if_ => |if_expr| {
                try self.propagateExprErasure(if_expr.cond);
                try self.propagateExprErasure(if_expr.then_body);
                try self.propagateExprErasure(if_expr.else_body);
            },
            .block => |block| {
                for (self.output.sliceStmtSpan(block.stmts)) |stmt_id| {
                    const stmt = self.output.getStmt(stmt_id);
                    switch (stmt) {
                        .decl => |decl| try self.propagateExprErasure(decl.body),
                        .var_decl => |decl| try self.propagateExprErasure(decl.body),
                        .reassign => |reassign| try self.propagateExprErasure(reassign.body),
                        .expr => |nested| try self.propagateExprErasure(nested),
                        .debug => |nested| try self.propagateExprErasure(nested),
                        .expect => |nested| try self.propagateExprErasure(nested),
                        .crash => {},
                        .return_ => |nested| try self.propagateExprErasure(nested),
                        .for_ => |for_stmt| {
                            try self.propagateExprErasure(for_stmt.iterable);
                            try self.propagateExprErasure(for_stmt.body);
                        },
                    }
                }
                try self.propagateExprErasure(block.final_expr);
            },
            .tuple => |tuple| for (self.output.sliceExprSpan(tuple)) |item| try self.propagateExprErasure(item),
            .tuple_access => |tuple_access| try self.propagateExprErasure(tuple_access.tuple),
            .list => |list| for (self.output.sliceExprSpan(list)) |item| try self.propagateExprErasure(item),
            .return_ => |ret| try self.propagateExprErasure(ret),
            .for_ => |for_expr| {
                try self.propagateExprErasure(for_expr.iterable);
                try self.propagateExprErasure(for_expr.body);
            },
        }
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
            .let_ => |let_expr| {
                try self.collectExprEdges(let_expr.body, edges);
                try self.collectExprEdges(let_expr.rest, edges);
            },
            .call => |call| {
                try self.collectExprEdges(call.func, edges);
                try self.collectExprEdges(call.arg, edges);
            },
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
                        .for_ => |for_stmt| {
                            try self.collectExprEdges(for_stmt.iterable, edges);
                            try self.collectExprEdges(for_stmt.body, edges);
                        },
                    }
                }
                try self.collectExprEdges(block.final_expr, edges);
            },
            .tuple => |tuple| for (self.output.sliceExprSpan(tuple)) |item| try self.collectExprEdges(item, edges),
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

    fn lookupEnv(self: *Lowerer, venv: []const EnvEntry, symbol: Symbol) ?TypeVarId {
        _ = self;
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
            .for_a => true,
            .link => unreachable,
            .content => |content| switch (content) {
                .primitive => false,
                .func => |func| try self.isGeneralizedRec(func.arg, visited) or
                    try self.isGeneralizedRec(func.lset, visited) or
                    try self.isGeneralizedRec(func.ret, visited),
                .list => |elem| try self.isGeneralizedRec(elem, visited),
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
        const id = self.types.unlink(ty);
        if (mapping.get(id)) |cached| return cached;

        switch (self.types.getNode(id)) {
            .unbd => return id,
            .for_a => {
                const fresh = try self.types.freshUnbd();
                try mapping.put(id, fresh);
                return fresh;
            },
            else => {},
        }

        const placeholder = try self.types.freshUnbd();
        try mapping.put(id, placeholder);

        const lowered = switch (self.types.getNode(id)) {
            .unbd, .for_a => unreachable,
            .link => unreachable,
            .content => |content| switch (content) {
                .primitive => type_mod.Node{ .content = .{ .primitive = content.primitive } },
                .func => type_mod.Node{ .content = .{ .func = .{
                    .arg = try self.instantiateGeneralizedRec(content.func.arg, mapping),
                    .lset = try self.instantiateGeneralizedRec(content.func.lset, mapping),
                    .ret = try self.instantiateGeneralizedRec(content.func.ret, mapping),
                } } },
                .list => |elem| type_mod.Node{ .content = .{
                    .list = try self.instantiateGeneralizedRec(elem, mapping),
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
            .unbd, .for_a => false,
            .link => unreachable,
            .content => |content| switch (content) {
                .primitive => false,
                .func => |func| try self.occursRec(needle, func.arg, visited) or
                    try self.occursRec(needle, func.lset, visited) or
                    try self.occursRec(needle, func.ret, visited),
                .list => |elem| try self.occursRec(needle, elem, visited),
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
                    if (self.occurs(id, entry.ty)) return;
                }
                self.types.setNode(id, .for_a);
            },
            .for_a => {},
            .link => unreachable,
            .content => |content| switch (content) {
                .primitive => {},
                .func => |func| {
                    try self.generalizeRec(venv, func.arg, visited);
                    try self.generalizeRec(venv, func.lset, visited);
                    try self.generalizeRec(venv, func.ret, visited);
                },
                .list => |elem| {
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

        const next_node: type_mod.Node = switch (self.types.getNode(l)) {
            .unbd => self.types.getNode(r),
            .for_a => {
                return debugPanic("lambdasolved.unify generalized type without instantiation");
            },
            .link => unreachable,
            .content => |left_content| switch (self.types.getNode(r)) {
                .unbd => type_mod.Node{ .content = left_content },
                .for_a => {
                    return debugPanic("lambdasolved.unify generalized type without instantiation");
                },
                .link => unreachable,
                .content => |right_content| try self.unifyContent(left_content, right_content, visited),
            },
        };

        const merged = try self.types.fresh(next_node);
        self.types.setNode(l, .{ .link = merged });
        self.types.setNode(r, .{ .link = merged });
    }

    fn unifyContent(self: *Lowerer, left: type_mod.Content, right: type_mod.Content, visited: *std.ArrayList(TypePair)) std.mem.Allocator.Error!type_mod.Node {
        if (@as(std.meta.Tag(type_mod.Content), left) != @as(std.meta.Tag(type_mod.Content), right)) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "lambdasolved.unify incompatible types {s} vs {s}",
                    .{
                        @tagName(left),
                        @tagName(right),
                    },
                );
            } else unreachable;
        }

        return switch (left) {
            .primitive => |prim| blk: {
                if (prim != right.primitive) {
                    return debugPanic("lambdasolved.unify incompatible primitives");
                }
                break :blk .{ .content = .{ .primitive = prim } };
            },
            .func => |func| blk: {
                try self.unifyRec(func.arg, right.func.arg, visited);
                try self.unifyRec(func.lset, right.func.lset, visited);
                try self.unifyRec(func.ret, right.func.ret, visited);
                break :blk .{ .content = .{ .func = func } };
            },
            .list => |elem| blk: {
                try self.unifyRec(elem, right.list, visited);
                break :blk .{ .content = .{ .list = elem } };
            },
            .tuple => |tuple| blk: {
                const left_elems = self.types.sliceTypeVarSpan(tuple);
                const right_elems = self.types.sliceTypeVarSpan(right.tuple);
                if (left_elems.len != right_elems.len) {
                    return debugPanic("lambdasolved.unify tuple arity mismatch");
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
                if (left_args.len != right_args.len) return debugPanic("lambdasolved.unify tag arity mismatch");
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
        if (left.len != right.len) return debugPanic("lambdasolved.unify incompatible captures");

        var merged = std.ArrayList(type_mod.Capture).empty;
        defer merged.deinit(self.allocator);

        for (left) |left_capture| {
            const right_capture = findCaptureBySymbol(right, left_capture.symbol) orelse return debugPanic("lambdasolved.unify incompatible captures");
            try self.unifyRec(left_capture.ty, right_capture.ty, visited);
            try merged.append(self.allocator, left_capture);
        }

        return try self.types.addCaptures(merged.items);
    }

    fn unlinkToLambdaSet(self: *Lowerer, ty: TypeVarId) ?TypeVarId {
        const id = self.types.unlink(ty);
        return switch (self.types.getNode(id)) {
            .content => |content| switch (content) {
                .func => |func| self.unlinkToLambdaSet(func.lset),
                .lambda_set => id,
                else => null,
            },
            else => null,
        };
    }

    fn fixCaptures(self: *Lowerer, venv: []const EnvEntry, ty: TypeVarId, lambda_symbol: Symbol) std.mem.Allocator.Error!void {
        const lset_id = self.unlinkToLambdaSet(ty) orelse return;
        const node = self.types.getNode(self.types.unlink(lset_id));
        const lambda_set = node.content.lambda_set;
        for (self.types.sliceLambdas(lambda_set)) |lambda| {
            if (lambda.symbol != lambda_symbol) continue;
            for (self.types.sliceCaptures(lambda.captures)) |capture| {
                const env_ty = self.lookupEnv(venv, capture.symbol) orelse return debugPanic("lambdasolved.fixCaptures missing capture binding");
                try self.unify(capture.ty, env_ty);
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

fn containsFieldByName(fields: []const type_mod.Field, name: base.Ident.Idx) bool {
    for (fields) |field| {
        if (field.name == name) return true;
    }
    return false;
}

fn containsLambdaBySymbol(lambdas: []const type_mod.Lambda, symbol: Symbol) bool {
    for (lambdas) |lambda| {
        if (lambda.symbol == symbol) return true;
    }
    return false;
}

fn findCaptureBySymbol(captures: []const type_mod.Capture, symbol: Symbol) ?type_mod.Capture {
    for (captures) |capture| {
        if (capture.symbol == symbol) return capture;
    }
    return null;
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

fn debugTodoLowLevel(op: base.LowLevel) noreturn {
    @branchHint(.cold);
    std.debug.panic("TODO lambdasolved low-level op {s}", .{@tagName(op)});
}

test "lambdasolved lower tests" {
    std.testing.refAllDecls(@This());
}
