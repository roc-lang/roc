//! Lambda lifting from monotype to monotype_lifted.
//!
//! This follows `cor`'s strategy:
//! - traverse one monomorphic program
//! - compute closure/letfn free variables directly from the monotype AST
//! - lift every closure and local letfn into a top-level function def
//! - thread only the rename environment needed for lifted local letfns
//!
//! Roc-only constructs that `cor` does not have yet remain explicit extensions:
//! mutable statements, blocks, loops, returns, runtime errors, and low-level ops.

const std = @import("std");
const base = @import("base");
const mono = @import("../monotype/mod.zig");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("../symbol/mod.zig");

const MonoResult = mono.Lower.Result;
const MonoAst = mono.Ast;
const Symbol = symbol_mod.Symbol;

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.types.deinit();
    }
};

pub fn run(allocator: std.mem.Allocator, input: MonoResult) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, input);
    defer lowerer.deinit();
    try lowerer.collectTopLevels();
    try lowerer.lowerProgram();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: MonoResult,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    top_levels: std.AutoHashMap(Symbol, void),
    binding_types: std.AutoHashMap(Symbol, type_mod.TypeId),

    const Rename = struct {
        from: Symbol,
        to: Symbol,
    };

    const LoweredExpr = struct {
        expr: ast.ExprId,
        lifted_defs: std.ArrayList(ast.Def),

        fn init(allocator: std.mem.Allocator) LoweredExpr {
            return .{
                .expr = undefined,
                .lifted_defs = std.ArrayList(ast.Def).init(allocator),
            };
        }

        fn deinit(self: *LoweredExpr, allocator: std.mem.Allocator) void {
            self.lifted_defs.deinit(allocator);
        }
    };

    fn init(allocator: std.mem.Allocator, input: MonoResult) Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .top_levels = std.AutoHashMap(Symbol, void).init(allocator),
            .binding_types = std.AutoHashMap(Symbol, type_mod.TypeId).init(allocator),
        };
    }

    fn deinit(self: *Lowerer) void {
        self.binding_types.deinit();
        self.top_levels.deinit();
        self.root_defs.deinit(self.allocator);
        self.output.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .store = self.output,
            .root_defs = self.root_defs,
            .symbols = self.input.symbols,
            .types = self.input.types,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.types = type_mod.Store.init(self.allocator);
        self.input.program = mono.Lower.Program.init(self.allocator);
        return result;
    }

    fn collectTopLevels(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.program.store.defsSlice()) |def| {
            try self.top_levels.put(def.bind.symbol, {});
            try self.binding_types.put(def.bind.symbol, def.bind.ty);
            switch (def.value) {
                .fn_ => |fn_def| {
                    try self.binding_types.put(fn_def.arg.symbol, fn_def.arg.ty);
                    try self.collectBindingTypesExpr(fn_def.body);
                },
                .val => |expr_id| try self.collectBindingTypesExpr(expr_id),
                .run => |run_def| try self.collectBindingTypesExpr(run_def.body),
            }
        }
    }

    fn lowerProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.program.store.defsSlice()) |def| {
            _ = try self.lowerDef(def);
        }
    }

    fn emitDef(self: *Lowerer, def: ast.Def) std.mem.Allocator.Error!ast.DefId {
        const def_id = try self.output.addDef(def);
        try self.root_defs.append(self.allocator, def_id);
        return def_id;
    }

    fn emitLiftedDefs(self: *Lowerer, defs: []const ast.Def) std.mem.Allocator.Error!void {
        for (defs) |def| {
            _ = try self.emitDef(def);
        }
    }

    fn lowerDef(self: *Lowerer, def: MonoAst.Def) std.mem.Allocator.Error!ast.DefId {
        const empty_venv: []const Rename = &.{};
        return switch (def.value) {
            .fn_ => |fn_def| blk: {
                var lowered_body = try self.lowerExpr(empty_venv, fn_def.body);
                defer lowered_body.deinit(self.allocator);
                try self.emitLiftedDefs(lowered_body.lifted_defs.items);
                break :blk try self.emitDef(.{
                    .bind = def.bind,
                    .value = .{ .fn_ = .{
                        .arg = fn_def.arg,
                        .captures = try self.output.addTypedSymbolSpan(&.{}),
                        .body = lowered_body.expr,
                    } },
                });
            },
            .val => |expr_id| blk: {
                var lowered_expr = try self.lowerExpr(empty_venv, expr_id);
                defer lowered_expr.deinit(self.allocator);
                try self.emitLiftedDefs(lowered_expr.lifted_defs.items);
                break :blk try self.emitDef(.{
                    .bind = def.bind,
                    .value = .{ .val = lowered_expr.expr },
                });
            },
            .run => |run_def| blk: {
                var lowered_expr = try self.lowerExpr(empty_venv, run_def.body);
                defer lowered_expr.deinit(self.allocator);
                try self.emitLiftedDefs(lowered_expr.lifted_defs.items);
                break :blk try self.emitDef(.{
                    .bind = def.bind,
                    .value = .{ .run = .{
                        .body = lowered_expr.expr,
                        .entry_ty = run_def.entry_ty,
                    } },
                });
            },
        };
    }

    fn lowerExpr(self: *Lowerer, venv: []const Rename, expr_id: MonoAst.ExprId) std.mem.Allocator.Error!LoweredExpr {
        const expr = self.input.program.store.getExpr(expr_id);
        var lowered = LoweredExpr.init(self.allocator);
        errdefer lowered.deinit(self.allocator);

        switch (expr.data) {
            .var_ => |symbol| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .var_ = self.lookupRenamedSymbol(venv, symbol) },
                });
            },
            .int_lit => |value| lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .int_lit = value } }),
            .frac_f32_lit => |value| lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .frac_f32_lit = value } }),
            .frac_f64_lit => |value| lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .frac_f64_lit = value } }),
            .dec_lit => |value| lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .dec_lit = value } }),
            .str_lit => |value| lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .str_lit = value } }),
            .tag => |tag| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .tag = .{
                        .name = tag.name,
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, tag.args),
                    } },
                });
            },
            .record => |fields| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .record = try self.lowerFieldSpan(&lowered.lifted_defs, venv, fields) },
                });
            },
            .access => |access| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .access = .{
                        .record = try self.lowerExprInto(&lowered.lifted_defs, venv, access.record),
                        .field = access.field,
                    } },
                });
            },
            .let_ => |let_expr| switch (let_expr.def) {
                .let_val => |let_val| {
                    const body = try self.lowerExprInto(&lowered.lifted_defs, venv, let_val.body);
                    const rest = try self.lowerExprInto(&lowered.lifted_defs, venv, let_expr.rest);
                    lowered.expr = try self.output.addExpr(.{
                        .ty = expr.ty,
                        .data = .{ .let_ = .{
                            .bind = let_val.bind,
                            .body = body,
                            .rest = rest,
                        } },
                    });
                },
                .let_fn => |let_fn| {
                    const lifted_symbol = try self.freshLiftedLocalFnSymbol(let_fn.bind.symbol);
                    const recursive_venv = try self.extendRename(venv, let_fn.bind.symbol, lifted_symbol);
                    defer self.allocator.free(recursive_venv);

                    const captures = try self.computeCaptures(recursive_venv, &.{ let_fn.bind.symbol, let_fn.arg.symbol }, let_fn.body);
                    defer self.allocator.free(captures);

                    var lowered_body = try self.lowerExpr(recursive_venv, let_fn.body);
                    defer lowered_body.deinit(self.allocator);
                    try lowered.lifted_defs.appendSlice(self.allocator, lowered_body.lifted_defs.items);

                    try lowered.lifted_defs.append(self.allocator, .{
                        .bind = .{ .ty = expr.ty, .symbol = lifted_symbol },
                        .value = .{ .fn_ = .{
                            .arg = let_fn.arg,
                            .captures = try self.output.addTypedSymbolSpan(captures),
                            .body = lowered_body.expr,
                        } },
                    });

                    if (captures.len == 0) {
                        lowered.expr = try self.lowerExprInto(&lowered.lifted_defs, recursive_venv, let_expr.rest);
                    } else {
                        const rest = try self.lowerExprInto(&lowered.lifted_defs, venv, let_expr.rest);
                        const lifted_ref = try self.output.addExpr(.{
                            .ty = let_fn.bind.ty,
                            .data = .{ .var_ = lifted_symbol },
                        });
                        lowered.expr = try self.output.addExpr(.{
                            .ty = expr.ty,
                            .data = .{ .let_ = .{
                                .bind = let_fn.bind,
                                .body = lifted_ref,
                                .rest = rest,
                            } },
                        });
                    }
                },
            },
            .clos => |clos| {
                const captures = try self.computeCaptures(venv, &.{clos.arg.symbol}, clos.body);
                defer self.allocator.free(captures);

                var lowered_body = try self.lowerExpr(venv, clos.body);
                defer lowered_body.deinit(self.allocator);
                try lowered.lifted_defs.appendSlice(self.allocator, lowered_body.lifted_defs.items);

                const lifted_symbol = try self.freshLiftedClosureSymbol();
                try lowered.lifted_defs.append(self.allocator, .{
                    .bind = .{ .ty = expr.ty, .symbol = lifted_symbol },
                    .value = .{ .fn_ = .{
                        .arg = clos.arg,
                        .captures = try self.output.addTypedSymbolSpan(captures),
                        .body = lowered_body.expr,
                    } },
                });
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .var_ = lifted_symbol },
                });
            },
            .call => |call| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .call = .{
                        .func = try self.lowerExprInto(&lowered.lifted_defs, venv, call.func),
                        .arg = try self.lowerExprInto(&lowered.lifted_defs, venv, call.arg),
                    } },
                });
            },
            .low_level => |ll| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .low_level = .{
                        .op = ll.op,
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, ll.args),
                    } },
                });
            },
            .when => |when_expr| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .when = .{
                        .cond = try self.lowerExprInto(&lowered.lifted_defs, venv, when_expr.cond),
                        .branches = try self.lowerBranchSpan(&lowered.lifted_defs, venv, when_expr.branches),
                    } },
                });
            },
            .if_ => |if_expr| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .if_ = .{
                        .cond = try self.lowerExprInto(&lowered.lifted_defs, venv, if_expr.cond),
                        .then_body = try self.lowerExprInto(&lowered.lifted_defs, venv, if_expr.then_body),
                        .else_body = try self.lowerExprInto(&lowered.lifted_defs, venv, if_expr.else_body),
                    } },
                });
            },
            .block => |block| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .block = .{
                        .stmts = try self.lowerStmtSpan(&lowered.lifted_defs, venv, block.stmts),
                        .final_expr = try self.lowerExprInto(&lowered.lifted_defs, venv, block.final_expr),
                    } },
                });
            },
            .tuple => |tuple| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .tuple = try self.lowerExprSpan(&lowered.lifted_defs, venv, tuple) },
                });
            },
            .tuple_access => |tuple_access| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .tuple_access = .{
                        .tuple = try self.lowerExprInto(&lowered.lifted_defs, venv, tuple_access.tuple),
                        .elem_index = tuple_access.elem_index,
                    } },
                });
            },
            .list => |list| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .list = try self.lowerExprSpan(&lowered.lifted_defs, venv, list) },
                });
            },
            .unit => lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .unit }),
            .return_ => |ret| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .return_ = try self.lowerExprInto(&lowered.lifted_defs, venv, ret) },
                });
            },
            .runtime_error => |msg| {
                lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .runtime_error = msg } });
            },
            .for_ => |for_expr| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .for_ = .{
                        .patt = try self.lowerPat(for_expr.patt),
                        .iterable = try self.lowerExprInto(&lowered.lifted_defs, venv, for_expr.iterable),
                        .body = try self.lowerExprInto(&lowered.lifted_defs, venv, for_expr.body),
                    } },
                });
            },
        }

        return lowered;
    }

    fn lowerExprInto(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        expr_id: MonoAst.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var lowered = try self.lowerExpr(venv, expr_id);
        defer lowered.deinit(self.allocator);
        try lifted_defs.appendSlice(self.allocator, lowered.lifted_defs.items);
        return lowered.expr;
    }

    fn lowerExprSpan(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        span: MonoAst.Span(MonoAst.ExprId),
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const exprs = self.input.program.store.sliceExprSpan(span);
        const lowered = try self.allocator.alloc(ast.ExprId, exprs.len);
        defer self.allocator.free(lowered);
        for (exprs, 0..) |expr_id, i| {
            lowered[i] = try self.lowerExprInto(lifted_defs, venv, expr_id);
        }
        return try self.output.addExprSpan(lowered);
    }

    fn lowerFieldSpan(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        span: MonoAst.Span(MonoAst.FieldExpr),
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const fields = self.input.program.store.sliceFieldExprSpan(span);
        const lowered = try self.allocator.alloc(ast.FieldExpr, fields.len);
        defer self.allocator.free(lowered);
        for (fields, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .value = try self.lowerExprInto(lifted_defs, venv, field.value),
            };
        }
        return try self.output.addFieldExprSpan(lowered);
    }

    fn lowerBranchSpan(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        span: MonoAst.Span(MonoAst.BranchId),
    ) std.mem.Allocator.Error!ast.Span(ast.BranchId) {
        const branch_ids = self.input.program.store.sliceBranchSpan(span);
        const lowered = try self.allocator.alloc(ast.Branch, branch_ids.len);
        defer self.allocator.free(lowered);
        for (branch_ids, 0..) |branch_id, i| {
            const branch = self.input.program.store.getBranch(branch_id);
            lowered[i] = .{
                .pat = try self.lowerPat(branch.pat),
                .body = try self.lowerExprInto(lifted_defs, venv, branch.body),
            };
        }
        return try self.output.addBranchSpan(lowered);
    }

    fn lowerStmtSpan(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        span: MonoAst.Span(MonoAst.StmtId),
    ) std.mem.Allocator.Error!ast.Span(ast.StmtId) {
        const stmt_ids = self.input.program.store.sliceStmtSpan(span);
        const lowered = try self.allocator.alloc(ast.StmtId, stmt_ids.len);
        defer self.allocator.free(lowered);
        for (stmt_ids, 0..) |stmt_id, i| {
            lowered[i] = try self.lowerStmt(lifted_defs, venv, stmt_id);
        }
        return try self.output.addStmtSpan(lowered);
    }

    fn lowerStmt(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        stmt_id: MonoAst.StmtId,
    ) std.mem.Allocator.Error!ast.StmtId {
        const stmt = self.input.program.store.getStmt(stmt_id);
        const lowered_stmt: ast.Stmt = switch (stmt) {
            .decl => |decl| .{ .decl = .{
                .bind = decl.bind,
                .body = try self.lowerExprInto(lifted_defs, venv, decl.body),
            } },
            .var_decl => |decl| .{ .var_decl = .{
                .bind = decl.bind,
                .body = try self.lowerExprInto(lifted_defs, venv, decl.body),
            } },
            .reassign => |reassign| .{ .reassign = .{
                .target = self.lookupRenamedSymbol(venv, reassign.target),
                .body = try self.lowerExprInto(lifted_defs, venv, reassign.body),
            } },
            .expr => |expr_id| .{ .expr = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .expect => |expr_id| .{ .expect = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .crash => |msg| .{ .crash = msg },
            .return_ => |expr_id| .{ .return_ = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .for_ => |for_stmt| .{ .for_ = .{
                .patt = try self.lowerPat(for_stmt.patt),
                .iterable = try self.lowerExprInto(lifted_defs, venv, for_stmt.iterable),
                .body = try self.lowerExprInto(lifted_defs, venv, for_stmt.body),
            } },
        };
        return try self.output.addStmt(lowered_stmt);
    }

    fn lowerPat(self: *Lowerer, pat_id: MonoAst.PatId) std.mem.Allocator.Error!ast.PatId {
        const pat = self.input.program.store.getPat(pat_id);
        return switch (pat.data) {
            .var_ => |symbol| try self.output.addPat(.{
                .ty = pat.ty,
                .data = .{ .var_ = symbol },
            }),
            .tag => |tag| blk: {
                const args = self.input.program.store.slicePatSpan(tag.args);
                const lowered = try self.allocator.alloc(ast.PatId, args.len);
                defer self.allocator.free(lowered);
                for (args, 0..) |arg_id, i| {
                    lowered[i] = try self.lowerPat(arg_id);
                }
                break :blk try self.output.addPat(.{
                    .ty = pat.ty,
                    .data = .{ .tag = .{
                        .name = tag.name,
                        .args = try self.output.addPatSpan(lowered),
                    } },
                });
            },
        };
    }

    fn computeCaptures(
        self: *Lowerer,
        venv: []const Rename,
        initially_bound: []const Symbol,
        body_expr: MonoAst.ExprId,
    ) std.mem.Allocator.Error![]ast.TypedSymbol {
        var bound = std.AutoHashMap(Symbol, void).init(self.allocator);
        defer bound.deinit();
        for (initially_bound) |symbol| {
            if (!symbol.isNone()) {
                try bound.put(symbol, {});
            }
        }

        var free = std.AutoHashMap(Symbol, type_mod.TypeId).init(self.allocator);
        defer free.deinit();
        try self.collectFreeVarsExpr(body_expr, &bound, &free);

        var captures_map = std.AutoHashMap(Symbol, type_mod.TypeId).init(self.allocator);
        defer captures_map.deinit();

        var free_iter = free.iterator();
        while (free_iter.next()) |entry| {
            const symbol = self.lookupRenamedSymbol(venv, entry.key_ptr.*);
            if (self.top_levels.contains(symbol)) continue;
            try captures_map.put(symbol, entry.value_ptr.*);
        }

        const captures = try self.allocator.alloc(ast.TypedSymbol, captures_map.count());
        errdefer self.allocator.free(captures);

        var captures_iter = captures_map.iterator();
        var i: usize = 0;
        while (captures_iter.next()) |entry| : (i += 1) {
            captures[i] = .{
                .ty = entry.value_ptr.*,
                .symbol = entry.key_ptr.*,
            };
        }

        self.sortTypedSymbols(captures);
        return captures;
    }

    fn collectFreeVarsExpr(
        self: *Lowerer,
        expr_id: MonoAst.ExprId,
        bound: *std.AutoHashMap(Symbol, void),
        free: *std.AutoHashMap(Symbol, type_mod.TypeId),
    ) std.mem.Allocator.Error!void {
        const expr = self.input.program.store.getExpr(expr_id);
        switch (expr.data) {
            .var_ => |symbol| {
                if (!symbol.isNone() and !bound.contains(symbol)) {
                    try free.put(symbol, expr.ty);
                }
            },
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .unit,
            .runtime_error,
            => {},
            .tag => |tag| for (self.input.program.store.sliceExprSpan(tag.args)) |arg| try self.collectFreeVarsExpr(arg, bound, free),
            .record => |fields| {
                for (self.input.program.store.sliceFieldExprSpan(fields)) |field| {
                    try self.collectFreeVarsExpr(field.value, bound, free);
                }
            },
            .access => |access| try self.collectFreeVarsExpr(access.record, bound, free),
            .let_ => |let_expr| switch (let_expr.def) {
                .let_val => |let_val| {
                    try self.collectFreeVarsExpr(let_val.body, bound, free);
                    const inserted = try self.bindTemporarily(bound, let_val.bind.symbol);
                    defer self.unbindTemporarily(bound, let_val.bind.symbol, inserted);
                    try self.collectFreeVarsExpr(let_expr.rest, bound, free);
                },
                .let_fn => |let_fn| {
                    const inserted_bind = try self.bindTemporarily(bound, let_fn.bind.symbol);
                    defer self.unbindTemporarily(bound, let_fn.bind.symbol, inserted_bind);

                    const inserted_arg = try self.bindTemporarily(bound, let_fn.arg.symbol);
                    defer self.unbindTemporarily(bound, let_fn.arg.symbol, inserted_arg);

                    try self.collectFreeVarsExpr(let_fn.body, bound, free);
                    self.unbindTemporarily(bound, let_fn.arg.symbol, inserted_arg);
                    try self.collectFreeVarsExpr(let_expr.rest, bound, free);
                },
            },
            .clos => |clos| {
                const inserted_arg = try self.bindTemporarily(bound, clos.arg.symbol);
                defer self.unbindTemporarily(bound, clos.arg.symbol, inserted_arg);
                try self.collectFreeVarsExpr(clos.body, bound, free);
            },
            .call => |call| {
                try self.collectFreeVarsExpr(call.func, bound, free);
                try self.collectFreeVarsExpr(call.arg, bound, free);
            },
            .low_level => |ll| for (self.input.program.store.sliceExprSpan(ll.args)) |arg| try self.collectFreeVarsExpr(arg, bound, free),
            .when => |when_expr| {
                try self.collectFreeVarsExpr(when_expr.cond, bound, free);
                for (self.input.program.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.input.program.store.getBranch(branch_id);
                    var added = std.ArrayList(Symbol).empty;
                    defer added.deinit(self.allocator);
                    try self.bindPatSymbols(branch.pat, bound, &added);
                    try self.collectFreeVarsExpr(branch.body, bound, free);
                    for (added.items) |symbol| _ = bound.remove(symbol);
                }
            },
            .if_ => |if_expr| {
                try self.collectFreeVarsExpr(if_expr.cond, bound, free);
                try self.collectFreeVarsExpr(if_expr.then_body, bound, free);
                try self.collectFreeVarsExpr(if_expr.else_body, bound, free);
            },
            .block => |block| {
                for (self.input.program.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    try self.collectFreeVarsStmt(stmt_id, bound, free);
                }
                try self.collectFreeVarsExpr(block.final_expr, bound, free);
            },
            .tuple => |tuple| for (self.input.program.store.sliceExprSpan(tuple)) |arg| try self.collectFreeVarsExpr(arg, bound, free),
            .tuple_access => |tuple_access| try self.collectFreeVarsExpr(tuple_access.tuple, bound, free),
            .list => |list| for (self.input.program.store.sliceExprSpan(list)) |arg| try self.collectFreeVarsExpr(arg, bound, free),
            .return_ => |ret| try self.collectFreeVarsExpr(ret, bound, free),
            .for_ => |for_expr| {
                try self.collectFreeVarsExpr(for_expr.iterable, bound, free);
                var added = std.ArrayList(Symbol).empty;
                defer added.deinit(self.allocator);
                try self.bindPatSymbols(for_expr.patt, bound, &added);
                try self.collectFreeVarsExpr(for_expr.body, bound, free);
                for (added.items) |symbol| _ = bound.remove(symbol);
            },
        }
    }

    fn collectFreeVarsStmt(
        self: *Lowerer,
        stmt_id: MonoAst.StmtId,
        bound: *std.AutoHashMap(Symbol, void),
        free: *std.AutoHashMap(Symbol, type_mod.TypeId),
    ) std.mem.Allocator.Error!void {
        const stmt = self.input.program.store.getStmt(stmt_id);
        switch (stmt) {
            .decl => |decl| {
                try self.collectFreeVarsExpr(decl.body, bound, free);
                _ = try self.bindTemporarily(bound, decl.bind.symbol);
            },
            .var_decl => |decl| {
                try self.collectFreeVarsExpr(decl.body, bound, free);
                _ = try self.bindTemporarily(bound, decl.bind.symbol);
            },
            .reassign => |reassign| {
                if (!reassign.target.isNone() and !bound.contains(reassign.target)) {
                    try free.put(reassign.target, self.lookupTypeForSymbol(reassign.target));
                }
                try self.collectFreeVarsExpr(reassign.body, bound, free);
            },
            .expr => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .expect => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .crash => {},
            .return_ => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .for_ => |for_stmt| {
                try self.collectFreeVarsExpr(for_stmt.iterable, bound, free);
                var added = std.ArrayList(Symbol).empty;
                defer added.deinit(self.allocator);
                try self.bindPatSymbols(for_stmt.patt, bound, &added);
                try self.collectFreeVarsExpr(for_stmt.body, bound, free);
                for (added.items) |symbol| _ = bound.remove(symbol);
            },
        }
    }

    fn bindPatSymbols(
        self: *Lowerer,
        pat_id: MonoAst.PatId,
        bound: *std.AutoHashMap(Symbol, void),
        added: *std.ArrayList(Symbol),
    ) std.mem.Allocator.Error!void {
        const pat = self.input.program.store.getPat(pat_id);
        switch (pat.data) {
            .var_ => |symbol| {
                if (!symbol.isNone() and !bound.contains(symbol)) {
                    try bound.put(symbol, {});
                    try added.append(self.allocator, symbol);
                }
            },
            .tag => |tag| {
                for (self.input.program.store.slicePatSpan(tag.args)) |arg_pat| {
                    try self.bindPatSymbols(arg_pat, bound, added);
                }
            },
        }
    }

    fn lookupRenamedSymbol(self: *Lowerer, venv: []const Rename, symbol: Symbol) Symbol {
        _ = self;
        var i = venv.len;
        while (i > 0) {
            i -= 1;
            const rename = venv[i];
            if (rename.from == symbol) return rename.to;
        }
        return symbol;
    }

    fn extendRename(self: *Lowerer, venv: []const Rename, from: Symbol, to: Symbol) std.mem.Allocator.Error![]Rename {
        const next = try self.allocator.alloc(Rename, venv.len + 1);
        std.mem.copyForwards(Rename, next[0..venv.len], venv);
        next[venv.len] = .{ .from = from, .to = to };
        return next;
    }

    fn bindTemporarily(self: *Lowerer, bound: *std.AutoHashMap(Symbol, void), symbol: Symbol) std.mem.Allocator.Error!bool {
        _ = self;
        if (symbol.isNone()) return false;
        if (bound.contains(symbol)) return false;
        try bound.put(symbol, {});
        return true;
    }

    fn unbindTemporarily(self: *Lowerer, bound: *std.AutoHashMap(Symbol, void), symbol: Symbol, inserted: bool) void {
        _ = self;
        if (inserted) {
            _ = bound.remove(symbol);
        }
    }

    fn freshLiftedLocalFnSymbol(self: *Lowerer, source_symbol: Symbol) std.mem.Allocator.Error!Symbol {
        const entry = self.input.symbols.get(source_symbol);
        const symbol = try self.input.symbols.add(entry.name, .{
            .lifted_local_fn = .{ .source_symbol = source_symbol.raw() },
        });
        try self.top_levels.put(symbol, {});
        try self.binding_types.put(symbol, self.lookupTypeForSymbol(source_symbol));
        return symbol;
    }

    fn freshLiftedClosureSymbol(self: *Lowerer) std.mem.Allocator.Error!Symbol {
        const symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
        try self.top_levels.put(symbol, {});
        return symbol;
    }

    fn lookupTypeForSymbol(self: *Lowerer, symbol: Symbol) type_mod.TypeId {
        if (self.binding_types.get(symbol)) |ty| {
            return ty;
        }
        return debugPanic("monotype_lifted.lookupTypeForSymbol missing symbol type");
    }

    fn collectBindingTypesExpr(self: *Lowerer, expr_id: MonoAst.ExprId) std.mem.Allocator.Error!void {
        const expr = self.input.program.store.getExpr(expr_id);
        switch (expr.data) {
            .var_,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .unit,
            .runtime_error,
            => {},
            .tag => |tag| for (self.input.program.store.sliceExprSpan(tag.args)) |arg| try self.collectBindingTypesExpr(arg),
            .record => |fields| {
                for (self.input.program.store.sliceFieldExprSpan(fields)) |field| {
                    try self.collectBindingTypesExpr(field.value);
                }
            },
            .access => |access| try self.collectBindingTypesExpr(access.record),
            .let_ => |let_expr| switch (let_expr.def) {
                .let_val => |let_val| {
                    try self.binding_types.put(let_val.bind.symbol, let_val.bind.ty);
                    try self.collectBindingTypesExpr(let_val.body);
                    try self.collectBindingTypesExpr(let_expr.rest);
                },
                .let_fn => |let_fn| {
                    try self.binding_types.put(let_fn.bind.symbol, let_fn.bind.ty);
                    try self.binding_types.put(let_fn.arg.symbol, let_fn.arg.ty);
                    try self.collectBindingTypesExpr(let_fn.body);
                    try self.collectBindingTypesExpr(let_expr.rest);
                },
            },
            .clos => |clos| {
                try self.binding_types.put(clos.arg.symbol, clos.arg.ty);
                try self.collectBindingTypesExpr(clos.body);
            },
            .call => |call| {
                try self.collectBindingTypesExpr(call.func);
                try self.collectBindingTypesExpr(call.arg);
            },
            .low_level => |ll| for (self.input.program.store.sliceExprSpan(ll.args)) |arg| try self.collectBindingTypesExpr(arg),
            .when => |when_expr| {
                try self.collectBindingTypesExpr(when_expr.cond);
                for (self.input.program.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.input.program.store.getBranch(branch_id);
                    try self.collectBindingTypesExpr(branch.body);
                }
            },
            .if_ => |if_expr| {
                try self.collectBindingTypesExpr(if_expr.cond);
                try self.collectBindingTypesExpr(if_expr.then_body);
                try self.collectBindingTypesExpr(if_expr.else_body);
            },
            .block => |block| {
                for (self.input.program.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    try self.collectBindingTypesStmt(stmt_id);
                }
                try self.collectBindingTypesExpr(block.final_expr);
            },
            .tuple => |tuple| for (self.input.program.store.sliceExprSpan(tuple)) |arg| try self.collectBindingTypesExpr(arg),
            .tuple_access => |tuple_access| try self.collectBindingTypesExpr(tuple_access.tuple),
            .list => |list| for (self.input.program.store.sliceExprSpan(list)) |arg| try self.collectBindingTypesExpr(arg),
            .return_ => |ret| try self.collectBindingTypesExpr(ret),
            .for_ => |for_expr| {
                try self.collectBindingTypesExpr(for_expr.iterable);
                try self.collectBindingTypesExpr(for_expr.body);
            },
        }
    }

    fn collectBindingTypesStmt(self: *Lowerer, stmt_id: MonoAst.StmtId) std.mem.Allocator.Error!void {
        const stmt = self.input.program.store.getStmt(stmt_id);
        switch (stmt) {
            .decl => |decl| {
                try self.binding_types.put(decl.bind.symbol, decl.bind.ty);
                try self.collectBindingTypesExpr(decl.body);
            },
            .var_decl => |decl| {
                try self.binding_types.put(decl.bind.symbol, decl.bind.ty);
                try self.collectBindingTypesExpr(decl.body);
            },
            .reassign => |reassign| try self.collectBindingTypesExpr(reassign.body),
            .expr => |expr_id| try self.collectBindingTypesExpr(expr_id),
            .expect => |expr_id| try self.collectBindingTypesExpr(expr_id),
            .crash => {},
            .return_ => |expr_id| try self.collectBindingTypesExpr(expr_id),
            .for_ => |for_stmt| {
                try self.collectBindingTypesExpr(for_stmt.iterable);
                try self.collectBindingTypesExpr(for_stmt.body);
            },
        }
    }

    fn sortTypedSymbols(self: *Lowerer, values: []ast.TypedSymbol) void {
        _ = self;
        var i: usize = 0;
        while (i < values.len) : (i += 1) {
            var best = i;
            var j = i + 1;
            while (j < values.len) : (j += 1) {
                if (values[j].symbol.raw() < values[best].symbol.raw()) {
                    best = j;
                }
            }
            if (best != i) {
                const tmp = values[i];
                values[i] = values[best];
                values[best] = tmp;
            }
        }
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

test "monotype_lifted lower tests" {
    std.testing.refAllDecls(@This());
}
