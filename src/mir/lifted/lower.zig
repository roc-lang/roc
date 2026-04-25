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
const builtin = @import("builtin");
const base = @import("base");
const mono = @import("../mono/mod.zig");
const mono_row = @import("../mono_row/mod.zig");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");

const MonoResult = mono.Lower.Result;
const MonoAst = mono.Ast;
const Symbol = symbol_mod.Symbol;

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

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
        self.idents.deinit(self.store.allocator);
        self.attached_method_index.deinit();
        self.runtime_inspect_symbols.deinit();
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
        };
        return result;
    }
};

/// Run this compilation stage.
pub fn run(allocator: std.mem.Allocator, input: *mono_row.Result) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, try input.takeMono(allocator));
    defer lowerer.deinit();
    try lowerer.collectTopLevels();
    try lowerer.lowerProgram();
    return try lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: MonoResult,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    top_levels: std.AutoHashMap(Symbol, void),
    binding_types: std.AutoHashMap(Symbol, type_mod.TypeId),
    current_expr_lower_cache: ?*std.AutoHashMap(MonoAst.ExprId, ast.ExprId),

    const Rename = struct {
        from: Symbol,
        to: Symbol,
    };

    const LoweredExpr = struct {
        expr: ast.ExprId,
        lifted_defs: std.ArrayList(ast.Def),

        fn init() LoweredExpr {
            return .{
                .expr = undefined,
                .lifted_defs = .empty,
            };
        }

        fn deinit(self: *LoweredExpr, allocator: std.mem.Allocator) void {
            self.lifted_defs.deinit(allocator);
        }
    };

    const LoweredBlock = struct {
        stmts: ast.Span(ast.StmtId),
        final_venv: []const Rename,
        owned_final_venv: ?[]Rename = null,

        fn deinit(self: *LoweredBlock, allocator: std.mem.Allocator) void {
            if (self.owned_final_venv) |buf| allocator.free(buf);
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
            .current_expr_lower_cache = null,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.binding_types.deinit();
        self.top_levels.deinit();
        self.root_defs.deinit(self.allocator);
        self.output.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) std.mem.Allocator.Error!Result {
        const result = Result{
            .store = self.output,
            .root_defs = self.root_defs,
            .symbols = self.input.symbols,
            .types = self.input.types,
            .strings = self.input.strings,
            .idents = self.input.idents,
            .attached_method_index = self.input.attached_method_index,
            .builtin_module_idx = self.input.builtin_module_idx,
            .builtin_list_ident = self.input.builtin_list_ident,
            .builtin_box_ident = self.input.builtin_box_ident,
            .builtin_primitive_owner_idents = self.input.builtin_primitive_owner_idents,
            .runtime_inspect_symbols = self.input.runtime_inspect_symbols,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.types = type_mod.Store.init(self.allocator);
        self.input.strings = .{};
        self.input.idents = try base.Ident.Store.initCapacity(self.allocator, 1);
        self.input.attached_method_index = symbol_mod.AttachedMethodIndex.init(self.allocator);
        self.input.builtin_module_idx = 0;
        self.input.builtin_list_ident = base.Ident.Idx.NONE;
        self.input.builtin_box_ident = base.Ident.Idx.NONE;
        self.input.builtin_primitive_owner_idents = symbol_mod.PrimitiveMethodOwnerIdents.none();
        self.input.runtime_inspect_symbols = std.AutoHashMap(Symbol, Symbol).init(self.allocator);
        return result;
    }

    fn collectTopLevels(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.program.root_defs.items) |def_id| {
            const def = self.input.program.store.getDef(def_id);
            self.assertPublishedType(def.bind.ty, "monotype def bind");
            try self.top_levels.put(def.bind.symbol, {});
            try self.binding_types.put(def.bind.symbol, def.bind.ty);
            switch (def.value) {
                .fn_ => |fn_def| {
                    for (self.input.program.store.sliceTypedSymbolSpan(fn_def.args)) |arg| {
                        self.assertPublishedType(arg.ty, "monotype fn arg");
                        try self.binding_types.put(arg.symbol, arg.ty);
                    }
                    try self.collectBindingTypesExpr(fn_def.body);
                },
                .hosted_fn => |hosted_fn| {
                    for (self.input.program.store.sliceTypedSymbolSpan(hosted_fn.args)) |arg| {
                        self.assertPublishedType(arg.ty, "monotype hosted fn arg");
                        try self.binding_types.put(arg.symbol, arg.ty);
                    }
                },
                .val => |expr_id| try self.collectBindingTypesExpr(expr_id),
                .run => |run_def| try self.collectBindingTypesExpr(run_def.body),
            }
        }
    }

    fn lowerProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.program.root_defs.items) |def_id| {
            const def = self.input.program.store.getDef(def_id);
            const lowered_id = try self.lowerDef(def);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(self.root_defs.items[self.root_defs.items.len - 1] == lowered_id);
            } else if (self.root_defs.items[self.root_defs.items.len - 1] != lowered_id) {
                unreachable;
            }
        }
    }

    fn emitDef(self: *Lowerer, def: ast.Def) std.mem.Allocator.Error!ast.DefId {
        self.assertPublishedType(def.bind.ty, "lifted def bind");
        const def_id = try self.output.addDef(def);
        try self.root_defs.append(self.allocator, def_id);
        return def_id;
    }

    fn emitLiftedDefs(self: *Lowerer, defs: []const ast.Def) std.mem.Allocator.Error!void {
        for (defs) |def| {
            const def_id = try self.emitDef(def);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(self.root_defs.items[self.root_defs.items.len - 1] == def_id);
            } else if (self.root_defs.items[self.root_defs.items.len - 1] != def_id) {
                unreachable;
            }
        }
    }

    fn lowerDef(self: *Lowerer, def: MonoAst.Def) std.mem.Allocator.Error!ast.DefId {
        const empty_venv: []const Rename = &.{};
        return switch (def.value) {
            .fn_ => |fn_def| blk: {
                var expr_cache = std.AutoHashMap(MonoAst.ExprId, ast.ExprId).init(self.allocator);
                defer expr_cache.deinit();
                const previous_expr_cache = self.current_expr_lower_cache;
                self.current_expr_lower_cache = &expr_cache;
                defer self.current_expr_lower_cache = previous_expr_cache;

                var lowered_body = try self.lowerExpr(empty_venv, fn_def.body);
                defer lowered_body.deinit(self.allocator);
                try self.emitLiftedDefs(lowered_body.lifted_defs.items);
                const lowered_args = try self.copyMonoTypedSymbols(self.input.program.store.sliceTypedSymbolSpan(fn_def.args));
                defer self.allocator.free(lowered_args);
                break :blk try self.emitDef(.{
                    .bind = .{ .ty = def.bind.ty, .symbol = def.bind.symbol },
                    .value = .{ .fn_ = .{
                        .args = try self.output.addTypedSymbolSpan(lowered_args),
                        .captures = try self.output.addTypedSymbolSpan(&.{}),
                        .body = lowered_body.expr,
                    } },
                });
            },
            .hosted_fn => |hosted_fn| blk: {
                const args = self.input.program.store.sliceTypedSymbolSpan(hosted_fn.args);
                const lowered_args = try self.allocator.alloc(ast.TypedSymbol, args.len);
                defer self.allocator.free(lowered_args);
                for (args, 0..) |arg, i| {
                    lowered_args[i] = .{ .ty = arg.ty, .symbol = arg.symbol };
                }
                break :blk try self.emitDef(.{
                    .bind = .{ .ty = def.bind.ty, .symbol = def.bind.symbol },
                    .value = .{ .hosted_fn = .{
                        .bind = .{ .ty = hosted_fn.bind.ty, .symbol = hosted_fn.bind.symbol },
                        .args = try self.output.addTypedSymbolSpan(lowered_args),
                        .hosted = hosted_fn.hosted,
                    } },
                });
            },
            .val => |expr_id| blk: {
                var expr_cache = std.AutoHashMap(MonoAst.ExprId, ast.ExprId).init(self.allocator);
                defer expr_cache.deinit();
                const previous_expr_cache = self.current_expr_lower_cache;
                self.current_expr_lower_cache = &expr_cache;
                defer self.current_expr_lower_cache = previous_expr_cache;

                var lowered_expr = try self.lowerExpr(empty_venv, expr_id);
                defer lowered_expr.deinit(self.allocator);
                try self.emitLiftedDefs(lowered_expr.lifted_defs.items);
                break :blk try self.emitDef(.{
                    .bind = .{ .ty = def.bind.ty, .symbol = def.bind.symbol },
                    .value = .{ .val = lowered_expr.expr },
                });
            },
            .run => |run_def| blk: {
                var expr_cache = std.AutoHashMap(MonoAst.ExprId, ast.ExprId).init(self.allocator);
                defer expr_cache.deinit();
                const previous_expr_cache = self.current_expr_lower_cache;
                self.current_expr_lower_cache = &expr_cache;
                defer self.current_expr_lower_cache = previous_expr_cache;

                var lowered_expr = try self.lowerExpr(empty_venv, run_def.body);
                defer lowered_expr.deinit(self.allocator);
                try self.emitLiftedDefs(lowered_expr.lifted_defs.items);
                break :blk try self.emitDef(.{
                    .bind = .{ .ty = def.bind.ty, .symbol = def.bind.symbol },
                    .value = .{ .run = .{
                        .body = lowered_expr.expr,
                        .entry_ty = run_def.entry_ty,
                    } },
                });
            },
        };
    }

    fn lowerExpr(self: *Lowerer, venv: []const Rename, expr_id: MonoAst.ExprId) std.mem.Allocator.Error!LoweredExpr {
        if (self.current_expr_lower_cache) |cache| {
            if (cache.get(expr_id)) |existing| {
                return .{
                    .expr = existing,
                    .lifted_defs = .empty,
                };
            }
        }

        const expr = self.input.program.store.getExpr(expr_id);
        var lowered = LoweredExpr.init();
        errdefer lowered.deinit(self.allocator);

        switch (expr.data) {
            .var_ => |symbol| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .var_ = self.lookupRenamedSymbol(venv, symbol) },
                });
            },
            .bool_lit => |value| lowered.expr = try self.output.addExpr(.{ .ty = expr.ty, .data = .{ .bool_lit = value } }),
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
                        .discriminant = tag.discriminant,
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, tag.args),
                        .constructor_ty = tag.constructor_ty,
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
                        .field_index = access.field_index,
                    } },
                });
            },
            .structural_eq => |eq| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .structural_eq = .{
                        .lhs = try self.lowerExprInto(&lowered.lifted_defs, venv, eq.lhs),
                        .rhs = try self.lowerExprInto(&lowered.lifted_defs, venv, eq.rhs),
                    } },
                });
            },
            .method_eq => |eq| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .method_eq = .{
                        .lhs = try self.lowerExprInto(&lowered.lifted_defs, venv, eq.lhs),
                        .rhs = try self.lowerExprInto(&lowered.lifted_defs, venv, eq.rhs),
                        .negated = eq.negated,
                        .dispatch_constraint_ty = eq.dispatch_constraint_ty,
                    } },
                });
            },
            .dispatch_call => |method_call| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .dispatch_call = .{
                        .receiver = try self.lowerExprInto(&lowered.lifted_defs, venv, method_call.receiver),
                        .method_name = method_call.method_name,
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, method_call.args),
                        .dispatch_constraint_ty = method_call.dispatch_constraint_ty,
                    } },
                });
            },
            .type_dispatch_call => |method_call| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .type_dispatch_call = .{
                        .dispatcher_ty = method_call.dispatcher_ty,
                        .method_name = method_call.method_name,
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, method_call.args),
                        .dispatch_constraint_ty = method_call.dispatch_constraint_ty,
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
                            .bind = .{ .ty = let_val.bind.ty, .symbol = let_val.bind.symbol },
                            .body = body,
                            .rest = rest,
                        } },
                    });
                },
                .let_fn => |let_fn| {
                    const lifted_symbol = try self.freshLiftedLocalFnSymbol(let_fn.bind.symbol);
                    const recursive_venv = try self.extendRename(venv, let_fn.bind.symbol, lifted_symbol);
                    defer self.allocator.free(recursive_venv);

                    const let_fn_args = self.input.program.store.sliceTypedSymbolSpan(let_fn.args);
                    const bound_symbols = try self.allocator.alloc(Symbol, let_fn_args.len + 1);
                    defer self.allocator.free(bound_symbols);
                    bound_symbols[0] = let_fn.bind.symbol;
                    for (let_fn_args, 0..) |arg, i| bound_symbols[i + 1] = arg.symbol;

                    const captures = try self.computeCaptures(recursive_venv, bound_symbols, let_fn.body);
                    defer self.allocator.free(captures);

                    var lowered_body = try self.lowerExpr(recursive_venv, let_fn.body);
                    defer lowered_body.deinit(self.allocator);
                    try lowered.lifted_defs.appendSlice(self.allocator, lowered_body.lifted_defs.items);
                    const lowered_fn_args = try self.copyMonoTypedSymbols(let_fn_args);
                    defer self.allocator.free(lowered_fn_args);

                    try lowered.lifted_defs.append(self.allocator, .{
                        .bind = .{ .ty = let_fn.bind.ty, .symbol = lifted_symbol },
                        .value = .{ .fn_ = .{
                            .args = try self.output.addTypedSymbolSpan(lowered_fn_args),
                            .captures = try self.output.addTypedSymbolSpan(captures),
                            .body = lowered_body.expr,
                        } },
                    });

                    if (captures.len == 0) {
                        lowered.expr = try self.lowerExprInto(&lowered.lifted_defs, recursive_venv, let_expr.rest);
                    } else {
                        const alias_symbol = try self.freshLiftedLocalFnAliasSymbol(let_fn.bind.symbol);
                        const alias_venv = try self.extendRename(venv, let_fn.bind.symbol, alias_symbol);
                        defer self.allocator.free(alias_venv);

                        const rest = try self.lowerExprInto(&lowered.lifted_defs, alias_venv, let_expr.rest);
                        const lifted_ref = try self.output.addExpr(.{
                            .ty = let_fn.bind.ty,
                            .data = .{ .var_ = lifted_symbol },
                        });
                        lowered.expr = try self.output.addExpr(.{
                            .ty = expr.ty,
                            .data = .{ .let_ = .{
                                .bind = .{ .ty = let_fn.bind.ty, .symbol = alias_symbol },
                                .body = lifted_ref,
                                .rest = rest,
                            } },
                        });
                    }
                },
            },
            .clos => |clos| {
                const clos_args = self.input.program.store.sliceTypedSymbolSpan(clos.args);
                const bound_symbols = try self.allocator.alloc(Symbol, clos_args.len);
                defer self.allocator.free(bound_symbols);
                for (clos_args, 0..) |arg, i| bound_symbols[i] = arg.symbol;

                const captures = try self.computeCaptures(venv, bound_symbols, clos.body);
                defer self.allocator.free(captures);

                var lowered_body = try self.lowerExpr(venv, clos.body);
                defer lowered_body.deinit(self.allocator);
                try lowered.lifted_defs.appendSlice(self.allocator, lowered_body.lifted_defs.items);

                const lifted_symbol = try self.freshLiftedClosureSymbol();
                const lowered_args = try self.allocator.alloc(ast.TypedSymbol, clos_args.len);
                defer self.allocator.free(lowered_args);
                for (clos_args, 0..) |arg, i| {
                    lowered_args[i] = .{ .ty = arg.ty, .symbol = arg.symbol };
                }
                try lowered.lifted_defs.append(self.allocator, .{
                    .bind = .{ .ty = expr.ty, .symbol = lifted_symbol },
                    .value = .{ .fn_ = .{
                        .args = try self.output.addTypedSymbolSpan(lowered_args),
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
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, call.args),
                        .call_constraint_ty = call.call_constraint_ty,
                    } },
                });
            },
            .inspect => |value| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .inspect = try self.lowerExprInto(&lowered.lifted_defs, venv, value) },
                });
            },
            .low_level => |ll| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .low_level = .{
                        .op = ll.op,
                        .args = try self.lowerExprSpan(&lowered.lifted_defs, venv, ll.args),
                        .source_constraint_ty = ll.source_constraint_ty,
                    } },
                });
            },
            .when => |when_expr| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .when = .{
                        .cond = try self.lowerExprInto(&lowered.lifted_defs, venv, when_expr.cond),
                        .branches = try self.lowerBranchSpan(&lowered.lifted_defs, venv, when_expr.branches),
                        .is_try_suffix = when_expr.is_try_suffix,
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
                var lowered_block = try self.lowerBlock(&lowered.lifted_defs, venv, block.stmts);
                defer lowered_block.deinit(self.allocator);
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .block = .{
                        .stmts = lowered_block.stmts,
                        .final_expr = try self.lowerExprInto(&lowered.lifted_defs, lowered_block.final_venv, block.final_expr),
                    } },
                });
            },
            .tuple => |tuple| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .tuple = try self.lowerExprSpan(&lowered.lifted_defs, venv, tuple) },
                });
            },
            .tag_payload => |tag_payload| {
                lowered.expr = try self.output.addExpr(.{
                    .ty = expr.ty,
                    .data = .{ .tag_payload = .{
                        .tag_union = try self.lowerExprInto(&lowered.lifted_defs, venv, tag_payload.tag_union),
                        .tag_name = tag_payload.tag_name,
                        .tag_discriminant = tag_payload.tag_discriminant,
                        .payload_index = tag_payload.payload_index,
                    } },
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

        if (self.current_expr_lower_cache) |cache| {
            try cache.put(expr_id, lowered.expr);
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

    fn lowerBlock(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        span: MonoAst.Span(MonoAst.StmtId),
    ) std.mem.Allocator.Error!LoweredBlock {
        const stmt_ids = self.input.program.store.sliceStmtSpan(span);
        var lowered_stmt_ids = std.ArrayList(ast.StmtId).empty;
        defer lowered_stmt_ids.deinit(self.allocator);

        var current_venv = venv;
        var owned_current_venv: ?[]Rename = null;
        errdefer if (owned_current_venv) |buf| self.allocator.free(buf);

        var i: usize = 0;
        while (i < stmt_ids.len) {
            if (try self.lowerLocalFnDeclGroup(lifted_defs, &lowered_stmt_ids, &current_venv, &owned_current_venv, stmt_ids, &i)) {
                continue;
            }

            const lowered_stmt_id = try self.lowerStmt(lifted_defs, current_venv, stmt_ids[i]);
            try lowered_stmt_ids.append(self.allocator, lowered_stmt_id);
            i += 1;
        }

        return .{
            .stmts = try self.output.addStmtSpan(lowered_stmt_ids.items),
            .final_venv = current_venv,
            .owned_final_venv = owned_current_venv,
        };
    }

    fn lowerStmt(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        venv: []const Rename,
        stmt_id: MonoAst.StmtId,
    ) std.mem.Allocator.Error!ast.StmtId {
        const stmt = self.input.program.store.getStmt(stmt_id);
        const lowered_stmt: ast.Stmt = switch (stmt) {
            .local_fn => debugPanic("monotype_lifted invariant violated: local_fn stmt must be handled by lowerLocalFnDeclGroup"),
            .decl => |decl| .{ .decl = .{
                .bind = .{ .ty = decl.bind.ty, .symbol = decl.bind.symbol },
                .body = try self.lowerExprInto(lifted_defs, venv, decl.body),
            } },
            .var_decl => |decl| .{ .var_decl = .{
                .bind = .{ .ty = decl.bind.ty, .symbol = decl.bind.symbol },
                .body = try self.lowerExprInto(lifted_defs, venv, decl.body),
            } },
            .reassign => |reassign| .{ .reassign = .{
                .target = self.lookupRenamedSymbol(venv, reassign.target),
                .body = try self.lowerExprInto(lifted_defs, venv, reassign.body),
            } },
            .expr => |expr_id| .{ .expr = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .debug => |expr_id| .{ .debug = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .expect => |expr_id| .{ .expect = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .crash => |msg| .{ .crash = msg },
            .return_ => |expr_id| .{ .return_ = try self.lowerExprInto(lifted_defs, venv, expr_id) },
            .break_ => .break_,
            .for_ => |for_stmt| .{ .for_ = .{
                .patt = try self.lowerPat(for_stmt.patt),
                .iterable = try self.lowerExprInto(lifted_defs, venv, for_stmt.iterable),
                .body = try self.lowerExprInto(lifted_defs, venv, for_stmt.body),
            } },
            .while_ => |while_stmt| .{ .while_ = .{
                .cond = try self.lowerExprInto(lifted_defs, venv, while_stmt.cond),
                .body = try self.lowerExprInto(lifted_defs, venv, while_stmt.body),
            } },
        };
        return try self.output.addStmt(lowered_stmt);
    }

    fn lowerLocalFnDeclGroup(
        self: *Lowerer,
        lifted_defs: *std.ArrayList(ast.Def),
        lowered_stmt_ids: *std.ArrayList(ast.StmtId),
        current_venv: *[]const Rename,
        owned_current_venv: *?[]Rename,
        stmt_ids: []const MonoAst.StmtId,
        index: *usize,
    ) std.mem.Allocator.Error!bool {
        if (index.* >= stmt_ids.len) return false;

        const first_stmt = self.input.program.store.getStmt(stmt_ids[index.*]);
        if (first_stmt != .local_fn) return false;

        var group_end = index.*;
        while (group_end < stmt_ids.len) : (group_end += 1) {
            const stmt = self.input.program.store.getStmt(stmt_ids[group_end]);
            if (stmt != .local_fn) break;
        }

        const GroupMember = struct {
            letfn: MonoAst.LetFn,
            lifted_symbol: Symbol,
            alias_symbol: Symbol,
        };

        const group_len = group_end - index.*;
        const group = try self.allocator.alloc(GroupMember, group_len);
        defer self.allocator.free(group);

        for (stmt_ids[index.*..group_end], 0..) |stmt_id, group_i| {
            const letfn = self.input.program.store.getStmt(stmt_id).local_fn;
            const lifted_symbol = try self.freshLiftedLocalFnSymbol(letfn.bind.symbol);
            const alias_symbol = try self.freshLiftedLocalFnAliasSymbol(letfn.bind.symbol);
            group[group_i] = .{
                .letfn = letfn,
                .lifted_symbol = lifted_symbol,
                .alias_symbol = alias_symbol,
            };
        }

        const group_base_venv = current_venv.*;
        const needs_alias = try self.allocator.alloc(bool, group_len);
        defer self.allocator.free(needs_alias);
        @memset(needs_alias, false);

        var changed = true;
        while (changed) {
            changed = false;
            for (group, 0..) |member, member_i| {
                var body_venv = group_base_venv;
                var owned_body_venv: ?[]Rename = null;
                defer if (owned_body_venv) |buf| self.allocator.free(buf);

                for (group, 0..) |other, other_i| {
                    const target_symbol = if (other_i == member_i)
                        other.lifted_symbol
                    else if (needs_alias[other_i])
                        other.alias_symbol
                    else
                        other.lifted_symbol;
                    body_venv = try self.pushRename(body_venv, &owned_body_venv, other.letfn.bind.symbol, target_symbol);
                }

                const member_args = self.input.program.store.sliceTypedSymbolSpan(member.letfn.args);
                const bound_symbols = try self.allocator.alloc(Symbol, member_args.len + 1);
                defer self.allocator.free(bound_symbols);
                bound_symbols[0] = member.letfn.bind.symbol;
                for (member_args, 0..) |arg, i| bound_symbols[i + 1] = arg.symbol;

                const captures = try self.computeCaptures(body_venv, bound_symbols, member.letfn.body);
                defer self.allocator.free(captures);

                const next_needs_alias = captures.len != 0;
                if (next_needs_alias != needs_alias[member_i]) {
                    needs_alias[member_i] = next_needs_alias;
                    changed = true;
                }
            }
        }

        const LoweredMember = struct {
            captures: ast.Span(ast.TypedSymbol),
            body: ast.ExprId,
        };

        const lowered_members = try self.allocator.alloc(LoweredMember, group_len);
        defer self.allocator.free(lowered_members);

        for (group, 0..) |member, member_i| {
            var body_venv = group_base_venv;
            var owned_body_venv: ?[]Rename = null;
            defer if (owned_body_venv) |buf| self.allocator.free(buf);

            for (group, 0..) |other, other_i| {
                const target_symbol = if (other_i == member_i)
                    other.lifted_symbol
                else if (needs_alias[other_i])
                    other.alias_symbol
                else
                    other.lifted_symbol;
                body_venv = try self.pushRename(body_venv, &owned_body_venv, other.letfn.bind.symbol, target_symbol);
            }

            const member_args = self.input.program.store.sliceTypedSymbolSpan(member.letfn.args);
            const bound_symbols = try self.allocator.alloc(Symbol, member_args.len + 1);
            defer self.allocator.free(bound_symbols);
            bound_symbols[0] = member.letfn.bind.symbol;
            for (member_args, 0..) |arg, i| bound_symbols[i + 1] = arg.symbol;

            const captures = try self.computeCaptures(body_venv, bound_symbols, member.letfn.body);
            defer self.allocator.free(captures);

            var lowered_body = try self.lowerExpr(body_venv, member.letfn.body);
            defer lowered_body.deinit(self.allocator);
            try lifted_defs.appendSlice(self.allocator, lowered_body.lifted_defs.items);
            lowered_members[member_i] = .{
                .captures = try self.output.addTypedSymbolSpan(captures),
                .body = lowered_body.expr,
            };
            const lowered_member_args = try self.copyMonoTypedSymbols(self.input.program.store.sliceTypedSymbolSpan(member.letfn.args));
            defer self.allocator.free(lowered_member_args);
            try lifted_defs.append(self.allocator, .{
                .bind = .{ .ty = member.letfn.bind.ty, .symbol = member.lifted_symbol },
                .value = .{ .fn_ = .{
                    .args = try self.output.addTypedSymbolSpan(lowered_member_args),
                    .captures = lowered_members[member_i].captures,
                    .body = lowered_members[member_i].body,
                } },
            });
        }

        for (group, 0..) |member, member_i| {
            if (!needs_alias[member_i]) {
                current_venv.* = try self.pushRename(current_venv.*, owned_current_venv, member.letfn.bind.symbol, member.lifted_symbol);
            }
        }

        const emitted_aliases = try self.allocator.alloc(bool, group_len);
        defer self.allocator.free(emitted_aliases);
        @memset(emitted_aliases, false);

        var remaining_aliases: usize = 0;
        for (needs_alias) |needed| {
            if (needed) remaining_aliases += 1;
        }

        while (remaining_aliases > 0) {
            var progressed = false;
            for (group, 0..) |member, member_i| {
                if (!needs_alias[member_i] or emitted_aliases[member_i]) continue;

                var blocked = false;
                for (self.output.sliceTypedSymbolSpan(lowered_members[member_i].captures)) |capture| {
                    for (group, 0..) |other, other_i| {
                        if (other_i == member_i or !needs_alias[other_i] or emitted_aliases[other_i]) continue;
                        if (capture.symbol == other.alias_symbol) {
                            blocked = true;
                            break;
                        }
                    }
                    if (blocked) break;
                }
                if (blocked) continue;

                const lifted_ref = try self.output.addExpr(.{
                    .ty = member.letfn.bind.ty,
                    .data = .{ .var_ = member.lifted_symbol },
                });
                const alias_stmt = try self.output.addStmt(.{ .decl = .{
                    .bind = .{ .ty = member.letfn.bind.ty, .symbol = member.alias_symbol },
                    .body = lifted_ref,
                } });
                try lowered_stmt_ids.append(self.allocator, alias_stmt);
                current_venv.* = try self.pushRename(current_venv.*, owned_current_venv, member.letfn.bind.symbol, member.alias_symbol);
                emitted_aliases[member_i] = true;
                remaining_aliases -= 1;
                progressed = true;
            }

            if (!progressed) {
                return debugPanic("monotype_lifted local function alias cycle");
            }
        }

        index.* = group_end;
        return true;
    }

    fn lowerPat(self: *Lowerer, pat_id: MonoAst.PatId) std.mem.Allocator.Error!ast.PatId {
        const pat = self.input.program.store.getPat(pat_id);
        return switch (pat.data) {
            .var_ => |symbol| try self.output.addPat(.{
                .ty = pat.ty,
                .data = .{ .var_ = symbol },
            }),
            .bool_lit => |value| try self.output.addPat(.{
                .ty = pat.ty,
                .data = .{ .bool_lit = value },
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
                        .discriminant = tag.discriminant,
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
            const source_symbol = entry.key_ptr.*;
            const symbol = self.lookupRenamedSymbol(venv, source_symbol);
            if (self.isGlobalValueSymbol(symbol)) continue;
            try captures_map.put(symbol, self.lookupTypeForSymbol(source_symbol));
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

    fn copyMonoTypedSymbols(
        self: *Lowerer,
        values: []const MonoAst.TypedSymbol,
    ) std.mem.Allocator.Error![]ast.TypedSymbol {
        const out = try self.allocator.alloc(ast.TypedSymbol, values.len);
        errdefer self.allocator.free(out);
        for (values, 0..) |value, i| {
            out[i] = .{
                .ty = value.ty,
                .symbol = value.symbol,
            };
        }
        return out;
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
            .bool_lit,
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
            .structural_eq => |eq| {
                try self.collectFreeVarsExpr(eq.lhs, bound, free);
                try self.collectFreeVarsExpr(eq.rhs, bound, free);
            },
            .method_eq => |eq| {
                try self.collectFreeVarsExpr(eq.lhs, bound, free);
                try self.collectFreeVarsExpr(eq.rhs, bound, free);
            },
            .dispatch_call => |method_call| {
                try self.collectFreeVarsExpr(method_call.receiver, bound, free);
                for (self.input.program.store.sliceExprSpan(method_call.args)) |arg| {
                    try self.collectFreeVarsExpr(arg, bound, free);
                }
            },
            .type_dispatch_call => |method_call| {
                for (self.input.program.store.sliceExprSpan(method_call.args)) |arg| {
                    try self.collectFreeVarsExpr(arg, bound, free);
                }
            },
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

                    const let_fn_args = self.input.program.store.sliceTypedSymbolSpan(let_fn.args);
                    const inserted_args = try self.allocator.alloc(bool, let_fn_args.len);
                    defer self.allocator.free(inserted_args);
                    for (let_fn_args, 0..) |arg, i| {
                        inserted_args[i] = try self.bindTemporarily(bound, arg.symbol);
                    }
                    defer for (let_fn_args, inserted_args) |arg, inserted| {
                        self.unbindTemporarily(bound, arg.symbol, inserted);
                    };

                    try self.collectFreeVarsExpr(let_fn.body, bound, free);
                    try self.collectFreeVarsExpr(let_expr.rest, bound, free);
                },
            },
            .clos => |clos| {
                const clos_args = self.input.program.store.sliceTypedSymbolSpan(clos.args);
                const inserted_args = try self.allocator.alloc(bool, clos_args.len);
                defer self.allocator.free(inserted_args);
                for (clos_args, 0..) |arg, i| {
                    inserted_args[i] = try self.bindTemporarily(bound, arg.symbol);
                }
                defer for (clos_args, inserted_args) |arg, inserted| {
                    self.unbindTemporarily(bound, arg.symbol, inserted);
                };
                try self.collectFreeVarsExpr(clos.body, bound, free);
            },
            .call => |call| {
                try self.collectFreeVarsExpr(call.func, bound, free);
                for (self.input.program.store.sliceExprSpan(call.args)) |arg| {
                    try self.collectFreeVarsExpr(arg, bound, free);
                }
            },
            .inspect => |value| try self.collectFreeVarsExpr(value, bound, free),
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
                var added = std.ArrayList(Symbol).empty;
                defer added.deinit(self.allocator);
                for (self.input.program.store.sliceStmtSpan(block.stmts)) |stmt_id| {
                    try self.collectFreeVarsStmtInBlock(stmt_id, bound, free, &added);
                }
                try self.collectFreeVarsExpr(block.final_expr, bound, free);
                for (added.items) |symbol| {
                    const removed = bound.remove(symbol);
                    if (comptime builtin.mode == .Debug) {
                        std.debug.assert(removed);
                    } else if (!removed) {
                        unreachable;
                    }
                }
            },
            .tuple => |tuple| for (self.input.program.store.sliceExprSpan(tuple)) |arg| try self.collectFreeVarsExpr(arg, bound, free),
            .tag_payload => |tag_payload| try self.collectFreeVarsExpr(tag_payload.tag_union, bound, free),
            .tuple_access => |tuple_access| try self.collectFreeVarsExpr(tuple_access.tuple, bound, free),
            .list => |list| for (self.input.program.store.sliceExprSpan(list)) |arg| try self.collectFreeVarsExpr(arg, bound, free),
            .return_ => |ret| try self.collectFreeVarsExpr(ret, bound, free),
            .for_ => |for_expr| {
                try self.collectFreeVarsExpr(for_expr.iterable, bound, free);
                var added = std.ArrayList(Symbol).empty;
                defer added.deinit(self.allocator);
                try self.bindPatSymbols(for_expr.patt, bound, &added);
                try self.collectFreeVarsExpr(for_expr.body, bound, free);
                for (added.items) |symbol| {
                    const removed = bound.remove(symbol);
                    if (comptime builtin.mode == .Debug) {
                        std.debug.assert(removed);
                    } else if (!removed) {
                        unreachable;
                    }
                }
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
            .local_fn => |local_fn| {
                const inserted_bind = try self.bindTemporarily(bound, local_fn.bind.symbol);
                defer self.unbindTemporarily(bound, local_fn.bind.symbol, inserted_bind);

                const local_fn_args = self.input.program.store.sliceTypedSymbolSpan(local_fn.args);
                const inserted_args = try self.allocator.alloc(bool, local_fn_args.len);
                defer self.allocator.free(inserted_args);
                for (local_fn_args, 0..) |arg, i| {
                    inserted_args[i] = try self.bindTemporarily(bound, arg.symbol);
                }
                defer for (local_fn_args, inserted_args) |arg, inserted| {
                    self.unbindTemporarily(bound, arg.symbol, inserted);
                };

                try self.collectFreeVarsExpr(local_fn.body, bound, free);
            },
            .decl => |decl| {
                try self.collectFreeVarsExpr(decl.body, bound, free);
                const inserted = try self.bindTemporarily(bound, decl.bind.symbol);
                defer self.unbindTemporarily(bound, decl.bind.symbol, inserted);
            },
            .var_decl => |decl| {
                try self.collectFreeVarsExpr(decl.body, bound, free);
                const inserted = try self.bindTemporarily(bound, decl.bind.symbol);
                defer self.unbindTemporarily(bound, decl.bind.symbol, inserted);
            },
            .reassign => |reassign| {
                if (!reassign.target.isNone() and !bound.contains(reassign.target)) {
                    try free.put(reassign.target, self.lookupTypeForSymbol(reassign.target));
                }
                try self.collectFreeVarsExpr(reassign.body, bound, free);
            },
            .expr => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .debug => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .expect => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .crash => {},
            .return_ => |expr_id| try self.collectFreeVarsExpr(expr_id, bound, free),
            .break_ => {},
            .for_ => |for_stmt| {
                try self.collectFreeVarsExpr(for_stmt.iterable, bound, free);
                var added = std.ArrayList(Symbol).empty;
                defer added.deinit(self.allocator);
                try self.bindPatSymbols(for_stmt.patt, bound, &added);
                try self.collectFreeVarsExpr(for_stmt.body, bound, free);
                for (added.items) |symbol| {
                    const removed = bound.remove(symbol);
                    if (comptime builtin.mode == .Debug) {
                        std.debug.assert(removed);
                    } else if (!removed) {
                        unreachable;
                    }
                }
            },
            .while_ => |while_stmt| {
                try self.collectFreeVarsExpr(while_stmt.cond, bound, free);
                try self.collectFreeVarsExpr(while_stmt.body, bound, free);
            },
        }
    }

    fn collectFreeVarsStmtInBlock(
        self: *Lowerer,
        stmt_id: MonoAst.StmtId,
        bound: *std.AutoHashMap(Symbol, void),
        free: *std.AutoHashMap(Symbol, type_mod.TypeId),
        added: *std.ArrayList(Symbol),
    ) std.mem.Allocator.Error!void {
        const stmt = self.input.program.store.getStmt(stmt_id);
        switch (stmt) {
            .local_fn => |local_fn| {
                const inserted_bind = try self.bindTemporarily(bound, local_fn.bind.symbol);
                if (inserted_bind) {
                    try added.append(self.allocator, local_fn.bind.symbol);
                }

                const local_fn_args = self.input.program.store.sliceTypedSymbolSpan(local_fn.args);
                const inserted_args = try self.allocator.alloc(bool, local_fn_args.len);
                defer self.allocator.free(inserted_args);
                for (local_fn_args, 0..) |arg, i| {
                    inserted_args[i] = try self.bindTemporarily(bound, arg.symbol);
                }
                defer for (local_fn_args, inserted_args) |arg, inserted| {
                    self.unbindTemporarily(bound, arg.symbol, inserted);
                };

                try self.collectFreeVarsExpr(local_fn.body, bound, free);
            },
            .decl => |decl| {
                try self.collectFreeVarsExpr(decl.body, bound, free);
                const inserted = try self.bindTemporarily(bound, decl.bind.symbol);
                if (inserted) {
                    try added.append(self.allocator, decl.bind.symbol);
                }
            },
            .var_decl => |decl| {
                try self.collectFreeVarsExpr(decl.body, bound, free);
                const inserted = try self.bindTemporarily(bound, decl.bind.symbol);
                if (inserted) {
                    try added.append(self.allocator, decl.bind.symbol);
                }
            },
            else => try self.collectFreeVarsStmt(stmt_id, bound, free),
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
            .bool_lit => {},
            .tag => |tag| {
                for (self.input.program.store.slicePatSpan(tag.args)) |arg_pat| {
                    try self.bindPatSymbols(arg_pat, bound, added);
                }
            },
        }
    }

    fn lookupRenamedSymbol(_: *Lowerer, venv: []const Rename, symbol: Symbol) Symbol {
        var i = venv.len;
        while (i > 0) {
            i -= 1;
            const rename = venv[i];
            if (rename.from == symbol) return rename.to;
        }
        return symbol;
    }

    fn isGlobalValueSymbol(self: *const Lowerer, symbol: Symbol) bool {
        if (self.top_levels.contains(symbol)) return true;

        return switch (self.input.symbols.get(symbol).origin) {
            .top_level_def,
            .specialized_top_level_def,
            => true,
            else => false,
        };
    }

    fn extendRename(self: *Lowerer, venv: []const Rename, from: Symbol, to: Symbol) std.mem.Allocator.Error![]Rename {
        const next = try self.allocator.alloc(Rename, venv.len + 1);
        std.mem.copyForwards(Rename, next[0..venv.len], venv);
        next[venv.len] = .{ .from = from, .to = to };
        return next;
    }

    fn pushRename(self: *Lowerer, venv: []const Rename, owned_venv: *?[]Rename, from: Symbol, to: Symbol) std.mem.Allocator.Error![]const Rename {
        const next = try self.extendRename(venv, from, to);
        if (owned_venv.*) |buf| self.allocator.free(buf);
        owned_venv.* = next;
        return next;
    }

    fn bindTemporarily(_: *Lowerer, bound: *std.AutoHashMap(Symbol, void), symbol: Symbol) std.mem.Allocator.Error!bool {
        if (symbol.isNone()) return false;
        if (bound.contains(symbol)) return false;
        try bound.put(symbol, {});
        return true;
    }

    fn unbindTemporarily(_: *Lowerer, bound: *std.AutoHashMap(Symbol, void), symbol: Symbol, inserted: bool) void {
        if (inserted) {
            const removed = bound.remove(symbol);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(removed);
            } else if (!removed) {
                unreachable;
            }
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

    fn freshLiftedLocalFnAliasSymbol(self: *Lowerer, source_symbol: Symbol) std.mem.Allocator.Error!Symbol {
        const entry = self.input.symbols.get(source_symbol);
        const symbol = try self.input.symbols.add(entry.name, .{
            .lifted_local_fn_alias = .{ .source_symbol = source_symbol.raw() },
        });
        try self.binding_types.put(symbol, self.lookupTypeForSymbol(source_symbol));
        return symbol;
    }

    fn lookupTypeForSymbol(self: *Lowerer, symbol: Symbol) type_mod.TypeId {
        if (self.binding_types.get(symbol)) |ty| {
            self.assertPublishedType(ty, "lifted symbol lookup");
            return ty;
        }
        const entry = self.input.symbols.get(symbol);
        const name = if (entry.name.isNone()) "<none>" else self.input.idents.getText(entry.name);
        std.debug.panic(
            "monotype_lifted.lookupTypeForSymbol missing symbol type for {d} ({s}) origin {s}",
            .{ symbol.raw(), name, @tagName(entry.origin) },
        );
    }

    fn assertPublishedType(self: *const Lowerer, ty: type_mod.TypeId, comptime site: []const u8) void {
        if (comptime builtin.mode == .Debug) {
            if (self.input.types.containsPlaceholder(ty)) {
                debugPanic("monotype_lifted invariant violated: " ++ site ++ " leaked monotype builder placeholder");
            }
        }
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
            .bool_lit,
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
            .structural_eq => |eq| {
                try self.collectBindingTypesExpr(eq.lhs);
                try self.collectBindingTypesExpr(eq.rhs);
            },
            .method_eq => |eq| {
                try self.collectBindingTypesExpr(eq.lhs);
                try self.collectBindingTypesExpr(eq.rhs);
            },
            .dispatch_call => |method_call| {
                try self.collectBindingTypesExpr(method_call.receiver);
                for (self.input.program.store.sliceExprSpan(method_call.args)) |arg| {
                    try self.collectBindingTypesExpr(arg);
                }
            },
            .type_dispatch_call => |method_call| {
                for (self.input.program.store.sliceExprSpan(method_call.args)) |arg| {
                    try self.collectBindingTypesExpr(arg);
                }
            },
            .let_ => |let_expr| switch (let_expr.def) {
                .let_val => |let_val| {
                    try self.binding_types.put(let_val.bind.symbol, let_val.bind.ty);
                    try self.collectBindingTypesExpr(let_val.body);
                    try self.collectBindingTypesExpr(let_expr.rest);
                },
                .let_fn => |let_fn| {
                    try self.binding_types.put(let_fn.bind.symbol, let_fn.bind.ty);
                    for (self.input.program.store.sliceTypedSymbolSpan(let_fn.args)) |arg| {
                        try self.binding_types.put(arg.symbol, arg.ty);
                    }
                    try self.collectBindingTypesExpr(let_fn.body);
                    try self.collectBindingTypesExpr(let_expr.rest);
                },
            },
            .clos => |clos| {
                for (self.input.program.store.sliceTypedSymbolSpan(clos.args)) |arg| {
                    try self.binding_types.put(arg.symbol, arg.ty);
                }
                try self.collectBindingTypesExpr(clos.body);
            },
            .call => |call| {
                try self.collectBindingTypesExpr(call.func);
                for (self.input.program.store.sliceExprSpan(call.args)) |arg| {
                    try self.collectBindingTypesExpr(arg);
                }
            },
            .inspect => |value| try self.collectBindingTypesExpr(value),
            .low_level => |ll| for (self.input.program.store.sliceExprSpan(ll.args)) |arg| try self.collectBindingTypesExpr(arg),
            .when => |when_expr| {
                try self.collectBindingTypesExpr(when_expr.cond);
                for (self.input.program.store.sliceBranchSpan(when_expr.branches)) |branch_id| {
                    const branch = self.input.program.store.getBranch(branch_id);
                    try self.collectBindingTypesPat(branch.pat);
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
            .tag_payload => |tag_payload| try self.collectBindingTypesExpr(tag_payload.tag_union),
            .tuple_access => |tuple_access| try self.collectBindingTypesExpr(tuple_access.tuple),
            .list => |list| for (self.input.program.store.sliceExprSpan(list)) |arg| try self.collectBindingTypesExpr(arg),
            .return_ => |ret| try self.collectBindingTypesExpr(ret),
            .for_ => |for_expr| {
                try self.collectBindingTypesExpr(for_expr.iterable);
                try self.collectBindingTypesPat(for_expr.patt);
                try self.collectBindingTypesExpr(for_expr.body);
            },
        }
    }

    fn collectBindingTypesPat(self: *Lowerer, pat_id: MonoAst.PatId) std.mem.Allocator.Error!void {
        const pat = self.input.program.store.getPat(pat_id);
        switch (pat.data) {
            .var_ => |symbol| if (!symbol.isNone()) {
                try self.binding_types.put(symbol, pat.ty);
            },
            .bool_lit => {},
            .tag => |tag| {
                for (self.input.program.store.slicePatSpan(tag.args)) |arg| {
                    try self.collectBindingTypesPat(arg);
                }
            },
        }
    }

    fn collectBindingTypesStmt(self: *Lowerer, stmt_id: MonoAst.StmtId) std.mem.Allocator.Error!void {
        const stmt = self.input.program.store.getStmt(stmt_id);
        switch (stmt) {
            .local_fn => |local_fn| {
                try self.binding_types.put(local_fn.bind.symbol, local_fn.bind.ty);
                for (self.input.program.store.sliceTypedSymbolSpan(local_fn.args)) |arg| {
                    try self.binding_types.put(arg.symbol, arg.ty);
                }
                try self.collectBindingTypesExpr(local_fn.body);
            },
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
            .debug => |expr_id| try self.collectBindingTypesExpr(expr_id),
            .expect => |expr_id| try self.collectBindingTypesExpr(expr_id),
            .crash => {},
            .return_ => |expr_id| try self.collectBindingTypesExpr(expr_id),
            .break_ => {},
            .for_ => |for_stmt| {
                try self.collectBindingTypesExpr(for_stmt.iterable);
                try self.collectBindingTypesPat(for_stmt.patt);
                try self.collectBindingTypesExpr(for_stmt.body);
            },
            .while_ => |while_stmt| {
                try self.collectBindingTypesExpr(while_stmt.cond);
                try self.collectBindingTypesExpr(while_stmt.body);
            },
        }
    }

    fn sortTypedSymbols(_: *Lowerer, values: []ast.TypedSymbol) void {
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
