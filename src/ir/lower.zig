//! Lower lambdamono into cor-style IR.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const lambdamono = @import("lambdamono");
const symbol_mod = @import("symbol");
const ast = @import("ast.zig");
const layout_mod = @import("layout");
const ir_layout = @import("layout.zig");

const Symbol = symbol_mod.Symbol;

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    layouts: ir_layout.Graph,
    strings: base.StringLiteral.Store,

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.layouts.deinit(self.store.allocator);
        self.strings.deinit(self.store.allocator);
    }
};

pub fn run(allocator: std.mem.Allocator, input: lambdamono.Lower.Result) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, input);
    defer lowerer.deinit();
    try lowerer.lowerProgram();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: lambdamono.Lower.Result,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    value_thunks: std.AutoHashMap(Symbol, ValueThunk),

    const EnvEntry = struct {
        symbol: Symbol,
        var_: ast.Var,
    };

    const ValueThunk = struct {
        proc: Symbol,
        result_ty: lambdamono.Type.TypeId,
    };

    const LoweredBlock = struct {
        stmts: std.ArrayList(ast.Stmt),
        term: ast.Term,
        has_term: bool,

        fn init(allocator: std.mem.Allocator) LoweredBlock {
            _ = allocator;
            return .{
                .stmts = .empty,
                .term = undefined,
                .has_term = false,
            };
        }

        fn deinit(self: *LoweredBlock, allocator: std.mem.Allocator) void {
            self.stmts.deinit(allocator);
        }

        fn setTerm(self: *LoweredBlock, term: ast.Term) void {
            self.term = term;
            self.has_term = true;
        }

        fn requireTerm(self: *const LoweredBlock) ast.Term {
            if (!self.has_term) {
                debugPanic("ir.lower invariant violated: lowered block missing terminator");
            }
            return self.term;
        }
    };

    fn init(allocator: std.mem.Allocator, input: lambdamono.Lower.Result) Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .value_thunks = std.AutoHashMap(Symbol, ValueThunk).init(allocator),
        };
    }

    fn deinit(self: *Lowerer) void {
        self.value_thunks.deinit();
        self.root_defs.deinit(self.allocator);
        self.output.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const layouts = self.input.layout_facts.graph;
        self.input.layout_facts.graph = .{};

        const result = Result{
            .store = self.output,
            .root_defs = self.root_defs,
            .symbols = self.input.symbols,
            .layouts = layouts,
            .strings = self.input.strings,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.strings = .{};
        return result;
    }

    fn lowerProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        try self.registerValueThunks();

        for (self.input.store.defsSlice(), 0..) |def, i| {
            const lowered = try self.lowerDef(@enumFromInt(@as(u32, @intCast(i))), def);
            const def_id = try self.output.addDef(lowered);
            try self.root_defs.append(self.allocator, def_id);
        }
    }

    fn registerValueThunks(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .val => |expr_id| try self.value_thunks.put(def.bind, .{
                    .proc = def.bind,
                    .result_ty = def.result_ty orelse self.input.store.getExpr(expr_id).ty,
                }),
                .run => |run_def| try self.value_thunks.put(def.bind, .{
                    .proc = def.bind,
                    .result_ty = def.result_ty orelse self.input.store.getExpr(run_def.body).ty,
                }),
                .fn_, .hosted_fn => {},
            }
        }
    }

    fn lowerDef(self: *Lowerer, def_id: lambdamono.Ast.DefId, def: lambdamono.Ast.Def) std.mem.Allocator.Error!ast.Def {
        return switch (def.value) {
            .fn_ => |fn_def| blk: {
                const args = try self.lowerTypedSymbolSpan(fn_def.args);
                const env = try self.envFromVarSpan(args);
                defer self.allocator.free(env);
                break :blk .{
                    .name = def.bind,
                    .args = args,
                    .body = try self.lowerBlock(env, fn_def.body),
                    .ret_layout = self.input.layout_facts.defRetLayout(def_id),
                };
            },
            .hosted_fn => |hosted_fn| .{
                .name = def.bind,
                .args = try self.lowerTypedSymbolSpan(hosted_fn.args),
                .ret_layout = self.input.layout_facts.defRetLayout(def_id),
                .hosted = hosted_fn.hosted,
            },
            .val => |expr_id| .{
                .name = def.bind,
                .args = try self.output.addVarSpan(&.{}),
                .body = try self.lowerBlock(&.{}, expr_id),
                .ret_layout = self.input.layout_facts.defRetLayout(def_id),
            },
            .run => |run_def| .{
                .name = def.bind,
                .args = try self.output.addVarSpan(&.{}),
                .body = try self.lowerBlock(&.{}, run_def.body),
                .ret_layout = self.input.layout_facts.defRetLayout(def_id),
                .entry_ty = run_def.entry_ty,
            },
        };
    }

    fn lowerTypedSymbol(self: *Lowerer, value: lambdamono.Ast.TypedSymbol) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = self.input.layout_facts.layoutForType(value.ty),
            .symbol = value.symbol,
        };
    }

    fn lowerTypedSymbolSpan(self: *Lowerer, span: lambdamono.Ast.Span(lambdamono.Ast.TypedSymbol)) std.mem.Allocator.Error!ast.Span(ast.Var) {
        const args = self.input.store.sliceTypedSymbolSpan(span);
        const out = try self.allocator.alloc(ast.Var, args.len);
        defer self.allocator.free(out);
        for (args, 0..) |arg, i| {
            out[i] = try self.lowerTypedSymbol(arg);
        }
        return try self.output.addVarSpan(out);
    }

    fn envFromVarSpan(self: *Lowerer, span: ast.Span(ast.Var)) std.mem.Allocator.Error![]EnvEntry {
        const vars = self.output.sliceVarSpan(span);
        const out = try self.allocator.alloc(EnvEntry, vars.len);
        for (vars, 0..) |var_, i| {
            out[i] = .{ .symbol = var_.symbol, .var_ = var_ };
        }
        return out;
    }

    fn lowerBlock(self: *Lowerer, env: []const EnvEntry, expr_id: lambdamono.Ast.ExprId) std.mem.Allocator.Error!ast.BlockId {
        var lowered = try self.lowerExpr(env, expr_id);
        defer lowered.deinit(self.allocator);
        const stmt_span = try self.addStmtSpan(lowered.stmts.items);
        return try self.output.addBlock(.{
            .stmts = stmt_span,
            .term = lowered.requireTerm(),
        });
    }

    fn lowerExpr(self: *Lowerer, env: []const EnvEntry, expr_id: lambdamono.Ast.ExprId) std.mem.Allocator.Error!LoweredBlock {
        const expr = self.input.store.getExpr(expr_id);
        var block = LoweredBlock.init(self.allocator);
        errdefer block.deinit(self.allocator);

        switch (expr.data) {
            .var_ => |symbol| {
                if (self.lookupEnv(env, symbol)) |value| {
                    block.setTerm(.{ .value = value });
                } else if (self.value_thunks.get(symbol)) |value_thunk| {
                    const temp = try self.freshVar(value_thunk.result_ty, "thunk_result");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = temp,
                        .expr = try self.output.addExpr(.{ .call_direct = .{
                            .proc = value_thunk.proc,
                            .args = try self.output.addVarSpan(&.{}),
                        } }),
                    } });
                    block.setTerm(.{ .value = temp });
                } else {
                    block.setTerm(.{ .value = try self.lowerTypedSymbol(.{
                        .ty = expr.ty,
                        .symbol = symbol,
                    }) });
                }
            },
            .bool_lit => |value| try self.emitLeafExpr(&block, expr.ty, "bool", .{ .lit = .{ .bool = value } }),
            .int_lit => |value| try self.emitLeafExpr(&block, expr.ty, "int", .{ .lit = .{ .int = value } }),
            .frac_f32_lit => |value| try self.emitLeafExpr(&block, expr.ty, "f32", .{ .lit = .{ .f32 = value } }),
            .frac_f64_lit => |value| try self.emitLeafExpr(&block, expr.ty, "f64", .{ .lit = .{ .f64 = value } }),
            .dec_lit => |value| try self.emitLeafExpr(&block, expr.ty, "dec", .{ .lit = .{ .dec = value } }),
            .str_lit => |value| try self.emitLeafExpr(&block, expr.ty, "str", .{ .lit = .{ .str = value } }),
            .unit => try self.emitLeafExpr(&block, expr.ty, "unit", .{
                .make_struct = try self.output.addVarSpan(&.{}),
            }),
            .record => |fields| {
                const lowered_fields = try self.lowerFieldValues(&block, env, fields);
                if (lowered_fields == null) return if (block.has_term) block else debugPanic("ir.lower record missing terminator");
                try self.emitLeafExpr(&block, expr.ty, "record", .{ .make_struct = lowered_fields.? });
            },
            .tuple => |items| {
                const lowered_items = try self.lowerValueSpan(&block, env, items);
                if (lowered_items == null) return if (block.has_term) block else debugPanic("ir.lower tuple missing terminator");
                try self.emitLeafExpr(&block, expr.ty, "tuple", .{ .make_struct = lowered_items.? });
            },
            .list => |items| {
                const lowered_items = try self.lowerValueSpan(&block, env, items);
                if (lowered_items == null) return if (block.has_term) block else debugPanic("ir.lower list missing terminator");
                try self.emitLeafExpr(&block, expr.ty, "list", .{ .make_list = lowered_items.? });
            },
            .tag => |tag| {
                const lowered_args = try self.lowerValueSpan(&block, env, tag.args);
                if (lowered_args == null) return if (block.has_term) block else debugPanic("ir.lower tag missing terminator");

                const payload: ?ast.Var = blk: {
                    const args = self.output.sliceVarSpan(lowered_args.?);
                    if (args.len == 0) break :blk null;
                    const payload_layout = self.input.layout_facts.exprTagPayloadLayout(expr_id);
                    const payload_var = try self.freshVarWithLayout(
                        payload_layout,
                        "tag_payload",
                    );
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = payload_var,
                        .expr = try self.output.addExpr(.{ .make_struct = lowered_args.? }),
                    } });
                    break :blk payload_var;
                };

                const temp = try self.freshVar(expr.ty, "tag");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .make_union = .{
                        .discriminant = tag.discriminant,
                        .payload = payload,
                    } }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .access => |access| {
                const record = try self.lowerSubexprValue(&block, env, access.record);
                if (record == null) return if (block.has_term) block else debugPanic("ir.lower access missing terminator");
                const field_layout = self.input.layout_facts.exprFieldLayout(expr_id);
                const temp = try self.freshVarWithLayout(
                    field_layout,
                    "field",
                );
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .get_struct_field = .{
                        .record = record.?,
                        .field_index = access.field_index,
                    } }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .tuple_access => |tuple_access| {
                const tuple = try self.lowerSubexprValue(&block, env, tuple_access.tuple);
                if (tuple == null) return if (block.has_term) block else debugPanic("ir.lower tuple_access missing terminator");
                const field_layout = self.input.layout_facts.exprFieldLayout(expr_id);
                const temp = try self.freshVarWithLayout(
                    field_layout,
                    "tuple_field",
                );
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .get_struct_field = .{
                        .record = tuple.?,
                        .field_index = @intCast(tuple_access.elem_index),
                    } }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .let_ => |let_expr| {
                const body_value = try self.lowerSubexprValue(&block, env, let_expr.body);
                if (body_value == null) return if (block.has_term) block else debugPanic("ir.lower let missing terminator");
                const bind_var = try self.lowerTypedSymbol(let_expr.bind);
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = bind_var,
                    .expr = try self.output.addExpr(.{ .var_ = body_value.? }),
                } });

                const extended_env = try self.extendEnv(env, .{
                    .symbol = let_expr.bind.symbol,
                    .var_ = bind_var,
                });
                defer self.allocator.free(extended_env);

                var rest = try self.lowerExpr(extended_env, let_expr.rest);
                defer rest.deinit(self.allocator);
                try self.appendChildBlock(&block, &rest);
            },
            .call => |call| {
                const args = try self.lowerValueSpan(&block, env, call.args);
                if (args == null) return if (block.has_term) block else debugPanic("ir.lower call missing terminator");
                const temp = try self.freshVar(expr.ty, "call");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .call_direct = .{
                        .proc = call.proc,
                        .args = args.?,
                    } }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .packed_fn => |packed_fn| {
                const fn_ptr = try self.freshOpaqueVar("fn_ptr");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = fn_ptr,
                    .expr = try self.output.addExpr(.{ .fn_ptr = packed_fn.lambda }),
                } });

                const captures = if (packed_fn.captures) |captures_expr| blk: {
                    const lowered = try self.lowerSubexprValue(&block, env, captures_expr);
                    if (lowered == null) return if (block.has_term) block else debugPanic("ir.lower packed_fn captures missing terminator");
                    break :blk lowered.?;
                } else blk: {
                    const null_ptr = try self.freshOpaqueVar("null_ptr");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = null_ptr,
                        .expr = try self.output.addExpr(.null_ptr),
                    } });
                    break :blk null_ptr;
                };

                const temp = try self.freshVar(expr.ty, "packed_fn");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{
                        .make_struct = try self.output.addVarSpan(&.{ fn_ptr, captures }),
                    }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .call_indirect => |call| {
                const func = try self.lowerSubexprValue(&block, env, call.func);
                if (func == null) return if (block.has_term) block else debugPanic("ir.lower call_indirect missing func terminator");
                const args = try self.lowerValueSpan(&block, env, call.args);
                if (args == null) return if (block.has_term) block else debugPanic("ir.lower call_indirect missing args terminator");
                const temp = try self.freshVar(expr.ty, "call_indirect");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .call_indirect = .{
                        .func = func.?,
                        .args = args.?,
                    } }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .low_level => |ll| {
                const args = try self.lowerValueSpan(&block, env, ll.args);
                if (args == null) return if (block.has_term) block else debugPanic("ir.lower low_level missing terminator");
                const temp = try self.freshVar(expr.ty, "low_level");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .call_low_level = .{
                        .op = ll.op,
                        .args = args.?,
                    } }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .when => |when_expr| try self.lowerWhenExpr(&block, env, expr.ty, when_expr.cond, when_expr.branches),
            .if_ => |if_expr| try self.lowerIfExpr(&block, env, expr.ty, if_expr.cond, if_expr.then_body, if_expr.else_body),
            .block => |body| try self.lowerBlockExpr(&block, env, body.stmts, body.final_expr),
            .return_ => |ret_expr| {
                const value = try self.lowerSubexprValue(&block, env, ret_expr);
                if (value == null) return if (block.has_term) block else debugPanic("ir.lower return missing terminator");
                block.setTerm(.{ .return_ = value.? });
            },
            .runtime_error => block.setTerm(.runtime_error),
            .for_ => |for_expr| try self.lowerForExpr(&block, env, for_expr.patt, for_expr.iterable, for_expr.body),
        }

        if (!block.has_term) debugPanic("ir.lower invariant violated: expr lowered without terminator");
        return block;
    }

    fn freshVar(self: *Lowerer, ty: lambdamono.Type.TypeId, comptime label: []const u8) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = self.input.layout_facts.layoutForType(ty),
            .symbol = try self.freshSymbol(label),
        };
    }

    fn freshVarWithLayout(
        self: *Lowerer,
        layout: ir_layout.Ref,
        comptime label: []const u8,
    ) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = layout,
            .symbol = try self.freshSymbol(label),
        };
    }

    fn freshOpaqueVar(self: *Lowerer, comptime label: []const u8) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = .{ .canonical = .opaque_ptr },
            .symbol = try self.freshSymbol(label),
        };
    }

    fn freshSymbol(self: *Lowerer, comptime _: []const u8) std.mem.Allocator.Error!Symbol {
        return try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
    }

    fn lookupEnv(_: *const Lowerer, env: []const EnvEntry, symbol: Symbol) ?ast.Var {
        for (env) |entry| {
            if (entry.symbol == symbol) return entry.var_;
        }
        return null;
    }

    fn extendEnv(self: *Lowerer, env: []const EnvEntry, extra: EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, env.len + 1);
        std.mem.copyForwards(EnvEntry, out[0..env.len], env);
        out[env.len] = extra;
        return out;
    }

    fn emitLeafExpr(
        self: *Lowerer,
        block: *LoweredBlock,
        ty: lambdamono.Type.TypeId,
        comptime label: []const u8,
        expr: ast.Expr,
    ) std.mem.Allocator.Error!void {
        const temp = try self.freshVar(ty, label);
        try block.stmts.append(self.allocator, .{ .let_ = .{
            .bind = temp,
            .expr = try self.output.addExpr(expr),
        } });
        block.setTerm(.{ .value = temp });
    }

    fn appendChildBlock(self: *Lowerer, dst: *LoweredBlock, src: *const LoweredBlock) std.mem.Allocator.Error!void {
        try dst.stmts.appendSlice(self.allocator, src.stmts.items);
        dst.term = src.requireTerm();
        dst.has_term = true;
    }

    fn lowerSubexprValue(
        self: *Lowerer,
        dst: *LoweredBlock,
        env: []const EnvEntry,
        expr_id: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!?ast.Var {
        var child = try self.lowerExpr(env, expr_id);
        defer child.deinit(self.allocator);
        try dst.stmts.appendSlice(self.allocator, child.stmts.items);
        return switch (child.term) {
            .value => |value| value,
            else => blk: {
                dst.term = child.requireTerm();
                dst.has_term = true;
                break :blk null;
            },
        };
    }

    fn lowerValueSpan(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        span: lambdamono.Ast.Span(lambdamono.Ast.ExprId),
    ) std.mem.Allocator.Error!?ast.Span(ast.Var) {
        const exprs = self.input.store.sliceExprSpan(span);
        const out = try self.allocator.alloc(ast.Var, exprs.len);
        defer self.allocator.free(out);
        for (exprs, 0..) |expr_id, i| {
            const value = try self.lowerSubexprValue(block, env, expr_id);
            if (value == null) return null;
            out[i] = value.?;
        }
        return try self.output.addVarSpan(out);
    }

    fn lowerFieldValues(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        span: lambdamono.Ast.Span(lambdamono.Ast.FieldExpr),
    ) std.mem.Allocator.Error!?ast.Span(ast.Var) {
        const fields = self.input.store.sliceFieldExprSpan(span);
        const out = try self.allocator.alloc(ast.Var, fields.len);
        defer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            const value = try self.lowerSubexprValue(block, env, field.value);
            if (value == null) return null;
            out[i] = value.?;
        }
        return try self.output.addVarSpan(out);
    }

    fn lowerWhenExpr(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        result_ty: lambdamono.Type.TypeId,
        cond_expr: lambdamono.Ast.ExprId,
        branches_span: lambdamono.Ast.Span(lambdamono.Ast.BranchId),
    ) std.mem.Allocator.Error!void {
        const cond = try self.lowerSubexprValue(block, env, cond_expr);
        if (cond == null) return;

        const discr = if (self.input.layout_facts.exprDiscriminantLayout(cond_expr)) |discr_layout| blk: {
            const discr_var = try self.freshVarWithLayout(
                discr_layout,
                "when_discr",
            );
            try block.stmts.append(self.allocator, .{ .let_ = .{
                .bind = discr_var,
                .expr = try self.output.addExpr(.{ .get_union_id = cond.? }),
            } });
            break :blk discr_var;
        } else cond.?;

        var tag_branches = std.ArrayList(ast.Branch).empty;
        defer tag_branches.deinit(self.allocator);

        var default_block: ?ast.BlockId = null;
        const join = try self.freshVar(result_ty, "when_join");

        for (self.input.store.sliceBranchSpan(branches_span)) |branch_id| {
            const branch = self.input.store.getBranch(branch_id);
            const pat = self.input.store.getPat(branch.pat);
            switch (pat.data) {
                .var_ => {
                    default_block = try self.lowerPatternBranchBlock(env, cond.?, branch.pat, branch.body);
                },
                .bool_lit => |value| {
                    try tag_branches.append(self.allocator, .{
                        .value = if (value) 1 else 0,
                        .block = try self.lowerPatternBranchBlock(env, cond.?, branch.pat, branch.body),
                    });
                },
                .tag => |tag| {
                    try tag_branches.append(self.allocator, .{
                        .value = tag.discriminant,
                        .block = try self.lowerPatternBranchBlock(env, cond.?, branch.pat, branch.body),
                    });
                },
            }
        }

        std.mem.sort(ast.Branch, tag_branches.items, {}, struct {
            fn lessThan(_: void, left: ast.Branch, right: ast.Branch) bool {
                return left.value < right.value;
            }
        }.lessThan);

        const default_final = default_block orelse try self.output.addBlock(.{
            .stmts = try self.output.addStmtSpan(&.{}),
            .term = .runtime_error,
        });

        try block.stmts.append(self.allocator, .{ .switch_ = .{
            .cond = discr,
            .branches = try self.output.addBranchSpan(tag_branches.items),
            .default_block = default_final,
            .join = join,
        } });
        block.setTerm(.{ .value = join });
    }

    fn lowerIfExpr(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        result_ty: lambdamono.Type.TypeId,
        cond_expr: lambdamono.Ast.ExprId,
        then_expr: lambdamono.Ast.ExprId,
        else_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        const cond = try self.lowerSubexprValue(block, env, cond_expr);
        if (cond == null) return;

        const then_block = try self.lowerBlock(env, then_expr);
        const else_block = try self.lowerBlock(env, else_expr);
        const join = try self.freshVar(result_ty, "if_join");

        const branches = try self.output.addBranchSpan(&.{
            .{ .value = 1, .block = then_block },
        });
        try block.stmts.append(self.allocator, .{ .switch_ = .{
            .cond = cond.?,
            .branches = branches,
            .default_block = else_block,
            .join = join,
        } });
        block.setTerm(.{ .value = join });
    }

    fn lowerBlockExpr(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        stmts_span: lambdamono.Ast.Span(lambdamono.Ast.StmtId),
        final_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        var current_env = try self.allocator.dupe(EnvEntry, env);
        defer self.allocator.free(current_env);

        for (self.input.store.sliceStmtSpan(stmts_span)) |stmt_id| {
            const keep_going = try self.lowerStmt(block, &current_env, stmt_id);
            if (!keep_going) return;
        }

        var tail = try self.lowerExpr(current_env, final_expr);
        defer tail.deinit(self.allocator);
        try self.appendChildBlock(block, &tail);
    }

    fn lowerStmt(
        self: *Lowerer,
        block: *LoweredBlock,
        env: *[]EnvEntry,
        stmt_id: lambdamono.Ast.StmtId,
    ) std.mem.Allocator.Error!bool {
        const stmt = self.input.store.getStmt(stmt_id);
        switch (stmt) {
            .decl => |decl| {
                const value = try self.lowerSubexprValue(block, env.*, decl.body);
                if (value == null) return false;
                const bind = try self.lowerTypedSymbol(decl.bind);
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = bind,
                    .expr = try self.output.addExpr(.{ .var_ = value.? }),
                } });
                const extended = try self.extendEnv(env.*, .{
                    .symbol = decl.bind.symbol,
                    .var_ = bind,
                });
                self.allocator.free(env.*);
                env.* = extended;
                return true;
            },
            .var_decl => |decl| {
                const value = try self.lowerSubexprValue(block, env.*, decl.body);
                if (value == null) return false;
                const bind = try self.lowerTypedSymbol(decl.bind);
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = bind,
                    .expr = try self.output.addExpr(.{ .var_ = value.? }),
                } });
                const extended = try self.extendEnv(env.*, .{
                    .symbol = decl.bind.symbol,
                    .var_ = bind,
                });
                self.allocator.free(env.*);
                env.* = extended;
                return true;
            },
            .reassign => |reassign| {
                const value = try self.lowerSubexprValue(block, env.*, reassign.body);
                if (value == null) return false;
                const target = self.lookupEnv(env.*, reassign.target) orelse debugPanic("ir.lower.reassign missing target");
                try block.stmts.append(self.allocator, .{ .set = .{
                    .target = target,
                    .value = value.?,
                } });
                return true;
            },
            .expr => |expr_id| {
                const value = try self.lowerSubexprValue(block, env.*, expr_id);
                if (value == null) return false;
                return true;
            },
            .debug => |expr_id| {
                const value = try self.lowerSubexprValue(block, env.*, expr_id);
                if (value == null) return false;
                try block.stmts.append(self.allocator, .{ .debug = value.? });
                return true;
            },
            .expect => |expr_id| {
                const value = try self.lowerSubexprValue(block, env.*, expr_id);
                if (value == null) return false;
                try block.stmts.append(self.allocator, .{ .expect = value.? });
                return true;
            },
            .crash => |msg| {
                block.setTerm(.{ .crash = msg });
                return false;
            },
            .return_ => |expr_id| {
                const value = try self.lowerSubexprValue(block, env.*, expr_id);
                if (value == null) return false;
                block.setTerm(.{ .return_ = value.? });
                return false;
            },
            .break_ => {
                block.setTerm(.{ .@"unreachable" = {} });
                try block.stmts.append(self.allocator, .break_);
                return false;
            },
            .for_ => |for_stmt| {
                try self.lowerForStmt(block, env.*, for_stmt.patt, for_stmt.iterable, for_stmt.body);
                return true;
            },
            .while_ => |while_stmt| {
                try self.lowerWhileStmt(block, env.*, while_stmt.cond, while_stmt.body);
                return true;
            },
        }
    }

    fn lowerForExpr(
        self: *Lowerer,
        block: *Lowerer.LoweredBlock,
        env: []const EnvEntry,
        patt: lambdamono.Ast.PatId,
        iterable_expr: lambdamono.Ast.ExprId,
        body_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        try self.lowerForLoop(block, env, patt, iterable_expr, body_expr);
        block.setTerm(.{ .value = try self.makeUnitValue(block, "for_progress") });
    }

    fn lowerForStmt(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        patt: lambdamono.Ast.PatId,
        iterable_expr: lambdamono.Ast.ExprId,
        body_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        try self.lowerForLoop(block, env, patt, iterable_expr, body_expr);
    }

    fn lowerForLoop(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        patt: lambdamono.Ast.PatId,
        iterable_expr: lambdamono.Ast.ExprId,
        body_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        const iterable = try self.lowerSubexprValue(block, env, iterable_expr);
        if (iterable == null) return;

        const elem = try self.freshVarWithLayout(self.input.layout_facts.patLayout(patt), "for_elem");
        const body_block = try self.lowerPatternBranchBlock(env, elem, patt, body_expr);
        try block.stmts.append(self.allocator, .{ .for_list = .{
            .elem = elem,
            .iterable = iterable.?,
            .body = body_block,
        } });
    }

    fn lowerWhileStmt(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        cond_expr: lambdamono.Ast.ExprId,
        body_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!void {
        try block.stmts.append(self.allocator, .{ .while_ = .{
            .cond = try self.lowerBlock(env, cond_expr),
            .body = try self.lowerBlock(env, body_expr),
        } });
    }

    fn lowerPatternBranchBlock(
        self: *Lowerer,
        env: []const EnvEntry,
        source_var: ast.Var,
        pat_id: lambdamono.Ast.PatId,
        body_expr: lambdamono.Ast.ExprId,
    ) std.mem.Allocator.Error!ast.BlockId {
        var prefix = std.ArrayList(ast.Stmt).empty;
        defer prefix.deinit(self.allocator);
        var additions = std.ArrayList(EnvEntry).empty;
        defer additions.deinit(self.allocator);

        try self.destructurePattern(&prefix, &additions, source_var, pat_id);

        const extended_env = try self.concatEnv(env, additions.items);
        defer self.allocator.free(extended_env);

        var body = try self.lowerExpr(extended_env, body_expr);
        defer body.deinit(self.allocator);

        try prefix.appendSlice(self.allocator, body.stmts.items);
        const stmt_span = try self.addStmtSpan(prefix.items);
        return try self.output.addBlock(.{
            .stmts = stmt_span,
            .term = body.requireTerm(),
        });
    }

    fn destructurePattern(
        self: *Lowerer,
        prefix: *std.ArrayList(ast.Stmt),
        additions: *std.ArrayList(EnvEntry),
        source_var: ast.Var,
        pat_id: lambdamono.Ast.PatId,
    ) std.mem.Allocator.Error!void {
        const pat = self.input.store.getPat(pat_id);
        switch (pat.data) {
            .var_ => |symbol| try additions.append(self.allocator, .{
                .symbol = symbol,
                .var_ = source_var,
            }),
            .bool_lit => {},
            .tag => |tag| {
                const args = self.input.store.slicePatSpan(tag.args);
                if (args.len == 0) return;
                const payload_layout = self.input.layout_facts.patTagPayloadLayout(pat_id);

                const payload_var = try self.freshVarWithLayout(
                    payload_layout,
                    "pat_payload",
                );
                try prefix.append(self.allocator, .{ .let_ = .{
                    .bind = payload_var,
                    .expr = try self.output.addExpr(.{ .get_union_struct = .{
                        .value = source_var,
                        .tag_discriminant = tag.discriminant,
                    } }),
                } });

                for (args, 0..) |arg_pat_id, i| {
                    const field_layout = try self.input.layout_facts.structFieldLayout(payload_var.layout, @intCast(i));
                    const field_var = try self.freshVarWithLayout(
                        field_layout,
                        "pat_field",
                    );
                    try prefix.append(self.allocator, .{ .let_ = .{
                        .bind = field_var,
                        .expr = try self.output.addExpr(.{ .get_struct_field = .{
                            .record = payload_var,
                            .field_index = @intCast(i),
                        } }),
                    } });
                    try self.destructurePattern(prefix, additions, field_var, arg_pat_id);
                }
            },
        }
    }

    fn concatEnv(self: *Lowerer, left: []const EnvEntry, right: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, left.len + right.len);
        std.mem.copyForwards(EnvEntry, out[0..left.len], left);
        std.mem.copyForwards(EnvEntry, out[left.len..], right);
        return out;
    }

    fn makeUnitValue(self: *Lowerer, block: *LoweredBlock, comptime label: []const u8) std.mem.Allocator.Error!ast.Var {
        const unit = try self.freshUnitVar(label);
        try block.stmts.append(self.allocator, .{ .let_ = .{
            .bind = unit,
            .expr = try self.output.addExpr(.{ .make_struct = try self.output.addVarSpan(&.{}) }),
        } });
        return unit;
    }

    fn freshUnitVar(self: *Lowerer, comptime label: []const u8) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = .{ .canonical = .zst },
            .symbol = try self.freshSymbol(label),
        };
    }

    fn addStmtSpan(self: *Lowerer, stmts: []const ast.Stmt) std.mem.Allocator.Error!ast.Span(ast.StmtId) {
        const ids = try self.allocator.alloc(ast.StmtId, stmts.len);
        defer self.allocator.free(ids);

        for (stmts, 0..) |stmt, i| {
            ids[i] = try self.output.addStmt(stmt);
        }

        return try self.output.addStmtSpan(ids);
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

fn debugTodoLowLevel(op: base.LowLevel) noreturn {
    @branchHint(.cold);
    std.debug.panic("TODO ir low-level op {s}", .{@tagName(op)});
}

test "ir lower tests" {
    std.testing.refAllDecls(@This());
}
